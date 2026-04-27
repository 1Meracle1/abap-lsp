use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;

use rusqlite::types::Value;
use rusqlite::{Connection, OpenFlags, OptionalExtension, params, params_from_iter};
use serde::Deserialize;
use thiserror::Error;

const SCHEMA_VERSION: i64 = 1;
const MIGRATION_SQL: &str = r#"
CREATE TABLE IF NOT EXISTS schema_meta (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS dependency_artifacts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    product_version TEXT NOT NULL,
    package_name TEXT NOT NULL,
    package_version TEXT NOT NULL,
    object_kind TEXT NOT NULL,
    object_name TEXT NOT NULL,
    object_uri TEXT NOT NULL,
    object_type TEXT NOT NULL,
    description TEXT NOT NULL,
    file_extension TEXT NOT NULL,
    source_text TEXT NOT NULL,
    fetched_at TEXT NOT NULL,
    UNIQUE(product_version, package_name, package_version, object_kind, object_name)
);

CREATE TABLE IF NOT EXISTS dependency_symbol_index (
    artifact_id INTEGER NOT NULL,
    symbol_name TEXT NOT NULL,
    symbol_kind TEXT NOT NULL,
    range_start INTEGER NOT NULL,
    range_end INTEGER NOT NULL,
    priority INTEGER NOT NULL,
    FOREIGN KEY(artifact_id) REFERENCES dependency_artifacts(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_dependency_artifacts_lookup
    ON dependency_artifacts(product_version, object_name, object_kind, package_version);

CREATE INDEX IF NOT EXISTS idx_dependency_symbol_lookup
    ON dependency_symbol_index(symbol_name, symbol_kind, priority DESC, artifact_id);

CREATE TABLE IF NOT EXISTS dependency_negative_lookups (
    profile_key TEXT NOT NULL,
    product_version TEXT NOT NULL,
    connection_key TEXT NOT NULL,
    candidate_kind TEXT NOT NULL,
    candidate_name TEXT NOT NULL,
    recorded_at TEXT NOT NULL,
    PRIMARY KEY(profile_key, connection_key, candidate_kind, candidate_name)
);

CREATE INDEX IF NOT EXISTS idx_dependency_negative_lookup
    ON dependency_negative_lookups(product_version, connection_key, candidate_kind, candidate_name);
"#;

#[derive(Debug, Error)]
pub enum DependencyStoreError {
    #[error("failed to resolve dependency store path")]
    MissingStorePath,
    #[error("sqlite error: {0}")]
    Sqlite(#[from] rusqlite::Error),
    #[error("failed to create dependency store directory {path}: {source}")]
    CreateDirectory {
        path: String,
        source: std::io::Error,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct DependencyProfile {
    pub product_version: String,
    pub default_package_version: String,
    #[serde(default)]
    pub packages: BTreeMap<String, String>,
}

impl DependencyProfile {
    pub fn normalized_product_version(&self) -> String {
        self.product_version.trim().to_ascii_lowercase()
    }

    pub fn package_version_for(&self, package_name: &str) -> String {
        let normalized_package = normalize_name(package_name);
        self.packages
            .iter()
            .find(|(name, _)| normalize_name(name) == normalized_package)
            .map(|(_, version)| normalize_name(version))
            .unwrap_or_else(|| normalize_name(&self.default_package_version))
    }

    pub fn package_version_set(&self) -> Vec<String> {
        let mut versions = BTreeSet::new();
        versions.insert(normalize_name(&self.default_package_version));
        for version in self.packages.values() {
            versions.insert(normalize_name(version));
        }
        versions.into_iter().collect()
    }

    pub fn profile_key(&self) -> String {
        let mut out = format!(
            "{}|{}",
            self.normalized_product_version(),
            normalize_name(&self.default_package_version)
        );
        for (package_name, version) in &self.packages {
            out.push('|');
            out.push_str(&normalize_name(package_name));
            out.push('=');
            out.push_str(&normalize_name(version));
        }
        out
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StoredSymbolInput {
    pub symbol_name: String,
    pub symbol_kind: String,
    pub range_start: usize,
    pub range_end: usize,
    pub priority: i64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StoredArtifactInput {
    pub package_name: String,
    pub object_kind: String,
    pub object_name: String,
    pub object_uri: String,
    pub object_type: String,
    pub description: String,
    pub file_extension: String,
    pub source_text: String,
    pub fetched_at: String,
    pub symbols: Vec<StoredSymbolInput>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StoredArtifactRecord {
    pub artifact_id: i64,
    pub package_name: String,
    pub package_version: String,
    pub object_kind: String,
    pub object_name: String,
    pub object_uri: String,
    pub object_type: String,
    pub description: String,
    pub file_extension: String,
    pub source_text: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolLookupResult {
    pub artifact_id: i64,
    pub package_name: String,
    pub package_version: String,
    pub object_kind: String,
    pub object_name: String,
    pub file_extension: String,
    pub range_start: usize,
    pub range_end: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CandidateCacheStatus {
    Missing,
    Artifact,
    Negative,
}

#[derive(Debug, Clone)]
pub struct DependencyStore {
    path: PathBuf,
}

pub struct DependencyStoreReader {
    connection: Connection,
}

impl DependencyStore {
    pub fn from_override_path(override_path: Option<&Path>) -> Result<Self, DependencyStoreError> {
        let path = resolve_dependency_store_path(override_path)
            .ok_or(DependencyStoreError::MissingStorePath)?;
        Ok(Self { path })
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn reader(&self) -> Result<DependencyStoreReader, DependencyStoreError> {
        Ok(DependencyStoreReader {
            connection: self.open_read_connection()?,
        })
    }

    pub fn put_artifact(
        &self,
        profile: &DependencyProfile,
        artifact: &StoredArtifactInput,
    ) -> Result<i64, DependencyStoreError> {
        let mut connection = self.open_connection()?;
        let tx = connection.transaction()?;
        let artifact_id = put_artifact_in_tx(&tx, profile, artifact)?;
        tx.commit()?;
        Ok(artifact_id)
    }

    pub fn put_artifacts(
        &self,
        profile: &DependencyProfile,
        artifacts: &[StoredArtifactInput],
    ) -> Result<Vec<i64>, DependencyStoreError> {
        let mut connection = self.open_connection()?;
        let tx = connection.transaction()?;
        let mut ids = Vec::with_capacity(artifacts.len());
        for artifact in artifacts {
            ids.push(put_artifact_in_tx(&tx, profile, artifact)?);
        }
        tx.commit()?;
        Ok(ids)
    }

    pub fn find_cached_candidate(
        &self,
        profile: &DependencyProfile,
        connection_key: &str,
        candidate_name: &str,
        candidate_kind: &str,
    ) -> Result<CandidateCacheStatus, DependencyStoreError> {
        self.reader()?.find_cached_candidate(
            profile,
            connection_key,
            candidate_name,
            candidate_kind,
        )
    }

    pub fn lookup_symbol(
        &self,
        profile: &DependencyProfile,
        symbol_name: &str,
        candidate_kind: &str,
    ) -> Result<Option<SymbolLookupResult>, DependencyStoreError> {
        self.reader()?
            .lookup_symbol(profile, symbol_name, candidate_kind)
    }

    pub fn read_artifact_source(
        &self,
        artifact_id: i64,
    ) -> Result<Option<StoredArtifactRecord>, DependencyStoreError> {
        self.reader()?.read_artifact_source(artifact_id)
    }

    pub fn find_artifact_for_candidate(
        &self,
        profile: &DependencyProfile,
        candidate_name: &str,
        candidate_kind: &str,
    ) -> Result<Option<StoredArtifactRecord>, DependencyStoreError> {
        self.reader()?
            .find_artifact_for_candidate(profile, candidate_name, candidate_kind)
    }

    pub fn list_artifacts_by_kind(
        &self,
        profile: &DependencyProfile,
        object_kind: &str,
    ) -> Result<Vec<StoredArtifactRecord>, DependencyStoreError> {
        self.reader()?.list_artifacts_by_kind(profile, object_kind)
    }

    pub fn record_negative_lookup(
        &self,
        profile: &DependencyProfile,
        connection_key: &str,
        candidate_name: &str,
        candidate_kind: &str,
        recorded_at: &str,
    ) -> Result<(), DependencyStoreError> {
        let mut connection = self.open_connection()?;
        let tx = connection.transaction()?;
        tx.execute(
            r#"
INSERT INTO dependency_negative_lookups (
    profile_key,
    product_version,
    connection_key,
    candidate_kind,
    candidate_name,
    recorded_at
) VALUES (?1, ?2, ?3, ?4, ?5, ?6)
ON CONFLICT(profile_key, connection_key, candidate_kind, candidate_name)
DO UPDATE SET recorded_at = excluded.recorded_at
"#,
            params![
                profile.profile_key(),
                profile.normalized_product_version(),
                normalize_name(connection_key),
                normalize_name(candidate_kind),
                normalize_name(candidate_name),
                recorded_at,
            ],
        )?;
        tx.commit()?;
        Ok(())
    }

    pub fn clear_profile_scope(
        &self,
        profile: &DependencyProfile,
    ) -> Result<(), DependencyStoreError> {
        let mut connection = self.open_connection()?;
        let tx = connection.transaction()?;

        let package_versions = profile.package_version_set();
        let mut delete_artifacts = String::from(
            "DELETE FROM dependency_artifacts WHERE product_version = ? AND package_version IN (",
        );
        append_placeholders(&mut delete_artifacts, package_versions.len());
        delete_artifacts.push(')');
        let mut artifact_params = Vec::with_capacity(1 + package_versions.len());
        artifact_params.push(Value::from(profile.normalized_product_version()));
        artifact_params.extend(package_versions.iter().cloned().map(Value::from));
        tx.execute(&delete_artifacts, params_from_iter(artifact_params))?;

        tx.execute(
            "DELETE FROM dependency_negative_lookups WHERE profile_key = ?1",
            params![profile.profile_key()],
        )?;

        tx.commit()?;
        Ok(())
    }

    fn open_connection(&self) -> Result<Connection, DependencyStoreError> {
        if let Some(parent) = self.path.parent() {
            fs::create_dir_all(parent).map_err(|source| DependencyStoreError::CreateDirectory {
                path: parent.display().to_string(),
                source,
            })?;
        }
        let mut flags = OpenFlags::SQLITE_OPEN_READ_WRITE | OpenFlags::SQLITE_OPEN_CREATE;
        flags |= OpenFlags::SQLITE_OPEN_URI;
        let connection = Connection::open_with_flags(&self.path, flags)?;
        connection.busy_timeout(Duration::from_secs(5))?;
        connection.pragma_update(None, "journal_mode", "WAL")?;
        connection.pragma_update(None, "foreign_keys", "ON")?;
        connection.execute_batch(MIGRATION_SQL)?;
        connection.execute(
            r#"
INSERT INTO schema_meta(key, value)
VALUES('schema_version', ?1)
ON CONFLICT(key) DO UPDATE SET value = excluded.value
"#,
            params![SCHEMA_VERSION.to_string()],
        )?;
        Ok(connection)
    }

    fn open_read_connection(&self) -> Result<Connection, DependencyStoreError> {
        let flags = OpenFlags::SQLITE_OPEN_READ_ONLY | OpenFlags::SQLITE_OPEN_URI;
        let uri = sqlite_file_uri(&self.path, "mode=ro");
        let connection = Connection::open_with_flags(&uri, flags)?;
        connection.busy_timeout(Duration::from_secs(5))?;
        if validate_read_connection(&connection).is_ok() {
            return Ok(connection);
        }

        let connection = {
            let immutable_uri = sqlite_file_uri(&self.path, "mode=ro&immutable=1");
            Connection::open_with_flags(&immutable_uri, flags)
        }?;
        connection.busy_timeout(Duration::from_secs(5))?;
        Ok(connection)
    }
}

fn validate_read_connection(connection: &Connection) -> rusqlite::Result<()> {
    connection.query_row("SELECT name FROM sqlite_master LIMIT 1", [], |_| Ok(()))
}

fn sqlite_file_uri(path: &Path, query: &str) -> String {
    let mut path = path.to_string_lossy().replace('\\', "/");
    if path.as_bytes().get(1) == Some(&b':') {
        path.insert(0, '/');
    }
    format!(
        "file://{}?{}",
        percent_encode_sqlite_uri_path(path.as_bytes()),
        query
    )
}

fn percent_encode_sqlite_uri_path(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len());
    for &byte in bytes {
        if byte.is_ascii_alphanumeric() || matches!(byte, b'/' | b':' | b'-' | b'_' | b'.' | b'~') {
            out.push(byte as char);
        } else {
            out.push('%');
            out.push(hex_digit(byte >> 4));
            out.push(hex_digit(byte & 0x0f));
        }
    }
    out
}

fn hex_digit(value: u8) -> char {
    match value {
        0..=9 => (b'0' + value) as char,
        10..=15 => (b'A' + (value - 10)) as char,
        _ => '0',
    }
}

impl DependencyStoreReader {
    pub fn find_cached_candidate(
        &self,
        profile: &DependencyProfile,
        connection_key: &str,
        candidate_name: &str,
        candidate_kind: &str,
    ) -> Result<CandidateCacheStatus, DependencyStoreError> {
        let normalized_name = normalize_name(candidate_name);
        let normalized_kind = normalize_name(candidate_kind);
        if normalized_name.is_empty() || normalized_kind.is_empty() {
            return Ok(CandidateCacheStatus::Missing);
        }
        let artifact_exists = candidate_artifact_exists(
            &self.connection,
            profile,
            normalized_name.as_str(),
            normalized_kind.as_str(),
        )?;
        if artifact_exists {
            return Ok(CandidateCacheStatus::Artifact);
        }
        let negative_exists = self
            .connection
            .query_row(
                r#"
SELECT 1
FROM dependency_negative_lookups
WHERE profile_key = ?1
  AND connection_key = ?2
  AND candidate_kind = ?3
  AND candidate_name = ?4
LIMIT 1
"#,
                params![
                    profile.profile_key(),
                    normalize_name(connection_key),
                    normalized_kind,
                    normalized_name
                ],
                |_| Ok(()),
            )
            .optional()?
            .is_some();
        Ok(if negative_exists {
            CandidateCacheStatus::Negative
        } else {
            CandidateCacheStatus::Missing
        })
    }

    pub fn lookup_symbol(
        &self,
        profile: &DependencyProfile,
        symbol_name: &str,
        candidate_kind: &str,
    ) -> Result<Option<SymbolLookupResult>, DependencyStoreError> {
        let allowed_kinds = candidate_artifact_kinds(candidate_kind);
        let allowed_symbol_kinds = candidate_symbol_kinds(candidate_kind);
        if allowed_kinds.is_empty() || allowed_symbol_kinds.is_empty() {
            return Ok(None);
        }

        let normalized_symbol_name = normalize_name(symbol_name);
        if normalized_symbol_name.is_empty() {
            return Ok(None);
        }

        let mut sql = String::from(
            r#"
SELECT
    artifact.id,
    artifact.package_name,
    artifact.package_version,
    artifact.object_kind,
    artifact.object_name,
    artifact.file_extension,
    symbol.range_start,
    symbol.range_end
FROM dependency_symbol_index AS symbol
JOIN dependency_artifacts AS artifact
    ON artifact.id = symbol.artifact_id
WHERE artifact.product_version = ?
  AND symbol.symbol_name = ?
  AND symbol.symbol_kind IN ("#,
        );
        append_placeholders(&mut sql, allowed_symbol_kinds.len());
        sql.push_str(") AND artifact.package_version IN (");
        append_placeholders(&mut sql, profile.package_version_set().len());
        sql.push_str(") AND artifact.object_kind IN (");
        append_placeholders(&mut sql, allowed_kinds.len());
        sql.push_str(") ORDER BY symbol.priority DESC, artifact.package_name ASC LIMIT 1");

        let params = symbol_lookup_params(
            profile,
            &normalized_symbol_name,
            &allowed_symbol_kinds,
            &allowed_kinds,
        );
        let mut statement = self.connection.prepare(&sql)?;
        let row = statement
            .query_row(params_from_iter(params), |row| {
                Ok(SymbolLookupResult {
                    artifact_id: row.get(0)?,
                    package_name: row.get(1)?,
                    package_version: row.get(2)?,
                    object_kind: row.get(3)?,
                    object_name: row.get(4)?,
                    file_extension: row.get(5)?,
                    range_start: row.get::<_, i64>(6)? as usize,
                    range_end: row.get::<_, i64>(7)? as usize,
                })
            })
            .optional()?;
        Ok(row)
    }

    pub fn read_artifact_source(
        &self,
        artifact_id: i64,
    ) -> Result<Option<StoredArtifactRecord>, DependencyStoreError> {
        let artifact = self
            .connection
            .query_row(
                r#"
SELECT
    id,
    package_name,
    package_version,
    object_kind,
    object_name,
    object_uri,
    object_type,
    description,
    file_extension,
    source_text
FROM dependency_artifacts
WHERE id = ?1
"#,
                params![artifact_id],
                |row| {
                    Ok(StoredArtifactRecord {
                        artifact_id: row.get(0)?,
                        package_name: row.get(1)?,
                        package_version: row.get(2)?,
                        object_kind: row.get(3)?,
                        object_name: row.get(4)?,
                        object_uri: row.get(5)?,
                        object_type: row.get(6)?,
                        description: row.get(7)?,
                        file_extension: row.get(8)?,
                        source_text: row.get(9)?,
                    })
                },
            )
            .optional()?;
        Ok(artifact)
    }

    pub fn find_artifact_for_candidate(
        &self,
        profile: &DependencyProfile,
        candidate_name: &str,
        candidate_kind: &str,
    ) -> Result<Option<StoredArtifactRecord>, DependencyStoreError> {
        let allowed_kinds = candidate_artifact_kinds(candidate_kind);
        if allowed_kinds.is_empty() {
            return Ok(None);
        }

        let normalized_name = normalize_name(candidate_name);
        if normalized_name.is_empty() {
            return Ok(None);
        }

        let package_versions = profile.package_version_set();
        let mut sql = String::from(
            r#"
SELECT
    id,
    package_name,
    package_version,
    object_kind,
    object_name,
    object_uri,
    object_type,
    description,
    file_extension,
    source_text
FROM dependency_artifacts
WHERE product_version = ?
  AND object_name = ?
  AND package_version IN ("#,
        );
        append_placeholders(&mut sql, package_versions.len());
        sql.push_str(") AND object_kind IN (");
        append_placeholders(&mut sql, allowed_kinds.len());
        sql.push(')');

        let mut params = Vec::with_capacity(2 + package_versions.len() + allowed_kinds.len());
        params.push(Value::from(profile.normalized_product_version()));
        params.push(Value::from(normalized_name));
        params.extend(package_versions.into_iter().map(Value::from));
        params.extend(allowed_kinds.iter().cloned().map(Value::from));

        let mut statement = self.connection.prepare(&sql)?;
        let mut rows = statement.query(params_from_iter(params))?;
        let mut candidates = Vec::new();
        while let Some(row) = rows.next()? {
            candidates.push(StoredArtifactRecord {
                artifact_id: row.get(0)?,
                package_name: row.get(1)?,
                package_version: row.get(2)?,
                object_kind: row.get(3)?,
                object_name: row.get(4)?,
                object_uri: row.get(5)?,
                object_type: row.get(6)?,
                description: row.get(7)?,
                file_extension: row.get(8)?,
                source_text: row.get(9)?,
            });
        }
        candidates.sort_by(|left, right| {
            artifact_kind_rank(&allowed_kinds, left.object_kind.as_str())
                .cmp(&artifact_kind_rank(
                    &allowed_kinds,
                    right.object_kind.as_str(),
                ))
                .then_with(|| left.package_name.cmp(&right.package_name))
                .then_with(|| left.object_name.cmp(&right.object_name))
        });
        Ok(candidates.into_iter().next())
    }

    pub fn list_artifacts_by_kind(
        &self,
        profile: &DependencyProfile,
        object_kind: &str,
    ) -> Result<Vec<StoredArtifactRecord>, DependencyStoreError> {
        let normalized_kind = normalize_name(object_kind);
        if normalized_kind.is_empty() {
            return Ok(Vec::new());
        }

        let package_versions = profile.package_version_set();
        let mut sql = String::from(
            r#"
SELECT
    id,
    package_name,
    package_version,
    object_kind,
    object_name,
    object_uri,
    object_type,
    description,
    file_extension,
    source_text
FROM dependency_artifacts
WHERE product_version = ?
  AND object_kind = ?
  AND package_version IN ("#,
        );
        append_placeholders(&mut sql, package_versions.len());
        sql.push_str(") ORDER BY package_name ASC, object_name ASC");

        let mut params = Vec::with_capacity(2 + package_versions.len());
        params.push(Value::from(profile.normalized_product_version()));
        params.push(Value::from(normalized_kind));
        params.extend(package_versions.into_iter().map(Value::from));

        let mut statement = self.connection.prepare(&sql)?;
        let rows = statement.query_map(params_from_iter(params), |row| {
            Ok(StoredArtifactRecord {
                artifact_id: row.get(0)?,
                package_name: row.get(1)?,
                package_version: row.get(2)?,
                object_kind: row.get(3)?,
                object_name: row.get(4)?,
                object_uri: row.get(5)?,
                object_type: row.get(6)?,
                description: row.get(7)?,
                file_extension: row.get(8)?,
                source_text: row.get(9)?,
            })
        })?;

        rows.collect::<Result<Vec<_>, _>>()
            .map_err(DependencyStoreError::from)
    }
}

pub fn resolve_dependency_store_path(override_path: Option<&Path>) -> Option<PathBuf> {
    if let Some(path) = override_path {
        let trimmed = path.as_os_str().to_string_lossy().trim().to_string();
        if !trimmed.is_empty() {
            return Some(PathBuf::from(trimmed));
        }
    }

    dependency_store_default_path()
}

pub fn dependency_store_default_path() -> Option<PathBuf> {
    let base = if cfg!(windows) {
        std::env::var_os("LOCALAPPDATA")
            .map(PathBuf::from)
            .or_else(|| {
                std::env::var_os("USERPROFILE")
                    .map(PathBuf::from)
                    .map(|home| home.join("AppData").join("Local"))
            })
    } else if cfg!(target_os = "macos") {
        std::env::var_os("HOME")
            .map(PathBuf::from)
            .map(|home| home.join("Library").join("Caches"))
    } else {
        std::env::var_os("XDG_CACHE_HOME")
            .map(PathBuf::from)
            .or_else(|| {
                std::env::var_os("HOME")
                    .map(PathBuf::from)
                    .map(|home| home.join(".cache"))
            })
    };
    base.map(|base| base.join("abap-ls").join("dependency-cache.sqlite3"))
}

fn put_artifact_in_tx(
    tx: &rusqlite::Transaction<'_>,
    profile: &DependencyProfile,
    artifact: &StoredArtifactInput,
) -> Result<i64, DependencyStoreError> {
    let package_name = normalize_name(&artifact.package_name);
    let package_version = profile.package_version_for(&package_name);
    let object_kind = normalize_name(&artifact.object_kind);
    let object_name = normalize_name(&artifact.object_name);

    tx.execute(
        r#"
INSERT INTO dependency_artifacts (
    product_version,
    package_name,
    package_version,
    object_kind,
    object_name,
    object_uri,
    object_type,
    description,
    file_extension,
    source_text,
    fetched_at
) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11)
ON CONFLICT(product_version, package_name, package_version, object_kind, object_name)
DO UPDATE SET
    object_uri = excluded.object_uri,
    object_type = excluded.object_type,
    description = excluded.description,
    file_extension = excluded.file_extension,
    source_text = excluded.source_text,
    fetched_at = excluded.fetched_at
"#,
        params![
            profile.normalized_product_version(),
            package_name,
            package_version,
            object_kind,
            object_name,
            artifact.object_uri.trim(),
            artifact.object_type.trim(),
            artifact.description.trim(),
            normalize_name(&artifact.file_extension),
            artifact.source_text,
            artifact.fetched_at.trim(),
        ],
    )?;

    let artifact_id: i64 = tx.query_row(
        r#"
SELECT id
FROM dependency_artifacts
WHERE product_version = ?1
  AND package_name = ?2
  AND package_version = ?3
  AND object_kind = ?4
  AND object_name = ?5
"#,
        params![
            profile.normalized_product_version(),
            package_name,
            package_version,
            object_kind,
            object_name
        ],
        |row| row.get(0),
    )?;

    tx.execute(
        "DELETE FROM dependency_symbol_index WHERE artifact_id = ?1",
        params![artifact_id],
    )?;
    for symbol in &artifact.symbols {
        tx.execute(
            r#"
INSERT INTO dependency_symbol_index (
    artifact_id,
    symbol_name,
    symbol_kind,
    range_start,
    range_end,
    priority
) VALUES (?1, ?2, ?3, ?4, ?5, ?6)
"#,
            params![
                artifact_id,
                normalize_name(&symbol.symbol_name),
                normalize_name(&symbol.symbol_kind),
                symbol.range_start as i64,
                symbol.range_end as i64,
                symbol.priority,
            ],
        )?;
    }

    Ok(artifact_id)
}

fn candidate_artifact_exists(
    connection: &Connection,
    profile: &DependencyProfile,
    candidate_name: &str,
    candidate_kind: &str,
) -> Result<bool, rusqlite::Error> {
    let allowed_kinds = candidate_artifact_kinds(candidate_kind);
    if allowed_kinds.is_empty() {
        return Ok(false);
    }
    let package_versions = profile.package_version_set();
    let mut sql = String::from(
        "SELECT 1 FROM dependency_artifacts WHERE product_version = ? AND object_name = ? AND package_version IN (",
    );
    append_placeholders(&mut sql, package_versions.len());
    sql.push_str(") AND object_kind IN (");
    append_placeholders(&mut sql, allowed_kinds.len());
    sql.push_str(") LIMIT 1");

    let mut params = Vec::with_capacity(2 + package_versions.len() + allowed_kinds.len());
    params.push(Value::from(profile.normalized_product_version()));
    params.push(Value::from(candidate_name.to_string()));
    params.extend(package_versions.into_iter().map(Value::from));
    params.extend(allowed_kinds.into_iter().map(Value::from));
    connection
        .query_row(&sql, params_from_iter(params), |_| Ok(()))
        .optional()
        .map(|row| row.is_some())
}

fn symbol_lookup_params(
    profile: &DependencyProfile,
    symbol_name: &str,
    allowed_symbol_kinds: &[String],
    allowed_kinds: &[String],
) -> Vec<Value> {
    let package_versions = profile.package_version_set();
    let mut params = Vec::with_capacity(
        2 + allowed_symbol_kinds.len() + package_versions.len() + allowed_kinds.len(),
    );
    params.push(Value::from(profile.normalized_product_version()));
    params.push(Value::from(symbol_name.to_string()));
    params.extend(allowed_symbol_kinds.iter().cloned().map(Value::from));
    params.extend(package_versions.into_iter().map(Value::from));
    params.extend(allowed_kinds.iter().cloned().map(Value::from));
    params
}

fn candidate_artifact_kinds(candidate_kind: &str) -> Vec<String> {
    match normalize_name(candidate_kind).as_str() {
        "include" => vec!["include".to_string()],
        "message-class" => vec!["message-class".to_string()],
        "report" => vec!["report".to_string()],
        "function" => vec!["function-module".to_string()],
        "static" => vec!["global-class".to_string(), "global-interface".to_string()],
        "type" | "symbol" => vec![
            "global-class".to_string(),
            "global-interface".to_string(),
            "report".to_string(),
            "ddic-data-element".to_string(),
            "ddic-domain".to_string(),
            "ddic-structure".to_string(),
            "ddic-table".to_string(),
            "ddic-table-type".to_string(),
            "ddic-view".to_string(),
        ],
        other if !other.is_empty() => vec![other.to_string()],
        _ => Vec::new(),
    }
}

fn candidate_symbol_kinds(candidate_kind: &str) -> Vec<String> {
    match normalize_name(candidate_kind).as_str() {
        "include" => vec!["include".to_string()],
        "message-class" => vec!["typedef".to_string()],
        "report" => vec!["report".to_string()],
        "function" => vec!["function-module".to_string(), "module".to_string()],
        "static" => vec!["class".to_string(), "interface".to_string()],
        "type" => vec![
            "class".to_string(),
            "interface".to_string(),
            "typedef".to_string(),
            "report".to_string(),
        ],
        "symbol" => vec![
            "class".to_string(),
            "interface".to_string(),
            "typedef".to_string(),
            "report".to_string(),
            "include".to_string(),
            "form".to_string(),
            "module".to_string(),
            "function-module".to_string(),
            "variable".to_string(),
            "constant".to_string(),
            "class-member".to_string(),
        ],
        _ => Vec::new(),
    }
}

fn append_placeholders(out: &mut String, count: usize) {
    for idx in 0..count {
        if idx > 0 {
            out.push_str(", ");
        }
        out.push('?');
    }
}

fn artifact_kind_rank(allowed_kinds: &[String], object_kind: &str) -> usize {
    allowed_kinds
        .iter()
        .position(|candidate| candidate.eq_ignore_ascii_case(object_kind))
        .unwrap_or(allowed_kinds.len())
}

fn normalize_name(value: &str) -> String {
    value.trim().to_ascii_lowercase()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn temp_store_path(name: &str) -> PathBuf {
        let path = std::env::temp_dir()
            .join("abap-lsp-dependency-store-tests")
            .join(format!("{name}.sqlite3"));
        if let Some(parent) = path.parent() {
            let _ = fs::remove_dir_all(parent);
        }
        path
    }

    fn sample_profile() -> DependencyProfile {
        DependencyProfile {
            product_version: "S4-2023".to_string(),
            default_package_version: "v1".to_string(),
            packages: BTreeMap::from([("sabap".to_string(), "v2".to_string())]),
        }
    }

    fn sample_artifact() -> StoredArtifactInput {
        StoredArtifactInput {
            package_name: "SABAP".to_string(),
            object_kind: "global-class".to_string(),
            object_name: "CL_ABAP_TYPEDESCR".to_string(),
            object_uri: "/sap/bc/adt/oo/classes/CL_ABAP_TYPEDESCR".to_string(),
            object_type: "CLAS/OC".to_string(),
            description: "Global class".to_string(),
            file_extension: "abap".to_string(),
            source_text: "CLASS cl_abap_typedescr DEFINITION. ENDCLASS.".to_string(),
            fetched_at: "2026-04-23T10:00:00Z".to_string(),
            symbols: vec![StoredSymbolInput {
                symbol_name: "CL_ABAP_TYPEDESCR".to_string(),
                symbol_kind: "class".to_string(),
                range_start: 6,
                range_end: 23,
                priority: 100,
            }],
        }
    }

    #[test]
    fn sqlite_file_uri_uses_read_only_uri_form() {
        let uri = sqlite_file_uri(
            Path::new(r"C:\Users\demo\AppData\Local\abap-ls\dependency cache.sqlite3"),
            "mode=ro&immutable=1",
        );
        assert_eq!(
            uri,
            "file:///C:/Users/demo/AppData/Local/abap-ls/dependency%20cache.sqlite3?mode=ro&immutable=1"
        );
    }

    #[test]
    fn stores_and_looks_up_artifacts() {
        let path = temp_store_path("stores_and_looks_up_artifacts");
        let store = DependencyStore::from_override_path(Some(&path)).expect("store");
        let profile = sample_profile();
        let artifact = sample_artifact();

        let artifact_id = store.put_artifact(&profile, &artifact).expect("put");
        let status = store
            .find_cached_candidate(
                &profile,
                "https://sap.example|100|demo",
                "cl_abap_typedescr",
                "type",
            )
            .expect("status");
        assert_eq!(status, CandidateCacheStatus::Artifact);

        let lookup = store
            .lookup_symbol(&profile, "cl_abap_typedescr", "type")
            .expect("lookup")
            .expect("result");
        assert_eq!(lookup.artifact_id, artifact_id);
        assert_eq!(lookup.range_start, 6);

        let stored = store
            .read_artifact_source(artifact_id)
            .expect("read")
            .expect("artifact");
        assert_eq!(stored.object_name, "cl_abap_typedescr");

        let _ = fs::remove_file(path);
    }

    #[test]
    fn lists_artifacts_by_kind_in_profile_scope() {
        let path = temp_store_path("lists_artifacts_by_kind_in_profile_scope");
        let store = DependencyStore::from_override_path(Some(&path)).expect("store");
        let profile = sample_profile();
        let mut data_element = sample_artifact();
        data_element.object_kind = "ddic-data-element".to_string();
        data_element.object_name = "ZDEMO".to_string();
        data_element.object_uri = "/sap/bc/adt/ddic/dataelements/ZDEMO".to_string();
        data_element.object_type = "DTEL/DE".to_string();
        data_element.source_text = "TYPES zdemo TYPE c LENGTH 10.".to_string();
        data_element.symbols = Vec::new();
        store
            .put_artifacts(&profile, &[sample_artifact(), data_element])
            .expect("put");

        let records = store
            .list_artifacts_by_kind(&profile, "ddic-data-element")
            .expect("list");
        assert_eq!(records.len(), 1);
        assert_eq!(records[0].object_name, "zdemo");

        let _ = fs::remove_file(path);
    }

    #[test]
    fn type_candidates_include_ddic_domains() {
        assert!(
            super::candidate_artifact_kinds("type")
                .iter()
                .any(|kind| kind == "ddic-domain")
        );
    }

    #[test]
    fn records_negative_candidates_by_profile_scope() {
        let path = temp_store_path("records_negative_candidates_by_profile_scope");
        let store = DependencyStore::from_override_path(Some(&path)).expect("store");
        let profile = sample_profile();

        store
            .record_negative_lookup(
                &profile,
                "https://sap.example|100|demo",
                "boolean",
                "type",
                "2026-04-23T10:00:00Z",
            )
            .expect("record");

        let status = store
            .find_cached_candidate(&profile, "https://sap.example|100|demo", "boolean", "type")
            .expect("status");
        assert_eq!(status, CandidateCacheStatus::Negative);

        let _ = fs::remove_file(path);
    }

    #[test]
    fn clears_profile_scope() {
        let path = temp_store_path("clears_profile_scope");
        let store = DependencyStore::from_override_path(Some(&path)).expect("store");
        let profile = sample_profile();
        store
            .put_artifact(&profile, &sample_artifact())
            .expect("put");
        store
            .record_negative_lookup(
                &profile,
                "https://sap.example|100|demo",
                "boolean",
                "type",
                "2026-04-23T10:00:00Z",
            )
            .expect("record");

        store.clear_profile_scope(&profile).expect("clear");
        assert_eq!(
            store
                .find_cached_candidate(
                    &profile,
                    "https://sap.example|100|demo",
                    "cl_abap_typedescr",
                    "type"
                )
                .expect("status"),
            CandidateCacheStatus::Missing
        );
        assert_eq!(
            store
                .find_cached_candidate(&profile, "https://sap.example|100|demo", "boolean", "type")
                .expect("status"),
            CandidateCacheStatus::Missing
        );

        let _ = fs::remove_file(path);
    }
}
