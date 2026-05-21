package abap_frontend_dependency_store

import sqlite3 "../persistence/sqlite3"

import "core:c"
import "core:mem"
import "core:os"
import filepath "core:path/filepath"
import "core:strings"

SCHEMA_VERSION :: "1"

MIGRATION_SQL :: `
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

CREATE INDEX IF NOT EXISTS idx_dependency_symbol_artifact_lookup
    ON dependency_symbol_index(artifact_id, symbol_name, symbol_kind, priority DESC);

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
`

Store_Error :: enum {
	None,
	Missing_Store_Path,
	Sqlite,
	Create_Directory,
}

Candidate_Cache_Status :: enum {
	Missing,
	Artifact,
	Negative,
}

Package_Version :: struct {
	package_name: string,
	version:      string,
}

Dependency_Profile :: struct {
	product_version:         string,
	default_package_version: string,
	packages:                []Package_Version,
}

Stored_Symbol_Input :: struct {
	symbol_name: string,
	symbol_kind: string,
	range_start: int,
	range_end:   int,
	priority:    i64,
}

Stored_Artifact_Input :: struct {
	package_name:   string,
	object_kind:    string,
	object_name:    string,
	object_uri:     string,
	object_type:    string,
	description:    string,
	file_extension: string,
	source_text:    string,
	fetched_at:     string,
	symbols:        []Stored_Symbol_Input,
}

Stored_Artifact_Record :: struct {
	artifact_id:     i64,
	package_name:    string,
	package_version: string,
	object_kind:     string,
	object_name:     string,
	object_uri:      string,
	object_type:     string,
	description:     string,
	file_extension:  string,
	source_text:     string,
}

Symbol_Lookup_Result :: struct {
	artifact_id:     i64,
	package_name:    string,
	package_version: string,
	object_kind:     string,
	object_name:     string,
	file_extension:  string,
	range_start:     int,
	range_end:       int,
}

Dependency_Store :: struct {
	path: string,
}

Dependency_Store_Reader :: struct {
	connection: ^sqlite3.Connection,
}

dependency_store_from_override_path :: proc(
	override_path: string,
	allocator: mem.Allocator,
) -> (Dependency_Store, Store_Error) {
	path, ok := resolve_dependency_store_path(override_path, allocator)
	if !ok {
		return {}, .Missing_Store_Path
	}
	return Dependency_Store{path = path}, .None
}

reader :: proc(store: ^Dependency_Store, allocator: mem.Allocator) -> (
	Dependency_Store_Reader,
	Store_Error,
) {
	connection, err := open_read_connection(store.path, allocator)
	if err != .None {
		return {}, err
	}
	return Dependency_Store_Reader{connection = connection}, .None
}

reader_destroy :: proc(reader: ^Dependency_Store_Reader) {
	if reader.connection != nil {
		sqlite3.close(reader.connection)
		reader.connection = nil
	}
}

put_artifact :: proc(
	store: ^Dependency_Store,
	profile: ^Dependency_Profile,
	artifact: ^Stored_Artifact_Input,
	allocator: mem.Allocator,
) -> (i64, Store_Error) {
	one := [?]Stored_Artifact_Input{artifact^}
	ids, err := put_artifacts(store, profile, one[:], allocator)
	if err != .None || len(ids) == 0 {
		return 0, err
	}
	return ids[0], .None
}

put_artifacts :: proc(
	store: ^Dependency_Store,
	profile: ^Dependency_Profile,
	artifacts: []Stored_Artifact_Input,
	allocator: mem.Allocator,
) -> ([dynamic]i64, Store_Error) {
	ids := make([dynamic]i64, 0, len(artifacts), allocator)
	db, err := open_connection(store.path, allocator)
	if err != .None {
		return ids, err
	}
	defer sqlite3.close(db)

	if err = exec_sql(db, "BEGIN", allocator); err != .None {
		return ids, err
	}
	for &artifact in artifacts {
		id, put_err := put_artifact_in_tx(db, profile, &artifact, allocator)
		if put_err != .None {
			exec_sql(db, "ROLLBACK", allocator)
			return ids, put_err
		}
		append(&ids, id)
	}
	if err = exec_sql(db, "COMMIT", allocator); err != .None {
		return ids, err
	}
	return ids, .None
}

find_cached_candidate :: proc(
	store: ^Dependency_Store,
	profile: ^Dependency_Profile,
	connection_key: string,
	candidate_name: string,
	candidate_kind: string,
	allocator: mem.Allocator,
) -> (Candidate_Cache_Status, Store_Error) {
	r, err := reader(store, allocator)
	if err != .None {
		return .Missing, err
	}
	defer reader_destroy(&r)
	return reader_find_cached_candidate(&r, profile, connection_key, candidate_name, candidate_kind, allocator)
}

lookup_symbol :: proc(
	store: ^Dependency_Store,
	profile: ^Dependency_Profile,
	symbol_name: string,
	candidate_kind: string,
	allocator: mem.Allocator,
) -> (Symbol_Lookup_Result, bool, Store_Error) {
	r, err := reader(store, allocator)
	if err != .None {
		return {}, false, err
	}
	defer reader_destroy(&r)
	return reader_lookup_symbol(&r, profile, symbol_name, candidate_kind, allocator)
}

lookup_artifact_symbol :: proc(
	store: ^Dependency_Store,
	artifact_id: i64,
	symbol_name: string,
	symbol_kinds: []string,
	allocator: mem.Allocator,
) -> (Symbol_Lookup_Result, bool, Store_Error) {
	r, err := reader(store, allocator)
	if err != .None {
		return {}, false, err
	}
	defer reader_destroy(&r)
	return reader_lookup_artifact_symbol(&r, artifact_id, symbol_name, symbol_kinds, allocator)
}

read_artifact_source :: proc(
	store: ^Dependency_Store,
	artifact_id: i64,
	allocator: mem.Allocator,
) -> (Stored_Artifact_Record, bool, Store_Error) {
	r, err := reader(store, allocator)
	if err != .None {
		return {}, false, err
	}
	defer reader_destroy(&r)
	return reader_read_artifact_source(&r, artifact_id, allocator)
}

find_artifact_for_candidate :: proc(
	store: ^Dependency_Store,
	profile: ^Dependency_Profile,
	candidate_name: string,
	candidate_kind: string,
	allocator: mem.Allocator,
) -> (Stored_Artifact_Record, bool, Store_Error) {
	r, err := reader(store, allocator)
	if err != .None {
		return {}, false, err
	}
	defer reader_destroy(&r)
	return reader_find_artifact_for_candidate(&r, profile, candidate_name, candidate_kind, allocator)
}

list_artifacts_by_kind :: proc(
	store: ^Dependency_Store,
	profile: ^Dependency_Profile,
	object_kind: string,
	allocator: mem.Allocator,
) -> ([dynamic]Stored_Artifact_Record, Store_Error) {
	r, err := reader(store, allocator)
	if err != .None {
		return make([dynamic]Stored_Artifact_Record, allocator), err
	}
	defer reader_destroy(&r)
	return reader_list_artifacts_by_kind(&r, profile, object_kind, allocator)
}

record_negative_lookup :: proc(
	store: ^Dependency_Store,
	profile: ^Dependency_Profile,
	connection_key: string,
	candidate_name: string,
	candidate_kind: string,
	recorded_at: string,
	allocator: mem.Allocator,
) -> Store_Error {
	db, err := open_connection(store.path, allocator)
	if err != .None {
		return err
	}
	defer sqlite3.close(db)

	if err = exec_sql(db, "BEGIN", allocator); err != .None {
		return err
	}
	stmt: ^sqlite3.Statement
	stmt, err = prepare(
		db,
		`
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
`,
		allocator,
	)
	if err != .None {
		exec_sql(db, "ROLLBACK", allocator)
		return err
	}
	defer sqlite3.finalize(stmt)

	profile_key_text := profile_key(profile, allocator)
	product := normalized_product_version(profile, allocator)
	connection := normalize_name(connection_key, allocator)
	kind := normalize_name(candidate_kind, allocator)
	name := normalize_name(candidate_name, allocator)

	bind_text(stmt, 1, profile_key_text)
	bind_text(stmt, 2, product)
	bind_text(stmt, 3, connection)
	bind_text(stmt, 4, kind)
	bind_text(stmt, 5, name)
	bind_text(stmt, 6, strings.trim_space(recorded_at))
	if step_done(stmt) != .None {
		exec_sql(db, "ROLLBACK", allocator)
		return .Sqlite
	}
	return exec_sql(db, "COMMIT", allocator)
}

clear_profile_scope :: proc(
	store: ^Dependency_Store,
	profile: ^Dependency_Profile,
	allocator: mem.Allocator,
) -> Store_Error {
	db, err := open_connection(store.path, allocator)
	if err != .None {
		return err
	}
	defer sqlite3.close(db)
	if err = exec_sql(db, "BEGIN", allocator); err != .None {
		return err
	}

	package_versions := package_version_set(profile, allocator)
	sql := strings.builder_make(allocator)
	defer strings.builder_destroy(&sql)
	strings.write_string(
		&sql,
		"DELETE FROM dependency_artifacts WHERE product_version = ? AND package_version IN (",
	)
	append_placeholders(&sql, len(package_versions))
	strings.write_byte(&sql, ')')

	stmt, prep_err := prepare(db, strings.to_string(sql), allocator)
	if prep_err != .None {
		exec_sql(db, "ROLLBACK", allocator)
		return prep_err
	}
	bind_text(stmt, 1, normalized_product_version(profile, allocator))
	for value, i in package_versions {
		bind_text(stmt, 2 + i, value)
	}
	if step_done(stmt) != .None {
		sqlite3.finalize(stmt)
		exec_sql(db, "ROLLBACK", allocator)
		return .Sqlite
	}
	sqlite3.finalize(stmt)

	stmt, prep_err = prepare(
		db,
		"DELETE FROM dependency_negative_lookups WHERE profile_key = ?1",
		allocator,
	)
	if prep_err != .None {
		exec_sql(db, "ROLLBACK", allocator)
		return prep_err
	}
	bind_text(stmt, 1, profile_key(profile, allocator))
	if step_done(stmt) != .None {
		sqlite3.finalize(stmt)
		exec_sql(db, "ROLLBACK", allocator)
		return .Sqlite
	}
	sqlite3.finalize(stmt)
	return exec_sql(db, "COMMIT", allocator)
}

reader_find_cached_candidate :: proc(
	r: ^Dependency_Store_Reader,
	profile: ^Dependency_Profile,
	connection_key: string,
	candidate_name: string,
	candidate_kind: string,
	allocator: mem.Allocator,
) -> (Candidate_Cache_Status, Store_Error) {
	name := normalize_name(candidate_name, allocator)
	kind := normalize_name(candidate_kind, allocator)
	if name == "" || kind == "" {
		return .Missing, .None
	}
	exists, err := candidate_artifact_exists(r.connection, profile, name, kind, allocator)
	if err != .None {
		return .Missing, err
	}
	if exists {
		return .Artifact, .None
	}

	stmt: ^sqlite3.Statement
	stmt, err = prepare(
		r.connection,
		`
SELECT 1
FROM dependency_negative_lookups
WHERE profile_key = ?1
  AND connection_key = ?2
  AND candidate_kind = ?3
  AND candidate_name = ?4
LIMIT 1
`,
		allocator,
	)
	if err != .None {
		return .Missing, err
	}
	defer sqlite3.finalize(stmt)
	bind_text(stmt, 1, profile_key(profile, allocator))
	bind_text(stmt, 2, normalize_name(connection_key, allocator))
	bind_text(stmt, 3, kind)
	bind_text(stmt, 4, name)
	code := sqlite3.step(stmt)
	if code == sqlite3.ROW {
		return .Negative, .None
	}
	if code == sqlite3.DONE {
		return .Missing, .None
	}
	return .Missing, .Sqlite
}

reader_lookup_symbol :: proc(
	r: ^Dependency_Store_Reader,
	profile: ^Dependency_Profile,
	symbol_name: string,
	candidate_kind: string,
	allocator: mem.Allocator,
) -> (Symbol_Lookup_Result, bool, Store_Error) {
	allowed_kinds := make([dynamic]string, 0, 8, allocator)
	allowed_symbol_kinds := make([dynamic]string, 0, 8, allocator)
	candidate_artifact_kinds(candidate_kind, &allowed_kinds, allocator)
	candidate_symbol_kinds(candidate_kind, &allowed_symbol_kinds)
	name := normalize_name(symbol_name, allocator)
	if len(allowed_kinds) == 0 || len(allowed_symbol_kinds) == 0 || name == "" {
		return {}, false, .None
	}

	package_versions := package_version_set(profile, allocator)
	sql := strings.builder_make(allocator)
	defer strings.builder_destroy(&sql)
	strings.write_string(
		&sql,
		`
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
  AND symbol.symbol_kind IN (`,
	)
	append_placeholders(&sql, len(allowed_symbol_kinds))
	strings.write_string(&sql, ") AND artifact.package_version IN (")
	append_placeholders(&sql, len(package_versions))
	strings.write_string(&sql, ") AND artifact.object_kind IN (")
	append_placeholders(&sql, len(allowed_kinds))
	strings.write_string(
		&sql,
		") ORDER BY symbol.priority DESC, artifact.package_name ASC LIMIT 1",
	)

	stmt, err := prepare(r.connection, strings.to_string(sql), allocator)
	if err != .None {
		return {}, false, err
	}
	defer sqlite3.finalize(stmt)
	bind_text(stmt, 1, normalized_product_version(profile, allocator))
	bind_text(stmt, 2, name)
	index := 3
	index = bind_text_list(stmt, index, allowed_symbol_kinds[:])
	index = bind_text_list(stmt, index, package_versions[:])
	bind_text_list(stmt, index, allowed_kinds[:])
	return step_symbol_lookup(stmt, allocator)
}

reader_lookup_artifact_symbol :: proc(
	r: ^Dependency_Store_Reader,
	artifact_id: i64,
	symbol_name: string,
	symbol_kinds: []string,
	allocator: mem.Allocator,
) -> (Symbol_Lookup_Result, bool, Store_Error) {
	name := normalize_name(symbol_name, allocator)
	kinds := make([dynamic]string, 0, len(symbol_kinds), allocator)
	for kind in symbol_kinds {
		normalized := normalize_name(kind, allocator)
		if normalized != "" {
			append(&kinds, normalized)
		}
	}
	if name == "" || len(kinds) == 0 {
		return {}, false, .None
	}

	sql := strings.builder_make(allocator)
	defer strings.builder_destroy(&sql)
	strings.write_string(
		&sql,
		`
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
WHERE artifact.id = ?
  AND symbol.symbol_name = ?
  AND symbol.symbol_kind IN (`,
	)
	append_placeholders(&sql, len(kinds))
	strings.write_string(&sql, ") ORDER BY symbol.priority DESC LIMIT 1")

	stmt, err := prepare(r.connection, strings.to_string(sql), allocator)
	if err != .None {
		return {}, false, err
	}
	defer sqlite3.finalize(stmt)
	bind_i64(stmt, 1, artifact_id)
	bind_text(stmt, 2, name)
	bind_text_list(stmt, 3, kinds[:])
	return step_symbol_lookup(stmt, allocator)
}

reader_read_artifact_source :: proc(
	r: ^Dependency_Store_Reader,
	artifact_id: i64,
	allocator: mem.Allocator,
) -> (Stored_Artifact_Record, bool, Store_Error) {
	stmt, err := prepare(
		r.connection,
		`
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
`,
		allocator,
	)
	if err != .None {
		return {}, false, err
	}
	defer sqlite3.finalize(stmt)
	bind_i64(stmt, 1, artifact_id)
	return step_artifact_record(stmt, allocator)
}

reader_find_artifact_for_candidate :: proc(
	r: ^Dependency_Store_Reader,
	profile: ^Dependency_Profile,
	candidate_name: string,
	candidate_kind: string,
	allocator: mem.Allocator,
) -> (Stored_Artifact_Record, bool, Store_Error) {
	allowed_kinds := make([dynamic]string, 0, 8, allocator)
	candidate_artifact_kinds(candidate_kind, &allowed_kinds, allocator)
	name := normalize_name(candidate_name, allocator)
	if len(allowed_kinds) == 0 || name == "" {
		return {}, false, .None
	}
	package_versions := package_version_set(profile, allocator)

	sql := strings.builder_make(allocator)
	defer strings.builder_destroy(&sql)
	strings.write_string(
		&sql,
		`
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
  AND package_version IN (`,
	)
	append_placeholders(&sql, len(package_versions))
	strings.write_string(&sql, ") AND object_kind IN (")
	append_placeholders(&sql, len(allowed_kinds))
	strings.write_string(&sql, ") ORDER BY CASE object_kind")
	for _ in allowed_kinds {
		strings.write_string(&sql, " WHEN ? THEN ?")
	}
	strings.write_string(&sql, " ELSE ? END, package_name ASC, object_name ASC LIMIT 1")

	stmt, err := prepare(r.connection, strings.to_string(sql), allocator)
	if err != .None {
		return {}, false, err
	}
	defer sqlite3.finalize(stmt)
	bind_text(stmt, 1, normalized_product_version(profile, allocator))
	bind_text(stmt, 2, name)
	index := 3
	index = bind_text_list(stmt, index, package_versions[:])
	index = bind_text_list(stmt, index, allowed_kinds[:])
	for kind, rank in allowed_kinds {
		bind_text(stmt, index, kind)
		bind_i64(stmt, index + 1, i64(rank))
		index += 2
	}
	bind_i64(stmt, index, i64(len(allowed_kinds)))
	return step_artifact_record(stmt, allocator)
}

reader_list_artifacts_by_kind :: proc(
	r: ^Dependency_Store_Reader,
	profile: ^Dependency_Profile,
	object_kind: string,
	allocator: mem.Allocator,
) -> ([dynamic]Stored_Artifact_Record, Store_Error) {
	records := make([dynamic]Stored_Artifact_Record, 0, 4, allocator)
	kind := normalize_name(object_kind, allocator)
	if kind == "" {
		return records, .None
	}
	package_versions := package_version_set(profile, allocator)
	sql := strings.builder_make(allocator)
	defer strings.builder_destroy(&sql)
	strings.write_string(
		&sql,
		`
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
  AND package_version IN (`,
	)
	append_placeholders(&sql, len(package_versions))
	strings.write_string(&sql, ") ORDER BY package_name ASC, object_name ASC")

	stmt, err := prepare(r.connection, strings.to_string(sql), allocator)
	if err != .None {
		return records, err
	}
	defer sqlite3.finalize(stmt)
	bind_text(stmt, 1, normalized_product_version(profile, allocator))
	bind_text(stmt, 2, kind)
	bind_text_list(stmt, 3, package_versions[:])

	for {
		code := sqlite3.step(stmt)
		if code == sqlite3.DONE {
			return records, .None
		}
		if code != sqlite3.ROW {
			return records, .Sqlite
		}
		append(&records, artifact_record_from_row(stmt, allocator))
	}
}

resolve_dependency_store_path :: proc(override_path: string, allocator: mem.Allocator) -> (
	string,
	bool,
) {
	path := strings.trim_space(override_path)
	if path != "" {
		if filepath.is_abs(path) {
			return strings.clone(path, allocator), true
		}
		cwd, err := os.get_working_directory(allocator)
		if err != nil {
			return "", false
		}
		joined, join_err := filepath.join({cwd, path}, allocator)
		return joined, join_err == nil
	}
	return dependency_store_default_path(allocator)
}

dependency_store_default_path :: proc(allocator: mem.Allocator) -> (string, bool) {
	base := ""
	when ODIN_OS == .Windows {
		if value, ok := os.lookup_env("LOCALAPPDATA", allocator); ok {
			base = value
		} else if home, home_ok := os.lookup_env("USERPROFILE", allocator); home_ok {
			base = filepath.join({home, "AppData", "Local"}, allocator) or_else ""
		}
	} else when ODIN_OS == .Darwin {
		if home, ok := os.lookup_env("HOME", allocator); ok {
			base = filepath.join({home, "Library", "Caches"}, allocator) or_else ""
		}
	} else {
		if value, ok := os.lookup_env("XDG_CACHE_HOME", allocator); ok {
			base = value
		} else if home, home_ok := os.lookup_env("HOME", allocator); home_ok {
			base = filepath.join({home, ".cache"}, allocator) or_else ""
		}
	}
	if base == "" {
		return "", false
	}
	path, err := filepath.join({base, "abap-ls", "dependency-cache.sqlite3"}, allocator)
	return path, err == nil
}

sqlite_file_uri :: proc(path: string, query: string, allocator: mem.Allocator) -> string {
	normalized := strings.builder_make(allocator)
	defer strings.builder_destroy(&normalized)
	if len(path) > 1 && path[1] == ':' {
		strings.write_byte(&normalized, '/')
	}
	for ch in path {
		if ch == '\\' {
			strings.write_byte(&normalized, '/')
		} else {
			strings.write_rune(&normalized, ch)
		}
	}

	out := strings.builder_make(allocator)
	strings.write_string(&out, "file://")
	percent_encode_sqlite_uri_path(&out, strings.to_string(normalized))
	strings.write_byte(&out, '?')
	strings.write_string(&out, query)
	return strings.to_string(out)
}

normalized_product_version :: proc(profile: ^Dependency_Profile, allocator: mem.Allocator) -> string {
	return normalize_name(profile.product_version, allocator)
}

package_version_for :: proc(
	profile: ^Dependency_Profile,
	package_name: string,
	allocator: mem.Allocator,
) -> string {
	normalized_package := normalize_name(package_name, allocator)
	for pkg in profile.packages {
		if normalize_name(pkg.package_name, allocator) == normalized_package {
			return normalize_name(pkg.version, allocator)
		}
	}
	return normalize_name(profile.default_package_version, allocator)
}

package_version_set :: proc(profile: ^Dependency_Profile, allocator: mem.Allocator) -> [dynamic]string {
	versions := make([dynamic]string, 0, 1 + len(profile.packages), allocator)
	insert_unique_string(&versions, normalize_name(profile.default_package_version, allocator))
	for pkg in profile.packages {
		insert_unique_string(&versions, normalize_name(pkg.version, allocator))
	}
	sort_strings(versions[:])
	return versions
}

profile_key :: proc(profile: ^Dependency_Profile, allocator: mem.Allocator) -> string {
	packages := make([dynamic]Package_Version, 0, len(profile.packages), allocator)
	for pkg in profile.packages {
		append(
			&packages,
			Package_Version {
				package_name = normalize_name(pkg.package_name, allocator),
				version      = normalize_name(pkg.version, allocator),
			},
		)
	}
	sort_packages(packages[:])

	out := strings.builder_make(allocator)
	strings.write_string(&out, normalized_product_version(profile, allocator))
	strings.write_byte(&out, '|')
	strings.write_string(&out, normalize_name(profile.default_package_version, allocator))
	for pkg in packages {
		strings.write_byte(&out, '|')
		strings.write_string(&out, pkg.package_name)
		strings.write_byte(&out, '=')
		strings.write_string(&out, pkg.version)
	}
	return strings.to_string(out)
}

open_connection :: proc(path: string, allocator: mem.Allocator) -> (^sqlite3.Connection, Store_Error) {
	parent := filepath.dir(path)
	if parent != "" && parent != "." {
		if os.make_directory_all(parent) != nil {
			return nil, .Create_Directory
		}
	}
	db: ^sqlite3.Connection
	flags := sqlite_open_flags(sqlite3.OPEN_READWRITE, sqlite3.OPEN_CREATE, sqlite3.OPEN_URI)
	cpath := cstring_buffer(path, allocator)
	defer delete(cpath, allocator)
	if sqlite3.open_v2(cstring(raw_data(cpath)), &db, flags, nil) != sqlite3.OK {
		return nil, .Sqlite
	}
	if sqlite3.busy_timeout(db, 5000) != sqlite3.OK {
		sqlite3.close(db)
		return nil, .Sqlite
	}
	setup_sql := [?]string {
		"PRAGMA journal_mode = WAL",
		"PRAGMA foreign_keys = ON",
		MIGRATION_SQL,
		"INSERT INTO schema_meta(key, value) VALUES('schema_version', '1') ON CONFLICT(key) DO UPDATE SET value = excluded.value",
	}
	for sql in setup_sql {
		if exec_sql(db, sql, allocator) != .None {
			sqlite3.close(db)
			return nil, .Sqlite
		}
	}
	return db, .None
}

open_read_connection :: proc(path: string, allocator: mem.Allocator) -> (^sqlite3.Connection, Store_Error) {
	flags := sqlite_open_flags(sqlite3.OPEN_READONLY, sqlite3.OPEN_URI)
	uri := sqlite_file_uri(path, "mode=ro", allocator)
	db, err := open_sqlite_filename(uri, flags, allocator)
	if err != .None {
		return nil, err
	}
	sqlite3.busy_timeout(db, 5000)
	if validate_read_connection(db, allocator) {
		return db, .None
	}
	sqlite3.close(db)

	immutable_uri := sqlite_file_uri(path, "mode=ro&immutable=1", allocator)
	db, err = open_sqlite_filename(immutable_uri, flags, allocator)
	if err != .None {
		return nil, err
	}
	sqlite3.busy_timeout(db, 5000)
	return db, .None
}

open_sqlite_filename :: proc(
	filename: string,
	flags: sqlite3.Open_Flags,
	allocator: mem.Allocator,
) -> (^sqlite3.Connection, Store_Error) {
	db: ^sqlite3.Connection
	cfilename := cstring_buffer(filename, allocator)
	defer delete(cfilename, allocator)
	if sqlite3.open_v2(cstring(raw_data(cfilename)), &db, flags, nil) != sqlite3.OK {
		return nil, .Sqlite
	}
	return db, .None
}

validate_read_connection :: proc(db: ^sqlite3.Connection, allocator: mem.Allocator) -> bool {
	stmt, err := prepare(db, "SELECT name FROM sqlite_master LIMIT 1", allocator)
	if err != .None {
		return false
	}
	defer sqlite3.finalize(stmt)
	return sqlite3.step(stmt) == sqlite3.ROW
}

put_artifact_in_tx :: proc(
	db: ^sqlite3.Connection,
	profile: ^Dependency_Profile,
	artifact: ^Stored_Artifact_Input,
	allocator: mem.Allocator,
) -> (i64, Store_Error) {
	stmt, err := prepare(
		db,
		`
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
`,
		allocator,
	)
	if err != .None {
		return 0, err
	}
	package_name := normalize_name(artifact.package_name, allocator)
	package_version := package_version_for(profile, package_name, allocator)
	object_kind := normalize_name(artifact.object_kind, allocator)
	object_name := normalize_name(artifact.object_name, allocator)
	bind_text(stmt, 1, normalized_product_version(profile, allocator))
	bind_text(stmt, 2, package_name)
	bind_text(stmt, 3, package_version)
	bind_text(stmt, 4, object_kind)
	bind_text(stmt, 5, object_name)
	bind_text(stmt, 6, strings.trim_space(artifact.object_uri))
	bind_text(stmt, 7, strings.trim_space(artifact.object_type))
	bind_text(stmt, 8, strings.trim_space(artifact.description))
	bind_text(stmt, 9, normalize_name(artifact.file_extension, allocator))
	bind_text(stmt, 10, artifact.source_text)
	bind_text(stmt, 11, strings.trim_space(artifact.fetched_at))
	if step_done(stmt) != .None {
		sqlite3.finalize(stmt)
		return 0, .Sqlite
	}
	sqlite3.finalize(stmt)

	stmt, err = prepare(
		db,
		`
SELECT id
FROM dependency_artifacts
WHERE product_version = ?1
  AND package_name = ?2
  AND package_version = ?3
  AND object_kind = ?4
  AND object_name = ?5
`,
		allocator,
	)
	if err != .None {
		return 0, err
	}
	bind_text(stmt, 1, normalized_product_version(profile, allocator))
	bind_text(stmt, 2, package_name)
	bind_text(stmt, 3, package_version)
	bind_text(stmt, 4, object_kind)
	bind_text(stmt, 5, object_name)
	if sqlite3.step(stmt) != sqlite3.ROW {
		sqlite3.finalize(stmt)
		return 0, .Sqlite
	}
	artifact_id := i64(sqlite3.column_int64(stmt, 0))
	sqlite3.finalize(stmt)

	stmt, err = prepare(db, "DELETE FROM dependency_symbol_index WHERE artifact_id = ?1", allocator)
	if err != .None {
		return 0, err
	}
	bind_i64(stmt, 1, artifact_id)
	if step_done(stmt) != .None {
		sqlite3.finalize(stmt)
		return 0, .Sqlite
	}
	sqlite3.finalize(stmt)

	for symbol in artifact.symbols {
		stmt, err = prepare(
			db,
			`
INSERT INTO dependency_symbol_index (
    artifact_id,
    symbol_name,
    symbol_kind,
    range_start,
    range_end,
    priority
) VALUES (?1, ?2, ?3, ?4, ?5, ?6)
`,
			allocator,
		)
		if err != .None {
			return 0, err
		}
		bind_i64(stmt, 1, artifact_id)
		bind_text(stmt, 2, normalize_name(symbol.symbol_name, allocator))
		bind_text(stmt, 3, normalize_name(symbol.symbol_kind, allocator))
		bind_i64(stmt, 4, i64(symbol.range_start))
		bind_i64(stmt, 5, i64(symbol.range_end))
		bind_i64(stmt, 6, symbol.priority)
		if step_done(stmt) != .None {
			sqlite3.finalize(stmt)
			return 0, .Sqlite
		}
		sqlite3.finalize(stmt)
	}

	return artifact_id, .None
}

candidate_artifact_exists :: proc(
	db: ^sqlite3.Connection,
	profile: ^Dependency_Profile,
	candidate_name: string,
	candidate_kind: string,
	allocator: mem.Allocator,
) -> (bool, Store_Error) {
	allowed_kinds := make([dynamic]string, 0, 8, allocator)
	candidate_artifact_kinds(candidate_kind, &allowed_kinds, allocator)
	if len(allowed_kinds) == 0 {
		return false, .None
	}
	package_versions := package_version_set(profile, allocator)
	sql := strings.builder_make(allocator)
	defer strings.builder_destroy(&sql)
	strings.write_string(
		&sql,
		"SELECT 1 FROM dependency_artifacts WHERE product_version = ? AND object_name = ? AND package_version IN (",
	)
	append_placeholders(&sql, len(package_versions))
	strings.write_string(&sql, ") AND object_kind IN (")
	append_placeholders(&sql, len(allowed_kinds))
	strings.write_string(&sql, ") LIMIT 1")

	stmt, err := prepare(db, strings.to_string(sql), allocator)
	if err != .None {
		return false, err
	}
	defer sqlite3.finalize(stmt)
	bind_text(stmt, 1, normalized_product_version(profile, allocator))
	bind_text(stmt, 2, candidate_name)
	index := bind_text_list(stmt, 3, package_versions[:])
	bind_text_list(stmt, index, allowed_kinds[:])
	code := sqlite3.step(stmt)
	if code == sqlite3.ROW {
		return true, .None
	}
	if code == sqlite3.DONE {
		return false, .None
	}
	return false, .Sqlite
}

candidate_artifact_kinds :: proc(kind: string, out: ^[dynamic]string, allocator: mem.Allocator) {
	trimmed := strings.trim_space(kind)
	switch {
	case ascii_equal_ignore_case(trimmed, "include"):
		append(out, "include")
	case ascii_equal_ignore_case(trimmed, "message-class"):
		append(out, "message-class")
	case ascii_equal_ignore_case(trimmed, "report"):
		append(out, "report")
	case ascii_equal_ignore_case(trimmed, "function"):
		append(out, "function-module")
	case ascii_equal_ignore_case(trimmed, "static"):
		append(out, "global-class")
		append(out, "global-interface")
	case ascii_equal_ignore_case(trimmed, "type") || ascii_equal_ignore_case(trimmed, "symbol"):
		kinds := [?]string {
			"global-class",
			"global-interface",
			"report",
			"ddic-data-element",
			"ddic-domain",
			"ddic-structure",
			"ddic-table",
			"ddic-table-type",
			"ddic-view",
		}
		for v in kinds {
			append(out, v)
		}
	case trimmed != "":
		append(out, normalize_name(trimmed, allocator))
	}
}

candidate_symbol_kinds :: proc(kind: string, out: ^[dynamic]string) {
	trimmed := strings.trim_space(kind)
	switch {
	case ascii_equal_ignore_case(trimmed, "include"):
		append(out, "include")
	case ascii_equal_ignore_case(trimmed, "message-class"):
		append(out, "typedef")
	case ascii_equal_ignore_case(trimmed, "report"):
		append(out, "report")
	case ascii_equal_ignore_case(trimmed, "function"):
		append(out, "function-module")
		append(out, "module")
	case ascii_equal_ignore_case(trimmed, "static"):
		append(out, "class")
		append(out, "interface")
	case ascii_equal_ignore_case(trimmed, "type"):
		append(out, "class")
		append(out, "interface")
		append(out, "typedef")
		append(out, "report")
	case ascii_equal_ignore_case(trimmed, "symbol"):
		kinds := [?]string {
			"class",
			"interface",
			"typedef",
			"report",
			"include",
			"form",
			"module",
			"function-module",
			"variable",
			"constant",
			"class-member",
		}
		for v in kinds {
			append(out, v)
		}
	}
}

step_symbol_lookup :: proc(
	stmt: ^sqlite3.Statement,
	allocator: mem.Allocator,
) -> (Symbol_Lookup_Result, bool, Store_Error) {
	code := sqlite3.step(stmt)
	if code == sqlite3.DONE {
		return {}, false, .None
	}
	if code != sqlite3.ROW {
		return {}, false, .Sqlite
	}
	return Symbol_Lookup_Result {
		artifact_id     = i64(sqlite3.column_int64(stmt, 0)),
		package_name    = column_string(stmt, 1, allocator),
		package_version = column_string(stmt, 2, allocator),
		object_kind     = column_string(stmt, 3, allocator),
		object_name     = column_string(stmt, 4, allocator),
		file_extension  = column_string(stmt, 5, allocator),
		range_start     = int(sqlite3.column_int64(stmt, 6)),
		range_end       = int(sqlite3.column_int64(stmt, 7)),
	}, true, .None
}

step_artifact_record :: proc(
	stmt: ^sqlite3.Statement,
	allocator: mem.Allocator,
) -> (Stored_Artifact_Record, bool, Store_Error) {
	code := sqlite3.step(stmt)
	if code == sqlite3.DONE {
		return {}, false, .None
	}
	if code != sqlite3.ROW {
		return {}, false, .Sqlite
	}
	return artifact_record_from_row(stmt, allocator), true, .None
}

artifact_record_from_row :: proc(
	stmt: ^sqlite3.Statement,
	allocator: mem.Allocator,
) -> Stored_Artifact_Record {
	return Stored_Artifact_Record {
		artifact_id     = i64(sqlite3.column_int64(stmt, 0)),
		package_name    = column_string(stmt, 1, allocator),
		package_version = column_string(stmt, 2, allocator),
		object_kind     = column_string(stmt, 3, allocator),
		object_name     = column_string(stmt, 4, allocator),
		object_uri      = column_string(stmt, 5, allocator),
		object_type     = column_string(stmt, 6, allocator),
		description     = column_string(stmt, 7, allocator),
		file_extension  = column_string(stmt, 8, allocator),
		source_text     = column_string(stmt, 9, allocator),
	}
}

append_placeholders :: proc(out: ^strings.Builder, count: int) {
	for i in 0 ..< count {
		if i > 0 {
			strings.write_string(out, ", ")
		}
		strings.write_byte(out, '?')
	}
}

normalize_name :: proc(value: string, allocator: mem.Allocator) -> string {
	trimmed := strings.trim_space(value)
	out := make([]byte, len(trimmed), allocator)
	for ch, i in transmute([]byte)trimmed {
		out[i] = ascii_lower(ch)
	}
	return string(out)
}

percent_encode_sqlite_uri_path :: proc(out: ^strings.Builder, path: string) {
	for byte in transmute([]byte)path {
		if ascii_uri_byte(byte) {
			strings.write_byte(out, byte)
		} else {
			strings.write_byte(out, '%')
			strings.write_byte(out, hex_digit(byte >> 4))
			strings.write_byte(out, hex_digit(byte & 0x0f))
		}
	}
}

hex_digit :: proc(value: byte) -> byte {
	if value <= 9 {
		return '0' + value
	}
	return 'A' + (value - 10)
}

ascii_uri_byte :: proc(value: byte) -> bool {
	return(
		('0' <= value && value <= '9') ||
		('A' <= value && value <= 'Z') ||
		('a' <= value && value <= 'z') ||
		value == '/' ||
		value == ':' ||
		value == '-' ||
		value == '_' ||
		value == '.' ||
		value == '~' \
	)
}

ascii_lower :: proc(value: byte) -> byte {
	if 'A' <= value && value <= 'Z' {
		return value + ('a' - 'A')
	}
	return value
}

ascii_equal_ignore_case :: proc(a, b: string) -> bool {
	if len(a) != len(b) {
		return false
	}
	for ch, i in transmute([]byte)a {
		if ascii_lower(ch) != ascii_lower(b[i]) {
			return false
		}
	}
	return true
}

insert_unique_string :: proc(values: ^[dynamic]string, value: string) {
	for existing in values {
		if existing == value {
			return
		}
	}
	append(values, value)
}

sort_strings :: proc(values: []string) {
	for i in 1 ..< len(values) {
		value := values[i]
		j := i
		for j > 0 && values[j - 1] > value {
			values[j] = values[j - 1]
			j -= 1
		}
		values[j] = value
	}
}

sort_packages :: proc(values: []Package_Version) {
	for i in 1 ..< len(values) {
		value := values[i]
		j := i
		for j > 0 && values[j - 1].package_name > value.package_name {
			values[j] = values[j - 1]
			j -= 1
		}
		values[j] = value
	}
}

sqlite_open_flags :: proc(flags: ..sqlite3.Open_Flags) -> sqlite3.Open_Flags {
	out: c.int
	for flag in flags {
		out |= c.int(flag)
	}
	return sqlite3.Open_Flags(out)
}

prepare :: proc(
	db: ^sqlite3.Connection,
	sql: string,
	allocator: mem.Allocator,
) -> (^sqlite3.Statement, Store_Error) {
	stmt: ^sqlite3.Statement
	csql := cstring_buffer(sql, allocator)
	defer delete(csql, allocator)
	if sqlite3.prepare_v2(db, cstring(raw_data(csql)), -1, &stmt, nil) != sqlite3.OK {
		return nil, .Sqlite
	}
	return stmt, .None
}

exec_sql :: proc(db: ^sqlite3.Connection, sql: string, allocator: mem.Allocator) -> Store_Error {
	csql := cstring_buffer(sql, allocator)
	defer delete(csql, allocator)
	if sqlite3.exec(db, cstring(raw_data(csql)), nil, nil, nil) != sqlite3.OK {
		return .Sqlite
	}
	return .None
}

step_done :: proc(stmt: ^sqlite3.Statement) -> Store_Error {
	return .None if sqlite3.step(stmt) == sqlite3.DONE else .Sqlite
}

bind_text :: proc(stmt: ^sqlite3.Statement, index: int, value: string) -> Store_Error {
	if sqlite3.bind_text(
		stmt,
		c.int(index),
		strings.unsafe_string_to_cstring(value),
		c.int(len(value)),
		sqlite3.DESTRUCTOR_TRANSIENT,
	) != sqlite3.OK {
		return .Sqlite
	}
	return .None
}

bind_i64 :: proc(stmt: ^sqlite3.Statement, index: int, value: i64) -> Store_Error {
	if sqlite3.bind_int64(stmt, c.int(index), c.int64_t(value)) != sqlite3.OK {
		return .Sqlite
	}
	return .None
}

bind_text_list :: proc(stmt: ^sqlite3.Statement, start: int, values: []string) -> int {
	index := start
	for value in values {
		bind_text(stmt, index, value)
		index += 1
	}
	return index
}

column_string :: proc(
	stmt: ^sqlite3.Statement,
	index: int,
	allocator: mem.Allocator,
) -> string {
	text := sqlite3.column_text(stmt, c.int(index))
	if text == nil {
		return ""
	}
	return strings.clone_from_cstring(text, allocator)
}

cstring_buffer :: proc(value: string, allocator: mem.Allocator) -> []byte {
	buf := make([]byte, len(value) + 1, allocator)
	copy(buf, value)
	buf[len(value)] = 0
	return buf
}
