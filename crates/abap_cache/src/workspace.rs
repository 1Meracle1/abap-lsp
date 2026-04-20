use std::borrow::Cow;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use quick_xml::Reader;
use quick_xml::events::{BytesStart, Event};
use serde::Deserialize;

pub const DEPENDENCY_MODE_REMOTE_ON_DEMAND: &str = "remote-on-demand";
pub const DEPENDENCY_MODE_LOCAL_FIRST: &str = "local-first";
pub const UNKNOWN_SYMBOL_MODE_REMOTE: &str = "remote";
pub const UNKNOWN_SYMBOL_MODE_LOG: &str = "log";
pub const WORKSPACE_PERFORMANCE_MODE_AUTO: &str = "auto";
pub const WORKSPACE_PERFORMANCE_MODE_EDITOR_FIRST: &str = "editor-first";
pub const WORKSPACE_PERFORMANCE_MODE_FULL_WORKSPACE: &str = "full-workspace";
pub const DEFAULT_REMOTE_REQUESTS_PER_SECOND: usize = 24;
pub const EDITOR_FIRST_UNIT_COUNT_THRESHOLD: usize = 1_000;
pub const EDITOR_FIRST_DEPENDENCY_MEMBER_THRESHOLD: usize = 1_000;
pub const EDITOR_FIRST_MANIFEST_BYTES_THRESHOLD: usize = 1_000_000;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WorkspacePerformanceMode {
    Auto,
    EditorFirst,
    FullWorkspace,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct WorkspaceManifest {
    #[serde(default = "default_manifest_version")]
    pub version: i64,
    #[serde(default)]
    pub connection: String,
    #[serde(default)]
    pub resolution: ManifestResolution,
    #[serde(default)]
    pub performance: ManifestPerformance,
    #[serde(default, rename = "unit")]
    pub units: Vec<ManifestUnit>,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct ManifestResolution {
    #[serde(default = "default_dependency_mode")]
    pub dependency_mode: String,
    #[serde(default = "default_cache_dir")]
    pub cache_dir: String,
    #[serde(default = "default_unknown_symbol_mode")]
    pub unknown_symbol_mode: String,
    #[serde(default = "default_remote_requests_per_second")]
    pub remote_requests_per_second: usize,
    #[serde(default, rename = "remote_request_parallelism")]
    legacy_remote_request_parallelism: Option<usize>,
}

impl Default for ManifestResolution {
    fn default() -> Self {
        Self {
            dependency_mode: default_dependency_mode(),
            cache_dir: default_cache_dir(),
            unknown_symbol_mode: default_unknown_symbol_mode(),
            remote_requests_per_second: default_remote_requests_per_second(),
            legacy_remote_request_parallelism: None,
        }
    }
}

impl ManifestResolution {
    pub fn remote_request_parallelism(&self) -> Option<usize> {
        self.legacy_remote_request_parallelism
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct ManifestPerformance {
    #[serde(default = "default_workspace_performance_mode")]
    pub mode: String,
}

impl Default for ManifestPerformance {
    fn default() -> Self {
        Self {
            mode: default_workspace_performance_mode(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ManifestUnit {
    pub name: String,
    pub kind: String,
    pub package_name: String,
    pub root_file: String,
    pub dependency_of: Vec<ManifestUnitDependencyOf>,
    pub members: Vec<ManifestUnitMember>,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
struct ManifestUnitSerde {
    #[serde(default)]
    pub name: String,
    #[serde(default)]
    pub kind: String,
    #[serde(default)]
    pub package_name: String,
    #[serde(default)]
    pub root_file: String,
    #[serde(default, rename = "dependency_of")]
    pub dependency_of: Vec<ManifestUnitDependencyOfInline>,
    #[serde(default)]
    pub members: Vec<ManifestUnitMemberInline>,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(untagged)]
enum ManifestUnitMemberInline {
    File(String),
    Entry(ManifestUnitMember),
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct ManifestUnitDependencyOf {
    #[serde(default)]
    pub file: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(untagged)]
enum ManifestUnitDependencyOfInline {
    File(String),
    Entry(ManifestUnitDependencyOf),
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct ManifestUnitMember {
    #[serde(default)]
    pub role: String,
    #[serde(default)]
    pub file: String,
    #[serde(default)]
    pub object_name: String,
}

impl<'de> Deserialize<'de> for ManifestUnit {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let raw = ManifestUnitSerde::deserialize(deserializer)?;
        Ok(Self {
            name: raw.name,
            kind: raw.kind,
            package_name: raw.package_name,
            root_file: raw.root_file,
            dependency_of: raw
                .dependency_of
                .into_iter()
                .map(ManifestUnitDependencyOf::from)
                .collect(),
            members: raw
                .members
                .into_iter()
                .map(ManifestUnitMember::from)
                .collect(),
        })
    }
}

impl From<ManifestUnitDependencyOfInline> for ManifestUnitDependencyOf {
    fn from(value: ManifestUnitDependencyOfInline) -> Self {
        match value {
            ManifestUnitDependencyOfInline::File(file) => Self { file },
            ManifestUnitDependencyOfInline::Entry(entry) => entry,
        }
    }
}

impl From<ManifestUnitMemberInline> for ManifestUnitMember {
    fn from(value: ManifestUnitMemberInline) -> Self {
        match value {
            ManifestUnitMemberInline::File(file) => Self {
                role: String::new(),
                file,
                object_name: String::new(),
            },
            ManifestUnitMemberInline::Entry(member) => member,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
struct DependencyCacheManifest {
    #[serde(default)]
    pub source_file: String,
    #[serde(default, rename = "unit")]
    pub units: Vec<ManifestUnit>,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Default)]
struct UnitSidecarLocalExport {
    #[serde(default)]
    pub roots: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Default)]
struct UnitSidecarDependencies {
    #[serde(default)]
    pub source: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum LocalDependencySourceMode {
    #[default]
    LocalFirst,
    LocalOnly,
    AdtFirst,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct LocalExportConfig {
    pub mode: LocalDependencySourceMode,
    pub roots: Vec<PathBuf>,
}

impl LocalExportConfig {
    pub fn uses_local_exports(&self) -> bool {
        self.mode != LocalDependencySourceMode::AdtFirst && !self.roots.is_empty()
    }
}

#[derive(Debug, Default)]
pub struct LocalExportResolver {
    indices: HashMap<String, LocalExportIndex>,
}

#[derive(Debug, Default)]
struct LocalExportIndex {
    artifacts_by_file_name: HashMap<String, Vec<LocalExportArtifact>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct LocalExportArtifact {
    path: PathBuf,
    kind_hint: String,
    object_name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Default)]
struct UnitSidecarManifest {
    #[serde(default)]
    pub includes: BTreeMap<String, String>,
    #[serde(default)]
    pub members: Vec<String>,
    #[serde(default)]
    pub local_export: UnitSidecarLocalExport,
    #[serde(default)]
    pub dependencies: UnitSidecarDependencies,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WorkspaceDocument {
    pub uri: Arc<str>,
    pub version: i32,
    pub text: String,
    pub is_dependency: bool,
    pub object_name: Option<Arc<str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WorkspaceLoadResult {
    pub root_uri: Arc<str>,
    pub root_path: PathBuf,
    pub manifest_uri: Arc<str>,
    pub manifest_len_bytes: usize,
    pub manifest: Option<WorkspaceManifest>,
    pub manifest_error: Option<String>,
    pub documents: Vec<WorkspaceDocument>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpenDocumentOverlay {
    pub version: i32,
    pub text: Arc<str>,
}

pub fn load_manifest_from_workspace(root_path: &Path) -> Option<WorkspaceManifest> {
    load_manifest_from_workspace_result(root_path)
        .ok()
        .flatten()
}

pub fn load_manifest_from_workspace_result(
    root_path: &Path,
) -> Result<Option<WorkspaceManifest>, String> {
    let manifest_path = root_path.join("abapls.toml");
    let text = match fs::read_to_string(&manifest_path) {
        Ok(text) => text,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(error) => {
            return Err(format!(
                "failed to read {}: {error}",
                manifest_path.display()
            ));
        }
    };
    let mut manifest: WorkspaceManifest = toml::from_str(&text)
        .map_err(|error| format!("failed to parse {}: {error}", manifest_path.display()))?;
    normalize_manifest(&mut manifest);
    Ok(Some(manifest))
}

pub fn load_workspace_documents(
    root_uri: &str,
    overlays: &HashMap<String, OpenDocumentOverlay>,
) -> WorkspaceLoadResult {
    load_workspace_documents_with_progress(root_uri, overlays, None)
}

struct WorkspaceLoadProgress<'a> {
    callback: Option<&'a (dyn Fn(usize, usize) + Sync)>,
    loaded_document_count: usize,
    total_document_count: usize,
}

impl WorkspaceLoadProgress<'_> {
    fn loaded_document(&mut self) {
        self.loaded_document_count += 1;
        if let Some(callback) = self.callback {
            callback(
                self.loaded_document_count,
                self.total_document_count.saturating_mul(2),
            );
        }
    }
}

pub fn load_workspace_documents_with_progress(
    root_uri: &str,
    overlays: &HashMap<String, OpenDocumentOverlay>,
    progress: Option<&(dyn Fn(usize, usize) + Sync)>,
) -> WorkspaceLoadResult {
    let root_path = file_uri_to_path(root_uri).unwrap_or_default();
    let manifest_uri = Arc::<str>::from(path_to_file_uri(&root_path.join("abapls.toml")));
    let manifest_len_bytes = fs::metadata(root_path.join("abapls.toml"))
        .ok()
        .map(|metadata| metadata.len() as usize)
        .unwrap_or(0);
    let (manifest, manifest_error) = match load_manifest_from_workspace_result(&root_path) {
        Ok(manifest) => (manifest, None),
        Err(error) => (None, Some(error)),
    };
    let cache_dir = manifest_cache_dir(manifest.as_ref()).to_string();
    let manifest_for_loading = manifest
        .as_ref()
        .map(|manifest| manifest_with_discovered_units(manifest, &root_path));
    let total_document_count = progress
        .is_some()
        .then(|| {
            planned_workspace_document_count(
                &root_path,
                root_uri,
                manifest_for_loading.as_ref(),
                &cache_dir,
                overlays,
            )
        })
        .unwrap_or(0);
    let mut load_progress = WorkspaceLoadProgress {
        callback: progress,
        loaded_document_count: 0,
        total_document_count,
    };
    let mut documents = Vec::new();
    let mut seen = HashSet::new();
    let mut effective_manifest = manifest_for_loading.clone();

    if let Some(manifest) = manifest_for_loading.as_ref() {
        let mut loaded_units = manifest.units.clone();
        collect_manifest_documents(
            manifest,
            &root_path,
            root_uri,
            &cache_dir,
            overlays,
            &mut seen,
            &mut documents,
            &mut load_progress,
            &mut loaded_units,
        );
        if let Some(effective_manifest) = effective_manifest.as_mut() {
            effective_manifest.units = loaded_units;
        }
    } else {
        collect_abap_sources(
            &root_path,
            root_uri,
            overlays,
            &mut seen,
            &mut documents,
            false,
            &mut load_progress,
        );
    }
    for (uri, overlay) in overlays {
        if uri_starts_with_workspace(uri, root_uri) && seen.insert(uri.clone()) {
            documents.push(WorkspaceDocument {
                uri: Arc::from(uri.as_str()),
                version: overlay.version,
                text: overlay.text.to_string(),
                is_dependency: false,
                object_name: None,
            });
            load_progress.loaded_document();
        }
    }

    documents.sort_by(|left, right| left.uri.cmp(&right.uri));

    WorkspaceLoadResult {
        root_uri: Arc::from(root_uri),
        root_path,
        manifest_uri,
        manifest_len_bytes,
        manifest: effective_manifest,
        manifest_error,
        documents,
    }
}

fn manifest_with_discovered_units(
    manifest: &WorkspaceManifest,
    root_path: &Path,
) -> WorkspaceManifest {
    if !manifest.units.is_empty() {
        return manifest.clone();
    }

    let mut discovered = manifest.clone();
    discovered.units = discover_conventional_src_units(root_path);
    discovered
}

pub fn manifest_supports_remote_resolution(manifest: Option<&WorkspaceManifest>) -> bool {
    let Some(manifest) = manifest else {
        return false;
    };
    normalize_dependency_mode(&manifest.resolution.dependency_mode)
        == DEPENDENCY_MODE_REMOTE_ON_DEMAND
}

pub fn manifest_cache_dir(manifest: Option<&WorkspaceManifest>) -> &str {
    manifest
        .map(|manifest| manifest.resolution.cache_dir.as_str())
        .filter(|value| !value.trim().is_empty())
        .unwrap_or(".abapls/cache")
}

pub fn normalize_dependency_mode(value: &str) -> &'static str {
    match value.trim().to_ascii_lowercase().as_str() {
        DEPENDENCY_MODE_LOCAL_FIRST => DEPENDENCY_MODE_LOCAL_FIRST,
        _ => DEPENDENCY_MODE_REMOTE_ON_DEMAND,
    }
}

pub fn normalize_unknown_symbol_mode(value: &str) -> &'static str {
    match value.trim().to_ascii_lowercase().as_str() {
        UNKNOWN_SYMBOL_MODE_LOG => UNKNOWN_SYMBOL_MODE_LOG,
        _ => UNKNOWN_SYMBOL_MODE_REMOTE,
    }
}

pub fn normalize_workspace_performance_mode(value: &str) -> &'static str {
    match value.trim().to_ascii_lowercase().as_str() {
        WORKSPACE_PERFORMANCE_MODE_EDITOR_FIRST => WORKSPACE_PERFORMANCE_MODE_EDITOR_FIRST,
        WORKSPACE_PERFORMANCE_MODE_FULL_WORKSPACE => WORKSPACE_PERFORMANCE_MODE_FULL_WORKSPACE,
        _ => WORKSPACE_PERFORMANCE_MODE_AUTO,
    }
}

pub fn resolve_workspace_performance_mode(
    manifest: Option<&WorkspaceManifest>,
    manifest_len_bytes: usize,
) -> WorkspacePerformanceMode {
    let configured = manifest
        .map(|manifest| normalize_workspace_performance_mode(&manifest.performance.mode))
        .unwrap_or(WORKSPACE_PERFORMANCE_MODE_AUTO);
    match configured {
        WORKSPACE_PERFORMANCE_MODE_EDITOR_FIRST => WorkspacePerformanceMode::EditorFirst,
        WORKSPACE_PERFORMANCE_MODE_FULL_WORKSPACE => WorkspacePerformanceMode::FullWorkspace,
        _ => {
            let Some(manifest) = manifest else {
                return WorkspacePerformanceMode::FullWorkspace;
            };
            let unit_count = manifest.units.len();
            let cache_dir = manifest_cache_dir(Some(manifest));
            let dependency_member_count = manifest
                .units
                .iter()
                .flat_map(|unit| unit.members.iter())
                .filter(|member| manifest_member_role(member, cache_dir) == "dependency")
                .count();
            if unit_count >= EDITOR_FIRST_UNIT_COUNT_THRESHOLD
                || dependency_member_count >= EDITOR_FIRST_DEPENDENCY_MEMBER_THRESHOLD
                || manifest_len_bytes >= EDITOR_FIRST_MANIFEST_BYTES_THRESHOLD
            {
                WorkspacePerformanceMode::EditorFirst
            } else {
                WorkspacePerformanceMode::FullWorkspace
            }
        }
    }
}

pub fn workspace_relative_path(root_path: &Path, path: &Path) -> String {
    path.strip_prefix(root_path)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}

pub fn file_uri_to_path(uri: &str) -> Option<PathBuf> {
    let rest = uri.strip_prefix("file:///")?;
    let decoded = percent_decode(rest);
    #[cfg(windows)]
    {
        Some(PathBuf::from(decoded.replace('/', "\\")))
    }
    #[cfg(not(windows))]
    {
        Some(PathBuf::from(format!("/{decoded}")))
    }
}

pub fn path_to_file_uri(path: &Path) -> String {
    let value = path.to_string_lossy().replace('\\', "/");
    let mut out = String::from("file:///");
    let mut first = true;
    for ch in value.chars() {
        if !first && ch == '/' {
            out.push('/');
        } else {
            append_uri_path_char(&mut out, ch);
        }
        first = false;
    }
    out
}

pub fn uri_starts_with_workspace(uri: &str, workspace_uri: &str) -> bool {
    uri == workspace_uri
        || uri
            .strip_prefix(workspace_uri)
            .is_some_and(|suffix| suffix.starts_with('/'))
}

pub fn is_remote_lookup_name(name: &str) -> bool {
    let trimmed = name.trim();
    if trimmed.is_empty() {
        return false;
    }
    if trimmed.starts_with('/') {
        return true;
    }
    let lower = trimmed.to_ascii_lowercase();
    lower.starts_with('z') || lower.starts_with('y')
}

pub fn is_remote_lookup_candidate(name: &str, kind: &str) -> bool {
    let trimmed = name.trim();
    if trimmed.is_empty() {
        return false;
    }
    if is_remote_lookup_name(trimmed) {
        return true;
    }

    match kind.trim().to_ascii_lowercase().as_str() {
        "type" | "static" | "function" | "report" => is_standard_remote_type_like_name(trimmed),
        "message-class" => is_standard_message_class_name(trimmed),
        _ => false,
    }
}

pub fn is_remote_lookup_candidate_after_local_resolution(name: &str, kind: &str) -> bool {
    let trimmed = name.trim();
    if trimmed.is_empty() {
        return false;
    }
    if is_remote_lookup_name(trimmed) {
        return true;
    }

    match kind.trim().to_ascii_lowercase().as_str() {
        "type" | "static" => is_standard_remote_type_like_name_after_local_resolution(trimmed),
        "function" | "report" => is_standard_remote_type_like_name_after_local_resolution(trimmed),
        "message-class" => is_standard_message_class_name(trimmed),
        _ => false,
    }
}

fn is_standard_remote_type_like_name(name: &str) -> bool {
    if name.starts_with('/') {
        return true;
    }
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !first.is_ascii_alphabetic() {
        return false;
    }

    let lower = name.to_ascii_lowercase();
    if is_likely_local_identifier_style(&lower) {
        return false;
    }
    if is_likely_builtin_type_name(&lower) {
        return false;
    }

    chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_' || ch == '/')
}

fn is_standard_remote_type_like_name_after_local_resolution(name: &str) -> bool {
    if name.starts_with('/') {
        return true;
    }
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !first.is_ascii_alphabetic() {
        return false;
    }

    let lower = name.to_ascii_lowercase();
    if is_likely_builtin_type_name(&lower) {
        return false;
    }

    chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_' || ch == '/')
}

fn is_likely_builtin_type_name(lower: &str) -> bool {
    matches!(
        lower,
        "i" | "int1"
            | "int2"
            | "int4"
            | "int8"
            | "f"
            | "p"
            | "decfloat"
            | "decfloat16"
            | "decfloat34"
            | "string"
            | "c"
            | "n"
            | "d"
            | "t"
            | "x"
            | "xstring"
            | "data"
            | "any"
            | "abap_bool"
            | "flag"
            | "xfeld"
            | "syst"
            | "guid"
            | "symsgv"
            | "sydatum"
            | "timestamp"
            | "cursor"
            | "tabname"
            | "cdobjectcl"
            | "rs38l_fnam"
            | "memoryid"
            | "time"
            | "timestmp"
            | "object"
            | "standard"
            | "table"
            | "simple"
            | "numeric"
            | "csequence"
            | "clike"
            | "xsequence"
            | "previous"
            | "to"
    ) || (lower.starts_with("char") && lower[4..].chars().all(|ch| ch.is_ascii_digit()))
}

fn is_likely_local_identifier_style(lower: &str) -> bool {
    const LOCAL_PREFIXES: &[&str] = &[
        "lv_", "ls_", "lt_", "lr_", "lo_", "li_", "lm_", "lx_", "lc_", "ld_", "gv_", "gs_", "gt_",
        "gr_", "go_", "gi_", "gm_", "gx_", "gc_", "gd_", "mv_", "ms_", "mt_", "mr_", "mo_", "mi_",
        "mm_", "mx_", "mc_", "md_", "iv_", "is_", "it_", "ir_", "io_", "ii_", "im_", "ix_", "ic_",
        "id_", "ev_", "es_", "et_", "er_", "eo_", "ei_", "em_", "ex_", "ec_", "ed_", "rv_", "rs_",
        "rt_", "rr_", "ro_", "ri_", "rm_", "rx_", "rc_", "rd_", "cv_", "cs_", "ct_", "cr_", "co_",
        "ci_", "cm_", "cc_", "cd_", "sv_", "ss_", "st_", "sr_", "so_", "si_", "sm_", "sx_", "sc_",
        "sd_", "tv_", "ts_", "tt_", "tr_", "to_", "ti_", "tm_", "tx_", "tc_", "td_", "uv_", "us_",
        "ut_", "ur_", "uo_", "ui_", "um_", "ux_", "uc_", "ud_", "wv_", "ws_", "wt_", "wr_", "wo_",
        "wi_", "wm_", "wx_", "wc_", "wd_", "xv_", "xs_", "xt_", "xr_", "xo_", "xi_", "xm_", "xx_",
        "xc_", "xd_", "yv_", "ys_", "yt_", "yr_", "yo_", "yi_", "ym_", "yx_", "yc_", "yd_", "zv_",
        "zs_", "zt_", "zr_", "zo_", "zi_", "zm_", "zx_", "zc_", "zd_",
    ];

    LOCAL_PREFIXES
        .iter()
        .any(|prefix| lower.starts_with(prefix))
}

fn is_standard_message_class_name(name: &str) -> bool {
    if name.starts_with('/') {
        return true;
    }
    if name.chars().all(|ch| ch.is_ascii_digit()) {
        return true;
    }
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !first.is_ascii_alphabetic() {
        return false;
    }
    !is_likely_local_identifier_style(&name.to_ascii_lowercase())
        && chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_' || ch == '/')
}

fn collect_abap_sources(
    root_path: &Path,
    root_uri: &str,
    overlays: &HashMap<String, OpenDocumentOverlay>,
    seen: &mut HashSet<String>,
    documents: &mut Vec<WorkspaceDocument>,
    is_dependency: bool,
    progress: &mut WorkspaceLoadProgress<'_>,
) {
    if !root_path.exists() {
        return;
    }
    let mut stack = vec![root_path.to_path_buf()];
    while let Some(dir) = stack.pop() {
        let Ok(entries) = fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            let Ok(file_type) = entry.file_type() else {
                continue;
            };
            if file_type.is_dir() {
                let name = entry.file_name();
                let name = name.to_string_lossy();
                if name == ".git" || name == "target" {
                    continue;
                }
                if !is_dependency && name == ".abapls" {
                    continue;
                }
                stack.push(path);
                continue;
            }
            if path.extension().and_then(|ext| ext.to_str()) != Some("abap") {
                continue;
            }
            let uri = path_to_file_uri(&path);
            if !uri_starts_with_workspace(&uri, root_uri) || !seen.insert(uri.clone()) {
                continue;
            }
            let (version, text) = if let Some(overlay) = overlays.get(&uri) {
                (overlay.version, overlay.text.to_string())
            } else {
                match fs::read_to_string(&path) {
                    Ok(text) => (0, text),
                    Err(_) => continue,
                }
            };
            documents.push(WorkspaceDocument {
                uri: Arc::from(uri),
                version,
                text,
                is_dependency,
                object_name: None,
            });
            progress.loaded_document();
        }
    }
}

fn discover_conventional_src_units(root_path: &Path) -> Vec<ManifestUnit> {
    let src_path = root_path.join("src");
    if !src_path.is_dir() {
        return Vec::new();
    }

    let mut units = Vec::new();
    units.extend(discover_single_file_units(
        root_path,
        "src/classes",
        "global-class",
    ));
    units.extend(discover_single_file_units(
        root_path,
        "src/interfaces",
        "global-interface",
    ));
    units.extend(discover_single_file_units(
        root_path,
        "src/includes",
        "include",
    ));
    units.extend(discover_folder_units(root_path, "src/reports", "report"));
    units.extend(discover_folder_units(
        root_path,
        "src/function-groups",
        "function-group",
    ));
    units.sort_by(|left, right| left.root_file.cmp(&right.root_file));
    units
}

fn discover_single_file_units(
    root_path: &Path,
    relative_dir: &str,
    kind: &str,
) -> Vec<ManifestUnit> {
    let dir = root_path.join(relative_dir);
    let Ok(entries) = fs::read_dir(&dir) else {
        return Vec::new();
    };
    entries
        .flatten()
        .filter_map(|entry| {
            let path = entry.path();
            if !entry.file_type().ok()?.is_file() {
                return None;
            }
            if path.extension().and_then(|ext| ext.to_str()) != Some("abap") {
                return None;
            }
            Some(single_file_unit(root_path, &path, kind))
        })
        .collect()
}

fn discover_folder_units(root_path: &Path, relative_dir: &str, kind: &str) -> Vec<ManifestUnit> {
    let dir = root_path.join(relative_dir);
    let Ok(entries) = fs::read_dir(&dir) else {
        return Vec::new();
    };
    let mut units = Vec::new();
    for entry in entries.flatten() {
        let path = entry.path();
        let Ok(file_type) = entry.file_type() else {
            continue;
        };
        if file_type.is_file()
            && kind == "report"
            && path.extension().and_then(|ext| ext.to_str()) == Some("abap")
        {
            units.push(single_file_unit(root_path, &path, kind));
            continue;
        }
        if !file_type.is_dir() {
            continue;
        }
        if let Some(unit) = folder_unit(root_path, &path, kind) {
            units.push(unit);
        }
    }
    units
}

fn single_file_unit(root_path: &Path, path: &Path, kind: &str) -> ManifestUnit {
    let relative = workspace_relative_path(root_path, path);
    let name = infer_object_name_from_manifest_path(&relative).unwrap_or_default();
    let mut unit = ManifestUnit {
        name,
        kind: kind.to_string(),
        package_name: String::new(),
        root_file: relative,
        dependency_of: Vec::new(),
        members: Vec::new(),
    };
    apply_single_file_unit_sidecar(root_path, path, &mut unit);
    unit
}

fn folder_unit(root_path: &Path, dir_path: &Path, kind: &str) -> Option<ManifestUnit> {
    let encoded_dir_name = dir_path.file_name()?.to_str()?;
    let root_file_path = dir_path.join(format!("{encoded_dir_name}.abap"));
    if !root_file_path.is_file() {
        return None;
    }

    let root_file = workspace_relative_path(root_path, &root_file_path);
    let mut member_paths = Vec::new();
    collect_abap_file_paths(dir_path, &mut member_paths);
    member_paths.sort();
    let members = member_paths
        .into_iter()
        .filter(|path| path != &root_file_path)
        .map(|path| ManifestUnitMember {
            role: String::new(),
            file: workspace_relative_path(root_path, &path),
            object_name: String::new(),
        })
        .collect::<Vec<_>>();
    let mut unit = ManifestUnit {
        name: percent_decode(encoded_dir_name),
        kind: kind.to_string(),
        package_name: String::new(),
        root_file,
        dependency_of: Vec::new(),
        members,
    };
    apply_unit_sidecar_manifest(root_path, dir_path, &mut unit);
    Some(unit)
}

fn collect_abap_file_paths(dir_path: &Path, output: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir_path) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        let Ok(file_type) = entry.file_type() else {
            continue;
        };
        if file_type.is_dir() {
            collect_abap_file_paths(&path, output);
            continue;
        }
        if path.extension().and_then(|ext| ext.to_str()) == Some("abap") {
            output.push(path);
        }
    }
}

fn apply_unit_sidecar_manifest(root_path: &Path, dir_path: &Path, unit: &mut ManifestUnit) {
    let Some(sidecar) = load_folder_unit_sidecar_manifest(dir_path) else {
        return;
    };
    apply_sidecar_manifest_entries(root_path, dir_path, unit, sidecar);
}

fn apply_single_file_unit_sidecar(root_path: &Path, file_path: &Path, unit: &mut ManifestUnit) {
    let Some(sidecar) = load_single_file_unit_sidecar_manifest(file_path) else {
        return;
    };
    let Some(base_dir) = file_path.parent() else {
        return;
    };
    apply_sidecar_manifest_entries(root_path, base_dir, unit, sidecar);
}

fn apply_sidecar_manifest_entries(
    root_path: &Path,
    base_dir: &Path,
    unit: &mut ManifestUnit,
    sidecar: UnitSidecarManifest,
) {
    for member_path in sidecar.members {
        append_sidecar_member(root_path, base_dir, unit, String::new(), member_path);
    }
    for (object_name, relative_path) in sidecar.includes {
        append_sidecar_member(root_path, base_dir, unit, object_name, relative_path);
    }
    unit.members.sort_by(|left, right| {
        normalize_manifest_path(&left.file).cmp(&normalize_manifest_path(&right.file))
    });
}

fn append_sidecar_member(
    root_path: &Path,
    base_dir: &Path,
    unit: &mut ManifestUnit,
    object_name: String,
    relative_path: String,
) {
    let mapped_path = if Path::new(&relative_path).is_absolute() {
        PathBuf::from(&relative_path)
    } else {
        base_dir.join(&relative_path)
    };
    if !mapped_path.starts_with(root_path) {
        return;
    }
    let mapped_file = workspace_relative_path(root_path, &mapped_path);
    if normalize_manifest_path(&mapped_file) == normalize_manifest_path(&unit.root_file) {
        return;
    }
    if let Some(existing) = unit.members.iter_mut().find(|member| {
        normalize_manifest_path(&member.file) == normalize_manifest_path(&mapped_file)
    }) {
        if !object_name.trim().is_empty() {
            existing.object_name = object_name.trim().to_string();
        }
        return;
    }
    unit.members.push(ManifestUnitMember {
        role: String::new(),
        file: mapped_file,
        object_name: object_name.trim().to_string(),
    });
}

fn load_folder_unit_sidecar_manifest(dir_path: &Path) -> Option<UnitSidecarManifest> {
    let text = fs::read_to_string(dir_path.join("abapls-unit.toml")).ok()?;
    toml::from_str(&text).ok()
}

fn load_single_file_unit_sidecar_manifest(file_path: &Path) -> Option<UnitSidecarManifest> {
    let file_name = file_path.file_name()?.to_str()?;
    let sidecar_name = format!("{file_name}.abapls-unit.toml");
    let text = fs::read_to_string(file_path.with_file_name(sidecar_name)).ok()?;
    toml::from_str(&text).ok()
}

pub fn local_export_config_for_source(
    workspace_root: &Path,
    source_uri: &str,
) -> LocalExportConfig {
    let sidecar_paths = source_unit_sidecar_paths(workspace_root, source_uri);
    if sidecar_paths.is_empty() {
        return LocalExportConfig::default();
    }

    let mut roots = Vec::new();
    let mut seen_roots = HashSet::new();
    let mut saw_local_first = false;
    let mut saw_adt_first = false;

    for sidecar_path in sidecar_paths {
        let Some(sidecar) = load_unit_sidecar_manifest(&sidecar_path) else {
            continue;
        };
        for root in resolve_unit_sidecar_local_roots(&sidecar_path, &sidecar) {
            let key = normalized_local_export_path_key(&root);
            if seen_roots.insert(key) {
                roots.push(root);
            }
        }

        match normalize_local_dependency_source_mode(&sidecar.dependencies.source) {
            LocalDependencySourceMode::LocalOnly => {
                return LocalExportConfig {
                    mode: LocalDependencySourceMode::LocalOnly,
                    roots,
                };
            }
            LocalDependencySourceMode::LocalFirst => saw_local_first = true,
            LocalDependencySourceMode::AdtFirst => saw_adt_first = true,
        }
    }

    let mode = if saw_local_first {
        LocalDependencySourceMode::LocalFirst
    } else if saw_adt_first {
        LocalDependencySourceMode::AdtFirst
    } else {
        LocalDependencySourceMode::LocalFirst
    };

    LocalExportConfig { mode, roots }
}

pub fn resolve_local_export_dependency_document(
    roots: &[PathBuf],
    resolver: &mut LocalExportResolver,
    candidate_name: &str,
    candidate_kind: &str,
) -> Option<WorkspaceDocument> {
    let file_names = local_export_candidate_file_names(candidate_name, candidate_kind);
    if file_names.is_empty() {
        return None;
    }

    for root in roots {
        let key = normalized_local_export_path_key(root);
        let index = resolver
            .indices
            .entry(key)
            .or_insert_with(|| build_local_export_index(root));
        for file_name in &file_names {
            let Some(artifacts) = index.artifacts_by_file_name.get(file_name) else {
                continue;
            };
            for artifact in artifacts {
                let mut source_text = None;
                if !local_export_artifact_matches_candidate(
                    artifact,
                    candidate_name,
                    candidate_kind,
                ) {
                    source_text = local_export_fallback_source_if_matches(
                        artifact,
                        candidate_name,
                        candidate_kind,
                    );
                    if source_text.is_none() {
                        continue;
                    }
                }
                let source_text = match source_text {
                    Some(source_text) => source_text,
                    None => fs::read_to_string(&artifact.path).ok()?,
                };
                let text = if artifact
                    .path
                    .extension()
                    .and_then(|ext| ext.to_str())
                    .is_some_and(|ext| ext.eq_ignore_ascii_case("xml"))
                {
                    ddic_xml_to_abap_source(
                        artifact.object_name.as_str(),
                        artifact.kind_hint.as_str(),
                        source_text.as_str(),
                    )
                    .unwrap_or(source_text)
                } else {
                    source_text
                };
                return Some(WorkspaceDocument {
                    uri: Arc::from(path_to_file_uri(&artifact.path)),
                    version: 0,
                    text,
                    is_dependency: true,
                    object_name: Some(Arc::from(artifact.object_name.to_ascii_lowercase())),
                });
            }
        }
    }

    None
}

pub fn resolve_local_export_function_module_documents_by_prefix(
    roots: &[PathBuf],
    resolver: &mut LocalExportResolver,
    prefix: &str,
    limit: usize,
) -> Vec<WorkspaceDocument> {
    let prefix = prefix.trim();
    if prefix.is_empty() || limit == 0 {
        return Vec::new();
    }

    let prefix_lower = prefix.to_ascii_lowercase();
    let encoded_prefix = encode_local_export_component(prefix).to_ascii_lowercase();
    if encoded_prefix.is_empty() {
        return Vec::new();
    }

    let mut out = Vec::new();
    let mut seen = HashSet::<String>::new();

    for root in roots {
        let key = normalized_local_export_path_key(root);
        let index = resolver
            .indices
            .entry(key)
            .or_insert_with(|| build_local_export_index(root));
        let mut file_names: Vec<_> = index
            .artifacts_by_file_name
            .keys()
            .filter(|file_name| {
                file_name.starts_with(&encoded_prefix) && file_name.ends_with(".abap")
            })
            .cloned()
            .collect();
        file_names.sort();

        for file_name in file_names {
            let Some(artifacts) = index.artifacts_by_file_name.get(&file_name) else {
                continue;
            };
            for artifact in artifacts {
                let Ok(source_text) = fs::read_to_string(&artifact.path) else {
                    continue;
                };
                let function_name = if artifact.kind_hint.eq_ignore_ascii_case("function-module")
                    && artifact
                        .object_name
                        .to_ascii_lowercase()
                        .starts_with(&prefix_lower)
                {
                    artifact.object_name.clone()
                } else {
                    let Some(function_name) = first_abap_function_module_name(&source_text) else {
                        continue;
                    };
                    if !function_name
                        .to_ascii_lowercase()
                        .starts_with(&prefix_lower)
                    {
                        continue;
                    }
                    function_name.to_string()
                };
                let dedupe_key = function_name.to_ascii_lowercase();
                if !seen.insert(dedupe_key.clone()) {
                    continue;
                }
                out.push(WorkspaceDocument {
                    uri: Arc::from(path_to_file_uri(&artifact.path)),
                    version: 0,
                    text: source_text,
                    is_dependency: true,
                    object_name: Some(Arc::from(dedupe_key)),
                });
                if out.len() >= limit {
                    return out;
                }
            }
        }
    }

    out
}

fn local_export_fallback_source_if_matches(
    artifact: &LocalExportArtifact,
    candidate_name: &str,
    candidate_kind: &str,
) -> Option<String> {
    match candidate_kind.trim().to_ascii_lowercase().as_str() {
        "function" => local_export_flat_function_module_source_if_matches(artifact, candidate_name),
        _ => None,
    }
}

fn local_export_flat_function_module_source_if_matches(
    artifact: &LocalExportArtifact,
    candidate_name: &str,
) -> Option<String> {
    if !artifact
        .path
        .extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| ext.eq_ignore_ascii_case("abap"))
    {
        return None;
    }

    let source_text = fs::read_to_string(&artifact.path).ok()?;
    let function_name = first_abap_function_module_name(source_text.as_str())?;
    function_name
        .eq_ignore_ascii_case(candidate_name.trim())
        .then_some(source_text)
}

fn first_abap_function_module_name(text: &str) -> Option<&str> {
    for line in text.lines() {
        let trimmed = line.trim_start();
        if trimmed.is_empty() || trimmed.starts_with('*') || trimmed.starts_with('"') {
            continue;
        }
        let rest = abap_keyword_rest(trimmed, "function")?;
        return rest
            .trim_start()
            .split_whitespace()
            .next()
            .map(|name| name.trim_end_matches('.'));
    }
    None
}

fn abap_keyword_rest<'a>(line: &'a str, keyword: &str) -> Option<&'a str> {
    if line.len() < keyword.len() {
        return None;
    }
    let (head, tail) = line.split_at(keyword.len());
    if !head.eq_ignore_ascii_case(keyword) {
        return None;
    }
    if tail.is_empty() {
        return Some(tail);
    }
    tail.chars()
        .next()
        .is_some_and(char::is_whitespace)
        .then_some(tail)
}

fn source_unit_sidecar_paths(workspace_root: &Path, source_uri: &str) -> Vec<PathBuf> {
    let Some(source_path) = file_uri_to_path(source_uri) else {
        return Vec::new();
    };
    if !source_path.starts_with(workspace_root) {
        return Vec::new();
    }

    let mut sidecar_paths = Vec::new();
    let mut seen = HashSet::new();

    if let Some(file_name) = source_path.file_name().and_then(|value| value.to_str()) {
        push_sidecar_path_if_exists(
            &mut sidecar_paths,
            &mut seen,
            source_path.with_file_name(format!("{file_name}.abapls-unit.toml")),
        );
    }

    let mut current_dir = source_path.parent();
    while let Some(dir) = current_dir {
        if !dir.starts_with(workspace_root) {
            break;
        }
        push_sidecar_path_if_exists(&mut sidecar_paths, &mut seen, dir.join("abapls-unit.toml"));
        if dir == workspace_root {
            break;
        }
        current_dir = dir.parent();
    }

    sidecar_paths
}

fn push_sidecar_path_if_exists(
    sidecar_paths: &mut Vec<PathBuf>,
    seen: &mut HashSet<String>,
    path: PathBuf,
) {
    if !path.is_file() {
        return;
    }
    let key = normalized_local_export_path_key(&path);
    if seen.insert(key) {
        sidecar_paths.push(path);
    }
}

fn load_unit_sidecar_manifest(sidecar_path: &Path) -> Option<UnitSidecarManifest> {
    let text = fs::read_to_string(sidecar_path).ok()?;
    toml::from_str(&text).ok()
}

fn resolve_unit_sidecar_local_roots(
    sidecar_path: &Path,
    sidecar: &UnitSidecarManifest,
) -> Vec<PathBuf> {
    let base_dir = sidecar_path.parent().unwrap_or_else(|| Path::new("."));
    let mut roots = Vec::new();
    let mut seen = HashSet::new();

    for root in &sidecar.local_export.roots {
        let root = root.trim();
        if root.is_empty() {
            continue;
        }
        let path = if Path::new(root).is_absolute() {
            PathBuf::from(root)
        } else {
            base_dir.join(root)
        };
        let normalized = match path.canonicalize() {
            Ok(path) => normalize_windows_path(path),
            Err(_) => normalize_windows_path(path),
        };
        let key = normalized_local_export_path_key(&normalized);
        if seen.insert(key) {
            roots.push(normalized);
        }
    }

    roots
}

fn normalize_local_dependency_source_mode(value: &str) -> LocalDependencySourceMode {
    match value.trim().to_ascii_lowercase().as_str() {
        "local-only" => LocalDependencySourceMode::LocalOnly,
        "adt-first" => LocalDependencySourceMode::AdtFirst,
        _ => LocalDependencySourceMode::LocalFirst,
    }
}

fn build_local_export_index(root: &Path) -> LocalExportIndex {
    let mut index = LocalExportIndex::default();
    if !root.is_dir() {
        return index;
    }

    let mut stack = vec![root.to_path_buf()];
    while let Some(current) = stack.pop() {
        let mut entries: Vec<_> = match fs::read_dir(&current) {
            Ok(entries) => entries.flatten().collect(),
            Err(_) => continue,
        };
        entries.sort_by_key(|entry| entry.path());

        for entry in entries {
            let path = entry.path();
            let Ok(file_type) = entry.file_type() else {
                continue;
            };
            if file_type.is_dir() {
                stack.push(path);
                continue;
            }
            if !file_type.is_file() {
                continue;
            }
            let Some(file_name) = path.file_name().and_then(|value| value.to_str()) else {
                continue;
            };
            let artifact = LocalExportArtifact {
                kind_hint: infer_local_export_kind_hint(&path),
                object_name: infer_object_name_from_manifest_path(file_name)
                    .unwrap_or_else(|| percent_decode(file_name)),
                path: path.clone(),
            };
            index
                .artifacts_by_file_name
                .entry(file_name.to_ascii_lowercase())
                .or_default()
                .push(artifact);
        }
    }

    index
}

fn infer_local_export_kind_hint(path: &Path) -> String {
    path.parent()
        .and_then(|parent| parent.file_name())
        .and_then(|name| name.to_str())
        .map(|name| name.trim().to_string())
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| "dependency".to_string())
}

fn local_export_artifact_matches_candidate(
    artifact: &LocalExportArtifact,
    candidate_name: &str,
    candidate_kind: &str,
) -> bool {
    let kind = artifact.kind_hint.trim().to_ascii_lowercase();
    match candidate_kind.trim().to_ascii_lowercase().as_str() {
        "include" => kind == "include",
        "function" => kind == "function-module",
        "report" => matches!(kind.as_str(), "program" | "report"),
        "message-class" => kind == "message-class",
        "symbol" | "static" | "type" => {
            local_export_artifact_matches_type_like_candidate(kind.as_str(), candidate_name)
        }
        _ => false,
    }
}

fn local_export_artifact_matches_type_like_candidate(
    kind_hint: &str,
    candidate_name: &str,
) -> bool {
    if is_class_like_remote_type_name(candidate_name) {
        return matches!(
            kind_hint,
            "class" | "global-class" | "interface" | "global-interface"
        );
    }

    matches!(
        kind_hint,
        "ddic-data-element"
            | "ddic-domain"
            | "ddic-lock-object"
            | "ddic-search-help"
            | "ddic-structure"
            | "ddic-table"
            | "ddic-table-type"
            | "ddic-view"
            | "type-group"
            | "type-pool"
    )
}

fn is_class_like_remote_type_name(name: &str) -> bool {
    let lower = name.trim().to_ascii_lowercase();
    let tail = lower.rsplit('/').next().unwrap_or(lower.as_str());
    tail.starts_with("cl_")
        || tail.starts_with("cx_")
        || tail.starts_with("if_")
        || tail.starts_with("ycl_")
        || tail.starts_with("ycx_")
        || tail.starts_with("yif_")
        || tail.starts_with("zcl_")
        || tail.starts_with("zcx_")
        || tail.starts_with("zif_")
}

fn local_export_candidate_file_names(candidate_name: &str, candidate_kind: &str) -> Vec<String> {
    let encoded_name = encode_local_export_component(candidate_name.trim());
    if encoded_name.is_empty() {
        return Vec::new();
    }
    let extensions: &[&str] = match candidate_kind.trim().to_ascii_lowercase().as_str() {
        "include" | "function" | "static" | "report" => &["abap"],
        "message-class" => &["xml"],
        "symbol" | "type" => &["xml", "abap"],
        _ => &[],
    };

    extensions
        .iter()
        .map(|extension| format!("{encoded_name}.{extension}").to_ascii_lowercase())
        .collect()
}

fn encode_local_export_component(value: &str) -> String {
    let normalized = value.trim().to_ascii_uppercase();
    let mut out = String::with_capacity(normalized.len());
    for byte in normalized.bytes() {
        if byte.is_ascii_alphanumeric()
            || matches!(
                byte,
                b'-' | b'_' | b'.' | b'!' | b'~' | b'*' | b'\'' | b'(' | b')'
            )
        {
            out.push(byte as char);
            continue;
        }
        out.push('%');
        out.push(hex_digit(byte >> 4));
        out.push(hex_digit(byte & 0x0f));
    }
    out
}

fn normalized_local_export_path_key(path: &Path) -> String {
    normalize_windows_path(path.to_path_buf())
        .to_string_lossy()
        .replace('\\', "/")
        .to_ascii_lowercase()
}

fn collect_manifest_documents(
    manifest: &WorkspaceManifest,
    root_path: &Path,
    root_uri: &str,
    cache_dir: &str,
    overlays: &HashMap<String, OpenDocumentOverlay>,
    seen: &mut HashSet<String>,
    documents: &mut Vec<WorkspaceDocument>,
    progress: &mut WorkspaceLoadProgress<'_>,
    loaded_units: &mut Vec<ManifestUnit>,
) {
    let selected_dependency_sources =
        collect_selected_dependency_sources(manifest, root_path, root_uri, cache_dir, overlays);
    for unit in &manifest.units {
        if !should_load_manifest_unit(unit, cache_dir, &selected_dependency_sources) {
            continue;
        }
        let mut unit_files = HashSet::new();

        for member in &unit.members {
            let relative = normalize_manifest_path(&member.file);
            if relative.is_empty() || !unit_files.insert(relative.clone()) {
                continue;
            }
            collect_manifest_document(
                unit,
                manifest_member_object_name(unit, Some(member)),
                manifest_member_role(member, cache_dir) == "dependency",
                &relative,
                root_path,
                root_uri,
                overlays,
                seen,
                documents,
                progress,
            );
        }

        let relative = normalize_manifest_path(&unit.root_file);
        if relative.is_empty() || !unit_files.insert(relative.clone()) {
            continue;
        }
        collect_manifest_document(
            unit,
            manifest_member_object_name(unit, None),
            manifest_unit_root_is_dependency(unit, cache_dir),
            &relative,
            root_path,
            root_uri,
            overlays,
            seen,
            documents,
            progress,
        );
    }
    collect_dependency_cache_documents(
        manifest,
        root_path,
        root_uri,
        cache_dir,
        overlays,
        seen,
        documents,
        progress,
        loaded_units,
    );
}

fn collect_manifest_document(
    unit: &ManifestUnit,
    object_name: Option<Arc<str>>,
    is_dependency: bool,
    relative: &str,
    root_path: &Path,
    root_uri: &str,
    overlays: &HashMap<String, OpenDocumentOverlay>,
    seen: &mut HashSet<String>,
    documents: &mut Vec<WorkspaceDocument>,
    progress: &mut WorkspaceLoadProgress<'_>,
) {
    let path = root_path.join(relative);
    let uri = path_to_file_uri(&path);
    if !uri_starts_with_workspace(&uri, root_uri) || !seen.insert(uri.clone()) {
        return;
    }

    let (version, source_text) = if let Some(overlay) = overlays.get(&uri) {
        (overlay.version, overlay.text.to_string())
    } else {
        match fs::read_to_string(&path) {
            Ok(text) => (0, text),
            Err(_) => return,
        }
    };

    let text = if path.extension().and_then(|ext| ext.to_str()) == Some("xml") {
        ddic_xml_to_abap_source(unit.name.as_str(), unit.kind.as_str(), source_text.as_str())
            .unwrap_or(source_text)
    } else {
        source_text
    };
    let is_dependency = is_dependency && !overlays.contains_key(&uri);
    documents.push(WorkspaceDocument {
        uri: Arc::from(uri),
        version,
        text,
        is_dependency,
        object_name,
    });
    progress.loaded_document();
}

fn manifest_unit_root_is_dependency(unit: &ManifestUnit, cache_dir: &str) -> bool {
    let root_file = normalize_manifest_path(&unit.root_file);
    if let Some(member) = unit
        .members
        .iter()
        .find(|member| normalize_manifest_path(&member.file) == root_file)
    {
        return manifest_member_role(member, cache_dir) == "dependency";
    }
    if unit.members.is_empty() {
        return manifest_path_is_dependency_cache(&root_file, cache_dir);
    }
    unit.members
        .iter()
        .all(|member| manifest_member_role(member, cache_dir) == "dependency")
        || manifest_path_is_dependency_cache(&root_file, cache_dir)
}

fn manifest_member_object_name(
    unit: &ManifestUnit,
    member: Option<&ManifestUnitMember>,
) -> Option<Arc<str>> {
    if let Some(member) = member {
        if let Some(name) = manifest_member_explicit_object_name(member)
            .map(str::to_string)
            .or_else(|| infer_object_name_from_manifest_path(&member.file))
        {
            return Some(Arc::from(name.to_ascii_lowercase()));
        }
    }
    let explicit = (!unit.name.trim().is_empty())
        .then(|| unit.name.trim().to_string())
        .or_else(|| infer_object_name_from_manifest_path(&unit.root_file))?;
    Some(Arc::from(explicit.to_ascii_lowercase()))
}

fn manifest_unit_is_dependency(unit: &ManifestUnit, cache_dir: &str) -> bool {
    manifest_unit_root_is_dependency(unit, cache_dir)
        || unit.members.iter().any(|member| {
            manifest_member_role(member, cache_dir) == "dependency"
                || manifest_path_is_dependency_cache(&member.file, cache_dir)
        })
}

fn manifest_member_role<'a>(member: &'a ManifestUnitMember, cache_dir: &'a str) -> &'a str {
    let role = member.role.trim();
    if !role.is_empty() {
        return role;
    }
    if manifest_path_is_dependency_cache(&member.file, cache_dir) {
        "dependency"
    } else {
        "root"
    }
}

fn manifest_member_explicit_object_name<'a>(member: &'a ManifestUnitMember) -> Option<&'a str> {
    let name = member.object_name.trim();
    (!name.is_empty()).then_some(name)
}

fn infer_object_name_from_manifest_path(file: &str) -> Option<String> {
    let normalized = normalize_manifest_path(file);
    let base_name = Path::new(&normalized).file_stem()?.to_str()?.trim();
    if base_name.is_empty() {
        return None;
    }
    Some(percent_decode(base_name).trim().to_string())
}

fn manifest_unit_files(unit: &ManifestUnit) -> Vec<String> {
    let mut files = Vec::new();
    let mut seen = HashSet::new();
    for member in &unit.members {
        let file = normalize_manifest_path(&member.file);
        if !file.is_empty() && seen.insert(file.clone()) {
            files.push(file);
        }
    }
    let root_file = normalize_manifest_path(&unit.root_file);
    if !root_file.is_empty() && seen.insert(root_file.clone()) {
        files.push(root_file);
    }
    files
}

fn dependency_cache_root_prefix(cache_dir: &str) -> String {
    let cache_dir = normalize_manifest_path(cache_dir);
    if cache_dir.is_empty() {
        "dependencies/".to_string()
    } else {
        format!("{cache_dir}/dependencies/")
    }
}

fn dependency_cache_manifest_root_prefix(cache_dir: &str) -> String {
    let cache_dir = normalize_manifest_path(cache_dir);
    if cache_dir.is_empty() {
        "dependency-manifests/".to_string()
    } else {
        format!("{cache_dir}/dependency-manifests/")
    }
}

fn package_cache_root_prefix(cache_dir: &str) -> String {
    let cache_dir = normalize_manifest_path(cache_dir);
    if cache_dir.is_empty() {
        "packages/".to_string()
    } else {
        format!("{cache_dir}/packages/")
    }
}

fn manifest_path_is_dependency_cache(file: &str, cache_dir: &str) -> bool {
    let normalized = normalize_manifest_path(file);
    normalized.starts_with(&dependency_cache_root_prefix(cache_dir))
        || normalized.starts_with(&package_cache_root_prefix(cache_dir))
}

fn is_dependency_cache_uri(root_path: &Path, root_uri: &str, cache_dir: &str, uri: &str) -> bool {
    if !uri_starts_with_workspace(uri, root_uri) {
        return false;
    }
    let Some(path) = file_uri_to_path(uri) else {
        return false;
    };
    let relative = workspace_relative_path(root_path, &path);
    manifest_path_is_dependency_cache(&relative, cache_dir)
}

fn collect_dependency_cache_documents(
    manifest: &WorkspaceManifest,
    root_path: &Path,
    root_uri: &str,
    cache_dir: &str,
    overlays: &HashMap<String, OpenDocumentOverlay>,
    seen: &mut HashSet<String>,
    documents: &mut Vec<WorkspaceDocument>,
    progress: &mut WorkspaceLoadProgress<'_>,
    loaded_units: &mut Vec<ManifestUnit>,
) {
    let mut pending_sources: Vec<_> =
        initial_dependency_cache_sources(manifest, root_path, root_uri, cache_dir, overlays)
            .into_iter()
            .collect();
    let mut visited_sources = HashSet::new();
    let mut loaded_unit_keys = loaded_units
        .iter()
        .map(manifest_unit_identity_key)
        .collect::<HashSet<_>>();

    while let Some(source_file) = pending_sources.pop() {
        let source_file = normalize_manifest_path(&source_file);
        if source_file.is_empty() || !visited_sources.insert(source_file.clone()) {
            continue;
        }
        let units = load_dependency_cache_manifest_units(root_path, cache_dir, &source_file);
        for unit in units {
            let unit_files = manifest_unit_files(&unit);
            let unit_key = manifest_unit_identity_key(&unit);
            if loaded_unit_keys.insert(unit_key) {
                loaded_units.push(unit.clone());
            }

            let mut seen_files = HashSet::new();
            for member in &unit.members {
                let relative = normalize_manifest_path(&member.file);
                if relative.is_empty() || !seen_files.insert(relative.clone()) {
                    continue;
                }
                collect_manifest_document(
                    &unit,
                    manifest_member_object_name(&unit, Some(member)),
                    true,
                    &relative,
                    root_path,
                    root_uri,
                    overlays,
                    seen,
                    documents,
                    progress,
                );
            }

            let relative = normalize_manifest_path(&unit.root_file);
            if !relative.is_empty() && seen_files.insert(relative.clone()) {
                collect_manifest_document(
                    &unit,
                    manifest_member_object_name(&unit, None),
                    true,
                    &relative,
                    root_path,
                    root_uri,
                    overlays,
                    seen,
                    documents,
                    progress,
                );
            }

            pending_sources.extend(unit_files);
        }
    }
}

fn initial_dependency_cache_sources(
    manifest: &WorkspaceManifest,
    root_path: &Path,
    root_uri: &str,
    cache_dir: &str,
    overlays: &HashMap<String, OpenDocumentOverlay>,
) -> HashSet<String> {
    let mut sources = HashSet::new();
    for unit in manifest
        .units
        .iter()
        .filter(|unit| !manifest_unit_is_dependency(unit, cache_dir))
    {
        sources.extend(manifest_unit_files(unit));
    }
    for uri in overlays.keys() {
        if is_dependency_cache_uri(root_path, root_uri, cache_dir, uri) {
            let Some(path) = file_uri_to_path(uri) else {
                continue;
            };
            sources.insert(workspace_relative_path(root_path, &path));
        }
    }
    sources
}

fn load_dependency_cache_manifest_units(
    root_path: &Path,
    cache_dir: &str,
    source_file: &str,
) -> Vec<ManifestUnit> {
    let manifest_path = dependency_cache_manifest_path(root_path, cache_dir, source_file);
    let text = match fs::read_to_string(&manifest_path) {
        Ok(text) => text,
        Err(_) => return Vec::new(),
    };
    let mut manifest: DependencyCacheManifest = match toml::from_str(&text) {
        Ok(manifest) => manifest,
        Err(_) => return Vec::new(),
    };
    normalize_manifest_units(&mut manifest.units);
    manifest.units
}

fn dependency_cache_manifest_path(root_path: &Path, cache_dir: &str, source_file: &str) -> PathBuf {
    root_path.join(dependency_cache_manifest_relative_path(
        cache_dir,
        source_file,
    ))
}

fn dependency_cache_manifest_relative_path(cache_dir: &str, source_file: &str) -> String {
    let prefix = dependency_cache_manifest_root_prefix(cache_dir);
    format!(
        "{prefix}{}.toml",
        encode_manifest_cache_file_component(source_file)
    )
}

fn encode_manifest_cache_file_component(value: &str) -> String {
    let mut out = String::new();
    for ch in normalize_manifest_path(value).chars() {
        append_manifest_cache_component_char(&mut out, ch);
    }
    out
}

fn append_manifest_cache_component_char(out: &mut String, ch: char) {
    if ch.is_ascii_alphanumeric()
        || matches!(ch, '-' | '_' | '.' | '!' | '~' | '*' | '\'' | '(' | ')')
    {
        out.push(ch);
        return;
    }
    let mut buf = [0; 4];
    for byte in ch.encode_utf8(&mut buf).as_bytes() {
        out.push('%');
        out.push(hex_digit(byte >> 4));
        out.push(hex_digit(byte & 0x0f));
    }
}

fn manifest_unit_identity_key(unit: &ManifestUnit) -> String {
    let root_file = normalize_manifest_path(&unit.root_file);
    if !root_file.is_empty() {
        return root_file;
    }
    let name = unit.name.trim();
    if !name.is_empty() {
        return name.to_ascii_lowercase();
    }
    format!("{:?}", unit.members)
}

fn collect_selected_dependency_sources(
    manifest: &WorkspaceManifest,
    root_path: &Path,
    root_uri: &str,
    cache_dir: &str,
    overlays: &HashMap<String, OpenDocumentOverlay>,
) -> HashSet<String> {
    let mut active = HashSet::new();
    for unit in manifest
        .units
        .iter()
        .filter(|unit| manifest_unit_is_dependency(unit, cache_dir))
    {
        let unit_files = manifest_unit_files(unit);
        let is_open = unit_files.iter().any(|file| {
            overlays.contains_key(&path_to_file_uri(&root_path.join(file)))
                && uri_starts_with_workspace(&path_to_file_uri(&root_path.join(file)), root_uri)
        });
        if is_open {
            active.extend(unit_files);
        }
    }

    loop {
        let mut changed = false;
        for unit in manifest
            .units
            .iter()
            .filter(|unit| manifest_unit_is_dependency(unit, cache_dir))
        {
            if !unit
                .dependency_of
                .iter()
                .map(|dependency| normalize_manifest_path(&dependency.file))
                .any(|file| active.contains(&file))
            {
                continue;
            }
            for file in manifest_unit_files(unit) {
                changed |= active.insert(file);
            }
        }
        if !changed {
            break;
        }
    }

    active
}

fn should_load_manifest_unit(
    unit: &ManifestUnit,
    cache_dir: &str,
    selected_dependency_sources: &HashSet<String>,
) -> bool {
    if !manifest_unit_is_dependency(unit, cache_dir) {
        return true;
    }
    if unit.dependency_of.is_empty() {
        return true;
    }
    unit.dependency_of
        .iter()
        .map(|dependency| normalize_manifest_path(&dependency.file))
        .any(|file| {
            !manifest_path_is_dependency_cache(&file, cache_dir)
                || selected_dependency_sources.contains(&file)
        })
}

pub fn manifest_declares_uri(
    root_path: &Path,
    root_uri: &str,
    manifest: &WorkspaceManifest,
    uri: &str,
) -> bool {
    if !uri_starts_with_workspace(uri, root_uri) {
        return false;
    }

    manifest.units.iter().any(|unit| {
        let root_file = normalize_manifest_path(&unit.root_file);
        (!root_file.is_empty() && path_to_file_uri(&root_path.join(&root_file)) == uri)
            || unit.members.iter().any(|member| {
                let member_file = normalize_manifest_path(&member.file);
                !member_file.is_empty() && path_to_file_uri(&root_path.join(member_file)) == uri
            })
    })
}

pub fn manifest_document_metadata(
    root_path: &Path,
    root_uri: &str,
    manifest: &WorkspaceManifest,
    uri: &str,
) -> Option<(bool, Option<Arc<str>>)> {
    if !uri_starts_with_workspace(uri, root_uri) {
        return None;
    }
    let cache_dir = manifest_cache_dir(Some(manifest));

    manifest.units.iter().find_map(|unit| {
        for member in &unit.members {
            let member_file = normalize_manifest_path(&member.file);
            if !member_file.is_empty() && path_to_file_uri(&root_path.join(&member_file)) == uri {
                return Some((
                    manifest_member_role(member, cache_dir) == "dependency",
                    manifest_member_object_name(unit, Some(member)),
                ));
            }
        }

        let root_file = normalize_manifest_path(&unit.root_file);
        if !root_file.is_empty() && path_to_file_uri(&root_path.join(root_file)) == uri {
            return Some((
                manifest_unit_root_is_dependency(unit, cache_dir),
                manifest_member_object_name(unit, None),
            ));
        }

        None
    })
}

fn planned_workspace_document_count(
    root_path: &Path,
    root_uri: &str,
    manifest: Option<&WorkspaceManifest>,
    cache_dir: &str,
    overlays: &HashMap<String, OpenDocumentOverlay>,
) -> usize {
    let mut seen = HashSet::new();
    if let Some(manifest) = manifest {
        collect_manifest_document_uris(
            manifest, root_path, root_uri, cache_dir, overlays, &mut seen,
        );
    } else {
        collect_abap_source_uris(root_path, root_uri, &mut seen, false);
    }
    for uri in overlays.keys() {
        if uri_starts_with_workspace(uri, root_uri) {
            seen.insert(uri.clone());
        }
    }
    seen.len()
}

fn collect_abap_source_uris(
    root_path: &Path,
    root_uri: &str,
    seen: &mut HashSet<String>,
    is_dependency: bool,
) {
    if !root_path.exists() {
        return;
    }
    let mut stack = vec![root_path.to_path_buf()];
    while let Some(dir) = stack.pop() {
        let Ok(entries) = fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            let Ok(file_type) = entry.file_type() else {
                continue;
            };
            if file_type.is_dir() {
                let name = entry.file_name();
                let name = name.to_string_lossy();
                if name == ".git" || name == "target" {
                    continue;
                }
                if !is_dependency && name == ".abapls" {
                    continue;
                }
                stack.push(path);
                continue;
            }
            if path.extension().and_then(|ext| ext.to_str()) != Some("abap") {
                continue;
            }
            let uri = path_to_file_uri(&path);
            if uri_starts_with_workspace(&uri, root_uri) {
                seen.insert(uri);
            }
        }
    }
}

fn collect_manifest_document_uris(
    manifest: &WorkspaceManifest,
    root_path: &Path,
    root_uri: &str,
    cache_dir: &str,
    overlays: &HashMap<String, OpenDocumentOverlay>,
    seen: &mut HashSet<String>,
) {
    let selected_dependency_sources =
        collect_selected_dependency_sources(manifest, root_path, root_uri, cache_dir, overlays);
    for unit in &manifest.units {
        if !should_load_manifest_unit(unit, cache_dir, &selected_dependency_sources) {
            continue;
        }
        let mut unit_files = HashSet::new();
        for member in &unit.members {
            let relative = normalize_manifest_path(&member.file);
            if relative.is_empty() || !unit_files.insert(relative.clone()) {
                continue;
            }
            let uri = path_to_file_uri(&root_path.join(&relative));
            if uri_starts_with_workspace(&uri, root_uri) {
                seen.insert(uri);
            }
        }
        let relative = normalize_manifest_path(&unit.root_file);
        if relative.is_empty() || !unit_files.insert(relative.clone()) {
            continue;
        }
        let uri = path_to_file_uri(&root_path.join(relative));
        if uri_starts_with_workspace(&uri, root_uri) {
            seen.insert(uri);
        }
    }
    collect_dependency_cache_document_uris(
        manifest, root_path, root_uri, cache_dir, overlays, seen,
    );
}

fn collect_dependency_cache_document_uris(
    manifest: &WorkspaceManifest,
    root_path: &Path,
    root_uri: &str,
    cache_dir: &str,
    overlays: &HashMap<String, OpenDocumentOverlay>,
    seen: &mut HashSet<String>,
) {
    let mut pending_sources: Vec<_> =
        initial_dependency_cache_sources(manifest, root_path, root_uri, cache_dir, overlays)
            .into_iter()
            .collect();
    let mut visited_sources = HashSet::new();
    while let Some(source_file) = pending_sources.pop() {
        let source_file = normalize_manifest_path(&source_file);
        if source_file.is_empty() || !visited_sources.insert(source_file.clone()) {
            continue;
        }
        for unit in load_dependency_cache_manifest_units(root_path, cache_dir, &source_file) {
            let unit_files = manifest_unit_files(&unit);
            let unit_open = unit_files
                .iter()
                .any(|file| overlays.contains_key(&path_to_file_uri(&root_path.join(file))));
            for file in &unit_files {
                let uri = path_to_file_uri(&root_path.join(file));
                if uri_starts_with_workspace(&uri, root_uri) {
                    seen.insert(uri);
                }
            }
            if unit_open {
                pending_sources.extend(unit_files);
            }
        }
    }
}

fn normalize_manifest_path(value: &str) -> String {
    value
        .trim()
        .replace('\\', "/")
        .trim_start_matches("./")
        .to_string()
}

fn normalize_manifest(manifest: &mut WorkspaceManifest) {
    manifest.resolution.dependency_mode =
        normalize_dependency_mode(&manifest.resolution.dependency_mode).to_string();
    manifest.resolution.unknown_symbol_mode =
        normalize_unknown_symbol_mode(&manifest.resolution.unknown_symbol_mode).to_string();
    manifest.performance.mode =
        normalize_workspace_performance_mode(&manifest.performance.mode).to_string();
    if manifest.resolution.cache_dir.trim().is_empty() {
        manifest.resolution.cache_dir = default_cache_dir();
    }
    manifest.resolution.remote_requests_per_second =
        manifest.resolution.remote_requests_per_second.max(1);
    manifest.resolution.legacy_remote_request_parallelism = manifest
        .resolution
        .legacy_remote_request_parallelism
        .map(|value| value.max(1));
    normalize_manifest_units(&mut manifest.units);
}

fn normalize_manifest_units(units: &mut [ManifestUnit]) {
    for unit in units {
        unit.kind = unit.kind.trim().to_ascii_lowercase();
        unit.package_name = unit.package_name.trim().to_string();
        unit.root_file = normalize_manifest_path(&unit.root_file);
        for dependency in &mut unit.dependency_of {
            dependency.file = normalize_manifest_path(&dependency.file);
        }
        for member in &mut unit.members {
            member.role = member.role.trim().to_ascii_lowercase();
            member.file = normalize_manifest_path(&member.file);
        }
    }
}

fn percent_decode(value: &str) -> String {
    let bytes = value.as_bytes();
    let mut out = String::with_capacity(value.len());
    let mut idx = 0;
    while idx < bytes.len() {
        if bytes[idx] == b'%'
            && idx + 2 < bytes.len()
            && let (Some(hi), Some(lo)) = (hex_value(bytes[idx + 1]), hex_value(bytes[idx + 2]))
        {
            out.push(char::from((hi << 4) | lo));
            idx += 3;
            continue;
        }
        out.push(bytes[idx] as char);
        idx += 1;
    }
    out
}

fn normalize_windows_path(path: PathBuf) -> PathBuf {
    let text = path.to_string_lossy();
    if let Some(stripped) = text.strip_prefix(r"\\?\") {
        return PathBuf::from(stripped);
    }
    path
}

fn append_uri_path_char(out: &mut String, ch: char) {
    if ch.is_ascii_alphanumeric() || matches!(ch, '/' | '-' | '_' | '.' | '~' | ':') {
        out.push(ch);
        return;
    }
    let mut buf = [0; 4];
    for byte in ch.encode_utf8(&mut buf).as_bytes() {
        out.push('%');
        out.push(hex_digit(byte >> 4));
        out.push(hex_digit(byte & 0x0f));
    }
}

fn hex_digit(value: u8) -> char {
    match value {
        0..=9 => (b'0' + value) as char,
        10..=15 => (b'A' + (value - 10)) as char,
        _ => '0',
    }
}

fn hex_value(value: u8) -> Option<u8> {
    match value {
        b'0'..=b'9' => Some(value - b'0'),
        b'a'..=b'f' => Some(value - b'a' + 10),
        b'A'..=b'F' => Some(value - b'A' + 10),
        _ => None,
    }
}

fn default_manifest_version() -> i64 {
    1
}

fn default_dependency_mode() -> String {
    DEPENDENCY_MODE_REMOTE_ON_DEMAND.to_string()
}

fn default_cache_dir() -> String {
    ".abapls/cache".to_string()
}

fn default_unknown_symbol_mode() -> String {
    UNKNOWN_SYMBOL_MODE_REMOTE.to_string()
}

fn default_workspace_performance_mode() -> String {
    WORKSPACE_PERFORMANCE_MODE_AUTO.to_string()
}

fn default_remote_requests_per_second() -> usize {
    DEFAULT_REMOTE_REQUESTS_PER_SECOND
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DdicField {
    name: String,
    type_name: Option<String>,
    builtin_type: Option<String>,
    is_table: bool,
    include_part_of: Option<String>,
}

pub fn ddic_xml_to_abap_source(object_name: &str, kind_hint: &str, xml: &str) -> Option<String> {
    let kind = kind_hint.trim().to_ascii_lowercase();
    if kind == "message-class" {
        return Some(message_class_to_abap_source(object_name, xml));
    }
    if kind == "ddic-data-element" {
        return Some(data_element_to_abap_source(object_name, xml));
    }
    if kind == "ddic-table-type" {
        return Some(table_type_to_abap_source(object_name, xml));
    }
    Some(structured_ddic_to_abap_source(object_name, xml))
}

fn data_element_to_abap_source(object_name: &str, xml: &str) -> String {
    let referenced = first_tag_text(
        xml,
        &[
            "ROLLNAME",
            "PREDEFINED_TYPE_REF",
            "REFNAME",
            "typeName",
            "referenceType",
        ],
    )
    .filter(|value| !value.eq_ignore_ascii_case(object_name))
    .map(|value| normalize_ddic_type_name(&value).into_owned())
    .or_else(|| {
        first_tag_text(xml, &["DATATYPE", "dataType"])
            .map(|value| normalize_ddic_builtin_type(&value).into_owned())
    })
    .unwrap_or_else(|| "string".to_string());
    format!(
        "TYPES {name} TYPE {ty}.\n",
        name = object_name.to_ascii_lowercase(),
        ty = referenced
    )
}

fn table_type_to_abap_source(object_name: &str, xml: &str) -> String {
    let line_type = table_type_line_type(xml).unwrap_or_else(|| {
        first_tag_text(
            xml,
            &[
                "LINE_TYPE",
                "ROWTYPE",
                "DD40V-ROWTYPE",
                "ROLLNAME",
                "REFNAME",
            ],
        )
        .unwrap_or_else(|| "string".to_string())
    });
    format!(
        "TYPES {name} TYPE STANDARD TABLE OF {line_type} WITH EMPTY KEY.\n",
        name = object_name.to_ascii_lowercase(),
        line_type = normalize_ddic_type_name(&line_type)
    )
}

fn table_type_line_type(xml: &str) -> Option<String> {
    let mut reader = Reader::from_str(xml);
    reader.config_mut().trim_text(true);
    let mut tag_stack: Vec<Vec<u8>> = Vec::new();
    let mut current_name = None::<String>;
    let mut current_property_key = None::<String>;

    loop {
        match reader.read_event() {
            Ok(Event::Start(start)) => {
                let depth = tag_stack.len();
                if is_field_start(&start) && depth == 1 {
                    current_name = attr_local_text(&start, b"name");
                }
                if local_name_eq(start.name().as_ref(), b"entry") {
                    current_property_key = attr_local_text(&start, b"key");
                }
                tag_stack.push(start.name().as_ref().to_vec());
            }
            Ok(Event::Text(text)) => {
                if current_property_key
                    .as_deref()
                    .is_some_and(|key| key.eq_ignore_ascii_case("ddicRowType"))
                {
                    let _ = text;
                }
            }
            Ok(Event::End(end)) => {
                if is_field_end(end.name().as_ref())
                    && tag_stack.len() == 2
                    && let Some(name) = current_name.take()
                {
                    return Some(name);
                }
                if local_name_eq(end.name().as_ref(), b"entry") {
                    current_property_key = None;
                }
                let _ = tag_stack.pop();
            }
            Ok(Event::Empty(start)) => {
                if is_field_start(&start) && tag_stack.len() == 1 {
                    return attr_local_text(&start, b"name");
                }
            }
            Ok(Event::Eof) => break,
            Err(_) => break,
            _ => {}
        }
    }
    None
}

fn structured_ddic_to_abap_source(object_name: &str, xml: &str) -> String {
    let fields = collect_ddic_fields(xml);
    if fields.is_empty() {
        return format!(
            "TYPES {name} TYPE string.\n",
            name = object_name.to_ascii_lowercase()
        );
    }

    let mut out = String::new();
    out.push_str(&format!(
        "TYPES: BEGIN OF {name},\n",
        name = object_name.to_ascii_lowercase()
    ));
    for (idx, field) in fields.iter().enumerate() {
        let suffix = if idx + 1 == fields.len() { "" } else { "," };
        let ty = if let Some(type_name) = field.type_name.as_ref() {
            normalize_ddic_type_name(type_name)
        } else if let Some(builtin) = field.builtin_type.as_ref() {
            normalize_ddic_builtin_type(builtin)
        } else {
            Cow::Borrowed("string")
        };
        if field.is_table {
            out.push_str(&format!(
                "  {field_name} TYPE STANDARD TABLE OF {ty} WITH EMPTY KEY{suffix}\n",
                field_name = field.name.to_ascii_lowercase(),
            ));
        } else {
            out.push_str(&format!(
                "  {field_name} TYPE {ty}{suffix}\n",
                field_name = field.name.to_ascii_lowercase(),
            ));
        }
    }
    out.push_str(&format!(
        "END OF {name}.\n",
        name = object_name.to_ascii_lowercase()
    ));
    out
}

fn message_class_to_abap_source(object_name: &str, xml: &str) -> String {
    let mut out = format!(
        "TYPES {name} TYPE c LENGTH 1.\n",
        name = object_name.to_ascii_lowercase()
    );
    let messages = collect_message_class_messages(xml);
    if messages.is_empty() {
        return out;
    }

    for (msgno, msgtext) in messages {
        out.push_str(&format!(
            "\" MESSAGE {msgno}: {msgtext}\n",
            msgno = msgno,
            msgtext = msgtext.replace('\r', " ").replace('\n', " ")
        ));
    }
    out
}

fn collect_message_class_messages(xml: &str) -> Vec<(String, String)> {
    let mut reader = Reader::from_str(xml);
    reader.config_mut().trim_text(true);
    let mut messages = Vec::new();

    loop {
        match reader.read_event() {
            Ok(Event::Start(start)) | Ok(Event::Empty(start)) => {
                if !matches_local_name(start.name().as_ref(), &[b"messages"]) {
                    continue;
                }
                let Some(msgno) = attr_local_text(&start, b"msgno") else {
                    continue;
                };
                let msgtext = attr_local_text(&start, b"msgtext").unwrap_or_default();
                messages.push((msgno, msgtext));
            }
            Ok(Event::Eof) => break,
            Err(_) => break,
            _ => {}
        }
    }

    messages
}

fn collect_ddic_fields(xml: &str) -> Vec<DdicField> {
    let mut reader = Reader::from_str(xml);
    reader.config_mut().trim_text(true);
    let mut fields = Vec::new();
    let mut current = None::<DdicField>;
    let mut current_property_key = None::<String>;
    let mut tag_stack: Vec<Vec<u8>> = Vec::new();

    loop {
        match reader.read_event() {
            Ok(Event::Start(start)) => {
                if is_field_start(&start) && !tag_stack.is_empty() {
                    current = Some(DdicField {
                        name: attr_local_text(&start, b"name").unwrap_or_default(),
                        type_name: attr_local_text(&start, b"rollname")
                            .or_else(|| attr_local_text(&start, b"refname")),
                        builtin_type: attr_local_text(&start, b"datatype"),
                        is_table: attr_local_text(&start, b"isTableType")
                            .is_some_and(|value| value.eq_ignore_ascii_case("true")),
                        include_part_of: None,
                    });
                }
                if local_name_eq(start.name().as_ref(), b"entry") {
                    current_property_key = attr_local_text(&start, b"key");
                }
                tag_stack.push(start.name().as_ref().to_vec());
            }
            Ok(Event::Empty(start)) => {
                if is_field_start(&start) && !tag_stack.is_empty() {
                    let field = DdicField {
                        name: attr_local_text(&start, b"name").unwrap_or_default(),
                        type_name: attr_local_text(&start, b"rollname")
                            .or_else(|| attr_local_text(&start, b"refname")),
                        builtin_type: attr_local_text(&start, b"datatype"),
                        is_table: attr_local_text(&start, b"isTableType")
                            .is_some_and(|value| value.eq_ignore_ascii_case("true")),
                        include_part_of: None,
                    };
                    if !field.name.is_empty() {
                        fields.push(field);
                    }
                }
            }
            Ok(Event::Text(text)) => {
                let Some(current) = current.as_mut() else {
                    continue;
                };
                let name = tag_stack
                    .last()
                    .map(|tag| tag.as_slice())
                    .unwrap_or_default();
                let value = text.decode().ok().map(Cow::into_owned).unwrap_or_default();
                if current_property_key
                    .as_deref()
                    .is_some_and(|key| key.eq_ignore_ascii_case("ddicIncludeName"))
                {
                    current.type_name = Some(value.clone());
                    current.name = derive_ddic_include_field_name(&value);
                    continue;
                }
                if current_property_key
                    .as_deref()
                    .is_some_and(|key| key.eq_ignore_ascii_case("ddicIsPartOfInclude"))
                {
                    current.include_part_of = Some(value);
                    continue;
                }
                if current_property_key
                    .as_deref()
                    .is_some_and(|key| key.eq_ignore_ascii_case("ddicDataElement"))
                    && current.type_name.is_none()
                {
                    current.type_name = Some(value);
                    continue;
                }
                if current_property_key
                    .as_deref()
                    .is_some_and(|key| key.eq_ignore_ascii_case("ddicDataType"))
                    && current.builtin_type.is_none()
                {
                    current.builtin_type = Some(value);
                    continue;
                }
                if matches_local_name(name, &[b"name", b"fieldname", b"scrtext_s"])
                    && current.name.is_empty()
                {
                    current.name = value;
                } else if matches_local_name(name, &[b"rollname", b"refname", b"comptype"])
                    && current.type_name.is_none()
                {
                    current.type_name = Some(value);
                } else if matches_local_name(
                    name,
                    &[
                        b"datatype",
                        b"builtintype",
                        b"ddicdatatype",
                        b"datatypekind",
                    ],
                ) && current.builtin_type.is_none()
                {
                    current.builtin_type = Some(value);
                }
            }
            Ok(Event::End(end)) => {
                if is_field_end(end.name().as_ref())
                    && let Some(field) = current.take()
                    && !field.name.is_empty()
                {
                    fields.push(field);
                }
                if local_name_eq(end.name().as_ref(), b"entry") {
                    current_property_key = None;
                }
                let _ = tag_stack.pop();
            }
            Ok(Event::Eof) => break,
            Err(_) => break,
            _ => {}
        }
    }

    let mut deduped = BTreeMap::<String, DdicField>::new();
    for field in fields {
        deduped
            .entry(field.name.to_ascii_lowercase())
            .or_insert(field);
    }
    deduped.into_values().collect()
}

fn is_field_start(start: &BytesStart<'_>) -> bool {
    matches_local_name(
        start.name().as_ref(),
        &[b"elementinfo", b"component", b"field"],
    )
}

fn is_field_end(name: &[u8]) -> bool {
    matches_local_name(name, &[b"elementinfo", b"component", b"field"])
}

fn attr_local_text(start: &BytesStart<'_>, key: &[u8]) -> Option<String> {
    start
        .attributes()
        .flatten()
        .find(|attr| local_name_eq(attr.key.as_ref(), key))
        .and_then(|attr| String::from_utf8(attr.value.into_owned()).ok())
}

fn first_tag_text(xml: &str, tags: &[&str]) -> Option<String> {
    let mut reader = Reader::from_str(xml);
    reader.config_mut().trim_text(true);
    let tags: HashSet<Vec<u8>> = tags
        .iter()
        .map(|tag| tag.as_bytes().iter().map(u8::to_ascii_lowercase).collect())
        .collect();
    let mut current = None::<Vec<u8>>;
    loop {
        match reader.read_event() {
            Ok(Event::Start(start)) => {
                current = Some(local_name(start.name().as_ref()).to_ascii_lowercase());
            }
            Ok(Event::Text(text)) => {
                let Some(tag) = current.as_ref() else {
                    continue;
                };
                if tags.contains(tag) {
                    return text.decode().ok().map(Cow::into_owned);
                }
            }
            Ok(Event::Eof) => break,
            Err(_) => break,
            _ => {}
        }
    }
    None
}

fn local_name(name: &[u8]) -> &[u8] {
    name.rsplit(|byte| *byte == b':').next().unwrap_or(name)
}

fn local_name_eq(actual: &[u8], expected: &[u8]) -> bool {
    local_name(actual).eq_ignore_ascii_case(expected)
}

fn matches_local_name(actual: &[u8], expected: &[&[u8]]) -> bool {
    expected
        .iter()
        .any(|candidate| local_name_eq(actual, candidate))
}

fn normalize_ddic_type_name(value: &str) -> Cow<'_, str> {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        return Cow::Borrowed("string");
    }
    Cow::Owned(trimmed.to_ascii_lowercase())
}

fn derive_ddic_include_field_name(type_name: &str) -> String {
    let tail = type_name
        .rsplit('/')
        .next()
        .unwrap_or(type_name)
        .trim()
        .to_ascii_lowercase();
    tail.strip_prefix("s_")
        .or_else(|| tail.strip_prefix("t_"))
        .unwrap_or(&tail)
        .to_string()
}

fn normalize_ddic_builtin_type(value: &str) -> Cow<'_, str> {
    match value.trim().to_ascii_uppercase().as_str() {
        "CHAR" | "SSTRING" | "STRING" | "UNIT" | "CUKY" | "LANG" | "CLNT" | "NUMC" => {
            Cow::Borrowed("string")
        }
        "INT1" | "INT2" | "INT4" | "INT8" => Cow::Borrowed("i"),
        "DEC" | "CURR" | "QUAN" | "FLTP" | "DF16_DEC" | "DF34_DEC" => Cow::Borrowed("p"),
        "DATS" => Cow::Borrowed("d"),
        "TIMS" => Cow::Borrowed("t"),
        "RAW" | "RAWSTRING" | "LRAW" | "LCHR" => Cow::Borrowed("xstring"),
        other if !other.is_empty() => Cow::Owned(other.to_ascii_lowercase()),
        _ => Cow::Borrowed("string"),
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::fs;
    use std::sync::Arc;

    use super::{
        DEFAULT_REMOTE_REQUESTS_PER_SECOND, LocalExportResolver, OpenDocumentOverlay,
        UNKNOWN_SYMBOL_MODE_REMOTE, WORKSPACE_PERFORMANCE_MODE_AUTO,
        WORKSPACE_PERFORMANCE_MODE_EDITOR_FIRST, WorkspaceManifest, WorkspacePerformanceMode,
        ddic_xml_to_abap_source, is_remote_lookup_candidate,
        is_remote_lookup_candidate_after_local_resolution, is_remote_lookup_name,
        load_manifest_from_workspace, load_workspace_documents, manifest_declares_uri,
        manifest_document_metadata, manifest_supports_remote_resolution, path_to_file_uri,
        resolve_local_export_dependency_document, resolve_workspace_performance_mode,
    };

    #[test]
    fn parses_manifest_defaults() {
        let manifest: WorkspaceManifest = toml::from_str("version = 1\n").expect("manifest");
        assert_eq!(
            manifest.resolution.unknown_symbol_mode,
            UNKNOWN_SYMBOL_MODE_REMOTE
        );
        assert_eq!(
            manifest.resolution.remote_requests_per_second,
            DEFAULT_REMOTE_REQUESTS_PER_SECOND
        );
        assert_eq!(manifest.resolution.remote_request_parallelism(), None);
        assert_eq!(manifest.performance.mode, WORKSPACE_PERFORMANCE_MODE_AUTO);
    }

    #[test]
    fn preserves_legacy_remote_request_parallelism_override() {
        let manifest: WorkspaceManifest = toml::from_str(
            r#"
version = 1

[resolution]
remote_request_parallelism = 6
remote_requests_per_second = 12
"#,
        )
        .expect("manifest");

        assert_eq!(manifest.resolution.remote_request_parallelism(), Some(6));
        assert_eq!(manifest.resolution.remote_requests_per_second, 12);
    }

    #[test]
    fn parses_manifest_performance_mode() {
        let manifest: WorkspaceManifest = toml::from_str(
            r#"
version = 1

[performance]
mode = "editor-first"
"#,
        )
        .expect("manifest");

        assert_eq!(
            manifest.performance.mode,
            WORKSPACE_PERFORMANCE_MODE_EDITOR_FIRST
        );
    }

    #[test]
    fn resolves_auto_performance_mode_to_editor_first_for_large_manifest() {
        let manifest: WorkspaceManifest = toml::from_str(
            r#"
version = 1

[[unit]]
name = "ZCL_MAIN"
kind = "global-class"
root_file = "src/ZCL_MAIN.abap"

[[unit]]
name = "ZCL_DEP"
kind = "global-class"
root_file = ".abapls/cache/dependencies/global-class/ZCL_DEP.abap"
"#,
        )
        .expect("manifest");

        assert_eq!(
            resolve_workspace_performance_mode(Some(&manifest), 1_000_000),
            WorkspacePerformanceMode::EditorFirst
        );
    }

    #[test]
    fn resolves_explicit_full_workspace_mode() {
        let manifest: WorkspaceManifest = toml::from_str(
            r#"
version = 1

[performance]
mode = "full-workspace"
"#,
        )
        .expect("manifest");

        assert_eq!(
            resolve_workspace_performance_mode(Some(&manifest), 10_000_000),
            WorkspacePerformanceMode::FullWorkspace
        );
    }

    #[test]
    fn converts_data_element_xml_to_type_alias() {
        let xml = "<root><DATATYPE>CHAR</DATATYPE></root>";
        let source = ddic_xml_to_abap_source("ZDEMO", "ddic-data-element", xml).expect("source");
        assert!(
            source
                .to_ascii_lowercase()
                .contains("types zdemo type string")
        );
    }

    #[test]
    fn converts_namespaced_data_element_xml_to_type_alias() {
        let xml = r#"
<blue:wbobj xmlns:blue="http://www.sap.com/wbobj/dictionary/dtel">
  <dtel:dataElement xmlns:dtel="http://www.sap.com/adt/dictionary/dataelements">
    <dtel:typeKind>domain</dtel:typeKind>
    <dtel:typeName>/STTP/D_ACTION_XML</dtel:typeName>
    <dtel:dataType>NUMC</dtel:dataType>
  </dtel:dataElement>
</blue:wbobj>
"#;
        let source = ddic_xml_to_abap_source("/STTP/E_ACTION_FILE", "ddic-data-element", xml)
            .expect("source");
        assert!(
            source
                .to_ascii_lowercase()
                .contains("types /sttp/e_action_file type /sttp/d_action_xml")
        );
    }

    #[test]
    fn converts_elementinfo_xml_to_structure() {
        let xml = r#"
<root>
  <elementInfo name="FIELD_ONE" rollname="BUKRS" />
  <elementInfo name="FIELD_TWO" datatype="CHAR" />
</root>
"#;
        let source = ddic_xml_to_abap_source("ZSTR", "ddic-structure", xml).expect("source");
        let lowered = source.to_ascii_lowercase();
        assert!(lowered.contains("begin of zstr"));
        assert!(lowered.contains("field_one type bukrs"));
        assert!(lowered.contains("field_two type string"));
    }

    #[test]
    fn converts_namespaced_elementinfo_xml_to_structure() {
        let xml = r#"
<abapsource:elementInfo adtcore:name="/sttp/epc1"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="controller">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">prxctrltab</abapsource:entry>
      <abapsource:entry abapsource:key="ddicDataType">ttyp</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="content">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataType">string</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#;
        let source = ddic_xml_to_abap_source("/STTP/EPC1", "ddic-structure", xml).expect("source");
        let lowered = source.to_ascii_lowercase();
        assert!(lowered.contains("begin of /sttp/epc1"));
        assert!(lowered.contains("controller type prxctrltab"));
        assert!(lowered.contains("content type string"));
        assert!(!lowered.contains("/sttp/epc1 type"));
    }

    #[test]
    fn converts_message_class_xml_to_dependency_source() {
        let xml = r#"
<mc:messageClass adtcore:name="/STTP/INT_MSG"
    xmlns:mc="http://www.sap.com/adt/MessageClass"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <mc:messages mc:msgno="043" mc:msgtext="Received &amp;1 documents"/>
</mc:messageClass>
"#;
        let source =
            ddic_xml_to_abap_source("/STTP/INT_MSG", "message-class", xml).expect("source");
        let lowered = source.to_ascii_lowercase();
        assert!(lowered.contains("types /sttp/int_msg type c length 1"));
        assert!(lowered.contains("\" message 043:"));
    }

    #[test]
    fn converts_ddic_proxy_include_entries_to_named_nested_fields() {
        let xml = r#"
<abapsource:elementInfo adtcore:name="/sttp/s_encode_decode"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DS" adtcore:name=".include">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicIncludeName">/sttp/s_obj_ids</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="owner">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">/sttp/e_gen_owner</abapsource:entry>
      <abapsource:entry abapsource:key="ddicIsPartOfInclude">/sttp/s_obj_ids</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#;
        let source = ddic_xml_to_abap_source("/STTP/S_ENCODE_DECODE", "ddic-structure", xml)
            .expect("source");
        let lowered = source.to_ascii_lowercase();
        assert!(lowered.contains("obj_ids type /sttp/s_obj_ids"));
        assert!(!lowered.contains(".include"));
        assert!(lowered.contains("owner type /sttp/e_gen_owner"));
    }

    #[test]
    fn converts_namespaced_table_type_xml_to_table_type() {
        let xml = r#"
<abapsource:elementInfo adtcore:name="/aif/pers_xml_tt"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DT" adtcore:name="/aif/pers_xml">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicRowType">X</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#;
        let source =
            ddic_xml_to_abap_source("/AIF/PERS_XML_TT", "ddic-table-type", xml).expect("source");
        assert!(source.to_ascii_lowercase().contains(
            "types /aif/pers_xml_tt type standard table of /aif/pers_xml with empty key"
        ));
    }

    #[test]
    fn local_export_resolver_requires_compatible_artifact_kind_for_type_candidates() {
        let root = std::env::temp_dir().join("abap-lsp-local-export-kind-filter");
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(root.join("packages/EE30/function-group")).expect("function group dir");
        fs::create_dir_all(root.join("packages/ME/ddic-table")).expect("ddic table dir");
        fs::create_dir_all(root.join("packages/ZPKG/global-class")).expect("class dir");
        fs::write(
            root.join("packages/EE30/function-group/EKKO.abap"),
            "FUNCTION-POOL ekko.",
        )
        .expect("function group");
        fs::write(
            root.join("packages/ME/ddic-table/EKPO.xml"),
            r#"<?xml version="1.0" encoding="utf-8"?><abapsource:elementInfo adtcore:uri="/sap/bc/adt/vit/wb/object_type/tabldt/object_name/EKPO" adtcore:type="TABL/DT" adtcore:name="ekpo" xmlns:abapsource="http://www.sap.com/adt/abapsource" xmlns:adtcore="http://www.sap.com/adt/core"></abapsource:elementInfo>"#,
        )
        .expect("ddic table");
        fs::write(
            root.join("packages/ZPKG/global-class/ZCL_DEMO.abap"),
            "CLASS zcl_demo DEFINITION. ENDCLASS.",
        )
        .expect("class");

        let mut resolver = LocalExportResolver::default();
        let roots = vec![root.clone()];

        assert!(
            resolve_local_export_dependency_document(&roots, &mut resolver, "ekko", "type")
                .is_none()
        );
        assert!(
            resolve_local_export_dependency_document(&roots, &mut resolver, "ekpo", "type")
                .is_some()
        );
        assert!(
            resolve_local_export_dependency_document(&roots, &mut resolver, "zcl_demo", "type")
                .is_some()
        );

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn local_export_resolver_accepts_flat_root_function_module_exports() {
        let root = std::env::temp_dir().join("abap-lsp-local-export-flat-function-module");
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(&root).expect("root dir");
        fs::write(
            root.join("ENQUEUE_E_TABLEE.abap"),
            concat!(
                "* generated export\n",
                "FUNCTION ENQUEUE_E_TABLEE\n",
                "ENDFUNCTION.\n",
            ),
        )
        .expect("function module");

        let mut resolver = LocalExportResolver::default();
        let roots = vec![root.clone()];
        let document = resolve_local_export_dependency_document(
            &roots,
            &mut resolver,
            "enqueue_e_tablee",
            "function",
        )
        .expect("flat function module");

        assert!(
            document.uri.ends_with("/ENQUEUE_E_TABLEE.abap"),
            "{}",
            document.uri
        );
        assert!(
            document
                .text
                .to_ascii_lowercase()
                .contains("function enqueue_e_tablee"),
            "{}",
            document.text
        );

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn detects_remote_lookup_candidate_names() {
        assert!(is_remote_lookup_name("zcl_demo"));
        assert!(is_remote_lookup_name("/foo/bar"));
        assert!(!is_remote_lookup_name("cl_abap_typedescr"));
    }

    #[test]
    fn detects_remote_lookup_candidates_by_kind() {
        assert!(is_remote_lookup_candidate("cl_abap_typedescr", "type"));
        assert!(is_remote_lookup_candidate("if_sxml_reader", "static"));
        assert!(is_remote_lookup_candidate("rfc_ping", "function"));
        assert!(is_remote_lookup_candidate("cx_root", "type"));
        assert!(is_remote_lookup_candidate("00", "message-class"));
        assert!(is_remote_lookup_candidate("/sttp/int_msg", "message-class"));
        assert!(is_remote_lookup_candidate(
            "/aif/file_process_data",
            "function"
        ));
        assert!(is_remote_lookup_candidate("boolean", "type"));
        assert!(!is_remote_lookup_candidate("cl_abap_typedescr", "symbol"));
        assert!(!is_remote_lookup_candidate("lv_function_name", "function"));
        assert!(!is_remote_lookup_candidate("lv_type_name", "type"));
        assert!(!is_remote_lookup_candidate("time", "type"));
        assert!(!is_remote_lookup_candidate("timestmp", "type"));
        assert!(!is_remote_lookup_candidate("object", "type"));
        assert!(!is_remote_lookup_candidate("standard", "type"));
        assert!(!is_remote_lookup_candidate("csequence", "type"));
        assert!(!is_remote_lookup_candidate("clike", "type"));
        assert!(!is_remote_lookup_candidate("xsequence", "type"));
        assert!(!is_remote_lookup_candidate("decfloat", "type"));
        assert!(!is_remote_lookup_candidate("lv_msgid", "message-class"));
    }

    #[test]
    fn detects_remote_lookup_candidates_after_local_resolution() {
        assert!(is_remote_lookup_candidate_after_local_resolution(
            "tt_ltap_vb",
            "type"
        ));
        assert!(is_remote_lookup_candidate_after_local_resolution(
            "ct_messages",
            "static"
        ));
        assert!(is_remote_lookup_candidate_after_local_resolution(
            "cl_abap_typedescr",
            "type"
        ));
        assert!(is_remote_lookup_candidate_after_local_resolution(
            "md_convert_material_unit",
            "function"
        ));
        assert!(is_remote_lookup_candidate_after_local_resolution(
            "sd_route_determination",
            "function"
        ));
        assert!(is_remote_lookup_candidate_after_local_resolution(
            "ws_delivery_update_2",
            "function"
        ));
        assert!(is_remote_lookup_candidate_after_local_resolution(
            "rsnast00", "report"
        ));
        assert!(!is_remote_lookup_candidate_after_local_resolution(
            "time", "type"
        ));
        assert!(!is_remote_lookup_candidate_after_local_resolution(
            "standard", "type"
        ));
        assert!(!is_remote_lookup_candidate_after_local_resolution(
            "cl_abap_typedescr",
            "symbol"
        ));
    }

    #[test]
    fn manifest_supports_log_mode_for_candidate_reporting() {
        let manifest: WorkspaceManifest = toml::from_str(
            r#"
version = 1

[resolution]
dependency_mode = "remote-on-demand"
unknown_symbol_mode = "log"
"#,
        )
        .expect("manifest");

        assert!(manifest_supports_remote_resolution(Some(&manifest)));
    }

    #[test]
    fn reports_manifest_parse_errors() {
        let root = std::env::temp_dir().join("abap-lsp-invalid-manifest");
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(&root).expect("root dir");
        fs::write(
            root.join("abapls.toml"),
            "version = 1\n[[unit]]\nname = \"X\"[[unit]]\n",
        )
        .expect("manifest");

        let loaded = load_workspace_documents(&path_to_file_uri(&root), &HashMap::new());
        assert!(loaded.manifest.is_none());
        assert!(loaded.manifest_error.is_some());

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn manifest_limits_workspace_local_sources_to_declared_files() {
        let root = std::env::temp_dir().join("abap-lsp-manifest-local-sources");
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(root.join("src")).expect("src dir");
        fs::write(
            root.join("abapls.toml"),
            r#"
version = 1

[[unit]]
name = "ZCL_MANAGED"
kind = "global-class"
root_file = "src/ZCL_MANAGED.abap"
"#,
        )
        .expect("manifest");
        fs::write(
            root.join("src/ZCL_MANAGED.abap"),
            "CLASS zcl_managed DEFINITION. ENDCLASS.",
        )
        .expect("managed");
        fs::write(root.join("src/ZCL_LOOSE.abap"), "REPORT zcl_loose.").expect("loose");

        let loaded = load_workspace_documents(&path_to_file_uri(&root), &HashMap::new());
        let uris: Vec<_> = loaded
            .documents
            .iter()
            .map(|doc| doc.uri.as_ref())
            .collect();

        assert!(
            uris.iter()
                .any(|uri| uri.ends_with("/src/ZCL_MANAGED.abap"))
        );
        assert!(!uris.iter().any(|uri| uri.ends_with("/src/ZCL_LOOSE.abap")));

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn manifest_declares_root_and_member_uris() {
        let root = std::env::temp_dir().join("abap-lsp-manifest-declared-uris");
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(root.join("src")).expect("src dir");
        fs::write(
            root.join("abapls.toml"),
            r#"
version = 1

[[unit]]
name = "ZCL_MAIN"
kind = "global-class"
root_file = "src/ZCL_MAIN.abap"
members = [
  { file = "src/ZTOP.abap", object_name = "ZTOP" }
]
"#,
        )
        .expect("manifest");

        let manifest: WorkspaceManifest =
            toml::from_str(&fs::read_to_string(root.join("abapls.toml")).expect("manifest text"))
                .expect("manifest");
        let root_uri = path_to_file_uri(&root);

        assert!(manifest_declares_uri(
            &root,
            &root_uri,
            &manifest,
            &format!("{root_uri}/src/ZCL_MAIN.abap")
        ));
        assert!(manifest_declares_uri(
            &root,
            &root_uri,
            &manifest,
            &format!("{root_uri}/src/ZTOP.abap")
        ));
        assert!(!manifest_declares_uri(
            &root,
            &root_uri,
            &manifest,
            &format!("{root_uri}/src/ZOTHER.abap")
        ));

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn manifest_loads_only_direct_dependency_layer_until_dependency_is_opened() {
        let root = std::env::temp_dir().join("abap-lsp-manifest-dependency-layers");
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(root.join("src")).expect("src dir");
        fs::create_dir_all(root.join(".abapls/cache/packages/ZPKG/global-class"))
            .expect("dependency dir");
        fs::write(
            root.join("abapls.toml"),
            r#"
version = 1

[resolution]
cache_dir = ".abapls/cache"

[[unit]]
name = "ZMAIN"
kind = "report"
root_file = "src/ZMAIN.abap"

[[unit]]
name = "ZCL_FIRST"
kind = "global-class"
root_file = ".abapls/cache/packages/ZPKG/global-class/ZCL_FIRST.abap"
dependency_of = [
  "src/ZMAIN.abap"
]

[[unit]]
name = "ZCL_SECOND"
kind = "global-class"
root_file = ".abapls/cache/packages/ZPKG/global-class/ZCL_SECOND.abap"
dependency_of = [
  ".abapls/cache/packages/ZPKG/global-class/ZCL_FIRST.abap"
]
"#,
        )
        .expect("manifest");
        fs::write(root.join("src/ZMAIN.abap"), "REPORT zmain.").expect("main");
        fs::write(
            root.join(".abapls/cache/packages/ZPKG/global-class/ZCL_FIRST.abap"),
            "CLASS zcl_first DEFINITION. ENDCLASS.",
        )
        .expect("first");
        fs::write(
            root.join(".abapls/cache/packages/ZPKG/global-class/ZCL_SECOND.abap"),
            "CLASS zcl_second DEFINITION. ENDCLASS.",
        )
        .expect("second");

        let root_uri = path_to_file_uri(&root);
        let loaded = load_workspace_documents(&root_uri, &HashMap::new());
        let loaded_uris: Vec<_> = loaded
            .documents
            .iter()
            .map(|document| document.uri.as_ref())
            .collect();
        assert!(
            loaded_uris
                .iter()
                .any(|uri| uri.ends_with("/src/ZMAIN.abap"))
        );
        assert!(
            loaded_uris.iter().any(
                |uri| uri.ends_with("/.abapls/cache/packages/ZPKG/global-class/ZCL_FIRST.abap")
            )
        );
        assert!(
            !loaded_uris
                .iter()
                .any(|uri| uri
                    .ends_with("/.abapls/cache/packages/ZPKG/global-class/ZCL_SECOND.abap"))
        );

        let dependency_uri =
            format!("{root_uri}/.abapls/cache/packages/ZPKG/global-class/ZCL_FIRST.abap");
        let mut overlays = HashMap::new();
        overlays.insert(
            dependency_uri,
            OpenDocumentOverlay {
                version: 1,
                text: Arc::from("CLASS zcl_first DEFINITION. ENDCLASS."),
            },
        );
        let opened = load_workspace_documents(&root_uri, &overlays);
        let opened_uris: Vec<_> = opened
            .documents
            .iter()
            .map(|document| document.uri.as_ref())
            .collect();
        assert!(
            opened_uris
                .iter()
                .any(|uri| uri
                    .ends_with("/.abapls/cache/packages/ZPKG/global-class/ZCL_SECOND.abap"))
        );

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn cache_side_dependency_manifests_extend_workspace_without_polluting_project_manifest() {
        let root = std::env::temp_dir().join("abap-lsp-cache-side-dependency-manifests");
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(root.join("src")).expect("src dir");
        fs::create_dir_all(root.join(".abapls/cache/dependency-manifests"))
            .expect("dependency manifest dir");
        fs::create_dir_all(root.join(".abapls/cache/packages/ZPKG/global-class"))
            .expect("package dir");
        fs::write(
            root.join("abapls.toml"),
            r#"
version = 1

[resolution]
cache_dir = ".abapls/cache"

[[unit]]
name = "ZMAIN"
kind = "report"
root_file = "src/ZMAIN.abap"
"#,
        )
        .expect("manifest");
        fs::write(root.join("src/ZMAIN.abap"), "REPORT zmain.").expect("main");
        fs::write(
            root.join(".abapls/cache/packages/ZPKG/global-class/ZCL_FIRST.abap"),
            "CLASS zcl_first DEFINITION. ENDCLASS.",
        )
        .expect("first");
        fs::write(
            root.join(".abapls/cache/packages/ZPKG/global-class/ZCL_SECOND.abap"),
            "CLASS zcl_second DEFINITION. ENDCLASS.",
        )
        .expect("second");
        fs::write(
            root.join(".abapls/cache/dependency-manifests/src%2FZMAIN.abap.toml"),
            r#"
source_file = "src/ZMAIN.abap"

[[unit]]
name = "ZCL_FIRST"
kind = "global-class"
package_name = "ZPKG"
root_file = ".abapls/cache/packages/ZPKG/global-class/ZCL_FIRST.abap"
"#,
        )
        .expect("root dependency manifest");
        fs::write(
            root.join(
                ".abapls/cache/dependency-manifests/.abapls%2Fcache%2Fpackages%2FZPKG%2Fglobal-class%2FZCL_FIRST.abap.toml",
            ),
            r#"
source_file = ".abapls/cache/packages/ZPKG/global-class/ZCL_FIRST.abap"

[[unit]]
name = "ZCL_SECOND"
kind = "global-class"
package_name = "ZPKG"
root_file = ".abapls/cache/packages/ZPKG/global-class/ZCL_SECOND.abap"
"#,
        )
        .expect("nested dependency manifest");

        let root_uri = path_to_file_uri(&root);
        let loaded = load_workspace_documents(&root_uri, &HashMap::new());
        let loaded_uris: Vec<_> = loaded
            .documents
            .iter()
            .map(|document| document.uri.as_ref())
            .collect();
        assert_eq!(
            loaded
                .manifest
                .as_ref()
                .map(|manifest| manifest.units.len()),
            Some(3)
        );
        assert!(
            loaded_uris
                .iter()
                .any(|uri| uri.ends_with("/src/ZMAIN.abap"))
        );
        assert!(
            loaded_uris.iter().any(
                |uri| uri.ends_with("/.abapls/cache/packages/ZPKG/global-class/ZCL_FIRST.abap")
            )
        );
        assert!(
            loaded_uris
                .iter()
                .any(|uri| uri
                    .ends_with("/.abapls/cache/packages/ZPKG/global-class/ZCL_SECOND.abap"))
        );

        let dependency_uri =
            format!("{root_uri}/.abapls/cache/packages/ZPKG/global-class/ZCL_FIRST.abap");
        let mut overlays = HashMap::new();
        overlays.insert(
            dependency_uri,
            OpenDocumentOverlay {
                version: 1,
                text: Arc::from("CLASS zcl_first DEFINITION. ENDCLASS."),
            },
        );
        let opened = load_workspace_documents(&root_uri, &overlays);
        let opened_uris: Vec<_> = opened
            .documents
            .iter()
            .map(|document| document.uri.as_ref())
            .collect();
        assert_eq!(
            opened
                .manifest
                .as_ref()
                .map(|manifest| manifest.units.len()),
            Some(3)
        );
        assert!(
            opened_uris
                .iter()
                .any(|uri| uri
                    .ends_with("/.abapls/cache/packages/ZPKG/global-class/ZCL_SECOND.abap"))
        );

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn manifest_metadata_infers_object_names_for_concise_multi_file_units() {
        let root = std::env::temp_dir().join("abap-lsp-concise-multi-file-manifest");
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(root.join("src/function-groups/%2FSTTP%2FSHF_MD/includes"))
            .expect("includes dir");
        fs::write(
            root.join("abapls.toml"),
            r#"
version = 1

[resolution]
cache_dir = ".abapls/cache"

[[unit]]
name = "/STTP/SHF_MD"
kind = "function-group"
root_file = "src/function-groups/%2FSTTP%2FSHF_MD/%2FSTTP%2FSHF_MD.abap"
members = [
  "src/function-groups/%2FSTTP%2FSHF_MD/includes/%2FSTTP%2FLSHF_MDTOP.abap"
]
"#,
        )
        .expect("manifest");
        let manifest = load_manifest_from_workspace(&root).expect("manifest");
        let root_uri = path_to_file_uri(&root);
        let include_uri = path_to_file_uri(
            &root.join("src/function-groups/%2FSTTP%2FSHF_MD/includes/%2FSTTP%2FLSHF_MDTOP.abap"),
        );

        let metadata = manifest_document_metadata(&root, &root_uri, &manifest, &include_uri)
            .expect("metadata");
        assert!(!metadata.0);
        assert_eq!(metadata.1.as_deref(), Some("/sttp/lshf_mdtop"));

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn settings_only_manifest_discovers_src_units_by_convention() {
        let root = std::env::temp_dir().join("abap-lsp-settings-only-discovery");
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(root.join("src/classes")).expect("classes dir");
        fs::create_dir_all(root.join("src/reports/ZREP/forms")).expect("report dir");
        fs::create_dir_all(root.join("src/function-groups/ZFG/includes"))
            .expect("function group dir");
        fs::write(
            root.join("abapls.toml"),
            r#"
version = 1

[resolution]
dependency_mode = "local-first"
cache_dir = ".abapls/cache"
unknown_symbol_mode = "log"
"#,
        )
        .expect("manifest");
        fs::write(
            root.join("src/classes/ZCL_GLOBAL.abap"),
            "CLASS zcl_global DEFINITION. ENDCLASS.",
        )
        .expect("class");
        fs::write(root.join("src/reports/ZREP/ZREP.abap"), "REPORT zrep.").expect("report");
        fs::write(
            root.join("src/reports/ZREP/forms/ZREP_F01.abap"),
            "FORM demo. ENDFORM.",
        )
        .expect("report member");
        fs::write(
            root.join("src/function-groups/ZFG/ZFG.abap"),
            "FUNCTION-POOL zfg.",
        )
        .expect("function group");
        fs::write(
            root.join("src/function-groups/ZFG/includes/LZFGTOP.abap"),
            "* include",
        )
        .expect("function group member");

        let root_uri = path_to_file_uri(&root);
        let loaded = load_workspace_documents(&root_uri, &HashMap::new());
        let manifest = loaded.manifest.as_ref().expect("effective manifest");
        let loaded_uris: Vec<_> = loaded
            .documents
            .iter()
            .map(|document| document.uri.as_ref())
            .collect();

        assert!(
            manifest.units.iter().any(|unit| unit.kind == "global-class"
                && unit.root_file == "src/classes/ZCL_GLOBAL.abap")
        );
        assert!(manifest
            .units
            .iter()
            .any(|unit| unit.kind == "report" && unit.root_file == "src/reports/ZREP/ZREP.abap"));
        assert!(manifest.units.iter().any(|unit| {
            unit.kind == "function-group"
                && unit.root_file == "src/function-groups/ZFG/ZFG.abap"
                && unit
                    .members
                    .iter()
                    .any(|member| member.file == "src/function-groups/ZFG/includes/LZFGTOP.abap")
        }));
        assert!(
            loaded_uris
                .iter()
                .any(|uri| uri.ends_with("/src/classes/ZCL_GLOBAL.abap"))
        );
        assert!(
            loaded_uris
                .iter()
                .any(|uri| uri.ends_with("/src/reports/ZREP/ZREP.abap"))
        );
        assert!(
            loaded_uris
                .iter()
                .any(|uri| uri.ends_with("/src/reports/ZREP/forms/ZREP_F01.abap"))
        );
        assert!(
            loaded_uris
                .iter()
                .any(|uri| uri.ends_with("/src/function-groups/ZFG/includes/LZFGTOP.abap"))
        );

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn unit_sidecar_includes_add_explicit_include_members() {
        let root = std::env::temp_dir().join("abap-lsp-unit-sidecar-include-map");
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(root.join("src/reports/ZREP/forms")).expect("report dir");
        fs::write(
            root.join("abapls.toml"),
            r#"
version = 1

[resolution]
dependency_mode = "local-first"
cache_dir = ".abapls/cache"
unknown_symbol_mode = "log"
"#,
        )
        .expect("manifest");
        fs::write(root.join("src/reports/ZREP/ZREP.abap"), "REPORT zrep.").expect("report");
        fs::write(
            root.join("src/reports/ZREP/forms/WHATEVER.abap"),
            "* include",
        )
        .expect("include");
        fs::write(
            root.join("src/reports/ZREP/abapls-unit.toml"),
            r#"
includes = { "ZREP_TOP" = "forms/WHATEVER.abap" }
"#,
        )
        .expect("unit sidecar");

        let root_uri = path_to_file_uri(&root);
        let loaded = load_workspace_documents(&root_uri, &HashMap::new());
        let manifest = loaded.manifest.as_ref().expect("effective manifest");
        let report_unit = manifest
            .units
            .iter()
            .find(|unit| unit.root_file == "src/reports/ZREP/ZREP.abap")
            .expect("report unit");
        assert!(report_unit.members.iter().any(|member| {
            member.file == "src/reports/ZREP/forms/WHATEVER.abap"
                && member.object_name == "ZREP_TOP"
        }));

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn single_file_unit_sidecar_adds_members_and_include_names() {
        let root = std::env::temp_dir().join("abap-lsp-single-file-unit-sidecar");
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(root.join("src/reports/forms")).expect("report helpers dir");
        fs::write(
            root.join("abapls.toml"),
            r#"
version = 1

[resolution]
dependency_mode = "local-first"
cache_dir = ".abapls/cache"
unknown_symbol_mode = "log"
"#,
        )
        .expect("manifest");
        fs::write(root.join("src/reports/ZREP.abap"), "REPORT zrep.").expect("report");
        fs::write(root.join("src/reports/forms/WHATEVER.abap"), "* include").expect("include");
        fs::write(
            root.join("src/reports/ZREP.abap.abapls-unit.toml"),
            r#"
members = ["forms/WHATEVER.abap"]
includes = { "ZREP_TOP" = "forms/WHATEVER.abap" }
"#,
        )
        .expect("unit sidecar");

        let root_uri = path_to_file_uri(&root);
        let loaded = load_workspace_documents(&root_uri, &HashMap::new());
        let manifest = loaded.manifest.as_ref().expect("effective manifest");
        let report_unit = manifest
            .units
            .iter()
            .find(|unit| unit.root_file == "src/reports/ZREP.abap")
            .expect("report unit");
        assert!(report_unit.members.iter().any(|member| {
            member.file == "src/reports/forms/WHATEVER.abap" && member.object_name == "ZREP_TOP"
        }));

        let _ = fs::remove_dir_all(&root);
    }
}
