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
pub const DEFAULT_REMOTE_REQUEST_PARALLELISM: usize = 4;
pub const DEFAULT_REMOTE_REQUESTS_PER_SECOND: usize = 8;
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
    #[serde(default = "default_remote_request_parallelism")]
    pub remote_request_parallelism: usize,
    #[serde(default = "default_remote_requests_per_second")]
    pub remote_requests_per_second: usize,
}

impl Default for ManifestResolution {
    fn default() -> Self {
        Self {
            dependency_mode: default_dependency_mode(),
            cache_dir: default_cache_dir(),
            unknown_symbol_mode: default_unknown_symbol_mode(),
            remote_request_parallelism: default_remote_request_parallelism(),
            remote_requests_per_second: default_remote_requests_per_second(),
        }
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

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct ManifestUnit {
    #[serde(default)]
    pub name: String,
    #[serde(default)]
    pub kind: String,
    #[serde(default)]
    pub root_file: String,
    #[serde(default)]
    pub adt_uri: String,
    #[serde(default, rename = "member")]
    pub members: Vec<ManifestUnitMember>,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct ManifestUnitMember {
    #[serde(default)]
    pub role: String,
    #[serde(default)]
    pub file: String,
    #[serde(default)]
    pub object_name: String,
    #[serde(default)]
    pub adt_uri: String,
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
    let mut documents = Vec::new();
    let mut seen = HashSet::new();

    if let Some(manifest) = manifest.as_ref() {
        collect_manifest_documents(
            manifest,
            &root_path,
            root_uri,
            overlays,
            &mut seen,
            &mut documents,
        );
    } else {
        collect_abap_sources(
            &root_path,
            root_uri,
            overlays,
            &mut seen,
            &mut documents,
            false,
        );
    }
    collect_dependency_cache_files(
        &root_path,
        &cache_dir,
        root_uri,
        overlays,
        &mut seen,
        &mut documents,
    );

    for (uri, overlay) in overlays {
        if uri_starts_with_workspace(uri, root_uri) && seen.insert(uri.clone()) {
            documents.push(WorkspaceDocument {
                uri: Arc::from(uri.as_str()),
                version: overlay.version,
                text: overlay.text.to_string(),
                is_dependency: false,
                object_name: None,
            });
        }
    }

    documents.sort_by(|left, right| left.uri.cmp(&right.uri));

    WorkspaceLoadResult {
        root_uri: Arc::from(root_uri),
        root_path,
        manifest_uri,
        manifest_len_bytes,
        manifest,
        manifest_error,
        documents,
    }
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
            let dependency_member_count = manifest
                .units
                .iter()
                .flat_map(|unit| unit.members.iter())
                .filter(|member| member.role == "dependency")
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
        "type" | "static" | "function" => is_standard_remote_type_like_name(trimmed),
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
        }
    }
}

fn collect_manifest_documents(
    manifest: &WorkspaceManifest,
    root_path: &Path,
    root_uri: &str,
    overlays: &HashMap<String, OpenDocumentOverlay>,
    seen: &mut HashSet<String>,
    documents: &mut Vec<WorkspaceDocument>,
) {
    for unit in &manifest.units {
        let mut unit_files = HashSet::new();

        for member in &unit.members {
            let relative = normalize_manifest_path(&member.file);
            if relative.is_empty() || !unit_files.insert(relative.clone()) {
                continue;
            }
            collect_manifest_document(
                unit,
                manifest_member_object_name(unit, Some(member)),
                member.role == "dependency",
                &relative,
                root_path,
                root_uri,
                overlays,
                seen,
                documents,
            );
        }

        let relative = normalize_manifest_path(&unit.root_file);
        if relative.is_empty() || !unit_files.insert(relative.clone()) {
            continue;
        }
        collect_manifest_document(
            unit,
            manifest_member_object_name(unit, None),
            manifest_unit_root_is_dependency(unit),
            &relative,
            root_path,
            root_uri,
            overlays,
            seen,
            documents,
        );
    }
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
}

fn manifest_unit_root_is_dependency(unit: &ManifestUnit) -> bool {
    if unit.members.is_empty() {
        return false;
    }
    let root_file = normalize_manifest_path(&unit.root_file);
    if let Some(member) = unit
        .members
        .iter()
        .find(|member| normalize_manifest_path(&member.file) == root_file)
    {
        return member.role == "dependency";
    }
    unit.members
        .iter()
        .all(|member| member.role == "dependency")
}

fn manifest_member_object_name(
    unit: &ManifestUnit,
    member: Option<&ManifestUnitMember>,
) -> Option<Arc<str>> {
    let explicit = member
        .map(|member| member.object_name.trim())
        .filter(|name| !name.is_empty())
        .or_else(|| (!unit.name.trim().is_empty()).then(|| unit.name.trim()))?;
    Some(Arc::from(explicit.to_ascii_lowercase()))
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

    manifest.units.iter().find_map(|unit| {
        for member in &unit.members {
            let member_file = normalize_manifest_path(&member.file);
            if !member_file.is_empty() && path_to_file_uri(&root_path.join(&member_file)) == uri {
                return Some((
                    member.role == "dependency",
                    manifest_member_object_name(unit, Some(member)),
                ));
            }
        }

        let root_file = normalize_manifest_path(&unit.root_file);
        if !root_file.is_empty() && path_to_file_uri(&root_path.join(root_file)) == uri {
            return Some((
                manifest_unit_root_is_dependency(unit),
                manifest_member_object_name(unit, None),
            ));
        }

        None
    })
}

fn collect_dependency_cache_files(
    root_path: &Path,
    cache_dir: &str,
    root_uri: &str,
    overlays: &HashMap<String, OpenDocumentOverlay>,
    seen: &mut HashSet<String>,
    documents: &mut Vec<WorkspaceDocument>,
) {
    let dependencies_root = root_path
        .join(normalize_manifest_path(cache_dir))
        .join("dependencies");
    if !dependencies_root.exists() {
        return;
    }

    let mut stack = vec![dependencies_root];
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
                stack.push(path);
                continue;
            }
            let Some(extension) = path.extension().and_then(|ext| ext.to_str()) else {
                continue;
            };
            if extension != "abap" && extension != "xml" {
                continue;
            }

            let uri = path_to_file_uri(&path);
            if !uri_starts_with_workspace(&uri, root_uri) || !seen.insert(uri.clone()) {
                continue;
            }

            let (version, source_text) = if let Some(overlay) = overlays.get(&uri) {
                (overlay.version, overlay.text.to_string())
            } else {
                match fs::read_to_string(&path) {
                    Ok(text) => (0, text),
                    Err(_) => continue,
                }
            };

            let kind_hint = path
                .parent()
                .and_then(|parent| parent.file_name())
                .and_then(|name| name.to_str())
                .unwrap_or_default()
                .to_ascii_lowercase();
            let object_name = path
                .file_stem()
                .and_then(|stem| stem.to_str())
                .map(percent_decode)
                .unwrap_or_else(String::new);

            let text = if extension == "xml" {
                ddic_xml_to_abap_source(&object_name, &kind_hint, &source_text)
                    .unwrap_or(source_text)
            } else {
                source_text
            };
            let is_dependency = !overlays.contains_key(&uri);

            documents.push(WorkspaceDocument {
                uri: Arc::from(uri),
                version,
                text,
                is_dependency,
                object_name: Some(Arc::from(object_name.to_ascii_lowercase())),
            });
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
    manifest.resolution.remote_request_parallelism =
        manifest.resolution.remote_request_parallelism.max(1);
    manifest.resolution.remote_requests_per_second =
        manifest.resolution.remote_requests_per_second.max(1);
    for unit in &mut manifest.units {
        unit.kind = unit.kind.trim().to_ascii_lowercase();
        unit.root_file = normalize_manifest_path(&unit.root_file);
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

fn default_remote_request_parallelism() -> usize {
    DEFAULT_REMOTE_REQUEST_PARALLELISM
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

    use super::{
        UNKNOWN_SYMBOL_MODE_REMOTE, WORKSPACE_PERFORMANCE_MODE_AUTO,
        WORKSPACE_PERFORMANCE_MODE_EDITOR_FIRST, WorkspaceManifest, WorkspacePerformanceMode,
        ddic_xml_to_abap_source, is_remote_lookup_candidate, is_remote_lookup_name,
        load_workspace_documents, manifest_declares_uri, manifest_supports_remote_resolution,
        path_to_file_uri, resolve_workspace_performance_mode,
    };

    #[test]
    fn parses_manifest_defaults() {
        let manifest: WorkspaceManifest = toml::from_str("version = 1\n").expect("manifest");
        assert_eq!(
            manifest.resolution.unknown_symbol_mode,
            UNKNOWN_SYMBOL_MODE_REMOTE
        );
        assert_eq!(manifest.resolution.remote_request_parallelism, 4);
        assert_eq!(manifest.performance.mode, WORKSPACE_PERFORMANCE_MODE_AUTO);
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

[[unit.member]]
role = "main"
file = "src/ZCL_MAIN.abap"
object_name = "ZCL_MAIN"

[[unit]]
name = "ZCL_DEP"
kind = "global-class"
root_file = ".abapls/cache/dependencies/global-class/ZCL_DEP.abap"

[[unit.member]]
role = "dependency"
file = ".abapls/cache/dependencies/global-class/ZCL_DEP.abap"
object_name = "ZCL_DEP"
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

[[unit.member]]
role = "main"
file = "src/ZCL_MANAGED.abap"
object_name = "ZCL_MANAGED"
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

[[unit.member]]
role = "main"
file = "src/ZCL_MAIN.abap"
object_name = "ZCL_MAIN"

[[unit.member]]
role = "root"
file = "src/ZTOP.abap"
object_name = "ZTOP"
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
}
