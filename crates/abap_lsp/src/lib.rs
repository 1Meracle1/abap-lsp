#[cfg(test)]
mod perf_tests;
pub(crate) mod sem_tokens;

use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::path::{Path, PathBuf};
use std::str::FromStr;
#[cfg(test)]
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, OnceLock};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use abap_cache::{
    CallableCompletionKind, DependencyDiagnosticsMode, DocumentInput, DocumentStore,
    LintDiagnostic, LintLevel, LintPolicy, LocalExportConfig, LocalExportResolveProfile,
    LocalExportResolver, ManifestDiagnostic, SnapshotBuildPlan, WorkspaceDocument,
    WorkspaceManifest, analysis_text_for_document, ddic_xml_to_abap_source, file_uri_to_path,
    function_module_completion_items_from_source, is_remote_lookup_candidate,
    is_remote_lookup_candidate_after_local_resolution, lint_docs_anchor,
    lint_id_for_diagnostic_kind, load_effective_manifest_from_workspace_result,
    load_manifest_diagnostics_from_workspace, load_workspace_documents_with_progress,
    local_export_candidate_kind_for_reference, local_export_config_for_source,
    manifest_document_metadata, manifest_supports_remote_resolution, path_to_file_uri, registry,
    resolve_local_export_dependency_document, resolve_local_export_dependency_document_profiled,
    resolve_local_export_dependency_documents, resolve_local_export_dependency_documents_profiled,
    resolve_local_export_function_module_documents_by_prefix, resolve_workspace_performance_mode,
    uri_starts_with_workspace,
};
use abap_dependency_store::{
    CandidateCacheStatus, DependencyProfile, DependencyStore, DependencyStoreReader,
    StoredArtifactInput, StoredArtifactRecord, StoredSymbolInput, resolve_dependency_store_path,
};
use abap_parser::{parse, parse_error_is_include_fragment_boundary};
use abap_symbols::{
    DiagnosticKind, NamedArgumentTarget, Namespace, ReferenceKind, Resolution, SqlResolution,
    SymbolKind, UnitId, analyze_unit,
};
use lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionProviderCapability, CodeDescription,
    CompletionItem, CompletionItemKind, CompletionOptions, Diagnostic, DiagnosticSeverity,
    DiagnosticTag, Documentation, FoldingRange, FoldingRangeProviderCapability, Hover,
    HoverContents, HoverProviderCapability, InitializeResult, InlayHint, InlayHintKind,
    InlayHintOptions, InlayHintServerCapabilities, InsertTextFormat, Location, MarkupContent,
    MarkupKind, NumberOrString, OneOf, Position, PrepareRenameResponse, PublishDiagnosticsParams,
    Range, RenameOptions, SemanticTokens, SemanticTokensFullOptions, SemanticTokensOptions,
    SemanticTokensServerCapabilities, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextEdit, Uri, WorkspaceEdit,
};
use serde::{Deserialize, Serialize};
use tracing::debug;

pub use abap_cache::{AnalysisSnapshot, OpenDocumentOverlay, WorkspacePerformanceMode};
pub use lsp_types::{
    CodeActionParams, CodeActionResponse, CompletionParams, CompletionResponse,
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, FoldingRangeParams,
    GotoDefinitionParams, GotoDefinitionResponse, HoverParams, InlayHintParams, ReferenceParams,
    RenameParams, SemanticTokensParams, TextDocumentPositionParams,
};
pub use sem_tokens::build_semantic_tokens;
pub use serde;

pub const RESOLVE_REMOTE_DEPENDENCIES: &str = "abapls/resolveRemoteDependencies";
pub const REMOTE_DEPENDENCIES_UPDATED: &str = "abapls/remoteDependenciesUpdated";
pub const WORKSPACE_MANIFEST_UPDATED: &str = "abapls/workspaceManifestUpdated";
pub const DEPENDENCY_CACHE_REFRESH_REQUESTED: &str = "abapls/dependencyCacheRefreshRequested";
pub const WORKSPACE_ANALYSIS_STATUS: &str = "abapls/workspaceAnalysisStatus";
pub const STORE_REMOTE_DEPENDENCY_ARTIFACTS: &str = "abapls/storeRemoteDependencyArtifacts";
pub const READ_DEPENDENCY_DOCUMENT: &str = "abapls/readDependencyDocument";
pub const SEARCH_REPOSITORY_OBJECTS: &str = "abapls/searchRepositoryObjects";
pub const MATERIALIZE_EDITABLE_ADT_OBJECT: &str = "abapls/materializeEditableAdtObject";
pub const SAP_ATC_RESULTS_UPDATED: &str = "abapls/sapAtcResultsUpdated";
const LOCAL_EXPORT_FUNCTION_MODULE_COMPLETION_LIMIT: usize = 64;
const DIAGNOSTIC_CODE_MISSING_METHOD_IMPLEMENTATION: &str = "missing-method-implementation";
const DEPENDENCY_DOCUMENT_SCHEME: &str = "abapls-cache";
const LINT_DIAGNOSTIC_SOURCE: &str = "abap-lsp-lints";
const LINT_REFERENCE_DOCS_URL: &str =
    "https://github.com/1Meracle1/abap-lsp/blob/main/docs/reference/lints.md";
const SAP_ATC_DIAGNOSTIC_SOURCE: &str = "sap-atc";
const LOCAL_EXPORT_DEPENDENCY_CANDIDATE_CACHE_MAX_ENTRIES: usize = 16_384;
const DEPENDENCY_DOCUMENT_SNAPSHOT_CACHE_MAX_ENTRIES: usize = 512;

#[derive(Debug, Clone)]
pub struct ServerState {
    pub cache: DocumentStore,
    pub workspaces: HashMap<String, WorkspaceState>,
    pub workspace_roots_desc: Vec<String>,
    pub document_workspace_index: HashMap<String, String>,
    pub client_capabilities: ClientCapabilitiesState,
    pub dependency_store_path_override: Option<PathBuf>,
    pub shutdown_requested: bool,
}

#[derive(Debug, Clone, Default)]
pub struct ClientCapabilitiesState {
    pub completion_snippet_support: bool,
}

#[derive(Debug, Clone)]
pub struct WorkspaceState {
    pub root_uri: String,
    pub cache: DocumentStore,
    pub preview_snapshots: HashMap<String, Arc<AnalysisSnapshot>>,
    pub dependency_parent_uris: HashMap<String, HashSet<String>>,
    pub dependency_batch_candidates: HashMap<String, CachedDependencyBatchCandidates>,
    pub local_export_chain_candidates: HashMap<String, Vec<RemoteDependencyCandidate>>,
    pub local_export_chain_refresh_candidates: HashMap<String, Vec<RemoteDependencyCandidate>>,
    pub dependency_store_hydration_metrics: Option<DependencyStoreHydrationMetrics>,
    pub local_export_resolver: Arc<Mutex<LocalExportResolver>>,
    pub manifest: Option<WorkspaceManifest>,
    pub manifest_uri: String,
    pub manifest_error: Option<String>,
    pub manifest_diagnostics: Vec<ManifestDiagnostic>,
    pub open_documents: HashMap<String, OpenDocumentOverlay>,
    pub pending_open_dependency_requests: HashSet<String>,
    pub remote_resolution_seen: HashSet<String>,
    pub remote_lookup_failures: HashSet<String>,
    pub remote_resolution_in_flight: bool,
    pub performance_mode: WorkspacePerformanceMode,
    pub dependency_profile: Option<DependencyProfile>,
    pub dependency_store_path_override: Option<PathBuf>,
    pub sap_atc_results: HashMap<SapAtcCacheKey, SapAtcCachedResult>,
}

impl Default for ServerState {
    fn default() -> Self {
        Self {
            cache: DocumentStore::default(),
            workspaces: HashMap::new(),
            workspace_roots_desc: Vec::new(),
            document_workspace_index: HashMap::new(),
            client_capabilities: ClientCapabilitiesState::default(),
            dependency_store_path_override: default_dependency_store_path_override(),
            shutdown_requested: false,
        }
    }
}

#[cfg(test)]
fn default_dependency_store_path_override() -> Option<PathBuf> {
    static NEXT_ID: AtomicU64 = AtomicU64::new(1);
    let mut path = std::env::temp_dir();
    path.push(format!(
        "abap_lsp_test_dependency_store_{}_{}.sqlite3",
        std::process::id(),
        NEXT_ID.fetch_add(1, Ordering::Relaxed)
    ));
    Some(path)
}

#[cfg(not(test))]
fn default_dependency_store_path_override() -> Option<PathBuf> {
    None
}

#[derive(Debug, Clone)]
pub struct ServerConfig {
    pub name: &'static str,
    pub version: &'static str,
}

#[derive(Debug, Clone)]
pub struct CachedDependencyBatchCandidates {
    pub text_len: usize,
    pub text_hash: u64,
    pub object_name: Option<Arc<str>>,
    pub candidates: Vec<RemoteDependencyCandidate>,
}

#[derive(Debug, Clone)]
struct FoldingBlock {
    kind: FoldingBlockKind,
    start_line: u32,
    end_keyword: &'static str,
    current_arm_start_line: Option<u32>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FoldingBlockKind {
    Simple,
    If,
    Case,
}

#[derive(Debug, Clone, Default)]
pub struct DependencyStoreHydrationMetrics {
    pub elapsed: Duration,
    pub supported: bool,
    pub profile_present: bool,
    pub reader_available: bool,
    pub iterations: usize,
    pub cache_uri_scans: usize,
    pub source_snapshots_examined: usize,
    pub dependency_snapshots_skipped: usize,
    pub candidate_count: usize,
    pub unique_candidate_queries: usize,
    pub artifact_hits: usize,
    pub existing_or_duplicate_inputs: usize,
    pub hydrated_input_count: usize,
    pub published_batch_count: usize,
    pub candidate_collection_micros: u128,
    pub store_lookup_micros: u128,
    pub publish_micros: u128,
}

#[derive(Debug, Clone)]
struct CachedLocalExportDependencyCandidates {
    version: i32,
    text_len: usize,
    text_hash: u64,
    is_dependency: bool,
    object_name: Option<Arc<str>>,
    candidates: Vec<RemoteDependencyCandidate>,
}

struct CachedDependencyDocumentSnapshot {
    text_len: usize,
    text_hash: u64,
    snapshot: Arc<AnalysisSnapshot>,
}

fn local_export_dependency_candidate_cache()
-> &'static Mutex<HashMap<String, CachedLocalExportDependencyCandidates>> {
    static CACHE: OnceLock<Mutex<HashMap<String, CachedLocalExportDependencyCandidates>>> =
        OnceLock::new();
    CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

fn dependency_document_snapshot_cache()
-> &'static Mutex<HashMap<String, CachedDependencyDocumentSnapshot>> {
    static CACHE: OnceLock<Mutex<HashMap<String, CachedDependencyDocumentSnapshot>>> =
        OnceLock::new();
    CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

#[cfg(test)]
fn clear_local_export_dependency_candidate_cache_for_tests() {
    local_export_dependency_candidate_cache()
        .lock()
        .unwrap_or_else(|error| error.into_inner())
        .clear();
}

fn clear_workspace_dependency_candidate_caches(workspace: &mut WorkspaceState) {
    workspace.dependency_batch_candidates.clear();
    workspace.local_export_chain_candidates.clear();
    workspace.local_export_chain_refresh_candidates.clear();
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            name: "abap-lsp-rs",
            version: env!("CARGO_PKG_VERSION"),
        }
    }
}

impl WorkspaceState {
    pub fn new(root_uri: impl Into<String>) -> Self {
        Self {
            root_uri: root_uri.into(),
            cache: DocumentStore::default(),
            preview_snapshots: HashMap::new(),
            dependency_parent_uris: HashMap::new(),
            dependency_batch_candidates: HashMap::new(),
            local_export_chain_candidates: HashMap::new(),
            local_export_chain_refresh_candidates: HashMap::new(),
            dependency_store_hydration_metrics: None,
            local_export_resolver: Arc::new(Mutex::new(LocalExportResolver::default())),
            manifest: None,
            manifest_uri: String::new(),
            manifest_error: None,
            manifest_diagnostics: Vec::new(),
            open_documents: HashMap::new(),
            pending_open_dependency_requests: HashSet::new(),
            remote_resolution_seen: HashSet::new(),
            remote_lookup_failures: HashSet::new(),
            remote_resolution_in_flight: false,
            performance_mode: WorkspacePerformanceMode::FullWorkspace,
            dependency_profile: None,
            dependency_store_path_override: None,
            sap_atc_results: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct DependencyCacheInitializationOptions {
    #[serde(rename = "dependencyCachePath", default)]
    pub dependency_cache_path: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DependencyArtifactPayload {
    #[serde(rename = "packageName", default)]
    pub package_name: String,
    #[serde(rename = "objectKind", default)]
    pub object_kind: String,
    #[serde(rename = "objectName", default)]
    pub object_name: String,
    #[serde(rename = "objectUri", default)]
    pub object_uri: String,
    #[serde(rename = "objectType", default)]
    pub object_type: String,
    #[serde(default)]
    pub description: String,
    #[serde(rename = "fileExtension", default)]
    pub file_extension: String,
    #[serde(rename = "sourceText", default)]
    pub source_text: String,
    #[serde(rename = "fetchedAt", default)]
    pub fetched_at: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct StoreRemoteDependencyArtifactsParams {
    #[serde(rename = "workspaceUri")]
    pub workspace_uri: String,
    #[serde(rename = "connectionKey", default)]
    pub connection_key: Option<String>,
    #[serde(default)]
    pub artifacts: Vec<DependencyArtifactPayload>,
    #[serde(default)]
    pub negative: Vec<RemoteDependencyCandidate>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ReadDependencyDocumentParams {
    pub uri: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ReadDependencyDocumentResult {
    #[serde(rename = "sourceText")]
    pub source_text: String,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AdtObjectRefPayload {
    pub uri: String,
    #[serde(rename = "type", alias = "objectType")]
    pub object_type: String,
    pub name: String,
    #[serde(default, alias = "package_name")]
    pub package_name: String,
    #[serde(default)]
    pub description: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SearchRepositoryObjectsParams {
    pub workspace_uri: String,
    pub query: String,
    #[serde(default)]
    pub max_results: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SearchRepositoryObjectsResult {
    pub objects: Vec<AdtObjectRefPayload>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MaterializeEditableAdtObjectParams {
    pub workspace_uri: String,
    pub object_ref: AdtObjectRefPayload,
    pub target: EditableAdtObjectTarget,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum EditableAdtObjectTarget {
    File {
        #[serde(rename = "filePath")]
        file_path: String,
    },
    Directory {
        #[serde(rename = "directoryPath")]
        directory_path: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MaterializeEditableAdtObjectResult {
    pub opened_file_uri: String,
    #[serde(default)]
    pub created_file_uris: Vec<String>,
    #[serde(default)]
    pub message: String,
}

fn workspace_lint_policy(workspace: &WorkspaceState) -> LintPolicy {
    LintPolicy::from_config_opt(
        workspace
            .manifest
            .as_ref()
            .and_then(|manifest| manifest.lints.as_ref()),
    )
}

fn sync_workspace_lint_policy(workspace: &WorkspaceState) {
    workspace
        .cache
        .set_lint_policy(workspace_lint_policy(workspace));
}

fn prime_workspace_manifest_state(workspace: &mut WorkspaceState) {
    let Some(root_path) = file_uri_to_path(&workspace.root_uri) else {
        workspace.dependency_profile = None;
        workspace.performance_mode = WorkspacePerformanceMode::FullWorkspace;
        workspace.manifest_uri.clear();
        workspace.manifest = None;
        workspace.manifest_error = None;
        workspace.manifest_diagnostics.clear();
        sync_workspace_lint_policy(workspace);
        return;
    };
    let manifest_uri = path_to_file_uri(&root_path.join("abapls.toml"));
    let manifest_len_bytes = std::fs::metadata(root_path.join("abapls.toml"))
        .ok()
        .map(|metadata| metadata.len() as usize)
        .unwrap_or(0);
    match load_effective_manifest_from_workspace_result(
        &root_path,
        &workspace.root_uri,
        &workspace.open_documents,
    ) {
        Ok(manifest) => {
            workspace.performance_mode =
                resolve_workspace_performance_mode(manifest.as_ref(), manifest_len_bytes);
            workspace.dependency_profile = manifest
                .as_ref()
                .and_then(|manifest| manifest.dependency_store.clone());
            workspace.manifest_uri = manifest
                .as_ref()
                .map(|_| manifest_uri.clone())
                .unwrap_or_default();
            workspace.manifest_diagnostics =
                load_manifest_diagnostics_from_workspace(&root_path, manifest.as_ref());
            workspace.manifest = manifest;
            workspace.manifest_error = None;
        }
        Err(error) => {
            workspace.dependency_profile = None;
            workspace.performance_mode = WorkspacePerformanceMode::FullWorkspace;
            workspace.manifest_uri = manifest_uri;
            workspace.manifest = None;
            workspace.manifest_error = Some(error);
            workspace.manifest_diagnostics.clear();
        }
    }
    sync_workspace_lint_policy(workspace);
}

impl ServerState {
    pub fn register_workspace_folder(&mut self, root_uri: impl Into<String>) {
        let root_uri = normalize_lsp_uri(&root_uri.into());
        let dependency_store_path_override = self.dependency_store_path_override.clone();
        self.workspaces.entry(root_uri.clone()).or_insert_with(|| {
            let mut workspace = WorkspaceState::new(root_uri.clone());
            workspace.dependency_store_path_override = dependency_store_path_override;
            prime_workspace_manifest_state(&mut workspace);
            workspace
        });
        self.index_workspace_root(&root_uri);
    }

    pub fn refresh_workspace_routing(&mut self) {
        self.workspace_roots_desc = self.workspaces.keys().cloned().collect();
        self.workspace_roots_desc
            .sort_by(|left, right| right.len().cmp(&left.len()).then_with(|| left.cmp(right)));
    }

    pub fn index_workspace_members(&mut self, workspace_uri: &str) {
        let normalized_workspace_uri = normalize_lsp_uri(workspace_uri);
        let Some(workspace) = self.workspaces.get(&normalized_workspace_uri) else {
            return;
        };
        let workspace_root_uri = workspace.root_uri.clone();
        let cached_uris = workspace.cache.uris();
        let preview_uris: Vec<_> = workspace.preview_snapshots.keys().cloned().collect();
        let open_uris: Vec<_> = workspace.open_documents.keys().cloned().collect();
        let _ = workspace;

        self.index_workspace_root(&workspace_root_uri);
        for uri in cached_uris {
            self.index_workspace_uri(&workspace_root_uri, uri.as_ref());
        }
        for uri in preview_uris {
            self.index_workspace_uri(&workspace_root_uri, &uri);
        }
        for uri in open_uris {
            self.index_workspace_uri(&workspace_root_uri, &uri);
        }
    }

    pub fn workspace_for_uri(&self, uri: &str) -> Option<&WorkspaceState> {
        self.workspace_key_for_uri(uri)
            .and_then(|workspace_uri| self.workspaces.get(workspace_uri))
    }

    pub fn workspace_for_uri_mut(&mut self, uri: &str) -> Option<&mut WorkspaceState> {
        let key = self.workspace_key_for_uri(uri).map(str::to_owned)?;
        self.index_workspace_uri(&key, uri);
        self.workspaces.get_mut(&key)
    }

    fn index_workspace_root(&mut self, workspace_uri: &str) {
        if self
            .workspace_roots_desc
            .iter()
            .any(|root_uri| root_uri == workspace_uri)
        {
            return;
        }
        self.workspace_roots_desc.push(workspace_uri.to_owned());
        self.workspace_roots_desc
            .sort_by(|left, right| right.len().cmp(&left.len()).then_with(|| left.cmp(right)));
    }

    fn index_workspace_uri(&mut self, workspace_uri: &str, uri: &str) {
        self.index_workspace_root(workspace_uri);
        self.index_workspace_uri_key(workspace_uri, uri);
        let normalized_uri = normalize_lsp_uri(uri);
        if normalized_uri != uri {
            self.index_workspace_uri_key(workspace_uri, &normalized_uri);
        }
    }

    fn index_workspace_uri_key(&mut self, workspace_uri: &str, uri: &str) {
        match self.document_workspace_index.get(uri) {
            Some(existing_workspace_uri) if existing_workspace_uri.len() > workspace_uri.len() => {}
            _ => {
                self.document_workspace_index
                    .insert(uri.to_owned(), workspace_uri.to_owned());
            }
        }
    }

    fn workspace_key_for_uri(&self, uri: &str) -> Option<&str> {
        if let Some(workspace_uri) = self.document_workspace_index.get(uri)
            && self.workspaces.contains_key(workspace_uri)
            && (uri_belongs_to_workspace(uri, workspace_uri)
                || self
                    .workspaces
                    .get(workspace_uri)
                    .is_some_and(|workspace| workspace_has_document_uri(workspace, uri)))
        {
            return Some(workspace_uri.as_str());
        }

        for workspace_uri in &self.workspace_roots_desc {
            if uri_belongs_to_workspace(uri, workspace_uri)
                && self.workspaces.contains_key(workspace_uri)
            {
                return Some(workspace_uri.as_str());
            }
        }

        self.workspaces
            .values()
            .filter(|workspace| {
                uri_belongs_to_workspace(uri, &workspace.root_uri)
                    || workspace_has_document_uri(workspace, uri)
            })
            .max_by_key(|workspace| workspace.root_uri.len())
            .map(|workspace| workspace.root_uri.as_str())
    }
}

fn workspace_has_document_uri(workspace: &WorkspaceState, uri: &str) -> bool {
    if workspace.preview_snapshots.contains_key(uri)
        || workspace.open_documents.contains_key(uri)
        || workspace.cache.get(uri).is_some()
    {
        return true;
    }

    let normalized_uri = normalize_lsp_uri(uri);
    if normalized_uri == uri {
        return false;
    }

    workspace
        .preview_snapshots
        .keys()
        .any(|candidate| normalize_lsp_uri(candidate) == normalized_uri)
        || workspace
            .open_documents
            .keys()
            .any(|candidate| normalize_lsp_uri(candidate) == normalized_uri)
        || workspace
            .cache
            .uris()
            .iter()
            .any(|candidate| normalize_lsp_uri(candidate.as_ref()) == normalized_uri)
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RemoteDependencyCandidate {
    pub name: String,
    pub kind: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RemoteDependencyResolveParams {
    #[serde(rename = "workspaceUri")]
    pub workspace_uri: String,
    #[serde(rename = "sourceUri")]
    pub source_uri: String,
    #[serde(rename = "sourceUris", default)]
    pub source_uris: Vec<String>,
    #[serde(rename = "retryNegativeCandidates", default)]
    pub retry_negative_candidates: bool,
    #[serde(rename = "remoteRequestParallelism", default)]
    pub remote_request_parallelism: Option<usize>,
    #[serde(rename = "remoteRequestsPerSecond", default)]
    pub remote_requests_per_second: Option<usize>,
    #[serde(rename = "sourceCandidates", default)]
    pub source_candidates: HashMap<String, Vec<RemoteDependencyCandidate>>,
    pub candidates: Vec<RemoteDependencyCandidate>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RemoteDependenciesUpdatedParams {
    #[serde(rename = "workspaceUri")]
    pub workspace_uri: String,
    #[serde(rename = "sourceUri")]
    pub source_uri: String,
    #[serde(rename = "sourceUris", default)]
    pub source_uris: Vec<String>,
    pub fetched: Vec<String>,
    #[serde(default)]
    pub failed: Vec<RemoteDependencyCandidate>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SapAtcResultsUpdatedParams {
    #[serde(rename = "workspaceUri")]
    pub workspace_uri: String,
    #[serde(rename = "sourceUri")]
    pub source_uri: String,
    #[serde(rename = "documentVersion")]
    pub document_version: i32,
    #[serde(rename = "objectName", default)]
    pub object_name: String,
    #[serde(rename = "checkVariant", default = "default_sap_atc_check_variant")]
    pub check_variant: String,
    #[serde(default)]
    pub configuration: Option<String>,
    #[serde(rename = "fetchedAt", default)]
    pub fetched_at: String,
    #[serde(default)]
    pub findings: Vec<SapAtcFindingPayload>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SapAtcFindingPayload {
    #[serde(rename = "sapCheckId", alias = "checkId", default)]
    pub sap_check_id: String,
    #[serde(rename = "sapMessageId", alias = "messageId", default)]
    pub sap_message_id: String,
    #[serde(default)]
    pub message: String,
    #[serde(default = "default_sap_atc_severity")]
    pub severity: String,
    #[serde(rename = "mappedLocalLintId", default)]
    pub mapped_local_lint_id: Option<String>,
    #[serde(rename = "exemptionState", default)]
    pub exemption_state: Option<String>,
    #[serde(rename = "suppressionState", default)]
    pub suppression_state: Option<String>,
    #[serde(default)]
    pub location: SapAtcSourceLocationPayload,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct SapAtcSourceLocationPayload {
    #[serde(default)]
    pub uri: String,
    #[serde(rename = "objectName", default)]
    pub object_name: String,
    #[serde(rename = "includeName", default)]
    pub include_name: String,
    #[serde(rename = "startLine", default)]
    pub start_line: Option<u32>,
    #[serde(rename = "startColumn", default)]
    pub start_column: Option<u32>,
    #[serde(rename = "endLine", default)]
    pub end_line: Option<u32>,
    #[serde(rename = "endColumn", default)]
    pub end_column: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SapAtcCachedResult {
    pub source_uri: String,
    pub document_version: i32,
    pub object_name: String,
    pub check_variant: String,
    pub configuration: Option<String>,
    pub fetched_at: String,
    pub findings: Vec<SapAtcFindingPayload>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SapAtcCacheKey {
    pub source_uri: String,
    pub document_version: i32,
    pub object_name: String,
    pub check_variant: String,
    pub configuration: Option<String>,
}

fn default_sap_atc_check_variant() -> String {
    "DEFAULT".to_string()
}

fn default_sap_atc_severity() -> String {
    "warning".to_string()
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct WorkspaceManifestUpdatedParams {
    #[serde(rename = "workspaceUri")]
    pub workspace_uri: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum WorkspaceAnalysisPhase {
    Started,
    Progress,
    Finished,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct WorkspaceAnalysisStatusParams {
    #[serde(rename = "workspaceUri")]
    pub workspace_uri: String,
    pub phase: WorkspaceAnalysisPhase,
    pub trigger: String,
    #[serde(rename = "processedDocumentCount", default)]
    pub processed_document_count: usize,
    #[serde(rename = "totalDocumentCount", default)]
    pub total_document_count: usize,
    #[serde(rename = "analyzedDocumentCount", default)]
    pub analyzed_document_count: usize,
    #[serde(rename = "remoteResolutionInFlight", default)]
    pub remote_resolution_in_flight: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum RemoteDependencyBatchPhase {
    PriorityLocal,
    Dependency,
    OtherLocal,
}

/// Normalizes URIs so `DocumentStore` lookups stay stable (e.g. Windows `file:///C:/` vs `file:///c:/`).
pub fn normalize_lsp_uri(raw: &str) -> String {
    if raw.to_ascii_lowercase().starts_with("abapls-cache:") {
        return normalize_dependency_document_uri(raw).unwrap_or_else(|| raw.to_owned());
    }

    const PREFIX: &str = "file:///";
    let lower = raw.to_ascii_lowercase();
    if !lower.starts_with(PREFIX) {
        return raw.to_owned();
    }
    let path = &raw[PREFIX.len()..];
    let bytes = path.as_bytes();
    if bytes.len() >= 2 && bytes[0].is_ascii_alphabetic() && bytes[1] == b':' {
        let mut out = String::with_capacity(raw.len());
        out.push_str(PREFIX);
        out.push((bytes[0] as char).to_ascii_lowercase());
        out.push_str(&path[1..]);
        return out;
    }
    if bytes.len() >= 4 && bytes[0].is_ascii_alphabetic() && path[1..4].eq_ignore_ascii_case("%3a")
    {
        let mut out = String::with_capacity(raw.len());
        out.push_str(PREFIX);
        out.push((bytes[0] as char).to_ascii_lowercase());
        out.push(':');
        out.push_str(&path[4..]);
        return out;
    }
    raw.to_owned()
}

fn normalize_dependency_document_uri(raw: &str) -> Option<String> {
    let rest = raw
        .get(DEPENDENCY_DOCUMENT_SCHEME.len()..)?
        .strip_prefix(':')?;
    let (_, query) = rest.trim_start_matches('/').split_once('?')?;
    let workspace = dependency_document_query_value(query, "workspace")?;
    let artifact = dependency_document_query_value(query, "artifact")?
        .parse::<i64>()
        .ok()?;
    let name = dependency_document_query_value(query, "name")?;
    let kind = dependency_document_query_value(query, "kind");
    Some(dependency_document_uri_with_kind(
        &workspace,
        artifact,
        &name,
        kind.as_deref(),
    ))
}

fn dependency_document_query_value(query: &str, key: &str) -> Option<String> {
    dependency_document_query_param_from_pairs(query, key).or_else(|| {
        decode_uri_component(query)
            .and_then(|decoded| dependency_document_query_param_from_pairs(&decoded, key))
    })
}

fn uri_belongs_to_workspace(uri: &str, workspace_uri: &str) -> bool {
    if uri_starts_with_workspace(uri, workspace_uri) {
        return true;
    }
    dependency_document_workspace_uri(uri).is_some_and(|document_workspace_uri| {
        document_workspace_uri == normalize_lsp_uri(workspace_uri)
    })
}

fn is_dependency_document_uri(uri: &str) -> bool {
    uri.to_ascii_lowercase()
        .starts_with(&format!("{DEPENDENCY_DOCUMENT_SCHEME}:"))
}

fn dependency_document_uri(workspace_uri: &str, artifact_id: i64, object_name: &str) -> String {
    dependency_document_uri_with_kind(workspace_uri, artifact_id, object_name, None)
}

fn dependency_document_uri_with_kind(
    workspace_uri: &str,
    artifact_id: i64,
    object_name: &str,
    object_kind: Option<&str>,
) -> String {
    let display_name = dependency_document_display_name(object_name, artifact_id);
    let kind = object_kind
        .map(str::trim)
        .filter(|kind| !kind.is_empty())
        .map(|kind| format!("&kind={}", encode_uri_component(kind)))
        .unwrap_or_default();
    format!(
        "{DEPENDENCY_DOCUMENT_SCHEME}:///{display_name}.abap?workspace={workspace}&artifact={artifact_id}&name={name}{kind}",
        workspace = encode_uri_component(&normalize_lsp_uri(workspace_uri)),
        name = encode_uri_component(object_name),
    )
}

fn dependency_document_display_name(object_name: &str, artifact_id: i64) -> String {
    let mut out = String::with_capacity(object_name.len());
    let mut previous_was_separator = false;
    for ch in object_name.trim().chars() {
        if ch.is_ascii_alphanumeric() || matches!(ch, '-' | '_' | '.' | '~') {
            out.push(ch.to_ascii_lowercase());
            previous_was_separator = false;
        } else if !previous_was_separator {
            out.push('_');
            previous_was_separator = true;
        }
    }
    let display = out.trim_matches('_');
    if display.is_empty() {
        format!("artifact-{artifact_id}")
    } else {
        display.to_string()
    }
}

fn dependency_document_workspace_uri(uri: &str) -> Option<String> {
    dependency_document_query_param(uri, "workspace")
        .map(|workspace_uri| normalize_lsp_uri(&workspace_uri))
}

fn dependency_document_artifact_id(uri: &str) -> Option<i64> {
    dependency_document_query_param(uri, "artifact")?
        .parse::<i64>()
        .ok()
}

fn dependency_document_query_param(uri: &str, key: &str) -> Option<String> {
    let query = uri.split_once('?')?.1;
    dependency_document_query_param_from_pairs(query, key).or_else(|| {
        decode_uri_component(query)
            .and_then(|decoded| dependency_document_query_param_from_pairs(&decoded, key))
    })
}

fn dependency_document_query_param_from_pairs(query: &str, key: &str) -> Option<String> {
    for pair in query.split('&') {
        let (candidate_key, value) = pair.split_once('=').unwrap_or((pair, ""));
        if candidate_key == key {
            return decode_uri_component(value).or_else(|| Some(value.to_string()));
        }
    }
    None
}

fn encode_uri_component(value: &str) -> String {
    let mut out = String::with_capacity(value.len());
    for byte in value.bytes() {
        if byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_' | b'.' | b'~') {
            out.push(byte as char);
        } else {
            out.push('%');
            out.push(hex_digit(byte >> 4));
            out.push(hex_digit(byte & 0x0f));
        }
    }
    out
}

fn decode_uri_component(value: &str) -> Option<String> {
    let mut out = Vec::with_capacity(value.len());
    let bytes = value.as_bytes();
    let mut idx = 0usize;
    while idx < bytes.len() {
        match bytes[idx] {
            b'%' if idx + 2 < bytes.len() => {
                let high = from_hex_digit(bytes[idx + 1])?;
                let low = from_hex_digit(bytes[idx + 2])?;
                out.push((high << 4) | low);
                idx += 3;
            }
            b'+' => {
                out.push(b' ');
                idx += 1;
            }
            byte => {
                out.push(byte);
                idx += 1;
            }
        }
    }
    String::from_utf8(out).ok()
}

fn from_hex_digit(value: u8) -> Option<u8> {
    match value {
        b'0'..=b'9' => Some(value - b'0'),
        b'a'..=b'f' => Some(value - b'a' + 10),
        b'A'..=b'F' => Some(value - b'A' + 10),
        _ => None,
    }
}

fn workspace_dependency_store(workspace: &WorkspaceState) -> Option<DependencyStore> {
    DependencyStore::from_override_path(workspace.dependency_store_path_override.as_deref()).ok()
}

fn workspace_dependency_store_path(workspace: &WorkspaceState) -> Option<PathBuf> {
    resolve_dependency_store_path(workspace.dependency_store_path_override.as_deref())
}

fn workspace_dependency_profile(workspace: &WorkspaceState) -> Option<DependencyProfile> {
    workspace.dependency_profile.clone()
}

fn workspace_supports_dependency_store_resolution(workspace: &WorkspaceState) -> bool {
    manifest_supports_remote_resolution(workspace.manifest.as_ref())
        && workspace.dependency_profile.is_some()
        && workspace_dependency_store(workspace).is_some()
}

fn workspace_dependency_connection_key(workspace: &WorkspaceState) -> String {
    workspace
        .manifest
        .as_ref()
        .map(|manifest| manifest.connection.trim().to_ascii_lowercase())
        .filter(|connection| !connection.is_empty())
        .unwrap_or_else(|| "default".to_string())
}

fn dependency_artifact_for_uri(
    workspace: &WorkspaceState,
    uri: &str,
) -> Option<StoredArtifactRecord> {
    let artifact_id = dependency_document_artifact_id(uri)?;
    let store = workspace_dependency_store(workspace)?;
    store.read_artifact_source(artifact_id).ok().flatten()
}

fn dependency_document_input_from_record_with_kind(
    workspace_uri: &str,
    version: i32,
    record: &StoredArtifactRecord,
    is_dependency: bool,
) -> DocumentInput {
    DocumentInput {
        uri: Arc::from(dependency_document_uri_with_kind(
            workspace_uri,
            record.artifact_id,
            record.object_name.as_str(),
            Some(record.object_kind.as_str()),
        )),
        version,
        text: Arc::from(record.source_text.as_str()),
        is_dependency,
        object_name: Some(Arc::from(record.object_name.as_str())),
    }
}

fn dependency_document_input_from_payload_with_kind(
    workspace_uri: &str,
    artifact_id: i64,
    record: &StoredArtifactInput,
) -> DocumentInput {
    DocumentInput {
        uri: Arc::from(dependency_document_uri_with_kind(
            workspace_uri,
            artifact_id,
            record.object_name.as_str(),
            Some(record.object_kind.as_str()),
        )),
        version: 0,
        text: Arc::from(record.source_text.as_str()),
        is_dependency: true,
        object_name: Some(Arc::from(record.object_name.as_str())),
    }
}

fn workspace_dependency_document_input_from_payload_with_kind(
    workspace: &WorkspaceState,
    artifact_id: i64,
    record: &StoredArtifactInput,
) -> DocumentInput {
    let uri = dependency_document_uri(
        &workspace.root_uri,
        artifact_id,
        record.object_name.as_str(),
    );
    let uri_with_kind = dependency_document_uri_with_kind(
        &workspace.root_uri,
        artifact_id,
        record.object_name.as_str(),
        Some(record.object_kind.as_str()),
    );
    if let Some(overlay) = workspace
        .open_documents
        .get(&uri_with_kind)
        .or_else(|| workspace.open_documents.get(&uri))
    {
        let local_export = record.object_type.eq_ignore_ascii_case("local-export");
        return DocumentInput {
            uri: Arc::from(uri_with_kind),
            version: overlay.version,
            text: if local_export {
                Arc::from(record.source_text.as_str())
            } else {
                Arc::clone(&overlay.text)
            },
            is_dependency: local_export,
            object_name: Some(Arc::from(record.object_name.as_str())),
        };
    }
    dependency_document_input_from_payload_with_kind(&workspace.root_uri, artifact_id, record)
}

fn workspace_dependency_document_input(
    workspace: &WorkspaceState,
    record: &StoredArtifactRecord,
) -> DocumentInput {
    let uri = dependency_document_uri(
        &workspace.root_uri,
        record.artifact_id,
        record.object_name.as_str(),
    );
    let uri_with_kind = dependency_document_uri_with_kind(
        &workspace.root_uri,
        record.artifact_id,
        record.object_name.as_str(),
        Some(record.object_kind.as_str()),
    );
    if let Some(overlay) = workspace
        .open_documents
        .get(&uri_with_kind)
        .or_else(|| workspace.open_documents.get(&uri))
    {
        let local_export = record.object_type.eq_ignore_ascii_case("local-export");
        return DocumentInput {
            uri: Arc::from(uri_with_kind),
            version: overlay.version,
            text: if local_export {
                Arc::from(record.source_text.as_str())
            } else {
                Arc::clone(&overlay.text)
            },
            is_dependency: local_export,
            object_name: Some(Arc::from(record.object_name.as_str())),
        };
    }
    dependency_document_input_from_record_with_kind(&workspace.root_uri, 0, record, true)
}

fn dependency_workspace_for_uri<'a>(
    state: &'a ServerState,
    uri: &str,
) -> Option<&'a WorkspaceState> {
    dependency_document_workspace_uri(uri)
        .and_then(|workspace_uri| state.workspaces.get(&workspace_uri))
        .or_else(|| state.workspace_for_uri(uri))
}

fn dependency_document_input(
    workspace: &WorkspaceState,
    uri: &str,
    version: i32,
    text: &str,
) -> Option<DocumentInput> {
    let artifact = dependency_artifact_for_uri(workspace, uri)?;
    let opened = workspace.open_documents.contains_key(uri);
    let local_export = artifact.object_type.eq_ignore_ascii_case("local-export");
    Some(DocumentInput {
        uri: Arc::from(uri),
        version,
        text: if opened && !local_export {
            Arc::from(text)
        } else {
            Arc::from(artifact.source_text)
        },
        is_dependency: !opened || local_export,
        object_name: Some(Arc::from(artifact.object_name)),
    })
}

fn record_dependency_parent_uri(
    workspace: &mut WorkspaceState,
    dependency_uri: &str,
    parent_uri: &str,
) {
    let dependency_uri = normalize_lsp_uri(dependency_uri);
    let parent_uri = normalize_lsp_uri(parent_uri);
    if dependency_uri == parent_uri {
        return;
    }
    workspace
        .dependency_parent_uris
        .entry(dependency_uri)
        .or_default()
        .insert(parent_uri);
}

fn record_fetched_dependency_parent_uris(
    workspace: &mut WorkspaceState,
    fetched_names: &[String],
    source_uris: &[String],
) {
    if fetched_names.is_empty() || source_uris.is_empty() {
        return;
    }
    let normalized_sources = source_uris
        .iter()
        .map(|uri| normalize_lsp_uri(uri))
        .collect::<Vec<_>>();
    let fetched_names = fetched_names
        .iter()
        .map(|name| name.trim().to_ascii_lowercase())
        .filter(|name| !name.is_empty())
        .collect::<HashSet<_>>();
    if fetched_names.is_empty() {
        return;
    }

    let dependency_uris = workspace
        .cache
        .uris()
        .into_iter()
        .filter_map(|uri| {
            if !is_dependency_document_uri(uri.as_ref()) {
                return None;
            }
            let snapshot = workspace.cache.get(uri.as_ref())?;
            let object_name = snapshot.object_name.as_ref()?;
            fetched_names
                .contains(&object_name.trim().to_ascii_lowercase())
                .then(|| uri.to_string())
        })
        .collect::<Vec<_>>();

    for dependency_uri in dependency_uris {
        for source_uri in &normalized_sources {
            record_dependency_parent_uri(workspace, &dependency_uri, source_uri);
        }
    }
}

fn dependency_document_snapshot_from_record(
    uri: &str,
    record: &StoredArtifactRecord,
) -> Arc<AnalysisSnapshot> {
    let (text_len, text_hash) = dependency_batch_candidate_fingerprint(&record.source_text);
    if let Some(cached) = dependency_document_snapshot_cache()
        .lock()
        .unwrap_or_else(|error| error.into_inner())
        .get(uri)
        .filter(|cached| cached.text_len == text_len && cached.text_hash == text_hash)
    {
        debug!(
            uri,
            artifact_id = record.artifact_id,
            object_name = %record.object_name,
            object_type = %record.object_type,
            text_bytes = record.source_text.len(),
            "reused dependency document snapshot"
        );
        return Arc::clone(&cached.snapshot);
    }

    let start = Instant::now();
    let snapshot = DocumentStore::default().publish_input_with_build_plan(
        DocumentInput {
            uri: Arc::from(uri),
            version: 0,
            text: Arc::from(record.source_text.as_str()),
            is_dependency: true,
            object_name: Some(Arc::from(record.object_name.as_str())),
        },
        SnapshotBuildPlan::EDITOR_WORKSPACE,
    );
    debug!(
        uri,
        artifact_id = record.artifact_id,
        object_name = %record.object_name,
        object_type = %record.object_type,
        text_bytes = record.source_text.len(),
        elapsed = ?start.elapsed(),
        "built dependency document snapshot from artifact source"
    );
    let mut cache = dependency_document_snapshot_cache()
        .lock()
        .unwrap_or_else(|error| error.into_inner());
    if cache.len() >= DEPENDENCY_DOCUMENT_SNAPSHOT_CACHE_MAX_ENTRIES && !cache.contains_key(uri) {
        cache.clear();
    }
    cache.insert(
        uri.to_string(),
        CachedDependencyDocumentSnapshot {
            text_len,
            text_hash,
            snapshot: Arc::clone(&snapshot),
        },
    );
    snapshot
}

fn cache_for_uri<'a>(state: &'a ServerState, uri: &str) -> &'a DocumentStore {
    workspace_cache_for_uri(state, uri).unwrap_or(&state.cache)
}

fn workspace_cache_for_uri<'a>(state: &'a ServerState, uri: &str) -> Option<&'a DocumentStore> {
    if let Some(workspace) = state.workspace_for_uri(uri) {
        return Some(&workspace.cache);
    }

    state
        .workspaces
        .values()
        .find(|workspace| {
            workspace.preview_snapshots.contains_key(uri) || workspace.cache.get(uri).is_some()
        })
        .map(|workspace| &workspace.cache)
}

fn snapshot_for_uri(state: &ServerState, uri: &str) -> Option<Arc<AnalysisSnapshot>> {
    if let Some(snapshot) = state
        .workspace_for_uri(uri)
        .and_then(|workspace| workspace.preview_snapshots.get(uri))
    {
        return Some(Arc::clone(snapshot));
    }
    if let Some(snapshot) = state
        .workspaces
        .values()
        .find_map(|workspace| workspace.preview_snapshots.get(uri))
    {
        return Some(Arc::clone(snapshot));
    }
    cache_for_uri(state, uri).get(uri)
}

fn snapshot_with_version(snapshot: &Arc<AnalysisSnapshot>, version: i32) -> Arc<AnalysisSnapshot> {
    Arc::new(AnalysisSnapshot {
        scope_index: Arc::clone(&snapshot.scope_index),
        uri: Arc::clone(&snapshot.uri),
        version,
        text: Arc::clone(&snapshot.text),
        line_index: Arc::clone(&snapshot.line_index),
        project_texts: Arc::clone(&snapshot.project_texts),
        is_dependency: snapshot.is_dependency,
        object_name: snapshot.object_name.clone(),
        parse: Arc::clone(&snapshot.parse),
        symbols: Arc::clone(&snapshot.symbols),
        project: Arc::clone(&snapshot.project),
        routine_analysis: Arc::clone(&snapshot.routine_analysis),
        lint_analysis: Arc::clone(&snapshot.lint_analysis),
        static_analysis: snapshot.static_analysis.as_ref().map(Arc::clone),
        callable_summaries: Arc::clone(&snapshot.callable_summaries),
        call_graph: Arc::clone(&snapshot.call_graph),
    })
}

fn document_input_from_workspace_document(document: &WorkspaceDocument) -> DocumentInput {
    DocumentInput {
        uri: Arc::from(normalize_lsp_uri(document.uri.as_ref())),
        version: document.version,
        text: Arc::from(document.text.as_str()),
        is_dependency: document.is_dependency,
        object_name: document.object_name.clone(),
    }
}

fn merge_local_export_config(target: &mut LocalExportConfig, incoming: &LocalExportConfig) -> bool {
    let mut changed = false;
    if incoming.mode == abap_cache::LocalDependencySourceMode::LocalOnly
        && target.mode != abap_cache::LocalDependencySourceMode::LocalOnly
    {
        target.mode = abap_cache::LocalDependencySourceMode::LocalOnly;
        changed = true;
    } else if target.mode == abap_cache::LocalDependencySourceMode::AdtFirst
        && incoming.mode == abap_cache::LocalDependencySourceMode::LocalFirst
    {
        target.mode = abap_cache::LocalDependencySourceMode::LocalFirst;
        changed = true;
    }

    let mut seen = target
        .roots
        .iter()
        .map(|path| {
            path.to_string_lossy()
                .replace('\\', "/")
                .to_ascii_lowercase()
        })
        .collect::<HashSet<_>>();
    for root in &incoming.roots {
        let key = root
            .to_string_lossy()
            .replace('\\', "/")
            .to_ascii_lowercase();
        if seen.insert(key) {
            target.roots.push(root.clone());
            changed = true;
        }
    }
    changed
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LocalExportDependencyClosureProfile {
    pub total_elapsed: Duration,
    pub waves: usize,
    pub queue_iterations: usize,
    pub documents_examined_for_candidates: usize,
    pub remote_dependency_candidates_produced: usize,
    pub candidate_cache_hits: usize,
    pub candidate_cache_misses: usize,
    pub unique_candidate_resolution_attempts: usize,
    pub resolution_cache_hits: usize,
    pub resolution_cache_misses: usize,
    pub local_export_resolve: LocalExportResolveProfile,
    pub candidate_parse_analyze_time: Duration,
    pub candidate_collection_time: Duration,
    pub added_dependency_documents: usize,
    pub candidate_documents: Vec<LocalExportCandidateDocumentProfile>,
    pub wave_profiles: Vec<LocalExportDependencyClosureWaveProfile>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LocalExportCandidateDocumentProfile {
    pub uri: String,
    pub is_dependency: bool,
    pub text_len: usize,
    pub candidate_count: usize,
    pub cache_hit: bool,
    pub elapsed: Duration,
    pub parse_analyze_time: Duration,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LocalExportDependencyClosureWaveProfile {
    pub index: usize,
    pub elapsed: Duration,
    pub batch_documents: usize,
    pub candidates_produced: usize,
    pub candidate_cache_hits: usize,
    pub candidate_cache_misses: usize,
    pub unique_candidate_resolution_attempts: usize,
    pub resolution_cache_hits: usize,
    pub resolution_cache_misses: usize,
    pub added_dependency_documents: usize,
    pub candidate_collection_time: Duration,
    pub candidate_parse_analyze_time: Duration,
    pub local_export_resolve: LocalExportResolveProfile,
}

pub fn replace_all_workspace_documents_with_local_exports(
    store: &DocumentStore,
    root_path: &Path,
    documents: &[WorkspaceDocument],
    progress: Option<&(dyn Fn(usize, usize) + Sync)>,
) -> HashMap<Arc<str>, Arc<AnalysisSnapshot>> {
    replace_all_workspace_documents_with_local_exports_for_build_plan(
        store,
        root_path,
        documents,
        SnapshotBuildPlan::FULL,
        progress,
    )
}

pub fn replace_all_workspace_documents_with_local_exports_for_build_plan(
    store: &DocumentStore,
    root_path: &Path,
    documents: &[WorkspaceDocument],
    build_plan: SnapshotBuildPlan,
    progress: Option<&(dyn Fn(usize, usize) + Sync)>,
) -> HashMap<Arc<str>, Arc<AnalysisSnapshot>> {
    replace_all_workspace_documents_with_dependency_resolution_for_build_plan(
        store, root_path, None, None, documents, build_plan, progress,
    )
}

pub fn replace_all_workspace_documents_with_local_exports_for_build_plan_profiled(
    store: &DocumentStore,
    root_path: &Path,
    documents: &[WorkspaceDocument],
    build_plan: SnapshotBuildPlan,
    progress: Option<&(dyn Fn(usize, usize) + Sync)>,
) -> (
    HashMap<Arc<str>, Arc<AnalysisSnapshot>>,
    LocalExportDependencyClosureProfile,
) {
    replace_all_workspace_documents_with_dependency_resolution_for_build_plan_profiled(
        store, root_path, None, None, documents, build_plan, progress,
    )
}

fn replace_all_workspace_documents_with_dependency_resolution_for_build_plan(
    store: &DocumentStore,
    root_path: &Path,
    dependency_store: Option<&mut DependencyStoreResolutionContext>,
    local_export_resolver: Option<&mut LocalExportResolver>,
    documents: &[WorkspaceDocument],
    build_plan: SnapshotBuildPlan,
    progress: Option<&(dyn Fn(usize, usize) + Sync)>,
) -> HashMap<Arc<str>, Arc<AnalysisSnapshot>> {
    let mut inputs: Vec<_> = documents
        .iter()
        .map(document_input_from_workspace_document)
        .collect();
    let mut additions = collect_local_export_dependency_closure_documents(
        root_path,
        dependency_store,
        local_export_resolver,
        documents,
        local_export_dependency_max_waves(build_plan),
    );
    additions.sort_by(|left, right| left.uri.cmp(&right.uri));
    inputs.extend(additions.iter().map(document_input_from_workspace_document));
    store.replace_all_with_build_plan_and_progress(inputs, build_plan, progress)
}

fn replace_all_workspace_documents_with_dependency_resolution_for_build_plan_profiled(
    store: &DocumentStore,
    root_path: &Path,
    dependency_store: Option<&mut DependencyStoreResolutionContext>,
    local_export_resolver: Option<&mut LocalExportResolver>,
    documents: &[WorkspaceDocument],
    build_plan: SnapshotBuildPlan,
    progress: Option<&(dyn Fn(usize, usize) + Sync)>,
) -> (
    HashMap<Arc<str>, Arc<AnalysisSnapshot>>,
    LocalExportDependencyClosureProfile,
) {
    let mut inputs: Vec<_> = documents
        .iter()
        .map(document_input_from_workspace_document)
        .collect();
    let (mut additions, profile) = collect_local_export_dependency_closure_documents_profiled(
        root_path,
        dependency_store,
        local_export_resolver,
        documents,
        local_export_dependency_max_waves(build_plan),
    );
    additions.sort_by(|left, right| left.uri.cmp(&right.uri));
    inputs.extend(additions.iter().map(document_input_from_workspace_document));
    (
        store.replace_all_with_build_plan_and_progress(inputs, build_plan, progress),
        profile,
    )
}

fn local_export_dependency_max_waves(build_plan: SnapshotBuildPlan) -> Option<usize> {
    match build_plan.normalized().dependency_diagnostics {
        DependencyDiagnosticsMode::All => None,
        DependencyDiagnosticsMode::EditableAndIncludes => Some(1),
    }
}

struct DependencyStoreResolutionContext {
    workspace_uri: String,
    profile: DependencyProfile,
    store: DependencyStore,
    reader: Option<DependencyStoreReader>,
}

impl DependencyStoreResolutionContext {
    fn new(workspace: &WorkspaceState) -> Option<Self> {
        let profile = workspace_dependency_profile(workspace)?;
        let store = workspace_dependency_store(workspace)?;
        let reader = store.reader().ok();
        Some(Self {
            workspace_uri: workspace.root_uri.clone(),
            profile,
            store,
            reader,
        })
    }

    fn read_candidate(&self, candidate: &RemoteDependencyCandidate) -> Option<WorkspaceDocument> {
        let reader = self.reader.as_ref()?;
        let record = reader
            .find_artifact_for_candidate(
                &self.profile,
                candidate.name.as_str(),
                candidate.kind.as_str(),
            )
            .ok()
            .flatten()?;
        Some(workspace_document_from_dependency_record(
            &self.workspace_uri,
            &record,
        ))
    }

    fn store_local_export_documents(
        &mut self,
        candidate: &RemoteDependencyCandidate,
        documents: &[WorkspaceDocument],
    ) -> Option<WorkspaceDocument> {
        let artifacts: Vec<_> = documents
            .iter()
            .filter_map(|document| local_export_document_artifact(candidate, document))
            .collect();
        if artifacts.is_empty() {
            return None;
        }
        let artifact_ids = self.store.put_artifacts(&self.profile, &artifacts).ok()?;
        self.reader = self.store.reader().ok();
        if let Some(document) = self.read_candidate(candidate) {
            return Some(document);
        }
        artifact_ids.into_iter().find_map(|artifact_id| {
            self.store
                .read_artifact_source(artifact_id)
                .ok()
                .flatten()
                .map(|record| {
                    workspace_document_from_dependency_record(&self.workspace_uri, &record)
                })
        })
    }
}

fn workspace_document_from_dependency_record(
    workspace_uri: &str,
    record: &StoredArtifactRecord,
) -> WorkspaceDocument {
    WorkspaceDocument {
        uri: Arc::from(dependency_document_uri_with_kind(
            workspace_uri,
            record.artifact_id,
            record.object_name.as_str(),
            Some(record.object_kind.as_str()),
        )),
        version: 0,
        text: record.source_text.clone(),
        is_dependency: true,
        object_name: Some(Arc::from(record.object_name.as_str())),
    }
}

fn local_export_document_artifact(
    candidate: &RemoteDependencyCandidate,
    document: &WorkspaceDocument,
) -> Option<StoredArtifactInput> {
    let object_name = document
        .object_name
        .as_deref()
        .map(str::trim)
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| candidate.name.trim());
    if object_name.is_empty() {
        return None;
    }
    let object_kind = local_export_document_object_kind(candidate, document);
    if object_kind.is_empty() {
        return None;
    }
    let object_uri = document.uri.to_string();
    Some(StoredArtifactInput {
        package_name: local_export_document_package_name(document),
        object_kind,
        object_name: object_name.to_string(),
        object_uri: object_uri.clone(),
        object_type: "local-export".to_string(),
        description: "Local export dependency".to_string(),
        file_extension: "abap".to_string(),
        source_text: document.text.clone(),
        fetched_at: local_export_dependency_fetched_at(),
        symbols: extract_stored_dependency_symbols(object_uri.as_str(), document.text.as_str()),
    })
}

pub fn local_export_document_artifact_payload(
    candidate: &RemoteDependencyCandidate,
    document: &WorkspaceDocument,
) -> Option<DependencyArtifactPayload> {
    let artifact = local_export_document_artifact(candidate, document)?;
    Some(DependencyArtifactPayload {
        package_name: artifact.package_name,
        object_kind: artifact.object_kind,
        object_name: artifact.object_name,
        object_uri: artifact.object_uri,
        object_type: artifact.object_type,
        description: artifact.description,
        file_extension: artifact.file_extension,
        source_text: artifact.source_text,
        fetched_at: artifact.fetched_at,
    })
}

fn local_export_dependency_fetched_at() -> String {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|duration| format!("local-export-{}", duration.as_secs()))
        .unwrap_or_else(|_| "local-export".to_string())
}

fn local_export_document_package_name(document: &WorkspaceDocument) -> String {
    let path = file_uri_to_path(document.uri.as_ref());
    let Some(path) = path.as_ref() else {
        return "local-export".to_string();
    };
    let components = path
        .components()
        .filter_map(|component| component.as_os_str().to_str())
        .collect::<Vec<_>>();
    let keys = components
        .iter()
        .map(|component| local_export_path_segment_key(component))
        .collect::<Vec<_>>();

    for (idx, key) in keys.iter().enumerate() {
        if matches!(key.as_str(), "package" | "packages")
            && let Some(package_name) = components.get(idx + 1)
        {
            let package_name = percent_decode_local_export_segment(package_name)
                .trim()
                .to_ascii_uppercase();
            if !package_name.is_empty() {
                return package_name;
            }
        }
    }

    for (idx, key) in keys.iter().enumerate() {
        if idx > 0
            && canonical_local_export_kind_for_segment_key(key)
            && let Some(package_idx) = local_export_package_component_index_for_kind(&keys, idx)
            && let Some(package_name) = components.get(package_idx)
        {
            let package_name = percent_decode_local_export_segment(package_name)
                .trim()
                .to_ascii_uppercase();
            if !package_name.is_empty() && !package_name.ends_with(".ABAP") {
                return package_name;
            }
        }
    }

    "local-export".to_string()
}

fn local_export_package_component_index_for_kind(
    keys: &[String],
    kind_idx: usize,
) -> Option<usize> {
    let parent_idx = kind_idx.checked_sub(1)?;
    let parent_key = keys.get(parent_idx).map(String::as_str);
    if matches!(
        parent_key,
        Some("source-code-library" | "dictionary" | "others")
    ) {
        return kind_idx.checked_sub(2);
    }
    Some(parent_idx)
}

fn local_export_document_object_kind(
    candidate: &RemoteDependencyCandidate,
    document: &WorkspaceDocument,
) -> String {
    let path_key = file_uri_to_path(document.uri.as_ref())
        .map(|path| {
            path.to_string_lossy()
                .replace('\\', "/")
                .to_ascii_lowercase()
        })
        .unwrap_or_else(|| document.uri.to_ascii_lowercase());
    for (needle, kind) in [
        ("global-interface", "global-interface"),
        ("interfaces", "global-interface"),
        ("global-class", "global-class"),
        ("classes", "global-class"),
        ("function-module", "function-module"),
        ("function modules", "function-module"),
        ("function-modules", "function-module"),
        ("message-class", "message-class"),
        ("message classes", "message-class"),
        ("message-classes", "message-class"),
        ("data-element", "ddic-data-element"),
        ("data elements", "ddic-data-element"),
        ("data-elements", "ddic-data-element"),
        ("database-table", "ddic-table"),
        ("database tables", "ddic-table"),
        ("database-tables", "ddic-table"),
        ("structure", "ddic-structure"),
        ("structures", "ddic-structure"),
        ("table-type", "ddic-table-type"),
        ("table types", "ddic-table-type"),
        ("table-types", "ddic-table-type"),
        ("domain", "ddic-domain"),
        ("domains", "ddic-domain"),
        ("view", "ddic-view"),
        ("views", "ddic-view"),
        ("include", "include"),
        ("includes", "include"),
        ("report", "report"),
        ("program", "report"),
        ("programs", "report"),
    ] {
        if path_key.contains(needle) {
            return kind.to_string();
        }
    }

    let parsed = parse(document.text.as_str());
    let unit = analyze_unit(Arc::clone(&document.uri), document.text.as_str(), &parsed);
    if unit
        .symbols
        .iter()
        .any(|symbol| symbol.kind == abap_symbols::SymbolKind::Interface)
    {
        return "global-interface".to_string();
    }
    if unit
        .symbols
        .iter()
        .any(|symbol| symbol.kind == abap_symbols::SymbolKind::Class)
    {
        return "global-class".to_string();
    }
    if unit
        .symbols
        .iter()
        .any(|symbol| symbol.kind == abap_symbols::SymbolKind::Report)
    {
        return "report".to_string();
    }
    if unit
        .symbols
        .iter()
        .any(|symbol| symbol.kind == abap_symbols::SymbolKind::Include)
    {
        return "include".to_string();
    }

    match candidate.kind.trim().to_ascii_lowercase().as_str() {
        "include" => "include",
        "function" => "function-module",
        "message-class" => "message-class",
        "report" => "report",
        "static" => "global-class",
        "type" | "symbol" => "ddic-data-element",
        _ => "",
    }
    .to_string()
}

fn local_export_path_segment_key(segment: &str) -> String {
    segment
        .trim()
        .chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() {
                ch.to_ascii_lowercase()
            } else {
                '-'
            }
        })
        .collect::<String>()
        .split('-')
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>()
        .join("-")
}

fn canonical_local_export_kind_for_segment_key(key: &str) -> bool {
    matches!(
        key,
        "global-interface"
            | "interface"
            | "interfaces"
            | "global-class"
            | "class"
            | "classes"
            | "function-module"
            | "function-modules"
            | "message-class"
            | "message-classes"
            | "data-element"
            | "data-elements"
            | "database-table"
            | "database-tables"
            | "structure"
            | "structures"
            | "table-type"
            | "table-types"
            | "domain"
            | "domains"
            | "view"
            | "views"
            | "include"
            | "includes"
            | "report"
            | "reports"
            | "program"
            | "programs"
    )
}

fn percent_decode_local_export_segment(segment: &str) -> String {
    decode_uri_component(segment).unwrap_or_else(|| segment.to_string())
}

fn collect_local_export_dependency_closure_documents(
    root_path: &Path,
    mut dependency_store: Option<&mut DependencyStoreResolutionContext>,
    local_export_resolver: Option<&mut LocalExportResolver>,
    documents: &[WorkspaceDocument],
    max_waves: Option<usize>,
) -> Vec<WorkspaceDocument> {
    let mut documents_by_uri: HashMap<String, WorkspaceDocument> = documents
        .iter()
        .cloned()
        .map(|document| (document.uri.to_string(), document))
        .collect();
    let workspace_candidate_names = workspace_local_candidate_names(documents);
    let mut document_configs = HashMap::<String, LocalExportConfig>::new();
    let mut queue = VecDeque::<String>::new();
    let mut pending = HashSet::<String>::new();

    for document in documents.iter().filter(|document| !document.is_dependency) {
        let config = local_export_config_for_source(root_path, document.uri.as_ref());
        if !config.uses_local_exports() {
            continue;
        }
        let uri = document.uri.to_string();
        document_configs.insert(uri.clone(), config);
        if pending.insert(uri.clone()) {
            queue.push_back(uri);
        }
    }

    let mut additions = Vec::<WorkspaceDocument>::new();
    let mut resolution_cache = HashMap::<String, Option<WorkspaceDocument>>::new();
    let mut fallback_resolver = LocalExportResolver::default();
    let resolver = match local_export_resolver {
        Some(resolver) => resolver,
        None => &mut fallback_resolver,
    };

    let mut waves = 0usize;
    while !queue.is_empty() {
        waves += 1;
        let mut batch = Vec::new();
        while let Some(uri) = queue.pop_front() {
            pending.remove(&uri);
            let Some(config) = document_configs.get(&uri).cloned() else {
                continue;
            };
            if !config.uses_local_exports() {
                continue;
            }
            let Some(document) = documents_by_uri.get(&uri).cloned() else {
                continue;
            };
            batch.push((uri, config, document));
        }
        let batch_candidates = collect_local_export_dependency_candidate_batch(batch);

        for (_uri, config, candidates) in batch_candidates {
            for candidate in candidates {
                if workspace_candidate_names.contains(&candidate.name.trim().to_ascii_lowercase()) {
                    continue;
                }
                let resolution_key = local_export_resolution_cache_key(&config.roots, &candidate);
                let resolved = if let Some(cached) = resolution_cache.get(&resolution_key) {
                    cached.clone()
                } else {
                    let resolved = resolve_dependency_document_for_candidate(
                        dependency_store.as_deref_mut(),
                        &config.roots,
                        resolver,
                        &candidate,
                    );
                    resolution_cache.insert(resolution_key, resolved.clone());
                    resolved
                };
                let Some(resolved_document) = resolved else {
                    continue;
                };
                let resolved_uri = resolved_document.uri.to_string();
                let entry = document_configs
                    .entry(resolved_uri.clone())
                    .or_insert_with(|| config.clone());
                let config_changed = merge_local_export_config(entry, &config);
                let first_seen = !documents_by_uri.contains_key(&resolved_uri);
                if first_seen {
                    documents_by_uri.insert(resolved_uri.clone(), resolved_document.clone());
                    additions.push(resolved_document);
                }
                if (first_seen || config_changed) && pending.insert(resolved_uri.clone()) {
                    queue.push_back(resolved_uri);
                }
            }
        }
        if max_waves.is_some_and(|max_waves| waves >= max_waves) {
            break;
        }
    }

    additions
}

fn collect_local_export_dependency_closure_documents_profiled(
    root_path: &Path,
    mut dependency_store: Option<&mut DependencyStoreResolutionContext>,
    local_export_resolver: Option<&mut LocalExportResolver>,
    documents: &[WorkspaceDocument],
    max_waves: Option<usize>,
) -> (Vec<WorkspaceDocument>, LocalExportDependencyClosureProfile) {
    let total_start = Instant::now();
    let mut profile = LocalExportDependencyClosureProfile::default();
    let mut documents_by_uri: HashMap<String, WorkspaceDocument> = documents
        .iter()
        .cloned()
        .map(|document| (document.uri.to_string(), document))
        .collect();
    let workspace_candidate_names = workspace_local_candidate_names(documents);
    let mut document_configs = HashMap::<String, LocalExportConfig>::new();
    let mut queue = VecDeque::<String>::new();
    let mut pending = HashSet::<String>::new();

    for document in documents.iter().filter(|document| !document.is_dependency) {
        let config = local_export_config_for_source(root_path, document.uri.as_ref());
        if !config.uses_local_exports() {
            continue;
        }
        let uri = document.uri.to_string();
        document_configs.insert(uri.clone(), config);
        if pending.insert(uri.clone()) {
            queue.push_back(uri);
        }
    }

    let mut additions = Vec::<WorkspaceDocument>::new();
    let mut resolution_cache = HashMap::<String, Option<WorkspaceDocument>>::new();
    let mut fallback_resolver = LocalExportResolver::default();
    let resolver = match local_export_resolver {
        Some(resolver) => resolver,
        None => &mut fallback_resolver,
    };
    let mut wave_index = 0usize;

    while !queue.is_empty() {
        wave_index += 1;
        let wave_start = Instant::now();
        let mut wave_profile = LocalExportDependencyClosureWaveProfile {
            index: wave_index,
            ..Default::default()
        };
        let mut batch = Vec::new();
        while let Some(uri) = queue.pop_front() {
            pending.remove(&uri);
            profile.queue_iterations += 1;
            let Some(config) = document_configs.get(&uri).cloned() else {
                continue;
            };
            if !config.uses_local_exports() {
                continue;
            }
            let Some(document) = documents_by_uri.get(&uri).cloned() else {
                continue;
            };
            batch.push((uri, config, document));
        }
        wave_profile.batch_documents = batch.len();
        profile.waves += 1;

        let candidate_collection_start = Instant::now();
        let batch_candidates = collect_local_export_dependency_candidate_batch_profiled(batch);
        wave_profile.candidate_collection_time = candidate_collection_start.elapsed();

        let mut wave_resolve_profile = LocalExportResolveProfile::default();
        for (_uri, config, candidates, candidate_profile) in batch_candidates {
            profile.documents_examined_for_candidates += 1;
            profile.remote_dependency_candidates_produced += candidate_profile.candidate_count;
            if candidate_profile.cache_hit {
                profile.candidate_cache_hits += 1;
                wave_profile.candidate_cache_hits += 1;
            } else {
                profile.candidate_cache_misses += 1;
                wave_profile.candidate_cache_misses += 1;
            }
            profile.candidate_parse_analyze_time += candidate_profile.parse_analyze_time;
            wave_profile.candidates_produced += candidate_profile.candidate_count;
            wave_profile.candidate_parse_analyze_time += candidate_profile.parse_analyze_time;
            profile.candidate_documents.push(candidate_profile);

            for candidate in candidates {
                if workspace_candidate_names.contains(&candidate.name.trim().to_ascii_lowercase()) {
                    continue;
                }
                let resolution_key = local_export_resolution_cache_key(&config.roots, &candidate);
                let resolved = if let Some(cached) = resolution_cache.get(&resolution_key) {
                    profile.resolution_cache_hits += 1;
                    wave_profile.resolution_cache_hits += 1;
                    cached.clone()
                } else {
                    profile.resolution_cache_misses += 1;
                    profile.unique_candidate_resolution_attempts += 1;
                    wave_profile.resolution_cache_misses += 1;
                    wave_profile.unique_candidate_resolution_attempts += 1;
                    let resolved = resolve_dependency_document_for_candidate_profiled(
                        dependency_store.as_deref_mut(),
                        &config.roots,
                        resolver,
                        &candidate,
                        &mut wave_resolve_profile,
                    );
                    resolution_cache.insert(resolution_key, resolved.clone());
                    resolved
                };
                let Some(resolved_document) = resolved else {
                    continue;
                };
                let resolved_uri = resolved_document.uri.to_string();
                let entry = document_configs
                    .entry(resolved_uri.clone())
                    .or_insert_with(|| config.clone());
                let config_changed = merge_local_export_config(entry, &config);
                let first_seen = !documents_by_uri.contains_key(&resolved_uri);
                if first_seen {
                    documents_by_uri.insert(resolved_uri.clone(), resolved_document.clone());
                    additions.push(resolved_document);
                    wave_profile.added_dependency_documents += 1;
                }
                if (first_seen || config_changed) && pending.insert(resolved_uri.clone()) {
                    queue.push_back(resolved_uri);
                }
            }
        }

        wave_profile.local_export_resolve = wave_resolve_profile.clone();
        profile
            .local_export_resolve
            .merge_from(&wave_resolve_profile);
        profile.candidate_collection_time += wave_profile.candidate_collection_time;
        wave_profile.elapsed = wave_start.elapsed();
        profile.wave_profiles.push(wave_profile);
        if max_waves.is_some_and(|max_waves| wave_index >= max_waves) {
            break;
        }
    }

    profile.added_dependency_documents = additions.len();
    profile.total_elapsed = total_start.elapsed();
    (additions, profile)
}

fn workspace_local_candidate_names(documents: &[WorkspaceDocument]) -> HashSet<String> {
    let mut names = HashSet::new();
    for document in documents.iter().filter(|document| !document.is_dependency) {
        add_workspace_local_candidate_names(
            &mut names,
            document.uri.as_ref(),
            document.object_name.as_ref(),
        );
    }
    names
}

fn workspace_local_candidate_names_from_cache(
    workspace: &WorkspaceState,
    edited_document: &WorkspaceDocument,
) -> HashSet<String> {
    let mut names = HashSet::new();
    for uri in workspace.cache.uris() {
        let Some(snapshot) = workspace.cache.get(uri.as_ref()) else {
            continue;
        };
        if snapshot.is_dependency || snapshot.uri.as_ref() == edited_document.uri.as_ref() {
            continue;
        }
        add_workspace_local_candidate_names(
            &mut names,
            snapshot.uri.as_ref(),
            snapshot.object_name.as_ref(),
        );
    }
    if !edited_document.is_dependency {
        add_workspace_local_candidate_names(
            &mut names,
            edited_document.uri.as_ref(),
            edited_document.object_name.as_ref(),
        );
    }
    names
}

fn add_workspace_local_candidate_names(
    names: &mut HashSet<String>,
    uri: &str,
    object_name: Option<&Arc<str>>,
) {
    if let Some(object_name) = object_name {
        let normalized = object_name.trim().to_ascii_lowercase();
        if !normalized.is_empty() {
            names.insert(normalized);
        }
    }
    if let Some(stem) = file_uri_to_path(uri)
        .and_then(|path| {
            path.file_stem()
                .and_then(|stem| stem.to_str())
                .map(str::to_string)
        })
        .map(|stem| stem.trim().to_ascii_lowercase())
        .filter(|stem| !stem.is_empty())
    {
        names.insert(stem);
    }
}

fn local_export_resolution_cache_key(
    roots: &[PathBuf],
    candidate: &RemoteDependencyCandidate,
) -> String {
    let roots_key = roots
        .iter()
        .map(|root| {
            root.to_string_lossy()
                .replace('\\', "/")
                .to_ascii_lowercase()
        })
        .collect::<Vec<_>>()
        .join(";");
    format!("{roots_key}|{}", remote_candidate_key(candidate))
}

fn resolve_dependency_document_for_candidate(
    dependency_store: Option<&mut DependencyStoreResolutionContext>,
    roots: &[PathBuf],
    local_export_resolver: &mut LocalExportResolver,
    candidate: &RemoteDependencyCandidate,
) -> Option<WorkspaceDocument> {
    if let Some(context) = dependency_store {
        if let Some(document) = context.read_candidate(candidate) {
            return Some(document);
        }
        let documents = resolve_local_export_dependency_documents(
            roots,
            local_export_resolver,
            &candidate.name,
            &candidate.kind,
        );
        if !documents.is_empty() {
            let fallback = documents.first().cloned();
            return context
                .store_local_export_documents(candidate, &documents)
                .or(fallback);
        }
        return context.read_candidate(candidate);
    }

    resolve_local_export_dependency_document(
        roots,
        local_export_resolver,
        &candidate.name,
        &candidate.kind,
    )
}

fn resolve_dependency_document_for_candidate_profiled(
    dependency_store: Option<&mut DependencyStoreResolutionContext>,
    roots: &[PathBuf],
    local_export_resolver: &mut LocalExportResolver,
    candidate: &RemoteDependencyCandidate,
    profile: &mut LocalExportResolveProfile,
) -> Option<WorkspaceDocument> {
    if let Some(context) = dependency_store {
        if let Some(document) = context.read_candidate(candidate) {
            return Some(document);
        }
        let documents = resolve_local_export_dependency_documents_profiled(
            roots,
            local_export_resolver,
            &candidate.name,
            &candidate.kind,
            Some(profile),
        );
        if !documents.is_empty() {
            let fallback = documents.first().cloned();
            return context
                .store_local_export_documents(candidate, &documents)
                .or(fallback);
        }
        return context.read_candidate(candidate);
    }

    resolve_local_export_dependency_document_profiled(
        roots,
        local_export_resolver,
        &candidate.name,
        &candidate.kind,
        Some(profile),
    )
}

fn collect_local_export_dependency_candidate_batch(
    batch: Vec<(String, LocalExportConfig, WorkspaceDocument)>,
) -> Vec<(String, LocalExportConfig, Vec<RemoteDependencyCandidate>)> {
    if batch.len() < 8 {
        return batch
            .into_iter()
            .map(|(uri, config, document)| {
                (
                    uri,
                    config,
                    collect_local_export_dependency_candidates(&document),
                )
            })
            .collect();
    }

    let parallelism = std::thread::available_parallelism()
        .map(|value| value.get())
        .unwrap_or(1)
        .min(batch.len());
    if parallelism <= 1 {
        return batch
            .into_iter()
            .map(|(uri, config, document)| {
                (
                    uri,
                    config,
                    collect_local_export_dependency_candidates(&document),
                )
            })
            .collect();
    }

    let chunk_size = batch.len().div_ceil(parallelism);
    std::thread::scope(|scope| {
        let mut handles = Vec::new();
        for chunk in batch.chunks(chunk_size) {
            let chunk = chunk.to_vec();
            handles.push(scope.spawn(move || {
                chunk
                    .into_iter()
                    .map(|(uri, config, document)| {
                        (
                            uri,
                            config,
                            collect_local_export_dependency_candidates(&document),
                        )
                    })
                    .collect::<Vec<_>>()
            }));
        }

        let mut out = Vec::new();
        for handle in handles {
            out.extend(
                handle
                    .join()
                    .expect("local export candidate worker should not panic"),
            );
        }
        out
    })
}

fn collect_local_export_dependency_candidate_batch_profiled(
    batch: Vec<(String, LocalExportConfig, WorkspaceDocument)>,
) -> Vec<(
    String,
    LocalExportConfig,
    Vec<RemoteDependencyCandidate>,
    LocalExportCandidateDocumentProfile,
)> {
    if batch.len() < 8 {
        return batch
            .into_iter()
            .map(|(uri, config, document)| {
                let (candidates, profile) =
                    collect_local_export_dependency_candidates_profiled(&document);
                (uri, config, candidates, profile)
            })
            .collect();
    }

    let parallelism = std::thread::available_parallelism()
        .map(|value| value.get())
        .unwrap_or(1)
        .min(batch.len());
    if parallelism <= 1 {
        return batch
            .into_iter()
            .map(|(uri, config, document)| {
                let (candidates, profile) =
                    collect_local_export_dependency_candidates_profiled(&document);
                (uri, config, candidates, profile)
            })
            .collect();
    }

    let chunk_size = batch.len().div_ceil(parallelism);
    std::thread::scope(|scope| {
        let mut handles = Vec::new();
        for chunk in batch.chunks(chunk_size) {
            let chunk = chunk.to_vec();
            handles.push(scope.spawn(move || {
                chunk
                    .into_iter()
                    .map(|(uri, config, document)| {
                        let (candidates, profile) =
                            collect_local_export_dependency_candidates_profiled(&document);
                        (uri, config, candidates, profile)
                    })
                    .collect::<Vec<_>>()
            }));
        }

        let mut out = Vec::new();
        for handle in handles {
            out.extend(
                handle
                    .join()
                    .expect("local export candidate worker should not panic"),
            );
        }
        out
    })
}

fn collect_local_export_dependency_candidates(
    document: &WorkspaceDocument,
) -> Vec<RemoteDependencyCandidate> {
    if let Some(candidates) = cached_local_export_dependency_candidates(document) {
        return candidates;
    }

    let candidates = collect_local_export_dependency_candidates_uncached(document);
    store_local_export_dependency_candidates(document, candidates.clone());
    candidates
}

fn collect_local_export_dependency_candidates_uncached(
    document: &WorkspaceDocument,
) -> Vec<RemoteDependencyCandidate> {
    let analysis_text = if document.is_dependency {
        // Dependency surface projection strips method bodies, but local export closure
        // needs full dependency text to discover implementation-only transitive refs.
        Arc::<str>::from(document.text.as_str())
    } else {
        analysis_text_for_document(document.text.as_ref(), false)
    };
    let parsed = parse(analysis_text.as_ref());
    let unit = analyze_unit(Arc::clone(&document.uri), analysis_text.as_ref(), &parsed);
    collect_remote_dependency_candidates_for_unit(&unit)
}

fn cached_local_export_dependency_candidates(
    document: &WorkspaceDocument,
) -> Option<Vec<RemoteDependencyCandidate>> {
    let (text_len, text_hash) = dependency_batch_candidate_fingerprint(document.text.as_str());
    let cache = local_export_dependency_candidate_cache()
        .lock()
        .unwrap_or_else(|error| error.into_inner());
    let cached = cache.get(document.uri.as_ref())?;
    (cached.version == document.version
        && cached.text_len == text_len
        && cached.text_hash == text_hash
        && cached.is_dependency == document.is_dependency
        && cached.object_name == document.object_name)
        .then(|| cached.candidates.clone())
}

fn store_local_export_dependency_candidates(
    document: &WorkspaceDocument,
    candidates: Vec<RemoteDependencyCandidate>,
) {
    let (text_len, text_hash) = dependency_batch_candidate_fingerprint(document.text.as_str());
    let mut cache = local_export_dependency_candidate_cache()
        .lock()
        .unwrap_or_else(|error| error.into_inner());
    if cache.len() >= LOCAL_EXPORT_DEPENDENCY_CANDIDATE_CACHE_MAX_ENTRIES
        && !cache.contains_key(document.uri.as_ref())
    {
        cache.clear();
    }
    cache.insert(
        document.uri.to_string(),
        CachedLocalExportDependencyCandidates {
            version: document.version,
            text_len,
            text_hash,
            is_dependency: document.is_dependency,
            object_name: document.object_name.clone(),
            candidates,
        },
    );
}

fn collect_local_export_dependency_candidates_profiled(
    document: &WorkspaceDocument,
) -> (
    Vec<RemoteDependencyCandidate>,
    LocalExportCandidateDocumentProfile,
) {
    let total_start = Instant::now();
    if let Some(candidates) = cached_local_export_dependency_candidates(document) {
        let profile = LocalExportCandidateDocumentProfile {
            uri: document.uri.to_string(),
            is_dependency: document.is_dependency,
            text_len: document.text.len(),
            candidate_count: candidates.len(),
            cache_hit: true,
            elapsed: total_start.elapsed(),
            parse_analyze_time: Duration::ZERO,
        };
        return (candidates, profile);
    }

    let analysis_text = if document.is_dependency {
        // Dependency surface projection strips method bodies, but local export closure
        // needs full dependency text to discover implementation-only transitive refs.
        Arc::<str>::from(document.text.as_str())
    } else {
        analysis_text_for_document(document.text.as_ref(), false)
    };
    let parse_analyze_start = Instant::now();
    let parsed = parse(analysis_text.as_ref());
    let unit = analyze_unit(Arc::clone(&document.uri), analysis_text.as_ref(), &parsed);
    let parse_analyze_time = parse_analyze_start.elapsed();
    let candidates = collect_remote_dependency_candidates_for_unit(&unit);
    store_local_export_dependency_candidates(document, candidates.clone());
    let profile = LocalExportCandidateDocumentProfile {
        uri: document.uri.to_string(),
        is_dependency: document.is_dependency,
        text_len: document.text.len(),
        candidate_count: candidates.len(),
        cache_hit: false,
        elapsed: total_start.elapsed(),
        parse_analyze_time,
    };
    (candidates, profile)
}

fn document_uses_local_exports(workspace: &WorkspaceState, uri: &str) -> bool {
    let Some(root_path) = file_uri_to_path(&workspace.root_uri) else {
        return false;
    };
    local_export_config_for_source(&root_path, uri).uses_local_exports()
}

fn workspace_document_from_input(input: &DocumentInput) -> WorkspaceDocument {
    WorkspaceDocument {
        uri: Arc::clone(&input.uri),
        version: input.version,
        text: input.text.to_string(),
        is_dependency: input.is_dependency,
        object_name: input.object_name.clone(),
    }
}

fn collect_incremental_local_export_dependency_documents(
    workspace: &mut WorkspaceState,
    source_document: &WorkspaceDocument,
) -> Vec<WorkspaceDocument> {
    let Some(root_path) = file_uri_to_path(&workspace.root_uri) else {
        return Vec::new();
    };
    let source_config = local_export_config_for_source(&root_path, source_document.uri.as_ref());
    if !source_config.uses_local_exports() {
        return Vec::new();
    }

    let workspace_candidate_names =
        workspace_local_candidate_names_from_cache(workspace, source_document);
    let mut known_uris: HashSet<String> = workspace
        .cache
        .uris()
        .into_iter()
        .map(|uri| uri.to_string())
        .collect();
    known_uris.insert(source_document.uri.to_string());

    let mut additions = Vec::<WorkspaceDocument>::new();
    let mut queue = VecDeque::<(LocalExportConfig, WorkspaceDocument)>::new();
    queue.push_back((source_config, source_document.clone()));
    let mut resolution_cache = HashMap::<String, Option<WorkspaceDocument>>::new();
    let mut dependency_store_resolution = DependencyStoreResolutionContext::new(workspace);
    let resolver = Arc::clone(&workspace.local_export_resolver);
    let mut waves = 0usize;

    while let Some((config, document)) = queue.pop_front() {
        waves += 1;
        for candidate in collect_local_export_dependency_candidates(&document) {
            if workspace_candidate_names.contains(&candidate.name.trim().to_ascii_lowercase()) {
                continue;
            }
            let resolution_key = local_export_resolution_cache_key(&config.roots, &candidate);
            let resolved = if let Some(cached) = resolution_cache.get(&resolution_key) {
                cached.clone()
            } else {
                let resolved = {
                    let mut resolver = resolver.lock().unwrap_or_else(|error| error.into_inner());
                    resolve_dependency_document_for_candidate(
                        dependency_store_resolution.as_mut(),
                        &config.roots,
                        &mut resolver,
                        &candidate,
                    )
                };
                resolution_cache.insert(resolution_key, resolved.clone());
                resolved
            };
            let Some(resolved_document) = resolved else {
                continue;
            };
            let resolved_uri = resolved_document.uri.to_string();
            if !known_uris.insert(resolved_uri) {
                continue;
            }
            queue.push_back((config.clone(), resolved_document.clone()));
            additions.push(resolved_document);
        }
        if waves >= 1 {
            break;
        }
    }

    additions.sort_by(|left, right| left.uri.cmp(&right.uri));
    additions
}

fn publish_workspace_input_with_local_export_resolution(
    workspace: &mut WorkspaceState,
    input: DocumentInput,
    progress: Option<&(dyn Fn(usize, usize) + Sync)>,
) -> Arc<AnalysisSnapshot> {
    let uri = Arc::clone(&input.uri);
    let source_document = workspace_document_from_input(&input);
    let additions =
        collect_incremental_local_export_dependency_documents(workspace, &source_document);
    if additions.is_empty() {
        let build_plan = workspace_committed_build_plan(workspace);
        return publish_workspace_input_with_dependency_hydration(workspace, input, build_plan);
    }

    clear_workspace_dependency_candidate_caches(workspace);
    sync_workspace_lint_policy(workspace);
    let build_plan = workspace_committed_build_plan(workspace);
    let mut inputs = Vec::with_capacity(additions.len() + 1);
    inputs.push(input);
    inputs.extend(additions.iter().map(document_input_from_workspace_document));
    let snapshots = workspace
        .cache
        .publish_inputs_with_build_plan_and_progress(inputs, build_plan, progress);
    let _ = hydrate_workspace_dependency_documents(workspace);
    workspace
        .cache
        .get(uri.as_ref())
        .or_else(|| snapshots.get(uri.as_ref()).cloned())
        .expect("published local-export source should exist")
}

#[derive(Debug, Clone)]
struct WorkspaceManifestDocumentInfo {
    unit_name: String,
    unit_kind: String,
    is_member: bool,
    is_dependency: bool,
    object_name: Option<Arc<str>>,
}

#[derive(Debug, Default)]
struct WorkspaceManifestObjectMatches {
    uris: HashSet<String>,
    matched_names: HashSet<String>,
}

fn normalize_targeted_refresh_manifest_path(value: &str) -> String {
    value
        .trim()
        .replace('\\', "/")
        .trim_start_matches("./")
        .to_string()
}

fn workspace_manifest_document_info(
    workspace: &WorkspaceState,
    uri: &str,
) -> Option<WorkspaceManifestDocumentInfo> {
    let root_path = file_uri_to_path(&workspace.root_uri)?;
    let manifest = workspace.manifest.as_ref()?;
    let (is_dependency, object_name) =
        manifest_document_metadata(&root_path, &workspace.root_uri, manifest, uri)?;

    manifest.units.iter().find_map(|unit| {
        let root_file = normalize_targeted_refresh_manifest_path(&unit.root_file);
        let root_uri =
            (!root_file.is_empty()).then(|| path_to_file_uri(&root_path.join(&root_file)));
        let member_match = unit.members.iter().any(|member| {
            let member_file = normalize_targeted_refresh_manifest_path(&member.file);
            !member_file.is_empty() && path_to_file_uri(&root_path.join(member_file)) == uri
        });
        (root_uri.as_deref() == Some(uri) || member_match).then(|| WorkspaceManifestDocumentInfo {
            unit_name: unit.name.clone(),
            unit_kind: unit.kind.clone(),
            is_member: member_match,
            is_dependency,
            object_name: object_name.clone(),
        })
    })
}

fn workspace_manifest_uris_for_object_names(
    workspace: &WorkspaceState,
    fetched_names: &HashSet<String>,
) -> WorkspaceManifestObjectMatches {
    let Some(root_path) = file_uri_to_path(&workspace.root_uri) else {
        return WorkspaceManifestObjectMatches::default();
    };
    let Some(manifest) = workspace.manifest.as_ref() else {
        return WorkspaceManifestObjectMatches::default();
    };

    let mut matches = WorkspaceManifestObjectMatches::default();
    for unit in &manifest.units {
        let unit_name = unit.name.trim().to_ascii_lowercase();
        let unit_matches = !unit_name.is_empty() && fetched_names.contains(&unit_name);
        let member_matches: Vec<_> = unit
            .members
            .iter()
            .filter_map(|member| {
                let object_name = member.object_name.trim().to_ascii_lowercase();
                fetched_names.contains(&object_name).then_some(object_name)
            })
            .collect();
        if !unit_matches && member_matches.is_empty() {
            continue;
        }

        if unit_matches {
            matches.matched_names.insert(unit_name);
        }
        matches.matched_names.extend(member_matches);

        let root_file = normalize_targeted_refresh_manifest_path(&unit.root_file);
        if !root_file.is_empty() {
            matches
                .uris
                .insert(path_to_file_uri(&root_path.join(root_file)));
        }
        for member in &unit.members {
            let member_file = normalize_targeted_refresh_manifest_path(&member.file);
            if member_file.is_empty() {
                continue;
            }
            matches
                .uris
                .insert(path_to_file_uri(&root_path.join(member_file)));
        }
    }

    matches
}

fn workspace_remote_dependency_refresh_inputs(
    workspace: &WorkspaceState,
    params: &RemoteDependenciesUpdatedParams,
) -> Option<Vec<DocumentInput>> {
    let mut target_uris = HashSet::<String>::new();
    for uri in params
        .source_uris
        .iter()
        .map(|uri| normalize_lsp_uri(uri))
        .chain((!params.source_uri.is_empty()).then(|| normalize_lsp_uri(&params.source_uri)))
    {
        target_uris.insert(uri);
    }

    let fetched_names: HashSet<_> = params
        .fetched
        .iter()
        .map(|name| name.trim().to_ascii_lowercase())
        .filter(|name| !name.is_empty())
        .collect();
    let mut matched_names = HashSet::<String>::new();
    for uri in workspace.cache.uris() {
        let Some(snapshot) = workspace.cache.get(uri.as_ref()) else {
            continue;
        };
        let Some(object_name) = snapshot.object_name.as_ref() else {
            continue;
        };
        let normalized_name = object_name.trim().to_ascii_lowercase();
        if fetched_names.contains(&normalized_name) {
            matched_names.insert(normalized_name);
            target_uris.insert(uri.to_string());
        }
    }

    let manifest_matches = workspace_manifest_uris_for_object_names(workspace, &fetched_names);
    matched_names.extend(manifest_matches.matched_names);
    target_uris.extend(manifest_matches.uris);

    if !fetched_names.is_subset(&matched_names) {
        return None;
    }

    let mut uris: Vec<_> = target_uris.into_iter().collect();
    uris.sort();
    uris.into_iter()
        .map(|uri| {
            let current = workspace.cache.get(&uri);
            let manifest_info = workspace_manifest_document_info(workspace, &uri);
            let (version, source_text) = if let Some(overlay) = workspace.open_documents.get(&uri) {
                (overlay.version, overlay.text.to_string())
            } else if let Some(current) = current.as_ref() {
                (current.version, current.text.to_string())
            } else {
                let path = file_uri_to_path(&uri)?;
                (0, fs::read_to_string(&path).ok()?)
            };
            let (is_dependency, object_name) = if let Some(current) = current.as_ref() {
                (current.is_dependency, current.object_name.clone())
            } else {
                let info = manifest_info.as_ref()?;
                (info.is_dependency, info.object_name.clone())
            };
            let text = if uri.ends_with(".xml") {
                let info = manifest_info.as_ref()?;
                ddic_xml_to_abap_source(
                    info.unit_name.as_str(),
                    info.unit_kind.as_str(),
                    source_text.as_str(),
                )
                .unwrap_or(source_text)
            } else {
                source_text
            };
            Some(DocumentInput {
                uri: Arc::from(uri.as_str()),
                version,
                text: Arc::from(text),
                is_dependency,
                object_name,
            })
        })
        .collect()
}

fn remote_dependency_source_context_uris(
    workspace: &WorkspaceState,
    source_uri: &str,
) -> Vec<String> {
    let source_uri = normalize_lsp_uri(source_uri);
    let root_path = file_uri_to_path(&workspace.root_uri);
    let manifest = workspace.manifest.as_ref();

    let mut out = Vec::new();
    let mut seen = HashSet::new();
    let mut queue = VecDeque::from([source_uri]);
    while let Some(current_uri) = queue.pop_front() {
        if !seen.insert(current_uri.clone()) {
            continue;
        }
        out.push(current_uri.clone());
        if let (Some(manifest), Some(root_path)) = (manifest, root_path.as_deref()) {
            queue.extend(remote_dependency_manifest_parent_uris(
                manifest,
                root_path,
                &current_uri,
            ));
        }
        if let Some(parents) = workspace.dependency_parent_uris.get(&current_uri) {
            queue.extend(parents.iter().cloned());
        }
        queue.extend(infer_dependency_source_parent_uris(workspace, &current_uri));
    }
    out
}

fn infer_dependency_source_parent_uris(
    workspace: &WorkspaceState,
    source_uri: &str,
) -> Vec<String> {
    if !is_dependency_document_uri(source_uri) {
        return Vec::new();
    }

    let object_name = workspace
        .cache
        .get(source_uri)
        .and_then(|snapshot| snapshot.object_name.as_ref().map(|name| name.to_string()))
        .or_else(|| {
            dependency_artifact_for_uri(workspace, source_uri).map(|record| record.object_name)
        })
        .map(|name| name.trim().to_ascii_lowercase())
        .filter(|name| !name.is_empty());
    let Some(object_name) = object_name else {
        return Vec::new();
    };

    let mut parent_uris = Vec::new();
    for uri in workspace.cache.uris() {
        if uri.as_ref() == source_uri || workspace_uri_is_dependency_source(workspace, uri.as_ref())
        {
            continue;
        }
        let Some(snapshot) = workspace.cache.get(uri.as_ref()) else {
            continue;
        };
        if unit_references_object(snapshot.as_ref(), object_name.as_str()) {
            parent_uris.push(uri.to_string());
        }
    }
    parent_uris.sort();
    parent_uris
}

fn unit_references_object(snapshot: &AnalysisSnapshot, object_name: &str) -> bool {
    let references = snapshot.symbols.semantic().refs();
    if references
        .all()
        .any(|reference| reference.name.eq_ignore_ascii_case(object_name))
    {
        return true;
    }

    if snapshot
        .symbols
        .include_edges
        .iter()
        .any(|edge| edge.name.eq_ignore_ascii_case(object_name))
    {
        return true;
    }

    if snapshot
        .symbols
        .sql_sources
        .iter()
        .any(|source| source.name.eq_ignore_ascii_case(object_name))
    {
        return true;
    }

    snapshot
        .symbols
        .call_sites
        .iter()
        .any(|call_site| match &call_site.target {
            NamedArgumentTarget::Constructor { type_name } => {
                type_name.eq_ignore_ascii_case(object_name)
            }
            NamedArgumentTarget::Function { function_name } => {
                function_name.eq_ignore_ascii_case(object_name)
            }
            NamedArgumentTarget::Report { report_name } => {
                report_name.eq_ignore_ascii_case(object_name)
            }
            NamedArgumentTarget::Routine { routine_name } => {
                routine_name.eq_ignore_ascii_case(object_name)
            }
            NamedArgumentTarget::ImplicitMethod { method_name } => {
                method_name.eq_ignore_ascii_case(object_name)
            }
            NamedArgumentTarget::Method { base_name, .. } => {
                base_name.eq_ignore_ascii_case(object_name)
            }
            NamedArgumentTarget::Event {
                qualifier,
                event_name,
            } => {
                qualifier
                    .as_ref()
                    .is_some_and(|qualifier| qualifier.eq_ignore_ascii_case(object_name))
                    || event_name.eq_ignore_ascii_case(object_name)
            }
        })
}

fn remote_dependency_manifest_parent_uris(
    manifest: &WorkspaceManifest,
    root_path: &Path,
    uri: &str,
) -> Vec<String> {
    let mut out = Vec::new();
    let mut seen = HashSet::new();

    for unit in &manifest.units {
        let root_file = normalize_targeted_refresh_manifest_path(&unit.root_file);
        let root_matches =
            !root_file.is_empty() && path_to_file_uri(&root_path.join(&root_file)) == uri;
        let member_matches = unit.members.iter().any(|member| {
            let member_file = normalize_targeted_refresh_manifest_path(&member.file);
            !member_file.is_empty() && path_to_file_uri(&root_path.join(member_file)) == uri
        });
        if !root_matches && !member_matches {
            continue;
        }

        for dependency in &unit.dependency_of {
            let dependency_file = normalize_targeted_refresh_manifest_path(&dependency.file);
            if dependency_file.is_empty() {
                continue;
            }
            let dependency_uri = path_to_file_uri(&root_path.join(dependency_file));
            if seen.insert(dependency_uri.clone()) {
                out.push(dependency_uri);
            }
        }
    }

    out
}

fn source_context_uses_local_exports(workspace: &WorkspaceState, source_uri: &str) -> bool {
    let Some(root_path) = file_uri_to_path(&workspace.root_uri) else {
        return false;
    };
    remote_dependency_source_context_uris(workspace, source_uri)
        .into_iter()
        .any(|uri| local_export_config_for_source(&root_path, &uri).uses_local_exports())
}

fn extend_remote_dependency_source_candidates(
    source_candidates: &mut HashMap<String, Vec<RemoteDependencyCandidate>>,
    source_uris: &mut Vec<String>,
    source_uri_seen: &mut HashSet<String>,
    context_uris: &[String],
    candidates: &[RemoteDependencyCandidate],
) {
    for context_uri in context_uris {
        if source_uri_seen.insert(context_uri.clone()) {
            source_uris.push(context_uri.clone());
        }
        source_candidates
            .entry(context_uri.clone())
            .or_default()
            .extend(candidates.iter().cloned());
    }
}

fn refresh_workspace_inputs_with_progress(
    workspace: &mut WorkspaceState,
    inputs: Vec<DocumentInput>,
    progress: Option<&(dyn Fn(usize, usize) + Sync)>,
) -> Vec<Arc<AnalysisSnapshot>> {
    if inputs.is_empty() {
        return Vec::new();
    }
    clear_workspace_dependency_candidate_caches(workspace);
    sync_workspace_lint_policy(workspace);
    let refreshed_uris: Vec<_> = inputs.iter().map(|input| Arc::clone(&input.uri)).collect();
    let build_plan = workspace_committed_build_plan(workspace);
    let snapshots = workspace
        .cache
        .publish_inputs_with_build_plan_and_progress(inputs, build_plan, progress);
    refreshed_uris
        .into_iter()
        .filter_map(|uri| snapshots.get(uri.as_ref()).cloned())
        .collect()
}

fn open_dependency_document_inputs(workspace: &WorkspaceState) -> Vec<DocumentInput> {
    workspace
        .open_documents
        .iter()
        .filter_map(|(uri, overlay)| {
            is_dependency_document_uri(uri)
                .then(|| {
                    dependency_document_input(
                        workspace,
                        uri,
                        overlay.version,
                        overlay.text.as_ref(),
                    )
                })
                .flatten()
        })
        .collect()
}

fn workspace_committed_build_plan(_workspace: &WorkspaceState) -> SnapshotBuildPlan {
    SnapshotBuildPlan::EDITOR_WORKSPACE
}

fn hydrate_workspace_dependency_documents(
    workspace: &mut WorkspaceState,
) -> HashMap<Arc<str>, Arc<AnalysisSnapshot>> {
    let total_start = Instant::now();
    let mut metrics = DependencyStoreHydrationMetrics::default();
    let hydrated = hydrate_workspace_dependency_documents_with_metrics(workspace, &mut metrics);
    metrics.elapsed = total_start.elapsed();
    workspace.dependency_store_hydration_metrics = Some(metrics);
    workspace.local_export_chain_candidates.clear();
    workspace.local_export_chain_refresh_candidates.clear();
    hydrated
}

fn hydrate_workspace_dependency_documents_with_metrics(
    workspace: &mut WorkspaceState,
    metrics: &mut DependencyStoreHydrationMetrics,
) -> HashMap<Arc<str>, Arc<AnalysisSnapshot>> {
    if !workspace_supports_dependency_store_resolution(workspace) {
        return HashMap::new();
    }
    metrics.supported = true;
    let Some(profile) = workspace_dependency_profile(workspace) else {
        return HashMap::new();
    };
    metrics.profile_present = true;
    let Some(store) = workspace_dependency_store(workspace) else {
        return HashMap::new();
    };
    let Some(reader) = store.reader().ok() else {
        return HashMap::new();
    };
    metrics.reader_available = true;

    let build_plan = workspace_committed_build_plan(workspace);
    let mut queried_candidates = HashSet::<String>::new();
    let mut scanned_candidate_sources = HashSet::<String>::new();
    let mut hydrated_uris = HashSet::<Arc<str>>::new();

    loop {
        metrics.iterations += 1;
        let mut inputs = Vec::<DocumentInput>::new();
        let mut input_uris = HashSet::<String>::new();

        let cache_uris = workspace.cache.uris();
        metrics.cache_uri_scans += cache_uris.len();
        for uri in cache_uris {
            let Some(snapshot) = workspace.cache.get(uri.as_ref()) else {
                continue;
            };
            let snapshot = Arc::clone(&snapshot);
            if snapshot.is_dependency && !workspace.open_documents.contains_key(uri.as_ref()) {
                metrics.dependency_snapshots_skipped += 1;
                continue;
            }
            if !scanned_candidate_sources.insert(uri.to_string()) {
                continue;
            }

            metrics.source_snapshots_examined += 1;
            let candidate_start = Instant::now();
            let mut candidates =
                if snapshot.is_dependency && workspace.open_documents.contains_key(uri.as_ref()) {
                    cached_dependency_batch_candidates(workspace, snapshot.as_ref())
                } else {
                    collect_remote_dependency_candidates_for_include_component(snapshot.as_ref())
                };
            candidates.extend(resolved_dependency_inheritance_candidates(
                workspace,
                snapshot.as_ref(),
            ));
            metrics.candidate_collection_micros += candidate_start.elapsed().as_micros();
            metrics.candidate_count += candidates.len();
            for candidate in candidates {
                let candidate_key = remote_candidate_key(&candidate);
                if !queried_candidates.insert(candidate_key) {
                    continue;
                }
                metrics.unique_candidate_queries += 1;
                let lookup_start = Instant::now();
                let record = reader
                    .find_artifact_for_candidate(
                        &profile,
                        candidate.name.as_str(),
                        candidate.kind.as_str(),
                    )
                    .ok()
                    .flatten();
                metrics.store_lookup_micros += lookup_start.elapsed().as_micros();
                let Some(record) = record else {
                    continue;
                };
                metrics.artifact_hits += 1;
                let mut records = VecDeque::from([(record, uri.to_string())]);
                let mut expanded_artifacts = HashSet::new();
                while let Some((record, parent_uri)) = records.pop_front() {
                    let input = workspace_dependency_document_input(workspace, &record);
                    record_dependency_parent_uri(workspace, input.uri.as_ref(), &parent_uri);
                    if workspace.cache.get(input.uri.as_ref()).is_some()
                        || !input_uris.insert(input.uri.to_string())
                    {
                        metrics.existing_or_duplicate_inputs += 1;
                    } else {
                        hydrated_uris.insert(Arc::clone(&input.uri));
                        inputs.push(input.clone());
                    }
                    if !expanded_artifacts.insert(record.artifact_id) {
                        continue;
                    }
                    let inheritance_candidates =
                        dependency_inheritance_candidates_from_record(&record);
                    metrics.candidate_count += inheritance_candidates.len();
                    for candidate in inheritance_candidates {
                        let candidate_key = remote_candidate_key(&candidate);
                        if !queried_candidates.insert(candidate_key) {
                            continue;
                        }
                        metrics.unique_candidate_queries += 1;
                        let lookup_start = Instant::now();
                        let inherited_record = reader
                            .find_artifact_for_candidate(
                                &profile,
                                candidate.name.as_str(),
                                candidate.kind.as_str(),
                            )
                            .ok()
                            .flatten();
                        metrics.store_lookup_micros += lookup_start.elapsed().as_micros();
                        let Some(inherited_record) = inherited_record else {
                            continue;
                        };
                        metrics.artifact_hits += 1;
                        records.push_back((inherited_record, input.uri.to_string()));
                    }
                }
            }
        }

        if inputs.is_empty() {
            break;
        }

        metrics.hydrated_input_count += inputs.len();
        metrics.published_batch_count += 1;
        let publish_start = Instant::now();
        workspace
            .cache
            .publish_inputs_with_build_plan(inputs, build_plan);
        metrics.publish_micros += publish_start.elapsed().as_micros();
    }

    let mut hydrated = HashMap::new();
    for uri in hydrated_uris {
        if let Some(snapshot) = workspace.cache.get(uri.as_ref()) {
            hydrated.insert(Arc::clone(&snapshot.uri), snapshot);
        }
    }
    hydrated
}

fn resolved_dependency_inheritance_candidates(
    workspace: &WorkspaceState,
    snapshot: &AnalysisSnapshot,
) -> Vec<RemoteDependencyCandidate> {
    let mut deduped = HashMap::new();
    for reference in snapshot.symbols.semantic().refs().all() {
        if !matches!(
            reference.kind,
            ReferenceKind::TypeRef | ReferenceKind::StaticTarget
        ) {
            continue;
        }
        let Some(Resolution::Symbol(handle)) = reference.resolution else {
            continue;
        };
        let Some(target_unit) = snapshot.project.units.get(handle.unit.as_usize()) else {
            continue;
        };
        if !workspace_uri_is_dependency_source(workspace, target_unit.uri.as_ref()) {
            continue;
        }
        let target_symbol = target_unit.symbol(handle.symbol);
        if !matches!(
            target_symbol.kind,
            SymbolKind::Class | SymbolKind::Interface
        ) {
            continue;
        }
        insert_dependency_inheritance_candidates(target_unit, handle.symbol, &mut deduped);
    }
    deduped.into_values().collect()
}

fn dependency_inheritance_candidates_from_record(
    record: &StoredArtifactRecord,
) -> Vec<RemoteDependencyCandidate> {
    if !matches!(
        record.object_kind.as_str(),
        "global-class" | "global-interface"
    ) {
        return Vec::new();
    }
    let analysis_text = analysis_text_for_document(record.source_text.as_str(), true);
    let parsed = parse(analysis_text.as_ref());
    let unit = analyze_unit(
        Arc::from(record.object_uri.as_str()),
        analysis_text.as_ref(),
        &parsed,
    );
    let mut deduped = HashMap::new();
    for inheritance in &unit.class_inheritance {
        insert_remote_candidate(
            &mut deduped,
            RemoteDependencyCandidate {
                name: inheritance.superclass_name.to_string(),
                kind: "type".to_string(),
            },
        );
    }
    for interface in &unit.implemented_interfaces {
        insert_remote_candidate(
            &mut deduped,
            RemoteDependencyCandidate {
                name: interface.interface_name.to_string(),
                kind: "type".to_string(),
            },
        );
    }
    deduped.into_values().collect()
}

fn insert_dependency_inheritance_candidates(
    unit: &abap_symbols::UnitAnalysis,
    owner_symbol: abap_symbols::SymbolId,
    deduped: &mut HashMap<String, RemoteDependencyCandidate>,
) {
    if let Some(inheritance) = unit.class_superclass(owner_symbol) {
        insert_remote_candidate(
            deduped,
            RemoteDependencyCandidate {
                name: inheritance.superclass_name.to_string(),
                kind: "type".to_string(),
            },
        );
    }
    for interface in &unit.implemented_interfaces {
        if interface.owner_symbol == owner_symbol {
            insert_remote_candidate(
                deduped,
                RemoteDependencyCandidate {
                    name: interface.interface_name.to_string(),
                    kind: "type".to_string(),
                },
            );
        }
    }
}

fn rebuild_workspace_cache_with_progress(
    workspace: &mut WorkspaceState,
    progress: Option<&(dyn Fn(usize, usize) + Sync)>,
) -> HashMap<Arc<str>, Arc<AnalysisSnapshot>> {
    let local_export_resolver = Arc::clone(&workspace.local_export_resolver);
    *local_export_resolver
        .lock()
        .unwrap_or_else(|error| error.into_inner()) = LocalExportResolver::default();
    let loaded = load_workspace_documents_with_progress(
        &workspace.root_uri,
        &workspace.open_documents,
        progress,
    );
    workspace.performance_mode =
        resolve_workspace_performance_mode(loaded.manifest.as_ref(), loaded.manifest_len_bytes);
    workspace.dependency_profile = loaded
        .manifest
        .as_ref()
        .and_then(|manifest| manifest.dependency_store.clone());
    workspace.manifest = loaded.manifest.clone();
    workspace.manifest_uri = loaded.manifest_uri.to_string();
    workspace.manifest_error = loaded.manifest_error.clone();
    workspace.manifest_diagnostics = loaded.manifest_diagnostics.clone();
    sync_workspace_lint_policy(workspace);
    workspace.dependency_parent_uris.clear();
    clear_workspace_dependency_candidate_caches(workspace);
    let documents = loaded.documents;
    let build_plan = workspace_committed_build_plan(workspace);
    let mut dependency_store_resolution = DependencyStoreResolutionContext::new(workspace);
    let mut snapshots = {
        let mut resolver = local_export_resolver
            .lock()
            .unwrap_or_else(|error| error.into_inner());
        if let Some(progress) = progress {
            let stage_count = documents.len();
            let analysis_progress = |processed: usize, _total: usize| {
                progress(
                    stage_count.saturating_add(processed),
                    stage_count.saturating_mul(2),
                );
            };
            replace_all_workspace_documents_with_dependency_resolution_for_build_plan(
                &workspace.cache,
                &loaded.root_path,
                dependency_store_resolution.as_mut(),
                Some(&mut resolver),
                &documents,
                build_plan,
                Some(&analysis_progress),
            )
        } else {
            replace_all_workspace_documents_with_dependency_resolution_for_build_plan(
                &workspace.cache,
                &loaded.root_path,
                dependency_store_resolution.as_mut(),
                Some(&mut resolver),
                &documents,
                build_plan,
                None,
            )
        }
    };
    let open_dependency_inputs = open_dependency_document_inputs(workspace);
    if !open_dependency_inputs.is_empty() {
        snapshots.extend(
            workspace
                .cache
                .publish_inputs_with_build_plan(open_dependency_inputs, build_plan),
        );
    }
    let hydrated = hydrate_workspace_dependency_documents(workspace);
    if !hydrated.is_empty() {
        snapshots.extend(hydrated);
        for uri in snapshots.keys().cloned().collect::<Vec<_>>() {
            if let Some(snapshot) = workspace.cache.get(uri.as_ref()) {
                snapshots.insert(uri, snapshot);
            }
        }
    }
    snapshots
}

fn uri_is_manifest_dependency(workspace: &WorkspaceState, uri: &str) -> bool {
    let Some(root_path) = file_uri_to_path(&workspace.root_uri) else {
        return false;
    };
    workspace.manifest.as_ref().is_some_and(|manifest| {
        manifest_document_metadata(&root_path, &workspace.root_uri, manifest, uri)
            .is_some_and(|(is_dependency, _)| is_dependency)
    })
}

pub fn workspace_uri_is_dependency_source(workspace: &WorkspaceState, uri: &str) -> bool {
    let uri = normalize_lsp_uri(uri);
    workspace
        .cache
        .get(&uri)
        .is_some_and(|snapshot| snapshot.is_dependency)
        || (workspace.open_documents.contains_key(&uri)
            && (is_dependency_document_uri(&uri) || uri_is_manifest_dependency(workspace, &uri)))
}

pub fn stage_workspace_preview_snapshot(
    state: &mut ServerState,
    uri: &str,
    version: i32,
    text: &str,
) -> bool {
    let normalized_uri = normalize_lsp_uri(uri);
    let Some((workspace_uri, preview)) = ({
        let Some(workspace) = state.workspace_for_uri_mut(&normalized_uri) else {
            return false;
        };
        if workspace.performance_mode == WorkspacePerformanceMode::EditorFirst
            && workspace.cache.len() == 0
        {
            return false;
        }
        let preview =
            incremental_workspace_document_input(workspace, &normalized_uri, version, text)
                .map(|input| workspace.cache.preview_publish_input(input))
                .unwrap_or_else(|| {
                    standalone_preview_snapshot(workspace, &normalized_uri, version, text)
                });
        let workspace_uri = workspace.root_uri.clone();
        workspace
            .preview_snapshots
            .insert(normalized_uri.clone(), Arc::clone(&preview));
        Some((workspace_uri, preview))
    }) else {
        return false;
    };
    let _ = preview;
    state.index_workspace_uri(&workspace_uri, &normalized_uri);
    true
}

fn standalone_preview_snapshot(
    workspace: &WorkspaceState,
    uri: &str,
    version: i32,
    text: &str,
) -> Arc<AnalysisSnapshot> {
    let store = DocumentStore::default();
    let preview = store.publish(uri.to_string(), version, text);
    let deps = message_class_preview_dependency_inputs(workspace, preview.as_ref());
    if deps.is_empty() {
        return preview;
    }

    let mut inputs = Vec::with_capacity(deps.len() + 1);
    inputs.push(DocumentInput {
        uri: Arc::from(uri),
        version,
        text: Arc::from(text),
        is_dependency: false,
        object_name: None,
    });
    inputs.extend(deps);
    store
        .replace_all_with_build_plan(inputs, workspace_committed_build_plan(workspace))
        .get(uri)
        .cloned()
        .unwrap_or(preview)
}

fn message_class_preview_dependency_inputs(
    workspace: &WorkspaceState,
    preview: &AnalysisSnapshot,
) -> Vec<DocumentInput> {
    if !workspace_supports_dependency_store_resolution(workspace) {
        return Vec::new();
    }
    let Some(profile) = workspace_dependency_profile(workspace) else {
        return Vec::new();
    };
    let Some(reader) = workspace_dependency_store(workspace).and_then(|store| store.reader().ok())
    else {
        return Vec::new();
    };

    let mut seen = HashSet::new();
    let mut inputs = Vec::new();
    for candidate in collect_remote_dependency_candidates(preview) {
        if candidate.kind != "message-class" {
            continue;
        }
        let Ok(Some(record)) = reader.find_artifact_for_candidate(
            &profile,
            candidate.name.as_str(),
            candidate.kind.as_str(),
        ) else {
            continue;
        };
        let input = workspace_dependency_document_input(workspace, &record);
        if seen.insert(input.uri.to_string()) {
            inputs.push(input);
        }
    }
    inputs
}

pub fn prune_workspace_preview_snapshots(workspace: &mut WorkspaceState) {
    workspace.preview_snapshots.retain(|uri, preview| {
        workspace
            .cache
            .get(uri)
            .is_none_or(|committed| committed.version < preview.version)
    });
}

fn incremental_workspace_document_input(
    workspace: &WorkspaceState,
    uri: &str,
    version: i32,
    text: &str,
) -> Option<DocumentInput> {
    if is_dependency_document_uri(uri) {
        return dependency_document_input(workspace, uri, version, text);
    }

    if !uri.ends_with(".abap") {
        return None;
    }

    if let Some(current) = workspace.cache.get(uri) {
        let is_dependency = current.is_dependency && uri_is_manifest_dependency(workspace, uri);
        return Some(DocumentInput {
            uri: Arc::clone(&current.uri),
            version,
            text: Arc::from(text),
            // Keep manifest-backed dependency files in dependency mode even while open, but let
            // local-export files promote to full analysis once the user opens them.
            is_dependency,
            object_name: current.object_name.clone(),
        });
    }

    if workspace.cache.len() == 0 {
        return None;
    }

    if !uri_starts_with_workspace(uri, &workspace.root_uri) {
        return None;
    }

    let root_path = file_uri_to_path(&workspace.root_uri)?;
    let manifest_metadata = workspace.manifest.as_ref().and_then(|manifest| {
        manifest_document_metadata(&root_path, &workspace.root_uri, manifest, uri)
    });

    if workspace.manifest.is_some() && manifest_metadata.is_none() {
        return None;
    }

    let (is_dependency, object_name) = manifest_metadata.unwrap_or((false, None));

    Some(DocumentInput {
        uri: Arc::from(uri),
        version,
        text: Arc::from(text),
        is_dependency,
        object_name,
    })
}

pub fn workspace_manifest_diagnostics_params(
    state: &ServerState,
    workspace_uri: &str,
) -> Option<PublishDiagnosticsParams> {
    let workspace_uri = normalize_lsp_uri(workspace_uri);
    let workspace = state.workspaces.get(&workspace_uri)?;
    if workspace.manifest_uri.is_empty() {
        return None;
    }

    let mut diagnostics = Vec::new();
    if let Some(message) = workspace.manifest_error.as_ref() {
        diagnostics.push(Diagnostic {
            range: Range {
                start: Position::new(0, 0),
                end: Position::new(0, 1),
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: Some("abap-lsp".to_string()),
            message: format!(
                "{message}. Manifest loading failed; workspace dependency resolution is disabled until abapls.toml is fixed."
            ),
            related_information: None,
            tags: None,
            data: None,
        });
    } else {
        diagnostics.extend(
            workspace
                .manifest_diagnostics
                .iter()
                .map(manifest_config_diagnostic_to_lsp),
        );
    }

    Some(PublishDiagnosticsParams {
        uri: Uri::from_str(&workspace.manifest_uri).ok()?,
        diagnostics,
        version: None,
    })
}

fn manifest_config_diagnostic_to_lsp(diagnostic: &ManifestDiagnostic) -> Diagnostic {
    Diagnostic {
        range: Range {
            start: Position::new(
                diagnostic.range.start_line,
                diagnostic.range.start_character,
            ),
            end: Position::new(diagnostic.range.end_line, diagnostic.range.end_character),
        },
        severity: Some(DiagnosticSeverity::WARNING),
        code: Some(NumberOrString::String("lint-config".to_string())),
        code_description: None,
        source: Some("abap-lsp".to_string()),
        message: diagnostic.message.clone(),
        related_information: None,
        tags: None,
        data: None,
    }
}

fn remote_candidate_key(candidate: &RemoteDependencyCandidate) -> String {
    format!(
        "{}:{}",
        candidate.kind.trim().to_ascii_lowercase(),
        candidate.name.trim().to_ascii_lowercase()
    )
}

pub fn publish_open_document(
    state: &ServerState,
    params: &DidOpenTextDocumentParams,
) -> Arc<AnalysisSnapshot> {
    let uri = normalize_lsp_uri(params.text_document.uri.as_str());
    state.cache.publish(
        uri,
        params.text_document.version,
        &params.text_document.text,
    )
}

pub fn publish_open_document_mut(
    state: &mut ServerState,
    params: &DidOpenTextDocumentParams,
) -> Arc<AnalysisSnapshot> {
    publish_open_document_mut_with_progress(state, params, None)
}

fn publish_workspace_input_with_dependency_hydration(
    workspace: &mut WorkspaceState,
    input: DocumentInput,
    build_plan: SnapshotBuildPlan,
) -> Arc<AnalysisSnapshot> {
    clear_workspace_dependency_candidate_caches(workspace);
    sync_workspace_lint_policy(workspace);
    let uri = Arc::clone(&input.uri);
    let snapshot = workspace
        .cache
        .publish_input_with_build_plan(input, build_plan);

    let _ = hydrate_workspace_dependency_documents(workspace);
    workspace.cache.get(uri.as_ref()).unwrap_or(snapshot)
}

pub fn publish_open_document_mut_with_progress(
    state: &mut ServerState,
    params: &DidOpenTextDocumentParams,
    progress: Option<&(dyn Fn(usize, usize) + Sync)>,
) -> Arc<AnalysisSnapshot> {
    let uri = normalize_lsp_uri(params.text_document.uri.as_str());
    let workspace_result = if let Some(workspace) = state.workspace_for_uri_mut(&uri) {
        let manifest_dependency_open = uri_is_manifest_dependency(workspace, &uri);
        let workspace_uri = workspace.root_uri.clone();
        let build_plan = workspace_committed_build_plan(workspace);
        if let Some(current) = workspace
            .cache
            .get(&uri)
            .filter(|snapshot| snapshot.text.as_ref() == params.text_document.text.as_str())
        {
            let uses_local_exports = document_uses_local_exports(workspace, &uri);
            workspace.open_documents.insert(
                uri.clone(),
                OpenDocumentOverlay {
                    version: params.text_document.version,
                    text: Arc::from(params.text_document.text.as_str()),
                },
            );
            if manifest_dependency_open {
                let snapshots = rebuild_workspace_cache_with_progress(workspace, progress);
                Some((
                    workspace_uri,
                    snapshots
                        .get(uri.as_str())
                        .cloned()
                        .expect("opened manifest dependency should exist after rebuild"),
                    true,
                ))
            } else if uses_local_exports {
                let snapshot = snapshot_with_version(&current, params.text_document.version);
                workspace.cache.insert_snapshot(Arc::clone(&snapshot));
                Some((workspace_uri, snapshot, false))
            } else if let Some(input) = incremental_workspace_document_input(
                workspace,
                &uri,
                params.text_document.version,
                &params.text_document.text,
            ) && (current.is_dependency != input.is_dependency
                || current.object_name != input.object_name)
            {
                Some((
                    workspace_uri,
                    publish_workspace_input_with_dependency_hydration(workspace, input, build_plan),
                    false,
                ))
            } else {
                let snapshot = snapshot_with_version(&current, params.text_document.version);
                workspace.cache.insert_snapshot(Arc::clone(&snapshot));
                Some((workspace_uri, snapshot, false))
            }
        } else {
            workspace.open_documents.insert(
                uri.clone(),
                OpenDocumentOverlay {
                    version: params.text_document.version,
                    text: Arc::from(params.text_document.text.as_str()),
                },
            );
            if manifest_dependency_open {
                let snapshots = rebuild_workspace_cache_with_progress(workspace, progress);
                Some((
                    workspace_uri,
                    snapshots
                        .get(uri.as_str())
                        .cloned()
                        .expect("opened manifest dependency should exist after rebuild"),
                    true,
                ))
            } else if document_uses_local_exports(workspace, &uri) {
                if let Some(input) = incremental_workspace_document_input(
                    workspace,
                    &uri,
                    params.text_document.version,
                    &params.text_document.text,
                ) {
                    Some((
                        workspace_uri,
                        publish_workspace_input_with_local_export_resolution(
                            workspace, input, progress,
                        ),
                        false,
                    ))
                } else {
                    let snapshots = rebuild_workspace_cache_with_progress(workspace, progress);
                    Some((
                        workspace_uri,
                        snapshots.get(uri.as_str()).cloned().expect(
                            "opened workspace document should exist after local export rebuild",
                        ),
                        true,
                    ))
                }
            } else if let Some(input) = incremental_workspace_document_input(
                workspace,
                &uri,
                params.text_document.version,
                &params.text_document.text,
            ) {
                Some((
                    workspace_uri,
                    publish_workspace_input_with_dependency_hydration(workspace, input, build_plan),
                    false,
                ))
            } else {
                let snapshots = rebuild_workspace_cache_with_progress(workspace, progress);
                Some((
                    workspace_uri,
                    snapshots
                        .get(uri.as_str())
                        .cloned()
                        .expect("opened workspace document should exist after rebuild"),
                    true,
                ))
            }
        }
    } else {
        None
    };
    if let Some((workspace_uri, snapshot, reindex_members)) = workspace_result {
        if reindex_members {
            state.index_workspace_members(&workspace_uri);
        } else {
            state.index_workspace_uri(&workspace_uri, &uri);
        }
        return snapshot;
    }
    publish_standalone_external_document(
        state,
        Arc::from(uri.as_str()),
        params.text_document.version,
        &params.text_document.text,
    )
}

pub fn publish_changed_document(
    state: &ServerState,
    params: &DidChangeTextDocumentParams,
) -> Option<Arc<AnalysisSnapshot>> {
    let change = params.content_changes.last()?;
    let uri = normalize_lsp_uri(params.text_document.uri.as_str());
    Some(
        state
            .cache
            .publish(uri, params.text_document.version, &change.text),
    )
}

fn publish_standalone_external_document(
    state: &ServerState,
    uri: Arc<str>,
    version: i32,
    text: &str,
) -> Arc<AnalysisSnapshot> {
    let is_dependency = uri_is_known_local_export_source(state, uri.as_ref());
    state.cache.publish_input_with_build_plan(
        DocumentInput {
            uri,
            version,
            text: Arc::from(text),
            is_dependency,
            object_name: None,
        },
        SnapshotBuildPlan::EFFECTIVE_SOURCE,
    )
}

fn uri_is_known_local_export_source(state: &ServerState, uri: &str) -> bool {
    let Some(path) = file_uri_to_path(uri) else {
        return false;
    };
    state.workspaces.values().any(|workspace| {
        let Some(root_path) = file_uri_to_path(&workspace.root_uri) else {
            return false;
        };
        workspace.cache.uris().into_iter().any(|source_uri| {
            workspace
                .cache
                .get(source_uri.as_ref())
                .is_some_and(|snapshot| !snapshot.is_dependency)
                && local_export_config_for_source(&root_path, source_uri.as_ref())
                    .roots
                    .iter()
                    .any(|root| path_is_under_root(&path, root))
        })
    })
}

fn path_is_under_root(path: &Path, root: &Path) -> bool {
    if path.starts_with(root) {
        return true;
    }
    let path = path
        .to_string_lossy()
        .replace('\\', "/")
        .to_ascii_lowercase();
    let root = root
        .to_string_lossy()
        .replace('\\', "/")
        .trim_end_matches('/')
        .to_ascii_lowercase();
    path == root || path.starts_with(&format!("{root}/"))
}

pub fn publish_changed_document_mut(
    state: &mut ServerState,
    params: &DidChangeTextDocumentParams,
) -> Option<Arc<AnalysisSnapshot>> {
    publish_changed_document_mut_with_progress(state, params, None)
}

pub fn publish_changed_document_mut_with_progress(
    state: &mut ServerState,
    params: &DidChangeTextDocumentParams,
    progress: Option<&(dyn Fn(usize, usize) + Sync)>,
) -> Option<Arc<AnalysisSnapshot>> {
    let change = params.content_changes.last()?;
    let uri = normalize_lsp_uri(params.text_document.uri.as_str());
    let workspace_result = if let Some(workspace) = state.workspace_for_uri_mut(&uri) {
        let workspace_uri = workspace.root_uri.clone();
        let build_plan = workspace_committed_build_plan(workspace);
        if let Some(current) = workspace
            .cache
            .get(&uri)
            .filter(|snapshot| snapshot.text.as_ref() == change.text.as_str())
        {
            let uses_local_exports = document_uses_local_exports(workspace, &uri);
            workspace.open_documents.insert(
                uri.clone(),
                OpenDocumentOverlay {
                    version: params.text_document.version,
                    text: Arc::from(change.text.as_str()),
                },
            );
            if uses_local_exports {
                let snapshot = snapshot_with_version(&current, params.text_document.version);
                workspace.cache.insert_snapshot(Arc::clone(&snapshot));
                Some((workspace_uri, Some(snapshot), false))
            } else if let Some(input) = incremental_workspace_document_input(
                workspace,
                &uri,
                params.text_document.version,
                &change.text,
            ) && (current.is_dependency != input.is_dependency
                || current.object_name != input.object_name)
            {
                Some((
                    workspace_uri,
                    Some(publish_workspace_input_with_dependency_hydration(
                        workspace, input, build_plan,
                    )),
                    false,
                ))
            } else {
                let snapshot = snapshot_with_version(&current, params.text_document.version);
                workspace.cache.insert_snapshot(Arc::clone(&snapshot));
                Some((workspace_uri, Some(snapshot), false))
            }
        } else {
            workspace.open_documents.insert(
                uri.clone(),
                OpenDocumentOverlay {
                    version: params.text_document.version,
                    text: Arc::from(change.text.as_str()),
                },
            );
            if document_uses_local_exports(workspace, &uri) {
                if let Some(input) = incremental_workspace_document_input(
                    workspace,
                    &uri,
                    params.text_document.version,
                    &change.text,
                ) {
                    Some((
                        workspace_uri,
                        Some(publish_workspace_input_with_local_export_resolution(
                            workspace, input, progress,
                        )),
                        false,
                    ))
                } else {
                    let snapshots = rebuild_workspace_cache_with_progress(workspace, progress);
                    Some((workspace_uri, snapshots.get(uri.as_str()).cloned(), true))
                }
            } else if let Some(input) = incremental_workspace_document_input(
                workspace,
                &uri,
                params.text_document.version,
                &change.text,
            ) {
                Some((
                    workspace_uri,
                    Some(publish_workspace_input_with_dependency_hydration(
                        workspace, input, build_plan,
                    )),
                    false,
                ))
            } else {
                let snapshots = rebuild_workspace_cache_with_progress(workspace, progress);
                Some((workspace_uri, snapshots.get(uri.as_str()).cloned(), true))
            }
        }
    } else {
        None
    };
    if let Some((workspace_uri, snapshot, reindex_members)) = workspace_result {
        if reindex_members {
            state.index_workspace_members(&workspace_uri);
        } else {
            state.index_workspace_uri(&workspace_uri, &uri);
        }
        return snapshot;
    }
    Some(publish_standalone_external_document(
        state,
        Arc::from(uri.as_str()),
        params.text_document.version,
        &change.text,
    ))
}

pub fn refresh_workspace(
    state: &mut ServerState,
    workspace_uri: &str,
) -> Vec<Arc<AnalysisSnapshot>> {
    refresh_workspace_with_progress(state, workspace_uri, None)
}

pub fn refresh_workspace_with_progress(
    state: &mut ServerState,
    workspace_uri: &str,
    progress: Option<&(dyn Fn(usize, usize) + Sync)>,
) -> Vec<Arc<AnalysisSnapshot>> {
    let workspace_uri = normalize_lsp_uri(workspace_uri);
    let Some(workspace) = state.workspaces.get_mut(&workspace_uri) else {
        return Vec::new();
    };
    let snapshots: Vec<_> = rebuild_workspace_cache_with_progress(workspace, progress)
        .into_values()
        .collect();
    let _ = workspace;
    state.index_workspace_members(&workspace_uri);
    snapshots
}

pub fn handle_workspace_manifest_updated(
    state: &mut ServerState,
    params: &WorkspaceManifestUpdatedParams,
) -> Vec<Arc<AnalysisSnapshot>> {
    handle_workspace_manifest_updated_with_progress(state, params, None)
}

pub fn handle_workspace_manifest_updated_with_progress(
    state: &mut ServerState,
    params: &WorkspaceManifestUpdatedParams,
    progress: Option<&(dyn Fn(usize, usize) + Sync)>,
) -> Vec<Arc<AnalysisSnapshot>> {
    refresh_workspace_with_progress(state, &params.workspace_uri, progress)
}

pub fn handle_dependency_cache_refresh_requested(
    state: &mut ServerState,
    params: &WorkspaceManifestUpdatedParams,
) -> Vec<Arc<AnalysisSnapshot>> {
    handle_dependency_cache_refresh_requested_with_progress(state, params, None)
}

pub fn handle_dependency_cache_refresh_requested_with_progress(
    state: &mut ServerState,
    params: &WorkspaceManifestUpdatedParams,
    progress: Option<&(dyn Fn(usize, usize) + Sync)>,
) -> Vec<Arc<AnalysisSnapshot>> {
    let workspace_uri = normalize_lsp_uri(&params.workspace_uri);
    if let Some(workspace) = state.workspaces.get_mut(&workspace_uri) {
        workspace.remote_resolution_seen.clear();
        workspace.remote_lookup_failures.clear();
        workspace.remote_resolution_in_flight = false;
    }
    refresh_workspace_with_progress(state, &workspace_uri, progress)
}

pub fn handle_remote_dependencies_updated(
    state: &mut ServerState,
    params: &RemoteDependenciesUpdatedParams,
) -> Vec<Arc<AnalysisSnapshot>> {
    handle_remote_dependencies_updated_with_progress(state, params, None)
}

fn canonicalize_dependency_artifact_source(artifact: &DependencyArtifactPayload) -> String {
    let kind = artifact.object_kind.trim().to_ascii_lowercase();
    let file_extension = artifact.file_extension.trim().to_ascii_lowercase();
    if file_extension == "xml" || kind == "message-class" || kind.starts_with("ddic-") {
        ddic_xml_to_abap_source(
            artifact.object_name.as_str(),
            artifact.object_kind.as_str(),
            artifact.source_text.as_str(),
        )
        .unwrap_or_else(|| artifact.source_text.clone())
    } else {
        artifact.source_text.clone()
    }
}

fn stored_symbol_priority(kind: abap_symbols::SymbolKind) -> i64 {
    match kind {
        abap_symbols::SymbolKind::Class
        | abap_symbols::SymbolKind::Interface
        | abap_symbols::SymbolKind::Report
        | abap_symbols::SymbolKind::Include
        | abap_symbols::SymbolKind::Form
        | abap_symbols::SymbolKind::TypeDef => 100,
        abap_symbols::SymbolKind::Method
        | abap_symbols::SymbolKind::Event
        | abap_symbols::SymbolKind::Module => 90,
        _ => 50,
    }
}

fn should_index_dependency_symbol(
    unit: &abap_symbols::UnitAnalysis,
    symbol: &abap_symbols::SymbolData,
) -> bool {
    if symbol.scope != unit.root_scope {
        return false;
    }
    matches!(
        symbol.kind,
        abap_symbols::SymbolKind::Class
            | abap_symbols::SymbolKind::Interface
            | abap_symbols::SymbolKind::TypeDef
            | abap_symbols::SymbolKind::Include
            | abap_symbols::SymbolKind::Form
            | abap_symbols::SymbolKind::Module
            | abap_symbols::SymbolKind::Report
            | abap_symbols::SymbolKind::Variable
            | abap_symbols::SymbolKind::Constant
            | abap_symbols::SymbolKind::EnumMember
            | abap_symbols::SymbolKind::Event
    )
}

fn extract_stored_dependency_symbols(object_uri: &str, text: &str) -> Vec<StoredSymbolInput> {
    let parsed = parse(text);
    let unit = analyze_unit(Arc::<str>::from(object_uri), text, &parsed);
    let mut out = Vec::<StoredSymbolInput>::new();
    let mut seen = HashSet::<(String, usize, usize)>::new();

    for symbol in &unit.symbols {
        if !should_index_dependency_symbol(&unit, symbol) {
            continue;
        }
        let symbol_name = symbol.name.trim().to_ascii_lowercase();
        if symbol_name.is_empty() {
            continue;
        }
        let key = (
            symbol_name.clone(),
            symbol.decl_range.start,
            symbol.decl_range.end,
        );
        if !seen.insert(key) {
            continue;
        }
        out.push(StoredSymbolInput {
            symbol_name,
            symbol_kind: if symbol.kind == abap_symbols::SymbolKind::EnumMember {
                "constant".to_string()
            } else {
                format!("{:?}", symbol.kind).to_ascii_lowercase()
            },
            range_start: symbol.decl_range.start,
            range_end: symbol.decl_range.end,
            priority: stored_symbol_priority(symbol.kind),
        });
    }

    for member in &unit.class_members {
        if member.visibility == abap_symbols::Visibility::Private {
            continue;
        }
        let symbol_name = member.name.trim().to_ascii_lowercase();
        if symbol_name.is_empty() {
            continue;
        }
        let key = (
            symbol_name.clone(),
            member.decl_range.start,
            member.decl_range.end,
        );
        if !seen.insert(key) {
            continue;
        }
        out.push(StoredSymbolInput {
            symbol_name,
            symbol_kind: "class-member".to_string(),
            range_start: member.decl_range.start,
            range_end: member.decl_range.end,
            priority: 80,
        });
        if let Some(implementation) = member.implementation.as_ref() {
            let range = method_implementation_name_range(
                member.name.as_ref(),
                implementation.range.clone(),
            );
            out.push(StoredSymbolInput {
                symbol_name: member.name.trim().to_ascii_lowercase(),
                symbol_kind: "class-member-implementation".to_string(),
                range_start: range.start,
                range_end: range.end,
                priority: 90,
            });
        }
    }

    for function_module in &unit.function_modules {
        let symbol = unit.symbol(function_module.symbol);
        let symbol_name = symbol.name.trim().to_ascii_lowercase();
        if symbol_name.is_empty() {
            continue;
        }
        let key = (
            symbol_name.clone(),
            symbol.decl_range.start,
            symbol.decl_range.end,
        );
        if !seen.insert(key) {
            continue;
        }
        out.push(StoredSymbolInput {
            symbol_name,
            symbol_kind: "function-module".to_string(),
            range_start: symbol.decl_range.start,
            range_end: symbol.decl_range.end,
            priority: 100,
        });
    }

    out
}

pub fn store_remote_dependency_artifacts(
    state: &mut ServerState,
    params: &StoreRemoteDependencyArtifactsParams,
) -> Result<(), String> {
    let workspace_uri = normalize_lsp_uri(&params.workspace_uri);
    let Some(workspace) = state.workspaces.get_mut(&workspace_uri) else {
        return Err(format!("unknown workspace: {}", params.workspace_uri));
    };
    let Some(profile) = workspace_dependency_profile(workspace) else {
        return Err(
            "dependency store profile is missing; configure [dependency_store] in abapls.toml"
                .to_string(),
        );
    };
    let Some(store) = workspace_dependency_store(workspace) else {
        return Err("dependency store is unavailable".to_string());
    };

    let artifacts = params
        .artifacts
        .iter()
        .map(|artifact| {
            let source_text = canonicalize_dependency_artifact_source(artifact);
            StoredArtifactInput {
                package_name: artifact.package_name.clone(),
                object_kind: artifact.object_kind.clone(),
                object_name: artifact.object_name.clone(),
                object_uri: artifact.object_uri.clone(),
                object_type: artifact.object_type.clone(),
                description: artifact.description.clone(),
                file_extension: "abap".to_string(),
                source_text: source_text.clone(),
                fetched_at: artifact.fetched_at.clone(),
                symbols: extract_stored_dependency_symbols(
                    artifact.object_uri.as_str(),
                    source_text.as_str(),
                ),
            }
        })
        .collect::<Vec<_>>();
    let artifact_ids = store
        .put_artifacts(&profile, &artifacts)
        .map_err(|error| error.to_string())?;

    let connection_key = params
        .connection_key
        .as_deref()
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .map(str::to_string)
        .unwrap_or_else(|| workspace_dependency_connection_key(workspace));
    for candidate in &params.negative {
        store
            .record_negative_lookup(
                &profile,
                &connection_key,
                candidate.name.as_str(),
                candidate.kind.as_str(),
                "remote-fetch-failed",
            )
            .map_err(|error| error.to_string())?;
    }

    if !artifacts.is_empty() {
        clear_workspace_dependency_candidate_caches(workspace);
        let inputs = artifact_ids
            .into_iter()
            .zip(artifacts.iter())
            .map(|(artifact_id, artifact)| {
                workspace_dependency_document_input_from_payload_with_kind(
                    workspace,
                    artifact_id,
                    artifact,
                )
            })
            .collect::<Vec<_>>();
        workspace
            .cache
            .publish_inputs_with_build_plan(inputs, workspace_committed_build_plan(workspace));
        let _ = hydrate_workspace_dependency_documents(workspace);
    }

    let _ = workspace;
    state.index_workspace_members(&workspace_uri);
    Ok(())
}

pub fn read_dependency_document(
    state: &ServerState,
    params: &ReadDependencyDocumentParams,
) -> Result<Option<ReadDependencyDocumentResult>, String> {
    let uri = normalize_lsp_uri(&params.uri);
    let Some(workspace) = dependency_workspace_for_uri(state, &uri) else {
        return Err(format!(
            "dependency document does not map to an active workspace: {uri}"
        ));
    };
    let record = dependency_artifact_for_uri(workspace, &uri);
    let Some(record) = record else {
        return Err(format!("dependency artifact is missing for uri: {uri}"));
    };
    Ok(Some(ReadDependencyDocumentResult {
        source_text: record.source_text,
    }))
}

pub fn handle_remote_dependencies_updated_with_progress(
    state: &mut ServerState,
    params: &RemoteDependenciesUpdatedParams,
    progress: Option<&(dyn Fn(usize, usize) + Sync)>,
) -> Vec<Arc<AnalysisSnapshot>> {
    let workspace_uri = normalize_lsp_uri(&params.workspace_uri);
    let targeted_refresh = if let Some(workspace) = state.workspaces.get_mut(&workspace_uri) {
        workspace.remote_resolution_in_flight = false;
        workspace.remote_resolution_seen.clear();
        if !params.fetched.is_empty() {
            prime_workspace_manifest_state(workspace);
            let source_uris = if params.source_uris.is_empty() {
                vec![normalize_lsp_uri(&params.source_uri)]
            } else {
                params.source_uris.clone()
            };
            record_fetched_dependency_parent_uris(workspace, &params.fetched, &source_uris);
        }
        for name in &params.fetched {
            workspace
                .remote_lookup_failures
                .remove(&remote_candidate_key(&RemoteDependencyCandidate {
                    name: name.clone(),
                    kind: "type".to_string(),
                }));
        }
        for candidate in &params.failed {
            workspace
                .remote_lookup_failures
                .insert(remote_candidate_key(candidate));
        }
        workspace_remote_dependency_refresh_inputs(workspace, params)
            .map(|inputs| refresh_workspace_inputs_with_progress(workspace, inputs, progress))
    } else {
        return Vec::new();
    };

    targeted_refresh
        .unwrap_or_else(|| refresh_workspace_with_progress(state, &params.workspace_uri, progress))
}

pub fn handle_sap_atc_results_updated(
    state: &mut ServerState,
    params: &SapAtcResultsUpdatedParams,
) -> Vec<Arc<AnalysisSnapshot>> {
    let workspace_uri = normalize_lsp_uri(&params.workspace_uri);
    let source_uri = normalize_lsp_uri(&params.source_uri);
    let Some(workspace) = state.workspaces.get_mut(&workspace_uri) else {
        return Vec::new();
    };

    let snapshot = workspace.cache.get(&source_uri);
    let object_name = normalized_sap_atc_object_name(
        &params.object_name,
        snapshot
            .as_ref()
            .and_then(|snapshot| snapshot.object_name.as_deref()),
    );
    let check_variant = normalized_sap_atc_check_variant(&params.check_variant);
    let configuration = normalized_sap_atc_configuration(params.configuration.as_deref());
    let key = SapAtcCacheKey {
        source_uri: source_uri.clone(),
        document_version: params.document_version,
        object_name: object_name.clone(),
        check_variant: check_variant.clone(),
        configuration: configuration.clone(),
    };
    workspace.sap_atc_results.insert(
        key,
        SapAtcCachedResult {
            source_uri,
            document_version: params.document_version,
            object_name,
            check_variant,
            configuration,
            fetched_at: params.fetched_at.trim().to_string(),
            findings: params.findings.clone(),
        },
    );

    snapshot.into_iter().collect()
}

fn normalized_sap_atc_object_name(value: &str, fallback: Option<&str>) -> String {
    let value = value.trim();
    if value.is_empty() {
        fallback.unwrap_or_default().trim().to_ascii_uppercase()
    } else {
        value.to_ascii_uppercase()
    }
}

fn normalized_sap_atc_check_variant(value: &str) -> String {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        default_sap_atc_check_variant()
    } else {
        trimmed.to_ascii_uppercase()
    }
}

fn normalized_sap_atc_configuration(value: Option<&str>) -> Option<String> {
    value
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .map(str::to_string)
}

pub fn collect_remote_dependency_candidates(
    snapshot: &AnalysisSnapshot,
) -> Vec<RemoteDependencyCandidate> {
    collect_remote_dependency_candidates_for_unit(&snapshot.symbols)
}

fn collect_remote_dependency_candidates_for_request(
    workspace: &mut WorkspaceState,
    snapshot: &AnalysisSnapshot,
    source_uri: &str,
) -> Vec<RemoteDependencyCandidate> {
    if snapshot.is_dependency && workspace.open_documents.contains_key(source_uri) {
        return cached_dependency_batch_candidates(workspace, snapshot);
    }
    if !snapshot.is_dependency && document_uses_local_exports(workspace, source_uri) {
        return collect_remote_dependency_candidates_for_local_export_chain(workspace, source_uri);
    }
    collect_remote_dependency_candidates_for_include_component(snapshot)
}

fn collect_remote_dependency_candidates_for_batch(
    workspace: &mut WorkspaceState,
    snapshot: &AnalysisSnapshot,
    source_uri: &str,
) -> Vec<RemoteDependencyCandidate> {
    if snapshot.is_dependency {
        // Background dependency batches need the full dependency text so transitive
        // implementation refs are discovered even when the dependency file stays closed.
        return cached_dependency_batch_candidates(workspace, snapshot);
    }
    if document_uses_local_exports(workspace, source_uri) {
        return collect_remote_dependency_candidates_for_local_export_chain(workspace, source_uri);
    }
    collect_remote_dependency_candidates_for_include_component(snapshot)
}

fn collect_remote_dependency_refresh_candidates_for_batch(
    workspace: &mut WorkspaceState,
    snapshot: &AnalysisSnapshot,
    source_uri: &str,
) -> Vec<RemoteDependencyCandidate> {
    if document_uses_local_exports(workspace, source_uri) {
        return collect_remote_dependency_refresh_candidates_for_local_export_chain(
            workspace, source_uri,
        );
    }

    collect_remote_dependency_refresh_candidates_for_include_component(workspace, snapshot)
}

fn local_export_chain_candidate_cache_key(
    workspace: &WorkspaceState,
    source_uri: &str,
    roots: &[PathBuf],
) -> String {
    let roots_key = roots
        .iter()
        .map(|root| {
            root.to_string_lossy()
                .replace('\\', "/")
                .to_ascii_lowercase()
        })
        .collect::<Vec<_>>()
        .join(";");
    let mut component_uris = workspace
        .cache
        .get(source_uri)
        .map(|snapshot| {
            include_component_unit_ids(snapshot.as_ref())
                .into_iter()
                .filter_map(|unit_id| {
                    snapshot
                        .project
                        .units
                        .get(unit_id.as_usize())
                        .map(|unit| unit.uri.to_string())
                })
                .collect::<Vec<_>>()
        })
        .filter(|uris| !uris.is_empty())
        .unwrap_or_else(|| vec![source_uri.to_string()]);
    component_uris.sort();
    component_uris.dedup();
    format!("{roots_key}|{}", component_uris.join(";"))
}

fn collect_remote_dependency_candidates_for_local_export_chain(
    workspace: &mut WorkspaceState,
    source_uri: &str,
) -> Vec<RemoteDependencyCandidate> {
    let Some(root_path) = file_uri_to_path(&workspace.root_uri) else {
        return workspace
            .cache
            .get(source_uri)
            .map(|snapshot| {
                collect_remote_dependency_candidates_for_include_component(snapshot.as_ref())
            })
            .unwrap_or_default();
    };
    let config = local_export_config_for_source(&root_path, source_uri);
    if !config.uses_local_exports() {
        return workspace
            .cache
            .get(source_uri)
            .map(|snapshot| {
                collect_remote_dependency_candidates_for_include_component(snapshot.as_ref())
            })
            .unwrap_or_default();
    }

    let cache_key =
        local_export_chain_candidate_cache_key(workspace, source_uri, config.roots.as_slice());
    if let Some(cached) = workspace
        .local_export_chain_candidates
        .get(&cache_key)
        .cloned()
    {
        return cached;
    }

    let candidates =
        collect_remote_dependency_candidates_for_local_export_chain_uncached(workspace, source_uri);
    workspace
        .local_export_chain_candidates
        .insert(cache_key, candidates.clone());
    candidates
}

fn collect_remote_dependency_candidates_for_local_export_chain_uncached(
    workspace: &WorkspaceState,
    source_uri: &str,
) -> Vec<RemoteDependencyCandidate> {
    let mut deduped = HashMap::<String, RemoteDependencyCandidate>::new();
    let mut visited_uris = HashSet::from([source_uri.to_string()]);
    let mut queue = VecDeque::from([source_uri.to_string()]);
    let dependency_uri_by_name = workspace_dependency_uri_by_object_name(workspace);
    let local_export_config = file_uri_to_path(&workspace.root_uri)
        .map(|root_path| local_export_config_for_source(&root_path, source_uri))
        .filter(|config| config.uses_local_exports());
    let component_index = workspace
        .cache
        .get(source_uri)
        .map(|snapshot| IncludeComponentIndex::new(snapshot.project.as_ref()));

    while let Some(current_uri) = queue.pop_front() {
        let Some(snapshot) = workspace.cache.get(&current_uri) else {
            continue;
        };

        enqueue_resolved_local_export_dependency_uris(
            workspace,
            snapshot.as_ref(),
            &mut visited_uris,
            &mut queue,
        );
        for candidate in collect_remote_dependency_candidates_for_include_component_indexed(
            snapshot.as_ref(),
            component_index.as_ref(),
        ) {
            if let Some(dependency_uri) =
                dependency_uri_by_name.get(&candidate.name.trim().to_ascii_lowercase())
            {
                if visited_uris.insert(dependency_uri.clone())
                    && workspace.cache.get(dependency_uri).is_some()
                {
                    queue.push_back(dependency_uri.clone());
                }
                continue;
            }
            if local_export_config.as_ref().is_some_and(|config| {
                local_export_candidate_resolves_from_config(workspace, config, &candidate)
            }) {
                continue;
            }
            insert_remote_candidate(&mut deduped, candidate);
        }
    }

    deduped.into_values().collect()
}

fn local_export_candidate_resolves_from_config(
    workspace: &WorkspaceState,
    config: &LocalExportConfig,
    candidate: &RemoteDependencyCandidate,
) -> bool {
    let resolver = Arc::clone(&workspace.local_export_resolver);
    let mut resolver = resolver.lock().unwrap_or_else(|error| error.into_inner());
    resolve_local_export_dependency_document(
        &config.roots,
        &mut resolver,
        candidate.name.as_str(),
        candidate.kind.as_str(),
    )
    .is_some()
}

fn collect_remote_dependency_refresh_candidates_for_local_export_chain(
    workspace: &mut WorkspaceState,
    source_uri: &str,
) -> Vec<RemoteDependencyCandidate> {
    let Some(root_path) = file_uri_to_path(&workspace.root_uri) else {
        return workspace
            .cache
            .get(source_uri)
            .map(|snapshot| {
                collect_remote_dependency_refresh_candidates_for_include_component(
                    workspace,
                    snapshot.as_ref(),
                )
            })
            .unwrap_or_default();
    };
    let config = local_export_config_for_source(&root_path, source_uri);
    if !config.uses_local_exports() {
        return workspace
            .cache
            .get(source_uri)
            .map(|snapshot| {
                collect_remote_dependency_refresh_candidates_for_include_component(
                    workspace,
                    snapshot.as_ref(),
                )
            })
            .unwrap_or_default();
    }

    let cache_key =
        local_export_chain_candidate_cache_key(workspace, source_uri, config.roots.as_slice());
    if let Some(cached) = workspace
        .local_export_chain_refresh_candidates
        .get(&cache_key)
        .cloned()
    {
        return cached;
    }

    let candidates = collect_remote_dependency_refresh_candidates_for_local_export_chain_uncached(
        workspace, source_uri,
    );
    workspace
        .local_export_chain_refresh_candidates
        .insert(cache_key, candidates.clone());
    candidates
}

fn collect_remote_dependency_refresh_candidates_for_local_export_chain_uncached(
    workspace: &WorkspaceState,
    source_uri: &str,
) -> Vec<RemoteDependencyCandidate> {
    let mut deduped = HashMap::<String, RemoteDependencyCandidate>::new();
    let mut visited_uris = HashSet::from([source_uri.to_string()]);
    let mut queue = VecDeque::from([source_uri.to_string()]);
    let dependency_uri_by_name = workspace_dependency_uri_by_object_name(workspace);
    let component_index = workspace
        .cache
        .get(source_uri)
        .map(|snapshot| IncludeComponentIndex::new(snapshot.project.as_ref()));

    while let Some(current_uri) = queue.pop_front() {
        let Some(snapshot) = workspace.cache.get(&current_uri) else {
            continue;
        };

        enqueue_resolved_local_export_dependency_uris(
            workspace,
            snapshot.as_ref(),
            &mut visited_uris,
            &mut queue,
        );
        for candidate in collect_remote_dependency_refresh_candidates_for_include_component_indexed(
            workspace,
            snapshot.as_ref(),
            component_index.as_ref(),
        ) {
            if let Some(dependency_uri) =
                dependency_uri_by_name.get(&candidate.name.trim().to_ascii_lowercase())
            {
                if visited_uris.insert(dependency_uri.clone())
                    && workspace.cache.get(dependency_uri).is_some()
                {
                    queue.push_back(dependency_uri.clone());
                }
            }
            insert_remote_candidate(&mut deduped, candidate);
        }
    }

    deduped.into_values().collect()
}

fn workspace_dependency_uri_by_object_name(workspace: &WorkspaceState) -> HashMap<String, String> {
    let mut out = HashMap::new();
    for uri in workspace.cache.uris() {
        let Some(snapshot) = workspace.cache.get(uri.as_ref()) else {
            continue;
        };
        if !snapshot.is_dependency {
            continue;
        }
        let Some(object_name) = snapshot.object_name.as_ref() else {
            continue;
        };
        let normalized = object_name.trim().to_ascii_lowercase();
        if normalized.is_empty() {
            continue;
        }
        out.entry(normalized).or_insert_with(|| uri.to_string());
    }
    out
}

fn enqueue_resolved_local_export_dependency_uris(
    workspace: &WorkspaceState,
    snapshot: &AnalysisSnapshot,
    visited_uris: &mut HashSet<String>,
    queue: &mut VecDeque<String>,
) {
    for reference in &snapshot.symbols.references {
        if local_export_candidate_kind_for_reference(reference.kind, reference.namespace).is_none()
        {
            continue;
        }
        let Some(abap_symbols::Resolution::Symbol(handle)) = &reference.resolution else {
            continue;
        };
        let Some(resolved_unit) = snapshot.project.units.get(handle.unit.as_usize()) else {
            continue;
        };
        let dependency_uri = resolved_unit.uri.to_string();
        if dependency_uri == snapshot.uri.as_ref() {
            continue;
        }
        if !workspace
            .cache
            .get(&dependency_uri)
            .is_some_and(|snapshot| snapshot.is_dependency)
        {
            continue;
        }
        if visited_uris.insert(dependency_uri.clone()) {
            queue.push_back(dependency_uri);
        }
    }
}

fn collect_remote_dependency_candidates_for_unit(
    unit: &abap_symbols::UnitAnalysis,
) -> Vec<RemoteDependencyCandidate> {
    let mut deduped = HashMap::<String, RemoteDependencyCandidate>::new();
    let semantic = unit.semantic();

    for edge in unit
        .include_edges
        .iter()
        .filter(|edge| edge.target.is_none())
    {
        if !is_remote_lookup_candidate(edge.name.as_ref(), "include") {
            continue;
        }
        insert_remote_candidate(
            &mut deduped,
            RemoteDependencyCandidate {
                name: edge.name.to_string(),
                kind: "include".to_string(),
            },
        );
    }

    for reference in semantic.refs().all() {
        if !matches!(reference.resolution, None | Some(Resolution::External)) {
            continue;
        }
        if let Some(candidate) = remote_dependency_candidate_for_reference(reference) {
            insert_remote_candidate(&mut deduped, candidate);
        }
    }
    insert_message_class_dependency_candidates(&mut deduped, unit);

    for sql_source in &unit.sql_sources {
        if let Some(candidate) = remote_dependency_candidate_for_sql_source(sql_source) {
            insert_remote_candidate(&mut deduped, candidate);
        }
    }

    for call_site in &unit.call_sites {
        if let Some(candidate) = remote_dependency_candidate_for_call_site(call_site) {
            insert_remote_candidate(&mut deduped, candidate);
        }
    }

    deduped.into_values().collect()
}

fn collect_remote_dependency_refresh_candidates_for_unit(
    workspace: &WorkspaceState,
    snapshot: &AnalysisSnapshot,
    unit: &abap_symbols::UnitAnalysis,
) -> Vec<RemoteDependencyCandidate> {
    let mut deduped = HashMap::<String, RemoteDependencyCandidate>::new();

    for edge in &unit.include_edges {
        let target_is_dependency = edge
            .target
            .and_then(|target| snapshot.project.units.get(target.as_usize()))
            .is_some_and(|target_unit| {
                workspace_uri_is_dependency_source(workspace, target_unit.uri.as_ref())
            });
        if edge.target.is_some() && !target_is_dependency {
            continue;
        }
        if !is_remote_lookup_candidate(edge.name.as_ref(), "include") {
            continue;
        }
        insert_remote_candidate(
            &mut deduped,
            RemoteDependencyCandidate {
                name: edge.name.to_string(),
                kind: "include".to_string(),
            },
        );
    }

    for reference in unit.semantic().refs().all() {
        let Some(candidate) = remote_dependency_candidate_for_reference(reference) else {
            continue;
        };
        let include = match reference.resolution {
            None | Some(Resolution::External) => true,
            Some(Resolution::Symbol(handle)) => snapshot
                .project
                .units
                .get(handle.unit.as_usize())
                .is_some_and(|target_unit| {
                    workspace_uri_is_dependency_source(workspace, target_unit.uri.as_ref())
                }),
            Some(
                Resolution::BuiltinType
                | Resolution::BuiltinRoutine
                | Resolution::InternalTableLine,
            ) => false,
        };
        if include {
            insert_remote_candidate(&mut deduped, candidate);
        }
    }
    insert_message_class_dependency_candidates(&mut deduped, unit);

    for sql_source in &unit.sql_sources {
        if let Some(candidate) = remote_dependency_candidate_for_sql_source(sql_source) {
            insert_remote_candidate(&mut deduped, candidate);
        }
    }

    for call_site in &unit.call_sites {
        if let Some(candidate) = remote_dependency_candidate_for_call_site(call_site) {
            insert_remote_candidate(&mut deduped, candidate);
        }
    }

    deduped.into_values().collect()
}

fn insert_message_class_dependency_candidates(
    deduped: &mut HashMap<String, RemoteDependencyCandidate>,
    unit: &abap_symbols::UnitAnalysis,
) {
    if let Some(message_class) = &unit.message_default_class {
        insert_message_class_dependency_candidate(deduped, message_class.name.as_ref());
    }
    for message in &unit.message_uses {
        if let Some(message_class) = &message.class_name {
            insert_message_class_dependency_candidate(deduped, message_class.as_ref());
        }
    }
}

fn insert_message_class_dependency_candidate(
    deduped: &mut HashMap<String, RemoteDependencyCandidate>,
    name: &str,
) {
    if is_remote_lookup_candidate(name, "message-class") {
        insert_remote_candidate(
            deduped,
            RemoteDependencyCandidate {
                name: name.to_string(),
                kind: "message-class".to_string(),
            },
        );
    }
}

fn remote_dependency_candidate_for_reference(
    reference: &abap_symbols::ReferenceData,
) -> Option<RemoteDependencyCandidate> {
    let kind = match reference.kind {
        ReferenceKind::Include => return None,
        ReferenceKind::StaticTarget => "static",
        ReferenceKind::TypeRef => "type",
        ReferenceKind::StructuredDeclEnd => return None,
        ReferenceKind::MessageClass => "message-class",
        ReferenceKind::RoutineCall if reference.namespace == abap_symbols::Namespace::Routine => {
            "function"
        }
        ReferenceKind::Identifier | ReferenceKind::RoutineCall => "symbol",
    };
    let is_remote_candidate = match reference.kind {
        ReferenceKind::StaticTarget | ReferenceKind::TypeRef => {
            is_remote_lookup_candidate_after_local_resolution(reference.name.as_ref(), kind)
        }
        ReferenceKind::RoutineCall if reference.namespace == abap_symbols::Namespace::Routine => {
            is_remote_lookup_candidate_after_local_resolution(reference.name.as_ref(), kind)
        }
        _ => is_remote_lookup_candidate(reference.name.as_ref(), kind),
    };
    if !is_remote_candidate {
        return None;
    }
    Some(RemoteDependencyCandidate {
        name: reference.name.to_string(),
        kind: kind.to_string(),
    })
}

fn remote_dependency_candidate_for_sql_source(
    sql_source: &abap_symbols::SqlSourceData,
) -> Option<RemoteDependencyCandidate> {
    (sql_source.resolution == SqlResolution::External
        && is_remote_lookup_candidate(sql_source.name.as_ref(), "type"))
    .then(|| RemoteDependencyCandidate {
        name: sql_source.name.to_string(),
        kind: "type".to_string(),
    })
}

fn remote_dependency_candidate_for_call_site(
    call_site: &abap_symbols::CallSiteData,
) -> Option<RemoteDependencyCandidate> {
    let (name, kind) = match &call_site.target {
        abap_symbols::NamedArgumentTarget::Function { function_name } => {
            (function_name, "function")
        }
        abap_symbols::NamedArgumentTarget::Report { report_name } => (report_name, "report"),
        _ => return None,
    };
    is_remote_lookup_candidate_after_local_resolution(name.as_ref(), kind).then(|| {
        RemoteDependencyCandidate {
            name: name.to_string(),
            kind: kind.to_string(),
        }
    })
}

fn collect_remote_dependency_candidates_for_include_component(
    snapshot: &AnalysisSnapshot,
) -> Vec<RemoteDependencyCandidate> {
    collect_remote_dependency_candidates_for_include_component_indexed(snapshot, None)
}

fn collect_remote_dependency_candidates_for_include_component_indexed(
    snapshot: &AnalysisSnapshot,
    component_index: Option<&IncludeComponentIndex>,
) -> Vec<RemoteDependencyCandidate> {
    let mut deduped = HashMap::<String, RemoteDependencyCandidate>::new();
    let unit_ids = component_index
        .map(|index| index.component_unit_ids(snapshot.symbols.unit_id))
        .unwrap_or_else(|| include_component_unit_ids(snapshot));
    for unit_id in unit_ids {
        let Some(unit) = snapshot.project.units.get(unit_id.as_usize()) else {
            continue;
        };
        for candidate in collect_remote_dependency_candidates_for_unit(unit) {
            insert_remote_candidate(&mut deduped, candidate);
        }
    }
    if let Some(candidate) = stale_dependency_refresh_candidate(snapshot) {
        insert_remote_candidate(&mut deduped, candidate);
    }
    deduped.into_values().collect()
}

fn collect_remote_dependency_refresh_candidates_for_include_component(
    workspace: &WorkspaceState,
    snapshot: &AnalysisSnapshot,
) -> Vec<RemoteDependencyCandidate> {
    collect_remote_dependency_refresh_candidates_for_include_component_indexed(
        workspace, snapshot, None,
    )
}

fn collect_remote_dependency_refresh_candidates_for_include_component_indexed(
    workspace: &WorkspaceState,
    snapshot: &AnalysisSnapshot,
    component_index: Option<&IncludeComponentIndex>,
) -> Vec<RemoteDependencyCandidate> {
    let mut deduped = HashMap::<String, RemoteDependencyCandidate>::new();
    let unit_ids = component_index
        .map(|index| index.component_unit_ids(snapshot.symbols.unit_id))
        .unwrap_or_else(|| include_component_unit_ids(snapshot));
    for unit_id in unit_ids {
        let Some(unit) = snapshot.project.units.get(unit_id.as_usize()) else {
            continue;
        };
        for candidate in
            collect_remote_dependency_refresh_candidates_for_unit(workspace, snapshot, unit)
        {
            insert_remote_candidate(&mut deduped, candidate);
        }
    }
    if let Some(candidate) = stale_dependency_refresh_candidate(snapshot) {
        insert_remote_candidate(&mut deduped, candidate);
    }
    deduped.into_values().collect()
}

struct IncludeComponentIndex {
    adjacency: HashMap<UnitId, HashSet<UnitId>>,
}

impl IncludeComponentIndex {
    fn new(project: &abap_symbols::ProjectAnalysis) -> Self {
        let mut adjacency = HashMap::<UnitId, HashSet<UnitId>>::new();
        for unit in &project.units {
            for edge in &unit.include_edges {
                let Some(target) = edge.target else {
                    continue;
                };
                adjacency.entry(unit.unit_id).or_default().insert(target);
                adjacency.entry(target).or_default().insert(unit.unit_id);
            }
        }
        Self { adjacency }
    }

    fn component_unit_ids(&self, root_unit_id: UnitId) -> HashSet<UnitId> {
        if self.adjacency.is_empty() {
            return HashSet::from([root_unit_id]);
        }

        let mut visited = HashSet::new();
        let mut queue = VecDeque::from([root_unit_id]);
        while let Some(unit_id) = queue.pop_front() {
            if !visited.insert(unit_id) {
                continue;
            }
            if let Some(neighbors) = self.adjacency.get(&unit_id) {
                queue.extend(neighbors.iter().copied());
            }
        }

        if visited.is_empty() {
            HashSet::from([root_unit_id])
        } else {
            visited
        }
    }
}

fn include_component_unit_ids(snapshot: &AnalysisSnapshot) -> HashSet<UnitId> {
    let root_unit_id = snapshot.symbols.unit_id;
    if snapshot.project.units.is_empty() {
        return HashSet::from([root_unit_id]);
    }

    let mut adjacency = HashMap::<UnitId, HashSet<UnitId>>::new();
    for unit in &snapshot.project.units {
        for edge in &unit.include_edges {
            let Some(target) = edge.target else {
                continue;
            };
            adjacency.entry(unit.unit_id).or_default().insert(target);
            adjacency.entry(target).or_default().insert(unit.unit_id);
        }
    }

    let mut visited = HashSet::new();
    let mut queue = VecDeque::from([root_unit_id]);
    while let Some(unit_id) = queue.pop_front() {
        if !visited.insert(unit_id) {
            continue;
        }
        if let Some(neighbors) = adjacency.get(&unit_id) {
            queue.extend(neighbors.iter().copied());
        }
    }

    if visited.is_empty() {
        HashSet::from([root_unit_id])
    } else {
        visited
    }
}

fn collect_remote_dependency_candidates_for_workspace_batch(
    snapshot: &AnalysisSnapshot,
) -> Vec<RemoteDependencyCandidate> {
    if !snapshot.is_dependency {
        return collect_remote_dependency_candidates(snapshot);
    }

    let document = WorkspaceDocument {
        uri: Arc::clone(&snapshot.uri),
        version: snapshot.version,
        text: snapshot.text.to_string(),
        is_dependency: true,
        object_name: snapshot.object_name.clone(),
    };
    let mut deduped = HashMap::<String, RemoteDependencyCandidate>::new();
    for candidate in collect_local_export_dependency_candidates(&document) {
        insert_remote_candidate(&mut deduped, candidate);
    }
    if let Some(candidate) = stale_dependency_refresh_candidate(snapshot) {
        insert_remote_candidate(&mut deduped, candidate);
    }
    deduped.into_values().collect()
}

fn stale_dependency_refresh_candidate(
    snapshot: &AnalysisSnapshot,
) -> Option<RemoteDependencyCandidate> {
    stale_ddic_data_element_alias_refresh_candidate(snapshot)
        .or_else(|| stale_ddic_table_key_metadata_refresh_candidate(snapshot))
}

fn stale_ddic_data_element_alias_refresh_candidate(
    snapshot: &AnalysisSnapshot,
) -> Option<RemoteDependencyCandidate> {
    if dependency_document_query_param(snapshot.uri.as_ref(), "kind").as_deref()
        != Some("ddic-data-element")
    {
        return None;
    }
    let object_name = snapshot.object_name.as_deref()?;
    ddic_data_element_alias_refresh_candidate(object_name, snapshot.text.as_ref())
}

fn ddic_data_element_alias_refresh_candidate(
    object_name: &str,
    source_text: &str,
) -> Option<RemoteDependencyCandidate> {
    let alias_target = ddic_data_element_alias_target(source_text, object_name)?;
    if !is_remote_lookup_candidate_after_local_resolution(alias_target.as_str(), "type") {
        return None;
    }
    Some(RemoteDependencyCandidate {
        name: object_name.to_string(),
        kind: "type".to_string(),
    })
}

fn stale_ddic_table_key_metadata_refresh_candidate(
    snapshot: &AnalysisSnapshot,
) -> Option<RemoteDependencyCandidate> {
    if dependency_document_query_param(snapshot.uri.as_ref(), "kind").as_deref()
        != Some("ddic-table")
    {
        return None;
    }
    let object_name = snapshot.object_name.as_deref()?;
    ddic_table_key_metadata_refresh_candidate(object_name, snapshot.text.as_ref())
}

fn ddic_table_key_metadata_refresh_candidate(
    object_name: &str,
    source_text: &str,
) -> Option<RemoteDependencyCandidate> {
    if !source_text_looks_like_ddic_structure_stub(source_text, object_name)
        || ddic_structure_stub_has_key_metadata(source_text)
    {
        return None;
    }
    Some(RemoteDependencyCandidate {
        name: object_name.to_string(),
        kind: "type".to_string(),
    })
}

fn source_text_looks_like_ddic_structure_stub(source_text: &str, object_name: &str) -> bool {
    let Some(first_code_line) = source_text
        .lines()
        .map(|line| line.split_once('"').map_or(line, |(code, _)| code).trim())
        .find(|line| !line.is_empty())
    else {
        return false;
    };
    let parts = first_code_line.split_whitespace().collect::<Vec<_>>();
    parts.len() >= 4
        && parts[0].trim_end_matches(':').eq_ignore_ascii_case("types")
        && parts[1].eq_ignore_ascii_case("begin")
        && parts[2].eq_ignore_ascii_case("of")
        && parts[3]
            .trim_end_matches(',')
            .eq_ignore_ascii_case(object_name)
}

fn ddic_structure_stub_has_key_metadata(source_text: &str) -> bool {
    source_text.lines().any(|line| {
        let Some((_, comment)) = line.split_once('"') else {
            return false;
        };
        let comment = comment.trim_start().to_ascii_lowercase();
        matches!(comment.as_str(), "key" | "key field" | "primary key")
            || comment.starts_with("key;")
            || comment.starts_with("key field;")
            || comment.starts_with("key fields:")
            || comment.starts_with("primary key;")
            || comment.starts_with("primary key:")
    })
}

fn ddic_data_element_alias_target(text: &str, object_name: &str) -> Option<String> {
    let statement = text
        .lines()
        .map(|line| line.split_once('"').map_or(line, |(code, _)| code))
        .collect::<Vec<_>>()
        .join(" ");
    let statement = statement.trim();
    let statement = statement.strip_suffix('.')?.trim();
    let parts = statement.split_whitespace().collect::<Vec<_>>();
    if parts.len() != 4
        || !parts[0].eq_ignore_ascii_case("TYPES")
        || !parts[1].eq_ignore_ascii_case(object_name)
        || !parts[2].eq_ignore_ascii_case("TYPE")
    {
        return None;
    }
    Some(parts[3].to_string())
}

fn dependency_batch_candidate_fingerprint(text: &str) -> (usize, u64) {
    let mut hasher = DefaultHasher::new();
    text.hash(&mut hasher);
    (text.len(), hasher.finish())
}

fn cached_dependency_batch_candidates(
    workspace: &mut WorkspaceState,
    snapshot: &AnalysisSnapshot,
) -> Vec<RemoteDependencyCandidate> {
    let uri = snapshot.uri.to_string();
    let (text_len, text_hash) = dependency_batch_candidate_fingerprint(snapshot.text.as_ref());
    if let Some(cached) = workspace.dependency_batch_candidates.get(uri.as_str())
        && cached.text_len == text_len
        && cached.text_hash == text_hash
        && cached.object_name == snapshot.object_name
    {
        return cached.candidates.clone();
    }

    let candidates = collect_remote_dependency_candidates_for_workspace_batch(snapshot);
    workspace.dependency_batch_candidates.insert(
        uri,
        CachedDependencyBatchCandidates {
            text_len,
            text_hash,
            object_name: snapshot.object_name.clone(),
            candidates: candidates.clone(),
        },
    );
    candidates
}

fn remote_dependency_batch_phase(
    workspace: &WorkspaceState,
    uri: &str,
) -> RemoteDependencyBatchPhase {
    if workspace.open_documents.contains_key(uri) {
        return RemoteDependencyBatchPhase::PriorityLocal;
    }
    if uri_is_manifest_dependency(workspace, uri) {
        return RemoteDependencyBatchPhase::Dependency;
    }

    let Some(root_path) = file_uri_to_path(&workspace.root_uri) else {
        return RemoteDependencyBatchPhase::OtherLocal;
    };
    let relative = file_uri_to_path(uri)
        .map(|path| abap_cache::workspace_relative_path(&root_path, &path))
        .unwrap_or_default()
        .replace('\\', "/");
    if relative == "src" || relative.starts_with("src/") {
        RemoteDependencyBatchPhase::PriorityLocal
    } else {
        RemoteDependencyBatchPhase::OtherLocal
    }
}

fn insert_remote_candidate(
    deduped: &mut HashMap<String, RemoteDependencyCandidate>,
    candidate: RemoteDependencyCandidate,
) {
    let normalized_name = candidate.name.trim().to_ascii_lowercase();
    if normalized_name.is_empty() {
        return;
    }
    let priority = remote_candidate_kind_priority(&candidate.kind);
    match deduped.get(&normalized_name) {
        Some(existing) if remote_candidate_kind_priority(&existing.kind) >= priority => {}
        _ => {
            deduped.insert(
                normalized_name.clone(),
                RemoteDependencyCandidate {
                    name: normalized_name,
                    kind: candidate.kind.trim().to_ascii_lowercase(),
                },
            );
        }
    }
}

fn remote_candidate_kind_priority(kind: &str) -> usize {
    match kind.trim().to_ascii_lowercase().as_str() {
        "message-class" => 5,
        "include" => 4,
        "function" => 4,
        "static" => 3,
        "type" => 2,
        _ => 1,
    }
}

struct RemoteDependencyCacheContext {
    dependency_reader: Option<DependencyStoreReader>,
    dependency_profile: Option<DependencyProfile>,
    connection_key: String,
}

impl RemoteDependencyCacheContext {
    fn new(workspace: &WorkspaceState) -> Self {
        Self {
            dependency_reader: workspace_dependency_store(workspace)
                .and_then(|store| store.reader().ok()),
            dependency_profile: workspace_dependency_profile(workspace),
            connection_key: workspace_dependency_connection_key(workspace),
        }
    }
}

fn has_cached_remote_dependency_candidate(
    cache_context: &RemoteDependencyCacheContext,
    candidate: &RemoteDependencyCandidate,
) -> bool {
    cache_context
        .dependency_reader
        .as_ref()
        .zip(cache_context.dependency_profile.as_ref())
        .and_then(|(reader, profile)| {
            reader
                .find_cached_candidate(
                    profile,
                    &cache_context.connection_key,
                    candidate.name.as_str(),
                    candidate.kind.as_str(),
                )
                .ok()
        })
        .is_some_and(|status| matches!(status, CandidateCacheStatus::Artifact))
}

struct RemoteDependencyMemo {
    request_candidates: HashMap<String, Vec<RemoteDependencyCandidate>>,
    batch_candidates: HashMap<String, Vec<RemoteDependencyCandidate>>,
    refresh_candidates: HashMap<String, Vec<RemoteDependencyCandidate>>,
    cached_candidate_presence: HashMap<String, bool>,
    negative_candidate_presence: HashMap<String, bool>,
    source_context_uris: HashMap<String, Vec<String>>,
    source_context_local_exports: HashMap<String, bool>,
}

impl RemoteDependencyMemo {
    fn new() -> Self {
        Self {
            request_candidates: HashMap::new(),
            batch_candidates: HashMap::new(),
            refresh_candidates: HashMap::new(),
            cached_candidate_presence: HashMap::new(),
            negative_candidate_presence: HashMap::new(),
            source_context_uris: HashMap::new(),
            source_context_local_exports: HashMap::new(),
        }
    }

    fn candidates_for_request(
        &mut self,
        workspace: &mut WorkspaceState,
        snapshot: &AnalysisSnapshot,
        source_uri: &str,
    ) -> Vec<RemoteDependencyCandidate> {
        self.request_candidates
            .entry(source_uri.to_owned())
            .or_insert_with(|| {
                collect_remote_dependency_candidates_for_request(workspace, snapshot, source_uri)
            })
            .clone()
    }

    fn candidates_for_batch(
        &mut self,
        workspace: &mut WorkspaceState,
        snapshot: &AnalysisSnapshot,
        source_uri: &str,
    ) -> Vec<RemoteDependencyCandidate> {
        self.batch_candidates
            .entry(source_uri.to_owned())
            .or_insert_with(|| {
                collect_remote_dependency_candidates_for_batch(workspace, snapshot, source_uri)
            })
            .clone()
    }

    fn candidates_for_refresh_batch(
        &mut self,
        workspace: &mut WorkspaceState,
        snapshot: &AnalysisSnapshot,
        source_uri: &str,
    ) -> Vec<RemoteDependencyCandidate> {
        self.refresh_candidates
            .entry(source_uri.to_owned())
            .or_insert_with(|| {
                collect_remote_dependency_refresh_candidates_for_batch(
                    workspace, snapshot, source_uri,
                )
            })
            .clone()
    }

    fn source_context_uris(&mut self, workspace: &WorkspaceState, source_uri: &str) -> Vec<String> {
        self.source_context_uris
            .entry(source_uri.to_owned())
            .or_insert_with(|| remote_dependency_source_context_uris(workspace, source_uri))
            .clone()
    }

    fn source_uses_local_exports(&mut self, workspace: &WorkspaceState, source_uri: &str) -> bool {
        if let Some(uses_local_exports) = self.source_context_local_exports.get(source_uri) {
            return *uses_local_exports;
        }
        let uses_local_exports = source_context_uses_local_exports(workspace, source_uri);
        self.source_context_local_exports
            .insert(source_uri.to_owned(), uses_local_exports);
        uses_local_exports
    }

    fn has_cached_candidate(
        &mut self,
        cache_context: &RemoteDependencyCacheContext,
        candidate: &RemoteDependencyCandidate,
    ) -> bool {
        let key = remote_candidate_key(candidate);
        if let Some(cached) = self.cached_candidate_presence.get(&key) {
            return *cached;
        }
        let cached = has_cached_remote_dependency_candidate(cache_context, candidate);
        self.cached_candidate_presence.insert(key, cached);
        cached
    }

    fn has_persisted_negative_candidate(
        &mut self,
        cache_context: &RemoteDependencyCacheContext,
        candidate: &RemoteDependencyCandidate,
    ) -> bool {
        let key = remote_candidate_key(candidate);
        if let Some(negative) = self.negative_candidate_presence.get(&key) {
            return *negative;
        }
        let negative = has_persisted_negative_remote_dependency_candidate(cache_context, candidate);
        self.negative_candidate_presence.insert(key, negative);
        negative
    }
}

fn has_persisted_negative_remote_dependency_candidate(
    cache_context: &RemoteDependencyCacheContext,
    candidate: &RemoteDependencyCandidate,
) -> bool {
    cache_context
        .dependency_reader
        .as_ref()
        .zip(cache_context.dependency_profile.as_ref())
        .and_then(|(reader, profile)| {
            reader
                .find_cached_candidate(
                    profile,
                    &cache_context.connection_key,
                    candidate.name.as_str(),
                    candidate.kind.as_str(),
                )
                .ok()
        })
        .is_some_and(|status| matches!(status, CandidateCacheStatus::Negative))
}

fn stale_cached_dependency_refresh_sources(
    workspace: &WorkspaceState,
) -> Vec<(String, RemoteDependencyCandidate)> {
    let Some(profile) = workspace_dependency_profile(workspace) else {
        return Vec::new();
    };
    let Some(store) = workspace_dependency_store(workspace) else {
        return Vec::new();
    };
    let mut out = Vec::new();

    if let Ok(records) = store.list_artifacts_by_kind(&profile, "ddic-data-element") {
        out.extend(records.into_iter().filter_map(|record| {
            let candidate = ddic_data_element_alias_refresh_candidate(
                record.object_name.as_str(),
                record.source_text.as_str(),
            )?;
            let uri = dependency_document_uri_with_kind(
                &workspace.root_uri,
                record.artifact_id,
                record.object_name.as_str(),
                Some(record.object_kind.as_str()),
            );
            Some((uri, candidate))
        }));
    }

    if let Ok(records) = store.list_artifacts_by_kind(&profile, "ddic-table") {
        out.extend(records.into_iter().filter_map(|record| {
            let candidate = ddic_table_key_metadata_refresh_candidate(
                record.object_name.as_str(),
                record.source_text.as_str(),
            )?;
            let uri = dependency_document_uri_with_kind(
                &workspace.root_uri,
                record.artifact_id,
                record.object_name.as_str(),
                Some(record.object_kind.as_str()),
            );
            Some((uri, candidate))
        }));
    }

    out
}

#[derive(Debug, Clone, Copy, Default)]
struct RemoteDependencyRequestOptions {
    retry_negative_candidates: bool,
    bypass_cached_candidates: bool,
    include_resolved_dependencies: bool,
}

fn build_remote_dependency_request_for_snapshot(
    workspace: &mut WorkspaceState,
    source_uri: &str,
    snapshot: &AnalysisSnapshot,
    memo: &mut RemoteDependencyMemo,
    cache_context: &RemoteDependencyCacheContext,
    options: RemoteDependencyRequestOptions,
) -> Option<RemoteDependencyResolveParams> {
    let mut candidates = Vec::new();
    let source_uses_local_exports = memo.source_uses_local_exports(workspace, source_uri);
    let source_candidates = if options.include_resolved_dependencies {
        memo.candidates_for_refresh_batch(workspace, snapshot, source_uri)
    } else {
        memo.candidates_for_request(workspace, snapshot, source_uri)
    };
    let stale_refresh_key = stale_dependency_refresh_candidate(snapshot)
        .map(|candidate| remote_candidate_key(&candidate));
    for candidate in source_candidates {
        let key = remote_candidate_key(&candidate);
        let is_stale_refresh = stale_refresh_key.as_deref() == Some(key.as_str());
        if !options.bypass_cached_candidates
            && !is_stale_refresh
            && memo.has_cached_candidate(cache_context, &candidate)
        {
            continue;
        }
        if !options.retry_negative_candidates
            && !source_uses_local_exports
            && memo.has_persisted_negative_candidate(cache_context, &candidate)
        {
            continue;
        }
        if !options.retry_negative_candidates && workspace.remote_lookup_failures.contains(&key) {
            continue;
        }
        if options.retry_negative_candidates || workspace.remote_resolution_seen.insert(key) {
            candidates.push(candidate);
        }
    }
    if candidates.is_empty() {
        return None;
    }

    let context_uris = memo.source_context_uris(workspace, source_uri);
    let mut source_uris = Vec::new();
    let mut source_uri_seen = HashSet::new();
    let mut source_candidates = HashMap::new();
    extend_remote_dependency_source_candidates(
        &mut source_candidates,
        &mut source_uris,
        &mut source_uri_seen,
        &context_uris,
        &candidates,
    );
    Some(RemoteDependencyResolveParams {
        workspace_uri: workspace.root_uri.clone(),
        source_uri: source_uri.to_owned(),
        source_uris,
        retry_negative_candidates: options.retry_negative_candidates,
        remote_request_parallelism: workspace
            .manifest
            .as_ref()
            .and_then(|manifest| manifest.resolution.remote_request_parallelism()),
        remote_requests_per_second: workspace
            .manifest
            .as_ref()
            .map(|manifest| manifest.resolution.remote_requests_per_second),
        source_candidates,
        candidates,
    })
}

fn hex_digit(value: u8) -> char {
    match value {
        0..=9 => (b'0' + value) as char,
        10..=15 => (b'A' + (value - 10)) as char,
        _ => '0',
    }
}

pub fn build_remote_dependency_request(
    state: &mut ServerState,
    source_uri: &str,
) -> Option<RemoteDependencyResolveParams> {
    let source_uri = normalize_lsp_uri(source_uri);
    let workspace = state.workspace_for_uri_mut(&source_uri)?;
    if !workspace_supports_dependency_store_resolution(workspace) {
        return None;
    }
    let snapshot = workspace.cache.get(&source_uri)?;
    let snapshot = Arc::clone(&snapshot);
    let cache_context = RemoteDependencyCacheContext::new(workspace);
    let mut memo = RemoteDependencyMemo::new();
    build_remote_dependency_request_for_snapshot(
        workspace,
        &source_uri,
        snapshot.as_ref(),
        &mut memo,
        &cache_context,
        RemoteDependencyRequestOptions::default(),
    )
}

pub fn build_remote_dependency_request_retrying_negatives(
    state: &mut ServerState,
    source_uri: &str,
) -> Option<RemoteDependencyResolveParams> {
    let source_uri = normalize_lsp_uri(source_uri);
    let workspace = state.workspace_for_uri_mut(&source_uri)?;
    if !workspace_supports_dependency_store_resolution(workspace) {
        return None;
    }
    let snapshot = workspace.cache.get(&source_uri)?;
    let snapshot = Arc::clone(&snapshot);
    let cache_context = RemoteDependencyCacheContext::new(workspace);
    let mut memo = RemoteDependencyMemo::new();
    build_remote_dependency_request_for_snapshot(
        workspace,
        &source_uri,
        snapshot.as_ref(),
        &mut memo,
        &cache_context,
        RemoteDependencyRequestOptions {
            retry_negative_candidates: true,
            ..RemoteDependencyRequestOptions::default()
        },
    )
}

pub fn build_remote_dependency_batch_for_workspace(
    state: &mut ServerState,
    workspace_uri: &str,
) -> Option<RemoteDependencyResolveParams> {
    build_remote_dependency_batch_for_workspace_filtered(state, workspace_uri, None)
}

pub fn build_remote_dependency_refresh_for_workspace(
    state: &mut ServerState,
    workspace_uri: &str,
) -> Option<RemoteDependencyResolveParams> {
    build_remote_dependency_batch_for_workspace_filtered_with_options(
        state,
        workspace_uri,
        None,
        RemoteDependencyRequestOptions {
            retry_negative_candidates: true,
            bypass_cached_candidates: true,
            include_resolved_dependencies: true,
        },
    )
}

pub fn build_remote_dependency_batch_for_workspace_filtered(
    state: &mut ServerState,
    workspace_uri: &str,
    source_uri_filter: Option<&HashSet<Arc<str>>>,
) -> Option<RemoteDependencyResolveParams> {
    build_remote_dependency_batch_for_workspace_filtered_with_options(
        state,
        workspace_uri,
        source_uri_filter,
        RemoteDependencyRequestOptions::default(),
    )
}

fn build_remote_dependency_batch_for_workspace_filtered_with_options(
    state: &mut ServerState,
    workspace_uri: &str,
    source_uri_filter: Option<&HashSet<Arc<str>>>,
    options: RemoteDependencyRequestOptions,
) -> Option<RemoteDependencyResolveParams> {
    let workspace_uri = normalize_lsp_uri(workspace_uri);
    let workspace = state.workspaces.get_mut(&workspace_uri)?;
    if workspace.remote_resolution_in_flight
        || !workspace_supports_dependency_store_resolution(workspace)
    {
        return None;
    }

    let mut uris: Vec<_> = if let Some(source_uri_filter) = source_uri_filter {
        source_uri_filter.iter().cloned().collect()
    } else {
        workspace.cache.uris()
    };
    let mut uri_entries: Vec<_> = uris
        .drain(..)
        .map(|uri| {
            let phase = remote_dependency_batch_phase(workspace, uri.as_ref());
            (uri, phase)
        })
        .collect();
    uri_entries.sort_by(|left, right| {
        left.1
            .cmp(&right.1)
            .then_with(|| left.0.as_ref().cmp(right.0.as_ref()))
    });

    let mut source_uris = Vec::new();
    let mut candidates = Vec::new();
    let mut source_candidates = HashMap::new();
    let mut batch_seen = HashSet::new();
    let mut source_uri_seen = HashSet::new();
    let mut selected_phase = None;
    let mut memo = RemoteDependencyMemo::new();
    let cache_context = RemoteDependencyCacheContext::new(workspace);
    let remote_request_parallelism = workspace
        .manifest
        .as_ref()
        .and_then(|manifest| manifest.resolution.remote_request_parallelism());
    let remote_requests_per_second = workspace
        .manifest
        .as_ref()
        .map(|manifest| manifest.resolution.remote_requests_per_second);
    let skip_unparented_closed_dependencies = source_uri_filter.is_none()
        && !options.include_resolved_dependencies
        && workspace_has_local_export_sources(workspace);

    for (uri, phase) in uri_entries {
        if selected_phase.is_some_and(|selected| phase != selected) {
            break;
        }

        let Some(snapshot) = workspace.cache.get(uri.as_ref()) else {
            continue;
        };
        let snapshot = Arc::clone(&snapshot);
        if skip_unparented_closed_dependencies
            && skip_closed_dependency_batch_source(workspace, uri.as_ref(), snapshot.as_ref())
        {
            continue;
        }

        let mut added_for_uri = false;
        let mut uri_candidates = Vec::new();
        let mut uri_seen = HashSet::new();
        let source_uses_local_exports = memo.source_uses_local_exports(workspace, uri.as_ref());
        let snapshot_candidates = if options.include_resolved_dependencies {
            memo.candidates_for_refresh_batch(workspace, snapshot.as_ref(), uri.as_ref())
        } else {
            memo.candidates_for_batch(workspace, snapshot.as_ref(), uri.as_ref())
        };
        let stale_refresh_key = stale_dependency_refresh_candidate(snapshot.as_ref())
            .map(|candidate| remote_candidate_key(&candidate));
        for candidate in snapshot_candidates {
            let key = remote_candidate_key(&candidate);
            let is_stale_refresh = stale_refresh_key.as_deref() == Some(key.as_str());
            if !options.bypass_cached_candidates
                && !is_stale_refresh
                && memo.has_cached_candidate(&cache_context, &candidate)
            {
                continue;
            }
            if !options.retry_negative_candidates
                && !source_uses_local_exports
                && memo.has_persisted_negative_candidate(&cache_context, &candidate)
            {
                continue;
            }
            if !options.retry_negative_candidates
                && (workspace.remote_resolution_seen.contains(&key)
                    || workspace.remote_lookup_failures.contains(&key))
            {
                continue;
            }
            if uri_seen.insert(key.clone()) {
                uri_candidates.push(candidate.clone());
            }
            if !batch_seen.insert(key) {
                continue;
            }
            if selected_phase.is_none() {
                selected_phase = Some(phase);
            }
            candidates.push(candidate);
            added_for_uri = true;
        }

        if added_for_uri {
            let context_uris = memo.source_context_uris(workspace, uri.as_ref());
            extend_remote_dependency_source_candidates(
                &mut source_candidates,
                &mut source_uris,
                &mut source_uri_seen,
                &context_uris,
                &uri_candidates,
            );
        }
    }

    if options.include_resolved_dependencies && options.bypass_cached_candidates {
        for (source_uri, candidate) in stale_cached_dependency_refresh_sources(workspace) {
            let key = remote_candidate_key(&candidate);
            if !batch_seen.insert(key) {
                continue;
            }
            if source_uri_seen.insert(source_uri.clone()) {
                source_uris.push(source_uri.clone());
            }
            source_candidates
                .entry(source_uri)
                .or_default()
                .push(candidate.clone());
            candidates.push(candidate);
        }
    }

    if candidates.is_empty() {
        return None;
    }

    for candidate in &candidates {
        workspace
            .remote_resolution_seen
            .insert(remote_candidate_key(candidate));
    }
    workspace.remote_resolution_in_flight = true;

    Some(RemoteDependencyResolveParams {
        workspace_uri: workspace.root_uri.clone(),
        source_uri: source_uris.first().cloned().unwrap_or_default(),
        source_uris,
        retry_negative_candidates: options.retry_negative_candidates,
        remote_request_parallelism,
        remote_requests_per_second,
        source_candidates,
        candidates,
    })
}

pub fn build_remote_dependency_requests_for_workspace(
    state: &mut ServerState,
    workspace_uri: &str,
) -> Vec<RemoteDependencyResolveParams> {
    let workspace_uri = normalize_lsp_uri(workspace_uri);
    let Some(sorted_uris) = state.workspaces.get(&workspace_uri).map(|workspace| {
        let mut uris = workspace.cache.uris();
        uris.sort();
        uris
    }) else {
        return Vec::new();
    };

    let Some(workspace) = state.workspaces.get_mut(&workspace_uri) else {
        return Vec::new();
    };
    if !workspace_supports_dependency_store_resolution(workspace) {
        return Vec::new();
    }

    let cache_context = RemoteDependencyCacheContext::new(workspace);
    let mut memo = RemoteDependencyMemo::new();
    let mut requests = Vec::new();
    let skip_unparented_closed_dependencies = workspace_has_local_export_sources(workspace);
    for uri in sorted_uris {
        let Some(snapshot) = workspace.cache.get(uri.as_ref()) else {
            continue;
        };
        let snapshot = Arc::clone(&snapshot);
        if skip_unparented_closed_dependencies
            && skip_closed_dependency_batch_source(workspace, uri.as_ref(), snapshot.as_ref())
        {
            continue;
        }
        if let Some(request) = build_remote_dependency_request_for_snapshot(
            workspace,
            uri.as_ref(),
            snapshot.as_ref(),
            &mut memo,
            &cache_context,
            RemoteDependencyRequestOptions::default(),
        ) {
            requests.push(request);
        }
    }
    requests
}

fn workspace_has_local_export_sources(workspace: &WorkspaceState) -> bool {
    workspace.cache.uris().into_iter().any(|uri| {
        workspace.cache.get(uri.as_ref()).is_some_and(|snapshot| {
            !snapshot.is_dependency && document_uses_local_exports(workspace, uri.as_ref())
        })
    })
}

fn skip_closed_dependency_batch_source(
    workspace: &WorkspaceState,
    uri: &str,
    snapshot: &AnalysisSnapshot,
) -> bool {
    snapshot.is_dependency
        && !workspace.open_documents.contains_key(uri)
        && !uri_is_manifest_dependency(workspace, uri)
        && !workspace.dependency_parent_uris.contains_key(uri)
}

fn semantic_diagnostic_severity(kind: DiagnosticKind) -> DiagnosticSeverity {
    match kind {
        DiagnosticKind::DuplicateDeclaration | DiagnosticKind::ShadowedSymbol => {
            DiagnosticSeverity::WARNING
        }
        DiagnosticKind::UnknownFunctionModuleException
        | DiagnosticKind::UseBeforeDefiniteAssignment
        | DiagnosticKind::PossiblyUnboundFieldSymbol
        | DiagnosticKind::UnreachableCode
        | DiagnosticKind::DeadStore
        | DiagnosticKind::UnsortedReadTableBinarySearch => DiagnosticSeverity::WARNING,
        DiagnosticKind::IncompatibleAssignmentType
        | DiagnosticKind::IncompatibleArgumentType
        | DiagnosticKind::MissingMethodImplementation => DiagnosticSeverity::ERROR,
        DiagnosticKind::UnverifiedOpenSqlSource => DiagnosticSeverity::ERROR,
        DiagnosticKind::UnresolvedReference
        | DiagnosticKind::UnresolvedInclude
        | DiagnosticKind::IncludeCycle
        | DiagnosticKind::MismatchedStructuredDeclaration
        | DiagnosticKind::WrongNamespace
        | DiagnosticKind::UnknownField
        | DiagnosticKind::InvalidBuiltinNamedArgument
        | DiagnosticKind::InvalidPerformCall
        | DiagnosticKind::AbstractClassInstantiation
        | DiagnosticKind::MissingSuperConstructorCall
        | DiagnosticKind::InvalidObjectTypeReference
        | DiagnosticKind::InvalidParameterType
        | DiagnosticKind::UnknownNamedParameter
        | DiagnosticKind::DuplicateNamedParameter
        | DiagnosticKind::MissingRequiredParameter
        | DiagnosticKind::InvalidOpenSqlIntoTarget
        | DiagnosticKind::InvalidOpenSqlSyntax
        | DiagnosticKind::InvalidMessage
        | DiagnosticKind::InvalidConstructorForIteratorReuse
        | DiagnosticKind::MissingTablesDeclaration => DiagnosticSeverity::ERROR,
    }
}

fn semantic_diagnostic_code(kind: DiagnosticKind) -> Option<NumberOrString> {
    match kind {
        DiagnosticKind::MissingMethodImplementation => Some(NumberOrString::String(
            DIAGNOSTIC_CODE_MISSING_METHOD_IMPLEMENTATION.to_string(),
        )),
        _ => None,
    }
}

fn snapshot_is_workspace_include_fragment(
    workspace: Option<&WorkspaceState>,
    snapshot: &AnalysisSnapshot,
) -> bool {
    if dependency_document_query_param(snapshot.uri.as_ref(), "kind")
        .is_some_and(|kind| kind.eq_ignore_ascii_case("include"))
    {
        return true;
    }

    let Some(workspace) = workspace else {
        return false;
    };
    workspace_manifest_document_info(workspace, snapshot.uri.as_ref())
        .is_some_and(|info| info.is_member || info.unit_kind.trim().eq_ignore_ascii_case("include"))
}

fn build_lsp_parse_diagnostics(
    snapshot: &AnalysisSnapshot,
    include_fragment_policy: bool,
) -> Vec<Diagnostic> {
    snapshot
        .parse
        .errors
        .iter()
        .filter(|err| !(include_fragment_policy && parse_error_is_include_fragment_boundary(err)))
        .filter_map(|err| {
            Some(Diagnostic {
                range: byte_range_to_lsp_range_snapshot(snapshot, err.range.clone())?,
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("abap-parser".to_owned()),
                message: err.message.clone(),
                related_information: None,
                tags: None,
                data: None,
            })
        })
        .collect()
}

fn build_lsp_semantic_diagnostics(snapshot: &AnalysisSnapshot) -> Vec<Diagnostic> {
    let mut out = Vec::new();
    for diag_inner in &snapshot.symbols.diagnostics {
        if lint_id_for_diagnostic_kind(diag_inner.kind).is_some() {
            continue;
        }
        let Some(range) = byte_range_to_lsp_range_snapshot(snapshot, diag_inner.range.clone())
        else {
            continue;
        };
        out.push(Diagnostic {
            range,
            severity: Some(semantic_diagnostic_severity(diag_inner.kind)),
            code: semantic_diagnostic_code(diag_inner.kind),
            code_description: None,
            source: Some("abap-symbols".to_owned()),
            message: diag_inner.message.clone(),
            related_information: None,
            tags: None,
            data: None,
        });
    }
    out
}

fn lint_diagnostic_severity(level: LintLevel) -> Option<DiagnosticSeverity> {
    match level {
        LintLevel::Allow => None,
        LintLevel::Info => Some(DiagnosticSeverity::INFORMATION),
        LintLevel::Warn => Some(DiagnosticSeverity::WARNING),
        LintLevel::Deny => Some(DiagnosticSeverity::ERROR),
    }
}

fn lint_diagnostic_code_description(id: &str) -> Option<CodeDescription> {
    let anchor = lint_docs_anchor(id);
    let href = if anchor.is_empty() {
        LINT_REFERENCE_DOCS_URL.to_string()
    } else {
        format!("{LINT_REFERENCE_DOCS_URL}#{anchor}")
    };
    Uri::from_str(&href)
        .ok()
        .map(|href| CodeDescription { href })
}

#[derive(Serialize)]
struct LspLintDiagnosticData<'a> {
    lint_id: &'a str,
    group: &'a str,
    origin: &'a str,
    suppressed: bool,
    suppression: Option<LspLintSuppressionData<'a>>,
}

#[derive(Serialize)]
struct LspLintSuppressionData<'a> {
    kind: &'a str,
    token: &'a str,
    range: [usize; 2],
}

fn build_lsp_lint_diagnostic(
    snapshot: &AnalysisSnapshot,
    lint: &LintDiagnostic,
) -> Option<Diagnostic> {
    let severity = lint_diagnostic_severity(lint.level)?;
    let data = serde_json::to_value(LspLintDiagnosticData {
        lint_id: lint.id.as_str(),
        group: lint.group.as_str(),
        origin: lint.origin.as_str(),
        suppressed: lint.suppressed,
        suppression: lint
            .suppression
            .as_ref()
            .map(|suppression| LspLintSuppressionData {
                kind: suppression.kind.as_str(),
                token: suppression.token.as_str(),
                range: [suppression.range.start, suppression.range.end],
            }),
    })
    .ok();
    Some(Diagnostic {
        range: byte_range_to_lsp_range_snapshot(snapshot, lint.range.clone())?,
        severity: Some(severity),
        code: Some(NumberOrString::String(lint.id.clone())),
        code_description: lint_diagnostic_code_description(lint.id.as_str()),
        source: Some(LINT_DIAGNOSTIC_SOURCE.to_owned()),
        message: lint.message.clone(),
        related_information: None,
        tags: lint.suppressed.then(|| vec![DiagnosticTag::UNNECESSARY]),
        data,
    })
}

fn build_lsp_lint_diagnostics(snapshot: &AnalysisSnapshot) -> Vec<Diagnostic> {
    snapshot
        .lint_diagnostics()
        .iter()
        .filter_map(|lint| build_lsp_lint_diagnostic(snapshot, lint))
        .collect()
}

#[derive(Serialize)]
struct LspSapAtcDiagnosticData<'a> {
    kind: &'static str,
    #[serde(rename = "sapCheckId")]
    sap_check_id: &'a str,
    #[serde(rename = "sapMessageId")]
    sap_message_id: &'a str,
    #[serde(rename = "objectName")]
    object_name: &'a str,
    #[serde(rename = "includeName")]
    include_name: &'a str,
    #[serde(rename = "checkVariant")]
    check_variant: &'a str,
    configuration: Option<&'a str>,
    #[serde(rename = "fetchedAt")]
    fetched_at: &'a str,
    #[serde(rename = "mappedLocalLintId")]
    mapped_local_lint_id: Option<&'a str>,
    #[serde(rename = "exemptionState")]
    exemption_state: Option<&'a str>,
    #[serde(rename = "suppressionState")]
    suppression_state: Option<&'a str>,
}

fn build_lsp_sap_atc_diagnostics(
    workspace: &WorkspaceState,
    snapshot: &AnalysisSnapshot,
) -> Vec<Diagnostic> {
    let Some(lints) = workspace
        .manifest
        .as_ref()
        .and_then(|manifest| manifest.lints.as_ref())
    else {
        return Vec::new();
    };
    if !lints.sap_atc.mode.is_enabled() {
        return Vec::new();
    }

    let check_variant = normalized_sap_atc_check_variant(&lints.sap_atc.check_variant);
    let configuration = normalized_sap_atc_configuration(lints.sap_atc.configuration.as_deref());
    let object_name = snapshot.object_name.as_deref().unwrap_or_default();
    let mut diagnostics = Vec::new();
    for result in workspace.sap_atc_results.values() {
        if result.source_uri != snapshot.uri.as_ref()
            || result.document_version != snapshot.version
            || result.check_variant != check_variant
            || result.configuration != configuration
            || !sap_atc_object_names_match(object_name, &result.object_name)
        {
            continue;
        }

        for finding in &result.findings {
            if !sap_atc_finding_targets_snapshot(snapshot, finding) {
                continue;
            }
            let Some(range) = sap_atc_finding_lsp_range(snapshot, &finding.location) else {
                continue;
            };
            let mapped_lint_id = sap_atc_mapped_lint_id(finding);
            let data = serde_json::to_value(LspSapAtcDiagnosticData {
                kind: "sap_atc_lint",
                sap_check_id: finding.sap_check_id.as_str(),
                sap_message_id: finding.sap_message_id.as_str(),
                object_name: result.object_name.as_str(),
                include_name: finding.location.include_name.as_str(),
                check_variant: result.check_variant.as_str(),
                configuration: result.configuration.as_deref(),
                fetched_at: result.fetched_at.as_str(),
                mapped_local_lint_id: mapped_lint_id.as_deref(),
                exemption_state: finding.exemption_state.as_deref(),
                suppression_state: finding.suppression_state.as_deref(),
            })
            .ok();
            diagnostics.push(Diagnostic {
                range,
                severity: Some(sap_atc_diagnostic_severity(&finding.severity)),
                code: Some(NumberOrString::String(sap_atc_diagnostic_code(
                    finding,
                    mapped_lint_id.as_deref(),
                ))),
                code_description: None,
                source: Some(SAP_ATC_DIAGNOSTIC_SOURCE.to_owned()),
                message: sap_atc_diagnostic_message(finding),
                related_information: None,
                tags: sap_atc_diagnostic_tags(finding),
                data,
            });
        }
    }
    diagnostics
}

fn sap_atc_object_names_match(snapshot_object_name: &str, result_object_name: &str) -> bool {
    snapshot_object_name.is_empty()
        || result_object_name.is_empty()
        || snapshot_object_name.eq_ignore_ascii_case(result_object_name)
}

fn sap_atc_finding_targets_snapshot(
    snapshot: &AnalysisSnapshot,
    finding: &SapAtcFindingPayload,
) -> bool {
    let uri = finding.location.uri.trim();
    uri.is_empty() || normalize_lsp_uri(uri) == snapshot.uri.as_ref()
}

fn sap_atc_finding_lsp_range(
    snapshot: &AnalysisSnapshot,
    location: &SapAtcSourceLocationPayload,
) -> Option<Range> {
    let start_line = location.start_line.unwrap_or(1).saturating_sub(1);
    let start_column = location.start_column.unwrap_or(1).saturating_sub(1);
    let start = Position {
        line: start_line,
        character: start_column,
    };
    position_to_offset_snapshot(snapshot, start)?;

    let end_line = location
        .end_line
        .or(location.start_line)
        .unwrap_or(1)
        .saturating_sub(1);
    let end_column = location
        .end_column
        .or_else(|| location.start_column.map(|column| column.saturating_add(1)))
        .unwrap_or(1)
        .saturating_sub(1);
    let mut end = Position {
        line: end_line,
        character: end_column,
    };
    let end_valid = position_to_offset_snapshot(snapshot, end).is_some();
    if !end_valid
        || end.line < start.line
        || (end.line == start.line && end.character < start.character)
    {
        end = start;
    }

    Some(Range { start, end })
}

fn sap_atc_diagnostic_severity(value: &str) -> DiagnosticSeverity {
    match value.trim().to_ascii_lowercase().as_str() {
        "1" | "error" | "err" | "fatal" | "priority-1" => DiagnosticSeverity::ERROR,
        "3" | "info" | "information" | "note" | "priority-3" => DiagnosticSeverity::INFORMATION,
        "4" | "hint" | "priority-4" => DiagnosticSeverity::HINT,
        _ => DiagnosticSeverity::WARNING,
    }
}

fn sap_atc_diagnostic_message(finding: &SapAtcFindingPayload) -> String {
    let message = finding.message.trim();
    if !message.is_empty() {
        return message.to_string();
    }
    format!(
        "SAP ATC finding {}",
        sap_atc_external_code(&finding.sap_check_id, &finding.sap_message_id)
    )
}

fn sap_atc_diagnostic_code(finding: &SapAtcFindingPayload, mapped_lint_id: Option<&str>) -> String {
    mapped_lint_id
        .map(str::to_string)
        .unwrap_or_else(|| sap_atc_external_code(&finding.sap_check_id, &finding.sap_message_id))
}

fn sap_atc_external_code(check_id: &str, message_id: &str) -> String {
    let check = stable_sap_atc_code_part(check_id);
    let message = stable_sap_atc_code_part(message_id);
    if message.is_empty() {
        format!(
            "sap-atc:{}",
            if check.is_empty() { "unknown" } else { &check }
        )
    } else {
        format!(
            "sap-atc:{}/{}",
            if check.is_empty() { "unknown" } else { &check },
            message
        )
    }
}

fn stable_sap_atc_code_part(value: &str) -> String {
    value
        .trim()
        .chars()
        .map(|ch| {
            let ch = ch.to_ascii_lowercase();
            if ch.is_ascii_alphanumeric() || matches!(ch, '_' | '-' | '.') {
                ch
            } else {
                '_'
            }
        })
        .collect()
}

fn sap_atc_mapped_lint_id(finding: &SapAtcFindingPayload) -> Option<String> {
    if let Some(id) = finding
        .mapped_local_lint_id
        .as_deref()
        .map(str::trim)
        .filter(|id| !id.is_empty())
    {
        return Some(id.to_string());
    }

    let check_id = finding.sap_check_id.trim();
    let message_id = finding.sap_message_id.trim();
    registry().iter().find_map(|metadata| {
        metadata
            .sap_aliases
            .iter()
            .any(|alias| {
                alias.eq_ignore_ascii_case(check_id) || alias.eq_ignore_ascii_case(message_id)
            })
            .then(|| metadata.id.to_string())
    })
}

fn sap_atc_diagnostic_tags(finding: &SapAtcFindingPayload) -> Option<Vec<DiagnosticTag>> {
    let exempted = finding
        .exemption_state
        .as_deref()
        .is_some_and(sap_atc_state_is_suppressed);
    let suppressed = finding
        .suppression_state
        .as_deref()
        .is_some_and(sap_atc_state_is_suppressed);
    (exempted || suppressed).then(|| vec![DiagnosticTag::UNNECESSARY])
}

fn sap_atc_state_is_suppressed(value: &str) -> bool {
    let value = value.trim().to_ascii_lowercase();
    value.contains("exempt") || value.contains("suppress")
}

fn sort_lsp_diagnostics(diagnostics: &mut [Diagnostic]) {
    diagnostics.sort_by(|a, b| {
        a.range
            .start
            .line
            .cmp(&b.range.start.line)
            .then(a.range.start.character.cmp(&b.range.start.character))
    });
}

pub fn build_lsp_diagnostics(snapshot: &AnalysisSnapshot) -> Vec<Diagnostic> {
    let mut out = build_lsp_parse_diagnostics(snapshot, false);
    out.extend(build_lsp_semantic_diagnostics(snapshot));
    out.extend(build_lsp_lint_diagnostics(snapshot));
    sort_lsp_diagnostics(&mut out);
    out
}

fn candidate_key_for_open_sql_source(snapshot: &AnalysisSnapshot, range: &Range) -> Option<String> {
    let byte_range = range_to_byte_range_snapshot(snapshot, *range)?;
    let name = snapshot
        .symbols
        .sql_sources
        .iter()
        .find(|sql_source| sql_source.range == byte_range)
        .map(|sql_source| sql_source.name.as_ref())
        .or_else(|| {
            snapshot
                .symbols
                .sql_name_refs
                .iter()
                .find(|sql_ref| {
                    sql_ref.kind == abap_symbols::SqlNameRefKind::Source
                        && sql_ref.range == byte_range
                })
                .map(|sql_ref| sql_ref.name.as_ref())
        })?;
    Some(remote_candidate_key(&RemoteDependencyCandidate {
        name: name.to_string(),
        kind: "type".to_string(),
    }))
}

fn candidate_key_for_unresolved_type_name(name: &str) -> Option<String> {
    if !is_remote_lookup_candidate_after_local_resolution(name, "type") {
        return None;
    }
    Some(remote_candidate_key(&RemoteDependencyCandidate {
        name: name.to_string(),
        kind: "type".to_string(),
    }))
}

pub fn build_lsp_diagnostics_for_workspace(
    workspace: Option<&WorkspaceState>,
    snapshot: &AnalysisSnapshot,
) -> Vec<Diagnostic> {
    let include_fragment_policy = snapshot_is_workspace_include_fragment(workspace, snapshot);
    let mut diagnostics = build_lsp_parse_diagnostics(snapshot, include_fragment_policy);
    diagnostics.extend(build_lsp_semantic_diagnostics(snapshot));
    diagnostics.extend(build_lsp_lint_diagnostics(snapshot));
    let Some(workspace) = workspace else {
        sort_lsp_diagnostics(&mut diagnostics);
        return diagnostics;
    };
    diagnostics.extend(build_lsp_sap_atc_diagnostics(workspace, snapshot));
    sort_lsp_diagnostics(&mut diagnostics);

    for diagnostic in &mut diagnostics {
        let Some(severity) = diagnostic.severity else {
            continue;
        };
        let source = diagnostic.source.as_deref();
        if source != Some("abap-symbols") && source != Some(LINT_DIAGNOSTIC_SOURCE) {
            continue;
        }

        if diagnostic
            .message
            .contains("DDIC/repository lookup is not connected")
        {
            let Some(candidate_key) =
                candidate_key_for_open_sql_source(snapshot, &diagnostic.range)
            else {
                continue;
            };
            if !workspace.remote_lookup_failures.contains(&candidate_key) {
                continue;
            }

            diagnostic.severity = Some(DiagnosticSeverity::ERROR);
            if let Some(start) = diagnostic.message.find('\'') {
                if let Some(end_rel) = diagnostic.message[start + 1..].find('\'') {
                    let end = start + 1 + end_rel;
                    let name = &diagnostic.message[start + 1..end];
                    diagnostic.message = format!(
                        "Open SQL source '{}' was not found in the connected SAP system during DDIC/repository lookup",
                        name
                    );
                }
            }
            continue;
        }

        if severity == DiagnosticSeverity::ERROR
            && diagnostic.message.starts_with("unknown type '")
            && workspace_supports_dependency_store_resolution(workspace)
        {
            let Some(start) = diagnostic.message.find('\'') else {
                continue;
            };
            let Some(end_rel) = diagnostic.message[start + 1..].find('\'') else {
                continue;
            };
            let end = start + 1 + end_rel;
            let name = &diagnostic.message[start + 1..end];
            let Some(candidate_key) = candidate_key_for_unresolved_type_name(name) else {
                continue;
            };

            if workspace.remote_lookup_failures.contains(&candidate_key) {
                diagnostic.message = format!(
                    "Type '{}' was not found in the connected SAP system during DDIC/repository lookup",
                    name
                );
            } else {
                diagnostic.message = format!(
                    "Type '{}' is not verified against a SAP system (DDIC/repository lookup is not connected)",
                    name
                );
            }
        }
    }

    diagnostics
}

pub fn publish_diagnostics_params(
    state: &ServerState,
    snapshot: &AnalysisSnapshot,
) -> PublishDiagnosticsParams {
    let uri: Uri = snapshot
        .uri
        .as_ref()
        .parse()
        .expect("cached document URI must be a valid URL");
    let workspace = state.workspace_for_uri(snapshot.uri.as_ref());
    PublishDiagnosticsParams {
        uri,
        diagnostics: build_lsp_diagnostics_for_workspace(workspace, snapshot),
        version: Some(snapshot.version),
    }
}

pub fn hover(state: &ServerState, params: &HoverParams) -> Option<Hover> {
    let uri = normalize_lsp_uri(
        params
            .text_document_position_params
            .text_document
            .uri
            .as_str(),
    );
    let snapshot = snapshot_for_uri(state, &uri)?;
    let offset =
        position_to_offset_snapshot(&snapshot, params.text_document_position_params.position)?;
    if let Some(component) = snapshot.hovered_component_at(offset) {
        return structured_field_hover(&snapshot, component);
    }
    if let Some(call_target) = snapshot.hovered_call_target_at(offset) {
        return resolved_symbol_hover(&snapshot, call_target);
    }
    if let Some(named_argument) = snapshot.hovered_named_argument_at(offset) {
        return resolved_symbol_hover(&snapshot, named_argument);
    }
    if let Some(sql_ref) = snapshot.hovered_sql_name_ref_at(offset) {
        return resolved_symbol_hover(&snapshot, sql_ref);
    }
    if let Some(message) = snapshot.hovered_message_at(offset) {
        return resolved_symbol_hover(&snapshot, message);
    }
    if let Some(symbol) = snapshot.hovered_resolved_symbol_at(offset) {
        return resolved_symbol_hover(&snapshot, symbol);
    }
    hover_from_dependency_store_method_call(state, &uri, snapshot.as_ref(), offset)
}

fn hover_from_dependency_store_method_call(
    state: &ServerState,
    source_uri: &str,
    snapshot: &AnalysisSnapshot,
    offset: usize,
) -> Option<Hover> {
    let (target_snapshot, target_range) =
        dependency_method_target_at_offset(state, source_uri, snapshot, offset)?;
    let hover_offset = target_range.start.saturating_add(1);
    let symbol = target_snapshot.hovered_resolved_symbol_at(hover_offset)?;
    resolved_symbol_hover(&target_snapshot, symbol)
}

fn remote_dependency_candidate_at_offset(
    snapshot: &AnalysisSnapshot,
    offset: usize,
) -> Option<RemoteDependencyCandidate> {
    for edge in snapshot
        .symbols
        .include_edges
        .iter()
        .filter(|edge| edge.target.is_none())
    {
        if edge.range.start <= offset
            && offset <= edge.range.end
            && is_remote_lookup_candidate(edge.name.as_ref(), "include")
        {
            return Some(RemoteDependencyCandidate {
                name: edge.name.to_string(),
                kind: "include".to_string(),
            });
        }
    }

    if let Some(reference) = snapshot
        .symbols
        .semantic()
        .refs()
        .reference_at_offset(offset)
        && (reference.resolution.is_none() || reference.kind == ReferenceKind::MessageClass)
    {
        let kind = match reference.kind {
            ReferenceKind::Include => None,
            ReferenceKind::StaticTarget => Some("static"),
            ReferenceKind::TypeRef => Some("type"),
            ReferenceKind::StructuredDeclEnd => None,
            ReferenceKind::MessageClass => Some("message-class"),
            ReferenceKind::RoutineCall
                if reference.namespace == abap_symbols::Namespace::Routine =>
            {
                Some("function")
            }
            ReferenceKind::Identifier | ReferenceKind::RoutineCall => Some("symbol"),
        }?;
        let is_remote_candidate = match reference.kind {
            ReferenceKind::StaticTarget | ReferenceKind::TypeRef => {
                is_remote_lookup_candidate_after_local_resolution(reference.name.as_ref(), kind)
            }
            ReferenceKind::RoutineCall
                if reference.namespace == abap_symbols::Namespace::Routine =>
            {
                is_remote_lookup_candidate_after_local_resolution(reference.name.as_ref(), kind)
            }
            _ => is_remote_lookup_candidate(reference.name.as_ref(), kind),
        };
        if is_remote_candidate {
            return Some(RemoteDependencyCandidate {
                name: reference.name.to_string(),
                kind: kind.to_string(),
            });
        }
    }

    if let Some(sql_source) = snapshot.symbols.sql_sources.iter().find(|sql_source| {
        sql_source.resolution == SqlResolution::External
            && sql_source.range.start <= offset
            && offset <= sql_source.range.end
    }) && is_remote_lookup_candidate(sql_source.name.as_ref(), "type")
    {
        return Some(RemoteDependencyCandidate {
            name: sql_source.name.to_string(),
            kind: "type".to_string(),
        });
    }

    for call_site in &snapshot.symbols.call_sites {
        let candidate = match &call_site.target {
            abap_symbols::NamedArgumentTarget::Function { function_name } => {
                if !call_site_target_name_matches(snapshot, call_site, function_name, offset)
                    || !is_remote_lookup_candidate_after_local_resolution(
                        function_name.as_ref(),
                        "function",
                    )
                {
                    continue;
                }
                Some(RemoteDependencyCandidate {
                    name: function_name.to_string(),
                    kind: "function".to_string(),
                })
            }
            abap_symbols::NamedArgumentTarget::Report { report_name } => {
                if !call_site_target_name_matches(snapshot, call_site, report_name, offset)
                    || !is_remote_lookup_candidate_after_local_resolution(
                        report_name.as_ref(),
                        "report",
                    )
                {
                    continue;
                }
                Some(RemoteDependencyCandidate {
                    name: report_name.to_string(),
                    kind: "report".to_string(),
                })
            }
            _ => None,
        };
        if let Some(candidate) = candidate {
            return Some(candidate);
        }
    }

    None
}

fn call_site_target_name_matches(
    snapshot: &AnalysisSnapshot,
    call_site: &abap_symbols::CallSiteData,
    target_name: &Arc<str>,
    offset: usize,
) -> bool {
    call_site_target_name_range(snapshot.text.as_ref(), call_site, target_name)
        .is_some_and(|range| range.start <= offset && offset <= range.end)
}

fn call_site_target_name_range(
    text: &str,
    call_site: &abap_symbols::CallSiteData,
    target_name: &Arc<str>,
) -> Option<std::ops::Range<usize>> {
    let call_text = text.get(call_site.range.clone())?;
    let target_name = target_name.as_ref().to_ascii_lowercase();
    let target_text = call_text
        .find('(')
        .map(|args_start| &call_text[..args_start])
        .unwrap_or(call_text);
    let rel_start = target_text.to_ascii_lowercase().find(&target_name)?;
    let start = call_site.range.start + rel_start;
    let end = start + target_name.len();
    Some(start..end)
}

fn definition_from_dependency_store(
    state: &ServerState,
    source_uri: &str,
    candidate: &RemoteDependencyCandidate,
) -> Option<GotoDefinitionResponse> {
    let workspace = state.workspace_for_uri(source_uri)?;
    let profile = workspace_dependency_profile(workspace)?;
    let store = workspace_dependency_store(workspace)?;
    let start = Instant::now();
    let lookup = store
        .lookup_symbol(&profile, candidate.name.as_str(), candidate.kind.as_str())
        .ok()
        .flatten()?;
    let target_uri = dependency_document_uri_with_kind(
        &workspace.root_uri,
        lookup.artifact_id,
        lookup.object_name.as_str(),
        Some(lookup.object_kind.as_str()),
    );
    let target_snapshot = snapshot_for_uri(state, &target_uri);
    let range_source = if target_snapshot.is_some() {
        "cached-snapshot"
    } else {
        "artifact-source"
    };
    let range = if let Some(target_snapshot) = target_snapshot {
        byte_range_to_lsp_range_snapshot(
            target_snapshot.as_ref(),
            lookup.range_start..lookup.range_end,
        )?
    } else {
        let record = store
            .read_artifact_source(lookup.artifact_id)
            .ok()
            .flatten()?;
        byte_range_to_lsp_range_text(&record.source_text, lookup.range_start..lookup.range_end)?
    };
    let uri: Uri = target_uri.parse().ok()?;
    debug!(
        source_uri,
        candidate_name = %candidate.name,
        candidate_kind = %candidate.kind,
        target_uri = %target_uri,
        artifact_id = lookup.artifact_id,
        object_name = %lookup.object_name,
        object_kind = %lookup.object_kind,
        range_source,
        dependency_store_path = %workspace_dependency_store_path(workspace)
            .map(|path| path.display().to_string())
            .unwrap_or_else(|| "<none>".to_string()),
        dependency_profile = %profile.profile_key(),
        elapsed = ?start.elapsed(),
        "definition used dependency-store symbol index"
    );
    Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
}

fn definition_from_dependency_store_method_call(
    state: &ServerState,
    source_uri: &str,
    snapshot: &AnalysisSnapshot,
    offset: usize,
) -> Option<GotoDefinitionResponse> {
    let index_start = Instant::now();
    if let Some(location) =
        definition_from_dependency_store_method_index(state, source_uri, snapshot, offset)
    {
        debug!(
            source_uri,
            target_uri = location.uri.as_str(),
            elapsed = ?index_start.elapsed(),
            "definition used dependency-store artifact method index"
        );
        return Some(GotoDefinitionResponse::Scalar(location));
    }
    debug!(
        source_uri,
        elapsed = ?index_start.elapsed(),
        "dependency-store artifact method index missed; falling back to dependency snapshot"
    );
    let fallback_start = Instant::now();
    let (target_snapshot, target_range) =
        dependency_method_target_at_offset(state, source_uri, snapshot, offset)?;
    let uri: Uri = target_snapshot.uri.as_ref().parse().ok()?;
    let range = byte_range_to_lsp_range_snapshot(target_snapshot.as_ref(), target_range)?;
    debug!(
        source_uri,
        target_uri = %target_snapshot.uri,
        target_is_dependency = target_snapshot.is_dependency,
        elapsed = ?fallback_start.elapsed(),
        "definition used dependency snapshot method fallback"
    );
    Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
}

fn definition_from_dependency_store_method_index(
    state: &ServerState,
    source_uri: &str,
    snapshot: &AnalysisSnapshot,
    offset: usize,
) -> Option<Location> {
    let workspace = state.workspace_for_uri(source_uri)?;
    let profile = workspace_dependency_profile(workspace)?;
    let store = workspace_dependency_store(workspace)?;
    let (base_name, method_name) = dependency_static_method_call_at_offset(snapshot, offset)?;
    if !is_remote_lookup_candidate_after_local_resolution(base_name.as_ref(), "type") {
        return None;
    }

    let owner_lookup = store
        .lookup_symbol(&profile, base_name.as_ref(), "type")
        .ok()
        .flatten()?;
    let target_uri = dependency_document_uri_with_kind(
        &workspace.root_uri,
        owner_lookup.artifact_id,
        owner_lookup.object_name.as_str(),
        Some(owner_lookup.object_kind.as_str()),
    );
    if snapshot_for_uri(state, &target_uri).is_some_and(|snapshot| !snapshot.is_dependency) {
        return None;
    }

    let member_lookup = store
        .lookup_artifact_symbol(
            owner_lookup.artifact_id,
            method_name.as_ref(),
            &["class-member-implementation", "class-member"],
        )
        .ok()
        .flatten()?;
    let record = store
        .read_artifact_source(owner_lookup.artifact_id)
        .ok()
        .flatten()?;
    let range = byte_range_to_lsp_range_text(
        &record.source_text,
        member_lookup.range_start..member_lookup.range_end,
    )?;
    debug!(
        source_uri,
        base_name = %base_name,
        method_name = %method_name,
        target_uri = %target_uri,
        artifact_id = owner_lookup.artifact_id,
        object_name = %owner_lookup.object_name,
        object_kind = %owner_lookup.object_kind,
        object_type = %record.object_type,
        dependency_store_path = %workspace_dependency_store_path(workspace)
            .map(|path| path.display().to_string())
            .unwrap_or_else(|| "<none>".to_string()),
        dependency_profile = %profile.profile_key(),
        "dependency-store artifact method index hit"
    );
    Some(Location {
        uri: target_uri.parse().ok()?,
        range,
    })
}

fn dependency_static_method_call_at_offset(
    snapshot: &AnalysisSnapshot,
    offset: usize,
) -> Option<(&Arc<str>, &Arc<str>)> {
    for call_site in &snapshot.symbols.call_sites {
        let (base_name, method_name) = match &call_site.target {
            NamedArgumentTarget::Method {
                base_namespace,
                base_name,
                method_name,
            } if *base_namespace == Namespace::Type => (base_name, method_name),
            _ => continue,
        };
        let Some(range) =
            call_site_target_name_range(snapshot.text.as_ref(), call_site, method_name)
        else {
            continue;
        };
        if range.start <= offset && offset < range.end {
            return Some((base_name, method_name));
        }
    }

    None
}

fn dependency_method_target_at_offset(
    state: &ServerState,
    source_uri: &str,
    snapshot: &AnalysisSnapshot,
    offset: usize,
) -> Option<(Arc<AnalysisSnapshot>, std::ops::Range<usize>)> {
    let workspace = state.workspace_for_uri(source_uri)?;
    let profile = workspace_dependency_profile(workspace)?;
    let store = workspace_dependency_store(workspace)?;
    let (base_name, method_name) = dependency_static_method_call_at_offset(snapshot, offset)?;
    if !is_remote_lookup_candidate_after_local_resolution(base_name.as_ref(), "type") {
        return None;
    }

    let owner_lookup = store
        .lookup_symbol(&profile, base_name.as_ref(), "type")
        .ok()
        .flatten()?;
    let target_uri = dependency_document_uri_with_kind(
        &workspace.root_uri,
        owner_lookup.artifact_id,
        owner_lookup.object_name.as_str(),
        Some(owner_lookup.object_kind.as_str()),
    );
    let target_snapshot = match snapshot_for_uri(state, &target_uri) {
        Some(snapshot) => snapshot,
        None => {
            let record = store
                .read_artifact_source(owner_lookup.artifact_id)
                .ok()
                .flatten()?;
            dependency_document_snapshot_from_record(&target_uri, &record)
        }
    };
    let member = target_snapshot
        .symbols
        .class_members
        .iter()
        .find(|member| {
            member
                .name
                .as_ref()
                .eq_ignore_ascii_case(method_name.as_ref())
        })?;
    let decl_range = member.decl_range.clone();
    let Some(implementation) = member.implementation.clone() else {
        return Some((target_snapshot, decl_range));
    };
    let implementation_unit = &target_snapshot.project.units[implementation.unit.as_usize()];
    let implementation_snapshot = if implementation_unit.uri.as_ref() == target_uri.as_str() {
        Arc::clone(&target_snapshot)
    } else {
        match snapshot_for_uri(state, implementation_unit.uri.as_ref()) {
            Some(snapshot) => snapshot,
            None => return Some((target_snapshot, decl_range)),
        }
    };
    Some((
        implementation_snapshot,
        method_implementation_name_range(method_name.as_ref(), implementation.range),
    ))
}

fn method_implementation_name_range(
    method_name: &str,
    range: std::ops::Range<usize>,
) -> std::ops::Range<usize> {
    let name = method_name.rsplit('~').next().unwrap_or(method_name);
    if range.end.saturating_sub(range.start) >= name.len() {
        range.end - name.len()..range.end
    } else {
        range
    }
}

pub fn definition(
    state: &ServerState,
    params: &GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let uri = normalize_lsp_uri(
        params
            .text_document_position_params
            .text_document
            .uri
            .as_str(),
    );
    let snapshot = snapshot_for_uri(state, &uri)?;
    let offset =
        position_to_offset_snapshot(&snapshot, params.text_document_position_params.position)?;
    let Some(target) = snapshot.definition_at(offset) else {
        if let Some(definition) =
            definition_from_dependency_store_method_call(state, &uri, snapshot.as_ref(), offset)
        {
            return Some(definition);
        }
        let candidate = remote_dependency_candidate_at_offset(snapshot.as_ref(), offset)?;
        return definition_from_dependency_store(state, &uri, &candidate);
    };
    let target_snapshot = if target.uri.as_ref() == snapshot.uri.as_ref() {
        Arc::clone(&snapshot)
    } else {
        snapshot_for_uri(state, target.uri.as_ref())?
    };
    debug!(
        source_uri = %uri,
        target_uri = %target.uri,
        target_is_dependency = target_snapshot.is_dependency,
        "definition used analysis snapshot resolution"
    );
    let uri: Uri = target
        .uri
        .as_ref()
        .parse()
        .expect("cached document URI must be a valid URL");
    let range = byte_range_to_lsp_range_snapshot(target_snapshot.as_ref(), target.range)?;
    Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
}

pub fn references(state: &ServerState, params: &ReferenceParams) -> Option<Vec<Location>> {
    let uri = normalize_lsp_uri(params.text_document_position.text_document.uri.as_str());
    let snapshot = snapshot_for_uri(state, &uri)?;
    let offset = position_to_offset_snapshot(&snapshot, params.text_document_position.position)?;
    let references = cache_for_uri(state, &uri).references_for_snapshot(
        snapshot.as_ref(),
        offset,
        params.context.include_declaration,
    )?;
    let mut locations = Vec::with_capacity(references.len());
    for reference in references {
        let target_snapshot = if reference.uri.as_ref() == snapshot.uri.as_ref() {
            Arc::clone(&snapshot)
        } else {
            snapshot_for_uri(state, reference.uri.as_ref())?
        };
        let uri: Uri = reference
            .uri
            .as_ref()
            .parse()
            .expect("cached document URI must be a valid URL");
        let range = byte_range_to_lsp_range_snapshot(target_snapshot.as_ref(), reference.range)?;
        locations.push(Location { uri, range });
    }
    Some(locations)
}

pub fn prepare_rename(
    state: &ServerState,
    params: &TextDocumentPositionParams,
) -> Option<PrepareRenameResponse> {
    let uri = normalize_lsp_uri(params.text_document.uri.as_str());
    let snapshot = snapshot_for_uri(state, &uri)?;
    let offset = position_to_offset_snapshot(&snapshot, params.position)?;
    let plan = cache_for_uri(state, &uri).rename_plan_for_snapshot(snapshot.as_ref(), offset)?;
    let range = byte_range_to_lsp_range_snapshot(snapshot.as_ref(), plan.range)?;
    Some(PrepareRenameResponse::RangeWithPlaceholder {
        range,
        placeholder: plan.placeholder,
    })
}

pub fn rename(state: &ServerState, params: &RenameParams) -> Result<Option<WorkspaceEdit>, String> {
    let uri = normalize_lsp_uri(params.text_document_position.text_document.uri.as_str());
    let snapshot = match snapshot_for_uri(state, &uri) {
        Some(snapshot) => snapshot,
        None => return Ok(None),
    };
    let Some(offset) =
        position_to_offset_snapshot(&snapshot, params.text_document_position.position)
    else {
        return Ok(None);
    };
    let Some(plan) = cache_for_uri(state, &uri).rename_plan_for_snapshot(snapshot.as_ref(), offset)
    else {
        return Ok(None);
    };
    plan.validate_new_name(&params.new_name)?;

    let mut changes: HashMap<Uri, Vec<TextEdit>> = HashMap::new();
    for location in plan.locations {
        let target_snapshot = if location.uri.as_ref() == snapshot.uri.as_ref() {
            Arc::clone(&snapshot)
        } else {
            match snapshot_for_uri(state, location.uri.as_ref()) {
                Some(target_snapshot) => target_snapshot,
                None => return Ok(None),
            }
        };
        let uri: Uri = location
            .uri
            .as_ref()
            .parse()
            .expect("cached document URI must be a valid URL");
        let Some(range) =
            byte_range_to_lsp_range_snapshot(target_snapshot.as_ref(), location.range)
        else {
            return Ok(None);
        };
        changes.entry(uri).or_default().push(TextEdit {
            range,
            new_text: params.new_name.clone(),
        });
    }

    Ok(Some(WorkspaceEdit {
        changes: Some(changes),
        ..WorkspaceEdit::default()
    }))
}

pub fn code_actions(state: &ServerState, params: &CodeActionParams) -> Option<CodeActionResponse> {
    let uri = normalize_lsp_uri(params.text_document.uri.as_str());
    let snapshot = snapshot_for_uri(state, &uri)?;
    let mut actions = Vec::new();
    let mut seen = HashSet::new();

    for diagnostic in &params.context.diagnostics {
        let Some(NumberOrString::String(code)) = diagnostic.code.as_ref() else {
            continue;
        };
        if code != DIAGNOSTIC_CODE_MISSING_METHOD_IMPLEMENTATION {
            continue;
        }

        let Some(offset) = position_to_offset_snapshot(&snapshot, diagnostic.range.start) else {
            continue;
        };
        let Some(action) = snapshot.missing_method_implementation_action_at(offset) else {
            continue;
        };
        if !seen.insert((action.edit_range.start, action.edit_range.end)) {
            continue;
        }

        let Some(start) = offset_to_position_snapshot(snapshot.as_ref(), action.edit_range.start)
        else {
            continue;
        };
        let Some(end) = offset_to_position_snapshot(snapshot.as_ref(), action.edit_range.end)
        else {
            continue;
        };
        let uri: Uri = snapshot
            .uri
            .as_ref()
            .parse()
            .expect("cached document URI must be a valid URL");
        actions.push(CodeActionOrCommand::CodeAction(CodeAction {
            title: action.title,
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics: Some(vec![diagnostic.clone()]),
            edit: Some(WorkspaceEdit {
                changes: Some(HashMap::from([(
                    uri,
                    vec![TextEdit {
                        range: Range { start, end },
                        new_text: action.new_text,
                    }],
                )])),
                document_changes: None,
                change_annotations: None,
            }),
            is_preferred: Some(true),
            disabled: None,
            data: None,
            command: None,
        }));
    }

    if let Some(offset) = position_to_offset_snapshot(&snapshot, params.range.start)
        && let Some(action) = snapshot.method_parameter_comments_action_at(offset)
    {
        let Some(start) = offset_to_position_snapshot(snapshot.as_ref(), action.edit_range.start)
        else {
            return Some(actions);
        };
        let Some(end) = offset_to_position_snapshot(snapshot.as_ref(), action.edit_range.end)
        else {
            return Some(actions);
        };
        let uri: Uri = snapshot
            .uri
            .as_ref()
            .parse()
            .expect("cached document URI must be a valid URL");
        actions.push(CodeActionOrCommand::CodeAction(CodeAction {
            title: action.title,
            kind: Some(CodeActionKind::REFACTOR_REWRITE),
            diagnostics: None,
            edit: Some(WorkspaceEdit {
                changes: Some(HashMap::from([(
                    uri,
                    vec![TextEdit {
                        range: Range { start, end },
                        new_text: action.new_text,
                    }],
                )])),
                document_changes: None,
                change_annotations: None,
            }),
            is_preferred: None,
            disabled: None,
            data: None,
            command: None,
        }));
    }

    Some(actions)
}

fn resolved_symbol_hover(
    snapshot: &AnalysisSnapshot,
    info: abap_cache::HoveredSymbolInfo,
) -> Option<Hover> {
    let range = byte_range_to_lsp_range_snapshot(snapshot, info.range)?;
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: info.markdown_lines.join("\n\n"),
        }),
        range: Some(range),
    })
}

fn scalar_component_summary(
    field_owner_structure_name: Option<&Arc<str>>,
    field_name: &str,
) -> String {
    field_owner_structure_name
        .map(|s| s.as_ref())
        .and_then(|owner| abap_symbols::builtin_structure_field_description(owner, field_name))
        .map(str::to_string)
        .unwrap_or_else(|| "scalar component".to_string())
}

fn structured_field_hover(
    snapshot: &AnalysisSnapshot,
    component: abap_cache::HoveredComponentInfo,
) -> Option<Hover> {
    let range = byte_range_to_lsp_range_snapshot(snapshot, component.range.clone())?;
    let is_method = matches!(component.kind, abap_cache::HoveredComponentKind::Method);
    let mut lines = vec![format!("`{}`", component.field_name)];
    match &component.kind {
        abap_cache::HoveredComponentKind::Scalar => {
            lines.push(component.description.unwrap_or_else(|| {
                scalar_component_summary(
                    component.field_owner_structure_name.as_ref(),
                    component.field_name.as_ref(),
                )
            }))
        }
        abap_cache::HoveredComponentKind::Structured { structure_name } => {
            lines.push(format!("structured component of `{}`", structure_name))
        }
        abap_cache::HoveredComponentKind::Attribute => {
            if let Some(declaration) = &component.declaration {
                lines[0] = format!("```abap\n{}\n```", declaration);
            }
            let storage = if component.is_static_method {
                "static"
            } else {
                "instance"
            };
            lines.push(format!("{storage} attribute of `{}`", component.base_name));
        }
        abap_cache::HoveredComponentKind::Method => {
            if let Some(declaration) = &component.declaration {
                lines[0] = format!("```abap\n{}\n```", declaration);
            }
            let storage = if component.is_static_method {
                "static"
            } else {
                "instance"
            };
            lines.push(format!("{storage} method of `{}`", component.base_name));
        }
        abap_cache::HoveredComponentKind::Interface => {
            if let Some(declaration) = &component.declaration {
                lines[0] = format!("```abap\n{}\n```", declaration);
            }
            lines.push("interface".to_string());
        }
        abap_cache::HoveredComponentKind::Type => {
            if let Some(declaration) = &component.declaration {
                lines[0] = format!("```abap\n{}\n```", declaration);
            }
            lines.push(format!("type definition of `{}`", component.base_name));
        }
    }
    if let Some(declared_type) = component.declared_type {
        lines.push(format!("declared as `{}`", declared_type));
    }
    if let Some(value_clause_display) = component.value_clause_display {
        lines.push(format!(
            "```abap\nVALUE {}\n```",
            value_clause_display.trim()
        ));
    }
    if !is_method {
        let mut path = component.base_name.to_string();
        let separator = if component.base_namespace == abap_symbols::Namespace::Type {
            "=>"
        } else {
            "-"
        };
        for segment in &component.component_path {
            path.push_str(separator);
            path.push_str(segment.as_ref());
        }
        lines.push(format!("path: `{}`", path));
    }
    if component.in_type_position {
        lines.push("used in type position".to_string());
    }
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: lines.join("\n\n"),
        }),
        range: Some(range),
    })
}

pub fn completion(state: &ServerState, params: &CompletionParams) -> Option<CompletionResponse> {
    let uri = normalize_lsp_uri(params.text_document_position.text_document.uri.as_str());
    let snapshot = snapshot_for_uri(state, &uri)?;
    let offset = position_to_offset_snapshot(&snapshot, params.text_document_position.position)?;
    let completion = supplement_function_module_completion_from_local_exports(
        state,
        &uri,
        snapshot.as_ref(),
        snapshot.completion_at(offset),
        snapshot.callable_statement_completion_context_at(offset),
    )?;
    let range = byte_range_to_lsp_range_snapshot(snapshot.as_ref(), completion.replace_range)?;
    let items = completion
        .items
        .into_iter()
        .map(|item| {
            completion_item_to_lsp(
                &item,
                range,
                state.client_capabilities.completion_snippet_support,
            )
        })
        .collect();
    Some(CompletionResponse::Array(items))
}

fn supplement_function_module_completion_from_local_exports(
    state: &ServerState,
    uri: &str,
    _snapshot: &AnalysisSnapshot,
    completion: Option<abap_cache::CompletionInfo>,
    callable_context: Option<abap_cache::CallableStatementCompletionContext>,
) -> Option<abap_cache::CompletionInfo> {
    let Some(context) = callable_context else {
        return completion;
    };
    if context.kind != CallableCompletionKind::FunctionModule || context.prefix.is_empty() {
        return completion;
    }
    let Some(workspace) = state.workspace_for_uri(uri) else {
        return completion;
    };
    let Some(root_path) = file_uri_to_path(&workspace.root_uri) else {
        return completion;
    };
    let config = local_export_config_for_source(&root_path, uri);
    if !config.uses_local_exports() {
        return completion;
    }

    let documents = {
        let mut resolver = workspace
            .local_export_resolver
            .lock()
            .unwrap_or_else(|error| error.into_inner());
        resolve_local_export_function_module_documents_by_prefix(
            &config.roots,
            &mut resolver,
            context.prefix.as_ref(),
            LOCAL_EXPORT_FUNCTION_MODULE_COMPLETION_LIMIT,
        )
    };
    if documents.is_empty() {
        return completion;
    }

    let mut replace_range = context.replace_range.clone();
    let mut in_type_position = false;
    let mut items = match completion {
        Some(completion) => {
            replace_range = completion.replace_range;
            in_type_position = completion.in_type_position;
            completion.items
        }
        None => Vec::new(),
    };
    let mut seen: HashSet<_> = items.iter().map(cache_completion_item_key).collect();

    for document in documents {
        for item in function_module_completion_items_from_source(
            document.uri.as_ref(),
            document.text.as_str(),
            document.object_name.clone(),
        ) {
            let key = item.name.to_ascii_lowercase();
            if !key.starts_with(context.prefix.as_ref()) || !seen.insert(key) {
                continue;
            }
            items.push(abap_cache::CompletionItem::Callable(item));
        }
    }

    if items.is_empty() {
        return None;
    }
    items.sort_by(|left, right| {
        cache_completion_item_name(left).cmp(cache_completion_item_name(right))
    });
    Some(abap_cache::CompletionInfo {
        replace_range,
        items,
        in_type_position,
    })
}

pub fn semantic_tokens(
    state: &ServerState,
    params: &SemanticTokensParams,
) -> Option<SemanticTokens> {
    let uri = normalize_lsp_uri(params.text_document.uri.as_str());
    let snapshot = snapshot_for_uri(state, &uri)?;
    Some(sem_tokens::build_semantic_tokens(snapshot.as_ref()))
}

pub fn folding_ranges(
    state: &ServerState,
    params: &FoldingRangeParams,
) -> Option<Vec<FoldingRange>> {
    let uri = normalize_lsp_uri(params.text_document.uri.as_str());
    let snapshot = snapshot_for_uri(state, &uri)?;
    Some(folding_ranges_from_text(snapshot.text.as_ref()))
}

fn folding_ranges_from_text(text: &str) -> Vec<FoldingRange> {
    let lines: Vec<_> = text.lines().collect();
    let mut ranges = Vec::new();
    let mut stack = Vec::new();

    for (line_idx, raw_line) in lines.iter().enumerate() {
        let code = raw_line.trim();
        if code.is_empty() || code.starts_with('"') {
            continue;
        }
        let Some(keyword) = leading_abap_keyword(code) else {
            continue;
        };
        let line = line_idx as u32;

        match keyword.as_str() {
            "IF" => stack.push(FoldingBlock {
                kind: FoldingBlockKind::If,
                start_line: line,
                end_keyword: "ENDIF",
                current_arm_start_line: Some(line),
            }),
            "ELSEIF" => {
                if let Some(entry) = find_nearest_folding_block(&mut stack, FoldingBlockKind::If) {
                    push_folding_range(
                        &mut ranges,
                        entry.current_arm_start_line.unwrap_or(entry.start_line),
                        line.saturating_sub(1),
                    );
                    entry.current_arm_start_line = Some(line);
                }
            }
            "ELSE" => {
                if let Some(entry) = find_nearest_arm_folding_block(&mut stack) {
                    push_folding_range(
                        &mut ranges,
                        entry.current_arm_start_line.unwrap_or(entry.start_line),
                        line.saturating_sub(1),
                    );
                    entry.current_arm_start_line = Some(line);
                }
            }
            "ENDIF" => {
                if let Some(entry) =
                    pop_nearest_folding_block(&mut stack, FoldingBlockKind::If, "ENDIF")
                {
                    push_folding_range(
                        &mut ranges,
                        entry.current_arm_start_line.unwrap_or(entry.start_line),
                        line.saturating_sub(1),
                    );
                }
            }
            "CLASS" if starts_with_keyword_not_component(code, "CLASS") => {
                stack.push(simple_folding_block(line, "ENDCLASS"));
            }
            "METHOD" => stack.push(simple_folding_block(line, "ENDMETHOD")),
            "CASE" => stack.push(FoldingBlock {
                kind: FoldingBlockKind::Case,
                start_line: line,
                end_keyword: "ENDCASE",
                current_arm_start_line: None,
            }),
            "WHEN" => {
                if let Some(entry) = find_nearest_folding_block(&mut stack, FoldingBlockKind::Case)
                {
                    push_folding_range(
                        &mut ranges,
                        entry.current_arm_start_line.unwrap_or(entry.start_line),
                        line.saturating_sub(1),
                    );
                    entry.current_arm_start_line = Some(line);
                } else if let Some(entry) =
                    find_nearest_folding_block(&mut stack, FoldingBlockKind::If)
                {
                    push_folding_range(
                        &mut ranges,
                        entry.current_arm_start_line.unwrap_or(entry.start_line),
                        line.saturating_sub(1),
                    );
                    entry.current_arm_start_line = Some(line);
                }
            }
            "ENDCASE" => {
                if let Some(entry) =
                    pop_nearest_folding_block(&mut stack, FoldingBlockKind::Case, "ENDCASE")
                {
                    push_folding_range(
                        &mut ranges,
                        entry.current_arm_start_line.unwrap_or(entry.start_line),
                        line.saturating_sub(1),
                    );
                    push_folding_range(&mut ranges, entry.start_line, line.saturating_sub(1));
                }
            }
            "INTERFACE" if starts_with_keyword_not_component(code, "INTERFACE") => {
                stack.push(simple_folding_block(line, "ENDINTERFACE"));
            }
            "TRY" => stack.push(simple_folding_block(line, "ENDTRY")),
            "LOOP" => stack.push(simple_folding_block(line, "ENDLOOP")),
            "DO" => stack.push(simple_folding_block(line, "ENDDO")),
            "WHILE" => stack.push(simple_folding_block(line, "ENDWHILE")),
            _ => {
                if let Some(entry) = pop_nearest_simple_folding_block(&mut stack, &keyword) {
                    push_folding_range(&mut ranges, entry.start_line, line.saturating_sub(1));
                }
            }
        }
    }

    ranges
}

fn simple_folding_block(start_line: u32, end_keyword: &'static str) -> FoldingBlock {
    FoldingBlock {
        kind: FoldingBlockKind::Simple,
        start_line,
        end_keyword,
        current_arm_start_line: None,
    }
}

fn leading_abap_keyword(line: &str) -> Option<String> {
    let end = line
        .char_indices()
        .take_while(|(idx, ch)| {
            (*idx == 0 && ch.is_ascii_alphabetic())
                || (*idx > 0 && (ch.is_ascii_alphanumeric() || *ch == '-'))
        })
        .last()
        .map(|(idx, ch)| idx + ch.len_utf8())?;
    let keyword = &line[..end];
    keyword
        .chars()
        .next()
        .is_some_and(|ch| ch.is_ascii_alphabetic())
        .then(|| keyword.to_ascii_uppercase())
}

fn starts_with_keyword_not_component(line: &str, keyword: &str) -> bool {
    let rest = line.get(keyword.len()..).unwrap_or_default();
    rest.is_empty() || !rest.starts_with('-')
}

fn find_nearest_folding_block(
    stack: &mut [FoldingBlock],
    kind: FoldingBlockKind,
) -> Option<&mut FoldingBlock> {
    stack.iter_mut().rev().find(|entry| entry.kind == kind)
}

fn find_nearest_arm_folding_block(stack: &mut [FoldingBlock]) -> Option<&mut FoldingBlock> {
    stack
        .iter_mut()
        .rev()
        .find(|entry| matches!(entry.kind, FoldingBlockKind::If | FoldingBlockKind::Case))
}

fn pop_nearest_folding_block(
    stack: &mut Vec<FoldingBlock>,
    kind: FoldingBlockKind,
    end_keyword: &str,
) -> Option<FoldingBlock> {
    let idx = stack
        .iter()
        .rposition(|entry| entry.kind == kind && entry.end_keyword == end_keyword)?;
    Some(stack.remove(idx))
}

fn pop_nearest_simple_folding_block(
    stack: &mut Vec<FoldingBlock>,
    keyword: &str,
) -> Option<FoldingBlock> {
    let idx = stack.iter().rposition(|entry| {
        entry.kind == FoldingBlockKind::Simple && entry.end_keyword == keyword
    })?;
    Some(stack.remove(idx))
}

fn push_folding_range(ranges: &mut Vec<FoldingRange>, start_line: u32, end_line: u32) {
    if end_line > start_line {
        ranges.push(FoldingRange {
            start_line,
            start_character: None,
            end_line,
            end_character: None,
            kind: None,
            collapsed_text: None,
        });
    }
}

pub fn inlay_hints(state: &ServerState, params: &InlayHintParams) -> Option<Vec<InlayHint>> {
    let uri = normalize_lsp_uri(params.text_document.uri.as_str());
    let snapshot = snapshot_for_uri(state, &uri)?;
    let byte_range = range_to_byte_range_snapshot(snapshot.as_ref(), params.range)?;
    let mut hint_infos = snapshot.perform_parameter_inlay_hints_in_range(byte_range.clone());
    hint_infos.extend(snapshot.function_module_parameter_inlay_hints_in_range(byte_range.clone()));
    hint_infos.extend(snapshot.method_parameter_inlay_hints_in_range(byte_range.clone()));
    hint_infos.sort_by_key(|hint| hint.position);
    let mut hints: Vec<_> = hint_infos
        .into_iter()
        .filter_map(|hint| {
            let label = if hint.trailing_colon {
                format!("{}:", hint.label)
            } else {
                hint.label.to_string()
            };
            Some(InlayHint {
                position: offset_to_position_snapshot(snapshot.as_ref(), hint.position)?,
                label: label.into(),
                kind: Some(InlayHintKind::PARAMETER),
                text_edits: None,
                tooltip: Some(
                    MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: hint.tooltip_markdown,
                    }
                    .into(),
                ),
                padding_left: hint.padding_left.then_some(true),
                padding_right: hint.padding_right.then_some(true),
                data: None,
            })
        })
        .collect();
    hints.extend(
        snapshot
            .inline_variable_type_inlay_hints_in_range(byte_range)
            .into_iter()
            .filter_map(|hint| {
                Some(InlayHint {
                    position: offset_to_position_snapshot(snapshot.as_ref(), hint.position)?,
                    label: hint.label.to_string().into(),
                    kind: Some(InlayHintKind::TYPE),
                    text_edits: None,
                    tooltip: Some(
                        MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: hint.tooltip_markdown,
                        }
                        .into(),
                    ),
                    padding_left: Some(true),
                    padding_right: None,
                    data: None,
                })
            }),
    );
    hints.sort_by_key(|hint| {
        (
            hint.position.line,
            hint.position.character,
            match hint.kind {
                Some(InlayHintKind::PARAMETER) => 0u8,
                Some(InlayHintKind::TYPE) => 1u8,
                _ => 2u8,
            },
        )
    });
    Some(hints)
}

pub fn initialize_result(config: &ServerConfig) -> InitializeResult {
    InitializeResult {
        server_info: Some(lsp_types::ServerInfo {
            name: config.name.to_owned(),
            version: Some(config.version.to_owned()),
        }),
        capabilities: ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            completion_provider: Some(CompletionOptions {
                trigger_characters: Some(vec![
                    "<".to_string(),
                    "-".to_string(),
                    ">".to_string(),
                    "~".to_string(),
                    "(".to_string(),
                ]),
                ..CompletionOptions::default()
            }),
            definition_provider: Some(OneOf::Left(true)),
            references_provider: Some(OneOf::Left(true)),
            rename_provider: Some(OneOf::Right(RenameOptions {
                prepare_provider: Some(true),
                work_done_progress_options: Default::default(),
            })),
            inlay_hint_provider: Some(OneOf::Right(InlayHintServerCapabilities::Options(
                InlayHintOptions {
                    resolve_provider: None,
                    work_done_progress_options: Default::default(),
                },
            ))),
            semantic_tokens_provider: Some(
                SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                    legend: sem_tokens::semantic_tokens_legend(),
                    full: Some(SemanticTokensFullOptions::Bool(true)),
                    range: None,
                    work_done_progress_options: Default::default(),
                }),
            ),
            folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
            code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
            ..ServerCapabilities::default()
        },
    }
}

fn byte_range_to_lsp_range_snapshot(
    snapshot: &AnalysisSnapshot,
    range: std::ops::Range<usize>,
) -> Option<Range> {
    Some(Range {
        start: offset_to_position_snapshot(snapshot, range.start)?,
        end: offset_to_position_snapshot(snapshot, range.end)?,
    })
}

fn byte_range_to_lsp_range_text(text: &str, range: std::ops::Range<usize>) -> Option<Range> {
    let line_index = abap_cache::LineIndex::new(text);
    Some(Range {
        start: offset_to_position_text(text, &line_index, range.start)?,
        end: offset_to_position_text(text, &line_index, range.end)?,
    })
}

fn offset_to_position_text(
    text: &str,
    line_index: &abap_cache::LineIndex,
    offset: usize,
) -> Option<Position> {
    let (line, character) = line_index.offset_to_line_utf16_position(text, offset)?;
    Some(Position { line, character })
}

#[cfg(test)]
fn offset_to_position(text: &str, offset: usize) -> Option<Position> {
    if offset > text.len() {
        return None;
    }
    let mut line = 0u32;
    let mut line_start = 0usize;
    for (idx, ch) in text.char_indices() {
        if idx >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            line_start = idx + ch.len_utf8();
        }
    }
    let line_end = text[line_start..]
        .find('\n')
        .map(|rel| line_start + rel)
        .unwrap_or(text.len());
    let line_text = text[line_start..line_end]
        .strip_suffix('\r')
        .unwrap_or(&text[line_start..line_end]);
    if offset < line_start || offset > line_start + line_text.len() {
        return None;
    }
    let character = line_text[..offset - line_start]
        .chars()
        .map(|ch| ch.len_utf16() as u32)
        .sum();
    Some(Position { line, character })
}

fn offset_to_position_snapshot(snapshot: &AnalysisSnapshot, offset: usize) -> Option<Position> {
    let (line, character) = snapshot.offset_to_line_utf16_position(offset)?;
    Some(Position { line, character })
}

fn position_to_offset_snapshot(snapshot: &AnalysisSnapshot, position: Position) -> Option<usize> {
    snapshot.line_utf16_position_to_offset(position.line, position.character)
}

fn range_to_byte_range_snapshot(
    snapshot: &AnalysisSnapshot,
    range: Range,
) -> Option<std::ops::Range<usize>> {
    Some(
        position_to_offset_snapshot(snapshot, range.start)?
            ..position_to_offset_snapshot(snapshot, range.end)?,
    )
}

fn cache_completion_item_name(item: &abap_cache::CompletionItem) -> &str {
    match item {
        abap_cache::CompletionItem::Selector(item) => item.name.as_ref(),
        abap_cache::CompletionItem::NamedArgument(item) => item.name.as_ref(),
        abap_cache::CompletionItem::Symbol(item) => item.name.as_ref(),
        abap_cache::CompletionItem::Template(item) => item.name.as_ref(),
        abap_cache::CompletionItem::Callable(item) => item.name.as_ref(),
        abap_cache::CompletionItem::Keyword(item) => item.name.as_ref(),
    }
}

fn cache_completion_item_key(item: &abap_cache::CompletionItem) -> String {
    cache_completion_item_name(item).to_ascii_lowercase()
}

fn completion_item_to_lsp(
    item: &abap_cache::CompletionItem,
    range: Range,
    snippet_support: bool,
) -> CompletionItem {
    let (label, kind, detail, documentation, plain_text, snippet_text) = match item {
        abap_cache::CompletionItem::Selector(item) => {
            let (detail, documentation) = completion_item_metadata(item);
            (
                item.name.to_string(),
                Some(match item.kind {
                    abap_cache::HoveredComponentKind::Method => CompletionItemKind::METHOD,
                    abap_cache::HoveredComponentKind::Interface => CompletionItemKind::INTERFACE,
                    abap_cache::HoveredComponentKind::Type => CompletionItemKind::TYPE_PARAMETER,
                    abap_cache::HoveredComponentKind::Attribute
                    | abap_cache::HoveredComponentKind::Scalar
                    | abap_cache::HoveredComponentKind::Structured { .. } => {
                        CompletionItemKind::FIELD
                    }
                }),
                detail,
                documentation,
                item.insertion.plain_text.clone(),
                item.insertion.snippet_text.clone(),
            )
        }
        abap_cache::CompletionItem::NamedArgument(item) => {
            let (detail, documentation) = named_argument_completion_item_metadata(item);
            (
                item.name.to_string(),
                Some(CompletionItemKind::VARIABLE),
                detail,
                documentation,
                item.insertion.plain_text.clone(),
                item.insertion.snippet_text.clone(),
            )
        }
        abap_cache::CompletionItem::Symbol(item) => {
            let (detail, documentation) = symbol_completion_item_metadata(item);
            (
                item.name.to_string(),
                Some(symbol_completion_item_kind(item.kind)),
                detail,
                documentation,
                item.insertion.plain_text.clone(),
                item.insertion.snippet_text.clone(),
            )
        }
        abap_cache::CompletionItem::Template(item) => (
            item.name.to_string(),
            Some(CompletionItemKind::SNIPPET),
            item.detail.clone(),
            None,
            item.insertion.plain_text.clone(),
            item.insertion.snippet_text.clone(),
        ),
        abap_cache::CompletionItem::Callable(item) => {
            let (detail, documentation) = callable_completion_item_metadata(item);
            (
                item.name.to_string(),
                Some(CompletionItemKind::FUNCTION),
                detail,
                documentation,
                item.insertion.plain_text.clone(),
                item.insertion.snippet_text.clone(),
            )
        }
        abap_cache::CompletionItem::Keyword(item) => (
            item.name.to_string(),
            Some(CompletionItemKind::KEYWORD),
            Some("ABAP keyword".to_string()),
            None,
            item.insertion.plain_text.clone(),
            item.insertion.snippet_text.clone(),
        ),
    };
    let (new_text, insert_text_format) = if snippet_support {
        if let Some(snippet_text) = snippet_text {
            (snippet_text, Some(InsertTextFormat::SNIPPET))
        } else {
            (plain_text, None)
        }
    } else {
        (plain_text, None)
    };
    CompletionItem {
        sort_text: Some(completion_item_sort_text(item, &label)),
        label,
        kind,
        detail,
        documentation,
        insert_text_format,
        text_edit: Some(lsp_types::CompletionTextEdit::Edit(TextEdit {
            range,
            new_text,
        })),
        ..CompletionItem::default()
    }
}

fn completion_item_sort_text(item: &abap_cache::CompletionItem, label: &str) -> String {
    let priority = match item {
        abap_cache::CompletionItem::Keyword(_) => "2",
        _ => "1",
    };
    format!("{priority}:{}", label.to_ascii_lowercase())
}

fn completion_item_metadata(
    item: &abap_cache::SelectorCompletionItem,
) -> (Option<String>, Option<Documentation>) {
    let mut lines = vec![format!("`{}`", item.name)];
    let detail = match &item.kind {
        abap_cache::HoveredComponentKind::Scalar => {
            let summary = scalar_component_summary(
                item.field_owner_structure_name.as_ref(),
                item.name.as_ref(),
            );
            lines.push(summary.clone());
            item.declared_type.clone().or(Some(summary))
        }
        abap_cache::HoveredComponentKind::Structured { structure_name } => {
            lines.push(format!("structured component of `{}`", structure_name));
            Some(match &item.declared_type {
                Some(type_ref) => format!("{type_ref} -> {structure_name}"),
                None => format!("structured component -> {structure_name}"),
            })
        }
        abap_cache::HoveredComponentKind::Attribute => {
            if let Some(declaration) = &item.declaration {
                lines[0] = format!("```abap\n{}\n```", declaration);
            }
            lines.push("class attribute".to_string());
            item.declaration.clone()
        }
        abap_cache::HoveredComponentKind::Method => {
            if let Some(declaration) = &item.declaration {
                lines[0] = format!("```abap\n{}\n```", declaration);
            }
            lines.push("static method".to_string());
            item.declaration.clone()
        }
        abap_cache::HoveredComponentKind::Interface => {
            if let Some(declaration) = &item.declaration {
                lines[0] = format!("```abap\n{}\n```", declaration);
            }
            lines.push("interface".to_string());
            item.declaration.clone()
        }
        abap_cache::HoveredComponentKind::Type => {
            if let Some(declaration) = &item.declaration {
                lines[0] = format!("```abap\n{}\n```", declaration);
            }
            lines.push("type definition".to_string());
            item.declared_type
                .clone()
                .or_else(|| item.declaration.clone())
        }
    };
    if let Some(declared_type) = &item.declared_type {
        lines.push(format!("declared as `{}`", declared_type));
    }
    let documentation = Some(Documentation::MarkupContent(MarkupContent {
        kind: MarkupKind::Markdown,
        value: lines.join("\n\n"),
    }));
    (detail, documentation)
}

fn symbol_completion_item_kind(kind: abap_symbols::SymbolKind) -> CompletionItemKind {
    match kind {
        abap_symbols::SymbolKind::BuiltinType | abap_symbols::SymbolKind::TypeDef => {
            CompletionItemKind::TYPE_PARAMETER
        }
        abap_symbols::SymbolKind::Class => CompletionItemKind::CLASS,
        abap_symbols::SymbolKind::Interface => CompletionItemKind::INTERFACE,
        abap_symbols::SymbolKind::EnumMember => CompletionItemKind::ENUM_MEMBER,
        abap_symbols::SymbolKind::Constant | abap_symbols::SymbolKind::BuiltinConstant => {
            CompletionItemKind::CONSTANT
        }
        abap_symbols::SymbolKind::Form
        | abap_symbols::SymbolKind::Method
        | abap_symbols::SymbolKind::Module
        | abap_symbols::SymbolKind::Event
        | abap_symbols::SymbolKind::BuiltinRoutine => CompletionItemKind::FUNCTION,
        _ => CompletionItemKind::VARIABLE,
    }
}

fn symbol_completion_kind_label(kind: abap_symbols::SymbolKind) -> &'static str {
    match kind {
        abap_symbols::SymbolKind::BuiltinType => "built-in type",
        abap_symbols::SymbolKind::BuiltinRoutine => "built-in routine",
        abap_symbols::SymbolKind::BuiltinConstant => "built-in constant",
        abap_symbols::SymbolKind::BuiltinVariable => "built-in variable",
        abap_symbols::SymbolKind::Variable => "variable",
        abap_symbols::SymbolKind::Constant => "constant",
        abap_symbols::SymbolKind::EnumMember => "enum member",
        abap_symbols::SymbolKind::TypeDef => "type definition",
        abap_symbols::SymbolKind::FieldSymbol => "field symbol",
        abap_symbols::SymbolKind::Form => "form",
        abap_symbols::SymbolKind::Parameter => "parameter",
        abap_symbols::SymbolKind::Class => "class",
        abap_symbols::SymbolKind::Interface => "interface",
        abap_symbols::SymbolKind::Method => "method",
        abap_symbols::SymbolKind::Field => "field",
        abap_symbols::SymbolKind::Include => "include program",
        abap_symbols::SymbolKind::Event => "event",
        abap_symbols::SymbolKind::Module => "module",
        abap_symbols::SymbolKind::Control => "control",
        abap_symbols::SymbolKind::Report => "report",
    }
}

fn symbol_completion_item_metadata(
    item: &abap_cache::SymbolCompletionItem,
) -> (Option<String>, Option<Documentation>) {
    let mut lines = vec![
        format!("`{}`", item.name),
        symbol_completion_kind_label(item.kind).to_string(),
    ];
    if let Some(declaration) = &item.declaration {
        lines[0] = format!("```abap\n{}\n```", declaration);
    }
    if let Some(declared_type) = &item.declared_type {
        lines.push(format!("declared as `{declared_type}`"));
    }
    let documentation = Some(Documentation::MarkupContent(MarkupContent {
        kind: MarkupKind::Markdown,
        value: lines.join("\n\n"),
    }));
    (
        item.declaration.clone().or(item.declared_type.clone()),
        documentation,
    )
}

fn named_argument_completion_item_metadata(
    item: &abap_cache::NamedArgumentCompletionItem,
) -> (Option<String>, Option<Documentation>) {
    let mut lines = vec![format!("`{}`", item.name), "Parameter".to_string()];
    if let Some(declared_type) = &item.declared_type {
        lines.push(format!("declared as `{declared_type}`"));
    }
    let documentation = Some(Documentation::MarkupContent(MarkupContent {
        kind: MarkupKind::Markdown,
        value: lines.join("\n\n"),
    }));
    (
        item.declaration.clone().or(item.declared_type.clone()),
        documentation,
    )
}

fn callable_completion_item_metadata(
    item: &abap_cache::CallableCompletionItem,
) -> (Option<String>, Option<Documentation>) {
    let mut lines = vec![format!("`{}`", item.name)];
    if let Some(declaration) = &item.declaration {
        lines[0] = format!("```abap\n{}\n```", declaration);
    }
    lines.push(
        match item.kind {
            abap_cache::CallableCompletionKind::FunctionModule => "function module",
            abap_cache::CallableCompletionKind::Form => "form routine",
        }
        .to_string(),
    );
    let documentation = Some(Documentation::MarkupContent(MarkupContent {
        kind: MarkupKind::Markdown,
        value: lines.join("\n\n"),
    }));
    (item.declaration.clone(), documentation)
}

#[cfg(test)]
mod tests {
    use abap_cache::{
        ABAP_LSP_IGNORED_CALL_FUNCTION_RESULT, ABAP_LSP_SELECT_SINGLE_WITHOUT_FULL_KEY,
        ABAP_LSP_SELECT_STAR, ABAP_LSP_UNREACHABLE_CODE, DocumentInput, DocumentStore,
        ManifestPerformance, ManifestResolution, ManifestUnit, ManifestUnitMember,
        WorkspaceDocument, WorkspaceManifest, path_to_file_uri,
    };
    use abap_dependency_store::StoredArtifactInput;
    use abap_symbols::DiagnosticKind;
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::str::FromStr;
    use std::sync::Arc;
    use std::time::{Duration, SystemTime, UNIX_EPOCH};

    use lsp_types::{
        CodeActionContext, CodeActionOrCommand, CompletionContext, CompletionTriggerKind,
        DiagnosticSeverity, DidChangeTextDocumentParams, DidOpenTextDocumentParams, Documentation,
        FoldingRange, GotoDefinitionResponse, HoverContents, InlayHintKind, InlayHintLabel,
        InlayHintTooltip, InsertTextFormat, NumberOrString, Position, PrepareRenameResponse, Range,
        SemanticTokensParams, TextDocumentContentChangeEvent, TextDocumentIdentifier,
        TextDocumentItem, TextDocumentPositionParams, Uri, VersionedTextDocumentIdentifier,
    };

    use crate::sem_tokens;

    use super::{
        CodeActionParams, CompletionParams, CompletionResponse, DEPENDENCY_CACHE_REFRESH_REQUESTED,
        DIAGNOSTIC_CODE_MISSING_METHOD_IMPLEMENTATION, DependencyArtifactPayload,
        GotoDefinitionParams, HoverParams, InlayHintParams, LINT_DIAGNOSTIC_SOURCE,
        REMOTE_DEPENDENCIES_UPDATED, RESOLVE_REMOTE_DEPENDENCIES, ReadDependencyDocumentParams,
        ReferenceParams, RemoteDependencyCandidate, RenameParams, SAP_ATC_DIAGNOSTIC_SOURCE,
        SapAtcFindingPayload, SapAtcResultsUpdatedParams, SapAtcSourceLocationPayload, ServerState,
        StoreRemoteDependencyArtifactsParams, WORKSPACE_MANIFEST_UPDATED,
        WorkspaceManifestUpdatedParams, WorkspaceState, build_lsp_diagnostics,
        build_lsp_diagnostics_for_workspace, build_remote_dependency_batch_for_workspace,
        build_remote_dependency_refresh_for_workspace, build_remote_dependency_request,
        build_remote_dependency_request_retrying_negatives,
        build_remote_dependency_requests_for_workspace,
        clear_local_export_dependency_candidate_cache_for_tests, code_actions,
        collect_local_export_dependency_candidates,
        collect_local_export_dependency_candidates_profiled, collect_remote_dependency_candidates,
        collect_remote_dependency_candidates_for_workspace_batch, completion, definition,
        dependency_document_input_from_payload_with_kind, dependency_document_uri,
        extract_stored_dependency_symbols, folding_ranges_from_text,
        handle_dependency_cache_refresh_requested, handle_remote_dependencies_updated,
        handle_sap_atc_results_updated, hover, hydrate_workspace_dependency_documents,
        initialize_result, inlay_hints, normalize_lsp_uri, offset_to_position, prepare_rename,
        publish_changed_document, publish_changed_document_mut, publish_open_document,
        publish_open_document_mut, read_dependency_document, references, refresh_workspace, rename,
        semantic_tokens, snapshot_for_uri, stage_workspace_preview_snapshot,
        store_local_export_dependency_candidates, store_remote_dependency_artifacts,
        workspace_committed_build_plan, workspace_dependency_store,
        workspace_manifest_diagnostics_params,
    };

    fn temp_workspace_path(name: &str) -> PathBuf {
        let mut path = std::env::temp_dir();
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time")
            .as_nanos();
        path.push(format!("abap_lsp_{name}_{unique}"));
        path
    }

    fn folding_line_ranges(ranges: &[FoldingRange]) -> Vec<(u32, u32)> {
        ranges
            .iter()
            .map(|range| (range.start_line, range.end_line))
            .collect()
    }

    fn configure_test_dependency_store(state: &mut ServerState, workspace_path: &Path) {
        state.dependency_store_path_override = Some(
            workspace_path
                .join("dependency-store")
                .join("dependency-cache.sqlite3"),
        );
    }

    fn dependency_uri_for_object_name(
        state: &ServerState,
        workspace_uri: &str,
        object_name: &str,
    ) -> String {
        let workspace = state
            .workspaces
            .get(&normalize_lsp_uri(workspace_uri))
            .expect("workspace");
        let normalized_name = object_name.trim().to_ascii_lowercase();
        workspace
            .cache
            .uris()
            .into_iter()
            .find_map(|uri| {
                workspace
                    .cache
                    .get(uri.as_ref())
                    .filter(|snapshot| {
                        snapshot
                            .object_name
                            .as_deref()
                            .is_some_and(|name| name.eq_ignore_ascii_case(&normalized_name))
                    })
                    .map(|_| uri.to_string())
            })
            .expect("dependency uri")
    }

    fn dependency_text_for_uri(state: &ServerState, uri: &str) -> String {
        read_dependency_document(
            state,
            &ReadDependencyDocumentParams {
                uri: uri.to_string(),
            },
        )
        .expect("read dependency document")
        .expect("dependency document")
        .source_text
    }

    fn inlay_hint_label_string(hint: &lsp_types::InlayHint) -> &str {
        let InlayHintLabel::String(label) = &hint.label else {
            panic!("expected string label");
        };
        label
    }

    fn semantic_token_type_at(
        tokens: &lsp_types::SemanticTokens,
        line: u32,
        character: u32,
    ) -> Option<u32> {
        let mut current_line = 0u32;
        let mut current_char = 0u32;
        for token in &tokens.data {
            current_line += token.delta_line;
            current_char = if token.delta_line == 0 {
                current_char + token.delta_start
            } else {
                token.delta_start
            };
            if current_line == line
                && current_char <= character
                && character < current_char + token.length
            {
                return Some(token.token_type);
            }
        }
        None
    }

    fn semantic_token_positions(
        tokens: &lsp_types::SemanticTokens,
    ) -> Vec<(u32, u32, u32, u32, u32)> {
        let mut current_line = 0u32;
        let mut current_char = 0u32;
        let mut out = Vec::with_capacity(tokens.data.len());
        for token in &tokens.data {
            current_line += token.delta_line;
            current_char = if token.delta_line == 0 {
                current_char + token.delta_start
            } else {
                token.delta_start
            };
            out.push((
                current_line,
                current_char,
                token.length,
                token.token_type,
                token.token_modifiers_bitset,
            ));
        }
        out
    }

    fn unreachable_lint_source() -> &'static str {
        "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    RETURN.
    WRITE 'after'.
  ENDMETHOD.
ENDCLASS."
    }

    fn workspace_diagnostics_with_lints_config(
        test_name: &str,
        lints_config: &str,
        source: &str,
    ) -> Vec<lsp_types::Diagnostic> {
        let workspace_path = temp_workspace_path(test_name);
        fs::create_dir_all(workspace_path.join("src")).expect("src dir");
        let manifest = format!(
            r#"
version = 1

{lints_config}

[[unit]]
name = "ZMAIN"
kind = "report"
root_file = "src/ZMAIN.abap"
"#
        );
        fs::write(workspace_path.join("abapls.toml"), manifest).expect("manifest");
        let source_path = workspace_path.join("src/ZMAIN.abap");
        fs::write(&source_path, source).expect("source");

        let workspace_uri = normalize_lsp_uri(&path_to_file_uri(&workspace_path));
        let source_uri = normalize_lsp_uri(&path_to_file_uri(&source_path));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let workspace = state
            .workspaces
            .get(&normalize_lsp_uri(&workspace_uri))
            .expect("workspace");
        let snapshot = workspace.cache.get(&source_uri).expect("snapshot");
        build_lsp_diagnostics_for_workspace(Some(workspace), snapshot.as_ref())
    }

    fn build_sap_atc_test_workspace(
        test_name: &str,
        sap_atc_config: &str,
    ) -> (PathBuf, ServerState, String, String, i32) {
        let workspace_path = temp_workspace_path(test_name);
        fs::create_dir_all(workspace_path.join("src")).expect("src dir");
        let manifest = format!(
            r#"
version = 1

[lints.sap_atc]
{sap_atc_config}

[[unit]]
name = "ZMAIN"
kind = "report"
root_file = "src/ZMAIN.abap"
"#
        );
        fs::write(workspace_path.join("abapls.toml"), manifest).expect("manifest");
        let source_path = workspace_path.join("src/ZMAIN.abap");
        fs::write(&source_path, "REPORT zmain.\nWRITE 'x'.\n").expect("source");

        let workspace_uri = normalize_lsp_uri(&path_to_file_uri(&workspace_path));
        let source_uri = normalize_lsp_uri(&path_to_file_uri(&source_path));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        let snapshot_version = state
            .workspace_for_uri(&source_uri)
            .and_then(|workspace| workspace.cache.get(&source_uri))
            .expect("snapshot")
            .version;

        (
            workspace_path,
            state,
            normalize_lsp_uri(&workspace_uri),
            source_uri,
            snapshot_version,
        )
    }

    fn import_sap_atc_finding(
        state: &mut ServerState,
        workspace_uri: &str,
        source_uri: &str,
        document_version: i32,
    ) {
        handle_sap_atc_results_updated(
            state,
            &SapAtcResultsUpdatedParams {
                workspace_uri: workspace_uri.to_string(),
                source_uri: source_uri.to_string(),
                document_version,
                object_name: "ZMAIN".to_string(),
                check_variant: "DEFAULT".to_string(),
                configuration: None,
                fetched_at: "2026-04-27T00:00:00Z".to_string(),
                findings: vec![SapAtcFindingPayload {
                    sap_check_id: "CI_ALL_FIELDS_NEEDED".to_string(),
                    sap_message_id: "MSG001".to_string(),
                    message: "ATC asks for an explicit field list".to_string(),
                    severity: "warning".to_string(),
                    mapped_local_lint_id: None,
                    exemption_state: Some("none".to_string()),
                    suppression_state: None,
                    location: SapAtcSourceLocationPayload {
                        uri: source_uri.to_string(),
                        object_name: "ZMAIN".to_string(),
                        include_name: "ZMAIN".to_string(),
                        start_line: Some(2),
                        start_column: Some(1),
                        end_line: Some(2),
                        end_column: Some(6),
                    },
                }],
            },
        );
    }

    #[test]
    fn imported_sap_atc_findings_publish_as_lsp_diagnostics() {
        let (workspace_path, mut state, workspace_uri, source_uri, version) =
            build_sap_atc_test_workspace(
                "sap_atc_imported_diagnostic",
                r#"mode = "manual"
check_variant = "DEFAULT""#,
            );

        import_sap_atc_finding(&mut state, &workspace_uri, &source_uri, version);

        let workspace = state.workspace_for_uri(&source_uri).expect("workspace");
        let snapshot = workspace.cache.get(&source_uri).expect("snapshot");
        let diagnostics = build_lsp_diagnostics_for_workspace(Some(workspace), snapshot.as_ref());
        let diagnostic = diagnostics
            .iter()
            .find(|diagnostic| diagnostic.source.as_deref() == Some(SAP_ATC_DIAGNOSTIC_SOURCE))
            .expect("sap-atc diagnostic");

        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::WARNING));
        assert_eq!(
            diagnostic.code,
            Some(NumberOrString::String(ABAP_LSP_SELECT_STAR.to_string()))
        );
        assert_eq!(diagnostic.range.start.line, 1);
        assert_eq!(diagnostic.range.start.character, 0);
        assert_eq!(diagnostic.message, "ATC asks for an explicit field list");
        let data = diagnostic.data.as_ref().expect("diagnostic data");
        assert_eq!(
            data.get("kind").and_then(|value| value.as_str()),
            Some("sap_atc_lint")
        );
        assert_eq!(
            data.get("sapCheckId").and_then(|value| value.as_str()),
            Some("CI_ALL_FIELDS_NEEDED")
        );
        assert_eq!(
            data.get("mappedLocalLintId")
                .and_then(|value| value.as_str()),
            Some(ABAP_LSP_SELECT_STAR)
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn sap_atc_diagnostics_ignore_stale_versions_and_off_mode() {
        let (workspace_path, mut state, workspace_uri, source_uri, version) =
            build_sap_atc_test_workspace(
                "sap_atc_stale_version",
                r#"mode = "manual"
check_variant = "DEFAULT""#,
            );

        import_sap_atc_finding(&mut state, &workspace_uri, &source_uri, version + 1);

        let workspace = state.workspace_for_uri(&source_uri).expect("workspace");
        let snapshot = workspace.cache.get(&source_uri).expect("snapshot");
        let diagnostics = build_lsp_diagnostics_for_workspace(Some(workspace), snapshot.as_ref());
        assert!(
            diagnostics
                .iter()
                .all(|diagnostic| diagnostic.source.as_deref() != Some(SAP_ATC_DIAGNOSTIC_SOURCE)),
            "{diagnostics:#?}"
        );
        let _ = fs::remove_dir_all(&workspace_path);

        let (workspace_path, mut state, workspace_uri, source_uri, version) =
            build_sap_atc_test_workspace(
                "sap_atc_off_mode",
                r#"mode = "off"
check_variant = "DEFAULT""#,
            );

        import_sap_atc_finding(&mut state, &workspace_uri, &source_uri, version);

        let workspace = state.workspace_for_uri(&source_uri).expect("workspace");
        let snapshot = workspace.cache.get(&source_uri).expect("snapshot");
        let diagnostics = build_lsp_diagnostics_for_workspace(Some(workspace), snapshot.as_ref());
        assert!(
            diagnostics
                .iter()
                .all(|diagnostic| diagnostic.source.as_deref() != Some(SAP_ATC_DIAGNOSTIC_SOURCE)),
            "{diagnostics:#?}"
        );
        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn normalize_lsp_uri_lowercases_windows_file_drive_prefix() {
        assert_eq!(
            normalize_lsp_uri("file:///D:/project/foo.abap"),
            "file:///d:/project/foo.abap"
        );
        assert_eq!(
            normalize_lsp_uri("file:///d%3A/project/foo.abap"),
            "file:///d:/project/foo.abap"
        );
        assert_eq!(normalize_lsp_uri("untitled:1"), "untitled:1");
    }

    #[test]
    fn normalize_lsp_uri_canonicalizes_dependency_documents() {
        let canonical = "abapls-cache:///zattp_cl_ar_dm_object.abap?workspace=file%3A%2F%2F%2Fd%3A%2Fdev%2Fabap%2Farchiving&artifact=1&name=zattp_cl_ar_dm_object&kind=global-class";
        assert_eq!(
            normalize_lsp_uri(
                "abapls-cache:/zattp_cl_ar_dm_object.abap?workspace=file:///d:/dev/abap/archiving&artifact=1&name=zattp_cl_ar_dm_object&kind=global-class"
            ),
            canonical
        );
        assert_eq!(
            normalize_lsp_uri(
                "abapls-cache:/zattp_cl_ar_dm_object.abap?workspace%3Dfile%3A%2F%2F%2Fd%3A%2Fdev%2Fabap%2Farchiving%26artifact%3D1%26name%3Dzattp_cl_ar_dm_object%26kind%3Dglobal-class"
            ),
            canonical
        );
    }

    #[test]
    fn initialize_result_exposes_server_capabilities() {
        let result = initialize_result(&Default::default());

        assert!(result.capabilities.text_document_sync.is_some());
        assert!(result.capabilities.semantic_tokens_provider.is_some());
        let triggers = result
            .capabilities
            .completion_provider
            .as_ref()
            .and_then(|completion| completion.trigger_characters.as_ref())
            .expect("completion triggers");
        assert!(triggers.iter().any(|trigger| trigger == "("));
        assert!(triggers.iter().any(|trigger| trigger == "<"));
        assert!(matches!(
            result.capabilities.definition_provider,
            Some(lsp_types::OneOf::Left(true))
        ));
        assert!(matches!(
            result.capabilities.references_provider,
            Some(lsp_types::OneOf::Left(true))
        ));
        assert!(matches!(
            result.capabilities.rename_provider,
            Some(lsp_types::OneOf::Right(lsp_types::RenameOptions {
                prepare_provider: Some(true),
                ..
            }))
        ));
        assert!(matches!(
            result.capabilities.inlay_hint_provider,
            Some(lsp_types::OneOf::Right(
                lsp_types::InlayHintServerCapabilities::Options(_)
            ))
        ));
        assert!(matches!(
            result.capabilities.code_action_provider,
            Some(lsp_types::CodeActionProviderCapability::Simple(true))
        ));
        assert!(matches!(
            result.capabilities.folding_range_provider,
            Some(lsp_types::FoldingRangeProviderCapability::Simple(true))
        ));
        assert!(result.server_info.is_some());
    }

    #[test]
    fn folding_ranges_split_if_branches() {
        let text = [
            "IF foo = 1.",
            "  WRITE / 'one'.",
            "ELSEIF foo = 2.",
            "  WRITE / 'two'.",
            "ELSE.",
            "  WRITE / 'other'.",
            "ENDIF.",
        ]
        .join("\n");
        let ranges = folding_ranges_from_text(&text);

        assert_eq!(folding_line_ranges(&ranges), vec![(0, 1), (2, 3), (4, 5)]);
    }

    #[test]
    fn folding_ranges_keep_nested_case_branches_separate() {
        let text = [
            "CASE outer.",
            "  WHEN 1.",
            "    CASE inner.",
            "      WHEN 'A'.",
            "        WRITE / 'a'.",
            "      ELSE.",
            "        WRITE / 'b'.",
            "    ENDCASE.",
            "  ELSE.",
            "    WRITE / 'other'.",
            "ENDCASE.",
        ]
        .join("\n");
        let ranges = folding_ranges_from_text(&text);

        assert_eq!(
            folding_line_ranges(&ranges),
            vec![(3, 4), (5, 6), (2, 6), (1, 7), (8, 9), (0, 9)]
        );
    }

    #[test]
    fn workspace_include_fragment_diagnostics_suppress_block_boundaries_only() {
        let root_path = temp_workspace_path("include_fragment_diagnostics");
        let root_uri = path_to_file_uri(&root_path);
        let include_path =
            root_path.join("src/function-groups/MEACCTVI/includes/IFRE_END_OF_RE_EA_FIN.abap");
        let include_uri = path_to_file_uri(&include_path);
        let mut workspace = WorkspaceState::new(root_uri);
        workspace.manifest = Some(WorkspaceManifest {
            version: 1,
            connection: String::new(),
            dependency_store: None,
            resolution: ManifestResolution::default(),
            performance: ManifestPerformance::default(),
            lints: None,
            units: vec![ManifestUnit {
                name: "MEACCTVI".to_string(),
                kind: "function-group".to_string(),
                package_name: String::new(),
                root_file: "src/function-groups/MEACCTVI/MEACCTVI.abap".to_string(),
                dependency_of: Vec::new(),
                members: vec![ManifestUnitMember {
                    role: String::new(),
                    file: "src/function-groups/MEACCTVI/includes/IFRE_END_OF_RE_EA_FIN.abap"
                        .to_string(),
                    object_name: "IFRE_END_OF_RE_EA_FIN".to_string(),
                }],
            }],
        });

        let snapshot = DocumentStore::default().publish_input(DocumentInput {
            uri: Arc::from(include_uri),
            version: 1,
            text: Arc::from("ENDIF.\nIF ."),
            is_dependency: false,
            object_name: Some(Arc::from("ifre_end_of_re_ea_fin")),
        });
        let strict = build_lsp_diagnostics(snapshot.as_ref());
        assert!(
            strict.iter().any(|diag| diag
                .message
                .contains("unexpected ENDIF without matching IF")),
            "{strict:?}"
        );

        let workspace_diagnostics =
            build_lsp_diagnostics_for_workspace(Some(&workspace), snapshot.as_ref());
        assert!(
            workspace_diagnostics.iter().all(|diag| !diag
                .message
                .contains("unexpected ENDIF without matching IF")),
            "{workspace_diagnostics:?}"
        );
        assert!(
            workspace_diagnostics
                .iter()
                .any(|diag| diag.message.contains("expected condition after IF")),
            "{workspace_diagnostics:?}"
        );
    }

    #[test]
    fn workspace_lint_rule_allow_suppresses_lsp_lint_diagnostic() {
        let diagnostics = workspace_diagnostics_with_lints_config(
            "lint_allow_unreachable",
            r#"
[lints.rules]
"abap-lsp.unreachable-code" = "allow"
"#,
            unreachable_lint_source(),
        );

        assert!(
            diagnostics.iter().all(|diag| {
                diag.code
                    != Some(NumberOrString::String(
                        ABAP_LSP_UNREACHABLE_CODE.to_string(),
                    ))
            }),
            "{diagnostics:#?}"
        );
        assert!(
            diagnostics
                .iter()
                .all(|diag| diag.source.as_deref() != Some(LINT_DIAGNOSTIC_SOURCE)),
            "{diagnostics:#?}"
        );
        assert!(
            diagnostics
                .iter()
                .all(|diag| !diag.message.contains("unreachable code")),
            "{diagnostics:#?}"
        );
    }

    #[test]
    fn workspace_lint_rule_allow_suppresses_local_lint_diagnostic() {
        let diagnostics = workspace_diagnostics_with_lints_config(
            "lint_allow_select_star",
            r#"
[lints.rules]
"abap-lsp.select-star" = "allow"
"#,
            "SELECT * FROM scarr INTO TABLE @DATA(lt_scarr).",
        );

        assert!(
            diagnostics.iter().all(|diag| {
                diag.code != Some(NumberOrString::String(ABAP_LSP_SELECT_STAR.to_string()))
            }),
            "{diagnostics:#?}"
        );
    }

    #[test]
    fn workspace_publishes_select_single_without_full_key_lsp_diagnostic() {
        let workspace_path = temp_workspace_path("lint_select_single_without_full_key");
        fs::create_dir_all(workspace_path.join("src/dictionary/database-tables"))
            .expect("workspace dirs");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[[unit]]
name = "ZMAIN"
kind = "report"
root_file = "src/ZMAIN.abap"

[[unit]]
name = "ZFLIGHT"
kind = "ddic-table"
root_file = "src/dictionary/database-tables/ZFLIGHT.abap"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("src/dictionary/database-tables/ZFLIGHT.abap"),
            "\
TYPES: BEGIN OF zflight,
         mandt  TYPE c LENGTH 3, \" primary key; client
         carrid TYPE c LENGTH 3, \" primary key; carrier
         connid TYPE c LENGTH 4, \" primary key; connection
       END OF zflight.",
        )
        .expect("ddic");
        let source_path = workspace_path.join("src/ZMAIN.abap");
        fs::write(
            &source_path,
            "\
DATA lv_carrid TYPE c LENGTH 3.
SELECT SINGLE carrid
  FROM zflight
  INTO @DATA(lv_carrid_out)
  WHERE carrid = @lv_carrid.",
        )
        .expect("source");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = normalize_lsp_uri(&path_to_file_uri(&source_path));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let workspace = state
            .workspaces
            .get(&normalize_lsp_uri(&workspace_uri))
            .expect("workspace");
        let snapshot = workspace.cache.get(&source_uri).expect("snapshot");
        let diagnostics = build_lsp_diagnostics_for_workspace(Some(workspace), snapshot.as_ref());
        let diagnostic = diagnostics
            .iter()
            .find(|diag| {
                diag.code
                    == Some(NumberOrString::String(
                        ABAP_LSP_SELECT_SINGLE_WITHOUT_FULL_KEY.to_string(),
                    ))
            })
            .expect("select single full-key lint");

        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::INFORMATION));
        assert_eq!(diagnostic.source.as_deref(), Some(LINT_DIAGNOSTIC_SOURCE));
        assert!(
            diagnostic
                .code_description
                .as_ref()
                .is_some_and(|description| description
                    .href
                    .as_str()
                    .contains("abap-lspselect-single-without-full-key")),
            "{diagnostic:#?}"
        );
        assert!(
            diagnostic.message.contains("connid") && !diagnostic.message.contains("mandt"),
            "{diagnostic:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn workspace_lint_rule_deny_publishes_lsp_error_with_lint_metadata() {
        let diagnostics = workspace_diagnostics_with_lints_config(
            "lint_deny_unreachable",
            r#"
[lints.rules]
"abap-lsp.unreachable-code" = "deny"
"#,
            unreachable_lint_source(),
        );

        let diagnostic = diagnostics
            .iter()
            .find(|diag| {
                diag.code
                    == Some(NumberOrString::String(
                        ABAP_LSP_UNREACHABLE_CODE.to_string(),
                    ))
            })
            .expect("unreachable-code lint diagnostic");
        assert_eq!(diagnostic.source.as_deref(), Some(LINT_DIAGNOSTIC_SOURCE));
        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
        let docs_href = diagnostic
            .code_description
            .as_ref()
            .map(|description| description.href.as_str())
            .expect("lint diagnostic docs href");
        assert!(docs_href.contains("docs/reference/lints.md#abap-lspunreachable-code"));
        let data = diagnostic.data.as_ref().expect("lint diagnostic data");
        assert_eq!(
            data.get("lint_id").and_then(serde_json::Value::as_str),
            Some(ABAP_LSP_UNREACHABLE_CODE)
        );
        assert_eq!(
            data.get("group").and_then(serde_json::Value::as_str),
            Some("correctness")
        );
        assert_eq!(
            data.get("origin").and_then(serde_json::Value::as_str),
            Some("abap-lsp")
        );
    }

    #[test]
    fn lsp_diagnostics_publish_call_function_result_lint_metadata() {
        let store = DocumentStore::default();
        let src = "\
CALL FUNCTION 'Z_DEMO'
  EXCEPTIONS
    failed = 1.
SELECT SINGLE carrid FROM scarr INTO @DATA(lv_carrid).
IF sy-subrc <> 0.
  RETURN.
ENDIF.";
        let snapshot = store.publish("file:///lint_call_function_result_lsp.abap", 1, src);
        let diagnostics = build_lsp_diagnostics(snapshot.as_ref());

        let diagnostic = diagnostics
            .iter()
            .find(|diag| {
                diag.code
                    == Some(NumberOrString::String(
                        ABAP_LSP_IGNORED_CALL_FUNCTION_RESULT.to_string(),
                    ))
            })
            .expect("call function result lint diagnostic");
        assert_eq!(diagnostic.source.as_deref(), Some(LINT_DIAGNOSTIC_SOURCE));
        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::INFORMATION));
        let data = diagnostic.data.as_ref().expect("lint diagnostic data");
        assert_eq!(
            data.get("lint_id").and_then(serde_json::Value::as_str),
            Some(ABAP_LSP_IGNORED_CALL_FUNCTION_RESULT)
        );
        assert_eq!(
            data.get("group").and_then(serde_json::Value::as_str),
            Some("correctness")
        );
    }

    #[test]
    fn workspace_lint_source_suppression_hides_lsp_lint_diagnostic() {
        let source = unreachable_lint_source().replace(
            "    WRITE 'after'.",
            "    WRITE 'after'. \" abap-lsp:allow(abap-lsp.unreachable-code)",
        );
        let diagnostics = workspace_diagnostics_with_lints_config(
            "lint_source_suppressed_unreachable",
            "",
            &source,
        );

        assert!(
            diagnostics.iter().all(|diag| {
                diag.code
                    != Some(NumberOrString::String(
                        ABAP_LSP_UNREACHABLE_CODE.to_string(),
                    ))
            }),
            "{diagnostics:#?}"
        );
    }

    #[test]
    fn workspace_lint_report_suppressed_keeps_lsp_hint_with_marker() {
        let source = unreachable_lint_source().replace(
            "    WRITE 'after'.",
            "    WRITE 'after'. \" abap-lsp:allow(abap-lsp.unreachable-code)",
        );
        let diagnostics = workspace_diagnostics_with_lints_config(
            "lint_report_suppressed_unreachable",
            r#"
[lints]
report_suppressed = true
"#,
            &source,
        );

        let diagnostic = diagnostics
            .iter()
            .find(|diag| {
                diag.code
                    == Some(NumberOrString::String(
                        ABAP_LSP_UNREACHABLE_CODE.to_string(),
                    ))
            })
            .expect("suppressed unreachable-code lint diagnostic");
        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::INFORMATION));
        let data = diagnostic.data.as_ref().expect("lint diagnostic data");
        assert_eq!(
            data.get("suppressed").and_then(serde_json::Value::as_bool),
            Some(true)
        );
        assert_eq!(
            data.get("suppression")
                .and_then(|value| value.get("kind"))
                .and_then(serde_json::Value::as_str),
            Some("abap-lsp-allow")
        );
    }

    #[test]
    fn workspace_lint_policy_does_not_suppress_parse_errors() {
        let diagnostics = workspace_diagnostics_with_lints_config(
            "lint_policy_parse_error",
            r#"
[lints]
profile = "none"

[lints.groups]
suspicious = "allow"
"#,
            "IF .",
        );

        assert!(
            diagnostics.iter().any(|diag| {
                diag.source.as_deref() == Some("abap-parser")
                    && diag.severity == Some(DiagnosticSeverity::ERROR)
            }),
            "{diagnostics:#?}"
        );
    }

    #[test]
    fn workspace_lint_policy_does_not_suppress_semantic_hard_errors() {
        let diagnostics = workspace_diagnostics_with_lints_config(
            "lint_policy_semantic_error",
            r#"
[lints]
profile = "none"

[lints.rules]
"epc.invalid-open-sql-into-target" = "allow"
"epc.invalid-open-sql-target" = "allow"
"abap-lsp.missing-rule" = "allow"
"#,
            r#"
TYPES ty_row TYPE i.
DATA wa TYPE ty_row.
SELECT * FROM ty_row INTO TABLE wa.
"#,
        );

        let diagnostic = diagnostics
            .iter()
            .find(|diag| {
                diag.code
                    == Some(NumberOrString::String(
                        "epc.invalid-open-sql-into-target".to_string(),
                    ))
            })
            .expect("invalid Open SQL target diagnostic");
        assert_eq!(diagnostic.source.as_deref(), Some(LINT_DIAGNOSTIC_SOURCE));
        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
    }

    #[test]
    fn workspace_manifest_diagnostics_report_invalid_lint_config() {
        let workspace_path = temp_workspace_path("manifest_invalid_lint_config");
        fs::create_dir_all(workspace_path.join("src")).expect("src dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[lints.groups]
suspicious = "warn"

[lints.rules]
"abap-lsp.missing-rule" = "warn"
"sap-atc:zcheck/zmsg" = "allow"

[[unit]]
name = "ZMAIN"
kind = "report"
root_file = "src/ZMAIN.abap"
"#,
        )
        .expect("manifest");
        fs::write(workspace_path.join("src/ZMAIN.abap"), "REPORT zmain.").expect("source");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let manifest_diagnostics = workspace_manifest_diagnostics_params(&state, &workspace_uri)
            .expect("manifest diagnostics");
        assert!(manifest_diagnostics.uri.as_str().ends_with("/abapls.toml"));

        let messages: Vec<_> = manifest_diagnostics
            .diagnostics
            .iter()
            .map(|diagnostic| diagnostic.message.as_str())
            .collect();
        assert_eq!(messages.len(), 2, "{messages:?}");
        assert!(
            messages
                .iter()
                .any(|message| message.contains("unknown lint group 'suspicious'"))
        );
        assert!(messages.iter().any(|message| {
            message.contains("unknown native lint rule 'abap-lsp.missing-rule'")
        }));
        assert!(
            manifest_diagnostics.diagnostics.iter().all(|diagnostic| {
                diagnostic.severity == Some(DiagnosticSeverity::WARNING)
                    && diagnostic.code == Some(NumberOrString::String("lint-config".to_string()))
            }),
            "{:#?}",
            manifest_diagnostics.diagnostics
        );
        assert_eq!(manifest_diagnostics.diagnostics[0].range.start.line, 4);
        assert_eq!(manifest_diagnostics.diagnostics[1].range.start.line, 7);

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn code_action_creates_missing_method_implementation_at_end_of_class_impl() {
        let state = ServerState::default();
        let uri = Uri::from_str("file:///missing_method_code_action.abap").expect("uri");
        let text = "\
CLASS lcl_demo DEFINITION.\n\
  PUBLIC SECTION.\n\
    METHODS existing.\n\
    METHODS missing.\n\
ENDCLASS.\n\
\n\
CLASS lcl_demo IMPLEMENTATION.\n\
  METHOD existing.\n\
  ENDMETHOD.\n\
ENDCLASS.\n";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let snapshot = snapshot_for_uri(&state, uri.as_str()).expect("snapshot");
        let diagnostic = build_lsp_diagnostics(snapshot.as_ref())
            .into_iter()
            .find(|diag| {
                diag.code.as_ref().is_some_and(|code| {
                    matches!(
                        code,
                        NumberOrString::String(value)
                            if value == DIAGNOSTIC_CODE_MISSING_METHOD_IMPLEMENTATION
                    )
                }) && diag.message.contains("missing")
            })
            .expect("missing method diagnostic");

        let actions = code_actions(
            &state,
            &CodeActionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                range: diagnostic.range,
                context: CodeActionContext {
                    diagnostics: vec![diagnostic],
                    only: None,
                    trigger_kind: None,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("code actions");

        assert_eq!(actions.len(), 1);
        let CodeActionOrCommand::CodeAction(action) = &actions[0] else {
            panic!("expected code action");
        };
        let changes = action
            .edit
            .as_ref()
            .and_then(|edit| edit.changes.as_ref())
            .expect("workspace changes");
        let edits = changes.get(&uri).expect("uri changes");
        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, "\n  METHOD missing.\n  ENDMETHOD.\n");
        assert_eq!(
            edits[0].range.start,
            Position {
                line: 9,
                character: 0
            }
        );
        assert_eq!(
            edits[0].range.end,
            Position {
                line: 9,
                character: 0
            }
        );
    }

    #[test]
    fn code_action_creates_missing_method_implementation_in_empty_class_impl() {
        let state = ServerState::default();
        let uri = Uri::from_str("file:///missing_method_empty_impl.abap").expect("uri");
        let text = "\
CLASS lo_epcis_builder DEFINITION.\n\
  PUBLIC SECTION.\n\
    METHODS build.\n\
ENDCLASS.\n\
\n\
CLASS lo_epcis_builder IMPLEMENTATION.\n\
  METHOD build.\n\
  ENDMETHOD.\n\
ENDCLASS.\n\
\n\
CLASS lcl_object_event DEFINITION.\n\
  PUBLIC SECTION.\n\
    METHODS add_to_epcis\n\
      CHANGING\n\
        co_epcis_builder TYPE REF TO lo_epcis_builder.\n\
ENDCLASS.\n\
\n\
CLASS lcl_object_event IMPLEMENTATION.\n\
\n\
ENDCLASS.\n";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let snapshot = snapshot_for_uri(&state, uri.as_str()).expect("snapshot");
        let diagnostic = build_lsp_diagnostics(snapshot.as_ref())
            .into_iter()
            .find(|diag| {
                diag.code.as_ref().is_some_and(|code| {
                    matches!(
                        code,
                        NumberOrString::String(value)
                            if value == DIAGNOSTIC_CODE_MISSING_METHOD_IMPLEMENTATION
                    )
                }) && diag.message.contains("add_to_epcis")
            })
            .expect("missing method diagnostic");

        let actions = code_actions(
            &state,
            &CodeActionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                range: diagnostic.range,
                context: CodeActionContext {
                    diagnostics: vec![diagnostic],
                    only: None,
                    trigger_kind: None,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("code actions");

        assert_eq!(actions.len(), 1);
        let CodeActionOrCommand::CodeAction(action) = &actions[0] else {
            panic!("expected code action");
        };
        let changes = action
            .edit
            .as_ref()
            .and_then(|edit| edit.changes.as_ref())
            .expect("workspace changes");
        let edits = changes.get(&uri).expect("uri changes");
        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, "  METHOD add_to_epcis.\n  ENDMETHOD.\n");
        assert_eq!(
            edits[0].range.start,
            Position {
                line: 19,
                character: 0
            }
        );
        assert_eq!(
            edits[0].range.end,
            Position {
                line: 19,
                character: 0
            }
        );
    }

    #[test]
    fn code_action_creates_missing_class_implementation_when_absent() {
        let state = ServerState::default();
        let uri = Uri::from_str("file:///missing_class_impl.abap").expect("uri");
        let text = "\
CLASS zcl_ast_node DEFINITION.\n\
  PUBLIC SECTION.\n\
    METHODS to_string \n\
      RETURNING VALUE(rv_text) TYPE string.\n\
ENDCLASS.";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let snapshot = snapshot_for_uri(&state, uri.as_str()).expect("snapshot");
        let diagnostic = build_lsp_diagnostics(snapshot.as_ref())
            .into_iter()
            .find(|diag| {
                diag.code.as_ref().is_some_and(|code| {
                    matches!(
                        code,
                        NumberOrString::String(value)
                            if value == DIAGNOSTIC_CODE_MISSING_METHOD_IMPLEMENTATION
                    )
                }) && diag.message.contains("to_string")
            })
            .expect("missing method diagnostic");

        let actions = code_actions(
            &state,
            &CodeActionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                range: diagnostic.range,
                context: CodeActionContext {
                    diagnostics: vec![diagnostic],
                    only: None,
                    trigger_kind: None,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("code actions");

        assert_eq!(actions.len(), 1);
        let CodeActionOrCommand::CodeAction(action) = &actions[0] else {
            panic!("expected code action");
        };
        let changes = action
            .edit
            .as_ref()
            .and_then(|edit| edit.changes.as_ref())
            .expect("workspace changes");
        let edits = changes.get(&uri).expect("uri changes");
        assert_eq!(edits.len(), 1);
        assert_eq!(
            edits[0].new_text,
            "\n\nCLASS zcl_ast_node IMPLEMENTATION.\n  METHOD to_string.\n  ENDMETHOD.\nENDCLASS.\n"
        );
        assert_eq!(
            edits[0].range.start,
            Position {
                line: 4,
                character: 9
            }
        );
        assert_eq!(
            edits[0].range.end,
            Position {
                line: 4,
                character: 9
            }
        );
    }

    #[test]
    fn references_return_locations_for_declaration_and_use() {
        let state = ServerState::default();
        let text = "DATA lv TYPE i.\nlv = 1.";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///refs.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let locations = references(
            &state,
            &ReferenceParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///refs.abap").expect("uri"),
                    },
                    position: Position {
                        line: 1,
                        character: 1,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: lsp_types::ReferenceContext {
                    include_declaration: true,
                },
            },
        )
        .expect("references");

        assert_eq!(locations.len(), 2);
        assert_eq!(locations[0].range.start.line, 0);
        assert_eq!(locations[1].range.start.line, 1);
    }

    #[test]
    fn prepare_rename_returns_placeholder_and_range_for_variable() {
        let state = ServerState::default();
        let text = "DATA lv TYPE i.\nlv = 1.";
        let uri = Uri::from_str("file:///prepare_rename.abap").expect("uri");
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let offset = text.rfind("lv").expect("variable use") + 1;
        let position = offset_to_position(text, offset).expect("position");
        let response = prepare_rename(
            &state,
            &TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position,
            },
        )
        .expect("prepare rename");

        let PrepareRenameResponse::RangeWithPlaceholder { range, placeholder } = response else {
            panic!("expected placeholder response");
        };
        assert_eq!(placeholder, "lv");
        assert_eq!(range.start.line, 1);
        assert_eq!(range.start.character, 0);
        assert_eq!(range.end.character, 2);
    }

    #[test]
    fn rename_returns_workspace_edit_for_method_declaration_implementation_and_call() {
        let state = ServerState::default();
        let text = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
    METHODS caller.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
  METHOD caller.
    run( ).
  ENDMETHOD.
ENDCLASS.";
        let uri = Uri::from_str("file:///rename_method.abap").expect("uri");
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let offset = text.rfind("run(").expect("method call") + 1;
        let position = offset_to_position(text, offset).expect("position");
        let edit = rename(
            &state,
            &RenameParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position,
                },
                new_name: "execute".to_string(),
                work_done_progress_params: Default::default(),
            },
        )
        .expect("rename request")
        .expect("workspace edit");

        let changes = edit.changes.expect("changes");
        let edits = changes.get(&uri).expect("uri edits");
        assert_eq!(edits.len(), 3, "{edits:?}");
        assert!(edits.iter().all(|edit| edit.new_text == "execute"));
        assert_eq!(
            edits
                .iter()
                .map(|edit| edit.range.start.line)
                .collect::<Vec<_>>(),
            vec![2, 7, 10]
        );
    }

    #[test]
    fn rename_rejects_field_symbol_name_without_angle_brackets() {
        let state = ServerState::default();
        let text = "FIELD-SYMBOLS <fs> TYPE any.\nASSIGN 1 TO <fs>.";
        let uri = Uri::from_str("file:///rename_field_symbol.abap").expect("uri");
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let offset = text.rfind("<fs>").expect("field symbol use") + 1;
        let position = offset_to_position(text, offset).expect("position");
        let error = rename(
            &state,
            &RenameParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position,
                },
                new_name: "fs2".to_string(),
                work_done_progress_params: Default::default(),
            },
        )
        .expect_err("rename should reject invalid field-symbol name");

        assert!(error.contains("angle brackets"), "{error}");
    }

    #[test]
    fn references_use_preview_snapshot_before_workspace_commit() {
        let workspace_path = temp_workspace_path("preview_references");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("main.abap"),
            "DATA lv TYPE i.\nlv = 1.\n",
        )
        .expect("main");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/main.abap");
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        let _ = handle_remote_dependencies_updated(
            &mut state,
            &super::RemoteDependenciesUpdatedParams {
                workspace_uri,
                source_uri: source_uri.clone(),
                source_uris: vec![source_uri.clone()],
                fetched: vec!["ZCL_HELPER".to_string()],
                failed: Vec::new(),
            },
        );
        let _ = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&source_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "\
REPORT zreport_main.
DATA lo_helper TYPE REF TO zcl_helper.
lo_helper->
"
                    .to_string(),
                },
            },
        );

        assert!(stage_workspace_preview_snapshot(
            &mut state,
            &source_uri,
            2,
            "DATA lv TYPE i.\nlv = 1.\nlv = lv + 1.\n"
        ));

        let locations = references(
            &state,
            &ReferenceParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&source_uri).expect("uri"),
                    },
                    position: Position {
                        line: 2,
                        character: 6,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: lsp_types::ReferenceContext {
                    include_declaration: true,
                },
            },
        )
        .expect("references");

        assert_eq!(locations.len(), 4, "{locations:?}");
        assert_eq!(locations[0].range.start.line, 0);
        assert_eq!(locations[1].range.start.line, 1);
        assert_eq!(locations[2].range.start.line, 2);
        assert_eq!(locations[3].range.start.line, 2);

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn hover_definition_and_references_work_for_for_all_entries_host_expr() {
        let state = ServerState::default();
        let text = "\
DATA lt_rep_evt TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA lt_obj_rel TYPE STANDARD TABLE OF string WITH EMPTY KEY.

SELECT rep_evtid,
       objid
  FROM /sttp/rep_obj_rl
  INTO TABLE @lt_obj_rel
  FOR ALL ENTRIES IN @lt_rep_evt
  WHERE rep_evtid = @lt_rep_evt.
";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///fae_hover.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let hover_result = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///fae_hover.abap").expect("uri"),
                    },
                    position: Position {
                        line: 7,
                        character: 24,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");
        let HoverContents::Markup(markup) = hover_result.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            markup.value.contains("lt_rep_evt"),
            "unexpected hover: {}",
            markup.value
        );

        let definition_result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///fae_hover.abap").expect("uri"),
                    },
                    position: Position {
                        line: 7,
                        character: 24,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("definition");
        let GotoDefinitionResponse::Scalar(location) = definition_result else {
            panic!("expected scalar location");
        };
        assert_eq!(location.range.start.line, 0);

        let locations = references(
            &state,
            &ReferenceParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///fae_hover.abap").expect("uri"),
                    },
                    position: Position {
                        line: 7,
                        character: 24,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: lsp_types::ReferenceContext {
                    include_declaration: true,
                },
            },
        )
        .expect("references");
        assert_eq!(locations.len(), 3, "{locations:?}");
        assert_eq!(locations[0].range.start.line, 0);
    }

    #[test]
    fn hover_and_definition_work_for_bare_delete_where_field_in_workspace_cached_ddic_proxy_include()
     {
        let workspace_path = temp_workspace_path("workspace_bare_delete_where_ddic");
        fs::create_dir_all(workspace_path.join("src")).expect("src dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1
connection = "default"

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "ZCL_MAIN"
kind = "global-class"
root_file = "src/ZCL_MAIN.abap"
"#,
        )
        .expect("manifest");

        let main_src = "\
CLASS zcl_main DEFINITION.\n\
  PRIVATE SECTION.\n\
    DATA mt_obj_itm TYPE /sttp/t_dm_obj_itm.\n\
    METHODS run.\n\
ENDCLASS.\n\
\n\
CLASS zcl_main IMPLEMENTATION.\n\
  METHOD run.\n\
    DATA(lt_obj_itm) = mt_obj_itm.\n\
    DELETE lt_obj_itm WHERE uom NE 'PK'.\n\
  ENDMETHOD.\n\
ENDCLASS.\n";
        fs::write(workspace_path.join("src/ZCL_MAIN.abap"), main_src).expect("main source");

        let include_xml = r#"
<abapsource:elementInfo adtcore:name="/sttp/s_dm_obj_itm"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="uom">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">/sttp/e_uom</abapsource:entry>
      <abapsource:entry abapsource:key="ddicDataType">char</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#;
        let row_xml = r#"
<abapsource:elementInfo adtcore:name="/sttp/dm_obj_itm"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DS" adtcore:name=".include">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicIncludeName">/sttp/s_dm_obj_itm</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#;
        let table_type_xml = r#"
<abapsource:elementInfo adtcore:name="/sttp/t_dm_obj_itm"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DT" adtcore:name="/sttp/dm_obj_itm">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicRowType">X</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#;

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/src/ZCL_MAIN.abap");
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("default".to_string()),
                artifacts: vec![
                    DependencyArtifactPayload {
                        package_name: "STTP".to_string(),
                        object_kind: "ddic-structure".to_string(),
                        object_name: "/STTP/S_DM_OBJ_ITM".to_string(),
                        object_uri: "/sap/bc/adt/ddic/structures/%2FSTTP%2FS_DM_OBJ_ITM"
                            .to_string(),
                        object_type: "TABL/DS".to_string(),
                        description: "Structure".to_string(),
                        file_extension: "xml".to_string(),
                        source_text: include_xml.to_string(),
                        fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    },
                    DependencyArtifactPayload {
                        package_name: "STTP".to_string(),
                        object_kind: "ddic-table".to_string(),
                        object_name: "/STTP/DM_OBJ_ITM".to_string(),
                        object_uri: "/sap/bc/adt/ddic/tables/%2FSTTP%2FDM_OBJ_ITM".to_string(),
                        object_type: "TABL/DT".to_string(),
                        description: "Table".to_string(),
                        file_extension: "xml".to_string(),
                        source_text: row_xml.to_string(),
                        fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    },
                    DependencyArtifactPayload {
                        package_name: "STTP".to_string(),
                        object_kind: "ddic-table-type".to_string(),
                        object_name: "/STTP/T_DM_OBJ_ITM".to_string(),
                        object_uri: "/sap/bc/adt/ddic/tabletypes/%2FSTTP%2FT_DM_OBJ_ITM"
                            .to_string(),
                        object_type: "TABL/TT".to_string(),
                        description: "Table type".to_string(),
                        file_extension: "xml".to_string(),
                        source_text: table_type_xml.to_string(),
                        fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    },
                ],
                negative: Vec::new(),
            },
        )
        .expect("store ddic artifacts");
        let _ = handle_remote_dependencies_updated(
            &mut state,
            &super::RemoteDependenciesUpdatedParams {
                workspace_uri: workspace_uri.clone(),
                source_uri: source_uri.clone(),
                source_uris: vec![source_uri.clone()],
                fetched: vec![
                    "/STTP/S_DM_OBJ_ITM".to_string(),
                    "/STTP/DM_OBJ_ITM".to_string(),
                    "/STTP/T_DM_OBJ_ITM".to_string(),
                ],
                failed: Vec::new(),
            },
        );

        let snapshot =
            snapshot_for_uri(&state, &normalize_lsp_uri(&source_uri)).expect("workspace snapshot");
        let hover_offset = main_src.rfind("uom").expect("uom use") + 1;
        let hover_position =
            offset_to_position(snapshot.text.as_ref(), hover_offset).expect("hover position");
        let direct_hover = snapshot.hovered_component_at(hover_offset);
        assert!(
            direct_hover.is_some(),
            "snapshot diagnostics={:?} refs={:?}",
            snapshot.symbols.diagnostics,
            snapshot.symbols.references
        );

        let hover_result = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&source_uri).expect("uri"),
                    },
                    position: hover_position,
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");
        let HoverContents::Markup(markup) = hover_result.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            markup.value.contains("`uom`"),
            "unexpected hover: {}",
            markup.value
        );

        let definition_result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&source_uri).expect("uri"),
                    },
                    position: hover_position,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("definition");
        let GotoDefinitionResponse::Scalar(location) = definition_result else {
            panic!("expected scalar location");
        };
        assert!(
            location.uri.as_str().starts_with("abapls-cache:"),
            "unexpected definition uri: {:?}",
            location.uri
        );
    }

    #[test]
    fn hover_and_definition_fall_back_to_ddic_data_element_for_bare_where_field_when_proxy_cache_is_incomplete()
     {
        let workspace_path = temp_workspace_path("workspace_bare_where_inferred_ddic_field");
        fs::create_dir_all(workspace_path.join("src")).expect("src dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1
connection = "default"

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "ZCL_MAIN"
kind = "global-class"
root_file = "src/ZCL_MAIN.abap"
"#,
        )
        .expect("manifest");

        let main_src = "\
CLASS zcl_main DEFINITION.\n\
  PRIVATE SECTION.\n\
    DATA mt_obj_itm TYPE /sttp/t_dm_obj_itm.\n\
    METHODS run.\n\
ENDCLASS.\n\
\n\
CLASS zcl_main IMPLEMENTATION.\n\
  METHOD run.\n\
    DATA(lt_obj_itm) = mt_obj_itm.\n\
    DELETE lt_obj_itm WHERE uom NE 'PK'.\n\
  ENDMETHOD.\n\
ENDCLASS.\n";
        fs::write(workspace_path.join("src/ZCL_MAIN.abap"), main_src).expect("main source");

        let data_element_xml = r#"
<abapsource:elementInfo adtcore:name="/sttp/e_uom"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
</abapsource:elementInfo>
"#;
        let include_xml = r#"
<abapsource:elementInfo adtcore:name="/sttp/s_dm_obj_itm"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="serno">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">/sttp/e_serno</abapsource:entry>
      <abapsource:entry abapsource:key="ddicDataType">char</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#;
        let row_xml = r#"
<abapsource:elementInfo adtcore:name="/sttp/dm_obj_itm"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DS" adtcore:name=".include">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicIncludeName">/sttp/s_dm_obj_itm</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#;
        let table_type_xml = r#"
<abapsource:elementInfo adtcore:name="/sttp/t_dm_obj_itm"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DT" adtcore:name="/sttp/dm_obj_itm">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicRowType">X</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#;

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/src/ZCL_MAIN.abap");
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("default".to_string()),
                artifacts: vec![
                    DependencyArtifactPayload {
                        package_name: "STTP".to_string(),
                        object_kind: "ddic-data-element".to_string(),
                        object_name: "/STTP/E_UOM".to_string(),
                        object_uri: "/sap/bc/adt/ddic/dataelements/%2FSTTP%2FE_UOM".to_string(),
                        object_type: "DTEL".to_string(),
                        description: "Data element".to_string(),
                        file_extension: "xml".to_string(),
                        source_text: data_element_xml.to_string(),
                        fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    },
                    DependencyArtifactPayload {
                        package_name: "STTP".to_string(),
                        object_kind: "ddic-structure".to_string(),
                        object_name: "/STTP/S_DM_OBJ_ITM".to_string(),
                        object_uri: "/sap/bc/adt/ddic/structures/%2FSTTP%2FS_DM_OBJ_ITM"
                            .to_string(),
                        object_type: "TABL/DS".to_string(),
                        description: "Structure".to_string(),
                        file_extension: "xml".to_string(),
                        source_text: include_xml.to_string(),
                        fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    },
                    DependencyArtifactPayload {
                        package_name: "STTP".to_string(),
                        object_kind: "ddic-table".to_string(),
                        object_name: "/STTP/DM_OBJ_ITM".to_string(),
                        object_uri: "/sap/bc/adt/ddic/tables/%2FSTTP%2FDM_OBJ_ITM".to_string(),
                        object_type: "TABL/DT".to_string(),
                        description: "Table".to_string(),
                        file_extension: "xml".to_string(),
                        source_text: row_xml.to_string(),
                        fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    },
                    DependencyArtifactPayload {
                        package_name: "STTP".to_string(),
                        object_kind: "ddic-table-type".to_string(),
                        object_name: "/STTP/T_DM_OBJ_ITM".to_string(),
                        object_uri: "/sap/bc/adt/ddic/tabletypes/%2FSTTP%2FT_DM_OBJ_ITM"
                            .to_string(),
                        object_type: "TABL/TT".to_string(),
                        description: "Table type".to_string(),
                        file_extension: "xml".to_string(),
                        source_text: table_type_xml.to_string(),
                        fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    },
                ],
                negative: Vec::new(),
            },
        )
        .expect("store ddic artifacts");
        let _ = handle_remote_dependencies_updated(
            &mut state,
            &super::RemoteDependenciesUpdatedParams {
                workspace_uri: workspace_uri.clone(),
                source_uri: source_uri.clone(),
                source_uris: vec![source_uri.clone()],
                fetched: vec![
                    "/STTP/E_UOM".to_string(),
                    "/STTP/S_DM_OBJ_ITM".to_string(),
                    "/STTP/DM_OBJ_ITM".to_string(),
                    "/STTP/T_DM_OBJ_ITM".to_string(),
                ],
                failed: Vec::new(),
            },
        );

        let snapshot =
            snapshot_for_uri(&state, &normalize_lsp_uri(&source_uri)).expect("workspace snapshot");
        let hover_offset = main_src.rfind("uom").expect("uom use") + 1;
        let hover_position =
            offset_to_position(snapshot.text.as_ref(), hover_offset).expect("hover position");

        let hover_result = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&source_uri).expect("uri"),
                    },
                    position: hover_position,
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");
        let HoverContents::Markup(markup) = hover_result.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            markup.value.contains("TYPE /sttp/e_uom"),
            "unexpected hover: {}",
            markup.value
        );

        let definition_result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&source_uri).expect("uri"),
                    },
                    position: hover_position,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("definition");
        let GotoDefinitionResponse::Scalar(location) = definition_result else {
            panic!("expected scalar location");
        };
        assert!(
            location.uri.as_str().starts_with("abapls-cache:"),
            "unexpected definition uri: {:?}",
            location.uri
        );
    }

    #[test]
    fn definition_returns_location_for_named_argument_parameter() {
        let state = ServerState::default();
        let text = "\
CLASS zcl_program DEFINITION.
  PUBLIC SECTION.
    METHODS add_statement
      IMPORTING io_stmt TYPE string.
ENDCLASS.

CLASS zcl_program IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA(lo_prog) = NEW zcl_program( ).
  lo_prog->add_statement( io_stmt = 'x' ).";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///definition.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let use_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("io_stmt = 'x'"))
            .expect("named argument line");
        let use_col = use_line.1.find("io_stmt").expect("named argument column") as u32 + 1;

        let result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///definition.abap").expect("uri"),
                    },
                    position: Position {
                        line: use_line.0 as u32,
                        character: use_col,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("definition");

        let GotoDefinitionResponse::Scalar(location) = result else {
            panic!("expected scalar location");
        };
        assert_eq!(
            location.uri,
            Uri::from_str("file:///definition.abap").expect("uri")
        );
        assert_eq!(location.range.start.line, 3);
        assert_eq!(location.range.start.character, 16);
        assert_eq!(location.range.end.line, 3);
        assert_eq!(location.range.end.character, 23);
    }

    #[test]
    fn definition_switches_between_class_method_declaration_and_implementation() {
        let state = ServerState::default();
        let text = "\
CLASS zcl_program DEFINITION.
  PUBLIC SECTION.
    METHODS add_statement.
ENDCLASS.

CLASS zcl_program IMPLEMENTATION.
  METHOD add_statement.
  ENDMETHOD.
ENDCLASS.";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///method_definition.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let declaration_result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///method_definition.abap").expect("uri"),
                    },
                    position: Position {
                        line: 2,
                        character: 12,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("declaration definition");
        let GotoDefinitionResponse::Scalar(declaration_location) = declaration_result else {
            panic!("expected scalar location");
        };
        assert_eq!(declaration_location.range.start.line, 6);
        assert_eq!(declaration_location.range.start.character, 9);

        let implementation_result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///method_definition.abap").expect("uri"),
                    },
                    position: Position {
                        line: 6,
                        character: 9,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("implementation definition");
        let GotoDefinitionResponse::Scalar(implementation_location) = implementation_result else {
            panic!("expected scalar location");
        };
        assert_eq!(implementation_location.range.start.line, 2);
        assert_eq!(implementation_location.range.start.character, 12);
    }

    #[test]
    fn definition_on_method_call_returns_method_implementation() {
        let state = ServerState::default();
        let text = "\
CLASS zcl_program DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS add_statement.
ENDCLASS.

CLASS zcl_program IMPLEMENTATION.
  METHOD add_statement.
  ENDMETHOD.
ENDCLASS.

zcl_program=>add_statement( ).";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///method_call_definition.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let call_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("=>add_statement"))
            .expect("method call line");
        let call_col = call_line.1.find("add_statement").expect("method call") as u32;

        let result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///method_call_definition.abap").expect("uri"),
                    },
                    position: Position {
                        line: call_line.0 as u32,
                        character: call_col,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("definition");
        let GotoDefinitionResponse::Scalar(location) = result else {
            panic!("expected scalar location");
        };
        assert_eq!(location.range.start.line, 6);
        assert_eq!(location.range.start.character, 9);
    }

    #[test]
    fn hover_and_definition_resolve_wrapped_table_element_type() {
        let state = ServerState::default();
        let text = "\
CLASS zcl_stmt DEFINITION.
ENDCLASS.

CLASS zcl_stmt IMPLEMENTATION.
ENDCLASS.

CLASS zcl_program DEFINITION.
  PUBLIC SECTION.
    TYPES ty_stmt_tab TYPE STANDARD TABLE OF REF TO zcl_stmt WITH DEFAULT KEY.
ENDCLASS.";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///wrapped_type.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let type_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("STANDARD TABLE OF REF TO zcl_stmt"))
            .expect("table type line");
        let type_col = type_line.1.rfind("zcl_stmt").expect("wrapped type column") as u32 + 1;

        let hover_result = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///wrapped_type.abap").expect("uri"),
                    },
                    position: Position {
                        line: type_line.0 as u32,
                        character: type_col,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");
        let HoverContents::Markup(markup) = hover_result.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("`zcl_stmt`"));
        assert!(markup.value.contains("Class"));

        let definition_result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///wrapped_type.abap").expect("uri"),
                    },
                    position: Position {
                        line: type_line.0 as u32,
                        character: type_col,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("definition");
        let GotoDefinitionResponse::Scalar(location) = definition_result else {
            panic!("expected scalar location");
        };
        assert_eq!(location.range.start.line, 0);
        assert_eq!(location.range.start.character, 6);
        assert_eq!(location.range.end.character, 14);
    }

    #[test]
    fn semantic_tokens_marks_declarations_and_references() {
        use lsp_types::SemanticTokenType;

        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///sem.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "DATA lv TYPE i.\nlv = 1.".to_string(),
                },
            },
        );

        let snapshot = state.cache.get("file:///sem.abap").expect("snapshot");
        let tokens = sem_tokens::build_semantic_tokens(snapshot.as_ref());
        assert!(
            !tokens.data.is_empty(),
            "expected semantic tokens from symbol table"
        );

        let legend = sem_tokens::semantic_tokens_legend();
        let var_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::VARIABLE)
            .expect("legend has variable") as u32;
        let decl_mod = 1u32
            << legend
                .token_modifiers
                .iter()
                .position(|m| m.as_str() == "declaration")
                .expect("declaration modifier");
        assert!(
            tokens
                .data
                .iter()
                .any(|t| t.token_type == var_idx && (t.token_modifiers_bitset & decl_mod) != 0),
            "expected a declared variable token"
        );
    }

    #[test]
    fn semantic_tokens_include_open_sql_namespace_sources() {
        use lsp_types::SemanticTokenType;

        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///sem_sql.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "SELECT * FROM /sttp/demo INTO TABLE DATA(lt).\n".to_string(),
                },
            },
        );

        let snapshot = state.cache.get("file:///sem_sql.abap").expect("snapshot");
        let tokens = sem_tokens::build_semantic_tokens(snapshot.as_ref());
        let legend = sem_tokens::semantic_tokens_legend();
        let namespace_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::NAMESPACE)
            .expect("legend has namespace") as u32;
        assert!(
            tokens.data.iter().any(|t| t.token_type == namespace_idx),
            "expected Open SQL source token"
        );
    }

    #[test]
    fn semantic_tokens_mark_structure_field_declarations_and_accesses() {
        use lsp_types::SemanticTokenType;

        let state = ServerState::default();
        let text = "\
TYPES: BEGIN OF ty_row,
         field_a TYPE i,
         field_b TYPE string,
       END OF ty_row.
DATA ls_row TYPE ty_row.
ls_row-field_a = 1.";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///sem_structure_fields.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let snapshot = state
            .cache
            .get("file:///sem_structure_fields.abap")
            .expect("snapshot");
        let tokens = sem_tokens::build_semantic_tokens(snapshot.as_ref());
        let legend = sem_tokens::semantic_tokens_legend();
        let property_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::PROPERTY)
            .expect("legend has property") as u32;
        let decl_mod = 1u32
            << legend
                .token_modifiers
                .iter()
                .position(|m| m.as_str() == "declaration")
                .expect("declaration modifier");

        let positions = semantic_token_positions(&tokens);
        assert!(
            positions
                .iter()
                .any(|&(line, character, _, token_type, modifiers)| {
                    line == 1
                        && character == 9
                        && token_type == property_idx
                        && (modifiers & decl_mod) != 0
                }),
            "expected field declaration token, tokens={positions:?}"
        );
        assert!(
            positions
                .iter()
                .any(|&(line, character, _, token_type, modifiers)| {
                    line == 5
                        && character == 7
                        && token_type == property_idx
                        && (modifiers & decl_mod) == 0
                }),
            "expected field access token, tokens={positions:?}"
        );
    }

    #[test]
    fn semantic_tokens_mark_structured_decl_end_names() {
        use lsp_types::SemanticTokenType;

        let state = ServerState::default();
        let text = "\
DATA BEGIN OF wa_row.
DATA: value TYPE i,
      END OF wa_row.
TYPES: BEGIN OF ty_row,
         field TYPE i,
       END OF ty_row.";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///sem_structured_end_names.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let snapshot = state
            .cache
            .get("file:///sem_structured_end_names.abap")
            .expect("snapshot");
        let tokens = sem_tokens::build_semantic_tokens(snapshot.as_ref());
        let legend = sem_tokens::semantic_tokens_legend();
        let variable_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::VARIABLE)
            .expect("legend has variable") as u32;
        let type_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::TYPE)
            .expect("legend has type") as u32;
        let decl_mod = 1u32
            << legend
                .token_modifiers
                .iter()
                .position(|m| m.as_str() == "declaration")
                .expect("declaration modifier");

        let positions = semantic_token_positions(&tokens);
        assert!(
            positions
                .iter()
                .any(|&(line, character, _, token_type, modifiers)| {
                    line == 2
                        && character == 13
                        && token_type == variable_idx
                        && (modifiers & decl_mod) == 0
                }),
            "expected END OF DATA name token, tokens={positions:?}"
        );
        assert!(
            positions
                .iter()
                .any(|&(line, character, _, token_type, modifiers)| {
                    line == 5
                        && character == 14
                        && token_type == type_idx
                        && (modifiers & decl_mod) == 0
                }),
            "expected END OF TYPES name token, tokens={positions:?}"
        );
    }

    #[test]
    fn semantic_tokens_mark_corresponding_mapping_fields() {
        use lsp_types::SemanticTokenType;

        let state = ServerState::default();
        let text = "\
TYPES ty_objid_rng TYPE RANGE OF i.
TYPES: BEGIN OF ty_evt,
         objid TYPE i,
       END OF ty_evt.
DATA ct_amdp_rec_evt_objid TYPE STANDARD TABLE OF ty_evt WITH EMPTY KEY.
DATA(lr_objid) = CORRESPONDING ty_objid_rng(
                   ct_amdp_rec_evt_objid
                 MAPPING low = objid ).";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///sem_corresponding_mapping.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let snapshot = state
            .cache
            .get("file:///sem_corresponding_mapping.abap")
            .expect("snapshot");
        let tokens = sem_tokens::build_semantic_tokens(snapshot.as_ref());
        let legend = sem_tokens::semantic_tokens_legend();
        let property_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::PROPERTY)
            .expect("legend has property") as u32;

        let lines: Vec<_> = text.lines().collect();
        let target_char = lines[7].find("low").expect("target field") as u32;
        let source_char = lines[7].find("objid").expect("source field") as u32;
        let positions = semantic_token_positions(&tokens);

        assert!(
            positions
                .iter()
                .any(|&(line, character, _, token_type, _)| {
                    line == 7 && character == target_char && token_type == property_idx
                }),
            "expected CORRESPONDING target field token, tokens={positions:?}"
        );
        assert!(
            positions
                .iter()
                .any(|&(line, character, _, token_type, _)| {
                    line == 7 && character == source_char && token_type == property_idx
                }),
            "expected CORRESPONDING source field token, tokens={positions:?}"
        );
    }

    #[test]
    fn semantic_tokens_mark_value_constructor_named_fields() {
        use lsp_types::SemanticTokenType;

        let state = ServerState::default();
        let text = "\
TYPES: BEGIN OF ty_selopt,
         sign TYPE c LENGTH 1,
         option TYPE c LENGTH 2,
       END OF ty_selopt.
DATA(ls_selopt) = VALUE ty_selopt(
  sign = 'I'
  option = 'EQ' ).";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///sem_value_fields.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let snapshot = state
            .cache
            .get("file:///sem_value_fields.abap")
            .expect("snapshot");
        let tokens = sem_tokens::build_semantic_tokens(snapshot.as_ref());
        let legend = sem_tokens::semantic_tokens_legend();
        let property_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::PROPERTY)
            .expect("legend has property") as u32;

        let lines: Vec<_> = text.lines().collect();
        let sign_char = lines[5].find("sign").expect("sign field") as u32;
        let option_char = lines[6].find("option").expect("option field") as u32;
        let positions = semantic_token_positions(&tokens);

        assert!(
            positions
                .iter()
                .any(|&(line, character, _, token_type, _)| {
                    line == 5 && character == sign_char && token_type == property_idx
                }),
            "expected VALUE field token for `sign`, tokens={positions:?}"
        );
        assert!(
            positions
                .iter()
                .any(|&(line, character, _, token_type, _)| {
                    line == 6 && character == option_char && token_type == property_idx
                }),
            "expected VALUE field token for `option`, tokens={positions:?}"
        );
    }

    #[test]
    fn semantic_tokens_mark_static_method_declaration_and_use() {
        use lsp_types::SemanticTokenType;

        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///sem_method.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "\
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS exec.
ENDCLASS.

some_class=>exec( )."
                        .to_string(),
                },
            },
        );

        let snapshot = state
            .cache
            .get("file:///sem_method.abap")
            .expect("snapshot");
        let tokens = sem_tokens::build_semantic_tokens(snapshot.as_ref());
        let legend = sem_tokens::semantic_tokens_legend();
        let method_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::METHOD)
            .expect("legend has method") as u32;
        let method_tokens = tokens
            .data
            .iter()
            .filter(|t| t.token_type == method_idx)
            .count();
        assert!(
            method_tokens >= 2,
            "expected declaration and call to be marked as methods"
        );
    }

    #[test]
    fn semantic_tokens_mark_full_event_block_header() {
        use lsp_types::SemanticTokenType;

        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///sem_event.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "START-OF-SELECTION.\n".to_string(),
                },
            },
        );

        let snapshot = state.cache.get("file:///sem_event.abap").expect("snapshot");
        let tokens = sem_tokens::build_semantic_tokens(snapshot.as_ref());
        let legend = sem_tokens::semantic_tokens_legend();
        let event_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::EVENT)
            .expect("legend has event") as u32;
        let decl_mod = 1u32
            << legend
                .token_modifiers
                .iter()
                .position(|m| m.as_str() == "declaration")
                .expect("declaration modifier");

        assert!(
            tokens.data.iter().any(|token| {
                token.delta_line == 0
                    && token.delta_start == 0
                    && token.length == "START-OF-SELECTION".len() as u32
                    && token.token_type == event_idx
                    && (token.token_modifiers_bitset & decl_mod) != 0
            }),
            "expected a declaration token spanning the full event header"
        );
    }

    #[test]
    fn semantic_tokens_and_hover_cover_super_constructor_call() {
        use lsp_types::SemanticTokenType;

        let state = ServerState::default();
        let text = "\
CLASS some_parent DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_value TYPE i.
ENDCLASS.

CLASS some_parent IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
ENDCLASS.

CLASS some_child DEFINITION INHERITING FROM some_parent.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_value TYPE i.
ENDCLASS.

CLASS some_child IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_value = iv_value ).
  ENDMETHOD.
ENDCLASS.";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///super_ctor_hover.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let snapshot = state
            .cache
            .get("file:///super_ctor_hover.abap")
            .expect("snapshot");
        let tokens = sem_tokens::build_semantic_tokens(snapshot.as_ref());
        let legend = sem_tokens::semantic_tokens_legend();
        let class_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::CLASS)
            .expect("legend has class") as u32;
        let method_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::METHOD)
            .expect("legend has method") as u32;

        let super_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("super->constructor"))
            .expect("super call line");
        let super_col = super_line.1.find("super").expect("super column") as u32;
        let constructor_col = super_line
            .1
            .find("constructor")
            .expect("constructor column") as u32;

        assert_eq!(
            semantic_token_type_at(&tokens, super_line.0 as u32, super_col),
            Some(class_idx),
            "expected `super` to be highlighted as a class-like reference"
        );
        assert_eq!(
            semantic_token_type_at(&tokens, super_line.0 as u32, constructor_col),
            Some(method_idx),
            "expected `constructor` to be highlighted as a method"
        );

        let super_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///super_ctor_hover.abap").expect("uri"),
                    },
                    position: Position {
                        line: super_line.0 as u32,
                        character: super_col + 1,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("super hover");
        let HoverContents::Markup(super_markup) = super_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(super_markup.value.contains("Direct superclass reference"));
        assert!(super_markup.value.contains("some_parent"));

        let constructor_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///super_ctor_hover.abap").expect("uri"),
                    },
                    position: Position {
                        line: super_line.0 as u32,
                        character: constructor_col + 1,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("constructor hover");
        let HoverContents::Markup(constructor_markup) = constructor_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(constructor_markup.value.contains("METHODS constructor"));
        assert!(constructor_markup.value.contains("iv_value TYPE i"));
    }

    #[test]
    fn custom_notification_names_are_stable() {
        assert_eq!(
            RESOLVE_REMOTE_DEPENDENCIES,
            "abapls/resolveRemoteDependencies"
        );
        assert_eq!(
            REMOTE_DEPENDENCIES_UPDATED,
            "abapls/remoteDependenciesUpdated"
        );
        assert_eq!(
            WORKSPACE_MANIFEST_UPDATED,
            "abapls/workspaceManifestUpdated"
        );
        assert_eq!(
            DEPENDENCY_CACHE_REFRESH_REQUESTED,
            "abapls/dependencyCacheRefreshRequested"
        );
    }

    #[test]
    fn collects_message_class_remote_dependency_candidates() {
        let store = DocumentStore::default();
        let snapshot = store.publish(
            "file:///message_stmt.abap",
            1,
            "MESSAGE i043(/sttp/int_msg) WITH lv_lines iv_logsys iv_mode INTO DATA(lv_message).",
        );

        let candidates = collect_remote_dependency_candidates(snapshot.as_ref());

        assert!(candidates.iter().any(|candidate| {
            candidate.name == "/sttp/int_msg" && candidate.kind == "message-class"
        }));
    }

    #[test]
    fn collects_standard_message_class_remote_dependency_candidates() {
        let store = DocumentStore::default();
        let snapshot = store.publish(
            "file:///message_stmt_std.abap",
            1,
            "MESSAGE s398(00) WITH TEXT-007 DISPLAY LIKE 'E'.",
        );

        let candidates = collect_remote_dependency_candidates(snapshot.as_ref());

        assert!(
            candidates
                .iter()
                .any(|candidate| { candidate.name == "00" && candidate.kind == "message-class" })
        );
    }

    #[test]
    fn workspace_refresh_hydrates_message_classes_before_diagnostics() {
        let workspace_path = temp_workspace_path("message_class_startup_hydration");
        let source_dir = workspace_path.join("src");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(&source_dir).expect("source dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[performance]
mode = "full-workspace"

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "ZMSG"
kind = "report"
root_file = "src/ZMSG.abap"
"#,
        )
        .expect("manifest");
        let source_text = "\
REPORT zmsg MESSAGE-ID zmsgcls.

START-OF-SELECTION.
  MESSAGE e999(zmsgcls).
";
        fs::write(source_dir.join("ZMSG.abap"), source_text).expect("source");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/ZMSG.abap"));
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("default".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "message-class".to_string(),
                    object_name: "ZMSGCLS".to_string(),
                    object_uri: "/sap/bc/adt/messageclasses/zmsgcls".to_string(),
                    object_type: "MSAG".to_string(),
                    description: "Messages".to_string(),
                    file_extension: "xml".to_string(),
                    source_text: r#"
<mc:messageClass adtcore:name="ZMSGCLS"
    xmlns:mc="http://www.sap.com/adt/MessageClass"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <mc:messages mc:msgno="001" mc:msgtext="Existing message"/>
</mc:messageClass>
"#
                    .to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store message class");

        assert!(stage_workspace_preview_snapshot(
            &mut state,
            &source_uri,
            1,
            source_text
        ));
        let preview = snapshot_for_uri(&state, &source_uri).expect("preview snapshot");
        let preview_messages: Vec<_> = preview
            .symbols
            .diagnostics
            .iter()
            .map(|diagnostic| diagnostic.message.as_str())
            .collect();
        assert!(
            !preview_messages
                .iter()
                .any(|message| message.contains("unknown message class")),
            "{preview_messages:#?}"
        );
        assert!(
            preview_messages
                .iter()
                .any(|message| message.contains("unknown message id '999'")),
            "{preview_messages:#?}"
        );
        state
            .workspaces
            .get_mut(&normalize_lsp_uri(&workspace_uri))
            .expect("workspace")
            .preview_snapshots
            .clear();

        let refreshed = refresh_workspace(&mut state, &workspace_uri);
        let returned_messages: Vec<_> = refreshed
            .iter()
            .find(|snapshot| snapshot.uri.as_ref() == source_uri)
            .expect("returned source snapshot")
            .symbols
            .diagnostics
            .iter()
            .map(|diagnostic| diagnostic.message.as_str())
            .collect();
        assert!(
            !returned_messages
                .iter()
                .any(|message| message.contains("unknown message class")),
            "{returned_messages:#?}"
        );
        assert!(
            returned_messages
                .iter()
                .any(|message| message.contains("unknown message id '999'")),
            "{returned_messages:#?}"
        );
        let snapshot = state
            .workspaces
            .get(&normalize_lsp_uri(&workspace_uri))
            .and_then(|workspace| workspace.cache.get(&source_uri))
            .expect("source snapshot");
        let messages: Vec<_> = snapshot
            .symbols
            .diagnostics
            .iter()
            .map(|diagnostic| diagnostic.message.as_str())
            .collect();
        assert!(
            !messages
                .iter()
                .any(|message| message.contains("unknown message class")),
            "{messages:#?}"
        );
        assert!(
            messages
                .iter()
                .any(|message| message.contains("unknown message id '999'")),
            "{messages:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn remote_dependency_request_is_suppressed_by_syntax_errors() {
        let workspace_path = temp_workspace_path("syntax_gate");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "ZMAIN"
kind = "report"
root_file = "src/reports/ZMAIN/ZMAIN.abap"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/main.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "DATA lv_before TYPE i\nDATA lv_after TYPE zcl_remote_demo.".to_string(),
                },
            },
        );

        assert!(
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/main.abap"))
                .is_none()
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn remote_dependency_resolution_requires_dependency_store_profile() {
        let workspace_path = temp_workspace_path("missing_dependency_store_profile");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/main.abap");

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&source_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "DATA lo_remote TYPE REF TO zcl_remote_demo.".to_string(),
                },
            },
        );

        assert!(build_remote_dependency_request(&mut state, &source_uri).is_none());

        let manifest_diagnostics = workspace_manifest_diagnostics_params(&state, &workspace_uri)
            .expect("manifest diagnostics");
        assert!(manifest_diagnostics.diagnostics.is_empty());

        let error = store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri,
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_REMOTE_DEMO".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_remote_demo".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "CLASS zcl_remote_demo DEFINITION. ENDCLASS.".to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect_err("missing dependency store profile should reject artifact writes");
        assert!(error.contains("[dependency_store]"));

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn unresolved_interfaces_statement_emits_type_dependency_candidate() {
        let store = DocumentStore::default();
        let snapshot = store.publish(
            "file:///interfaces_remote.abap",
            1,
            "CLASS zcl_demo DEFINITION.\n  PUBLIC SECTION.\n    INTERFACES if_rest_client.\nENDCLASS.",
        );

        let candidates = collect_remote_dependency_candidates(snapshot.as_ref());
        assert!(
            candidates.iter().any(|candidate| {
                candidate.kind == "type" && candidate.name == "if_rest_client"
            }),
            "{candidates:#?}"
        );
    }

    #[test]
    fn unresolved_local_style_type_name_emits_type_dependency_candidate() {
        let store = DocumentStore::default();
        let snapshot = store.publish("file:///tt_remote.abap", 1, "DATA lt_ltap TYPE tt_ltap_vb.");

        let candidates = collect_remote_dependency_candidates(snapshot.as_ref());
        assert!(
            candidates
                .iter()
                .any(|candidate| { candidate.kind == "type" && candidate.name == "tt_ltap_vb" }),
            "{candidates:#?}"
        );
    }

    #[test]
    fn select_options_matchcode_object_emits_type_dependency_candidate() {
        let store = DocumentStore::default();
        let snapshot = store.publish(
            "file:///select_options_matchcode.abap",
            1,
            "DATA gv_gln TYPE string.\nSELECT-OPTIONS s_gln FOR gv_gln NO INTERVALS LOWER CASE MATCHCODE OBJECT /sttp/h_loc_gln.",
        );

        let candidates = collect_remote_dependency_candidates(snapshot.as_ref());
        assert!(
            candidates.iter().any(|candidate| {
                candidate.kind == "type" && candidate.name == "/sttp/h_loc_gln"
            }),
            "{candidates:#?}"
        );
    }

    #[test]
    fn collects_function_module_remote_dependency_candidates() {
        let store = DocumentStore::default();
        let snapshot = store.publish(
            "file:///function_remote.abap",
            1,
            "CALL FUNCTION '/AIF/FILE_PROCESS_DATA'.",
        );

        let candidates = collect_remote_dependency_candidates(snapshot.as_ref());
        assert!(candidates.iter().any(|candidate| {
            candidate.kind == "function" && candidate.name == "/aif/file_process_data"
        }));
    }

    #[test]
    fn definition_returns_location_for_function_module_call() {
        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///main.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "CALL FUNCTION '/AIF/FILE_PROCESS_DATA'.".to_string(),
                },
            },
        );
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///fm_dep.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "FUNCTION /aif/file_process_data\nENDFUNCTION.".to_string(),
                },
            },
        );

        let result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///main.abap").expect("uri"),
                    },
                    position: Position {
                        line: 0,
                        character: 17,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("definition");

        let GotoDefinitionResponse::Scalar(location) = result else {
            panic!("expected scalar location");
        };
        assert_eq!(
            location.uri,
            Uri::from_str("file:///fm_dep.abap").expect("uri")
        );
        assert_eq!(location.range.start.line, 0);
        assert_eq!(location.range.start.character, 9);
    }

    #[test]
    fn remote_dependency_request_still_builds_candidates_without_legacy_unknown_symbol_flag() {
        let workspace_path =
            temp_workspace_path("remote_candidates_without_legacy_unknown_symbol_flag");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/main.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "DATA lv_flag TYPE boolean.".to_string(),
                },
            },
        );

        let request =
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/main.abap"))
                .expect("remote request");
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.name == "boolean")
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn remote_dependency_request_preserves_legacy_parallelism_override() {
        let workspace_path = temp_workspace_path("manifest_legacy_parallelism");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
remote_request_parallelism = 6
remote_requests_per_second = 12
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/main.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "DATA lo_demo TYPE REF TO zcl_remote_demo.".to_string(),
                },
            },
        );

        let request =
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/main.abap"))
                .expect("remote request");
        assert_eq!(request.remote_request_parallelism, Some(6));
        assert_eq!(request.remote_requests_per_second, Some(12));

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn workspace_refresh_resolves_local_export_dependencies_from_unit_sidecars() {
        let workspace_path = temp_workspace_path("workspace_local_export_refresh");
        let export_root = temp_workspace_path("workspace_local_export_refresh_export");
        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(workspace_path.join("src/reports/ZREP")).expect("report dir");
        fs::create_dir_all(export_root.join("packages/ZFIC/ddic-data-element"))
            .expect("export dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP.abap"),
            "REPORT zrep.",
        )
        .expect("report");
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP_TOP.abap"),
            "DATA lv_status TYPE zzf_status_code.\n",
        )
        .expect("top include");
        fs::write(
            workspace_path.join("src/reports/ZREP/abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("sidecar");
        fs::write(
            export_root.join("packages/ZFIC/ddic-data-element/ZZF_STATUS_CODE.xml"),
            r#"<?xml version="1.0" encoding="utf-8"?><dataElement />"#,
        )
        .expect("export");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let target_uri =
            normalize_lsp_uri(&format!("{workspace_uri}/src/reports/ZREP/ZREP_TOP.abap"));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let snapshot = snapshot_for_uri(&state, &target_uri).expect("snapshot");
        assert!(
            !snapshot
                .symbols
                .diagnostics
                .iter()
                .any(|diag| diag.message.contains("zzf_status_code")),
            "{:#?}",
            snapshot.symbols.diagnostics
        );
        assert!(
            build_remote_dependency_request(&mut state, &target_uri).is_none(),
            "resolved local export should not trigger remote request"
        );

        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn local_first_export_populates_dependency_store_and_reuses_cache() {
        let workspace_path = temp_workspace_path("workspace_local_export_populates_store");
        let export_root = temp_workspace_path("workspace_local_export_populates_store_export");
        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(workspace_path.join("src")).expect("src dir");
        let export_class_dir = export_root.join("packages/ZPKG/global-class");
        fs::create_dir_all(&export_class_dir).expect("export dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let source_text = "DATA lo_cached TYPE REF TO zcl_cached.\n";
        fs::write(workspace_path.join("src/ZMAIN.abap"), source_text).expect("source");
        fs::write(
            workspace_path.join("src/abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("sidecar");
        let local_export_path = export_class_dir.join("ZCL_CACHED.abap");
        fs::write(
            &local_export_path,
            "CLASS zcl_cached DEFINITION PUBLIC.\n  PUBLIC SECTION.\n    CLASS-METHODS local_only.\nENDCLASS.\nCLASS zcl_cached IMPLEMENTATION.\nENDCLASS.\n",
        )
        .expect("local export");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri =
            normalize_lsp_uri(&path_to_file_uri(&workspace_path.join("src/ZMAIN.abap")));
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let type_offset = source_text.find("zcl_cached").expect("type ref");
        let definition_result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&source_uri).expect("uri"),
                    },
                    position: offset_to_position(source_text, type_offset + 1).expect("position"),
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("definition");
        let GotoDefinitionResponse::Scalar(location) = definition_result else {
            panic!("expected scalar location");
        };
        assert_eq!(
            location.uri.scheme().map(|scheme| scheme.as_str()),
            Some("abapls-cache")
        );
        let dependency_uri = location.uri.to_string();
        let initial_cached_text = read_dependency_document(
            &state,
            &ReadDependencyDocumentParams {
                uri: dependency_uri.clone(),
            },
        )
        .expect("read dependency document")
        .expect("dependency document")
        .source_text;
        assert!(
            initial_cached_text.contains("local_only"),
            "{initial_cached_text}"
        );
        let normalized_workspace_uri = normalize_lsp_uri(&workspace_uri);
        let before_open_revision = state
            .workspaces
            .get(&normalized_workspace_uri)
            .expect("workspace")
            .cache
            .last_analysis_revision();
        let opened_cached = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&dependency_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 2,
                    text: initial_cached_text.clone(),
                },
            },
        );
        assert!(opened_cached.is_dependency);
        assert_eq!(
            state
                .workspaces
                .get(&normalized_workspace_uri)
                .expect("workspace")
                .cache
                .last_analysis_revision(),
            before_open_revision
        );

        let local_export_uri = normalize_lsp_uri(&path_to_file_uri(&local_export_path));
        let workspace = state
            .workspace_for_uri(&source_uri)
            .expect("workspace after refresh");
        let store = workspace_dependency_store(workspace).expect("dependency store");
        let profile = workspace
            .dependency_profile
            .clone()
            .expect("dependency profile");
        let matching_records: Vec<_> = store
            .list_artifacts_by_kind(&profile, "global-class")
            .expect("list global classes")
            .into_iter()
            .filter(|record| record.object_name == "zcl_cached")
            .collect();
        assert_eq!(matching_records.len(), 1, "{matching_records:#?}");
        assert!(matching_records[0].source_text.contains("local_only"));
        assert!(
            workspace.cache.get(&local_export_uri).is_none(),
            "local export file should be stored as a sqlite-backed virtual dependency"
        );

        fs::write(
            &local_export_path,
            "CLASS zcl_cached DEFINITION PUBLIC.\n  PUBLIC SECTION.\n    CLASS-METHODS changed_only.\nENDCLASS.\nCLASS zcl_cached IMPLEMENTATION.\nENDCLASS.\n",
        )
        .expect("changed local export");
        refresh_workspace(&mut state, &workspace_uri);
        let cached_text_after_refresh = read_dependency_document(
            &state,
            &ReadDependencyDocumentParams {
                uri: dependency_uri,
            },
        )
        .expect("read dependency document")
        .expect("dependency document")
        .source_text;
        assert!(cached_text_after_refresh.contains("local_only"));
        assert!(!cached_text_after_refresh.contains("changed_only"));

        assert!(
            build_remote_dependency_request(&mut state, &source_uri).is_none(),
            "cached sqlite artifact should suppress ADT lookup"
        );

        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn opened_cached_local_export_dependency_stays_dependency_surface() {
        let workspace_path = temp_workspace_path("opened_local_export_dependency_store");
        let export_root = temp_workspace_path("opened_local_export_dependency_store_export");
        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(workspace_path.join("src")).expect("src dir");
        let class_dir = export_root.join("ZPKG/Source Code Library/Classes");
        fs::create_dir_all(&class_dir).expect("class dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let main_source = "\
REPORT zmain.
START-OF-SELECTION.
  /sttp/cl_ar_dm_object=>main_processing_rel_step( ).
";
        fs::write(workspace_path.join("src/ZMAIN.abap"), main_source).expect("source");
        fs::write(
            workspace_path.join("src/abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("sidecar");
        let local_class_source = "\
CLASS /sttp/cl_ar_dm_object DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS main_processing_rel_step.
ENDCLASS.

CLASS /sttp/cl_ar_dm_object IMPLEMENTATION.
  METHOD main_processing_rel_step.
    DATA lv_objid TYPE /sttp/e_objid.
  ENDMETHOD.
ENDCLASS.
";
        let local_class_path = class_dir.join("%2FSTTP%2FCL_AR_DM_OBJECT.abap");
        fs::write(&local_class_path, local_class_source).expect("local class");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri =
            normalize_lsp_uri(&path_to_file_uri(&workspace_path.join("src/ZMAIN.abap")));
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "/STTP/DDIC".to_string(),
                    object_kind: "ddic-data-element".to_string(),
                    object_name: "/STTP/E_OBJID".to_string(),
                    object_uri: "/sap/bc/adt/ddic/dataelements/%2fsttp%2fe_objid".to_string(),
                    object_type: "DTEL/DE".to_string(),
                    description: "Remote data element".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "TYPES /sttp/e_objid TYPE c LENGTH 20.".to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifact");

        let method_offset = main_source
            .find("main_processing_rel_step")
            .expect("method ref");
        let definition_result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&source_uri).expect("uri"),
                    },
                    position: offset_to_position(main_source, method_offset + 1).expect("position"),
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("local export definition");
        let GotoDefinitionResponse::Scalar(class_location) = definition_result else {
            panic!("expected scalar location");
        };
        assert_eq!(
            class_location.uri.scheme().map(|scheme| scheme.as_str()),
            Some("abapls-cache")
        );

        let cached_dependency_uri = normalize_lsp_uri(class_location.uri.as_str());
        let cached_dependency_text = read_dependency_document(
            &state,
            &ReadDependencyDocumentParams {
                uri: cached_dependency_uri.clone(),
            },
        )
        .expect("read dependency document")
        .expect("dependency document")
        .source_text;
        let opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&cached_dependency_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 2,
                    text: cached_dependency_text.clone(),
                },
            },
        );
        assert!(opened.is_dependency);
        assert!(
            state.workspace_for_uri(&cached_dependency_uri).is_some(),
            "cached local-export dependency should route back to its workspace"
        );
        let type_offset = cached_dependency_text
            .find("/sttp/e_objid")
            .expect("type ref");
        assert!(opened.definition_at(type_offset + 1).is_none());

        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn workspace_refresh_indexes_local_adt_ddic_xml_for_types_and_open_sql() {
        let workspace_path = temp_workspace_path("workspace_local_adt_ddic_xml");
        let export_root = temp_workspace_path("workspace_local_adt_ddic_xml_export");
        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(workspace_path.join("src/reports/ZREP")).expect("report dir");
        fs::create_dir_all(export_root.join("ZPKG/Dictionary/Database Tables")).expect("table dir");
        fs::create_dir_all(export_root.join("ZPKG/Dictionary/Structures")).expect("structure dir");
        fs::create_dir_all(export_root.join("ZPKG/Dictionary/Table Types"))
            .expect("table type dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP.abap"),
            r#"
REPORT zrep.
DATA ls_monitor TYPE zarrive_monitor.
DATA lt_return TYPE zattp_tt_bapiret2.

SELECT mguid, docref, evt_time
  FROM zattp_agg_pro
  INTO TABLE @DATA(lt_rows).

SELECT ztb~missing
  FROM zattp_agg_pro AS ztb
  INTO TABLE @DATA(lt_bad).
"#,
        )
        .expect("report");
        fs::write(
            workspace_path.join("src/reports/ZREP/abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("sidecar");
        fs::write(
            export_root.join("ZPKG/Dictionary/Database Tables/ZATTP_AGG_PRO.xml"),
            r#"
<abapsource:elementInfo adtcore:name="zattp_agg_pro"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="mguid">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicIsKey">true</abapsource:entry>
      <abapsource:entry abapsource:key="ddicDataType">char</abapsource:entry>
      <abapsource:entry abapsource:key="ddicLength">000032</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="docref">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataType">char</abapsource:entry>
      <abapsource:entry abapsource:key="ddicLength">000020</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="evt_time">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataType">dec</abapsource:entry>
      <abapsource:entry abapsource:key="ddicLength">000021</abapsource:entry>
      <abapsource:entry abapsource:key="ddicDecimals">000007</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#,
        )
        .expect("table xml");
        fs::write(
            export_root.join("ZPKG/Dictionary/Structures/ZARRIVE_MONITOR.xml"),
            r#"
<abapsource:elementInfo adtcore:name="zarrive_monitor"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="delivery">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataType">char</abapsource:entry>
      <abapsource:entry abapsource:key="ddicLength">000020</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#,
        )
        .expect("structure xml");
        fs::write(
            export_root.join("ZPKG/Dictionary/Structures/ZATTP_DT_BAPIRET2.xml"),
            r#"
<abapsource:elementInfo adtcore:name="zattp_dt_bapiret2"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="message">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataType">string</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#,
        )
        .expect("row structure xml");
        fs::write(
            export_root.join("ZPKG/Dictionary/Table Types/ZATTP_TT_BAPIRET2.xml"),
            r#"
<abapsource:elementInfo adtcore:name="zattp_tt_bapiret2"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DS" adtcore:name="zattp_dt_bapiret2">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicRowType">X</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#,
        )
        .expect("table type xml");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let target_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/reports/ZREP/ZREP.abap"));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let snapshot = snapshot_for_uri(&state, &target_uri).expect("snapshot");
        assert!(
            !snapshot.symbols.diagnostics.iter().any(|diag| {
                matches!(
                    diag.kind,
                    DiagnosticKind::UnresolvedReference | DiagnosticKind::UnverifiedOpenSqlSource
                )
            }),
            "{:#?}",
            snapshot.symbols.diagnostics
        );
        assert!(
            snapshot.symbols.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnknownField && diag.message.contains("missing")
            }),
            "expected SQL UnknownField, got {:#?}",
            snapshot.symbols.diagnostics
        );
        assert!(
            build_remote_dependency_request(&mut state, &target_uri).is_none(),
            "resolved local DDIC XML should not trigger remote request"
        );

        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn workspace_refresh_prefers_workspace_include_over_same_named_local_export() {
        let workspace_path = temp_workspace_path("workspace_local_include_before_export");
        let export_root = temp_workspace_path("workspace_local_include_before_export_root");
        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(workspace_path.join("src/reports/ZREP")).expect("report dir");
        fs::create_dir_all(export_root.join("includes")).expect("export includes dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP.abap"),
            "REPORT zrep.\nINCLUDE zrep_top.\n",
        )
        .expect("report");
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP_TOP.abap"),
            "DATA lv_workspace TYPE i.\n",
        )
        .expect("workspace include");
        fs::write(
            workspace_path.join("src/reports/ZREP/abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("sidecar");
        fs::write(
            export_root.join("includes/ZREP_TOP.abap"),
            "DATA lv_export TYPE i.\n",
        )
        .expect("export include");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let report_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/reports/ZREP/ZREP.abap"));
        let workspace_include_uri =
            normalize_lsp_uri(&format!("{workspace_uri}/src/reports/ZREP/ZREP_TOP.abap"));
        let export_include_uri = normalize_lsp_uri(&path_to_file_uri(
            &export_root.join("includes/ZREP_TOP.abap"),
        ));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let snapshot = snapshot_for_uri(&state, &report_uri).expect("snapshot");
        let target_uri = snapshot
            .symbols
            .include_edges
            .iter()
            .find(|edge| edge.name.as_ref() == "zrep_top")
            .and_then(|edge| edge.target)
            .and_then(|target| snapshot.project.units.get(target.as_usize()))
            .map(|unit| unit.uri.as_ref().to_string())
            .expect("include target");
        assert_eq!(target_uri, workspace_include_uri);
        assert!(
            snapshot
                .project
                .units
                .iter()
                .all(|unit| unit.uri.as_ref() != export_include_uri),
            "local export duplicate should not be loaded when the workspace include exists"
        );

        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn workspace_refresh_resolves_flat_src_report_includes_without_opening_them() {
        let workspace_path = temp_workspace_path("workspace_flat_src_report_includes");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(workspace_path.join("src/ZREP")).expect("report dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("src/ZREP/ZREP.abap"),
            "REPORT zrep.\nINCLUDE: zrep_top,\n         zrep_cls.\nSTART-OF-SELECTION.\n  CREATE OBJECT gr_demo.\n  CALL METHOD gr_demo->get_data.\n",
        )
        .expect("report");
        fs::write(
            workspace_path.join("src/ZREP/ZREP_TOP.abap"),
            "CLASS lcl_demo DEFINITION.\n  PUBLIC SECTION.\n    METHODS get_data.\nENDCLASS.\n",
        )
        .expect("top include");
        fs::write(
            workspace_path.join("src/ZREP/ZREP_CLS.abap"),
            "DATA gr_demo TYPE REF TO lcl_demo.\nCLASS lcl_demo IMPLEMENTATION.\n  METHOD get_data.\n  ENDMETHOD.\nENDCLASS.\n",
        )
        .expect("class include");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let report_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/ZREP/ZREP.abap"));
        let top_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/ZREP/ZREP_TOP.abap"));
        let cls_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/ZREP/ZREP_CLS.abap"));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        assert!(
            snapshot_for_uri(&state, &top_uri).is_some(),
            "top include should be loaded by workspace refresh"
        );
        assert!(
            snapshot_for_uri(&state, &cls_uri).is_some(),
            "class include should be loaded by workspace refresh"
        );
        let snapshot = snapshot_for_uri(&state, &report_uri).expect("report snapshot");
        let include_targets: Vec<_> = snapshot
            .symbols
            .include_edges
            .iter()
            .filter_map(|edge| edge.target)
            .filter_map(|target| snapshot.project.units.get(target.as_usize()))
            .map(|unit| unit.uri.as_ref())
            .collect();

        assert!(include_targets.contains(&top_uri.as_str()));
        assert!(include_targets.contains(&cls_uri.as_str()));
        assert!(
            !snapshot.symbols.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedInclude
                    || diag.message.contains("unknown symbol 'gr_demo'")
                    || diag.message.contains("unknown field or method 'get_data'")
            }),
            "{:#?}",
            snapshot.symbols.diagnostics
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn workspace_refresh_uses_single_actual_case_uri_for_namespaced_selection_include() {
        let workspace_path = temp_workspace_path("workspace_namespaced_selection_include_case");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(workspace_path.join("src/Includes")).expect("includes dir");
        fs::write(workspace_path.join("abapls.toml"), "version = 1\n").expect("manifest");
        fs::write(
            workspace_path.join("src/basic.abap"),
            "REPORT zattp_ar_dm_obj_rel.\n\
INCLUDE /sttp/ar_dm_obj_rel_top.\n\
INCLUDE /sttp/ar_dm_obj_rel_ssc.\n\
INCLUDE /sttp/ar_dm_obj_rel_f01.\n",
        )
        .expect("report");
        fs::write(
            workspace_path.join("src/Includes/%2FSTTP%2FAR_DM_OBJ_REL_TOP.abap"),
            "DATA gv_code_char TYPE string.\n",
        )
        .expect("top include");
        fs::write(
            workspace_path.join("src/Includes/%2FSTTP%2FAR_DM_OBJ_REL_SSC.abap"),
            "SELECT-OPTIONS pr_codch FOR gv_code_char.\n",
        )
        .expect("selection include");
        let f01_path = workspace_path.join("src/Includes/%2FSTTP%2FAR_DM_OBJ_REL_F01.abap");
        let f01_text = "FORM main_processing.\n\
  FIELD-SYMBOLS <ls_codch> LIKE LINE OF pr_codch.\n\
  LOOP AT pr_codch ASSIGNING <ls_codch>.\n\
  ENDLOOP.\n\
ENDFORM.\n";
        fs::write(&f01_path, f01_text).expect("form include");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let f01_uri = normalize_lsp_uri(&path_to_file_uri(&f01_path));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let workspace = state
            .workspaces
            .get(&normalize_lsp_uri(&workspace_uri))
            .expect("workspace");
        let f01_uris: Vec<_> = workspace
            .cache
            .uris()
            .into_iter()
            .filter(|uri| uri.contains("AR_DM_OBJ_REL_F01"))
            .collect();
        assert_eq!(f01_uris, vec![Arc::<str>::from(f01_uri.as_str())]);

        let snapshot = snapshot_for_uri(&state, &f01_uri).expect("f01 snapshot");
        let diagnostics = build_lsp_diagnostics_for_workspace(Some(workspace), snapshot.as_ref());
        assert!(
            diagnostics
                .iter()
                .all(|diag| !diag.message.contains("pr_codch")),
            "{diagnostics:#?}"
        );

        let opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&f01_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 2,
                    text: f01_text.to_string(),
                },
            },
        );
        let workspace = state
            .workspaces
            .get(&normalize_lsp_uri(&workspace_uri))
            .expect("workspace");
        let f01_uris: Vec<_> = workspace
            .cache
            .uris()
            .into_iter()
            .filter(|uri| uri.contains("AR_DM_OBJ_REL_F01"))
            .collect();
        assert_eq!(f01_uris, vec![Arc::<str>::from(f01_uri.as_str())]);
        let diagnostics = build_lsp_diagnostics_for_workspace(Some(workspace), opened.as_ref());
        assert!(
            diagnostics
                .iter()
                .all(|diag| !diag.message.contains("pr_codch")),
            "{diagnostics:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn workspace_refresh_resolves_flat_root_includes_without_opening_them() {
        let workspace_path = temp_workspace_path("workspace_flat_root_includes");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[resolution]
dependency_mode = "local-first"
cache_dir = "legacy-cache"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("basic.abap"),
            "INCLUDE decl.\nDATA ls_object_src TYPE ts_obj_ids.\n",
        )
        .expect("basic");
        fs::write(
            workspace_path.join("decl.abap"),
            "TYPES: BEGIN OF ts_obj_ids,\n  owner TYPE char12,\nEND OF ts_obj_ids.\n",
        )
        .expect("decl");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let basic_uri = normalize_lsp_uri(&format!("{workspace_uri}/basic.abap"));
        let decl_uri = normalize_lsp_uri(&format!("{workspace_uri}/decl.abap"));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        assert!(
            snapshot_for_uri(&state, &decl_uri).is_some(),
            "root include should be loaded by workspace refresh"
        );
        let snapshot = snapshot_for_uri(&state, &basic_uri).expect("basic snapshot");
        let include_targets: Vec<_> = snapshot
            .symbols
            .include_edges
            .iter()
            .filter_map(|edge| edge.target)
            .filter_map(|target| snapshot.project.units.get(target.as_usize()))
            .map(|unit| unit.uri.as_ref())
            .collect();

        assert!(include_targets.contains(&decl_uri.as_str()));
        assert!(
            !snapshot.symbols.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnresolvedInclude
                    || diag.message.contains("ts_obj_ids")
            }),
            "{:#?}",
            snapshot.symbols.diagnostics
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn workspace_refresh_does_not_leak_unincluded_flat_src_siblings() {
        let workspace_path = temp_workspace_path("workspace_flat_src_unincluded_siblings");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(workspace_path.join("src/ZREP")).expect("report dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("src/ZREP/ZREP.abap"),
            "REPORT zrep.\nSTART-OF-SELECTION.\n  gr_demo = 1.\n",
        )
        .expect("report");
        fs::write(
            workspace_path.join("src/ZREP/ZREP_TOP.abap"),
            "DATA gr_demo TYPE i.\n",
        )
        .expect("sibling");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let report_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/ZREP/ZREP.abap"));
        let sibling_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/ZREP/ZREP_TOP.abap"));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        assert!(
            snapshot_for_uri(&state, &sibling_uri).is_some(),
            "sibling should still be loaded as its own project unit"
        );
        let snapshot = snapshot_for_uri(&state, &report_uri).expect("report snapshot");
        assert!(
            snapshot.symbols.references.iter().any(|reference| {
                reference.name.as_ref() == "gr_demo" && reference.resolution.is_none()
            }),
            "{:#?}",
            snapshot.symbols.references
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn workspace_refresh_resolves_transitive_local_export_dependencies() {
        let workspace_path = temp_workspace_path("workspace_local_export_transitive_refresh");
        let export_root = temp_workspace_path("workspace_local_export_transitive_refresh_export");
        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(workspace_path.join("src/reports/ZREP")).expect("report dir");
        fs::create_dir_all(export_root.join("packages/ZPKG/global-class")).expect("class dir");
        fs::create_dir_all(export_root.join("packages/ZPKG/ddic-data-element")).expect("type dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP.abap"),
            "REPORT zrep.",
        )
        .expect("report");
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP_TOP.abap"),
            "DATA lo_factory TYPE REF TO zcl_factory.\n",
        )
        .expect("top include");
        fs::write(
            workspace_path.join("src/reports/ZREP/abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("sidecar");
        fs::write(
            export_root.join("packages/ZPKG/global-class/ZCL_FACTORY.abap"),
            "\
CLASS zcl_factory DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS get_status RETURNING VALUE(rv_status) TYPE zzf_status_code.
ENDCLASS.
CLASS zcl_factory IMPLEMENTATION.
  METHOD get_status.
  ENDMETHOD.
ENDCLASS.
",
        )
        .expect("class export");
        fs::write(
            export_root.join("packages/ZPKG/ddic-data-element/ZZF_STATUS_CODE.xml"),
            r#"<?xml version="1.0" encoding="utf-8"?><dataElement />"#,
        )
        .expect("type export");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let target_uri =
            normalize_lsp_uri(&format!("{workspace_uri}/src/reports/ZREP/ZREP_TOP.abap"));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let snapshot = snapshot_for_uri(&state, &target_uri).expect("snapshot");
        assert!(
            !snapshot
                .symbols
                .diagnostics
                .iter()
                .any(|diag| diag.message.contains("zcl_factory")),
            "{:#?}",
            snapshot.symbols.diagnostics
        );
        assert!(
            build_remote_dependency_request(&mut state, &target_uri).is_none(),
            "transitive local export should not trigger remote request"
        );

        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn workspace_refresh_resolves_legacy_layout_static_local_export_dependencies() {
        let workspace_path = temp_workspace_path("workspace_local_export_legacy_static_refresh");
        let export_root =
            temp_workspace_path("workspace_local_export_legacy_static_refresh_export");
        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(workspace_path.join("src/reports/ZREP")).expect("report dir");
        fs::create_dir_all(export_root.join("ZPKG/Source Code Library/Classes"))
            .expect("class dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP.abap"),
            "REPORT zrep.",
        )
        .expect("report");
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP_TOP.abap"),
            "DATA lo_factory TYPE REF TO zcl_factory.\n",
        )
        .expect("top include");
        fs::write(
            workspace_path.join("src/reports/ZREP/abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("sidecar");
        fs::write(
            export_root.join("ZPKG/Source Code Library/Classes/ZCL_FACTORY.abap"),
            "\
CLASS zcl_factory DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS build.
ENDCLASS.
CLASS zcl_factory IMPLEMENTATION.
  METHOD build.
    zcl_helper=>assist( ).
  ENDMETHOD.
ENDCLASS.
",
        )
        .expect("factory export");
        fs::write(
            export_root.join("ZPKG/Source Code Library/Classes/ZCL_HELPER.abap"),
            "\
CLASS zcl_helper DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS assist.
ENDCLASS.
CLASS zcl_helper IMPLEMENTATION.
  METHOD assist.
  ENDMETHOD.
ENDCLASS.
",
        )
        .expect("helper export");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let target_uri =
            normalize_lsp_uri(&format!("{workspace_uri}/src/reports/ZREP/ZREP_TOP.abap"));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let target_snapshot = snapshot_for_uri(&state, &target_uri).expect("target snapshot");
        assert!(
            !target_snapshot
                .symbols
                .diagnostics
                .iter()
                .any(|diag| diag.message.contains("zcl_factory")),
            "{:#?}",
            target_snapshot.symbols.diagnostics
        );

        let workspace = state
            .workspaces
            .get(&normalize_lsp_uri(&workspace_uri))
            .expect("workspace");
        let factory_uri = dependency_uri_for_object_name(&state, &workspace_uri, "ZCL_FACTORY");
        assert_eq!(
            workspace
                .cache
                .get(&factory_uri)
                .expect("factory snapshot")
                .uri
                .as_ref(),
            factory_uri.as_str()
        );
        let factory_snapshot = snapshot_for_uri(&state, &factory_uri).expect("factory snapshot");
        assert!(
            !factory_snapshot
                .symbols
                .diagnostics
                .iter()
                .any(|diag| diag.message.contains("zcl_helper")),
            "{:#?}",
            factory_snapshot.symbols.diagnostics
        );
        assert!(
            workspace.cache.uris().into_iter().all(|uri| {
                workspace
                    .cache
                    .get(uri.as_ref())
                    .and_then(|snapshot| snapshot.object_name.as_ref().map(|name| name.to_string()))
                    .is_none_or(|name| !name.eq_ignore_ascii_case("ZCL_HELPER"))
            }),
            "transitive helper dependency should stay out of the editor startup graph"
        );
        assert!(
            build_remote_dependency_request(&mut state, &target_uri).is_none(),
            "legacy local export should not trigger remote request"
        );

        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn collect_local_export_dependency_candidates_includes_static_targets_for_dependencies() {
        let document = WorkspaceDocument {
            uri: Arc::from("file:///factory.abap"),
            version: 0,
            text: "\
CLASS zcl_factory DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS build.
ENDCLASS.
CLASS zcl_factory IMPLEMENTATION.
  METHOD build.
    zcl_helper=>assist( ).
  ENDMETHOD.
ENDCLASS.
"
            .to_string(),
            is_dependency: true,
            object_name: Some(Arc::from("zcl_factory")),
        };

        let candidates = collect_local_export_dependency_candidates(&document);
        assert!(
            candidates
                .iter()
                .any(|candidate| candidate.kind == "static" && candidate.name == "zcl_helper"),
            "{candidates:#?}"
        );
    }

    #[test]
    fn local_export_dependency_candidate_cache_reuses_unchanged_text_and_refreshes_changes() {
        clear_local_export_dependency_candidate_cache_for_tests();

        let base_document = WorkspaceDocument {
            uri: Arc::from("file:///candidate-cache/factory.abap"),
            version: 0,
            text: "\
CLASS zcl_factory DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS build.
ENDCLASS.
CLASS zcl_factory IMPLEMENTATION.
  METHOD build.
    zcl_helper=>assist( ).
  ENDMETHOD.
ENDCLASS.
"
            .to_string(),
            is_dependency: true,
            object_name: Some(Arc::from("zcl_factory")),
        };

        let (first, first_profile) =
            collect_local_export_dependency_candidates_profiled(&base_document);
        assert!(!first_profile.cache_hit);
        assert!(
            first
                .iter()
                .any(|candidate| { candidate.kind == "static" && candidate.name == "zcl_helper" })
        );

        let (second, second_profile) =
            collect_local_export_dependency_candidates_profiled(&base_document);
        assert!(second_profile.cache_hit);
        assert_eq!(first, second);
        assert_eq!(second_profile.parse_analyze_time, Duration::ZERO);

        let changed_document = WorkspaceDocument {
            text: base_document.text.replace("zcl_helper", "zcl_other"),
            ..base_document.clone()
        };
        let (changed, changed_profile) =
            collect_local_export_dependency_candidates_profiled(&changed_document);
        assert!(!changed_profile.cache_hit);
        assert!(
            changed
                .iter()
                .any(|candidate| { candidate.kind == "static" && candidate.name == "zcl_other" })
        );
        assert!(
            !changed
                .iter()
                .any(|candidate| { candidate.kind == "static" && candidate.name == "zcl_helper" })
        );
    }

    #[test]
    fn workspace_refresh_uses_editor_workspace_build_plan() {
        let workspace_path = temp_workspace_path("workspace_editor_build_plan");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("main.abap"),
            "\
REPORT zworkspace_plan.

DATA lv_value TYPE i.

START-OF-SELECTION.
  lv_value = 1.
",
        )
        .expect("main");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = normalize_lsp_uri(&format!("{workspace_uri}/main.abap"));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let snapshot = snapshot_for_uri(&state, &source_uri).expect("snapshot");
        let metrics = state
            .workspaces
            .get(&normalize_lsp_uri(&workspace_uri))
            .and_then(|workspace| workspace.cache.last_analysis_metrics_snapshot())
            .expect("workspace analysis metrics");

        assert!(!snapshot.routine_analysis().routines.is_empty());
        assert!(snapshot.static_analysis().is_none());
        assert!(snapshot.call_graph().nodes.is_empty());
        assert!(snapshot.callable_summaries().summaries.is_empty());
        assert_eq!(metrics.static_analysis_summary_micros, 0);
        assert_eq!(metrics.callable_summary_micros, 0);

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn workspace_include_member_semantic_tokens_and_inlay_hints_cover_local_export_function_module_calls()
     {
        use lsp_types::SemanticTokenType;

        let workspace_path = temp_workspace_path("workspace_local_export_function_semantics");
        let export_root = temp_workspace_path("workspace_local_export_function_semantics_export");
        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(workspace_path.join("src/reports/ZREP")).expect("report dir");
        fs::create_dir_all(export_root.join("packages/STXD/function-module")).expect("export dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP.abap"),
            "REPORT zrep.\nINCLUDE zrep_f01.\nSTART-OF-SELECTION.\n  PERFORM f_save_text.\n",
        )
        .expect("report");
        let include_text = "\
FORM f_save_text.
  DATA ls_header TYPE thead.
  DATA ls_header_new TYPE thead.
  DATA lt_tline TYPE STANDARD TABLE OF tline WITH EMPTY KEY.
  DATA lw_value TYPE c LENGTH 1.
  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      header          = ls_header
      insert          = abap_true
      savemode_direct = abap_true
    IMPORTING
      function        = lw_value
      newheader       = ls_header_new
    TABLES
      lines           = lt_tline
    EXCEPTIONS
      id              = 1
      language        = 2
      name            = 3
      object          = 4
      OTHERS          = 5.
ENDFORM.
";
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP_F01.abap"),
            include_text,
        )
        .expect("include");
        fs::write(
            workspace_path.join("src/reports/ZREP/abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("sidecar");
        fs::write(
            export_root.join("packages/STXD/function-module/SAVE_TEXT.abap"),
            "\
FUNCTION SAVE_TEXT
  IMPORTING
    VALUE(CLIENT) LIKE SY-MANDT DEFAULT SY-MANDT
    VALUE(HEADER) LIKE THEAD
    VALUE(INSERT) TYPE ANY DEFAULT SPACE ##ADT_PARAMETER_UNTYPED
    VALUE(SAVEMODE_DIRECT) TYPE ANY DEFAULT SPACE ##ADT_PARAMETER_UNTYPED
    VALUE(OWNER_SPECIFIED) TYPE ANY DEFAULT SPACE ##ADT_PARAMETER_UNTYPED
    VALUE(LOCAL_CAT) TYPE ANY DEFAULT SPACE ##ADT_PARAMETER_UNTYPED
    VALUE(KEEP_LAST_CHANGED) TYPE ANY DEFAULT SPACE ##ADT_PARAMETER_UNTYPED
  EXPORTING
    VALUE(FUNCTION) TYPE ANY ##ADT_PARAMETER_UNTYPED
    VALUE(NEWHEADER) LIKE THEAD
  TABLES
    LINES LIKE TLINE
  EXCEPTIONS
    ID
    LANGUAGE
    NAME
    OBJECT.
ENDFUNCTION.",
        )
        .expect("export");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri =
            normalize_lsp_uri(&format!("{workspace_uri}/src/reports/ZREP/ZREP_F01.abap"));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&source_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: include_text.to_string(),
                },
            },
        );

        let snapshot = snapshot_for_uri(&state, &source_uri).expect("snapshot");
        let dep_unit = snapshot
            .project
            .units
            .iter()
            .find(|unit| {
                unit.function_modules.iter().any(|function_module| {
                    unit.symbol(function_module.symbol)
                        .name
                        .eq_ignore_ascii_case("save_text")
                })
            })
            .expect("dependency unit");
        let function_module = dep_unit.function_modules.first().expect("function module");
        let parameter_names: Vec<_> = function_module
            .parameters
            .iter()
            .map(|parameter| parameter.name.as_ref())
            .collect();
        assert_eq!(
            parameter_names,
            vec![
                "client",
                "header",
                "insert",
                "savemode_direct",
                "owner_specified",
                "local_cat",
                "keep_last_changed",
                "function",
                "newheader",
                "lines",
            ]
        );
        let exception_names: Vec<_> = function_module
            .exceptions
            .iter()
            .map(|exception| exception.name.as_ref())
            .collect();
        assert_eq!(exception_names, vec!["id", "language", "name", "object"]);
        let tokens = semantic_tokens(
            &state,
            &SemanticTokensParams {
                text_document: TextDocumentIdentifier {
                    uri: Uri::from_str(&source_uri).expect("uri"),
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("semantic tokens");
        let legend = sem_tokens::semantic_tokens_legend();
        let parameter_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::PARAMETER)
            .expect("legend has parameter") as u32;

        for (needle, marker) in [
            ("header", "      header          ="),
            ("insert", "      insert          ="),
            ("savemode_direct", "      savemode_direct ="),
            ("function", "      function        ="),
            ("newheader", "      newheader       ="),
            ("lines", "      lines           ="),
            ("id", "      id              ="),
            ("language", "      language        ="),
            ("name", "      name            ="),
            ("object", "      object          ="),
        ] {
            let offset = include_text.find(marker).expect("needle offset");
            let offset = offset + marker.find(needle).expect("needle in marker");
            let position = offset_to_position(include_text, offset + 1).expect("needle position");
            assert_eq!(
                semantic_token_type_at(&tokens, position.line, position.character),
                Some(parameter_idx),
                "expected semantic token for `{needle}`"
            );
        }

        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: offset_to_position(include_text, include_text.len()).expect("end position"),
        };
        let hints = inlay_hints(
            &state,
            &InlayHintParams {
                text_document: TextDocumentIdentifier {
                    uri: Uri::from_str(&source_uri).expect("uri"),
                },
                range,
                work_done_progress_params: Default::default(),
            },
        )
        .expect("inlay hints");

        let labels: Vec<_> = hints
            .iter()
            .map(|hint| match &hint.label {
                InlayHintLabel::String(label) => label.clone(),
                _ => String::new(),
            })
            .collect();
        assert_eq!(
            labels,
            vec![
                "THEAD".to_string(),
                "ANY".to_string(),
                "ANY".to_string(),
                "ANY".to_string(),
                "THEAD".to_string(),
                "STANDARD TABLE OF TLINE".to_string(),
            ]
        );

        let header_hint = hints.first().expect("header hint");
        let Some(InlayHintTooltip::MarkupContent(header_tooltip)) = header_hint.tooltip.as_ref()
        else {
            panic!("expected markdown tooltip");
        };
        assert!(
            header_tooltip
                .value
                .contains("parameter of FUNCTION MODULE `save_text`")
        );
        assert!(header_tooltip.value.contains("IMPORTING"));
        assert!(header_tooltip.value.contains("header LIKE thead"));

        assert!(
            snapshot
                .hovered_named_argument_at(
                    include_text
                        .find("      header          =")
                        .expect("header marker")
                        + "      ".len()
                        + 1,
                )
                .is_some(),
            "named argument hover should resolve against local export function module"
        );

        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn local_export_dependency_public_superclass_triggers_source_remote_request() {
        let workspace_path = temp_workspace_path("workspace_local_export_public_superclass_remote");
        let export_root =
            temp_workspace_path("workspace_local_export_public_superclass_remote_export");
        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(workspace_path.join("src/reports/ZREP")).expect("report dir");
        fs::create_dir_all(export_root.join("packages/ZPKG/global-class")).expect("export dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP.abap"),
            "REPORT zrep.\nDATA lo_doc TYPE REF TO zcl_document.\nSTART-OF-SELECTION.\n  lo_doc->add_text( ).\n",
        )
        .expect("report");
        fs::write(
            workspace_path.join("src/reports/ZREP/abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("sidecar");
        fs::write(
            export_root.join("packages/ZPKG/global-class/ZCL_DOCUMENT.abap"),
            "CLASS zcl_document DEFINITION PUBLIC INHERITING FROM zcl_area CREATE PUBLIC.\n  PUBLIC SECTION.\n    METHODS display_document.\nENDCLASS.\nCLASS zcl_document IMPLEMENTATION.\n  METHOD display_document.\n  ENDMETHOD.\nENDCLASS.\n",
        )
        .expect("exported class");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/reports/ZREP/ZREP.abap"));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let snapshot = snapshot_for_uri(&state, &source_uri).expect("snapshot");
        assert!(
            snapshot
                .symbols
                .diagnostics
                .iter()
                .any(|diag| diag.message.contains("unknown member 'add_text'")),
            "{:#?}",
            snapshot.symbols.diagnostics
        );

        let request =
            build_remote_dependency_request(&mut state, &source_uri).expect("remote request");
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.kind == "type" && candidate.name == "zcl_area"),
            "{:#?}",
            request.candidates
        );
        assert!(
            !request
                .candidates
                .iter()
                .any(|candidate| candidate.name == "zcl_document"),
            "{:#?}",
            request.candidates
        );

        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn workspace_refresh_resolves_submit_report_from_local_export_program_artifact() {
        let workspace_path = temp_workspace_path("workspace_local_export_submit_program");
        let export_root = temp_workspace_path("workspace_local_export_submit_program_export");
        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(workspace_path.join("src/reports/ZREP")).expect("report dir");
        fs::create_dir_all(export_root.join("packages/VN/report")).expect("export dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP.abap"),
            "REPORT zrep.\nSTART-OF-SELECTION.\n  SUBMIT rsnast00.\n",
        )
        .expect("report");
        fs::write(
            workspace_path.join("src/reports/ZREP/abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("sidecar");
        fs::write(
            export_root.join("packages/VN/report/RSNAST00.abap"),
            "PROGRAM rsnast00 MESSAGE-ID vn.\n",
        )
        .expect("export");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let target_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/reports/ZREP/ZREP.abap"));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let snapshot = snapshot_for_uri(&state, &target_uri).expect("snapshot");
        assert!(
            !snapshot
                .symbols
                .diagnostics
                .iter()
                .any(|diag| diag.message.contains("unknown symbol 'rsnast00'")),
            "{:#?}",
            snapshot.symbols.diagnostics
        );

        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn definition_persists_local_export_class_to_dependency_store() {
        let workspace_path = temp_workspace_path("workspace_local_export_definition");
        let export_root = temp_workspace_path("workspace_local_export_definition_export");
        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(workspace_path.join("src/reports/ZREP")).expect("report dir");
        fs::create_dir_all(export_root.join("packages/ZPKG/global-class")).expect("export dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP.abap"),
            "REPORT zrep.",
        )
        .expect("report");
        let source_text = "DATA lo_helper TYPE REF TO zcl_helper.\n";
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP_TOP.abap"),
            source_text,
        )
        .expect("top include");
        fs::write(
            workspace_path.join("src/reports/ZREP/abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("sidecar");
        fs::write(
            export_root.join("packages/ZPKG/global-class/ZCL_HELPER.abap"),
            "CLASS zcl_helper DEFINITION PUBLIC.\nENDCLASS.\nCLASS zcl_helper IMPLEMENTATION.\nENDCLASS.\n",
        )
        .expect("export");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri =
            normalize_lsp_uri(&format!("{workspace_uri}/src/reports/ZREP/ZREP_TOP.abap"));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let helper_col = source_text.find("zcl_helper").expect("type ref column") as u32 + 1;
        let result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&source_uri).expect("uri"),
                    },
                    position: Position {
                        line: 0,
                        character: helper_col,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("definition");
        let GotoDefinitionResponse::Scalar(location) = result else {
            panic!("expected scalar definition");
        };
        assert_eq!(
            location.uri.scheme().map(|scheme| scheme.as_str()),
            Some("abapls-cache")
        );
        assert!(
            read_dependency_document(
                &state,
                &ReadDependencyDocumentParams {
                    uri: location.uri.to_string()
                }
            )
            .expect("read dependency document")
            .expect("dependency document")
            .source_text
            .contains("CLASS zcl_helper DEFINITION")
        );

        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn opening_changed_workspace_file_rebuilds_local_export_dependencies() {
        let workspace_path = temp_workspace_path("workspace_local_export_open_changed");
        let export_root = temp_workspace_path("workspace_local_export_open_changed_export");
        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(workspace_path.join("src/reports/ZREP")).expect("report dir");
        fs::create_dir_all(export_root.join("packages/ZFIC/ddic-data-element"))
            .expect("export dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP.abap"),
            "REPORT zrep.",
        )
        .expect("report");
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP_TOP.abap"),
            "DATA lv_text TYPE string.\n",
        )
        .expect("top include");
        fs::write(
            workspace_path.join("src/reports/ZREP/abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("sidecar");
        fs::write(
            export_root.join("packages/ZFIC/ddic-data-element/ZZF_STATUS_CODE.xml"),
            r#"<?xml version="1.0" encoding="utf-8"?><dataElement />"#,
        )
        .expect("export");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let target_uri =
            normalize_lsp_uri(&format!("{workspace_uri}/src/reports/ZREP/ZREP_TOP.abap"));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&target_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 2,
                    text: "DATA lv_status TYPE zzf_status_code.\n".to_string(),
                },
            },
        );

        assert!(
            !opened
                .symbols
                .diagnostics
                .iter()
                .any(|diag| diag.message.contains("zzf_status_code")),
            "{:#?}",
            opened.symbols.diagnostics
        );
        assert!(
            build_remote_dependency_request(&mut state, &target_uri).is_none(),
            "changed file should rebuild with local export dependency"
        );

        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn changing_local_export_workspace_file_body_stays_incremental() {
        let workspace_path = temp_workspace_path("workspace_local_export_change_body_incremental");
        let export_root =
            temp_workspace_path("workspace_local_export_change_body_incremental_export");
        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(workspace_path.join("src")).expect("src dir");
        fs::create_dir_all(export_root.join("packages/ZPKG/global-class")).expect("export dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "ZCL_PROVIDER"
kind = "global-class"
root_file = "src/ZCL_PROVIDER.abap"

[[unit]]
name = "ZREP"
kind = "report"
root_file = "src/ZREP.abap"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("src/abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("sidecar");
        let provider_v1 = "\
CLASS zcl_provider DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS value RETURNING VALUE(rv_value) TYPE i.
ENDCLASS.
CLASS zcl_provider IMPLEMENTATION.
  METHOD value.
    zcl_ext=>assist( ).
    rv_value = 1.
  ENDMETHOD.
ENDCLASS.
";
        let provider_v2 = provider_v1.replace("rv_value = 1.", "rv_value = 2.");
        fs::write(workspace_path.join("src/ZCL_PROVIDER.abap"), provider_v1).expect("provider");
        fs::write(
            workspace_path.join("src/ZREP.abap"),
            "DATA lo_provider TYPE REF TO zcl_provider.\nSTART-OF-SELECTION.\n  lo_provider->value( ).\n",
        )
        .expect("consumer");
        fs::write(
            export_root.join("packages/ZPKG/global-class/ZCL_EXT.abap"),
            "\
CLASS zcl_ext DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS assist.
ENDCLASS.
CLASS zcl_ext IMPLEMENTATION.
  METHOD assist.
  ENDMETHOD.
ENDCLASS.
",
        )
        .expect("export");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let provider_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/ZCL_PROVIDER.abap"));
        let consumer_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/ZREP.abap"));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let changed = publish_changed_document_mut(
            &mut state,
            &DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: Uri::from_str(&provider_uri).expect("uri"),
                    version: 2,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: provider_v2,
                }],
            },
        )
        .expect("changed snapshot");

        let workspace = state
            .workspaces
            .get(&normalize_lsp_uri(&workspace_uri))
            .expect("workspace");
        let metrics = workspace
            .cache
            .last_analysis_metrics_snapshot()
            .expect("metrics");
        let dirty = workspace.cache.last_dirty_uris();

        assert_eq!(changed.version, 2);
        assert!(!metrics.full_rebuild);
        assert_eq!(metrics.parse_count, 1);
        assert_eq!(metrics.local_phase_count, 1);
        assert_eq!(metrics.dirty_unit_count, 1);
        assert!(dirty.contains(provider_uri.as_str()));
        assert!(!dirty.contains(consumer_uri.as_str()));
        assert!(
            dependency_uri_for_object_name(&state, &workspace_uri, "ZCL_EXT")
                .starts_with("abapls-cache:")
        );

        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn changing_local_export_workspace_file_signature_marks_dependents_without_full_rebuild() {
        let workspace_path =
            temp_workspace_path("workspace_local_export_change_signature_incremental");
        let export_root =
            temp_workspace_path("workspace_local_export_change_signature_incremental_export");
        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(workspace_path.join("src")).expect("src dir");
        fs::create_dir_all(&export_root).expect("export dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "ZCL_PROVIDER"
kind = "global-class"
root_file = "src/ZCL_PROVIDER.abap"

[[unit]]
name = "ZREP"
kind = "report"
root_file = "src/ZREP.abap"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("src/abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("sidecar");
        let provider_v1 = "\
CLASS zcl_provider DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS value RETURNING VALUE(rv_value) TYPE i.
ENDCLASS.
CLASS zcl_provider IMPLEMENTATION.
  METHOD value.
    rv_value = 1.
  ENDMETHOD.
ENDCLASS.
";
        let provider_v2 = "\
CLASS zcl_provider DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS value RETURNING VALUE(rv_value) TYPE i.
    METHODS extra.
ENDCLASS.
CLASS zcl_provider IMPLEMENTATION.
  METHOD value.
    rv_value = 1.
  ENDMETHOD.
  METHOD extra.
  ENDMETHOD.
ENDCLASS.
";
        fs::write(workspace_path.join("src/ZCL_PROVIDER.abap"), provider_v1).expect("provider");
        fs::write(
            workspace_path.join("src/ZREP.abap"),
            "DATA lo_provider TYPE REF TO zcl_provider.\nSTART-OF-SELECTION.\n  lo_provider->value( ).\n",
        )
        .expect("consumer");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let provider_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/ZCL_PROVIDER.abap"));
        let consumer_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/ZREP.abap"));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let changed = publish_changed_document_mut(
            &mut state,
            &DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: Uri::from_str(&provider_uri).expect("uri"),
                    version: 2,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: provider_v2.to_string(),
                }],
            },
        )
        .expect("changed snapshot");

        let workspace = state
            .workspaces
            .get(&normalize_lsp_uri(&workspace_uri))
            .expect("workspace");
        let metrics = workspace
            .cache
            .last_analysis_metrics_snapshot()
            .expect("metrics");
        let dirty = workspace.cache.last_dirty_uris();

        assert_eq!(changed.version, 2);
        assert!(!metrics.full_rebuild);
        assert_eq!(metrics.parse_count, 1);
        assert_eq!(metrics.local_phase_count, 1);
        assert_eq!(metrics.dirty_unit_count, 2);
        assert!(dirty.contains(provider_uri.as_str()));
        assert!(dirty.contains(consumer_uri.as_str()));

        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn workspace_manifest_refresh_enables_remote_dependency_requests() {
        let workspace_path = temp_workspace_path("manifest_refresh");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/main.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "DATA lo_demo TYPE REF TO zcl_remote_demo.\nlo_demo = zcl_remote_demo=>create( ).".to_string(),
                },
            },
        );

        let request =
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/main.abap"))
                .expect("remote request");
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.name == "zcl_remote_demo")
        );

        let _ = handle_dependency_cache_refresh_requested(
            &mut state,
            &WorkspaceManifestUpdatedParams {
                workspace_uri: workspace_uri.clone(),
            },
        );
        let request =
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/main.abap"))
                .expect("remote request after dependency cache refresh");
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.kind == "static")
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn opening_unchanged_workspace_file_reuses_existing_project_snapshot() {
        let workspace_path = temp_workspace_path("workspace_open_unchanged");
        fs::create_dir_all(workspace_path.join("src")).expect("src dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[[unit]]
name = "ZCL_MAIN"
kind = "global-class"
root_file = "src/ZCL_MAIN.abap"
"#,
        )
        .expect("manifest");
        let text = "CLASS zcl_main DEFINITION. ENDCLASS.";
        fs::write(workspace_path.join("src/ZCL_MAIN.abap"), text).expect("source");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/src/ZCL_MAIN.abap");
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        let before = snapshot_for_uri(&state, &normalize_lsp_uri(&source_uri)).expect("snapshot");

        let opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&source_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 7,
                    text: text.to_string(),
                },
            },
        );

        assert!(Arc::ptr_eq(&before.project, &opened.project));
        assert!(Arc::ptr_eq(&before.symbols, &opened.symbols));
        assert_eq!(opened.version, 7);

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn opening_unchanged_workspace_file_reuses_local_export_snapshot() {
        let workspace_path = temp_workspace_path("workspace_local_export_open_unchanged");
        let export_root = temp_workspace_path("workspace_local_export_open_unchanged_export");
        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(workspace_path.join("src/reports/ZREP")).expect("report dir");
        fs::create_dir_all(export_root.join("packages/ZFIC/ddic-data-element"))
            .expect("export dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let source_text = "DATA lv_status TYPE zzf_status_code.\n";
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP.abap"),
            "REPORT zrep.\nINCLUDE zrep_top.\n",
        )
        .expect("report");
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP_TOP.abap"),
            source_text,
        )
        .expect("top include");
        fs::write(
            workspace_path.join("src/reports/ZREP/abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("sidecar");
        fs::write(
            export_root.join("packages/ZFIC/ddic-data-element/ZZF_STATUS_CODE.xml"),
            r#"<?xml version="1.0" encoding="utf-8"?><dataElement />"#,
        )
        .expect("export");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let target_uri =
            normalize_lsp_uri(&format!("{workspace_uri}/src/reports/ZREP/ZREP_TOP.abap"));
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        let before = snapshot_for_uri(&state, &target_uri).expect("snapshot before open");

        fs::remove_file(export_root.join("packages/ZFIC/ddic-data-element/ZZF_STATUS_CODE.xml"))
            .expect("remove export");

        let opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&target_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 2,
                    text: source_text.to_string(),
                },
            },
        );

        assert!(Arc::ptr_eq(&before.project, &opened.project));
        assert!(Arc::ptr_eq(&before.symbols, &opened.symbols));
        assert!(
            !opened
                .symbols
                .diagnostics
                .iter()
                .any(|diag| diag.message.contains("zzf_status_code")),
            "{:#?}",
            opened.symbols.diagnostics
        );
        assert!(
            build_remote_dependency_request(&mut state, &target_uri).is_none(),
            "unchanged open should reuse the local export-backed snapshot"
        );

        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn opening_external_local_export_source_uses_dependency_surface_snapshot() {
        let workspace_path = temp_workspace_path("workspace_external_local_export_open");
        let export_root = temp_workspace_path("workspace_external_local_export_open_export");
        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(workspace_path.join("src/reports/ZREP")).expect("report dir");
        fs::create_dir_all(export_root.join("packages/ZPKG/global-class")).expect("export dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP.abap"),
            "DATA lo_dep TYPE REF TO zcl_dep.\n",
        )
        .expect("source");
        fs::write(
            workspace_path.join("src/reports/ZREP/abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("sidecar");
        let dependency_text = "\
CLASS zcl_dep DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
  METHOD run.
    DATA lv_value TYPE i.
    lv_value = 1.
  ENDMETHOD.
ENDCLASS.";
        let dependency_path = export_root.join("packages/ZPKG/global-class/ZCL_DEP.abap");
        fs::write(&dependency_path, dependency_text).expect("dependency source");

        let workspace_uri = normalize_lsp_uri(&path_to_file_uri(&workspace_path));
        let dependency_uri = path_to_file_uri(&dependency_path);
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        let before_revision = state
            .workspaces
            .get(&workspace_uri)
            .expect("workspace")
            .cache
            .last_analysis_revision();

        let opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&dependency_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: dependency_text.to_string(),
                },
            },
        );

        assert!(opened.is_dependency);
        assert_eq!(
            state
                .workspaces
                .get(&workspace_uri)
                .expect("workspace")
                .cache
                .last_analysis_revision(),
            before_revision
        );
        let local_use_offset = dependency_text
            .match_indices("lv_value")
            .nth(1)
            .map(|(offset, _)| offset + 1)
            .expect("local use");
        assert!(opened.definition_at(local_use_offset).is_none());

        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn opening_cached_dependency_file_keeps_dependency_surface_analysis() {
        let workspace_path = temp_workspace_path("workspace_open_dependency_surface_analysis");
        fs::create_dir_all(workspace_path.join("src")).expect("src dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("src/ZMAIN.abap"),
            "DATA lo_dep TYPE REF TO zcl_dep.\n",
        )
        .expect("source");
        let dependency_text = "\
CLASS zcl_dep DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
  METHOD run.
    DATA lv_value TYPE i.
    lv_value = 1.
    lv_value = lv_value + 1.
  ENDMETHOD.
ENDCLASS.";

        let workspace_uri = path_to_file_uri(&workspace_path);
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_DEP".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_dep".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: dependency_text.to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifact");

        let use_offset = dependency_text
            .match_indices("lv_value")
            .nth(2)
            .map(|(offset, _)| offset + 1)
            .expect("usage offset");
        let dependency_uri = dependency_uri_for_object_name(&state, &workspace_uri, "ZCL_DEP");

        let before = snapshot_for_uri(&state, &dependency_uri).expect("dependency snapshot");
        assert!(before.is_dependency);
        assert!(before.definition_at(use_offset).is_none());

        let dependency_text = dependency_text_for_uri(&state, &dependency_uri);
        let opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&dependency_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: dependency_text,
                },
            },
        );

        assert!(!opened.is_dependency);
        assert!(opened.definition_at(use_offset).is_some());

        refresh_workspace(&mut state, &workspace_uri);
        let refreshed =
            snapshot_for_uri(&state, &dependency_uri).expect("dependency snapshot after refresh");
        assert!(!refreshed.is_dependency);
        assert!(refreshed.definition_at(use_offset).is_some());

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn storing_remote_artifact_preserves_open_dependency_diagnostics() {
        let workspace_path = temp_workspace_path("workspace_open_dependency_store_preserves");
        fs::create_dir_all(workspace_path.join("src")).expect("src dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(workspace_path.join("src/ZMAIN.abap"), "REPORT zmain.").expect("source");
        let dependency_text = "\
CLASS zcl_dep DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
  METHOD run.
    DATA lv_missing TYPE zty_missing.
  ENDMETHOD.
ENDCLASS.";

        let workspace_uri = path_to_file_uri(&workspace_path);
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let artifact = DependencyArtifactPayload {
            package_name: "ZPKG".to_string(),
            object_kind: "global-class".to_string(),
            object_name: "ZCL_DEP".to_string(),
            object_uri: "/sap/bc/adt/oo/classes/zcl_dep".to_string(),
            object_type: "CLAS/OC".to_string(),
            description: "Remote class".to_string(),
            file_extension: "abap".to_string(),
            source_text: dependency_text.to_string(),
            fetched_at: "2026-04-23T00:00:00Z".to_string(),
        };
        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![artifact.clone()],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifact");

        let dependency_uri = dependency_uri_for_object_name(&state, &workspace_uri, "ZCL_DEP");
        let opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&dependency_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 7,
                    text: dependency_text.to_string(),
                },
            },
        );
        assert!(!opened.is_dependency);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    fetched_at: "2026-04-23T00:01:00Z".to_string(),
                    ..artifact
                }],
                negative: Vec::new(),
            },
        )
        .expect("refresh dependency artifact");

        let workspace = state
            .workspaces
            .get(&normalize_lsp_uri(&workspace_uri))
            .expect("workspace");
        let refreshed = snapshot_for_uri(&state, &dependency_uri).expect("refreshed dependency");
        assert!(!refreshed.is_dependency);
        assert_eq!(refreshed.version, 7);
        let diagnostics = build_lsp_diagnostics_for_workspace(Some(workspace), refreshed.as_ref());
        assert!(
            diagnostics
                .iter()
                .any(|diagnostic| diagnostic.message.contains("zty_missing")),
            "{diagnostics:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn opening_cached_dependency_include_resolves_at_group_headers_without_def_assign_warnings() {
        let workspace_path = temp_workspace_path("workspace_open_dependency_at_group_headers");
        let src_dir = workspace_path.join("src");
        fs::create_dir_all(&src_dir).expect("src dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "ZMAIN"
kind = "report"
root_file = "src/ZMAIN.abap"
"#,
        )
        .expect("manifest");
        fs::write(
            src_dir.join("ZMAIN.abap"),
            "\
REPORT zmain.
INCLUDE ztop.
INCLUDE zf02.",
        )
        .expect("main source");
        let top_text = "\
TYPES: BEGIN OF typ_output_data,
         src_plant TYPE i,
       END OF typ_output_data.
DATA t_final_data TYPE STANDARD TABLE OF typ_output_data WITH DEFAULT KEY.";
        let dependency_text = "\
FORM create_sto.
  FIELD-SYMBOLS <lfs_final_data> TYPE typ_output_data.
  LOOP AT t_final_data ASSIGNING <lfs_final_data>.
    AT NEW src_plant.
      WRITE <lfs_final_data>-src_plant.
    ENDAT.
    AT END OF src_plant.
      WRITE <lfs_final_data>-src_plant.
    ENDAT.
  ENDLOOP.
ENDFORM.";

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/src/ZMAIN.abap");
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![
                    DependencyArtifactPayload {
                        package_name: "ZPKG".to_string(),
                        object_kind: "include".to_string(),
                        object_name: "ZTOP".to_string(),
                        object_uri: "/sap/bc/adt/programs/includes/ztop".to_string(),
                        object_type: "PROG/I".to_string(),
                        description: "Top include".to_string(),
                        file_extension: "abap".to_string(),
                        source_text: top_text.to_string(),
                        fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    },
                    DependencyArtifactPayload {
                        package_name: "ZPKG".to_string(),
                        object_kind: "include".to_string(),
                        object_name: "ZF02".to_string(),
                        object_uri: "/sap/bc/adt/programs/includes/zf02".to_string(),
                        object_type: "PROG/I".to_string(),
                        description: "Form include".to_string(),
                        file_extension: "abap".to_string(),
                        source_text: dependency_text.to_string(),
                        fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    },
                ],
                negative: Vec::new(),
            },
        )
        .expect("store include artifacts");
        let _ = handle_remote_dependencies_updated(
            &mut state,
            &super::RemoteDependenciesUpdatedParams {
                workspace_uri: workspace_uri.clone(),
                source_uri: source_uri.clone(),
                source_uris: vec![source_uri],
                fetched: vec!["ZTOP".to_string(), "ZF02".to_string()],
                failed: Vec::new(),
            },
        );

        let dependency_uri = dependency_uri_for_object_name(&state, &workspace_uri, "ZF02");
        let before = snapshot_for_uri(&state, &dependency_uri).expect("dependency snapshot");
        assert!(before.is_dependency);

        let dependency_text = dependency_text_for_uri(&state, &dependency_uri);
        let opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&dependency_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: dependency_text,
                },
            },
        );

        assert!(!opened.is_dependency);
        assert!(
            opened.symbols.diagnostics.iter().all(|diag| {
                !matches!(
                    diag.kind,
                    DiagnosticKind::UseBeforeDefiniteAssignment
                        | DiagnosticKind::UnresolvedReference
                ) || !diag.message.contains("src_plant")
            }),
            "{:#?}",
            opened.symbols.diagnostics
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn opening_cached_dependency_include_does_not_flag_legacy_table_body_assignment_target() {
        let workspace_path = temp_workspace_path("workspace_open_dependency_legacy_table_body");
        let src_dir = workspace_path.join("src");
        fs::create_dir_all(&src_dir).expect("src dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "ZMAIN"
kind = "report"
root_file = "src/ZMAIN.abap"
"#,
        )
        .expect("manifest");
        fs::write(
            src_dir.join("ZMAIN.abap"),
            "\
REPORT zmain.
INCLUDE ztop.
INCLUDE zf02.",
        )
        .expect("main source");
        let top_text = "\
TYPES: BEGIN OF typ_output_row,
         src_plant TYPE i,
         dest_plant TYPE i,
       END OF typ_output_row.
TYPES typ_t_output_data TYPE STANDARD TABLE OF typ_output_row WITH DEFAULT KEY.";
        let dependency_text = "\
FORM f_sto_data USING ct_final_data TYPE typ_t_output_data.
  DATA lt_temp TYPE typ_t_output_data.

  IF ct_final_data IS NOT INITIAL.
    lt_temp[] = ct_final_data[].
    SORT lt_temp BY src_plant dest_plant.
    DELETE ADJACENT DUPLICATES FROM lt_temp COMPARING src_plant dest_plant.
  ENDIF.
ENDFORM.";

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/src/ZMAIN.abap");
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![
                    DependencyArtifactPayload {
                        package_name: "ZPKG".to_string(),
                        object_kind: "include".to_string(),
                        object_name: "ZTOP".to_string(),
                        object_uri: "/sap/bc/adt/programs/includes/ztop".to_string(),
                        object_type: "PROG/I".to_string(),
                        description: "Top include".to_string(),
                        file_extension: "abap".to_string(),
                        source_text: top_text.to_string(),
                        fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    },
                    DependencyArtifactPayload {
                        package_name: "ZPKG".to_string(),
                        object_kind: "include".to_string(),
                        object_name: "ZF02".to_string(),
                        object_uri: "/sap/bc/adt/programs/includes/zf02".to_string(),
                        object_type: "PROG/I".to_string(),
                        description: "Form include".to_string(),
                        file_extension: "abap".to_string(),
                        source_text: dependency_text.to_string(),
                        fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    },
                ],
                negative: Vec::new(),
            },
        )
        .expect("store include artifacts");
        let _ = handle_remote_dependencies_updated(
            &mut state,
            &super::RemoteDependenciesUpdatedParams {
                workspace_uri: workspace_uri.clone(),
                source_uri: source_uri.clone(),
                source_uris: vec![source_uri],
                fetched: vec!["ZTOP".to_string(), "ZF02".to_string()],
                failed: Vec::new(),
            },
        );

        let dependency_uri = dependency_uri_for_object_name(&state, &workspace_uri, "ZF02");
        let before = snapshot_for_uri(&state, &dependency_uri).expect("dependency snapshot");
        assert!(before.is_dependency);

        let dependency_text = dependency_text_for_uri(&state, &dependency_uri);
        let opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&dependency_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: dependency_text,
                },
            },
        );

        assert!(!opened.is_dependency);
        assert!(
            opened.symbols.diagnostics.iter().all(|diag| {
                !matches!(
                    diag.kind,
                    DiagnosticKind::UseBeforeDefiniteAssignment
                        | DiagnosticKind::IncompatibleAssignmentType
                ) || (!diag.message.contains("lt_temp")
                    && !diag.message.contains("typ_t_output_data"))
            }),
            "{:#?}",
            opened.symbols.diagnostics
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn completion_for_opened_include_field_symbol_does_not_leak_adjacent_structure_fields() {
        let workspace_path = temp_workspace_path("workspace_include_selector_completion");
        let src_dir = workspace_path.join("src");
        fs::create_dir_all(&src_dir).expect("src dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "ZMAIN"
kind = "report"
root_file = "src/ZMAIN.abap"
"#,
        )
        .expect("manifest");
        fs::write(
            src_dir.join("ZMAIN.abap"),
            "\
REPORT zmain.
INCLUDE ztop.
INCLUDE zf01.",
        )
        .expect("main source");
        let top_text = "\
TYPES: BEGIN OF typ_output_data,
         message_attp TYPE string,
         vbeln TYPE string,
       END OF typ_output_data,
       BEGIN OF typ_header,
         lgnum TYPE string,
         lgtyp TYPE string,
       END OF typ_header.
DATA t_final_data TYPE STANDARD TABLE OF typ_output_data WITH DEFAULT KEY.";
        let f01_text = "\
FORM update_message_final_tab.
  FIELD-SYMBOLS <lfs_final_data> TYPE typ_output_data.
  LOOP AT t_final_data ASSIGNING <lfs_final_data>.
    <lfs_final_data>-
  ENDLOOP.
ENDFORM.";

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/src/ZMAIN.abap");
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![
                    DependencyArtifactPayload {
                        package_name: "ZPKG".to_string(),
                        object_kind: "include".to_string(),
                        object_name: "ZTOP".to_string(),
                        object_uri: "/sap/bc/adt/programs/includes/ztop".to_string(),
                        object_type: "PROG/I".to_string(),
                        description: "Top include".to_string(),
                        file_extension: "abap".to_string(),
                        source_text: top_text.to_string(),
                        fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    },
                    DependencyArtifactPayload {
                        package_name: "ZPKG".to_string(),
                        object_kind: "include".to_string(),
                        object_name: "ZF01".to_string(),
                        object_uri: "/sap/bc/adt/programs/includes/zf01".to_string(),
                        object_type: "PROG/I".to_string(),
                        description: "Form include".to_string(),
                        file_extension: "abap".to_string(),
                        source_text: f01_text.to_string(),
                        fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    },
                ],
                negative: Vec::new(),
            },
        )
        .expect("store include artifacts");
        let _ = handle_remote_dependencies_updated(
            &mut state,
            &super::RemoteDependenciesUpdatedParams {
                workspace_uri: workspace_uri.clone(),
                source_uri: source_uri.clone(),
                source_uris: vec![source_uri],
                fetched: vec!["ZTOP".to_string(), "ZF01".to_string()],
                failed: Vec::new(),
            },
        );

        let f01_uri = dependency_uri_for_object_name(&state, &workspace_uri, "ZF01");

        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&f01_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: f01_text.to_string(),
                },
            },
        );

        let snapshot = snapshot_for_uri(&state, &f01_uri).expect("workspace snapshot");
        let offset =
            snapshot.text.find("<lfs_final_data>-").expect("selector") + "<lfs_final_data>-".len();
        let completion = snapshot.completion_at(offset).expect("completion");
        let labels: Vec<_> = completion
            .items
            .into_iter()
            .map(|item| match item {
                abap_cache::CompletionItem::Selector(item) => item.name.to_string(),
                abap_cache::CompletionItem::NamedArgument(item) => item.name.to_string(),
                abap_cache::CompletionItem::Symbol(item) => item.name.to_string(),
                abap_cache::CompletionItem::Template(item) => item.name.to_string(),
                abap_cache::CompletionItem::Callable(item) => item.name.to_string(),
                abap_cache::CompletionItem::Keyword(item) => item.name.to_string(),
            })
            .collect();

        assert!(
            labels.iter().any(|label| label == "message_attp"),
            "expected typ_output_data selector completion: {labels:?}"
        );
        assert!(
            !labels
                .iter()
                .any(|label| label == "lgnum" || label == "lgtyp"),
            "unexpected leaked selector completions: {labels:?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn preview_completion_for_local_include_field_symbol_does_not_leak_adjacent_structure_fields() {
        let workspace_path =
            temp_workspace_path("workspace_preview_local_include_selector_completion");
        let report_dir = workspace_path.join("src/reports/ZREP/forms");
        fs::create_dir_all(&report_dir).expect("report dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[resolution]
dependency_mode = "local-first"
cache_dir = "legacy-cache"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP.abap"),
            "\
REPORT zrep.
INCLUDE zrep_top.
INCLUDE zrep_f01.",
        )
        .expect("report");
        fs::write(
            workspace_path.join("src/reports/ZREP/abapls-unit.toml"),
            r#"
members = ["ZREP_TOP.abap", "forms/ZREP_F01.abap"]
includes = { "ZREP_TOP" = "ZREP_TOP.abap", "ZREP_F01" = "forms/ZREP_F01.abap" }
"#,
        )
        .expect("unit sidecar");
        fs::write(
            workspace_path.join("src/reports/ZREP/ZREP_TOP.abap"),
            "\
TYPES: BEGIN OF typ_output_data,
         message_attp TYPE string,
         vbeln TYPE string,
       END OF typ_output_data,
       BEGIN OF typ_header,
         lgnum TYPE string,
         lgtyp TYPE string,
       END OF typ_header.
DATA t_final_data TYPE STANDARD TABLE OF typ_output_data WITH DEFAULT KEY.",
        )
        .expect("top include");
        fs::write(
            workspace_path.join("src/reports/ZREP/forms/ZREP_F01.abap"),
            "\
FORM update_message_final_tab.
  FIELD-SYMBOLS <lfs_final_data> TYPE typ_output_data.
  LOOP AT t_final_data ASSIGNING <lfs_final_data>.
  ENDLOOP.
ENDFORM.",
        )
        .expect("f01");

        let preview_text = "\
FORM update_message_final_tab.
  FIELD-SYMBOLS <lfs_final_data> TYPE typ_output_data.
  LOOP AT t_final_data ASSIGNING <lfs_final_data>.
    <lfs_final_data>-
  ENDLOOP.
ENDFORM.";

        let workspace_uri = path_to_file_uri(&workspace_path);
        let f01_uri = format!("{workspace_uri}/src/reports/ZREP/forms/ZREP_F01.abap");
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        assert!(stage_workspace_preview_snapshot(
            &mut state,
            &f01_uri,
            2,
            preview_text
        ));

        let completion_offset = preview_text
            .find("<lfs_final_data>-")
            .expect("selector offset")
            + "<lfs_final_data>-".len();
        let position = offset_to_position(preview_text, completion_offset).expect("position");

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&f01_uri).expect("uri"),
                    },
                    position,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let labels: Vec<_> = items.into_iter().map(|item| item.label).collect();

        assert!(
            labels.iter().any(|label| label == "message_attp"),
            "expected typ_output_data preview selector completion: {labels:?}"
        );
        assert!(
            !labels
                .iter()
                .any(|label| label == "lgnum" || label == "lgtyp"),
            "unexpected leaked preview selector completions: {labels:?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn opening_changed_cached_workspace_file_does_not_reload_other_workspace_files_from_disk() {
        let workspace_path = temp_workspace_path("workspace_open_incremental");
        fs::create_dir_all(workspace_path.join("src")).expect("src dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[[unit]]
name = "ZCL_MAIN"
kind = "global-class"
root_file = "src/ZCL_MAIN.abap"

[[unit]]
name = "ZCL_HELPER"
kind = "global-class"
root_file = "src/ZCL_HELPER.abap"
"#,
        )
        .expect("manifest");
        let main_text = "CLASS zcl_main DEFINITION. ENDCLASS.";
        let helper_text = "CLASS zcl_helper DEFINITION. ENDCLASS.";
        fs::write(workspace_path.join("src/ZCL_MAIN.abap"), main_text).expect("main source");
        fs::write(workspace_path.join("src/ZCL_HELPER.abap"), helper_text).expect("helper source");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let main_uri = format!("{workspace_uri}/src/ZCL_MAIN.abap");
        let helper_uri = format!("{workspace_uri}/src/ZCL_HELPER.abap");
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        fs::write(
            workspace_path.join("src/ZCL_HELPER.abap"),
            "CLASS zcl_helper DEFINITION PUBLIC. ENDCLASS.",
        )
        .expect("mutated helper source");

        let opened_text = "CLASS zcl_main DEFINITION PUBLIC. ENDCLASS.";
        let opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&main_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 3,
                    text: opened_text.to_string(),
                },
            },
        );

        assert_eq!(opened.text.as_ref(), opened_text);
        let helper = snapshot_for_uri(&state, &normalize_lsp_uri(&helper_uri)).expect("helper");
        assert_eq!(helper.text.as_ref(), helper_text);

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn opening_manifest_declared_workspace_file_missing_at_refresh_stays_incremental() {
        let workspace_path = temp_workspace_path("workspace_open_declared_incremental");
        fs::create_dir_all(workspace_path.join("src")).expect("src dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[[unit]]
name = "ZCL_MAIN"
kind = "global-class"
root_file = "src/ZCL_MAIN.abap"

[[unit]]
name = "ZCL_HELPER"
kind = "global-class"
root_file = "src/ZCL_HELPER.abap"
"#,
        )
        .expect("manifest");
        let main_text = "CLASS zcl_main DEFINITION. ENDCLASS.";
        fs::write(workspace_path.join("src/ZCL_MAIN.abap"), main_text).expect("main source");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let main_uri = format!("{workspace_uri}/src/ZCL_MAIN.abap");
        let helper_uri = format!("{workspace_uri}/src/ZCL_HELPER.abap");
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        assert!(
            snapshot_for_uri(&state, &normalize_lsp_uri(&helper_uri)).is_none(),
            "helper should be uncached before it is opened"
        );

        fs::write(
            workspace_path.join("src/ZCL_MAIN.abap"),
            "CLASS zcl_main DEFINITION PUBLIC. ENDCLASS.",
        )
        .expect("mutated main source");

        let helper_text = "CLASS zcl_helper DEFINITION. ENDCLASS.";
        let opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&helper_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: helper_text.to_string(),
                },
            },
        );

        assert_eq!(opened.text.as_ref(), helper_text);
        assert_eq!(opened.object_name.as_deref(), Some("zcl_helper"));
        let main = snapshot_for_uri(&state, &normalize_lsp_uri(&main_uri)).expect("main");
        assert_eq!(main.text.as_ref(), main_text);

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn opening_new_workspace_file_without_manifest_stays_incremental() {
        let workspace_path = temp_workspace_path("workspace_open_no_manifest_incremental");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        let main_text = "CLASS zcl_main DEFINITION. ENDCLASS.";
        fs::write(workspace_path.join("main.abap"), main_text).expect("main source");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let main_uri = format!("{workspace_uri}/main.abap");
        let new_uri = format!("{workspace_uri}/new_file.abap");
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        fs::write(
            workspace_path.join("main.abap"),
            "CLASS zcl_main DEFINITION PUBLIC. ENDCLASS.",
        )
        .expect("mutated main source");

        let new_text = "CLASS zcl_new_file DEFINITION. ENDCLASS.";
        let opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&new_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: new_text.to_string(),
                },
            },
        );

        assert_eq!(opened.text.as_ref(), new_text);
        let main = snapshot_for_uri(&state, &normalize_lsp_uri(&main_uri)).expect("main");
        assert_eq!(main.text.as_ref(), main_text);

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn changing_cached_workspace_file_does_not_reload_other_workspace_files_from_disk() {
        let workspace_path = temp_workspace_path("workspace_change_incremental");
        fs::create_dir_all(workspace_path.join("src")).expect("src dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[[unit]]
name = "ZCL_MAIN"
kind = "global-class"
root_file = "src/ZCL_MAIN.abap"

[[unit]]
name = "ZCL_HELPER"
kind = "global-class"
root_file = "src/ZCL_HELPER.abap"
"#,
        )
        .expect("manifest");
        let main_text = "CLASS zcl_main DEFINITION. ENDCLASS.";
        let helper_text = "CLASS zcl_helper DEFINITION. ENDCLASS.";
        fs::write(workspace_path.join("src/ZCL_MAIN.abap"), main_text).expect("main source");
        fs::write(workspace_path.join("src/ZCL_HELPER.abap"), helper_text).expect("helper source");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let main_uri = format!("{workspace_uri}/src/ZCL_MAIN.abap");
        let helper_uri = format!("{workspace_uri}/src/ZCL_HELPER.abap");
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        fs::write(
            workspace_path.join("src/ZCL_HELPER.abap"),
            "CLASS zcl_helper DEFINITION PUBLIC. ENDCLASS.",
        )
        .expect("mutated helper source");

        let changed_text = "CLASS zcl_main DEFINITION PUBLIC. ENDCLASS.";
        let changed = publish_changed_document_mut(
            &mut state,
            &DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: Uri::from_str(&main_uri).expect("uri"),
                    version: 2,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: changed_text.to_string(),
                }],
            },
        )
        .expect("changed snapshot");

        assert_eq!(changed.text.as_ref(), changed_text);
        let helper = snapshot_for_uri(&state, &normalize_lsp_uri(&helper_uri)).expect("helper");
        assert_eq!(helper.text.as_ref(), helper_text);

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn dependency_cache_refresh_reissues_requests() {
        let workspace_path = temp_workspace_path("dependency_cache_refresh");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::create_dir_all(workspace_path.join("src")).expect("src dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "ZMAIN"
kind = "report"
root_file = "src/ZMAIN.abap"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);
        fs::write(
            workspace_path.join("src/ZMAIN.abap"),
            "DATA lo_demo TYPE REF TO zcl_remote_demo.\nlo_demo = zcl_remote_demo=>create( ).",
        )
        .expect("source");

        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "SABAPDEMOS".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_REMOTE_DEMO".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_remote_demo".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "\
CLASS zcl_remote_demo DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS create RETURNING VALUE(ro_demo) TYPE REF TO zcl_remote_demo.
ENDCLASS.

CLASS zcl_remote_demo IMPLEMENTATION.
  METHOD create.
  ENDMETHOD.
ENDCLASS.
"
                    .to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifacts");

        assert!(
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/src/ZMAIN.abap"))
                .is_none()
        );

        let snapshots = handle_dependency_cache_refresh_requested(
            &mut state,
            &WorkspaceManifestUpdatedParams {
                workspace_uri: workspace_uri.clone(),
            },
        );
        assert!(
            !snapshots.is_empty(),
            "expected workspace refresh after dependency cache refresh"
        );

        let request = build_remote_dependency_refresh_for_workspace(&mut state, &workspace_uri)
            .expect("remote refresh request");
        assert!(request.retry_negative_candidates);
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.name == "zcl_remote_demo")
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn cached_dependency_static_struct_constant_reports_bad_member_and_component() {
        let workspace_path = temp_workspace_path("dependency_static_struct_constant");
        fs::create_dir_all(workspace_path.join("src")).expect("src dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "ZMAIN"
kind = "report"
root_file = "src/ZMAIN.abap"
"#,
        )
        .expect("manifest");
        let main_src = "\
DATA p_legisl TYPE string.
IF p_legisl EQ zattp_cl_rep_constants=>gcs_legislationx-cn.
ENDIF.
IF p_legisl EQ zattp_cl_rep_constants=>gcs_legislation-bogus.
ENDIF.
";
        fs::write(workspace_path.join("src/ZMAIN.abap"), main_src).expect("source");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/ZMAIN.abap"));
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZATTP_CL_REP_CONSTANTS".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zattp_cl_rep_constants".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote constants".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "\
CLASS zattp_cl_rep_constants DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gcs_legislation,
        cn TYPE string VALUE 'CN',
      END OF gcs_legislation.
ENDCLASS.

CLASS zattp_cl_rep_constants IMPLEMENTATION.
ENDCLASS.
"
                    .to_string(),
                    fetched_at: "2026-04-30T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifact");
        refresh_workspace(&mut state, &workspace_uri);

        let snapshot = snapshot_for_uri(&state, &source_uri).expect("source snapshot");
        assert!(
            snapshot.symbols.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnknownField
                    && diag.message.contains("gcs_legislationx")
            }),
            "{:#?}",
            snapshot.symbols.diagnostics
        );
        assert!(
            snapshot.symbols.diagnostics.iter().any(|diag| {
                diag.kind == DiagnosticKind::UnknownField && diag.message.contains("bogus")
            }),
            "{:#?}",
            snapshot.symbols.diagnostics
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn cached_dependency_file_suppresses_remote_request_even_if_still_unresolved() {
        let workspace_path = temp_workspace_path("cached_dependency_short_circuit");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::create_dir_all(workspace_path.join("src")).expect("src dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "ZMAIN"
kind = "report"
root_file = "src/ZMAIN.abap"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);
        fs::write(
            workspace_path.join("src/ZMAIN.abap"),
            "DATA lo_demo TYPE REF TO zcl_remote_demo.\nlo_demo = zcl_remote_demo=>create( ).",
        )
        .expect("source");

        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "SABAPDEMOS".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_REMOTE_DEMO".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_remote_demo".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "CLASS zcl_remote_demo DEFINITION.\n".to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifacts");

        assert!(
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/src/ZMAIN.abap"))
                .is_none(),
            "stored dependency artifact should suppress remote request notification"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn cached_dependency_file_suppresses_symbol_remote_request() {
        let workspace_path = temp_workspace_path("cached_symbol_dependency_short_circuit");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::create_dir_all(workspace_path.join("src")).expect("src dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "ZMAIN"
kind = "report"
root_file = "src/ZMAIN.abap"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);
        fs::write(
            workspace_path.join("src/ZMAIN.abap"),
            "zcl_remote_demo = 1.",
        )
        .expect("source");

        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "SABAPDEMOS".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_REMOTE_DEMO".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_remote_demo".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "CLASS zcl_remote_demo DEFINITION.\n".to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifacts");

        let store = DocumentStore::default();
        let candidates = collect_remote_dependency_candidates(
            store
                .publish("file:///symbol_candidate.abap", 1, "zcl_remote_demo = 1.")
                .as_ref(),
        );
        assert!(
            candidates
                .iter()
                .any(|candidate| candidate.name == "zcl_remote_demo" && candidate.kind == "symbol"),
            "{candidates:#?}"
        );

        assert!(
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/src/ZMAIN.abap"))
                .is_none(),
            "stored dependency artifact should suppress symbol-based remote request notification"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn remote_dependency_request_includes_standard_type_candidates() {
        let workspace_path = temp_workspace_path("standard_type_remote_candidates");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/main.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: concat!(
                        "DATA lv_boolean TYPE boolean.\n",
                        "DATA lv_any TYPE xsdany.\n",
                        "DATA lv_guid TYPE sxmsmguid.\n",
                        "DATA lo_reader TYPE REF TO if_sxml_reader.\n",
                        "DATA lo_node TYPE REF TO if_sxml_node.\n",
                        "DATA lx_root TYPE REF TO cx_root.\n",
                        "TRY.\n",
                        "  WRITE 'x'.\n",
                        "CATCH cx_sxml_parse_error INTO DATA(lx_parse).\n",
                        "ENDTRY.\n",
                    )
                    .to_string(),
                },
            },
        );

        let request =
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/main.abap"))
                .expect("remote request");
        let names: std::collections::HashSet<_> = request
            .candidates
            .iter()
            .map(|candidate| candidate.name.as_str())
            .collect();

        assert!(names.contains("boolean"));
        assert!(names.contains("xsdany"));
        assert!(names.contains("sxmsmguid"));
        assert!(names.contains("if_sxml_reader"));
        assert!(names.contains("if_sxml_node"));
        assert!(names.contains("cx_root"));
        assert!(names.contains("cx_sxml_parse_error"));

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn remote_dependency_request_includes_local_style_type_candidates_after_local_resolution() {
        let workspace_path = temp_workspace_path("local_style_type_remote_candidates");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/main.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "DATA lt_ltap TYPE tt_ltap_vb.".to_string(),
                },
            },
        );

        let request =
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/main.abap"))
                .expect("remote request");
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.kind == "type" && candidate.name == "tt_ltap_vb"),
            "{request:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn remote_dependency_request_includes_local_style_function_candidates_after_local_resolution() {
        let workspace_path = temp_workspace_path("local_style_function_remote_candidates");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/main.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: concat!(
                        "CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'.\n",
                        "CALL FUNCTION 'SD_ROUTE_DETERMINATION'.\n",
                        "CALL FUNCTION 'WS_DELIVERY_UPDATE_2'.\n",
                    )
                    .to_string(),
                },
            },
        );

        let request =
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/main.abap"))
                .expect("remote request");
        let candidates: std::collections::HashSet<_> = request
            .candidates
            .iter()
            .filter(|candidate| candidate.kind == "function")
            .map(|candidate| candidate.name.as_str())
            .collect();
        assert!(
            candidates.contains("md_convert_material_unit"),
            "{request:#?}"
        );
        assert!(
            candidates.contains("sd_route_determination"),
            "{request:#?}"
        );
        assert!(candidates.contains("ws_delivery_update_2"), "{request:#?}");

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn remote_dependency_request_includes_submit_report_candidates() {
        let workspace_path = temp_workspace_path("submit_report_remote_candidates");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/main.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "SUBMIT rsnast00.\nSUBMIT rsnast0d AND RETURN.".to_string(),
                },
            },
        );

        let request =
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/main.abap"))
                .expect("remote request");
        let candidates: std::collections::HashSet<_> = request
            .candidates
            .iter()
            .filter(|candidate| candidate.kind == "report")
            .map(|candidate| candidate.name.as_str())
            .collect();
        assert!(candidates.contains("rsnast00"), "{request:#?}");
        assert!(candidates.contains("rsnast0d"), "{request:#?}");

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn remote_dependency_request_includes_open_sql_source_candidates() {
        let workspace_path = temp_workspace_path("open_sql_remote_candidates");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/main.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "SELECT * FROM ekko INTO TABLE @DATA(lt_ekko).".to_string(),
                },
            },
        );

        let request =
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/main.abap"))
                .expect("remote request");
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.kind == "type" && candidate.name == "ekko"),
            "{request:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn workspace_remote_dependency_batch_is_single_wave_and_blocks_while_in_flight() {
        let workspace_path = temp_workspace_path("workspace_remote_batch");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/first.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "DATA lo_first TYPE REF TO zcl_first.\nlo_first = zcl_first=>create( )."
                        .to_string(),
                },
            },
        );
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/second.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text:
                        "DATA lo_second TYPE REF TO zcl_second.\nlo_second = zcl_second=>create( )."
                            .to_string(),
                },
            },
        );

        let batch = build_remote_dependency_batch_for_workspace(&mut state, &workspace_uri)
            .expect("workspace batch");
        assert_eq!(batch.candidates.len(), 2, "{batch:#?}");
        assert_eq!(batch.source_uris.len(), 2, "{batch:#?}");
        assert!(
            build_remote_dependency_batch_for_workspace(&mut state, &workspace_uri).is_none(),
            "second batch should be suppressed while first wave is in flight"
        );

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_FIRST".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_first".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text:
                        "CLASS zcl_first DEFINITION.\nENDCLASS.\nCLASS zcl_first IMPLEMENTATION.\nENDCLASS.\n"
                            .to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifact");

        let _ = handle_remote_dependencies_updated(
            &mut state,
            &super::RemoteDependenciesUpdatedParams {
                workspace_uri: workspace_uri.clone(),
                source_uri: batch.source_uri.clone(),
                source_uris: batch.source_uris,
                fetched: vec!["ZCL_FIRST".to_string()],
                failed: vec![super::RemoteDependencyCandidate {
                    name: "zcl_second".to_string(),
                    kind: "static".to_string(),
                }],
            },
        );
        assert!(
            build_remote_dependency_batch_for_workspace(&mut state, &workspace_uri).is_none(),
            "no immediate reissue expected after fetched/failed results were recorded"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn workspace_remote_dependency_batch_dedupes_candidates_across_files() {
        let workspace_path = temp_workspace_path("workspace_remote_batch_dedupe");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        for file_name in ["first.abap", "second.abap"] {
            publish_open_document_mut(
                &mut state,
                &DidOpenTextDocumentParams {
                    text_document: TextDocumentItem {
                        uri: Uri::from_str(&format!("{workspace_uri}/{file_name}")).expect("uri"),
                        language_id: "abap".to_string(),
                        version: 1,
                        text:
                            "DATA lo_demo TYPE REF TO zcl_first.\nlo_demo = zcl_first=>create( )."
                                .to_string(),
                    },
                },
            );
        }

        let batch = build_remote_dependency_batch_for_workspace(&mut state, &workspace_uri)
            .expect("workspace batch");
        assert_eq!(batch.candidates.len(), 1, "{batch:#?}");
        assert_eq!(batch.candidates[0].name, "zcl_first");
        assert_eq!(batch.source_uris.len(), 1, "{batch:#?}");

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn workspace_remote_dependency_batch_prioritizes_open_and_src_sources_before_dependencies() {
        let workspace_path = temp_workspace_path("workspace_remote_batch_src_priority");
        let source_dir = workspace_path.join("src");
        let dependency_dir = workspace_path
            .join("legacy-cache-root")
            .join("cache")
            .join("dependencies")
            .join("global-class");
        fs::create_dir_all(&source_dir).expect("source dir");
        fs::create_dir_all(&dependency_dir).expect("dependency dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "ZMAIN"
kind = "report"
root_file = "src/ZMAIN.abap"

[[unit.member]]
role = "root"
file = "src/ZMAIN.abap"
object_name = "ZMAIN"

[[unit]]
name = "ZCL_DEP"
kind = "global-class"
root_file = "legacy-cache/dependencies/global-class/ZCL_DEP.abap"

[[unit.member]]
role = "dependency"
file = "legacy-cache/dependencies/global-class/ZCL_DEP.abap"
object_name = "ZCL_DEP"
"#,
        )
        .expect("manifest");
        fs::write(
            source_dir.join("ZMAIN.abap"),
            "REPORT zmain.\nDATA lo_demo TYPE REF TO zcl_first.\nlo_demo = zcl_first=>create( ).\n",
        )
        .expect("main");
        fs::write(
            dependency_dir.join("ZCL_DEP.abap"),
            "CLASS zcl_dep DEFINITION.\n  PUBLIC SECTION.\n    DATA ms_bal TYPE bal_s_msg.\nENDCLASS.\nCLASS zcl_dep IMPLEMENTATION.\nENDCLASS.\n",
        )
        .expect("dependency");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let batch = build_remote_dependency_batch_for_workspace(&mut state, &workspace_uri)
            .expect("workspace batch");
        assert_eq!(batch.source_uris.len(), 1, "{batch:#?}");
        assert!(
            batch.source_uris[0].ends_with("/src/ZMAIN.abap"),
            "{batch:#?}"
        );
        assert!(
            batch
                .candidates
                .iter()
                .any(|candidate| candidate.name == "zcl_first"),
            "{batch:#?}"
        );
        assert!(
            batch
                .candidates
                .iter()
                .all(|candidate| candidate.name != "bal_s_msg"),
            "{batch:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn workspace_remote_dependency_batch_requests_all_priority_candidates_without_tiny_cap() {
        let workspace_path = temp_workspace_path("workspace_remote_batch_chunking");
        let source_dir = workspace_path.join("src");
        let candidate_count = 27;
        fs::create_dir_all(&source_dir).expect("source dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "ZMAIN"
kind = "report"
root_file = "src/ZMAIN.abap"

[[unit.member]]
role = "root"
file = "src/ZMAIN.abap"
object_name = "ZMAIN"
"#,
        )
        .expect("manifest");
        let source = (0..candidate_count)
            .map(|index| {
                format!(
                    "DATA lo_{index} TYPE REF TO zcl_remote_{index:02}.\nlo_{index} = zcl_remote_{index:02}=>create( )."
                )
            })
            .collect::<Vec<_>>()
            .join("\n");
        fs::write(source_dir.join("ZMAIN.abap"), source).expect("main");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let first = build_remote_dependency_batch_for_workspace(&mut state, &workspace_uri)
            .expect("first batch");
        assert_eq!(first.candidates.len(), candidate_count, "{first:#?}");

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn dependency_batches_retain_local_export_context_from_ancestor_sources() {
        let workspace_path = temp_workspace_path("workspace_remote_dependency_context");
        let source_dir = workspace_path.join("src");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(&source_dir).expect("source dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "ZMAIN"
kind = "report"
root_file = "src/ZMAIN.abap"
"#,
        )
        .expect("manifest");
        fs::write(
            source_dir.join("ZMAIN.abap"),
            "REPORT zmain.\nWRITE 'ok'.\n",
        )
        .expect("source");
        fs::write(
            source_dir.join("ZMAIN.abap.abapls-unit.toml"),
            "[local_export]\nroots = [\"D:/dev/abap/d65\"]\n\n[dependencies]\nsource = \"local-first\"\n",
        )
        .expect("sidecar");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/ZMAIN.abap"));

        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_DEP".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_dep".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "CLASS zcl_dep DEFINITION.\n  PUBLIC SECTION.\n    DATA ms_bal TYPE bal_s_msg.\nENDCLASS.\nCLASS zcl_dep IMPLEMENTATION.\nENDCLASS.\n".to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: vec![RemoteDependencyCandidate {
                    name: "bal_s_msg".to_string(),
                    kind: "type".to_string(),
                }],
            },
        )
        .expect("store dependency artifact");
        let _ = handle_remote_dependencies_updated(
            &mut state,
            &super::RemoteDependenciesUpdatedParams {
                workspace_uri: workspace_uri.clone(),
                source_uri: source_uri.clone(),
                source_uris: vec![source_uri.clone()],
                fetched: vec!["ZCL_DEP".to_string()],
                failed: Vec::new(),
            },
        );
        let dependency_uri = dependency_uri_for_object_name(&state, &workspace_uri, "ZCL_DEP");

        let batch = build_remote_dependency_batch_for_workspace(&mut state, &workspace_uri)
            .expect("dependency batch");
        assert!(batch.source_uris.contains(&dependency_uri), "{batch:#?}");
        assert!(batch.source_uris.contains(&source_uri), "{batch:#?}");
        assert!(
            batch
                .candidates
                .iter()
                .any(|candidate| candidate.name == "bal_s_msg"),
            "{batch:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn remote_dependency_updates_clear_seen_candidates_for_same_session_retry() {
        let workspace_path = temp_workspace_path("workspace_remote_retry_after_update");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/main.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "DATA lo_demo TYPE REF TO zcl_retry.\nlo_demo = zcl_retry=>create( )."
                        .to_string(),
                },
            },
        );

        let first = build_remote_dependency_batch_for_workspace(&mut state, &workspace_uri)
            .expect("first batch");
        assert!(
            first
                .candidates
                .iter()
                .any(|candidate| candidate.name == "zcl_retry"),
            "{first:#?}"
        );

        let _ = handle_remote_dependencies_updated(
            &mut state,
            &super::RemoteDependenciesUpdatedParams {
                workspace_uri: workspace_uri.clone(),
                source_uri: first.source_uri.clone(),
                source_uris: first.source_uris,
                fetched: Vec::new(),
                failed: Vec::new(),
            },
        );

        let second = build_remote_dependency_batch_for_workspace(&mut state, &workspace_uri)
            .expect("second batch");
        assert!(
            second
                .candidates
                .iter()
                .any(|candidate| candidate.name == "zcl_retry"),
            "{second:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn remote_dependency_request_still_emits_candidates_with_persisted_negative_markers_for_local_exports()
     {
        let workspace_path = temp_workspace_path("workspace_negative_dependency_marker");
        let export_root = temp_workspace_path("workspace_negative_dependency_marker_export");
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::create_dir_all(&export_root).expect("export root");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);
        fs::write(
            workspace_path.join("main.abap.abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("sidecar");

        let negative_path = workspace_path
            .join("legacy-cache-root")
            .join("cache")
            .join("negative-dependencies")
            .join("type")
            .join("BOOLEAN.json");
        fs::create_dir_all(negative_path.parent().expect("negative marker dir"))
            .expect("negative marker dir");
        fs::write(
            &negative_path,
            r#"{"name":"boolean","kind":"type","reason":"exact-match-domain-only"}"#,
        )
        .expect("negative marker");

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/main.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "DATA lv_boolean TYPE boolean.".to_string(),
                },
            },
        );

        let request =
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/main.abap"))
                .expect("remote request");
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.kind == "type" && candidate.name == "boolean"),
            "persisted negative markers must not suppress client-side local-first resolution: {request:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn workspace_remote_dependency_batch_skips_persisted_negative_markers_without_local_exports() {
        let workspace_path = temp_workspace_path("workspace_negative_dependency_batch");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/main.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "DATA lv_boolean TYPE boolean.".to_string(),
                },
            },
        );

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: None,
                artifacts: Vec::new(),
                negative: vec![RemoteDependencyCandidate {
                    name: "boolean".to_string(),
                    kind: "type".to_string(),
                }],
            },
        )
        .expect("store negative lookup");

        assert!(
            build_remote_dependency_batch_for_workspace(&mut state, &workspace_uri).is_none(),
            "persisted negative lookups should suppress repeated remote batches when no local export roots are configured"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn refreshed_dependency_files_can_trigger_follow_up_remote_requests() {
        let workspace_path = temp_workspace_path("dependency_of_dependency");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/main.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "DATA lo_demo TYPE REF TO zcl_first.\nlo_demo = zcl_first=>create( )."
                        .to_string(),
                },
            },
        );

        let initial =
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/main.abap"))
                .expect("initial request");
        assert!(
            initial
                .candidates
                .iter()
                .any(|candidate| candidate.name == "zcl_first")
        );

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_FIRST".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_first".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "CLASS zcl_first DEFINITION.\n  PUBLIC SECTION.\n    CLASS-METHODS create RETURNING VALUE(ro_inst) TYPE REF TO zcl_second.\nENDCLASS.\nCLASS zcl_first IMPLEMENTATION.\n  METHOD create.\n  ENDMETHOD.\nENDCLASS.\n".to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifact");

        let _ = handle_remote_dependencies_updated(
            &mut state,
            &super::RemoteDependenciesUpdatedParams {
                workspace_uri: workspace_uri.clone(),
                source_uri: format!("{workspace_uri}/main.abap"),
                source_uris: Vec::new(),
                fetched: vec!["ZCL_FIRST".to_string()],
                failed: Vec::new(),
            },
        );

        let dependency_uri = dependency_uri_for_object_name(&state, &workspace_uri, "ZCL_FIRST");
        let follow_up = build_remote_dependency_requests_for_workspace(&mut state, &workspace_uri);
        assert!(follow_up.iter().any(|request| {
            request.source_uri == dependency_uri
                && request
                    .candidates
                    .iter()
                    .any(|candidate| candidate.name == "zcl_second")
        }));

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn remote_dependency_updates_refresh_new_sidecar_without_full_workspace_fanout() {
        let workspace_path = temp_workspace_path("dependency_sidecar_targeted_refresh");
        let source_dir = workspace_path.join("src");
        fs::create_dir_all(&source_dir).expect("source dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "ZREPORT_MAIN"
kind = "report"
root_file = "src/ZREPORT_MAIN.abap"

[[unit.member]]
role = "root"
file = "src/ZREPORT_MAIN.abap"
object_name = "ZREPORT_MAIN"

[[unit]]
name = "ZREPORT_OTHER"
kind = "report"
root_file = "src/ZREPORT_OTHER.abap"

[[unit.member]]
role = "root"
file = "src/ZREPORT_OTHER.abap"
object_name = "ZREPORT_OTHER"
"#,
        )
        .expect("manifest");
        fs::write(
            source_dir.join("ZREPORT_MAIN.abap"),
            "REPORT zreport_main.\nDATA lo_dep TYPE REF TO zcl_dep.\n",
        )
        .expect("main");
        fs::write(
            source_dir.join("ZREPORT_OTHER.abap"),
            "REPORT zreport_other.\nWRITE 'ok'.\n",
        )
        .expect("other");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/ZREPORT_MAIN.abap"));
        let unrelated_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/ZREPORT_OTHER.abap"));

        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_DEP".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_dep".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "\
CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS create RETURNING VALUE(ro_dep) TYPE REF TO object.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
  METHOD create.
  ENDMETHOD.
ENDCLASS."
                        .to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifact");
        let dependency_uri = dependency_uri_for_object_name(&state, &workspace_uri, "ZCL_DEP");

        let refreshed = handle_remote_dependencies_updated(
            &mut state,
            &super::RemoteDependenciesUpdatedParams {
                workspace_uri: workspace_uri.clone(),
                source_uri: source_uri.clone(),
                source_uris: vec![source_uri.clone()],
                fetched: vec!["ZCL_DEP".to_string()],
                failed: Vec::new(),
            },
        );

        let refreshed_uris: Vec<_> = refreshed
            .iter()
            .map(|snapshot| snapshot.uri.as_ref())
            .collect();
        assert_eq!(refreshed_uris.len(), 2, "{refreshed_uris:?}");
        assert!(
            refreshed_uris.contains(&source_uri.as_str()),
            "{refreshed_uris:?}"
        );
        assert!(
            refreshed_uris.contains(&dependency_uri.as_str()),
            "{refreshed_uris:?}"
        );
        assert!(
            !refreshed_uris.contains(&unrelated_uri.as_str()),
            "{refreshed_uris:?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn workspace_dependency_refresh_batches_closed_dependency_transitive_references() {
        let workspace_path = temp_workspace_path("dependency_impl_batch_refresh");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/main.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "DATA lo_demo TYPE REF TO zcl_first.\nlo_demo = zcl_first=>create( )."
                        .to_string(),
                },
            },
        );

        let initial =
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/main.abap"))
                .expect("initial request");
        assert!(
            initial
                .candidates
                .iter()
                .any(|candidate| candidate.name == "zcl_first")
        );

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_FIRST".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_first".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "CLASS zcl_first DEFINITION.\n  PUBLIC SECTION.\n    CLASS-METHODS create RETURNING VALUE(ro_inst) TYPE REF TO zcl_first.\nENDCLASS.\nCLASS zcl_first IMPLEMENTATION.\n  METHOD create.\n    SELECT SINGLE * FROM /aif/t_finf INTO @DATA(ls_finf).\n  ENDMETHOD.\nENDCLASS.\n".to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifact");

        let _ = handle_remote_dependencies_updated(
            &mut state,
            &super::RemoteDependenciesUpdatedParams {
                workspace_uri: workspace_uri.clone(),
                source_uri: format!("{workspace_uri}/main.abap"),
                source_uris: Vec::new(),
                fetched: vec!["ZCL_FIRST".to_string()],
                failed: Vec::new(),
            },
        );

        let batch = build_remote_dependency_batch_for_workspace(&mut state, &workspace_uri)
            .expect("follow-up batch");
        let dependency_uri = dependency_uri_for_object_name(&state, &workspace_uri, "ZCL_FIRST");
        assert!(batch.source_uri == dependency_uri, "{batch:#?}");
        assert!(
            batch
                .candidates
                .iter()
                .any(|candidate| candidate.kind == "type" && candidate.name == "/aif/t_finf"),
            "{batch:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn workspace_remote_dependency_batch_skips_unparented_closed_dependency_sources() {
        let workspace_path = temp_workspace_path("unparented_closed_dependency_batch");
        let export_root = temp_workspace_path("unparented_closed_dependency_batch_export");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::create_dir_all(&export_root).expect("export root");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(workspace_path.join("main.abap"), "REPORT zmain.\n").expect("main");
        fs::write(
            workspace_path.join("main.abap.abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("sidecar");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_UNPARENTED".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_unparented".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Unparented cached class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "CLASS zcl_unparented DEFINITION.\n  PUBLIC SECTION.\n    METHODS missing.\nENDCLASS.\nCLASS zcl_unparented IMPLEMENTATION.\n  METHOD missing.\n    DATA lo_inner TYPE REF TO zcl_inner.\n  ENDMETHOD.\nENDCLASS.\n".to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifact");

        assert!(
            build_remote_dependency_batch_for_workspace(&mut state, &workspace_uri).is_none(),
            "unparented closed cached dependencies should not fan out background remote waves"
        );

        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn dependency_batch_cache_reuses_closed_dependency_candidate_projection() {
        let workspace_path = temp_workspace_path("dependency_batch_candidate_cache");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/main.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "DATA lo_demo TYPE REF TO zcl_first.\nlo_demo = zcl_first=>create( )."
                        .to_string(),
                },
            },
        );

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_FIRST".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_first".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "CLASS zcl_first DEFINITION.\n  PUBLIC SECTION.\n    CLASS-METHODS create RETURNING VALUE(ro_inst) TYPE REF TO zcl_first.\nENDCLASS.\nCLASS zcl_first IMPLEMENTATION.\n  METHOD create.\n    DATA lo_dep TYPE REF TO zcl_second.\n  ENDMETHOD.\nENDCLASS.\n".to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifact");
        let _ = handle_remote_dependencies_updated(
            &mut state,
            &super::RemoteDependenciesUpdatedParams {
                workspace_uri: workspace_uri.clone(),
                source_uri: format!("{workspace_uri}/main.abap"),
                source_uris: Vec::new(),
                fetched: vec!["ZCL_FIRST".to_string()],
                failed: Vec::new(),
            },
        );

        let first = build_remote_dependency_batch_for_workspace(&mut state, &workspace_uri)
            .expect("first follow-up batch");
        assert!(
            first
                .candidates
                .iter()
                .any(|candidate| candidate.name == "zcl_second"),
            "{first:#?}"
        );

        let workspace = state
            .workspaces
            .get_mut(&normalize_lsp_uri(&workspace_uri))
            .expect("workspace");
        workspace.remote_resolution_in_flight = false;
        workspace.remote_resolution_seen.clear();

        let second = build_remote_dependency_batch_for_workspace(&mut state, &workspace_uri)
            .expect("second follow-up batch");
        assert!(
            second
                .candidates
                .iter()
                .any(|candidate| candidate.name == "zcl_second"),
            "{second:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn hydration_clears_local_export_chain_candidate_cache() {
        let mut workspace = WorkspaceState::new("file:///workspace");
        workspace.local_export_chain_candidates.insert(
            "stale".to_string(),
            vec![RemoteDependencyCandidate {
                name: "zcached".to_string(),
                kind: "type".to_string(),
            }],
        );
        workspace
            .local_export_chain_refresh_candidates
            .insert("stale".to_string(), Vec::new());

        let hydrated = hydrate_workspace_dependency_documents(&mut workspace);

        assert!(hydrated.is_empty());
        assert!(workspace.local_export_chain_candidates.is_empty());
        assert!(workspace.local_export_chain_refresh_candidates.is_empty());
        assert!(workspace.dependency_store_hydration_metrics.is_some());
    }

    #[test]
    fn dependency_workspace_batch_reuses_full_text_candidate_cache() {
        clear_local_export_dependency_candidate_cache_for_tests();
        let uri = Arc::<str>::from("file:///dependency.abap");
        let object_name = Some(Arc::<str>::from("zcl_cached_dep"));
        let text = "CLASS zcl_cached_dep DEFINITION.\nENDCLASS.\nCLASS zcl_cached_dep IMPLEMENTATION.\nENDCLASS.\n";
        let cached_candidate = RemoteDependencyCandidate {
            name: "zfrom_cache".to_string(),
            kind: "type".to_string(),
        };
        let document = WorkspaceDocument {
            uri: Arc::clone(&uri),
            version: 1,
            text: text.to_string(),
            is_dependency: true,
            object_name: object_name.clone(),
        };
        store_local_export_dependency_candidates(&document, vec![cached_candidate.clone()]);

        let snapshot = DocumentStore::default().publish_input(DocumentInput {
            uri,
            version: 1,
            text: Arc::from(text),
            is_dependency: true,
            object_name,
        });
        let candidates =
            collect_remote_dependency_candidates_for_workspace_batch(snapshot.as_ref());

        assert!(
            candidates
                .iter()
                .any(|candidate| candidate == &cached_candidate),
            "{candidates:#?}"
        );
    }

    #[test]
    fn dependency_batch_cache_refreshes_when_dependency_source_changes() {
        let workspace_path = temp_workspace_path("dependency_batch_candidate_cache_refresh");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/main.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "DATA lo_demo TYPE REF TO zcl_first.\nlo_demo = zcl_first=>create( )."
                        .to_string(),
                },
            },
        );

        for (dependency_text, expected, unexpected) in [
            (
                "CLASS zcl_first DEFINITION.\n  PUBLIC SECTION.\n    CLASS-METHODS create RETURNING VALUE(ro_inst) TYPE REF TO zcl_first.\nENDCLASS.\nCLASS zcl_first IMPLEMENTATION.\n  METHOD create.\n    DATA lo_dep TYPE REF TO zcl_second.\n  ENDMETHOD.\nENDCLASS.\n",
                "zcl_second",
                "zcl_third",
            ),
            (
                "CLASS zcl_first DEFINITION.\n  PUBLIC SECTION.\n    CLASS-METHODS create RETURNING VALUE(ro_inst) TYPE REF TO zcl_first.\nENDCLASS.\nCLASS zcl_first IMPLEMENTATION.\n  METHOD create.\n    DATA lo_dep TYPE REF TO zcl_third.\n  ENDMETHOD.\nENDCLASS.\n",
                "zcl_third",
                "zcl_second",
            ),
        ] {
            store_remote_dependency_artifacts(
                &mut state,
                &StoreRemoteDependencyArtifactsParams {
                    workspace_uri: workspace_uri.clone(),
                    connection_key: Some("https://example.sap.local".to_string()),
                    artifacts: vec![DependencyArtifactPayload {
                        package_name: "ZPKG".to_string(),
                        object_kind: "global-class".to_string(),
                        object_name: "ZCL_FIRST".to_string(),
                        object_uri: "/sap/bc/adt/oo/classes/zcl_first".to_string(),
                        object_type: "CLAS/OC".to_string(),
                        description: "Remote class".to_string(),
                        file_extension: "abap".to_string(),
                        source_text: dependency_text.to_string(),
                        fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    }],
                    negative: Vec::new(),
                },
            )
            .expect("store dependency artifact");
            let _ = handle_remote_dependencies_updated(
                &mut state,
                &super::RemoteDependenciesUpdatedParams {
                    workspace_uri: workspace_uri.clone(),
                    source_uri: format!("{workspace_uri}/main.abap"),
                    source_uris: Vec::new(),
                    fetched: vec!["ZCL_FIRST".to_string()],
                    failed: Vec::new(),
                },
            );

            let workspace = state
                .workspaces
                .get_mut(&normalize_lsp_uri(&workspace_uri))
                .expect("workspace");
            workspace.remote_resolution_in_flight = false;
            workspace.remote_resolution_seen.clear();

            let batch = build_remote_dependency_batch_for_workspace(&mut state, &workspace_uri)
                .expect("follow-up batch");
            assert!(
                batch
                    .candidates
                    .iter()
                    .any(|candidate| candidate.name == expected),
                "{batch:#?}"
            );
            assert!(
                batch
                    .candidates
                    .iter()
                    .all(|candidate| candidate.name != unexpected),
                "{batch:#?}"
            );
        }

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn dependency_private_implementation_references_do_not_trigger_follow_up_remote_requests() {
        let workspace_path = temp_workspace_path("dependency_private_impl");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/main.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "DATA lo_demo TYPE REF TO zcl_first.\nlo_demo = zcl_first=>create( )."
                        .to_string(),
                },
            },
        );

        let initial =
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/main.abap"))
                .expect("initial request");
        assert!(
            initial
                .candidates
                .iter()
                .any(|candidate| candidate.name == "zcl_first")
        );

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_FIRST".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_first".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "CLASS zcl_first DEFINITION.\n  PUBLIC SECTION.\n    CLASS-METHODS create RETURNING VALUE(ro_inst) TYPE REF TO zcl_first.\n  PRIVATE SECTION.\n    CLASS-METHODS hidden.\nENDCLASS.\nCLASS zcl_first IMPLEMENTATION.\n  METHOD create.\n    hidden( ).\n  ENDMETHOD.\n  METHOD hidden.\n    DATA lo_hidden TYPE REF TO zcl_second.\n  ENDMETHOD.\nENDCLASS.\n".to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifact");

        let follow_up = build_remote_dependency_requests_for_workspace(&mut state, &workspace_uri);
        assert!(!follow_up.iter().any(|request| {
            request
                .source_uri
                .to_ascii_lowercase()
                .starts_with("abapls-cache:")
                && request
                    .candidates
                    .iter()
                    .any(|candidate| candidate.name == "zcl_second")
        }));

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn dependency_method_include_triggers_follow_up_remote_request() {
        let workspace_path = temp_workspace_path("dependency_method_include");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/main.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "DATA lo_demo TYPE REF TO zcl_first.\nlo_demo->run( ).".to_string(),
                },
            },
        );

        let initial =
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/main.abap"))
                .expect("initial request");
        assert!(
            initial
                .candidates
                .iter()
                .any(|candidate| candidate.name == "zcl_first")
        );

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_FIRST".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_first".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "CLASS zcl_first DEFINITION.\n  PUBLIC SECTION.\n    METHODS run.\nENDCLASS.\nCLASS zcl_first IMPLEMENTATION.\n  METHOD run.\n    INCLUDE zinc_method.\n  ENDMETHOD.\nENDCLASS.\n".to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifact");

        let _ = handle_remote_dependencies_updated(
            &mut state,
            &super::RemoteDependenciesUpdatedParams {
                workspace_uri: workspace_uri.clone(),
                source_uri: format!("{workspace_uri}/main.abap"),
                source_uris: Vec::new(),
                fetched: vec!["ZCL_FIRST".to_string()],
                failed: Vec::new(),
            },
        );

        let dependency_uri = dependency_uri_for_object_name(&state, &workspace_uri, "ZCL_FIRST");
        let dependency_snapshot = state
            .workspace_for_uri(&dependency_uri)
            .and_then(|workspace| workspace.cache.get(&dependency_uri))
            .expect("dependency snapshot");
        let dependency_candidates =
            collect_remote_dependency_candidates(dependency_snapshot.as_ref());
        assert!(
            dependency_candidates
                .iter()
                .any(|candidate| candidate.kind == "include" && candidate.name == "zinc_method"),
            "dependency_candidates={dependency_candidates:#?}"
        );

        let follow_up = build_remote_dependency_requests_for_workspace(&mut state, &workspace_uri);
        assert!(
            follow_up.iter().any(|request| {
                request.source_uri == dependency_uri
                    && request.candidates.iter().any(|candidate| {
                        candidate.kind == "include" && candidate.name == "zinc_method"
                    })
            }),
            "follow_up={follow_up:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn report_remote_dependency_request_includes_local_include_component_candidates() {
        let workspace_path = temp_workspace_path("report_include_component_remote");
        let report_dir = workspace_path.join("src").join("reports").join("ZREP");
        fs::create_dir_all(&report_dir).expect("report dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(
            report_dir.join("ZREP.abap"),
            "REPORT zrep.\nINCLUDE zrep_top.\nSTART-OF-SELECTION.\n  lo_app->run( ).\n",
        )
        .expect("report");
        fs::write(
            report_dir.join("ZREP_TOP.abap"),
            "DATA lo_app TYPE REF TO zcl_remote.\n",
        )
        .expect("include");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/src/reports/ZREP/ZREP.abap");
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri);
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&source_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text:
                        "REPORT zrep.\nINCLUDE zrep_top.\nSTART-OF-SELECTION.\n  lo_app->run( ).\n"
                            .to_string(),
                },
            },
        );

        let request =
            build_remote_dependency_request(&mut state, &source_uri).expect("remote request");
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.kind == "type" && candidate.name == "zcl_remote"),
            "request={request:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn opening_dependency_function_group_with_unsupported_simple_statements_requests_new_symbols() {
        let workspace_path = temp_workspace_path("dependency_function_group_open_remote");
        let dependency_dir = workspace_path
            .join("legacy-cache-root")
            .join("cache")
            .join("dependencies")
            .join("function-group");
        fs::create_dir_all(&dependency_dir).expect("dependency dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "/AIF/FILE_PROCESS_DATA"
kind = "function-group"
root_file = "legacy-cache/dependencies/function-group/%2FAIF%2FFILE_PROCESS_DATA.abap"
"#,
        )
        .expect("manifest");
        let stored_dependency_text = "\
FUNCTION /AIF/FILE_PROCESS_DATA.
  DATA lv_log_handle TYPE i.
  DATA lr_runtime TYPE REF TO object.
  SET UPDATE TASK LOCAL.
  GET TIME.
  LOG-POINT ID /aif/err_cp_01 SUBKEY 'FILE_PRO_DATA'
    FIELDS lv_log_handle.
  GET BADI lr_runtime.
  zcl_second=>run( ).
ENDFUNCTION.";
        fs::write(
            dependency_dir.join("%2FAIF%2FFILE_PROCESS_DATA.abap"),
            stored_dependency_text,
        )
        .expect("dependency file");
        let opened_dependency_text = "\
FUNCTION /AIF/FILE_PROCESS_DATA
  IMPORTING
    FILENR TYPE /AIF/FILENR OPTIONAL
    XIMSGGUID TYPE SXMSMGUID OPTIONAL
    CLASS_NAME_STD_IMPL TYPE SEOCLSNAME OPTIONAL
  CHANGING
    DATA TYPE ANY
  TABLES
    RETURN_TAB LIKE BAPIRET2 OPTIONAL
  EXCEPTIONS
    NOT_FOUND.
  zcl_second=>run( ).
ENDFUNCTION.";

        let workspace_uri = path_to_file_uri(&workspace_path);
        let dependency_uri = format!(
            "{workspace_uri}/legacy-cache/dependencies/function-group/%2FAIF%2FFILE_PROCESS_DATA.abap"
        );
        let normalized_dependency_uri = normalize_lsp_uri(&dependency_uri);
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        assert!(
            build_remote_dependency_request(&mut state, &dependency_uri).is_none(),
            "dependency surface projection should not expose implementation-only candidates"
        );

        let opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&dependency_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: opened_dependency_text.to_string(),
                },
            },
        );

        assert_eq!(opened.uri.as_ref(), normalized_dependency_uri.as_str());
        assert!(!opened.is_dependency);

        let request =
            build_remote_dependency_request(&mut state, normalized_dependency_uri.as_str())
                .expect("remote request after opening dependency");
        assert_eq!(request.source_uri, normalized_dependency_uri);
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.name == "zcl_second"),
            "request={request:#?}"
        );
        for expected in ["/aif/filenr", "sxmsmguid", "seoclsname", "bapiret2"] {
            assert!(
                request
                    .candidates
                    .iter()
                    .any(|candidate| candidate.kind == "type" && candidate.name == expected),
                "missing {expected} in request={request:#?}"
            );
        }

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn undeclared_dependency_cache_files_do_not_suppress_remote_requests() {
        let workspace_path = temp_workspace_path("dependency_cache_scan");
        let dependency_dir = workspace_path
            .join("legacy-cache-root")
            .join("cache")
            .join("dependencies")
            .join("ddic-structure");
        fs::create_dir_all(&dependency_dir).expect("dependency dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(
            dependency_dir.join("ZATTP_S_EU_NOTIF_32_JSON.xml"),
            r#"<root><elementInfo name="PAYLOAD" datatype="CHAR" /></root>"#,
        )
        .expect("xml");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        let _snapshot = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&format!("{workspace_uri}/main.abap")).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "DATA ls_payload TYPE zattp_s_eu_notif_32_json.".to_string(),
                },
            },
        );
        let request =
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/main.abap"))
                .expect("remote request");
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.kind == "type"
                    && candidate.name == "zattp_s_eu_notif_32_json")
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn changed_source_hydrates_new_type_from_central_dependency_store() {
        let workspace_path = temp_workspace_path("changed_source_hydrates_cached_type");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let initial_text = "REPORT zmain.\nDATA lv_text TYPE string.\n";
        fs::write(workspace_path.join("main.abap"), initial_text).expect("source");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/main.abap");
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        {
            let workspace = state
                .workspaces
                .get(&normalize_lsp_uri(&workspace_uri))
                .expect("workspace");
            let store = workspace_dependency_store(workspace).expect("dependency store");
            let profile = workspace
                .dependency_profile
                .clone()
                .expect("dependency profile");
            let source_text = "TYPES zcached_type TYPE string.";
            store
                .put_artifact(
                    &profile,
                    &StoredArtifactInput {
                        package_name: "ZPKG".to_string(),
                        object_kind: "ddic-data-element".to_string(),
                        object_name: "ZCACHED_TYPE".to_string(),
                        object_uri: "/sap/bc/adt/ddic/dataelements/zcached_type".to_string(),
                        object_type: "DTEL/DE".to_string(),
                        description: "Cached test type".to_string(),
                        file_extension: "abap".to_string(),
                        source_text: source_text.to_string(),
                        fetched_at: "2026-04-23T00:00:00Z".to_string(),
                        symbols: extract_stored_dependency_symbols(
                            "/sap/bc/adt/ddic/dataelements/zcached_type",
                            source_text,
                        ),
                    },
                )
                .expect("store cached artifact");
        }

        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&source_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: initial_text.to_string(),
                },
            },
        );

        let changed_text =
            "REPORT zmain.\nDATA lv_text TYPE string.\nDATA lv_cached TYPE zcached_type.\n";
        let snapshot = publish_changed_document_mut(
            &mut state,
            &DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: Uri::from_str(&source_uri).expect("uri"),
                    version: 2,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: changed_text.to_string(),
                }],
            },
        )
        .expect("changed snapshot");

        let workspace = state
            .workspaces
            .get(&normalize_lsp_uri(&workspace_uri))
            .expect("workspace");
        assert!(
            workspace.cache.uris().into_iter().any(|uri| {
                workspace.cache.get(uri.as_ref()).is_some_and(|snapshot| {
                    snapshot
                        .object_name
                        .as_ref()
                        .is_some_and(|name| name.eq_ignore_ascii_case("zcached_type"))
                })
            }),
            "cached type should be hydrated into the workspace after the edit"
        );

        let refreshed = workspace
            .cache
            .get(&normalize_lsp_uri(&source_uri))
            .expect("refreshed source snapshot");
        assert_eq!(snapshot.uri, refreshed.uri);
        let diagnostics = build_lsp_diagnostics_for_workspace(Some(workspace), refreshed.as_ref());
        assert!(
            diagnostics
                .iter()
                .all(|diagnostic| !diagnostic.message.contains("zcached_type")),
            "{diagnostics:#?}"
        );
        assert!(
            build_remote_dependency_request(&mut state, &source_uri).is_none(),
            "hydrated cached type should not be re-requested remotely"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn opened_cached_dependency_hydrates_inherited_member_owner() {
        let workspace_path = temp_workspace_path("opened_cached_dependency_inherited_member");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("main.abap"),
            "REPORT zmain.\nDATA lo_target TYPE REF TO zcl_target.\n",
        )
        .expect("source");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        {
            let workspace = state
                .workspaces
                .get(&normalize_lsp_uri(&workspace_uri))
                .expect("workspace");
            let store = workspace_dependency_store(workspace).expect("dependency store");
            let profile = workspace
                .dependency_profile
                .clone()
                .expect("dependency profile");
            for (name, source_text) in [
                (
                    "ZCL_TARGET",
                    "\
CLASS zcl_target DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS zcl_target IMPLEMENTATION.
  METHOD run.
    DATA lo_messages TYPE REF TO zcl_messages.
    lo_messages->set_message( ).
  ENDMETHOD.
ENDCLASS.",
                ),
                (
                    "ZCL_MESSAGES",
                    "\
CLASS zcl_messages DEFINITION PUBLIC INHERITING FROM zcl_base_messages.
ENDCLASS.
CLASS zcl_messages IMPLEMENTATION.
  METHOD constructor.
    DATA lo_noise TYPE REF TO zcl_noise.
  ENDMETHOD.
ENDCLASS.",
                ),
                (
                    "ZCL_BASE_MESSAGES",
                    "\
CLASS zcl_base_messages DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS set_message.
ENDCLASS.
CLASS zcl_base_messages IMPLEMENTATION.
ENDCLASS.",
                ),
                (
                    "ZCL_NOISE",
                    "\
CLASS zcl_noise DEFINITION PUBLIC.
ENDCLASS.
CLASS zcl_noise IMPLEMENTATION.
ENDCLASS.",
                ),
            ] {
                store
                    .put_artifact(
                        &profile,
                        &StoredArtifactInput {
                            package_name: "ZPKG".to_string(),
                            object_kind: "global-class".to_string(),
                            object_name: name.to_string(),
                            object_uri: format!("/sap/bc/adt/oo/classes/{}", name.to_lowercase()),
                            object_type: "CLAS/OC".to_string(),
                            description: "Remote class".to_string(),
                            file_extension: "abap".to_string(),
                            source_text: source_text.to_string(),
                            fetched_at: "2026-04-23T00:00:00Z".to_string(),
                            symbols: extract_stored_dependency_symbols(
                                &format!("/sap/bc/adt/oo/classes/{}", name.to_lowercase()),
                                source_text,
                            ),
                        },
                    )
                    .expect("store artifact");
            }
        }

        refresh_workspace(&mut state, &workspace_uri);
        let dependency_uri = dependency_uri_for_object_name(&state, &workspace_uri, "ZCL_TARGET");
        let dependency_text = dependency_text_for_uri(&state, &dependency_uri);
        let opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&dependency_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: dependency_text,
                },
            },
        );

        let workspace = state
            .workspaces
            .get(&normalize_lsp_uri(&workspace_uri))
            .expect("workspace");
        assert!(
            workspace.cache.uris().into_iter().any(|uri| {
                workspace.cache.get(uri.as_ref()).is_some_and(|snapshot| {
                    snapshot
                        .object_name
                        .as_ref()
                        .is_some_and(|name| name.eq_ignore_ascii_case("zcl_base_messages"))
                })
            }),
            "base class should be hydrated through the opened dependency chain"
        );
        assert!(
            workspace.cache.uris().into_iter().all(|uri| {
                workspace.cache.get(uri.as_ref()).is_none_or(|snapshot| {
                    snapshot
                        .object_name
                        .as_ref()
                        .is_none_or(|name| !name.eq_ignore_ascii_case("zcl_noise"))
                })
            }),
            "closed dependency implementation references should not fan out during hydration"
        );
        let diagnostics = build_lsp_diagnostics_for_workspace(Some(workspace), opened.as_ref());
        assert!(
            diagnostics
                .iter()
                .all(|diagnostic| { !diagnostic.message.contains("unknown member 'set_message'") }),
            "{diagnostics:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn failed_remote_open_sql_lookup_is_reported_as_error() {
        let workspace_path = temp_workspace_path("failed_remote_open_sql_lookup");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/main.abap");

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        let snapshot = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&source_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "SELECT * FROM zattp_rs_leg_ctr INTO TABLE @DATA(lt_rows).".to_string(),
                },
            },
        );

        let initial = build_lsp_diagnostics(snapshot.as_ref());
        assert!(initial.iter().any(|diag| {
            diag.severity == Some(DiagnosticSeverity::ERROR)
                && diag
                    .message
                    .contains("DDIC/repository lookup is not connected")
        }));

        let snapshots = handle_remote_dependencies_updated(
            &mut state,
            &super::RemoteDependenciesUpdatedParams {
                workspace_uri: workspace_uri.clone(),
                source_uri: source_uri.clone(),
                source_uris: Vec::new(),
                fetched: Vec::new(),
                failed: vec![super::RemoteDependencyCandidate {
                    name: "zattp_rs_leg_ctr".to_string(),
                    kind: "type".to_string(),
                }],
            },
        );
        assert!(!snapshots.is_empty());

        let workspace = state
            .workspaces
            .get(&normalize_lsp_uri(&workspace_uri))
            .expect("workspace");
        let snapshot = workspace
            .cache
            .get(&normalize_lsp_uri(&source_uri))
            .expect("refreshed snapshot");
        let diagnostics = build_lsp_diagnostics_for_workspace(Some(workspace), snapshot.as_ref());
        assert!(diagnostics.iter().any(|diag| {
            diag.severity == Some(DiagnosticSeverity::ERROR)
                && diag.message.contains("zattp_rs_leg_ctr")
                && diag
                    .message
                    .contains("was not found in the connected SAP system")
        }));

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn unresolved_remote_type_stays_error_until_lookup_fails() {
        let workspace_path = temp_workspace_path("unresolved_remote_type_lookup");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/main.abap");

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        let snapshot = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&source_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "DATA lt_objid TYPE /sttp/t_objid.".to_string(),
                },
            },
        );

        let workspace = state
            .workspaces
            .get(&normalize_lsp_uri(&workspace_uri))
            .expect("workspace");
        let initial = build_lsp_diagnostics_for_workspace(Some(workspace), snapshot.as_ref());
        assert!(
            initial.iter().any(|diag| {
                diag.severity == Some(DiagnosticSeverity::ERROR)
                    && diag.message.contains("/sttp/t_objid")
                    && diag
                        .message
                        .contains("DDIC/repository lookup is not connected")
            }),
            "initial diagnostics: {initial:?}"
        );

        let snapshots = handle_remote_dependencies_updated(
            &mut state,
            &super::RemoteDependenciesUpdatedParams {
                workspace_uri: workspace_uri.clone(),
                source_uri: source_uri.clone(),
                source_uris: Vec::new(),
                fetched: Vec::new(),
                failed: vec![super::RemoteDependencyCandidate {
                    name: "/sttp/t_objid".to_string(),
                    kind: "type".to_string(),
                }],
            },
        );
        assert!(!snapshots.is_empty());

        let workspace = state
            .workspaces
            .get(&normalize_lsp_uri(&workspace_uri))
            .expect("workspace");
        let snapshot = workspace
            .cache
            .get(&normalize_lsp_uri(&source_uri))
            .expect("refreshed snapshot");
        let diagnostics = build_lsp_diagnostics_for_workspace(Some(workspace), snapshot.as_ref());
        assert!(diagnostics.iter().any(|diag| {
            diag.severity == Some(DiagnosticSeverity::ERROR)
                && diag.message.contains("/sttp/t_objid")
                && diag
                    .message
                    .contains("was not found in the connected SAP system")
        }));

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn qualified_interface_method_scope_resolves_with_manifest_dependency() {
        let workspace_path = temp_workspace_path("qualified_interface_method_scope");
        let source_dir = workspace_path.join("src");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(&source_dir).expect("source dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "ZATTP_CL_RULE_PROC"
kind = "global-class"
root_file = "src/ZATTP_CL_RULE_PROC.abap"
members = [
  { file = "src/zcl_demo.abap", object_name = "zcl_demo" }
]
"#,
        )
        .expect("manifest");
        let dependency_source = r#"interface /STTP/IF_BADI_RULE_PROCESSING
  public .

  interfaces IF_BADI_INTERFACE .

  methods EXECUTE
    importing
      !IV_EVTID type /STTP/E_EVTID
      !IS_RULE_KEYS type /STTP/S_RULES_KEY optional
    changing
      !CO_MESSAGES type ref to /STTP/CL_MESSAGES optional .
endinterface.
"#;
        fs::write(
            source_dir.join("zcl_demo.abap"),
            r#"class zcl_demo definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces /STTP/IF_BADI_RULE_PROCESSING .

  methods PREPARE_DATA
    importing
      value(IS_RULE_KEYS) type /STTP/S_RULES_KEY
    exporting
      !EV_IFNAME type /AIF/IFNAME
      !EO_REQUEST_AIF_STRUCT type ref to DATA
      !EO_RESPONSE_AIF_STRUCT type ref to DATA
      !EO_SAP_STRUCT type ref to DATA
      !EO_SAP_TABLE type ref to DATA .
protected section.
private section.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD /sttp/if_badi_rule_processing~execute.

      CALL METHOD me->prepare_data
        EXPORTING
          is_rule_keys           = is_rule_keys
        IMPORTING
          ev_ifname              = DATA(lv_ifname)
          eo_request_aif_struct  = DATA(lo_req_str)
          eo_response_aif_struct = DATA(lo_resp_str)
          eo_sap_struct          = DATA(lo_sap_str)
          eo_sap_table           = DATA(lo_sap_tab).

      IF lv_ifname IS INITIAL.
        RETURN.
      ENDIF.

      CALL METHOD zattp_cl_rule_rs_aif_proc=>start_processing
        EXPORTING
          iv_evtid     = iv_evtid
          is_rule_keys = is_rule_keys
          iv_if_name   = lv_ifname
          io_req_str   = lo_req_str
          io_resp_str  = lo_resp_str
          io_sap_str   = lo_sap_str
          io_sap_tab   = lo_sap_tab
        CHANGING
          co_messages  = co_messages.

  ENDMETHOD.

  METHOD prepare_data.
  ENDMETHOD.
ENDCLASS.
"#,
        )
        .expect("source file");
        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/src/zcl_demo.abap");

        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri,
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-interface".to_string(),
                    object_name: "/STTP/IF_BADI_RULE_PROCESSING".to_string(),
                    object_uri: "/sap/bc/adt/oo/interfaces/%2FSTTP%2FIF_BADI_RULE_PROCESSING"
                        .to_string(),
                    object_type: "INTF/OI".to_string(),
                    description: "Remote interface".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: dependency_source.to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifact");
        let snapshot = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&source_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: fs::read_to_string(source_dir.join("zcl_demo.abap"))
                        .expect("source text"),
                },
            },
        );

        let diagnostics = build_lsp_diagnostics(snapshot.as_ref());
        assert!(
            !diagnostics.iter().any(|diag| {
                diag.message.contains("unknown symbol 'me'")
                    || diag.message.contains("unknown symbol 'is_rule_keys'")
                    || diag.message.contains("unknown symbol 'iv_evtid'")
                    || diag.message.contains("unknown symbol 'co_messages'")
            }),
            "{diagnostics:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn hover_returns_component_metadata_for_selector_field() {
        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///hover.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "\
TYPES: BEGIN OF ty_inner,
         a TYPE i,
       END OF ty_inner.
TYPES: BEGIN OF ty_outer,
         inner TYPE ty_inner,
       END OF ty_outer.
DATA ls_outer TYPE ty_outer.
ls_outer-inner-a = 1."
                        .to_string(),
                },
            },
        );

        let hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover.abap").expect("uri"),
                    },
                    position: Position {
                        line: 7,
                        character: 15,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");

        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("`a`"));
        assert!(markup.value.contains("scalar component"));
        assert!(markup.value.contains("`TYPE i`"));
        assert!(markup.value.contains("`ls_outer-inner-a`"));
    }

    #[test]
    fn opened_dependency_file_runs_full_analysis_when_opened_directly() {
        let workspace_path = temp_workspace_path("opened_dependency_remote_candidates");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let dependency_text = "\
CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
  METHOD run.
    DATA lo_helper TYPE REF TO zcl_missing.
    zcl_missing=>run( ).
  ENDMETHOD.
ENDCLASS.";

        let workspace_uri = path_to_file_uri(&workspace_path);
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_DEP".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_dep".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: dependency_text.to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifact");
        let dependency_uri = dependency_uri_for_object_name(&state, &workspace_uri, "ZCL_DEP");

        assert!(
            build_remote_dependency_request(&mut state, &dependency_uri).is_none(),
            "closed dependency should stay on public-surface candidates only"
        );

        let dependency_text = dependency_text_for_uri(&state, &dependency_uri);
        let opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&dependency_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: dependency_text,
                },
            },
        );

        assert!(!opened.is_dependency);
        let request = build_remote_dependency_request(&mut state, &dependency_uri)
            .expect("opened dependency request");
        assert_eq!(request.source_uri, dependency_uri);
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.name == "zcl_missing"),
            "request={request:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn opened_dependency_request_can_retry_negative_candidates_when_forced() {
        let workspace_path = temp_workspace_path("opened_dependency_retry_negatives");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let dependency_text = "\
CLASS zattp_cl_ar_dm_object DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS zattp_cl_ar_dm_object IMPLEMENTATION.
  METHOD run.
    DATA lt_params TYPE zattp_t_param_value.
    DATA lt_ranges TYPE rsds_frange_t.
  ENDMETHOD.
ENDCLASS.";

        let workspace_uri = path_to_file_uri(&workspace_path);
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZATTP_CL_AR_DM_OBJECT".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zattp_cl_ar_dm_object".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: dependency_text.to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: vec![
                    RemoteDependencyCandidate {
                        name: "zattp_t_param_value".to_string(),
                        kind: "type".to_string(),
                    },
                    RemoteDependencyCandidate {
                        name: "rsds_frange_t".to_string(),
                        kind: "type".to_string(),
                    },
                ],
            },
        )
        .expect("store dependency artifact");
        let dependency_uri =
            dependency_uri_for_object_name(&state, &workspace_uri, "ZATTP_CL_AR_DM_OBJECT");
        let dependency_text = dependency_text_for_uri(&state, &dependency_uri);
        let _opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&dependency_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: dependency_text,
                },
            },
        );
        let workspace = state
            .workspaces
            .get_mut(&normalize_lsp_uri(&workspace_uri))
            .expect("workspace");
        workspace
            .remote_lookup_failures
            .insert("type:zattp_t_param_value".to_string());
        workspace
            .remote_lookup_failures
            .insert("type:rsds_frange_t".to_string());

        assert!(
            build_remote_dependency_request(&mut state, &dependency_uri).is_none(),
            "default direct-open request should respect persisted negatives and session failures"
        );

        let request =
            build_remote_dependency_request_retrying_negatives(&mut state, &dependency_uri)
                .expect("forced opened dependency request");
        assert!(request.retry_negative_candidates);
        assert!(
            request.candidates.iter().any(
                |candidate| candidate.kind == "type" && candidate.name == "zattp_t_param_value"
            ),
            "{request:#?}"
        );
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.kind == "type" && candidate.name == "rsds_frange_t"),
            "{request:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn opened_stale_ddic_data_element_alias_requests_data_element_refresh() {
        let workspace_path = temp_workspace_path("opened_stale_ddic_data_element_alias");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let stale_data_element_xml = r#"
<blue:wbobj adtcore:name="/STTP/E_GS1_GLN" adtcore:type="DTEL/DE"
    xmlns:blue="http://www.sap.com/wbobj/dictionary/dtel"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <dtel:dataElement xmlns:dtel="http://www.sap.com/adt/dictionary/dataelements">
    <dtel:typeKind>domain</dtel:typeKind>
    <dtel:typeName>/STTP/D_GS1_GLN</dtel:typeName>
  </dtel:dataElement>
</blue:wbobj>
"#;

        let workspace_uri = path_to_file_uri(&workspace_path);
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "/STTP/BASIS".to_string(),
                    object_kind: "ddic-data-element".to_string(),
                    object_name: "/STTP/E_GS1_GLN".to_string(),
                    object_uri: "/sap/bc/adt/ddic/dataelements/%2fsttp%2fe_gs1_gln".to_string(),
                    object_type: "DTEL/DE".to_string(),
                    description: "Global Location Number".to_string(),
                    file_extension: "xml".to_string(),
                    source_text: stale_data_element_xml.to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: vec![RemoteDependencyCandidate {
                    name: "/sttp/d_gs1_gln".to_string(),
                    kind: "type".to_string(),
                }],
            },
        )
        .expect("store dependency artifact");
        let dependency_uri =
            dependency_uri_for_object_name(&state, &workspace_uri, "/STTP/E_GS1_GLN");
        let dependency_text = dependency_text_for_uri(&state, &dependency_uri);
        assert!(
            dependency_text
                .to_ascii_lowercase()
                .contains("types /sttp/e_gs1_gln type /sttp/d_gs1_gln")
        );

        let _opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&dependency_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: dependency_text,
                },
            },
        );

        let request =
            build_remote_dependency_request_retrying_negatives(&mut state, &dependency_uri)
                .expect("opened dependency refresh request");
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.kind == "type"
                    && candidate.name.eq_ignore_ascii_case("/sttp/e_gs1_gln")),
            "{request:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn opened_stale_ddic_table_without_key_metadata_requests_table_refresh() {
        let workspace_path = temp_workspace_path("opened_stale_ddic_table_key_metadata");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let stale_table_source = r#"
TYPES: BEGIN OF /sttp/loc, " Location
  locid TYPE /sttp/e_locid, " Location Identifier
  mandt TYPE mandt " Client
END OF /sttp/loc.
"#;

        let workspace_uri = path_to_file_uri(&workspace_path);
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let artifact = StoredArtifactInput {
            package_name: "/STTP/BASIS".to_string(),
            object_kind: "ddic-table".to_string(),
            object_name: "/STTP/LOC".to_string(),
            object_uri: "/sap/bc/adt/ddic/elementinfo?path=%2FSTTP%2FLOC".to_string(),
            object_type: "TABL/DT".to_string(),
            description: "Location".to_string(),
            file_extension: "abap".to_string(),
            source_text: stale_table_source.to_string(),
            fetched_at: "2026-04-23T00:00:00Z".to_string(),
            symbols: extract_stored_dependency_symbols(
                "/sap/bc/adt/ddic/elementinfo?path=%2FSTTP%2FLOC",
                stale_table_source,
            ),
        };
        let artifact_id = {
            let workspace = state
                .workspaces
                .get(&normalize_lsp_uri(&workspace_uri))
                .expect("workspace");
            let store = workspace_dependency_store(workspace).expect("dependency store");
            let profile = workspace
                .dependency_profile
                .clone()
                .expect("dependency profile");
            store.put_artifact(&profile, &artifact).expect("put table")
        };
        let input = dependency_document_input_from_payload_with_kind(
            &workspace_uri,
            artifact_id,
            &artifact,
        );
        {
            let workspace = state
                .workspaces
                .get_mut(&normalize_lsp_uri(&workspace_uri))
                .expect("workspace");
            let build_plan = workspace_committed_build_plan(workspace);
            workspace
                .cache
                .publish_inputs_with_build_plan(vec![input], build_plan);
        }

        let dependency_uri = dependency_uri_for_object_name(&state, &workspace_uri, "/STTP/LOC");
        let dependency_text = dependency_text_for_uri(&state, &dependency_uri);
        let _opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&dependency_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: dependency_text,
                },
            },
        );

        let request =
            build_remote_dependency_request_retrying_negatives(&mut state, &dependency_uri)
                .expect("opened stale table refresh request");
        assert!(
            request.candidates.iter().any(|candidate| {
                candidate.kind == "type" && candidate.name.eq_ignore_ascii_case("/sttp/loc")
            }),
            "{request:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn workspace_dependency_batch_requests_stale_cached_data_element_alias_refresh() {
        let workspace_path = temp_workspace_path("batch_stale_ddic_data_element_alias");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let stale_data_element_xml = r#"
<blue:wbobj adtcore:name="/STTP/E_GS1_GLN" adtcore:type="DTEL/DE"
    xmlns:blue="http://www.sap.com/wbobj/dictionary/dtel"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <dtel:dataElement xmlns:dtel="http://www.sap.com/adt/dictionary/dataelements">
    <dtel:typeKind>domain</dtel:typeKind>
    <dtel:typeName>/STTP/D_GS1_GLN</dtel:typeName>
  </dtel:dataElement>
</blue:wbobj>
"#;

        let workspace_uri = path_to_file_uri(&workspace_path);
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "/STTP/BASIS".to_string(),
                    object_kind: "ddic-data-element".to_string(),
                    object_name: "/STTP/E_GS1_GLN".to_string(),
                    object_uri: "/sap/bc/adt/ddic/dataelements/%2fsttp%2fe_gs1_gln".to_string(),
                    object_type: "DTEL/DE".to_string(),
                    description: "Global Location Number".to_string(),
                    file_extension: "xml".to_string(),
                    source_text: stale_data_element_xml.to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifact");

        let request = build_remote_dependency_batch_for_workspace(&mut state, &workspace_uri)
            .expect("dependency batch request");
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.kind == "type"
                    && candidate.name.eq_ignore_ascii_case("/sttp/e_gs1_gln")),
            "{request:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn dependency_cache_refresh_requests_stale_cached_data_element_aliases() {
        let workspace_path = temp_workspace_path("refresh_stale_cached_data_element_aliases");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let (profile, store) = {
            let workspace = state
                .workspaces
                .get(&normalize_lsp_uri(&workspace_uri))
                .expect("workspace");
            (
                workspace.dependency_profile.clone().expect("profile"),
                workspace_dependency_store(workspace).expect("store"),
            )
        };
        store
            .put_artifact(
                &profile,
                &StoredArtifactInput {
                    package_name: "/STTP/BASIS".to_string(),
                    object_kind: "ddic-data-element".to_string(),
                    object_name: "/STTP/E_GS1_GLN".to_string(),
                    object_uri: "/sap/bc/adt/ddic/dataelements/%2fsttp%2fe_gs1_gln".to_string(),
                    object_type: "DTEL/DE".to_string(),
                    description: "Global Location Number".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "TYPES /sttp/e_gs1_gln TYPE /sttp/d_gs1_gln.\n".to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    symbols: Vec::new(),
                },
            )
            .expect("store stale artifact");

        let request = build_remote_dependency_refresh_for_workspace(&mut state, &workspace_uri)
            .expect("refresh request");
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.kind == "type"
                    && candidate.name.eq_ignore_ascii_case("/sttp/e_gs1_gln")),
            "{request:#?}"
        );
        assert!(
            request.source_candidates.values().any(|candidates| {
                candidates.iter().any(|candidate| {
                    candidate.kind == "type"
                        && candidate.name.eq_ignore_ascii_case("/sttp/e_gs1_gln")
                })
            }),
            "{request:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn dependency_cache_refresh_requests_stale_cached_ddic_table_key_metadata() {
        let workspace_path = temp_workspace_path("refresh_stale_cached_ddic_table_keys");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let (profile, store) = {
            let workspace = state
                .workspaces
                .get(&normalize_lsp_uri(&workspace_uri))
                .expect("workspace");
            (
                workspace.dependency_profile.clone().expect("profile"),
                workspace_dependency_store(workspace).expect("store"),
            )
        };
        store
            .put_artifact(
                &profile,
                &StoredArtifactInput {
                    package_name: "/STTP/BASIS".to_string(),
                    object_kind: "ddic-table".to_string(),
                    object_name: "/STTP/LOC".to_string(),
                    object_uri: "/sap/bc/adt/ddic/elementinfo?path=%2FSTTP%2FLOC".to_string(),
                    object_type: "TABL/DT".to_string(),
                    description: "Location".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "\
TYPES: BEGIN OF /sttp/loc, \" Location
  locid TYPE /sttp/e_locid, \" Location Identifier
  mandt TYPE mandt \" Client
END OF /sttp/loc.
"
                    .to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    symbols: Vec::new(),
                },
            )
            .expect("store stale table artifact");

        let request = build_remote_dependency_refresh_for_workspace(&mut state, &workspace_uri)
            .expect("refresh request");
        assert!(
            request.candidates.iter().any(|candidate| {
                candidate.kind == "type" && candidate.name.eq_ignore_ascii_case("/sttp/loc")
            }),
            "{request:#?}"
        );
        assert!(
            request.source_candidates.values().any(|candidates| {
                candidates.iter().any(|candidate| {
                    candidate.kind == "type" && candidate.name.eq_ignore_ascii_case("/sttp/loc")
                })
            }),
            "{request:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn opened_dependency_request_infers_parent_unit_sidecar_context() {
        let workspace_path = temp_workspace_path("opened_dependency_parent_sidecar_context");
        let source_dir = workspace_path.join("src/reports/ZMAIN");
        fs::create_dir_all(&source_dir).expect("source dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(
            source_dir.join("abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                workspace_path
                    .join("exports")
                    .display()
                    .to_string()
                    .replace('\\', "/")
            ),
        )
        .expect("unit sidecar");
        fs::write(
            source_dir.join("ZMAIN.abap"),
            "REPORT zmain.\nzcl_dep=>run( ).\n",
        )
        .expect("source");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = normalize_lsp_uri(&path_to_file_uri(&source_dir.join("ZMAIN.abap")));
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_DEP".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_dep".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "\
CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
  METHOD run.
    DATA lo_missing TYPE REF TO zcl_missing.
  ENDMETHOD.
ENDCLASS."
                        .to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: vec![RemoteDependencyCandidate {
                    name: "zcl_missing".to_string(),
                    kind: "type".to_string(),
                }],
            },
        )
        .expect("store dependency artifact");
        let dependency_uri = dependency_uri_for_object_name(&state, &workspace_uri, "ZCL_DEP");
        let dependency_alias_uri = dependency_uri
            .replace("abapls-cache:///", "abapls-cache:/")
            .replace("?workspace=", "?workspace%3D")
            .replace("&artifact=", "%26artifact%3D")
            .replace("&name=", "%26name%3D")
            .replace("&kind=", "%26kind%3D");
        state
            .workspaces
            .get_mut(&normalize_lsp_uri(&workspace_uri))
            .expect("workspace")
            .dependency_parent_uris
            .clear();

        let dependency_text = dependency_text_for_uri(&state, &dependency_alias_uri);
        let opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&dependency_alias_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: dependency_text,
                },
            },
        );
        assert!(!opened.is_dependency);
        assert_eq!(opened.uri.as_ref(), dependency_uri);
        let workspace = state
            .workspaces
            .get(&normalize_lsp_uri(&workspace_uri))
            .expect("workspace");
        assert!(workspace.open_documents.contains_key(&dependency_uri));
        assert!(!workspace.open_documents.contains_key(&dependency_alias_uri));
        let dependency_cache_uris: Vec<_> = workspace
            .cache
            .uris()
            .into_iter()
            .filter(|uri| {
                workspace.cache.get(uri.as_ref()).is_some_and(|snapshot| {
                    snapshot
                        .object_name
                        .as_deref()
                        .is_some_and(|name| name.eq_ignore_ascii_case("ZCL_DEP"))
                })
            })
            .collect();
        assert_eq!(dependency_cache_uris.len(), 1, "{dependency_cache_uris:#?}");

        let request = build_remote_dependency_request(&mut state, &dependency_uri)
            .expect("opened dependency request should use inferred parent sidecar context");
        assert!(
            request.source_uris.contains(&dependency_uri),
            "{request:#?}"
        );
        assert!(request.source_uris.contains(&source_uri), "{request:#?}");
        assert!(
            request
                .source_candidates
                .get(&source_uri)
                .is_some_and(|candidates| candidates.iter().any(|candidate| {
                    candidate.name == "zcl_missing" && candidate.kind == "type"
                })),
            "{request:#?}"
        );
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.name == "zcl_missing" && candidate.kind == "type"),
            "local-first sidecar context should keep persisted negatives eligible: {request:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn opened_dependency_file_hydrates_cached_transitive_type_artifacts() {
        let workspace_path = temp_workspace_path("opened_dependency_hydrates_cached_types");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![
                    DependencyArtifactPayload {
                        package_name: "ZPKG".to_string(),
                        object_kind: "global-class".to_string(),
                        object_name: "ZCL_DEP".to_string(),
                        object_uri: "/sap/bc/adt/oo/classes/zcl_dep".to_string(),
                        object_type: "CLAS/OC".to_string(),
                        description: "Remote class".to_string(),
                        file_extension: "abap".to_string(),
                        source_text: "\
CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    DATA objid TYPE /sttp/e_objid.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
ENDCLASS."
                            .to_string(),
                        fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    },
                    DependencyArtifactPayload {
                        package_name: "/STTP/CORE".to_string(),
                        object_kind: "ddic-data-element".to_string(),
                        object_name: "/STTP/E_OBJID".to_string(),
                        object_uri: "/sap/bc/adt/ddic/dataelements/%2FSTTP%2FE_OBJID".to_string(),
                        object_type: "DTEL/DE".to_string(),
                        description: "Object Internal Identifier".to_string(),
                        file_extension: "abap".to_string(),
                        source_text: "TYPES /sttp/e_objid TYPE c LENGTH 32.".to_string(),
                        fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    },
                ],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifacts");

        let dependency_uri = dependency_uri_for_object_name(&state, &workspace_uri, "ZCL_DEP");
        let dependency_text = dependency_text_for_uri(&state, &dependency_uri);
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&dependency_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: dependency_text,
                },
            },
        );

        let workspace = state
            .workspaces
            .get(&normalize_lsp_uri(&workspace_uri))
            .expect("workspace");
        assert!(
            workspace.cache.uris().into_iter().any(|uri| {
                workspace.cache.get(uri.as_ref()).is_some_and(|snapshot| {
                    snapshot
                        .object_name
                        .as_ref()
                        .is_some_and(|name| name.eq_ignore_ascii_case("/sttp/e_objid"))
                })
            }),
            "cached data element should be hydrated into the workspace"
        );
        let snapshot = workspace
            .cache
            .get(&dependency_uri)
            .expect("opened dependency snapshot");
        let diagnostics = build_lsp_diagnostics_for_workspace(Some(workspace), snapshot.as_ref());
        assert!(
            diagnostics
                .iter()
                .all(|diagnostic| !diagnostic.message.contains("/sttp/e_objid")),
            "{diagnostics:#?}"
        );

        let request =
            build_remote_dependency_request_retrying_negatives(&mut state, &dependency_uri);
        assert!(
            request.is_none_or(|request| {
                request.candidates.iter().all(|candidate| {
                    !(candidate.kind == "type" && candidate.name == "/sttp/e_objid")
                })
            }),
            "already hydrated cached type should not be re-requested"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn opened_dependency_file_uses_cached_table_line_type_for_loop_inline_target() {
        let workspace_path = temp_workspace_path("opened_dependency_loop_inline_table_line");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![
                    DependencyArtifactPayload {
                        package_name: "ZPKG".to_string(),
                        object_kind: "global-class".to_string(),
                        object_name: "ZATTP_CL_AR_DM_OBJECT".to_string(),
                        object_uri: "/sap/bc/adt/oo/classes/zattp_cl_ar_dm_object".to_string(),
                        object_type: "CLAS/OC".to_string(),
                        description: "Remote class".to_string(),
                        file_extension: "abap".to_string(),
                        source_text: "\
CLASS zattp_cl_ar_dm_object DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS zattp_cl_ar_dm_object IMPLEMENTATION.
  METHOD run.
    TYPES: BEGIN OF ty_selopt,
             low TYPE string,
           END OF ty_selopt.
    DATA lt_bizstep_ex TYPE zattp_t_param_value.
    DATA ls_bizstep_p TYPE ty_selopt.

    LOOP AT lt_bizstep_ex INTO DATA(ls_bizstep_ex).
      ls_bizstep_p-low = ls_bizstep_ex.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS."
                            .to_string(),
                        fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    },
                    DependencyArtifactPayload {
                        package_name: "ZPKG".to_string(),
                        object_kind: "ddic-table-type".to_string(),
                        object_name: "ZATTP_T_PARAM_VALUE".to_string(),
                        object_uri: "/sap/bc/adt/ddic/tabletypes/zattp_t_param_value".to_string(),
                        object_type: "TTYP/DA".to_string(),
                        description: "Parameter values".to_string(),
                        file_extension: "abap".to_string(),
                        source_text: "TYPES zattp_t_param_value TYPE STANDARD TABLE OF zattp_param_value WITH EMPTY KEY.".to_string(),
                        fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    },
                    DependencyArtifactPayload {
                        package_name: "ZPKG".to_string(),
                        object_kind: "ddic-data-element".to_string(),
                        object_name: "ZATTP_PARAM_VALUE".to_string(),
                        object_uri: "/sap/bc/adt/ddic/dataelements/zattp_param_value".to_string(),
                        object_type: "DTEL/DE".to_string(),
                        description: "Parameter value".to_string(),
                        file_extension: "abap".to_string(),
                        source_text: "TYPES zattp_param_value TYPE string.".to_string(),
                        fetched_at: "2026-04-23T00:00:00Z".to_string(),
                    },
                ],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifacts");

        let dependency_uri =
            dependency_uri_for_object_name(&state, &workspace_uri, "ZATTP_CL_AR_DM_OBJECT");
        let dependency_text = dependency_text_for_uri(&state, &dependency_uri);
        publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&dependency_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: dependency_text,
                },
            },
        );

        let workspace = state
            .workspaces
            .get(&normalize_lsp_uri(&workspace_uri))
            .expect("workspace");
        let snapshot = workspace
            .cache
            .get(&dependency_uri)
            .expect("opened dependency snapshot");
        let diagnostics = build_lsp_diagnostics_for_workspace(Some(workspace), snapshot.as_ref());
        assert!(
            diagnostics.iter().all(|diagnostic| {
                !(diagnostic
                    .message
                    .contains("assignment target 'string' is incompatible with source")
                    && diagnostic.message.contains("zattp_t_param_value"))
            }),
            "{diagnostics:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn closed_dependency_batches_collect_full_transitive_candidates() {
        let workspace_path = temp_workspace_path("closed_dependency_batch_remote_candidates");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let dependency_text = "\
CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
  METHOD run.
    DATA lv_dbcnt TYPE sydbcnt.
    DATA lv_objtype TYPE string.
    lv_objtype = /sttp/cl_dm_constants=>gcs_objtype-lot.
    lv_dbcnt = 1.
  ENDMETHOD.
ENDCLASS.";

        let workspace_uri = path_to_file_uri(&workspace_path);
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_DEP".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_dep".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: dependency_text.to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifact");
        let dependency_uri = dependency_uri_for_object_name(&state, &workspace_uri, "ZCL_DEP");

        assert!(
            build_remote_dependency_request(&mut state, &dependency_uri).is_none(),
            "closed direct request should stay on public-surface candidates only"
        );

        let batch = build_remote_dependency_batch_for_workspace(&mut state, &workspace_uri)
            .expect("workspace batch");
        assert_eq!(batch.source_uri, dependency_uri, "{batch:#?}");
        assert!(
            batch
                .candidates
                .iter()
                .any(|candidate| candidate.kind == "type" && candidate.name == "sydbcnt"),
            "{batch:#?}"
        );
        assert!(
            batch.candidates.iter().any(|candidate| {
                candidate.kind == "static" && candidate.name == "/sttp/cl_dm_constants"
            }),
            "{batch:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn hover_and_definition_work_for_split_into_table_inline_target_and_source_field() {
        let state = ServerState::default();
        let text = "\
TYPES: BEGIN OF ty_trn,
         trncode TYPE string,
       END OF ty_trn.
DATA ls_trn TYPE ty_trn.

SPLIT ls_trn-trncode AT ':' INTO TABLE DATA(lt_split).
CLEAR lt_split.";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///split_hover.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let lt_split_offset = text.rfind("lt_split").expect("lt_split use") + 1;
        let lt_split_position =
            offset_to_position(text, lt_split_offset).expect("lt_split position");
        let lt_split_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///split_hover.abap").expect("uri"),
                    },
                    position: lt_split_position,
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("lt_split hover");
        let HoverContents::Markup(lt_split_markup) = lt_split_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(lt_split_markup.value.contains("`lt_split`"));
        assert!(
            lt_split_markup
                .value
                .contains("TYPE STANDARD TABLE OF string")
        );

        let ls_trn_offset = text.find("ls_trn-trncode").expect("ls_trn use") + 1;
        let ls_trn_position = offset_to_position(text, ls_trn_offset).expect("ls_trn position");
        let ls_trn_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///split_hover.abap").expect("uri"),
                    },
                    position: ls_trn_position,
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("ls_trn hover");
        let HoverContents::Markup(ls_trn_markup) = ls_trn_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(ls_trn_markup.value.contains("`ls_trn`"));
        assert!(ls_trn_markup.value.contains("TYPE ty_trn"));

        let trncode_offset = text.rfind("trncode").expect("trncode use") + 1;
        let trncode_position = offset_to_position(text, trncode_offset).expect("trncode position");
        let trncode_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///split_hover.abap").expect("uri"),
                    },
                    position: trncode_position,
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("trncode hover");
        let HoverContents::Markup(trncode_markup) = trncode_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(trncode_markup.value.contains("`trncode`"));
        assert!(trncode_markup.value.contains("scalar component"));
        assert!(trncode_markup.value.contains("TYPE string"));
        assert!(trncode_markup.value.contains("`ls_trn-trncode`"));

        let definition_result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///split_hover.abap").expect("uri"),
                    },
                    position: trncode_position,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("trncode definition");
        let GotoDefinitionResponse::Scalar(location) = definition_result else {
            panic!("expected scalar location");
        };
        assert_eq!(
            location.uri,
            Uri::from_str("file:///split_hover.abap").expect("uri")
        );
        let decl_offset = text
            .find("trncode TYPE string")
            .expect("trncode declaration");
        let decl_position = offset_to_position(text, decl_offset).expect("decl position");
        assert_eq!(location.range.start, decl_position);
    }

    #[test]
    fn hover_works_for_split_after_read_table_inline_into_source() {
        let state = ServerState::default();
        let text = "\
TYPES: BEGIN OF /sttp/dm_trn,
         bizttype TYPE i,
         trncode TYPE string,
       END OF /sttp/dm_trn.
TYPES /sttp/t_dm_trn TYPE STANDARD TABLE OF /sttp/dm_trn WITH EMPTY KEY.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    DATA mt_trn TYPE /sttp/t_dm_trn.
    METHODS run.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    READ TABLE mt_trn INTO DATA(ls_trn) WITH KEY bizttype = 60.
    SPLIT ls_trn-trncode AT ':' INTO TABLE DATA(lt_split).
    CLEAR lt_split.
  ENDMETHOD.
ENDCLASS.";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///read_table_split_hover.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let ls_trn_offset = text.find("ls_trn-trncode").expect("ls_trn use") + 1;
        let ls_trn_position = offset_to_position(text, ls_trn_offset).expect("ls_trn position");
        let ls_trn_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///read_table_split_hover.abap").expect("uri"),
                    },
                    position: ls_trn_position,
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("ls_trn hover");
        let HoverContents::Markup(ls_trn_markup) = ls_trn_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(ls_trn_markup.value.contains("`ls_trn`"));
        assert!(ls_trn_markup.value.contains("TYPE /sttp/dm_trn"));

        let trncode_offset = text.rfind("trncode").expect("trncode use") + 1;
        let trncode_position = offset_to_position(text, trncode_offset).expect("trncode position");
        let trncode_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///read_table_split_hover.abap").expect("uri"),
                    },
                    position: trncode_position,
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("trncode hover");
        let HoverContents::Markup(trncode_markup) = trncode_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(trncode_markup.value.contains("`trncode`"));
        assert!(trncode_markup.value.contains("TYPE string"));
        assert!(trncode_markup.value.contains("`ls_trn-trncode`"));
    }

    #[test]
    fn hover_shows_builtin_description_for_sy_field() {
        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///sy_hover.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "IF sy-subrc = 0. ENDIF.".to_string(),
                },
            },
        );

        let hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///sy_hover.abap").expect("uri"),
                    },
                    position: Position {
                        line: 0,
                        character: 7,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");

        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            !markup.value.contains("scalar component"),
            "unexpected generic scalar text: {}",
            markup.value
        );
        assert!(
            markup.value.contains("Return code") || markup.value.contains("return code"),
            "expected syst field documentation: {}",
            markup.value
        );
    }

    #[test]
    fn hover_returns_static_method_metadata_for_fat_arrow_selector() {
        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///hover_method.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "\
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS exec
      IMPORTING
        iv_value TYPE i.
ENDCLASS.

some_class=>exec( iv_value = 1 )."
                        .to_string(),
                },
            },
        );

        let hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_method.abap").expect("uri"),
                    },
                    position: Position {
                        line: 7,
                        character: 13,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");

        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("CLASS-METHODS exec"));
        assert!(markup.value.contains("iv_value TYPE i"));
        assert!(markup.value.contains("static method of `some_class`"));
    }

    #[test]
    fn hover_definition_and_completion_cover_class_type_selectors() {
        let state = ServerState::default();
        let text = "\
CLASS lcl_repro DEFINITION.
  PUBLIC SECTION.
    TYPES tr_errors TYPE RANGE OF string.
ENDCLASS.

DATA lt_data TYPE lcl_repro=>tr_errors.
DATA lv_other TYPE lcl_repro=>tr";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///hover_class_type.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let type_use_offset = text.rfind("tr_errors").expect("type use");
        let type_use_position = offset_to_position(text, type_use_offset + 1).expect("position");
        let hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_class_type.abap").expect("uri"),
                    },
                    position: type_use_position,
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");
        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            markup
                .value
                .contains("TYPES tr_errors TYPE RANGE OF string.")
        );
        assert!(markup.value.contains("type definition of `lcl_repro`"));
        assert!(markup.value.contains("used in type position"));

        let definition_result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_class_type.abap").expect("uri"),
                    },
                    position: type_use_position,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("definition");
        let GotoDefinitionResponse::Scalar(location) = definition_result else {
            panic!("expected scalar definition");
        };
        assert_eq!(
            location.range.start,
            offset_to_position(
                text,
                text.find("tr_errors TYPE RANGE OF string")
                    .expect("type declaration"),
            )
            .expect("declaration position")
        );

        let completion_offset =
            text.rfind("lcl_repro=>tr").expect("completion use") + "lcl_repro=>tr".len();
        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_class_type.abap").expect("uri"),
                    },
                    position: offset_to_position(text, completion_offset)
                        .expect("completion position"),
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");
        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].label, "tr_errors");
        assert_eq!(
            items[0].kind,
            Some(lsp_types::CompletionItemKind::TYPE_PARAMETER)
        );
        assert_eq!(items[0].detail.as_deref(), Some("TYPE RANGE OF string"));
        let Some(Documentation::MarkupContent(completion_docs)) = &items[0].documentation else {
            panic!("expected markdown docs");
        };
        assert!(completion_docs.value.contains("type definition"));
    }

    #[test]
    fn completion_returns_class_types_after_bare_fat_arrow() {
        let state = ServerState::default();
        let text = "\
CLASS lcl_repro DEFINITION.
  PUBLIC SECTION.
    TYPES tr_errors TYPE RANGE OF string.
ENDCLASS.

DATA lt_data TYPE lcl_repro=>";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_class_type_bare.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_class_type_bare.abap").expect("uri"),
                    },
                    position: offset_to_position(text, text.len()).expect("completion position"),
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: Some(CompletionContext {
                    trigger_kind: CompletionTriggerKind::TRIGGER_CHARACTER,
                    trigger_character: Some(">".to_string()),
                }),
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].label, "tr_errors");
    }

    #[test]
    fn hover_formats_method_signature_sections_across_multiple_lines() {
        let state = ServerState::default();
        let text = "\
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    METHODS send_notification_acc
      IMPORTING
        ! it_acc_obj TYPE gtt_acc_obj
        ! iv_rule_type TYPE /sttp/e_rule_t
        ! iv_cum_rule TYPE /sttp/e_rule_t
      EXPORTING
        ! ev_rep_status TYPE /sttp/e_status_rep_evt
        ! ev_http_code TYPE char10
      RAISING /sttp/cx_rep_exception.
ENDCLASS.

CLASS some_class IMPLEMENTATION.
ENDCLASS.

DATA lo_ref TYPE REF TO some_class.
lo_ref->send_notification_acc( ).";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///hover_method_pretty.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let call_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("send_notification_acc("))
            .expect("method call line");
        let method_col = call_line
            .1
            .find("send_notification_acc")
            .expect("method name column") as u32
            + 1;

        let hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_method_pretty.abap").expect("uri"),
                    },
                    position: Position {
                        line: call_line.0 as u32,
                        character: method_col,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");

        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            markup.value.contains("METHODS send_notification_acc"),
            "{}",
            markup.value
        );
        assert!(markup.value.contains("\n  IMPORTING\n"), "{}", markup.value);
        assert!(
            markup.value.contains("\n    ! it_acc_obj")
                && markup.value.contains("TYPE gtt_acc_obj"),
            "{}",
            markup.value
        );
        assert!(
            markup.value.contains("\n    ! iv_rule_type")
                && markup.value.contains("TYPE /sttp/e_rule_t"),
            "{}",
            markup.value
        );
        assert!(markup.value.contains("\n  EXPORTING\n"), "{}", markup.value);
        assert!(
            markup.value.contains("\n    ! ev_rep_status")
                && markup.value.contains("TYPE /sttp/e_status_rep_evt"),
            "{}",
            markup.value
        );
        assert!(markup.value.contains("\n  RAISING\n"), "{}", markup.value);
        assert!(
            markup.value.contains("\n    /sttp/cx_rep_exception"),
            "{}",
            markup.value
        );
    }

    #[test]
    fn hover_returns_superclass_and_signature_parameter_metadata() {
        let state = ServerState::default();
        let text = "\
CLASS some_base DEFINITION.
ENDCLASS.

CLASS some_base IMPLEMENTATION.
ENDCLASS.

CLASS some_sub DEFINITION INHERITING FROM some_base.
  PUBLIC SECTION.
    METHODS exec
      IMPORTING iv_input TYPE i
      RETURNING VALUE(rv_output) TYPE string.
ENDCLASS.
";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///hover_signature.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let superclass_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("INHERITING FROM some_base"))
            .expect("subclass header");
        let superclass_col = superclass_line
            .1
            .find("some_base")
            .expect("superclass column") as u32
            + 1;
        let superclass_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_signature.abap").expect("uri"),
                    },
                    position: Position {
                        line: superclass_line.0 as u32,
                        character: superclass_col,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("superclass hover");

        let HoverContents::Markup(super_markup) = superclass_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(super_markup.value.contains("`some_base`"));
        assert!(super_markup.value.contains("Class"));

        let param_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("iv_input"))
            .expect("parameter line")
            .0 as u32;
        let param_col = text
            .lines()
            .nth(param_line as usize)
            .expect("parameter text")
            .find("iv_input")
            .expect("parameter column") as u32
            + 1;
        let param_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_signature.abap").expect("uri"),
                    },
                    position: Position {
                        line: param_line,
                        character: param_col,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("parameter hover");

        let HoverContents::Markup(param_markup) = param_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(param_markup.value.contains("`iv_input`"));
        assert!(param_markup.value.contains("Parameter"));
        assert!(param_markup.value.contains("```abap\nTYPE i\n```"));
    }

    #[test]
    fn semantic_tokens_and_hover_cover_constructor_signature_parameters_and_types() {
        use lsp_types::SemanticTokenType;

        let state = ServerState::default();
        let text = "\
CLASS zcl_expr DEFINITION ABSTRACT.
ENDCLASS.

CLASS zcl_expr IMPLEMENTATION.
ENDCLASS.

CLASS zcl_binary_expr DEFINITION INHERITING FROM zcl_expr.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_left  TYPE REF TO zcl_expr
        iv_op    TYPE string
        io_right TYPE REF TO zcl_expr.
ENDCLASS.
";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///hover_ctor_signature.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let snapshot = state
            .cache
            .get("file:///hover_ctor_signature.abap")
            .expect("snapshot");
        let tokens = sem_tokens::build_semantic_tokens(snapshot.as_ref());
        let legend = sem_tokens::semantic_tokens_legend();
        let parameter_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::PARAMETER)
            .expect("legend has parameter") as u32;
        let type_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::TYPE)
            .expect("legend has type") as u32;
        let class_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::CLASS)
            .expect("legend has class") as u32;

        let io_left_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("io_left"))
            .expect("io_left line");
        let io_left_col = io_left_line.1.find("io_left").expect("io_left col") as u32 + 1;
        let zcl_expr_col = io_left_line.1.rfind("zcl_expr").expect("zcl_expr col") as u32 + 1;
        assert_eq!(
            semantic_token_type_at(&tokens, io_left_line.0 as u32, io_left_col),
            Some(parameter_idx),
            "expected constructor parameter name to highlight as parameter"
        );
        assert_eq!(
            semantic_token_type_at(&tokens, io_left_line.0 as u32, zcl_expr_col),
            Some(class_idx),
            "expected constructor ref type to highlight as class"
        );

        let iv_op_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("iv_op"))
            .expect("iv_op line");
        let iv_op_col = iv_op_line.1.find("iv_op").expect("iv_op col") as u32 + 1;
        let string_col = iv_op_line.1.find("string").expect("string col") as u32 + 1;
        assert_eq!(
            semantic_token_type_at(&tokens, iv_op_line.0 as u32, iv_op_col),
            Some(parameter_idx),
            "expected constructor scalar parameter to highlight as parameter"
        );
        assert_eq!(
            semantic_token_type_at(&tokens, iv_op_line.0 as u32, string_col),
            Some(type_idx),
            "expected constructor scalar type to highlight as type"
        );

        let io_left_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_ctor_signature.abap").expect("uri"),
                    },
                    position: Position {
                        line: io_left_line.0 as u32,
                        character: io_left_col,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("constructor parameter hover");
        let HoverContents::Markup(io_left_markup) = io_left_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(io_left_markup.value.contains("`io_left`"));
        assert!(io_left_markup.value.contains("Parameter"));
        assert!(
            io_left_markup
                .value
                .contains("```abap\nTYPE REF TO zcl_expr\n```")
        );

        let zcl_expr_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_ctor_signature.abap").expect("uri"),
                    },
                    position: Position {
                        line: io_left_line.0 as u32,
                        character: zcl_expr_col,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("constructor ref type hover");
        let HoverContents::Markup(zcl_expr_markup) = zcl_expr_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(zcl_expr_markup.value.contains("`zcl_expr`"));
        assert!(zcl_expr_markup.value.contains("Class"));

        let string_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_ctor_signature.abap").expect("uri"),
                    },
                    position: Position {
                        line: iv_op_line.0 as u32,
                        character: string_col,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("constructor built-in type hover");
        let HoverContents::Markup(string_markup) = string_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(string_markup.value.contains("`string`"));
        assert!(string_markup.value.contains("Built-in ABAP type"));
    }

    #[test]
    fn semantic_tokens_mark_class_attribute_declaration_as_property() {
        use lsp_types::SemanticTokenType;

        let state = ServerState::default();
        let text = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    DATA mv_value TYPE i.
ENDCLASS.
";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///sem_class_attr.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let snapshot = state
            .cache
            .get("file:///sem_class_attr.abap")
            .expect("snapshot");
        let tokens = sem_tokens::build_semantic_tokens(snapshot.as_ref());
        let legend = sem_tokens::semantic_tokens_legend();
        let property_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::PROPERTY)
            .expect("legend has property") as u32;
        let decl_mod = 1u32
            << legend
                .token_modifiers
                .iter()
                .position(|m| m.as_str() == "declaration")
                .expect("declaration modifier");

        let value_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("mv_value"))
            .expect("mv_value line");
        let value_col = value_line.1.find("mv_value").expect("mv_value col") as u32;

        assert_eq!(
            semantic_token_type_at(&tokens, value_line.0 as u32, value_col),
            Some(property_idx),
            "expected class attribute declaration to highlight as property"
        );
        let positions = semantic_token_positions(&tokens);
        assert!(
            positions
                .iter()
                .any(|&(line, character, _, token_type, modifiers)| {
                    line == value_line.0 as u32
                        && character == value_col
                        && token_type == property_idx
                        && (modifiers & decl_mod) != 0
                }),
            "expected declaration modifier on class attribute, tokens={positions:?}"
        );
    }

    #[test]
    fn hover_returns_resolved_variable_symbol() {
        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///hover_lv.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "DATA lv TYPE i.\nlv = 1.".to_string(),
                },
            },
        );
        let text = "DATA lv TYPE i.\nlv = 1.";
        let line_1_start = text.find('\n').expect("newline") + 1;
        let lv_use_col = (text.rfind("lv").expect("lv use") - line_1_start) as u32;

        let hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_lv.abap").expect("uri"),
                    },
                    position: Position {
                        line: 1,
                        character: lv_use_col + 1,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");

        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("`lv`"));
        assert!(markup.value.contains("Variable"));
        assert!(
            markup.value.contains("```abap\nTYPE i\n```"),
            "{}",
            markup.value
        );
    }

    #[test]
    fn hover_returns_constant_assigned_value() {
        let state = ServerState::default();
        let text = "CONSTANTS lc_flag TYPE c VALUE 'X'.\nDATA lv TYPE c.\nlv = lc_flag.";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///hover_constant.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );
        let line_2_start = text.rmatch_indices('\n').next().expect("last newline").0 + 1;
        let flag_use_col = (text.rfind("lc_flag").expect("constant use") - line_2_start) as u32;

        let hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_constant.abap").expect("uri"),
                    },
                    position: Position {
                        line: 2,
                        character: flag_use_col + 1,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");

        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("`lc_flag`"));
        assert!(markup.value.contains("Constant"));
        assert!(
            markup.value.contains("```abap\nTYPE c\n```"),
            "{}",
            markup.value
        );
        assert!(
            markup.value.contains("```abap\nVALUE 'X'\n```"),
            "{}",
            markup.value
        );
    }

    #[test]
    fn hover_formats_grouped_constant_and_shows_nested_value() {
        let state = ServerState::default();
        let text = "\
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gcs_rule_type,
        code_pairing TYPE string VALUE 'PAIR',
        comm_c TYPE string VALUE 'COMM',
      END OF gcs_rule_type.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA(lv_a) = zcl_demo=>gcs_rule_type-comm_c.
  DATA(lv_b) = zcl_demo=>gcs_rule_type-code_pairing.";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///hover_grouped_constant.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let parent_use = text.find("gcs_rule_type-comm_c").expect("parent use");
        let parent_line_start = text[..parent_use].rfind('\n').map_or(0, |idx| idx + 1);
        let parent_line = text[..parent_use]
            .bytes()
            .filter(|&byte| byte == b'\n')
            .count() as u32;
        let parent_col = (parent_use - parent_line_start) as u32;
        let parent_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_grouped_constant.abap").expect("uri"),
                    },
                    position: Position {
                        line: parent_line,
                        character: parent_col + 2,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("parent hover");
        let HoverContents::Markup(parent_markup) = parent_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            parent_markup.value.contains("BEGIN OF gcs_rule_type,\n    code_pairing TYPE string VALUE 'PAIR',\n    comm_c TYPE string VALUE 'COMM',\n  END OF gcs_rule_type."),
            "{}",
            parent_markup.value
        );

        let child_use = text.rfind("code_pairing").expect("child use");
        let child_line_start = text[..child_use].rfind('\n').map_or(0, |idx| idx + 1);
        let child_line = text[..child_use]
            .bytes()
            .filter(|&byte| byte == b'\n')
            .count() as u32;
        let child_col = (child_use - child_line_start) as u32;
        let child_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_grouped_constant.abap").expect("uri"),
                    },
                    position: Position {
                        line: child_line,
                        character: child_col + 1,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("child hover");
        let HoverContents::Markup(child_markup) = child_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(child_markup.value.contains("`code_pairing`"));
        assert!(
            child_markup.value.contains("```abap\nVALUE 'PAIR'\n```"),
            "{}",
            child_markup.value
        );
    }

    #[test]
    fn hover_returns_metadata_for_constructor_arguments_and_token_only_statements() {
        let state = ServerState::default();
        let text = "\
CLASS zcl_ast_node DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS to_string ABSTRACT
      RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_ast_node IMPLEMENTATION.
ENDCLASS.

CLASS zcl_expr DEFINITION ABSTRACT INHERITING FROM zcl_ast_node.
  PUBLIC SECTION.
ENDCLASS.

CLASS zcl_expr IMPLEMENTATION.
ENDCLASS.

CLASS zcl_stmt DEFINITION ABSTRACT INHERITING FROM zcl_ast_node.
  PUBLIC SECTION.
ENDCLASS.

CLASS zcl_stmt IMPLEMENTATION.
ENDCLASS.

CLASS zcl_assign_stmt DEFINITION INHERITING FROM zcl_stmt.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_name TYPE string
        io_expr TYPE REF TO zcl_expr.
ENDCLASS.

CLASS zcl_assign_stmt IMPLEMENTATION.
ENDCLASS.

CLASS zcl_print_stmt DEFINITION INHERITING FROM zcl_stmt.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_expr TYPE REF TO zcl_expr.
ENDCLASS.

CLASS zcl_print_stmt IMPLEMENTATION.
ENDCLASS.

CLASS zcl_program DEFINITION INHERITING FROM zcl_ast_node.
  PUBLIC SECTION.
    METHODS add_statement
      IMPORTING io_stmt TYPE REF TO zcl_stmt.
    METHODS to_string REDEFINITION.
ENDCLASS.

CLASS zcl_program IMPLEMENTATION.
  METHOD add_statement.
  ENDMETHOD.

  METHOD to_string.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo_expr1 TYPE REF TO zcl_expr.
  DATA lo_assign TYPE REF TO zcl_assign_stmt.
  DATA lo_print TYPE REF TO zcl_print_stmt.
  DATA lo_prog TYPE REF TO zcl_program.

  lo_assign = NEW zcl_assign_stmt(
    iv_name = 'x'
    io_expr = lo_expr1
  ).
  lo_prog->add_statement( lo_assign ).
  lo_prog->add_statement( lo_print ).
  WRITE / lo_prog->to_string( ).
";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///hover_simple_ast.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let lo_expr1_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("io_expr = lo_expr1"))
            .expect("constructor arg line");
        let lo_expr1_col = lo_expr1_line
            .1
            .find("lo_expr1")
            .expect("constructor arg col") as u32
            + 1;
        let lo_expr1_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_simple_ast.abap").expect("uri"),
                    },
                    position: Position {
                        line: lo_expr1_line.0 as u32,
                        character: lo_expr1_col,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("constructor argument hover");
        let HoverContents::Markup(lo_expr1_markup) = lo_expr1_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(lo_expr1_markup.value.contains("`lo_expr1`"));
        assert!(lo_expr1_markup.value.contains("Variable"));

        let add_stmt_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("add_statement( lo_assign )"))
            .expect("method call line");
        let add_stmt_col = add_stmt_line
            .1
            .find("add_statement")
            .expect("method name col") as u32
            + 1;
        let add_stmt_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_simple_ast.abap").expect("uri"),
                    },
                    position: Position {
                        line: add_stmt_line.0 as u32,
                        character: add_stmt_col,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("method hover");
        let HoverContents::Markup(add_stmt_markup) = add_stmt_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(add_stmt_markup.value.contains("METHODS add_statement"));
        assert!(
            add_stmt_markup
                .value
                .contains("io_stmt TYPE REF TO zcl_stmt")
        );

        let write_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("WRITE / lo_prog->to_string"))
            .expect("write line");
        let to_string_col = write_line.1.find("to_string").expect("to_string col") as u32 + 1;
        let to_string_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_simple_ast.abap").expect("uri"),
                    },
                    position: Position {
                        line: write_line.0 as u32,
                        character: to_string_col,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("write selector hover");
        let HoverContents::Markup(to_string_markup) = to_string_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(to_string_markup.value.contains("METHODS to_string"));
        assert!(
            to_string_markup
                .value
                .contains("instance method of `lo_prog`")
        );
    }

    #[test]
    fn hover_returns_instance_method_and_named_parameter_metadata_for_inline_new() {
        let state = ServerState::default();
        let text = "\
CLASS zcl_program DEFINITION.
  PUBLIC SECTION.
    METHODS add_statement
      IMPORTING io_stmt TYPE string.
ENDCLASS.

CLASS zcl_program IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA(lo_prog) = NEW zcl_program( ).
  lo_prog->add_statement( io_stmt = 'x' ).
";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///hover_inline_call.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let method_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("lo_prog->add_statement"))
            .expect("method line");
        let method_col = method_line.1.find("add_statement").expect("method col") as u32 + 1;
        let method_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_inline_call.abap").expect("uri"),
                    },
                    position: Position {
                        line: method_line.0 as u32,
                        character: method_col,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("method hover");
        let HoverContents::Markup(method_markup) = method_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(method_markup.value.contains("METHODS add_statement"));
        assert!(method_markup.value.contains("instance method of `lo_prog`"));

        let param_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("io_stmt = 'x'"))
            .expect("parameter line");
        let param_col = param_line.1.find("io_stmt").expect("parameter col") as u32 + 1;
        let param_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_inline_call.abap").expect("uri"),
                    },
                    position: Position {
                        line: param_line.0 as u32,
                        character: param_col,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("parameter hover");
        let HoverContents::Markup(param_markup) = param_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(param_markup.value.contains("`io_stmt`"));
        assert!(param_markup.value.contains("Parameter"));
        assert!(param_markup.value.contains("```abap\nTYPE string\n```"));
    }

    #[test]
    fn hover_returns_constructor_parameter_metadata_for_new_shorthand_target() {
        let state = ServerState::default();
        let text = "\
CLASS lcl_child DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING container_name TYPE string.
ENDCLASS.

CLASS lcl_child IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS display.
  PRIVATE SECTION.
    DATA mo_cont TYPE REF TO lcl_child.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD display.
    mo_cont = NEW #( container_name = 'CCONTAINER' ).
  ENDMETHOD.
ENDCLASS.
";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///hover_new_shorthand.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let param_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("container_name = 'CCONTAINER'"))
            .expect("parameter line");
        let param_col = param_line.1.find("container_name").expect("parameter col") as u32 + 1;
        let param_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_new_shorthand.abap").expect("uri"),
                    },
                    position: Position {
                        line: param_line.0 as u32,
                        character: param_col,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("parameter hover");
        let HoverContents::Markup(param_markup) = param_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(param_markup.value.contains("`container_name`"));
        assert!(param_markup.value.contains("Parameter"));
        assert!(param_markup.value.contains("```abap\nTYPE string\n```"));
    }

    #[test]
    fn builtin_routine_named_parameters_produce_diagnostics_not_hover() {
        use lsp_types::SemanticTokenType;

        let state = ServerState::default();
        let text = "DATA text TYPE string.\nDATA len TYPE i.\nlen = strlen( val = text ).";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///hover_routine_named.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let param_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("val = text"))
            .expect("parameter line");
        let param_col = param_line.1.find("val").expect("parameter col") as u32 + 1;
        let param_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_routine_named.abap").expect("uri"),
                    },
                    position: Position {
                        line: param_line.0 as u32,
                        character: param_col,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        );
        assert!(
            param_hover.is_none(),
            "builtin named parameter should not hover"
        );

        let snapshot = state
            .cache
            .get("file:///hover_routine_named.abap")
            .expect("snapshot");
        let diagnostics = build_lsp_diagnostics(snapshot.as_ref());
        assert!(diagnostics.iter().any(|diag| {
            diag.message.contains("strlen") && diag.message.contains("named parameter passing")
        }));

        let tokens = sem_tokens::build_semantic_tokens(snapshot.as_ref());
        let legend = sem_tokens::semantic_tokens_legend();
        let parameter_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::PARAMETER)
            .expect("legend has parameter") as u32;
        assert!(
            !tokens
                .data
                .iter()
                .any(|token| token.token_type == parameter_idx),
            "builtin named argument label should not be highlighted as parameter"
        );
    }

    #[test]
    fn builtin_routine_hover_uses_richer_shared_signatures() {
        let state = ServerState::default();
        let text = "DATA text TYPE string.\nDATA len TYPE i.\nlen = numofchar( arg = text ).";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///hover_builtin_signature.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let routine_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("numofchar"))
            .expect("routine line");
        let routine_col = routine_line.1.find("numofchar").expect("routine col") as u32 + 1;
        let routine_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_builtin_signature.abap").expect("uri"),
                    },
                    position: Position {
                        line: routine_line.0 as u32,
                        character: routine_col,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("routine hover");
        let HoverContents::Markup(routine_markup) = routine_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(routine_markup.value.contains("numofchar( arg )"));
        assert!(routine_markup.value.contains("returns `i`"));

        let param_col = routine_line.1.find("arg").expect("parameter col") as u32 + 1;
        let param_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_builtin_signature.abap").expect("uri"),
                    },
                    position: Position {
                        line: routine_line.0 as u32,
                        character: param_col,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        );
        assert!(
            param_hover.is_none(),
            "builtin named parameter should not hover"
        );

        let snapshot = state
            .cache
            .get("file:///hover_builtin_signature.abap")
            .expect("snapshot");
        let diagnostics = build_lsp_diagnostics(snapshot.as_ref());
        assert!(diagnostics.iter().any(|diag| {
            diag.message.contains("numofchar") && diag.message.contains("named parameter passing")
        }));
    }

    #[test]
    fn builtin_to_lower_hover_uses_shared_builtin_signature() {
        let state = ServerState::default();
        let text = "DATA text TYPE string.\nDATA lower TYPE string.\nlower = to_lower( text ).";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///hover_builtin_to_lower.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let routine_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("to_lower"))
            .expect("routine line");
        let routine_col = routine_line.1.find("to_lower").expect("routine col") as u32 + 1;
        let routine_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_builtin_to_lower.abap").expect("uri"),
                    },
                    position: Position {
                        line: routine_line.0 as u32,
                        character: routine_col,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("routine hover");
        let HoverContents::Markup(routine_markup) = routine_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(routine_markup.value.contains("to_lower( arg )"));
        assert!(routine_markup.value.contains("returns `string`"));
    }

    #[test]
    fn semantic_tokens_mark_line_exists_with_table_expression_as_function() {
        use lsp_types::SemanticTokenType;

        let state = ServerState::default();
        let text = "\
DATA lt_rep_evt TYPE STANDARD TABLE OF string WITH EMPTY KEY.\n\
IF line_exists( lt_rep_evt[ table_line = 'X' ] ).\n\
ENDIF.\n\
";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///semantic_line_exists.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let snapshot = state
            .cache
            .get("file:///semantic_line_exists.abap")
            .expect("snapshot");
        let tokens = sem_tokens::build_semantic_tokens(snapshot.as_ref());
        let legend = sem_tokens::semantic_tokens_legend();
        let function_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::FUNCTION)
            .expect("legend has function") as u32;

        let routine_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("line_exists"))
            .expect("routine line");
        let routine_col = routine_line.1.find("line_exists").expect("routine col") as u32;

        assert_eq!(
            semantic_token_type_at(&tokens, routine_line.0 as u32, routine_col),
            Some(function_idx)
        );
    }

    #[test]
    fn semantic_tokens_mark_line_exists_in_not_and_condition_as_function() {
        use lsp_types::SemanticTokenType;

        let state = ServerState::default();
        let text = "\
DATA lt_rep_evt TYPE STANDARD TABLE OF string WITH EMPTY KEY.\n\
DATA lt_obj_comm TYPE STANDARD TABLE OF string WITH EMPTY KEY.\n\
CONSTANTS lc_rs_comm TYPE string VALUE 'COMM'.\n\
IF NOT line_exists( lt_rep_evt[ rule_type = lc_rs_comm ] ) AND lt_obj_comm IS NOT INITIAL.\n\
ENDIF.\n\
";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///semantic_line_exists_not_and.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let snapshot = state
            .cache
            .get("file:///semantic_line_exists_not_and.abap")
            .expect("snapshot");
        let tokens = sem_tokens::build_semantic_tokens(snapshot.as_ref());
        let legend = sem_tokens::semantic_tokens_legend();
        let function_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::FUNCTION)
            .expect("legend has function") as u32;

        let routine_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("line_exists"))
            .expect("routine line");
        let routine_col = routine_line.1.find("line_exists").expect("routine col") as u32;

        assert_eq!(
            semantic_token_type_at(&tokens, routine_line.0 as u32, routine_col),
            Some(function_idx)
        );
    }

    #[test]
    fn hover_shows_builtin_signature_for_line_exists_in_or_condition() {
        let state = ServerState::default();
        let text = "\
TYPES: BEGIN OF ty_child,\n\
         trkid TYPE string,\n\
         serial TYPE string,\n\
       END OF ty_child.\n\
DATA lt_resp TYPE STANDARD TABLE OF ty_child WITH EMPTY KEY.\n\
DATA ls_child TYPE ty_child.\n\
IF line_exists( lt_resp[ trkid = ls_child-trkid ] ) OR\n\
   line_exists( lt_resp[ serial = ls_child-serial ] ).\n\
ENDIF.\n\
";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///hover_line_exists_or.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let routine_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("line_exists"))
            .expect("routine line");
        let routine_col = routine_line.1.find("line_exists").expect("routine col") as u32 + 1;
        let routine_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_line_exists_or.abap").expect("uri"),
                    },
                    position: Position {
                        line: routine_line.0 as u32,
                        character: routine_col,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("routine hover");
        let HoverContents::Markup(routine_markup) = routine_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(routine_markup.value.contains("line_exists( table_line )"));
        assert!(routine_markup.value.contains("returns `abap_bool`"));
    }

    #[test]
    fn hover_on_event_block_of_returns_full_event_header() {
        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///hover_event.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "START-OF-SELECTION.\n".to_string(),
                },
            },
        );

        let hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_event.abap").expect("uri"),
                    },
                    position: Position {
                        line: 0,
                        character: 7,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");

        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("`start-of-selection`"));
        assert!(markup.value.contains("Event"));

        let range = hover.range.expect("hover range");
        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 0);
        assert_eq!(range.end.line, 0);
        assert_eq!(range.end.character, "START-OF-SELECTION".len() as u32);
    }

    #[test]
    fn hover_and_definition_work_for_selection_screen_value_request_parameter() {
        let state = ServerState::default();
        let text = "\
PARAMETERS p_pub TYPE string.\n\
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pub.\n\
  WRITE p_pub.\n";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///selection_screen_hover.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let header_offset = text.find("FOR p_pub").expect("header ref") + "FOR ".len() + 1;
        let header_position = offset_to_position(text, header_offset).expect("header position");

        let hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///selection_screen_hover.abap").expect("uri"),
                    },
                    position: header_position,
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("header hover");

        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("`p_pub`"));
        assert!(markup.value.contains("TYPE string"));

        let definition_result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///selection_screen_hover.abap").expect("uri"),
                    },
                    position: header_position,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("header definition");
        let GotoDefinitionResponse::Scalar(location) = definition_result else {
            panic!("expected scalar location");
        };
        let decl_offset = text.find("p_pub TYPE string").expect("declaration");
        let decl_position = offset_to_position(text, decl_offset).expect("decl position");
        assert_eq!(location.range.start, decl_position);
    }

    #[test]
    fn hover_and_definition_work_for_selection_screen_value_request_parameter_singular() {
        let state = ServerState::default();
        let text = "\
PARAMETER p_pub TYPE string.\n\
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pub.\n\
  WRITE p_pub.\n";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///selection_screen_hover_parameter.abap")
                        .expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let header_offset = text.find("FOR p_pub").expect("header ref") + "FOR ".len() + 1;
        let header_position = offset_to_position(text, header_offset).expect("header position");

        let hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///selection_screen_hover_parameter.abap")
                            .expect("uri"),
                    },
                    position: header_position,
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("header hover");

        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("`p_pub`"));
        assert!(markup.value.contains("TYPE string"));

        let definition_result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///selection_screen_hover_parameter.abap")
                            .expect("uri"),
                    },
                    position: header_position,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("header definition");
        let GotoDefinitionResponse::Scalar(location) = definition_result else {
            panic!("expected scalar location");
        };
        let decl_offset = text.find("p_pub TYPE string").expect("declaration");
        let decl_position = offset_to_position(text, decl_offset).expect("decl position");
        assert_eq!(location.range.start, decl_position);
    }

    #[test]
    fn hover_returns_form_parameter_with_declared_type() {
        let state = ServerState::default();
        let text = "FORM f CHANGING cv TYPE string.\n  cv = 'x'.\nENDFORM.\n";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///form_hover.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///form_hover.abap").expect("uri"),
                    },
                    position: Position {
                        line: 1,
                        character: 3,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");

        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("`cv`"));
        assert!(markup.value.contains("Parameter"));
        assert!(
            markup
                .value
                .contains("```abap\nCHANGING\n  cv TYPE string\n```"),
            "{}",
            markup.value
        );
        assert!(markup.value.contains("parameter of FORM `f`"));
        assert!(!markup.value.contains("FORM f"), "{}", markup.value);
    }

    #[test]
    fn hover_on_perform_argument_returns_actual_variable_symbol() {
        let state = ServerState::default();
        let text = "\
FORM f USING VALUE(iv_input) TYPE i CHANGING cv_text TYPE string.
  cv_text = |{ iv_input }|.
ENDFORM.

START-OF-SELECTION.
  DATA lv_input TYPE i VALUE 1.
  DATA lv_text TYPE string.
  PERFORM f USING lv_input CHANGING lv_text.
";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///perform_hover.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let using_offset = text
            .rfind("lv_input")
            .expect("using argument at perform call");
        let using_position = offset_to_position(text, using_offset).expect("using position");
        let using_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///perform_hover.abap").expect("uri"),
                    },
                    position: using_position,
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("perform using hover");
        let HoverContents::Markup(using_markup) = using_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            using_markup.value.contains("`lv_input`"),
            "{}",
            using_markup.value
        );
        assert!(using_markup.value.contains("Variable"));
        assert!(
            using_markup.value.contains("```abap\nTYPE i\n```"),
            "{}",
            using_markup.value
        );

        let changing_offset = text
            .rfind("lv_text.")
            .expect("changing argument at perform call");
        let changing_position =
            offset_to_position(text, changing_offset).expect("changing position");
        let changing_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///perform_hover.abap").expect("uri"),
                    },
                    position: changing_position,
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("perform changing hover");
        let HoverContents::Markup(changing_markup) = changing_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            changing_markup.value.contains("`lv_text`"),
            "{}",
            changing_markup.value
        );
        assert!(changing_markup.value.contains("Variable"));
        assert!(
            changing_markup.value.contains("```abap\nTYPE string\n```"),
            "{}",
            changing_markup.value
        );
    }

    #[test]
    fn inlay_hints_show_form_parameter_names_for_perform_arguments() {
        let state = ServerState::default();
        let text = "\
FORM f USING VALUE(iv_input) TYPE i CHANGING cv_text TYPE string.
  cv_text = |{ iv_input }|.
ENDFORM.

START-OF-SELECTION.
  DATA lv_input TYPE i VALUE 1.
  DATA lv_text TYPE string.
  PERFORM f USING lv_input CHANGING lv_text.
";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///perform_hover.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let range = Range {
            start: offset_to_position(text, 0).expect("start position"),
            end: offset_to_position(text, text.len()).expect("end position"),
        };
        let hints = inlay_hints(
            &state,
            &InlayHintParams {
                text_document: TextDocumentIdentifier {
                    uri: Uri::from_str("file:///perform_hover.abap").expect("uri"),
                },
                range,
                work_done_progress_params: Default::default(),
            },
        )
        .expect("inlay hints");

        assert_eq!(hints.len(), 2, "{hints:?}");
        assert_eq!(
            hints[0].position,
            offset_to_position(text, text.rfind("lv_input").expect("using argument"))
                .expect("using position")
        );
        assert!(matches!(hints[0].kind, Some(InlayHintKind::PARAMETER)));
        let InlayHintLabel::String(using_label) = &hints[0].label else {
            panic!("expected string label");
        };
        assert_eq!(using_label, "iv_input:");
        let Some(InlayHintTooltip::MarkupContent(using_tooltip)) = hints[0].tooltip.as_ref() else {
            panic!("expected markdown tooltip");
        };
        assert!(using_tooltip.value.contains("parameter of FORM `f`"));
        assert!(using_tooltip.value.contains("VALUE(iv_input) TYPE i"));

        assert_eq!(
            hints[1].position,
            offset_to_position(text, text.rfind("lv_text.").expect("changing argument"))
                .expect("changing position")
        );
        assert!(matches!(hints[1].kind, Some(InlayHintKind::PARAMETER)));
        let InlayHintLabel::String(changing_label) = &hints[1].label else {
            panic!("expected string label");
        };
        assert_eq!(changing_label, "cv_text:");
        let Some(InlayHintTooltip::MarkupContent(changing_tooltip)) = hints[1].tooltip.as_ref()
        else {
            panic!("expected markdown tooltip");
        };
        assert!(changing_tooltip.value.contains("parameter of FORM `f`"));
        assert!(changing_tooltip.value.contains("cv_text TYPE string"));
    }

    #[test]
    fn inlay_hints_show_inline_variable_types() {
        let state = ServerState::default();
        let text = "\
CLASS lcl_demo DEFINITION.
ENDCLASS.

TYPES: stringtab TYPE STANDARD TABLE OF string WITH EMPTY KEY.
TYPES ty_fcat TYPE string.
DATA mt_fieldcat TYPE STANDARD TABLE OF ty_fcat WITH EMPTY KEY.

START-OF-SELECTION.
  DATA(lt_text) = VALUE stringtab( FOR n = 1 UNTIL n > 3 ( |{ n }| ) ).
  DATA(lo_demo) = NEW lcl_demo( ).
  APPEND INITIAL LINE TO mt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///inline_type_hint.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let range = Range {
            start: offset_to_position(text, 0).expect("start position"),
            end: offset_to_position(text, text.len()).expect("end position"),
        };
        let hints = inlay_hints(
            &state,
            &InlayHintParams {
                text_document: TextDocumentIdentifier {
                    uri: Uri::from_str("file:///inline_type_hint.abap").expect("uri"),
                },
                range,
                work_done_progress_params: Default::default(),
            },
        )
        .expect("inlay hints");

        let type_hints: Vec<_> = hints
            .iter()
            .filter(|hint| matches!(hint.kind, Some(InlayHintKind::TYPE)))
            .collect();
        assert_eq!(type_hints.len(), 3, "{hints:?}");

        assert_eq!(
            type_hints[0].position,
            offset_to_position(
                text,
                text.find("lt_text").expect("lt_text declaration") + "lt_text".len()
            )
            .expect("lt_text position")
        );
        let InlayHintLabel::String(lt_text_label) = &type_hints[0].label else {
            panic!("expected string label");
        };
        assert_eq!(lt_text_label, "stringtab");
        let Some(InlayHintTooltip::MarkupContent(lt_text_tooltip)) = type_hints[0].tooltip.as_ref()
        else {
            panic!("expected markdown tooltip");
        };
        assert!(
            lt_text_tooltip
                .value
                .contains("```abap\nTYPE stringtab\n```")
        );

        assert_eq!(
            type_hints[1].position,
            offset_to_position(
                text,
                text.find("lo_demo").expect("lo_demo declaration") + "lo_demo".len()
            )
            .expect("lo_demo position")
        );
        let InlayHintLabel::String(lo_demo_label) = &type_hints[1].label else {
            panic!("expected string label");
        };
        assert_eq!(lo_demo_label, "REF TO lcl_demo");
        let Some(InlayHintTooltip::MarkupContent(lo_demo_tooltip)) = type_hints[1].tooltip.as_ref()
        else {
            panic!("expected markdown tooltip");
        };
        assert!(
            lo_demo_tooltip
                .value
                .contains("```abap\nTYPE REF TO lcl_demo\n```")
        );

        assert_eq!(
            type_hints[2].position,
            offset_to_position(
                text,
                text.find("<fs_fcat>").expect("field-symbol declaration") + "<fs_fcat>".len()
            )
            .expect("field-symbol position")
        );
        let InlayHintLabel::String(fs_fcat_label) = &type_hints[2].label else {
            panic!("expected string label");
        };
        assert_eq!(fs_fcat_label, "ty_fcat");
        let Some(InlayHintTooltip::MarkupContent(fs_fcat_tooltip)) = type_hints[2].tooltip.as_ref()
        else {
            panic!("expected markdown tooltip");
        };
        assert!(fs_fcat_tooltip.value.contains("```abap\nTYPE ty_fcat\n```"));
    }

    #[test]
    fn inlay_hints_cover_call_function_parameters() {
        let state = ServerState::default();
        let dep_text = "\
FUNCTION z_demo_call
  IMPORTING
    iv_name TYPE string
  CHANGING
    cv_text TYPE string
  EXCEPTIONS
    failed.
ENDFUNCTION.
";
        let main_text = "\
START-OF-SELECTION.
  DATA lv_name TYPE string.
  DATA lv_text TYPE string.
  CALL FUNCTION 'z_demo_call'
    EXPORTING
      iv_name = lv_name
    CHANGING
      cv_text = lv_text
    EXCEPTIONS
      failed = 1.
";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///function_hint_dep.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: dep_text.to_string(),
                },
            },
        );
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///function_hint_main.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: main_text.to_string(),
                },
            },
        );

        let range = Range {
            start: offset_to_position(main_text, 0).expect("start position"),
            end: offset_to_position(main_text, main_text.len()).expect("end position"),
        };
        let hints = inlay_hints(
            &state,
            &InlayHintParams {
                text_document: TextDocumentIdentifier {
                    uri: Uri::from_str("file:///function_hint_main.abap").expect("uri"),
                },
                range,
                work_done_progress_params: Default::default(),
            },
        )
        .expect("inlay hints");

        assert_eq!(hints.len(), 2, "{hints:?}");
        assert_eq!(
            hints[0].position,
            offset_to_position(
                main_text,
                main_text.rfind("lv_name").expect("exporting argument")
            )
            .expect("exporting argument position")
        );
        let InlayHintLabel::String(exporting_label) = &hints[0].label else {
            panic!("expected string label");
        };
        assert_eq!(exporting_label, "string");
        let Some(InlayHintTooltip::MarkupContent(exporting_tooltip)) = hints[0].tooltip.as_ref()
        else {
            panic!("expected markdown tooltip");
        };
        assert!(
            exporting_tooltip
                .value
                .contains("parameter of FUNCTION MODULE `z_demo_call`")
        );
        assert!(exporting_tooltip.value.contains("IMPORTING"));
        assert!(exporting_tooltip.value.contains("iv_name TYPE string"));

        assert_eq!(
            hints[1].position,
            offset_to_position(
                main_text,
                main_text.rfind("lv_text").expect("changing argument")
            )
            .expect("changing argument position")
        );
        let InlayHintLabel::String(changing_label) = &hints[1].label else {
            panic!("expected string label");
        };
        assert_eq!(changing_label, "string");
        let Some(InlayHintTooltip::MarkupContent(changing_tooltip)) = hints[1].tooltip.as_ref()
        else {
            panic!("expected markdown tooltip");
        };
        assert!(
            changing_tooltip
                .value
                .contains("parameter of FUNCTION MODULE `z_demo_call`")
        );
        assert!(changing_tooltip.value.contains("CHANGING"));
        assert!(changing_tooltip.value.contains("cv_text TYPE string"));
    }

    #[test]
    fn code_action_syncs_method_parameter_comments_on_implementation_header() {
        let state = ServerState::default();
        let uri = Uri::from_str("file:///method_impl_comments.abap").expect("uri");
        let text = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS do_stmt_else
      IMPORTING
        iv_importing TYPE i
      EXPORTING
        ev_exporting TYPE i
      CHANGING
        cv_changing TYPE i
      RECEIVING
        VALUE(rv_receiving) TYPE i
      RETURNING
        VALUE(rv_returning) TYPE i.
    METHODS no_params.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD do_stmt_else.
  ENDMETHOD.
  METHOD no_params.
  ENDMETHOD.
ENDCLASS.
";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let header_offset = text
            .rfind("METHOD do_stmt_else")
            .expect("method implementation");
        let header_position =
            offset_to_position(text, header_offset + "METHOD ".len()).expect("header position");
        let actions = code_actions(
            &state,
            &CodeActionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                range: Range {
                    start: header_position,
                    end: header_position,
                },
                context: CodeActionContext {
                    diagnostics: Vec::new(),
                    only: None,
                    trigger_kind: None,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("code actions");

        assert_eq!(actions.len(), 1, "{actions:?}");
        let CodeActionOrCommand::CodeAction(action) = &actions[0] else {
            panic!("expected code action");
        };
        assert_eq!(
            action.title,
            "Sync method parameter comments for 'do_stmt_else'"
        );
        let changes = action
            .edit
            .as_ref()
            .and_then(|edit| edit.changes.as_ref())
            .expect("workspace changes");
        let edits = changes.get(&uri).expect("uri changes");
        assert_eq!(edits.len(), 1);
        let expected_insert = text.find("  ENDMETHOD.").expect("method body");
        assert_eq!(
            edits[0].range.start,
            offset_to_position(text, expected_insert).expect("insert position")
        );
        assert_eq!(edits[0].range.start, edits[0].range.end);
        assert_eq!(
            edits[0].new_text,
            "    \" IMPORTING\n    \"   iv_importing TYPE i\n    \" EXPORTING\n    \"   ev_exporting TYPE i\n    \" CHANGING\n    \"   cv_changing TYPE i\n    \" RECEIVING\n    \"   VALUE(rv_receiving) TYPE i\n    \" RETURNING\n    \"   VALUE(rv_returning) TYPE i\n"
        );
    }

    #[test]
    fn code_action_replaces_stale_method_parameter_comments() {
        let state = ServerState::default();
        let uri = Uri::from_str("file:///method_impl_stale_comments.abap").expect("uri");
        let text = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run
      IMPORTING
        iv_new TYPE string
      CHANGING
        cv_count TYPE i.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    \" abap-lsp: parameters begin
    \" IMPORTING
    \"   iv_old TYPE i
    \" abap-lsp: parameters end
    cv_count = cv_count.
  ENDMETHOD.
ENDCLASS.
";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let header_position = offset_to_position(
            text,
            text.rfind("METHOD run").expect("method implementation") + "METHOD ".len(),
        )
        .expect("header position");
        let actions = code_actions(
            &state,
            &CodeActionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                range: Range {
                    start: header_position,
                    end: header_position,
                },
                context: CodeActionContext {
                    diagnostics: Vec::new(),
                    only: None,
                    trigger_kind: None,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("code actions");

        assert_eq!(actions.len(), 1, "{actions:?}");
        let CodeActionOrCommand::CodeAction(action) = &actions[0] else {
            panic!("expected code action");
        };
        let changes = action
            .edit
            .as_ref()
            .and_then(|edit| edit.changes.as_ref())
            .expect("workspace changes");
        let edits = changes.get(&uri).expect("uri changes");
        assert_eq!(edits.len(), 1);
        assert_eq!(
            edits[0].range.start,
            offset_to_position(
                text,
                text.find("    \" abap-lsp: parameters begin")
                    .expect("managed block start")
            )
            .expect("managed block start position")
        );
        assert_eq!(
            edits[0].new_text,
            "    \" IMPORTING\n    \"   iv_new TYPE string\n    \" CHANGING\n    \"   cv_count TYPE i\n"
        );
    }

    #[test]
    fn inlay_hints_cover_method_and_constructor_parameters_in_all_call_syntaxes() {
        let state = ServerState::default();
        let text = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_ctor_text TYPE string.
    METHODS run
      IMPORTING
        iv_name TYPE string
      CHANGING
        cv_count TYPE i.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
  METHOD run.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo_demo TYPE REF TO lcl_demo.
  CREATE OBJECT lo_demo
    EXPORTING
      iv_ctor_text = 'legacy ctor'.
  CALL METHOD lo_demo->run
    EXPORTING
      iv_name = 'legacy method'
    CHANGING
      cv_count = DATA(lv_legacy_count).
  lo_demo->run(
    EXPORTING
      iv_name = 'expr method'
    CHANGING
      cv_count = DATA(lv_expr_count) ).
  DATA(lo_new) = NEW lcl_demo(
    iv_ctor_text = 'expr ctor' ).
";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///method_ctor_inlay.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let range = Range {
            start: offset_to_position(text, 0).expect("start position"),
            end: offset_to_position(text, text.len()).expect("end position"),
        };
        let hints = inlay_hints(
            &state,
            &InlayHintParams {
                text_document: TextDocumentIdentifier {
                    uri: Uri::from_str("file:///method_ctor_inlay.abap").expect("uri"),
                },
                range,
                work_done_progress_params: Default::default(),
            },
        )
        .expect("inlay hints");

        let parameter_hints: Vec<_> = hints
            .iter()
            .filter(|hint| matches!(hint.kind, Some(InlayHintKind::PARAMETER)))
            .collect();
        let type_hints: Vec<_> = hints
            .iter()
            .filter(|hint| matches!(hint.kind, Some(InlayHintKind::TYPE)))
            .collect();
        let call_parameter_hints: Vec<_> = parameter_hints
            .iter()
            .copied()
            .filter(|hint| matches!(inlay_hint_label_string(hint), "string" | "i"))
            .collect();

        assert_eq!(call_parameter_hints.len(), 6, "{hints:?}");
        assert_eq!(type_hints.len(), 3, "{hints:?}");

        let expected_positions = [
            text.find("'legacy ctor'").expect("legacy ctor position"),
            text.find("'legacy method'")
                .expect("legacy method position"),
            text.find("DATA(lv_legacy_count)")
                .expect("legacy changing position"),
            text.find("'expr method'").expect("expr method position"),
            text.find("DATA(lv_expr_count)")
                .expect("expr changing position"),
            text.find("'expr ctor'").expect("expr ctor position"),
        ];
        let expected_labels = ["string", "string", "i", "string", "i", "string"];
        let expected_tooltip_snippets = [
            (
                "parameter of CONSTRUCTOR `lcl_demo`",
                "iv_ctor_text TYPE string",
            ),
            ("parameter of METHOD `run`", "iv_name TYPE string"),
            ("parameter of METHOD `run`", "cv_count TYPE i"),
            ("parameter of METHOD `run`", "iv_name TYPE string"),
            ("parameter of METHOD `run`", "cv_count TYPE i"),
            (
                "parameter of CONSTRUCTOR `lcl_demo`",
                "iv_ctor_text TYPE string",
            ),
        ];

        for (idx, hint) in call_parameter_hints.iter().enumerate() {
            assert_eq!(
                hint.position,
                offset_to_position(text, expected_positions[idx]).expect("hint position")
            );
            let label = inlay_hint_label_string(hint);
            assert_eq!(label, expected_labels[idx]);
            let Some(InlayHintTooltip::MarkupContent(tooltip)) = hint.tooltip.as_ref() else {
                panic!("expected markdown tooltip");
            };
            assert!(tooltip.value.contains(expected_tooltip_snippets[idx].0));
            assert!(tooltip.value.contains(expected_tooltip_snippets[idx].1));
        }

        let expected_type_positions = [
            text.find("lv_legacy_count")
                .expect("legacy inline declaration")
                + "lv_legacy_count".len(),
            text.find("lv_expr_count").expect("expr inline declaration") + "lv_expr_count".len(),
            text.find("lo_new").expect("constructor inline declaration") + "lo_new".len(),
        ];
        let expected_type_labels = ["i", "i", "REF TO lcl_demo"];
        let expected_type_tooltips = [
            "```abap\nTYPE i\n```",
            "```abap\nTYPE i\n```",
            "```abap\nTYPE REF TO lcl_demo\n```",
        ];

        for (idx, hint) in type_hints.iter().enumerate() {
            assert_eq!(
                hint.position,
                offset_to_position(text, expected_type_positions[idx]).expect("type hint position")
            );
            let InlayHintLabel::String(label) = &hint.label else {
                panic!("expected string label");
            };
            assert_eq!(label, expected_type_labels[idx]);
            let Some(InlayHintTooltip::MarkupContent(tooltip)) = hint.tooltip.as_ref() else {
                panic!("expected markdown tooltip");
            };
            assert!(tooltip.value.contains(expected_type_tooltips[idx]));
        }
    }

    #[test]
    fn hover_returns_form_signature_when_hovering_form_name() {
        let state = ServerState::default();
        let text = "\
FORM f USING VALUE(iv_input) TYPE i CHANGING cv_text TYPE string.
  cv_text = |{ iv_input }|.
ENDFORM.

START-OF-SELECTION.
  DATA lv_input TYPE i VALUE 1.
  DATA lv_text TYPE string.
  PERFORM f USING lv_input CHANGING lv_text.
";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///form_name_hover.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let decl_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///form_name_hover.abap").expect("uri"),
                    },
                    position: Position {
                        line: 0,
                        character: 5,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("form declaration hover");
        let HoverContents::Markup(decl_markup) = decl_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            decl_markup
                .value
                .contains(
                    "```abap\nFORM f\n  USING\n    VALUE(iv_input) TYPE i\n  CHANGING\n    cv_text TYPE string\n```"
                ),
            "{}",
            decl_markup.value
        );
        assert_eq!(
            decl_markup.value,
            "```abap\nFORM f\n  USING\n    VALUE(iv_input) TYPE i\n  CHANGING\n    cv_text TYPE string\n```"
        );

        let call_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///form_name_hover.abap").expect("uri"),
                    },
                    position: Position {
                        line: 7,
                        character: 10,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("perform form hover");
        let HoverContents::Markup(call_markup) = call_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            call_markup
                .value
                .contains(
                    "```abap\nFORM f\n  USING\n    VALUE(iv_input) TYPE i\n  CHANGING\n    cv_text TYPE string\n```"
                ),
            "{}",
            call_markup.value
        );
        assert_eq!(
            call_markup.value,
            "```abap\nFORM f\n  USING\n    VALUE(iv_input) TYPE i\n  CHANGING\n    cv_text TYPE string\n```"
        );
    }

    #[test]
    fn hover_preserves_ref_to_type_clause_for_variable() {
        let state = ServerState::default();
        let text = "\
CLASS some_class DEFINITION.
ENDCLASS.

CLASS some_class IMPLEMENTATION.
ENDCLASS.

DATA lo_instance TYPE REF TO some_class.
CREATE OBJECT lo_instance.";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///hover_ref.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );
        let line_6_start = text.rmatch_indices('\n').next().expect("last newline").0 + 1;
        let lo_instance_use_col =
            (text.rfind("lo_instance").expect("lo_instance use") - line_6_start) as u32;

        let hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///hover_ref.abap").expect("uri"),
                    },
                    position: Position {
                        line: 6,
                        character: lo_instance_use_col + 1,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");

        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("`lo_instance`"));
        assert!(markup.value.contains("Variable"));
        assert!(
            markup
                .value
                .contains("```abap\nTYPE REF TO some_class\n```"),
            "{}",
            markup.value
        );
    }

    #[test]
    fn hover_returns_type_for_conditional_for_iteration_variable() {
        let state = ServerState::default();
        let text = "\
TYPES: stringtab TYPE STANDARD TABLE OF string WITH EMPTY KEY.

DATA(lt_text) = VALUE stringtab( FOR n = 1 UNTIL n > 3 ( |{ n }| ) ).";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///for_hover.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );
        let n_offset = text.rfind("{ n }").expect("template n") + 2;
        let line_2_start = text[..n_offset].rfind('\n').expect("line 2 newline") + 1;
        let n_col = (n_offset - line_2_start) as u32;

        let hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///for_hover.abap").expect("uri"),
                    },
                    position: Position {
                        line: 2,
                        character: n_col,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");

        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("`n`"), "{}", markup.value);
        assert!(markup.value.contains("Variable"), "{}", markup.value);
        assert!(
            markup.value.contains("```abap\nTYPE i\n```"),
            "{}",
            markup.value
        );
    }

    #[test]
    fn hover_preserves_named_value_constructor_type_for_inline_variable() {
        let state = ServerState::default();
        let text = "\
TYPES: stringtab TYPE STANDARD TABLE OF string WITH EMPTY KEY.

DATA(lt_text) = VALUE stringtab( FOR n = 1 UNTIL n > 3 ( |{ n }| ) ).
CLEAR lt_text.";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///inline_value_hover.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );
        let lt_text_offset = text.rfind("lt_text").expect("lt_text use") + 2;
        let line_start = text[..lt_text_offset].rfind('\n').expect("line newline") + 1;
        let column = (lt_text_offset - line_start) as u32;
        let line = text[..lt_text_offset]
            .bytes()
            .filter(|&b| b == b'\n')
            .count() as u32;

        let hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///inline_value_hover.abap").expect("uri"),
                    },
                    position: Position {
                        line,
                        character: column,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");

        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("`lt_text`"), "{}", markup.value);
        assert!(markup.value.contains("Variable"), "{}", markup.value);
        assert!(
            markup.value.contains("```abap\nTYPE stringtab\n```"),
            "{}",
            markup.value
        );
    }

    #[test]
    fn hover_uses_row_type_for_value_optional_table_expression_with_named_table_type() {
        let state = ServerState::default();
        let text = "\
TYPES: BEGIN OF ty_item,
         objid TYPE string,
       END OF ty_item.

TYPES: tty_item TYPE STANDARD TABLE OF ty_item WITH EMPTY KEY.

DATA it_obj_itm TYPE tty_item.
DATA is_obj_ids TYPE ty_item.
DATA(ls_obj_itm) = VALUE #( it_obj_itm[ objid = is_obj_ids-objid ] OPTIONAL ).
CLEAR ls_obj_itm.";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///value_optional_row_hover.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );
        let offset = text.rfind("ls_obj_itm").expect("ls_obj_itm use") + 2;
        let line_start = text[..offset].rfind('\n').expect("line newline") + 1;
        let column = (offset - line_start) as u32;
        let line = text[..offset].bytes().filter(|&b| b == b'\n').count() as u32;

        let hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///value_optional_row_hover.abap").expect("uri"),
                    },
                    position: Position {
                        line,
                        character: column,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");

        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("`ls_obj_itm`"), "{}", markup.value);
        assert!(markup.value.contains("Variable"), "{}", markup.value);
        assert!(
            markup.value.contains("```abap\nTYPE ty_item\n```"),
            "{}",
            markup.value
        );
    }

    #[test]
    fn hover_returns_type_for_let_variable() {
        let state = ServerState::default();
        let text = "\
TYPES: stringtab TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA(lt_text) = VALUE stringtab(
  LET it = `be`
  IN ( |To { it } is to do| )
     ( |To do is to { it }| ) ).";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///let_hover.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );
        let it_offset = text.find("{ it }").expect("template it") + 2;
        let line_start = text[..it_offset].rfind('\n').expect("line newline") + 1;
        let it_col = (it_offset - line_start) as u32;
        let it_line = text[..it_offset].bytes().filter(|&b| b == b'\n').count() as u32;

        let hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///let_hover.abap").expect("uri"),
                    },
                    position: Position {
                        line: it_line,
                        character: it_col,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");

        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("`it`"), "{}", markup.value);
        assert!(markup.value.contains("Variable"), "{}", markup.value);
        assert!(
            markup.value.contains("```abap\nTYPE string\n```"),
            "{}",
            markup.value
        );
    }

    #[test]
    fn did_change_updates_hover_results() {
        let state = ServerState::default();
        let uri = Uri::from_str("file:///hover_change.abap").expect("uri");
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "abap".to_string(),
                    version: 1,
                    text:
                        "DATA: BEGIN OF ls_date, yyyy(4), END OF ls_date.\nls_date-yyyy = '2026'."
                            .to_string(),
                },
            },
        );
        publish_changed_document(
            &state,
            &DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: uri.clone(),
                    version: 2,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: "DATA: BEGIN OF ls_date, mm(2), END OF ls_date.\nls_date-mm = '04'."
                        .to_string(),
                }],
            },
        );

        let hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position {
                        line: 1,
                        character: 8,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");

        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("`mm`"));
        assert!(markup.value.contains("`ls_date-mm`"));
    }

    #[test]
    fn hover_works_for_describe_table_source_inside_structured_simple_stmt() {
        let state = ServerState::default();
        let text = "\
DATA lt_split TYPE STANDARD TABLE OF string WITH EMPTY KEY.

DESCRIBE TABLE lt_split LINES DATA(lv_lines).
IF lv_lines > 0.
  WRITE lv_lines.
ENDIF.";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///describe_hover.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let offset = text.find("lt_split LINES").expect("describe source");
        let line = text[..offset].bytes().filter(|&b| b == b'\n').count() as u32;
        let line_start = text[..offset].rfind('\n').map(|idx| idx + 1).unwrap_or(0);
        let character = (offset - line_start) as u32;

        let hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///describe_hover.abap").expect("uri"),
                    },
                    position: Position { line, character },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");

        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("`lt_split`"), "{}", markup.value);
        assert!(markup.value.contains("Variable"), "{}", markup.value);
    }

    #[test]
    fn completion_returns_selector_components() {
        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "\
TYPES: BEGIN OF ty_inner,
         alpha TYPE i,
         amount TYPE i,
       END OF ty_inner.
TYPES: BEGIN OF ty_outer,
         inner TYPE ty_inner,
       END OF ty_outer.
DATA ls_outer TYPE ty_outer.
ls_outer-inner-a"
                        .to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion.abap").expect("uri"),
                    },
                    position: Position {
                        line: 8,
                        character: 16,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].label, "alpha");
        assert_eq!(items[1].label, "amount");
        assert_eq!(items[0].detail.as_deref(), Some("TYPE i"));
        let Some(Documentation::MarkupContent(markup)) = &items[0].documentation else {
            panic!("expected markdown docs");
        };
        assert!(markup.value.contains("scalar component"));
        assert!(markup.value.contains("declared as `TYPE i`"));
    }

    #[test]
    fn completion_returns_bare_delete_where_fields_after_where_keyword() {
        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_where.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "\
TYPES: BEGIN OF ty_row,
         status_trn TYPE i,
         trn_id TYPE i,
       END OF ty_row.
TYPES ty_tab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lt_trans_del TYPE ty_tab.
DELETE lt_trans_del WHERE "
                        .to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_where.abap").expect("uri"),
                    },
                    position: Position {
                        line: 6,
                        character: 26,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].label, "status_trn");
        assert_eq!(items[1].label, "trn_id");
    }

    #[test]
    fn completion_returns_delete_comparing_fields_after_comparing_keyword() {
        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_comparing.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "\
TYPES: BEGIN OF ty_row,
         matnr TYPE i,
         lgnum TYPE i,
       END OF ty_row.
TYPES ty_tab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lt_lqua TYPE ty_tab.
DELETE ADJACENT DUPLICATES FROM lt_lqua COMPARING "
                        .to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_comparing.abap").expect("uri"),
                    },
                    position: Position {
                        line: 6,
                        character: 49,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].label, "lgnum");
        assert_eq!(items[1].label, "matnr");
    }

    #[test]
    fn completion_returns_modify_transporting_fields_after_transporting_keyword() {
        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_modify_transporting.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "\
TYPES: BEGIN OF ty_row,
         low TYPE string,
         sign TYPE string,
         option TYPE string,
       END OF ty_row.
TYPES ty_tab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lt_rows TYPE ty_tab.
DATA ls_row TYPE ty_row.
MODIFY lt_rows FROM ls_row TRANSPORTING "
                        .to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_modify_transporting.abap")
                            .expect("uri"),
                    },
                    position: Position {
                        line: 8,
                        character: 40,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        assert_eq!(items.len(), 3);
        assert_eq!(items[0].label, "low");
        assert_eq!(items[1].label, "option");
        assert_eq!(items[2].label, "sign");
    }

    #[test]
    fn completion_returns_modify_where_fields_after_where_keyword() {
        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_modify_where.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "\
TYPES: BEGIN OF ty_row,
         low TYPE string,
         sign TYPE string,
         option TYPE string,
       END OF ty_row.
TYPES ty_tab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lt_rows TYPE ty_tab.
DATA ls_row TYPE ty_row.
MODIFY lt_rows FROM ls_row TRANSPORTING sign option WHERE "
                        .to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_modify_where.abap").expect("uri"),
                    },
                    position: Position {
                        line: 8,
                        character: 58,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        assert_eq!(items.len(), 3);
        assert_eq!(items[0].label, "low");
        assert_eq!(items[1].label, "option");
        assert_eq!(items[2].label, "sign");
    }

    #[test]
    fn completion_returns_public_static_methods_after_fat_arrow() {
        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_method.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "\
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS exec.
    CLASS-METHODS expose.
  PRIVATE SECTION.
    CLASS-METHODS hidden.
ENDCLASS.

some_class=>e"
                        .to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_method.abap").expect("uri"),
                    },
                    position: Position {
                        line: 8,
                        character: 12,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].label, "exec");
        assert_eq!(items[1].label, "expose");
        assert!(
            items
                .iter()
                .all(|item| item.kind == Some(lsp_types::CompletionItemKind::METHOD))
        );
        let Some(Documentation::MarkupContent(markup)) = &items[0].documentation else {
            panic!("expected markdown docs");
        };
        assert!(markup.value.contains("CLASS-METHODS exec"));
        assert!(markup.value.contains("static method"));
    }

    #[test]
    fn completion_emits_method_call_snippet_with_required_parameters() {
        let mut state = ServerState::default();
        state.client_capabilities.completion_snippet_support = true;
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_method_snippet.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "\
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run
      IMPORTING iv_value TYPE i
      CHANGING cv_total TYPE i.
ENDCLASS.

some_class=>r"
                        .to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_method_snippet.abap").expect("uri"),
                    },
                    position: Position {
                        line: 7,
                        character: 13,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "run")
            .expect("run completion item");
        assert_eq!(item.insert_text_format, Some(InsertTextFormat::SNIPPET));
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(
            edit.new_text,
            "run(\n  EXPORTING\n    iv_value = ${1}\n  CHANGING\n    cv_total = ${2}\n)$0"
        );
    }

    #[test]
    fn completion_emits_importing_section_for_methods_with_output_parameters() {
        let mut state = ServerState::default();
        state.client_capabilities.completion_snippet_support = true;
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_instance_method_sections.abap")
                        .expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "\
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    METHODS run
      IMPORTING iv_value TYPE i
      EXPORTING ev_total TYPE i
      CHANGING cv_text TYPE string.
ENDCLASS.

CLASS some_class IMPLEMENTATION.
ENDCLASS.

DATA lo_demo TYPE REF TO some_class.
lo_demo->r"
                        .to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_instance_method_sections.abap")
                            .expect("uri"),
                    },
                    position: Position {
                        line: 12,
                        character: 10,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "run")
            .expect("run completion item");
        assert_eq!(item.insert_text_format, Some(InsertTextFormat::SNIPPET));
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(
            edit.new_text,
            "run(\n  EXPORTING\n    iv_value = ${1}\n  IMPORTING\n    ev_total = ${2}\n  CHANGING\n    cv_text = ${3}\n)$0"
        );
    }

    #[test]
    fn completion_falls_back_to_plain_text_call_templates_without_snippet_support() {
        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_method_plain.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "\
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run
      IMPORTING iv_value TYPE i.
ENDCLASS.

some_class=>r"
                        .to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_method_plain.abap").expect("uri"),
                    },
                    position: Position {
                        line: 6,
                        character: 13,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "run")
            .expect("run completion item");
        assert_eq!(item.insert_text_format, None);
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(edit.new_text, "run(\n  iv_value = \n)");
    }

    #[test]
    fn completion_emits_function_module_call_snippet() {
        let mut state = ServerState::default();
        state.client_capabilities.completion_snippet_support = true;
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_function_module.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "\
FUNCTION z_demo_call
  IMPORTING
    iv_name TYPE string
  EXPORTING
    ev_text TYPE string
  EXCEPTIONS
    failed.
ENDFUNCTION.

START-OF-SELECTION.
  CALL FUNCTION 'z_de"
                        .to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_function_module.abap").expect("uri"),
                    },
                    position: Position {
                        line: 10,
                        character: 21,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "z_demo_call")
            .expect("function module completion item");
        assert_eq!(item.kind, Some(lsp_types::CompletionItemKind::FUNCTION));
        assert_eq!(item.insert_text_format, Some(InsertTextFormat::SNIPPET));
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(
            edit.new_text,
            "z_demo_call'\n  EXPORTING\n    iv_name = ${1}\n  IMPORTING\n    ev_text = ${2}\n  EXCEPTIONS\n    failed = ${3:1}.$0"
        );
    }

    #[test]
    fn completion_emits_perform_call_template_without_snippet_support() {
        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_perform.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "\
FORM update_item USING uv_name TYPE string CHANGING cv_total TYPE i.
ENDFORM.

START-OF-SELECTION.
  PERFORM up"
                        .to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_perform.abap").expect("uri"),
                    },
                    position: Position {
                        line: 4,
                        character: 12,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "update_item")
            .expect("perform completion item");
        assert_eq!(item.kind, Some(lsp_types::CompletionItemKind::FUNCTION));
        assert_eq!(item.insert_text_format, None);
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(
            edit.new_text,
            "update_item\n  USING\n    uv_name\n  CHANGING\n    cv_total."
        );
    }

    #[test]
    fn completion_returns_local_export_function_module_template_from_workspace_sidecar() {
        let workspace_path = temp_workspace_path("workspace_local_export_function_completion");
        let export_root = temp_workspace_path("workspace_local_export_function_completion_export");
        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
        fs::create_dir_all(workspace_path.join("src/reports/ZREP")).expect("report dir");
        fs::create_dir_all(export_root.join("function-module")).expect("export dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let source = "\
REPORT zrep.
START-OF-SELECTION.
  CALL FUNCTION 'z_de";
        fs::write(workspace_path.join("src/reports/ZREP/ZREP.abap"), source).expect("report");
        fs::write(
            workspace_path.join("src/reports/ZREP/abapls-unit.toml"),
            format!(
                "[local_export]\nroots = [\"{}\"]\n\n[dependencies]\nsource = \"local-first\"\n",
                export_root.to_string_lossy().replace('\\', "/")
            ),
        )
        .expect("sidecar");
        fs::write(
            export_root.join("function-module/Z_DEMO_CALL.abap"),
            "\
FUNCTION z_demo_call
  IMPORTING
    iv_name TYPE string
  EXPORTING
    ev_text TYPE string
  EXCEPTIONS
    failed.
ENDFUNCTION.",
        )
        .expect("export");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/src/reports/ZREP/ZREP.abap");
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let line = source
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("CALL FUNCTION"))
            .expect("call line");
        let character = line.1.find("z_de").expect("prefix") as u32 + 4;
        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&source_uri).expect("uri"),
                    },
                    position: Position {
                        line: line.0 as u32,
                        character,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "z_demo_call")
            .expect("function module completion item");
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(
            edit.new_text,
            "z_demo_call'\n  EXPORTING\n    iv_name = \n  IMPORTING\n    ev_text = \n  EXCEPTIONS\n    failed = 1."
        );

        let _ = fs::remove_dir_all(&workspace_path);
        let _ = fs::remove_dir_all(&export_root);
    }

    #[test]
    fn completion_emits_constructor_call_snippet() {
        let mut state = ServerState::default();
        state.client_capabilities.completion_snippet_support = true;
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_constructor_snippet.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "\
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_value TYPE i.
    METHODS run.
ENDCLASS.

CLASS some_class IMPLEMENTATION.
  METHOD run.
    me->con
  ENDMETHOD.
ENDCLASS."
                        .to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_constructor_snippet.abap")
                            .expect("uri"),
                    },
                    position: Position {
                        line: 9,
                        character: 11,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "constructor")
            .expect("constructor completion item");
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(edit.new_text, "constructor(\n  iv_value = ${1}\n)$0");
    }

    #[test]
    fn completion_returns_named_argument_labels_inside_method_calls() {
        let mut state = ServerState::default();
        state.client_capabilities.completion_snippet_support = true;
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_named_args.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "\
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    METHODS run
      IMPORTING iv_value TYPE i
                iv_other TYPE i OPTIONAL.
ENDCLASS.

CLASS some_class IMPLEMENTATION.
ENDCLASS.

DATA lo_demo TYPE REF TO some_class.
lo_demo->run( iv_v )"
                        .to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_named_args.abap").expect("uri"),
                    },
                    position: Position {
                        line: 11,
                        character: 18,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "iv_value")
            .expect("iv_value completion item");
        assert_eq!(item.kind, Some(lsp_types::CompletionItemKind::VARIABLE));
        assert_eq!(item.insert_text_format, Some(InsertTextFormat::SNIPPET));
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(edit.new_text, "iv_value = ${1}");
    }

    #[test]
    fn completion_returns_method_parameters_inside_method_implementation() {
        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_method_impl_params.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "\
CLASS lo_epcis_builder DEFINITION.
  PUBLIC SECTION.
    METHODS method_name
      IMPORTING
        iv_importing TYPE i
      EXPORTING
        ev_exporting TYPE i
      CHANGING
        cv_changing TYPE i
      RETURNING
        VALUE(rv_returning) TYPE i.
ENDCLASS.

CLASS lo_epcis_builder IMPLEMENTATION.
  METHOD method_name.
    rv_returning = iv_imp
  ENDMETHOD.
ENDCLASS."
                        .to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_method_impl_params.abap")
                            .expect("uri"),
                    },
                    position: Position {
                        line: 15,
                        character: 25,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "iv_importing")
            .expect("iv_importing completion item");
        assert_eq!(item.kind, Some(lsp_types::CompletionItemKind::VARIABLE));
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(edit.new_text, "iv_importing");
    }

    #[test]
    fn completion_marks_keyword_items_as_secondary_priority_after_symbols() {
        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_keywords_after_symbols.abap")
                        .expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_input TYPE i.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    i
  ENDMETHOD.
ENDCLASS."
                        .to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_keywords_after_symbols.abap")
                            .expect("uri"),
                    },
                    position: Position {
                        line: 7,
                        character: 5,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let symbol = items
            .iter()
            .find(|item| item.label == "iv_input")
            .expect("symbol completion item");
        let keyword = items
            .iter()
            .find(|item| item.label == "IF")
            .expect("keyword completion item");
        assert_eq!(symbol.kind, Some(lsp_types::CompletionItemKind::VARIABLE));
        assert_eq!(keyword.kind, Some(lsp_types::CompletionItemKind::KEYWORD));
        assert_eq!(symbol.sort_text.as_deref(), Some("1:iv_input"));
        assert_eq!(keyword.sort_text.as_deref(), Some("2:if"));
    }

    #[test]
    fn completion_returns_global_scope_type_and_variable_items() {
        let state = ServerState::default();
        let text = "\
TYPES: BEGIN OF ts_obj,
        objid TYPE c LENGTH 50,
        status_pack TYPE i,
       END OF ts_obj,

       tt_obj TYPE TABLE OF ts_obj.

TYPES: BEGIN OF ts_obj_ids,
        objid TYPE c LENGTH 50,
        serial TYPE c LENGTH 60,
       END OF ts_obj_ids,

       tt_obj_ids TYPE TABLE OF ts_obj_ids.

TYPES: BEGIN OF ts_loc,
        locno TYPE c LENGTH 6,
        gln TYPE c LENGTH 13,
       END OF ts_loc,

       tt_loc TYPE TABLE OF ts_loc.

DATA: lt_obj TYPE tt_obj,
      ls_obj TYPE ts_obj,
      lt_obj_ids TYPE tt_obj_ids,
      ls_obj_ids TYPE ts_obj_ids,
      lt_loc TYPE tt_loc,
      ls_loc TYPE ts_loc.

MOVE-CORRESPONDING ls_loc TO ls_obj.

SORT lt_obj BY objid.
";
        let uri = Uri::from_str("file:///completion_global_scope.abap").expect("uri");
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let type_offset =
            text.find("lt_obj TYPE tt_obj").expect("type usage") + "lt_obj TYPE tt_".len();
        let type_completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: offset_to_position(text, type_offset).expect("type position"),
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("type completion");
        let CompletionResponse::Array(type_items) = type_completion else {
            panic!("expected array completion");
        };
        let table_type = type_items
            .iter()
            .find(|item| item.label == "tt_obj")
            .expect("tt_obj type completion");
        assert_eq!(
            table_type.kind,
            Some(lsp_types::CompletionItemKind::TYPE_PARAMETER)
        );

        let value_offset = text.find("MOVE-CORRESPONDING ls_loc").expect("value usage")
            + "MOVE-CORRESPONDING ls_".len();
        let value_completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: offset_to_position(text, value_offset).expect("value position"),
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("value completion");
        let CompletionResponse::Array(value_items) = value_completion else {
            panic!("expected array completion");
        };
        let variable = value_items
            .iter()
            .find(|item| item.label == "ls_loc")
            .expect("ls_loc variable completion");
        assert_eq!(variable.kind, Some(lsp_types::CompletionItemKind::VARIABLE));
    }

    #[test]
    fn completion_filters_already_specified_named_arguments() {
        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_named_args_filter.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "\
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    METHODS run
      IMPORTING iv_value TYPE i
                iv_other TYPE i OPTIONAL.
ENDCLASS.

CLASS some_class IMPLEMENTATION.
ENDCLASS.

DATA lo_demo TYPE REF TO some_class.
lo_demo->run( iv_value = 1 iv_ )"
                        .to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_named_args_filter.abap")
                            .expect("uri"),
                    },
                    position: Position {
                        line: 11,
                        character: 17,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let labels: Vec<_> = items.into_iter().map(|item| item.label).collect();
        assert_eq!(labels, vec!["iv_other"]);
    }

    #[test]
    fn workspace_preview_completion_keeps_dependency_backed_context() {
        let workspace_path = temp_workspace_path("preview_dependency_completion");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(workspace_path.join("src/reports/ZMAIN")).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("src/reports/ZMAIN/ZMAIN.abap"),
            "\
REPORT zreport_main.
DATA lo_helper TYPE REF TO zcl_helper.
lo_helper->",
        )
        .expect("main");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri =
            normalize_lsp_uri(&format!("{workspace_uri}/src/reports/ZMAIN/ZMAIN.abap"));
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_HELPER".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_helper".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Helper class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "\
CLASS zcl_helper DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS zcl_helper IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS."
                        .to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifact");
        refresh_workspace(&mut state, &workspace_uri);
        let dependency_uri = dependency_uri_for_object_name(&state, &workspace_uri, "ZCL_HELPER");
        assert!(
            snapshot_for_uri(&state, &dependency_uri).is_some(),
            "expected hydrated dependency snapshot after refresh"
        );
        let committed = snapshot_for_uri(&state, &source_uri).expect("committed source snapshot");
        assert!(
            committed
                .completion_at(
                    committed
                        .text
                        .find("lo_helper->")
                        .map(|offset| offset + "lo_helper->".len())
                        .expect("selector offset"),
                )
                .map(|completion| {
                    completion
                        .items
                        .into_iter()
                        .map(|item| match item {
                            abap_cache::CompletionItem::Selector(item) => item.name.to_string(),
                            abap_cache::CompletionItem::NamedArgument(item) => {
                                item.name.to_string()
                            }
                            abap_cache::CompletionItem::Symbol(item) => item.name.to_string(),
                            abap_cache::CompletionItem::Template(item) => item.name.to_string(),
                            abap_cache::CompletionItem::Callable(item) => item.name.to_string(),
                            abap_cache::CompletionItem::Keyword(item) => item.name.to_string(),
                        })
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default()
                .iter()
                .any(|item| item == "run"),
            "expected committed dependency-backed completion"
        );

        assert!(stage_workspace_preview_snapshot(
            &mut state,
            &source_uri,
            2,
            "\
REPORT zreport_main.
DATA lo_helper TYPE REF TO zcl_helper.
lo_helper->r
"
        ));

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&source_uri).expect("uri"),
                    },
                    position: Position {
                        line: 2,
                        character: 12,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        assert!(
            items.iter().any(|item| item.label == "run"),
            "expected dependency-backed preview completion: {:?}",
            items
                .iter()
                .map(|item| item.label.clone())
                .collect::<Vec<_>>()
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn preview_completion_returns_current_document_form_templates() {
        let workspace_path = temp_workspace_path("preview_local_form_completion");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("main.abap"),
            "\
REPORT zpreview.
START-OF-SELECTION.
  PERFORM he",
        )
        .expect("main");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/main.abap");
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let preview = "\
REPORT zpreview.
START-OF-SELECTION.
  PERFORM he

FORM helper USING iv_value TYPE i.
ENDFORM.";
        assert!(stage_workspace_preview_snapshot(
            &mut state,
            &source_uri,
            2,
            preview
        ));

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&source_uri).expect("uri"),
                    },
                    position: Position {
                        line: 2,
                        character: 12,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "helper")
            .expect("perform completion item");
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(edit.new_text, "helper\n  USING\n    iv_value.");

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn preview_completion_returns_current_document_form_templates_before_endform() {
        let workspace_path = temp_workspace_path("preview_local_form_completion_before_endform");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("main.abap"),
            "\
REPORT zpreview.

FORM run.
  WRITE space.
ENDFORM.

FORM helper USING iv_value TYPE i.
ENDFORM.",
        )
        .expect("main");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/main.abap");
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let preview = "\
REPORT zpreview.

FORM run.
  PERFORM he
ENDFORM.

FORM helper USING iv_value TYPE i.
ENDFORM.";
        assert!(stage_workspace_preview_snapshot(
            &mut state,
            &source_uri,
            2,
            preview
        ));

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&source_uri).expect("uri"),
                    },
                    position: Position {
                        line: 3,
                        character: 12,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "helper")
            .expect("perform completion item");
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(edit.new_text, "helper\n  USING\n    iv_value.");

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn preview_completion_returns_current_document_form_templates_before_endform_with_crlf() {
        let workspace_path =
            temp_workspace_path("preview_local_form_completion_before_endform_crlf");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("main.abap"),
            "REPORT zpreview.\r\n\r\nFORM run.\r\n  WRITE space.\r\nENDFORM.\r\n\r\nFORM helper USING iv_value TYPE i.\r\nENDFORM.\r\n",
        )
        .expect("main");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/main.abap");
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let preview = "REPORT zpreview.\r\n\r\nFORM run.\r\n  PERFORM he\r\nENDFORM.\r\n\r\nFORM helper USING iv_value TYPE i.\r\nENDFORM.\r\n";
        assert!(stage_workspace_preview_snapshot(
            &mut state,
            &source_uri,
            2,
            preview
        ));

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&source_uri).expect("uri"),
                    },
                    position: Position {
                        line: 3,
                        character: 12,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "helper")
            .expect("perform completion item");
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(edit.new_text, "helper\n  USING\n    iv_value.");

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn completion_emits_function_module_call_snippet_before_endform_with_crlf() {
        let mut state = ServerState::default();
        state.client_capabilities.completion_snippet_support = true;
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_function_module_before_endform_crlf.abap")
                        .expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "FUNCTION z_demo_call\r\n  IMPORTING\r\n    iv_name TYPE string\r\n  EXPORTING\r\n    ev_text TYPE string\r\n  EXCEPTIONS\r\n    failed.\r\nENDFUNCTION.\r\n\r\nFORM run.\r\n  CALL FUNCTION 'z_de\r\nENDFORM.\r\n"
                        .to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(
                            "file:///completion_function_module_before_endform_crlf.abap",
                        )
                        .expect("uri"),
                    },
                    position: Position {
                        line: 10,
                        character: 21,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "z_demo_call")
            .expect("function module completion item");
        assert_eq!(item.kind, Some(lsp_types::CompletionItemKind::FUNCTION));
        assert_eq!(item.insert_text_format, Some(InsertTextFormat::SNIPPET));
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(
            edit.new_text,
            "z_demo_call'\n  EXPORTING\n    iv_name = ${1}\n  IMPORTING\n    ev_text = ${2}\n  EXCEPTIONS\n    failed = ${3:1}.$0"
        );
    }

    #[test]
    fn completion_emits_types_begin_template_snippet_inside_types_section() {
        let mut state = ServerState::default();
        state.client_capabilities.completion_snippet_support = true;
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_types_begin_template.abap")
                        .expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "REPORT zdemo.\n\nTYPES:\n  beg".to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_types_begin_template.abap")
                            .expect("uri"),
                    },
                    position: Position {
                        line: 3,
                        character: 5,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "BEGIN OF type_name")
            .expect("types begin template item");
        assert_eq!(item.kind, Some(lsp_types::CompletionItemKind::SNIPPET));
        assert_eq!(item.detail.as_deref(), Some("TYPES structure scaffold"));
        assert_eq!(item.insert_text_format, Some(InsertTextFormat::SNIPPET));
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(
            edit.new_text,
            "BEGIN OF ${1:type_name},\n  $0\nEND OF ${1:type_name}."
        );
        assert_eq!(
            edit.range,
            Range {
                start: Position {
                    line: 3,
                    character: 2,
                },
                end: Position {
                    line: 3,
                    character: 5,
                },
            }
        );
    }

    #[test]
    fn completion_emits_types_begin_template_snippet_for_same_line_begin_prefix() {
        let mut state = ServerState::default();
        state.client_capabilities.completion_snippet_support = true;
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_types_begin_template_same_line.abap")
                        .expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "TYPES: BEGIN".to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(
                            "file:///completion_types_begin_template_same_line.abap",
                        )
                        .expect("uri"),
                    },
                    position: Position {
                        line: 0,
                        character: 12,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "BEGIN OF type_name")
            .expect("types begin template item");
        assert_eq!(item.kind, Some(lsp_types::CompletionItemKind::SNIPPET));
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(
            edit.range,
            Range {
                start: Position {
                    line: 0,
                    character: 7,
                },
                end: Position {
                    line: 0,
                    character: 12,
                },
            }
        );
    }

    #[test]
    fn completion_emits_types_begin_template_snippet_when_begin_keyword_typing_starts() {
        let mut state = ServerState::default();
        state.client_capabilities.completion_snippet_support = true;
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_types_begin_template_typing_begin.abap")
                        .expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "TYPES: B".to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(
                            "file:///completion_types_begin_template_typing_begin.abap",
                        )
                        .expect("uri"),
                    },
                    position: Position {
                        line: 0,
                        character: 8,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "BEGIN OF type_name")
            .expect("types begin template item");
        assert_eq!(item.kind, Some(lsp_types::CompletionItemKind::SNIPPET));
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(
            edit.range,
            Range {
                start: Position {
                    line: 0,
                    character: 7,
                },
                end: Position {
                    line: 0,
                    character: 8,
                },
            }
        );
    }

    #[test]
    fn completion_emits_types_begin_template_snippet_after_same_line_begin_prefix_whitespace() {
        let mut state = ServerState::default();
        state.client_capabilities.completion_snippet_support = true;
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(
                        "file:///completion_types_begin_template_same_line_whitespace.abap",
                    )
                    .expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "TYPES: BEGIN ".to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(
                            "file:///completion_types_begin_template_same_line_whitespace.abap",
                        )
                        .expect("uri"),
                    },
                    position: Position {
                        line: 0,
                        character: 13,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "BEGIN OF type_name")
            .expect("types begin template item");
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(
            edit.range,
            Range {
                start: Position {
                    line: 0,
                    character: 7,
                },
                end: Position {
                    line: 0,
                    character: 13,
                },
            }
        );
    }

    #[test]
    fn completion_emits_local_class_definition_template_snippet() {
        let mut state = ServerState::default();
        state.client_capabilities.completion_snippet_support = true;
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_local_class_template.abap")
                        .expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "REPORT zdemo.\n\nlcl_demo".to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_local_class_template.abap")
                            .expect("uri"),
                    },
                    position: Position {
                        line: 2,
                        character: 8,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "lcl_demo")
            .expect("local class template item");
        assert_eq!(item.kind, Some(lsp_types::CompletionItemKind::SNIPPET));
        assert_eq!(item.detail.as_deref(), Some("Local class definition"));
        assert_eq!(item.insert_text_format, Some(InsertTextFormat::SNIPPET));
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(
            edit.new_text,
            "CLASS ${1:lcl_demo} DEFINITION.\n  PUBLIC SECTION.\n    $0\nENDCLASS.\n\nCLASS ${1:lcl_demo} IMPLEMENTATION.\nENDCLASS."
        );
    }

    #[test]
    fn completion_falls_back_to_plain_local_class_definition_template_without_snippet_support() {
        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_local_class_template_plain.abap")
                        .expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "REPORT zdemo.\n\nlcl".to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_local_class_template_plain.abap")
                            .expect("uri"),
                    },
                    position: Position {
                        line: 2,
                        character: 3,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "lcl_demo")
            .expect("local class template item");
        assert_eq!(item.kind, Some(lsp_types::CompletionItemKind::SNIPPET));
        assert_eq!(item.insert_text_format, None);
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(
            edit.new_text,
            "CLASS lcl_demo DEFINITION.\n  PUBLIC SECTION.\nENDCLASS.\n\nCLASS lcl_demo IMPLEMENTATION.\nENDCLASS."
        );
    }

    #[test]
    fn completion_emits_local_class_definition_template_between_abap_statements() {
        let mut state = ServerState::default();
        state.client_capabilities.completion_snippet_support = true;
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_local_class_template_between_statements.abap")
                        .expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "CLASS lo_epcis_builder DEFINITION.\n  PUBLIC SECTION.\n    METHODS build.\nENDCLASS.\n\nCLASS lo_epcis_builder IMPLEMENTATION.\n  METHOD build.\n    \n  ENDMETHOD.\nENDCLASS.\n\nlcl\n\nCLASS lcl_object_event DEFINITION.\n  PUBLIC SECTION.\n    METHODS add_to_epcis\n      CHANGING\n        co_epcis_builder TYPE REF TO lo_epcis_builder.\nENDCLASS.\n\nCLASS lcl_object_event IMPLEMENTATION.\n\nENDCLASS.".to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(
                            "file:///completion_local_class_template_between_statements.abap",
                        )
                        .expect("uri"),
                    },
                    position: Position {
                        line: 11,
                        character: 3,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "lcl_demo")
            .expect("local class template item");
        assert_eq!(item.kind, Some(lsp_types::CompletionItemKind::SNIPPET));
        assert_eq!(item.detail.as_deref(), Some("Local class definition"));
        assert_eq!(item.insert_text_format, Some(InsertTextFormat::SNIPPET));
    }

    #[test]
    fn completion_emits_method_definition_template_snippet_inside_class_definition() {
        let mut state = ServerState::default();
        state.client_capabilities.completion_snippet_support = true;
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_method_definition_template.abap")
                        .expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "CLASS lcl_demo DEFINITION.\n  PUBLIC SECTION.\n    meth\nENDCLASS."
                        .to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_method_definition_template.abap")
                            .expect("uri"),
                    },
                    position: Position {
                        line: 2,
                        character: 8,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "methods")
            .expect("method definition template item");
        assert_eq!(item.kind, Some(lsp_types::CompletionItemKind::SNIPPET));
        assert_eq!(item.detail.as_deref(), Some("Method definition"));
        assert_eq!(item.insert_text_format, Some(InsertTextFormat::SNIPPET));
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(
            edit.new_text,
            "METHODS ${1:method_name}\n  IMPORTING\n    ${2:iv_importing} TYPE ${3:i}\n  EXPORTING\n    ${4:ev_exporting} TYPE ${5:i}\n  CHANGING\n    ${6:cv_changing} TYPE ${7:i}\n  RECEIVING\n    VALUE(${8:rv_receiving}) TYPE ${9:i}\n  RETURNING\n    VALUE(${10:rv_returning}) TYPE ${11:i}.$0"
        );
    }

    #[test]
    fn completion_emits_local_test_class_definition_template_snippet() {
        let mut state = ServerState::default();
        state.client_capabilities.completion_snippet_support = true;
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_local_test_class_template.abap")
                        .expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "REPORT zdemo.\n\nltcl_demo".to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_local_test_class_template.abap")
                            .expect("uri"),
                    },
                    position: Position {
                        line: 2,
                        character: 9,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "ltcl_demo")
            .expect("local test class template item");
        assert_eq!(item.kind, Some(lsp_types::CompletionItemKind::SNIPPET));
        assert_eq!(item.detail.as_deref(), Some("Local test class definition"));
        assert_eq!(item.insert_text_format, Some(InsertTextFormat::SNIPPET));
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(
            edit.new_text,
            "CLASS ${1:ltcl_demo} DEFINITION FOR TESTING \n  DURATION SHORT\n  RISK LEVEL HARMLESS.\n\n  PRIVATE SECTION.\n    METHODS:\n      setup,\n      teardown,\n      ${2:test_demo} FOR TESTING.\nENDCLASS.\n\nCLASS ${1:ltcl_demo} IMPLEMENTATION.\n\n  METHOD setup.\n  ENDMETHOD.\n\n  METHOD teardown.\n  ENDMETHOD.\n\n  METHOD ${2:test_demo}.\n    cl_abap_unit_assert=>assert_equals(\n      act = ${3:abap_true} \n      exp = ${4:abap_true} \n    ).\n    $0\n  ENDMETHOD.\nENDCLASS."
        );
    }

    #[test]
    fn completion_falls_back_to_plain_local_test_class_definition_template_without_snippet_support()
    {
        let state = ServerState::default();
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_local_test_class_template_plain.abap")
                        .expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "REPORT zdemo.\n\nltcl".to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(
                            "file:///completion_local_test_class_template_plain.abap",
                        )
                        .expect("uri"),
                    },
                    position: Position {
                        line: 2,
                        character: 4,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "ltcl_demo")
            .expect("local test class template item");
        assert_eq!(item.kind, Some(lsp_types::CompletionItemKind::SNIPPET));
        assert_eq!(item.insert_text_format, None);
        let Some(lsp_types::CompletionTextEdit::Edit(edit)) = item.text_edit else {
            panic!("expected text edit");
        };
        assert_eq!(
            edit.new_text,
            "CLASS ltcl_demo DEFINITION FOR TESTING \n  DURATION SHORT\n  RISK LEVEL HARMLESS.\n\n  PRIVATE SECTION.\n    METHODS:\n      setup,\n      teardown,\n      test_demo FOR TESTING.\nENDCLASS.\n\nCLASS ltcl_demo IMPLEMENTATION.\n\n  METHOD setup.\n  ENDMETHOD.\n\n  METHOD teardown.\n  ENDMETHOD.\n\n  METHOD test_demo.\n    cl_abap_unit_assert=>assert_equals(\n      act = abap_true \n      exp = abap_true \n    ).\n  ENDMETHOD.\nENDCLASS."
        );
    }

    #[test]
    fn completion_still_emits_local_test_class_template_after_previous_one_exists() {
        let mut state = ServerState::default();
        state.client_capabilities.completion_snippet_support = true;
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_local_test_class_template_repeat.abap")
                        .expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: "CLASS ltcl_demo DEFINITION FOR TESTING.\n  DURATION SHORT.\n  RISK LEVEL HARMLESS.\n\n  PRIVATE SECTION.\n    METHODS test_demo FOR TESTING.\nENDCLASS.\n\nCLASS ltcl_demo IMPLEMENTATION.\n  METHOD test_demo.\n  ENDMETHOD.\nENDCLASS.\n\nltcl_demo".to_string(),
                },
            },
        );

        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(
                            "file:///completion_local_test_class_template_repeat.abap",
                        )
                        .expect("uri"),
                    },
                    position: Position {
                        line: 13,
                        character: 9,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        let item = items
            .into_iter()
            .find(|item| item.label == "ltcl_demo")
            .expect("local test class template item");
        assert_eq!(item.detail.as_deref(), Some("Local test class definition"));
        assert_eq!(item.insert_text_format, Some(InsertTextFormat::SNIPPET));
    }

    #[test]
    fn completion_returns_inherited_methods_after_super_arrow() {
        let state = ServerState::default();
        let text = "\
CLASS some_parent DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS inherited_method
      IMPORTING iv_value TYPE i.
ENDCLASS.

CLASS some_parent IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
  METHOD inherited_method.
  ENDMETHOD.
ENDCLASS.

CLASS some_child DEFINITION INHERITING FROM some_parent.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS some_child IMPLEMENTATION.
  METHOD constructor.
    super->i
  ENDMETHOD.
ENDCLASS.";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///completion_super.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("super->i"))
            .expect("super completion line");
        let character = line.1.find("i").expect("completion column") as u32 + 1;
        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///completion_super.abap").expect("uri"),
                    },
                    position: Position {
                        line: line.0 as u32,
                        character,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        assert!(
            items.iter().any(|item| item.label == "inherited_method"),
            "expected inherited parent method in completion items: {:?}",
            items
                .iter()
                .map(|item| item.label.clone())
                .collect::<Vec<_>>()
        );
        assert!(
            items
                .iter()
                .filter(|item| item.label == "inherited_method")
                .all(|item| item.kind == Some(lsp_types::CompletionItemKind::METHOD))
        );
    }

    #[test]
    fn semantic_tokens_and_hover_cover_template_interpolation_methods() {
        use lsp_types::SemanticTokenType;

        let state = ServerState::default();
        let text = "\
CLASS zcl_expr DEFINITION.
  PUBLIC SECTION.
    METHODS to_string
      RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_expr IMPLEMENTATION.
  METHOD to_string.
    rv_text = 'expr'.
  ENDMETHOD.
ENDCLASS.

DATA lo_expr TYPE REF TO zcl_expr.
DATA mv_op TYPE string.
DATA rv_text TYPE string.
rv_text = |({ lo_expr->to_string( ) } { mv_op })|.";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///template_hover.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let snapshot = state
            .cache
            .get("file:///template_hover.abap")
            .expect("snapshot");
        let tokens = sem_tokens::build_semantic_tokens(snapshot.as_ref());
        let legend = sem_tokens::semantic_tokens_legend();
        let method_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::METHOD)
            .expect("legend has method") as u32;
        let variable_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::VARIABLE)
            .expect("legend has variable") as u32;

        let template_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("rv_text = |("))
            .expect("template line");
        let method_col = template_line.1.find("to_string").expect("to_string column") as u32;
        let mv_op_col = template_line.1.find("mv_op").expect("mv_op column") as u32;

        assert_eq!(
            semantic_token_type_at(&tokens, template_line.0 as u32, method_col),
            Some(method_idx),
            "expected template method call to be highlighted as a method"
        );
        assert_eq!(
            semantic_token_type_at(&tokens, template_line.0 as u32, mv_op_col),
            Some(variable_idx),
            "expected template variable interpolation to be highlighted as a variable"
        );

        let hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///template_hover.abap").expect("uri"),
                    },
                    position: Position {
                        line: template_line.0 as u32,
                        character: method_col + 1,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("template method hover");
        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("METHODS to_string"));
        assert!(markup.value.contains("instance method of `lo_expr`"));
    }

    #[test]
    fn completion_returns_methods_inside_assignment_template_expression() {
        let state = ServerState::default();
        let text = "\
CLASS zcl_expr DEFINITION.
  PUBLIC SECTION.
    METHODS to_source.
    METHODS to_string
      RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_expr IMPLEMENTATION.
ENDCLASS.

DATA lo_expr TYPE REF TO zcl_expr.
DATA rv_text TYPE string.
rv_text = |value: { lo_expr->to_ }|.";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///template_completion.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let template_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("lo_expr->to_"))
            .expect("template completion line");
        let character =
            template_line.1.find("to_").expect("completion column") as u32 + "to_".len() as u32;
        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///template_completion.abap").expect("uri"),
                    },
                    position: Position {
                        line: template_line.0 as u32,
                        character,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");

        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        assert_eq!(
            items
                .iter()
                .map(|item| item.label.as_str())
                .collect::<Vec<_>>(),
            vec!["to_source", "to_string"]
        );
        assert!(
            items
                .iter()
                .all(|item| item.kind == Some(lsp_types::CompletionItemKind::METHOD))
        );
    }

    #[test]
    fn call_validation_diagnostics_use_warning_and_error_severities() {
        let state = ServerState::default();
        let text = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_req TYPE i.
    METHODS take_table IMPORTING it_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
  METHOD take_table.
  ENDMETHOD.
ENDCLASS.

DATA lo_demo TYPE REF TO lcl_demo.
DATA lv_text TYPE string.

START-OF-SELECTION.
  lo_demo->run(
    iv_req = 1
    iv_req = 2 ).
  lo_demo->run( iv_missing = 1 ).
  lo_demo->run( ).
  lo_demo->take_table( it_values = lv_text ).";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///call_diag_severity.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let snapshot = state
            .cache
            .get("file:///call_diag_severity.abap")
            .expect("snapshot");
        let diagnostics = build_lsp_diagnostics(snapshot.as_ref());

        assert!(diagnostics.iter().any(|diag| {
            diag.message.contains("duplicate named parameter 'iv_req'")
                && diag.severity == Some(DiagnosticSeverity::ERROR)
        }));
        assert!(diagnostics.iter().any(|diag| {
            diag.message
                .contains("unknown named parameter 'iv_missing'")
                && diag.severity == Some(DiagnosticSeverity::ERROR)
        }));
        assert!(diagnostics.iter().any(|diag| {
            diag.message.contains("missing required parameter 'iv_req'")
                && diag.severity == Some(DiagnosticSeverity::ERROR)
        }));
        assert!(diagnostics.iter().any(|diag| {
            diag.message.contains("it_values") && diag.severity == Some(DiagnosticSeverity::ERROR)
        }));
    }

    #[test]
    fn unknown_call_function_exception_is_reported_as_warning() {
        let state = ServerState::default();
        let dep_text = "\
FUNCTION BAL_DB_SAVE
  EXCEPTIONS
    log_not_found.
ENDFUNCTION.
";
        let main_text = "\
START-OF-SELECTION.
  CALL FUNCTION 'BAL_DB_SAVE'
    EXCEPTIONS
      error_message = 1.
";
        for (uri, text) in [
            ("file:///fm_exception_warning_dep.abap", dep_text),
            ("file:///fm_exception_warning_main.abap", main_text),
        ] {
            publish_open_document(
                &state,
                &DidOpenTextDocumentParams {
                    text_document: TextDocumentItem {
                        uri: Uri::from_str(uri).expect("uri"),
                        language_id: "abap".to_string(),
                        version: 1,
                        text: text.to_string(),
                    },
                },
            );
        }

        let snapshot = state
            .cache
            .get("file:///fm_exception_warning_main.abap")
            .expect("snapshot");
        let diagnostics = build_lsp_diagnostics(snapshot.as_ref());
        let diagnostic = diagnostics
            .iter()
            .find(|diag| diag.message.contains("unknown exception 'error_message'"))
            .expect("unknown exception diagnostic");
        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::WARNING));
    }

    #[test]
    fn incompatible_assignment_type_is_reported_as_error() {
        let state = ServerState::default();
        let text = "\
TYPES: BEGIN OF street_type,
         name TYPE string,
         no TYPE i,
       END OF street_type.

DATA lv_address TYPE street_type.

START-OF-SELECTION.
  lv_address = 2.";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///assignment_diag_severity.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let snapshot = state
            .cache
            .get("file:///assignment_diag_severity.abap")
            .expect("snapshot");
        let diagnostics = build_lsp_diagnostics(snapshot.as_ref());

        assert!(diagnostics.iter().any(|diag| {
            diag.message
                .contains("assignment target 'street_type' is incompatible with source 'i'")
                && diag.severity == Some(DiagnosticSeverity::ERROR)
        }));
    }

    #[test]
    fn method_navigation_and_completion_survive_call_validation() {
        let state = ServerState::default();
        let text = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_req TYPE i.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.

DATA lo_demo TYPE REF TO lcl_demo.
DATA lv_value TYPE i.

START-OF-SELECTION.
  lo_demo->run( iv_req = lv_value ).
  lo_demo->ru";
        publish_open_document(
            &state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str("file:///call_nav_completion.abap").expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: text.to_string(),
                },
            },
        );

        let call_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("lo_demo->run("))
            .expect("call line");
        let call_character = call_line.1.find("run").expect("run column") as u32 + 1;

        let hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///call_nav_completion.abap").expect("uri"),
                    },
                    position: Position {
                        line: call_line.0 as u32,
                        character: call_character,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("hover");
        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("METHODS run"));

        let definition_result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///call_nav_completion.abap").expect("uri"),
                    },
                    position: Position {
                        line: call_line.0 as u32,
                        character: call_character,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("definition");
        let GotoDefinitionResponse::Scalar(location) = definition_result else {
            panic!("expected scalar location");
        };
        assert_eq!(location.range.start.line, 6);
        assert_eq!(location.range.start.character, 9);

        let completion_line = text
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("lo_demo->ru"))
            .expect("completion line");
        let completion_character =
            completion_line.1.find("ru").expect("completion column") as u32 + 2;
        let completion = completion(
            &state,
            &CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///call_nav_completion.abap").expect("uri"),
                    },
                    position: Position {
                        line: completion_line.0 as u32,
                        character: completion_character,
                    },
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: None,
            },
        )
        .expect("completion");
        let CompletionResponse::Array(items) = completion else {
            panic!("expected array completion");
        };
        assert!(items.iter().any(|item| item.label == "run"));
    }

    #[test]
    fn dependency_symbol_index_extracts_only_addressable_symbols() {
        let symbols = extract_stored_dependency_symbols(
            "/sap/bc/adt/oo/classes/zcl_dep",
            "\
CLASS zcl_dep DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CONSTANTS gc_public TYPE i VALUE 1.
    CLASS-METHODS run IMPORTING iv_input TYPE i.
  PRIVATE SECTION.
    DATA mv_private TYPE i.
    METHODS hidden IMPORTING iv_hidden TYPE i.
ENDCLASS.

CLASS zcl_dep IMPLEMENTATION.
  METHOD run.
    DATA lv_local TYPE i.
  ENDMETHOD.

  METHOD hidden.
  ENDMETHOD.
ENDCLASS.
",
        );
        let names = symbols
            .iter()
            .map(|symbol| (symbol.symbol_name.as_str(), symbol.symbol_kind.as_str()))
            .collect::<Vec<_>>();

        assert!(names.contains(&("zcl_dep", "class")));
        assert!(names.contains(&("gc_public", "class-member")));
        assert!(names.contains(&("run", "class-member")));
        assert!(names.contains(&("run", "class-member-implementation")));
        assert!(!names.iter().any(|(name, _)| *name == "iv_input"));
        assert!(!names.iter().any(|(name, _)| *name == "lv_local"));
        assert!(!names.iter().any(|(name, _)| *name == "hidden"));
        assert!(!names.iter().any(|(_, kind)| kind.starts_with("builtin")));
    }

    #[test]
    fn dependency_store_definition_uses_closed_artifact_symbol_ranges() {
        let workspace_path = temp_workspace_path("closed_dependency_definition");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(workspace_path.join("src/reports/ZMAIN")).expect("report dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let source = "\
REPORT zmain.
START-OF-SELECTION.
  zcl_closed=>run( ).
  CALL METHOD zcl_closed=>run.
";
        fs::write(workspace_path.join("src/reports/ZMAIN/ZMAIN.abap"), source).expect("source");

        let workspace_uri = normalize_lsp_uri(&path_to_file_uri(&workspace_path));
        let source_uri = normalize_lsp_uri(&path_to_file_uri(
            &workspace_path.join("src/reports/ZMAIN/ZMAIN.abap"),
        ));
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let filler_lines = 37u32;
        let dependency_source = format!(
            "{}\
CLASS zcl_closed DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run.
ENDCLASS.

CLASS zcl_closed IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.
",
            "* filler\n".repeat(filler_lines as usize)
        );
        let workspace = state.workspaces.get(&workspace_uri).expect("workspace");
        let profile = workspace.dependency_profile.clone().expect("profile");
        let store = workspace_dependency_store(workspace).expect("store");
        store
            .put_artifacts(
                &profile,
                &[StoredArtifactInput {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_CLOSED".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_closed".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Closed class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: dependency_source.clone(),
                    fetched_at: "2026-04-29T00:00:00Z".to_string(),
                    symbols: extract_stored_dependency_symbols(
                        "/sap/bc/adt/oo/classes/zcl_closed",
                        &dependency_source,
                    ),
                }],
            )
            .expect("store artifact");

        let class_offset = source.find("zcl_closed").expect("class ref");
        let definition_result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&source_uri).expect("uri"),
                    },
                    position: offset_to_position(source, class_offset + 1).expect("position"),
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("class definition");
        let GotoDefinitionResponse::Scalar(class_location) = definition_result else {
            panic!("expected scalar location");
        };
        assert_eq!(class_location.range.start.line, filler_lines);
        assert_eq!(class_location.range.start.character, 6);
        assert!(snapshot_for_uri(&state, class_location.uri.as_str()).is_none());

        let method_offset = source.find("run( )").expect("method ref");
        let method_result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&source_uri).expect("uri"),
                    },
                    position: offset_to_position(source, method_offset + 1).expect("position"),
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("method definition");
        let GotoDefinitionResponse::Scalar(method_location) = method_result else {
            panic!("expected scalar location");
        };
        assert_eq!(method_location.range.start.line, filler_lines + 6);
        assert_eq!(method_location.range.start.character, 9);
        assert!(snapshot_for_uri(&state, class_location.uri.as_str()).is_none());

        let call_method_offset = source.rfind("run").expect("CALL METHOD ref");
        let call_method_result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&source_uri).expect("uri"),
                    },
                    position: offset_to_position(source, call_method_offset + 1).expect("position"),
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("CALL METHOD definition");
        let GotoDefinitionResponse::Scalar(call_method_location) = call_method_result else {
            panic!("expected scalar location");
        };
        assert_eq!(call_method_location.range.start.line, filler_lines + 6);
        assert_eq!(call_method_location.range.start.character, 9);
        assert!(snapshot_for_uri(&state, class_location.uri.as_str()).is_none());

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn centralized_dependency_store_definition_returns_virtual_dependency_uri() {
        let workspace_path = temp_workspace_path("centralized_dependency_definition");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(workspace_path.join("src/reports/ZMAIN")).expect("report dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let source = "REPORT zmain.\nDATA lo_demo TYPE REF TO cl_demo_remote.\n";
        fs::write(workspace_path.join("src/reports/ZMAIN/ZMAIN.abap"), source).expect("source");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = normalize_lsp_uri(&path_to_file_uri(
            &workspace_path.join("src/reports/ZMAIN/ZMAIN.abap"),
        ));
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "SABAPDEMOS".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "CL_DEMO_REMOTE".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/cl_demo_remote".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "\
CLASS cl_demo_remote DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS cl_demo_remote IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.
"
                    .to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifacts");

        assert!(
            build_remote_dependency_request(&mut state, &source_uri).is_none(),
            "stored dependency artifact should suppress a follow-up remote request"
        );

        let type_offset = source.find("cl_demo_remote").expect("type ref");
        let definition_result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&source_uri).expect("uri"),
                    },
                    position: offset_to_position(source, type_offset + 1).expect("position"),
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("definition");
        let GotoDefinitionResponse::Scalar(location) = definition_result else {
            panic!("expected scalar location");
        };
        assert_eq!(
            location.uri.scheme().map(|scheme| scheme.as_str()),
            Some("abapls-cache")
        );

        let dependency_text = read_dependency_document(
            &state,
            &ReadDependencyDocumentParams {
                uri: location.uri.to_string(),
            },
        )
        .expect("read dependency document")
        .expect("dependency document");
        assert!(
            dependency_text
                .source_text
                .to_ascii_lowercase()
                .contains("class cl_demo_remote definition"),
            "{}",
            dependency_text.source_text
        );

        let encoded_query_uri = location
            .uri
            .to_string()
            .replace("?workspace=", "?workspace%3D")
            .replace("&artifact=", "%26artifact%3D");
        let encoded_dependency_text = read_dependency_document(
            &state,
            &ReadDependencyDocumentParams {
                uri: encoded_query_uri,
            },
        )
        .expect("read encoded dependency document")
        .expect("encoded dependency document");
        assert!(
            encoded_dependency_text
                .source_text
                .to_ascii_lowercase()
                .contains("class cl_demo_remote definition"),
            "{}",
            encoded_dependency_text.source_text
        );

        let dependency_snapshot = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: location.uri.clone(),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: dependency_text.source_text,
                },
            },
        );
        assert!(!dependency_snapshot.is_dependency);
        assert_eq!(
            dependency_snapshot.object_name.as_deref(),
            Some("cl_demo_remote")
        );
        assert!(snapshot_for_uri(&state, location.uri.as_str()).is_some());

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn centralized_dependency_store_resolves_static_method_hover_and_definition() {
        let workspace_path = temp_workspace_path("centralized_dependency_static_method");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(workspace_path.join("src/reports/ZMAIN")).expect("report dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let source = "\
REPORT zmain.
START-OF-SELECTION.
  zattp_cl_ar_dm_object=>main_processing_pre_step( ).
";
        fs::write(workspace_path.join("src/reports/ZMAIN/ZMAIN.abap"), source).expect("source");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = normalize_lsp_uri(&path_to_file_uri(
            &workspace_path.join("src/reports/ZMAIN/ZMAIN.abap"),
        ));
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZATTP_CL_AR_DM_OBJECT".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zattp_cl_ar_dm_object".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "\
CLASS zattp_cl_ar_dm_object DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS main_processing_pre_step.
ENDCLASS.

CLASS zattp_cl_ar_dm_object IMPLEMENTATION.
  METHOD main_processing_pre_step.
  ENDMETHOD.
ENDCLASS.
"
                    .to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifacts");

        let method_offset = source.find("main_processing_pre_step").expect("method ref");
        let position = offset_to_position(source, method_offset + 1).expect("position");
        let definition_result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&source_uri).expect("uri"),
                    },
                    position,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("definition");
        let GotoDefinitionResponse::Scalar(location) = definition_result else {
            panic!("expected scalar location");
        };
        assert_eq!(
            location.uri.scheme().map(|scheme| scheme.as_str()),
            Some("abapls-cache")
        );
        assert_eq!(location.range.start.line, 6);
        assert_eq!(location.range.start.character, 9);

        let hover_result = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&source_uri).expect("uri"),
                    },
                    position,
                },
                work_done_progress_params: Default::default(),
            },
        );
        assert!(
            hover_result.is_some(),
            "expected hover for remote static method"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn centralized_dependency_store_resolves_namespaced_static_method_definition() {
        let workspace_path = temp_workspace_path("centralized_dependency_namespaced_static_method");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(workspace_path.join("src/reports/ZMAIN")).expect("report dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let source = "\
REPORT zmain.
START-OF-SELECTION.
  /sttp/cl_rr_ru_utilities=>get_safedata_key( ).
";
        fs::write(workspace_path.join("src/reports/ZMAIN/ZMAIN.abap"), source).expect("source");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let source_uri = normalize_lsp_uri(&path_to_file_uri(
            &workspace_path.join("src/reports/ZMAIN/ZMAIN.abap"),
        ));
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "/STTP/RU".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "/STTP/CL_RR_RU_UTILITIES".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/%2fsttp%2fcl_rr_ru_utilities".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote namespaced class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "\
CLASS /sttp/cl_rr_ru_utilities DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS get_safedata_key
      IMPORTING
        !iv_urltype TYPE /sttp/e_urltype
        !iv_type TYPE string
        !iv_uname TYPE uname OPTIONAL
      EXPORTING
        !ev_key TYPE /sttp/e_save_content_key.
ENDCLASS.

CLASS /sttp/cl_rr_ru_utilities IMPLEMENTATION.
  METHOD get_safedata_key.
  ENDMETHOD.
ENDCLASS.
"
                    .to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifacts");
        refresh_workspace(&mut state, &workspace_uri);

        let method_offset = source.find("get_safedata_key").expect("method ref");
        let position = offset_to_position(source, method_offset + 1).expect("position");
        let definition_result = definition(
            &state,
            &GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str(&source_uri).expect("uri"),
                    },
                    position,
                },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .expect("definition");
        let GotoDefinitionResponse::Scalar(location) = definition_result else {
            panic!("expected scalar location");
        };
        assert_eq!(
            location.uri.scheme().map(|scheme| scheme.as_str()),
            Some("abapls-cache")
        );
        let uri_text = location.uri.to_string();
        let path_text = uri_text.split('?').next().unwrap_or(uri_text.as_str());
        assert!(
            path_text.contains("sttp_cl_rr_ru_utilities.abap"),
            "{uri_text}"
        );
        assert!(
            !path_text.to_ascii_lowercase().contains("%2f"),
            "cache URI path must not contain encoded slashes that VS Code treats as path separators: {uri_text}"
        );
        assert!(
            read_dependency_document(&state, &ReadDependencyDocumentParams { uri: uri_text })
                .expect("read dependency document")
                .is_some()
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn opened_central_dependency_document_builds_transitive_remote_requests() {
        let workspace_path = temp_workspace_path("opened_central_dependency_follow_up");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(workspace_path.join("src/reports/ZMAIN")).expect("report dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        fs::write(
            workspace_path.join("src/reports/ZMAIN/ZMAIN.abap"),
            "REPORT zmain.\nDATA lo_demo TYPE REF TO zcl_dep.\n",
        )
        .expect("source");

        let workspace_uri = path_to_file_uri(&workspace_path);
        let mut state = ServerState::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: vec![DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_DEP".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_dep".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Remote class".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "\
CLASS zcl_dep DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS zcl_dep IMPLEMENTATION.
  METHOD run.
    zcl_missing=>run( ).
  ENDMETHOD.
ENDCLASS.
"
                    .to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                }],
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifacts");

        let dependency_uri = dependency_document_uri(&workspace_uri, 1, "zcl_dep");
        let dependency_text = read_dependency_document(
            &state,
            &ReadDependencyDocumentParams {
                uri: dependency_uri.clone(),
            },
        )
        .expect("read dependency document")
        .expect("dependency document");

        let opened = publish_open_document_mut(
            &mut state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Uri::from_str(&dependency_uri).expect("uri"),
                    language_id: "abap".to_string(),
                    version: 1,
                    text: dependency_text.source_text,
                },
            },
        );

        assert!(!opened.is_dependency);
        let request = build_remote_dependency_request(&mut state, &dependency_uri)
            .expect("opened dependency request");
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.name == "zcl_missing"),
            "request={request:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }
}
