#[cfg(test)]
mod perf_tests;
pub(crate) mod sem_tokens;

use std::collections::{HashMap, HashSet};
use std::str::FromStr;
use std::sync::Arc;

use abap_cache::{
    AnalysisSnapshot, DocumentInput, DocumentStore, OpenDocumentOverlay,
    UNKNOWN_SYMBOL_MODE_REMOTE, WorkspaceManifest, is_remote_lookup_candidate,
    load_workspace_documents, manifest_supports_remote_resolution, uri_starts_with_workspace,
};
use abap_symbols::{DiagnosticKind, ReferenceKind, SqlResolution};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionOptions, Diagnostic, DiagnosticSeverity,
    Documentation, GotoDefinitionResponse, Hover, HoverContents, HoverProviderCapability,
    InitializeResult, Location, MarkupContent, MarkupKind, OneOf, Position,
    PublishDiagnosticsParams, Range, SemanticTokens, SemanticTokensFullOptions,
    SemanticTokensOptions, SemanticTokensServerCapabilities, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Uri,
};
use serde::{Deserialize, Serialize};

pub use lsp_types::{
    CompletionParams, CompletionResponse, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
    GotoDefinitionParams, HoverParams, ReferenceParams, SemanticTokensParams,
};
pub use sem_tokens::build_semantic_tokens;
pub use serde;

pub const RESOLVE_REMOTE_DEPENDENCIES: &str = "abapls/resolveRemoteDependencies";
pub const REMOTE_DEPENDENCIES_UPDATED: &str = "abapls/remoteDependenciesUpdated";
pub const WORKSPACE_MANIFEST_UPDATED: &str = "abapls/workspaceManifestUpdated";
pub const DEPENDENCY_CACHE_CLEARED: &str = "abapls/dependencyCacheCleared";

#[derive(Debug)]
pub struct ServerState {
    pub cache: DocumentStore,
    pub workspaces: HashMap<String, WorkspaceState>,
    pub shutdown_requested: bool,
}

#[derive(Debug)]
pub struct WorkspaceState {
    pub root_uri: String,
    pub cache: DocumentStore,
    pub manifest: Option<WorkspaceManifest>,
    pub manifest_uri: String,
    pub manifest_error: Option<String>,
    pub open_documents: HashMap<String, OpenDocumentOverlay>,
    pub remote_resolution_seen: HashSet<String>,
}

impl Default for ServerState {
    fn default() -> Self {
        Self {
            cache: DocumentStore::default(),
            workspaces: HashMap::new(),
            shutdown_requested: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ServerConfig {
    pub name: &'static str,
    pub version: &'static str,
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
            manifest: None,
            manifest_uri: String::new(),
            manifest_error: None,
            open_documents: HashMap::new(),
            remote_resolution_seen: HashSet::new(),
        }
    }
}

impl ServerState {
    pub fn register_workspace_folder(&mut self, root_uri: impl Into<String>) {
        let root_uri = normalize_lsp_uri(&root_uri.into());
        self.workspaces
            .entry(root_uri.clone())
            .or_insert_with(|| WorkspaceState::new(root_uri));
    }

    pub fn workspace_for_uri(&self, uri: &str) -> Option<&WorkspaceState> {
        self.workspaces
            .values()
            .filter(|workspace| uri_starts_with_workspace(uri, &workspace.root_uri))
            .max_by_key(|workspace| workspace.root_uri.len())
    }

    pub fn workspace_for_uri_mut(&mut self, uri: &str) -> Option<&mut WorkspaceState> {
        let key = self
            .workspaces
            .values()
            .filter(|workspace| uri_starts_with_workspace(uri, &workspace.root_uri))
            .max_by_key(|workspace| workspace.root_uri.len())
            .map(|workspace| workspace.root_uri.clone())?;
        self.workspaces.get_mut(&key)
    }
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
    #[serde(rename = "unknownSymbolMode", default)]
    pub unknown_symbol_mode: Option<String>,
    #[serde(rename = "remoteRequestParallelism", default)]
    pub remote_request_parallelism: Option<usize>,
    #[serde(rename = "remoteRequestsPerSecond", default)]
    pub remote_requests_per_second: Option<usize>,
    pub candidates: Vec<RemoteDependencyCandidate>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RemoteDependenciesUpdatedParams {
    #[serde(rename = "workspaceUri")]
    pub workspace_uri: String,
    #[serde(rename = "sourceUri")]
    pub source_uri: String,
    pub fetched: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct WorkspaceManifestUpdatedParams {
    #[serde(rename = "workspaceUri")]
    pub workspace_uri: String,
}

/// Normalizes `file:` URIs so `DocumentStore` lookups stay stable (e.g. Windows `file:///C:/` vs `file:///c:/`).
pub fn normalize_lsp_uri(raw: &str) -> String {
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

fn cache_for_uri<'a>(state: &'a ServerState, uri: &str) -> &'a DocumentStore {
    state
        .workspace_for_uri(uri)
        .map(|workspace| &workspace.cache)
        .unwrap_or(&state.cache)
}

fn snapshot_for_uri(state: &ServerState, uri: &str) -> Option<Arc<AnalysisSnapshot>> {
    cache_for_uri(state, uri).get(uri)
}

fn rebuild_workspace_cache(
    workspace: &mut WorkspaceState,
) -> HashMap<Arc<str>, Arc<AnalysisSnapshot>> {
    let loaded = load_workspace_documents(&workspace.root_uri, &workspace.open_documents);
    workspace.manifest = loaded.manifest.clone();
    workspace.manifest_uri = loaded.manifest_uri.to_string();
    workspace.manifest_error = loaded.manifest_error.clone();
    let inputs: Vec<_> = loaded
        .documents
        .into_iter()
        .map(|document| DocumentInput {
            uri: document.uri,
            version: document.version,
            text: Arc::from(document.text),
            is_dependency: document.is_dependency,
            object_name: document.object_name,
        })
        .collect();
    workspace.cache.replace_all(inputs)
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

    let diagnostics = workspace
        .manifest_error
        .as_ref()
        .map(|message| {
            vec![Diagnostic {
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
            }]
        })
        .unwrap_or_default();

    Some(PublishDiagnosticsParams {
        uri: Uri::from_str(&workspace.manifest_uri).ok()?,
        diagnostics,
        version: None,
    })
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
    let uri = normalize_lsp_uri(params.text_document.uri.as_str());
    if let Some(workspace) = state.workspace_for_uri_mut(&uri) {
        workspace.open_documents.insert(
            uri.clone(),
            OpenDocumentOverlay {
                version: params.text_document.version,
                text: Arc::from(params.text_document.text.as_str()),
            },
        );
        let snapshots = rebuild_workspace_cache(workspace);
        return snapshots
            .get(uri.as_str())
            .cloned()
            .expect("opened workspace document should exist after rebuild");
    }
    state.cache.publish(
        uri,
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

pub fn publish_changed_document_mut(
    state: &mut ServerState,
    params: &DidChangeTextDocumentParams,
) -> Option<Arc<AnalysisSnapshot>> {
    let change = params.content_changes.last()?;
    let uri = normalize_lsp_uri(params.text_document.uri.as_str());
    if let Some(workspace) = state.workspace_for_uri_mut(&uri) {
        workspace.open_documents.insert(
            uri.clone(),
            OpenDocumentOverlay {
                version: params.text_document.version,
                text: Arc::from(change.text.as_str()),
            },
        );
        let snapshots = rebuild_workspace_cache(workspace);
        return snapshots.get(uri.as_str()).cloned();
    }
    Some(
        state
            .cache
            .publish(uri, params.text_document.version, &change.text),
    )
}

pub fn refresh_workspace(
    state: &mut ServerState,
    workspace_uri: &str,
) -> Vec<Arc<AnalysisSnapshot>> {
    let workspace_uri = normalize_lsp_uri(workspace_uri);
    let Some(workspace) = state.workspaces.get_mut(&workspace_uri) else {
        return Vec::new();
    };
    rebuild_workspace_cache(workspace).into_values().collect()
}

pub fn handle_workspace_manifest_updated(
    state: &mut ServerState,
    params: &WorkspaceManifestUpdatedParams,
) -> Vec<Arc<AnalysisSnapshot>> {
    refresh_workspace(state, &params.workspace_uri)
}

pub fn handle_dependency_cache_cleared(
    state: &mut ServerState,
    params: &WorkspaceManifestUpdatedParams,
) -> Vec<Arc<AnalysisSnapshot>> {
    let workspace_uri = normalize_lsp_uri(&params.workspace_uri);
    if let Some(workspace) = state.workspaces.get_mut(&workspace_uri) {
        workspace.remote_resolution_seen.clear();
    }
    refresh_workspace(state, &workspace_uri)
}

pub fn handle_remote_dependencies_updated(
    state: &mut ServerState,
    params: &RemoteDependenciesUpdatedParams,
) -> Vec<Arc<AnalysisSnapshot>> {
    refresh_workspace(state, &params.workspace_uri)
}

pub fn collect_remote_dependency_candidates(
    snapshot: &AnalysisSnapshot,
) -> Vec<RemoteDependencyCandidate> {
    let mut deduped = HashMap::<String, RemoteDependencyCandidate>::new();
    let semantic = snapshot.symbols.semantic();

    for edge in snapshot
        .symbols
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
        let kind = match reference.kind {
            ReferenceKind::Include => continue,
            ReferenceKind::StaticTarget => "static",
            ReferenceKind::TypeRef => "type",
            ReferenceKind::MessageClass => "message-class",
            ReferenceKind::Identifier | ReferenceKind::RoutineCall => "symbol",
        };
        if reference.resolution.is_some()
            || !is_remote_lookup_candidate(reference.name.as_ref(), kind)
        {
            continue;
        }
        insert_remote_candidate(
            &mut deduped,
            RemoteDependencyCandidate {
                name: reference.name.to_string(),
                kind: kind.to_string(),
            },
        );
    }

    for sql_ref in semantic.sql().name_refs() {
        if sql_ref.kind == abap_symbols::SqlNameRefKind::Source
            && sql_ref.resolution == SqlResolution::External
            && is_remote_lookup_candidate(sql_ref.name.as_ref(), "type")
        {
            insert_remote_candidate(
                &mut deduped,
                RemoteDependencyCandidate {
                    name: sql_ref.name.to_string(),
                    kind: "type".to_string(),
                },
            );
        }
    }

    deduped.into_values().collect()
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
        "static" => 3,
        "type" => 2,
        _ => 1,
    }
}

pub fn build_remote_dependency_request(
    state: &mut ServerState,
    source_uri: &str,
) -> Option<RemoteDependencyResolveParams> {
    let source_uri = normalize_lsp_uri(source_uri);
    let workspace = state.workspace_for_uri_mut(&source_uri)?;
    if !manifest_supports_remote_resolution(workspace.manifest.as_ref()) {
        return None;
    }
    let snapshot = workspace.cache.get(&source_uri)?;
    if !snapshot.parse.errors.is_empty() {
        return None;
    }

    let mut candidates = Vec::new();
    for candidate in collect_remote_dependency_candidates(snapshot.as_ref()) {
        let key = remote_candidate_key(&candidate);
        if workspace.remote_resolution_seen.insert(key) {
            candidates.push(candidate);
        }
    }
    if candidates.is_empty() {
        return None;
    }

    Some(RemoteDependencyResolveParams {
        workspace_uri: workspace.root_uri.clone(),
        source_uri,
        unknown_symbol_mode: workspace
            .manifest
            .as_ref()
            .map(|manifest| manifest.resolution.unknown_symbol_mode.clone())
            .or(Some(UNKNOWN_SYMBOL_MODE_REMOTE.to_string())),
        remote_request_parallelism: workspace
            .manifest
            .as_ref()
            .map(|manifest| manifest.resolution.remote_request_parallelism),
        remote_requests_per_second: workspace
            .manifest
            .as_ref()
            .map(|manifest| manifest.resolution.remote_requests_per_second),
        candidates,
    })
}

pub fn build_remote_dependency_requests_for_workspace(
    state: &mut ServerState,
    workspace_uri: &str,
) -> Vec<RemoteDependencyResolveParams> {
    let workspace_uri = normalize_lsp_uri(workspace_uri);
    let Some(workspace) = state.workspaces.get(&workspace_uri) else {
        return Vec::new();
    };
    let mut uris = workspace.cache.uris();
    uris.sort();
    let mut requests = Vec::new();
    for uri in uris {
        if let Some(request) = build_remote_dependency_request(state, uri.as_ref()) {
            requests.push(request);
        }
    }
    requests
}

fn semantic_diagnostic_severity(kind: DiagnosticKind) -> DiagnosticSeverity {
    match kind {
        DiagnosticKind::DuplicateDeclaration | DiagnosticKind::ShadowedSymbol => {
            DiagnosticSeverity::WARNING
        }
        DiagnosticKind::UnverifiedOpenSqlSource => DiagnosticSeverity::WARNING,
        DiagnosticKind::UnresolvedReference
        | DiagnosticKind::UnresolvedInclude
        | DiagnosticKind::IncludeCycle
        | DiagnosticKind::WrongNamespace
        | DiagnosticKind::UnknownField
        | DiagnosticKind::InvalidBuiltinNamedArgument
        | DiagnosticKind::InvalidPerformCall
        | DiagnosticKind::MissingSuperConstructorCall
        | DiagnosticKind::InvalidOpenSqlIntoTarget => DiagnosticSeverity::ERROR,
    }
}

pub fn build_lsp_diagnostics(snapshot: &AnalysisSnapshot) -> Vec<Diagnostic> {
    let text = snapshot.text.as_ref();
    let mut out: Vec<Diagnostic> = snapshot
        .parse
        .errors
        .iter()
        .filter_map(|err| {
            Some(Diagnostic {
                range: byte_range_to_lsp_range(text, err.range.clone())?,
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
        .collect();
    for diag_inner in &snapshot.symbols.diagnostics {
        let Some(range) = byte_range_to_lsp_range(text, diag_inner.range.clone()) else {
            continue;
        };
        out.push(Diagnostic {
            range,
            severity: Some(semantic_diagnostic_severity(diag_inner.kind)),
            code: None,
            code_description: None,
            source: Some("abap-symbols".to_owned()),
            message: diag_inner.message.clone(),
            related_information: None,
            tags: None,
            data: None,
        });
    }
    out.sort_by(|a, b| {
        a.range
            .start
            .line
            .cmp(&b.range.start.line)
            .then(a.range.start.character.cmp(&b.range.start.character))
    });
    out
}

pub fn publish_diagnostics_params(snapshot: &AnalysisSnapshot) -> PublishDiagnosticsParams {
    let uri: Uri = snapshot
        .uri
        .as_ref()
        .parse()
        .expect("cached document URI must be a valid URL");
    PublishDiagnosticsParams {
        uri,
        diagnostics: build_lsp_diagnostics(snapshot),
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
    let offset = position_to_offset(
        snapshot.text.as_ref(),
        params.text_document_position_params.position,
    )?;
    if let Some(component) = snapshot.hovered_component_at(offset) {
        return structured_field_hover(&snapshot, component);
    }
    if let Some(argument) = snapshot.hovered_perform_argument_at(offset) {
        return resolved_symbol_hover(&snapshot, argument);
    }
    if let Some(named_argument) = snapshot.hovered_named_argument_at(offset) {
        return resolved_symbol_hover(&snapshot, named_argument);
    }
    if let Some(sql_ref) = snapshot.hovered_sql_name_ref_at(offset) {
        return resolved_symbol_hover(&snapshot, sql_ref);
    }
    let symbol = snapshot.hovered_resolved_symbol_at(offset)?;
    resolved_symbol_hover(&snapshot, symbol)
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
    let offset = position_to_offset(
        snapshot.text.as_ref(),
        params.text_document_position_params.position,
    )?;
    let target = snapshot.definition_at(offset)?;
    let target_snapshot = if target.uri.as_ref() == snapshot.uri.as_ref() {
        Arc::clone(&snapshot)
    } else {
        snapshot_for_uri(state, target.uri.as_ref())?
    };
    let uri: Uri = target
        .uri
        .as_ref()
        .parse()
        .expect("cached document URI must be a valid URL");
    let range = byte_range_to_lsp_range(target_snapshot.text.as_ref(), target.range)?;
    Some(GotoDefinitionResponse::Scalar(Location { uri, range }))
}

pub fn references(state: &ServerState, params: &ReferenceParams) -> Option<Vec<Location>> {
    let uri = normalize_lsp_uri(params.text_document_position.text_document.uri.as_str());
    let snapshot = snapshot_for_uri(state, &uri)?;
    let offset = position_to_offset(
        snapshot.text.as_ref(),
        params.text_document_position.position,
    )?;
    let references =
        cache_for_uri(state, &uri).references(&uri, offset, params.context.include_declaration)?;
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
        let range = byte_range_to_lsp_range(target_snapshot.text.as_ref(), reference.range)?;
        locations.push(Location { uri, range });
    }
    Some(locations)
}

fn resolved_symbol_hover(
    snapshot: &AnalysisSnapshot,
    info: abap_cache::HoveredSymbolInfo,
) -> Option<Hover> {
    let range = byte_range_to_lsp_range(snapshot.text.as_ref(), info.range)?;
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
    let range = byte_range_to_lsp_range(snapshot.text.as_ref(), component.range.clone())?;
    let is_method = matches!(component.kind, abap_cache::HoveredComponentKind::Method);
    let mut lines = vec![format!("`{}`", component.field_name)];
    match &component.kind {
        abap_cache::HoveredComponentKind::Scalar => lines.push(scalar_component_summary(
            component.field_owner_structure_name.as_ref(),
            component.field_name.as_ref(),
        )),
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
    }
    if let Some(declared_type) = component.declared_type {
        lines.push(format!("declared as `{}`", declared_type));
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
    let offset = position_to_offset(
        snapshot.text.as_ref(),
        params.text_document_position.position,
    )?;
    let completion = snapshot.selector_completion_at(offset)?;
    let range = byte_range_to_lsp_range(snapshot.text.as_ref(), completion.replace_range)?;
    let items = completion
        .items
        .into_iter()
        .map(|item| {
            let (detail, documentation) = completion_item_metadata(&item);
            CompletionItem {
                label: item.name.to_string(),
                kind: Some(match item.kind {
                    abap_cache::HoveredComponentKind::Method => CompletionItemKind::METHOD,
                    abap_cache::HoveredComponentKind::Interface => CompletionItemKind::INTERFACE,
                    abap_cache::HoveredComponentKind::Attribute
                    | abap_cache::HoveredComponentKind::Scalar
                    | abap_cache::HoveredComponentKind::Structured { .. } => {
                        CompletionItemKind::FIELD
                    }
                }),
                detail,
                documentation,
                text_edit: Some(lsp_types::CompletionTextEdit::Edit(TextEdit {
                    range,
                    new_text: item.name.to_string(),
                })),
                ..CompletionItem::default()
            }
        })
        .collect();
    Some(CompletionResponse::Array(items))
}

pub fn semantic_tokens(
    state: &ServerState,
    params: &SemanticTokensParams,
) -> Option<SemanticTokens> {
    let uri = normalize_lsp_uri(params.text_document.uri.as_str());
    let snapshot = snapshot_for_uri(state, &uri)?;
    Some(sem_tokens::build_semantic_tokens(snapshot.as_ref()))
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
                trigger_characters: Some(vec!["-".to_string(), ">".to_string(), "~".to_string()]),
                ..CompletionOptions::default()
            }),
            definition_provider: Some(OneOf::Left(true)),
            references_provider: Some(OneOf::Left(true)),
            semantic_tokens_provider: Some(
                SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                    legend: sem_tokens::semantic_tokens_legend(),
                    full: Some(SemanticTokensFullOptions::Bool(true)),
                    range: None,
                    work_done_progress_options: Default::default(),
                }),
            ),
            ..ServerCapabilities::default()
        },
    }
}

fn byte_range_to_lsp_range(text: &str, range: std::ops::Range<usize>) -> Option<Range> {
    Some(Range {
        start: offset_to_position(text, range.start)?,
        end: offset_to_position(text, range.end)?,
    })
}

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

fn position_to_offset(text: &str, position: Position) -> Option<usize> {
    let mut line_start = 0usize;
    for _ in 0..position.line {
        let rel = text[line_start..].find('\n')?;
        line_start += rel + 1;
    }
    let line_end = text[line_start..]
        .find('\n')
        .map(|rel| line_start + rel)
        .unwrap_or(text.len());
    let line_text = text[line_start..line_end]
        .strip_suffix('\r')
        .unwrap_or(&text[line_start..line_end]);
    let mut utf16_units = 0u32;
    for (idx, ch) in line_text.char_indices() {
        if utf16_units == position.character {
            return Some(line_start + idx);
        }
        utf16_units += ch.len_utf16() as u32;
        if utf16_units > position.character {
            return None;
        }
    }
    (utf16_units == position.character).then_some(line_start + line_text.len())
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

#[cfg(test)]
mod tests {
    use abap_cache::{DocumentStore, path_to_file_uri};
    use std::fs;
    use std::path::PathBuf;
    use std::str::FromStr;
    use std::time::{SystemTime, UNIX_EPOCH};

    use lsp_types::{
        DidChangeTextDocumentParams, DidOpenTextDocumentParams, Documentation,
        GotoDefinitionResponse, HoverContents, Position, TextDocumentContentChangeEvent,
        TextDocumentIdentifier, TextDocumentItem, TextDocumentPositionParams, Uri,
        VersionedTextDocumentIdentifier,
    };

    use crate::sem_tokens;

    use super::{
        CompletionParams, CompletionResponse, DEPENDENCY_CACHE_CLEARED, GotoDefinitionParams,
        HoverParams, REMOTE_DEPENDENCIES_UPDATED, RESOLVE_REMOTE_DEPENDENCIES, ReferenceParams,
        ServerState, WORKSPACE_MANIFEST_UPDATED, WorkspaceManifestUpdatedParams,
        build_lsp_diagnostics, build_remote_dependency_request,
        build_remote_dependency_requests_for_workspace, collect_remote_dependency_candidates,
        completion, definition, handle_dependency_cache_cleared,
        handle_remote_dependencies_updated, hover, initialize_result, normalize_lsp_uri,
        publish_changed_document, publish_open_document, publish_open_document_mut, references,
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
    fn initialize_result_exposes_server_capabilities() {
        let result = initialize_result(&Default::default());

        assert!(result.capabilities.text_document_sync.is_some());
        assert!(result.capabilities.semantic_tokens_provider.is_some());
        assert!(matches!(
            result.capabilities.definition_provider,
            Some(lsp_types::OneOf::Left(true))
        ));
        assert!(matches!(
            result.capabilities.references_provider,
            Some(lsp_types::OneOf::Left(true))
        ));
        assert!(result.server_info.is_some());
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
        assert_eq!(DEPENDENCY_CACHE_CLEARED, "abapls/dependencyCacheCleared");
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

        assert!(candidates.iter().any(|candidate| {
            candidate.name == "00" && candidate.kind == "message-class"
        }));
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
unknown_symbol_mode = "remote"
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
    fn log_mode_still_builds_remote_dependency_requests() {
        let workspace_path = temp_workspace_path("log_mode_candidates");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[resolution]
dependency_mode = "remote-on-demand"
unknown_symbol_mode = "log"
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
        assert_eq!(request.unknown_symbol_mode.as_deref(), Some("log"));
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.name == "boolean")
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn workspace_manifest_refresh_enables_remote_dependency_requests() {
        let workspace_path = temp_workspace_path("manifest_refresh");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[resolution]
dependency_mode = "remote-on-demand"
unknown_symbol_mode = "remote"
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

        let _ = handle_dependency_cache_cleared(
            &mut state,
            &WorkspaceManifestUpdatedParams {
                workspace_uri: workspace_uri.clone(),
            },
        );
        let request =
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/main.abap"))
                .expect("remote request after cache clear");
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.kind == "static")
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn dependency_cache_clear_refreshes_workspace_and_reissues_requests() {
        let workspace_path = temp_workspace_path("dependency_cache_clear_refresh");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[resolution]
dependency_mode = "remote-on-demand"
unknown_symbol_mode = "remote"
"#,
        )
        .expect("manifest");
        let workspace_uri = path_to_file_uri(&workspace_path);

        let dependency_dir = workspace_path
            .join(".abapls")
            .join("cache")
            .join("dependencies")
            .join("global-class");
        fs::create_dir_all(&dependency_dir).expect("dependency dir");
        fs::write(
            dependency_dir.join("ZCL_REMOTE_DEMO.abap"),
            "CLASS zcl_remote_demo DEFINITION.\nENDCLASS.\nCLASS zcl_remote_demo IMPLEMENTATION.\nENDCLASS.\n",
        )
        .expect("dependency file");

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

        assert!(
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/main.abap"))
                .is_none()
        );

        fs::remove_dir_all(workspace_path.join(".abapls").join("cache")).expect("clear cache dir");

        let snapshots = handle_dependency_cache_cleared(
            &mut state,
            &WorkspaceManifestUpdatedParams {
                workspace_uri: workspace_uri.clone(),
            },
        );
        assert!(
            !snapshots.is_empty(),
            "expected workspace refresh after cache clear"
        );

        let request =
            build_remote_dependency_request(&mut state, &format!("{workspace_uri}/main.abap"))
                .expect("remote request after cache clear");
        assert!(
            request
                .candidates
                .iter()
                .any(|candidate| candidate.name == "zcl_remote_demo")
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

[resolution]
dependency_mode = "remote-on-demand"
unknown_symbol_mode = "remote"
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
    fn refreshed_dependency_files_can_trigger_follow_up_remote_requests() {
        let workspace_path = temp_workspace_path("dependency_of_dependency");
        let dependency_dir = workspace_path
            .join(".abapls")
            .join("cache")
            .join("dependencies")
            .join("global-class");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[resolution]
dependency_mode = "remote-on-demand"
unknown_symbol_mode = "remote"
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

        fs::create_dir_all(&dependency_dir).expect("dependency dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[resolution]
dependency_mode = "remote-on-demand"
unknown_symbol_mode = "remote"

[[unit]]
name = "ZCL_FIRST"
kind = "global-class"
root_file = ".abapls/cache/dependencies/global-class/ZCL_FIRST.abap"
adt_uri = "/sap/bc/adt/oo/classes/zcl_first"

[[unit.member]]
role = "dependency"
file = ".abapls/cache/dependencies/global-class/ZCL_FIRST.abap"
object_name = "ZCL_FIRST"
adt_uri = "/sap/bc/adt/oo/classes/zcl_first"
"#,
        )
        .expect("updated manifest");
        fs::write(
            dependency_dir.join("ZCL_FIRST.abap"),
            "CLASS zcl_first DEFINITION.\n  PUBLIC SECTION.\n    CLASS-METHODS create RETURNING VALUE(ro_inst) TYPE REF TO zcl_second.\nENDCLASS.\nCLASS zcl_first IMPLEMENTATION.\n  METHOD create.\n  ENDMETHOD.\nENDCLASS.\n",
        )
        .expect("dependency file");

        let _ = handle_remote_dependencies_updated(
            &mut state,
            &super::RemoteDependenciesUpdatedParams {
                workspace_uri: workspace_uri.clone(),
                source_uri: format!("{workspace_uri}/main.abap"),
                fetched: vec!["ZCL_FIRST".to_string()],
            },
        );

        let follow_up = build_remote_dependency_requests_for_workspace(&mut state, &workspace_uri);
        assert!(follow_up.iter().any(|request| {
            request.source_uri.ends_with("ZCL_FIRST.abap")
                && request
                    .candidates
                    .iter()
                    .any(|candidate| candidate.name == "zcl_second")
        }));

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn dependency_private_implementation_references_do_not_trigger_follow_up_remote_requests() {
        let workspace_path = temp_workspace_path("dependency_private_impl");
        let dependency_dir = workspace_path
            .join(".abapls")
            .join("cache")
            .join("dependencies")
            .join("global-class");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[resolution]
dependency_mode = "remote-on-demand"
unknown_symbol_mode = "remote"
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

        fs::create_dir_all(&dependency_dir).expect("dependency dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[resolution]
dependency_mode = "remote-on-demand"
unknown_symbol_mode = "remote"

[[unit]]
name = "ZCL_FIRST"
kind = "global-class"
root_file = ".abapls/cache/dependencies/global-class/ZCL_FIRST.abap"
adt_uri = "/sap/bc/adt/oo/classes/zcl_first"

[[unit.member]]
role = "dependency"
file = ".abapls/cache/dependencies/global-class/ZCL_FIRST.abap"
object_name = "ZCL_FIRST"
adt_uri = "/sap/bc/adt/oo/classes/zcl_first"
"#,
        )
        .expect("updated manifest");
        fs::write(
            dependency_dir.join("ZCL_FIRST.abap"),
            "CLASS zcl_first DEFINITION.\n  PUBLIC SECTION.\n    CLASS-METHODS create RETURNING VALUE(ro_inst) TYPE REF TO zcl_first.\n  PRIVATE SECTION.\n    CLASS-METHODS hidden.\nENDCLASS.\nCLASS zcl_first IMPLEMENTATION.\n  METHOD create.\n    hidden( ).\n  ENDMETHOD.\n  METHOD hidden.\n    DATA lo_hidden TYPE REF TO zcl_second.\n  ENDMETHOD.\nENDCLASS.\n",
        )
        .expect("dependency file");

        let _ = handle_remote_dependencies_updated(
            &mut state,
            &super::RemoteDependenciesUpdatedParams {
                workspace_uri: workspace_uri.clone(),
                source_uri: format!("{workspace_uri}/main.abap"),
                fetched: vec!["ZCL_FIRST".to_string()],
            },
        );

        let follow_up = build_remote_dependency_requests_for_workspace(&mut state, &workspace_uri);
        assert!(!follow_up.iter().any(|request| {
            request.source_uri.ends_with("ZCL_FIRST.abap")
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
        let dependency_dir = workspace_path
            .join(".abapls")
            .join("cache")
            .join("dependencies")
            .join("global-class");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[resolution]
dependency_mode = "remote-on-demand"
unknown_symbol_mode = "remote"
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

        fs::create_dir_all(&dependency_dir).expect("dependency dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[resolution]
dependency_mode = "remote-on-demand"
unknown_symbol_mode = "remote"

[[unit]]
name = "ZCL_FIRST"
kind = "global-class"
root_file = ".abapls/cache/dependencies/global-class/ZCL_FIRST.abap"
adt_uri = "/sap/bc/adt/oo/classes/zcl_first"

[[unit.member]]
role = "dependency"
file = ".abapls/cache/dependencies/global-class/ZCL_FIRST.abap"
object_name = "ZCL_FIRST"
adt_uri = "/sap/bc/adt/oo/classes/zcl_first"
"#,
        )
        .expect("updated manifest");
        fs::write(
            dependency_dir.join("ZCL_FIRST.abap"),
            "CLASS zcl_first DEFINITION.\n  PUBLIC SECTION.\n    METHODS run.\nENDCLASS.\nCLASS zcl_first IMPLEMENTATION.\n  METHOD run.\n    INCLUDE zinc_method.\n  ENDMETHOD.\nENDCLASS.\n",
        )
        .expect("dependency file");

        let _ = handle_remote_dependencies_updated(
            &mut state,
            &super::RemoteDependenciesUpdatedParams {
                workspace_uri: workspace_uri.clone(),
                source_uri: format!("{workspace_uri}/main.abap"),
                fetched: vec!["ZCL_FIRST".to_string()],
            },
        );

        let dependency_uri =
            normalize_lsp_uri(&path_to_file_uri(&dependency_dir.join("ZCL_FIRST.abap")));
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
                request
                    .source_uri
                    .to_ascii_lowercase()
                    .ends_with("zcl_first.abap")
                    && request.candidates.iter().any(|candidate| {
                        candidate.kind == "include" && candidate.name == "zinc_method"
                    })
            }),
            "follow_up={follow_up:#?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn dependency_cache_files_are_loaded_even_without_manifest_unit_entries() {
        let workspace_path = temp_workspace_path("dependency_cache_scan");
        let dependency_dir = workspace_path
            .join(".abapls")
            .join("cache")
            .join("dependencies")
            .join("ddic-structure");
        fs::create_dir_all(&dependency_dir).expect("dependency dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[resolution]
dependency_mode = "remote-on-demand"
unknown_symbol_mode = "remote"
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
        let snapshot = publish_open_document_mut(
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

        let diagnostics = build_lsp_diagnostics(&snapshot);
        assert!(
            !diagnostics
                .iter()
                .any(|diag| diag.message.contains("unknown type"))
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
                .contains("```abap\nFORM f\n  CHANGING\n    cv TYPE string\n```"),
            "{}",
            markup.value
        );
        assert!(markup.value.contains("parameter of FORM `f`"));
    }

    #[test]
    fn hover_returns_form_parameter_metadata_at_perform_statement_and_declaration() {
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

        let decl_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///perform_hover.abap").expect("uri"),
                    },
                    position: Position {
                        line: 0,
                        character: 20,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("declaration hover");
        let HoverContents::Markup(decl_markup) = decl_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            decl_markup.value.contains(
                "```abap\nFORM f\n  USING\n    VALUE(iv_input) TYPE i\n  CHANGING\n    cv_text TYPE string\n```"
            ),
            "{}",
            decl_markup.value
        );
        assert!(decl_markup.value.contains("parameter of FORM `f`"));

        let call_hover = hover(
            &state,
            &HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier {
                        uri: Uri::from_str("file:///perform_hover.abap").expect("uri"),
                    },
                    position: Position {
                        line: 7,
                        character: 18,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        )
        .expect("perform hover");
        let HoverContents::Markup(call_markup) = call_hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            call_markup.value.contains("`iv_input`"),
            "{}",
            call_markup.value
        );
        assert!(
            call_markup.value.contains(
                "```abap\nFORM f\n  USING\n    VALUE(iv_input) TYPE i\n  CHANGING\n    cv_text TYPE string\n```"
            ),
            "{}",
            call_markup.value
        );
        assert!(call_markup.value.contains("parameter of FORM `f`"));
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
        let line_6_start = text.rmatch_indices('\n').nth(0).expect("last newline").0 + 1;
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
}
