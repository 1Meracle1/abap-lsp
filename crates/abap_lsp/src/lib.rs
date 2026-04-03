pub(crate) mod sem_tokens;

use std::sync::Arc;

use abap_cache::{AnalysisSnapshot, DocumentStore};
use abap_symbols::DiagnosticKind;
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionOptions, Diagnostic, DiagnosticSeverity,
    Documentation, Hover, HoverContents, HoverProviderCapability, InitializeResult, MarkupContent,
    MarkupKind, OneOf, Position, PublishDiagnosticsParams, Range, SemanticTokens,
    SemanticTokensFullOptions, SemanticTokensOptions, SemanticTokensServerCapabilities,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Uri,
};
use serde::{Deserialize, Serialize};

pub use lsp_types::{
    CompletionParams, CompletionResponse, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
    HoverParams, SemanticTokensParams,
};
pub use serde;

pub const RESOLVE_REMOTE_DEPENDENCIES: &str = "abapls/resolveRemoteDependencies";
pub const REMOTE_DEPENDENCIES_UPDATED: &str = "abapls/remoteDependenciesUpdated";
pub const WORKSPACE_MANIFEST_UPDATED: &str = "abapls/workspaceManifestUpdated";
pub const DEPENDENCY_CACHE_CLEARED: &str = "abapls/dependencyCacheCleared";

#[derive(Debug)]
pub struct ServerState {
    pub cache: DocumentStore,
    pub shutdown_requested: bool,
}

impl Default for ServerState {
    fn default() -> Self {
        Self {
            cache: DocumentStore::default(),
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

fn semantic_diagnostic_severity(kind: DiagnosticKind) -> DiagnosticSeverity {
    match kind {
        DiagnosticKind::DuplicateDeclaration | DiagnosticKind::ShadowedSymbol => {
            DiagnosticSeverity::WARNING
        }
        DiagnosticKind::UnresolvedReference
        | DiagnosticKind::UnresolvedInclude
        | DiagnosticKind::IncludeCycle
        | DiagnosticKind::WrongNamespace
        | DiagnosticKind::UnknownField
        | DiagnosticKind::InvalidBuiltinNamedArgument => DiagnosticSeverity::ERROR,
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
    let snapshot = state.cache.get(&uri)?;
    let offset = position_to_offset(
        snapshot.text.as_ref(),
        params.text_document_position_params.position,
    )?;
    if let Some(component) = snapshot.hovered_component_at(offset) {
        return structured_field_hover(&snapshot, component);
    }
    if let Some(named_argument) = snapshot.hovered_named_argument_at(offset) {
        return resolved_symbol_hover(&snapshot, named_argument);
    }
    let symbol = snapshot.hovered_resolved_symbol_at(offset)?;
    resolved_symbol_hover(&snapshot, symbol)
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

fn structured_field_hover(
    snapshot: &AnalysisSnapshot,
    component: abap_cache::HoveredComponentInfo,
) -> Option<Hover> {
    let range = byte_range_to_lsp_range(snapshot.text.as_ref(), component.range.clone())?;
    let is_method = matches!(component.kind, abap_cache::HoveredComponentKind::Method);
    let mut lines = vec![format!("`{}`", component.field_name)];
    match &component.kind {
        abap_cache::HoveredComponentKind::Scalar => lines.push("scalar component".to_string()),
        abap_cache::HoveredComponentKind::Structured { structure_name } => {
            lines.push(format!("structured component of `{}`", structure_name))
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
    let snapshot = state.cache.get(&uri)?;
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
                    abap_cache::HoveredComponentKind::Scalar
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
    let snapshot = state.cache.get(&uri)?;
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
            definition_provider: Some(OneOf::Left(false)),
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
            lines.push("scalar component".to_string());
            item.declared_type
                .clone()
                .or_else(|| Some("scalar component".to_string()))
        }
        abap_cache::HoveredComponentKind::Structured { structure_name } => {
            lines.push(format!("structured component of `{}`", structure_name));
            Some(match &item.declared_type {
                Some(type_ref) => format!("{type_ref} -> {structure_name}"),
                None => format!("structured component -> {structure_name}"),
            })
        }
        abap_cache::HoveredComponentKind::Method => {
            if let Some(declaration) = &item.declaration {
                lines[0] = format!("```abap\n{}\n```", declaration);
            }
            lines.push("static method".to_string());
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
    use std::str::FromStr;

    use lsp_types::{
        DidChangeTextDocumentParams, DidOpenTextDocumentParams, Documentation, HoverContents,
        Position, TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem,
        TextDocumentPositionParams, Uri, VersionedTextDocumentIdentifier,
    };

    use crate::sem_tokens;

    use super::{
        CompletionParams, CompletionResponse, DEPENDENCY_CACHE_CLEARED, HoverParams,
        REMOTE_DEPENDENCIES_UPDATED, RESOLVE_REMOTE_DEPENDENCIES, ServerState,
        WORKSPACE_MANIFEST_UPDATED, build_lsp_diagnostics, completion, hover, initialize_result,
        normalize_lsp_uri, publish_changed_document, publish_open_document,
    };

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
        assert!(result.server_info.is_some());
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

        let snapshot = state
            .cache
            .get("file:///sem_event.abap")
            .expect("snapshot");
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
        assert!(markup.value.contains("```abap\nTYPE i\n```"), "{}", markup.value);
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
        let lo_expr1_col =
            lo_expr1_line.1.find("lo_expr1").expect("constructor arg col") as u32 + 1;
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
        let add_stmt_col =
            add_stmt_line.1.find("add_statement").expect("method name col") as u32 + 1;
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
        assert!(add_stmt_markup.value.contains("io_stmt TYPE REF TO zcl_stmt"));

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
        assert!(to_string_markup.value.contains("instance method of `lo_prog`"));
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
        assert!(param_hover.is_none(), "builtin named parameter should not hover");

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
            !tokens.data.iter().any(|token| token.token_type == parameter_idx),
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
        assert!(param_hover.is_none(), "builtin named parameter should not hover");

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
            markup.value.contains("```abap\nTYPE string\n```"),
            "{}",
            markup.value
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
            markup.value.contains("```abap\nTYPE REF TO some_class\n```"),
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
}
