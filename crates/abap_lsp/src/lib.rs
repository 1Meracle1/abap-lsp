use abap_cache::DocumentStore;
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionOptions, Documentation, Hover, HoverContents,
    HoverProviderCapability, InitializeResult, MarkupContent, MarkupKind, OneOf, Position, Range,
    ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit,
};
use serde::{Deserialize, Serialize};

pub use lsp_types::{CompletionParams, CompletionResponse, DidChangeTextDocumentParams, DidOpenTextDocumentParams, HoverParams};
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

pub fn publish_open_document(state: &ServerState, params: &DidOpenTextDocumentParams) {
    state.cache.publish(
        params.text_document.uri.to_string(),
        params.text_document.version,
        &params.text_document.text,
    );
}

pub fn publish_changed_document(state: &ServerState, params: &DidChangeTextDocumentParams) {
    let Some(change) = params.content_changes.last() else {
        return;
    };
    state.cache.publish(
        params.text_document.uri.to_string(),
        params.text_document.version,
        &change.text,
    );
}

pub fn hover(state: &ServerState, params: &HoverParams) -> Option<Hover> {
    let uri = params.text_document_position_params.text_document.uri.as_str();
    let snapshot = state.cache.get(uri)?;
    let offset = position_to_offset(
        snapshot.text.as_ref(),
        params.text_document_position_params.position,
    )?;
    let component = snapshot.hovered_component_at(offset)?;
    let range = byte_range_to_lsp_range(snapshot.text.as_ref(), component.range.clone())?;
    let mut lines = vec![format!("`{}`", component.field_name)];
    match component.kind {
        abap_cache::HoveredComponentKind::Scalar => lines.push("scalar component".to_string()),
        abap_cache::HoveredComponentKind::Structured { structure_name } => {
            lines.push(format!("structured component of `{}`", structure_name))
        }
    }
    if let Some(declared_type) = component.declared_type {
        lines.push(format!("declared as `{}`", declared_type));
    }
    let mut path = component.base_name.to_string();
    for segment in &component.component_path {
        path.push('-');
        path.push_str(segment.as_ref());
    }
    lines.push(format!("path: `{}`", path));
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
    let uri = params.text_document_position.text_document.uri.as_str();
    let snapshot = state.cache.get(uri)?;
    let offset = position_to_offset(snapshot.text.as_ref(), params.text_document_position.position)?;
    let completion = snapshot.selector_completion_at(offset)?;
    let range = byte_range_to_lsp_range(snapshot.text.as_ref(), completion.replace_range)?;
    let items = completion
        .items
        .into_iter()
        .map(|item| {
            let (detail, documentation) = completion_item_metadata(&item);
            CompletionItem {
                label: item.name.to_string(),
                kind: Some(CompletionItemKind::FIELD),
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
    let line_text = text[line_start..line_end].strip_suffix('\r').unwrap_or(&text[line_start..line_end]);
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
    let line_text = text[line_start..line_end].strip_suffix('\r').unwrap_or(&text[line_start..line_end]);
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
            item.declared_type.clone().or_else(|| Some("scalar component".to_string()))
        }
        abap_cache::HoveredComponentKind::Structured { structure_name } => {
            lines.push(format!("structured component of `{}`", structure_name));
            Some(match &item.declared_type {
                Some(type_ref) => format!("{type_ref} -> {structure_name}"),
                None => format!("structured component -> {structure_name}"),
            })
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
        DidChangeTextDocumentParams, DidOpenTextDocumentParams, Documentation, HoverContents, Position,
        TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem,
        TextDocumentPositionParams, Uri, VersionedTextDocumentIdentifier,
    };

    use super::{
        CompletionParams, CompletionResponse, DEPENDENCY_CACHE_CLEARED, REMOTE_DEPENDENCIES_UPDATED,
        RESOLVE_REMOTE_DEPENDENCIES, ServerState, WORKSPACE_MANIFEST_UPDATED, HoverParams, completion,
        hover, initialize_result, publish_changed_document, publish_open_document,
    };

    #[test]
    fn initialize_result_exposes_server_capabilities() {
        let result = initialize_result(&Default::default());

        assert!(result.capabilities.text_document_sync.is_some());
        assert!(result.server_info.is_some());
    }

    #[test]
    fn custom_notification_names_are_stable() {
        assert_eq!(RESOLVE_REMOTE_DEPENDENCIES, "abapls/resolveRemoteDependencies");
        assert_eq!(REMOTE_DEPENDENCIES_UPDATED, "abapls/remoteDependenciesUpdated");
        assert_eq!(WORKSPACE_MANIFEST_UPDATED, "abapls/workspaceManifestUpdated");
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
                    text: "DATA: BEGIN OF ls_date, yyyy(4), END OF ls_date.\nls_date-yyyy = '2026'."
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
                    text: "DATA: BEGIN OF ls_date, mm(2), END OF ls_date.\nls_date-mm = '04'.".to_string(),
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
}
