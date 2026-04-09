use std::io::{self, BufRead, BufReader, BufWriter, Write};
use std::net::{SocketAddr, TcpListener};
use std::collections::HashSet;

use abap_jsonrpc::{JSON_RPC_VERSION, Response, read_frame, write_frame};
use abap_lsp::{
    CompletionParams, DEPENDENCY_CACHE_CLEARED, DidChangeTextDocumentParams,
    DidOpenTextDocumentParams, GotoDefinitionParams, HoverParams, REMOTE_DEPENDENCIES_UPDATED,
    RESOLVE_REMOTE_DEPENDENCIES, ReferenceParams, SemanticTokensParams, ServerConfig, ServerState,
    WORKSPACE_MANIFEST_UPDATED, WorkspaceManifestUpdatedParams, build_remote_dependency_batch_for_workspace,
    completion, definition,
    handle_dependency_cache_cleared, handle_remote_dependencies_updated,
    handle_workspace_manifest_updated, hover, initialize_result, publish_changed_document_mut,
    publish_diagnostics_params, publish_open_document_mut, references, semantic_tokens,
    workspace_manifest_diagnostics_params,
};
use serde_json::{Value, json};
use tracing::warn;

const METHOD_NOT_FOUND: i64 = -32601;
const INVALID_REQUEST: i64 = -32600;

#[derive(Debug, Clone, Default, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct InitializeParamsLite {
    #[serde(default)]
    workspace_folders: Vec<WorkspaceFolderLite>,
    #[serde(rename = "capabilities")]
    _capabilities: InitializeCapabilitiesLite,
}

#[derive(Debug, Clone, Default, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct WorkspaceFolderLite {
    uri: String,
}

#[derive(Debug, Clone, Default, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct InitializeCapabilitiesLite {
    #[serde(rename = "window")]
    _window: WindowCapabilitiesLite,
}

#[derive(Debug, Clone, Default, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct WindowCapabilitiesLite {
    #[serde(rename = "workDoneProgress")]
    _work_done_progress: Option<bool>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .with_writer(io::stderr)
        .without_time()
        .init();

    if let Some(addr) = listen_address_from_cli_or_env()? {
        let listener = TcpListener::bind(addr)?;
        let bound = listener.local_addr()?;
        tracing::info!(%bound, "waiting for a language client (TCP)");
        let (stream, peer) = listener.accept()?;
        tracing::info!(%peer, "language client connected");
        let reader_stream = stream.try_clone()?;
        let mut reader = BufReader::new(reader_stream);
        let mut writer = BufWriter::new(stream);
        serve(&mut reader, &mut writer)?;
    } else {
        let stdin = io::stdin();
        let stdout = io::stdout();
        let mut reader = BufReader::new(stdin.lock());
        let mut writer = BufWriter::new(stdout.lock());
        serve(&mut reader, &mut writer)?;
    }

    Ok(())
}

/// `--listen 127.0.0.1:9472` or env `ABAP_LSP_LISTEN` (same format). If unset, uses stdio.
fn listen_address_from_cli_or_env() -> Result<Option<SocketAddr>, Box<dyn std::error::Error>> {
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        if arg == "--listen" || arg == "-l" {
            let value = args
                .next()
                .ok_or("expected address after --listen (e.g. 127.0.0.1:9472)")?;
            return Ok(Some(value.parse()?));
        }
        if let Some(rest) = arg.strip_prefix("--listen=") {
            return Ok(Some(rest.parse()?));
        }
    }

    if let Ok(raw) = std::env::var("ABAP_LSP_LISTEN") {
        let trimmed = raw.trim();
        if !trimmed.is_empty() {
            return Ok(Some(trimmed.parse()?));
        }
    }

    Ok(None)
}

fn serve(
    reader: &mut impl BufRead,
    writer: &mut impl Write,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut state = ServerState::default();
    let config = ServerConfig::default();

    while let Some(frame) = read_frame(reader)? {
        let message: Value = serde_json::from_slice(&frame)?;
        let method = message
            .get("method")
            .and_then(Value::as_str)
            .map(str::to_owned);
        if method.as_deref() == Some(REMOTE_DEPENDENCIES_UPDATED) {
            if let Some(params) =
                parse_params::<abap_lsp::RemoteDependenciesUpdatedParams>(&message)?
            {
                let mut source_uris: HashSet<String> = params
                    .source_uris
                    .iter()
                    .map(|uri| abap_lsp::normalize_lsp_uri(uri))
                    .collect();
                if !params.source_uri.is_empty() {
                    source_uris.insert(abap_lsp::normalize_lsp_uri(&params.source_uri));
                }
                let snapshots = handle_remote_dependencies_updated(&mut state, &params);
                for snapshot in snapshots.iter() {
                    if source_uris.contains(snapshot.uri.as_ref()) {
                        let params_value =
                            serde_json::to_value(publish_diagnostics_params(&state, snapshot))?;
                        send_notification(writer, "textDocument/publishDiagnostics", params_value)?;
                    }
                }
                if let Some(request) =
                    build_remote_dependency_batch_for_workspace(&mut state, &params.workspace_uri)
                {
                    send_notification(
                        writer,
                        RESOLVE_REMOTE_DEPENDENCIES,
                        serde_json::to_value(request)?,
                    )?;
                }
            }
            continue;
        }
        let handled = handle_message(&mut state, &config, message)?;
        for (method, params) in handled.notifications {
            send_notification(writer, &method, params)?;
        }
        if let Some(response) = handled.response {
            send_response(writer, &response)?;
        }

        if state.shutdown_requested && method.as_deref() == Some("exit") {
            break;
        }
    }

    Ok(())
}

fn send_response(
    writer: &mut impl Write,
    response: &Response,
) -> Result<(), Box<dyn std::error::Error>> {
    let payload = serde_json::to_vec(&json!({
        "jsonrpc": JSON_RPC_VERSION,
        "id": response.id,
        "result": response.result,
        "error": response.error,
    }))?;
    write_frame(writer, &payload)?;
    Ok(())
}

fn send_notification(
    writer: &mut impl Write,
    method: &str,
    params: Value,
) -> Result<(), Box<dyn std::error::Error>> {
    let payload = serde_json::to_vec(&json!({
        "jsonrpc": JSON_RPC_VERSION,
        "method": method,
        "params": params,
    }))?;
    write_frame(writer, &payload)?;
    Ok(())
}

struct HandledMessage {
    response: Option<Response>,
    notifications: Vec<(String, Value)>,
}

fn handle_message(
    state: &mut ServerState,
    config: &ServerConfig,
    message: Value,
) -> Result<HandledMessage, Box<dyn std::error::Error>> {
    let method = message.get("method").and_then(Value::as_str);
    let id = message.get("id").cloned();
    match method {
        Some("initialize") => {
            if let Some(params) = parse_params::<InitializeParamsLite>(&message)? {
                for workspace in params.workspace_folders {
                    if !workspace.uri.is_empty() {
                        state.register_workspace_folder(workspace.uri);
                    }
                }
            }
            let result = serde_json::to_value(initialize_result(config))?;
            Ok(HandledMessage {
                response: Some(Response::success(id.unwrap_or(Value::Null), result)),
                notifications: Vec::new(),
            })
        }
        Some("shutdown") => {
            state.shutdown_requested = true;
            Ok(HandledMessage {
                response: Some(Response::success(id.unwrap_or(Value::Null), Value::Null)),
                notifications: Vec::new(),
            })
        }
        Some("textDocument/didOpen") => {
            let mut notifications = Vec::new();
            if let Some(params) = parse_params::<DidOpenTextDocumentParams>(&message)? {
                let snapshot = publish_open_document_mut(state, &params);
                let params_value =
                    serde_json::to_value(publish_diagnostics_params(state, &snapshot))?;
                notifications.push(("textDocument/publishDiagnostics".to_owned(), params_value));
                if let Some(params_value) = state
                    .workspace_for_uri(snapshot.uri.as_ref())
                    .and_then(|workspace| {
                        workspace_manifest_diagnostics_params(state, &workspace.root_uri)
                    })
                    .and_then(|params| serde_json::to_value(params).ok())
                {
                    notifications
                        .push(("textDocument/publishDiagnostics".to_owned(), params_value));
                }
                if let Some(workspace_uri) = state
                    .workspace_for_uri(snapshot.uri.as_ref())
                    .map(|workspace| workspace.root_uri.clone())
                    && let Some(request) =
                        build_remote_dependency_batch_for_workspace(state, &workspace_uri)
                {
                    notifications.push((
                        RESOLVE_REMOTE_DEPENDENCIES.to_owned(),
                        serde_json::to_value(request)?,
                    ));
                }
            }
            Ok(HandledMessage {
                response: None,
                notifications,
            })
        }
        Some("textDocument/didChange") => {
            let mut notifications = Vec::new();
            if let Some(params) = parse_params::<DidChangeTextDocumentParams>(&message)? {
                if let Some(snapshot) = publish_changed_document_mut(state, &params) {
                    let params_value =
                        serde_json::to_value(publish_diagnostics_params(state, &snapshot))?;
                    notifications
                        .push(("textDocument/publishDiagnostics".to_owned(), params_value));
                    if let Some(params_value) = state
                        .workspace_for_uri(snapshot.uri.as_ref())
                        .and_then(|workspace| {
                            workspace_manifest_diagnostics_params(state, &workspace.root_uri)
                        })
                        .and_then(|params| serde_json::to_value(params).ok())
                    {
                        notifications
                            .push(("textDocument/publishDiagnostics".to_owned(), params_value));
                    }
                    if let Some(workspace_uri) = state
                        .workspace_for_uri(snapshot.uri.as_ref())
                        .map(|workspace| workspace.root_uri.clone())
                        && let Some(request) =
                            build_remote_dependency_batch_for_workspace(state, &workspace_uri)
                    {
                        notifications.push((
                            RESOLVE_REMOTE_DEPENDENCIES.to_owned(),
                            serde_json::to_value(request)?,
                        ));
                    }
                }
            }
            Ok(HandledMessage {
                response: None,
                notifications,
            })
        }
        Some(WORKSPACE_MANIFEST_UPDATED) => {
            if let Some(params) = parse_params::<WorkspaceManifestUpdatedParams>(&message)? {
                let snapshots = handle_workspace_manifest_updated(state, &params);
                let mut notifications = Vec::new();
                if let Some(params_value) =
                    workspace_manifest_diagnostics_params(state, &params.workspace_uri)
                        .and_then(|params| serde_json::to_value(params).ok())
                {
                    notifications
                        .push(("textDocument/publishDiagnostics".to_owned(), params_value));
                }
                for snapshot in &snapshots {
                    let params_value =
                        serde_json::to_value(publish_diagnostics_params(state, snapshot))?;
                    notifications
                        .push(("textDocument/publishDiagnostics".to_owned(), params_value));
                }
                if let Some(request) =
                    build_remote_dependency_batch_for_workspace(state, &params.workspace_uri)
                {
                    notifications.push((
                        RESOLVE_REMOTE_DEPENDENCIES.to_string(),
                        serde_json::to_value(request)?,
                    ));
                }
                return Ok(HandledMessage {
                    response: None,
                    notifications,
                });
            }
            Ok(HandledMessage {
                response: None,
                notifications: Vec::new(),
            })
        }
        Some(DEPENDENCY_CACHE_CLEARED) => {
            if let Some(params) = parse_params::<WorkspaceManifestUpdatedParams>(&message)? {
                let snapshots = handle_dependency_cache_cleared(state, &params);
                let mut notifications = Vec::new();
                if let Some(params_value) =
                    workspace_manifest_diagnostics_params(state, &params.workspace_uri)
                        .and_then(|params| serde_json::to_value(params).ok())
                {
                    notifications
                        .push(("textDocument/publishDiagnostics".to_string(), params_value));
                }
                for snapshot in &snapshots {
                    let params_value =
                        serde_json::to_value(publish_diagnostics_params(state, snapshot))?;
                    notifications
                        .push(("textDocument/publishDiagnostics".to_string(), params_value));
                }
                if let Some(request) =
                    build_remote_dependency_batch_for_workspace(state, &params.workspace_uri)
                {
                    notifications.push((
                        RESOLVE_REMOTE_DEPENDENCIES.to_string(),
                        serde_json::to_value(request)?,
                    ));
                }
                return Ok(HandledMessage {
                    response: None,
                    notifications,
                });
            }
            Ok(HandledMessage {
                response: None,
                notifications: Vec::new(),
            })
        }
        Some(REMOTE_DEPENDENCIES_UPDATED) => Ok(HandledMessage {
            response: None,
            notifications: Vec::new(),
        }),
        Some("textDocument/hover") => {
            let Some(hover_params) = parse_params::<HoverParams>(&message)? else {
                return Ok(HandledMessage {
                    response: Some(Response::failure(
                        id.unwrap_or(Value::Null),
                        INVALID_REQUEST,
                        "textDocument/hover requires params",
                    )),
                    notifications: Vec::new(),
                });
            };
            let result = serde_json::to_value(hover(state, &hover_params))?;
            Ok(HandledMessage {
                response: Some(Response::success(id.unwrap_or(Value::Null), result)),
                notifications: Vec::new(),
            })
        }
        Some("textDocument/definition") => {
            let Some(definition_params) = parse_params::<GotoDefinitionParams>(&message)? else {
                return Ok(HandledMessage {
                    response: Some(Response::failure(
                        id.unwrap_or(Value::Null),
                        INVALID_REQUEST,
                        "textDocument/definition requires params",
                    )),
                    notifications: Vec::new(),
                });
            };
            let result = serde_json::to_value(definition(state, &definition_params))?;
            Ok(HandledMessage {
                response: Some(Response::success(id.unwrap_or(Value::Null), result)),
                notifications: Vec::new(),
            })
        }
        Some("textDocument/references") => {
            let Some(reference_params) = parse_params::<ReferenceParams>(&message)? else {
                return Ok(HandledMessage {
                    response: Some(Response::failure(
                        id.unwrap_or(Value::Null),
                        INVALID_REQUEST,
                        "textDocument/references requires params",
                    )),
                    notifications: Vec::new(),
                });
            };
            let result = serde_json::to_value(references(state, &reference_params))?;
            Ok(HandledMessage {
                response: Some(Response::success(id.unwrap_or(Value::Null), result)),
                notifications: Vec::new(),
            })
        }
        Some("textDocument/completion") => {
            let Some(completion_params) = parse_params::<CompletionParams>(&message)? else {
                return Ok(HandledMessage {
                    response: Some(Response::failure(
                        id.unwrap_or(Value::Null),
                        INVALID_REQUEST,
                        "textDocument/completion requires params",
                    )),
                    notifications: Vec::new(),
                });
            };
            let result = serde_json::to_value(completion(state, &completion_params))?;
            Ok(HandledMessage {
                response: Some(Response::success(id.unwrap_or(Value::Null), result)),
                notifications: Vec::new(),
            })
        }
        Some("textDocument/semanticTokens/full") => {
            let Some(st_params) = parse_params::<SemanticTokensParams>(&message)? else {
                return Ok(HandledMessage {
                    response: Some(Response::failure(
                        id.unwrap_or(Value::Null),
                        INVALID_REQUEST,
                        "textDocument/semanticTokens/full requires params",
                    )),
                    notifications: Vec::new(),
                });
            };
            let result = serde_json::to_value(semantic_tokens(state, &st_params))?;
            Ok(HandledMessage {
                response: Some(Response::success(id.unwrap_or(Value::Null), result)),
                notifications: Vec::new(),
            })
        }
        Some("initialized") | Some("$/progress") | Some("$/cancelRequest") => Ok(HandledMessage {
            response: None,
            notifications: Vec::new(),
        }),
        Some("exit") => Ok(HandledMessage {
            response: None,
            notifications: Vec::new(),
        }),
        Some(other) => {
            if let Some(id) = id {
                Ok(HandledMessage {
                    response: Some(Response::failure(
                        id,
                        METHOD_NOT_FOUND,
                        format!("unsupported method: {other}"),
                    )),
                    notifications: Vec::new(),
                })
            } else {
                warn!("ignoring unsupported notification: {other}");
                Ok(HandledMessage {
                    response: None,
                    notifications: Vec::new(),
                })
            }
        }
        None => {
            if let Some(id) = id {
                Ok(HandledMessage {
                    response: Some(Response::failure(
                        id,
                        INVALID_REQUEST,
                        "request is missing method",
                    )),
                    notifications: Vec::new(),
                })
            } else {
                Ok(HandledMessage {
                    response: None,
                    notifications: Vec::new(),
                })
            }
        }
    }
}

fn parse_params<T: abap_lsp::serde::de::DeserializeOwned>(
    message: &Value,
) -> Result<Option<T>, Box<dyn std::error::Error>> {
    let Some(params) = message.get("params").cloned() else {
        return Ok(None);
    };
    Ok(Some(serde_json::from_value(params)?))
}

#[cfg(test)]
mod tests {
    use super::handle_message;
    use abap_lsp::{ServerConfig, ServerState};
    use serde_json::json;

    #[test]
    fn handles_hover_after_open_document() {
        let mut state = ServerState::default();
        let config = ServerConfig::default();

        let opened = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///hover.abap",
                        "languageId": "abap",
                        "version": 1,
                        "text": "TYPES: BEGIN OF ty_inner,\n         a TYPE i,\n       END OF ty_inner.\nTYPES: BEGIN OF ty_outer,\n         inner TYPE ty_inner,\n       END OF ty_outer.\nDATA ls_outer TYPE ty_outer.\nls_outer-inner-a = 1."
                    }
                }
            }),
        )
        .expect("didOpen");
        assert!(opened.response.is_none());
        assert_eq!(opened.notifications.len(), 1);

        let hover_msg = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "textDocument/hover",
                "params": {
                    "textDocument": { "uri": "file:///hover.abap" },
                    "position": { "line": 7, "character": 15 }
                }
            }),
        )
        .expect("hover");

        let result = hover_msg
            .response
            .expect("hover response")
            .result
            .expect("hover result");
        assert!(result.to_string().contains("scalar component"));
        assert!(result.to_string().contains("TYPE i"));
    }

    #[test]
    fn handles_completion_after_open_document() {
        let mut state = ServerState::default();
        let config = ServerConfig::default();

        let opened = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///completion.abap",
                        "languageId": "abap",
                        "version": 1,
                        "text": "TYPES: BEGIN OF ty_inner,\n         alpha TYPE i,\n         amount TYPE i,\n       END OF ty_inner.\nTYPES: BEGIN OF ty_outer,\n         inner TYPE ty_inner,\n       END OF ty_outer.\nDATA ls_outer TYPE ty_outer.\nls_outer-inner-a"
                    }
                }
            }),
        )
        .expect("didOpen");
        assert!(opened.response.is_none());

        let completion_msg = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "textDocument/completion",
                "params": {
                    "textDocument": { "uri": "file:///completion.abap" },
                    "position": { "line": 8, "character": 16 }
                }
            }),
        )
        .expect("completion");

        let result = completion_msg
            .response
            .expect("completion response")
            .result
            .expect("completion result");
        assert!(result.to_string().contains("alpha"));
        assert!(result.to_string().contains("amount"));
    }

    #[test]
    fn handles_definition_after_open_document() {
        let mut state = ServerState::default();
        let config = ServerConfig::default();

        let opened = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///definition.abap",
                        "languageId": "abap",
                        "version": 1,
                        "text": "CLASS zcl_program DEFINITION.\n  PUBLIC SECTION.\n    METHODS add_statement\n      IMPORTING io_stmt TYPE string.\nENDCLASS.\n\nCLASS zcl_program IMPLEMENTATION.\nENDCLASS.\n\nSTART-OF-SELECTION.\n  DATA(lo_prog) = NEW zcl_program( ).\n  lo_prog->add_statement( io_stmt = 'x' )."
                    }
                }
            }),
        )
        .expect("didOpen");
        assert!(opened.response.is_none());

        let definition_msg = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "textDocument/definition",
                "params": {
                    "textDocument": { "uri": "file:///definition.abap" },
                    "position": { "line": 11, "character": 27 }
                }
            }),
        )
        .expect("definition");

        let result = definition_msg
            .response
            .expect("definition response")
            .result
            .expect("definition result");
        assert!(result.to_string().contains("file:///definition.abap"));
        assert!(result.to_string().contains("\"line\":3"));
        assert!(result.to_string().contains("\"character\":16"));
    }

    #[test]
    fn handles_references_after_open_document() {
        let mut state = ServerState::default();
        let config = ServerConfig::default();

        let opened = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///references.abap",
                        "languageId": "abap",
                        "version": 1,
                        "text": "DATA lv TYPE i.\nlv = 1."
                    }
                }
            }),
        )
        .expect("didOpen");
        assert!(opened.response.is_none());

        let references_msg = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "textDocument/references",
                "params": {
                    "textDocument": { "uri": "file:///references.abap" },
                    "position": { "line": 1, "character": 1 },
                    "context": { "includeDeclaration": true }
                }
            }),
        )
        .expect("references");

        let result = references_msg
            .response
            .expect("references response")
            .result
            .expect("references result");
        let locations = result.as_array().expect("array result");
        assert_eq!(locations.len(), 2);
    }

    #[test]
    fn handles_semantic_tokens_full_after_open_document() {
        let mut state = ServerState::default();
        let config = ServerConfig::default();

        let opened = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///st.abap",
                        "languageId": "abap",
                        "version": 1,
                        "text": "DATA lv TYPE i."
                    }
                }
            }),
        )
        .expect("didOpen");
        assert!(opened.response.is_none());

        let st_msg = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "id": 2,
                "method": "textDocument/semanticTokens/full",
                "params": { "textDocument": { "uri": "file:///st.abap" } }
            }),
        )
        .expect("semanticTokens");

        let result = st_msg
            .response
            .expect("semanticTokens response")
            .result
            .expect("semanticTokens result");
        let data = result.get("data").expect("data array");
        assert!(data.as_array().is_some_and(|row| !row.is_empty()));
    }
}
