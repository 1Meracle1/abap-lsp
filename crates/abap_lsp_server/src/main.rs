use std::io::{self, BufRead, BufReader, BufWriter, Write};
use std::net::{SocketAddr, TcpListener};

use abap_jsonrpc::{JSON_RPC_VERSION, Response, read_frame, write_frame};
use abap_lsp::{
    CompletionParams, DidChangeTextDocumentParams, DidOpenTextDocumentParams, HoverParams, ServerConfig,
    ServerState, completion, hover, initialize_result, publish_changed_document, publish_open_document,
};
use serde_json::{Value, json};
use tracing::warn;

const METHOD_NOT_FOUND: i64 = -32601;
const INVALID_REQUEST: i64 = -32600;

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
        if let Some(response) = handle_message(&mut state, &config, message)? {
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

fn handle_message(
    state: &mut ServerState,
    config: &ServerConfig,
    message: Value,
) -> Result<Option<Response>, Box<dyn std::error::Error>> {
    let method = message.get("method").and_then(Value::as_str);
    let id = message.get("id").cloned();
    match method {
        Some("initialize") => {
            let result = serde_json::to_value(initialize_result(config))?;
            Ok(Some(Response::success(id.unwrap_or(Value::Null), result)))
        }
        Some("shutdown") => {
            state.shutdown_requested = true;
            Ok(Some(Response::success(id.unwrap_or(Value::Null), Value::Null)))
        }
        Some("textDocument/didOpen") => {
            if let Some(params) = parse_params::<DidOpenTextDocumentParams>(&message)? {
                publish_open_document(state, &params);
            }
            Ok(None)
        }
        Some("textDocument/didChange") => {
            if let Some(params) = parse_params::<DidChangeTextDocumentParams>(&message)? {
                publish_changed_document(state, &params);
            }
            Ok(None)
        }
        Some("textDocument/hover") => {
            let Some(hover_params) = parse_params::<HoverParams>(&message)? else {
                return Ok(Some(Response::failure(
                    id.unwrap_or(Value::Null),
                    INVALID_REQUEST,
                    "textDocument/hover requires params",
                )));
            };
            let result = serde_json::to_value(hover(state, &hover_params))?;
            Ok(Some(Response::success(id.unwrap_or(Value::Null), result)))
        }
        Some("textDocument/completion") => {
            let Some(completion_params) = parse_params::<CompletionParams>(&message)? else {
                return Ok(Some(Response::failure(
                    id.unwrap_or(Value::Null),
                    INVALID_REQUEST,
                    "textDocument/completion requires params",
                )));
            };
            let result = serde_json::to_value(completion(state, &completion_params))?;
            Ok(Some(Response::success(id.unwrap_or(Value::Null), result)))
        }
        Some("exit") => Ok(None),
        Some(other) => {
            if let Some(id) = id {
                Ok(Some(Response::failure(
                    id,
                    METHOD_NOT_FOUND,
                    format!("unsupported method: {other}"),
                )))
            } else {
                warn!("ignoring unsupported notification: {other}");
                Ok(None)
            }
        }
        None => {
            if let Some(id) = id {
                Ok(Some(Response::failure(
                    id,
                    INVALID_REQUEST,
                    "request is missing method",
                )))
            } else {
                Ok(None)
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

        assert!(handle_message(
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
        .expect("didOpen")
        .is_none());

        let response = handle_message(
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
        .expect("hover")
        .expect("hover response");

        let result = response.result.expect("hover result");
        assert!(result.to_string().contains("scalar component"));
        assert!(result.to_string().contains("TYPE i"));
    }

    #[test]
    fn handles_completion_after_open_document() {
        let mut state = ServerState::default();
        let config = ServerConfig::default();

        assert!(handle_message(
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
        .expect("didOpen")
        .is_none());

        let response = handle_message(
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
        .expect("completion")
        .expect("completion response");

        let result = response.result.expect("completion result");
        assert!(result.to_string().contains("alpha"));
        assert!(result.to_string().contains("amount"));
    }
}
