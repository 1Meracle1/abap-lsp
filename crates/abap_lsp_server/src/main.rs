use std::io::{self, BufReader, BufWriter};

use abap_jsonrpc::{JSON_RPC_VERSION, Response, read_frame, write_frame};
use abap_lsp::{ServerConfig, ServerState, initialize_result};
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

    let stdin = io::stdin();
    let stdout = io::stdout();
    let mut reader = BufReader::new(stdin.lock());
    let mut writer = BufWriter::new(stdout.lock());
    let mut state = ServerState::default();
    let config = ServerConfig::default();

    while let Some(frame) = read_frame(&mut reader)? {
        let message: Value = serde_json::from_slice(&frame)?;
        let method = message.get("method").and_then(Value::as_str);

        match method {
            Some("initialize") => {
                let id = message.get("id").cloned().unwrap_or(Value::Null);
                let result = serde_json::to_value(initialize_result(&config))?;
                let response = Response::success(id, result);
                send_response(&mut writer, &response)?;
            }
            Some("shutdown") => {
                state.shutdown_requested = true;
                let id = message.get("id").cloned().unwrap_or(Value::Null);
                let response = Response::success(id, Value::Null);
                send_response(&mut writer, &response)?;
            }
            Some("exit") => {
                break;
            }
            Some(other) => {
                if let Some(id) = message.get("id").cloned() {
                    let response = Response::failure(id, METHOD_NOT_FOUND, format!("unsupported method: {other}"));
                    send_response(&mut writer, &response)?;
                } else {
                    warn!("ignoring unsupported notification: {other}");
                }
            }
            None => {
                if let Some(id) = message.get("id").cloned() {
                    let response = Response::failure(id, INVALID_REQUEST, "request is missing method");
                    send_response(&mut writer, &response)?;
                }
            }
        }

        if state.shutdown_requested && method == Some("exit") {
            break;
        }
    }

    Ok(())
}

fn send_response(
    writer: &mut BufWriter<std::io::StdoutLock<'_>>,
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
