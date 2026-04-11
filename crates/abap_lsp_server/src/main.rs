use std::collections::{HashMap, HashSet};
use std::io::{self, BufRead, BufReader, BufWriter, Write};
use std::net::{SocketAddr, TcpListener};
use std::sync::mpsc::{self, Receiver, RecvTimeoutError, SyncSender};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

use abap_jsonrpc::{JSON_RPC_VERSION, Response, read_frame, write_frame};
use abap_lsp::{
    CompletionParams, DEPENDENCY_CACHE_CLEARED, DidChangeTextDocumentParams,
    DidOpenTextDocumentParams, GotoDefinitionParams, HoverParams, REMOTE_DEPENDENCIES_UPDATED,
    RESOLVE_REMOTE_DEPENDENCIES, ReferenceParams, SemanticTokensParams, ServerConfig, ServerState,
    WORKSPACE_ANALYSIS_STATUS, WORKSPACE_MANIFEST_UPDATED, WorkspaceAnalysisPhase,
    WorkspaceAnalysisStatusParams, WorkspaceManifestUpdatedParams, WorkspaceState,
    build_remote_dependency_batch_for_workspace, completion, definition,
    handle_dependency_cache_cleared_with_progress,
    handle_remote_dependencies_updated_with_progress,
    handle_workspace_manifest_updated_with_progress, hover, initialize_result,
    prune_workspace_preview_snapshots, publish_changed_document_mut_with_progress,
    publish_diagnostics_params, publish_open_document_mut_with_progress, references,
    refresh_workspace_with_progress, semantic_tokens, stage_workspace_preview_snapshot,
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
    #[serde(default)]
    root_uri: Option<String>,
    #[serde(default, rename = "capabilities")]
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
        let mut reader = BufReader::new(stdin);
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

enum InboundMessage {
    Message(Value),
    Closed,
    Error(String),
}

enum AnalysisTaskKind {
    DidOpen(DidOpenTextDocumentParams),
    DidChange(DidChangeTextDocumentParams),
    ManifestUpdated(WorkspaceManifestUpdatedParams),
    DependencyCacheCleared(WorkspaceManifestUpdatedParams),
    RemoteDependenciesUpdated(abap_lsp::RemoteDependenciesUpdatedParams),
    Initialized,
}

struct AnalysisTask {
    workspace_uri: String,
    generation: u64,
    started: Option<WorkspaceAnalysisStatusParams>,
    workspace: WorkspaceState,
    kind: AnalysisTaskKind,
}

struct AnalysisCompletion {
    workspace_uri: String,
    generation: u64,
    started: Option<WorkspaceAnalysisStatusParams>,
    workspace: WorkspaceState,
    notifications: Vec<(String, Value)>,
}

fn next_workspace_generation(
    generations: &Arc<Mutex<HashMap<String, u64>>>,
    workspace_uri: &str,
) -> u64 {
    let mut generations = generations
        .lock()
        .expect("workspace generation tracking should not be poisoned");
    let entry = generations.entry(workspace_uri.to_owned()).or_insert(0);
    *entry += 1;
    *entry
}

fn current_workspace_generation(
    generations: &Arc<Mutex<HashMap<String, u64>>>,
    workspace_uri: &str,
) -> u64 {
    generations
        .lock()
        .expect("workspace generation tracking should not be poisoned")
        .get(workspace_uri)
        .copied()
        .unwrap_or(0)
}

fn try_schedule_background_analysis(
    state: &mut ServerState,
    message: &Value,
    task_tx: &SyncSender<AnalysisTask>,
    generations: &Arc<Mutex<HashMap<String, u64>>>,
) -> Result<Option<Vec<WorkspaceAnalysisStatusParams>>, Box<dyn std::error::Error>> {
    let Some(method) = message.get("method").and_then(Value::as_str) else {
        return Ok(None);
    };

    let mut started_statuses = Vec::new();

    match method {
        "textDocument/didOpen" => {
            let Some(params) = parse_params::<DidOpenTextDocumentParams>(message)? else {
                return Ok(Some(started_statuses));
            };
            let Some(workspace_uri) = stage_workspace_open_overlay(state, &params) else {
                return Ok(None);
            };
            let Some(workspace) = state.workspaces.get(&workspace_uri).cloned() else {
                return Ok(None);
            };
            let started = workspace_analysis_status_started(state, message)?;
            if let Some(params) = started.clone() {
                started_statuses.push(params);
            }
            task_tx
                .send(AnalysisTask {
                    workspace_uri: workspace_uri.clone(),
                    generation: next_workspace_generation(generations, &workspace_uri),
                    started,
                    workspace,
                    kind: AnalysisTaskKind::DidOpen(params),
                })
                .map_err(|error| format!("failed to enqueue analysis task: {error}"))?;
            Ok(Some(started_statuses))
        }
        "textDocument/didChange" => {
            let Some(params) = parse_params::<DidChangeTextDocumentParams>(message)? else {
                return Ok(Some(started_statuses));
            };
            let Some(workspace_uri) = stage_workspace_change_overlay(state, &params) else {
                return Ok(None);
            };
            let Some(workspace) = state.workspaces.get(&workspace_uri).cloned() else {
                return Ok(None);
            };
            let started = workspace_analysis_status_started(state, message)?;
            if let Some(params) = started.clone() {
                started_statuses.push(params);
            }
            task_tx
                .send(AnalysisTask {
                    workspace_uri: workspace_uri.clone(),
                    generation: next_workspace_generation(generations, &workspace_uri),
                    started,
                    workspace,
                    kind: AnalysisTaskKind::DidChange(params),
                })
                .map_err(|error| format!("failed to enqueue analysis task: {error}"))?;
            Ok(Some(started_statuses))
        }
        WORKSPACE_MANIFEST_UPDATED => {
            let Some(params) = parse_params::<WorkspaceManifestUpdatedParams>(message)? else {
                return Ok(Some(started_statuses));
            };
            let workspace_uri = abap_lsp::normalize_lsp_uri(&params.workspace_uri);
            let Some(workspace) = state.workspaces.get(&workspace_uri).cloned() else {
                return Ok(Some(started_statuses));
            };
            let started = workspace_analysis_status_started(state, message)?;
            if let Some(params) = started.clone() {
                started_statuses.push(params);
            }
            task_tx
                .send(AnalysisTask {
                    workspace_uri: workspace_uri.clone(),
                    generation: next_workspace_generation(generations, &workspace_uri),
                    started,
                    workspace,
                    kind: AnalysisTaskKind::ManifestUpdated(WorkspaceManifestUpdatedParams {
                        workspace_uri,
                    }),
                })
                .map_err(|error| format!("failed to enqueue analysis task: {error}"))?;
            Ok(Some(started_statuses))
        }
        DEPENDENCY_CACHE_CLEARED => {
            let Some(params) = parse_params::<WorkspaceManifestUpdatedParams>(message)? else {
                return Ok(Some(started_statuses));
            };
            let workspace_uri = abap_lsp::normalize_lsp_uri(&params.workspace_uri);
            let Some(workspace) = state.workspaces.get(&workspace_uri).cloned() else {
                return Ok(Some(started_statuses));
            };
            let started = workspace_analysis_status_started(state, message)?;
            if let Some(params) = started.clone() {
                started_statuses.push(params);
            }
            task_tx
                .send(AnalysisTask {
                    workspace_uri: workspace_uri.clone(),
                    generation: next_workspace_generation(generations, &workspace_uri),
                    started,
                    workspace,
                    kind: AnalysisTaskKind::DependencyCacheCleared(
                        WorkspaceManifestUpdatedParams { workspace_uri },
                    ),
                })
                .map_err(|error| format!("failed to enqueue analysis task: {error}"))?;
            Ok(Some(started_statuses))
        }
        REMOTE_DEPENDENCIES_UPDATED => {
            let Some(params) = parse_params::<abap_lsp::RemoteDependenciesUpdatedParams>(message)?
            else {
                return Ok(Some(started_statuses));
            };
            let workspace_uri = abap_lsp::normalize_lsp_uri(&params.workspace_uri);
            let Some(workspace) = state.workspaces.get(&workspace_uri).cloned() else {
                return Ok(Some(started_statuses));
            };
            let started = workspace_analysis_status_started(state, message)?;
            if let Some(params) = started.clone() {
                started_statuses.push(params);
            }
            task_tx
                .send(AnalysisTask {
                    workspace_uri: workspace_uri.clone(),
                    generation: next_workspace_generation(generations, &workspace_uri),
                    started,
                    workspace,
                    kind: AnalysisTaskKind::RemoteDependenciesUpdated(
                        abap_lsp::RemoteDependenciesUpdatedParams {
                            workspace_uri,
                            ..params
                        },
                    ),
                })
                .map_err(|error| format!("failed to enqueue analysis task: {error}"))?;
            Ok(Some(started_statuses))
        }
        "initialized" => {
            let workspace_uris: Vec<_> = state.workspaces.keys().cloned().collect();
            for workspace_uri in workspace_uris {
                let Some(workspace) = state.workspaces.get(&workspace_uri).cloned() else {
                    continue;
                };
                task_tx
                    .send(AnalysisTask {
                        workspace_uri: workspace_uri.clone(),
                        generation: next_workspace_generation(generations, &workspace_uri),
                        started: workspace_analysis_status_started(state, message)?,
                        workspace,
                        kind: AnalysisTaskKind::Initialized,
                    })
                    .map_err(|error| format!("failed to enqueue analysis task: {error}"))?;
            }
            Ok(Some(started_statuses))
        }
        _ => Ok(None),
    }
}

fn run_analysis_task(task: AnalysisTask) -> Result<AnalysisCompletion, Box<dyn std::error::Error>> {
    let mut state = ServerState {
        cache: Default::default(),
        workspaces: HashMap::from([(task.workspace_uri.clone(), task.workspace)]),
        shutdown_requested: false,
    };

    let notifications = match &task.kind {
        AnalysisTaskKind::DidOpen(params) => handle_did_open_notifications(&mut state, params)?,
        AnalysisTaskKind::DidChange(params) => handle_did_change_notifications(&mut state, params)?,
        AnalysisTaskKind::ManifestUpdated(params) => {
            handle_workspace_manifest_updated_notifications(&mut state, params)?
        }
        AnalysisTaskKind::DependencyCacheCleared(params) => {
            handle_dependency_cache_cleared_notifications(&mut state, params)?
        }
        AnalysisTaskKind::RemoteDependenciesUpdated(params) => {
            handle_remote_dependencies_updated_notifications(&mut state, params)?
        }
        AnalysisTaskKind::Initialized => {
            handle_initialized_workspace_notifications(&mut state, &task.workspace_uri)?
        }
    };

    let workspace = state
        .workspaces
        .remove(&task.workspace_uri)
        .expect("analysis task should keep its workspace");

    Ok(AnalysisCompletion {
        workspace_uri: task.workspace_uri,
        generation: task.generation,
        started: task.started,
        workspace,
        notifications,
    })
}

fn flush_analysis_completions(
    state: &mut ServerState,
    writer: &mut impl Write,
    completion_rx: &Receiver<AnalysisCompletion>,
    generations: &Arc<Mutex<HashMap<String, u64>>>,
) -> Result<(), Box<dyn std::error::Error>> {
    while let Ok(completion) = completion_rx.try_recv() {
        if completion.generation
            != current_workspace_generation(generations, &completion.workspace_uri)
        {
            continue;
        }

        state
            .workspaces
            .insert(completion.workspace_uri.clone(), completion.workspace);
        if let Some(workspace) = state.workspaces.get_mut(&completion.workspace_uri) {
            prune_workspace_preview_snapshots(workspace);
        }

        for (method, params) in completion.notifications {
            send_notification(writer, &method, params)?;
        }
        if let Some(params) = completion
            .started
            .as_ref()
            .and_then(|started| workspace_analysis_status_finished(state, started))
        {
            send_notification(
                writer,
                WORKSPACE_ANALYSIS_STATUS,
                serde_json::to_value(params)?,
            )?;
        }
    }
    Ok(())
}

fn serve(
    reader: &mut (impl BufRead + Send),
    writer: &mut impl Write,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut state = ServerState::default();
    let config = ServerConfig::default();
    let generations = Arc::new(Mutex::new(HashMap::<String, u64>::new()));
    let (message_tx, message_rx) = mpsc::channel();
    let (task_tx, task_rx): (SyncSender<AnalysisTask>, Receiver<AnalysisTask>) =
        mpsc::sync_channel(8);
    let (completion_tx, completion_rx) = mpsc::channel();

    thread::scope(|scope| -> Result<(), Box<dyn std::error::Error>> {
        scope.spawn(|| {
            loop {
                match read_frame(reader) {
                    Ok(Some(frame)) => match serde_json::from_slice(&frame) {
                        Ok(message) => {
                            if message_tx.send(InboundMessage::Message(message)).is_err() {
                                break;
                            }
                        }
                        Err(error) => {
                            let _ = message_tx.send(InboundMessage::Error(error.to_string()));
                            break;
                        }
                    },
                    Ok(None) => {
                        let _ = message_tx.send(InboundMessage::Closed);
                        break;
                    }
                    Err(error) => {
                        let _ = message_tx.send(InboundMessage::Error(error.to_string()));
                        break;
                    }
                }
            }
        });

        let worker_generations = Arc::clone(&generations);
        let worker_completion_tx = completion_tx.clone();
        scope.spawn(move || {
            while let Ok(task) = task_rx.recv() {
                if task.generation
                    != current_workspace_generation(&worker_generations, &task.workspace_uri)
                {
                    continue;
                }
                match run_analysis_task(task) {
                    Ok(completion) => {
                        if completion.generation
                            == current_workspace_generation(
                                &worker_generations,
                                &completion.workspace_uri,
                            )
                        {
                            if worker_completion_tx.send(completion).is_err() {
                                break;
                            }
                        }
                    }
                    Err(error) => {
                        warn!(error = %error, "background analysis task failed");
                    }
                }
            }
        });

        let mut reader_closed = false;
        loop {
            flush_analysis_completions(&mut state, writer, &completion_rx, &generations)?;

            match message_rx.recv_timeout(Duration::from_millis(10)) {
                Ok(InboundMessage::Message(message)) => {
                    let method = message
                        .get("method")
                        .and_then(Value::as_str)
                        .map(str::to_owned);
                    if let Some(started_statuses) = try_schedule_background_analysis(
                        &mut state,
                        &message,
                        &task_tx,
                        &generations,
                    )? {
                        for params in started_statuses {
                            send_notification(
                                writer,
                                WORKSPACE_ANALYSIS_STATUS,
                                serde_json::to_value(params)?,
                            )?;
                        }
                    } else {
                        let analysis_status = workspace_analysis_status_started(&state, &message)?;
                        if let Some(params) = analysis_status.as_ref() {
                            send_notification(
                                writer,
                                WORKSPACE_ANALYSIS_STATUS,
                                serde_json::to_value(params)?,
                            )?;
                        }
                        let handled = handle_message(&mut state, &config, message)?;
                        for (method, params) in handled.notifications {
                            send_notification(writer, &method, params)?;
                        }
                        if let Some(response) = handled.response {
                            send_response(writer, &response)?;
                        }
                        if let Some(params) = analysis_status
                            .as_ref()
                            .and_then(|params| workspace_analysis_status_finished(&state, params))
                        {
                            send_notification(
                                writer,
                                WORKSPACE_ANALYSIS_STATUS,
                                serde_json::to_value(params)?,
                            )?;
                        }
                    }

                    flush_analysis_completions(&mut state, writer, &completion_rx, &generations)?;

                    if state.shutdown_requested && method.as_deref() == Some("exit") {
                        break;
                    }
                }
                Ok(InboundMessage::Closed) => {
                    reader_closed = true;
                }
                Ok(InboundMessage::Error(error)) => return Err(error.into()),
                Err(RecvTimeoutError::Timeout) => {
                    if reader_closed {
                        break;
                    }
                }
                Err(RecvTimeoutError::Disconnected) => break,
            }
        }

        drop(task_tx);
        flush_analysis_completions(&mut state, writer, &completion_rx, &generations)?;
        Ok(())
    })
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

fn push_workspace_diagnostics_notifications(
    state: &ServerState,
    workspace_uri: &str,
    notifications: &mut Vec<(String, Value)>,
) -> Result<(), Box<dyn std::error::Error>> {
    let Some(workspace) = state.workspaces.get(workspace_uri) else {
        return Ok(());
    };
    let mut uris = workspace.cache.uris();
    uris.sort();
    for uri in uris {
        let Some(snapshot) = workspace.cache.get(uri.as_ref()) else {
            continue;
        };
        notifications.push((
            "textDocument/publishDiagnostics".to_owned(),
            serde_json::to_value(publish_diagnostics_params(state, snapshot.as_ref()))?,
        ));
    }
    Ok(())
}

struct HandledMessage {
    response: Option<Response>,
    notifications: Vec<(String, Value)>,
}

fn handle_did_open_notifications(
    state: &mut ServerState,
    params: &DidOpenTextDocumentParams,
) -> Result<Vec<(String, Value)>, Box<dyn std::error::Error>> {
    let mut notifications = Vec::new();
    let normalized_uri = abap_lsp::normalize_lsp_uri(params.text_document.uri.as_str());
    let unchanged_workspace_open = state
        .workspace_for_uri(&normalized_uri)
        .and_then(|workspace| workspace.cache.get(&normalized_uri))
        .is_some_and(|snapshot| snapshot.text.as_ref() == params.text_document.text.as_str());
    let progress_notifications = Mutex::new(Vec::new());
    let workspace_uri = state
        .workspace_for_uri(normalized_uri.as_str())
        .map(|workspace| workspace.root_uri.clone());
    let progress = |processed: usize, total: usize| {
        if let Some(workspace_uri) = workspace_uri.as_ref() {
            push_workspace_analysis_progress(
                &progress_notifications,
                workspace_uri,
                "open",
                processed,
                total,
            );
        }
    };
    let snapshot = publish_open_document_mut_with_progress(state, params, Some(&progress));
    notifications.extend(
        progress_notifications
            .into_inner()
            .expect("progress notification collection should not be poisoned"),
    );
    if unchanged_workspace_open {
        let params_value = serde_json::to_value(publish_diagnostics_params(state, &snapshot))?;
        notifications.push(("textDocument/publishDiagnostics".to_owned(), params_value));
    } else if let Some(workspace_uri) = state
        .workspace_for_uri(snapshot.uri.as_ref())
        .filter(|workspace| workspace.cache.get(snapshot.uri.as_ref()).is_some())
        .map(|workspace| workspace.root_uri.clone())
    {
        push_workspace_diagnostics_notifications(state, &workspace_uri, &mut notifications)?;
    } else {
        let params_value = serde_json::to_value(publish_diagnostics_params(state, &snapshot))?;
        notifications.push(("textDocument/publishDiagnostics".to_owned(), params_value));
    }
    if let Some(params_value) = state
        .workspace_for_uri(snapshot.uri.as_ref())
        .and_then(|workspace| workspace_manifest_diagnostics_params(state, &workspace.root_uri))
        .and_then(|params| serde_json::to_value(params).ok())
    {
        notifications.push(("textDocument/publishDiagnostics".to_owned(), params_value));
    }
    if !unchanged_workspace_open
        && let Some(workspace_uri) = state
            .workspace_for_uri(snapshot.uri.as_ref())
            .filter(|workspace| workspace.cache.get(snapshot.uri.as_ref()).is_some())
            .map(|workspace| workspace.root_uri.clone())
        && let Some(request) = build_remote_dependency_batch_for_workspace(state, &workspace_uri)
    {
        notifications.push((
            RESOLVE_REMOTE_DEPENDENCIES.to_owned(),
            serde_json::to_value(request)?,
        ));
    }
    Ok(notifications)
}

fn handle_did_change_notifications(
    state: &mut ServerState,
    params: &DidChangeTextDocumentParams,
) -> Result<Vec<(String, Value)>, Box<dyn std::error::Error>> {
    let mut notifications = Vec::new();
    let normalized_uri = abap_lsp::normalize_lsp_uri(params.text_document.uri.as_str());
    let progress_notifications = Mutex::new(Vec::new());
    let workspace_uri = state
        .workspace_for_uri(normalized_uri.as_str())
        .map(|workspace| workspace.root_uri.clone());
    let change = params.content_changes.last();
    let unchanged_workspace_change = change.and_then(|change| {
        state
            .workspace_for_uri(&normalized_uri)
            .and_then(|workspace| workspace.cache.get(&normalized_uri))
            .map(|snapshot| snapshot.text.as_ref() == change.text.as_str())
    }) == Some(true);
    let progress = |processed: usize, total: usize| {
        if let Some(workspace_uri) = workspace_uri.as_ref() {
            push_workspace_analysis_progress(
                &progress_notifications,
                workspace_uri,
                "change",
                processed,
                total,
            );
        }
    };
    if let Some(snapshot) =
        publish_changed_document_mut_with_progress(state, params, Some(&progress))
    {
        notifications.extend(
            progress_notifications
                .into_inner()
                .expect("progress notification collection should not be poisoned"),
        );
        if unchanged_workspace_change {
            let params_value = serde_json::to_value(publish_diagnostics_params(state, &snapshot))?;
            notifications.push(("textDocument/publishDiagnostics".to_owned(), params_value));
        } else if let Some(workspace_uri) = state
            .workspace_for_uri(snapshot.uri.as_ref())
            .filter(|workspace| workspace.cache.get(snapshot.uri.as_ref()).is_some())
            .map(|workspace| workspace.root_uri.clone())
        {
            push_workspace_diagnostics_notifications(state, &workspace_uri, &mut notifications)?;
        } else {
            let params_value = serde_json::to_value(publish_diagnostics_params(state, &snapshot))?;
            notifications.push(("textDocument/publishDiagnostics".to_owned(), params_value));
        }
        if let Some(params_value) = state
            .workspace_for_uri(snapshot.uri.as_ref())
            .and_then(|workspace| workspace_manifest_diagnostics_params(state, &workspace.root_uri))
            .and_then(|params| serde_json::to_value(params).ok())
        {
            notifications.push(("textDocument/publishDiagnostics".to_owned(), params_value));
        }
        if !unchanged_workspace_change
            && let Some(workspace_uri) = state
                .workspace_for_uri(snapshot.uri.as_ref())
                .filter(|workspace| workspace.cache.get(snapshot.uri.as_ref()).is_some())
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
    Ok(notifications)
}

fn handle_workspace_manifest_updated_notifications(
    state: &mut ServerState,
    params: &WorkspaceManifestUpdatedParams,
) -> Result<Vec<(String, Value)>, Box<dyn std::error::Error>> {
    let progress_notifications = Mutex::new(Vec::new());
    let progress = |processed: usize, total: usize| {
        push_workspace_analysis_progress(
            &progress_notifications,
            &params.workspace_uri,
            "manifest-updated",
            processed,
            total,
        );
    };
    let snapshots = handle_workspace_manifest_updated_with_progress(state, params, Some(&progress));
    let mut notifications = Vec::new();
    notifications.extend(
        progress_notifications
            .into_inner()
            .expect("progress notification collection should not be poisoned"),
    );
    if let Some(params_value) = workspace_manifest_diagnostics_params(state, &params.workspace_uri)
        .and_then(|params| serde_json::to_value(params).ok())
    {
        notifications.push(("textDocument/publishDiagnostics".to_owned(), params_value));
    }
    for snapshot in &snapshots {
        let params_value = serde_json::to_value(publish_diagnostics_params(state, snapshot))?;
        notifications.push(("textDocument/publishDiagnostics".to_owned(), params_value));
    }
    if let Some(request) = build_remote_dependency_batch_for_workspace(state, &params.workspace_uri)
    {
        notifications.push((
            RESOLVE_REMOTE_DEPENDENCIES.to_string(),
            serde_json::to_value(request)?,
        ));
    }
    Ok(notifications)
}

fn handle_dependency_cache_cleared_notifications(
    state: &mut ServerState,
    params: &WorkspaceManifestUpdatedParams,
) -> Result<Vec<(String, Value)>, Box<dyn std::error::Error>> {
    let progress_notifications = Mutex::new(Vec::new());
    let progress = |processed: usize, total: usize| {
        push_workspace_analysis_progress(
            &progress_notifications,
            &params.workspace_uri,
            "dependency-cache-cleared",
            processed,
            total,
        );
    };
    let snapshots = handle_dependency_cache_cleared_with_progress(state, params, Some(&progress));
    let mut notifications = Vec::new();
    notifications.extend(
        progress_notifications
            .into_inner()
            .expect("progress notification collection should not be poisoned"),
    );
    if let Some(params_value) = workspace_manifest_diagnostics_params(state, &params.workspace_uri)
        .and_then(|params| serde_json::to_value(params).ok())
    {
        notifications.push(("textDocument/publishDiagnostics".to_string(), params_value));
    }
    for snapshot in &snapshots {
        let params_value = serde_json::to_value(publish_diagnostics_params(state, snapshot))?;
        notifications.push(("textDocument/publishDiagnostics".to_string(), params_value));
    }
    if let Some(request) = build_remote_dependency_batch_for_workspace(state, &params.workspace_uri)
    {
        notifications.push((
            RESOLVE_REMOTE_DEPENDENCIES.to_string(),
            serde_json::to_value(request)?,
        ));
    }
    Ok(notifications)
}

fn handle_remote_dependencies_updated_notifications(
    state: &mut ServerState,
    params: &abap_lsp::RemoteDependenciesUpdatedParams,
) -> Result<Vec<(String, Value)>, Box<dyn std::error::Error>> {
    let progress_notifications = Mutex::new(Vec::new());
    let progress = |processed: usize, total: usize| {
        push_workspace_analysis_progress(
            &progress_notifications,
            &params.workspace_uri,
            "remote-dependencies-updated",
            processed,
            total,
        );
    };
    let mut source_uris: HashSet<String> = params
        .source_uris
        .iter()
        .map(|uri| abap_lsp::normalize_lsp_uri(uri))
        .collect();
    if !params.source_uri.is_empty() {
        source_uris.insert(abap_lsp::normalize_lsp_uri(&params.source_uri));
    }
    let snapshots =
        handle_remote_dependencies_updated_with_progress(state, params, Some(&progress));
    let mut notifications = progress_notifications
        .into_inner()
        .expect("progress notification collection should not be poisoned");
    for snapshot in snapshots.iter() {
        if source_uris.contains(snapshot.uri.as_ref()) {
            let params_value = serde_json::to_value(publish_diagnostics_params(state, snapshot))?;
            notifications.push(("textDocument/publishDiagnostics".to_owned(), params_value));
        }
    }
    if let Some(request) = build_remote_dependency_batch_for_workspace(state, &params.workspace_uri)
    {
        notifications.push((
            RESOLVE_REMOTE_DEPENDENCIES.to_owned(),
            serde_json::to_value(request)?,
        ));
    }
    Ok(notifications)
}

fn handle_initialized_workspace_notifications(
    state: &mut ServerState,
    workspace_uri: &str,
) -> Result<Vec<(String, Value)>, Box<dyn std::error::Error>> {
    let progress_notifications = Mutex::new(Vec::new());
    let progress = |processed: usize, total: usize| {
        push_workspace_analysis_progress(
            &progress_notifications,
            workspace_uri,
            "initialized",
            processed,
            total,
        );
    };
    let _ = refresh_workspace_with_progress(state, workspace_uri, Some(&progress));
    let mut notifications = progress_notifications
        .into_inner()
        .expect("progress notification collection should not be poisoned");
    if let Some(params_value) = workspace_manifest_diagnostics_params(state, workspace_uri)
        .and_then(|params| serde_json::to_value(params).ok())
    {
        notifications.push(("textDocument/publishDiagnostics".to_owned(), params_value));
    }
    push_workspace_diagnostics_notifications(state, workspace_uri, &mut notifications)?;
    if let Some(request) = build_remote_dependency_batch_for_workspace(state, workspace_uri) {
        notifications.push((
            RESOLVE_REMOTE_DEPENDENCIES.to_owned(),
            serde_json::to_value(request)?,
        ));
    }
    Ok(notifications)
}

fn stage_workspace_open_overlay(
    state: &mut ServerState,
    params: &DidOpenTextDocumentParams,
) -> Option<String> {
    let uri = abap_lsp::normalize_lsp_uri(params.text_document.uri.as_str());
    let workspace = state.workspace_for_uri_mut(&uri)?;
    workspace.open_documents.insert(
        uri,
        abap_lsp::OpenDocumentOverlay {
            version: params.text_document.version,
            text: Arc::from(params.text_document.text.as_str()),
        },
    );
    let root_uri = workspace.root_uri.clone();
    let _ = workspace;
    let _ = stage_workspace_preview_snapshot(
        state,
        params.text_document.uri.as_str(),
        params.text_document.version,
        &params.text_document.text,
    );
    Some(root_uri)
}

fn stage_workspace_change_overlay(
    state: &mut ServerState,
    params: &DidChangeTextDocumentParams,
) -> Option<String> {
    let change = params.content_changes.last()?;
    let uri = abap_lsp::normalize_lsp_uri(params.text_document.uri.as_str());
    let workspace = state.workspace_for_uri_mut(&uri)?;
    workspace.open_documents.insert(
        uri,
        abap_lsp::OpenDocumentOverlay {
            version: params.text_document.version,
            text: Arc::from(change.text.as_str()),
        },
    );
    let root_uri = workspace.root_uri.clone();
    let _ = workspace;
    let _ = stage_workspace_preview_snapshot(
        state,
        params.text_document.uri.as_str(),
        params.text_document.version,
        &change.text,
    );
    Some(root_uri)
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
                let mut registered_workspace = false;
                for workspace in params.workspace_folders {
                    if !workspace.uri.is_empty() {
                        state.register_workspace_folder(workspace.uri);
                        registered_workspace = true;
                    }
                }
                if !registered_workspace
                    && let Some(root_uri) = params.root_uri
                    && !root_uri.is_empty()
                {
                    state.register_workspace_folder(root_uri);
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
            let notifications = parse_params::<DidOpenTextDocumentParams>(&message)?
                .map(|params| handle_did_open_notifications(state, &params))
                .transpose()?
                .unwrap_or_default();
            Ok(HandledMessage {
                response: None,
                notifications,
            })
        }
        Some("textDocument/didChange") => {
            let notifications = parse_params::<DidChangeTextDocumentParams>(&message)?
                .map(|params| handle_did_change_notifications(state, &params))
                .transpose()?
                .unwrap_or_default();
            Ok(HandledMessage {
                response: None,
                notifications,
            })
        }
        Some(WORKSPACE_MANIFEST_UPDATED) => Ok(HandledMessage {
            response: None,
            notifications: parse_params::<WorkspaceManifestUpdatedParams>(&message)?
                .map(|params| handle_workspace_manifest_updated_notifications(state, &params))
                .transpose()?
                .unwrap_or_default(),
        }),
        Some(DEPENDENCY_CACHE_CLEARED) => Ok(HandledMessage {
            response: None,
            notifications: parse_params::<WorkspaceManifestUpdatedParams>(&message)?
                .map(|params| handle_dependency_cache_cleared_notifications(state, &params))
                .transpose()?
                .unwrap_or_default(),
        }),
        Some(REMOTE_DEPENDENCIES_UPDATED) => Ok(HandledMessage {
            response: None,
            notifications: parse_params::<abap_lsp::RemoteDependenciesUpdatedParams>(&message)?
                .map(|params| handle_remote_dependencies_updated_notifications(state, &params))
                .transpose()?
                .unwrap_or_default(),
        }),
        Some("initialized") => {
            let mut notifications = Vec::new();
            let workspace_uris: Vec<_> = state.workspaces.keys().cloned().collect();
            for workspace_uri in workspace_uris {
                notifications.extend(handle_initialized_workspace_notifications(
                    state,
                    &workspace_uri,
                )?);
            }
            Ok(HandledMessage {
                response: None,
                notifications,
            })
        }
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
        Some("$/progress") | Some("$/cancelRequest") => Ok(HandledMessage {
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

fn workspace_analysis_status_started(
    state: &ServerState,
    message: &Value,
) -> Result<Option<WorkspaceAnalysisStatusParams>, Box<dyn std::error::Error>> {
    let Some(method) = message.get("method").and_then(Value::as_str) else {
        return Ok(None);
    };

    let status = match method {
        "textDocument/didOpen" => {
            let Some(params) = parse_params::<DidOpenTextDocumentParams>(message)? else {
                return Ok(None);
            };
            let uri = abap_lsp::normalize_lsp_uri(params.text_document.uri.as_str());
            let Some(workspace_uri) = state
                .workspace_for_uri(&uri)
                .map(|workspace| workspace.root_uri.clone())
            else {
                return Ok(None);
            };
            WorkspaceAnalysisStatusParams {
                workspace_uri,
                phase: WorkspaceAnalysisPhase::Started,
                trigger: "open".to_string(),
                processed_document_count: 0,
                total_document_count: 0,
                analyzed_document_count: 0,
                remote_resolution_in_flight: false,
            }
        }
        WORKSPACE_MANIFEST_UPDATED => {
            let Some(params) = parse_params::<WorkspaceManifestUpdatedParams>(message)? else {
                return Ok(None);
            };
            WorkspaceAnalysisStatusParams {
                workspace_uri: abap_lsp::normalize_lsp_uri(&params.workspace_uri),
                phase: WorkspaceAnalysisPhase::Started,
                trigger: "manifest-updated".to_string(),
                processed_document_count: 0,
                total_document_count: 0,
                analyzed_document_count: 0,
                remote_resolution_in_flight: false,
            }
        }
        DEPENDENCY_CACHE_CLEARED => {
            let Some(params) = parse_params::<WorkspaceManifestUpdatedParams>(message)? else {
                return Ok(None);
            };
            WorkspaceAnalysisStatusParams {
                workspace_uri: abap_lsp::normalize_lsp_uri(&params.workspace_uri),
                phase: WorkspaceAnalysisPhase::Started,
                trigger: "dependency-cache-cleared".to_string(),
                processed_document_count: 0,
                total_document_count: 0,
                analyzed_document_count: 0,
                remote_resolution_in_flight: false,
            }
        }
        REMOTE_DEPENDENCIES_UPDATED => {
            let Some(params) = parse_params::<abap_lsp::RemoteDependenciesUpdatedParams>(message)?
            else {
                return Ok(None);
            };
            WorkspaceAnalysisStatusParams {
                workspace_uri: abap_lsp::normalize_lsp_uri(&params.workspace_uri),
                phase: WorkspaceAnalysisPhase::Started,
                trigger: "remote-dependencies-updated".to_string(),
                processed_document_count: 0,
                total_document_count: 0,
                analyzed_document_count: 0,
                remote_resolution_in_flight: false,
            }
        }
        _ => return Ok(None),
    };

    Ok(Some(status))
}

fn workspace_analysis_status_finished(
    state: &ServerState,
    started: &WorkspaceAnalysisStatusParams,
) -> Option<WorkspaceAnalysisStatusParams> {
    let workspace = state.workspaces.get(&started.workspace_uri)?;
    Some(WorkspaceAnalysisStatusParams {
        workspace_uri: started.workspace_uri.clone(),
        phase: WorkspaceAnalysisPhase::Finished,
        trigger: started.trigger.clone(),
        processed_document_count: workspace.cache.uris().len(),
        total_document_count: workspace.cache.uris().len(),
        analyzed_document_count: workspace.cache.uris().len(),
        remote_resolution_in_flight: workspace.remote_resolution_in_flight,
    })
}

fn push_workspace_analysis_progress(
    notifications: &Mutex<Vec<(String, Value)>>,
    workspace_uri: &str,
    trigger: &str,
    processed: usize,
    total: usize,
) {
    let params = WorkspaceAnalysisStatusParams {
        workspace_uri: abap_lsp::normalize_lsp_uri(workspace_uri),
        phase: WorkspaceAnalysisPhase::Progress,
        trigger: trigger.to_string(),
        processed_document_count: processed,
        total_document_count: total,
        analyzed_document_count: 0,
        remote_resolution_in_flight: false,
    };
    notifications
        .lock()
        .expect("progress notification collection should not be poisoned")
        .push((
            WORKSPACE_ANALYSIS_STATUS.to_string(),
            serde_json::to_value(params).expect("workspace analysis progress should serialize"),
        ));
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::fs;
    use std::path::PathBuf;
    use std::sync::mpsc;
    use std::sync::{Arc, Mutex};
    use std::time::{SystemTime, UNIX_EPOCH};

    use super::{
        handle_message, try_schedule_background_analysis, workspace_analysis_status_finished,
        workspace_analysis_status_started,
    };
    use abap_lsp::{ServerConfig, ServerState, refresh_workspace};
    use serde_json::{Value, json};

    fn temp_workspace_path(name: &str) -> PathBuf {
        let mut path = std::env::temp_dir();
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time")
            .as_nanos();
        path.push(format!("abap_lsp_server_{name}_{unique}"));
        path
    }

    fn file_uri(path: &std::path::Path) -> String {
        format!("file:///{}", path.to_string_lossy().replace('\\', "/"))
    }

    fn semantic_token_positions(result: &Value) -> Vec<(u64, u64)> {
        let data = result
            .get("data")
            .and_then(Value::as_array)
            .expect("semantic token data");
        let mut current_line = 0u64;
        let mut current_char = 0u64;
        let mut out = Vec::new();

        if data.first().is_some_and(Value::is_number) {
            for chunk in data.chunks(5) {
                let delta_line = chunk[0].as_u64().expect("delta line");
                let delta_start = chunk[1].as_u64().expect("delta start");
                current_line += delta_line;
                current_char = if delta_line == 0 {
                    current_char + delta_start
                } else {
                    delta_start
                };
                out.push((current_line, current_char));
            }
            return out;
        }

        for token in data {
            let delta_line = token
                .get("deltaLine")
                .and_then(Value::as_u64)
                .expect("delta line");
            let delta_start = token
                .get("deltaStart")
                .and_then(Value::as_u64)
                .expect("delta start");
            current_line += delta_line;
            current_char = if delta_line == 0 {
                current_char + delta_start
            } else {
                delta_start
            };
            out.push((current_line, current_char));
        }
        out
    }

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
    fn emits_workspace_analysis_status_for_manifest_refresh() {
        let mut state = ServerState::default();
        state.register_workspace_folder("file:///c:/workspace");

        let message = json!({
            "jsonrpc": "2.0",
            "method": "abapls/workspaceManifestUpdated",
            "params": {
                "workspaceUri": "file:///c:/workspace"
            }
        });

        let started = workspace_analysis_status_started(&state, &message)
            .expect("status start")
            .expect("progress should be emitted");
        assert_eq!(started.trigger, "manifest-updated");
        assert_eq!(started.phase, abap_lsp::WorkspaceAnalysisPhase::Started);

        let finished = workspace_analysis_status_finished(&state, &started).expect("status finish");
        assert_eq!(finished.workspace_uri, "file:///c:/workspace");
        assert_eq!(finished.phase, abap_lsp::WorkspaceAnalysisPhase::Finished);
        assert_eq!(finished.analyzed_document_count, 0);
        assert!(!finished.remote_resolution_in_flight);
    }

    #[test]
    fn initialize_registers_root_uri_when_workspace_folders_are_missing() {
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        let workspace_uri = "file:///c:/root-uri-workspace";

        let initialized = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "initialize",
                "params": {
                    "rootUri": workspace_uri,
                    "workspaceFolders": []
                }
            }),
        )
        .expect("initialize");

        assert!(initialized.response.is_some());
        assert!(state.workspaces.contains_key(workspace_uri));
    }

    #[test]
    fn background_scheduler_stages_workspace_open_and_enqueues_job() {
        let workspace_path = temp_workspace_path("background_open_schedule");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        let workspace_uri = file_uri(&workspace_path);
        let normalized_workspace_uri = abap_lsp::normalize_lsp_uri(&workspace_uri);
        let source_uri = format!("{workspace_uri}/main.abap");
        let normalized_uri = abap_lsp::normalize_lsp_uri(&source_uri);

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        let generations = Arc::new(Mutex::new(HashMap::new()));
        let (task_tx, task_rx) = mpsc::sync_channel(1);
        let message = json!({
            "jsonrpc": "2.0",
            "method": "textDocument/didOpen",
            "params": {
                "textDocument": {
                    "uri": source_uri,
                    "languageId": "abap",
                    "version": 3,
                    "text": "DATA lv_value TYPE i."
                }
            }
        });

        let started =
            try_schedule_background_analysis(&mut state, &message, &task_tx, &generations)
                .expect("schedule")
                .expect("background job");
        assert_eq!(started.len(), 1);
        assert_eq!(started[0].trigger, "open");

        let workspace = state
            .workspaces
            .get(&normalized_workspace_uri)
            .expect("workspace");
        let overlay = workspace
            .open_documents
            .get(&normalized_uri)
            .expect("staged overlay");
        assert_eq!(overlay.version, 3);
        assert_eq!(overlay.text.as_ref(), "DATA lv_value TYPE i.");

        let task = task_rx.recv().expect("scheduled task");
        assert_eq!(task.workspace_uri, normalized_workspace_uri);
        assert_eq!(task.generation, 1);

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn background_scheduler_exposes_preview_semantic_tokens_for_commented_out_statement() {
        let workspace_path = temp_workspace_path("background_preview_semantic_tokens");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        let workspace_uri = file_uri(&workspace_path);
        let source_path = workspace_path.join("main.abap");
        let source_uri = format!("{workspace_uri}/main.abap");
        fs::write(&source_path, "DATA lv_value TYPE i.\nlv_value = 1.\n").expect("source");

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let generations = Arc::new(Mutex::new(HashMap::new()));
        let (task_tx, _task_rx) = mpsc::sync_channel(1);
        let changed_text = "DATA lv_value TYPE i.\n* lv_value = 1.\n";
        let message = json!({
            "jsonrpc": "2.0",
            "method": "textDocument/didChange",
            "params": {
                "textDocument": {
                    "uri": source_uri,
                    "version": 2
                },
                "contentChanges": [{
                    "text": changed_text
                }]
            }
        });

        try_schedule_background_analysis(&mut state, &message, &task_tx, &generations)
            .expect("schedule")
            .expect("background job");

        let semantic_tokens_msg = handle_message(
            &mut state,
            &ServerConfig::default(),
            json!({
                "jsonrpc": "2.0",
                "id": 7,
                "method": "textDocument/semanticTokens/full",
                "params": { "textDocument": { "uri": source_uri } }
            }),
        )
        .expect("semantic tokens");
        let result = semantic_tokens_msg
            .response
            .expect("semantic tokens response")
            .result
            .expect("semantic tokens result");
        let positions = semantic_token_positions(&result);

        assert!(
            positions.iter().all(|(line, _)| *line == 0),
            "preview semantic tokens should align with commented statement text: {positions:?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn workspace_open_publishes_diagnostics_for_all_rebuilt_snapshots() {
        let workspace_path = temp_workspace_path("workspace_diagnostics_fanout");
        let source_dir = workspace_path.join("src");
        fs::create_dir_all(&source_dir).expect("source dir");
        fs::write(
            workspace_path.join("abapls.toml"),
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
name = "ZREPORT_TWO"
kind = "report"
root_file = "src/ZREPORT_TWO.abap"

[[unit.member]]
role = "root"
file = "src/ZREPORT_TWO.abap"
object_name = "ZREPORT_TWO"
"#,
        )
        .expect("manifest");
        fs::write(
            source_dir.join("ZCL_MAIN.abap"),
            "CLASS zcl_main DEFINITION. ENDCLASS.",
        )
        .expect("main");
        fs::write(source_dir.join("ZREPORT_TWO.abap"), "REPORT zreport_two.").expect("report");

        let workspace_uri = file_uri(&workspace_path);

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        let config = ServerConfig::default();

        let opened = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": format!("{workspace_uri}/src/ZCL_MAIN.abap"),
                        "languageId": "abap",
                        "version": 1,
                        "text": "CLASS zcl_main DEFINITION. ENDCLASS."
                    }
                }
            }),
        )
        .expect("didOpen");

        let diagnostic_uris: Vec<_> = opened
            .notifications
            .iter()
            .filter(|(method, _)| method == "textDocument/publishDiagnostics")
            .filter_map(|(_, payload)| payload.get("uri").and_then(|value| value.as_str()))
            .collect();

        assert!(
            diagnostic_uris
                .iter()
                .any(|uri| uri.ends_with("/src/ZCL_MAIN.abap"))
        );
        assert!(
            diagnostic_uris
                .iter()
                .any(|uri| uri.ends_with("/src/ZREPORT_TWO.abap"))
        );
        assert!(
            diagnostic_uris
                .iter()
                .any(|uri| uri.ends_with("/abapls.toml"))
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn initialized_triggers_workspace_analysis_for_registered_workspace() {
        let workspace_path = temp_workspace_path("workspace_initialized_refresh");
        let source_dir = workspace_path.join("src");
        fs::create_dir_all(&source_dir).expect("source dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[[unit]]
name = "ZREPORT_INIT"
kind = "report"
root_file = "src/ZREPORT_INIT.abap"

[[unit.member]]
role = "root"
file = "src/ZREPORT_INIT.abap"
object_name = "ZREPORT_INIT"
"#,
        )
        .expect("manifest");
        fs::write(source_dir.join("ZREPORT_INIT.abap"), "REPORT zreport_init.").expect("report");

        let workspace_uri = file_uri(&workspace_path);
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "initialize",
                "params": {
                    "rootUri": workspace_uri,
                    "workspaceFolders": []
                }
            }),
        )
        .expect("initialize");

        let initialized = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "initialized",
                "params": {}
            }),
        )
        .expect("initialized");

        let diagnostic_uris: Vec<_> = initialized
            .notifications
            .iter()
            .filter(|(method, _)| method == "textDocument/publishDiagnostics")
            .filter_map(|(_, payload)| payload.get("uri").and_then(|value| value.as_str()))
            .collect();
        assert!(
            diagnostic_uris
                .iter()
                .any(|uri| uri.ends_with("/src/ZREPORT_INIT.abap"))
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn unchanged_workspace_open_does_not_fan_out_workspace_diagnostics_again() {
        let workspace_path = temp_workspace_path("workspace_unchanged_open_notifications");
        let source_dir = workspace_path.join("src");
        fs::create_dir_all(&source_dir).expect("source dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[[unit]]
name = "ZCL_ONE"
kind = "global-class"
root_file = "src/ZCL_ONE.abap"

[[unit.member]]
role = "main"
file = "src/ZCL_ONE.abap"
object_name = "ZCL_ONE"

[[unit]]
name = "ZCL_TWO"
kind = "global-class"
root_file = "src/ZCL_TWO.abap"

[[unit.member]]
role = "main"
file = "src/ZCL_TWO.abap"
object_name = "ZCL_TWO"
"#,
        )
        .expect("manifest");
        fs::write(
            source_dir.join("ZCL_ONE.abap"),
            "CLASS zcl_one DEFINITION. ENDCLASS.",
        )
        .expect("one");
        fs::write(
            source_dir.join("ZCL_TWO.abap"),
            "CLASS zcl_two DEFINITION. ENDCLASS.",
        )
        .expect("two");

        let workspace_uri = file_uri(&workspace_path);
        let open_uri = format!("{workspace_uri}/src/ZCL_ONE.abap");
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        let config = ServerConfig::default();

        handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "initialized",
                "params": {}
            }),
        )
        .expect("initialized");

        let opened = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": open_uri,
                        "languageId": "abap",
                        "version": 1,
                        "text": "CLASS zcl_one DEFINITION. ENDCLASS."
                    }
                }
            }),
        )
        .expect("didOpen");

        let diagnostic_uris: Vec<_> = opened
            .notifications
            .iter()
            .filter(|(method, _)| method == "textDocument/publishDiagnostics")
            .filter_map(|(_, payload)| payload.get("uri").and_then(|value| value.as_str()))
            .collect();
        assert_eq!(diagnostic_uris.len(), 2);
        assert!(
            diagnostic_uris
                .iter()
                .any(|uri| uri.ends_with("/src/ZCL_ONE.abap"))
        );
        assert!(
            diagnostic_uris
                .iter()
                .any(|uri| uri.ends_with("/abapls.toml"))
        );

        let _ = fs::remove_dir_all(&workspace_path);
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
