use std::collections::{HashMap, HashSet};
use std::io::{self, BufRead, BufReader, BufWriter, Write};
use std::net::{SocketAddr, TcpListener};
use std::sync::mpsc::{self, Receiver, RecvTimeoutError, SyncSender};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

use abap_jsonrpc::{JSON_RPC_VERSION, Response, read_frame, write_frame};
use abap_lsp::{
    CompletionParams, DEPENDENCY_CACHE_CLEARED, DidChangeTextDocumentParams,
    DidOpenTextDocumentParams, GotoDefinitionParams, HoverParams, REMOTE_DEPENDENCIES_UPDATED,
    RESOLVE_REMOTE_DEPENDENCIES, ReferenceParams, SemanticTokensParams, ServerConfig, ServerState,
    WORKSPACE_ANALYSIS_STATUS, WORKSPACE_MANIFEST_UPDATED, WorkspaceAnalysisPhase,
    WorkspaceAnalysisStatusParams, WorkspaceManifestUpdatedParams, WorkspacePerformanceMode,
    WorkspaceState, build_remote_dependency_batch_for_workspace,
    build_remote_dependency_batch_for_workspace_filtered, completion, definition,
    handle_dependency_cache_cleared_with_progress,
    handle_remote_dependencies_updated_with_progress,
    handle_workspace_manifest_updated_with_progress, hover, initialize_result,
    prune_workspace_preview_snapshots, publish_changed_document_mut_with_progress,
    publish_diagnostics_params, publish_open_document_mut_with_progress, references,
    refresh_workspace_with_progress, semantic_tokens, stage_workspace_preview_snapshot,
    workspace_manifest_diagnostics_params,
};
use serde_json::{Value, json};
use tracing::{debug, warn};

const METHOD_NOT_FOUND: i64 = -32601;
const INVALID_REQUEST: i64 = -32600;
const CHANGE_ANALYSIS_DEBOUNCE: Duration = Duration::from_millis(250);
const EDITOR_FIRST_DIAGNOSTIC_LIMIT: usize = 16;

#[derive(Debug, Clone, Default, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct InitializeParamsLite {
    #[serde(default)]
    workspace_folders: Vec<WorkspaceFolderLite>,
    #[serde(default)]
    root_uri: Option<String>,
    #[serde(default, rename = "capabilities")]
    capabilities: InitializeCapabilitiesLite,
}

#[derive(Debug, Clone, Default, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct WorkspaceFolderLite {
    uri: String,
}

#[derive(Debug, Clone, Default, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct InitializeCapabilitiesLite {
    #[serde(default, rename = "window")]
    _window: WindowCapabilitiesLite,
    #[serde(default, rename = "textDocument")]
    text_document: TextDocumentCapabilitiesLite,
}

#[derive(Debug, Clone, Default, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct WindowCapabilitiesLite {
    #[serde(rename = "workDoneProgress")]
    _work_done_progress: Option<bool>,
}

#[derive(Debug, Clone, Default, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct TextDocumentCapabilitiesLite {
    #[serde(default)]
    completion: CompletionCapabilitiesLite,
}

#[derive(Debug, Clone, Default, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct CompletionCapabilitiesLite {
    #[serde(default, rename = "completionItem")]
    completion_item: CompletionItemCapabilitiesLite,
}

#[derive(Debug, Clone, Default, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct CompletionItemCapabilitiesLite {
    #[serde(default)]
    snippet_support: bool,
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

struct ScheduledBackgroundWork {
    started_statuses: Vec<WorkspaceAnalysisStatusParams>,
    notifications: Vec<(String, Value)>,
}

struct DebouncedAnalysisTask {
    task: AnalysisTask,
    due_at: Instant,
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

fn workspace_uses_editor_first_mode(state: &ServerState, workspace_uri: &str) -> bool {
    state
        .workspaces
        .get(workspace_uri)
        .is_some_and(|workspace| {
            workspace.performance_mode == WorkspacePerformanceMode::EditorFirst
        })
}

fn enqueue_background_task(
    task: AnalysisTask,
    task_tx: &SyncSender<String>,
    pending_tasks: &Arc<Mutex<HashMap<String, AnalysisTask>>>,
) -> Result<(), Box<dyn std::error::Error>> {
    let workspace_uri = task.workspace_uri.clone();
    pending_tasks
        .lock()
        .expect("pending analysis tasks should not be poisoned")
        .insert(workspace_uri.clone(), task);
    task_tx
        .send(workspace_uri)
        .map_err(|error| format!("failed to enqueue analysis task: {error}"))?;
    Ok(())
}

fn flush_due_debounced_tasks(
    now: Instant,
    debounced_tasks: &mut HashMap<String, DebouncedAnalysisTask>,
    task_tx: &SyncSender<String>,
    pending_tasks: &Arc<Mutex<HashMap<String, AnalysisTask>>>,
) -> Result<Vec<WorkspaceAnalysisStatusParams>, Box<dyn std::error::Error>> {
    let mut ready = Vec::new();
    let mut ready_keys = Vec::new();
    for (workspace_uri, entry) in debounced_tasks.iter() {
        if entry.due_at <= now {
            ready_keys.push(workspace_uri.clone());
        }
    }
    ready_keys.sort();
    for workspace_uri in ready_keys {
        let Some(entry) = debounced_tasks.remove(&workspace_uri) else {
            continue;
        };
        debug!(workspace_uri = %workspace_uri, "flushing debounced workspace change analysis");
        if let Some(started) = entry.task.started.clone() {
            ready.push(started);
        }
        enqueue_background_task(entry.task, task_tx, pending_tasks)?;
    }
    Ok(ready)
}

fn try_schedule_background_analysis(
    state: &mut ServerState,
    message: &Value,
    task_tx: &SyncSender<String>,
    pending_tasks: &Arc<Mutex<HashMap<String, AnalysisTask>>>,
    generations: &Arc<Mutex<HashMap<String, u64>>>,
    debounced_tasks: &mut HashMap<String, DebouncedAnalysisTask>,
) -> Result<Option<ScheduledBackgroundWork>, Box<dyn std::error::Error>> {
    let Some(method) = message.get("method").and_then(Value::as_str) else {
        return Ok(None);
    };

    let mut started_statuses = Vec::new();
    let mut notifications = Vec::new();

    match method {
        "textDocument/didOpen" => {
            let Some(params) = parse_params::<DidOpenTextDocumentParams>(message)? else {
                return Ok(Some(ScheduledBackgroundWork {
                    started_statuses,
                    notifications,
                }));
            };
            let Some(workspace_uri) = stage_workspace_open_overlay(state, &params) else {
                return Ok(None);
            };
            if let Some(snapshot) = state
                .workspace_for_uri(&workspace_uri)
                .and_then(|workspace| {
                    workspace
                        .preview_snapshots
                        .get(&abap_lsp::normalize_lsp_uri(
                            params.text_document.uri.as_str(),
                        ))
                })
            {
                notifications.push((
                    "textDocument/publishDiagnostics".to_owned(),
                    serde_json::to_value(publish_diagnostics_params(state, snapshot))?,
                ));
            }
            push_workspace_manifest_diagnostics_notification(
                state,
                &workspace_uri,
                &mut notifications,
            );
            let Some(workspace) = state.workspaces.get(&workspace_uri).cloned() else {
                return Ok(None);
            };
            let started = workspace_analysis_status_started(state, message)?;
            if let Some(params) = started.clone() {
                started_statuses.push(params);
            }
            let task = AnalysisTask {
                workspace_uri: workspace_uri.clone(),
                generation: next_workspace_generation(generations, &workspace_uri),
                started,
                workspace,
                kind: AnalysisTaskKind::DidOpen(params),
            };
            debounced_tasks.remove(&workspace_uri);
            enqueue_background_task(task, task_tx, pending_tasks)?;
            Ok(Some(ScheduledBackgroundWork {
                started_statuses,
                notifications,
            }))
        }
        "textDocument/didChange" => {
            let Some(params) = parse_params::<DidChangeTextDocumentParams>(message)? else {
                return Ok(Some(ScheduledBackgroundWork {
                    started_statuses,
                    notifications,
                }));
            };
            let Some(workspace_uri) = stage_workspace_change_overlay(state, &params) else {
                return Ok(None);
            };
            let normalized_uri = abap_lsp::normalize_lsp_uri(params.text_document.uri.as_str());
            if let Some(snapshot) = state
                .workspace_for_uri(&workspace_uri)
                .and_then(|workspace| workspace.preview_snapshots.get(&normalized_uri))
            {
                notifications.push((
                    "textDocument/publishDiagnostics".to_owned(),
                    serde_json::to_value(publish_diagnostics_params(state, snapshot))?,
                ));
            }
            push_workspace_manifest_diagnostics_notification(
                state,
                &workspace_uri,
                &mut notifications,
            );
            let Some(workspace) = state.workspaces.get(&workspace_uri).cloned() else {
                return Ok(None);
            };
            let started = workspace_analysis_status_started(state, message)?;
            if let Some(params) = started.clone() {
                started_statuses.push(params);
            }
            let task = AnalysisTask {
                workspace_uri: workspace_uri.clone(),
                generation: next_workspace_generation(generations, &workspace_uri),
                started,
                workspace,
                kind: AnalysisTaskKind::DidChange(params),
            };
            if workspace_uses_editor_first_mode(state, &workspace_uri) {
                debug!(
                    workspace_uri = %workspace_uri,
                    due_in_ms = CHANGE_ANALYSIS_DEBOUNCE.as_millis(),
                    "debouncing workspace change analysis"
                );
                debounced_tasks.insert(
                    workspace_uri,
                    DebouncedAnalysisTask {
                        task,
                        due_at: Instant::now() + CHANGE_ANALYSIS_DEBOUNCE,
                    },
                );
                started_statuses.clear();
            } else {
                enqueue_background_task(task, task_tx, pending_tasks)?;
            }
            Ok(Some(ScheduledBackgroundWork {
                started_statuses,
                notifications,
            }))
        }
        WORKSPACE_MANIFEST_UPDATED => {
            let Some(params) = parse_params::<WorkspaceManifestUpdatedParams>(message)? else {
                return Ok(Some(ScheduledBackgroundWork {
                    started_statuses,
                    notifications,
                }));
            };
            let workspace_uri = abap_lsp::normalize_lsp_uri(&params.workspace_uri);
            let Some(workspace) = state.workspaces.get(&workspace_uri).cloned() else {
                return Ok(Some(ScheduledBackgroundWork {
                    started_statuses,
                    notifications,
                }));
            };
            let started = workspace_analysis_status_started(state, message)?;
            if let Some(params) = started.clone() {
                started_statuses.push(params);
            }
            debounced_tasks.remove(&workspace_uri);
            enqueue_background_task(
                AnalysisTask {
                    workspace_uri: workspace_uri.clone(),
                    generation: next_workspace_generation(generations, &workspace_uri),
                    started,
                    workspace,
                    kind: AnalysisTaskKind::ManifestUpdated(WorkspaceManifestUpdatedParams {
                        workspace_uri: workspace_uri.clone(),
                    }),
                },
                task_tx,
                pending_tasks,
            )?;
            Ok(Some(ScheduledBackgroundWork {
                started_statuses,
                notifications,
            }))
        }
        DEPENDENCY_CACHE_CLEARED => {
            let Some(params) = parse_params::<WorkspaceManifestUpdatedParams>(message)? else {
                return Ok(Some(ScheduledBackgroundWork {
                    started_statuses,
                    notifications,
                }));
            };
            let workspace_uri = abap_lsp::normalize_lsp_uri(&params.workspace_uri);
            let Some(workspace) = state.workspaces.get(&workspace_uri).cloned() else {
                return Ok(Some(ScheduledBackgroundWork {
                    started_statuses,
                    notifications,
                }));
            };
            let started = workspace_analysis_status_started(state, message)?;
            if let Some(params) = started.clone() {
                started_statuses.push(params);
            }
            debounced_tasks.remove(&workspace_uri);
            enqueue_background_task(
                AnalysisTask {
                    workspace_uri: workspace_uri.clone(),
                    generation: next_workspace_generation(generations, &workspace_uri),
                    started,
                    workspace,
                    kind: AnalysisTaskKind::DependencyCacheCleared(
                        WorkspaceManifestUpdatedParams {
                            workspace_uri: workspace_uri.clone(),
                        },
                    ),
                },
                task_tx,
                pending_tasks,
            )?;
            Ok(Some(ScheduledBackgroundWork {
                started_statuses,
                notifications,
            }))
        }
        REMOTE_DEPENDENCIES_UPDATED => {
            let Some(params) = parse_params::<abap_lsp::RemoteDependenciesUpdatedParams>(message)?
            else {
                return Ok(Some(ScheduledBackgroundWork {
                    started_statuses,
                    notifications,
                }));
            };
            let workspace_uri = abap_lsp::normalize_lsp_uri(&params.workspace_uri);
            let Some(workspace) = state.workspaces.get(&workspace_uri).cloned() else {
                return Ok(Some(ScheduledBackgroundWork {
                    started_statuses,
                    notifications,
                }));
            };
            let started = workspace_analysis_status_started(state, message)?;
            if let Some(params) = started.clone() {
                started_statuses.push(params);
            }
            debounced_tasks.remove(&workspace_uri);
            enqueue_background_task(
                AnalysisTask {
                    workspace_uri: workspace_uri.clone(),
                    generation: next_workspace_generation(generations, &workspace_uri),
                    started,
                    workspace,
                    kind: AnalysisTaskKind::RemoteDependenciesUpdated(
                        abap_lsp::RemoteDependenciesUpdatedParams {
                            workspace_uri: workspace_uri.clone(),
                            ..params
                        },
                    ),
                },
                task_tx,
                pending_tasks,
            )?;
            Ok(Some(ScheduledBackgroundWork {
                started_statuses,
                notifications,
            }))
        }
        "initialized" => {
            let workspace_uris: Vec<_> = state.workspaces.keys().cloned().collect();
            for workspace_uri in workspace_uris {
                let Some(workspace) = state.workspaces.get(&workspace_uri).cloned() else {
                    continue;
                };
                debounced_tasks.remove(&workspace_uri);
                enqueue_background_task(
                    AnalysisTask {
                        workspace_uri: workspace_uri.clone(),
                        generation: next_workspace_generation(generations, &workspace_uri),
                        started: workspace_analysis_status_started(state, message)?,
                        workspace,
                        kind: AnalysisTaskKind::Initialized,
                    },
                    task_tx,
                    pending_tasks,
                )?;
            }
            Ok(Some(ScheduledBackgroundWork {
                started_statuses,
                notifications,
            }))
        }
        _ => Ok(None),
    }
}

fn run_analysis_task(task: AnalysisTask) -> Result<AnalysisCompletion, Box<dyn std::error::Error>> {
    let mut state = ServerState {
        cache: Default::default(),
        workspaces: HashMap::from([(task.workspace_uri.clone(), task.workspace)]),
        client_capabilities: Default::default(),
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
    let pending_tasks = Arc::new(Mutex::new(HashMap::<String, AnalysisTask>::new()));
    let (task_tx, task_rx): (SyncSender<String>, Receiver<String>) = mpsc::sync_channel(8);
    let (completion_tx, completion_rx) = mpsc::channel();
    let mut debounced_tasks = HashMap::<String, DebouncedAnalysisTask>::new();

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
        let worker_pending_tasks = Arc::clone(&pending_tasks);
        scope.spawn(move || {
            while let Ok(workspace_uri) = task_rx.recv() {
                let Some(task) = worker_pending_tasks
                    .lock()
                    .expect("pending analysis tasks should not be poisoned")
                    .remove(&workspace_uri)
                else {
                    continue;
                };
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
            for params in flush_due_debounced_tasks(
                Instant::now(),
                &mut debounced_tasks,
                &task_tx,
                &pending_tasks,
            )? {
                send_notification(
                    writer,
                    WORKSPACE_ANALYSIS_STATUS,
                    serde_json::to_value(params)?,
                )?;
            }
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
                        &pending_tasks,
                        &generations,
                        &mut debounced_tasks,
                    )? {
                        for (method, params) in started_statuses.notifications {
                            send_notification(writer, &method, params)?;
                        }
                        for params in started_statuses.started_statuses {
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

                    for params in flush_due_debounced_tasks(
                        Instant::now(),
                        &mut debounced_tasks,
                        &task_tx,
                        &pending_tasks,
                    )? {
                        send_notification(
                            writer,
                            WORKSPACE_ANALYSIS_STATUS,
                            serde_json::to_value(params)?,
                        )?;
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

        for params in flush_due_debounced_tasks(
            Instant::now() + CHANGE_ANALYSIS_DEBOUNCE,
            &mut debounced_tasks,
            &task_tx,
            &pending_tasks,
        )? {
            send_notification(
                writer,
                WORKSPACE_ANALYSIS_STATUS,
                serde_json::to_value(params)?,
            )?;
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

fn push_publish_diagnostics_notification(
    state: &ServerState,
    snapshot: &abap_lsp::AnalysisSnapshot,
    notifications: &mut Vec<(String, Value)>,
) -> Result<(), Box<dyn std::error::Error>> {
    notifications.push((
        "textDocument/publishDiagnostics".to_owned(),
        serde_json::to_value(publish_diagnostics_params(state, snapshot))?,
    ));
    Ok(())
}

fn push_workspace_manifest_diagnostics_notification(
    state: &ServerState,
    workspace_uri: &str,
    notifications: &mut Vec<(String, Value)>,
) {
    if let Some(params_value) = workspace_manifest_diagnostics_params(state, workspace_uri)
        .and_then(|params| serde_json::to_value(params).ok())
    {
        notifications.push(("textDocument/publishDiagnostics".to_owned(), params_value));
    }
}

fn push_workspace_diagnostics_notifications(
    state: &ServerState,
    workspace_uri: &str,
    notifications: &mut Vec<(String, Value)>,
) -> Result<(), Box<dyn std::error::Error>> {
    push_workspace_diagnostics_notifications_for_uris(state, workspace_uri, None, notifications)
}

fn push_workspace_diagnostics_notifications_for_uris(
    state: &ServerState,
    workspace_uri: &str,
    dirty_uris: Option<&HashSet<Arc<str>>>,
    notifications: &mut Vec<(String, Value)>,
) -> Result<(), Box<dyn std::error::Error>> {
    let Some(workspace) = state.workspaces.get(workspace_uri) else {
        return Ok(());
    };
    let mut uris: Vec<_> = if let Some(dirty_uris) = dirty_uris {
        dirty_uris.iter().cloned().collect()
    } else {
        workspace.cache.uris()
    };
    uris.sort();
    for uri in uris {
        let Some(snapshot) = workspace.cache.get(uri.as_ref()) else {
            continue;
        };
        push_publish_diagnostics_notification(state, snapshot.as_ref(), notifications)?;
    }
    Ok(())
}

fn workspace_open_dirty_uris(
    state: &ServerState,
    workspace_uri: &str,
    dirty_uris: &HashSet<Arc<str>>,
) -> HashSet<Arc<str>> {
    let Some(workspace) = state.workspaces.get(workspace_uri) else {
        return HashSet::new();
    };
    workspace
        .open_documents
        .keys()
        .filter(|uri| dirty_uris.contains(uri.as_str()))
        .filter_map(|uri| {
            workspace
                .cache
                .get(uri)
                .map(|_| Arc::<str>::from(uri.as_str()))
        })
        .collect()
}

fn editor_first_diagnostic_uris(
    state: &ServerState,
    workspace_uri: &str,
    changed_uri: Option<&str>,
    dirty_uris: &HashSet<Arc<str>>,
) -> Vec<Arc<str>> {
    let Some(workspace) = state.workspaces.get(workspace_uri) else {
        return Vec::new();
    };
    let mut selected = Vec::new();
    let mut seen = HashSet::new();

    if let Some(uri) = changed_uri {
        if dirty_uris.contains(uri)
            && workspace
                .cache
                .get(uri)
                .is_some_and(|snapshot| !snapshot.is_dependency)
            && seen.insert(uri.to_owned())
        {
            selected.push(Arc::<str>::from(uri));
        }
    }

    let mut extras: Vec<_> = workspace
        .open_documents
        .keys()
        .filter(|uri| dirty_uris.contains(uri.as_str()))
        .filter(|uri| seen.insert((*uri).clone()))
        .filter_map(|uri| {
            workspace
                .cache
                .get(uri)
                .filter(|snapshot| !snapshot.is_dependency)
                .map(|_| Arc::<str>::from(uri.as_str()))
        })
        .collect();
    extras.sort();
    selected.extend(
        extras
            .into_iter()
            .take(EDITOR_FIRST_DIAGNOSTIC_LIMIT - selected.len()),
    );
    selected
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
        push_publish_diagnostics_notification(state, &snapshot, &mut notifications)?;
    } else if let Some(workspace_uri) = state
        .workspace_for_uri(snapshot.uri.as_ref())
        .filter(|workspace| workspace.cache.get(snapshot.uri.as_ref()).is_some())
        .map(|workspace| workspace.root_uri.clone())
    {
        let dirty_uris = state
            .workspaces
            .get(&workspace_uri)
            .map(|workspace| workspace.cache.last_dirty_uris())
            .unwrap_or_default();
        if workspace_uses_editor_first_mode(state, &workspace_uri) {
            let selected_uris = editor_first_diagnostic_uris(
                state,
                &workspace_uri,
                Some(snapshot.uri.as_ref()),
                &dirty_uris,
            );
            debug!(
                workspace_uri = %workspace_uri,
                dirty_uri_count = dirty_uris.len(),
                selected_uri_count = selected_uris.len(),
                "publishing editor-first open diagnostics"
            );
            for uri in selected_uris {
                if let Some(snapshot) = state
                    .workspaces
                    .get(&workspace_uri)
                    .and_then(|workspace| workspace.cache.get(uri.as_ref()))
                {
                    push_publish_diagnostics_notification(
                        state,
                        snapshot.as_ref(),
                        &mut notifications,
                    )?;
                }
            }
        } else {
            push_workspace_diagnostics_notifications_for_uris(
                state,
                &workspace_uri,
                Some(&dirty_uris),
                &mut notifications,
            )?;
        }
    } else {
        push_publish_diagnostics_notification(state, &snapshot, &mut notifications)?;
    }
    if let Some(workspace_uri) = state
        .workspace_for_uri(snapshot.uri.as_ref())
        .map(|workspace| workspace.root_uri.clone())
    {
        push_workspace_manifest_diagnostics_notification(state, &workspace_uri, &mut notifications);
    }
    if !unchanged_workspace_open
        && let Some(workspace_uri) = state
            .workspace_for_uri(snapshot.uri.as_ref())
            .filter(|workspace| workspace.cache.get(snapshot.uri.as_ref()).is_some())
            .map(|workspace| workspace.root_uri.clone())
        && let Some(dirty_uris) = state
            .workspaces
            .get(&workspace_uri)
            .map(|workspace| workspace.cache.last_dirty_uris())
        && let Some(request) = if workspace_uses_editor_first_mode(state, &workspace_uri) {
            let open_dirty = workspace_open_dirty_uris(state, &workspace_uri, &dirty_uris);
            debug!(
                workspace_uri = %workspace_uri,
                dirty_uri_count = dirty_uris.len(),
                open_dirty_uri_count = open_dirty.len(),
                "building editor-first open remote dependency batch"
            );
            build_remote_dependency_batch_for_workspace_filtered(
                state,
                &workspace_uri,
                Some(&open_dirty),
            )
        } else {
            build_remote_dependency_batch_for_workspace_filtered(
                state,
                &workspace_uri,
                Some(&dirty_uris),
            )
        }
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
            push_publish_diagnostics_notification(state, &snapshot, &mut notifications)?;
        } else if let Some(workspace_uri) = state
            .workspace_for_uri(snapshot.uri.as_ref())
            .filter(|workspace| workspace.cache.get(snapshot.uri.as_ref()).is_some())
            .map(|workspace| workspace.root_uri.clone())
        {
            let dirty_uris = state
                .workspaces
                .get(&workspace_uri)
                .map(|workspace| workspace.cache.last_dirty_uris())
                .unwrap_or_default();
            if workspace_uses_editor_first_mode(state, &workspace_uri) {
                let selected_uris = editor_first_diagnostic_uris(
                    state,
                    &workspace_uri,
                    Some(snapshot.uri.as_ref()),
                    &dirty_uris,
                );
                debug!(
                    workspace_uri = %workspace_uri,
                    dirty_uri_count = dirty_uris.len(),
                    selected_uri_count = selected_uris.len(),
                    "publishing editor-first change diagnostics"
                );
                for uri in selected_uris {
                    if let Some(snapshot) = state
                        .workspaces
                        .get(&workspace_uri)
                        .and_then(|workspace| workspace.cache.get(uri.as_ref()))
                    {
                        push_publish_diagnostics_notification(
                            state,
                            snapshot.as_ref(),
                            &mut notifications,
                        )?;
                    }
                }
            } else {
                push_workspace_diagnostics_notifications_for_uris(
                    state,
                    &workspace_uri,
                    Some(&dirty_uris),
                    &mut notifications,
                )?;
            }
        } else {
            push_publish_diagnostics_notification(state, &snapshot, &mut notifications)?;
        }
        if let Some(workspace_uri) = state
            .workspace_for_uri(snapshot.uri.as_ref())
            .map(|workspace| workspace.root_uri.clone())
        {
            push_workspace_manifest_diagnostics_notification(
                state,
                &workspace_uri,
                &mut notifications,
            );
        }
        if !unchanged_workspace_change
            && let Some(workspace_uri) = state
                .workspace_for_uri(snapshot.uri.as_ref())
                .filter(|workspace| workspace.cache.get(snapshot.uri.as_ref()).is_some())
                .map(|workspace| workspace.root_uri.clone())
            && let Some(dirty_uris) = state
                .workspaces
                .get(&workspace_uri)
                .map(|workspace| workspace.cache.last_dirty_uris())
            && let Some(request) = if workspace_uses_editor_first_mode(state, &workspace_uri) {
                let open_dirty = workspace_open_dirty_uris(state, &workspace_uri, &dirty_uris);
                debug!(
                    workspace_uri = %workspace_uri,
                    dirty_uri_count = dirty_uris.len(),
                    open_dirty_uri_count = open_dirty.len(),
                    "building editor-first change remote dependency batch"
                );
                build_remote_dependency_batch_for_workspace_filtered(
                    state,
                    &workspace_uri,
                    Some(&open_dirty),
                )
            } else {
                build_remote_dependency_batch_for_workspace_filtered(
                    state,
                    &workspace_uri,
                    Some(&dirty_uris),
                )
            }
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
        if source_uris.contains(snapshot.uri.as_ref())
            && (!workspace_uses_editor_first_mode(state, &params.workspace_uri)
                || state
                    .workspaces
                    .get(&params.workspace_uri)
                    .is_some_and(|workspace| {
                        workspace.open_documents.contains_key(snapshot.uri.as_ref())
                    })
                    && !snapshot.is_dependency)
        {
            push_publish_diagnostics_notification(state, snapshot, &mut notifications)?;
        }
    }
    let request = if workspace_uses_editor_first_mode(state, &params.workspace_uri) {
        let source_filter: HashSet<_> = source_uris
            .iter()
            .filter_map(|uri| {
                state
                    .workspaces
                    .get(&params.workspace_uri)
                    .and_then(|workspace| {
                        workspace
                            .open_documents
                            .contains_key(uri)
                            .then_some(uri.as_str())
                            .and_then(|uri| {
                                workspace
                                    .cache
                                    .get(uri)
                                    .filter(|snapshot| !snapshot.is_dependency)
                                    .map(|_| Arc::<str>::from(uri))
                            })
                    })
            })
            .collect();
        debug!(
            workspace_uri = %params.workspace_uri,
            source_uri_count = source_uris.len(),
            filtered_source_uri_count = source_filter.len(),
            "building editor-first post-remote dependency batch"
        );
        build_remote_dependency_batch_for_workspace_filtered(
            state,
            &params.workspace_uri,
            Some(&source_filter),
        )
    } else {
        build_remote_dependency_batch_for_workspace(state, &params.workspace_uri)
    };
    if let Some(request) = request {
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
                state.client_capabilities.completion_snippet_support = params
                    .capabilities
                    .text_document
                    .completion
                    .completion_item
                    .snippet_support;
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
    use std::time::{Instant, SystemTime, UNIX_EPOCH};

    use super::{
        AnalysisTaskKind, CHANGE_ANALYSIS_DEBOUNCE, EDITOR_FIRST_DIAGNOSTIC_LIMIT,
        RESOLVE_REMOTE_DEPENDENCIES, flush_due_debounced_tasks, handle_did_change_notifications,
        handle_message, run_analysis_task, try_schedule_background_analysis,
        workspace_analysis_status_finished, workspace_analysis_status_started,
    };
    use abap_lsp::{
        DidChangeTextDocumentParams, ServerConfig, ServerState, WorkspacePerformanceMode,
        refresh_workspace,
    };
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

    fn write_manifest_workspace(
        workspace_path: &std::path::Path,
        performance_mode: Option<&str>,
        dependency_mode: Option<&str>,
        local_units: &[(&str, &str, &str, &str)],
        generated_dependency_units: usize,
    ) {
        let mut manifest = String::from("version = 1\n\n");
        if let Some(mode) = performance_mode {
            manifest.push_str("[performance]\n");
            manifest.push_str(&format!("mode = \"{mode}\"\n\n"));
        }
        if let Some(mode) = dependency_mode {
            manifest.push_str("[resolution]\n");
            manifest.push_str(&format!("dependency_mode = \"{mode}\"\n"));
            manifest.push_str("unknown_symbol_mode = \"remote\"\n\n");
        }
        for (name, kind, role, relative_path) in local_units {
            manifest.push_str("[[unit]]\n");
            manifest.push_str(&format!("name = \"{name}\"\n"));
            manifest.push_str(&format!("kind = \"{kind}\"\n"));
            manifest.push_str(&format!("root_file = \"{relative_path}\"\n\n"));
            manifest.push_str("[[unit.member]]\n");
            manifest.push_str(&format!("role = \"{role}\"\n"));
            manifest.push_str(&format!("file = \"{relative_path}\"\n"));
            manifest.push_str(&format!("object_name = \"{name}\"\n\n"));
        }
        for idx in 0..generated_dependency_units {
            manifest.push_str("[[unit]]\n");
            manifest.push_str(&format!("name = \"ZCL_DEP_{idx:04}\"\n"));
            manifest.push_str("kind = \"global-class\"\n");
            manifest.push_str(&format!(
                "root_file = \".abapls/cache/dependencies/global-class/ZCL_DEP_{idx:04}.abap\"\n\n"
            ));
            manifest.push_str("[[unit.member]]\n");
            manifest.push_str("role = \"dependency\"\n");
            manifest.push_str(&format!(
                "file = \".abapls/cache/dependencies/global-class/ZCL_DEP_{idx:04}.abap\"\n"
            ));
            manifest.push_str(&format!("object_name = \"ZCL_DEP_{idx:04}\"\n\n"));
        }
        fs::write(workspace_path.join("abapls.toml"), manifest).expect("manifest");
    }

    fn did_change_params(
        uri: &str,
        version: i32,
        text: impl Into<String>,
    ) -> DidChangeTextDocumentParams {
        serde_json::from_value(json!({
            "textDocument": {
                "uri": uri,
                "version": version,
            },
            "contentChanges": [{
                "text": text.into(),
            }],
        }))
        .expect("didChange params")
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
    fn initialize_records_completion_snippet_support() {
        let mut state = ServerState::default();
        let initialized = handle_message(
            &mut state,
            &ServerConfig::default(),
            json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "initialize",
                "params": {
                    "capabilities": {
                        "textDocument": {
                            "completion": {
                                "completionItem": {
                                    "snippetSupport": true
                                }
                            }
                        }
                    }
                }
            }),
        )
        .expect("initialize");

        assert!(initialized.response.is_some());
        assert!(state.client_capabilities.completion_snippet_support);
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
        let pending_tasks = Arc::new(Mutex::new(HashMap::new()));
        let mut debounced_tasks = HashMap::new();
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

        let started = try_schedule_background_analysis(
            &mut state,
            &message,
            &task_tx,
            &pending_tasks,
            &generations,
            &mut debounced_tasks,
        )
        .expect("schedule")
        .expect("background job");
        assert_eq!(started.started_statuses.len(), 1);
        assert_eq!(started.started_statuses[0].trigger, "open");
        assert_eq!(started.notifications.len(), 1);

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

        let queued_workspace_uri = task_rx.recv().expect("scheduled task");
        assert_eq!(queued_workspace_uri, normalized_workspace_uri);
        let task = pending_tasks
            .lock()
            .expect("pending task map")
            .get(&queued_workspace_uri)
            .expect("pending analysis task")
            .workspace_uri
            .clone();
        assert_eq!(task, normalized_workspace_uri);

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
        let pending_tasks = Arc::new(Mutex::new(HashMap::new()));
        let mut debounced_tasks = HashMap::new();
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

        let scheduled = try_schedule_background_analysis(
            &mut state,
            &message,
            &task_tx,
            &pending_tasks,
            &generations,
            &mut debounced_tasks,
        )
        .expect("schedule")
        .expect("background job");
        assert_eq!(scheduled.notifications.len(), 2);

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
    fn background_scheduler_keeps_workspace_completion_visible_before_commit() {
        let workspace_path = temp_workspace_path("background_preview_completion");
        let source_dir = workspace_path.join("src");
        fs::create_dir_all(&source_dir).expect("source dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[[unit]]
name = "ZCL_HELPER"
kind = "global-class"
root_file = "src/ZCL_HELPER.abap"

[[unit.member]]
role = "main"
file = "src/ZCL_HELPER.abap"
object_name = "ZCL_HELPER"

[[unit]]
name = "ZREPORT_MAIN"
kind = "report"
root_file = "src/ZREPORT_MAIN.abap"

[[unit.member]]
role = "root"
file = "src/ZREPORT_MAIN.abap"
object_name = "ZREPORT_MAIN"
"#,
        )
        .expect("manifest");
        fs::write(
            source_dir.join("ZCL_HELPER.abap"),
            "\
CLASS zcl_helper DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS zcl_helper IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.",
        )
        .expect("helper");
        fs::write(
            source_dir.join("ZREPORT_MAIN.abap"),
            "\
REPORT zreport_main.
DATA lo_helper TYPE REF TO zcl_helper.
lo_helper->",
        )
        .expect("main");

        let workspace_uri = file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/src/ZREPORT_MAIN.abap");
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let generations = Arc::new(Mutex::new(HashMap::new()));
        let pending_tasks = Arc::new(Mutex::new(HashMap::new()));
        let mut debounced_tasks = HashMap::new();
        let (task_tx, _task_rx) = mpsc::sync_channel(1);
        let changed_text = "\
REPORT zreport_main.
DATA lo_helper TYPE REF TO zcl_helper.
lo_helper->r";
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

        let scheduled = try_schedule_background_analysis(
            &mut state,
            &message,
            &task_tx,
            &pending_tasks,
            &generations,
            &mut debounced_tasks,
        )
        .expect("schedule")
        .expect("background job");
        assert_eq!(scheduled.notifications.len(), 2);

        let completion = handle_message(
            &mut state,
            &ServerConfig::default(),
            json!({
                "jsonrpc": "2.0",
                "id": 7,
                "method": "textDocument/completion",
                "params": {
                    "textDocument": { "uri": source_uri },
                    "position": { "line": 2, "character": 11 }
                }
            }),
        )
        .expect("completion");
        let result = completion
            .response
            .expect("completion response")
            .result
            .expect("completion result");
        let labels: Vec<_> = result
            .as_array()
            .expect("completion array")
            .iter()
            .filter_map(|item| item.get("label").and_then(Value::as_str))
            .collect();

        assert!(
            labels.iter().any(|label| *label == "run"),
            "expected helper method completion before workspace commit: {labels:?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn background_scheduler_keeps_only_latest_pending_task_per_workspace() {
        let workspace_path = temp_workspace_path("background_latest_only");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        let workspace_uri = file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/main.abap");

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        let generations = Arc::new(Mutex::new(HashMap::new()));
        let pending_tasks = Arc::new(Mutex::new(HashMap::new()));
        let mut debounced_tasks = HashMap::new();
        let (task_tx, task_rx) = mpsc::sync_channel(4);

        for version in [2, 3] {
            let message = json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didChange",
                "params": {
                    "textDocument": {
                        "uri": source_uri,
                        "version": version
                    },
                    "contentChanges": [{
                        "text": format!("DATA lv_value TYPE i.\nlv_value = {version}.\n")
                    }]
                }
            });
            try_schedule_background_analysis(
                &mut state,
                &message,
                &task_tx,
                &pending_tasks,
                &generations,
                &mut debounced_tasks,
            )
            .expect("schedule")
            .expect("background job");
        }

        let first_workspace = task_rx.recv().expect("first queued workspace");
        assert_eq!(first_workspace, abap_lsp::normalize_lsp_uri(&workspace_uri));
        let pending_guard = pending_tasks.lock().expect("pending tasks");
        let latest_task = pending_guard
            .get(&first_workspace)
            .expect("latest pending task");
        match &latest_task.kind {
            AnalysisTaskKind::DidChange(params) => {
                assert_eq!(params.text_document.version, 3);
            }
            _ => panic!("expected didChange task"),
        }

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn large_manifest_did_change_stays_editor_first_in_foreground() {
        let workspace_path = temp_workspace_path("editor_first_foreground");
        let source_dir = workspace_path.join("src");
        fs::create_dir_all(&source_dir).expect("source dir");
        fs::write(
            source_dir.join("ZCL_HELPER.abap"),
            "\
CLASS zcl_helper DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS zcl_helper IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.",
        )
        .expect("helper");
        fs::write(
            source_dir.join("ZREPORT_MAIN.abap"),
            "\
REPORT zreport_main.
DATA lo_helper TYPE REF TO zcl_helper.
DATA lo_remote TYPE REF TO zcl_remote_demo.
lo_helper->",
        )
        .expect("main");
        write_manifest_workspace(
            &workspace_path,
            None,
            Some("remote-on-demand"),
            &[
                ("ZCL_HELPER", "global-class", "main", "src/ZCL_HELPER.abap"),
                ("ZREPORT_MAIN", "report", "root", "src/ZREPORT_MAIN.abap"),
            ],
            5_000,
        );

        let workspace_uri = file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/src/ZREPORT_MAIN.abap");
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        assert_eq!(
            state
                .workspaces
                .get(&abap_lsp::normalize_lsp_uri(&workspace_uri))
                .expect("workspace")
                .performance_mode,
            WorkspacePerformanceMode::EditorFirst
        );

        let generations = Arc::new(Mutex::new(HashMap::new()));
        let pending_tasks = Arc::new(Mutex::new(HashMap::new()));
        let mut debounced_tasks = HashMap::new();
        let (task_tx, task_rx) = mpsc::sync_channel(1);
        let message = json!({
            "jsonrpc": "2.0",
            "method": "textDocument/didChange",
            "params": {
                "textDocument": {
                    "uri": source_uri,
                    "version": 2
                },
                "contentChanges": [{
                    "text": "\
REPORT zreport_main.
DATA lo_helper TYPE REF TO zcl_helper.
DATA lo_remote TYPE REF TO zcl_remote_demo.
lo_helper->r"
                }]
            }
        });

        let scheduled = try_schedule_background_analysis(
            &mut state,
            &message,
            &task_tx,
            &pending_tasks,
            &generations,
            &mut debounced_tasks,
        )
        .expect("schedule")
        .expect("background job");

        assert!(scheduled.started_statuses.is_empty());
        let foreground_diagnostics: Vec<_> = scheduled
            .notifications
            .iter()
            .filter(|(method, _)| method == "textDocument/publishDiagnostics")
            .filter_map(|(_, payload)| payload.get("uri").and_then(Value::as_str))
            .collect();
        assert_eq!(foreground_diagnostics.len(), 2);
        assert!(
            foreground_diagnostics
                .iter()
                .any(|uri| uri.ends_with("/src/ZREPORT_MAIN.abap"))
        );
        assert!(
            foreground_diagnostics
                .iter()
                .any(|uri| uri.ends_with("/abapls.toml"))
        );
        assert!(
            scheduled
                .notifications
                .iter()
                .all(|(method, _)| method != RESOLVE_REMOTE_DEPENDENCIES)
        );
        assert!(
            task_rx.try_recv().is_err(),
            "editor-first changes should stay debounced"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn large_manifest_change_debounce_collapses_to_latest_task() {
        let workspace_path = temp_workspace_path("editor_first_debounce");
        let source_dir = workspace_path.join("src");
        fs::create_dir_all(&source_dir).expect("source dir");
        fs::write(source_dir.join("ZREPORT_MAIN.abap"), "REPORT zreport_main.").expect("main");
        write_manifest_workspace(
            &workspace_path,
            None,
            Some("remote-on-demand"),
            &[("ZREPORT_MAIN", "report", "root", "src/ZREPORT_MAIN.abap")],
            5_000,
        );

        let workspace_uri = file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/src/ZREPORT_MAIN.abap");
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let generations = Arc::new(Mutex::new(HashMap::new()));
        let pending_tasks = Arc::new(Mutex::new(HashMap::new()));
        let mut debounced_tasks = HashMap::new();
        let (task_tx, task_rx) = mpsc::sync_channel(4);

        for version in [2, 3] {
            let message = json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didChange",
                "params": {
                    "textDocument": {
                        "uri": source_uri,
                        "version": version
                    },
                    "contentChanges": [{
                        "text": format!("REPORT zreport_main.\nDATA lv_value TYPE i VALUE {version}.")
                    }]
                }
            });
            let scheduled = try_schedule_background_analysis(
                &mut state,
                &message,
                &task_tx,
                &pending_tasks,
                &generations,
                &mut debounced_tasks,
            )
            .expect("schedule")
            .expect("background job");
            assert!(scheduled.started_statuses.is_empty());
        }

        assert!(
            task_rx.try_recv().is_err(),
            "debounced tasks should not enqueue immediately"
        );
        let _started = flush_due_debounced_tasks(
            Instant::now() + CHANGE_ANALYSIS_DEBOUNCE,
            &mut debounced_tasks,
            &task_tx,
            &pending_tasks,
        )
        .expect("flush");
        assert!(debounced_tasks.is_empty());
        let queued_workspace = task_rx.recv().expect("queued workspace");
        let pending = pending_tasks.lock().expect("pending tasks");
        match &pending.get(&queued_workspace).expect("latest task").kind {
            AnalysisTaskKind::DidChange(params) => {
                assert_eq!(params.text_document.version, 3);
            }
            _ => panic!("expected didChange task"),
        }

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn large_manifest_preview_completion_stays_visible_while_change_is_debounced() {
        let workspace_path = temp_workspace_path("editor_first_preview_completion");
        let source_dir = workspace_path.join("src");
        fs::create_dir_all(&source_dir).expect("source dir");
        fs::write(
            source_dir.join("ZCL_HELPER.abap"),
            "\
CLASS zcl_helper DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS zcl_helper IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.",
        )
        .expect("helper");
        fs::write(
            source_dir.join("ZREPORT_MAIN.abap"),
            "\
REPORT zreport_main.
DATA lo_helper TYPE REF TO zcl_helper.
lo_helper->",
        )
        .expect("main");
        write_manifest_workspace(
            &workspace_path,
            None,
            Some("remote-on-demand"),
            &[
                ("ZCL_HELPER", "global-class", "main", "src/ZCL_HELPER.abap"),
                ("ZREPORT_MAIN", "report", "root", "src/ZREPORT_MAIN.abap"),
            ],
            5_000,
        );

        let workspace_uri = file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/src/ZREPORT_MAIN.abap");
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let generations = Arc::new(Mutex::new(HashMap::new()));
        let pending_tasks = Arc::new(Mutex::new(HashMap::new()));
        let mut debounced_tasks = HashMap::new();
        let (task_tx, task_rx) = mpsc::sync_channel(1);
        let message = json!({
            "jsonrpc": "2.0",
            "method": "textDocument/didChange",
            "params": {
                "textDocument": {
                    "uri": source_uri,
                    "version": 2
                },
                "contentChanges": [{
                    "text": "\
REPORT zreport_main.
DATA lo_helper TYPE REF TO zcl_helper.
lo_helper->r"
                }]
            }
        });

        let scheduled = try_schedule_background_analysis(
            &mut state,
            &message,
            &task_tx,
            &pending_tasks,
            &generations,
            &mut debounced_tasks,
        )
        .expect("schedule")
        .expect("background job");
        assert!(scheduled.started_statuses.is_empty());
        assert!(task_rx.try_recv().is_err());

        let completion = handle_message(
            &mut state,
            &ServerConfig::default(),
            json!({
                "jsonrpc": "2.0",
                "id": 7,
                "method": "textDocument/completion",
                "params": {
                    "textDocument": { "uri": source_uri },
                    "position": { "line": 2, "character": 11 }
                }
            }),
        )
        .expect("completion");
        let result = completion
            .response
            .expect("completion response")
            .result
            .expect("completion result");
        let labels: Vec<_> = result
            .as_array()
            .expect("completion array")
            .iter()
            .filter_map(|item| item.get("label").and_then(Value::as_str))
            .collect();
        assert!(
            labels.iter().any(|label| *label == "run"),
            "expected helper method completion during debounce: {labels:?}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn editor_first_preview_updates_changed_file_diagnostics_before_dependents() {
        let workspace_path = temp_workspace_path("editor_first_preview_diagnostics");
        let source_dir = workspace_path.join("src");
        fs::create_dir_all(&source_dir).expect("source dir");
        fs::write(
            source_dir.join("ZCL_PROVIDER.abap"),
            "\
CLASS zcl_provider DEFINITION.
  PUBLIC SECTION.
    METHODS value RETURNING VALUE(rv_value) TYPE i.
ENDCLASS.
CLASS zcl_provider IMPLEMENTATION.
  METHOD value.
    rv_value = 1.
  ENDMETHOD.
ENDCLASS.",
        )
        .expect("provider");
        fs::write(
            source_dir.join("ZREPORT_MAIN.abap"),
            "\
REPORT zreport_main.
DATA lo_provider TYPE REF TO zcl_provider.
lo_provider->value( ).",
        )
        .expect("consumer");
        write_manifest_workspace(
            &workspace_path,
            Some("editor-first"),
            None,
            &[
                (
                    "ZCL_PROVIDER",
                    "global-class",
                    "main",
                    "src/ZCL_PROVIDER.abap",
                ),
                ("ZREPORT_MAIN", "report", "root", "src/ZREPORT_MAIN.abap"),
            ],
            0,
        );

        let workspace_uri = file_uri(&workspace_path);
        let provider_uri = format!("{workspace_uri}/src/ZCL_PROVIDER.abap");
        let consumer_uri = format!("{workspace_uri}/src/ZREPORT_MAIN.abap");
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": consumer_uri,
                        "languageId": "abap",
                        "version": 1,
                        "text": "\
REPORT zreport_main.
DATA lo_provider TYPE REF TO zcl_provider.
lo_provider->value( )."
                    }
                }
            }),
        )
        .expect("didOpen");

        let generations = Arc::new(Mutex::new(HashMap::new()));
        let pending_tasks = Arc::new(Mutex::new(HashMap::new()));
        let mut debounced_tasks = HashMap::new();
        let (task_tx, task_rx) = mpsc::sync_channel(1);
        let message = json!({
            "jsonrpc": "2.0",
            "method": "textDocument/didChange",
            "params": {
                "textDocument": {
                    "uri": provider_uri,
                    "version": 2
                },
                "contentChanges": [{
                    "text": "\
CLASS zcl_provider DEFINITION.
  PUBLIC SECTION.
ENDCLASS.
CLASS zcl_provider IMPLEMENTATION.
ENDCLASS."
                }]
            }
        });

        let scheduled = try_schedule_background_analysis(
            &mut state,
            &message,
            &task_tx,
            &pending_tasks,
            &generations,
            &mut debounced_tasks,
        )
        .expect("schedule")
        .expect("background job");
        let immediate_diagnostic_uris: Vec<_> = scheduled
            .notifications
            .iter()
            .filter(|(method, _)| method == "textDocument/publishDiagnostics")
            .filter_map(|(_, payload)| payload.get("uri").and_then(Value::as_str))
            .collect();
        assert!(
            immediate_diagnostic_uris
                .iter()
                .any(|uri| uri.ends_with("/src/ZCL_PROVIDER.abap"))
        );
        assert!(
            immediate_diagnostic_uris
                .iter()
                .all(|uri| !uri.ends_with("/src/ZREPORT_MAIN.abap"))
        );

        let _started = flush_due_debounced_tasks(
            Instant::now() + CHANGE_ANALYSIS_DEBOUNCE,
            &mut debounced_tasks,
            &task_tx,
            &pending_tasks,
        )
        .expect("flush");
        let queued_workspace = task_rx.recv().expect("queued workspace");
        let task = pending_tasks
            .lock()
            .expect("pending tasks")
            .remove(&queued_workspace)
            .expect("pending task");
        let completion = run_analysis_task(task).expect("analysis completion");
        let background_diagnostic_uris: Vec<_> = completion
            .notifications
            .iter()
            .filter(|(method, _)| method == "textDocument/publishDiagnostics")
            .filter_map(|(_, payload)| payload.get("uri").and_then(Value::as_str))
            .collect();
        assert!(
            background_diagnostic_uris
                .iter()
                .any(|uri| uri.ends_with("/src/ZREPORT_MAIN.abap"))
        );

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn editor_first_background_change_caps_diagnostics_to_open_local_dirty_uris() {
        let workspace_path = temp_workspace_path("editor_first_diagnostic_cap");
        let source_dir = workspace_path.join("src");
        fs::create_dir_all(&source_dir).expect("source dir");
        fs::write(
            source_dir.join("ZCL_PROVIDER.abap"),
            "\
CLASS zcl_provider DEFINITION.
  PUBLIC SECTION.
    METHODS value RETURNING VALUE(rv_value) TYPE i.
ENDCLASS.
CLASS zcl_provider IMPLEMENTATION.
  METHOD value.
    rv_value = 1.
  ENDMETHOD.
ENDCLASS.",
        )
        .expect("provider");
        let mut manifest_units = vec![(
            "ZCL_PROVIDER",
            "global-class",
            "main",
            "src/ZCL_PROVIDER.abap",
        )];
        for idx in 0..20 {
            let relative = format!("src/ZREPORT_{idx:02}.abap");
            fs::write(
                source_dir.join(format!("ZREPORT_{idx:02}.abap")),
                "\
REPORT zreport.
DATA lo_provider TYPE REF TO zcl_provider.
DATA lo_remote TYPE REF TO zcl_remote_demo.
lo_provider->value( ).
lo_remote = zcl_remote_demo=>create( ).",
            )
            .expect("consumer");
            manifest_units.push((
                Box::leak(format!("ZREPORT_{idx:02}").into_boxed_str()),
                "report",
                "root",
                Box::leak(relative.into_boxed_str()),
            ));
        }
        write_manifest_workspace(
            &workspace_path,
            Some("editor-first"),
            Some("remote-on-demand"),
            &manifest_units,
            0,
        );

        let workspace_uri = file_uri(&workspace_path);
        let provider_uri = format!("{workspace_uri}/src/ZCL_PROVIDER.abap");
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        for idx in 0..20 {
            handle_message(
                &mut state,
                &config,
                json!({
                    "jsonrpc": "2.0",
                    "method": "textDocument/didOpen",
                    "params": {
                        "textDocument": {
                            "uri": format!("{workspace_uri}/src/ZREPORT_{idx:02}.abap"),
                            "languageId": "abap",
                            "version": 1,
                            "text": "\
REPORT zreport.
DATA lo_provider TYPE REF TO zcl_provider.
DATA lo_remote TYPE REF TO zcl_remote_demo.
lo_provider->value( ).
lo_remote = zcl_remote_demo=>create( )."
                        }
                    }
                }),
            )
            .expect("didOpen");
        }

        let notifications = handle_did_change_notifications(
            &mut state,
            &did_change_params(
                &provider_uri,
                2,
                "\
CLASS zcl_provider DEFINITION.
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
ENDCLASS.",
            ),
        )
        .expect("didChange");

        let diagnostic_uris: Vec<_> = notifications
            .iter()
            .filter(|(method, _)| method == "textDocument/publishDiagnostics")
            .filter_map(|(_, payload)| payload.get("uri").and_then(Value::as_str))
            .collect();
        let local_diagnostics: Vec<_> = diagnostic_uris
            .iter()
            .copied()
            .filter(|uri| !uri.ends_with("/abapls.toml"))
            .collect();
        assert_eq!(local_diagnostics.len(), EDITOR_FIRST_DIAGNOSTIC_LIMIT);
        assert!(
            local_diagnostics
                .iter()
                .any(|uri| uri.ends_with("/src/ZCL_PROVIDER.abap"))
        );
        assert!(
            diagnostic_uris
                .iter()
                .any(|uri| uri.ends_with("/abapls.toml"))
        );
        assert!(
            diagnostic_uris
                .iter()
                .all(|uri| !uri.contains(".abapls/cache/dependencies"))
        );
    }

    #[test]
    fn editor_first_background_remote_batch_ignores_closed_dirty_locals() {
        let workspace_path = temp_workspace_path("editor_first_remote_filter");
        let source_dir = workspace_path.join("src");
        fs::create_dir_all(&source_dir).expect("source dir");
        fs::write(
            source_dir.join("ZCL_PROVIDER.abap"),
            "\
CLASS zcl_provider DEFINITION.
  PUBLIC SECTION.
    METHODS value RETURNING VALUE(rv_value) TYPE i.
ENDCLASS.
CLASS zcl_provider IMPLEMENTATION.
  METHOD value.
    rv_value = 1.
  ENDMETHOD.
ENDCLASS.",
        )
        .expect("provider");
        for name in ["ZREPORT_OPEN", "ZREPORT_CLOSED"] {
            fs::write(
                source_dir.join(format!("{name}.abap")),
                "\
REPORT zreport.
DATA lo_provider TYPE REF TO zcl_provider.
DATA lo_remote TYPE REF TO zcl_remote_demo.
lo_provider->value( ).
lo_remote = zcl_remote_demo=>create( ).",
            )
            .expect("consumer");
        }
        write_manifest_workspace(
            &workspace_path,
            Some("editor-first"),
            Some("remote-on-demand"),
            &[
                (
                    "ZCL_PROVIDER",
                    "global-class",
                    "main",
                    "src/ZCL_PROVIDER.abap",
                ),
                ("ZREPORT_OPEN", "report", "root", "src/ZREPORT_OPEN.abap"),
                (
                    "ZREPORT_CLOSED",
                    "report",
                    "root",
                    "src/ZREPORT_CLOSED.abap",
                ),
            ],
            0,
        );

        let workspace_uri = file_uri(&workspace_path);
        let provider_uri = format!("{workspace_uri}/src/ZCL_PROVIDER.abap");
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": format!("{workspace_uri}/src/ZREPORT_OPEN.abap"),
                        "languageId": "abap",
                        "version": 1,
                        "text": "\
REPORT zreport.
DATA lo_provider TYPE REF TO zcl_provider.
DATA lo_remote TYPE REF TO zcl_remote_demo.
lo_provider->value( ).
lo_remote = zcl_remote_demo=>create( )."
                    }
                }
            }),
        )
        .expect("didOpen");

        let notifications = handle_did_change_notifications(
            &mut state,
            &did_change_params(
                &provider_uri,
                2,
                "\
CLASS zcl_provider DEFINITION.
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
ENDCLASS.",
            ),
        )
        .expect("didChange");

        let remote_request = notifications
            .iter()
            .find(|(method, _)| method == RESOLVE_REMOTE_DEPENDENCIES)
            .and_then(|(_, payload)| payload.get("sourceUris"))
            .and_then(Value::as_array)
            .expect("remote request source uris");
        let source_uris: Vec<_> = remote_request.iter().filter_map(Value::as_str).collect();
        assert!(
            source_uris
                .iter()
                .any(|uri| uri.ends_with("/src/ZREPORT_OPEN.abap"))
        );
        assert!(
            source_uris
                .iter()
                .all(|uri| !uri.ends_with("/src/ZREPORT_CLOSED.abap"))
        );
    }

    #[test]
    fn editor_first_open_dependency_file_can_request_remote_function_modules() {
        let workspace_path = temp_workspace_path("editor_first_open_dependency_remote");
        let dependency_dir = workspace_path
            .join(".abapls")
            .join("cache")
            .join("dependencies")
            .join("function-group");
        fs::create_dir_all(&dependency_dir).expect("dependency dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[performance]
mode = "editor-first"

[resolution]
dependency_mode = "remote-on-demand"
unknown_symbol_mode = "remote"

[[unit]]
name = "/AIF/FILE_PROCESS_DATA"
kind = "function-group"
root_file = ".abapls/cache/dependencies/function-group/%2FAIF%2FFILE_PROCESS_DATA.abap"

[[unit.member]]
role = "dependency"
file = ".abapls/cache/dependencies/function-group/%2FAIF%2FFILE_PROCESS_DATA.abap"
object_name = "/AIF/FILE_PROCESS_DATA"
"#,
        )
        .expect("manifest");
        let dependency_text = "\
FUNCTION /AIF/FILE_PROCESS_DATA.
  DATA lv_msg_guid TYPE string.
  DATA ls_ifkeys TYPE string.
  DATA gv_trace_level TYPE string.
  CALL FUNCTION '/AIF/DETERMINE_TRACE_LEVEL'
    EXPORTING
      iv_msgguid     = lv_msg_guid
      is_ifkeys      = ls_ifkeys
    IMPORTING
      ev_trace_level = gv_trace_level.
ENDFUNCTION.";
        let dependency_path = dependency_dir.join("%2FAIF%2FFILE_PROCESS_DATA.abap");
        fs::write(&dependency_path, dependency_text).expect("dependency file");

        let workspace_uri = file_uri(&workspace_path);
        let dependency_uri = format!(
            "{workspace_uri}/.abapls/cache/dependencies/function-group/%2FAIF%2FFILE_PROCESS_DATA.abap"
        );
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let handled = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": dependency_uri,
                        "languageId": "abap",
                        "version": 1,
                        "text": dependency_text
                    }
                }
            }),
        )
        .expect("didOpen");

        let request = handled
            .notifications
            .iter()
            .find(|(method, _)| method == RESOLVE_REMOTE_DEPENDENCIES)
            .map(|(_, payload)| payload)
            .expect("remote dependency request");
        let source_uris = request
            .get("sourceUris")
            .and_then(Value::as_array)
            .expect("source uris");
        assert!(source_uris.iter().filter_map(Value::as_str).any(|uri| {
            uri.ends_with(
                "/.abapls/cache/dependencies/function-group/%2FAIF%2FFILE_PROCESS_DATA.abap",
            )
        }));
        let candidates = request
            .get("candidates")
            .and_then(Value::as_array)
            .expect("candidates");
        assert!(candidates.iter().any(|candidate| {
            candidate.get("kind").and_then(Value::as_str) == Some("function")
                && candidate.get("name").and_then(Value::as_str)
                    == Some("/aif/determine_trace_level")
        }));
    }

    #[test]
    fn explicit_full_workspace_mode_keeps_broad_dirty_diagnostics() {
        let workspace_path = temp_workspace_path("full_workspace_diagnostics");
        let source_dir = workspace_path.join("src");
        fs::create_dir_all(&source_dir).expect("source dir");
        fs::write(
            source_dir.join("ZCL_PROVIDER.abap"),
            "\
CLASS zcl_provider DEFINITION.
  PUBLIC SECTION.
    METHODS value RETURNING VALUE(rv_value) TYPE i.
ENDCLASS.
CLASS zcl_provider IMPLEMENTATION.
  METHOD value.
    rv_value = 1.
  ENDMETHOD.
ENDCLASS.",
        )
        .expect("provider");
        fs::write(
            source_dir.join("ZREPORT_DEPENDENT.abap"),
            "\
REPORT zreport_dependent.
DATA lo_provider TYPE REF TO zcl_provider.
lo_provider->value( ).",
        )
        .expect("dependent");
        write_manifest_workspace(
            &workspace_path,
            Some("full-workspace"),
            None,
            &[
                (
                    "ZCL_PROVIDER",
                    "global-class",
                    "main",
                    "src/ZCL_PROVIDER.abap",
                ),
                (
                    "ZREPORT_DEPENDENT",
                    "report",
                    "root",
                    "src/ZREPORT_DEPENDENT.abap",
                ),
            ],
            0,
        );

        let workspace_uri = file_uri(&workspace_path);
        let provider_uri = format!("{workspace_uri}/src/ZCL_PROVIDER.abap");
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        assert_eq!(
            state
                .workspaces
                .get(&abap_lsp::normalize_lsp_uri(&workspace_uri))
                .expect("workspace")
                .performance_mode,
            WorkspacePerformanceMode::FullWorkspace
        );

        let notifications = handle_did_change_notifications(
            &mut state,
            &did_change_params(
                &provider_uri,
                2,
                "\
CLASS zcl_provider DEFINITION.
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
ENDCLASS.",
            ),
        )
        .expect("didChange");

        let diagnostic_uris: Vec<_> = notifications
            .iter()
            .filter(|(method, _)| method == "textDocument/publishDiagnostics")
            .filter_map(|(_, payload)| payload.get("uri").and_then(Value::as_str))
            .collect();
        assert!(
            diagnostic_uris
                .iter()
                .any(|uri| uri.ends_with("/src/ZCL_PROVIDER.abap"))
        );
        assert!(
            diagnostic_uris
                .iter()
                .any(|uri| uri.ends_with("/src/ZREPORT_DEPENDENT.abap"))
        );
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
