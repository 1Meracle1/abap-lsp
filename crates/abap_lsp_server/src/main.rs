use std::collections::{HashMap, HashSet};
use std::io::{self, BufRead, BufReader, BufWriter, Write};
use std::net::{SocketAddr, TcpListener};
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::sync::mpsc::{self, Receiver, RecvTimeoutError, Sender, SyncSender};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

use abap_jsonrpc::{JSON_RPC_VERSION, Response, read_frame, write_frame};
use abap_lsp::{
    CodeActionParams, CompletionParams, DEPENDENCY_CACHE_REFRESH_REQUESTED,
    DependencyCacheInitializationOptions, DidChangeTextDocumentParams, DidOpenTextDocumentParams,
    FoldingRangeParams, GotoDefinitionParams, GotoDefinitionResponse, HoverParams, InlayHintParams,
    READ_DEPENDENCY_DOCUMENT, REMOTE_DEPENDENCIES_UPDATED, RESOLVE_REMOTE_DEPENDENCIES,
    ReferenceParams, RenameParams, SAP_ATC_RESULTS_UPDATED, STORE_REMOTE_DEPENDENCY_ARTIFACTS,
    SapAtcResultsUpdatedParams, SemanticTokensParams, ServerConfig, ServerState,
    StoreRemoteDependencyArtifactsParams, TextDocumentPositionParams, WORKSPACE_ANALYSIS_STATUS,
    WORKSPACE_MANIFEST_UPDATED, WorkspaceAnalysisPhase, WorkspaceAnalysisStatusParams,
    WorkspaceManifestUpdatedParams, WorkspacePerformanceMode, WorkspaceState,
    build_remote_dependency_batch_for_workspace,
    build_remote_dependency_batch_for_workspace_filtered,
    build_remote_dependency_refresh_for_workspace, build_remote_dependency_request,
    build_remote_dependency_request_retrying_negatives, code_actions, completion, definition,
    folding_ranges, handle_dependency_cache_refresh_requested_with_progress,
    handle_remote_dependencies_updated_with_progress, handle_sap_atc_results_updated,
    handle_workspace_manifest_updated_with_progress, hover, initialize_result, inlay_hints,
    prepare_rename, prune_workspace_preview_snapshots, publish_changed_document_mut_with_progress,
    publish_diagnostics_params, publish_open_document_mut_with_progress, read_dependency_document,
    references, refresh_workspace_with_progress, rename, semantic_tokens,
    stage_workspace_preview_snapshot, store_remote_dependency_artifacts,
    workspace_manifest_diagnostics_params, workspace_uri_is_dependency_source,
};
use serde_json::{Value, json};
use tracing::{debug, info, warn};

const METHOD_NOT_FOUND: i64 = -32601;
const INVALID_REQUEST: i64 = -32600;
const INVALID_PARAMS: i64 = -32602;
const CHANGE_ANALYSIS_DEBOUNCE: Duration = Duration::from_millis(250);
const EDITOR_FIRST_DIAGNOSTIC_LIMIT: usize = 16;
const MAX_BACKGROUND_ANALYSIS_WORKERS: usize = 4;

#[derive(Debug, Clone, Default, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct InitializeParamsLite {
    #[serde(default)]
    workspace_folders: Vec<WorkspaceFolderLite>,
    #[serde(default)]
    root_uri: Option<String>,
    #[serde(default, rename = "initializationOptions")]
    initialization_options: DependencyCacheInitializationOptions,
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

    info!(
        exe = %std::env::current_exe()
            .map(|path| path.display().to_string())
            .unwrap_or_else(|error| format!("<unavailable: {error}>")),
        cwd = %std::env::current_dir()
            .map(|path| path.display().to_string())
            .unwrap_or_else(|error| format!("<unavailable: {error}>")),
        build_profile = if cfg!(debug_assertions) { "debug" } else { "release" },
        "starting abap_lsp_server"
    );

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
    DependencyCacheRefreshRequested(WorkspaceManifestUpdatedParams),
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

struct AnalysisProgress {
    workspace_uri: String,
    generation: u64,
    params: WorkspaceAnalysisStatusParams,
}

struct ScheduledBackgroundWork {
    started_statuses: Vec<WorkspaceAnalysisStatusParams>,
    notifications: Vec<(String, Value)>,
}

struct DebouncedAnalysisTask {
    task: AnalysisTask,
    due_at: Instant,
}

#[derive(Default)]
struct PendingAnalysisQueue {
    pending_tasks: HashMap<String, AnalysisTask>,
    scheduled_workspaces: HashSet<String>,
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
    let workspace_uri = abap_lsp::normalize_lsp_uri(workspace_uri);
    state
        .workspaces
        .get(&workspace_uri)
        .is_some_and(|workspace| {
            workspace.performance_mode == WorkspacePerformanceMode::EditorFirst
        })
}

fn enqueue_background_task(
    task: AnalysisTask,
    task_tx: &SyncSender<String>,
    queue_state: &Arc<Mutex<PendingAnalysisQueue>>,
) -> Result<(), Box<dyn std::error::Error>> {
    let workspace_uri = task.workspace_uri.clone();
    let should_send = {
        let mut queue = queue_state
            .lock()
            .expect("pending analysis queue should not be poisoned");
        queue.pending_tasks.insert(workspace_uri.clone(), task);
        queue.scheduled_workspaces.insert(workspace_uri.clone())
    };
    if should_send {
        task_tx
            .send(workspace_uri)
            .map_err(|error| format!("failed to enqueue analysis task: {error}"))?;
    }
    Ok(())
}

fn take_pending_background_task(
    workspace_uri: &str,
    queue_state: &Arc<Mutex<PendingAnalysisQueue>>,
) -> Option<AnalysisTask> {
    queue_state
        .lock()
        .expect("pending analysis queue should not be poisoned")
        .pending_tasks
        .remove(workspace_uri)
}

fn finish_background_task(
    workspace_uri: &str,
    task_tx: &SyncSender<String>,
    queue_state: &Arc<Mutex<PendingAnalysisQueue>>,
) -> Result<(), Box<dyn std::error::Error>> {
    let should_reschedule = {
        let mut queue = queue_state
            .lock()
            .expect("pending analysis queue should not be poisoned");
        if queue.pending_tasks.contains_key(workspace_uri) {
            true
        } else {
            queue.scheduled_workspaces.remove(workspace_uri);
            false
        }
    };
    if should_reschedule {
        task_tx
            .send(workspace_uri.to_owned())
            .map_err(|error| format!("failed to requeue analysis task: {error}"))?;
    }
    Ok(())
}

fn background_analysis_worker_count() -> usize {
    thread::available_parallelism()
        .map(|parallelism| parallelism.get().min(MAX_BACKGROUND_ANALYSIS_WORKERS))
        .unwrap_or(1)
}

fn flush_due_debounced_tasks(
    now: Instant,
    debounced_tasks: &mut HashMap<String, DebouncedAnalysisTask>,
    task_tx: &SyncSender<String>,
    queue_state: &Arc<Mutex<PendingAnalysisQueue>>,
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
        enqueue_background_task(entry.task, task_tx, queue_state)?;
    }
    Ok(ready)
}

fn try_schedule_background_analysis(
    state: &mut ServerState,
    message: &Value,
    task_tx: &SyncSender<String>,
    queue_state: &Arc<Mutex<PendingAnalysisQueue>>,
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
            enqueue_background_task(task, task_tx, queue_state)?;
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
                enqueue_background_task(task, task_tx, queue_state)?;
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
                queue_state,
            )?;
            Ok(Some(ScheduledBackgroundWork {
                started_statuses,
                notifications,
            }))
        }
        DEPENDENCY_CACHE_REFRESH_REQUESTED => {
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
                    kind: AnalysisTaskKind::DependencyCacheRefreshRequested(
                        WorkspaceManifestUpdatedParams {
                            workspace_uri: workspace_uri.clone(),
                        },
                    ),
                },
                task_tx,
                queue_state,
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
                queue_state,
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
                if workspace.performance_mode == WorkspacePerformanceMode::EditorFirst {
                    debug!(
                        workspace_uri = %workspace_uri,
                        "skipping eager initialized analysis for editor-first workspace"
                    );
                    continue;
                }
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
                    queue_state,
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

fn run_analysis_task(
    task: AnalysisTask,
    progress_sink: Option<&(dyn Fn(WorkspaceAnalysisStatusParams) + Sync)>,
) -> Result<AnalysisCompletion, Box<dyn std::error::Error>> {
    let mut state = ServerState::default();
    state
        .workspaces
        .insert(task.workspace_uri.clone(), task.workspace);
    state.refresh_workspace_routing();
    state.index_workspace_members(&task.workspace_uri);

    let notifications = match &task.kind {
        AnalysisTaskKind::DidOpen(params) => {
            handle_did_open_notifications(&mut state, params, progress_sink)?
        }
        AnalysisTaskKind::DidChange(params) => {
            handle_did_change_notifications(&mut state, params, progress_sink)?
        }
        AnalysisTaskKind::ManifestUpdated(params) => {
            handle_workspace_manifest_updated_notifications(&mut state, params, progress_sink)?
        }
        AnalysisTaskKind::DependencyCacheRefreshRequested(params) => {
            handle_dependency_cache_refresh_requested_notifications(
                &mut state,
                params,
                progress_sink,
            )?
        }
        AnalysisTaskKind::RemoteDependenciesUpdated(params) => {
            handle_remote_dependencies_updated_notifications(&mut state, params, progress_sink)?
        }
        AnalysisTaskKind::Initialized => handle_initialized_workspace_notifications(
            &mut state,
            &task.workspace_uri,
            progress_sink,
        )?,
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
            // A later open/change may supersede workspace state while this completion still
            // carries the diagnostic refresh for unchanged documents.
            forward_current_stale_diagnostics(state, writer, &completion)?;
            continue;
        }

        state
            .workspaces
            .insert(completion.workspace_uri.clone(), completion.workspace);
        if let Some(workspace) = state.workspaces.get_mut(&completion.workspace_uri) {
            prune_workspace_preview_snapshots(workspace);
        }
        state.refresh_workspace_routing();
        state.index_workspace_members(&completion.workspace_uri);

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

fn send_analysis_completion(
    completion_tx: &Sender<AnalysisCompletion>,
    completion: AnalysisCompletion,
) -> Result<(), mpsc::SendError<AnalysisCompletion>> {
    // Stale completions can still carry diagnostics for unchanged open documents.
    completion_tx.send(completion)
}

fn forward_current_stale_diagnostics(
    state: &ServerState,
    writer: &mut impl Write,
    completion: &AnalysisCompletion,
) -> Result<(), Box<dyn std::error::Error>> {
    for (method, params) in &completion.notifications {
        if method != "textDocument/publishDiagnostics" {
            continue;
        }
        if stale_diagnostic_payload_matches_current_document(
            state,
            &completion.workspace_uri,
            params,
        ) {
            send_notification(writer, method, params.clone())?;
        }
    }
    Ok(())
}

fn stale_diagnostic_payload_matches_current_document(
    state: &ServerState,
    workspace_uri: &str,
    params: &Value,
) -> bool {
    let Some(uri) = params.get("uri").and_then(Value::as_str) else {
        return false;
    };
    let Some(version) = params.get("version").and_then(Value::as_i64) else {
        return false;
    };
    let uri = abap_lsp::normalize_lsp_uri(uri);
    let workspace_uri = abap_lsp::normalize_lsp_uri(workspace_uri);
    let Some(workspace) = state
        .workspaces
        .get(&workspace_uri)
        .or_else(|| state.workspace_for_uri(&uri))
    else {
        return false;
    };

    if let Some(overlay) = workspace.open_documents.get(&uri) {
        return i64::from(overlay.version) == version;
    }
    if let Some(snapshot) = workspace.preview_snapshots.get(&uri) {
        return i64::from(snapshot.version) == version;
    }
    if let Some(snapshot) = workspace.cache.get(&uri) {
        return i64::from(snapshot.version) == version;
    }
    state
        .cache
        .get(&uri)
        .is_some_and(|snapshot| i64::from(snapshot.version) == version)
}

fn flush_analysis_progress(
    writer: &mut impl Write,
    progress_rx: &Receiver<AnalysisProgress>,
    generations: &Arc<Mutex<HashMap<String, u64>>>,
) -> Result<(), Box<dyn std::error::Error>> {
    while let Ok(progress) = progress_rx.try_recv() {
        if progress.generation != current_workspace_generation(generations, &progress.workspace_uri)
        {
            continue;
        }
        send_notification(
            writer,
            WORKSPACE_ANALYSIS_STATUS,
            serde_json::to_value(progress.params)?,
        )?;
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
    let queue_state = Arc::new(Mutex::new(PendingAnalysisQueue::default()));
    let (task_tx, task_rx): (SyncSender<String>, Receiver<String>) = mpsc::sync_channel(8);
    let task_rx = Arc::new(Mutex::new(task_rx));
    let (completion_tx, completion_rx) = mpsc::channel();
    let (progress_tx, progress_rx) = mpsc::channel();
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

        let worker_count = background_analysis_worker_count();
        debug!(worker_count, "starting background analysis workers");
        for _ in 0..worker_count {
            let worker_generations = Arc::clone(&generations);
            let worker_completion_tx = completion_tx.clone();
            let worker_progress_tx = progress_tx.clone();
            let worker_queue_state = Arc::clone(&queue_state);
            let worker_task_rx = Arc::clone(&task_rx);
            let worker_task_tx = task_tx.clone();
            scope.spawn(move || {
                loop {
                    let workspace_uri = {
                        let receiver = worker_task_rx
                            .lock()
                            .expect("analysis task receiver should not be poisoned");
                        match receiver.recv() {
                            Ok(workspace_uri) => workspace_uri,
                            Err(_) => break,
                        }
                    };
                    let Some(task) =
                        take_pending_background_task(&workspace_uri, &worker_queue_state)
                    else {
                        if let Err(error) = finish_background_task(
                            &workspace_uri,
                            &worker_task_tx,
                            &worker_queue_state,
                        ) {
                            warn!(error = %error, "background analysis queue cleanup failed");
                            break;
                        }
                        continue;
                    };
                    if task.generation
                        != current_workspace_generation(&worker_generations, &task.workspace_uri)
                    {
                        if let Err(error) = finish_background_task(
                            &workspace_uri,
                            &worker_task_tx,
                            &worker_queue_state,
                        ) {
                            warn!(error = %error, "background analysis queue cleanup failed");
                            break;
                        }
                        continue;
                    }
                    let progress_workspace_uri = task.workspace_uri.clone();
                    let progress_generation = task.generation;
                    let progress = |params: WorkspaceAnalysisStatusParams| {
                        let _ = worker_progress_tx.send(AnalysisProgress {
                            workspace_uri: progress_workspace_uri.clone(),
                            generation: progress_generation,
                            params,
                        });
                    };
                    let fallback_finished = task.started.as_ref().map(|started| {
                        workspace_analysis_status_finished_for_workspace(started, &task.workspace)
                    });
                    match catch_unwind(AssertUnwindSafe(|| {
                        run_analysis_task(task, Some(&progress))
                    })) {
                        Ok(completion) => match completion {
                            Ok(completion) => {
                                if send_analysis_completion(&worker_completion_tx, completion)
                                    .is_err()
                                {
                                    break;
                                }
                            }
                            Err(error) => {
                                warn!(error = %error, "background analysis task failed");
                                if let Some(params) = fallback_finished.clone() {
                                    let _ = worker_progress_tx.send(AnalysisProgress {
                                        workspace_uri: progress_workspace_uri.clone(),
                                        generation: progress_generation,
                                        params,
                                    });
                                }
                            }
                        },
                        Err(_) => {
                            warn!("background analysis task panicked");
                            if let Some(params) = fallback_finished {
                                let _ = worker_progress_tx.send(AnalysisProgress {
                                    workspace_uri: progress_workspace_uri.clone(),
                                    generation: progress_generation,
                                    params,
                                });
                            }
                        }
                    }
                    if let Err(error) =
                        finish_background_task(&workspace_uri, &worker_task_tx, &worker_queue_state)
                    {
                        warn!(error = %error, "background analysis queue cleanup failed");
                        break;
                    }
                }
            });
        }

        let mut reader_closed = false;
        loop {
            for params in flush_due_debounced_tasks(
                Instant::now(),
                &mut debounced_tasks,
                &task_tx,
                &queue_state,
            )? {
                send_notification(
                    writer,
                    WORKSPACE_ANALYSIS_STATUS,
                    serde_json::to_value(params)?,
                )?;
            }
            flush_analysis_progress(writer, &progress_rx, &generations)?;
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
                        &queue_state,
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
                        &queue_state,
                    )? {
                        send_notification(
                            writer,
                            WORKSPACE_ANALYSIS_STATUS,
                            serde_json::to_value(params)?,
                        )?;
                    }
                    flush_analysis_progress(writer, &progress_rx, &generations)?;
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
            &queue_state,
        )? {
            send_notification(
                writer,
                WORKSPACE_ANALYSIS_STATUS,
                serde_json::to_value(params)?,
            )?;
        }
        drop(task_tx);
        flush_analysis_progress(writer, &progress_rx, &generations)?;
        flush_analysis_completions(&mut state, writer, &completion_rx, &generations)?;
        Ok(())
    })
}

fn definition_response_summary(
    result: &Option<GotoDefinitionResponse>,
) -> (&'static str, Option<String>) {
    match result {
        Some(GotoDefinitionResponse::Scalar(location)) => {
            ("scalar", Some(location.uri.to_string()))
        }
        Some(GotoDefinitionResponse::Array(locations)) => (
            "array",
            locations.first().map(|location| location.uri.to_string()),
        ),
        Some(GotoDefinitionResponse::Link(links)) => (
            "link",
            links
                .first()
                .map(|location| location.target_uri.to_string()),
        ),
        None => ("none", None),
    }
}

fn send_response(
    writer: &mut impl Write,
    response: &Response,
) -> Result<(), Box<dyn std::error::Error>> {
    let serialize_start = Instant::now();
    let payload = serde_json::to_vec(&json!({
        "jsonrpc": JSON_RPC_VERSION,
        "id": response.id,
        "result": response.result,
        "error": response.error,
    }))?;
    let serialize_elapsed = serialize_start.elapsed();
    let write_start = Instant::now();
    write_frame(writer, &payload)?;
    let write_elapsed = write_start.elapsed();
    debug!(
        id = %response.id,
        payload_bytes = payload.len(),
        serialize_elapsed = ?serialize_elapsed,
        write_elapsed = ?write_elapsed,
        "sent LSP response"
    );
    Ok(())
}

fn send_notification(
    writer: &mut impl Write,
    method: &str,
    params: Value,
) -> Result<(), Box<dyn std::error::Error>> {
    let serialize_start = Instant::now();
    let payload = serde_json::to_vec(&json!({
        "jsonrpc": JSON_RPC_VERSION,
        "method": method,
        "params": params,
    }))?;
    let serialize_elapsed = serialize_start.elapsed();
    let write_start = Instant::now();
    write_frame(writer, &payload)?;
    let write_elapsed = write_start.elapsed();
    debug!(
        method,
        payload_bytes = payload.len(),
        serialize_elapsed = ?serialize_elapsed,
        write_elapsed = ?write_elapsed,
        "sent LSP notification"
    );
    Ok(())
}

fn push_publish_diagnostics_notification(
    state: &ServerState,
    snapshot: &abap_lsp::AnalysisSnapshot,
    notifications: &mut Vec<(String, Value)>,
) -> Result<(), Box<dyn std::error::Error>> {
    let params_start = Instant::now();
    let params = publish_diagnostics_params(state, snapshot);
    let params_elapsed = params_start.elapsed();
    let diagnostic_count = params.diagnostics.len();
    let value_start = Instant::now();
    let params_value = serde_json::to_value(params)?;
    let value_elapsed = value_start.elapsed();
    debug!(
        uri = %snapshot.uri,
        diagnostic_count,
        text_bytes = snapshot.text.len(),
        parse_diagnostic_count = snapshot.parse.errors.len(),
        semantic_diagnostic_count = snapshot.symbols.diagnostics.len(),
        build_params_elapsed = ?params_elapsed,
        to_value_elapsed = ?value_elapsed,
        "built publishDiagnostics notification"
    );
    notifications.push(("textDocument/publishDiagnostics".to_owned(), params_value));
    Ok(())
}

fn push_publish_diagnostics_notification_once(
    state: &ServerState,
    snapshot: &abap_lsp::AnalysisSnapshot,
    published_uris: &mut HashSet<String>,
    notifications: &mut Vec<(String, Value)>,
) -> Result<(), Box<dyn std::error::Error>> {
    if published_uris.insert(snapshot.uri.to_string()) {
        push_publish_diagnostics_notification(state, snapshot, notifications)?;
    }
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
    let workspace_uri = abap_lsp::normalize_lsp_uri(workspace_uri);
    let Some(workspace) = state.workspaces.get(&workspace_uri) else {
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
    let workspace_uri = abap_lsp::normalize_lsp_uri(workspace_uri);
    let Some(workspace) = state.workspaces.get(&workspace_uri) else {
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

fn workspace_uri_for_cached_snapshot(state: &ServerState, uri: &str) -> Option<String> {
    state
        .workspace_for_uri(uri)
        .filter(|workspace| workspace.cache.get(uri).is_some())
        .map(|workspace| workspace.root_uri.clone())
}

fn workspace_dirty_uris(state: &ServerState, workspace_uri: &str) -> HashSet<Arc<str>> {
    state
        .workspaces
        .get(workspace_uri)
        .map(|workspace| workspace.cache.last_dirty_uris())
        .unwrap_or_default()
}

fn workspace_local_source_uris(state: &ServerState, workspace_uri: &str) -> HashSet<Arc<str>> {
    let workspace_uri = abap_lsp::normalize_lsp_uri(workspace_uri);
    let Some(workspace) = state.workspaces.get(&workspace_uri) else {
        return HashSet::new();
    };
    workspace
        .cache
        .uris()
        .into_iter()
        .filter(|uri| {
            workspace
                .cache
                .get(uri.as_ref())
                .is_some_and(|snapshot| !snapshot.is_dependency)
        })
        .collect()
}

fn workspace_remote_dependency_follow_up_source_uris(
    state: &ServerState,
    workspace_uri: &str,
    fetched: &[String],
) -> HashSet<Arc<str>> {
    let workspace_uri = abap_lsp::normalize_lsp_uri(workspace_uri);
    let Some(workspace) = state.workspaces.get(&workspace_uri) else {
        return HashSet::new();
    };
    let fetched_names: HashSet<_> = fetched
        .iter()
        .map(|name| name.trim().to_ascii_lowercase())
        .filter(|name| !name.is_empty())
        .collect();
    if fetched_names.is_empty() {
        return HashSet::new();
    }
    let mut selected = HashSet::new();
    for uri in workspace.cache.uris() {
        let Some(snapshot) = workspace.cache.get(uri.as_ref()) else {
            continue;
        };
        if !workspace_uri_is_dependency_source(workspace, uri.as_ref()) {
            continue;
        }
        if snapshot.object_name.as_ref().is_some_and(|object_name| {
            fetched_names.contains(&object_name.trim().to_ascii_lowercase())
        }) {
            selected.insert(uri);
        }
    }
    selected
}

fn workspace_remote_dependency_follow_up_filter(
    state: &ServerState,
    params: &abap_lsp::RemoteDependenciesUpdatedParams,
) -> Option<HashSet<Arc<str>>> {
    let mut selected = workspace_remote_dependency_follow_up_source_uris(
        state,
        &params.workspace_uri,
        &params.fetched,
    );
    for uri in params
        .source_uris
        .iter()
        .map(|uri| abap_lsp::normalize_lsp_uri(uri))
        .chain(
            (!params.source_uri.is_empty())
                .then(|| abap_lsp::normalize_lsp_uri(&params.source_uri)),
        )
    {
        selected.insert(Arc::<str>::from(uri.as_str()));
    }
    (!selected.is_empty()).then_some(selected)
}

fn editor_first_diagnostic_uris(
    state: &ServerState,
    workspace_uri: &str,
    changed_uri: Option<&str>,
    dirty_uris: &HashSet<Arc<str>>,
) -> Vec<Arc<str>> {
    let workspace_uri = abap_lsp::normalize_lsp_uri(workspace_uri);
    let Some(workspace) = state.workspaces.get(&workspace_uri) else {
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

fn push_document_update_diagnostics(
    state: &ServerState,
    workspace_uri: Option<&str>,
    dirty_uris: Option<&HashSet<Arc<str>>>,
    snapshot: &abap_lsp::AnalysisSnapshot,
    unchanged: bool,
    trigger: &str,
    notifications: &mut Vec<(String, Value)>,
) -> Result<(), Box<dyn std::error::Error>> {
    let Some(workspace_uri) = workspace_uri else {
        push_publish_diagnostics_notification(state, snapshot, notifications)?;
        return Ok(());
    };
    let Some(dirty_uris) = dirty_uris else {
        push_publish_diagnostics_notification(state, snapshot, notifications)?;
        return Ok(());
    };

    if unchanged {
        push_publish_diagnostics_notification(state, snapshot, notifications)?;
        return Ok(());
    }

    if workspace_uses_editor_first_mode(state, workspace_uri) {
        let selected_uris = editor_first_diagnostic_uris(
            state,
            workspace_uri,
            Some(snapshot.uri.as_ref()),
            dirty_uris,
        );
        debug!(
            workspace_uri = %workspace_uri,
            trigger = trigger,
            dirty_uri_count = dirty_uris.len(),
            selected_uri_count = selected_uris.len(),
            "publishing editor-first workspace diagnostics"
        );
        for uri in selected_uris {
            if let Some(snapshot) = state
                .workspaces
                .get(workspace_uri)
                .and_then(|workspace| workspace.cache.get(uri.as_ref()))
            {
                push_publish_diagnostics_notification(state, snapshot.as_ref(), notifications)?;
            }
        }
        return Ok(());
    }

    push_workspace_diagnostics_notifications_for_uris(
        state,
        workspace_uri,
        Some(dirty_uris),
        notifications,
    )
}

fn build_dirty_remote_dependency_batch(
    state: &mut ServerState,
    workspace_uri: Option<&str>,
    dirty_uris: Option<&HashSet<Arc<str>>>,
    focused_uri: Option<&str>,
    trigger: &str,
) -> Option<abap_lsp::RemoteDependencyResolveParams> {
    let workspace_uri = workspace_uri?;
    if let Some(focused_uri) = focused_uri
        && !workspace_uses_editor_first_mode(state, workspace_uri)
    {
        let normalized_focused_uri = abap_lsp::normalize_lsp_uri(focused_uri);
        let focused = HashSet::from([Arc::<str>::from(normalized_focused_uri.as_str())]);
        debug!(
            workspace_uri = %workspace_uri,
            trigger = trigger,
            focused_uri = %normalized_focused_uri,
            "building focused full-workspace remote dependency batch"
        );
        return build_remote_dependency_batch_for_workspace_filtered(
            state,
            workspace_uri,
            Some(&focused),
        );
    }
    let dirty_uris = dirty_uris?;
    if workspace_uses_editor_first_mode(state, workspace_uri) {
        let open_dirty = workspace_open_dirty_uris(state, workspace_uri, dirty_uris);
        debug!(
            workspace_uri = %workspace_uri,
            trigger = trigger,
            dirty_uri_count = dirty_uris.len(),
            open_dirty_uri_count = open_dirty.len(),
            "building editor-first workspace remote dependency batch"
        );
        build_remote_dependency_batch_for_workspace_filtered(
            state,
            workspace_uri,
            Some(&open_dirty),
        )
    } else {
        build_remote_dependency_batch_for_workspace_filtered(state, workspace_uri, Some(dirty_uris))
    }
}

fn queue_open_dependency_request(state: &mut ServerState, source_uri: &str) {
    let source_uri = abap_lsp::normalize_lsp_uri(source_uri);
    let Some(workspace) = state.workspace_for_uri_mut(&source_uri) else {
        return;
    };
    workspace
        .pending_open_dependency_requests
        .insert(source_uri);
}

fn take_pending_open_dependency_request(workspace: &mut WorkspaceState) -> Option<String> {
    let mut pending: Vec<_> = workspace
        .pending_open_dependency_requests
        .iter()
        .cloned()
        .collect();
    pending.sort();
    let source_uri = pending.into_iter().next()?;
    workspace
        .pending_open_dependency_requests
        .remove(&source_uri);
    Some(source_uri)
}

fn build_pending_open_dependency_request(
    state: &mut ServerState,
    workspace_uri: Option<&str>,
) -> Option<abap_lsp::RemoteDependencyResolveParams> {
    let workspace_uri = abap_lsp::normalize_lsp_uri(workspace_uri?);
    let source_uri = {
        let workspace = state.workspaces.get_mut(&workspace_uri)?;
        if workspace.remote_resolution_in_flight {
            return None;
        }
        take_pending_open_dependency_request(workspace)?
    };
    let request = build_remote_dependency_request_retrying_negatives(state, &source_uri)?;
    if let Some(workspace) = state.workspaces.get_mut(&workspace_uri) {
        workspace.remote_resolution_in_flight = true;
    }
    Some(request)
}

struct HandledMessage {
    response: Option<Response>,
    notifications: Vec<(String, Value)>,
}

fn handle_did_open_notifications(
    state: &mut ServerState,
    params: &DidOpenTextDocumentParams,
    progress_sink: Option<&(dyn Fn(WorkspaceAnalysisStatusParams) + Sync)>,
) -> Result<Vec<(String, Value)>, Box<dyn std::error::Error>> {
    let total_start = Instant::now();
    let mut notifications = Vec::new();
    let normalized_uri = abap_lsp::normalize_lsp_uri(params.text_document.uri.as_str());
    let line_count = params.text_document.text.lines().count();
    let initial_workspace_uri = state
        .workspace_for_uri(normalized_uri.as_str())
        .map(|workspace| workspace.root_uri.clone());
    let progress_workspace_uri = state
        .workspace_for_uri(normalized_uri.as_str())
        .map(|workspace| workspace.root_uri.clone());
    let unchanged_start = Instant::now();
    let unchanged_workspace_open = state
        .workspace_for_uri(&normalized_uri)
        .and_then(|workspace| workspace.cache.get(&normalized_uri))
        .is_some_and(|snapshot| snapshot.text.as_ref() == params.text_document.text.as_str());
    let unchanged_elapsed = unchanged_start.elapsed();
    let progress_notifications = Mutex::new(Vec::new());
    let progress = |processed: usize, total: usize| {
        if let Some(workspace_uri) = progress_workspace_uri.as_ref() {
            emit_workspace_analysis_progress(
                Some(&progress_notifications),
                progress_sink,
                workspace_uri,
                "open",
                processed,
                total,
            );
        }
    };
    let publish_start = Instant::now();
    let snapshot = publish_open_document_mut_with_progress(state, params, Some(&progress));
    let publish_elapsed = publish_start.elapsed();
    notifications.extend(
        progress_notifications
            .into_inner()
            .expect("progress notification collection should not be poisoned"),
    );
    let workspace_lookup_start = Instant::now();
    let workspace_uri = workspace_uri_for_cached_snapshot(state, snapshot.uri.as_ref());
    let workspace_lookup_elapsed = workspace_lookup_start.elapsed();
    let dirty_start = Instant::now();
    let dirty_uris = workspace_uri
        .as_deref()
        .map(|workspace_uri| workspace_dirty_uris(state, workspace_uri));
    let dirty_elapsed = dirty_start.elapsed();
    let metrics = workspace_uri
        .as_deref()
        .and_then(|workspace_uri| state.workspaces.get(workspace_uri))
        .and_then(|workspace| workspace.cache.last_analysis_metrics_snapshot());
    if let Some(metrics) = metrics.as_ref() {
        debug!(
            uri = %snapshot.uri,
            workspace_uri = workspace_uri.as_deref().unwrap_or("<none>"),
            parse_count = metrics.parse_count,
            local_phase_count = metrics.local_phase_count,
            dirty_uri_count = metrics.dirty_uri_count,
            full_rebuild = metrics.full_rebuild,
            unit_count = metrics.unit_count,
            dirty_unit_count = metrics.dirty_unit_count,
            parse_micros = metrics.parse_micros,
            parse_work_micros = metrics.parse_work_micros,
            local_phase_micros = metrics.local_phase_micros,
            local_phase_work_micros = metrics.local_phase_work_micros,
            project_update_micros = metrics.project_update_micros,
            snapshot_build_micros = metrics.snapshot_build_micros,
            routine_analysis_micros = metrics.routine_analysis_micros,
            routine_analysis_index_micros = metrics.routine_analysis_index_micros,
            routine_analysis_ir_micros = metrics.routine_analysis_ir_micros,
            routine_analysis_cfg_micros = metrics.routine_analysis_cfg_micros,
            routine_analysis_dataflow_micros = metrics.routine_analysis_dataflow_micros,
            routine_analysis_dead_store_micros = metrics.routine_analysis_dead_store_micros,
            routine_analysis_perform_routine_count = metrics.routine_analysis_perform_routine_count,
            routine_analysis_dataflow_pass_count = metrics.routine_analysis_dataflow_pass_count,
            routine_analysis_dataflow_routine_runs = metrics
                .routine_analysis_dataflow_routine_runs,
            resolve_cross_unit_micros = metrics.resolve_cross_unit_micros,
            validate_micros = metrics.validate_micros,
            collect_project_diagnostics_micros = metrics.collect_project_diagnostics_micros,
            "workspace analysis metrics after didOpen publish"
        );
    }
    let diagnostics_start = Instant::now();
    push_document_update_diagnostics(
        state,
        workspace_uri.as_deref(),
        dirty_uris.as_ref(),
        &snapshot,
        unchanged_workspace_open,
        "open",
        &mut notifications,
    )?;
    let diagnostics_elapsed = diagnostics_start.elapsed();
    let manifest_start = Instant::now();
    if let Some(workspace_uri) = workspace_uri.as_deref() {
        push_workspace_manifest_diagnostics_notification(state, workspace_uri, &mut notifications);
    }
    let manifest_elapsed = manifest_start.elapsed();
    let dependency_source_start = Instant::now();
    let opened_dependency_source = workspace_uri.as_deref().is_some_and(|workspace_uri| {
        state
            .workspaces
            .get(&abap_lsp::normalize_lsp_uri(workspace_uri))
            .is_some_and(|workspace| {
                workspace_uri_is_dependency_source(workspace, snapshot.uri.as_ref())
            })
    });
    let dependency_source_elapsed = dependency_source_start.elapsed();
    if opened_dependency_source {
        queue_open_dependency_request(state, snapshot.uri.as_ref());
    }
    let remote_request_start = Instant::now();
    let remote_request = if opened_dependency_source {
        build_pending_open_dependency_request(state, workspace_uri.as_deref())
    } else if unchanged_workspace_open && snapshot.is_dependency {
        build_remote_dependency_request(state, snapshot.uri.as_ref())
    } else {
        build_dirty_remote_dependency_batch(
            state,
            workspace_uri.as_deref(),
            dirty_uris.as_ref(),
            Some(snapshot.uri.as_ref()),
            "open",
        )
    };
    let remote_request_elapsed = remote_request_start.elapsed();
    let remote_request_candidate_count = remote_request
        .as_ref()
        .map(|request| request.candidates.len())
        .unwrap_or(0);
    let remote_request_source_uri_count = remote_request
        .as_ref()
        .map(|request| request.source_uris.len())
        .unwrap_or(0);
    if let Some(request) = remote_request {
        let request_value_start = Instant::now();
        let request_value = serde_json::to_value(request)?;
        let request_value_elapsed = request_value_start.elapsed();
        debug!(
            uri = %snapshot.uri,
            candidate_count = remote_request_candidate_count,
            source_uri_count = remote_request_source_uri_count,
            to_value_elapsed = ?request_value_elapsed,
            "built remote dependency request notification"
        );
        notifications.push((RESOLVE_REMOTE_DEPENDENCIES.to_owned(), request_value));
    }
    debug!(
        uri = %snapshot.uri,
        initial_workspace_uri = initial_workspace_uri.as_deref().unwrap_or("<none>"),
        workspace_uri = workspace_uri.as_deref().unwrap_or("<none>"),
        text_bytes = params.text_document.text.len(),
        line_count,
        snapshot_is_dependency = snapshot.is_dependency,
        unchanged_workspace_open,
        opened_dependency_source,
        dirty_uri_count = dirty_uris.as_ref().map(HashSet::len).unwrap_or(0),
        notification_count = notifications.len(),
        remote_request_candidate_count,
        remote_request_source_uri_count,
        unchanged_elapsed = ?unchanged_elapsed,
        publish_elapsed = ?publish_elapsed,
        workspace_lookup_elapsed = ?workspace_lookup_elapsed,
        dirty_elapsed = ?dirty_elapsed,
        diagnostics_elapsed = ?diagnostics_elapsed,
        manifest_elapsed = ?manifest_elapsed,
        dependency_source_elapsed = ?dependency_source_elapsed,
        remote_request_elapsed = ?remote_request_elapsed,
        total_elapsed = ?total_start.elapsed(),
        "handled textDocument/didOpen publish path"
    );
    Ok(notifications)
}

fn handle_did_change_notifications(
    state: &mut ServerState,
    params: &DidChangeTextDocumentParams,
    progress_sink: Option<&(dyn Fn(WorkspaceAnalysisStatusParams) + Sync)>,
) -> Result<Vec<(String, Value)>, Box<dyn std::error::Error>> {
    let total_start = Instant::now();
    let mut notifications = Vec::new();
    let normalized_uri = abap_lsp::normalize_lsp_uri(params.text_document.uri.as_str());
    let progress_notifications = Mutex::new(Vec::new());
    let progress_workspace_uri = state
        .workspace_for_uri(normalized_uri.as_str())
        .map(|workspace| workspace.root_uri.clone());
    let change = params.content_changes.last();
    let change_text_len = change.map(|change| change.text.len()).unwrap_or(0);
    let line_count = change
        .map(|change| change.text.lines().count())
        .unwrap_or(0);
    let unchanged_start = Instant::now();
    let unchanged_workspace_change = change.and_then(|change| {
        state
            .workspace_for_uri(&normalized_uri)
            .and_then(|workspace| workspace.cache.get(&normalized_uri))
            .map(|snapshot| snapshot.text.as_ref() == change.text.as_str())
    }) == Some(true);
    let unchanged_elapsed = unchanged_start.elapsed();
    let progress = |processed: usize, total: usize| {
        if let Some(workspace_uri) = progress_workspace_uri.as_ref() {
            emit_workspace_analysis_progress(
                Some(&progress_notifications),
                progress_sink,
                workspace_uri,
                "change",
                processed,
                total,
            );
        }
    };
    let publish_start = Instant::now();
    if let Some(snapshot) =
        publish_changed_document_mut_with_progress(state, params, Some(&progress))
    {
        let publish_elapsed = publish_start.elapsed();
        notifications.extend(
            progress_notifications
                .into_inner()
                .expect("progress notification collection should not be poisoned"),
        );
        let workspace_lookup_start = Instant::now();
        let workspace_uri = workspace_uri_for_cached_snapshot(state, snapshot.uri.as_ref());
        let workspace_lookup_elapsed = workspace_lookup_start.elapsed();
        let dirty_start = Instant::now();
        let dirty_uris = workspace_uri
            .as_deref()
            .map(|workspace_uri| workspace_dirty_uris(state, workspace_uri));
        let dirty_elapsed = dirty_start.elapsed();
        let diagnostics_start = Instant::now();
        push_document_update_diagnostics(
            state,
            workspace_uri.as_deref(),
            dirty_uris.as_ref(),
            &snapshot,
            unchanged_workspace_change,
            "change",
            &mut notifications,
        )?;
        let diagnostics_elapsed = diagnostics_start.elapsed();
        let manifest_start = Instant::now();
        if let Some(workspace_uri) = workspace_uri.as_deref() {
            push_workspace_manifest_diagnostics_notification(
                state,
                workspace_uri,
                &mut notifications,
            );
        }
        let manifest_elapsed = manifest_start.elapsed();
        let remote_request_start = Instant::now();
        let mut remote_request_candidate_count = 0usize;
        let mut remote_request_source_uri_count = 0usize;
        if !unchanged_workspace_change
            && let Some(request) = build_dirty_remote_dependency_batch(
                state,
                workspace_uri.as_deref(),
                dirty_uris.as_ref(),
                Some(snapshot.uri.as_ref()),
                "change",
            )
        {
            remote_request_candidate_count = request.candidates.len();
            remote_request_source_uri_count = request.source_uris.len();
            notifications.push((
                RESOLVE_REMOTE_DEPENDENCIES.to_owned(),
                serde_json::to_value(request)?,
            ));
        }
        let remote_request_elapsed = remote_request_start.elapsed();
        debug!(
            uri = %snapshot.uri,
            workspace_uri = workspace_uri.as_deref().unwrap_or("<none>"),
            text_bytes = change_text_len,
            line_count,
            snapshot_is_dependency = snapshot.is_dependency,
            unchanged_workspace_change,
            dirty_uri_count = dirty_uris.as_ref().map(HashSet::len).unwrap_or(0),
            notification_count = notifications.len(),
            remote_request_candidate_count,
            remote_request_source_uri_count,
            unchanged_elapsed = ?unchanged_elapsed,
            publish_elapsed = ?publish_elapsed,
            workspace_lookup_elapsed = ?workspace_lookup_elapsed,
            dirty_elapsed = ?dirty_elapsed,
            diagnostics_elapsed = ?diagnostics_elapsed,
            manifest_elapsed = ?manifest_elapsed,
            remote_request_elapsed = ?remote_request_elapsed,
            total_elapsed = ?total_start.elapsed(),
            "handled textDocument/didChange publish path"
        );
    } else {
        debug!(
            uri = %normalized_uri,
            text_bytes = change_text_len,
            line_count,
            unchanged_workspace_change,
            unchanged_elapsed = ?unchanged_elapsed,
            publish_elapsed = ?publish_start.elapsed(),
            total_elapsed = ?total_start.elapsed(),
            "ignored textDocument/didChange without a full-content change"
        );
    }
    Ok(notifications)
}

fn handle_workspace_manifest_updated_notifications(
    state: &mut ServerState,
    params: &WorkspaceManifestUpdatedParams,
    progress_sink: Option<&(dyn Fn(WorkspaceAnalysisStatusParams) + Sync)>,
) -> Result<Vec<(String, Value)>, Box<dyn std::error::Error>> {
    let progress_notifications = Mutex::new(Vec::new());
    let progress = |processed: usize, total: usize| {
        emit_workspace_analysis_progress(
            Some(&progress_notifications),
            progress_sink,
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

fn handle_dependency_cache_refresh_requested_notifications(
    state: &mut ServerState,
    params: &WorkspaceManifestUpdatedParams,
    progress_sink: Option<&(dyn Fn(WorkspaceAnalysisStatusParams) + Sync)>,
) -> Result<Vec<(String, Value)>, Box<dyn std::error::Error>> {
    let progress_notifications = Mutex::new(Vec::new());
    let progress = |processed: usize, total: usize| {
        emit_workspace_analysis_progress(
            Some(&progress_notifications),
            progress_sink,
            &params.workspace_uri,
            "dependency-cache-refresh",
            processed,
            total,
        );
    };
    let snapshots =
        handle_dependency_cache_refresh_requested_with_progress(state, params, Some(&progress));
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
    if let Some(request) =
        build_remote_dependency_refresh_for_workspace(state, &params.workspace_uri)
    {
        notifications.push((
            RESOLVE_REMOTE_DEPENDENCIES.to_string(),
            serde_json::to_value(request)?,
        ));
    }
    Ok(notifications)
}

fn push_remote_dependency_update_diagnostics(
    state: &ServerState,
    workspace_uri: &str,
    source_uris: &HashSet<String>,
    refreshed_snapshots: &[Arc<abap_lsp::AnalysisSnapshot>],
    notifications: &mut Vec<(String, Value)>,
) -> Result<(), Box<dyn std::error::Error>> {
    let normalized_workspace_uri = abap_lsp::normalize_lsp_uri(workspace_uri);
    let mut published_uris = HashSet::new();
    for snapshot in refreshed_snapshots {
        if source_uris.contains(snapshot.uri.as_ref())
            && (!workspace_uses_editor_first_mode(state, workspace_uri)
                || state
                    .workspaces
                    .get(&normalized_workspace_uri)
                    .is_some_and(|workspace| {
                        workspace.open_documents.contains_key(snapshot.uri.as_ref())
                    }))
        {
            push_publish_diagnostics_notification_once(
                state,
                snapshot,
                &mut published_uris,
                notifications,
            )?;
        }
    }

    let dirty_uris = workspace_dirty_uris(state, &normalized_workspace_uri);
    if dirty_uris.is_empty() {
        return Ok(());
    }

    let diagnostic_uris = if workspace_uses_editor_first_mode(state, workspace_uri) {
        editor_first_diagnostic_uris(state, &normalized_workspace_uri, None, &dirty_uris)
    } else {
        let mut uris: Vec<_> = dirty_uris.into_iter().collect();
        uris.sort();
        uris
    };

    let Some(workspace) = state.workspaces.get(&normalized_workspace_uri) else {
        return Ok(());
    };
    for uri in diagnostic_uris {
        if let Some(snapshot) = workspace.cache.get(uri.as_ref()) {
            push_publish_diagnostics_notification_once(
                state,
                snapshot.as_ref(),
                &mut published_uris,
                notifications,
            )?;
        }
    }
    Ok(())
}

fn handle_remote_dependencies_updated_notifications(
    state: &mut ServerState,
    params: &abap_lsp::RemoteDependenciesUpdatedParams,
    progress_sink: Option<&(dyn Fn(WorkspaceAnalysisStatusParams) + Sync)>,
) -> Result<Vec<(String, Value)>, Box<dyn std::error::Error>> {
    let progress_notifications = Mutex::new(Vec::new());
    let normalized_workspace_uri = abap_lsp::normalize_lsp_uri(&params.workspace_uri);
    let progress = |processed: usize, total: usize| {
        emit_workspace_analysis_progress(
            Some(&progress_notifications),
            progress_sink,
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
    push_remote_dependency_update_diagnostics(
        state,
        &params.workspace_uri,
        &source_uris,
        &snapshots,
        &mut notifications,
    )?;
    let request = if let Some(request) =
        build_pending_open_dependency_request(state, Some(&params.workspace_uri))
    {
        Some(request)
    } else {
        let follow_up_filter = workspace_remote_dependency_follow_up_filter(state, params);
        if let Some(source_filter) = follow_up_filter.as_ref() {
            let dirty_uri_count = state
                .workspaces
                .get(&normalized_workspace_uri)
                .map(|workspace| workspace.cache.last_dirty_uris().len())
                .unwrap_or(0);
            debug!(
                workspace_uri = %params.workspace_uri,
                source_uri_count = source_uris.len(),
                dirty_uri_count,
                filtered_source_uri_count = source_filter.len(),
                "building scoped post-remote dependency batch"
            );
            build_remote_dependency_batch_for_workspace_filtered(
                state,
                &params.workspace_uri,
                Some(source_filter),
            )
        } else {
            build_remote_dependency_batch_for_workspace(state, &params.workspace_uri)
        }
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
    progress_sink: Option<&(dyn Fn(WorkspaceAnalysisStatusParams) + Sync)>,
) -> Result<Vec<(String, Value)>, Box<dyn std::error::Error>> {
    let progress_notifications = Mutex::new(Vec::new());
    let progress = |processed: usize, total: usize| {
        emit_workspace_analysis_progress(
            Some(&progress_notifications),
            progress_sink,
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
    let request = if workspace_uses_editor_first_mode(state, workspace_uri) {
        let source_filter = workspace_local_source_uris(state, workspace_uri);
        build_remote_dependency_batch_for_workspace_filtered(
            state,
            workspace_uri,
            Some(&source_filter),
        )
    } else {
        build_remote_dependency_batch_for_workspace(state, workspace_uri)
    };
    if let Some(request) = request {
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
    let start = Instant::now();
    let uri = abap_lsp::normalize_lsp_uri(params.text_document.uri.as_str());
    let workspace = state.workspace_for_uri_mut(&uri)?;
    workspace.open_documents.insert(
        uri.clone(),
        abap_lsp::OpenDocumentOverlay {
            version: params.text_document.version,
            text: Arc::from(params.text_document.text.as_str()),
        },
    );
    let root_uri = workspace.root_uri.clone();
    let _ = workspace;
    let preview_staged = stage_workspace_preview_snapshot(
        state,
        params.text_document.uri.as_str(),
        params.text_document.version,
        &params.text_document.text,
    );
    let preview = state
        .workspaces
        .get(&root_uri)
        .and_then(|workspace| workspace.preview_snapshots.get(&uri));
    debug!(
        uri,
        workspace_uri = %root_uri,
        text_bytes = params.text_document.text.len(),
        preview_staged,
        preview_is_dependency = preview.is_some_and(|snapshot| snapshot.is_dependency),
        elapsed = ?start.elapsed(),
        "staged textDocument/didOpen overlay"
    );
    Some(root_uri)
}

fn stage_workspace_change_overlay(
    state: &mut ServerState,
    params: &DidChangeTextDocumentParams,
) -> Option<String> {
    let start = Instant::now();
    let change = params.content_changes.last()?;
    let uri = abap_lsp::normalize_lsp_uri(params.text_document.uri.as_str());
    let workspace = state.workspace_for_uri_mut(&uri)?;
    workspace.open_documents.insert(
        uri.clone(),
        abap_lsp::OpenDocumentOverlay {
            version: params.text_document.version,
            text: Arc::from(change.text.as_str()),
        },
    );
    let root_uri = workspace.root_uri.clone();
    let _ = workspace;
    let preview_staged = stage_workspace_preview_snapshot(
        state,
        params.text_document.uri.as_str(),
        params.text_document.version,
        &change.text,
    );
    let preview = state
        .workspaces
        .get(&root_uri)
        .and_then(|workspace| workspace.preview_snapshots.get(&uri));
    debug!(
        uri,
        workspace_uri = %root_uri,
        text_bytes = change.text.len(),
        preview_staged,
        preview_is_dependency = preview.is_some_and(|snapshot| snapshot.is_dependency),
        elapsed = ?start.elapsed(),
        "staged textDocument/didChange overlay"
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
                let workspace_folder_count = params.workspace_folders.len();
                let root_uri_for_log = params.root_uri.as_deref().unwrap_or("<none>").to_string();
                let dependency_cache_path_for_log = params
                    .initialization_options
                    .dependency_cache_path
                    .as_deref()
                    .map(str::trim)
                    .filter(|path| !path.is_empty())
                    .unwrap_or("<default>")
                    .to_string();
                state.client_capabilities.completion_snippet_support = params
                    .capabilities
                    .text_document
                    .completion
                    .completion_item
                    .snippet_support;
                state.dependency_store_path_override = params
                    .initialization_options
                    .dependency_cache_path
                    .as_deref()
                    .map(str::trim)
                    .filter(|path| !path.is_empty())
                    .map(std::path::PathBuf::from);
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
                info!(
                    workspace_folder_count,
                    root_uri = %root_uri_for_log,
                    dependency_cache_path = %dependency_cache_path_for_log,
                    registered_workspace_count = state.workspaces.len(),
                    "handled initialize"
                );
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
                .map(|params| handle_did_open_notifications(state, &params, None))
                .transpose()?
                .unwrap_or_default();
            Ok(HandledMessage {
                response: None,
                notifications,
            })
        }
        Some("textDocument/didChange") => {
            let notifications = parse_params::<DidChangeTextDocumentParams>(&message)?
                .map(|params| handle_did_change_notifications(state, &params, None))
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
                .map(|params| handle_workspace_manifest_updated_notifications(state, &params, None))
                .transpose()?
                .unwrap_or_default(),
        }),
        Some(DEPENDENCY_CACHE_REFRESH_REQUESTED) => Ok(HandledMessage {
            response: None,
            notifications: parse_params::<WorkspaceManifestUpdatedParams>(&message)?
                .map(|params| {
                    handle_dependency_cache_refresh_requested_notifications(state, &params, None)
                })
                .transpose()?
                .unwrap_or_default(),
        }),
        Some(REMOTE_DEPENDENCIES_UPDATED) => Ok(HandledMessage {
            response: None,
            notifications: parse_params::<abap_lsp::RemoteDependenciesUpdatedParams>(&message)?
                .map(|params| {
                    handle_remote_dependencies_updated_notifications(state, &params, None)
                })
                .transpose()?
                .unwrap_or_default(),
        }),
        Some(SAP_ATC_RESULTS_UPDATED) => {
            let mut notifications = Vec::new();
            if let Some(params) = parse_params::<SapAtcResultsUpdatedParams>(&message)? {
                let snapshots = handle_sap_atc_results_updated(state, &params);
                for snapshot in snapshots {
                    push_publish_diagnostics_notification(
                        state,
                        snapshot.as_ref(),
                        &mut notifications,
                    )?;
                }
            }
            Ok(HandledMessage {
                response: None,
                notifications,
            })
        }
        Some("initialized") => {
            let mut notifications = Vec::new();
            let workspace_uris: Vec<_> = state.workspaces.keys().cloned().collect();
            for workspace_uri in workspace_uris {
                notifications.extend(handle_initialized_workspace_notifications(
                    state,
                    &workspace_uri,
                    None,
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
            let uri = abap_lsp::normalize_lsp_uri(
                definition_params
                    .text_document_position_params
                    .text_document
                    .uri
                    .as_str(),
            );
            let position = definition_params.text_document_position_params.position;
            let start = Instant::now();
            let definition_result = definition(state, &definition_params);
            let elapsed = start.elapsed();
            let (result_kind, target_uri) = definition_response_summary(&definition_result);
            debug!(
                uri,
                line = position.line,
                character = position.character,
                result_kind,
                target_uri = target_uri.as_deref().unwrap_or("<none>"),
                elapsed = ?elapsed,
                "handled textDocument/definition"
            );
            let result = serde_json::to_value(definition_result)?;
            Ok(HandledMessage {
                response: Some(Response::success(id.unwrap_or(Value::Null), result)),
                notifications: Vec::new(),
            })
        }
        Some(STORE_REMOTE_DEPENDENCY_ARTIFACTS) => {
            let Some(store_params) =
                parse_params::<StoreRemoteDependencyArtifactsParams>(&message)?
            else {
                return Ok(HandledMessage {
                    response: Some(Response::failure(
                        id.unwrap_or(Value::Null),
                        INVALID_REQUEST,
                        "abapls/storeRemoteDependencyArtifacts requires params",
                    )),
                    notifications: Vec::new(),
                });
            };
            if let Err(message) = store_remote_dependency_artifacts(state, &store_params) {
                return Ok(HandledMessage {
                    response: Some(Response::failure(
                        id.unwrap_or(Value::Null),
                        INVALID_PARAMS,
                        message,
                    )),
                    notifications: Vec::new(),
                });
            }
            Ok(HandledMessage {
                response: Some(Response::success(id.unwrap_or(Value::Null), Value::Null)),
                notifications: Vec::new(),
            })
        }
        Some(READ_DEPENDENCY_DOCUMENT) => {
            let Some(read_params) =
                parse_params::<abap_lsp::ReadDependencyDocumentParams>(&message)?
            else {
                return Ok(HandledMessage {
                    response: Some(Response::failure(
                        id.unwrap_or(Value::Null),
                        INVALID_REQUEST,
                        "abapls/readDependencyDocument requires params",
                    )),
                    notifications: Vec::new(),
                });
            };
            let uri = abap_lsp::normalize_lsp_uri(&read_params.uri);
            let start = Instant::now();
            let result = match read_dependency_document(state, &read_params) {
                Ok(result) => result,
                Err(message) => {
                    return Ok(HandledMessage {
                        response: Some(Response::failure(
                            id.unwrap_or(Value::Null),
                            INVALID_PARAMS,
                            message,
                        )),
                        notifications: Vec::new(),
                    });
                }
            };
            debug!(
                uri,
                found = result.is_some(),
                source_bytes = result.as_ref().map(|result| result.source_text.len()).unwrap_or(0),
                elapsed = ?start.elapsed(),
                "handled abapls/readDependencyDocument"
            );
            let result = serde_json::to_value(result)?;
            Ok(HandledMessage {
                response: Some(Response::success(id.unwrap_or(Value::Null), result)),
                notifications: Vec::new(),
            })
        }
        Some("textDocument/inlayHint") => {
            let Some(inlay_hint_params) = parse_params::<InlayHintParams>(&message)? else {
                return Ok(HandledMessage {
                    response: Some(Response::failure(
                        id.unwrap_or(Value::Null),
                        INVALID_REQUEST,
                        "textDocument/inlayHint requires params",
                    )),
                    notifications: Vec::new(),
                });
            };
            let result = serde_json::to_value(inlay_hints(state, &inlay_hint_params))?;
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
        Some("textDocument/prepareRename") => {
            let Some(rename_params) = parse_params::<TextDocumentPositionParams>(&message)? else {
                return Ok(HandledMessage {
                    response: Some(Response::failure(
                        id.unwrap_or(Value::Null),
                        INVALID_REQUEST,
                        "textDocument/prepareRename requires params",
                    )),
                    notifications: Vec::new(),
                });
            };
            let result = serde_json::to_value(prepare_rename(state, &rename_params))?;
            Ok(HandledMessage {
                response: Some(Response::success(id.unwrap_or(Value::Null), result)),
                notifications: Vec::new(),
            })
        }
        Some("textDocument/rename") => {
            let Some(rename_params) = parse_params::<RenameParams>(&message)? else {
                return Ok(HandledMessage {
                    response: Some(Response::failure(
                        id.unwrap_or(Value::Null),
                        INVALID_REQUEST,
                        "textDocument/rename requires params",
                    )),
                    notifications: Vec::new(),
                });
            };
            let result = match rename(state, &rename_params) {
                Ok(result) => result,
                Err(message) => {
                    return Ok(HandledMessage {
                        response: Some(Response::failure(
                            id.unwrap_or(Value::Null),
                            INVALID_PARAMS,
                            message,
                        )),
                        notifications: Vec::new(),
                    });
                }
            };
            let result = serde_json::to_value(result)?;
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
        Some("textDocument/codeAction") => {
            let Some(code_action_params) = parse_params::<CodeActionParams>(&message)? else {
                return Ok(HandledMessage {
                    response: Some(Response::failure(
                        id.unwrap_or(Value::Null),
                        INVALID_REQUEST,
                        "textDocument/codeAction requires params",
                    )),
                    notifications: Vec::new(),
                });
            };
            let result = serde_json::to_value(code_actions(state, &code_action_params))?;
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
        Some("textDocument/foldingRange") => {
            let Some(folding_params) = parse_params::<FoldingRangeParams>(&message)? else {
                return Ok(HandledMessage {
                    response: Some(Response::failure(
                        id.unwrap_or(Value::Null),
                        INVALID_REQUEST,
                        "textDocument/foldingRange requires params",
                    )),
                    notifications: Vec::new(),
                });
            };
            let result = serde_json::to_value(folding_ranges(state, &folding_params))?;
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
        DEPENDENCY_CACHE_REFRESH_REQUESTED => {
            let Some(params) = parse_params::<WorkspaceManifestUpdatedParams>(message)? else {
                return Ok(None);
            };
            WorkspaceAnalysisStatusParams {
                workspace_uri: abap_lsp::normalize_lsp_uri(&params.workspace_uri),
                phase: WorkspaceAnalysisPhase::Started,
                trigger: "dependency-cache-refresh".to_string(),
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
    Some(workspace_analysis_status_finished_for_workspace(
        started, workspace,
    ))
}

fn workspace_analysis_status_finished_for_workspace(
    started: &WorkspaceAnalysisStatusParams,
    workspace: &WorkspaceState,
) -> WorkspaceAnalysisStatusParams {
    let document_count = workspace.cache.uris().len();
    WorkspaceAnalysisStatusParams {
        workspace_uri: started.workspace_uri.clone(),
        phase: WorkspaceAnalysisPhase::Finished,
        trigger: started.trigger.clone(),
        processed_document_count: document_count,
        total_document_count: document_count,
        analyzed_document_count: document_count,
        remote_resolution_in_flight: workspace.remote_resolution_in_flight,
    }
}

fn emit_workspace_analysis_progress(
    notifications: Option<&Mutex<Vec<(String, Value)>>>,
    sink: Option<&(dyn Fn(WorkspaceAnalysisStatusParams) + Sync)>,
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
    if let Some(sink) = sink {
        sink(params);
        return;
    }
    if let Some(notifications) = notifications {
        notifications
            .lock()
            .expect("progress notification collection should not be poisoned")
            .push((
                WORKSPACE_ANALYSIS_STATUS.to_string(),
                serde_json::to_value(params).expect("workspace analysis progress should serialize"),
            ));
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::sync::mpsc;
    use std::sync::{Arc, Mutex};
    use std::time::{Instant, SystemTime, UNIX_EPOCH};

    use super::{
        AnalysisCompletion, AnalysisTaskKind, CHANGE_ANALYSIS_DEBOUNCE,
        EDITOR_FIRST_DIAGNOSTIC_LIMIT, PendingAnalysisQueue, REMOTE_DEPENDENCIES_UPDATED,
        RESOLVE_REMOTE_DEPENDENCIES, finish_background_task, flush_analysis_completions,
        flush_due_debounced_tasks, handle_did_change_notifications, handle_message,
        run_analysis_task, send_analysis_completion, take_pending_background_task,
        try_schedule_background_analysis, workspace_analysis_status_finished,
        workspace_analysis_status_started,
    };
    use abap_lsp::{
        DependencyArtifactPayload, DidChangeTextDocumentParams, SAP_ATC_RESULTS_UPDATED,
        ServerConfig, ServerState, StoreRemoteDependencyArtifactsParams, WorkspacePerformanceMode,
        normalize_lsp_uri, refresh_workspace, store_remote_dependency_artifacts,
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

    fn lsp_position_for_offset(text: &str, offset: usize) -> Value {
        let mut line = 0u32;
        let mut line_start = 0usize;
        for (idx, byte) in text.bytes().enumerate() {
            if idx == offset {
                break;
            }
            if byte == b'\n' {
                line += 1;
                line_start = idx + 1;
            }
        }
        json!({
            "line": line,
            "character": offset.saturating_sub(line_start)
        })
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
                            .is_some_and(|name| name.eq_ignore_ascii_case(object_name))
                    })
                    .map(|_| uri.to_string())
            })
            .expect("dependency uri")
    }

    fn dependency_text_for_object_name(
        state: &ServerState,
        workspace_uri: &str,
        object_name: &str,
    ) -> String {
        let dependency_uri = dependency_uri_for_object_name(state, workspace_uri, object_name);
        state
            .workspaces
            .get(&normalize_lsp_uri(workspace_uri))
            .and_then(|workspace| workspace.cache.get(&dependency_uri))
            .map(|snapshot| snapshot.text.to_string())
            .expect("dependency text")
    }

    fn store_dependency_artifacts(
        state: &mut ServerState,
        workspace_uri: &str,
        artifacts: Vec<DependencyArtifactPayload>,
    ) {
        store_remote_dependency_artifacts(
            state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.to_string(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts,
                negative: Vec::new(),
            },
        )
        .expect("store dependency artifacts");
    }

    fn assert_source_has_no_lt_rogln_argument_error(
        notifications: &[(String, Value)],
        source_uri: &str,
    ) {
        let mut saw_source_diagnostics = false;
        for (_, payload) in notifications.iter().filter(|(method, payload)| {
            method == "textDocument/publishDiagnostics"
                && payload.get("uri").and_then(Value::as_str) == Some(source_uri)
        }) {
            saw_source_diagnostics = true;
            let diagnostics = payload
                .get("diagnostics")
                .and_then(Value::as_array)
                .expect("diagnostics");
            assert!(
                diagnostics.iter().all(|diagnostic| {
                    !diagnostic
                        .get("message")
                        .and_then(Value::as_str)
                        .is_some_and(|message| {
                            message.contains("argument 'lt_rogln'") && message.contains("s_rogln")
                        })
                }),
                "unexpected source diagnostics: {diagnostics:#?}"
            );
        }
        assert!(saw_source_diagnostics, "missing source diagnostics");
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
            manifest.push_str("[dependency_store]\n");
            manifest.push_str("product_version = \"s4-2023\"\n");
            manifest.push_str("default_package_version = \"001\"\n\n");
            manifest.push_str("[resolution]\n");
            manifest.push_str(&format!("dependency_mode = \"{mode}\"\n"));
            manifest.push('\n');
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
                "root_file = \"legacy-cache/dependencies/global-class/ZCL_DEP_{idx:04}.abap\"\n\n"
            ));
            manifest.push_str("[[unit.member]]\n");
            manifest.push_str("role = \"dependency\"\n");
            manifest.push_str(&format!(
                "file = \"legacy-cache/dependencies/global-class/ZCL_DEP_{idx:04}.abap\"\n"
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
    fn handles_inlay_hints_after_open_document() {
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        let text = "\
FORM f USING VALUE(iv_input) TYPE i CHANGING cv_text TYPE string.
  cv_text = |{ iv_input }|.
ENDFORM.

START-OF-SELECTION.
  DATA lv_input TYPE i VALUE 1.
  DATA lv_text TYPE string.
  PERFORM f USING lv_input CHANGING lv_text.
";

        let opened = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///perform_inlay.abap",
                        "languageId": "abap",
                        "version": 1,
                        "text": text
                    }
                }
            }),
        )
        .expect("didOpen");
        assert!(opened.response.is_none());
        assert_eq!(opened.notifications.len(), 1);

        let inlay_hint_msg = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "textDocument/inlayHint",
                "params": {
                    "textDocument": { "uri": "file:///perform_inlay.abap" },
                    "range": {
                        "start": { "line": 0, "character": 0 },
                        "end": { "line": 8, "character": 0 }
                    }
                }
            }),
        )
        .expect("inlay hints");

        let result = inlay_hint_msg
            .response
            .expect("inlay hint response")
            .result
            .expect("inlay hint result");
        assert!(result.to_string().contains("iv_input:"));
        assert!(result.to_string().contains("cv_text:"));
    }

    #[test]
    fn handles_folding_ranges_after_open_document() {
        let mut state = ServerState::default();
        let config = ServerConfig::default();

        handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///folding.abap",
                        "languageId": "abap",
                        "version": 1,
                        "text": "IF foo = 1.\n  WRITE / 'one'.\nELSE.\n  WRITE / 'other'.\nENDIF."
                    }
                }
            }),
        )
        .expect("didOpen");

        let folding_msg = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "textDocument/foldingRange",
                "params": {
                    "textDocument": { "uri": "file:///folding.abap" }
                }
            }),
        )
        .expect("folding ranges");

        let result = folding_msg
            .response
            .expect("folding response")
            .result
            .expect("folding result");
        assert_eq!(result[0]["startLine"], 0);
        assert_eq!(result[0]["endLine"], 1);
        assert_eq!(result[1]["startLine"], 2);
        assert_eq!(result[1]["endLine"], 3);
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
        state.register_workspace_folder(workspace_uri);
        let generations = Arc::new(Mutex::new(HashMap::new()));
        let queue_state = Arc::new(Mutex::new(PendingAnalysisQueue::default()));
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
            &queue_state,
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
        let task = queue_state
            .lock()
            .expect("pending analysis queue")
            .pending_tasks
            .get(&queued_workspace_uri)
            .expect("pending analysis task")
            .workspace_uri
            .clone();
        assert_eq!(task, normalized_workspace_uri);

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn editor_first_cold_open_skips_incomplete_preview_diagnostics_for_dependency_files() {
        let workspace_path = temp_workspace_path("editor_first_cold_dependency_preview");
        let dependency_class_dir = workspace_path
            .join("legacy-cache-root")
            .join("cache")
            .join("dependencies")
            .join("global-class");
        let dependency_ddic_dir = workspace_path
            .join("legacy-cache-root")
            .join("cache")
            .join("dependencies")
            .join("ddic-data-element");
        fs::create_dir_all(&dependency_class_dir).expect("dependency class dir");
        fs::create_dir_all(&dependency_ddic_dir).expect("dependency ddic dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[performance]
mode = "editor-first"

[resolution]
dependency_mode = "remote-on-demand"

[[unit]]
name = "/STTP/CL_DEP"
kind = "global-class"
root_file = "legacy-cache/dependencies/global-class/%2FSTTP%2FCL_DEP.abap"

[[unit.member]]
role = "dependency"
file = "legacy-cache/dependencies/global-class/%2FSTTP%2FCL_DEP.abap"
object_name = "/STTP/CL_DEP"
"#,
        )
        .expect("manifest");
        let dependency_text = "\
CLASS /sttp/cl_dep DEFINITION INHERITING FROM /sttp/cl_base.
  PUBLIC SECTION.
    METHODS run
      IMPORTING
        !iv_evtid TYPE /sttp/e_evtid.
ENDCLASS.
CLASS /sttp/cl_dep IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.";
        fs::write(
            dependency_class_dir.join("%2FSTTP%2FCL_DEP.abap"),
            dependency_text,
        )
        .expect("dependency file");
        fs::write(
            dependency_class_dir.join("%2FSTTP%2FCL_BASE.abap"),
            "\
CLASS /sttp/cl_base DEFINITION.
ENDCLASS.
CLASS /sttp/cl_base IMPLEMENTATION.
ENDCLASS.",
        )
        .expect("base dependency");
        fs::write(
            dependency_ddic_dir.join("%2FSTTP%2FE_EVTID.xml"),
            "<root><DATATYPE>CHAR</DATATYPE></root>",
        )
        .expect("type dependency");

        let workspace_uri = file_uri(&workspace_path);
        let dependency_uri =
            format!("{workspace_uri}/legacy-cache/dependencies/global-class/%2FSTTP%2FCL_DEP.abap");
        let normalized_workspace_uri = abap_lsp::normalize_lsp_uri(&workspace_uri);
        let normalized_dependency_uri = abap_lsp::normalize_lsp_uri(&dependency_uri);

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        assert!(super::workspace_uses_editor_first_mode(
            &state,
            &workspace_uri
        ));

        let generations = Arc::new(Mutex::new(HashMap::new()));
        let queue_state = Arc::new(Mutex::new(PendingAnalysisQueue::default()));
        let mut debounced_tasks = HashMap::new();
        let (task_tx, _task_rx) = mpsc::sync_channel(1);
        let message = json!({
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
        });

        let scheduled = try_schedule_background_analysis(
            &mut state,
            &message,
            &task_tx,
            &queue_state,
            &generations,
            &mut debounced_tasks,
        )
        .expect("schedule")
        .expect("background job");
        assert!(
            scheduled.notifications.iter().all(|(method, payload)| {
                method != "textDocument/publishDiagnostics"
                    || payload.get("uri").and_then(Value::as_str)
                        != Some(normalized_dependency_uri.as_str())
            }),
            "unexpected preview diagnostics: {:?}",
            scheduled.notifications
        );
        let workspace = state
            .workspaces
            .get(&normalized_workspace_uri)
            .expect("workspace");
        assert!(
            !workspace
                .preview_snapshots
                .contains_key(&normalized_dependency_uri),
            "cold editor-first open should not stage a standalone preview snapshot"
        );

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
        let queue_state = Arc::new(Mutex::new(PendingAnalysisQueue::default()));
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
            &queue_state,
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
        let queue_state = Arc::new(Mutex::new(PendingAnalysisQueue::default()));
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
            &queue_state,
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
            labels.contains(&"run"),
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
        let queue_state = Arc::new(Mutex::new(PendingAnalysisQueue::default()));
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
                &queue_state,
                &generations,
                &mut debounced_tasks,
            )
            .expect("schedule")
            .expect("background job");
        }

        let first_workspace = task_rx.recv().expect("first queued workspace");
        assert_eq!(first_workspace, abap_lsp::normalize_lsp_uri(&workspace_uri));
        let pending_guard = queue_state.lock().expect("pending analysis queue");
        let latest_task = pending_guard
            .pending_tasks
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
    fn background_scheduler_requeues_latest_task_after_in_flight_workspace_finishes() {
        let workspace_path = temp_workspace_path("background_requeue_latest");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        let workspace_uri = file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/main.abap");
        let normalized_workspace_uri = abap_lsp::normalize_lsp_uri(&workspace_uri);

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri);
        let generations = Arc::new(Mutex::new(HashMap::new()));
        let queue_state = Arc::new(Mutex::new(PendingAnalysisQueue::default()));
        let mut debounced_tasks = HashMap::new();
        let (task_tx, task_rx) = mpsc::sync_channel(4);

        let mut schedule_change = |state: &mut ServerState, version| {
            try_schedule_background_analysis(
                state,
                &json!({
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
                }),
                &task_tx,
                &queue_state,
                &generations,
                &mut debounced_tasks,
            )
            .expect("schedule")
            .expect("background job");
        };

        schedule_change(&mut state, 2);
        let first_workspace = task_rx.recv().expect("first queued workspace");
        assert_eq!(first_workspace, normalized_workspace_uri);
        let in_flight = take_pending_background_task(&first_workspace, &queue_state)
            .expect("in-flight task should exist");
        match &in_flight.kind {
            AnalysisTaskKind::DidChange(params) => {
                assert_eq!(params.text_document.version, 2);
            }
            _ => panic!("expected didChange task"),
        }

        schedule_change(&mut state, 3);
        assert!(
            task_rx.try_recv().is_err(),
            "workspace should not be enqueued twice while already in flight"
        );

        finish_background_task(&first_workspace, &task_tx, &queue_state).expect("requeue");
        let requeued_workspace = task_rx.recv().expect("requeued workspace");
        assert_eq!(requeued_workspace, normalized_workspace_uri);
        let pending_guard = queue_state.lock().expect("pending analysis queue");
        match &pending_guard
            .pending_tasks
            .get(&requeued_workspace)
            .expect("latest task")
            .kind
        {
            AnalysisTaskKind::DidChange(params) => {
                assert_eq!(params.text_document.version, 3);
            }
            _ => panic!("expected didChange task"),
        }

        let _ = fs::remove_dir_all(&workspace_path);
    }

    #[test]
    fn stale_background_completion_still_forwards_current_root_diagnostics() {
        let workspace_path = temp_workspace_path("background_stale_diagnostics");
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        let workspace_uri = file_uri(&workspace_path);
        let normalized_workspace_uri = abap_lsp::normalize_lsp_uri(&workspace_uri);
        let source_uri = format!("{workspace_uri}/main.abap");
        let normalized_source_uri = abap_lsp::normalize_lsp_uri(&source_uri);

        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri);
        state
            .workspaces
            .get_mut(&normalized_workspace_uri)
            .expect("workspace")
            .open_documents
            .insert(
                normalized_source_uri,
                abap_lsp::OpenDocumentOverlay {
                    version: 1,
                    text: Arc::from("REPORT zmain."),
                },
            );

        let generations = Arc::new(Mutex::new(HashMap::from([(
            normalized_workspace_uri.clone(),
            2,
        )])));
        let (completion_tx, completion_rx) = mpsc::channel();
        let workspace = state
            .workspaces
            .get(&normalized_workspace_uri)
            .expect("workspace")
            .clone();
        send_analysis_completion(
            &completion_tx,
            AnalysisCompletion {
                workspace_uri: normalized_workspace_uri.clone(),
                generation: 1,
                started: None,
                workspace,
                notifications: vec![(
                    "textDocument/publishDiagnostics".to_string(),
                    json!({
                        "uri": source_uri,
                        "version": 1,
                        "diagnostics": []
                    }),
                )],
            },
        )
        .expect("completion");
        drop(completion_tx);

        let mut writer = Vec::new();
        flush_analysis_completions(&mut state, &mut writer, &completion_rx, &generations)
            .expect("flush completions");

        let output = String::from_utf8(writer).expect("utf8 output");
        assert!(
            output.contains("textDocument/publishDiagnostics"),
            "stale completion should still clear diagnostics for unchanged open documents: {output}"
        );
        assert!(output.contains(&source_uri));

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
        let queue_state = Arc::new(Mutex::new(PendingAnalysisQueue::default()));
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
            &queue_state,
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
        let queue_state = Arc::new(Mutex::new(PendingAnalysisQueue::default()));
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
                &queue_state,
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
            &queue_state,
        )
        .expect("flush");
        assert!(debounced_tasks.is_empty());
        let queued_workspace = task_rx.recv().expect("queued workspace");
        let pending = queue_state.lock().expect("pending analysis queue");
        match &pending
            .pending_tasks
            .get(&queued_workspace)
            .expect("latest task")
            .kind
        {
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
        let queue_state = Arc::new(Mutex::new(PendingAnalysisQueue::default()));
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
            &queue_state,
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
            labels.contains(&"run"),
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

        let _opened = handle_message(
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
        let queue_state = Arc::new(Mutex::new(PendingAnalysisQueue::default()));
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
            &queue_state,
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
            &queue_state,
        )
        .expect("flush");
        let queued_workspace = task_rx.recv().expect("queued workspace");
        let task = queue_state
            .lock()
            .expect("pending analysis queue")
            .pending_tasks
            .remove(&queued_workspace)
            .expect("pending task");
        let completion = run_analysis_task(task, None).expect("analysis completion");
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
            None,
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
                .all(|uri| !uri.contains("legacy-cache/dependencies"))
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
        let opened = handle_message(
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
            None,
        )
        .expect("didChange");
        let all_notifications: Vec<_> = opened
            .notifications
            .iter()
            .chain(notifications.iter())
            .collect();
        let remote_request = all_notifications
            .iter()
            .find(|(method, _)| method == RESOLVE_REMOTE_DEPENDENCIES)
            .and_then(|(_, payload)| payload.get("sourceUris"))
            .and_then(Value::as_array)
            .unwrap_or_else(|| {
                panic!("remote request source uris; notifications={all_notifications:#?}")
            });
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
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[performance]
mode = "editor-first"

[resolution]
dependency_mode = "remote-on-demand"
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

        let workspace_uri = file_uri(&workspace_path);
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        store_dependency_artifacts(
            &mut state,
            &workspace_uri,
            vec![DependencyArtifactPayload {
                package_name: "/AIF/CORE".to_string(),
                object_kind: "function-group".to_string(),
                object_name: "/AIF/FILE_PROCESS_DATA".to_string(),
                object_uri: "/sap/bc/adt/functions/groups/%2FAIF%2FFILE_PROCESS_DATA/fmodules/%2FAIF%2FFILE_PROCESS_DATA"
                    .to_string(),
                object_type: "FUGR/F".to_string(),
                description: "Remote function module".to_string(),
                file_extension: "abap".to_string(),
                source_text: dependency_text.to_string(),
                fetched_at: "2026-04-23T00:00:00Z".to_string(),
            }],
        );
        let dependency_uri =
            dependency_uri_for_object_name(&state, &workspace_uri, "/AIF/FILE_PROCESS_DATA");
        let dependency_text =
            dependency_text_for_object_name(&state, &workspace_uri, "/AIF/FILE_PROCESS_DATA");

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
        assert!(
            source_uris
                .iter()
                .filter_map(Value::as_str)
                .any(|uri| uri == dependency_uri)
        );
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
    fn editor_first_open_report_requests_remote_candidates_from_local_includes() {
        let workspace_path = temp_workspace_path("editor_first_open_report_include_remote");
        let report_dir = workspace_path.join("src").join("reports").join("ZREP");
        fs::create_dir_all(&report_dir).expect("report dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[performance]
mode = "editor-first"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let report_text =
            "REPORT zrep.\nINCLUDE zrep_top.\nSTART-OF-SELECTION.\n  lo_app->run( ).\n";
        fs::write(report_dir.join("ZREP.abap"), report_text).expect("report");
        fs::write(
            report_dir.join("ZREP_TOP.abap"),
            "DATA lo_app TYPE REF TO zcl_remote.\n",
        )
        .expect("include");

        let workspace_uri = file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/src/reports/ZREP/ZREP.abap");
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
                        "uri": source_uri,
                        "languageId": "abap",
                        "version": 1,
                        "text": report_text
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
        let candidates = request
            .get("candidates")
            .and_then(Value::as_array)
            .expect("candidates");
        assert!(candidates.iter().any(|candidate| {
            candidate.get("kind").and_then(Value::as_str) == Some("type")
                && candidate.get("name").and_then(Value::as_str) == Some("zcl_remote")
        }));
    }

    #[test]
    fn editor_first_open_dependency_class_can_request_full_follow_up_candidates() {
        let workspace_path = temp_workspace_path("editor_first_open_dependency_class_remote");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[performance]
mode = "editor-first"

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

        let workspace_uri = file_uri(&workspace_path);
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        store_dependency_artifacts(
            &mut state,
            &workspace_uri,
            vec![DependencyArtifactPayload {
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
        );
        let dependency_uri = dependency_uri_for_object_name(&state, &workspace_uri, "ZCL_DEP");
        let dependency_text = dependency_text_for_object_name(&state, &workspace_uri, "ZCL_DEP");

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
        let candidates = request
            .get("candidates")
            .and_then(Value::as_array)
            .expect("candidates");
        assert!(candidates.iter().any(|candidate| {
            candidate.get("name").and_then(Value::as_str) == Some("zcl_missing")
        }));
    }

    #[test]
    fn editor_first_remote_dependency_updates_refresh_open_dependency_diagnostics() {
        let workspace_path = temp_workspace_path("editor_first_open_dependency_diag_refresh");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[performance]
mode = "editor-first"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let dependency_text = "\
CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS run
      IMPORTING
        !iv_evtid TYPE zmissing.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.";

        let workspace_uri = file_uri(&workspace_path);
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        store_dependency_artifacts(
            &mut state,
            &workspace_uri,
            vec![DependencyArtifactPayload {
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
        );
        let dependency_uri = dependency_uri_for_object_name(&state, &workspace_uri, "ZCL_DEP");
        let dependency_text = dependency_text_for_object_name(&state, &workspace_uri, "ZCL_DEP");
        let normalized_dependency_uri = normalize_lsp_uri(&dependency_uri);

        let opened = handle_message(
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

        let initial_diags = opened
            .notifications
            .iter()
            .find(|(method, payload)| {
                method == "textDocument/publishDiagnostics"
                    && payload.get("uri").and_then(Value::as_str)
                        == Some(normalized_dependency_uri.as_str())
            })
            .and_then(|(_, payload)| payload.get("diagnostics"))
            .and_then(Value::as_array)
            .expect("initial dependency diagnostics");
        assert!(initial_diags.iter().any(|diag| {
            diag.get("message")
                .and_then(Value::as_str)
                .is_some_and(|message| {
                    message.contains("Type 'zmissing' is not verified against a SAP system")
                })
        }));
        let request = opened
            .notifications
            .iter()
            .find(|(method, _)| method == RESOLVE_REMOTE_DEPENDENCIES)
            .map(|(_, payload)| payload)
            .expect("remote dependency request");
        let candidates = request
            .get("candidates")
            .and_then(Value::as_array)
            .expect("candidates");
        assert!(candidates.iter().any(|candidate| {
            candidate.get("kind").and_then(Value::as_str) == Some("type")
                && candidate.get("name").and_then(Value::as_str) == Some("zmissing")
        }));

        store_dependency_artifacts(
            &mut state,
            &workspace_uri,
            vec![DependencyArtifactPayload {
                package_name: "ZPKG".to_string(),
                object_kind: "ddic-data-element".to_string(),
                object_name: "ZMISSING".to_string(),
                object_uri: "/sap/bc/adt/ddic/dataelements/zmissing".to_string(),
                object_type: "DTEL/DT".to_string(),
                description: "Remote DDIC type".to_string(),
                file_extension: "xml".to_string(),
                source_text: "<root><DATATYPE>CHAR</DATATYPE></root>".to_string(),
                fetched_at: "2026-04-23T00:00:00Z".to_string(),
            }],
        );

        let refreshed = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": REMOTE_DEPENDENCIES_UPDATED,
                "params": {
                    "workspaceUri": workspace_uri,
                    "sourceUri": dependency_uri,
                    "sourceUris": [dependency_uri],
                    "fetched": ["zmissing"],
                    "failed": []
                }
            }),
        )
        .expect("remote dependencies updated");

        let refreshed_diags = refreshed
            .notifications
            .iter()
            .find(|(method, payload)| {
                method == "textDocument/publishDiagnostics"
                    && payload.get("uri").and_then(Value::as_str)
                        == Some(normalized_dependency_uri.as_str())
            })
            .and_then(|(_, payload)| payload.get("diagnostics"))
            .and_then(Value::as_array)
            .expect("refreshed dependency diagnostics");
        assert!(
            refreshed_diags.iter().all(|diag| {
                !diag
                    .get("message")
                    .and_then(Value::as_str)
                    .is_some_and(|message| {
                        message.contains("Type 'zmissing' is not verified against a SAP system")
                    })
            }),
            "unexpected refreshed diagnostics: {refreshed_diags:?}"
        );
    }

    #[test]
    fn editor_first_remote_dependency_updates_refresh_open_encoded_dependency_diagnostics() {
        let workspace_path = temp_workspace_path("editor_first_open_encoded_dependency_diag");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[performance]
mode = "editor-first"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let dependency_text = "\
CLASS /sttp/cl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS run
      IMPORTING
        !iv_evtid TYPE /sttp/zmissing.
ENDCLASS.
CLASS /sttp/cl_dep IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.";

        let workspace_uri = file_uri(&workspace_path);
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        store_dependency_artifacts(
            &mut state,
            &workspace_uri,
            vec![DependencyArtifactPayload {
                package_name: "/STTP/CORE".to_string(),
                object_kind: "global-class".to_string(),
                object_name: "/STTP/CL_DEP".to_string(),
                object_uri: "/sap/bc/adt/oo/classes/%2FSTTP%2FCL_DEP".to_string(),
                object_type: "CLAS/OC".to_string(),
                description: "Remote class".to_string(),
                file_extension: "abap".to_string(),
                source_text: dependency_text.to_string(),
                fetched_at: "2026-04-23T00:00:00Z".to_string(),
            }],
        );
        let dependency_uri = dependency_uri_for_object_name(&state, &workspace_uri, "/STTP/CL_DEP");
        let dependency_text =
            dependency_text_for_object_name(&state, &workspace_uri, "/STTP/CL_DEP");
        let normalized_dependency_uri = normalize_lsp_uri(&dependency_uri);

        let opened = handle_message(
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

        let initial_diags = opened
            .notifications
            .iter()
            .find(|(method, payload)| {
                method == "textDocument/publishDiagnostics"
                    && payload.get("uri").and_then(Value::as_str)
                        == Some(normalized_dependency_uri.as_str())
            })
            .and_then(|(_, payload)| payload.get("diagnostics"))
            .and_then(Value::as_array)
            .expect("initial dependency diagnostics");
        assert!(initial_diags.iter().any(|diag| {
            diag.get("message")
                .and_then(Value::as_str)
                .is_some_and(|message| {
                    message.contains("Type '/sttp/zmissing' is not verified against a SAP system")
                })
        }));

        store_dependency_artifacts(
            &mut state,
            &workspace_uri,
            vec![DependencyArtifactPayload {
                package_name: "/STTP/CORE".to_string(),
                object_kind: "ddic-data-element".to_string(),
                object_name: "/STTP/ZMISSING".to_string(),
                object_uri: "/sap/bc/adt/ddic/dataelements/%2FSTTP%2FZMISSING".to_string(),
                object_type: "DTEL/DT".to_string(),
                description: "Remote DDIC type".to_string(),
                file_extension: "xml".to_string(),
                source_text: "<root><DATATYPE>CHAR</DATATYPE></root>".to_string(),
                fetched_at: "2026-04-23T00:00:00Z".to_string(),
            }],
        );

        let refreshed = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": REMOTE_DEPENDENCIES_UPDATED,
                "params": {
                    "workspaceUri": workspace_uri,
                    "sourceUri": dependency_uri,
                    "sourceUris": [dependency_uri],
                    "fetched": ["/sttp/zmissing"],
                    "failed": []
                }
            }),
        )
        .expect("remote dependencies updated");

        let refreshed_diags = refreshed
            .notifications
            .iter()
            .find(|(method, payload)| {
                method == "textDocument/publishDiagnostics"
                    && payload.get("uri").and_then(Value::as_str)
                        == Some(normalized_dependency_uri.as_str())
            })
            .and_then(|(_, payload)| payload.get("diagnostics"))
            .and_then(Value::as_array)
            .expect("refreshed dependency diagnostics");
        assert!(
            refreshed_diags.iter().all(|diag| {
                !diag
                    .get("message")
                    .and_then(Value::as_str)
                    .is_some_and(|message| {
                        message
                            .contains("Type '/sttp/zmissing' is not verified against a SAP system")
                    })
            }),
            "unexpected refreshed diagnostics: {refreshed_diags:?}"
        );
    }

    #[test]
    fn editor_first_initialized_remote_batch_starts_from_local_sources_only() {
        let workspace_path = temp_workspace_path("editor_first_initialized_local_only_remote");
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
mode = "editor-first"

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
"#,
        )
        .expect("manifest");
        fs::write(
            source_dir.join("ZREPORT_MAIN.abap"),
            "\
REPORT zreport_main.
DATA lo_first TYPE REF TO zcl_first.
",
        )
        .expect("main source");

        let workspace_uri = file_uri(&workspace_path);
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        store_dependency_artifacts(
            &mut state,
            &workspace_uri,
            vec![DependencyArtifactPayload {
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
    METHODS run.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
  METHOD run.
    DATA lo_second TYPE REF TO zcl_second.
  ENDMETHOD.
ENDCLASS.
"
                .to_string(),
                fetched_at: "2026-04-23T00:00:00Z".to_string(),
            }],
        );

        let handled = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "initialized",
                "params": {}
            }),
        )
        .expect("initialized");

        let request = handled
            .notifications
            .iter()
            .find(|(method, _)| method == RESOLVE_REMOTE_DEPENDENCIES)
            .map(|(_, payload)| payload)
            .expect("startup remote dependency request");
        let source_uris = request
            .get("sourceUris")
            .and_then(Value::as_array)
            .expect("source uris");
        assert!(
            source_uris
                .iter()
                .filter_map(Value::as_str)
                .any(|uri| uri.ends_with("/src/ZREPORT_MAIN.abap")),
            "unexpected source uris: {source_uris:?}"
        );
        assert!(
            source_uris
                .iter()
                .filter_map(Value::as_str)
                .all(|uri| !uri.to_ascii_lowercase().starts_with("abapls-cache:")),
            "unexpected source uris: {source_uris:?}"
        );
        let candidates = request
            .get("candidates")
            .and_then(Value::as_array)
            .expect("candidates");
        assert!(candidates.iter().any(|candidate| {
            candidate.get("name").and_then(Value::as_str) == Some("zcl_first")
        }));
        assert!(!candidates.iter().any(|candidate| {
            candidate.get("name").and_then(Value::as_str) == Some("zcl_second")
        }));
    }

    #[test]
    fn editor_first_remote_dependency_updates_trigger_follow_up_dependency_fetches() {
        let workspace_path = temp_workspace_path("editor_first_dependency_follow_up");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(&workspace_path).expect("workspace dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[dependency_store]
product_version = "s4-2023"
default_package_version = "001"

[performance]
mode = "editor-first"

[resolution]
dependency_mode = "remote-on-demand"
"#,
        )
        .expect("manifest");
        let dependency_text = "\
CLASS /STTP/CL_MESSAGES DEFINITION
  PUBLIC
  INHERITING FROM /CDBASIS/CL_MESSAGES
  CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES ts_bal_msg TYPE BAL_S_MSG.
    CONSTANTS:
      BEGIN OF gcs_log_level,
        very_high TYPE te_loglevel VALUE 1,
END OF gcs_log_level.
ENDCLASS.
CLASS /STTP/CL_MESSAGES IMPLEMENTATION.
ENDCLASS.";

        let workspace_uri = file_uri(&workspace_path);
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        store_dependency_artifacts(
            &mut state,
            &workspace_uri,
            vec![DependencyArtifactPayload {
                package_name: "/STTP/CORE".to_string(),
                object_kind: "global-class".to_string(),
                object_name: "/STTP/CL_MESSAGES".to_string(),
                object_uri: "/sap/bc/adt/oo/classes/%2FSTTP%2FCL_MESSAGES".to_string(),
                object_type: "CLAS/OC".to_string(),
                description: "Remote class".to_string(),
                file_extension: "abap".to_string(),
                source_text: dependency_text.to_string(),
                fetched_at: "2026-04-23T00:00:00Z".to_string(),
            }],
        );
        let dependency_uri =
            dependency_uri_for_object_name(&state, &workspace_uri, "/STTP/CL_MESSAGES");

        let handled = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": REMOTE_DEPENDENCIES_UPDATED,
                "params": {
                    "workspaceUri": workspace_uri,
                    "sourceUri": dependency_uri,
                    "sourceUris": [dependency_uri],
                    "fetched": ["/sttp/cl_messages"],
                    "failed": []
                }
            }),
        )
        .expect("remote dependencies updated");

        let request = handled
            .notifications
            .iter()
            .find(|(method, _)| method == RESOLVE_REMOTE_DEPENDENCIES)
            .map(|(_, payload)| payload)
            .expect("follow-up remote dependency request");
        let source_uris = request
            .get("sourceUris")
            .and_then(Value::as_array)
            .expect("source uris");
        assert!(
            source_uris
                .iter()
                .filter_map(Value::as_str)
                .any(|uri| uri == dependency_uri),
            "unexpected source uris: {source_uris:?}"
        );
        assert!(
            source_uris
                .iter()
                .filter_map(Value::as_str)
                .all(|uri| !uri.ends_with("/src/ZREPORT_MAIN.abap")),
            "unexpected source uris: {source_uris:?}"
        );
        let candidates = request
            .get("candidates")
            .and_then(Value::as_array)
            .expect("candidates");
        assert!(candidates.iter().any(|candidate| {
            candidate.get("kind").and_then(Value::as_str) == Some("type")
                && candidate.get("name").and_then(Value::as_str) == Some("/cdbasis/cl_messages")
        }));
        assert!(candidates.iter().any(|candidate| {
            candidate.get("kind").and_then(Value::as_str) == Some("type")
                && candidate.get("name").and_then(Value::as_str) == Some("bal_s_msg")
        }));
        assert!(candidates.iter().any(|candidate| {
            candidate.get("kind").and_then(Value::as_str) == Some("type")
                && candidate.get("name").and_then(Value::as_str) == Some("te_loglevel")
        }));
    }

    #[test]
    fn editor_first_remote_dependency_updates_do_not_rescan_unrelated_dirty_dependencies() {
        let workspace_path = temp_workspace_path("editor_first_dependency_follow_up_scope_limited");
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
mode = "editor-first"

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
"#,
        )
        .expect("manifest");
        fs::write(
            source_dir.join("ZREPORT_MAIN.abap"),
            "REPORT zreport_main.\nDATA lo_msg TYPE REF TO /sttp/cl_messages.\n",
        )
        .expect("main");

        let workspace_uri = file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/src/ZREPORT_MAIN.abap");
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        store_dependency_artifacts(
            &mut state,
            &workspace_uri,
            vec![
                DependencyArtifactPayload {
                    package_name: "/STTP/CORE".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "/STTP/CL_MESSAGES".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/%2FSTTP%2FCL_MESSAGES".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Fetched dependency".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "\
CLASS /STTP/CL_MESSAGES DEFINITION.
  PUBLIC SECTION.
    DATA ms_bal TYPE bal_s_msg.
ENDCLASS.
CLASS /STTP/CL_MESSAGES IMPLEMENTATION.
ENDCLASS."
                        .to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                },
                DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_UNRELATED".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_unrelated".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Unrelated dependency".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "\
CLASS zcl_unrelated DEFINITION.
  PUBLIC SECTION.
    DATA mo_missing TYPE REF TO zcl_noise.
ENDCLASS.
CLASS zcl_unrelated IMPLEMENTATION.
ENDCLASS."
                        .to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                },
            ],
        );
        let dependency_uri =
            dependency_uri_for_object_name(&state, &workspace_uri, "/STTP/CL_MESSAGES");
        let unrelated_dependency_uri =
            dependency_uri_for_object_name(&state, &workspace_uri, "ZCL_UNRELATED");
        assert!(super::workspace_uses_editor_first_mode(
            &state,
            &workspace_uri
        ));

        let handled = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": REMOTE_DEPENDENCIES_UPDATED,
                "params": {
                    "workspaceUri": workspace_uri,
                    "sourceUri": source_uri,
                    "sourceUris": [source_uri],
                    "fetched": ["/sttp/cl_messages"],
                    "failed": []
                }
            }),
        )
        .expect("remote dependencies updated");

        let request = handled
            .notifications
            .iter()
            .find(|(method, _)| method == RESOLVE_REMOTE_DEPENDENCIES)
            .map(|(_, payload)| payload)
            .expect("follow-up remote dependency request");
        let source_uris = request
            .get("sourceUris")
            .and_then(Value::as_array)
            .expect("source uris");
        assert!(
            source_uris
                .iter()
                .filter_map(Value::as_str)
                .any(|uri| uri == dependency_uri),
            "unexpected source uris: {source_uris:?}"
        );
        assert!(
            source_uris
                .iter()
                .filter_map(Value::as_str)
                .all(|uri| uri != unrelated_dependency_uri),
            "unexpected source uris: {source_uris:?}"
        );
        let candidates = request
            .get("candidates")
            .and_then(Value::as_array)
            .expect("candidates");
        assert!(candidates.iter().any(|candidate| {
            candidate.get("kind").and_then(Value::as_str) == Some("type")
                && candidate.get("name").and_then(Value::as_str) == Some("bal_s_msg")
        }));
        assert!(!candidates.iter().any(|candidate| {
            candidate.get("name").and_then(Value::as_str) == Some("zcl_noise")
        }));
    }

    #[test]
    fn full_workspace_did_open_scopes_remote_dependency_batch_to_opened_source() {
        let workspace_path = temp_workspace_path("full_workspace_open_scope_limited");
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
name = "ZREPORT_ONE"
kind = "report"
root_file = "src/ZREPORT_ONE.abap"

[[unit.member]]
role = "root"
file = "src/ZREPORT_ONE.abap"
object_name = "ZREPORT_ONE"

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
            source_dir.join("ZREPORT_ONE.abap"),
            "REPORT zreport_one.\nDATA lo_first TYPE REF TO zcl_first.\n",
        )
        .expect("report one");
        fs::write(
            source_dir.join("ZREPORT_TWO.abap"),
            "REPORT zreport_two.\nDATA lo_second TYPE REF TO zcl_second.\n",
        )
        .expect("report two");

        let workspace_uri = file_uri(&workspace_path);
        let open_uri = format!("{workspace_uri}/src/ZREPORT_ONE.abap");
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri);

        let handled = handle_message(
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
                        "text": "REPORT zreport_one.\nDATA lo_first TYPE REF TO zcl_first.\n"
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
        let candidates = request
            .get("candidates")
            .and_then(Value::as_array)
            .expect("candidates");
        assert!(candidates.iter().any(|candidate| {
            candidate.get("name").and_then(Value::as_str) == Some("zcl_first")
        }));
        assert!(!candidates.iter().any(|candidate| {
            candidate.get("name").and_then(Value::as_str) == Some("zcl_second")
        }));
    }

    #[test]
    fn full_workspace_direct_open_dependency_retries_negative_candidates() {
        let workspace_path = temp_workspace_path("full_workspace_open_dependency_retry_negatives");
        let _ = fs::remove_dir_all(&workspace_path);
        fs::create_dir_all(&workspace_path).expect("workspace dir");
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

        let workspace_uri = file_uri(&workspace_path);
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        store_dependency_artifacts(
            &mut state,
            &workspace_uri,
            vec![DependencyArtifactPayload {
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
        );
        store_remote_dependency_artifacts(
            &mut state,
            &StoreRemoteDependencyArtifactsParams {
                workspace_uri: workspace_uri.clone(),
                connection_key: Some("https://example.sap.local".to_string()),
                artifacts: Vec::new(),
                negative: vec![
                    abap_lsp::RemoteDependencyCandidate {
                        name: "zattp_t_param_value".to_string(),
                        kind: "type".to_string(),
                    },
                    abap_lsp::RemoteDependencyCandidate {
                        name: "rsds_frange_t".to_string(),
                        kind: "type".to_string(),
                    },
                ],
            },
        )
        .expect("store negative lookups");
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

        let dependency_uri =
            dependency_uri_for_object_name(&state, &workspace_uri, "ZATTP_CL_AR_DM_OBJECT");
        let dependency_text =
            dependency_text_for_object_name(&state, &workspace_uri, "ZATTP_CL_AR_DM_OBJECT");
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
        .expect("didOpen dependency");

        let request = handled
            .notifications
            .iter()
            .find(|(method, _)| method == RESOLVE_REMOTE_DEPENDENCIES)
            .map(|(_, payload)| payload)
            .expect("forced dependency request");
        assert_eq!(
            request
                .get("retryNegativeCandidates")
                .and_then(Value::as_bool),
            Some(true),
            "{request:#?}"
        );
        let candidates = request
            .get("candidates")
            .and_then(Value::as_array)
            .expect("candidates");
        assert!(candidates.iter().any(|candidate| {
            candidate.get("kind").and_then(Value::as_str) == Some("type")
                && candidate.get("name").and_then(Value::as_str) == Some("zattp_t_param_value")
        }));
        assert!(candidates.iter().any(|candidate| {
            candidate.get("kind").and_then(Value::as_str) == Some("type")
                && candidate.get("name").and_then(Value::as_str) == Some("rsds_frange_t")
        }));
        assert!(
            state
                .workspaces
                .get(&normalize_lsp_uri(&workspace_uri))
                .is_some_and(|workspace| workspace.remote_resolution_in_flight),
            "forced direct-open request should mark a dependency wave in flight"
        );
    }

    #[test]
    fn full_workspace_remote_dependency_updates_do_not_rescan_unrelated_sources() {
        let workspace_path = temp_workspace_path("full_workspace_dependency_follow_up_scope");
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
            "REPORT zreport_main.\nDATA lo_msg TYPE REF TO /sttp/cl_messages.\n",
        )
        .expect("main");
        fs::write(
            source_dir.join("ZREPORT_OTHER.abap"),
            "REPORT zreport_other.\nDATA lo_other TYPE REF TO zcl_unrelated.\n",
        )
        .expect("other");

        let workspace_uri = file_uri(&workspace_path);
        let source_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/ZREPORT_MAIN.abap"));
        let other_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/ZREPORT_OTHER.abap"));
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        store_dependency_artifacts(
            &mut state,
            &workspace_uri,
            vec![
                DependencyArtifactPayload {
                    package_name: "/STTP/CORE".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "/STTP/CL_MESSAGES".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/%2FSTTP%2FCL_MESSAGES".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Fetched dependency".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "\
CLASS /STTP/CL_MESSAGES DEFINITION.
  PUBLIC SECTION.
    DATA ms_bal TYPE bal_s_msg.
ENDCLASS.
CLASS /STTP/CL_MESSAGES IMPLEMENTATION.
ENDCLASS."
                        .to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                },
                DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_UNRELATED".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_unrelated".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Unrelated dependency".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "\
CLASS zcl_unrelated DEFINITION.
  PUBLIC SECTION.
    DATA mo_missing TYPE REF TO zcl_noise.
ENDCLASS.
CLASS zcl_unrelated IMPLEMENTATION.
ENDCLASS."
                        .to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                },
            ],
        );
        let dependency_uri = normalize_lsp_uri(&dependency_uri_for_object_name(
            &state,
            &workspace_uri,
            "/STTP/CL_MESSAGES",
        ));

        let handled = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": REMOTE_DEPENDENCIES_UPDATED,
                "params": {
                    "workspaceUri": workspace_uri,
                    "sourceUri": source_uri,
                    "sourceUris": [source_uri],
                    "fetched": ["/sttp/cl_messages"],
                    "failed": []
                }
            }),
        )
        .expect("remote dependencies updated");

        let request = handled
            .notifications
            .iter()
            .find(|(method, _)| method == RESOLVE_REMOTE_DEPENDENCIES)
            .map(|(_, payload)| payload)
            .expect("follow-up remote dependency request");
        let source_uris = request
            .get("sourceUris")
            .and_then(Value::as_array)
            .expect("source uris");
        assert!(
            source_uris
                .iter()
                .filter_map(Value::as_str)
                .any(|uri| uri == dependency_uri),
            "unexpected source uris: {source_uris:?}"
        );
        assert!(
            source_uris
                .iter()
                .filter_map(Value::as_str)
                .all(|uri| uri == dependency_uri || uri == source_uri),
            "unexpected source uris: {source_uris:?}"
        );
        assert!(
            source_uris
                .iter()
                .filter_map(Value::as_str)
                .all(|uri| uri != other_uri),
            "unexpected source uris: {source_uris:?}"
        );
        let candidates = request
            .get("candidates")
            .and_then(Value::as_array)
            .expect("candidates");
        assert!(candidates.iter().any(|candidate| {
            candidate.get("kind").and_then(Value::as_str) == Some("type")
                && candidate.get("name").and_then(Value::as_str) == Some("bal_s_msg")
        }));
        assert!(!candidates.iter().any(|candidate| {
            candidate.get("name").and_then(Value::as_str) == Some("zcl_noise")
        }));
    }

    #[test]
    fn full_workspace_opened_dependency_during_in_flight_wave_gets_follow_up_request() {
        let workspace_path = temp_workspace_path("full_workspace_open_dependency_in_flight");
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
"#,
        )
        .expect("manifest");
        let source_text =
            "REPORT zattp_ar_dm_obj_pre.\nDATA lo_dep TYPE REF TO zattp_cl_ar_dm_object.\n";
        fs::write(source_dir.join("ZATTP_AR_DM_OBJ_PRE.abap"), source_text).expect("source");

        let workspace_uri = file_uri(&workspace_path);
        let source_uri =
            normalize_lsp_uri(&format!("{workspace_uri}/src/ZATTP_AR_DM_OBJ_PRE.abap"));
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());

        let opened_source = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": source_uri,
                        "languageId": "abap",
                        "version": 1,
                        "text": source_text
                    }
                }
            }),
        )
        .expect("didOpen source");
        let initial_request = opened_source
            .notifications
            .iter()
            .find(|(method, _)| method == RESOLVE_REMOTE_DEPENDENCIES)
            .map(|(_, payload)| payload)
            .expect("initial remote dependency request");
        let initial_candidates = initial_request
            .get("candidates")
            .and_then(Value::as_array)
            .expect("initial candidates");
        assert!(initial_candidates.iter().any(|candidate| {
            candidate.get("name").and_then(Value::as_str) == Some("zattp_cl_ar_dm_object")
        }));

        store_dependency_artifacts(
            &mut state,
            &workspace_uri,
            vec![
                DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZATTP_CL_AR_DM_OBJECT".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zattp_cl_ar_dm_object".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Fetched dependency".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "\
CLASS zattp_cl_ar_dm_object DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS zattp_cl_ar_dm_object IMPLEMENTATION.
  METHOD run.
    DATA lt_params TYPE zattp_t_param_value.
    DATA lt_ranges TYPE rsds_frange_t.
  ENDMETHOD.
ENDCLASS."
                        .to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                },
                DependencyArtifactPayload {
                    package_name: "ZPKG".to_string(),
                    object_kind: "global-class".to_string(),
                    object_name: "ZCL_UNRELATED".to_string(),
                    object_uri: "/sap/bc/adt/oo/classes/zcl_unrelated".to_string(),
                    object_type: "CLAS/OC".to_string(),
                    description: "Unrelated dependency".to_string(),
                    file_extension: "abap".to_string(),
                    source_text: "\
CLASS zcl_unrelated DEFINITION.
  PUBLIC SECTION.
    DATA lo_noise TYPE REF TO zcl_noise.
ENDCLASS.
CLASS zcl_unrelated IMPLEMENTATION.
ENDCLASS."
                        .to_string(),
                    fetched_at: "2026-04-23T00:00:00Z".to_string(),
                },
            ],
        );
        let dependency_uri =
            dependency_uri_for_object_name(&state, &workspace_uri, "ZATTP_CL_AR_DM_OBJECT");
        let dependency_text =
            dependency_text_for_object_name(&state, &workspace_uri, "ZATTP_CL_AR_DM_OBJECT");
        let unrelated_dependency_uri =
            dependency_uri_for_object_name(&state, &workspace_uri, "ZCL_UNRELATED");

        let opened_dependency = handle_message(
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
        .expect("didOpen dependency");
        assert!(
            opened_dependency
                .notifications
                .iter()
                .all(|(method, _)| method != RESOLVE_REMOTE_DEPENDENCIES),
            "dependency open should stay queued behind the in-flight wave: {:#?}",
            opened_dependency.notifications
        );

        let refreshed = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": REMOTE_DEPENDENCIES_UPDATED,
                "params": {
                    "workspaceUri": workspace_uri,
                    "sourceUri": source_uri,
                    "sourceUris": [source_uri],
                    "fetched": ["zattp_cl_ar_dm_object"],
                    "failed": []
                }
            }),
        )
        .expect("remote dependencies updated");

        let follow_up_request = refreshed
            .notifications
            .iter()
            .find(|(method, _)| method == RESOLVE_REMOTE_DEPENDENCIES)
            .map(|(_, payload)| payload)
            .expect("follow-up remote dependency request");
        assert_eq!(
            follow_up_request
                .get("retryNegativeCandidates")
                .and_then(Value::as_bool),
            Some(true),
            "{follow_up_request:#?}"
        );
        let source_uris = follow_up_request
            .get("sourceUris")
            .and_then(Value::as_array)
            .expect("source uris");
        assert!(
            source_uris
                .iter()
                .filter_map(Value::as_str)
                .any(|uri| uri == dependency_uri),
            "unexpected source uris: {source_uris:?}"
        );
        assert!(
            source_uris
                .iter()
                .filter_map(Value::as_str)
                .all(|uri| uri != unrelated_dependency_uri),
            "unexpected source uris: {source_uris:?}"
        );
        let follow_up_candidates = follow_up_request
            .get("candidates")
            .and_then(Value::as_array)
            .expect("follow-up candidates");
        assert!(follow_up_candidates.iter().any(|candidate| {
            candidate.get("kind").and_then(Value::as_str) == Some("type")
                && candidate.get("name").and_then(Value::as_str) == Some("zattp_t_param_value")
        }));
        assert!(follow_up_candidates.iter().any(|candidate| {
            candidate.get("kind").and_then(Value::as_str) == Some("type")
                && candidate.get("name").and_then(Value::as_str) == Some("rsds_frange_t")
        }));
        assert!(!follow_up_candidates.iter().any(|candidate| {
            candidate.get("name").and_then(Value::as_str) == Some("zcl_noise")
        }));
    }

    #[test]
    fn full_workspace_reopening_source_after_dependency_definition_keeps_range_argument_clean() {
        let workspace_path = temp_workspace_path("full_workspace_reopen_after_dependency");
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
name = "ZATTP_AR_DM_OBJ_PRE"
kind = "report"
root_file = "src/ZATTP_AR_DM_OBJ_PRE.abap"

[[unit.member]]
role = "root"
file = "src/ZATTP_AR_DM_OBJ_PRE.abap"
object_name = "ZATTP_AR_DM_OBJ_PRE"
"#,
        )
        .expect("manifest");
        let source_text = "\
REPORT zattp_ar_dm_obj_pre.
DATA lv_rogln TYPE /sttp/e_gs1_gln.
SELECT-OPTIONS s_rogln FOR lv_rogln.

START-OF-SELECTION.
  zattp_cl_ar_dm_object=>main_processing_pre_step(
    EXPORTING
      lt_rogln = s_rogln[] ).
";
        fs::write(source_dir.join("ZATTP_AR_DM_OBJ_PRE.abap"), source_text).expect("source");

        let workspace_uri = file_uri(&workspace_path);
        let source_uri =
            normalize_lsp_uri(&format!("{workspace_uri}/src/ZATTP_AR_DM_OBJ_PRE.abap"));
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        store_dependency_artifacts(
            &mut state,
            &workspace_uri,
            vec![DependencyArtifactPayload {
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
    CLASS-METHODS main_processing_pre_step
      IMPORTING lt_rogln TYPE /sttp/t_rng_gln.
ENDCLASS.
CLASS zattp_cl_ar_dm_object IMPLEMENTATION.
  METHOD main_processing_pre_step.
  ENDMETHOD.
ENDCLASS."
                    .to_string(),
                fetched_at: "2026-04-23T00:00:00Z".to_string(),
            }],
        );
        let range_type_artifacts = vec![
            DependencyArtifactPayload {
                package_name: "/STTP/CORE".to_string(),
                object_kind: "ddic-table-type".to_string(),
                object_name: "/STTP/T_RNG_GLN".to_string(),
                object_uri: "/sap/bc/adt/ddic/tabletypes/%2FSTTP%2FT_RNG_GLN".to_string(),
                object_type: "TTYP/DA".to_string(),
                description: "GLN range table".to_string(),
                file_extension: "abap".to_string(),
                source_text: "TYPES /sttp/t_rng_gln TYPE STANDARD TABLE OF string WITH EMPTY KEY."
                    .to_string(),
                fetched_at: "2026-04-23T00:00:00Z".to_string(),
            },
            DependencyArtifactPayload {
                package_name: "/STTP/CORE".to_string(),
                object_kind: "ddic-data-element".to_string(),
                object_name: "/STTP/E_GS1_GLN".to_string(),
                object_uri: "/sap/bc/adt/ddic/dataelements/%2FSTTP%2FE_GS1_GLN".to_string(),
                object_type: "DTEL/DE".to_string(),
                description: "GLN".to_string(),
                file_extension: "abap".to_string(),
                source_text: "TYPES /sttp/e_gs1_gln TYPE c LENGTH 18.".to_string(),
                fetched_at: "2026-04-23T00:00:00Z".to_string(),
            },
        ];

        let opened_source = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": source_uri,
                        "languageId": "abap",
                        "version": 1,
                        "text": source_text
                    }
                }
            }),
        )
        .expect("didOpen source");
        assert_source_has_no_lt_rogln_argument_error(&opened_source.notifications, &source_uri);

        let definition_offset = source_text
            .find("zattp_cl_ar_dm_object")
            .expect("class reference");
        let definition_msg = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "textDocument/definition",
                "params": {
                    "textDocument": { "uri": source_uri },
                    "position": lsp_position_for_offset(source_text, definition_offset)
                }
            }),
        )
        .expect("definition");
        let dependency_uri = definition_msg
            .response
            .as_ref()
            .and_then(|response| response.result.as_ref())
            .and_then(|result| result.get("uri"))
            .and_then(Value::as_str)
            .expect("dependency uri")
            .to_string();
        let dependency_text =
            dependency_text_for_object_name(&state, &workspace_uri, "ZATTP_CL_AR_DM_OBJECT");

        handle_message(
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
        .expect("didOpen dependency");

        store_dependency_artifacts(&mut state, &workspace_uri, range_type_artifacts);
        let refreshed = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": REMOTE_DEPENDENCIES_UPDATED,
                "params": {
                    "workspaceUri": workspace_uri,
                    "sourceUri": dependency_uri,
                    "sourceUris": [dependency_uri, source_uri],
                    "fetched": ["/sttp/t_rng_gln", "/sttp/e_gs1_gln"],
                    "failed": []
                }
            }),
        )
        .expect("remote dependencies updated");
        assert_source_has_no_lt_rogln_argument_error(&refreshed.notifications, &source_uri);

        let reopened_source = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": source_uri,
                        "languageId": "abap",
                        "version": 2,
                        "text": source_text
                    }
                }
            }),
        )
        .expect("reopen source");
        assert_source_has_no_lt_rogln_argument_error(&reopened_source.notifications, &source_uri);
    }

    #[test]
    fn full_workspace_dependency_only_update_reemits_dirty_root_diagnostics() {
        let workspace_path = temp_workspace_path("full_workspace_dependency_dirty_root_diags");
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
        let source_text = "REPORT zreport_main.\nDATA lo_remote TYPE REF TO zcl_remote.\n";
        fs::write(source_dir.join("ZREPORT_MAIN.abap"), source_text).expect("source");

        let workspace_uri = file_uri(&workspace_path);
        let source_uri = normalize_lsp_uri(&format!("{workspace_uri}/src/ZREPORT_MAIN.abap"));
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());

        let opened = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": source_uri,
                        "languageId": "abap",
                        "version": 1,
                        "text": source_text
                    }
                }
            }),
        )
        .expect("didOpen source");
        let initial_source_diags = opened
            .notifications
            .iter()
            .find(|(method, payload)| {
                method == "textDocument/publishDiagnostics"
                    && payload.get("uri").and_then(Value::as_str) == Some(source_uri.as_str())
            })
            .and_then(|(_, payload)| payload.get("diagnostics"))
            .and_then(Value::as_array)
            .expect("initial source diagnostics");
        assert!(
            initial_source_diags.iter().any(|diag| {
                diag.get("message")
                    .and_then(Value::as_str)
                    .is_some_and(|message| message.contains("zcl_remote"))
            }),
            "expected initial unresolved remote diagnostic: {initial_source_diags:#?}"
        );

        store_dependency_artifacts(
            &mut state,
            &workspace_uri,
            vec![DependencyArtifactPayload {
                package_name: "ZPKG".to_string(),
                object_kind: "global-class".to_string(),
                object_name: "ZCL_REMOTE".to_string(),
                object_uri: "/sap/bc/adt/oo/classes/zcl_remote".to_string(),
                object_type: "CLAS/OC".to_string(),
                description: "Fetched dependency".to_string(),
                file_extension: "abap".to_string(),
                source_text: "\
CLASS zcl_remote DEFINITION PUBLIC FINAL CREATE PUBLIC.
ENDCLASS.
CLASS zcl_remote IMPLEMENTATION.
ENDCLASS."
                    .to_string(),
                fetched_at: "2026-04-23T00:00:00Z".to_string(),
            }],
        );
        let dependency_uri = dependency_uri_for_object_name(&state, &workspace_uri, "ZCL_REMOTE");

        let refreshed = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": REMOTE_DEPENDENCIES_UPDATED,
                "params": {
                    "workspaceUri": workspace_uri,
                    "sourceUri": dependency_uri,
                    "sourceUris": [dependency_uri],
                    "fetched": ["zcl_remote"],
                    "failed": []
                }
            }),
        )
        .expect("remote dependencies updated");

        let refreshed_source_diags = refreshed
            .notifications
            .iter()
            .find(|(method, payload)| {
                method == "textDocument/publishDiagnostics"
                    && payload.get("uri").and_then(Value::as_str) == Some(source_uri.as_str())
            })
            .and_then(|(_, payload)| payload.get("diagnostics"))
            .and_then(Value::as_array)
            .expect("refreshed source diagnostics");
        assert!(
            refreshed_source_diags.iter().all(|diag| {
                !diag
                    .get("message")
                    .and_then(Value::as_str)
                    .is_some_and(|message| message.contains("zcl_remote"))
            }),
            "unexpected stale source diagnostics: {refreshed_source_diags:#?}"
        );
    }

    #[test]
    fn full_workspace_opened_dependency_failed_follow_up_candidates_do_not_repeat() {
        let workspace_path = temp_workspace_path("full_workspace_open_dependency_no_retry_loop");
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
"#,
        )
        .expect("manifest");
        let source_text =
            "REPORT zattp_ar_dm_obj_pre.\nDATA lo_dep TYPE REF TO zattp_cl_ar_dm_object.\n";
        fs::write(source_dir.join("ZATTP_AR_DM_OBJ_PRE.abap"), source_text).expect("source");

        let workspace_uri = file_uri(&workspace_path);
        let source_uri =
            normalize_lsp_uri(&format!("{workspace_uri}/src/ZATTP_AR_DM_OBJ_PRE.abap"));
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());

        let _ = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": source_uri,
                        "languageId": "abap",
                        "version": 1,
                        "text": source_text
                    }
                }
            }),
        )
        .expect("didOpen source");

        store_dependency_artifacts(
            &mut state,
            &workspace_uri,
            vec![DependencyArtifactPayload {
                package_name: "ZPKG".to_string(),
                object_kind: "global-class".to_string(),
                object_name: "ZATTP_CL_AR_DM_OBJECT".to_string(),
                object_uri: "/sap/bc/adt/oo/classes/zattp_cl_ar_dm_object".to_string(),
                object_type: "CLAS/OC".to_string(),
                description: "Fetched dependency".to_string(),
                file_extension: "abap".to_string(),
                source_text: "\
CLASS zattp_cl_ar_dm_object DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS zattp_cl_ar_dm_object IMPLEMENTATION.
  METHOD run.
    DATA lt_params TYPE zattp_t_param_value.
    DATA lt_ranges TYPE rsds_frange_t.
  ENDMETHOD.
ENDCLASS."
                    .to_string(),
                fetched_at: "2026-04-23T00:00:00Z".to_string(),
            }],
        );
        let dependency_uri =
            dependency_uri_for_object_name(&state, &workspace_uri, "ZATTP_CL_AR_DM_OBJECT");
        let dependency_text =
            dependency_text_for_object_name(&state, &workspace_uri, "ZATTP_CL_AR_DM_OBJECT");

        let _ = handle_message(
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
        .expect("didOpen dependency");

        let refreshed = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": REMOTE_DEPENDENCIES_UPDATED,
                "params": {
                    "workspaceUri": workspace_uri,
                    "sourceUri": source_uri,
                    "sourceUris": [source_uri],
                    "fetched": ["zattp_cl_ar_dm_object"],
                    "failed": []
                }
            }),
        )
        .expect("remote dependencies updated");
        let follow_up_request = refreshed
            .notifications
            .iter()
            .find(|(method, _)| method == RESOLVE_REMOTE_DEPENDENCIES)
            .map(|(_, payload)| payload)
            .expect("follow-up remote dependency request");
        let follow_up_candidates = follow_up_request
            .get("candidates")
            .and_then(Value::as_array)
            .expect("follow-up candidates");
        assert!(follow_up_candidates.iter().any(|candidate| {
            candidate.get("name").and_then(Value::as_str) == Some("zattp_t_param_value")
        }));
        assert!(follow_up_candidates.iter().any(|candidate| {
            candidate.get("name").and_then(Value::as_str) == Some("rsds_frange_t")
        }));

        let failed_update = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": REMOTE_DEPENDENCIES_UPDATED,
                "params": {
                    "workspaceUri": workspace_uri,
                    "sourceUri": dependency_uri,
                    "sourceUris": [dependency_uri],
                    "fetched": [],
                    "failed": [
                        { "name": "zattp_t_param_value", "kind": "type" },
                        { "name": "rsds_frange_t", "kind": "type" }
                    ]
                }
            }),
        )
        .expect("failed dependency update");
        assert!(
            failed_update
                .notifications
                .iter()
                .all(|(method, _)| method != RESOLVE_REMOTE_DEPENDENCIES),
            "failed candidates should not be re-requested immediately: {:#?}",
            failed_update.notifications
        );
    }

    #[test]
    fn editor_first_remote_dependency_updates_include_newly_fetched_dependency_files() {
        let workspace_path = temp_workspace_path("editor_first_dependency_follow_up_main_source");
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
mode = "editor-first"

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
"#,
        )
        .expect("manifest");
        fs::write(
            source_dir.join("ZREPORT_MAIN.abap"),
            "REPORT zreport_main.\nDATA lo_msg TYPE REF TO /sttp/cl_messages.\n",
        )
        .expect("main");

        let workspace_uri = file_uri(&workspace_path);
        let source_uri = format!("{workspace_uri}/src/ZREPORT_MAIN.abap");
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        configure_test_dependency_store(&mut state, &workspace_path);
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);
        store_dependency_artifacts(
            &mut state,
            &workspace_uri,
            vec![DependencyArtifactPayload {
                package_name: "/STTP/CORE".to_string(),
                object_kind: "global-class".to_string(),
                object_name: "/STTP/CL_MESSAGES".to_string(),
                object_uri: "/sap/bc/adt/oo/classes/%2FSTTP%2FCL_MESSAGES".to_string(),
                object_type: "CLAS/OC".to_string(),
                description: "Remote class".to_string(),
                file_extension: "abap".to_string(),
                source_text: "\
CLASS /STTP/CL_MESSAGES DEFINITION
  PUBLIC
  INHERITING FROM /CDBASIS/CL_MESSAGES
  CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES ts_bal_msg TYPE BAL_S_MSG.
    CONSTANTS:
      BEGIN OF gcs_log_level,
        very_high TYPE te_loglevel VALUE 1,
      END OF gcs_log_level.
ENDCLASS.
CLASS /STTP/CL_MESSAGES IMPLEMENTATION.
ENDCLASS."
                    .to_string(),
                fetched_at: "2026-04-23T00:00:00Z".to_string(),
            }],
        );
        let dependency_uri =
            dependency_uri_for_object_name(&state, &workspace_uri, "/STTP/CL_MESSAGES");

        let handled = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": REMOTE_DEPENDENCIES_UPDATED,
                "params": {
                    "workspaceUri": workspace_uri,
                    "sourceUri": source_uri,
                    "sourceUris": [source_uri],
                    "fetched": ["/sttp/cl_messages"],
                    "failed": []
                }
            }),
        )
        .expect("remote dependencies updated");

        let request = handled
            .notifications
            .iter()
            .find(|(method, _)| method == RESOLVE_REMOTE_DEPENDENCIES)
            .map(|(_, payload)| payload)
            .expect("follow-up remote dependency request");
        let source_uris = request
            .get("sourceUris")
            .and_then(Value::as_array)
            .expect("source uris");
        assert!(
            source_uris
                .iter()
                .filter_map(Value::as_str)
                .any(|uri| uri == dependency_uri),
            "unexpected source uris: {source_uris:?}"
        );
        let candidates = request
            .get("candidates")
            .and_then(Value::as_array)
            .expect("candidates");
        assert!(candidates.iter().any(|candidate| {
            candidate.get("kind").and_then(Value::as_str) == Some("type")
                && candidate.get("name").and_then(Value::as_str) == Some("/cdbasis/cl_messages")
        }));
        assert!(candidates.iter().any(|candidate| {
            candidate.get("kind").and_then(Value::as_str) == Some("type")
                && candidate.get("name").and_then(Value::as_str) == Some("bal_s_msg")
        }));
        assert!(candidates.iter().any(|candidate| {
            candidate.get("kind").and_then(Value::as_str) == Some("type")
                && candidate.get("name").and_then(Value::as_str) == Some("te_loglevel")
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
            None,
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
    fn initialized_skips_eager_background_analysis_for_editor_first_workspace() {
        let workspace_path = temp_workspace_path("workspace_initialized_editor_first_skip");
        let source_dir = workspace_path.join("src");
        fs::create_dir_all(&source_dir).expect("source dir");
        write_manifest_workspace(
            &workspace_path,
            Some("editor-first"),
            None,
            &[("ZREPORT_INIT", "report", "root", "src/ZREPORT_INIT.abap")],
            0,
        );
        fs::write(source_dir.join("ZREPORT_INIT.abap"), "REPORT zreport_init.").expect("report");

        let workspace_uri = file_uri(&workspace_path);
        let mut state = ServerState::default();
        state.register_workspace_folder(workspace_uri.clone());
        let normalized_workspace_uri = abap_lsp::normalize_lsp_uri(&workspace_uri);
        assert_eq!(
            state
                .workspaces
                .get(&normalized_workspace_uri)
                .expect("workspace")
                .performance_mode,
            WorkspacePerformanceMode::EditorFirst
        );

        let generations = Arc::new(Mutex::new(HashMap::new()));
        let queue_state = Arc::new(Mutex::new(PendingAnalysisQueue::default()));
        let mut debounced_tasks = HashMap::new();
        let (task_tx, task_rx) = mpsc::sync_channel(1);
        let scheduled = try_schedule_background_analysis(
            &mut state,
            &json!({
                "jsonrpc": "2.0",
                "method": "initialized",
                "params": {}
            }),
            &task_tx,
            &queue_state,
            &generations,
            &mut debounced_tasks,
        )
        .expect("schedule initialized")
        .expect("scheduled work");

        assert!(scheduled.started_statuses.is_empty());
        assert!(scheduled.notifications.is_empty());
        assert!(
            queue_state
                .lock()
                .expect("pending analysis queue")
                .pending_tasks
                .is_empty()
        );
        assert!(task_rx.try_recv().is_err());

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
        state.register_workspace_folder(workspace_uri);
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
    fn handles_sap_atc_results_updated_notification() {
        let workspace_path = temp_workspace_path("sap_atc_results_notification");
        let source_dir = workspace_path.join("src/reports/ZMAIN");
        fs::create_dir_all(&source_dir).expect("source dir");
        fs::write(
            workspace_path.join("abapls.toml"),
            r#"
version = 1

[lints.sap_atc]
mode = "manual"
check_variant = "DEFAULT"

[[unit]]
name = "ZMAIN"
kind = "report"
root_file = "src/reports/ZMAIN/ZMAIN.abap"
"#,
        )
        .expect("manifest");
        let source_path = source_dir.join("ZMAIN.abap");
        fs::write(&source_path, "REPORT zmain.\nWRITE 'x'.\n").expect("source");

        let workspace_uri = file_uri(&workspace_path);
        let source_uri = file_uri(&source_path);
        let mut state = ServerState::default();
        let config = ServerConfig::default();
        state.register_workspace_folder(workspace_uri.clone());
        refresh_workspace(&mut state, &workspace_uri);

        let handled = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": SAP_ATC_RESULTS_UPDATED,
                "params": {
                    "workspaceUri": workspace_uri,
                    "sourceUri": source_uri,
                    "documentVersion": 0,
                    "objectName": "ZMAIN",
                    "checkVariant": "DEFAULT",
                    "fetchedAt": "2026-04-27T00:00:00Z",
                    "findings": [{
                        "sapCheckId": "ZCHECK",
                        "sapMessageId": "ZMSG",
                        "message": "Remote ATC finding",
                        "severity": "error",
                        "location": {
                            "uri": file_uri(&source_path),
                            "objectName": "ZMAIN",
                            "includeName": "ZMAIN",
                            "startLine": 2,
                            "startColumn": 1
                        }
                    }]
                }
            }),
        )
        .expect("sap atc update");

        assert!(handled.response.is_none());
        let diagnostics = handled
            .notifications
            .iter()
            .find(|(method, _)| method == "textDocument/publishDiagnostics")
            .and_then(|(_, payload)| payload.get("diagnostics"))
            .and_then(Value::as_array)
            .expect("diagnostics");
        assert!(diagnostics.iter().any(|diagnostic| {
            diagnostic.get("source").and_then(Value::as_str) == Some("sap-atc")
                && diagnostic.get("message").and_then(Value::as_str) == Some("Remote ATC finding")
                && diagnostic.get("code").and_then(Value::as_str) == Some("sap-atc:zcheck/zmsg")
        }));

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
    fn server_message_definition_resolves_namespaced_cached_static_method_after_open() {
        let workspace_path = temp_workspace_path("message_namespaced_cached_definition");
        let source_dir = workspace_path.join("src/reports/ZMAIN");
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
"#,
        )
        .expect("manifest");
        let source = "\
REPORT zmain.
START-OF-SELECTION.
  /sttp/cl_rr_ru_utilities=>get_safedata_key( ).
";
        let source_path = source_dir.join("ZMAIN.abap");
        fs::write(&source_path, source).expect("source");

        let workspace_uri = file_uri(&workspace_path);
        let source_uri = file_uri(&source_path);
        let store_path = workspace_path
            .join("dependency-store")
            .join("dependency-cache.sqlite3");
        let position = lsp_position_for_offset(
            source,
            source.find("get_safedata_key").expect("method offset") + 1,
        );
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
                    "workspaceFolders": [{ "uri": workspace_uri, "name": "workspace" }],
                    "initializationOptions": {
                        "dependencyCachePath": store_path.to_string_lossy()
                    },
                    "capabilities": {}
                }
            }),
        )
        .expect("initialize");

        handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "id": 2,
                "method": "abapls/storeRemoteDependencyArtifacts",
                "params": {
                    "workspaceUri": workspace_uri,
                    "connectionKey": "https://example.sap.local",
                    "artifacts": [{
                        "packageName": "/STTP/RU",
                        "objectKind": "global-class",
                        "objectName": "/STTP/CL_RR_RU_UTILITIES",
                        "objectUri": "/sap/bc/adt/oo/classes/%2FSTTP%2FCL_RR_RU_UTILITIES",
                        "objectType": "CLAS/OC",
                        "description": "Remote namespaced class",
                        "fileExtension": "abap",
                        "sourceText": "CLASS /sttp/cl_rr_ru_utilities DEFINITION PUBLIC FINAL CREATE PUBLIC.\n  PUBLIC SECTION.\n    CLASS-METHODS get_safedata_key.\nENDCLASS.\n",
                        "fetchedAt": "2026-04-23T00:00:00Z"
                    }],
                    "negative": []
                }
            }),
        )
        .expect("store dependency");

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

        handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": source_uri,
                        "languageId": "abap",
                        "version": 1,
                        "text": source
                    }
                }
            }),
        )
        .expect("didOpen");

        let definition_msg = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "id": 3,
                "method": "textDocument/definition",
                "params": {
                    "textDocument": { "uri": source_uri },
                    "position": position
                }
            }),
        )
        .expect("definition");
        let definition = definition_msg
            .response
            .expect("definition response")
            .result
            .expect("definition result");
        let uri = definition
            .get("uri")
            .and_then(Value::as_str)
            .expect("definition uri");
        assert!(
            uri.starts_with("abapls-cache:///sttp_cl_rr_ru_utilities.abap?"),
            "{uri}"
        );
        assert!(
            !uri.split('?')
                .next()
                .unwrap_or(uri)
                .to_ascii_lowercase()
                .contains("%2f"),
            "{uri}"
        );

        let _ = fs::remove_dir_all(&workspace_path);
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
    fn handles_prepare_rename_and_rename_after_open_document() {
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
                        "uri": "file:///rename.abap",
                        "languageId": "abap",
                        "version": 1,
                        "text": "CLASS lcl_demo DEFINITION.\n  PUBLIC SECTION.\n    METHODS run.\nENDCLASS.\n\nCLASS lcl_demo IMPLEMENTATION.\n  METHOD run.\n    run( ).\n  ENDMETHOD.\nENDCLASS."
                    }
                }
            }),
        )
        .expect("didOpen");
        assert!(opened.response.is_none());

        let prepare_msg = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "id": 2,
                "method": "textDocument/prepareRename",
                "params": {
                    "textDocument": { "uri": "file:///rename.abap" },
                    "position": { "line": 7, "character": 5 }
                }
            }),
        )
        .expect("prepareRename");

        let prepare_result = prepare_msg
            .response
            .expect("prepareRename response")
            .result
            .expect("prepareRename result");
        assert_eq!(
            prepare_result
                .get("placeholder")
                .and_then(|value| value.as_str()),
            Some("run")
        );

        let rename_msg = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "id": 3,
                "method": "textDocument/rename",
                "params": {
                    "textDocument": { "uri": "file:///rename.abap" },
                    "position": { "line": 7, "character": 5 },
                    "newName": "execute"
                }
            }),
        )
        .expect("rename");

        let result = rename_msg
            .response
            .expect("rename response")
            .result
            .expect("rename result");
        let edits = result
            .get("changes")
            .and_then(|changes| changes.get("file:///rename.abap"))
            .and_then(|value| value.as_array())
            .expect("rename edits");
        assert_eq!(edits.len(), 3);
        assert!(edits.iter().all(|edit| {
            edit.get("newText")
                .and_then(|value| value.as_str())
                .is_some_and(|text| text == "execute")
        }));
    }

    #[test]
    fn rename_returns_invalid_params_for_bad_new_name() {
        let mut state = ServerState::default();
        let config = ServerConfig::default();

        handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "method": "textDocument/didOpen",
                "params": {
                    "textDocument": {
                        "uri": "file:///field_symbol.abap",
                        "languageId": "abap",
                        "version": 1,
                        "text": "FIELD-SYMBOLS <fs> TYPE any.\nASSIGN 1 TO <fs>."
                    }
                }
            }),
        )
        .expect("didOpen");

        let rename_msg = handle_message(
            &mut state,
            &config,
            json!({
                "jsonrpc": "2.0",
                "id": 4,
                "method": "textDocument/rename",
                "params": {
                    "textDocument": { "uri": "file:///field_symbol.abap" },
                    "position": { "line": 1, "character": 13 },
                    "newName": "fs2"
                }
            }),
        )
        .expect("rename");

        let response = rename_msg.response.expect("rename response");
        let error = response.error.expect("rename error");
        assert_eq!(error.code, crate::INVALID_PARAMS);
        assert!(
            error.message.contains("angle brackets"),
            "{}",
            error.message
        );
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
