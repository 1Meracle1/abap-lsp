use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::str::FromStr;
use std::sync::Arc;
use std::time::{Duration, Instant};

use abap_cache::{AnalysisSnapshot, WorkspaceAnalysisMetricsSnapshot, path_to_file_uri};
use abap_lsp::{
    CompletionParams, DidChangeTextDocumentParams, DidOpenTextDocumentParams, GotoDefinitionParams,
    HoverParams, ServerState, TextDocumentPositionParams, completion, definition, hover,
    normalize_lsp_uri, publish_changed_document_mut, publish_open_document_mut, refresh_workspace,
};
use lsp_types::{
    CompletionResponse, Position, TextDocumentContentChangeEvent, TextDocumentIdentifier,
    TextDocumentItem, Uri, VersionedTextDocumentIdentifier,
};

struct Config {
    workspace_root: PathBuf,
    target_file: PathBuf,
    dependency_store_path: Option<PathBuf>,
    symbol: String,
}

#[derive(Default)]
struct StoreCapture {
    revision: u64,
    dependency_snapshots: HashMap<String, Arc<AnalysisSnapshot>>,
}

struct OperationRecord {
    label: &'static str,
    elapsed: Duration,
    analysis_ran: bool,
    metrics: Option<WorkspaceAnalysisMetricsSnapshot>,
    snapshot_count: usize,
    dependency_snapshot_count: usize,
    dependency_snapshot_reuse_count: usize,
    dependency_snapshots_reused: bool,
    result: Option<String>,
}

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(message) => {
            eprintln!("{message}");
            ExitCode::FAILURE
        }
    }
}

fn run() -> Result<(), String> {
    let config = parse_args(std::env::args().skip(1))?;
    let workspace_uri = normalize_lsp_uri(&path_to_file_uri(&config.workspace_root));
    let target_uri = normalize_lsp_uri(&path_to_file_uri(&config.target_file));
    let source_text = fs::read_to_string(&config.target_file)
        .map_err(|err| format!("failed to read '{}': {err}", config.target_file.display()))?;
    let target_lsp_uri =
        Uri::from_str(&target_uri).map_err(|err| format!("invalid target uri: {err}"))?;

    let mut state = ServerState::default();
    state.dependency_store_path_override = config.dependency_store_path.clone();
    let before_initial = StoreCapture::default();
    let initial_start = Instant::now();
    state.register_workspace_folder(workspace_uri.clone());
    let initial_snapshots = refresh_workspace(&mut state, &workspace_uri);
    let initial_elapsed = initial_start.elapsed();
    let initial_record = finish_record(
        "initial_rebuild",
        before_initial,
        initial_elapsed,
        &state,
        &workspace_uri,
        Some(format!("refreshed_snapshots={}", initial_snapshots.len())),
    )?;

    let open_record = measure(&mut state, &workspace_uri, "open_document", |state| {
        let snapshot = publish_open_document_mut(
            state,
            &DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: target_lsp_uri.clone(),
                    language_id: "abap".to_string(),
                    version: 2,
                    text: source_text.clone(),
                },
            },
        );
        format!("snapshot_version={}", snapshot.version)
    })?;

    let noop_record = measure(&mut state, &workspace_uri, "noop_version_bump", |state| {
        let snapshot = publish_changed_document_mut(
            state,
            &DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: target_lsp_uri.clone(),
                    version: 3,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: source_text.clone(),
                }],
            },
        )
        .expect("change should produce a snapshot");
        format!("snapshot_version={}", snapshot.version)
    })?;

    let tiny_text = non_signature_edit(&source_text);
    let tiny_record = measure(
        &mut state,
        &workspace_uri,
        "tiny_non_signature_edit",
        |state| {
            let snapshot = publish_changed_document_mut(
                state,
                &DidChangeTextDocumentParams {
                    text_document: VersionedTextDocumentIdentifier {
                        uri: target_lsp_uri.clone(),
                        version: 4,
                    },
                    content_changes: vec![TextDocumentContentChangeEvent {
                        range: None,
                        range_length: None,
                        text: tiny_text.clone(),
                    }],
                },
            )
            .expect("change should produce a snapshot");
            format!("snapshot_version={}", snapshot.version)
        },
    )?;

    let signature_text = signature_edit(&tiny_text);
    let signature_record = measure(
        &mut state,
        &workspace_uri,
        "signature_affecting_edit",
        |state| {
            let snapshot = publish_changed_document_mut(
                state,
                &DidChangeTextDocumentParams {
                    text_document: VersionedTextDocumentIdentifier {
                        uri: target_lsp_uri.clone(),
                        version: 5,
                    },
                    content_changes: vec![TextDocumentContentChangeEvent {
                        range: None,
                        range_length: None,
                        text: signature_text.clone(),
                    }],
                },
            )
            .expect("change should produce a snapshot");
            format!("snapshot_version={}", snapshot.version)
        },
    )?;

    let symbol_position = symbol_position(&signature_text, &config.symbol).ok_or_else(|| {
        format!(
            "symbol '{}' not found in '{}'",
            config.symbol,
            config.target_file.display()
        )
    })?;
    let definition_record = measure(&mut state, &workspace_uri, "definition_request", |state| {
        let found = definition(
            state,
            &GotoDefinitionParams {
                text_document_position_params: text_document_position(
                    target_lsp_uri.clone(),
                    symbol_position,
                ),
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        )
        .is_some();
        format!("definition_found={found}")
    })?;
    let hover_record = measure(&mut state, &workspace_uri, "hover_request", |state| {
        let found = hover(
            state,
            &HoverParams {
                text_document_position_params: text_document_position(
                    target_lsp_uri.clone(),
                    symbol_position,
                ),
                work_done_progress_params: Default::default(),
            },
        )
        .is_some();
        format!("hover_found={found}")
    })?;
    let completion_position = completion_position(&signature_text).unwrap_or(symbol_position);
    let completion_record = measure(&mut state, &workspace_uri, "completion_request", |state| {
        let count = completion(
            state,
            &CompletionParams {
                text_document_position: text_document_position(
                    target_lsp_uri.clone(),
                    completion_position,
                ),
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
                context: Default::default(),
            },
        )
        .map(completion_item_count)
        .unwrap_or(0);
        format!("completion_items={count}")
    })?;

    println!("workspace_root={}", config.workspace_root.display());
    println!("target_file={}", config.target_file.display());
    println!("workspace_uri={workspace_uri}");
    println!("target_uri={target_uri}");
    println!(
        "dependency_store_path={}",
        config
            .dependency_store_path
            .as_ref()
            .map(|path| path.display().to_string())
            .unwrap_or_else(|| "<default>".to_string())
    );
    println!("query_symbol={}", config.symbol);
    for record in [
        initial_record,
        open_record,
        noop_record,
        tiny_record,
        signature_record,
        definition_record,
        hover_record,
        completion_record,
    ] {
        print_record(&record);
    }

    Ok(())
}

fn measure(
    state: &mut ServerState,
    workspace_uri: &str,
    label: &'static str,
    operation: impl FnOnce(&mut ServerState) -> String,
) -> Result<OperationRecord, String> {
    let before = capture_store(state, workspace_uri);
    let start = Instant::now();
    let result = operation(state);
    let elapsed = start.elapsed();
    finish_record(label, before, elapsed, state, workspace_uri, Some(result))
}

fn capture_store(state: &ServerState, workspace_uri: &str) -> StoreCapture {
    let Some(workspace) = state.workspaces.get(&normalize_lsp_uri(workspace_uri)) else {
        return StoreCapture::default();
    };
    let dependency_snapshots = workspace
        .cache
        .uris()
        .into_iter()
        .filter_map(|uri| {
            let snapshot = workspace.cache.get(uri.as_ref())?;
            snapshot
                .is_dependency
                .then(|| (uri.to_string(), Arc::clone(&snapshot)))
        })
        .collect();
    StoreCapture {
        revision: workspace.cache.last_analysis_revision(),
        dependency_snapshots,
    }
}

fn finish_record(
    label: &'static str,
    before: StoreCapture,
    elapsed: Duration,
    state: &ServerState,
    workspace_uri: &str,
    result: Option<String>,
) -> Result<OperationRecord, String> {
    let workspace = state
        .workspaces
        .get(&normalize_lsp_uri(workspace_uri))
        .ok_or_else(|| format!("workspace missing: {workspace_uri}"))?;
    let after_revision = workspace.cache.last_analysis_revision();
    let analysis_ran = after_revision != before.revision;
    let metrics = analysis_ran
        .then(|| workspace.cache.last_analysis_metrics_snapshot())
        .flatten();
    let mut dependency_snapshot_count = 0usize;
    let mut dependency_snapshot_reuse_count = 0usize;
    for uri in workspace.cache.uris() {
        let Some(snapshot) = workspace.cache.get(uri.as_ref()) else {
            continue;
        };
        if !snapshot.is_dependency {
            continue;
        }
        dependency_snapshot_count += 1;
        if before
            .dependency_snapshots
            .get(uri.as_ref())
            .is_some_and(|before| Arc::ptr_eq(before, &snapshot))
        {
            dependency_snapshot_reuse_count += 1;
        }
    }
    let dependency_snapshots_reused = dependency_snapshot_count
        == before.dependency_snapshots.len()
        && dependency_snapshot_reuse_count == before.dependency_snapshots.len();
    Ok(OperationRecord {
        label,
        elapsed,
        analysis_ran,
        metrics,
        snapshot_count: workspace.cache.len(),
        dependency_snapshot_count,
        dependency_snapshot_reuse_count,
        dependency_snapshots_reused,
        result,
    })
}

fn print_record(record: &OperationRecord) {
    let metrics = record.metrics.clone().unwrap_or_default();
    println!("{}__wall={:?}", record.label, record.elapsed);
    println!("{}__analysis_ran={}", record.label, record.analysis_ran);
    println!("{}__parse_count={}", record.label, metrics.parse_count);
    println!(
        "{}__local_phase_count={}",
        record.label, metrics.local_phase_count
    );
    println!(
        "{}__dirty_uri_count={}",
        record.label, metrics.dirty_uri_count
    );
    println!("{}__full_rebuild={}", record.label, metrics.full_rebuild);
    println!("{}__unit_count={}", record.label, metrics.unit_count);
    println!(
        "{}__dirty_unit_count={}",
        record.label, metrics.dirty_unit_count
    );
    println!(
        "{}__validation_unit_count={}",
        record.label, metrics.validation_unit_count
    );
    println!(
        "{}__project_update={:?}",
        record.label,
        Duration::from_micros(metrics.project_update_micros as u64)
    );
    println!(
        "{}__validate={:?}",
        record.label,
        Duration::from_micros(metrics.validate_micros as u64)
    );
    println!(
        "{}__snapshot_build={:?}",
        record.label,
        Duration::from_micros(metrics.snapshot_build_micros as u64)
    );
    println!(
        "{}__build_workspace_index={:?}",
        record.label,
        Duration::from_micros(metrics.build_workspace_index_micros as u64)
    );
    println!(
        "{}__compute_dirty_set={:?}",
        record.label,
        Duration::from_micros(metrics.compute_dirty_set_micros as u64)
    );
    println!(
        "{}__clone_previous_units={:?}",
        record.label,
        Duration::from_micros(metrics.clone_previous_units_micros as u64)
    );
    println!(
        "{}__resolve_cross_unit={:?}",
        record.label,
        Duration::from_micros(metrics.resolve_cross_unit_micros as u64)
    );
    println!(
        "{}__infer_semantic_facts={:?}",
        record.label,
        Duration::from_micros(metrics.infer_semantic_facts_micros as u64)
    );
    println!(
        "{}__rebuild_semantic_index={:?}",
        record.label,
        Duration::from_micros(metrics.rebuild_semantic_index_micros as u64)
    );
    println!(
        "{}__collect_project_diagnostics={:?}",
        record.label,
        Duration::from_micros(metrics.collect_project_diagnostics_micros as u64)
    );
    println!(
        "{}__routine_analysis={:?}",
        record.label,
        Duration::from_micros(metrics.routine_analysis_micros as u64)
    );
    println!("{}__snapshots={}", record.label, record.snapshot_count);
    println!(
        "{}__dependency_snapshots={}",
        record.label, record.dependency_snapshot_count
    );
    println!(
        "{}__dependency_snapshot_reuse_count={}",
        record.label, record.dependency_snapshot_reuse_count
    );
    println!(
        "{}__dependency_snapshots_reused={}",
        record.label, record.dependency_snapshots_reused
    );
    if let Some(result) = &record.result {
        println!("{}__result={result}", record.label);
    }
}

fn text_document_position(uri: Uri, position: Position) -> TextDocumentPositionParams {
    TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri },
        position,
    }
}

fn completion_item_count(response: CompletionResponse) -> usize {
    match response {
        CompletionResponse::Array(items) => items.len(),
        CompletionResponse::List(list) => list.items.len(),
    }
}

fn non_signature_edit(text: &str) -> String {
    if text.contains("\" Main processing.") {
        return text.replace("\" Main processing.", "\" Main processing warm edit.");
    }
    let mut edited = text.to_string();
    edited.push_str("\n\" warm non-signature edit\n");
    edited
}

fn signature_edit(text: &str) -> String {
    if text
        .to_ascii_lowercase()
        .contains("form zperf_signature_probe")
    {
        return text.to_string();
    }
    format!("{text}\nFORM zperf_signature_probe.\nENDFORM.\n")
}

fn symbol_position(text: &str, symbol: &str) -> Option<Position> {
    let symbol = symbol.to_ascii_lowercase();
    for (line_index, line) in text.lines().enumerate() {
        if let Some(column) = line.to_ascii_lowercase().find(&symbol) {
            return Some(Position {
                line: line_index as u32,
                character: column as u32,
            });
        }
    }
    None
}

fn completion_position(text: &str) -> Option<Position> {
    for (line_index, line) in text.lines().enumerate() {
        if let Some(column) = line.to_ascii_lowercase().find("perform ") {
            return Some(Position {
                line: line_index as u32,
                character: (column + "perform ".len()) as u32,
            });
        }
    }
    None
}

fn parse_args(args: impl Iterator<Item = String>) -> Result<Config, String> {
    let mut workspace_root = None;
    let mut target_file = None;
    let mut dependency_store_path = None;
    let mut symbol = "main_processing".to_string();
    let mut args = args.peekable();

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--workspace" => {
                let value = args
                    .next()
                    .ok_or_else(|| "--workspace requires a directory path".to_string())?;
                workspace_root = Some(PathBuf::from(value));
            }
            "--target" => {
                let value = args
                    .next()
                    .ok_or_else(|| "--target requires a file path".to_string())?;
                target_file = Some(PathBuf::from(value));
            }
            "--dependency-store" => {
                let value = args
                    .next()
                    .ok_or_else(|| "--dependency-store requires a file path".to_string())?;
                dependency_store_path = Some(PathBuf::from(value));
            }
            "--symbol" => {
                symbol = args
                    .next()
                    .ok_or_else(|| "--symbol requires a string".to_string())?;
            }
            "--help" | "-h" => {
                print_usage();
                std::process::exit(0);
            }
            other => return Err(format!("unrecognized argument: {other}")),
        }
    }

    let workspace_root = workspace_root.ok_or_else(|| "--workspace is required".to_string())?;
    let target_file = target_file.ok_or_else(|| "--target is required".to_string())?;
    if !Path::new(&workspace_root).is_dir() {
        return Err(format!(
            "workspace root does not exist or is not a directory: '{}'",
            workspace_root.display()
        ));
    }
    if !Path::new(&target_file).is_file() {
        return Err(format!(
            "target file does not exist or is not a file: '{}'",
            target_file.display()
        ));
    }

    Ok(Config {
        workspace_root,
        target_file,
        dependency_store_path,
        symbol,
    })
}

fn print_usage() {
    println!("Usage: cargo run -p abap_lsp --example workspace_incremental_path_perf -- [options]");
    println!("Options:");
    println!("  --workspace <dir>   Workspace root");
    println!("  --target <file>     Target ABAP file");
    println!("  --dependency-store <db>   Override dependency store path");
    println!("  --symbol <name>     Symbol used for definition/hover timing");
    println!("  --help              Show this message");
}
