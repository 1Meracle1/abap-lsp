use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::str::FromStr;
use std::sync::Arc;
use std::time::Instant;

use abap_cache::path_to_file_uri;
use abap_lsp::{
    GotoDefinitionParams, ServerState, TextDocumentPositionParams,
    build_remote_dependency_batch_for_workspace,
    build_remote_dependency_batch_for_workspace_filtered, definition, normalize_lsp_uri,
    publish_open_document_mut,
};
use lsp_types::{
    DidOpenTextDocumentParams, Position, TextDocumentIdentifier, TextDocumentItem, Uri,
};

struct Config {
    workspace_root: PathBuf,
    source_file: PathBuf,
    dependency_store_path: Option<PathBuf>,
    symbol: String,
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
    let source_uri = normalize_lsp_uri(&path_to_file_uri(&config.source_file));
    let source_text = fs::read_to_string(&config.source_file)
        .map_err(|err| format!("failed to read '{}': {err}", config.source_file.display()))?;

    let mut state = ServerState::default();
    state.dependency_store_path_override = config.dependency_store_path.clone();
    let register_start = Instant::now();
    state.register_workspace_folder(workspace_uri.clone());
    let performance_mode = state
        .workspaces
        .get(&workspace_uri)
        .map(|workspace| workspace.performance_mode)
        .ok_or_else(|| format!("workspace not registered: {workspace_uri}"))?;
    let register_elapsed = register_start.elapsed();

    let open_start = Instant::now();
    let snapshot = publish_open_document_mut(
        &mut state,
        &DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: Uri::from_str(&source_uri)
                    .map_err(|err| format!("invalid source uri: {err}"))?,
                language_id: "abap".to_string(),
                version: 1,
                text: source_text.clone(),
            },
        },
    );
    let open_elapsed = open_start.elapsed();

    let first_batch_start = Instant::now();
    let first_batch = build_remote_dependency_batch_for_workspace(&mut state, &workspace_uri);
    let first_batch_elapsed = first_batch_start.elapsed();
    if let Some(workspace) = state.workspaces.get_mut(&workspace_uri) {
        workspace.remote_resolution_in_flight = false;
        workspace.remote_resolution_seen.clear();
    }
    let mut filtered_source_uris = HashSet::new();
    filtered_source_uris.insert(Arc::<str>::from(source_uri.as_str()));
    let filtered_batch_start = Instant::now();
    let filtered_batch = build_remote_dependency_batch_for_workspace_filtered(
        &mut state,
        &workspace_uri,
        Some(&filtered_source_uris),
    );
    let filtered_batch_elapsed = filtered_batch_start.elapsed();

    let symbol_position = symbol_position(&source_text, &config.symbol).ok_or_else(|| {
        format!(
            "symbol '{}' not found in '{}'",
            config.symbol,
            config.source_file.display()
        )
    })?;
    let definition_start = Instant::now();
    let definition_result = definition(
        &state,
        &GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: Uri::from_str(&source_uri)
                        .map_err(|err| format!("invalid source uri: {err}"))?,
                },
                position: symbol_position,
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        },
    );
    let definition_elapsed = definition_start.elapsed();

    let workspace = state
        .workspaces
        .get(&workspace_uri)
        .ok_or_else(|| format!("workspace missing after open: {workspace_uri}"))?;
    let last_metrics = workspace.cache.last_analysis_metrics_snapshot();

    println!("workspace_root={}", config.workspace_root.display());
    println!("source_file={}", config.source_file.display());
    println!("workspace_uri={workspace_uri}");
    println!("source_uri={source_uri}");
    println!("performance_mode={performance_mode:?}");
    println!("register_workspace={register_elapsed:?}");
    println!("open_document={open_elapsed:?}");
    println!("snapshot_uri={}", snapshot.uri);
    println!("snapshot_is_dependency={}", snapshot.is_dependency);
    println!("workspace_cache_documents={}", workspace.cache.len());
    println!("first_batch_elapsed={first_batch_elapsed:?}");
    if let Some(batch) = &first_batch {
        println!("first_batch_source_uris={}", batch.source_uris.len());
        println!("first_batch_candidates={}", batch.candidates.len());
        for candidate in batch.candidates.iter().take(20) {
            println!(
                "first_batch_candidate={} {}",
                candidate.kind, candidate.name
            );
        }
    } else {
        println!("first_batch_candidates=0");
    }
    println!("filtered_batch_elapsed={filtered_batch_elapsed:?}");
    if let Some(batch) = &filtered_batch {
        println!("filtered_batch_source_uris={}", batch.source_uris.len());
        println!("filtered_batch_candidates={}", batch.candidates.len());
        for candidate in batch.candidates.iter().take(20) {
            println!(
                "filtered_batch_candidate={} {}",
                candidate.kind, candidate.name
            );
        }
    } else {
        println!("filtered_batch_candidates=0");
    }
    println!("definition_elapsed={definition_elapsed:?}");
    println!("definition_found={}", definition_result.is_some());
    if let Some(metrics) = last_metrics {
        println!("analysis_parse_count={}", metrics.parse_count);
        println!("analysis_local_phase_count={}", metrics.local_phase_count);
        println!("analysis_dirty_uri_count={}", metrics.dirty_uri_count);
        println!(
            "analysis_dependency_projection_micros={}",
            metrics.dependency_projection_micros
        );
        println!(
            "analysis_project_update_micros={}",
            metrics.project_update_micros
        );
        println!(
            "analysis_snapshot_build_micros={}",
            metrics.snapshot_build_micros
        );
        println!(
            "analysis_routine_analysis_micros={}",
            metrics.routine_analysis_micros
        );
        println!("analysis_full_rebuild={}", metrics.full_rebuild);
        println!("analysis_unit_count={}", metrics.unit_count);
        println!("analysis_dirty_unit_count={}", metrics.dirty_unit_count);
    }

    Ok(())
}

fn parse_args(args: impl Iterator<Item = String>) -> Result<Config, String> {
    let mut workspace_root = None;
    let mut source_file = None;
    let mut dependency_store_path = None;
    let mut symbol = "main_processing_pre_step".to_string();
    let mut args = args.peekable();

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--workspace" => {
                let value = args
                    .next()
                    .ok_or_else(|| "--workspace requires a directory path".to_string())?;
                workspace_root = Some(PathBuf::from(value));
            }
            "--source" => {
                let value = args
                    .next()
                    .ok_or_else(|| "--source requires a file path".to_string())?;
                source_file = Some(PathBuf::from(value));
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
    let source_file = source_file.ok_or_else(|| "--source is required".to_string())?;
    if !Path::new(&workspace_root).is_dir() {
        return Err(format!(
            "workspace root does not exist or is not a directory: '{}'",
            workspace_root.display()
        ));
    }
    if !Path::new(&source_file).is_file() {
        return Err(format!(
            "source file does not exist or is not a file: '{}'",
            source_file.display()
        ));
    }
    if let Some(path) = dependency_store_path.as_ref()
        && !Path::new(path).is_file()
    {
        return Err(format!(
            "dependency store does not exist or is not a file: '{}'",
            path.display()
        ));
    }

    Ok(Config {
        workspace_root,
        source_file,
        dependency_store_path,
        symbol,
    })
}

fn symbol_position(text: &str, symbol: &str) -> Option<Position> {
    let mut found = None;
    for (line_index, line) in text.lines().enumerate() {
        let Some(column) = line.to_ascii_lowercase().find(&symbol.to_ascii_lowercase()) else {
            continue;
        };
        found = Some(Position {
            line: line_index as u32,
            character: column as u32,
        });
    }
    found
}

fn print_usage() {
    println!("Usage: cargo run -p abap_lsp --example remote_dependency_wave_perf -- [options]");
    println!("Options:");
    println!("  --workspace <dir>         Workspace root");
    println!("  --source <file>           Source file to open");
    println!("  --dependency-store <db>   Override dependency store path");
    println!("  --symbol <name>           Symbol to time definition on");
    println!("  --help                    Show this message");
}
