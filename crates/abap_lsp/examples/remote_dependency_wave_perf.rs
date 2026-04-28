use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::str::FromStr;
use std::sync::Arc;
use std::time::Instant;

use abap_cache::path_to_file_uri;
use abap_dependency_store::{CandidateCacheStatus, DependencyStore};
use abap_lsp::{
    GotoDefinitionParams, HoverParams, RemoteDependencyCandidate, ServerState,
    TextDocumentPositionParams, WorkspaceState, build_remote_dependency_batch_for_workspace,
    build_remote_dependency_batch_for_workspace_filtered, definition, hover, normalize_lsp_uri,
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

    let workspace_after_open = state
        .workspaces
        .get(&workspace_uri)
        .ok_or_else(|| format!("workspace missing after open: {workspace_uri}"))?;
    let document_composition = document_composition(workspace_after_open);
    let direct_candidate_composition = direct_candidate_composition(workspace_after_open);
    let initial_chain_cache = local_export_chain_candidate_composition(workspace_after_open);

    let mut first_batch_state = state.clone();
    let first_batch_start = Instant::now();
    let first_batch =
        build_remote_dependency_batch_for_workspace(&mut first_batch_state, &workspace_uri);
    let first_batch_elapsed = first_batch_start.elapsed();
    let in_flight_repeat_start = Instant::now();
    let in_flight_repeat_batch =
        build_remote_dependency_batch_for_workspace(&mut first_batch_state, &workspace_uri);
    let in_flight_repeat_elapsed = in_flight_repeat_start.elapsed();
    if let Some(workspace) = first_batch_state.workspaces.get_mut(&workspace_uri) {
        workspace.remote_resolution_in_flight = false;
    }
    let seen_repeat_start = Instant::now();
    let seen_repeat_batch =
        build_remote_dependency_batch_for_workspace(&mut first_batch_state, &workspace_uri);
    let seen_repeat_elapsed = seen_repeat_start.elapsed();

    let mut filtered_source_uris = HashSet::new();
    filtered_source_uris.insert(Arc::<str>::from(source_uri.as_str()));
    let mut filtered_state = state.clone();
    let filtered_batch_start = Instant::now();
    let filtered_batch = build_remote_dependency_batch_for_workspace_filtered(
        &mut filtered_state,
        &workspace_uri,
        Some(&filtered_source_uris),
    );
    let filtered_batch_elapsed = filtered_batch_start.elapsed();
    if let Some(workspace) = filtered_state.workspaces.get_mut(&workspace_uri) {
        workspace.remote_resolution_in_flight = false;
    }
    let filtered_seen_repeat_start = Instant::now();
    let filtered_seen_repeat_batch = build_remote_dependency_batch_for_workspace_filtered(
        &mut filtered_state,
        &workspace_uri,
        Some(&filtered_source_uris),
    );
    let filtered_seen_repeat_elapsed = filtered_seen_repeat_start.elapsed();

    let mut filtered_cached_state = state.clone();
    if let Some(workspace) = filtered_cached_state.workspaces.get_mut(&workspace_uri) {
        workspace.remote_resolution_seen.clear();
    }
    let filtered_cached_start = Instant::now();
    let filtered_cached_batch = build_remote_dependency_batch_for_workspace_filtered(
        &mut filtered_cached_state,
        &workspace_uri,
        Some(&filtered_source_uris),
    );
    let filtered_cached_elapsed = filtered_cached_start.elapsed();

    let mut filtered_cold_chain_state = state.clone();
    if let Some(workspace) = filtered_cold_chain_state.workspaces.get_mut(&workspace_uri) {
        workspace.remote_resolution_seen.clear();
        workspace.local_export_chain_candidates.clear();
        workspace.local_export_chain_refresh_candidates.clear();
    }
    let filtered_cold_chain_start = Instant::now();
    let filtered_cold_chain_batch = build_remote_dependency_batch_for_workspace_filtered(
        &mut filtered_cold_chain_state,
        &workspace_uri,
        Some(&filtered_source_uris),
    );
    let filtered_cold_chain_elapsed = filtered_cold_chain_start.elapsed();
    let chain_candidate_composition = filtered_cold_chain_state
        .workspaces
        .get(&workspace_uri)
        .map(local_export_chain_candidate_composition)
        .unwrap_or_default();
    let chain_candidate_cache_status = dependency_store_candidate_status(
        &config,
        workspace_after_open,
        &chain_candidate_composition,
    );

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
    let hover_start = Instant::now();
    let hover_result = hover(
        &state,
        &HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: Uri::from_str(&source_uri)
                        .map_err(|err| format!("invalid source uri: {err}"))?,
                },
                position: symbol_position,
            },
            work_done_progress_params: Default::default(),
        },
    );
    let hover_elapsed = hover_start.elapsed();

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
    print_document_composition(&document_composition);
    print_direct_candidate_composition(&direct_candidate_composition);
    print_chain_candidate_composition("initial", &initial_chain_cache);
    print_hydration_metrics(workspace);
    println!("first_batch_elapsed={first_batch_elapsed:?}");
    print_batch("first_batch", first_batch.as_ref());
    println!("in_flight_repeat_elapsed={in_flight_repeat_elapsed:?}");
    print_batch("in_flight_repeat", in_flight_repeat_batch.as_ref());
    println!("seen_repeat_elapsed={seen_repeat_elapsed:?}");
    print_batch("seen_repeat", seen_repeat_batch.as_ref());
    println!("filtered_batch_elapsed={filtered_batch_elapsed:?}");
    print_batch("filtered_batch", filtered_batch.as_ref());
    println!("filtered_seen_repeat_elapsed={filtered_seen_repeat_elapsed:?}");
    print_batch("filtered_seen_repeat", filtered_seen_repeat_batch.as_ref());
    println!("filtered_cached_elapsed={filtered_cached_elapsed:?}");
    print_batch("filtered_cached", filtered_cached_batch.as_ref());
    println!("filtered_cold_chain_elapsed={filtered_cold_chain_elapsed:?}");
    print_batch("filtered_cold_chain", filtered_cold_chain_batch.as_ref());
    print_chain_candidate_composition("source_chain", &chain_candidate_composition);
    print_candidate_cache_status(&chain_candidate_cache_status);
    println!("definition_elapsed={definition_elapsed:?}");
    println!("definition_found={}", definition_result.is_some());
    println!("hover_elapsed={hover_elapsed:?}");
    println!("hover_found={}", hover_result.is_some());
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

#[derive(Debug, Default)]
struct DocumentComposition {
    editable_documents: usize,
    local_export_dependency_documents: usize,
    dependency_store_artifact_documents: usize,
}

#[derive(Debug, Clone, Default)]
struct CandidateComposition {
    total: usize,
    unique: usize,
    by_kind: BTreeMap<String, usize>,
    candidates: Vec<RemoteDependencyCandidate>,
}

#[derive(Debug, Default)]
struct DirectCandidateComposition {
    editable: CandidateComposition,
    local_export_dependencies: CandidateComposition,
    dependency_store_artifacts: CandidateComposition,
}

#[derive(Debug, Default)]
struct CandidateCacheStatusComposition {
    artifact: usize,
    negative: usize,
    missing: usize,
    unavailable: bool,
}

fn document_composition(workspace: &WorkspaceState) -> DocumentComposition {
    let mut out = DocumentComposition::default();
    for uri in workspace.cache.uris() {
        let Some(snapshot) = workspace.cache.get(uri.as_ref()) else {
            continue;
        };
        match document_bucket(uri.as_ref(), snapshot.is_dependency) {
            "editable" => out.editable_documents += 1,
            "dependency_store_artifact" => out.dependency_store_artifact_documents += 1,
            _ => out.local_export_dependency_documents += 1,
        }
    }
    out
}

fn direct_candidate_composition(workspace: &WorkspaceState) -> DirectCandidateComposition {
    let mut out = DirectCandidateComposition::default();
    for uri in workspace.cache.uris() {
        let Some(snapshot) = workspace.cache.get(uri.as_ref()) else {
            continue;
        };
        let candidates = abap_lsp::collect_remote_dependency_candidates(snapshot.as_ref());
        match document_bucket(uri.as_ref(), snapshot.is_dependency) {
            "editable" => out.editable.extend(candidates),
            "dependency_store_artifact" => out.dependency_store_artifacts.extend(candidates),
            _ => out.local_export_dependencies.extend(candidates),
        }
    }
    out.editable.finish();
    out.local_export_dependencies.finish();
    out.dependency_store_artifacts.finish();
    out
}

fn local_export_chain_candidate_composition(workspace: &WorkspaceState) -> CandidateComposition {
    let mut out = CandidateComposition::default();
    for candidates in workspace.local_export_chain_candidates.values() {
        out.extend(candidates.clone());
    }
    out.finish();
    out
}

fn dependency_store_candidate_status(
    config: &Config,
    workspace: &WorkspaceState,
    candidates: &CandidateComposition,
) -> CandidateCacheStatusComposition {
    let Some(profile) = workspace.dependency_profile.as_ref() else {
        return CandidateCacheStatusComposition {
            unavailable: true,
            ..Default::default()
        };
    };
    let Ok(store) = DependencyStore::from_override_path(config.dependency_store_path.as_deref())
    else {
        return CandidateCacheStatusComposition {
            unavailable: true,
            ..Default::default()
        };
    };
    let Ok(reader) = store.reader() else {
        return CandidateCacheStatusComposition {
            unavailable: true,
            ..Default::default()
        };
    };
    let connection_key = workspace
        .manifest
        .as_ref()
        .map(|manifest| manifest.connection.trim().to_ascii_lowercase())
        .filter(|connection| !connection.is_empty())
        .unwrap_or_else(|| "default".to_string());
    let mut out = CandidateCacheStatusComposition::default();
    for candidate in &candidates.candidates {
        match reader
            .find_cached_candidate(
                profile,
                connection_key.as_str(),
                candidate.name.as_str(),
                candidate.kind.as_str(),
            )
            .unwrap_or(CandidateCacheStatus::Missing)
        {
            CandidateCacheStatus::Artifact => out.artifact += 1,
            CandidateCacheStatus::Negative => out.negative += 1,
            CandidateCacheStatus::Missing => out.missing += 1,
        }
    }
    out
}

impl CandidateComposition {
    fn extend(&mut self, candidates: Vec<RemoteDependencyCandidate>) {
        self.total += candidates.len();
        self.candidates.extend(candidates);
    }

    fn finish(&mut self) {
        let mut unique = HashMap::<String, RemoteDependencyCandidate>::new();
        for candidate in self.candidates.drain(..) {
            let key = format!(
                "{}|{}",
                candidate.kind.trim().to_ascii_lowercase(),
                candidate.name.trim().to_ascii_lowercase()
            );
            unique.entry(key).or_insert(candidate);
        }
        self.unique = unique.len();
        for candidate in unique.values() {
            *self
                .by_kind
                .entry(candidate.kind.trim().to_ascii_lowercase())
                .or_default() += 1;
        }
        let mut candidates: Vec<_> = unique.into_values().collect();
        candidates.sort_by(|left, right| {
            left.kind
                .cmp(&right.kind)
                .then_with(|| left.name.cmp(&right.name))
        });
        self.candidates = candidates;
    }
}

fn document_bucket(uri: &str, is_dependency: bool) -> &'static str {
    if !is_dependency {
        "editable"
    } else if uri.to_ascii_lowercase().starts_with("abapls-cache:") {
        "dependency_store_artifact"
    } else {
        "local_export_dependency"
    }
}

fn print_document_composition(composition: &DocumentComposition) {
    println!("editable_documents={}", composition.editable_documents);
    println!(
        "local_export_dependency_documents={}",
        composition.local_export_dependency_documents
    );
    println!(
        "dependency_store_artifact_documents={}",
        composition.dependency_store_artifact_documents
    );
}

fn print_direct_candidate_composition(composition: &DirectCandidateComposition) {
    print_candidate_composition("editable_direct", &composition.editable);
    print_candidate_composition(
        "local_export_dependency_direct",
        &composition.local_export_dependencies,
    );
    print_candidate_composition(
        "dependency_store_artifact_direct",
        &composition.dependency_store_artifacts,
    );
}

fn print_chain_candidate_composition(label: &str, composition: &CandidateComposition) {
    print_candidate_composition(label, composition);
    for candidate in composition.candidates.iter().take(20) {
        println!("{label}_candidate={} {}", candidate.kind, candidate.name);
    }
}

fn print_candidate_composition(label: &str, composition: &CandidateComposition) {
    println!("{label}_candidate_total={}", composition.total);
    println!("{label}_candidate_unique={}", composition.unique);
    println!(
        "{label}_candidate_by_kind={}",
        format_kind_counts(&composition.by_kind)
    );
}

fn print_candidate_cache_status(status: &CandidateCacheStatusComposition) {
    println!(
        "source_chain_candidate_cache_status_unavailable={}",
        status.unavailable
    );
    println!("source_chain_candidate_cache_artifact={}", status.artifact);
    println!("source_chain_candidate_cache_negative={}", status.negative);
    println!("source_chain_candidate_cache_missing={}", status.missing);
}

fn print_hydration_metrics(workspace: &WorkspaceState) {
    let Some(metrics) = workspace.dependency_store_hydration_metrics.as_ref() else {
        println!("dependency_store_hydration_metrics_present=false");
        return;
    };
    println!("dependency_store_hydration_metrics_present=true");
    println!("dependency_store_hydration_elapsed={:?}", metrics.elapsed);
    println!("dependency_store_hydration_supported={}", metrics.supported);
    println!(
        "dependency_store_hydration_profile_present={}",
        metrics.profile_present
    );
    println!(
        "dependency_store_hydration_reader_available={}",
        metrics.reader_available
    );
    println!(
        "dependency_store_hydration_iterations={}",
        metrics.iterations
    );
    println!(
        "dependency_store_hydration_cache_uri_scans={}",
        metrics.cache_uri_scans
    );
    println!(
        "dependency_store_hydration_source_snapshots_examined={}",
        metrics.source_snapshots_examined
    );
    println!(
        "dependency_store_hydration_dependency_snapshots_skipped={}",
        metrics.dependency_snapshots_skipped
    );
    println!(
        "dependency_store_hydration_candidate_count={}",
        metrics.candidate_count
    );
    println!(
        "dependency_store_hydration_unique_candidate_queries={}",
        metrics.unique_candidate_queries
    );
    println!(
        "dependency_store_hydration_artifact_hits={}",
        metrics.artifact_hits
    );
    println!(
        "dependency_store_hydration_existing_or_duplicate_inputs={}",
        metrics.existing_or_duplicate_inputs
    );
    println!(
        "dependency_store_hydration_hydrated_input_count={}",
        metrics.hydrated_input_count
    );
    println!(
        "dependency_store_hydration_published_batch_count={}",
        metrics.published_batch_count
    );
    println!(
        "dependency_store_hydration_candidate_collection_micros={}",
        metrics.candidate_collection_micros
    );
    println!(
        "dependency_store_hydration_store_lookup_micros={}",
        metrics.store_lookup_micros
    );
    println!(
        "dependency_store_hydration_publish_micros={}",
        metrics.publish_micros
    );
}

fn print_batch(label: &str, batch: Option<&abap_lsp::RemoteDependencyResolveParams>) {
    if let Some(batch) = batch {
        println!("{label}_source_uris={}", batch.source_uris.len());
        println!("{label}_candidates={}", batch.candidates.len());
        println!(
            "{label}_candidate_by_kind={}",
            candidate_kind_counts(batch.candidates.as_slice())
        );
        for candidate in batch.candidates.iter().take(20) {
            println!("{label}_candidate={} {}", candidate.kind, candidate.name);
        }
    } else {
        println!("{label}_source_uris=0");
        println!("{label}_candidates=0");
        println!("{label}_candidate_by_kind=");
    }
}

fn candidate_kind_counts(candidates: &[RemoteDependencyCandidate]) -> String {
    let mut counts = BTreeMap::new();
    for candidate in candidates {
        *counts
            .entry(candidate.kind.trim().to_ascii_lowercase())
            .or_insert(0usize) += 1;
    }
    format_kind_counts(&counts)
}

fn format_kind_counts(counts: &BTreeMap<String, usize>) -> String {
    counts
        .iter()
        .map(|(kind, count)| format!("{kind}:{count}"))
        .collect::<Vec<_>>()
        .join(",")
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
