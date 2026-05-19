use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::sync::Arc;
use std::time::Instant;

use abap_cache::{
    DocumentInput, DocumentStore, SnapshotBuildPlan, WorkspaceDocument, WorkspaceLoadResult,
    load_workspace_documents, manifest_document_metadata, path_to_file_uri,
};
use abap_lsp::{
    LocalExportDependencyClosureProfile,
    replace_all_workspace_documents_with_local_exports_for_build_plan_profiled,
};

const TOP_N: usize = 10;

struct Config {
    workspace_root: PathBuf,
    target_file: Option<PathBuf>,
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
    abap_runtime::init_global_executor();
    let config = parse_args(std::env::args().skip(1))?;
    let workspace_uri = path_to_file_uri(&config.workspace_root);

    let load_start = Instant::now();
    let workspace = load_workspace_documents(&workspace_uri, &HashMap::new());
    let load_elapsed = load_start.elapsed();
    let documents = workspace_documents_with_target(&workspace, config.target_file.as_deref())?;
    let dependency_document_count = documents
        .iter()
        .filter(|document| document.is_dependency)
        .count();

    let inputs: Vec<_> = documents
        .iter()
        .map(|document| DocumentInput {
            uri: Arc::clone(&document.uri),
            version: document.version,
            text: Arc::from(document.text.as_str()),
            is_dependency: document.is_dependency,
            object_name: document.object_name.clone(),
        })
        .collect();

    let baseline_store = DocumentStore::default();
    let baseline_start = Instant::now();
    let baseline_snapshots = baseline_store.replace_all(inputs);
    let baseline_elapsed = baseline_start.elapsed();
    let baseline_metrics = baseline_store
        .last_analysis_metrics_snapshot()
        .ok_or_else(|| "baseline analysis metrics were not recorded".to_string())?;

    let local_export_store = DocumentStore::default();
    let local_export_cold_start = Instant::now();
    let (local_export_cold_snapshots, local_export_cold_profile) =
        replace_all_workspace_documents_with_local_exports_for_build_plan_profiled(
            &local_export_store,
            &workspace.root_path,
            &documents,
            SnapshotBuildPlan::FULL,
            None,
        );
    let local_export_cold_elapsed = local_export_cold_start.elapsed();
    let local_export_cold_metrics = local_export_store
        .last_analysis_metrics_snapshot()
        .ok_or_else(|| "cold local-export analysis metrics were not recorded".to_string())?;

    let local_export_warm_store = DocumentStore::default();
    let local_export_warm_start = Instant::now();
    let (local_export_warm_snapshots, local_export_warm_profile) =
        replace_all_workspace_documents_with_local_exports_for_build_plan_profiled(
            &local_export_warm_store,
            &workspace.root_path,
            &documents,
            SnapshotBuildPlan::FULL,
            None,
        );
    let local_export_warm_elapsed = local_export_warm_start.elapsed();
    let local_export_warm_metrics = local_export_warm_store
        .last_analysis_metrics_snapshot()
        .ok_or_else(|| "warm local-export analysis metrics were not recorded".to_string())?;

    let editor_workspace_store = DocumentStore::default();
    let editor_workspace_start = Instant::now();
    let (editor_workspace_snapshots, editor_workspace_profile) =
        replace_all_workspace_documents_with_local_exports_for_build_plan_profiled(
            &editor_workspace_store,
            &workspace.root_path,
            &documents,
            SnapshotBuildPlan::EDITOR_WORKSPACE,
            None,
        );
    let editor_workspace_elapsed = editor_workspace_start.elapsed();
    let editor_workspace_metrics = editor_workspace_store
        .last_analysis_metrics_snapshot()
        .ok_or_else(|| "editor-workspace analysis metrics were not recorded".to_string())?;

    let target_uri = config
        .target_file
        .as_ref()
        .map(|path| path_to_file_uri(path.as_path()))
        .or_else(|| documents.first().map(|document| document.uri.to_string()));

    let baseline_target = target_uri
        .as_deref()
        .and_then(|uri| baseline_snapshots.get(uri))
        .cloned();
    let local_export_cold_target = target_uri
        .as_deref()
        .and_then(|uri| local_export_cold_snapshots.get(uri))
        .cloned();
    let local_export_warm_target = target_uri
        .as_deref()
        .and_then(|uri| local_export_warm_snapshots.get(uri))
        .cloned();
    let editor_workspace_target = target_uri
        .as_deref()
        .and_then(|uri| editor_workspace_snapshots.get(uri))
        .cloned();

    println!("workspace_root={}", config.workspace_root.display());
    println!(
        "target_uri={}",
        target_uri.unwrap_or_else(|| "<none>".to_string())
    );
    println!("manifest_present={}", workspace.manifest.is_some());
    println!("loaded_documents={}", documents.len());
    println!("loaded_dependency_documents={dependency_document_count}");
    println!("load_workspace={load_elapsed:?}");
    print_result(
        "baseline",
        baseline_elapsed,
        &baseline_metrics,
        baseline_snapshots.len(),
        baseline_snapshots
            .values()
            .filter(|snapshot| snapshot.is_dependency)
            .count(),
        baseline_target
            .as_ref()
            .map_or(baseline_snapshots.len(), |snapshot| {
                snapshot.project.units.len()
            }),
    );
    print_result(
        "local_export_cold",
        local_export_cold_elapsed,
        &local_export_cold_metrics,
        local_export_cold_snapshots.len(),
        local_export_cold_snapshots
            .values()
            .filter(|snapshot| snapshot.is_dependency)
            .count(),
        local_export_cold_target
            .as_ref()
            .map_or(local_export_cold_snapshots.len(), |snapshot| {
                snapshot.project.units.len()
            }),
    );
    print_closure_profile("local_export_cold", &local_export_cold_profile, TOP_N);
    print_result(
        "local_export_warm",
        local_export_warm_elapsed,
        &local_export_warm_metrics,
        local_export_warm_snapshots.len(),
        local_export_warm_snapshots
            .values()
            .filter(|snapshot| snapshot.is_dependency)
            .count(),
        local_export_warm_target
            .as_ref()
            .map_or(local_export_warm_snapshots.len(), |snapshot| {
                snapshot.project.units.len()
            }),
    );
    print_closure_profile("local_export_warm", &local_export_warm_profile, TOP_N);
    print_result(
        "editor_workspace",
        editor_workspace_elapsed,
        &editor_workspace_metrics,
        editor_workspace_snapshots.len(),
        editor_workspace_snapshots
            .values()
            .filter(|snapshot| snapshot.is_dependency)
            .count(),
        editor_workspace_target
            .as_ref()
            .map_or(editor_workspace_snapshots.len(), |snapshot| {
                snapshot.project.units.len()
            }),
    );
    print_closure_profile("editor_workspace", &editor_workspace_profile, TOP_N);

    Ok(())
}

fn workspace_documents_with_target(
    workspace: &WorkspaceLoadResult,
    target_file: Option<&Path>,
) -> Result<Vec<WorkspaceDocument>, String> {
    let mut documents = workspace.documents.clone();
    let Some(target_file) = target_file else {
        return Ok(documents);
    };

    let target_uri = path_to_file_uri(target_file);
    if documents
        .iter()
        .any(|document| document.uri.as_ref() == target_uri)
    {
        return Ok(documents);
    }

    let source = std::fs::read_to_string(target_file)
        .map_err(|err| format!("{}: {err}", target_file.display()))?;
    let (is_dependency, object_name) = workspace
        .manifest
        .as_ref()
        .and_then(|manifest| {
            manifest_document_metadata(
                &workspace.root_path,
                &workspace.root_uri,
                manifest,
                &target_uri,
            )
        })
        .unwrap_or((false, None));
    documents.push(WorkspaceDocument {
        uri: Arc::from(target_uri.as_str()),
        version: 1,
        text: source,
        is_dependency,
        object_name,
    });
    Ok(documents)
}

fn print_result(
    label: &str,
    elapsed: std::time::Duration,
    metrics: &abap_cache::WorkspaceAnalysisMetricsSnapshot,
    snapshot_count: usize,
    dependency_snapshot_count: usize,
    project_unit_count: usize,
) {
    println!("{label}_elapsed={elapsed:?}");
    println!("{label}_snapshots={snapshot_count}");
    println!("{label}_dependency_snapshots={dependency_snapshot_count}");
    println!("{label}_project_units={project_unit_count}");
    println!("{label}_parse_count={}", metrics.parse_count);
    println!("{label}_local_phase_count={}", metrics.local_phase_count);
    println!(
        "{label}_prepare_documents={:?}",
        std::time::Duration::from_micros(metrics.local_phase_micros as u64)
    );
    println!(
        "{label}_dependency_projection={:?}",
        std::time::Duration::from_micros(metrics.dependency_projection_micros as u64)
    );
    println!(
        "{label}_parse_work={:?}",
        std::time::Duration::from_micros(metrics.parse_work_micros as u64)
    );
    println!(
        "{label}_local_phase_work={:?}",
        std::time::Duration::from_micros(metrics.local_phase_work_micros as u64)
    );
    println!(
        "{label}_project_update={:?}",
        std::time::Duration::from_micros(metrics.project_update_micros as u64)
    );
    println!(
        "{label}_snapshot_build={:?}",
        std::time::Duration::from_micros(metrics.snapshot_build_micros as u64)
    );
    println!(
        "{label}_routine_analysis={:?}",
        std::time::Duration::from_micros(metrics.routine_analysis_micros as u64)
    );
    println!(
        "{label}_static_analysis_summary={:?}",
        std::time::Duration::from_micros(metrics.static_analysis_summary_micros as u64)
    );
    println!(
        "{label}_callable_summary={:?}",
        std::time::Duration::from_micros(metrics.callable_summary_micros as u64)
    );
    println!("{label}_full_rebuild={}", metrics.full_rebuild);
    println!("{label}_dirty_unit_count={}", metrics.dirty_unit_count);
    println!(
        "{label}_diagnostic_scope_unit_count={}",
        metrics.diagnostic_scope_unit_count
    );
    println!(
        "{label}_validation_unit_count={}",
        metrics.validation_unit_count
    );
    println!(
        "{label}_resolve_cross_unit={:?}",
        std::time::Duration::from_micros(metrics.resolve_cross_unit_micros as u64)
    );
    println!(
        "{label}_infer_semantic_facts={:?}",
        std::time::Duration::from_micros(metrics.infer_semantic_facts_micros as u64)
    );
    println!(
        "{label}_validate={:?}",
        std::time::Duration::from_micros(metrics.validate_micros as u64)
    );
}

fn print_closure_profile(label: &str, profile: &LocalExportDependencyClosureProfile, top_n: usize) {
    let resolver = &profile.local_export_resolve;
    println!(
        "{label}_local_export_closure_total={:?}",
        profile.total_elapsed
    );
    println!("{label}_local_export_closure_waves={}", profile.waves);
    println!(
        "{label}_local_export_closure_queue_iterations={}",
        profile.queue_iterations
    );
    println!(
        "{label}_local_export_documents_examined_for_candidates={}",
        profile.documents_examined_for_candidates
    );
    println!(
        "{label}_local_export_remote_dependency_candidates_produced={}",
        profile.remote_dependency_candidates_produced
    );
    println!(
        "{label}_local_export_candidate_cache_hits={}",
        profile.candidate_cache_hits
    );
    println!(
        "{label}_local_export_candidate_cache_misses={}",
        profile.candidate_cache_misses
    );
    println!(
        "{label}_local_export_unique_candidate_resolution_attempts={}",
        profile.unique_candidate_resolution_attempts
    );
    println!(
        "{label}_local_export_resolution_cache_hits={}",
        profile.resolution_cache_hits
    );
    println!(
        "{label}_local_export_resolution_cache_misses={}",
        profile.resolution_cache_misses
    );
    println!(
        "{label}_local_export_resolver_index_builds={}",
        resolver.index_build_count
    );
    println!(
        "{label}_local_export_resolver_index_build_time={:?}",
        resolver.index_build_time
    );
    println!(
        "{label}_local_export_resolver_index_refreshes={}",
        resolver.index_refresh_count
    );
    println!(
        "{label}_local_export_resolver_index_refresh_time={:?}",
        resolver.index_refresh_time
    );
    println!(
        "{label}_local_export_resolver_index_hits={}",
        resolver.resolver_index_hits
    );
    println!(
        "{label}_local_export_shared_index_cache_hits={}",
        resolver.shared_index_cache_hits
    );
    println!(
        "{label}_local_export_document_reads={}",
        resolver.document_read_count
    );
    println!(
        "{label}_local_export_document_read_cache_hits={}",
        resolver.document_read_cache_hits
    );
    println!(
        "{label}_local_export_document_read_time={:?}",
        resolver.document_read_time
    );
    println!(
        "{label}_local_export_document_read_bytes={}",
        resolver.document_read_bytes
    );
    println!(
        "{label}_local_export_candidate_collection_wall={:?}",
        profile.candidate_collection_time
    );
    println!(
        "{label}_local_export_candidate_parse_analyze_time={:?}",
        profile.candidate_parse_analyze_time
    );
    println!(
        "{label}_local_export_added_dependency_documents={}",
        profile.added_dependency_documents
    );

    let mut slow_documents = profile.candidate_documents.clone();
    slow_documents.sort_by_key(|document| std::cmp::Reverse(document.elapsed));
    for (idx, document) in slow_documents.into_iter().take(top_n).enumerate() {
        let rank = idx + 1;
        println!(
            "{label}_local_export_top_candidate_{rank}=elapsed={:?},parse_analyze={:?},candidates={},cache_hit={},dependency={},bytes={},uri={}",
            document.elapsed,
            document.parse_analyze_time,
            document.candidate_count,
            document.cache_hit,
            document.is_dependency,
            document.text_len,
            document.uri
        );
    }

    let mut slow_waves = profile.wave_profiles.clone();
    slow_waves.sort_by_key(|wave| std::cmp::Reverse(wave.elapsed));
    for (idx, wave) in slow_waves.into_iter().take(top_n).enumerate() {
        let rank = idx + 1;
        println!(
            "{label}_local_export_top_wave_{rank}=index={},elapsed={:?},batch_documents={},candidates={},candidate_cache_hits={},candidate_cache_misses={},attempts={},cache_hits={},cache_misses={},added={},candidate_wall={:?},candidate_parse_analyze={:?},index_build={:?},index_refresh={:?},document_read={:?},document_read_cache_hits={}",
            wave.index,
            wave.elapsed,
            wave.batch_documents,
            wave.candidates_produced,
            wave.candidate_cache_hits,
            wave.candidate_cache_misses,
            wave.unique_candidate_resolution_attempts,
            wave.resolution_cache_hits,
            wave.resolution_cache_misses,
            wave.added_dependency_documents,
            wave.candidate_collection_time,
            wave.candidate_parse_analyze_time,
            wave.local_export_resolve.index_build_time,
            wave.local_export_resolve.index_refresh_time,
            wave.local_export_resolve.document_read_time,
            wave.local_export_resolve.document_read_cache_hits
        );
    }
}

fn parse_args(args: impl Iterator<Item = String>) -> Result<Config, String> {
    let mut workspace_root = None;
    let mut target_file = None;
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
            "--help" | "-h" => {
                print_usage();
                std::process::exit(0);
            }
            other => return Err(format!("unrecognized argument: {other}")),
        }
    }

    let workspace_root = workspace_root.ok_or_else(|| "--workspace is required".to_string())?;
    if !workspace_root.is_dir() {
        return Err(format!(
            "workspace root does not exist or is not a directory: '{}'",
            workspace_root.display()
        ));
    }
    if let Some(target_file) = target_file.as_ref()
        && !target_file.is_file()
    {
        return Err(format!(
            "target file does not exist: '{}'",
            target_file.display()
        ));
    }

    Ok(Config {
        workspace_root,
        target_file,
    })
}

fn print_usage() {
    println!(
        "Usage: cargo run -p abap_lsp --example workspace_local_export_perf -- --workspace <dir> [--target <file>]"
    );
}
