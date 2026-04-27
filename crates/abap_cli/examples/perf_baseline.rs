use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use abap_cache::{
    CallDataflowQuery, CallGraphEdge, CallGraphEdgeKind, CallGraphNode, CallGraphNodeKind,
    CallGraphResolutionStatus, DocumentInput, DocumentStore, ProjectCallGraph, SnapshotBuildPlan,
    build_call_dataflow_trace, load_workspace_documents, path_to_file_uri,
};
use abap_lsp::build_semantic_tokens;
use abap_parser::parse;
use abap_symbols::analyze_unit;
use serde_json::{Value, json};

const DEFAULT_ITERATIONS: usize = 5;
const DEFAULT_WARMUP_ITERATIONS: usize = 1;
const DEFAULT_PARSER_FILE: &str = "single-file/ZPERF_PARSER_MIXED.abap";
const DEFAULT_WORKSPACE_TARGET: &str = "src/reports/ZPERF_DRIVER/ZPERF_DRIVER.abap";
const DEFAULT_CALL_DATAFLOW_TARGET: &str = "BAPI_PO_CREATE1";
const DEFAULT_CALL_DATAFLOW_CALLER: &str = "call_api";

#[derive(Debug, Clone)]
struct Config {
    corpus_root: PathBuf,
    parser_file: PathBuf,
    workspace_target: PathBuf,
    iterations: usize,
    warmup_iterations: usize,
    pretty: bool,
    output: Option<PathBuf>,
    call_dataflow_target: String,
    call_dataflow_caller: Option<String>,
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
    let config = parse_args(env::args().skip(1))?;
    let baseline = run_baseline(&config)?;
    let rendered = if config.pretty {
        serde_json::to_string_pretty(&baseline)
    } else {
        serde_json::to_string(&baseline)
    }
    .map_err(|err| err.to_string())?;

    if let Some(output) = config.output.as_ref() {
        if let Some(parent) = output.parent()
            && !parent.as_os_str().is_empty()
        {
            fs::create_dir_all(parent)
                .map_err(|err| format!("failed to create '{}': {err}", parent.display()))?;
        }
        fs::write(output, rendered.as_bytes())
            .map_err(|err| format!("failed to write '{}': {err}", output.display()))?;
        println!("wrote performance baseline JSON to {}", output.display());
    } else {
        println!("{rendered}");
    }

    Ok(())
}

fn run_baseline(config: &Config) -> Result<Value, String> {
    let parser_source = fs::read_to_string(&config.parser_file)
        .map_err(|err| format!("failed to read '{}': {err}", config.parser_file.display()))?;
    let parser_uri = "file:///perf_corpus/ZPERF_PARSER_MIXED.abap";
    let parser_parse = parse(&parser_source);
    if !parser_parse.errors.is_empty() {
        return Err(format!(
            "parser corpus has {} parse error(s); fix '{}' before recording a baseline",
            parser_parse.errors.len(),
            config.parser_file.display()
        ));
    }

    let workspace_uri = path_to_file_uri(&config.corpus_root);
    let workspace_target_uri = path_to_file_uri(&config.workspace_target);
    let warm_workspace = load_workspace_documents(&workspace_uri, &HashMap::new());
    if warm_workspace.manifest_error.is_some() {
        return Err(format!(
            "workspace manifest failed to load: {}",
            warm_workspace.manifest_error.unwrap_or_default()
        ));
    }
    let workspace_inputs = workspace_inputs(&warm_workspace.documents);
    if !workspace_inputs
        .iter()
        .any(|input| input.uri.as_ref() == workspace_target_uri)
    {
        return Err(format!(
            "workspace target '{}' was not loaded from corpus '{}'",
            config.workspace_target.display(),
            config.corpus_root.display()
        ));
    }

    let semantic_store = DocumentStore::default();
    let semantic_snapshot = semantic_store.publish_input_with_build_plan(
        DocumentInput {
            uri: Arc::from(parser_uri),
            version: 1,
            text: Arc::from(parser_source.as_str()),
            is_dependency: false,
            object_name: None,
        },
        SnapshotBuildPlan::FULL,
    );
    let semantic_warmup = build_semantic_tokens(semantic_snapshot.as_ref());
    if semantic_warmup.data.is_empty() {
        return Err("semantic-token benchmark corpus produced no tokens".to_string());
    }

    let benchmarks = vec![
        measure("parser.parse_file", config, || {
            let parsed = parse(&parser_source);
            if !parsed.errors.is_empty() {
                return Err(format!("parse produced {} error(s)", parsed.errors.len()));
            }
            Ok(json!({
                "source_path": config.parser_file.display().to_string(),
                "bytes": parser_source.len(),
                "lines": parser_source.lines().count(),
                "tokens": parsed.tokens.len(),
                "parse_errors": parsed.errors.len(),
            }))
        })?,
        measure("symbols.analyze_unit", config, || {
            let unit = analyze_unit(parser_uri, &parser_source, &parser_parse);
            Ok(json!({
                "source_path": config.parser_file.display().to_string(),
                "symbols": unit.symbols.len(),
                "references": unit.references.len(),
                "diagnostics": unit.diagnostics.len(),
                "field_accesses": unit.field_accesses.len(),
                "named_arguments": unit.named_arguments.len(),
                "call_sites": unit.call_sites.len(),
                "assignment_sites": unit.assignment_sites.len(),
            }))
        })?,
        measure("semantic_tokens.build", config, || {
            let tokens = build_semantic_tokens(semantic_snapshot.as_ref());
            Ok(json!({
                "source_path": config.parser_file.display().to_string(),
                "tokens": tokens.data.len(),
            }))
        })?,
        measure("workspace.load_documents", config, || {
            let workspace = load_workspace_documents(&workspace_uri, &HashMap::new());
            if workspace.manifest_error.is_some() {
                return Err(format!(
                    "workspace manifest failed to load: {}",
                    workspace.manifest_error.unwrap_or_default()
                ));
            }
            Ok(json!({
                "workspace_root": config.corpus_root.display().to_string(),
                "manifest_present": workspace.manifest.is_some(),
                "manifest_len_bytes": workspace.manifest_len_bytes,
                "documents": workspace.documents.len(),
                "dependency_documents": workspace.documents.iter().filter(|document| document.is_dependency).count(),
                "total_bytes": workspace.documents.iter().map(|document| document.text.len()).sum::<usize>(),
            }))
        })?,
        measure("workspace.replace_all_full", config, || {
            let store = DocumentStore::default();
            let snapshots = store
                .replace_all_with_build_plan(workspace_inputs.clone(), SnapshotBuildPlan::FULL);
            let metrics = store
                .last_analysis_metrics_snapshot()
                .ok_or_else(|| "workspace analysis metrics were not recorded".to_string())?;
            let target = snapshot_for_uri(&snapshots, &workspace_target_uri)?;
            Ok(json!({
                "workspace_root": config.corpus_root.display().to_string(),
                "snapshots": snapshots.len(),
                "target_uri": workspace_target_uri.as_str(),
                "project_units": target.project.units.len(),
                "project_diagnostics": target.project.diagnostics.len(),
                "parse_count": metrics.parse_count,
                "local_phase_count": metrics.local_phase_count,
                "dirty_uri_count": metrics.dirty_uri_count,
                "routine_analysis_micros": metrics.routine_analysis_micros,
                "static_analysis_summary_micros": metrics.static_analysis_summary_micros,
                "callable_summary_micros": metrics.callable_summary_micros,
                "resolve_cross_unit_micros": metrics.resolve_cross_unit_micros,
                "validate_micros": metrics.validate_micros,
                "full_rebuild": metrics.full_rebuild,
            }))
        })?,
        measure("call_graph.export_json", config, || {
            let store = DocumentStore::default();
            let snapshots = store.replace_all_with_build_plan(
                workspace_inputs.clone(),
                SnapshotBuildPlan::CALL_GRAPH,
            );
            let target = snapshot_for_uri(&snapshots, &workspace_target_uri)?;
            let export =
                call_graph_export_json(workspace_target_uri.as_str(), target.call_graph.as_ref());
            let output_bytes = serde_json::to_vec(&export)
                .map_err(|err| format!("failed to serialize call graph export: {err}"))?
                .len();
            Ok(json!({
                "target_uri": workspace_target_uri.as_str(),
                "project_node_count": target.call_graph.nodes.len(),
                "project_edge_count": target.call_graph.edges.len(),
                "output_bytes": output_bytes,
            }))
        })?,
        measure("call_dataflow.export_json", config, || {
            let store = DocumentStore::default();
            let snapshots = store.replace_all_with_build_plan(
                workspace_inputs.clone(),
                SnapshotBuildPlan::CALL_DATAFLOW,
            );
            let target = snapshot_for_uri(&snapshots, &workspace_target_uri)?;
            let trace = build_call_dataflow_trace(
                target.as_ref(),
                CallDataflowQuery {
                    target: config.call_dataflow_target.clone(),
                    caller: config.call_dataflow_caller.clone(),
                    occurrence: None,
                },
            );
            if trace.selected_call.is_none() {
                return Err(format!(
                    "call-dataflow query did not select a call for target '{}'",
                    config.call_dataflow_target
                ));
            }
            let output_bytes = serde_json::to_vec(&trace)
                .map_err(|err| format!("failed to serialize call-dataflow export: {err}"))?
                .len();
            Ok(json!({
                "target_uri": workspace_target_uri.as_str(),
                "query_target": config.call_dataflow_target.as_str(),
                "query_caller": config.call_dataflow_caller.as_deref(),
                "match_count": trace.summary.match_count,
                "ambiguous": trace.summary.ambiguous,
                "lifecycle_node_count": trace.summary.lifecycle_node_count,
                "lifecycle_edge_count": trace.summary.lifecycle_edge_count,
                "parameter_count": trace.summary.parameter_count,
                "mapping_count": trace.summary.mapping_count,
                "output_bytes": output_bytes,
            }))
        })?,
    ];

    Ok(json!({
        "schema": "abap.performance_baseline",
        "schema_version": 1,
        "generated_at_unix_seconds": SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|duration| duration.as_secs())
            .unwrap_or_default(),
        "runner": {
            "package": "abap_cli",
            "example": "perf_baseline",
            "profile": if cfg!(debug_assertions) { "debug" } else { "release" },
            "os": env::consts::OS,
            "arch": env::consts::ARCH,
        },
        "config": {
            "corpus_root": config.corpus_root.display().to_string(),
            "parser_file": config.parser_file.display().to_string(),
            "workspace_target": config.workspace_target.display().to_string(),
            "iterations": config.iterations,
            "warmup_iterations": config.warmup_iterations,
            "call_dataflow_target": config.call_dataflow_target.as_str(),
            "call_dataflow_caller": config.call_dataflow_caller.as_deref(),
        },
        "benchmarks": benchmarks,
    }))
}

fn measure<F>(name: &str, config: &Config, mut run_once: F) -> Result<Value, String>
where
    F: FnMut() -> Result<Value, String>,
{
    for _ in 0..config.warmup_iterations {
        let _ = run_once()?;
    }

    let mut durations = Vec::with_capacity(config.iterations);
    let mut last = Value::Null;
    for _ in 0..config.iterations {
        let start = Instant::now();
        last = run_once()?;
        durations.push(start.elapsed());
    }

    Ok(json!({
        "name": name,
        "iterations": config.iterations,
        "warmup_iterations": config.warmup_iterations,
        "stats": duration_stats(&durations),
        "last": last,
    }))
}

fn duration_stats(durations: &[Duration]) -> Value {
    let mut micros: Vec<u64> = durations
        .iter()
        .map(|duration| duration.as_micros().min(u128::from(u64::MAX)) as u64)
        .collect();
    micros.sort_unstable();
    let total: u64 = micros.iter().sum();
    let mean = if micros.is_empty() {
        0.0
    } else {
        total as f64 / micros.len() as f64
    };
    let median = match micros.len() {
        0 => 0,
        len if len % 2 == 1 => micros[len / 2],
        len => (micros[len / 2 - 1] + micros[len / 2]) / 2,
    };

    json!({
        "total_micros": total,
        "min_micros": micros.first().copied().unwrap_or_default(),
        "median_micros": median,
        "mean_micros": mean,
        "max_micros": micros.last().copied().unwrap_or_default(),
        "samples_micros": micros,
    })
}

fn workspace_inputs(documents: &[abap_cache::WorkspaceDocument]) -> Vec<DocumentInput> {
    documents
        .iter()
        .map(|document| DocumentInput {
            uri: Arc::clone(&document.uri),
            version: document.version,
            text: Arc::from(document.text.as_str()),
            is_dependency: document.is_dependency,
            object_name: document.object_name.clone(),
        })
        .collect()
}

fn snapshot_for_uri(
    snapshots: &HashMap<Arc<str>, Arc<abap_cache::AnalysisSnapshot>>,
    uri: &str,
) -> Result<Arc<abap_cache::AnalysisSnapshot>, String> {
    snapshots
        .get(uri)
        .cloned()
        .ok_or_else(|| format!("target snapshot was not built: {uri}"))
}

fn call_graph_export_json(target_uri: &str, graph: &ProjectCallGraph) -> Value {
    json!({
        "phase": "call_graph",
        "target_uri": target_uri,
        "project_node_count": graph.nodes.len(),
        "project_edge_count": graph.edges.len(),
        "nodes": graph.nodes.iter().map(call_graph_node_json).collect::<Vec<_>>(),
        "edges": graph.edges.iter().map(call_graph_edge_json).collect::<Vec<_>>(),
    })
}

fn call_graph_node_json(node: &CallGraphNode) -> Value {
    json!({
        "id": node.id.as_ref(),
        "kind": call_graph_node_kind(node.kind),
        "name": node.name.as_ref(),
        "qualified_name": node.qualified_name.as_ref(),
        "unit_uri": node.unit_uri.as_ref(),
        "decl_range": [node.decl_range.start, node.decl_range.end],
    })
}

fn call_graph_edge_json(edge: &CallGraphEdge) -> Value {
    json!({
        "source": edge.source.as_ref(),
        "target": edge.target.as_deref(),
        "edge_kind": call_graph_edge_kind(edge.edge_kind),
        "resolution_status": call_graph_resolution_status(edge.resolution_status),
        "target_name": edge.target_name.as_ref(),
        "source_range": [edge.source_range.start, edge.source_range.end],
    })
}

fn call_graph_node_kind(kind: CallGraphNodeKind) -> &'static str {
    match kind {
        CallGraphNodeKind::Method => "method",
        CallGraphNodeKind::Form => "form",
        CallGraphNodeKind::FunctionModule => "function_module",
        CallGraphNodeKind::EventBlock => "event_block",
        CallGraphNodeKind::Report => "report",
    }
}

fn call_graph_edge_kind(kind: CallGraphEdgeKind) -> &'static str {
    match kind {
        CallGraphEdgeKind::MethodCall => "method_call",
        CallGraphEdgeKind::Perform => "perform",
        CallGraphEdgeKind::FunctionCall => "function_call",
    }
}

fn call_graph_resolution_status(status: CallGraphResolutionStatus) -> &'static str {
    match status {
        CallGraphResolutionStatus::Resolved => "resolved",
        CallGraphResolutionStatus::Unresolved => "unresolved",
    }
}

fn parse_args(args: impl Iterator<Item = String>) -> Result<Config, String> {
    let mut corpus_root = env::var("ABAP_PERF_CORPUS")
        .map(PathBuf::from)
        .unwrap_or_else(|_| default_corpus_root());
    let mut parser_file: Option<PathBuf> = None;
    let mut workspace_target: Option<PathBuf> = None;
    let mut iterations = env::var("ABAP_PERF_BASELINE_ITERATIONS")
        .or_else(|_| env::var("ABAP_PERF_ITERATIONS"))
        .ok()
        .and_then(|raw| raw.parse::<usize>().ok())
        .filter(|value| *value > 0)
        .unwrap_or(DEFAULT_ITERATIONS);
    let mut warmup_iterations = env::var("ABAP_PERF_BASELINE_WARMUP")
        .or_else(|_| env::var("ABAP_PERF_WARMUP"))
        .ok()
        .and_then(|raw| raw.parse::<usize>().ok())
        .unwrap_or(DEFAULT_WARMUP_ITERATIONS);
    let mut pretty = false;
    let mut output = env::var("ABAP_PERF_BASELINE").ok().map(PathBuf::from);
    let mut call_dataflow_target = env::var("ABAP_PERF_CALL_DATAFLOW_TARGET")
        .unwrap_or_else(|_| DEFAULT_CALL_DATAFLOW_TARGET.to_string());
    let mut call_dataflow_caller = env::var("ABAP_PERF_CALL_DATAFLOW_CALLER")
        .ok()
        .or_else(|| Some(DEFAULT_CALL_DATAFLOW_CALLER.to_string()));

    let mut args = args.peekable();
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--corpus" => {
                let value = args
                    .next()
                    .ok_or_else(|| "--corpus requires a directory path".to_string())?;
                corpus_root = PathBuf::from(value);
            }
            "--parser-file" => {
                let value = args
                    .next()
                    .ok_or_else(|| "--parser-file requires a file path".to_string())?;
                parser_file = Some(PathBuf::from(value));
            }
            "--workspace-target" => {
                let value = args
                    .next()
                    .ok_or_else(|| "--workspace-target requires a file path".to_string())?;
                workspace_target = Some(PathBuf::from(value));
            }
            "--iterations" => {
                let value = args
                    .next()
                    .ok_or_else(|| "--iterations requires a number".to_string())?;
                iterations = parse_positive_usize(&value, "--iterations")?;
            }
            "--warmup" => {
                let value = args
                    .next()
                    .ok_or_else(|| "--warmup requires a number".to_string())?;
                warmup_iterations = parse_usize(&value, "--warmup")?;
            }
            "--output" => {
                let value = args
                    .next()
                    .ok_or_else(|| "--output requires a file path".to_string())?;
                output = Some(PathBuf::from(value));
            }
            "--call-dataflow-target" => {
                call_dataflow_target = args
                    .next()
                    .ok_or_else(|| "--call-dataflow-target requires a name".to_string())?;
            }
            "--call-dataflow-caller" => {
                call_dataflow_caller = Some(
                    args.next()
                        .ok_or_else(|| "--call-dataflow-caller requires a name".to_string())?,
                );
            }
            "--no-call-dataflow-caller" => {
                call_dataflow_caller = None;
            }
            "--pretty" => pretty = true,
            "--help" | "-h" => {
                print_usage();
                std::process::exit(0);
            }
            other => return Err(format!("unrecognized argument: {other}")),
        }
    }

    corpus_root = normalize_existing_dir(corpus_root, "--corpus")?;
    let parser_file = normalize_corpus_file(
        &corpus_root,
        parser_file.unwrap_or_else(|| PathBuf::from(DEFAULT_PARSER_FILE)),
        "--parser-file",
    )?;
    let workspace_target = normalize_corpus_file(
        &corpus_root,
        workspace_target.unwrap_or_else(|| PathBuf::from(DEFAULT_WORKSPACE_TARGET)),
        "--workspace-target",
    )?;

    Ok(Config {
        corpus_root,
        parser_file,
        workspace_target,
        iterations,
        warmup_iterations,
        pretty,
        output,
        call_dataflow_target,
        call_dataflow_caller,
    })
}

fn default_corpus_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
        .join("examples")
        .join("perf_corpus")
}

fn normalize_existing_dir(path: PathBuf, flag: &str) -> Result<PathBuf, String> {
    let path = path
        .canonicalize()
        .map_err(|err| format!("{flag} '{}' is not readable: {err}", path.display()))?;
    let path = strip_windows_verbatim_prefix(path);
    if !path.is_dir() {
        return Err(format!("{flag} '{}' is not a directory", path.display()));
    }
    Ok(path)
}

fn normalize_corpus_file(root: &Path, path: PathBuf, flag: &str) -> Result<PathBuf, String> {
    let path = if path.is_absolute() {
        path
    } else {
        root.join(path)
    };
    let path = path
        .canonicalize()
        .map_err(|err| format!("{flag} '{}' is not readable: {err}", path.display()))?;
    let path = strip_windows_verbatim_prefix(path);
    if !path.is_file() {
        return Err(format!("{flag} '{}' is not a file", path.display()));
    }
    Ok(path)
}

fn strip_windows_verbatim_prefix(path: PathBuf) -> PathBuf {
    #[cfg(windows)]
    {
        let raw = path.to_string_lossy();
        if let Some(rest) = raw.strip_prefix(r"\\?\UNC\") {
            return PathBuf::from(format!(r"\\{rest}"));
        }
        if let Some(rest) = raw.strip_prefix(r"\\?\") {
            return PathBuf::from(rest);
        }
    }
    path
}

fn parse_positive_usize(raw: &str, flag: &str) -> Result<usize, String> {
    let value = parse_usize(raw, flag)?;
    if value == 0 {
        return Err(format!("{flag} expects a positive integer, got '{raw}'"));
    }
    Ok(value)
}

fn parse_usize(raw: &str, flag: &str) -> Result<usize, String> {
    raw.parse::<usize>()
        .map_err(|_| format!("{flag} expects an unsigned integer, got '{raw}'"))
}

fn print_usage() {
    println!("Usage: cargo run -p abap_cli --example perf_baseline -- [options]");
    println!("Options:");
    println!("  --corpus <dir>                Benchmark corpus root");
    println!(
        "  --parser-file <path>          Standalone ABAP source, relative to corpus by default"
    );
    println!(
        "  --workspace-target <path>     Workspace ABAP target, relative to corpus by default"
    );
    println!("  --iterations <n>              Measured iterations (default: {DEFAULT_ITERATIONS})");
    println!(
        "  --warmup <n>                  Warmup iterations (default: {DEFAULT_WARMUP_ITERATIONS})"
    );
    println!("  --output <path>               Write JSON baseline to a file");
    println!("  --pretty                      Pretty-print JSON");
    println!("  --call-dataflow-target <name> Target function or method name");
    println!("  --call-dataflow-caller <name> Caller routine filter");
    println!("  --no-call-dataflow-caller     Clear caller filter");
    println!("Environment:");
    println!("  ABAP_PERF_CORPUS              Default corpus root");
    println!("  ABAP_PERF_BASELINE            Default output file");
    println!("  ABAP_PERF_BASELINE_ITERATIONS Default measured iterations");
    println!("  ABAP_PERF_BASELINE_WARMUP     Default warmup iterations");
}
