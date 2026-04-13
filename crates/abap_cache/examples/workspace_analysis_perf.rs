use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::sync::Arc;
use std::time::Instant;

use abap_cache::{DocumentInput, DocumentStore, load_workspace_documents, path_to_file_uri};

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
    let config = parse_args(std::env::args().skip(1))?;
    let workspace_uri = path_to_file_uri(&config.workspace_root);

    let load_start = Instant::now();
    let workspace = load_workspace_documents(&workspace_uri, &HashMap::new());
    let load_elapsed = load_start.elapsed();

    let dependency_unit_count = workspace
        .documents
        .iter()
        .filter(|document| document.is_dependency)
        .count();

    let inputs: Vec<_> = workspace
        .documents
        .iter()
        .map(|document| DocumentInput {
            uri: Arc::clone(&document.uri),
            version: document.version,
            text: Arc::from(document.text.as_str()),
            is_dependency: document.is_dependency,
            object_name: document.object_name.clone(),
        })
        .collect();

    let store = DocumentStore::default();
    let analyze_start = Instant::now();
    let snapshots = store.replace_all(inputs);
    let analyze_elapsed = analyze_start.elapsed();

    if let Some(target_file) = config.target_file {
        let target_uri = path_to_file_uri(&target_file);
        if !snapshots.contains_key(target_uri.as_str()) {
            return Err(format!(
                "analysis did not include target '{}'",
                target_file.display()
            ));
        }
    }

    let metrics = store
        .last_analysis_metrics_snapshot()
        .ok_or_else(|| "analysis metrics were not recorded".to_string())?;

    println!("workspace_root={}", config.workspace_root.display());
    println!("documents={}", workspace.documents.len());
    println!("dependency_documents={dependency_unit_count}");
    println!("manifest_present={}", workspace.manifest.is_some());
    println!("load_workspace={load_elapsed:?}");
    println!("replace_all_total={analyze_elapsed:?}");
    println!("parse_count={}", metrics.parse_count);
    println!("local_phase_count={}", metrics.local_phase_count);
    println!("dirty_uri_count={}", metrics.dirty_uri_count);
    println!(
        "prepare_documents_total={:?}",
        std::time::Duration::from_micros(metrics.local_phase_micros as u64)
    );
    println!(
        "project_update={:?}",
        std::time::Duration::from_micros(metrics.project_update_micros as u64)
    );
    println!(
        "snapshot_build={:?}",
        std::time::Duration::from_micros(metrics.snapshot_build_micros as u64)
    );

    Ok(())
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
    if !Path::new(&workspace_root).is_dir() {
        return Err(format!(
            "workspace root does not exist or is not a directory: '{}'",
            workspace_root.display()
        ));
    }
    if let Some(target_file) = target_file.as_ref()
        && !Path::new(target_file).is_file()
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
        "Usage: cargo run -p abap_cache --example workspace_analysis_perf -- --workspace <dir> [--target <file>]"
    );
}
