use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Instant;

use abap_parser::parse;

use crate::collector::collect_unit;
use crate::ids::UnitId;
use crate::project::{ProjectAnalysis, analyze_unit};
use crate::resolver::{build_scope_index, resolve_unit_with_index};
use crate::validate::validate_project_with_scope_indexes;

const DEFAULT_SAMPLE_PATH: &str =
    r"D:\dev\abap\lsp_development_examples2\.abapls\cache\sources\%2FSTTP%2FCL_MD_BPARTNER.abap";
const PERF_SAMPLE_URI: &str = "file:///perf_sample.abap";

fn perf_sample_path() -> PathBuf {
    env::var("ABAP_PERF_SAMPLE")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from(DEFAULT_SAMPLE_PATH))
}

fn load_perf_sample() -> (PathBuf, String) {
    let path = perf_sample_path();
    let source = fs::read_to_string(&path)
        .unwrap_or_else(|err| panic!("failed to read perf sample '{}': {err}", path.display()));
    (path, source)
}

#[test]
#[ignore = "manual large-file performance check"]
fn large_file_phase_breakdown() {
    let (path, source) = load_perf_sample();
    let line_count = source.lines().count();
    assert!(
        line_count >= 10_000,
        "expected a large ABAP sample, got only {line_count} lines from '{}'",
        path.display()
    );

    let parse_start = Instant::now();
    let parsed = parse(&source);
    let parse_elapsed = parse_start.elapsed();

    let collect_start = Instant::now();
    let mut unit = collect_unit(
        UnitId(0),
        Arc::from(PERF_SAMPLE_URI),
        &source,
        &parsed.file,
        &parsed.tokens,
    );
    let collect_elapsed = collect_start.elapsed();

    let scope_index = build_scope_index(&unit);
    let resolve_start = Instant::now();
    resolve_unit_with_index(&mut unit, &scope_index);
    let resolve_elapsed = resolve_start.elapsed();

    let mut project = ProjectAnalysis {
        units: vec![unit],
        uri_to_unit: HashMap::from([(Arc::from(PERF_SAMPLE_URI), UnitId(0))]),
        provided_name_to_unit: HashMap::new(),
        diagnostics: Vec::new(),
    };
    let validate_start = Instant::now();
    validate_project_with_scope_indexes(&mut project, &[scope_index]);
    let validate_elapsed = validate_start.elapsed();

    let full_symbols_start = Instant::now();
    let full_unit = analyze_unit(PERF_SAMPLE_URI, &source, &parsed);
    let full_symbols_elapsed = full_symbols_start.elapsed();

    let unit = &project.units[0];
    eprintln!(
        concat!(
            "large-file perf sample: {}\n",
            "bytes={} lines={} tokens={} parse_errors={}\n",
            "parse={:?}\n",
            "collect={:?}\n",
            "resolve={:?}\n",
            "validate={:?}\n",
            "symbols_total={:?}\n",
            "symbols={} references={} diagnostics={}\n"
        ),
        path.display(),
        source.len(),
        line_count,
        parsed.tokens.len(),
        parsed.errors.len(),
        parse_elapsed,
        collect_elapsed,
        resolve_elapsed,
        validate_elapsed,
        full_symbols_elapsed,
        full_unit.symbols.len(),
        full_unit.references.len(),
        unit.diagnostics.len()
    );

    assert_eq!(full_unit.symbols.len(), unit.symbols.len());
    assert_eq!(full_unit.references.len(), unit.references.len());
}
