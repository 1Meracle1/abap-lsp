use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Instant;

use abap_parser::parse;

use crate::collector::collect_unit;
use crate::ids::UnitId;
use crate::project::{ProjectAnalysis, analyze_project_from_units, analyze_unit};
use crate::resolver::{build_scope_index, resolve_unit_with_index};
use crate::routine_analysis::{RoutineInstructionKind, build_project_routine_analysis};
use crate::static_analysis::{StaticAnalysisFindingKind, build_project_static_analysis_summary};
use crate::validate::validate_project_with_scope_indexes;

const DEFAULT_SAMPLE_PATH: &str = r"D:\dev\abap\prod_rep_check\perf-samples\CL_GUI_ALV_GRID.abap";
const PERF_SAMPLE_URI: &str = "file:///perf_sample.abap";

fn load_perf_sample() -> (PathBuf, String) {
    let path = env::var("ABAP_PERF_SAMPLE")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from(DEFAULT_SAMPLE_PATH));
    let source = fs::read_to_string(&path)
        .unwrap_or_else(|err| panic!("failed to read perf sample '{}': {err}", path.display()));
    (path, source)
}

#[test]
#[ignore = "manual large-file performance check"]
fn large_file_phase_breakdown() {
    let (path, source) = load_perf_sample();
    let line_count = source.lines().count();

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

#[test]
fn call_dense_routine_ir_suppresses_nested_call_argument_reads() {
    let call_count = 256;
    let source = call_dense_source(call_count, false);
    let parsed = parse(&source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);

    let unit = analyze_unit(PERF_SAMPLE_URI, &source, &parsed);
    let project = analyze_project_from_units(vec![unit.clone()]);
    let routine_analysis = build_project_routine_analysis(&project);
    let start_of_selection = routine_analysis
        .routines
        .iter()
        .find(|routine| routine.descriptor.name.as_ref() == "start-of-selection")
        .expect("start-of-selection routine");

    let call_instructions = start_of_selection
        .ir
        .instructions
        .iter()
        .filter(|instruction| instruction.kind() == RoutineInstructionKind::Call)
        .count();
    let value_read_instructions = start_of_selection
        .ir
        .instructions
        .iter()
        .filter(|instruction| instruction.kind() == RoutineInstructionKind::ValueRead)
        .count();

    assert_eq!(call_instructions, call_count);
    assert_eq!(
        value_read_instructions, 0,
        "call argument references should be modeled by call instructions, not duplicate value reads"
    );
    assert!(
        routine_analysis
            .diagnostics_for_unit(unit.unit_id)
            .is_empty(),
        "{:#?}",
        routine_analysis.diagnostics_for_unit(unit.unit_id)
    );
}

#[test]
fn call_dense_static_analysis_preserves_unreachable_diagnostic() {
    let call_count = 128;
    let source = call_dense_source(call_count, true);
    let parsed = parse(&source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);

    let unit = analyze_unit(PERF_SAMPLE_URI, &source, &parsed);
    let project = analyze_project_from_units(vec![unit.clone()]);
    let routine_analysis = build_project_routine_analysis(&project);
    let static_analysis = build_project_static_analysis_summary(&project, &routine_analysis);

    let routine_diagnostics = routine_analysis.diagnostics_for_unit(unit.unit_id);
    assert!(
        routine_diagnostics.iter().any(|diagnostic| {
            diagnostic.kind == crate::DiagnosticKind::UnreachableCode
                && source[diagnostic.range.clone()].contains("WRITE 'unreachable'")
        }),
        "{routine_diagnostics:#?}"
    );

    let findings: Vec<_> = static_analysis
        .routines_for_unit(unit.unit_id)
        .flat_map(|routine| routine.findings.iter())
        .collect();
    assert!(
        findings.iter().any(|finding| {
            finding.kind == StaticAnalysisFindingKind::UnreachableCode
                && source[finding.range.clone()].contains("WRITE 'unreachable'")
        }),
        "{findings:#?}"
    );
    assert_eq!(
        static_analysis.metrics.routine_count,
        routine_analysis.routines.len()
    );
}

#[test]
#[ignore = "manual synthetic large-file routine/static-analysis performance check"]
fn synthetic_call_dense_routine_static_analysis_perf() {
    let call_count = 2_000;
    let source = call_dense_source(call_count, true);

    let parse_start = Instant::now();
    let parsed = parse(&source);
    let parse_elapsed = parse_start.elapsed();
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);

    let analyze_start = Instant::now();
    let unit = analyze_unit(PERF_SAMPLE_URI, &source, &parsed);
    let analyze_elapsed = analyze_start.elapsed();

    let project = analyze_project_from_units(vec![unit.clone()]);

    let routine_start = Instant::now();
    let routine_analysis = build_project_routine_analysis(&project);
    let routine_elapsed = routine_start.elapsed();

    let static_start = Instant::now();
    let static_analysis = build_project_static_analysis_summary(&project, &routine_analysis);
    let static_elapsed = static_start.elapsed();

    let start_of_selection = routine_analysis
        .routines
        .iter()
        .find(|routine| routine.descriptor.name.as_ref() == "start-of-selection")
        .expect("start-of-selection routine");
    let call_instructions = start_of_selection
        .ir
        .instructions
        .iter()
        .filter(|instruction| instruction.kind() == RoutineInstructionKind::Call)
        .count();

    eprintln!(
        concat!(
            "synthetic call-dense perf: bytes={} calls={} parse={:?} analyze_unit={:?} ",
            "routine={:?} static={:?} routine_total={}us ir={}us cfg={}us dataflow={}us ",
            "static_total={}us static_index={}us diagnostics={}"
        ),
        source.len(),
        call_count,
        parse_elapsed,
        analyze_elapsed,
        routine_elapsed,
        static_elapsed,
        routine_analysis.metrics.total_micros,
        routine_analysis.metrics.ir_micros,
        routine_analysis.metrics.cfg_micros,
        routine_analysis.metrics.dataflow_micros,
        static_analysis.metrics.total_micros,
        static_analysis.metrics.index_micros,
        routine_analysis.diagnostics_for_unit(unit.unit_id).len()
    );

    assert_eq!(call_instructions, call_count);
    assert!(static_analysis.metrics.finding_count >= 1);
}

fn call_dense_source(call_count: usize, include_unreachable_tail: bool) -> String {
    let mut source = String::from(
        "\
CLASS lcl_sink DEFINITION.\n\
  PUBLIC SECTION.\n\
    CLASS-METHODS touch IMPORTING iv_value TYPE i.\n\
ENDCLASS.\n\
\n\
CLASS lcl_sink IMPLEMENTATION.\n\
  METHOD touch.\n\
  ENDMETHOD.\n\
ENDCLASS.\n\
\n\
START-OF-SELECTION.\n\
  DATA lv_value TYPE i VALUE 1.\n",
    );

    for _ in 0..call_count {
        source.push_str("  lcl_sink=>touch( iv_value = lv_value ).\n");
    }

    if include_unreachable_tail {
        source.push_str("  STOP.\n  WRITE 'unreachable'.\n");
    }

    source
}
