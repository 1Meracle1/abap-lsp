# Performance Baselines

The repository includes a committed ABAP benchmark corpus at `examples/perf_corpus` and a JSON
baseline runner:

```bat
cargo run -p abap_cli --example perf_baseline --release -- --pretty
```

The runner emits one `abap.performance_baseline` JSON document with measured timings in
microseconds and stable workload counters for:

- `parser.parse_file`
- `symbols.analyze_unit`
- `semantic_tokens.build`
- `workspace.load_documents`
- `workspace.replace_all_full`
- `call_graph.export_json`
- `call_dataflow.export_json`

`perf_test.bat` also runs the raw workspace and local-export examples against the committed
synthetic local-export corpus so regressions in dependency-closure instrumentation remain visible
without relying on a private SAP checkout.

Each benchmark records `min_micros`, `median_micros`, `mean_micros`, `max_micros`, the sorted
sample list, and a `last` object with counters such as token count, symbol count, document count,
project diagnostics, call graph nodes/edges, dataflow mappings, and serialized JSON size.

## Corpus Layout

`examples/perf_corpus` is synthetic but intentionally shaped like a small real workspace:

- `single-file/ZPERF_PARSER_MIXED.abap`: standalone mixed source for parser, symbol, and
  semantic-token measurements.
- `abapls.toml`: manifest using `full-workspace` mode.
- `src/reports/ZPERF_DRIVER`: report with includes, event blocks, forms, a screen module, and a
  selected `CALL FUNCTION` site.
- `src/classes/ZCL_PERF_SERVICE.abap`: cross-unit static method calls used by the report include.
- `dependencies/BAPI_PO_CREATE1.abap`: dependency function-module signature used for call graph and
  call-dataflow export measurements.
- `local-export-workspace/` and `local-export-root/`: a deterministic local-export workload where
  one editable report expands through transitive class and DDIC references.

Keep this corpus deterministic. Do not put SAP customer code, credentials, generated exports, or
machine-local paths in it.

## Local-Export Regression Surface

The private workspace that motivated the latest tuning is fast when analyzed as a raw local
workspace because it only has 8 editable ABAP files, no manifest-loaded dependency documents, and
about 15 KB of direct source. In that mode `workspace_analysis_perf` feeds only those editable units
to `DocumentStore`; the saved run on this machine finished `replace_all_total` in about 5.3 ms with
`parse_count=8`, `local_phase_count=8`, and `dependency_documents=0`.

The editor local-export path is a different workload. A unit sidecar can point at a broad local SAP
export root. The LSP starts from editable references, resolves each candidate into local-export
artifacts, extracts candidates from those dependency artifacts, and repeats until the dependency
closure stops growing. On the target private workspace, 8 editable files expanded to 3,865 snapshots
with 3,857 dependency snapshots and 24,668 produced candidates.

Use these metrics to tell where time went:

- Closure collection before `DocumentStore`: `*_local_export_closure_total`,
  `*_local_export_documents_examined_for_candidates`,
  `*_local_export_candidate_cache_hits`, `*_local_export_candidate_cache_misses`,
  `*_local_export_document_read_cache_hits`, `*_local_export_candidate_collection_wall`,
  `*_local_export_candidate_parse_analyze_time`, and
  `*_local_export_added_dependency_documents`.
- Expanded `DocumentStore` analysis: `*_parse_count`, `*_local_phase_count`,
  `*_dependency_projection`, `*_project_update`, `*_resolve_cross_unit`, `*_validate`,
  `*_snapshot_build`, `*_diagnostic_scope_unit_count`, and `*_validation_unit_count`.

## Private Workspace Rerun

Keep private paths in environment variables or ignored `tmp/` outputs:

```powershell
$env:ABAP_LSP_PERF_WORKSPACE = "<private workspace root>"
$env:ABAP_LSP_PERF_TARGET = "<private target ABAP file>"
$env:ABAP_LSP_PERF_DEPENDENCY_STORE = "target\perf-private\dependency-cache.sqlite3"

cargo run -p abap_cache --example workspace_analysis_perf --release -- `
  --workspace $env:ABAP_LSP_PERF_WORKSPACE `
  --target $env:ABAP_LSP_PERF_TARGET

cargo run -p abap_lsp --example workspace_local_export_perf --release -- `
  --workspace $env:ABAP_LSP_PERF_WORKSPACE `
  --target $env:ABAP_LSP_PERF_TARGET

cargo run -p abap_lsp --example workspace_incremental_path_perf --release -- `
  --workspace $env:ABAP_LSP_PERF_WORKSPACE `
  --target $env:ABAP_LSP_PERF_TARGET `
  --dependency-store $env:ABAP_LSP_PERF_DEPENDENCY_STORE

cargo run -p abap_lsp --example remote_dependency_wave_perf --release -- `
  --workspace $env:ABAP_LSP_PERF_WORKSPACE `
  --source $env:ABAP_LSP_PERF_TARGET `
  --symbol main_processing
```

Do not commit the generated private benchmark output. Save it under `tmp/` or `target/`.

Expected private-workspace results from this Windows machine, collected 2026-04-28 after release
builds were already warm:

| Metric | Before tuning | After local-export cache | After editor diagnostic scope |
| --- | ---: | ---: | ---: |
| Raw `workspace_analysis_perf replace_all_total` | 5.3 ms | unchanged | unchanged |
| `editor_workspace_elapsed` | 6.705 s | 2.935 s | 2.683 s |
| `editor_workspace_local_export_closure_total` | 4.365 s | 0.921 s | 1.140 s |
| `editor_workspace_local_export_candidate_parse_analyze_time` | 6.785 s summed worker time | 0 ns warm/editor | 0 ns warm/editor |
| `editor_workspace_project_update` | 1.698 s | 1.462 s | 0.798 s |
| `editor_workspace_validate` | 1.099 s | 1.024 s | 0.254 s |
| `editor_workspace_validation_unit_count` | all 3,865 units | all 3,865 units | 8 units |

Treat these as same-machine guardrails, not portable thresholds. Antivirus, filesystem cache state,
CPU power mode, local-export root size, and dependency-store contents can move wall-clock numbers.
Stable workload counters such as snapshot counts, dependency snapshot counts, candidate counts,
cache hits/misses, and validation unit counts should remain comparable.

## Running From `perf_test.bat`

`perf_test.bat` runs the JSON baseline first, then the deterministic local-export corpus examples,
the existing ignored smoke checks, and optional large-file checks:

```bat
.\perf_test.bat
.\perf_test.bat release
```

Write the JSON to a file:

```bat
set ABAP_PERF_BASELINE=target\perf-baselines\baseline.json
.\perf_test.bat release
```

The older large-file hooks are still available:

```bat
set ABAP_PERF_SAMPLE=D:\path\to\large.abap
.\perf_test.bat release
```

## Runner Options

Direct invocation supports:

```bat
cargo run -p abap_cli --example perf_baseline --release -- ^
  --iterations 10 ^
  --warmup 2 ^
  --pretty ^
  --output target\perf-baselines\baseline.json
```

Useful options:

- `--corpus <dir>` or `ABAP_PERF_CORPUS`: alternate corpus root.
- `--parser-file <path>`: standalone ABAP file, relative to the corpus unless absolute.
- `--workspace-target <path>`: workspace target, relative to the corpus unless absolute.
- `--iterations <n>` or `ABAP_PERF_BASELINE_ITERATIONS`: measured iterations.
- `--warmup <n>` or `ABAP_PERF_BASELINE_WARMUP`: warmup iterations.
- `--output <path>` or `ABAP_PERF_BASELINE`: write JSON instead of printing it.
- `--call-dataflow-target <name>` and `--call-dataflow-caller <name>`: override the selected
  dataflow query.

`ABAP_PERF_ITERATIONS` and `ABAP_PERF_WARMUP` remain fallback defaults so the runner can share the
same environment as the existing perf examples.

## Baseline Workflow

Use release mode for comparable numbers:

1. Run `.\perf_test.bat release` before the change and store the JSON under `target/`.
2. Run it again after the change on the same machine with the same iteration count.
3. Compare medians first, then means and sample spread.
4. Treat counter changes as semantic workload changes. For example, different call graph edge
   counts or dataflow mapping counts mean the timed surface changed, not just its speed.
5. Keep generated baseline JSON under `target/` or another ignored location unless a review
   explicitly asks for the artifact.
