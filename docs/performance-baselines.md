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

Keep this corpus deterministic. Do not put SAP customer code, credentials, generated exports, or
machine-local paths in it.

## Running From `perf_test.bat`

`perf_test.bat` runs the JSON baseline first, then the existing ignored smoke checks and optional
large-file checks:

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
