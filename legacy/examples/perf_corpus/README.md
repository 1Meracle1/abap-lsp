# ABAP Performance Corpus

This directory is a committed benchmark corpus for repeatable local performance baselines.

- `single-file/ZPERF_PARSER_MIXED.abap` is a mixed standalone source used for parser, symbol, and semantic-token benchmarks.
- `abapls.toml` plus `src/` and `dependencies/` form a small workspace used for workspace loading, call graph, and call dataflow export benchmarks.
- `local-export-workspace/` plus `local-export-root/` form a small workspace/local-export pair used to exercise local-export dependency closure, candidate caching, and expanded `DocumentStore` analysis without private SAP source.

The corpus is synthetic but intentionally exercises report events, includes, forms, Open SQL host variables, table work areas, function-module signatures, cross-unit calls, and field-level argument provenance.
