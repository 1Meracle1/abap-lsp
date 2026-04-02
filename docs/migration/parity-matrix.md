# Parity And Benchmark Checklist

## Feature Parity

Track each area against the legacy implementation in `legacy/`:

- JSON-RPC transport and shutdown behavior
- document open/change lifecycle
- hover
- completion
- diagnostics
- semantic tokens
- manifest loading
- remote dependency candidate generation
- remote dependency update handling
- cache invalidation and republish behavior
- workspace initialization and multi-folder handling

## Test Parity

- Port `legacy/tests/parser/` in batches and note coverage status.
- Port `legacy/tests/symbols/` after parser output stabilizes.
- Port `legacy/tests/cache/` once snapshot publication APIs exist.
- Port `legacy/tests/lsp/` after handler plumbing and custom notifications exist.

## Performance Benchmarks

Measure at least:

- lexer throughput on representative ABAP source files,
- parser throughput and allocation rate,
- symbol resolution latency per file,
- diagnostics latency after document change,
- full workspace refresh time,
- parallel batch analysis scaling across cores.

## Regression Gates

- no significant correctness regressions relative to legacy test expectations,
- no accidental quadratic behavior on large files,
- no material slowdown in hot paths without a documented reason,
- deterministic outputs across repeated parallel runs.
