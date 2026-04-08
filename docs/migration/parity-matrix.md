# Parity And Benchmark Checklist

## Feature Parity

Track each area against the expected Rust server behavior and existing regression coverage:

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

- Expand parser tests in batches and note coverage status.
- Expand symbol-resolution tests after parser output stabilizes.
- Expand cache tests once snapshot publication APIs exist.
- Expand LSP tests after handler plumbing and custom notifications exist.

## Performance Benchmarks

Measure at least:

- lexer throughput on representative ABAP source files,
- parser throughput and allocation rate,
- symbol resolution latency per file,
- diagnostics latency after document change,
- full workspace refresh time,
- parallel batch analysis scaling across cores.

## Regression Gates

- no significant correctness regressions relative to current regression expectations,
- no accidental quadratic behavior on large files,
- no material slowdown in hot paths without a documented reason,
- deterministic outputs across repeated parallel runs.
