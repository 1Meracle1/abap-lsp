# ABAP LSP Rust Workspace

This repository is now organized as a Rust-first workspace for the ABAP language server rewrite.
The VS Code extension lives in `editors/vscode/` and targets the Rust server workspace.

## Repository Layout

- `crates/abap_adt_cli`: ADT query CLI for remote SAP search, source fetches, DDIC lookups, and child-object discovery.
- `crates/abap_jsonrpc`: blocking JSON-RPC framing utilities.
- `crates/abap_lexer`: tokenization and source ranges.
- `crates/abap_ast`: syntax tree model and shared node types.
- `crates/abap_parser`: syntax-only parser built on lexer + AST.
- `crates/abap_symbols`: symbol indexing, name resolution, and validation scaffolding.
- `crates/abap_cache`: immutable snapshot publication and shared workspace state.
- `crates/abap_lsp`: LSP protocol types, custom notifications, and handler scaffolding.
- `crates/abap_lsp_server`: blocking server binary entry point.
- `docs/`: migration, architecture, parity, benchmarking, and tool usage guidance.
- `editors/vscode/`: editor client integration and ADT-mediated remote dependency fetches.

## Build

Debug build:

```bat
.\build.bat
```

Release build:

```bat
.\build.bat release
```

Build a single package:

```bat
.\build.bat -p abap_lsp_server
```

## Test

```bat
.\test.bat
```

## Performance test

```bat
.\perf_test.bat

$env:CARGO_PROFILE_RELEASE_DEBUG="true"
$env:ABAP_PERF_ITERATIONS="50"
$env:ABAP_PERF_WARMUP="1"

cargo flamegraph -p abap_symbols --example build_symbols_collect_perf --release
cargo flamegraph -p abap_symbols --example build_symbols_validate_perf --release
```

## Migration Docs

- `docs/abap-adt-cli.md`
- `docs/semantic-dossier.md`
- `docs/migration/repo-layout.md`
- `docs/migration/frontend-porting.md`
- `docs/architecture/concurrency.md`
- `docs/architecture/remote-dependencies.md`
- `docs/migration/parity-matrix.md`

## Semantic Dossier CLI

Use `abap_cli analyze` when you need one compact machine-readable export for an ABAP file or
workspace object.

```bat
cargo run -p abap_cli -- analyze --json path\to\zcl_demo.abap
cargo run -p abap_cli -- analyze --json --with-project path\to\zcl_demo.abap
cargo run -p abap_cli -- analyze --json --with-project --pretty path\to\zcl_demo.abap
```

The dossier is designed for downstream tools and AI workflows. It includes:

- target metadata and project context,
- parse and semantic diagnostics,
- declared symbols, scopes, and class facts,
- references with resolutions when available,
- call sites and perform calls,
- Open SQL queries, touched objects, and targets,
- include edges and unresolved-name buckets,
- a summary section with stable counts.

See `docs/semantic-dossier.md` for the JSON schema and usage notes.

## Remote SAP Lookup

Use `abap-adt` when you need live SAP information that is not available in the local workspace yet.

- Search repository objects on the remote SAP system.
- Fetch ABAP source for reports, includes, classes, interfaces, function groups, and function modules.
- Fetch DDIC metadata for data elements, table types, structures, views, and tables.
- Inspect child objects for packages, reports, and function groups.

See `docs/abap-adt-cli.md` for command shapes, environment variables, `.env` loading, and examples.

## Current Status

The Rust workspace currently provides:

- a blocking ADT query CLI for remote SAP discovery and source lookups,
- a semantic dossier CLI export for AI-oriented ABAP object analysis,
- the crate layout and dependency boundaries for the rewrite,
- a minimal blocking JSON-RPC transport,
- a smoke-test LSP server binary over stdio,
- architecture and migration documents that track concurrency, SAP transport, and parity requirements.
