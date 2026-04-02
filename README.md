# ABAP LSP Rust Workspace

This repository is now organized as a Rust-first workspace for the ABAP language server rewrite.

The previous Odin implementation has been archived under `legacy/` so it remains buildable, testable, and available for migration reference. The VS Code extension stays at `editors/vscode/` and is intended to work against either the archived Odin server or the new Rust server during the transition.

## Repository Layout

- `crates/abap_jsonrpc`: blocking JSON-RPC framing utilities.
- `crates/abap_lexer`: tokenization and source ranges.
- `crates/abap_ast`: syntax tree model and shared node types.
- `crates/abap_parser`: syntax-only parser built on lexer + AST.
- `crates/abap_symbols`: symbol indexing, name resolution, and validation scaffolding.
- `crates/abap_cache`: immutable snapshot publication and shared workspace state.
- `crates/abap_lsp`: LSP protocol types, custom notifications, and handler scaffolding.
- `crates/abap_lsp_server`: blocking server binary entry point.
- `docs/`: migration, architecture, parity, and benchmarking guidance.
- `legacy/`: archived Odin source tree, tests, build scripts, docs, and IDE config.
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

## Migration Docs

- `docs/migration/repo-layout.md`
- `docs/migration/frontend-porting.md`
- `docs/architecture/concurrency.md`
- `docs/architecture/remote-dependencies.md`
- `docs/migration/parity-matrix.md`

## Legacy Odin Implementation

The Odin codebase has been moved into `legacy/`. Use the archived README and scripts there if you need to compare behavior or run the old implementation:

- `legacy/README.md`
- `legacy/build.bat`
- `legacy/test.bat`

## Current Status

The Rust workspace currently provides:

- the crate layout and dependency boundaries for the rewrite,
- a minimal blocking JSON-RPC transport,
- a smoke-test LSP server binary over stdio,
- architecture and migration documents that track concurrency, SAP transport, and parity requirements.
