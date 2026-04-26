# ABAP LSP

ABAP LSP is a Rust workspace for local ABAP source analysis, editor language
features, and machine-readable exports for downstream tools.

The project is a source-first ABAP language server and toolchain. It is not a
replacement for SAP ADT today. The current focus is fast local parsing,
semantic indexing, cross-file navigation, remote dependency discovery, and
JSON/graph exports that make ABAP systems easier to inspect from scripts,
editors, and AI-assisted workflows.

## What It Provides

- A Rust language server binary, `abap_lsp_server`, for VS Code and other LSP
  clients.
- A VS Code client under `editors/vscode/` with ABAP syntax support, LSP
  startup, workspace commands, SAP connection configuration, remote dependency
  fetches, and virtual cached dependency documents.
- `abap-cli`, a local analysis CLI for lexing, parsing, checking, symbol
  indexing, semantic dossiers, effective source expansion, call graphs, and
  call-dataflow traces.
- `abap-adt`, a SAP ADT query CLI for repository search, source fetches, DDIC
  metadata fetches, and child-object discovery.
- A local workspace model based on `abapls.toml`, `src/` conventions,
  optional `abapls-unit.toml` sidecars, local exported SAP source roots, and a
  centralized SQLite dependency store.

## Current Editor Features

The LSP server currently advertises:

- diagnostics from parse, semantic, type, call, Open SQL, include, and routine
  analysis,
- semantic tokens,
- hover,
- completion,
- go to definition,
- references,
- rename and prepare rename,
- inlay hints,
- code actions for supported diagnostics,
- background workspace analysis with progress notifications,
- on-demand remote dependency resolution through the VS Code client.

The implementation is still under active development. Expect useful behavior on
common custom ABAP source, but not full ABAP platform parity.

## CLI Tools

### `abap-cli`

Use this for local source analysis and exports:

```bat
cargo run -p abap_cli -- check examples\report_minimal.abap
cargo run -p abap_cli -- parse --json --ast examples\data_typed.abap
cargo run -p abap_cli -- analyze --json --with-project path\to\zcl_demo.abap
cargo run -p abap_cli -- call-graph --json --symbol zcl_demo~run path\to\zcl_demo.abap
cargo run -p abap_cli -- call-dataflow --target BAPI_PO_CREATE1 path\to\report.abap
cargo run -p abap_cli -- remote-candidates --json path\to\workspace
```

The two most important machine-readable exports are:

- semantic dossier: symbols, references, scopes, classes, function modules,
  SQL facts, include edges, unresolved names, static-analysis summaries, and
  stable counts,
- call graph and call-dataflow: project-scale caller/callee graphs and
  selected-call parameter provenance.

See [docs/semantic-dossier.md](docs/semantic-dossier.md),
[docs/call-graph.md](docs/call-graph.md), and
[docs/call-dataflow.md](docs/call-dataflow.md).

### `abap-adt`

Use this when live SAP information is needed:

```bat
cargo run -p abap_adt_cli -- search "MARA"
cargo run -p abap_adt_cli -- get source class zcl_demo
cargo run -p abap_adt_cli -- get source function-module BAPI_USER_GET_DETAIL --group SUSR
cargo run -p abap_adt_cli -- get ddic table mara --raw
cargo run -p abap_adt_cli -- children package zpackage
```

Connection values can come from CLI flags, environment variables, or a
repository-local `.env` file. See [docs/abap-adt-cli.md](docs/abap-adt-cli.md).

## Workspace Model

The preferred local layout is:

```text
workspace/
  abapls.toml
  src/
    reports/
    function-groups/
    classes/
    interfaces/
    includes/
```

`abapls.toml` can be small and settings-only. When explicit units are absent,
the server discovers reports, function groups, classes, interfaces, includes,
and other `.abap` files from `src/` conventions.

Use `abapls-unit.toml` sidecars when a unit needs explicit members, include
name mappings, local exported SAP roots, or dependency source preferences.

Remote dependency state is stored in a centralized SQLite cache instead of
materializing every fetched dependency into the project tree. Cached dependency
documents are opened through the `abapls-cache:` URI scheme.

See [docs/workspace-layout.md](docs/workspace-layout.md).

## Project Goals

The long-term goal is a practical ABAP language server and analysis toolkit
that can:

- understand local ABAP workspaces without requiring a live SAP system for
  every editor operation,
- fetch missing repository and DDIC dependencies on demand through SAP ADT,
- provide fast editor feedback for large ABAP codebases,
- expose stable JSON exports for CI checks, migration tooling, and AI agents,
- model call flow, data flow, Open SQL usage, includes, class relationships,
  and common ABAP runtime conventions conservatively instead of guessing.

Non-goals for the current Rust rewrite:

- replacing SAP activation, transport, debugging, profiling, or repository
  administration,
- complete ABAP Platform, CDS, RAP, Dynpro, and DDIC editor parity,
- implementing the core server on top of an async runtime,
- treating dynamic ABAP, generated code, or macro-heavy code as fully precise
  when the analyzer cannot prove it.

## Status And Coverage

This is an early alpha Rust rewrite with usable pieces. These percentages are
engineering estimates as of April 2026, not formal conformance metrics.

| Area | Estimated coverage | Notes |
| --- | ---: | --- |
| Workspace and LSP plumbing | 55-65% | Stdio/TCP server, VS Code client, background analysis, diagnostics, navigation, rename, semantic tokens, dependency notifications, and cache documents are wired. Packaging and polish are still incomplete. |
| Parser coverage for common ABAP source | 45-55% | Handles many declarations, reports, includes, forms, methods, classes, interfaces, control-flow blocks, expressions, assignments, Open SQL SELECT, and common table/string statements. Many ABAP statement variants still parse conservatively or need grammar work. |
| Semantic analysis | 35-45% | Symbol collection, scope modeling, includes, class facts, references, type/value facts, calls, Open SQL facts, validation, and routine diagnostics exist. Dynamic dispatch, macro-heavy flow, full DDIC semantics, and broad interprocedural precision remain limited. |
| Static analysis | 30-40% | Current findings include unreachable code, use before definite assignment, possibly unbound field symbols, and dead stores, with compact summaries in semantic dossiers. |
| SAP integration | 35-45% | Repository search, source/DDIC fetches, child discovery, local export fallback, and centralized dependency caching exist. Writeback, activation, debugger, transports, and full ADT object editing are out of scope today. |
| Complete ABAP language and platform parity | 25-30% | The project is source-analysis first. Full SAP ADT-level coverage would include many object editors, server-side services, CDS/RAP, debugger, activation, transports, and quality tooling that are not implemented here. |

Read the ranges as a map of where the project is useful, not a promise that
every construct in that bucket works.

## Comparison To Adjacent Tools

| Tool | Best fit | How this project differs |
| --- | --- | --- |
| [SAP ABAP Development Tools for Eclipse](https://help.sap.com/docs/btp/sap-business-technology-platform/eclipse-tool-for-abap-environment) | Official full ABAP IDE with server-backed editing, syntax check, activation, debugging, navigation, refactoring, testing, and SAP platform integration. | ABAP LSP is local/source-first and independent of Eclipse. It can use ADT for lookup and dependency fetches, but it does not replace ADT for production SAP lifecycle operations. |
| [SAP ABAP Development Tools for VS Code](https://community.sap.com/t5/technology-blog-posts-by-sap/abap-development-tools-for-vs-code-everything-you-need-to-know/bc-p/14261964) | Announced official SAP VS Code direction. | This repository is independent. It focuses on Rust LSP internals, local workspace analysis, semantic exports, and dependency caching rather than official SAP IDE parity. |
| [abaplint](https://github.com/abaplint/abaplint) and [vscode-abaplint](https://github.com/abaplint/vscode-abaplint) | Mature TypeScript static analysis, linting, rule enforcement, CI checks, and VS Code diagnostics for ABAP projects. | ABAP LSP focuses on Rust performance, LSP navigation, semantic dossiers, call graph/dataflow exports, and ADT-backed dependency resolution. abaplint is the stronger choice today for established lint rules and CI policy. |
| [ABAP remote filesystem](https://marketplace.visualstudio.com/items?itemName=murbani.vscode-abap-remote-fs) and similar VS Code/MCP tools | Live SAP repository access from VS Code or AI tools. | ABAP LSP treats local source as the primary workspace and fetches missing dependencies into a cache. It can complement remote filesystem tools, but it is not mainly a live SAP filesystem. |

## Repository Layout

- `crates/abap_lexer`: tokenization and source ranges.
- `crates/abap_ast`: syntax tree model and shared node types.
- `crates/abap_parser`: syntax parser built on the lexer and AST.
- `crates/abap_symbols`: symbol indexing, semantic facts, validation, and routine analysis.
- `crates/abap_cache`: workspace loading, immutable snapshots, call graphs, effective source, and dependency-aware state.
- `crates/abap_dependency_store`: SQLite-backed remote dependency cache.
- `crates/abap_lsp`: LSP handlers, custom notifications, diagnostics, semantic tokens, and editor features.
- `crates/abap_lsp_server`: blocking LSP server binary over stdio or TCP.
- `crates/abap_cli`: local analysis CLI.
- `crates/abap_adt_cli`: SAP ADT query CLI.
- `editors/vscode/`: VS Code client, commands, TextMate grammar, and remote dependency orchestration.
- `docs/`: architecture, workspace, semantic export, call graph, call-dataflow, ADT, and migration notes.
- `examples/`: small ABAP fixtures for parser and smoke coverage.

## Build And Test

Prerequisites:

- Rust 1.86 or newer,
- Node.js/npm only if you are working on the VS Code client,
- SAP ADT credentials only for live remote lookup/fetch workflows.

Build everything:

```bat
.\build.bat
cargo build --workspace
```

Build one package:

```bat
.\build.bat -p abap_lsp_server
cargo build -p abap_lsp_server
```

Run tests:

```bat
.\test.bat
cargo test --workspace
cargo test -p abap_parser
```

Run performance checks:

```bat
.\perf_test.bat
```

Start the server for editor debugging over TCP:

```bat
cargo run -p abap_lsp_server -- --listen 127.0.0.1:9472
```

Then configure the VS Code extension setting `abap-ls.serverTransport` to
`tcp`, or set `__ABAP_LSP_CONNECT=127.0.0.1:9472`.

## Documentation

- [Workspace layout](docs/workspace-layout.md)
- [Semantic dossier](docs/semantic-dossier.md)
- [Call graph](docs/call-graph.md)
- [Call dataflow](docs/call-dataflow.md)
- [ABAP ADT CLI](docs/abap-adt-cli.md)
- [Remote dependencies](docs/architecture/remote-dependencies.md)
- [Concurrency model](docs/architecture/concurrency.md)
- [Static analysis artifacts](docs/architecture/static-analysis.md)
- [Migration parity checklist](docs/migration/parity-matrix.md)

## Security And Configuration

Repository-local ADT settings may live in `.env`, but SAP credentials and
machine-specific overrides must not be committed. Remote dependency artifacts
and profiling output are generated data and should be treated as disposable or
cache-owned unless explicitly exported for review.
