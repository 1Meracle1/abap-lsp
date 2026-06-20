# ABAP LSP

ABAP LSP is an Odin ABAP frontend for local source analysis, editor language
features, and SAP ADT-backed dependency discovery.

The project is source-first. It is not a replacement for SAP ADT today. The
current focus is fast local parsing, semantic indexing, diagnostics, navigation,
completion, linting, and dependency lookup that make ABAP workspaces easier to
inspect from editors and scripts.

## What It Provides

- `abap_language_server`, an Odin language server binary for VS Code, Zed, and
  other LSP clients that can spawn a stdio server.
- A VS Code client under `editors/vscode/` with ABAP syntax support, LSP
  startup, workspace commands, SAP connection configuration, remote dependency
  fetches, and virtual cached dependency documents.
- A Zed extension under `editors/zed/` that can launch `abap_language_server`
  from `PATH`, Zed LSP settings, or `__ABAP_LSP_SERVER_PATH`.
- `abap_frontend`, a local analysis CLI for parsing, AST inspection, semantic
  checking, and native lint output.
- `adt_cli`, a SAP ADT query CLI for repository search, source fetches, DDIC
  metadata fetches, and child-object discovery.
- A workspace model based on `abapls.toml`, `src/` conventions, optional
  manifest units, local exported SAP source roots, and a centralized SQLite
  dependency store.

## Current Editor Features

The language server currently advertises:

- diagnostics from parsing, semantic analysis, type checks, Open SQL checks, and
  native lints,
- semantic tokens,
- hover,
- completion with snippets,
- go to definition, go to implementation, and references,
- rename and prepare rename,
- folding ranges,
- code actions for supported diagnostics,
- workspace file create/rename/delete notifications,
- background workspace analysis with progress notifications,
- on-demand remote dependency resolution through editor clients.

The implementation is still under active development. Expect useful behavior on
common custom ABAP source, but not full ABAP platform parity.

## Build And Test

Use the root wrapper scripts on Windows.

Prerequisites:

- Odin, available at the path configured in `build.bat` and `test.bat`,
- Node.js/npm only if you are working on the VS Code client,
- SAP ADT credentials only for live remote lookup/fetch workflows.

Build debug binaries:

```bat
.\build.bat
```

Build optimized binaries:

```bat
.\build.bat release
```

Run the Odin package checks and tests:

```bat
.\test.bat
```

Run a built tool through the wrapper:

```bat
.\run.bat debug abap_frontend parse examples\ZPERF_PARSER_MIXED.abap
.\run.bat debug abap_frontend tree examples\ZPERF_PARSER_MIXED.abap
.\run.bat debug abap_frontend analyze path\to\workspace --enable-lints
.\run.bat debug abap_frontend lint --json --pretty path\to\workspace
```

## Language Server

Build the server:

```bat
.\build.bat
```

For VS Code stdio startup, point the extension at the built binary:

```json
{
  "abap-ls.serverExecutable": "D:\\dev\\rust\\abap-lsp\\bin\\debug\\abap_language_server.exe"
}
```

The same path can be provided through `__ABAP_LSP_SERVER_PATH`. On Windows,
`.exe` is appended automatically by the VS Code client when the configured path
has no extension.

## CLI Tools

### `abap_frontend`

Use this for local source analysis:

```bat
.\run.bat debug abap_frontend parse path\to\zreport.abap
.\run.bat debug abap_frontend tree path\to\zreport.abap
.\run.bat debug abap_frontend analyze path\to\workspace --enable-lints
.\run.bat debug abap_frontend lint --json --with-project path\to\zreport.abap
.\run.bat debug abap_frontend lint --json --all-files --fail-on-warnings path\to\workspace
```

See [docs/reference/lints.md](docs/reference/lints.md) for native lint IDs,
profiles, suppressions, and JSON output shape. See
[docs/typecheck.md](docs/typecheck.md) for the current type-checking notes.

### `adt_cli`

Use this when live SAP information is needed:

```bat
.\run.bat debug adt_cli search "MARA"
.\run.bat debug adt_cli get source class zcl_demo
.\run.bat debug adt_cli get source function-module BAPI_USER_GET_DETAIL --group SUSR
.\run.bat debug adt_cli get ddic table mara --raw
.\run.bat debug adt_cli children package zpackage
```

Connection values can come from CLI flags, environment variables, or a
repository-local `.env` file.

Accepted environment aliases:

- Base URL: `ABAP_ADT_URL`, `ABAP_ADT_BASE_URL`, `SAPBASE_URL`
- Username: `ABAP_ADT_USER`, `ABAP_ADT_USERNAME`, `SAPUSER`
- Password: `ABAP_ADT_PASSWORD`, `SAPPASS`
- SAP client: `ABAP_ADT_CLIENT`, `SAPCLIENT`
- Optional type-pool resolver: `ABAP_TYPEPOOL_RESOLVER_URL`

Do not commit `.env` files containing credentials.

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

When explicit units are absent, the workspace scans `.abap` files under the
workspace root. Manifest units can be added when a source root needs explicit
kind, root file, members, or dependency relationships:

```toml
version = 1
connection = "default"

[dependency_store]
product_version = "SAP NETWEAVER"
default_package_version = "7.50"

[dependencies]
source = "local-first"

[[unit]]
name = "ZCL_DEMO"
kind = "global-class"
root_file = "src/classes/ZCL_DEMO.abap"
```

Remote dependency state is stored in a centralized SQLite cache instead of
materializing every fetched dependency into the project tree. Cached dependency
documents are opened through the `abapls-cache:` URI scheme in supporting
editors.

Local exported SAP source can be searched before or instead of ADT by setting:

```toml
[local_export]
roots = ["D:/dev/abap/sap_export"]

[dependencies]
source = "local-first"
```

Supported dependency sources are `local-first`, `local-only`, and `adt-first`.

## Native Lints

Native lints are configured from `abapls.toml` and emitted through both LSP
diagnostics and `abap_frontend lint`:

```toml
[lints]
profile = "recommended"
report_suppressed = false

[lints.rules]
"abap-lsp.select-star" = "warn"
"abap-lsp.dead-store" = "info"
```

Suppress one statement with an explicit native ID or a supported SAP alias:

```abap
DATA lv_unused TYPE i. " abap-lsp:allow(abap-lsp.dead-store)
gv_unused = 1 ##NEEDED.
SELECT * FROM mara INTO TABLE @DATA(lt_mara). "#EC CI_ALL_FIELDS_NEEDED
```

See [docs/reference/lints.md](docs/reference/lints.md) for stable IDs, defaults,
groups, and aliases.

## Repository Layout

- `cmd/abap_frontend`: local parser, analyzer, and lint CLI.
- `cmd/abap_language_server`: stdio LSP server entry point.
- `cmd/adt_cli`: SAP ADT query CLI.
- `src/tokenizer`: tokenization and source ranges.
- `src/ast`: syntax tree model and shared node types.
- `src/parser`: ABAP parser built on the tokenizer and AST.
- `src/semantic`: semantic analysis, type checks, entity lookup, and dependency
  candidates.
- `src/lints`: native lint metadata and lint analysis.
- `src/workspace`: workspace loading, manifests, snapshots, and semantic graph
  scheduling.
- `src/remote_dependencies`: ADT/local-export dependency resolution and
  external interface preparation.
- `src/dependency_store`: SQLite-backed remote dependency cache.
- `src/lsp`: LSP protocol handlers, diagnostics, navigation, completion, and
  editor features.
- `src/adt`: SAP ADT HTTP client and XML parsing helpers.
- `editors/vscode`: VS Code client, commands, TextMate grammar, and remote
  dependency orchestration.
- `editors/zed`: Zed extension, language config, queries, and theme assets.
- `docs`: reference notes for type checking and native lints.
- `integration`: optional SAP-side helpers such as the type-pool resolver.
- `examples`: small ABAP fixtures for parser and smoke coverage.
- `scripts`: local verification helpers.

## Project Goals

The long-term goal is a practical ABAP language server and analysis toolkit
that can:

- understand local ABAP workspaces without requiring a live SAP system for every
  editor operation,
- fetch missing repository and DDIC dependencies on demand through SAP ADT,
- provide fast editor feedback for large ABAP codebases,
- expose stable machine-readable outputs for CI checks, analysis tooling, and
  AI-assisted workflows,
- model type flow, Open SQL usage, includes, class relationships, and common
  ABAP runtime conventions conservatively instead of guessing.

Non-goals for the current project:

- replacing SAP activation, transports, debugging, profiling, or repository
  administration,
- complete ABAP Platform, CDS, RAP, Dynpro, and DDIC editor parity,
- treating dynamic ABAP, generated code, or macro-heavy code as fully precise
  when the analyzer cannot prove it.

## Security And Configuration

Repository-local ADT settings may live in `.env`, but SAP credentials and
machine-specific overrides must not be committed. Remote dependency artifacts
and profiling output are generated data and should be treated as disposable or
cache-owned unless explicitly exported for review.
