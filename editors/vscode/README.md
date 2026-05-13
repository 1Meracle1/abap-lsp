# ABAP LSP for Visual Studio Code

ABAP LSP provides ABAP language support backed by the Rust `abap_lsp_server`
binary in this repository. The extension is source-first: it analyzes local
ABAP workspaces, can request missing repository/DDIC dependencies through SAP
ADT, and projects cached remote artifacts as read-only `abapls-cache:` documents.

This extension is in preview. It is useful for local parsing, diagnostics,
navigation, semantic tokens, folding, completion, rename, inlay hints, and
remote dependency discovery, but it is not a replacement for SAP ADT activation,
debugging, transports, or repository administration.

## Requirements

- VS Code 1.105 or newer.
- A built `abap_lsp_server` binary from this repository.
- Optional: SAP ADT HTTP(S) access when remote dependency fetches or repository
  search are needed.

Build the server from the repository root:

```bat
cargo build -p abap_lsp_server
```

Then point VS Code at the resulting binary, for example:

```json
{
  "abap-ls.serverExecutable": "D:\\dev\\rust\\abap-lsp\\target\\debug\\abap_lsp_server.exe"
}
```

## Server Startup

### Stdio

`stdio` is the default and the normal editor mode. The extension spawns
`abap_lsp_server` and talks LSP over standard input/output.

Configuration:

```json
{
  "abap-ls.serverTransport": "stdio",
  "abap-ls.serverExecutable": "D:\\dev\\rust\\abap-lsp\\target\\debug\\abap_lsp_server.exe"
}
```

Environment overrides, in priority order:

- `__ABAP_LSP_SERVER_PATH`
- `__ABAP_LSP_SERVER_DEBUG`
- `abap-ls.serverExecutable`

On Windows, `.exe` is appended automatically when the configured path has no
extension.

### TCP

Use TCP when you want to run the server yourself, usually under a debugger.

Start the server:

```bat
cargo run -p abap_lsp_server -- --listen 127.0.0.1:9472
```

Or start an existing binary:

```bat
abap_lsp_server --listen 127.0.0.1:9472
```

Then configure VS Code:

```json
{
  "abap-ls.serverTransport": "tcp",
  "abap-ls.serverTcpAddress": "127.0.0.1:9472"
}
```

Environment overrides:

- `ABAP_LSP_LISTEN=127.0.0.1:9472` makes the server listen.
- `__ABAP_LSP_CONNECT=127.0.0.1:9472` makes the extension connect over TCP
  without changing VS Code settings.

## Workspace Setup

Open a folder that contains ABAP source and run `ABAP LSP: Create Workspace
Manifest` from the command palette. The extension creates `abapls.toml` with the
default source discovery and remote dependency settings.

The preferred layout is:

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

The server can also discover single `.abap` files under `src/`. Use
`abapls-unit.toml` sidecars when a unit needs explicit members, include mappings,
local exported SAP roots, or dependency source preferences.

See `docs/workspace-layout.md` in the repository for the full workspace model.

## SAP Connection

Run `ABAP LSP: Configure SAP Connection` from the command palette. The extension
stores the SAP base URL and username in workspace-folder settings. The password
is stored in VS Code Secret Storage.

The base URL can be a SAP host root or an ADT root:

```text
https://sap.example.com
https://sap.example.com/sap/bc/adt
```

Connection defaults can also come from the process environment or from a
repository-local `.env` file:

```text
ABAP_ADT_URL=https://sap.example.com/sap/bc/adt
ABAP_ADT_USER=YOUR_SAP_USERNAME
ABAP_ADT_PASSWORD=YOUR_SAP_PASSWORD
```

Accepted aliases:

- Base URL: `ABAP_ADT_URL`, `ABAP_ADT_BASE_URL`, `SAPBASE_URL`
- Username: `ABAP_ADT_USER`, `ABAP_ADT_USERNAME`, `SAPUSER`
- Password: `ABAP_ADT_PASSWORD`, `SAPPASS`

Do not commit `.env` files containing credentials.

Useful SAP commands:

- `ABAP LSP: Configure SAP Connection`
- `ABAP LSP: Search Repository Objects`
- `ABAP LSP: Add Editable ADT Object to Workspace`

## Dependency Cache

Remote dependencies are stored in a centralized SQLite cache instead of being
written into the workspace. Cached source and DDIC documents are opened through
the `abapls-cache:` URI scheme.

Default cache locations:

- Windows: `%LOCALAPPDATA%\\abap-ls\\dependency-cache.sqlite3`
- macOS: `~/Library/Caches/abap-ls/dependency-cache.sqlite3`
- Linux: `${XDG_CACHE_HOME:-~/.cache}/abap-ls/dependency-cache.sqlite3`

Override the location with:

```json
{
  "abap-ls.dependencyCache.path": "D:\\cache\\abap-ls\\dependency-cache.sqlite3"
}
```

Remote dependency fetches require an `abapls.toml` manifest with
`[dependency_store]` configured. SAP ADT credentials are read by the server from
the process environment or a workspace/repository `.env` file using
`ABAP_ADT_URL` / `ABAP_ADT_BASE_URL` / `SAPBASE_URL`,
`ABAP_ADT_USER` / `ABAP_ADT_USERNAME` / `SAPUSER`,
`ABAP_ADT_PASSWORD` / `SAPPASS`, and optionally
`ABAP_ADT_CLIENT` / `SAPCLIENT`. Use `ABAP LSP: Refresh Dependency Cache` after
changing cache paths, dependency source preferences, or local exported SAP roots.

Local exported SAP source can be searched before or instead of ADT by setting
sidecar configuration next to source files or in an ancestor folder:

```toml
[local_export]
roots = ["D:/dev/abap/sap_export"]

[dependencies]
source = "local-first"
```

Supported dependency sources are `local-first`, `local-only`, and `adt-first`.

## Settings

| Setting | Default | Description |
| --- | --- | --- |
| `abap-ls.serverTransport` | `stdio` | Spawn the server over stdio or connect to an existing TCP server. |
| `abap-ls.serverExecutable` | empty | Path to `abap_lsp_server` for stdio mode. |
| `abap-ls.serverTcpAddress` | `127.0.0.1:9472` | TCP address used when `serverTransport` is `tcp`. |
| `abap-ls.trace.server` | `verbose` | LSP protocol tracing level. |
| `abap-ls.maxNumberOfProblems` | `100` | Maximum problems reported by the server. |
| `abap-ls.dependencyCache.path` | empty | Optional centralized dependency cache path override. |

## Troubleshooting

### No server starts in stdio mode

Set `abap-ls.serverExecutable` or `__ABAP_LSP_SERVER_PATH` to a built
`abap_lsp_server` binary. Check the `ABAP Language Server` output channel for
the startup line and process errors.

### TCP connection is refused

Start the server with `--listen 127.0.0.1:9472` or set `ABAP_LSP_LISTEN` before
switching the extension to TCP mode. Make sure `abap-ls.serverTcpAddress` or
`__ABAP_LSP_CONNECT` matches the address the server printed.

### SAP credentials are missing

Set the accepted `ABAP_ADT_*` / `SAP*` environment variables before starting the
server, or put them in an untracked `.env` file in the workspace or repository.

### Remote dependencies are not fetched

Confirm that the workspace has `abapls.toml`, that `[dependency_store]` is
configured, and that the server process has SAP credentials. Skipped fetches and
ADT request failures are logged by the server in the `ABAP Language Server`
output channel.

### Cache documents are stale

Run `ABAP LSP: Refresh Dependency Cache`. If you changed
`abap-ls.dependencyCache.path`, the extension restarts the language client so
the server receives the new cache location.

## Development

Install dependencies and compile the extension from `editors/vscode`:

```bat
npm install
npm run compile
```

Run VS Code integration tests when a local test environment is available:

```bat
npm run test:e2e
```

Server-backed tests need either `abap-ls.serverExecutable`,
`__ABAP_LSP_SERVER_PATH`, or `__ABAP_LSP_CONNECT` configured for the launched
test window.
