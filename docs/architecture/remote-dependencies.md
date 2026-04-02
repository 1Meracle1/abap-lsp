# Remote Dependency Contract

## Current Decision

The Rust rewrite keeps SAP communication client-mediated first.

The server remains responsible for:

- detecting unresolved remote dependency candidates,
- deduplicating and prioritizing them,
- notifying the client when a fetch round is needed,
- reloading workspace state after the client updates files and manifest data.

The VS Code client remains responsible for:

- SAP credentials,
- ADT HTTP(S) requests,
- rate limiting and scheduling,
- writing fetched source files into the workspace cache,
- notifying the server when the workspace view has changed.

## Notification Surface To Preserve

- `abapls/resolveRemoteDependencies`
- `abapls/remoteDependenciesUpdated`
- `abapls/workspaceManifestUpdated`
- `abapls/dependencyCacheCleared`

## Migration Requirement

The Rust server must keep these notification names and payloads stable enough that `editors/vscode/` continues to function during migration. If direct SAP access is ever introduced later, it should sit behind a transport abstraction and must not leak into parser, symbols, or cache crates.

## Why Keep It Client-Mediated First

- credentials and trust stores already live naturally in the editor,
- the current product behavior already depends on client-side cache writes,
- it avoids mixing SAP transport complexity into the first Rust milestones,
- parity can be measured sooner because the protocol boundary already exists.
