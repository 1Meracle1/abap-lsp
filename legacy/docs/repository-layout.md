# Repository Layout

## Kept At Root

- `editors/vscode/` stays at the root because the extension continues to mediate SAP/ADT communication for the Rust workspace.
- `.gitignore` stays at the root and now covers both Rust and extension artifacts.

## Rust Workspace Root

- `Cargo.toml` defines the workspace and shared dependency policy.
- `crates/` contains the Rust implementation split by layer.
- `docs/` tracks architecture, concurrency design, remote dependency protocol, coverage, and benchmark notes.
- `.vscode/` targets Cargo and the Rust server workflow.
- `.cursor/rules/` contains Rust-specific project guidance.

## Development Discipline

- Rust work lands at the root workspace.
- Behavior changes should come with crate-local regression coverage or updated examples.
- Shared editor integration changes should preserve compatibility with the current Rust server contract.
