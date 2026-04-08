# Repository Layout Migration

## Kept At Root

- `editors/vscode/` stays at the root because the extension continues to mediate SAP/ADT communication for the Rust workspace.
- `.gitignore` stays at the root and now covers both Rust and extension artifacts.

## New Rust-First Root

- `Cargo.toml` defines the workspace and shared dependency policy.
- `crates/` contains the new Rust implementation split by layer.
- `docs/` tracks migration sequencing, concurrency design, remote dependency protocol, and parity metrics.
- `.vscode/` targets Cargo and the Rust server workflow.
- `.cursor/rules/` contains Rust-specific project guidance.

## Migration Discipline

- New Rust work lands at the root workspace.
- Behavior changes should come with crate-local regression coverage or updated examples.
- Shared editor integration changes should preserve compatibility with the current Rust server contract.
