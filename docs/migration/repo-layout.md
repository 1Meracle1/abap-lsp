# Repository Layout Migration

## Archived Into `legacy/`

The following Odin-era root entries were moved into `legacy/` unchanged enough to remain a reference implementation:

- `legacy/src/`
- `legacy/tests/`
- `legacy/build.bat`
- `legacy/test.bat`
- `legacy/README.md`
- `legacy/ols.json`
- `legacy/ols_raddbg_project`
- `legacy/.vscode/`
- `legacy/.cursor/rules/abap-lsp-project-rule.mdc`

## Kept At Root

- `editors/vscode/` stays at the root because the extension continues to mediate SAP/ADT communication and should be reusable against both implementations.
- `.gitignore` stays at the root and now covers both Rust and extension artifacts.

## New Rust-First Root

- `Cargo.toml` defines the workspace and shared dependency policy.
- `crates/` contains the new Rust implementation split by layer.
- `docs/` tracks migration sequencing, concurrency design, remote dependency protocol, and parity metrics.
- `.vscode/` now targets Cargo and can launch either the Rust server or the legacy Odin server.
- `.cursor/rules/` contains Rust-specific project guidance.

## Migration Discipline

- New Rust work lands at the root workspace.
- Behavior comparisons and reference lookups use `legacy/`.
- Shared editor integration changes should preserve compatibility with both the Rust and Odin servers wherever practical.
