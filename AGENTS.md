# Repository Guidelines

## Top Priority Convention

This section has priority over every other convention in this file.

- Simplicity first: write the minimum code that solves the problem. Nothing
  speculative.
- Implement only what was asked. No features beyond the request.
- Do not add abstractions for single-use code.
- Do not add flexibility or configurability that was not requested.
- Do not add error handling for impossible scenarios.
- If you write 20 lines and it could be 5, rewrite it.
- Minimize code size aggressively: no pessimization, no bloat, no excess.
- Write compact, simple, clean, deeply thought-through code from first
  principles that surgically executes the intention.

## Project Structure & Module Organization
This repository is a Rust workspace rooted at `Cargo.toml`. Core crates live under `crates/`, with `abap_lsp_server` as the default binary and supporting libraries such as `abap_parser`, `abap_symbols`, and `abap_lsp` split by responsibility. Shared examples live in `examples/*.abap`, design and architecture notes in `docs/`, and the editor client in `editors/vscode/`. Keep dependency flow one-way: lower layers (`abap_jsonrpc`, `abap_lexer`, `abap_ast`) must not depend on higher layers such as `abap_cache`, `abap_lsp`, or `abap_lsp_server`.

## Build, Test, and Development Commands
Use the wrapper scripts on Windows, or run Cargo directly.

- `.\build.bat` or `cargo build --workspace`: debug build for all crates.
- `.\build.bat release` or `cargo build --workspace --release`: optimized build.
- `.\build.bat -p abap_lsp_server`: build one package.
- `.\test.bat` or `cargo test --workspace`: run the Rust test suite.
- `cargo test -p abap_parser`: run tests for one crate while iterating.
- `.\perf_test.bat`: run the repository’s performance checks.
- `cargo run -p abap_adt_cli -- --help`: inspect the ADT CLI used for live SAP lookups.
- `cargo run -p abap_cli -- analyze --json <FILE>`: emit a semantic dossier JSON export for one ABAP file.
- `cargo run -p abap_cli -- analyze --json --with-project <FILE>`: analyze a file with workspace/project context for cross-unit resolution.
- `cargo run -p abap_cli -- call-graph --json <FILE>`: emit a project-scale call graph JSON export rooted in the workspace around one file.
- `cargo run -p abap_cli -- call-graph --json --symbol <NAME> <FILE>`: query inbound/outbound/unresolved call edges for one callable symbol within the project graph.
- `cargo flamegraph -p abap_lsp --example build_semantic_tokens_perf --release`: profile semantic token generation.

## Coding Style & Naming Conventions
Follow standard Rust style with `rustfmt` formatting and 4-space indentation. Use `snake_case` for functions, modules, and test names, `CamelCase` for types, and `SCREAMING_SNAKE_CASE` for constants. Keep crates focused by responsibility and prefer small internal modules such as `control_stmt.rs` or `type_ref.rs` over oversized files. Avoid async unless there is measured need; prefer immutable `Arc`-published snapshots, one foreground protocol loop, and bounded worker pools for CPU-heavy work. Ask before adding new external dependencies or changing workspace dependency versions.

## Testing Guidelines
Place unit tests alongside implementation when they are tightly scoped, and integration tests under `crates/<crate>/tests/`. Existing tests use descriptive `snake_case` names such as `parses_workspace_examples` and `resolves_do_times_count_variable_in_header`. Reuse `examples/*.abap` for parser coverage when possible, add regression tests for each parser or semantic-analysis fix, and port tests into the crate they exercise instead of rebuilding a root-level monolithic suite.

## Commit & Pull Request Guidelines
Recent commits use short, imperative, lower-case subjects focused on behavior, for example `enable semantic analysis on OpenSql SELECT statements`. Keep commits narrow and explain the user-visible or parser-visible change. Pull requests should include a concise summary, linked issue or task when applicable, and the exact validation performed, such as `cargo test --workspace`. Include screenshots only for `editors/vscode` UI changes.

## Configuration & Environment Notes
Repository-local environment settings may be stored in `.env` for ADT tooling. Do not commit secrets, SAP credentials, or machine-specific overrides. When live SAP repository or DDIC data is needed, prefer `abap_adt_cli` and `docs/abap-adt-cli.md` over guessing from partial local files. Treat `target/` and generated profiling artifacts as disposable build output.
