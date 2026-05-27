# Repository Guidelines

## Top Priority Convention

This section has priority over every other convention in this file.

- Simplicity first: write the minimum code that solves the problem. Nothing
  speculative.
- Implement only what was asked. No features beyond the request.
- Do not add abstractions for single-use code.
- Do not add flexibility or configurability that was not requested.
- Do not add error handling for impossible scenarios, use assertions if these conditions can be verified in debug builds.
- If you write 20 lines and it could be 5, rewrite it.
- Minimize code size aggressively: no pessimization, no bloat, no excess.
- Write compact, simple, clean, deeply thought-through code from first
  principles that surgically executes the intention.

## Project Structure & Module Organization

The root implementation is the Odin ABAP frontend. Entry points live under
`cmd/`, shared packages under `src/`, generated binaries and test data under
`bin/`, and scratch notes/output under `tmp/`. Keep package boundaries direct:
tokenizer, AST, parser, semantic analysis, workspace, ADT, dependency store,
runtime, and persistence code should depend only on the lower-level packages
they actually use.

The old Rust implementation is preserved under `legacy/`. The VS Code extension
under `editors/vscode/` still launches the legacy Rust language server until an
Odin language server replaces it, so server build and launch paths should point
at `legacy/target/...`.

## Build, Test, and Development Commands

Use the root Odin wrapper scripts on Windows.

- `.\build.bat`: debug build for `cmd/abap_frontend` and `cmd/adt_cli`.
- `.\build.bat release`: optimized Odin build.
- `.\build.bat trace-adt-fetch`: build with ADT fetch tracing enabled.
- `.\run.bat [debug|release] [abap_frontend|adt_cli] ...`: build and run a root Odin executable.
- `.\test.bat`: check and test the Odin packages.
- `.\test.bat --no-leak-warnings`: run tests with quieter leak logging.

For the legacy Rust server only:

- `.\legacy\build.bat -p abap_lsp_server`: build the legacy debug server.
- `.\legacy\build.bat release -p abap_lsp_server`: build the legacy release server.
- `.\legacy\test.bat`: run the legacy Rust workspace tests.

## Odin Coding Style & Naming Conventions

Follow the style already used in `src/`: package names are lower snake case,
types use `Camel_Case` where the surrounding Odin code does, procedures and
variables use `snake_case`, and tests are descriptive `snake_case` procedures
annotated with `@(test)`. Keep modules focused by responsibility and prefer
small local helpers over framework-like abstractions.

Default procedure calling convention passes implicit `context` pointer on each call- this context variable is local to each scope.
In tests, pass `context.allocator` directly instead of creating a heap allocator or local allocator alias.

Ask before adding new external dependencies, native libraries, toolchain
requirements, or changing bundled library versions.

## Testing Guidelines

Place Odin unit tests next to the implementation package they exercise, using
the existing `*_test.odin` pattern. Add focused regression tests for parser,
semantic-analysis, workspace, ADT, dependency-store, and runtime fixes. Keep
test fixtures under `bin/test-data/` when they are generated or temporary.

Run the smallest relevant package check/test while iterating, then use
`.\test.bat` before handing off broad changes. Use `.\legacy\test.bat` only
when changing files under `legacy/`.

## Commit & Pull Request Guidelines

Use short, imperative, lower-case subjects focused on behavior, for example
`resolve includes from manifest members`. Keep commits narrow and explain the
user-visible, parser-visible, or tooling-visible change. Pull requests should
include a concise summary and the exact validation performed, such as
`.\test.bat` or `.\legacy\test.bat`.

## Configuration & Environment Notes

Repository-local environment settings may be stored in `.env` for ADT tooling.
Do not commit secrets, SAP credentials, or machine-specific overrides. When live
SAP repository or DDIC data is needed, prefer the Odin `adt_cli` path before
guessing from partial local files. Treat `bin/`, `target/`, and generated
profiling or trace artifacts as disposable build output.
