# Repository Guidelines

## Top Priority Convention

This section has priority over every other convention in this file.

- Make the change that best fits the existing architecture. Understand the
  relevant modules, ownership boundaries, and data flow before editing.
- Simplicity means clear design with the least necessary code, not the shortest
  local workaround. A small change that duplicates behavior, bypasses the owning
  subsystem, or leaves inconsistent rules in different places is not simple.
- Implement only what was asked. Do not add speculative features, configuration,
  dependencies, or framework-like abstractions.
- Reuse existing parser, semantic-analysis, workspace, ADT, dependency-store,
  runtime, and persistence APIs before creating new code. If behavior already
  exists, extend or adapt the owning implementation instead of copying it.
- Add helpers or abstractions only when they remove real duplication, preserve
  module boundaries, or express an established project pattern.
- Fix root causes in the responsible subsystem. Avoid call-site workarounds
  unless the issue is truly local to that call site.
- Keep code compact and direct, but prioritize correctness, coherence, and
  maintainability over minimizing line count.
- Do not add error handling for impossible scenarios; use assertions if these
  conditions can be verified in debug builds.

## Project Structure & Module Organization

The root implementation is the Odin ABAP frontend. Entry points live under
`cmd/`, shared packages under `src/`, generated binaries and test data under
`bin/`, and scratch notes/output under `tmp/`. Keep package boundaries direct:
tokenizer, AST, parser, semantic analysis, workspace, ADT, dependency store,
runtime, and persistence code should depend only on the lower-level packages
they actually use.

The VS Code extension under `editors/vscode/` launches the Odin language server
from `bin/<mode>/abap_language_server.exe` when configured for stdio mode.

## Build, Test, and Development Commands

Use the root Odin wrapper scripts on Windows.

- `.\build.bat`: debug build for `cmd/abap_frontend`, `cmd/adt_cli`, and `cmd/abap_language_server`.
- `.\build.bat release`: optimized Odin build.
- `.\build.bat trace`: build with trace logging enabled.
- `.\run.bat [debug|release] [abap_frontend|adt_cli|abap_language_server|lsp|adt] ...`: build and run a root Odin executable.
- `.\test.bat`: check and test the Odin packages.
- `.\test.bat --no-leak-warnings`: run tests with quieter leak logging.

## Odin Coding Style & Naming Conventions

Follow the style already used in `src/`: package names are lower snake case,
types use `Camel_Case` where the surrounding Odin code does, procedures and
variables use `snake_case`, and tests are descriptive `snake_case` procedures
annotated with `@(test)`. Keep modules focused by responsibility and prefer
existing package helpers over duplicated local logic. Use small local helpers
when behavior is truly local, and avoid framework-like abstractions.

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
`.\test.bat` before handing off broad changes.

## Commit & Pull Request Guidelines

Use short, imperative, lower-case subjects focused on behavior, for example
`resolve includes from manifest members`. Keep commits narrow and explain the
user-visible, parser-visible, or tooling-visible change. Pull requests should
include a concise summary and the exact validation performed, such as
`.\test.bat`.

## Configuration & Environment Notes

Repository-local environment settings may be stored in `.env` for ADT tooling.
Do not commit secrets, SAP credentials, or machine-specific overrides. When live
SAP repository or DDIC data is needed, prefer the Odin `adt_cli` path before
guessing from partial local files. Treat `bin/`, `target/`, and generated
profiling or trace artifacts as disposable build output.
