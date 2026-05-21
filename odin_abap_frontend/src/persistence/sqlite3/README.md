# SQLite3 Odin Bindings

This package exposes a compact raw binding to the SQLite3 C API for the Odin
frontend.

Bundled Windows x64 static libraries:

- `lib/windows-amd64/debug/sqlite3.lib`: SQLite 3.53.1, unoptimized, debug info.
- `lib/windows-amd64/release/sqlite3.lib`: SQLite 3.53.1, optimized.

The libraries were compiled from the official SQLite amalgamation
`sqlite-amalgamation-3530100.zip` with `clang-cl /MT /Zl` and archived with
`llvm-lib`. SQLite's deliverable code is public domain.

The binding defaults to the release library, and uses the debug library when
Odin is built with `-debug`. Non-Windows builds fall back to `system:sqlite3`.
