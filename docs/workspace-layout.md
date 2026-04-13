# ABAP Workspace Layout

This document describes the intended local workspace shape for `abap-lsp` projects.

The current direction is:

- keep the root `abapls.toml` small and human-editable
- discover normal project units from `src/` by convention
- use optional sidecar files only where conventions need help
- keep external dependency state under `.abapls/cache`

## Root Manifest

The root `abapls.toml` is primarily for global workspace settings.

Typical shape:

```toml
version = 1
connection = "default"

[resolution]
dependency_mode = "remote-on-demand"
cache_dir = ".abapls/cache"
unknown_symbol_mode = "remote"
remote_request_parallelism = 8
remote_requests_per_second = 24
```

The root manifest does not need explicit `[[unit]]` entries for normal `src` content.

## `src/` Discovery

When the root manifest has no explicit units, the language server discovers units from `src/`.

### Reports

Preferred layout:

```text
src/
  reports/
    ZMY_REPORT/
      ZMY_REPORT.abap
      ...
```

Discovery rule:

- `src/reports/<REPORT>/<REPORT>.abap` defines one report unit
- other `.abap` files under that report folder belong to that report unit

Single-file reports are also supported:

```text
src/reports/ZMY_REPORT.abap
```

### Function Groups

Preferred layout:

```text
src/
  function-groups/
    ZFG_DEMO/
      ZFG_DEMO.abap
      includes/
        LZFG_DEMOTOP.abap
      function-modules/
        Z_FG_DEMO.abap
```

Discovery rule:

- `src/function-groups/<GROUP>/<GROUP>.abap` defines one function-group unit
- other `.abap` files under that group folder belong to that function-group unit

### Global Classes and Interfaces

```text
src/classes/ZCL_DEMO.abap
src/interfaces/ZIF_DEMO.abap
```

Discovery rule:

- each file is a separate single-file unit
- these units are treated as workspace-global providers

### Includes

```text
src/includes/ZINC_DEMO.abap
```

Discovery rule:

- each file is a separate single-file unit
- explicit ownership can be refined with a unit sidecar

## Unit Sidecars

Conventions are intentionally simple. When they are not enough, use an `abapls-unit.toml` sidecar.

### Folder Unit Sidecar

Examples:

```text
src/reports/ZMY_REPORT/abapls-unit.toml
src/function-groups/ZFG_DEMO/abapls-unit.toml
```

### Single-File Unit Sidecar

Examples:

```text
src/reports/ZMY_REPORT.abap.abapls-unit.toml
src/classes/ZCL_DEMO.abap.abapls-unit.toml
```

### Sidecar Schema

```toml
members = [
  "forms/ZMY_REPORT_F01.abap",
]

includes = { "ZMY_REPORT_TOP" = "forms/ZMY_REPORT_TOP.abap" }

[local_export]
roots = ["D:/dev/abap/sap_system_export"]

[dependencies]
source = "local-first"
```

#### `members`

Additional `.abap` files that belong to the unit.

Use this when folder discovery is not enough or for single-file units that need helpers.

#### `includes`

Explicit include-name-to-file mapping.

Use this when the include name cannot be inferred reliably from the file path, or when a project wants to make ownership explicit.

#### `[local_export]`

Optional exported SAP source roots to use for dependency sourcing before or instead of ADT.

`roots` entries may be absolute or relative to the sidecar file.

#### `[dependencies]`

Optional unit-local dependency sourcing preference.

Supported values:

- `local-first`
- `local-only`
- `adt-first`

Behavior:

- `local-first`: try local exported SAP roots first, then ADT
- `local-only`: only use local exported SAP roots for dependencies from this unit
- `adt-first`: prefer ADT, but fall back to local exported SAP roots if ADT fetch fails

## External Dependency Cache

External dependency state lives under `.abapls/cache`.

Current responsibilities:

- cached fetched dependency source files
- cache-side dependency manifests that describe dependency layers per source file
- negative lookup markers and object metadata

This cache is implementation-oriented. It is not intended to be edited manually.

## Design Intention

The intended split is:

- root `abapls.toml`: workspace settings
- `src/`: primary source of truth for local project units
- `abapls-unit.toml`: exceptional ownership and sourcing hints
- `.abapls/cache`: external dependency state

This keeps the user-facing project structure simple while still allowing layered dependency loading and optional offline/exported SAP source usage.
