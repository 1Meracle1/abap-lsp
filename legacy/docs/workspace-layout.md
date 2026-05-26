# ABAP Workspace Layout

This document describes the intended local workspace shape for `abap-lsp` projects.

The current direction is:

- keep the root `abapls.toml` small and human-editable
- discover normal project units from `src/` by convention
- use optional sidecar files only where conventions need help
- keep remote dependency state in a centralized SQLite store outside the workspace

## Root Manifest

The root `abapls.toml` is primarily for global workspace settings.

Typical shape:

```toml
version = 1
connection = "default"

[dependency_store]
product_version = "SAP NETWEAVER"
default_package_version = "7.50"

[resolution]
dependency_mode = "remote-on-demand"
remote_requests_per_second = 24

[local_export]
roots = ["D:/dev/abap/sap_system_export"]

[dependencies]
source = "local-first"

[lints]
profile = "recommended"
report_suppressed = false

[lints.rules]
"abap-lsp.dead-store" = "info"
"abap-lsp.select-in-loop" = "info"
"abap-lsp.select-single-without-full-key" = "info"
```

The root manifest does not need explicit `[[unit]]` entries for normal `src` content.
The optional `[dependency_store]` section enables centralized remote dependency resolution and versions cached ABAP/DDIC artifacts by SAP product/package version.
The optional `[local_export]` and `[dependencies]` sections define workspace defaults for local
exported SAP source roots and sourcing preference. Unit sidecars can override these defaults per
report or per unit.

### Lint Configuration

The optional `[lints]` section controls the native lint diagnostic surface used by the language
server and cache-backed tooling.

Supported top-level fields:

- `profile`: `recommended` (default), `strict`, `all`, or `none`.
- `report_suppressed`: when `true`, diagnostics disabled by config or source suppressions remain as
  informational suppressed diagnostics with suppression metadata; when `false`, they are dropped.

Supported override tables:

- `[lints.groups]`: maps built-in group names such as `correctness`, `performance`, and `style` to
  `allow`, `info`, `warn`, or `deny`.
- `[lints.rules]`: maps exact lint IDs such as `"abap-lsp.dead-store"` to `allow`, `info`, `warn`,
  or `deny`.
- `[lints.sap_atc]`: configures imported remote SAP ATC findings. `mode` is `off`, `manual`, or
  `on-save`; `check_variant` defaults to `DEFAULT`; `configuration` is optional.

Unknown built-in groups and unknown native rule IDs in the `abap-lsp.*` and `epc.*` namespaces are
reported as `abapls.toml` manifest diagnostics and ignored by the effective lint policy. Other
unknown rule IDs are also diagnosed unless they use an external provider namespace of the form
`<provider>:<id>`, for example `"sap-atc:check/message"`.

`recommended` keeps parser and semantic hard errors as errors while keeping noisy SAP-inspired
heuristics at `info`. Use targeted rule overrides when a team is ready to make one rule stricter:

```toml
[lints.rules]
"abap-lsp.select-star" = "warn"
"abap-lsp.select-single-without-full-key" = "warn"
"abap-lsp.for-all-entries-without-guard" = "warn"
```

Use `profile = "strict"` only when the workspace has already resolved or suppressed expected
informational findings. The `epc.*` semantic hard-error IDs remain `deny` under profiles and
rule/group config.

The implemented lint registry, default levels, groups, origins, and SAP suppression aliases are
listed in [`docs/reference/lints.md`](reference/lints.md).

Source suppressions are statement-scoped unless explicitly file-scoped:

```abap
DATA lv_unused TYPE i. " abap-lsp:allow(abap-lsp.dead-store)
gv_unused = 1 ##NEEDED.
SELECT * FROM mara INTO TABLE @DATA(lt_mara). "#EC CI_ALL_FIELDS_NEEDED
" abap-lsp:allow-next-line(abap-lsp.select-single-without-full-key)
SELECT SINGLE carrid FROM scarr INTO @DATA(lv_carrid_out) WHERE carrid = @lv_carrid.
```

SAP pragmas and pseudo comments only suppress lints that list the corresponding SAP alias in lint
metadata. Broad forms such as `#EC *`, `all`, and `group:<name>` are intentionally ignored by the
current scanner.

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

### Other `src/` Files

Any `.abap` file under `src/` that is not already owned by a conventional unit above is loaded as a separate single-file unit by default.

For example:

```text
src/ZMY_REPORT/ZMY_REPORT.abap
src/ZMY_REPORT/ZMY_REPORT_TOP.abap
src/ZMY_REPORT/ZMY_REPORT_CLS.abap
```

These files are discovered independently. If `ZMY_REPORT.abap` contains `INCLUDE zmy_report_top.`, the semantic project resolves that include and analyzes the included source in the including compilation context.

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

`roots` entries in `abapls.toml` may be absolute or relative to the workspace root. In sidecars,
they may be absolute or relative to the sidecar file. A sidecar `[local_export]` section replaces
the workspace default roots for that unit.

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

A sidecar `[dependencies]` section replaces the workspace default sourcing preference for that unit.

## Central Dependency Store

Remote dependency state lives in a centralized SQLite store owned by Rust.

Current responsibilities:

- cached fetched ABAP and DDIC artifacts
- versioned symbol lookup for `go-to-definition`, hover, completion, and diagnostics
- negative lookup tracking scoped by SAP connection and dependency profile
- read-only `abapls-cache:` virtual documents opened by the editor

Default location:

- Windows: `%LOCALAPPDATA%/abap-ls/dependency-cache.sqlite3`
- macOS: `~/Library/Caches/abap-ls/dependency-cache.sqlite3`
- Linux: `${XDG_CACHE_HOME:-~/.cache}/abap-ls/dependency-cache.sqlite3`

The location can be overridden with the editor-local VS Code setting `abap-ls.dependencyCache.path`.

## Design Intention

The intended split is:

- root `abapls.toml`: workspace settings
- `src/`: primary source of truth for local project units
- `abapls-unit.toml`: exceptional ownership and sourcing hints
- central dependency store: remote ABAP/DDIC state shared across workspaces with matching dependency profiles

This keeps the user-facing project structure simple while still allowing layered dependency loading and optional offline/exported SAP source usage without materializing transient dependencies in the workspace.
