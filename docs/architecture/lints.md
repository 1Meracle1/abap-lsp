# ABAP Lint Architecture

This document records the implemented Clippy-like lint layer as of April 28, 2026. User-facing rule
IDs, defaults, CLI usage, and suppressions are documented in
[`docs/reference/lints.md`](../reference/lints.md).

## Current Status

Implemented pieces:

- `abap_lints` owns the stable registry, levels, profiles, config policy, documentation anchors,
  SAP aliases, and source suppression scanner.
- `abapls.toml` supports `[lints]`, `[lints.groups]`, `[lints.rules]`, and `[lints.sap_atc]`.
- snapshot analysis stores native lint diagnostics separately from parse and semantic hard errors.
- LSP diagnostics publish native lints with stable codes, docs links, and lint metadata.
- source suppressions support explicit `abap-lsp:allow...` comments plus mapped SAP pragmas and
  pseudo comments.
- `abap-cli lint` supports single-file, project-context, and all-files runs with human or JSON
  output.
- imported SAP ATC findings can be rendered as a separate LSP diagnostic provider when client
  results are supplied.

Still incomplete:

- block suppressions are not implemented.
- native lint fix-its and code actions are not implemented.
- remote SAP ATC execution is not performed by the Rust server; imported client results are rendered.
- the local rule pack is useful but intentionally smaller than mature ABAP lint tools.

## Layering

Dependency flow stays one-way:

```text
abap_lexer
  -> abap_lints
  -> abap_cache
  -> abap_lsp / abap_cli
```

Current responsibilities:

- `crates/abap_lints`: low-level lint model and policy. It depends only on `abap_lexer` and
  `serde`, so it does not pull in parser, cache, LSP, or CLI layers.
- `crates/abap_cache`: parses the workspace manifest, applies lint policy while materializing
  snapshots, builds local SAP-inspired lint diagnostics from semantic facts, and re-exports lint
  types for cache consumers.
- `crates/abap_lsp`: renders native lint diagnostics, reports lint-config manifest diagnostics, and
  renders imported SAP ATC diagnostics.
- `crates/abap_cli`: exposes `abap-cli lint`, including JSON reports for single files, project
  context, and all editable workspace files.

Lower crates do not depend on higher crates for lint behavior. In particular, `abap_lints` has no
dependency on `abap_cache`, `abap_lsp`, or `abap_cli`.

## Stable IDs

Native lint IDs are dotted, lowercase ASCII strings:

- `abap-lsp.<rule>` for local analyzer rules.
- `epc.<rule>` for semantic hard errors surfaced through lint metadata.

The registry normalizes case and underscore-versus-hyphen spelling in config and suppressions, but
the canonical IDs in diagnostics and docs are stable. Rename by adding a new ID and preserving the
old one long enough for users to migrate.

External provider IDs must use a provider namespace such as:

```toml
[lints.rules]
"sap-atc:check/message" = "warn"
```

Unknown native IDs and unknown built-in groups produce `abapls.toml` manifest diagnostics. External
provider IDs are accepted so imported providers can be configured before a native mapping exists.

## Config Defaults

Supported levels are `allow`, `info`, `warn`, and `deny`. Accepted config aliases are `warning` for
`warn` and `error` for `deny`.

Supported profiles:

- `recommended`: default profile. Keeps hard errors at `deny` and leaves noisy or uncertain
  SAP-inspired lints at their registry defaults, usually `info`.
- `strict`: escalates enabled non-hard-error findings for teams that already cleaned up expected
  informational findings.
- `all`: enables otherwise allowed local rules without making every rule an error.
- `none`: disables configurable non-hard-error lints.

`epc.*` hard-error lints remain `deny` under profile, group, and rule overrides. This protects
parser/semantic hard errors from being muted by lint policy.

Typical safe manifest:

```toml
[lints]
profile = "recommended"
report_suppressed = false

[lints.rules]
"abap-lsp.select-star" = "warn"
"abap-lsp.dead-store" = "info"
```

Group and rule overrides are applied after the profile. Rule overrides are the most specific policy,
except that hard-error lints still stay at `deny`.

## Suppressions

The suppression scanner lives in `abap_lints` and consumes source text plus `abap_lexer` trivia.
Implemented forms:

```abap
DATA lv_unused TYPE i. " abap-lsp:allow(abap-lsp.dead-store)
" abap-lsp:allow-next-line(abap-lsp.select-single-without-full-key)
SELECT SINGLE carrid FROM scarr INTO @DATA(lv_carrid) WHERE carrid = @p_carrid.
* abap-lsp:allow-file(epc.unverified-open-sql-source)
gv_unused = 1 ##NEEDED.
SELECT * FROM mara INTO TABLE @DATA(lt_mara). "#EC CI_ALL_FIELDS_NEEDED
```

Rules:

- explicit abap-lsp comments accept exact lint IDs only.
- SAP pragmas and pseudo comments suppress only lints whose registry metadata lists the alias.
- broad forms such as `all`, `group:<name>`, and `#EC *` are intentionally ignored.
- when `report_suppressed = true`, source-suppressed and config-disabled diagnostics remain in the
  lint surface with suppression metadata; otherwise they are dropped before publishing.

## Snapshot And LSP Flow

Snapshot construction creates native lint diagnostics after local semantic/project facts are
available. The cache stores final lint diagnostics on `AnalysisSnapshot`, while parse diagnostics and
semantic hard diagnostics remain separate so existing consumers are not forced through lint policy.

Native LSP diagnostics use:

- `source = "abap-lsp-lints"`
- `code = <stable lint ID>`
- `codeDescription.href` pointing at the lint reference anchor when available
- severity from the effective lint level
- `data` with `lint_id`, `group`, `origin`, `suppressed`, and optional suppression metadata

Imported SAP ATC diagnostics use `source = "sap-atc"` and `data.kind = "sap_atc_lint"`. They are
filtered by the configured `[lints.sap_atc]` mode/check variant/configuration and by the target
source version. Live SAP ATC HTTP execution remains outside the Rust server for now.

## CLI Shape

Implemented surface:

```text
abap-cli lint [--json] [--pretty] [--with-project] [--all-files] [--show-suppressed] [--fail-on-warnings] [FILE|PATH]
```

Behavior:

- human output renders rustc-style diagnostics to stderr.
- `--json` writes structured output to stdout with `schema = "abap-lsp.lint"` and `version = 1`.
- `--with-project` loads workspace context around a file.
- `--all-files` loads workspace context and reports every editable workspace file rooted at `PATH`.
- `--show-suppressed` includes config-disabled and source-suppressed diagnostics.
- exit code is nonzero for parse hard errors, unsuppressed `deny` findings, or unsuppressed `warn`
  findings when `--fail-on-warnings` is set.

## Local Rule Pack

The current registry contains:

- routine/dataflow rules: unreachable code, use-before-definite-assignment, possibly unbound field
  symbols, dead stores, and unsorted `READ TABLE ... BINARY SEARCH`.
- Open SQL rules: `SELECT *`, `SELECT` in loops, `SELECT SINGLE` without all known key fields,
  `FOR ALL ENTRIES` without a visible guard, and dynamic Open SQL fragments.
- result-handling rules: ignored `AUTHORITY-CHECK` and conservative ignored `CALL FUNCTION`
  results.
- EPC-backed hard errors for unverified Open SQL source, invalid Open SQL target shape, and missing
  `TABLES` declarations.

The exact registry, defaults, groups, origins, and suppression aliases are maintained in
[`docs/reference/lints.md`](../reference/lints.md), with tests checking that documented IDs and
anchors stay in sync with the registry.

## Test Coverage

Existing lint-related tests cover:

- registry uniqueness and documentation anchors.
- safe profile behavior and hard-error clamping.
- config diagnostics for unknown groups and rules.
- SAP pragma, SAP pseudo-comment, and explicit abap-lsp source suppressions.
- local lint emission for the current rule pack.
- LSP native lint diagnostics, docs links, manifest diagnostics, and imported SAP ATC diagnostics.
- CLI parsing, JSON shape, `--show-suppressed`, `--all-files`, and warning-failure behavior.

Useful focused commands while changing lint code:

```bat
cargo test -p abap_lints
cargo test -p abap_cache lint
cargo test -p abap_lsp lint
cargo test -p abap_cli lint
```

Run `cargo test --workspace` before landing broader lint changes when feasible.

## Next Work

High-value next steps:

1. Add code actions for safe suppressions and narrow fix-its.
2. Add block suppressions only if real codebases need them; keep exact-ID suppressions as the
   default.
3. Add more SAP-inspired local rules with conservative defaults, especially hard-coded text,
   swallowed exceptions, broad `CATCH cx_root`, missing `sy-subrc` checks, and obsolete syntax.
4. Normalize imported SAP ATC findings into the CLI/all-files report path, not only LSP publishing.
5. Add performance guardrails for all-files linting on large workspaces.
