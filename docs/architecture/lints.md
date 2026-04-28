# ABAP Lint Architecture

## Goals

Add a Clippy-like lint layer on top of the existing parser, semantic, project, and routine-analysis
pipeline. The lint layer must provide:

- stable native lint IDs independent of Rust enum names
- groups and configurable levels in `abapls.toml`
- SAP ATC, Extended Program Check, and Code Inspector alias metadata
- source suppressions from ABAP pragmas, pseudo comments, and abap-lsp comments
- LSP diagnostics with stable `source`, `code`, `codeDescription`, and `data`
- CLI output, starting with `abap-cli lint --json`
- a clear extension point for a future remote SAP ATC provider

The first implementation should not add external dependencies. `serde`, `serde_json`, and `toml`
are already workspace dependencies.

## Current Code References

These are the integration points this design is pinned to:

- `crates/abap_symbols/src/def_map.rs:298` defines `DiagnosticKind`.
- `crates/abap_symbols/src/def_map.rs:334` defines the current semantic `Diagnostic` as
  `{ kind, range, message }`.
- `crates/abap_symbols/src/dossier.rs:1652` has the existing `DiagnosticKind` to snake-case name
  mapping used by dossier export.
- `crates/abap_symbols/src/static_analysis.rs` derives compact routine findings from routine
  diagnostics.
- `crates/abap_cache/src/lib.rs:78` defines `AnalysisSnapshot`.
- `crates/abap_cache/src/lib.rs:111` defines `SnapshotBuildPlan`.
- `crates/abap_cache/src/lib.rs:10803` materializes project-wide snapshot artifacts.
- `crates/abap_cache/src/lib.rs:10938` merges routine diagnostics back into `UnitAnalysis`.
- `crates/abap_cache/src/workspace.rs:29` defines `WorkspaceManifest`.
- `crates/abap_cache/src/workspace.rs:337` parses `abapls.toml`.
- `crates/abap_lsp/src/lib.rs:3814` maps `DiagnosticKind` to LSP severity today.
- `crates/abap_lsp/src/lib.rs:3849` maps the one existing LSP diagnostic code today.
- `crates/abap_lsp/src/lib.rs:3965` builds workspace-aware LSP diagnostics.
- `crates/abap_lsp/src/lib.rs:4066` publishes LSP diagnostics.
- `crates/abap_cli/src/main.rs:52` defines the current hand-rolled CLI command enum.

## Layering

Keep dependency flow one-way.

Recommended placement:

- Add `crates/abap_symbols/src/lints.rs`.
- Re-export the public lint data types from `crates/abap_symbols/src/lib.rs`.
- Parse lint configuration in `crates/abap_cache/src/workspace.rs` as part of `WorkspaceManifest`.
- Apply configuration and suppressions in `crates/abap_cache/src/lib.rs` while materializing
  snapshots.
- Render LSP diagnostics in `crates/abap_lsp`.
- Render CLI diagnostics in `crates/abap_cli`.

This keeps `abap_symbols` as the owner of semantic/routine diagnostic meaning and avoids a circular
dependency between `abap_cache` and `abap_lsp`. If the lint registry grows large enough, it can later
move to a low-level `abap_lints` crate that depends only on `abap_lexer` and `serde`; that is not
necessary for the first implementation.

## Data Model

### IDs

Use snake-case stable IDs for native lints. The initial IDs should match the existing dossier names,
for example `dead_store`, `use_before_definite_assignment`, and
`unsorted_read_table_binary_search`.

Rules:

- IDs are ASCII and case-insensitive at parse time.
- Native IDs normalize to lowercase snake case.
- Unknown IDs in config should produce a manifest diagnostic, not silently no-op.
- External provider IDs are allowed in quoted TOML keys for future use, for example
  `"sap_atc:CHECK_ID/MESSAGE_ID"`.

```rust
pub struct LintId(Arc<str>);
```

`LintId` should expose:

- `as_str()`
- `parse_normalized(&str) -> Result<LintId, LintIdParseError>`
- `is_native()`

### Levels

Use Clippy-like project levels, then map them to LSP severity.

```rust
pub enum LintLevel {
    Allow,
    Hint,
    Info,
    Warn,
    Deny,
}
```

Accepted config aliases:

- `off` and `allow` -> `Allow`
- `hint` -> `Hint`
- `info` -> `Info`
- `warn` and `warning` -> `Warn`
- `deny`, `error`, and `err` -> `Deny`

`Allow` means the diagnostic is not emitted unless the caller explicitly asks for suppressed or
allowed diagnostics. `Deny` maps to an LSP error and a CLI failure by default.

### Registry

`abap_symbols::lints` should expose a static registry:

```rust
pub struct LintSpec {
    pub id: &'static str,
    pub title: &'static str,
    pub origin: LintOrigin,
    pub default_level: LintLevel,
    pub groups: &'static [&'static str],
    pub tags: &'static [LintTag],
    pub docs_slug: &'static str,
    pub sap_aliases: SapAliases,
}

pub enum LintOrigin {
    NativeSemantic,
    NativeRoutine,
    NativeProject,
    NativeOpenSql,
    RemoteSapAtc,
}

pub enum LintTag {
    Correctness,
    Suspicious,
    Performance,
    Style,
    Security,
    Compatibility,
    Experimental,
    Remote,
    Unnecessary,
}

pub struct SapAliases {
    pub atc_checks: &'static [&'static str],
    pub code_inspector_checks: &'static [&'static str],
    pub extended_program_check: &'static [&'static str],
    pub pragmas: &'static [&'static str],
    pub pseudo_comments: &'static [&'static str],
}
```

`docs_slug` should produce stable documentation URLs. Prefer a local docs path first:

- local path: `docs/reference/lints.md#dead_store`
- public URL used by LSP `codeDescription.href`:
  `https://github.com/1Meracle1/abap-lsp/blob/main/docs/reference/lints.md#dead_store`

The public URL is best-effort. The stable contract is the lint ID, not the GitHub path.

### Effective Diagnostics

Add a snapshot-level lint diagnostic type:

```rust
pub struct LintDiagnostic {
    pub id: LintId,
    pub range: TextRange,
    pub message: String,
    pub origin: LintOrigin,
    pub default_level: LintLevel,
    pub effective_level: LintLevel,
    pub groups: Vec<Arc<str>>,
    pub tags: Vec<LintTag>,
    pub docs_url: Option<Arc<str>>,
    pub sap_aliases: Vec<SapAlias>,
    pub source_kind: Option<DiagnosticKind>,
}
```

`source_kind` is a migration bridge from the current `DiagnosticKind` world. New lints do not need
to have a `DiagnosticKind`.

Add to `AnalysisSnapshot`:

```rust
pub lint_diagnostics: Arc<[LintDiagnostic]>,
```

Do not remove `UnitAnalysis::diagnostics` in the first pass. It remains the raw semantic/routine
diagnostic surface used by older code and tests.

## Mapping Existing Diagnostics

Every current `DiagnosticKind` must map exhaustively to one native lint. The mapping preserves the
existing range and message.

| `DiagnosticKind` | Lint ID | Origin | Default | Groups |
| --- | --- | --- | --- | --- |
| `DuplicateDeclaration` | `duplicate_declaration` | native semantic | deny | correctness |
| `ShadowedSymbol` | `shadowed_symbol` | native semantic | warn | suspicious |
| `MismatchedStructuredDeclaration` | `mismatched_structured_declaration` | native semantic | deny | correctness |
| `UnresolvedReference` | `unresolved_reference` | native semantic | deny | correctness |
| `UnresolvedInclude` | `unresolved_include` | native project | deny | correctness |
| `IncludeCycle` | `include_cycle` | native project | deny | correctness |
| `WrongNamespace` | `wrong_namespace` | native semantic | deny | correctness |
| `UnknownField` | `unknown_field` | native semantic | deny | correctness |
| `InvalidBuiltinNamedArgument` | `invalid_builtin_named_argument` | native semantic | deny | correctness |
| `InvalidPerformCall` | `invalid_perform_call` | native semantic | deny | correctness |
| `AbstractClassInstantiation` | `abstract_class_instantiation` | native semantic | deny | correctness |
| `MissingMethodImplementation` | `missing_method_implementation` | native semantic | deny | correctness |
| `MissingSuperConstructorCall` | `missing_super_constructor_call` | native semantic | deny | correctness |
| `InvalidObjectTypeReference` | `invalid_object_type_reference` | native semantic | deny | correctness |
| `IncompatibleAssignmentType` | `incompatible_assignment_type` | native semantic | deny | correctness |
| `IncompatibleArgumentType` | `incompatible_argument_type` | native semantic | warn | correctness |
| `UnknownNamedParameter` | `unknown_named_parameter` | native semantic | deny | correctness |
| `DuplicateNamedParameter` | `duplicate_named_parameter` | native semantic | deny | correctness |
| `MissingRequiredParameter` | `missing_required_parameter` | native semantic | deny | correctness |
| `UnverifiedOpenSqlSource` | `unverified_open_sql_source` | native Open SQL | deny | correctness, compatibility |
| `InvalidOpenSqlIntoTarget` | `invalid_open_sql_into_target` | native Open SQL | deny | correctness |
| `InvalidOpenSqlSyntax` | `invalid_open_sql_syntax` | native Open SQL | deny | correctness, compatibility |
| `InvalidConstructorForIteratorReuse` | `invalid_constructor_for_iterator_reuse` | native semantic | deny | correctness |
| `MissingTablesDeclaration` | `missing_tables_declaration` | native semantic | deny | correctness, compatibility |
| `UnreachableCode` | `unreachable_code` | native routine | warn | suspicious |
| `UseBeforeDefiniteAssignment` | `use_before_definite_assignment` | native routine | warn | correctness |
| `PossiblyUnboundFieldSymbol` | `possibly_unbound_field_symbol` | native routine | warn | correctness |
| `DeadStore` | `dead_store` | native routine | warn | suspicious |
| `UnsortedReadTableBinarySearch` | `unsorted_read_table_binary_search` | native routine | warn | performance, correctness |

The defaults intentionally preserve today's LSP severity behavior as closely as possible:

- current errors remain `deny`
- current warnings remain `warn`
- `MissingMethodImplementation` keeps a stable LSP code, but moves to the general lint-code path

Implementation detail:

```rust
pub fn lint_spec_for_diagnostic_kind(kind: DiagnosticKind) -> &'static LintSpec
```

Use a `match` without `_` so adding a `DiagnosticKind` fails to compile until a lint mapping is
chosen.

## Groups

Initial built-in groups:

- `all`: every native lint
- `correctness`: likely runtime, compile, resolution, dataflow, or type problems
- `suspicious`: code that is legal but likely unintended
- `performance`: inefficient or performance-risky ABAP idioms
- `compatibility`: release, DDIC, Open SQL, and legacy syntax compatibility checks
- `style`: reserved for future formatting/naming rules
- `sap_atc`: native lints that have SAP ATC, EPC, or Code Inspector aliases

Groups are labels, not a tree. A lint can be in several groups. Effective level precedence is:

1. rule-specific config in `[lints.rules]`
2. group config in `[lints.groups]`, with the most severe configured level winning when multiple
   groups match
3. `[lints] default_level`, if set
4. registry default level
5. source suppression, which can only lower to `Allow` unless a future `Forbid` level is added

## `abapls.toml` Schema

Extend `WorkspaceManifest` with:

```rust
#[serde(default)]
pub lints: ManifestLints,
```

Suggested TOML:

```toml
version = 1

[lints]
enabled = true
# Optional. Omit to use the built-in default for each lint.
default_level = "warn"
respect_sap_suppressions = true
respect_abap_lsp_suppressions = true

[lints.groups]
correctness = "deny"
performance = "warn"
suspicious = "warn"
style = "allow"

[lints.rules]
dead_store = "warn"
use_before_definite_assignment = "deny"
unsorted_read_table_binary_search = "warn"
unverified_open_sql_source = "info"

[lints.sap_atc]
mode = "off" # "off" | "manual" | "on-save"
check_variant = "DEFAULT"
configuration = ""
```

Serde model:

```rust
pub struct ManifestLints {
    pub enabled: bool,
    pub default_level: Option<String>,
    pub respect_sap_suppressions: bool,
    pub respect_abap_lsp_suppressions: bool,
    pub groups: BTreeMap<String, ManifestLintLevel>,
    pub rules: BTreeMap<String, ManifestLintRule>,
    pub sap_atc: ManifestSapAtcLints,
}

#[serde(untagged)]
pub enum ManifestLintRule {
    Level(String),
    Detailed {
        level: String,
        reason: Option<String>,
        expires: Option<String>,
    },
}
```

`expires` stays a string in the first implementation. Do not add a date/time dependency.

Normalization in `normalize_manifest` should:

- normalize lint IDs and group names to lowercase
- normalize levels and reject unknown levels
- retain unknown IDs as manifest errors or warnings so users see typos
- clamp future numeric SAP ATC settings if added

`[lints.sap_atc]` now controls imported remote SAP ATC findings. The first implementation accepts
client- or CLI-provided results over the `abapls/sapAtcResultsUpdated` protocol and renders them as
`source = "sap-atc"` diagnostics. Live SAP ATC HTTP execution still belongs in the VS Code client or
`abap_adt_cli` until the ADT ATC API is validated against a configured SAP system.

Future remote provider fields can extend this table without changing the native lint schema:

```toml
[lints.sap_atc]
mode = "on-save"
check_variant = "ABAP_CLOUD_DEVELOPMENT_DEFAULT"
configuration = "CLOUD_READINESS"
object_set = "workspace"
timeout_ms = 30000
include_unmapped_remote_findings = true
unmapped_remote_level = "warn"
```

The remote provider should use existing workspace `connection` settings instead of adding secrets to
`[lints.sap_atc]`.

## Suppression Model

Suppressions are applied after raw lint diagnostics are created and after project config computes
the effective level. A suppression only hides diagnostics whose effective level is not `Deny` if the
project later adds a `forbid`-like level. In the first implementation, all emitted native lints can
be suppressed.

Current implementation notes:

- The suppression scanner lives in `abap_lints` and consumes the source text plus
  `abap_lexer::LexedSource`.
- Lint IDs use the current registry IDs such as `abap-lsp.dead-store` and
  `epc.unverified-open-sql-source`. The scanner normalizes case and `_` versus `-`, but it does not
  infer short IDs such as `dead-store` for `abap-lsp.dead-store`.
- SAP pragmas and pseudo comments match only `LintMetadata.sap_aliases`; an arbitrary SAP-looking
  code never suppresses a native lint unless the registry exposes that alias.
- abap-lsp allow comments match exact lint IDs only. Group-level, `all`, and block suppressions are
  intentionally not implemented yet.
- When `[lints].report_suppressed = true`, source-suppressed and config-disabled lint diagnostics
  remain in the lint surface at `info` level with `suppressed = true` plus suppression metadata.
  Otherwise they are dropped before LSP publishing and cache consumers see only unsuppressed lints.

### Statement Association

Associate a lint diagnostic with the statement containing `diagnostic.range.start`.

Use `ParseResult.tokens` and `ParseResult.lexed`:

- find the previous significant period before the diagnostic start
- find the next significant period after the diagnostic start
- statement range is previous period end to next period end
- include trailing trivia on the last significant token for inline comments and pragmas
- include leading comment-only lines immediately before the first token for next-line and file
  suppressions

Fallback to the physical line if statement association fails.

### SAP Pragmas

ABAP pragmas are already tokenized as `TriviaKind::Pragma` when the text starts with `##`.

Rules:

- `##...` suppressions apply to the statement they are attached to.
- Match pragma names case-insensitively against `LintMetadata.sap_aliases`.
- Do not treat an arbitrary `##FOO` as an abap-lsp suppression.
- Examples are recognized only through registry aliases, for example `##NEEDED` for
  `abap-lsp.dead-store`.

### EPC and Code Inspector Pseudo Comments

Pseudo comments are quote comments containing `#EC`.

Rules:

- `"#EC <code>` suppressions apply to the statement carrying the trailing comment.
- Match `<code>` case-insensitively against `LintMetadata.sap_aliases`.
- Support multiple whitespace-separated pseudo-comment codes on the same comment.
- Do not make `#EC *` suppress all lints unless a future compatibility mode explicitly enables it.

Examples:

```abap
CALL 'ThWpInfo' ID 'OPCODE' FIELD lv_opcode. "#EC CI_CCALL
DATA lv_text TYPE string VALUE 'x'. "#EC NOTEXT
```

### abap-lsp Allow Comments

Use explicit abap-lsp comments for native lint IDs. These comments should not conflict with SAP
syntax and should be easy to search.

Supported first-pass forms:

```abap
DATA lv_unused TYPE i. " abap-lsp:allow(abap-lsp.dead-store)
" abap-lsp:allow-next-line(abap-lsp.use-before-definite-assignment)
lv_value = lv_other.
* abap-lsp:allow-file(epc.unverified-open-sql-source)
```

Rules:

- `allow(...)` in trailing trivia suppresses the current statement.
- `allow-next-line(...)` in a full-line comment suppresses the next non-comment statement.
- `allow-file(...)` suppresses the listed lint IDs for the whole file.
- Accepted items are exact lint IDs only. `group:<name>`, `all`, malformed items, and unknown future
  syntax do not suppress anything.
- Keep a small `SuppressionInfo` on suppressed diagnostics for `--show-suppressed` and future code
  actions.

Block suppressions can be added later:

```abap
" abap-lsp: allow-begin(dead_store)
" abap-lsp: allow-end(dead_store)
```

Do not implement block suppressions in the first pass unless a real use case requires them.

## Snapshot Construction

Native lint analysis should run in `materialize_snapshots` after:

1. project analysis is built
2. routine analysis is optionally built
3. routine diagnostics are merged into each `UnitAnalysis` by `augment_unit_with_routine_diagnostics`

Then, for each prepared document:

1. start from `unit.diagnostics`
2. map each `DiagnosticKind` to a `LintSpec`
3. compute effective level from manifest config
4. inspect `ParseResult.lexed` for suppressions
5. drop `Allow` or suppressed diagnostics from `snapshot.lint_diagnostics`
6. sort by range, ID, and message

`SnapshotBuildPlan::EDITOR_WORKSPACE` already builds routine analysis, so editor diagnostics can
include the current native routine lints. `SnapshotBuildPlan::EFFECTIVE_SOURCE`,
`REMOTE_CANDIDATES`, and `CALL_GRAPH` can attach an empty lint list because they do not need user
diagnostics. `SnapshotBuildPlan::SEMANTIC_DOSSIER` should include lints so dossier and future CLI
exports can share the same surface.

API changes:

- Introduce `AnalysisOptions { build_plan, lint_config }` or extend the existing build-plan APIs
  with defaulted lint config helpers.
- Store a lint-config fingerprint in `CachedWorkspaceAnalysis`.
- Include the fingerprint in cache reuse checks, so changing `abapls.toml` rebuilds diagnostics even
  if document text did not change.
- LSP workspace rebuild paths already refresh manifest state when `abapls.toml` changes; pass
  `workspace.manifest.as_ref().map(|m| &m.lints)` through the cache call.

Avoid storing heavy suppression indexes in `AnalysisSnapshot`. The snapshot should hold the final
diagnostics and enough metadata to render them.

## LSP Diagnostics

Parse diagnostics stay unchanged:

- `source = "abap-parser"`
- `code = None`

Native lint diagnostics:

- `source = "abap-lsp"`
- `code = String(lint_id)`
- `codeDescription.href = docs_url` when available
- `severity` from effective level:
  - `Deny` -> `DiagnosticSeverity::ERROR`
  - `Warn` -> `DiagnosticSeverity::WARNING`
  - `Info` -> `DiagnosticSeverity::INFORMATION`
  - `Hint` -> `DiagnosticSeverity::HINT`
  - `Allow` -> not emitted
- `tags`:
  - `DiagnosticTag::UNNECESSARY` for `dead_store` and probably `unreachable_code`
  - `DiagnosticTag::DEPRECATED` only for future deprecation lints
- `data`:

```json
{
  "kind": "lint",
  "lintId": "dead_store",
  "origin": "native_routine",
  "defaultLevel": "warn",
  "effectiveLevel": "warn",
  "groups": ["suspicious"],
  "tags": ["suspicious", "unnecessary"],
  "docsUrl": "https://github.com/1Meracle1/abap-lsp/blob/main/docs/reference/lints.md#dead_store",
  "sapAliases": [
    { "kind": "pragma", "id": "NEEDED" }
  ],
  "sourceDiagnosticKind": "DeadStore"
}
```

Remote SAP ATC diagnostics, when implemented:

- `source = "sap-atc"`
- `code = String(mapped_native_lint_id)` if mapped, otherwise a stable external ID such as
  `sap_atc:<check>/<message>`
- `data.kind = "sap_atc_lint"`
- include SAP object key, check ID, message ID, ATC priority, provider profile, and cache age

`semantic_diagnostic_severity` and `semantic_diagnostic_code` should become migration helpers or be
removed after LSP rendering switches to `snapshot.lint_diagnostics`.

## CLI Shape

Add:

```text
abap-cli lint [--json] [--pretty] [--with-project] [--all-files] [--show-suppressed] [FILE|PATH]
```

Recommended behavior:

- Without `--json`, render rustc-style diagnostics to stderr.
- Human output should prefix severity and ID, for example `warning[dead_store]: value assigned to lv_x is never read`.
- `--json` writes structured output to stdout.
- Exit code is `1` if parse errors or `Deny` lint diagnostics exist.
- Warnings do not fail by default. Add `--fail-on-warnings` later if needed.
- `--with-project` loads workspace context around a file, matching `analyze --with-project`.
- `--all-files` lints all editable workspace files rooted at `PATH`.
- Stdin uses default lint config and a single-file snapshot.

JSON shape:

```json
{
  "phase": "lint",
  "workspaceRootUri": "file:///D:/dev/rust/abap-lsp",
  "manifestPresent": true,
  "diagnostics": [
    {
      "uri": "file:///D:/repo/src/zrep.abap",
      "range": [120, 128],
      "level": "warn",
      "lintId": "dead_store",
      "message": "value assigned to 'lv_count' is never read",
      "origin": "native_routine",
      "groups": ["suspicious"],
      "tags": ["suspicious", "unnecessary"],
      "docsUrl": "https://github.com/1Meracle1/abap-lsp/blob/main/docs/reference/lints.md#dead_store",
      "sapAliases": []
    }
  ],
  "suppressed": [],
  "summary": {
    "deny": 0,
    "warn": 1,
    "info": 0,
    "hint": 0,
    "suppressed": 0
  }
}
```

The current `check` command can remain front-end only. Do not overload it with configurable lint
semantics.

## SAP ATC, EPC, and Code Inspector Mapping

Native lints should carry alias metadata where there is a clear local equivalent. Alias metadata is
for interoperability and suppression recognition; it does not claim byte-for-byte parity with SAP
tools.

Initial priority:

| Native lint | SAP-inspired mapping priority |
| --- | --- |
| `unreachable_code` | Extended Program Check unreachable/dead branch style findings |
| `dead_store` | EPC/Code Inspector unused assignment style findings; recognize `##NEEDED` when the assignment/declaration is intentionally retained |
| `unsorted_read_table_binary_search` | Code Inspector/ATC performance check for binary search on non-sorted data |
| `use_before_definite_assignment` | EPC data-flow/use-before-assignment family |
| `possibly_unbound_field_symbol` | EPC field-symbol assignment/boundness family |
| `unverified_open_sql_source` | ATC/Open SQL/DDIC verification family |
| `invalid_open_sql_syntax` | EPC/Open SQL syntax compatibility family |
| `missing_tables_declaration` | legacy TABLES/DDIC declaration compatibility family |
| `shadowed_symbol` | Code Inspector naming/shadowing style checks |

Do not hard-code broad SAP names that are not verified by local fixtures. Add aliases incrementally
with tests that show the suppression syntax actually appears in ABAP code seen by the parser.

For remote ATC:

1. Run native lint analysis first.
2. Fetch or import remote findings as a separate provider.
3. Map remote findings to native IDs when an alias exists.
4. Deduplicate by `(uri/object, range, native lint ID, message fingerprint)`.
5. Emit imported findings with stable external IDs when no native mapping exists.
6. Cache remote results by object/version/check variant and, for live execution, SAP system and
   provider version.

## Initial Rule Inventory

Priority 1: current routine/static findings

- `use_before_definite_assignment`
- `possibly_unbound_field_symbol`
- `dead_store`
- `unreachable_code`
- `unsorted_read_table_binary_search`

Priority 2: current semantic correctness findings

- `unresolved_reference`
- `unknown_field`
- `wrong_namespace`
- `incompatible_assignment_type`
- `incompatible_argument_type`
- `unknown_named_parameter`
- `duplicate_named_parameter`
- `missing_required_parameter`
- `invalid_builtin_named_argument`
- `invalid_perform_call`
- `invalid_constructor_for_iterator_reuse`
- `abstract_class_instantiation`
- `missing_super_constructor_call`
- `missing_method_implementation`
- `invalid_object_type_reference`
- `duplicate_declaration`
- `mismatched_structured_declaration`

Priority 3: project/include/Open SQL findings

- `unresolved_include`
- `include_cycle`
- `unverified_open_sql_source`
- `invalid_open_sql_into_target`
- `invalid_open_sql_syntax`
- `missing_tables_declaration`

Priority 4: new SAP-inspired native lints after the framework lands

- dynamic SQL without whitelist (first local pack: `abap-lsp.dynamic-open-sql`)
- `SELECT *` outside explicitly allowed contexts (first local pack: `abap-lsp.select-star`)
- database access in loops (first local pack: `abap-lsp.select-in-loop`)
- `FOR ALL ENTRIES` without an initial-table guard (first local pack:
  `abap-lsp.for-all-entries-without-guard`)
- missing `ORDER BY` before order-dependent reads (covered by `abap-lsp.unsorted-read-table-binary-search`)
- `SELECT SINGLE` without a full known DDIC/repository primary key (first local pack:
  `abap-lsp.select-single-without-full-key`)
- obsolete statements and syntax forms
- hard-coded text literals without text symbol or pragma
- `CALL 'SYSTEM'` or kernel C calls without approved pseudo comment
- authority-check result ignored (first local pack: `abap-lsp.ignored-authority-check`)
- conservative CALL FUNCTION result ignored or overwritten before check (first local pack:
  `abap-lsp.ignored-call-function-result`)
- broad `CATCH cx_root` without handling
- empty `CATCH` or swallowed exceptions
- missing `sy-subrc` check after statements where it matters

## Test Strategy

`abap_symbols`:

- registry contains each native ID once
- each `DiagnosticKind` maps exhaustively to a `LintSpec`
- default levels preserve current LSP severities
- SAP alias matching is case-insensitive

`abap_cache`:

- `WorkspaceManifest` parses `[lints]`, `[lints.groups]`, `[lints.rules]`, and
  `[lints.sap_atc]`
- normalization handles level aliases and snake-case IDs
- unknown lint IDs and groups become manifest diagnostics
- snapshot lint diagnostics honor default levels, group overrides, rule overrides, and disabled
  lints
- lint-config fingerprint invalidates cached diagnostics

Suppressions:

- `##...` pragma suppresses only mapped lint aliases on the same statement
- `"#EC ...` suppresses only mapped pseudo-comment aliases on the same statement
- `abap-lsp: allow(...)` suppresses current statement
- `abap-lsp: allow-next-line(...)` suppresses next non-comment statement
- `abap-lsp: allow-file(...)` suppresses file-wide
- unknown suppression IDs do not suppress anything

LSP:

- native lint diagnostics use `source = "abap-lsp"`
- `code` is the lint ID
- severity follows effective level
- `codeDescription.href` is populated when docs are available
- `data` is stable camelCase JSON
- parse diagnostics remain sourced from `abap-parser`

CLI:

- `parse_cli_args` accepts `lint`
- `lint --json` emits the documented shape
- human `lint` emits warning/error IDs
- exit code is nonzero for parse errors and `Deny` lints
- `--with-project` includes routine/project diagnostics that need workspace context

Regression coverage should reuse `examples/*.abap` where possible and add focused tests near the
crate that owns the behavior.

## Migration Plan

1. Add `abap_symbols::lints` with registry, levels, groups, SAP alias structs, and exhaustive
   `DiagnosticKind` mapping. No behavior change.
2. Add manifest parsing for lint config in `abap_cache::workspace`. Keep defaults equivalent to the
   current behavior.
3. Add snapshot `lint_diagnostics` derived from `UnitAnalysis::diagnostics`, with no suppressions
   yet. Keep LSP using old diagnostics until parity tests pass.
4. Switch LSP rendering to `snapshot.lint_diagnostics`. Preserve parse diagnostics and current
   remote lookup message rewriting.
5. Add suppression scanning and suppressed-diagnostic metadata.
6. Add `abap-cli lint --json` and then human output.
7. Add `docs/reference/lints.md` with per-rule documentation and generated or hand-maintained
   anchors.
8. Add native SAP-inspired rules one by one after the framework is stable.
9. Add the remote SAP ATC provider as a separate provider that merges into the same lint diagnostic
   surface.

## Exact Next Implementation Steps

1. Create `crates/abap_symbols/src/lints.rs` with `LintId`, `LintLevel`, `LintOrigin`, `LintTag`,
   `LintSpec`, `SapAliases`, `LintDiagnostic`, and the static registry.
2. Add `lint_spec_for_diagnostic_kind(kind: DiagnosticKind) -> &'static LintSpec` with an exhaustive
   `match`.
3. Re-export the lint types from `crates/abap_symbols/src/lib.rs`.
4. Extend `WorkspaceManifest` in `crates/abap_cache/src/workspace.rs` with a
   `lints: ManifestLints` field, defaults, normalization, and unit tests for the TOML shown above.
5. Add an `EffectiveLintConfig` builder that resolves `[lints]`, `[lints.groups]`, and
   `[lints.rules]` against the registry and records unknown IDs/groups.
6. Extend `AnalysisSnapshot` with `lint_diagnostics: Arc<[LintDiagnostic]>` and update snapshot
   clone/materialization sites.
7. Extend cache build APIs with lint config or an `AnalysisOptions` wrapper and include a config
   fingerprint in `CachedWorkspaceAnalysis`.
8. In `materialize_snapshots`, derive lint diagnostics after
   `augment_unit_with_routine_diagnostics`.
9. Update `crates/abap_lsp/src/lib.rs` to render `snapshot.lint_diagnostics` with the LSP
   conventions above.
10. Preserve the current workspace-specific remote lookup message rewrite by applying it to the
    relevant lint diagnostic before LSP conversion.
11. Add suppression scanning using `ParseResult.lexed` and wire it into lint diagnostic filtering.
12. Add `lint` to `crates/abap_cli/src/main.rs`, starting with `abap-cli lint --json`.
13. Add `docs/reference/lints.md` with initial rule entries matching every native ID.
14. Run `cargo test -p abap_symbols`, `cargo test -p abap_cache`, `cargo test -p abap_lsp`, and
    `cargo test -p abap_cli`, then `cargo test --workspace`.
