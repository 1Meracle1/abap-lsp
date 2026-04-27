# Native Lints

The language server emits native lint diagnostics with stable IDs. Configure them in
`abapls.toml` with `[lints.rules]` and suppress individual statements with
`abap-lsp:allow(<lint-id>)`.

Lint IDs are stable, lowercase ASCII strings. They are documented as part of the CLI and LSP
surface and should not be renamed; add a new ID instead when a rule's meaning changes
incompatibly.

SAP pragma and pseudo-comment suppressions only work when the lint exposes the listed alias.

The `epc.*` entries represent semantic hard errors surfaced through lint metadata. Lint profiles,
group overrides, and rule overrides do not lower those IDs below `deny`, so parser and semantic hard
errors stay visible even when a project uses `profile = "none"`.

## CLI Usage

Run native lints for a single file:

```bat
cargo run -p abap_cli -- lint path\to\zreport.abap
```

Human output is silent when no findings are emitted. When findings exist, diagnostics are rendered
to stderr in a rustc-style format such as `warning[abap-lsp.dead-store]: ...`.

Use JSON for CI or downstream tooling:

```bat
cargo run -p abap_cli -- lint --json --pretty path\to\zreport.abap
cargo run -p abap_cli -- lint --json --with-project path\to\zreport.abap
```

`--with-project` discovers the workspace around `FILE`, applies `[lints]` from `abapls.toml`, and
uses the same project-loading conventions as `abap-cli analyze --json --with-project`. Without
`--with-project`, the command uses a single-file snapshot and the default lint profile.

JSON output uses top-level `schema = "abap-lsp.lint"` and `version = 1`, includes target/workspace
metadata, `findings`, `hard_errors`, and `summary` counts by level and group. Warn-only findings do
not fail the process. The exit status is nonzero when parse hard errors prevent linting or when at
least one unsuppressed `deny` finding is emitted.

| ID | Default | Group | Origin | SAP suppression aliases |
| --- | --- | --- | --- | --- |
| `abap-lsp.unreachable-code` | `warn` | `correctness` | `abap-lsp` | none |
| `abap-lsp.use-before-definite-assignment` | `warn` | `correctness` | `abap-lsp` | none |
| `abap-lsp.possibly-unbound-field-symbol` | `warn` | `correctness` | `abap-lsp` | none |
| `abap-lsp.dead-store` | `info` | `style` | `abap-lsp` | `##NEEDED` |
| `abap-lsp.unsorted-read-table-binary-search` | `info` | `correctness` | `abap-lsp` | none |
| `abap-lsp.select-star` | `info` | `performance` | `sap-code-inspector` | `"#EC CI_ALL_FIELDS_NEEDED` |
| `abap-lsp.select-in-loop` | `info` | `performance` | `sap-code-inspector` | `"#EC CI_SEL_NESTED` |
| `abap-lsp.for-all-entries-without-guard` | `info` | `correctness` | `sap-code-inspector` | `"#EC CI_FAE_LINES_ENSURED` |
| `abap-lsp.dynamic-open-sql` | `info` | `security` | `sap-code-inspector` | none |
| `abap-lsp.ignored-authority-check` | `info` | `security` | `sap-atc` | none |
| `epc.unverified-open-sql-source` | `deny` | `correctness` | `sap-extended-program-check` | `"#EC extended-program-check` |
| `epc.invalid-open-sql-into-target` | `deny` | `correctness` | `sap-extended-program-check` | `"#EC extended-program-check` |
| `epc.missing-tables-declaration` | `deny` | `correctness` | `sap-extended-program-check` | `"#EC extended-program-check` |

## Rule Details

### `abap-lsp.unreachable-code`

Flags statements after control-flow terminators such as `RETURN`, `RAISE`, `LEAVE`, and `STOP`.

### `abap-lsp.use-before-definite-assignment`

Flags reads that routine dataflow cannot prove are preceded by an assignment on every path.

### `abap-lsp.possibly-unbound-field-symbol`

Flags field-symbol reads that may occur before a successful assignment or binding.

### `abap-lsp.dead-store`

Flags writes whose value is overwritten or never read. Defaults to `info` because unused writes can
be intentional in generated, tracing, or framework callback code.

### `abap-lsp.unsorted-read-table-binary-search`

Flags `READ TABLE ... BINARY SEARCH` when the analyzer cannot prove the table is sorted. Defaults to
`info` because sorting may happen through dynamic or framework-controlled paths.

### `abap-lsp.select-star`

Flags `SELECT *` and qualified star projections such as `alias~*`.

### `abap-lsp.select-in-loop`

Flags Open SQL `SELECT` statements inside `LOOP`, `DO`, or `WHILE` bodies. Defaults to `info`
because small lookup tables and buffered reads may be acceptable.

### `abap-lsp.for-all-entries-without-guard`

Flags `FOR ALL ENTRIES` without a visible non-empty-table guard. Defaults to `info` because guard
patterns can be hidden behind helper routines or generated control flow.

### `abap-lsp.dynamic-open-sql`

Flags dynamic Open SQL source, projection, and `WHERE` fragments that cannot be statically checked.

### `abap-lsp.ignored-authority-check`

Flags `AUTHORITY-CHECK` when the result in `sy-subrc` is not observed before another `sy-subrc`
write. Defaults to `info` because legacy authorization wrappers can obscure the check.

### `epc.unverified-open-sql-source`

Reports Open SQL sources that cannot be verified against local or repository metadata.

### `epc.invalid-open-sql-into-target`

Reports incompatible Open SQL `INTO` or `APPENDING` targets.

### `epc.missing-tables-declaration`

Reports classic table work-area usage that requires a top-level `TABLES` declaration.

## Local SAP-Inspired Pack

The first local pack uses facts already produced by `abap_symbols`.

- `abap-lsp.select-star`: flags `SELECT *` and qualified star projections such as `alias~*`.
- `abap-lsp.select-in-loop`: flags Open SQL `SELECT` statements whose query scope is inside a
  `LOOP`, `DO`, or `WHILE` body.
- `abap-lsp.for-all-entries-without-guard`: flags `FOR ALL ENTRIES` when the entries table is not
  protected by an enclosing `IS NOT INITIAL` guard, the `ELSE` branch of an `IS INITIAL` guard, or
  a prior `IF table IS INITIAL. RETURN. ENDIF.` guard in the same flow.
- `abap-lsp.dynamic-open-sql`: flags dynamic Open SQL source, projection, and `WHERE` fragments.
- `abap-lsp.ignored-authority-check`: flags `AUTHORITY-CHECK` when its `sy-subrc` result is not
  observed before another `sy-subrc` update becomes the latest checkable result.
