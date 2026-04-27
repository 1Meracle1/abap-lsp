# Native Lints

The language server emits native lint diagnostics with stable IDs. Configure them in
`abapls.toml` with `[lints.rules]` and suppress individual statements with
`abap-lsp:allow(<lint-id>)`.

SAP pragma and pseudo-comment suppressions only work when the lint exposes the listed alias.

| ID | Default | Group | Origin | SAP suppression aliases |
| --- | --- | --- | --- | --- |
| `abap-lsp.unreachable-code` | `warn` | `correctness` | `abap-lsp` | none |
| `abap-lsp.use-before-definite-assignment` | `warn` | `correctness` | `abap-lsp` | none |
| `abap-lsp.possibly-unbound-field-symbol` | `warn` | `correctness` | `abap-lsp` | none |
| `abap-lsp.dead-store` | `warn` | `style` | `abap-lsp` | `##NEEDED` |
| `abap-lsp.unsorted-read-table-binary-search` | `warn` | `correctness` | `abap-lsp` | none |
| `abap-lsp.select-star` | `info` | `performance` | `sap-code-inspector` | `"#EC CI_ALL_FIELDS_NEEDED` |
| `abap-lsp.select-in-loop` | `warn` | `performance` | `sap-code-inspector` | `"#EC CI_SEL_NESTED` |
| `abap-lsp.for-all-entries-without-guard` | `warn` | `correctness` | `sap-code-inspector` | `"#EC CI_FAE_LINES_ENSURED` |
| `abap-lsp.dynamic-open-sql` | `info` | `security` | `sap-code-inspector` | none |
| `abap-lsp.ignored-authority-check` | `warn` | `security` | `sap-atc` | none |
| `epc.unverified-open-sql-source` | `deny` | `correctness` | `sap-extended-program-check` | `"#EC extended-program-check` |
| `epc.invalid-open-sql-into-target` | `deny` | `correctness` | `sap-extended-program-check` | `"#EC extended-program-check` |
| `epc.missing-tables-declaration` | `deny` | `correctness` | `sap-extended-program-check` | `"#EC extended-program-check` |

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
