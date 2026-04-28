# ABAP Parser and Semantic Coverage Review

This review summarizes the parser, semantic-analysis, and export coverage completed in commits `4464c360` through `dbf0720b`.

## Completed Coverage

### Classic Arithmetic

- Added dedicated parser and AST coverage for `COMPUTE`, `MULTIPLY`, and `DIVIDE`, while preserving the existing `ADD` and `SUBTRACT` statement shapes.
- Added grouped arithmetic operand nodes so chained statements can be represented without statement-specific operand kinds.
- Collected arithmetic source operands as value reads and arithmetic targets as assignment sites, including read/write handling for no-`GIVING` changed operands.

User-visible gain: classic arithmetic statements now produce structured parse output, resolved symbol references, assignment sites, and value-flow edges instead of falling back to generic token handling.

### Classic String and Text Statements

- Added parser and semantic coverage for `TRANSLATE`, `SHIFT`, `SEARCH`, `CONDENSE`, `OVERLAY`, `PACK`, and `UNPACK`.
- Reused generic text source and target operand nodes to keep the AST surface compact.
- Modeled read/write text targets for mutating statements and write-only targets for `PACK`/`UNPACK`.
- Added `SEARCH` system-field updates for `sy-subrc` and `sy-fdpos`.

User-visible gain: these text operations now participate in symbol resolution, assignment/value-flow facts, and system-field reporting where the statement semantics justify it.

### Runtime Parameter, Time, and Log Statements

- Added statement classification for `GET PARAMETER`, `SET PARAMETER`, `GET TIME`, and `LOG-POINT`.
- Collected parameter IDs, `FIELD`, `SUBKEY`, and `FIELDS` operands through the existing token-wise simple-statement collector.
- Modeled `GET PARAMETER ... FIELD` and `GET TIME FIELD` targets as writes, and `SET PARAMETER ... FIELD` operands as reads.

User-visible gain: runtime parameter and log statements no longer emit keyword-style false positives for supported operand positions, and their data-bearing operands now resolve through the semantic model.

### Source Maintenance Statements

- Added parser and semantic coverage for `READ REPORT`, `INSERT REPORT`, `DELETE REPORT`, and `SYNTAX-CHECK`.
- Added generic source-maintenance operand nodes for program names, source tables, message targets, line targets, and word targets.
- Modeled report/source inputs as reads and `INTO`, `MESSAGE`, `LINE`, and `WORD` operands as assignment targets with conservative value-flow facts.
- Added conservative `sy-subrc` updates for the supported source-maintenance statements.
- Improved statement-boundary recovery before `INSERT`, `DELETE`, and `SYNTAX`.

User-visible gain: report-source maintenance code now has parse structure, symbol resolution, assignment sites, value flow, system-field facts, and better recovery after missing periods.

### Classic List-Control Statements

- Added parser and semantic coverage for `SKIP`, `ULINE`, `NEW-LINE`, `NEW-PAGE`, `RESERVE`, and `BACK`.
- Added a generic list-control operand node for value-bearing additions such as counts, positions, page sizes, line counts, page operands, and dynamic titles.
- Left static formatting words as syntax tokens rather than semantic operands.
- Improved statement-boundary recovery for `SKIP`, `ULINE`, `RESERVE`, and `BACK`.

User-visible gain: list-control operands now resolve where they carry values, without adding misleading data-flow or control-flow effects for pure list-formatting statements.

### Open SQL Common Table Expressions

- Added parser support for `WITH +cte AS ( SELECT ... ) SELECT ...`, including multiple CTE definitions.
- Preserved CTE bodies as nested `SelectQuery` nodes so existing SQL lowering can collect projections, predicates, host variables, and source facts.
- Treated local `+cte` sources as local CTE sources during SQL lowering and avoided unverified-DDIC diagnostics for those names.
- Kept external touched-object reporting focused on real external SQL sources rather than local CTE aliases.

User-visible gain: modern Open SQL CTEs now parse and export SQL facts without false source diagnostics for local `+cte` names.

### Export Surface

- Exposed the newly collected facts through existing semantic dossier sections: `references`, `assignment_sites`, `value_flow_edges`, `system_field_updates`, and `sql.queries`.
- Documented semantic dossier schema version 5 and the local CTE source export behavior.
- Added call-dataflow regression coverage proving classic text assignment sites feed existing parameter provenance.

User-visible gain: downstream consumers can use the new parser and semantic coverage through the current dossier schema without adopting new top-level fields.

## Validation Performed

- `cargo test --workspace` was attempted on 2026-04-28. It failed during compilation of the `abap_lsp` example `remote_dependency_wave_perf` with `LINK : fatal error LNK1318: Unexpected PDB error; LIMIT (12)`, not with a Rust test assertion.
- `cargo test -p abap_parser -p abap_symbols -p abap_cache` passed on 2026-04-28.
- Focused regressions recorded during implementation passed for arithmetic, text, runtime parameter/log, source-maintenance, list-control, Open SQL CTE, dossier export, and call-dataflow coverage.

## Known Residual Gaps

- Runtime parameter, time, and log statements still do not emit statement-specific system-field updates. That is intentional until there is a defensible model for the exact updates.
- Classic list-control statements do not emit routine or control-flow effects. Current coverage is limited to parsing and value-bearing operand resolution.
- Raw AST nodes for arithmetic entries, text operands, source-maintenance operands, list-control operands, and CTE definitions are parser internals unless a caller explicitly consumes parse JSON.
- The new semantic facts are exported through existing dossier sections rather than new schema fields. This keeps the schema stable but means consumers must read the appropriate existing sections.
- Open SQL CTE support is conservative. It covers local CTE source resolution and nested query facts, but broader SQL grammar features around CTEs should be expanded from real corpus examples.
- Statement-boundary recovery intentionally avoids broad leads such as `SET`, `LOG`, and `NEW` because those words appear in other valid contexts.
- A full workspace test run is currently blocked locally by the Windows linker PDB limit on an example target.

## Prioritized Next Steps

1. Expand Open SQL coverage from corpus examples, especially joins, unions, aggregate projections, aliases, dynamic fragments, and nested CTE combinations.
2. Add statement families with high semantic value: classic file/dataset statements, spool/list output variants, dynpro flow statements, and additional system-field-producing statements.
3. Tighten system-field modeling by documenting per-statement effects first, then adding facts only where behavior is clear and testable.
4. Add corpus-driven recovery tests for missing periods before newly supported statements and for ambiguous lead keywords that were deliberately left out.
5. Add semantic dossier consumer examples that demonstrate how to read the new facts from existing sections.
6. Investigate the Windows `LNK1318` PDB limit for workspace tests, likely by reducing debug info for example targets or excluding heavyweight examples from default workspace test builds.
