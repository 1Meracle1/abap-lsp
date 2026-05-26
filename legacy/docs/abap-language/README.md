# ABAP Language And Supported Features

This section documents ABAP language areas in a form useful for humans and LLMs
that need to read or write ABAP source. It is also a practical map of what this
project parses and analyzes today.

The documents describe ABAP syntax and runtime semantics first, then call out
`abap-lsp` support where the implementation is intentionally partial. The
project is source-analysis first, so "supported" means the parser, symbol
collector, semantic export, or lint layer has useful handling for the construct.
It does not mean full SAP activation parity.

## Language Area Documents

| Area | Document | Typical constructs |
| --- | --- | --- |
| Source layout | [Lexical and program structure](lexical-and-program-structure.md) | statements, periods, comments, reports, includes, event blocks, macros |
| Data model | [Declarations and types](declarations-and-types.md) | `DATA`, `TYPES`, `CONSTANTS`, `FIELD-SYMBOLS`, `PARAMETERS`, structured declarations |
| Evaluation | [Expressions and assignments](expressions-and-assignments.md) | arithmetic, string templates, constructors, selectors, table expressions, `MOVE`, `ASSIGN` |
| Flow | [Control flow](control-flow.md) | `IF`, `CASE`, `LOOP`, `DO`, `WHILE`, `TRY`, exits |
| Modularization | [Procedures, events, and modularization](procedures-events-and-modularization.md) | event blocks, `FORM`, function modules, modules, `PERFORM`, `SUBMIT` |
| Object model | [Object-oriented ABAP](object-oriented-abap.md) | classes, interfaces, methods, events, aliases, inheritance, AMDP islands |
| Data containers | [Internal tables and data movement](internal-tables-and-data-movement.md) | `APPEND`, `INSERT`, `READ TABLE`, `MODIFY`, `DELETE`, `SORT`, `CLEAR` |
| Database access | [Open SQL and database access](open-sql-and-database-access.md) | `SELECT`, joins, CTEs, cursors, DB `INSERT`/`UPDATE`/`DELETE`, native SQL |
| Text processing | [Strings, text, and list output](strings-text-and-list-output.md) | `CONCATENATE`, `SPLIT`, `FIND`, `REPLACE`, `WRITE`, list statements |
| Runtime integration | [Runtime, system, and dynpro statements](runtime-system-and-dynpro.md) | `MESSAGE`, `AUTHORITY-CHECK`, datasets, parameters, transactions, dynpro |

## Support Model

`abap-lsp` currently supports common custom ABAP source best when code is
statement-oriented and dependencies are available locally or through ADT fetches.
The parser handles many statements structurally and keeps unsupported valid
statements as token-level nodes where possible.

Strongest areas:

- local parsing of reports, includes, forms, function modules, classes,
  interfaces, methods, event blocks, Open SQL, internal-table operations, and
  many runtime statements,
- symbol collection for declarations, parameters, methods, forms, function
  modules, includes, class/interface facts, references, and assignment sites,
- conservative facts for Open SQL sources, host expressions, value flow, calls,
  system fields, and routine analysis.

Known broad limits:

- CDS, RAP behavior definitions, DDIC object editors, dynpro flow logic, and
  generated object metadata are not full language surfaces in this project,
- dynamic dispatch, dynamic SQL fragments, generated code, and macro-heavy code
  are represented conservatively,
- AMDP SQLScript and `EXEC SQL` bodies are preserved as opaque islands rather
  than parsed as ABAP.

## ABAP Authoring Guidance For LLMs

Prefer explicit, boring ABAP unless a codebase already uses a different style:

- end every statement with a period,
- declare data close to use with `DATA(...)` only when the inferred type is
  obvious from the right-hand side,
- use `@` host variables in modern Open SQL,
- check `sy-subrc` after operations that signal success through it,
- keep dynamic names and dynamic SQL isolated behind small routines,
- favor class methods and typed parameters over global state in new code,
- keep examples activation-friendly by declaring every variable shown in a
  snippet unless the snippet explicitly focuses on a fragment.

