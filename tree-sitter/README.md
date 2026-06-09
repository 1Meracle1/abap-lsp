# ABAP tree-sitter grammar

This directory contains the tree-sitter grammar used for ABAP syntax support in
Zed and for local grammar experiments. It is separate from the root Odin parser
and semantic analyzer. The goal here is editor-friendly parsing and highlighting,
not full ABAP validation.

## Directory layout

- `grammar.js`: source grammar. Edit this first when changing parse behavior.
- `src/grammar.json`, `src/node-types.json`, `src/parser.c`: generated
  tree-sitter output. Regenerate these after changing `grammar.js`.
- `src/scanner.c`: external scanner for ABAP comments, pragmas, and string
  templates.
- `queries/highlights.scm`: generic tree-sitter highlight query for this grammar.
- `test/corpus/*.txt`: tree-sitter corpus tests covering supported syntax and
  regressions.
- `tree-sitter.json`: grammar metadata for the tree-sitter CLI and consumers.
- `package.json`, `package-lock.json`: local tree-sitter CLI dependency and npm
  scripts.

The Zed extension lives outside this directory under `../editors/zed`. Zed has
its own language queries, including `../editors/zed/languages/abap/highlights.scm`.
When grammar node shapes change, keep both highlight query locations compatible
with the generated parser.

## Supported syntax

The grammar currently supports enough ABAP structure for syntax highlighting and
basic editor features:

- ABAP `.abap` files with grammar name `abap` and scope `source.abap`.
- Case-insensitive ABAP keywords.
- Line comments (`" ...`), full-line star comments, and pragmas (`##...`).
- Character literals, raw string literals, string templates, and template
  interpolation.
- Identifiers, escaped identifiers, `/namespace/name` identifiers, CTE-style
  `+name` identifiers, field symbols, component paths, object selectors, static
  selectors, interface selectors, and wildcard selectors.
- Basic expressions, including unary and binary operators, parenthesized
  expressions, host expressions (`@...`), method/function-style calls, table
  expressions, substring expressions, and selected constructor expressions.
- Common declarations: `DATA`, `TYPES`, `CONSTANTS`, `FIELD-SYMBOLS`,
  `STATICS`, `TABLES`, `RANGES`, `PARAMETERS`, `SELECT-OPTIONS`, `CONTROLS`,
  `CLASS-DATA`, `TYPE-POOLS`, `FUNCTION-POOL`, and `INCLUDE TYPE/STRUCTURE`.
- Program structure: `REPORT`, `PROGRAM`, `INCLUDE`, events, forms, function
  modules, dynpro modules, macros, classes, interfaces, methods, visibility
  sections, and deferred class/interface declarations.
- Control flow: `IF`, `ELSEIF`, `ELSE`, `CASE`, `WHEN`, `WHILE`, `DO`, `LOOP`,
  `AT FIRST/LAST/NEW/END OF`, and `TRY/CATCH/CLEANUP`.
- Broad statement parsing for many common ABAP commands such as `PERFORM`,
  `CALL FUNCTION`, `CALL METHOD`, `CALL TRANSFORMATION`, `SUBMIT`, `READ TABLE`,
  `DELETE`, `UPDATE`, `SORT`, `CREATE`, `ADD`, `UNPACK`, `APPEND`, `MODIFY`,
  `WRITE`, `MESSAGE`, and related additions.
- Open SQL-shaped statements beginning with `SELECT` or `WITH`, plus selected
  DML forms.
- Recovery for unknown statements by consuming tokens up to the next period.

The corpus tests show the supported surface most precisely. Start with
`test/corpus/lexical.txt`, `declarations.txt`, `control_and_oop.txt`,
`modularization_and_sql.txt`, `recovery.txt`, and `regressions.txt`.

## Known limits

This grammar is intentionally permissive and incomplete:

- It is not the canonical ABAP parser for the project. The Odin parser under
  `../src/parser` owns frontend correctness.
- It does not perform semantic analysis, name resolution, type checking, DDIC
  lookup, include resolution, or Open SQL validation.
- Many statement additions are parsed as generic tail tokens or `tail_fragment`
  nodes instead of detailed ABAP-specific AST nodes.
- Open SQL support is structural and highlighting-oriented, not a full SQL
  grammar.
- Constructor expressions are only modeled for the forms currently needed by
  highlighting and corpus coverage.
- Unknown or unsupported statements may still parse successfully through
  recovery. A successful parse does not mean the ABAP source is valid.
- Highlight captures are intentionally simple: comments, pragmas, keywords,
  literals, builtin types in common positions, variables, operators, and
  punctuation.

## Validation commands

Run these commands from this directory on Windows PowerShell:

```powershell
npm install
npm test
```

Use these when changing `grammar.js`:

```powershell
npm run generate
npm test
git diff -- src/grammar.json src/node-types.json src/parser.c
```

Parse the sample ABAP file:

```powershell
npm run parse:sample
```

Check parser performance and success on a larger local ABAP file:

```powershell
npm exec tree-sitter -- parse -q --stat D:\dev\abap\zabapgit\src\zabapgit_standalone.abap
```

Validate the generic highlight query:

```powershell
npm exec tree-sitter -- highlight ..\examples\ZPERF_PARSER_MIXED.abap
```

Validate the Zed highlight query copy against the same grammar:

```powershell
npm exec tree-sitter -- query -q ..\editors\zed\languages\abap\highlights.scm ..\examples\ZPERF_PARSER_MIXED.abap
```

The tree-sitter CLI may warn that parser directories are not configured. That
warning is harmless for these in-directory commands as long as the command exits
successfully.
