# Lexical And Program Structure

ABAP source is organized as statements. A normal statement ends at the first
top-level period, not at the end of the physical line. Line breaks and
indentation are mostly formatting, so long statements are often wrapped.

`abap-lsp` support: statement-period scanning, comments, string templates,
reports/programs, includes, event blocks, macros, test seams, enhancement
blocks, and structured recovery around common block boundaries are supported.
Unsupported valid statements may still be retained as raw statement nodes.

## Statements, Periods, And Comments

```abap
REPORT z_demo_structure.

" A double quote starts an inline comment.
" Comments do not end the statement; the period does.
DATA lv_count TYPE i VALUE 0.

" A statement can span several lines. The parser reads until the period.
WRITE: / 'Current count:',
         lv_count.

* A star in column 1 is the classic full-line comment style.
```

Semantics:

- keywords are not case-sensitive, but most ABAP code writes keywords in upper
  case and application names in lower case,
- a period commits the statement syntactically, so missing periods can make the
  next keyword look like part of the current statement,
- inline comments start with `"` and run to the end of the physical line,
- pragmas and pseudo comments such as `##NEEDED` or `"#EC ...` are comment-like
  control signals for tools and SAP checks.

## Reports, Programs, And Includes

```abap
REPORT z_demo_report MESSAGE-ID zdemo.

" INCLUDE pulls another source unit into the effective program text.
" The include normally contains declarations, forms, modules, or local classes.
INCLUDE zinc_demo_top.

" TABLES is legacy global work-area exposure. Prefer explicit DATA in new code,
" but recognize it in old reports and generated maintenance programs.
TABLES: mara, t001.
```

Semantics:

- `REPORT` and `PROGRAM` introduce executable program source,
- `MESSAGE-ID` sets a default message class for compact `MESSAGE` statements,
- includes are compile-time source composition, not runtime imports,
- include order matters because declarations and routines become part of the
  containing program.

`abap-lsp` models include edges, effective source expansion, and include member
mapping in project workspaces. Missing includes are reported conservatively.

## Event Blocks

```abap
INITIALIZATION.
  " Runs before the selection screen is shown. Use it for default values that
  " are too dynamic for the declaration VALUE clause.
  p_limit = 100.

AT SELECTION-SCREEN.
  " Runs during selection-screen processing. Validation errors are usually
  " raised here with MESSAGE ... TYPE 'E'.
  IF p_limit <= 0.
    MESSAGE 'Limit must be positive' TYPE 'E'.
  ENDIF.

START-OF-SELECTION.
  " Main report execution starts here after selection-screen processing.
  PERFORM run.

END-OF-SELECTION.
  " Often used for final list output in classic reports.
  WRITE / 'Done'.
```

Semantics:

- event block names are statements that open an implicit processing block,
- classic report flow is runtime-driven rather than explicitly called from a
  `main` function,
- event blocks share global program data, so new code should keep each block
  short and delegate to typed routines.

## Macros

```abap
DEFINE write_status.
  " Macro arguments are textual placeholders: &1, &2, and so on.
  " They are expanded before normal ABAP syntax checks.
  WRITE: / &1, &2.
END-OF-DEFINITION.

write_status 'status' lv_status.
```

Semantics:

- `DEFINE ... END-OF-DEFINITION` is textual preprocessing,
- macro calls are hard for static tools because argument meaning depends on the
  macro body and expansion context,
- prefer methods or forms for new code unless the codebase already depends on
  generated macro idioms.

`abap-lsp` recognizes macro definitions and macro call statements but does not
fully expand macro bodies for precise flow and symbol semantics.

