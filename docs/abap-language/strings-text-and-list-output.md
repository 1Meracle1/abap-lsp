# Strings, Text, And List Output

ABAP has both expression-level string templates and classic statement-level
text operations. Older reports also use list output statements for screen-like
printed lists.

`abap-lsp` support: string templates, `CONCATENATE`, `SPLIT`, `CONDENSE`,
`REPLACE`, `FIND`, `TRANSLATE`, `SHIFT`, `SEARCH`, `OVERLAY`, `PACK`, `UNPACK`,
`WRITE`, `FORMAT`, `SKIP`, `ULINE`, `NEW-LINE`, `NEW-PAGE`, `RESERVE`,
`POSITION`, `HIDE`, `READ LINE`, and `MODIFY LINE` are parsed in common forms.
Several mutating text statements emit assignment/value-flow facts.

## String Templates And Concatenation

```abap
DATA: lv_name TYPE string VALUE 'Ada',
      lv_city TYPE string VALUE 'Berlin'.

" Prefer templates when producing readable text from expressions.
DATA(lv_sentence) = |{ lv_name } lives in { lv_city }.|.

" CONCATENATE is common in older code and useful when parts are dynamic lists.
CONCATENATE lv_name lv_city
  INTO DATA(lv_key)
  SEPARATED BY ':'.
```

Semantics:

- templates are expressions and can be passed directly to methods,
- `CONCATENATE` writes to the target after `INTO`,
- `SEPARATED BY` inserts the separator between source operands.

## Splitting And Normalizing

```abap
DATA lv_path TYPE string VALUE 'A:B:C'.

" SPLIT writes each segment into a target, or into a table with INTO TABLE.
SPLIT lv_path AT ':' INTO DATA(lv_a) DATA(lv_b) DATA(lv_c).

" CONDENSE removes leading/trailing spaces and compresses inner space runs.
DATA lv_text TYPE string VALUE '  too    much   space  '.
CONDENSE lv_text.

" NO-GAPS removes all spaces.
CONDENSE lv_text NO-GAPS.
```

Semantics:

- `SPLIT ... INTO` writes positional targets,
- `SPLIT ... INTO TABLE` creates one row per segment,
- `CONDENSE` mutates the target operand.

## Finding And Replacing

```abap
DATA lv_payload TYPE string VALUE '<id>4711</id>'.

" FIND can write offset, length, submatches, or result tables.
FIND FIRST OCCURRENCE OF REGEX '<id>([^<]+)</id>'
  IN lv_payload
  SUBMATCHES DATA(lv_id).

IF sy-subrc = 0.
  WRITE / lv_id.
ENDIF.

" REPLACE mutates the target after IN.
REPLACE ALL OCCURRENCES OF '<id>' IN lv_payload WITH '<order_id>'.
REPLACE ALL OCCURRENCES OF '</id>' IN lv_payload WITH '</order_id>'.
```

Semantics:

- `FIND` reports success through `sy-subrc`,
- `MATCH OFFSET`, `MATCH LENGTH`, `SUBMATCHES`, and `RESULTS` are write
  positions,
- regex syntax is ABAP runtime regex syntax, not Rust regex syntax,
- `REPLACE` changes the target text in place.

## Classic Text Statements

```abap
DATA lv_code TYPE string VALUE ' ab-c '.

" TRANSLATE mutates character-like data.
TRANSLATE lv_code TO UPPER CASE.

" SHIFT removes or moves characters depending on additions.
SHIFT lv_code LEFT DELETING LEADING space.

" SEARCH is older than FIND and also updates sy-subrc and sy-fdpos.
SEARCH lv_code FOR '-'.
IF sy-subrc = 0.
  WRITE / sy-fdpos.
ENDIF.

" OVERLAY replaces characters in the first operand using a mask.
OVERLAY lv_code WITH '_____'.
```

Semantics:

- `TRANSLATE`, `SHIFT`, and `OVERLAY` are mutating statements,
- `SEARCH` can operate on character fields and internal tables in older code,
- `PACK` and `UNPACK` convert between display-like numeric text and packed
  representations.

## List Output

```abap
" WRITE creates classic list output. Slash starts a new output line.
WRITE: / 'Order', lv_id,
       / 'Amount', lv_amount.

" FORMAT affects following WRITE output until changed or reset.
FORMAT COLOR COL_HEADING INTENSIFIED.
WRITE / 'Important heading'.
FORMAT RESET.

" SKIP and ULINE are classic report-list layout controls.
SKIP 1.
ULINE.

" HIDE stores a value for interactive list processing at the current line.
HIDE lv_id.
```

Semantics:

- list output is stateful and tied to classic report processing,
- `FORMAT` changes output attributes rather than data values,
- `HIDE` supports interactive list events by associating values with lines,
- modern UI code normally uses ALV, dynpro, Web Dynpro, UI5, or RAP instead of
  raw list processing.
