# Internal Tables And Data Movement

Internal tables are ABAP's primary in-memory collection type. Data movement
statements copy, append, update, delete, clear, or alias data objects.

`abap-lsp` support: `APPEND`, internal-table `INSERT`, `READ TABLE`, `MODIFY`,
`DELETE`, `SORT`, `LOOP AT`, `COLLECT`, `CLEAR`, `REFRESH`, `FREE`, `UNASSIGN`,
`MOVE`, `MOVE-CORRESPONDING`, field-symbol assignment, and table-related
declarations are parsed in common forms. Static analysis includes conservative
facts for table reads, binary search, assignment sites, and field symbols.

## Building Tables

```abap
TYPES: BEGIN OF ty_order,
         order_id TYPE string,
         amount   TYPE i,
       END OF ty_order,
       ty_order_tab TYPE STANDARD TABLE OF ty_order WITH EMPTY KEY.

DATA lt_orders TYPE ty_order_tab.

" APPEND adds a row to the end of a standard table.
APPEND VALUE ty_order( order_id = 'A' amount = 10 ) TO lt_orders.

" INSERT can place a row before an index or into a sorted/hashed table by key.
INSERT VALUE ty_order( order_id = 'B' amount = 20 ) INTO TABLE lt_orders.

" COLLECT inserts or aggregates by the table key. It is compact but easy to
" misuse when the key is implicit, so prefer explicit aggregation in new code.
COLLECT VALUE ty_order( order_id = 'A' amount = 5 ) INTO lt_orders.
```

Semantics:

- standard tables preserve insertion order,
- sorted tables maintain key order,
- hashed tables are key-addressed and do not support index access,
- `APPEND` is only valid for index-like tables, while `INSERT ... INTO TABLE`
  works with key-managed tables.

## Reading Tables

```abap
" READ TABLE reports success in sy-subrc.
" sy-subrc = 0 means a row was found.
READ TABLE lt_orders INTO DATA(ls_order) WITH KEY order_id = 'A'.
IF sy-subrc = 0.
  WRITE / ls_order-amount.
ENDIF.

" ASSIGNING avoids a copy and lets the caller mutate the found row.
READ TABLE lt_orders ASSIGNING FIELD-SYMBOL(<ls_order>) WITH KEY order_id = 'B'.
IF sy-subrc = 0.
  <ls_order>-amount = <ls_order>-amount + 1.
ENDIF.

" Binary search is only correct when the table is sorted by the searched key.
SORT lt_orders BY order_id.
READ TABLE lt_orders INTO ls_order
  WITH KEY order_id = 'B'
  BINARY SEARCH.
```

Semantics:

- `READ TABLE ... INTO` copies a row into a work area,
- `READ TABLE ... ASSIGNING` binds a field symbol to the row,
- `READ TABLE ... REFERENCE INTO` returns a data reference to the row,
- `sy-tabix` is set for index tables after many successful table operations,
- `BINARY SEARCH` requires matching sort order; otherwise results are undefined.

## Updating And Deleting

```abap
" MODIFY changes rows. With TRANSPORTING, only listed components are updated.
ls_order-amount = 99.
MODIFY lt_orders FROM ls_order
  TRANSPORTING amount
  WHERE order_id = ls_order-order_id.

" DELETE removes rows by key, index, condition, or adjacent duplicate grouping.
DELETE lt_orders WHERE amount <= 0.

SORT lt_orders BY order_id.
DELETE ADJACENT DUPLICATES FROM lt_orders COMPARING order_id.
```

Semantics:

- `MODIFY ... FROM` copies values from a work area into matching rows,
- `TRANSPORTING` limits which components are changed,
- `DELETE ... WHERE` evaluates a condition for rows,
- adjacent duplicate deletion only compares neighboring rows, so sort first by
  the same components.

## Clearing, Freeing, And Aliasing

```abap
DATA lv_text TYPE string VALUE 'temporary'.

" CLEAR resets a data object to its type-specific initial value.
CLEAR lv_text.

" REFRESH clears the body of an internal table. CLEAR lt_orders also clears the
" table body in modern ABAP, but REFRESH appears in older code.
REFRESH lt_orders.

" FREE releases the internal memory held by a table or data object where the
" runtime can do so.
FREE lt_orders.

" UNASSIGN removes a field-symbol binding.
UNASSIGN <ls_order>.
```

Semantics:

- `CLEAR` changes value, not declaration,
- `FREE` is about memory release and also leaves an initial value,
- `UNASSIGN` prevents accidental later writes through an old field-symbol alias.

## Moving Corresponding Fields

```abap
TYPES: BEGIN OF ty_source,
         order_id TYPE string,
         amount   TYPE i,
         ignored  TYPE string,
       END OF ty_source,
       BEGIN OF ty_target,
         order_id TYPE string,
         amount   TYPE i,
       END OF ty_target.

DATA ls_source TYPE ty_source.
DATA ls_target TYPE ty_target.

" MOVE-CORRESPONDING copies same-named components.
" Components without a matching target are ignored.
MOVE-CORRESPONDING ls_source TO ls_target.
```

Semantics:

- matching is by component name, not by position,
- type conversion can occur per copied component,
- nested structures and tables have additional options in newer ABAP through
  `CORRESPONDING #( ... )`.

