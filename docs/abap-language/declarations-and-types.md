# Declarations And Types

Declarations introduce data objects, constants, field symbols, selection-screen
parameters, and type aliases. ABAP supports global declarations, class
attributes, routine-local declarations, inline declarations, and structured
types.

`abap-lsp` support: `DATA`, `CLASS-DATA`, `STATICS`, `TYPES`, `CONSTANTS`,
`FIELD-SYMBOLS`, `TABLES`, `RANGES`, `CONTROLS`, `PARAMETERS`,
`SELECT-OPTIONS`, `TYPE-POOLS`, structured declarations, `INCLUDE TYPE`,
`INCLUDE STRUCTURE`, `VALUE`, `LENGTH`, `DECIMALS`, `TYPE`, `LIKE`, `REF TO`,
table types, and inline `DATA(...)` / `FIELD-SYMBOL(...)` are supported in
common forms.

## Scalar And Structured Data

```abap
TYPES: BEGIN OF ty_order,
         " A component gets a name and a type. Built-in types such as i, c,
         " string, xstring, d, t, and p are common in application code.
         order_id TYPE string,

         " Packed numbers usually need DECIMALS for business amounts.
         amount   TYPE p LENGTH 12 DECIMALS 2,

         " ABAP booleans are normally c(1) values using abap_true/abap_false.
         active   TYPE abap_bool,
       END OF ty_order.

DATA ls_order TYPE ty_order.

" VALUE assigns an initial value when the data object is created.
DATA lv_retries TYPE i VALUE 3.

" Multiple declarations after DATA: share the same statement but each name is a
" separate object. Keep them short so tools and reviewers can scan them.
DATA: lv_status TYPE c LENGTH 1 VALUE 'N',
      lv_text   TYPE string.
```

Semantics:

- `TYPE` references a type definition, DDIC type, class type, or built-in type,
- `LIKE` copies the type shape of an existing data object or DDIC field,
- `VALUE` initializes the object once at declaration time,
- local declarations are visible in their routine or block scope; global report
  declarations are visible across includes in effective source order.

## Tables And Ranges

```abap
TYPES ty_order_tab TYPE STANDARD TABLE OF ty_order WITH EMPTY KEY.

DATA lt_orders TYPE ty_order_tab.

" RANGES creates a classic selection table with SIGN, OPTION, LOW, HIGH.
" It is common in older code and selection-screen handling.
RANGES r_order_id FOR ls_order-order_id.

" SELECT-OPTIONS creates a selection-screen range and a backing range table.
SELECT-OPTIONS s_order FOR ls_order-order_id.
```

Semantics:

- `STANDARD TABLE` is insertion-ordered and usually searched linearly unless a
  sorted/hashed key is declared,
- `WITH EMPTY KEY` avoids implicit default key surprises for standard tables,
- range tables encode inclusive/exclusive predicates through `SIGN` and
  `OPTION`,
- `SELECT-OPTIONS` has UI behavior; `RANGES` is only a data declaration.

## Constants, Statics, And Field Symbols

```abap
CONSTANTS gc_status_open TYPE c VALUE 'O'.

FORM next_id CHANGING cv_id TYPE i.
  " STATICS persists between calls to this FORM, unlike local DATA.
  STATICS sv_last_id TYPE i.

  sv_last_id = sv_last_id + 1.
  cv_id = sv_last_id.
ENDFORM.

FIELD-SYMBOLS <ls_order> LIKE LINE OF lt_orders.

LOOP AT lt_orders ASSIGNING <ls_order>.
  " A field symbol aliases the current row. Changes through <ls_order> update
  " the table row directly when the loop uses ASSIGNING.
  <ls_order>-active = abap_true.
ENDLOOP.
```

Semantics:

- `CONSTANTS` values cannot be assigned after declaration,
- `STATICS` are routine-local but lifetime-persistent,
- field symbols are typed aliases and must be assigned before dereference,
- unassigned field symbols raise runtime errors if used as values.

## Inline Declarations

```abap
" DATA(...) declares a local variable at the write position.
" The static type is inferred from the expression or target position.
DATA(lv_count) = lines( lt_orders ).

" Inline declarations in Open SQL are common and keep result scope local.
SELECT *
  FROM zorders
  INTO TABLE @DATA(lt_db_orders)
  WHERE status = @gc_status_open.

" FIELD-SYMBOL(...) declares the alias at an assigning position.
READ TABLE lt_db_orders ASSIGNING FIELD-SYMBOL(<ls_db_order>) INDEX 1.
IF sy-subrc = 0.
  <ls_db_order>-active = abap_true.
ENDIF.
```

Guidance:

- use inline declarations when the inferred type is clear at the line of use,
- use explicit declarations for public signatures, global data, DDIC-facing
  structures, and values whose type matters to the reader,
- do not hide important business types behind long constructor expressions.

