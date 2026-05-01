# Expressions And Assignments

Expressions compute values. Assignments write values into variables, fields,
field symbols, references, and constructor targets.

`abap-lsp` support: arithmetic/logical expressions, selectors, table
expressions, substring access, constructor-style calls, `LET`/`FOR`/`COND`/
`SWITCH`/`REDUCE`-style constructor clauses, string templates, assignment
statements, `MOVE`, `MOVE-CORRESPONDING`, `COMPUTE`, classic arithmetic,
`ASSIGN`, and `GET REFERENCE` are supported in common forms.

## Assignment And Arithmetic

```abap
DATA: lv_net    TYPE p LENGTH 12 DECIMALS 2 VALUE '100.00',
      lv_tax    TYPE p LENGTH 12 DECIMALS 2 VALUE '19.00',
      lv_factor TYPE i VALUE 2,
      lv_total  TYPE p LENGTH 12 DECIMALS 2.

" The right side is evaluated first, then assigned to the left side.
lv_total = ( lv_net + lv_tax ) * lv_factor.

" COMPUTE is the classic spelling. It is semantically an assignment.
COMPUTE lv_total = lv_total + 10.

" Classic arithmetic mutates the target when GIVING is absent.
ADD 1 TO lv_factor.

" With GIVING, the source operands are read and the GIVING target is written.
DIVIDE lv_total BY 2 GIVING DATA(lv_half).
```

Semantics:

- `=` assigns compatible values and may perform ABAP conversion,
- `?=` is down-cast assignment and can fail at runtime when the dynamic type is
  incompatible,
- arithmetic follows ABAP numeric conversion rules, not JavaScript or Rust
  rules,
- classic arithmetic without `GIVING` reads and writes the changed operand.

## Selectors, Table Expressions, And Substrings

```abap
DATA lv_city TYPE string.

" '-' selects a component of a structure.
lv_city = ls_address-city.

" '->' selects an instance attribute or method through an object reference.
lv_city = lo_customer->get_city( ).

" '=>' selects a static class member.
lv_city = zcl_defaults=>get_city( ).

" Table expressions read one row and raise an exception if no row exists,
" unless OPTIONAL or DEFAULT is used in a surrounding constructor expression.
DATA(ls_order) = lt_orders[ order_id = '4711' ].

" Offset/length works on character-like or byte-like data.
" Here the first eight characters are copied.
DATA(lv_prefix) = lv_city(8).
```

Guidance:

- prefer `READ TABLE ...` when older code expects `sy-subrc` instead of
  exceptions,
- use table expressions when absence is exceptional or when wrapped with
  `VALUE #( ... OPTIONAL )`,
- avoid chaining many selectors in one line when intermediate references can be
  initial.

## Constructor Expressions

```abap
TYPES: BEGIN OF ty_line,
         id     TYPE string,
         amount TYPE i,
       END OF ty_line,
       ty_line_tab TYPE STANDARD TABLE OF ty_line WITH EMPTY KEY.

" VALUE constructs a structure. Field names on the left are target components.
DATA(ls_line) = VALUE ty_line(
  id     = 'A'
  amount = 10
).

" VALUE also constructs internal tables. Each parenthesized group is one row.
DATA(lt_lines) = VALUE ty_line_tab(
  ( id = 'A' amount = 10 )
  ( id = 'B' amount = 20 )
).

" COND chooses one expression branch. ELSE should be present unless every
" possible value is covered and the type has a useful initial value.
DATA(lv_bucket) = COND string(
  WHEN ls_line-amount > 100 THEN 'large'
  WHEN ls_line-amount > 0   THEN 'normal'
  ELSE 'empty'
).

" LET names intermediate values inside a constructor expression.
DATA(lv_label) = COND string(
  LET lv_amount = ls_line-amount IN
  WHEN lv_amount > 0 THEN |Amount { lv_amount }|
  ELSE 'No amount'
).
```

Semantics:

- constructor expressions are expressions, so they can appear on the right side
  of assignments or as actual parameters,
- `#` asks the compiler to infer the result type from the target position,
- `BASE` in a `VALUE` expression copies an existing value before applying named
  component changes,
- `CORRESPONDING` maps same-named fields and can use `MAPPING` and `EXCEPT`.

## String Templates

```abap
DATA: lv_matnr TYPE matnr VALUE '123',
      lv_qty   TYPE p LENGTH 8 DECIMALS 2 VALUE '7.5'.

" Text outside braces is literal text.
" Expressions inside braces are evaluated and formatted into the string.
DATA(lv_text) =
  |Material { lv_matnr ALPHA = IN } has quantity { lv_qty DECIMALS = 2 }|.
```

Semantics:

- `|...|` creates a character string template,
- `{ ... }` evaluates an ABAP expression inside the template,
- formatting options such as `WIDTH`, `ALIGN`, `DECIMALS`, `ALPHA`, `DATE`,
  and `TIME` affect only text rendering,
- templates are often clearer than `CONCATENATE` for user-facing messages.

## Dynamic Assignment

```abap
FIELD-SYMBOLS <lv_value> TYPE any.

" ASSIGN binds a field symbol to a data object, component, or dynamic target.
" Always check sy-subrc before reading a dynamically assigned field symbol.
ASSIGN COMPONENT 'AMOUNT' OF STRUCTURE ls_line TO <lv_value>.
IF sy-subrc = 0.
  <lv_value> = 42.
ENDIF.

" A data reference points at a data object. The object must outlive the useful
" lifetime of the reference.
GET REFERENCE OF ls_line INTO DATA(lr_line).
```

Semantics:

- `ASSIGN` changes alias binding, not the underlying value by itself,
- assigning through a bound field symbol writes the underlying data object,
- dynamic component names are runtime strings and cannot be fully resolved
  statically unless the value is obvious.

