# Control Flow

Control flow determines which statements execute and how loops, exceptions, and
early exits behave.

`abap-lsp` support: `IF`/`ELSEIF`/`ELSE`, `CASE`/`WHEN`, `WHILE`, `DO`, `LOOP`,
`AT ... ENDAT` inside loops, `TRY`/`CATCH`/`CLEANUP`, `CATCH
SYSTEM-EXCEPTIONS`, `CHECK`, `ASSERT`, `CONTINUE`, `EXIT`, `RETURN`, `STOP`,
`RESUME`, and `RETRY` are parsed and used by routine analysis where supported.

## Conditions

```abap
IF lv_status = 'O' AND lv_amount > 0.
  " This branch handles open rows with a positive amount.
  PERFORM process_open_order.
ELSEIF lv_status = 'C'.
  " ELSEIF is tested only when previous branches were false.
  PERFORM process_closed_order.
ELSE.
  " ELSE is the fallback branch.
  PERFORM process_unknown_order.
ENDIF.

CASE lv_status.
  WHEN 'O'.
    WRITE / 'Open'.
  WHEN 'C' OR 'X'.
    WRITE / 'Closed or cancelled'.
  WHEN OTHERS.
    WRITE / 'Other'.
ENDCASE.
```

Semantics:

- `IF` conditions are boolean expressions using relational and logical
  operators,
- `IS INITIAL`, `IS BOUND`, `IS ASSIGNED`, `IS SUPPLIED`, and `IS INSTANCE OF`
  are predicate forms with ABAP-specific meaning,
- `CASE` compares one subject expression against `WHEN` alternatives,
- `WHEN OTHERS` is the default branch and should normally be last.

## CASE Forms

Normal `CASE` uses a subject expression followed by one or more alternatives.
Each `WHEN` alternative is an operand value, not a boolean condition.

```abap
DATA lv_status TYPE c LENGTH 1 VALUE 'O'.
DATA lv_count TYPE i VALUE -2.
DATA lv_text TYPE string VALUE `ABAP`.
DATA lt_numbers TYPE STANDARD TABLE OF i WITH EMPTY KEY.

lt_numbers = VALUE #( ( 10 ) ( 20 ) ).

CASE lv_status.
  WHEN 'O'.
    WRITE / 'Open'.
  WHEN 'C' OR 'X'.
    WRITE / 'Closed or cancelled'.
  WHEN OTHERS.
    WRITE / 'Other'.
ENDCASE.

CASE strlen( lv_text ).
  WHEN abs( lv_count ).
    WRITE / 'Length matches absolute count'.
  WHEN lines( lt_numbers ).
    WRITE / 'Length matches row count'.
  WHEN CONV i( 4 ).
    WRITE / 'Length is four'.
ENDCASE.
```

Valid normal `WHEN` operands include data objects, literals, selected built-in
function calls, constructor expressions, and functional method calls. Multiple
alternatives use `OR`; `AND`, relational comparisons, and range syntax are not
normal `WHEN` syntax.

```abap
DATA lv_num TYPE i VALUE 3.
DATA lt_int TYPE STANDARD TABLE OF i WITH EMPTY KEY.

lt_int = VALUE #( ( 3 ) ).

" Invalid CASE syntax. Use IF for conditions like these.
CASE lv_num.
  WHEN lv_num = 3.
    WRITE / 'Not valid'.
  WHEN lv_num > 1.
    WRITE / 'Not valid'.
  WHEN 1 TO 5.
    WRITE / 'Not valid'.
  WHEN lt_int[ 1 ].
    WRITE / 'Not valid'.
ENDCASE.
```

`CASE TYPE OF` is a separate form for object reference type checks.

```abap
CLASS lcl_base DEFINITION.
ENDCLASS.

CLASS lcl_child DEFINITION INHERITING FROM lcl_base.
ENDCLASS.

DATA lo_ref TYPE REF TO lcl_base.

lo_ref = NEW lcl_child( ).

CASE TYPE OF lo_ref.
  WHEN TYPE lcl_child INTO DATA(lo_child).
    WRITE / 'Child reference'.
  WHEN TYPE lcl_base.
    WRITE / 'Base reference'.
  WHEN OTHERS.
    WRITE / 'Other reference'.
ENDCASE.
```

`abap-lsp` support notes:

- normal `CASE` supports `WHEN operand [OR operand ...]` and `WHEN OTHERS`,
- `CASE TYPE OF` supports `WHEN TYPE class_or_interface [INTO target]`,
  including inline `DATA(...)` targets,
- direct table expressions after normal `WHEN` are rejected,
- malformed `WHEN` headers are represented as invalid statements without
  cascading errors onto later `WHEN` branches or `ENDCASE`.

## Loops

```abap
" DO repeats a fixed number of times. sy-index contains the current iteration.
DO 3 TIMES.
  WRITE / sy-index.
ENDDO.

" WHILE repeats while the condition is true.
WHILE lv_count > 0.
  lv_count = lv_count - 1.
ENDWHILE.

" LOOP AT iterates an internal table. INTO copies each row into ls_order.
LOOP AT lt_orders INTO DATA(ls_order) WHERE active = abap_true.
  WRITE / ls_order-order_id.
ENDLOOP.

" ASSIGNING aliases the table row, so assignments update the row in place.
LOOP AT lt_orders ASSIGNING FIELD-SYMBOL(<ls_order>).
  <ls_order>-active = abap_false.
ENDLOOP.
```

Semantics:

- `sy-index` is maintained for `DO` and `WHILE`,
- `sy-tabix` is maintained by many internal-table operations on index tables,
- `LOOP ... INTO` copies data; `LOOP ... ASSIGNING` aliases the row,
- `WHERE`, `FROM`, `TO`, `STEP`, `GROUP BY`, and `LOOP AT GROUP` refine loop
  iteration.

## Loop Control And Routine Exits

```abap
LOOP AT lt_orders INTO DATA(ls_order).
  IF ls_order-active = abap_false.
    " CONTINUE skips to the next loop iteration.
    CONTINUE.
  ENDIF.

  IF ls_order-amount < 0.
    " EXIT leaves the innermost loop. In a FORM outside a loop, EXIT leaves the
    " routine, so prefer RETURN for routine exits in new code.
    EXIT.
  ENDIF.

  PERFORM process_order USING ls_order.
ENDLOOP.

FORM validate USING iv_amount TYPE i.
  IF iv_amount <= 0.
    " CHECK exits the current processing block when the condition is false.
    CHECK iv_amount > 0.
  ENDIF.

  IF iv_amount > 100000.
    " RETURN leaves the current method, function module, or form.
    RETURN.
  ENDIF.
ENDFORM.
```

Semantics:

- `CHECK` inside a loop skips the current iteration when false,
- `CHECK` outside a loop leaves the current processing block,
- `RETURN` leaves the current routine,
- `STOP` ends report processing after the current event context and is mostly
  a classic report construct.

## Exceptions

```abap
TRY.
    " Code in TRY can raise class-based exceptions.
    zcl_order_service=>save( ls_order ).

  CATCH zcx_validation INTO DATA(lx_validation).
    " Specific exceptions should be caught before broad base classes.
    MESSAGE lx_validation->get_text( ) TYPE 'E'.

  CATCH cx_root INTO DATA(lx_root).
    " cx_root is broad. Use it only at a boundary where a generic fallback is
    " really intended.
    MESSAGE lx_root->get_text( ) TYPE 'E'.

  CLEANUP.
    " CLEANUP runs during stack unwinding before the exception leaves the TRY.
    " Do not hide the original exception here.
    PERFORM release_temp_state.
ENDTRY.
```

Semantics:

- class-based exceptions propagate until caught or declared by signatures where
  required by the ABAP release,
- `CLEANUP` is not a normal finally block; it runs when the `TRY` is left by an
  exception,
- resumable exceptions can use `RESUME`; retryable handling can use `RETRY` in
  supported contexts,
- `CATCH SYSTEM-EXCEPTIONS` is classic, non-class-based exception handling.
