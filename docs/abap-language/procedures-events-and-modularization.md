# Procedures, Events, And Modularization

ABAP code is organized through report event blocks, subroutines, function
modules, dynpro modules, and class methods. New application code should prefer
typed methods, but older ABAP code often uses all of these forms in one program.

`abap-lsp` support: event blocks, `FORM`/`ENDFORM`, `PERFORM`, function module
source, function parameter sections, `MODULE`/`ENDMODULE`, `SUBMIT`, test seams,
and enhancement sections are parsed. The semantic model collects routine facts,
call sites, parameter reads/writes, and include-aware references where possible.

## Report Event Blocks

```abap
REPORT z_demo_modularization.

PARAMETERS p_id TYPE string.

START-OF-SELECTION.
  " Keep event blocks small. Delegate work to a routine with explicit inputs.
  PERFORM run USING p_id.

FORM run USING iv_id TYPE string.
  WRITE / iv_id.
ENDFORM.
```

Semantics:

- report events are called by the ABAP runtime,
- global report data is visible to forms and event blocks,
- explicit `USING` and `CHANGING` parameters make dependencies clearer than
  reading globals in every routine.

## Forms And PERFORM

```abap
FORM normalize_text
  USING    iv_raw    TYPE string
  CHANGING cv_result TYPE string.

  " USING parameters are inputs by convention.
  " CHANGING parameters are read/write outputs.
  cv_result = iv_raw.
  CONDENSE cv_result.
  TRANSLATE cv_result TO UPPER CASE.
ENDFORM.

DATA lv_name TYPE string VALUE '  example  '.

" PERFORM calls the subroutine by name. The actual parameters are positional.
PERFORM normalize_text USING lv_name CHANGING lv_name.
```

Semantics:

- form parameters are positional, so order matters,
- `USING` should be input-only and `CHANGING` should be changed by the form,
  although old code does not always obey that convention,
- dynamic `PERFORM (lv_form)` is possible but hard for static tools to resolve,
- `TABLES` form parameters are legacy table parameters; prefer typed table
  parameters on methods.

## Function Modules

```abap
FUNCTION z_demo_get_status.
*"----------------------------------------------------------------------
*"  IMPORTING
*"     VALUE(iv_id) TYPE string
*"  EXPORTING
*"     VALUE(ev_status) TYPE c
*"  EXCEPTIONS
*"      not_found
*"----------------------------------------------------------------------

  " Function modules are globally callable repository objects.
  " The interface is defined by IMPORTING, EXPORTING, CHANGING, TABLES, and
  " EXCEPTIONS metadata. In source exports, that interface may appear in the
  " FUNCTION header block.
  SELECT SINGLE status
    FROM zorders
    INTO @ev_status
    WHERE order_id = @iv_id.

  IF sy-subrc <> 0.
    RAISE not_found.
  ENDIF.
ENDFUNCTION.
```

Semantics:

- function modules live in function groups and have global names,
- classic exceptions are named exception codes, not class-based exception
  objects,
- RFC-enabled and update-task function modules have runtime behavior outside
  plain source syntax,
- use class-based services for new code unless integration requires a function
  module.

## Dynpro Modules

```abap
MODULE status_0100 OUTPUT.
  " OUTPUT modules run during PBO processing for a screen.
  SET PF-STATUS 'MAIN'.
  SET TITLEBAR 'TITLE'.
ENDMODULE.

MODULE user_command_0100 INPUT.
  " INPUT modules run during PAI processing after user interaction.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
```

Semantics:

- dynpro modules are called from screen flow logic, not directly by ABAP source
  statements in the same way as methods,
- `OUTPUT` modules prepare screen state before display,
- `INPUT` modules react to user actions and validate entered values.

## SUBMIT And Cross-Program Calls

```abap
" SUBMIT starts another executable report.
" AND RETURN asks the runtime to return to the caller after the report finishes.
SUBMIT z_report_detail
  WITH p_id = lv_order_id
  VIA SELECTION-SCREEN
  AND RETURN.
```

Semantics:

- `SUBMIT` executes a report in a new report context,
- `WITH` clauses set selection-screen parameters and select-options,
- `VIA JOB ... NUMBER ...` schedules background execution,
- `AND RETURN` controls whether control returns to the caller.

