REPORT zperf_parser_mixed.

TYPES: BEGIN OF ty_source,
         matnr TYPE string,
         werks TYPE string,
         menge TYPE i,
         status TYPE string,
       END OF ty_source.
TYPES ty_source_tab TYPE STANDARD TABLE OF ty_source WITH EMPTY KEY.

TYPES: BEGIN OF ty_target,
         material TYPE string,
         plant TYPE string,
         quantity TYPE i,
         message TYPE string,
       END OF ty_target.
TYPES ty_target_tab TYPE STANDARD TABLE OF ty_target WITH EMPTY KEY.

DATA gt_source TYPE ty_source_tab.
DATA gt_target TYPE ty_target_tab.
DATA gs_target TYPE ty_target.
DATA gv_total TYPE i.
DATA gv_message TYPE string.
DATA gv_runtime_message TYPE string.

PARAMETERS p_plant TYPE string.
SELECT-OPTIONS s_matnr FOR gs_target-material.

CLASS lcl_accumulator DEFINITION.
  PUBLIC SECTION.
    METHODS add
      IMPORTING
        is_source TYPE ty_source
      CHANGING
        cs_target TYPE ty_target.
    METHODS total
      RETURNING VALUE(rv_total) TYPE i.
  PRIVATE SECTION.
    DATA mv_total TYPE i.
ENDCLASS.

CLASS lcl_accumulator IMPLEMENTATION.
  METHOD add.
    cs_target-material = is_source-matnr.
    cs_target-plant = is_source-werks.
    cs_target-quantity = is_source-menge.
    cs_target-message = |{ is_source-matnr }/{ is_source-werks }|.
    mv_total = mv_total + is_source-menge.
  ENDMETHOD.

  METHOD total.
    rv_total = mv_total.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_parent_class DEFINITION.
  PUBLIC SECTION.
    METHODS mul
      IMPORTING
        iv_val1 TYPE numeric
        iv_val2 TYPE numeric
      RETURNING
        VALUE(rv_res) TYPE numeric.
ENDCLASS.

CLASS lcl_parent_class IMPLEMENTATION.
  METHOD mul.
    rv_res = iv_val1 * iv_val2.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_class DEFINITION INHERITING FROM lcl_parent_class.
  PUBLIC SECTION.
    CLASS-METHODS add
      IMPORTING
        iv_val1 TYPE numeric
        iv_val2 TYPE numeric
      RETURNING
        VALUE(rv_res) TYPE numeric.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD add.
    rv_res = iv_val1 + iv_val2.
  ENDMETHOD.
ENDCLASS.

FORM seed_source CHANGING ct_source TYPE ty_source_tab.
  DATA ls_source TYPE ty_source.

  ls_source-matnr = 'MAT-001'.
  ls_source-werks = p_plant.
  ls_source-menge = 10.
  ls_source-status = 'A'.
  APPEND ls_source TO ct_source.

  ls_source-matnr = 'MAT-002'.
  ls_source-werks = p_plant.
  ls_source-menge = 20.
  ls_source-status = 'B'.
  APPEND ls_source TO ct_source.

  ls_source-matnr = 'MAT-003'.
  ls_source-werks = p_plant.
  ls_source-menge = 30.
  ls_source-status = 'C'.
  APPEND ls_source TO ct_source.
ENDFORM.

FORM transform_source USING it_source TYPE ty_source_tab
                      CHANGING ct_target TYPE ty_target_tab
                               cv_total TYPE i.
  DATA lo_acc TYPE REF TO lcl_accumulator.
  DATA ls_target TYPE ty_target.
  FIELD-SYMBOLS <source> TYPE ty_source.

  CREATE OBJECT lo_acc.
  LOOP AT it_source ASSIGNING <source>.
    CLEAR ls_target.
    lo_acc->add(
      EXPORTING
        is_source = <source>
      CHANGING
        cs_target = ls_target ).
    IF <source>-status = 'A'.
      ls_target-message = |active: { ls_target-message }|.
    ELSEIF <source>-status = 'B'.
      ls_target-message = |blocked: { ls_target-message }|.
    ELSE.
      ls_target-message = |other: { ls_target-message }|.
    ENDIF.
    APPEND ls_target TO ct_target.
  ENDLOOP.
  cv_total = lo_acc->total( ).
ENDFORM.

FORM summarize USING it_target TYPE ty_target_tab
               CHANGING cv_message TYPE string.
  DATA lv_index TYPE i.
  DATA ls_target TYPE ty_target.

  LOOP AT it_target INTO ls_target.
    lv_index = lv_index + 1.
    CASE ls_target-plant.
      WHEN p_plant.
        cv_message = |{ cv_message }#{ lv_index }:{ ls_target-material }|.
      WHEN OTHERS.
        cv_message = |{ cv_message }#other|.
    ENDCASE.
  ENDLOOP.

  READ TABLE it_target INTO ls_target INDEX 1.
  IF sy-subrc = 0.
    cv_message = |{ cv_message } first={ ls_target-material }|.
  ENDIF.
ENDFORM.

FORM guarded_check USING iv_flag TYPE i
                   CHANGING cv_message TYPE string.
  CHECK iv_flag = 1.
  CONCATENATE cv_message 'check' INTO cv_message SEPARATED BY '|'.
ENDFORM.

FORM exercise_runtime_coverage CHANGING cv_message TYPE string.
  DATA lv_calc TYPE i VALUE 2.
  DATA lv_text TYPE string.
  DATA lv_first TYPE string.
  DATA lv_second TYPE string.
  DATA lv_rest TYPE string.
  DATA lv_offset TYPE i.
  DATA lv_length TYPE i.
  DATA lv_count TYPE i.
  DATA lv_loop TYPE i.
  DATA lv_written TYPE string.
  DATA lv_clear TYPE string VALUE 'clear-me'.
  DATA lr_calc TYPE REF TO i.
  DATA lr_line TYPE REF TO i.
  DATA lt_nums TYPE STANDARD TABLE OF i.
  DATA lt_scratch TYPE STANDARD TABLE OF i.
  DATA lv_num TYPE i.
  FIELD-SYMBOLS <num> TYPE i.

  MOVE 2 TO lv_calc.
  COMPUTE lv_calc = lv_calc + 3.
  ADD 5 TO lv_calc.
  SUBTRACT 4 FROM lv_calc.
  MULTIPLY lv_calc BY 2.
  DIVIDE lv_calc BY 4.
  WRITE lv_calc TO lv_written.

  CONCATENATE 'AA' 'BB' 'CC' INTO lv_text SEPARATED BY '-'.
  SPLIT lv_text AT '-' INTO lv_first lv_second lv_rest.
  REPLACE ALL OCCURRENCES OF 'C' IN lv_rest WITH 'x'.
  SHIFT lv_rest RIGHT BY 1 PLACES.
  CONDENSE lv_rest NO-GAPS.
  TRANSLATE lv_rest TO UPPER CASE.
  FIND ALL OCCURRENCES OF 'A' IN lv_text MATCH OFFSET lv_offset MATCH LENGTH lv_length MATCH COUNT lv_count.
  SEARCH lv_text FOR 'bb'.

  ASSIGN lv_calc TO <num>.
  <num> = 7.
  lr_calc = REF #( lv_calc ).
  lr_calc->* = lr_calc->* + 1.
  UNASSIGN <num>.

  CREATE DATA lr_line.
  lr_line->* = 11.
  APPEND 5 TO lt_nums ASSIGNING <num>.
  <num> = 6.
  INSERT 9 INTO TABLE lt_nums INDEX 1 REFERENCE INTO lr_line.
  lv_num = 8.
  MODIFY lt_nums FROM lv_num INDEX 2.
  READ TABLE lt_nums INTO lv_num INDEX 2.
  DELETE lt_nums WHERE table_line = 9.
  SORT lt_nums DESCENDING.

  WHILE lv_loop < 3.
    lv_loop = lv_loop + 1.
    IF lv_loop = 2.
      CONTINUE.
    ENDIF.
  ENDWHILE.

  DO.
    lv_loop = lv_loop + 1.
    IF lv_loop = 4.
      EXIT.
    ENDIF.
  ENDDO.

  PERFORM guarded_check USING 0 CHANGING cv_message.
  PERFORM guarded_check USING 1 CHANGING cv_message.

  TRY.
      RAISE EXCEPTION TYPE cx_root.
      CONCATENATE cv_message 'miss' INTO cv_message SEPARATED BY '|'.
    CATCH cx_root INTO DATA(lx_error).
      CONCATENATE cv_message 'caught' INTO cv_message SEPARATED BY '|'.
  ENDTRY.

  MESSAGE 'runtime coverage message' TYPE 'I'.

  CONCATENATE cv_message lv_written lv_first lv_second lv_rest lv_offset lv_length lv_count sy-fdpos lv_calc lr_calc->* lr_line->* lv_num lv_loop
    INTO cv_message SEPARATED BY '|'.

  CLEAR lv_clear.
  REFRESH lt_scratch.
  FREE lt_scratch.
  FREE lr_calc.
ENDFORM.

START-OF-SELECTION.
  PERFORM seed_source CHANGING gt_source.
  PERFORM transform_source USING gt_source CHANGING gt_target gv_total.
  PERFORM summarize USING gt_target CHANGING gv_message.
  WRITE: / gv_total, gv_message.

  DATA(lv_add_res) = lcl_class=>add( iv_val1 = 1 iv_val2 = 3 ).
  WRITE: / lv_add_res.
  DATA(lo_class) = NEW lcl_class( ).
  DATA(lv_mul_res) = lo_class->mul( iv_val1 = 1 iv_val2 = 3 ).
  WRITE: / lv_mul_res.

  PERFORM exercise_runtime_coverage CHANGING gv_runtime_message.
  WRITE: / gv_runtime_message.
