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

START-OF-SELECTION.
  PERFORM seed_source CHANGING gt_source.
  PERFORM transform_source USING gt_source CHANGING gt_target gv_total.
  PERFORM summarize USING gt_target CHANGING gv_message.
  WRITE: / gv_total, gv_message.
