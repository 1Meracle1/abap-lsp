CLASS zcl_perf_service DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS normalize_material
      IMPORTING
        iv_material TYPE string
      RETURNING VALUE(rv_material) TYPE string.
    CLASS-METHODS enrich_message
      IMPORTING
        iv_material TYPE string
        iv_quantity TYPE i
      RETURNING VALUE(rv_message) TYPE string.
ENDCLASS.

CLASS zcl_perf_service IMPLEMENTATION.
  METHOD normalize_material.
    rv_material = iv_material.
    TRANSLATE rv_material TO UPPER CASE.
  ENDMETHOD.

  METHOD enrich_message.
    rv_message = |{ iv_material }:{ iv_quantity }|.
  ENDMETHOD.
ENDCLASS.
