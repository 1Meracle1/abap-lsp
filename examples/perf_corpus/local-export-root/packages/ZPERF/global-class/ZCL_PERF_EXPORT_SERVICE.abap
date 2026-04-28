CLASS zcl_perf_export_service DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS run RETURNING VALUE(rv_status) TYPE zzp_perf_status.
ENDCLASS.

CLASS zcl_perf_export_service IMPLEMENTATION.
  METHOD run.
    DATA lo_helper TYPE REF TO zcl_perf_export_helper.
    CREATE OBJECT lo_helper.
    rv_status = lo_helper->status( ).
  ENDMETHOD.
ENDCLASS.
