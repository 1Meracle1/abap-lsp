CLASS zcl_perf_export_helper DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS status RETURNING VALUE(rv_status) TYPE zzp_perf_status.
ENDCLASS.

CLASS zcl_perf_export_helper IMPLEMENTATION.
  METHOD status.
    rv_status = 'A'.
  ENDMETHOD.
ENDCLASS.
