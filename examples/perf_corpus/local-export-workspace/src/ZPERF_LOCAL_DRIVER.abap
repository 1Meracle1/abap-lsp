REPORT zperf_local_driver.

DATA go_service TYPE REF TO zcl_perf_export_service.
DATA gv_status TYPE zzp_perf_status.

START-OF-SELECTION.
  CREATE OBJECT go_service.
  gv_status = go_service->run( ).
