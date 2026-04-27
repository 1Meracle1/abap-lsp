REPORT zperf_driver.

PARAMETERS p_vendor TYPE string.
PARAMETERS p_plant TYPE string.

INCLUDE zperf_driver_top.
INCLUDE zperf_driver_f01.
INCLUDE zperf_driver_pai.

INITIALIZATION.
  p_vendor = 'VEND-001'.
  p_plant = 'PL01'.

START-OF-SELECTION.
  PERFORM create_sto.

END-OF-SELECTION.
  CALL SCREEN 9000.
