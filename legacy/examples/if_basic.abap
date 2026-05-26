DATA lv_flag TYPE abap_bool.
IF lv_flag = abap_true.
  lv_flag = abap_false.
ELSE.
  lv_flag = abap_true.
ENDIF.
