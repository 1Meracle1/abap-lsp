MODULE user_command_9000 INPUT.
  PERFORM create_sto.
ENDMODULE.

MODULE status_9000 OUTPUT.
  gv_log = |screen status { gv_api_status }|.
ENDMODULE.
