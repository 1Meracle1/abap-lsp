REPORT zexception_trace.

FORM top_layer.
  PERFORM middle_layer.
ENDFORM.

FORM middle_layer.
  PERFORM bottom_layer.
ENDFORM.

FORM bottom_layer.
  RAISE EXCEPTION TYPE cx_root.
ENDFORM.

START-OF-SELECTION.
  PERFORM top_layer.
