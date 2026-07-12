REPORT zvm_memory_aggregates.

TYPES: BEGIN OF ty_pair,
         left  TYPE i,
         right TYPE i,
       END OF ty_pair.

DATA gs_pair TYPE ty_pair.
DATA gr_value TYPE REF TO i.

START-OF-SELECTION.
  gs_pair = VALUE ty_pair(
    left  = 7
    right = 11 ).

  CREATE DATA gr_value.
  gr_value->* = gs_pair-left + gs_pair-right.

  WRITE gs_pair-left.
  WRITE gs_pair-right.
  WRITE gr_value->*.
