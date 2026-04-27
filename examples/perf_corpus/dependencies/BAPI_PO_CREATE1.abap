TYPES: BEGIN OF ty_header,
         doc_type TYPE string,
         vendor TYPE string,
         plant TYPE string,
         ref_1 TYPE string,
       END OF ty_header.

TYPES: BEGIN OF ty_item,
         material TYPE string,
         plant TYPE string,
         quantity TYPE i,
         message TYPE string,
       END OF ty_item.
TYPES ty_item_tab TYPE STANDARD TABLE OF ty_item WITH EMPTY KEY.

FUNCTION bapi_po_create1
  IMPORTING
    poheader TYPE ty_header
  EXPORTING
    ev_status TYPE string
  TABLES
    poitem TYPE ty_item.
  ev_status = 'S'.
ENDFUNCTION.
