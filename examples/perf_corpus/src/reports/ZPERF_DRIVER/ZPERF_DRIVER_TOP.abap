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

DATA gs_header TYPE ty_header.
DATA gt_items TYPE ty_item_tab.
DATA gv_api_status TYPE string.
DATA gv_log TYPE string.
