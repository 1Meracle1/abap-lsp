FORM build_header CHANGING cs_header TYPE ty_header.
  DATA lv_doc TYPE string.

  lv_doc = 'NB'.
  cs_header-doc_type = lv_doc.
  cs_header-vendor = p_vendor.
  cs_header-plant = p_plant.
  cs_header-ref_1 = |{ p_vendor }/{ p_plant }|.
ENDFORM.

FORM build_items CHANGING ct_items TYPE ty_item_tab.
  DATA ls_item TYPE ty_item.

  CLEAR ls_item.
  ls_item-material = zcl_perf_service=>normalize_material( 'mat-001' ).
  ls_item-plant = p_plant.
  ls_item-quantity = 10.
  ls_item-message = zcl_perf_service=>enrich_message(
    iv_material = ls_item-material
    iv_quantity = ls_item-quantity ).
  APPEND ls_item TO ct_items.

  CLEAR ls_item.
  ls_item-material = zcl_perf_service=>normalize_material( 'mat-002' ).
  ls_item-plant = p_plant.
  ls_item-quantity = 20.
  ls_item-message = zcl_perf_service=>enrich_message(
    iv_material = ls_item-material
    iv_quantity = ls_item-quantity ).
  APPEND ls_item TO ct_items.
ENDFORM.

FORM call_api USING us_header TYPE ty_header
                    ut_items TYPE ty_item_tab
              CHANGING cv_status TYPE string.
  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader = us_header
    IMPORTING
      ev_status = cv_status
    TABLES
      poitem = ut_items.
ENDFORM.

FORM create_sto.
  REFRESH gt_items.
  PERFORM build_header CHANGING gs_header.
  PERFORM build_items CHANGING gt_items.
  PERFORM call_api USING gs_header gt_items
                   CHANGING gv_api_status.
  gv_log = |status={ gv_api_status } items={ lines( gt_items ) }|.
ENDFORM.
