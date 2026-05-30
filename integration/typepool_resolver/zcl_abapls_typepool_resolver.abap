CLASS zcl_abapls_typepool_resolver DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_extension.

  PRIVATE SECTION.
    CLASS-METHODS resolve_owner
      IMPORTING iv_symbol TYPE string
      RETURNING VALUE(rv_pool) TYPE string.

    CLASS-METHODS existing_typegroup
      IMPORTING iv_pool TYPE string
      RETURNING VALUE(rv_pool) TYPE string.

    CLASS-METHODS read_typepool_source
      IMPORTING iv_pool TYPE string
      RETURNING VALUE(rv_source) TYPE string.

    CLASS-METHODS send_text
      IMPORTING
        io_server TYPE REF TO if_http_server
        iv_status TYPE i
        iv_reason TYPE string
        iv_body   TYPE string.
ENDCLASS.

CLASS zcl_abapls_typepool_resolver IMPLEMENTATION.
  METHOD if_http_extension~handle_request.
    DATA lv_op TYPE string.
    DATA lv_name TYPE string.
    DATA lv_pool TYPE string.
    DATA lv_source TYPE string.

    lv_op = server->request->get_form_field( 'op' ).
    TRANSLATE lv_op TO LOWER CASE.

    IF lv_op = 'owner'.
      lv_name = server->request->get_form_field( 'name' ).
      lv_pool = resolve_owner( lv_name ).
      IF lv_pool IS INITIAL.
        send_text( io_server = server iv_status = 404 iv_reason = 'Not Found' iv_body = '' ).
        RETURN.
      ENDIF.
      send_text( io_server = server iv_status = 200 iv_reason = 'OK' iv_body = lv_pool ).
      RETURN.
    ENDIF.

    IF lv_op = 'source'.
      lv_pool = server->request->get_form_field( 'pool' ).
      lv_source = read_typepool_source( lv_pool ).
      IF lv_source IS INITIAL.
        send_text( io_server = server iv_status = 404 iv_reason = 'Not Found' iv_body = '' ).
        RETURN.
      ENDIF.
      send_text( io_server = server iv_status = 200 iv_reason = 'OK' iv_body = lv_source ).
      RETURN.
    ENDIF.

    send_text( io_server = server iv_status = 400 iv_reason = 'Bad Request' iv_body = '' ).
  ENDMETHOD.

  METHOD resolve_owner.
    DATA lv_symbol TYPE string.
    DATA lv_symbol_len TYPE i.
    DATA lv_index TYPE i.
    DATA lv_candidate TYPE string.
    DATA lv_prefix_len TYPE i.
    DATA lv_pattern TYPE string.
    DATA lv_typegroup TYPE ddtypet-typegroup.
    DATA lv_pool TYPE string.
    DATA lv_pool_len TYPE i.
    DATA lv_best TYPE string.
    DATA lv_best_len TYPE i.

    lv_symbol = iv_symbol.
    TRANSLATE lv_symbol TO UPPER CASE.
    CONDENSE lv_symbol NO-GAPS.
    lv_symbol_len = strlen( lv_symbol ).
    IF lv_symbol_len = 0.
      RETURN.
    ENDIF.

    DO lv_symbol_len TIMES.
      lv_index = sy-index - 1.
      IF lv_index > 0 AND lv_symbol+lv_index(1) = '_'.
        lv_candidate = lv_symbol(lv_index).
        rv_pool = existing_typegroup( lv_candidate ).
        IF rv_pool IS NOT INITIAL.
          RETURN.
        ENDIF.
      ENDIF.
    ENDDO.

    lv_prefix_len = lv_symbol_len.
    IF lv_prefix_len > 5.
      lv_prefix_len = 5.
    ENDIF.

    WHILE lv_prefix_len >= 2.
      lv_candidate = lv_symbol(lv_prefix_len).
      CONCATENATE lv_candidate '%' INTO lv_pattern.
      CLEAR: lv_best, lv_best_len.

      SELECT typegroup
        FROM ddtypet
        INTO lv_typegroup
        WHERE typegroup LIKE lv_pattern.
        lv_pool = lv_typegroup.
        CONDENSE lv_pool NO-GAPS.
        lv_pool_len = strlen( lv_pool ).
        IF lv_pool_len > lv_best_len
           AND lv_pool_len <= lv_symbol_len
           AND lv_symbol(lv_pool_len) = lv_pool.
          lv_best = lv_pool.
          lv_best_len = lv_pool_len.
        ENDIF.
      ENDSELECT.

      IF lv_best IS NOT INITIAL.
        rv_pool = existing_typegroup( lv_best ).
        IF rv_pool IS NOT INITIAL.
          RETURN.
        ENDIF.
      ENDIF.

      lv_prefix_len = lv_prefix_len - 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD existing_typegroup.
    DATA lv_pool TYPE string.
    DATA lv_typegroup TYPE ddtypet-typegroup.
    DATA lv_source TYPE string.

    lv_pool = iv_pool.
    TRANSLATE lv_pool TO UPPER CASE.
    CONDENSE lv_pool NO-GAPS.
    IF lv_pool IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE typegroup
      FROM ddtypet
      INTO lv_typegroup
      WHERE typegroup = lv_pool.
    IF sy-subrc = 0.
      rv_pool = lv_typegroup.
      RETURN.
    ENDIF.

    lv_source = read_typepool_source( lv_pool ).
    IF lv_source IS NOT INITIAL.
      rv_pool = lv_pool.
    ENDIF.
  ENDMETHOD.

  METHOD read_typepool_source.
    DATA lv_pool TYPE c LENGTH 30.
    DATA lv_program TYPE c LENGTH 40.
    DATA lv_line TYPE string.
    DATA lt_source TYPE STANDARD TABLE OF string.

    lv_pool = iv_pool.
    TRANSLATE lv_pool TO UPPER CASE.
    CONCATENATE '%_C' lv_pool INTO lv_program.
    READ REPORT lv_program INTO lt_source.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_source INTO lv_line.
      CONCATENATE rv_source lv_line cl_abap_char_utilities=>newline INTO rv_source.
    ENDLOOP.
  ENDMETHOD.

  METHOD send_text.
    io_server->response->set_status( code = iv_status reason = iv_reason ).
    io_server->response->set_header_field(
      name  = 'Content-Type'
      value = 'text/plain; charset=utf-8' ).
    io_server->response->set_cdata( iv_body ).
  ENDMETHOD.
ENDCLASS.
