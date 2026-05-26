REPORT z_rsa_pkcs1_v15_encrypt_public_key.

PARAMETERS:
  p_text TYPE string LOWER CASE OBLIGATORY,
  p_pub  TYPE string LOWER CASE OBLIGATORY.

DATA:
  gv_encrypted_b64 TYPE string,
  gv_saved_path    TYPE string.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pub.
  PERFORM pick_public_key_file CHANGING p_pub.

START-OF-SELECTION.
  PERFORM run.

FORM run.
  DATA: lv_key_file_x   TYPE xstring,
        lv_key_text     TYPE string,
        lv_modulus_x    TYPE xstring,
        lv_exponent_x   TYPE xstring,
        lv_plaintext_x  TYPE xstring,
        lv_ciphertext_x TYPE xstring.

  PERFORM load_file_binary USING p_pub CHANGING lv_key_file_x.
  PERFORM utf8_xstring_to_string USING lv_key_file_x CHANGING lv_key_text.
  PERFORM parse_public_key USING lv_key_text CHANGING lv_modulus_x lv_exponent_x.
  PERFORM utf8_string_to_xstring USING p_text CHANGING lv_plaintext_x.

  " Match Python's public_key.encrypt(..., PKCS1v15()) on the raw payload.
  PERFORM rsaes_pkcs1_v15_encrypt USING lv_modulus_x lv_exponent_x lv_plaintext_x
                                  CHANGING lv_ciphertext_x.

  gv_encrypted_b64 = cl_http_utility=>encode_x_base64( lv_ciphertext_x ).

  PERFORM prompt_save_file CHANGING gv_saved_path.
  IF gv_saved_path IS NOT INITIAL.
    PERFORM save_text_file USING gv_saved_path gv_encrypted_b64.
    WRITE: / 'Encrypted output written to:', / gv_saved_path.
  ELSE.
    WRITE: / 'Encrypted output was not saved.'.
  ENDIF.

  WRITE: /.
  WRITE: / 'Encrypted output (Base64):'.
  WRITE: / gv_encrypted_b64.
  WRITE: /.
  WRITE: / 'Note: PKCS#1 v1.5 padding is randomized, so the ciphertext changes on every run.'.
ENDFORM.

FORM pick_public_key_file CHANGING cv_path TYPE string.
  DATA: lt_files  TYPE filetable,
        ls_file   TYPE file_table,
        lv_rc     TYPE i,
        lv_action TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title = 'Choose public key file'
      file_filter  = 'Public Key Files (*.pem;*.pub;*.xml)|*.pem;*.pub;*.xml|All Files (*.*)|*.*|'
    CHANGING
      file_table   = lt_files
      rc           = lv_rc
      user_action  = lv_action
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  IF lv_action <> cl_gui_frontend_services=>action_ok OR lv_rc = 0.
    RETURN.
  ENDIF.

  READ TABLE lt_files INTO ls_file INDEX 1.
  IF sy-subrc = 0.
    cv_path = ls_file-filename.
  ENDIF.
ENDFORM.

FORM prompt_save_file CHANGING cv_path TYPE string.
  DATA: lv_filename TYPE string,
        lv_path     TYPE string,
        lv_fullpath TYPE string,
        lv_action   TYPE i.

  CLEAR cv_path.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title        = 'Save encrypted output'
      default_extension   = 'b64'
      default_file_name   = 'encrypted.b64'
      file_filter         = 'Base64 Files (*.b64;*.txt)|*.b64;*.txt|All Files (*.*)|*.*|'
      prompt_on_overwrite = abap_true
    CHANGING
      filename            = lv_filename
      path                = lv_path
      fullpath            = lv_fullpath
      user_action         = lv_action
    EXCEPTIONS
      OTHERS              = 1.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  IF lv_action = cl_gui_frontend_services=>action_ok AND lv_fullpath IS NOT INITIAL.
    cv_path = lv_fullpath.
  ENDIF.
ENDFORM.

FORM load_file_binary USING    iv_path TYPE string
                      CHANGING cv_xstr TYPE xstring.
  DATA: lt_bin      TYPE solix_tab,
        lv_len      TYPE i,
        lv_filename TYPE string.

  lv_filename = iv_path.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename   = lv_filename
      filetype   = 'BIN'
    IMPORTING
      filelength = lv_len
    TABLES
      data_tab   = lt_bin
    EXCEPTIONS
      OTHERS     = 1.
  IF sy-subrc <> 0.
    MESSAGE |Could not read public key file "{ iv_path }".| TYPE 'E'.
  ENDIF.

  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = lv_len
    IMPORTING
      buffer       = cv_xstr
    TABLES
      binary_tab   = lt_bin
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Could not convert public key file to XSTRING.' TYPE 'E'.
  ENDIF.
ENDFORM.

FORM save_text_file USING iv_path TYPE string
                          iv_text TYPE string.
  DATA: lt_lines     TYPE STANDARD TABLE OF soli WITH DEFAULT KEY,
        ls_line      TYPE soli,
        lv_len       TYPE i,
        lv_off       TYPE i,
        lv_remaining TYPE i,
        lv_filename  TYPE string.

  lv_filename = iv_path.
  lv_len = strlen( iv_text ).

  IF lv_len = 0.
    CLEAR ls_line.
    APPEND ls_line TO lt_lines.
  ELSE.
    WHILE lv_off < lv_len.
      lv_remaining = lv_len - lv_off.
      CLEAR ls_line.

      IF lv_remaining > 255.
        ls_line-line = iv_text+lv_off(255).
        lv_off = lv_off + 255.
      ELSE.
        ls_line-line = iv_text+lv_off(lv_remaining).
        lv_off = lv_len.
      ENDIF.

      APPEND ls_line TO lt_lines.
    ENDWHILE.
  ENDIF.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename = lv_filename
      filetype = 'ASC'
    TABLES
      data_tab = lt_lines
    EXCEPTIONS
      OTHERS   = 1.
  IF sy-subrc <> 0.
    MESSAGE |Could not write output file "{ iv_path }".| TYPE 'E'.
  ENDIF.
ENDFORM.

FORM utf8_xstring_to_string USING    iv_xstr TYPE xstring
                            CHANGING cv_text TYPE string.
  cv_text = cl_secxml_helper=>utf8_2_string( iv_xstr ).
ENDFORM.

FORM utf8_string_to_xstring USING    iv_text TYPE string
                            CHANGING cv_xstr TYPE xstring.
  cv_xstr = cl_secxml_helper=>string_2_utf8( iv_text ).
ENDFORM.

FORM parse_public_key USING    iv_key_text TYPE string
                      CHANGING cv_modulus_x TYPE xstring
                               cv_exponent_x TYPE xstring.
  IF iv_key_text CS '<RSAKeyValue>'.
    PERFORM parse_xml_public_key USING iv_key_text
                                 CHANGING cv_modulus_x cv_exponent_x.
  ELSEIF iv_key_text CS '-----BEGIN PUBLIC KEY-----'
      OR iv_key_text CS '-----BEGIN RSA PUBLIC KEY-----'.
    PERFORM parse_pem_public_key USING iv_key_text
                                 CHANGING cv_modulus_x cv_exponent_x.
  ELSE.
    MESSAGE 'Unsupported public key format. Use RSAKeyValue XML, BEGIN PUBLIC KEY, or BEGIN RSA PUBLIC KEY.' TYPE 'E'.
  ENDIF.
ENDFORM.

FORM parse_xml_public_key USING    iv_key_text TYPE string
                          CHANGING cv_modulus_x TYPE xstring
                                   cv_exponent_x TYPE xstring.
  DATA: lv_modulus_b64  TYPE string,
        lv_exponent_b64 TYPE string.

  FIND FIRST OCCURRENCE OF REGEX '<Modulus>\s*([^<]+)\s*</Modulus>'
    IN iv_key_text SUBMATCHES lv_modulus_b64.
  IF sy-subrc <> 0 OR lv_modulus_b64 IS INITIAL.
    MESSAGE 'Could not find <Modulus> in RSAKeyValue.' TYPE 'E'.
  ENDIF.

  FIND FIRST OCCURRENCE OF REGEX '<Exponent>\s*([^<]+)\s*</Exponent>'
    IN iv_key_text SUBMATCHES lv_exponent_b64.
  IF sy-subrc <> 0 OR lv_exponent_b64 IS INITIAL.
    MESSAGE 'Could not find <Exponent> in RSAKeyValue.' TYPE 'E'.
  ENDIF.

  REPLACE ALL OCCURRENCES OF REGEX '\s+' IN lv_modulus_b64 WITH ''.
  REPLACE ALL OCCURRENCES OF REGEX '\s+' IN lv_exponent_b64 WITH ''.

  cv_modulus_x = cl_http_utility=>decode_x_base64( lv_modulus_b64 ).
  cv_exponent_x = cl_http_utility=>decode_x_base64( lv_exponent_b64 ).

  PERFORM strip_leading_zero_byte CHANGING cv_modulus_x.
  PERFORM strip_leading_zero_byte CHANGING cv_exponent_x.
ENDFORM.

FORM parse_pem_public_key USING    iv_key_text TYPE string
                          CHANGING cv_modulus_x TYPE xstring
                                   cv_exponent_x TYPE xstring.
  DATA: lv_pem_b64        TYPE string,
        lv_der            TYPE xstring,
        lv_is_pkcs1_pem   TYPE c LENGTH 1 VALUE ' '.

  lv_pem_b64 = iv_key_text.

  IF iv_key_text CS '-----BEGIN RSA PUBLIC KEY-----'.
    lv_is_pkcs1_pem = abap_true.
    REPLACE ALL OCCURRENCES OF '-----BEGIN RSA PUBLIC KEY-----' IN lv_pem_b64 WITH ''.
    REPLACE ALL OCCURRENCES OF '-----END RSA PUBLIC KEY-----' IN lv_pem_b64 WITH ''.
  ELSE.
    REPLACE ALL OCCURRENCES OF '-----BEGIN PUBLIC KEY-----' IN lv_pem_b64 WITH ''.
    REPLACE ALL OCCURRENCES OF '-----END PUBLIC KEY-----' IN lv_pem_b64 WITH ''.
  ENDIF.

  REPLACE ALL OCCURRENCES OF REGEX '\s+' IN lv_pem_b64 WITH ''.
  IF lv_pem_b64 IS INITIAL.
    MESSAGE 'PEM public key payload is empty.' TYPE 'E'.
  ENDIF.

  lv_der = cl_http_utility=>decode_x_base64( lv_pem_b64 ).

  IF lv_is_pkcs1_pem = abap_true.
    PERFORM parse_rsa_public_key_der USING lv_der
                                     CHANGING cv_modulus_x cv_exponent_x.
  ELSE.
    PERFORM parse_subject_public_key_info USING lv_der
                                          CHANGING cv_modulus_x cv_exponent_x.
  ENDIF.
ENDFORM.

FORM parse_subject_public_key_info USING    iv_der TYPE xstring
                                   CHANGING cv_modulus_x TYPE xstring
                                            cv_exponent_x TYPE xstring.
  DATA: lv_offset TYPE i VALUE 0,
        lv_tag    TYPE x LENGTH 1,
        lv_len    TYPE i,
        lv_unused TYPE x LENGTH 1,
        lv_rsa_len TYPE i,
        lv_rsa_der TYPE xstring.

  lv_tag = iv_der+lv_offset(1).
  IF lv_tag <> '30'.
    MESSAGE 'Invalid public key DER: expected outer SEQUENCE.' TYPE 'E'.
  ENDIF.
  lv_offset = lv_offset + 1.
  PERFORM read_der_length USING iv_der lv_offset CHANGING lv_len lv_offset.

  lv_tag = iv_der+lv_offset(1).
  IF lv_tag <> '30'.
    MESSAGE 'Invalid public key DER: expected algorithm SEQUENCE.' TYPE 'E'.
  ENDIF.
  lv_offset = lv_offset + 1.
  PERFORM read_der_length USING iv_der lv_offset CHANGING lv_len lv_offset.
  lv_offset = lv_offset + lv_len.

  lv_tag = iv_der+lv_offset(1).
  IF lv_tag <> '03'.
    MESSAGE 'Invalid public key DER: expected BIT STRING.' TYPE 'E'.
  ENDIF.
  lv_offset = lv_offset + 1.
  PERFORM read_der_length USING iv_der lv_offset CHANGING lv_len lv_offset.

  lv_unused = iv_der+lv_offset(1).
  IF lv_unused <> '00'.
    MESSAGE 'Invalid public key DER: unsupported BIT STRING padding.' TYPE 'E'.
  ENDIF.
  lv_offset = lv_offset + 1.

  lv_rsa_len = lv_len - 1.
  lv_rsa_der = iv_der+lv_offset(lv_rsa_len).
  PERFORM parse_rsa_public_key_der USING lv_rsa_der
                                   CHANGING cv_modulus_x cv_exponent_x.
ENDFORM.

FORM parse_rsa_public_key_der USING    iv_der TYPE xstring
                              CHANGING cv_modulus_x TYPE xstring
                                       cv_exponent_x TYPE xstring.
  DATA: lv_offset TYPE i VALUE 0,
        lv_tag    TYPE x LENGTH 1,
        lv_len    TYPE i.

  lv_tag = iv_der+lv_offset(1).
  IF lv_tag <> '30'.
    MESSAGE 'Invalid RSA public key DER: expected SEQUENCE.' TYPE 'E'.
  ENDIF.
  lv_offset = lv_offset + 1.
  PERFORM read_der_length USING iv_der lv_offset CHANGING lv_len lv_offset.

  PERFORM read_der_integer USING iv_der lv_offset CHANGING cv_modulus_x lv_offset.
  PERFORM read_der_integer USING iv_der lv_offset CHANGING cv_exponent_x lv_offset.
ENDFORM.

FORM read_der_integer USING    iv_der TYPE xstring
                               iv_offset TYPE i
                      CHANGING cv_value TYPE xstring
                               cv_new_offset TYPE i.
  DATA: lv_tag TYPE x LENGTH 1,
        lv_len TYPE i.

  cv_new_offset = iv_offset.
  lv_tag = iv_der+cv_new_offset(1).
  IF lv_tag <> '02'.
    MESSAGE 'Invalid RSA public key DER: expected INTEGER.' TYPE 'E'.
  ENDIF.
  cv_new_offset = cv_new_offset + 1.

  PERFORM read_der_length USING iv_der cv_new_offset CHANGING lv_len cv_new_offset.
  cv_value = iv_der+cv_new_offset(lv_len).
  cv_new_offset = cv_new_offset + lv_len.

  PERFORM strip_leading_zero_byte CHANGING cv_value.
ENDFORM.

FORM read_der_length USING    iv_der TYPE xstring
                              iv_offset TYPE i
                     CHANGING cv_length TYPE i
                              cv_new_offset TYPE i.
  DATA: lv_len_byte TYPE x LENGTH 1,
        lv_len_i    TYPE i,
        lv_count    TYPE i,
        lv_part     TYPE x LENGTH 1,
        lv_part_i   TYPE i.

  cv_new_offset = iv_offset.
  lv_len_byte = iv_der+cv_new_offset(1).
  cv_new_offset = cv_new_offset + 1.

  lv_len_i = lv_len_byte.
  IF lv_len_i < 128.
    cv_length = lv_len_i.
    RETURN.
  ENDIF.

  lv_count = lv_len_i - 128.
  IF lv_count <= 0.
    MESSAGE 'Invalid DER length encoding.' TYPE 'E'.
  ENDIF.

  CLEAR cv_length.
  DO lv_count TIMES.
    lv_part = iv_der+cv_new_offset(1).
    cv_new_offset = cv_new_offset + 1.
    lv_part_i = lv_part.
    cv_length = ( cv_length * 256 ) + lv_part_i.
  ENDDO.
ENDFORM.

FORM strip_leading_zero_byte CHANGING cv_value TYPE xstring.
  DATA lv_len TYPE i.

  WHILE xstrlen( cv_value ) > 0 AND cv_value(1) = '00'.
    lv_len = xstrlen( cv_value ) - 1.
    IF lv_len <= 0.
      CLEAR cv_value.
      EXIT.
    ENDIF.
    cv_value = cv_value+1(lv_len).
  ENDWHILE.
ENDFORM.

FORM rsaes_pkcs1_v15_encrypt USING    iv_modulus_x TYPE xstring
                                      iv_exponent_x TYPE xstring
                                      iv_plaintext_x TYPE xstring
                             CHANGING cv_ciphertext_x TYPE xstring.
  DATA: lv_key_len      TYPE i,
        lv_padding_len  TYPE i,
        lv_padding_x    TYPE xstring,
        lv_message_x    TYPE xstring,
        lv_prefix_x     TYPE xstring VALUE '0002',
        lv_separator_x  TYPE x LENGTH 1 VALUE '00'.

  lv_key_len = xstrlen( iv_modulus_x ).
  IF lv_key_len < 11.
    MESSAGE 'RSA modulus is too short for PKCS#1 v1.5 encryption.' TYPE 'E'.
  ENDIF.

  IF xstrlen( iv_plaintext_x ) > lv_key_len - 11.
    MESSAGE |Plaintext is too long for this RSA key. Max { lv_key_len - 11 } bytes, got { xstrlen( iv_plaintext_x ) }.| TYPE 'E'.
  ENDIF.

  lv_padding_len = lv_key_len - xstrlen( iv_plaintext_x ) - 3.
  IF lv_padding_len < 8.
    MESSAGE 'PKCS#1 v1.5 requires at least 8 bytes of padding.' TYPE 'E'.
  ENDIF.

  PERFORM generate_nonzero_random USING lv_padding_len CHANGING lv_padding_x.
  CONCATENATE lv_prefix_x lv_padding_x lv_separator_x iv_plaintext_x
    INTO lv_message_x IN BYTE MODE.

  IF xstrlen( lv_message_x ) <> lv_key_len.
    MESSAGE 'Could not build a PKCS#1 v1.5 message block of the correct size.' TYPE 'E'.
  ENDIF.

  PERFORM modexp USING lv_message_x iv_exponent_x iv_modulus_x
                 CHANGING cv_ciphertext_x.
ENDFORM.

FORM generate_nonzero_random USING    iv_length TYPE i
                             CHANGING cv_random TYPE xstring.
  DATA: lv_chunk_x   TYPE xstring,
        lv_request   TYPE i,
        lv_idx       TYPE i,
        lv_byte_x    TYPE x LENGTH 1,
        lv_byte_i    TYPE i.

  CLEAR cv_random.
  IF iv_length <= 0.
    RETURN.
  ENDIF.

  WHILE xstrlen( cv_random ) < iv_length.
    lv_request = iv_length - xstrlen( cv_random ).
    IF lv_request < 16.
      lv_request = 16.
    ELSEIF lv_request > 32.
      lv_request = 32.
    ELSEIF lv_request > 24.
      lv_request = 32.
    ELSEIF lv_request > 16.
      lv_request = 24.
    ENDIF.

    PERFORM generate_random_bytes USING lv_request CHANGING lv_chunk_x.

    DO xstrlen( lv_chunk_x ) TIMES.
      lv_idx = sy-index - 1.
      lv_byte_x = lv_chunk_x+lv_idx(1).
      lv_byte_i = lv_byte_x.
      IF lv_byte_i = 0.
        CONTINUE.
      ENDIF.

      CONCATENATE cv_random lv_byte_x INTO cv_random IN BYTE MODE.
      IF xstrlen( cv_random ) >= iv_length.
        EXIT.
      ENDIF.
    ENDDO.
  ENDWHILE.
ENDFORM.

FORM generate_random_bytes USING    iv_length TYPE i
                           CHANGING cv_random TYPE xstring.
  DATA: lv_needed    TYPE i,
        lv_request   TYPE i,
        lv_chunk_x   TYPE xstring.

  CLEAR cv_random.
  IF iv_length <= 0.
    RETURN.
  ENDIF.

  WHILE xstrlen( cv_random ) < iv_length.
    lv_needed = iv_length - xstrlen( cv_random ).
    lv_request = lv_needed.

    IF lv_request < 16.
      lv_request = 16.
    ELSEIF lv_request > 32.
      lv_request = 32.
    ELSEIF lv_request > 24.
      lv_request = 32.
    ELSEIF lv_request > 16.
      lv_request = 24.
    ENDIF.

    CALL FUNCTION 'GENERATE_SEC_RANDOM'
      EXPORTING
        length         = lv_request
      IMPORTING
        random         = lv_chunk_x
      EXCEPTIONS
        invalid_length = 1
        no_memory      = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      MESSAGE |GENERATE_SEC_RANDOM failed (SY-SUBRC={ sy-subrc }).| TYPE 'E'.
    ENDIF.

    lv_needed = iv_length - xstrlen( cv_random ).
    IF xstrlen( lv_chunk_x ) > lv_needed.
      lv_chunk_x = lv_chunk_x(lv_needed).
    ENDIF.

    CONCATENATE cv_random lv_chunk_x INTO cv_random IN BYTE MODE.
  ENDWHILE.
ENDFORM.

FORM modexp USING    iv_base TYPE xstring
                     iv_exponent TYPE xstring
                     iv_modulus TYPE xstring
            CHANGING cv_result TYPE xstring.
  DATA: lv_base_x    TYPE xstring,
        lv_result_x  TYPE xstring,
        lv_mod_len   TYPE i,
        lv_exp_len   TYPE i,
        lv_idx       TYPE i,
        lv_byte_x    TYPE x LENGTH 1,
        lv_byte_i    TYPE i.

  lv_mod_len = xstrlen( iv_modulus ).
  IF lv_mod_len = 0.
    MESSAGE 'RSA modulus must not be empty.' TYPE 'E'.
  ENDIF.
  IF iv_exponent IS INITIAL.
    MESSAGE 'RSA exponent must not be empty.' TYPE 'E'.
  ENDIF.

  PERFORM left_pad_zero USING iv_base lv_mod_len CHANGING lv_base_x.
  PERFORM make_one_block USING lv_mod_len CHANGING lv_result_x.

  lv_exp_len = xstrlen( iv_exponent ).
  DO lv_exp_len TIMES.
    lv_idx = lv_exp_len - sy-index.
    lv_byte_x = iv_exponent+lv_idx(1).
    lv_byte_i = lv_byte_x.

    DO 8 TIMES.
      IF lv_byte_i MOD 2 = 1.
        PERFORM multiply_mod USING lv_result_x lv_base_x iv_modulus
                             CHANGING lv_result_x.
      ENDIF.

      lv_byte_i = lv_byte_i DIV 2.
      PERFORM multiply_mod USING lv_base_x lv_base_x iv_modulus
                           CHANGING lv_base_x.
    ENDDO.
  ENDDO.

  cv_result = lv_result_x.
ENDFORM.

FORM multiply_mod USING    iv_left TYPE xstring
                           iv_right TYPE xstring
                           iv_modulus TYPE xstring
                  CHANGING cv_product TYPE xstring.
  DATA: lv_left_x    TYPE xstring,
        lv_right_x   TYPE xstring,
        lv_result_x  TYPE xstring,
        lv_mod_len   TYPE i,
        lv_idx       TYPE i,
        lv_byte_x    TYPE x LENGTH 1,
        lv_byte_i    TYPE i.

  lv_mod_len = xstrlen( iv_modulus ).
  PERFORM left_pad_zero USING iv_left lv_mod_len CHANGING lv_left_x.
  PERFORM left_pad_zero USING iv_right lv_mod_len CHANGING lv_right_x.
  PERFORM make_zero_block USING lv_mod_len CHANGING lv_result_x.

  DO lv_mod_len TIMES.
    lv_idx = lv_mod_len - sy-index.
    lv_byte_x = lv_right_x+lv_idx(1).
    lv_byte_i = lv_byte_x.

    DO 8 TIMES.
      IF lv_byte_i MOD 2 = 1.
        PERFORM add_mod USING lv_result_x lv_left_x iv_modulus
                        CHANGING lv_result_x.
      ENDIF.

      lv_byte_i = lv_byte_i DIV 2.
      PERFORM add_mod USING lv_left_x lv_left_x iv_modulus
                      CHANGING lv_left_x.
    ENDDO.
  ENDDO.

  cv_product = lv_result_x.
ENDFORM.

FORM add_mod USING    iv_left TYPE xstring
                      iv_right TYPE xstring
                      iv_modulus TYPE xstring
             CHANGING cv_sum TYPE xstring.
  DATA: lv_left_x       TYPE xstring,
        lv_right_x      TYPE xstring,
        lv_modulus_x    TYPE xstring,
        lv_mod_minus_x  TYPE xstring,
        lv_cmp          TYPE i,
        lv_mod_len      TYPE i.

  lv_mod_len = xstrlen( iv_modulus ).
  PERFORM left_pad_zero USING iv_left lv_mod_len CHANGING lv_left_x.
  PERFORM left_pad_zero USING iv_right lv_mod_len CHANGING lv_right_x.
  PERFORM left_pad_zero USING iv_modulus lv_mod_len CHANGING lv_modulus_x.

  PERFORM subtract_bigint USING lv_modulus_x lv_right_x CHANGING lv_mod_minus_x.
  PERFORM compare_bigint USING lv_left_x lv_mod_minus_x CHANGING lv_cmp.

  IF lv_cmp >= 0.
    PERFORM subtract_bigint USING lv_left_x lv_mod_minus_x CHANGING cv_sum.
  ELSE.
    PERFORM add_bigint USING lv_left_x lv_right_x CHANGING cv_sum.
  ENDIF.
ENDFORM.

FORM add_bigint USING    iv_left TYPE xstring
                         iv_right TYPE xstring
                CHANGING cv_sum TYPE xstring.
  DATA: lv_left_x    TYPE xstring,
        lv_right_x   TYPE xstring,
        lv_len       TYPE i,
        lv_left_len  TYPE i,
        lv_right_len TYPE i,
        lv_idx       TYPE i,
        lv_carry     TYPE i,
        lv_left_i    TYPE i,
        lv_right_i   TYPE i,
        lv_sum_i     TYPE i,
        lv_left_b    TYPE x LENGTH 1,
        lv_right_b   TYPE x LENGTH 1,
        lv_sum_b     TYPE x LENGTH 1.

  lv_left_len = xstrlen( iv_left ).
  lv_right_len = xstrlen( iv_right ).
  IF lv_left_len > lv_right_len.
    lv_len = lv_left_len.
  ELSE.
    lv_len = lv_right_len.
  ENDIF.

  PERFORM left_pad_zero USING iv_left lv_len CHANGING lv_left_x.
  PERFORM left_pad_zero USING iv_right lv_len CHANGING lv_right_x.

  CLEAR cv_sum.
  DO lv_len TIMES.
    lv_idx = lv_len - sy-index.
    lv_left_b = lv_left_x+lv_idx(1).
    lv_right_b = lv_right_x+lv_idx(1).
    lv_left_i = lv_left_b.
    lv_right_i = lv_right_b.

    lv_sum_i = lv_left_i + lv_right_i + lv_carry.
    IF lv_sum_i >= 256.
      lv_sum_i = lv_sum_i - 256.
      lv_carry = 1.
    ELSE.
      lv_carry = 0.
    ENDIF.

    lv_sum_b = lv_sum_i.
    CONCATENATE lv_sum_b cv_sum INTO cv_sum IN BYTE MODE.
  ENDDO.

  IF lv_carry <> 0.
    MESSAGE 'Big integer addition overflowed unexpectedly.' TYPE 'E'.
  ENDIF.
ENDFORM.

FORM subtract_bigint USING    iv_left TYPE xstring
                              iv_right TYPE xstring
                     CHANGING cv_diff TYPE xstring.
  DATA: lv_left_x    TYPE xstring,
        lv_right_x   TYPE xstring,
        lv_len       TYPE i,
        lv_left_len  TYPE i,
        lv_right_len TYPE i,
        lv_cmp       TYPE i,
        lv_idx       TYPE i,
        lv_borrow    TYPE i,
        lv_left_i    TYPE i,
        lv_right_i   TYPE i,
        lv_diff_i    TYPE i,
        lv_left_b    TYPE x LENGTH 1,
        lv_right_b   TYPE x LENGTH 1,
        lv_diff_b    TYPE x LENGTH 1.

  lv_left_len = xstrlen( iv_left ).
  lv_right_len = xstrlen( iv_right ).
  IF lv_left_len > lv_right_len.
    lv_len = lv_left_len.
  ELSE.
    lv_len = lv_right_len.
  ENDIF.

  PERFORM left_pad_zero USING iv_left lv_len CHANGING lv_left_x.
  PERFORM left_pad_zero USING iv_right lv_len CHANGING lv_right_x.
  PERFORM compare_bigint USING lv_left_x lv_right_x CHANGING lv_cmp.
  IF lv_cmp < 0.
    MESSAGE 'Big integer subtraction underflowed unexpectedly.' TYPE 'E'.
  ENDIF.

  CLEAR cv_diff.
  DO lv_len TIMES.
    lv_idx = lv_len - sy-index.
    lv_left_b = lv_left_x+lv_idx(1).
    lv_right_b = lv_right_x+lv_idx(1).
    lv_left_i = lv_left_b.
    lv_right_i = lv_right_b.

    lv_diff_i = lv_left_i - lv_right_i - lv_borrow.
    IF lv_diff_i < 0.
      lv_diff_i = lv_diff_i + 256.
      lv_borrow = 1.
    ELSE.
      lv_borrow = 0.
    ENDIF.

    lv_diff_b = lv_diff_i.
    CONCATENATE lv_diff_b cv_diff INTO cv_diff IN BYTE MODE.
  ENDDO.

  IF lv_borrow <> 0.
    MESSAGE 'Big integer subtraction ended with a borrow unexpectedly.' TYPE 'E'.
  ENDIF.
ENDFORM.

FORM compare_bigint USING    iv_left TYPE xstring
                             iv_right TYPE xstring
                    CHANGING cv_result TYPE i.
  DATA: lv_left_x    TYPE xstring,
        lv_right_x   TYPE xstring,
        lv_len       TYPE i,
        lv_left_len  TYPE i,
        lv_right_len TYPE i,
        lv_idx       TYPE i,
        lv_left_i    TYPE i,
        lv_right_i   TYPE i,
        lv_left_b    TYPE x LENGTH 1,
        lv_right_b   TYPE x LENGTH 1.

  lv_left_len = xstrlen( iv_left ).
  lv_right_len = xstrlen( iv_right ).
  IF lv_left_len > lv_right_len.
    lv_len = lv_left_len.
  ELSE.
    lv_len = lv_right_len.
  ENDIF.

  PERFORM left_pad_zero USING iv_left lv_len CHANGING lv_left_x.
  PERFORM left_pad_zero USING iv_right lv_len CHANGING lv_right_x.

  cv_result = 0.
  DO lv_len TIMES.
    lv_idx = sy-index - 1.
    lv_left_b = lv_left_x+lv_idx(1).
    lv_right_b = lv_right_x+lv_idx(1).
    lv_left_i = lv_left_b.
    lv_right_i = lv_right_b.

    IF lv_left_i < lv_right_i.
      cv_result = -1.
      RETURN.
    ELSEIF lv_left_i > lv_right_i.
      cv_result = 1.
      RETURN.
    ENDIF.
  ENDDO.
ENDFORM.

FORM left_pad_zero USING    iv_value TYPE xstring
                            iv_length TYPE i
                   CHANGING cv_value TYPE xstring.
  DATA: lv_pad_len TYPE i,
        lv_pad_x   TYPE xstring.

  cv_value = iv_value.
  IF iv_length < 0.
    MESSAGE 'Invalid target length for big integer normalization.' TYPE 'E'.
  ENDIF.
  IF xstrlen( cv_value ) > iv_length.
    MESSAGE 'Big integer is longer than the target modulus length.' TYPE 'E'.
  ENDIF.

  lv_pad_len = iv_length - xstrlen( cv_value ).
  IF lv_pad_len > 0.
    PERFORM make_zero_block USING lv_pad_len CHANGING lv_pad_x.
    CONCATENATE lv_pad_x cv_value INTO cv_value IN BYTE MODE.
  ENDIF.
ENDFORM.

FORM make_zero_block USING    iv_length TYPE i
                     CHANGING cv_block TYPE xstring.
  DATA lv_zero TYPE x LENGTH 1 VALUE '00'.

  CLEAR cv_block.
  IF iv_length <= 0.
    RETURN.
  ENDIF.

  DO iv_length TIMES.
    CONCATENATE cv_block lv_zero INTO cv_block IN BYTE MODE.
  ENDDO.
ENDFORM.

FORM make_one_block USING    iv_length TYPE i
                    CHANGING cv_block TYPE xstring.
  DATA: lv_zero_len TYPE i,
        lv_zero_x   TYPE xstring,
        lv_one      TYPE x LENGTH 1 VALUE '01'.

  IF iv_length <= 0.
    CLEAR cv_block.
    RETURN.
  ENDIF.

  lv_zero_len = iv_length - 1.
  PERFORM make_zero_block USING lv_zero_len CHANGING lv_zero_x.
  CONCATENATE lv_zero_x lv_one INTO cv_block IN BYTE MODE.
ENDFORM.
