REPORT zsimple_ast.

CLASS zcl_ast_node DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS to_string
      RETURNING VALUE(rv_text) TYPE string
      ABSTRACT.
ENDCLASS.

CLASS zcl_ast_node IMPLEMENTATION.
ENDCLASS.


CLASS zcl_expr DEFINITION ABSTRACT INHERITING FROM zcl_ast_node.
  PUBLIC SECTION.
ENDCLASS.

CLASS zcl_expr IMPLEMENTATION.
ENDCLASS.


CLASS zcl_stmt DEFINITION ABSTRACT INHERITING FROM zcl_ast_node.
  PUBLIC SECTION.
ENDCLASS.

CLASS zcl_stmt IMPLEMENTATION.
ENDCLASS.


CLASS zcl_number_literal DEFINITION INHERITING FROM zcl_expr.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_value TYPE string.
    METHODS to_string REDEFINITION.
  PRIVATE SECTION.
    DATA mv_value TYPE string.
ENDCLASS.

CLASS zcl_number_literal IMPLEMENTATION.
  METHOD constructor.
    mv_value = iv_value.
  ENDMETHOD.

  METHOD to_string.
    rv_text = mv_value.
  ENDMETHOD.
ENDCLASS.


CLASS zcl_identifier DEFINITION INHERITING FROM zcl_expr.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_name TYPE string.
    METHODS to_string REDEFINITION.
  PRIVATE SECTION.
    DATA mv_name TYPE string.
ENDCLASS.

CLASS zcl_identifier IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_name.
  ENDMETHOD.

  METHOD to_string.
    rv_text = mv_name.
  ENDMETHOD.
ENDCLASS.


CLASS zcl_binary_expr DEFINITION INHERITING FROM zcl_expr.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_left  TYPE REF TO zcl_expr
        iv_op    TYPE string
        io_right TYPE REF TO zcl_expr.
    METHODS to_string REDEFINITION.
  PRIVATE SECTION.
    DATA mo_left  TYPE REF TO zcl_expr.
    DATA mv_op    TYPE string.
    DATA mo_right TYPE REF TO zcl_expr.
ENDCLASS.

CLASS zcl_binary_expr IMPLEMENTATION.
  METHOD constructor.
    mo_left  = io_left.
    mv_op    = iv_op.
    mo_right = io_right.
  ENDMETHOD.

  METHOD to_string.
    rv_text = |({ mo_left->to_string( ) } { mv_op } { mo_right->to_string( ) })|.
  ENDMETHOD.
ENDCLASS.


CLASS zcl_assign_stmt DEFINITION INHERITING FROM zcl_stmt.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_name TYPE string
        io_expr TYPE REF TO zcl_expr.
    METHODS to_string REDEFINITION.
  PRIVATE SECTION.
    DATA mv_name TYPE string.
    DATA mo_expr TYPE REF TO zcl_expr.
ENDCLASS.

CLASS zcl_assign_stmt IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_name.
    mo_expr = io_expr.
  ENDMETHOD.

  METHOD to_string.
    rv_text = |{ mv_name } = { mo_expr->to_string( ) };|.
  ENDMETHOD.
ENDCLASS.


CLASS zcl_print_stmt DEFINITION INHERITING FROM zcl_stmt.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_expr TYPE REF TO zcl_expr.
    METHODS to_string REDEFINITION.
  PRIVATE SECTION.
    DATA mo_expr TYPE REF TO zcl_expr.
ENDCLASS.

CLASS zcl_print_stmt IMPLEMENTATION.
  METHOD constructor.
    mo_expr = io_expr.
  ENDMETHOD.

  METHOD to_string.
    rv_text = |print { mo_expr->to_string( ) };|.
  ENDMETHOD.
ENDCLASS.


CLASS zcl_program DEFINITION INHERITING FROM zcl_ast_node.
  PUBLIC SECTION.
    TYPES:
      ty_stmt_tab TYPE STANDARD TABLE OF REF TO zcl_stmt WITH DEFAULT KEY.

    METHODS add_statement
      IMPORTING io_stmt TYPE REF TO zcl_stmt.
    METHODS to_string REDEFINITION.
  PRIVATE SECTION.
    DATA mt_statements TYPE ty_stmt_tab.
ENDCLASS.

CLASS zcl_program IMPLEMENTATION.
  METHOD add_statement.
    APPEND io_stmt TO mt_statements.
  ENDMETHOD.

  METHOD to_string.
    DATA lo_stmt TYPE REF TO zcl_stmt.

    LOOP AT mt_statements INTO lo_stmt.
      IF rv_text IS NOT INITIAL.
        rv_text = rv_text && cl_abap_char_utilities=>newline.
      ENDIF.
      rv_text = rv_text && lo_stmt->to_string( ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.


START-OF-SELECTION.
  DATA(lo_prog) = NEW zcl_program( ).

  DATA(lo_expr1) = NEW zcl_binary_expr(
    io_left  = NEW zcl_number_literal( iv_value = '1' )
    iv_op    = '+'
    io_right = NEW zcl_number_literal( iv_value = '2' )
  ).

  DATA(lo_assign) = NEW zcl_assign_stmt(
    iv_name = 'x'
    io_expr = lo_expr1
  ).

  DATA(lo_expr2) = NEW zcl_binary_expr(
    io_left  = NEW zcl_identifier( iv_name = 'x' )
    iv_op    = '*'
    io_right = NEW zcl_number_literal( iv_value = '3' )
  ).

  DATA(lo_print) = NEW zcl_print_stmt(
    io_expr = lo_expr2
  ).

  lo_prog->add_statement( lo_assign ).
  lo_prog->add_statement( lo_print ).

  WRITE / lo_prog->to_string( ).