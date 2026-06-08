package abap_frontend_parser

import "src:ast"

import "core:testing"

@(test)
print_node_uses_formatting_options :: proc(t: ^testing.T) {
	source := `IF a = 1. lv = 1. ENDIF.`
	parsed := parse(source, "format_options.abap", context.allocator)
	options := ast.Print_Options{newline = "\r\n", indent = "  "}

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator, options), "IF a = 1.\r\n  lv = 1.\r\nENDIF.")
}

@(test)
statement_batch_control_blocks :: proc(t: ^testing.T) {
	source := `IF a = 1. WRITE 'a'. ELSEIF a = 2. WRITE 'b'. ELSE. WRITE 'c'. ENDIF.
CASE a. WHEN 1 OR 2. WRITE 'n'. WHEN OTHERS. WRITE 'o'. ENDCASE.
WHILE a > 0. a = a - 1. ENDWHILE.
DO 3 TIMES. WRITE a. ENDDO.
LOOP AT itab INTO wa. AT FIRST. WRITE wa. ENDAT. ENDLOOP.
TRY. WRITE 'x'. CATCH cx_root INTO DATA(lo). WRITE 'y'. CLEANUP. WRITE 'z'. ENDTRY.`
	parsed := parse(source, "control.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.if_stmt, 1)
	testing.expect_value(t, counts.case_stmt, 1)
	testing.expect_value(t, counts.while_stmt, 1)
	testing.expect_value(t, counts.do_stmt, 1)
	testing.expect_value(t, counts.loop_stmt, 1)
	testing.expect_value(t, counts.at_stmt, 1)
	testing.expect_value(t, counts.try_stmt, 1)

	if_stmt := parsed.root.stmts[0].derived_stmt.(^ast.If_Stmt)
	case_stmt := parsed.root.stmts[1].derived_stmt.(^ast.Case_Stmt)
	try_stmt := parsed.root.stmts[5].derived_stmt.(^ast.Try_Stmt)
	testing.expect_value(t, len(if_stmt.elseif_clauses), 1)
	testing.expect(t, if_stmt.else_clause != nil)
	testing.expect_value(t, len(case_stmt.whens), 2)
	testing.expect_value(t, len(try_stmt.catches), 1)
	testing.expect(t, try_stmt.cleanup != nil)
}

@(test)
oop_and_form_names_are_limited_to_thirty_characters :: proc(t: ^testing.T) {
	source := `REPORT zlen_oop_30.

CLASS lcl_test DEFINITION.
  PUBLIC SECTION.
    DATA abcdefghijabcdefghijabcdefghija TYPE i.
    METHODS abcdefghijabcdefghijabcdefghijb
      IMPORTING abcdefghijabcdefghijabcdefghijc TYPE i.
ENDCLASS.

CLASS lcl_test IMPLEMENTATION.
  METHOD abcdefghijabcdefghijabcdefghijb.
  ENDMETHOD.
ENDCLASS.

FORM abcdefghijabcdefghijabcdefghijd
  USING abcdefghijabcdefghijabcdefghije TYPE i.
ENDFORM.`
	parsed := parse(source, "oop_name_length.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 6)
	expect_error_contains(t, parsed, "name can be up to 30 characters long")
}

@(test)
catch_system_exceptions_block_is_accepted :: proc(t: ^testing.T) {
	source := `CATCH SYSTEM-EXCEPTIONS conversion_errors = 0 data_access_errors = 0.
  WHILE x < 4.
    ADD 1 TO x.
  ENDWHILE.
ENDCATCH.`
	parsed := parse(source, "catch_system.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
}

@(test)
at_group_stmt_kinds_are_parser_modeled :: proc(t: ^testing.T) {
	source := `LOOP AT itab INTO wa.
  AT FIRST.
  ENDAT.
  AT LAST.
  ENDAT.
  AT NEW field.
  ENDAT.
  AT END OF field.
  ENDAT.
ENDLOOP.`
	parsed := parse(source, "at_groups.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	loop := parsed.root.stmts[0].derived_stmt.(^ast.Loop_Stmt)
	testing.expect_value(t, len(loop.body), 4)

	first := loop.body[0].derived_stmt.(^ast.At_Stmt)
	last := loop.body[1].derived_stmt.(^ast.At_Stmt)
	new_ := loop.body[2].derived_stmt.(^ast.At_Stmt)
	end_of := loop.body[3].derived_stmt.(^ast.At_Stmt)
	testing.expect_value(t, first.kind, ast.At_Stmt_Kind.First)
	testing.expect_value(t, last.kind, ast.At_Stmt_Kind.Last)
	testing.expect_value(t, new_.kind, ast.At_Stmt_Kind.New)
	testing.expect_value(t, end_of.kind, ast.At_Stmt_Kind.End_Of)
	testing.expect(t, first.expr == nil)
	testing.expect(t, last.expr == nil)
	testing.expect_value(t, new_.field_name, "field")
	testing.expect_value(t, end_of.field_name, "field")
}

@(test)
statement_batch_structural_blocks :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION. ENDCLASS.
INTERFACE lif. ENDINTERFACE.
CLASS lcl IMPLEMENTATION. METHOD run. DATA lv TYPE i. ENDMETHOD. ENDCLASS.
FORM frm. WRITE 'f'. ENDFORM.
FUNCTION z_fm. WRITE 'x'. ENDFUNCTION.
MODULE pai INPUT. WRITE 'm'. ENDMODULE.
ENHANCEMENT enh. WRITE 'e'. ENDENHANCEMENT.
TEST-SEAM seam. WRITE 's'. END-TEST-SEAM.
TEST-INJECTION seam. WRITE 'i'. END-TEST-INJECTION.
START-OF-SELECTION. WRITE 'start'.`
	parsed := parse(source, "structural.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.class_decl, 2)
	testing.expect_value(t, counts.interface_decl, 1)
	testing.expect_value(t, counts.method_decl, 1)
	testing.expect_value(t, counts.form_decl, 1)
	testing.expect_value(t, counts.function_decl, 1)
	testing.expect_value(t, counts.module_decl, 1)
	testing.expect_value(t, counts.enhancement, 1)
	testing.expect_value(t, counts.test_seam, 1)
	testing.expect_value(t, counts.test_injection, 1)
	testing.expect_value(t, counts.event_block, 1)
}

@(test)
structural_keyword_variable_assignment_stays_in_current_block :: proc(t: ^testing.T) {
	source := `FUNCTION z_fm.
  IF suppress_corr_check IS INITIAL.
    function = funcname.
  ENDIF.
ENDFUNCTION.`
	parsed := parse(source, "keyword_assignment.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.function_decl, 1)
	testing.expect_value(t, counts.if_stmt, 1)
	testing.expect_value(t, counts.assign, 1)
}

@(test)
form_and_function_header_parameters_are_ast_fields :: proc(t: ^testing.T) {
	source := `FORM run TABLES !ct_rows STRUCTURE mara USING VALUE(iv_text) TYPE string REFERENCE(iv_ref) LIKE sy-uname CHANGING cv_count TYPE i.
ENDFORM.
FUNCTION z_demo
  IMPORTING VALUE(iv_value) TYPE i OPTIONAL iv_text TYPE string DEFAULT 'x'
  EXPORTING ev_text LIKE sy-uname
  CHANGING REFERENCE(cv_any) TYPE REF TO object
  TABLES et_return STRUCTURE bapiret2
  EXCEPTIONS failed = 1 not_found.
ENDFUNCTION.`
	parsed := parse(source, "routine_parameters.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	form := parsed.root.stmts[0].derived_stmt.(^ast.Form_Decl)
	testing.expect_value(t, len(form.form_parameters), 4)
	testing.expect_value(t, form.form_parameters[0].section, ast.Form_Parameter_Section.Tables)
	testing.expect_value(t, form.form_parameters[0].passing, ast.Parameter_Passing_Kind.Direct)
	testing.expect_value(t, form.form_parameters[0].type_clause.form, ast.Data_Type_Form.Structure)
	testing.expect_value(t, source[form.form_parameters[0].range.start:form.form_parameters[0].range.end], "ct_rows")
	testing.expect_value(t, form.form_parameters[1].name, "iv_text")
	testing.expect_value(t, form.form_parameters[1].passing, ast.Parameter_Passing_Kind.Value)
	testing.expect_value(t, form.form_parameters[2].name, "iv_ref")
	testing.expect_value(t, form.form_parameters[2].passing, ast.Parameter_Passing_Kind.Reference)
	form_ref := form.form_parameters[2].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, form_ref.base_name, "sy")
	testing.expect_value(t, form_ref.path[0].name, "uname")
	testing.expect_value(t, form.form_parameters[3].section, ast.Form_Parameter_Section.Changing)

	function := parsed.root.stmts[1].derived_stmt.(^ast.Function_Decl)
	testing.expect_value(t, len(function.function_parameters), 5)
	testing.expect_value(t, function.function_parameters[0].name, "iv_value")
	testing.expect(t, .Is_Optional in function.function_parameters[0].flags)
	testing.expect_value(t, function.function_parameters[1].name, "iv_text")
	testing.expect(t, .Has_Default_Value in function.function_parameters[1].flags)
	testing.expect_value(t, function.function_parameters[2].section, ast.Function_Parameter_Section.Exporting)
	function_ref := function.function_parameters[2].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, function_ref.base_name, "sy")
	testing.expect_value(t, function_ref.path[0].name, "uname")
	testing.expect_value(t, function.function_parameters[3].passing, ast.Parameter_Passing_Kind.Reference)
	testing.expect_value(t, function.function_parameters[3].type_clause.form, ast.Data_Type_Form.Ref_To)
	testing.expect_value(t, function.function_parameters[4].type_clause.form, ast.Data_Type_Form.Structure)
	testing.expect_value(t, len(function.exceptions), 2)
	testing.expect_value(t, function.exceptions[0].name, "failed")
	testing.expect_value(t, function.exceptions[1].name, "not_found")
}

@(test)
form_tables_structure_keeps_following_untyped_parameter :: proc(t: ^testing.T) {
	source := `FORM get_non_deleted_objects TABLES resulttab STRUCTURE ddsymtab
                                    rangetab
                             USING par1 par2.
ENDFORM.`
	parsed := parse(source, "form_tables_structure.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	form := parsed.root.stmts[0].derived_stmt.(^ast.Form_Decl)
	testing.expect_value(t, len(form.form_parameters), 4)
	testing.expect_value(t, form.form_parameters[0].name, "resulttab")
	testing.expect_value(t, form.form_parameters[0].section, ast.Form_Parameter_Section.Tables)
	testing.expect_value(t, form.form_parameters[0].type_clause.form, ast.Data_Type_Form.Structure)
	ref := form.form_parameters[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, ref.base_name, "ddsymtab")
	testing.expect_value(t, form.form_parameters[1].name, "rangetab")
	testing.expect_value(t, form.form_parameters[1].section, ast.Form_Parameter_Section.Tables)
	testing.expect(t, form.form_parameters[1].type_clause == nil)
	testing.expect_value(t, form.form_parameters[2].section, ast.Form_Parameter_Section.Using)
	testing.expect_value(t, form.form_parameters[3].name, "par2")
}

@(test)
form_typed_parameter_keeps_following_untyped_parameter :: proc(t: ^testing.T) {
	source := `FORM masdel_exec USING testdel protocol cnt TYPE i prid.
ENDFORM.`
	parsed := parse(source, "form_typed_then_untyped.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	form := parsed.root.stmts[0].derived_stmt.(^ast.Form_Decl)
	testing.expect_value(t, len(form.form_parameters), 4)
	testing.expect_value(t, form.form_parameters[2].name, "cnt")
	cnt_ref := form.form_parameters[2].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, cnt_ref.base_name, "i")
	testing.expect_value(t, form.form_parameters[3].name, "prid")
	testing.expect(t, form.form_parameters[3].type_clause == nil)
}

@(test)
form_tables_structure_requires_name :: proc(t: ^testing.T) {
	source := `FORM bad TABLES rows STRUCTURE USING par1.
ENDFORM.`
	parsed := parse(source, "form_tables_structure_missing.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 1)
	testing.expect_value(t, parsed.errors[0].message, "syntax error: expected structure name")
	form := parsed.root.stmts[0].derived_stmt.(^ast.Form_Decl)
	testing.expect_value(t, len(form.form_parameters), 2)
	testing.expect_value(t, form.form_parameters[1].section, ast.Form_Parameter_Section.Using)
	testing.expect_value(t, form.form_parameters[1].name, "par1")
}

@(test)
form_raising_clause_is_not_a_parameter :: proc(t: ^testing.T) {
	source := `FORM open_gui USING iv_value TYPE string RAISING zcx_abapgit_exception.
ENDFORM.`
	parsed := parse(source, "form_raising.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	form := parsed.root.stmts[0].derived_stmt.(^ast.Form_Decl)
	testing.expect_value(t, len(form.form_parameters), 1)
	testing.expect_value(t, form.form_parameters[0].name, "iv_value")
}

@(test)
routine_headers_accept_escaped_keyword_parameters :: proc(t: ^testing.T) {
	source := `FUNCTION z_keywords
  IMPORTING !VALUE TYPE i !REFERENCE TYPE string.
ENDFUNCTION.`
	parsed := parse(source, "routine_escaped_keyword_parameters.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	function := parsed.root.stmts[0].derived_stmt.(^ast.Function_Decl)
	testing.expect_value(t, len(function.function_parameters), 2)
	testing.expect_value(t, function.function_parameters[0].name, "VALUE")
	testing.expect_value(t, function.function_parameters[0].passing, ast.Parameter_Passing_Kind.Direct)
	testing.expect_value(t, function.function_parameters[1].name, "REFERENCE")
	testing.expect_value(t, function.function_parameters[1].passing, ast.Parameter_Passing_Kind.Direct)
}

@(test)
function_tables_like_parameter_keeps_optional_addition :: proc(t: ^testing.T) {
	source := `FUNCTION rh_read_object
  TABLES
    EXISTENCE LIKE HROEXIST OPTIONAL
  EXCEPTIONS
    NOT_FOUND.
ENDFUNCTION.`
	parsed := parse(source, "function_tables_like.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	function := parsed.root.stmts[0].derived_stmt.(^ast.Function_Decl)
	testing.expect_value(t, len(function.function_parameters), 1)
	param := function.function_parameters[0]
	testing.expect_value(t, param.section, ast.Function_Parameter_Section.Tables)
	testing.expect_value(t, param.name, "EXISTENCE")
	testing.expect_value(t, param.type_clause.form, ast.Data_Type_Form.Like)
	testing.expect(t, .Is_Optional in param.flags)
	ref := param.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, ref.base_name, "HROEXIST")
	testing.expect_value(t, len(function.exceptions), 1)
	testing.expect_value(t, function.exceptions[0].name, "NOT_FOUND")
}

@(test)
multiline_class_headers_keep_their_create_addition :: proc(t: ^testing.T) {
	source := `CLASS zcx_error DEFINITION
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.
  PUBLIC SECTION.
ENDCLASS.`
	parsed := parse(source, "class_header.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.class_decl, 1)
	testing.expect_value(t, counts.oop_simple, 1)
}

@(test)
class_header_facts_are_ast_fields :: proc(t: ^testing.T) {
	source := `CLASS lcl_abs DEFINITION ABSTRACT.
ENDCLASS.
CLASS lcl_child DEFINITION INHERITING FROM lcl_super.
ENDCLASS.
CLASS lcl_impl IMPLEMENTATION.
ENDCLASS.
CLASS lcl_deferred DEFINITION DEFERRED.`
	parsed := parse(source, "class_header_facts.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, len(parsed.root.stmts), 4)

	abs := parsed.root.stmts[0].derived_stmt.(^ast.Class_Decl)
	child := parsed.root.stmts[1].derived_stmt.(^ast.Class_Decl)
	impl := parsed.root.stmts[2].derived_stmt.(^ast.Class_Decl)
	deferred := parsed.root.stmts[3].derived_stmt.(^ast.Class_Decl)

	testing.expect(t, .Abstract in abs.flags)
	testing.expect(t, !(.Implementation in abs.flags))
	testing.expect(t, !(.Bodyless in abs.flags))
	testing.expect_value(t, child.superclass_name, "lcl_super")
	testing.expect_value(
		t,
		source[child.superclass_range.start:child.superclass_range.end],
		"lcl_super",
	)
	testing.expect(t, .Implementation in impl.flags)
	testing.expect(t, !(.Bodyless in impl.flags))
	testing.expect(t, .Bodyless in deferred.flags)
	testing.expect_value(t, len(deferred.body), 0)
}

@(test)
oop_load_statements_are_not_class_or_interface_declarations :: proc(t: ^testing.T) {
	source := `INTERFACE if_demo.
  CLASS cl_gui_column_tree DEFINITION LOAD.
ENDINTERFACE.
INTERFACE if_sxml LOAD.`
	parsed := parse(source, "oop_load.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.class_decl, 0)
	testing.expect_value(t, len(parsed.root.stmts), 2)

	iface := parsed.root.stmts[0].derived_stmt.(^ast.Interface_Decl)
	class_load := iface.body[0].derived_stmt.(^ast.Oop_Load_Stmt)
	interface_load := parsed.root.stmts[1].derived_stmt.(^ast.Oop_Load_Stmt)
	testing.expect_value(t, class_load.kind, ast.Oop_Load_Kind.Class)
	testing.expect_value(t, class_load.name, "cl_gui_column_tree")
	testing.expect_value(t, interface_load.kind, ast.Oop_Load_Kind.Interface)
	testing.expect_value(t, interface_load.name, "if_sxml")
}

@(test)
class_header_friends_are_ast_fields :: proc(t: ^testing.T) {
	source := `CLASS lcl_target DEFINITION
  CREATE PRIVATE
  FRIENDS lcl_friend zcl_global.
ENDCLASS.`
	parsed := parse(source, "class_header_friends.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, len(parsed.root.stmts), 1)

	class := parsed.root.stmts[0].derived_stmt.(^ast.Class_Decl)
	testing.expect_value(t, len(class.friends), 2)
	testing.expect_value(t, class.friends[0].name, "lcl_friend")
	testing.expect_value(t, class.friends[1].name, "zcl_global")
	testing.expect_value(
		t,
		source[class.friends[0].range.start:class.friends[0].range.end],
		"lcl_friend",
	)
}

@(test)
empty_control_flow_headers_report_specific_errors :: proc(t: ^testing.T) {
	source := `IF . ENDIF.
IF ok = abap_true. ELSEIF . ENDIF.
WHILE . ENDWHILE.
CASE . ENDCASE.
CASE lv. WHEN . ENDCASE.
TRY. CATCH . ENDTRY.`
	parsed := parse(source, "empty_headers.abap", context.allocator)

	expect_error_contains(t, parsed, "expected condition after IF")
	expect_error_contains(t, parsed, "expected condition after ELSEIF")
	expect_error_contains(t, parsed, "expected condition after WHILE")
	expect_error_contains(t, parsed, "expected expression after CASE")
	expect_error_contains(t, parsed, "expected expression after WHEN")
	expect_error_contains(t, parsed, "expected exception class after CATCH")
}

@(test)
loop_at_requires_source_targets_and_clause_expressions :: proc(t: ^testing.T) {
	source := `LOOP AT . ENDLOOP.
LOOP AT itab INTO . ENDLOOP.
LOOP AT itab WHERE . ENDLOOP.`
	parsed := parse(source, "loop_negative.abap", context.allocator)
	counts := count_nodes(parsed.root)

	expect_error_contains(t, parsed, "expected loop source after LOOP AT")
	expect_error_contains(t, parsed, "expected target after INTO")
	expect_error_contains(t, parsed, "expected expression after WHERE")
	testing.expect_value(t, counts.loop_stmt, 0)
	testing.expect(t, counts.invalid_stmt >= 3)
}

@(test)
unmatched_delimiters_do_not_hide_following_statement_boundaries :: proc(t: ^testing.T) {
	source := `lv_value = foo ).
lv_other = foo )
DATA lv_after TYPE i.`
	parsed := parse(source, "delimiters.abap", context.allocator)
	counts := count_nodes(parsed.root)

	expect_error_contains(t, parsed, "unmatched closing ')'")
	testing.expect(t, counts.invalid_stmt >= 2)
	testing.expect_value(t, counts.data_decl, 1)
}

@(test)
if_and_while_header_unmatched_delimiters_fail_the_statement :: proc(t: ^testing.T) {
	parsed := parse(
		"IF lv_flag ). ENDIF.\nWHILE lv_flag ]. ENDWHILE.",
		"header_delimiters.abap",
		context.allocator,
	)
	counts := count_nodes(parsed.root)

	expect_error_contains(t, parsed, "expected '.' after IF condition")
	expect_error_contains(t, parsed, "unexpected ENDIF without matching IF")
	expect_error_contains(t, parsed, "expected '.' after WHILE condition")
	expect_error_contains(t, parsed, "unexpected ENDWHILE without matching WHILE")
	testing.expect_value(t, counts.if_stmt, 0)
	testing.expect_value(t, counts.while_stmt, 0)
	testing.expect(t, counts.invalid_stmt >= 2)
}

@(test)
case_bad_when_header_does_not_scan_case_body :: proc(t: ^testing.T) {
	parsed := parse(
		"CASE lv_kind. WHEN = 1. DATA lv_inside TYPE i. ENDCASE. DATA lv_after TYPE i.",
		"bad_when.abap",
		context.allocator,
	)
	counts := count_nodes(parsed.root)
	case_stmt := parsed.root.stmts[0].derived_stmt.(^ast.Case_Stmt)

	expect_error_contains(t, parsed, "expected expression after WHEN")
	expect_no_error_contains(t, parsed, "unexpected ENDCASE without matching CASE")
	testing.expect_value(t, counts.case_stmt, 1)
	testing.expect_value(t, len(case_stmt.whens), 0)
	testing.expect_value(t, counts.data_decl, 2)
	testing.expect(t, counts.invalid_stmt >= 1)
}

@(test)
case_when_missing_period_keeps_clause :: proc(t: ^testing.T) {
	source := `CASE lv_kind.
  WHEN 'A'
    lv_a = 1.
  WHEN 'B'.
    lv_b = 2.
ENDCASE.
DATA lv_after TYPE i.`
	parsed := parse(source, "when_period.abap", context.allocator)
	counts := count_nodes(parsed.root)
	case_stmt := parsed.root.stmts[0].derived_stmt.(^ast.Case_Stmt)

	expect_error_contains(t, parsed, "expected '.'")
	expect_no_error_contains(t, parsed, "unexpected WHEN without matching CASE")
	expect_no_error_contains(t, parsed, "unexpected ENDCASE without matching CASE")
	testing.expect_value(t, counts.case_stmt, 1)
	testing.expect_value(t, len(case_stmt.whens), 2)
	testing.expect_value(t, counts.assign, 2)
	testing.expect_value(t, counts.data_decl, 1)
}

@(test)
missing_control_header_periods_keep_matching_boundaries :: proc(t: ^testing.T) {
	source := `IF flag = abap_true
  RETURN.
ELSE
  RETURN.
ENDIF.
CASE kind
  WHEN 'A'
    value = 1.
  WHEN OTHERS
ENDCASE.
WHILE flag = abap_true
ENDWHILE.
DO
ENDDO.
DO 2 TIMES
ENDDO.
LOOP AT itab INTO wa
  AT FIRST
  ENDAT.
ENDLOOP.
TRY
  RETURN.
CATCH cx_root
  RETURN.
CLEANUP
ENDTRY.`
	parsed := parse(source, "missing_control_periods.abap", context.allocator)
	counts := count_nodes(parsed.root)

	expect_error_contains(t, parsed, "expected '.' after IF condition")
	expect_error_contains(t, parsed, "expected '.' after ELSE")
	expect_error_contains(t, parsed, "expected '.' after CASE")
	expect_error_contains(t, parsed, "expected '.' after WHEN")
	expect_error_contains(t, parsed, "expected '.' after WHILE condition")
	expect_error_contains(t, parsed, "expected '.' after DO")
	expect_error_contains(t, parsed, "expected '.' after LOOP")
	expect_error_contains(t, parsed, "expected '.' after AT")
	expect_error_contains(t, parsed, "expected '.' after TRY")
	expect_error_contains(t, parsed, "expected '.' after CATCH clause")
	expect_error_contains(t, parsed, "expected '.' after CLEANUP")
	expect_no_error_contains(t, parsed, "unexpected")
	testing.expect_value(t, counts.if_stmt, 1)
	testing.expect_value(t, counts.case_stmt, 1)
	testing.expect_value(t, counts.while_stmt, 1)
	testing.expect_value(t, counts.do_stmt, 2)
	testing.expect_value(t, counts.loop_stmt, 1)
	testing.expect_value(t, counts.at_stmt, 1)
	testing.expect_value(t, counts.try_stmt, 1)
}

@(test)
case_when_accepts_selector_operands :: proc(t: ^testing.T) {
	source := `CASE cs_itf-tdline.
  WHEN c_section_token-cause.
    WRITE 'cause'.
  WHEN zif_abapgit_definitions=>c_action-go_home.
    WRITE 'home'.
ENDCASE.`
	parsed := parse(source, "when_selectors.abap", context.allocator)
	case_stmt := parsed.root.stmts[0].derived_stmt.(^ast.Case_Stmt)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, len(case_stmt.whens), 2)
}

@(test)
target_condition_operators_do_not_break_blocks :: proc(t: ^testing.T) {
	source := `IF lv_langu NOT IN lt_language_filter.
ENDIF.
WHILE lv_byte BIT-AND lc_msb <> lc_zero.
ENDWHILE.
IF ls_tstc-cinfo O lc_hex_rep AND ls_tstc-cinfo Z lc_hex_obj.
ENDIF.`
	parsed := parse(source, "target_conditions.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.if_stmt, 2)
	testing.expect_value(t, counts.while_stmt, 1)
}

@(test)
missing_inner_control_end_does_not_consume_structural_boundary :: proc(t: ^testing.T) {
	source := `CLASS lcl IMPLEMENTATION.
  METHOD run.
    IF flag = abap_true.
  ENDMETHOD.
ENDCLASS.`
	parsed := parse(source, "missing_inner_end.abap", context.allocator)
	counts := count_nodes(parsed.root)

	expect_error_contains(t, parsed, "expected ENDIF")
	expect_no_error_contains(t, parsed, "unexpected ENDMETHOD without matching METHOD")
	expect_no_error_contains(t, parsed, "unexpected ENDCLASS without matching CLASS")
	testing.expect_value(t, counts.class_decl, 1)
	testing.expect_value(t, counts.method_decl, 1)
	testing.expect(t, counts.invalid_stmt >= 1)
}

@(test)
print_node_retains_structural_and_loop_headers :: proc(t: ^testing.T) {
	source := `CLASS lcl DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
ENDCLASS.
LOOP AT itab ASSIGNING <row> WHERE flag = abap_true.
ENDLOOP.`
	parsed := parse(source, "header_print.abap", context.allocator)
	text := ast.print_node(parsed.root, context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(
		t,
		text,
		"CLASS lcl DEFINITION FINAL CREATE PUBLIC.\n    PUBLIC SECTION.\nENDCLASS.\nLOOP AT itab ASSIGNING <row> WHERE flag = abap_true.\nENDLOOP.",
	)
}

@(test)
loop_header_keeps_target_key_bounds_and_where :: proc(t: ^testing.T) {
	source := `LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<row>) FROM lv_from TO lv_to USING KEY (lv_key) WHERE id = lv_id.
ENDLOOP.`
	parsed := parse(source, "loop_header_shape.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	loop := parsed.root.stmts[0].derived_stmt.(^ast.Loop_Stmt)
	testing.expect_value(t, loop.target_kind, ast.Loop_Target_Kind.Assigning)
	testing.expect(t, loop.target != nil)
	testing.expect(t, loop.from != nil)
	testing.expect(t, loop.to != nil)
	testing.expect(t, loop.using_key.dynamic_name != nil)
	testing.expect(t, loop.where_cond != nil)
}

@(test)
loop_header_allows_group_by_targets :: proc(t: ^testing.T) {
	source := `LOOP AT lt_hu_por ASSIGNING FIELD-SYMBOL(<fs_hu_por>)
  GROUP BY ( unique_id = <fs_hu_por>-unique_id ) ASCENDING
  ASSIGNING FIELD-SYMBOL(<fs_group>).
ENDLOOP.
LOOP AT lt_serno ASSIGNING FIELD-SYMBOL(<lfs_serno>)
  GROUP BY ( date = <lfs_serno>-date size = GROUP SIZE )
  INTO DATA(ls_serno_grp).
ENDLOOP.
LOOP AT lt_rows INTO DATA(ls_row)
  GROUP BY ls_row-id WITHOUT MEMBERS.
ENDLOOP.`
	parsed := parse(source, "loop_group_by.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, len(parsed.root.stmts), 3)
	first := parsed.root.stmts[0].derived_stmt.(^ast.Loop_Stmt)
	testing.expect_value(t, first.target_kind, ast.Loop_Target_Kind.Assigning)
	testing.expect(t, first.target != nil)
	testing.expect(t, first.group_by != nil)
	testing.expect_value(t, source[first.group_by.range.start:first.group_by.range.end], "( unique_id = <fs_hu_por>-unique_id )")
	testing.expect_value(t, first.group_order, ast.Loop_Group_Order.Ascending)
	testing.expect_value(t, first.group_target_kind, ast.Loop_Target_Kind.Assigning)
	testing.expect(t, first.group_target != nil)

	second := parsed.root.stmts[1].derived_stmt.(^ast.Loop_Stmt)
	testing.expect(t, second.group_by != nil)
	testing.expect_value(t, source[second.group_by.range.start:second.group_by.range.end], "( date = <lfs_serno>-date size = GROUP SIZE )")
	testing.expect_value(t, second.group_target_kind, ast.Loop_Target_Kind.Into)
	testing.expect(t, second.group_target != nil)

	third := parsed.root.stmts[2].derived_stmt.(^ast.Loop_Stmt)
	testing.expect(t, third.group_by != nil)
	testing.expect_value(t, source[third.group_by.range.start:third.group_by.range.end], "ls_row-id")
	testing.expect(t, third.group_without_members)
	testing.expect_value(t, source[third.group_without_members_range.start:third.group_without_members_range.end], "WITHOUT MEMBERS")
}

@(test)
loop_at_group_allows_member_iteration :: proc(t: ^testing.T) {
	source := `LOOP AT GROUP <fs_group> ASSIGNING FIELD-SYMBOL(<fs_group_mem>).
  lt_ids = VALUE #( BASE lt_ids ( <fs_group_mem> ) ).
ENDLOOP.`
	parsed := parse(source, "loop_at_group.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	loop := parsed.root.stmts[0].derived_stmt.(^ast.Loop_Stmt)
	testing.expect(t, loop.source != nil)
	testing.expect_value(t, loop.target_kind, ast.Loop_Target_Kind.Assigning)
}

@(test)
loop_header_requires_assignment_target :: proc(t: ^testing.T) {
	source := `LOOP AT lt_rows ASSIGNING.
ENDLOOP.`
	parsed := parse(source, "loop_header_missing_target.abap", context.allocator)

	expect_error_contains(t, parsed, "expected target after ASSIGNING")
}

@(test)
loop_header_allows_where_pragmas_with_arguments :: proc(t: ^testing.T) {
	source := `LOOP AT lt_rows ASSIGNING <row> WHERE id IS INITIAL ##PRIMKEY[SEC_KEY].
ENDLOOP.`
	parsed := parse(source, "loop_header_pragma.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
}

@(test)
loop_header_pragma_argument_does_not_extend_where_expr :: proc(t: ^testing.T) {
	source := `LOOP AT lt_rows ASSIGNING <row> WHERE path = lv_path AND ( filename CP lv_a OR filename CP lv_b ) ##PRIMKEY[FILE_PATH].
ENDLOOP.`
	parsed := parse(source, "loop_header_pragma_arg_bounds.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	loop := parsed.root.stmts[0].derived_stmt.(^ast.Loop_Stmt)
	and_expr, and_ok := loop.where_cond.derived_expr.(^ast.Binary_Expr)
	testing.expect(t, and_ok)
	if and_ok {
		_, pragma_arg_as_table_expr := and_expr.right.derived_expr.(^ast.Table_Expr)
		testing.expect(t, !pragma_arg_as_table_expr)
	}
}

@(test)
amdp_method_body_is_retained_as_sqlscript_island :: proc(t: ^testing.T) {
	source := `CLASS lcl IMPLEMENTATION.
  METHOD select_rows BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT OPTIONS READ-ONLY USING mara.
    lt_rows = SELECT matnr FROM mara;
  ENDMETHOD.
ENDCLASS.`
	parsed := parse(source, "amdp_method.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.class_decl, 1)
	testing.expect_value(t, counts.method_decl, 1)
	method := parsed.root.stmts[0].derived_stmt.(^ast.Class_Decl).body[0].derived_stmt.(^ast.Method_Decl)
	testing.expect(t, method.is_amdp)
	testing.expect(t, method.amdp_body != "")
	testing.expect_value(t, len(method.body), 0)
}

@(test)
kernel_method_header_retains_modules_and_requires_empty_body :: proc(t: ^testing.T) {
	source := `CLASS lcl IMPLEMENTATION.
  METHOD run BY KERNEL MODULE abkm_Run IGNORE.
  ENDMETHOD.
ENDCLASS.`
	parsed := parse(source, "kernel_method.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	method := parsed.root.stmts[0].derived_stmt.(^ast.Class_Decl).body[0].derived_stmt.(^ast.Method_Decl)
	testing.expect(t, method.is_kernel)
	testing.expect_value(t, len(method.kernel_modules), 2)
	testing.expect_value(t, method.kernel_modules[0], "abkm_Run")
	testing.expect_value(t, method.kernel_modules[1], "IGNORE")

	with_body := parse(
		`CLASS lcl IMPLEMENTATION.
  METHOD run BY KERNEL MODULE abkm_Run.
    DATA lv_value TYPE i.
  ENDMETHOD.
ENDCLASS.`,
		"kernel_method_body.abap",
		context.allocator,
	)
	testing.expect_value(t, len(with_body.errors), 1)
	testing.expect_value(t, with_body.errors[0].message, "syntax error: kernel method implementation must be empty")
}

@(test)
method_block_keeps_interface_qualified_name :: proc(t: ^testing.T) {
	source := `CLASS lcl IMPLEMENTATION.
  METHOD if_demo~run.
  ENDMETHOD.
ENDCLASS.`
	parsed := parse(source, "qualified_method_block.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	method := parsed.root.stmts[0].derived_stmt.(^ast.Class_Decl).body[0].derived_stmt.(^ast.Method_Decl)
	testing.expect_value(t, method.name, "if_demo~run")
}
