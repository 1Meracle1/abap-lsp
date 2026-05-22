package abap_frontend_parser

import "../ast"

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
case_when_missing_period_uses_local_invalid_statement :: proc(t: ^testing.T) {
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
	testing.expect_value(t, len(case_stmt.whens), 1)
	testing.expect_value(t, counts.assign, 2)
	testing.expect_value(t, counts.data_decl, 1)
	testing.expect(t, counts.invalid_stmt >= 1)
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
