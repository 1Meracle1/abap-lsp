package abap_frontend_semantic2

import "src:ast"

import "core:testing"

@(test)
line_operands_follow_readable_and_writable_directions :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA lv_line TYPE i.
DATA lv_index TYPE i.
DATA lv_field TYPE string.
DATA lv_target TYPE string.
DATA lv_source TYPE string.
DATA lv_whole TYPE string.
READ LINE lv_line INDEX lv_index INTO lv_whole FIELD VALUE lv_target FIELD VALUE lv_field INTO lv_target.
MODIFY LINE lv_line INDEX lv_index FIELD VALUE lv_field FIELD VALUE lv_field FROM lv_source.`
	checker, file := checker_test_check_source(t, &project, source, "mem://line_direction.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	read := file.root.stmts[6].derived_stmt.(^ast.Line_Stmt)
	modify := file.root.stmts[7].derived_stmt.(^ast.Line_Stmt)
	readable := [?]^ast.Expr {
		read.line,
		read.index,
		read.fields[1].field,
		modify.line,
		modify.index,
		modify.fields[0].field,
		modify.fields[1].field,
		modify.fields[1].target,
	}
	for expr in readable {
		info, ok := checker_test_expr_info_for_node(t, &checker, &expr.expr_base)
		testing.expect(t, ok)
		if ok {
			testing.expect(t, !info.is_lhs)
		}
	}
	writable := [?]^ast.Expr {read.into, read.fields[0].field, read.fields[1].target}
	for expr in writable {
		info, ok := checker_test_expr_info_for_node(t, &checker, &expr.expr_base)
		testing.expect(t, ok)
		if ok {
			testing.expect(t, info.is_lhs)
		}
	}
}

@(test)
read_line_supports_inline_targets :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `READ LINE 1 INTO DATA(lv_line) FIELD VALUE sy-index INTO DATA(lv_value).`
	checker, file := checker_test_check_source(t, &project, source, "mem://line_inline.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	testing.expect(t, checker_test_lookup(t, &project, file.root_scope, .Value, "lv_line", .Variable) != nil)
	testing.expect(t, checker_test_lookup(t, &project, file.root_scope, .Value, "lv_value", .Variable) != nil)
}

@(test)
line_operands_report_unresolved_values_and_reject_constant_read_targets :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `CONSTANTS constant TYPE string VALUE ''.
READ LINE missing_line INDEX missing_index INTO missing_into FIELD VALUE missing_direct FIELD VALUE missing_field INTO missing_target.
MODIFY LINE missing_modify_line INDEX missing_modify_index FIELD VALUE missing_modify_field FIELD VALUE constant FROM missing_modify_source.
READ LINE 1 INTO constant FIELD VALUE constant FIELD VALUE sy-index INTO constant.
MODIFY LINE 1 FIELD VALUE constant FROM constant.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://line_validation.abap")

	names := [?]string {
		"missing_line",
		"missing_index",
		"missing_into",
		"missing_direct",
		"missing_field",
		"missing_target",
		"missing_modify_line",
		"missing_modify_index",
		"missing_modify_field",
		"missing_modify_source",
	}
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), len(names))
	for name in names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 1)
	}
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "READ LINE INTO target is not writable"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "READ LINE FIELD VALUE target is not writable"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "READ LINE FIELD VALUE INTO target is not writable"),
		1,
	)
}
