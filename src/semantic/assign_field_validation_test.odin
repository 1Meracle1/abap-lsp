package abap_frontend_semantic2

import "src:ast"

import "core:testing"

@(test)
assign_field_operands_follow_readable_and_writable_directions :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA source TYPE string.
DATA component TYPE string.
DATA structure TYPE string.
DATA decimals TYPE i.
FIELD-SYMBOLS <target> TYPE string.
ASSIGN source TO <target> CASTING DECIMALS decimals.
ASSIGN COMPONENT component OF STRUCTURE structure TO FIELD-SYMBOL(<inline>).`
	checker, file := checker_test_check_source(t, &project, source, "mem://assign_field_direction.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	direct := file.root.stmts[5].derived_stmt.(^ast.Assign_Field_Stmt)
	component := file.root.stmts[6].derived_stmt.(^ast.Assign_Field_Stmt)
	readable := [?]^ast.Expr {direct.source, direct.casting_decimals, component.component, component.structure}
	for expr in readable {
		info, ok := checker_test_expr_info_for_node(t, &checker, &expr.expr_base)
		testing.expect(t, ok)
		if ok {
			testing.expect(t, !info.is_lhs)
		}
	}
	writable := [?]^ast.Expr {direct.target, component.target}
	for expr in writable {
		info, ok := checker_test_expr_info_for_node(t, &checker, &expr.expr_base)
		testing.expect(t, ok)
		if ok {
			testing.expect(t, info.is_lhs)
		}
	}
	inline := checker_test_lookup(t, &project, file.root_scope, .Value, "<inline>", .Field_Symbol)
	testing.expect(t, inline != nil)
}

@(test)
assign_field_operands_report_unresolved_values_and_reject_constant_target :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `CONSTANTS constant TYPE string VALUE ''.
FIELD-SYMBOLS <target> TYPE any.
ASSIGN missing_source TO <target> CASTING TYPE (missing_type_name) DECIMALS missing_decimals.
ASSIGN COMPONENT missing_component OF STRUCTURE missing_structure TO <target>.
ASSIGN constant TO constant.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://assign_field_validation.abap")

	names := [?]string {
		"missing_source",
		"missing_type_name",
		"missing_decimals",
		"missing_component",
		"missing_structure",
	}
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), len(names))
	for name in names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 1)
	}
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "ASSIGN target is not writable"),
		1,
	)
}
