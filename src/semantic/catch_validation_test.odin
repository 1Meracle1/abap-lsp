package abap_frontend_semantic2

import "core:testing"
import ast "src:ast"

@(test)
catch_operands_have_expected_roles_and_inline_target_type :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `CLASS lcx_first DEFINITION INHERITING FROM cx_root.
ENDCLASS.
CLASS lcx_second DEFINITION INHERITING FROM cx_root.
ENDCLASS.

TRY.
CATCH lcx_first lcx_second INTO DATA(lx_error).
ENDTRY.`
	checker, file := checker_test_check_source(t, &project, source, "mem://catch_roles.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	try_stmt := file.root.stmts[2].derived_stmt.(^ast.Try_Stmt)
	clause := try_stmt.catches[0]
	for exception in clause.exceptions {
		info, ok := checker_test_expr_info_for_node(t, &checker, &exception.expr_base)
		testing.expect(t, ok)
		if ok {
			testing.expect_value(t, info.mode, ast.Addressing_Mode.Type)
			testing.expect(t, !info.is_lhs)
		}
	}
	checker_test_expect_expr_lhs(t, &checker, clause.into, true)

	lx_error := checker_test_lookup(t, &project, file.root_scope, .Value, "lx_error", .Variable)
	testing.expect(t, lx_error != nil && lx_error.type != nil)
	if lx_error != nil && lx_error.type != nil {
		testing.expect_value(t, lx_error.type.kind, Type_Kind.Ref)
		testing.expect(t, lx_error.type.base != nil)
		if lx_error.type.base != nil {
			testing.expect_value(t, lx_error.type.base.name, "lcx_first")
		}
	}
}

@(test)
catch_reports_class_and_target_candidates_in_their_namespaces :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TRY.
CATCH zcx_missing INTO missing_target.
ENDTRY.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://catch_unresolved.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Type), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Class, "zcx_missing"), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "zcx_missing"), 0)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_target"), 1)
}

@(test)
catch_rejects_known_non_class_exception_types :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES ty_value TYPE i.
TRY.
CATCH ty_value.
ENDTRY.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://catch_non_class.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"CATCH exception type is not a class",
		),
		1,
	)
}

@(test)
catch_target_validates_writability_and_assignment_compatibility_once :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `CONSTANTS lc_error TYPE REF TO object VALUE IS INITIAL.
DATA lv_text TYPE string.
TRY.
CATCH cx_root INTO lc_error.
ENDTRY.
TRY.
CATCH cx_root INTO lv_text.
ENDTRY.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://catch_targets.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"CATCH INTO target is not writable",
		),
		1,
	)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 1)
}
