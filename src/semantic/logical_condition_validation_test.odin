package abap_frontend_semantic2

import "core:testing"
import ast "src:ast"

@(test)
statement_logical_conditions_are_readable_and_while_body_is_checked :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA assert_flag TYPE abap_bool.
DATA check_flag TYPE abap_bool.
DATA while_flag TYPE abap_bool.
DATA body_value TYPE i.
ASSERT assert_flag.
CHECK check_flag.
WHILE while_flag.
  body_value = body_value + 1.
ENDWHILE.`
	checker, file := checker_test_check_source(t, &project, source, "mem://logical_condition_roles.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	assert_stmt := file.root.stmts[4].derived_stmt.(^ast.Assert_Stmt)
	check_stmt := file.root.stmts[5].derived_stmt.(^ast.Check_Stmt)
	while_stmt := file.root.stmts[6].derived_stmt.(^ast.While_Stmt)
	checker_test_expect_expr_lhs(t, &checker, assert_stmt.condition, false)
	checker_test_expect_expr_lhs(t, &checker, check_stmt.condition, false)
	checker_test_expect_expr_lhs(t, &checker, while_stmt.condition, false)
	testing.expect_value(t, len(while_stmt.body), 1)
	names := [?]string{"assert_flag", "check_flag", "while_flag", "body_value"}
	for name in names {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}

@(test)
statement_logical_conditions_validate_known_types_and_preserve_unknowns :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA number TYPE i.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.
ASSERT number.
CHECK values.
WHILE reference.
ENDWHILE.
CHECK missing_condition.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://logical_condition_types.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Syntax_Form), 3)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_condition"),
		1,
	)
	seen_assert := false
	seen_check := false
	seen_while := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form {
			continue
		}
		text := source[diagnostic.range.start:diagnostic.range.end]
		switch text {
		case "number":
			testing.expect_value(t, diagnostic.message, "ASSERT condition is not logical")
			seen_assert = true
		case "values":
			testing.expect_value(t, diagnostic.message, "CHECK condition is not logical")
			seen_check = true
		case "reference":
			testing.expect_value(t, diagnostic.message, "WHILE condition is not logical")
			seen_while = true
		case:
			testing.expect(t, false)
		}
	}
	testing.expect(t, seen_assert && seen_check && seen_while)
}

@(test)
statement_logical_conditions_record_raw_and_selector_operands_once :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_state,
         ready TYPE abap_bool,
       END OF ty_state.
DATA state TYPE ty_state.
ASSERT state-ready = abap_true.
CHECK missing_check IS INITIAL.
WHILE missing_while IS NOT INITIAL.
ENDWHILE.`
	checker, file := checker_test_check_source(t, &project, source, "mem://logical_condition_facts.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)
	names := [?]string{"missing_check", "missing_while"}
	for name in names {
		testing.expect_value(
			t,
			checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name),
			1,
		)
	}
	state := checker_test_lookup(t, &project, file.root_scope, .Value, "state", .Variable)
	testing.expect(t, state != nil && .Used in state.flags)
}
