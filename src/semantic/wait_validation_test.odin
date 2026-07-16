package abap_frontend_semantic2

import "core:testing"
import ast "src:ast"

@(test)
wait_until_condition_is_readable_and_duration_remains_independent :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA ready TYPE abap_bool.
DATA duration TYPE i.
WAIT UNTIL ready UP TO duration SECONDS.`
	checker, file := checker_test_check_source(t, &project, source, "mem://wait_roles.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	stmt := file.root.stmts[2].derived_stmt.(^ast.Wait_Stmt)
	checker_test_expect_expr_lhs(t, &checker, stmt.condition, false)
	checker_test_expect_expr_lhs(t, &checker, stmt.duration, false)
	names := [?]string{"ready", "duration"}
	for name in names {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}

@(test)
wait_until_validates_known_types_and_preserves_unknowns :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA ready TYPE abap_bool.
DATA number TYPE i.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.
WAIT UNTIL ready.
WAIT UNTIL number.
WAIT UNTIL values.
WAIT UNTIL reference.
WAIT UNTIL missing_condition.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://wait_types.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Syntax_Form), 3)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"WAIT UNTIL condition is not logical",
		),
		3,
	)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(
			&checker,
			&project,
			.Global_Symbol,
			"missing_condition",
		),
		1,
	)
}

@(test)
wait_until_records_selector_and_raw_operands_once :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_state,
         ready TYPE abap_bool,
       END OF ty_state.
DATA state TYPE ty_state.
WAIT UNTIL state-ready = abap_true.
WAIT UNTIL missing_wait IS INITIAL.`
	checker, file := checker_test_check_source(t, &project, source, "mem://wait_facts.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Syntax_Form), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_wait"),
		1,
	)
	state := checker_test_lookup(t, &project, file.root_scope, .Value, "state", .Variable)
	testing.expect(t, state != nil && .Used in state.flags)
}
