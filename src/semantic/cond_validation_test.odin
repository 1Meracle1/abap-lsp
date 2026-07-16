package abap_frontend_semantic2

import "core:testing"
import ast "src:ast"

@(test)
cond_when_operands_are_readable_and_let_binding_is_scoped :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA ready TYPE abap_bool.
DATA value TYPE i.
DATA(result) = COND i( LET local = ready IN WHEN local = abap_true THEN CONV i( value ) ELSE CONV i( 0 ) ).`
	checker, file := checker_test_check_source(t, &project, source, "mem://cond_when_roles.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	decl := file.root.stmts[2].derived_stmt.(^ast.Data_Inline_Decl)
	constructor := decl.expr.derived_expr.(^ast.Constructor_Expr)
	let_expr := constructor.args[0].derived_expr.(^ast.Let_Expr)
	when_clause := let_expr.body[0].derived_expr.(^ast.Constructor_When_Clause_Expr)
	else_clause := let_expr.body[1].derived_expr.(^ast.Constructor_Else_Clause_Expr)
	checker_test_expect_expr_lhs(t, &checker, when_clause.condition, false)
	checker_test_expect_expr_lhs(t, &checker, when_clause.result, false)
	checker_test_expect_expr_lhs(t, &checker, else_clause.result, false)
	names := [?]string{"ready", "value"}
	for name in names {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
	local_name := project_intern_lower_ascii(&project, "local")
	_, local_visible := scope_lookup_declaration(file.root_scope, .Value, local_name)
	testing.expect(t, !local_visible)
}

@(test)
cond_when_conditions_validate_known_types_and_preserve_unknowns :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA number TYPE i.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.
DATA result TYPE i.
result = COND i( WHEN number THEN 1 ELSE 0 ).
result = COND i( WHEN values THEN 1 ELSE 0 ).
result = COND i( WHEN reference THEN 1 ELSE 0 ).
result = COND i( WHEN missing_condition THEN 1 ELSE 0 ).`
	checker, _ := checker_test_check_source(t, &project, source, "mem://cond_when_types.abap")

	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "COND WHEN condition is not logical"), 3)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_condition"), 1)
}

@(test)
cond_when_conditions_record_selector_and_raw_operands_once :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_state,
         ready TYPE abap_bool,
       END OF ty_state.
DATA state TYPE ty_state.
DATA result TYPE i.
result = COND i( WHEN state-ready = abap_true THEN 1 WHEN missing_condition IS NOT INITIAL THEN 2 ELSE 0 ).`
	checker, file := checker_test_check_source(t, &project, source, "mem://cond_when_facts.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Syntax_Form), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_condition"), 1)
	state := checker_test_lookup(t, &project, file.root_scope, .Value, "state", .Variable)
	testing.expect(t, state != nil && .Used in state.flags)
}
