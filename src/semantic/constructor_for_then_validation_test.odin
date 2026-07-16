package abap_frontend_semantic2

import "core:testing"
import ast "src:ast"

@(test)
constructor_for_then_operands_are_readable_and_iteration_variable_is_scoped :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA start TYPE i.
DATA step TYPE i.
DATA ready TYPE abap_bool.
DATA(result) = REDUCE i( INIT total = 0 FOR index = start THEN index + step WHILE ready NEXT total = total + index ).`
	checker, file := checker_test_check_source(t, &project, source, "mem://constructor_for_then_roles.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	decl := file.root.stmts[3].derived_stmt.(^ast.Data_Inline_Decl)
	constructor := decl.expr.derived_expr.(^ast.Constructor_Expr)
	for_clause := constructor.args[1].derived_expr.(^ast.Constructor_For_Clause_Expr)
	checker_test_expect_expr_lhs(t, &checker, for_clause.init, false)
	checker_test_expect_expr_lhs(t, &checker, for_clause.then_expr, false)
	checker_test_expect_expr_lhs(t, &checker, for_clause.condition, false)
	names := [?]string{"start", "step", "ready"}
	for name in names {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
	index_name := project_intern_lower_ascii(&project, "index")
	_, index_visible := scope_lookup_declaration(file.root_scope, .Value, index_name)
	testing.expect(t, !index_visible)
}

@(test)
constructor_for_then_conditions_validate_known_types_and_preserve_unknowns :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA number TYPE i.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.
DATA(a) = VALUE i( FOR i = 1 UNTIL number ( i ) ).
DATA(b) = VALUE i( FOR i = 1 WHILE values ( i ) ).
DATA(c) = VALUE i( FOR i = 1 UNTIL reference ( i ) ).
DATA(d) = VALUE i( FOR i = 1 WHILE missing_condition ( i ) ).`
	checker, _ := checker_test_check_source(t, &project, source, "mem://constructor_for_then_types.abap")

	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "FOR UNTIL condition is not logical"), 2)
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "FOR WHILE condition is not logical"), 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_condition"), 1)
}

@(test)
constructor_for_then_conditions_record_selector_and_raw_operands_once :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_state,
         ready TYPE abap_bool,
       END OF ty_state.
DATA state TYPE ty_state.
DATA(a) = VALUE i( FOR i = 1 UNTIL state-ready = abap_true ( i ) ).
DATA(b) = VALUE i( FOR i = 1 WHILE missing_condition IS NOT INITIAL ( i ) ).`
	checker, file := checker_test_check_source(t, &project, source, "mem://constructor_for_then_facts.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Syntax_Form), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_condition"), 1)
	state := checker_test_lookup(t, &project, file.root_scope, .Value, "state", .Variable)
	testing.expect(t, state != nil && .Used in state.flags)
}
