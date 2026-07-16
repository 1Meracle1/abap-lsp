package abap_frontend_semantic2

import "core:testing"
import ast "src:ast"

@(test)
if_conditions_are_readable_values_and_every_branch_is_checked :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA first TYPE abap_bool.
DATA second TYPE abap_bool.
DATA third TYPE abap_bool.
IF first.
ELSEIF second.
ELSEIF third.
ELSE.
ENDIF.`
	checker, file := checker_test_check_source(t, &project, source, "mem://if_roles.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	stmt := file.root.stmts[3].derived_stmt.(^ast.If_Stmt)
	checker_test_expect_expr_lhs(t, &checker, stmt.condition, false)
	testing.expect_value(t, len(stmt.elseif_clauses), 2)
	for clause in stmt.elseif_clauses {
		checker_test_expect_expr_lhs(t, &checker, clause.condition, false)
	}
	testing.expect(t, stmt.else_clause != nil)
	names := [?]string{"first", "second", "third"}
	for name in names {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}

@(test)
if_conditions_validate_known_types_and_preserve_unknowns :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA logical TYPE abap_bool.
DATA number TYPE i.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
IF logical.
ELSEIF number.
ELSEIF values.
ELSEIF missing_condition.
ENDIF.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://if_condition_types.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Syntax_Form), 2)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_condition"),
		1,
	)
	seen_number := false
	seen_values := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form {
			continue
		}
		testing.expect_value(t, diagnostic.message, "IF condition is not logical")
		text := source[diagnostic.range.start:diagnostic.range.end]
		seen_number = seen_number || text == "number"
		seen_values = seen_values || text == "values"
	}
	testing.expect(t, seen_number)
	testing.expect(t, seen_values)
}

@(test)
if_logical_expressions_record_raw_and_selector_operands_once :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_state,
         ready TYPE abap_bool,
       END OF ty_state.
DATA state TYPE ty_state.
IF state-ready = abap_true.
ELSEIF missing_raw IS INITIAL.
ENDIF.`
	checker, file := checker_test_check_source(t, &project, source, "mem://if_condition_facts.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_raw"),
		1,
	)
	state := checker_test_lookup(t, &project, file.root_scope, .Value, "state", .Variable)
	testing.expect(t, state != nil && .Used in state.flags)
}
