package abap_frontend_semantic2

import "core:testing"
import ast "src:ast"

@(test)
delete_operands_have_expected_directionality :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         id TYPE i,
       END OF ty_row,
       ty_table TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA itab TYPE ty_table.
DATA row TYPE ty_row.
DATA index TYPE i.
DATA key_name TYPE string.
DATA field_name TYPE string.
DATA value TYPE i.

DELETE itab FROM row INDEX index USING KEY (key_name) WHERE id = value.
DELETE ADJACENT DUPLICATES FROM itab COMPARING (field_name).`
	checker, file := checker_test_check_source(t, &project, source, "mem://delete_directionality.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 0)
	for stmt in file.root.stmts {
		delete_stmt, ok := stmt.derived_stmt.(^ast.Delete_Stmt)
		if !ok {
			continue
		}
		checker_test_expect_expr_lhs(t, &checker, delete_stmt.target, true)
		if delete_stmt.source != nil {
			checker_test_expect_expr_lhs(t, &checker, delete_stmt.source, false)
			checker_test_expect_expr_lhs(t, &checker, delete_stmt.index, false)
			checker_test_expect_expr_lhs(t, &checker, delete_stmt.using_key.dynamic_name, false)
		}
		for comparing in delete_stmt.comparing {
			if comparing.expr != nil {
				checker_test_expect_expr_lhs(t, &checker, comparing.expr, false)
			}
		}
	}
}

@(test)
delete_statements_report_unresolved_readable_operands_and_candidates :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES ty_table TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA itab TYPE ty_table.

DELETE itab FROM missing_source INDEX missing_index USING KEY (missing_key).
DELETE ADJACENT DUPLICATES FROM itab COMPARING (missing_comparing).
DELETE dbtab FROM TABLE missing_db_source.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://delete_unresolved.abap")

	names := [?]string {
		"missing_source",
		"missing_index",
		"missing_key",
		"missing_comparing",
		"missing_db_source",
	}
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), len(names))
	for name in names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 1)
	}
}

@(test)
delete_targets_reject_constants_and_static_components_resolve_as_fields :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         id TYPE i,
       END OF ty_row,
       ty_table TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
CONSTANTS constant TYPE ty_table VALUE IS INITIAL.
DATA itab TYPE ty_table.

DELETE constant WHERE id = 1.
DELETE ADJACENT DUPLICATES FROM itab COMPARING id.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://delete_targets.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Delete_Operand, "DELETE target is not writable"),
		1,
	)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 0)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "id"), 0)
}
