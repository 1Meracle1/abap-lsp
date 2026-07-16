package abap_frontend_semantic2

import "core:testing"
import ast "src:ast"

@(test)
read_table_operands_have_expected_directionality :: proc(t: ^testing.T) {
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
DATA component_name TYPE string.
DATA value TYPE i.
DATA reference TYPE REF TO ty_row.
FIELD-SYMBOLS <row> TYPE ty_row.

READ TABLE itab INTO row INDEX index USING KEY (key_name) COMPARING (component_name).
READ TABLE itab ASSIGNING <row> WITH KEY (component_name) = value.
READ TABLE itab REFERENCE INTO reference WITH KEY id = value.`
	checker, file := checker_test_check_source(t, &project, source, "mem://read_table_directionality.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 0)
	for stmt in file.root.stmts {
		read_stmt, ok := stmt.derived_stmt.(^ast.Read_Table_Stmt)
		if !ok {
			continue
		}
		for entry in read_stmt.entries {
			checker_test_expect_expr_lhs(t, &checker, entry.table, false)
			targets := [?]^ast.Expr {entry.into, entry.assigning, entry.reference_into}
			for target in targets {
				if target != nil {
					checker_test_expect_expr_lhs(t, &checker, target, true)
				}
			}
			if entry.index != nil {
				checker_test_expect_expr_lhs(t, &checker, entry.index, false)
			}
			if entry.using_key.dynamic_name != nil {
				checker_test_expect_expr_lhs(t, &checker, entry.using_key.dynamic_name, false)
			}
			for key in entry.key_values {
				if key.dynamic_name != nil {
					checker_test_expect_expr_lhs(t, &checker, key.dynamic_name, false)
				}
				checker_test_expect_expr_lhs(t, &checker, key.value, false)
			}
			for comparing in entry.comparing {
				checker_test_expect_expr_lhs(t, &checker, comparing, false)
			}
		}
	}
}

@(test)
read_table_reports_unresolved_operands_and_candidates :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES ty_table TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA itab TYPE ty_table.

READ TABLE missing_table INTO missing_into INDEX missing_index USING KEY (missing_key).
READ TABLE itab ASSIGNING missing_assigning WITH KEY (missing_component) = missing_value COMPARING (missing_comparing).
READ TABLE itab REFERENCE INTO missing_reference INDEX 1.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://read_table_unresolved.abap")

	names := [?]string {
		"missing_table",
		"missing_into",
		"missing_index",
		"missing_key",
		"missing_assigning",
		"missing_component",
		"missing_value",
		"missing_comparing",
		"missing_reference",
	}
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), len(names))
	for name in names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 1)
	}
}

@(test)
read_table_targets_reject_constants_and_support_inline_declarations :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES ty_table TYPE STANDARD TABLE OF i WITH EMPTY KEY.
CONSTANTS constant TYPE i VALUE 1.
DATA itab TYPE ty_table.

READ TABLE itab INTO constant INDEX 1.
READ TABLE itab ASSIGNING constant INDEX 1.
READ TABLE itab REFERENCE INTO constant INDEX 1.
READ TABLE itab INTO DATA(line) INDEX 1.
READ TABLE itab ASSIGNING FIELD-SYMBOL(<line>) INDEX 1.
READ TABLE itab REFERENCE INTO DATA(reference) INDEX 1.`
	checker, file := checker_test_check_source(t, &project, source, "mem://read_table_targets.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "internal table result target is not writable"),
		3,
	)
	testing.expect(t, checker_test_lookup(t, &project, file.root_scope, .Value, "line", .Variable) != nil)
	testing.expect(t, checker_test_lookup(t, &project, file.root_scope, .Value, "<line>", .Field_Symbol) != nil)
	testing.expect(t, checker_test_lookup(t, &project, file.root_scope, .Value, "reference", .Variable) != nil)
}
