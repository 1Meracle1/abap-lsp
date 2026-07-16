package abap_frontend_semantic2

import "core:testing"
import ast "src:ast"

@(test)
table_mutation_operands_have_expected_directionality :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES ty_table TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA source TYPE i.
DATA index TYPE i.
DATA itab TYPE ty_table.
DATA result TYPE i.

APPEND source TO itab ASSIGNING result.
INSERT source INTO itab INDEX index REFERENCE INTO result.
MODIFY itab FROM source INDEX index.
SORT itab BY (source).`
	checker, file := checker_test_check_source(t, &project, source, "mem://table_mutation_directionality.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 0)
	for stmt in file.root.stmts {
		#partial switch n in stmt.derived_stmt {
		case ^ast.Append_Stmt:
			checker_test_expect_expr_lhs(t, &checker, n.source, false)
			checker_test_expect_expr_lhs(t, &checker, n.target, true)
			checker_test_expect_expr_lhs(t, &checker, n.assigning, true)
		case ^ast.Insert_Stmt:
			checker_test_expect_expr_lhs(t, &checker, n.source, false)
			checker_test_expect_expr_lhs(t, &checker, n.target, true)
			checker_test_expect_expr_lhs(t, &checker, n.index, false)
			checker_test_expect_expr_lhs(t, &checker, n.reference_into, true)
		case ^ast.Modify_Stmt:
			checker_test_expect_expr_lhs(t, &checker, n.target, true)
			checker_test_expect_expr_lhs(t, &checker, n.source, false)
			checker_test_expect_expr_lhs(t, &checker, n.index, false)
		case ^ast.Sort_Stmt:
			checker_test_expect_expr_lhs(t, &checker, n.target, true)
			checker_test_expect_expr_lhs(t, &checker, n.fields[0].expr, false)
		}
	}
}

checker_test_expect_expr_lhs :: proc(
	t: ^testing.T,
	checker: ^Checker,
	expr: ^ast.Expr,
	expected: bool,
) {
	info, ok := checker_test_expr_info_for_node(t, checker, &expr.expr_base)
	testing.expect(t, ok)
	if ok {
		testing.expect_value(t, info.is_lhs, expected)
	}
}

@(test)
table_mutation_statements_report_unresolved_operands_and_candidates :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES ty_table TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA itab TYPE ty_table.

APPEND missing_append TO itab ASSIGNING missing_assigning.
INSERT missing_insert INTO itab INDEX missing_insert_index REFERENCE INTO missing_reference.
MODIFY itab FROM missing_modify INDEX missing_modify_index.
SORT itab BY (missing_sort).`
	checker, _ := checker_test_check_source(t, &project, source, "mem://table_mutation_unresolved.abap")

	names := [?]string {
		"missing_append",
		"missing_assigning",
		"missing_insert",
		"missing_insert_index",
		"missing_reference",
		"missing_modify",
		"missing_modify_index",
		"missing_sort",
	}
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), len(names))
	for name in names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 1)
	}
}

@(test)
table_mutation_result_targets_reject_constants_and_support_inline_declarations :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES ty_table TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
CONSTANTS constant TYPE i VALUE 1.
DATA itab TYPE ty_table.

APPEND 1 TO itab ASSIGNING constant.
INSERT 1 INTO itab REFERENCE INTO constant.
APPEND 1 TO itab ASSIGNING FIELD-SYMBOL(<line>).
INSERT 1 INTO itab REFERENCE INTO DATA(reference).`
	checker, file := checker_test_check_source(t, &project, source, "mem://table_mutation_targets.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "internal table result target is not writable"),
		2,
	)
	testing.expect(t, checker_test_lookup(t, &project, file.root_scope, .Value, "<line>", .Field_Symbol) != nil)
	testing.expect(t, checker_test_lookup(t, &project, file.root_scope, .Value, "reference", .Variable) != nil)
}
