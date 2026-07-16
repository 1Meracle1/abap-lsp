package abap_frontend_semantic2

import "core:testing"
import ast "src:ast"

@(test)
loop_operands_have_expected_directionality :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         id TYPE i,
       END OF ty_row,
       ty_table TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA itab TYPE ty_table.
DATA row TYPE ty_row.
DATA reference TYPE REF TO i.
DATA lower TYPE i.
DATA upper TYPE i.
DATA key_name TYPE string.
DATA id TYPE i.
DATA group_key TYPE i.
FIELD-SYMBOLS <row> TYPE ty_row.

LOOP AT itab INTO row FROM lower TO upper USING KEY (key_name) WHERE id = group_key GROUP BY group_key REFERENCE INTO reference.
ENDLOOP.
LOOP AT itab ASSIGNING <row>.
ENDLOOP.`
	checker, file := checker_test_check_source(t, &project, source, "mem://loop_directionality.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	for stmt in file.root.stmts {
		loop, ok := stmt.derived_stmt.(^ast.Loop_Stmt)
		if !ok {
			continue
		}
		checker_test_expect_expr_lhs(t, &checker, loop.source, false)
		if loop.target != nil {
			checker_test_expect_expr_lhs(t, &checker, loop.target, true)
		}
		if loop.from != nil {
			checker_test_expect_expr_lhs(t, &checker, loop.from, false)
		}
		if loop.to != nil {
			checker_test_expect_expr_lhs(t, &checker, loop.to, false)
		}
		if loop.using_key.dynamic_name != nil {
			checker_test_expect_expr_lhs(t, &checker, loop.using_key.dynamic_name, false)
		}
		if loop.group_by != nil {
			checker_test_expect_expr_lhs(t, &checker, loop.group_by, false)
		}
		if loop.group_target != nil {
			checker_test_expect_expr_lhs(t, &checker, loop.group_target, true)
		}
	}
}

@(test)
loop_reports_unresolved_readable_operands_and_targets :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES ty_table TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA itab TYPE ty_table.

LOOP AT missing_table INTO missing_into FROM missing_from TO missing_to USING KEY (missing_key) WHERE table_line = missing_where.
ENDLOOP.
LOOP AT itab ASSIGNING missing_assigning GROUP BY missing_group REFERENCE INTO missing_group_target.
ENDLOOP.
LOOP AT itab REFERENCE INTO missing_reference.
ENDLOOP.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://loop_unresolved.abap")

	names := [?]string {
		"missing_table",
		"missing_into",
		"missing_from",
		"missing_to",
		"missing_key",
		"missing_where",
		"missing_assigning",
		"missing_group",
		"missing_group_target",
		"missing_reference",
	}
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), len(names))
	for name in names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 1)
	}
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "table_line"), 0)
}

@(test)
loop_validates_targets_bounds_types_and_row_components :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_nested,
         part TYPE string,
       END OF ty_nested,
       BEGIN OF ty_row,
         id TYPE i,
         nested TYPE ty_nested,
       END OF ty_row,
       ty_table TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA itab TYPE ty_table.
DATA text TYPE string.
DATA dynamic_type TYPE string.
CONSTANTS constant TYPE ty_row VALUE IS INITIAL.

LOOP AT itab INTO constant FROM text TO 1sdf TRANSPORTING id nested-part WHERE id = 1.
ENDLOOP.
LOOP AT itab ASSIGNING constant.
ENDLOOP.
LOOP AT itab REFERENCE INTO constant.
ENDLOOP.
LOOP AT itab INTO DATA(group_row) GROUP BY group_row-id INTO constant.
ENDLOOP.
LOOP AT itab INTO DATA(row).
ENDLOOP.
LOOP AT itab ASSIGNING FIELD-SYMBOL(<row>) CASTING TYPE ty_row.
ENDLOOP.
LOOP AT itab REFERENCE INTO DATA(reference).
ENDLOOP.
LOOP AT itab ASSIGNING FIELD-SYMBOL(<dynamic>) CASTING TYPE (dynamic_type).
ENDLOOP.`
	checker, file := checker_test_check_source(t, &project, source, "mem://loop_validation.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "internal table result target is not writable"),
		4,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "LOOP FROM operand is not integer-compatible"),
		1,
	)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "1sdf"), 1)
	component_names := [?]string{"id", "nested", "part"}
	for name in component_names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 0)
	}
	testing.expect(t, checker_test_lookup(t, &project, file.root_scope, .Value, "row", .Variable) != nil)
	testing.expect(t, checker_test_lookup(t, &project, file.root_scope, .Value, "<row>", .Field_Symbol) != nil)
	testing.expect(t, checker_test_lookup(t, &project, file.root_scope, .Value, "reference", .Variable) != nil)
	testing.expect(t, checker_test_lookup(t, &project, file.root_scope, .Value, "<dynamic>", .Field_Symbol) != nil)
	ty_row := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_row", .Type_Def)
	dynamic_type := checker_test_lookup(t, &project, file.root_scope, .Value, "dynamic_type", .Variable)
	testing.expect(t, ty_row != nil && .Used in ty_row.flags)
	testing.expect(t, dynamic_type != nil && .Used in dynamic_type.flags)
}
