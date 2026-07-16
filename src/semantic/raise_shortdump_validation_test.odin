package abap_frontend_semantic2

import "core:testing"
import ast "src:ast"

@(test)
raise_shortdump_type_requires_exception_class_and_checks_constructor_arguments :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES ty_value TYPE i.
CLASS lcl_regular DEFINITION.
ENDCLASS.
CLASS lcx_dump DEFINITION INHERITING FROM cx_root.
  PUBLIC SECTION.
    METHODS constructor IMPORTING text TYPE string.
ENDCLASS.

DATA lv_text TYPE string.
RAISE SHORTDUMP TYPE ty_value.
RAISE SHORTDUMP TYPE lcl_regular.
RAISE SHORTDUMP TYPE lcx_dump EXPORTING text = lv_text.
RAISE SHORTDUMP TYPE lcx_dump EXPORTING missing = lv_text.`
	checker, file := checker_test_check_source(t, &project, source, "mem://raise_shortdump_type.abap")

	message := "RAISE SHORTDUMP TYPE target is not an exception class"
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, message), 2)
	invalid_ranges := [?]string{"ty_value", "lcl_regular"}
	invalid_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind == .Invalid_Syntax_Form && diagnostic.message == message {
			testing.expect(t, invalid_count < len(invalid_ranges))
			if invalid_count < len(invalid_ranges) {
				testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], invalid_ranges[invalid_count])
			}
			invalid_count += 1
		}
	}
	testing.expect_value(t, invalid_count, len(invalid_ranges))
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Named_Parameter), 1)

	for stmt in file.root.stmts[4:] {
		raise := stmt.derived_stmt.(^ast.Raise_Stmt)
		info, ok := checker_test_expr_info_for_node(t, &checker, &raise.target.expr_base)
		testing.expect(t, ok)
		if ok {
			testing.expect_value(t, info.mode, ast.Addressing_Mode.Type)
			testing.expect(t, !info.is_lhs)
		}
	}

	lv_text := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_text", .Variable)
	testing.expect(t, lv_text != nil && .Used in lv_text.flags)
}

@(test)
raise_shortdump_type_preserves_unresolved_type_and_value_candidates :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `RAISE SHORTDUMP TYPE zcx_missing_dump EXPORTING text = missing_dump_value.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://raise_shortdump_unresolved.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_namespace_count(
			&checker,
			&project,
			.Global_Symbol,
			.Type,
			"zcx_missing_dump",
		),
		1,
	)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_dump_value"),
		1,
	)
}
