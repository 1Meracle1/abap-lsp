package abap_frontend_semantic2

import "core:testing"
import ast "src:ast"

@(test)
case_operands_are_readable_and_inline_alternatives_use_selector_type :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA lv_selector TYPE i.
DATA lv_alternative TYPE i.
CASE lv_selector.
  WHEN lv_alternative OR DATA(lv_inline) OR 3.
  WHEN OTHERS.
ENDCASE.`
	checker, file := checker_test_check_source(t, &project, source, "mem://case_roles.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	stmt := file.root.stmts[2].derived_stmt.(^ast.Case_Stmt)
	checker_test_expect_expr_lhs(t, &checker, stmt.expr, false)
	for operand in stmt.whens[0].operands {
		checker_test_expect_expr_lhs(t, &checker, operand, false)
	}

	lv_selector := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_selector", .Variable)
	lv_alternative := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_alternative", .Variable)
	lv_inline := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_inline", .Variable)
	testing.expect(t, lv_selector != nil && .Used in lv_selector.flags)
	testing.expect(t, lv_alternative != nil && .Used in lv_alternative.flags)
	testing.expect(t, lv_inline != nil && lv_inline.type != nil)
	if lv_inline != nil && lv_inline.type != nil {
		testing.expect_value(t, lv_inline.type.name, "i")
	}
}

@(test)
case_reports_unresolved_selector_and_every_alternative_once :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `CASE missing_selector.
  WHEN missing_first OR missing_second.
  WHEN OTHERS.
ENDCASE.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://case_unresolved.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 3)
	names := [?]string{"missing_selector", "missing_first", "missing_second"}
	for name in names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 1)
	}
}

@(test)
case_checks_when_values_toward_the_selector_type :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA lv_selector TYPE i.
DATA lt_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
CASE lv_selector.
  WHEN 1 OR lt_values.
  WHEN OTHERS.
ENDCASE.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://case_compatibility.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Argument_Type), 1)
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind == .Incompatible_Argument_Type {
			testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "lt_values")
		}
	}
}

@(test)
case_type_of_resolves_types_and_contextualizes_into_target :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `CLASS lcl_first DEFINITION.
ENDCLASS.
CLASS lcl_second DEFINITION.
ENDCLASS.
DATA reference TYPE REF TO object.
CASE TYPE OF reference.
  WHEN TYPE lcl_first OR TYPE lcl_second INTO DATA(typed_reference).
  WHEN OTHERS.
ENDCASE.`
	checker, file := checker_test_check_source(t, &project, source, "mem://case_type_roles.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	stmt := file.root.stmts[3].derived_stmt.(^ast.Case_Stmt)
	checker_test_expect_expr_lhs(t, &checker, stmt.expr, false)
	for type_operand in stmt.whens[0].type_operands {
		info, ok := checker_test_expr_info_for_node(t, &checker, &type_operand.expr_base)
		testing.expect(t, ok)
		if ok {
			testing.expect_value(t, info.mode, ast.Addressing_Mode.Type)
			testing.expect(t, !info.is_lhs)
		}
	}
	checker_test_expect_expr_lhs(t, &checker, stmt.whens[0].into, true)
	typed_reference := checker_test_lookup(t, &project, file.root_scope, .Value, "typed_reference", .Variable)
	testing.expect(t, typed_reference != nil && checker_type_is_ref(typed_reference.type))
	if typed_reference != nil && checker_type_is_ref(typed_reference.type) {
		testing.expect_value(t, typed_reference.type.base.name, "lcl_first")
	}
}

@(test)
case_type_of_reports_type_and_target_candidates_in_their_namespaces :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA reference TYPE REF TO object.
CASE TYPE OF reference.
  WHEN TYPE zcl_missing INTO missing_target.
ENDCASE.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://case_type_unresolved.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Type), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "zcl_missing"), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_target"), 1)
}

@(test)
case_type_of_validates_selector_and_into_target :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `CLASS lcl_type DEFINITION.
ENDCLASS.
CLASS lcl_other DEFINITION.
ENDCLASS.
DATA value TYPE i.
CONSTANTS constant_ref TYPE REF TO lcl_type VALUE IS INITIAL.
DATA other_ref TYPE REF TO lcl_other.
CASE TYPE OF value.
  WHEN TYPE lcl_type INTO constant_ref.
ENDCASE.
CASE TYPE OF constant_ref.
  WHEN TYPE lcl_type INTO other_ref.
ENDCASE.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://case_type_invalid.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "CASE TYPE OF selector is not a reference"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "CASE TYPE OF INTO target is not writable"),
		1,
	)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 1)
}
