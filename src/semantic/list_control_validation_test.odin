package abap_frontend_semantic2

import "core:testing"

@(test)
list_control_position_requires_type_i :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA lv_position TYPE i.
DATA lv_text TYPE string.
DATA lt_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA lr_value TYPE REF TO i.
FIELD-SYMBOLS <generic> TYPE any.
POSITION lv_position.
POSITION <generic>.
POSITION lv_text.
POSITION lt_values.
POSITION lr_value.
POSITION missing_position.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://list_control_position_types.abap")

	invalid_ranges := [?]string {"lv_text", "lt_values", "lr_value"}
	seen := [len(invalid_ranges)]bool{}
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form {
			continue
		}
		testing.expect_value(t, diagnostic.message, "POSITION operand is not type i")
		text := source[diagnostic.range.start:diagnostic.range.end]
		matched := false
		for expected, index in invalid_ranges {
			if text == expected {
				seen[index] = true
				matched = true
			}
		}
		testing.expect(t, matched)
	}
	for found in seen {
		testing.expect(t, found)
	}
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Syntax_Form), len(invalid_ranges))
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_position"),
		1,
	)
}

@(test)
list_control_hide_requires_writable_flat_variables :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_flat,
  value TYPE i,
END OF ty_flat.
TYPES: BEGIN OF ty_deep,
  value TYPE string,
END OF ty_deep.
DATA lv_value TYPE i.
DATA ls_flat TYPE ty_flat.
DATA lv_string TYPE string.
DATA ls_deep TYPE ty_deep.
DATA lt_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
CONSTANTS gc_value TYPE i VALUE 1.
FIELD-SYMBOLS <generic> TYPE any.
HIDE lv_value.
HIDE ls_flat.
HIDE <generic>.
HIDE gc_value.
HIDE lv_string.
HIDE ls_deep.
HIDE lt_values.
HIDE missing_hide.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://list_control_hide_types.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "HIDE operand is not writable"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "HIDE operand is not flat"),
		3,
	)
	invalid_ranges := [?]string {"gc_value", "lv_string", "ls_deep", "lt_values"}
	seen := [len(invalid_ranges)]bool{}
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form {
			continue
		}
		text := source[diagnostic.range.start:diagnostic.range.end]
		matched := false
		for expected, index in invalid_ranges {
			if text == expected {
				seen[index] = true
				matched = true
			}
		}
		testing.expect(t, matched)
	}
	for found in seen {
		testing.expect(t, found)
	}
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Syntax_Form), len(invalid_ranges))
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_hide"),
		1,
	)
}
