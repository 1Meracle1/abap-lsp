package abap_frontend_semantic2

import "core:testing"

@(test)
authority_check_operands_require_flat_character_like_values :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
  value TYPE c LENGTH 1,
END OF ty_row.
DATA lv_object TYPE c LENGTH 10.
DATA lv_id TYPE n LENGTH 8.
DATA lv_field TYPE d.
DATA lv_integer TYPE i.
DATA lv_string TYPE string.
DATA lv_bytes TYPE x LENGTH 4.
DATA ls_row TYPE ty_row.
DATA lt_values TYPE STANDARD TABLE OF c WITH EMPTY KEY.
DATA lr_value TYPE REF TO c.
FIELD-SYMBOLS <generic> TYPE any.
AUTHORITY-CHECK OBJECT lv_object ID lv_id FIELD lv_field.
AUTHORITY-CHECK OBJECT <generic> ID <generic> FIELD <generic>.
AUTHORITY-CHECK OBJECT lv_integer ID lv_string FIELD lv_bytes
  ID ls_row FIELD lt_values
  ID lr_value FIELD missing_field.
AUTHORITY-CHECK missing_legacy operand.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://authority_check_operand_types.abap")

	object_message := "AUTHORITY-CHECK OBJECT operand is not flat character-like"
	id_message := "AUTHORITY-CHECK ID operand is not flat character-like"
	field_message := "AUTHORITY-CHECK FIELD operand is not flat character-like"
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, object_message), 1)
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, id_message), 3)
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, field_message), 2)
	invalid_ranges := [?]string{"lv_integer", "lv_string", "lv_bytes", "ls_row", "lt_values", "lr_value"}
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
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 3)
	unresolved_names := [?]string{"missing_field", "missing_legacy", "operand"}
	for name in unresolved_names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 1)
	}
}
