package abap_frontend_semantic2

import "core:testing"
import ast "src:ast"

@(test)
write_position_and_length_require_type_i_operands :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA integer TYPE i.
DATA int8_value TYPE int8.
DATA packed_value TYPE p DECIMALS 0.
DATA text TYPE string.
DATA row TYPE sy.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.

WRITE AT integer text.
WRITE /1(integer) text.
WRITE AT int8_value text.
WRITE /1(packed_value) text.
WRITE AT row text.
WRITE /1(values) text.
WRITE AT reference text.
WRITE /1(text) text.
WRITE AT missing_position missing_value.
WRITE /1(missing_length) text.`
	checker, file := checker_test_check_source(t, &project, source, "mem://write_format_operand_types.abap")

	messages := [?]string {
		"WRITE AT position operand does not have type i",
		"WRITE output length operand does not have type i",
		"WRITE AT position operand does not have type i",
		"WRITE output length operand does not have type i",
		"WRITE AT position operand does not have type i",
		"WRITE output length operand does not have type i",
	}
	invalid_ranges := [?]string{"int8_value", "packed_value", "row", "values", "reference", "text"}
	invalid_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form {
			continue
		}
		testing.expect_value(t, diagnostic.message, messages[invalid_count])
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], invalid_ranges[invalid_count])
		invalid_count += 1
	}
	testing.expect_value(t, invalid_count, len(invalid_ranges))
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 3)
	unresolved_names := [?]string{"missing_position", "missing_length", "missing_value"}
	for name in unresolved_names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 1)
	}

	valid_position_stmt := file.root.stmts[7].derived_stmt.(^ast.Write_Stmt)
	valid_length_stmt := file.root.stmts[8].derived_stmt.(^ast.Write_Stmt)
	checker_test_expect_expr_lhs(t, &checker, valid_position_stmt.operands[0].position, false)
	checker_test_expect_expr_lhs(t, &checker, valid_position_stmt.operands[0].value, false)
	checker_test_expect_expr_lhs(t, &checker, valid_length_stmt.operands[0].length, false)
	for name in invalid_ranges {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}

@(test)
write_value_requires_a_simple_type_or_character_like_flat_structure :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF character_row,
  text TYPE c LENGTH 3,
  digits TYPE n LENGTH 2,
  date TYPE d,
END OF character_row.
TYPES: BEGIN OF numeric_row,
  text TYPE c LENGTH 3,
  number TYPE i,
END OF numeric_row.
TYPES: BEGIN OF string_row,
  text TYPE string,
END OF string_row.
TYPES: BEGIN OF nested_row,
  value TYPE character_row,
END OF nested_row.

DATA character_value TYPE character_row.
DATA numeric_value TYPE numeric_row.
DATA string_value TYPE string_row.
DATA nested_value TYPE nested_row.
DATA table_value TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference_value TYPE REF TO i.
DATA object_value TYPE object.
DATA integer TYPE i.
DATA bytes TYPE xstring.
FIELD-SYMBOLS <simple_value> TYPE simple.
FIELD-SYMBOLS <unknown_value> TYPE any.

WRITE character_value.
WRITE integer.
WRITE bytes.
WRITE <simple_value>.
WRITE <unknown_value>.
WRITE numeric_value.
WRITE string_value.
WRITE nested_value.
WRITE table_value.
WRITE reference_value.
WRITE object_value.
WRITE missing_value.`
	checker, file := checker_test_check_source(t, &project, source, "mem://write_value_types.abap")

	invalid_ranges := [?]string {
		"numeric_value",
		"string_value",
		"nested_value",
		"table_value",
		"reference_value",
		"object_value",
	}
	invalid_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form {
			continue
		}
		testing.expect_value(t, diagnostic.message, "WRITE value operand is not simple")
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], invalid_ranges[invalid_count])
		invalid_count += 1
	}
	testing.expect_value(t, invalid_count, len(invalid_ranges))
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_value"), 1)

	for stmt_index in 15..<27 {
		stmt := file.root.stmts[stmt_index].derived_stmt.(^ast.Write_Stmt)
		checker_test_expect_expr_lhs(t, &checker, stmt.operands[0].value, false)
	}
	used_names := [?]string {
		"character_value",
		"numeric_value",
		"string_value",
		"nested_value",
		"table_value",
		"reference_value",
		"object_value",
		"integer",
		"bytes",
	}
	for name in used_names {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}
