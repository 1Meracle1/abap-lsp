package abap_frontend_semantic2

import "core:testing"
import ast "src:ast"

@(test)
set_bit_position_and_to_operands_require_type_i :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         value TYPE i,
       END OF ty_row.
DATA bytes TYPE x LENGTH 2.
DATA integer TYPE i.
DATA int8_value TYPE int8.
DATA packed_value TYPE p DECIMALS 2.
DATA float_value TYPE f.
DATA text_value TYPE string.
DATA numeric_text TYPE n LENGTH 4.
DATA row TYPE ty_row.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.

SET BIT 1 OF bytes TO 0.
SET BIT ( integer + 1 ) OF bytes TO integer.
SET BIT int8_value OF bytes TO packed_value.
SET BIT float_value OF bytes TO text_value.
SET BIT numeric_text OF bytes TO row.
SET BIT values OF bytes TO reference.
SET BIT missing_position OF bytes TO missing_value.`
	checker, file := checker_test_check_source(t, &project, source, "mem://set_bit_operand_types.abap")

	position_message := "SET BIT position operand does not have type i"
	to_message := "SET BIT TO operand does not have type i"
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, position_message), 4)
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, to_message), 4)
	invalid_ranges := [?]string {
		"int8_value",
		"packed_value",
		"float_value",
		"text_value",
		"numeric_text",
		"row",
		"values",
		"reference",
	}
	invalid_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form ||
		   (diagnostic.message != position_message && diagnostic.message != to_message) {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], invalid_ranges[invalid_count])
		invalid_count += 1
	}
	testing.expect_value(t, invalid_count, len(invalid_ranges))
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)
	unresolved_names := [?]string{"missing_position", "missing_value"}
	for name in unresolved_names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 1)
	}

	valid_stmt := file.root.stmts[12].derived_stmt.(^ast.Bit_Stmt)
	valid_position := valid_stmt.position.derived_expr.(^ast.Paren_Expr).expr.derived_expr.(^ast.Binary_Expr)
	checker_test_expect_expr_lhs(t, &checker, valid_position.left, false)
	checker_test_expect_expr_lhs(t, &checker, valid_stmt.target, true)
	checker_test_expect_expr_lhs(t, &checker, valid_stmt.value, false)
	for name in invalid_ranges {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}
