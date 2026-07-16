package abap_frontend_semantic2

import "core:testing"
import ast "src:ast"

@(test)
textpool_operands_require_character_values_and_textpool_table_rows :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_textpool,
         id TYPE c,
         key TYPE c,
         entry TYPE c,
         length TYPE i,
       END OF ty_textpool,
       BEGIN OF ty_wrong_row,
         id TYPE c,
         key TYPE c,
         entry TYPE c,
         value TYPE i,
       END OF ty_wrong_row.
DATA valid_pool TYPE STANDARD TABLE OF ty_textpool WITH EMPTY KEY.
DATA builtin_pool TYPE textpool_table.
DATA wrong_pool TYPE STANDARD TABLE OF ty_wrong_row WITH EMPTY KEY.
DATA scalar_pool TYPE string.
DATA integer_rows TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA number TYPE i.
DATA bytes TYPE xstring.
DATA row TYPE ty_textpool.
DATA reference TYPE REF TO i.

READ TEXTPOOL 'ZPROGRAM' INTO valid_pool LANGUAGE sy-langu.
INSERT TEXTPOOL 'ZPROGRAM' FROM builtin_pool LANGUAGE 'E'.
READ TEXTPOOL number INTO scalar_pool LANGUAGE bytes.
INSERT TEXTPOOL row FROM wrong_pool LANGUAGE reference.
READ TEXTPOOL bytes INTO integer_rows LANGUAGE number.
INSERT TEXTPOOL missing_program FROM missing_pool LANGUAGE missing_language.`
	checker, file := checker_test_check_source(t, &project, source, "mem://textpool_operand_types.abap")

	program_message := "TEXTPOOL program operand is not flat character-like"
	language_message := "TEXTPOOL LANGUAGE operand is not flat character-like"
	table_message := "TEXTPOOL table operand is not an internal table"
	row_message := "TEXTPOOL table row does not correspond to TEXTPOOL"
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, program_message), 3)
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, language_message), 3)
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, table_message), 1)
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, row_message), 2)
	invalid_ranges := [?]string {
		"number", "bytes", "scalar_pool",
		"row", "reference", "wrong_pool",
		"bytes", "number", "integer_rows",
	}
	invalid_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form || diagnostic.message == "READ TEXTPOOL target is not writable" {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], invalid_ranges[invalid_count])
		invalid_count += 1
	}
	testing.expect_value(t, invalid_count, len(invalid_ranges))
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 3)
	unresolved_names := [?]string{"missing_program", "missing_pool", "missing_language"}
	for name in unresolved_names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 1)
	}

	read_stmt := file.root.stmts[10].derived_stmt.(^ast.Textpool_Stmt)
	checker_test_expect_expr_lhs(t, &checker, read_stmt.program, false)
	checker_test_expect_expr_lhs(t, &checker, read_stmt.table, true)
	checker_test_expect_expr_lhs(t, &checker, read_stmt.language, false)
	insert_stmt := file.root.stmts[11].derived_stmt.(^ast.Textpool_Stmt)
	checker_test_expect_expr_lhs(t, &checker, insert_stmt.program, false)
	checker_test_expect_expr_lhs(t, &checker, insert_stmt.table, false)
	checker_test_expect_expr_lhs(t, &checker, insert_stmt.language, false)
	used_names := [?]string{"valid_pool", "builtin_pool", "wrong_pool", "scalar_pool", "integer_rows", "number", "bytes", "row", "reference"}
	for name in used_names {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}

@(test)
read_textpool_table_must_be_writable_without_double_checking_it :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `CONSTANTS pool TYPE textpool_table VALUE IS INITIAL.
READ TEXTPOOL 'ZPROGRAM' INTO pool LANGUAGE 'E'.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://read_textpool_writable.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Syntax_Form), 1)
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "READ TEXTPOOL target is not writable"), 1)
}
