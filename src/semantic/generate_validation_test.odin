package abap_frontend_semantic2

import "core:testing"
import ast "src:ast"

@(test)
generate_subroutine_pool_requires_character_source_rows_and_typed_outputs :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES ty_secondary TYPE STANDARD TABLE OF string
        WITH EMPTY KEY
        WITH NON-UNIQUE SORTED KEY by_line COMPONENTS table_line.
DATA valid_source TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA fixed_source TYPE STANDARD TABLE OF c WITH EMPTY KEY.
DATA secondary_source TYPE ty_secondary.
DATA sorted_source TYPE SORTED TABLE OF string WITH NON-UNIQUE KEY table_line.
DATA hashed_source TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
DATA integer_source TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA scalar_source TYPE string.
DATA text TYPE string.
DATA number TYPE i.
DATA packed TYPE p DECIMALS 0.
DATA bytes TYPE xstring.
DATA reference TYPE REF TO i.

GENERATE SUBROUTINE POOL valid_source NAME text MESSAGE text LINE number WORD text OFFSET number.
GENERATE SUBROUTINE POOL fixed_source NAME text.
GENERATE SUBROUTINE POOL scalar_source NAME number MESSAGE bytes LINE text WORD reference OFFSET packed.
GENERATE SUBROUTINE POOL sorted_source NAME text.
GENERATE SUBROUTINE POOL hashed_source NAME text.
GENERATE SUBROUTINE POOL secondary_source NAME text.
GENERATE SUBROUTINE POOL integer_source NAME text.
GENERATE SUBROUTINE POOL missing_source NAME missing_name MESSAGE missing_message LINE missing_line WORD missing_word OFFSET missing_offset.`
	checker, file := checker_test_check_source(t, &project, source, "mem://generate_operand_types.abap")

	messages := [?]string {
		"GENERATE source operand is not an internal table",
		"GENERATE NAME target is not character-like",
		"GENERATE MESSAGE target is not character-like",
		"GENERATE WORD target is not character-like",
		"GENERATE LINE target does not have type i",
		"GENERATE OFFSET target does not have type i",
		"GENERATE source operand is not a standard table",
		"GENERATE source operand is not a standard table",
		"GENERATE source table has secondary keys",
		"GENERATE source table row is not character-like",
	}
	invalid_ranges := [?]string {
		"scalar_source", "number", "bytes", "reference", "text", "packed",
		"sorted_source", "hashed_source", "secondary_source", "integer_source",
	}
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
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 6)
	unresolved_names := [?]string{"missing_source", "missing_name", "missing_message", "missing_line", "missing_word", "missing_offset"}
	for name in unresolved_names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 1)
	}

	valid_stmt := file.root.stmts[13].derived_stmt.(^ast.Generate_Stmt)
	checker_test_expect_expr_lhs(t, &checker, valid_stmt.source, false)
	checker_test_expect_expr_lhs(t, &checker, valid_stmt.name, true)
	checker_test_expect_expr_lhs(t, &checker, valid_stmt.message, true)
	checker_test_expect_expr_lhs(t, &checker, valid_stmt.line, true)
	checker_test_expect_expr_lhs(t, &checker, valid_stmt.word, true)
	checker_test_expect_expr_lhs(t, &checker, valid_stmt.offset, true)
	used_names := [?]string {
		"valid_source", "fixed_source", "secondary_source", "sorted_source", "hashed_source",
		"integer_source", "scalar_source", "text", "number", "packed", "bytes", "reference",
	}
	for name in used_names {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}

@(test)
generate_subroutine_pool_outputs_must_be_writable_without_affecting_dynpro :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA source TYPE STANDARD TABLE OF string WITH EMPTY KEY.
CONSTANTS text TYPE string VALUE ''.
CONSTANTS number TYPE i VALUE 0.
GENERATE SUBROUTINE POOL source NAME text MESSAGE text LINE number WORD text OFFSET number.
GENERATE DYNPRO missing_program missing_dynpro.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://generate_writable.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Syntax_Form), 5)
	messages := [?]string {
		"GENERATE NAME target is not writable",
		"GENERATE MESSAGE target is not writable",
		"GENERATE LINE target is not writable",
		"GENERATE WORD target is not writable",
		"GENERATE OFFSET target is not writable",
	}
	for message in messages {
		testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, message), 1)
	}
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)
}
