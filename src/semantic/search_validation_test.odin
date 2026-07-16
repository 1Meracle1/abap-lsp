package abap_frontend_semantic2

import "core:testing"
import ast "src:ast"

@(test)
search_requires_character_operands_and_type_i_positions :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA text TYPE string.
DATA fixed_text TYPE c LENGTH 10.
DATA bytes TYPE xstring.
DATA number TYPE i.
DATA int8_value TYPE int8.
DATA packed_value TYPE p DECIMALS 0.
DATA row TYPE sy.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.

SEARCH text FOR fixed_text STARTING AT number ENDING AT number.
SEARCH bytes FOR number STARTING AT int8_value ENDING AT packed_value.
SEARCH row FOR values STARTING AT reference ENDING AT text.
SEARCH missing_target FOR missing_pattern STARTING AT missing_start ENDING AT missing_end.`
	checker, file := checker_test_check_source(t, &project, source, "mem://search_operand_types.abap")

	messages := [?]string {
		"SEARCH target is not character-like",
		"SEARCH pattern is not character-like",
		"SEARCH STARTING AT operand does not have type i",
		"SEARCH ENDING AT operand does not have type i",
		"SEARCH target is not character-like",
		"SEARCH pattern is not character-like",
		"SEARCH STARTING AT operand does not have type i",
		"SEARCH ENDING AT operand does not have type i",
	}
	invalid_ranges := [?]string {
		"bytes", "number", "int8_value", "packed_value",
		"row", "values", "reference", "text",
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
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 4)
	unresolved_names := [?]string{"missing_target", "missing_pattern", "missing_start", "missing_end"}
	for name in unresolved_names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 1)
	}

	valid_stmt := file.root.stmts[9].derived_stmt.(^ast.Search_Stmt)
	checker_test_expect_expr_lhs(t, &checker, valid_stmt.target, false)
	checker_test_expect_expr_lhs(t, &checker, valid_stmt.pattern, false)
	checker_test_expect_expr_lhs(t, &checker, valid_stmt.starting_at, false)
	checker_test_expect_expr_lhs(t, &checker, valid_stmt.ending_at, false)
	for name in invalid_ranges {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}

@(test)
search_and_mark_requires_a_writable_target :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA text TYPE string.
CONSTANTS constant_text TYPE string VALUE 'text'.
SEARCH text FOR 'x' AND MARK.
SEARCH constant_text FOR 'x' AND MARK.
SEARCH 'literal' FOR 'x' AND MARK.`
	checker, file := checker_test_check_source(t, &project, source, "mem://search_mark_writable.abap")

	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "SEARCH target is not writable"), 2)
	invalid_ranges := [?]string{"constant_text", "'literal'"}
	invalid_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form || diagnostic.message != "SEARCH target is not writable" {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], invalid_ranges[invalid_count])
		invalid_count += 1
	}
	testing.expect_value(t, invalid_count, len(invalid_ranges))

	writable_stmt := file.root.stmts[2].derived_stmt.(^ast.Search_Stmt)
	checker_test_expect_expr_lhs(t, &checker, writable_stmt.target, true)
	checker_test_expect_expr_lhs(t, &checker, writable_stmt.pattern, false)
}
