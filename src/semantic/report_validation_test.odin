package abap_frontend_semantic2

import "core:testing"
import ast "src:ast"

@(test)
read_and_insert_report_require_character_programs_and_standard_character_tables :: proc(t: ^testing.T) {
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
DATA number TYPE i.
DATA bytes TYPE xstring.
DATA reference TYPE REF TO i.

READ REPORT 'ZVALID' INTO valid_source.
INSERT REPORT sy-repid FROM fixed_source.
READ REPORT number INTO scalar_source.
INSERT REPORT bytes FROM sorted_source.
READ REPORT reference INTO hashed_source.
INSERT REPORT 'ZSECONDARY' FROM secondary_source.
READ REPORT 'ZROWS' INTO integer_source.
INSERT REPORT missing_program FROM missing_source.`
	checker, file := checker_test_check_source(t, &project, source, "mem://report_operand_types.abap")

	program_message := "REPORT program operand is not flat character-like"
	table_message := "REPORT source operand is not an internal table"
	standard_message := "REPORT source operand is not a standard table"
	secondary_message := "REPORT source table has secondary keys"
	row_message := "REPORT source table row is not character-like"
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, program_message), 3)
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, table_message), 1)
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, standard_message), 2)
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, secondary_message), 1)
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, row_message), 1)

	invalid_ranges := [?]string {
		"number", "scalar_source",
		"bytes", "sorted_source",
		"reference", "hashed_source",
		"secondary_source", "integer_source",
	}
	invalid_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], invalid_ranges[invalid_count])
		invalid_count += 1
	}
	testing.expect_value(t, invalid_count, len(invalid_ranges))
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_program"), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_source"), 1)

	read_stmt := file.root.stmts[11].derived_stmt.(^ast.Report_Stmt)
	checker_test_expect_expr_lhs(t, &checker, read_stmt.name, false)
	checker_test_expect_expr_lhs(t, &checker, read_stmt.source, true)
	insert_stmt := file.root.stmts[12].derived_stmt.(^ast.Report_Stmt)
	checker_test_expect_expr_lhs(t, &checker, insert_stmt.name, false)
	checker_test_expect_expr_lhs(t, &checker, insert_stmt.source, false)
	used_names := [?]string {
		"valid_source", "fixed_source", "secondary_source", "sorted_source",
		"hashed_source", "integer_source", "scalar_source", "number", "bytes", "reference",
	}
	for name in used_names {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}

@(test)
read_report_source_must_be_writable_without_changing_other_report_forms :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `REPORT zmain.
CONSTANTS source TYPE string_table VALUE IS INITIAL.
READ REPORT 'ZPROGRAM' INTO source.
DELETE REPORT missing_program.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://read_report_writable.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Syntax_Form), 1)
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "READ REPORT target is not writable"), 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 0)
}
