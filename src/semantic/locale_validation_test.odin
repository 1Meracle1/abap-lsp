package abap_frontend_semantic2

import "core:testing"
import ast "src:ast"

@(test)
set_locale_operands_require_character_like_values :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         value TYPE i,
       END OF ty_row.
DATA language TYPE c LENGTH 1.
DATA country TYPE string.
DATA modifier TYPE n LENGTH 8.
DATA date_value TYPE d.
DATA time_value TYPE t.
DATA integer TYPE i.
DATA packed_value TYPE p DECIMALS 2.
DATA bytes TYPE x LENGTH 3.
DATA byte_string TYPE xstring.
DATA row TYPE ty_row.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.

SET LOCALE LANGUAGE language COUNTRY country MODIFIER modifier.
SET LOCALE LANGUAGE 'E' COUNTRY date_value MODIFIER time_value.
SET LOCALE LANGUAGE integer COUNTRY packed_value MODIFIER bytes.
SET LOCALE LANGUAGE byte_string COUNTRY row MODIFIER values.
SET LOCALE LANGUAGE reference COUNTRY missing_country MODIFIER missing_modifier.`
	checker, file := checker_test_check_source(t, &project, source, "mem://set_locale_operand_types.abap")

	language_message := "SET LOCALE LANGUAGE operand is not character-like"
	country_message := "SET LOCALE COUNTRY operand is not character-like"
	modifier_message := "SET LOCALE MODIFIER operand is not character-like"
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, language_message), 3)
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, country_message), 2)
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, modifier_message), 2)
	invalid_ranges := [?]string {
		"integer",
		"packed_value",
		"bytes",
		"byte_string",
		"row",
		"values",
		"reference",
	}
	invalid_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form ||
		   (diagnostic.message != language_message &&
		    diagnostic.message != country_message &&
		    diagnostic.message != modifier_message) {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], invalid_ranges[invalid_count])
		invalid_count += 1
	}
	testing.expect_value(t, invalid_count, len(invalid_ranges))
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)
	unresolved_names := [?]string{"missing_country", "missing_modifier"}
	for name in unresolved_names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 1)
	}

	valid_stmt := file.root.stmts[14].derived_stmt.(^ast.Locale_Stmt)
	checker_test_expect_expr_lhs(t, &checker, valid_stmt.language, false)
	checker_test_expect_expr_lhs(t, &checker, valid_stmt.country, false)
	checker_test_expect_expr_lhs(t, &checker, valid_stmt.modifier, false)
	for name in invalid_ranges {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}
