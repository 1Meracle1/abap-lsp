package abap_frontend_semantic2

import "core:testing"
import ast "src:ast"

@(test)
set_cursor_field_requires_a_flat_character_like_value :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
  value TYPE c LENGTH 1,
END OF ty_row.
DATA character TYPE c LENGTH 20.
DATA numeric_text TYPE n LENGTH 8.
DATA date_value TYPE d.
DATA time_value TYPE t.
DATA integer TYPE i.
DATA text TYPE string.
DATA bytes TYPE x LENGTH 4.
DATA row TYPE ty_row.
DATA values TYPE STANDARD TABLE OF c WITH EMPTY KEY.
DATA reference TYPE REF TO c.
FIELD-SYMBOLS <generic> TYPE any.
SET CURSOR FIELD 'P_INPUT'.
SET CURSOR FIELD character.
SET CURSOR FIELD numeric_text.
SET CURSOR FIELD date_value.
SET CURSOR FIELD time_value.
SET CURSOR FIELD <generic>.
SET CURSOR FIELD integer.
SET CURSOR FIELD text.
SET CURSOR FIELD bytes.
SET CURSOR FIELD row.
SET CURSOR FIELD values.
SET CURSOR FIELD reference.
SET CURSOR FIELD missing_field.`
	checker, file := checker_test_check_source(t, &project, source, "mem://set_cursor_field_types.abap")

	message := "SET CURSOR FIELD operand is not flat character-like"
	invalid_ranges := [?]string{"integer", "text", "bytes", "row", "values", "reference"}
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, message),
		len(invalid_ranges),
	)
	invalid_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form || diagnostic.message != message {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], invalid_ranges[invalid_count])
		invalid_count += 1
	}
	testing.expect_value(t, invalid_count, len(invalid_ranges))
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_field"),
		1,
	)

	for name in invalid_ranges {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
	stmt := file.root.stmts[14].derived_stmt.(^ast.Set_Cursor_Stmt)
	checker_test_expect_expr_lhs(t, &checker, stmt.field, false)
}

@(test)
cursor_statements_validate_handle_and_package_directionality :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         value TYPE i,
       END OF ty_row.
CONSTANTS gc_cursor TYPE cursor VALUE IS INITIAL.
CONSTANTS gc_package_size TYPE i VALUE 10.
DATA cursor TYPE cursor.
DATA result TYPE i.

OPEN CURSOR @gc_cursor FOR SELECT value FROM ty_row.
OPEN CURSOR @cursor FOR SELECT value FROM ty_row.
FETCH NEXT CURSOR @cursor INTO @result PACKAGE SIZE gc_package_size.
CLOSE CURSOR @gc_cursor.`
	checker, file := checker_test_check_source(t, &project, source, "mem://cursor_directionality.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "OPEN CURSOR handle is not writable"),
		1,
	)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Open_Sql_Into_Target), 0)
	open_index := 0
	for stmt in file.root.stmts {
		#partial switch n in stmt.derived_stmt {
		case ^ast.Open_Cursor_Stmt:
			info, ok := checker_test_expr_info_for_node(t, &checker, &n.handle.expr_base)
			testing.expect(t, ok)
			if ok {
				expected := ast.Addressing_Mode.Constant if open_index == 0 else ast.Addressing_Mode.Variable
				testing.expect_value(t, info.mode, expected)
				testing.expect(t, info.is_lhs)
			}
			open_index += 1
		case ^ast.Fetch_Stmt:
			handle_info, handle_ok := checker_test_expr_info_for_node(t, &checker, &n.handle.expr_base)
			package_info, package_ok := checker_test_expr_info_for_node(t, &checker, &n.package_size.expr_base)
			testing.expect(t, handle_ok && package_ok)
			if handle_ok {
				testing.expect(t, !handle_info.is_lhs)
			}
			if package_ok {
				testing.expect_value(t, package_info.mode, ast.Addressing_Mode.Constant)
				testing.expect(t, !package_info.is_lhs)
			}
		case ^ast.Close_Cursor_Stmt:
			info, ok := checker_test_expr_info_for_node(t, &checker, &n.handle.expr_base)
			testing.expect(t, ok)
			if ok {
				testing.expect_value(t, info.mode, ast.Addressing_Mode.Constant)
				testing.expect(t, !info.is_lhs)
			}
		}
	}
}

@(test)
cursor_statements_report_unresolved_value_operands_and_candidates :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         value TYPE i,
       END OF ty_row.
DATA result TYPE i.

OPEN CURSOR @missing_open FOR SELECT value FROM ty_row WHERE value = @missing_query.
FETCH NEXT CURSOR @missing_fetch INTO @result PACKAGE SIZE missing_package.
CLOSE CURSOR @missing_close.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://cursor_unresolved.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 5)
	names := [?]string{"missing_open", "missing_query", "missing_fetch", "missing_package", "missing_close"}
	for name in names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 1)
	}
}

@(test)
set_cursor_positions_require_type_i :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         value TYPE i,
       END OF ty_row.
DATA integer TYPE i.
DATA int8_value TYPE int8.
DATA packed_value TYPE p DECIMALS 2.
DATA float_value TYPE f.
DATA text_value TYPE string.
DATA numeric_text TYPE n LENGTH 4.
DATA row TYPE ty_row.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.

SET CURSOR FIELD 'P_INPUT' OFFSET 1.
SET CURSOR ( integer + 1 ) integer.
SET CURSOR FIELD 'P_INPUT' OFFSET int8_value.
SET CURSOR packed_value float_value.
SET CURSOR text_value numeric_text.
SET CURSOR row values.
SET CURSOR FIELD 'P_INPUT' OFFSET reference.
SET CURSOR missing_line missing_column.`
	checker, file := checker_test_check_source(t, &project, source, "mem://set_cursor_position_types.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"SET CURSOR OFFSET operand does not have type i",
		),
		2,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"SET CURSOR LINE operand does not have type i",
		),
		3,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"SET CURSOR COLUMN operand does not have type i",
		),
		3,
	)
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
		   (diagnostic.message != "SET CURSOR OFFSET operand does not have type i" &&
		    diagnostic.message != "SET CURSOR LINE operand does not have type i" &&
		    diagnostic.message != "SET CURSOR COLUMN operand does not have type i") {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], invalid_ranges[invalid_count])
		invalid_count += 1
	}
	testing.expect_value(t, invalid_count, len(invalid_ranges))
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)
	unresolved_names := [?]string{"missing_line", "missing_column"}
	for name in unresolved_names {
		testing.expect_value(
			t,
			checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name),
			1,
		)
	}

	valid_stmt := file.root.stmts[11].derived_stmt.(^ast.Set_Cursor_Stmt)
	valid_expr := valid_stmt.line.derived_expr.(^ast.Paren_Expr).expr.derived_expr.(^ast.Binary_Expr)
	checker_test_expect_expr_lhs(t, &checker, valid_expr.left, false)
	for name in invalid_ranges {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}

@(test)
fetch_package_size_requires_an_integer_row_count :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         value TYPE i,
       END OF ty_row.
DATA cursor TYPE cursor.
DATA result TYPE i.
DATA row TYPE ty_row.
DATA int1_size TYPE int1.
DATA int2_size TYPE int2.
DATA int8_size TYPE int8.
DATA packed_size TYPE p DECIMALS 2.
DATA float_size TYPE f.
DATA text_size TYPE string.
DATA numeric_text_size TYPE n LENGTH 4.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.

OPEN CURSOR @cursor FOR SELECT value FROM ty_row.
FETCH NEXT CURSOR @cursor INTO @result PACKAGE SIZE 10.
FETCH NEXT CURSOR @cursor INTO @result PACKAGE SIZE ( int1_size + int2_size ).
FETCH NEXT CURSOR @cursor INTO @result PACKAGE SIZE int8_size.
FETCH NEXT CURSOR @cursor INTO @result PACKAGE SIZE packed_size.
FETCH NEXT CURSOR @cursor INTO @result PACKAGE SIZE float_size.
FETCH NEXT CURSOR @cursor INTO @result PACKAGE SIZE text_size.
FETCH NEXT CURSOR @cursor INTO @result PACKAGE SIZE numeric_text_size.
FETCH NEXT CURSOR @cursor INTO @result PACKAGE SIZE row.
FETCH NEXT CURSOR @cursor INTO @result PACKAGE SIZE values.
FETCH NEXT CURSOR @cursor INTO @result PACKAGE SIZE reference.
FETCH NEXT CURSOR @cursor INTO @result PACKAGE SIZE missing_size.`
	checker, file := checker_test_check_source(t, &project, source, "mem://fetch_package_size_types.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"FETCH PACKAGE SIZE operand is not an integer row count",
		),
		7,
	)
	invalid_ranges := [?]string {
		"packed_size",
		"float_size",
		"text_size",
		"numeric_text_size",
		"row",
		"values",
		"reference",
	}
	invalid_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form ||
		   diagnostic.message != "FETCH PACKAGE SIZE operand is not an integer row count" {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], invalid_ranges[invalid_count])
		invalid_count += 1
	}
	testing.expect_value(t, invalid_count, len(invalid_ranges))
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_size"),
		1,
	)

	expression_stmt := file.root.stmts[15].derived_stmt.(^ast.Fetch_Stmt)
	expression := expression_stmt.package_size.derived_expr.(^ast.Paren_Expr).expr.derived_expr.(^ast.Binary_Expr)
	checker_test_expect_expr_lhs(t, &checker, expression.left, false)
	checker_test_expect_expr_lhs(t, &checker, expression.right, false)
	used_names := [?]string {
		"int1_size",
		"int2_size",
		"int8_size",
		"packed_size",
		"float_size",
		"text_size",
		"numeric_text_size",
		"row",
		"values",
		"reference",
	}
	for name in used_names {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}

@(test)
cursor_inline_handle_and_fetch_target_preserve_query_shape :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         value TYPE i,
       END OF ty_row.

OPEN CURSOR @DATA(cursor) FOR SELECT value FROM ty_row.
FETCH NEXT CURSOR @cursor INTO @DATA(result).
result = 1.
CLOSE CURSOR @cursor.`
	checker, file := checker_test_check_source(t, &project, source, "mem://cursor_inline.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 0)
	cursor := checker_test_lookup(t, &project, file.root_scope, .Value, "cursor", .Variable)
	testing.expect(t, cursor != nil && cursor.type != nil)
	if cursor != nil && cursor.type != nil {
		testing.expect_value(t, checker_test_type_name(&project, cursor.type), "cursor")
	}
	result := checker_test_lookup(t, &project, file.root_scope, .Value, "result", .Variable)
	testing.expect(t, result != nil && result.type != nil)
	if result != nil && result.type != nil {
		testing.expect_value(t, checker_test_type_name(&project, result.type), "i")
	}
}
