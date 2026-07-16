package abap_frontend_semantic2

import "core:testing"
import "core:strings"
import ast "src:ast"

@(test)
open_sql_is_null_subjects_are_scalar_and_checked_once :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         amount TYPE i,
         text TYPE string,
         values TYPE STANDARD TABLE OF i WITH EMPTY KEY,
         reference TYPE REF TO i,
       END OF ty_row.
DATA row TYPE ty_row.
DATA number TYPE i.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.

SELECT amount FROM ty_row INTO TABLE @DATA(valid_column_rows) WHERE amount IS NULL.
SELECT amount FROM ty_row INTO TABLE @DATA(valid_expression_rows) WHERE amount + @number IS NOT NULL.
SELECT amount FROM ty_row INTO TABLE @DATA(invalid_column_table_rows) WHERE values IS NULL.
SELECT amount FROM ty_row INTO TABLE @DATA(invalid_column_reference_rows) WHERE reference IS NOT NULL.
SELECT amount FROM ty_row INTO TABLE @DATA(invalid_host_structure_rows) WHERE @row IS NULL.
SELECT amount FROM ty_row INTO TABLE @DATA(invalid_host_table_rows) WHERE @values IS NOT NULL.
SELECT amount FROM ty_row INTO TABLE @DATA(invalid_host_reference_rows) WHERE @reference IS NULL.
SELECT amount FROM ty_row INTO TABLE @DATA(unknown_column_rows) WHERE missing_value IS NULL.
SELECT amount FROM ty_row INTO TABLE @DATA(unknown_host_rows) WHERE @missing_value IS NOT NULL.`
	checker, file := checker_test_check_source(t, &project, source, "mem://sql_is_null_subject_types.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "SQL IS NULL subject is not scalar"),
		5,
	)
	invalid_column_values, invalid_column_reference := 0, 0
	invalid_row, invalid_values, invalid_reference := 0, 0, 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form || diagnostic.message != "SQL IS NULL subject is not scalar" {
			continue
		}
		operand_text := source[diagnostic.range.start:diagnostic.range.end]
		switch operand_text {
		case "values":
			invalid_column_values += 1
		case "reference":
			invalid_column_reference += 1
		case "@row":
			invalid_row += 1
		case "@values":
			invalid_values += 1
		case "@reference":
			invalid_reference += 1
		}
	}
	testing.expect_value(t, invalid_column_values, 1)
	testing.expect_value(t, invalid_column_reference, 1)
	testing.expect_value(t, invalid_row, 1)
	testing.expect_value(t, invalid_values, 1)
	testing.expect_value(t, invalid_reference, 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_value"),
		1,
	)
	valid_predicate := file.root.stmts[6].derived_stmt.(^ast.Select_Stmt).query.where_cond.derived_expr.(^ast.Is_Predicate_Expr)
	valid_expression := valid_predicate.subject.derived_expr.(^ast.Binary_Expr)
	checker_test_expect_expr_lhs(t, &checker, valid_expression.right, false)
	unknown_host := file.root.stmts[13].derived_stmt.(^ast.Select_Stmt).query.where_cond.derived_expr.(^ast.Is_Predicate_Expr)
	checker_test_expect_expr_lhs(t, &checker, unknown_host.subject, false)
	number := checker_test_lookup(t, &project, file.root_scope, .Value, "number", .Variable)
	row := checker_test_lookup(t, &project, file.root_scope, .Value, "row", .Variable)
	values := checker_test_lookup(t, &project, file.root_scope, .Value, "values", .Variable)
	reference := checker_test_lookup(t, &project, file.root_scope, .Value, "reference", .Variable)
	testing.expect(t, number != nil && .Used in number.flags)
	testing.expect(t, row != nil && .Used in row.flags)
	testing.expect(t, values != nil && .Used in values.flags)
	testing.expect(t, reference != nil && .Used in reference.flags)
}

@(test)
open_sql_arithmetic_operands_are_numeric_and_results_keep_value_types :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         amount TYPE i,
         text TYPE string,
		 values TYPE STANDARD TABLE OF i WITH EMPTY KEY,
		 reference TYPE REF TO i,
       END OF ty_row.
DATA number TYPE i.

SELECT amount + 1 AS result FROM ty_row INTO TABLE @DATA(add_rows) WHERE amount + @number = 2.
SELECT amount - 1 AS result FROM ty_row INTO TABLE @DATA(subtract_rows).
SELECT amount * 2 AS result FROM ty_row INTO TABLE @DATA(multiply_rows).
SELECT amount / 2 AS result FROM ty_row INTO TABLE @DATA(divide_rows).
SELECT amount DIV 2 AS result FROM ty_row INTO TABLE @DATA(integer_divide_rows).
SELECT amount MOD 2 AS result FROM ty_row INTO TABLE @DATA(modulo_rows).
SELECT +amount AS result FROM ty_row INTO TABLE @DATA(unary_plus_rows).
SELECT -amount AS result FROM ty_row INTO TABLE @DATA(unary_minus_rows).
SELECT -missing_unary AS result FROM ty_row INTO TABLE @DATA(unknown_unary_rows).
SELECT missing_left + amount AS result FROM ty_row INTO TABLE @DATA(unknown_rows).
SELECT text + values AS result FROM ty_row INTO TABLE @DATA(invalid_rows).
SELECT reference * text AS result FROM ty_row INTO TABLE @DATA(more_invalid_rows).
SELECT +text AS result FROM ty_row INTO TABLE @DATA(invalid_unary_text_rows).
SELECT -values AS result FROM ty_row INTO TABLE @DATA(invalid_unary_table_rows).
SELECT -reference AS result FROM ty_row INTO TABLE @DATA(invalid_unary_reference_rows).`
	checker, file := checker_test_check_source(t, &project, source, "mem://sql_arithmetic_operand_types.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "SQL arithmetic operand is not numeric"),
		7,
	)
	invalid_text, invalid_values, invalid_reference := 0, 0, 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form || diagnostic.message != "SQL arithmetic operand is not numeric" {
			continue
		}
		switch source[diagnostic.range.start:diagnostic.range.end] {
		case "text":
			invalid_text += 1
		case "values":
			invalid_values += 1
		case "reference":
			invalid_reference += 1
		}
	}
	testing.expect_value(t, invalid_text, 3)
	testing.expect_value(t, invalid_values, 2)
	testing.expect_value(t, invalid_reference, 2)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 2)
	for stmt_index in 2 ..= 9 {
		stmt := file.root.stmts[stmt_index].derived_stmt.(^ast.Select_Stmt)
		testing.expect(t, !stmt.query.projection_clauses[0].is_dynamic)
		result_name := stmt.query.result.target.derived_expr.(^ast.Host_Expr).value.derived_expr.(^ast.Data_Inline_Name_Expr).name.text
		result := checker_test_lookup(t, &project, file.root_scope, .Value, result_name, .Variable)
		row_type := checker_type_row(&checker.builtin_context, result.type)
		structure := checker_type_structure(row_type)
		testing.expect(t, structure != nil && len(structure.fields) == 1)
		field_name, ok := checker_type_builtin_name(&checker.builtin_context, structure.fields[0].type)
		testing.expect(t, ok)
		testing.expect_value(t, field_name, "i")
	}
	number := checker_test_lookup(t, &project, file.root_scope, .Value, "number", .Variable)
	testing.expect(t, number != nil && .Used in number.flags)
	ty_row := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_row", .Type_Def)
	structure := checker_type_structure(ty_row.type)
	field_names := [?]string{"amount", "text", "values", "reference"}
	for name in field_names {
		field := checker_test_structure_field(t, &project, structure, name)
		testing.expect(t, field != nil && .Used in field.flags)
	}
}

@(test)
open_sql_concatenation_operands_are_character_like_and_result_is_string :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         text TYPE string,
         code TYPE c LENGTH 4,
         amount TYPE i,
         values TYPE STANDARD TABLE OF i WITH EMPTY KEY,
         reference TYPE REF TO i,
       END OF ty_row.
DATA suffix TYPE string.

SELECT text && code AS result FROM ty_row INTO TABLE @DATA(valid_rows) WHERE text && @suffix = 'ready'.
SELECT text && missing_text AS result FROM ty_row INTO TABLE @DATA(unknown_rows).
SELECT amount && text AS result FROM ty_row INTO TABLE @DATA(invalid_number_rows).
SELECT values && reference AS result FROM ty_row INTO TABLE @DATA(invalid_complex_rows).`
	checker, file := checker_test_check_source(t, &project, source, "mem://sql_concatenation_operand_types.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"SQL concatenation operand is not character-like",
		),
		3,
	)
	invalid_amount, invalid_values, invalid_reference := 0, 0, 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form ||
		   diagnostic.message != "SQL concatenation operand is not character-like" {
			continue
		}
		switch source[diagnostic.range.start:diagnostic.range.end] {
		case "amount":
			invalid_amount += 1
		case "values":
			invalid_values += 1
		case "reference":
			invalid_reference += 1
		}
	}
	testing.expect_value(t, invalid_amount, 1)
	testing.expect_value(t, invalid_values, 1)
	testing.expect_value(t, invalid_reference, 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 1)
	for stmt_index in 2 ..= 5 {
		stmt := file.root.stmts[stmt_index].derived_stmt.(^ast.Select_Stmt)
		result_name := stmt.query.result.target.derived_expr.(^ast.Host_Expr).value.derived_expr.(^ast.Data_Inline_Name_Expr).name.text
		result := checker_test_lookup(t, &project, file.root_scope, .Value, result_name, .Variable)
		row_type := checker_type_row(&checker.builtin_context, result.type)
		structure := checker_type_structure(row_type)
		testing.expect(t, structure != nil && len(structure.fields) == 1)
		field_name, ok := checker_type_builtin_name(&checker.builtin_context, structure.fields[0].type)
		testing.expect(t, ok)
		testing.expect_value(t, field_name, "string")
	}
	suffix := checker_test_lookup(t, &project, file.root_scope, .Value, "suffix", .Variable)
	testing.expect(t, suffix != nil && .Used in suffix.flags)
	ty_row := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_row", .Type_Def)
	structure := checker_type_structure(ty_row.type)
	field_names := [?]string{"text", "code", "amount", "values", "reference"}
	for name in field_names {
		field := checker_test_structure_field(t, &project, structure, name)
		testing.expect(t, field != nil && .Used in field.flags)
	}
}

@(test)
open_sql_like_operands_are_character_like_and_checked_once :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         text TYPE string,
         code TYPE c LENGTH 4,
         amount TYPE i,
         values TYPE STANDARD TABLE OF i WITH EMPTY KEY,
         reference TYPE REF TO i,
       END OF ty_row.
DATA pattern TYPE string.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.

SELECT text FROM ty_row INTO TABLE @DATA(valid_rows) WHERE text LIKE @pattern.
SELECT text FROM ty_row INTO TABLE @DATA(valid_not_rows) WHERE code NOT LIKE 'A%'.
SELECT text FROM ty_row INTO TABLE @DATA(invalid_subject_rows) WHERE amount LIKE @pattern.
SELECT text FROM ty_row INTO TABLE @DATA(invalid_pattern_rows) WHERE text NOT LIKE @values.
SELECT text FROM ty_row INTO TABLE @DATA(invalid_complex_rows) WHERE values LIKE @reference.
SELECT text FROM ty_row INTO TABLE @DATA(unknown_column_rows) WHERE missing_text LIKE @pattern.
SELECT text FROM ty_row INTO TABLE @DATA(unknown_host_rows) WHERE text LIKE @missing_pattern.`
	checker, file := checker_test_check_source(t, &project, source, "mem://sql_like_operand_types.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"SQL LIKE operand is not character-like",
		),
		4,
	)
	invalid_amount, invalid_values, invalid_reference := 0, 0, 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form ||
		   diagnostic.message != "SQL LIKE operand is not character-like" {
			continue
		}
		switch source[diagnostic.range.start:diagnostic.range.end] {
		case "amount":
			invalid_amount += 1
		case "@values", "values":
			invalid_values += 1
		case "@reference":
			invalid_reference += 1
		}
	}
	testing.expect_value(t, invalid_amount, 1)
	testing.expect_value(t, invalid_values, 2)
	testing.expect_value(t, invalid_reference, 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_pattern"),
		1,
	)
	host_like := file.root.stmts[10].derived_stmt.(^ast.Select_Stmt).query.where_cond.derived_expr.(^ast.Binary_Expr)
	checker_test_expect_expr_lhs(t, &checker, host_like.right, false)
	pattern := checker_test_lookup(t, &project, file.root_scope, .Value, "pattern", .Variable)
	values := checker_test_lookup(t, &project, file.root_scope, .Value, "values", .Variable)
	reference := checker_test_lookup(t, &project, file.root_scope, .Value, "reference", .Variable)
	testing.expect(t, pattern != nil && .Used in pattern.flags)
	testing.expect(t, values != nil && .Used in values.flags)
	testing.expect(t, reference != nil && .Used in reference.flags)
}

@(test)
open_sql_in_collections_are_tables_with_compatible_rows_and_checked_once :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         amount TYPE i,
         text TYPE string,
	       END OF ty_row.
TYPES ty_amounts TYPE STANDARD TABLE OF i WITH EMPTY KEY.
TYPES ty_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
TYPES: BEGIN OF ty_range,
         sign TYPE c LENGTH 1,
         option TYPE c LENGTH 2,
	         low TYPE i,
	         high TYPE i,
	       END OF ty_range.
TYPES ty_ranges TYPE STANDARD TABLE OF ty_range WITH EMPTY KEY.
DATA amounts TYPE ty_amounts.
DATA rows TYPE ty_rows.
DATA ranges TYPE ty_ranges.
DATA row TYPE ty_row.
DATA amount TYPE i.
DATA reference TYPE REF TO i.
DATA text TYPE string.
DATA valid_range_rows TYPE ty_amounts.

SELECT amount FROM ty_row INTO TABLE @DATA(valid_rows) WHERE amount IN @amounts.
SELECT amount FROM ty_row INTO TABLE valid_range_rows WHERE amount NOT IN ranges.
SELECT amount FROM ty_row INTO TABLE @DATA(invalid_scalar_rows) WHERE amount IN @amount.
SELECT amount FROM ty_row INTO TABLE @DATA(invalid_structure_rows) WHERE amount NOT IN @row.
SELECT amount FROM ty_row INTO TABLE @DATA(invalid_reference_rows) WHERE amount IN @reference.
SELECT amount FROM ty_row INTO TABLE @DATA(invalid_item_rows) WHERE amount IN @rows.
SELECT amount FROM ty_row INTO TABLE @DATA(raw_list_rows) WHERE amount IN ( 1, @amount, @text ).
SELECT amount FROM ty_row INTO TABLE @DATA(unknown_rows) WHERE amount IN @missing_ranges.`
	checker, file := checker_test_check_source(t, &project, source, "mem://sql_in_collection_types.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"SQL IN operand is not an internal table",
		),
		3,
	)
	invalid_ranges := map[string]int{}
	defer delete(invalid_ranges)
	for diagnostic in checker.info.diagnostics {
		if diagnostic.message == "SQL IN operand is not an internal table" ||
		   strings.has_prefix(diagnostic.message, "SQL IN table row is not compatible") {
			invalid_ranges[source[diagnostic.range.start:diagnostic.range.end]] += 1
		}
	}
	testing.expect_value(t, invalid_ranges["@amount"], 1)
	testing.expect_value(t, invalid_ranges["@row"], 1)
	testing.expect_value(t, invalid_ranges["@reference"], 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_ranges"),
		1,
	)
	unknown := file.root.stmts[len(file.root.stmts) - 1].derived_stmt.(^ast.Select_Stmt).query.where_cond.derived_expr.(^ast.Binary_Expr)
	checker_test_expect_expr_lhs(t, &checker, unknown.right, false)
	used_names := [?]string{"amounts", "rows", "ranges", "row", "amount", "reference", "text"}
	for name in used_names {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}

@(test)
open_sql_output_targets_must_be_writable :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         amount TYPE i,
       END OF ty_row.
CONSTANTS gc_value TYPE i VALUE 0.
DATA value TYPE i.
DATA cursor TYPE cursor.

SELECT SINGLE value FROM ty_row INTO @gc_value.
SELECT SINGLE value FROM ty_row INTO @value.
OPEN CURSOR @cursor FOR SELECT value FROM ty_row.
FETCH NEXT CURSOR @cursor INTO @gc_value.
FETCH NEXT CURSOR @cursor INTO @value.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://open_sql_writable.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Open_Sql_Into_Target, "Open SQL target is not writable"),
		2,
	)
}

@(test)
open_sql_searched_case_conditions_are_logical_and_checked_once :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         ready TYPE abap_bool,
         value TYPE i,
       END OF ty_row.
DATA number TYPE i.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.
DATA ready TYPE abap_bool.

SELECT CASE WHEN @ready THEN @number ELSE 0 END AS result FROM ty_row INTO TABLE @DATA(valid_rows).
SELECT CASE WHEN @number THEN value ELSE 0 END AS result FROM ty_row INTO TABLE @DATA(number_rows).
SELECT CASE WHEN @values THEN value ELSE 0 END AS result FROM ty_row INTO TABLE @DATA(table_rows).
SELECT CASE WHEN @reference THEN value ELSE 0 END AS result FROM ty_row INTO TABLE @DATA(reference_rows).
SELECT CASE WHEN @missing_condition THEN value ELSE 0 END AS result FROM ty_row INTO TABLE @DATA(unknown_rows).`
	checker, file := checker_test_check_source(t, &project, source, "mem://sql_searched_case_conditions.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "SQL CASE WHEN condition is not logical"),
		3,
	)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_condition"),
		1,
	)
	valid_case := file.root.stmts[5].derived_stmt.(^ast.Select_Stmt).query.projection_clauses[0].value.derived_expr.(^ast.Sql_Case_Expr)
	valid_when := valid_case.whens[0].derived_expr.(^ast.Sql_Case_When_Expr)
	checker_test_expect_expr_lhs(t, &checker, valid_when.condition, false)
	checker_test_expect_expr_lhs(t, &checker, valid_when.result, false)
	checker_test_expect_expr_lhs(t, &checker, valid_case.else_expr, false)
	ready := checker_test_lookup(t, &project, file.root_scope, .Value, "ready", .Variable)
	value := checker_test_lookup(t, &project, file.root_scope, .Value, "number", .Variable)
	testing.expect(t, ready != nil && .Used in ready.flags)
	testing.expect(t, value != nil && .Used in value.flags)
}

@(test)
open_sql_simple_case_arms_are_values_and_case_results_are_compatible :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         value TYPE i,
       END OF ty_row.
DATA number TYPE i.
DATA reference TYPE REF TO i.

SELECT CASE value WHEN @number THEN value ELSE 0 END AS result FROM ty_row INTO TABLE @DATA(simple_rows).
SELECT CASE WHEN value = 0 THEN @number WHEN value = 1 THEN @reference ELSE 0 END AS result FROM ty_row INTO TABLE @DATA(incompatible_when_rows).
SELECT CASE WHEN value = 0 THEN @number ELSE @reference END AS result FROM ty_row INTO TABLE @DATA(incompatible_else_rows).`
	checker, _ := checker_test_check_source(t, &project, source, "mem://sql_case_result_types.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "SQL CASE WHEN condition is not logical"),
		0,
	)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 2)
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind == .Incompatible_Assignment_Type {
			testing.expect(t, strings.contains(diagnostic.message, "SQL CASE branch result is not compatible"))
		}
	}
}

@(test)
open_sql_simple_case_validates_selector_comparison_types_once :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         value TYPE i,
         text TYPE string,
       END OF ty_row.
DATA number TYPE i.
DATA text TYPE string.
DATA reference TYPE REF TO i.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.

SELECT CASE value WHEN @number THEN value WHEN 0 THEN value END AS result FROM ty_row INTO TABLE @DATA(valid_host_rows).
SELECT CASE @number WHEN @values THEN value END AS result FROM ty_row INTO TABLE @DATA(incompatible_arm_rows).
SELECT CASE @values WHEN @number THEN value END AS result FROM ty_row INTO TABLE @DATA(incompatible_selector_rows).
SELECT CASE @missing_selector WHEN @missing_arm THEN value END AS result FROM ty_row INTO TABLE @DATA(unknown_rows).`
	checker, file := checker_test_check_source(t, &project, source, "mem://sql_simple_case_comparison_types.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_count(&checker, .Incompatible_Argument_Type),
		2,
	)
	incompatible_values, incompatible_number := 0, 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Incompatible_Argument_Type {
			continue
		}
		testing.expect(t, strings.contains(diagnostic.message, "SQL CASE WHEN operand is not compatible"))
		operand_text := source[diagnostic.range.start:diagnostic.range.end]
		if operand_text == "@values" {
			incompatible_values += 1
		} else if operand_text == "@number" {
			incompatible_number += 1
		}
	}
	testing.expect_value(t, incompatible_values, 1)
	testing.expect_value(t, incompatible_number, 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)
	unresolved_names := [?]string{"missing_selector", "missing_arm"}
	for name in unresolved_names {
		testing.expect_value(
			t,
			checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name),
			1,
		)
	}
	valid_case := file.root.stmts[5].derived_stmt.(^ast.Select_Stmt).query.projection_clauses[0].value.derived_expr.(^ast.Sql_Case_Expr)
	valid_when := valid_case.whens[0].derived_expr.(^ast.Sql_Case_When_Expr)
	checker_test_expect_expr_lhs(t, &checker, valid_when.condition, false)
	number := checker_test_lookup(t, &project, file.root_scope, .Value, "number", .Variable)
	testing.expect(t, number != nil && .Used in number.flags)
}

@(test)
open_sql_coalesce_operands_are_compatible_and_checked_once :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         value TYPE i,
       END OF ty_row.
DATA number TYPE i.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.

SELECT COALESCE( value, @number ) AS result FROM ty_row INTO TABLE @DATA(valid_rows).
SELECT COALESCE( @number, @values ) AS result FROM ty_row INTO TABLE @DATA(incompatible_alternative_rows).
SELECT COALESCE( @values, @number ) AS result FROM ty_row INTO TABLE @DATA(incompatible_anchor_rows).
SELECT COALESCE( @missing_anchor, @number ) AS result FROM ty_row INTO TABLE @DATA(unknown_anchor_rows).
SELECT COALESCE( @number, @missing_alternative ) AS result FROM ty_row INTO TABLE @DATA(unknown_alternative_rows).`
	checker, file := checker_test_check_source(t, &project, source, "mem://sql_coalesce_operand_types.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 2)
	incompatible_values, incompatible_number := 0, 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Incompatible_Assignment_Type {
			continue
		}
		testing.expect(t, strings.contains(diagnostic.message, "SQL COALESCE operand is not compatible"))
		operand_text := source[diagnostic.range.start:diagnostic.range.end]
		if operand_text == "@values" {
			incompatible_values += 1
		} else if operand_text == "@number" {
			incompatible_number += 1
		}
	}
	testing.expect_value(t, incompatible_values, 1)
	testing.expect_value(t, incompatible_number, 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)
	unresolved_names := [?]string{"missing_anchor", "missing_alternative"}
	for name in unresolved_names {
		testing.expect_value(
			t,
			checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name),
			1,
		)
	}
	valid_call := file.root.stmts[3].derived_stmt.(^ast.Select_Stmt).query.projection_clauses[0].value.derived_expr.(^ast.Sql_Call_Expr)
	checker_test_expect_expr_lhs(t, &checker, valid_call.args[1], false)
	number := checker_test_lookup(t, &project, file.root_scope, .Value, "number", .Variable)
	values := checker_test_lookup(t, &project, file.root_scope, .Value, "values", .Variable)
	testing.expect(t, number != nil && .Used in number.flags)
	testing.expect(t, values != nil && .Used in values.flags)
}

@(test)
open_sql_scalar_function_arguments_are_scalar_and_checked_once :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         value TYPE i,
       END OF ty_row.
DATA row TYPE ty_row.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.

SELECT COALESCE( value, value ) AS valid FROM ty_row INTO TABLE @DATA(valid_rows).
SELECT COALESCE( @row, @values, @reference ) AS invalid FROM ty_row INTO TABLE @DATA(invalid_rows).
SELECT COALESCE( @missing, value ) AS unresolved FROM ty_row INTO TABLE @DATA(unresolved_rows).`
	checker, file := checker_test_check_source(t, &project, source, "mem://sql_scalar_function_arguments.abap")

	invalid_ranges := [?]string{"@row", "@values", "@reference"}
	invalid_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form ||
		   diagnostic.message != "SQL function argument is not scalar" {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], invalid_ranges[invalid_count])
		invalid_count += 1
	}
	testing.expect_value(t, invalid_count, len(invalid_ranges))
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing"),
		1,
	)
	invalid_call := file.root.stmts[5].derived_stmt.(^ast.Select_Stmt).query.projection_clauses[0].value.derived_expr.(^ast.Sql_Call_Expr)
	for arg in invalid_call.args {
		checker_test_expect_expr_lhs(t, &checker, arg, false)
	}
	row := checker_test_lookup(t, &project, file.root_scope, .Value, "row", .Variable)
	values := checker_test_lookup(t, &project, file.root_scope, .Value, "values", .Variable)
	reference := checker_test_lookup(t, &project, file.root_scope, .Value, "reference", .Variable)
	testing.expect(t, row != nil && .Used in row.flags)
	testing.expect(t, values != nil && .Used in values.flags)
	testing.expect(t, reference != nil && .Used in reference.flags)
}

@(test)
open_sql_stars_are_limited_to_projections_and_count_arguments :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         value TYPE i,
       END OF ty_row.

SELECT *, row~* FROM ty_row AS row INTO TABLE @DATA(valid_projection_rows).
SELECT COUNT(*) AS total FROM ty_row INTO @DATA(valid_count).
SELECT COALESCE( *, value ) AS invalid_function FROM ty_row INTO TABLE @DATA(function_rows).
SELECT COALESCE( value, * ) AS invalid_later_argument FROM ty_row INTO TABLE @DATA(later_argument_rows).
SELECT SUM( * ) AS invalid_aggregate FROM ty_row INTO @DATA(aggregate_result).
SELECT CASE WHEN value = 1 THEN * ELSE value END AS invalid_case FROM ty_row INTO TABLE @DATA(case_rows).
SELECT COALESCE( COALESCE( *, value ), value ) AS invalid_nested FROM ty_row INTO TABLE @DATA(nested_rows).`
	checker, _ := checker_test_check_source(t, &project, source, "mem://sql_star_placements.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"SQL star is only valid as a projection or COUNT(*) argument",
		),
		5,
	)
	invalid_star_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form ||
		   diagnostic.message != "SQL star is only valid as a projection or COUNT(*) argument" {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "*")
		invalid_star_count += 1
	}
	testing.expect_value(t, invalid_star_count, 5)
}

@(test)
open_sql_projections_require_scalar_expressions :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         amount TYPE i,
         values TYPE STANDARD TABLE OF i WITH EMPTY KEY,
         reference TYPE REF TO i,
       END OF ty_row.
DATA row TYPE ty_row.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.
DATA dynamic_projection TYPE string.

SELECT amount, amount + 1 AS adjusted, COUNT(*) AS total FROM ty_row INTO TABLE @DATA(valid_rows).
SELECT (dynamic_projection) FROM ty_row INTO TABLE @DATA(dynamic_rows).
SELECT @row AS result FROM ty_row INTO TABLE @DATA(structure_rows).
SELECT @values AS result FROM ty_row INTO TABLE @DATA(table_rows).
SELECT @reference AS result FROM ty_row INTO TABLE @DATA(reference_rows).
SELECT values AS result FROM ty_row INTO TABLE @DATA(column_table_rows).
SELECT reference AS result FROM ty_row INTO TABLE @DATA(column_reference_rows).`
	checker, file := checker_test_check_source(t, &project, source, "mem://sql_projection_expression_types.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"Open SQL projection expression is not scalar",
		),
		5,
	)
	invalid_ranges := [?]string{"@row", "@values", "@reference", "values", "reference"}
	invalid_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form ||
		   diagnostic.message != "Open SQL projection expression is not scalar" {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], invalid_ranges[invalid_count])
		invalid_count += 1
	}
	testing.expect_value(t, invalid_count, len(invalid_ranges))

	dynamic_stmt := file.root.stmts[6].derived_stmt.(^ast.Select_Stmt)
	testing.expect(t, dynamic_stmt.query.projection_clauses[0].is_dynamic)
	used_names := [?]string{"row", "values", "reference", "dynamic_projection"}
	for name in used_names {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
	ty_row := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_row", .Type_Def)
	structure := checker_type_structure(ty_row.type)
	field_names := [?]string{"amount", "values", "reference"}
	for name in field_names {
		field := checker_test_structure_field(t, &project, structure, name)
		testing.expect(t, field != nil && .Used in field.flags)
	}
}

@(test)
open_sql_up_to_rows_requires_an_integer_row_count :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         amount TYPE i,
       END OF ty_row.
DATA row TYPE ty_row.
DATA int1_limit TYPE int1.
DATA int2_limit TYPE int2.
DATA int8_limit TYPE int8.
DATA packed_limit TYPE p DECIMALS 2.
DATA float_limit TYPE f.
DATA text_limit TYPE string.
DATA numeric_text_limit TYPE n LENGTH 4.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.

SELECT amount FROM ty_row INTO TABLE @DATA(literal_rows) UP TO 10 ROWS.
SELECT amount FROM ty_row INTO TABLE @DATA(expression_rows) UP TO @( int1_limit + int2_limit ) ROWS.
SELECT amount FROM ty_row INTO TABLE @DATA(int8_rows) UP TO @int8_limit ROWS.
SELECT amount FROM ty_row INTO TABLE @DATA(packed_rows) UP TO @packed_limit ROWS.
SELECT amount FROM ty_row INTO TABLE @DATA(float_rows) UP TO @float_limit ROWS.
SELECT amount FROM ty_row INTO TABLE @DATA(text_rows) UP TO @text_limit ROWS.
SELECT amount FROM ty_row INTO TABLE @DATA(numeric_text_rows) UP TO @numeric_text_limit ROWS.
SELECT amount FROM ty_row INTO TABLE @DATA(structure_rows) UP TO @row ROWS.
SELECT amount FROM ty_row INTO TABLE @DATA(table_rows) UP TO @values ROWS.
SELECT amount FROM ty_row INTO TABLE @DATA(reference_rows) UP TO @reference ROWS.
SELECT amount FROM ty_row INTO TABLE @DATA(unknown_rows) UP TO @missing_limit ROWS.`
	checker, file := checker_test_check_source(t, &project, source, "mem://sql_up_to_rows_types.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"Open SQL UP TO operand is not an integer row count",
		),
		7,
	)
	invalid_ranges := [?]string {
		"@packed_limit",
		"@float_limit",
		"@text_limit",
		"@numeric_text_limit",
		"@row",
		"@values",
		"@reference",
	}
	invalid_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form ||
		   diagnostic.message != "Open SQL UP TO operand is not an integer row count" {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], invalid_ranges[invalid_count])
		invalid_count += 1
	}
	testing.expect_value(t, invalid_count, len(invalid_ranges))
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_limit"),
		1,
	)

	expression_stmt := file.root.stmts[12].derived_stmt.(^ast.Select_Stmt)
	expression_host := expression_stmt.query.up_to_rows.derived_expr.(^ast.Host_Expr)
	expression := expression_host.value.derived_expr.(^ast.Paren_Expr).expr.derived_expr.(^ast.Binary_Expr)
	checker_test_expect_expr_lhs(t, &checker, expression.left, false)
	checker_test_expect_expr_lhs(t, &checker, expression.right, false)
	used_names := [?]string {
		"int1_limit",
		"int2_limit",
		"int8_limit",
		"packed_limit",
		"float_limit",
		"text_limit",
		"numeric_text_limit",
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
open_sql_package_size_requires_an_integer_row_count :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         amount TYPE i,
       END OF ty_row.
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

SELECT amount FROM ty_row INTO TABLE @DATA(literal_rows) PACKAGE SIZE 10.
SELECT amount FROM ty_row INTO TABLE @DATA(expression_rows) PACKAGE SIZE @( int1_size + int2_size ).
SELECT amount FROM ty_row INTO TABLE @DATA(int8_rows) PACKAGE SIZE @int8_size.
SELECT amount FROM ty_row INTO TABLE @DATA(packed_rows) PACKAGE SIZE @packed_size.
SELECT amount FROM ty_row INTO TABLE @DATA(float_rows) PACKAGE SIZE @float_size.
SELECT amount FROM ty_row INTO TABLE @DATA(text_rows) PACKAGE SIZE @text_size.
SELECT amount FROM ty_row INTO TABLE @DATA(numeric_text_rows) PACKAGE SIZE @numeric_text_size.
SELECT amount FROM ty_row INTO TABLE @DATA(structure_rows) PACKAGE SIZE @row.
SELECT amount FROM ty_row INTO TABLE @DATA(table_rows) PACKAGE SIZE @values.
SELECT amount FROM ty_row INTO TABLE @DATA(reference_rows) PACKAGE SIZE @reference.
SELECT amount FROM ty_row INTO TABLE @DATA(unknown_rows) PACKAGE SIZE @missing_size.`
	checker, file := checker_test_check_source(t, &project, source, "mem://sql_package_size_types.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"Open SQL PACKAGE SIZE operand is not an integer row count",
		),
		7,
	)
	invalid_ranges := [?]string {
		"@packed_size",
		"@float_size",
		"@text_size",
		"@numeric_text_size",
		"@row",
		"@values",
		"@reference",
	}
	invalid_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form ||
		   diagnostic.message != "Open SQL PACKAGE SIZE operand is not an integer row count" {
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

	expression_stmt := file.root.stmts[12].derived_stmt.(^ast.Select_Stmt)
	expression_host := expression_stmt.query.package_size.derived_expr.(^ast.Host_Expr)
	expression := expression_host.value.derived_expr.(^ast.Paren_Expr).expr.derived_expr.(^ast.Binary_Expr)
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
open_sql_group_by_requires_scalar_non_aggregate_expressions :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         amount TYPE i,
         values TYPE STANDARD TABLE OF i WITH EMPTY KEY,
         reference TYPE REF TO i,
       END OF ty_row.
DATA row TYPE ty_row.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.
DATA dynamic_group TYPE string.

SELECT amount, COUNT(*) AS total FROM ty_row GROUP BY amount INTO TABLE @DATA(valid_column_rows).
SELECT amount + 1 AS adjusted FROM ty_row GROUP BY amount + 1 INTO TABLE @DATA(valid_expression_rows).
SELECT amount FROM ty_row GROUP BY (dynamic_group) INTO TABLE @DATA(dynamic_rows).
SELECT amount FROM ty_row GROUP BY @row INTO TABLE @DATA(structure_rows).
SELECT amount FROM ty_row GROUP BY @values INTO TABLE @DATA(table_rows).
SELECT amount FROM ty_row GROUP BY @reference INTO TABLE @DATA(reference_rows).
SELECT amount FROM ty_row GROUP BY SUM( amount ) INTO TABLE @DATA(aggregate_rows).
SELECT amount FROM ty_row GROUP BY COALESCE( SUM( amount ), MAX( amount ) ) INTO TABLE @DATA(nested_aggregate_rows).`
	checker, file := checker_test_check_source(t, &project, source, "mem://sql_group_by_expressions.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Open_Sql_Group_By,
			"Open SQL GROUP BY expression is not scalar",
		),
		3,
	)
	non_scalar_ranges := [?]string{"@row", "@values", "@reference"}
	non_scalar_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Open_Sql_Group_By ||
		   diagnostic.message != "Open SQL GROUP BY expression is not scalar" {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], non_scalar_ranges[non_scalar_count])
		non_scalar_count += 1
	}
	testing.expect_value(t, non_scalar_count, len(non_scalar_ranges))

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Open_Sql_Group_By,
			"Open SQL GROUP BY cannot contain aggregate expressions",
		),
		3,
	)
	aggregate_ranges := [?]string{"SUM( amount )", "SUM( amount )", "MAX( amount )"}
	aggregate_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Open_Sql_Group_By ||
		   diagnostic.message != "Open SQL GROUP BY cannot contain aggregate expressions" {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], aggregate_ranges[aggregate_count])
		aggregate_count += 1
	}
	testing.expect_value(t, aggregate_count, len(aggregate_ranges))

	for stmt_index in 5 ..= 8 {
		stmt := file.root.stmts[stmt_index].derived_stmt.(^ast.Select_Stmt)
		group_expr := stmt.query.group_by[0]
		testing.expect(t, group_expr.is_dynamic == (stmt_index == 7))
	}
	used_names := [?]string{"row", "values", "reference", "dynamic_group"}
	for name in used_names {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}

@(test)
open_sql_between_bounds_are_compatible_and_checked_once :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         value TYPE i,
       END OF ty_row.
DATA number TYPE i.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
CONSTANTS upper_bound TYPE i VALUE 10.

SELECT amount FROM ty_row INTO TABLE @DATA(valid_rows) WHERE amount BETWEEN @number AND @upper_bound.
SELECT amount FROM ty_row INTO TABLE @DATA(incompatible_low_rows) WHERE @number BETWEEN @values AND 10.
SELECT amount FROM ty_row INTO TABLE @DATA(incompatible_high_rows) WHERE @values BETWEEN @number AND @values.
SELECT amount FROM ty_row INTO TABLE @DATA(unknown_subject_rows) WHERE @missing_subject BETWEEN @number AND 10.
SELECT amount FROM ty_row INTO TABLE @DATA(unknown_bound_rows) WHERE @number BETWEEN @missing_low AND @missing_high.`
	checker, file := checker_test_check_source(t, &project, source, "mem://sql_between_operand_types.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 2)
	incompatible_values, incompatible_number := 0, 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Incompatible_Assignment_Type {
			continue
		}
		testing.expect(t, strings.contains(diagnostic.message, "SQL BETWEEN bound is not compatible"))
		operand_text := source[diagnostic.range.start:diagnostic.range.end]
		if operand_text == "@values" {
			incompatible_values += 1
		} else if operand_text == "@number" {
			incompatible_number += 1
		}
	}
	testing.expect_value(t, incompatible_values, 1)
	testing.expect_value(t, incompatible_number, 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 3)
	unresolved_names := [?]string{"missing_subject", "missing_low", "missing_high"}
	for name in unresolved_names {
		testing.expect_value(
			t,
			checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name),
			1,
		)
	}
	valid_between := file.root.stmts[4].derived_stmt.(^ast.Select_Stmt).query.where_cond.derived_expr.(^ast.Between_Expr)
	checker_test_expect_expr_lhs(t, &checker, valid_between.low, false)
	checker_test_expect_expr_lhs(t, &checker, valid_between.high, false)
	number := checker_test_lookup(t, &project, file.root_scope, .Value, "number", .Variable)
	values := checker_test_lookup(t, &project, file.root_scope, .Value, "values", .Variable)
	upper_bound := checker_test_lookup(t, &project, file.root_scope, .Value, "upper_bound", .Constant)
	testing.expect(t, number != nil && .Used in number.flags)
	testing.expect(t, values != nil && .Used in values.flags)
	testing.expect(t, upper_bound != nil && .Used in upper_bound.flags)
}

@(test)
open_sql_comparison_operands_are_compatible_and_checked_once :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         amount TYPE i,
       END OF ty_row.
DATA number TYPE i.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
CONSTANTS limit TYPE i VALUE 10.

SELECT amount FROM ty_row INTO TABLE @DATA(equal_rows) WHERE amount = @number.
SELECT amount FROM ty_row INTO TABLE @DATA(not_equal_rows) WHERE amount <> @limit.
SELECT amount FROM ty_row INTO TABLE @DATA(less_rows) WHERE amount < 10.
SELECT amount FROM ty_row INTO TABLE @DATA(less_equal_rows) WHERE amount <= @number.
SELECT amount FROM ty_row INTO TABLE @DATA(greater_rows) WHERE amount > @number.
SELECT amount FROM ty_row INTO TABLE @DATA(greater_equal_rows) WHERE amount >= @limit.
SELECT amount FROM ty_row INTO TABLE @DATA(incompatible_right_rows) WHERE @number = @values.
SELECT amount FROM ty_row INTO TABLE @DATA(incompatible_left_rows) WHERE @values <> @number.
SELECT amount FROM ty_row INTO TABLE @DATA(unknown_left_rows) WHERE @missing_left < @number.
SELECT amount FROM ty_row INTO TABLE @DATA(unknown_right_rows) WHERE @number >= @missing_right.`
	checker, file := checker_test_check_source(t, &project, source, "mem://sql_comparison_operand_types.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 2)
	incompatible_values, incompatible_number := 0, 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Incompatible_Assignment_Type {
			continue
		}
		testing.expect(t, strings.contains(diagnostic.message, "SQL comparison operand is not compatible"))
		operand_text := source[diagnostic.range.start:diagnostic.range.end]
		if operand_text == "@values" {
			incompatible_values += 1
		} else if operand_text == "@number" {
			incompatible_number += 1
		}
	}
	testing.expect_value(t, incompatible_values, 1)
	testing.expect_value(t, incompatible_number, 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)
	unresolved_names := [?]string{"missing_left", "missing_right"}
	for name in unresolved_names {
		testing.expect_value(
			t,
			checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name),
			1,
		)
	}
	host_comparison := file.root.stmts[10].derived_stmt.(^ast.Select_Stmt).query.where_cond.derived_expr.(^ast.Binary_Expr)
	checker_test_expect_expr_lhs(t, &checker, host_comparison.left, false)
	checker_test_expect_expr_lhs(t, &checker, host_comparison.right, false)
	number := checker_test_lookup(t, &project, file.root_scope, .Value, "number", .Variable)
	values := checker_test_lookup(t, &project, file.root_scope, .Value, "values", .Variable)
	limit := checker_test_lookup(t, &project, file.root_scope, .Value, "limit", .Constant)
	testing.expect(t, number != nil && .Used in number.flags)
	testing.expect(t, values != nil && .Used in values.flags)
	testing.expect(t, limit != nil && .Used in limit.flags)
}

@(test)
open_sql_logical_binary_operands_are_logical_and_checked_once :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         amount TYPE i,
         ready TYPE abap_bool,
       END OF ty_row.
DATA number TYPE i.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.
DATA host_ready TYPE abap_bool.

SELECT amount FROM ty_row INTO TABLE @DATA(valid_rows) WHERE amount = @number AND ( ready OR NOT @host_ready ).
SELECT amount FROM ty_row INTO TABLE @DATA(number_rows) WHERE @number AND amount = 1.
SELECT amount FROM ty_row INTO TABLE @DATA(table_rows) WHERE amount = 1 AND NOT @values.
SELECT amount FROM ty_row INTO TABLE @DATA(reference_rows) WHERE @reference OR ready.
SELECT amount FROM ty_row INTO TABLE @DATA(unknown_left_rows) WHERE @missing_left OR ready.
SELECT amount FROM ty_row INTO TABLE @DATA(unknown_right_rows) WHERE ready AND ( @missing_right ).`
	checker, file := checker_test_check_source(t, &project, source, "mem://sql_logical_binary_operand_types.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "SQL AND condition is not logical"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "SQL OR condition is not logical"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "SQL NOT condition is not logical"),
		1,
	)
	invalid_number, invalid_values, invalid_reference := 0, 0, 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form ||
		   !strings.contains(diagnostic.message, "condition is not logical") {
			continue
		}
		operand_text := source[diagnostic.range.start:diagnostic.range.end]
		switch operand_text {
		case "@number":
			invalid_number += 1
		case "@values":
			invalid_values += 1
		case "@reference":
			invalid_reference += 1
		}
	}
	testing.expect_value(t, invalid_number, 1)
	testing.expect_value(t, invalid_values, 1)
	testing.expect_value(t, invalid_reference, 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)
	unresolved_names := [?]string{"missing_left", "missing_right"}
	for name in unresolved_names {
		testing.expect_value(
			t,
			checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name),
			1,
		)
	}
	valid_condition := file.root.stmts[5].derived_stmt.(^ast.Select_Stmt).query.where_cond.derived_expr.(^ast.Binary_Expr)
	valid_comparison := valid_condition.left.derived_expr.(^ast.Binary_Expr)
	valid_group := valid_condition.right.derived_expr.(^ast.Paren_Expr)
	valid_or := valid_group.expr.derived_expr.(^ast.Binary_Expr)
	valid_not := valid_or.right.derived_expr.(^ast.Unary_Expr)
	checker_test_expect_expr_lhs(t, &checker, valid_comparison.right, false)
	checker_test_expect_expr_lhs(t, &checker, valid_not.expr, false)
	number := checker_test_lookup(t, &project, file.root_scope, .Value, "number", .Variable)
	values := checker_test_lookup(t, &project, file.root_scope, .Value, "values", .Variable)
	reference := checker_test_lookup(t, &project, file.root_scope, .Value, "reference", .Variable)
	host_ready := checker_test_lookup(t, &project, file.root_scope, .Value, "host_ready", .Variable)
	testing.expect(t, number != nil && .Used in number.flags)
	testing.expect(t, values != nil && .Used in values.flags)
	testing.expect(t, reference != nil && .Used in reference.flags)
	testing.expect(t, host_ready != nil && .Used in host_ready.flags)
}

@(test)
open_sql_not_validates_direct_operands_in_nested_predicates :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         amount TYPE i,
       END OF ty_row.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.

SELECT CASE WHEN NOT @values THEN amount ELSE amount END AS result
  FROM ty_row INTO TABLE @DATA(case_rows).
SELECT amount FROM ty_row INTO TABLE @DATA(double_not_rows) WHERE NOT NOT @reference.`
	checker, file := checker_test_check_source(t, &project, source, "mem://sql_not_nested_operand_types.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "SQL NOT condition is not logical"),
		2,
	)
	values_diagnostics, reference_diagnostics := 0, 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form || diagnostic.message != "SQL NOT condition is not logical" {
			continue
		}
		switch source[diagnostic.range.start:diagnostic.range.end] {
		case "@values":
			values_diagnostics += 1
		case "@reference":
			reference_diagnostics += 1
		}
	}
	testing.expect_value(t, values_diagnostics, 1)
	testing.expect_value(t, reference_diagnostics, 1)
	values := checker_test_lookup(t, &project, file.root_scope, .Value, "values", .Variable)
	reference := checker_test_lookup(t, &project, file.root_scope, .Value, "reference", .Variable)
	testing.expect(t, values != nil && .Used in values.flags)
	testing.expect(t, reference != nil && .Used in reference.flags)
}

@(test)
open_sql_predicate_roots_are_logical_and_checked_once :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
         amount TYPE i,
         ready TYPE abap_bool,
       END OF ty_row.
DATA row TYPE ty_row.
DATA number TYPE i.
DATA values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.
DATA ready TYPE abap_bool.
DATA where_text TYPE string VALUE 'amount = 1'.

SELECT amount FROM ty_row INTO TABLE @DATA(valid_rows) WHERE ( NOT @ready ).
SELECT amount FROM ty_row INTO TABLE @DATA(number_rows) WHERE @number.
SELECT amount FROM ty_row INTO TABLE @DATA(table_rows) WHERE @values.
SELECT amount FROM ty_row INTO TABLE @DATA(reference_rows) WHERE @reference.
SELECT amount FROM ty_row INTO TABLE @DATA(unknown_rows) WHERE @missing_condition.
SELECT a~amount FROM ty_row AS a INNER JOIN ty_row AS b ON @number INTO TABLE @DATA(join_rows).
UPDATE ty_row SET amount = @number WHERE @number.
MODIFY ty_row FROM @row WHERE NOT @values.
DELETE FROM ty_row WHERE ( NOT @reference ).
SELECT amount FROM ty_row INTO TABLE @DATA(dynamic_rows) WHERE (where_text).`
	checker, file := checker_test_check_source(t, &project, source, "mem://sql_predicate_root_types.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "SQL condition is not logical"),
		5,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "SQL NOT condition is not logical"),
		2,
	)
	invalid_number, invalid_values, invalid_reference := 0, 0, 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form ||
		   diagnostic.message != "SQL condition is not logical" {
			continue
		}
		operand_text := source[diagnostic.range.start:diagnostic.range.end]
		switch operand_text {
		case "@number":
			invalid_number += 1
		case "@values":
			invalid_values += 1
		case "@reference":
			invalid_reference += 1
		}
	}
	testing.expect_value(t, invalid_number, 3)
	testing.expect_value(t, invalid_values, 1)
	testing.expect_value(t, invalid_reference, 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_condition"),
		1,
	)
	valid_condition := file.root.stmts[7].derived_stmt.(^ast.Select_Stmt).query.where_cond
	valid_group := valid_condition.derived_expr.(^ast.Paren_Expr)
	valid_not := valid_group.expr.derived_expr.(^ast.Unary_Expr)
	checker_test_expect_expr_lhs(t, &checker, valid_not.expr, false)
	ready := checker_test_lookup(t, &project, file.root_scope, .Value, "ready", .Variable)
	number := checker_test_lookup(t, &project, file.root_scope, .Value, "number", .Variable)
	values := checker_test_lookup(t, &project, file.root_scope, .Value, "values", .Variable)
	reference := checker_test_lookup(t, &project, file.root_scope, .Value, "reference", .Variable)
	where_text := checker_test_lookup(t, &project, file.root_scope, .Value, "where_text", .Variable)
	testing.expect(t, ready != nil && .Used in ready.flags)
	testing.expect(t, number != nil && .Used in number.flags)
	testing.expect(t, values != nil && .Used in values.flags)
	testing.expect(t, reference != nil && .Used in reference.flags)
	testing.expect(t, where_text != nil && .Used in where_text.flags)
}
