package abap_frontend_semantic2

import "src:ast"

import "core:testing"

@(test)
get_parameter_operands_require_supported_parameter_shapes :: proc(t: ^testing.T) {
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
GET PARAMETER ID 'ABC' FIELD DATA(inline_value).
GET PARAMETER ID character FIELD numeric_text.
GET PARAMETER ID date_value FIELD time_value.
GET PARAMETER ID <generic> FIELD <generic>.
GET PARAMETER ID integer FIELD integer.
GET PARAMETER ID text FIELD text.
GET PARAMETER ID bytes FIELD bytes.
GET PARAMETER ID row FIELD row.
GET PARAMETER ID values FIELD values.
GET PARAMETER ID reference FIELD reference.
GET PARAMETER ID 'ABCDEFGHIJKLMNOPQRSTU' FIELD character.
GET PARAMETER ID '   ' FIELD character.
GET PARAMETER ID missing_id FIELD missing_field.`
	checker, file := checker_test_check_source(t, &project, source, "mem://get_parameter_types.abap")

	id_message := "GET PARAMETER ID operand is not flat character-like"
	field_message := "GET PARAMETER FIELD target is not a flat character-like variable"
	literal_message := "GET PARAMETER ID literal must contain 1 to 20 non-blank characters"
	invalid_ranges := [?]string{"integer", "text", "bytes", "row", "values", "reference"}
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, id_message), len(invalid_ranges))
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, field_message), len(invalid_ranges))
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, literal_message), 2)
	id_count, field_count, literal_count := 0, 0, 0
	literal_ranges := [?]string{"'ABCDEFGHIJKLMNOPQRSTU'", "'   '"}
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form {
			continue
		}
		switch diagnostic.message {
		case id_message:
			testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], invalid_ranges[id_count])
			id_count += 1
		case field_message:
			testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], invalid_ranges[field_count])
			field_count += 1
		case literal_message:
			testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], literal_ranges[literal_count])
			literal_count += 1
		}
	}
	testing.expect_value(t, id_count, len(invalid_ranges))
	testing.expect_value(t, field_count, len(invalid_ranges))
	testing.expect_value(t, literal_count, len(literal_ranges))
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_id"), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_field"), 1)

	inline_stmt := file.root.stmts[12].derived_stmt.(^ast.Runtime_Stmt)
	inline_info, inline_ok := checker_test_expr_info_for_node(t, &checker, &inline_stmt.field.expr_base)
	testing.expect(t, inline_ok)
	if inline_ok {
		testing.expect_value(t, checker_test_type_name(&project, inline_info.type), "c")
		length, length_ok := type_length(inline_info.type)
		testing.expect(t, length_ok)
		testing.expect_value(t, length, 255)
	}
	for i in 13 ..< len(file.root.stmts) {
		stmt := file.root.stmts[i].derived_stmt.(^ast.Runtime_Stmt)
		checker_test_expect_expr_lhs(t, &checker, stmt.id, false)
		checker_test_expect_expr_lhs(t, &checker, stmt.field, true)
	}
	for name in invalid_ranges {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}

@(test)
get_time_stamp_target_requires_a_short_or_long_timestamp :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
  value TYPE i,
END OF ty_row.
DATA short_timestamp TYPE timestamp.
DATA packed_short TYPE p LENGTH 8 DECIMALS 0.
DATA packed_long TYPE p LENGTH 11 DECIMALS 7.
DATA wrong_length TYPE p LENGTH 10 DECIMALS 7.
DATA wrong_decimals TYPE p LENGTH 11 DECIMALS 2.
DATA integer TYPE i.
DATA row TYPE ty_row.
DATA table_value TYPE STANDARD TABLE OF timestamp WITH EMPTY KEY.
DATA reference TYPE REF TO timestamp.
FIELD-SYMBOLS <generic> TYPE any.
GET TIME STAMP FIELD DATA(inline_timestamp).
GET TIME STAMP FIELD short_timestamp.
GET TIME STAMP FIELD packed_short.
GET TIME STAMP FIELD packed_long.
GET TIME STAMP FIELD <generic>.
GET TIME STAMP FIELD wrong_length.
GET TIME STAMP FIELD wrong_decimals.
GET TIME STAMP FIELD integer.
GET TIME STAMP FIELD row.
GET TIME STAMP FIELD table_value.
GET TIME STAMP FIELD reference.
GET TIME STAMP FIELD missing_target.`
	checker, file := checker_test_check_source(t, &project, source, "mem://get_time_stamp_types.abap")

	message := "GET TIME STAMP target must have type timestamp or timestampl"
	invalid_ranges := [?]string{"wrong_length", "wrong_decimals", "integer", "row", "table_value", "reference"}
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, message), len(invalid_ranges))
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
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_target"), 1)

	inline_stmt := file.root.stmts[11].derived_stmt.(^ast.Runtime_Stmt)
	inline_info, inline_ok := checker_test_expr_info_for_node(t, &checker, &inline_stmt.target.expr_base)
	testing.expect(t, inline_ok)
	if inline_ok {
		testing.expect_value(t, checker_test_type_name(&project, inline_info.type), "timestamp")
	}
	for i in 12 ..< len(file.root.stmts) {
		stmt := file.root.stmts[i].derived_stmt.(^ast.Runtime_Stmt)
		checker_test_expect_expr_lhs(t, &checker, stmt.target, true)
	}
	for name in invalid_ranges {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}

@(test)
get_run_time_target_must_accept_an_integer_result :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
  value TYPE i,
END OF ty_row.
DATA integer TYPE i.
DATA packed TYPE p LENGTH 8 DECIMALS 2.
DATA character TYPE c LENGTH 20.
DATA bytes TYPE x LENGTH 4.
DATA row TYPE ty_row.
DATA table_value TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference TYPE REF TO i.
FIELD-SYMBOLS <generic> TYPE any.
GET RUN TIME FIELD DATA(inline_integer).
GET RUN TIME FIELD integer.
GET RUN TIME FIELD packed.
GET RUN TIME FIELD character.
GET RUN TIME FIELD bytes.
GET RUN TIME FIELD <generic>.
GET RUN TIME FIELD row.
GET RUN TIME FIELD table_value.
GET RUN TIME FIELD reference.
GET RUN TIME FIELD missing_target.`
	checker, file := checker_test_check_source(t, &project, source, "mem://get_run_time_types.abap")

	message := "GET RUN TIME target cannot receive a value of type i"
	invalid_ranges := [?]string{"row", "table_value", "reference"}
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, message), len(invalid_ranges))
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
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_target"), 1)

	inline_stmt := file.root.stmts[10].derived_stmt.(^ast.Runtime_Stmt)
	inline_info, inline_ok := checker_test_expr_info_for_node(t, &checker, &inline_stmt.target.expr_base)
	testing.expect(t, inline_ok)
	if inline_ok {
		testing.expect_value(t, checker_test_type_name(&project, inline_info.type), "i")
	}
	for i in 11 ..< len(file.root.stmts) {
		stmt := file.root.stmts[i].derived_stmt.(^ast.Runtime_Stmt)
		checker_test_expect_expr_lhs(t, &checker, stmt.target, true)
	}
	for name in invalid_ranges {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}

@(test)
runtime_get_outputs_must_be_writable :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `CONSTANTS gc_text TYPE string VALUE ''.
CONSTANTS gc_number TYPE i VALUE 0.
DATA value TYPE string.
DATA number TYPE i.
GET RUN TIME FIELD gc_number.
GET PARAMETER ID 'ABC' FIELD gc_text.
GET CURSOR FIELD gc_text LINE gc_number OFFSET gc_number VALUE gc_text.
GET REFERENCE OF value INTO gc_text.
GET BADI gc_text.
GET RUN TIME FIELD number.
GET PARAMETER ID 'ABC' FIELD value.
GET CURSOR FIELD value LINE number OFFSET number VALUE value.
GET BADI value.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://runtime_get_writable.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "GET RUN TIME target is not writable"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "GET PARAMETER FIELD target is not writable"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "GET CURSOR FIELD target is not writable"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "GET CURSOR LINE target is not writable"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "GET CURSOR OFFSET target is not writable"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "GET CURSOR VALUE target is not writable"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "GET REFERENCE target is not writable"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "GET BADI target is not writable"),
		1,
	)
}

@(test)
runtime_structured_inputs_are_readable_and_get_outputs_are_writable :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA id TYPE c LENGTH 20.
DATA source TYPE c LENGTH 255.
DATA target TYPE c LENGTH 255.
DATA status TYPE string.
DATA excluded TYPE string.
DATA title TYPE string.
DATA title_arg TYPE string.
GET PARAMETER ID id FIELD target.
SET PARAMETER ID id FIELD source.
GET REFERENCE OF source INTO DATA(reference).
SET PF-STATUS status EXCLUDING excluded.
SET TITLEBAR title WITH title_arg.
SET SCREEN 100.
SET USER-COMMAND 'ENTER'.`
	checker, file := checker_test_check_source(t, &project, source, "mem://runtime_directionality.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	get_parameter := file.root.stmts[7].derived_stmt.(^ast.Runtime_Stmt)
	set_parameter := file.root.stmts[8].derived_stmt.(^ast.Runtime_Stmt)
	reference := file.root.stmts[9].derived_stmt.(^ast.Runtime_Stmt)
	pf_status := file.root.stmts[10].derived_stmt.(^ast.Runtime_Stmt)
	titlebar := file.root.stmts[11].derived_stmt.(^ast.Runtime_Stmt)
	readable := [?]^ast.Expr {
		get_parameter.id,
		set_parameter.id,
		set_parameter.field,
		reference.value,
		pf_status.target,
		pf_status.excluding[0],
		titlebar.target,
		titlebar.operands[0],
		file.root.stmts[12].derived_stmt.(^ast.Runtime_Stmt).target,
	}
	for expr in readable {
		info, ok := checker_test_expr_info_for_node(t, &checker, &expr.expr_base)
		testing.expect(t, ok)
		if ok {
			testing.expect(t, !info.is_lhs)
		}
	}
	writable := [?]^ast.Expr {get_parameter.field, reference.target}
	for expr in writable {
		info, ok := checker_test_expr_info_for_node(t, &checker, &expr.expr_base)
		testing.expect(t, ok)
		if ok {
			testing.expect(t, info.is_lhs)
		}
	}
}

@(test)
set_screen_requires_a_four_digit_numeric_text_value :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA screen_number TYPE n LENGTH 4.
DATA short_number TYPE n LENGTH 3.
DATA character TYPE c LENGTH 4.
DATA integer TYPE i.
DATA structure TYPE screen.
DATA table_value TYPE STANDARD TABLE OF n WITH EMPTY KEY.
DATA reference TYPE REF TO n.
FIELD-SYMBOLS <generic> TYPE any.
SET SCREEN 0.
SET SCREEN 9999.
SET SCREEN screen_number.
SET SCREEN <generic>.
SET SCREEN 10000.
SET SCREEN short_number.
SET SCREEN character.
SET SCREEN integer.
SET SCREEN structure.
SET SCREEN table_value.
SET SCREEN reference.
SET SCREEN missing_screen.`
	checker, file := checker_test_check_source(t, &project, source, "mem://set_screen_types.abap")

	range_message := "SET SCREEN literal is outside the range 0 to 9999"
	type_message := "SET SCREEN operand must have type n and length 4"
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, range_message), 1)
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, type_message), 6)
	invalid_ranges := [?]string{"10000", "short_number", "character", "integer", "structure", "table_value", "reference"}
	invalid_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], invalid_ranges[invalid_count])
		invalid_count += 1
	}
	testing.expect_value(t, invalid_count, len(invalid_ranges))
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_screen"), 1)

	stmt := file.root.stmts[10].derived_stmt.(^ast.Runtime_Stmt)
	checker_test_expect_expr_lhs(t, &checker, stmt.target, false)
	for name in invalid_ranges[1:] {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}

@(test)
set_user_command_requires_a_character_like_value :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `DATA character TYPE c LENGTH 20.
DATA numeric_text TYPE n LENGTH 20.
DATA date_value TYPE d.
DATA time_value TYPE t.
DATA text TYPE string.
DATA integer TYPE i.
DATA bytes TYPE x LENGTH 4.
DATA structure TYPE screen.
DATA table_value TYPE STANDARD TABLE OF c WITH EMPTY KEY.
DATA reference TYPE REF TO c.
FIELD-SYMBOLS <generic> TYPE any.
SET USER-COMMAND 'ENTER'.
SET USER-COMMAND character.
SET USER-COMMAND numeric_text.
SET USER-COMMAND date_value.
SET USER-COMMAND time_value.
SET USER-COMMAND text.
SET USER-COMMAND <generic>.
SET USER-COMMAND integer.
SET USER-COMMAND bytes.
SET USER-COMMAND structure.
SET USER-COMMAND table_value.
SET USER-COMMAND reference.
SET USER-COMMAND missing_command.`
	checker, file := checker_test_check_source(t, &project, source, "mem://set_user_command_types.abap")

	message := "SET USER-COMMAND operand is not character-like"
	invalid_ranges := [?]string{"integer", "bytes", "structure", "table_value", "reference"}
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, message), len(invalid_ranges))
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
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_command"), 1)

	stmt := file.root.stmts[12].derived_stmt.(^ast.Runtime_Stmt)
	checker_test_expect_expr_lhs(t, &checker, stmt.target, false)
	for name in invalid_ranges {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}

@(test)
runtime_readable_values_report_unresolved_references_and_accept_constants :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `SET PARAMETER ID missing_id FIELD missing_field.
GET REFERENCE OF missing_source INTO DATA(reference).
SET PF-STATUS missing_status EXCLUDING missing_excluding.
SET TITLEBAR missing_title WITH missing_title_arg.
SET SCREEN missing_screen.
SET USER-COMMAND missing_command.
SET PARAMETER ID 'ABC' FIELD 'value'.
SET PF-STATUS 'MAIN' EXCLUDING 'BACK'.
SET TITLEBAR 'TITLE' WITH 'argument'.
SET SCREEN 100.
SET USER-COMMAND 'ENTER'.`
	checker, _ := checker_test_check_source(t, &project, source, "mem://runtime_readable_values.abap")

	names := [?]string {
		"missing_id",
		"missing_field",
		"missing_source",
		"missing_status",
		"missing_excluding",
		"missing_title",
		"missing_title_arg",
		"missing_screen",
		"missing_command",
	}
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), len(names))
	for name in names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 1)
	}
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Syntax_Form), 0)
}

@(test)
get_reference_requires_data_object_and_compatible_data_reference_target :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `CONSTANTS constant TYPE i VALUE 1.
DATA integer TYPE i.
DATA text TYPE string.
DATA integer_ref TYPE REF TO i.
DATA text_ref TYPE REF TO string.
DATA generic_ref TYPE REF TO data.
FIELD-SYMBOLS <generic> TYPE any.
GET REFERENCE OF integer INTO integer_ref.
GET REFERENCE OF constant INTO generic_ref.
GET REFERENCE OF text INTO DATA(inline_ref).
GET REFERENCE OF <generic> INTO generic_ref.
GET REFERENCE OF integer INTO text_ref.
GET REFERENCE OF integer INTO text.
GET REFERENCE OF 1 INTO integer_ref.
GET REFERENCE OF integer + 1 INTO integer_ref.
GET REFERENCE OF missing_source INTO missing_target.`
	checker, file := checker_test_check_source(t, &project, source, "mem://get_reference_types.abap")

	data_object_message := "GET REFERENCE source is not a data object"
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, data_object_message), 2)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "GET REFERENCE target is not a data reference variable"),
		1,
	)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 1)
	invalid_source_ranges := [?]string{"1", "integer + 1"}
	invalid_source_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind == .Invalid_Syntax_Form && diagnostic.message == data_object_message {
			testing.expect_value(
				t,
				source[diagnostic.range.start:diagnostic.range.end],
				invalid_source_ranges[invalid_source_count],
			)
			invalid_source_count += 1
		}
	}
	testing.expect_value(t, invalid_source_count, len(invalid_source_ranges))
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_source"), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_target"), 1)

	inline_ref := checker_test_lookup(t, &project, file.root_scope, .Value, "inline_ref", .Variable)
	testing.expect(t, inline_ref != nil && checker_type_is_ref(inline_ref.type))
	if inline_ref != nil && checker_type_is_ref(inline_ref.type) {
		testing.expect_value(t, checker_type_ref_target(&checker.builtin_context, inline_ref.type).name, "string")
	}
	stmt := file.root.stmts[7].derived_stmt.(^ast.Runtime_Stmt)
	checker_test_expect_expr_lhs(t, &checker, stmt.value, false)
	checker_test_expect_expr_lhs(t, &checker, stmt.target, true)
	used_variables := [?]string{"integer", "text", "integer_ref", "text_ref", "generic_ref"}
	for name in used_variables {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
	constant := checker_test_lookup(t, &project, file.root_scope, .Value, "constant", .Constant)
	testing.expect(t, constant != nil && .Used in constant.flags)
}

@(test)
get_badi_requires_writable_badi_reference_target :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `INTERFACE lif_badi.
ENDINTERFACE.
CLASS lcl_badi DEFINITION.
ENDCLASS.
TYPES: BEGIN OF ty_row,
  value TYPE i,
END OF ty_row.
DATA interface_ref TYPE REF TO lif_badi.
DATA class_ref TYPE REF TO lcl_badi.
DATA object_ref TYPE REF TO object.
DATA data_ref TYPE REF TO data.
DATA scalar TYPE i.
DATA row TYPE ty_row.
DATA table_value TYPE STANDARD TABLE OF i WITH EMPTY KEY.
CONSTANTS constant_ref TYPE REF TO lif_badi VALUE IS INITIAL.
DATA unresolved_ref TYPE REF TO missing_type.
GET BADI interface_ref.
GET BADI class_ref.
GET BADI constant_ref.
GET BADI object_ref.
GET BADI data_ref.
GET BADI scalar.
GET BADI row.
GET BADI table_value.
GET BADI DATA(inline_ref).
GET BADI unresolved_ref.
GET BADI missing_target.`
	checker, file := checker_test_check_source(t, &project, source, "mem://get_badi_types.abap")

	type_message := "GET BADI target is not a BAdI reference variable"
	invalid_ranges := [?]string{"object_ref", "data_ref", "scalar", "row", "table_value"}
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, type_message), len(invalid_ranges))
	invalid_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form || diagnostic.message != type_message {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], invalid_ranges[invalid_count])
		invalid_count += 1
	}
	testing.expect_value(t, invalid_count, len(invalid_ranges))
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "GET BADI target is not writable"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "GET BADI target cannot be declared inline"),
		1,
	)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Type), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_target"), 1)

	for i in 12 ..< len(file.root.stmts) {
		stmt := file.root.stmts[i].derived_stmt.(^ast.Runtime_Stmt)
		checker_test_expect_expr_lhs(t, &checker, stmt.target, true)
	}
	used_variables := [?]string{"interface_ref", "class_ref", "object_ref", "data_ref", "scalar", "row", "table_value", "unresolved_ref"}
	for name in used_variables {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
	constant := checker_test_lookup(t, &project, file.root_scope, .Value, "constant_ref", .Constant)
	testing.expect(t, constant != nil && .Used in constant.flags)
}

@(test)
set_pf_status_operands_require_supported_character_like_values :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF ty_row,
  value TYPE c LENGTH 1,
END OF ty_row.
DATA character TYPE c LENGTH 20.
DATA text TYPE string.
DATA integer TYPE i.
DATA bytes TYPE x LENGTH 4.
DATA row TYPE ty_row.
DATA reference TYPE REF TO c.
DATA characters TYPE STANDARD TABLE OF c WITH EMPTY KEY.
DATA texts TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA integers TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
FIELD-SYMBOLS <generic> TYPE any.
SET PF-STATUS 'MAIN' EXCLUDING 'BACK'.
SET PF-STATUS character EXCLUDING text.
SET PF-STATUS text EXCLUDING characters.
SET PF-STATUS <generic> EXCLUDING <generic>.
SET PF-STATUS integer EXCLUDING integer.
SET PF-STATUS bytes EXCLUDING bytes.
SET PF-STATUS row EXCLUDING row.
SET PF-STATUS reference EXCLUDING reference.
SET PF-STATUS characters EXCLUDING texts.
SET PF-STATUS missing_status EXCLUDING missing_excluding.
SET PF-STATUS character EXCLUDING integers.
SET PF-STATUS character EXCLUDING rows.`
	checker, file := checker_test_check_source(t, &project, source, "mem://set_pf_status_types.abap")

	status_message := "SET PF-STATUS operand is not character-like"
	excluding_message := "SET PF-STATUS EXCLUDING operand is not character-like or an internal table"
	row_message := "SET PF-STATUS EXCLUDING table row is not flat character-like"
	status_ranges := [?]string{"integer", "bytes", "row", "reference", "characters"}
	excluding_ranges := [?]string{"integer", "bytes", "row", "reference"}
	row_ranges := [?]string{"texts", "integers", "rows"}
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, status_message), len(status_ranges))
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, excluding_message), len(excluding_ranges))
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, row_message), len(row_ranges))
	status_count, excluding_count, row_count := 0, 0, 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form {
			continue
		}
		switch diagnostic.message {
		case status_message:
			testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], status_ranges[status_count])
			status_count += 1
		case excluding_message:
			testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], excluding_ranges[excluding_count])
			excluding_count += 1
		case row_message:
			testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], row_ranges[row_count])
			row_count += 1
		}
	}
	testing.expect_value(t, status_count, len(status_ranges))
	testing.expect_value(t, excluding_count, len(excluding_ranges))
	testing.expect_value(t, row_count, len(row_ranges))
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_status"), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_excluding"), 1)

	for name in status_ranges {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
	stmt := file.root.stmts[14].derived_stmt.(^ast.Runtime_Stmt)
	checker_test_expect_expr_lhs(t, &checker, stmt.target, false)
	checker_test_expect_expr_lhs(t, &checker, stmt.excluding[0], false)
}

@(test)
set_parameter_operands_require_flat_character_like_values :: proc(t: ^testing.T) {
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
SET PARAMETER ID 'ABC' FIELD 'value'.
SET PARAMETER ID character FIELD numeric_text.
SET PARAMETER ID date_value FIELD time_value.
SET PARAMETER ID <generic> FIELD <generic>.
SET PARAMETER ID integer FIELD integer.
SET PARAMETER ID text FIELD text.
SET PARAMETER ID bytes FIELD bytes.
SET PARAMETER ID row FIELD row.
SET PARAMETER ID values FIELD values.
SET PARAMETER ID reference FIELD reference.
SET PARAMETER ID missing_id FIELD missing_field.`
	checker, file := checker_test_check_source(t, &project, source, "mem://set_parameter_types.abap")

	id_message := "SET PARAMETER ID operand is not flat character-like"
	field_message := "SET PARAMETER FIELD operand is not flat character-like"
	invalid_ranges := [?]string{"integer", "text", "bytes", "row", "values", "reference"}
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, id_message), len(invalid_ranges))
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, field_message), len(invalid_ranges))
	id_count, field_count := 0, 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form {
			continue
		}
		if diagnostic.message == id_message {
			testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], invalid_ranges[id_count])
			id_count += 1
		} else if diagnostic.message == field_message {
			testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], invalid_ranges[field_count])
			field_count += 1
		}
	}
	testing.expect_value(t, id_count, len(invalid_ranges))
	testing.expect_value(t, field_count, len(invalid_ranges))
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_id"), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_field"), 1)

	for name in invalid_ranges {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
	stmt := file.root.stmts[14].derived_stmt.(^ast.Runtime_Stmt)
	checker_test_expect_expr_lhs(t, &checker, stmt.id, false)
	checker_test_expect_expr_lhs(t, &checker, stmt.field, false)
}

@(test)
set_titlebar_operands_require_character_like_title_and_write_compatible_values :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	source := `TYPES: BEGIN OF character_row,
  text TYPE c LENGTH 3,
  digits TYPE n LENGTH 2,
END OF character_row.
TYPES: BEGIN OF numeric_row,
  text TYPE c LENGTH 3,
  number TYPE i,
END OF numeric_row.
DATA title TYPE string.
DATA integer TYPE i.
DATA character_value TYPE character_row.
DATA numeric_value TYPE numeric_row.
DATA table_value TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA reference_value TYPE REF TO i.
FIELD-SYMBOLS <generic> TYPE any.
SET TITLEBAR title WITH integer character_value.
SET TITLEBAR 'TITLE' WITH <generic>.
SET TITLEBAR integer WITH numeric_value table_value reference_value.
SET TITLEBAR missing_title WITH missing_value.`
	checker, file := checker_test_check_source(t, &project, source, "mem://set_titlebar_types.abap")

	title_message := "SET TITLEBAR operand is not character-like"
	with_message := "SET TITLEBAR WITH operand is not a WRITE-compatible value"
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, title_message), 1)
	testing.expect_value(t, checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, with_message), 3)
	invalid_ranges := [?]string{"integer", "numeric_value", "table_value", "reference_value"}
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
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_title"), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_value"), 1)

	stmt := file.root.stmts[9].derived_stmt.(^ast.Runtime_Stmt)
	checker_test_expect_expr_lhs(t, &checker, stmt.target, false)
	checker_test_expect_expr_lhs(t, &checker, stmt.operands[0], false)
	for name in invalid_ranges {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
}
