package abap_frontend_parser

import "src:ast"

import "core:testing"

@(test)
statement_batch_declarations :: proc(t: ^testing.T) {
	source := `DATA lv TYPE i.
DATA(lv_inline) = 1.
TYPES ty_i TYPE i.
CONSTANTS c_i TYPE i VALUE 1.
FIELD-SYMBOLS <fs> TYPE any.
STATICS st TYPE i.
TABLES mara.
RANGES r_matnr FOR mara-matnr.
PARAMETERS p_count TYPE i DEFAULT 1.
SELECT-OPTIONS s_matnr FOR mara-matnr.
CONTROLS tc TYPE TABLEVIEW USING SCREEN 100.
CLASS-DATA gv TYPE i.`
	parsed := parse(source, "decls.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.data_decl, 1)
	testing.expect_value(t, counts.data_inline, 1)
	testing.expect_value(t, counts.types_decl, 1)
	testing.expect_value(t, counts.constants, 1)
	testing.expect_value(t, counts.field_symbols, 1)
	testing.expect_value(t, counts.statics, 1)
	testing.expect_value(t, counts.tables_decl, 1)
	testing.expect_value(t, counts.ranges, 1)
	testing.expect_value(t, counts.parameters, 1)
	testing.expect_value(t, counts.select_options, 1)
	testing.expect_value(t, counts.controls, 1)
	testing.expect_value(t, counts.class_data, 1)
}

@(test)
declaration_nodes_keep_concrete_clause_fields :: proc(t: ^testing.T) {
	source := `TYPES ty_i TYPE i.
CONSTANTS c_i TYPE i VALUE 1.
RANGES r_matnr FOR mara-matnr.
PARAMETERS p_count TYPE i DEFAULT 1.
SELECT-OPTIONS s_matnr FOR mara-matnr.
CONTROLS tc TYPE TABLEVIEW USING SCREEN 100.
CLASS-DATA gv TYPE i VALUE 0.`
	parsed := parse(source, "decl_fields.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	types := parsed.root.stmts[0].derived_stmt.(^ast.Types_Decl)
	constants := parsed.root.stmts[1].derived_stmt.(^ast.Constants_Decl)
	ranges := parsed.root.stmts[2].derived_stmt.(^ast.Ranges_Decl)
	parameters := parsed.root.stmts[3].derived_stmt.(^ast.Parameters_Decl)
	options := parsed.root.stmts[4].derived_stmt.(^ast.Select_Options_Decl)
	controls := parsed.root.stmts[5].derived_stmt.(^ast.Controls_Decl)
	class_data := parsed.root.stmts[6].derived_stmt.(^ast.Class_Data_Decl)

	testing.expect_value(t, len(types.types), 1)
	testing.expect_value(t, types.types[0].name.text, "ty_i")
	testing.expect(t, types.types[0].type_clause != nil)
	testing.expect_value(t, types.types[0].type_clause.form, ast.Data_Type_Form.Type)
	testing.expect_value(t, len(constants.constants), 1)
	testing.expect(t, constants.constants[0].value_clause != nil)
	testing.expect(t, ranges.ranges[0].for_expr != nil)
	range_ref := ranges.ranges[0].for_expr.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, range_ref.base_name.text, "mara")
	testing.expect_value(t, range_ref.path[0].name.text, "matnr")
	testing.expect(t, parameters.parameters[0].default_expr != nil)
	testing.expect(t, options.options[0].for_expr != nil)
	testing.expect(t, controls.controls[0].using_screen != nil)
	testing.expect(t, class_data.decls[0].value_clause != nil)
}

@(test)
ranges_for_type_reference_keeps_base_and_path :: proc(t: ^testing.T) {
	source := `RANGES: gr_mblnr FOR string, gr_date FOR sy-datum.`
	parsed := parse(source, "ranges_for_type_ref.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	ranges := parsed.root.stmts[0].derived_stmt.(^ast.Ranges_Decl)
	testing.expect_value(t, len(ranges.ranges), 2)

	string_ref := ranges.ranges[0].for_expr.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, string_ref.name.text, "string")
	testing.expect_value(t, string_ref.base_name.text, "string")

	date_ref := ranges.ranges[1].for_expr.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, date_ref.base_name.text, "sy")
	testing.expect_value(t, len(date_ref.path), 1)
	testing.expect_value(t, date_ref.path[0].name.text, "datum")
	testing.expect_value(t, date_ref.path[0].selector, ast.Selector_Op.Dash)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
data_and_class_data_share_clause_shape_for_parse_and_print :: proc(t: ^testing.T) {
	source := `DATA: BEGIN OF gs_data OCCURS 1, field(4) LENGTH 4 TYPE c VALUE 'A' READ-ONLY, INCLUDE STRUCTURE textpool AS data_part RENAMING WITH SUFFIX dsu, END OF gs_data.
CLASS-DATA: BEGIN OF gt_data OCCURS 2, field(5) LENGTH 5 TYPE c VALUE 'B' READ-ONLY, INCLUDE STRUCTURE textpool AS class_part RENAMING WITH SUFFIX csu, END OF gt_data.`
	parsed := parse(source, "data_class_data_clauses.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	data := parsed.root.stmts[0].derived_stmt.(^ast.Data_Chained_Decl)
	class_data := parsed.root.stmts[1].derived_stmt.(^ast.Class_Data_Decl)

	testing.expect_value(t, len(data.decls), 4)
	testing.expect_value(t, data.decls[0].kind, ast.Decl_Clause_Kind.Begin_Group)
	testing.expect(t, data.decls[0].occurs != nil)
	testing.expect(t, data.decls[1].paren_length != nil)
	testing.expect_value(t, len(data.decls[1].length_clauses), 1)
	testing.expect(t, data.decls[1].type_clause != nil)
	testing.expect(t, data.decls[1].value_clause != nil)
	testing.expect(t, .Read_Only in data.decls[1].flags)
	testing.expect_value(t, data.decls[2].kind, ast.Decl_Clause_Kind.Include_Structure)
	testing.expect_value(t, data.decls[2].as_name.text, "data_part")
	testing.expect_value(t, data.decls[2].renaming_suffix.text, "dsu")
	testing.expect_value(t, data.decls[3].kind, ast.Decl_Clause_Kind.End_Group)

	testing.expect_value(t, len(class_data.decls), 4)
	testing.expect_value(t, class_data.decls[0].kind, ast.Decl_Clause_Kind.Begin_Group)
	testing.expect(t, class_data.decls[0].occurs != nil)
	testing.expect(t, class_data.decls[1].paren_length != nil)
	testing.expect_value(t, len(class_data.decls[1].length_clauses), 1)
	testing.expect(t, class_data.decls[1].type_clause != nil)
	testing.expect(t, class_data.decls[1].value_clause != nil)
	testing.expect(t, .Read_Only in class_data.decls[1].flags)
	testing.expect_value(t, class_data.decls[2].kind, ast.Decl_Clause_Kind.Include_Structure)
	testing.expect_value(t, class_data.decls[2].as_name.text, "class_part")
	testing.expect_value(t, class_data.decls[2].renaming_suffix.text, "csu")
	testing.expect_value(t, class_data.decls[3].kind, ast.Decl_Clause_Kind.End_Group)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
selection_screen_declaration_names_are_limited_to_eight_characters :: proc(t: ^testing.T) {
	source := `PARAMETERS p_too_long TYPE i.
SELECT-OPTIONS so_too_long FOR mara-matnr.`
	parsed := parse(source, "selection_screen_decl_name_length.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 2)
	expect_error_contains(t, parsed, "parameter name can be up to eight characters long")
	expect_error_contains(t, parsed, "select-option name can be up to eight characters long")
	testing.expect_value(
		t,
		source[parsed.errors[0].range.start:parsed.errors[0].range.end],
		"p_too_long",
	)
	testing.expect_value(
		t,
		source[parsed.errors[1].range.start:parsed.errors[1].range.end],
		"so_too_long",
	)
}

@(test)
abap_declaration_names_are_limited_to_thirty_characters :: proc(t: ^testing.T) {
	source := `REPORT zlen_general_30.

DATA abcdefghijabcdefghijabcdefghija TYPE i.
DATA(abcdefghijabcdefghijabcdefghijb) = 1.

TYPES: BEGIN OF ty_s,
         abcdefghijabcdefghijabcdefghijc TYPE i,
       END OF ty_s.

FIELD-SYMBOLS <abcdefghijabcdefghijabcdefghijd> TYPE any.`
	parsed := parse(source, "abap_name_length.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 4)
	expect_error_contains(t, parsed, "name can be up to 30 characters long")
}

@(test)
declaration_additions_keep_concrete_fields :: proc(t: ^testing.T) {
	source := `CONSTANTS lcv_max(14) TYPE p DECIMALS 7 VALUE '0.9999999'.
FIELD-SYMBOLS <line> LIKE LINE OF itab.
FIELD-SYMBOLS <lt_records> TYPE STANDARD TABLE.
PARAMETERS p_flag AS CHECKBOX DEFAULT 'X' MODIF ID md.
PARAMETERS p_mode RADIOBUTTON GROUP g01 USER-COMMAND upd LOWER CASE OBLIGATORY.
SELECT-OPTIONS s_matnr FOR mara-matnr NO-DISPLAY VISIBLE LENGTH 20 DEFAULT 'A' TO 'Z' OPTION BT SIGN I MATCHCODE OBJECT /sttp/h_loc_gln MEMORY ID gln MODIF ID grp.`
	parsed := parse(source, "decl_additions.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	constants := parsed.root.stmts[0].derived_stmt.(^ast.Constants_Decl)
	field_line := parsed.root.stmts[1].derived_stmt.(^ast.Field_Symbols_Decl)
	field_table := parsed.root.stmts[2].derived_stmt.(^ast.Field_Symbols_Decl)
	checkbox := parsed.root.stmts[3].derived_stmt.(^ast.Parameters_Decl)
	radio := parsed.root.stmts[4].derived_stmt.(^ast.Parameters_Decl)
	options := parsed.root.stmts[5].derived_stmt.(^ast.Select_Options_Decl)

	testing.expect(t, constants.constants[0].paren_length != nil)
	testing.expect_value(t, len(constants.constants[0].length_clauses), 1)
	testing.expect_value(t, constants.constants[0].length_clauses[0].kind, ast.Length_Clause_Kind.Decimals)
	testing.expect_value(t, field_line.field_symbols[0].type_clause.form, ast.Data_Type_Form.Like_Line_Of)
	testing.expect_value(t, field_table.field_symbols[0].type_clause.form, ast.Data_Type_Form.Standard_Table)
	testing.expect(t, .As_Checkbox in checkbox.parameters[0].flags)
	testing.expect(t, checkbox.parameters[0].default_expr != nil)
	checkbox_modif_id, checkbox_has_modif_id := checkbox.parameters[0].modif_id.?
	radio_group, radio_has_group := radio.parameters[0].radiobutton_group.?
	radio_command, radio_has_command := radio.parameters[0].user_command.?
	testing.expect(t, checkbox_has_modif_id)
	testing.expect(t, radio_has_group)
	testing.expect(t, radio_has_command)
	testing.expect_value(t, checkbox_modif_id.text, "md")
	testing.expect_value(t, radio_group.text, "g01")
	testing.expect_value(t, radio_command.text, "upd")
	testing.expect(t, .Lower_Case in radio.parameters[0].flags)
	testing.expect(t, .Obligatory in radio.parameters[0].flags)
	testing.expect(t, .No_Display in options.options[0].flags)
	testing.expect(t, options.options[0].visible_length != nil)
	testing.expect(t, options.options[0].to_expr != nil)
	option, has_option := options.options[0].option.?
	sign, has_sign := options.options[0].sign.?
	testing.expect(t, has_option)
	testing.expect(t, has_sign)
	testing.expect_value(t, option.text, "BT")
	testing.expect_value(t, sign.text, "I")
	testing.expect(t, options.options[0].matchcode_object != nil)
	testing.expect(t, options.options[0].memory_id != nil)
	option_modif_id, option_has_modif_id := options.options[0].modif_id.?
	testing.expect(t, option_has_modif_id)
	testing.expect_value(t, option_modif_id.text, "grp")
}

@(test)
select_options_request_additions_keep_target_tokens :: proc(t: ^testing.T) {
	source := `SELECT-OPTIONS s_date FOR sy-datum HELP-REQUEST FOR LOW VALUE-REQUEST FOR HIGH.`
	parsed := parse(source, "select_option_requests.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Options_Decl)
	clause := stmt.options[0]
	help_request, has_help_request := clause.help_request.?
	value_request, has_value_request := clause.value_request.?
	testing.expect(t, has_help_request)
	testing.expect(t, has_value_request)
	testing.expect_value(t, help_request.kind, ast.Selection_Request_Kind.Help_Request)
	testing.expect_value(t, value_request.kind, ast.Selection_Request_Kind.Value_Request)
	testing.expect_value(t, help_request.target.text, "LOW")
	testing.expect_value(t, source[help_request.target.range.start:help_request.target.range.end], "LOW")
	testing.expect_value(t, value_request.target.text, "HIGH")
	testing.expect_value(t, source[value_request.target.range.start:value_request.target.range.end], "HIGH")
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
parameters_declaration_keeps_structural_print_facts :: proc(t: ^testing.T) {
	source := `PARAMETER p_one TYPE c LENGTH 3 AS CHECKBOX DEFAULT 'X' MODIF ID md.
PARAMETERS: p_two TYPE string.`
	parsed := parse(source, "parameter_structural_facts.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	parameter := parsed.root.stmts[0].derived_stmt.(^ast.Parameters_Decl)
	parameters := parsed.root.stmts[1].derived_stmt.(^ast.Parameters_Decl)
	clause := parameter.parameters[0]

	testing.expect_value(t, parameter.keyword.text, "PARAMETER")
	testing.expect(t, !parameter.has_colon)
	testing.expect_value(t, parameters.keyword.text, "PARAMETERS")
	testing.expect(t, parameters.has_colon)
	testing.expect_value(t, len(clause.parts), 5)
	testing.expect_value(t, clause.parts[0], ast.Parameter_Clause_Part.Type_Clause)
	testing.expect_value(t, clause.parts[1], ast.Parameter_Clause_Part.Length_Clause)
	testing.expect_value(t, clause.parts[2], ast.Parameter_Clause_Part.As_Checkbox)
	testing.expect_value(t, clause.parts[3], ast.Parameter_Clause_Part.Default_Clause)
	testing.expect_value(t, clause.parts[4], ast.Parameter_Clause_Part.Modif_Id)
	modif_id, has_modif_id := clause.modif_id.?
	testing.expect(t, has_modif_id)
	testing.expect_value(t, source[modif_id.range.start:modif_id.range.end], "md")
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
pool_declarations_keep_concrete_nodes :: proc(t: ^testing.T) {
	source := `TYPE-POOLS: cxtab, vimty.
FUNCTION-POOL zfg MESSAGE-ID sv.`
	parsed := parse(source, "pool_decls.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.type_pools, 1)
	testing.expect_value(t, counts.function_pool, 1)

	pools := parsed.root.stmts[0].derived_stmt.(^ast.Type_Pools_Decl)
	pool := parsed.root.stmts[1].derived_stmt.(^ast.Function_Pool_Decl)
	testing.expect_value(t, len(pools.pools), 2)
	testing.expect_value(t, pools.pools[0].text, "cxtab")
	testing.expect_value(t, pool.name.text, "zfg")
	testing.expect_value(t, pool.message_id.text, "sv")
}

@(test)
structured_declaration_entries_keep_kinds :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_outer, INCLUDE TYPE ty_inner AS inner RENAMING WITH SUFFIX _x, field TYPE STANDARD TABLE OF string WITH DEFAULT KEY, END OF ty_outer.
DATA: BEGIN OF itab OCCURS 10, field(4) TYPE c VALUE 'A', INCLUDE STRUCTURE textpool, END OF itab.
CONSTANTS: BEGIN OF c_pair, a TYPE c VALUE IS INITIAL, END OF c_pair.`
	parsed := parse(source, "structured_decls.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	types := parsed.root.stmts[0].derived_stmt.(^ast.Types_Decl)
	data := parsed.root.stmts[1].derived_stmt.(^ast.Data_Chained_Decl)
	constants := parsed.root.stmts[2].derived_stmt.(^ast.Constants_Decl)

	testing.expect_value(t, types.types[0].kind, ast.Decl_Clause_Kind.Begin_Group)
	testing.expect_value(t, types.types[1].kind, ast.Decl_Clause_Kind.Include_Type)
	testing.expect_value(t, types.types[1].depth, 1)
	testing.expect_value(t, types.types[1].as_name.text, "inner")
	testing.expect_value(t, types.types[1].renaming_suffix.text, "_x")
	testing.expect_value(t, types.types[2].type_clause.form, ast.Data_Type_Form.Standard_Table)
	testing.expect_value(t, types.types[2].depth, 1)
	testing.expect_value(t, types.types[3].kind, ast.Decl_Clause_Kind.End_Group)
	testing.expect_value(t, types.types[3].depth, 0)
	testing.expect_value(t, data.decls[0].kind, ast.Decl_Clause_Kind.Begin_Group)
	testing.expect(t, data.decls[0].occurs != nil)
	testing.expect(t, data.decls[1].paren_length != nil)
	testing.expect_value(t, data.decls[1].depth, 1)
	testing.expect(t, data.decls[1].value_clause != nil)
	testing.expect_value(t, data.decls[2].kind, ast.Decl_Clause_Kind.Include_Structure)
	testing.expect_value(t, data.decls[2].depth, 1)
	testing.expect_value(t, constants.constants[0].kind, ast.Decl_Clause_Kind.Begin_Group)
	testing.expect_value(t, constants.constants[1].depth, 1)
	testing.expect(t, constants.constants[1].value_clause.is_initial)
}

@(test)
structured_declaration_escaped_keyword_name_is_normal_field :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF d010inc, !include TYPE string, END OF d010inc.`
	parsed := parse(source, "escaped_keyword_field.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	types := parsed.root.stmts[0].derived_stmt.(^ast.Types_Decl)
	testing.expect_value(t, types.types[1].kind, ast.Decl_Clause_Kind.Normal)
	testing.expect_value(t, types.types[1].name.text, "include")
	testing.expect_value(t, types.types[1].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr).name.text, "string")
}

@(test)
structured_type_components_allow_begin_and_end_names :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_code_range,
  begin TYPE i,
  end TYPE i,
END OF ty_code_range.
TYPES ty_code_ranges TYPE SORTED TABLE OF ty_code_range WITH UNIQUE KEY begin.`
	parsed := parse(source, "keyword_components.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	range_decl := parsed.root.stmts[0].derived_stmt.(^ast.Types_Decl)
	table_decl := parsed.root.stmts[1].derived_stmt.(^ast.Types_Decl)

	testing.expect_value(t, len(range_decl.types), 4)
	testing.expect_value(t, range_decl.types[1].kind, ast.Decl_Clause_Kind.Normal)
	testing.expect_value(t, range_decl.types[1].name.text, "begin")
	testing.expect_value(t, range_decl.types[1].type_clause.form, ast.Data_Type_Form.Type)
	testing.expect_value(t, range_decl.types[2].kind, ast.Decl_Clause_Kind.Normal)
	testing.expect_value(t, range_decl.types[2].name.text, "end")
	testing.expect_value(t, range_decl.types[3].kind, ast.Decl_Clause_Kind.End_Group)

	table_ref := table_decl.types[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, table_ref.name.text, "ty_code_range")
	testing.expect(t, table_ref.key != nil)
	testing.expect_value(t, table_ref.key.kind, ast.Type_Ref_Key_Kind.Unique)
	testing.expect_value(t, table_ref.key.components[0].text, "begin")
}

@(test)
table_type_key_clause_keeps_precise_identifier_ranges :: proc(t: ^testing.T) {
	source := `TYPES:
  BEGIN OF ty_order_map,
    odata_property TYPE string,
  END OF ty_order_map,
  tt_order_map TYPE HASHED TABLE OF ty_order_map
    WITH UNIQUE KEY odata_property.`
	parsed := parse(source, "table_key_ranges.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	decl := parsed.root.stmts[0].derived_stmt.(^ast.Types_Decl)
	ref := decl.types[3].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)

	testing.expect_value(t, ref.name.text, "ty_order_map")
	testing.expect_value(t, source[ref.base_name.range.start:ref.base_name.range.end], "ty_order_map")
	testing.expect(t, ref.key != nil)
	if ref.key != nil {
		testing.expect_value(t, ref.key.components[0].text, "odata_property")
		testing.expect_value(t, source[ref.key.components[0].range.start:ref.key.components[0].range.end], "odata_property")
	}
}

@(test)
structured_type_keyword_head_requires_of_when_not_component :: proc(t: ^testing.T) {
	parsed := parse("TYPES: BEGIN ty_code_range, field TYPE i.", "bad_keyword_group.abap", context.allocator)

	expect_error_contains(t, parsed, "expected OF after BEGIN")
}

@(test)
types_structured_declaration_reports_missing_comma_before_end :: proc(t: ^testing.T) {
	source := `TYPES:
  BEGIN OF ty_line,
    field TYPE string
  END OF ty_line.`
	parsed := parse(source, "types_missing_comma_before_end.abap", context.allocator)

	expect_error_contains(t, parsed, "expected ',' between TYPES clauses")
	types := parsed.root.stmts[0].derived_stmt.(^ast.Types_Decl)
	testing.expect_value(t, len(types.types), 3)
	testing.expect_value(t, types.types[0].kind, ast.Decl_Clause_Kind.Begin_Group)
	testing.expect_value(t, types.types[1].name.text, "field")
	testing.expect_value(t, types.types[1].depth, 1)
	testing.expect_value(t, types.types[2].kind, ast.Decl_Clause_Kind.End_Group)
}

@(test)
types_structured_declaration_continues_across_include_periods :: proc(t: ^testing.T) {
	source := `TYPES:
  BEGIN OF ty_bus_msg.
  INCLUDE TYPE etobj_key.
TYPES:
  bus_msg_no TYPE c LENGTH 1,
  arbgb TYPE arbgb,
  END OF ty_bus_msg,
  ty_bus_msgs TYPE STANDARD TABLE OF ty_bus_msg.`
	parsed := parse(source, "split_type_structure.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, len(parsed.root.stmts), 1)
	types := parsed.root.stmts[0].derived_stmt.(^ast.Types_Decl)
	testing.expect_value(t, len(types.types), 6)
	testing.expect_value(t, types.types[0].kind, ast.Decl_Clause_Kind.Begin_Group)
	testing.expect_value(t, types.types[0].name.text, "ty_bus_msg")
	testing.expect_value(t, types.types[1].kind, ast.Decl_Clause_Kind.Include_Type)
	testing.expect_value(t, types.types[1].depth, 1)
	testing.expect_value(t, types.types[2].name.text, "bus_msg_no")
	testing.expect_value(t, types.types[2].depth, 1)
	testing.expect_value(t, types.types[4].kind, ast.Decl_Clause_Kind.End_Group)
	testing.expect_value(t, types.types[4].depth, 0)
	testing.expect_value(t, types.types[5].name.text, "ty_bus_msgs")
	testing.expect_value(t, types.types[5].depth, 0)
	table_ref := types.types[5].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, table_ref.name.text, "ty_bus_msg")
}

@(test)
types_chained_declaration_reports_clause_after_period :: proc(t: ^testing.T) {
	source := `TYPES:
  ty_type TYPE c.

  ty_another_type TYPE c.`
	parsed := parse(source, "types_clause_after_period.abap", context.allocator)

	expect_error_contains(t, parsed, "expected ',' between TYPES clauses")
	types := parsed.root.stmts[0].derived_stmt.(^ast.Types_Decl)
	testing.expect_value(t, len(types.types), 1)
	testing.expect_value(t, types.types[0].name.text, "ty_type")
}

@(test)
types_recovery_keeps_later_clause_and_following_statement :: proc(t: ^testing.T) {
	source := `TYPES:
  tr_docnum TYPE RANGE OF string,
  BEGIN OF ty_line,
    field TYPE string,
  END OF ty_line
  ty_something TYPE string
DATA lt_delivery_header TYPE STANDARD TABLE OF ty_line.`
	parsed := parse(source, "types_recovery.abap", context.allocator)

	expect_error_contains(t, parsed, "expected ',' between TYPES clauses")
	expect_error_contains(t, parsed, "expected '.' after TYPES declaration")
	testing.expect_value(t, len(parsed.root.stmts), 2)

	types := parsed.root.stmts[0].derived_stmt.(^ast.Types_Decl)
	data := single_data_branch(parsed.root.stmts[1])

	testing.expect_value(t, len(types.types), 5)
	testing.expect_value(t, types.types[0].name.text, "tr_docnum")
	testing.expect_value(t, types.types[0].type_clause.form, ast.Data_Type_Form.Range_Of)
	testing.expect_value(t, types.types[1].kind, ast.Decl_Clause_Kind.Begin_Group)
	testing.expect_value(t, types.types[2].name.text, "field")
	testing.expect_value(t, types.types[3].kind, ast.Decl_Clause_Kind.End_Group)
	testing.expect_value(t, types.types[4].name.text, "ty_something")
	testing.expect_value(t, types.types[4].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr).name.text, "string")
	testing.expect_value(t, data.name.text, "lt_delivery_header")
	testing.expect_value(t, data.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr).name.text, "ty_line")
}

@(test)
data_chain_recovery_keeps_clause_after_missing_comma :: proc(t: ^testing.T) {
	source := `DATA: lv_var1 TYPE c
      lv_var2 TYPE c.`
	parsed := parse(source, "data_missing_comma.abap", context.allocator)

	expect_error_contains(t, parsed, "expected ',' between DATA declarations")
	testing.expect_value(t, len(parsed.root.stmts), 1)

	data := parsed.root.stmts[0].derived_stmt.(^ast.Data_Chained_Decl)
	testing.expect_value(t, len(data.decls), 2)
	testing.expect_value(t, data.decls[0].name.text, "lv_var1")
	testing.expect_value(t, data.decls[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr).name.text, "c")
	testing.expect_value(t, data.decls[1].name.text, "lv_var2")
	testing.expect_value(t, data.decls[1].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr).name.text, "c")
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), "DATA: lv_var1 TYPE c, lv_var2 TYPE c.")
}

@(test)
data_common_part_delimiters_mark_ast_fact :: proc(t: ^testing.T) {
	source := `DATA: BEGIN OF COMMON PART fm06lcbe.
DATA: END OF COMMON PART.
DATA: BEGIN OF common, field TYPE i, END OF common.`
	parsed := parse(source, "common_part_decls.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	begin := parsed.root.stmts[0].derived_stmt.(^ast.Data_Chained_Decl)
	end := parsed.root.stmts[1].derived_stmt.(^ast.Data_Chained_Decl)
	normal := parsed.root.stmts[2].derived_stmt.(^ast.Data_Chained_Decl)

	testing.expect_value(t, begin.decls[0].kind, ast.Decl_Clause_Kind.Begin_Group)
	testing.expect(t, .Common_Part_Delimiter in begin.decls[0].flags)
	testing.expect_value(t, begin.decls[0].name.text, "fm06lcbe")
	testing.expect_value(t, end.decls[0].kind, ast.Decl_Clause_Kind.End_Group)
	testing.expect(t, .Common_Part_Delimiter in end.decls[0].flags)
	testing.expect_value(t, end.decls[0].name.text, "")
	testing.expect_value(t, normal.decls[0].kind, ast.Decl_Clause_Kind.Begin_Group)
	testing.expect(t, !(.Common_Part_Delimiter in normal.decls[0].flags))
	testing.expect_value(t, normal.decls[0].name.text, "common")
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
table_and_range_type_references_are_retained :: proc(t: ^testing.T) {
	source := `TYPES ty_range TYPE RANGE OF sy-datum.
TYPES ty_tab TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
TYPES ty_any TYPE ANY TABLE OF string.
FIELD-SYMBOLS <lt> LIKE SORTED TABLE OF <ls> WITH UNIQUE KEY id key.
FIELD-SYMBOLS <any> TYPE ANY TABLE.
DATA it_index TYPE INDEX TABLE.
DATA mv_text TYPE string READ-ONLY.`
	parsed := parse(source, "type_refs.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	range_decl := parsed.root.stmts[0].derived_stmt.(^ast.Types_Decl)
	table_decl := parsed.root.stmts[1].derived_stmt.(^ast.Types_Decl)
	any_type_decl := parsed.root.stmts[2].derived_stmt.(^ast.Types_Decl)
	field_decl := parsed.root.stmts[3].derived_stmt.(^ast.Field_Symbols_Decl)
	any_decl := parsed.root.stmts[4].derived_stmt.(^ast.Field_Symbols_Decl)
	index_decl := single_data_branch(parsed.root.stmts[5])
	data_decl := single_data_branch(parsed.root.stmts[6])

	testing.expect_value(t, range_decl.types[0].type_clause.form, ast.Data_Type_Form.Range_Of)
	testing.expect_value(t, table_decl.types[0].type_clause.form, ast.Data_Type_Form.Hashed_Table)
	table_ref := table_decl.types[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, table_ref.name.text, "string")
	testing.expect(t, table_ref.key != nil)
	testing.expect_value(t, table_ref.key.kind, ast.Type_Ref_Key_Kind.Unique)
	testing.expect_value(t, table_ref.key.components[0].text, "table_line")
	testing.expect_value(t, any_type_decl.types[0].type_clause.form, ast.Data_Type_Form.Any_Table)
	testing.expect(t, any_type_decl.types[0].type_clause.table_has_of)
	any_type_ref := any_type_decl.types[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, any_type_ref.name.text, "string")
	testing.expect_value(t, field_decl.field_symbols[0].type_clause.form, ast.Data_Type_Form.Like_Sorted_Table)
	field_ref := field_decl.field_symbols[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, field_ref.key.kind, ast.Type_Ref_Key_Kind.Unique)
	testing.expect_value(t, len(field_ref.key.components), 2)
	testing.expect_value(t, any_decl.field_symbols[0].type_clause.form, ast.Data_Type_Form.Any_Table)
	testing.expect(t, any_decl.field_symbols[0].type_clause.type_ref == nil)
	testing.expect_value(t, index_decl.type_clause.form, ast.Data_Type_Form.Index_Table)
	testing.expect(t, index_decl.type_clause.type_ref == nil)
	testing.expect(t, .Read_Only in data_decl.flags)
	printed := ast.print_node(parsed.root, context.allocator)
	testing.expect_value(t, printed, source)
}

@(test)
type_reference_base_and_path_are_ast_fields :: proc(t: ^testing.T) {
	source := `DATA lv_date LIKE sy-datum.
DATA lr_item TYPE REF TO lif_demo=>ty_item.
DATA ls_asset TYPE lif_demo~ty_asset.
DATA lv_phase LIKE lif_demo=>scriptcallphase_enum.
TYPES ty_field TYPE zstruc-field.
TYPES ty_tab TYPE STANDARD TABLE OF REF TO lif_demo=>ty_item WITH KEY table_line.
FIELD-SYMBOLS <item> LIKE LINE OF mr_source_tree->*.`
	parsed := parse(source, "type_ref_paths.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	date_decl := single_data_branch(parsed.root.stmts[0])
	item_decl := single_data_branch(parsed.root.stmts[1])
	asset_decl := single_data_branch(parsed.root.stmts[2])
	phase_decl := single_data_branch(parsed.root.stmts[3])
	field_decl := parsed.root.stmts[4].derived_stmt.(^ast.Types_Decl)
	table_decl := parsed.root.stmts[5].derived_stmt.(^ast.Types_Decl)
	deref_decl := parsed.root.stmts[6].derived_stmt.(^ast.Field_Symbols_Decl)

	date_ref := date_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, date_ref.base_name.text, "sy")
	testing.expect_value(t, source[date_ref.base_name.range.start:date_ref.base_name.range.end], "sy")
	testing.expect_value(t, len(date_ref.path), 1)
	testing.expect_value(t, date_ref.path[0].name.text, "datum")
	testing.expect_value(t, date_ref.path[0].selector, ast.Selector_Op.Dash)
	testing.expect_value(t, source[date_ref.path[0].name.range.start:date_ref.path[0].name.range.end], "datum")

	item_ref := item_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, item_ref.base_name.text, "lif_demo")
	testing.expect_value(t, item_ref.path[0].name.text, "ty_item")
	testing.expect_value(t, item_ref.path[0].selector, ast.Selector_Op.Fat_Arrow)

	asset_ref := asset_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, asset_ref.base_name.text, "lif_demo")
	testing.expect_value(t, asset_ref.path[0].name.text, "ty_asset")
	testing.expect_value(t, asset_ref.path[0].selector, ast.Selector_Op.Tilde)

	phase_ref := phase_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, phase_ref.base_name.text, "lif_demo")
	testing.expect_value(t, phase_ref.path[0].name.text, "scriptcallphase_enum")
	testing.expect_value(t, phase_ref.path[0].selector, ast.Selector_Op.Fat_Arrow)

	field_ref := field_decl.types[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, field_ref.base_name.text, "zstruc")
	testing.expect_value(t, field_ref.path[0].name.text, "field")
	testing.expect_value(t, field_ref.path[0].selector, ast.Selector_Op.Dash)

	table_ref := table_decl.types[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, table_ref.source.text, "REF TO lif_demo=>ty_item WITH KEY table_line")
	testing.expect_value(t, table_ref.name.text, "REF TO lif_demo=>ty_item")
	testing.expect_value(t, source[table_ref.name.range.start:table_ref.name.range.end], "REF TO lif_demo=>ty_item")
	testing.expect(t, table_ref.is_ref)
	testing.expect_value(t, table_ref.base_name.text, "lif_demo")
	testing.expect_value(t, table_ref.path[0].name.text, "ty_item")
	testing.expect_value(t, table_ref.path[0].selector, ast.Selector_Op.Fat_Arrow)

	deref_ref := deref_decl.field_symbols[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, deref_ref.base_name.text, "mr_source_tree")
	testing.expect_value(t, deref_ref.path[0].name.text, "*")
	testing.expect_value(t, deref_ref.path[0].selector, ast.Selector_Op.Arrow)
}

@(test)
type_ref_ranges_stop_before_declaration_additions :: proc(t: ^testing.T) {
	source := `DATA int_eket LIKE beket OCCURS 0 WITH HEADER LINE.
DATA lv_value TYPE i VALUE 1.
DATA lv_len TYPE c LENGTH 3.
DATA lv_dec TYPE p DECIMALS 2.
DATA mv_text TYPE string READ-ONLY.
PARAMETERS p_count TYPE i DEFAULT 1.`
	parsed := parse(source, "type_ref_addition_bounds.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	occurs_decl := single_data_branch(parsed.root.stmts[0])
	value_decl := single_data_branch(parsed.root.stmts[1])
	length_decl := single_data_branch(parsed.root.stmts[2])
	decimal_decl := single_data_branch(parsed.root.stmts[3])
	read_only_decl := single_data_branch(parsed.root.stmts[4])
	default_decl := parsed.root.stmts[5].derived_stmt.(^ast.Parameters_Decl)

	occurs_ref := occurs_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	value_ref := value_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	length_ref := length_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	decimal_ref := decimal_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	read_only_ref := read_only_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	default_ref := default_decl.parameters[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)

	testing.expect_value(t, occurs_ref.name.text, "beket")
	testing.expect_value(t, occurs_ref.base_name.text, "beket")
	testing.expect_value(t, source[occurs_ref.range.start:occurs_ref.range.end], "beket")
	testing.expect(t, occurs_decl.occurs != nil)
	testing.expect(t, .With_Header_Line in occurs_decl.flags)
	testing.expect_value(t, source[value_ref.range.start:value_ref.range.end], "i")
	testing.expect(t, value_decl.value_clause != nil)
	testing.expect_value(t, source[length_ref.range.start:length_ref.range.end], "c")
	testing.expect_value(t, len(length_decl.length_clauses), 1)
	testing.expect_value(t, source[decimal_ref.range.start:decimal_ref.range.end], "p")
	testing.expect_value(t, decimal_decl.length_clauses[0].kind, ast.Length_Clause_Kind.Decimals)
	testing.expect_value(t, source[read_only_ref.range.start:read_only_ref.range.end], "string")
	testing.expect(t, .Read_Only in read_only_decl.flags)
	testing.expect_value(t, source[default_ref.range.start:default_ref.range.end], "i")
	testing.expect(t, default_decl.parameters[0].default_expr != nil)
}

@(test)
structure_component_like_occurs_keeps_bounded_type_ref :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ldb_stack_line,
         dyns_fields LIKE rsdsfields OCCURS 0,
       END OF ldb_stack_line.`
	parsed := parse(source, "structure_like_occurs.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	decl := parsed.root.stmts[0].derived_stmt.(^ast.Types_Decl)
	field := decl.types[1]
	ref := field.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)

	testing.expect_value(t, field.name.text, "dyns_fields")
	testing.expect_value(t, ref.name.text, "rsdsfields")
	testing.expect_value(t, source[ref.range.start:ref.range.end], "rsdsfields")
	testing.expect(t, field.occurs != nil)
	if field.occurs != nil {
		testing.expect_value(t, source[field.occurs.range.start:field.occurs.range.end], "0")
	}
}

@(test)
occurs_requires_count_before_header_line :: proc(t: ^testing.T) {
	parsed := parse("DATA rows LIKE row OCCURS WITH HEADER LINE.", "bad_occurs.abap", context.allocator)

	testing.expect(t, len(parsed.errors) > 0)
}

@(test)
table_key_clauses_stay_inside_type_refs_but_header_line_does_not :: proc(t: ^testing.T) {
	source := `TYPES ty_def TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
TYPES ty_unique TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
DATA itab TYPE STANDARD TABLE OF i WITH HEADER LINE.`
	parsed := parse(source, "type_ref_keys_vs_header.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	default_decl := parsed.root.stmts[0].derived_stmt.(^ast.Types_Decl)
	unique_decl := parsed.root.stmts[1].derived_stmt.(^ast.Types_Decl)
	header_stmt := single_data_stmt(parsed.root.stmts[2])
	header_decl := header_stmt.decls[0]

	default_ref := default_decl.types[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	unique_ref := unique_decl.types[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	header_ref := header_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)

	testing.expect_value(t, default_ref.name.text, "string")
	testing.expect_value(t, source[default_ref.range.start:default_ref.range.end], "string WITH DEFAULT KEY")
	testing.expect_value(t, default_ref.key.kind, ast.Type_Ref_Key_Kind.Default)
	testing.expect_value(t, unique_ref.name.text, "string")
	testing.expect_value(t, source[unique_ref.range.start:unique_ref.range.end], "string WITH UNIQUE KEY table_line")
	testing.expect_value(t, unique_ref.key.kind, ast.Type_Ref_Key_Kind.Unique)
	testing.expect_value(t, unique_ref.key.components[0].text, "table_line")
	testing.expect_value(t, header_ref.name.text, "i")
	testing.expect_value(t, source[header_ref.range.start:header_ref.range.end], "i")
	testing.expect(t, .With_Header_Line in header_decl.flags)
	testing.expect_value(t, ast.print_node(header_stmt, context.allocator), "DATA itab TYPE STANDARD TABLE OF i WITH HEADER LINE.")
}

@(test)
table_initial_size_stays_outside_type_ref :: proc(t: ^testing.T) {
	source := `TYPES tsg_cons_prxs TYPE STANDARD TABLE OF prx_r3name INITIAL SIZE 5.`
	parsed := parse(source, "table_initial_size.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	decl := parsed.root.stmts[0].derived_stmt.(^ast.Types_Decl)
	clause := decl.types[0].type_clause
	ref := clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)

	testing.expect_value(t, ref.name.text, "prx_r3name")
	testing.expect_value(t, source[ref.range.start:ref.range.end], "prx_r3name")
	testing.expect(t, clause.initial_size != nil)
	if clause.initial_size != nil {
		testing.expect_value(t, source[clause.initial_size.range.start:clause.initial_size.range.end], "5")
	}
	testing.expect_value(t, ast.print_node(decl, context.allocator), source)
}

@(test)
table_initial_size_shape_is_validated :: proc(t: ^testing.T) {
	missing_size := parse("TYPES ty TYPE STANDARD TABLE OF string INITIAL 5.", "missing_size.abap", context.allocator)
	non_table := parse("TYPES ty TYPE i INITIAL SIZE 5.", "non_table_initial_size.abap", context.allocator)

	expect_error_contains(t, missing_size, "expected SIZE after INITIAL")
	expect_error_contains(t, non_table, "INITIAL SIZE only valid for table types")
}

@(test)
declaration_unknown_additions_are_diagnosed :: proc(t: ^testing.T) {
	source := `DATA lv TYPE i BOGUS VALUE 1.
TYPES ty TYPE string UNKNOWN.
PARAMETERS p_text TYPE string LOWERCASE.`
	parsed := parse(source, "decl_unknown_additions.abap", context.allocator)

	expect_error_contains(t, parsed, "unexpected declaration addition")
	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, "syntax error: unexpected declaration addition"),
		3,
	)
}

@(test)
non_data_chained_declarations_recover_missing_commas :: proc(t: ^testing.T) {
	source := `CONSTANTS: c_one TYPE i VALUE 1
  c_two TYPE i VALUE 2.
FIELD-SYMBOLS: <one> TYPE any
  <two> TYPE any.`
	parsed := parse(source, "decl_missing_commas.abap", context.allocator)
	counts := count_nodes(parsed.root)

	expect_error_contains(t, parsed, "expected ',' between CONSTANTS declarations")
	expect_error_contains(t, parsed, "expected ',' between FIELD-SYMBOLS declarations")
	testing.expect_value(t, counts.constants, 1)
	testing.expect_value(t, counts.field_symbols, 1)
	constants := parsed.root.stmts[0].derived_stmt.(^ast.Constants_Decl)
	field_symbols := parsed.root.stmts[1].derived_stmt.(^ast.Field_Symbols_Decl)
	testing.expect_value(t, len(constants.constants), 2)
	testing.expect_value(t, len(field_symbols.field_symbols), 2)
}

@(test)
type_references_report_unclosed_delimiters :: proc(t: ^testing.T) {
	parsed := parse("DATA lv TYPE (string.", "decl_bad_type_ref_delimiter.abap", context.allocator)

	expect_error_contains(t, parsed, "expected ')' before end of raw operand")
}
