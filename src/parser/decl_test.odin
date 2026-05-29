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
	testing.expect_value(t, types.types[0].name, "ty_i")
	testing.expect(t, types.types[0].type_clause != nil)
	testing.expect_value(t, types.types[0].type_clause.form, ast.Data_Type_Form.Type)
	testing.expect_value(t, len(constants.constants), 1)
	testing.expect(t, constants.constants[0].value_clause != nil)
	testing.expect(t, ranges.ranges[0].for_clause != nil)
	testing.expect(t, parameters.parameters[0].default_clause != nil)
	testing.expect(t, options.options[0].for_clause != nil)
	testing.expect(t, controls.controls[0].using_screen != nil)
	testing.expect(t, class_data.decls[0].value_clause != nil)
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
	testing.expect(t, checkbox.parameters[0].default_clause != nil)
	testing.expect_value(t, checkbox.parameters[0].modif_id.id, "md")
	testing.expect_value(t, radio.parameters[0].radiobutton_group.group, "g01")
	testing.expect_value(t, radio.parameters[0].user_command.command, "upd")
	testing.expect(t, .Lower_Case in radio.parameters[0].flags)
	testing.expect(t, .Obligatory in radio.parameters[0].flags)
	testing.expect(t, .No_Display in options.options[0].flags)
	testing.expect(t, options.options[0].visible_length != nil)
	testing.expect(t, options.options[0].to_clause != nil)
	testing.expect_value(t, options.options[0].option_clause.option, "BT")
	testing.expect_value(t, options.options[0].sign_clause.sign, "I")
	testing.expect(t, options.options[0].matchcode_object != nil)
	testing.expect(t, options.options[0].memory_id != nil)
	testing.expect_value(t, options.options[0].modif_id.id, "grp")
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
	testing.expect_value(t, pools.pools[0], "cxtab")
	testing.expect_value(t, pool.name, "zfg")
	testing.expect_value(t, pool.message_id, "sv")
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
	testing.expect_value(t, types.types[1].as_name, "inner")
	testing.expect_value(t, types.types[1].renaming_suffix, "_x")
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
types_structured_declaration_allows_end_without_component_comma :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF dd03p,
  decimals TYPE decimals,
  fieldname TYPE fieldname, " Field Name
  valexi TYPE valexi " Existence of fixed values
END OF dd03p.`
	parsed := parse(source, "dd03p.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	types := parsed.root.stmts[0].derived_stmt.(^ast.Types_Decl)
	testing.expect_value(t, len(types.types), 5)
	testing.expect_value(t, types.types[0].kind, ast.Decl_Clause_Kind.Begin_Group)
	testing.expect_value(t, types.types[1].depth, 1)
	testing.expect_value(t, types.types[1].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr).name, "decimals")
	testing.expect_value(t, types.types[3].name, "valexi")
	testing.expect_value(t, types.types[4].kind, ast.Decl_Clause_Kind.End_Group)
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
	testing.expect_value(t, begin.decls[0].name, "fm06lcbe")
	testing.expect_value(t, end.decls[0].kind, ast.Decl_Clause_Kind.End_Group)
	testing.expect(t, .Common_Part_Delimiter in end.decls[0].flags)
	testing.expect_value(t, end.decls[0].name, "")
	testing.expect_value(t, normal.decls[0].kind, ast.Decl_Clause_Kind.Begin_Group)
	testing.expect(t, !(.Common_Part_Delimiter in normal.decls[0].flags))
	testing.expect_value(t, normal.decls[0].name, "common")
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
table_and_range_type_references_are_retained :: proc(t: ^testing.T) {
	source := `TYPES ty_range TYPE RANGE OF sy-datum.
TYPES ty_tab TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
FIELD-SYMBOLS <lt> LIKE SORTED TABLE OF <ls> WITH UNIQUE KEY id key.
FIELD-SYMBOLS <any> TYPE ANY TABLE.
DATA it_index TYPE INDEX TABLE.
DATA mv_text TYPE string READ-ONLY.`
	parsed := parse(source, "type_refs.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	range_decl := parsed.root.stmts[0].derived_stmt.(^ast.Types_Decl)
	table_decl := parsed.root.stmts[1].derived_stmt.(^ast.Types_Decl)
	field_decl := parsed.root.stmts[2].derived_stmt.(^ast.Field_Symbols_Decl)
	any_decl := parsed.root.stmts[3].derived_stmt.(^ast.Field_Symbols_Decl)
	index_decl := parsed.root.stmts[4].derived_stmt.(^ast.Data_Decl)
	data_decl := parsed.root.stmts[5].derived_stmt.(^ast.Data_Decl)

	testing.expect_value(t, range_decl.types[0].type_clause.form, ast.Data_Type_Form.Range_Of)
	testing.expect_value(t, table_decl.types[0].type_clause.form, ast.Data_Type_Form.Hashed_Table)
	table_ref := table_decl.types[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, table_ref.name, "string")
	testing.expect(t, table_ref.key != nil)
	testing.expect_value(t, table_ref.key.kind, ast.Type_Ref_Key_Kind.Unique)
	testing.expect_value(t, table_ref.key.components[0], "table_line")
	testing.expect_value(t, field_decl.field_symbols[0].type_clause.form, ast.Data_Type_Form.Like_Sorted_Table)
	field_ref := field_decl.field_symbols[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, field_ref.key.kind, ast.Type_Ref_Key_Kind.Unique)
	testing.expect_value(t, len(field_ref.key.components), 2)
	testing.expect_value(t, any_decl.field_symbols[0].type_clause.form, ast.Data_Type_Form.Any_Table)
	testing.expect(t, any_decl.field_symbols[0].type_clause.type_ref == nil)
	testing.expect_value(t, index_decl.type_clause.form, ast.Data_Type_Form.Index_Table)
	testing.expect(t, index_decl.type_clause.type_ref == nil)
	testing.expect(t, data_decl.read_only)
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
	date_decl := parsed.root.stmts[0].derived_stmt.(^ast.Data_Decl)
	item_decl := parsed.root.stmts[1].derived_stmt.(^ast.Data_Decl)
	asset_decl := parsed.root.stmts[2].derived_stmt.(^ast.Data_Decl)
	phase_decl := parsed.root.stmts[3].derived_stmt.(^ast.Data_Decl)
	field_decl := parsed.root.stmts[4].derived_stmt.(^ast.Types_Decl)
	table_decl := parsed.root.stmts[5].derived_stmt.(^ast.Types_Decl)
	deref_decl := parsed.root.stmts[6].derived_stmt.(^ast.Field_Symbols_Decl)

	date_ref := date_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, date_ref.base_name, "sy")
	testing.expect_value(t, source[date_ref.base_range.start:date_ref.base_range.end], "sy")
	testing.expect_value(t, len(date_ref.path), 1)
	testing.expect_value(t, date_ref.path[0].name, "datum")
	testing.expect_value(t, date_ref.path[0].selector, ast.Selector_Op.Dash)
	testing.expect_value(t, source[date_ref.path[0].range.start:date_ref.path[0].range.end], "datum")

	item_ref := item_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, item_ref.base_name, "lif_demo")
	testing.expect_value(t, item_ref.path[0].name, "ty_item")
	testing.expect_value(t, item_ref.path[0].selector, ast.Selector_Op.Fat_Arrow)

	asset_ref := asset_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, asset_ref.base_name, "lif_demo")
	testing.expect_value(t, asset_ref.path[0].name, "ty_asset")
	testing.expect_value(t, asset_ref.path[0].selector, ast.Selector_Op.Tilde)

	phase_ref := phase_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, phase_ref.base_name, "lif_demo")
	testing.expect_value(t, phase_ref.path[0].name, "scriptcallphase_enum")
	testing.expect_value(t, phase_ref.path[0].selector, ast.Selector_Op.Fat_Arrow)

	field_ref := field_decl.types[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, field_ref.base_name, "zstruc")
	testing.expect_value(t, field_ref.path[0].name, "field")
	testing.expect_value(t, field_ref.path[0].selector, ast.Selector_Op.Dash)

	table_ref := table_decl.types[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, table_ref.name, "REF TO lif_demo=>ty_item")
	testing.expect(t, table_ref.is_ref)
	testing.expect_value(t, table_ref.base_name, "lif_demo")
	testing.expect_value(t, table_ref.path[0].name, "ty_item")
	testing.expect_value(t, table_ref.path[0].selector, ast.Selector_Op.Fat_Arrow)

	deref_ref := deref_decl.field_symbols[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, deref_ref.base_name, "mr_source_tree")
	testing.expect_value(t, deref_ref.path[0].name, "*")
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
	occurs_decl := parsed.root.stmts[0].derived_stmt.(^ast.Data_Decl)
	value_decl := parsed.root.stmts[1].derived_stmt.(^ast.Data_Decl)
	length_decl := parsed.root.stmts[2].derived_stmt.(^ast.Data_Decl)
	decimal_decl := parsed.root.stmts[3].derived_stmt.(^ast.Data_Decl)
	read_only_decl := parsed.root.stmts[4].derived_stmt.(^ast.Data_Decl)
	default_decl := parsed.root.stmts[5].derived_stmt.(^ast.Parameters_Decl)

	occurs_ref := occurs_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	value_ref := value_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	length_ref := length_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	decimal_ref := decimal_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	read_only_ref := read_only_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	default_ref := default_decl.parameters[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)

	testing.expect_value(t, occurs_ref.name, "beket")
	testing.expect_value(t, occurs_ref.base_name, "beket")
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
	testing.expect(t, read_only_decl.read_only)
	testing.expect_value(t, source[default_ref.range.start:default_ref.range.end], "i")
	testing.expect(t, default_decl.parameters[0].default_clause != nil)
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
	header_decl := parsed.root.stmts[2].derived_stmt.(^ast.Data_Decl)

	default_ref := default_decl.types[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	unique_ref := unique_decl.types[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	header_ref := header_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)

	testing.expect_value(t, default_ref.name, "string")
	testing.expect_value(t, source[default_ref.range.start:default_ref.range.end], "string WITH DEFAULT KEY")
	testing.expect_value(t, default_ref.key.kind, ast.Type_Ref_Key_Kind.Default)
	testing.expect_value(t, unique_ref.name, "string")
	testing.expect_value(t, source[unique_ref.range.start:unique_ref.range.end], "string WITH UNIQUE KEY table_line")
	testing.expect_value(t, unique_ref.key.kind, ast.Type_Ref_Key_Kind.Unique)
	testing.expect_value(t, unique_ref.key.components[0], "table_line")
	testing.expect_value(t, header_ref.name, "i")
	testing.expect_value(t, source[header_ref.range.start:header_ref.range.end], "i")
	testing.expect(t, .With_Header_Line in header_decl.flags)
	testing.expect_value(t, ast.print_node(header_decl, context.allocator), "DATA itab TYPE STANDARD TABLE OF i WITH HEADER LINE.")
}
