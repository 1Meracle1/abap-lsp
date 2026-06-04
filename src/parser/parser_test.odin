package abap_frontend_parser

import "src:ast"
import "src:tokenizer"

import "core:mem/virtual"
import "core:strings"
import "core:testing"

Node_Counts :: struct {
	binary:        int,
	selector:      int,
	interface_qualified_selector: int,
	table:         int,
	template:      int,
	interpolation: int,
	format_spec:   int,
	constructor:   int,
	is_predicate:  int,
	instance_of:   int,
	between_expr:  int,
	let_expr:      int,
	constructor_when: int,
	constructor_else: int,
	constructor_for:  int,
	constructor_for_group: int,
	constructor_init: int,
	constructor_next: int,
	constructor_named: int,
	constructor_base: int,
	constructor_lines: int,
	constructor_optional: int,
	constructor_mapping: int,
	constructor_mapping_assignment: int,
	constructor_except: int,
	host_expr:      int,
	data_decl:     int,
	data_inline:   int,
	types_decl:    int,
	constants:     int,
	field_symbols: int,
	statics:       int,
	tables_decl:   int,
	ranges:        int,
	parameters:    int,
	select_options: int,
	controls:      int,
	class_data:    int,
	type_pools:    int,
	function_pool: int,
	include_stmt:  int,
	assign:        int,
	downcast:      int,
	clear:         int,
	refresh:       int,
	free:          int,
	unassign:      int,
	move_stmt:     int,
	add_stmt:      int,
	concatenate:   int,
	perform:       int,
	call_stmt:     int,
	submit:        int,
	message:       int,
	write:         int,
	assert_stmt:   int,
	check_stmt:    int,
	flow_stmt:     int,
	transaction_stmt: int,
	describe_stmt: int,
	runtime_stmt:  int,
	set_handler:   int,
	bit_stmt:      int,
	locale_stmt:   int,
	set_cursor:    int,
	receive_results: int,
	raise_stmt:    int,
	authority_check: int,
	field_groups:  int,
	insert_dummy:  int,
	field_stmt:    int,
	assign_field:  int,
	create_object: int,
	create_data:   int,
	text_transform: int,
	wait_stmt:     int,
	convert_time_stamp: int,
	list_control: int,
	line_stmt:     int,
	macro_def:     int,
	macro_call:    int,
	oop_simple:    int,
	if_stmt:       int,
	case_stmt:     int,
	while_stmt:    int,
	do_stmt:       int,
	loop_stmt:     int,
	at_stmt:       int,
	try_stmt:      int,
	class_decl:    int,
	interface_decl: int,
	method_decl:   int,
	form_decl:     int,
	function_decl: int,
	module_decl:   int,
	event_block:   int,
	enhancement:   int,
	test_seam:     int,
	test_injection: int,
	select_stmt:   int,
	open_cursor:   int,
	fetch_stmt:    int,
	close_cursor:  int,
	insert_stmt:   int,
	append_stmt:   int,
	modify_stmt:   int,
	sort_stmt:     int,
	update_stmt:   int,
	delete_stmt:   int,
	read_table:    int,
	dataset_stmt:  int,
	report_stmt:   int,
	textpool_stmt: int,
	exec_sql_stmt: int,
	generate_stmt: int,
	invalid_stmt:  int,
}

@(test)
comments_attach_to_statement_nodes_for_printing :: proc(t: ^testing.T) {
	source := `" keep this comment
DATA lv TYPE i. " inline comment`
	parsed := parse(source, "comments.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0]
	testing.expect_value(t, len(stmt.leading_comments), 1)
	testing.expect_value(t, stmt.leading_comments[0], `" keep this comment`)
	testing.expect_value(t, stmt.trailing_comment, `" inline comment`)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

count_visit :: proc(v: ^ast.Visitor, node: ^ast.Node) -> ^ast.Visitor {
	if node == nil {
		return v
	}
	counts := cast(^Node_Counts)v.data
	#partial switch n in node.derived {
	case ^ast.Binary_Expr:
		counts.binary += 1
	case ^ast.Selector_Expr:
		counts.selector += 1
	case ^ast.Interface_Qualified_Selector_Expr:
		counts.interface_qualified_selector += 1
	case ^ast.Table_Expr:
		counts.table += 1
	case ^ast.Char_String_Template_Expr:
		counts.template += 1
	case ^ast.Template_Interpolation_Expr:
		counts.interpolation += 1
	case ^ast.Template_Format_Spec_Expr:
		counts.format_spec += 1
	case ^ast.Constructor_Expr:
		counts.constructor += 1
	case ^ast.Is_Predicate_Expr:
		counts.is_predicate += 1
	case ^ast.Instance_Of_Predicate_Expr:
		counts.instance_of += 1
	case ^ast.Between_Expr:
		counts.between_expr += 1
	case ^ast.Let_Expr:
		counts.let_expr += 1
	case ^ast.Constructor_When_Clause_Expr:
		counts.constructor_when += 1
	case ^ast.Constructor_Else_Clause_Expr:
		counts.constructor_else += 1
	case ^ast.Constructor_For_Clause_Expr:
		counts.constructor_for += 1
		if n.group_source != "" {
			counts.constructor_for_group += 1
		}
	case ^ast.Constructor_Init_Clause_Expr:
		counts.constructor_init += 1
	case ^ast.Constructor_Next_Clause_Expr:
		counts.constructor_next += 1
	case ^ast.Constructor_Named_Assignment_Expr:
		counts.constructor_named += 1
	case ^ast.Constructor_Base_Clause_Expr:
		counts.constructor_base += 1
	case ^ast.Constructor_Lines_Of_Clause_Expr:
		counts.constructor_lines += 1
	case ^ast.Constructor_Optional_Expr:
		counts.constructor_optional += 1
	case ^ast.Constructor_Corresponding_Mapping_Clause_Expr:
		counts.constructor_mapping += 1
	case ^ast.Constructor_Corresponding_Mapping_Assignment_Expr:
		counts.constructor_mapping_assignment += 1
	case ^ast.Constructor_Corresponding_Except_Clause_Expr:
		counts.constructor_except += 1
	case ^ast.Host_Expr:
		counts.host_expr += 1
	case ^ast.Data_Decl:
		counts.data_decl += 1
	case ^ast.Data_Inline_Decl:
		counts.data_inline += 1
	case ^ast.Types_Decl:
		counts.types_decl += 1
	case ^ast.Constants_Decl:
		counts.constants += 1
	case ^ast.Field_Symbols_Decl:
		counts.field_symbols += 1
	case ^ast.Statics_Decl:
		counts.statics += 1
	case ^ast.Tables_Decl:
		counts.tables_decl += 1
	case ^ast.Ranges_Decl:
		counts.ranges += 1
	case ^ast.Parameters_Decl:
		counts.parameters += 1
	case ^ast.Select_Options_Decl:
		counts.select_options += 1
	case ^ast.Controls_Decl:
		counts.controls += 1
	case ^ast.Class_Data_Decl:
		counts.class_data += 1
	case ^ast.Type_Pools_Decl:
		counts.type_pools += 1
	case ^ast.Function_Pool_Decl:
		counts.function_pool += 1
	case ^ast.Include_Stmt:
		counts.include_stmt += 1
	case ^ast.Assign_Stmt:
		counts.assign += 1
	case ^ast.Downcast_Assign_Stmt:
		counts.downcast += 1
	case ^ast.Clear_Stmt:
		counts.clear += 1
	case ^ast.Refresh_Stmt:
		counts.refresh += 1
	case ^ast.Free_Stmt:
		counts.free += 1
	case ^ast.Unassign_Stmt:
		counts.unassign += 1
	case ^ast.Move_Stmt:
		counts.move_stmt += 1
	case ^ast.Move_Corresponding_Stmt:
		counts.move_stmt += 1
	case ^ast.Add_Stmt:
		counts.add_stmt += 1
	case ^ast.Concatenate_Stmt:
		counts.concatenate += 1
	case ^ast.Perform_Stmt:
		counts.perform += 1
	case ^ast.Call_Stmt:
		counts.call_stmt += 1
	case ^ast.Submit_Stmt:
		counts.submit += 1
	case ^ast.Message_Stmt:
		counts.message += 1
	case ^ast.Write_Stmt:
		counts.write += 1
	case ^ast.Write_To_Stmt:
		counts.write += 1
	case ^ast.Assert_Stmt:
		counts.assert_stmt += 1
	case ^ast.Check_Stmt:
		counts.check_stmt += 1
	case ^ast.Flow_Stmt:
		counts.flow_stmt += 1
	case ^ast.Transaction_Stmt:
		counts.transaction_stmt += 1
	case ^ast.Describe_Stmt:
		counts.describe_stmt += 1
	case ^ast.Runtime_Stmt:
		counts.runtime_stmt += 1
	case ^ast.Set_Handler_Stmt:
		counts.set_handler += 1
	case ^ast.Bit_Stmt:
		counts.bit_stmt += 1
	case ^ast.Locale_Stmt:
		counts.locale_stmt += 1
	case ^ast.Set_Cursor_Stmt:
		counts.set_cursor += 1
	case ^ast.Receive_Results_Stmt:
		counts.receive_results += 1
	case ^ast.Raise_Stmt:
		counts.raise_stmt += 1
	case ^ast.Authority_Check_Stmt:
		counts.authority_check += 1
	case ^ast.Field_Groups_Stmt:
		counts.field_groups += 1
	case ^ast.Insert_Dummy_Stmt:
		counts.insert_dummy += 1
	case ^ast.Field_Stmt:
		counts.field_stmt += 1
	case ^ast.Assign_Field_Stmt:
		counts.assign_field += 1
	case ^ast.Create_Object_Stmt:
		counts.create_object += 1
	case ^ast.Create_Data_Stmt:
		counts.create_data += 1
	case ^ast.Text_Transform_Stmt:
		counts.text_transform += 1
	case ^ast.Wait_Stmt:
		counts.wait_stmt += 1
	case ^ast.Convert_Time_Stamp_Stmt:
		counts.convert_time_stamp += 1
	case ^ast.List_Control_Stmt:
		counts.list_control += 1
	case ^ast.Line_Stmt:
		counts.line_stmt += 1
	case ^ast.Macro_Def_Stmt:
		counts.macro_def += 1
	case ^ast.Macro_Call_Stmt:
		counts.macro_call += 1
	case ^ast.Oop_Simple_Stmt:
		counts.oop_simple += 1
	case ^ast.If_Stmt:
		counts.if_stmt += 1
	case ^ast.Case_Stmt:
		counts.case_stmt += 1
	case ^ast.While_Stmt:
		counts.while_stmt += 1
	case ^ast.Do_Stmt:
		counts.do_stmt += 1
	case ^ast.Loop_Stmt:
		counts.loop_stmt += 1
	case ^ast.At_Stmt:
		counts.at_stmt += 1
	case ^ast.Try_Stmt:
		counts.try_stmt += 1
	case ^ast.Class_Decl:
		counts.class_decl += 1
	case ^ast.Interface_Decl:
		counts.interface_decl += 1
	case ^ast.Method_Decl:
		counts.method_decl += 1
	case ^ast.Form_Decl:
		counts.form_decl += 1
	case ^ast.Function_Decl:
		counts.function_decl += 1
	case ^ast.Module_Decl:
		counts.module_decl += 1
	case ^ast.Event_Block_Stmt:
		counts.event_block += 1
	case ^ast.Enhancement_Stmt:
		counts.enhancement += 1
	case ^ast.Test_Seam_Stmt:
		counts.test_seam += 1
	case ^ast.Test_Injection_Stmt:
		counts.test_injection += 1
	case ^ast.Select_Stmt:
		counts.select_stmt += 1
	case ^ast.Open_Cursor_Stmt:
		counts.open_cursor += 1
	case ^ast.Fetch_Stmt:
		counts.fetch_stmt += 1
	case ^ast.Close_Cursor_Stmt:
		counts.close_cursor += 1
	case ^ast.Insert_Stmt:
		counts.insert_stmt += 1
	case ^ast.Append_Stmt:
		counts.append_stmt += 1
	case ^ast.Modify_Stmt:
		counts.modify_stmt += 1
	case ^ast.Sort_Stmt:
		counts.sort_stmt += 1
	case ^ast.Update_Stmt:
		counts.update_stmt += 1
	case ^ast.Delete_Stmt:
		counts.delete_stmt += 1
	case ^ast.Read_Table_Stmt:
		counts.read_table += 1
	case ^ast.Dataset_Stmt:
		counts.dataset_stmt += 1
	case ^ast.Report_Stmt:
		counts.report_stmt += 1
	case ^ast.Textpool_Stmt:
		counts.textpool_stmt += 1
	case ^ast.Exec_Sql_Stmt:
		counts.exec_sql_stmt += 1
	case ^ast.Generate_Stmt:
		counts.generate_stmt += 1
	case ^ast.Invalid_Stmt:
		counts.invalid_stmt += 1
	}
	return v
}

count_nodes :: proc(root: ^ast.Node) -> Node_Counts {
	counts := Node_Counts{}
	visitor := ast.Visitor{visit = count_visit, data = rawptr(&counts)}
	ast.walk(&visitor, root)
	return counts
}

error_contains :: proc(parsed: Parsed_File, needle: string) -> bool {
	for e in parsed.errors {
		if strings.contains(e.message, needle) {
			return true
		}
	}
	return false
}

expect_error_contains :: proc(t: ^testing.T, parsed: Parsed_File, needle: string) {
	testing.expect(t, error_contains(parsed, needle))
}

expect_no_error_contains :: proc(t: ^testing.T, parsed: Parsed_File, needle: string) {
	testing.expect(t, !error_contains(parsed, needle))
}

test_parser :: proc(source: string) -> Parser {
	return init_parser(source, "test.abap", context.allocator)
}

mutable_test_source :: proc(source: string) -> []byte {
	bytes := make([]byte, len(source), context.temp_allocator)
	copy(bytes, source)
	return bytes
}

overwrite_test_source :: proc(bytes: []byte) {
	for i in 0 ..< len(bytes) {
		bytes[i] = byte('#')
	}
}

parse_then_overwrite_source :: proc(source: string) -> Parsed_File {
	bytes := mutable_test_source(source)
	input := string(bytes)
	parsed := parse(input, "ownership.abap", context.allocator)
	overwrite_test_source(bytes)
	return parsed
}

clone_parse_after_source_overwrite :: proc(t: ^testing.T, source: string) -> ^ast.File {
	parse_arena: virtual.Arena
	_ = virtual.arena_init_growing(&parse_arena)
	defer virtual.arena_destroy(&parse_arena)

	bytes := mutable_test_source(source)
	input := string(bytes)
	parsed := parse(input, "clone_ownership.abap", virtual.arena_allocator(&parse_arena))
	overwrite_test_source(bytes)
	testing.expect_value(t, len(parsed.errors), 0)

	cloned := ast.clone_node(parsed.root, context.allocator).derived.(^ast.File)
	virtual.arena_free_all(&parse_arena)
	poison := make([]byte, 256 * 1024, virtual.arena_allocator(&parse_arena))
	overwrite_test_source(poison)
	return cloned
}

@(test)
parser_text_helpers_return_owned_strings :: proc(t: ^testing.T) {
	bytes := mutable_test_source("DATA lv_text TYPE string.")
	input := string(bytes)
	p := init_parser(input, "helpers.abap", context.allocator)

	name := parser_intern_token_name(&p, p.tokens[1])
	name_again := parser_intern_name(&p, tokenizer.token_lexeme(p.tokens[1], p.source))
	token_text := parser_clone_token_text(&p, p.tokens[3])
	range_text := parser_clone_range_text(&p, tokenizer.text_range(0, 4))
	overwrite_test_source(bytes)

	testing.expect_value(t, name, "lv_text")
	testing.expect_value(t, name_again, "lv_text")
	testing.expect_value(t, token_text, "string")
	testing.expect_value(t, range_text, "DATA")
}

@(test)
parsed_expression_strings_survive_source_overwrite :: proc(t: ^testing.T) {
	parsed := parse_then_overwrite_source("lv_result = 'A'.\nrv_text = |Hello { lv_value WIDTH = 5 }|.")

	testing.expect_value(t, len(parsed.errors), 0)
	first := parsed.root.stmts[0].derived_stmt.(^ast.Assign_Stmt)
	first_lhs := first.lhs.derived_expr.(^ast.Ident_Expr)
	first_rhs := first.rhs.derived_expr.(^ast.Literal_Expr)
	second := parsed.root.stmts[1].derived_stmt.(^ast.Assign_Stmt)
	second_lhs := second.lhs.derived_expr.(^ast.Ident_Expr)
	template := second.rhs.derived_expr.(^ast.Char_String_Template_Expr)
	template_lit := template.parts[0].derived_expr.(^ast.Template_Literal_Expr)
	template_interp := template.parts[1].derived_expr.(^ast.Template_Interpolation_Expr)
	template_spec := template_interp.format_specs[0].derived_expr.(^ast.Template_Format_Spec_Expr)

	testing.expect_value(t, first_lhs.name, "lv_result")
	testing.expect_value(t, first_rhs.value, "'A'")
	testing.expect_value(t, second_lhs.name, "rv_text")
	testing.expect_value(t, template_lit.literal, "Hello ")
	testing.expect_value(t, template_spec.name, "WIDTH")
}

@(test)
parsed_selector_call_and_inline_names_survive_source_overwrite :: proc(t: ^testing.T) {
	source := `rv_num = ls_row-1.
rv_value = lo_obj->method( EXPORTING iv_value = DATA(lv_inline) CHANGING cv_any = FIELD-SYMBOL(<fs_any>) ).`
	parsed := parse_then_overwrite_source(source)

	testing.expect_value(t, len(parsed.errors), 0)
	first := parsed.root.stmts[0].derived_stmt.(^ast.Assign_Stmt)
	selector := first.rhs.derived_expr.(^ast.Selector_Expr)
	selector_base := selector.base.derived_expr.(^ast.Ident_Expr)
	selector_field := selector.field.derived_expr.(^ast.Literal_Expr)
	second := parsed.root.stmts[1].derived_stmt.(^ast.Assign_Stmt)
	call := second.rhs.derived_expr.(^ast.Call_Expr)
	call_args := call.args.derived_expr.(^ast.Call_Arg_List_Expr)
	exporting := call_args.args[0].derived_expr.(^ast.Call_Arg_Section_Expr)
	changing := call_args.args[1].derived_expr.(^ast.Call_Arg_Section_Expr)
	data_arg := exporting.args[0].derived_expr.(^ast.Call_Named_Arg_Expr)
	field_arg := changing.args[0].derived_expr.(^ast.Call_Named_Arg_Expr)
	data_inline := data_arg.value.derived_expr.(^ast.Data_Inline_Name_Expr)
	field_inline := field_arg.value.derived_expr.(^ast.Field_Symbol_Inline_Name_Expr)

	testing.expect_value(t, selector_base.name, "ls_row")
	testing.expect_value(t, selector_field.value, "1")
	testing.expect_value(t, exporting.name, "EXPORTING")
	testing.expect_value(t, data_arg.name, "iv_value")
	testing.expect_value(t, data_inline.name, "lv_inline")
	testing.expect_value(t, changing.name, "CHANGING")
	testing.expect_value(t, field_arg.name, "cv_any")
	testing.expect_value(t, field_inline.name, "<fs_any>")
}

@(test)
parsed_type_ref_key_strings_survive_source_overwrite :: proc(t: ^testing.T) {
	source := `DATA lv_date LIKE sy-datum.
TYPES ty_tab TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.`
	parsed := parse_then_overwrite_source(source)

	testing.expect_value(t, len(parsed.errors), 0)
	data_decl := parsed.root.stmts[0].derived_stmt.(^ast.Data_Decl)
	date_ref := data_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	types_decl := parsed.root.stmts[1].derived_stmt.(^ast.Types_Decl)
	table_ref := types_decl.types[0].type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)

	testing.expect_value(t, date_ref.base_name, "sy")
	testing.expect_value(t, date_ref.path[0].name, "datum")
	testing.expect_value(t, table_ref.name, "string")
	testing.expect(t, table_ref.key != nil)
	testing.expect_value(t, table_ref.key.kind, ast.Type_Ref_Key_Kind.Unique)
	testing.expect_value(t, table_ref.key.components[0], "table_line")
}

@(test)
parsed_raw_operand_facts_survive_source_overwrite :: proc(t: ^testing.T) {
	source := `RAISE EVENT changed EXPORTING value = ls_row-field other = DATA(lv_raw).
ASSIGN COMPONENT lv_name OF STRUCTURE ls_row TO FIELD-SYMBOL(<fs_raw>).
CALL FUNCTION 'Z_READ'
  EXPORTING iv_in = ls_call-field
  IMPORTING ev_out = DATA(lv_out).`
	parsed := parse_then_overwrite_source(source)

	testing.expect_value(t, len(parsed.errors), 0)
	raise := parsed.root.stmts[0].derived_stmt.(^ast.Raise_Stmt)
	raise_target := raise.target.derived_expr.(^ast.Type_Ref_Expr)
	raise_args := raise.operands[0].derived_expr.(^ast.Type_Ref_Expr)
	assign := parsed.root.stmts[1].derived_stmt.(^ast.Assign_Field_Stmt)
	assign_component := assign.component.derived_expr.(^ast.Type_Ref_Expr)
	assign_structure := assign.structure.derived_expr.(^ast.Type_Ref_Expr)
	assign_target := assign.target.derived_expr.(^ast.Type_Ref_Expr)
	call := parsed.root.stmts[2].derived_stmt.(^ast.Call_Stmt)

	testing.expect_value(t, raise_target.raw_refs[0].name, "changed")
	testing.expect_value(t, raise_args.raw_decls[0].name, "lv_raw")
	testing.expect_value(t, raise_args.raw_refs[0].name, "ls_row")
	testing.expect_value(t, raise_args.raw_refs[0].path[0].name, "field")
	testing.expect_value(t, assign_component.raw_refs[0].name, "lv_name")
	testing.expect_value(t, assign_structure.raw_refs[0].name, "ls_row")
	testing.expect_value(t, assign_target.raw_decls[0].name, "<fs_raw>")
	testing.expect_value(t, call.named_args[0].raw_refs[0].name, "ls_call")
	testing.expect_value(t, call.named_args[0].raw_refs[0].path[0].name, "field")
	testing.expect_value(t, call.named_args[1].raw_decls[0].name, "lv_out")
}

@(test)
parsed_constructor_names_survive_source_overwrite :: proc(t: ^testing.T) {
	source := `DATA(lt_let) = VALUE #( LET lv_item = 'A' IN ( comp = lv_item ) ).
DATA(lt_group) = VALUE #( FOR ls_row IN GROUP lg_source ( item-field = ls_row-field ) ).
DATA(lt_lines) = VALUE #( LINES OF lt_rows USING KEY sec_key ).
DATA(ls_dst) = CORRESPONDING #( ls_src MAPPING dst_field = src_field EXCEPT skip ).`
	parsed := parse_then_overwrite_source(source)

	testing.expect_value(t, len(parsed.errors), 0)
	let_ctor := parsed.root.stmts[0].derived_stmt.(^ast.Data_Inline_Decl).expr.derived_expr.(^ast.Constructor_Expr)
	let_expr := let_ctor.args[0].derived_expr.(^ast.Let_Expr)
	let_binding := let_expr.bindings[0].derived_expr.(^ast.Constructor_Let_Binding_Expr)
	let_row := let_expr.body[0].derived_expr.(^ast.Call_Arg_List_Expr)
	let_assignment := let_row.args[0].derived_expr.(^ast.Constructor_Named_Assignment_Expr)
	group_ctor := parsed.root.stmts[1].derived_stmt.(^ast.Data_Inline_Decl).expr.derived_expr.(^ast.Constructor_Expr)
	for_clause := group_ctor.args[0].derived_expr.(^ast.Constructor_For_Clause_Expr)
	group_row := for_clause.body[0].derived_expr.(^ast.Call_Arg_List_Expr)
	group_assignment := group_row.args[0].derived_expr.(^ast.Constructor_Named_Assignment_Expr)
	lines_ctor := parsed.root.stmts[2].derived_stmt.(^ast.Data_Inline_Decl).expr.derived_expr.(^ast.Constructor_Expr)
	lines_clause := lines_ctor.args[0].derived_expr.(^ast.Constructor_Lines_Of_Clause_Expr)
	corresponding_ctor := parsed.root.stmts[3].derived_stmt.(^ast.Data_Inline_Decl).expr.derived_expr.(^ast.Constructor_Expr)
	mapping := corresponding_ctor.args[1].derived_expr.(^ast.Constructor_Corresponding_Mapping_Clause_Expr)
	mapping_assignment := mapping.assignments[0].derived_expr.(^ast.Constructor_Corresponding_Mapping_Assignment_Expr)
	except := mapping_assignment.except.derived_expr.(^ast.Constructor_Corresponding_Except_Clause_Expr)
	except_name := except.names[0].derived_expr.(^ast.Ident_Expr)

	testing.expect_value(t, let_binding.name, "lv_item")
	testing.expect_value(t, let_assignment.name, "comp")
	testing.expect_value(t, for_clause.variable, "ls_row")
	testing.expect_value(t, for_clause.group_source, "lg_source")
	testing.expect_value(t, group_assignment.name, "item-field")
	testing.expect_value(t, lines_clause.using_key, "sec_key")
	testing.expect_value(t, mapping_assignment.target, "dst_field")
	testing.expect_value(t, except_name.name, "skip")
}

@(test)
parsed_table_key_names_survive_source_overwrite :: proc(t: ^testing.T) {
	source := `READ TABLE lt_rows INTO ls_row INDEX lv_idx USING KEY array_index.
READ TABLE gt_rows INTO ls_row WITH TABLE KEY iso2 COMPONENTS langshort = iv_src.
READ TABLE rt_item INTO lr_item WITH KEY item-obj_type = lv_type.`
	parsed := parse_then_overwrite_source(source)

	testing.expect_value(t, len(parsed.errors), 0)
	static_read := parsed.root.stmts[0].derived_stmt.(^ast.Read_Table_Stmt)
	table_key_read := parsed.root.stmts[1].derived_stmt.(^ast.Read_Table_Stmt)
	component_key_read := parsed.root.stmts[2].derived_stmt.(^ast.Read_Table_Stmt)

	testing.expect_value(t, static_read.entries[0].using_key.name, "array_index")
	testing.expect_value(t, table_key_read.entries[0].key_name, "iso2")
	testing.expect_value(t, table_key_read.entries[0].key_values[0].name, "langshort")
	testing.expect_value(t, table_key_read.entries[0].key_values[0].path[0].name, "langshort")
	testing.expect_value(t, component_key_read.entries[0].key_values[0].name, "item-obj_type")
	testing.expect_value(t, component_key_read.entries[0].key_values[0].path[0].name, "item")
	testing.expect_value(t, component_key_read.entries[0].key_values[0].path[1].name, "obj_type")
}

@(test)
parsed_declaration_strings_survive_source_overwrite :: proc(t: ^testing.T) {
	source := `DATA lv_text TYPE string AS lv_alias RENAMING WITH SUFFIX suff.
DATA(ls_inline) = lv_text.
TYPES ty_text TYPE string AS ty_alias RENAMING WITH SUFFIX tys.
CONSTANTS c_text TYPE string VALUE 'A' AS c_alias RENAMING WITH SUFFIX cs.
STATICS st_text TYPE string AS st_alias RENAMING WITH SUFFIX sts.
CLASS-DATA gv_text TYPE string AS gv_alias RENAMING WITH SUFFIX gvs.
FIELD-SYMBOLS <fs_text> TYPE any.
TABLES mara.
RANGES r_date FOR sy-datum.
PARAMETERS p_one RADIOBUTTON GROUP g1 USER-COMMAND go MODIF ID mid.
SELECT-OPTIONS s_date FOR sy-datum OPTION EQ SIGN I MODIF ID sid HELP-REQUEST FOR LOW VALUE-REQUEST FOR HIGH.
CONTROLS tc_one TYPE TABLEVIEW USING SCREEN 100.
TYPE-POOLS abap.
FUNCTION-POOL zfg MESSAGE-ID zmsg.
INCLUDE zinc IF FOUND.`
	parsed := parse_then_overwrite_source(source)

	testing.expect_value(t, len(parsed.errors), 0)
	data_decl := parsed.root.stmts[0].derived_stmt.(^ast.Data_Decl)
	inline_decl := parsed.root.stmts[1].derived_stmt.(^ast.Data_Inline_Decl)
	types_decl := parsed.root.stmts[2].derived_stmt.(^ast.Types_Decl)
	constants_decl := parsed.root.stmts[3].derived_stmt.(^ast.Constants_Decl)
	statics_decl := parsed.root.stmts[4].derived_stmt.(^ast.Statics_Decl)
	class_data_decl := parsed.root.stmts[5].derived_stmt.(^ast.Class_Data_Decl)
	field_symbols_decl := parsed.root.stmts[6].derived_stmt.(^ast.Field_Symbols_Decl)
	tables_decl := parsed.root.stmts[7].derived_stmt.(^ast.Tables_Decl)
	ranges_decl := parsed.root.stmts[8].derived_stmt.(^ast.Ranges_Decl)
	parameters_decl := parsed.root.stmts[9].derived_stmt.(^ast.Parameters_Decl)
	select_options_decl := parsed.root.stmts[10].derived_stmt.(^ast.Select_Options_Decl)
	controls_decl := parsed.root.stmts[11].derived_stmt.(^ast.Controls_Decl)
	type_pools_decl := parsed.root.stmts[12].derived_stmt.(^ast.Type_Pools_Decl)
	function_pool_decl := parsed.root.stmts[13].derived_stmt.(^ast.Function_Pool_Decl)
	include_stmt := parsed.root.stmts[14].derived_stmt.(^ast.Include_Stmt)

	testing.expect_value(t, data_decl.name, "lv_text")
	testing.expect_value(t, data_decl.as_name, "lv_alias")
	testing.expect_value(t, data_decl.renaming_suffix, "suff")
	testing.expect_value(t, inline_decl.name, "ls_inline")
	testing.expect_value(t, types_decl.types[0].name, "ty_text")
	testing.expect_value(t, types_decl.types[0].as_name, "ty_alias")
	testing.expect_value(t, types_decl.types[0].renaming_suffix, "tys")
	testing.expect_value(t, constants_decl.constants[0].name, "c_text")
	testing.expect_value(t, constants_decl.constants[0].as_name, "c_alias")
	testing.expect_value(t, constants_decl.constants[0].renaming_suffix, "cs")
	testing.expect_value(t, statics_decl.statics[0].name, "st_text")
	testing.expect_value(t, statics_decl.statics[0].as_name, "st_alias")
	testing.expect_value(t, statics_decl.statics[0].renaming_suffix, "sts")
	testing.expect_value(t, class_data_decl.decls[0].name, "gv_text")
	testing.expect_value(t, class_data_decl.decls[0].as_name, "gv_alias")
	testing.expect_value(t, class_data_decl.decls[0].renaming_suffix, "gvs")
	testing.expect_value(t, field_symbols_decl.field_symbols[0].name, "<fs_text>")
	testing.expect_value(t, tables_decl.tables[0].name, "mara")
	testing.expect_value(t, ranges_decl.ranges[0].name, "r_date")
	testing.expect_value(t, parameters_decl.parameters[0].name, "p_one")
	testing.expect_value(t, parameters_decl.parameters[0].radiobutton_group.group, "g1")
	testing.expect_value(t, parameters_decl.parameters[0].user_command.command, "go")
	testing.expect_value(t, parameters_decl.parameters[0].modif_id.id, "mid")
	testing.expect_value(t, select_options_decl.options[0].name, "s_date")
	testing.expect_value(t, select_options_decl.options[0].option_clause.option, "EQ")
	testing.expect_value(t, select_options_decl.options[0].sign_clause.sign, "I")
	testing.expect_value(t, select_options_decl.options[0].modif_id.id, "sid")
	testing.expect_value(t, select_options_decl.options[0].help_request.target, "LOW")
	testing.expect_value(t, select_options_decl.options[0].value_request.target, "HIGH")
	testing.expect_value(t, controls_decl.controls[0].name, "tc_one")
	testing.expect_value(t, type_pools_decl.pools[0], "abap")
	testing.expect_value(t, function_pool_decl.name, "zfg")
	testing.expect_value(t, function_pool_decl.message_id, "zmsg")
	testing.expect_value(t, include_stmt.names[0].name, "zinc")
}

@(test)
parsed_oop_header_strings_survive_source_overwrite :: proc(t: ^testing.T) {
	source := `CLASS lcl_child DEFINITION INHERITING FROM lcl_base FRIENDS lcl_friend.
PUBLIC SECTION.
  METHODS run IMPORTING !iv_input TYPE string RETURNING VALUE(rv_out) TYPE string.
  METHODS lif_iface~run REDEFINITION.
  METHODS on_changed FOR EVENT changed OF lcl_source IMPORTING sender.
  ALIASES alias_run FOR lif_iface~run.
ENDCLASS.
METHOD lif_iface~run BY KERNEL MODULE zkernel.
ENDMETHOD.
FORM sub_form USING VALUE(iv_form) TYPE string CHANGING cv_form TYPE string.
ENDFORM.
FUNCTION z_func IMPORTING !iv_func TYPE string EXCEPTIONS failed = 1.
ENDFUNCTION.`
	parsed := parse_then_overwrite_source(source)

	testing.expect_value(t, len(parsed.errors), 0)
	class_decl := parsed.root.stmts[0].derived_stmt.(^ast.Class_Decl)
	methods_stmt := class_decl.body[1].derived_stmt.(^ast.Oop_Simple_Stmt)
	qualified_methods_stmt := class_decl.body[2].derived_stmt.(^ast.Oop_Simple_Stmt)
	event_stmt := class_decl.body[3].derived_stmt.(^ast.Oop_Simple_Stmt)
	aliases_stmt := class_decl.body[4].derived_stmt.(^ast.Oop_Simple_Stmt)
	method_decl := parsed.root.stmts[1].derived_stmt.(^ast.Method_Decl)
	form_decl := parsed.root.stmts[2].derived_stmt.(^ast.Form_Decl)
	function_decl := parsed.root.stmts[3].derived_stmt.(^ast.Function_Decl)

	testing.expect_value(t, class_decl.name, "lcl_child")
	testing.expect_value(t, class_decl.superclass_name, "lcl_base")
	testing.expect_value(t, class_decl.friends[0].name, "lcl_friend")
	testing.expect_value(t, methods_stmt.members[0].name, "run")
	testing.expect_value(t, methods_stmt.members[0].signatures[0].parameters[0].name, "iv_input")
	testing.expect_value(t, methods_stmt.members[0].signatures[1].parameters[0].name, "rv_out")
	testing.expect_value(t, qualified_methods_stmt.members[0].name, "lif_iface~run")
	testing.expect_value(t, qualified_methods_stmt.members[0].qualifier, "lif_iface")
	testing.expect_value(t, qualified_methods_stmt.members[0].member_name, "run")
	testing.expect_value(t, event_stmt.members[0].name, "on_changed")
	testing.expect_value(t, event_stmt.members[0].event_handler.event_name, "changed")
	testing.expect_value(t, event_stmt.members[0].signatures[0].parameters[0].name, "sender")
	testing.expect_value(t, aliases_stmt.aliases[0].name, "alias_run")
	testing.expect_value(t, aliases_stmt.aliases[0].target_interface_name, "lif_iface")
	testing.expect_value(t, aliases_stmt.aliases[0].target_member_name, "run")
	testing.expect_value(t, method_decl.name, "lif_iface~run")
	testing.expect_value(t, method_decl.qualifier, "lif_iface")
	testing.expect_value(t, method_decl.member_name, "run")
	testing.expect_value(t, method_decl.kernel_modules[0], "zkernel")
	testing.expect_value(t, form_decl.name, "sub_form")
	testing.expect_value(t, form_decl.form_parameters[0].name, "iv_form")
	testing.expect_value(t, form_decl.form_parameters[1].name, "cv_form")
	testing.expect_value(t, function_decl.name, "z_func")
	testing.expect_value(t, function_decl.function_parameters[0].name, "iv_func")
	testing.expect_value(t, function_decl.exceptions[0].name, "failed")
}

@(test)
parsed_call_submit_message_strings_survive_source_overwrite :: proc(t: ^testing.T) {
	source := `CALL FUNCTION 'Z_FM'
  EXPORTING iv_input = lv_value
  EXCEPTIONS system_failure = 1 MESSAGE lv_msg.
CALL METHOD lo_obj->run EXPORTING #1 = lv_pos iv_named = lv_named.
CALL TRANSFORMATION ztrans SOURCE XML lv_xml RESULT rv_result = lv_result.
SUBMIT zreport WITH p_matnr = lv_matnr AND RETURN.
MESSAGE e001(zmsg) WITH lv_msg.`
	parsed := parse_then_overwrite_source(source)

	testing.expect_value(t, len(parsed.errors), 0)
	call_function := parsed.root.stmts[0].derived_stmt.(^ast.Call_Stmt)
	call_method := parsed.root.stmts[1].derived_stmt.(^ast.Call_Stmt)
	call_transformation := parsed.root.stmts[2].derived_stmt.(^ast.Call_Stmt)
	submit := parsed.root.stmts[3].derived_stmt.(^ast.Submit_Stmt)
	message := parsed.root.stmts[4].derived_stmt.(^ast.Message_Stmt)

	testing.expect_value(t, call_function.named_args[0].name, "iv_input")
	testing.expect_value(t, call_function.named_args[1].name, "system_failure")
	testing.expect(t, call_function.named_args[1].message != nil)
	testing.expect_value(t, call_method.named_args[0].name, "#1")
	testing.expect_value(t, call_method.named_args[1].name, "iv_named")
	testing.expect_value(t, call_transformation.transformation_args[0].name, "XML")
	testing.expect_value(t, call_transformation.transformation_args[1].name, "rv_result")
	testing.expect_value(t, submit.options[0].name, "p_matnr")
	testing.expect_value(t, message.head.compact_class_name, "zmsg")
}

@(test)
parsed_sql_and_surface_names_survive_source_overwrite :: proc(t: ^testing.T) {
	source := `SELECT a~matnr AS mat_alias, count( DISTINCT a~matnr ) AS cnt_alias
  FROM mara AS a
  INNER JOIN makt AS b ON b~matnr = a~matnr
  ORDER BY a~matnr
  INTO TABLE @lt_rows.
MODIFY lt_rows FROM ls_row TRANSPORTING comp sub-comp.
SORT lt_rows BY comp sub-comp DESCENDING.
OPEN DATASET lv_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
REPORT zrep MESSAGE-ID zmsg.`
	parsed := parse_then_overwrite_source(source)

	testing.expect_value(t, len(parsed.errors), 0)
	select_stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	first_column := select_stmt.query.projections[0].derived_expr.(^ast.Sql_Column_Expr)
	sql_call := select_stmt.query.projections[1].derived_expr.(^ast.Sql_Call_Expr)
	modify_stmt := parsed.root.stmts[1].derived_stmt.(^ast.Modify_Stmt)
	sort_stmt := parsed.root.stmts[2].derived_stmt.(^ast.Sort_Stmt)
	dataset_stmt := parsed.root.stmts[3].derived_stmt.(^ast.Dataset_Stmt)
	report_stmt := parsed.root.stmts[4].derived_stmt.(^ast.Report_Stmt)

	testing.expect_value(t, first_column.qualifier, "a")
	testing.expect_value(t, first_column.name, "matnr")
	testing.expect_value(t, select_stmt.query.projection_clauses[0].alias, "mat_alias")
	testing.expect_value(t, sql_call.name, "count")
	testing.expect_value(t, select_stmt.query.projection_clauses[1].alias, "cnt_alias")
	testing.expect_value(t, select_stmt.query.source_clause.alias, "a")
	testing.expect_value(t, select_stmt.query.source_clause.joins[0].alias, "b")
	testing.expect_value(t, select_stmt.query.order_by_fields[0], "matnr")
	testing.expect_value(t, modify_stmt.transporting[0].name, "comp")
	testing.expect_value(t, modify_stmt.transporting[0].path[0].name, "comp")
	testing.expect_value(t, modify_stmt.transporting[1].name, "sub-comp")
	testing.expect_value(t, modify_stmt.transporting[1].path[0].name, "sub")
	testing.expect_value(t, modify_stmt.transporting[1].path[1].name, "comp")
	testing.expect_value(t, sort_stmt.fields[0].name, "comp")
	testing.expect_value(t, sort_stmt.fields[1].name, "sub-comp")
	testing.expect_value(t, dataset_stmt.encoding, "DEFAULT")
	testing.expect_value(t, report_stmt.message_id, "zmsg")
}

@(test)
cloned_parsed_ast_strings_survive_source_and_parse_arena_reuse :: proc(t: ^testing.T) {
	source := `" keep this comment
PARAMETERS p_text TYPE string. " inline comment
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
CLASS lcl_util DEFINITION LOAD.
CLASS lcl DEFINITION.
PUBLIC SECTION.
  METHODS run.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT OPTIONS READ-ONLY USING mara.
    lt_rows = SELECT matnr FROM mara;
  ENDMETHOD.
ENDCLASS.
DEFINE set_field.
  &1 = &2.
END-OF-DEFINITION.
EXEC SQL.
  SELECT * FROM mara
ENDEXEC.
SELECT a~matnr AS mat_alias FROM mara AS a INTO TABLE @lt_rows ORDER BY a~matnr.
READ TABLE lt_rows INTO ls_row WITH KEY item-obj_type = 'A'.
MODIFY lt_rows FROM ls_row TRANSPORTING comp sub-comp.
SORT lt_rows BY comp sub-comp DESCENDING.
DATA(lv_text) = |Hello { lv_value WIDTH = 5 }|.
DATA(lv_lit) = 'B'.
DATA lv_date LIKE sy-datum.`
	root := clone_parse_after_source_overwrite(t, source)

	parameters := root.stmts[0].derived_stmt.(^ast.Parameters_Decl)
	selection_screen := root.stmts[1].derived_stmt.(^ast.Selection_Screen_Stmt)
	load := root.stmts[2].derived_stmt.(^ast.Oop_Load_Stmt)
	class_def := root.stmts[3].derived_stmt.(^ast.Class_Decl)
	methods := class_def.body[1].derived_stmt.(^ast.Oop_Simple_Stmt)
	class_impl := root.stmts[4].derived_stmt.(^ast.Class_Decl)
	method := class_impl.body[0].derived_stmt.(^ast.Method_Decl)
	macro := root.stmts[5].derived_stmt.(^ast.Macro_Def_Stmt)
	exec_sql := root.stmts[6].derived_stmt.(^ast.Exec_Sql_Stmt)
	select_stmt := root.stmts[7].derived_stmt.(^ast.Select_Stmt)
	read_table := root.stmts[8].derived_stmt.(^ast.Read_Table_Stmt)
	modify_stmt := root.stmts[9].derived_stmt.(^ast.Modify_Stmt)
	sort_stmt := root.stmts[10].derived_stmt.(^ast.Sort_Stmt)
	template_decl := root.stmts[11].derived_stmt.(^ast.Data_Inline_Decl)
	literal_decl := root.stmts[12].derived_stmt.(^ast.Data_Inline_Decl)
	type_decl := root.stmts[13].derived_stmt.(^ast.Data_Decl)

	testing.expect_value(t, parameters.leading_comments[0], `" keep this comment`)
	testing.expect_value(t, parameters.trailing_comment, `" inline comment`)
	testing.expect_value(t, parameters.text, "PARAMETERS p_text TYPE string.")
	testing.expect_value(t, parameters.parameters[0].name, "p_text")
	testing.expect_value(t, selection_screen.text, "SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.")
	testing.expect_value(t, selection_screen.title_name, "text")
	testing.expect_value(t, load.name, "lcl_util")
	testing.expect_value(t, load.text, "CLASS lcl_util DEFINITION LOAD.")
	testing.expect_value(t, class_def.header_text, "CLASS lcl DEFINITION")
	testing.expect_value(t, methods.text, "METHODS run.")
	testing.expect_value(t, methods.members[0].name, "run")
	testing.expect_value(t, method.header_text, "METHOD run BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT OPTIONS READ-ONLY USING mara")
	testing.expect(t, strings.contains(method.amdp_body, "lt_rows = SELECT matnr FROM mara;"))
	testing.expect_value(t, macro.name, "set_field")
	testing.expect(t, strings.contains(macro.body, "&1 = &2."))
	testing.expect(t, strings.contains(exec_sql.body, "SELECT * FROM mara"))
	testing.expect_value(t, select_stmt.query.projection_clauses[0].alias, "mat_alias")
	testing.expect_value(t, select_stmt.query.source_clause.alias, "a")
	testing.expect_value(t, select_stmt.query.order_by_fields[0], "matnr")
	testing.expect_value(t, read_table.entries[0].key_values[0].name, "item-obj_type")
	testing.expect_value(t, read_table.entries[0].key_values[0].path[0].name, "item")
	testing.expect_value(t, read_table.entries[0].key_values[0].path[1].name, "obj_type")
	read_value := read_table.entries[0].key_values[0].value.derived_expr.(^ast.Literal_Expr)
	testing.expect_value(t, read_value.value, "'A'")
	testing.expect_value(t, modify_stmt.transporting[1].name, "sub-comp")
	testing.expect_value(t, modify_stmt.transporting[1].path[0].name, "sub")
	testing.expect_value(t, modify_stmt.transporting[1].path[1].name, "comp")
	testing.expect_value(t, sort_stmt.fields[1].name, "sub-comp")
	template := template_decl.expr.derived_expr.(^ast.Char_String_Template_Expr)
	template_lit := template.parts[0].derived_expr.(^ast.Template_Literal_Expr)
	template_interp := template.parts[1].derived_expr.(^ast.Template_Interpolation_Expr)
	template_spec := template_interp.format_specs[0].derived_expr.(^ast.Template_Format_Spec_Expr)
	testing.expect_value(t, template_decl.name, "lv_text")
	testing.expect_value(t, template_lit.literal, "Hello ")
	testing.expect_value(t, template_spec.name, "WIDTH")
	literal := literal_decl.expr.derived_expr.(^ast.Literal_Expr)
	testing.expect_value(t, literal.value, "'B'")
	type_ref := type_decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect_value(t, type_ref.base_name, "sy")
	testing.expect_value(t, type_ref.path[0].name, "datum")
}

@(test)
expect_token_mismatch_does_not_advance :: proc(t: ^testing.T) {
	p := test_parser("DATA lv.")

	tok := expect_token(&p, .Period)

	testing.expect_value(t, tok.kind, tokenizer.Token_Kind.Ident)
	testing.expect_value(t, p.index, 0)
	testing.expect(t, at_keyword(&p, "DATA"))
	testing.expect_value(t, len(p.errors), 1)
}

@(test)
expect_token_match_advances :: proc(t: ^testing.T) {
	p := test_parser(".")

	tok := expect_token(&p, .Period)

	testing.expect_value(t, p.index, 1)
	testing.expect_value(t, tok.kind, tokenizer.Token_Kind.Period)
	testing.expect_value(t, len(p.errors), 0)
}

@(test)
top_level_loop_makes_progress_on_unexpected_tokens :: proc(t: ^testing.T) {
	parsed := parse("@ @ .", "test.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.root.stmts), 2)
	testing.expect_value(t, counts.invalid_stmt, 2)
	testing.expect(t, len(parsed.errors) > 0)
}

@(test)
empty_statements_are_ignored :: proc(t: ^testing.T) {
	parsed := parse(".\nDATA lv TYPE i..\n.", "empty_statements.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, len(parsed.root.stmts), 1)
}

@(test)
missing_period_invalidates_recognized_statement :: proc(t: ^testing.T) {
	parsed := parse("DATA lv", "test.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.root.stmts), 1)
	_, ok := parsed.root.stmts[0].derived_stmt.(^ast.Invalid_Stmt)
	testing.expect(t, ok)
	testing.expect_value(t, counts.data_decl, 0)
	testing.expect_value(t, counts.invalid_stmt, 1)
	expect_error_contains(t, parsed, "expected '.'")
}

@(test)
missing_period_does_not_swallow_following_simple_statement :: proc(t: ^testing.T) {
	parsed := parse(`DATA first
DATA second.`, "test.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.root.stmts), 2)
	_, first_invalid := parsed.root.stmts[0].derived_stmt.(^ast.Invalid_Stmt)
	second, second_data := parsed.root.stmts[1].derived_stmt.(^ast.Data_Decl)
	testing.expect(t, first_invalid)
	testing.expect(t, second_data)
	testing.expect_value(t, second.name, "second")
	testing.expect_value(t, counts.data_decl, 1)
	testing.expect_value(t, counts.invalid_stmt, 1)
	expect_error_contains(t, parsed, "expected '.'")
}

@(test)
missing_period_diagnostics_point_after_previous_token :: proc(t: ^testing.T) {
	source := `DATA(lv_val) = 1
IF lv_val = 1
    RETURN.
ENDIF.`
	parsed := parse(source, "missing_periods.abap", context.allocator)
	counts := count_nodes(parsed.root)
	data_pos := strings.index(source, "\nIF")
	if_pos := strings.index(source, "\n    RETURN")
	found_data := false
	found_if := false

	for e in parsed.errors {
		if strings.contains(e.message, "expected '.' after inline DATA declaration") {
			testing.expect_value(t, e.range.start, data_pos)
			testing.expect_value(t, e.range.end, data_pos)
			found_data = true
		}
		if strings.contains(e.message, "expected '.' after IF condition") {
			testing.expect_value(t, e.range.start, if_pos)
			testing.expect_value(t, e.range.end, if_pos)
			found_if = true
		}
	}
	testing.expect(t, found_data)
	testing.expect(t, found_if)
	expect_no_error_contains(t, parsed, "unexpected ENDIF without matching IF")
	testing.expect_value(t, counts.if_stmt, 1)
}

@(test)
statement_list_stop_keywords_are_not_consumed :: proc(t: ^testing.T) {
	p := test_parser(`DATA lv.
ENDIF.`)
	stops := []string{"ENDIF"}

	stmts := parse_stmt_list_until(&p, stops)

	testing.expect_value(t, len(stmts), 1)
	testing.expect(t, at_keyword(&p, "ENDIF"))
}

@(test)
include_fragment_policy_suppresses_only_block_boundary_errors :: proc(t: ^testing.T) {
	strict_open := parse("IF lv_ok = abap_true.\n  lv_value = 1.", "open.abap", context.allocator)
	include_open := parse_with_diagnostic_policy(
		"IF lv_ok = abap_true.\n  lv_value = 1.",
		"open.abap",
		context.allocator,
		.Include_Fragment,
	)
	strict_close := parse("ENDIF.", "close.abap", context.allocator)
	include_close := parse_with_diagnostic_policy(
		"ENDIF.",
		"close.abap",
		context.allocator,
		.Include_Fragment,
	)
	malformed := parse_with_diagnostic_policy("IF .", "bad.abap", context.allocator, .Include_Fragment)

	expect_error_contains(t, strict_open, "expected ENDIF")
	testing.expect_value(t, len(include_open.errors), 0)
	expect_error_contains(t, strict_close, "unexpected ENDIF without matching IF")
	testing.expect_value(t, len(include_close.errors), 0)
	expect_error_contains(t, malformed, "expected condition after IF")
}

@(test)
stray_boundaries_recover_to_next_statement :: proc(t: ^testing.T) {
	parsed := parse(
		"ENDIF.\nDATA lv TYPE i.\nCATCH cx_root.\nDATA lv_other TYPE i.",
		"stray.abap",
		context.allocator,
	)
	counts := count_nodes(parsed.root)

	expect_error_contains(t, parsed, "unexpected ENDIF without matching IF")
	expect_error_contains(t, parsed, "unexpected CATCH without matching TRY")
	testing.expect_value(t, counts.data_decl, 2)
	testing.expect_value(t, counts.invalid_stmt, 2)
}

@(test)
unknown_significant_tokens_progress_one_at_a_time :: proc(t: ^testing.T) {
	parsed := parse(") ] DATA lv_after TYPE i.", "unknown.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, counts.invalid_stmt, 2)
	testing.expect_value(t, counts.data_decl, 1)
}
