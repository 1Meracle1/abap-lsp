package abap_frontend_parser

import "src:ast"
import "src:tokenizer"

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
	#partial switch _ in node.derived {
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

	testing.expect_value(t, len(parsed.root.stmts), 3)
	testing.expect_value(t, counts.invalid_stmt, 3)
	testing.expect(t, len(parsed.errors) > 0)
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
