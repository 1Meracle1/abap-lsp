package abap_frontend_ast

import "src:tokenizer"

import "core:mem"
import "core:strings"

Print_Options :: struct {
	newline: string,
	indent:  string,
}

DEFAULT_PRINT_OPTIONS :: Print_Options {
	newline = "\n",
	indent  = "    ",
}

Printer :: struct {
	out:          ^strings.Builder,
	options:      Print_Options,
	indent_level: int,
	macro_args:   []^Expr,
}

print_node :: proc(node: ^Node, allocator: mem.Allocator, options := DEFAULT_PRINT_OPTIONS) -> string {
	out := strings.builder_make(allocator)
	write_node(&out, node, options)
	return strings.to_string(out)
}

write_node :: proc(out: ^strings.Builder, node: ^Node, options := DEFAULT_PRINT_OPTIONS) {
	p := Printer{out = out, options = options}
	emit_node(&p, node)
}

emit :: proc {
	emit_string,
	emit_token_text,
}

emit_string :: proc(p: ^Printer, text: string) {
	strings.write_string(p.out, text)
}

emit_token_text :: proc(p: ^Printer, token: Token_Text) {
	emit_string(p, token.text)
}

emit_space :: proc(p: ^Printer) {
	emit(p, " ")
}

emit_newline :: proc(p: ^Printer) {
	emit(p, p.options.newline)
	for _ in 0 ..< p.indent_level {
		emit(p, p.options.indent)
	}
}

emit_leading_trivia :: proc(p: ^Printer, node: ^Node) {
	for trivia in node.leading_trivia {
		emit(p, trivia.text)
		emit_newline(p)
	}
}

emit_node :: proc(p: ^Printer, node: ^Node) {
	if node == nil {
		return
	}
	emit_leading_trivia(p, node)
	switch n in node.derived {
	case ^File:
		emit_file(p, n)
	case ^Bad_Expr:
		emit(p, "?")
	case ^Char_String_Template_Expr:
		emit_template(p, n)
	case ^Template_Literal_Expr:
		emit(p, n.literal)
	case ^Template_Interpolation_Expr:
		emit_template_interpolation(p, n)
	case ^Template_Expr:
		emit_node(p, n.expr)
	case ^Template_Format_Spec_Expr:
		emit(p, n.name)
		emit(p, " = ")
		emit_node(p, n.value)
	case ^Binary_Expr:
		emit_node(p, n.left)
		emit_space(p)
		emit(p, binary_op_text(n.op))
		emit_space(p)
		emit_node(p, n.right)
	case ^Unary_Expr:
		op := unary_op_text(n.op)
		emit(p, op)
		if n.op == .Not {
			emit_space(p)
		}
		emit_node(p, n.expr)
	case ^Paren_Expr:
		emit(p, "( ")
		emit_node(p, n.expr)
		emit(p, " )")
	case ^Ident_Expr:
		emit(p, n.name)
	case ^Literal_Expr:
		emit(p, n.value)
	case ^Macro_Arg_Ref_Expr:
		if arg := macro_arg_replacement(p, n); arg != nil {
			emit_macro_arg_replacement(p, arg)
			break
		}
		emit(p, n.name)
	case ^Type_Ref_Expr:
		emit_type_ref_expr(p, n)
	case ^Dynamic_Call_Method_Target_Expr:
		emit_dynamic_call_method_target_expr(p, n)
	case ^Ole_Call_Method_Target_Expr:
		emit_ole_call_method_target_expr(p, n)
	case ^Host_Expr:
		if !n.implicit {
			emit(p, "@")
		}
		emit_node(p, n.value)
	case ^Table_Expr:
		emit_node(p, n.table)
		emit(p, "[")
		if len(n.selectors) > 0 {
			emit_space(p)
			emit_expr_list(p, n.selectors, " ")
			emit_space(p)
		}
		emit(p, "]")
	case ^Selector_Expr:
		emit_node(p, n.base)
		emit(p, selector_op_text(n.op))
		emit_node(p, n.field)
	case ^Interface_Qualified_Selector_Expr:
		emit_node(p, n.receiver)
		emit(p, selector_op_text(n.receiver_op))
		emit_node(p, n.interface)
		emit(p, "~")
		emit_node(p, n.member)
	case ^Substring_Expr:
		emit_node(p, n.base)
		if n.offset != nil {
			emit(p, "+")
			emit_node(p, n.offset)
		}
		if n.length != nil {
			emit(p, "(")
			emit_node(p, n.length)
			emit(p, ")")
		}
	case ^Call_Expr:
		emit_node(p, n.callee)
		emit_node(p, n.args)
	case ^Call_Arg_List_Expr:
		emit(p, "( ")
		emit_expr_list(p, n.args, " ")
		emit(p, " )")
	case ^Call_Arg_Section_Expr:
		emit(p, n.name)
		if len(n.args) > 0 {
			emit_space(p)
			emit_expr_list(p, n.args, " ")
		}
	case ^Call_Named_Arg_Expr:
		emit(p, n.name)
		emit(p, " = ")
		emit_node(p, n.value)
	case ^Call_Positional_Arg_Expr:
		emit_node(p, n.value)
	case ^Sql_Column_Expr:
		if n.qualifier.text != "" {
			emit(p, n.qualifier)
			emit(p, "~")
		}
		emit(p, n.name)
	case ^Sql_Star_Expr:
		if n.qualifier.text != "" {
			emit(p, n.qualifier)
			emit(p, "~")
		}
		emit(p, "*")
	case ^Sql_Call_Expr:
		emit(p, n.name)
		emit(p, "( ")
		if n.modifier != .None {
			emit(p, sql_call_modifier_text(n.modifier))
			if len(n.args) > 0 {
				emit_space(p)
			}
		}
		emit_expr_list(p, n.args, ", ")
		emit(p, " )")
	case ^Constructor_Expr:
		emit(p, constructor_kind_text(n.kind))
		emit_space(p)
		emit_node(p, n.type_ref)
		emit(p, "( ")
		emit_expr_list(p, n.args, " ")
		emit(p, " )")
	case ^Is_Predicate_Expr:
		emit_node(p, n.subject)
		emit(p, " IS ")
		if n.negated {
			emit(p, "NOT ")
		}
		emit(p, is_predicate_kind_text(n.kind))
	case ^Instance_Of_Predicate_Expr:
		emit_node(p, n.subject)
		emit(p, " IS ")
		if n.negated {
			emit(p, "NOT ")
		}
		emit(p, "INSTANCE OF ")
		emit_node(p, n.type_ref)
	case ^Between_Expr:
		emit_node(p, n.subject)
		emit(p, " BETWEEN ")
		emit_node(p, n.low)
		emit(p, " AND ")
		emit_node(p, n.high)
	case ^Sql_Case_When_Expr:
		emit(p, "WHEN ")
		emit_node(p, n.condition)
		emit(p, " THEN ")
		emit_node(p, n.result)
	case ^Sql_Case_Expr:
		emit(p, "CASE")
		if n.operand != nil {
			emit_space(p)
			emit_node(p, n.operand)
		}
		for item in n.whens {
			emit_space(p)
			emit_node(p, item)
		}
		if n.else_expr != nil {
			emit(p, " ELSE ")
			emit_node(p, n.else_expr)
		}
		emit(p, " END")
	case ^Let_Expr:
		emit(p, "LET ")
		emit_expr_list(p, n.bindings, " ")
		emit(p, " IN ")
		emit_expr_list(p, n.body, " ")
	case ^Constructor_Let_Binding_Expr:
		emit(p, n.name)
		emit(p, " = ")
		emit_node(p, n.value)
	case ^Constructor_When_Clause_Expr:
		emit(p, "WHEN ")
		emit_node(p, n.condition)
		emit(p, " THEN ")
		emit_node(p, n.result)
	case ^Constructor_Else_Clause_Expr:
		emit(p, "ELSE ")
		emit_node(p, n.result)
	case ^Constructor_For_Clause_Expr:
		emit_constructor_for_clause(p, n)
	case ^Constructor_Where_Clause_Expr:
		emit(p, "WHERE ( ")
		emit_node(p, n.condition)
		emit(p, " )")
	case ^Constructor_Init_Clause_Expr:
		emit(p, "INIT ")
		emit_expr_list(p, n.assignments, " ")
	case ^Constructor_Next_Clause_Expr:
		emit(p, "NEXT ")
		emit_expr_list(p, n.assignments, " ")
	case ^Constructor_Named_Assignment_Expr:
		emit(p, n.name)
		emit(p, " = ")
		emit_node(p, n.value)
	case ^Constructor_Base_Clause_Expr:
		emit(p, "BASE ")
		emit_node(p, n.value)
	case ^Constructor_Lines_Of_Clause_Expr:
		emit_constructor_lines_of_clause(p, n)
	case ^Constructor_Optional_Expr:
		emit_node(p, n.value)
		emit(p, " OPTIONAL")
	case ^Constructor_Corresponding_Mapping_Clause_Expr:
		emit(p, "MAPPING ")
		emit_expr_list(p, n.assignments, " ")
	case ^Constructor_Corresponding_Mapping_Assignment_Expr:
		emit_constructor_mapping_assignment(p, n)
	case ^Constructor_Corresponding_Except_Clause_Expr:
		emit(p, "EXCEPT ")
		emit_expr_list(p, n.names, " ")
	case ^Data_Inline_Name_Expr:
		emit(p, "DATA(")
		emit(p, n.name)
		emit(p, ")")
	case ^Field_Symbol_Inline_Name_Expr:
		emit(p, "FIELD-SYMBOL(")
		emit(p, n.name)
		emit(p, ")")
	case ^Data_Chained_Decl:
		emit_data_chained_decl(p, n)
	case ^Data_Inline_Decl:
		emit(p, "DATA(")
		emit(p, n.name)
		emit(p, ") = ")
		emit_node(p, n.expr)
		emit(p, ".")
	case ^Types_Decl:
		emit_types_decl(p, n)
	case ^Constants_Decl:
		emit_constants_decl(p, n)
	case ^Field_Symbols_Decl:
		emit_field_symbols_decl(p, n)
	case ^Statics_Decl:
		emit_statics_decl(p, n)
	case ^Tables_Decl:
		emit_tables_decl(p, n)
	case ^Ranges_Decl:
		emit_ranges_decl(p, n)
	case ^Parameters_Decl:
		emit_parameters_decl(p, n)
	case ^Select_Options_Decl:
		emit_select_options_decl(p, n)
	case ^Controls_Decl:
		emit_controls_decl(p, n)
	case ^Class_Data_Decl:
		emit_class_data_decl(p, n)
	case ^Type_Pools_Decl:
		emit_type_pools_decl(p, n)
	case ^Function_Pool_Decl:
		emit_function_pool_decl(p, n)
	case ^Include_Stmt:
		emit_include_stmt(p, n)
	case ^Assign_Stmt:
		emit_node(p, n.lhs)
		emit(p, " = ")
		emit_node(p, n.rhs)
		emit(p, ".")
	case ^Downcast_Assign_Stmt:
		emit_node(p, n.lhs)
		emit(p, " ?= ")
		emit_node(p, n.rhs)
		emit(p, ".")
	case ^Expr_Stmt:
		emit_node(p, n.expr)
		emit(p, ".")
	case ^Clear_Stmt:
		emit_clear_stmt(p, n)
	case ^Refresh_Stmt:
		emit_refresh_stmt(p, n)
	case ^Free_Stmt:
		emit_free_stmt(p, n)
	case ^Unassign_Stmt:
		emit_unassign_stmt(p, n)
	case ^Move_Stmt:
		emit_move_stmt(p, n)
	case ^Move_Corresponding_Stmt:
		emit_move_corresponding_stmt(p, n)
	case ^Add_Stmt:
		emit_add_stmt(p, n)
	case ^Subtract_Stmt:
		emit_subtract_stmt(p, n)
	case ^Multiply_Stmt:
		emit_multiply_stmt(p, n)
	case ^Divide_Stmt:
		emit_divide_stmt(p, n)
	case ^Compute_Stmt:
		emit_compute_stmt(p, n)
	case ^Concatenate_Stmt:
		emit_concatenate_stmt(p, n)
	case ^Split_Stmt:
		emit_split_stmt(p, n)
	case ^Condense_Stmt:
		emit(p, "CONDENSE ")
		emit_node(p, n.target)
		if n.no_gaps {
			emit(p, " NO-GAPS")
		}
		emit(p, ".")
	case ^Replace_Stmt:
		emit_replace_stmt(p, n)
	case ^Translate_Stmt:
		emit_translate_stmt(p, n)
	case ^Shift_Stmt:
		emit_shift_stmt(p, n)
	case ^Find_Stmt:
		emit_find_stmt(p, n)
	case ^Search_Stmt:
		emit_search_stmt(p, n)
	case ^Perform_Stmt:
		emit_perform_stmt(p, n)
	case ^Call_Stmt:
		emit_call_stmt(p, n)
	case ^Submit_Stmt:
		emit_submit_stmt(p, n)
	case ^Message_Stmt:
		emit_message_stmt(p, n)
	case ^Write_Stmt:
		emit_write_stmt(p, n)
	case ^Write_To_Stmt:
		emit_write_to_stmt(p, n)
	case ^Assert_Stmt:
		emit(p, "ASSERT ")
		emit_node(p, n.condition)
		emit(p, ".")
	case ^Check_Stmt:
		emit(p, "CHECK ")
		emit_node(p, n.condition)
		emit(p, ".")
	case ^Flow_Stmt:
		emit(p, flow_kind_text(n.kind))
		emit(p, ".")
	case ^Transaction_Stmt:
		emit(p, transaction_kind_text(n.kind))
		emit(p, " WORK")
		if n.wait {
			emit(p, " AND WAIT")
		}
		emit(p, ".")
	case ^Describe_Stmt:
		emit_describe_stmt(p, n)
	case ^Runtime_Stmt:
		emit_runtime_stmt(p, n)
	case ^Set_Handler_Stmt:
		emit_set_handler_stmt(p, n)
	case ^Import_Stmt:
		emit_import_stmt(p, n)
	case ^Export_Stmt:
		emit_export_stmt(p, n)
	case ^Bit_Stmt:
		emit_bit_stmt(p, n)
	case ^Locale_Stmt:
		emit_locale_stmt(p, n)
	case ^Set_Cursor_Stmt:
		emit_set_cursor_stmt(p, n)
	case ^Receive_Results_Stmt:
		emit_receive_results_stmt(p, n)
	case ^Raise_Stmt:
		emit_raise_stmt(p, n)
	case ^Authority_Check_Stmt:
		emit_authority_check_stmt(p, n)
	case ^Field_Groups_Stmt:
		emit(p, "FIELD-GROUPS")
		if len(n.groups) > 0 {
			emit_space(p)
			emit_expr_list(p, n.groups, " ")
		}
		emit(p, ".")
	case ^Insert_Dummy_Stmt:
		emit(p, "INSERT DUMMY")
		if n.target != nil {
			emit(p, " INTO ")
			emit_node(p, n.target)
		}
		emit(p, ".")
	case ^Field_Stmt:
		emit(p, "FIELD")
		if len(n.operands) > 0 {
			emit_space(p)
			emit_expr_list(p, n.operands, " ")
		}
		emit(p, ".")
	case ^Assign_Field_Stmt:
		emit(p, "ASSIGN")
		if n.component != nil {
			emit(p, " COMPONENT ")
			emit_node(p, n.component)
			if n.structure != nil {
				emit(p, " OF STRUCTURE ")
				emit_node(p, n.structure)
			}
		} else if n.source != nil {
			emit_space(p)
			emit_node(p, n.source)
		}
		if n.target != nil {
			emit(p, " TO ")
			emit_node(p, n.target)
		}
		if n.casting {
			emit(p, " CASTING")
			if n.casting_type != nil {
				emit(p, " TYPE ")
				emit_node(p, n.casting_type)
			}
			if n.casting_decimals != nil {
				emit(p, " DECIMALS ")
				emit_node(p, n.casting_decimals)
			}
		}
		emit(p, ".")
	case ^Create_Object_Stmt:
		emit(p, "CREATE OBJECT")
		if n.target != nil {
			emit_space(p)
			emit_node(p, n.target)
		}
		if n.type_ref != nil {
			emit(p, " TYPE ")
			emit_node(p, n.type_ref)
		}
		if len(n.operands) > 0 {
			emit_space(p)
			emit_expr_list(p, n.operands, " ")
		}
		emit(p, ".")
	case ^Create_Data_Stmt:
		emit(p, "CREATE DATA")
		if n.target != nil {
			emit_space(p)
			emit_node(p, n.target)
		}
		if n.type_ref != nil {
			emit(p, " TYPE ")
			emit_node(p, n.type_ref)
		}
		if n.type_handle != nil {
			emit(p, " TYPE HANDLE ")
			emit_node(p, n.type_handle)
		}
		if len(n.operands) > 0 {
			emit_space(p)
			emit_expr_list(p, n.operands, " ")
		}
		emit(p, ".")
	case ^Text_Transform_Stmt:
		emit(p, text_transform_kind_text(n.kind))
		if len(n.operands) > 0 {
			emit_space(p)
			emit_expr_list(p, n.operands, " ")
		}
		emit(p, ".")
	case ^Wait_Stmt:
		emit(p, "WAIT")
		if n.condition != nil {
			emit(p, " UNTIL ")
			emit_node(p, n.condition)
		}
		if n.duration != nil {
			emit(p, " UP TO ")
			emit_node(p, n.duration)
			emit(p, " SECONDS")
		}
		emit(p, ".")
	case ^Convert_Time_Stamp_Stmt:
		emit_convert_time_stamp_stmt(p, n)
	case ^List_Control_Stmt:
		emit(p, list_control_kind_text(n.kind))
		if len(n.operands) > 0 {
			emit_space(p)
			emit_expr_list(p, n.operands, " ")
		}
		emit(p, ".")
	case ^Line_Stmt:
		emit_line_stmt(p, n)
	case ^Macro_Def_Stmt:
		emit_macro_def_stmt(p, n)
	case ^Macro_Call_Stmt:
		emit(p, n.name)
		if len(n.args) > 0 {
			emit_space(p)
			emit_expr_list(p, n.args, " ")
		}
		emit(p, ".")
	case ^Selection_Screen_Stmt:
		if n.text != "" {
			emit(p, n.text)
		} else {
			emit(p, "SELECTION-SCREEN.")
		}
	case ^Oop_Simple_Stmt:
		emit_oop_simple_stmt(p, n)
	case ^Oop_Load_Stmt:
		emit_oop_load_stmt(p, n)
	case ^If_Stmt:
		emit_if_stmt(p, n)
	case ^Case_Stmt:
		emit_case_stmt(p, n)
	case ^While_Stmt:
		emit(p, "WHILE ")
		emit_node(p, n.condition)
		emit_block(p, n.body, "ENDWHILE")
	case ^Do_Stmt:
		emit(p, "DO")
		if n.count != nil {
			emit_space(p)
			emit_node(p, n.count)
			emit(p, " TIMES")
		}
		emit_block(p, n.body, "ENDDO")
	case ^Loop_Stmt:
		if n.header_text != "" {
			emit(p, n.header_text)
		} else {
			emit(p, "LOOP AT ")
			emit_node(p, n.source)
			switch n.target_kind {
			case .Into:
				emit(p, " INTO ")
				emit_node(p, n.target)
			case .Assigning:
				emit(p, " ASSIGNING ")
				emit_node(p, n.target)
			case .Reference_Into:
				emit(p, " REFERENCE INTO ")
				emit_node(p, n.target)
			case .None:
			}
			if n.from != nil {
				emit(p, " FROM ")
				emit_node(p, n.from)
			}
			if n.to != nil {
				emit(p, " TO ")
				emit_node(p, n.to)
			}
			if n.using_key.name.text != "" || n.using_key.dynamic_name != nil {
				emit(p, " USING KEY ")
				emit_table_key_selector(p, n.using_key)
			}
			if n.transporting_no_fields {
				emit(p, " TRANSPORTING NO FIELDS")
			}
			if n.where_cond != nil {
				emit(p, " WHERE ")
				emit_node(p, n.where_cond)
			}
			if n.group_by != nil {
				emit(p, " GROUP BY ")
				emit_node(p, n.group_by)
				switch n.group_order {
				case .Ascending:
					emit(p, " ASCENDING")
				case .Descending:
					emit(p, " DESCENDING")
				case .None:
				}
				if n.group_without_members {
					emit(p, " WITHOUT MEMBERS")
				}
				switch n.group_target_kind {
				case .Into:
					emit(p, " INTO ")
					emit_node(p, n.group_target)
				case .Assigning:
					emit(p, " ASSIGNING ")
					emit_node(p, n.group_target)
				case .Reference_Into:
					emit(p, " REFERENCE INTO ")
					emit_node(p, n.group_target)
				case .None:
				}
			}
		}
		emit_block(p, n.body, "ENDLOOP")
	case ^At_Stmt:
		emit(p, "AT ")
		switch n.kind {
		case .First:
			emit(p, "FIRST")
		case .Last:
			emit(p, "LAST")
		case .New:
			emit(p, "NEW")
		case .End_Of:
			emit(p, "END OF")
		}
		if n.field_name.text != "" {
			emit_space(p)
			emit(p, n.field_name)
		} else if n.expr != nil {
			emit_space(p)
			emit_node(p, n.expr)
		}
		emit_block(p, n.body, "ENDAT")
	case ^Try_Stmt:
		emit_try_stmt(p, n)
	case ^Class_Decl:
		emit_named_block(p, "CLASS", n.name.text, n.header_text, n.body, "ENDCLASS")
	case ^Interface_Decl:
		emit_named_block(p, "INTERFACE", n.name.text, n.header_text, n.body, "ENDINTERFACE")
	case ^Method_Decl:
		if n.is_amdp {
			emit_amdp_method(p, n)
		} else {
			emit_named_block(p, "METHOD", n.name.text, n.header_text, n.body, "ENDMETHOD")
		}
	case ^Form_Decl:
		emit_named_block(p, "FORM", n.name, n.header_text, n.body, "ENDFORM")
	case ^Function_Decl:
		emit_named_block(p, "FUNCTION", n.name, n.header_text, n.body, "ENDFUNCTION")
	case ^Module_Decl:
		emit_named_block(p, "MODULE", n.name, n.header_text, n.body, "ENDMODULE")
	case ^Event_Block_Stmt:
		emit_named_block(p, n.kind, "", n.header_text, n.body, "")
	case ^Enhancement_Stmt:
		emit_named_block(p, "ENHANCEMENT", n.name, n.header_text, n.body, "ENDENHANCEMENT")
	case ^Enhancement_Section_Stmt:
		emit_named_block(p, "ENHANCEMENT-SECTION", n.name, n.header_text, n.body, "END-ENHANCEMENT-SECTION")
	case ^Test_Seam_Stmt:
		emit_named_block(p, "TEST-SEAM", n.name, n.header_text, n.body, "END-TEST-SEAM")
	case ^Test_Injection_Stmt:
		emit_named_block(p, "TEST-INJECTION", n.name, n.header_text, n.body, "END-TEST-INJECTION")
	case ^Select_Stmt:
		emit_select_stmt(p, n)
	case ^Open_Cursor_Stmt:
		emit_open_cursor_stmt(p, n)
	case ^Fetch_Stmt:
		emit_fetch_stmt(p, n)
	case ^Close_Cursor_Stmt:
		emit(p, "CLOSE CURSOR ")
		emit_node(p, n.handle)
		emit(p, ".")
	case ^Insert_Stmt:
		emit_insert_stmt(p, n)
	case ^Append_Stmt:
		emit_append_stmt(p, n)
	case ^Modify_Stmt:
		emit_modify_stmt(p, n)
	case ^Sort_Stmt:
		emit_sort_stmt(p, n)
	case ^Update_Stmt:
		emit_update_stmt(p, n)
	case ^Delete_Stmt:
		emit_delete_stmt(p, n)
	case ^Read_Table_Stmt:
		emit_read_table_stmt(p, n)
	case ^Dataset_Stmt:
		emit_dataset_stmt(p, n)
	case ^Report_Stmt:
		emit_report_stmt(p, n)
	case ^Textpool_Stmt:
		emit_textpool_stmt(p, n)
	case ^Exec_Sql_Stmt:
		emit_exec_sql_stmt(p, n)
	case ^Generate_Stmt:
		emit_generate_stmt(p, n)
	case ^Invalid_Stmt:
		emit(p, "?")
	}
	emit_trailing_trivia(p, node)
}

emit_file :: proc(p: ^Printer, file: ^File) {
	detached_index := 0
	wrote_any := false
	for stmt in file.stmts {
		for detached_index < len(file.detached_trivia) &&
		    file.detached_trivia[detached_index].trivia.range.start < stmt.range.start {
			if wrote_any {
				emit_newline(p)
			}
			emit(p, file.detached_trivia[detached_index].trivia.text)
			wrote_any = true
			detached_index += 1
		}
		if wrote_any {
			emit_newline(p)
		}
		emit_node(p, stmt)
		wrote_any = true
	}
	for detached_index < len(file.detached_trivia) {
		if wrote_any {
			emit_newline(p)
		}
		emit(p, file.detached_trivia[detached_index].trivia.text)
		wrote_any = true
		detached_index += 1
	}
}

emit_trailing_trivia :: proc(p: ^Printer, node: ^Node) {
	if len(node.trailing_trivia) == 0 {
		return
	}
	period_removed := false
	for trivia in node.trailing_trivia {
		if trivia_is_in_printed_source_fragment(node, trivia) {
			continue
		}
		if trivia.kind != .Pragma || !trivia_prints_before_final_period(node, trivia) {
			continue
		}
		if !period_removed {
			period_removed = remove_final_period(p)
		}
		emit_space(p)
		emit(p, trivia.text)
	}
	if period_removed {
		emit(p, ".")
	}
	for trivia in node.trailing_trivia {
		if trivia_is_in_printed_source_fragment(node, trivia) {
			continue
		}
		if trivia.kind == .Pragma && trivia_prints_before_final_period(node, trivia) {
			continue
		}
		if trivia.kind == .Pragma && trivia.range.end <= node.range.end {
			continue
		}
		emit_space(p)
		emit(p, trivia.text)
	}
}

trivia_prints_before_final_period :: proc(node: ^Node, trivia: Ast_Trivia) -> bool {
	return trivia.range.end <= node.range.end && node.range.end - trivia.range.end <= 1
}

trivia_is_in_printed_source_fragment :: proc(node: ^Node, trivia: Ast_Trivia) -> bool {
	#partial switch n in node.derived {
	case ^Selection_Screen_Stmt:
		return n.text != "" && range_contains(n.range, trivia.range)
	case ^Oop_Simple_Stmt:
		return n.text != "" && range_contains(n.range, trivia.range)
	case ^Oop_Load_Stmt:
		return n.text != "" && range_contains(n.range, trivia.range)
	case ^Loop_Stmt:
		return range_contains(n.header_range, trivia.range)
	case ^Class_Decl:
		return range_contains(n.header_range, trivia.range)
	case ^Interface_Decl:
		return range_contains(n.header_range, trivia.range)
	case ^Method_Decl:
		return range_contains(n.header_range, trivia.range)
	case ^Form_Decl:
		return range_contains(n.header_range, trivia.range)
	case ^Function_Decl:
		return range_contains(n.header_range, trivia.range)
	case ^Module_Decl:
		return range_contains(n.header_range, trivia.range)
	case ^Event_Block_Stmt:
		return range_contains(n.header_range, trivia.range)
	case ^Enhancement_Stmt:
		return range_contains(n.header_range, trivia.range)
	case ^Enhancement_Section_Stmt:
		return range_contains(n.header_range, trivia.range)
	case ^Test_Seam_Stmt:
		return range_contains(n.header_range, trivia.range)
	case ^Test_Injection_Stmt:
		return range_contains(n.header_range, trivia.range)
	}
	return false
}

range_contains :: proc(outer, inner: tokenizer.Range) -> bool {
	return outer.end > outer.start && outer.start <= inner.start && inner.end <= outer.end
}

remove_final_period :: proc(p: ^Printer) -> bool {
	if len(p.out.buf) == 0 || p.out.buf[len(p.out.buf) - 1] != '.' {
		return false
	}
	pop(&p.out.buf)
	return true
}

emit_expr_list :: proc(p: ^Printer, list: [dynamic]^Expr, separator: string) {
	for expr, i in list {
		if i > 0 {
			emit(p, separator)
		}
		emit_node(p, expr)
	}
}

emit_stmt_list :: proc(p: ^Printer, list: [dynamic]^Stmt) {
	for stmt, i in list {
		if i > 0 {
			emit_newline(p)
		}
		emit_node(p, stmt)
	}
}

emit_block :: proc(p: ^Printer, body: [dynamic]^Stmt, end_keyword: string) {
	emit(p, ".")
	p.indent_level += 1
	if len(body) > 0 {
		emit_newline(p)
		emit_stmt_list(p, body)
	}
	p.indent_level -= 1
	emit_newline(p)
	emit(p, end_keyword)
	emit(p, ".")
}

emit_template :: proc(p: ^Printer, expr: ^Char_String_Template_Expr) {
	emit(p, "|")
	for part in expr.parts {
		emit_node(p, part)
	}
	emit(p, "|")
}

emit_template_interpolation :: proc(p: ^Printer, expr: ^Template_Interpolation_Expr) {
	emit(p, "{ ")
	emit_node(p, expr.expr)
	for spec in expr.format_specs {
		emit_space(p)
		emit_node(p, spec)
	}
	emit(p, " }")
}

emit_type_ref_expr :: proc(p: ^Printer, expr: ^Type_Ref_Expr) {
	if expr.name.text == "" {
		emit(p, expr.source)
		return
	}
	emit(p, expr.name)
	if len(expr.keys) > 0 {
		for key in expr.keys {
			emit_type_ref_key_clause(p, key)
		}
	} else if expr.key != nil {
		emit_type_ref_key_clause(p, expr.key)
	}
}

emit_dynamic_call_method_target_expr :: proc(p: ^Printer, expr: ^Dynamic_Call_Method_Target_Expr) {
	if expr.base != nil {
		if expr.base_dynamic {emit(p, "(")}
		emit_node(p, expr.base)
		if expr.base_dynamic {emit(p, ")")}
		emit(p, selector_op_text(expr.selector))
	}
	if expr.method != nil {
		if expr.method_dynamic {emit(p, "(")}
		emit_node(p, expr.method)
		if expr.method_dynamic {emit(p, ")")}
	}
}

emit_ole_call_method_target_expr :: proc(p: ^Printer, expr: ^Ole_Call_Method_Target_Expr) {
	emit(p, "OF ")
	emit_node(p, expr.object)
	emit_space(p)
	emit_node(p, expr.member)
	if expr.result != nil {
		emit(p, " = ")
		emit_node(p, expr.result)
	}
}

emit_type_ref_key_clause :: proc(p: ^Printer, key: ^Type_Ref_Key_Clause) {
	emit(p, " WITH ")
	switch key.kind {
	case .Default:
		emit(p, "DEFAULT KEY")
		return
	case .Empty:
		emit(p, "EMPTY KEY")
		return
	case .Unique:
		emit(p, "UNIQUE ")
	case .Non_Unique:
		emit(p, "NON-UNIQUE ")
	case .Generic:
	}
	if key.default_key {
		emit(p, "DEFAULT KEY")
		return
	}
	if key.sorted {
		emit(p, "SORTED ")
	} else if key.hashed {
		emit(p, "HASHED ")
	}
	emit(p, "KEY")
	if key.name.text != "" {
		emit_space(p)
		emit(p, key.name)
	}
	if len(key.components) > 0 {
		emit(p, " COMPONENTS " if key.name.text != "" else " ")
		for component, i in key.components {
			if i > 0 {
				emit_space(p)
			}
			emit(p, component)
		}
	}
}

emit_constructor_for_clause :: proc(p: ^Printer, expr: ^Constructor_For_Clause_Expr) {
	emit(p, "FOR ")
	emit(p, expr.variable)
	if expr.group_source.text != "" {
		emit(p, " IN GROUP ")
		emit(p, expr.group_source)
	} else if expr.source != nil {
		emit(p, " IN ")
		emit_node(p, expr.source)
		if expr.where_clause != nil {
			emit_space(p)
			emit_node(p, expr.where_clause)
		}
	} else {
		emit(p, " = ")
		emit_node(p, expr.init)
		if expr.then_expr != nil {
			emit(p, " THEN ")
			emit_node(p, expr.then_expr)
		}
		emit(p, " ")
		emit(p, "WHILE" if expr.kind == .For_Then_While else "UNTIL")
		emit_space(p)
		emit_node(p, expr.condition)
	}
	if len(expr.body) > 0 {
		emit_space(p)
		emit_expr_list(p, expr.body, " ")
	}
}

emit_constructor_lines_of_clause :: proc(p: ^Printer, expr: ^Constructor_Lines_Of_Clause_Expr) {
	emit(p, "LINES OF ")
	emit_node(p, expr.source)
	if expr.from != nil {
		emit(p, " FROM ")
		emit_node(p, expr.from)
	}
	if expr.to != nil {
		emit(p, " TO ")
		emit_node(p, expr.to)
	}
	if expr.using_key.text != "" {
		emit(p, " USING KEY ")
		emit(p, expr.using_key)
	}
}

emit_constructor_mapping_assignment :: proc(
	p: ^Printer,
	expr: ^Constructor_Corresponding_Mapping_Assignment_Expr,
) {
	emit(p, expr.target)
	emit(p, " = ")
	if expr.source != nil {
		emit_node(p, expr.source)
	} else if expr.default_value != nil {
		emit(p, "DEFAULT ")
		emit_node(p, expr.default_value)
	}
	if expr.discarding_duplicates {
		emit(p, " DISCARDING DUPLICATES")
	}
	if expr.source != nil && expr.default_value != nil {
		emit(p, " DEFAULT ")
		emit_node(p, expr.default_value)
	}
	if expr.mapping != nil {
		emit_space(p)
		emit_node(p, expr.mapping)
	}
	if expr.except != nil {
		emit_space(p)
		emit_node(p, expr.except)
	}
}

emit_data_chained_decl :: proc(p: ^Printer, decl: ^Data_Chained_Decl) {
	emit(p, "DATA")
	emit(p, ": " if decl.has_colon || len(decl.decls) > 1 else " ")
	for branch, i in decl.decls {
		if i > 0 {
			emit(p, ", ")
		}
		emit_decl_prefix(p, branch.kind, branch.name, branch.include_ref, .Common_Part_Delimiter in branch.flags)
		emit_paren_length(p, branch.paren_length)
		emit_occurs(p, branch.occurs)
		emit_include_additions(p, branch.as_name, branch.renaming_suffix)
		emit_length_clauses(p, branch.length_clauses)
		emit_type_clause(p, branch.type_clause)
		emit_header_line(p, branch.flags)
		emit_value_clause(p, branch.value_clause)
		if .Read_Only in branch.flags {
			emit(p, " READ-ONLY")
		}
	}
	emit(p, ".")
}

emit_types_decl :: proc(p: ^Printer, decl: ^Types_Decl) {
	if len(decl.types) == 1 &&
	   (decl.types[0].kind == .Include_Type || decl.types[0].kind == .Include_Structure) {
		clause := decl.types[0]
		emit_decl_prefix(p, clause.kind, clause.name, clause.include_ref, .Common_Part_Delimiter in clause.flags)
		emit_occurs(p, clause.occurs)
		emit_include_additions(p, clause.as_name, clause.renaming_suffix)
		emit(p, ".")
		return
	}
	emit(p, "TYPES")
	emit(p, ": " if len(decl.types) > 1 else " ")
	for clause, i in decl.types {
		if i > 0 {
			emit(p, ", ")
		}
		emit_decl_prefix(p, clause.kind, clause.name, clause.include_ref, .Common_Part_Delimiter in clause.flags)
		emit_paren_length(p, clause.paren_length)
		emit_occurs(p, clause.occurs)
		emit_include_additions(p, clause.as_name, clause.renaming_suffix)
		emit_length_clauses(p, clause.length_clauses)
		emit_type_clause(p, clause.type_clause)
		emit_header_line(p, clause.flags)
	}
	emit(p, ".")
}

emit_constants_decl :: proc(p: ^Printer, decl: ^Constants_Decl) {
	emit(p, "CONSTANTS")
	emit(p, ": " if len(decl.constants) > 1 else " ")
	for clause, i in decl.constants {
		if i > 0 {
			emit(p, ", ")
		}
		emit_decl_prefix(p, clause.kind, clause.name, clause.include_ref, .Common_Part_Delimiter in clause.flags)
		emit_paren_length(p, clause.paren_length)
		emit_occurs(p, clause.occurs)
		emit_include_additions(p, clause.as_name, clause.renaming_suffix)
		emit_length_clauses(p, clause.length_clauses)
		emit_type_clause(p, clause.type_clause)
		emit_header_line(p, clause.flags)
		emit_value_clause(p, clause.value_clause)
	}
	emit(p, ".")
}

emit_field_symbols_decl :: proc(p: ^Printer, decl: ^Field_Symbols_Decl) {
	emit(p, "FIELD-SYMBOLS")
	emit(p, ": " if len(decl.field_symbols) > 1 else " ")
	for clause, i in decl.field_symbols {
		if i > 0 {
			emit(p, ", ")
		}
		emit(p, clause.name)
		emit_type_clause(p, clause.type_clause)
	}
	emit(p, ".")
}

emit_statics_decl :: proc(p: ^Printer, decl: ^Statics_Decl) {
	emit(p, "STATICS")
	emit(p, ": " if len(decl.statics) > 1 else " ")
	for clause, i in decl.statics {
		if i > 0 {
			emit(p, ", ")
		}
		emit_decl_prefix(p, clause.kind, clause.name, clause.include_ref, .Common_Part_Delimiter in clause.flags)
		emit_paren_length(p, clause.paren_length)
		emit_occurs(p, clause.occurs)
		emit_include_additions(p, clause.as_name, clause.renaming_suffix)
		emit_length_clauses(p, clause.length_clauses)
		emit_type_clause(p, clause.type_clause)
		emit_header_line(p, clause.flags)
		emit_value_clause(p, clause.value_clause)
	}
	emit(p, ".")
}

emit_tables_decl :: proc(p: ^Printer, decl: ^Tables_Decl) {
	emit(p, "TABLES")
	emit(p, ": " if len(decl.tables) > 1 else " ")
	for clause, i in decl.tables {
		if i > 0 {
			emit(p, ", ")
		}
		emit(p, clause.name)
	}
	emit(p, ".")
}

emit_ranges_decl :: proc(p: ^Printer, decl: ^Ranges_Decl) {
	emit(p, "RANGES")
	emit(p, ": " if len(decl.ranges) > 1 else " ")
	for clause, i in decl.ranges {
		if i > 0 {
			emit(p, ", ")
		}
		emit(p, clause.name)
		if clause.for_expr != nil {
			emit(p, " FOR ")
			emit_node(p, clause.for_expr)
		}
	}
	emit(p, ".")
}

emit_parameters_decl :: proc(p: ^Printer, decl: ^Parameters_Decl) {
	if decl.keyword.text != "" {
		emit(p, decl.keyword)
	} else {
		emit(p, "PARAMETERS")
	}
	emit(p, ": " if decl.has_colon || len(decl.parameters) > 1 else " ")
	for clause, i in decl.parameters {
		if i > 0 {
			emit(p, ", ")
		}
		emit(p, clause.name)
		emit_paren_length(p, clause.paren_length)
		emit_parameter_clause_tail(p, clause)
	}
	emit(p, ".")
}

emit_select_options_decl :: proc(p: ^Printer, decl: ^Select_Options_Decl) {
	emit(p, "SELECT-OPTIONS")
	emit(p, ": " if len(decl.options) > 1 else " ")
	for clause, i in decl.options {
		if i > 0 {
			emit(p, ", ")
		}
		emit(p, clause.name)
		if clause.for_expr != nil {
			emit(p, " FOR ")
			emit_node(p, clause.for_expr)
		}
		emit_default_expr(p, clause.default_expr)
		if clause.to_expr != nil {
			emit(p, " TO ")
			emit_node(p, clause.to_expr)
		}
		if option, ok := clause.option.?; ok {
			emit(p, " OPTION ")
			emit(p, option)
		}
		if sign, ok := clause.sign.?; ok {
			emit(p, " SIGN ")
			emit(p, sign)
		}
		emit_select_option_additions(p, clause)
	}
	emit(p, ".")
}

emit_controls_decl :: proc(p: ^Printer, decl: ^Controls_Decl) {
	emit(p, "CONTROLS")
	emit(p, ": " if len(decl.controls) > 1 else " ")
	for clause, i in decl.controls {
		if i > 0 {
			emit(p, ", ")
		}
		emit(p, clause.name)
		emit_type_clause(p, clause.type_clause)
		if clause.using_screen != nil {
			emit(p, " USING SCREEN ")
			emit_node(p, clause.using_screen.screen)
		}
	}
	emit(p, ".")
}

emit_class_data_decl :: proc(p: ^Printer, decl: ^Class_Data_Decl) {
	emit(p, "CLASS-DATA")
	emit(p, ": " if len(decl.decls) > 1 else " ")
	for clause, i in decl.decls {
		if i > 0 {
			emit(p, ", ")
		}
		emit_decl_prefix(p, clause.kind, clause.name, clause.include_ref, .Common_Part_Delimiter in clause.flags)
		emit_paren_length(p, clause.paren_length)
		emit_occurs(p, clause.occurs)
		emit_include_additions(p, clause.as_name, clause.renaming_suffix)
		emit_length_clauses(p, clause.length_clauses)
		emit_type_clause(p, clause.type_clause)
		emit_header_line(p, clause.flags)
		emit_value_clause(p, clause.value_clause)
		if .Read_Only in clause.flags {
			emit(p, " READ-ONLY")
		}
	}
	emit(p, ".")
}

emit_type_pools_decl :: proc(p: ^Printer, decl: ^Type_Pools_Decl) {
	emit(p, "TYPE-POOLS")
	emit(p, ": " if len(decl.pools) > 1 else " ")
	for pool, i in decl.pools {
		if i > 0 {
			emit(p, ", ")
		}
		emit(p, pool)
	}
	emit(p, ".")
}

emit_function_pool_decl :: proc(p: ^Printer, decl: ^Function_Pool_Decl) {
	emit(p, "FUNCTION-POOL ")
	emit(p, decl.name)
	if decl.message_id.text != "" {
		emit(p, " MESSAGE-ID ")
		emit(p, decl.message_id)
	}
	emit(p, ".")
}

emit_include_stmt :: proc(p: ^Printer, stmt: ^Include_Stmt) {
	emit(p, "INCLUDE")
	emit(p, ": " if len(stmt.names) > 1 else " ")
	for name, i in stmt.names {
		if i > 0 {
			emit(p, ", ")
		}
		emit(p, name.name)
	}
	if stmt.if_found {
		emit(p, " IF FOUND")
	}
	emit(p, ".")
}

emit_decl_prefix :: proc(
	p: ^Printer,
	kind: Decl_Clause_Kind,
	name: Token_Text,
	include_ref: ^Expr,
	is_common_part_delimiter := false,
) {
	switch kind {
	case .Begin_Group:
		if is_common_part_delimiter {
			emit(p, "BEGIN OF COMMON PART")
			if name.text != "" {
				emit(p, " ")
				emit(p, name)
			}
			return
		}
		emit(p, "BEGIN OF ")
		emit(p, name)
	case .End_Group:
		if is_common_part_delimiter {
			emit(p, "END OF COMMON PART")
			if name.text != "" {
				emit(p, " ")
				emit(p, name)
			}
			return
		}
		emit(p, "END OF ")
		emit(p, name)
	case .Include_Type:
		emit(p, "INCLUDE TYPE ")
		emit_node(p, include_ref)
	case .Include_Structure:
		emit(p, "INCLUDE STRUCTURE ")
		emit_node(p, include_ref)
	case .Normal:
		emit(p, name)
	}
}

emit_include_additions :: proc(p: ^Printer, as_name, suffix: Token_Text) {
	if as_name.text != "" {
		emit(p, " AS ")
		emit(p, as_name)
	}
	if suffix.text != "" {
		emit(p, " RENAMING WITH SUFFIX ")
		emit(p, suffix)
	}
}

emit_occurs :: proc(p: ^Printer, occurs: ^Expr) {
	if occurs != nil {
		emit(p, " OCCURS ")
		emit_node(p, occurs)
	}
}

emit_header_line :: proc(p: ^Printer, flags: Decl_Clause_Flags) {
	if .With_Header_Line in flags {
		emit(p, " WITH HEADER LINE")
	}
}

emit_type_clause :: proc(p: ^Printer, clause: ^Data_Type_Clause) {
	if clause == nil {
		return
	}
	emit_space(p)
	switch clause.form {
	case .Type:
		emit(p, "TYPE")
	case .Like:
		emit(p, "LIKE")
	case .Structure:
		emit(p, "STRUCTURE")
	case .Ref_To:
		emit(p, "TYPE REF TO")
	case .Like_Line_Of:
		emit(p, "LIKE LINE OF")
	case .Type_Line_Of:
		emit(p, "TYPE LINE OF")
	case .Any_Table:
		emit(p, "TYPE ANY TABLE")
	case .Table:
		emit(p, "TYPE TABLE")
	case .Like_Table:
		emit(p, "LIKE TABLE")
	case .Index_Table:
		emit(p, "TYPE INDEX TABLE")
	case .Standard_Table:
		emit(p, "TYPE STANDARD TABLE")
	case .Sorted_Table:
		emit(p, "TYPE SORTED TABLE")
	case .Hashed_Table:
		emit(p, "TYPE HASHED TABLE")
	case .Like_Standard_Table:
		emit(p, "LIKE STANDARD TABLE")
	case .Like_Sorted_Table:
		emit(p, "LIKE SORTED TABLE")
	case .Like_Hashed_Table:
		emit(p, "LIKE HASHED TABLE")
	case .Range_Of:
		emit(p, "TYPE RANGE OF")
	}
	if clause.type_ref != nil || clause.table_has_of {
		if clause.form == .Any_Table ||
		   clause.form == .Table ||
		   clause.form == .Like_Table ||
		   clause.form == .Index_Table ||
		   clause.form == .Standard_Table ||
		   clause.form == .Sorted_Table ||
		   clause.form == .Hashed_Table ||
		   clause.form == .Like_Standard_Table ||
		   clause.form == .Like_Sorted_Table ||
		   clause.form == .Like_Hashed_Table {
			emit(p, " OF")
		}
	}
	if clause.type_ref != nil {
		emit_space(p)
		emit_node(p, clause.type_ref)
	}
	if clause.initial_size != nil {
		emit(p, " INITIAL SIZE ")
		emit_node(p, clause.initial_size)
	}
}

emit_paren_length :: proc(p: ^Printer, clause: ^Paren_Length_Clause) {
	if clause != nil {
		emit(p, "(")
		emit_node(p, clause.expr)
		emit(p, ")")
	}
}

emit_length_clauses :: proc(p: ^Printer, clauses: [dynamic]Length_Clause) {
	for clause in clauses {
		emit_length_clause(p, clause)
	}
}

emit_length_clause :: proc(p: ^Printer, clause: Length_Clause) {
	emit_space(p)
	emit(p, "DECIMALS" if clause.kind == .Decimals else "LENGTH")
	emit_space(p)
	emit_node(p, clause.expr)
}

emit_value_clause :: proc(p: ^Printer, clause: ^Value_Clause) {
	if clause != nil {
		emit(p, " VALUE ")
		if clause.is_initial {
			emit(p, "IS INITIAL")
		} else {
			emit_node(p, clause.expr)
		}
	}
}

emit_default_expr :: proc(p: ^Printer, expr: ^Expr) {
	if expr != nil {
		emit(p, " DEFAULT ")
		emit_node(p, expr)
	}
}

emit_parameter_clause_tail :: proc(p: ^Printer, clause: Parameters_Clause) {
	if len(clause.parts) == 0 {
		emit_type_clause(p, clause.type_clause)
		emit_length_clauses(p, clause.length_clauses)
		emit_default_expr(p, clause.default_expr)
		emit_parameter_additions(p, clause)
		return
	}

	length_index := 0
	for part in clause.parts {
		#partial switch part {
		case .Type_Clause:
			emit_type_clause(p, clause.type_clause)
		case .Length_Clause:
			if length_index < len(clause.length_clauses) {
				emit_length_clause(p, clause.length_clauses[length_index])
				length_index += 1
			}
		case .Default_Clause:
			emit_default_expr(p, clause.default_expr)
		case .As_Checkbox:
			if .As_Checkbox in clause.flags {
				emit(p, " AS CHECKBOX")
			}
		case .Lower_Case:
			if .Lower_Case in clause.flags {
				emit(p, " LOWER CASE")
			}
		case .Obligatory:
			if .Obligatory in clause.flags {
				emit(p, " OBLIGATORY")
			}
		case .No_Display:
			if .No_Display in clause.flags {
				emit(p, " NO-DISPLAY")
			}
		case .Value_Check:
			if .Value_Check in clause.flags {
				emit(p, " VALUE CHECK")
			}
		case .Help_Request:
			if .Help_Request in clause.flags {
				emit(p, " HELP-REQUEST")
			}
		case .Value_Request:
			if .Value_Request in clause.flags {
				emit(p, " VALUE-REQUEST")
			}
		case .Radiobutton_Group:
			if group, ok := clause.radiobutton_group.?; ok {
				emit(p, " RADIOBUTTON GROUP ")
				emit(p, group)
			}
		case .User_Command:
			if command, ok := clause.user_command.?; ok {
				emit(p, " USER-COMMAND ")
				emit(p, command)
			}
		case .Modif_Id:
			if id, ok := clause.modif_id.?; ok {
				emit(p, " MODIF ID ")
				emit(p, id)
			}
		case .Memory_Id:
			if clause.memory_id != nil {
				emit(p, " MEMORY ID ")
				emit_node(p, clause.memory_id)
			}
		case .Matchcode_Object:
			if clause.matchcode_object != nil {
				emit(p, " MATCHCODE OBJECT ")
				emit_node(p, clause.matchcode_object)
			}
		case .Visible_Length:
			if clause.visible_length != nil {
				emit(p, " VISIBLE LENGTH ")
				emit_node(p, clause.visible_length)
			}
		}
	}
}

emit_parameter_additions :: proc(p: ^Printer, clause: Parameters_Clause) {
	if .As_Checkbox in clause.flags {
		emit(p, " AS CHECKBOX")
	}
	if .Lower_Case in clause.flags {
		emit(p, " LOWER CASE")
	}
	if .Obligatory in clause.flags {
		emit(p, " OBLIGATORY")
	}
	if .No_Display in clause.flags {
		emit(p, " NO-DISPLAY")
	}
	if .Value_Check in clause.flags {
		emit(p, " VALUE CHECK")
	}
	if .Help_Request in clause.flags {
		emit(p, " HELP-REQUEST")
	}
	if .Value_Request in clause.flags {
		emit(p, " VALUE-REQUEST")
	}
	if group, ok := clause.radiobutton_group.?; ok {
		emit(p, " RADIOBUTTON GROUP ")
		emit(p, group)
	}
	if command, ok := clause.user_command.?; ok {
		emit(p, " USER-COMMAND ")
		emit(p, command)
	}
	if id, ok := clause.modif_id.?; ok {
		emit(p, " MODIF ID ")
		emit(p, id)
	}
	if clause.memory_id != nil {
		emit(p, " MEMORY ID ")
		emit_node(p, clause.memory_id)
	}
	if clause.matchcode_object != nil {
		emit(p, " MATCHCODE OBJECT ")
		emit_node(p, clause.matchcode_object)
	}
	if clause.visible_length != nil {
		emit(p, " VISIBLE LENGTH ")
		emit_node(p, clause.visible_length)
	}
}

emit_select_option_additions :: proc(p: ^Printer, clause: Select_Options_Clause) {
	if .Lower_Case in clause.flags {
		emit(p, " LOWER CASE")
	}
	if .Obligatory in clause.flags {
		emit(p, " OBLIGATORY")
	}
	if .No_Display in clause.flags {
		emit(p, " NO-DISPLAY")
	}
	if .No_Extension in clause.flags {
		emit(p, " NO-EXTENSION")
	}
	if .No_Intervals in clause.flags {
		emit(p, " NO INTERVALS")
	}
	if .No_Database_Selection in clause.flags {
		emit(p, " NO DATABASE SELECTION")
	}
	if id, ok := clause.modif_id.?; ok {
		emit(p, " MODIF ID ")
		emit(p, id)
	}
	if clause.memory_id != nil {
		emit(p, " MEMORY ID ")
		emit_node(p, clause.memory_id)
	}
	if clause.matchcode_object != nil {
		emit(p, " MATCHCODE OBJECT ")
		emit_node(p, clause.matchcode_object)
	}
	if clause.visible_length != nil {
		emit(p, " VISIBLE LENGTH ")
		emit_node(p, clause.visible_length)
	}
	if clause.help_request != nil {
		emit(p, " HELP-REQUEST FOR ")
		emit(p, clause.help_request.target)
	}
	if clause.value_request != nil {
		emit(p, " VALUE-REQUEST FOR ")
		emit(p, clause.value_request.target)
	}
}

emit_clear_stmt :: proc(p: ^Printer, stmt: ^Clear_Stmt) {
	emit(p, "CLEAR")
	emit(p, ": " if len(stmt.operands) > 1 else " ")
	for clause, i in stmt.operands {
		if i > 0 {
			emit(p, ", ")
		}
		emit_node(p, clause.target)
		if clause.mode == .With_Value {
			emit(p, " WITH ")
			emit_node(p, clause.value)
		} else if clause.mode == .Initial {
			emit(p, " INITIAL")
		}
	}
	emit(p, ".")
}

emit_refresh_stmt :: proc(p: ^Printer, stmt: ^Refresh_Stmt) {
	emit(p, "REFRESH")
	emit(p, ": " if len(stmt.operands) > 1 else " ")
	for clause, i in stmt.operands {
		if i > 0 {
			emit(p, ", ")
		}
		if clause.table {
			emit(p, "TABLE ")
		}
		emit_node(p, clause.target)
	}
	emit(p, ".")
}

emit_free_stmt :: proc(p: ^Printer, stmt: ^Free_Stmt) {
	emit(p, "FREE ")
	if stmt.memory {
		emit(p, "MEMORY")
		if stmt.memory_id != nil {
			emit(p, " ID ")
			emit_node(p, stmt.memory_id)
		}
		emit(p, ".")
		return
	}
	if len(stmt.operands) > 1 {
		emit(p, ": ")
	}
	for clause, i in stmt.operands {
		if i > 0 {
			emit(p, ", ")
		}
		if clause.object {
			emit(p, "OBJECT ")
		}
		emit_node(p, clause.target)
	}
	emit(p, ".")
}

emit_unassign_stmt :: proc(p: ^Printer, stmt: ^Unassign_Stmt) {
	emit(p, "UNASSIGN")
	emit(p, ": " if len(stmt.operands) > 1 else " ")
	for clause, i in stmt.operands {
		if i > 0 {
			emit(p, ", ")
		}
		emit_node(p, clause.target)
	}
	emit(p, ".")
}

emit_move_stmt :: proc(p: ^Printer, stmt: ^Move_Stmt) {
	emit(p, "MOVE")
	emit_move_entries(p, stmt.entries)
}

emit_move_corresponding_stmt :: proc(p: ^Printer, stmt: ^Move_Corresponding_Stmt) {
	emit(p, "MOVE-CORRESPONDING")
	emit_move_entries(p, stmt.entries)
}

emit_move_entries :: proc(p: ^Printer, entries: [dynamic]Move_Entry_Clause) {
	emit(p, ": " if len(entries) > 1 else " ")
	for entry, i in entries {
		if i > 0 {
			emit(p, ", ")
		}
		emit_node(p, entry.source)
		emit(p, " TO ")
		emit_node(p, entry.target)
	}
	emit(p, ".")
}

emit_add_stmt :: proc(p: ^Printer, stmt: ^Add_Stmt) {
	emit(p, "ADD")
	emit(p, ": " if len(stmt.entries) > 1 else " ")
	for entry, i in stmt.entries {
		if i > 0 {
			emit(p, ", ")
		}
		emit_node(p, entry.source)
		emit(p, " TO ")
		emit_node(p, entry.target)
		if entry.result != nil {
			emit(p, " GIVING ")
			emit_node(p, entry.result)
		}
	}
	emit(p, ".")
}

emit_subtract_stmt :: proc(p: ^Printer, stmt: ^Subtract_Stmt) {
	emit(p, "SUBTRACT")
	emit(p, ": " if len(stmt.entries) > 1 else " ")
	for entry, i in stmt.entries {
		if i > 0 {
			emit(p, ", ")
		}
		emit_node(p, entry.source)
		emit(p, " FROM ")
		emit_node(p, entry.target)
		if entry.result != nil {
			emit(p, " GIVING ")
			emit_node(p, entry.result)
		}
	}
	emit(p, ".")
}

emit_multiply_stmt :: proc(p: ^Printer, stmt: ^Multiply_Stmt) {
	emit(p, "MULTIPLY")
	emit(p, ": " if len(stmt.entries) > 1 else " ")
	for entry, i in stmt.entries {
		if i > 0 {
			emit(p, ", ")
		}
		emit_node(p, entry.target)
		emit(p, " BY ")
		emit_node(p, entry.source)
		if entry.result != nil {
			emit(p, " GIVING ")
			emit_node(p, entry.result)
		}
	}
	emit(p, ".")
}

emit_divide_stmt :: proc(p: ^Printer, stmt: ^Divide_Stmt) {
	emit(p, "DIVIDE")
	emit(p, ": " if len(stmt.entries) > 1 else " ")
	for entry, i in stmt.entries {
		if i > 0 {
			emit(p, ", ")
		}
		if entry.form == .Into {
			emit_node(p, entry.source)
			emit(p, " INTO ")
			emit_node(p, entry.target)
		} else {
			emit_node(p, entry.target)
			emit(p, " BY ")
			emit_node(p, entry.source)
		}
		if entry.result != nil {
			emit(p, " GIVING ")
			emit_node(p, entry.result)
		}
	}
	emit(p, ".")
}

emit_compute_stmt :: proc(p: ^Printer, stmt: ^Compute_Stmt) {
	emit(p, "COMPUTE")
	emit(p, ": " if len(stmt.entries) > 1 else " ")
	for entry, i in stmt.entries {
		if i > 0 {
			emit(p, ", ")
		}
		if entry.exact {
			emit(p, "EXACT ")
		}
		emit_node(p, entry.target)
		emit(p, " = ")
		emit_node(p, entry.source)
	}
	emit(p, ".")
}

emit_concatenate_stmt :: proc(p: ^Printer, stmt: ^Concatenate_Stmt) {
	emit(p, "CONCATENATE")
	emit(p, ": " if len(stmt.entries) > 1 else " ")
	for entry, i in stmt.entries {
		if i > 0 {
			emit(p, ", ")
		}
		if entry.lines_of {
			emit(p, "LINES OF ")
		}
		emit_expr_list(p, entry.sources, " ")
		emit(p, " INTO ")
		emit_node(p, entry.target)
		if entry.separator != nil {
			emit(p, " SEPARATED BY ")
			emit_node(p, entry.separator)
		}
		if entry.respecting_blanks {
			emit(p, " RESPECTING BLANKS")
		}
	}
	if stmt.byte_mode {
		emit(p, " IN BYTE MODE")
	}
	emit(p, ".")
}

emit_split_stmt :: proc(p: ^Printer, stmt: ^Split_Stmt) {
	emit(p, "SPLIT")
	emit(p, ": " if len(stmt.entries) > 1 else " ")
	for entry, i in stmt.entries {
		if i > 0 {
			emit(p, ", ")
		}
		emit_node(p, entry.source)
		emit(p, " AT ")
		emit_node(p, entry.separator)
		emit(p, " INTO ")
		if entry.into_table {
			emit(p, "TABLE ")
		}
		emit_expr_list(p, entry.targets, " ")
	}
	emit(p, ".")
}

emit_replace_stmt :: proc(p: ^Printer, stmt: ^Replace_Stmt) {
	emit(p, "REPLACE ")
	if stmt.occurrence == .First {
		emit(p, "FIRST OCCURRENCE OF ")
	} else if stmt.occurrence == .All {
		emit(p, "ALL OCCURRENCES OF ")
	}
	if stmt.regex {
		emit(p, "REGEX ")
	}
	if stmt.section_offset != nil || stmt.section_length != nil {
		emit(p, "SECTION ")
		if stmt.section_offset != nil {
			emit(p, "OFFSET ")
			emit_node(p, stmt.section_offset)
			emit_space(p)
		}
		if stmt.section_length != nil {
			emit(p, "LENGTH ")
			emit_node(p, stmt.section_length)
			emit_space(p)
		}
		emit(p, "OF ")
		emit_node(p, stmt.target)
	} else {
		emit_node(p, stmt.pattern)
		if stmt.target != nil {
			emit(p, " IN ")
			if stmt.in_table {
				emit(p, "TABLE ")
			}
			emit_node(p, stmt.target)
		}
	}
	if stmt.replacement != nil {
		emit(p, " WITH ")
		emit_node(p, stmt.replacement)
	}
	emit(p, ".")
}

emit_translate_stmt :: proc(p: ^Printer, stmt: ^Translate_Stmt) {
	emit(p, "TRANSLATE ")
	emit_node(p, stmt.target)
	switch stmt.form {
	case .Using:
		emit(p, " USING ")
		emit_node(p, stmt.operand)
	case .To_Upper:
		emit(p, " TO UPPER CASE")
	case .To_Lower:
		emit(p, " TO LOWER CASE")
	case .To_Code_Page:
		emit(p, " TO CODE PAGE ")
		emit_node(p, stmt.operand)
	case .From_Code_Page:
		emit(p, " FROM CODE PAGE ")
		emit_node(p, stmt.operand)
	case .To_Number_Format:
		emit(p, " TO NUMBER FORMAT ")
		emit_node(p, stmt.operand)
	case .From_Number_Format:
		emit(p, " FROM NUMBER FORMAT ")
		emit_node(p, stmt.operand)
	case .Default:
	}
	emit(p, ".")
}

emit_shift_stmt :: proc(p: ^Printer, stmt: ^Shift_Stmt) {
	emit(p, "SHIFT ")
	emit_node(p, stmt.target)
	if stmt.direction == .Left {
		emit(p, " LEFT")
	} else if stmt.direction == .Right {
		emit(p, " RIGHT")
	}
	if stmt.places != nil {
		emit(p, " BY ")
		emit_node(p, stmt.places)
		emit(p, " PLACES")
	}
	if stmt.circular {
		emit(p, " CIRCULAR")
	}
	if stmt.delete_direction != .None {
		emit(p, " DELETING ")
		emit(p, "LEADING" if stmt.delete_direction == .Leading else "TRAILING")
		emit_space(p)
		emit_node(p, stmt.delete_pattern)
	}
	emit(p, ".")
}

emit_find_stmt :: proc(p: ^Printer, stmt: ^Find_Stmt) {
	emit(p, "FIND ")
	if stmt.occurrence == .First {
		emit(p, "FIRST OCCURRENCE OF ")
	} else if stmt.occurrence == .All {
		emit(p, "ALL OCCURRENCES OF ")
	}
	if stmt.regex {
		emit(p, "REGEX ")
	}
	emit_node(p, stmt.pattern)
	if stmt.target != nil {
		emit(p, " IN ")
		if stmt.in_table {
			emit(p, "TABLE ")
		} else if stmt.section_offset != nil || stmt.section_length != nil {
			emit(p, "SECTION ")
			if stmt.section_offset != nil {
				emit(p, "OFFSET ")
				emit_node(p, stmt.section_offset)
				emit_space(p)
			}
			if stmt.section_length != nil {
				emit(p, "LENGTH ")
				emit_node(p, stmt.section_length)
				emit_space(p)
			}
			emit(p, "OF ")
		}
		emit_node(p, stmt.target)
	}
	if stmt.match_offset != nil {
		emit(p, " MATCH OFFSET ")
		emit_node(p, stmt.match_offset)
	}
	if stmt.match_length != nil {
		emit(p, " MATCH LENGTH ")
		emit_node(p, stmt.match_length)
	}
	if stmt.match_line != nil {
		emit(p, " MATCH LINE ")
		emit_node(p, stmt.match_line)
	}
	if stmt.match_count != nil {
		emit(p, " MATCH COUNT ")
		emit_node(p, stmt.match_count)
	}
	if stmt.results != nil {
		emit(p, " RESULTS ")
		emit_node(p, stmt.results)
	}
	if len(stmt.submatches) > 0 {
		emit(p, " SUBMATCHES ")
		emit_expr_list(p, stmt.submatches, " ")
	}
	emit(p, ".")
}

emit_search_stmt :: proc(p: ^Printer, stmt: ^Search_Stmt) {
	emit(p, "SEARCH ")
	emit_node(p, stmt.target)
	if stmt.pattern != nil {
		emit(p, " FOR ")
		emit_node(p, stmt.pattern)
	}
	if stmt.starting_at != nil {
		emit(p, " STARTING AT ")
		emit_node(p, stmt.starting_at)
	}
	if stmt.ending_at != nil {
		emit(p, " ENDING AT ")
		emit_node(p, stmt.ending_at)
	}
	if stmt.abbreviated {
		emit(p, " ABBREVIATED")
	}
	emit(p, ".")
}

emit_perform_stmt :: proc(p: ^Printer, stmt: ^Perform_Stmt) {
	emit(p, "PERFORM ")
	emit_node(p, stmt.form)
	if stmt.has_program_clause || stmt.program != nil {
		emit(p, " IN PROGRAM")
		if stmt.program != nil {
			emit(p, " ")
			emit_node(p, stmt.program)
		}
	}
	if len(stmt.tables) > 0 {
		emit(p, " TABLES ")
		emit_expr_list(p, stmt.tables, " ")
	}
	if len(stmt.using_args) > 0 {
		emit(p, " USING ")
		emit_expr_list(p, stmt.using_args, " ")
	}
	if len(stmt.changing) > 0 {
		emit(p, " CHANGING ")
		emit_expr_list(p, stmt.changing, " ")
	}
	if stmt.if_found {
		emit(p, " IF FOUND")
	}
	emit(p, ".")
}

emit_call_stmt :: proc(p: ^Printer, stmt: ^Call_Stmt) {
	if stmt.kind == .Direct {
		emit_node(p, stmt.call)
		emit(p, ".")
		return
	}
	emit(p, "CALL ")
	emit(p, call_kind_text(stmt.kind))
	if stmt.target != nil {
		emit_space(p)
		emit_node(p, stmt.target)
	}
	current_kind: Call_Transformation_Arg_Kind
	has_current_kind := false
	for arg in stmt.transformation_args {
		if !has_current_kind || current_kind != arg.kind {
			emit_space(p)
			emit(p, call_transformation_arg_kind_text(arg.kind))
			current_kind = arg.kind
			has_current_kind = true
		}
		if arg.name.text != "" {
			emit_space(p)
			emit(p, arg.name)
			if arg.has_eq {
				emit(p, " =")
			}
		}
		if arg.value != nil {
			emit_space(p)
			emit_node(p, arg.value)
		}
	}
	emit(p, ".")
}

emit_submit_stmt :: proc(p: ^Printer, stmt: ^Submit_Stmt) {
	emit(p, "SUBMIT ")
	emit_node(p, stmt.target)
	for option in stmt.options {
		emit_submit_option(p, option)
	}
	if stmt.via_selection_screen {
		emit(p, " VIA SELECTION-SCREEN")
	}
	if stmt.exporting_list_to_memory {
		emit(p, " EXPORTING LIST TO MEMORY")
	}
	if stmt.to_sap_spool {
		emit(p, " TO SAP-SPOOL")
	}
	if stmt.without_spool_dynpro {
		emit(p, " WITHOUT SPOOL DYNPRO")
	}
	if stmt.and_return {
		emit(p, " AND RETURN")
	}
	emit(p, ".")
}

emit_submit_option :: proc(p: ^Printer, option: Submit_Option_Clause) {
	switch option.kind {
	case .Using_Selection_Screen:
		emit(p, " USING SELECTION-SCREEN ")
		emit_node(p, option.value)
	case .Using_Selection_Set:
		emit(p, " USING SELECTION-SET ")
		emit_node(p, option.value)
	case .With_Selection_Table:
		emit(p, " WITH SELECTION-TABLE ")
		emit_node(p, option.value)
	case .With_Free_Selections:
		emit(p, " WITH FREE SELECTIONS ")
		emit_node(p, option.value)
	case .With_Parameter:
		emit(p, " WITH ")
		emit(p, option.name)
		if option.operator != .None {
			emit_space(p)
			emit(p, submit_operator_text(option.operator))
			emit_space(p)
			emit_node(p, option.value)
		}
	case .Line_Size:
		emit(p, " LINE-SIZE ")
		emit_node(p, option.value)
	case .Line_Count:
		emit(p, " LINE-COUNT ")
		emit_node(p, option.value)
	case .User:
		emit(p, " USER ")
		emit_node(p, option.value)
	case .Via_Job:
		emit(p, " VIA JOB ")
		emit_node(p, option.value)
	case .Number:
		emit(p, " NUMBER ")
		emit_node(p, option.value)
	case .Language:
		emit(p, " LANGUAGE ")
		emit_node(p, option.value)
	case .Using_Selection_Sets_Of_Program:
	}
}

emit_message_stmt :: proc(p: ^Printer, stmt: ^Message_Stmt) {
	emit(p, "MESSAGE")
	emit_message_head(p, stmt.head)
	if len(stmt.with_args) > 0 {
		emit(p, " WITH ")
		emit_expr_list(p, stmt.with_args, " ")
	}
	if stmt.into != nil {
		emit(p, " INTO ")
		emit_node(p, stmt.into)
	}
	if stmt.display_like != nil {
		emit(p, " DISPLAY LIKE ")
		emit_node(p, stmt.display_like)
	}
	if stmt.raising != nil {
		emit(p, " RAISING ")
		emit_node(p, stmt.raising)
	}
	emit(p, ".")
}

emit_message_head :: proc(p: ^Printer, head: ^Message_Head_Clause) {
	if head == nil {
		return
	}
	if head.id != nil {
		emit(p, " ID ")
		emit_node(p, head.id)
		if head.msg_type != nil {
			emit(p, " TYPE ")
			emit_node(p, head.msg_type)
		}
		if head.number != nil {
			emit(p, " NUMBER ")
			emit_node(p, head.number)
		}
		return
	}
	if head.code != nil {
		emit_space(p)
		emit_node(p, head.code)
	}
	if head.msg_type != nil {
		emit(p, " TYPE ")
		emit_node(p, head.msg_type)
	}
}

emit_write_stmt :: proc(p: ^Printer, stmt: ^Write_Stmt) {
	emit(p, "WRITE")
	if len(stmt.operands) > 0 {
		emit_space(p)
	}
	for clause, i in stmt.operands {
		if i > 0 {
			emit_space(p)
		}
		if clause.line_break {
			emit(p, "/")
		}
		if clause.position != nil {
			if !clause.line_break {
				emit(p, "AT ")
			}
			emit_node(p, clause.position)
		}
		if clause.length != nil {
			emit(p, "(")
			emit_node(p, clause.length)
			emit(p, ")")
		}
		if clause.value != nil {
			if clause.line_break || clause.position != nil || clause.length != nil {
				emit_space(p)
			}
			emit_node(p, clause.value)
		}
	}
	emit(p, ".")
}

emit_write_to_stmt :: proc(p: ^Printer, stmt: ^Write_To_Stmt) {
	emit(p, "WRITE")
	if len(stmt.entries) > 0 {
		emit_space(p)
	}
	for entry, i in stmt.entries {
		if i > 0 {
			emit_space(p)
		}
		emit_node(p, entry.source)
		emit(p, " TO ")
		emit_node(p, entry.target)
	}
	emit(p, ".")
}

emit_describe_stmt :: proc(p: ^Printer, stmt: ^Describe_Stmt) {
	emit(p, "DESCRIBE")
	emit(p, ": " if len(stmt.entries) > 1 else " ")
	for entry, i in stmt.entries {
		if i > 0 {
			emit(p, ", ")
		}
		if entry.table {
			emit(p, "TABLE ")
		}
		emit_node(p, entry.source)
		if entry.target != nil {
			emit(p, " LINES ")
			emit_node(p, entry.target)
		}
	}
	emit(p, ".")
}

emit_runtime_stmt :: proc(p: ^Printer, stmt: ^Runtime_Stmt) {
	emit(p, runtime_kind_text(stmt.kind))
	switch stmt.subject {
	case .None:
	case .Run_Time_Field:
		emit(p, " RUN TIME FIELD ")
		emit_node(p, stmt.target)
	case .Time_Stamp_Field:
		emit(p, " TIME STAMP FIELD ")
		emit_node(p, stmt.target)
	case .Parameter_ID_Field:
		emit(p, " PARAMETER ID ")
		emit_node(p, stmt.id)
		emit(p, " FIELD ")
		emit_node(p, stmt.field)
	case .Cursor:
		emit(p, " CURSOR")
		if stmt.field != nil {
			emit(p, " FIELD ")
			emit_node(p, stmt.field)
		}
		if stmt.line != nil {
			emit(p, " LINE ")
			emit_node(p, stmt.line)
		}
		if stmt.offset != nil {
			emit(p, " OFFSET ")
			emit_node(p, stmt.offset)
		}
		if stmt.value != nil {
			emit(p, " VALUE ")
			emit_node(p, stmt.value)
		}
	case .Reference:
		emit(p, " REFERENCE")
		if stmt.value != nil {
			emit(p, " OF ")
			emit_node(p, stmt.value)
		}
		if stmt.target != nil {
			emit(p, " INTO ")
			emit_node(p, stmt.target)
		}
	case .PF_Status:
		emit(p, " PF-STATUS ")
		emit_node(p, stmt.target)
		if len(stmt.excluding) > 0 {
			emit(p, " EXCLUDING ")
			emit_expr_list(p, stmt.excluding, " ")
		}
	case .Titlebar:
		emit(p, " TITLEBAR ")
		emit_node(p, stmt.target)
	case .Screen:
		emit(p, " SCREEN ")
		emit_node(p, stmt.target)
	case .User_Command:
		emit(p, " USER-COMMAND ")
		emit_node(p, stmt.target)
	case .Badi:
		emit(p, " BADI ")
		emit_node(p, stmt.target)
	case .Update_Task_Local:
		emit(p, " UPDATE TASK LOCAL")
	}
	if len(stmt.operands) > 0 {
		emit_space(p)
		emit_expr_list(p, stmt.operands, " ")
	}
	emit(p, ".")
}

emit_set_handler_stmt :: proc(p: ^Printer, stmt: ^Set_Handler_Stmt) {
	emit(p, "SET HANDLER")
	if len(stmt.handlers) > 0 {
		emit_space(p)
		emit_expr_list(p, stmt.handlers, " ")
	}
	if stmt.all_instances {
		emit(p, " FOR ALL INSTANCES")
	} else if stmt.sender != nil {
		emit(p, " FOR ")
		emit_node(p, stmt.sender)
	}
	if stmt.activation != nil {
		emit(p, " ACTIVATION ")
		emit_node(p, stmt.activation)
	}
	emit(p, ".")
}

emit_import_stmt :: proc(p: ^Printer, stmt: ^Import_Stmt) {
	emit(p, "IMPORT")
	if len(stmt.parameters) > 0 {
		emit(p, " ")
		emit_data_cluster_parameters(p, stmt.parameters)
	}
	emit(p, " FROM ")
	emit_data_cluster_medium(p, stmt.medium, "TO")
	emit(p, ".")
}

emit_export_stmt :: proc(p: ^Printer, stmt: ^Export_Stmt) {
	emit(p, "EXPORT")
	if len(stmt.parameters) > 0 {
		emit(p, " ")
		emit_data_cluster_parameters(p, stmt.parameters)
	}
	emit(p, " TO ")
	emit_data_cluster_medium(p, stmt.medium, "FROM")
	emit(p, ".")
}

emit_data_cluster_medium :: proc(p: ^Printer, medium: Data_Cluster_Medium_Clause, work_area_keyword: string) {
	switch medium.kind {
	case .Data_Buffer:
		emit(p, "DATA BUFFER ")
		emit_node(p, medium.object)
	case .Internal_Table:
		emit(p, "INTERNAL TABLE ")
		emit_node(p, medium.object)
	case .Memory_ID:
		emit(p, "MEMORY ID ")
		emit_node(p, medium.id)
	case .Database:
		emit(p, "DATABASE ")
		emit_data_cluster_database_medium(p, medium, work_area_keyword)
	case .Shared_Memory:
		emit(p, "SHARED MEMORY ")
		emit_data_cluster_database_medium(p, medium, work_area_keyword)
	case .Shared_Buffer:
		emit(p, "SHARED BUFFER ")
		emit_data_cluster_database_medium(p, medium, work_area_keyword)
	}
}

emit_data_cluster_database_medium :: proc(
	p: ^Printer,
	medium: Data_Cluster_Medium_Clause,
	work_area_keyword: string,
) {
	emit(p, medium.dbtab)
	emit(p, "(")
	emit(p, medium.area)
	emit(p, ")")
	if medium.work_area != nil {
		emit_space(p)
		emit(p, work_area_keyword)
		emit(p, " ")
		emit_node(p, medium.work_area)
	}
	if medium.client != nil {
		emit(p, " CLIENT ")
		emit_node(p, medium.client)
	}
	if medium.id != nil {
		emit(p, " ID ")
		emit_node(p, medium.id)
	}
}

emit_data_cluster_parameters :: proc(
	p: ^Printer,
	parameters: [dynamic]Data_Cluster_Parameter_Clause,
) {
	for entry, i in parameters {
		if i > 0 {
			emit(p, " ")
		}
		if entry.name.text != "" {
			emit(p, entry.name)
			emit(p, " = ")
		}
		emit_node(p, entry.value)
	}
}

emit_bit_stmt :: proc(p: ^Printer, stmt: ^Bit_Stmt) {
	if stmt.kind == .Get {
		emit(p, "GET BIT ")
		emit_node(p, stmt.position)
		emit(p, " OF ")
		emit_node(p, stmt.source)
		emit(p, " INTO ")
		emit_node(p, stmt.target)
	} else {
		emit(p, "SET BIT ")
		emit_node(p, stmt.position)
		emit(p, " OF ")
		emit_node(p, stmt.target)
		emit(p, " TO ")
		emit_node(p, stmt.value)
	}
	emit(p, ".")
}

emit_locale_stmt :: proc(p: ^Printer, stmt: ^Locale_Stmt) {
	emit(p, "GET" if stmt.kind == .Get else "SET")
	emit(p, " LOCALE")
	if stmt.language != nil {
		emit(p, " LANGUAGE ")
		emit_node(p, stmt.language)
	}
	if stmt.country != nil {
		emit(p, " COUNTRY ")
		emit_node(p, stmt.country)
	}
	if stmt.modifier != nil {
		emit(p, " MODIFIER ")
		emit_node(p, stmt.modifier)
	}
	emit(p, ".")
}

emit_set_cursor_stmt :: proc(p: ^Printer, stmt: ^Set_Cursor_Stmt) {
	emit(p, "SET CURSOR")
	if stmt.field != nil {
		emit(p, " FIELD ")
		emit_node(p, stmt.field)
		if stmt.offset != nil {
			emit(p, " OFFSET ")
			emit_node(p, stmt.offset)
		}
	} else {
		if stmt.line != nil {
			emit_space(p)
			emit_node(p, stmt.line)
		}
		if stmt.column != nil {
			emit_space(p)
			emit_node(p, stmt.column)
		}
	}
	emit(p, ".")
}

emit_receive_results_stmt :: proc(p: ^Printer, stmt: ^Receive_Results_Stmt) {
	emit(p, "RECEIVE RESULTS FROM FUNCTION")
	if stmt.target != nil {
		emit_space(p)
		emit_node(p, stmt.target)
	}
	emit(p, ".")
}

emit_authority_check_stmt :: proc(p: ^Printer, stmt: ^Authority_Check_Stmt) {
	emit(p, "AUTHORITY-CHECK")
	if stmt.object != nil {
		emit(p, " OBJECT ")
		emit_node(p, stmt.object)
		for clause in stmt.ids {
			emit(p, " ID ")
			emit_node(p, clause.id)
			if clause.field != nil {
				emit(p, " FIELD ")
				emit_node(p, clause.field)
			}
		}
	} else if len(stmt.operands) > 0 {
		emit_space(p)
		emit_expr_list(p, stmt.operands, " ")
	}
	emit(p, ".")
}

emit_line_stmt :: proc(p: ^Printer, stmt: ^Line_Stmt) {
	emit(p, "READ" if stmt.kind == .Read else "MODIFY")
	if stmt.current {
		emit(p, " CURRENT LINE")
	} else {
		emit(p, " LINE")
		if stmt.line != nil {
			emit_space(p)
			emit_node(p, stmt.line)
		}
	}
	if stmt.index != nil {
		emit(p, " INDEX ")
		emit_node(p, stmt.index)
	}
	if stmt.into != nil {
		emit(p, " INTO ")
		emit_node(p, stmt.into)
	}
	for clause in stmt.fields {
		emit(p, " FIELD VALUE ")
		emit_node(p, clause.field)
		if clause.target != nil {
			emit(p, " INTO ")
			emit_node(p, clause.target)
		}
	}
	emit(p, ".")
}

emit_raise_stmt :: proc(p: ^Printer, stmt: ^Raise_Stmt) {
	emit(p, "RAISE ")
	emit(p, "EVENT" if stmt.kind == .Event else "EXCEPTION")
	if stmt.target != nil {
		emit_space(p)
		if stmt.target_type {
			emit(p, "TYPE ")
		}
		emit_node(p, stmt.target)
	}
	if len(stmt.operands) > 0 {
		emit_space(p)
		emit_expr_list(p, stmt.operands, " ")
	}
	emit(p, ".")
}

emit_macro_def_stmt :: proc(p: ^Printer, stmt: ^Macro_Def_Stmt) {
	emit(p, "DEFINE ")
	emit(p, stmt.name)
	emit_block(p, stmt.body, "END-OF-DEFINITION")
}

emit_oop_simple_stmt :: proc(p: ^Printer, stmt: ^Oop_Simple_Stmt) {
	if stmt.kind == .Aliases && len(stmt.aliases) > 0 {
		emit(p, "ALIASES")
		if len(stmt.aliases) > 1 {
			emit(p, ":")
		}
		for alias, i in stmt.aliases {
			if i > 0 {
				emit(p, ",")
			}
			emit_space(p)
			emit(p, alias.name)
			emit(p, " FOR ")
			emit_node(p, alias.target)
		}
		emit(p, ".")
		return
	}
	if len(stmt.members) == 0 {
		if stmt.text != "" {
			emit(p, stmt.text)
		} else if stmt.kind == .Class_Section && stmt.visibility != .Unspecified {
			emit(p, oop_visibility_text(stmt.visibility))
			emit(p, " SECTION.")
		}
		return
	}
	emit(p, oop_simple_kind_text(stmt.kind))
	if len(stmt.members) > 1 {
		emit(p, ":")
	}
	for member, i in stmt.members {
		if i > 0 {
			emit(p, ",")
		}
		emit_space(p)
		emit(p, member.name)
		for sig in member.signatures {
			emit_space(p)
			emit(p, oop_signature_kind_text(sig.kind))
			if len(sig.values) > 0 {
				emit_space(p)
				emit_expr_list(p, sig.values, " ")
			}
		}
	}
	emit(p, ".")
}

emit_oop_load_stmt :: proc(p: ^Printer, stmt: ^Oop_Load_Stmt) {
	if stmt.text != "" {
		emit(p, stmt.text)
		return
	}
	if stmt.kind == .Class {
		emit(p, "CLASS ")
		emit(p, stmt.name)
		emit(p, " DEFINITION LOAD.")
	} else {
		emit(p, "INTERFACE ")
		emit(p, stmt.name)
		emit(p, " LOAD.")
	}
}

emit_amdp_method :: proc(p: ^Printer, stmt: ^Method_Decl) {
	if stmt.header_text != "" {
		emit(p, stmt.header_text)
	} else {
		emit(p, "METHOD ")
		emit(p, stmt.name)
	}
	emit(p, ".")
	if stmt.amdp_body != "" {
		emit(p, stmt.amdp_body)
	} else {
		emit_newline(p)
	}
	emit(p, "ENDMETHOD.")
}

emit_if_stmt :: proc(p: ^Printer, stmt: ^If_Stmt) {
	emit(p, "IF ")
	emit_node(p, stmt.condition)
	emit(p, ".")
	p.indent_level += 1
	if len(stmt.body) > 0 {
		emit_newline(p)
		emit_stmt_list(p, stmt.body)
	}
	p.indent_level -= 1
	for clause in stmt.elseif_clauses {
		emit_newline(p)
		emit(p, "ELSEIF ")
		emit_node(p, clause.condition)
		emit(p, ".")
		p.indent_level += 1
		if len(clause.body) > 0 {
			emit_newline(p)
			emit_stmt_list(p, clause.body)
		}
		p.indent_level -= 1
	}
	if stmt.else_clause != nil {
		emit_newline(p)
		emit(p, "ELSE.")
		p.indent_level += 1
		if len(stmt.else_clause.body) > 0 {
			emit_newline(p)
			emit_stmt_list(p, stmt.else_clause.body)
		}
		p.indent_level -= 1
	}
	emit_newline(p)
	emit(p, "ENDIF.")
}

emit_case_stmt :: proc(p: ^Printer, stmt: ^Case_Stmt) {
	emit(p, "CASE ")
	if stmt.is_type_of {
		emit(p, "TYPE OF ")
	}
	emit_node(p, stmt.expr)
	emit(p, ".")
	for clause in stmt.whens {
		emit_newline(p)
		emit(p, "WHEN")
		if clause.is_others {
			emit(p, " OTHERS")
		} else if len(clause.operands) > 0 {
			emit_space(p)
			emit_expr_list(p, clause.operands, " OR ")
		}
		emit(p, ".")
		p.indent_level += 1
		if len(clause.body) > 0 {
			emit_newline(p)
			emit_stmt_list(p, clause.body)
		}
		p.indent_level -= 1
	}
	emit_newline(p)
	emit(p, "ENDCASE.")
}

emit_try_stmt :: proc(p: ^Printer, stmt: ^Try_Stmt) {
	emit(p, "TRY.")
	p.indent_level += 1
	if len(stmt.body) > 0 {
		emit_newline(p)
		emit_stmt_list(p, stmt.body)
	}
	p.indent_level -= 1
	for clause in stmt.catches {
		emit_newline(p)
		emit(p, "CATCH ")
		emit_expr_list(p, clause.exceptions, " ")
		if clause.into != nil {
			emit(p, " INTO ")
			emit_node(p, clause.into)
		}
		emit(p, ".")
		p.indent_level += 1
		if len(clause.body) > 0 {
			emit_newline(p)
			emit_stmt_list(p, clause.body)
		}
		p.indent_level -= 1
	}
	if stmt.cleanup != nil {
		emit_newline(p)
		emit(p, "CLEANUP.")
		p.indent_level += 1
		if len(stmt.cleanup.body) > 0 {
			emit_newline(p)
			emit_stmt_list(p, stmt.cleanup.body)
		}
		p.indent_level -= 1
	}
	emit_newline(p)
	emit(p, "ENDTRY.")
}

emit_named_block :: proc(
	p: ^Printer,
	start_keyword, name, header_text: string,
	body: [dynamic]^Stmt,
	end_keyword: string,
) {
	if header_text != "" {
		emit(p, header_text)
	} else {
		emit(p, start_keyword)
		if name != "" {
			emit_space(p)
			emit(p, name)
		}
	}
	if end_keyword == "" {
		emit(p, ".")
		p.indent_level += 1
		if len(body) > 0 {
			emit_newline(p)
			emit_stmt_list(p, body)
		}
		p.indent_level -= 1
	} else {
		emit_block(p, body, end_keyword)
	}
}

emit_select_stmt :: proc(p: ^Printer, stmt: ^Select_Stmt) {
	if stmt.with != nil {
		emit_select_with(p, stmt.with)
		emit_space(p)
	}
	emit_select_query(p, stmt.query)
	if len(stmt.body) == 0 {
		emit(p, ".")
		return
	}
	emit_block(p, stmt.body, "ENDSELECT")
}

emit_select_with :: proc(p: ^Printer, clause: ^Select_With_Clause) {
	emit(p, "WITH")
	if len(clause.entries) == 0 {
		return
	}
	emit_space(p)
	for entry, i in clause.entries {
		if i > 0 {
			emit(p, ", ")
		}
		emit(p, entry.name)
		emit(p, " AS ( ")
		emit_select_query(p, entry.query)
		emit(p, " )")
	}
}

emit_select_query :: proc(p: ^Printer, query: Select_Query_Clause) {
	emit(p, "SELECT")
	if query.single {
		emit(p, " SINGLE")
	}
	if query.is_distinct {
		emit(p, " DISTINCT")
	}
	if len(query.projection_clauses) > 0 {
		emit_space(p)
		emit_select_projections(p, query.projection_clauses)
	} else if len(query.projections) > 0 {
		emit_space(p)
		emit_expr_list(p, query.projections, " ")
	}
	if query.source_clause != nil {
		emit(p, " FROM ")
		emit_select_source(p, query.source_clause)
	} else if query.source != nil {
		emit(p, " FROM ")
		emit_node(p, query.source)
	}
	if query.result != nil {
		emit_select_result(p, query.result)
	}
	if query.for_all_entries != nil {
		emit(p, " FOR ALL ENTRIES IN ")
		emit_node(p, query.for_all_entries)
	}
	if query.where_cond != nil {
		emit(p, " WHERE ")
		emit_node(p, query.where_cond)
	}
	if query.order_by_primary_key || len(query.order_by_fields) > 0 {
		emit(p, " ORDER BY ")
		if query.order_by_primary_key {
			emit(p, "PRIMARY KEY")
		} else {
			for field, i in query.order_by_fields {
				if i > 0 {
					emit(p, ", ")
				}
				emit(p, field)
			}
		}
	}
	if query.package_size != nil {
		emit(p, " PACKAGE SIZE ")
		emit_node(p, query.package_size)
	}
	if query.up_to_rows != nil {
		emit(p, " UP TO ")
		emit_node(p, query.up_to_rows)
		emit(p, " ROWS")
	}
	if query.for_update_clause.end > query.for_update_clause.start {
		emit(p, " FOR UPDATE")
	}
	for set_op in query.set_ops {
		emit_space(p)
		emit(p, select_set_kind_text(set_op.kind))
		if set_op.all {
			emit(p, " ALL")
		}
		emit_space(p)
		emit_select_query(p, set_op.query)
	}
}

emit_select_projections :: proc(p: ^Printer, clauses: [dynamic]Select_Projection_Clause) {
	for clause, i in clauses {
		if i > 0 {
			emit(p, ", ")
		}
		emit_node(p, clause.value)
		if clause.alias.text != "" {
			emit(p, " AS ")
			emit(p, clause.alias)
		}
	}
}

emit_select_source :: proc(p: ^Printer, clause: ^Select_Source_Clause) {
	emit_node(p, clause.source)
	if clause.alias.text != "" {
		emit(p, " AS ")
		emit(p, clause.alias)
	}
	for join in clause.joins {
		emit_space(p)
		emit(p, select_join_kind_text(join.kind))
		emit(p, " JOIN ")
		emit_node(p, join.source)
		if join.alias.text != "" {
			emit(p, " AS ")
			emit(p, join.alias)
		}
		if join.on != nil {
			emit(p, " ON ")
			emit_node(p, join.on)
		}
	}
}

emit_select_result :: proc(p: ^Printer, clause: ^Select_Result_Clause) {
	emit(p, " ")
	emit(p, "APPENDING" if clause.kind == .Appending else "INTO")
	if clause.corresponding_fields {
		emit(p, " CORRESPONDING FIELDS OF")
	}
	if clause.table {
		emit(p, " TABLE")
	}
	if clause.target != nil {
		emit_space(p)
		emit_node(p, clause.target)
	}
}

emit_open_cursor_stmt :: proc(p: ^Printer, stmt: ^Open_Cursor_Stmt) {
	emit(p, "OPEN CURSOR")
	if stmt.with_hold {
		emit(p, " WITH HOLD")
	}
	if stmt.handle != nil {
		emit_space(p)
		emit_node(p, stmt.handle)
	}
	emit(p, " FOR ")
	emit_select_query(p, stmt.query)
	emit(p, ".")
}

emit_fetch_stmt :: proc(p: ^Printer, stmt: ^Fetch_Stmt) {
	emit(p, "FETCH NEXT CURSOR ")
	emit_node(p, stmt.handle)
	if stmt.result != nil {
		emit_select_result(p, stmt.result)
	}
	if stmt.package_size != nil {
		emit(p, " PACKAGE SIZE ")
		emit_node(p, stmt.package_size)
	}
	emit(p, ".")
}

emit_table_key_selector :: proc(p: ^Printer, selector: Table_Key_Selector) {
	if selector.dynamic_name != nil {
		emit(p, "(")
		emit_node(p, selector.dynamic_name)
		emit(p, ")")
	} else {
		emit(p, selector.name)
	}
}

emit_read_table_stmt :: proc(p: ^Printer, stmt: ^Read_Table_Stmt) {
	emit(p, "READ TABLE")
	for entry, i in stmt.entries {
		if i > 0 {
			emit(p, ",")
		}
		emit_space(p)
		emit_node(p, entry.table)
		if entry.into != nil {
			emit(p, " INTO ")
			emit_node(p, entry.into)
		}
		if entry.assigning != nil {
			emit(p, " ASSIGNING ")
			emit_node(p, entry.assigning)
		}
		if entry.reference_into != nil {
			emit(p, " REFERENCE INTO ")
			emit_node(p, entry.reference_into)
		}
		if entry.key_kind != .None {
			emit(p, " WITH")
			if entry.key_kind == .Table_Key {
				emit(p, " TABLE")
			}
			emit(p, " KEY")
			if entry.key_name.text != "" {
				emit_space(p)
				emit(p, entry.key_name)
			}
			if entry.key_name.text != "" && len(entry.key_values) > 0 {
				emit(p, " COMPONENTS")
			}
			for key in entry.key_values {
				emit_space(p)
				emit(p, key.name)
				emit(p, " = ")
				emit_node(p, key.value)
			}
		}
		if entry.index != nil {
			emit(p, " INDEX ")
			emit_node(p, entry.index)
		}
		if entry.using_key.name.text != "" || entry.using_key.dynamic_name != nil {
			emit(p, " USING KEY ")
			emit_table_key_selector(p, entry.using_key)
		}
		if entry.transporting_no_fields {
			emit(p, " TRANSPORTING NO FIELDS")
		}
		if entry.binary_search {
			emit(p, " BINARY SEARCH")
		}
		if len(entry.comparing) > 0 {
			emit(p, " COMPARING ")
			emit_expr_list(p, entry.comparing, " ")
		}
	}
	emit(p, ".")
}

emit_insert_stmt :: proc(p: ^Printer, stmt: ^Insert_Stmt) {
	emit(p, "INSERT ")
	if stmt.form == .Db_Table {
		if stmt.into_db_table {
			emit(p, "INTO ")
		}
		emit_node(p, stmt.target)
		if len(stmt.assignments) > 0 {
			emit(p, " SET ")
			emit_sql_assignments(p, stmt.assignments)
		} else if stmt.source != nil {
			emit(p, " VALUES " if stmt.values_clause else " FROM ")
			if stmt.from_table {
				emit(p, "TABLE ")
			}
			emit_node(p, stmt.source)
		}
		if stmt.accepting_duplicate_keys {
			emit(p, " ACCEPTING DUPLICATE KEYS")
		}
		emit(p, ".")
		return
	}
	if stmt.form == .Lines_Of {
		emit(p, "LINES OF ")
	}
	if stmt.initial_line {
		emit(p, "INITIAL LINE")
	} else {
		emit_node(p, stmt.source)
	}
	if stmt.target != nil {
		emit(p, " INTO")
		if stmt.form == .Internal_Table || stmt.form == .Lines_Of {
			emit(p, " TABLE")
		}
		emit_space(p)
		emit_node(p, stmt.target)
	}
	if stmt.index != nil {
		emit(p, " INDEX ")
		emit_node(p, stmt.index)
	}
	if stmt.assigning != nil {
		emit(p, " ASSIGNING ")
		emit_node(p, stmt.assigning)
	}
	if stmt.reference_into != nil {
		emit(p, " REFERENCE INTO ")
		emit_node(p, stmt.reference_into)
	}
	emit(p, ".")
}

emit_append_stmt :: proc(p: ^Printer, stmt: ^Append_Stmt) {
	emit(p, "APPEND ")
	if stmt.lines_of {
		emit(p, "LINES OF ")
	}
	if stmt.initial_line {
		emit(p, "INITIAL LINE")
	} else {
		emit_node(p, stmt.source)
	}
	if stmt.target != nil {
		emit(p, " TO ")
		if stmt.sorted {
			emit(p, "SORTED ")
		}
		emit_node(p, stmt.target)
	}
	if stmt.assigning != nil {
		emit(p, " ASSIGNING ")
		emit_node(p, stmt.assigning)
	}
	if stmt.reference_into != nil {
		emit(p, " REFERENCE INTO ")
		emit_node(p, stmt.reference_into)
	}
	emit(p, ".")
}

emit_modify_stmt :: proc(p: ^Printer, stmt: ^Modify_Stmt) {
	emit(p, "MODIFY ")
	if stmt.table_keyword {
		emit(p, "TABLE ")
	}
	emit_node(p, stmt.target)
	if stmt.source != nil {
		emit(p, " FROM ")
		emit_node(p, stmt.source)
	}
	if stmt.index != nil {
		emit(p, " INDEX ")
		emit_node(p, stmt.index)
	}
	if len(stmt.transporting) > 0 {
		emit(p, " TRANSPORTING ")
		for field, i in stmt.transporting {
			if i > 0 {
				emit(p, " ")
			}
			emit(p, field.name)
		}
	}
	if stmt.where_cond != nil {
		emit(p, " WHERE ")
		emit_node(p, stmt.where_cond)
	}
	emit(p, ".")
}

emit_sort_stmt :: proc(p: ^Printer, stmt: ^Sort_Stmt) {
	emit(p, "SORT ")
	if stmt.stable {
		emit(p, "STABLE ")
	}
	emit_node(p, stmt.target)
	if stmt.as_text {
		emit(p, " AS TEXT")
	}
	if len(stmt.fields) > 0 {
		emit(p, " BY ")
		for field, i in stmt.fields {
			if i > 0 {
				emit(p, " ")
			}
			emit_node(p, field.expr)
			if field.ascending {
				emit(p, " ASCENDING")
			}
			if field.descending {
				emit(p, " DESCENDING")
			}
			if field.as_text {
				emit(p, " AS TEXT")
			}
		}
	}
	if stmt.descending {
		emit(p, " DESCENDING")
	}
	emit(p, ".")
}

emit_update_stmt :: proc(p: ^Printer, stmt: ^Update_Stmt) {
	emit(p, "UPDATE ")
	emit_node(p, stmt.target)
	if stmt.source != nil {
		emit(p, " FROM ")
		if stmt.from_table {
			emit(p, "TABLE ")
		}
		emit_node(p, stmt.source)
	}
	if len(stmt.assignments) > 0 {
		emit(p, " SET ")
		emit_sql_assignments(p, stmt.assignments)
	}
	if stmt.where_cond != nil {
		emit(p, " WHERE ")
		emit_node(p, stmt.where_cond)
	}
	emit(p, ".")
}

emit_delete_stmt :: proc(p: ^Printer, stmt: ^Delete_Stmt) {
	emit(p, "DELETE ")
	if stmt.form == .Adjacent_Duplicates {
		emit(p, "ADJACENT DUPLICATES FROM ")
	} else if stmt.form == .Db_Table {
		emit(p, "FROM ")
	}
	emit_node(p, stmt.target)
	if stmt.source != nil {
		emit(p, " FROM ")
		if stmt.from_table {
			emit(p, "TABLE ")
		}
		emit_node(p, stmt.source)
	}
	if stmt.index != nil {
		emit(p, " INDEX ")
		emit_node(p, stmt.index)
	}
	if stmt.where_cond != nil {
		emit(p, " WHERE ")
		emit_node(p, stmt.where_cond)
	}
	if stmt.using_key.name.text != "" || stmt.using_key.dynamic_name != nil {
		emit(p, " USING KEY ")
		emit_table_key_selector(p, stmt.using_key)
	}
	if len(stmt.comparing) > 0 {
		emit(p, " COMPARING ")
		for clause, i in stmt.comparing {
			if i > 0 {
				emit(p, " ")
			}
			if clause.all_fields {
				emit(p, "ALL FIELDS")
			} else {
				emit_node(p, clause.expr)
			}
		}
	}
	emit(p, ".")
}

emit_dataset_stmt :: proc(p: ^Printer, stmt: ^Dataset_Stmt) {
	switch stmt.kind {
	case .Open:
		emit(p, "OPEN DATASET ")
	case .Read:
		emit(p, "READ DATASET ")
	case .Transfer:
		emit(p, "TRANSFER ")
		emit_node(p, stmt.source)
		emit(p, " TO ")
	case .Close:
		emit(p, "CLOSE DATASET ")
	case .Delete:
		emit(p, "DELETE DATASET ")
	case .Get:
		emit(p, "GET DATASET ")
	case .Set:
		emit(p, "SET DATASET ")
	case .Truncate:
		emit(p, "TRUNCATE DATASET ")
	}
	emit_node(p, stmt.dataset)
	if stmt.target != nil {
		emit(p, " INTO ")
		emit_node(p, stmt.target)
	}
	emit(p, ".")
}

emit_report_stmt :: proc(p: ^Printer, stmt: ^Report_Stmt) {
	switch stmt.kind {
	case .Report:
		emit(p, "REPORT ")
	case .Program:
		emit(p, "PROGRAM ")
	case .Read_Report:
		emit(p, "READ REPORT ")
	case .Insert_Report:
		emit(p, "INSERT REPORT ")
	case .Delete_Report:
		emit(p, "DELETE REPORT ")
	}
	emit_node(p, stmt.name)
	if stmt.has_message_id {
		emit(p, " MESSAGE-ID ")
		emit(p, stmt.message_id)
	}
	if stmt.source != nil {
		emit(p, " INTO ")
		emit_node(p, stmt.source)
	}
	emit(p, ".")
}

emit_textpool_stmt :: proc(p: ^Printer, stmt: ^Textpool_Stmt) {
	emit(p, "READ" if stmt.kind == .Read else "INSERT" if stmt.kind == .Insert else "DELETE")
	emit(p, " TEXTPOOL ")
	emit_node(p, stmt.program)
	if stmt.table != nil {
		emit(p, " INTO ")
		emit_node(p, stmt.table)
	}
	if stmt.language != nil {
		emit(p, " LANGUAGE ")
		emit_node(p, stmt.language)
	}
	emit(p, ".")
}

emit_exec_sql_stmt :: proc(p: ^Printer, stmt: ^Exec_Sql_Stmt) {
	emit(p, "EXEC SQL.")
	if stmt.body != "" {
		emit(p, stmt.body)
	} else {
		emit_newline(p)
	}
	emit(p, "ENDEXEC.")
}

emit_generate_stmt :: proc(p: ^Printer, stmt: ^Generate_Stmt) {
	emit(p, "GENERATE ")
	if stmt.kind == .Subroutine_Pool {
		emit(p, "SUBROUTINE POOL ")
		emit_node(p, stmt.source)
		if stmt.name != nil {
			emit(p, " NAME ")
			emit_node(p, stmt.name)
		}
		if stmt.message != nil {
			emit(p, " MESSAGE ")
			emit_node(p, stmt.message)
		}
		if stmt.line != nil {
			emit(p, " LINE ")
			emit_node(p, stmt.line)
		}
		if stmt.word != nil {
			emit(p, " WORD ")
			emit_node(p, stmt.word)
		}
		if stmt.offset != nil {
			emit(p, " OFFSET ")
			emit_node(p, stmt.offset)
		}
	} else {
		emit(p, "DYNPRO ")
		emit_node(p, stmt.program)
		if stmt.dynpro != nil {
			emit_space(p)
			emit_node(p, stmt.dynpro)
		}
	}
	emit(p, ".")
}

emit_sql_assignments :: proc(p: ^Printer, list: [dynamic]Sql_Assignment_Clause) {
	for item, i in list {
		if i > 0 {
			emit(p, ", ")
		}
		emit_node(p, item.name)
		emit(p, " = ")
		emit_node(p, item.value)
	}
}

binary_op_text :: proc(op: Binary_Op) -> string {
	switch op {
	case .Add: return "+"
	case .Subtract: return "-"
	case .Multiply: return "*"
	case .Divide: return "/"
	case .Integer_Divide: return "DIV"
	case .Modulo: return "MOD"
	case .Concatenate: return "&&"
	case .Equal: return "="
	case .Not_Equal: return "<>"
	case .Less: return "<"
	case .Less_Equal: return "<="
	case .Greater: return ">"
	case .Greater_Equal: return ">="
	case .Contains_Only: return "CO"
	case .Contains_Not_Only: return "CN"
	case .Contains_Any: return "CA"
	case .Contains_Not_Any: return "NA"
	case .Contains_String: return "CS"
	case .Contains_No_String: return "NS"
	case .Covers_Pattern: return "CP"
	case .Covers_No_Pattern: return "NP"
	case .In: return "IN"
	case .Not_In: return "NOT IN"
	case .Bit_And: return "BIT-AND"
	case .Bit_Or: return "BIT-OR"
	case .Bit_Xor: return "BIT-XOR"
	case .Bit_O: return "O"
	case .Bit_Z: return "Z"
	case .Bit_M: return "M"
	case .And: return "AND"
	case .Or: return "OR"
	case .Is: return "IS"
	case .Between: return "BETWEEN"
	case .Like: return "LIKE"
	case .Not_Like: return "NOT LIKE"
	}
	return "?"
}

unary_op_text :: proc(op: Unary_Op) -> string {
	switch op {
	case .Minus: return "-"
	case .Plus: return "+"
	case .Not: return "NOT"
	}
	return "?"
}

is_predicate_kind_text :: proc(kind: Is_Predicate_Kind) -> string {
	switch kind {
	case .Initial: return "INITIAL"
	case .Bound: return "BOUND"
	case .Assigned: return "ASSIGNED"
	case .Requested: return "REQUESTED"
	case .Supplied: return "SUPPLIED"
	case .Null: return "NULL"
	}
	return "?"
}

selector_op_text :: proc(op: Selector_Op) -> string {
	switch op {
	case .Dash: return "-"
	case .Arrow: return "->"
	case .Fat_Arrow: return "=>"
	case .Tilde: return "~"
	}
	return "?"
}

constructor_kind_text :: proc(kind: Constructor_Kind) -> string {
	switch kind {
	case .New: return "NEW"
	case .Value: return "VALUE"
	case .Conv: return "CONV"
	case .Ref: return "REF"
	case .Cast: return "CAST"
	case .Exact: return "EXACT"
	case .Corresponding: return "CORRESPONDING"
	case .Filter: return "FILTER"
	case .Reduce: return "REDUCE"
	case .Switch: return "SWITCH"
	case .Cond: return "COND"
	case .Throw: return "THROW"
	}
	return "?"
}

call_kind_text :: proc(kind: Call_Kind) -> string {
	switch kind {
	case .Direct: return ""
	case .Method: return "METHOD"
	case .Function: return "FUNCTION"
	case .Customer_Function: return "CUSTOMER-FUNCTION"
	case .Database_Procedure: return "DATABASE PROCEDURE"
	case .Transformation: return "TRANSFORMATION"
	case .Badi: return "BADI"
	case .Screen: return "SCREEN"
	case .Selection_Screen: return "SELECTION-SCREEN"
	case .Transaction: return "TRANSACTION"
	case .Dialog: return "DIALOG"
	case .Subscreen: return "SUBSCREEN"
	}
	return "?"
}

call_transformation_arg_kind_text :: proc(kind: Call_Transformation_Arg_Kind) -> string {
	switch kind {
	case .Options: return "OPTIONS"
	case .Parameters: return "PARAMETERS"
	case .Source: return "SOURCE"
	case .Result: return "RESULT"
	}
	return "?"
}

submit_operator_text :: proc(op: Submit_Option_Operator) -> string {
	switch op {
	case .None: return ""
	case .Assign: return "="
	case .Eq: return "EQ"
	case .Ne: return "NE"
	case .Bt: return "BT"
	case .Nb: return "NB"
	case .Cp: return "CP"
	case .Np: return "NP"
	case .Ge: return "GE"
	case .Gt: return "GT"
	case .Le: return "LE"
	case .Lt: return "LT"
	case .Other: return "="
	}
	return "?"
}

flow_kind_text :: proc(kind: Flow_Kind) -> string {
	switch kind {
	case .Return: return "RETURN"
	case .Continue: return "CONTINUE"
	case .Exit: return "EXIT"
	case .Stop: return "STOP"
	case .Leave_List_Processing: return "LEAVE LIST-PROCESSING"
	}
	return "?"
}

transaction_kind_text :: proc(kind: Transaction_Kind) -> string {
	switch kind {
	case .Commit: return "COMMIT"
	case .Rollback: return "ROLLBACK"
	}
	return "?"
}

runtime_kind_text :: proc(kind: Runtime_Kind) -> string {
	switch kind {
	case .Get: return "GET"
	case .Set: return "SET"
	case .Log_Point: return "LOG-POINT"
	case .Get_Badi: return "GET BADI"
	case .Export: return "EXPORT"
	case .Import: return "IMPORT"
	case .Receive: return "RECEIVE"
	}
	return "?"
}

emit_convert_time_stamp_stmt :: proc(p: ^Printer, stmt: ^Convert_Time_Stamp_Stmt) {
	switch stmt.kind {
	case .Time_Stamp_To_Date_Time:
		emit(p, "CONVERT TIME STAMP ")
		emit_node(p, stmt.time_stamp)
		emit(p, " TIME ZONE ")
		emit_node(p, stmt.time_zone)
		emit(p, " INTO DATE ")
		emit_node(p, stmt.date)
		emit(p, " TIME ")
		emit_node(p, stmt.time)
	case .Date_Time_To_Time_Stamp:
		emit(p, "CONVERT DATE ")
		emit_node(p, stmt.date)
		emit(p, " TIME ")
		emit_node(p, stmt.time)
		emit(p, " INTO TIME STAMP ")
		emit_node(p, stmt.time_stamp)
		emit(p, " TIME ZONE ")
		emit_node(p, stmt.time_zone)
	}
	emit(p, ".")
}

text_transform_kind_text :: proc(kind: Text_Transform_Kind) -> string {
	switch kind {
	case .Overlay: return "OVERLAY"
	case .Pack: return "PACK"
	case .Unpack: return "UNPACK"
	case .Convert: return "CONVERT"
	}
	return "?"
}

list_control_kind_text :: proc(kind: List_Control_Kind) -> string {
	switch kind {
	case .Skip: return "SKIP"
	case .Uline: return "ULINE"
	case .New_Line: return "NEW-LINE"
	case .New_Page: return "NEW-PAGE"
	case .Reserve: return "RESERVE"
	case .Back: return "BACK"
	case .Format: return "FORMAT"
	case .Position: return "POSITION"
	case .Hide: return "HIDE"
	}
	return "?"
}

oop_simple_kind_text :: proc(kind: Oop_Simple_Kind) -> string {
	switch kind {
	case .Methods: return "METHODS"
	case .Class_Methods: return "CLASS-METHODS"
	case .Interfaces: return "INTERFACES"
	case .Events: return "EVENTS"
	case .Class_Events: return "CLASS-EVENTS"
	case .Aliases: return "ALIASES"
	case .Class_Section: return "PUBLIC SECTION"
	case .Class_Deferred: return "CLASS DEFERRED"
	case .Interface_Deferred: return "INTERFACE DEFERRED"
	case .Class_Load: return "CLASS LOAD"
	case .Interface_Load: return "INTERFACE LOAD"
	}
	return "?"
}

oop_signature_kind_text :: proc(kind: Oop_Signature_Kind) -> string {
	switch kind {
	case .Importing: return "IMPORTING"
	case .Exporting: return "EXPORTING"
	case .Changing: return "CHANGING"
	case .Receiving: return "RECEIVING"
	case .Returning: return "RETURNING"
	case .Raising: return "RAISING"
	case .Exceptions: return "EXCEPTIONS"
	case .For: return "FOR"
	}
	return "?"
}

oop_visibility_text :: proc(visibility: Oop_Visibility) -> string {
	switch visibility {
	case .Public: return "PUBLIC"
	case .Protected: return "PROTECTED"
	case .Private: return "PRIVATE"
	case .Unspecified:
	}
	return "?"
}

select_join_kind_text :: proc(kind: Select_Join_Kind) -> string {
	switch kind {
	case .Inner: return "INNER"
	case .Left_Outer: return "LEFT OUTER"
	case .Right_Outer: return "RIGHT OUTER"
	case .Full_Outer: return "FULL OUTER"
	case .Cross: return "CROSS"
	}
	return "INNER"
}

select_set_kind_text :: proc(kind: Select_Set_Kind) -> string {
	switch kind {
	case .Union: return "UNION"
	case .Intersect: return "INTERSECT"
	case .Except: return "EXCEPT"
	}
	return "UNION"
}

sql_call_modifier_text :: proc(modifier: Sql_Call_Modifier) -> string {
	switch modifier {
	case .Distinct: return "DISTINCT"
	case .All: return "ALL"
	case .None:
	}
	return ""
}
