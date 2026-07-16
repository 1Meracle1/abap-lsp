package abap_frontend_ast

Visitor :: struct {
	visit: proc(visitor: ^Visitor, node: ^Node) -> ^Visitor,
	data:  rawptr,
}

inspect :: proc(node: ^Node, f: proc(^Node) -> bool) {
	v := &Visitor {
		visit = proc(v: ^Visitor, node: ^Node) -> ^Visitor {
			f := (proc(^Node) -> bool)(v.data)
			if f(node) {
				return v
			}
			return nil
		},
		data = rawptr(f),
	}
	walk(v, node)
}

walk :: proc(v: ^Visitor, node: ^Node) {
	if node == nil {
		return
	}
	next := v->visit(node)
	if next == nil {
		return
	}
	switch n in node.derived {
	case ^File:
		walk_stmt_list(next, n.stmts)
	case ^Bad_Expr:
	case ^Char_String_Template_Expr:
		walk_expr_list(next, n.parts)
	case ^Template_Literal_Expr:
	case ^Template_Interpolation_Expr:
		walk(next, n.expr)
		walk_expr_list(next, n.format_specs)
	case ^Template_Expr:
		walk(next, n.expr)
	case ^Template_Format_Spec_Expr:
		walk(next, n.value)
	case ^Binary_Expr:
		walk(next, n.left)
		walk(next, n.right)
	case ^Unary_Expr:
		walk(next, n.expr)
	case ^Paren_Expr:
		walk(next, n.expr)
	case ^Ident_Expr:
	case ^Literal_Expr:
	case ^Macro_Arg_Ref_Expr:
	case ^Type_Ref_Expr:
	case ^Dynamic_Call_Method_Target_Expr:
		walk(next, n.base)
		walk(next, n.method)
	case ^Ole_Call_Method_Target_Expr:
		walk(next, n.object)
		walk(next, n.member)
		walk(next, n.result)
	case ^Host_Expr:
		walk(next, n.value)
	case ^Table_Expr:
		walk(next, n.table)
		walk_expr_list(next, n.selectors)
	case ^Selector_Expr:
		walk(next, n.base)
		walk(next, n.field)
	case ^Interface_Qualified_Selector_Expr:
		walk(next, n.receiver)
		walk(next, n.interface)
		walk(next, n.member)
	case ^Substring_Expr:
		walk(next, n.base)
		walk(next, n.offset)
		walk(next, n.length)
	case ^Call_Expr:
		walk(next, n.callee)
		walk(next, n.args)
	case ^Call_Arg_List_Expr:
		walk_expr_list(next, n.args)
	case ^Call_Arg_Section_Expr:
		walk_expr_list(next, n.args)
	case ^Call_Named_Arg_Expr:
		walk(next, n.value)
	case ^Call_Positional_Arg_Expr:
		walk(next, n.value)
	case ^Sql_Column_Expr:
	case ^Sql_Star_Expr:
	case ^Sql_Call_Expr:
		walk_expr_list(next, n.args)
	case ^Constructor_Expr:
		walk(next, n.type_ref)
		walk_expr_list(next, n.args)
	case ^Is_Predicate_Expr:
		walk(next, n.subject)
	case ^Instance_Of_Predicate_Expr:
		walk(next, n.subject)
		walk(next, n.type_ref)
	case ^Between_Expr:
		walk(next, n.subject)
		walk(next, n.low)
		walk(next, n.high)
	case ^Sql_Case_When_Expr:
		walk(next, n.condition)
		walk(next, n.result)
	case ^Sql_Case_Expr:
		walk(next, n.operand)
		walk_expr_list(next, n.whens)
		walk(next, n.else_expr)
	case ^Let_Expr:
		walk_expr_list(next, n.bindings)
		walk_expr_list(next, n.body)
	case ^Constructor_Let_Binding_Expr:
		walk(next, n.value)
	case ^Constructor_When_Clause_Expr:
		walk(next, n.condition)
		walk(next, n.result)
	case ^Constructor_Else_Clause_Expr:
		walk(next, n.result)
	case ^Constructor_For_Clause_Expr:
		walk(next, n.init)
		walk(next, n.then_expr)
		walk(next, n.condition)
		walk(next, n.source)
		walk(next, n.group_by)
		walk(next, n.where_clause)
		walk_expr_list(next, n.body)
	case ^Constructor_Where_Clause_Expr:
		walk(next, n.condition)
	case ^Constructor_Filter_Except_In_Clause_Expr:
		walk(next, n.source)
		walk(next, n.where_clause)
	case ^Constructor_Filter_Using_Key_Clause_Expr:
		walk(next, n.using_key.dynamic_name)
	case ^Constructor_Init_Clause_Expr:
		walk_expr_list(next, n.assignments)
	case ^Constructor_Next_Clause_Expr:
		walk_expr_list(next, n.assignments)
	case ^Constructor_Named_Assignment_Expr:
		walk(next, n.value)
	case ^Constructor_Base_Clause_Expr:
		walk(next, n.value)
	case ^Constructor_Lines_Of_Clause_Expr:
		walk(next, n.source)
		walk(next, n.from)
		walk(next, n.to)
	case ^Constructor_Optional_Expr:
		walk(next, n.value)
	case ^Constructor_Corresponding_Mapping_Clause_Expr:
		walk_expr_list(next, n.assignments)
	case ^Constructor_Corresponding_Mapping_Assignment_Expr:
		walk(next, n.source)
		walk(next, n.default_value)
		walk(next, n.mapping)
		walk(next, n.except)
	case ^Constructor_Corresponding_Except_Clause_Expr:
		walk_expr_list(next, n.names)
	case ^Data_Inline_Name_Expr:
	case ^Field_Symbol_Inline_Name_Expr:
	case ^Data_Chained_Decl:
		for branch in n.decls {
			walk_data_decl_clause(next, branch)
		}
	case ^Data_Inline_Decl:
		walk(next, n.expr)
	case ^Types_Decl:
		for clause in n.types {
			walk_paren_length_clause(next, clause.paren_length)
			walk_length_clauses(next, clause.length_clauses)
			walk_data_type_clause(next, clause.type_clause)
			walk(next, clause.occurs)
			walk(next, clause.include_ref)
		}
	case ^Constants_Decl:
		for clause in n.constants {
			walk_paren_length_clause(next, clause.paren_length)
			walk_length_clauses(next, clause.length_clauses)
			walk_data_type_clause(next, clause.type_clause)
			walk_value_clause(next, clause.value_clause)
			walk(next, clause.occurs)
			walk(next, clause.include_ref)
		}
	case ^Field_Symbols_Decl:
		for clause in n.field_symbols {
			walk_data_type_clause(next, clause.type_clause)
		}
	case ^Statics_Decl:
		for clause in n.statics {
			walk_paren_length_clause(next, clause.paren_length)
			walk_length_clauses(next, clause.length_clauses)
			walk_data_type_clause(next, clause.type_clause)
			walk_value_clause(next, clause.value_clause)
			walk(next, clause.occurs)
			walk(next, clause.include_ref)
		}
	case ^Tables_Decl:
	case ^Ranges_Decl:
		for clause in n.ranges {
			walk(next, clause.for_expr)
		}
	case ^Parameters_Decl:
		for clause in n.parameters {
			walk_paren_length_clause(next, clause.paren_length)
			walk_length_clauses(next, clause.length_clauses)
			walk_data_type_clause(next, clause.type_clause)
			walk(next, clause.default_expr)
			walk(next, clause.memory_id)
			walk(next, clause.matchcode_object)
			walk(next, clause.visible_length)
		}
	case ^Select_Options_Decl:
		for clause in n.options {
			walk(next, clause.for_expr)
			walk(next, clause.default_expr)
			walk(next, clause.to_expr)
			walk(next, clause.memory_id)
			walk(next, clause.matchcode_object)
			walk(next, clause.visible_length)
		}
	case ^Controls_Decl:
		for clause in n.controls {
			walk_data_type_clause(next, clause.type_clause)
			walk_using_screen_clause(next, clause.using_screen)
		}
	case ^Class_Data_Decl:
		for clause in n.decls {
			walk_data_decl_clause(next, clause)
		}
	case ^Type_Pools_Decl:
	case ^Function_Pool_Decl:
	case ^Include_Stmt:
	case ^Assign_Stmt:
		walk(next, n.lhs)
		walk_expr_list(next, n.chain_lhs)
		walk(next, n.rhs)
	case ^Downcast_Assign_Stmt:
		walk(next, n.lhs)
		walk(next, n.rhs)
	case ^Expr_Stmt:
		walk(next, n.expr)
	case ^Clear_Stmt:
		for clause in n.operands {
			walk(next, clause.target)
			walk(next, clause.value)
		}
	case ^Refresh_Stmt:
		for clause in n.operands {
			walk(next, clause.target)
		}
	case ^Free_Stmt:
		for clause in n.operands {
			walk(next, clause.target)
		}
		walk(next, n.memory_id)
	case ^Unassign_Stmt:
		for clause in n.operands {
			walk(next, clause.target)
		}
	case ^Move_Stmt:
		for clause in n.entries {
			walk(next, clause.source)
			walk(next, clause.target)
		}
	case ^Move_Corresponding_Stmt:
		for clause in n.entries {
			walk(next, clause.source)
			walk(next, clause.target)
		}
	case ^Add_Stmt:
		for clause in n.entries {
			walk(next, clause.source)
			walk(next, clause.target)
			walk(next, clause.result)
		}
	case ^Subtract_Stmt:
		for clause in n.entries {
			walk(next, clause.source)
			walk(next, clause.target)
			walk(next, clause.result)
		}
	case ^Multiply_Stmt:
		for clause in n.entries {
			walk(next, clause.target)
			walk(next, clause.source)
			walk(next, clause.result)
		}
	case ^Divide_Stmt:
		for clause in n.entries {
			walk(next, clause.source)
			walk(next, clause.target)
			walk(next, clause.result)
		}
	case ^Compute_Stmt:
		for clause in n.entries {
			walk(next, clause.target)
			walk(next, clause.source)
		}
	case ^Concatenate_Stmt:
		for clause in n.entries {
			walk_expr_list(next, clause.sources)
			walk(next, clause.target)
			walk(next, clause.separator)
		}
	case ^Split_Stmt:
		for clause in n.entries {
			walk(next, clause.source)
			walk(next, clause.separator)
			walk_expr_list(next, clause.targets)
		}
	case ^Condense_Stmt:
		walk(next, n.target)
	case ^Replace_Stmt:
		walk(next, n.pattern)
		walk(next, n.target)
		walk(next, n.replacement)
		walk(next, n.section_offset)
		walk(next, n.section_length)
	case ^Translate_Stmt:
		walk(next, n.target)
		walk(next, n.operand)
	case ^Shift_Stmt:
		walk(next, n.target)
		walk(next, n.places)
		walk(next, n.up_to)
		walk(next, n.delete_pattern)
	case ^Find_Stmt:
		walk(next, n.pattern)
		walk(next, n.target)
		walk(next, n.section_offset)
		walk(next, n.section_length)
		walk(next, n.match_offset)
		walk(next, n.match_length)
		walk(next, n.match_line)
		walk(next, n.match_count)
		walk(next, n.results)
		walk_expr_list(next, n.submatches)
	case ^Search_Stmt:
		walk(next, n.target)
		walk(next, n.pattern)
		walk(next, n.starting_at)
		walk(next, n.ending_at)
	case ^Perform_Stmt:
		walk(next, n.form)
		walk(next, n.program)
		walk_expr_list(next, n.tables)
		walk_expr_list(next, n.using_args)
		walk_expr_list(next, n.changing)
	case ^Call_Stmt:
		walk(next, n.call)
		walk(next, n.target)
		walk(next, n.function_destination)
		walk(next, n.function_task)
		walk(next, n.function_end_task_handler)
		walk(next, n.function_parameter_table)
		walk(next, n.function_exception_table)
		for arg in n.named_args {
			walk(next, arg.value)
			walk(next, arg.message)
		}
		walk_expr_list(next, n.transaction_operands)
		for arg in n.transformation_args {
			walk(next, arg.value)
		}
	case ^Submit_Stmt:
		walk(next, n.target)
		for clause in n.options {
			walk(next, clause.value)
			walk(next, clause.high_value)
			walk(next, clause.sign_value)
		}
	case ^Message_Stmt:
		walk_message_head(next, n.head)
		walk_expr_list(next, n.with_args)
		walk(next, n.into)
		walk(next, n.display_like)
		walk(next, n.raising)
	case ^Write_Stmt:
		for clause in n.operands {
			walk(next, clause.value)
			walk(next, clause.position)
			walk(next, clause.length)
		}
	case ^Write_To_Stmt:
		for entry in n.entries {
			walk(next, entry.source)
			walk(next, entry.target)
		}
	case ^Assert_Stmt:
		walk(next, n.condition)
	case ^Check_Stmt:
		walk(next, n.condition)
	case ^Flow_Stmt:
	case ^Transaction_Stmt:
	case ^Describe_Stmt:
		for clause in n.entries {
			walk(next, clause.source)
			walk(next, clause.target)
		}
	case ^Runtime_Stmt:
		walk(next, n.id)
		walk(next, n.field)
		walk(next, n.target)
		walk(next, n.value)
		walk(next, n.line)
		walk(next, n.offset)
		walk_expr_list(next, n.excluding)
		walk_expr_list(next, n.operands)
	case ^Set_Handler_Stmt:
		walk_expr_list(next, n.handlers)
		walk(next, n.sender)
		walk(next, n.activation)
	case ^Import_Stmt:
		walk_data_cluster_medium(next, n.medium)
		for clause in n.parameters {
			walk(next, clause.value)
		}
	case ^Export_Stmt:
		walk_data_cluster_medium(next, n.medium)
		for clause in n.parameters {
			walk(next, clause.value)
		}
	case ^Bit_Stmt:
		walk(next, n.position)
		walk(next, n.source)
		walk(next, n.target)
		walk(next, n.value)
	case ^Locale_Stmt:
		walk(next, n.language)
		walk(next, n.country)
		walk(next, n.modifier)
	case ^Set_Cursor_Stmt:
		walk(next, n.field)
		walk(next, n.offset)
		walk(next, n.line)
		walk(next, n.column)
	case ^Receive_Results_Stmt:
		walk(next, n.target)
		for arg in n.named_args {
			walk(next, arg.value)
			walk(next, arg.message)
		}
	case ^Raise_Stmt:
		walk(next, n.target)
		for arg in n.named_args {
			walk(next, arg.value)
		}
	case ^Authority_Check_Stmt:
		walk_expr_list(next, n.operands)
		walk(next, n.object)
		for clause in n.ids {
			walk(next, clause.id)
			walk(next, clause.field)
		}
	case ^Field_Groups_Stmt:
		walk_expr_list(next, n.groups)
	case ^Insert_Dummy_Stmt:
		walk(next, n.target)
	case ^Field_Stmt:
		walk_expr_list(next, n.operands)
		walk(next, n.module)
		walk(next, n.condition)
	case ^Assign_Field_Stmt:
		walk(next, n.source)
		walk(next, n.component)
		walk(next, n.structure)
		walk(next, n.target)
		walk(next, n.casting_type)
		walk(next, n.casting_decimals)
	case ^Create_Object_Stmt:
		walk(next, n.target)
		walk(next, n.type_ref)
		walk_data_type_clause(next, n.type_clause)
		walk(next, n.type_dynamic_expr)
		walk_expr_list(next, n.operands)
	case ^Create_Data_Stmt:
		walk(next, n.target)
		walk(next, n.type_ref)
		walk_data_type_clause(next, n.type_clause)
		walk(next, n.type_dynamic_expr)
		walk(next, n.type_handle)
		walk_expr_list(next, n.operands)
	case ^Text_Transform_Stmt:
		walk(next, n.source)
		walk(next, n.target)
		walk(next, n.only)
	case ^Wait_Stmt:
		walk(next, n.condition)
		walk(next, n.duration)
	case ^Convert_Time_Stamp_Stmt:
		walk(next, n.time_stamp)
		walk(next, n.time_zone)
		walk(next, n.date)
		walk(next, n.time)
		walk(next, n.daylight_saving_time)
	case ^List_Control_Stmt:
		walk_expr_list(next, n.operands)
	case ^Line_Stmt:
		walk(next, n.line)
		walk(next, n.index)
		walk(next, n.into)
		for clause in n.fields {
			walk(next, clause.field)
			walk(next, clause.target)
		}
	case ^Macro_Def_Stmt:
		walk_stmt_list(next, n.body)
	case ^Macro_Call_Stmt:
		walk_expr_list(next, n.args)
	case ^Selection_Screen_Stmt:
	case ^Oop_Simple_Stmt:
		for alias in n.aliases {
			walk(next, alias.target)
		}
		for member in n.members {
			walk(next, member.event_handler.source_type)
			for clause in member.signatures {
				walk_expr_list(next, clause.values)
				for param in clause.parameters {
					walk_data_type_clause(next, param.type_clause)
					walk(next, param.default_expr)
				}
			}
		}
	case ^Oop_Load_Stmt:
	case ^If_Stmt:
		walk(next, n.condition)
		walk_stmt_list(next, n.body)
		for clause in n.elseif_clauses {
			walk_elseif_clause(next, clause)
		}
		walk_else_clause(next, n.else_clause)
	case ^Case_Stmt:
		walk(next, n.expr)
		walk_stmt_list(next, n.recovery)
		for clause in n.whens {
			walk_when_clause(next, clause)
		}
	case ^While_Stmt:
		walk(next, n.condition)
		walk_stmt_list(next, n.body)
	case ^Do_Stmt:
		walk(next, n.count)
		walk_stmt_list(next, n.body)
	case ^Loop_Stmt:
		walk(next, n.source)
		walk(next, n.target)
		walk(next, n.target_casting_type)
		walk(next, n.from)
		walk(next, n.to)
		walk(next, n.where_cond)
		walk(next, n.using_key.dynamic_name)
		walk(next, n.group_by)
		walk(next, n.group_target)
		walk_stmt_list(next, n.body)
	case ^At_Stmt:
		walk(next, n.expr)
		walk_stmt_list(next, n.body)
	case ^Try_Stmt:
		walk_stmt_list(next, n.body)
		for clause in n.catches {
			walk_catch_clause(next, clause)
		}
		walk_cleanup_clause(next, n.cleanup)
	case ^Class_Decl:
		walk_stmt_list(next, n.body)
	case ^Interface_Decl:
		walk_stmt_list(next, n.body)
	case ^Method_Decl:
		walk_stmt_list(next, n.body)
	case ^Form_Decl:
		for param in n.form_parameters {
			walk_data_type_clause(next, param.type_clause)
		}
		walk_stmt_list(next, n.body)
	case ^Function_Decl:
		for param in n.function_parameters {
			walk_data_type_clause(next, param.type_clause)
			walk(next, param.default_expr)
		}
		for exception in n.exceptions {
			walk(next, exception.code_expr)
		}
		walk_stmt_list(next, n.body)
	case ^Module_Decl:
		walk_stmt_list(next, n.body)
	case ^Event_Block_Stmt:
		walk_stmt_list(next, n.body)
	case ^Enhancement_Stmt:
		walk_stmt_list(next, n.body)
	case ^Enhancement_Section_Stmt:
		walk_stmt_list(next, n.body)
	case ^Test_Seam_Stmt:
		walk_stmt_list(next, n.body)
	case ^Test_Injection_Stmt:
		walk_stmt_list(next, n.body)
	case ^Select_Stmt:
		walk_select_with(next, n.with)
		walk_select_query(next, n.query)
		walk_stmt_list(next, n.body)
	case ^Open_Cursor_Stmt:
		walk(next, n.handle)
		walk_select_query(next, n.query)
	case ^Fetch_Stmt:
		walk(next, n.handle)
		walk_select_result(next, n.result)
		walk(next, n.package_size)
	case ^Close_Cursor_Stmt:
		walk(next, n.handle)
	case ^Insert_Stmt:
		walk(next, n.source)
		walk(next, n.target)
		walk(next, n.index)
		walk(next, n.assigning)
		walk(next, n.reference_into)
		walk_sql_assignments(next, n.assignments)
	case ^Append_Stmt:
		walk(next, n.source)
		walk(next, n.target)
		walk(next, n.assigning)
		walk(next, n.reference_into)
	case ^Modify_Stmt:
		walk(next, n.target)
		walk(next, n.source)
		walk(next, n.index)
		walk(next, n.where_cond)
	case ^Sort_Stmt:
		walk(next, n.target)
		for field in n.fields {
			walk(next, field.expr)
		}
	case ^Update_Stmt:
		walk(next, n.target)
		walk(next, n.source)
		walk_sql_assignments(next, n.assignments)
		walk(next, n.where_cond)
	case ^Delete_Stmt:
		walk(next, n.target)
		walk(next, n.source)
		walk(next, n.index)
		walk(next, n.where_cond)
		walk(next, n.using_key.dynamic_name)
		for clause in n.comparing {
			if !clause.all_fields {
				walk(next, clause.expr)
			}
		}
	case ^Read_Table_Stmt:
		for clause in n.entries {
			walk(next, clause.table)
			walk(next, clause.into)
			walk(next, clause.assigning)
			walk(next, clause.reference_into)
			for key_value in clause.key_values {
				walk(next, key_value.dynamic_name)
				walk(next, key_value.value)
			}
			walk(next, clause.index)
			walk(next, clause.using_key.dynamic_name)
			walk_expr_list(next, clause.comparing)
		}
	case ^Dataset_Stmt:
		walk(next, n.dataset)
		walk(next, n.source)
		walk(next, n.target)
		walk(next, n.code_page)
		walk(next, n.file_type)
		walk(next, n.filter)
		walk(next, n.replacement)
		walk(next, n.position)
		walk(next, n.message)
		walk(next, n.maximum_length)
		walk(next, n.actual_length)
		walk(next, n.length)
		walk(next, n.attributes)
	case ^Report_Stmt:
		walk(next, n.name)
		walk(next, n.source)
		walk(next, n.line_size)
		walk(next, n.line_count)
	case ^Textpool_Stmt:
		walk(next, n.program)
		walk(next, n.table)
		walk(next, n.language)
	case ^Exec_Sql_Stmt:
	case ^Generate_Stmt:
		walk(next, n.source)
		walk(next, n.name)
		walk(next, n.program)
		walk(next, n.dynpro)
		walk(next, n.message)
		walk(next, n.line)
		walk(next, n.word)
		walk(next, n.offset)
	case ^Invalid_Stmt:
	}

	next->visit(nil)
}

walk_expr_list :: proc(v: ^Visitor, list: [dynamic]^Expr) {
	for x in list {
		walk(v, x)
	}
}

walk_data_cluster_medium :: proc(v: ^Visitor, medium: Data_Cluster_Medium_Clause) {
	walk(v, medium.object)
	walk(v, medium.work_area)
	walk(v, medium.client)
	walk(v, medium.id)
}

walk_stmt_list :: proc(v: ^Visitor, list: [dynamic]^Stmt) {
	for x in list {
		walk(v, x)
	}
}

walk_elseif_clause :: proc(v: ^Visitor, clause: ^Elseif_Clause) {
	if clause != nil {
		walk(v, clause.condition)
		walk_stmt_list(v, clause.body)
	}
}

walk_else_clause :: proc(v: ^Visitor, clause: ^Else_Clause) {
	if clause != nil {
		walk_stmt_list(v, clause.body)
	}
}

walk_when_clause :: proc(v: ^Visitor, clause: ^When_Clause) {
	if clause != nil {
		walk_expr_list(v, clause.operands)
		walk_expr_list(v, clause.type_operands)
		walk(v, clause.into)
		walk_stmt_list(v, clause.body)
	}
}

walk_catch_clause :: proc(v: ^Visitor, clause: ^Catch_Clause) {
	if clause != nil {
		walk_expr_list(v, clause.exceptions)
		walk(v, clause.into)
		walk_stmt_list(v, clause.body)
	}
}

walk_cleanup_clause :: proc(v: ^Visitor, clause: ^Cleanup_Clause) {
	if clause != nil {
		walk_stmt_list(v, clause.body)
	}
}

walk_data_type_clause :: proc(v: ^Visitor, clause: ^Data_Type_Clause) {
	if clause != nil {
		walk(v, clause.type_ref)
		walk(v, clause.initial_size)
	}
}

walk_data_decl_clause :: proc(v: ^Visitor, clause: Data_Decl_Clause) {
	walk_paren_length_clause(v, clause.paren_length)
	walk_length_clauses(v, clause.length_clauses)
	walk_data_type_clause(v, clause.type_clause)
	walk_value_clause(v, clause.value_clause)
	walk(v, clause.occurs)
	walk(v, clause.include_ref)
}

walk_paren_length_clause :: proc(v: ^Visitor, clause: ^Paren_Length_Clause) {
	if clause != nil {
		walk(v, clause.expr)
	}
}

walk_length_clauses :: proc(v: ^Visitor, list: [dynamic]Length_Clause) {
	for clause in list {
		walk(v, clause.expr)
	}
}

walk_value_clause :: proc(v: ^Visitor, clause: ^Value_Clause) {
	if clause != nil {
		walk(v, clause.expr)
	}
}

walk_using_screen_clause :: proc(v: ^Visitor, clause: ^Using_Screen_Clause) {
	if clause != nil {
		walk(v, clause.screen)
	}
}

walk_select_query :: proc(v: ^Visitor, clause: Select_Query_Clause) {
	if len(clause.projection_clauses) > 0 {
		for projection in clause.projection_clauses {
			walk(v, projection.value)
		}
	} else {
		walk_expr_list(v, clause.projections)
	}
	if clause.source_clause != nil {
		walk_select_source(v, clause.source_clause)
	} else {
		walk(v, clause.source)
	}
	walk_select_result(v, clause.result)
	walk(v, clause.where_cond)
	walk(v, clause.for_all_entries)
	for group_expr in clause.group_by {
		walk(v, group_expr.value)
	}
	walk(v, clause.package_size)
	walk(v, clause.up_to_rows)
	for set_op in clause.set_ops {
		walk_select_query(v, set_op.query)
	}
}

walk_select_with :: proc(v: ^Visitor, clause: ^Select_With_Clause) {
	if clause == nil {
		return
	}
	for entry in clause.entries {
		walk_select_query(v, entry.query)
	}
}

walk_select_source :: proc(v: ^Visitor, clause: ^Select_Source_Clause) {
	if clause == nil {
		return
	}
	walk(v, clause.source)
	for join in clause.joins {
		walk(v, join.source)
		walk(v, join.on)
	}
}

walk_select_result :: proc(v: ^Visitor, clause: ^Select_Result_Clause) {
	if clause != nil {
		walk(v, clause.target)
	}
}

walk_sql_assignments :: proc(v: ^Visitor, list: [dynamic]Sql_Assignment_Clause) {
	for clause in list {
		walk(v, clause.name)
		walk(v, clause.value)
	}
}

walk_message_head :: proc(v: ^Visitor, clause: ^Message_Head_Clause) {
	if clause != nil {
		walk(v, clause.code)
		walk(v, clause.id)
		walk(v, clause.msg_type)
		walk(v, clause.number)
	}
}
