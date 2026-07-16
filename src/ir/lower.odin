package abap_frontend_ir

import "src:ast"
import semantic "src:semantic"
import "src:tokenizer"

import "core:mem"
import "core:strings"

Lower_Diagnostic :: struct {
	message: string,
	source:  Source_Loc,
}

Lower_Result :: struct {
	module:      Module,
	diagnostics: [dynamic]Lower_Diagnostic,
	ok:          bool,
}

Lower_Context :: struct {
	module:     ^Module,
	project:    ^semantic.Project,
	checker:    ^semantic.Checker,
	file:       ^semantic.Project_File,
	query:      semantic.Semantic_Query,
	decl_query: semantic.Semantic_Decl_Query,
	ref_query:  semantic.Semantic_Ref_Query,
	fact_query: semantic.Semantic_Fact_Query,
	builder:    ^Builder,
	control_targets: [dynamic]Lower_Control_Target,
	exception_targets: [dynamic]Lower_Exception_Target,
}

Lower_Control_Target_Kind :: enum {
	Loop,
	Select,
}

Lower_Control_Target :: struct {
	kind:           Lower_Control_Target_Kind,
	continue_block: Block_Id,
	continue_arg:   Value_Id,
	exit_block:     Block_Id,
}

Lower_Exception_Target :: struct {
	dispatch_block: Block_Id,
}

Lower_File_Event :: struct {
	kind:        ast.Event_Block_Kind,
	addition:    ast.Event_Block_Addition,
	function_id: Function_Id,
}

Lower_File_Callables :: struct {
	load_globals: Function_Id,
	implicit_start: Function_Id,
	events:      [dynamic]Lower_File_Event,
}

lower_result_destroy :: proc(result: ^Lower_Result) {
	module_destroy(&result.module)
	delete(result.diagnostics)
	result^ = {}
}

lower_project :: proc(
	project: ^semantic.Project,
	checker: ^semantic.Checker,
	allocator: mem.Allocator = context.allocator,
) -> Lower_Result {
	result := Lower_Result {
		module = module_make(allocator),
		diagnostics = make([dynamic]Lower_Diagnostic, 0, 4, allocator),
		ok = true,
	}
	query := semantic.semantic_query(project, checker)
	for file in semantic.semantic_query_files(query) {
		lower_file(&result.module, project, checker, file)
	}
	lower_resolve_canonical_call_targets(&result.module)
	return result
}

lower_file :: proc(
	module: ^Module,
	project: ^semantic.Project,
	checker: ^semantic.Checker,
	file: ^semantic.Project_File,
) -> Function_Id {
	if file.root == nil {
		return INVALID_FUNCTION_ID
	}
	source := source_loc_from_node(file, &file.root.node)
	entry_name := lower_file_entry_name(file, project.allocator)
	entry_builder := builder_begin_function(module, entry_name, source = source, role = .Report_Entry)
	entry_id := entry_builder.function_id
	callables := Lower_File_Callables {
		load_globals = INVALID_FUNCTION_ID,
		implicit_start = INVALID_FUNCTION_ID,
		events = make([dynamic]Lower_File_Event, 0, 4, context.temp_allocator),
	}
	defer delete(callables.events)

	if lower_file_has_global_initializers(file.root.stmts[:]) {
		load_name := lower_generated_file_callable_name(file, "load_globals", project.allocator)
		load_body := lower_file_global_initializer_stmts(file.root.stmts[:], context.temp_allocator)
		defer delete(load_body)
		callables.load_globals = lower_callable(module, project, checker, file, nil, load_name, load_body[:], source, .Load_Of_Program)
	}
	if lower_file_has_implicit_start_stmts(file.root.stmts[:]) {
		start_name := lower_generated_file_callable_name(file, "start_of_selection", project.allocator)
		start_body := lower_file_implicit_start_stmts(file.root.stmts[:], context.temp_allocator)
		defer delete(start_body)
		callables.implicit_start = lower_callable(module, project, checker, file, nil, start_name, start_body[:], source, .Event)
	}

	lower_collect_nested_callables(module, project, checker, file, file.root.stmts[:], &callables)
	lower_resolve_canonical_call_targets(module)
	lower_emit_report_entry(&entry_builder, &callables, source)
	module_add_entry(module, entry_id)
	return entry_id
}

lower_resolve_canonical_call_targets :: proc(module: ^Module) {
	for &function in module.functions {
		for &block in function.blocks {
			for instruction in block.instructions {
				op := &function.instructions[int(instruction)]
				if (op.opcode != .Intrinsic && op.opcode != .Invoke) ||
				   op.intrinsic == INVALID_INTRINSIC_ID ||
				   int(op.intrinsic) >= len(module.intrinsics) {
					continue
				}
				intrinsic := &module.intrinsics[int(op.intrinsic)]
				if intrinsic.family != .Call {
					continue
				}
				payload, payload_ok := intrinsic.payload.(Intrinsic_Call_Payload)
				if !payload_ok || payload.has_call_function_target || payload.callee_name == "" {
					continue
				}
				target, target_ok := lower_find_canonical_call_target_for_op(module, op)
				if !target_ok {
					target, target_ok = lower_find_canonical_call_target(module, payload)
				}
				if !target_ok {
					continue
				}
				payload.call_function_target = target
				payload.has_call_function_target = true
				intrinsic.payload = payload
			}
		}
	}
}

lower_find_canonical_call_target_for_op :: proc "contextless" (
	module: ^Module,
	op: ^Op,
) -> (
	Function_Id,
	bool,
) {
	if op.intrinsic == INVALID_INTRINSIC_ID || int(op.intrinsic) >= len(module.intrinsics) {
		return INVALID_FUNCTION_ID, false
	}
	payload, payload_ok := module.intrinsics[int(op.intrinsic)].payload.(Intrinsic_Call_Payload)
	if !payload_ok {
		return INVALID_FUNCTION_ID, false
	}
	if payload.has_call_function_target {
		target := payload.call_function_target
		if target != INVALID_FUNCTION_ID && int(target) < len(module.functions) {
			return target, true
		}
	}
	return lower_find_canonical_call_target(module, payload)
}

lower_find_canonical_call_target :: proc "contextless" (
	module: ^Module,
	payload: Intrinsic_Call_Payload,
) -> (
	Function_Id,
	bool,
) {
	if payload.callee_name == "" {
		return INVALID_FUNCTION_ID, false
	}
	for function, i in module.functions {
		if function.name != payload.callee_name {
			continue
		}
		#partial switch payload.call_kind {
		case .Method:
			if function.role != .Method && function.role != .Constructor && function.role != .Class_Constructor {
				continue
			}
		case .Form:
			if function.role != .Form {
				continue
			}
		case .Function_Module:
			if function.role != .Function_Module {
				continue
			}
		case:
		}
		return Function_Id(i), true
	}
	return INVALID_FUNCTION_ID, false
}

lower_collect_nested_callables :: proc(
	module: ^Module,
	project: ^semantic.Project,
	checker: ^semantic.Checker,
	file: ^semantic.Project_File,
	stmts: []^ast.Stmt,
	callables: ^Lower_File_Callables = nil,
) {
	query := semantic.semantic_query(project, checker, file)
	decl_query := semantic.semantic_query_decls(query)
	ref_query := semantic.semantic_query_refs(query)
	for stmt in stmts {
		if stmt == nil {
			continue
		}
		#partial switch n in stmt.derived_stmt {
		case ^ast.Method_Decl:
			entity := lower_method_entity(decl_query, ref_query, n)
			name := lower_callable_entity_name(entity, lower_method_name(n, module.allocator), module.allocator)
			lower_callable(module, project, checker, file, entity, name, n.body[:], source_loc_from_node(file, &n.node.stmt_base), .Method)
		case ^ast.Form_Decl:
			entity := lower_entity_at_range(decl_query, n.name.range, .Form)
			lower_callable(module, project, checker, file, entity, n.name.text, n.body[:], source_loc_from_node(file, &n.node.stmt_base), .Form)
		case ^ast.Function_Decl:
			entity := lower_entity_at_range(decl_query, n.name.range, .Module)
			lower_callable(module, project, checker, file, entity, n.name.text, n.body[:], source_loc_from_node(file, &n.node.stmt_base), .Function_Module)
		case ^ast.Module_Decl:
			entity := lower_entity_at_range(decl_query, n.name.range, .Module)
			lower_callable(module, project, checker, file, entity, n.name.text, n.body[:], source_loc_from_node(file, &n.node.stmt_base), .Module)
		case ^ast.Event_Block_Stmt:
			name := event_block_name(n.kind)
			id := lower_callable(module, project, checker, file, nil, name, n.body[:], source_loc_from_node(file, &n.node.stmt_base), .Event)
			if callables != nil {
				append(&callables.events, Lower_File_Event{kind = n.kind, addition = n.addition, function_id = id})
			}
		case ^ast.Class_Decl:
			lower_collect_nested_callables(module, project, checker, file, n.body[:], callables)
		case ^ast.Interface_Decl:
			lower_collect_nested_callables(module, project, checker, file, n.body[:], callables)
		case ^ast.Enhancement_Stmt:
			lower_collect_nested_callables(module, project, checker, file, n.body[:], callables)
		case ^ast.Enhancement_Section_Stmt:
			lower_collect_nested_callables(module, project, checker, file, n.body[:], callables)
		case ^ast.Test_Seam_Stmt:
			lower_collect_nested_callables(module, project, checker, file, n.body[:], callables)
		case ^ast.Test_Injection_Stmt:
			lower_collect_nested_callables(module, project, checker, file, n.body[:], callables)
		}
	}
}

lower_emit_report_entry :: proc(builder: ^Builder, callables: ^Lower_File_Callables, source: Source_Loc) {
	if callables.load_globals != INVALID_FUNCTION_ID {
		lower_emit_core_call_with_unhandled(builder, callables.load_globals, source)
	}
	lower_emit_event_calls(builder, callables.events[:], .Load_Of_Program, source)
	lower_emit_event_calls(builder, callables.events[:], .Initialization, source)
	if callables.implicit_start != INVALID_FUNCTION_ID {
		lower_emit_core_call_with_unhandled(builder, callables.implicit_start, source)
	}
	lower_emit_event_calls(builder, callables.events[:], .Start_Of_Selection, source)
	lower_emit_event_calls(builder, callables.events[:], .End_Of_Selection, source)
	builder_set_return_world(builder, source)
}

lower_emit_event_calls :: proc(
	builder: ^Builder,
	events: []Lower_File_Event,
	kind: ast.Event_Block_Kind,
	source: Source_Loc,
) {
	for event in events {
		if event.kind == kind && event.function_id != INVALID_FUNCTION_ID {
			lower_emit_core_call_with_unhandled(builder, event.function_id, source)
		}
	}
}

lower_emit_core_call_with_unhandled :: proc(builder: ^Builder, target: Function_Id, source: Source_Loc) {
	normal_block := builder_add_world_block(builder, "invoke_normal", source)
	exception_block := builder_add_world_block(builder, "invoke_unhandled", source)
	builder_emit_core_call_invoke(builder, target, normal_block, exception_block, source = source)
	builder_set_unreachable(builder, source)

	builder_position_at_end(builder, exception_block)
	builder_emit_exception_unhandled(builder, source)
	builder_set_return_world(builder, source)

	builder_position_at_end(builder, normal_block)
}

lower_file_has_global_initializers :: proc(stmts: []^ast.Stmt) -> bool {
	for stmt in stmts {
		if lower_file_stmt_is_global_initializer(stmt) {
			return true
		}
	}
	return false
}

lower_file_global_initializer_stmts :: proc(stmts: []^ast.Stmt, allocator: mem.Allocator) -> [dynamic]^ast.Stmt {
	out := make([dynamic]^ast.Stmt, 0, len(stmts), allocator)
	for stmt in stmts {
		if lower_file_stmt_is_global_initializer(stmt) {
			append(&out, stmt)
		}
	}
	return out
}

lower_file_stmt_is_global_initializer :: proc(stmt: ^ast.Stmt) -> bool {
	if stmt == nil {
		return false
	}
	#partial switch n in stmt.derived_stmt {
	case ^ast.Data_Chained_Decl:
		for clause in n.decls {
			if clause.value_clause != nil && clause.value_clause.expr != nil {
				return true
			}
		}
	}
	return false
}

lower_file_has_implicit_start_stmts :: proc(stmts: []^ast.Stmt) -> bool {
	for stmt in stmts {
		if lower_file_stmt_is_implicit_start(stmt) {
			return true
		}
	}
	return false
}

lower_file_implicit_start_stmts :: proc(stmts: []^ast.Stmt, allocator: mem.Allocator) -> [dynamic]^ast.Stmt {
	out := make([dynamic]^ast.Stmt, 0, len(stmts), allocator)
	for stmt in stmts {
		if lower_file_stmt_is_implicit_start(stmt) {
			append(&out, stmt)
		}
	}
	return out
}

lower_file_stmt_is_implicit_start :: proc(stmt: ^ast.Stmt) -> bool {
	if stmt == nil || lower_file_stmt_is_global_initializer(stmt) {
		return false
	}
	#partial switch _ in stmt.derived_stmt {
	case ^ast.Data_Chained_Decl,
	     ^ast.Types_Decl,
	     ^ast.Constants_Decl,
	     ^ast.Field_Symbols_Decl,
	     ^ast.Statics_Decl,
	     ^ast.Tables_Decl,
	     ^ast.Ranges_Decl,
	     ^ast.Parameters_Decl,
	     ^ast.Select_Options_Decl,
	     ^ast.Controls_Decl,
	     ^ast.Class_Data_Decl,
	     ^ast.Type_Pools_Decl,
	     ^ast.Function_Pool_Decl,
	     ^ast.Include_Stmt,
	     ^ast.Report_Stmt,
	     ^ast.Class_Decl,
	     ^ast.Interface_Decl,
	     ^ast.Method_Decl,
	     ^ast.Form_Decl,
	     ^ast.Function_Decl,
	     ^ast.Module_Decl,
	     ^ast.Event_Block_Stmt,
	     ^ast.Oop_Simple_Stmt,
	     ^ast.Oop_Load_Stmt,
	     ^ast.Macro_Def_Stmt:
		return false
	}
	return true
}

lower_file_entry_name :: proc(file: ^semantic.Project_File, allocator: mem.Allocator) -> string {
	return lower_generated_file_callable_name(file, "report_entry", allocator)
}

lower_generated_file_callable_name :: proc(file: ^semantic.Project_File, suffix: string, allocator: mem.Allocator) -> string {
	prefix := "file"
	if file != nil && file.path != "" {
		prefix = file.path
	}
	builder := strings.builder_make(allocator)
	strings.write_string(&builder, prefix)
	strings.write_byte(&builder, '$')
	strings.write_string(&builder, suffix)
	return strings.to_string(builder)
}

lower_callable :: proc(
	module: ^Module,
	project: ^semantic.Project,
	checker: ^semantic.Checker,
	file: ^semantic.Project_File,
	entity: ^semantic.Entity,
	name: string,
	body: []^ast.Stmt,
	source: Source_Loc = {},
	role: Function_Role = .Unknown,
) -> Function_Id {
	function_name := name
	if function_name == "" && entity != nil {
		function_name = entity.name
	}
	if function_name == "" {
		function_name = "callable"
	}
	builder := builder_begin_function(module, function_name, entity, source, role)
	query := semantic.semantic_query(project, checker, file)
	ctx := Lower_Context {
		module = module,
		project = project,
		checker = checker,
		file = file,
		query = query,
		decl_query = semantic.semantic_query_decls(query),
		ref_query = semantic.semantic_query_refs(query),
		fact_query = semantic.semantic_query_facts(query),
		builder = &builder,
		control_targets = make([dynamic]Lower_Control_Target, 0, 4, context.temp_allocator),
		exception_targets = make([dynamic]Lower_Exception_Target, 0, 4, context.temp_allocator),
	}
	defer delete(ctx.control_targets)
	defer delete(ctx.exception_targets)
	lower_seed_callable_frame(&ctx, source)
	lower_stmt_list(&ctx, body)
	if lower_current_block_open(&ctx) {
		builder_set_return_world(ctx.builder, source)
	}
	return builder.function_id
}

lower_stmt_list :: proc(ctx: ^Lower_Context, stmts: []^ast.Stmt) {
	for stmt in stmts {
		if stmt == nil || !lower_current_block_open(ctx) {
			continue
		}
		lower_stmt(ctx, stmt)
	}
}

lower_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Stmt) {
	source := source_loc_from_node(ctx.file, &stmt.stmt_base)
	#partial switch n in stmt.derived_stmt {
	case ^ast.Data_Chained_Decl:
		for clause in n.decls {
			if clause.kind != .Normal {
				continue
			}
			slot := lower_ensure_slot_for_decl(ctx, clause.name.text, clause.name.range, .Local)
			if clause.value_clause != nil && clause.value_clause.expr != nil {
				value := lower_expr(ctx, clause.value_clause.expr)
				if value != INVALID_VALUE_ID {
					builder_emit_store(ctx.builder, slot, lower_move_value_to_type(ctx, value, slot_ptr(builder_function(ctx.builder), slot).type, source), source)
				}
			}
		}
	case ^ast.Data_Inline_Decl:
		slot := lower_ensure_slot_for_decl(ctx, n.name.text, n.name.range, .Local)
		value := lower_expr(ctx, n.expr)
		if value != INVALID_VALUE_ID {
			builder_emit_store(ctx.builder, slot, lower_move_value_to_type(ctx, value, slot_ptr(builder_function(ctx.builder), slot).type, source), source)
		}
	case ^ast.Assign_Stmt:
		value := lower_expr(ctx, n.rhs)
		lower_store_expr(ctx, n.lhs, value)
		for lhs in n.chain_lhs {
			lower_store_expr(ctx, lhs, value)
		}
	case ^ast.Downcast_Assign_Stmt:
		value := lower_expr(ctx, n.rhs)
		lower_store_expr(ctx, n.lhs, lower_cast_value_to_type(ctx, value, lower_type_for_expr(ctx, n.lhs), source))
	case ^ast.Move_Stmt:
		for entry in n.entries {
			lower_store_expr(ctx, entry.target, lower_expr(ctx, entry.source))
		}
	case ^ast.Compute_Stmt:
		for entry in n.entries {
			lower_store_expr(ctx, entry.target, lower_expr(ctx, entry.source))
		}
	case ^ast.Concatenate_Stmt:
		lower_concatenate_stmt(ctx, n, source)
	case ^ast.Split_Stmt:
		lower_split_stmt(ctx, n, source)
	case ^ast.Condense_Stmt:
		lower_condense_stmt(ctx, n, source)
	case ^ast.Replace_Stmt:
		lower_replace_stmt(ctx, n, source)
	case ^ast.Translate_Stmt:
		lower_translate_stmt(ctx, n, source)
	case ^ast.Shift_Stmt:
		lower_shift_stmt(ctx, n, source)
	case ^ast.Find_Stmt:
		lower_find_stmt(ctx, n, source)
	case ^ast.Search_Stmt:
		lower_search_stmt(ctx, n, source)
	case ^ast.Write_To_Stmt:
		for entry in n.entries {
			lower_store_expr(ctx, entry.target, lower_expr(ctx, entry.source))
		}
	case ^ast.Write_Stmt:
		lower_write_stmt(ctx, n, source)
	case ^ast.Add_Stmt:
		for entry in n.entries {
			lower_arithmetic_stmt(ctx, .ABAP_Add, entry.source, entry.target, entry.result, source)
		}
	case ^ast.Subtract_Stmt:
		for entry in n.entries {
			lower_arithmetic_stmt(ctx, .ABAP_Subtract, entry.source, entry.target, entry.result, source)
		}
	case ^ast.Multiply_Stmt:
		for entry in n.entries {
			lower_arithmetic_stmt(ctx, .ABAP_Multiply, entry.source, entry.target, entry.result, source)
		}
	case ^ast.Divide_Stmt:
		for entry in n.entries {
			lower_arithmetic_stmt(ctx, .ABAP_Divide, entry.source, entry.target, entry.result, source)
		}
	case ^ast.If_Stmt:
		lower_if_stmt(ctx, n)
	case ^ast.Case_Stmt:
		lower_case_stmt(ctx, n)
	case ^ast.While_Stmt:
		lower_while_stmt(ctx, n)
	case ^ast.Do_Stmt:
		lower_do_stmt(ctx, n)
	case ^ast.Loop_Stmt:
		lower_loop_stmt(ctx, n)
	case ^ast.Check_Stmt:
		lower_check_stmt(ctx, n)
	case ^ast.Flow_Stmt:
		lower_flow_stmt(ctx, n)
	case ^ast.Read_Table_Stmt:
		lower_read_table_stmt(ctx, n)
	case ^ast.Append_Stmt:
		lower_append_stmt(ctx, n, source)
	case ^ast.Insert_Stmt:
		lower_insert_stmt(ctx, n, source)
	case ^ast.Modify_Stmt:
		lower_modify_stmt(ctx, n, source)
	case ^ast.Update_Stmt:
		lower_update_stmt(ctx, n, source)
	case ^ast.Delete_Stmt:
		lower_delete_stmt(ctx, n, source)
	case ^ast.Sort_Stmt:
		lower_sort_stmt(ctx, n, source)
	case ^ast.Select_Stmt:
		lower_select_stmt(ctx, n)
	case ^ast.Open_Cursor_Stmt:
		lower_sql_cursor_stmt(ctx, .SQL_Open_Cursor, n.handle, &n.query, source)
	case ^ast.Fetch_Stmt:
		lower_fetch_stmt(ctx, n, source)
	case ^ast.Close_Cursor_Stmt:
		lower_sql_cursor_stmt(ctx, .SQL_Close_Cursor, n.handle, nil, source)
	case ^ast.Call_Stmt:
		lower_call_stmt(ctx, n)
	case ^ast.Perform_Stmt:
		lower_perform_stmt(ctx, n, source)
	case ^ast.Message_Stmt:
		lower_message_stmt(ctx, n, source)
	case ^ast.Expr_Stmt:
		lower_expr(ctx, n.expr)
	case ^ast.Clear_Stmt:
		for operand in n.operands {
			lower_clear_operand(ctx, operand, source)
		}
	case ^ast.Refresh_Stmt:
		for operand in n.operands {
			lower_refresh_operand(ctx, operand, source)
		}
	case ^ast.Free_Stmt:
		if n.memory {
			lower_expr(ctx, n.memory_id)
			builder_emit_unsupported(ctx.builder, "FREE MEMORY semantics", source = source)
			return
		}
		for operand in n.operands {
			lower_free_operand(ctx, operand, source)
		}
	case ^ast.Unassign_Stmt:
		for operand in n.operands {
			lower_unassign_operand(ctx, operand, source)
		}
	case ^ast.Assign_Field_Stmt:
		lower_assign_field_stmt(ctx, n, source)
	case ^ast.Create_Object_Stmt:
		lower_create_object_stmt(ctx, n, source)
	case ^ast.Create_Data_Stmt:
		lower_create_data_stmt(ctx, n, source)
	case ^ast.Raise_Stmt:
		lower_raise_stmt(ctx, n, source)
	case ^ast.Try_Stmt:
		lower_try_stmt(ctx, n, source)
	case:
		if lower_stmt_is_non_executable_declaration(stmt) {
			return
		}
		builder_emit_unsupported(ctx.builder, "unsupported statement", source = source)
	}
}

lower_expr :: proc(ctx: ^Lower_Context, expr: ^ast.Expr) -> Value_Id {
	if expr == nil {
		return INVALID_VALUE_ID
	}
	source := source_loc_from_node(ctx.file, &expr.expr_base)
	#partial switch n in expr.derived_expr {
	case ^ast.Literal_Expr:
		return builder_emit_const(ctx.builder, n.value, lower_type_for_expr(ctx, expr), source)
	case ^ast.Ident_Expr:
		return lower_load_named_expr(ctx, expr, n.name, source)
	case ^ast.Type_Ref_Expr:
		return lower_type_ref_expr(ctx, expr, n, source)
	case ^ast.Data_Inline_Name_Expr:
		slot := lower_ensure_slot_for_decl(ctx, n.name.text, n.name.range, .Local)
		return builder_emit_load(ctx.builder, slot, source)
	case ^ast.Field_Symbol_Inline_Name_Expr:
		slot := lower_ensure_slot_for_decl(ctx, n.name.text, n.name.range, .Local, .Field_Symbol)
		return builder_emit_load(ctx.builder, slot, source)
	case ^ast.Paren_Expr:
		return lower_expr(ctx, n.expr)
	case ^ast.Host_Expr:
		return lower_expr(ctx, n.value)
	case ^ast.Char_String_Template_Expr:
		return lower_string_template_expr(ctx, expr, n, source)
	case ^ast.Template_Literal_Expr:
		return builder_emit_const(ctx.builder, n.literal, lower_type_for_expr(ctx, expr), source)
	case ^ast.Template_Interpolation_Expr:
		return lower_template_interpolation_expr(ctx, expr, n, source)
	case ^ast.Template_Expr:
		return lower_expr(ctx, n.expr)
	case ^ast.Template_Format_Spec_Expr:
		return builder_emit_unsupported(ctx.builder, "string template format option", lower_type_for_expr(ctx, expr), source)
	case ^ast.Binary_Expr:
		left := lower_expr(ctx, n.left)
		right := lower_expr(ctx, n.right)
		assert(left != INVALID_VALUE_ID && right != INVALID_VALUE_ID)
		kind := lower_binary_op_kind(n.op)
		typ := lower_binary_result_type(ctx, expr, n.op)
		if kind == .Unsupported {
			return builder_emit_unsupported(ctx.builder, "unsupported binary expression", typ, source)
		}
		operands := [?]Value_Id{left, right}
		result_types := [?]Type_Id{typ}
		op_id := builder_emit_intrinsic(ctx.builder, kind, operands[:], result_types[:], source = source)
		return op_ptr(builder_function(ctx.builder), op_id).results[0]
	case ^ast.Unary_Expr:
		value := lower_expr(ctx, n.expr)
		assert(value != INVALID_VALUE_ID)
		kind := Intrinsic_Op.ABAP_Not if n.op == .Not else Intrinsic_Op.Unsupported
		if kind == .Unsupported {
			return builder_emit_unsupported(ctx.builder, "unsupported unary expression", lower_type_for_expr(ctx, expr), source)
		}
		operands := [?]Value_Id{value}
		result_types := [?]Type_Id{lower_type_for_expr(ctx, expr)}
		op_id := builder_emit_intrinsic(ctx.builder, kind, operands[:], result_types[:], source = source)
		return op_ptr(builder_function(ctx.builder), op_id).results[0]
	case ^ast.Selector_Expr:
		return lower_selector_expr(ctx, expr, n, source)
	case ^ast.Interface_Qualified_Selector_Expr:
		return lower_interface_selector_expr(ctx, expr, n, source)
	case ^ast.Call_Expr:
		return lower_call_expr(ctx, n, source)
	case ^ast.Table_Expr:
		table := lower_expr(ctx, n.table)
		assert(table != INVALID_VALUE_ID)
		inputs := make([dynamic]Value_Id, 0, 1 + len(n.selectors), context.temp_allocator)
		defer delete(inputs)
		append(&inputs, table)
		components := make([dynamic]Intrinsic_Table_Component, 0, len(n.selectors), context.temp_allocator)
		defer lower_table_components_destroy_scratch(&components)
		if !lower_table_expr_selector_inputs(ctx, n, &inputs, &components) {
			builder_emit_unsupported(ctx.builder, "table expression selector semantics", source = source)
		}
		row_type := lower_table_row_type_for_expr(ctx, n.table)
		row, subrc, _ := builder_emit_table_read(
			ctx.builder,
			inputs[:],
			lower_type_for_expr(ctx, expr),
			row_type,
			Intrinsic_Table_Payload {
				access = lower_table_expr_access(n),
				key_kind = Table_Key_Kind.Free if len(n.selectors) > 0 else Table_Key_Kind.None,
				result_kind = .Value,
				component_count = len(n.selectors),
				components = components,
			},
			source,
		)
		builder_emit_system_write(ctx.builder, "subrc", subrc, source)
		return row
	case ^ast.Constructor_Expr:
		return lower_constructor_expr(ctx, expr, n, source)
	case:
		return builder_emit_unsupported(ctx.builder, "unsupported expression", lower_type_for_expr(ctx, expr), source)
	}
}

lower_store_expr :: proc(ctx: ^Lower_Context, target: ^ast.Expr, value: Value_Id) {
	if target == nil || value == INVALID_VALUE_ID {
		return
	}
	source := source_loc_from_node(ctx.file, &target.expr_base)
	#partial switch n in target.derived_expr {
	case ^ast.Ident_Expr:
		lower_store_named_expr(ctx, target, n.name, value, source)
	case ^ast.Type_Ref_Expr:
		if n.raw_operand {
			lower_store_raw_operand_expr(ctx, target, n, value, source)
			return
		}
		name := n.base_name.text
		if name == "" {
			name = n.name.text
		}
		lower_store_named_expr(ctx, target, name, value, source)
	case ^ast.Data_Inline_Name_Expr:
		slot := lower_ensure_slot_for_decl(ctx, n.name.text, n.name.range, .Local)
		builder_emit_store(ctx.builder, slot, lower_move_value_to_target(ctx, target, value, source), source)
	case ^ast.Field_Symbol_Inline_Name_Expr:
		slot := lower_ensure_slot_for_decl(ctx, n.name.text, n.name.range, .Local, .Field_Symbol)
		builder_emit_store(ctx.builder, slot, lower_move_value_to_target(ctx, target, value, source), source)
	case ^ast.Selector_Expr:
		lower_store_selector_expr(ctx, target, n, value, source)
	case ^ast.Interface_Qualified_Selector_Expr:
		lower_store_interface_selector_expr(ctx, target, n, value, source)
	case:
		builder_emit_unsupported(ctx.builder, "unsupported assignment target", source = source)
	}
}

lower_string_template_expr :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Expr,
	template: ^ast.Char_String_Template_Expr,
	source: Source_Loc,
) -> Value_Id {
	operands := make([dynamic]Value_Id, 0, len(template.parts), context.temp_allocator)
	defer delete(operands)
	for part in template.parts {
		value := lower_expr(ctx, part)
		if value != INVALID_VALUE_ID {
			append(&operands, value)
		}
	}
	return lower_emit_value_op(ctx, .ABAP_String_Template, operands[:], lower_type_for_expr(ctx, expr), source)
}

lower_template_interpolation_expr :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Expr,
	interpolation: ^ast.Template_Interpolation_Expr,
	source: Source_Loc,
) -> Value_Id {
	operands := make([dynamic]Value_Id, 0, 1 + len(interpolation.format_specs), context.temp_allocator)
	defer delete(operands)
	value := lower_expr(ctx, interpolation.expr)
	if value != INVALID_VALUE_ID {
		append(&operands, value)
	}
	for spec in interpolation.format_specs {
		if spec == nil {
			continue
		}
		if spec_node, ok := spec.derived_expr.(^ast.Template_Format_Spec_Expr); ok && spec_node.value != nil {
			if _, has_fact := lower_expr_info_for_node(ctx, &spec_node.value.expr_base); has_fact {
				spec_value := lower_expr(ctx, spec_node.value)
				if spec_value != INVALID_VALUE_ID {
					append(&operands, spec_value)
				}
			}
		}
	}
	return lower_emit_value_op(ctx, .ABAP_String_Template, operands[:], lower_type_for_expr(ctx, expr), source)
}

lower_constructor_expr :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Expr,
	constructor: ^ast.Constructor_Expr,
	source: Source_Loc,
) -> Value_Id {
	result_type := lower_type_for_expr(ctx, expr)
	#partial switch constructor.kind {
	case .Conv, .Exact, .Cast:
		if value, ok := lower_constructor_single_value(ctx, constructor); ok {
			return lower_cast_value_to_type(ctx, value, result_type, source)
		}
		lower_constructor_collect_args(ctx, constructor.args[:], nil)
		return builder_emit_unsupported(ctx.builder, "constructor cast value", result_type, source)
	case .Value, .New, .Ref:
		operands := make([dynamic]Value_Id, 0, len(constructor.args), context.temp_allocator)
		defer delete(operands)
		lower_constructor_collect_args(ctx, constructor.args[:], &operands)
		created := lower_emit_value_op(
			ctx,
			.ABAP_Construct,
			operands[:],
			result_type,
			source,
			Intrinsic_Call_Payload{callee_name = lower_constructor_kind_name(constructor.kind)},
		)
		if constructor.kind == .New {
			lower_emit_constructor_call_for_expr(ctx, expr, created, operands[:], source)
		}
		return created
	case .Corresponding, .Filter, .Reduce, .Switch, .Cond, .Throw:
		operands := make([dynamic]Value_Id, 0, len(constructor.args), context.temp_allocator)
		defer delete(operands)
		lower_constructor_collect_args(ctx, constructor.args[:], &operands)
		return builder_emit_unsupported(ctx.builder, lower_constructor_deferred_message(constructor.kind), result_type, source)
	}
	operands := make([dynamic]Value_Id, 0, len(constructor.args), context.temp_allocator)
	defer delete(operands)
	lower_constructor_collect_args(ctx, constructor.args[:], &operands)
	created := lower_emit_value_op(
		ctx,
		.ABAP_Construct,
		operands[:],
		result_type,
		source,
		Intrinsic_Call_Payload{callee_name = lower_constructor_kind_name(constructor.kind)},
	)
	if constructor.kind == .New {
		lower_emit_constructor_call_for_expr(ctx, expr, created, operands[:], source)
	}
	return created
}

lower_emit_constructor_call_for_expr :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Expr,
	receiver: Value_Id,
	args: []Value_Id,
	source: Source_Loc,
) {
	if expr == nil {
		return
	}
	if info, ok := lower_expr_info_for_node(ctx, &expr.expr_base); ok {
		lower_emit_constructor_call_for_type(ctx, info.type, receiver, args, source)
	}
}

lower_emit_constructor_call_for_type :: proc(
	ctx: ^Lower_Context,
	typ: ^semantic.Type,
	receiver: Value_Id,
	args: []Value_Id,
	source: Source_Loc,
) {
	if receiver == INVALID_VALUE_ID || typ == nil {
		return
	}
	constructor := lower_constructor_method_for_type(ctx, typ)
	if constructor == nil {
		return
	}
	inputs := make([dynamic]Value_Id, 0, 1 + len(args), context.temp_allocator)
	defer delete(inputs)
	append(&inputs, receiver)
	for arg in args {
		if arg != INVALID_VALUE_ID {
			append(&inputs, arg)
		}
	}
	lower_emit_method_may_throw(
		ctx,
		constructor,
			lower_callable_entity_name(constructor, "constructor", ctx.module.allocator),
			inputs[:],
			source = source,
		)
}

lower_constructor_method_for_type :: proc(ctx: ^Lower_Context, typ: ^semantic.Type) -> ^semantic.Entity {
	if typ == nil {
		return nil
	}
	owner := semantic.checker_type_object_entity(typ)
	if owner == nil || owner.kind != .Class {
		return nil
	}
	payload, ok := owner.payload.(^semantic.Entity_Object_Payload)
	if !ok || payload == nil || payload.definition_scope == nil {
		return nil
	}
	name := semantic.project_intern_lower_ascii(ctx.project, "constructor")
	constructor, found := semantic.scope_lookup_declaration(payload.definition_scope, .Routine, name)
	if !found || constructor == nil || constructor.kind != .Method {
		return nil
	}
	return constructor
}

lower_constructor_single_value :: proc(ctx: ^Lower_Context, constructor: ^ast.Constructor_Expr) -> (Value_Id, bool) {
	if constructor == nil || len(constructor.args) != 1 {
		return INVALID_VALUE_ID, false
	}
	arg := constructor.args[0]
	if arg == nil {
		return INVALID_VALUE_ID, false
	}
	#partial switch n in arg.derived_expr {
	case ^ast.Call_Arg_List_Expr:
		if len(n.args) != 1 {
			return INVALID_VALUE_ID, false
		}
		return lower_constructor_arg_value(ctx, n.args[0])
	case ^ast.Constructor_Named_Assignment_Expr:
		return lower_constructor_arg_value(ctx, n.value)
	case ^ast.Call_Named_Arg_Expr:
		return lower_constructor_arg_value(ctx, n.value)
	case ^ast.Call_Positional_Arg_Expr:
		return lower_constructor_arg_value(ctx, n.value)
	}
	return lower_constructor_arg_value(ctx, arg)
}

lower_constructor_arg_value :: proc(ctx: ^Lower_Context, expr: ^ast.Expr) -> (Value_Id, bool) {
	value := lower_expr(ctx, expr)
	return value, value != INVALID_VALUE_ID
}

lower_constructor_collect_args :: proc(ctx: ^Lower_Context, args: []^ast.Expr, out: ^[dynamic]Value_Id) {
	for arg in args {
		lower_constructor_collect_arg(ctx, arg, out)
	}
}

lower_constructor_collect_arg :: proc(ctx: ^Lower_Context, arg: ^ast.Expr, out: ^[dynamic]Value_Id) {
	if arg == nil {
		return
	}
	append_value := proc(out: ^[dynamic]Value_Id, value: Value_Id) {
		if out != nil && value != INVALID_VALUE_ID {
			append(out, value)
		}
	}
	#partial switch n in arg.derived_expr {
	case ^ast.Call_Arg_List_Expr:
		lower_constructor_collect_args(ctx, n.args[:], out)
	case ^ast.Call_Arg_Section_Expr:
		lower_constructor_collect_args(ctx, n.args[:], out)
	case ^ast.Call_Named_Arg_Expr:
		append_value(out, lower_expr(ctx, n.value))
	case ^ast.Call_Positional_Arg_Expr:
		append_value(out, lower_expr(ctx, n.value))
	case ^ast.Constructor_Let_Binding_Expr:
		append_value(out, lower_expr(ctx, n.value))
	case ^ast.Constructor_Named_Assignment_Expr:
		append_value(out, lower_expr(ctx, n.value))
	case ^ast.Constructor_Base_Clause_Expr:
		append_value(out, lower_expr(ctx, n.value))
	case ^ast.Constructor_Lines_Of_Clause_Expr:
		append_value(out, lower_expr(ctx, n.source))
		append_value(out, lower_expr(ctx, n.from))
		append_value(out, lower_expr(ctx, n.to))
	case ^ast.Constructor_Optional_Expr:
		append_value(out, lower_expr(ctx, n.value))
	case ^ast.Constructor_When_Clause_Expr:
		append_value(out, lower_expr(ctx, n.condition))
		append_value(out, lower_expr(ctx, n.result))
	case ^ast.Constructor_Else_Clause_Expr:
		append_value(out, lower_expr(ctx, n.result))
	case ^ast.Constructor_For_Clause_Expr:
		append_value(out, lower_expr(ctx, n.init))
		append_value(out, lower_expr(ctx, n.then_expr))
		append_value(out, lower_expr(ctx, n.condition))
		append_value(out, lower_expr(ctx, n.source))
		append_value(out, lower_expr(ctx, n.group_by))
		append_value(out, lower_expr(ctx, n.where_clause))
		lower_constructor_collect_args(ctx, n.body[:], out)
	case ^ast.Constructor_Where_Clause_Expr:
		append_value(out, lower_expr(ctx, n.condition))
	case ^ast.Constructor_Filter_Except_In_Clause_Expr:
		append_value(out, lower_expr(ctx, n.source))
		append_value(out, lower_expr(ctx, n.where_clause))
	case ^ast.Constructor_Init_Clause_Expr:
		lower_constructor_collect_args(ctx, n.assignments[:], out)
	case ^ast.Constructor_Next_Clause_Expr:
		lower_constructor_collect_args(ctx, n.assignments[:], out)
	case ^ast.Constructor_Corresponding_Mapping_Clause_Expr:
		lower_constructor_collect_args(ctx, n.assignments[:], out)
	case ^ast.Constructor_Corresponding_Mapping_Assignment_Expr:
		append_value(out, lower_expr(ctx, n.source))
		append_value(out, lower_expr(ctx, n.default_value))
		append_value(out, lower_expr(ctx, n.mapping))
		append_value(out, lower_expr(ctx, n.except))
	case ^ast.Constructor_Corresponding_Except_Clause_Expr:
		lower_constructor_collect_args(ctx, n.names[:], out)
	case ^ast.Let_Expr:
		lower_constructor_collect_args(ctx, n.bindings[:], out)
		lower_constructor_collect_args(ctx, n.body[:], out)
	case:
		append_value(out, lower_expr(ctx, arg))
	}
}

lower_type_ref_expr :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Expr,
	ref: ^ast.Type_Ref_Expr,
	source: Source_Loc,
) -> Value_Id {
	if ref.raw_operand {
		return lower_raw_operand_expr(ctx, expr, ref, source)
	}
	name := ref.base_name.text
	if name == "" {
		name = ref.name.text
	}
	return lower_load_named_expr(ctx, expr, name, source)
}

lower_raw_operand_expr :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Expr,
	raw: ^ast.Type_Ref_Expr,
	source: Source_Loc,
) -> Value_Id {
	fact_count := len(raw.raw_decls) + len(raw.raw_refs)
	if fact_count != 1 {
		return builder_emit_unsupported(ctx.builder, "ambiguous raw operand", lower_type_for_expr(ctx, expr), source)
	}
	if len(raw.raw_decls) == 1 {
		decl := raw.raw_decls[0]
		entity_kind := semantic.Entity_Kind.Variable if decl.kind == .Data else semantic.Entity_Kind.Field_Symbol
		slot := lower_ensure_slot_for_decl(ctx, decl.name.text, decl.name.range, .Local, entity_kind)
		return builder_emit_load(ctx.builder, slot, Source_Loc{file = ctx.file, node = &expr.expr_base, range = decl.name.range})
	}
	return lower_raw_operand_ref(ctx, expr, raw.raw_refs[0], source)
}

lower_raw_operand_ref :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Expr,
	ref: ast.Raw_Operand_Ref,
	source: Source_Loc,
) -> Value_Id {
	if ref.call_like || ref.type_base || ref.name.text == "" {
		return builder_emit_unsupported(ctx.builder, "unsupported raw operand reference", lower_type_for_expr(ctx, expr), source)
	}
	value := lower_load_raw_operand_base(ctx, expr, ref)
	if value == INVALID_VALUE_ID {
		return builder_emit_unsupported(ctx.builder, "unresolved raw operand", lower_type_for_expr(ctx, expr), source)
	}
	projection_segments := make([dynamic]Projection_Segment, 0, len(ref.path), context.temp_allocator)
	defer delete(projection_segments)
	for segment, i in ref.path {
		segment_name := lower_raw_operand_segment_name(segment)
		if segment_name == "" {
			return builder_emit_unsupported(ctx.builder, "dynamic selector path", lower_type_for_expr(ctx, expr), source)
		}
		is_last := i == len(ref.path) - 1
		segment_source := Source_Loc{file = ctx.file, node = &expr.expr_base, range = segment.name.range}
		segment_type := lower_type_for_raw_segment(ctx, expr, segment, is_last)
		entity := lower_field_entity_at_range(ctx, segment.name.range)
		append(
			&projection_segments,
				lower_field_projection_segment(segment_name, segment_source, entity, segment.selector),
		)
		value = lower_emit_field_load(ctx, value, segment_name, segment_type, segment_source, entity, segment.selector, projection_segments[:])
	}
	if ref.dynamic_path {
		return builder_emit_unsupported(ctx.builder, "dynamic selector path", lower_type_for_expr(ctx, expr), source)
	}
	return value
}

lower_load_raw_operand_base :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Expr,
	ref: ast.Raw_Operand_Ref,
) -> Value_Id {
	use := semantic.semantic_ref_use_at_range(ctx.ref_query, ref.name.range)
	entity := use.entity if use != nil else nil
	base_type := module_type_from_semantic(ctx.module, entity.type) if entity != nil else BUILTIN_TYPE_UNKNOWN
	if len(ref.path) == 0 && !ref.dynamic_path {
		base_type = lower_type_for_expr(ctx, expr) if entity == nil else base_type
	}
	source := Source_Loc{file = ctx.file, node = &expr.expr_base, range = ref.name.range}
	if lower_entity_is_instance_attribute(entity) {
		return lower_load_instance_attribute(ctx, entity, ref.name.text, base_type, source)
	}
	slot := lower_ensure_slot(
		ctx,
		entity,
		ref.name.text,
		base_type,
		lower_slot_kind_for_entity(entity),
		source,
	)
	return builder_emit_load(ctx.builder, slot, source)
}

lower_type_for_raw_segment :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Expr,
	segment: ast.Raw_Operand_Path_Segment,
	is_last: bool,
) -> Type_Id {
	if use := semantic.semantic_ref_use_at_range(ctx.ref_query, segment.name.range); use != nil && use.entity != nil {
		return module_type_from_semantic(ctx.module, use.entity.type)
	}
	if is_last {
		return lower_type_for_expr(ctx, expr)
	}
	return BUILTIN_TYPE_UNKNOWN
}

lower_store_raw_operand_expr :: proc(
	ctx: ^Lower_Context,
	target: ^ast.Expr,
	raw: ^ast.Type_Ref_Expr,
	value: Value_Id,
	source: Source_Loc,
) {
	fact_count := len(raw.raw_decls) + len(raw.raw_refs)
	if fact_count != 1 {
		builder_emit_unsupported(ctx.builder, "ambiguous raw operand assignment target", source = source)
		return
	}
	if len(raw.raw_decls) == 1 {
		decl := raw.raw_decls[0]
		entity_kind := semantic.Entity_Kind.Variable if decl.kind == .Data else semantic.Entity_Kind.Field_Symbol
		slot := lower_ensure_slot_for_decl(ctx, decl.name.text, decl.name.range, .Local, entity_kind)
		builder_emit_store(ctx.builder, slot, lower_move_value_to_target(ctx, target, value, source), source)
		return
	}
	lower_store_raw_operand_ref(ctx, target, raw.raw_refs[0], value, source)
}

lower_store_raw_operand_ref :: proc(
	ctx: ^Lower_Context,
	target: ^ast.Expr,
	ref: ast.Raw_Operand_Ref,
	value: Value_Id,
	source: Source_Loc,
) {
	if ref.call_like || ref.type_base || ref.dynamic_path || ref.name.text == "" {
		builder_emit_unsupported(ctx.builder, "unsupported raw operand assignment target", source = source)
		return
	}
	if len(ref.path) == 0 {
		use := semantic.semantic_ref_use_at_range(ctx.ref_query, ref.name.range)
		entity := use.entity if use != nil else nil
		if lower_entity_is_instance_attribute(entity) {
			lower_store_instance_attribute(ctx, target, entity, ref.name.text, value, source)
			return
		}
		slot := lower_ensure_slot(
			ctx,
			entity,
			ref.name.text,
			lower_type_for_expr(ctx, target),
			lower_slot_kind_for_entity(entity),
			Source_Loc{file = ctx.file, node = &target.expr_base, range = ref.name.range},
		)
		builder_emit_store(ctx.builder, slot, lower_move_value_to_target(ctx, target, value, source), source)
		return
	}
	base := lower_load_raw_operand_base(ctx, target, ref)
	if base == INVALID_VALUE_ID {
		builder_emit_unsupported(ctx.builder, "unresolved raw operand assignment target", source = source)
		return
	}
	projection_segments := make([dynamic]Projection_Segment, 0, len(ref.path), context.temp_allocator)
	defer delete(projection_segments)
	for segment in ref.path[:len(ref.path) - 1] {
		segment_name := lower_raw_operand_segment_name(segment)
		if segment_name == "" {
			builder_emit_unsupported(ctx.builder, "dynamic selector path", source = source)
			return
		}
		segment_source := Source_Loc{file = ctx.file, node = &target.expr_base, range = segment.name.range}
		entity := lower_field_entity_at_range(ctx, segment.name.range)
		append(
			&projection_segments,
				lower_field_projection_segment(segment_name, segment_source, entity, segment.selector),
		)
		base = lower_emit_field_load(
			ctx,
			base,
			segment_name,
			lower_type_for_raw_segment(ctx, target, segment, false),
			segment_source,
			entity,
			segment.selector,
			projection_segments[:],
		)
		}
	last := ref.path[len(ref.path) - 1]
	last_name := lower_raw_operand_segment_name(last)
	if last_name == "" {
		builder_emit_unsupported(ctx.builder, "dynamic selector path", source = source)
		return
	}
	last_entity := lower_field_entity_at_range(ctx, last.name.range)
	last_source := Source_Loc{file = ctx.file, node = &target.expr_base, range = last.name.range}
	append(
		&projection_segments,
			lower_field_projection_segment(last_name, last_source, last_entity, last.selector),
	)
	last_projection := lower_add_projection_segments(ctx, projection_segments[:])
		builder_emit_field_store(
			ctx.builder,
			base,
			lower_move_value_to_target(ctx, target, value, source),
			last_projection,
			last_source,
		)
}

lower_raw_operand_segment_name :: proc "contextless" (segment: ast.Raw_Operand_Path_Segment) -> string {
	if segment.name.text != "" {
		return segment.name.text
	}
	if segment.selector == .Arrow {
		return "*"
	}
	return ""
}

lower_selector_field_name :: proc(ctx: ^Lower_Context, field: ^ast.Expr, selector: ast.Selector_Op) -> string {
	name := lower_expr_name(field, ctx.project.allocator)
	if name == "" && selector == .Arrow {
		return "*"
	}
	return name
}

lower_selector_expr :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Expr,
	selector: ^ast.Selector_Expr,
	source: Source_Loc,
) -> Value_Id {
	if field_name, ok := lower_system_field_selector(ctx, selector); ok {
		return builder_emit_system_read(ctx.builder, field_name, lower_type_for_expr(ctx, expr), source)
	}
	base := lower_expr(ctx, selector.base)
	if base == INVALID_VALUE_ID {
		return builder_emit_unsupported(ctx.builder, "selector base value", lower_type_for_expr(ctx, expr), source)
	}
	field_name := lower_selector_field_name(ctx, selector.field, selector.op)
	entity := lower_entity_for_node(ctx, &selector.field.expr_base)
	projection_segments := make([dynamic]Projection_Segment, 0, 4, context.temp_allocator)
	defer delete(projection_segments)
	lower_append_projection_prefix_for_expr(ctx, selector.base, &projection_segments)
	append(
		&projection_segments,
			lower_field_projection_segment(field_name, source, entity, selector.op),
	)
	return lower_emit_field_load(
		ctx,
		base,
		field_name,
		lower_type_for_expr(ctx, expr),
		source,
		entity,
		selector.op,
		projection_segments[:],
	)
}

lower_system_field_selector :: proc(ctx: ^Lower_Context, selector: ^ast.Selector_Expr) -> (string, bool) {
	if selector == nil || selector.op != .Dash {
		return "", false
	}
	base_name := lower_expr_name(selector.base, context.temp_allocator)
	if !strings.equal_fold(base_name, "sy") && !strings.equal_fold(base_name, "syst") {
		return "", false
	}
	entity := lower_entity_for_node(ctx, &selector.field.expr_base)
	if entity == nil || entity.kind != .Field || !semantic.entity_is_builtin(entity) ||
	   entity.owner == nil || !strings.equal_fold(entity.owner.name, "syst") {
		return "", false
	}
	field_name := entity.name
	if field_name == "" {
		field_name = semantic.project_intern_lower_ascii(
			ctx.project,
			lower_expr_name(selector.field, context.temp_allocator),
		)
	}
	return field_name, field_name != ""
}

lower_interface_selector_expr :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Expr,
	selector: ^ast.Interface_Qualified_Selector_Expr,
	source: Source_Loc,
) -> Value_Id {
	receiver := lower_expr(ctx, selector.receiver)
	if receiver == INVALID_VALUE_ID {
		return builder_emit_unsupported(ctx.builder, "interface selector receiver", lower_type_for_expr(ctx, expr), source)
	}
	field_name := lower_interface_selector_name(selector, ctx.project.allocator)
	entity := lower_entity_for_node(ctx, &selector.member.expr_base)
	projection_segments := make([dynamic]Projection_Segment, 0, 4, context.temp_allocator)
	defer delete(projection_segments)
	lower_append_projection_prefix_for_expr(ctx, selector.receiver, &projection_segments)
	append(
		&projection_segments,
			lower_field_projection_segment(field_name, source, entity, selector.receiver_op),
	)
	return lower_emit_field_load(
		ctx,
		receiver,
		field_name,
		lower_type_for_expr(ctx, expr),
		source,
		entity,
		selector.receiver_op,
		projection_segments[:],
	)
}

lower_store_selector_expr :: proc(
	ctx: ^Lower_Context,
	target: ^ast.Expr,
	selector: ^ast.Selector_Expr,
	value: Value_Id,
	source: Source_Loc,
) {
	if field_name, ok := lower_system_field_selector(ctx, selector); ok {
		builder_emit_system_write(ctx.builder, field_name, lower_move_value_to_target(ctx, target, value, source), source)
		return
	}
	base := lower_expr(ctx, selector.base)
	if base == INVALID_VALUE_ID {
		builder_emit_unsupported(ctx.builder, "selector assignment base", source = source)
		return
	}
	field_name := lower_selector_field_name(ctx, selector.field, selector.op)
	entity := lower_entity_for_node(ctx, &selector.field.expr_base)
	projection_segments := make([dynamic]Projection_Segment, 0, 4, context.temp_allocator)
	defer delete(projection_segments)
	lower_append_projection_prefix_for_expr(ctx, selector.base, &projection_segments)
	append(
		&projection_segments,
			lower_field_projection_segment(field_name, source, entity, selector.op),
	)
	projection := lower_add_projection_segments(ctx, projection_segments[:])
		builder_emit_field_store(
			ctx.builder,
			base,
			lower_move_value_to_target(ctx, target, value, source),
			projection,
			source,
		)
}

lower_store_interface_selector_expr :: proc(
	ctx: ^Lower_Context,
	target: ^ast.Expr,
	selector: ^ast.Interface_Qualified_Selector_Expr,
	value: Value_Id,
	source: Source_Loc,
) {
	receiver := lower_expr(ctx, selector.receiver)
	if receiver == INVALID_VALUE_ID {
		builder_emit_unsupported(ctx.builder, "interface selector receiver assignment", source = source)
		return
	}
	field_name := lower_interface_selector_name(selector, ctx.project.allocator)
	entity := lower_entity_for_node(ctx, &selector.member.expr_base)
	projection_segments := make([dynamic]Projection_Segment, 0, 4, context.temp_allocator)
	defer delete(projection_segments)
	lower_append_projection_prefix_for_expr(ctx, selector.receiver, &projection_segments)
	append(
		&projection_segments,
			lower_field_projection_segment(field_name, source, entity, selector.receiver_op),
	)
	projection := lower_add_projection_segments(ctx, projection_segments[:])
		builder_emit_field_store(
			ctx.builder,
			receiver,
			lower_move_value_to_target(ctx, target, value, source),
			projection,
			source,
		)
}

lower_clear_operand :: proc(ctx: ^Lower_Context, operand: ast.Clear_Operand_Clause, source: Source_Loc) {
	inputs := make([dynamic]Value_Id, 0, 1, context.temp_allocator)
	defer delete(inputs)
	if operand.mode == .With_Value {
		value := lower_expr(ctx, operand.value)
		if value != INVALID_VALUE_ID {
			append(&inputs, value)
		}
	}
	result := lower_emit_effect_value_op(ctx, .ABAP_Clear, inputs[:], lower_type_for_expr(ctx, operand.target), source)
	lower_store_expr(ctx, operand.target, result)
}

lower_refresh_operand :: proc(ctx: ^Lower_Context, operand: ast.Refresh_Operand_Clause, source: Source_Loc) {
	result := lower_emit_effect_value_op(ctx, .ABAP_Refresh, nil, lower_type_for_expr(ctx, operand.target), source)
	lower_store_expr(ctx, operand.target, result)
}

lower_free_operand :: proc(ctx: ^Lower_Context, operand: ast.Free_Operand_Clause, source: Source_Loc) {
	if operand.object {
		builder_emit_unsupported(ctx.builder, "FREE OBJECT semantics", lower_type_for_expr(ctx, operand.target), source)
		return
	}
	result := lower_emit_effect_value_op(ctx, .ABAP_Free, nil, lower_type_for_expr(ctx, operand.target), source)
	lower_store_expr(ctx, operand.target, result)
}

lower_unassign_operand :: proc(ctx: ^Lower_Context, operand: ast.Unassign_Operand_Clause, source: Source_Loc) {
	result := lower_emit_effect_value_op(ctx, .ABAP_Unassign, nil, lower_type_for_expr(ctx, operand.target), source)
	lower_store_expr(ctx, operand.target, result)
}

lower_assign_field_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Assign_Field_Stmt, source: Source_Loc) {
	inputs := make([dynamic]Value_Id, 0, 2, context.temp_allocator)
	defer delete(inputs)
	if stmt.component != nil || stmt.structure != nil {
		lower_expr(ctx, stmt.component)
		lower_expr(ctx, stmt.structure)
		builder_emit_unsupported(ctx.builder, "ASSIGN COMPONENT semantics", source = source)
		return
	}
	if stmt.source != nil {
		source_value := lower_expr(ctx, stmt.source)
		if source_value != INVALID_VALUE_ID {
			append(&inputs, source_value)
		}
	}
	if stmt.casting || stmt.casting_type != nil || stmt.casting_decimals != nil {
		lower_expr(ctx, stmt.casting_type)
		lower_expr(ctx, stmt.casting_decimals)
		builder_emit_unsupported(ctx.builder, "ASSIGN CASTING semantics", source = source)
	}
	if len(inputs) == 0 || stmt.target == nil {
		builder_emit_unsupported(ctx.builder, "ASSIGN field-symbol operands", source = source)
		return
	}
	result := lower_emit_effect_value_op(ctx, .ABAP_Assign_Field, inputs[:], lower_type_for_expr(ctx, stmt.target), source)
	lower_store_expr(ctx, stmt.target, result)
	builder_emit_system_write(ctx.builder, "subrc", source = source)
}

lower_emit_field_load :: proc(
	ctx: ^Lower_Context,
	base: Value_Id,
	field_name: string,
	result_type: Type_Id,
	source: Source_Loc,
	entity: ^semantic.Entity = nil,
	selector: ast.Selector_Op = .Dash,
	projection_segments: []Projection_Segment = nil,
) -> Value_Id {
	projection := lower_add_projection_segments(ctx, projection_segments) if len(projection_segments) > 0 else lower_add_field_projection(ctx, field_name, source, entity, selector)
		return builder_emit_field_load(
			ctx.builder,
			base,
			projection,
			lower_known_result_type(result_type),
			source,
	)
}

lower_add_field_projection :: proc(
	ctx: ^Lower_Context,
	field_name: string,
	source: Source_Loc,
	entity: ^semantic.Entity = nil,
	selector: ast.Selector_Op = .Dash,
) -> Projection_Id {
	segment := lower_field_projection_segment(field_name, source, entity, selector)
	segments := [?]Projection_Segment{segment}
	return lower_add_projection_segments(ctx, segments[:])
}

lower_add_projection_segments :: proc(ctx: ^Lower_Context, segments: []Projection_Segment) -> Projection_Id {
	return function_add_projection(builder_function(ctx.builder), segments, ctx.module.allocator)
}

lower_field_projection_segment :: proc(
	field_name: string,
	source: Source_Loc,
	entity: ^semantic.Entity = nil,
	selector: ast.Selector_Op = .Dash,
) -> Projection_Segment {
	return Projection_Segment {
		kind = .Field,
		name = field_name,
		selector = selector,
		field_index = lower_field_index(entity),
		entity = entity,
		source = source,
	}
}

lower_append_projection_prefix_for_expr :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Expr,
	segments: ^[dynamic]Projection_Segment,
) {
	if expr == nil {
		return
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Selector_Expr:
		lower_append_projection_prefix_for_expr(ctx, n.base, segments)
		field_name := lower_selector_field_name(ctx, n.field, n.op)
		source := Source_Loc{file = ctx.file, node = &n.field.expr_base, range = n.field.range}
			append(
				segments,
				lower_field_projection_segment(
					field_name,
					source,
					lower_entity_for_node(ctx, &n.field.expr_base),
				n.op,
			),
		)
	case ^ast.Interface_Qualified_Selector_Expr:
		lower_append_projection_prefix_for_expr(ctx, n.receiver, segments)
		field_name := lower_interface_selector_name(n, ctx.project.allocator)
		source := Source_Loc{file = ctx.file, node = &n.member.expr_base, range = n.member.range}
			append(
				segments,
				lower_field_projection_segment(
					field_name,
					source,
					lower_entity_for_node(ctx, &n.member.expr_base),
				n.receiver_op,
			),
		)
	case ^ast.Paren_Expr:
		lower_append_projection_prefix_for_expr(ctx, n.expr, segments)
	}
}

lower_field_entity_at_range :: proc(ctx: ^Lower_Context, range: tokenizer.Range) -> ^semantic.Entity {
	if use := semantic.semantic_ref_use_at_range(ctx.ref_query, range); use != nil {
		return use.entity
	}
	return nil
}

lower_field_index :: proc(entity: ^semantic.Entity) -> i32 {
	if entity == nil {
		return -1
	}
	payload, ok := entity.payload.(^semantic.Entity_Field_Payload)
	if !ok || payload == nil {
		return -1
	}
	return i32(payload.field_index)
}

lower_emit_value_op :: proc(
	ctx: ^Lower_Context,
	kind: Intrinsic_Op,
	operands: []Value_Id,
	result_type: Type_Id,
	source: Source_Loc,
	payload: Intrinsic_Payload = {},
) -> Value_Id {
	result_types := [?]Type_Id{lower_known_result_type(result_type)}
	op_id := builder_emit_intrinsic(ctx.builder, kind, operands, result_types[:], payload = payload, source = source)
	return op_ptr(builder_function(ctx.builder), op_id).results[0]
}

lower_emit_effect_value_op :: proc(
	ctx: ^Lower_Context,
	kind: Intrinsic_Op,
	inputs: []Value_Id,
	result_type: Type_Id,
	source: Source_Loc,
	payload: Intrinsic_Payload = {},
) -> Value_Id {
	result_types := [?]Type_Id{lower_known_result_type(result_type)}
	op_id := builder_emit_effect_intrinsic(ctx.builder, kind, inputs, result_types[:], payload = payload, source = source)
	return op_ptr(builder_function(ctx.builder), op_id).results[1]
}

lower_move_value_to_target :: proc(
	ctx: ^Lower_Context,
	target: ^ast.Expr,
	value: Value_Id,
	source: Source_Loc,
) -> Value_Id {
	return lower_move_value_to_type(ctx, value, lower_type_for_expr(ctx, target), source)
}

lower_move_value_to_type :: proc(ctx: ^Lower_Context, value: Value_Id, target_type: Type_Id, source: Source_Loc) -> Value_Id {
	if value == INVALID_VALUE_ID || target_type == INVALID_TYPE_ID || target_type == BUILTIN_TYPE_UNKNOWN {
		return value
	}
	if value_type(builder_function(ctx.builder), value) == target_type {
		return value
	}
	operands := [?]Value_Id{value}
	return lower_emit_value_op(ctx, .ABAP_Move, operands[:], target_type, source)
}

lower_cast_value_to_type :: proc(ctx: ^Lower_Context, value: Value_Id, target_type: Type_Id, source: Source_Loc) -> Value_Id {
	if value == INVALID_VALUE_ID {
		return INVALID_VALUE_ID
	}
	if target_type == INVALID_TYPE_ID || target_type == BUILTIN_TYPE_UNKNOWN {
		return builder_emit_unsupported(ctx.builder, "cast result type", BUILTIN_TYPE_UNKNOWN, source)
	}
	operands := [?]Value_Id{value}
	result_types := [?]Type_Id{target_type}
	op_id := builder_emit_op(ctx.builder, .Cast, operands[:], result_types[:], source = source)
	return op_ptr(builder_function(ctx.builder), op_id).results[0]
}

lower_known_result_type :: proc "contextless" (typ: Type_Id) -> Type_Id {
	if typ == INVALID_TYPE_ID {
		return BUILTIN_TYPE_UNKNOWN
	}
	return typ
}

lower_concatenate_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Concatenate_Stmt, source: Source_Loc) {
	if stmt.byte_mode {
		for entry in stmt.entries {
			lower_concatenate_entry_inputs(ctx, entry)
		}
		builder_emit_unsupported(ctx.builder, "CONCATENATE BYTE MODE semantics", source = source)
		return
	}
	for entry in stmt.entries {
		if entry.target == nil {
			lower_concatenate_entry_inputs(ctx, entry)
			builder_emit_unsupported(ctx.builder, "CONCATENATE target semantics", source = source)
			continue
		}
		if entry.lines_of {
			lower_concatenate_entry_inputs(ctx, entry)
			builder_emit_unsupported(ctx.builder, "CONCATENATE LINES OF semantics", source = source)
			continue
		}
		inputs := make([dynamic]Value_Id, 0, len(entry.sources) + 1, context.temp_allocator)
		defer delete(inputs)
		lower_concatenate_entry_inputs_into(ctx, entry, &inputs)
		payload := Intrinsic_String_Payload {
			has_separator = entry.separator != nil,
			respecting_blanks = entry.respecting_blanks,
		}
		if len(inputs) == 0 || (payload.has_separator && len(inputs) < 2) {
			builder_emit_unsupported(ctx.builder, "CONCATENATE source semantics", source = source)
			continue
		}
		result := lower_emit_value_op(
			ctx,
			.ABAP_Concatenate,
			inputs[:],
			lower_type_for_expr(ctx, entry.target),
			source,
			payload,
		)
		lower_store_expr(ctx, entry.target, result)
	}
}

lower_concatenate_entry_inputs :: proc(ctx: ^Lower_Context, entry: ast.Concatenate_Entry_Clause) {
	inputs := make([dynamic]Value_Id, 0, len(entry.sources) + 1, context.temp_allocator)
	defer delete(inputs)
	lower_concatenate_entry_inputs_into(ctx, entry, &inputs)
}

lower_concatenate_entry_inputs_into :: proc(
	ctx: ^Lower_Context,
	entry: ast.Concatenate_Entry_Clause,
	inputs: ^[dynamic]Value_Id,
) {
	for source_expr in entry.sources {
		value := lower_expr(ctx, source_expr)
		if value != INVALID_VALUE_ID {
			append(inputs, value)
		}
	}
	if entry.separator != nil {
		separator := lower_expr(ctx, entry.separator)
		if separator != INVALID_VALUE_ID {
			append(inputs, separator)
		}
	}
}

lower_split_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Split_Stmt, source: Source_Loc) {
	for entry in stmt.entries {
		source_value := lower_expr(ctx, entry.source)
		separator := lower_expr(ctx, entry.separator)
		if entry.into_table {
			for target in entry.targets {
				lower_expr(ctx, target)
			}
			builder_emit_unsupported(ctx.builder, "SPLIT INTO TABLE semantics", source = source)
			continue
		}
		if len(entry.targets) == 0 {
			builder_emit_unsupported(ctx.builder, "SPLIT target semantics", source = source)
			continue
		}
		if source_value == INVALID_VALUE_ID || separator == INVALID_VALUE_ID {
			continue
		}
		inputs := [?]Value_Id{source_value, separator}
		result_types := make([dynamic]Type_Id, 0, len(entry.targets), context.temp_allocator)
		defer delete(result_types)
		for target in entry.targets {
			append(&result_types, lower_known_result_type(lower_type_for_expr(ctx, target)))
		}
		op_id := builder_emit_intrinsic(ctx.builder, .ABAP_Split, inputs[:], result_types[:], source = source)
		op := op_ptr(builder_function(ctx.builder), op_id)
		for target, i in entry.targets {
			lower_store_expr(ctx, target, op.results[i])
		}
	}
}

lower_condense_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Condense_Stmt, source: Source_Loc) {
	if stmt.target == nil {
		builder_emit_unsupported(ctx.builder, "CONDENSE target semantics", source = source)
		return
	}
	value := lower_expr(ctx, stmt.target)
	if value == INVALID_VALUE_ID {
		return
	}
	inputs := [?]Value_Id{value}
	result := lower_emit_value_op(
		ctx,
		.ABAP_Condense,
		inputs[:],
		lower_type_for_expr(ctx, stmt.target),
		source,
		Intrinsic_String_Payload{no_gaps = stmt.no_gaps},
	)
	lower_store_expr(ctx, stmt.target, result)
}

lower_replace_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Replace_Stmt, source: Source_Loc) {
	target := lower_expr(ctx, stmt.target)
	pattern := lower_expr(ctx, stmt.pattern)
	replacement := lower_expr(ctx, stmt.replacement)
	lower_expr(ctx, stmt.section_offset)
	lower_expr(ctx, stmt.section_length)
	if stmt.regex {
		builder_emit_unsupported(ctx.builder, "REPLACE REGEX semantics", source = source)
		return
	}
	if stmt.in_table {
		builder_emit_unsupported(ctx.builder, "REPLACE IN TABLE semantics", source = source)
		return
	}
	if stmt.section_offset != nil || stmt.section_length != nil {
		builder_emit_unsupported(ctx.builder, "REPLACE SECTION semantics", source = source)
		return
	}
	occurrence, occurrence_ok := lower_replace_occurrence(stmt.occurrence)
	if !occurrence_ok || target == INVALID_VALUE_ID || pattern == INVALID_VALUE_ID || replacement == INVALID_VALUE_ID {
		builder_emit_unsupported(ctx.builder, "REPLACE operand semantics", source = source)
		return
	}
	inputs := [?]Value_Id{target, pattern, replacement}
	result := lower_emit_value_op(
		ctx,
		.ABAP_Replace,
		inputs[:],
		lower_type_for_expr(ctx, stmt.target),
		source,
		Intrinsic_String_Payload{replace_occurrence = occurrence},
	)
	lower_store_expr(ctx, stmt.target, result)
}

lower_replace_occurrence :: proc "contextless" (occurrence: ast.Replace_Occurrence) -> (Abap_Replace_Occurrence, bool) {
	switch occurrence {
	case .Default, .First:
		return .First, true
	case .All:
		return .All, true
	}
	return .Unknown, false
}

lower_translate_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Translate_Stmt, source: Source_Loc) {
	if stmt.target == nil {
		builder_emit_unsupported(ctx.builder, "TRANSLATE target semantics", source = source)
		return
	}
	value := lower_expr(ctx, stmt.target)
	if stmt.operand != nil {
		lower_expr(ctx, stmt.operand)
	}
	mode, supported := lower_translate_mode(stmt.form)
	if !supported {
		builder_emit_unsupported(ctx.builder, "TRANSLATE form semantics", source = source)
		return
	}
	if value == INVALID_VALUE_ID {
		return
	}
	inputs := [?]Value_Id{value}
	result := lower_emit_value_op(
		ctx,
		.ABAP_Translate,
		inputs[:],
		lower_type_for_expr(ctx, stmt.target),
		source,
		Intrinsic_String_Payload{translate_mode = mode},
	)
	lower_store_expr(ctx, stmt.target, result)
}

lower_translate_mode :: proc "contextless" (form: ast.Translate_Form) -> (Abap_Translate_Mode, bool) {
	switch form {
	case .To_Upper:
		return .To_Upper, true
	case .To_Lower:
		return .To_Lower, true
	case .Default,
	     .Using,
	     .To_Code_Page,
	     .From_Code_Page,
	     .To_Number_Format,
	     .From_Number_Format:
	}
	return .Unknown, false
}

lower_shift_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Shift_Stmt, source: Source_Loc) {
	target := lower_expr(ctx, stmt.target)
	places := lower_expr(ctx, stmt.places)
	lower_expr(ctx, stmt.up_to)
	lower_expr(ctx, stmt.delete_pattern)
	if stmt.circular {
		builder_emit_unsupported(ctx.builder, "SHIFT CIRCULAR semantics", source = source)
		return
	}
	if stmt.up_to != nil {
		builder_emit_unsupported(ctx.builder, "SHIFT UP TO semantics", source = source)
		return
	}
	if stmt.delete_direction != .None || stmt.delete_pattern != nil {
		builder_emit_unsupported(ctx.builder, "SHIFT DELETING semantics", source = source)
		return
	}
	direction, direction_ok := lower_shift_direction(stmt.direction)
	if !direction_ok || target == INVALID_VALUE_ID || (stmt.places != nil && places == INVALID_VALUE_ID) {
		builder_emit_unsupported(ctx.builder, "SHIFT operand semantics", source = source)
		return
	}
	inputs := make([dynamic]Value_Id, 0, 2, context.temp_allocator)
	defer delete(inputs)
	append(&inputs, target)
	if stmt.places != nil {
		append(&inputs, places)
	}
	result := lower_emit_value_op(
		ctx,
		.ABAP_Shift,
		inputs[:],
		lower_type_for_expr(ctx, stmt.target),
		source,
		Intrinsic_String_Payload{shift_direction = direction},
	)
	lower_store_expr(ctx, stmt.target, result)
}

lower_shift_direction :: proc "contextless" (direction: ast.Shift_Direction) -> (Abap_Shift_Direction, bool) {
	switch direction {
	case .Default, .Left:
		return .Left, true
	case .Right:
		return .Right, true
	}
	return .Unknown, false
}

lower_find_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Find_Stmt, source: Source_Loc) {
	pattern := lower_expr(ctx, stmt.pattern)
	target := lower_expr(ctx, stmt.target)
	if stmt.regex {
		lower_find_match_outputs(ctx, stmt)
		builder_emit_unsupported(ctx.builder, "FIND REGEX semantics", source = source)
		return
	}
	if stmt.in_table {
		lower_expr(ctx, stmt.match_line)
		lower_find_match_outputs(ctx, stmt)
		builder_emit_unsupported(ctx.builder, "FIND IN TABLE semantics", source = source)
		return
	}
	if stmt.section_offset != nil || stmt.section_length != nil {
		lower_expr(ctx, stmt.section_offset)
		lower_expr(ctx, stmt.section_length)
		lower_find_match_outputs(ctx, stmt)
		builder_emit_unsupported(ctx.builder, "FIND SECTION semantics", source = source)
		return
	}
	if stmt.match_line != nil {
		lower_expr(ctx, stmt.match_line)
		lower_find_match_outputs(ctx, stmt)
		builder_emit_unsupported(ctx.builder, "FIND MATCH LINE semantics", source = source)
		return
	}
	if stmt.results != nil {
		lower_find_match_outputs(ctx, stmt)
		builder_emit_unsupported(ctx.builder, "FIND RESULTS semantics", source = source)
		return
	}
	if len(stmt.submatches) > 0 {
		lower_find_match_outputs(ctx, stmt)
		builder_emit_unsupported(ctx.builder, "FIND SUBMATCHES semantics", source = source)
		return
	}
	occurrence, occurrence_ok := lower_find_occurrence(stmt.occurrence)
	if !occurrence_ok || pattern == INVALID_VALUE_ID || target == INVALID_VALUE_ID {
		lower_find_match_outputs(ctx, stmt)
		builder_emit_unsupported(ctx.builder, "FIND operand semantics", source = source)
		return
	}
	inputs := [?]Value_Id{pattern, target}
	result_types := [?]Type_Id{
		BUILTIN_TYPE_INTEGER,
		BUILTIN_TYPE_INTEGER,
		BUILTIN_TYPE_INTEGER,
		BUILTIN_TYPE_INTEGER,
	}
	op_id := builder_emit_intrinsic(
		ctx.builder,
		.ABAP_Find,
		inputs[:],
		result_types[:],
		payload = Intrinsic_String_Payload {
			find_occurrence = occurrence,
			find_ignoring_case = stmt.case_mode == .Ignoring,
		},
		source = source,
	)
	op := op_ptr(builder_function(ctx.builder), op_id)
	builder_emit_system_write(ctx.builder, "subrc", op.results[0], source)
	lower_store_expr(ctx, stmt.match_offset, op.results[1])
	lower_store_expr(ctx, stmt.match_length, op.results[2])
	lower_store_expr(ctx, stmt.match_count, op.results[3])
}

lower_search_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Search_Stmt, source: Source_Loc) {
	target := lower_expr(ctx, stmt.target)
	pattern := lower_expr(ctx, stmt.pattern)
	if stmt.starting_at != nil || stmt.ending_at != nil {
		lower_expr(ctx, stmt.starting_at)
		lower_expr(ctx, stmt.ending_at)
		builder_emit_unsupported(ctx.builder, "SEARCH range semantics", source = source)
		return
	}
	if stmt.abbreviated {
		builder_emit_unsupported(ctx.builder, "SEARCH ABBREVIATED semantics", source = source)
		return
	}
	if stmt.mark {
		builder_emit_unsupported(ctx.builder, "SEARCH MARK semantics", source = source)
		return
	}
	if target == INVALID_VALUE_ID || pattern == INVALID_VALUE_ID {
		builder_emit_unsupported(ctx.builder, "SEARCH operand semantics", source = source)
		return
	}
	inputs := [?]Value_Id{target, pattern}
	result_types := [?]Type_Id{BUILTIN_TYPE_INTEGER, BUILTIN_TYPE_INTEGER}
	op_id := builder_emit_intrinsic(ctx.builder, .ABAP_Search, inputs[:], result_types[:], source = source)
	op := op_ptr(builder_function(ctx.builder), op_id)
	builder_emit_system_write(ctx.builder, "subrc", op.results[0], source)
	builder_emit_system_write(ctx.builder, "fdpos", op.results[1], source)
}

lower_find_match_outputs :: proc(ctx: ^Lower_Context, stmt: ^ast.Find_Stmt) {
	lower_expr(ctx, stmt.match_offset)
	lower_expr(ctx, stmt.match_length)
	lower_expr(ctx, stmt.match_count)
	lower_expr(ctx, stmt.results)
	for submatch in stmt.submatches {
		lower_expr(ctx, submatch)
	}
}

lower_find_occurrence :: proc "contextless" (occurrence: ast.Find_Occurrence) -> (Abap_Find_Occurrence, bool) {
	switch occurrence {
	case .Default, .First:
		return .First, true
	case .All:
		return .All, true
	}
	return .Unknown, false
}

lower_write_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Write_Stmt, source: Source_Loc) {
	inputs := make([dynamic]Value_Id, 0, len(stmt.operands), context.temp_allocator)
	defer delete(inputs)
	append_input := proc(inputs: ^[dynamic]Value_Id, value: Value_Id) {
		if value != INVALID_VALUE_ID {
			append(inputs, value)
		}
	}
	for operand in stmt.operands {
		append_input(&inputs, lower_expr(ctx, operand.position))
		append_input(&inputs, lower_expr(ctx, operand.length))
		append_input(&inputs, lower_expr(ctx, operand.value))
	}
	builder_emit_write(ctx.builder, inputs[:], source)
}

lower_create_object_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Create_Object_Stmt, source: Source_Loc) {
	target_type := lower_type_for_expr(ctx, stmt.target)
	constructor_type := lower_create_object_constructor_type(ctx, stmt)
	construct_type := lower_create_object_construct_type(ctx, constructor_type, target_type)
	inputs := make([dynamic]Value_Id, 0, len(stmt.operands), context.temp_allocator)
	defer delete(inputs)
	if stmt.type_dynamic_expr != nil {
		value := lower_expr(ctx, stmt.type_dynamic_expr)
		if value != INVALID_VALUE_ID {
			append(&inputs, value)
		}
	}
	constructor_args := make([dynamic]Value_Id, 0, len(stmt.operands), context.temp_allocator)
	defer delete(constructor_args)
	lower_constructor_collect_args(ctx, stmt.operands[:], &constructor_args)
	for arg in constructor_args {
		append(&inputs, arg)
	}
	created := lower_emit_value_op(
		ctx,
		.ABAP_Construct,
		inputs[:],
		construct_type,
		source,
		Intrinsic_Call_Payload{callee_name = "new"},
	)
	if constructor_type != nil && !stmt.type_dynamic {
		lower_emit_constructor_call_for_type(ctx, constructor_type, created, constructor_args[:], source)
	}
	lower_store_expr(ctx, stmt.target, created)
}

lower_create_data_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Create_Data_Stmt, source: Source_Loc) {
	if stmt.type_dynamic || stmt.type_dynamic_expr != nil || stmt.type_handle != nil || len(stmt.operands) > 0 {
		lower_expr(ctx, stmt.type_dynamic_expr)
		lower_expr(ctx, stmt.type_handle)
		lower_constructor_collect_args(ctx, stmt.operands[:], nil)
		builder_emit_unsupported(ctx.builder, "CREATE DATA dynamic or constructor semantics", source = source)
		return
	}
	if stmt.type_ref != nil || stmt.type_clause != nil {
		lower_expr(ctx, stmt.type_ref)
		builder_emit_unsupported(ctx.builder, "CREATE DATA explicit type semantics", source = source)
		return
	}
	created := lower_emit_value_op(
		ctx,
		.ABAP_Construct,
		nil,
		lower_type_for_expr(ctx, stmt.target),
		source,
		Intrinsic_Call_Payload{callee_name = "create_data"},
	)
	lower_store_expr(ctx, stmt.target, created)
}

lower_create_object_constructor_type :: proc(
	ctx: ^Lower_Context,
	stmt: ^ast.Create_Object_Stmt,
) -> ^semantic.Type {
	if stmt == nil {
		return nil
	}
	if !stmt.type_dynamic {
		if stmt.type_clause != nil && stmt.type_clause.type_ref != nil {
			if info, ok := lower_expr_info_for_node(ctx, &stmt.type_clause.type_ref.expr_base); ok {
				return info.type
			}
		}
		if stmt.type_ref != nil {
			if info, ok := lower_expr_info_for_node(ctx, &stmt.type_ref.expr_base); ok {
				return info.type
			}
		}
	}
	if stmt.target != nil {
		if info, ok := lower_expr_info_for_node(ctx, &stmt.target.expr_base); ok {
			return info.type
		}
	}
	return nil
}

lower_create_object_construct_type :: proc(
	ctx: ^Lower_Context,
	constructor_type: ^semantic.Type,
	fallback: Type_Id,
) -> Type_Id {
	if constructor_type == nil || constructor_type.kind == .Unknown {
		return fallback
	}
	if constructor_type.kind == .Ref {
		return module_type_from_semantic(ctx.module, constructor_type)
	}
	if semantic.checker_type_object_entity(constructor_type) != nil {
		return module_type_from_semantic(ctx.module, semantic.project_type_ref(ctx.project, constructor_type))
	}
	return fallback
}

lower_if_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.If_Stmt) {
	source := source_loc_from_node(ctx.file, &stmt.node.stmt_base)
	condition := lower_expr(ctx, stmt.condition)
	then_block := builder_add_world_block(ctx.builder, "if_then", source)
	else_block := builder_add_world_block(ctx.builder, "if_else", source)
	after_block := builder_add_world_block(ctx.builder, "if_after", source)
	true_args := [?]Value_Id{ctx.builder.current_world}
	false_args := [?]Value_Id{ctx.builder.current_world}
	builder_set_cond_branch(
		ctx.builder,
		condition,
		then_block,
		true_args[:],
		else_block,
		false_args[:],
		source,
	)

	builder_position_at_end(ctx.builder, then_block)
	lower_stmt_list(ctx, stmt.body[:])
	if lower_current_block_open(ctx) {
		builder_set_branch_world(ctx.builder, after_block, source)
	}

	builder_position_at_end(ctx.builder, else_block)
	lower_lower_else_chain(ctx, stmt.elseif_clauses[:], stmt.else_clause, after_block, source)

	builder_position_at_end(ctx.builder, after_block)
}

lower_lower_else_chain :: proc(
	ctx: ^Lower_Context,
	clauses: []^ast.Elseif_Clause,
	else_clause: ^ast.Else_Clause,
	after_block: Block_Id,
	source: Source_Loc,
) {
	if len(clauses) == 0 {
		if else_clause != nil {
			lower_stmt_list(ctx, else_clause.body[:])
		}
		if lower_current_block_open(ctx) {
			builder_set_branch_world(ctx.builder, after_block, source)
		}
		return
	}
	clause := clauses[0]
	condition := lower_expr(ctx, clause.condition)
	then_block := builder_add_world_block(ctx.builder, "elseif_then", source)
	next_block := builder_add_world_block(ctx.builder, "elseif_next", source)
	true_args := [?]Value_Id{ctx.builder.current_world}
	false_args := [?]Value_Id{ctx.builder.current_world}
	builder_set_cond_branch(
		ctx.builder,
		condition,
		then_block,
		true_args[:],
		next_block,
		false_args[:],
		source,
	)
	builder_position_at_end(ctx.builder, then_block)
	lower_stmt_list(ctx, clause.body[:])
	if lower_current_block_open(ctx) {
		builder_set_branch_world(ctx.builder, after_block, source)
	}
	builder_position_at_end(ctx.builder, next_block)
	lower_lower_else_chain(ctx, clauses[1:], else_clause, after_block, source)
}

lower_case_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Case_Stmt) {
	source := source_loc_from_node(ctx.file, &stmt.node.stmt_base)
	subject := lower_expr(ctx, stmt.expr)
	if stmt.is_type_of {
		builder_emit_unsupported(ctx.builder, "CASE TYPE OF semantics", source = source)
		return
	}
	assert(subject != INVALID_VALUE_ID)
	after_block := builder_add_world_block(ctx.builder, "case_after", source)
	lower_case_when_chain(ctx, subject, stmt.whens[:], after_block, source)
	builder_position_at_end(ctx.builder, after_block)
}

lower_case_when_chain :: proc(
	ctx: ^Lower_Context,
	subject: Value_Id,
	whens: []^ast.When_Clause,
	after_block: Block_Id,
	source: Source_Loc,
) {
	if len(whens) == 0 {
		builder_set_branch_world(ctx.builder, after_block, source)
		return
	}
	clause := whens[0]
	clause_source := lower_when_clause_source(ctx, clause, source)
	body_block := builder_add_world_block(ctx.builder, "case_when", clause_source)
	if clause.is_others {
		builder_set_branch_world(ctx.builder, body_block, clause_source)
		builder_position_at_end(ctx.builder, body_block)
		lower_stmt_list(ctx, clause.body[:])
		if lower_current_block_open(ctx) {
			builder_set_branch_world(ctx.builder, after_block, clause_source)
		}
		return
	}

	next_block := builder_add_world_block(ctx.builder, "case_next", clause_source)
	condition := lower_case_when_condition(ctx, subject, clause, clause_source)
	true_args := [?]Value_Id{ctx.builder.current_world}
	false_args := [?]Value_Id{ctx.builder.current_world}
	builder_set_cond_branch(
		ctx.builder,
		condition,
		body_block,
		true_args[:],
		next_block,
		false_args[:],
		clause_source,
	)

	builder_position_at_end(ctx.builder, body_block)
	lower_stmt_list(ctx, clause.body[:])
	if lower_current_block_open(ctx) {
		builder_set_branch_world(ctx.builder, after_block, clause_source)
	}

	builder_position_at_end(ctx.builder, next_block)
	lower_case_when_chain(ctx, subject, whens[1:], after_block, source)
}

lower_case_when_condition :: proc(
	ctx: ^Lower_Context,
	subject: Value_Id,
	clause: ^ast.When_Clause,
	source: Source_Loc,
) -> Value_Id {
	condition := INVALID_VALUE_ID
	for operand in clause.operands {
		value := lower_expr(ctx, operand)
		if value == INVALID_VALUE_ID {
			continue
		}
		inputs := [?]Value_Id{subject, value}
		equals := lower_emit_value_op(ctx, .ABAP_Equal, inputs[:], BUILTIN_TYPE_PREDICATE, source)
		if condition == INVALID_VALUE_ID {
			condition = equals
			continue
		}
		or_inputs := [?]Value_Id{condition, equals}
		condition = lower_emit_value_op(ctx, .ABAP_Or, or_inputs[:], BUILTIN_TYPE_PREDICATE, source)
	}
	if condition == INVALID_VALUE_ID {
		return builder_emit_unsupported(ctx.builder, "CASE WHEN condition", BUILTIN_TYPE_PREDICATE, source)
	}
	return condition
}

lower_when_clause_source :: proc(
	ctx: ^Lower_Context,
	clause: ^ast.When_Clause,
	fallback: Source_Loc,
) -> Source_Loc {
	if clause == nil || clause.range.end <= clause.range.start {
		return fallback
	}
	return Source_Loc{file = ctx.file, range = clause.range}
}

lower_while_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.While_Stmt) {
	source := source_loc_from_node(ctx.file, &stmt.node.stmt_base)
	cond_block := builder_add_world_block(ctx.builder, "while_cond", source)
	body_block := builder_add_world_block(ctx.builder, "while_body", source)
	after_block := builder_add_world_block(ctx.builder, "while_after", source)
	builder_set_branch_world(ctx.builder, cond_block, source)

	builder_position_at_end(ctx.builder, cond_block)
	condition := lower_expr(ctx, stmt.condition)
	true_args := [?]Value_Id{ctx.builder.current_world}
	false_args := [?]Value_Id{ctx.builder.current_world}
	builder_set_cond_branch(
		ctx.builder,
		condition,
		body_block,
		true_args[:],
		after_block,
		false_args[:],
		source,
	)

	builder_position_at_end(ctx.builder, body_block)
	lower_push_control_target(
		ctx,
		Lower_Control_Target {
			kind = .Loop,
			continue_block = cond_block,
			continue_arg = INVALID_VALUE_ID,
			exit_block = after_block,
		},
	)
	lower_stmt_list(ctx, stmt.body[:])
	if lower_current_block_open(ctx) {
		builder_set_branch_world(ctx.builder, cond_block, source)
	}
	lower_pop_control_target(ctx)

	builder_position_at_end(ctx.builder, after_block)
}

lower_do_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Do_Stmt) {
	source := source_loc_from_node(ctx.file, &stmt.node.stmt_base)
	body_block := builder_add_world_block(ctx.builder, "do_body", source)
	after_block := builder_add_world_block(ctx.builder, "do_after", source)
	continue_block := body_block
	if stmt.count != nil {
		lower_expr(ctx, stmt.count)
		cond_block := builder_add_world_block(ctx.builder, "do_cond", source)
		continue_block = cond_block
		builder_set_branch_world(ctx.builder, cond_block, source)

		builder_position_at_end(ctx.builder, cond_block)
		condition := builder_emit_unsupported(ctx.builder, "DO loop count semantics", BUILTIN_TYPE_PREDICATE, source)
		true_args := [?]Value_Id{ctx.builder.current_world}
		false_args := [?]Value_Id{ctx.builder.current_world}
		builder_set_cond_branch(
			ctx.builder,
			condition,
			body_block,
			true_args[:],
			after_block,
			false_args[:],
			source,
		)
	} else {
		builder_set_branch_world(ctx.builder, body_block, source)
	}
	builder_position_at_end(ctx.builder, body_block)
	lower_push_control_target(
		ctx,
		Lower_Control_Target {
			kind = .Loop,
			continue_block = continue_block,
			continue_arg = INVALID_VALUE_ID,
			exit_block = after_block,
		},
	)
	lower_stmt_list(ctx, stmt.body[:])
	if lower_current_block_open(ctx) {
		builder_set_branch_world(ctx.builder, continue_block, source)
	}
	lower_pop_control_target(ctx)
	builder_position_at_end(ctx.builder, after_block)
}

lower_loop_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Loop_Stmt) {
	source := source_loc_from_node(ctx.file, &stmt.node.stmt_base)
	source_value := lower_expr(ctx, stmt.source)
	assert(source_value != INVALID_VALUE_ID)
	lower_loop_header_deferred_semantics(ctx, stmt, source)
	row_type := lower_table_row_type_for_expr(ctx, stmt.source)
	row_result_type := lower_loop_result_type(ctx, stmt, row_type)
	filter_inputs := make([dynamic]Value_Id, 0, 2, context.temp_allocator)
	defer delete(filter_inputs)
	filter_components := make([dynamic]Intrinsic_Table_Component, 0, 2, context.temp_allocator)
	defer lower_table_components_destroy_scratch(&filter_components)
	filter_count := 0
	if stmt.where_cond != nil {
		if count, ok := lower_table_condition_inputs(ctx, stmt.where_cond, &filter_inputs, &filter_components, 1); ok {
			filter_count = count
		} else {
			lower_expr(ctx, stmt.where_cond)
			builder_emit_unsupported(ctx.builder, "LOOP WHERE filtering", source = source)
		}
	}
	table_payload := Intrinsic_Table_Payload {
		access = .Sequential,
		result_kind = lower_loop_result_kind(stmt),
		source_kind = .Row,
		component_count = filter_count,
		components = filter_components,
	}
	iter := builder_emit_table_iter(ctx.builder, source_value, row_type, table_payload, source, filter_inputs[:])
	iter_type := value_type(builder_function(ctx.builder), iter)

	next_block := builder_add_world_block(ctx.builder, "loop_next", source)
	function_add_block_param(builder_function(ctx.builder), next_block, iter_type, "iter")
	body_block := builder_add_world_block(ctx.builder, "loop_body", source)
	function_add_block_param(builder_function(ctx.builder), body_block, iter_type, "iter")
	row_param := function_add_block_param(builder_function(ctx.builder), body_block, row_result_type, "row")
	after_block := builder_add_world_block(ctx.builder, "loop_after", source)
	next_args := [?]Value_Id{ctx.builder.current_world, iter}
	builder_set_branch(ctx.builder, next_block, next_args[:], source)

	builder_position_at_end(ctx.builder, next_block)
	next_iter := block_ptr(builder_function(ctx.builder), next_block).args[1]
	has_row, row, tabix, subrc := builder_emit_table_next(ctx.builder, next_iter, row_result_type, row_type, table_payload, source)
	builder_emit_system_write(ctx.builder, "subrc", subrc, source)
	builder_emit_system_write(ctx.builder, "tabix", tabix, source)
	body_args := [?]Value_Id{ctx.builder.current_world, next_iter, row}
	after_args := [?]Value_Id{ctx.builder.current_world}
	builder_set_cond_branch(
		ctx.builder,
		has_row,
		body_block,
		body_args[:],
		after_block,
		after_args[:],
		source,
	)

	builder_position_at_end(ctx.builder, body_block)
	body_iter := block_ptr(builder_function(ctx.builder), body_block).args[1]
	lower_push_control_target(
		ctx,
		Lower_Control_Target {
			kind = .Loop,
			continue_block = next_block,
			continue_arg = body_iter,
			exit_block = after_block,
		},
	)
	if stmt.target != nil {
		lower_store_expr(ctx, stmt.target, row_param)
	}
	lower_stmt_list(ctx, stmt.body[:])
	if lower_current_block_open(ctx) {
		lower_branch_to_control_continue(ctx, source)
	}
	lower_pop_control_target(ctx)
	builder_position_at_end(ctx.builder, after_block)
}

lower_check_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Check_Stmt) {
	source := source_loc_from_node(ctx.file, &stmt.node.stmt_base)
	condition := lower_expr(ctx, stmt.condition)
	_, has_control_target := lower_current_control_target(ctx)
	cont := builder_add_world_block(ctx.builder, "check_continue", source)
	fail_name := "check_control" if has_control_target else "check_return"
	fail := builder_add_world_block(ctx.builder, fail_name, source)
	true_args := [?]Value_Id{ctx.builder.current_world}
	false_args := [?]Value_Id{ctx.builder.current_world}
	builder_set_cond_branch(
		ctx.builder,
		condition,
		cont,
		true_args[:],
		fail,
		false_args[:],
		source,
	)
	builder_position_at_end(ctx.builder, fail)
	if has_control_target {
		lower_branch_to_control_continue(ctx, source)
	} else {
		builder_set_return_world(ctx.builder, source)
	}
	builder_position_at_end(ctx.builder, cont)
}

lower_flow_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Flow_Stmt) {
	source := source_loc_from_node(ctx.file, &stmt.node.stmt_base)
	#partial switch stmt.kind {
	case .Return, .Stop, .Leave_List_Processing:
		builder_set_return_world(ctx.builder, source)
	case .Exit:
		if _, ok := lower_current_control_target(ctx); ok {
			lower_branch_to_control_exit(ctx, source)
		} else {
			builder_set_return_world(ctx.builder, source)
		}
	case .Continue:
		if _, ok := lower_current_control_target(ctx); ok {
			lower_branch_to_control_continue(ctx, source)
		} else {
			builder_emit_unsupported(ctx.builder, "CONTINUE outside structured control target", source = source)
		}
	}
}

lower_prepare_invoke_targets :: proc(
	ctx: ^Lower_Context,
	source: Source_Loc,
) -> (
	normal_block: Block_Id,
	exception_block: Block_Id,
	uses_unhandled_block: bool,
) {
	normal_block = builder_add_world_block(ctx.builder, "invoke_normal", source)
	if target, ok := lower_current_exception_target(ctx); ok {
		exception_block = target.dispatch_block
		return
	}
	exception_block = builder_add_world_block(ctx.builder, "invoke_unhandled", source)
	uses_unhandled_block = true
	return
}

lower_finish_invoke_targets :: proc(
	ctx: ^Lower_Context,
	normal_block: Block_Id,
	exception_block: Block_Id,
	uses_unhandled_block: bool,
	close_normal: bool,
	source: Source_Loc,
) {
	builder_set_unreachable(ctx.builder, source)
	if uses_unhandled_block {
		builder_position_at_end(ctx.builder, exception_block)
		builder_emit_exception_unhandled(ctx.builder, source)
		builder_set_return_world(ctx.builder, source)
	}
	builder_position_at_end(ctx.builder, normal_block)
	if close_normal {
		builder_set_unreachable(ctx.builder, source)
	}
}

lower_emit_method_may_throw :: proc(
	ctx: ^Lower_Context,
	target: ^semantic.Entity,
	callee_name: string,
	inputs: []Value_Id = nil,
	result_types: []Type_Id = nil,
	source: Source_Loc = {},
) -> Op_Id {
	normal_block, exception_block, uses_unhandled_block := lower_prepare_invoke_targets(ctx, source)
	op_id := builder_emit_method_invoke(
		ctx.builder,
		target,
		callee_name,
		normal_block,
		exception_block,
			inputs,
			result_types,
			source = source,
		)
	lower_finish_invoke_targets(ctx, normal_block, exception_block, uses_unhandled_block, false, source)
	return op_id
}

lower_emit_routine_may_throw :: proc(
	ctx: ^Lower_Context,
	target: ^semantic.Entity,
	callee_name: string,
	inputs: []Value_Id = nil,
	result_types: []Type_Id = nil,
	source: Source_Loc = {},
	call_kind: Abap_Call_Kind = .Unknown,
) -> Op_Id {
	normal_block, exception_block, uses_unhandled_block := lower_prepare_invoke_targets(ctx, source)
	op_id := builder_emit_routine_invoke(
		ctx.builder,
		target,
		callee_name,
		normal_block,
		exception_block,
			inputs,
			result_types,
			source = source,
			call_kind = call_kind,
		)
	lower_finish_invoke_targets(ctx, normal_block, exception_block, uses_unhandled_block, false, source)
	return op_id
}

lower_raise_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Raise_Stmt, source: Source_Loc) {
	if stmt.kind == .Event {
		for arg in stmt.named_args {
			lower_expr(ctx, arg.value)
		}
		builder_emit_unsupported(ctx.builder, "RAISE EVENT semantics", source = source)
		return
	}
	if !stmt.target_type {
		if stmt.target != nil {
			lower_expr(ctx, stmt.target)
		}
		for arg in stmt.named_args {
			lower_expr(ctx, arg.value)
		}
		builder_emit_unsupported(ctx.builder, "RAISE EXCEPTION object semantics", source = source)
		return
	}
	if len(stmt.named_args) > 0 {
		for arg in stmt.named_args {
			lower_expr(ctx, arg.value)
		}
		builder_emit_unsupported(ctx.builder, "RAISE EXCEPTION constructor operands", source = source)
		return
	}
	exception_name, exception_ok := lower_exception_static_name(ctx, stmt.target)
	if !exception_ok {
		builder_emit_unsupported(ctx.builder, "RAISE EXCEPTION dynamic type semantics", source = source)
		return
	}
	normal_block, exception_block, uses_unhandled_block := lower_prepare_invoke_targets(ctx, source)
	builder_emit_exception_raise_invoke(ctx.builder, exception_name, normal_block, exception_block, source)
	lower_finish_invoke_targets(ctx, normal_block, exception_block, uses_unhandled_block, true, source)
}

lower_try_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Try_Stmt, source: Source_Loc) {
	if stmt.cleanup != nil {
		builder_emit_unsupported(ctx.builder, "TRY CLEANUP semantics", source = source)
		return
	}
	if len(stmt.catches) == 0 {
		lower_stmt_list(ctx, stmt.body[:])
		return
	}

	dispatch_block := builder_add_world_block(ctx.builder, "try_exception", source)
	after_block := builder_add_world_block(ctx.builder, "try_after", source)

	lower_push_exception_target(ctx, Lower_Exception_Target{dispatch_block = dispatch_block})
	lower_stmt_list(ctx, stmt.body[:])
	lower_pop_exception_target(ctx)
	if lower_current_block_open(ctx) {
		builder_set_branch_world(ctx.builder, after_block, source)
	}

	builder_position_at_end(ctx.builder, dispatch_block)
	lower_try_catch_chain(ctx, stmt, 0, after_block, source)

	builder_position_at_end(ctx.builder, after_block)
}

lower_try_catch_chain :: proc(
	ctx: ^Lower_Context,
	stmt: ^ast.Try_Stmt,
	catch_index: int,
	after_block: Block_Id,
	source: Source_Loc,
) {
	if catch_index >= len(stmt.catches) {
		if target, ok := lower_current_exception_target(ctx); ok {
			builder_set_branch_world(ctx.builder, target.dispatch_block, source)
			return
		}
		builder_emit_exception_unhandled(ctx.builder, source)
		builder_set_return_world(ctx.builder, source)
		return
	}

	clause := stmt.catches[catch_index]
	clause_source := lower_catch_clause_source(ctx, clause, source)
	condition := lower_catch_condition(ctx, clause, clause_source)
	catch_block := builder_add_world_block(ctx.builder, "try_catch", clause_source)
	next_block := builder_add_world_block(ctx.builder, "try_catch_next", clause_source)
	true_args := [?]Value_Id{ctx.builder.current_world}
	false_args := [?]Value_Id{ctx.builder.current_world}
	builder_set_cond_branch(
		ctx.builder,
		condition,
		catch_block,
		true_args[:],
		next_block,
		false_args[:],
		clause_source,
	)

	builder_position_at_end(ctx.builder, catch_block)
	if clause.into != nil {
		exception := builder_emit_exception_catch(ctx.builder, lower_type_for_expr(ctx, clause.into), clause_source)
		if exception != INVALID_VALUE_ID {
			lower_store_expr(ctx, clause.into, exception)
		}
	} else {
		builder_emit_exception_catch(ctx.builder, source = clause_source)
	}
	lower_stmt_list(ctx, clause.body[:])
	if lower_current_block_open(ctx) {
		builder_set_branch_world(ctx.builder, after_block, clause_source)
	}

	builder_position_at_end(ctx.builder, next_block)
	lower_try_catch_chain(ctx, stmt, catch_index + 1, after_block, source)
}

lower_catch_condition :: proc(ctx: ^Lower_Context, clause: ^ast.Catch_Clause, source: Source_Loc) -> Value_Id {
	condition := INVALID_VALUE_ID
	for exception in clause.exceptions {
		name, name_ok := lower_exception_static_name(ctx, exception)
		if !name_ok {
			return builder_emit_unsupported(ctx.builder, "CATCH exception type semantics", BUILTIN_TYPE_PREDICATE, source)
		}
		match := builder_emit_exception_match(ctx.builder, name, source)
		if condition == INVALID_VALUE_ID {
			condition = match
			continue
		}
		inputs := [?]Value_Id{condition, match}
		condition = lower_emit_value_op(ctx, .ABAP_Or, inputs[:], BUILTIN_TYPE_PREDICATE, source)
	}
	if condition == INVALID_VALUE_ID {
		return builder_emit_unsupported(ctx.builder, "CATCH exception type semantics", BUILTIN_TYPE_PREDICATE, source)
	}
	return condition
}

lower_catch_clause_source :: proc(ctx: ^Lower_Context, clause: ^ast.Catch_Clause, fallback: Source_Loc) -> Source_Loc {
	if clause == nil || clause.range.end <= clause.range.start {
		return fallback
	}
	return Source_Loc{file = ctx.file, range = clause.range}
}

lower_exception_static_name :: proc(ctx: ^Lower_Context, expr: ^ast.Expr) -> (string, bool) {
	if expr == nil {
		return "", false
	}
	if entity := lower_entity_for_node(ctx, &expr.expr_base); entity != nil && lower_entity_is_exception_type(entity) {
		return entity.name, entity.name != ""
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		return n.name, n.name != ""
	case ^ast.Type_Ref_Expr:
		if n.raw_operand {
			return lower_exception_static_name_from_raw(ctx, n)
		}
		name := n.base_name.text
		if name == "" {
			name = n.name.text
		}
		return name, name != ""
	case ^ast.Paren_Expr:
		return lower_exception_static_name(ctx, n.expr)
	}
	return "", false
}

lower_exception_static_name_from_raw :: proc(ctx: ^Lower_Context, raw: ^ast.Type_Ref_Expr) -> (string, bool) {
	if raw == nil || len(raw.raw_refs) != 1 || len(raw.raw_decls) != 0 {
		return "", false
	}
	ref := raw.raw_refs[0]
	if ref.call_like || ref.type_base || ref.dynamic_path || len(ref.path) != 0 {
		return "", false
	}
	if use := semantic.semantic_ref_use_at_range(ctx.ref_query, ref.name.range); use != nil &&
	   use.entity != nil &&
	   lower_entity_is_exception_type(use.entity) {
		return use.entity.name, use.entity.name != ""
	}
	return ref.name.text, ref.name.text != ""
}

lower_entity_is_exception_type :: proc "contextless" (entity: ^semantic.Entity) -> bool {
	if entity == nil {
		return false
	}
	#partial switch entity.kind {
	case .Class, .Interface, .Type_Def, .Builtin:
		return true
	}
	return false
}

lower_read_table_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Read_Table_Stmt) {
	source := source_loc_from_node(ctx.file, &stmt.node.stmt_base)
	for entry in stmt.entries {
		table := lower_expr(ctx, entry.table)
		inputs := make([dynamic]Value_Id, 0, 2 + len(entry.key_values), context.temp_allocator)
		defer delete(inputs)
		assert(table != INVALID_VALUE_ID)
		append(&inputs, table)
		if entry.index != nil {
			index := lower_expr(ctx, entry.index)
			assert(index != INVALID_VALUE_ID)
			append(&inputs, index)
		}
		components := make([dynamic]Intrinsic_Table_Component, 0, len(entry.key_values), context.temp_allocator)
		defer lower_table_components_destroy_scratch(&components)
		dynamic_key, dynamic_component := lower_read_table_key_inputs(ctx, entry, &inputs, &components)
		if len(entry.transporting_fields) > 0 {
			builder_emit_unsupported(ctx.builder, "READ TABLE TRANSPORTING field semantics", source = source)
		}
		if len(entry.comparing) > 0 {
			builder_emit_unsupported(ctx.builder, "READ TABLE COMPARING semantics", source = source)
		}
		row_type := lower_table_row_type_for_expr(ctx, entry.table)
		row_result_type := lower_read_table_result_type(ctx, entry, row_type)
		row, subrc, tabix := builder_emit_table_read(
			ctx.builder,
			inputs[:],
			row_result_type,
			row_type,
			lower_read_table_payload(entry, components, dynamic_key, dynamic_component),
			source,
		)
		lower_store_expr(ctx, entry.into, row)
		lower_store_expr(ctx, entry.assigning, row)
		lower_store_expr(ctx, entry.reference_into, row)
		builder_emit_system_write(ctx.builder, "subrc", subrc, source)
		builder_emit_system_write(ctx.builder, "tabix", tabix, source)
	}
}

lower_append_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Append_Stmt, source: Source_Loc) {
	if stmt.target == nil {
		builder_emit_unsupported(ctx.builder, "APPEND target semantics", source = source)
		return
	}
	if stmt.lines_of {
		builder_emit_unsupported(ctx.builder, "APPEND source semantics", source = source)
	}
	row_type := lower_table_row_type_for_expr(ctx, stmt.target)
	inputs := make([dynamic]Value_Id, 0, 2, context.temp_allocator)
	defer delete(inputs)
	if stmt.source != nil {
		source_value := lower_expr(ctx, stmt.source)
		if source_value != INVALID_VALUE_ID {
			append(&inputs, source_value)
		}
	} else if stmt.initial_line {
		source_value := lower_emit_effect_value_op(ctx, .ABAP_Clear, nil, row_type, source)
		append(&inputs, source_value)
	} else {
		builder_emit_unsupported(ctx.builder, "APPEND source semantics", source = source)
	}
	target := lower_expr(ctx, stmt.target)
	assert(target != INVALID_VALUE_ID)
	append(&inputs, target)
	if stmt.sorted {
		builder_emit_unsupported(ctx.builder, "APPEND SORTED BY semantics", source = source)
	}
	append_op := builder_emit_table_mutation(
		ctx.builder,
		.Table_Append,
		inputs[:],
		row_type,
		Intrinsic_Table_Payload {
			access = .Full,
			result_kind = lower_append_result_kind(stmt),
			source_kind = lower_append_source_kind(stmt),
		},
		source,
	)
	append_result := op_ptr(builder_function(ctx.builder), append_op)
	subrc := append_result.results[1]
	tabix := append_result.results[2]
	builder_emit_system_write(ctx.builder, "subrc", subrc, source)
	builder_emit_system_write(ctx.builder, "tabix", tabix, source)
	lower_table_result_binding(ctx, target, tabix, row_type, stmt.assigning, stmt.reference_into, lower_append_result_kind(stmt), source)
}

lower_insert_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Insert_Stmt, source: Source_Loc) {
	if stmt.form == .Db_Table {
		lower_sql_insert_stmt(ctx, stmt, source)
		return
	}
	if stmt.target == nil {
		builder_emit_unsupported(ctx.builder, "INSERT target semantics", source = source)
		return
	}
	if stmt.from_table || stmt.form == .Lines_Of {
		builder_emit_unsupported(ctx.builder, "INSERT source semantics", source = source)
	}
	row_type := lower_table_row_type_for_expr(ctx, stmt.target)
	inputs := make([dynamic]Value_Id, 0, 3, context.temp_allocator)
	defer delete(inputs)
	if stmt.source != nil {
		source_value := lower_expr(ctx, stmt.source)
		if source_value != INVALID_VALUE_ID {
			append(&inputs, source_value)
		}
	} else if stmt.initial_line {
		source_value := lower_emit_effect_value_op(ctx, .ABAP_Clear, nil, row_type, source)
		append(&inputs, source_value)
	} else if len(stmt.assignments) > 0 {
		for assignment in stmt.assignments {
			lower_expr(ctx, assignment.name)
			lower_expr(ctx, assignment.value)
		}
		builder_emit_unsupported(ctx.builder, "INSERT assignment semantics", source = source)
	} else {
		builder_emit_unsupported(ctx.builder, "INSERT source semantics", source = source)
	}
	target := lower_expr(ctx, stmt.target)
	assert(target != INVALID_VALUE_ID)
	append(&inputs, target)
	if stmt.index != nil {
		index := lower_expr(ctx, stmt.index)
		if index != INVALID_VALUE_ID {
			append(&inputs, index)
		}
	}
	insert_op := builder_emit_table_mutation(
		ctx.builder,
		.Table_Insert,
		inputs[:],
		row_type,
		Intrinsic_Table_Payload {
			access = Table_Access_Kind.Index if stmt.index != nil else Table_Access_Kind.Full,
			result_kind = lower_insert_result_kind(stmt),
			source_kind = lower_insert_source_kind(stmt),
		},
		source,
	)
	insert_result := op_ptr(builder_function(ctx.builder), insert_op)
	subrc := insert_result.results[1]
	tabix := insert_result.results[2]
	builder_emit_system_write(ctx.builder, "subrc", subrc, source)
	builder_emit_system_write(ctx.builder, "tabix", tabix, source)
	lower_table_result_binding(ctx, target, tabix, row_type, stmt.assigning, stmt.reference_into, lower_insert_result_kind(stmt), source)
}

lower_table_result_binding :: proc(
	ctx: ^Lower_Context,
	table: Value_Id,
	tabix: Value_Id,
	row_type: Type_Id,
	assigning: ^ast.Expr,
	reference_into: ^ast.Expr,
	result_kind: Table_Result_Kind,
	source: Source_Loc,
) {
	target := assigning if assigning != nil else reference_into
	if target == nil {
		return
	}
	row_result_type := lower_type_for_expr(ctx, target)
	inputs := [?]Value_Id{table, tabix}
	row, _, _ := builder_emit_table_read(
		ctx.builder,
		inputs[:],
		row_result_type,
		row_type,
		Intrinsic_Table_Payload {
			access = .Index,
			result_kind = result_kind,
		},
		source,
	)
	lower_store_expr(ctx, target, row)
}

lower_modify_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Modify_Stmt, source: Source_Loc) {
	if _, is_sql := lower_sql_dml_fact_for_stmt(ctx, &stmt.node); is_sql {
		lower_sql_modify_stmt(ctx, stmt, source)
		return
	}
	if stmt.dynamic_source ||
	   stmt.client_clause.end > stmt.client_clause.start ||
	   stmt.connection_clause.end > stmt.connection_clause.start {
		lower_expr(ctx, stmt.source)
		lower_expr(ctx, stmt.target)
		builder_emit_unsupported(ctx.builder, "MODIFY dynamic source semantics", source = source)
		return
	}
	if stmt.target == nil || stmt.source == nil {
		builder_emit_unsupported(ctx.builder, "MODIFY table operands", source = source)
		return
	}
	if stmt.from_table {
		builder_emit_unsupported(ctx.builder, "MODIFY table operands", source = source)
	}
	row_type := lower_table_row_type_for_expr(ctx, stmt.target)
	inputs := make([dynamic]Value_Id, 0, 4, context.temp_allocator)
	defer delete(inputs)
	source_value := lower_expr(ctx, stmt.source)
	target := lower_expr(ctx, stmt.target)
	assert(source_value != INVALID_VALUE_ID && target != INVALID_VALUE_ID)
	append(&inputs, source_value)
	append(&inputs, target)
	if stmt.index != nil {
		index := lower_expr(ctx, stmt.index)
		if index != INVALID_VALUE_ID {
			append(&inputs, index)
		}
	}
	components := make([dynamic]Intrinsic_Table_Component, 0, 2, context.temp_allocator)
	defer lower_table_components_destroy_scratch(&components)
	where_count := 0
	if stmt.where_cond != nil {
		if stmt.dynamic_where {
			lower_expr(ctx, stmt.where_cond)
			builder_emit_unsupported(ctx.builder, "MODIFY WHERE semantics", source = source)
		} else if count, ok := lower_table_condition_inputs(ctx, stmt.where_cond, &inputs, &components, 0); ok {
			where_count = count
		} else {
			lower_expr(ctx, stmt.where_cond)
			builder_emit_unsupported(ctx.builder, "MODIFY WHERE semantics", source = source)
		}
	}
	if len(stmt.transporting) > 0 {
		builder_emit_unsupported(ctx.builder, "MODIFY TRANSPORTING semantics", source = source)
	}
	modify_op := builder_emit_table_mutation(
		ctx.builder,
		.Table_Modify,
		inputs[:],
		row_type,
		Intrinsic_Table_Payload {
			access = lower_modify_access(stmt),
			source_kind = Table_Source_Kind.From_Table if stmt.from_table else Table_Source_Kind.Row,
			component_count = where_count + len(stmt.transporting),
			components = components,
		},
		source,
	)
	modify_result := op_ptr(builder_function(ctx.builder), modify_op)
	builder_emit_system_write(ctx.builder, "subrc", modify_result.results[1], source)
	builder_emit_system_write(ctx.builder, "tabix", modify_result.results[2], source)
}

lower_delete_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Delete_Stmt, source: Source_Loc) {
	if stmt.form == .Db_Table || stmt.dynamic_source || stmt.client_clause.end > stmt.client_clause.start || stmt.connection_clause.end > stmt.connection_clause.start {
		lower_sql_delete_stmt(ctx, stmt, source)
		return
	}
	target_expr := stmt.target if stmt.target != nil else stmt.source
	if target_expr == nil {
		builder_emit_unsupported(ctx.builder, "DELETE target semantics", source = source)
		return
	}
	row_type := lower_table_row_type_for_expr(ctx, target_expr)
	inputs := make([dynamic]Value_Id, 0, 4, context.temp_allocator)
	defer delete(inputs)
	target := lower_expr(ctx, target_expr)
	assert(target != INVALID_VALUE_ID)
	append(&inputs, target)
	if stmt.source != nil && stmt.source != target_expr {
		source_value := lower_expr(ctx, stmt.source)
		if source_value != INVALID_VALUE_ID {
			append(&inputs, source_value)
		}
	}
	if stmt.index != nil {
		index := lower_expr(ctx, stmt.index)
		if index != INVALID_VALUE_ID {
			append(&inputs, index)
		}
	}
	components := make([dynamic]Intrinsic_Table_Component, 0, 2, context.temp_allocator)
	defer lower_table_components_destroy_scratch(&components)
	where_count := 0
	if stmt.where_cond != nil {
		if stmt.dynamic_where {
			lower_expr(ctx, stmt.where_cond)
			builder_emit_unsupported(ctx.builder, "DELETE WHERE semantics", source = source)
		} else if count, ok := lower_table_condition_inputs(ctx, stmt.where_cond, &inputs, &components, 0); ok {
			where_count = count
		} else {
			lower_expr(ctx, stmt.where_cond)
			builder_emit_unsupported(ctx.builder, "DELETE WHERE semantics", source = source)
		}
	}
	if stmt.form == .Adjacent_Duplicates {
		builder_emit_unsupported(ctx.builder, "DELETE ADJACENT DUPLICATES semantics", source = source)
	}
	if len(stmt.comparing) > 0 {
		builder_emit_unsupported(ctx.builder, "DELETE COMPARING semantics", source = source)
	}
	if stmt.using_key.dynamic_name != nil {
		key := lower_expr(ctx, stmt.using_key.dynamic_name)
		if key != INVALID_VALUE_ID {
			append(&inputs, key)
		}
	}
	delete_op := builder_emit_table_mutation(
		ctx.builder,
		.Table_Delete,
		inputs[:],
		row_type,
		Intrinsic_Table_Payload {
			access = lower_delete_access(stmt),
			key_kind = lower_table_key_selector_kind(stmt.using_key),
			key_name = stmt.using_key.name.text,
			source_kind = Table_Source_Kind.Row if stmt.source != nil && stmt.source != target_expr else Table_Source_Kind.Unknown,
			component_count = where_count + len(stmt.comparing),
			components = components,
		},
		source,
	)
	delete_result := op_ptr(builder_function(ctx.builder), delete_op)
	builder_emit_system_write(ctx.builder, "subrc", delete_result.results[1], source)
	builder_emit_system_write(ctx.builder, "tabix", delete_result.results[2], source)
}

lower_sort_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Sort_Stmt, source: Source_Loc) {
	if stmt.target == nil {
		builder_emit_unsupported(ctx.builder, "SORT target semantics", source = source)
		return
	}
	target := lower_expr(ctx, stmt.target)
	assert(target != INVALID_VALUE_ID)
	inputs := [?]Value_Id{target}
	sort_components := make([dynamic]Intrinsic_Table_Sort_Component, 0, len(stmt.fields), context.temp_allocator)
	defer lower_table_sort_components_destroy_scratch(&sort_components)
	if !lower_sort_components(stmt, &sort_components) {
		builder_emit_unsupported(ctx.builder, "SORT dynamic component semantics", source = source)
	}
	sort_op := builder_emit_table_mutation(
		ctx.builder,
		.Table_Sort,
		inputs[:],
		lower_table_row_type_for_expr(ctx, stmt.target),
		Intrinsic_Table_Payload {
			access = .Sort,
			component_count = len(stmt.fields),
			stable = stmt.stable,
			descending = stmt.descending,
			sort_components = sort_components,
		},
		source,
	)
	sort_result := op_ptr(builder_function(ctx.builder), sort_op)
	builder_emit_system_write(ctx.builder, "subrc", sort_result.results[1], source)
	builder_emit_system_write(ctx.builder, "tabix", sort_result.results[2], source)
}

lower_select_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Select_Stmt) {
	source := source_loc_from_node(ctx.file, &stmt.node.stmt_base)
	lower_select_query_inputs(ctx, &stmt.query)
	query_fact, query_ok := lower_sql_query_fact_for_query(ctx, &stmt.query)
	if !query_ok || !lower_sql_query_fact_is_modeled(query_fact) {
		result := builder_emit_unsupported(ctx.builder, "SQL SELECT source semantics", lower_select_result_type(ctx, &stmt.query), source)
		if stmt.query.result != nil && len(stmt.body) == 0 {
			lower_store_expr(ctx, stmt.query.result.target, result)
		}
		builder_emit_system_write(ctx.builder, "subrc", source = source)
		builder_emit_system_write(ctx.builder, "dbcnt", source = source)
		return
	}
	result, count := builder_emit_sql_select(
		ctx.builder,
		lower_select_result_type(ctx, &stmt.query),
		lower_sql_query_payload(ctx, &stmt.query, query_fact),
		source,
	)
	if stmt.query.result != nil {
		if len(stmt.body) == 0 {
			lower_store_expr(ctx, stmt.query.result.target, result)
		}
	}
	builder_emit_system_write(ctx.builder, "subrc", count, source)
	builder_emit_system_write(ctx.builder, "dbcnt", source = source)
	if len(stmt.body) > 0 {
		next_block := builder_add_world_block(ctx.builder, "select_next", source)
		body_block := builder_add_world_block(ctx.builder, "select_body", source)
		after_block := builder_add_world_block(ctx.builder, "select_after", source)
		builder_set_branch_world(ctx.builder, next_block, source)

		builder_position_at_end(ctx.builder, next_block)
		condition := builder_emit_unsupported(ctx.builder, "SELECT loop body semantics", BUILTIN_TYPE_PREDICATE, source)
		true_args := [?]Value_Id{ctx.builder.current_world}
		false_args := [?]Value_Id{ctx.builder.current_world}
		builder_set_cond_branch(
			ctx.builder,
			condition,
			body_block,
			true_args[:],
			after_block,
			false_args[:],
			source,
		)

		builder_position_at_end(ctx.builder, body_block)
		lower_push_control_target(
			ctx,
			Lower_Control_Target {
				kind = .Select,
				continue_block = next_block,
				continue_arg = INVALID_VALUE_ID,
				exit_block = after_block,
			},
		)
		if stmt.query.result != nil {
			lower_store_expr(ctx, stmt.query.result.target, result)
		}
		lower_stmt_list(ctx, stmt.body[:])
		if lower_current_block_open(ctx) {
			lower_branch_to_control_continue(ctx, source)
		}
		lower_pop_control_target(ctx)
		builder_position_at_end(ctx.builder, after_block)
	}
}

lower_sql_cursor_stmt :: proc(
	ctx: ^Lower_Context,
	kind: Intrinsic_Op,
	handle: ^ast.Expr,
	query: ^ast.Select_Query_Clause,
	source: Source_Loc,
) {
	handle_value := lower_expr(ctx, handle)
	assert(handle_value != INVALID_VALUE_ID)
	payload := Intrinsic_SQL_Payload{}
	if kind == .SQL_Open_Cursor {
		lower_select_query_inputs(ctx, query)
		query_fact, query_ok := lower_sql_query_fact_for_query(ctx, query)
		if !query_ok || !lower_sql_query_fact_is_modeled(query_fact) {
			builder_emit_unsupported(ctx.builder, "SQL cursor query source semantics", source = source)
			return
		}
		payload = lower_sql_query_payload(ctx, query, query_fact)
	} else if kind == .SQL_Fetch {
		handle_entity := lower_entity_for_node(ctx, &handle.expr_base)
		if shape, ok := semantic.semantic_fact_sql_cursor_query_shape(ctx.fact_query, handle_entity); ok {
			payload = lower_sql_shape_payload(ctx, shape, nil, Sql_Result_Kind.None)
		} else {
			builder_emit_unsupported(ctx.builder, "FETCH cursor query shape", source = source)
			return
		}
	}
	builder_emit_sql_cursor(ctx.builder, kind, handle_value, payload, source)
}

lower_fetch_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Fetch_Stmt, source: Source_Loc) {
	lower_sql_cursor_stmt(ctx, .SQL_Fetch, stmt.handle, nil, source)
	lower_expr(ctx, stmt.package_size)
	if stmt.result != nil {
		result := builder_emit_unsupported(ctx.builder, "FETCH result value", lower_type_for_expr(ctx, stmt.result.target), source)
		lower_store_expr(ctx, stmt.result.target, result)
	}
}

lower_sql_insert_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Insert_Stmt, source: Source_Loc) {
	inputs := make([dynamic]Value_Id, 0, 2 + len(stmt.assignments), context.temp_allocator)
	defer delete(inputs)
	lower_sql_append_input(ctx, stmt.source, &inputs)
	for assignment in stmt.assignments {
		lower_sql_append_input(ctx, assignment.value, &inputs)
	}
	if stmt.accepting_duplicate_keys ||
	   stmt.client_clause.end > stmt.client_clause.start ||
	   stmt.connection_clause.end > stmt.connection_clause.start {
		builder_emit_unsupported(ctx.builder, "SQL INSERT additions", source = source)
		return
	}
	lower_sql_emit_mutation(ctx, .SQL_Insert, &stmt.node, inputs[:], source)
}

lower_update_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Update_Stmt, source: Source_Loc) {
	inputs := make([dynamic]Value_Id, 0, 2 + len(stmt.assignments), context.temp_allocator)
	defer delete(inputs)
	lower_sql_append_input(ctx, stmt.source, &inputs)
	for assignment in stmt.assignments {
		lower_sql_append_input(ctx, assignment.value, &inputs)
	}
	lower_sql_append_input(ctx, stmt.where_cond, &inputs)
	if stmt.client_clause.end > stmt.client_clause.start ||
	   stmt.connection_clause.end > stmt.connection_clause.start {
		builder_emit_unsupported(ctx.builder, "SQL UPDATE additions", source = source)
		return
	}
	lower_sql_emit_mutation(ctx, .SQL_Update, &stmt.node, inputs[:], source)
}

lower_sql_modify_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Modify_Stmt, source: Source_Loc) {
	inputs := make([dynamic]Value_Id, 0, 2, context.temp_allocator)
	defer delete(inputs)
	lower_sql_append_input(ctx, stmt.source, &inputs)
	lower_sql_append_input(ctx, stmt.where_cond, &inputs)
	if stmt.client_clause.end > stmt.client_clause.start ||
	   stmt.connection_clause.end > stmt.connection_clause.start {
		builder_emit_unsupported(ctx.builder, "SQL MODIFY additions", source = source)
		return
	}
	lower_sql_emit_mutation(ctx, .SQL_Modify, &stmt.node, inputs[:], source)
}

lower_sql_delete_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Delete_Stmt, source: Source_Loc) {
	inputs := make([dynamic]Value_Id, 0, 2, context.temp_allocator)
	defer delete(inputs)
	lower_sql_append_input(ctx, stmt.source, &inputs)
	lower_sql_append_input(ctx, stmt.where_cond, &inputs)
	if stmt.client_clause.end > stmt.client_clause.start ||
	   stmt.connection_clause.end > stmt.connection_clause.start {
		builder_emit_unsupported(ctx.builder, "SQL DELETE additions", source = source)
		return
	}
	lower_sql_emit_mutation(ctx, .SQL_Delete, &stmt.node, inputs[:], source)
}

lower_sql_emit_mutation :: proc(
	ctx: ^Lower_Context,
	kind: Intrinsic_Op,
	stmt: ^ast.Stmt,
	inputs: []Value_Id,
	source: Source_Loc,
) {
	fact, fact_ok := lower_sql_dml_fact_for_stmt(ctx, stmt)
	if !fact_ok || !lower_sql_source_is_modeled(fact.source) || fact.dynamic_where {
		builder_emit_unsupported(ctx.builder, "SQL mutation source semantics", source = source)
		return
	}
	builder_emit_sql_mutation(
		ctx.builder,
		kind,
		inputs,
			lower_sql_dml_payload(ctx, fact),
		source,
	)
	builder_emit_system_write(ctx.builder, "subrc", source = source)
	builder_emit_system_write(ctx.builder, "dbcnt", source = source)
}

lower_sql_append_input :: proc(ctx: ^Lower_Context, expr: ^ast.Expr, inputs: ^[dynamic]Value_Id) {
	value := lower_expr(ctx, expr)
	if value != INVALID_VALUE_ID {
		append(inputs, value)
	}
}

lower_select_result_type :: proc(ctx: ^Lower_Context, query: ^ast.Select_Query_Clause) -> Type_Id {
	if query != nil && query.result != nil {
		return lower_type_for_expr(ctx, query.result.target)
	}
	return BUILTIN_TYPE_UNKNOWN
}

lower_sql_query_fact_for_query :: proc(
	ctx: ^Lower_Context,
	query: ^ast.Select_Query_Clause,
) -> (
	^semantic.Checker_Sql_Query_Fact,
	bool,
) {
	return semantic.semantic_fact_sql_query_for_query(ctx.fact_query, query)
}

lower_sql_dml_fact_for_stmt :: proc(
	ctx: ^Lower_Context,
	stmt: ^ast.Stmt,
) -> (
	^semantic.Checker_Sql_Dml_Fact,
	bool,
) {
	return semantic.semantic_fact_sql_dml_for_stmt(ctx.fact_query, stmt)
}

lower_sql_query_fact_is_modeled :: proc "contextless" (fact: ^semantic.Checker_Sql_Query_Fact) -> bool {
	if fact == nil || len(fact.sources) == 0 || len(fact.shape.fields) == 0 {
		return false
	}
	for source in fact.sources {
		if !lower_sql_source_is_modeled(source) {
			return false
		}
	}
	return true
}

lower_sql_source_is_modeled :: proc "contextless" (source: semantic.Sql_Source_Info) -> bool {
	kind := lower_sql_source_kind(source)
	return kind == .Resolved || kind == .Internal
}

lower_sql_query_payload :: proc(
	ctx: ^Lower_Context,
	query: ^ast.Select_Query_Clause,
	fact: ^semantic.Checker_Sql_Query_Fact,
) -> Intrinsic_SQL_Payload {
	primary: ^semantic.Sql_Source_Info
	if len(fact.sources) > 0 {
		primary = &fact.sources[0]
	}
	payload := lower_sql_shape_payload(ctx, fact.shape, primary, lower_sql_result_kind(query.result))
	payload.source_count = len(fact.sources)
	payload.single = query.single
	payload.is_distinct = query.is_distinct
	payload.for_all_entries = fact.for_all_entries.present
	return payload
}

lower_sql_dml_payload :: proc(
	ctx: ^Lower_Context,
	fact: ^semantic.Checker_Sql_Dml_Fact,
) -> Intrinsic_SQL_Payload {
	source := fact.source
	payload := lower_sql_source_payload(source)
	payload.row_type = module_type_from_semantic(ctx.module, source.row_type)
	payload.scalar_type = BUILTIN_TYPE_VOID
	payload.source_count = 1
	payload.assignment_count = fact.assignment_count
	payload.from_table = fact.from_table
	return payload
}

lower_sql_shape_payload :: proc(
	ctx: ^Lower_Context,
	shape: semantic.Sql_Query_Shape,
	source: ^semantic.Sql_Source_Info,
	result_kind: Sql_Result_Kind,
) -> Intrinsic_SQL_Payload {
	payload := Intrinsic_SQL_Payload {
		result_kind = result_kind,
		row_type = module_type_from_semantic(ctx.module, shape.row_type),
		scalar_type = module_type_from_semantic(ctx.module, shape.scalar_type),
		projection_count = len(shape.fields),
	}
	if source != nil {
		source_payload := lower_sql_source_payload(source^)
		payload.source_kind = source_payload.source_kind
		payload.source_name = source_payload.source_name
		payload.source_alias = source_payload.source_alias
	}
	return payload
}

lower_sql_source_payload :: proc "contextless" (source: semantic.Sql_Source_Info) -> Intrinsic_SQL_Payload {
	return Intrinsic_SQL_Payload {
		source_kind = lower_sql_source_kind(source),
		source_name = source.name,
		source_alias = source.alias,
	}
}

lower_sql_source_kind :: proc "contextless" (source: semantic.Sql_Source_Info) -> Sql_Source_Kind {
	if source.is_dynamic {
		return .Dynamic
	}
	if source.internal {
		return .Internal
	}
	if source.resolved {
		return .Resolved
	}
	if source.name != "" {
		return .Unresolved
	}
	return .Unknown
}

lower_sql_result_kind :: proc "contextless" (result: ^ast.Select_Result_Clause) -> Sql_Result_Kind {
	if result == nil {
		return .None
	}
	if result.kind == .Appending {
		return .Appending_Table if result.table else .Appending
	}
	if result.kind == .Into {
		return .Into_Table if result.table else .Into
	}
	return .None
}

lower_call_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Call_Stmt) {
	source := source_loc_from_node(ctx.file, &stmt.node.stmt_base)
	if stmt.kind == .Direct {
		lower_direct_call_stmt(ctx, stmt, source)
		return
	}
	inputs := make([dynamic]Value_Id, 0, len(stmt.named_args), context.temp_allocator)
	defer delete(inputs)
	target := lower_call_stmt_target(stmt)
	receiver := lower_call_receiver(ctx, target)
	if receiver != INVALID_VALUE_ID {
		append(&inputs, receiver)
	}
	lower_call_stmt_arg_inputs(ctx, stmt.named_args[:], &inputs)
	outputs := make([dynamic]^ast.Expr, 0, 2, context.temp_allocator)
	defer delete(outputs)
	lower_call_stmt_output_exprs(stmt.named_args[:], &outputs)
	result_types := lower_call_output_result_types(ctx, outputs[:], context.temp_allocator)
	defer delete(result_types)
	target_entity := lower_call_target_entity(ctx, target)
	callee_name := lower_call_target_name(ctx, target, target_entity)
	op_id := INVALID_OP_ID
	if target_entity == nil {
		builder_emit_unsupported(ctx.builder, lower_call_stmt_unsupported_message(stmt), source = source)
	} else if target_entity.kind == .Method {
		op_id = lower_emit_method_may_throw(
			ctx,
			target_entity,
			callee_name,
				inputs[:],
				result_types[:],
				source = source,
			)
	} else {
		op_id = lower_emit_routine_may_throw(
			ctx,
			target_entity,
			callee_name,
				inputs[:],
				result_types[:],
				source = source,
				call_kind = lower_call_kind_for_stmt(stmt, target_entity),
			)
	}
	lower_store_call_outputs(ctx, op_id, outputs[:])
	if stmt.kind == .Function {
		builder_emit_system_write(ctx.builder, "subrc", source = source)
	}
}

lower_direct_call_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Call_Stmt, source: Source_Loc) {
	if stmt.call == nil {
		builder_emit_unsupported(ctx.builder, "unresolved call target", source = source)
		return
	}
	call, ok := stmt.call.derived_expr.(^ast.Call_Expr)
	if !ok {
		lower_expr(ctx, stmt.call)
		return
	}
	inputs := make([dynamic]Value_Id, 0, 4, context.temp_allocator)
	defer delete(inputs)
	receiver := lower_call_receiver(ctx, call.callee)
	if receiver != INVALID_VALUE_ID {
		append(&inputs, receiver)
	}
	lower_call_arg_inputs(ctx, call.args, &inputs)
	outputs := make([dynamic]^ast.Expr, 0, 2, context.temp_allocator)
	defer delete(outputs)
	lower_call_arg_output_exprs(call.args, &outputs)
	result_types := lower_call_output_result_types(ctx, outputs[:], context.temp_allocator)
	defer delete(result_types)
	target_entity := lower_call_target_entity(ctx, call.callee)
	callee_name := lower_call_target_name(ctx, call.callee, target_entity)
	if target_entity != nil && target_entity.kind == .Builtin {
		builder_emit_builtin_call(
			ctx.builder,
			callee_name,
			lower_type_for_expr(ctx, stmt.call),
			inputs[:],
			source,
		)
		return
	}
	if target_entity == nil {
		builder_emit_unsupported(ctx.builder, "unresolved call target", source = source)
		return
	}
	if target_entity.kind == .Method {
		op_id := lower_emit_method_may_throw(
			ctx,
			target_entity,
			callee_name,
				inputs[:],
				result_types[:],
				source = source,
			)
		lower_store_call_outputs(ctx, op_id, outputs[:])
		return
	}
	op_id := lower_emit_routine_may_throw(
		ctx,
		target_entity,
		callee_name,
		inputs[:],
		result_types[:],
		source = source,
	)
	lower_store_call_outputs(ctx, op_id, outputs[:])
}

lower_perform_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Perform_Stmt, source: Source_Loc) {
	inputs := make([dynamic]Value_Id, 0, len(stmt.tables) + len(stmt.using_args) + len(stmt.changing), context.temp_allocator)
	defer delete(inputs)
	lower_call_expr_list_inputs(ctx, stmt.tables[:], &inputs)
	lower_call_expr_list_inputs(ctx, stmt.using_args[:], &inputs)
	lower_call_expr_list_inputs(ctx, stmt.changing[:], &inputs)
	result_types := lower_call_output_result_types(ctx, stmt.changing[:], context.temp_allocator)
	defer delete(result_types)
	lower_expr(ctx, stmt.program)
	target_entity := lower_call_target_entity(ctx, stmt.form)
	callee_name := lower_call_target_name(ctx, stmt.form, target_entity)
	if target_entity == nil {
		builder_emit_unsupported(ctx.builder, "unresolved PERFORM target", source = source)
		return
	}
	op_id := lower_emit_routine_may_throw(
		ctx,
		target_entity,
		callee_name,
		inputs[:],
		result_types[:],
		source = source,
		call_kind = .Form,
	)
	lower_store_call_outputs(ctx, op_id, stmt.changing[:])
}

lower_message_stmt :: proc(ctx: ^Lower_Context, stmt: ^ast.Message_Stmt, source: Source_Loc) {
	inputs := make([dynamic]Value_Id, 0, 4 + len(stmt.with_args), context.temp_allocator)
	defer delete(inputs)

	payload := Intrinsic_Message_Payload{}
	lower_message_head(ctx, stmt.head, &payload, &inputs)
	for arg in stmt.with_args {
		if lower_message_append_operand(ctx, arg, &inputs) {
			payload.arg_count += 1
		}
	}
	if stmt.into != nil {
		payload.has_into = true
	}
	if stmt.display_like != nil {
		payload.has_display_like = true
		if text, static := lower_message_static_expr_text(ctx, stmt.display_like); static {
			payload.display_like = text
		} else if lower_message_append_operand(ctx, stmt.display_like, &inputs) {
			payload.display_like_operand = true
		}
	}
	if stmt.raising != nil {
		payload.has_raising = true
		if text, static := lower_message_static_expr_text(ctx, stmt.raising); static {
			payload.raising = text
		} else if lower_message_append_operand(ctx, stmt.raising, &inputs) {
			payload.raising_operand = true
		}
	}

	result_type := INVALID_TYPE_ID
	if stmt.into != nil {
		result_type = lower_type_for_expr(ctx, stmt.into)
	}
	message := builder_emit_message(ctx.builder, inputs[:], result_type, payload, source)
	if stmt.into != nil && message != INVALID_VALUE_ID {
		lower_store_expr(ctx, stmt.into, message)
	}
}

lower_message_head :: proc(
	ctx: ^Lower_Context,
	head: ^ast.Message_Head_Clause,
	payload: ^Intrinsic_Message_Payload,
	inputs: ^[dynamic]Value_Id,
) {
	if head == nil {
		return
	}
	if head.id != nil {
		payload.form = .Explicit
		lower_message_head_part(ctx, head.id, &payload.id, payload, inputs)
		lower_message_head_part(ctx, head.msg_type, &payload.msg_type, payload, inputs)
		lower_message_head_part(ctx, head.number, &payload.number, payload, inputs)
		return
	}

	payload.form = .Compact if head.has_compact_class else .Default
	if head.has_compact_class {
		payload.id = head.compact_class_name.text
	}
	if head.code != nil {
		if text, static := lower_message_static_expr_text(ctx, head.code); static {
			if lower_message_text_is_static_code(text) && !lower_message_expr_is_quoted_literal(head.code) {
				lower_message_apply_code(payload, text)
			} else {
				lower_message_append_static_arg(ctx, head.code, text, inputs)
				payload.arg_count += 1
			}
		} else if lower_message_append_operand(ctx, head.code, inputs) {
			payload.arg_count += 1
		}
	}
	lower_message_head_part(ctx, head.msg_type, &payload.msg_type, payload, inputs)
}

lower_message_head_part :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Expr,
	target: ^string,
	payload: ^Intrinsic_Message_Payload,
	inputs: ^[dynamic]Value_Id,
) {
	if expr == nil {
		return
	}
	if text, static := lower_message_static_expr_text(ctx, expr); static {
		target^ = text
		return
	}
	if lower_message_append_operand(ctx, expr, inputs) {
		payload.head_operands += 1
	}
}

lower_message_append_operand :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Expr,
	inputs: ^[dynamic]Value_Id,
) -> bool {
	value := lower_expr(ctx, expr)
	if value == INVALID_VALUE_ID {
		return false
	}
	append(inputs, value)
	return true
}

lower_message_static_expr_text :: proc(ctx: ^Lower_Context, expr: ^ast.Expr) -> (string, bool) {
	if expr == nil || lower_message_expr_is_semantic_value(ctx, expr) {
		return "", false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Literal_Expr:
		return lower_static_literal_text(n.value), n.value != ""
	case ^ast.Ident_Expr:
		return n.name, n.name != ""
	case ^ast.Type_Ref_Expr:
		if n.base_name.text != "" {
			return lower_static_literal_text(n.base_name.text), true
		}
		if n.name.text != "" {
			return lower_static_literal_text(n.name.text), true
		}
		return lower_static_literal_text(n.source.text), n.source.text != ""
	case ^ast.Paren_Expr:
		return lower_message_static_expr_text(ctx, n.expr)
	}
	return "", false
}

lower_message_append_static_arg :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Expr,
	text: string,
	inputs: ^[dynamic]Value_Id,
) {
	source := source_loc_from_node(ctx.file, &expr.expr_base)
	value := builder_emit_const(ctx.builder, text, BUILTIN_TYPE_STRING, source)
	append(inputs, value)
}

lower_static_literal_text :: proc "contextless" (text: string) -> string {
	if len(text) >= 2 &&
	   ((text[0] == '\'' && text[len(text) - 1] == '\'') ||
	    (text[0] == '`' && text[len(text) - 1] == '`')) {
		return text[1:len(text) - 1]
	}
	return text
}

lower_message_expr_is_quoted_literal :: proc "contextless" (expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Literal_Expr:
		return lower_message_text_is_quoted_literal(n.value)
	case ^ast.Type_Ref_Expr:
		return lower_message_text_is_quoted_literal(n.source.text)
	case ^ast.Paren_Expr:
		return lower_message_expr_is_quoted_literal(n.expr)
	}
	return false
}

lower_message_text_is_quoted_literal :: proc "contextless" (text: string) -> bool {
	return len(text) >= 2 &&
	       ((text[0] == '\'' && text[len(text) - 1] == '\'') ||
	        (text[0] == '`' && text[len(text) - 1] == '`'))
}

lower_message_expr_is_semantic_value :: proc(ctx: ^Lower_Context, expr: ^ast.Expr) -> bool {
	if info, ok := lower_expr_info_for_node(ctx, &expr.expr_base); ok {
		#partial switch info.mode {
		case .Variable,
		     .Field,
		     .Table_Line,
		     .Routine,
		     .Method:
			return true
		}
	}
	return false
}

lower_message_text_is_static_code :: proc "contextless" (text: string) -> bool {
	message_type, message_number := lower_message_code_parts(text)
	return message_type != "" && message_number != ""
}

lower_message_apply_code :: proc "contextless" (payload: ^Intrinsic_Message_Payload, text: string) {
	if text == "" {
		return
	}
	message_type, message_number := lower_message_code_parts(text)
	if message_type == "" {
		return
	}
	if message_type != "" {
		payload.msg_type = message_type
	}
	if message_number != "" {
		payload.number = message_number
	}
}

lower_message_code_parts :: proc "contextless" (text: string) -> (string, string) {
	end := len(text)
	for i in 0 ..< len(text) {
		if text[i] == '(' {
			end = i
			break
		}
	}
	code := text[:end]
	if len(code) > 1 && lower_message_type_byte(code[0]) && lower_message_digit_byte(code[1]) {
		return code[:1], code[1:]
	}
	return "", code
}

lower_message_type_byte :: proc "contextless" (ch: byte) -> bool {
	return ch == 'a' || ch == 'A' ||
	       ch == 'e' || ch == 'E' ||
	       ch == 'i' || ch == 'I' ||
	       ch == 's' || ch == 'S' ||
	       ch == 'w' || ch == 'W' ||
	       ch == 'x' || ch == 'X'
}

lower_message_digit_byte :: proc "contextless" (ch: byte) -> bool {
	return '0' <= ch && ch <= '9'
}

lower_call_expr :: proc(ctx: ^Lower_Context, expr: ^ast.Call_Expr, source: Source_Loc) -> Value_Id {
	inputs := make([dynamic]Value_Id, 0, 4, context.temp_allocator)
	defer delete(inputs)
	receiver := lower_call_receiver(ctx, expr.callee)
	if receiver != INVALID_VALUE_ID {
		append(&inputs, receiver)
	}
	lower_call_arg_inputs(ctx, expr.args, &inputs)
	result_types := [?]Type_Id{lower_type_for_expr(ctx, &expr.node)}
	callee_entity := lower_call_target_entity(ctx, expr.callee)
	callee_name := lower_call_target_name(ctx, expr.callee, callee_entity)
	if callee_entity != nil && callee_entity.kind == .Builtin {
		return builder_emit_builtin_call(
			ctx.builder,
			callee_name,
			result_types[0],
			inputs[:],
			source,
		)
	}
	if callee_entity == nil {
		return builder_emit_unsupported(ctx.builder, "unresolved call expression", result_types[0], source)
	}
	op_id: Op_Id
	if callee_entity.kind == .Method {
		op_id = lower_emit_method_may_throw(
			ctx,
			callee_entity,
			callee_name,
			inputs[:],
			result_types[:],
			source,
		)
	} else {
		op_id = lower_emit_routine_may_throw(
			ctx,
			callee_entity,
			callee_name,
			inputs[:],
			result_types[:],
			source,
		)
	}
	return op_ptr(builder_function(ctx.builder), op_id).results[1]
}

lower_call_stmt_target :: proc "contextless" (stmt: ^ast.Call_Stmt) -> ^ast.Expr {
	if stmt == nil {
		return nil
	}
	if stmt.target != nil {
		return stmt.target
	}
	return stmt.call
}

lower_call_stmt_arg_inputs :: proc(ctx: ^Lower_Context, args: []ast.Call_Stmt_Named_Arg, inputs: ^[dynamic]Value_Id) {
	for arg in args {
		if arg.value != nil {
			value := lower_expr(ctx, arg.value)
			if value != INVALID_VALUE_ID {
				append(inputs, value)
			}
		}
		if arg.message != nil {
			value := lower_expr(ctx, arg.message)
			if value != INVALID_VALUE_ID {
				append(inputs, value)
			}
		}
	}
}

lower_call_expr_list_inputs :: proc(ctx: ^Lower_Context, exprs: []^ast.Expr, inputs: ^[dynamic]Value_Id) {
	for expr in exprs {
		value := lower_expr(ctx, expr)
		if value != INVALID_VALUE_ID {
			append(inputs, value)
		}
	}
}

lower_call_stmt_output_exprs :: proc(args: []ast.Call_Stmt_Named_Arg, outputs: ^[dynamic]^ast.Expr) {
	for arg in args {
		if lower_call_section_is_output(arg.section) && arg.value != nil {
			append(outputs, arg.value)
		}
	}
}

lower_call_arg_output_exprs :: proc(
	expr: ^ast.Expr,
	outputs: ^[dynamic]^ast.Expr,
	output_section: bool = false,
) {
	if expr == nil {
		return
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Call_Arg_List_Expr:
		for arg in n.args {
			lower_call_arg_output_exprs(arg, outputs, output_section)
		}
	case ^ast.Call_Arg_Section_Expr:
		is_output := lower_call_section_is_output(n.kind)
		for arg in n.args {
			lower_call_arg_output_exprs(arg, outputs, is_output)
		}
	case ^ast.Call_Named_Arg_Expr:
		if output_section && n.value != nil {
			append(outputs, n.value)
		}
	case ^ast.Call_Positional_Arg_Expr:
		if output_section && n.value != nil {
			append(outputs, n.value)
		}
	}
}

lower_call_section_is_output :: #force_inline proc "contextless" (section: ast.Call_Arg_Section_Kind) -> bool {
	#partial switch section {
	case .Changing, .Importing, .Receiving:
		return true
	}
	return false
}

lower_call_output_result_types :: proc(
	ctx: ^Lower_Context,
	outputs: []^ast.Expr,
	allocator: mem.Allocator,
) -> [dynamic]Type_Id {
	result_types := make([dynamic]Type_Id, 0, len(outputs), allocator)
	for output in outputs {
		append(&result_types, lower_type_for_expr(ctx, output))
	}
	return result_types
}

lower_store_call_outputs :: proc(ctx: ^Lower_Context, op_id: Op_Id, outputs: []^ast.Expr) {
	if op_id == INVALID_OP_ID || len(outputs) == 0 {
		return
	}
	op := op_ptr(builder_function(ctx.builder), op_id)
	for output, i in outputs {
		result_index := i + 1
		if result_index >= len(op.results) {
			break
		}
		lower_store_expr(ctx, output, op.results[result_index])
	}
}

lower_call_target_entity :: proc(ctx: ^Lower_Context, expr: ^ast.Expr) -> ^semantic.Entity {
	if expr == nil {
		return nil
	}
	if entity := lower_entity_for_node(ctx, &expr.expr_base); entity != nil && lower_entity_is_call_target(entity) {
		return entity
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Type_Ref_Expr:
		if n.raw_operand {
			return lower_call_target_entity_for_raw(ctx, n)
		}
	case ^ast.Selector_Expr:
		return lower_call_target_entity(ctx, n.field)
	case ^ast.Interface_Qualified_Selector_Expr:
		return lower_call_target_entity(ctx, n.member)
	case ^ast.Paren_Expr:
		return lower_call_target_entity(ctx, n.expr)
	}
	return nil
}

lower_call_target_entity_for_raw :: proc(ctx: ^Lower_Context, raw: ^ast.Type_Ref_Expr) -> ^semantic.Entity {
	if raw == nil || len(raw.raw_refs) != 1 {
		return nil
	}
	ref := raw.raw_refs[0]
	use_range := ref.name.range
	if len(ref.path) > 0 {
		use_range = ref.path[len(ref.path) - 1].name.range
	}
	if use := semantic.semantic_ref_use_at_range(ctx.ref_query, use_range); use != nil && lower_entity_is_call_target(use.entity) {
		return use.entity
	}
	return nil
}

lower_entity_is_call_target :: proc "contextless" (entity: ^semantic.Entity) -> bool {
	if entity == nil {
		return false
	}
	#partial switch entity.kind {
	case .Builtin, .Form, .Method, .Module, .Event:
		return true
	}
	return false
}

lower_call_receiver :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Expr,
) -> Value_Id {
	if expr == nil {
		return INVALID_VALUE_ID
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Selector_Expr:
		#partial switch n.op {
		case .Arrow:
			return lower_expr(ctx, n.base)
		case .Fat_Arrow:
			return INVALID_VALUE_ID
		case .Tilde:
			return INVALID_VALUE_ID
		}
	case ^ast.Interface_Qualified_Selector_Expr:
		if n.receiver_op == .Arrow {
			return lower_expr(ctx, n.receiver)
		}
		return INVALID_VALUE_ID
	case ^ast.Type_Ref_Expr:
		if n.raw_operand {
			return lower_call_receiver_for_raw(ctx, expr, n)
		}
	case ^ast.Dynamic_Call_Method_Target_Expr:
		if n.base != nil && !n.base_dynamic {
			return lower_expr(ctx, n.base)
		}
		return INVALID_VALUE_ID
	case ^ast.Ole_Call_Method_Target_Expr:
		if n.object != nil {
			return lower_expr(ctx, n.object)
		}
		return INVALID_VALUE_ID
	case ^ast.Paren_Expr:
		return lower_call_receiver(ctx, n.expr)
	}
	return lower_implicit_method_receiver(ctx, expr)
}

lower_call_receiver_for_raw :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Expr,
	raw: ^ast.Type_Ref_Expr,
) -> Value_Id {
	if raw == nil || len(raw.raw_refs) != 1 {
		return INVALID_VALUE_ID
	}
	ref := raw.raw_refs[0]
	if ref.dynamic_path {
		return INVALID_VALUE_ID
	}
	if len(ref.path) == 0 {
		return lower_implicit_method_receiver(ctx, expr)
	}
	last_index := len(ref.path) - 1
	last := ref.path[last_index]
	#partial switch last.selector {
	case .Arrow:
		return lower_call_receiver_raw_prefix(ctx, expr, ref, last_index)
	case .Fat_Arrow:
		return INVALID_VALUE_ID
	case .Tilde:
		if last_index > 0 && ref.path[last_index - 1].selector == .Arrow {
			return lower_call_receiver_raw_prefix(ctx, expr, ref, last_index - 1)
		}
		return INVALID_VALUE_ID
	}
	return lower_implicit_method_receiver(ctx, expr)
}

lower_implicit_method_receiver :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Expr,
) -> Value_Id {
	entity := lower_call_target_entity(ctx, expr)
	if entity == nil || entity.kind != .Method {
		return INVALID_VALUE_ID
	}
	if lower_entity_is_instance_method(entity) {
		source := source_loc_from_node(ctx.file, &expr.expr_base)
		return lower_load_current_instance(ctx, source, entity.owner)
	}
	return INVALID_VALUE_ID
}

lower_call_receiver_raw_prefix :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Expr,
	ref: ast.Raw_Operand_Ref,
	segment_count: int,
) -> Value_Id {
	value := lower_load_raw_operand_base(ctx, expr, ref)
	if value == INVALID_VALUE_ID {
		return INVALID_VALUE_ID
	}
	projection_segments := make([dynamic]Projection_Segment, 0, segment_count, context.temp_allocator)
	defer delete(projection_segments)
	for segment, i in ref.path {
		if i >= segment_count {
			break
		}
		segment_source := Source_Loc{file = ctx.file, node = &expr.expr_base, range = segment.name.range}
		entity := lower_field_entity_at_range(ctx, segment.name.range)
		append(
			&projection_segments,
				lower_field_projection_segment(segment.name.text, segment_source, entity, segment.selector),
		)
		value = lower_emit_field_load(
			ctx,
			value,
			segment.name.text,
			lower_type_for_raw_segment(ctx, expr, segment, false),
			segment_source,
			entity,
			segment.selector,
			projection_segments[:],
		)
	}
	return value
}

lower_call_target_name :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Expr,
	entity: ^semantic.Entity,
) -> string {
	if entity != nil {
		entity_name := lower_callable_entity_name(entity, allocator = ctx.module.allocator)
		if entity_name != "" {
			return entity_name
		}
	}
	name := lower_expr_name(expr, ctx.project.allocator)
	if name != "" {
		return name
	}
	if expr == nil {
		return ""
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Literal_Expr:
		if len(n.value) >= 2 &&
		   ((n.value[0] == '\'' && n.value[len(n.value) - 1] == '\'') ||
		    (n.value[0] == '`' && n.value[len(n.value) - 1] == '`')) {
			return n.value[1:len(n.value) - 1]
		}
		return n.value
	case ^ast.Type_Ref_Expr:
		if n.raw_operand && len(n.raw_refs) == 1 {
			ref := n.raw_refs[0]
			if len(ref.path) > 0 {
				return ref.path[len(ref.path) - 1].name.text
			}
			return ref.name.text
		}
	}
	return ""
}

lower_callable_entity_name :: proc(
	entity: ^semantic.Entity,
	fallback: string = "",
	allocator: mem.Allocator = context.temp_allocator,
) -> string {
	if entity == nil {
		return fallback
	}
	if entity.kind == .Method &&
	   entity.owner != nil &&
	   (entity.owner.kind == .Class || entity.owner.kind == .Interface) &&
	   entity.owner.name != "" {
		method_name := entity.name
		if method_name == "" {
			method_name = fallback
		}
		if method_name != "" {
			builder := strings.builder_make(allocator)
			strings.write_string(&builder, entity.owner.name)
			strings.write_byte(&builder, '.')
			strings.write_string(&builder, method_name)
			return strings.to_string(builder)
		}
	}
	if entity.name != "" {
		return entity.name
	}
	return fallback
}

lower_call_kind_for_stmt :: proc "contextless" (
	stmt: ^ast.Call_Stmt,
	target: ^semantic.Entity,
) -> Abap_Call_Kind {
	if stmt != nil {
		#partial switch stmt.kind {
		case .Function, .Customer_Function:
			return .Function_Module
		case .Method:
			return .Method
		}
	}
	return abap_call_kind_for_entity(target)
}

lower_call_stmt_unsupported_message :: proc "contextless" (stmt: ^ast.Call_Stmt) -> string {
	if stmt == nil {
		return "unresolved call target"
	}
	#partial switch stmt.kind {
	case .Method:
		return "unresolved or dynamic CALL METHOD target"
	case .Function, .Customer_Function:
		return "unresolved CALL FUNCTION target"
	case .Transformation:
		return "CALL TRANSFORMATION semantics"
	case .Transaction:
		return "CALL TRANSACTION semantics"
	}
	return "unresolved call target"
}

lower_call_arg_inputs :: proc(ctx: ^Lower_Context, expr: ^ast.Expr, inputs: ^[dynamic]Value_Id) {
	if expr == nil {
		return
	}
	append_input := proc(inputs: ^[dynamic]Value_Id, value: Value_Id) {
		if value != INVALID_VALUE_ID {
			append(inputs, value)
		}
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Call_Arg_List_Expr:
		for arg in n.args {
			lower_call_arg_inputs(ctx, arg, inputs)
		}
	case ^ast.Call_Arg_Section_Expr:
		for arg in n.args {
			lower_call_arg_inputs(ctx, arg, inputs)
		}
	case ^ast.Call_Named_Arg_Expr:
		append_input(inputs, lower_expr(ctx, n.value))
	case ^ast.Call_Positional_Arg_Expr:
		append_input(inputs, lower_expr(ctx, n.value))
	case:
		append_input(inputs, lower_expr(ctx, expr))
	}
}

lower_arithmetic_stmt :: proc(
	ctx: ^Lower_Context,
	kind: Intrinsic_Op,
	source_expr: ^ast.Expr,
	target_expr: ^ast.Expr,
	result_expr: ^ast.Expr,
	source: Source_Loc,
) {
	left := lower_expr(ctx, target_expr)
	right := lower_expr(ctx, source_expr)
	assert(left != INVALID_VALUE_ID && right != INVALID_VALUE_ID)
	target := result_expr if result_expr != nil else target_expr
	result_type := lower_type_for_expr(ctx, target)
	operands := [?]Value_Id{left, right}
	result_types := [?]Type_Id{result_type}
	op_id := builder_emit_intrinsic(ctx.builder, kind, operands[:], result_types[:], source = source)
	lower_store_expr(ctx, target, op_ptr(builder_function(ctx.builder), op_id).results[0])
}

lower_loop_header_deferred_semantics :: proc(
	ctx: ^Lower_Context,
	stmt: ^ast.Loop_Stmt,
	source: Source_Loc,
) {
	if stmt.source_kind != .Table {
		builder_emit_unsupported(ctx.builder, "LOOP source kind semantics", source = source)
	}
	if stmt.from != nil || stmt.to != nil {
		lower_expr(ctx, stmt.from)
		lower_expr(ctx, stmt.to)
		builder_emit_unsupported(ctx.builder, "LOOP range semantics", source = source)
	}
	if stmt.using_key.name.text != "" || stmt.using_key.dynamic_name != nil {
		lower_expr(ctx, stmt.using_key.dynamic_name)
		builder_emit_unsupported(ctx.builder, "LOOP USING KEY semantics", source = source)
	}
	if stmt.target_casting {
		lower_expr(ctx, stmt.target_casting_type)
		builder_emit_unsupported(ctx.builder, "LOOP target casting semantics", source = source)
	}
	if stmt.transporting_no_fields || len(stmt.transporting_fields) > 0 {
		builder_emit_unsupported(ctx.builder, "LOOP TRANSPORTING semantics", source = source)
	}
	if stmt.group_by != nil || stmt.group_target != nil {
		lower_expr(ctx, stmt.group_by)
		builder_emit_unsupported(ctx.builder, "LOOP GROUP BY semantics", source = source)
	}
}

lower_select_query_inputs :: proc(ctx: ^Lower_Context, query: ^ast.Select_Query_Clause) {
	for projection in query.projections {
		lower_expr(ctx, projection)
	}
	for projection in query.projection_clauses {
		lower_expr(ctx, projection.value)
	}
	lower_expr(ctx, query.source)
	if query.source_clause != nil {
		lower_expr(ctx, query.source_clause.source)
		for join in query.source_clause.joins {
			lower_expr(ctx, join.source)
			lower_expr(ctx, join.on)
		}
	}
	lower_expr(ctx, query.where_cond)
	lower_expr(ctx, query.for_all_entries)
	for group_by in query.group_by {
		lower_expr(ctx, group_by.value)
	}
	lower_expr(ctx, query.package_size)
	lower_expr(ctx, query.up_to_rows)
}

lower_load_named_expr :: proc(ctx: ^Lower_Context, expr: ^ast.Expr, name: string, source: Source_Loc) -> Value_Id {
	entity := lower_entity_for_node(ctx, &expr.expr_base)
	if lower_entity_is_instance_attribute(entity) {
		return lower_load_instance_attribute(ctx, entity, name, lower_type_for_expr(ctx, expr), source)
	}
	slot := lower_ensure_slot(ctx, entity, name, lower_type_for_expr(ctx, expr), .Local, source)
	return builder_emit_load(ctx.builder, slot, source)
}

lower_store_named_expr :: proc(
	ctx: ^Lower_Context,
	target: ^ast.Expr,
	name: string,
	value: Value_Id,
	source: Source_Loc,
) {
	entity := lower_entity_for_node(ctx, &target.expr_base)
	if lower_entity_is_instance_attribute(entity) {
		lower_store_instance_attribute(ctx, target, entity, name, value, source)
		return
	}
	slot := lower_ensure_slot(ctx, entity, name, lower_type_for_expr(ctx, target), .Local, source)
	builder_emit_store(ctx.builder, slot, lower_move_value_to_target(ctx, target, value, source), source)
}

lower_load_instance_attribute :: proc(
	ctx: ^Lower_Context,
	entity: ^semantic.Entity,
	name: string,
	result_type: Type_Id,
	source: Source_Loc,
) -> Value_Id {
	receiver := lower_load_current_instance(ctx, source, entity.owner)
	assert(receiver != INVALID_VALUE_ID)
	return lower_emit_field_load(ctx, receiver, lower_instance_attribute_name(entity, name), result_type, source, entity, .Arrow)
}

lower_store_instance_attribute :: proc(
	ctx: ^Lower_Context,
	target: ^ast.Expr,
	entity: ^semantic.Entity,
	name: string,
	value: Value_Id,
	source: Source_Loc,
) {
	receiver := lower_load_current_instance(ctx, source, entity.owner)
	assert(receiver != INVALID_VALUE_ID)
	field_name := lower_instance_attribute_name(entity, name)
	projection := lower_add_field_projection(ctx, field_name, source, entity, .Arrow)
	builder_emit_field_store(
		ctx.builder,
		receiver,
		lower_move_value_to_target(ctx, target, value, source),
		projection,
		source,
	)
}

lower_load_current_instance :: proc(
	ctx: ^Lower_Context,
	source: Source_Loc,
	owner: ^semantic.Entity = nil,
) -> Value_Id {
	entity := lower_current_instance_entity(ctx)
	if entity != nil {
		typ := module_type_from_semantic(ctx.module, entity.type)
		slot := lower_ensure_slot(ctx, entity, entity.name, typ, .Instance, source)
		return builder_emit_load(ctx.builder, slot, source)
	}
	typ := module_type_from_semantic(ctx.module, owner.type) if owner != nil else BUILTIN_TYPE_UNKNOWN
	slot := lower_ensure_slot(ctx, nil, "me", typ, .Instance, source)
	return builder_emit_load(ctx.builder, slot, source)
}

lower_current_instance_entity :: proc(ctx: ^Lower_Context) -> ^semantic.Entity {
	function := builder_function(ctx.builder)
	routine := function.entity
	if routine == nil || routine.kind != .Method {
		return nil
	}
	payload, ok := routine.payload.(^semantic.Entity_Routine_Payload)
	if !ok || payload == nil || payload.is_static || payload.signature_scope == nil {
		return nil
	}
	name := semantic.project_intern_lower_ascii(ctx.project, "me")
	entity, found := semantic.scope_lookup_declaration(payload.signature_scope, .Value, name)
	return entity if found else nil
}

lower_seed_callable_frame :: proc(ctx: ^Lower_Context, source: Source_Loc) {
	if entity := lower_current_instance_entity(ctx); entity != nil {
		typ := module_type_from_semantic(ctx.module, entity.type)
		lower_ensure_slot(ctx, entity, entity.name, typ, .Instance, source)
	}
}

lower_instance_attribute_name :: proc "contextless" (entity: ^semantic.Entity, fallback: string) -> string {
	if entity != nil && entity.name != "" {
		return entity.name
	}
	return fallback
}

lower_ensure_slot_for_decl :: proc(
	ctx: ^Lower_Context,
	name: string,
	range: tokenizer.Range,
	kind: Slot_Kind,
	entity_kind: semantic.Entity_Kind = .Variable,
) -> Slot_Id {
	entity := lower_entity_at_range(ctx.decl_query, range, entity_kind)
	typ := module_type_from_semantic(ctx.module, entity.type) if entity != nil else BUILTIN_TYPE_UNKNOWN
	slot_kind := kind
	if kind == .Local && entity != nil {
		slot_kind = lower_slot_kind_for_entity(entity)
	}
	return lower_ensure_slot(ctx, entity, name, typ, slot_kind, Source_Loc{file = ctx.file, range = range})
}

lower_ensure_slot :: proc(
	ctx: ^Lower_Context,
	entity: ^semantic.Entity,
	name: string,
	typ: Type_Id,
	kind: Slot_Kind,
	source: Source_Loc,
) -> Slot_Id {
	slot_name := name
	slot_type := typ
	slot_kind := kind
	if kind == .Local && entity != nil {
		slot_kind = lower_slot_kind_for_entity(entity)
	}
	function := builder_function(ctx.builder)
	if entity != nil {
		for slot, i in function.slots {
			if slot.entity == entity {
				return Slot_Id(i)
			}
		}
		if entity.name != "" {
			slot_name = entity.name
		}
		if slot_type == INVALID_TYPE_ID {
			slot_type = module_type_from_semantic(ctx.module, entity.type)
		}
		return function_add_slot(function, slot_kind, slot_name, slot_type, entity, lower_source_loc_for_entity(entity, source))
	}
	if slot_type == INVALID_TYPE_ID {
		slot_type = BUILTIN_TYPE_UNKNOWN
	}
	return function_add_slot(function, slot_kind, slot_name, slot_type, nil, source)
}

lower_source_loc_for_entity :: proc "contextless" (entity: ^semantic.Entity, fallback: Source_Loc) -> Source_Loc {
	if entity == nil || entity.name_range.end <= entity.name_range.start {
		return fallback
	}
	return Source_Loc{file = entity.source_file, node = entity.node, range = entity.name_range}
}

lower_type_for_expr :: proc(ctx: ^Lower_Context, expr: ^ast.Expr) -> Type_Id {
	if expr == nil {
		return BUILTIN_TYPE_UNKNOWN
	}
	if info, ok := lower_expr_info_for_node(ctx, &expr.expr_base); ok {
		return module_type_from_semantic(ctx.module, info.type)
	}
	return BUILTIN_TYPE_UNKNOWN
}

lower_expr_info_for_node :: proc(
	ctx: ^Lower_Context,
	node: ^ast.Node,
) -> (
	semantic.Checker_Expr_Info,
	bool,
) {
	if info, ok := semantic.semantic_fact_operand_info_for_node(ctx.fact_query, node); ok {
		return info, true
	}
	return semantic.semantic_fact_operand_info_at_range(ctx.fact_query, node.range)
}

lower_binary_result_type :: proc(ctx: ^Lower_Context, expr: ^ast.Expr, op: ast.Binary_Op) -> Type_Id {
	#partial switch op {
	case .Equal,
	     .Not_Equal,
	     .Less,
	     .Less_Equal,
	     .Greater,
	     .Greater_Equal,
	     .Contains_Only,
	     .Contains_Not_Only,
	     .Contains_Any,
	     .Contains_Not_Any,
	     .Contains_String,
	     .Contains_No_String,
	     .Covers_Pattern,
	     .Covers_No_Pattern,
	     .In,
	     .Not_In,
	     .And,
	     .Or,
	     .Is,
	     .Between,
	     .Like,
	     .Not_Like:
		return BUILTIN_TYPE_PREDICATE
	case .Concatenate:
		return BUILTIN_TYPE_STRING
	}
	return lower_type_for_expr(ctx, expr)
}

lower_loop_result_type :: proc(ctx: ^Lower_Context, stmt: ^ast.Loop_Stmt, row_type: Type_Id) -> Type_Id {
	if stmt.target != nil {
		return lower_type_for_expr(ctx, stmt.target)
	}
	return row_type
}

lower_loop_result_kind :: proc "contextless" (stmt: ^ast.Loop_Stmt) -> Table_Result_Kind {
	if stmt == nil || stmt.target == nil {
		return .None
	}
	#partial switch stmt.target_kind {
	case .Into:
		return .Into
	case .Assigning:
		return .Assigning
	case .Reference_Into:
		return .Reference_Into
	}
	return .None
}

lower_read_table_result_type :: proc(ctx: ^Lower_Context, entry: ast.Read_Table_Entry_Clause, row_type: Type_Id) -> Type_Id {
	if entry.into != nil {
		return lower_type_for_expr(ctx, entry.into)
	}
	if entry.assigning != nil {
		return lower_type_for_expr(ctx, entry.assigning)
	}
	if entry.reference_into != nil {
		return lower_type_for_expr(ctx, entry.reference_into)
	}
	return row_type
}

lower_table_expr_selector_inputs :: proc(
	ctx: ^Lower_Context,
	expr: ^ast.Table_Expr,
	inputs: ^[dynamic]Value_Id,
	components: ^[dynamic]Intrinsic_Table_Component,
) -> bool {
	if expr == nil || len(expr.selectors) == 0 {
		return true
	}
	access := lower_table_expr_access(expr)
	for selector in expr.selectors {
		if selector == nil {
			continue
		}
		if access == .Index {
			value := lower_expr(ctx, selector)
			if value != INVALID_VALUE_ID {
				append(inputs, value)
			}
			continue
		}
		binary, ok := selector.derived_expr.(^ast.Binary_Expr)
		if !ok || binary.op != .Equal {
			return false
		}
		path, path_ok := lower_table_path_from_expr(binary.left, context.temp_allocator)
		if !path_ok {
			return false
		}
		value_index := len(inputs)
		value := lower_expr(ctx, binary.right)
		if value == INVALID_VALUE_ID {
			delete(path)
			return false
		}
		append(inputs, value)
		append(components, Intrinsic_Table_Component{path = path, value_index = value_index})
	}
	return true
}

lower_table_condition_inputs :: proc(
	ctx: ^Lower_Context,
	condition: ^ast.Expr,
	inputs: ^[dynamic]Value_Id,
	components: ^[dynamic]Intrinsic_Table_Component,
	request_value_offset: int,
) -> (int, bool) {
	if condition == nil {
		return 0, true
	}
	#partial switch n in condition.derived_expr {
	case ^ast.Paren_Expr:
		return lower_table_condition_inputs(ctx, n.expr, inputs, components, request_value_offset)
	case ^ast.Binary_Expr:
		if n.op == .And {
			left_count, left_ok := lower_table_condition_inputs(ctx, n.left, inputs, components, request_value_offset)
			right_count, right_ok := lower_table_condition_inputs(ctx, n.right, inputs, components, request_value_offset)
			return left_count + right_count, left_ok && right_ok
		}
		if n.op != .Equal {
			return 0, false
		}
		path, path_ok := lower_table_path_from_expr(n.left, context.temp_allocator)
		if !path_ok {
			return 0, false
		}
		value_index := request_value_offset + len(inputs)
		value := lower_expr(ctx, n.right)
		if value == INVALID_VALUE_ID {
			delete(path)
			return 0, false
		}
		append(inputs, value)
		append(components, Intrinsic_Table_Component{path = path, value_index = value_index})
		return 1, true
	}
	return 0, false
}

lower_table_expr_access :: proc "contextless" (expr: ^ast.Table_Expr) -> Table_Access_Kind {
	if expr == nil || len(expr.selectors) == 0 {
		return .Full
	}
	if len(expr.selectors) == 1 {
		if _, is_key := expr.selectors[0].derived_expr.(^ast.Binary_Expr); !is_key {
			return .Index
		}
	}
	return .Key
}

lower_read_table_key_inputs :: proc(
	ctx: ^Lower_Context,
	entry: ast.Read_Table_Entry_Clause,
	inputs: ^[dynamic]Value_Id,
	components: ^[dynamic]Intrinsic_Table_Component,
) -> (
	bool,
	bool,
) {
	dynamic_key := false
	dynamic_component := false
	if entry.using_key.dynamic_name != nil {
		dynamic_key = true
		value := lower_expr(ctx, entry.using_key.dynamic_name)
		if value != INVALID_VALUE_ID {
			append(inputs, value)
		}
	}
	for key in entry.key_values {
		if key.dynamic_name != nil {
			dynamic_component = true
			value := lower_expr(ctx, key.dynamic_name)
			if value != INVALID_VALUE_ID {
				append(inputs, value)
			}
		}
		if key.is_dynamic {
			dynamic_component = true
		}
		path, path_ok := lower_table_path_from_read_key(key, context.temp_allocator)
		if !path_ok {
			dynamic_component = true
		}
		value_index := len(inputs)
		value := lower_expr(ctx, key.value)
		if value != INVALID_VALUE_ID {
			append(inputs, value)
		}
		if path_ok && !key.is_dynamic && key.dynamic_name == nil && value != INVALID_VALUE_ID {
			append(components, Intrinsic_Table_Component{path = path, value_index = value_index})
		} else if path_ok {
			delete(path)
		}
	}
	return dynamic_key, dynamic_component
}

lower_read_table_payload :: proc "contextless" (
	entry: ast.Read_Table_Entry_Clause,
	components: [dynamic]Intrinsic_Table_Component,
	dynamic_key: bool,
	dynamic_component: bool,
) -> Intrinsic_Table_Payload {
	return Intrinsic_Table_Payload {
		access = lower_read_table_access(entry),
		key_kind = lower_read_table_key_kind(entry),
		result_kind = lower_read_table_result_kind(entry),
		key_name = lower_read_table_key_name(entry),
		component_count = len(entry.key_values) + len(entry.transporting_fields) + len(entry.comparing),
		components = components,
		binary_search = entry.binary_search,
		dynamic_key = dynamic_key,
		dynamic_component = dynamic_component,
	}
}

lower_read_table_access :: proc "contextless" (entry: ast.Read_Table_Entry_Clause) -> Table_Access_Kind {
	if entry.index != nil {
		return .Index
	}
	#partial switch entry.key_kind {
	case .Table_Key:
		return .Table_Key
	case .Key:
		return .Key
	}
	if len(entry.key_values) > 0 {
		return .Key
	}
	return .Full
}

lower_read_table_key_kind :: proc "contextless" (entry: ast.Read_Table_Entry_Clause) -> Table_Key_Kind {
	if selector_kind := lower_table_key_selector_kind(entry.using_key); selector_kind != .None {
		return selector_kind
	}
	if entry.key_name.text != "" {
		return .Primary if entry.key_name.text == "primary_key" else .Named
	}
	#partial switch entry.key_kind {
	case .Table_Key:
		return .Table
	case .Key:
		return .Free
	}
	if len(entry.key_values) > 0 {
		return .Free
	}
	return .None
}

lower_read_table_key_name :: proc "contextless" (entry: ast.Read_Table_Entry_Clause) -> string {
	if entry.using_key.name.text != "" {
		return entry.using_key.name.text
	}
	return entry.key_name.text
}

lower_sort_components :: proc(stmt: ^ast.Sort_Stmt, components: ^[dynamic]Intrinsic_Table_Sort_Component) -> bool {
	if stmt == nil {
		return true
	}
	for field in stmt.fields {
		path, path_ok := lower_table_path_from_sort_field(field, context.temp_allocator)
		if !path_ok {
			return false
		}
		append(
			components,
			Intrinsic_Table_Sort_Component {
				path = path,
				descending = field.descending || (stmt.descending && !field.ascending),
			},
		)
	}
	return true
}

lower_table_path_from_read_key :: proc(
	key: ast.Read_Table_Key_Value_Clause,
	allocator: mem.Allocator,
) -> ([dynamic]string, bool) {
	out := make([dynamic]string, 0, 1 + len(key.path), allocator)
	if len(key.path) > 0 {
		for segment in key.path {
			if segment.name.text == "" || segment.selector != .Dash {
				delete(out)
				return nil, false
			}
			append(&out, segment.name.text)
		}
		return out, true
	}
	if key.table_line {
		append(&out, "table_line")
	} else if key.name.text != "" {
		append(&out, key.name.text)
	} else {
		delete(out)
		return nil, false
	}
	return out, true
}

lower_table_path_from_sort_field :: proc(
	field: ast.Sort_Field_Clause,
	allocator: mem.Allocator,
) -> ([dynamic]string, bool) {
	if field.expr != nil {
		return lower_table_path_from_expr(field.expr, allocator)
	}
	if field.name.text != "" {
		out := make([dynamic]string, 0, 1, allocator)
		append(&out, field.name.text)
		return out, true
	}
	return nil, false
}

lower_table_path_from_expr :: proc(
	expr: ^ast.Expr,
	allocator: mem.Allocator,
) -> ([dynamic]string, bool) {
	out := make([dynamic]string, 0, 4, allocator)
	if !lower_table_path_append_expr(&out, expr) {
		delete(out)
		return nil, false
	}
	return out, true
}

lower_table_path_append_expr :: proc(out: ^[dynamic]string, expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		if n.name == "" {
			return false
		}
		append(out, n.name)
		return true
	case ^ast.Selector_Expr:
		if n.op != .Dash || !lower_table_path_append_expr(out, n.base) {
			return false
		}
		name := lower_table_expr_name(n.field)
		if name == "" {
			return false
		}
		append(out, name)
		return true
	case ^ast.Paren_Expr:
		return lower_table_path_append_expr(out, n.expr)
	}
	return false
}

lower_table_expr_name :: proc "contextless" (expr: ^ast.Expr) -> string {
	if expr == nil {
		return ""
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		return n.name
	case ^ast.Type_Ref_Expr:
		if n.name.text != "" {
			return n.name.text
		}
		return n.base_name.text
	}
	return ""
}

lower_table_components_destroy_scratch :: proc(components: ^[dynamic]Intrinsic_Table_Component) {
	if components == nil {
		return
	}
	for &component in components^ {
		delete(component.path)
	}
	delete(components^)
}

lower_table_sort_components_destroy_scratch :: proc(components: ^[dynamic]Intrinsic_Table_Sort_Component) {
	if components == nil {
		return
	}
	for &component in components^ {
		delete(component.path)
	}
	delete(components^)
}

lower_read_table_result_kind :: proc "contextless" (entry: ast.Read_Table_Entry_Clause) -> Table_Result_Kind {
	if entry.transporting_no_fields {
		return .No_Fields
	}
	if entry.into != nil {
		return .Into
	}
	if entry.assigning != nil {
		return .Assigning
	}
	if entry.reference_into != nil {
		return .Reference_Into
	}
	return .Value
}

lower_table_key_selector_kind :: proc "contextless" (selector: ast.Table_Key_Selector) -> Table_Key_Kind {
	if selector.dynamic_name != nil {
		return .Dynamic
	}
	if selector.name.text == "" {
		return .None
	}
	return .Primary if selector.name.text == "primary_key" else .Named
}

lower_append_result_kind :: proc "contextless" (stmt: ^ast.Append_Stmt) -> Table_Result_Kind {
	if stmt == nil {
		return .None
	}
	if stmt.assigning != nil {
		return .Assigning
	}
	if stmt.reference_into != nil {
		return .Reference_Into
	}
	return .None
}

lower_append_source_kind :: proc "contextless" (stmt: ^ast.Append_Stmt) -> Table_Source_Kind {
	if stmt == nil {
		return .Unknown
	}
	if stmt.lines_of {
		return .Lines_Of
	}
	if stmt.initial_line {
		return .Initial_Line
	}
	return .Row
}

lower_insert_result_kind :: proc "contextless" (stmt: ^ast.Insert_Stmt) -> Table_Result_Kind {
	if stmt == nil {
		return .None
	}
	if stmt.assigning != nil {
		return .Assigning
	}
	if stmt.reference_into != nil {
		return .Reference_Into
	}
	return .None
}

lower_insert_source_kind :: proc "contextless" (stmt: ^ast.Insert_Stmt) -> Table_Source_Kind {
	if stmt == nil {
		return .Unknown
	}
	if stmt.initial_line {
		return .Initial_Line
	}
	if stmt.from_table {
		return .From_Table
	}
	if stmt.form == .Lines_Of {
		return .Lines_Of
	}
	return .Row
}

lower_modify_access :: proc "contextless" (stmt: ^ast.Modify_Stmt) -> Table_Access_Kind {
	if stmt == nil {
		return .Unknown
	}
	if stmt.index != nil {
		return .Index
	}
	if stmt.where_cond != nil {
		return .Where
	}
	return .Full
}

lower_delete_access :: proc "contextless" (stmt: ^ast.Delete_Stmt) -> Table_Access_Kind {
	if stmt == nil {
		return .Unknown
	}
	if stmt.index != nil {
		return .Index
	}
	if stmt.where_cond != nil {
		return .Where
	}
	if stmt.source != nil {
		return .Key
	}
	return .Full
}

lower_table_row_type_for_expr :: proc(ctx: ^Lower_Context, expr: ^ast.Expr) -> Type_Id {
	if expr == nil {
		return BUILTIN_TYPE_UNKNOWN
	}
	if info, ok := lower_expr_info_for_node(ctx, &expr.expr_base); ok {
		if row_type := lower_semantic_table_row_type(info.type); row_type != nil {
			return module_type_from_semantic(ctx.module, row_type)
		}
	}
	if entity := lower_entity_for_node(ctx, &expr.expr_base); entity != nil {
		if row_type := lower_semantic_table_row_type(entity.type); row_type != nil {
			return module_type_from_semantic(ctx.module, row_type)
		}
	}
	return BUILTIN_TYPE_UNKNOWN
}

lower_semantic_table_row_type :: proc "contextless" (typ: ^semantic.Type, depth := 0) -> ^semantic.Type {
	if typ == nil || depth > 16 {
		return nil
	}
	#partial switch typ.kind {
	case .Table:
		return typ.base
	case .Named:
		return lower_semantic_table_row_type(typ.base, depth + 1)
	}
	return nil
}

lower_entity_for_node :: proc(ctx: ^Lower_Context, node: ^ast.Node) -> ^semantic.Entity {
	if use := semantic.semantic_ref_use_for_node(ctx.ref_query, node); use != nil {
		return use.entity
	}
	if use := semantic.semantic_ref_use_at_range(ctx.ref_query, node.range); use != nil {
		return use.entity
	}
	return nil
}

lower_entity_at_range :: proc(
	query: semantic.Semantic_Decl_Query,
	range: tokenizer.Range,
	kind: semantic.Entity_Kind,
) -> ^semantic.Entity {
	if range.end <= range.start {
		return nil
	}
	return semantic.semantic_decl_entity_with_kind_and_decl_range(query, kind, range)
}

lower_binary_op_kind :: proc "contextless" (op: ast.Binary_Op) -> Intrinsic_Op {
	#partial switch op {
	case .Add:
		return .ABAP_Add
	case .Subtract:
		return .ABAP_Subtract
	case .Multiply:
		return .ABAP_Multiply
	case .Divide:
		return .ABAP_Divide
	case .Integer_Divide:
		return .ABAP_Integer_Divide
	case .Modulo:
		return .ABAP_Modulo
	case .Equal, .Is, .Between, .In:
		return .ABAP_Equal
	case .Not_Equal, .Not_In:
		return .ABAP_Not_Equal
	case .Less:
		return .ABAP_Less
	case .Less_Equal:
		return .ABAP_Less_Equal
	case .Greater:
		return .ABAP_Greater
	case .Greater_Equal:
		return .ABAP_Greater_Equal
	case .And:
		return .ABAP_And
	case .Or:
		return .ABAP_Or
	case .Concatenate:
		return .ABAP_String_Concat
	}
	return .Unsupported
}

lower_current_block_open :: proc(ctx: ^Lower_Context) -> bool {
	block := block_ptr(builder_function(ctx.builder), ctx.builder.block)
	return block.terminator == INVALID_INSTRUCTION_ID
}

lower_push_control_target :: proc(ctx: ^Lower_Context, target: Lower_Control_Target) {
	assert(target.continue_block != INVALID_BLOCK_ID && target.exit_block != INVALID_BLOCK_ID)
	append(&ctx.control_targets, target)
}

lower_pop_control_target :: proc(ctx: ^Lower_Context) {
	assert(len(ctx.control_targets) > 0)
	pop(&ctx.control_targets)
}

lower_current_control_target :: proc(ctx: ^Lower_Context) -> (^Lower_Control_Target, bool) {
	if len(ctx.control_targets) == 0 {
		return nil, false
	}
	return &ctx.control_targets[len(ctx.control_targets) - 1], true
}

lower_push_exception_target :: proc(ctx: ^Lower_Context, target: Lower_Exception_Target) {
	assert(target.dispatch_block != INVALID_BLOCK_ID)
	append(&ctx.exception_targets, target)
}

lower_pop_exception_target :: proc(ctx: ^Lower_Context) {
	assert(len(ctx.exception_targets) > 0)
	pop(&ctx.exception_targets)
}

lower_current_exception_target :: proc(ctx: ^Lower_Context) -> (^Lower_Exception_Target, bool) {
	if len(ctx.exception_targets) == 0 {
		return nil, false
	}
	return &ctx.exception_targets[len(ctx.exception_targets) - 1], true
}

lower_branch_to_control_continue :: proc(ctx: ^Lower_Context, source: Source_Loc) {
	target, ok := lower_current_control_target(ctx)
	assert(ok)
	lower_branch_to_control_block(ctx, target.continue_block, target.continue_arg, source)
}

lower_branch_to_control_exit :: proc(ctx: ^Lower_Context, source: Source_Loc) {
	target, ok := lower_current_control_target(ctx)
	assert(ok)
	lower_branch_to_control_block(ctx, target.exit_block, INVALID_VALUE_ID, source)
}

lower_branch_to_control_block :: proc(
	ctx: ^Lower_Context,
	block: Block_Id,
	arg: Value_Id,
	source: Source_Loc,
) {
	if arg != INVALID_VALUE_ID {
		args := [?]Value_Id{ctx.builder.current_world, arg}
		builder_set_branch(ctx.builder, block, args[:], source)
		return
	}
	builder_set_branch_world(ctx.builder, block, source)
}

lower_stmt_is_non_executable_declaration :: proc(stmt: ^ast.Stmt) -> bool {
	#partial switch _ in stmt.derived_stmt {
	case ^ast.Types_Decl,
	     ^ast.Constants_Decl,
	     ^ast.Field_Symbols_Decl,
	     ^ast.Statics_Decl,
	     ^ast.Tables_Decl,
	     ^ast.Ranges_Decl,
	     ^ast.Parameters_Decl,
	     ^ast.Select_Options_Decl,
	     ^ast.Controls_Decl,
	     ^ast.Class_Data_Decl,
	     ^ast.Type_Pools_Decl,
	     ^ast.Function_Pool_Decl,
	     ^ast.Include_Stmt,
	     ^ast.Report_Stmt,
	     ^ast.Class_Decl,
	     ^ast.Interface_Decl,
	     ^ast.Method_Decl,
	     ^ast.Form_Decl,
	     ^ast.Function_Decl,
	     ^ast.Module_Decl,
	     ^ast.Event_Block_Stmt,
	     ^ast.Oop_Simple_Stmt,
	     ^ast.Oop_Load_Stmt,
	     ^ast.Macro_Def_Stmt:
		return true
	}
	return false
}

lower_expr_name :: proc(expr: ^ast.Expr, allocator: mem.Allocator = context.temp_allocator) -> string {
	if expr == nil {
		return ""
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		return n.name
	case ^ast.Type_Ref_Expr:
		if n.base_name.text != "" {
			return n.base_name.text
		}
		return n.name.text
	case ^ast.Data_Inline_Name_Expr:
		return n.name.text
	case ^ast.Field_Symbol_Inline_Name_Expr:
		return n.name.text
	case ^ast.Selector_Expr:
		base := lower_expr_name(n.base, allocator)
		field := lower_expr_name(n.field, allocator)
		if base == "" {
			return field
		}
		if field == "" {
			return base
		}
		builder := strings.builder_make(allocator)
		strings.write_string(&builder, base)
		strings.write_byte(&builder, '-')
		strings.write_string(&builder, field)
		return strings.to_string(builder)
	case ^ast.Paren_Expr:
		return lower_expr_name(n.expr, allocator)
	}
	return ""
}

lower_interface_selector_name :: proc(
	selector: ^ast.Interface_Qualified_Selector_Expr,
	allocator: mem.Allocator = context.temp_allocator,
) -> string {
	if selector == nil {
		return ""
	}
	interface_name := lower_expr_name(selector.interface, allocator)
	member_name := lower_expr_name(selector.member, allocator)
	if interface_name == "" {
		return member_name
	}
	if member_name == "" {
		return interface_name
	}
	builder := strings.builder_make(allocator)
	strings.write_string(&builder, interface_name)
	strings.write_byte(&builder, '~')
	strings.write_string(&builder, member_name)
	return strings.to_string(builder)
}

lower_constructor_kind_name :: proc "contextless" (kind: ast.Constructor_Kind) -> string {
	switch kind {
	case .New:
		return "new"
	case .Value:
		return "value"
	case .Conv:
		return "conv"
	case .Ref:
		return "ref"
	case .Cast:
		return "cast"
	case .Exact:
		return "exact"
	case .Corresponding:
		return "corresponding"
	case .Filter:
		return "filter"
	case .Reduce:
		return "reduce"
	case .Switch:
		return "switch"
	case .Cond:
		return "cond"
	case .Throw:
		return "throw"
	}
	unreachable()
}

lower_constructor_deferred_message :: proc "contextless" (kind: ast.Constructor_Kind) -> string {
	#partial switch kind {
	case .Corresponding:
		return "CORRESPONDING constructor semantics"
	case .Filter:
		return "FILTER constructor semantics"
	case .Reduce:
		return "REDUCE constructor semantics"
	case .Switch:
		return "SWITCH constructor semantics"
	case .Cond:
		return "COND constructor semantics"
	case .Throw:
		return "THROW constructor semantics"
	}
	return "constructor semantics"
}

lower_slot_kind_for_entity :: proc "contextless" (entity: ^semantic.Entity) -> Slot_Kind {
	if entity == nil {
		return .Local
	}
	if lower_entity_is_oop_receiver(entity) {
		return .Instance
	}
	if entity.scope != nil && entity.scope.kind == .File {
		#partial switch entity.kind {
		case .Variable,
		     .Constant,
		     .Field_Symbol,
		     .Control:
			return .Global
		}
	}
	#partial switch entity.kind {
	case .Parameter:
		return .Parameter
	case .Field:
		return .Field
	case .Control:
		return .Global
	}
	return .Local
}

lower_entity_is_instance_attribute :: proc "contextless" (entity: ^semantic.Entity) -> bool {
	return entity != nil &&
	       entity.member_kind == .Attribute &&
	       entity.owner != nil &&
	       (entity.owner.kind == .Class || entity.owner.kind == .Interface) &&
	       !(.Static in entity.flags)
}

lower_entity_is_instance_method :: proc "contextless" (entity: ^semantic.Entity) -> bool {
	return entity != nil &&
	       entity.kind == .Method &&
	       entity.owner != nil &&
	       (entity.owner.kind == .Class || entity.owner.kind == .Interface) &&
	       !(.Static in entity.flags)
}

lower_entity_is_oop_receiver :: proc "contextless" (entity: ^semantic.Entity) -> bool {
	if entity == nil || entity.kind != .Parameter || entity.owner == nil || entity.owner.kind != .Method {
		return false
	}
	return entity.name == "me" || entity.name == "super"
}

lower_method_entity :: proc(
	decl_query: semantic.Semantic_Decl_Query,
	ref_query: semantic.Semantic_Ref_Query,
	method: ^ast.Method_Decl,
) -> ^semantic.Entity {
	if entity := lower_entity_at_range(decl_query, method.name.range, .Method); entity != nil {
		return entity
	}
	if entity := lower_method_member_entity_at_range(decl_query, method.name.range); entity != nil {
		return entity
	}
	if entity := lower_method_entity_at_range(ref_query, method.name.range); entity != nil {
		return entity
	}
	if entity := lower_method_entity_at_range(ref_query, method.member_name.range); entity != nil {
		return entity
	}
	return nil
}

lower_method_member_entity_at_range :: proc(
	decl_query: semantic.Semantic_Decl_Query,
	range: tokenizer.Range,
) -> ^semantic.Entity {
	if range.end <= range.start {
		return nil
	}
	entity := semantic.semantic_decl_class_member_at_offset(decl_query, range.start)
	if entity != nil && entity.kind == .Method {
		return entity
	}
	return nil
}

lower_method_entity_at_range :: proc(
	ref_query: semantic.Semantic_Ref_Query,
	range: tokenizer.Range,
) -> ^semantic.Entity {
	if range.end <= range.start {
		return nil
	}
	use := semantic.semantic_ref_use_at_range(ref_query, range)
	if use != nil && use.entity != nil && use.entity.kind == .Method {
		return use.entity
	}
	return nil
}

lower_method_name :: proc(method: ^ast.Method_Decl, allocator: mem.Allocator = context.temp_allocator) -> string {
	if method.qualifier.text == "" {
		return method.name.text
	}
	builder := strings.builder_make(allocator)
	strings.write_string(&builder, method.qualifier.text)
	strings.write_string(&builder, "=>")
	strings.write_string(&builder, method.member_name.text)
	return strings.to_string(builder)
}

event_block_name :: proc "contextless" (kind: ast.Event_Block_Kind) -> string {
	switch kind {
	case .Invalid:
		return "event_block"
	case .Initialization:
		return "initialization"
	case .Load_Of_Program:
		return "load_of_program"
	case .Start_Of_Selection:
		return "start_of_selection"
	case .End_Of_Selection:
		return "end_of_selection"
	case .Top_Of_Page:
		return "top_of_page"
	case .End_Of_Page:
		return "end_of_page"
	case .At_Selection_Screen:
		return "at_selection_screen"
	}
	unreachable()
}
