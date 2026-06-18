package abap_frontend_semantic2

import "src:ast"

import "core:strings"

Checker_Call_Argument :: struct {
	name:          string,
	name_text:     string,
	name_range:    Range,
	section:       ast.Call_Arg_Section_Kind,
	has_section:   bool,
	value:         ^ast.Expr,
	value_range:   Range,
	message:       ^ast.Expr,
	message_range: Range,
	raw_decls:     []ast.Raw_Operand_Inline_Decl,
	raw_refs:      []ast.Raw_Operand_Ref,
}

Checker_Call_Parameter_Key :: struct {
	section: Entity_Parameter_Section,
	name:    string,
}

Checker_Ref_Target_Kind :: enum {
	Data,
	Data_Generic,
	Object_Generic,
	Class,
	Interface,
}

Checker_Ref_Target :: struct {
	kind:   Checker_Ref_Target_Kind,
	name:   string,
	entity: ^Entity,
}

Checker_Scalar_Group :: enum {
	Unknown,
	Numeric,
	Character,
	Byte,
	Date,
	Time,
	Generic_Simple,
}

Checker_Move_Corresponding_Operand_Kind :: enum {
	Unknown,
	Structure,
	Table,
	Invalid,
}

Checker_Table_Component_Segment :: struct {
	name:  string,
	range: Range,
	node:  ^ast.Node,
}

checker_check_stmt_list :: proc(
	ctx: ^Checker_Context,
	body: [dynamic]^ast.Stmt,
	collect_declarations := true,
) {
	for stmt in body {
		checker_check_stmt(ctx, stmt, collect_declarations)
	}
}

checker_check_stmt :: proc(
	ctx: ^Checker_Context,
	stmt: ^ast.Stmt,
	collect_declarations := true,
) {
	if stmt == nil {
		return
	}

	#partial switch n in stmt.derived_stmt {
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
	     ^ast.Oop_Simple_Stmt:
		if collect_declarations {
			checker_collect_stmt_entities(ctx, stmt)
		}
	case ^ast.Oop_Load_Stmt:
		checker_check_oop_load_stmt(ctx, n)
	case ^ast.Data_Inline_Decl:
		rhs := checker_check_expr_with_unresolved_value_diagnostics(ctx, n.expr)
		if checker_data_inline_decl_has_inferred_value_constructor(n) &&
		   checker_type_is_unknown(rhs.type) {
			checker_add_diagnostic(
				ctx,
				.Invalid_Syntax_Form,
				checker_expr_range(n.expr),
				"inline DATA declaration cannot use VALUE #(...)",
			)
		}
		checker_collect_inferred_expr_decl(
			ctx,
			n.name.text,
			.Variable,
			n.name.range,
			&n.node.decl_base.stmt_base,
			rhs.type,
		)
	case ^ast.Assign_Stmt:
		checker_check_assignment_stmt(ctx, n.lhs, n.rhs, chain_lhs = n.chain_lhs[:])
	case ^ast.Downcast_Assign_Stmt:
		checker_check_assignment_stmt(ctx, n.lhs, n.rhs, downcast = true)
	case ^ast.Expr_Stmt:
		checker_check_expr(ctx, n.expr)
	case ^ast.Clear_Stmt:
		for operand in n.operands {
			checker_check_expr_with_unresolved_value_diagnostics(ctx, operand.target, .Value, true)
			checker_check_expr_with_unresolved_value_diagnostics(ctx, operand.value)
		}
	case ^ast.Refresh_Stmt:
		for operand in n.operands {
			checker_check_expr(ctx, operand.target, .Value, true)
		}
	case ^ast.Free_Stmt:
		for operand in n.operands {
			checker_check_expr(ctx, operand.target, .Value, true)
		}
		checker_check_expr(ctx, n.memory_id)
	case ^ast.Unassign_Stmt:
		for operand in n.operands {
			checker_check_expr(ctx, operand.target, .Value, true)
		}
	case ^ast.Move_Stmt:
		for entry in n.entries {
			checker_check_assignment_stmt(ctx, entry.target, entry.source)
		}
	case ^ast.Move_Corresponding_Stmt:
		checker_check_move_corresponding_stmt(ctx, n)
	case ^ast.Add_Stmt:
		for entry in n.entries {
			checker_check_expr(ctx, entry.source)
			checker_check_expr(ctx, entry.target, .Value, true)
			checker_check_expr(ctx, entry.result, .Value, true)
		}
	case ^ast.Subtract_Stmt:
		for entry in n.entries {
			checker_check_expr(ctx, entry.source)
			checker_check_expr(ctx, entry.target, .Value, true)
			checker_check_expr(ctx, entry.result, .Value, true)
		}
	case ^ast.Multiply_Stmt:
		for entry in n.entries {
			checker_check_expr(ctx, entry.target, .Value, true)
			checker_check_expr(ctx, entry.source)
			checker_check_expr(ctx, entry.result, .Value, true)
		}
	case ^ast.Divide_Stmt:
		for entry in n.entries {
			checker_check_expr(ctx, entry.source)
			checker_check_expr(ctx, entry.target, .Value, true)
			checker_check_expr(ctx, entry.result, .Value, true)
		}
	case ^ast.Compute_Stmt:
		for entry in n.entries {
			checker_check_assignment_stmt(ctx, entry.target, entry.source)
		}
	case ^ast.Concatenate_Stmt:
		checker_check_concatenate_stmt(ctx, n)
	case ^ast.Split_Stmt:
		checker_check_split_stmt(ctx, n)
	case ^ast.Condense_Stmt:
		checker_check_condense_stmt(ctx, n)
	case ^ast.Replace_Stmt:
		checker_check_expr(ctx, n.pattern)
		checker_check_expr(ctx, n.target, .Value, true)
		checker_check_expr(ctx, n.replacement)
		checker_check_expr(ctx, n.section_offset)
		checker_check_expr(ctx, n.section_length)
	case ^ast.Translate_Stmt:
		checker_check_expr(ctx, n.target, .Value, true)
		checker_check_expr(ctx, n.operand)
	case ^ast.Shift_Stmt:
		checker_check_shift_stmt(ctx, n)
	case ^ast.Find_Stmt:
		checker_check_find_stmt(ctx, n)
	case ^ast.Search_Stmt:
		checker_check_expr(ctx, n.target)
		checker_check_expr(ctx, n.pattern)
		checker_check_expr(ctx, n.starting_at)
		checker_check_expr(ctx, n.ending_at)
	case ^ast.Perform_Stmt:
		checker_check_perform_stmt(ctx, n)
	case ^ast.Call_Stmt:
		checker_check_call_stmt(ctx, n)
	case ^ast.Submit_Stmt:
		checker_check_submit_stmt(ctx, n)
	case ^ast.Message_Stmt:
		checker_check_message_stmt(ctx, n)
	case ^ast.Write_Stmt:
		for operand in n.operands {
			checker_check_expr(ctx, operand.value)
			checker_check_expr(ctx, operand.position)
			checker_check_expr(ctx, operand.length)
		}
	case ^ast.Write_To_Stmt:
		for entry in n.entries {
			checker_check_assignment_stmt(ctx, entry.target, entry.source)
		}
	case ^ast.Assert_Stmt:
		checker_check_expr(ctx, n.condition)
	case ^ast.Check_Stmt:
		checker_check_expr(ctx, n.condition)
	case ^ast.Flow_Stmt, ^ast.Transaction_Stmt, ^ast.Macro_Def_Stmt, ^ast.Exec_Sql_Stmt, ^ast.Invalid_Stmt:
	case ^ast.Describe_Stmt:
		checker_check_describe_stmt(ctx, n)
	case ^ast.Runtime_Stmt:
		checker_check_runtime_stmt(ctx, n)
	case ^ast.Set_Handler_Stmt:
		checker_check_expr_list(ctx, n.handlers[:], .Routine)
		checker_check_expr(ctx, n.sender)
		checker_check_expr(ctx, n.activation)
	case ^ast.Import_Stmt:
		checker_check_data_cluster_parameters(ctx, n.parameters[:], true)
		checker_check_data_cluster_medium(ctx, n.medium)
	case ^ast.Export_Stmt:
		checker_check_data_cluster_parameters(ctx, n.parameters[:])
		checker_check_data_cluster_medium(ctx, n.medium)
	case ^ast.Bit_Stmt:
		checker_check_expr(ctx, n.position)
		checker_check_expr(ctx, n.source)
		checker_check_expr(ctx, n.target, .Value, n.kind == .Set)
		checker_check_expr(ctx, n.value)
	case ^ast.Locale_Stmt:
		checker_check_expr(ctx, n.language, .Value, n.kind == .Get)
		checker_check_expr(ctx, n.country, .Value, n.kind == .Get)
		checker_check_expr(ctx, n.modifier, .Value, n.kind == .Get)
	case ^ast.Set_Cursor_Stmt:
		checker_check_expr(ctx, n.field)
		checker_check_expr(ctx, n.offset)
		checker_check_expr(ctx, n.line)
		checker_check_expr(ctx, n.column)
	case ^ast.Receive_Results_Stmt:
		checker_check_expr(ctx, n.target)
		checker_check_call_stmt_args(ctx, n.named_args[:], nil, n.range)
	case ^ast.Raise_Stmt:
		if n.target_type {
			checker_check_expr(ctx, n.target, .Type)
		} else {
			checker_check_expr(ctx, n.target)
		}
		checker_check_expr_list(ctx, n.operands[:])
	case ^ast.Authority_Check_Stmt:
		checker_check_expr(ctx, n.object)
		checker_check_expr_list(ctx, n.operands[:])
		for id in n.ids {
			checker_check_expr(ctx, id.id)
			checker_check_expr(ctx, id.field)
		}
	case ^ast.Field_Groups_Stmt:
		checker_check_expr_list(ctx, n.groups[:])
	case ^ast.Insert_Dummy_Stmt:
		checker_check_expr(ctx, n.target, .Value, true)
	case ^ast.Field_Stmt:
		checker_check_expr_list(ctx, n.operands[:])
	case ^ast.Assign_Field_Stmt:
		checker_check_assign_field_stmt(ctx, n)
	case ^ast.Create_Object_Stmt:
		checker_check_create_object_stmt(ctx, n)
	case ^ast.Create_Data_Stmt:
		checker_check_create_data_stmt(ctx, n)
	case ^ast.Text_Transform_Stmt:
		checker_check_expr_list(ctx, n.operands[:], .Value, true)
	case ^ast.Wait_Stmt:
		checker_check_expr(ctx, n.condition)
		checker_check_expr(ctx, n.duration)
	case ^ast.Convert_Time_Stamp_Stmt:
		checker_check_convert_time_stamp_stmt(ctx, n)
	case ^ast.List_Control_Stmt:
		checker_check_expr_list(ctx, n.operands[:])
	case ^ast.Line_Stmt:
		checker_check_line_stmt(ctx, n)
	case ^ast.Macro_Call_Stmt:
		checker_check_expr_list(ctx, n.args[:])
	case ^ast.Selection_Screen_Stmt:
		checker_check_selection_screen_stmt(ctx, n)
	case ^ast.If_Stmt:
		checker_check_expr_with_unresolved_value_diagnostics(ctx, n.condition)
		checker_check_stmt_list(ctx, n.body)
		for clause in n.elseif_clauses {
			checker_check_expr_with_unresolved_value_diagnostics(ctx, clause.condition)
			checker_check_stmt_list(ctx, clause.body)
		}
		if n.else_clause != nil {
			checker_check_stmt_list(ctx, n.else_clause.body)
		}
	case ^ast.Case_Stmt:
		checker_check_case_stmt(ctx, n)
	case ^ast.While_Stmt:
		checker_check_expr(ctx, n.condition)
		checker_check_stmt_list(ctx, n.body)
	case ^ast.Do_Stmt:
		checker_check_expr(ctx, n.count)
		checker_check_stmt_list(ctx, n.body)
	case ^ast.Loop_Stmt:
		checker_check_loop_stmt(ctx, n)
	case ^ast.At_Stmt:
		checker_check_expr(ctx, n.expr)
		checker_check_stmt_list(ctx, n.body)
	case ^ast.Try_Stmt:
		checker_check_stmt_list(ctx, n.body)
		for clause in n.catches {
			catch_type := checker_check_catch_exceptions(ctx, clause.exceptions[:])
			checker_check_catch_into(ctx, clause.into, catch_type)
			checker_check_stmt_list(ctx, clause.body)
		}
		if n.cleanup != nil {
			checker_check_stmt_list(ctx, n.cleanup.body)
		}
	case ^ast.Enhancement_Stmt:
		checker_check_stmt_list(ctx, n.body)
	case ^ast.Enhancement_Section_Stmt:
		checker_check_stmt_list(ctx, n.body)
	case ^ast.Test_Seam_Stmt:
		checker_check_stmt_list(ctx, n.body)
	case ^ast.Test_Injection_Stmt:
		checker_check_stmt_list(ctx, n.body)
	case ^ast.Select_Stmt:
		checker_check_select_stmt(ctx, n)
	case ^ast.Open_Cursor_Stmt:
		handle := checker_check_cursor_handle_expr(ctx, n.handle, true)
		shape := checker_check_sql_select_query(ctx, n.query)
		checker_sql_register_cursor_query(ctx, handle.entity, shape)
	case ^ast.Fetch_Stmt:
		handle := checker_check_cursor_handle_expr(ctx, n.handle, false)
		shape, ok := checker_sql_cursor_query_shape(ctx, handle.entity)
		if !ok {
			shape = checker_sql_unknown_query_shape(ctx)
		}
		checker_check_sql_select_result(ctx, n.result, shape)
		checker_check_expr(ctx, n.package_size)
	case ^ast.Close_Cursor_Stmt:
		checker_check_cursor_handle_expr(ctx, n.handle, false)
	case ^ast.Insert_Stmt:
		checker_check_insert_stmt(ctx, n)
	case ^ast.Append_Stmt:
		checker_check_append_stmt(ctx, n)
	case ^ast.Modify_Stmt:
		checker_check_modify_stmt(ctx, n)
	case ^ast.Sort_Stmt:
		checker_check_sort_stmt(ctx, n)
	case ^ast.Update_Stmt:
		checker_check_update_stmt(ctx, n)
	case ^ast.Delete_Stmt:
		checker_check_delete_stmt(ctx, n)
	case ^ast.Read_Table_Stmt:
		checker_check_read_table_stmt(ctx, n)
	case ^ast.Dataset_Stmt:
		checker_check_dataset_stmt(ctx, n)
	case ^ast.Textpool_Stmt:
		checker_check_expr(ctx, n.program)
		checker_check_expr(ctx, n.table, .Value, n.kind == .Read)
		checker_check_expr(ctx, n.language)
	case ^ast.Generate_Stmt:
		checker_check_expr(ctx, n.source)
		checker_check_expr(ctx, n.name, .Value, true)
		checker_check_expr(ctx, n.program)
		checker_check_expr(ctx, n.dynpro)
		checker_check_expr(ctx, n.message, .Value, true)
		checker_check_expr(ctx, n.line, .Value, true)
		checker_check_expr(ctx, n.word, .Value, true)
		checker_check_expr(ctx, n.offset, .Value, true)
	}
}

checker_data_inline_decl_has_inferred_value_constructor :: proc(stmt: ^ast.Data_Inline_Decl) -> bool {
	if stmt == nil || stmt.expr == nil {
		return false
	}
	return checker_expr_is_inferred_value_constructor(stmt.expr)
}

checker_expr_is_inferred_value_constructor :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Paren_Expr:
		return checker_expr_is_inferred_value_constructor(n.expr)
	case ^ast.Constructor_Expr:
		return n.kind == .Value && checker_expr_is_inferred_type_ref(n.type_ref)
	}
	return false
}

checker_check_convert_time_stamp_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Convert_Time_Stamp_Stmt) {
	timestamp_type := checker_builtin_type_from_name(ctx.checker, "timestamp")
	date_type := checker_builtin_type_from_name(ctx.checker, "d")
	time_type := checker_builtin_type_from_name(ctx.checker, "t")
	dst_type := checker_builtin_type_from_name(ctx.checker, "c")

	switch stmt.kind {
	case .Date_Time_To_Time_Stamp:
		checker_check_expr(ctx, stmt.date)
		checker_check_expr(ctx, stmt.time)
		checker_check_expr(ctx, stmt.daylight_saving_time)
		checker_check_type_hinted_target(ctx, stmt.time_stamp, timestamp_type)
		checker_check_expr(ctx, stmt.time_zone)
	case .Time_Stamp_To_Date_Time:
		checker_check_expr(ctx, stmt.time_stamp)
		checker_check_expr(ctx, stmt.time_zone)
		checker_check_type_hinted_target(ctx, stmt.date, date_type)
		checker_check_type_hinted_target(ctx, stmt.time, time_type)
		checker_check_type_hinted_target(ctx, stmt.daylight_saving_time, dst_type)
	}
}

checker_check_type_hinted_target :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	type_hint: ^Type,
) -> Operand {
	if expr == nil {
		return checker_invalid_operand()
	}
	local := ctx^
	local.type_hint = type_hint
	local.type_hint_expr = expr
	return checker_check_expr(&local, expr, .Value, true)
}

checker_check_describe_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Describe_Stmt) {
	for entry in stmt.entries {
		source := checker_check_expr(ctx, entry.source)
		checker_check_unresolved_variable_operand(ctx, entry.source, source)

		target_ctx := ctx^
		target_ctx.type_hint = checker_describe_target_type_hint(ctx, entry.target_kind)
		target_ctx.type_hint_expr = entry.target
		target := checker_check_expr(&target_ctx, entry.target, .Value, true)
		checker_check_unresolved_variable_operand(ctx, entry.target, target)
	}
}

checker_check_catch_exceptions :: proc(ctx: ^Checker_Context, exceptions: []^ast.Expr) -> ^Type {
	target_type := project_type_unknown(ctx.project)
	for exception, i in exceptions {
		typ, entity := checker_type_from_expr(ctx, exception, .Type)
		node: ^ast.Node
		if exception != nil {
			node = &exception.expr_base
		}
		checker_record_operand(ctx, node, .Type, typ, entity)
		if i == 0 {
			target_type = checker_catch_exception_target_type(ctx, typ)
		}
	}
	return target_type
}

checker_catch_exception_target_type :: proc(ctx: ^Checker_Context, typ: ^Type) -> ^Type {
	if typ == nil || checker_type_is_unknown(typ) {
		return project_type_unknown(ctx.project)
	}
	if checker_type_is_ref(typ) {
		return typ
	}
	return project_type_ref(ctx.project, typ)
}

checker_check_catch_into :: proc(
	ctx: ^Checker_Context,
	into: ^ast.Expr,
	type_hint: ^Type,
) -> Operand {
	if into == nil {
		return checker_invalid_operand()
	}
	local := ctx^
	local.type_hint = type_hint
	local.type_hint_expr = into
	target := checker_check_expr(&local, into, .Value, true)
	checker_check_catch_into_compatibility(ctx, type_hint, target.type, checker_expr_range(into))
	return target
}

checker_check_catch_into_compatibility :: proc(
	ctx: ^Checker_Context,
	expected: ^Type,
	actual: ^Type,
	range: Range,
) {
	if checker_type_is_unknown(expected) || checker_type_is_unknown(actual) {
		return
	}
	expected_ref := checker_type_is_ref(expected)
	actual_ref := checker_type_is_ref(actual)
	if expected_ref != actual_ref {
		checker_add_catch_into_incompatible_diagnostic(ctx, expected, actual, range)
		return
	}
	if !expected_ref {
		return
	}
	if actual_target, actual_target_ok := checker_ref_target(ctx, actual); actual_target_ok {
		if actual_target.kind == .Object_Generic {
			return
		}
		if actual_target.kind == .Data_Generic ||
		   (actual_target.kind == .Data && actual_target.entity != nil) {
			checker_add_catch_into_incompatible_diagnostic(ctx, expected, actual, range)
			return
		}
	}
	checker_check_assignment_compatibility(ctx, expected, actual, range)
}

checker_add_catch_into_incompatible_diagnostic :: proc(
	ctx: ^Checker_Context,
	expected: ^Type,
	actual: ^Type,
	range: Range,
) {
	checker_add_diagnostic(
		ctx,
		.Incompatible_Assignment_Type,
		range,
		checker_type_mismatch_message(ctx, "incompatible assignment", expected, actual),
	)
}

checker_describe_target_type_hint :: proc(
	ctx: ^Checker_Context,
	kind: ast.Describe_Target_Kind,
) -> ^Type {
	#partial switch kind {
	case .Lines, .Length:
		return checker_builtin_type_from_name(ctx.checker, "i")
	case .Type:
		return checker_builtin_type_from_name(ctx.checker, "c")
	case:
		return nil
	}
}

checker_check_concatenate_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Concatenate_Stmt) {
	for entry in stmt.entries {
		if entry.lines_of {
			for source_expr in entry.sources {
				checker_check_concatenate_lines_source(ctx, source_expr)
			}
		} else {
			checker_check_expr_list(ctx, entry.sources[:])
		}
		target := checker_check_expr(ctx, entry.target, .Value, true)
		checker_check_concatenate_target(ctx, entry.target, target)
		checker_check_expr(ctx, entry.separator)
	}
}

checker_check_concatenate_lines_source :: proc(ctx: ^Checker_Context, expr: ^ast.Expr) -> Operand {
	source := checker_check_expr(ctx, expr)
	if checker_check_unresolved_variable_operand(ctx, expr, source) || checker_type_is_unknown(source.type) {
		return source
	}
	if !checker_type_is_table_like(ctx, source.type) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Concatenate_Operand,
			checker_expr_range(expr),
			"CONCATENATE LINES OF source is not an internal table",
		)
	}
	return source
}

checker_check_concatenate_target :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, target: Operand) {
	if checker_check_unresolved_variable_operand(ctx, expr, target) || checker_type_is_unknown(target.type) {
		return
	}
	if !checker_operand_is_writable(target) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Concatenate_Operand,
			checker_expr_range(expr),
			"CONCATENATE INTO target is not writable",
		)
	}
}

checker_check_split_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Split_Stmt) {
	for entry in stmt.entries {
		checker_check_split_source(ctx, entry.source)
		checker_check_expr(ctx, entry.separator)
		if entry.into_table {
			for target_expr in entry.targets {
				target := checker_check_expr(ctx, target_expr, .Value, true)
				checker_check_split_table_target(ctx, target_expr, target)
			}
		} else {
			for target_expr in entry.targets {
				target := checker_check_expr(ctx, target_expr, .Value, true)
				checker_check_split_target(ctx, target_expr, target)
			}
		}
	}
}

checker_check_split_source :: proc(ctx: ^Checker_Context, expr: ^ast.Expr) -> Operand {
	source := checker_check_expr(ctx, expr)
	if checker_check_unresolved_variable_operand(ctx, expr, source) || checker_type_is_unknown(source.type) {
		return source
	}
	if ok, known := checker_split_source_type_supported(ctx, source.type); known && !ok {
		checker_add_diagnostic(
			ctx,
			.Invalid_Split_Operand,
			checker_expr_range(expr),
			"SPLIT source is not character-like or byte-like",
		)
	}
	return source
}

checker_split_source_type_supported :: proc(ctx: ^Checker_Context, typ: ^Type) -> (bool, bool) {
	return checker_text_or_byte_type_supported(ctx, typ)
}

checker_text_or_byte_type_supported :: proc(ctx: ^Checker_Context, typ: ^Type) -> (bool, bool) {
	if checker_type_is_unknown(typ) {
		return false, false
	}
	if checker_type_structure(typ) != nil || checker_type_is_table_like(ctx, typ) || checker_type_is_ref(typ) {
		return false, true
	}
	name, ok := checker_type_builtin_name(ctx, typ)
	if !ok {
		return false, false
	}
	switch name {
	case "c", "n", "d", "t", "string", "abap_bool", "x", "xstring", "clike", "csequence", "xsequence":
		return true, true
	case "any", "data", "simple":
		return false, false
	}
	return false, true
}

checker_character_like_type_supported :: proc(ctx: ^Checker_Context, typ: ^Type) -> (bool, bool) {
	if checker_type_is_unknown(typ) {
		return false, false
	}
	if checker_type_structure(typ) != nil || checker_type_is_table_like(ctx, typ) || checker_type_is_ref(typ) {
		return false, true
	}
	name, ok := checker_type_builtin_name(ctx, typ)
	if !ok {
		return false, false
	}
	if checker_builtin_clike_name(name) || name == "clike" || name == "csequence" {
		return true, true
	}
	switch name {
	case "any", "data", "simple":
		return false, false
	}
	return false, true
}

checker_check_split_table_target :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, target: Operand) {
	if checker_check_unresolved_variable_operand(ctx, expr, target) || checker_type_is_unknown(target.type) {
		return
	}
	if !checker_operand_is_writable(target) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Split_Operand,
			checker_expr_range(expr),
			"SPLIT INTO TABLE target is not writable",
		)
		return
	}
	if !checker_type_is_table_like(ctx, target.type) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Split_Operand,
			checker_expr_range(expr),
			"SPLIT INTO TABLE target is not an internal table",
		)
	}
}

checker_check_split_target :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, target: Operand) {
	if checker_check_unresolved_variable_operand(ctx, expr, target) || checker_type_is_unknown(target.type) {
		return
	}
	if !checker_operand_is_writable(target) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Split_Operand,
			checker_expr_range(expr),
			"SPLIT INTO target is not writable",
		)
	}
}

checker_check_condense_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Condense_Stmt) {
	target := checker_check_expr(ctx, stmt.target, .Value, true)
	checker_check_condense_target(ctx, stmt.target, target)
}

checker_check_condense_target :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, target: Operand) {
	if checker_check_unresolved_variable_operand(ctx, expr, target) || checker_type_is_unknown(target.type) {
		return
	}
	if !checker_operand_is_writable(target) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			checker_expr_range(expr),
			"CONDENSE target is not writable",
		)
		return
	}
	if ok, known := checker_character_like_type_supported(ctx, target.type); known && !ok {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			checker_expr_range(expr),
			"CONDENSE target is not character-like",
		)
	}
}

checker_check_shift_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Shift_Stmt) {
	target := checker_check_expr(ctx, stmt.target, .Value, true)
	checker_check_shift_target(ctx, stmt.target, target)
	checker_check_shift_places(ctx, stmt.places)
	checker_check_shift_text_operand(
		ctx,
		stmt.delete_pattern,
		"SHIFT DELETING pattern is not character-like or byte-like",
	)
}

checker_check_shift_target :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, target: Operand) {
	if checker_check_unresolved_variable_operand(ctx, expr, target) || checker_type_is_unknown(target.type) {
		return
	}
	if !checker_operand_is_writable(target) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			checker_expr_range(expr),
			"SHIFT target is not writable",
		)
		return
	}
	if ok, known := checker_text_or_byte_type_supported(ctx, target.type); known && !ok {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			checker_expr_range(expr),
			"SHIFT target is not character-like or byte-like",
		)
	}
}

checker_check_shift_places :: proc(ctx: ^Checker_Context, expr: ^ast.Expr) {
	if expr == nil {
		return
	}
	int_type := checker_builtin_type_from_name(ctx.checker, "i")
	local := ctx^
	local.type_hint = int_type
	local.type_hint_expr = expr
	local.diagnose_unresolved_value_refs = true
	operand := checker_check_expr(&local, expr)
	if checker_check_unresolved_variable_operand(ctx, expr, operand) {
		return
	}
	checker_check_integer_compatible_type(
		ctx,
		operand.type,
		checker_expr_range(expr),
		"SHIFT BY operand is not integer-compatible",
	)
}

checker_check_shift_text_operand :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, message: string) {
	if expr == nil {
		return
	}
	operand := checker_check_expr(ctx, expr)
	if checker_check_unresolved_variable_operand(ctx, expr, operand) || checker_type_is_unknown(operand.type) {
		return
	}
	if ok, known := checker_text_or_byte_type_supported(ctx, operand.type); known && !ok {
		checker_add_diagnostic(ctx, .Invalid_Syntax_Form, checker_expr_range(expr), message)
	}
}

checker_check_find_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Find_Stmt) {
	checker_check_find_text_operand(ctx, stmt.pattern, "FIND pattern is not character-like or byte-like")
	target := checker_check_expr(ctx, stmt.target)
	checker_check_find_target(ctx, stmt.target, target, stmt.in_table)
	checker_check_find_integer_value(ctx, stmt.section_offset)
	checker_check_find_integer_value(ctx, stmt.section_length)
	checker_check_find_integer_target(ctx, stmt.match_offset)
	checker_check_find_integer_target(ctx, stmt.match_length)
	checker_check_find_integer_target(ctx, stmt.match_line)
	checker_check_find_integer_target(ctx, stmt.match_count)
	checker_check_find_write_target(ctx, stmt.results, nil, "FIND RESULTS target is not writable")
	for submatch in stmt.submatches {
		checker_check_find_write_target(ctx, submatch, nil, "FIND SUBMATCHES target is not writable")
	}
}

checker_check_find_target :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	target: Operand,
	in_table: bool,
) {
	if checker_check_unresolved_variable_operand(ctx, expr, target) || checker_type_is_unknown(target.type) {
		return
	}
	if in_table {
		if !checker_type_is_table_like(ctx, target.type) {
			checker_add_diagnostic(
				ctx,
				.Invalid_Syntax_Form,
				checker_expr_range(expr),
				"FIND IN TABLE target is not an internal table",
			)
		}
		return
	}
	if ok, known := checker_text_or_byte_type_supported(ctx, target.type); known && !ok {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			checker_expr_range(expr),
			"FIND target is not character-like or byte-like",
		)
	}
}

checker_check_find_text_operand :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, message: string) {
	operand := checker_check_expr(ctx, expr)
	if checker_check_unresolved_variable_operand(ctx, expr, operand) || checker_type_is_unknown(operand.type) {
		return
	}
	if ok, known := checker_text_or_byte_type_supported(ctx, operand.type); known && !ok {
		checker_add_diagnostic(ctx, .Invalid_Syntax_Form, checker_expr_range(expr), message)
	}
}

checker_check_find_integer_value :: proc(ctx: ^Checker_Context, expr: ^ast.Expr) {
	if expr == nil {
		return
	}
	int_type := checker_builtin_type_from_name(ctx.checker, "i")
	local := ctx^
	local.type_hint = int_type
	local.type_hint_expr = expr
	operand := checker_check_expr(&local, expr)
	checker_check_find_integer_type(ctx, operand.type, checker_expr_range(expr))
}

checker_check_find_integer_target :: proc(ctx: ^Checker_Context, expr: ^ast.Expr) {
	int_type := checker_builtin_type_from_name(ctx.checker, "i")
	target := checker_check_find_write_target(ctx, expr, int_type, "FIND MATCH target is not writable")
	checker_check_find_integer_type(ctx, target.type, checker_expr_range(expr))
}

checker_check_find_write_target :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	type_hint: ^Type,
	message: string,
) -> Operand {
	if expr == nil {
		return checker_invalid_operand()
	}
	local := ctx^
	local.type_hint = type_hint
	local.type_hint_expr = expr
	target := checker_check_expr(&local, expr, .Value, true)
	if checker_check_unresolved_variable_operand(ctx, expr, target) || checker_type_is_unknown(target.type) {
		return target
	}
	if !checker_operand_is_writable(target) {
		checker_add_diagnostic(ctx, .Invalid_Syntax_Form, checker_expr_range(expr), message)
	}
	return target
}

checker_check_find_integer_type :: proc(ctx: ^Checker_Context, typ: ^Type, range: Range) {
	checker_check_integer_compatible_type(ctx, typ, range, "FIND numeric operand is not integer-compatible")
}

checker_check_integer_compatible_type :: proc(
	ctx: ^Checker_Context,
	typ: ^Type,
	range: Range,
	message: string,
) {
	if checker_type_is_unknown(typ) {
		return
	}
	if checker_type_structure(typ) != nil || checker_type_is_table_like(ctx, typ) || checker_type_is_ref(typ) {
		checker_add_diagnostic(ctx, .Invalid_Syntax_Form, range, message)
		return
	}
	name, ok := checker_type_builtin_name(ctx, typ)
	if !ok {
		return
	}
	group := checker_scalar_group(name)
	if group != .Numeric && name != "n" {
		checker_add_diagnostic(ctx, .Invalid_Syntax_Form, range, message)
	}
}

checker_check_case_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Case_Stmt) {
	checker_check_expr(ctx, stmt.expr)
	if len(stmt.whens) == 0 {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			stmt.range,
			"CASE requires at least one WHEN branch",
		)
	}
	seen_others := false
	for clause in stmt.whens {
		if seen_others {
			checker_add_diagnostic(
				ctx,
				.Invalid_Syntax_Form,
				clause.range,
				"WHEN OTHERS must be the last CASE branch",
			)
		}
		if clause.is_others {
			seen_others = true
			if len(clause.operands) > 0 {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					clause.range,
					"WHEN OTHERS cannot have operands",
				)
			}
		} else {
			if len(clause.operands) == 0 {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					clause.range,
					"WHEN requires at least one operand",
				)
			}
			checker_check_expr_list(ctx, clause.operands[:])
		}
		checker_check_stmt_list(ctx, clause.body)
	}
	checker_check_stmt_list(ctx, stmt.recovery)
}

checker_check_unresolved_variable_operand :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	operand: Operand,
) -> bool {
	if !checker_type_is_unknown(operand.type) || operand.entity != nil {
		return false
	}
	if name, range, unresolved := checker_simple_unresolved_variable_expr(expr); unresolved {
		return checker_check_unresolved_named_operand(ctx, name, range, operand)
	}
	return false
}

checker_check_expr_list :: proc(
	ctx: ^Checker_Context,
	exprs: []^ast.Expr,
	namespace: Namespace = .Value,
	lhs := false,
) {
	for expr in exprs {
		checker_check_expr(ctx, expr, namespace, lhs)
	}
}

checker_check_move_corresponding_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Move_Corresponding_Stmt) {
	for entry in stmt.entries {
		checker_check_move_corresponding_entry(ctx, entry)
	}
}

checker_check_move_corresponding_entry :: proc(ctx: ^Checker_Context, entry: ast.Move_Entry_Clause) {
	diagnose_unresolved := checker_should_diagnose_unresolved_value_operand(ctx)

	source := checker_check_expr(ctx, entry.source)
	if diagnose_unresolved {
		checker_check_unresolved_variable_operand(ctx, entry.source, source)
	}

	target := checker_check_expr(ctx, entry.target, .Value, true)
	if diagnose_unresolved {
		checker_check_unresolved_variable_operand(ctx, entry.target, target)
	}

	source_structure, source_kind := checker_move_corresponding_structure(ctx, entry.source, source, true)
	target_structure, target_kind := checker_move_corresponding_structure(ctx, entry.target, target, false)
	if source_structure == nil || target_structure == nil {
		return
	}
	if source_kind != target_kind {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			checker_expr_range(entry.target),
			"MOVE-CORRESPONDING source and target must both be structures or both be internal tables",
		)
		return
	}
	checker_check_move_corresponding_fields(ctx, source_structure, target_structure, checker_expr_range(entry.source))
}

checker_move_corresponding_structure :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	operand: Operand,
	source: bool,
) -> (
	^Structure,
	Checker_Move_Corresponding_Operand_Kind,
) {
	if checker_type_is_unknown(operand.type) {
		return nil, .Unknown
	}
	if checker_type_is_table_like(ctx, operand.type) {
		row_type := checker_type_row(ctx, operand.type)
		if checker_type_is_unknown(row_type) {
			return nil, .Unknown
		}
		if structure := checker_type_structure(row_type); structure != nil {
			return structure, .Table
		}
		checker_add_move_corresponding_operand_diagnostic(ctx, expr, source, true)
		return nil, .Invalid
	}
	if structure := checker_type_structure(operand.type); structure != nil {
		return structure, .Structure
	}
	checker_add_move_corresponding_operand_diagnostic(ctx, expr, source, false)
	return nil, .Invalid
}

checker_add_move_corresponding_operand_diagnostic :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	source: bool,
	table_row: bool,
) {
	message := "MOVE-CORRESPONDING source is not a structure or internal table"
	if source {
		if table_row {
			message = "MOVE-CORRESPONDING source row is not a structure"
		}
	} else {
		message = "MOVE-CORRESPONDING target is not a structure or internal table"
		if table_row {
			message = "MOVE-CORRESPONDING target row is not a structure"
		}
	}
	checker_add_diagnostic(ctx, .Invalid_Syntax_Form, checker_expr_range(expr), message)
}

checker_check_move_corresponding_fields :: proc(
	ctx: ^Checker_Context,
	source_structure: ^Structure,
	target_structure: ^Structure,
	range: Range,
) {
	for target_field in target_structure.fields {
		if target_field == nil {
			continue
		}
		source_field, ok := checker_lookup_structure_field(source_structure, target_field.name)
		if !ok || source_field == nil {
			continue
		}
		checker_check_entity_for_operand(ctx, source_field)
		checker_check_entity_for_operand(ctx, target_field)
		source_type := source_field.type if source_field.type != nil else project_type_unknown(ctx.project)
		target_type := target_field.type if target_field.type != nil else project_type_unknown(ctx.project)
		checker_check_assignment_compatibility(ctx, source_type, target_type, range)
	}
}

checker_check_assignment_stmt :: proc(
	ctx: ^Checker_Context,
	lhs_expr: ^ast.Expr,
	rhs_expr: ^ast.Expr,
	downcast := false,
	chain_lhs: []^ast.Expr = nil,
) {
	diagnose_unresolved := checker_should_diagnose_unresolved_value_operand(ctx)

	target_exprs := make([dynamic]^ast.Expr, 0, 1 + len(chain_lhs), context.temp_allocator)
	append(&target_exprs, lhs_expr)
	for target in chain_lhs {
		append(&target_exprs, target)
	}

	targets := make([dynamic]Operand, 0, len(target_exprs), context.temp_allocator)
	for target_expr in target_exprs {
		target := checker_check_expr(ctx, target_expr, .Value, true)
		if diagnose_unresolved {
			checker_check_unresolved_variable_operand(ctx, target_expr, target)
		}
		append(&targets, target)
	}

	rhs_ctx := ctx^
	rhs_ctx.type_hint = targets[len(targets) - 1].type
	rhs_ctx.type_hint_expr = target_exprs[len(target_exprs) - 1]
	rhs_ctx.diagnose_unresolved_value_refs = diagnose_unresolved
	rhs := checker_check_expr(&rhs_ctx, rhs_expr)
	if diagnose_unresolved {
		checker_check_unresolved_variable_operand(ctx, rhs_expr, rhs)
	}

	source_type := rhs.type
	source_range := checker_expr_range(rhs_expr)
	for i := len(targets) - 1; i >= 0; i -= 1 {
		checker_check_assignment_compatibility(ctx, source_type, targets[i].type, source_range, downcast)
		source_type = targets[i].type
		source_range = checker_expr_range(target_exprs[i])
	}
}

checker_check_assignment_compatibility :: proc(
	ctx: ^Checker_Context,
	src: ^Type,
	dst: ^Type,
	range: Range,
	downcast := false,
) {
	if ok, known := checker_type_assignment_compatible(ctx, src, dst, downcast); known && !ok {
		checker_add_diagnostic(ctx, .Incompatible_Assignment_Type, range, checker_type_mismatch_message(ctx, "incompatible assignment", src, dst))
	}
}

checker_check_argument_compatibility :: proc(
	ctx: ^Checker_Context,
	src: ^Type,
	dst: ^Type,
	range: Range,
) {
	if ok, known := checker_type_argument_compatible(ctx, src, dst); known && !ok {
		checker_add_diagnostic(ctx, .Incompatible_Argument_Type, range, checker_type_mismatch_message(ctx, "incompatible argument", src, dst))
	}
}

checker_check_table_line_target :: proc(
	ctx: ^Checker_Context,
	target: ^ast.Expr,
	row_type: ^Type,
	target_kind: ast.Loop_Target_Kind,
) {
	if target == nil {
		return
	}
	hint := row_type
	if target_kind == .Reference_Into {
		hint = project_type_ref(ctx.project, row_type)
	}
	target_ctx := ctx^
	target_ctx.type_hint = hint
	target_ctx.type_hint_expr = target
	operand := checker_check_expr(&target_ctx, target, .Value, true)
	checker_check_assignment_compatibility(ctx, hint, operand.type, checker_expr_range(target))
}

checker_check_loop_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Loop_Stmt) {
	source := checker_check_expr(ctx, stmt.source)
	row_type := checker_loop_source_row_type(ctx, stmt, source)
	row_structure := checker_type_structure(row_type)
	checker_check_table_line_target(ctx, stmt.target, row_type, stmt.target_kind)
	checker_check_expr(ctx, stmt.target_casting_type)
	checker_check_expr(ctx, stmt.from)
	checker_check_expr(ctx, stmt.to)
	checker_check_table_key_selector(ctx, stmt.using_key)
	checker_check_loop_transporting_fields(ctx, stmt.transporting_fields[:], row_type, row_structure)
	checker_check_internal_table_where_expr(ctx, stmt.where_cond, row_type, row_structure)
	checker_check_expr(ctx, stmt.group_by)
	checker_check_table_line_target(ctx, stmt.group_target, row_type, stmt.group_target_kind)
	checker_check_stmt_list(ctx, stmt.body)
}

checker_loop_source_row_type :: proc(
	ctx: ^Checker_Context,
	stmt: ^ast.Loop_Stmt,
	source: Operand,
) -> ^Type {
	if stmt.source_kind == .Group {
		return checker_type_row(ctx, source.type)
	}
	if checker_type_is_table_like(ctx, source.type) {
		return checker_type_row(ctx, source.type)
	}
	if checker_type_is_range_like(ctx, source.type) {
		return source.type
	}
	if checker_type_is_unknown(source.type) && source.entity == nil {
		if name, range, unresolved := checker_loop_source_unresolved_reference(stmt.source); unresolved {
			checker_add_diagnostic(
				ctx,
				.Unresolved_Reference,
				range,
				checker_unresolved_variable_message(name),
			)
		}
		return project_type_unknown(ctx.project)
	}
	if !checker_type_is_unknown(source.type) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Loop_Source,
			checker_expr_range(stmt.source),
			"LOOP AT source is not an internal table or range",
		)
	}
	return project_type_unknown(ctx.project)
}

checker_loop_source_unresolved_reference :: proc(expr: ^ast.Expr) -> (string, Range, bool) {
	if expr == nil {
		return "", {}, false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		return n.name, n.range, n.name != ""
	case ^ast.Type_Ref_Expr:
		if n.raw_operand || n.name.text == "" || n.base_name.text != "" || len(n.path) > 0 {
			return "", {}, false
		}
		return n.name.text, n.name.range, true
	case ^ast.Host_Expr:
		return checker_loop_source_unresolved_reference(n.value)
	case ^ast.Paren_Expr:
		return checker_loop_source_unresolved_reference(n.expr)
	}
	return "", {}, false
}

checker_type_is_range_like :: proc(ctx: ^Checker_Context, typ: ^Type) -> bool {
	structure := checker_type_structure(typ)
	if structure == nil {
		return false
	}
	names := [?]string{"sign", "option", "low", "high"}
	for name_text in names {
		name := project_intern_lower_ascii(ctx.project, name_text)
		if name == "" {
			return false
		}
		if _, ok := checker_lookup_structure_field(structure, name); !ok {
			return false
		}
	}
	return true
}

checker_check_loop_transporting_fields :: proc(
	ctx: ^Checker_Context,
	fields: []ast.Transporting_Field_Clause,
	row_type: ^Type,
	row_structure: ^Structure,
) {
	for field in fields {
		checker_check_transporting_field(ctx, field, row_type, row_structure)
	}
}

checker_check_transporting_field :: proc(
	ctx: ^Checker_Context,
	field: ast.Transporting_Field_Clause,
	row_type: ^Type,
	row_structure: ^Structure,
) {
	if len(field.path) == 0 {
		return
	}
	current_type := row_type
	current_structure := row_structure
	for segment in field.path {
		if checker_table_component_is_table_line(segment.name.text) {
			current_structure = checker_type_structure(current_type)
			continue
		}
		if current_structure == nil {
			if checker_type_is_unknown(current_type) {
				return
			}
			checker_add_unknown_table_component_diagnostic(ctx, segment.name.text, segment.name.range)
			return
		}
		name := project_intern_lower_ascii(ctx.project, segment.name.text)
		if name == "" {
			return
		}
		entity, ok := checker_lookup_structure_field(current_structure, name)
		if !ok {
			checker_add_diagnostic(ctx, .Unknown_Field, segment.name.range, checker_table_component_message(ctx, "unknown internal table field ", name))
			return
		}
		checker_add_entity_use_at_range(ctx, nil, entity, segment.name.range)
		current_type = entity.type if entity.type != nil else project_type_unknown(ctx.project)
		current_structure = checker_type_structure(current_type)
	}
}

checker_check_read_table_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Read_Table_Stmt) {
	for entry in stmt.entries {
		table := checker_check_expr(ctx, entry.table)
		checker_check_read_table_source(ctx, entry.table, table)
		row_type := checker_type_row(ctx, table.type)
		row_structure := checker_type_structure(row_type)
		checker_check_table_line_target(ctx, entry.into, row_type, .Into)
		checker_check_table_line_target(ctx, entry.assigning, row_type, .Assigning)
		checker_check_table_line_target(ctx, entry.reference_into, row_type, .Reference_Into)
		checker_check_expr(ctx, entry.index)
		checker_check_table_key_selector(ctx, entry.using_key)
		checker_check_read_table_comparing(ctx, entry.comparing[:], row_type, row_structure)
		for key in entry.key_values {
			checker_check_read_table_key_value(ctx, row_type, row_structure, key)
		}
	}
}

checker_check_read_table_source :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, source: Operand) {
	if checker_check_unresolved_variable_operand(ctx, expr, source) || checker_type_is_unknown(source.type) {
		return
	}
	if !checker_type_is_table_like(ctx, source.type) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			checker_expr_range(expr),
			"READ TABLE source is not an internal table",
		)
	}
}

checker_check_read_table_comparing :: proc(
	ctx: ^Checker_Context,
	comparing: []^ast.Expr,
	row_type: ^Type,
	row_structure: ^Structure,
) {
	if checker_read_table_comparing_all_fields(comparing) {
		return
	}
	for expr in comparing {
		if _, ok := checker_check_table_component_expr(ctx, expr, row_type, row_structure, false); ok {
			continue
		}
		checker_check_expr(ctx, expr)
	}
}

checker_read_table_comparing_all_fields :: proc(comparing: []^ast.Expr) -> bool {
	if len(comparing) != 2 {
		return false
	}
	first, _, first_ok := checker_expr_simple_name(comparing[0])
	second, _, second_ok := checker_expr_simple_name(comparing[1])
	return first_ok &&
	       second_ok &&
	       strings.equal_fold(first, "all") &&
	       strings.equal_fold(second, "fields")
}

checker_check_read_table_key_value :: proc(
	ctx: ^Checker_Context,
	row_type: ^Type,
	row_structure: ^Structure,
	key: ast.Read_Table_Key_Value_Clause,
) {
	checker_check_expr_with_unresolved_value_diagnostics(ctx, key.dynamic_name)
	expected := checker_check_read_table_key_name(ctx, row_type, row_structure, key).type
	value_ctx := ctx^
	value_ctx.diagnose_unresolved_value_refs = true
	if !checker_type_is_unknown(expected) {
		value_ctx.type_hint = expected
		value_ctx.type_hint_expr = key.value
	}
	value := checker_check_expr(&value_ctx, key.value)
	checker_check_assignment_compatibility(ctx, value.type, expected, checker_expr_range(key.value))
}

checker_check_read_table_key_name :: proc(
	ctx: ^Checker_Context,
	row_type: ^Type,
	row_structure: ^Structure,
	key: ast.Read_Table_Key_Value_Clause,
) -> Operand {
	if key.is_dynamic || len(key.path) == 0 {
		return Operand{mode = .Value, type = project_type_unknown(ctx.project)}
	}
	structure := row_structure
	current_type := row_type
	final_field: ^Entity
	for segment, i in key.path {
		if i == 0 && checker_table_component_is_table_line(segment.name.text) {
			continue
		}
		if segment.selector == .Arrow {
			current_type = checker_type_ref_target(ctx, current_type)
			structure = checker_type_structure(current_type)
		}
		if structure == nil {
			if checker_type_is_unknown(current_type) {
				return Operand{mode = .Value, type = project_type_unknown(ctx.project)}
			}
			checker_add_unknown_table_component_diagnostic(ctx, segment.name.text, segment.name.range)
			return Operand{mode = .Value, type = project_type_unknown(ctx.project)}
		}
		name := project_intern_lower_ascii(ctx.project, segment.name.text)
		if name == "" {
			return Operand{mode = .Value, type = project_type_unknown(ctx.project)}
		}
		field, ok := checker_lookup_structure_field(structure, name)
		if !ok {
			checker_add_diagnostic(ctx, .Unknown_Field, segment.name.range, checker_table_component_message(ctx, "unknown internal table field ", name))
			return Operand{mode = .Value, type = project_type_unknown(ctx.project)}
		}
		checker_add_entity_use_at_range(ctx, nil, field, segment.name.range)
		final_field = field
		current_type = field.type
		structure = checker_type_structure(current_type)
	}
	if final_field != nil {
		return Operand{mode = .Field, type = current_type, entity = final_field}
	}
	if len(key.path) == 1 && checker_table_component_is_table_line(key.path[0].name.text) {
		return Operand{mode = .Table_Line, type = row_type}
	}
	return Operand{mode = .Value, type = project_type_unknown(ctx.project)}
}

checker_check_append_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Append_Stmt) {
	target := checker_check_expr(ctx, stmt.target, .Value, true)
	checker_check_append_target(ctx, stmt.target, target)
	row_type := checker_type_row(ctx, target.type)
	if stmt.source != nil {
		expected := row_type
		if stmt.lines_of {
			expected = target.type
		}
		source_ctx := ctx^
		source_ctx.type_hint = expected
		source_ctx.type_hint_expr = stmt.source
		source_ctx.diagnose_unresolved_value_refs = true
		source := checker_check_expr(&source_ctx, stmt.source)
		if !checker_check_unresolved_variable_operand(ctx, stmt.source, source) {
			checker_check_assignment_compatibility(ctx, source.type, expected, checker_expr_range(stmt.source))
		}
	}
	checker_check_table_line_target(ctx, stmt.assigning, row_type, .Assigning)
	checker_check_table_line_target(ctx, stmt.reference_into, row_type, .Reference_Into)
}

checker_check_append_target :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, target: Operand) {
	if checker_check_unresolved_variable_operand(ctx, expr, target) || checker_type_is_unknown(target.type) {
		return
	}
	if !checker_operand_is_writable(target) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Append_Operand,
			checker_expr_range(expr),
			"APPEND target is not writable",
		)
		return
	}
	if !checker_type_is_table_like(ctx, target.type) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Append_Operand,
			checker_expr_range(expr),
			"APPEND target is not an internal table",
		)
	}
}

checker_check_sort_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Sort_Stmt) {
	target := checker_check_expr(ctx, stmt.target, .Value, true)
	checker_check_sort_target(ctx, stmt.target, target)
	row_type := checker_type_row(ctx, target.type)
	row_structure := checker_type_structure(row_type)
	for field in stmt.fields {
		checker_check_sort_field(ctx, field, row_type, row_structure)
	}
}

checker_check_sort_target :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, target: Operand) {
	if checker_check_unresolved_variable_operand(ctx, expr, target) || checker_type_is_unknown(target.type) {
		return
	}
	if !checker_operand_is_writable(target) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Sort_Operand,
			checker_expr_range(expr),
			"SORT target is not writable",
		)
		return
	}
	if !checker_type_is_table_like(ctx, target.type) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Sort_Operand,
			checker_expr_range(expr),
			"SORT target is not an internal table",
		)
	}
}

checker_check_sort_field :: proc(
	ctx: ^Checker_Context,
	field: ast.Sort_Field_Clause,
	row_type: ^Type,
	row_structure: ^Structure,
) {
	if _, ok := checker_check_table_component_expr(ctx, field.expr, row_type, row_structure, false); ok {
		return
	}
	checker_check_expr(ctx, field.expr)
}

checker_check_insert_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Insert_Stmt) {
	if stmt.form == .Db_Table {
		checker_check_sql_insert_stmt(ctx, stmt)
		return
	}
	target := checker_check_expr(ctx, stmt.target, .Value, true)
	checker_check_insert_target(ctx, stmt.target, target)
	row_type := checker_type_row(ctx, target.type)
	if stmt.source != nil {
		source := checker_check_expr(ctx, stmt.source)
		expected := row_type if stmt.form == .Internal_Table && !stmt.from_table else target.type
		checker_check_assignment_compatibility(ctx, source.type, expected, checker_expr_range(stmt.source))
	}
	checker_check_expr(ctx, stmt.index)
	checker_check_table_line_target(ctx, stmt.assigning, row_type, .Assigning)
	checker_check_table_line_target(ctx, stmt.reference_into, row_type, .Reference_Into)
	for assignment in stmt.assignments {
		checker_check_expr(ctx, assignment.name)
		checker_check_expr(ctx, assignment.value)
	}
}

checker_check_insert_target :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, target: Operand) {
	if checker_check_unresolved_variable_operand(ctx, expr, target) || checker_type_is_unknown(target.type) {
		return
	}
	if !checker_operand_is_writable(target) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Insert_Operand,
			checker_expr_range(expr),
			"INSERT target is not writable",
		)
		return
	}
	if !checker_type_is_table_like(ctx, target.type) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Insert_Operand,
			checker_expr_range(expr),
			"INSERT target is not an internal table",
		)
	}
}

checker_check_modify_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Modify_Stmt) {
	if checker_modify_stmt_uses_db_source(ctx, stmt) {
		checker_check_sql_modify_stmt(ctx, stmt)
		return
	}
	target := checker_check_expr(ctx, stmt.target, .Value, true)
	if checker_modify_stmt_is_screen(ctx, stmt) {
		checker_check_unresolved_variable_operand(ctx, stmt.target, target)
		return
	}
	checker_check_modify_target(ctx, stmt.target, target)
	row_type := checker_type_row(ctx, target.type)
	row_structure := checker_type_structure(row_type)
	if stmt.source != nil {
		expected := target.type if stmt.from_table else row_type
		if checker_type_is_unknown(expected) && !checker_type_is_unknown(target.type) {
			expected = target.type
		}
		source_ctx := ctx^
		source_ctx.type_hint = expected
		source_ctx.type_hint_expr = stmt.source
		source_ctx.diagnose_unresolved_value_refs = true
		source := checker_check_expr(&source_ctx, stmt.source)
		if !checker_check_unresolved_variable_operand(ctx, stmt.source, source) {
			checker_check_assignment_compatibility(
				ctx,
				source.type,
				expected,
				checker_expr_range(stmt.source),
			)
		}
		checker_refine_modify_row_type_from_source(ctx, stmt, source.type, &row_type, &row_structure)
	}
	checker_check_expr(ctx, stmt.index)
	checker_check_loop_transporting_fields(ctx, stmt.transporting[:], row_type, row_structure)
	checker_check_internal_table_where_expr(ctx, stmt.where_cond, row_type, row_structure)
}

checker_refine_modify_row_type_from_source :: proc(
	ctx: ^Checker_Context,
	stmt: ^ast.Modify_Stmt,
	source_type: ^Type,
	row_type: ^^Type,
	row_structure: ^^Structure,
) {
	if !checker_type_is_unknown(row_type^) || checker_type_is_unknown(source_type) {
		return
	}
	source_row_type := checker_type_row(ctx, source_type) if stmt.from_table else source_type
	if structure := checker_type_structure(source_row_type); structure != nil {
		row_type^ = source_row_type
		row_structure^ = structure
	}
}

checker_modify_stmt_is_screen :: proc(ctx: ^Checker_Context, stmt: ^ast.Modify_Stmt) -> bool {
	if stmt == nil ||
	   stmt.table_keyword ||
	   stmt.source != nil ||
	   stmt.index != nil ||
	   stmt.where_cond != nil ||
	   len(stmt.transporting) > 0 {
		return false
	}
	name := checker_sql_simple_expr_name(ctx, stmt.target)
	return name == project_intern_lower_ascii(ctx.project, "screen")
}

checker_check_modify_target :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, target: Operand) {
	if checker_check_unresolved_variable_operand(ctx, expr, target) || checker_type_is_unknown(target.type) {
		return
	}
	if !checker_operand_is_writable(target) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Modify_Operand,
			checker_expr_range(expr),
			"MODIFY target is not writable",
		)
		return
	}
	if !checker_type_is_table_like(ctx, target.type) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Modify_Operand,
			checker_expr_range(expr),
			"MODIFY target is not an internal table",
		)
	}
}

checker_modify_stmt_uses_db_source :: proc(ctx: ^Checker_Context, stmt: ^ast.Modify_Stmt) -> bool {
	if stmt == nil || stmt.target == nil || stmt.table_keyword {
		return false
	}
	if stmt.dynamic_source {
		return true
	}
	name := checker_sql_simple_expr_name(ctx, stmt.target)
	if name == "" {
		return false
	}
	_, _, value_ok := checker_lookup_reference(ctx, .Value, name)
	return !value_ok
}

checker_check_update_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Update_Stmt) {
	checker_check_sql_update_stmt(ctx, stmt)
}

checker_check_delete_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Delete_Stmt) {
	if stmt.form == .Db_Table {
		checker_check_sql_delete_stmt(ctx, stmt)
		return
	}
	target := checker_check_expr(ctx, stmt.target, .Value, true)
	checker_check_delete_target(ctx, stmt.target, target)
	row_type := checker_type_row(ctx, target.type)
	row_structure := checker_type_structure(row_type)
	checker_check_expr(ctx, stmt.source)
	checker_check_expr(ctx, stmt.index)
	checker_check_internal_table_where_expr(ctx, stmt.where_cond, row_type, row_structure)
	checker_check_table_key_selector(ctx, stmt.using_key)
	for comparing in stmt.comparing {
		if comparing.all_fields {
			continue
		}
		if _, ok := checker_check_table_component_expr(ctx, comparing.expr, row_type, row_structure, false); ok {
			continue
		}
		checker_check_expr(ctx, comparing.expr)
	}
}

checker_check_delete_target :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, target: Operand) {
	if checker_check_unresolved_variable_operand(ctx, expr, target) || checker_type_is_unknown(target.type) {
		return
	}
	if !checker_operand_is_writable(target) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Delete_Operand,
			checker_expr_range(expr),
			"DELETE target is not writable",
		)
		return
	}
	if !checker_type_is_table_like(ctx, target.type) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Delete_Operand,
			checker_expr_range(expr),
			"DELETE target is not an internal table",
		)
	}
}

checker_check_table_key_selector :: proc(ctx: ^Checker_Context, selector: ast.Table_Key_Selector) {
	checker_check_expr(ctx, selector.dynamic_name)
}

checker_check_internal_table_where_expr :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	row_type: ^Type,
	row_structure: ^Structure,
) -> Operand {
	if expr == nil {
		return checker_invalid_operand()
	}
	if operand, ok := checker_check_table_component_expr(ctx, expr, row_type, row_structure, true); ok {
		return operand
	}
	node := &expr.expr_base
	#partial switch n in expr.derived_expr {
	case ^ast.Binary_Expr:
		left, right: Operand
		if n.op == .And || n.op == .Or {
			left = checker_check_internal_table_where_expr(ctx, n.left, row_type, row_structure)
			right = checker_check_internal_table_where_expr(ctx, n.right, row_type, row_structure)
		} else if checker_internal_table_where_component_binary_op(n.op) {
			left = checker_check_internal_table_where_component_expr(ctx, n.left, row_type, row_structure)
			right = checker_check_expr_with_unresolved_value_diagnostics(ctx, n.right)
			checker_check_internal_table_where_operand_compatibility(
				ctx,
				n.op,
				right.type,
				left.type,
				checker_expr_range(n.right),
			)
		} else {
			left = checker_check_expr_with_unresolved_value_diagnostics(ctx, n.left)
			right = checker_check_expr_with_unresolved_value_diagnostics(ctx, n.right)
		}
		return checker_record_operand(ctx, node, .Value, checker_binary_result_type(ctx, n.op, left, right))
	case ^ast.Unary_Expr:
		operand := checker_check_internal_table_where_expr(ctx, n.expr, row_type, row_structure) if n.op == .Not else checker_check_expr_with_unresolved_value_diagnostics(ctx, n.expr)
		return checker_record_operand(ctx, node, .Value, operand.type)
	case ^ast.Paren_Expr:
		operand := checker_check_internal_table_where_expr(ctx, n.expr, row_type, row_structure)
		return checker_record_operand(ctx, node, operand.mode, operand.type, operand.entity)
	case ^ast.Substring_Expr:
		base := checker_check_internal_table_where_component_expr(ctx, n.base, row_type, row_structure)
		checker_check_expr_with_unresolved_value_diagnostics(ctx, n.offset)
		checker_check_expr_with_unresolved_value_diagnostics(ctx, n.length)
		return checker_record_operand(ctx, node, .Value, base.type, base.entity)
	case ^ast.Between_Expr:
		subject := checker_check_internal_table_where_component_expr(ctx, n.subject, row_type, row_structure)
		low := checker_check_expr_with_unresolved_value_diagnostics(ctx, n.low)
		high := checker_check_expr_with_unresolved_value_diagnostics(ctx, n.high)
		checker_check_internal_table_where_operand_compatibility(
			ctx,
			.Between,
			low.type,
			subject.type,
			checker_expr_range(n.low),
		)
		checker_check_internal_table_where_operand_compatibility(
			ctx,
			.Between,
			high.type,
			subject.type,
			checker_expr_range(n.high),
		)
		return checker_record_operand(ctx, node, .Value, checker_builtin_type_from_name(ctx.checker, "abap_bool"))
	case ^ast.Is_Predicate_Expr:
		checker_check_internal_table_where_component_expr(ctx, n.subject, row_type, row_structure)
		return checker_record_operand(ctx, node, .Value, checker_builtin_type_from_name(ctx.checker, "abap_bool"))
	}
	return checker_check_expr_with_unresolved_value_diagnostics(ctx, expr)
}

checker_check_internal_table_where_component_expr :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	row_type: ^Type,
	row_structure: ^Structure,
) -> Operand {
	if expr == nil {
		return checker_invalid_operand()
	}
	if operand, ok := checker_check_table_component_expr(ctx, expr, row_type, row_structure, true); ok {
		return operand
	}
	node := &expr.expr_base
	#partial switch n in expr.derived_expr {
	case ^ast.Paren_Expr:
		operand := checker_check_internal_table_where_component_expr(ctx, n.expr, row_type, row_structure)
		return checker_record_operand(ctx, node, operand.mode, operand.type, operand.entity)
	case ^ast.Substring_Expr:
		base := checker_check_internal_table_where_component_expr(ctx, n.base, row_type, row_structure)
		checker_check_expr_with_unresolved_value_diagnostics(ctx, n.offset)
		checker_check_expr_with_unresolved_value_diagnostics(ctx, n.length)
		return checker_record_operand(ctx, node, .Value, base.type, base.entity)
	}
	return checker_check_expr_with_unresolved_value_diagnostics(ctx, expr)
}

checker_check_filter_except_in_where_expr :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	left_row_type: ^Type,
	left_row_structure: ^Structure,
	right_row_type: ^Type,
	right_row_structure: ^Structure,
) -> Operand {
	if expr == nil {
		return checker_invalid_operand()
	}
	if operand, ok := checker_check_table_component_expr(ctx, expr, left_row_type, left_row_structure, false); ok {
		return operand
	}
	node := &expr.expr_base
	#partial switch n in expr.derived_expr {
	case ^ast.Binary_Expr:
		left, right: Operand
		if n.op == .And || n.op == .Or {
			left = checker_check_filter_except_in_where_expr(
				ctx,
				n.left,
				left_row_type,
				left_row_structure,
				right_row_type,
				right_row_structure,
			)
			right = checker_check_filter_except_in_where_expr(
				ctx,
				n.right,
				left_row_type,
				left_row_structure,
				right_row_type,
				right_row_structure,
			)
		} else if checker_internal_table_where_component_binary_op(n.op) {
			left = checker_check_filter_except_in_where_side_expr(
				ctx,
				n.left,
				left_row_type,
				left_row_structure,
			)
			right = checker_check_filter_except_in_where_side_expr(
				ctx,
				n.right,
				right_row_type,
				right_row_structure,
			)
			checker_check_internal_table_where_operand_compatibility(
				ctx,
				n.op,
				right.type,
				left.type,
				checker_expr_range(n.right),
			)
		} else {
			left = checker_check_expr_with_unresolved_value_diagnostics(ctx, n.left)
			right = checker_check_expr_with_unresolved_value_diagnostics(ctx, n.right)
		}
		return checker_record_operand(ctx, node, .Value, checker_binary_result_type(ctx, n.op, left, right))
	case ^ast.Unary_Expr:
		operand := checker_check_filter_except_in_where_expr(
			ctx,
			n.expr,
			left_row_type,
			left_row_structure,
			right_row_type,
			right_row_structure,
		) if n.op == .Not else checker_check_expr_with_unresolved_value_diagnostics(ctx, n.expr)
		return checker_record_operand(ctx, node, .Value, operand.type)
	case ^ast.Paren_Expr:
		operand := checker_check_filter_except_in_where_expr(
			ctx,
			n.expr,
			left_row_type,
			left_row_structure,
			right_row_type,
			right_row_structure,
		)
		return checker_record_operand(ctx, node, operand.mode, operand.type, operand.entity)
	case ^ast.Substring_Expr:
		base := checker_check_filter_except_in_where_side_expr(ctx, n.base, left_row_type, left_row_structure)
		checker_check_expr_with_unresolved_value_diagnostics(ctx, n.offset)
		checker_check_expr_with_unresolved_value_diagnostics(ctx, n.length)
		return checker_record_operand(ctx, node, .Value, base.type, base.entity)
	case ^ast.Between_Expr:
		subject := checker_check_filter_except_in_where_side_expr(ctx, n.subject, left_row_type, left_row_structure)
		low := checker_check_filter_except_in_where_side_expr(ctx, n.low, right_row_type, right_row_structure)
		high := checker_check_filter_except_in_where_side_expr(ctx, n.high, right_row_type, right_row_structure)
		checker_check_internal_table_where_operand_compatibility(
			ctx,
			.Between,
			low.type,
			subject.type,
			checker_expr_range(n.low),
		)
		checker_check_internal_table_where_operand_compatibility(
			ctx,
			.Between,
			high.type,
			subject.type,
			checker_expr_range(n.high),
		)
		return checker_record_operand(ctx, node, .Value, checker_builtin_type_from_name(ctx.checker, "abap_bool"))
	case ^ast.Is_Predicate_Expr:
		checker_check_filter_except_in_where_side_expr(ctx, n.subject, left_row_type, left_row_structure)
		return checker_record_operand(ctx, node, .Value, checker_builtin_type_from_name(ctx.checker, "abap_bool"))
	}
	return checker_check_expr_with_unresolved_value_diagnostics(ctx, expr)
}

checker_check_filter_except_in_where_side_expr :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	row_type: ^Type,
	row_structure: ^Structure,
) -> Operand {
	if expr == nil {
		return checker_invalid_operand()
	}
	if operand, ok := checker_check_table_component_expr(ctx, expr, row_type, row_structure, false); ok {
		return operand
	}
	node := &expr.expr_base
	#partial switch n in expr.derived_expr {
	case ^ast.Paren_Expr:
		operand := checker_check_filter_except_in_where_side_expr(ctx, n.expr, row_type, row_structure)
		return checker_record_operand(ctx, node, operand.mode, operand.type, operand.entity)
	case ^ast.Substring_Expr:
		base := checker_check_filter_except_in_where_side_expr(ctx, n.base, row_type, row_structure)
		checker_check_expr_with_unresolved_value_diagnostics(ctx, n.offset)
		checker_check_expr_with_unresolved_value_diagnostics(ctx, n.length)
		return checker_record_operand(ctx, node, .Value, base.type, base.entity)
	}
	return checker_check_expr_with_unresolved_value_diagnostics(ctx, expr)
}

checker_internal_table_where_component_binary_op :: proc(op: ast.Binary_Op) -> bool {
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
	     .Like,
	     .Not_Like,
	     .Bit_O,
	     .Bit_Z,
	     .Bit_M:
		return true
	}
	return false
}

checker_check_internal_table_where_operand_compatibility :: proc(
	ctx: ^Checker_Context,
	op: ast.Binary_Op,
	actual: ^Type,
	expected: ^Type,
	range: Range,
) {
	if !checker_internal_table_where_operator_requires_value_compatibility(op) ||
	   checker_type_is_unknown(actual) ||
	   checker_type_is_unknown(expected) {
		return
	}
	if ok, known := checker_type_assignment_compatible(ctx, actual, expected); known {
		if !ok {
			checker_add_internal_table_where_type_diagnostic(ctx, actual, expected, range)
		}
		return
	}
	if checker_internal_table_where_type_category_mismatch(ctx, actual, expected) {
		checker_add_internal_table_where_type_diagnostic(ctx, actual, expected, range)
	}
}

checker_internal_table_where_operator_requires_value_compatibility :: proc(op: ast.Binary_Op) -> bool {
	#partial switch op {
	case .In, .Not_In:
		return false
	}
	return checker_internal_table_where_component_binary_op(op) || op == .Between
}

checker_internal_table_where_type_category_mismatch :: proc(
	ctx: ^Checker_Context,
	actual: ^Type,
	expected: ^Type,
) -> bool {
	actual_table := checker_type_is_table_like(ctx, actual)
	expected_table := checker_type_is_table_like(ctx, expected)
	actual_structure := checker_type_structure(actual) != nil
	expected_structure := checker_type_structure(expected) != nil
	actual_ref := checker_type_is_ref(actual)
	expected_ref := checker_type_is_ref(expected)
	return actual_table != expected_table ||
	       actual_structure != expected_structure ||
	       actual_ref != expected_ref
}

checker_add_internal_table_where_type_diagnostic :: proc(
	ctx: ^Checker_Context,
	actual: ^Type,
	expected: ^Type,
	range: Range,
) {
	checker_add_diagnostic(
		ctx,
		.Incompatible_Argument_Type,
		range,
		checker_type_mismatch_message(ctx, "incompatible WHERE operand", actual, expected),
	)
}

checker_check_table_component_expr :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	row_type: ^Type,
	row_structure: ^Structure,
	allow_local_value: bool,
) -> (Operand, bool) {
	if expr == nil {
		return checker_invalid_operand(), false
	}
	segments := make([dynamic]Checker_Table_Component_Segment, 0, 2, context.temp_allocator)
	if !checker_collect_table_component_segments(expr, &segments) || len(segments) == 0 {
		return checker_invalid_operand(), false
	}
	first := segments[0]
	if allow_local_value && !checker_table_component_is_table_line(first.name) {
		first_name := project_intern_lower_ascii(ctx.project, first.name)
		if first_name != "" {
			if _, _, ok := checker_lookup_declaration(ctx, .Value, first_name); ok {
				return checker_invalid_operand(), false
			}
		}
	}
	if row_structure == nil {
		if len(segments) == 1 && checker_table_component_is_table_line(first.name) {
			return checker_record_operand(ctx, &expr.expr_base, .Table_Line, row_type), true
		}
		if checker_type_is_unknown(row_type) {
			return checker_record_operand(ctx, &expr.expr_base, .Value, project_type_unknown(ctx.project)), true
		}
		unknown := first
		if checker_table_component_is_table_line(first.name) && len(segments) > 1 {
			unknown = segments[1]
		}
		checker_add_unknown_table_component_diagnostic(ctx, unknown.name, unknown.range)
		return checker_record_operand(ctx, &expr.expr_base, .Value, project_type_unknown(ctx.project)), true
	}
	current_type := row_type
	current_structure := row_structure
	final_field: ^Entity
	for i in 0 ..< len(segments) {
		segment := segments[i]
		if checker_table_component_is_table_line(segment.name) {
			if i == 0 {
				current_structure = checker_type_structure(current_type)
				continue
			}
		}
		if current_structure == nil {
			return checker_record_operand(ctx, &expr.expr_base, .Value, project_type_unknown(ctx.project)), true
		}
		name := project_intern_lower_ascii(ctx.project, segment.name)
		if name == "" {
			return checker_record_operand(ctx, &expr.expr_base, .Value, project_type_unknown(ctx.project)), true
		}
		field, ok := checker_lookup_structure_field(current_structure, name)
		if !ok {
			checker_add_diagnostic(ctx, .Unknown_Field, segment.range, checker_table_component_message(ctx, "unknown internal table field ", name))
			return checker_record_operand(ctx, &expr.expr_base, .Value, project_type_unknown(ctx.project)), true
		}
		checker_add_entity_use(ctx, segment.node, field)
		final_field = field
		current_type = field.type if field.type != nil else project_type_unknown(ctx.project)
		current_structure = checker_type_structure(current_type)
	}
	if final_field != nil {
		return checker_record_operand(ctx, &expr.expr_base, .Field, current_type, final_field), true
	}
	if len(segments) == 1 && checker_table_component_is_table_line(first.name) {
		return checker_record_operand(ctx, &expr.expr_base, .Table_Line, row_type), true
	}
	return checker_record_operand(ctx, &expr.expr_base, .Value, project_type_unknown(ctx.project)), true
}

checker_collect_table_component_segments :: proc(
	expr: ^ast.Expr,
	segments: ^[dynamic]Checker_Table_Component_Segment,
) -> bool {
	if expr == nil {
		return false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		return checker_append_table_component_segment(segments, n.name, n.range, &expr.expr_base)
	case ^ast.Type_Ref_Expr:
		if n.raw_operand {
			return false
		}
		base_name := n.base_name.text
		base_range := n.base_name.range
		if base_name == "" {
			base_name = n.name.text
			base_range = n.name.range
		}
		if !checker_append_table_component_segment(segments, base_name, base_range, &expr.expr_base) {
			return false
		}
		for segment in n.path {
			if segment.selector != .Dash {
				return false
			}
			if !checker_append_table_component_segment(segments, segment.name.text, segment.name.range, &expr.expr_base) {
				return false
			}
		}
		return true
	case ^ast.Sql_Column_Expr:
		if n.qualifier.text != "" {
			return false
		}
		return checker_append_table_component_segment(segments, n.name.text, n.name.range, &expr.expr_base)
	case ^ast.Selector_Expr:
		if n.op != .Dash {
			return false
		}
		if !checker_collect_table_component_segments(n.base, segments) {
			return false
		}
		return checker_append_table_component_leaf_segment(n.field, segments)
	}
	return false
}

checker_append_table_component_leaf_segment :: proc(
	expr: ^ast.Expr,
	segments: ^[dynamic]Checker_Table_Component_Segment,
) -> bool {
	if expr == nil {
		return false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		return checker_append_table_component_segment(segments, n.name, n.range, &expr.expr_base)
	case ^ast.Type_Ref_Expr:
		if n.raw_operand || len(n.path) > 0 {
			return false
		}
		name := n.base_name.text if n.base_name.text != "" else n.name.text
		range := n.base_name.range if n.base_name.text != "" else n.name.range
		return checker_append_table_component_segment(segments, name, range, &expr.expr_base)
	case ^ast.Sql_Column_Expr:
		if n.qualifier.text != "" {
			return false
		}
		return checker_append_table_component_segment(segments, n.name.text, n.name.range, &expr.expr_base)
	}
	return false
}

checker_append_table_component_segment :: proc(
	segments: ^[dynamic]Checker_Table_Component_Segment,
	name: string,
	range: Range,
	node: ^ast.Node,
) -> bool {
	if name == "" {
		return false
	}
	append(segments, Checker_Table_Component_Segment{name = name, range = range, node = node})
	return true
}

checker_table_component_is_table_line :: proc(name: string) -> bool {
	return strings.equal_fold(name, "table_line")
}

checker_add_unknown_table_component_diagnostic :: proc(
	ctx: ^Checker_Context,
	name_text: string,
	range: Range,
) {
	name := project_intern_lower_ascii(ctx.project, name_text)
	if name == "" {
		return
	}
	checker_add_diagnostic(
		ctx,
		.Unknown_Field,
		range,
		checker_table_component_message(ctx, "unknown internal table field ", name),
	)
}

checker_table_component_message :: proc(
	ctx: ^Checker_Context,
	prefix: string,
	name: string,
) -> string {
	builder := strings.builder_make(context.temp_allocator)
	strings.write_string(&builder, prefix)
	strings.write_string(&builder, name)
	return strings.to_string(builder)
}

checker_check_call_expr_arguments :: proc(
	ctx: ^Checker_Context,
	call: ^ast.Call_Expr,
	callee: Operand,
) {
	if call == nil || call.args == nil {
		return
	}
	args := make([dynamic]Checker_Call_Argument, 0, 4, context.temp_allocator)
	if arg_list, ok := call.args.derived_expr.(^ast.Call_Arg_List_Expr); ok {
		for arg in arg_list.args {
			checker_collect_call_expr_argument(ctx, &args, arg, .Exporting, false)
		}
	} else {
		checker_check_expr(ctx, call.args)
		return
	}
	if callee.entity == nil || callee.entity.kind == .Builtin {
		if checker_check_builtin_call_expr_arguments(ctx, callee.entity, args[:], call.range) {
			return
		}
		for arg in args {
			checker_check_call_argument_value(ctx, arg, nil, false)
		}
		return
	}
	checker_check_routine_call_arguments(ctx, callee.entity, args[:], call.range)
}

checker_check_builtin_call_expr_arguments :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	args: []Checker_Call_Argument,
	call_range: Range,
) -> bool {
	_ = call_range
	if entity == nil || entity.kind != .Builtin {
		return false
	}
	payload, ok := entity.payload.(^Entity_Builtin_Payload)
	if !ok || payload == nil {
		return false
	}
	id := Builtin_Proc_Id(payload.id)
	if id != .Condense && id != .Find {
		return false
	}
	checker_check_builtin_id_arguments(ctx, id, args)
	return true
}

checker_check_builtin_id_arguments :: proc(
	ctx: ^Checker_Context,
	id: Builtin_Proc_Id,
	args: []Checker_Call_Argument,
) {
	seen := make(map[string]bool, len(args), context.temp_allocator)
	positional_index := 0
	for arg in args {
		param: Builtin_Proc_Param
		param_ok := false
		if arg.name != "" {
			param, param_ok = checker_builtin_find_named_parameter(id, arg.name)
			if !param_ok {
				checker_check_call_argument_value(ctx, arg, nil, false)
				checker_add_diagnostic(
					ctx,
					.Unknown_Named_Parameter,
					arg.name_range,
					checker_unknown_named_parameter_message(ctx, nil, arg, .Unknown),
				)
				continue
			}
		} else {
			param, param_ok = checker_builtin_positional_parameter(id, positional_index)
			positional_index += 1
			if !param_ok {
				checker_check_call_argument_value(ctx, arg, nil, false)
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					arg.value_range,
					"builtin function has too many arguments",
				)
				continue
			}
		}

		if seen[param.name] {
			checker_check_builtin_parameter_argument(ctx, arg, param)
			duplicate_range := arg.name_range
			if duplicate_range.end <= duplicate_range.start {
				duplicate_range = arg.value_range
			}
			checker_add_diagnostic(ctx, .Duplicate_Named_Parameter, duplicate_range, "duplicate named parameter")
			continue
		}
		seen[param.name] = true
		checker_check_builtin_parameter_argument(ctx, arg, param)
	}
}

checker_builtin_find_named_parameter :: proc(
	id: Builtin_Proc_Id,
	name: string,
) -> (Builtin_Proc_Param, bool) {
	#partial switch id {
	case .Condense:
		if strings.equal_fold(name, "val") {
			return Builtin_Proc_Param{name = "val", type_name = "string"}, true
		}
		if strings.equal_fold(name, "del") {
			return Builtin_Proc_Param{name = "del", type_name = "string"}, true
		}
		if strings.equal_fold(name, "from") {
			return Builtin_Proc_Param{name = "from", type_name = "string"}, true
		}
		if strings.equal_fold(name, "to") {
			return Builtin_Proc_Param{name = "to", type_name = "string"}, true
		}
	case .Find:
		if strings.equal_fold(name, "val") {
			return Builtin_Proc_Param{name = "val", type_name = "string"}, true
		}
		if strings.equal_fold(name, "sub") {
			return Builtin_Proc_Param{name = "sub", type_name = "string"}, true
		}
		if strings.equal_fold(name, "regex") {
			return Builtin_Proc_Param{name = "regex", type_name = "string"}, true
		}
		if strings.equal_fold(name, "occ") {
			return Builtin_Proc_Param{name = "occ", type_name = "i"}, true
		}
		if strings.equal_fold(name, "case") {
			return Builtin_Proc_Param{name = "case", type_name = "abap_bool"}, true
		}
	case:
	}
	return {}, false
}

checker_builtin_positional_parameter :: proc(
	id: Builtin_Proc_Id,
	index: int,
) -> (Builtin_Proc_Param, bool) {
	#partial switch id {
	case .Condense:
		switch index {
		case 0:
			return Builtin_Proc_Param{name = "val", type_name = "string"}, true
		case 1:
			return Builtin_Proc_Param{name = "del", type_name = "string"}, true
		case 2:
			return Builtin_Proc_Param{name = "from", type_name = "string"}, true
		case 3:
			return Builtin_Proc_Param{name = "to", type_name = "string"}, true
		}
	case .Find:
		switch index {
		case 0:
			return Builtin_Proc_Param{name = "val", type_name = "string"}, true
		case 1:
			return Builtin_Proc_Param{name = "sub", type_name = "string"}, true
		case 2:
			return Builtin_Proc_Param{name = "regex", type_name = "string"}, true
		case 3:
			return Builtin_Proc_Param{name = "occ", type_name = "i"}, true
		case 4:
			return Builtin_Proc_Param{name = "case", type_name = "abap_bool"}, true
		}
	case:
	}
	return {}, false
}

checker_check_builtin_parameter_argument :: proc(
	ctx: ^Checker_Context,
	arg: Checker_Call_Argument,
	param: Builtin_Proc_Param,
) {
	formal_type := checker_builtin_type_from_name(ctx.checker, param.type_name)
	actual := checker_check_call_argument_value(ctx, arg, formal_type, false)
	if checker_literal_argument_compatible(ctx, arg.value, formal_type) {
		return
	}
	checker_check_builtin_argument_compatibility(ctx, actual.type, formal_type, arg.value_range)
}

checker_check_builtin_argument_compatibility :: proc(
	ctx: ^Checker_Context,
	actual: ^Type,
	expected: ^Type,
	range: Range,
) {
	if ok, known := checker_type_argument_compatible(ctx, actual, expected); known {
		if !ok {
			checker_add_diagnostic(
				ctx,
				.Incompatible_Argument_Type,
				range,
				checker_type_mismatch_message(ctx, "incompatible argument", actual, expected),
			)
		}
		return
	}
	if checker_builtin_scalar_argument_category_mismatch(ctx, actual, expected) {
		checker_add_diagnostic(
			ctx,
			.Incompatible_Argument_Type,
			range,
			checker_type_mismatch_message(ctx, "incompatible argument", actual, expected),
		)
	}
}

checker_builtin_scalar_argument_category_mismatch :: proc(
	ctx: ^Checker_Context,
	actual: ^Type,
	expected: ^Type,
) -> bool {
	if checker_type_is_unknown(actual) || checker_type_is_unknown(expected) {
		return false
	}
	expected_name, expected_ok := checker_type_builtin_name(ctx, expected)
	if !expected_ok || checker_generic_builtin_type_name(expected_name) {
		return false
	}
	return checker_type_structure(actual) != nil ||
	       checker_type_is_table_like(ctx, actual) ||
	       checker_type_is_ref(actual)
}

checker_collect_call_expr_argument :: proc(
	ctx: ^Checker_Context,
	args: ^[dynamic]Checker_Call_Argument,
	expr: ^ast.Expr,
	section: ast.Call_Arg_Section_Kind,
	has_section: bool,
) {
	if expr == nil {
		return
	}
	if section_expr, ok := expr.derived_expr.(^ast.Call_Arg_Section_Expr); ok {
		for arg in section_expr.args {
			checker_collect_call_expr_argument(ctx, args, arg, section_expr.kind, true)
		}
		return
	}
	arg := Checker_Call_Argument {
		section     = section,
		has_section = has_section,
		value_range = expr.range,
	}
	if named, named_ok := expr.derived_expr.(^ast.Call_Named_Arg_Expr); named_ok {
		arg.name_text = named.name.text
		arg.name = project_intern_lower_ascii(ctx.project, named.name.text)
		arg.name_range = named.name.range
		arg.value = named.value
		arg.value_range = checker_expr_range(named.value)
	} else if positional, positional_ok := expr.derived_expr.(^ast.Call_Positional_Arg_Expr); positional_ok {
		arg.value = positional.value
		arg.value_range = checker_expr_range(positional.value)
	} else {
		arg.value = expr
	}
	append(args, arg)
}

checker_check_call_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Call_Stmt) {
	#partial switch stmt.kind {
	case .Direct:
		checker_check_expr(ctx, stmt.call)
	case .Function, .Customer_Function:
		routine := checker_lookup_call_function_entity(ctx, stmt.target)
		checker_check_expr_with_unresolved_value_diagnostics(ctx, stmt.function_destination)
		checker_check_expr_with_unresolved_value_diagnostics(ctx, stmt.function_task)
		checker_check_expr(ctx, stmt.function_end_task_handler, .Routine)
		checker_check_expr_with_unresolved_value_diagnostics(ctx, stmt.function_parameter_table)
		checker_check_expr_with_unresolved_value_diagnostics(ctx, stmt.function_exception_table)
		checker_check_call_stmt_args(ctx, stmt.named_args[:], routine, stmt.range, stmt.function_parameter_table != nil)
	case .Method:
		target := checker_check_expr(ctx, stmt.target, .Routine)
		checker_check_call_stmt_args(ctx, stmt.named_args[:], target.entity, stmt.range)
	case .Transformation:
		for arg in stmt.transformation_args {
			checker_check_expr(ctx, arg.value)
		}
	case .Transaction:
		checker_check_expr(ctx, stmt.target)
		checker_check_expr_list(ctx, stmt.transaction_operands[:])
	case:
		checker_check_expr(ctx, stmt.target)
	}
}

checker_check_call_stmt_args :: proc(
	ctx: ^Checker_Context,
	named_args: []ast.Call_Stmt_Named_Arg,
	routine: ^Entity,
	call_range: Range,
	has_parameter_table := false,
) {
	args := make([dynamic]Checker_Call_Argument, 0, len(named_args), context.temp_allocator)
	for named in named_args {
		arg := Checker_Call_Argument {
			name          = project_intern_lower_ascii(ctx.project, named.name.text),
			name_text     = named.name.text,
			name_range    = named.name.range,
			section       = named.section,
			has_section   = named.has_section,
			value         = named.value,
			value_range   = named.value_range,
			message       = named.message,
			message_range = named.message_range,
			raw_decls     = named.raw_decls[:],
			raw_refs      = named.raw_refs[:],
		}
		append(&args, arg)
	}
	if routine != nil {
		checker_check_routine_call_arguments(ctx, routine, args[:], call_range, has_parameter_table)
		return
	}
	for arg in args {
		checker_check_call_argument_value(ctx, arg, nil, false, diagnose_unresolved_value_refs = true)
		if arg.message != nil {
			checker_check_call_function_exception_message(ctx, arg.message, arg.message_range)
		}
	}
}

checker_lookup_call_function_entity :: proc(ctx: ^Checker_Context, target: ^ast.Expr) -> ^Entity {
	name := checker_call_target_name(ctx, target)
	if name == "" {
		checker_check_expr(ctx, target)
		return nil
	}
	interned := project_intern_lower_ascii(ctx.project, name)
	_, entity, ok := checker_lookup_reference(ctx, .Routine, interned, .Function_Module)
	if !ok {
		checker_add_unresolved_candidate(
			ctx,
			interned,
			.Routine,
			.Function_Module,
			.Call_Function,
			.Unresolved_Routine,
			checker_expr_range(target),
			&target.expr_base if target != nil else nil,
		)
		return nil
	}
	if entity.kind != .Module {
		return nil
	}
	checker_add_entity_use(ctx, &target.expr_base if target != nil else nil, entity)
	checker_check_entity_for_operand(ctx, entity)
	return entity
}

checker_call_target_name :: proc(ctx: ^Checker_Context, target: ^ast.Expr) -> string {
	_ = ctx
	if target == nil {
		return ""
	}
	if lit, ok := target.derived_expr.(^ast.Literal_Expr); ok {
		return checker_strip_quotes(lit.value)
	}
	if ident, ok := target.derived_expr.(^ast.Ident_Expr); ok {
		return ident.name
	}
	if ref, ok := target.derived_expr.(^ast.Type_Ref_Expr); ok {
		return ref.name.text
	}
	return ""
}

checker_strip_quotes :: proc(text: string) -> string {
	if len(text) >= 2 &&
	   ((text[0] == '\'' && text[len(text) - 1] == '\'') ||
	    (text[0] == '`' && text[len(text) - 1] == '`')) {
		return text[1:len(text) - 1]
	}
	return text
}

checker_check_perform_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Perform_Stmt) {
	form_operand := checker_check_expr(ctx, stmt.form, .Routine)
	checker_check_expr(ctx, stmt.program)
	if stmt.has_program_clause && stmt.program_kind == .Static {
		checker_check_report_dependency_target(ctx, stmt.program, .Perform_In_Program, stmt.if_found)
	}
	args := make([dynamic]Checker_Call_Argument, 0, len(stmt.tables) + len(stmt.using_args) + len(stmt.changing), context.temp_allocator)
	for expr in stmt.tables {
		append(&args, checker_call_argument_from_expr(ctx, expr, .Tables, true))
	}
	for expr in stmt.using_args {
		append(&args, checker_call_argument_from_expr(ctx, expr, .Exporting, true))
	}
	for expr in stmt.changing {
		append(&args, checker_call_argument_from_expr(ctx, expr, .Changing, true))
	}
	if form_operand.entity != nil && form_operand.entity.kind == .Form {
		checker_check_routine_call_arguments(ctx, form_operand.entity, args[:], stmt.range)
		return
	}
	for arg in args {
		checker_check_call_argument_value(ctx, arg, nil, checker_call_arg_requires_writable(arg.section))
	}
}

checker_call_argument_from_expr :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	section: ast.Call_Arg_Section_Kind,
	has_section: bool,
) -> Checker_Call_Argument {
	_ = ctx
	return Checker_Call_Argument {
		section     = section,
		has_section = has_section,
		value       = expr,
		value_range = checker_expr_range(expr),
	}
}

checker_check_routine_call_arguments :: proc(
	ctx: ^Checker_Context,
	routine: ^Entity,
	args: []Checker_Call_Argument,
	call_range: Range,
	has_parameter_table := false,
) {
	if routine == nil || routine.kind == .Builtin {
		for arg in args {
			checker_check_call_argument_value(
				ctx,
				arg,
				nil,
				checker_call_arg_requires_writable(arg.section),
				diagnose_unresolved_value_refs = true,
			)
		}
		return
	}
	checker_check_entity_for_operand(ctx, routine)
	payload, ok := routine.payload.(^Entity_Routine_Payload)
	if !ok || payload == nil {
		for arg in args {
			checker_check_call_argument_value(
				ctx,
				arg,
				nil,
				checker_call_arg_requires_writable(arg.section),
				diagnose_unresolved_value_refs = true,
			)
		}
		return
	}

	supplied := make([dynamic]^Entity, 0, len(args), context.temp_allocator)
	positional := make([dynamic]int, 0, len(args), context.temp_allocator)
	seen := make(map[Checker_Call_Parameter_Key]bool, len(args), context.temp_allocator)
	required_mapping_ok := true

	for arg, index in args {
		if arg.section == .Exceptions {
			checker_check_call_argument_value(ctx, arg, nil, false, diagnose_unresolved_value_refs = true)
			if arg.message != nil {
				checker_check_call_function_exception_message(ctx, arg.message, arg.message_range)
			}
			continue
		}
		if arg.name == "" {
			append(&positional, index)
			continue
		}
		section := checker_call_effective_actual_section(routine.kind, arg.section, arg.has_section)
		formal, formal_ok := checker_call_find_named_parameter(ctx, routine, payload.parameters[:], arg.name, section)
		if !formal_ok {
			checker_check_call_argument_value(
				ctx,
				arg,
				nil,
				checker_call_arg_requires_writable(section),
				diagnose_unresolved_value_refs = true,
			)
			checker_add_diagnostic(
				ctx,
				.Unknown_Named_Parameter,
				arg.name_range,
				checker_unknown_named_parameter_message(ctx, routine, arg, section),
			)
			required_mapping_ok = false
			continue
		}
		param_section := checker_parameter_section(formal)
		key := Checker_Call_Parameter_Key{section = param_section, name = formal.name}
		if key in seen {
			checker_check_call_argument_value(ctx, arg, formal.type, checker_call_arg_requires_writable(section))
			checker_add_diagnostic(ctx, .Duplicate_Named_Parameter, arg.name_range, "duplicate named parameter")
			required_mapping_ok = false
			continue
		}
		seen[key] = true
		checker_check_call_argument_with_parameter(ctx, arg, section, formal)
		checker_note_supplied_parameter(&supplied, formal)
	}

	if len(positional) > 0 {
		eligible := make([dynamic]^Entity, 0, len(payload.parameters), context.temp_allocator)
		checker_call_eligible_positional_parameters(ctx, &eligible, routine, payload.parameters[:])
		if routine.kind == .Method {
			first := args[positional[0]]
			if formal := checker_first_unsupplied_positional_parameter(eligible[:], supplied[:]); formal != nil {
				checker_check_call_argument_with_parameter(
					ctx,
					first,
					checker_call_default_actual_section(routine.kind),
					formal,
					diagnose_unresolved_value_refs = true,
				)
				checker_note_supplied_parameter(&supplied, formal)
			} else {
				checker_check_call_argument_value(ctx, first, nil, false, diagnose_unresolved_value_refs = true)
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					first.value_range,
					"method call does not accept an unnamed argument",
				)
				required_mapping_ok = false
			}
			for arg_index in positional[1:] {
				arg := args[arg_index]
				checker_check_call_argument_value(ctx, arg, nil, false, diagnose_unresolved_value_refs = true)
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					arg.value_range,
					"method call allows only one unnamed argument",
				)
				required_mapping_ok = false
			}
		} else if len(positional) == len(eligible) {
			for arg_index, i in positional {
				arg := args[arg_index]
				formal := eligible[i]
				checker_check_call_argument_with_parameter(
					ctx,
					arg,
					checker_call_default_actual_section(routine.kind),
					formal,
				)
				checker_note_supplied_parameter(&supplied, formal)
			}
		} else {
			for arg_index in positional {
				arg := args[arg_index]
				checker_check_call_argument_value(ctx, arg, nil, false)
			}
			required_mapping_ok = false
		}
	}

	if required_mapping_ok && !has_parameter_table {
		checker_check_missing_required_parameters(ctx, routine, payload.parameters[:], supplied[:], call_range)
	}
}

checker_unknown_named_parameter_message :: proc(
	ctx: ^Checker_Context,
	routine: ^Entity,
	arg: Checker_Call_Argument,
	section: ast.Call_Arg_Section_Kind,
) -> string {
	builder := strings.builder_make(context.temp_allocator)
	strings.write_string(&builder, "unknown named parameter")
	name := arg.name_text
	if name == "" && arg.name != "" {
		name = arg.name
	}
	if name != "" {
		strings.write_string(&builder, " '")
		strings.write_string(&builder, name)
		strings.write_string(&builder, "'")
	}
	section_text := checker_call_arg_section_text(section)
	if section_text != "" {
		strings.write_string(&builder, " in ")
		strings.write_string(&builder, section_text)
		strings.write_string(&builder, " section")
	}
	if routine != nil && routine.name != "" {
		routine_kind := checker_routine_kind_text(routine.kind)
		if routine_kind != "" {
			strings.write_string(&builder, " for ")
			strings.write_string(&builder, routine_kind)
			strings.write_string(&builder, " '")
			strings.write_string(&builder, routine.name)
			strings.write_string(&builder, "'")
		}
	}
	return strings.to_string(builder)
}

checker_call_arg_section_text :: proc(section: ast.Call_Arg_Section_Kind) -> string {
	switch section {
	case .Exporting:
		return "EXPORTING"
	case .Importing:
		return "IMPORTING"
	case .Changing:
		return "CHANGING"
	case .Tables:
		return "TABLES"
	case .Receiving:
		return "RECEIVING"
	case .Exceptions:
		return "EXCEPTIONS"
	case .Unknown:
		return ""
	}
	return ""
}

checker_routine_kind_text :: proc(kind: Entity_Kind) -> string {
	#partial switch kind {
	case .Method:
		return "method"
	case .Module:
		return "function module"
	case .Form:
		return "form"
	case:
	}
	return ""
}

checker_check_call_argument_with_parameter :: proc(
	ctx: ^Checker_Context,
	arg: Checker_Call_Argument,
	actual_section: ast.Call_Arg_Section_Kind,
	formal: ^Entity,
	diagnose_unresolved_value_refs := true,
) {
	checker_check_entity_for_operand(ctx, formal)
	if formal != nil && arg.name != "" && arg.name_range.end > arg.name_range.start {
		checker_add_entity_use_at_range(ctx, nil, formal, arg.name_range)
	}
	formal_type := formal.type if formal != nil && formal.type != nil else project_type_unknown(ctx.project)
	receives := checker_call_arg_receives_from_formal(actual_section)
	writable := checker_call_arg_requires_writable(actual_section)
	actual := checker_check_call_argument_value(
		ctx,
		arg,
		formal_type,
		writable,
		diagnose_unresolved_value_refs = diagnose_unresolved_value_refs,
	)
	if writable && !checker_operand_is_writable(actual) {
		checker_add_diagnostic(ctx, .Incompatible_Argument_Type, arg.value_range, "argument is not writable")
		return
	}
	if receives {
		checker_check_argument_compatibility(ctx, formal_type, actual.type, arg.value_range)
	} else {
		if !checker_literal_argument_compatible(ctx, arg.value, formal_type) {
			checker_check_argument_compatibility(ctx, actual.type, formal_type, arg.value_range)
		}
	}
}

checker_literal_argument_compatible :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, dst: ^Type) -> bool {
	return checker_character_literal_argument_compatible(ctx, expr, dst) ||
	       checker_numeric_literal_argument_compatible(ctx, expr, dst)
}

checker_character_literal_argument_compatible :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, dst: ^Type) -> bool {
	value, value_ok := checker_expr_character_literal_text(expr)
	if !value_ok {
		return false
	}
	dst_name, dst_ok := checker_type_builtin_name(ctx, dst)
	if !dst_ok {
		return false
	}
	return checker_character_literal_matches_builtin_formal(value, dst_name)
}

checker_expr_character_literal_text :: proc(expr: ^ast.Expr) -> (string, bool) {
	if expr == nil {
		return "", false
	}
	lit, ok := expr.derived_expr.(^ast.Literal_Expr)
	if !ok || len(lit.value) < 2 {
		return "", false
	}
	quote := lit.value[0]
	if (quote == '\'' || quote == '`') && lit.value[len(lit.value) - 1] == quote {
		return lit.value[1:len(lit.value) - 1], true
	}
	return "", false
}

checker_character_literal_matches_builtin_formal :: proc(value: string, dst_name: string) -> bool {
	switch dst_name {
	case "c", "string", "clike", "csequence":
		return true
	case "d":
		return len(value) == 8
	case "t":
		return len(value) == 6
	case "n":
		return checker_text_literal_digits(value)
	case "i", "int1", "int2", "int4", "int8":
		return checker_text_literal_integer(value)
	case "p", "f", "decfloat", "decfloat16", "decfloat34", "numeric":
		return checker_text_literal_decimal(value)
	case "x", "xstring", "xsequence":
		return checker_text_literal_hex(value)
	}
	return false
}

checker_numeric_literal_argument_compatible :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, dst: ^Type) -> bool {
	value, value_ok := checker_expr_numeric_literal_text(expr)
	if !value_ok {
		return false
	}
	dst_name, dst_ok := checker_type_builtin_name(ctx, dst)
	if !dst_ok {
		return false
	}
	return checker_numeric_literal_matches_builtin_formal(value, dst_name)
}

checker_expr_numeric_literal_text :: proc(expr: ^ast.Expr) -> (string, bool) {
	if expr == nil {
		return "", false
	}
	lit, ok := expr.derived_expr.(^ast.Literal_Expr)
	if !ok || !checker_literal_is_integer(lit.value) {
		return "", false
	}
	return lit.value, true
}

checker_numeric_literal_matches_builtin_formal :: proc(value: string, dst_name: string) -> bool {
	if !checker_literal_is_integer(value) {
		return false
	}
	switch dst_name {
	case "n":
		return true
	}
	return false
}

checker_text_literal_digits :: proc(value: string) -> bool {
	if value == "" {
		return false
	}
	for i in 0 ..< len(value) {
		if value[i] < '0' || value[i] > '9' {
			return false
		}
	}
	return true
}

checker_text_literal_integer :: proc(value: string) -> bool {
	trimmed := strings.trim_space(value)
	if trimmed == "" {
		return false
	}
	digits := trimmed
	if trimmed[0] == '+' || trimmed[0] == '-' {
		digits = trimmed[1:]
	}
	return checker_text_literal_digits(digits)
}

checker_text_literal_decimal :: proc(value: string) -> bool {
	trimmed := strings.trim_space(value)
	if trimmed == "" {
		return false
	}
	digits := trimmed
	if trimmed[0] == '+' || trimmed[0] == '-' {
		digits = trimmed[1:]
	}
	seen_digit := false
	seen_decimal := false
	for i in 0 ..< len(digits) {
		ch := digits[i]
		if '0' <= ch && ch <= '9' {
			seen_digit = true
		} else if ch == '.' && !seen_decimal {
			seen_decimal = true
		} else {
			return false
		}
	}
	return seen_digit
}

checker_text_literal_hex :: proc(value: string) -> bool {
	if value == "" {
		return false
	}
	for i in 0 ..< len(value) {
		ch := value[i]
		if !('0' <= ch && ch <= '9' || 'A' <= ch && ch <= 'F' || 'a' <= ch && ch <= 'f') {
			return false
		}
	}
	return true
}

checker_check_call_argument_value :: proc(
	ctx: ^Checker_Context,
	arg: Checker_Call_Argument,
	type_hint: ^Type,
	lhs: bool,
	diagnose_unresolved_value_refs := false,
) -> Operand {
	if arg.value != nil {
		local := ctx^
		local.type_hint = type_hint
		local.type_hint_expr = arg.value
		local.diagnose_unresolved_value_refs = diagnose_unresolved_value_refs
		return checker_check_expr(&local, arg.value, .Value, lhs)
	}
	return checker_check_raw_operand_facts(ctx, arg.raw_decls, arg.raw_refs, type_hint, lhs)
}

checker_check_call_function_exception_message :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	range: Range,
) {
	operand := checker_check_expr_with_unresolved_value_diagnostics(ctx, expr)
	if ok, known := checker_type_message_field_compatible(ctx, operand.type); known && !ok {
		checker_add_diagnostic(ctx, .Incompatible_Argument_Type, range, "message field must be type c, n, d, or t")
	}
}

checker_check_missing_required_parameters :: proc(
	ctx: ^Checker_Context,
	routine: ^Entity,
	params: []^Entity,
	supplied: []^Entity,
	range: Range,
) {
	for param in params {
		if checker_parameter_supplied(supplied, param) || !checker_parameter_required_for_call(routine, param) {
			continue
		}
		checker_add_diagnostic(
			ctx,
			.Missing_Required_Parameter,
			range,
			checker_missing_required_parameter_message(ctx, param),
			param,
			param.decl_info,
		)
	}
}

checker_missing_required_parameter_message :: proc(ctx: ^Checker_Context, param: ^Entity) -> string {
	if param == nil || param.name == "" {
		return "missing required parameter"
	}
	builder := strings.builder_make(context.temp_allocator)
	strings.write_string(&builder, "missing required parameter '")
	strings.write_string(&builder, param.name)
	strings.write_string(&builder, "'")
	return strings.to_string(builder)
}

checker_call_find_named_parameter :: proc(
	ctx: ^Checker_Context,
	routine: ^Entity,
	params: []^Entity,
	name: string,
	section: ast.Call_Arg_Section_Kind,
) -> (^Entity, bool) {
	_ = ctx
	for param in params {
		if param.name != name {
			continue
		}
		if checker_call_parameter_matches_section(routine.kind, checker_parameter_section(param), section) {
			return param, true
		}
	}
	return nil, false
}

checker_call_eligible_positional_parameters :: proc(
	ctx: ^Checker_Context,
	out: ^[dynamic]^Entity,
	routine: ^Entity,
	params: []^Entity,
) {
	_ = ctx
	section := checker_call_default_actual_section(routine.kind)
	for param in params {
		if checker_call_parameter_matches_section(routine.kind, checker_parameter_section(param), section) {
			append(out, param)
		}
	}
}

checker_first_unsupplied_positional_parameter :: proc(
	eligible: []^Entity,
	supplied: []^Entity,
) -> ^Entity {
	for param in eligible {
		if !checker_parameter_supplied(supplied, param) {
			return param
		}
	}
	return nil
}

checker_call_parameter_matches_section :: proc(
	routine_kind: Entity_Kind,
	formal: Entity_Parameter_Section,
	actual: ast.Call_Arg_Section_Kind,
) -> bool {
	#partial switch routine_kind {
	case .Method:
		#partial switch actual {
		case .Exporting, .Unknown:
			return formal == .Method_Importing
		case .Importing:
			return formal == .Method_Exporting
		case .Changing:
			return formal == .Method_Changing
		case .Receiving:
			return formal == .Method_Returning || formal == .Method_Receiving
		case:
			return false
		}
	case .Module:
		#partial switch actual {
		case .Exporting, .Unknown:
			return formal == .Function_Importing
		case .Importing:
			return formal == .Function_Exporting
		case .Changing:
			return formal == .Function_Changing
		case .Tables:
			return formal == .Function_Tables
		case:
			return false
		}
	case .Form:
		#partial switch actual {
		case .Tables:
			return formal == .Form_Tables
		case .Exporting, .Unknown:
			return formal == .Form_Using
		case .Changing:
			return formal == .Form_Changing
		case:
			return false
		}
	case:
	}
	return false
}

checker_call_effective_actual_section :: proc(
	routine_kind: Entity_Kind,
	section: ast.Call_Arg_Section_Kind,
	has_section: bool,
) -> ast.Call_Arg_Section_Kind {
	if has_section && section != .Unknown {
		return section
	}
	return checker_call_default_actual_section(routine_kind)
}

checker_call_default_actual_section :: proc(routine_kind: Entity_Kind) -> ast.Call_Arg_Section_Kind {
	#partial switch routine_kind {
	case .Form:
		return .Exporting
	case .Method, .Module:
		return .Exporting
	case:
		return .Exporting
	}
	return .Exporting
}

checker_call_arg_requires_writable :: proc(section: ast.Call_Arg_Section_Kind) -> bool {
	#partial switch section {
	case .Importing, .Changing, .Receiving, .Tables:
		return true
	case:
		return false
	}
}

checker_call_arg_receives_from_formal :: proc(section: ast.Call_Arg_Section_Kind) -> bool {
	#partial switch section {
	case .Importing, .Receiving:
		return true
	case:
		return false
	}
}

checker_operand_is_writable :: proc(operand: Operand) -> bool {
	#partial switch operand.mode {
	case .Variable, .Field, .Table_Line:
		return true
	case:
		return false
	}
}

checker_parameter_section :: proc(param: ^Entity) -> Entity_Parameter_Section {
	assert(param != nil)
	payload, ok := param.payload.(^Entity_Variable_Payload)
	assert(ok && payload != nil)
	return payload.section
}

checker_parameter_required_for_call :: proc(routine: ^Entity, param: ^Entity) -> bool {
	if param == nil || .Optional in param.flags || .Has_Default_Value in param.flags {
		return false
	}
	section := checker_parameter_section(param)
	#partial switch routine.kind {
	case .Method:
		return section == .Method_Importing || section == .Method_Changing
	case .Module:
		return section == .Function_Importing || section == .Function_Changing || section == .Function_Tables
	case .Form:
		return section == .Form_Tables || section == .Form_Using || section == .Form_Changing
	case:
	}
	return false
}

checker_note_supplied_parameter :: proc(out: ^[dynamic]^Entity, param: ^Entity) {
	for existing in out^ {
		if existing == param {
			return
		}
	}
	append(out, param)
}

checker_parameter_supplied :: proc(supplied: []^Entity, param: ^Entity) -> bool {
	for item in supplied {
		if item == param {
			return true
		}
	}
	return false
}

checker_check_message_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Message_Stmt) {
	if stmt.head != nil {
		checker_check_expr(ctx, stmt.head.code)
		checker_check_expr(ctx, stmt.head.id)
		checker_check_expr(ctx, stmt.head.msg_type)
		checker_check_expr(ctx, stmt.head.number)
	}
	checker_check_expr_list(ctx, stmt.with_args[:])
	string_type := checker_builtin_type_from_name(ctx.checker, "string")
	if stmt.into != nil {
		local := ctx^
		local.type_hint = string_type
		local.type_hint_expr = stmt.into
		checker_check_expr(&local, stmt.into, .Value, true)
	}
	checker_check_expr(ctx, stmt.display_like)
	checker_check_expr(ctx, stmt.raising)
}

checker_check_submit_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Submit_Stmt) {
	target := checker_check_expr(ctx, stmt.target)
	if stmt.target_kind == .Static {
		checker_check_report_dependency_target(ctx, stmt.target, .Submit, false)
	} else {
		checker_check_unresolved_variable_operand(ctx, stmt.target, target)
	}
	for option in stmt.options {
		value := checker_check_expr(ctx, option.value)
		checker_check_unresolved_variable_operand(ctx, option.value, value)
		high_value := checker_check_expr(ctx, option.high_value)
		checker_check_unresolved_variable_operand(ctx, option.high_value, high_value)
		sign_value := checker_check_expr(ctx, option.sign_value)
		checker_check_unresolved_variable_operand(ctx, option.sign_value, sign_value)
	}
}

checker_check_report_dependency_target :: proc(
	ctx: ^Checker_Context,
	target: ^ast.Expr,
	hint: External_Candidate_Hint,
	if_found := false,
) {
	name := checker_call_target_name(ctx, target)
	if name == "" {
		return
	}
	interned := project_intern_lower_ascii(ctx.project, name)
	if interned == "" {
		return
	}
	_, entity, ok := checker_lookup_reference(ctx, .Value, interned, .Report)
	if ok && entity.kind == .Report {
		checker_add_entity_use(ctx, &target.expr_base if target != nil else nil, entity)
		return
	}
	checker_add_unresolved_candidate(
		ctx,
		interned,
		.Value,
		.Report,
		hint,
		.Unresolved_Reference,
		checker_expr_range(target),
		&target.expr_base if target != nil else nil,
		if_found,
	)
}

checker_check_runtime_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Runtime_Stmt) {
	checker_check_expr(ctx, stmt.id)
	checker_check_expr(ctx, stmt.field, .Value, stmt.kind == .Get)
	if stmt.kind == .Get && stmt.subject == .Time_Stamp_Field {
		timestamp_type := checker_builtin_type_from_name(ctx.checker, "timestamp")
		checker_check_type_hinted_target(ctx, stmt.target, timestamp_type)
	} else {
		checker_check_expr(ctx, stmt.target, .Value, true)
	}
	checker_check_expr(ctx, stmt.value)
	checker_check_expr(ctx, stmt.line, .Value, true)
	checker_check_expr(ctx, stmt.offset, .Value, true)
	checker_check_expr_list(ctx, stmt.excluding[:])
	checker_check_expr_list(ctx, stmt.operands[:])
}

checker_check_data_cluster_parameters :: proc(
	ctx: ^Checker_Context,
	parameters: []ast.Data_Cluster_Parameter_Clause,
	lhs := false,
) {
	for param in parameters {
		value := checker_check_expr(ctx, param.value, .Value, lhs)
		checker_check_unresolved_variable_operand(ctx, param.value, value)
	}
}

checker_check_data_cluster_medium :: proc(ctx: ^Checker_Context, medium: ast.Data_Cluster_Medium_Clause) {
	checker_check_expr(ctx, medium.object)
	checker_check_expr(ctx, medium.work_area)
	checker_check_expr(ctx, medium.client)
	id := checker_check_expr(ctx, medium.id)
	if medium.kind == .Memory_ID {
		checker_check_unresolved_variable_operand(ctx, medium.id, id)
	}
}

checker_check_assign_field_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Assign_Field_Stmt) {
	source := checker_check_expr(ctx, stmt.source)
	checker_check_expr(ctx, stmt.component)
	checker_check_expr(ctx, stmt.structure)
	local := ctx^
	local.type_hint = source.type
	local.type_hint_expr = stmt.target
	target := checker_check_expr(&local, stmt.target, .Value, true)
	checker_check_assignment_compatibility(ctx, source.type, target.type, checker_expr_range(stmt.target))
	checker_check_dynamic_or_static_type_expr(ctx, stmt.casting_type)
	checker_check_expr(ctx, stmt.casting_decimals)
}

checker_check_create_object_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Create_Object_Stmt) {
	target := checker_check_expr(ctx, stmt.target, .Value, true)
	constructor_type := checker_type_ref_target(ctx, target.type)
	if stmt.type_dynamic {
		checker_check_dynamic_type_name_expr(ctx, stmt.type_dynamic_expr, stmt.type_ref)
		checker_check_create_type_clause_non_ref_operands(ctx, stmt.type_clause)
	} else {
		if type_operand := checker_check_expr(ctx, stmt.type_ref, .Type); type_operand.type != nil {
			constructor_type = type_operand.type
		}
	}
	if stmt.type_clause != nil && !stmt.type_dynamic {
		if typ := checker_check_decl_type_clause(ctx, nil, stmt.type_clause); typ != nil {
			constructor_type = typ
		}
	}
	constructor := checker_constructor_method_for_type(ctx, constructor_type)
	checker_check_create_object_arguments(ctx, stmt.operands[:], constructor, stmt.range)
}

checker_check_create_object_arguments :: proc(
	ctx: ^Checker_Context,
	operands: []^ast.Expr,
	constructor: ^Entity,
	range: Range,
) {
	args := make([dynamic]Checker_Call_Argument, 0, len(operands), context.temp_allocator)
	for operand in operands {
		checker_collect_constructor_call_argument(ctx, &args, operand, .Exporting, false)
	}
	if constructor != nil {
		checker_check_routine_call_arguments(ctx, constructor, args[:], range)
		return
	}
	for arg in args {
		checker_check_call_argument_value(ctx, arg, nil, checker_call_arg_requires_writable(arg.section))
	}
}

checker_check_create_data_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Create_Data_Stmt) {
	checker_check_expr(ctx, stmt.target, .Value, true)
	if stmt.type_dynamic {
		checker_check_dynamic_type_name_expr(ctx, stmt.type_dynamic_expr, stmt.type_ref)
		checker_check_create_type_clause_non_ref_operands(ctx, stmt.type_clause)
	} else {
		checker_check_expr(ctx, stmt.type_ref, .Type)
	}
	if stmt.type_clause != nil && !stmt.type_dynamic {
		checker_check_decl_type_clause(ctx, nil, stmt.type_clause)
	}
	checker_check_expr(ctx, stmt.type_handle)
	checker_check_expr_list(ctx, stmt.operands[:])
}

checker_check_create_type_clause_non_ref_operands :: proc(
	ctx: ^Checker_Context,
	clause: ^ast.Data_Type_Clause,
) {
	if clause == nil {
		return
	}
	checker_check_expr(ctx, clause.initial_size, .Value)
}

checker_check_dynamic_or_static_type_expr :: proc(ctx: ^Checker_Context, expr: ^ast.Expr) {
	if expr == nil {
		return
	}
	if ref, ok := expr.derived_expr.(^ast.Type_Ref_Expr); ok && ref.raw_operand {
		checker_check_dynamic_type_name_expr(ctx, expr, expr)
		return
	}
	checker_check_expr(ctx, expr, .Type)
}

checker_check_dynamic_type_name_expr :: proc(
	ctx: ^Checker_Context,
	dynamic_expr: ^ast.Expr,
	node_expr: ^ast.Expr,
) {
	if dynamic_expr == nil {
		return
	}
	checker_check_expr(ctx, dynamic_expr)
	name, name_range, static_name := checker_dynamic_type_static_name(ctx, dynamic_expr)
	if !static_name {
		return
	}
	interned := project_intern_lower_ascii(ctx.project, name)
	if interned == "" {
		return
	}
	node := &node_expr.expr_base if node_expr != nil else &dynamic_expr.expr_base
	if _, entity, ok := checker_lookup_reference(ctx, .Type, interned); ok {
		checker_add_entity_use(ctx, node, entity)
		checker_check_entity_for_operand(ctx, entity)
		return
	}
	checker_add_unresolved_candidate(
		ctx,
		interned,
		.Type,
		.Global_Symbol,
		.Type_Reference,
		.Unresolved_Type,
		name_range,
		node,
	)
}

checker_dynamic_type_static_name :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
) -> (string, Range, bool) {
	if expr == nil {
		return "", {}, false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Type_Ref_Expr:
		if name, range, ok := checker_dynamic_token_literal_name(n.source); ok {
			return name, range, true
		}
		if len(n.raw_refs) == 1 {
			if name, ok := checker_dynamic_type_constant_name(ctx, n.raw_refs[0]); ok {
				return name, n.raw_refs[0].name.range, true
			}
		}
	case ^ast.Literal_Expr:
		if name, ok := checker_literal_text_value(n.value); ok {
			return name, n.range, true
		}
	case ^ast.Ident_Expr:
		ref := ast.Raw_Operand_Ref{name = ast.Token_Text{text = n.name, range = n.range}}
		if name, ok := checker_dynamic_type_constant_name(ctx, ref); ok {
			return name, n.range, true
		}
	}
	return "", {}, false
}

checker_dynamic_type_constant_name :: proc(
	ctx: ^Checker_Context,
	ref: ast.Raw_Operand_Ref,
) -> (string, bool) {
	if ref.name.text == "" || ref.type_base || ref.call_like || ref.dynamic_path || len(ref.path) > 0 {
		return "", false
	}
	interned := project_intern_lower_ascii(ctx.project, ref.name.text)
	_, entity, ok := checker_lookup_reference(ctx, .Value, interned)
	if !ok || entity == nil || entity.kind != .Constant {
		return "", false
	}
	checker_check_entity_for_operand(ctx, entity)
	payload, payload_ok := entity.payload.(^Entity_Constant_Payload)
	if !payload_ok || payload == nil {
		return "", false
	}
	value, value_ok := payload.constant_value.(^Constant_Text_Value)
	if !value_ok || value == nil {
		return "", false
	}
	return value.value, value.value != ""
}

checker_dynamic_token_literal_name :: proc(token: ast.Token_Text) -> (string, Range, bool) {
	value := strings.trim_space(token.text)
	if len(value) >= 2 && value[0] == '(' && value[len(value) - 1] == ')' {
		value = strings.trim_space(value[1:len(value) - 1])
	}
	name, ok := checker_literal_text_value(value)
	return name, token.range, ok
}

checker_check_line_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Line_Stmt) {
	checker_check_expr(ctx, stmt.line)
	checker_check_expr(ctx, stmt.index)
	checker_check_expr(ctx, stmt.into, .Value, true)
	for field in stmt.fields {
		checker_check_expr(ctx, field.field)
		checker_check_expr(ctx, field.target, .Value, true)
	}
}

checker_check_selection_screen_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Selection_Screen_Stmt) {
	if stmt.title_name.text != "" {
		checker_check_ident_name(ctx, nil, stmt.title_name.text, .Value, false)
	}
	if stmt.comment_name.text != "" {
		checker_check_ident_name(ctx, nil, stmt.comment_name.text, .Value, false)
	}
	if stmt.pushbutton_name.text != "" {
		checker_check_ident_name(ctx, nil, stmt.pushbutton_name.text, .Value, false)
	}
	if stmt.field_name.text != "" {
		checker_check_ident_name(ctx, nil, stmt.field_name.text, .Value, false)
	}
}

checker_check_dataset_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Dataset_Stmt) {
	checker_check_expr(ctx, stmt.dataset)
	checker_check_expr(ctx, stmt.source)
	checker_check_expr(ctx, stmt.target, .Value, stmt.kind == .Read || stmt.kind == .Get)
	checker_check_expr(ctx, stmt.position)
	checker_check_expr(ctx, stmt.message, .Value, true)
	checker_check_expr(ctx, stmt.maximum_length)
	checker_check_expr(ctx, stmt.actual_length, .Value, true)
	checker_check_expr(ctx, stmt.length)
	checker_check_expr(ctx, stmt.attributes, .Value, stmt.kind == .Get)
}

checker_check_select_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Select_Stmt) {
	if stmt.with != nil {
		for cte in stmt.with.entries {
			checker_check_sql_select_query(ctx, cte.query)
		}
	}
	checker_check_sql_select_query(ctx, stmt.query)
	checker_check_stmt_list(ctx, stmt.body)
}

checker_check_cursor_handle_expr :: proc(ctx: ^Checker_Context, handle: ^ast.Expr, lhs: bool) -> Operand {
	if handle == nil {
		return checker_invalid_operand()
	}
	cursor_type := checker_builtin_type_from_name(ctx.checker, "cursor")
	local := ctx^
	local.type_hint = cursor_type
	local.type_hint_expr = handle
	operand := checker_check_expr(&local, handle, .Value, lhs)
	checker_check_cursor_handle_type(ctx, operand.type, cursor_type, checker_expr_range(handle))
	return operand
}

checker_check_cursor_handle_type :: proc(
	ctx: ^Checker_Context,
	actual: ^Type,
	expected: ^Type,
	range: Range,
) {
	if checker_type_is_unknown(actual) || checker_type_is_unknown(expected) {
		return
	}
	actual_name, actual_ok := checker_type_builtin_name(ctx, actual)
	expected_name, expected_ok := checker_type_builtin_name(ctx, expected)
	if actual_ok && expected_ok {
		if actual_name == expected_name {
			return
		}
		checker_add_diagnostic(
			ctx,
			.Incompatible_Assignment_Type,
			range,
			checker_type_mismatch_message(ctx, "cursor handle is not compatible", actual, expected),
		)
		return
	}
	if checker_type_same(actual, expected) {
		return
	}
	if checker_type_is_ref(actual) || checker_type_is_table_like(ctx, actual) || checker_type_structure(actual) != nil {
		checker_add_diagnostic(
			ctx,
			.Incompatible_Assignment_Type,
			range,
			checker_type_mismatch_message(ctx, "cursor handle is not compatible", actual, expected),
		)
	}
}

checker_type_assignment_compatible :: proc(
	ctx: ^Checker_Context,
	src: ^Type,
	dst: ^Type,
	downcast := false,
) -> (bool, bool) {
	if checker_type_same(src, dst) {
		return true, true
	}
	if ok, known := checker_type_ref_compatible(ctx, src, dst, downcast); known {
		return ok, true
	}
	if checker_type_exact_or_generic(ctx, src, dst, false) {
		return true, true
	}
	src_name, src_ok := checker_type_builtin_name(ctx, src)
	dst_name, dst_ok := checker_type_builtin_name(ctx, dst)
	if src_ok && dst_ok {
		return checker_scalar_assignment_conversion(src_name, dst_name)
	}
	src_table := checker_type_is_table_like(ctx, src)
	dst_table := checker_type_is_table_like(ctx, dst)
	if src_table || dst_table {
		if src_table && dst_table {
			return true, true
		}
		if checker_type_is_unknown(src) || checker_type_is_unknown(dst) {
			return false, false
		}
		return false, true
	}
	src_structure := checker_type_structure(src) != nil
	dst_structure := checker_type_structure(dst) != nil
	if src_structure || dst_structure {
		if src_structure && dst_structure {
			return true, true
		}
		return false, false
	}
	return false, false
}

checker_type_argument_compatible :: proc(ctx: ^Checker_Context, src: ^Type, dst: ^Type) -> (bool, bool) {
	if ok, known := checker_type_ref_compatible(ctx, src, dst); known {
		return ok, true
	}
	src_name, src_ok := checker_type_builtin_name(ctx, src)
	dst_name, dst_ok := checker_type_builtin_name(ctx, dst)
	if dst_ok && checker_generic_builtin_type_name(dst_name) {
		if dst_name != "numeric" && dst_name != "decfloat" && dst_name != "clike" {
			return true, true
		}
		return checker_type_generic_accepts(ctx, src, dst), checker_type_generic_actual_family_known(ctx, src, src_name, src_ok)
	}
	if checker_type_exact_or_generic(ctx, src, dst, true) {
		return true, true
	}
	if src_ok && dst_ok {
		return checker_scalar_call_compatibility(src_name, dst_name)
	}
	src_table := checker_type_is_table_like(ctx, src)
	dst_table := checker_type_is_table_like(ctx, dst)
	if src_table || dst_table {
		if src_table && dst_table {
			return true, true
		}
		return false, false
	}
	src_structure := checker_type_structure(src)
	dst_structure := checker_type_structure(dst)
	if src_structure != nil || dst_structure != nil {
		if src_structure != nil && dst_structure != nil && src_structure == dst_structure {
			return true, true
		}
		return false, false
	}
	return false, false
}

checker_type_exact_or_generic :: proc(
	ctx: ^Checker_Context,
	src: ^Type,
	dst: ^Type,
	strict: bool,
) -> bool {
	if checker_type_same(src, dst) || checker_type_generic_accepts(ctx, src, dst) {
		return true
	}
	src_name, src_ok := checker_type_builtin_name(ctx, src)
	dst_name, dst_ok := checker_type_builtin_name(ctx, dst)
	if src_ok && dst_ok {
		if strict {
			return src_name == dst_name
		}
		ok, known := checker_scalar_assignment_conversion(src_name, dst_name)
		return known && ok
	}
	return false
}

checker_type_ref_compatible :: proc(
	ctx: ^Checker_Context,
	src: ^Type,
	dst: ^Type,
	downcast := false,
) -> (bool, bool) {
	src_ref := checker_type_is_ref(src)
	dst_ref := checker_type_is_ref(dst)
	if !src_ref && !dst_ref {
		return false, false
	}
	if src_ref != dst_ref {
		return false, false
	}
	src_target, src_known := checker_ref_target(ctx, src)
	dst_target, dst_known := checker_ref_target(ctx, dst)
	if !src_known || !dst_known {
		return true, false
	}
	if src_target.name == dst_target.name && src_target.name != "" {
		return true, true
	}
	if dst_target.kind == .Data_Generic {
		return checker_ref_target_kind_is_data(src_target.kind), true
	}
	if dst_target.kind == .Object_Generic {
		return checker_ref_target_kind_is_object(src_target.kind), true
	}
	if dst_target.kind == .Data {
		if src_target.kind == .Data_Generic || checker_ref_target_kind_is_object(src_target.kind) {
			return false, true
		}
		return false, false
	}
	if dst_target.kind == .Class {
		switch src_target.kind {
		case .Object_Generic:
			return downcast, true
		case .Class:
			if checker_class_is_or_inherits_from(ctx, src_target.entity, dst_target.name) {
				return true, true
			}
			if downcast && checker_class_is_or_inherits_from(ctx, dst_target.entity, src_target.name) {
				return true, true
			}
			return false, true
		case .Data, .Data_Generic:
			return false, true
		case .Interface:
			if downcast && checker_type_exposes_interface(ctx, dst_target.entity, src_target.name) {
				return true, true
			}
			return false, false
		}
	}
	if dst_target.kind == .Interface {
		switch src_target.kind {
		case .Object_Generic:
			return downcast, true
		case .Class, .Interface:
			if checker_type_exposes_interface(ctx, src_target.entity, dst_target.name) {
				return true, true
			}
			if downcast &&
			   src_target.kind == .Interface &&
			   checker_type_exposes_interface(ctx, dst_target.entity, src_target.name) {
				return true, true
			}
			return false, false
		case .Data, .Data_Generic:
			return false, true
		}
	}
	return false, false
}

checker_ref_target :: proc(ctx: ^Checker_Context, typ: ^Type) -> (Checker_Ref_Target, bool) {
	target_type := checker_type_ref_target(ctx, typ)
	name, name_ok := checker_type_named_name(ctx, target_type)
	if !name_ok {
		return {}, false
	}
	if name == project_intern_lower_ascii(ctx.project, "data") {
		return Checker_Ref_Target{kind = .Data_Generic, name = name}, true
	}
	if name == project_intern_lower_ascii(ctx.project, "object") {
		return Checker_Ref_Target{kind = .Object_Generic, name = name}, true
	}
	if entity := checker_type_object_entity(target_type); entity != nil {
		return Checker_Ref_Target {
			kind   = .Class if entity.kind == .Class else .Interface,
			name   = entity.name,
			entity = entity,
		}, true
	}
	return Checker_Ref_Target{kind = .Data, name = name, entity = checker_type_entity(target_type)}, true
}

checker_ref_target_kind_is_data :: proc(kind: Checker_Ref_Target_Kind) -> bool {
	return kind == .Data || kind == .Data_Generic
}

checker_ref_target_kind_is_object :: proc(kind: Checker_Ref_Target_Kind) -> bool {
	return kind == .Object_Generic || kind == .Class || kind == .Interface
}

checker_class_is_or_inherits_from :: proc(
	ctx: ^Checker_Context,
	class_entity: ^Entity,
	target_name: string,
	depth := 0,
) -> bool {
	if depth > 32 || class_entity == nil || class_entity.kind != .Class {
		return false
	}
	if class_entity.name == target_name {
		return true
	}
	payload, ok := class_entity.payload.(^Entity_Object_Payload)
	if !ok || payload == nil || payload.superclass_name == "" {
		return false
	}
	super, super_ok := checker_lookup_type_name_from_scope(ctx, class_entity.scope, payload.superclass_name, .Class)
	if !super_ok {
		return false
	}
	return checker_class_is_or_inherits_from(ctx, super, target_name, depth + 1)
}

checker_type_exposes_interface :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	interface_name: string,
	depth := 0,
) -> bool {
	if depth > 32 || entity == nil {
		return false
	}
	if entity.kind == .Interface && entity.name == interface_name {
		return true
	}
	payload, ok := entity.payload.(^Entity_Object_Payload)
	if !ok || payload == nil {
		return false
	}
	for implemented_name in payload.implemented_interfaces {
		if implemented_name == interface_name {
			return true
		}
		implemented, implemented_ok := checker_lookup_type_name_from_scope(ctx, entity.scope, implemented_name, .Interface)
		if implemented_ok && checker_type_exposes_interface(ctx, implemented, interface_name, depth + 1) {
			return true
		}
	}
	if entity.kind == .Class && payload.superclass_name != "" {
		super, super_ok := checker_lookup_type_name_from_scope(ctx, entity.scope, payload.superclass_name, .Class)
		if super_ok && checker_type_exposes_interface(ctx, super, interface_name, depth + 1) {
			return true
		}
	}
	return false
}

checker_type_generic_accepts :: proc(ctx: ^Checker_Context, src: ^Type, dst: ^Type) -> bool {
	dst_name, ok := checker_type_builtin_name(ctx, dst)
	if !ok || !checker_generic_builtin_type_name(dst_name) {
		return false
	}
	if dst_name == "any" || dst_name == "data" {
		return true
	}
	src_name, src_ok := checker_type_builtin_name(ctx, src)
	if !src_ok {
		return false
	}
	if src_name == dst_name {
		return true
	}
	switch dst_name {
	case "numeric":
		return checker_builtin_numeric_name(src_name)
	case "decfloat":
		return src_name == "decfloat16" || src_name == "decfloat34"
	case "clike":
		return checker_builtin_clike_name(src_name)
	case "csequence":
		return src_name == "c" || src_name == "string"
	case "xsequence":
		return src_name == "x" || src_name == "xstring"
	case "simple":
		return !checker_type_is_ref(src) && !checker_type_is_table_like(ctx, src)
	}
	return false
}

checker_type_generic_actual_family_known :: proc(
	ctx: ^Checker_Context,
	src: ^Type,
	src_name: string,
	src_ok: bool,
) -> bool {
	if src_ok {
		group := checker_scalar_group(src_name)
		if group != .Unknown && group != .Generic_Simple {
			return true
		}
	}
	return checker_type_structure(src) != nil || checker_type_is_table_like(ctx, src) || checker_type_is_ref(src)
}

checker_type_message_field_compatible :: proc(ctx: ^Checker_Context, typ: ^Type) -> (bool, bool) {
	if checker_type_structure(typ) != nil || checker_type_is_table_like(ctx, typ) || checker_type_is_ref(typ) {
		return false, true
	}
	name, ok := checker_type_builtin_name(ctx, typ)
	if !ok {
		return false, false
	}
	switch name {
	case "c", "n", "d", "t":
		return true, true
	case "i",
	     "int1",
	     "int2",
	     "int4",
	     "int8",
	     "f",
	     "p",
	     "decfloat16",
	     "decfloat34",
	     "string",
	     "x",
	     "xstring",
	     "%_c_pointer":
		return false, true
	}
	if checker_generic_builtin_type_name(name) {
		return false, false
	}
	return false, false
}

checker_type_is_ref :: proc(typ: ^Type, depth := 0) -> bool {
	if depth > 16 || typ == nil {
		return false
	}
	#partial switch typ.kind {
	case .Ref:
		return true
	case .Named:
		return checker_type_is_ref(typ.base, depth + 1)
	case:
		return false
	}
}

checker_type_is_table_like :: proc(ctx: ^Checker_Context, typ: ^Type, depth := 0) -> bool {
	if depth > 16 || typ == nil {
		return false
	}
	#partial switch typ.kind {
	case .Table:
		return true
	case .Named:
		return checker_type_is_table_like(ctx, typ.base, depth + 1)
	case .Builtin:
		name, ok := checker_type_builtin_name(ctx, typ)
		return ok && name == "any table"
	case:
		return false
	}
}

checker_type_builtin_name :: proc(ctx: ^Checker_Context, typ: ^Type, depth := 0) -> (string, bool) {
	if depth > 16 || typ == nil {
		return "", false
	}
	#partial switch typ.kind {
	case .Builtin:
		if typ.name != "" {
			return typ.name, true
		}
	case .Named:
		if typ.entity != nil && entity_is_builtin(typ.entity) && typ.name != "" {
			return typ.name, true
		}
		return checker_type_builtin_name(ctx, typ.base, depth + 1)
	case:
	}
	return "", false
}

checker_type_named_name :: proc(ctx: ^Checker_Context, typ: ^Type, depth := 0) -> (string, bool) {
	if depth > 16 || typ == nil {
		return "", false
	}
	if typ.name != "" {
		return typ.name, true
	}
	return checker_type_named_name(ctx, typ.base, depth + 1)
}

checker_scalar_assignment_conversion :: proc(src_name, dst_name: string) -> (bool, bool) {
	if src_name == dst_name {
		return true, true
	}
	src_group := checker_scalar_group(src_name)
	dst_group := checker_scalar_group(dst_name)
	if src_group == .Unknown || dst_group == .Unknown ||
	   src_group == .Generic_Simple || dst_group == .Generic_Simple {
		return false, false
	}
	if (src_group == .Date && dst_group == .Time) ||
	   (src_group == .Time && dst_group == .Date) {
		return false, true
	}
	return true, true
}

checker_scalar_call_compatibility :: proc(src_name, dst_name: string) -> (bool, bool) {
	if src_name == dst_name {
		return true, true
	}
	src_group := checker_scalar_group(src_name)
	dst_group := checker_scalar_group(dst_name)
	if src_group == .Unknown || dst_group == .Unknown ||
	   src_group == .Generic_Simple || dst_group == .Generic_Simple {
		return false, false
	}
	if src_group == dst_group {
		return true, true
	}
	return false, true
}

checker_scalar_group :: proc(name: string) -> Checker_Scalar_Group {
	switch name {
	case "i", "int1", "int2", "int4", "int8", "p", "decfloat16", "decfloat34", "f":
		return .Numeric
	case "c", "n", "string", "abap_bool":
		return .Character
	case "x", "xstring":
		return .Byte
	case "d":
		return .Date
	case "t":
		return .Time
	case "simple", "numeric", "decfloat", "clike", "csequence", "xsequence", "any", "data":
		return .Generic_Simple
	}
	return .Unknown
}

checker_builtin_numeric_name :: proc(name: string) -> bool {
	switch name {
	case "i", "int1", "int2", "int4", "int8", "p", "decfloat16", "decfloat34", "f":
		return true
	}
	return false
}

checker_builtin_clike_name :: proc(name: string) -> bool {
	switch name {
	case "c", "n", "string", "d", "t", "abap_bool":
		return true
	}
	return false
}

checker_type_mismatch_message :: proc(
	ctx: ^Checker_Context,
	prefix: string,
	src: ^Type,
	dst: ^Type,
) -> string {
	src_name, src_ok := checker_type_diagnostic_name(ctx, src)
	dst_name, dst_ok := checker_type_diagnostic_name(ctx, dst)
	if !src_ok || !dst_ok {
		return prefix
	}
	builder := strings.builder_make(context.temp_allocator)
	strings.write_string(&builder, prefix)
	strings.write_string(&builder, " (current type '")
	strings.write_string(&builder, src_name)
	strings.write_string(&builder, "', expected type '")
	strings.write_string(&builder, dst_name)
	strings.write_string(&builder, "')")
	return strings.to_string(builder)
}

checker_type_diagnostic_name :: proc(ctx: ^Checker_Context, typ: ^Type, depth := 0) -> (string, bool) {
	if depth > 16 || typ == nil {
		return "", false
	}
	switch typ.kind {
	case .Unknown:
		return "", false
	case .Builtin, .Named, .Class, .Interface:
		if typ.name != "" {
			return typ.name, true
		}
		return checker_type_diagnostic_name(ctx, typ.base, depth + 1)
	case .Structure:
		if typ.structure != nil && typ.structure.name != "" {
			return typ.structure.name, true
		}
	case .Table:
		row_name, row_ok := checker_type_diagnostic_name(ctx, typ.base, depth + 1)
		if row_ok {
			builder := strings.builder_make(context.temp_allocator)
			strings.write_string(&builder, "TABLE OF ")
			strings.write_string(&builder, row_name)
			return strings.to_string(builder), true
		}
	case .Ref:
		target_name, target_ok := checker_type_diagnostic_name(ctx, typ.base, depth + 1)
		if target_ok {
			builder := strings.builder_make(context.temp_allocator)
			strings.write_string(&builder, "REF TO ")
			strings.write_string(&builder, target_name)
			return strings.to_string(builder), true
		}
	case .Routine:
		return "routine", true
	}
	return "", false
}

checker_expr_range :: proc(expr: ^ast.Expr) -> Range {
	return expr.range if expr != nil else Range{}
}
