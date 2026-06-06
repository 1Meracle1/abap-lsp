package abap_frontend_semantic

import "src:ast"
import string_interner "src:string_interner"

import "core:strings"

Checker_Call_Argument :: struct {
	name:          string_interner.String,
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
	name:    string_interner.String,
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
	name:   string_interner.String,
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
	case ^ast.Data_Decl,
	     ^ast.Data_Chained_Decl,
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
	     ^ast.Oop_Load_Stmt:
		if collect_declarations {
			checker_collect_stmt_entities(ctx, stmt)
		}
	case ^ast.Data_Inline_Decl:
		if collect_declarations {
			checker_collect_stmt_entities(ctx, stmt)
		}
		rhs := checker_check_expr(ctx, n.expr)
		checker_apply_inline_decl_type(ctx, n.name, rhs.type)
	case ^ast.Assign_Stmt:
		checker_check_assignment_stmt(ctx, n.lhs, n.rhs)
	case ^ast.Downcast_Assign_Stmt:
		checker_check_assignment_stmt(ctx, n.lhs, n.rhs, downcast = true)
	case ^ast.Expr_Stmt:
		checker_check_expr(ctx, n.expr)
	case ^ast.Clear_Stmt:
		for operand in n.operands {
			checker_check_expr(ctx, operand.target, .Value, true)
			checker_check_expr(ctx, operand.value)
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
		for entry in n.entries {
			checker_check_expr(ctx, entry.source)
			checker_check_expr(ctx, entry.target, .Value, true)
		}
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
		for entry in n.entries {
			checker_check_expr_list(ctx, entry.sources[:])
			checker_check_expr(ctx, entry.target, .Value, true)
			checker_check_expr(ctx, entry.separator)
		}
	case ^ast.Split_Stmt:
		for entry in n.entries {
			checker_check_expr(ctx, entry.source)
			checker_check_expr(ctx, entry.separator)
			checker_check_expr_list(ctx, entry.targets[:], .Value, true)
		}
	case ^ast.Condense_Stmt:
		checker_check_expr(ctx, n.target, .Value, true)
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
		checker_check_expr(ctx, n.target, .Value, true)
		checker_check_expr(ctx, n.places)
		checker_check_expr(ctx, n.delete_pattern)
	case ^ast.Find_Stmt:
		checker_check_expr(ctx, n.pattern)
		checker_check_expr(ctx, n.target)
		checker_check_expr(ctx, n.section_offset)
		checker_check_expr(ctx, n.section_length)
		checker_check_expr(ctx, n.match_offset, .Value, true)
		checker_check_expr(ctx, n.match_length, .Value, true)
		checker_check_expr(ctx, n.match_line, .Value, true)
		checker_check_expr(ctx, n.match_count, .Value, true)
		checker_check_expr(ctx, n.results, .Value, true)
		checker_check_expr_list(ctx, n.submatches[:], .Value, true)
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
		for entry in n.entries {
			checker_check_expr(ctx, entry.source)
			checker_check_expr(ctx, entry.target, .Value, true)
		}
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
		checker_check_expr(ctx, n.time_stamp, .Value, n.kind == .Date_Time_To_Time_Stamp)
		checker_check_expr(ctx, n.time_zone)
		checker_check_expr(ctx, n.date, .Value, n.kind == .Time_Stamp_To_Date_Time)
		checker_check_expr(ctx, n.time, .Value, n.kind == .Time_Stamp_To_Date_Time)
	case ^ast.List_Control_Stmt:
		checker_check_expr_list(ctx, n.operands[:])
	case ^ast.Line_Stmt:
		checker_check_line_stmt(ctx, n)
	case ^ast.Macro_Call_Stmt:
		checker_check_expr_list(ctx, n.args[:])
	case ^ast.Selection_Screen_Stmt:
		checker_check_selection_screen_stmt(ctx, n)
	case ^ast.If_Stmt:
		checker_check_expr(ctx, n.condition)
		checker_check_stmt_list(ctx, n.body)
		for clause in n.elseif_clauses {
			checker_check_expr(ctx, clause.condition)
			checker_check_stmt_list(ctx, clause.body)
		}
		if n.else_clause != nil {
			checker_check_stmt_list(ctx, n.else_clause.body)
		}
	case ^ast.Case_Stmt:
		checker_check_expr(ctx, n.expr)
		for clause in n.whens {
			checker_check_expr_list(ctx, clause.operands[:])
			checker_check_stmt_list(ctx, clause.body)
		}
		checker_check_stmt_list(ctx, n.recovery)
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
			checker_check_expr_list(ctx, clause.exceptions[:], .Type)
			checker_check_expr(ctx, clause.into, .Value, true)
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
		checker_check_expr(ctx, n.handle, .Value, true)
		checker_check_sql_select_query(ctx, n.query)
	case ^ast.Fetch_Stmt:
		checker_check_expr(ctx, n.handle)
		checker_check_sql_select_result(
			ctx,
			n.result,
			Sql_Query_Shape {
				row_type    = project_type_unknown(ctx.project),
				scalar_type = project_type_unknown(ctx.project),
			},
		)
		checker_check_expr(ctx, n.package_size)
	case ^ast.Close_Cursor_Stmt:
		checker_check_expr(ctx, n.handle)
	case ^ast.Insert_Stmt:
		checker_check_insert_stmt(ctx, n)
	case ^ast.Append_Stmt:
		checker_check_append_stmt(ctx, n)
	case ^ast.Modify_Stmt:
		checker_check_modify_stmt(ctx, n)
	case ^ast.Sort_Stmt:
		checker_check_expr(ctx, n.target, .Value, true)
		for field in n.fields {
			checker_check_expr(ctx, field.expr)
		}
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

checker_check_assignment_stmt :: proc(
	ctx: ^Checker_Context,
	lhs_expr: ^ast.Expr,
	rhs_expr: ^ast.Expr,
	downcast := false,
) {
	lhs := checker_check_expr(ctx, lhs_expr, .Value, true)
	rhs_ctx := ctx^
	rhs_ctx.type_hint = lhs.type
	rhs_ctx.type_hint_expr = lhs_expr
	rhs := checker_check_expr(&rhs_ctx, rhs_expr)
	checker_check_assignment_compatibility(ctx, rhs.type, lhs.type, checker_expr_range(rhs_expr), downcast)
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
	row_type := checker_type_row(ctx, source.type)
	checker_check_table_line_target(ctx, stmt.target, row_type, stmt.target_kind)
	checker_check_expr(ctx, stmt.from)
	checker_check_expr(ctx, stmt.to)
	checker_check_table_key_selector(ctx, stmt.using_key)
	checker_check_expr(ctx, stmt.where_cond)
	checker_check_expr(ctx, stmt.group_by)
	checker_check_table_line_target(ctx, stmt.group_target, row_type, stmt.group_target_kind)
	checker_check_stmt_list(ctx, stmt.body)
}

checker_check_read_table_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Read_Table_Stmt) {
	for entry in stmt.entries {
		table := checker_check_expr(ctx, entry.table)
		row_type := checker_type_row(ctx, table.type)
		row_structure := checker_type_structure(row_type)
		checker_check_table_line_target(ctx, entry.into, row_type, .Into)
		checker_check_table_line_target(ctx, entry.assigning, row_type, .Assigning)
		checker_check_table_line_target(ctx, entry.reference_into, row_type, .Reference_Into)
		checker_check_expr(ctx, entry.index)
		checker_check_table_key_selector(ctx, entry.using_key)
		checker_check_expr_list(ctx, entry.comparing[:])
		for key in entry.key_values {
			checker_check_expr(ctx, key.dynamic_name)
			checker_check_read_table_key_name(ctx, row_type, row_structure, key)
			checker_check_expr(ctx, key.value)
		}
	}
}

checker_check_read_table_key_name :: proc(
	ctx: ^Checker_Context,
	row_type: ^Type,
	row_structure: ^Structure,
	key: ast.Read_Table_Key_Value_Clause,
) {
	if key.is_dynamic || key.table_line || len(key.path) == 0 {
		return
	}
	structure := row_structure
	current_type := row_type
	for segment in key.path {
		if segment.name == "table_line" {
			continue
		}
		if structure == nil {
			return
		}
		name := checker_intern_name(ctx.project, segment.name)
		field, ok := checker_lookup_structure_field(structure, name)
		if !ok {
			return
		}
		checker_add_entity_use(ctx, nil, field)
		current_type = field.type
		structure = checker_type_structure(current_type)
	}
}

checker_check_append_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Append_Stmt) {
	target := checker_check_expr(ctx, stmt.target, .Value, true)
	row_type := checker_type_row(ctx, target.type)
	if stmt.source != nil {
		source := checker_check_expr(ctx, stmt.source)
		expected := row_type
		if stmt.lines_of {
			expected = target.type
		}
		checker_check_assignment_compatibility(ctx, source.type, expected, checker_expr_range(stmt.source))
	}
	checker_check_table_line_target(ctx, stmt.assigning, row_type, .Assigning)
	checker_check_table_line_target(ctx, stmt.reference_into, row_type, .Reference_Into)
}

checker_check_insert_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Insert_Stmt) {
	if stmt.form == .Db_Table {
		checker_check_sql_insert_stmt(ctx, stmt)
		return
	}
	target := checker_check_expr(ctx, stmt.target, .Value, stmt.form == .Internal_Table)
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

checker_check_modify_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Modify_Stmt) {
	if checker_modify_stmt_uses_db_source(ctx, stmt) {
		checker_check_sql_modify_stmt(ctx, stmt)
		return
	}
	target := checker_check_expr(ctx, stmt.target, .Value, true)
	if stmt.source != nil {
		source := checker_check_expr(ctx, stmt.source)
		row_type := checker_type_row(ctx, target.type)
		expected := row_type if !checker_type_is_unknown(row_type) else target.type
		checker_check_assignment_compatibility(ctx, source.type, expected, checker_expr_range(stmt.source))
	}
	checker_check_expr(ctx, stmt.index)
	checker_check_expr(ctx, stmt.where_cond)
}

checker_modify_stmt_uses_db_source :: proc(ctx: ^Checker_Context, stmt: ^ast.Modify_Stmt) -> bool {
	if stmt == nil || stmt.target == nil || stmt.table_keyword {
		return false
	}
	if stmt.dynamic_source {
		return true
	}
	name := checker_sql_simple_expr_name(ctx, stmt.target)
	if !string_interner.is_valid(name) {
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
	checker_check_expr(ctx, stmt.target, .Value, true)
	checker_check_expr(ctx, stmt.source)
	checker_check_expr(ctx, stmt.index)
	checker_check_expr(ctx, stmt.where_cond)
	checker_check_table_key_selector(ctx, stmt.using_key)
	for comparing in stmt.comparing {
		checker_check_expr(ctx, comparing.expr)
	}
}

checker_check_table_key_selector :: proc(ctx: ^Checker_Context, selector: ast.Table_Key_Selector) {
	checker_check_expr(ctx, selector.dynamic_name)
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
		for arg in args {
			checker_check_call_argument_value(ctx, arg, nil, false)
		}
		return
	}
	checker_check_routine_call_arguments(ctx, callee.entity, args[:], call.range)
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
		arg.name_text = named.name
		arg.name = checker_intern_name(ctx.project, named.name)
		arg.name_range = named.range
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
		checker_check_expr(ctx, stmt.function_destination)
		checker_check_expr(ctx, stmt.function_task)
		checker_check_expr(ctx, stmt.function_end_task_handler, .Routine)
		checker_check_call_stmt_args(ctx, stmt.named_args[:], routine, stmt.range)
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
) {
	args := make([dynamic]Checker_Call_Argument, 0, len(named_args), context.temp_allocator)
	for named in named_args {
		arg := Checker_Call_Argument {
			name          = checker_intern_name(ctx.project, named.name),
			name_text     = named.name,
			name_range    = named.name_range,
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
		checker_check_routine_call_arguments(ctx, routine, args[:], call_range)
		return
	}
	for arg in args {
		checker_check_call_argument_value(ctx, arg, nil, false)
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
	interned := checker_intern_name(ctx.project, name)
	_, entity, ok := checker_lookup_reference(ctx, .Routine, interned)
	if !ok || entity.kind != .Module {
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
		return ref.name
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
) {
	if routine == nil || routine.kind == .Builtin {
		for arg in args {
			checker_check_call_argument_value(ctx, arg, nil, checker_call_arg_requires_writable(arg.section))
		}
		return
	}
	checker_check_entity_for_operand(ctx, routine)
	payload, ok := routine.payload.(^Entity_Routine_Payload)
	if !ok || payload == nil {
		for arg in args {
			checker_check_call_argument_value(ctx, arg, nil, checker_call_arg_requires_writable(arg.section))
		}
		return
	}

	supplied := make([dynamic]^Entity, 0, len(args), context.temp_allocator)
	positional := make([dynamic]int, 0, len(args), context.temp_allocator)
	seen := make(map[Checker_Call_Parameter_Key]bool, len(args), context.temp_allocator)
	required_mapping_ok := true

	for arg, index in args {
		if arg.section == .Exceptions {
			checker_check_call_argument_value(ctx, arg, nil, false)
			if arg.message != nil {
				checker_check_call_function_exception_message(ctx, arg.message, arg.message_range)
			}
			continue
		}
		if !string_interner.is_valid(arg.name) {
			append(&positional, index)
			continue
		}
		section := checker_call_effective_actual_section(routine.kind, arg.section, arg.has_section)
		formal, formal_ok := checker_call_find_named_parameter(ctx, routine, payload.parameters[:], arg.name, section)
		if !formal_ok {
			checker_check_call_argument_value(ctx, arg, nil, checker_call_arg_requires_writable(section))
			checker_add_diagnostic(ctx, .Unknown_Named_Parameter, arg.name_range, "unknown named parameter")
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
		if len(positional) == len(eligible) {
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

	if required_mapping_ok {
		checker_check_missing_required_parameters(ctx, routine, payload.parameters[:], supplied[:], call_range)
	}
}

checker_check_call_argument_with_parameter :: proc(
	ctx: ^Checker_Context,
	arg: Checker_Call_Argument,
	actual_section: ast.Call_Arg_Section_Kind,
	formal: ^Entity,
) {
	checker_check_entity_for_operand(ctx, formal)
	formal_type := formal.type if formal != nil && formal.type != nil else project_type_unknown(ctx.project)
	receives := checker_call_arg_receives_from_formal(actual_section)
	writable := checker_call_arg_requires_writable(actual_section)
	actual := checker_check_call_argument_value(ctx, arg, formal_type, writable)
	if writable && !checker_operand_is_writable(actual) {
		checker_add_diagnostic(ctx, .Incompatible_Argument_Type, arg.value_range, "argument is not writable")
		return
	}
	if receives {
		checker_check_argument_compatibility(ctx, formal_type, actual.type, arg.value_range)
	} else {
		checker_check_argument_compatibility(ctx, actual.type, formal_type, arg.value_range)
	}
}

checker_check_call_argument_value :: proc(
	ctx: ^Checker_Context,
	arg: Checker_Call_Argument,
	type_hint: ^Type,
	lhs: bool,
) -> Operand {
	if arg.value != nil {
		local := ctx^
		local.type_hint = type_hint
		local.type_hint_expr = arg.value
		return checker_check_expr(&local, arg.value, .Value, lhs)
	}
	for decl in arg.raw_decls {
		kind := Entity_Kind.Variable if decl.kind == .Data else Entity_Kind.Field_Symbol
		checker_collect_inferred_expr_decl(ctx, decl.name, kind, decl.range, nil, type_hint)
	}
	for ref in arg.raw_refs {
		namespace := Namespace.Routine if ref.call_like else Namespace.Value
		if ref.type_base {
			namespace = .Type
		}
		base := checker_check_ident_expr(ctx, nil, ref.name, namespace, false)
		for segment in ref.path {
			member_namespace := checker_selector_member_namespace(segment.selector, namespace)
			base = checker_lookup_selector_member(ctx, base, segment.selector, segment.name, member_namespace, nil, false)
		}
	}
	return Operand{mode = .Value, type = type_hint if type_hint != nil else project_type_unknown(ctx.project)}
}

checker_check_call_function_exception_message :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	range: Range,
) {
	operand := checker_check_expr(ctx, expr)
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
		checker_add_diagnostic(ctx, .Missing_Required_Parameter, range, "missing required parameter", param, param.decl_info)
	}
}

checker_call_find_named_parameter :: proc(
	ctx: ^Checker_Context,
	routine: ^Entity,
	params: []^Entity,
	name: string_interner.String,
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
	checker_check_expr(ctx, stmt.target)
	for option in stmt.options {
		checker_check_expr(ctx, option.value)
		checker_check_expr(ctx, option.high_value)
	}
}

checker_check_runtime_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Runtime_Stmt) {
	checker_check_expr(ctx, stmt.id)
	checker_check_expr(ctx, stmt.field, .Value, stmt.kind == .Get)
	checker_check_expr(ctx, stmt.target, .Value, true)
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
		checker_check_expr(ctx, param.value, .Value, lhs)
	}
}

checker_check_data_cluster_medium :: proc(ctx: ^Checker_Context, medium: ast.Data_Cluster_Medium_Clause) {
	checker_check_expr(ctx, medium.object)
	checker_check_expr(ctx, medium.work_area)
	checker_check_expr(ctx, medium.client)
	checker_check_expr(ctx, medium.id)
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
	checker_check_expr(ctx, stmt.casting_type, .Type)
	checker_check_expr(ctx, stmt.casting_decimals)
}

checker_check_create_object_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Create_Object_Stmt) {
	checker_check_expr(ctx, stmt.target, .Value, true)
	checker_check_expr(ctx, stmt.type_ref, .Type)
	if stmt.type_clause != nil {
		checker_check_decl_type_clause(ctx, nil, stmt.type_clause)
	}
	checker_check_expr(ctx, stmt.type_dynamic_expr)
	checker_check_expr_list(ctx, stmt.operands[:])
}

checker_check_create_data_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Create_Data_Stmt) {
	checker_check_expr(ctx, stmt.target, .Value, true)
	checker_check_expr(ctx, stmt.type_ref, .Type)
	if stmt.type_clause != nil {
		checker_check_decl_type_clause(ctx, nil, stmt.type_clause)
	}
	checker_check_expr(ctx, stmt.type_dynamic_expr)
	checker_check_expr(ctx, stmt.type_handle)
	checker_check_expr_list(ctx, stmt.operands[:])
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
	if stmt.title_name != "" {
		checker_check_ident_name(ctx, nil, stmt.title_name, .Value, false)
	}
	if stmt.comment_name != "" {
		checker_check_ident_name(ctx, nil, stmt.comment_name, .Value, false)
	}
	if stmt.field_name != "" {
		checker_check_ident_name(ctx, nil, stmt.field_name, .Value, false)
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
		return false, false
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
	if src_target.name == dst_target.name && string_interner.is_valid(src_target.name) {
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
	if name == checker_intern_name(ctx.project, "data") {
		return Checker_Ref_Target{kind = .Data_Generic, name = name}, true
	}
	if name == checker_intern_name(ctx.project, "object") {
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
	target_name: string_interner.String,
	depth := 0,
) -> bool {
	if depth > 32 || class_entity == nil || class_entity.kind != .Class {
		return false
	}
	if class_entity.name == target_name {
		return true
	}
	payload, ok := class_entity.payload.(^Entity_Object_Payload)
	if !ok || payload == nil || !string_interner.is_valid(payload.superclass_name) {
		return false
	}
	_, super, super_ok := checker_lookup_lexical_declaration_from_scope(class_entity.scope, .Type, payload.superclass_name)
	if !super_ok {
		return false
	}
	return checker_class_is_or_inherits_from(ctx, super, target_name, depth + 1)
}

checker_type_exposes_interface :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	interface_name: string_interner.String,
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
		_, implemented, implemented_ok := checker_lookup_lexical_declaration_from_scope(entity.scope, .Type, implemented_name)
		if implemented_ok && checker_type_exposes_interface(ctx, implemented, interface_name, depth + 1) {
			return true
		}
	}
	if entity.kind == .Class && string_interner.is_valid(payload.superclass_name) {
		_, super, super_ok := checker_lookup_lexical_declaration_from_scope(entity.scope, .Type, payload.superclass_name)
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
		if string_interner.is_valid(typ.name) {
			return string_interner.load(ctx.project.interner, typ.name), true
		}
	case .Named:
		if typ.entity != nil && entity_is_builtin(typ.entity) && string_interner.is_valid(typ.name) {
			return string_interner.load(ctx.project.interner, typ.name), true
		}
		return checker_type_builtin_name(ctx, typ.base, depth + 1)
	case:
	}
	return "", false
}

checker_type_named_name :: proc(ctx: ^Checker_Context, typ: ^Type, depth := 0) -> (string_interner.String, bool) {
	if depth > 16 || typ == nil {
		return string_interner.String(0), false
	}
	if string_interner.is_valid(typ.name) {
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
		if string_interner.is_valid(typ.name) {
			return string_interner.load(ctx.project.interner, typ.name), true
		}
		return checker_type_diagnostic_name(ctx, typ.base, depth + 1)
	case .Structure:
		if typ.structure != nil && string_interner.is_valid(typ.structure.name) {
			return string_interner.load(ctx.project.interner, typ.structure.name), true
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
