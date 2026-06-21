package abap_frontend_lints

import "src:ast"
import "src:semantic"
import "src:tokenizer"
import "src:utils"

import "core:mem"
import "core:strconv"
import "core:strings"

Value_Flow_Kind :: enum {
	Assignment,
	Call_Argument,
	Field_Symbol_Assignment,
	Conditional_Field_Symbol_Assignment,
}

Fact_Scope_Data :: struct {
	parent: int,
	range:  tokenizer.Range,
}

Guard_Data :: struct {
	name:   string,
	entity: ^semantic.Entity,
}

Select_Projection_Target_Info :: struct {
	name:  string,
	range: tokenizer.Range,
	field: ^semantic.Entity,
}

Value_Flow_Target_Kind :: enum {
	Assignment,
	Call_Parameter,
	Field_Symbol,
}

Value_Flow_Target_Data :: struct {
	kind:        Value_Flow_Target_Kind,
	range:       tokenizer.Range,
	call_range:  tokenizer.Range,
	name:        string,
}

Value_Flow_Edge_Data :: struct {
	scope:        ^semantic.Scope,
	kind:         Value_Flow_Kind,
	source_range: tokenizer.Range,
	target:       Value_Flow_Target_Data,
}

Perform_Parameter_Section :: enum {
	Tables,
	Using,
	Changing,
}

Perform_Argument_Data :: struct {
	range:              tokenizer.Range,
	section:            Perform_Parameter_Section,
	ordinal_in_section: int,
}

Perform_Program_Data :: struct {
	name:       string,
	range:      tokenizer.Range,
	is_dynamic: bool,
}

Perform_Call_Flag :: enum {
	Is_Dynamic,
	Has_Program,
	Has_If_Found,
	Section_Order_Invalid,
}
Perform_Call_Flags :: bit_set[Perform_Call_Flag]

Perform_Call_Data :: struct {
	range:         tokenizer.Range,
	routine_name:  string,
	routine_range: tokenizer.Range,
	program:       Perform_Program_Data,
	parameters:    [dynamic]Perform_Parameter_Section,
	arguments:     [dynamic]Perform_Argument_Data,
	flags:         Perform_Call_Flags,
}

Routine_Loop_Kind :: enum {
	While,
	Do,
	Loop,
}

Routine_Site_Kind :: enum {
	Unknown_Effect,
	Clear,
	Unassign,
	Delete,
	Read_Table,
	Return,
	Raise,
	Leave,
	Leave_List_Processing,
	Exit,
	Continue,
	Stop,
}

Routine_Site_Data :: struct {
	range:        tokenizer.Range,
	kind:         Routine_Site_Kind,
	target_range: tokenizer.Range,
	has_target:   bool,
}

Routine_Callable_Summary :: struct {
	entity:    ^semantic.Entity,
	no_return: bool,
}

Internal_Table_Order_Data :: struct {
	range:      tokenizer.Range,
	table_name: string,
	key_fields: [dynamic]string,
}

Read_Table_Binary_Search_Data :: struct {
	range:      tokenizer.Range,
	table_name: string,
	key_fields: [dynamic]string,
}

Find_Write_Target_Data :: struct {
	range:               tokenizer.Range,
	definitely_assigned: bool,
}

Find_Site_Data :: struct {
	range:         tokenizer.Range,
	read_ranges:   [dynamic]tokenizer.Range,
	write_targets: [dynamic]Find_Write_Target_Data,
}

System_Field_Statement_Kind :: enum {
	Append,
	Assign,
	Authority_Check,
	Call_Function,
	Convert,
	Delete_Report,
	Delete_Table,
	Delete_Db_Table,
	Describe_Table,
	Do,
	Find,
	Insert_Report,
	Insert_Table,
	Insert_Db_Table,
	Insert_Textpool,
	Loop_At,
	Message,
	Modify_Table,
	Modify_Db_Table,
	Read_Report,
	Read_Table,
	Search,
	Select,
	Syntax_Check,
	Update_Db_Table,
	While,
}

System_Field_Update_Data :: struct {
	scope:      int,
	range:      tokenizer.Range,
	statement:  System_Field_Statement_Kind,
	field_name: string,
}

Call_Function_Output_Target_Data :: struct {
	range:  tokenizer.Range,
	name:   string,
	entity: ^semantic.Entity,
}

Call_Function_Result_Data :: struct {
	scope:                 int,
	range:                 tokenizer.Range,
	has_exception_mapping: bool,
	has_changing_argument: bool,
	output_targets:        [dynamic]Call_Function_Output_Target_Data,
}

Value_Read_Data :: struct {
	scope:  int,
	range:  tokenizer.Range,
	name:   string,
	entity: ^semantic.Entity,
}

Field_Symbol_State_Check_Kind :: enum {
	Is_Assigned,
	Is_Not_Assigned,
}

Value_State_Check_Kind :: enum {
	Is_Initial,
	Is_Not_Initial,
	Equals_Zero,
	Not_Equals_Zero,
	Condition_Probe,
}

Field_Symbol_State_Check_Data :: struct {
	range:        tokenizer.Range,
	symbol_name:  string,
	symbol_range: tokenizer.Range,
	kind:         Field_Symbol_State_Check_Kind,
}

Value_State_Check_Data :: struct {
	scope:        int,
	range:        tokenizer.Range,
	symbol_name:  string,
	symbol_range: tokenizer.Range,
	field_name:   string,
	kind:         Value_State_Check_Kind,
}

At_Group_Kind :: enum {
	First,
	New,
	End_Of,
	Last,
}

Routine_Control_Region_Kind :: enum {
	If,
	Case,
	Loop,
	At,
	Try,
}

Routine_Control_Region_Data :: struct {
	kind:       Routine_Control_Region_Kind,
	range:      tokenizer.Range,
	loop_kind:  Routine_Loop_Kind,
	at_kind:    At_Group_Kind,
}

Unit_Lints :: struct {
	uri:                         string,
	project:                     ^semantic.Project,
	checker:                     ^semantic.Checker,
	file:                        ^semantic.Project_File,
	diagnostics:                 [dynamic]Diagnostic,
	fact_scopes:                 [dynamic]Fact_Scope_Data,
	value_reads:                 [dynamic]Value_Read_Data,
	value_flow_edges:            [dynamic]Value_Flow_Edge_Data,
	perform_calls:               [dynamic]Perform_Call_Data,
	find_sites:                  [dynamic]Find_Site_Data,
	system_field_updates:        [dynamic]System_Field_Update_Data,
	call_function_results:       [dynamic]Call_Function_Result_Data,
	routine_sites:               [dynamic]Routine_Site_Data,
	internal_table_orders:       [dynamic]Internal_Table_Order_Data,
	read_table_binary_searches:  [dynamic]Read_Table_Binary_Search_Data,
	field_symbol_state_checks:   [dynamic]Field_Symbol_State_Check_Data,
	value_state_checks:          [dynamic]Value_State_Check_Data,
	routine_control_regions:     [dynamic]Routine_Control_Region_Data,
	callable_summaries:          [dynamic]Routine_Callable_Summary,
}

fact_scope_child :: proc(out: ^Unit_Lints, parent: int, range: tokenizer.Range) -> int {
	if out == nil {
		return 0
	}
	if len(out.fact_scopes) == 0 {
		append(&out.fact_scopes, Fact_Scope_Data{parent = -1})
	}
	parent_id := parent
	if parent_id < 0 || parent_id >= len(out.fact_scopes) {
		parent_id = 0
	}
	append(&out.fact_scopes, Fact_Scope_Data{parent = parent_id, range = range})
	return len(out.fact_scopes) - 1
}

fact_scope_descends_from :: proc(out: ^Unit_Lints, child, ancestor: int) -> bool {
	if out == nil || child < 0 || ancestor < 0 || child >= len(out.fact_scopes) || ancestor >= len(out.fact_scopes) {
		return false
	}
	current := child
	for current >= 0 && current < len(out.fact_scopes) {
		if current == ancestor {
			return true
		}
		current = out.fact_scopes[current].parent
	}
	return false
}

fact_scopes_may_share_sequential_flow :: proc(out: ^Unit_Lints, left, right: int) -> bool {
	return left == right ||
	       fact_scope_descends_from(out, left, right) ||
	       fact_scope_descends_from(out, right, left)
}

event_block_leave_list_processing_exits :: proc "contextless" (kind: ast.Event_Block_Kind) -> bool {
	#partial switch kind {
	case .Initialization,
	     .Start_Of_Selection,
	     .End_Of_Selection,
	     .Top_Of_Page,
	     .End_Of_Page:
		return true
	}
	return false
}

lint_entity_for_access :: proc(out: ^Unit_Lints, access: Value_Access) -> ^semantic.Entity {
	if out == nil || out.project == nil || out.checker == nil || out.file == nil || access.base_range.end <= access.base_range.start {
		return nil
	}
	query := semantic.semantic_query(out.project, out.checker, out.file)
	use := semantic.semantic_ref_use_at_range(semantic.semantic_query_refs(query), access.base_range)
	return use.entity if use != nil else nil
}


collect_flat_stmt_list :: proc(
	out: ^Unit_Lints,
	stmts: []^ast.Stmt,
	allocator: mem.Allocator,
	policy: ^Policy = nil,
	loop_depth: int = 0,
	guarded_tables: []Guard_Data = nil,
	loop_kind: Routine_Loop_Kind = .Loop,
	fact_scope: int = 0,
	leave_list_processing_exits: bool = false,
) {
	flow_guards := make([dynamic]Guard_Data, 0, len(guarded_tables) + 2, allocator)
	for guard in guarded_tables {
		append(&flow_guards, guard)
	}
	unreachable := false
	for stmt in stmts {
		if stmt == nil {
			continue
		}
		if unreachable {
			if metadata, ok := metadata_for(UNREACHABLE_CODE); ok {
				emit_diagnostic(
					out,
					metadata,
					stmt.range,
					"statement can never be executed",
					policy,
					allocator,
				)
			}
		}
		collect_flat_stmt(
			out,
			stmt,
			allocator,
			policy,
			loop_depth,
			flow_guards[:],
			loop_kind,
			fact_scope,
			leave_list_processing_exits,
		)
		if guard, ok := initial_guard_with_direct_exit(out, stmt, allocator, leave_list_processing_exits); ok {
			append(&flow_guards, guard)
		}
		if stmt_prevents_fallthrough(out, stmt, leave_list_processing_exits) {
			unreachable = true
		}
	}
}

collect_flat_stmt :: proc(
	out: ^Unit_Lints,
	stmt: ^ast.Stmt,
	allocator: mem.Allocator,
	policy: ^Policy = nil,
	loop_depth: int = 0,
	guarded_tables: []Guard_Data = nil,
	loop_kind: Routine_Loop_Kind = .Loop,
	fact_scope: int = 0,
	leave_list_processing_exits: bool = false,
) {
	#partial switch n in stmt.derived_stmt {
	case ^ast.Assign_Stmt:
		add_value_flow(out, n.lhs, n.rhs)
		collect_expr_lints(out, n.lhs, allocator, false, fact_scope)
		collect_expr_lints(out, n.rhs, allocator, fact_scope = fact_scope)
	case ^ast.Downcast_Assign_Stmt:
		add_value_flow(out, n.lhs, n.rhs)
		collect_expr_lints(out, n.lhs, allocator, false, fact_scope)
		collect_expr_lints(out, n.rhs, allocator, fact_scope = fact_scope)
	case ^ast.Expr_Stmt:
		collect_expr_lints(out, n.expr, allocator, fact_scope = fact_scope)
	case ^ast.If_Stmt:
		body_scope := fact_scope_child(out, fact_scope, n.range)
		collect_expr_lints(out, n.condition, allocator, fact_scope = body_scope)
		body_guards := guarded_tables
		if guard, ok := non_initial_guard_data_from_condition(out, n.condition, allocator); ok {
			body_guards = guard_data_list_with(body_guards, guard, allocator)
		}
		collect_flat_stmt_list(out, n.body[:], allocator, policy, loop_depth, body_guards, loop_kind, body_scope, leave_list_processing_exits)
		for clause in n.elseif_clauses {
			clause_scope := fact_scope_child(out, fact_scope, clause.range)
			collect_expr_lints(out, clause.condition, allocator, fact_scope = clause_scope)
			clause_guards := guarded_tables
			if guard, ok := non_initial_guard_data_from_condition(out, clause.condition, allocator); ok {
				clause_guards = guard_data_list_with(clause_guards, guard, allocator)
			}
			collect_flat_stmt_list(out, clause.body[:], allocator, policy, loop_depth, clause_guards, loop_kind, clause_scope, leave_list_processing_exits)
		}
		if n.else_clause != nil {
			else_scope := fact_scope_child(out, fact_scope, n.else_clause.range)
			else_guards := guarded_tables
			if guard, ok := initial_guard_data_from_condition(out, n.condition, allocator); ok {
				else_guards = guard_data_list_with(else_guards, guard, allocator)
			}
			collect_flat_stmt_list(out, n.else_clause.body[:], allocator, policy, loop_depth, else_guards, loop_kind, else_scope, leave_list_processing_exits)
		}
	case ^ast.Case_Stmt:
		collect_expr_lints(out, n.expr, allocator, fact_scope = fact_scope)
		collect_flat_stmt_list(out, n.recovery[:], allocator, policy, loop_depth, guarded_tables, loop_kind, fact_scope, leave_list_processing_exits)
		for clause in n.whens {
			clause_scope := fact_scope_child(out, fact_scope, clause.range)
			for operand in clause.operands {
				collect_expr_lints(out, operand, allocator, fact_scope = clause_scope)
			}
			collect_flat_stmt_list(out, clause.body[:], allocator, policy, loop_depth, guarded_tables, loop_kind, clause_scope, leave_list_processing_exits)
		}
	case ^ast.While_Stmt:
		body_scope := fact_scope_child(out, fact_scope, n.range)
		collect_expr_lints(out, n.condition, allocator, fact_scope = body_scope)
		next_loop_kind := loop_kind if loop_depth > 0 else Routine_Loop_Kind.While
		collect_flat_stmt_list(out, n.body[:], allocator, policy, loop_depth + 1, guarded_tables, next_loop_kind, body_scope, leave_list_processing_exits)
	case ^ast.Do_Stmt:
		body_scope := fact_scope_child(out, fact_scope, n.range)
		collect_expr_lints(out, n.count, allocator, fact_scope = body_scope)
		next_loop_kind := loop_kind if loop_depth > 0 else Routine_Loop_Kind.Do
		collect_flat_stmt_list(out, n.body[:], allocator, policy, loop_depth + 1, guarded_tables, next_loop_kind, body_scope, leave_list_processing_exits)
	case ^ast.Loop_Stmt:
		body_scope := fact_scope_child(out, fact_scope, n.range)
		add_system_field_update(out, n.range, .Loop_At, "subrc", fact_scope)
		add_system_field_update(out, n.range, .Loop_At, "tabix", fact_scope)
		add_system_field_update(out, n.range, .Loop_At, "tfill", fact_scope)
		add_system_field_update(out, n.range, .Loop_At, "tleng", fact_scope)
		collect_expr_lints(out, n.source, allocator, fact_scope = body_scope)
		collect_expr_lints(out, n.target, allocator, fact_scope = body_scope)
		collect_expr_lints(out, n.target_casting_type, allocator, fact_scope = body_scope)
		collect_expr_lints(out, n.from, allocator, fact_scope = body_scope)
		collect_expr_lints(out, n.to, allocator, fact_scope = body_scope)
		collect_expr_lints(out, n.using_key.dynamic_name, allocator, fact_scope = body_scope)
		collect_expr_lints(out, n.where_cond, allocator, fact_scope = body_scope)
		collect_expr_lints(out, n.group_by, allocator, fact_scope = body_scope)
		collect_expr_lints(out, n.group_target, allocator, fact_scope = body_scope)
		next_loop_kind := loop_kind if loop_depth > 0 else Routine_Loop_Kind.Loop
		collect_flat_stmt_list(out, n.body[:], allocator, policy, loop_depth + 1, guarded_tables, next_loop_kind, body_scope, leave_list_processing_exits)
	case ^ast.At_Stmt:
		body_scope := fact_scope_child(out, fact_scope, n.range)
		collect_expr_lints(out, n.expr, allocator, fact_scope = body_scope)
		collect_flat_stmt_list(out, n.body[:], allocator, policy, loop_depth, guarded_tables, loop_kind, body_scope, leave_list_processing_exits)
	case ^ast.Try_Stmt:
		body_scope := fact_scope_child(out, fact_scope, n.range)
		collect_flat_stmt_list(out, n.body[:], allocator, policy, loop_depth, guarded_tables, loop_kind, body_scope, leave_list_processing_exits)
		for clause in n.catches {
			catch_scope := fact_scope_child(out, body_scope, clause.range)
			collect_flat_stmt_list(out, clause.body[:], allocator, policy, loop_depth, guarded_tables, loop_kind, catch_scope, leave_list_processing_exits)
		}
		if n.cleanup != nil {
			cleanup_scope := fact_scope_child(out, body_scope, n.cleanup.range)
			collect_flat_stmt_list(out, n.cleanup.body[:], allocator, policy, loop_depth, guarded_tables, loop_kind, cleanup_scope, leave_list_processing_exits)
		}
	case ^ast.Class_Decl:
		class_scope := fact_scope_child(out, fact_scope, n.range)
		collect_flat_stmt_list(out, n.body[:], allocator, policy, loop_depth, guarded_tables, loop_kind, class_scope, leave_list_processing_exits)
	case ^ast.Interface_Decl:
		interface_scope := fact_scope_child(out, fact_scope, n.range)
		collect_flat_stmt_list(out, n.body[:], allocator, policy, loop_depth, guarded_tables, loop_kind, interface_scope, leave_list_processing_exits)
	case ^ast.Method_Decl:
		method_scope := fact_scope_child(out, fact_scope, n.range)
		collect_flat_stmt_list(out, n.body[:], allocator, policy, loop_depth, nil, loop_kind, method_scope, false)
	case ^ast.Form_Decl:
		form_scope := fact_scope_child(out, fact_scope, n.range)
		collect_flat_stmt_list(out, n.body[:], allocator, policy, loop_depth, nil, loop_kind, form_scope, false)
	case ^ast.Function_Decl:
		function_scope := fact_scope_child(out, fact_scope, n.range)
		collect_flat_stmt_list(out, n.body[:], allocator, policy, loop_depth, nil, loop_kind, function_scope, false)
	case ^ast.Module_Decl:
		module_scope := fact_scope_child(out, fact_scope, n.range)
		collect_flat_stmt_list(out, n.body[:], allocator, policy, loop_depth, nil, loop_kind, module_scope, false)
	case ^ast.Event_Block_Stmt:
		event_scope := fact_scope_child(out, fact_scope, n.range)
		collect_flat_stmt_list(
			out,
			n.body[:],
			allocator,
			policy,
			loop_depth,
			nil,
			loop_kind,
			event_scope,
			event_block_leave_list_processing_exits(n.kind),
		)
	case ^ast.Select_Stmt:
		add_system_field_update(out, n.range, .Select, "subrc", fact_scope)
		add_system_field_update(out, n.range, .Select, "dbcnt", fact_scope)
		collect_select_order(out, &n.query, n.range, allocator)
		collect_select_lints(out, n, loop_depth, loop_kind, guarded_tables, policy, allocator)
		select_scope := fact_scope_child(out, fact_scope, n.range)
		collect_flat_stmt_list(out, n.body[:], allocator, policy, loop_depth, guarded_tables, loop_kind, select_scope, leave_list_processing_exits)
	case ^ast.Read_Table_Stmt:
		add_system_field_update(out, n.range, .Read_Table, "subrc", fact_scope)
		add_system_field_update(out, n.range, .Read_Table, "tabix", fact_scope)
		collect_read_table_binary_searches(out, n, allocator)
	case ^ast.Sort_Stmt:
		collect_sort_order(out, n, allocator)
	case ^ast.Perform_Stmt:
		collect_perform_call(out, n, allocator)
	case ^ast.Find_Stmt:
		collect_find_site(out, n, allocator, fact_scope)
	case ^ast.Search_Stmt:
		add_system_field_update(out, n.range, .Search, "subrc", fact_scope)
		add_system_field_update(out, n.range, .Search, "fdpos", fact_scope)
	case ^ast.Insert_Stmt:
		add_system_field_update(out, n.range, .Insert_Db_Table if n.form == .Db_Table else .Insert_Table, "subrc", fact_scope)
	case ^ast.Modify_Stmt:
		has_internal := n.table_keyword || n.index != nil || len(n.transporting) > 0
		add_system_field_update(out, n.range, .Modify_Table if has_internal else .Modify_Db_Table, "subrc", fact_scope)
	case ^ast.Update_Stmt:
		add_system_field_update(out, n.range, .Update_Db_Table, "subrc", fact_scope)
	case ^ast.Delete_Stmt:
		add_system_field_update(out, n.range, .Delete_Db_Table if n.form == .Db_Table else .Delete_Table, "subrc", fact_scope)
		add_routine_site(out, n.range, .Delete)
	case ^ast.Append_Stmt:
		add_system_field_update(out, n.range, .Append, "subrc", fact_scope)
	case ^ast.Report_Stmt:
		#partial switch n.kind {
		case .Read_Report:
			add_system_field_update(out, n.range, .Read_Report, "subrc", fact_scope)
		case .Insert_Report:
			add_system_field_update(out, n.range, .Insert_Report, "subrc", fact_scope)
			add_routine_site(out, n.range, .Unknown_Effect)
		case .Delete_Report:
			add_system_field_update(out, n.range, .Delete_Report, "subrc", fact_scope)
			add_routine_site(out, n.range, .Unknown_Effect)
		}
	case ^ast.Textpool_Stmt:
		if n.kind == .Insert {
			add_system_field_update(out, n.range, .Insert_Textpool, "subrc", fact_scope)
			add_routine_site(out, n.range, .Unknown_Effect)
		}
	case ^ast.Clear_Stmt:
		for op in n.operands {
			add_routine_site_target(out, n.range, .Clear, op.target)
		}
	case ^ast.Refresh_Stmt:
		for op in n.operands {
			add_routine_site_target(out, n.range, .Clear, op.target)
		}
	case ^ast.Free_Stmt:
		for op in n.operands {
			add_routine_site_target(out, n.range, .Clear, op.target)
		}
	case ^ast.Unassign_Stmt:
		for op in n.operands {
			add_routine_site_target(out, n.range, .Unassign, op.target)
		}
	case ^ast.Flow_Stmt:
		add_flow_site(out, n)
	case ^ast.Raise_Stmt:
		add_routine_site(out, n.range, .Raise)
	case ^ast.Assign_Field_Stmt:
		add_system_field_update(out, n.range, .Assign, "subrc", fact_scope)
	case ^ast.Authority_Check_Stmt:
		add_system_field_update(out, n.range, .Authority_Check, "subrc", fact_scope)
	case ^ast.Call_Stmt:
		collect_call_stmt_lints(out, n, allocator, fact_scope)
	case ^ast.Write_Stmt:
		for operand in n.operands {
			collect_expr_lints(out, operand.value, allocator, fact_scope = fact_scope)
			collect_expr_lints(out, operand.position, allocator, fact_scope = fact_scope)
			collect_expr_lints(out, operand.length, allocator, fact_scope = fact_scope)
		}
	case ^ast.Write_To_Stmt:
		for entry in n.entries {
			collect_expr_lints(out, entry.source, allocator, fact_scope = fact_scope)
			collect_expr_lints(out, entry.target, allocator, false, fact_scope)
		}
	case ^ast.Enhancement_Stmt:
		collect_flat_stmt_list(out, n.body[:], allocator, policy, loop_depth, guarded_tables, loop_kind, fact_scope, leave_list_processing_exits)
	case ^ast.Enhancement_Section_Stmt:
		collect_flat_stmt_list(out, n.body[:], allocator, policy, loop_depth, guarded_tables, loop_kind, fact_scope, leave_list_processing_exits)
	case ^ast.Test_Seam_Stmt:
		collect_flat_stmt_list(out, n.body[:], allocator, policy, loop_depth, guarded_tables, loop_kind, fact_scope, leave_list_processing_exits)
	case ^ast.Test_Injection_Stmt:
		collect_flat_stmt_list(out, n.body[:], allocator, policy, loop_depth, guarded_tables, loop_kind, fact_scope, leave_list_processing_exits)
	}
}

collect_expr_lints :: proc(
	out: ^Unit_Lints,
	expr: ^ast.Expr,
	allocator: mem.Allocator,
	record_reads: bool = true,
	fact_scope: int = 0,
) {
	if expr == nil {
		return
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Is_Predicate_Expr:
		collect_expr_lints(out, n.subject, allocator, record_reads, fact_scope)
		collect_state_check(out, n, allocator, fact_scope)
	case ^ast.Binary_Expr:
		collect_expr_lints(out, n.left, allocator, record_reads, fact_scope)
		collect_expr_lints(out, n.right, allocator, record_reads, fact_scope)
		collect_zero_state_check(out, n, allocator, fact_scope)
	case ^ast.Unary_Expr:
		collect_expr_lints(out, n.expr, allocator, record_reads, fact_scope)
	case ^ast.Paren_Expr:
		collect_expr_lints(out, n.expr, allocator, record_reads, fact_scope)
	case ^ast.Host_Expr:
		collect_expr_lints(out, n.value, allocator, record_reads, fact_scope)
	case ^ast.Selector_Expr:
		record_value_read(out, expr, allocator, record_reads, fact_scope)
		collect_expr_lints(out, n.base, allocator, record_reads, fact_scope)
		collect_expr_lints(out, n.field, allocator, false, fact_scope)
	case ^ast.Table_Expr:
		collect_expr_lints(out, n.table, allocator, record_reads, fact_scope)
		for selector in n.selectors {
			collect_expr_lints(out, selector, allocator, record_reads, fact_scope)
		}
	case ^ast.Call_Expr:
		collect_expr_lints(out, n.callee, allocator, false, fact_scope)
		collect_expr_lints(out, n.args, allocator, record_reads, fact_scope)
	case ^ast.Call_Arg_List_Expr:
		for arg in n.args {
			collect_expr_lints(out, arg, allocator, record_reads, fact_scope)
		}
	case ^ast.Call_Arg_Section_Expr:
		for arg in n.args {
			collect_expr_lints(out, arg, allocator, record_reads, fact_scope)
		}
	case ^ast.Call_Named_Arg_Expr:
		collect_expr_lints(out, n.value, allocator, record_reads, fact_scope)
	case ^ast.Call_Positional_Arg_Expr:
		collect_expr_lints(out, n.value, allocator, record_reads, fact_scope)
	case ^ast.Constructor_Expr:
		for arg in n.args {
			collect_expr_lints(out, arg, allocator, record_reads, fact_scope)
		}
	case ^ast.Ident_Expr, ^ast.Data_Inline_Name_Expr, ^ast.Field_Symbol_Inline_Name_Expr, ^ast.Type_Ref_Expr:
		record_value_read(out, expr, allocator, record_reads, fact_scope)
	}
}

record_value_read :: proc(
	out: ^Unit_Lints,
	expr: ^ast.Expr,
	allocator: mem.Allocator,
	record_reads: bool,
	fact_scope: int,
) {
	if !record_reads {
		return
	}
	access, ok := value_access_from_expr(expr, allocator)
	if !ok || access.base_name == "" {
		return
	}
	append(
		&out.value_reads,
		Value_Read_Data {
			scope = fact_scope,
			range = access.base_range,
			name = access.base_name,
			entity = lint_entity_for_access(out, access),
		},
	)
}

collect_select_lints :: proc(
	out: ^Unit_Lints,
	stmt: ^ast.Select_Stmt,
	loop_depth: int,
	loop_kind: Routine_Loop_Kind,
	guarded_tables: []Guard_Data,
	policy: ^Policy,
	allocator: mem.Allocator,
) {
	query := &stmt.query
	if metadata, ok := metadata_for(SELECT_STAR); ok {
		for projection in query.projection_clauses {
			if star, star_ok := projection.value.derived_expr.(^ast.Sql_Star_Expr); star_ok {
				message := "Open SQL SELECT * reads all columns; list the required columns explicitly"
				if star.qualifier.text != "" {
					builder := strings.builder_make(context.temp_allocator)
					strings.write_string(&builder, "Open SQL SELECT uses '")
					strings.write_string(&builder, star.qualifier.text)
					strings.write_string(&builder, "~*'; list the required columns explicitly")
					message = strings.to_string(builder)
				}
				emit_diagnostic(out, metadata, projection.range, message, policy, allocator)
			}
		}
		for projection in query.projections {
			if star, star_ok := projection.derived_expr.(^ast.Sql_Star_Expr); star_ok {
				message := "Open SQL SELECT * reads all columns; list the required columns explicitly"
				if star.qualifier.text != "" {
					builder := strings.builder_make(context.temp_allocator)
					strings.write_string(&builder, "Open SQL SELECT uses '")
					strings.write_string(&builder, star.qualifier.text)
					strings.write_string(&builder, "~*'; list the required columns explicitly")
					message = strings.to_string(builder)
				}
				emit_diagnostic(out, metadata, projection.range, message, policy, allocator)
			}
		}
	}
	if loop_depth > 0 {
		if metadata, ok := metadata_for(SELECT_IN_LOOP); ok {
			range := query.from_clause if query.from_clause.end > query.from_clause.start else stmt.range
			builder := strings.builder_make(context.temp_allocator)
			strings.write_string(&builder, "Open SQL SELECT runs inside a ")
			strings.write_string(&builder, loop_kind_string(loop_kind))
			strings.write_string(&builder, " body; prefer bulk selection before the loop")
			emit_diagnostic(
				out,
				metadata,
				range,
				strings.to_string(builder),
				policy,
				allocator,
			)
		}
	}
	collect_select_single_without_full_key_lints(out, query, policy, allocator)
	collect_select_target_shape_lints(out, query, policy, allocator)
	collect_dynamic_open_sql_lints(out, query, policy, allocator)
	collect_for_all_entries_lints(out, query, guarded_tables, policy, allocator)
}

collect_select_target_shape_lints :: proc(
	out: ^Unit_Lints,
	query: ^ast.Select_Query_Clause,
	policy: ^Policy,
	allocator: mem.Allocator,
) {
	if query == nil ||
	   query.result == nil ||
	   query.result.target == nil ||
	   query.result.kind == .None ||
	   query.result.corresponding_fields ||
	   select_query_has_dynamic_source(query) {
		return
	}
	target_structure, target_ok := select_target_structure(out, query.result)
	if !target_ok || target_structure == nil || len(target_structure.fields) == 0 {
		return
	}
	projections, projections_ok := select_projection_target_infos(out, query, context.temp_allocator)
	if !projections_ok || len(projections) == 0 {
		return
	}
	limit := len(projections)
	if len(target_structure.fields) < limit {
		limit = len(target_structure.fields)
	}
	if limit <= 0 {
		return
	}
	name_metadata, name_metadata_ok := metadata_for(SELECT_INTO_FIELD_NAME_MISMATCH)
	length_metadata, length_metadata_ok := metadata_for(SELECT_INTO_FIELD_LENGTH_NARROWING)
	for i in 0 ..< limit {
		projection := projections[i]
		target := target_structure.fields[i]
		if target == nil {
			continue
		}
		if name_metadata_ok &&
		   projection.name != "" &&
		   target.name != "" &&
		   !strings.equal_fold(projection.name, target.name) {
			emit_diagnostic(
				out,
				name_metadata,
				projection.range,
				select_target_name_mismatch_message(projection.name, target.name),
				policy,
				allocator,
			)
		}
		if length_metadata_ok && projection.field != nil {
			source_length, source_ok := entity_backing_length(out, projection.field)
			target_length, target_length_ok := entity_backing_length(out, target)
			if source_ok && target_length_ok && source_length > target_length {
				emit_diagnostic(
					out,
					length_metadata,
					projection.range,
					select_target_length_narrowing_message(
						projection.name,
						source_length,
						target.name,
						target_length,
					),
					policy,
					allocator,
				)
			}
		}
	}
}

select_projection_target_infos :: proc(
	out: ^Unit_Lints,
	query: ^ast.Select_Query_Clause,
	allocator: mem.Allocator,
) -> ([dynamic]Select_Projection_Target_Info, bool) {
	projections := make([dynamic]Select_Projection_Target_Info, 0, 4, allocator)
	if query == nil {
		return projections, false
	}
	if len(query.projection_clauses) > 0 {
		for projection in query.projection_clauses {
			if projection.is_dynamic || projection.value == nil || expr_is_sql_star(projection.value) {
				return projections, false
			}
			name, range := select_projection_output_name(projection.value, projection.alias, projection.range, allocator)
			if name == "" {
				return projections, false
			}
			append(
				&projections,
				Select_Projection_Target_Info {
					name = name,
					range = range,
					field = select_projection_field_entity(out, projection.value),
				},
			)
		}
		return projections, true
	}
	for projection in query.projections {
		if projection == nil || expr_is_sql_star(projection) {
			return projections, false
		}
		name, range := select_projection_output_name(projection, {}, projection.range, allocator)
		if name == "" {
			return projections, false
		}
		append(
			&projections,
			Select_Projection_Target_Info {
				name = name,
				range = range,
				field = select_projection_field_entity(out, projection),
			},
		)
	}
	return projections, len(projections) > 0
}

select_query_has_dynamic_source :: proc(query: ^ast.Select_Query_Clause) -> bool {
	if query == nil {
		return true
	}
	if query.source_clause != nil {
		if query.source_clause.dynamic_source || source_is_dynamic_sql_fragment(query.source_clause.source) {
			return true
		}
		for join in query.source_clause.joins {
			if source_is_dynamic_sql_fragment(join.source) {
				return true
			}
		}
		return false
	}
	return source_is_dynamic_sql_fragment(query.source)
}

select_projection_output_name :: proc(
	expr: ^ast.Expr,
	alias: ast.Token_Text,
	fallback_range: tokenizer.Range,
	allocator: mem.Allocator,
) -> (string, tokenizer.Range) {
	if alias.text != "" {
		return utils.to_lower_ascii(alias.text, allocator), alias.range
	}
	if expr == nil {
		return "", fallback_range
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Sql_Column_Expr:
		return utils.to_lower_ascii(n.name.text, allocator), n.name.range
	case ^ast.Sql_Call_Expr:
		return utils.to_lower_ascii(n.name.text, allocator), n.name.range
	case ^ast.Ident_Expr:
		return utils.to_lower_ascii(n.name, allocator), n.range
	case ^ast.Type_Ref_Expr:
		return utils.to_lower_ascii(n.name.text, allocator), n.name.range
	case ^ast.Selector_Expr:
		if n.op == .Tilde {
			return select_projection_output_name(n.field, {}, expr.range, allocator)
		}
	}
	return "", fallback_range
}

select_projection_field_entity :: proc(out: ^Unit_Lints, expr: ^ast.Expr) -> ^semantic.Entity {
	if out == nil || out.project == nil || out.checker == nil || out.file == nil || expr == nil {
		return nil
	}
	if !expr_is_direct_sql_field(expr) {
		return nil
	}
	query := semantic.semantic_query(out.project, out.checker, out.file)
	ref_query := semantic.semantic_query_refs(query)
	if use := semantic.semantic_ref_use_at_range(ref_query, expr.range); use != nil &&
	   use.entity != nil &&
	   use.entity.kind == .Field {
		return use.entity
	}
	if expr.range.end > expr.range.start {
		if use := semantic.semantic_ref_use_at_offset(ref_query, expr.range.start); use != nil &&
		   use.entity != nil &&
		   use.entity.kind == .Field {
			return use.entity
		}
	}
	return nil
}

expr_is_direct_sql_field :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Sql_Column_Expr:
		return true
	case ^ast.Selector_Expr:
		return n.op == .Tilde
	}
	return false
}

expr_is_sql_star :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	_, ok := expr.derived_expr.(^ast.Sql_Star_Expr)
	return ok
}

select_target_structure :: proc(
	out: ^Unit_Lints,
	result: ^ast.Select_Result_Clause,
) -> (^semantic.Structure, bool) {
	if out == nil || out.checker == nil || result == nil || result.target == nil {
		return nil, false
	}
	typ, type_ok := expr_type_for_exact_node(out, result.target)
	if !type_ok || typ == nil {
		return nil, false
	}
	if result.table {
		if !semantic.checker_type_is_table_like(&out.checker.builtin_context, typ) {
			return nil, false
		}
		row_type := semantic.checker_type_row(&out.checker.builtin_context, typ)
		structure := semantic.checker_type_structure(row_type)
		return structure, structure != nil
	}
	structure := semantic.checker_type_structure(typ)
	return structure, structure != nil
}

expr_type_for_exact_node :: proc(out: ^Unit_Lints, expr: ^ast.Expr) -> (^semantic.Type, bool) {
	if out == nil || out.checker == nil || expr == nil {
		return nil, false
	}
	for record in out.checker.info.expr_infos {
		if record.node == &expr.expr_base &&
		   (out.file == nil || record.file == out.file) {
			return record.info.type, record.info.type != nil
		}
	}
	return nil, false
}

entity_backing_length :: proc(out: ^Unit_Lints, entity: ^semantic.Entity, depth := 0) -> (int, bool) {
	if entity == nil || depth > 16 {
		return 0, false
	}
	if length, ok := decl_info_length(entity.decl_info); ok {
		return length, true
	}
	if entity.kind == .Field {
		if payload, payload_ok := entity.payload.(^semantic.Entity_Field_Payload);
		   payload_ok && payload != nil {
			if backing := type_ref_backing_entity(out, payload.type_ref); backing != nil && backing != entity {
				if length, ok := entity_backing_length(out, backing, depth + 1); ok {
					return length, true
				}
			}
		}
	}
	return type_backing_length(out, entity.type, entity, depth + 1)
}

type_backing_length :: proc(
	out: ^Unit_Lints,
	typ: ^semantic.Type,
	current: ^semantic.Entity,
	depth: int,
) -> (int, bool) {
	if typ == nil || depth > 16 {
		return 0, false
	}
	if typ.entity != nil && typ.entity != current {
		if length, ok := entity_backing_length(out, typ.entity, depth + 1); ok {
			return length, true
		}
	}
	return type_backing_length(out, typ.base, current, depth + 1)
}

type_ref_backing_entity :: proc(
	out: ^Unit_Lints,
	type_ref: semantic.Field_Type_Ref_Data,
) -> ^semantic.Entity {
	if out == nil ||
	   out.project == nil ||
	   out.checker == nil ||
	   out.file == nil ||
	   type_ref.base_name == "" {
		return nil
	}
	query := semantic.semantic_query(out.project, out.checker, out.file)
	ref_query := semantic.semantic_query_refs(query)
	if len(type_ref.field_ranges) > 0 {
		range := type_ref.field_ranges[len(type_ref.field_ranges) - 1]
		if use := semantic.semantic_ref_use_at_range(ref_query, range); use != nil &&
		   use.entity != nil {
			return use.entity
		}
	}
	if type_ref.base_range.end > type_ref.base_range.start {
		if use := semantic.semantic_ref_use_at_range(ref_query, type_ref.base_range); use != nil &&
		   use.entity != nil {
			return use.entity
		}
	}
	return nil
}

decl_info_length :: proc(info: ^semantic.Decl_Info) -> (int, bool) {
	if info == nil {
		return 0, false
	}
	if info.paren_length != nil {
		if length, ok := integer_literal_expr_value(info.paren_length.expr); ok {
			return length, true
		}
	}
	for clause in info.length_clauses {
		if clause.kind != .Length {
			continue
		}
		if length, ok := integer_literal_expr_value(clause.expr); ok {
			return length, true
		}
	}
	return 0, false
}

integer_literal_expr_value :: proc(expr: ^ast.Expr) -> (int, bool) {
	if expr == nil {
		return 0, false
	}
	lit, ok := expr.derived_expr.(^ast.Literal_Expr)
	if !ok || lit.value == "" {
		return 0, false
	}
	parsed, parse_ok := strconv.parse_int(lit.value, 10)
	if !parse_ok {
		return 0, false
	}
	return int(parsed), true
}

select_target_name_mismatch_message :: proc(select_name, target_name: string) -> string {
	builder := strings.builder_make(context.temp_allocator)
	strings.write_string(&builder, "Open SQL SELECT field '")
	strings.write_string(&builder, select_name)
	strings.write_string(&builder, "' is assigned by position to target field '")
	strings.write_string(&builder, target_name)
	strings.write_byte(&builder, '\'')
	return strings.to_string(builder)
}

select_target_length_narrowing_message :: proc(
	select_name: string,
	select_length: int,
	target_name: string,
	target_length: int,
) -> string {
	builder := strings.builder_make(context.temp_allocator)
	strings.write_string(&builder, "Open SQL SELECT field '")
	strings.write_string(&builder, select_name)
	strings.write_string(&builder, "' has backing length ")
	strings.write_int(&builder, select_length)
	strings.write_string(&builder, ", but target field '")
	strings.write_string(&builder, target_name)
	strings.write_string(&builder, "' has length ")
	strings.write_int(&builder, target_length)
	return strings.to_string(builder)
}

loop_kind_string :: proc "contextless" (kind: Routine_Loop_Kind) -> string {
	switch kind {
	case .Do:
		return "DO"
	case .While:
		return "WHILE"
	case .Loop:
		return "LOOP"
	}
	return "LOOP"
}

collect_select_single_without_full_key_lints :: proc(
	out: ^Unit_Lints,
	query: ^ast.Select_Query_Clause,
	policy: ^Policy,
	allocator: mem.Allocator,
) {
	if query == nil || !query.single || query.dynamic_where {
		return
	}
	source_expr, source_name, source_qualifier, ok := select_single_static_source(query, allocator)
	if !ok {
		return
	}
	structure, structure_ok := open_sql_source_structure(out, source_expr)
	if !structure_ok || structure == nil {
		return
	}
	required_keys := make([dynamic]string, 0, len(structure.fields), allocator)
	for field in structure.fields {
		if field == nil {
			continue
		}
		payload, payload_ok := field.payload.(^semantic.Entity_Field_Payload)
		if !payload_ok || payload == nil || !(.Is_Key in payload.flags) || is_client_column_name(field.name) {
			continue
		}
		append(&required_keys, utils.to_lower_ascii(field.name, allocator))
	}
	if len(required_keys) == 0 {
		return
	}
	where_fields := make([dynamic]string, 0, len(required_keys), allocator)
	collect_sql_where_column_names(query.where_cond, source_name, source_qualifier, &where_fields, allocator)
	missing := make([dynamic]string, 0, len(required_keys), allocator)
	for key in required_keys {
		if !guard_list_contains(where_fields[:], key) {
			append(&missing, key)
		}
	}
	if len(missing) == 0 {
		return
	}
	if metadata, metadata_ok := metadata_for(SELECT_SINGLE_WITHOUT_FULL_KEY); metadata_ok {
		builder := strings.builder_make(context.temp_allocator)
		strings.write_string(&builder, "SELECT SINGLE from '")
		strings.write_string(&builder, source_name)
		strings.write_string(&builder, "' does not restrict primary-key field(s) ")
		for field, i in missing {
			if i > 0 {
				strings.write_string(&builder, ", ")
			}
			strings.write_string(&builder, field)
		}
		strings.write_string(&builder, " in the WHERE clause")
		range := query.where_clause
		if range.end <= range.start {
			range = query.from_clause if query.from_clause.end > query.from_clause.start else source_expr.range
		}
		emit_diagnostic(out, metadata, range, strings.to_string(builder), policy, allocator)
	}
}

select_single_static_source :: proc(
	query: ^ast.Select_Query_Clause,
	allocator: mem.Allocator,
) -> (^ast.Expr, string, string, bool) {
	if query.source_clause != nil {
		if query.source_clause.dynamic_source ||
		   query.source_clause.source == nil ||
		   len(query.source_clause.joins) > 0 ||
		   source_is_dynamic_sql_fragment(query.source_clause.source) {
			return nil, "", "", false
		}
		name, _, name_ok := expr_name(query.source_clause.source)
		if !name_ok {
			return nil, "", "", false
		}
		source_name := utils.to_lower_ascii(name, allocator)
		qualifier := source_name
		if query.source_clause.alias.text != "" {
			qualifier = utils.to_lower_ascii(query.source_clause.alias.text, allocator)
		}
		return query.source_clause.source, source_name, qualifier, source_name != ""
	}
	if query.source == nil || source_is_dynamic_sql_fragment(query.source) {
		return nil, "", "", false
	}
	name, _, name_ok := expr_name(query.source)
	if !name_ok {
		return nil, "", "", false
	}
	source_name := utils.to_lower_ascii(name, allocator)
	return query.source, source_name, source_name, source_name != ""
}

open_sql_source_structure :: proc(out: ^Unit_Lints, source_expr: ^ast.Expr) -> (^semantic.Structure, bool) {
	if out == nil || out.project == nil || out.checker == nil || out.file == nil || source_expr == nil {
		return nil, false
	}
	query := semantic.semantic_query(out.project, out.checker, out.file)
	ref_query := semantic.semantic_query_refs(query)
	use := semantic.semantic_ref_use_at_range(ref_query, source_expr.range)
	if use == nil || use.entity == nil || use.entity.type == nil {
		return nil, false
	}
	structure := semantic.checker_type_structure(use.entity.type)
	return structure, structure != nil
}

collect_sql_where_column_names :: proc(
	expr: ^ast.Expr,
	source_name: string,
	source_qualifier: string,
	out: ^[dynamic]string,
	allocator: mem.Allocator,
) {
	if expr == nil {
		return
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Sql_Column_Expr:
		if sql_column_matches_source(n, source_name, source_qualifier) {
			name := utils.to_lower_ascii(n.name.text, allocator)
			if name != "" && !guard_list_contains(out[:], name) {
				append(out, name)
			}
		}
	case ^ast.Binary_Expr:
		collect_sql_where_column_names(n.left, source_name, source_qualifier, out, allocator)
		collect_sql_where_column_names(n.right, source_name, source_qualifier, out, allocator)
	case ^ast.Unary_Expr:
		collect_sql_where_column_names(n.expr, source_name, source_qualifier, out, allocator)
	case ^ast.Paren_Expr:
		collect_sql_where_column_names(n.expr, source_name, source_qualifier, out, allocator)
	case ^ast.Sql_Call_Expr:
		for arg in n.args {
			collect_sql_where_column_names(arg, source_name, source_qualifier, out, allocator)
		}
	case ^ast.Host_Expr:
		return
	}
}

sql_column_matches_source :: proc(
	column: ^ast.Sql_Column_Expr,
	source_name: string,
	source_qualifier: string,
) -> bool {
	if column == nil {
		return false
	}
	if column.qualifier.text == "" {
		return true
	}
	return strings.equal_fold(column.qualifier.text, source_name) ||
	       strings.equal_fold(column.qualifier.text, source_qualifier)
}

is_client_column_name :: proc(name: string) -> bool {
	return strings.equal_fold(name, "mandt") || strings.equal_fold(name, "client")
}

collect_dynamic_open_sql_lints :: proc(
	out: ^Unit_Lints,
	query: ^ast.Select_Query_Clause,
	policy: ^Policy,
	allocator: mem.Allocator,
) {
	metadata, ok := metadata_for(DYNAMIC_OPEN_SQL)
	if !ok {
		return
	}
	if query.source_clause != nil {
		if query.source_clause.dynamic_source && query.source_clause.source != nil {
			emit_diagnostic(
				out,
				metadata,
				query.source_clause.source.range,
				"Open SQL uses a dynamic source fragment that cannot be statically verified",
				policy,
				allocator,
			)
		}
		for join in query.source_clause.joins {
			if source_is_dynamic_sql_fragment(join.source) {
				emit_diagnostic(
					out,
					metadata,
					join.source.range,
					"Open SQL uses a dynamic source fragment that cannot be statically verified",
					policy,
					allocator,
				)
			}
		}
	}
	for projection in query.projection_clauses {
		if projection.is_dynamic {
			emit_diagnostic(
				out,
				metadata,
				projection.range,
				"Open SQL uses a dynamic projection fragment that cannot be statically verified",
				policy,
				allocator,
			)
		}
	}
	if query.dynamic_where && query.where_cond != nil {
		emit_diagnostic(
			out,
			metadata,
			query.where_cond.range,
			"Open SQL uses a dynamic WHERE fragment that cannot be statically verified",
			policy,
			allocator,
		)
	}
}

collect_for_all_entries_lints :: proc(
	out: ^Unit_Lints,
	query: ^ast.Select_Query_Clause,
	guarded_tables: []Guard_Data,
	policy: ^Policy,
	allocator: mem.Allocator,
) {
	if query.for_all_entries == nil {
		return
	}
	access, access_ok := value_access_from_expr(query.for_all_entries, allocator)
	if !access_ok {
		return
	}
	entity := lint_entity_for_access(out, access)
	if guard_data_list_contains(guarded_tables, access.base_name, entity) {
		return
	}
	if metadata, ok := metadata_for(FOR_ALL_ENTRIES_WITHOUT_GUARD); ok {
		builder := strings.builder_make(context.temp_allocator)
		strings.write_string(&builder, "FOR ALL ENTRIES on '")
		strings.write_string(&builder, access.base_name)
		strings.write_string(&builder, "' is not guarded by an initial-table check")
		range := query.for_all_entries_clause
		if range.end <= range.start {
			range = query.for_all_entries.range
		}
		emit_diagnostic(out, metadata, range, strings.to_string(builder), policy, allocator)
	}
}

collect_call_stmt_lints :: proc(out: ^Unit_Lints, stmt: ^ast.Call_Stmt, allocator: mem.Allocator, fact_scope: int) {
	for arg in stmt.named_args {
		collect_expr_lints(
			out,
			arg.value,
			allocator,
			!(stmt.kind == .Function && (arg.section == .Importing || arg.section == .Tables)),
			fact_scope,
		)
		collect_expr_lints(out, arg.message, allocator, fact_scope = fact_scope)
	}
	collect_expr_lints(out, stmt.call, allocator, fact_scope = fact_scope)
	collect_expr_lints(out, stmt.target, allocator, fact_scope = fact_scope)
	collect_expr_lints(out, stmt.function_destination, allocator, fact_scope = fact_scope)
	collect_expr_lints(out, stmt.function_task, allocator, fact_scope = fact_scope)
	collect_expr_lints(out, stmt.function_end_task_handler, allocator, fact_scope = fact_scope)
	collect_expr_lints(out, stmt.function_parameter_table, allocator, fact_scope = fact_scope)
	collect_expr_lints(out, stmt.function_exception_table, allocator, fact_scope = fact_scope)
	if stmt.kind == .Function {
		add_system_field_update(out, stmt.range, .Call_Function, "subrc", fact_scope)
		result := Call_Function_Result_Data {
			scope = fact_scope,
			range = stmt.range,
			output_targets = make([dynamic]Call_Function_Output_Target_Data, 0, 2, allocator),
		}
		for arg in stmt.named_args {
			if arg.section == .Exceptions {
				if exception_mapping_value_is_nonzero_literal(arg.value) {
					result.has_exception_mapping = true
				}
			}
			if arg.section == .Changing {
				result.has_changing_argument = true
			}
			if arg.section == .Importing || arg.section == .Tables {
				if access, access_ok := value_access_from_expr(arg.value, allocator); access_ok {
					append(
						&result.output_targets,
						Call_Function_Output_Target_Data {
							range = access.base_range,
							name = access.base_name,
							entity = lint_entity_for_access(out, access),
						},
					)
				} else if arg.value != nil {
					append(&result.output_targets, Call_Function_Output_Target_Data{range = arg.value.range})
				}
			}
		}
		append(&out.call_function_results, result)
	}
}

exception_mapping_value_is_nonzero_literal :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	if lit, ok := expr.derived_expr.(^ast.Literal_Expr); ok {
		value := strings.trim_space(lit.value)
		if value == "" {
			return false
		}
		has_nonzero := false
		for i in 0 ..< len(value) {
			if value[i] < '0' || value[i] > '9' {
				return false
			}
			if value[i] != '0' {
				has_nonzero = true
			}
		}
		return has_nonzero
	}
	return false
}


collect_control_facts :: proc(
	out: ^Unit_Lints,
	stmts: []^ast.Stmt,
	loop_depth: int,
	policy: ^Policy = nil,
	allocator: mem.Allocator = context.allocator,
) {
	_ = policy
	_ = allocator
	for stmt in stmts {
		if stmt == nil {
			continue
		}
		#partial switch n in stmt.derived_stmt {
		case ^ast.If_Stmt:
			append(&out.routine_control_regions, Routine_Control_Region_Data{kind = .If, range = n.range})
			collect_control_facts(out, n.body[:], loop_depth, policy, allocator)
			for clause in n.elseif_clauses {
				collect_control_facts(out, clause.body[:], loop_depth, policy, allocator)
			}
			if n.else_clause != nil {
				collect_control_facts(out, n.else_clause.body[:], loop_depth, policy, allocator)
			}
		case ^ast.Case_Stmt:
			append(&out.routine_control_regions, Routine_Control_Region_Data{kind = .Case, range = n.range})
			collect_control_facts(out, n.recovery[:], loop_depth, policy, allocator)
			for clause in n.whens {
				collect_control_facts(out, clause.body[:], loop_depth, policy, allocator)
			}
		case ^ast.While_Stmt:
			append(&out.routine_control_regions, Routine_Control_Region_Data{kind = .Loop, range = n.range, loop_kind = .While})
			add_system_field_update(out, n.range, .While, "index")
			collect_control_facts(out, n.body[:], loop_depth + 1, policy, allocator)
		case ^ast.Do_Stmt:
			append(&out.routine_control_regions, Routine_Control_Region_Data{kind = .Loop, range = n.range, loop_kind = .Do})
			add_system_field_update(out, n.range, .Do, "index")
			collect_control_facts(out, n.body[:], loop_depth + 1, policy, allocator)
		case ^ast.Loop_Stmt:
			append(&out.routine_control_regions, Routine_Control_Region_Data{kind = .Loop, range = n.range, loop_kind = .Loop})
			add_system_field_update(out, n.range, .Loop_At, "subrc")
			add_system_field_update(out, n.range, .Loop_At, "tabix")
			add_system_field_update(out, n.range, .Loop_At, "tfill")
			add_system_field_update(out, n.range, .Loop_At, "tleng")
			collect_control_facts(out, n.body[:], loop_depth + 1, policy, allocator)
		case ^ast.At_Stmt:
			kind := at_group_kind(n.kind)
			append(&out.routine_control_regions, Routine_Control_Region_Data{kind = .At, range = n.range, at_kind = kind})
			collect_control_facts(out, n.body[:], loop_depth, policy, allocator)
		case ^ast.Try_Stmt:
			append(&out.routine_control_regions, Routine_Control_Region_Data{kind = .Try, range = n.range})
			collect_control_facts(out, n.body[:], loop_depth, policy, allocator)
			for clause in n.catches {
				collect_control_facts(out, clause.body[:], loop_depth, policy, allocator)
			}
			if n.cleanup != nil {
				collect_control_facts(out, n.cleanup.body[:], loop_depth, policy, allocator)
			}
		case ^ast.Class_Decl:
			collect_control_facts(out, n.body[:], loop_depth, policy, allocator)
		case ^ast.Interface_Decl:
			collect_control_facts(out, n.body[:], loop_depth, policy, allocator)
		case ^ast.Method_Decl:
			collect_control_facts(out, n.body[:], loop_depth, policy, allocator)
		case ^ast.Form_Decl:
			collect_control_facts(out, n.body[:], loop_depth, policy, allocator)
		case ^ast.Function_Decl:
			collect_control_facts(out, n.body[:], loop_depth, policy, allocator)
		case ^ast.Module_Decl:
			collect_control_facts(out, n.body[:], loop_depth, policy, allocator)
		case ^ast.Event_Block_Stmt:
			collect_control_facts(out, n.body[:], loop_depth, policy, allocator)
		case ^ast.Enhancement_Stmt:
			collect_control_facts(out, n.body[:], loop_depth, policy, allocator)
		case ^ast.Enhancement_Section_Stmt:
			collect_control_facts(out, n.body[:], loop_depth, policy, allocator)
		case ^ast.Test_Seam_Stmt:
			collect_control_facts(out, n.body[:], loop_depth, policy, allocator)
		case ^ast.Test_Injection_Stmt:
			collect_control_facts(out, n.body[:], loop_depth, policy, allocator)
		case ^ast.Select_Stmt:
			collect_control_facts(out, n.body[:], loop_depth, policy, allocator)
		}
	}
}

at_group_kind :: proc(kind: ast.At_Stmt_Kind) -> At_Group_Kind {
	#partial switch kind {
	case .First:
		return .First
	case .Last:
		return .Last
	case .New:
		return .New
	case .End_Of:
		return .End_Of
	}
	return .First
}

collect_state_check :: proc(out: ^Unit_Lints, expr: ^ast.Is_Predicate_Expr, allocator: mem.Allocator, fact_scope: int) {
	access, ok := value_access_from_expr(expr.subject, allocator)
	if !ok {
		return
	}
	if expr.kind == .Assigned {
		append(
			&out.field_symbol_state_checks,
			Field_Symbol_State_Check_Data {
				range = expr.range,
				symbol_name = access.base_name,
				symbol_range = access.base_range,
				kind = .Is_Not_Assigned if expr.negated else .Is_Assigned,
			},
		)
		return
	}
	if expr.kind == .Initial || expr.kind == .Bound {
		check_kind := Value_State_Check_Kind.Is_Initial
		if (expr.kind == .Initial && expr.negated) || (expr.kind == .Bound && !expr.negated) {
			check_kind = .Is_Not_Initial
		}
		append(
			&out.value_state_checks,
			Value_State_Check_Data {
				scope = fact_scope,
				range = expr.range,
				symbol_name = access.base_name,
				symbol_range = access.base_range,
				field_name = last_field(access),
				kind = check_kind,
			},
		)
	}
}

collect_zero_state_check :: proc(out: ^Unit_Lints, expr: ^ast.Binary_Expr, allocator: mem.Allocator, fact_scope: int) {
	if expr.op != .Equal && expr.op != .Not_Equal {
		return
	}
	if !expr_is_zero_literal(expr.right) {
		return
	}
	access, ok := value_access_from_expr(expr.left, allocator)
	if !ok {
		return
	}
	append(
		&out.value_state_checks,
		Value_State_Check_Data {
			scope = fact_scope,
			range = expr.range,
			symbol_name = access.base_name,
			symbol_range = access.base_range,
			field_name = last_field(access),
			kind = .Equals_Zero if expr.op == .Equal else .Not_Equals_Zero,
		},
	)
}

expr_is_zero_literal :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	if lit, ok := expr.derived_expr.(^ast.Literal_Expr); ok {
		return lit.value == "0"
	}
	return false
}

add_value_flow :: proc(out: ^Unit_Lints, lhs, rhs: ^ast.Expr) {
	if lhs == nil || rhs == nil {
		return
	}
	append(
		&out.value_flow_edges,
		Value_Flow_Edge_Data {
			kind = .Assignment,
			source_range = rhs.range,
			target = Value_Flow_Target_Data{kind = .Assignment, range = lhs.range},
		},
	)
}

collect_read_table_binary_searches :: proc(out: ^Unit_Lints, stmt: ^ast.Read_Table_Stmt, allocator: mem.Allocator) {
	for entry in stmt.entries {
		if !entry.binary_search || entry.table == nil {
			continue
		}
		keys := make([dynamic]string, 0, len(entry.key_values), allocator)
		for key in entry.key_values {
			append(&keys, utils.to_lower_ascii(key.name.text, allocator))
		}
		name := ""
		if access, ok := value_access_from_expr(entry.table, allocator); ok {
			name = table_order_name_from_access(access, allocator)
		}
		append(
			&out.read_table_binary_searches,
			Read_Table_Binary_Search_Data {
				range = entry.binary_search_clause,
				table_name = utils.to_lower_ascii(name, allocator),
				key_fields = keys,
			},
		)
	}
}

collect_sort_order :: proc(out: ^Unit_Lints, stmt: ^ast.Sort_Stmt, allocator: mem.Allocator) {
	if stmt.target == nil || len(stmt.fields) == 0 || stmt.descending {
		return
	}
	access, ok := value_access_from_expr(stmt.target, allocator)
	if !ok {
		return
	}
	keys := make([dynamic]string, 0, len(stmt.fields), allocator)
	for field in stmt.fields {
		if field.name.text == "" || field.descending {
			return
		}
		append(&keys, utils.to_lower_ascii(field.name.text, allocator))
	}
	append(
		&out.internal_table_orders,
		Internal_Table_Order_Data {
			range = stmt.range,
			table_name = table_order_name_from_access(access, allocator),
			key_fields = keys,
		},
	)
}

collect_select_order :: proc(
	out: ^Unit_Lints,
	query: ^ast.Select_Query_Clause,
	range: tokenizer.Range,
	allocator: mem.Allocator,
) {
	if query == nil ||
	   query.result == nil ||
	   !query.result.table ||
	   len(query.order_by_fields) == 0 ||
	   query.order_by_has_descending {
		return
	}
	access, ok := value_access_from_expr(query.result.target, allocator)
	if !ok {
		return
	}
	keys := make([dynamic]string, 0, len(query.order_by_fields), allocator)
	for field in query.order_by_fields {
		append(&keys, utils.to_lower_ascii(field.text, allocator))
	}
	append(
		&out.internal_table_orders,
		Internal_Table_Order_Data {
			range = range,
			table_name = table_order_name_from_access(access, allocator),
			key_fields = keys,
		},
	)
}

collect_perform_call :: proc(out: ^Unit_Lints, stmt: ^ast.Perform_Stmt, allocator: mem.Allocator) {
	routine_name := ""
	routine_range := tokenizer.Range{}
	if name, range, ok := expr_name(stmt.form); ok && stmt.form_kind == .Static {
		routine_name = utils.to_lower_ascii(name, allocator)
		routine_range = range
	}
	program := Perform_Program_Data{}
	flags := Perform_Call_Flags{}
	if stmt.form_kind == .Dynamic {
		flags += {.Is_Dynamic}
	}
	if stmt.program != nil {
		flags += {.Has_Program}
		if name, range, ok := static_perform_program_name(stmt, allocator); ok {
			program = Perform_Program_Data {
				name = name,
				range = range,
				is_dynamic = stmt.program_kind == .Dynamic,
			}
		} else {
			program = Perform_Program_Data {
				name = "<dynamic>",
				range = stmt.program.range,
				is_dynamic = true,
			}
		}
	} else if stmt.program_kind == .Omitted {
		flags += {.Has_Program}
	}
	if stmt.if_found {
		flags += {.Has_If_Found}
	}
	parameters := make(
		[dynamic]Perform_Parameter_Section,
		0,
		len(stmt.tables) + len(stmt.using_args) + len(stmt.changing),
		allocator,
	)
	arguments := make([dynamic]Perform_Argument_Data, 0, cap(parameters), allocator)
	append_perform_args(&parameters, &arguments, stmt.tables[:], .Tables)
	append_perform_args(&parameters, &arguments, stmt.using_args[:], .Using)
	append_perform_args(&parameters, &arguments, stmt.changing[:], .Changing)
	append(
		&out.perform_calls,
		Perform_Call_Data {
			range = stmt.range,
			routine_name = routine_name,
			routine_range = routine_range,
			program = program,
			parameters = parameters,
			arguments = arguments,
			flags = flags,
		},
	)
}

static_perform_program_name :: proc(stmt: ^ast.Perform_Stmt, allocator: mem.Allocator) -> (string, tokenizer.Range, bool) {
	if stmt.program == nil {
		return "", tokenizer.Range{}, false
	}
	if stmt.program_kind == .Static {
		if name, range, ok := expr_name(stmt.program); ok {
			return utils.to_lower_ascii(strip_quotes(name), allocator), range, true
		}
	}
	if stmt.program_kind == .Dynamic {
		if paren, ok := stmt.program.derived_expr.(^ast.Paren_Expr); ok {
			if name, range, name_ok := expr_name(paren.expr); name_ok {
				if _, lit_ok := paren.expr.derived_expr.(^ast.Literal_Expr); lit_ok {
					return utils.to_lower_ascii(strip_quotes(name), allocator), range, true
				}
			}
		}
	}
	return "", tokenizer.Range{}, false
}

append_perform_args :: proc(
	parameters: ^[dynamic]Perform_Parameter_Section,
	arguments: ^[dynamic]Perform_Argument_Data,
	values: []^ast.Expr,
	section: Perform_Parameter_Section,
) {
	for value, i in values {
		if value == nil {
			continue
		}
		append(parameters, section)
		append(arguments, Perform_Argument_Data{range = value.range, section = section, ordinal_in_section = i})
	}
}

collect_find_site :: proc(out: ^Unit_Lints, stmt: ^ast.Find_Stmt, allocator: mem.Allocator, fact_scope: int) {
	add_system_field_update(out, stmt.range, .Find, "subrc", fact_scope)
	add_system_field_update(out, stmt.range, .Find, "fdpos", fact_scope)
	read_ranges := make([dynamic]tokenizer.Range, 0, 4, allocator)
	write_targets := make([dynamic]Find_Write_Target_Data, 0, 5, allocator)
	if stmt.pattern != nil {append(&read_ranges, stmt.pattern.range)}
	if stmt.target != nil {append(&read_ranges, stmt.target.range)}
	if stmt.section_offset != nil {append(&read_ranges, stmt.section_offset.range)}
	if stmt.section_length != nil {append(&read_ranges, stmt.section_length.range)}
	if stmt.match_offset != nil {append(&write_targets, Find_Write_Target_Data{range = stmt.match_offset.range, definitely_assigned = true})}
	if stmt.match_length != nil {append(&write_targets, Find_Write_Target_Data{range = stmt.match_length.range, definitely_assigned = true})}
	if stmt.match_line != nil {append(&write_targets, Find_Write_Target_Data{range = stmt.match_line.range, definitely_assigned = true})}
	if stmt.match_count != nil {append(&write_targets, Find_Write_Target_Data{range = stmt.match_count.range, definitely_assigned = true})}
	for submatch in stmt.submatches {
		if submatch != nil {
			append(&write_targets, Find_Write_Target_Data{range = submatch.range, definitely_assigned = true})
		}
	}
	if stmt.results != nil {
		append(
			&write_targets,
			Find_Write_Target_Data {
				range = stmt.results.range,
				definitely_assigned = stmt.occurrence == .All,
			},
		)
	}
	append(&out.find_sites, Find_Site_Data{range = stmt.range, read_ranges = read_ranges, write_targets = write_targets})
}

add_flow_site :: proc(out: ^Unit_Lints, stmt: ^ast.Flow_Stmt) {
	kind := Routine_Site_Kind.Unknown_Effect
	#partial switch stmt.kind {
	case .Return:
		kind = .Return
	case .Exit:
		kind = .Exit
	case .Continue:
		kind = .Continue
	case .Stop:
		kind = .Stop
	case .Leave_List_Processing:
		kind = .Leave_List_Processing
	}
	add_routine_site(out, stmt.range, kind)
}

add_routine_site :: proc(out: ^Unit_Lints, range: tokenizer.Range, kind: Routine_Site_Kind) {
	append(&out.routine_sites, Routine_Site_Data{range = range, kind = kind})
}

add_routine_site_target :: proc(out: ^Unit_Lints, range: tokenizer.Range, kind: Routine_Site_Kind, target: ^ast.Expr) {
	site := Routine_Site_Data{range = range, kind = kind}
	if target != nil {
		site.target_range = target.range
		site.has_target = true
	}
	append(&out.routine_sites, site)
}

add_system_field_update :: proc(
	out: ^Unit_Lints,
	range: tokenizer.Range,
	statement: System_Field_Statement_Kind,
	field_name: string,
	scope: int = 0,
) {
	append(&out.system_field_updates, System_Field_Update_Data{scope = scope, range = range, statement = statement, field_name = field_name})
}
