package abap_frontend_lints

import "src:ast"
import execution "src:execution"
import "src:parser"
import analyze "src:semantic/analyze"
import "src:tokenizer"

import "core:mem"
import "core:mem/virtual"
import "core:strings"

Value_Flow_Kind :: enum {
	Assignment,
	Call_Argument,
	Field_Symbol_Assignment,
	Conditional_Field_Symbol_Assignment,
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
	scope:        analyze.Scope_Id,
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
	range:      tokenizer.Range,
	statement:  System_Field_Statement_Kind,
	field_name: string,
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
	source:                      string,
	diagnostics:                 [dynamic]analyze.Diagnostic,
	value_flow_edges:            [dynamic]Value_Flow_Edge_Data,
	perform_calls:               [dynamic]Perform_Call_Data,
	find_sites:                  [dynamic]Find_Site_Data,
	system_field_updates:        [dynamic]System_Field_Update_Data,
	routine_sites:               [dynamic]Routine_Site_Data,
	internal_table_orders:       [dynamic]Internal_Table_Order_Data,
	read_table_binary_searches:  [dynamic]Read_Table_Binary_Search_Data,
	field_symbol_state_checks:   [dynamic]Field_Symbol_State_Check_Data,
	value_state_checks:          [dynamic]Value_State_Check_Data,
	routine_control_regions:     [dynamic]Routine_Control_Region_Data,
}

Lint_Task_State :: struct {
	project:     ^analyze.Project_Analysis,
	sources:     []analyze.Source_Input,
	results:     []Unit_Lints,
	allocators:  []mem.Allocator,
}

Lint_Task_Payload :: struct {
	state:      ^Lint_Task_State,
	source_file_index: int,
}

run_project_async :: proc(
	project: ^analyze.Project_Analysis,
	sources: []analyze.Source_Input,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) {
	_ = allocator
	assert(pool != nil)
	if len(project.providers.source_files) == 0 {
		return
	}
	arenas := make([]virtual.Arena, len(project.providers.source_files), context.temp_allocator)
	allocators := make([]mem.Allocator, len(project.providers.source_files), context.temp_allocator)
	for i in 0 ..< len(project.providers.source_files) {
		_ = virtual.arena_init_growing(&arenas[i])
		allocators[i] = virtual.arena_allocator(&arenas[i])
	}
	defer {
		for i in 0 ..< len(arenas) {
			virtual.arena_destroy(&arenas[i])
		}
	}

	results := make([]Unit_Lints, len(project.providers.source_files), context.temp_allocator)
	state := Lint_Task_State {
		project = project,
		sources = sources,
		results = results,
		allocators = allocators,
	}
	graph: execution.Graph
	execution.graph_init(&graph, pool, context.temp_allocator)
	defer execution.graph_destroy(&graph)

	tasks := make([dynamic]execution.Task(execution.No_Result), 0, len(project.providers.source_files), context.temp_allocator)
	for source_file_index in 0 ..< len(project.providers.source_files) {
		task := execution.submit_value(
			&graph,
			execution.worker_executor(pool),
			Lint_Task_Payload{state = &state, source_file_index = source_file_index},
			lint_task,
		)
		append(&tasks, task)
	}
	execution.graph_start(&graph)
	for task in tasks {
		_ = execution.wait(task)
	}
	execution.graph_wait(&graph)

	for lints, i in results {
		for diagnostic in lints.diagnostics {
			append(&project.providers.source_files[i].diagnostics, diagnostic)
		}
	}
	rebuild_project_diagnostics(project)
}

lint_task :: proc(payload: Lint_Task_Payload) -> execution.No_Result {
	unit := &payload.state.project.providers.source_files[payload.source_file_index]
	source := lint_source_for_uri(payload.state.sources, unit.uri)
	payload.state.results[payload.source_file_index] = unit_lints_make(
		unit.uri,
		source,
		payload.state.allocators[payload.source_file_index],
	)
	if source != "" {
		payload.state.results[payload.source_file_index] = collect_source(
			unit.uri,
			source,
			payload.state.allocators[payload.source_file_index],
		)
	}
	return execution.No_Result{}
}

lint_source_for_uri :: proc(sources: []analyze.Source_Input, uri: string) -> string {
	for source in sources {
		if source.uri == uri {
			return source.source
		}
	}
	return ""
}

collect_source :: proc(uri, source: string, allocator: mem.Allocator) -> Unit_Lints {
	parsed := parser.parse(source, uri, allocator)
	out := unit_lints_make(uri, source, allocator)
	if parsed.root == nil {
		return out
	}
	collect_flat_stmt_list(&out, parsed.root.stmts[:], allocator)
	collect_control_facts(&out, parsed.root.stmts[:], 0)
	return out
}

unit_lints_make :: proc(uri, source: string, allocator: mem.Allocator) -> Unit_Lints {
	return Unit_Lints {
		uri = uri,
		source = source,
		diagnostics = make([dynamic]analyze.Diagnostic, 0, 2, allocator),
		value_flow_edges = make([dynamic]Value_Flow_Edge_Data, 0, 4, allocator),
		perform_calls = make([dynamic]Perform_Call_Data, 0, 1, allocator),
		find_sites = make([dynamic]Find_Site_Data, 0, 1, allocator),
		system_field_updates = make([dynamic]System_Field_Update_Data, 0, 4, allocator),
		routine_sites = make([dynamic]Routine_Site_Data, 0, 4, allocator),
		internal_table_orders = make([dynamic]Internal_Table_Order_Data, 0, 1, allocator),
		read_table_binary_searches = make([dynamic]Read_Table_Binary_Search_Data, 0, 1, allocator),
		field_symbol_state_checks = make([dynamic]Field_Symbol_State_Check_Data, 0, 1, allocator),
		value_state_checks = make([dynamic]Value_State_Check_Data, 0, 1, allocator),
		routine_control_regions = make([dynamic]Routine_Control_Region_Data, 0, 4, allocator),
	}
}

collect_flat_stmt_list :: proc(out: ^Unit_Lints, stmts: []^ast.Stmt, allocator: mem.Allocator) {
	for stmt in stmts {
		if stmt == nil {
			continue
		}
		collect_flat_stmt(out, stmt, allocator)
	}
}

collect_flat_stmt :: proc(out: ^Unit_Lints, stmt: ^ast.Stmt, allocator: mem.Allocator) {
	#partial switch n in stmt.derived_stmt {
	case ^ast.Assign_Stmt:
		add_value_flow(out, n.lhs, n.rhs)
		collect_expr_lints(out, n.lhs, allocator)
		collect_expr_lints(out, n.rhs, allocator)
	case ^ast.Downcast_Assign_Stmt:
		add_value_flow(out, n.lhs, n.rhs)
		collect_expr_lints(out, n.lhs, allocator)
		collect_expr_lints(out, n.rhs, allocator)
	case ^ast.Expr_Stmt:
		collect_expr_lints(out, n.expr, allocator)
	case ^ast.If_Stmt:
		collect_expr_lints(out, n.condition, allocator)
		collect_flat_stmt_list(out, n.body[:], allocator)
		for clause in n.elseif_clauses {
			collect_expr_lints(out, clause.condition, allocator)
			collect_flat_stmt_list(out, clause.body[:], allocator)
		}
		if n.else_clause != nil {
			collect_flat_stmt_list(out, n.else_clause.body[:], allocator)
		}
	case ^ast.Case_Stmt:
		collect_expr_lints(out, n.expr, allocator)
		collect_flat_stmt_list(out, n.recovery[:], allocator)
		for clause in n.whens {
			for operand in clause.operands {
				collect_expr_lints(out, operand, allocator)
			}
			collect_flat_stmt_list(out, clause.body[:], allocator)
		}
	case ^ast.While_Stmt:
		collect_expr_lints(out, n.condition, allocator)
		collect_flat_stmt_list(out, n.body[:], allocator)
	case ^ast.Do_Stmt:
		collect_expr_lints(out, n.count, allocator)
		collect_flat_stmt_list(out, n.body[:], allocator)
	case ^ast.Loop_Stmt:
		add_system_field_update(out, n.range, .Loop_At, "subrc")
		add_system_field_update(out, n.range, .Loop_At, "tabix")
		add_system_field_update(out, n.range, .Loop_At, "tfill")
		add_system_field_update(out, n.range, .Loop_At, "tleng")
		collect_expr_lints(out, n.source, allocator)
		collect_expr_lints(out, n.target, allocator)
		collect_expr_lints(out, n.where_cond, allocator)
		collect_flat_stmt_list(out, n.body[:], allocator)
	case ^ast.At_Stmt:
		collect_expr_lints(out, n.expr, allocator)
		collect_flat_stmt_list(out, n.body[:], allocator)
	case ^ast.Try_Stmt:
		collect_flat_stmt_list(out, n.body[:], allocator)
		for clause in n.catches {
			collect_flat_stmt_list(out, clause.body[:], allocator)
		}
		if n.cleanup != nil {
			collect_flat_stmt_list(out, n.cleanup.body[:], allocator)
		}
	case ^ast.Class_Decl:
		collect_flat_stmt_list(out, n.body[:], allocator)
	case ^ast.Interface_Decl:
		collect_flat_stmt_list(out, n.body[:], allocator)
	case ^ast.Method_Decl:
		collect_flat_stmt_list(out, n.body[:], allocator)
	case ^ast.Form_Decl:
		collect_flat_stmt_list(out, n.body[:], allocator)
	case ^ast.Function_Decl:
		collect_flat_stmt_list(out, n.body[:], allocator)
	case ^ast.Module_Decl:
		collect_flat_stmt_list(out, n.body[:], allocator)
	case ^ast.Event_Block_Stmt:
		collect_flat_stmt_list(out, n.body[:], allocator)
	case ^ast.Select_Stmt:
		add_system_field_update(out, n.range, .Select, "subrc")
		add_system_field_update(out, n.range, .Select, "dbcnt")
		collect_select_order(out, &n.query, n.range, allocator)
		collect_flat_stmt_list(out, n.body[:], allocator)
	case ^ast.Read_Table_Stmt:
		add_system_field_update(out, n.range, .Read_Table, "subrc")
		add_system_field_update(out, n.range, .Read_Table, "tabix")
		collect_read_table_binary_searches(out, n, allocator)
	case ^ast.Sort_Stmt:
		collect_sort_order(out, n, allocator)
	case ^ast.Perform_Stmt:
		collect_perform_call(out, n, allocator)
	case ^ast.Find_Stmt:
		collect_find_site(out, n, allocator)
	case ^ast.Search_Stmt:
		add_system_field_update(out, n.range, .Search, "subrc")
		add_system_field_update(out, n.range, .Search, "fdpos")
	case ^ast.Insert_Stmt:
		add_system_field_update(out, n.range, .Insert_Db_Table if n.form == .Db_Table else .Insert_Table, "subrc")
	case ^ast.Modify_Stmt:
		has_internal := n.table_keyword || n.index != nil || len(n.transporting) > 0
		add_system_field_update(out, n.range, .Modify_Table if has_internal else .Modify_Db_Table, "subrc")
	case ^ast.Update_Stmt:
		add_system_field_update(out, n.range, .Update_Db_Table, "subrc")
	case ^ast.Delete_Stmt:
		add_system_field_update(out, n.range, .Delete_Db_Table if n.form == .Db_Table else .Delete_Table, "subrc")
		add_routine_site(out, n.range, .Delete)
	case ^ast.Append_Stmt:
		add_system_field_update(out, n.range, .Append, "subrc")
	case ^ast.Report_Stmt:
		#partial switch n.kind {
		case .Read_Report:
			add_system_field_update(out, n.range, .Read_Report, "subrc")
		case .Insert_Report:
			add_system_field_update(out, n.range, .Insert_Report, "subrc")
			add_routine_site(out, n.range, .Unknown_Effect)
		case .Delete_Report:
			add_system_field_update(out, n.range, .Delete_Report, "subrc")
			add_routine_site(out, n.range, .Unknown_Effect)
		}
	case ^ast.Textpool_Stmt:
		if n.kind == .Insert {
			add_system_field_update(out, n.range, .Insert_Textpool, "subrc")
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
		add_system_field_update(out, n.range, .Assign, "subrc")
	case ^ast.Enhancement_Stmt:
		collect_flat_stmt_list(out, n.body[:], allocator)
	case ^ast.Enhancement_Section_Stmt:
		collect_flat_stmt_list(out, n.body[:], allocator)
	case ^ast.Test_Seam_Stmt:
		collect_flat_stmt_list(out, n.body[:], allocator)
	case ^ast.Test_Injection_Stmt:
		collect_flat_stmt_list(out, n.body[:], allocator)
	}
}

collect_expr_lints :: proc(out: ^Unit_Lints, expr: ^ast.Expr, allocator: mem.Allocator) {
	if expr == nil {
		return
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Is_Predicate_Expr:
		collect_expr_lints(out, n.subject, allocator)
		collect_state_check(out, n, allocator)
	case ^ast.Binary_Expr:
		collect_expr_lints(out, n.left, allocator)
		collect_expr_lints(out, n.right, allocator)
		collect_zero_state_check(out, n, allocator)
	case ^ast.Unary_Expr:
		collect_expr_lints(out, n.expr, allocator)
	case ^ast.Paren_Expr:
		collect_expr_lints(out, n.expr, allocator)
	case ^ast.Host_Expr:
		collect_expr_lints(out, n.value, allocator)
	case ^ast.Selector_Expr:
		collect_expr_lints(out, n.base, allocator)
		collect_expr_lints(out, n.field, allocator)
	case ^ast.Table_Expr:
		collect_expr_lints(out, n.table, allocator)
		for selector in n.selectors {
			collect_expr_lints(out, selector, allocator)
		}
	case ^ast.Call_Expr:
		collect_expr_lints(out, n.callee, allocator)
		collect_expr_lints(out, n.args, allocator)
	case ^ast.Call_Arg_List_Expr:
		for arg in n.args {
			collect_expr_lints(out, arg, allocator)
		}
	case ^ast.Call_Arg_Section_Expr:
		for arg in n.args {
			collect_expr_lints(out, arg, allocator)
		}
	case ^ast.Call_Named_Arg_Expr:
		collect_expr_lints(out, n.value, allocator)
	case ^ast.Call_Positional_Arg_Expr:
		collect_expr_lints(out, n.value, allocator)
	case ^ast.Constructor_Expr:
		for arg in n.args {
			collect_expr_lints(out, arg, allocator)
		}
	}
}

collect_control_facts :: proc(out: ^Unit_Lints, stmts: []^ast.Stmt, loop_depth: int) {
	for stmt in stmts {
		if stmt == nil {
			continue
		}
		#partial switch n in stmt.derived_stmt {
		case ^ast.If_Stmt:
			append(&out.routine_control_regions, Routine_Control_Region_Data{kind = .If, range = n.range})
			collect_control_facts(out, n.body[:], loop_depth)
			for clause in n.elseif_clauses {
				collect_control_facts(out, clause.body[:], loop_depth)
			}
			if n.else_clause != nil {
				collect_control_facts(out, n.else_clause.body[:], loop_depth)
			}
		case ^ast.Case_Stmt:
			append(&out.routine_control_regions, Routine_Control_Region_Data{kind = .Case, range = n.range})
			collect_control_facts(out, n.recovery[:], loop_depth)
			for clause in n.whens {
				collect_control_facts(out, clause.body[:], loop_depth)
			}
		case ^ast.While_Stmt:
			append(&out.routine_control_regions, Routine_Control_Region_Data{kind = .Loop, range = n.range, loop_kind = .While})
			add_system_field_update(out, n.range, .While, "index")
			collect_control_facts(out, n.body[:], loop_depth + 1)
		case ^ast.Do_Stmt:
			append(&out.routine_control_regions, Routine_Control_Region_Data{kind = .Loop, range = n.range, loop_kind = .Do})
			add_system_field_update(out, n.range, .Do, "index")
			collect_control_facts(out, n.body[:], loop_depth + 1)
		case ^ast.Loop_Stmt:
			append(&out.routine_control_regions, Routine_Control_Region_Data{kind = .Loop, range = n.range, loop_kind = .Loop})
			add_system_field_update(out, n.range, .Loop_At, "subrc")
			add_system_field_update(out, n.range, .Loop_At, "tabix")
			add_system_field_update(out, n.range, .Loop_At, "tfill")
			add_system_field_update(out, n.range, .Loop_At, "tleng")
			collect_control_facts(out, n.body[:], loop_depth + 1)
		case ^ast.At_Stmt:
			kind := at_group_kind(n.kind)
			append(&out.routine_control_regions, Routine_Control_Region_Data{kind = .At, range = n.range, at_kind = kind})
			if loop_depth == 0 {
				append(
					&out.diagnostics,
					analyze.Diagnostic {
						kind = .Invalid_Control_Break,
						range = n.range,
						message = "AT group requires LOOP AT context",
					},
				)
			}
			collect_control_facts(out, n.body[:], loop_depth)
		case ^ast.Try_Stmt:
			append(&out.routine_control_regions, Routine_Control_Region_Data{kind = .Try, range = n.range})
			collect_control_facts(out, n.body[:], loop_depth)
			for clause in n.catches {
				collect_control_facts(out, clause.body[:], loop_depth)
			}
			if n.cleanup != nil {
				collect_control_facts(out, n.cleanup.body[:], loop_depth)
			}
		case ^ast.Class_Decl:
			collect_control_facts(out, n.body[:], loop_depth)
		case ^ast.Interface_Decl:
			collect_control_facts(out, n.body[:], loop_depth)
		case ^ast.Method_Decl:
			collect_control_facts(out, n.body[:], loop_depth)
		case ^ast.Form_Decl:
			collect_control_facts(out, n.body[:], loop_depth)
		case ^ast.Function_Decl:
			collect_control_facts(out, n.body[:], loop_depth)
		case ^ast.Module_Decl:
			collect_control_facts(out, n.body[:], loop_depth)
		case ^ast.Event_Block_Stmt:
			collect_control_facts(out, n.body[:], loop_depth)
		case ^ast.Enhancement_Stmt:
			collect_control_facts(out, n.body[:], loop_depth)
		case ^ast.Enhancement_Section_Stmt:
			collect_control_facts(out, n.body[:], loop_depth)
		case ^ast.Test_Seam_Stmt:
			collect_control_facts(out, n.body[:], loop_depth)
		case ^ast.Test_Injection_Stmt:
			collect_control_facts(out, n.body[:], loop_depth)
		case ^ast.Select_Stmt:
			collect_control_facts(out, n.body[:], loop_depth)
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

collect_state_check :: proc(out: ^Unit_Lints, expr: ^ast.Is_Predicate_Expr, allocator: mem.Allocator) {
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
				range = expr.range,
				symbol_name = access.base_name,
				symbol_range = access.base_range,
				field_name = last_field(access),
				kind = check_kind,
			},
		)
	}
}

collect_zero_state_check :: proc(out: ^Unit_Lints, expr: ^ast.Binary_Expr, allocator: mem.Allocator) {
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
			scope = analyze.INVALID_SCOPE_ID,
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
			append(&keys, canonical_name(key.name, allocator))
		}
		name := ""
		if access, ok := value_access_from_expr(entry.table, allocator); ok {
			name = table_order_name_from_access(access, allocator)
		}
		append(
			&out.read_table_binary_searches,
			Read_Table_Binary_Search_Data {
				range = entry.binary_search_clause,
				table_name = canonical_name(name, allocator),
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
		if field.name == "" || field.descending {
			return
		}
		append(&keys, canonical_name(field.name, allocator))
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
	if query == nil || query.result == nil || !query.result.table || len(query.order_by_fields) == 0 {
		return
	}
	access, ok := value_access_from_expr(query.result.target, allocator)
	if !ok {
		return
	}
	keys := make([dynamic]string, 0, len(query.order_by_fields), allocator)
	for field in query.order_by_fields {
		append(&keys, canonical_name(field.text, allocator))
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
		routine_name = canonical_name(name, allocator)
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
			return canonical_name(strip_quotes(name), allocator), range, true
		}
	}
	if stmt.program_kind == .Dynamic {
		if paren, ok := stmt.program.derived_expr.(^ast.Paren_Expr); ok {
			if name, range, name_ok := expr_name(paren.expr); name_ok {
				if _, lit_ok := paren.expr.derived_expr.(^ast.Literal_Expr); lit_ok {
					return canonical_name(strip_quotes(name), allocator), range, true
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

collect_find_site :: proc(out: ^Unit_Lints, stmt: ^ast.Find_Stmt, allocator: mem.Allocator) {
	add_system_field_update(out, stmt.range, .Find, "subrc")
	add_system_field_update(out, stmt.range, .Find, "fdpos")
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
) {
	append(&out.system_field_updates, System_Field_Update_Data{range = range, statement = statement, field_name = field_name})
}

Value_Access :: struct {
	base_name:  string,
	base_range: tokenizer.Range,
	fields:     [dynamic]string,
}

value_access_from_expr :: proc(expr: ^ast.Expr, allocator: mem.Allocator) -> (Value_Access, bool) {
	if expr == nil {
		return {}, false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Host_Expr:
		return value_access_from_expr(n.value, allocator)
	case ^ast.Paren_Expr:
		return value_access_from_expr(n.expr, allocator)
	case ^ast.Ident_Expr:
		if n.name == "" {
			return {}, false
		}
		return Value_Access {
			base_name = canonical_name(n.name, allocator),
			base_range = n.range,
			fields = make([dynamic]string, 0, 2, allocator),
		}, true
	case ^ast.Data_Inline_Name_Expr:
		if n.name == "" {
			return {}, false
		}
		return Value_Access {
			base_name = canonical_name(n.name, allocator),
			base_range = n.range,
			fields = make([dynamic]string, 0, 2, allocator),
		}, true
	case ^ast.Field_Symbol_Inline_Name_Expr:
		if n.name == "" {
			return {}, false
		}
		return Value_Access {
			base_name = canonical_name(n.name, allocator),
			base_range = n.range,
			fields = make([dynamic]string, 0, 2, allocator),
		}, true
	case ^ast.Type_Ref_Expr:
		if n.name == "" {
			return {}, false
		}
		return Value_Access {
			base_name = canonical_name(n.name, allocator),
			base_range = n.range,
			fields = make([dynamic]string, 0, 2, allocator),
		}, true
	case ^ast.Selector_Expr:
		access, ok := value_access_from_expr(n.base, allocator)
		if !ok {
			return {}, false
		}
		if name, _, name_ok := expr_name(n.field); name_ok {
			append(&access.fields, canonical_name(name, allocator))
			return access, true
		}
	}
	return {}, false
}

last_field :: proc(access: Value_Access) -> string {
	if len(access.fields) == 0 {
		return ""
	}
	return access.fields[len(access.fields) - 1]
}

table_order_name_from_access :: proc(access: Value_Access, allocator: mem.Allocator) -> string {
	if len(access.fields) == 0 {
		return access.base_name
	}
	out := strings.builder_make(allocator)
	strings.write_string(&out, access.base_name)
	for field in access.fields {
		strings.write_byte(&out, '-')
		strings.write_string(&out, field)
	}
	return strings.to_string(out)
}

expr_name :: proc(expr: ^ast.Expr) -> (string, tokenizer.Range, bool) {
	if expr == nil {
		return "", tokenizer.Range{}, false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		return n.name, n.range, n.name != ""
	case ^ast.Type_Ref_Expr:
		return n.name, n.range, n.name != ""
	case ^ast.Literal_Expr:
		return n.value, n.range, n.value != ""
	}
	return "", tokenizer.Range{}, false
}

canonical_name :: proc(name: string, allocator: mem.Allocator) -> string {
	return strings.to_lower(name, allocator)
}

strip_quotes :: proc(value: string) -> string {
	if len(value) >= 2 && ((value[0] == '\'' && value[len(value) - 1] == '\'') || (value[0] == '"' && value[len(value) - 1] == '"')) {
		return value[1:len(value) - 1]
	}
	return value
}

rebuild_project_diagnostics :: proc(project: ^analyze.Project_Analysis) {
	clear(&project.diagnostics)
	for unit in project.providers.source_files {
		for diagnostic in unit.diagnostics {
			if !project_diagnostic_present(project.diagnostics[:], diagnostic) {
				append(&project.diagnostics, diagnostic)
			}
		}
	}
}

project_diagnostic_present :: proc(existing: []analyze.Diagnostic, diagnostic: analyze.Diagnostic) -> bool {
	for item in existing {
		if item.kind == diagnostic.kind &&
		   item.range == diagnostic.range &&
		   item.message == diagnostic.message {
			return true
		}
	}
	return false
}
