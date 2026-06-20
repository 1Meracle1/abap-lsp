package abap_frontend_lints

import "src:ast"
import "src:semantic"
import "src:utils"

import "core:mem"
import "core:strings"

Routine_Flow_Context :: struct {
	out:                         ^Unit_Lints,
	ref_query:                   semantic.Semantic_Ref_Query,
	decl_query:                  semantic.Semantic_Decl_Query,
	policy:                      ^Policy,
	allocator:                   mem.Allocator,
	form_parameter_effects:      map[Routine_Flow_Form_Parameter_Key]Routine_Flow_Form_Parameter_Effect,
	routine_name:                string,
	routine_range:               semantic.Range,
	global_declarations:         bool,
	leave_list_processing_exits: bool,
	tracked_values:              [dynamic]^semantic.Entity,
	dead_store_untracked_values: [dynamic]^semantic.Entity,
}

Routine_Flow_State :: struct {
	assigned:              [dynamic]^semantic.Entity,
	assigned_fields:       [dynamic]Routine_Flow_Field_Assignment,
	bound:                 [dynamic]^semantic.Entity,
	non_initial:           [dynamic]^semantic.Entity,
	non_initial_fields:    [dynamic]Routine_Flow_Field_Assignment,
	last_success_assigned: [dynamic]^semantic.Entity,
	last_success_fields:   [dynamic]Routine_Flow_Field_Assignment,
	last_success_bound:    [dynamic]^semantic.Entity,
	terminated:           bool,
}

Routine_Flow_Field_Assignment :: struct {
	entity: ^semantic.Entity,
	field:  string,
}

Routine_Flow_Form_Parameter_Key :: struct {
	routine_name:       string,
	section:            Perform_Parameter_Section,
	ordinal_in_section: int,
}

Routine_Flow_Form_Parameter_Effect :: struct {
	reads_before_write: bool,
	may_write:          bool,
}

Routine_Flow_Form_Parameter_State :: struct {
	name:    string,
	key:     Routine_Flow_Form_Parameter_Key,
	effect:  Routine_Flow_Form_Parameter_Effect,
	written: bool,
}

Routine_Flow_Dead_Store_Write :: struct {
	entity: ^semantic.Entity,
	range:  semantic.Range,
}

Routine_Flow_Dead_Store_Transfer :: struct {
	reads:  [dynamic]^semantic.Entity,
	writes: [dynamic]Routine_Flow_Dead_Store_Write,
}

Routine_Flow_Dead_Store_Untracked_Walk :: struct {
	ctx: ^Routine_Flow_Context,
}

Routine_Flow_Callable_Body :: struct {
	entity: ^semantic.Entity,
	body:   []^ast.Stmt,
}


emit_routine_flow_lints :: proc(
	out: ^Unit_Lints,
	project: ^semantic.Project,
	checker: ^semantic.Checker,
	file: ^semantic.Project_File,
	policy: ^Policy,
	allocator: mem.Allocator,
) {
	if out == nil || project == nil || checker == nil || file == nil || file.root == nil {
		return
	}
	query := semantic.semantic_query(project, checker, file)
	form_parameter_effects := routine_flow_collect_form_parameter_effects(file.root.stmts[:], allocator)
	ctx := Routine_Flow_Context {
		out = out,
		ref_query = semantic.semantic_query_refs(query),
		decl_query = semantic.semantic_query_decls(query),
		policy = policy,
		allocator = allocator,
		form_parameter_effects = form_parameter_effects,
		tracked_values = make([dynamic]^semantic.Entity, 0, 4, allocator),
	}
	routine_flow_analyze_global_declarations(&ctx, file.root.stmts[:], file.root.range)
	routine_flow_visit_stmt_list_for_routines(&ctx, file.root.stmts[:])
}

routine_flow_collect_callable_summaries :: proc(
	out: ^Unit_Lints,
	stmts: []^ast.Stmt,
	allocator: mem.Allocator,
) -> [dynamic]Routine_Callable_Summary {
	summaries := make([dynamic]Routine_Callable_Summary, 0, 8, allocator)
	if out == nil || out.project == nil || out.checker == nil || out.file == nil {
		return summaries
	}
	bodies := make([dynamic]Routine_Flow_Callable_Body, 0, 8, allocator)
	routine_flow_collect_callable_bodies(out, &bodies, stmts)
	for body in bodies {
		if body.entity != nil && routine_flow_callable_summary_index(summaries[:], body.entity) < 0 {
			append(&summaries, Routine_Callable_Summary{entity = body.entity})
		}
	}
	for iteration := 0; iteration < len(bodies) + 1; iteration += 1 {
		changed := false
		for body in bodies {
			if body.entity == nil {
				continue
			}
			no_return := routine_flow_stmt_list_never_returns_to_caller(
				out,
				body.body,
				summaries[:],
			)
			if routine_flow_set_callable_no_return(&summaries, body.entity, no_return) {
				changed = true
			}
		}
		if !changed {
			break
		}
	}
	return summaries
}

routine_flow_collect_callable_bodies :: proc(
	out: ^Unit_Lints,
	bodies: ^[dynamic]Routine_Flow_Callable_Body,
	stmts: []^ast.Stmt,
) {
	for stmt in stmts {
		if stmt == nil {
			continue
		}
		#partial switch n in stmt.derived_stmt {
		case ^ast.Method_Decl:
			if len(n.body) > 0 {
				append(
					bodies,
					Routine_Flow_Callable_Body {
						entity = routine_flow_entity_for_method_decl(out, n),
						body = n.body[:],
					},
				)
			}
		case ^ast.Form_Decl:
			append(
				bodies,
				Routine_Flow_Callable_Body {
					entity = routine_flow_entity_for_routine_decl(out, .Form, n.name.range),
					body = n.body[:],
				},
			)
		case ^ast.Function_Decl:
			append(
				bodies,
				Routine_Flow_Callable_Body {
					entity = routine_flow_entity_for_routine_decl(out, .Module, n.name.range),
					body = n.body[:],
				},
			)
		case ^ast.Module_Decl:
			append(
				bodies,
				Routine_Flow_Callable_Body {
					entity = routine_flow_entity_for_routine_decl(out, .Module, n.name.range),
					body = n.body[:],
				},
			)
		case ^ast.Class_Decl:
			routine_flow_collect_callable_bodies(out, bodies, n.body[:])
		case ^ast.Interface_Decl:
			routine_flow_collect_callable_bodies(out, bodies, n.body[:])
		case ^ast.Enhancement_Stmt:
			routine_flow_collect_callable_bodies(out, bodies, n.body[:])
		case ^ast.Enhancement_Section_Stmt:
			routine_flow_collect_callable_bodies(out, bodies, n.body[:])
		case ^ast.Test_Seam_Stmt:
			routine_flow_collect_callable_bodies(out, bodies, n.body[:])
		case ^ast.Test_Injection_Stmt:
			routine_flow_collect_callable_bodies(out, bodies, n.body[:])
		}
	}
}

routine_flow_entity_for_method_decl :: proc(out: ^Unit_Lints, decl: ^ast.Method_Decl) -> ^semantic.Entity {
	if out == nil || out.project == nil || out.checker == nil || decl == nil || decl.name.range.end <= decl.name.range.start {
		return nil
	}
	query := semantic.semantic_query(out.project, out.checker, out.file)
	entity := semantic.semantic_decl_class_member_at_offset(semantic.semantic_query_decls(query), decl.name.range.start)
	if entity != nil && entity.kind == .Method {
		return entity
	}
	return nil
}

routine_flow_entity_for_routine_decl :: proc(
	out: ^Unit_Lints,
	kind: semantic.Entity_Kind,
	range: semantic.Range,
) -> ^semantic.Entity {
	if out == nil || out.project == nil || out.checker == nil || range.end <= range.start {
		return nil
	}
	query := semantic.semantic_query(out.project, out.checker, out.file)
	return semantic.semantic_decl_entity_with_kind_and_decl_range(semantic.semantic_query_decls(query), kind, range)
}

routine_flow_callable_summary_index :: proc(
	summaries: []Routine_Callable_Summary,
	entity: ^semantic.Entity,
) -> int {
	for summary, i in summaries {
		if summary.entity == entity {
			return i
		}
	}
	return -1
}

routine_flow_set_callable_no_return :: proc(
	summaries: ^[dynamic]Routine_Callable_Summary,
	entity: ^semantic.Entity,
	no_return: bool,
) -> bool {
	index := routine_flow_callable_summary_index(summaries[:], entity)
	if index < 0 {
		append(summaries, Routine_Callable_Summary{entity = entity, no_return = no_return})
		return no_return
	}
	if summaries[index].no_return == no_return {
		return false
	}
	summaries[index].no_return = no_return
	return true
}

routine_flow_stmt_list_never_returns_to_caller :: proc(
	out: ^Unit_Lints,
	stmts: []^ast.Stmt,
	summaries: []Routine_Callable_Summary,
) -> bool {
	for stmt in stmts {
		if stmt == nil {
			continue
		}
		if routine_flow_stmt_never_returns_to_caller(out, stmt, summaries) {
			return true
		}
	}
	return false
}

routine_flow_stmt_never_returns_to_caller :: proc(
	out: ^Unit_Lints,
	stmt: ^ast.Stmt,
	summaries: []Routine_Callable_Summary,
) -> bool {
	if stmt == nil {
		return false
	}
	#partial switch n in stmt.derived_stmt {
	case ^ast.Flow_Stmt:
		return n.kind == .Stop
	case ^ast.Raise_Stmt:
		return true
	case ^ast.Expr_Stmt:
		return expr_is_known_no_return_call_with_summaries(out, n.expr, summaries)
	case ^ast.Call_Stmt:
		return call_stmt_is_known_no_return_with_summaries(out, n, summaries)
	case ^ast.If_Stmt:
		if n.else_clause == nil {
			return false
		}
		if !routine_flow_stmt_list_never_returns_to_caller(out, n.body[:], summaries) {
			return false
		}
		for clause in n.elseif_clauses {
			if !routine_flow_stmt_list_never_returns_to_caller(out, clause.body[:], summaries) {
				return false
			}
		}
		return routine_flow_stmt_list_never_returns_to_caller(out, n.else_clause.body[:], summaries)
	case ^ast.Case_Stmt:
		has_others := false
		for clause in n.whens {
			if clause.is_others {
				has_others = true
			}
			if !routine_flow_stmt_list_never_returns_to_caller(out, clause.body[:], summaries) {
				return false
			}
		}
		return has_others
	}
	return false
}

routine_flow_collect_form_parameter_effects :: proc(
	stmts: []^ast.Stmt,
	allocator: mem.Allocator,
) -> map[Routine_Flow_Form_Parameter_Key]Routine_Flow_Form_Parameter_Effect {
	effects := make(map[Routine_Flow_Form_Parameter_Key]Routine_Flow_Form_Parameter_Effect, 8, allocator)
	forms := make([dynamic]^ast.Form_Decl, 0, 8, allocator)
	routine_flow_collect_form_decls_from_stmt_list(&forms, stmts, allocator)
	for iteration := 0; iteration < len(forms) + 1; iteration += 1 {
		changed := false
		for form in forms {
			if routine_flow_collect_form_parameter_effects_from_form(&effects, form, allocator) {
				changed = true
			}
		}
		if !changed {
			break
		}
	}
	return effects
}

routine_flow_collect_form_decls_from_stmt_list :: proc(
	forms: ^[dynamic]^ast.Form_Decl,
	stmts: []^ast.Stmt,
	allocator: mem.Allocator,
) {
	for stmt in stmts {
		if stmt == nil {
			continue
		}
		#partial switch n in stmt.derived_stmt {
		case ^ast.Form_Decl:
			append(forms, n)
		case ^ast.Class_Decl:
			routine_flow_collect_form_decls_from_stmt_list(forms, n.body[:], allocator)
		case ^ast.Interface_Decl:
			routine_flow_collect_form_decls_from_stmt_list(forms, n.body[:], allocator)
		case ^ast.Enhancement_Stmt:
			routine_flow_collect_form_decls_from_stmt_list(forms, n.body[:], allocator)
		case ^ast.Enhancement_Section_Stmt:
			routine_flow_collect_form_decls_from_stmt_list(forms, n.body[:], allocator)
		case ^ast.Test_Seam_Stmt:
			routine_flow_collect_form_decls_from_stmt_list(forms, n.body[:], allocator)
		case ^ast.Test_Injection_Stmt:
			routine_flow_collect_form_decls_from_stmt_list(forms, n.body[:], allocator)
		}
	}
}

routine_flow_collect_form_parameter_effects_from_form :: proc(
	effects: ^map[Routine_Flow_Form_Parameter_Key]Routine_Flow_Form_Parameter_Effect,
	form: ^ast.Form_Decl,
	allocator: mem.Allocator,
) -> bool {
	if form == nil || form.name.text == "" {
		return false
	}
	routine_name := utils.to_lower_ascii(form.name.text, allocator)
	states := make([dynamic]Routine_Flow_Form_Parameter_State, 0, len(form.form_parameters), allocator)
	ordinals := [Perform_Parameter_Section]int{}
	for param in form.form_parameters {
		if param.name.text == "" {
			continue
		}
		section := routine_flow_perform_section_from_form_section(param.section)
		ordinal := ordinals[section]
		ordinals[section] += 1
		append(
			&states,
			Routine_Flow_Form_Parameter_State {
				name = utils.to_lower_ascii(param.name.text, allocator),
				key = Routine_Flow_Form_Parameter_Key {
					routine_name = routine_name,
					section = section,
					ordinal_in_section = ordinal,
				},
			},
		)
	}
	if len(states) == 0 {
		return false
	}
	routine_flow_form_parameter_analyze_stmt_list(effects, &states, form.body[:], allocator)
	changed := false
	for state in states {
		if old, ok := effects[state.key]; !ok || !routine_flow_form_parameter_effect_equal(old, state.effect) {
			effects[state.key] = state.effect
			changed = true
		}
	}
	return changed
}

routine_flow_form_parameter_effect_equal :: proc "contextless" (
	left, right: Routine_Flow_Form_Parameter_Effect,
) -> bool {
	return left.reads_before_write == right.reads_before_write && left.may_write == right.may_write
}

routine_flow_perform_section_from_form_section :: proc "contextless" (section: ast.Form_Parameter_Section) -> Perform_Parameter_Section {
	#partial switch section {
	case .Tables:
		return .Tables
	case .Using:
		return .Using
	case .Changing:
		return .Changing
	}
	return .Using
}

routine_flow_form_parameter_analyze_stmt_list :: proc(
	effects: ^map[Routine_Flow_Form_Parameter_Key]Routine_Flow_Form_Parameter_Effect,
	states: ^[dynamic]Routine_Flow_Form_Parameter_State,
	stmts: []^ast.Stmt,
	allocator: mem.Allocator,
) {
	for stmt in stmts {
		routine_flow_form_parameter_analyze_stmt(effects, states, stmt, allocator)
	}
}

routine_flow_form_parameter_analyze_stmt :: proc(
	effects: ^map[Routine_Flow_Form_Parameter_Key]Routine_Flow_Form_Parameter_Effect,
	states: ^[dynamic]Routine_Flow_Form_Parameter_State,
	stmt: ^ast.Stmt,
	allocator: mem.Allocator,
) {
	if stmt == nil {
		return
	}
	#partial switch n in stmt.derived_stmt {
	case ^ast.Data_Chained_Decl:
		for clause in n.decls {
			if clause.value_clause != nil {
				routine_flow_form_parameter_read_expr(states, clause.value_clause.expr, allocator)
			}
		}
	case ^ast.Data_Inline_Decl:
		routine_flow_form_parameter_read_expr(states, n.expr, allocator)
	case ^ast.Assign_Stmt:
		routine_flow_form_parameter_read_expr(states, n.rhs, allocator)
		routine_flow_form_parameter_write_expr(states, n.lhs, allocator)
		for lhs in n.chain_lhs {
			routine_flow_form_parameter_write_expr(states, lhs, allocator)
		}
	case ^ast.Downcast_Assign_Stmt:
		routine_flow_form_parameter_read_expr(states, n.rhs, allocator)
		routine_flow_form_parameter_write_expr(states, n.lhs, allocator)
	case ^ast.Move_Stmt:
		for entry in n.entries {
			routine_flow_form_parameter_read_expr(states, entry.source, allocator)
			routine_flow_form_parameter_write_expr(states, entry.target, allocator)
		}
	case ^ast.Move_Corresponding_Stmt:
		for entry in n.entries {
			routine_flow_form_parameter_read_expr(states, entry.source, allocator)
			routine_flow_form_parameter_write_expr(states, entry.target, allocator)
		}
	case ^ast.Add_Stmt:
		for entry in n.entries {
			routine_flow_form_parameter_analyze_arithmetic_entry(states, entry.source, entry.target, entry.result, allocator)
		}
	case ^ast.Subtract_Stmt:
		for entry in n.entries {
			routine_flow_form_parameter_analyze_arithmetic_entry(states, entry.source, entry.target, entry.result, allocator)
		}
	case ^ast.Multiply_Stmt:
		for entry in n.entries {
			routine_flow_form_parameter_analyze_arithmetic_entry(states, entry.source, entry.target, entry.result, allocator)
		}
	case ^ast.Divide_Stmt:
		for entry in n.entries {
			routine_flow_form_parameter_analyze_arithmetic_entry(states, entry.source, entry.target, entry.result, allocator)
		}
	case ^ast.Compute_Stmt:
		for entry in n.entries {
			routine_flow_form_parameter_read_expr(states, entry.source, allocator)
			routine_flow_form_parameter_write_expr(states, entry.target, allocator)
		}
	case ^ast.Concatenate_Stmt:
		for entry in n.entries {
			for source in entry.sources {
				routine_flow_form_parameter_read_expr(states, source, allocator)
			}
			routine_flow_form_parameter_read_expr(states, entry.separator, allocator)
			routine_flow_form_parameter_write_expr(states, entry.target, allocator)
		}
	case ^ast.Split_Stmt:
		for entry in n.entries {
			routine_flow_form_parameter_read_expr(states, entry.source, allocator)
			routine_flow_form_parameter_read_expr(states, entry.separator, allocator)
			for target in entry.targets {
				routine_flow_form_parameter_write_expr(states, target, allocator)
			}
		}
	case ^ast.Condense_Stmt:
		routine_flow_form_parameter_read_expr(states, n.target, allocator)
		routine_flow_form_parameter_write_expr(states, n.target, allocator)
	case ^ast.Replace_Stmt:
		routine_flow_form_parameter_read_expr(states, n.pattern, allocator)
		routine_flow_form_parameter_read_expr(states, n.target, allocator)
		routine_flow_form_parameter_read_expr(states, n.replacement, allocator)
		routine_flow_form_parameter_read_expr(states, n.section_offset, allocator)
		routine_flow_form_parameter_read_expr(states, n.section_length, allocator)
		routine_flow_form_parameter_write_expr(states, n.target, allocator)
	case ^ast.Translate_Stmt:
		routine_flow_form_parameter_read_expr(states, n.target, allocator)
		routine_flow_form_parameter_read_expr(states, n.operand, allocator)
		routine_flow_form_parameter_write_expr(states, n.target, allocator)
	case ^ast.Shift_Stmt:
		routine_flow_form_parameter_read_expr(states, n.target, allocator)
		routine_flow_form_parameter_read_expr(states, n.places, allocator)
		routine_flow_form_parameter_read_expr(states, n.delete_pattern, allocator)
		routine_flow_form_parameter_write_expr(states, n.target, allocator)
	case ^ast.Expr_Stmt:
		routine_flow_form_parameter_read_expr(states, n.expr, allocator)
	case ^ast.If_Stmt:
		routine_flow_form_parameter_read_expr(states, n.condition, allocator)
		branches := make([dynamic][dynamic]Routine_Flow_Form_Parameter_State, 0, 2 + len(n.elseif_clauses), allocator)
		body := routine_flow_form_parameter_states_clone(states[:], allocator)
		routine_flow_form_parameter_analyze_stmt_list(effects, &body, n.body[:], allocator)
		append(&branches, body)
		remaining := routine_flow_form_parameter_states_clone(states[:], allocator)
		for clause in n.elseif_clauses {
			routine_flow_form_parameter_read_expr(&remaining, clause.condition, allocator)
			clause_state := routine_flow_form_parameter_states_clone(remaining[:], allocator)
			routine_flow_form_parameter_analyze_stmt_list(effects, &clause_state, clause.body[:], allocator)
			append(&branches, clause_state)
		}
		if n.else_clause != nil {
			else_state := routine_flow_form_parameter_states_clone(remaining[:], allocator)
			routine_flow_form_parameter_analyze_stmt_list(effects, &else_state, n.else_clause.body[:], allocator)
			append(&branches, else_state)
		} else {
			append(&branches, remaining)
		}
		routine_flow_form_parameter_merge_branch_states(states, branches[:])
	case ^ast.Case_Stmt:
		routine_flow_form_parameter_read_expr(states, n.expr, allocator)
		branches := make([dynamic][dynamic]Routine_Flow_Form_Parameter_State, 0, 1 + len(n.whens), allocator)
		if len(n.recovery) > 0 {
			recovery := routine_flow_form_parameter_states_clone(states[:], allocator)
			routine_flow_form_parameter_analyze_stmt_list(effects, &recovery, n.recovery[:], allocator)
			append(&branches, recovery)
		}
		for clause in n.whens {
			for operand in clause.operands {
				routine_flow_form_parameter_read_expr(states, operand, allocator)
			}
			clause_state := routine_flow_form_parameter_states_clone(states[:], allocator)
			routine_flow_form_parameter_analyze_stmt_list(effects, &clause_state, clause.body[:], allocator)
			append(&branches, clause_state)
		}
		fallthrough_state := routine_flow_form_parameter_states_clone(states[:], allocator)
		append(&branches, fallthrough_state)
		routine_flow_form_parameter_merge_branch_states(states, branches[:])
	case ^ast.While_Stmt:
		routine_flow_form_parameter_read_expr(states, n.condition, allocator)
		body := routine_flow_form_parameter_states_clone(states[:], allocator)
		routine_flow_form_parameter_analyze_stmt_list(effects, &body, n.body[:], allocator)
		routine_flow_form_parameter_merge_effects(states, body[:])
	case ^ast.Do_Stmt:
		routine_flow_form_parameter_read_expr(states, n.count, allocator)
		body := routine_flow_form_parameter_states_clone(states[:], allocator)
		routine_flow_form_parameter_analyze_stmt_list(effects, &body, n.body[:], allocator)
		routine_flow_form_parameter_merge_effects(states, body[:])
	case ^ast.Loop_Stmt:
		routine_flow_form_parameter_read_expr(states, n.source, allocator)
		routine_flow_form_parameter_read_expr(states, n.from, allocator)
		routine_flow_form_parameter_read_expr(states, n.to, allocator)
		routine_flow_form_parameter_read_expr(states, n.using_key.dynamic_name, allocator)
		routine_flow_form_parameter_read_expr(states, n.where_cond, allocator)
		routine_flow_form_parameter_read_expr(states, n.group_by, allocator)
		body := routine_flow_form_parameter_states_clone(states[:], allocator)
		routine_flow_form_parameter_write_expr(&body, n.target, allocator)
		routine_flow_form_parameter_write_expr(&body, n.group_target, allocator)
		routine_flow_form_parameter_analyze_stmt_list(effects, &body, n.body[:], allocator)
		routine_flow_form_parameter_merge_effects(states, body[:])
	case ^ast.At_Stmt:
		routine_flow_form_parameter_read_expr(states, n.expr, allocator)
		body := routine_flow_form_parameter_states_clone(states[:], allocator)
		routine_flow_form_parameter_analyze_stmt_list(effects, &body, n.body[:], allocator)
		routine_flow_form_parameter_merge_effects(states, body[:])
	case ^ast.Try_Stmt:
		branches := make([dynamic][dynamic]Routine_Flow_Form_Parameter_State, 0, 1 + len(n.catches), allocator)
		body := routine_flow_form_parameter_states_clone(states[:], allocator)
		routine_flow_form_parameter_analyze_stmt_list(effects, &body, n.body[:], allocator)
		append(&branches, body)
		for clause in n.catches {
			catch_state := routine_flow_form_parameter_states_clone(states[:], allocator)
			for exception in clause.exceptions {
				routine_flow_form_parameter_read_expr(&catch_state, exception, allocator)
			}
			routine_flow_form_parameter_write_expr(&catch_state, clause.into, allocator)
			routine_flow_form_parameter_analyze_stmt_list(effects, &catch_state, clause.body[:], allocator)
			append(&branches, catch_state)
		}
		routine_flow_form_parameter_merge_branch_states(states, branches[:])
		if n.cleanup != nil {
			routine_flow_form_parameter_analyze_stmt_list(effects, states, n.cleanup.body[:], allocator)
		}
	case ^ast.Select_Stmt:
		routine_flow_form_parameter_read_select_query(states, &n.query, allocator)
		if n.query.result != nil && n.query.result.target != nil {
			routine_flow_form_parameter_write_expr(states, n.query.result.target, allocator)
		}
		routine_flow_form_parameter_analyze_stmt_list(effects, states, n.body[:], allocator)
	case ^ast.Open_Cursor_Stmt:
		routine_flow_form_parameter_read_select_query(states, &n.query, allocator)
		routine_flow_form_parameter_write_expr(states, n.handle, allocator)
	case ^ast.Fetch_Stmt:
		routine_flow_form_parameter_read_expr(states, n.handle, allocator)
		routine_flow_form_parameter_read_expr(states, n.package_size, allocator)
		if n.result != nil {
			routine_flow_form_parameter_write_expr(states, n.result.target, allocator)
		}
	case ^ast.Close_Cursor_Stmt:
		routine_flow_form_parameter_read_expr(states, n.handle, allocator)
	case ^ast.Read_Table_Stmt:
		for entry in n.entries {
			routine_flow_form_parameter_read_expr(states, entry.table, allocator)
			routine_flow_form_parameter_read_expr(states, entry.index, allocator)
			routine_flow_form_parameter_read_expr(states, entry.using_key.dynamic_name, allocator)
			for key in entry.key_values {
				routine_flow_form_parameter_read_expr(states, key.dynamic_name, allocator)
				routine_flow_form_parameter_read_expr(states, key.value, allocator)
			}
			for comparing in entry.comparing {
				routine_flow_form_parameter_read_expr(states, comparing, allocator)
			}
			routine_flow_form_parameter_write_expr(states, entry.into, allocator)
			routine_flow_form_parameter_write_expr(states, entry.reference_into, allocator)
		}
	case ^ast.Assign_Field_Stmt:
		routine_flow_form_parameter_read_expr(states, n.source, allocator)
		routine_flow_form_parameter_read_expr(states, n.component, allocator)
		routine_flow_form_parameter_read_expr(states, n.structure, allocator)
		routine_flow_form_parameter_read_expr(states, n.casting_type, allocator)
		routine_flow_form_parameter_read_expr(states, n.casting_decimals, allocator)
		routine_flow_form_parameter_write_expr(states, n.target, allocator)
	case ^ast.Insert_Stmt:
		routine_flow_form_parameter_read_expr(states, n.source, allocator)
		routine_flow_form_parameter_read_expr(states, n.target, allocator)
		routine_flow_form_parameter_read_expr(states, n.index, allocator)
		for assignment in n.assignments {
			routine_flow_form_parameter_read_expr(states, assignment.name, allocator)
			routine_flow_form_parameter_read_expr(states, assignment.value, allocator)
		}
		routine_flow_form_parameter_write_expr(states, n.assigning, allocator)
		routine_flow_form_parameter_write_expr(states, n.reference_into, allocator)
	case ^ast.Append_Stmt:
		routine_flow_form_parameter_read_expr(states, n.source, allocator)
		routine_flow_form_parameter_read_expr(states, n.target, allocator)
		routine_flow_form_parameter_write_expr(states, n.assigning, allocator)
		routine_flow_form_parameter_write_expr(states, n.reference_into, allocator)
	case ^ast.Modify_Stmt:
		routine_flow_form_parameter_read_expr(states, n.target, allocator)
		routine_flow_form_parameter_read_expr(states, n.source, allocator)
		routine_flow_form_parameter_read_expr(states, n.index, allocator)
		routine_flow_form_parameter_read_expr(states, n.where_cond, allocator)
	case ^ast.Sort_Stmt:
		routine_flow_form_parameter_read_expr(states, n.target, allocator)
		for field in n.fields {
			routine_flow_form_parameter_read_expr(states, field.expr, allocator)
		}
	case ^ast.Update_Stmt:
		routine_flow_form_parameter_read_expr(states, n.target, allocator)
		routine_flow_form_parameter_read_expr(states, n.source, allocator)
		for assignment in n.assignments {
			routine_flow_form_parameter_read_expr(states, assignment.name, allocator)
			routine_flow_form_parameter_read_expr(states, assignment.value, allocator)
		}
		routine_flow_form_parameter_read_expr(states, n.where_cond, allocator)
	case ^ast.Delete_Stmt:
		routine_flow_form_parameter_read_expr(states, n.target, allocator)
		routine_flow_form_parameter_read_expr(states, n.source, allocator)
		routine_flow_form_parameter_read_expr(states, n.index, allocator)
		routine_flow_form_parameter_read_expr(states, n.where_cond, allocator)
		routine_flow_form_parameter_read_expr(states, n.using_key.dynamic_name, allocator)
		for comparing in n.comparing {
			routine_flow_form_parameter_read_expr(states, comparing.expr, allocator)
		}
	case ^ast.Clear_Stmt:
		for op in n.operands {
			routine_flow_form_parameter_read_expr(states, op.value, allocator)
			routine_flow_form_parameter_write_expr(states, op.target, allocator)
		}
	case ^ast.Refresh_Stmt:
		for op in n.operands {
			routine_flow_form_parameter_write_expr(states, op.target, allocator)
		}
	case ^ast.Free_Stmt:
		for op in n.operands {
			routine_flow_form_parameter_write_expr(states, op.target, allocator)
		}
		routine_flow_form_parameter_read_expr(states, n.memory_id, allocator)
	case ^ast.Unassign_Stmt:
		for op in n.operands {
			routine_flow_form_parameter_write_expr(states, op.target, allocator)
		}
	case ^ast.Find_Stmt:
		routine_flow_form_parameter_read_expr(states, n.pattern, allocator)
		routine_flow_form_parameter_read_expr(states, n.target, allocator)
		routine_flow_form_parameter_read_expr(states, n.section_offset, allocator)
		routine_flow_form_parameter_read_expr(states, n.section_length, allocator)
		routine_flow_form_parameter_write_expr(states, n.match_offset, allocator)
		routine_flow_form_parameter_write_expr(states, n.match_length, allocator)
		routine_flow_form_parameter_write_expr(states, n.match_line, allocator)
		routine_flow_form_parameter_write_expr(states, n.match_count, allocator)
		for submatch in n.submatches {
			routine_flow_form_parameter_write_expr(states, submatch, allocator)
		}
		routine_flow_form_parameter_write_expr(states, n.results, allocator)
	case ^ast.Search_Stmt:
		routine_flow_form_parameter_read_expr(states, n.target, allocator)
		routine_flow_form_parameter_read_expr(states, n.pattern, allocator)
		routine_flow_form_parameter_read_expr(states, n.starting_at, allocator)
		routine_flow_form_parameter_read_expr(states, n.ending_at, allocator)
	case ^ast.Call_Stmt:
		routine_flow_form_parameter_read_expr(states, n.call, allocator)
		routine_flow_form_parameter_read_expr(states, n.target, allocator)
		routine_flow_form_parameter_read_expr(states, n.function_destination, allocator)
		routine_flow_form_parameter_read_expr(states, n.function_task, allocator)
		routine_flow_form_parameter_read_expr(states, n.function_end_task_handler, allocator)
		routine_flow_form_parameter_read_expr(states, n.function_parameter_table, allocator)
		routine_flow_form_parameter_read_expr(states, n.function_exception_table, allocator)
		for arg in n.named_args {
			routine_flow_form_parameter_apply_call_arg_effect(states, arg.value, arg.section, allocator)
			routine_flow_form_parameter_read_expr(states, arg.message, allocator)
		}
		for arg in n.transformation_args {
			if arg.kind == .Result {
				routine_flow_form_parameter_write_expr(states, arg.value, allocator)
			} else {
				routine_flow_form_parameter_read_expr(states, arg.value, allocator)
			}
		}
	case ^ast.Receive_Results_Stmt:
		routine_flow_form_parameter_read_expr(states, n.target, allocator)
		for arg in n.named_args {
			routine_flow_form_parameter_apply_call_arg_effect(states, arg.value, arg.section, allocator)
			routine_flow_form_parameter_read_expr(states, arg.message, allocator)
		}
	case ^ast.Submit_Stmt:
		routine_flow_form_parameter_read_expr(states, n.target, allocator)
		for option in n.options {
			routine_flow_form_parameter_read_expr(states, option.value, allocator)
			routine_flow_form_parameter_read_expr(states, option.high_value, allocator)
			routine_flow_form_parameter_read_expr(states, option.sign_value, allocator)
		}
	case ^ast.Message_Stmt:
		routine_flow_form_parameter_read_message_head(states, n.head, allocator)
		for arg in n.with_args {
			routine_flow_form_parameter_read_expr(states, arg, allocator)
		}
		routine_flow_form_parameter_read_expr(states, n.display_like, allocator)
		routine_flow_form_parameter_read_expr(states, n.raising, allocator)
		routine_flow_form_parameter_write_expr(states, n.into, allocator)
	case ^ast.Perform_Stmt:
		routine_flow_form_parameter_read_expr(states, n.form, allocator)
		routine_flow_form_parameter_read_expr(states, n.program, allocator)
		for arg, i in n.tables {
			routine_flow_form_parameter_apply_perform_output_arg_effect(effects, states, n, arg, .Tables, i, allocator)
		}
		for arg in n.using_args {
			routine_flow_form_parameter_read_expr(states, arg, allocator)
		}
		for arg, i in n.changing {
			routine_flow_form_parameter_apply_perform_output_arg_effect(effects, states, n, arg, .Changing, i, allocator)
		}
	case ^ast.Create_Object_Stmt:
		routine_flow_form_parameter_write_expr(states, n.target, allocator)
		routine_flow_form_parameter_read_expr(states, n.type_dynamic_expr, allocator)
		if n.type_clause != nil {
			routine_flow_form_parameter_read_expr(states, n.type_clause.initial_size, allocator)
		}
		for operand in n.operands {
			routine_flow_form_parameter_read_call_args(states, operand, .Exporting, allocator)
		}
	case ^ast.Create_Data_Stmt:
		routine_flow_form_parameter_write_expr(states, n.target, allocator)
		routine_flow_form_parameter_read_expr(states, n.type_dynamic_expr, allocator)
		routine_flow_form_parameter_read_expr(states, n.type_handle, allocator)
		if n.type_clause != nil {
			routine_flow_form_parameter_read_expr(states, n.type_clause.initial_size, allocator)
		}
		for operand in n.operands {
			routine_flow_form_parameter_read_call_args(states, operand, .Exporting, allocator)
		}
	case ^ast.Convert_Time_Stamp_Stmt:
		switch n.kind {
		case .Time_Stamp_To_Date_Time:
			routine_flow_form_parameter_read_expr(states, n.time_stamp, allocator)
			routine_flow_form_parameter_read_expr(states, n.time_zone, allocator)
			routine_flow_form_parameter_write_expr(states, n.date, allocator)
			routine_flow_form_parameter_write_expr(states, n.time, allocator)
			routine_flow_form_parameter_write_expr(states, n.daylight_saving_time, allocator)
		case .Date_Time_To_Time_Stamp:
			routine_flow_form_parameter_read_expr(states, n.date, allocator)
			routine_flow_form_parameter_read_expr(states, n.time, allocator)
			routine_flow_form_parameter_read_expr(states, n.daylight_saving_time, allocator)
			routine_flow_form_parameter_write_expr(states, n.time_stamp, allocator)
			routine_flow_form_parameter_read_expr(states, n.time_zone, allocator)
		}
	case ^ast.Raise_Stmt:
		routine_flow_form_parameter_read_expr(states, n.target, allocator)
		for operand in n.operands {
			routine_flow_form_parameter_read_expr(states, operand, allocator)
		}
	case ^ast.Write_Stmt:
		for operand in n.operands {
			routine_flow_form_parameter_read_expr(states, operand.value, allocator)
			routine_flow_form_parameter_read_expr(states, operand.position, allocator)
			routine_flow_form_parameter_read_expr(states, operand.length, allocator)
		}
	case ^ast.Write_To_Stmt:
		for entry in n.entries {
			routine_flow_form_parameter_read_expr(states, entry.source, allocator)
			routine_flow_form_parameter_write_expr(states, entry.target, allocator)
		}
	case ^ast.Assert_Stmt:
		routine_flow_form_parameter_read_expr(states, n.condition, allocator)
	case ^ast.Check_Stmt:
		routine_flow_form_parameter_read_expr(states, n.condition, allocator)
	case ^ast.Describe_Stmt:
		for entry in n.entries {
			routine_flow_form_parameter_read_expr(states, entry.source, allocator)
			routine_flow_form_parameter_write_expr(states, entry.target, allocator)
		}
	case ^ast.Runtime_Stmt:
		routine_flow_form_parameter_read_expr(states, n.id, allocator)
		routine_flow_form_parameter_read_expr(states, n.value, allocator)
		routine_flow_form_parameter_read_expr(states, n.line, allocator)
		routine_flow_form_parameter_read_expr(states, n.offset, allocator)
		for operand in n.operands {
			routine_flow_form_parameter_read_expr(states, operand, allocator)
		}
		for excluding in n.excluding {
			routine_flow_form_parameter_read_expr(states, excluding, allocator)
		}
		routine_flow_form_parameter_write_expr(states, n.field, allocator)
		routine_flow_form_parameter_write_expr(states, n.target, allocator)
	case ^ast.Import_Stmt:
		for param in n.parameters {
			routine_flow_form_parameter_write_expr(states, param.value, allocator)
		}
		routine_flow_form_parameter_read_data_cluster_medium(states, &n.medium, allocator)
	case ^ast.Export_Stmt:
		for param in n.parameters {
			routine_flow_form_parameter_read_expr(states, param.value, allocator)
		}
		routine_flow_form_parameter_read_data_cluster_medium(states, &n.medium, allocator)
	case ^ast.Bit_Stmt:
		routine_flow_form_parameter_read_expr(states, n.position, allocator)
		if n.kind == .Get {
			routine_flow_form_parameter_read_expr(states, n.source, allocator)
			routine_flow_form_parameter_write_expr(states, n.target, allocator)
		} else {
			routine_flow_form_parameter_read_expr(states, n.target, allocator)
			routine_flow_form_parameter_read_expr(states, n.value, allocator)
			routine_flow_form_parameter_write_expr(states, n.target, allocator)
		}
	case ^ast.Locale_Stmt:
		if n.kind == .Get {
			routine_flow_form_parameter_write_expr(states, n.language, allocator)
			routine_flow_form_parameter_write_expr(states, n.country, allocator)
			routine_flow_form_parameter_write_expr(states, n.modifier, allocator)
		} else {
			routine_flow_form_parameter_read_expr(states, n.language, allocator)
			routine_flow_form_parameter_read_expr(states, n.country, allocator)
			routine_flow_form_parameter_read_expr(states, n.modifier, allocator)
		}
	case ^ast.Set_Cursor_Stmt:
		routine_flow_form_parameter_read_expr(states, n.field, allocator)
		routine_flow_form_parameter_read_expr(states, n.offset, allocator)
		routine_flow_form_parameter_read_expr(states, n.line, allocator)
		routine_flow_form_parameter_read_expr(states, n.column, allocator)
	case ^ast.Dataset_Stmt:
		routine_flow_form_parameter_read_expr(states, n.dataset, allocator)
		switch n.kind {
		case .Open:
			routine_flow_form_parameter_read_expr(states, n.position, allocator)
			routine_flow_form_parameter_read_expr(states, n.code_page, allocator)
			routine_flow_form_parameter_read_expr(states, n.file_type, allocator)
			routine_flow_form_parameter_read_expr(states, n.filter, allocator)
			routine_flow_form_parameter_read_expr(states, n.replacement, allocator)
			routine_flow_form_parameter_write_expr(states, n.message, allocator)
		case .Read:
			routine_flow_form_parameter_write_expr(states, n.target, allocator)
			routine_flow_form_parameter_read_expr(states, n.maximum_length, allocator)
			routine_flow_form_parameter_write_expr(states, n.actual_length, allocator)
			routine_flow_form_parameter_write_expr(states, n.length, allocator)
		case .Transfer:
			routine_flow_form_parameter_read_expr(states, n.source, allocator)
			routine_flow_form_parameter_read_expr(states, n.length, allocator)
		case .Get:
			routine_flow_form_parameter_write_expr(states, n.position, allocator)
			routine_flow_form_parameter_write_expr(states, n.attributes, allocator)
		case .Set:
			routine_flow_form_parameter_read_expr(states, n.position, allocator)
			routine_flow_form_parameter_read_expr(states, n.attributes, allocator)
		case .Truncate:
			routine_flow_form_parameter_read_expr(states, n.position, allocator)
		case .Close, .Delete:
		}
	case ^ast.Report_Stmt:
		switch n.kind {
		case .Read_Report:
			routine_flow_form_parameter_read_expr(states, n.name, allocator)
			routine_flow_form_parameter_write_expr(states, n.source, allocator)
		case .Insert_Report:
			routine_flow_form_parameter_read_expr(states, n.name, allocator)
			routine_flow_form_parameter_read_expr(states, n.source, allocator)
		case .Delete_Report:
			routine_flow_form_parameter_read_expr(states, n.name, allocator)
		case .Report, .Program:
		}
	case ^ast.Textpool_Stmt:
		routine_flow_form_parameter_read_expr(states, n.program, allocator)
		routine_flow_form_parameter_read_expr(states, n.language, allocator)
		if n.kind == .Read {
			routine_flow_form_parameter_write_expr(states, n.table, allocator)
		} else {
			routine_flow_form_parameter_read_expr(states, n.table, allocator)
		}
	case ^ast.Generate_Stmt:
		if n.kind == .Subroutine_Pool {
			routine_flow_form_parameter_read_expr(states, n.source, allocator)
			routine_flow_form_parameter_write_expr(states, n.name, allocator)
			routine_flow_form_parameter_write_expr(states, n.message, allocator)
			routine_flow_form_parameter_write_expr(states, n.line, allocator)
			routine_flow_form_parameter_write_expr(states, n.word, allocator)
			routine_flow_form_parameter_write_expr(states, n.offset, allocator)
		} else {
			routine_flow_form_parameter_read_expr(states, n.program, allocator)
			routine_flow_form_parameter_read_expr(states, n.dynpro, allocator)
		}
	}
}

routine_flow_form_parameter_read_select_query :: proc(
	states: ^[dynamic]Routine_Flow_Form_Parameter_State,
	query: ^ast.Select_Query_Clause,
	allocator: mem.Allocator,
) {
	if query == nil {
		return
	}
	for projection in query.projections {
		routine_flow_form_parameter_read_expr(states, projection, allocator)
	}
	for projection in query.projection_clauses {
		routine_flow_form_parameter_read_expr(states, projection.value, allocator)
	}
	routine_flow_form_parameter_read_expr(states, query.source, allocator)
	if query.source_clause != nil {
		routine_flow_form_parameter_read_expr(states, query.source_clause.source, allocator)
		for join in query.source_clause.joins {
			routine_flow_form_parameter_read_expr(states, join.source, allocator)
			routine_flow_form_parameter_read_expr(states, join.on, allocator)
		}
	}
	routine_flow_form_parameter_read_expr(states, query.where_cond, allocator)
	routine_flow_form_parameter_read_expr(states, query.for_all_entries, allocator)
	for group_by in query.group_by {
		routine_flow_form_parameter_read_expr(states, group_by.value, allocator)
	}
	routine_flow_form_parameter_read_expr(states, query.package_size, allocator)
	routine_flow_form_parameter_read_expr(states, query.up_to_rows, allocator)
}

routine_flow_form_parameter_read_message_head :: proc(
	states: ^[dynamic]Routine_Flow_Form_Parameter_State,
	head: ^ast.Message_Head_Clause,
	allocator: mem.Allocator,
) {
	if head == nil {
		return
	}
	routine_flow_form_parameter_read_expr(states, head.code, allocator)
	routine_flow_form_parameter_read_expr(states, head.id, allocator)
	routine_flow_form_parameter_read_expr(states, head.msg_type, allocator)
	routine_flow_form_parameter_read_expr(states, head.number, allocator)
}

routine_flow_form_parameter_read_data_cluster_medium :: proc(
	states: ^[dynamic]Routine_Flow_Form_Parameter_State,
	medium: ^ast.Data_Cluster_Medium_Clause,
	allocator: mem.Allocator,
) {
	if medium == nil {
		return
	}
	routine_flow_form_parameter_read_expr(states, medium.object, allocator)
	routine_flow_form_parameter_read_expr(states, medium.work_area, allocator)
	routine_flow_form_parameter_read_expr(states, medium.client, allocator)
	routine_flow_form_parameter_read_expr(states, medium.id, allocator)
}

routine_flow_form_parameter_analyze_arithmetic_entry :: proc(
	states: ^[dynamic]Routine_Flow_Form_Parameter_State,
	source: ^ast.Expr,
	target: ^ast.Expr,
	result: ^ast.Expr,
	allocator: mem.Allocator,
) {
	routine_flow_form_parameter_read_expr(states, source, allocator)
	routine_flow_form_parameter_read_expr(states, target, allocator)
	if result != nil {
		routine_flow_form_parameter_write_expr(states, result, allocator)
	} else {
		routine_flow_form_parameter_write_expr(states, target, allocator)
	}
}

routine_flow_form_parameter_apply_call_arg_effect :: proc(
	states: ^[dynamic]Routine_Flow_Form_Parameter_State,
	expr: ^ast.Expr,
	section: ast.Call_Arg_Section_Kind,
	allocator: mem.Allocator,
) {
	#partial switch section {
	case .Changing, .Tables:
		routine_flow_form_parameter_read_expr(states, expr, allocator)
		routine_flow_form_parameter_write_expr(states, expr, allocator)
	case .Importing, .Receiving:
		routine_flow_form_parameter_write_expr(states, expr, allocator)
	case:
		routine_flow_form_parameter_read_expr(states, expr, allocator)
	}
}

routine_flow_form_parameter_apply_perform_output_arg_effect :: proc(
	effects: ^map[Routine_Flow_Form_Parameter_Key]Routine_Flow_Form_Parameter_Effect,
	states: ^[dynamic]Routine_Flow_Form_Parameter_State,
	stmt: ^ast.Perform_Stmt,
	arg: ^ast.Expr,
	section: Perform_Parameter_Section,
	ordinal_in_section: int,
	allocator: mem.Allocator,
) {
	if effect, ok := routine_flow_form_parameter_perform_effect(effects, stmt, section, ordinal_in_section); ok {
		if effect.reads_before_write {
			routine_flow_form_parameter_read_expr(states, arg, allocator)
		}
		if effect.may_write {
			routine_flow_form_parameter_write_expr(states, arg, allocator)
		}
		return
	}
	routine_flow_form_parameter_write_expr(states, arg, allocator)
}

routine_flow_form_parameter_perform_effect :: proc(
	effects: ^map[Routine_Flow_Form_Parameter_Key]Routine_Flow_Form_Parameter_Effect,
	stmt: ^ast.Perform_Stmt,
	section: Perform_Parameter_Section,
	ordinal_in_section: int,
) -> (Routine_Flow_Form_Parameter_Effect, bool) {
	if effects == nil || stmt == nil || stmt.form_kind != .Static || stmt.program != nil {
		return {}, false
	}
	name, _, name_ok := expr_name(stmt.form)
	if !name_ok || name == "" {
		return {}, false
	}
	key := Routine_Flow_Form_Parameter_Key {
		routine_name = utils.to_lower_ascii(name, context.temp_allocator),
		section = section,
		ordinal_in_section = ordinal_in_section,
	}
	effect, ok := effects[key]
	return effect, ok
}

routine_flow_form_parameter_read_expr :: proc(
	states: ^[dynamic]Routine_Flow_Form_Parameter_State,
	expr: ^ast.Expr,
	allocator: mem.Allocator,
) {
	if expr == nil {
		return
	}
	if access, ok := value_access_from_expr(expr, allocator); ok {
		routine_flow_form_parameter_mark_read(states, access.base_name)
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Bad_Expr, ^ast.Literal_Expr, ^ast.Macro_Arg_Ref_Expr, ^ast.Sql_Star_Expr,
	     ^ast.Ident_Expr, ^ast.Data_Inline_Name_Expr, ^ast.Field_Symbol_Inline_Name_Expr, ^ast.Type_Ref_Expr:
		return
	case ^ast.Host_Expr:
		routine_flow_form_parameter_read_expr(states, n.value, allocator)
	case ^ast.Paren_Expr:
		routine_flow_form_parameter_read_expr(states, n.expr, allocator)
	case ^ast.Template_Expr:
		routine_flow_form_parameter_read_expr(states, n.expr, allocator)
	case ^ast.Char_String_Template_Expr:
		for part in n.parts {
			routine_flow_form_parameter_read_expr(states, part, allocator)
		}
	case ^ast.Template_Interpolation_Expr:
		routine_flow_form_parameter_read_expr(states, n.expr, allocator)
		for spec in n.format_specs {
			routine_flow_form_parameter_read_expr(states, spec, allocator)
		}
	case ^ast.Template_Format_Spec_Expr:
		routine_flow_form_parameter_read_expr(states, n.value, allocator)
	case ^ast.Binary_Expr:
		routine_flow_form_parameter_read_expr(states, n.left, allocator)
		routine_flow_form_parameter_read_expr(states, n.right, allocator)
	case ^ast.Unary_Expr:
		routine_flow_form_parameter_read_expr(states, n.expr, allocator)
	case ^ast.Selector_Expr:
		routine_flow_form_parameter_read_expr(states, n.base, allocator)
	case ^ast.Interface_Qualified_Selector_Expr:
		routine_flow_form_parameter_read_expr(states, n.receiver, allocator)
		routine_flow_form_parameter_read_expr(states, n.interface, allocator)
	case ^ast.Substring_Expr:
		routine_flow_form_parameter_read_expr(states, n.base, allocator)
		routine_flow_form_parameter_read_expr(states, n.offset, allocator)
		routine_flow_form_parameter_read_expr(states, n.length, allocator)
	case ^ast.Table_Expr:
		routine_flow_form_parameter_read_expr(states, n.table, allocator)
		for selector in n.selectors {
			routine_flow_form_parameter_read_expr(states, selector, allocator)
		}
	case ^ast.Call_Expr:
		routine_flow_form_parameter_read_expr(states, n.callee, allocator)
		routine_flow_form_parameter_read_call_args(states, n.args, .Exporting, allocator)
	case ^ast.Call_Arg_List_Expr:
		for arg in n.args {
			routine_flow_form_parameter_read_expr(states, arg, allocator)
		}
	case ^ast.Call_Arg_Section_Expr:
		for arg in n.args {
			routine_flow_form_parameter_read_expr(states, arg, allocator)
		}
	case ^ast.Call_Named_Arg_Expr:
		routine_flow_form_parameter_read_expr(states, n.value, allocator)
	case ^ast.Call_Positional_Arg_Expr:
		routine_flow_form_parameter_read_expr(states, n.value, allocator)
	case ^ast.Constructor_Expr:
		for arg in n.args {
			routine_flow_form_parameter_read_expr(states, arg, allocator)
		}
	case ^ast.Is_Predicate_Expr:
		if n.kind != .Assigned {
			routine_flow_form_parameter_read_expr(states, n.subject, allocator)
		}
	case ^ast.Instance_Of_Predicate_Expr:
		routine_flow_form_parameter_read_expr(states, n.subject, allocator)
	case ^ast.Between_Expr:
		routine_flow_form_parameter_read_expr(states, n.subject, allocator)
		routine_flow_form_parameter_read_expr(states, n.low, allocator)
		routine_flow_form_parameter_read_expr(states, n.high, allocator)
	case ^ast.Sql_Call_Expr:
		for arg in n.args {
			routine_flow_form_parameter_read_expr(states, arg, allocator)
		}
	}
}

routine_flow_form_parameter_read_call_args :: proc(
	states: ^[dynamic]Routine_Flow_Form_Parameter_State,
	expr: ^ast.Expr,
	section: ast.Call_Arg_Section_Kind,
	allocator: mem.Allocator,
) {
	if expr == nil {
		return
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Call_Arg_List_Expr:
		for arg in n.args {
			routine_flow_form_parameter_read_call_args(states, arg, section, allocator)
		}
	case ^ast.Call_Arg_Section_Expr:
		for arg in n.args {
			routine_flow_form_parameter_read_call_args(states, arg, n.kind, allocator)
		}
	case ^ast.Call_Named_Arg_Expr:
		routine_flow_form_parameter_apply_call_arg_effect(states, n.value, section, allocator)
	case ^ast.Call_Positional_Arg_Expr:
		routine_flow_form_parameter_apply_call_arg_effect(states, n.value, section, allocator)
	case:
		routine_flow_form_parameter_apply_call_arg_effect(states, expr, section, allocator)
	}
}

routine_flow_form_parameter_write_expr :: proc(
	states: ^[dynamic]Routine_Flow_Form_Parameter_State,
	expr: ^ast.Expr,
	allocator: mem.Allocator,
) {
	if expr == nil {
		return
	}
	if access, ok := value_access_from_expr(expr, allocator); ok {
		routine_flow_form_parameter_mark_write(states, access.base_name)
		return
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Host_Expr:
		routine_flow_form_parameter_write_expr(states, n.value, allocator)
	case ^ast.Paren_Expr:
		routine_flow_form_parameter_write_expr(states, n.expr, allocator)
	case ^ast.Table_Expr:
		routine_flow_form_parameter_read_expr(states, n.table, allocator)
		for selector in n.selectors {
			routine_flow_form_parameter_read_expr(states, selector, allocator)
		}
	case ^ast.Substring_Expr:
		routine_flow_form_parameter_write_expr(states, n.base, allocator)
		routine_flow_form_parameter_read_expr(states, n.offset, allocator)
		routine_flow_form_parameter_read_expr(states, n.length, allocator)
	}
}

routine_flow_form_parameter_mark_read :: proc(
	states: ^[dynamic]Routine_Flow_Form_Parameter_State,
	name: string,
) {
	if name == "" {
		return
	}
	for &state in states^ {
		if !strings.equal_fold(state.name, name) {
			continue
		}
		if !state.written {
			state.effect.reads_before_write = true
		}
	}
}

routine_flow_form_parameter_mark_write :: proc(
	states: ^[dynamic]Routine_Flow_Form_Parameter_State,
	name: string,
) {
	if name == "" {
		return
	}
	for &state in states^ {
		if !strings.equal_fold(state.name, name) {
			continue
		}
		state.effect.may_write = true
		state.written = true
	}
}

routine_flow_form_parameter_states_clone :: proc(
	states: []Routine_Flow_Form_Parameter_State,
	allocator: mem.Allocator,
) -> [dynamic]Routine_Flow_Form_Parameter_State {
	out := make([dynamic]Routine_Flow_Form_Parameter_State, 0, len(states), allocator)
	for state in states {
		append(&out, state)
	}
	return out
}

routine_flow_form_parameter_merge_branch_states :: proc(
	states: ^[dynamic]Routine_Flow_Form_Parameter_State,
	branches: [][dynamic]Routine_Flow_Form_Parameter_State,
) {
	if len(branches) == 0 {
		return
	}
	for i := 0; i < len(states^); i += 1 {
		written := true
		for branch in branches {
			if i >= len(branch) {
				written = false
				continue
			}
			states^[i].effect.reads_before_write = states^[i].effect.reads_before_write || branch[i].effect.reads_before_write
			states^[i].effect.may_write = states^[i].effect.may_write || branch[i].effect.may_write
			written = written && branch[i].written
		}
		states^[i].written = written
	}
}

routine_flow_form_parameter_merge_effects :: proc(
	states: ^[dynamic]Routine_Flow_Form_Parameter_State,
	other: []Routine_Flow_Form_Parameter_State,
) {
	for &state, i in states^ {
		if i >= len(other) {
			continue
		}
		state.effect.reads_before_write = state.effect.reads_before_write || other[i].effect.reads_before_write
		state.effect.may_write = state.effect.may_write || other[i].effect.may_write
	}
}

routine_flow_analyze_global_declarations :: proc(
	parent: ^Routine_Flow_Context,
	stmts: []^ast.Stmt,
	range: semantic.Range,
) {
	global_stmts := make([dynamic]^ast.Stmt, 0, len(stmts), parent.allocator)
	for stmt in stmts {
		if routine_flow_stmt_is_global_declaration_activity(stmt) {
			append(&global_stmts, stmt)
		}
	}
	if len(global_stmts) == 0 {
		return
	}
	ctx := parent^
	ctx.routine_name = "global declarations"
	ctx.routine_range = range
	ctx.global_declarations = true
	ctx.leave_list_processing_exits = false
	ctx.tracked_values = make([dynamic]^semantic.Entity, 0, 4, ctx.allocator)
	ctx.dead_store_untracked_values = make([dynamic]^semantic.Entity, 0, 4, ctx.allocator)
	state := routine_flow_state_make(ctx.allocator)
	_ = routine_flow_analyze_stmt_list(&ctx, global_stmts[:], state)
	routine_flow_dead_store_collect_untracked_stmt_list(&ctx, global_stmts[:])
	routine_flow_dead_store_analyze_stmt_list(&ctx, global_stmts[:], routine_flow_entity_list_make(ctx.allocator))
}

routine_flow_stmt_is_global_declaration_activity :: proc(stmt: ^ast.Stmt) -> bool {
	if stmt == nil {
		return false
	}
	#partial switch _ in stmt.derived_stmt {
	case ^ast.Class_Decl,
	     ^ast.Interface_Decl,
	     ^ast.Method_Decl,
	     ^ast.Form_Decl,
	     ^ast.Function_Decl,
	     ^ast.Module_Decl,
	     ^ast.Event_Block_Stmt,
	     ^ast.Enhancement_Stmt,
	     ^ast.Enhancement_Section_Stmt,
	     ^ast.Test_Seam_Stmt,
	     ^ast.Test_Injection_Stmt:
		return false
	}
	return true
}

routine_flow_visit_stmt_list_for_routines :: proc(ctx: ^Routine_Flow_Context, stmts: []^ast.Stmt) {
	for stmt in stmts {
		routine_flow_visit_stmt_for_routine(ctx, stmt)
	}
}

routine_flow_visit_stmt_for_routine :: proc(ctx: ^Routine_Flow_Context, stmt: ^ast.Stmt) {
	if stmt == nil {
		return
	}
	#partial switch n in stmt.derived_stmt {
	case ^ast.Method_Decl:
		if len(n.body) > 0 {
			routine_flow_analyze_routine(ctx, n.name.text, n.body[:], n.range, false)
		}
	case ^ast.Form_Decl:
		routine_flow_analyze_routine(ctx, n.name.text, n.body[:], n.range, false)
	case ^ast.Function_Decl:
		routine_flow_analyze_routine(ctx, n.name.text, n.body[:], n.range, false)
	case ^ast.Module_Decl:
		routine_flow_analyze_routine(ctx, n.name.text, n.body[:], n.range, false)
	case ^ast.Event_Block_Stmt:
		name := ast.event_block_kind_text(n.kind)
		routine_flow_analyze_routine(ctx, name, n.body[:], n.range, event_block_leave_list_processing_exits(n.kind))
	case ^ast.Class_Decl:
		routine_flow_visit_stmt_list_for_routines(ctx, n.body[:])
	case ^ast.Interface_Decl:
		routine_flow_visit_stmt_list_for_routines(ctx, n.body[:])
	case ^ast.Enhancement_Stmt:
		routine_flow_visit_stmt_list_for_routines(ctx, n.body[:])
	case ^ast.Enhancement_Section_Stmt:
		routine_flow_visit_stmt_list_for_routines(ctx, n.body[:])
	case ^ast.Test_Seam_Stmt:
		routine_flow_visit_stmt_list_for_routines(ctx, n.body[:])
	case ^ast.Test_Injection_Stmt:
		routine_flow_visit_stmt_list_for_routines(ctx, n.body[:])
	}
}

routine_flow_analyze_routine :: proc(
	parent: ^Routine_Flow_Context,
	name: string,
	body: []^ast.Stmt,
	range: semantic.Range,
	leave_list_processing_exits: bool,
) {
	ctx := parent^
	ctx.routine_name = name
	ctx.routine_range = range
	ctx.global_declarations = false
	ctx.leave_list_processing_exits = leave_list_processing_exits
	ctx.tracked_values = make([dynamic]^semantic.Entity, 0, 4, ctx.allocator)
	ctx.dead_store_untracked_values = make([dynamic]^semantic.Entity, 0, 4, ctx.allocator)
	state := routine_flow_state_make(ctx.allocator)
	_ = routine_flow_analyze_stmt_list(&ctx, body, state)
	routine_flow_dead_store_collect_untracked_stmt_list(&ctx, body)
	routine_flow_dead_store_analyze_stmt_list(&ctx, body, routine_flow_entity_list_make(ctx.allocator))
}

routine_flow_state_make :: proc(allocator: mem.Allocator) -> Routine_Flow_State {
	return Routine_Flow_State {
		assigned = make([dynamic]^semantic.Entity, 0, 8, allocator),
		assigned_fields = make([dynamic]Routine_Flow_Field_Assignment, 0, 4, allocator),
		bound = make([dynamic]^semantic.Entity, 0, 4, allocator),
		non_initial = make([dynamic]^semantic.Entity, 0, 2, allocator),
		non_initial_fields = make([dynamic]Routine_Flow_Field_Assignment, 0, 2, allocator),
		last_success_assigned = make([dynamic]^semantic.Entity, 0, 2, allocator),
		last_success_fields = make([dynamic]Routine_Flow_Field_Assignment, 0, 2, allocator),
		last_success_bound = make([dynamic]^semantic.Entity, 0, 2, allocator),
	}
}

routine_flow_state_clone :: proc(state: Routine_Flow_State, allocator: mem.Allocator) -> Routine_Flow_State {
	out := routine_flow_state_make(allocator)
	out.terminated = state.terminated
	routine_flow_entity_list_extend(&out.assigned, state.assigned[:])
	routine_flow_field_list_extend(&out.assigned_fields, state.assigned_fields[:])
	routine_flow_entity_list_extend(&out.bound, state.bound[:])
	routine_flow_entity_list_extend(&out.non_initial, state.non_initial[:])
	routine_flow_field_list_extend(&out.non_initial_fields, state.non_initial_fields[:])
	routine_flow_entity_list_extend(&out.last_success_assigned, state.last_success_assigned[:])
	routine_flow_field_list_extend(&out.last_success_fields, state.last_success_fields[:])
	routine_flow_entity_list_extend(&out.last_success_bound, state.last_success_bound[:])
	return out
}

routine_flow_analyze_stmt_list :: proc(
	ctx: ^Routine_Flow_Context,
	stmts: []^ast.Stmt,
	state: Routine_Flow_State,
) -> Routine_Flow_State {
	current := state
	for stmt in stmts {
		if current.terminated {
			break
		}
		current = routine_flow_analyze_stmt(ctx, stmt, current)
	}
	return current
}

routine_flow_analyze_stmt :: proc(
	ctx: ^Routine_Flow_Context,
	stmt: ^ast.Stmt,
	state: Routine_Flow_State,
) -> Routine_Flow_State {
	if stmt == nil || state.terminated {
		return state
	}
	next := state
	#partial switch n in stmt.derived_stmt {
	case ^ast.Data_Chained_Decl:
		for clause in n.decls {
			if clause.value_clause != nil {
				routine_flow_read_expr(ctx, &next, clause.value_clause.expr)
				routine_flow_mark_decl_range_assigned(ctx, &next, clause.name.range, .Variable)
			}
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Data_Inline_Decl:
		routine_flow_read_expr(ctx, &next, n.expr)
		routine_flow_mark_decl_range_assigned(ctx, &next, n.name.range, .Variable)
		routine_flow_clear_last_success(&next)
	case ^ast.Assign_Stmt:
		routine_flow_read_expr(ctx, &next, n.rhs)
		routine_flow_write_expr(ctx, &next, n.lhs)
		for lhs in n.chain_lhs {
			routine_flow_write_expr(ctx, &next, lhs)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Downcast_Assign_Stmt:
		routine_flow_read_expr(ctx, &next, n.rhs)
		routine_flow_write_expr(ctx, &next, n.lhs)
		routine_flow_clear_last_success(&next)
	case ^ast.Move_Stmt:
		for entry in n.entries {
			routine_flow_read_expr(ctx, &next, entry.source)
			routine_flow_write_expr(ctx, &next, entry.target)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Move_Corresponding_Stmt:
		for entry in n.entries {
			routine_flow_read_expr(ctx, &next, entry.source)
			routine_flow_write_expr(ctx, &next, entry.target)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Add_Stmt:
		for entry in n.entries {
			routine_flow_analyze_arithmetic_entry(ctx, &next, entry.source, entry.target, entry.result)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Subtract_Stmt:
		for entry in n.entries {
			routine_flow_analyze_arithmetic_entry(ctx, &next, entry.source, entry.target, entry.result)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Multiply_Stmt:
		for entry in n.entries {
			routine_flow_analyze_arithmetic_entry(ctx, &next, entry.source, entry.target, entry.result)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Divide_Stmt:
		for entry in n.entries {
			routine_flow_analyze_arithmetic_entry(ctx, &next, entry.source, entry.target, entry.result)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Compute_Stmt:
		for entry in n.entries {
			routine_flow_read_expr(ctx, &next, entry.source)
			routine_flow_write_expr(ctx, &next, entry.target)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Concatenate_Stmt:
		for entry in n.entries {
			for source in entry.sources {
				routine_flow_read_expr(ctx, &next, source)
			}
			routine_flow_read_expr(ctx, &next, entry.separator)
			routine_flow_write_expr(ctx, &next, entry.target)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Split_Stmt:
		for entry in n.entries {
			routine_flow_read_expr(ctx, &next, entry.source)
			routine_flow_read_expr(ctx, &next, entry.separator)
			for target in entry.targets {
				routine_flow_write_expr(ctx, &next, target)
			}
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Condense_Stmt:
		routine_flow_read_expr(ctx, &next, n.target)
		routine_flow_write_expr(ctx, &next, n.target)
		routine_flow_clear_last_success(&next)
	case ^ast.Replace_Stmt:
		routine_flow_read_expr(ctx, &next, n.pattern)
		routine_flow_read_expr(ctx, &next, n.target)
		routine_flow_read_expr(ctx, &next, n.replacement)
		routine_flow_read_expr(ctx, &next, n.section_offset)
		routine_flow_read_expr(ctx, &next, n.section_length)
		routine_flow_write_expr(ctx, &next, n.target)
		routine_flow_clear_last_success(&next)
	case ^ast.Translate_Stmt:
		routine_flow_read_expr(ctx, &next, n.target)
		routine_flow_read_expr(ctx, &next, n.operand)
		routine_flow_write_expr(ctx, &next, n.target)
		routine_flow_clear_last_success(&next)
	case ^ast.Shift_Stmt:
		routine_flow_read_expr(ctx, &next, n.target)
		routine_flow_read_expr(ctx, &next, n.places)
		routine_flow_read_expr(ctx, &next, n.delete_pattern)
		routine_flow_write_expr(ctx, &next, n.target)
		routine_flow_clear_last_success(&next)
	case ^ast.Expr_Stmt:
		routine_flow_read_expr(ctx, &next, n.expr)
		if expr_is_known_no_return_call(ctx.out, n.expr) {
			next.terminated = true
		}
		routine_flow_clear_last_success(&next)
	case ^ast.If_Stmt:
		next = routine_flow_analyze_if_stmt(ctx, n, next)
	case ^ast.Case_Stmt:
		routine_flow_read_expr(ctx, &next, n.expr)
		next = routine_flow_merge_case_stmt(ctx, n, next)
	case ^ast.While_Stmt:
		routine_flow_read_condition_expr(ctx, &next, n.condition)
		body := routine_flow_state_clone(next, ctx.allocator)
		_ = routine_flow_analyze_stmt_list(ctx, n.body[:], body)
		routine_flow_clear_last_success(&next)
	case ^ast.Do_Stmt:
		routine_flow_read_expr(ctx, &next, n.count)
		body := routine_flow_state_clone(next, ctx.allocator)
		_ = routine_flow_analyze_stmt_list(ctx, n.body[:], body)
		routine_flow_clear_last_success(&next)
	case ^ast.Loop_Stmt:
		routine_flow_read_expr(ctx, &next, n.source)
		routine_flow_read_expr(ctx, &next, n.from)
		routine_flow_read_expr(ctx, &next, n.to)
		routine_flow_read_expr(ctx, &next, n.using_key.dynamic_name)
		routine_flow_read_expr(ctx, &next, n.where_cond)
		routine_flow_read_expr(ctx, &next, n.group_by)
		body := routine_flow_state_clone(next, ctx.allocator)
		if n.target_kind == .Assigning {
			routine_flow_bind_expr(ctx, &body, n.target)
		} else {
			routine_flow_write_expr(ctx, &body, n.target)
		}
		if n.group_target_kind == .Assigning {
			routine_flow_bind_expr(ctx, &body, n.group_target)
		} else {
			routine_flow_write_expr(ctx, &body, n.group_target)
		}
		_ = routine_flow_analyze_stmt_list(ctx, n.body[:], body)
		routine_flow_clear_last_success(&next)
	case ^ast.At_Stmt:
		routine_flow_read_expr(ctx, &next, n.expr)
		body := routine_flow_state_clone(next, ctx.allocator)
		_ = routine_flow_analyze_stmt_list(ctx, n.body[:], body)
		routine_flow_clear_last_success(&next)
	case ^ast.Try_Stmt:
		next = routine_flow_merge_try_stmt(ctx, n, next)
	case ^ast.Select_Stmt:
		routine_flow_clear_last_success(&next)
		routine_flow_read_select_query(ctx, &next, &n.query)
		body := routine_flow_state_clone(next, ctx.allocator)
		conditional_success := routine_flow_select_result_target_is_conditional_success(n)
		if n.query.result != nil && n.query.result.target != nil {
			if conditional_success {
				routine_flow_write_expr(ctx, &next, n.query.result.target, conditional = true)
			} else {
				routine_flow_write_expr(ctx, &next, n.query.result.target)
			}
			if len(n.body) > 0 && !n.query.result.table {
				routine_flow_write_expr(ctx, &body, n.query.result.target)
			} else {
				body = routine_flow_state_clone(next, ctx.allocator)
			}
		}
		_ = routine_flow_analyze_stmt_list(ctx, n.body[:], body)
		if conditional_success {
			// Keep the success state for an immediately following sy-subrc guard.
		} else {
			routine_flow_clear_last_success(&next)
		}
	case ^ast.Open_Cursor_Stmt:
		routine_flow_clear_last_success(&next)
		routine_flow_read_select_query(ctx, &next, &n.query)
		routine_flow_write_expr(ctx, &next, n.handle)
		routine_flow_clear_last_success(&next)
	case ^ast.Fetch_Stmt:
		routine_flow_clear_last_success(&next)
		routine_flow_read_expr(ctx, &next, n.handle)
		routine_flow_read_expr(ctx, &next, n.package_size)
		if n.result != nil {
			routine_flow_write_expr(ctx, &next, n.result.target, conditional = true)
		}
	case ^ast.Close_Cursor_Stmt:
		routine_flow_read_expr(ctx, &next, n.handle)
		routine_flow_clear_last_success(&next)
	case ^ast.Dataset_Stmt:
		routine_flow_read_expr(ctx, &next, n.dataset)
		switch n.kind {
		case .Open:
			routine_flow_read_expr(ctx, &next, n.position)
			routine_flow_read_expr(ctx, &next, n.code_page)
			routine_flow_read_expr(ctx, &next, n.file_type)
			routine_flow_read_expr(ctx, &next, n.filter)
			routine_flow_read_expr(ctx, &next, n.replacement)
			routine_flow_write_expr(ctx, &next, n.message)
		case .Read:
			routine_flow_write_expr(ctx, &next, n.target)
			routine_flow_read_expr(ctx, &next, n.maximum_length)
			routine_flow_write_expr(ctx, &next, n.actual_length)
			routine_flow_write_expr(ctx, &next, n.length)
		case .Transfer:
			routine_flow_read_expr(ctx, &next, n.source)
			routine_flow_read_expr(ctx, &next, n.length)
		case .Get:
			routine_flow_write_expr(ctx, &next, n.position)
			routine_flow_write_expr(ctx, &next, n.attributes)
		case .Set:
			routine_flow_read_expr(ctx, &next, n.position)
			routine_flow_read_expr(ctx, &next, n.attributes)
		case .Truncate:
			routine_flow_read_expr(ctx, &next, n.position)
		case .Close, .Delete:
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Read_Table_Stmt:
		routine_flow_analyze_read_table_stmt(ctx, &next, n)
	case ^ast.Assign_Field_Stmt:
		routine_flow_analyze_assign_field_stmt(ctx, &next, n)
	case ^ast.Insert_Stmt:
		routine_flow_read_expr(ctx, &next, n.source)
		routine_flow_read_expr(ctx, &next, n.target)
		routine_flow_read_expr(ctx, &next, n.index)
		for assignment in n.assignments {
			routine_flow_read_expr(ctx, &next, assignment.name)
			routine_flow_read_expr(ctx, &next, assignment.value)
		}
		routine_flow_bind_expr(ctx, &next, n.assigning)
		routine_flow_write_expr(ctx, &next, n.reference_into)
		routine_flow_clear_last_success(&next)
	case ^ast.Append_Stmt:
		routine_flow_read_expr(ctx, &next, n.source)
		routine_flow_read_expr(ctx, &next, n.target)
		routine_flow_bind_expr(ctx, &next, n.assigning)
		routine_flow_write_expr(ctx, &next, n.reference_into)
		routine_flow_clear_last_success(&next)
	case ^ast.Modify_Stmt:
		routine_flow_read_expr(ctx, &next, n.target)
		routine_flow_read_expr(ctx, &next, n.source)
		routine_flow_read_expr(ctx, &next, n.index)
		routine_flow_read_expr(ctx, &next, n.where_cond)
		routine_flow_clear_last_success(&next)
	case ^ast.Sort_Stmt:
		routine_flow_read_expr(ctx, &next, n.target)
		for field in n.fields {
			routine_flow_read_expr(ctx, &next, field.expr)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Update_Stmt:
		routine_flow_read_expr(ctx, &next, n.target)
		routine_flow_read_expr(ctx, &next, n.source)
		for assignment in n.assignments {
			routine_flow_read_expr(ctx, &next, assignment.name)
			routine_flow_read_expr(ctx, &next, assignment.value)
		}
		routine_flow_read_expr(ctx, &next, n.where_cond)
		routine_flow_clear_last_success(&next)
	case ^ast.Delete_Stmt:
		routine_flow_read_expr(ctx, &next, n.target)
		routine_flow_read_expr(ctx, &next, n.source)
		routine_flow_read_expr(ctx, &next, n.index)
		routine_flow_read_expr(ctx, &next, n.where_cond)
		routine_flow_read_expr(ctx, &next, n.using_key.dynamic_name)
		for comparing in n.comparing {
			routine_flow_read_expr(ctx, &next, comparing.expr)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Clear_Stmt:
		for op in n.operands {
			routine_flow_read_expr(ctx, &next, op.value)
			routine_flow_write_expr(ctx, &next, op.target)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Refresh_Stmt:
		for op in n.operands {
			routine_flow_write_expr(ctx, &next, op.target)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Free_Stmt:
		for op in n.operands {
			routine_flow_write_expr(ctx, &next, op.target)
		}
		routine_flow_read_expr(ctx, &next, n.memory_id)
		routine_flow_clear_last_success(&next)
	case ^ast.Unassign_Stmt:
		for op in n.operands {
			if entity := routine_flow_entity_for_expr(ctx, op.target); entity != nil {
				routine_flow_entity_list_remove(&next.bound, entity)
			}
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Find_Stmt:
		routine_flow_read_expr(ctx, &next, n.pattern)
		routine_flow_read_expr(ctx, &next, n.target)
		routine_flow_read_expr(ctx, &next, n.section_offset)
		routine_flow_read_expr(ctx, &next, n.section_length)
		routine_flow_write_expr(ctx, &next, n.match_offset)
		routine_flow_write_expr(ctx, &next, n.match_length)
		routine_flow_write_expr(ctx, &next, n.match_line)
		routine_flow_write_expr(ctx, &next, n.match_count)
		for submatch in n.submatches {
			routine_flow_write_expr(ctx, &next, submatch)
		}
		routine_flow_write_expr(ctx, &next, n.results, conditional = n.occurrence != .All)
		routine_flow_clear_last_success(&next)
	case ^ast.Search_Stmt:
		routine_flow_read_expr(ctx, &next, n.target)
		routine_flow_read_expr(ctx, &next, n.pattern)
		routine_flow_read_expr(ctx, &next, n.starting_at)
		routine_flow_read_expr(ctx, &next, n.ending_at)
		routine_flow_clear_last_success(&next)
	case ^ast.Call_Stmt:
		routine_flow_analyze_call_stmt(ctx, &next, n)
		if call_stmt_is_known_no_return(ctx.out, n) {
			next.terminated = true
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Receive_Results_Stmt:
		routine_flow_read_expr(ctx, &next, n.target)
		for arg in n.named_args {
			if arg.section == .Importing || arg.section == .Receiving || arg.section == .Changing || arg.section == .Tables {
				routine_flow_write_expr(ctx, &next, arg.value)
			} else {
				routine_flow_read_expr(ctx, &next, arg.value)
			}
			routine_flow_read_expr(ctx, &next, arg.message)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Perform_Stmt:
		routine_flow_analyze_perform_stmt(ctx, &next, n)
		routine_flow_clear_last_success(&next)
	case ^ast.Submit_Stmt:
		routine_flow_read_expr(ctx, &next, n.target)
		for option in n.options {
			routine_flow_read_expr(ctx, &next, option.value)
			routine_flow_read_expr(ctx, &next, option.high_value)
			routine_flow_read_expr(ctx, &next, option.sign_value)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Message_Stmt:
		routine_flow_read_message_head(ctx, &next, n.head)
		for arg in n.with_args {
			routine_flow_read_expr(ctx, &next, arg)
		}
		routine_flow_read_expr(ctx, &next, n.display_like)
		routine_flow_read_expr(ctx, &next, n.raising)
		routine_flow_write_expr(ctx, &next, n.into)
		routine_flow_clear_last_success(&next)
	case ^ast.Flow_Stmt:
		if stmt_prevents_fallthrough(ctx.out, stmt, ctx.leave_list_processing_exits) {
			next.terminated = true
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Raise_Stmt:
		routine_flow_read_expr(ctx, &next, n.target)
		for operand in n.operands {
			routine_flow_read_expr(ctx, &next, operand)
		}
		next.terminated = true
		routine_flow_clear_last_success(&next)
	case ^ast.Create_Object_Stmt:
		routine_flow_write_expr(ctx, &next, n.target)
		routine_flow_read_expr(ctx, &next, n.type_dynamic_expr)
		if n.type_clause != nil {
			routine_flow_read_expr(ctx, &next, n.type_clause.initial_size)
		}
		for operand in n.operands {
			routine_flow_analyze_call_argument_expr(ctx, &next, operand, .Exporting)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Create_Data_Stmt:
		routine_flow_write_expr(ctx, &next, n.target)
		routine_flow_read_expr(ctx, &next, n.type_dynamic_expr)
		routine_flow_read_expr(ctx, &next, n.type_handle)
		if n.type_clause != nil {
			routine_flow_read_expr(ctx, &next, n.type_clause.initial_size)
		}
		for operand in n.operands {
			routine_flow_analyze_call_argument_expr(ctx, &next, operand, .Exporting)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Convert_Time_Stamp_Stmt:
		switch n.kind {
		case .Time_Stamp_To_Date_Time:
			routine_flow_read_expr(ctx, &next, n.time_stamp)
			routine_flow_read_expr(ctx, &next, n.time_zone)
			routine_flow_write_expr(ctx, &next, n.date)
			routine_flow_write_expr(ctx, &next, n.time)
			routine_flow_write_expr(ctx, &next, n.daylight_saving_time)
		case .Date_Time_To_Time_Stamp:
			routine_flow_read_expr(ctx, &next, n.date)
			routine_flow_read_expr(ctx, &next, n.time)
			routine_flow_read_expr(ctx, &next, n.daylight_saving_time)
			routine_flow_write_expr(ctx, &next, n.time_stamp)
			routine_flow_read_expr(ctx, &next, n.time_zone)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Write_Stmt:
		for operand in n.operands {
			routine_flow_read_expr(ctx, &next, operand.value)
			routine_flow_read_expr(ctx, &next, operand.position)
			routine_flow_read_expr(ctx, &next, operand.length)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Write_To_Stmt:
		for entry in n.entries {
			routine_flow_read_expr(ctx, &next, entry.source)
			routine_flow_write_expr(ctx, &next, entry.target)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Assert_Stmt:
		routine_flow_read_expr(ctx, &next, n.condition)
		routine_flow_clear_last_success(&next)
	case ^ast.Check_Stmt:
		routine_flow_read_expr(ctx, &next, n.condition)
		routine_flow_clear_last_success(&next)
	case ^ast.Describe_Stmt:
		for entry in n.entries {
			routine_flow_read_expr(ctx, &next, entry.source)
			routine_flow_write_expr(ctx, &next, entry.target)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Runtime_Stmt:
		routine_flow_read_expr(ctx, &next, n.id)
		routine_flow_read_expr(ctx, &next, n.value)
		routine_flow_read_expr(ctx, &next, n.line)
		routine_flow_read_expr(ctx, &next, n.offset)
		for operand in n.operands {
			routine_flow_read_expr(ctx, &next, operand)
		}
		for excluding in n.excluding {
			routine_flow_read_expr(ctx, &next, excluding)
		}
		routine_flow_write_expr(ctx, &next, n.field)
		routine_flow_write_expr(ctx, &next, n.target)
		routine_flow_clear_last_success(&next)
	case ^ast.Import_Stmt:
		routine_flow_clear_last_success(&next)
		for param in n.parameters {
			routine_flow_write_expr(ctx, &next, param.value, conditional = true)
		}
		routine_flow_read_data_cluster_medium(ctx, &next, &n.medium)
	case ^ast.Export_Stmt:
		for param in n.parameters {
			routine_flow_read_expr(ctx, &next, param.value)
		}
		routine_flow_read_data_cluster_medium(ctx, &next, &n.medium)
		routine_flow_clear_last_success(&next)
	case ^ast.Bit_Stmt:
		routine_flow_read_expr(ctx, &next, n.position)
		if n.kind == .Get {
			routine_flow_read_expr(ctx, &next, n.source)
			routine_flow_write_expr(ctx, &next, n.target)
		} else {
			routine_flow_read_expr(ctx, &next, n.target)
			routine_flow_read_expr(ctx, &next, n.value)
			routine_flow_write_expr(ctx, &next, n.target)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Locale_Stmt:
		if n.kind == .Get {
			routine_flow_write_expr(ctx, &next, n.language)
			routine_flow_write_expr(ctx, &next, n.country)
			routine_flow_write_expr(ctx, &next, n.modifier)
		} else {
			routine_flow_read_expr(ctx, &next, n.language)
			routine_flow_read_expr(ctx, &next, n.country)
			routine_flow_read_expr(ctx, &next, n.modifier)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Set_Cursor_Stmt:
		routine_flow_read_expr(ctx, &next, n.field)
		routine_flow_read_expr(ctx, &next, n.offset)
		routine_flow_read_expr(ctx, &next, n.line)
		routine_flow_read_expr(ctx, &next, n.column)
		routine_flow_clear_last_success(&next)
	case ^ast.Report_Stmt:
		switch n.kind {
		case .Read_Report:
			routine_flow_read_expr(ctx, &next, n.name)
			routine_flow_write_expr(ctx, &next, n.source)
		case .Insert_Report:
			routine_flow_read_expr(ctx, &next, n.name)
			routine_flow_read_expr(ctx, &next, n.source)
		case .Delete_Report:
			routine_flow_read_expr(ctx, &next, n.name)
		case .Report, .Program:
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Textpool_Stmt:
		routine_flow_read_expr(ctx, &next, n.program)
		routine_flow_read_expr(ctx, &next, n.language)
		if n.kind == .Read {
			routine_flow_write_expr(ctx, &next, n.table)
		} else {
			routine_flow_read_expr(ctx, &next, n.table)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Generate_Stmt:
		if n.kind == .Subroutine_Pool {
			routine_flow_read_expr(ctx, &next, n.source)
			routine_flow_write_expr(ctx, &next, n.name)
			routine_flow_write_expr(ctx, &next, n.message)
			routine_flow_write_expr(ctx, &next, n.line)
			routine_flow_write_expr(ctx, &next, n.word)
			routine_flow_write_expr(ctx, &next, n.offset)
		} else {
			routine_flow_read_expr(ctx, &next, n.program)
			routine_flow_read_expr(ctx, &next, n.dynpro)
		}
		routine_flow_clear_last_success(&next)
	case ^ast.Class_Decl, ^ast.Interface_Decl, ^ast.Method_Decl, ^ast.Form_Decl, ^ast.Function_Decl, ^ast.Module_Decl, ^ast.Event_Block_Stmt:
		routine_flow_clear_last_success(&next)
	case:
		routine_flow_read_uses_in_range(ctx, &next, stmt.range)
		routine_flow_clear_last_success(&next)
	}
	return next
}

routine_flow_analyze_if_stmt :: proc(
	ctx: ^Routine_Flow_Context,
	stmt: ^ast.If_Stmt,
	state: Routine_Flow_State,
) -> Routine_Flow_State {
	condition_state := state
	routine_flow_read_condition_expr(ctx, &condition_state, stmt.condition)
	true_state, false_state := routine_flow_split_condition(ctx, state, stmt.condition)
	branches := make([dynamic]Routine_Flow_State, 0, 2 + len(stmt.elseif_clauses), ctx.allocator)
	append(&branches, routine_flow_analyze_stmt_list(ctx, stmt.body[:], true_state))
	remaining := false_state
	for clause in stmt.elseif_clauses {
		routine_flow_read_condition_expr(ctx, &remaining, clause.condition)
		clause_true, clause_false := routine_flow_split_condition(ctx, remaining, clause.condition)
		append(&branches, routine_flow_analyze_stmt_list(ctx, clause.body[:], clause_true))
		remaining = clause_false
	}
	if stmt.else_clause != nil {
		append(&branches, routine_flow_analyze_stmt_list(ctx, stmt.else_clause.body[:], remaining))
	} else {
		append(&branches, remaining)
	}
	merged := routine_flow_merge_states(branches[:], ctx.allocator)
	routine_flow_clear_last_success(&merged)
	return merged
}

routine_flow_merge_case_stmt :: proc(
	ctx: ^Routine_Flow_Context,
	stmt: ^ast.Case_Stmt,
	state: Routine_Flow_State,
) -> Routine_Flow_State {
	branches := make([dynamic]Routine_Flow_State, 0, 1 + len(stmt.whens), ctx.allocator)
	if len(stmt.recovery) > 0 {
		append(&branches, routine_flow_analyze_stmt_list(ctx, stmt.recovery[:], routine_flow_state_clone(state, ctx.allocator)))
	}
	read_state := state
	for clause in stmt.whens {
		for operand in clause.operands {
			routine_flow_read_expr(ctx, &read_state, operand)
		}
		append(&branches, routine_flow_analyze_stmt_list(ctx, clause.body[:], routine_flow_state_clone(state, ctx.allocator)))
	}
	append(&branches, routine_flow_state_clone(state, ctx.allocator))
	merged := routine_flow_merge_states(branches[:], ctx.allocator)
	routine_flow_clear_last_success(&merged)
	return merged
}

routine_flow_merge_try_stmt :: proc(
	ctx: ^Routine_Flow_Context,
	stmt: ^ast.Try_Stmt,
	state: Routine_Flow_State,
) -> Routine_Flow_State {
	branches := make([dynamic]Routine_Flow_State, 0, 1 + len(stmt.catches), ctx.allocator)
	append(&branches, routine_flow_analyze_stmt_list(ctx, stmt.body[:], routine_flow_state_clone(state, ctx.allocator)))
	read_state := state
	for clause in stmt.catches {
		for exception in clause.exceptions {
			routine_flow_read_expr(ctx, &read_state, exception)
		}
		catch_state := routine_flow_state_clone(state, ctx.allocator)
		routine_flow_write_expr(ctx, &catch_state, clause.into)
		append(&branches, routine_flow_analyze_stmt_list(ctx, clause.body[:], catch_state))
	}
	merged := routine_flow_merge_states(branches[:], ctx.allocator)
	if stmt.cleanup != nil {
		merged = routine_flow_analyze_stmt_list(ctx, stmt.cleanup.body[:], merged)
	}
	routine_flow_clear_last_success(&merged)
	return merged
}

routine_flow_analyze_read_table_stmt :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	stmt: ^ast.Read_Table_Stmt,
) {
	routine_flow_clear_last_success(state)
	for entry in stmt.entries {
		routine_flow_read_expr(ctx, state, entry.table)
		routine_flow_read_expr(ctx, state, entry.index)
		routine_flow_read_expr(ctx, state, entry.using_key.dynamic_name)
		for key in entry.key_values {
			routine_flow_read_expr(ctx, state, key.dynamic_name)
			routine_flow_read_expr(ctx, state, key.value)
		}
		for comparing in entry.comparing {
			routine_flow_read_expr(ctx, state, comparing)
		}
		routine_flow_write_expr(ctx, state, entry.into, conditional = true)
		routine_flow_bind_expr(ctx, state, entry.assigning, conditional = true)
		routine_flow_write_expr(ctx, state, entry.reference_into, conditional = true)
	}
}

routine_flow_select_result_target_is_conditional_success :: proc(stmt: ^ast.Select_Stmt) -> bool {
	if stmt == nil ||
	   stmt.query.result == nil ||
	   stmt.query.result.target == nil ||
	   stmt.query.result.table {
		return false
	}
	if routine_flow_select_projection_list_is_aggregate(stmt.query) {
		return false
	}
	return true
}

routine_flow_select_projection_list_is_aggregate :: proc(query: ast.Select_Query_Clause) -> bool {
	if len(query.projections) > 0 {
		for projection in query.projections {
			if !routine_flow_select_projection_is_aggregate(projection) {
				return false
			}
		}
		return true
	}
	if len(query.projection_clauses) > 0 {
		for projection in query.projection_clauses {
			if !routine_flow_select_projection_is_aggregate(projection.value) {
				return false
			}
		}
		return true
	}
	return false
}

routine_flow_select_projection_is_aggregate :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Sql_Call_Expr:
		return n.kind == .Aggregate
	case ^ast.Call_Expr:
		if n.callee == nil {
			return false
		}
		if name, ok := n.callee.derived_expr.(^ast.Ident_Expr); ok {
			return routine_flow_select_aggregate_name(name.name)
		}
	case ^ast.Substring_Expr:
		if n.base != nil {
			if name, ok := n.base.derived_expr.(^ast.Ident_Expr); ok {
				return routine_flow_select_aggregate_name(name.name)
			}
		}
	}
	return false
}

routine_flow_select_aggregate_name :: proc(name: string) -> bool {
	return strings.equal_fold(name, "COUNT") ||
	       strings.equal_fold(name, "MAX") ||
	       strings.equal_fold(name, "MIN") ||
	       strings.equal_fold(name, "SUM") ||
	       strings.equal_fold(name, "AVG")
}

routine_flow_analyze_arithmetic_entry :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	source: ^ast.Expr,
	target: ^ast.Expr,
	result: ^ast.Expr,
) {
	routine_flow_read_expr(ctx, state, source)
	routine_flow_read_expr(ctx, state, target)
	if result != nil {
		routine_flow_write_expr(ctx, state, result)
	} else {
		routine_flow_write_expr(ctx, state, target)
	}
}

routine_flow_analyze_assign_field_stmt :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	stmt: ^ast.Assign_Field_Stmt,
) {
	routine_flow_read_expr(ctx, state, stmt.source)
	routine_flow_read_expr(ctx, state, stmt.component)
	routine_flow_read_expr(ctx, state, stmt.structure)
	routine_flow_read_expr(ctx, state, stmt.casting_type)
	routine_flow_read_expr(ctx, state, stmt.casting_decimals)
	conditional := stmt.component != nil || stmt.structure != nil
	routine_flow_bind_expr(ctx, state, stmt.target, conditional = conditional)
	if !conditional {
		routine_flow_clear_last_success(state)
	}
}

routine_flow_analyze_call_stmt :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	stmt: ^ast.Call_Stmt,
) {
	routine_flow_read_expr(ctx, state, stmt.call)
	routine_flow_read_expr(ctx, state, stmt.target)
	routine_flow_read_expr(ctx, state, stmt.function_destination)
	routine_flow_read_expr(ctx, state, stmt.function_task)
	routine_flow_read_expr(ctx, state, stmt.function_end_task_handler)
	routine_flow_read_expr(ctx, state, stmt.function_parameter_table)
	routine_flow_read_expr(ctx, state, stmt.function_exception_table)
	for arg in stmt.named_args {
		if arg.section == .Importing || arg.section == .Receiving || arg.section == .Changing || arg.section == .Tables {
			routine_flow_write_expr(ctx, state, arg.value)
		} else {
			routine_flow_read_expr(ctx, state, arg.value)
		}
		routine_flow_read_expr(ctx, state, arg.message)
	}
	for arg in stmt.transformation_args {
		if arg.kind == .Result {
			routine_flow_write_expr(ctx, state, arg.value)
		} else {
			routine_flow_read_expr(ctx, state, arg.value)
		}
	}
}

routine_flow_analyze_perform_stmt :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	stmt: ^ast.Perform_Stmt,
) {
	if stmt == nil {
		return
	}
	routine_flow_read_expr(ctx, state, stmt.form)
	routine_flow_read_expr(ctx, state, stmt.program)
	for arg, i in stmt.tables {
		routine_flow_analyze_perform_output_arg(ctx, state, stmt, arg, .Tables, i)
	}
	for arg in stmt.using_args {
		routine_flow_read_expr(ctx, state, arg)
	}
	for arg, i in stmt.changing {
		routine_flow_analyze_perform_output_arg(ctx, state, stmt, arg, .Changing, i)
	}
}

routine_flow_analyze_perform_output_arg :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	stmt: ^ast.Perform_Stmt,
	arg: ^ast.Expr,
	section: Perform_Parameter_Section,
	ordinal_in_section: int,
) {
	if effect, ok := routine_flow_perform_parameter_effect(ctx, stmt, section, ordinal_in_section); ok {
		if effect.reads_before_write || !effect.may_write {
			routine_flow_track_expr(ctx, arg)
		}
		if effect.reads_before_write {
			routine_flow_read_expr(ctx, state, arg)
		}
		if effect.may_write {
			routine_flow_write_expr(ctx, state, arg)
		}
		return
	}
	routine_flow_write_expr(ctx, state, arg)
}

routine_flow_track_expr :: proc(ctx: ^Routine_Flow_Context, expr: ^ast.Expr) {
	entity := routine_flow_entity_for_expr(ctx, expr)
	if entity != nil && entity.kind == .Variable {
		routine_flow_entity_list_add(&ctx.tracked_values, entity)
	}
}

routine_flow_perform_parameter_effect :: proc(
	ctx: ^Routine_Flow_Context,
	stmt: ^ast.Perform_Stmt,
	section: Perform_Parameter_Section,
	ordinal_in_section: int,
) -> (Routine_Flow_Form_Parameter_Effect, bool) {
	if ctx == nil || stmt == nil || stmt.form_kind != .Static || stmt.program != nil {
		return {}, false
	}
	name, _, name_ok := expr_name(stmt.form)
	if !name_ok || name == "" {
		return {}, false
	}
	key := Routine_Flow_Form_Parameter_Key {
		routine_name = utils.to_lower_ascii(name, context.temp_allocator),
		section = section,
		ordinal_in_section = ordinal_in_section,
	}
	effect, ok := ctx.form_parameter_effects[key]
	return effect, ok
}

routine_flow_read_select_query :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	query: ^ast.Select_Query_Clause,
) {
	if query == nil {
		return
	}
	for projection in query.projections {
		routine_flow_read_expr(ctx, state, projection)
	}
	for projection in query.projection_clauses {
		routine_flow_read_expr(ctx, state, projection.value)
	}
	routine_flow_read_expr(ctx, state, query.source)
	if query.source_clause != nil {
		routine_flow_read_expr(ctx, state, query.source_clause.source)
		for join in query.source_clause.joins {
			routine_flow_read_expr(ctx, state, join.source)
			routine_flow_read_expr(ctx, state, join.on)
		}
	}
	routine_flow_read_expr(ctx, state, query.where_cond)
	routine_flow_read_expr(ctx, state, query.for_all_entries)
	for group_by in query.group_by {
		routine_flow_read_expr(ctx, state, group_by.value)
	}
	routine_flow_read_expr(ctx, state, query.package_size)
	routine_flow_read_expr(ctx, state, query.up_to_rows)
}

routine_flow_read_message_head :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	head: ^ast.Message_Head_Clause,
) {
	if head == nil {
		return
	}
	routine_flow_read_expr(ctx, state, head.code)
	routine_flow_read_expr(ctx, state, head.id)
	routine_flow_read_expr(ctx, state, head.msg_type)
	routine_flow_read_expr(ctx, state, head.number)
}

routine_flow_read_data_cluster_medium :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	medium: ^ast.Data_Cluster_Medium_Clause,
) {
	if medium == nil {
		return
	}
	routine_flow_read_expr(ctx, state, medium.object)
	routine_flow_read_expr(ctx, state, medium.work_area)
	routine_flow_read_expr(ctx, state, medium.client)
	routine_flow_read_expr(ctx, state, medium.id)
}

routine_flow_read_condition_expr :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	expr: ^ast.Expr,
) {
	routine_flow_read_expr(ctx, state, expr, suppress_definite_assignment = true)
}

routine_flow_split_condition :: proc(
	ctx: ^Routine_Flow_Context,
	state: Routine_Flow_State,
	expr: ^ast.Expr,
) -> (Routine_Flow_State, Routine_Flow_State) {
	true_state := routine_flow_state_clone(state, ctx.allocator)
	false_state := routine_flow_state_clone(state, ctx.allocator)
	routine_flow_apply_condition_refinement(ctx, &true_state, &false_state, expr, state)
	return true_state, false_state
}

routine_flow_apply_condition_refinement :: proc(
	ctx: ^Routine_Flow_Context,
	true_state: ^Routine_Flow_State,
	false_state: ^Routine_Flow_State,
	expr: ^ast.Expr,
	base_state: Routine_Flow_State,
) {
	if expr == nil {
		return
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Paren_Expr:
		routine_flow_apply_condition_refinement(ctx, true_state, false_state, n.expr, base_state)
	case ^ast.Unary_Expr:
		if n.op == .Not {
			routine_flow_apply_condition_refinement(ctx, false_state, true_state, n.expr, base_state)
		}
	case ^ast.Is_Predicate_Expr:
		if n.kind == .Assigned {
			if entity := routine_flow_entity_for_expr(ctx, n.subject); entity != nil && entity.kind == .Field_Symbol {
				if n.negated {
					routine_flow_entity_list_add(&false_state.bound, entity)
				} else {
					routine_flow_entity_list_add(&true_state.bound, entity)
				}
			}
			return
		}
		if n.kind == .Initial && routine_flow_expr_is_sy_subrc(n.subject) {
			if n.negated {
				routine_flow_apply_success_refinement(false_state, base_state)
			} else {
				routine_flow_apply_success_refinement(true_state, base_state)
			}
			return
		}
		if n.kind == .Initial || n.kind == .Bound {
			if entity, access, ok := routine_flow_access_for_expr(ctx, n.subject); ok {
				true_branch_non_initial := (n.kind == .Initial && n.negated) || (n.kind == .Bound && !n.negated)
				target_state := true_state if true_branch_non_initial else false_state
				if field, field_ok := routine_flow_direct_structure_field(ctx, entity, access); field_ok {
					routine_flow_field_list_add(&target_state.non_initial_fields, entity, field)
				} else {
					routine_flow_entity_list_add(&target_state.non_initial, entity)
				}
		}
			return
		}
	case ^ast.Binary_Expr:
		if n.op == .And {
			routine_flow_apply_true_condition_refinement(ctx, true_state, n.left, base_state)
			routine_flow_apply_true_condition_refinement(ctx, true_state, n.right, base_state)
			return
		}
		if n.op == .Or {
			routine_flow_apply_false_condition_refinement(ctx, false_state, n.left, base_state)
			routine_flow_apply_false_condition_refinement(ctx, false_state, n.right, base_state)
			return
		}
		if routine_flow_binary_is_sy_subrc_success(n) {
			routine_flow_apply_success_refinement(true_state, base_state)
			return
		}
		if routine_flow_binary_is_sy_subrc_failure(n) {
			routine_flow_apply_success_refinement(false_state, base_state)
			return
		}
	}
}

routine_flow_apply_true_condition_refinement :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	expr: ^ast.Expr,
	base_state: Routine_Flow_State,
) {
	false_state := routine_flow_state_clone(state^, ctx.allocator)
	routine_flow_apply_condition_refinement(ctx, state, &false_state, expr, base_state)
}

routine_flow_apply_false_condition_refinement :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	expr: ^ast.Expr,
	base_state: Routine_Flow_State,
) {
	true_state := routine_flow_state_clone(state^, ctx.allocator)
	routine_flow_apply_condition_refinement(ctx, &true_state, state, expr, base_state)
}

routine_flow_apply_success_refinement :: proc(
	state: ^Routine_Flow_State,
	base_state: Routine_Flow_State,
) {
	routine_flow_entity_list_extend(&state.assigned, base_state.last_success_assigned[:])
	routine_flow_field_list_extend(&state.assigned_fields, base_state.last_success_fields[:])
	routine_flow_entity_list_extend(&state.bound, base_state.last_success_bound[:])
}

routine_flow_merge_states :: proc(states: []Routine_Flow_State, allocator: mem.Allocator) -> Routine_Flow_State {
	merged := routine_flow_state_make(allocator)
	first_live := true
	live_count := 0
	for state in states {
		if state.terminated {
			continue
		}
		live_count += 1
		if first_live {
			routine_flow_entity_list_extend(&merged.assigned, state.assigned[:])
			routine_flow_field_list_extend(&merged.assigned_fields, state.assigned_fields[:])
			routine_flow_entity_list_extend(&merged.bound, state.bound[:])
			routine_flow_entity_list_extend(&merged.non_initial, state.non_initial[:])
			routine_flow_field_list_extend(&merged.non_initial_fields, state.non_initial_fields[:])
			first_live = false
		} else {
			merged.assigned = routine_flow_entity_list_intersection(merged.assigned[:], state.assigned[:], allocator)
			merged.assigned_fields = routine_flow_field_list_intersection(merged.assigned_fields[:], state.assigned_fields[:], allocator)
			merged.bound = routine_flow_entity_list_intersection(merged.bound[:], state.bound[:], allocator)
			merged.non_initial = routine_flow_entity_list_intersection(merged.non_initial[:], state.non_initial[:], allocator)
			merged.non_initial_fields = routine_flow_field_list_intersection(merged.non_initial_fields[:], state.non_initial_fields[:], allocator)
		}
	}
	merged.terminated = live_count == 0
	return merged
}

routine_flow_read_expr :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	expr: ^ast.Expr,
	suppress_definite_assignment := false,
) {
	if expr == nil {
		return
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Bad_Expr, ^ast.Literal_Expr, ^ast.Macro_Arg_Ref_Expr, ^ast.Sql_Star_Expr:
		return
	case ^ast.Ident_Expr, ^ast.Data_Inline_Name_Expr, ^ast.Field_Symbol_Inline_Name_Expr, ^ast.Type_Ref_Expr:
		routine_flow_read_entity_at_range(ctx, state, expr.range, suppress_definite_assignment)
	case ^ast.Host_Expr:
		routine_flow_read_expr(ctx, state, n.value, suppress_definite_assignment)
	case ^ast.Paren_Expr:
		routine_flow_read_expr(ctx, state, n.expr, suppress_definite_assignment)
	case ^ast.Template_Expr:
		routine_flow_read_expr(ctx, state, n.expr, suppress_definite_assignment)
	case ^ast.Char_String_Template_Expr:
		for part in n.parts {
			routine_flow_read_expr(ctx, state, part, suppress_definite_assignment)
		}
	case ^ast.Template_Interpolation_Expr:
		routine_flow_read_expr(ctx, state, n.expr, suppress_definite_assignment)
		for spec in n.format_specs {
			routine_flow_read_expr(ctx, state, spec, suppress_definite_assignment)
		}
	case ^ast.Template_Format_Spec_Expr:
		routine_flow_read_expr(ctx, state, n.value, suppress_definite_assignment)
	case ^ast.Binary_Expr:
		routine_flow_read_expr(ctx, state, n.left, suppress_definite_assignment)
		routine_flow_read_expr(ctx, state, n.right, suppress_definite_assignment)
	case ^ast.Unary_Expr:
		routine_flow_read_expr(ctx, state, n.expr, suppress_definite_assignment)
	case ^ast.Selector_Expr:
		if entity, access, ok := routine_flow_access_for_expr(ctx, expr); ok {
			if field, field_ok := routine_flow_direct_structure_field(ctx, entity, access); field_ok {
				routine_flow_read_structure_field(ctx, state, entity, field, expr.range, suppress_definite_assignment)
				return
			}
		}
		routine_flow_read_expr(ctx, state, n.base, suppress_definite_assignment)
	case ^ast.Interface_Qualified_Selector_Expr:
		routine_flow_read_expr(ctx, state, n.receiver, suppress_definite_assignment)
		routine_flow_read_expr(ctx, state, n.interface, suppress_definite_assignment)
	case ^ast.Substring_Expr:
		routine_flow_read_expr(ctx, state, n.base, suppress_definite_assignment)
		routine_flow_read_expr(ctx, state, n.offset, suppress_definite_assignment)
		routine_flow_read_expr(ctx, state, n.length, suppress_definite_assignment)
	case ^ast.Table_Expr:
		routine_flow_read_expr(ctx, state, n.table, suppress_definite_assignment)
		for selector in n.selectors {
			routine_flow_read_expr(ctx, state, selector, suppress_definite_assignment)
		}
	case ^ast.Call_Expr:
		routine_flow_analyze_call_expr(ctx, state, n, suppress_definite_assignment)
	case ^ast.Call_Arg_List_Expr:
		for arg in n.args {
			routine_flow_read_expr(ctx, state, arg, suppress_definite_assignment)
		}
	case ^ast.Call_Arg_Section_Expr:
		for arg in n.args {
			routine_flow_read_expr(ctx, state, arg, suppress_definite_assignment)
		}
	case ^ast.Call_Named_Arg_Expr:
		routine_flow_read_expr(ctx, state, n.value, suppress_definite_assignment)
	case ^ast.Call_Positional_Arg_Expr:
		routine_flow_read_expr(ctx, state, n.value, suppress_definite_assignment)
	case ^ast.Constructor_Expr:
		for arg in n.args {
			routine_flow_read_expr(ctx, state, arg, suppress_definite_assignment)
		}
	case ^ast.Constructor_For_Clause_Expr:
		routine_flow_read_constructor_for_clause_expr(ctx, state, n, suppress_definite_assignment)
	case ^ast.Constructor_Where_Clause_Expr:
		routine_flow_read_expr(ctx, state, n.condition, suppress_definite_assignment)
	case ^ast.Is_Predicate_Expr:
		if n.kind != .Assigned {
			routine_flow_read_expr(ctx, state, n.subject, suppress_definite_assignment)
		}
	case ^ast.Instance_Of_Predicate_Expr:
		routine_flow_read_expr(ctx, state, n.subject, suppress_definite_assignment)
	case ^ast.Between_Expr:
		routine_flow_read_expr(ctx, state, n.subject, suppress_definite_assignment)
		routine_flow_read_expr(ctx, state, n.low, suppress_definite_assignment)
		routine_flow_read_expr(ctx, state, n.high, suppress_definite_assignment)
	case ^ast.Sql_Column_Expr:
		routine_flow_read_uses_in_range(ctx, state, expr.range, suppress_definite_assignment)
	case ^ast.Sql_Call_Expr:
		for arg in n.args {
			routine_flow_read_expr(ctx, state, arg, suppress_definite_assignment)
		}
	case:
		routine_flow_read_uses_in_range(ctx, state, expr.range, suppress_definite_assignment)
	}
}

routine_flow_read_constructor_for_clause_expr :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	expr: ^ast.Constructor_For_Clause_Expr,
	suppress_definite_assignment := false,
) {
	if expr == nil {
		return
	}
	switch expr.kind {
	case .For_In:
		routine_flow_read_expr(ctx, state, expr.source, suppress_definite_assignment)
		routine_flow_read_constructor_for_group_source(ctx, state, expr.group_source, suppress_definite_assignment)
		routine_flow_mark_constructor_for_variable_assigned(ctx, state, expr.variable)
		routine_flow_read_expr(ctx, state, expr.where_clause, suppress_definite_assignment)
	case .For_Groups:
		routine_flow_read_expr(ctx, state, expr.source, suppress_definite_assignment)
		routine_flow_mark_constructor_for_variable_assigned(ctx, state, expr.member_variable)
		routine_flow_read_expr(ctx, state, expr.group_by, suppress_definite_assignment)
		routine_flow_mark_constructor_for_variable_assigned(ctx, state, expr.variable)
	case .For_Then_Until,
	     .For_Then_While:
		routine_flow_read_expr(ctx, state, expr.init, suppress_definite_assignment)
		routine_flow_mark_constructor_for_variable_assigned(ctx, state, expr.variable)
		routine_flow_read_expr(ctx, state, expr.then_expr, suppress_definite_assignment)
		routine_flow_read_expr(ctx, state, expr.condition, suppress_definite_assignment)
	}
	for body in expr.body {
		routine_flow_read_expr(ctx, state, body, suppress_definite_assignment)
	}
}

routine_flow_read_constructor_for_group_source :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	name: ast.Token_Text,
	suppress_definite_assignment := false,
) {
	if name.text == "" {
		return
	}
	routine_flow_read_entity_at_range(ctx, state, name.range, suppress_definite_assignment)
}

routine_flow_mark_constructor_for_variable_assigned :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	name: ast.Token_Text,
) {
	if name.text == "" {
		return
	}
	routine_flow_mark_decl_range_assigned(ctx, state, name.range, .Variable)
}

routine_flow_analyze_call_expr :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	expr: ^ast.Call_Expr,
	suppress_definite_assignment := false,
) {
	if expr == nil {
		return
	}
	routine_flow_read_expr(ctx, state, expr.callee, suppress_definite_assignment)
	routine_flow_analyze_call_argument_expr(ctx, state, expr.args, .Exporting, suppress_definite_assignment)
}

routine_flow_analyze_call_argument_expr :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	expr: ^ast.Expr,
	section: ast.Call_Arg_Section_Kind,
	suppress_definite_assignment := false,
) {
	if expr == nil {
		return
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Call_Arg_List_Expr:
		for arg in n.args {
			routine_flow_analyze_call_argument_expr(ctx, state, arg, section, suppress_definite_assignment)
		}
	case ^ast.Call_Arg_Section_Expr:
		for arg in n.args {
			routine_flow_analyze_call_argument_expr(ctx, state, arg, n.kind, suppress_definite_assignment)
		}
	case ^ast.Call_Named_Arg_Expr:
		if routine_flow_call_section_writes_actual(section) {
			routine_flow_write_expr(ctx, state, n.value)
		} else {
			routine_flow_read_expr(ctx, state, n.value, suppress_definite_assignment)
		}
	case ^ast.Call_Positional_Arg_Expr:
		if routine_flow_call_section_writes_actual(section) {
			routine_flow_write_expr(ctx, state, n.value)
		} else {
			routine_flow_read_expr(ctx, state, n.value, suppress_definite_assignment)
		}
	case:
		if routine_flow_call_section_writes_actual(section) {
			routine_flow_write_expr(ctx, state, expr)
		} else {
			routine_flow_read_expr(ctx, state, expr, suppress_definite_assignment)
		}
	}
}

routine_flow_call_section_writes_actual :: proc "contextless" (section: ast.Call_Arg_Section_Kind) -> bool {
	#partial switch section {
	case .Importing, .Receiving, .Changing, .Tables:
		return true
	case:
		return false
	}
}

routine_flow_access_for_expr :: proc(
	ctx: ^Routine_Flow_Context,
	expr: ^ast.Expr,
) -> (
	^semantic.Entity,
	Value_Access,
	bool,
) {
	access, ok := value_access_from_expr(expr, ctx.allocator)
	if !ok {
		return nil, {}, false
	}
	entity := routine_flow_entity_for_access(ctx, access)
	if entity == nil {
		return nil, {}, false
	}
	return entity, access, true
}

routine_flow_entity_for_access :: proc(
	ctx: ^Routine_Flow_Context,
	access: Value_Access,
) -> ^semantic.Entity {
	if use := semantic.semantic_ref_use_at_range(ctx.ref_query, access.base_range); use != nil {
		return use.entity
	}
	if entity := semantic.semantic_decl_entity_with_kind_and_decl_range(ctx.decl_query, .Variable, access.base_range); entity != nil {
		return entity
	}
	if entity := semantic.semantic_decl_entity_with_kind_and_decl_range(ctx.decl_query, .Field_Symbol, access.base_range); entity != nil {
		return entity
	}
	if access.base_name != "" && ctx.out != nil {
		scope := semantic.semantic_query_scope_at_offset(ctx.out.file, access.base_range.start, ctx.ref_query.checker)
		if _, entity, ok := semantic.checker_lookup_declaration_from_scope(scope, .Value, access.base_name); ok {
			return entity
		}
	}
	return nil
}

routine_flow_direct_structure_field :: proc(
	ctx: ^Routine_Flow_Context,
	entity: ^semantic.Entity,
	access: Value_Access,
) -> (string, bool) {
	if entity == nil || len(access.fields) != 1 {
		return "", false
	}
	structure := semantic.checker_type_structure(entity.type)
	if structure == nil {
		return "", false
	}
	field := semantic.semantic_decl_structure_field(ctx.decl_query, structure, access.fields[0])
	if field == nil {
		return "", false
	}
	return field.name, field.name != ""
}

routine_flow_read_uses_in_range :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	range: semantic.Range,
	suppress_definite_assignment := false,
) {
	if range.end <= range.start || ctx.ref_query.checker == nil {
		return
	}
	for use in ctx.ref_query.checker.info.uses {
		if !semantic.semantic_query_use_matches_file(use, ctx.out.file) {
			continue
		}
		use_range := semantic.semantic_entity_use_range(use)
		if use_range.start < range.start || use_range.end > range.end || use_range.end <= use_range.start {
			continue
		}
		routine_flow_read_entity(ctx, state, use.entity, use_range, suppress_definite_assignment)
	}
}

routine_flow_read_entity_at_range :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	range: semantic.Range,
	suppress_definite_assignment := false,
) {
	use := semantic.semantic_ref_use_at_range(ctx.ref_query, range)
	if use == nil {
		return
	}
	routine_flow_read_entity(ctx, state, use.entity, semantic.semantic_entity_use_range(use^), suppress_definite_assignment)
}

routine_flow_read_entity :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	entity: ^semantic.Entity,
	range: semantic.Range,
	suppress_definite_assignment := false,
) {
	if entity == nil {
		return
	}
	if entity.kind == .Field_Symbol {
		if !routine_flow_entity_list_contains(state.bound[:], entity) {
			routine_flow_emit_field_symbol_unbound(ctx, entity, range)
		}
		return
	}
	if entity.kind == .Variable && !suppress_definite_assignment && routine_flow_entity_list_contains(ctx.tracked_values[:], entity) {
		if !routine_flow_value_read_assigned(state, entity) {
			routine_flow_emit_use_before_assignment(ctx, entity, range)
		}
	}
}

routine_flow_read_structure_field :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	entity: ^semantic.Entity,
	field: string,
	range: semantic.Range,
	suppress_definite_assignment := false,
) {
	if entity == nil || field == "" {
		return
	}
	if entity.kind == .Variable && !suppress_definite_assignment && routine_flow_entity_list_contains(ctx.tracked_values[:], entity) {
		if !routine_flow_structure_field_read_assigned(state, entity, field) {
			routine_flow_emit_use_before_assignment(ctx, entity, range)
		}
	}
}

routine_flow_value_read_assigned :: proc(
	state: ^Routine_Flow_State,
	entity: ^semantic.Entity,
) -> bool {
	if entity == nil {
		return false
	}
	return routine_flow_entity_list_contains(state.assigned[:], entity) ||
	       routine_flow_entity_list_contains(state.non_initial[:], entity) ||
	       routine_flow_field_list_has_entity(state.assigned_fields[:], entity) ||
	       routine_flow_value_assigned_on_entry(entity)
}

routine_flow_structure_field_read_assigned :: proc(
	state: ^Routine_Flow_State,
	entity: ^semantic.Entity,
	field: string,
) -> bool {
	if entity == nil || field == "" {
		return false
	}
	return routine_flow_entity_list_contains(state.assigned[:], entity) ||
	       routine_flow_entity_list_contains(state.non_initial[:], entity) ||
	       routine_flow_field_list_contains(state.assigned_fields[:], entity, field) ||
	       routine_flow_field_list_contains(state.non_initial_fields[:], entity, field) ||
	       routine_flow_value_assigned_on_entry(entity)
}

routine_flow_mark_structure_field_assigned :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	entity: ^semantic.Entity,
	field: string,
	conditional: bool,
) {
	if entity == nil || field == "" {
		return
	}
	routine_flow_entity_list_add(&ctx.tracked_values, entity)
	if conditional {
		routine_flow_field_list_add(&state.last_success_fields, entity, field)
		return
	}
	routine_flow_field_list_add(&state.assigned_fields, entity, field)
	if routine_flow_all_direct_structure_fields_assigned(ctx, state, entity) {
		routine_flow_entity_list_add(&state.assigned, entity)
	}
}

routine_flow_all_direct_structure_fields_assigned :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	entity: ^semantic.Entity,
) -> bool {
	if entity == nil {
		return false
	}
	structure := semantic.checker_type_structure(entity.type)
	if structure == nil || len(structure.fields) == 0 {
		return false
	}
	for field in structure.fields {
		if field == nil || field.name == "" {
			return false
		}
		if !routine_flow_field_list_contains(state.assigned_fields[:], entity, field.name) {
			return false
		}
	}
	return true
}

routine_flow_write_expr :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	expr: ^ast.Expr,
	conditional := false,
) {
	entity, access, access_ok := routine_flow_access_for_expr(ctx, expr)
	if access_ok && entity.kind == .Field_Symbol {
		routine_flow_read_entity(ctx, state, entity, access.base_range)
		routine_flow_read_lhs_expr(ctx, state, expr)
		return
	}
	if access_ok && len(access.fields) > 0 {
		if field, field_ok := routine_flow_direct_structure_field(ctx, entity, access); field_ok {
			routine_flow_mark_structure_field_assigned(ctx, state, entity, field, conditional)
			return
		}
	}
	if !access_ok {
		entity = routine_flow_entity_for_expr(ctx, expr)
	}
	if entity == nil {
		if expr != nil {
			routine_flow_read_lhs_expr(ctx, state, expr)
		}
		return
	}
	if entity.kind == .Field_Symbol {
		routine_flow_read_entity(ctx, state, entity, expr.range)
		routine_flow_read_lhs_expr(ctx, state, expr)
		return
	}
	routine_flow_read_lhs_expr(ctx, state, expr)
	if conditional {
		routine_flow_entity_list_add(&ctx.tracked_values, entity)
		routine_flow_entity_list_add(&state.last_success_assigned, entity)
		return
	}
	routine_flow_entity_list_add(&state.assigned, entity)
}

routine_flow_bind_expr :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	expr: ^ast.Expr,
	conditional := false,
) {
	entity := routine_flow_entity_for_expr(ctx, expr)
	if entity == nil || entity.kind != .Field_Symbol {
		return
	}
	routine_flow_bind_entity(state, entity, conditional)
}

routine_flow_bind_entity :: proc(
	state: ^Routine_Flow_State,
	entity: ^semantic.Entity,
	conditional: bool,
) {
	if conditional {
		routine_flow_entity_list_remove(&state.bound, entity)
		routine_flow_entity_list_add(&state.last_success_bound, entity)
		return
	}
	routine_flow_entity_list_add(&state.bound, entity)
}

routine_flow_read_lhs_expr :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	expr: ^ast.Expr,
) {
	if expr == nil {
		return
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Host_Expr:
		routine_flow_read_lhs_expr(ctx, state, n.value)
	case ^ast.Paren_Expr:
		routine_flow_read_lhs_expr(ctx, state, n.expr)
	case ^ast.Selector_Expr:
		routine_flow_read_expr(ctx, state, n.base)
	case ^ast.Table_Expr:
		routine_flow_read_expr(ctx, state, n.table)
		for selector in n.selectors {
			routine_flow_read_expr(ctx, state, selector)
		}
	case ^ast.Substring_Expr:
		routine_flow_read_lhs_expr(ctx, state, n.base)
		routine_flow_read_expr(ctx, state, n.offset)
		routine_flow_read_expr(ctx, state, n.length)
	}
}

routine_flow_entity_for_expr :: proc(
	ctx: ^Routine_Flow_Context,
	expr: ^ast.Expr,
) -> ^semantic.Entity {
	if expr == nil {
		return nil
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Host_Expr:
		return routine_flow_entity_for_expr(ctx, n.value)
	case ^ast.Paren_Expr:
		return routine_flow_entity_for_expr(ctx, n.expr)
	case ^ast.Data_Inline_Name_Expr:
		return semantic.semantic_decl_entity_with_kind_and_decl_range(ctx.decl_query, .Variable, n.name.range)
	case ^ast.Field_Symbol_Inline_Name_Expr:
		return semantic.semantic_decl_entity_with_kind_and_decl_range(ctx.decl_query, .Field_Symbol, n.name.range)
	}
	access, access_ok := value_access_from_expr(expr, ctx.allocator)
	if !access_ok {
		return nil
	}
	return routine_flow_entity_for_access(ctx, access)
}

routine_flow_mark_decl_range_assigned :: proc(
	ctx: ^Routine_Flow_Context,
	state: ^Routine_Flow_State,
	range: semantic.Range,
	kind: semantic.Entity_Kind,
) {
	entity := semantic.semantic_decl_entity_with_kind_and_decl_range(ctx.decl_query, kind, range)
	if entity != nil {
		routine_flow_entity_list_add(&state.assigned, entity)
	}
}

routine_flow_value_assigned_on_entry :: proc(entity: ^semantic.Entity) -> bool {
	if entity == nil {
		return false
	}
	if entity.kind == .Parameter || entity.kind == .Constant || entity.kind == .Enum_Member || .Builtin in entity.flags {
		return true
	}
	if entity.decl_info != nil && entity.decl_info.value_clause != nil {
		return true
	}
	if entity.type != nil && entity.type.kind == .Table {
		return true
	}
	return false
}

routine_flow_expr_is_sy_subrc :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	access, ok := value_access_from_expr(expr, context.temp_allocator)
	if !ok || len(access.fields) == 0 {
		return false
	}
	return(strings.equal_fold(access.base_name, "sy") || strings.equal_fold(access.base_name, "syst")) &&
	       strings.equal_fold(last_field(access), "subrc")
}

routine_flow_binary_is_sy_subrc_success :: proc(expr: ^ast.Binary_Expr) -> bool {
	if expr == nil {
		return false
	}
	return expr.op == .Equal &&
	       ((routine_flow_expr_is_sy_subrc(expr.left) && expr_is_zero_literal(expr.right)) ||
	        (routine_flow_expr_is_sy_subrc(expr.right) && expr_is_zero_literal(expr.left)))
}

routine_flow_binary_is_sy_subrc_failure :: proc(expr: ^ast.Binary_Expr) -> bool {
	if expr == nil {
		return false
	}
	return expr.op == .Not_Equal &&
	       ((routine_flow_expr_is_sy_subrc(expr.left) && expr_is_zero_literal(expr.right)) ||
	        (routine_flow_expr_is_sy_subrc(expr.right) && expr_is_zero_literal(expr.left)))
}

routine_flow_emit_use_before_assignment :: proc(
	ctx: ^Routine_Flow_Context,
	entity: ^semantic.Entity,
	range: semantic.Range,
) {
	if metadata, ok := metadata_for(USE_BEFORE_DEFINITE_ASSIGNMENT); ok {
		builder := strings.builder_make(context.temp_allocator)
		strings.write_byte(&builder, '\'')
		strings.write_string(&builder, entity.name)
		strings.write_string(&builder, "' may be used before definite assignment in routine '")
		strings.write_string(&builder, ctx.routine_name)
		strings.write_byte(&builder, '\'')
		emit_diagnostic(ctx.out, metadata, range, strings.to_string(builder), ctx.policy, ctx.allocator)
	}
}

routine_flow_emit_field_symbol_unbound :: proc(
	ctx: ^Routine_Flow_Context,
	entity: ^semantic.Entity,
	range: semantic.Range,
) {
	if metadata, ok := metadata_for(POSSIBLY_UNBOUND_FIELD_SYMBOL); ok {
		builder := strings.builder_make(context.temp_allocator)
		strings.write_string(&builder, "field symbol '")
		strings.write_string(&builder, entity.name)
		strings.write_string(&builder, "' may be unbound in routine '")
		strings.write_string(&builder, ctx.routine_name)
		strings.write_byte(&builder, '\'')
		emit_diagnostic(ctx.out, metadata, range, strings.to_string(builder), ctx.policy, ctx.allocator)
	}
}

routine_flow_dead_store_analyze_stmt_list :: proc(
	ctx: ^Routine_Flow_Context,
	stmts: []^ast.Stmt,
	live_after: [dynamic]^semantic.Entity,
	emit := true,
) -> [dynamic]^semantic.Entity {
	live := routine_flow_entity_list_clone(live_after[:], ctx.allocator)
	end := routine_flow_reachable_stmt_count(ctx, stmts)
	for i := end; i > 0; {
		i -= 1
		live = routine_flow_dead_store_analyze_stmt(ctx, stmts[i], live, emit)
	}
	return live
}

routine_flow_dead_store_analyze_stmt :: proc(
	ctx: ^Routine_Flow_Context,
	stmt: ^ast.Stmt,
	live_after: [dynamic]^semantic.Entity,
	emit := true,
) -> [dynamic]^semantic.Entity {
	if stmt == nil {
		return routine_flow_entity_list_clone(live_after[:], ctx.allocator)
	}
	#partial switch n in stmt.derived_stmt {
	case ^ast.If_Stmt:
		return routine_flow_dead_store_analyze_if_stmt(ctx, n, live_after, emit)
	case ^ast.Case_Stmt:
		return routine_flow_dead_store_analyze_case_stmt(ctx, n, live_after, emit)
	case ^ast.While_Stmt:
		transfer := routine_flow_dead_store_transfer_make(ctx.allocator)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.condition)
		return routine_flow_dead_store_analyze_loop_body(ctx, n.body[:], live_after, transfer)
	case ^ast.Do_Stmt:
		transfer := routine_flow_dead_store_transfer_make(ctx.allocator)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.count)
		return routine_flow_dead_store_analyze_loop_body(ctx, n.body[:], live_after, transfer)
	case ^ast.Loop_Stmt:
		transfer := routine_flow_dead_store_transfer_make(ctx.allocator)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.source)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.from)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.to)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.using_key.dynamic_name)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.where_cond)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.group_by)
		return routine_flow_dead_store_analyze_loop_body(ctx, n.body[:], live_after, transfer)
	case ^ast.At_Stmt:
		live := routine_flow_entity_list_clone(live_after[:], ctx.allocator)
		body_live := routine_flow_dead_store_analyze_stmt_list(
			ctx,
			n.body[:],
			routine_flow_entity_list_clone(live_after[:], ctx.allocator),
			emit,
		)
		routine_flow_entity_list_extend(&live, body_live[:])
		transfer := routine_flow_dead_store_transfer_make(ctx.allocator)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.expr)
		return routine_flow_dead_store_apply_reads(ctx, live, transfer)
	case ^ast.Try_Stmt:
		return routine_flow_dead_store_analyze_try_stmt(ctx, n, live_after, emit)
	case ^ast.Flow_Stmt:
		if n.kind == .Return || n.kind == .Stop ||
		   (n.kind == .Leave_List_Processing && ctx.leave_list_processing_exits) {
			return routine_flow_entity_list_make(ctx.allocator)
		}
	case ^ast.Raise_Stmt:
		transfer := routine_flow_dead_store_transfer_make(ctx.allocator)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.target)
		for operand in n.operands {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, operand)
		}
		return routine_flow_dead_store_apply_reads(ctx, routine_flow_entity_list_make(ctx.allocator), transfer)
	}
	transfer := routine_flow_dead_store_transfer_for_stmt(ctx, stmt)
	return routine_flow_dead_store_apply_transfer(ctx, live_after, transfer, emit)
}

routine_flow_dead_store_analyze_if_stmt :: proc(
	ctx: ^Routine_Flow_Context,
	stmt: ^ast.If_Stmt,
	live_after: [dynamic]^semantic.Entity,
	emit: bool,
) -> [dynamic]^semantic.Entity {
	live := routine_flow_entity_list_make(ctx.allocator)
	body_live := routine_flow_dead_store_analyze_stmt_list(
		ctx,
		stmt.body[:],
		routine_flow_entity_list_clone(live_after[:], ctx.allocator),
		emit,
	)
	routine_flow_entity_list_extend(&live, body_live[:])
	for clause in stmt.elseif_clauses {
		clause_live := routine_flow_dead_store_analyze_stmt_list(
			ctx,
			clause.body[:],
			routine_flow_entity_list_clone(live_after[:], ctx.allocator),
			emit,
		)
		routine_flow_entity_list_extend(&live, clause_live[:])
	}
	if stmt.else_clause != nil {
		else_live := routine_flow_dead_store_analyze_stmt_list(
			ctx,
			stmt.else_clause.body[:],
			routine_flow_entity_list_clone(live_after[:], ctx.allocator),
			emit,
		)
		routine_flow_entity_list_extend(&live, else_live[:])
	} else {
		routine_flow_entity_list_extend(&live, live_after[:])
	}

	transfer := routine_flow_dead_store_transfer_make(ctx.allocator)
	routine_flow_dead_store_add_read_expr(ctx, &transfer, stmt.condition)
	for clause in stmt.elseif_clauses {
		routine_flow_dead_store_add_read_expr(ctx, &transfer, clause.condition)
	}
	return routine_flow_dead_store_apply_reads(ctx, live, transfer)
}

routine_flow_dead_store_analyze_case_stmt :: proc(
	ctx: ^Routine_Flow_Context,
	stmt: ^ast.Case_Stmt,
	live_after: [dynamic]^semantic.Entity,
	emit: bool,
) -> [dynamic]^semantic.Entity {
	live := routine_flow_entity_list_make(ctx.allocator)
	if len(stmt.recovery) > 0 {
		recovery_live := routine_flow_dead_store_analyze_stmt_list(
			ctx,
			stmt.recovery[:],
			routine_flow_entity_list_clone(live_after[:], ctx.allocator),
			emit,
		)
		routine_flow_entity_list_extend(&live, recovery_live[:])
	}
	for clause in stmt.whens {
		clause_live := routine_flow_dead_store_analyze_stmt_list(
			ctx,
			clause.body[:],
			routine_flow_entity_list_clone(live_after[:], ctx.allocator),
			emit,
		)
		routine_flow_entity_list_extend(&live, clause_live[:])
	}
	routine_flow_entity_list_extend(&live, live_after[:])

	transfer := routine_flow_dead_store_transfer_make(ctx.allocator)
	routine_flow_dead_store_add_read_expr(ctx, &transfer, stmt.expr)
	for clause in stmt.whens {
		for operand in clause.operands {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, operand)
		}
	}
	return routine_flow_dead_store_apply_reads(ctx, live, transfer)
}

routine_flow_dead_store_analyze_try_stmt :: proc(
	ctx: ^Routine_Flow_Context,
	stmt: ^ast.Try_Stmt,
	live_after: [dynamic]^semantic.Entity,
	emit: bool,
) -> [dynamic]^semantic.Entity {
	after_cleanup := routine_flow_entity_list_clone(live_after[:], ctx.allocator)
	if stmt.cleanup != nil {
		after_cleanup = routine_flow_dead_store_analyze_stmt_list(ctx, stmt.cleanup.body[:], after_cleanup, emit)
	}
	live := routine_flow_entity_list_make(ctx.allocator)
	body_live := routine_flow_dead_store_analyze_stmt_list(
		ctx,
		stmt.body[:],
		routine_flow_entity_list_clone(after_cleanup[:], ctx.allocator),
		emit,
	)
	routine_flow_entity_list_extend(&live, body_live[:])
	transfer := routine_flow_dead_store_transfer_make(ctx.allocator)
	for clause in stmt.catches {
		for exception in clause.exceptions {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, exception)
		}
		catch_live := routine_flow_dead_store_analyze_stmt_list(
			ctx,
			clause.body[:],
			routine_flow_entity_list_clone(after_cleanup[:], ctx.allocator),
			emit,
		)
		routine_flow_entity_list_extend(&live, catch_live[:])
	}
	return routine_flow_dead_store_apply_reads(ctx, live, transfer)
}

routine_flow_dead_store_analyze_loop_body :: proc(
	ctx: ^Routine_Flow_Context,
	body: []^ast.Stmt,
	live_after: [dynamic]^semantic.Entity,
	header_transfer: Routine_Flow_Dead_Store_Transfer,
) -> [dynamic]^semantic.Entity {
	header_live := routine_flow_dead_store_apply_reads(
		ctx,
		routine_flow_entity_list_clone(live_after[:], ctx.allocator),
		header_transfer,
	)
	loop_live := routine_flow_entity_list_clone(header_live[:], ctx.allocator)
	for iteration := 0; iteration < 64; iteration += 1 {
		body_live := routine_flow_dead_store_analyze_stmt_list(
			ctx,
			body,
			routine_flow_entity_list_clone(loop_live[:], ctx.allocator),
			false,
		)
		next := routine_flow_entity_list_clone(header_live[:], ctx.allocator)
		routine_flow_entity_list_extend(&next, body_live[:])
		if routine_flow_entity_list_equal_set(loop_live[:], next[:]) {
			break
		}
		loop_live = next
	}
	return loop_live
}

routine_flow_dead_store_transfer_for_stmt :: proc(
	ctx: ^Routine_Flow_Context,
	stmt: ^ast.Stmt,
) -> Routine_Flow_Dead_Store_Transfer {
	transfer := routine_flow_dead_store_transfer_make(ctx.allocator)
	if stmt == nil {
		return transfer
	}
	#partial switch n in stmt.derived_stmt {
	case ^ast.Data_Chained_Decl:
		for clause in n.decls {
			if clause.value_clause != nil {
				routine_flow_dead_store_add_read_expr(ctx, &transfer, clause.value_clause.expr)
				routine_flow_dead_store_add_decl_write(ctx, &transfer, clause.name.range, .Variable)
			}
		}
	case ^ast.Data_Inline_Decl:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.expr)
		routine_flow_dead_store_add_decl_write(ctx, &transfer, n.name.range, .Variable)
	case ^ast.Assign_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.rhs)
		routine_flow_dead_store_add_write_expr(ctx, &transfer, n.lhs)
		for lhs in n.chain_lhs {
			routine_flow_dead_store_add_write_expr(ctx, &transfer, lhs)
		}
	case ^ast.Downcast_Assign_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.rhs)
		routine_flow_dead_store_add_write_expr(ctx, &transfer, n.lhs)
	case ^ast.Move_Stmt:
		for entry in n.entries {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, entry.source)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, entry.target)
		}
	case ^ast.Move_Corresponding_Stmt:
		for entry in n.entries {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, entry.source)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, entry.target)
		}
	case ^ast.Add_Stmt:
		for entry in n.entries {
			routine_flow_dead_store_add_arithmetic_entry(ctx, &transfer, entry.source, entry.target, entry.result)
		}
	case ^ast.Subtract_Stmt:
		for entry in n.entries {
			routine_flow_dead_store_add_arithmetic_entry(ctx, &transfer, entry.source, entry.target, entry.result)
		}
	case ^ast.Multiply_Stmt:
		for entry in n.entries {
			routine_flow_dead_store_add_arithmetic_entry(ctx, &transfer, entry.source, entry.target, entry.result)
		}
	case ^ast.Divide_Stmt:
		for entry in n.entries {
			routine_flow_dead_store_add_arithmetic_entry(ctx, &transfer, entry.source, entry.target, entry.result)
		}
	case ^ast.Compute_Stmt:
		for entry in n.entries {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, entry.source)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, entry.target)
		}
	case ^ast.Concatenate_Stmt:
		for entry in n.entries {
			for source in entry.sources {
				routine_flow_dead_store_add_read_expr(ctx, &transfer, source)
			}
			routine_flow_dead_store_add_read_expr(ctx, &transfer, entry.separator)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, entry.target)
		}
	case ^ast.Split_Stmt:
		for entry in n.entries {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, entry.source)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, entry.separator)
			for target in entry.targets {
				routine_flow_dead_store_add_write_expr(ctx, &transfer, target)
			}
		}
	case ^ast.Condense_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.target)
		routine_flow_dead_store_add_write_expr(ctx, &transfer, n.target)
	case ^ast.Replace_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.pattern)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.target)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.replacement)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.section_offset)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.section_length)
		routine_flow_dead_store_add_write_expr(ctx, &transfer, n.target)
	case ^ast.Translate_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.target)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.operand)
		routine_flow_dead_store_add_write_expr(ctx, &transfer, n.target)
	case ^ast.Shift_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.target)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.places)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.delete_pattern)
		routine_flow_dead_store_add_write_expr(ctx, &transfer, n.target)
	case ^ast.Expr_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.expr)
	case ^ast.Select_Stmt:
		routine_flow_dead_store_add_read_select_query(ctx, &transfer, &n.query)
		if n.query.result != nil {
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.query.result.target)
		}
	case ^ast.Open_Cursor_Stmt:
		routine_flow_dead_store_add_read_select_query(ctx, &transfer, &n.query)
		routine_flow_dead_store_add_write_expr(ctx, &transfer, n.handle)
	case ^ast.Fetch_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.handle)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.package_size)
		if n.result != nil {
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.result.target)
		}
	case ^ast.Close_Cursor_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.handle)
	case ^ast.Dataset_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.dataset)
		switch n.kind {
		case .Open:
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.position)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.code_page)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.file_type)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.filter)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.replacement)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.message)
		case .Read:
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.target)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.maximum_length)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.actual_length)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.length)
		case .Transfer:
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.source)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.length)
		case .Get:
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.position)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.attributes)
		case .Set:
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.position)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.attributes)
		case .Truncate:
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.position)
		case .Close, .Delete:
		}
	case ^ast.Read_Table_Stmt:
		for entry in n.entries {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, entry.table)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, entry.index)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, entry.using_key.dynamic_name)
			for key in entry.key_values {
				routine_flow_dead_store_add_read_expr(ctx, &transfer, key.dynamic_name)
				routine_flow_dead_store_add_read_expr(ctx, &transfer, key.value)
			}
			for comparing in entry.comparing {
				routine_flow_dead_store_add_read_expr(ctx, &transfer, comparing)
			}
			routine_flow_dead_store_add_write_expr(ctx, &transfer, entry.into)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, entry.reference_into)
		}
	case ^ast.Assign_Field_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.source)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.component)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.structure)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.casting_type)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.casting_decimals)
	case ^ast.Append_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.source)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.target)
		routine_flow_dead_store_add_write_expr(ctx, &transfer, n.reference_into)
	case ^ast.Insert_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.source)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.target)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.index)
		for assignment in n.assignments {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, assignment.name)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, assignment.value)
		}
		routine_flow_dead_store_add_write_expr(ctx, &transfer, n.reference_into)
	case ^ast.Modify_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.target)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.source)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.index)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.where_cond)
	case ^ast.Sort_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.target)
		for field in n.fields {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, field.expr)
		}
	case ^ast.Update_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.target)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.source)
		for assignment in n.assignments {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, assignment.name)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, assignment.value)
		}
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.where_cond)
	case ^ast.Delete_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.target)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.source)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.index)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.where_cond)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.using_key.dynamic_name)
		for comparing in n.comparing {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, comparing.expr)
		}
	case ^ast.Clear_Stmt:
		for op in n.operands {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, op.value)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, op.target)
		}
	case ^ast.Refresh_Stmt:
		for op in n.operands {
			routine_flow_dead_store_add_write_expr(ctx, &transfer, op.target)
		}
	case ^ast.Free_Stmt:
		for op in n.operands {
			routine_flow_dead_store_add_write_expr(ctx, &transfer, op.target)
		}
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.memory_id)
	case ^ast.Unassign_Stmt:
		for op in n.operands {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, op.target)
		}
	case ^ast.Find_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.pattern)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.target)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.section_offset)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.section_length)
		routine_flow_dead_store_add_write_expr(ctx, &transfer, n.match_offset)
		routine_flow_dead_store_add_write_expr(ctx, &transfer, n.match_length)
		routine_flow_dead_store_add_write_expr(ctx, &transfer, n.match_line)
		routine_flow_dead_store_add_write_expr(ctx, &transfer, n.match_count)
		for submatch in n.submatches {
			routine_flow_dead_store_add_write_expr(ctx, &transfer, submatch)
		}
		routine_flow_dead_store_add_write_expr(ctx, &transfer, n.results)
	case ^ast.Search_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.target)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.pattern)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.starting_at)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.ending_at)
	case ^ast.Call_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.call)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.target)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.function_destination)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.function_task)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.function_end_task_handler)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.function_parameter_table)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.function_exception_table)
		for arg in n.named_args {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, arg.value)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, arg.message)
		}
		for arg in n.transformation_args {
			if arg.kind == .Result {
				routine_flow_dead_store_add_write_expr(ctx, &transfer, arg.value)
			} else {
				routine_flow_dead_store_add_read_expr(ctx, &transfer, arg.value)
			}
		}
	case ^ast.Receive_Results_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.target)
		for arg in n.named_args {
			if routine_flow_call_section_writes_actual(arg.section) {
				routine_flow_dead_store_add_write_expr(ctx, &transfer, arg.value)
			} else {
				routine_flow_dead_store_add_read_expr(ctx, &transfer, arg.value)
			}
			routine_flow_dead_store_add_read_expr(ctx, &transfer, arg.message)
		}
	case ^ast.Perform_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.form)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.program)
		for arg in n.tables {routine_flow_dead_store_add_read_expr(ctx, &transfer, arg)}
		for arg in n.using_args {routine_flow_dead_store_add_read_expr(ctx, &transfer, arg)}
		for arg in n.changing {routine_flow_dead_store_add_read_expr(ctx, &transfer, arg)}
	case ^ast.Submit_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.target)
		for option in n.options {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, option.value)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, option.high_value)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, option.sign_value)
		}
	case ^ast.Message_Stmt:
		routine_flow_dead_store_add_read_message_head(ctx, &transfer, n.head)
		for arg in n.with_args {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, arg)
		}
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.display_like)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.raising)
		routine_flow_dead_store_add_write_expr(ctx, &transfer, n.into)
	case ^ast.Create_Object_Stmt:
		routine_flow_dead_store_add_write_expr(ctx, &transfer, n.target)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.type_dynamic_expr)
		if n.type_clause != nil {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.type_clause.initial_size)
		}
		for operand in n.operands {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, operand)
		}
	case ^ast.Create_Data_Stmt:
		routine_flow_dead_store_add_write_expr(ctx, &transfer, n.target)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.type_dynamic_expr)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.type_handle)
		if n.type_clause != nil {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.type_clause.initial_size)
		}
		for operand in n.operands {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, operand)
		}
	case ^ast.Convert_Time_Stamp_Stmt:
		switch n.kind {
		case .Time_Stamp_To_Date_Time:
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.time_stamp)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.time_zone)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.date)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.time)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.daylight_saving_time)
		case .Date_Time_To_Time_Stamp:
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.date)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.time)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.daylight_saving_time)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.time_stamp)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.time_zone)
		}
	case ^ast.Write_Stmt:
		for operand in n.operands {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, operand.value)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, operand.position)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, operand.length)
		}
	case ^ast.Write_To_Stmt:
		for entry in n.entries {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, entry.source)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, entry.target)
		}
	case ^ast.Assert_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.condition)
	case ^ast.Check_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.condition)
	case ^ast.Describe_Stmt:
		for entry in n.entries {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, entry.source)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, entry.target)
		}
	case ^ast.Runtime_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.id)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.value)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.line)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.offset)
		for operand in n.operands {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, operand)
		}
		for excluding in n.excluding {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, excluding)
		}
		routine_flow_dead_store_add_write_expr(ctx, &transfer, n.field)
		routine_flow_dead_store_add_write_expr(ctx, &transfer, n.target)
	case ^ast.Import_Stmt:
		for param in n.parameters {
			routine_flow_dead_store_add_write_expr(ctx, &transfer, param.value)
		}
		routine_flow_dead_store_add_read_data_cluster_medium(ctx, &transfer, &n.medium)
	case ^ast.Export_Stmt:
		for param in n.parameters {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, param.value)
		}
		routine_flow_dead_store_add_read_data_cluster_medium(ctx, &transfer, &n.medium)
	case ^ast.Bit_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.position)
		if n.kind == .Get {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.source)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.target)
		} else {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.target)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.value)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.target)
		}
	case ^ast.Locale_Stmt:
		if n.kind == .Get {
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.language)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.country)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.modifier)
		} else {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.language)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.country)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.modifier)
		}
	case ^ast.Set_Cursor_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.field)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.offset)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.line)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.column)
	case ^ast.Report_Stmt:
		switch n.kind {
		case .Read_Report:
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.name)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.source)
		case .Insert_Report:
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.name)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.source)
		case .Delete_Report:
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.name)
		case .Report, .Program:
		}
	case ^ast.Textpool_Stmt:
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.program)
		routine_flow_dead_store_add_read_expr(ctx, &transfer, n.language)
		if n.kind == .Read {
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.table)
		} else {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.table)
		}
	case ^ast.Generate_Stmt:
		if n.kind == .Subroutine_Pool {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.source)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.name)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.message)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.line)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.word)
			routine_flow_dead_store_add_write_expr(ctx, &transfer, n.offset)
		} else {
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.program)
			routine_flow_dead_store_add_read_expr(ctx, &transfer, n.dynpro)
		}
	case:
		routine_flow_dead_store_add_reads_in_range(ctx, &transfer, stmt.range)
	}
	return transfer
}

routine_flow_dead_store_transfer_make :: proc(allocator: mem.Allocator) -> Routine_Flow_Dead_Store_Transfer {
	return Routine_Flow_Dead_Store_Transfer {
		reads = make([dynamic]^semantic.Entity, 0, 4, allocator),
		writes = make([dynamic]Routine_Flow_Dead_Store_Write, 0, 2, allocator),
	}
}

routine_flow_dead_store_apply_transfer :: proc(
	ctx: ^Routine_Flow_Context,
	live_after: [dynamic]^semantic.Entity,
	transfer: Routine_Flow_Dead_Store_Transfer,
	emit: bool,
) -> [dynamic]^semantic.Entity {
	live := routine_flow_entity_list_clone(live_after[:], ctx.allocator)
	for i := len(transfer.writes); i > 0; {
		i -= 1
		write := transfer.writes[i]
		if !routine_flow_dead_store_entity_trackable(ctx, write.entity) {
			continue
		}
		if emit && !routine_flow_entity_list_contains(live[:], write.entity) {
			routine_flow_emit_dead_store(ctx, write.entity, write.range)
		}
		routine_flow_entity_list_remove(&live, write.entity)
	}
	for read in transfer.reads {
		if routine_flow_dead_store_entity_trackable(ctx, read) {
			routine_flow_entity_list_add(&live, read)
		}
	}
	return live
}

routine_flow_dead_store_apply_reads :: proc(
	ctx: ^Routine_Flow_Context,
	live_after: [dynamic]^semantic.Entity,
	transfer: Routine_Flow_Dead_Store_Transfer,
) -> [dynamic]^semantic.Entity {
	live := routine_flow_entity_list_clone(live_after[:], ctx.allocator)
	for read in transfer.reads {
		if routine_flow_dead_store_entity_trackable(ctx, read) {
			routine_flow_entity_list_add(&live, read)
		}
	}
	return live
}

routine_flow_dead_store_add_arithmetic_entry :: proc(
	ctx: ^Routine_Flow_Context,
	transfer: ^Routine_Flow_Dead_Store_Transfer,
	source: ^ast.Expr,
	target: ^ast.Expr,
	result: ^ast.Expr,
) {
	routine_flow_dead_store_add_read_expr(ctx, transfer, source)
	routine_flow_dead_store_add_read_expr(ctx, transfer, target)
	if result != nil {
		routine_flow_dead_store_add_write_expr(ctx, transfer, result)
	} else {
		routine_flow_dead_store_add_write_expr(ctx, transfer, target)
	}
}

routine_flow_dead_store_add_read_expr :: proc(
	ctx: ^Routine_Flow_Context,
	transfer: ^Routine_Flow_Dead_Store_Transfer,
	expr: ^ast.Expr,
) {
	if expr == nil {
		return
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Bad_Expr, ^ast.Literal_Expr, ^ast.Macro_Arg_Ref_Expr, ^ast.Sql_Star_Expr:
		return
	case ^ast.Ident_Expr, ^ast.Data_Inline_Name_Expr, ^ast.Field_Symbol_Inline_Name_Expr, ^ast.Type_Ref_Expr:
		routine_flow_dead_store_add_read_entity_at_range(ctx, transfer, expr.range)
	case ^ast.Host_Expr:
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.value)
	case ^ast.Paren_Expr:
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.expr)
	case ^ast.Template_Expr:
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.expr)
	case ^ast.Char_String_Template_Expr:
		for part in n.parts {
			routine_flow_dead_store_add_read_expr(ctx, transfer, part)
		}
	case ^ast.Template_Interpolation_Expr:
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.expr)
		for spec in n.format_specs {
			routine_flow_dead_store_add_read_expr(ctx, transfer, spec)
		}
	case ^ast.Template_Format_Spec_Expr:
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.value)
	case ^ast.Binary_Expr:
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.left)
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.right)
	case ^ast.Unary_Expr:
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.expr)
	case ^ast.Selector_Expr:
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.base)
	case ^ast.Interface_Qualified_Selector_Expr:
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.receiver)
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.interface)
	case ^ast.Substring_Expr:
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.base)
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.offset)
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.length)
	case ^ast.Table_Expr:
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.table)
		for selector in n.selectors {
			routine_flow_dead_store_add_read_expr(ctx, transfer, selector)
		}
	case ^ast.Call_Expr:
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.callee)
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.args)
	case ^ast.Call_Arg_List_Expr:
		for arg in n.args {
			routine_flow_dead_store_add_read_expr(ctx, transfer, arg)
		}
	case ^ast.Call_Arg_Section_Expr:
		for arg in n.args {
			routine_flow_dead_store_add_read_expr(ctx, transfer, arg)
		}
	case ^ast.Call_Named_Arg_Expr:
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.value)
	case ^ast.Call_Positional_Arg_Expr:
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.value)
	case ^ast.Constructor_Expr:
		for arg in n.args {
			routine_flow_dead_store_add_read_expr(ctx, transfer, arg)
		}
	case ^ast.Is_Predicate_Expr:
		if n.kind != .Assigned {
			routine_flow_dead_store_add_read_expr(ctx, transfer, n.subject)
		}
	case ^ast.Instance_Of_Predicate_Expr:
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.subject)
	case ^ast.Between_Expr:
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.subject)
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.low)
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.high)
	case ^ast.Sql_Column_Expr:
		routine_flow_dead_store_add_reads_in_range(ctx, transfer, expr.range)
	case ^ast.Sql_Call_Expr:
		for arg in n.args {
			routine_flow_dead_store_add_read_expr(ctx, transfer, arg)
		}
	case:
		routine_flow_dead_store_add_reads_in_range(ctx, transfer, expr.range)
	}
}

routine_flow_dead_store_add_read_select_query :: proc(
	ctx: ^Routine_Flow_Context,
	transfer: ^Routine_Flow_Dead_Store_Transfer,
	query: ^ast.Select_Query_Clause,
) {
	if query == nil {
		return
	}
	for projection in query.projections {
		routine_flow_dead_store_add_read_expr(ctx, transfer, projection)
	}
	for projection in query.projection_clauses {
		routine_flow_dead_store_add_read_expr(ctx, transfer, projection.value)
	}
	routine_flow_dead_store_add_read_expr(ctx, transfer, query.source)
	if query.source_clause != nil {
		routine_flow_dead_store_add_read_expr(ctx, transfer, query.source_clause.source)
		for join in query.source_clause.joins {
			routine_flow_dead_store_add_read_expr(ctx, transfer, join.source)
			routine_flow_dead_store_add_read_expr(ctx, transfer, join.on)
		}
	}
	routine_flow_dead_store_add_read_expr(ctx, transfer, query.where_cond)
	routine_flow_dead_store_add_read_expr(ctx, transfer, query.for_all_entries)
	for group_by in query.group_by {
		routine_flow_dead_store_add_read_expr(ctx, transfer, group_by.value)
	}
	routine_flow_dead_store_add_read_expr(ctx, transfer, query.package_size)
	routine_flow_dead_store_add_read_expr(ctx, transfer, query.up_to_rows)
}

routine_flow_dead_store_add_read_message_head :: proc(
	ctx: ^Routine_Flow_Context,
	transfer: ^Routine_Flow_Dead_Store_Transfer,
	head: ^ast.Message_Head_Clause,
) {
	if head == nil {
		return
	}
	routine_flow_dead_store_add_read_expr(ctx, transfer, head.code)
	routine_flow_dead_store_add_read_expr(ctx, transfer, head.id)
	routine_flow_dead_store_add_read_expr(ctx, transfer, head.msg_type)
	routine_flow_dead_store_add_read_expr(ctx, transfer, head.number)
}

routine_flow_dead_store_add_read_data_cluster_medium :: proc(
	ctx: ^Routine_Flow_Context,
	transfer: ^Routine_Flow_Dead_Store_Transfer,
	medium: ^ast.Data_Cluster_Medium_Clause,
) {
	if medium == nil {
		return
	}
	routine_flow_dead_store_add_read_expr(ctx, transfer, medium.object)
	routine_flow_dead_store_add_read_expr(ctx, transfer, medium.work_area)
	routine_flow_dead_store_add_read_expr(ctx, transfer, medium.client)
	routine_flow_dead_store_add_read_expr(ctx, transfer, medium.id)
}

routine_flow_dead_store_add_read_entity_at_range :: proc(
	ctx: ^Routine_Flow_Context,
	transfer: ^Routine_Flow_Dead_Store_Transfer,
	range: semantic.Range,
) {
	use := semantic.semantic_ref_use_at_range(ctx.ref_query, range)
	if use == nil {
		return
	}
	if routine_flow_dead_store_entity_trackable(ctx, use.entity) {
		routine_flow_entity_list_add(&transfer.reads, use.entity)
	}
}

routine_flow_dead_store_add_reads_in_range :: proc(
	ctx: ^Routine_Flow_Context,
	transfer: ^Routine_Flow_Dead_Store_Transfer,
	range: semantic.Range,
) {
	if range.end <= range.start || ctx.ref_query.checker == nil {
		return
	}
	for use in ctx.ref_query.checker.info.uses {
		if !semantic.semantic_query_use_matches_file(use, ctx.out.file) {
			continue
		}
		use_range := semantic.semantic_entity_use_range(use)
		if use_range.start < range.start || use_range.end > range.end || use_range.end <= use_range.start {
			continue
		}
		if routine_flow_dead_store_entity_trackable(ctx, use.entity) {
			routine_flow_entity_list_add(&transfer.reads, use.entity)
		}
	}
}

routine_flow_dead_store_add_write_expr :: proc(
	ctx: ^Routine_Flow_Context,
	transfer: ^Routine_Flow_Dead_Store_Transfer,
	expr: ^ast.Expr,
) {
	if expr == nil {
		return
	}
	routine_flow_dead_store_add_lhs_reads(ctx, transfer, expr)
	if entity, range, ok := routine_flow_dead_store_direct_write(ctx, expr); ok {
		append(&transfer.writes, Routine_Flow_Dead_Store_Write{entity = entity, range = range})
	}
}

routine_flow_dead_store_add_decl_write :: proc(
	ctx: ^Routine_Flow_Context,
	transfer: ^Routine_Flow_Dead_Store_Transfer,
	range: semantic.Range,
	kind: semantic.Entity_Kind,
) {
	entity := semantic.semantic_decl_entity_with_kind_and_decl_range(ctx.decl_query, kind, range)
	if routine_flow_dead_store_entity_trackable(ctx, entity) {
		append(&transfer.writes, Routine_Flow_Dead_Store_Write{entity = entity, range = range})
	}
}

routine_flow_dead_store_direct_write :: proc(
	ctx: ^Routine_Flow_Context,
	expr: ^ast.Expr,
) -> (^semantic.Entity, semantic.Range, bool) {
	if expr == nil {
		return nil, {}, false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Host_Expr:
		return routine_flow_dead_store_direct_write(ctx, n.value)
	case ^ast.Paren_Expr:
		return routine_flow_dead_store_direct_write(ctx, n.expr)
	case ^ast.Data_Inline_Name_Expr:
		entity := semantic.semantic_decl_entity_with_kind_and_decl_range(ctx.decl_query, .Variable, n.name.range)
		return entity, n.name.range, routine_flow_dead_store_entity_trackable(ctx, entity)
	}
	access, access_ok := value_access_from_expr(expr, ctx.allocator)
	if !access_ok || len(access.fields) > 0 {
		return nil, {}, false
	}
	entity: ^semantic.Entity
	if use := semantic.semantic_ref_use_at_range(ctx.ref_query, access.base_range); use != nil {
		entity = use.entity
	} else {
		entity = routine_flow_entity_for_access(ctx, access)
	}
	if !routine_flow_dead_store_entity_trackable(ctx, entity) {
		return nil, {}, false
	}
	return entity, access.base_range, true
}

routine_flow_dead_store_add_lhs_reads :: proc(
	ctx: ^Routine_Flow_Context,
	transfer: ^Routine_Flow_Dead_Store_Transfer,
	expr: ^ast.Expr,
) {
	if expr == nil {
		return
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Host_Expr:
		routine_flow_dead_store_add_lhs_reads(ctx, transfer, n.value)
	case ^ast.Paren_Expr:
		routine_flow_dead_store_add_lhs_reads(ctx, transfer, n.expr)
	case ^ast.Selector_Expr:
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.base)
	case ^ast.Table_Expr:
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.table)
		for selector in n.selectors {
			routine_flow_dead_store_add_read_expr(ctx, transfer, selector)
		}
	case ^ast.Substring_Expr:
		routine_flow_dead_store_add_lhs_reads(ctx, transfer, n.base)
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.offset)
		routine_flow_dead_store_add_read_expr(ctx, transfer, n.length)
	}
}

routine_flow_dead_store_entity_trackable :: proc(
	ctx: ^Routine_Flow_Context,
	entity: ^semantic.Entity,
) -> bool {
	if entity == nil || entity.kind != .Variable || .Builtin in entity.flags {
		return false
	}
	if entity.member_kind != .None || entity.owner != nil {
		return false
	}
	if !ctx.global_declarations && entity.scope != nil && entity.scope.kind == .File {
		return false
	}
	if routine_flow_entity_list_contains(ctx.dead_store_untracked_values[:], entity) {
		return false
	}
	return true
}

routine_flow_emit_dead_store :: proc(
	ctx: ^Routine_Flow_Context,
	entity: ^semantic.Entity,
	range: semantic.Range,
) {
	if metadata, ok := metadata_for(DEAD_STORE); ok {
		builder := strings.builder_make(context.temp_allocator)
		if ctx.global_declarations {
			strings.write_string(&builder, "write to global variable '")
			strings.write_string(&builder, entity.name)
			strings.write_string(&builder, "' is never read in global declarations")
			emit_diagnostic(ctx.out, metadata, range, strings.to_string(builder), ctx.policy, ctx.allocator)
			return
		}
		strings.write_string(&builder, "write to local variable '")
		strings.write_string(&builder, entity.name)
		strings.write_string(&builder, "' is never read in routine '")
		strings.write_string(&builder, ctx.routine_name)
		strings.write_byte(&builder, '\'')
		emit_diagnostic(ctx.out, metadata, range, strings.to_string(builder), ctx.policy, ctx.allocator)
	}
}

routine_flow_dead_store_collect_untracked_stmt_list :: proc(
	ctx: ^Routine_Flow_Context,
	stmts: []^ast.Stmt,
) {
	for stmt in stmts {
		routine_flow_dead_store_collect_untracked_stmt(ctx, stmt)
	}
}

routine_flow_dead_store_collect_untracked_stmt :: proc(
	ctx: ^Routine_Flow_Context,
	stmt: ^ast.Stmt,
) {
	if stmt == nil {
		return
	}
	routine_flow_dead_store_collect_untracked_call_exprs(ctx, stmt)
	#partial switch n in stmt.derived_stmt {
	case ^ast.If_Stmt:
		routine_flow_dead_store_collect_untracked_stmt_list(ctx, n.body[:])
		for clause in n.elseif_clauses {
			routine_flow_dead_store_collect_untracked_stmt_list(ctx, clause.body[:])
		}
		if n.else_clause != nil {
			routine_flow_dead_store_collect_untracked_stmt_list(ctx, n.else_clause.body[:])
		}
	case ^ast.Case_Stmt:
		routine_flow_dead_store_collect_untracked_stmt_list(ctx, n.recovery[:])
		for clause in n.whens {
			routine_flow_dead_store_collect_untracked_stmt_list(ctx, clause.body[:])
		}
	case ^ast.While_Stmt:
		routine_flow_dead_store_collect_untracked_stmt_list(ctx, n.body[:])
	case ^ast.Do_Stmt:
		routine_flow_dead_store_collect_untracked_stmt_list(ctx, n.body[:])
	case ^ast.Loop_Stmt:
		routine_flow_dead_store_collect_untracked_stmt_list(ctx, n.body[:])
	case ^ast.At_Stmt:
		routine_flow_dead_store_collect_untracked_stmt_list(ctx, n.body[:])
	case ^ast.Try_Stmt:
		routine_flow_dead_store_collect_untracked_stmt_list(ctx, n.body[:])
		for clause in n.catches {
			routine_flow_dead_store_collect_untracked_stmt_list(ctx, clause.body[:])
		}
		if n.cleanup != nil {
			routine_flow_dead_store_collect_untracked_stmt_list(ctx, n.cleanup.body[:])
		}
	case ^ast.Call_Stmt:
		for arg in n.named_args {
			if arg.section == .Importing || arg.section == .Receiving || arg.section == .Changing || arg.section == .Tables {
				routine_flow_dead_store_collect_untracked_expr(ctx, arg.value)
			}
		}
	case ^ast.Perform_Stmt:
		for arg in n.tables {routine_flow_dead_store_collect_untracked_expr(ctx, arg)}
		for arg in n.using_args {routine_flow_dead_store_collect_untracked_expr(ctx, arg)}
		for arg in n.changing {routine_flow_dead_store_collect_untracked_expr(ctx, arg)}
	}
}

routine_flow_dead_store_collect_untracked_call_exprs :: proc(
	ctx: ^Routine_Flow_Context,
	node: ^ast.Node,
) {
	if ctx == nil || node == nil {
		return
	}
	data := Routine_Flow_Dead_Store_Untracked_Walk{ctx = ctx}
	visitor := ast.Visitor{visit = routine_flow_dead_store_untracked_visit, data = rawptr(&data)}
	ast.walk(&visitor, node)
}

routine_flow_dead_store_untracked_visit :: proc(v: ^ast.Visitor, node: ^ast.Node) -> ^ast.Visitor {
	if v == nil || node == nil {
		return v
	}
	data := cast(^Routine_Flow_Dead_Store_Untracked_Walk)v.data
	if data == nil || data.ctx == nil {
		return v
	}
	#partial switch n in node.derived {
	case ^ast.Call_Expr:
		routine_flow_dead_store_collect_untracked_call_argument_expr(data.ctx, n.args, .Exporting)
	}
	return v
}

routine_flow_dead_store_collect_untracked_call_argument_expr :: proc(
	ctx: ^Routine_Flow_Context,
	expr: ^ast.Expr,
	section: ast.Call_Arg_Section_Kind,
) {
	if expr == nil {
		return
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Call_Arg_List_Expr:
		for arg in n.args {
			routine_flow_dead_store_collect_untracked_call_argument_expr(ctx, arg, section)
		}
	case ^ast.Call_Arg_Section_Expr:
		for arg in n.args {
			routine_flow_dead_store_collect_untracked_call_argument_expr(ctx, arg, n.kind)
		}
	case ^ast.Call_Named_Arg_Expr:
		if routine_flow_call_section_writes_actual(section) {
			routine_flow_dead_store_collect_untracked_expr(ctx, n.value)
		}
	case ^ast.Call_Positional_Arg_Expr:
		if routine_flow_call_section_writes_actual(section) {
			routine_flow_dead_store_collect_untracked_expr(ctx, n.value)
		}
	case:
		if routine_flow_call_section_writes_actual(section) {
			routine_flow_dead_store_collect_untracked_expr(ctx, expr)
		}
	}
}

routine_flow_dead_store_collect_untracked_expr :: proc(
	ctx: ^Routine_Flow_Context,
	expr: ^ast.Expr,
) {
	if expr == nil {
		return
	}
	routine_flow_dead_store_collect_untracked_range(ctx, expr.range)
}

routine_flow_dead_store_collect_untracked_range :: proc(
	ctx: ^Routine_Flow_Context,
	range: semantic.Range,
) {
	if range.end <= range.start || ctx.ref_query.checker == nil {
		return
	}
	for use in ctx.ref_query.checker.info.uses {
		if !semantic.semantic_query_use_matches_file(use, ctx.out.file) {
			continue
		}
		use_range := semantic.semantic_entity_use_range(use)
		if use_range.start < range.start || use_range.end > range.end || use_range.end <= use_range.start {
			continue
		}
		if use.entity != nil && use.entity.kind == .Variable {
			routine_flow_entity_list_add(&ctx.dead_store_untracked_values, use.entity)
		}
	}
}

routine_flow_reachable_stmt_count :: proc(ctx: ^Routine_Flow_Context, stmts: []^ast.Stmt) -> int {
	for stmt, i in stmts {
		if stmt != nil && stmt_prevents_fallthrough(ctx.out, stmt, ctx.leave_list_processing_exits) {
			return i + 1
		}
	}
	return len(stmts)
}

routine_flow_clear_last_success :: proc(state: ^Routine_Flow_State) {
	clear(&state.last_success_assigned)
	clear(&state.last_success_fields)
	clear(&state.last_success_bound)
}

routine_flow_entity_list_make :: proc(allocator: mem.Allocator) -> [dynamic]^semantic.Entity {
	return make([dynamic]^semantic.Entity, 0, 4, allocator)
}

routine_flow_entity_list_clone :: proc(values: []^semantic.Entity, allocator: mem.Allocator) -> [dynamic]^semantic.Entity {
	out := make([dynamic]^semantic.Entity, 0, len(values), allocator)
	routine_flow_entity_list_extend(&out, values)
	return out
}

routine_flow_entity_list_equal_set :: proc(left: []^semantic.Entity, right: []^semantic.Entity) -> bool {
	if len(left) != len(right) {
		return false
	}
	for entity in left {
		if !routine_flow_entity_list_contains(right, entity) {
			return false
		}
	}
	return true
}

routine_flow_entity_list_contains :: proc(list: []^semantic.Entity, entity: ^semantic.Entity) -> bool {
	if entity == nil {
		return false
	}
	for item in list {
		if item == entity {
			return true
		}
	}
	return false
}

routine_flow_entity_list_add :: proc(list: ^[dynamic]^semantic.Entity, entity: ^semantic.Entity) {
	if entity == nil || routine_flow_entity_list_contains(list[:], entity) {
		return
	}
	append(list, entity)
}

routine_flow_entity_list_extend :: proc(list: ^[dynamic]^semantic.Entity, values: []^semantic.Entity) {
	for value in values {
		routine_flow_entity_list_add(list, value)
	}
}

routine_flow_entity_list_remove :: proc(list: ^[dynamic]^semantic.Entity, entity: ^semantic.Entity) {
	if entity == nil {
		return
	}
	for i := 0; i < len(list^); i += 1 {
		if list^[i] == entity {
			unordered_remove(list, i)
			return
		}
	}
}

routine_flow_entity_list_intersection :: proc(
	left: []^semantic.Entity,
	right: []^semantic.Entity,
	allocator: mem.Allocator,
) -> [dynamic]^semantic.Entity {
	out := make([dynamic]^semantic.Entity, 0, len(left), allocator)
	for entity in left {
		if routine_flow_entity_list_contains(right, entity) {
			append(&out, entity)
		}
	}
	return out
}

routine_flow_field_list_contains :: proc(
	list: []Routine_Flow_Field_Assignment,
	entity: ^semantic.Entity,
	field: string,
) -> bool {
	if entity == nil || field == "" {
		return false
	}
	for item in list {
		if item.entity == entity && strings.equal_fold(item.field, field) {
			return true
		}
	}
	return false
}

routine_flow_field_list_has_entity :: proc(
	list: []Routine_Flow_Field_Assignment,
	entity: ^semantic.Entity,
) -> bool {
	if entity == nil {
		return false
	}
	for item in list {
		if item.entity == entity {
			return true
		}
	}
	return false
}

routine_flow_field_list_add :: proc(
	list: ^[dynamic]Routine_Flow_Field_Assignment,
	entity: ^semantic.Entity,
	field: string,
) {
	if entity == nil || field == "" || routine_flow_field_list_contains(list[:], entity, field) {
		return
	}
	append(list, Routine_Flow_Field_Assignment{entity = entity, field = field})
}

routine_flow_field_list_extend :: proc(
	list: ^[dynamic]Routine_Flow_Field_Assignment,
	values: []Routine_Flow_Field_Assignment,
) {
	for value in values {
		routine_flow_field_list_add(list, value.entity, value.field)
	}
}

routine_flow_field_list_intersection :: proc(
	left: []Routine_Flow_Field_Assignment,
	right: []Routine_Flow_Field_Assignment,
	allocator: mem.Allocator,
) -> [dynamic]Routine_Flow_Field_Assignment {
	out := make([dynamic]Routine_Flow_Field_Assignment, 0, len(left), allocator)
	for item in left {
		if routine_flow_field_list_contains(right, item.entity, item.field) {
			append(&out, item)
		}
	}
	return out
}
