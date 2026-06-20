package abap_frontend_lints

import "src:ast"
import "src:semantic"

import "core:mem"
import "core:strings"

guard_list_with :: proc(values: []string, value: string, allocator: mem.Allocator) -> []string {
	out := make([dynamic]string, 0, len(values) + 1, allocator)
	for existing in values {
		append(&out, existing)
	}
	if !guard_list_contains(out[:], value) {
		append(&out, value)
	}
	return out[:]
}

guard_list_contains :: proc(values: []string, value: string) -> bool {
	for existing in values {
		if strings.equal_fold(existing, value) {
			return true
		}
	}
	return false
}

guard_data_list_with :: proc(values: []Guard_Data, value: Guard_Data, allocator: mem.Allocator) -> []Guard_Data {
	out := make([dynamic]Guard_Data, 0, len(values) + 1, allocator)
	for existing in values {
		append(&out, existing)
	}
	if !guard_data_list_contains(out[:], value.name, value.entity) {
		append(&out, value)
	}
	return out[:]
}

guard_data_list_contains :: proc(values: []Guard_Data, name: string, entity: ^semantic.Entity) -> bool {
	for existing in values {
		if !guard_data_matches(existing, name, entity) {
			continue
		}
		return true
	}
	return false
}

guard_data_matches :: proc(guard: Guard_Data, name: string, entity: ^semantic.Entity) -> bool {
	if !strings.equal_fold(guard.name, name) {
		return false
	}
	return guard.entity == nil || entity == nil || guard.entity == entity
}

non_initial_guard_from_condition :: proc(expr: ^ast.Expr, allocator: mem.Allocator) -> (string, bool) {
	return state_guard_from_condition(expr, .Is_Not_Initial, allocator)
}

initial_guard_from_condition :: proc(expr: ^ast.Expr, allocator: mem.Allocator) -> (string, bool) {
	return state_guard_from_condition(expr, .Is_Initial, allocator)
}

state_guard_from_condition :: proc(
	expr: ^ast.Expr,
	kind: Value_State_Check_Kind,
	allocator: mem.Allocator,
) -> (string, bool) {
	if expr == nil {
		return "", false
	}
	if pred, ok := expr.derived_expr.(^ast.Is_Predicate_Expr); ok && pred.kind == .Initial {
		actual := Value_State_Check_Kind.Is_Not_Initial if pred.negated else .Is_Initial
		if actual != kind {
			return "", false
		}
		if access, access_ok := value_access_from_expr(pred.subject, allocator); access_ok {
			return access.base_name, access.base_name != ""
		}
	}
	if bin, ok := expr.derived_expr.(^ast.Binary_Expr); ok {
		return lines_state_guard_from_binary(bin, kind, allocator)
	}
	return "", false
}

non_initial_guard_data_from_condition :: proc(out: ^Unit_Lints, expr: ^ast.Expr, allocator: mem.Allocator) -> (Guard_Data, bool) {
	return state_guard_data_from_condition(out, expr, .Is_Not_Initial, allocator)
}

initial_guard_data_from_condition :: proc(out: ^Unit_Lints, expr: ^ast.Expr, allocator: mem.Allocator) -> (Guard_Data, bool) {
	return state_guard_data_from_condition(out, expr, .Is_Initial, allocator)
}

state_guard_data_from_condition :: proc(
	out: ^Unit_Lints,
	expr: ^ast.Expr,
	kind: Value_State_Check_Kind,
	allocator: mem.Allocator,
) -> (Guard_Data, bool) {
	access, ok := state_guard_access_from_condition(expr, kind, allocator)
	if !ok || access.base_name == "" {
		return {}, false
	}
	return Guard_Data{name = access.base_name, entity = lint_entity_for_access(out, access)}, true
}

state_guard_access_from_condition :: proc(
	expr: ^ast.Expr,
	kind: Value_State_Check_Kind,
	allocator: mem.Allocator,
) -> (Value_Access, bool) {
	if expr == nil {
		return {}, false
	}
	if pred, ok := expr.derived_expr.(^ast.Is_Predicate_Expr); ok && pred.kind == .Initial {
		actual := Value_State_Check_Kind.Is_Not_Initial if pred.negated else .Is_Initial
		if actual != kind {
			return {}, false
		}
		return value_access_from_expr(pred.subject, allocator)
	}
	if bin, ok := expr.derived_expr.(^ast.Binary_Expr); ok {
		return lines_state_guard_access_from_binary(bin, kind, allocator)
	}
	return {}, false
}

lines_state_guard_from_binary :: proc(
	expr: ^ast.Binary_Expr,
	kind: Value_State_Check_Kind,
	allocator: mem.Allocator,
) -> (string, bool) {
	access, ok := lines_state_guard_access_from_binary(expr, kind, allocator)
	if !ok {
		return "", false
	}
	return access.base_name, access.base_name != ""
}

lines_state_guard_access_from_binary :: proc(
	expr: ^ast.Binary_Expr,
	kind: Value_State_Check_Kind,
	allocator: mem.Allocator,
) -> (Value_Access, bool) {
	if expr == nil {
		return {}, false
	}
	left, left_ok := lines_call_argument(expr.left)
	right, right_ok := lines_call_argument(expr.right)
	actual: Value_State_Check_Kind
	target: ^ast.Expr
	matched := false
	if left_ok && expr_is_zero_literal(expr.right) {
		#partial switch expr.op {
		case .Equal:
			actual = .Is_Initial
			matched = true
		case .Not_Equal, .Greater:
			actual = .Is_Not_Initial
			matched = true
		}
		target = left
	} else if right_ok && expr_is_zero_literal(expr.left) {
		#partial switch expr.op {
		case .Equal:
			actual = .Is_Initial
			matched = true
		case .Not_Equal, .Less:
			actual = .Is_Not_Initial
			matched = true
		}
		target = right
	}
	if !matched || actual != kind {
		return {}, false
	}
	return value_access_from_expr(target, allocator)
}

lines_call_argument :: proc(expr: ^ast.Expr) -> (^ast.Expr, bool) {
	if expr == nil {
		return nil, false
	}
	call, ok := expr.derived_expr.(^ast.Call_Expr)
	if !ok {
		return nil, false
	}
	name, _, name_ok := expr_name(call.callee)
	if !name_ok || !strings.equal_fold(name, "lines") {
		return nil, false
	}
	if call.args == nil {
		return nil, false
	}
	args, args_ok := call.args.derived_expr.(^ast.Call_Arg_List_Expr)
	if !args_ok || len(args.args) != 1 {
		return nil, false
	}
	arg := args.args[0]
	if positional, positional_ok := arg.derived_expr.(^ast.Call_Positional_Arg_Expr); positional_ok {
		return positional.value, positional.value != nil
	}
	if named, named_ok := arg.derived_expr.(^ast.Call_Named_Arg_Expr); named_ok {
		return named.value, named.value != nil
	}
	return arg, arg != nil
}

initial_guard_with_direct_exit :: proc(
	out: ^Unit_Lints,
	stmt: ^ast.Stmt,
	allocator: mem.Allocator,
	leave_list_processing_exits: bool,
) -> (Guard_Data, bool) {
	if stmt == nil {
		return {}, false
	}
	if n, ok := stmt.derived_stmt.(^ast.If_Stmt); ok {
		guard, guard_ok := initial_guard_data_from_condition(out, n.condition, allocator)
		if !guard_ok || len(n.body) == 0 || n.else_clause != nil || len(n.elseif_clauses) > 0 {
			return {}, false
		}
		if stmt_list_directly_terminates(out, n.body[:], leave_list_processing_exits) {
			return guard, true
		}
	}
	return {}, false
}

stmt_list_directly_terminates :: proc(out: ^Unit_Lints, stmts: []^ast.Stmt, leave_list_processing_exits: bool) -> bool {
	for stmt in stmts {
		if stmt == nil {
			continue
		}
		if stmt_directly_terminates(out, stmt, leave_list_processing_exits) {
			return true
		}
	}
	return false
}

stmt_directly_terminates :: proc(out: ^Unit_Lints, stmt: ^ast.Stmt, leave_list_processing_exits: bool) -> bool {
	#partial switch n in stmt.derived_stmt {
	case ^ast.Flow_Stmt:
		return n.kind == .Return ||
		       (n.kind == .Leave_List_Processing && leave_list_processing_exits) ||
		       n.kind == .Stop
	case ^ast.Raise_Stmt:
		return true
	case ^ast.Expr_Stmt:
		return expr_is_known_no_return_call(out, n.expr)
	case ^ast.Call_Stmt:
		return call_stmt_is_known_no_return(out, n)
	}
	return false
}

stmt_list_prevents_fallthrough :: proc(out: ^Unit_Lints, stmts: []^ast.Stmt, leave_list_processing_exits: bool) -> bool {
	for stmt in stmts {
		if stmt == nil {
			continue
		}
		if stmt_prevents_fallthrough(out, stmt, leave_list_processing_exits) {
			return true
		}
	}
	return false
}

stmt_prevents_fallthrough :: proc(out: ^Unit_Lints, stmt: ^ast.Stmt, leave_list_processing_exits: bool) -> bool {
	if stmt == nil {
		return false
	}
	#partial switch n in stmt.derived_stmt {
	case ^ast.Flow_Stmt:
		return n.kind == .Return ||
		       n.kind == .Continue ||
		       n.kind == .Exit ||
		       (n.kind == .Leave_List_Processing && leave_list_processing_exits) ||
		       n.kind == .Stop
	case ^ast.Raise_Stmt:
		return true
	case ^ast.Expr_Stmt:
		return expr_is_known_no_return_call(out, n.expr)
	case ^ast.Call_Stmt:
		return call_stmt_is_known_no_return(out, n)
	case ^ast.If_Stmt:
		if n.else_clause == nil {
			return false
		}
		if !stmt_list_prevents_fallthrough(out, n.body[:], leave_list_processing_exits) {
			return false
		}
		for clause in n.elseif_clauses {
			if !stmt_list_prevents_fallthrough(out, clause.body[:], leave_list_processing_exits) {
				return false
			}
		}
		return stmt_list_prevents_fallthrough(out, n.else_clause.body[:], leave_list_processing_exits)
	case ^ast.Case_Stmt:
		has_others := false
		for clause in n.whens {
			if clause.is_others {
				has_others = true
			}
			if !stmt_list_prevents_fallthrough(out, clause.body[:], leave_list_processing_exits) {
				return false
			}
		}
		return has_others
	}
	return false
}

expr_is_known_no_return_call :: proc(out: ^Unit_Lints, expr: ^ast.Expr) -> bool {
	if out == nil {
		return false
	}
	return expr_is_known_no_return_call_with_summaries(out, expr, out.callable_summaries[:])
}

expr_is_known_no_return_call_with_summaries :: proc(
	out: ^Unit_Lints,
	expr: ^ast.Expr,
	summaries: []Routine_Callable_Summary,
) -> bool {
	if expr == nil {
		return false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Paren_Expr:
		return expr_is_known_no_return_call_with_summaries(out, n.expr, summaries)
	case ^ast.Host_Expr:
		return expr_is_known_no_return_call_with_summaries(out, n.value, summaries)
	case ^ast.Call_Expr:
		return callee_expr_is_known_no_return_with_summaries(out, n.callee, summaries)
	}
	return false
}

callee_expr_is_known_no_return :: proc(out: ^Unit_Lints, callee: ^ast.Expr) -> bool {
	if out == nil {
		return false
	}
	return callee_expr_is_known_no_return_with_summaries(out, callee, out.callable_summaries[:])
}

callee_expr_is_known_no_return_with_summaries :: proc(
	out: ^Unit_Lints,
	callee: ^ast.Expr,
	summaries: []Routine_Callable_Summary,
) -> bool {
	entity := callable_entity_for_callee_expr(out, callee)
	return callable_summary_no_return(summaries, entity)
}

call_stmt_is_known_no_return :: proc(out: ^Unit_Lints, stmt: ^ast.Call_Stmt) -> bool {
	if out == nil {
		return false
	}
	return call_stmt_is_known_no_return_with_summaries(out, stmt, out.callable_summaries[:])
}

call_stmt_is_known_no_return_with_summaries :: proc(
	out: ^Unit_Lints,
	stmt: ^ast.Call_Stmt,
	summaries: []Routine_Callable_Summary,
) -> bool {
	if stmt == nil {
		return false
	}
	return callee_expr_is_known_no_return_with_summaries(out, stmt.call, summaries) ||
	       callee_expr_is_known_no_return_with_summaries(out, stmt.target, summaries)
}

callable_entity_for_callee_expr :: proc(out: ^Unit_Lints, callee: ^ast.Expr) -> ^semantic.Entity {
	if out == nil || out.project == nil || out.checker == nil || out.file == nil || callee == nil || callee.range.end <= callee.range.start {
		return nil
	}
	#partial switch n in callee.derived_expr {
	case ^ast.Paren_Expr:
		return callable_entity_for_callee_expr(out, n.expr)
	case ^ast.Host_Expr:
		return callable_entity_for_callee_expr(out, n.value)
	case ^ast.Template_Expr:
		return callable_entity_for_callee_expr(out, n.expr)
	case ^ast.Selector_Expr:
		if entity := callable_entity_for_callee_expr(out, n.field); entity != nil {
			return entity
		}
	case ^ast.Interface_Qualified_Selector_Expr:
		if entity := callable_entity_for_callee_expr(out, n.member); entity != nil {
			return entity
		}
	case ^ast.Call_Expr:
		return callable_entity_for_callee_expr(out, n.callee)
	}
	query := semantic.semantic_query(out.project, out.checker, out.file)
	use := semantic.semantic_ref_use_at_offset(semantic.semantic_query_refs(query), callee.range.end - 1)
	if use == nil || !callable_entity_kind(use.entity) {
		return nil
	}
	return use.entity
}

callable_entity_kind :: proc(entity: ^semantic.Entity) -> bool {
	if entity == nil {
		return false
	}
	return entity.kind == .Method || entity.kind == .Form || entity.kind == .Module
}

callable_summary_no_return :: proc(summaries: []Routine_Callable_Summary, entity: ^semantic.Entity) -> bool {
	if entity == nil {
		return false
	}
	for summary in summaries {
		if summary.entity == entity {
			return summary.no_return
		}
	}
	return false
}

source_is_dynamic_sql_fragment :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Paren_Expr:
		return true
	case ^ast.Host_Expr:
		return true
	case:
	}
	return false
}
