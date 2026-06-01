#+private
package abap_frontend_semantic_analyze

import "src:ast"
import "src:tokenizer"

import "core:mem"
import "core:strings"

collect_decl_info_facts :: proc(c: ^Collector, scope: Scope_Id, info: Decl_Info) {
	for clause in info.length_clauses {
		collect_expr_refs(c, clause.expr, scope)
	}
	if info.paren_length != nil {
		collect_expr_refs(c, info.paren_length.expr, scope)
	}
	if info.value_clause != nil && !info.value_clause.is_initial {
		collect_expr_refs(c, info.value_clause.expr, scope)
	}
	if info.default_clause != nil {
		collect_expr_refs(c, info.default_clause.expr, scope)
	}
	if info.type_clause != nil {
		collect_expr_refs(c, info.type_clause.initial_size, scope)
	}
	collect_expr_refs(c, info.occurs, scope)
	if info.include_ref != nil {
		collect_expr_refs(c, info.include_ref, scope)
	}
}

collect_expr_list_refs :: proc(c: ^Collector, values: []^ast.Expr, scope: Scope_Id) {
	for value in values {
		collect_expr_refs(c, value, scope)
	}
}

collect_expr_refs :: proc(c: ^Collector, expr: ^ast.Expr, scope: Scope_Id) {
	if expr == nil {
		return
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Literal_Expr:
		fact := type_fact_from_expr(c, expr, scope)
		add_syntax_operand(c.unit, scope, n.range, .Constant, fact)
	case ^ast.Ident_Expr:
		if n.name != "#" {
			add_reference(c, scope, n.name, .Value, .Identifier, n.range)
			add_expression_fact(c, scope, n.range, .Reference, type_fact_from_expr(c, expr, scope))
		}
	case ^ast.Type_Ref_Expr:
		if n.raw_operand {
			collect_raw_operand_refs(c, n, scope)
		}
		return
	case ^ast.Dynamic_Call_Method_Target_Expr:
		collect_dynamic_call_method_target_refs(c, n, scope)
	case ^ast.Host_Expr:
		collect_expr_refs(c, n.value, scope)
	case ^ast.Table_Expr:
		collect_expr_refs(c, n.table, scope)
		collect_expr_list_refs(c, n.selectors[:], scope)
	case ^ast.Selector_Expr:
		collect_selector_expr_refs(c, n, scope, false)
	case ^ast.Interface_Qualified_Selector_Expr:
		collect_interface_qualified_selector_expr_refs(c, n, scope, false)
	case ^ast.Substring_Expr:
		collect_expr_refs(c, n.base, scope)
		collect_expr_refs(c, n.offset, scope)
		collect_expr_refs(c, n.length, scope)
	case ^ast.Call_Expr:
		collect_call_expr_refs(c, n, scope)
	case ^ast.Call_Arg_List_Expr:
		collect_call_arg_list_refs(c, n, scope, Named_Argument_Target{}, expr.range)
	case ^ast.Call_Arg_Section_Expr:
		section, has_section := named_argument_section_from_ast(n.kind)
		for arg in n.args {
			collect_call_arg_expr_refs(
				c,
				arg,
				scope,
				Named_Argument_Target{},
				section,
				has_section,
			)
		}
	case ^ast.Call_Named_Arg_Expr:
		collect_expr_refs(c, n.value, scope)
	case ^ast.Call_Positional_Arg_Expr:
		collect_expr_refs(c, n.value, scope)
	case ^ast.Constructor_Expr:
		collect_constructor_expr_refs(c, n, scope)
	case ^ast.Is_Predicate_Expr:
		collect_expr_refs(c, n.subject, scope)
	case ^ast.Instance_Of_Predicate_Expr:
		collect_expr_refs(c, n.subject, scope)
		collect_type_expr_ref(c, n.type_ref, scope, .Type)
	case ^ast.Between_Expr:
		collect_expr_refs(c, n.subject, scope)
		collect_expr_refs(c, n.low, scope)
		collect_expr_refs(c, n.high, scope)
	case ^ast.Sql_Case_When_Expr:
		collect_expr_refs(c, n.condition, scope)
		collect_expr_refs(c, n.result, scope)
	case ^ast.Sql_Case_Expr:
		collect_expr_refs(c, n.operand, scope)
		collect_expr_list_refs(c, n.whens[:], scope)
		collect_expr_refs(c, n.else_expr, scope)
	case ^ast.Binary_Expr:
		collect_expr_refs(c, n.left, scope)
		collect_expr_refs(c, n.right, scope)
	case ^ast.Unary_Expr:
		collect_expr_refs(c, n.expr, scope)
	case ^ast.Paren_Expr:
		collect_expr_refs(c, n.expr, scope)
	case ^ast.Let_Expr:
		for binding in n.bindings {
			if b, ok := binding.derived_expr.(^ast.Constructor_Let_Binding_Expr); ok {
				declare_name_if_present(c, scope, b.name, .Variable, b.range)
				collect_expr_refs(c, b.value, scope)
			} else {
				collect_expr_refs(c, binding, scope)
			}
		}
		collect_expr_list_refs(c, n.body[:], scope)
	case ^ast.Constructor_Let_Binding_Expr:
		declare_name_if_present(c, scope, n.name, .Variable, n.range)
		collect_expr_refs(c, n.value, scope)
	case ^ast.Constructor_When_Clause_Expr:
		collect_expr_refs(c, n.condition, scope)
		collect_expr_refs(c, n.result, scope)
	case ^ast.Constructor_Else_Clause_Expr:
		collect_expr_refs(c, n.result, scope)
	case ^ast.Constructor_For_Clause_Expr:
		collect_constructor_for_clause_refs(c, n, scope)
	case ^ast.Constructor_Where_Clause_Expr:
		collect_expr_refs(c, n.condition, scope)
	case ^ast.Constructor_Init_Clause_Expr:
		collect_expr_list_refs(c, n.assignments[:], scope)
	case ^ast.Constructor_Next_Clause_Expr:
		collect_expr_list_refs(c, n.assignments[:], scope)
	case ^ast.Constructor_Named_Assignment_Expr:
		collect_expr_refs(c, n.value, scope)
	case ^ast.Constructor_Base_Clause_Expr:
		collect_expr_refs(c, n.value, scope)
	case ^ast.Constructor_Lines_Of_Clause_Expr:
		collect_expr_refs(c, n.source, scope)
		collect_expr_refs(c, n.from, scope)
		collect_expr_refs(c, n.to, scope)
	case ^ast.Constructor_Optional_Expr:
		collect_expr_refs(c, n.value, scope)
	case ^ast.Constructor_Corresponding_Mapping_Clause_Expr:
		collect_expr_list_refs(c, n.assignments[:], scope)
	case ^ast.Constructor_Corresponding_Mapping_Assignment_Expr:
		collect_expr_refs(c, n.source, scope)
		collect_expr_refs(c, n.default_value, scope)
		collect_expr_refs(c, n.mapping, scope)
		collect_expr_refs(c, n.except, scope)
	case ^ast.Constructor_Corresponding_Except_Clause_Expr:
		collect_expr_list_refs(c, n.names[:], scope)
	case ^ast.Data_Inline_Name_Expr:
		if symbol_id := declare_name_if_present(c, scope, n.name, .Variable, n.range);
		   symbol_id != INVALID_SYMBOL_ID {
			add_syntax_operand(
				c.unit,
				scope,
				n.range,
				.Variable,
				unknown_type_fact(),
				symbol = Symbol_Handle{unit = c.unit.unit_id, symbol = symbol_id},
				has_symbol = true,
				assignable = true,
			)
		}
	case ^ast.Field_Symbol_Inline_Name_Expr:
		if symbol_id := declare_name_if_present(c, scope, n.name, .Field_Symbol, n.range);
		   symbol_id != INVALID_SYMBOL_ID {
			add_syntax_operand(
				c.unit,
				scope,
				n.range,
				.Variable,
				unknown_type_fact(),
				symbol = Symbol_Handle{unit = c.unit.unit_id, symbol = symbol_id},
				has_symbol = true,
				assignable = true,
			)
		}
	case ^ast.Char_String_Template_Expr:
		collect_expr_list_refs(c, n.parts[:], scope)
		fact := type_fact_from_expr(c, expr, scope)
		add_expression_fact(c, scope, expr.range, .Reference, fact)
		add_syntax_operand(c.unit, scope, expr.range, .Value, fact)
	case ^ast.Template_Interpolation_Expr:
		collect_expr_refs(c, n.expr, scope)
		collect_expr_list_refs(c, n.format_specs[:], scope)
	case ^ast.Template_Expr:
		collect_expr_refs(c, n.expr, scope)
	case ^ast.Template_Format_Spec_Expr:
		if option, ok := n.option.?; ok &&
		   (option == .Width || option == .Decimals) {
			collect_expr_refs(c, n.value, scope)
		}
	}
}

collect_type_expr_ref :: proc(
	c: ^Collector,
	expr: ^ast.Expr,
	scope: Scope_Id,
	namespace: Namespace,
) {
	if expr == nil {
		return
	}
	if type_ref, ok := type_ref_from_expr(c, expr, namespace); ok {
		add_type_reference(c, scope, type_ref, expr.range)
		if len(type_ref.field_path) == 0 {
			if access, access_ok := selector_access_from_expr(c, expr, scope, true);
			   access_ok && len(access.field_path) > 0 {
				access.base_namespace = namespace
				append(&c.unit.field_accesses, access)
			}
		}
		return
	}
	collect_expr_refs(c, expr, scope)
}

collect_type_clause_ref :: proc(c: ^Collector, clause: ^ast.Data_Type_Clause, scope: Scope_Id) {
	if clause == nil {
		return
	}
	collect_expr_refs(c, clause.initial_size, scope)
	if type_ref, ok := type_ref_from_clause(c, clause); ok {
		range := clause.type_ref.range if clause.type_ref != nil else tokenizer.Range{}
		add_type_reference(c, scope, type_ref, range)
	}
}

collect_constructor_expr_refs :: proc(
	c: ^Collector,
	expr: ^ast.Constructor_Expr,
	scope: Scope_Id,
) {
	collect_type_expr_ref(c, expr.type_ref, scope, .Type)
	for arg in expr.args {
		collect_expr_refs(c, arg, scope)
	}
	fact := type_fact_from_expr(c, expr, scope)
	add_expression_fact(c, scope, expr.range, .Call_Result, fact)
	add_syntax_operand(c.unit, scope, expr.range, .Value, fact)
}

collect_constructor_for_clause_refs :: proc(
	c: ^Collector,
	expr: ^ast.Constructor_For_Clause_Expr,
	scope: Scope_Id,
) {
	collect_expr_refs(c, expr.source, scope)
	source_access, has_source := value_access_from_expr(c, expr.source, scope)
	previous := c.current_scope
	c.current_scope = scope
	for_scope := push_scope(c, .Constructor_For, expr.range)
	collect_expr_refs(c, expr.init, for_scope)
	if expr.variable != "" {
		declare_name_if_present(c, for_scope, expr.variable, .Variable, expr.range)
		data := Constructor_For_Binding_Data {
			scope = for_scope,
			range = expr.range,
			name  = canonical_name(expr.variable, c.allocator),
		}
		if has_source {
			data.source_access = source_access
			data.has_source_access = true
		}
		append(&c.unit.constructor_for_bindings, data)
	}
	collect_expr_refs(c, expr.then_expr, for_scope)
	collect_expr_refs(c, expr.condition, for_scope)
	collect_expr_refs(c, expr.where_clause, for_scope)
	collect_expr_list_refs(c, expr.body[:], for_scope)
	c.current_scope = for_scope
	pop_scope(c)
	c.current_scope = previous
}

collect_selector_expr_refs :: proc(
	c: ^Collector,
	expr: ^ast.Selector_Expr,
	scope: Scope_Id,
	in_type_position: bool,
) {
	access, ok := selector_access_from_expr(c, expr, scope, in_type_position)
	if !ok {
		collect_expr_refs(c, expr.base, scope)
		return
	}
	kind := Reference_Kind.Identifier
	if access.base_namespace == .Type {
		kind = .Static_Target
	}
	add_reference(c, scope, access.base_name, access.base_namespace, kind, access.base_range)
	add_interface_qualified_segment_references(c, scope, access.field_path[:])
	if len(access.field_path) > 0 {
		append(&c.unit.field_accesses, access)
		add_expression_fact(c, scope, expr.range, .Selector, type_fact_from_expr(c, expr, scope))
	}
}

collect_interface_qualified_selector_expr_refs :: proc(
	c: ^Collector,
	expr: ^ast.Interface_Qualified_Selector_Expr,
	scope: Scope_Id,
	in_type_position: bool,
) {
	access, ok := selector_access_from_expr(c, expr, scope, in_type_position)
	if !ok {
		collect_expr_refs(c, expr.receiver, scope)
		return
	}
	kind := Reference_Kind.Identifier
	if access.base_namespace == .Type {
		kind = .Static_Target
	}
	add_reference(c, scope, access.base_name, access.base_namespace, kind, access.base_range)
	add_interface_qualified_segment_references(c, scope, access.field_path[:])
	if len(access.field_path) > 0 {
		append(&c.unit.field_accesses, access)
		add_expression_fact(c, scope, expr.range, .Selector, type_fact_from_expr(c, expr, scope))
	}
}

add_interface_qualified_segment_references :: proc(
	c: ^Collector,
	scope: Scope_Id,
	path: []Field_Access_Segment,
) {
	for segment in path {
		if segment.interface_qualified {
			add_reference(c, scope, segment.interface_name, .Type, .Type_Ref, segment.interface_range)
		}
	}
}

selector_access_from_expr :: proc(
	c: ^Collector,
	expr: ^ast.Expr,
	scope: Scope_Id,
	in_type_position: bool,
) -> (
	Field_Access,
	bool,
) {
	if expr == nil {
		return {}, false
	}
	if id, ok := expr.derived_expr.(^ast.Ident_Expr); ok {
		return Field_Access {
				scope = scope,
				base_namespace = .Value,
				base_name = canonical_name(id.name, c.allocator),
				base_range = id.range,
				field_path = make([dynamic]Field_Access_Segment, 0, 2, c.allocator),
				in_type_position = in_type_position,
			},
			true
	}
	sel, ok := expr.derived_expr.(^ast.Selector_Expr)
	if !ok {
		if q, q_ok := expr.derived_expr.(^ast.Interface_Qualified_Selector_Expr); q_ok {
			return interface_qualified_selector_access_from_expr(c, q, scope, in_type_position)
		}
		return {}, false
	}
	access, access_ok := selector_access_from_expr(c, sel.base, scope, in_type_position)
	if !access_ok {
		return {}, false
	}
	if len(access.field_path) == 0 {
		if sel.op == .Fat_Arrow || sel.op == .Tilde {
			access.base_namespace = .Type
		}
	}
	name, range, name_ok := expr_name(sel.field)
	if !name_ok {
		return {}, false
	}
	append(
		&access.field_path,
		Field_Access_Segment {
			name = canonical_name(name, c.allocator),
			range = range,
			selector = sel.op,
			deref = sel.op == .Arrow && name == "*",
		},
	)
	return access, true
}

interface_qualified_selector_access_from_expr :: proc(
	c: ^Collector,
	expr: ^ast.Interface_Qualified_Selector_Expr,
	scope: Scope_Id,
	in_type_position: bool,
) -> (
	Field_Access,
	bool,
) {
	access, access_ok := selector_access_from_expr(c, expr.receiver, scope, in_type_position)
	if !access_ok {
		return {}, false
	}
	if len(access.field_path) == 0 && expr.receiver_op == .Fat_Arrow {
		access.base_namespace = .Type
	}
	interface_name, interface_range, interface_ok := expr_name(expr.interface)
	member_name, member_range, member_ok := expr_name(expr.member)
	if !interface_ok || !member_ok {
		return {}, false
	}
	append(
		&access.field_path,
		Field_Access_Segment {
			name = canonical_name(member_name, c.allocator),
			range = member_range,
			selector = expr.receiver_op,
			interface_name = canonical_name(interface_name, c.allocator),
			interface_range = interface_range,
			interface_qualified = true,
		},
	)
	return access, true
}

value_access_from_expr :: proc(
	c: ^Collector,
	expr: ^ast.Expr,
	scope: Scope_Id,
) -> (
	Field_Access,
	bool,
) {
	access, ok := selector_access_from_expr(c, expr, scope, false)
	if ok && access.base_namespace == .Value {
		return access, true
	}
	return {}, false
}

collect_call_expr_refs :: proc(c: ^Collector, expr: ^ast.Call_Expr, scope: Scope_Id) {
	target := call_target_from_callee(c, expr.callee, scope)
	if target.kind == .Routine || target.kind == .Implicit_Method {
		name := target.routine_name
		if name == "" {
			name = target.method_name
		}
		if name != "" {
			add_reference(c, scope, name, .Routine, .Routine_Call, expr.callee.range)
		}
	} else {
		collect_call_method_target_refs(c, expr.callee, scope)
	}
	if args, ok := expr.args.derived_expr.(^ast.Call_Arg_List_Expr); ok {
		collect_call_arg_list_refs(c, args, scope, target, expr.range)
	}
	add_expression_fact(c, scope, expr.range, .Call_Result, unknown_type_fact())
}

collect_call_method_target_refs :: proc(c: ^Collector, target: ^ast.Expr, scope: Scope_Id) {
	if target == nil {
		return
	}
	if dyn, ok := target.derived_expr.(^ast.Dynamic_Call_Method_Target_Expr); ok {
		collect_dynamic_call_method_target_refs(c, dyn, scope)
		return
	}
	if raw, ok := target.derived_expr.(^ast.Type_Ref_Expr); ok && raw.raw_operand {
		for ref in raw.raw_refs {
			if ref.name == "" {
				continue
			}
			namespace := Namespace.Routine
			kind := Reference_Kind.Routine_Call
			if len(ref.path) > 0 {
				namespace = .Value
				kind = .Identifier
				if ref.type_base {
					namespace = .Type
					kind = .Static_Target
				}
			}
			add_reference(c, scope, ref.name, namespace, kind, ref.range)
		}
		return
	}
	if collect_call_method_selector_target_refs(c, target, scope) {
		return
	}
	if name, range, ok := expr_name(target); ok {
		add_reference(c, scope, name, .Routine, .Routine_Call, range)
		return
	}
	collect_expr_refs(c, target, scope)
}

collect_call_method_selector_target_refs :: proc(
	c: ^Collector,
	target: ^ast.Expr,
	scope: Scope_Id,
) -> bool {
	if receiver, receiver_op, interface_name, interface_range, _, _, qualified :=
		interface_qualified_method_parts(target);
	   qualified {
		if id, id_ok := receiver.derived_expr.(^ast.Ident_Expr);
		   id_ok && receiver_op == .Fat_Arrow {
			add_reference(c, scope, id.name, .Type, .Static_Target, id.range)
		} else {
			collect_expr_refs(c, receiver, scope)
		}
		add_reference(c, scope, interface_name, .Type, .Type_Ref, interface_range)
		return true
	}
	sel, ok := target.derived_expr.(^ast.Selector_Expr)
	if !ok {
		return false
	}
	if id, id_ok := sel.base.derived_expr.(^ast.Ident_Expr); id_ok {
		namespace := Namespace.Value
		kind := Reference_Kind.Identifier
		if sel.op == .Fat_Arrow || sel.op == .Tilde {
			namespace = .Type
			kind = .Static_Target
		}
		add_reference(c, scope, id.name, namespace, kind, id.range)
	} else {
		collect_expr_refs(c, sel.base, scope)
	}
	return true
}

call_target_from_callee :: proc(
	c: ^Collector,
	callee: ^ast.Expr,
	scope: Scope_Id,
) -> Named_Argument_Target {
	if callee == nil {
		return Named_Argument_Target{}
	}
	if id, ok := callee.derived_expr.(^ast.Ident_Expr); ok {
		name := canonical_name(id.name, c.allocator)
		if builtin_routine_spec(name) != nil {
			return Named_Argument_Target{kind = .Routine, routine_name = name}
		}
		return Named_Argument_Target{kind = .Implicit_Method, method_name = name, method_range = id.range}
	}
	if target, ok := interface_qualified_method_target_from_expr(c, callee, scope); ok {
		return target
	}
	if access, ok := selector_access_from_expr(c, callee, scope, false); ok {
		method_name := ""
		method_range := tokenizer.Range{}
		if len(access.field_path) > 0 {
			method := access.field_path[len(access.field_path) - 1]
			method_name = method.name
			method_range = method.range
		}
		return Named_Argument_Target {
			kind = .Method,
			base_namespace = access.base_namespace,
			base_name = access.base_name,
			method_name = method_name,
			method_range = method_range,
			receiver_path = method_receiver_path(c, access),
			interface_qualified = access.base_namespace == .Type,
		}
	}
	return Named_Argument_Target{}
}

interface_qualified_method_target_from_expr :: proc(
	c: ^Collector,
	expr: ^ast.Expr,
	scope: Scope_Id,
) -> (Named_Argument_Target, bool) {
	receiver, receiver_op, _, _, method_name, method_range, ok :=
		interface_qualified_method_parts(expr)
	if !ok {
		return {}, false
	}
	target := Named_Argument_Target {
		kind = .Method,
		method_name = canonical_name(method_name, c.allocator),
		method_range = method_range,
		interface_qualified = true,
	}
	if access, access_ok := selector_access_from_expr(c, receiver, scope, false); access_ok {
		if len(access.field_path) == 0 && receiver_op == .Fat_Arrow {
			access.base_namespace = .Type
		}
		target.base_namespace = access.base_namespace
		target.base_name = access.base_name
		target.receiver_path = access.field_path
	}
	return target, true
}

interface_qualified_method_parts :: proc(
	expr: ^ast.Expr,
) -> (
	receiver: ^ast.Expr,
	receiver_op: ast.Selector_Op,
	interface_name: string,
	interface_range: tokenizer.Range,
	method_name: string,
	method_range: tokenizer.Range,
	ok: bool,
) {
	if q, q_ok := expr.derived_expr.(^ast.Interface_Qualified_Selector_Expr); q_ok {
		iface_name, iface_range, interface_ok := expr_name(q.interface)
		meth_name, meth_range, method_ok := expr_name(q.member)
		if !interface_ok || !method_ok {
			return
		}
		return q.receiver, q.receiver_op, iface_name, iface_range, meth_name, meth_range, true
	}
	sel, sel_ok := expr.derived_expr.(^ast.Selector_Expr)
	if !sel_ok || sel.op != .Tilde {
		return
	}
	receiver_sel, receiver_ok := sel.base.derived_expr.(^ast.Selector_Expr)
	if !receiver_ok || (receiver_sel.op != .Arrow && receiver_sel.op != .Fat_Arrow) {
		return
	}
	iface_name, iface_range, interface_ok := expr_name(receiver_sel.field)
	meth_name, meth_range, method_ok := expr_name(sel.field)
	if !interface_ok || !method_ok {
		return
	}
	return receiver_sel.base, receiver_sel.op, iface_name, iface_range, meth_name, meth_range, true
}

method_receiver_path :: proc(
	c: ^Collector,
	access: Field_Access,
) -> [dynamic]Field_Access_Segment {
	assert(len(access.field_path) > 0)
	count := len(access.field_path) - 1
	path := make([dynamic]Field_Access_Segment, 0, count, c.allocator)
	for i in 0 ..< count {
		append(&path, access.field_path[i])
	}
	return path
}

collect_call_arg_list_refs :: proc(
	c: ^Collector,
	args: ^ast.Call_Arg_List_Expr,
	scope: Scope_Id,
	target: Named_Argument_Target,
	call_range: tokenizer.Range,
) {
	if args == nil {
		return
	}
	items := make([dynamic]Call_Argument_Data, 0, len(args.args), c.allocator)
	ordinal := 0
	current_section := Named_Argument_Section.Exporting
	has_section := false
	for arg in args.args {
		if section_expr, ok := arg.derived_expr.(^ast.Call_Arg_Section_Expr); ok {
			current_section, has_section = named_argument_section_from_ast(section_expr.kind)
			for section_arg in section_expr.args {
				collect_call_arg_expr_refs(
					c,
					section_arg,
					scope,
					target,
					current_section,
					has_section,
				)
				append_call_argument(
					c,
					&items,
					section_arg,
					scope,
					current_section,
					has_section,
					ordinal,
				)
				ordinal += 1
			}
			continue
		}
		collect_call_arg_expr_refs(c, arg, scope, target, current_section, has_section)
		append_call_argument(c, &items, arg, scope, current_section, has_section, ordinal)
		ordinal += 1
	}
	append(
		&c.unit.call_sites,
		Call_Site_Data{scope = scope, range = call_range, target = target, arguments = items},
	)
}

collect_call_arg_expr_refs :: proc(
	c: ^Collector,
	arg: ^ast.Expr,
	scope: Scope_Id,
	target: Named_Argument_Target,
	section: Named_Argument_Section,
	has_section: bool,
) {
	if named, ok := arg.derived_expr.(^ast.Call_Named_Arg_Expr); ok {
		append(
			&c.unit.named_arguments,
			Named_Argument_Access {
				scope = scope,
				name = canonical_name(named.name, c.allocator),
				range = named.range,
				section = section,
				has_section = has_section,
				target = target,
			},
		)
		collect_expr_refs(c, named.value, scope)
		return
	}
	if pos, ok := arg.derived_expr.(^ast.Call_Positional_Arg_Expr); ok {
		collect_expr_refs(c, pos.value, scope)
		return
	}
	collect_expr_refs(c, arg, scope)
}

append_call_argument :: proc(
	c: ^Collector,
	items: ^[dynamic]Call_Argument_Data,
	arg: ^ast.Expr,
	scope: Scope_Id,
	section: Named_Argument_Section,
	has_section: bool,
	ordinal: int,
) {
	name := ""
	value := arg
	if named, named_ok := arg.derived_expr.(^ast.Call_Named_Arg_Expr); named_ok {
		name = canonical_name(named.name, c.allocator)
		value = named.value
	} else if pos, pos_ok := arg.derived_expr.(^ast.Call_Positional_Arg_Expr); pos_ok {
		value = pos.value
	}
	append(
		items,
		Call_Argument_Data {
			range = arg.range,
			value_range = value.range if value != nil else arg.range,
			name = name,
			section = section,
			has_section = has_section,
			ordinal = ordinal,
			type_fact = type_fact_from_expr(c, value, scope),
		},
	)
}

named_argument_section_from_ast :: proc(
	kind: ast.Call_Arg_Section_Kind,
) -> (
	Named_Argument_Section,
	bool,
) {
	#partial switch kind {
	case .Exporting:
		return .Exporting, true
	case .Importing:
		return .Importing, true
	case .Changing:
		return .Changing, true
	case .Tables:
		return .Tables, true
	case .Receiving:
		return .Receiving, true
	case .Exceptions:
		return .Exceptions, true
	}
	return .Exporting, false
}

collect_raw_operand_refs :: proc(c: ^Collector, expr: ^ast.Type_Ref_Expr, scope: Scope_Id) {
	collect_raw_operand_fact_refs(c, expr.raw_decls[:], expr.raw_refs[:], scope)
}

collect_create_object_stmt_facts :: proc(
	c: ^Collector,
	stmt: ^ast.Create_Object_Stmt,
	scope: Scope_Id,
) {
	collect_expr_refs(c, stmt.target, scope)
	collect_create_type_refs(c, scope, stmt.type_ref, stmt.type_clause, stmt.type_dynamic, stmt.type_dynamic_expr)
	collect_expr_list_refs(c, stmt.operands[:], scope)
}

collect_create_data_stmt_facts :: proc(
	c: ^Collector,
	stmt: ^ast.Create_Data_Stmt,
	scope: Scope_Id,
) {
	collect_expr_refs(c, stmt.target, scope)
	collect_create_type_refs(c, scope, stmt.type_ref, stmt.type_clause, stmt.type_dynamic, stmt.type_dynamic_expr)
	collect_expr_refs(c, stmt.type_handle, scope)
	if stmt.type_handle != nil {
		target_name, target_range, _ := raw_operand_simple_ref(stmt.target, c.allocator)
		handle_name, handle_range, handle_ok := raw_operand_simple_ref(stmt.type_handle, c.allocator)
		if handle_ok {
			append(
				&c.unit.create_data_type_handles,
				Create_Data_Type_Handle_Site_Data {
					scope = scope,
					target_name = target_name,
					target_range = target_range,
					handle_name = handle_name,
					handle_range = handle_range,
				},
			)
		}
	}
	collect_expr_list_refs(c, stmt.operands[:], scope)
}

raw_operand_simple_ref :: proc(expr: ^ast.Expr, allocator: mem.Allocator) -> (string, tokenizer.Range, bool) {
	if expr == nil {
		return "", tokenizer.Range{}, false
	}
	raw, ok := expr.derived_expr.(^ast.Type_Ref_Expr)
	if !ok || len(raw.raw_refs) != 1 || len(raw.raw_refs[0].path) > 0 {
		return "", tokenizer.Range{}, false
	}
	ref := raw.raw_refs[0]
	return canonical_name(ref.name, allocator), ref.range, ref.name != ""
}

collect_create_type_refs :: proc(
	c: ^Collector,
	scope: Scope_Id,
	type_ref: ^ast.Expr,
	type_clause: ^ast.Data_Type_Clause,
	type_dynamic: bool,
	type_dynamic_expr: ^ast.Expr,
) {
	if type_dynamic {
		collect_expr_refs(c, type_dynamic_expr if type_dynamic_expr != nil else type_ref, scope)
	} else if type_clause != nil {
		collect_type_clause_ref(c, type_clause, scope)
	} else {
		collect_type_expr_ref(c, type_ref, scope, .Type)
	}
}

collect_dynamic_call_method_target_refs :: proc(
	c: ^Collector,
	expr: ^ast.Dynamic_Call_Method_Target_Expr,
	scope: Scope_Id,
) {
	if expr.base != nil {
		if expr.base_dynamic {
			collect_expr_refs(c, expr.base, scope)
		} else if access, access_ok := call_method_receiver_access_from_expr(c, expr.base, scope, expr.selector);
		   access_ok {
			kind := Reference_Kind.Identifier
			if access.base_namespace == .Type {
				kind = .Static_Target
			}
			add_reference(c, scope, access.base_name, access.base_namespace, kind, access.base_range)
			add_interface_qualified_segment_references(c, scope, access.field_path[:])
			if len(access.field_path) > 0 {
				append(&c.unit.field_accesses, access)
			}
		} else if name, range, ok := expr_name(expr.base); ok {
			namespace := Namespace.Value
			kind := Reference_Kind.Identifier
			if expr.selector == .Fat_Arrow || expr.selector == .Tilde {
				namespace = .Type
				kind = .Static_Target
			}
			add_reference(c, scope, name, namespace, kind, range)
		} else {
			collect_expr_refs(c, expr.base, scope)
		}
	}
	if expr.method_dynamic {
		collect_expr_refs(c, expr.method, scope)
	}
}

call_method_receiver_access_from_expr :: proc(
	c: ^Collector,
	expr: ^ast.Expr,
	scope: Scope_Id,
	selector: ast.Selector_Op,
) -> (Field_Access, bool) {
	access, ok := selector_access_from_expr(c, expr, scope, false)
	if !ok {
		return {}, false
	}
	if len(access.field_path) == 0 && (selector == .Fat_Arrow || selector == .Tilde) {
		access.base_namespace = .Type
	}
	return access, true
}

collect_raw_operand_fact_refs :: proc(
	c: ^Collector,
	decls: []ast.Raw_Operand_Inline_Decl,
	refs: []ast.Raw_Operand_Ref,
	scope: Scope_Id,
) {
	for decl in decls {
		kind := Symbol_Kind.Variable
		if decl.kind == .Field_Symbol {
			kind = .Field_Symbol
		}
		if symbol_id := declare_name_if_present(c, scope, decl.name, kind, decl.range);
		   symbol_id != INVALID_SYMBOL_ID {
			add_syntax_operand(
				c.unit,
				scope,
				decl.range,
				.Variable,
				unknown_type_fact(),
				symbol = Symbol_Handle{unit = c.unit.unit_id, symbol = symbol_id},
				has_symbol = true,
				assignable = true,
			)
		}
	}
	for ref in refs {
		if ref.name == "" {
			continue
		}
		namespace := Namespace.Value
		kind := Reference_Kind.Identifier
		if ref.type_base {
			namespace = .Type
			kind = .Static_Target
		}
		name := canonical_name(ref.name, c.allocator)
		if ref.call_like &&
		   !ref.type_base &&
		   len(ref.path) == 0 &&
		   builtin_routine_spec(name) != nil {
			add_reference(c, scope, name, .Routine, .Routine_Call, ref.range)
			continue
		}
		add_reference(c, scope, name, namespace, kind, ref.range)
		if len(ref.path) > 0 {
			segments := make([dynamic]Field_Access_Segment, 0, len(ref.path), c.allocator)
			for segment in ref.path {
				append(
					&segments,
					Field_Access_Segment {
						name = canonical_name(segment.name, c.allocator),
						range = segment.range,
						selector = segment.selector,
						deref = segment.selector == .Arrow && segment.name == "*",
					},
				)
			}
			append(
				&c.unit.field_accesses,
				Field_Access {
					scope = scope,
					base_namespace = namespace,
					base_name = name,
					base_range = ref.range,
					field_path = segments,
				},
			)
		}
	}
}

type_fact_from_expr :: proc(c: ^Collector, expr: ^ast.Expr, scope: Scope_Id) -> Type_Fact_Data {
	if expr == nil {
		return unknown_type_fact()
	}
	if _, ok := expr.derived_expr.(^ast.Char_String_Template_Expr); ok {
		return Type_Fact_Data {
			type_id = type_builtin(c.unit, "string"),
			type_unit = c.unit.unit_id,
			structure = INVALID_STRUCTURE_ID,
			structure_unit = INVALID_UNIT_ID,
			declared_type = builtin_type_ref("string"),
			has_declared_type = true,
		}
	}
	if lit, ok := expr.derived_expr.(^ast.Literal_Expr); ok {
		if len(lit.value) > 0 && lit.value[0] >= '0' && lit.value[0] <= '9' {
			return Type_Fact_Data {
				type_id = type_builtin(c.unit, "i"),
				type_unit = c.unit.unit_id,
				structure = INVALID_STRUCTURE_ID,
				structure_unit = INVALID_UNIT_ID,
				declared_type = builtin_type_ref("i"),
				has_declared_type = true,
			}
		}
		return Type_Fact_Data {
			type_id = type_builtin(c.unit, "string"),
			type_unit = c.unit.unit_id,
			structure = INVALID_STRUCTURE_ID,
			structure_unit = INVALID_UNIT_ID,
			declared_type = builtin_type_ref("string"),
			has_declared_type = true,
		}
	}
	if access, ok := value_access_from_expr(c, expr, scope); ok && len(access.field_path) == 0 {
		if id, found := lookup_symbol_in_scope_chain(c, scope, access.base_name, .Value); found {
			s := c.unit.symbols[symbol_id_index(id)]
			return Type_Fact_Data {
				type_id = s.type_id,
				type_unit = c.unit.unit_id,
				structure = s.structure,
				structure_unit = c.unit.unit_id if s.structure != INVALID_STRUCTURE_ID else INVALID_UNIT_ID,
				declared_type = s.declared_type,
				has_declared_type = s.has_declared_type,
				type_clause_display = s.type_clause_display,
			}
		}
	}
	if con, ok := expr.derived_expr.(^ast.Constructor_Expr); ok {
		if type_ref, has_type := type_ref_from_expr(
			c,
			con.type_ref,
			.Type,
			con.kind == .New || con.kind == .Ref,
		); has_type {
			type_id := UNKNOWN_TYPE_ID
			if len(type_ref.field_path) == 0 && is_builtin_type_name(type_ref.base_name) {
				type_id = type_builtin(c.unit, type_ref.base_name)
			}
			return Type_Fact_Data {
				type_id = type_id,
				type_unit = c.unit.unit_id if type_id_is_known(type_id) else INVALID_UNIT_ID,
				structure = INVALID_STRUCTURE_ID,
				structure_unit = INVALID_UNIT_ID,
				declared_type = type_ref,
				has_declared_type = true,
				type_clause_display = expr_display(c, con.type_ref),
			}
		}
	}
	return unknown_type_fact()
}

add_expression_fact :: proc(
	c: ^Collector,
	scope: Scope_Id,
	range: tokenizer.Range,
	kind: Expression_Fact_Kind,
	fact: Type_Fact_Data,
) {
	append(
		&c.unit.expression_facts,
		Expression_Fact_Data{scope = scope, range = range, kind = kind, type_fact = fact},
	)
}

collect_inline_data_stmt_facts :: proc(
	c: ^Collector,
	stmt: ^ast.Data_Inline_Decl,
	scope: Scope_Id,
) {
	collect_expr_refs(c, stmt.expr, scope)
	if stmt.expr != nil {
		add_assignment_site(
			c,
			scope,
			stmt.range,
			stmt.range,
			stmt.expr.range,
			Field_Access{},
			false,
			unknown_type_fact(),
			type_fact_from_expr(c, stmt.expr, scope),
		)
	}
}

collect_assignment_stmt_facts :: proc(
	c: ^Collector,
	range: tokenizer.Range,
	lhs, rhs: ^ast.Expr,
	scope: Scope_Id,
	corresponding: bool,
) {
	collect_expr_refs(c, lhs, scope)
	collect_expr_refs(c, rhs, scope)
	lhs_access, has_lhs := value_access_from_expr(c, lhs, scope)
	flags := Assignment_Site_Flags{}
	if has_lhs {
		flags += {.Has_Lhs_Target_Access}
	}
	if corresponding {
		flags += {.Is_Corresponding}
	}
	append(
		&c.unit.assignment_sites,
		Assignment_Site_Data {
			scope = scope,
			range = range,
			lhs_range = lhs.range if lhs != nil else tokenizer.Range{},
			rhs_range = rhs.range if rhs != nil else tokenizer.Range{},
			lhs_target_access = lhs_access,
			lhs = type_fact_from_expr(c, lhs, scope),
			rhs = type_fact_from_expr(c, rhs, scope),
			flags = flags,
		},
	)
}

add_assignment_site :: proc(
	c: ^Collector,
	scope: Scope_Id,
	range, lhs_range, rhs_range: tokenizer.Range,
	lhs_access: Field_Access,
	has_lhs: bool,
	lhs, rhs: Type_Fact_Data,
	extra_flags := Assignment_Site_Flags{},
) {
	flags := extra_flags
	if has_lhs {
		flags += {.Has_Lhs_Target_Access}
	}
	append(
		&c.unit.assignment_sites,
		Assignment_Site_Data {
			scope = scope,
			range = range,
			lhs_range = lhs_range,
			rhs_range = rhs_range,
			lhs_target_access = lhs_access,
			lhs = lhs,
			rhs = rhs,
			flags = flags,
		},
	)
}

collect_write_target_expr :: proc(
	c: ^Collector,
	scope: Scope_Id,
	stmt_range: tokenizer.Range,
	target: ^ast.Expr,
	rhs_range: tokenizer.Range = tokenizer.Range{},
) {
	if target == nil {
		return
	}
	collect_expr_refs(c, target, scope)
	lhs_access, has_lhs := value_access_from_expr(c, target, scope)
	add_assignment_site(
		c,
		scope,
		stmt_range,
		target.range,
		rhs_range,
		lhs_access,
		has_lhs,
		type_fact_from_expr(c, target, scope),
		unknown_type_fact(),
	)
}

collect_clear_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Clear_Stmt, scope: Scope_Id) {
	for op in stmt.operands {
		collect_expr_refs(c, op.target, scope)
		collect_expr_refs(c, op.value, scope)
		if op.target != nil {
			add_assignment_site(
				c,
				scope,
				stmt.range,
				op.target.range,
				tokenizer.Range{},
				Field_Access{},
				false,
				type_fact_from_expr(c, op.target, scope),
				unknown_type_fact(),
			)
		}
	}
}

collect_refresh_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Refresh_Stmt, scope: Scope_Id) {
	for op in stmt.operands {
		collect_expr_refs(c, op.target, scope)
	}
}

collect_free_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Free_Stmt, scope: Scope_Id) {
	for op in stmt.operands {
		collect_expr_refs(c, op.target, scope)
	}
	collect_expr_refs(c, stmt.memory_id, scope)
}

collect_unassign_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Unassign_Stmt, scope: Scope_Id) {
	for op in stmt.operands {
		collect_expr_refs(c, op.target, scope)
	}
}

collect_move_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Move_Stmt, scope: Scope_Id) {
	for entry in stmt.entries {
		collect_assignment_stmt_facts(c, stmt.range, entry.target, entry.source, scope, false)
	}
}

collect_move_corresponding_stmt_facts :: proc(
	c: ^Collector,
	stmt: ^ast.Move_Corresponding_Stmt,
	scope: Scope_Id,
) {
	for entry in stmt.entries {
		collect_assignment_stmt_facts(c, stmt.range, entry.target, entry.source, scope, true)
	}
}

collect_add_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Add_Stmt, scope: Scope_Id) {
	for e in stmt.entries {
		collect_expr_refs(c, e.source, scope)
		collect_expr_refs(c, e.target, scope)
		if e.result != nil {
			collect_assignment_stmt_facts(c, stmt.range, e.result, e.source, scope, false)
		} else {
			collect_assignment_stmt_facts(c, stmt.range, e.target, e.source, scope, false)
		}
	}
}

collect_subtract_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Subtract_Stmt, scope: Scope_Id) {
	for e in stmt.entries {
		collect_expr_refs(c, e.source, scope)
		collect_expr_refs(c, e.target, scope)
		if e.result != nil {
			collect_assignment_stmt_facts(c, stmt.range, e.result, e.source, scope, false)
		} else {
			collect_assignment_stmt_facts(c, stmt.range, e.target, e.source, scope, false)
		}
	}
}

collect_multiply_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Multiply_Stmt, scope: Scope_Id) {
	for e in stmt.entries {
		collect_expr_refs(c, e.source, scope)
		collect_expr_refs(c, e.target, scope)
		if e.result != nil {
			collect_assignment_stmt_facts(c, stmt.range, e.result, e.source, scope, false)
		} else {
			collect_assignment_stmt_facts(c, stmt.range, e.target, e.source, scope, false)
		}
	}
}

collect_divide_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Divide_Stmt, scope: Scope_Id) {
	for e in stmt.entries {
		collect_expr_refs(c, e.source, scope)
		collect_expr_refs(c, e.target, scope)
		if e.result != nil {
			collect_assignment_stmt_facts(c, stmt.range, e.result, e.source, scope, false)
		} else {
			collect_assignment_stmt_facts(c, stmt.range, e.target, e.source, scope, false)
		}
	}
}

collect_compute_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Compute_Stmt, scope: Scope_Id) {
	for e in stmt.entries {
		collect_assignment_stmt_facts(c, stmt.range, e.target, e.source, scope, false)
	}
}

collect_concatenate_stmt_facts :: proc(
	c: ^Collector,
	stmt: ^ast.Concatenate_Stmt,
	scope: Scope_Id,
) {
	for e in stmt.entries {
		collect_expr_list_refs(c, e.sources[:], scope)
		collect_expr_refs(c, e.separator, scope)
		collect_write_target_expr(c, scope, stmt.range, e.target, stmt.range)
		if e.lines_of && len(e.sources) > 0 {
			append(
				&c.unit.concatenate_lines_of_sites,
				Concatenate_Lines_Of_Site_Data {
					scope = scope,
					range = stmt.range,
					source_range = e.sources[0].range,
					source = type_fact_from_expr(c, e.sources[0], scope),
					byte_mode = stmt.byte_mode,
				},
			)
		}
	}
}

collect_split_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Split_Stmt, scope: Scope_Id) {
	for e in stmt.entries {
		collect_expr_refs(c, e.source, scope)
		collect_expr_refs(c, e.separator, scope)
		for target in e.targets {
			collect_expr_refs(c, target, scope)
		}
	}
}

collect_replace_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Replace_Stmt, scope: Scope_Id) {
	collect_expr_refs(c, stmt.pattern, scope)
	collect_expr_refs(c, stmt.target, scope)
	collect_expr_refs(c, stmt.replacement, scope)
	collect_expr_refs(c, stmt.section_offset, scope)
	collect_expr_refs(c, stmt.section_length, scope)
}

collect_translate_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Translate_Stmt, scope: Scope_Id) {
	collect_expr_refs(c, stmt.target, scope)
	collect_expr_refs(c, stmt.operand, scope)
}

collect_shift_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Shift_Stmt, scope: Scope_Id) {
	collect_expr_refs(c, stmt.target, scope)
	collect_expr_refs(c, stmt.places, scope)
	collect_expr_refs(c, stmt.delete_pattern, scope)
}

collect_find_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Find_Stmt, scope: Scope_Id) {
	collect_expr_refs(c, stmt.pattern, scope)
	collect_expr_refs(c, stmt.target, scope)
	collect_expr_refs(c, stmt.section_offset, scope)
	collect_expr_refs(c, stmt.section_length, scope)
	collect_expr_refs(c, stmt.match_offset, scope)
	collect_expr_refs(c, stmt.match_length, scope)
	collect_expr_refs(c, stmt.match_line, scope)
	collect_expr_refs(c, stmt.match_count, scope)
	collect_expr_refs(c, stmt.results, scope)
	collect_expr_list_refs(c, stmt.submatches[:], scope)
}

collect_search_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Search_Stmt, scope: Scope_Id) {
	collect_expr_refs(c, stmt.target, scope)
	collect_expr_refs(c, stmt.pattern, scope)
	collect_expr_refs(c, stmt.starting_at, scope)
	collect_expr_refs(c, stmt.ending_at, scope)
}

collect_perform_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Perform_Stmt, scope: Scope_Id) {
	routine_name := ""
	external_program := perform_has_external_program(stmt)
	if name, range, ok := expr_name(stmt.form); ok && stmt.form_kind == .Static {
		routine_name = canonical_name(name, c.allocator)
		if !external_program {
			add_reference(c, scope, routine_name, .Routine, .Routine_Call, range)
		}
	} else {
		collect_expr_refs(c, stmt.form, scope)
	}
	program_static := false
	program_name := ""
	if stmt.program != nil {
		if name, _, ok := static_perform_program_name(c, stmt); ok {
			program_static = true
			program_name = name
		} else {
			collect_expr_refs(c, stmt.program, scope)
		}
	}
	collect_expr_list_refs(c, stmt.tables[:], scope)
	collect_expr_list_refs(c, stmt.using_args[:], scope)
	collect_expr_list_refs(c, stmt.changing[:], scope)
	if program_static {
		append(
			&c.unit.call_sites,
			Call_Site_Data {
				scope = scope,
				range = stmt.range,
				target = Named_Argument_Target {
					kind        = .Report,
					report_name = program_name,
				},
			},
		)
	}
}

perform_has_external_program :: proc(stmt: ^ast.Perform_Stmt) -> bool {
	return stmt.has_program_clause && stmt.program_kind != .Omitted
}

static_perform_program_name :: proc(
	c: ^Collector,
	stmt: ^ast.Perform_Stmt,
) -> (string, tokenizer.Range, bool) {
	if stmt.program == nil {
		return "", tokenizer.Range{}, false
	}
	if stmt.program_kind == .Static {
		if name, range, ok := expr_name(stmt.program); ok {
			return canonical_name(strip_quotes(name), c.allocator), range, true
		}
		return "", tokenizer.Range{}, false
	}
	if stmt.program_kind == .Dynamic {
		if paren, ok := stmt.program.derived_expr.(^ast.Paren_Expr); ok {
			if name, range, name_ok := expr_name(paren.expr); name_ok {
				if _, lit_ok := paren.expr.derived_expr.(^ast.Literal_Expr); lit_ok {
					return canonical_name(strip_quotes(name), c.allocator), range, true
				}
			}
		}
	}
	return "", tokenizer.Range{}, false
}

collect_call_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Call_Stmt, scope: Scope_Id) {
	#partial switch stmt.kind {
	case .Direct:
		collect_expr_refs(c, stmt.call, scope)
	case .Function, .Customer_Function:
		target_name := call_stmt_function_name(c, stmt)
		target := Named_Argument_Target {
			kind          = .Function,
			function_name = target_name,
		}
		collect_raw_call_stmt_args(c, stmt, scope, target)
	case .Method:
		target := call_stmt_method_target(c, stmt, scope)
		collect_call_method_target_refs(c, stmt.target, scope)
		collect_raw_call_stmt_args(c, stmt, scope, target)
	case .Transaction:
		collect_expr_refs(c, stmt.target, scope)
		collect_expr_list_refs(c, stmt.transaction_operands[:], scope)
	case .Transformation:
		for arg in stmt.transformation_args {
			collect_expr_refs(c, arg.value, scope)
		}
	case:
		collect_expr_refs(c, stmt.target, scope)
	}
}

call_stmt_function_name :: proc(c: ^Collector, stmt: ^ast.Call_Stmt) -> string {
	return function_target_name(c, stmt.target)
}

function_target_name :: proc(c: ^Collector, target: ^ast.Expr) -> string {
	if target == nil {
		return ""
	}
	text := strings.trim_space(expr_display(c, target))
	if len(text) >= 2 &&
	   ((text[0] == '\'' && text[len(text) - 1] == '\'') ||
			   (text[0] == '`' && text[len(text) - 1] == '`')) {
		text = text[1:len(text) - 1]
	}
	return canonical_name(text, c.allocator)
}

call_stmt_method_target :: proc(
	c: ^Collector,
	stmt: ^ast.Call_Stmt,
	scope: Scope_Id,
) -> Named_Argument_Target {
	if stmt.target == nil {
		return Named_Argument_Target{kind = .Implicit_Method}
	}
	if dyn, ok := stmt.target.derived_expr.(^ast.Dynamic_Call_Method_Target_Expr); ok {
		if dyn.base == nil {
			return Named_Argument_Target{kind = .Implicit_Method}
		}
		namespace := Namespace.Value
		if dyn.selector == .Fat_Arrow || dyn.selector == .Tilde {
			namespace = .Type
		}
		base_name := ""
		receiver_path: [dynamic]Field_Access_Segment
		if !dyn.base_dynamic {
			if access, access_ok := call_method_receiver_access_from_expr(c, dyn.base, scope, dyn.selector);
			   access_ok {
				namespace = access.base_namespace
				base_name = access.base_name
				receiver_path = access.field_path
			} else if name, _, name_ok := expr_name(dyn.base); name_ok {
				base_name = canonical_name(name, c.allocator)
			}
		}
		method_name := ""
		method_range := tokenizer.Range{}
		if !dyn.method_dynamic {
			if name, range, name_ok := expr_name(dyn.method); name_ok {
				method_name = canonical_name(name, c.allocator)
				method_range = range
			}
		}
		return Named_Argument_Target {
			kind = .Method,
			base_namespace = namespace,
			base_name = base_name,
			method_name = method_name,
			method_range = method_range,
			receiver_path = receiver_path,
			interface_qualified = namespace == .Type,
		}
	}
	if target, ok := interface_qualified_method_target_from_expr(c, stmt.target, scope); ok {
		return target
	}
	if access, ok := selector_access_from_expr(c, stmt.target, scope, false);
	   ok && len(access.field_path) > 0 {
		method := access.field_path[len(access.field_path) - 1]
		return Named_Argument_Target {
			kind = .Method,
			base_namespace = access.base_namespace,
			base_name = access.base_name,
			method_name = method.name,
			method_range = method.range,
			receiver_path = method_receiver_path(c, access),
			interface_qualified = access.base_namespace == .Type,
		}
	}
	if raw, ok := stmt.target.derived_expr.(^ast.Type_Ref_Expr); ok {
		for ref in raw.raw_refs {
			if len(ref.path) == 0 {
				continue
			}
			namespace := Namespace.Value
			if ref.type_base {
				namespace = .Type
			}
			receiver_path := make(
				[dynamic]Field_Access_Segment,
				0,
				len(ref.path) - 1,
				c.allocator,
			)
			for i in 0 ..< len(ref.path) - 1 {
				append(
					&receiver_path,
					Field_Access_Segment {
						name = canonical_name(ref.path[i].name, c.allocator),
						range = ref.path[i].range,
						selector = ref.path[i].selector,
					},
				)
			}
			return Named_Argument_Target {
				kind = .Method,
				base_namespace = namespace,
				base_name = canonical_name(ref.name, c.allocator),
				method_name = canonical_name(ref.path[len(ref.path) - 1].name, c.allocator),
				method_range = ref.path[len(ref.path) - 1].range,
				receiver_path = receiver_path,
				interface_qualified = namespace == .Type,
			}
		}
	}
	if name, _, ok := expr_name(stmt.target); ok {
		return Named_Argument_Target {
			kind = .Implicit_Method,
			method_name = canonical_name(name, c.allocator),
			method_range = stmt.target.range,
		}
	}
	return Named_Argument_Target{kind = .Implicit_Method}
}

// Raw CALL fallback: CALL FUNCTION/CALL METHOD values keep only parser-populated
// value facts; semantic does not tokenize their source text.
collect_raw_call_stmt_args :: proc(
	c: ^Collector,
	stmt: ^ast.Call_Stmt,
	scope: Scope_Id,
	target: Named_Argument_Target,
) {
	collect_raw_call_args(c, stmt.named_args[:], stmt.range, scope, target)
}

collect_raw_call_args :: proc(
	c: ^Collector,
	named_args: []ast.Call_Stmt_Named_Arg,
	call_range: tokenizer.Range,
	scope: Scope_Id,
	target: Named_Argument_Target,
) {
	args := make([dynamic]Call_Argument_Data, 0, 4, c.allocator)
	ordinal := 0
	for arg in named_args {
		section, valid_section := named_argument_section_from_ast(arg.section)
		has_section := arg.has_section && valid_section
		name := canonical_name(arg.name, c.allocator)
		append(
			&c.unit.named_arguments,
			Named_Argument_Access {
				scope = scope,
				name = name,
				range = arg.name_range,
				section = section,
				has_section = has_section,
				target = target,
			},
		)
		if arg.value_range.start < arg.value_range.end {
			type_fact := unknown_type_fact()
			if arg.value != nil {
				collect_expr_refs(c, arg.value, scope)
				type_fact = type_fact_from_expr(c, arg.value, scope)
			} else {
				collect_raw_operand_fact_refs(c, arg.raw_decls[:], arg.raw_refs[:], scope)
			}
			append(
				&args,
				Call_Argument_Data {
					range = arg.value_range,
					value_range = arg.value_range,
					name = name,
					section = section,
					has_section = has_section,
					ordinal = ordinal,
					type_fact = type_fact,
				},
			)
			ordinal += 1
		}
	}
	append(
		&c.unit.call_sites,
		Call_Site_Data{scope = scope, range = call_range, target = target, arguments = args},
	)
}

collect_submit_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Submit_Stmt, scope: Scope_Id) {
	has_report_target := false
	if report_name, ok := submit_report_name(c, stmt); ok {
		has_report_target = true
		append(
			&c.unit.call_sites,
			Call_Site_Data {
				scope = scope,
				range = stmt.range,
				target = Named_Argument_Target {
					kind        = .Report,
					report_name = report_name,
				},
			},
		)
	}
	if stmt.target_kind == .Dynamic || !has_report_target {
		collect_expr_refs(c, stmt.target, scope)
	}
	for option in stmt.options {
		collect_expr_refs(c, option.value, scope)
		collect_expr_refs(c, option.high_value, scope)
	}
}

submit_report_name :: proc(
	c: ^Collector,
	stmt: ^ast.Submit_Stmt,
) -> (string, bool) {
	if stmt.target == nil {
		return "", false
	}
	if stmt.target_kind == .Static {
		if ident, ok := stmt.target.derived_expr.(^ast.Ident_Expr); ok {
			return canonical_name(ident.name, c.allocator), true
		}
		return "", false
	}
	paren, ok := stmt.target.derived_expr.(^ast.Paren_Expr)
	if !ok || paren.expr == nil {
		return "", false
	}
	if lit, lit_ok := paren.expr.derived_expr.(^ast.Literal_Expr); lit_ok {
		return submit_literal_report_name(c, lit.value)
	}
	return "", false
}

submit_literal_report_name :: proc(c: ^Collector, value: string) -> (string, bool) {
	if len(value) < 2 {
		return "", false
	}
	if !((value[0] == '\'' && value[len(value) - 1] == '\'') ||
	     (value[0] == '`' && value[len(value) - 1] == '`')) {
		return "", false
	}
	return canonical_name(value[1:len(value) - 1], c.allocator), true
}

collect_message_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Message_Stmt, scope: Scope_Id) {
	use := Message_Use_Data {
		range           = stmt.range,
		with_arg_ranges = make([dynamic]tokenizer.Range, 0, len(stmt.with_args), c.allocator),
	}
	if stmt.head != nil {
		collect_message_head(c, stmt.head, scope, &use)
	}
	for arg in stmt.with_args {
		collect_expr_refs(c, arg, scope)
		if arg != nil {
			append(&use.with_arg_ranges, arg.range)
		}
	}
	collect_expr_refs(c, stmt.into, scope)
	collect_expr_refs(c, stmt.display_like, scope)
	collect_expr_refs(c, stmt.raising, scope)
	append(&c.unit.message_uses, use)
}

collect_message_head :: proc(
	c: ^Collector,
	head: ^ast.Message_Head_Clause,
	scope: Scope_Id,
	use: ^Message_Use_Data,
) {
	if head.id != nil {
		name, range, ok := expr_name(head.id)
		if ok {
			class_name := canonical_name(strip_quotes(name), c.allocator)
			use.class_name = class_name
			use.class_range = range
			use.flags += {.Has_Class_Range}
			add_reference(c, scope, class_name, .Value, .Message_Class, range)
		} else {
			collect_expr_refs(c, head.id, scope)
		}
		collect_expr_refs(c, head.msg_type, scope)
		collect_expr_refs(c, head.number, scope)
		return
	}
	if head.code != nil {
		if head.has_compact_class {
			class_name := canonical_name(head.compact_class_name, c.allocator)
			use.class_name = class_name
			use.class_range = head.compact_class_range
			use.flags += {.Has_Class_Range}
			add_reference(c, scope, class_name, .Value, .Message_Class, head.compact_class_range)
		} else if c.unit.has_message_default_class {
			use.class_name = c.unit.message_default_class.name
			use.class_range = c.unit.message_default_class.range
		}
	}
	collect_expr_refs(c, head.msg_type, scope)
}

strip_quotes :: proc(text: string) -> string {
	if len(text) >= 2 &&
	   ((text[0] == '\'' && text[len(text) - 1] == '\'') ||
			   (text[0] == '`' && text[len(text) - 1] == '`')) {
		return text[1:len(text) - 1]
	}
	return text
}

collect_write_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Write_Stmt, scope: Scope_Id) {
	for op in stmt.operands {
		collect_expr_refs(c, op.value, scope)
		collect_expr_refs(c, op.position, scope)
		collect_expr_refs(c, op.length, scope)
	}
}

collect_write_to_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Write_To_Stmt, scope: Scope_Id) {
	for entry in stmt.entries {
		collect_expr_refs(c, entry.source, scope)
		collect_write_target_expr(c, scope, stmt.range, entry.target, expr_range(entry.source))
	}
}

collect_flow_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Flow_Stmt, scope: Scope_Id) {
	_ = c
	_ = stmt
	_ = scope
}

collect_describe_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Describe_Stmt, scope: Scope_Id) {
	for e in stmt.entries {
		collect_expr_refs(c, e.source, scope)
		collect_expr_refs(c, e.target, scope)
		if e.target != nil {
			add_assignment_site(
				c,
				scope,
				stmt.range,
				e.target.range,
				e.source.range if e.source != nil else tokenizer.Range{},
				Field_Access{},
				false,
				type_fact_from_expr(c, e.target, scope),
				unknown_type_fact(),
			)
		}
	}
}

collect_runtime_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Runtime_Stmt, scope: Scope_Id) {
	collect_expr_refs(c, stmt.id, scope)
	collect_expr_refs(c, stmt.field, scope)
	collect_expr_refs(c, stmt.target, scope)
	collect_expr_refs(c, stmt.value, scope)
	collect_expr_refs(c, stmt.line, scope)
	collect_expr_refs(c, stmt.offset, scope)
	collect_expr_list_refs(c, stmt.excluding[:], scope)
	collect_expr_list_refs(c, stmt.operands[:], scope)
}

collect_set_handler_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Set_Handler_Stmt, scope: Scope_Id) {
	for handler in stmt.handlers {
		collect_handler_ref(c, handler, scope)
	}
	collect_expr_refs(c, stmt.sender, scope)
	collect_expr_refs(c, stmt.activation, scope)
}

collect_handler_ref :: proc(c: ^Collector, expr: ^ast.Expr, scope: Scope_Id) {
	if expr == nil {
		return
	}
	if name, range, ok := expr_name(expr); ok {
		add_reference(c, scope, name, .Routine, .Routine_Call, range)
		return
	}
	collect_call_method_target_refs(c, expr, scope)
}

collect_import_stmt_facts :: proc(
	c: ^Collector,
	stmt: ^ast.Import_Stmt,
	scope: Scope_Id,
) {
	for parameter in stmt.parameters {
		collect_write_target_expr(c, scope, stmt.range, parameter.value)
	}
	collect_data_cluster_medium_refs(c, stmt.medium, scope, stmt.range, false)
}

collect_export_stmt_facts :: proc(
	c: ^Collector,
	stmt: ^ast.Export_Stmt,
	scope: Scope_Id,
) {
	for parameter in stmt.parameters {
		collect_expr_refs(c, parameter.value, scope)
	}
	collect_data_cluster_medium_refs(c, stmt.medium, scope, stmt.range, true)
}

collect_data_cluster_medium_refs :: proc(
	c: ^Collector,
	medium: ast.Data_Cluster_Medium_Clause,
	scope: Scope_Id,
	stmt_range: tokenizer.Range,
	exporting: bool,
) {
	switch medium.kind {
	case .Data_Buffer, .Internal_Table:
		if exporting {
			collect_write_target_expr(c, scope, stmt_range, medium.object)
		} else {
			collect_expr_refs(c, medium.object, scope)
		}
	case .Memory_ID:
		collect_expr_refs(c, medium.id, scope)
	case .Database, .Shared_Memory, .Shared_Buffer:
		if exporting {
			collect_expr_refs(c, medium.work_area, scope)
		} else {
			collect_write_target_expr(c, scope, stmt_range, medium.work_area)
		}
		collect_expr_refs(c, medium.client, scope)
		collect_expr_refs(c, medium.id, scope)
	}
}

collect_bit_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Bit_Stmt, scope: Scope_Id) {
	collect_expr_refs(c, stmt.position, scope)
	if stmt.kind == .Get {
		collect_expr_refs(c, stmt.source, scope)
		collect_write_target_expr(c, scope, stmt.range, stmt.target, expr_range(stmt.source))
	} else {
		collect_expr_refs(c, stmt.value, scope)
		collect_write_target_expr(c, scope, stmt.range, stmt.target, expr_range(stmt.value))
	}
}

collect_locale_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Locale_Stmt, scope: Scope_Id) {
	collect_expr_refs(c, stmt.language, scope)
	collect_expr_refs(c, stmt.country, scope)
	collect_expr_refs(c, stmt.modifier, scope)
}

collect_convert_time_stamp_stmt_facts :: proc(
	c: ^Collector,
	stmt: ^ast.Convert_Time_Stamp_Stmt,
	scope: Scope_Id,
) {
	collect_expr_refs(c, stmt.time_zone, scope)
	if stmt.kind == .Time_Stamp_To_Date_Time {
		collect_expr_refs(c, stmt.time_stamp, scope)
		collect_write_target_expr(c, scope, stmt.range, stmt.date, expr_range(stmt.time_stamp))
		collect_write_target_expr(c, scope, stmt.range, stmt.time, expr_range(stmt.time_stamp))
	} else {
		collect_expr_refs(c, stmt.date, scope)
		collect_expr_refs(c, stmt.time, scope)
		collect_write_target_expr(c, scope, stmt.range, stmt.time_stamp, expr_range(stmt.date))
	}
}

collect_receive_results_stmt_facts :: proc(
	c: ^Collector,
	stmt: ^ast.Receive_Results_Stmt,
	scope: Scope_Id,
) {
	collect_expr_refs(c, stmt.target, scope)
	collect_raw_call_args(
		c,
		stmt.named_args[:],
		stmt.range,
		scope,
		Named_Argument_Target {
			kind = .Function,
			function_name = function_target_name(c, stmt.target),
		},
	)
}

collect_raise_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Raise_Stmt, scope: Scope_Id) {
	if stmt.target_type {
		collect_type_expr_ref(c, stmt.target, scope, .Type)
	} else {
		collect_expr_refs(c, stmt.target, scope)
	}
	collect_expr_list_refs(c, stmt.operands[:], scope)
}

collect_authority_check_stmt_facts :: proc(
	c: ^Collector,
	stmt: ^ast.Authority_Check_Stmt,
	scope: Scope_Id,
) {
	collect_expr_refs(c, stmt.object, scope)
	collect_expr_list_refs(c, stmt.operands[:], scope)
	for id in stmt.ids {
		collect_expr_refs(c, id.id, scope)
		collect_expr_refs(c, id.field, scope)
	}
}

collect_assign_field_stmt_facts :: proc(
	c: ^Collector,
	stmt: ^ast.Assign_Field_Stmt,
	scope: Scope_Id,
) {
	collect_expr_refs(c, stmt.source, scope)
	collect_expr_refs(c, stmt.component, scope)
	collect_expr_refs(c, stmt.structure, scope)
	collect_expr_refs(c, stmt.target, scope)
	if stmt.casting_type != nil {
		if raw_type, ok := stmt.casting_type.derived_expr.(^ast.Type_Ref_Expr); ok && raw_type.raw_operand {
			collect_expr_refs(c, stmt.casting_type, scope)
		} else {
			collect_type_expr_ref(c, stmt.casting_type, scope, .Type)
		}
	}
	collect_expr_refs(c, stmt.casting_decimals, scope)
}

collect_line_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Line_Stmt, scope: Scope_Id) {
	collect_expr_refs(c, stmt.line, scope)
	collect_expr_refs(c, stmt.index, scope)
	collect_expr_refs(c, stmt.into, scope)
	for field in stmt.fields {
		collect_expr_refs(c, field.field, scope)
		collect_expr_refs(c, field.target, scope)
	}
}

collect_loop_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Loop_Stmt, scope: Scope_Id) {
	collect_expr_refs(c, stmt.source, scope)
	collect_expr_refs(c, stmt.from, scope)
	collect_expr_refs(c, stmt.to, scope)
	collect_expr_refs(c, stmt.using_key.dynamic_name, scope)
	if stmt.target != nil {
		collect_expr_refs(c, stmt.target, scope)
		if stmt.target_kind == .Into || stmt.target_kind == .Assigning {
			target_access, has_target := value_access_from_expr(c, stmt.target, scope)
			add_assignment_site(
				c,
				scope,
				stmt.range,
				stmt.target.range,
				stmt.source.range if stmt.source != nil else tokenizer.Range{},
				target_access,
				has_target,
				type_fact_from_expr(c, stmt.target, scope),
				type_fact_from_expr(c, stmt.source, scope),
				{.Assigns_Table_Line},
			)
		}
	}
	collect_internal_table_where_refs(c, stmt.source, stmt.where_cond, scope)
	source_access, has_source := value_access_from_expr(c, stmt.source, scope)
	previous := c.current_scope
	c.current_scope = scope
	loop_scope := push_scope(c, .Loop_Block, stmt.range)
	if has_source {
		append(&c.loop_source_stack, source_access)
		c.unit.scopes[scope_id_index(loop_scope)].allows_internal_table_line_selector = true
	}
	walk_stmt_list(c, stmt.body, loop_scope)
	if has_source {
		_ = pop_loop_source(c)
	}
	c.current_scope = loop_scope
	pop_scope(c)
	c.current_scope = previous
}

pop_loop_source :: proc(c: ^Collector) -> bool {
	if len(c.loop_source_stack) == 0 {
		return false
	}
	next := make([dynamic]Field_Access, 0, len(c.loop_source_stack) - 1, c.allocator)
	for i in 0 ..< len(c.loop_source_stack) - 1 {
		append(&next, c.loop_source_stack[i])
	}
	c.loop_source_stack = next
	return true
}

collect_at_stmt_facts :: proc(c: ^Collector, stmt: ^ast.At_Stmt, scope: Scope_Id) {
	previous := c.current_scope
	c.current_scope = scope
	at_scope := push_scope(c, .At_Block, stmt.range)
	if stmt.expr != nil {
		collect_expr_refs(c, stmt.expr, at_scope)
	}
	if stmt.field_name != "" && len(c.loop_source_stack) > 0 {
		source := c.loop_source_stack[len(c.loop_source_stack) - 1]
		field := Field_Access_Segment {
			name = canonical_name(stmt.field_name, c.allocator),
			range = stmt.field_range,
		}
		path := make([dynamic]Field_Access_Segment, 0, len(source.field_path) + 1, c.allocator)
		for segment in source.field_path {
			append(&path, segment)
		}
		append(&path, field)
		append(
			&c.unit.field_accesses,
			Field_Access {
				scope = at_scope,
				base_namespace = source.base_namespace,
				base_name = source.base_name,
				base_range = source.base_range,
				field_path = path,
			},
		)
		append(
			&c.unit.loop_at_field_contexts,
			Loop_At_Field_Context {
				scope = at_scope,
				range = stmt.field_range,
				source_access = source,
			},
		)
	}
	walk_stmt_list(c, stmt.body, at_scope)
	c.current_scope = at_scope
	pop_scope(c)
	c.current_scope = previous
}

walk_catch_clause_facts :: proc(
	c: ^Collector,
	clause: ^ast.Catch_Clause,
	parent_scope: Scope_Id,
) -> Scope_Id {
	previous := c.current_scope
	c.current_scope = parent_scope
	catch_scope := push_scope(c, .Catch_Clause, clause.range)
	for ex in clause.exceptions {
		collect_type_expr_ref(c, ex, catch_scope, .Type)
	}
	if clause.into != nil {
		if inline_name_expr, ok := clause.into.derived_expr.(^ast.Data_Inline_Name_Expr); ok {
			declared := Field_Type_Ref_Data{}
			has_type := false
			if len(clause.exceptions) > 0 {
				declared, has_type = type_ref_from_expr(c, clause.exceptions[0], .Type, true)
			}
			_ = declare_collected_symbol(
				c,
				catch_scope,
				inline_name_expr.name,
				.Variable,
				inline_name_expr.range,
				INVALID_STRUCTURE_ID,
				declared,
				has_type,
				expr_display(c, clause.exceptions[0]) if has_type else "",
			)
		} else {
			collect_expr_refs(c, clause.into, catch_scope)
		}
	}
	walk_stmt_list(c, clause.body, catch_scope)
	c.current_scope = catch_scope
	pop_scope(c)
	c.current_scope = previous
	return catch_scope
}

walk_function_pool_decl :: proc(c: ^Collector, stmt: ^ast.Function_Pool_Decl, scope: Scope_Id) {
	if stmt.name != "" {
		_ = declare_collected_symbol(c, scope, stmt.name, .Report, stmt.range)
	}
	if stmt.message_id != "" {
		set_message_default_class(c, stmt.message_id, stmt.range, scope)
	}
}

collect_report_stmt_refs :: proc(c: ^Collector, stmt: ^ast.Report_Stmt, scope: Scope_Id) {
	if stmt.has_message_id {
		set_message_default_class(c, stmt.message_id, stmt.message_id_range, scope)
		return
	}
	#partial switch stmt.kind {
	case .Read_Report:
		collect_expr_refs(c, stmt.name, scope)
		collect_write_target_expr(c, scope, stmt.range, stmt.source)
		collect_expr_refs(c, stmt.line_size, scope)
		collect_expr_refs(c, stmt.line_count, scope)
	case .Insert_Report:
		collect_expr_refs(c, stmt.name, scope)
		collect_expr_refs(c, stmt.source, scope)
		collect_expr_refs(c, stmt.line_size, scope)
		collect_expr_refs(c, stmt.line_count, scope)
	case .Delete_Report:
		collect_expr_refs(c, stmt.name, scope)
	case:
		collect_expr_refs(c, stmt.name, scope)
		collect_expr_refs(c, stmt.line_size, scope)
		collect_expr_refs(c, stmt.line_count, scope)
	}
}

set_message_default_class :: proc(
	c: ^Collector,
	name: string,
	range: tokenizer.Range,
	scope: Scope_Id,
) {
	class_name := canonical_name(strip_quotes(name), c.allocator)
	c.unit.message_default_class = Message_Class_Use_Data {
		name  = class_name,
		range = range,
	}
	c.unit.has_message_default_class = true
	add_reference(c, scope, class_name, .Value, .Message_Class, range)
}

collect_read_table_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Read_Table_Stmt, scope: Scope_Id) {
	for e in stmt.entries {
		collect_expr_refs(c, e.table, scope)
		collect_expr_refs(c, e.into, scope)
		collect_expr_refs(c, e.assigning, scope)
		collect_expr_refs(c, e.reference_into, scope)
		collect_expr_refs(c, e.index, scope)
		collect_expr_refs(c, e.using_key.dynamic_name, scope)
		collect_expr_list_refs(c, e.comparing[:], scope)
		for key in e.key_values {
			collect_expr_refs(c, key.dynamic_name, scope)
			collect_expr_refs(c, key.value, scope)
			if key.is_dynamic || key.table_line {
				continue
			}
			if len(key.path) == 0 {
				continue
			}
			if e.table != nil {
				access, ok := value_access_from_expr(c, e.table, scope)
				if ok {
					segments := make(
						[dynamic]Field_Access_Segment,
						0,
						len(access.field_path) + len(key.path),
						c.allocator,
					)
					for segment in access.field_path {
						append(&segments, segment)
					}
					start := 0
					if len(key.path) > 0 && strings.equal_fold(key.path[0].name, "table_line") {
						start = 1
					}
					for segment in key.path[start:] {
						append(
							&segments,
							Field_Access_Segment {
								name = canonical_name(segment.name, c.allocator),
								range = segment.range,
								selector = segment.selector,
							},
						)
					}
					append(
						&c.unit.field_accesses,
						Field_Access {
							scope = scope,
							base_namespace = access.base_namespace,
							base_name = access.base_name,
							base_range = access.base_range,
							field_path = segments,
						},
					)
				}
			}
		}
		if e.into != nil {
			lhs_access, has_lhs := value_access_from_expr(c, e.into, scope)
			add_assignment_site(
				c,
				scope,
				stmt.range,
				e.into.range,
				e.table.range if e.table != nil else tokenizer.Range{},
				lhs_access,
				has_lhs,
				type_fact_from_expr(c, e.into, scope),
				unknown_type_fact(),
			)
		}
	}
}

collect_insert_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Insert_Stmt, scope: Scope_Id) {
	query_id := -1
	is_sql := false
	if stmt.form == .Db_Table {
		query_id, is_sql = collect_db_table_sql_source(
			c,
			stmt.range,
			stmt.target,
			scope,
			nil,
			tokenizer.Range{},
			false,
		)
		if !is_sql {
			if stmt.has_db_table_name {
				query_id, is_sql = collect_db_table_sql_source_name(
					c,
					stmt.range,
					stmt.db_table_name,
					stmt.db_table_name_range,
					scope,
					nil,
					tokenizer.Range{},
					false,
				)
			}
		}
		if !is_sql {
			collect_expr_refs(c, stmt.target, scope)
		}
	} else {
		collect_expr_refs(c, stmt.target, scope)
	}
	collect_expr_refs(c, stmt.source, scope)
	collect_expr_refs(c, stmt.index, scope)
	collect_expr_refs(c, stmt.assigning, scope)
	collect_expr_refs(c, stmt.reference_into, scope)
	for a in stmt.assignments {
		if is_sql && a.column_name != "" {
			push_sql_name_ref(
				c,
				query_id,
				scope,
				a.column_range,
				a.column_name,
				"",
				.Column,
				.Unresolved,
			)
		} else if is_sql {
			collect_sql_name_refs_from_expr(c, query_id, a.name, scope, false)
		} else {
			collect_expr_refs(c, a.name, scope)
		}
		collect_expr_refs(c, a.value, scope)
	}
}

collect_append_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Append_Stmt, scope: Scope_Id) {
	collect_expr_refs(c, stmt.source, scope)
	collect_expr_refs(c, stmt.target, scope)
	collect_expr_refs(c, stmt.assigning, scope)
	collect_expr_refs(c, stmt.reference_into, scope)
}

collect_modify_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Modify_Stmt, scope: Scope_Id) {
	is_sql := false
	has_internal_table_clause := stmt.table_keyword || stmt.index != nil || len(stmt.transporting) > 0
	if !has_internal_table_clause {
		_, is_sql = collect_db_table_sql_source(
			c,
			stmt.range,
			stmt.target,
			scope,
			stmt.where_cond,
			stmt.where_clause,
			stmt.dynamic_where,
		)
	}
	if !is_sql {
		collect_expr_refs(c, stmt.target, scope)
		collect_internal_table_where_refs(c, stmt.target, stmt.where_cond, scope)
		collect_modify_where_context(c, stmt, scope)
	}
	collect_expr_refs(c, stmt.source, scope)
	collect_expr_refs(c, stmt.index, scope)
	collect_modify_transporting_refs(c, stmt, scope)
}

collect_modify_transporting_refs :: proc(c: ^Collector, stmt: ^ast.Modify_Stmt, scope: Scope_Id) {
	base, ok := value_access_from_expr(c, stmt.target, scope)
	if !ok {
		base, ok = value_access_from_expr(c, stmt.source, scope)
	}
	if !ok {
		return
	}
	for field in stmt.transporting {
		if len(field.path) == 0 {
			continue
		}
		path := make(
			[dynamic]Field_Access_Segment,
			0,
			len(base.field_path) + len(field.path),
			c.allocator,
		)
		for segment in base.field_path {
			append(&path, segment)
		}
		for segment in field.path {
			append(
				&path,
				Field_Access_Segment {
					name = canonical_name(segment.name, c.allocator),
					range = segment.range,
				},
			)
		}
		append(
			&c.unit.field_accesses,
			Field_Access {
				scope = scope,
				base_namespace = base.base_namespace,
				base_name = base.base_name,
				base_range = base.base_range,
				field_path = path,
			},
		)
	}
}

collect_modify_where_context :: proc(c: ^Collector, stmt: ^ast.Modify_Stmt, scope: Scope_Id) {
	if stmt.where_cond == nil {
		return
	}
	source, has_source := value_access_from_expr(c, stmt.target, scope)
	if !has_source {
		source, has_source = value_access_from_expr(c, stmt.source, scope)
	}
	if !has_source {
		return
	}
	target, has_target := value_access_from_expr(c, stmt.source, scope)
	append(
		&c.unit.loop_where_field_contexts,
		Loop_Where_Field_Context {
			scope = scope,
			range = stmt.where_clause if range_valid(stmt.where_clause) else stmt.where_cond.range,
			source_access = source,
			target_access = target,
			has_target = has_target,
		},
	)
}

collect_sort_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Sort_Stmt, scope: Scope_Id) {
	collect_expr_refs(c, stmt.target, scope)
	for field in stmt.fields {
		if field.name == "" {
			collect_expr_refs(c, field.expr, scope)
		}
	}
}

collect_update_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Update_Stmt, scope: Scope_Id) {
	query_id, is_sql := collect_db_table_sql_source(
		c,
		stmt.range,
		stmt.target,
		scope,
		stmt.where_cond,
		stmt.where_clause,
		stmt.dynamic_where,
	)
	if !is_sql {
		collect_expr_refs(c, stmt.target, scope)
		collect_expr_refs(c, stmt.where_cond, scope)
	}
	collect_expr_refs(c, stmt.source, scope)
	for a in stmt.assignments {
		if is_sql && a.column_name != "" {
			push_sql_name_ref(
				c,
				query_id,
				scope,
				a.column_range,
				a.column_name,
				"",
				.Column,
				.Unresolved,
			)
		} else if is_sql {
			collect_sql_name_refs_from_expr(c, query_id, a.name, scope, false)
		} else {
			collect_expr_refs(c, a.name, scope)
		}
		collect_expr_refs(c, a.value, scope)
	}
}

collect_delete_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Delete_Stmt, scope: Scope_Id) {
	is_sql := false
	if stmt.form == .Db_Table {
		_, is_sql = collect_db_table_sql_source(
			c,
			stmt.range,
			stmt.target,
			scope,
			stmt.where_cond,
			stmt.where_clause,
			stmt.dynamic_where,
		)
	}
	if !is_sql {
		collect_expr_refs(c, stmt.target, scope)
		collect_internal_table_where_refs(c, stmt.target, stmt.where_cond, scope)
	}
	collect_expr_refs(c, stmt.source, scope)
	collect_expr_refs(c, stmt.index, scope)
	collect_expr_refs(c, stmt.using_key.dynamic_name, scope)
	collect_delete_comparing_refs(c, stmt, scope)
}

collect_internal_table_where_refs :: proc(
	c: ^Collector,
	target, expr: ^ast.Expr,
	scope: Scope_Id,
) {
	if expr == nil {
		return
	}
	if access, ok := value_access_from_expr(c, expr, scope);
	   ok && !internal_table_where_value_exists(c, scope, access.base_name) {
		if target_access, target_ok := value_access_from_expr(c, target, scope); target_ok {
			if !internal_table_where_target_has_shape(c, scope, target_access) {
				return
			}
			path := make(
				[dynamic]Field_Access_Segment,
				0,
				len(target_access.field_path) + 1 + len(access.field_path),
				c.allocator,
			)
			for segment in target_access.field_path {append(&path, segment)}
			append(&path, Field_Access_Segment{name = access.base_name, range = access.base_range})
			for segment in access.field_path {append(&path, segment)}
			append(
				&c.unit.field_accesses,
				Field_Access {
					scope = scope,
					base_namespace = target_access.base_namespace,
					base_name = target_access.base_name,
					base_range = target_access.base_range,
					field_path = path,
					requires_known_base_shape = true,
					where_candidate_name = access.base_name,
				},
			)
			return
		}
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Binary_Expr:
		collect_internal_table_where_refs(c, target, n.left, scope)
		collect_internal_table_where_refs(c, target, n.right, scope)
	case ^ast.Unary_Expr:
		collect_internal_table_where_refs(c, target, n.expr, scope)
	case ^ast.Paren_Expr:
		collect_internal_table_where_refs(c, target, n.expr, scope)
	case ^ast.Substring_Expr:
		collect_internal_table_where_refs(c, target, n.base, scope)
		collect_expr_refs(c, n.offset, scope)
		collect_expr_refs(c, n.length, scope)
	case ^ast.Between_Expr:
		collect_internal_table_where_refs(c, target, n.subject, scope)
		collect_internal_table_where_refs(c, target, n.low, scope)
		collect_internal_table_where_refs(c, target, n.high, scope)
	case ^ast.Is_Predicate_Expr:
		collect_internal_table_where_refs(c, target, n.subject, scope)
	case:
		collect_expr_refs(c, expr, scope)
	}
}

internal_table_where_value_exists :: proc(c: ^Collector, scope: Scope_Id, name: string) -> bool {
	if sql_local_value_exists(c, scope, name) {
		return true
	}
	class_symbol, ok := enclosing_owner(c, scope, .Class)
	for ok {
		if class_scope_value_exists(c, class_symbol, name) {
			return true
		}
		super_name, has_super := collector_superclass_name(c, class_symbol)
		if !has_super {
			break
		}
		class_symbol, ok = lookup_symbol_in_scope_chain(c, scope, super_name, .Type)
	}
	return false
}

internal_table_where_target_has_shape :: proc(
	c: ^Collector,
	scope: Scope_Id,
	target: Field_Access,
) -> bool {
	if target.base_namespace != .Value {
		return false
	}
	symbol_id, ok := lookup_symbol_in_scope_chain(c, scope, target.base_name, .Value)
	if !ok {
		return false
	}
	s := c.unit.symbols[symbol_id_index(symbol_id)]
	return s.structure != INVALID_STRUCTURE_ID || s.has_declared_type
}

class_scope_value_exists :: proc(c: ^Collector, class_symbol: Symbol_Id, name: string) -> bool {
	for s in c.unit.scopes {
		if !(s.kind == .Class || s.kind == .Interface) || s.owner != class_symbol {
			continue
		}
		if _, ok := scope_lookup_declaration(c.unit, s.id, .Value, name); ok {
			return true
		}
	}
	return false
}

collector_superclass_name :: proc(c: ^Collector, class_symbol: Symbol_Id) -> (string, bool) {
	for inheritance in c.unit.class_inheritance {
		if inheritance.class_symbol == class_symbol {
			return inheritance.superclass_name, true
		}
	}
	return "", false
}

collect_delete_comparing_refs :: proc(c: ^Collector, stmt: ^ast.Delete_Stmt, scope: Scope_Id) {
	target, has_target := value_access_from_expr(c, stmt.target, scope)
	for clause in stmt.comparing {
		if clause.all_fields {
			continue
		}
		if has_target {
			if operand, ok := value_access_from_expr(c, clause.expr, scope); ok {
				path := make(
					[dynamic]Field_Access_Segment,
					0,
					len(target.field_path) + 1 + len(operand.field_path),
					c.allocator,
				)
				for segment in target.field_path {
					append(&path, segment)
				}
				append(&path, Field_Access_Segment{name = operand.base_name, range = operand.base_range})
				for segment in operand.field_path {
					append(&path, segment)
				}
				append(
					&c.unit.field_accesses,
					Field_Access {
						scope = scope,
						base_namespace = target.base_namespace,
						base_name = target.base_name,
						base_range = target.base_range,
						field_path = path,
					},
				)
				continue
			}
		}
		collect_expr_refs(c, clause.expr, scope)
	}
}

collect_dataset_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Dataset_Stmt, scope: Scope_Id) {
	collect_expr_refs(c, stmt.dataset, scope)
	collect_expr_refs(c, stmt.source, scope)
	if stmt.kind == .Read {
		collect_write_target_expr(c, scope, stmt.range, stmt.target, expr_range(stmt.dataset))
		collect_write_target_expr(c, scope, stmt.range, stmt.actual_length)
	} else {
		collect_expr_refs(c, stmt.target, scope)
	}
	if stmt.kind == .Get {
		collect_write_target_expr(c, scope, stmt.range, stmt.position)
		collect_write_target_expr(c, scope, stmt.range, stmt.attributes)
	} else {
		collect_expr_refs(c, stmt.position, scope)
		collect_expr_refs(c, stmt.attributes, scope)
	}
	if stmt.kind == .Open {
		collect_write_target_expr(c, scope, stmt.range, stmt.message)
	} else {
		collect_expr_refs(c, stmt.message, scope)
	}
	collect_expr_refs(c, stmt.maximum_length, scope)
	collect_expr_refs(c, stmt.length, scope)
}

collect_textpool_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Textpool_Stmt, scope: Scope_Id) {
	collect_expr_refs(c, stmt.program, scope)
	if stmt.kind == .Read {
		collect_write_target_expr(c, scope, stmt.range, stmt.table)
	} else {
		collect_expr_refs(c, stmt.table, scope)
	}
	collect_expr_refs(c, stmt.language, scope)
}

collect_generate_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Generate_Stmt, scope: Scope_Id) {
	collect_expr_refs(c, stmt.source, scope)
	collect_write_target_expr(c, scope, stmt.range, stmt.name, expr_range(stmt.source))
	collect_expr_refs(c, stmt.program, scope)
	collect_expr_refs(c, stmt.dynpro, scope)
	collect_write_target_expr(c, scope, stmt.range, stmt.message)
	collect_write_target_expr(c, scope, stmt.range, stmt.line)
	collect_write_target_expr(c, scope, stmt.range, stmt.word)
	collect_write_target_expr(c, scope, stmt.range, stmt.offset)
}
