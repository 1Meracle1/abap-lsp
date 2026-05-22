package abap_frontend_semantic

import "../ast"
import "../tokenizer"

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
	case ^ast.Host_Expr:
		collect_expr_refs(c, n.value, scope)
	case ^ast.Table_Expr:
		collect_expr_refs(c, n.table, scope)
		collect_expr_list_refs(c, n.selectors[:], scope)
	case ^ast.Selector_Expr:
		collect_selector_expr_refs(c, n, scope, false)
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
		add_state_check(c, n.subject, scope, n.range, n.kind, n.negated)
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
		collect_zero_state_check(c, n, scope)
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
		declare_name_if_present(c, scope, n.name, .Variable, n.range)
	case ^ast.Field_Symbol_Inline_Name_Expr:
		declare_name_if_present(c, scope, n.name, .Field_Symbol, n.range)
	case ^ast.Char_String_Template_Expr:
		collect_expr_list_refs(c, n.parts[:], scope)
	case ^ast.Template_Interpolation_Expr:
		collect_expr_refs(c, n.expr, scope)
		collect_expr_list_refs(c, n.format_specs[:], scope)
	case ^ast.Template_Expr:
		collect_expr_refs(c, n.expr, scope)
	case ^ast.Template_Format_Spec_Expr:
		collect_expr_refs(c, n.value, scope)
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
				append(&c.field_accesses, access)
			}
		}
		return
	}
	collect_expr_refs(c, expr, scope)
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
	add_expression_fact(c, scope, expr.range, .Call_Result, type_fact_from_expr(c, expr, scope))
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
		append(&c.constructor_for_bindings, data)
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
		collect_expr_refs(c, expr.field, scope)
		return
	}
	kind := Reference_Kind.Identifier
	if access.base_namespace == .Type {
		kind = .Static_Target
	}
	add_reference(c, scope, access.base_name, access.base_namespace, kind, access.base_range)
	if len(access.field_path) > 0 {
		append(&c.field_accesses, access)
		add_expression_fact(c, scope, expr.range, .Selector, type_fact_from_expr(c, expr, scope))
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
		Field_Access_Segment{name = canonical_name(name, c.allocator), range = range},
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
		collect_expr_refs(c, expr.callee, scope)
	}
	if args, ok := expr.args.derived_expr.(^ast.Call_Arg_List_Expr); ok {
		collect_call_arg_list_refs(c, args, scope, target, expr.range)
	}
	add_expression_fact(c, scope, expr.range, .Call_Result, unknown_type_fact())
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
		return Named_Argument_Target{kind = .Implicit_Method, method_name = name}
	}
	if access, ok := selector_access_from_expr(c, callee, scope, false); ok {
		method_name := ""
		if len(access.field_path) > 0 {
			method_name = access.field_path[len(access.field_path) - 1].name
		}
		return Named_Argument_Target {
			kind = .Method,
			base_namespace = access.base_namespace,
			base_name = access.base_name,
			method_name = method_name,
			interface_qualified = access.base_namespace == .Type,
		}
	}
	return Named_Argument_Target{}
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
		&c.call_sites,
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
			&c.named_arguments,
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
			name = name,
			section = section,
			has_section = has_section,
			ordinal = ordinal,
			type_fact = type_fact_from_expr(c, value, scope),
		},
	)
}

named_argument_section_from_ast :: proc(kind: ast.Call_Arg_Section_Kind) -> (
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

collect_raw_operand_refs :: proc(
	c: ^Collector,
	expr: ^ast.Type_Ref_Expr,
	scope: Scope_Id,
) {
	collect_raw_operand_fact_refs(c, expr.raw_decls[:], expr.raw_refs[:], scope)
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
		declare_name_if_present(c, scope, decl.name, kind, decl.range)
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
		add_reference(c, scope, name, namespace, kind, ref.range)
		if len(ref.path) > 0 {
			segments := make([dynamic]Field_Access_Segment, 0, len(ref.path), c.allocator)
			for segment in ref.path {
				append(
					&segments,
					Field_Access_Segment {
						name = canonical_name(segment.name, c.allocator),
						range = segment.range,
					},
				)
			}
			append(
				&c.field_accesses,
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
	if lit, ok := expr.derived_expr.(^ast.Literal_Expr); ok {
		if len(lit.value) > 0 && lit.value[0] >= '0' && lit.value[0] <= '9' {
			return Type_Fact_Data {
				structure = INVALID_STRUCTURE_ID,
				declared_type = builtin_type_ref("i"),
				has_declared_type = true,
			}
		}
		return Type_Fact_Data {
			structure = INVALID_STRUCTURE_ID,
			declared_type = builtin_type_ref("string"),
			has_declared_type = true,
		}
	}
	if access, ok := value_access_from_expr(c, expr, scope); ok && len(access.field_path) == 0 {
		if id, found := lookup_symbol_in_scope_chain(c, scope, access.base_name, .Value); found {
			s := c.symbols[symbol_id_index(id)]
			return Type_Fact_Data {
				structure = s.structure,
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
			return Type_Fact_Data {
				structure = INVALID_STRUCTURE_ID,
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
		&c.expression_facts,
		Expression_Fact_Data{scope = scope, range = range, kind = kind, type_fact = fact},
	)
}

collect_zero_state_check :: proc(c: ^Collector, expr: ^ast.Binary_Expr, scope: Scope_Id) {
	if expr.op != .Equal && expr.op != .Not_Equal {
		return
	}
	if !expr_is_zero_literal(expr.right) {
		return
	}
	access, ok := value_access_from_expr(c, expr.left, scope)
	if !ok {
		return
	}
	field_name := ""
	if len(access.field_path) > 0 {
		field_name = access.field_path[len(access.field_path) - 1].name
	}
	append(
		&c.value_state_checks,
		Value_State_Check_Data {
			scope = scope,
			range = expr.range,
			symbol_name = access.base_name,
			symbol_range = access.base_range,
			field_name = field_name,
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

add_state_check :: proc(
	c: ^Collector,
	subject: ^ast.Expr,
	scope: Scope_Id,
	range: tokenizer.Range,
	kind: ast.Is_Predicate_Kind,
	negated: bool,
) {
	access, ok := value_access_from_expr(c, subject, scope)
	if !ok {
		return
	}
	if kind == .Assigned {
		append(
			&c.field_symbol_state_checks,
			Field_Symbol_State_Check_Data {
				scope = scope,
				range = range,
				symbol_name = access.base_name,
				symbol_range = access.base_range,
				kind = .Is_Not_Assigned if negated else .Is_Assigned,
			},
		)
		return
	}
	if kind == .Initial || kind == .Bound {
		check_kind := Value_State_Check_Kind.Is_Initial
		if (kind == .Initial && negated) || (kind == .Bound && !negated) {
			check_kind = .Is_Not_Initial
		}
		field_name := ""
		if len(access.field_path) > 0 {
			field_name = access.field_path[len(access.field_path) - 1].name
		}
		append(
			&c.value_state_checks,
			Value_State_Check_Data {
				scope = scope,
				range = range,
				symbol_name = access.base_name,
				symbol_range = access.base_range,
				field_name = field_name,
				kind = check_kind,
			},
		)
	}
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
		&c.assignment_sites,
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
) {
	flags := Assignment_Site_Flags{}
	if has_lhs {
		flags += {.Has_Lhs_Target_Access}
	}
	append(
		&c.assignment_sites,
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
		add_routine_site_target(c, scope, stmt.range, .Clear, op.target)
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
		add_routine_site_target(c, scope, stmt.range, .Clear, op.target)
	}
}

collect_free_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Free_Stmt, scope: Scope_Id) {
	for op in stmt.operands {
		collect_expr_refs(c, op.target, scope)
		add_routine_site_target(c, scope, stmt.range, .Clear, op.target)
	}
	collect_expr_refs(c, stmt.memory_id, scope)
}

collect_unassign_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Unassign_Stmt, scope: Scope_Id) {
	for op in stmt.operands {
		collect_expr_refs(c, op.target, scope)
		add_routine_site_target(c, scope, stmt.range, .Unassign, op.target)
	}
}

collect_move_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Move_Stmt, scope: Scope_Id) {
	for entry in stmt.entries {
		collect_assignment_stmt_facts(c, stmt.range, entry.target, entry.source, scope, false)
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
	add_routine_site(c, scope, stmt.range, .Unknown_Effect)
	for e in stmt.entries {
		collect_expr_list_refs(c, e.sources[:], scope)
		collect_expr_refs(c, e.separator, scope)
		collect_write_target_expr(c, scope, stmt.range, e.target, stmt.range)
		if e.lines_of && len(e.sources) > 0 {
			append(
				&c.concatenate_lines_of_sites,
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
	add_routine_site(c, scope, stmt.range, .Unknown_Effect)
	for e in stmt.entries {
		collect_expr_refs(c, e.source, scope)
		collect_expr_refs(c, e.separator, scope)
		for target in e.targets {
			collect_expr_refs(c, target, scope)
		}
	}
}

collect_replace_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Replace_Stmt, scope: Scope_Id) {
	add_routine_site(c, scope, stmt.range, .Unknown_Effect)
	collect_expr_refs(c, stmt.pattern, scope)
	collect_expr_refs(c, stmt.target, scope)
	collect_expr_refs(c, stmt.replacement, scope)
}

collect_translate_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Translate_Stmt, scope: Scope_Id) {
	add_routine_site(c, scope, stmt.range, .Unknown_Effect)
	collect_expr_refs(c, stmt.target, scope)
	collect_expr_refs(c, stmt.operand, scope)
}

collect_shift_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Shift_Stmt, scope: Scope_Id) {
	add_routine_site(c, scope, stmt.range, .Unknown_Effect)
	collect_expr_refs(c, stmt.target, scope)
	collect_expr_refs(c, stmt.places, scope)
	collect_expr_refs(c, stmt.delete_pattern, scope)
}

collect_find_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Find_Stmt, scope: Scope_Id) {
	add_system_field_update(c, scope, stmt.range, .Find, "subrc")
	add_system_field_update(c, scope, stmt.range, .Find, "fdpos")
	collect_expr_refs(c, stmt.pattern, scope)
	collect_expr_refs(c, stmt.target, scope)
	collect_expr_refs(c, stmt.match_offset, scope)
	collect_expr_refs(c, stmt.match_length, scope)
	collect_expr_refs(c, stmt.results, scope)
	collect_expr_list_refs(c, stmt.submatches[:], scope)
	read_ranges := make([dynamic]tokenizer.Range, 0, 2, c.allocator)
	write_targets := make([dynamic]Find_Write_Target_Data, 0, 5, c.allocator)
	if stmt.pattern != nil {append(&read_ranges, stmt.pattern.range)}
	if stmt.target != nil {append(&read_ranges, stmt.target.range)}
	if stmt.match_offset !=
	   nil {append(&write_targets, Find_Write_Target_Data{range = stmt.match_offset.range, definitely_assigned = true})}
	if stmt.match_length !=
	   nil {append(&write_targets, Find_Write_Target_Data{range = stmt.match_length.range, definitely_assigned = true})}
	for submatch in stmt.submatches {
		if submatch !=
		   nil {append(&write_targets, Find_Write_Target_Data{range = submatch.range, definitely_assigned = true})}
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
	append(
		&c.find_sites,
		Find_Site_Data {
			scope = scope,
			range = stmt.range,
			read_ranges = read_ranges,
			write_targets = write_targets,
		},
	)
}

collect_search_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Search_Stmt, scope: Scope_Id) {
	add_system_field_update(c, scope, stmt.range, .Search, "subrc")
	add_system_field_update(c, scope, stmt.range, .Search, "fdpos")
	collect_expr_refs(c, stmt.target, scope)
	collect_expr_refs(c, stmt.pattern, scope)
	collect_expr_refs(c, stmt.starting_at, scope)
	collect_expr_refs(c, stmt.ending_at, scope)
}

collect_perform_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Perform_Stmt, scope: Scope_Id) {
	routine_name := ""
	routine_range := tokenizer.Range{}
	is_dynamic := true
	if name, range, ok := expr_name(stmt.form); ok {
		routine_name = canonical_name(name, c.allocator)
		routine_range = range
		is_dynamic = false
		if _, id_ok := stmt.form.derived_expr.(^ast.Ident_Expr); id_ok {
			add_reference(c, scope, routine_name, .Routine, .Routine_Call, range)
		} else {
			collect_expr_refs(c, stmt.form, scope)
		}
	}
	program := Perform_Program_Data{}
	flags := Perform_Call_Flags{}
	if is_dynamic {
		flags += {.Is_Dynamic}
	}
	if stmt.program != nil {
		flags += {.Has_Program}
		if name, range, ok := expr_name(stmt.program); ok {
			program = Perform_Program_Data {
				name  = canonical_name(name, c.allocator),
				range = range,
			}
			if _, id_ok := stmt.program.derived_expr.(^ast.Ident_Expr); !id_ok {
				program.is_dynamic = true
				collect_expr_refs(c, stmt.program, scope)
			}
		} else {
			program = Perform_Program_Data {
				name       = "<dynamic>",
				range      = stmt.program.range,
				is_dynamic = true,
			}
			collect_expr_refs(c, stmt.program, scope)
		}
	}
	if stmt.if_found {
		flags += {.Has_If_Found}
	}
	parameters := make(
		[dynamic]Perform_Parameter_Section,
		0,
		len(stmt.tables) + len(stmt.using_args) + len(stmt.changing),
		c.allocator,
	)
	arguments := make([dynamic]Perform_Argument_Data, 0, cap(parameters), c.allocator)
	append_perform_args(c, &parameters, &arguments, stmt.tables[:], scope, .Tables)
	append_perform_args(c, &parameters, &arguments, stmt.using_args[:], scope, .Using)
	append_perform_args(c, &parameters, &arguments, stmt.changing[:], scope, .Changing)
	append(
		&c.perform_calls,
		Perform_Call_Data {
			scope = scope,
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

append_perform_args :: proc(
	c: ^Collector,
	parameters: ^[dynamic]Perform_Parameter_Section,
	arguments: ^[dynamic]Perform_Argument_Data,
	values: []^ast.Expr,
	scope: Scope_Id,
	section: Perform_Parameter_Section,
) {
	for i in 0 ..< len(values) {
		collect_expr_refs(c, values[i], scope)
		append(parameters, section)
		append(
			arguments,
			Perform_Argument_Data {
				range = values[i].range,
				section = section,
				ordinal_in_section = i,
			},
		)
	}
}

collect_call_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Call_Stmt, scope: Scope_Id) {
	#partial switch stmt.kind {
	case .Direct:
		collect_expr_refs(c, stmt.call, scope)
	case .Function, .Customer_Function:
		add_system_field_update(c, scope, stmt.range, .Call_Function, "subrc")
		target_name := call_stmt_function_name(c, stmt)
		target := Named_Argument_Target {
			kind          = .Function,
			function_name = target_name,
		}
		collect_raw_call_stmt_args(c, stmt, scope, target)
	case .Method:
		target := call_stmt_method_target(c, stmt, scope)
		collect_expr_refs(c, stmt.target, scope)
		collect_raw_call_stmt_args(c, stmt, scope, target)
	case .Transaction:
		add_system_field_update(c, scope, stmt.range, .Call_Function, "subrc")
		collect_expr_refs(c, stmt.target, scope)
		collect_expr_list_refs(c, stmt.transaction_operands[:], scope)
	case:
		collect_expr_refs(c, stmt.target, scope)
	}
}

call_stmt_function_name :: proc(c: ^Collector, stmt: ^ast.Call_Stmt) -> string {
	if stmt.target == nil {
		return ""
	}
	text := strings.trim_space(expr_display(c, stmt.target))
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
	if access, ok := selector_access_from_expr(c, stmt.target, scope, false);
	   ok && len(access.field_path) > 0 {
		return Named_Argument_Target {
			kind = .Method,
			base_namespace = access.base_namespace,
			base_name = access.base_name,
			method_name = access.field_path[len(access.field_path) - 1].name,
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
			return Named_Argument_Target {
				kind = .Method,
				base_namespace = namespace,
				base_name = canonical_name(ref.name, c.allocator),
				method_name = canonical_name(ref.path[len(ref.path) - 1].name, c.allocator),
				interface_qualified = namespace == .Type,
			}
		}
	}
	if name, _, ok := expr_name(stmt.target); ok {
		return Named_Argument_Target {
			kind = .Implicit_Method,
			method_name = canonical_name(name, c.allocator),
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
	args := make([dynamic]Call_Argument_Data, 0, 4, c.allocator)
	ordinal := 0
	for arg in stmt.named_args {
		section, valid_section := named_argument_section_from_ast(arg.section)
		has_section := arg.has_section && valid_section
		name := canonical_name(arg.name, c.allocator)
		append(
			&c.named_arguments,
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
			collect_raw_operand_fact_refs(c, arg.raw_decls[:], arg.raw_refs[:], scope)
			append(
				&args,
				Call_Argument_Data {
					range = arg.value_range,
					name = name,
					section = section,
					has_section = has_section,
					ordinal = ordinal,
				},
			)
			ordinal += 1
		}
	}
	append(
		&c.call_sites,
		Call_Site_Data{scope = scope, range = stmt.range, target = target, arguments = args},
	)
}

collect_submit_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Submit_Stmt, scope: Scope_Id) {
	collect_expr_refs(c, stmt.target, scope)
	for option in stmt.options {
		collect_expr_refs(c, option.value, scope)
		collect_expr_refs(c, option.high_value, scope)
	}
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
	add_system_field_update(c, scope, stmt.range, .Message, "msgid")
	add_system_field_update(c, scope, stmt.range, .Message, "msgno")
	add_system_field_update(c, scope, stmt.range, .Message, "msgty")
	add_system_field_update(c, scope, stmt.range, .Message, "msgv1")
	add_system_field_update(c, scope, stmt.range, .Message, "msgv2")
	add_system_field_update(c, scope, stmt.range, .Message, "msgv3")
	add_system_field_update(c, scope, stmt.range, .Message, "msgv4")
	append(&c.message_uses, use)
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
		} else if c.has_message_default_class {
			use.class_name = c.message_default_class.name
			use.class_range = c.message_default_class.range
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

collect_flow_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Flow_Stmt, scope: Scope_Id) {
	kind := Routine_Site_Kind.Return
	#partial switch stmt.kind {
	case .Return:
		kind = .Return
	case .Continue:
		kind = .Continue
	case .Exit:
		kind = .Exit
	case .Stop:
		kind = .Stop
	}
	add_routine_site(c, scope, stmt.range, kind)
}

collect_describe_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Describe_Stmt, scope: Scope_Id) {
	add_system_field_update(c, scope, stmt.range, .Describe_Table, "tfill")
	add_system_field_update(c, scope, stmt.range, .Describe_Table, "tleng")
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
	add_routine_site(c, scope, stmt.range, .Unknown_Effect)
}

collect_raise_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Raise_Stmt, scope: Scope_Id) {
	if stmt.target_type {
		collect_type_expr_ref(c, stmt.target, scope, .Type)
	} else {
		collect_expr_refs(c, stmt.target, scope)
	}
	collect_expr_list_refs(c, stmt.operands[:], scope)
	add_routine_site(c, scope, stmt.range, .Raise)
}

collect_authority_check_stmt_facts :: proc(
	c: ^Collector,
	stmt: ^ast.Authority_Check_Stmt,
	scope: Scope_Id,
) {
	add_system_field_update(c, scope, stmt.range, .Authority_Check, "subrc")
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
	add_system_field_update(c, scope, stmt.range, .Assign, "subrc")
	collect_expr_list_refs(c, stmt.operands[:], scope)
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
	add_system_field_update(c, scope, stmt.range, .Loop_At, "subrc")
	add_system_field_update(c, scope, stmt.range, .Loop_At, "tabix")
	add_system_field_update(c, scope, stmt.range, .Loop_At, "tfill")
	add_system_field_update(c, scope, stmt.range, .Loop_At, "tleng")
	collect_expr_refs(c, stmt.source, scope)
	source_access, has_source := value_access_from_expr(c, stmt.source, scope)
	previous := c.current_scope
	c.current_scope = scope
	loop_scope := push_scope(c, .Loop_Block, stmt.range)
	if has_source {
		append(&c.loop_source_stack, source_access)
		c.scopes[scope_id_index(loop_scope)].allows_internal_table_line_selector = true
	}
	walk_stmt_list(c, stmt.body, loop_scope)
	if has_source {
		_ = pop_loop_source(c)
	}
	c.current_scope = loop_scope
	pop_scope(c)
	c.current_scope = previous
	add_loop_region_with_access(
		c,
		scope,
		stmt.range,
		.Loop,
		loop_scope,
		source_access,
		has_source,
		Field_Access{},
		false,
	)
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
	collect_expr_refs(c, stmt.expr, at_scope)
	if stmt.expr != nil && len(c.loop_source_stack) > 0 {
		source := c.loop_source_stack[len(c.loop_source_stack) - 1]
		append(
			&c.loop_at_field_contexts,
			Loop_At_Field_Context {
				scope = at_scope,
				range = stmt.expr.range,
				source_access = source,
			},
		)
	}
	walk_stmt_list(c, stmt.body, at_scope)
	c.current_scope = at_scope
	pop_scope(c)
	c.current_scope = previous
	kind := At_Group_Kind.First
	switch stmt.kind {
	case .First:
		kind = .First
	case .Last:
		kind = .Last
	case .New:
		kind = .New
	case .End_Of:
		kind = .End_Of
	}
	add_at_region(c, scope, stmt.range, kind, at_scope)
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

add_if_region :: proc(
	c: ^Collector,
	scope: Scope_Id,
	range: tokenizer.Range,
	then_scope: Scope_Id,
	elseif_scopes: [dynamic]Scope_Id,
	else_scope: Scope_Id,
) {
	append(
		&c.routine_control_regions,
		Routine_Control_Region_Data {
			kind = .If,
			if_ = If_Region_Data {
				scope = scope,
				range = range,
				then_scope = then_scope,
				elseif_scopes = elseif_scopes,
				else_scope = else_scope,
			},
		},
	)
}

add_case_region :: proc(
	c: ^Collector,
	scope: Scope_Id,
	range: tokenizer.Range,
	when_scopes: [dynamic]Scope_Id,
	has_others: bool,
) {
	append(
		&c.routine_control_regions,
		Routine_Control_Region_Data {
			kind = .Case,
			case_ = Case_Region_Data {
				scope = scope,
				range = range,
				when_scopes = when_scopes,
				has_when_others = has_others,
			},
		},
	)
}

add_loop_region :: proc(
	c: ^Collector,
	scope: Scope_Id,
	range: tokenizer.Range,
	kind: Routine_Loop_Kind,
	body_scope: Scope_Id,
) {
	add_loop_region_with_access(
		c,
		scope,
		range,
		kind,
		body_scope,
		Field_Access{},
		false,
		Field_Access{},
		false,
	)
}

add_loop_region_with_access :: proc(
	c: ^Collector,
	scope: Scope_Id,
	range: tokenizer.Range,
	kind: Routine_Loop_Kind,
	body_scope: Scope_Id,
	source: Field_Access,
	has_source: bool,
	target: Field_Access,
	has_target: bool,
) {
	flags := Loop_Region_Flags{}
	if has_source {flags += {.Has_Source_Access}}
	if has_target {flags += {.Has_Target_Access}}
	append(
		&c.routine_control_regions,
		Routine_Control_Region_Data {
			kind = .Loop,
			loop = Loop_Region_Data {
				scope = scope,
				range = range,
				kind = kind,
				body_scope = body_scope,
				source_access = source,
				target_access = target,
				flags = flags,
			},
		},
	)
}

add_at_region :: proc(
	c: ^Collector,
	scope: Scope_Id,
	range: tokenizer.Range,
	kind: At_Group_Kind,
	body_scope: Scope_Id,
) {
	append(
		&c.routine_control_regions,
		Routine_Control_Region_Data {
			kind = .At,
			at = At_Region_Data {
				scope = scope,
				range = range,
				kind = kind,
				body_scope = body_scope,
			},
		},
	)
}

add_try_region :: proc(
	c: ^Collector,
	scope: Scope_Id,
	range: tokenizer.Range,
	body_scope: Scope_Id,
	catch_scopes: [dynamic]Scope_Id,
	cleanup_scope: Scope_Id,
) {
	append(
		&c.routine_control_regions,
		Routine_Control_Region_Data {
			kind = .Try,
			try = Try_Region_Data {
				scope = scope,
				range = range,
				body_scope = body_scope,
				catch_scopes = catch_scopes,
				cleanup_scope = cleanup_scope,
			},
		},
	)
}

add_routine_site :: proc(
	c: ^Collector,
	scope: Scope_Id,
	range: tokenizer.Range,
	kind: Routine_Site_Kind,
) {
	append(&c.routine_sites, Routine_Site_Data{scope = scope, range = range, kind = kind})
}

add_routine_site_target :: proc(
	c: ^Collector,
	scope: Scope_Id,
	range: tokenizer.Range,
	kind: Routine_Site_Kind,
	target: ^ast.Expr,
) {
	site := Routine_Site_Data {
		scope = scope,
		range = range,
		kind  = kind,
	}
	if target != nil {
		site.target_range = target.range
		site.has_target = true
	}
	append(&c.routine_sites, site)
}

add_system_field_update :: proc(
	c: ^Collector,
	scope: Scope_Id,
	range: tokenizer.Range,
	statement: System_Field_Statement_Kind,
	field_name: string,
) {
	append(
		&c.system_field_updates,
		System_Field_Update_Data {
			scope = scope,
			range = range,
			statement = statement,
			field_name = strings.clone(field_name, c.allocator),
		},
	)
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
		add_system_field_update(c, scope, stmt.range, .Read_Report, "subrc")
		collect_expr_refs(c, stmt.name, scope)
		collect_write_target_expr(c, scope, stmt.range, stmt.source)
		collect_expr_refs(c, stmt.line_size, scope)
		collect_expr_refs(c, stmt.line_count, scope)
	case .Insert_Report:
		add_system_field_update(c, scope, stmt.range, .Insert_Report, "subrc")
		add_routine_site(c, scope, stmt.range, .Unknown_Effect)
		collect_expr_refs(c, stmt.name, scope)
		collect_expr_refs(c, stmt.source, scope)
		collect_expr_refs(c, stmt.line_size, scope)
		collect_expr_refs(c, stmt.line_count, scope)
	case .Delete_Report:
		add_system_field_update(c, scope, stmt.range, .Delete_Report, "subrc")
		add_routine_site(c, scope, stmt.range, .Unknown_Effect)
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
	c.message_default_class = Message_Class_Use_Data {
		name  = class_name,
		range = range,
	}
	c.has_message_default_class = true
	add_reference(c, scope, class_name, .Value, .Message_Class, range)
}

collect_read_table_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Read_Table_Stmt, scope: Scope_Id) {
	add_system_field_update(c, scope, stmt.range, .Read_Table, "subrc")
	add_system_field_update(c, scope, stmt.range, .Read_Table, "tabix")
	for e in stmt.entries {
		collect_expr_refs(c, e.table, scope)
		collect_expr_refs(c, e.into, scope)
		collect_expr_refs(c, e.assigning, scope)
		collect_expr_refs(c, e.reference_into, scope)
		collect_expr_refs(c, e.index, scope)
		collect_expr_refs(c, e.using_key, scope)
		collect_expr_list_refs(c, e.comparing[:], scope)
		for key in e.key_values {
			collect_expr_refs(c, key.value, scope)
			if e.table != nil {
				access, ok := value_access_from_expr(c, e.table, scope)
				if ok {
					segments := make([dynamic]Field_Access_Segment, 0, 1, c.allocator)
					append(
						&segments,
						Field_Access_Segment {
							name = canonical_name(key.name, c.allocator),
							range = key.value.range if key.value != nil else e.table.range,
						},
					)
					append(
						&c.field_accesses,
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
		if e.binary_search && e.table != nil {
			key_fields := make([dynamic]string, 0, len(e.key_values), c.allocator)
			for key in e.key_values {
				append(&key_fields, canonical_name(key.name, c.allocator))
			}
			name := ""
			if access, ok := value_access_from_expr(c, e.table, scope); ok {
				name = table_order_name_from_access(c, access)
			}
			record_read_table_binary_search(
				c,
				scope,
				e.binary_search_clause,
				name,
				key_fields[:],
			)
		}
	}
}

collect_insert_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Insert_Stmt, scope: Scope_Id) {
	query_id := -1
	is_sql := false
	if stmt.form == .Db_Table {
		add_system_field_update(c, scope, stmt.range, .Insert_Db_Table, "subrc")
		query_id, is_sql = collect_db_table_sql_source(c, stmt.range, stmt.target, scope, nil, tokenizer.Range{}, false)
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
		add_system_field_update(c, scope, stmt.range, .Insert_Table, "subrc")
		collect_expr_refs(c, stmt.target, scope)
	}
	collect_expr_refs(c, stmt.source, scope)
	collect_expr_refs(c, stmt.index, scope)
	collect_expr_refs(c, stmt.assigning, scope)
	collect_expr_refs(c, stmt.reference_into, scope)
	for a in stmt.assignments {
		if is_sql && a.column_name != "" {
			push_sql_name_ref(c, query_id, scope, a.column_range, a.column_name, "", .Column, .Unresolved)
		} else if is_sql {
			collect_sql_name_refs_from_expr(c, query_id, a.name, scope, false)
		} else {
			collect_expr_refs(c, a.name, scope)
		}
		collect_expr_refs(c, a.value, scope)
	}
}

collect_append_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Append_Stmt, scope: Scope_Id) {
	add_system_field_update(c, scope, stmt.range, .Append, "subrc")
	collect_expr_refs(c, stmt.source, scope)
	collect_expr_refs(c, stmt.target, scope)
	collect_expr_refs(c, stmt.assigning, scope)
	collect_expr_refs(c, stmt.reference_into, scope)
}

collect_modify_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Modify_Stmt, scope: Scope_Id) {
	is_sql := false
	if !stmt.table_keyword {
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
	add_system_field_update(
		c,
		scope,
		stmt.range,
		.Modify_Db_Table if is_sql else .Modify_Table,
		"subrc",
	)
	if !is_sql {
		collect_expr_refs(c, stmt.target, scope)
		collect_expr_refs(c, stmt.where_cond, scope)
	}
	collect_expr_refs(c, stmt.source, scope)
	collect_expr_refs(c, stmt.index, scope)
	collect_expr_list_refs(c, stmt.transporting[:], scope)
}

collect_sort_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Sort_Stmt, scope: Scope_Id) {
	collect_expr_refs(c, stmt.target, scope)
	collect_expr_list_refs(c, stmt.fields[:], scope)
	if access, ok := value_access_from_expr(c, stmt.target, scope);
	   ok && len(stmt.fields) > 0 && !stmt.descending {
		keys := make([dynamic]string, 0, len(stmt.fields), c.allocator)
		for field in stmt.fields {
			if name, _, name_ok := expr_name(field); name_ok {
				append(&keys, canonical_name(name, c.allocator))
			}
		}
		if len(keys) > 0 {
			record_internal_table_order(
				c,
				scope,
				stmt.range,
				table_order_name_from_access(c, access),
				keys[:],
			)
		}
	}
}

collect_update_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Update_Stmt, scope: Scope_Id) {
	add_system_field_update(c, scope, stmt.range, .Update_Db_Table, "subrc")
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
			push_sql_name_ref(c, query_id, scope, a.column_range, a.column_name, "", .Column, .Unresolved)
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
	if stmt.form != .Adjacent_Duplicates {
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
	add_system_field_update(
		c,
		scope,
		stmt.range,
		.Delete_Db_Table if is_sql else .Delete_Table,
		"subrc",
	)
	if !is_sql {
		collect_expr_refs(c, stmt.target, scope)
	}
	collect_expr_refs(c, stmt.source, scope)
	collect_expr_refs(c, stmt.index, scope)
	collect_expr_refs(c, stmt.using_key, scope)
	collect_expr_list_refs(c, stmt.comparing[:], scope)
	add_routine_site(c, scope, stmt.range, .Delete)
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
	add_routine_site(c, scope, stmt.range, .Unknown_Effect)
}

collect_textpool_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Textpool_Stmt, scope: Scope_Id) {
	collect_expr_refs(c, stmt.program, scope)
	if stmt.kind == .Read {
		collect_write_target_expr(c, scope, stmt.range, stmt.table)
	} else {
		if stmt.kind == .Insert {
			add_system_field_update(c, scope, stmt.range, .Insert_Textpool, "subrc")
			add_routine_site(c, scope, stmt.range, .Unknown_Effect)
		}
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
	add_routine_site(c, scope, stmt.range, .Unknown_Effect)
}
