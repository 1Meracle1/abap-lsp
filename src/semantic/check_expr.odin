package abap_frontend_semantic2

import "src:ast"

import "core:strings"

Operand :: struct {
	mode:   ast.Addressing_Mode,
	type:   ^Type,
	value:  ast.Exact_Value_Id,
	expr:   ^ast.Node,
	entity: ^Entity,
}

Checker_Reduce_Validation_State :: struct {
	init_seen:  bool,
	for_seen:   bool,
	next_seen:  bool,
	init_names: [dynamic]string,
}

checker_check_expr :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	namespace: Namespace = .Value,
	lhs := false,
) -> Operand {
	if expr == nil {
		return checker_invalid_operand()
	}
	node := &expr.expr_base
	#partial switch n in expr.derived_expr {
	case ^ast.Bad_Expr:
		return checker_record_operand(ctx, node, .Invalid, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Ident_Expr:
		return checker_check_ident_expr(ctx, node, n.name, namespace, lhs)
	case ^ast.Literal_Expr:
		return checker_record_operand(ctx, node, .Constant, checker_type_for_literal(ctx, n.value), lhs = lhs)
	case ^ast.Macro_Arg_Ref_Expr:
		mode := ast.Addressing_Mode.Variable if lhs else ast.Addressing_Mode.Value
		return checker_record_operand(ctx, node, mode, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Char_String_Template_Expr:
		for part in n.parts {
			checker_check_expr(ctx, part)
		}
		return checker_record_operand(ctx, node, .Value, checker_builtin_type_from_name(ctx.checker, "string"), lhs = lhs)
	case ^ast.Template_Literal_Expr:
		return checker_record_operand(ctx, node, .Constant, checker_builtin_type_from_name(ctx.checker, "string"), lhs = lhs)
	case ^ast.Template_Interpolation_Expr:
		checker_check_expr(ctx, n.expr)
		for spec in n.format_specs {
			checker_check_expr(ctx, spec)
		}
		return checker_record_operand(ctx, node, .Value, checker_builtin_type_from_name(ctx.checker, "string"), lhs = lhs)
	case ^ast.Template_Expr:
		operand := checker_check_expr(ctx, n.expr, namespace, lhs)
		return checker_record_operand(ctx, node, operand.mode, operand.type, operand.entity, lhs = lhs)
	case ^ast.Template_Format_Spec_Expr:
		if !checker_template_format_spec_value_is_literal(n) {
			checker_check_expr(ctx, n.value)
		}
		return checker_record_operand(ctx, node, .No_Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Type_Ref_Expr:
		if n.raw_operand {
			return checker_check_raw_operand_expr(ctx, node, n, lhs)
		}
		if namespace == .Type {
			typ, entity := checker_type_from_expr(ctx, expr, .Type)
			return checker_record_operand(ctx, node, .Type, typ, entity, lhs)
		}
		name := n.base_name.text
		if name == "" {
			name = n.name.text
		}
		return checker_check_ident_expr(ctx, node, name, namespace, lhs)
	case ^ast.Binary_Expr:
		left := checker_check_expr(ctx, n.left)
		right := checker_check_expr(ctx, n.right)
		return checker_record_operand(ctx, node, .Value, checker_binary_result_type(ctx, n.op, left, right), lhs = lhs)
	case ^ast.Unary_Expr:
		operand := checker_check_expr(ctx, n.expr)
		return checker_record_operand(ctx, node, .Value, operand.type, lhs = lhs)
	case ^ast.Paren_Expr:
		operand := checker_check_expr(ctx, n.expr, namespace, lhs)
		return checker_record_operand(ctx, node, operand.mode, operand.type, operand.entity, lhs = lhs)
	case ^ast.Dynamic_Call_Method_Target_Expr:
		checker_check_expr(ctx, n.base)
		checker_check_expr(ctx, n.method)
		return checker_record_operand(ctx, node, .Method, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Ole_Call_Method_Target_Expr:
		checker_check_expr(ctx, n.object)
		checker_check_expr(ctx, n.member)
		checker_check_expr(ctx, n.result, .Value, true)
		return checker_record_operand(ctx, node, .Method, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Host_Expr:
		return checker_check_host_expr(ctx, node, n, namespace, lhs)
	case ^ast.Table_Expr:
		return checker_check_table_expr(ctx, node, n, lhs)
	case ^ast.Selector_Expr:
		return checker_check_selector_expr(ctx, node, n, namespace, lhs)
	case ^ast.Interface_Qualified_Selector_Expr:
		return checker_check_interface_selector_expr(ctx, node, n, namespace, lhs)
	case ^ast.Substring_Expr:
		base := checker_check_expr(ctx, n.base, namespace, lhs)
		checker_check_expr(ctx, n.offset)
		checker_check_expr(ctx, n.length)
		mode := base.mode if lhs else ast.Addressing_Mode.Value
		return checker_record_operand(ctx, node, mode, base.type, base.entity, lhs = lhs)
	case ^ast.Call_Expr:
		callee := checker_check_expr(ctx, n.callee, .Routine)
		checker_check_call_expr_arguments(ctx, n, callee)
		if callee.entity != nil && callee.entity.kind == .Builtin {
			return checker_check_builtin_call(ctx, node, callee.entity, n, lhs)
		}
		return checker_record_operand(ctx, node, .Value, checker_call_result_type(ctx, callee), lhs = lhs)
	case ^ast.Call_Arg_List_Expr:
		for arg in n.args {
			checker_check_expr(ctx, arg)
		}
		return checker_record_operand(ctx, node, .No_Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Call_Arg_Section_Expr:
		for arg in n.args {
			checker_check_expr(ctx, arg)
		}
		return checker_record_operand(ctx, node, .No_Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Call_Named_Arg_Expr:
		checker_check_expr(ctx, n.value)
		return checker_record_operand(ctx, node, .No_Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Call_Positional_Arg_Expr:
		checker_check_expr(ctx, n.value)
		return checker_record_operand(ctx, node, .No_Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Sql_Column_Expr, ^ast.Sql_Star_Expr:
		return checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Sql_Call_Expr:
		for arg in n.args {
			checker_check_expr(ctx, arg)
		}
		return checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Constructor_Expr:
		return checker_check_constructor_expr(ctx, node, n, lhs)
	case ^ast.Is_Predicate_Expr:
		checker_check_expr(ctx, n.subject)
		return checker_record_operand(ctx, node, .Value, checker_builtin_type_from_name(ctx.checker, "abap_bool"), lhs = lhs)
	case ^ast.Instance_Of_Predicate_Expr:
		checker_check_expr(ctx, n.subject)
		checker_check_expr(ctx, n.type_ref, .Type)
		return checker_record_operand(ctx, node, .Value, checker_builtin_type_from_name(ctx.checker, "abap_bool"), lhs = lhs)
	case ^ast.Between_Expr:
		checker_check_expr(ctx, n.subject)
		checker_check_expr(ctx, n.low)
		checker_check_expr(ctx, n.high)
		return checker_record_operand(ctx, node, .Value, checker_builtin_type_from_name(ctx.checker, "abap_bool"), lhs = lhs)
	case ^ast.Sql_Case_When_Expr:
		checker_check_expr(ctx, n.condition)
		result := checker_check_expr(ctx, n.result)
		return checker_record_operand(ctx, node, .Value, result.type, lhs = lhs)
	case ^ast.Sql_Case_Expr:
		checker_check_expr(ctx, n.operand)
		result_type := project_type_unknown(ctx.project)
		for arm in n.whens {
			when_operand := checker_check_expr(ctx, arm)
			if checker_type_is_unknown(result_type) {
				result_type = when_operand.type
			}
		}
		else_operand := checker_check_expr(ctx, n.else_expr)
		if checker_type_is_unknown(result_type) {
			result_type = else_operand.type
		}
		return checker_record_operand(ctx, node, .Value, result_type, lhs = lhs)
	case ^ast.Let_Expr:
		return checker_check_let_expr(ctx, node, n, lhs)
	case ^ast.Constructor_Let_Binding_Expr:
		value := checker_check_expr(ctx, n.value)
		checker_collect_inferred_expr_decl(ctx, n.name.text, .Variable, n.name.range, node, value.type)
		return checker_record_operand(ctx, node, .No_Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Constructor_When_Clause_Expr:
		checker_check_expr(ctx, n.condition)
		result := checker_check_expr(ctx, n.result)
		return checker_record_operand(ctx, node, .Value, result.type, lhs = lhs)
	case ^ast.Constructor_Else_Clause_Expr:
		result := checker_check_expr(ctx, n.result)
		return checker_record_operand(ctx, node, .Value, result.type, lhs = lhs)
	case ^ast.Constructor_For_Clause_Expr:
		return checker_check_constructor_for_clause_expr(ctx, node, n, lhs)
	case ^ast.Constructor_Where_Clause_Expr:
		checker_check_expr(ctx, n.condition)
		return checker_record_operand(ctx, node, .No_Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Constructor_Filter_Except_In_Clause_Expr:
		checker_check_expr(ctx, n.source)
		checker_check_expr(ctx, n.where_clause)
		return checker_record_operand(ctx, node, .No_Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Constructor_Init_Clause_Expr:
		for assignment in n.assignments {
			checker_check_expr(ctx, assignment)
		}
		return checker_record_operand(ctx, node, .No_Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Constructor_Next_Clause_Expr:
		for assignment in n.assignments {
			checker_check_expr(ctx, assignment)
		}
		return checker_record_operand(ctx, node, .No_Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Constructor_Named_Assignment_Expr:
		value := checker_check_expr(ctx, n.value)
		return checker_record_operand(ctx, node, .No_Value, value.type, lhs = lhs)
	case ^ast.Constructor_Base_Clause_Expr:
		value := checker_check_expr(ctx, n.value)
		return checker_record_operand(ctx, node, .No_Value, value.type, lhs = lhs)
	case ^ast.Constructor_Lines_Of_Clause_Expr:
		source := checker_check_expr(ctx, n.source)
		checker_check_expr(ctx, n.from)
		checker_check_expr(ctx, n.to)
		return checker_record_operand(ctx, node, .Value, source.type, lhs = lhs)
	case ^ast.Constructor_Optional_Expr:
		value := checker_check_expr(ctx, n.value)
		return checker_record_operand(ctx, node, .Optional_Ok, value.type, value.entity, lhs = lhs)
	case ^ast.Constructor_Corresponding_Mapping_Clause_Expr:
		for assignment in n.assignments {
			checker_check_expr(ctx, assignment)
		}
		return checker_record_operand(ctx, node, .No_Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Constructor_Corresponding_Mapping_Assignment_Expr:
		checker_check_expr(ctx, n.source)
		checker_check_expr(ctx, n.default_value)
		checker_check_expr(ctx, n.mapping)
		checker_check_expr(ctx, n.except)
		return checker_record_operand(ctx, node, .No_Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Constructor_Corresponding_Except_Clause_Expr:
		for name in n.names {
			checker_check_expr(ctx, name)
		}
		return checker_record_operand(ctx, node, .No_Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Data_Inline_Name_Expr:
		entity := checker_collect_inferred_expr_decl(ctx, n.name.text, .Variable, n.name.range, node, ctx.type_hint)
		return checker_record_operand(ctx, node, .Variable, entity.type if entity != nil else project_type_unknown(ctx.project), entity, lhs)
	case ^ast.Field_Symbol_Inline_Name_Expr:
		entity := checker_collect_inferred_expr_decl(ctx, n.name.text, .Field_Symbol, n.name.range, node, ctx.type_hint)
		return checker_record_operand(ctx, node, .Variable, entity.type if entity != nil else project_type_unknown(ctx.project), entity, lhs)
	}
	return checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project), lhs = lhs)
}

checker_template_format_spec_value_is_literal :: proc(spec: ^ast.Template_Format_Spec_Expr) -> bool {
	if spec == nil {
		return false
	}
	option, has_option := spec.option.?
	if !has_option {
		return false
	}
	value, ok := checker_template_format_value_name(spec.value)
	if !ok {
		return false
	}
	#partial switch option {
	case .Alpha:
		return strings.equal_fold(value, "IN") ||
		       strings.equal_fold(value, "OUT") ||
		       strings.equal_fold(value, "RAW")
	case .Align:
		return strings.equal_fold(value, "LEFT") ||
		       strings.equal_fold(value, "RIGHT") ||
		       strings.equal_fold(value, "CENTER")
	case .Date, .Time:
		return strings.equal_fold(value, "RAW") ||
		       strings.equal_fold(value, "ISO") ||
		       strings.equal_fold(value, "USER") ||
		       strings.equal_fold(value, "ENVIRONMENT")
	case .Timestamp:
		return strings.equal_fold(value, "SPACE") ||
		       strings.equal_fold(value, "ISO") ||
		       strings.equal_fold(value, "USER") ||
		       strings.equal_fold(value, "ENVIRONMENT")
	}
	return false
}

checker_template_format_value_name :: proc(expr: ^ast.Expr) -> (string, bool) {
	if expr == nil {
		return "", false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		return n.name, n.name != ""
	case ^ast.Type_Ref_Expr:
		if n.raw_operand || n.name.text == "" || n.base_name.text != "" || len(n.path) > 0 {
			return "", false
		}
		return n.name.text, true
	}
	return "", false
}

checker_check_host_expr :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	expr: ^ast.Host_Expr,
	namespace: Namespace,
	lhs: bool,
) -> Operand {
	operand := checker_check_expr(ctx, expr.value, namespace, lhs)
	if checker_type_is_unknown(operand.type) && operand.entity == nil {
		if name, range, ok := checker_simple_unresolved_variable_expr(expr.value); ok {
			checker_add_unresolved_variable_diagnostic(ctx, name, range)
		}
	}
	return checker_record_operand(ctx, node, operand.mode, operand.type, operand.entity, lhs = lhs)
}

checker_simple_unresolved_variable_expr :: proc(expr: ^ast.Expr) -> (string, Range, bool) {
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
	case ^ast.Paren_Expr:
		return checker_simple_unresolved_variable_expr(n.expr)
	case ^ast.Host_Expr:
		return checker_simple_unresolved_variable_expr(n.value)
	}
	return "", {}, false
}

checker_unresolved_variable_message :: proc(name: string) -> string {
	if name == "" {
		return "unresolved variable"
	}
	builder := strings.builder_make(context.temp_allocator)
	strings.write_string(&builder, "unresolved variable ")
	strings.write_string(&builder, name)
	return strings.to_string(builder)
}

checker_add_unresolved_variable_diagnostic :: proc(ctx: ^Checker_Context, name: string, range: Range) {
	message := checker_unresolved_variable_message(name)
	if checker_diagnostic_present(ctx.info.diagnostics[:], .Unresolved_Reference, range, ctx.file, message) {
		return
	}
	checker_add_diagnostic(ctx, .Unresolved_Reference, range, message)
}

checker_check_unresolved_named_operand :: proc(
	ctx: ^Checker_Context,
	name: string,
	range: Range,
	operand: Operand,
) -> bool {
	if !checker_type_is_unknown(operand.type) || operand.entity != nil || name == "" {
		return false
	}
	checker_add_unresolved_variable_diagnostic(ctx, name, range)
	return true
}

checker_should_diagnose_unresolved_value_operand :: proc(ctx: ^Checker_Context) -> bool {
	return ctx.diagnose_unresolved_value_refs || ctx.current_routine != nil
}

checker_check_ident_expr :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	name: string,
	namespace: Namespace,
	lhs: bool,
	use_range: Range = {},
) -> Operand {
	entity, ok := checker_check_ident_name(ctx, node, name, namespace, lhs, use_range)
	if !ok {
		return checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project), lhs = lhs)
	}
	return checker_record_entity_operand(ctx, node, entity, lhs, record_use = false)
}

checker_check_ident_name :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	name: string,
	namespace: Namespace,
	lhs: bool,
	use_range: Range = {},
) -> (^Entity, bool) {
	_ = lhs
	if entity, ok, handled := checker_check_oop_receiver_ident(ctx, node, name, namespace, use_range); handled {
		return entity, ok
	}
	interned := project_intern_lower_ascii(ctx.project, name)
	if interned == "" {
		return nil, false
	}
	_, entity, ok := checker_lookup_reference(ctx, namespace, interned)
	if !ok {
		if namespace == .Value &&
		   checker_diagnose_constructor_for_iterator_reference(
			ctx,
			name,
			checker_use_range(node, use_range),
		   ) {
			return nil, false
		}
		kind := External_Candidate_Kind.Global_Symbol
		reason := External_Candidate_Reason.Unresolved_Reference
		if namespace == .Type {
			reason = .Unresolved_Type
		} else if namespace == .Routine {
			reason = .Unresolved_Routine
		}
		checker_add_unresolved_candidate(
			ctx,
			interned,
			namespace,
			kind,
			.Identifier,
			reason,
			checker_use_range(node, use_range),
			node,
		)
		if namespace == .Value && ctx.diagnose_unresolved_value_refs {
			checker_add_unresolved_variable_diagnostic(ctx, name, checker_use_range(node, use_range))
		}
		return nil, false
	}
	if namespace == .Value &&
	   ctx.diagnose_unresolved_value_refs &&
	   !entity_kind_occupies(entity.kind, .Value) {
		checker_add_unresolved_variable_diagnostic(ctx, name, checker_use_range(node, use_range))
		return nil, false
	}
	checker_add_entity_use_precise(ctx, node, entity, use_range)
	checker_check_entity_for_operand(ctx, entity)
	return entity, true
}

checker_use_range :: proc(node: ^ast.Node, range: Range) -> Range {
	if range.end > range.start {
		return range
	}
	return node.range if node != nil else Range{}
}

checker_add_entity_use_precise :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	entity: ^Entity,
	range: Range,
) {
	if range.end > range.start {
		checker_add_entity_use_at_range(ctx, node, entity, range)
		return
	}
	checker_add_entity_use(ctx, node, entity)
}

checker_record_expr_info :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	mode: ast.Addressing_Mode,
	typ: ^Type,
	value: ast.Exact_Value_Id = ast.INVALID_EXACT_VALUE_ID,
	is_lhs := false,
) -> Checker_Expr_Info {
	info := Checker_Expr_Info {
		mode   = mode,
		is_lhs = is_lhs,
		type   = typ if typ != nil else project_type_unknown(ctx.project),
		value  = value,
	}
	if node != nil {
		append(&ctx.info.expr_infos, Checker_Expr_Record{node = node, file = ctx.file, info = info})
	}
	return info
}

checker_record_operand :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	mode: ast.Addressing_Mode,
	typ: ^Type,
	entity: ^Entity = nil,
	lhs := false,
	value: ast.Exact_Value_Id = ast.INVALID_EXACT_VALUE_ID,
) -> Operand {
	info := checker_record_expr_info(ctx, node, mode, typ, value, lhs)
	return Operand {
		mode   = info.mode,
		type   = info.type,
		value  = info.value,
		expr   = node,
		entity = entity,
	}
}

checker_record_entity_operand :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	entity: ^Entity,
	lhs := false,
	record_use := true,
	use_range: Range = {},
) -> Operand {
	assert(entity != nil)
	if record_use {
		checker_add_entity_use_precise(ctx, node, entity, use_range)
	}
	checker_check_entity_for_operand(ctx, entity)
	typ := entity.type if entity.type != nil else project_type_unknown(ctx.project)
	return checker_record_operand(ctx, node, checker_addressing_mode_for_entity(entity), typ, entity, lhs = lhs)
}

checker_invalid_operand :: proc() -> Operand {
	return Operand{mode = .Invalid, value = ast.INVALID_EXACT_VALUE_ID}
}

checker_check_selector_expr :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	expr: ^ast.Selector_Expr,
	namespace: Namespace,
	lhs: bool,
) -> Operand {
	base_namespace := Namespace.Type if expr.op == .Fat_Arrow || expr.op == .Tilde else Namespace.Value
	base := checker_check_expr(ctx, expr.base, base_namespace)
	if base_namespace == .Value && checker_should_diagnose_unresolved_value_operand(ctx) {
		checker_check_unresolved_variable_operand(ctx, expr.base, base)
	}
	member_namespace := checker_selector_member_namespace(expr.op, namespace)
	name, member_node, name_ok := checker_expr_simple_name(expr.field)
	if !name_ok {
		checker_check_expr(ctx, expr.field, member_namespace, lhs)
		return checker_record_operand(ctx, node, .Field, project_type_unknown(ctx.project), lhs = lhs)
	}
	member := checker_lookup_selector_member(ctx, base, expr.op, name, member_namespace, member_node, lhs)
	if member.entity != nil {
		return checker_record_operand(ctx, node, member.mode, member.type, member.entity, lhs = lhs)
	}
	return checker_record_operand(ctx, node, .Field, member.type, lhs = lhs)
}

checker_check_interface_selector_expr :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	expr: ^ast.Interface_Qualified_Selector_Expr,
	namespace: Namespace,
	lhs: bool,
) -> Operand {
	receiver := checker_check_expr(ctx, expr.receiver, .Value)
	interface_operand := checker_check_expr(ctx, expr.interface, .Type)
	member_namespace := checker_selector_member_namespace(.Tilde, namespace)
	name, member_node, name_ok := checker_expr_simple_name(expr.member)
	if !name_ok || interface_operand.entity == nil {
		if !name_ok {
			checker_check_expr(ctx, expr.member, member_namespace, lhs)
		}
		return checker_record_operand(ctx, node, .Field, project_type_unknown(ctx.project), lhs = lhs)
	}
	if receiver_owner := checker_type_object_entity(receiver.type); receiver_owner != nil {
		if !checker_type_exposes_interface(ctx, receiver_owner, interface_operand.entity.name) {
			checker_add_diagnostic(
				ctx,
				.Inaccessible_Member,
				expr.interface.range,
				"receiver does not expose interface",
				interface_operand.entity,
				interface_operand.entity.decl_info,
			)
			checker_record_operand(ctx, member_node, .Value, project_type_unknown(ctx.project), lhs = lhs)
			return checker_record_operand(ctx, node, .Field, project_type_unknown(ctx.project), lhs = lhs)
		}
	}
	if member, ok := checker_lookup_object_member_visible(ctx, interface_operand.entity, member_namespace, project_intern_lower_ascii(ctx.project, name), member_node.range if member_node != nil else Range{}); ok {
		member_operand := checker_record_entity_operand(ctx, member_node, member, lhs)
		return checker_record_operand(ctx, node, member_operand.mode, member_operand.type, member, lhs = lhs)
	}
	checker_record_operand(ctx, member_node, .Value, project_type_unknown(ctx.project), lhs = lhs)
	return checker_record_operand(ctx, node, .Field, project_type_unknown(ctx.project), lhs = lhs)
}

checker_lookup_selector_member :: proc(
	ctx: ^Checker_Context,
	base: Operand,
	op: ast.Selector_Op,
	name: string,
	namespace: Namespace,
	node: ^ast.Node,
	lhs: bool,
	use_range: Range = {},
) -> Operand {
	interned := project_intern_lower_ascii(ctx.project, name)
	if interned == "" {
		return checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project), lhs = lhs)
	}
	if op == .Arrow && name == "*" {
		target := checker_type_ref_target(ctx, base.type)
		return checker_record_operand(ctx, node, .Value, target, lhs = lhs)
	}
	if op == .Dash {
		if structure := checker_type_structure(base.type); structure != nil {
			if field, ok := checker_lookup_structure_field(structure, interned); ok {
				return checker_record_entity_operand(ctx, node, field, lhs, use_range = use_range)
			}
			checker_add_diagnostic(
				ctx,
				.Unknown_Field,
				checker_use_range(node, use_range),
				checker_table_component_message(ctx, "unknown internal table field ", interned),
			)
		}
		return checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project), lhs = lhs)
	}
	if op == .Arrow {
		target := checker_type_ref_target(ctx, base.type)
		if structure := checker_type_structure(target); structure != nil {
			if field, ok := checker_lookup_structure_field(structure, interned); ok {
				return checker_record_entity_operand(ctx, node, field, lhs, use_range = use_range)
			}
		}
		if owner := checker_type_object_entity(target); owner != nil {
			if member, ok := checker_lookup_object_member_visible(ctx, owner, namespace, interned, checker_use_range(node, use_range)); ok {
				return checker_record_entity_operand(ctx, node, member, lhs, use_range = use_range)
			}
			_ = checker_diagnose_unaliased_interface_member_access(
				ctx,
				owner,
				namespace,
				interned,
				checker_use_range(node, use_range),
			)
		}
		return checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project), lhs = lhs)
	}
	if op == .Fat_Arrow || op == .Tilde {
		owner := base.entity
		if owner == nil {
			owner = checker_type_object_entity(base.type)
		}
		if owner != nil && (owner.kind == .Class || owner.kind == .Interface) {
			if member, ok := checker_lookup_object_member_visible(ctx, owner, namespace, interned, checker_use_range(node, use_range)); ok {
				return checker_record_entity_operand(ctx, node, member, lhs, use_range = use_range)
			}
			if op == .Fat_Arrow {
				_ = checker_diagnose_unaliased_interface_member_access(
					ctx,
					owner,
					namespace,
					interned,
					checker_use_range(node, use_range),
				)
			}
		}
	}
	return checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project), lhs = lhs)
}

checker_check_table_expr :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	expr: ^ast.Table_Expr,
	lhs: bool,
) -> Operand {
	table := checker_check_expr(ctx, expr.table, .Value, lhs && len(expr.selectors) == 0)
	if len(expr.selectors) == 0 {
		mode := table.mode if lhs else ast.Addressing_Mode.Value
		return checker_record_operand(ctx, node, mode, table.type, table.entity, lhs = lhs)
	}
	row_type := checker_type_row(ctx, table.type)
	row_structure := checker_type_structure(row_type)
	for selector in expr.selectors {
		checker_check_table_selector_expr(ctx, selector, row_type, row_structure)
	}
	return checker_record_operand(ctx, node, .Table_Line, row_type, lhs = lhs)
}

checker_check_table_selector_expr :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	row_type: ^Type,
	row_structure: ^Structure,
) {
	if expr == nil {
		return
	}
	if binary, ok := expr.derived_expr.(^ast.Binary_Expr); ok {
		if checker_check_table_key_operand(ctx, binary.left, row_type, row_structure) {
			checker_check_expr(ctx, binary.right)
			checker_record_operand(ctx, &expr.expr_base, .Value, checker_builtin_type_from_name(ctx.checker, "abap_bool"))
			return
		}
	}
	checker_check_expr(ctx, expr)
}

checker_check_table_key_operand :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	row_type: ^Type,
	row_structure: ^Structure,
) -> bool {
	name, node, ok := checker_expr_simple_name(expr)
	if !ok {
		return false
	}
	interned := project_intern_lower_ascii(ctx.project, name)
	if row_structure != nil {
		if field, field_ok := checker_lookup_structure_field(row_structure, interned); field_ok {
			checker_record_entity_operand(ctx, node, field)
			return true
		}
		checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project))
		return true
	}
	if name == "table_line" {
		checker_record_operand(ctx, node, .Table_Line, row_type)
		return true
	}
	checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project))
	return true
}

checker_check_raw_operand_expr :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	expr: ^ast.Type_Ref_Expr,
	lhs: bool,
) -> Operand {
	operand := checker_check_raw_operand_facts(ctx, expr.raw_decls[:], expr.raw_refs[:], ctx.type_hint, lhs, node)
	return checker_record_operand(ctx, node, operand.mode, operand.type, operand.entity, lhs = lhs)
}

checker_check_raw_operand_facts :: proc(
	ctx: ^Checker_Context,
	raw_decls: []ast.Raw_Operand_Inline_Decl,
	raw_refs: []ast.Raw_Operand_Ref,
	type_hint: ^Type,
	lhs: bool,
	node: ^ast.Node = nil,
) -> Operand {
	fallback := Operand{mode = .Value, type = type_hint if type_hint != nil else project_type_unknown(ctx.project)}
	operand := fallback
	fact_count := 0
	for decl in raw_decls {
		kind := Entity_Kind.Variable if decl.kind == .Data else Entity_Kind.Field_Symbol
		entity := checker_collect_inferred_expr_decl(ctx, decl.name.text, kind, decl.name.range, node, type_hint)
		fact_count += 1
		if fact_count == 1 && entity != nil {
			typ := entity.type if entity.type != nil else fallback.type
			operand = Operand{mode = .Variable, type = typ, entity = entity}
		}
	}
	for ref in raw_refs {
		fact_count += 1
		if fact_count == 1 {
			operand = checker_check_raw_operand_ref(ctx, ref, lhs, node)
		} else {
			_ = checker_check_raw_operand_ref(ctx, ref, false, node)
		}
	}
	if fact_count == 1 {
		return operand
	}
	return fallback
}

checker_check_raw_operand_ref :: proc(
	ctx: ^Checker_Context,
	ref: ast.Raw_Operand_Ref,
	lhs: bool,
	node: ^ast.Node = nil,
) -> Operand {
	namespace := Namespace.Routine if ref.call_like else Namespace.Value
	if ref.type_base {
		namespace = .Type
	}
	base := checker_check_ident_expr(
		ctx,
		node,
		ref.name.text,
		namespace,
		lhs && len(ref.path) == 0 && !ref.dynamic_path,
		ref.name.range,
	)
	if namespace == .Value && len(ref.path) > 0 && checker_should_diagnose_unresolved_value_operand(ctx) {
		checker_check_unresolved_named_operand(ctx, ref.name.text, ref.name.range, base)
	}
	for segment in ref.path {
		member_namespace := checker_selector_member_namespace(segment.selector, namespace)
		member := checker_lookup_selector_member(
			ctx,
			base,
			segment.selector,
			segment.name.text,
			member_namespace,
			node,
			lhs,
			segment.name.range,
		)
		if member.entity == nil {
			member.mode = .Field
		}
		base = member
	}
	if ref.dynamic_path {
		return Operand{mode = .Field, type = project_type_unknown(ctx.project)}
	}
	return base
}

checker_check_constructor_expr :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	expr: ^ast.Constructor_Expr,
	lhs: bool,
) -> Operand {
	typ := checker_constructor_result_type(ctx, expr)
	#partial switch expr.kind {
	case .New:
		checker_check_new_constructor_expr(ctx, expr, typ)
	case .Value:
		typ = checker_check_value_constructor_expr(ctx, expr, typ)
	case .Cond:
		typ = checker_check_cond_constructor_expr(ctx, expr, typ)
	case .Filter:
		typ = checker_check_filter_constructor_expr(ctx, expr, typ)
	case .Reduce:
		checker_check_reduce_constructor_expr(ctx, expr)
	case:
		for arg in expr.args {
			checker_check_expr(ctx, arg)
		}
	}
	return checker_record_operand(ctx, node, .Value, typ, lhs = lhs)
}

checker_check_value_constructor_expr :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Constructor_Expr,
	typ: ^Type,
) -> ^Type {
	result_type := typ
	inferred_base_arg: ^ast.Expr
	inferred_base_operand: Operand
	inferred_base_checked := false
	if checker_type_is_unknown(result_type) {
		base_type: ^Type
		base_type, inferred_base_arg, inferred_base_operand, inferred_base_checked = checker_infer_value_constructor_base_type(ctx, expr)
		if !checker_type_is_unknown(base_type) {
			result_type = base_type
		}
	}

	if checker_value_constructor_has_for_in_clause(expr) &&
	   !checker_type_is_unknown(result_type) &&
	   !checker_type_is_table_like(ctx, result_type) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			expr.type_ref.range if expr.type_ref != nil else expr.range,
			"VALUE constructor with FOR IN requires an internal table result type",
		)
	}

	if checker_type_is_table_like(ctx, result_type) {
		row_type := checker_type_row(ctx, result_type)
		for arg in expr.args {
			if checker_record_checked_value_constructor_base_arg(ctx, arg, inferred_base_arg, inferred_base_operand, inferred_base_checked) {
				continue
			}
			checker_check_value_constructor_table_arg(ctx, arg, result_type, row_type)
		}
		return result_type
	}

	if structure := checker_type_structure(result_type); structure != nil {
		for arg in expr.args {
			if checker_record_checked_value_constructor_base_arg(ctx, arg, inferred_base_arg, inferred_base_operand, inferred_base_checked) {
				continue
			}
			checker_check_value_constructor_structure_arg(ctx, arg, result_type, structure)
		}
		return result_type
	}

	for arg in expr.args {
		if checker_record_checked_value_constructor_base_arg(ctx, arg, inferred_base_arg, inferred_base_operand, inferred_base_checked) {
			continue
		}
		checker_check_expr_with_unresolved_value_diagnostics(ctx, arg)
	}
	return result_type
}

checker_infer_value_constructor_base_type :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Constructor_Expr,
) -> (
	typ: ^Type,
	arg: ^ast.Expr,
	operand: Operand,
	checked: bool,
) {
	if expr == nil || expr.kind != .Value || !checker_expr_is_inferred_type_ref(expr.type_ref) {
		return project_type_unknown(ctx.project), nil, {}, false
	}
	for candidate in expr.args {
		base, ok := candidate.derived_expr.(^ast.Constructor_Base_Clause_Expr)
		if !ok {
			continue
		}
		value := checker_check_expr_with_unresolved_value_diagnostics(ctx, base.value)
		return value.type, candidate, value, true
	}
	return project_type_unknown(ctx.project), nil, {}, false
}

checker_record_checked_value_constructor_base_arg :: proc(
	ctx: ^Checker_Context,
	arg: ^ast.Expr,
	inferred_base_arg: ^ast.Expr,
	inferred_base_operand: Operand,
	inferred_base_checked: bool,
) -> bool {
	if !inferred_base_checked || arg == nil || arg != inferred_base_arg {
		return false
	}
	checker_record_operand(ctx, &arg.expr_base, .No_Value, inferred_base_operand.type)
	return true
}

checker_value_constructor_has_for_in_clause :: proc(expr: ^ast.Constructor_Expr) -> bool {
	if expr == nil {
		return false
	}
	return checker_constructor_body_has_for_in_clause(expr.args[:])
}

checker_constructor_body_has_for_in_clause :: proc(args: []^ast.Expr) -> bool {
	for arg in args {
		if checker_constructor_arg_has_for_in_clause(arg) {
			return true
		}
	}
	return false
}

checker_constructor_arg_has_for_in_clause :: proc(arg: ^ast.Expr) -> bool {
	if arg == nil {
		return false
	}
	#partial switch n in arg.derived_expr {
	case ^ast.Constructor_For_Clause_Expr:
		return n.kind == .For_In ||
		       n.kind == .For_Groups ||
		       checker_constructor_body_has_for_in_clause(n.body[:])
	case ^ast.Let_Expr:
		return checker_constructor_body_has_for_in_clause(n.body[:])
	case ^ast.Call_Arg_List_Expr:
		return checker_constructor_body_has_for_in_clause(n.args[:])
	}
	return false
}

checker_check_value_constructor_table_arg :: proc(
	ctx: ^Checker_Context,
	arg: ^ast.Expr,
	table_type: ^Type,
	row_type: ^Type,
) {
	if arg == nil {
		return
	}
	#partial switch n in arg.derived_expr {
	case ^ast.Call_Arg_List_Expr:
		checker_check_value_constructor_row_arg_list(ctx, &arg.expr_base, n, row_type)
	case ^ast.Let_Expr:
		checker_check_value_constructor_table_let_expr(ctx, &arg.expr_base, n, table_type, row_type)
	case ^ast.Constructor_Base_Clause_Expr:
		value := checker_check_value_constructor_typed_value(ctx, n.value, table_type)
		checker_check_assignment_compatibility(ctx, value.type, table_type, checker_expr_range(n.value))
		checker_record_operand(ctx, &arg.expr_base, .No_Value, value.type)
	case ^ast.Constructor_Lines_Of_Clause_Expr:
		source := checker_check_expr_with_unresolved_value_diagnostics(ctx, n.source)
		checker_check_assignment_compatibility(ctx, source.type, table_type, checker_expr_range(n.source))
		checker_check_expr(ctx, n.from)
		checker_check_expr(ctx, n.to)
		checker_record_operand(ctx, &arg.expr_base, .Value, table_type)
	case ^ast.Constructor_For_Clause_Expr:
		checker_check_constructor_for_clause_expr(ctx, &arg.expr_base, n, false, body_type_hint = row_type)
	case:
		checker_check_value_constructor_row_value(ctx, arg, row_type)
	}
}

checker_check_value_constructor_row_value :: proc(
	ctx: ^Checker_Context,
	arg: ^ast.Expr,
	row_type: ^Type,
) {
	if arg == nil {
		return
	}
	if row, row_ok := arg.derived_expr.(^ast.Call_Arg_List_Expr); row_ok {
		checker_check_value_constructor_row_arg_list(ctx, &arg.expr_base, row, row_type)
		return
	}
	if let, let_ok := arg.derived_expr.(^ast.Let_Expr); let_ok {
		if structure := checker_type_structure(row_type); structure != nil {
			checker_check_value_constructor_structure_let_expr(ctx, &arg.expr_base, let, row_type, structure)
			return
		}
	}
	value := checker_check_value_constructor_typed_value(ctx, arg, row_type)
	checker_check_assignment_compatibility(ctx, value.type, row_type, checker_expr_range(arg))
}

checker_check_value_constructor_row_arg_list :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	arg_list: ^ast.Call_Arg_List_Expr,
	row_type: ^Type,
) {
	if structure := checker_type_structure(row_type); structure != nil {
		for arg in arg_list.args {
			checker_check_value_constructor_structure_arg(ctx, arg, row_type, structure)
		}
	} else {
		for arg in arg_list.args {
			value := checker_check_value_constructor_typed_value(ctx, arg, row_type)
			checker_check_assignment_compatibility(ctx, value.type, row_type, checker_expr_range(arg))
		}
	}
	checker_record_operand(ctx, node, .Value, row_type)
}

checker_check_value_constructor_structure_arg :: proc(
	ctx: ^Checker_Context,
	arg: ^ast.Expr,
	struct_type: ^Type,
	structure: ^Structure,
) {
	if arg == nil {
		return
	}
	#partial switch n in arg.derived_expr {
	case ^ast.Constructor_Named_Assignment_Expr:
		checker_check_value_constructor_named_assignment(ctx, &arg.expr_base, n, structure)
	case ^ast.Let_Expr:
		checker_check_value_constructor_structure_let_expr(ctx, &arg.expr_base, n, struct_type, structure)
	case ^ast.Call_Arg_List_Expr:
		for child in n.args {
			checker_check_value_constructor_structure_arg(ctx, child, struct_type, structure)
		}
		checker_record_operand(ctx, &arg.expr_base, .Value, struct_type)
	case ^ast.Constructor_Base_Clause_Expr:
		value := checker_check_value_constructor_typed_value(ctx, n.value, struct_type)
		checker_check_assignment_compatibility(ctx, value.type, struct_type, checker_expr_range(n.value))
		checker_record_operand(ctx, &arg.expr_base, .No_Value, value.type)
	case:
		value := checker_check_value_constructor_typed_value(ctx, arg, struct_type)
		checker_check_assignment_compatibility(ctx, value.type, struct_type, checker_expr_range(arg))
	}
}

checker_check_value_constructor_table_let_expr :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	expr: ^ast.Let_Expr,
	table_type: ^Type,
	row_type: ^Type,
) -> Operand {
	checker_open_scope(ctx, .Constructor_For, expr.range)
	defer checker_close_scope(ctx)

	for binding in expr.bindings {
		checker_check_expr(ctx, binding)
	}
	for body in expr.body {
		checker_check_value_constructor_table_arg(ctx, body, table_type, row_type)
	}
	return checker_record_operand(ctx, node, .Value, table_type)
}

checker_check_value_constructor_structure_let_expr :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	expr: ^ast.Let_Expr,
	struct_type: ^Type,
	structure: ^Structure,
) -> Operand {
	checker_open_scope(ctx, .Constructor_For, expr.range)
	defer checker_close_scope(ctx)

	for binding in expr.bindings {
		checker_check_expr(ctx, binding)
	}
	for body in expr.body {
		checker_check_value_constructor_structure_arg(ctx, body, struct_type, structure)
	}
	return checker_record_operand(ctx, node, .Value, struct_type)
}

checker_check_value_constructor_named_assignment :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	expr: ^ast.Constructor_Named_Assignment_Expr,
	structure: ^Structure,
) {
	name, simple_name := checker_constructor_assignment_simple_name(expr.name.text)
	field_type := project_type_unknown(ctx.project)
	field: ^Entity
	if simple_name {
		interned := project_intern_lower_ascii(ctx.project, name)
		if interned != "" {
			if found, ok := checker_lookup_structure_field(structure, interned); ok {
				field = found
				field_type = found.type if found.type != nil else project_type_unknown(ctx.project)
				checker_add_entity_use_at_range(ctx, node, found, expr.name.range)
			} else {
				checker_add_diagnostic(
					ctx,
					.Unknown_Field,
					expr.name.range,
					checker_table_component_message(ctx, "unknown structure field ", interned),
				)
			}
		}
	}
	value := checker_check_value_constructor_typed_value(ctx, expr.value, field_type)
	if field != nil {
		checker_check_assignment_compatibility(ctx, value.type, field_type, checker_expr_range(expr.value))
	}
	checker_record_operand(ctx, node, .No_Value, value.type)
}

checker_constructor_assignment_simple_name :: proc(name: string) -> (string, bool) {
	if name == "" {
		return "", false
	}
	for i in 0 ..< len(name) {
		switch name[i] {
		case '-', '>', '~', '[', ']':
			return "", false
		}
	}
	return name, true
}

checker_check_value_constructor_typed_value :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	type_hint: ^Type,
) -> Operand {
	local := ctx^
	local.type_hint = type_hint
	local.type_hint_expr = expr
	local.diagnose_unresolved_value_refs = true
	return checker_check_expr(&local, expr)
}

Checker_Cond_Validation_State :: struct {
	when_seen:   bool,
	else_seen:   bool,
	result_type: ^Type,
	inferred:    ^Type,
}

checker_check_cond_constructor_expr :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Constructor_Expr,
	result_type: ^Type,
) -> ^Type {
	state := Checker_Cond_Validation_State{result_type = result_type}
	checker_check_cond_sequence(ctx, expr.args[:], &state)
	if !state.when_seen {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			expr.range,
			"COND requires at least one WHEN clause",
		)
	}
	if !checker_type_is_unknown(state.result_type) {
		return state.result_type
	}
	return state.inferred if state.inferred != nil else project_type_unknown(ctx.project)
}

checker_check_cond_sequence :: proc(
	ctx: ^Checker_Context,
	args: []^ast.Expr,
	state: ^Checker_Cond_Validation_State,
) {
	for arg in args {
		#partial switch n in arg.derived_expr {
		case ^ast.Let_Expr:
			if state.when_seen || state.else_seen {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					arg.range,
					"COND LET must precede WHEN and ELSE",
				)
			}
			checker_check_cond_let_expr(ctx, &arg.expr_base, n, state)
		case ^ast.Constructor_When_Clause_Expr:
			if state.else_seen {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					arg.range,
					"COND WHEN must precede ELSE",
				)
			}
			state.when_seen = true
			checker_check_expr_with_unresolved_value_diagnostics(ctx, n.condition)
			result := checker_check_cond_result_expr(ctx, n.result, state)
			checker_record_operand(ctx, &arg.expr_base, .Value, result.type)
		case ^ast.Constructor_Else_Clause_Expr:
			if state.else_seen {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					arg.range,
					"COND allows only one ELSE clause",
				)
			}
			if !state.when_seen {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					arg.range,
					"COND ELSE requires a preceding WHEN clause",
				)
			}
			state.else_seen = true
			result := checker_check_cond_result_expr(ctx, n.result, state)
			checker_record_operand(ctx, &arg.expr_base, .Value, result.type)
		case:
			checker_add_diagnostic(
				ctx,
				.Invalid_Syntax_Form,
				arg.range,
				"COND allows only LET, WHEN, and ELSE clauses",
			)
			checker_check_expr(ctx, arg)
		}
	}
}

checker_check_cond_let_expr :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	expr: ^ast.Let_Expr,
	state: ^Checker_Cond_Validation_State,
) -> Operand {
	checker_open_scope(ctx, .Constructor_For, expr.range)
	defer checker_close_scope(ctx)

	for binding in expr.bindings {
		checker_check_expr(ctx, binding)
	}
	checker_check_cond_sequence(ctx, expr.body[:], state)
	return checker_record_operand(
		ctx,
		node,
		.Value,
		state.result_type if !checker_type_is_unknown(state.result_type) else state.inferred,
	)
}

checker_check_cond_result_expr :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	state: ^Checker_Cond_Validation_State,
) -> Operand {
	local := ctx^
	if !checker_type_is_unknown(state.result_type) {
		local.type_hint = state.result_type
		local.type_hint_expr = expr
	}
	local.diagnose_unresolved_value_refs = true
	result := checker_check_expr(&local, expr)
	if checker_type_is_unknown(state.result_type) {
		if state.inferred == nil || checker_type_is_unknown(state.inferred) {
			state.inferred = result.type
		}
		return result
	}
	checker_check_cond_result_compatibility(ctx, result.type, state.result_type, checker_expr_range(expr))
	return result
}

checker_check_cond_result_compatibility :: proc(
	ctx: ^Checker_Context,
	actual: ^Type,
	expected: ^Type,
	range: Range,
) {
	if checker_type_is_unknown(actual) || checker_type_is_unknown(expected) {
		return
	}
	if ok, known := checker_type_assignment_compatible(ctx, actual, expected); known {
		if !ok {
			checker_add_diagnostic(
				ctx,
				.Incompatible_Assignment_Type,
				range,
				checker_type_mismatch_message(ctx, "COND branch result is not compatible", actual, expected),
			)
		}
		return
	}
	actual_table := checker_type_is_table_like(ctx, actual)
	expected_table := checker_type_is_table_like(ctx, expected)
	actual_struct := checker_type_structure(actual) != nil
	expected_struct := checker_type_structure(expected) != nil
	actual_ref := checker_type_is_ref(actual)
	expected_ref := checker_type_is_ref(expected)
	if actual_table != expected_table ||
	   actual_struct != expected_struct ||
	   actual_ref != expected_ref {
		checker_add_diagnostic(
			ctx,
			.Incompatible_Assignment_Type,
			range,
			checker_type_mismatch_message(ctx, "COND branch result is not compatible", actual, expected),
		)
	}
}

checker_check_new_constructor_expr :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Constructor_Expr,
	typ: ^Type,
) {
	constructor := checker_constructor_method_for_type(ctx, typ)
	if constructor == nil {
		for arg in expr.args {
			checker_check_expr(ctx, arg)
		}
		return
	}
	args := make([dynamic]Checker_Call_Argument, 0, len(expr.args), context.temp_allocator)
	for arg in expr.args {
		checker_collect_constructor_call_argument(ctx, &args, arg, .Exporting, false)
	}
	checker_check_routine_call_arguments(ctx, constructor, args[:], expr.range)
}

checker_constructor_method_for_type :: proc(ctx: ^Checker_Context, typ: ^Type) -> ^Entity {
	owner := checker_type_object_entity(typ)
	if owner == nil {
		target := checker_type_ref_target(ctx, typ)
		owner = checker_type_object_entity(target)
	}
	if owner == nil || owner.kind != .Class {
		return nil
	}
	name := project_intern_lower_ascii(ctx.project, "constructor")
	constructor, found := checker_lookup_object_member_cached(ctx, owner, .Routine, name)
	if !found || constructor == nil || constructor.kind != .Method {
		return nil
	}
	checker_check_entity_for_operand(ctx, constructor)
	return constructor
}

checker_collect_constructor_call_argument :: proc(
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
			checker_collect_constructor_call_argument(ctx, args, arg, section_expr.kind, true)
		}
		return
	}
	arg := Checker_Call_Argument {
		section     = section,
		has_section = has_section,
		value_range = expr.range,
	}
	if named, named_ok := expr.derived_expr.(^ast.Constructor_Named_Assignment_Expr); named_ok {
		arg.name_text = named.name.text
		arg.name = project_intern_lower_ascii(ctx.project, named.name.text)
		arg.name_range = named.name.range
		arg.value = named.value
		arg.value_range = checker_expr_range(named.value)
	} else if call_named, call_named_ok := expr.derived_expr.(^ast.Call_Named_Arg_Expr); call_named_ok {
		arg.name_text = call_named.name.text
		arg.name = project_intern_lower_ascii(ctx.project, call_named.name.text)
		arg.name_range = call_named.name.range
		arg.value = call_named.value
		arg.value_range = checker_expr_range(call_named.value)
	} else if positional, positional_ok := expr.derived_expr.(^ast.Call_Positional_Arg_Expr); positional_ok {
		arg.value = positional.value
		arg.value_range = checker_expr_range(positional.value)
	} else {
		arg.value = expr
	}
	append(args, arg)
}

checker_check_filter_constructor_expr :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Constructor_Expr,
	result_type: ^Type,
) -> ^Type {
	if !checker_type_is_unknown(result_type) && !checker_type_is_table_like(ctx, result_type) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			expr.type_ref.range if expr.type_ref != nil else expr.range,
			"FILTER result type is not an internal table",
		)
	}

	sources := make([dynamic]^ast.Expr, 0, 2, context.temp_allocator)
	where_arg: ^ast.Expr
	where_clause: ^ast.Constructor_Where_Clause_Expr
	except_in_arg: ^ast.Expr
	except_in_clause: ^ast.Constructor_Filter_Except_In_Clause_Expr
	using_key_arg: ^ast.Expr
	using_key_clause: ^ast.Constructor_Filter_Using_Key_Clause_Expr
	seen_where := false
	for arg in expr.args {
		if except_node, ok := arg.derived_expr.(^ast.Constructor_Filter_Except_In_Clause_Expr); ok {
			if seen_where {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					arg.range,
					"FILTER source clauses must precede WHERE",
				)
			}
			if except_in_clause != nil {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					arg.range,
					"FILTER allows only one EXCEPT IN clause",
				)
			}
			except_in_arg = arg
			except_in_clause = except_node
			append(&sources, except_node.source)
			if except_node.where_clause != nil {
				if where_clause != nil {
					checker_add_diagnostic(
						ctx,
						.Invalid_Syntax_Form,
						except_node.where_clause.range,
						"FILTER allows only one WHERE clause",
					)
					checker_check_expr(ctx, except_node.where_clause)
					continue
				}
				if where_node, where_ok := except_node.where_clause.derived_expr.(^ast.Constructor_Where_Clause_Expr); where_ok {
					where_arg = except_node.where_clause
					where_clause = where_node
					seen_where = true
				} else {
					checker_check_expr(ctx, except_node.where_clause)
				}
			}
			continue
		}
		if using_key_node, ok := arg.derived_expr.(^ast.Constructor_Filter_Using_Key_Clause_Expr); ok {
			if seen_where {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					arg.range,
					"FILTER source clauses must precede WHERE",
				)
			}
			if using_key_clause != nil {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					arg.range,
					"FILTER allows only one USING KEY clause",
				)
			}
			using_key_arg = arg
			using_key_clause = using_key_node
			continue
		}
		if where_node, ok := arg.derived_expr.(^ast.Constructor_Where_Clause_Expr); ok {
			if where_clause != nil {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					arg.range,
					"FILTER allows only one WHERE clause",
				)
				checker_check_expr(ctx, arg)
				continue
			}
			where_arg = arg
			where_clause = where_node
			seen_where = true
			continue
		}
		if seen_where {
			checker_add_diagnostic(
				ctx,
				.Invalid_Syntax_Form,
				arg.range,
				"FILTER source clauses must precede WHERE",
			)
		}
		append(&sources, arg)
	}

	if len(sources) == 0 {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			expr.range,
			"FILTER requires a source table",
		)
	}
	if where_clause == nil {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			expr.range,
			"FILTER requires a WHERE clause",
		)
	}
	row_type := project_type_unknown(ctx.project)
	row_structure: ^Structure
	source_table_type := project_type_unknown(ctx.project)
	except_in_row_type := project_type_unknown(ctx.project)
	except_in_row_structure: ^Structure
	lookup_source_index := 0
	if len(sources) > 1 {
		lookup_source_index = 1
	}
	for source, i in sources {
		operand := checker_check_expr(ctx, source)
		if i > 1 {
			checker_add_diagnostic(
				ctx,
				.Invalid_Syntax_Form,
				source.range,
				"FILTER allows only a source table and optional IN table before WHERE",
			)
		}
		if checker_check_unresolved_variable_operand(ctx, source, operand) || checker_type_is_unknown(operand.type) {
			continue
		}
		if !checker_type_is_table_like(ctx, operand.type) {
			checker_add_diagnostic(
				ctx,
				.Invalid_Syntax_Form,
				source.range,
				"FILTER source is not an internal table",
			)
			continue
		}
		if i == lookup_source_index {
			checker_check_filter_lookup_key(ctx, source, operand, using_key_clause)
		}
		if i == 0 {
			source_table_type = operand.type
			row_type = checker_type_row(ctx, operand.type)
			row_structure = checker_type_structure(row_type)
		} else if i == 1 {
			except_in_row_type = checker_type_row(ctx, operand.type)
			except_in_row_structure = checker_type_structure(except_in_row_type)
		}
	}

	if where_clause != nil {
		if len(sources) > 1 {
			checker_check_filter_except_in_where_expr(
				ctx,
				where_clause.condition,
				row_type,
				row_structure,
				except_in_row_type,
				except_in_row_structure,
			)
		} else {
			checker_check_internal_table_where_expr(ctx, where_clause.condition, row_type, row_structure)
		}
		checker_record_operand(ctx, &where_arg.expr_base, .No_Value, project_type_unknown(ctx.project))
	}
	if except_in_arg != nil {
		checker_record_operand(ctx, &except_in_arg.expr_base, .No_Value, project_type_unknown(ctx.project))
	}
	if using_key_arg != nil {
		checker_check_table_key_selector(ctx, using_key_clause.using_key)
		checker_record_operand(ctx, &using_key_arg.expr_base, .No_Value, project_type_unknown(ctx.project))
	}

	if checker_filter_constructor_has_explicit_type_ref(expr) {
		checker_warn_filter_constructor_result_shape(ctx, expr, result_type, source_table_type)
	}
	if checker_filter_constructor_has_inferred_type_ref(expr) &&
	   checker_type_is_table_like(ctx, source_table_type) {
		return source_table_type
	}
	return result_type
}

checker_check_filter_lookup_key :: proc(
	ctx: ^Checker_Context,
	source: ^ast.Expr,
	operand: Operand,
	using_key_clause: ^ast.Constructor_Filter_Using_Key_Clause_Expr,
) {
	if checker_filter_table_key_is_suitable(ctx, operand.type, operand.entity, using_key_clause) {
		return
	}
	checker_add_diagnostic(
		ctx,
		.Invalid_Syntax_Form,
		checker_filter_key_diagnostic_range(source, using_key_clause),
		"FILTER requires a sorted or hashed table key",
	)
}

checker_filter_table_key_is_suitable :: proc(
	ctx: ^Checker_Context,
	typ: ^Type,
	entity: ^Entity,
	using_key_clause: ^ast.Constructor_Filter_Using_Key_Clause_Expr,
) -> bool {
	if using_key_clause != nil {
		if using_key_clause.using_key.dynamic_name != nil {
			return true
		}
		key_name := project_intern_lower_ascii(ctx.project, using_key_clause.using_key.name.text)
		if key_name == "" {
			return true
		}
		if checker_filter_key_name_is_primary(key_name) {
			return checker_filter_primary_key_is_suitable(typ)
		}
		return checker_filter_secondary_key_is_suitable(ctx, entity, typ, key_name)
	}
	return checker_filter_primary_key_is_suitable(typ)
}

checker_filter_primary_key_is_suitable :: proc(typ: ^Type) -> bool {
	form, ok := checker_type_table_form(typ)
	if !ok {
		return true
	}
	#partial switch form {
	case .Sorted_Table,
	     .Hashed_Table,
	     .Like_Sorted_Table,
	     .Like_Hashed_Table:
		return true
	}
	return false
}

checker_filter_secondary_key_is_suitable :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	typ: ^Type,
	key_name: string,
) -> bool {
	if checker_filter_entity_has_secondary_key(ctx, entity, key_name) {
		return true
	}
	type_entity := checker_filter_table_type_entity(typ)
	return type_entity != nil &&
	       type_entity != entity &&
	       checker_filter_entity_has_secondary_key(ctx, type_entity, key_name)
}

checker_filter_entity_has_secondary_key :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	key_name: string,
	depth := 0,
) -> bool {
	if depth > 16 || entity == nil {
		return false
	}
	if checker_filter_decl_has_secondary_key(ctx, entity.decl_info, key_name) {
		return true
	}
	next := checker_filter_next_type_entity(entity.type, entity)
	return next != nil && checker_filter_entity_has_secondary_key(ctx, next, key_name, depth + 1)
}

checker_filter_decl_has_secondary_key :: proc(
	ctx: ^Checker_Context,
	decl: ^Decl_Info,
	key_name: string,
) -> bool {
	if decl == nil || decl.type_clause == nil || decl.type_clause.type_ref == nil {
		return false
	}
	ref, ok := decl.type_clause.type_ref.derived_expr.(^ast.Type_Ref_Expr)
	if !ok {
		return false
	}
	if len(ref.keys) > 0 {
		for key in ref.keys {
			if checker_filter_key_clause_matches(ctx, key, key_name) {
				return true
			}
		}
		return false
	}
	return checker_filter_key_clause_matches(ctx, ref.key, key_name)
}

checker_filter_key_clause_matches :: proc(
	ctx: ^Checker_Context,
	key: ^ast.Type_Ref_Key_Clause,
	key_name: string,
) -> bool {
	return key != nil &&
	       (key.sorted || key.hashed) &&
	       project_intern_lower_ascii(ctx.project, key.name.text) == key_name
}

checker_filter_table_type_entity :: proc(typ: ^Type, depth := 0) -> ^Entity {
	if depth > 16 || typ == nil {
		return nil
	}
	#partial switch typ.kind {
	case .Named:
		if typ.entity != nil {
			return typ.entity
		}
		return checker_filter_table_type_entity(typ.base, depth + 1)
	}
	return nil
}

checker_filter_next_type_entity :: proc(typ: ^Type, current: ^Entity, depth := 0) -> ^Entity {
	if depth > 16 || typ == nil {
		return nil
	}
	#partial switch typ.kind {
	case .Named:
		if typ.entity != nil && typ.entity != current {
			return typ.entity
		}
		return checker_filter_next_type_entity(typ.base, current, depth + 1)
	}
	return nil
}

checker_filter_key_name_is_primary :: #force_inline proc "contextless" (name: string) -> bool {
	return name == "primary_key"
}

checker_filter_key_diagnostic_range :: proc(
	source: ^ast.Expr,
	using_key_clause: ^ast.Constructor_Filter_Using_Key_Clause_Expr,
) -> Range {
	if using_key_clause != nil && using_key_clause.using_key.name.text != "" {
		return using_key_clause.using_key.name.range
	}
	return checker_expr_range(source)
}

checker_filter_constructor_has_explicit_type_ref :: proc(expr: ^ast.Constructor_Expr) -> bool {
	return expr != nil &&
	       expr.type_ref != nil &&
	       !checker_expr_is_inferred_type_ref(expr.type_ref)
}

checker_filter_constructor_has_inferred_type_ref :: proc(expr: ^ast.Constructor_Expr) -> bool {
	return expr == nil ||
	       expr.type_ref == nil ||
	       checker_expr_is_inferred_type_ref(expr.type_ref)
}

checker_warn_filter_constructor_result_shape :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Constructor_Expr,
	result_type: ^Type,
	source_table_type: ^Type,
) {
	if expr == nil ||
	   expr.type_ref == nil ||
	   checker_type_is_unknown(result_type) ||
	   checker_type_is_unknown(source_table_type) ||
	   !checker_type_is_table_like(ctx, result_type) ||
	   !checker_type_is_table_like(ctx, source_table_type) {
		return
	}
	result_row_type := checker_type_row(ctx, result_type)
	source_row_type := checker_type_row(ctx, source_table_type)
	if same, known := checker_filter_row_types_structurally_same(ctx, result_row_type, source_row_type); !known || same {
		return
	}
	checker_add_diagnostic(
		ctx,
		.Invalid_Syntax_Form,
		expr.type_ref.range,
		"FILTER explicit result type is structurally different from the source table",
		severity = .Warning,
	)
}

checker_filter_row_types_structurally_same :: proc(
	ctx: ^Checker_Context,
	result_row_type: ^Type,
	source_row_type: ^Type,
) -> (bool, bool) {
	return checker_filter_types_structurally_same(ctx, result_row_type, source_row_type)
}

checker_filter_types_structurally_same :: proc(
	ctx: ^Checker_Context,
	left: ^Type,
	right: ^Type,
	depth := 0,
) -> (bool, bool) {
	if left == right {
		return true, true
	}
	if depth > 32 {
		return true, false
	}
	left_type := checker_filter_structural_type_base(left)
	right_type := checker_filter_structural_type_base(right)
	if checker_type_is_unknown(left_type) ||
	   checker_type_is_unknown(right_type) ||
	   left_type == nil ||
	   right_type == nil {
		return true, false
	}
	if left_type.kind != right_type.kind {
		return false, true
	}
	#partial switch left_type.kind {
	case .Builtin:
		left_name, left_ok := checker_type_builtin_name(ctx, left_type)
		right_name, right_ok := checker_type_builtin_name(ctx, right_type)
		if !left_ok || !right_ok {
			return true, false
		}
		return left_name == right_name, true
	case .Structure:
		return checker_filter_structures_structurally_same(ctx, left_type.structure, right_type.structure, depth + 1)
	case .Table:
		return checker_filter_types_structurally_same(ctx, left_type.base, right_type.base, depth + 1)
	case .Ref,
	     .Class,
	     .Interface,
	     .Routine:
		return checker_type_same(left_type, right_type), true
	}
	return checker_type_same(left_type, right_type), true
}

checker_filter_structures_structurally_same :: proc(
	ctx: ^Checker_Context,
	left: ^Structure,
	right: ^Structure,
	depth: int,
) -> (bool, bool) {
	if left == right {
		return true, true
	}
	if left == nil || right == nil {
		return true, false
	}
	if len(left.fields) != len(right.fields) {
		return false, true
	}
	known := true
	for left_field, i in left.fields {
		right_field := right.fields[i]
		if left_field == nil || right_field == nil {
			known = false
			continue
		}
		if left_field.name != right_field.name {
			return false, true
		}
		field_same, field_known := checker_filter_types_structurally_same(ctx, left_field.type, right_field.type, depth + 1)
		if !field_known {
			known = false
			continue
		}
		if !field_same {
			return false, true
		}
	}
	return true, known
}

checker_filter_structural_type_base :: proc(typ: ^Type, depth := 0) -> ^Type {
	if depth > 16 || typ == nil {
		return typ
	}
	if typ.kind == .Named && typ.base != nil {
		return checker_filter_structural_type_base(typ.base, depth + 1)
	}
	return typ
}

checker_check_reduce_constructor_expr :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Constructor_Expr,
) {
	checker_open_scope(ctx, .Constructor_For, expr.range)
	defer checker_close_scope(ctx)

	state := Checker_Reduce_Validation_State {
		init_names = make([dynamic]string, 0, 4, context.temp_allocator),
	}
	checker_check_reduce_sequence(ctx, expr.args[:], &state)
	if !state.init_seen {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			expr.range,
			"REDUCE requires an INIT clause",
		)
	}
	if !state.for_seen {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			expr.range,
			"REDUCE requires a FOR clause",
		)
	}
	if !state.next_seen {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			expr.range,
			"REDUCE requires a NEXT clause",
		)
	}
}

checker_check_reduce_sequence :: proc(
	ctx: ^Checker_Context,
	args: []^ast.Expr,
	state: ^Checker_Reduce_Validation_State,
) {
	for arg in args {
		#partial switch n in arg.derived_expr {
		case ^ast.Let_Expr:
			if state.init_seen || state.for_seen || state.next_seen {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					arg.range,
					"REDUCE LET must precede INIT, FOR, and NEXT",
				)
			}
			checker_check_reduce_let_expr(ctx, &arg.expr_base, n, state)
		case ^ast.Constructor_Init_Clause_Expr:
			if state.init_seen {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					arg.range,
					"REDUCE allows only one INIT clause",
				)
			}
			if state.for_seen || state.next_seen {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					arg.range,
					"REDUCE INIT must precede FOR and NEXT",
				)
			}
			checker_check_reduce_init_clause_expr(ctx, &arg.expr_base, n, state)
			state.init_seen = true
		case ^ast.Constructor_For_Clause_Expr:
			if !state.init_seen {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					arg.range,
					"REDUCE FOR requires a preceding INIT clause",
				)
			}
			if state.next_seen {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					arg.range,
					"REDUCE FOR must precede NEXT",
				)
			}
			state.for_seen = true
			checker_check_constructor_for_clause_expr(ctx, &arg.expr_base, n, false, state)
		case ^ast.Constructor_Next_Clause_Expr:
			if !state.init_seen {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					arg.range,
					"REDUCE NEXT requires a preceding INIT clause",
				)
			}
			if !state.for_seen {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					arg.range,
					"REDUCE NEXT requires a preceding FOR clause",
				)
			}
			if state.next_seen {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					arg.range,
					"REDUCE allows only one NEXT clause",
				)
			}
			state.next_seen = true
			checker_check_reduce_next_clause_expr(ctx, &arg.expr_base, n, state)
		case:
			checker_add_diagnostic(
				ctx,
				.Invalid_Syntax_Form,
				arg.range,
				"REDUCE allows only LET, INIT, FOR, and NEXT clauses",
			)
			checker_check_expr(ctx, arg)
		}
	}
}

checker_check_reduce_let_expr :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	expr: ^ast.Let_Expr,
	state: ^Checker_Reduce_Validation_State,
) -> Operand {
	checker_open_scope(ctx, .Constructor_For, expr.range)
	defer checker_close_scope(ctx)

	for binding in expr.bindings {
		checker_check_expr(ctx, binding)
	}
	checker_check_reduce_sequence(ctx, expr.body[:], state)
	return checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project))
}

checker_check_reduce_init_clause_expr :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	expr: ^ast.Constructor_Init_Clause_Expr,
	state: ^Checker_Reduce_Validation_State,
) -> Operand {
	if len(expr.assignments) == 0 {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			expr.range,
			"REDUCE INIT requires at least one assignment",
		)
	}
	for assignment in expr.assignments {
		named, ok := assignment.derived_expr.(^ast.Constructor_Named_Assignment_Expr)
		if !ok {
			checker_add_diagnostic(
				ctx,
				.Invalid_Syntax_Form,
				assignment.range,
				"REDUCE INIT accepts only named assignments",
			)
			checker_check_expr(ctx, assignment)
			continue
		}
		value := checker_check_expr_with_unresolved_value_diagnostics(ctx, named.value)
		name := project_intern_lower_ascii(ctx.project, named.name.text)
		if name == "" {
			checker_record_operand(ctx, &assignment.expr_base, .No_Value, value.type)
			continue
		}
		if checker_reduce_state_has_init_name(state, name) {
			checker_add_diagnostic(
				ctx,
				.Invalid_Syntax_Form,
				named.name.range,
				"REDUCE INIT assignment name is duplicated",
			)
		} else {
			append(&state.init_names, name)
			checker_collect_inferred_expr_decl(
				ctx,
				named.name.text,
				.Variable,
				named.name.range,
				&assignment.expr_base,
				value.type,
			)
		}
		checker_record_operand(ctx, &assignment.expr_base, .No_Value, value.type)
	}
	return checker_record_operand(ctx, node, .No_Value, project_type_unknown(ctx.project))
}

checker_check_reduce_next_clause_expr :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	expr: ^ast.Constructor_Next_Clause_Expr,
	state: ^Checker_Reduce_Validation_State,
) -> Operand {
	if len(expr.assignments) == 0 {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			expr.range,
			"REDUCE NEXT requires at least one assignment",
		)
	}
	assigned := make([dynamic]string, 0, len(expr.assignments), context.temp_allocator)
	for assignment in expr.assignments {
		named, ok := assignment.derived_expr.(^ast.Constructor_Named_Assignment_Expr)
		if !ok {
			checker_add_diagnostic(
				ctx,
				.Invalid_Syntax_Form,
				assignment.range,
				"REDUCE NEXT accepts only named assignments",
			)
			checker_check_expr(ctx, assignment)
			continue
		}
		name := project_intern_lower_ascii(ctx.project, named.name.text)
		if name != "" && checker_reduce_name_list_contains(assigned[:], name) {
			checker_add_diagnostic(
				ctx,
				.Invalid_Syntax_Form,
				named.name.range,
				"REDUCE NEXT assignment name is duplicated",
			)
		} else if name != "" {
			append(&assigned, name)
		}
		target_type := project_type_unknown(ctx.project)
		if name == "" || !checker_reduce_state_has_init_name(state, name) {
			checker_add_diagnostic(
				ctx,
				.Invalid_Syntax_Form,
				named.name.range,
				"REDUCE NEXT assignment must target an INIT variable",
			)
		} else if _, entity, found := checker_lookup_declaration(ctx, .Value, name); found {
			checker_add_entity_use_precise(ctx, &assignment.expr_base, entity, named.name.range)
			target_type = entity.type if entity.type != nil else project_type_unknown(ctx.project)
		}
		value_ctx := ctx^
		value_ctx.type_hint = target_type
		value_ctx.type_hint_expr = named.value
		value_ctx.diagnose_unresolved_value_refs = true
		value := checker_check_expr(&value_ctx, named.value)
		checker_check_assignment_compatibility(ctx, value.type, target_type, checker_expr_range(named.value))
		checker_record_operand(ctx, &assignment.expr_base, .No_Value, value.type)
	}
	return checker_record_operand(ctx, node, .No_Value, project_type_unknown(ctx.project))
}

checker_reduce_state_has_init_name :: proc(
	state: ^Checker_Reduce_Validation_State,
	name: string,
) -> bool {
	return checker_reduce_name_list_contains(state.init_names[:], name)
}

checker_reduce_name_list_contains :: proc(names: []string, name: string) -> bool {
	for existing in names {
		if existing == name {
			return true
		}
	}
	return false
}

checker_check_let_expr :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	expr: ^ast.Let_Expr,
	lhs: bool,
) -> Operand {
	checker_open_scope(ctx, .Constructor_For, expr.range)
	defer checker_close_scope(ctx)

	for binding in expr.bindings {
		checker_check_expr(ctx, binding)
	}
	result_type := project_type_unknown(ctx.project)
	for body in expr.body {
		operand := checker_check_expr(ctx, body)
		result_type = operand.type
	}
	return checker_record_operand(ctx, node, .Value, result_type, lhs = lhs)
}

checker_check_constructor_for_clause_expr :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	expr: ^ast.Constructor_For_Clause_Expr,
	lhs: bool,
	reduce_state: ^Checker_Reduce_Validation_State = nil,
	body_type_hint: ^Type = nil,
) -> Operand {
	checker_open_scope(ctx, .Constructor_For, expr.range)
	defer checker_close_scope(ctx)

	result_type := project_type_unknown(ctx.project)
	switch expr.kind {
	case .For_In:
		result_type = checker_check_constructor_for_in_clause(ctx, node, expr)
	case .For_Groups:
		result_type = checker_check_constructor_for_groups_clause(ctx, node, expr)
	case .For_Then_Until,
	     .For_Then_While:
		result_type = checker_check_constructor_for_then_clause(ctx, node, expr)
	}
	if reduce_state != nil {
		checker_check_reduce_sequence(ctx, expr.body[:], reduce_state)
	} else {
		if len(expr.body) == 0 {
			checker_add_diagnostic(
				ctx,
				.Invalid_Syntax_Form,
				expr.range,
				"FOR clause requires a body expression",
			)
		}
		for body in expr.body {
			if body_type_hint != nil {
				checker_check_value_constructor_row_value(ctx, body, body_type_hint)
				result_type = body_type_hint
			} else {
				operand := checker_check_expr(ctx, body)
				result_type = operand.type
			}
		}
	}
	return checker_record_operand(ctx, node, .No_Value, result_type, lhs = lhs)
}

checker_check_constructor_for_in_clause :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	expr: ^ast.Constructor_For_Clause_Expr,
) -> ^Type {
	if expr.init != nil || expr.then_expr != nil || expr.condition != nil {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			expr.range,
			"FOR IN cannot have THEN, UNTIL, or WHILE operands",
		)
	}
	row_type := project_type_unknown(ctx.project)
	source_table_type := project_type_unknown(ctx.project)
	row_structure: ^Structure
	if expr.source == nil && expr.group_source.text == "" {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			expr.range,
			"FOR IN requires a source table",
		)
	}
	if expr.source != nil {
		source := checker_check_expr(ctx, expr.source)
		if !checker_check_unresolved_variable_operand(ctx, expr.source, source) && !checker_type_is_unknown(source.type) {
			if checker_type_is_table_like(ctx, source.type) {
				source_table_type = source.type
				row_type = checker_type_row(ctx, source.type)
				row_structure = checker_type_structure(row_type)
			} else {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					expr.source.range,
					"FOR IN source is not an internal table",
				)
			}
		}
	}
	if expr.group_source.text != "" {
		group := checker_check_ident_expr(ctx, node, expr.group_source.text, .Value, false, expr.group_source.range)
		if checker_type_is_table_like(ctx, group.type) {
			source_table_type = group.type
		}
		row_type = checker_type_row(ctx, group.type)
		row_structure = checker_type_structure(row_type)
	}
	if expr.variable.text != "" {
		parent_scope := ctx.scope.parent if ctx.scope != nil && ctx.scope.parent != nil else ctx.scope
		checker_record_constructor_for_iterator_binding(
			ctx,
			parent_scope,
			expr.variable.text,
			expr.variable.range,
			source_table_type,
		)
		checker_collect_inferred_expr_decl(ctx, expr.variable.text, .Variable, expr.variable.range, node, row_type)
	}
	if expr.where_clause != nil {
		if where_node, ok := expr.where_clause.derived_expr.(^ast.Constructor_Where_Clause_Expr); ok {
			checker_check_internal_table_where_expr(ctx, where_node.condition, row_type, row_structure)
			checker_record_operand(ctx, &expr.where_clause.expr_base, .No_Value, project_type_unknown(ctx.project))
		} else {
			checker_check_expr(ctx, expr.where_clause)
		}
	}
	return row_type
}

checker_check_constructor_for_groups_clause :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	expr: ^ast.Constructor_For_Clause_Expr,
) -> ^Type {
	if expr.init != nil || expr.then_expr != nil || expr.condition != nil || expr.where_clause != nil || expr.group_source.text != "" {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			expr.range,
			"FOR GROUPS cannot have THEN, UNTIL, WHILE, WHERE, or IN GROUP operands",
		)
	}
	row_type := project_type_unknown(ctx.project)
	source_table_type := project_type_unknown(ctx.project)
	if expr.source == nil {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			expr.range,
			"FOR GROUPS requires a source table",
		)
	} else {
		source := checker_check_expr(ctx, expr.source)
		if !checker_check_unresolved_variable_operand(ctx, expr.source, source) && !checker_type_is_unknown(source.type) {
			if checker_type_is_table_like(ctx, source.type) {
				source_table_type = source.type
				row_type = checker_type_row(ctx, source.type)
			} else {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					expr.source.range,
					"FOR GROUPS source is not an internal table",
				)
			}
		}
	}
	if expr.member_variable.text != "" {
		parent_scope := ctx.scope.parent if ctx.scope != nil && ctx.scope.parent != nil else ctx.scope
		checker_record_constructor_for_iterator_binding(
			ctx,
			parent_scope,
			expr.member_variable.text,
			expr.member_variable.range,
			source_table_type,
		)
		checker_collect_inferred_expr_decl(
			ctx,
			expr.member_variable.text,
			.Variable,
			expr.member_variable.range,
			node,
			row_type,
		)
	}
	group_by := checker_check_expr(ctx, expr.group_by)
	group_type := group_by.type if expr.group_by != nil else project_type_unknown(ctx.project)
	if expr.group_by == nil {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			expr.range,
			"FOR GROUPS requires a GROUP BY expression",
		)
	}
	if expr.variable.text != "" {
		checker_collect_inferred_expr_decl(
			ctx,
			expr.variable.text,
			.Variable,
			expr.variable.range,
			node,
			group_type,
			inferred_type_entity = group_by.entity,
		)
	}
	return group_type
}

checker_check_constructor_for_then_clause :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	expr: ^ast.Constructor_For_Clause_Expr,
) -> ^Type {
	if expr.source != nil || expr.group_source.text != "" || expr.group_by != nil || expr.where_clause != nil || expr.member_variable.text != "" {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			expr.range,
			"FOR THEN cannot have IN, GROUP, or WHERE operands",
		)
	}
	if expr.init == nil {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			expr.range,
			"FOR THEN requires an initial value",
		)
	}
	init := checker_check_expr(ctx, expr.init)
	iter_type := init.type
	if expr.variable.text != "" {
		checker_collect_inferred_expr_decl(ctx, expr.variable.text, .Variable, expr.variable.range, node, iter_type)
	}
	if expr.then_expr != nil {
		then_ctx := ctx^
		then_ctx.type_hint = iter_type
		then_ctx.type_hint_expr = expr.then_expr
		then_ctx.diagnose_unresolved_value_refs = true
		next := checker_check_expr(&then_ctx, expr.then_expr)
		checker_check_assignment_compatibility(ctx, next.type, iter_type, checker_expr_range(expr.then_expr))
	}
	if expr.condition == nil {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			expr.range,
			"FOR THEN requires an UNTIL or WHILE condition",
		)
	} else {
		checker_check_expr_with_unresolved_value_diagnostics(ctx, expr.condition)
	}
	return iter_type
}

checker_check_expr_with_unresolved_value_diagnostics :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	namespace: Namespace = .Value,
	lhs := false,
) -> Operand {
	diagnostic_ctx := ctx^
	diagnostic_ctx.diagnose_unresolved_value_refs = true
	return checker_check_expr(&diagnostic_ctx, expr, namespace, lhs)
}

checker_collect_inferred_expr_decl :: proc(
	ctx: ^Checker_Context,
	name: string,
	kind: Entity_Kind,
	range: Range,
	node: ^ast.Node,
	typ: ^Type,
	inferred_type_entity: ^Entity = nil,
) -> ^Entity {
	entity := checker_collect_variable_decl(ctx, ctx.scope, name, kind, range, node, nil, nil)
	if entity == nil {
		return nil
	}
	checker_set_inferred_entity_type(ctx, entity, typ, inferred_type_entity)
	return entity
}

checker_apply_inline_decl_type :: proc(ctx: ^Checker_Context, name: string, typ: ^Type) {
	interned := project_intern_lower_ascii(ctx.project, name)
	if interned == "" {
		return
	}
	if entity, ok := scope_lookup_declaration(ctx.scope, .Value, interned); ok {
		checker_set_inferred_entity_type(ctx, entity, typ)
	}
}

checker_set_inferred_entity_type :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	typ: ^Type,
	inferred_type_entity: ^Entity = nil,
) {
	assert(entity != nil)
	resolved_type := typ
	if resolved_type == nil {
		resolved_type = project_type_unknown(ctx.project)
	}
	entity.type = resolved_type
	if payload, ok := entity.payload.(^Entity_Variable_Payload); ok && payload != nil {
		payload.inferred_type_entity = inferred_type_entity
	}
	entity.flags -= {.Untyped}
	entity.state = .Resolved
	if entity.decl_info != nil {
		entity.decl_info.state = .Resolved
	}
	for existing in ctx.info.checked_entities {
		if existing == entity {
			return
		}
	}
	append(&ctx.info.checked_entities, entity)
}

checker_check_entity_for_operand :: proc(ctx: ^Checker_Context, entity: ^Entity) {
	if entity == nil || entity_is_builtin(entity) || entity.state == .Resolved || entity.state == .Failed || entity.state == .Resolving {
		return
	}
	checker_check_entity_decl(ctx, entity)
}

checker_constructor_result_type :: proc(ctx: ^Checker_Context, expr: ^ast.Constructor_Expr) -> ^Type {
	if expr.type_ref == nil || checker_expr_is_inferred_type_ref(expr.type_ref) {
		return ctx.type_hint if ctx.type_hint != nil else project_type_unknown(ctx.project)
	}
	typ, _ := checker_type_from_expr(ctx, expr.type_ref, .Type)
	#partial switch expr.kind {
	case .New, .Ref:
		return project_type_ref(ctx.project, typ)
	}
	return typ
}

checker_expr_is_inferred_type_ref :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return true
	}
	if ref, ok := expr.derived_expr.(^ast.Type_Ref_Expr); ok {
		return ref.name.text == "#" || ref.base_name.text == "#"
	}
	if ident, ok := expr.derived_expr.(^ast.Ident_Expr); ok {
		return ident.name == "#"
	}
	return false
}

checker_call_result_type :: proc(ctx: ^Checker_Context, callee: Operand) -> ^Type {
	if callee.entity != nil {
		checker_check_entity_for_operand(ctx, callee.entity)
		if callee.entity.type != nil && callee.entity.type.base != nil {
			return callee.entity.type.base
		}
	}
	return project_type_unknown(ctx.project)
}

checker_binary_result_type :: proc(
	ctx: ^Checker_Context,
	op: ast.Binary_Op,
	left: Operand,
	right: Operand,
) -> ^Type {
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
		return checker_builtin_type_from_name(ctx.checker, "abap_bool")
	case .Concatenate:
		return checker_builtin_type_from_name(ctx.checker, "string")
	}
	if !checker_type_is_unknown(left.type) {
		return left.type
	}
	if !checker_type_is_unknown(right.type) {
		return right.type
	}
	return project_type_unknown(ctx.project)
}

checker_type_for_literal :: proc(ctx: ^Checker_Context, value: string) -> ^Type {
	if value == "" {
		return project_type_unknown(ctx.project)
	}
	if value[0] == '\'' || value[0] == '`' || value[0] == '|' {
		return checker_builtin_type_from_name(ctx.checker, "string")
	}
	if checker_literal_is_integer(value) {
		return checker_builtin_type_from_name(ctx.checker, "i")
	}
	return project_type_unknown(ctx.project)
}

checker_literal_is_integer :: proc(value: string) -> bool {
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

checker_expr_simple_name :: proc(expr: ^ast.Expr) -> (string, ^ast.Node, bool) {
	if expr == nil {
		return "", nil, false
	}
	name, _, ok := checker_expr_name(expr)
	return name, &expr.expr_base, ok
}

checker_selector_member_namespace :: proc(op: ast.Selector_Op, namespace: Namespace) -> Namespace {
	if namespace == .Routine {
		return .Routine
	}
	if namespace == .Type && (op == .Fat_Arrow || op == .Tilde) {
		return .Type
	}
	return .Value
}

checker_type_is_unknown :: proc(typ: ^Type) -> bool {
	return typ == nil || typ.kind == .Unknown
}

checker_addressing_mode_for_entity :: proc(entity: ^Entity) -> ast.Addressing_Mode {
	#partial switch entity.kind {
	case .Type_Def, .Class, .Interface:
		return .Type
	case .Form, .Module, .Event, .Builtin:
		return .Routine
	case .Method:
		return .Method
	case .Constant, .Enum_Member:
		return .Constant
	case .Field:
		return .Field
	case .Variable, .Field_Symbol, .Parameter, .Exception, .Control:
		return .Variable
	}
	return .Value
}
