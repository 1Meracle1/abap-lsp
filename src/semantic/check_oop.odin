package abap_frontend_semantic2

import "src:ast"

import "core:strings"

checker_check_object_semantics :: proc(ctx: ^Checker_Context, entity: ^Entity) {
	assert(entity != nil && (entity.kind == .Class || entity.kind == .Interface))
	payload, ok := entity.payload.(^Entity_Object_Payload)
	assert(ok && payload != nil)

	if entity.kind == .Class && payload.superclass_name != "" {
		if super, super_ok := checker_lookup_type_name_from_scope(
			ctx,
			entity.scope,
			payload.superclass_name,
			.Class,
		); super_ok && super.kind == .Class {
			checker_add_entity_use_at_range(ctx, entity.node, super, payload.superclass_range)
			checker_check_entity_for_operand(ctx, super)
		} else {
			checker_add_unresolved_oop_type_candidate(
				ctx,
				entity,
				payload.superclass_name,
				.Class,
				payload.superclass_range,
			)
		}
	}

	for interface_name in payload.implemented_interfaces {
		if interface_name == "" {
			continue
		}
		if iface, iface_ok := checker_lookup_type_name_from_scope(
			ctx,
			entity.scope,
			interface_name,
			.Interface,
		); iface_ok && iface.kind == .Interface {
			checker_add_entity_use(ctx, entity.node, iface)
			checker_check_entity_for_operand(ctx, iface)
		} else {
			checker_add_unresolved_oop_type_candidate(ctx, entity, interface_name, .Interface)
		}
	}
}

checker_check_oop_alias_decl :: proc(ctx: ^Checker_Context, entity: ^Entity, decl: ^Decl_Info) {
	assert(entity != nil && entity.kind == .Alias)
	payload, ok := entity.payload.(^Entity_Alias_Payload)
	assert(ok && payload != nil)
	if entity.owner == nil || (entity.owner.kind != .Class && entity.owner.kind != .Interface) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Context,
			entity.name_range,
			"ALIASES statement must be declared in a class or interface",
			entity,
			decl,
		)
		entity.type = project_type_unknown(ctx.project)
		return
	}
	if payload.target_interface_name == "" || payload.target_member_name == "" {
		entity.type = project_type_unknown(ctx.project)
		return
	}
	target_interface, interface_ok := checker_lookup_object_type_from_scope(
		ctx,
		entity.owner.scope,
		payload.target_interface_name,
		.Interface,
	)
	if !interface_ok {
		checker_add_unresolved_oop_type_candidate(
			ctx,
			entity.owner,
			payload.target_interface_name,
			.Interface,
			payload.target_interface_range,
		)
		entity.type = project_type_unknown(ctx.project)
		return
	}
	checker_add_entity_use_at_range(ctx, entity.node, target_interface, payload.target_interface_range)
	if !checker_object_exposes_interface(ctx, entity.owner, target_interface.name) {
		checker_add_diagnostic(
			ctx,
			.Inaccessible_Member,
			payload.target_interface_range,
			"alias target interface is not exposed by the owning object",
			target_interface,
			target_interface.decl_info,
		)
	}
	target_member, member_ok := checker_lookup_oop_alias_target_member(
		target_interface,
		payload.target_member_name,
	)
	if !member_ok {
		checker_add_diagnostic(
			ctx,
			.Unresolved_Reference,
			payload.target_member_range,
			"unknown interface member",
			target_interface,
			target_interface.decl_info,
		)
		entity.type = project_type_unknown(ctx.project)
		return
	}
	checker_add_entity_use_at_range(ctx, entity.node, target_member, payload.target_member_range)
	checker_check_entity_for_operand(ctx, target_member)
	entity.type = target_member.type if target_member.type != nil else project_type_unknown(ctx.project)
}

checker_lookup_oop_alias_target_member :: proc(target_interface: ^Entity, name: string) -> (
	^Entity,
	bool,
) {
	if target_interface == nil || target_interface.kind != .Interface || name == "" {
		return nil, false
	}
	namespaces := [?]Namespace{.Routine, .Value, .Type}
	for namespace in namespaces {
		if member, ok := checker_lookup_object_member(target_interface, namespace, name); ok {
			return member, true
		}
	}
	return nil, false
}

checker_object_exposes_interface :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	interface_name: string,
	depth := 0,
) -> bool {
	if ctx != nil {
		return checker_type_exposes_interface(ctx, entity, interface_name)
	}
	if depth > 32 || entity == nil || interface_name == "" {
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
		if implemented, implemented_ok := checker_lookup_object_type_from_scope(
			nil,
			entity.scope,
			implemented_name,
			.Interface,
		); implemented_ok && checker_object_exposes_interface(nil, implemented, interface_name, depth + 1) {
			return true
		}
	}
	if entity.kind == .Class && payload.superclass_name != "" {
		if super, super_ok := checker_lookup_object_type_from_scope(
			nil,
			entity.scope,
			payload.superclass_name,
			.Class,
		); super_ok && checker_object_exposes_interface(nil, super, interface_name, depth + 1) {
			return true
		}
	}
	return false
}

checker_diagnose_unaliased_interface_member_access :: proc(
	ctx: ^Checker_Context,
	owner: ^Entity,
	namespace: Namespace,
	name: string,
	range: Range,
) -> bool {
	if owner == nil || owner.kind != .Class {
		return false
	}
	if member, ok := checker_lookup_implemented_interface_member(
		owner,
		namespace,
		name,
		0,
		nil,
		ctx,
	); ok {
		checker_add_diagnostic(
			ctx,
			.Inaccessible_Member,
			range,
			"interface member requires interface-qualified access or an ALIASES declaration",
			member,
			member.decl_info,
		)
		return true
	}
	return false
}

checker_check_method_implementation_consistency :: proc(ctx: ^Checker_Context) {
	assert(ctx != nil && ctx.info != nil)
	for entity in ctx.info.definitions {
		if !checker_method_participates_in_class_implementation_check(entity) {
			continue
		}
		payload, ok := entity.payload.(^Entity_Routine_Payload)
		assert(ok && payload != nil)

		has_definition := checker_method_has_explicit_definition(entity, payload)
		if has_definition {
			if !payload.has_implementation {
				checker_add_method_consistency_diagnostic(
					ctx,
					.Missing_Method_Implementation,
					entity.source_file,
					entity.name_range,
					checker_method_missing_implementation_message(entity.name),
					entity,
					entity.decl_info,
				)
			}
			continue
		}

		if payload.has_implementation &&
		   !checker_method_implementation_satisfies_interface_contract(ctx, entity) {
			file := payload.implementation_unit if payload.implementation_unit != nil else entity.source_file
			range := payload.implementation_name_range
			if range.start >= range.end {
				range = entity.name_range
			}
			checker_add_method_consistency_diagnostic(
				ctx,
				.Missing_Method_Definition,
				file,
				range,
				checker_method_missing_definition_message(entity.name),
				entity,
				entity.decl_info,
			)
		}
	}
}

checker_method_participates_in_class_implementation_check :: proc(entity: ^Entity) -> bool {
	if entity == nil || entity.kind != .Method || entity.owner == nil || entity.owner.kind != .Class {
		return false
	}
	if entity.source_file == nil || .Abstract in entity.flags {
		return false
	}
	payload, ok := entity.payload.(^Entity_Routine_Payload)
	return ok && payload != nil
}

checker_method_has_explicit_definition :: proc(
	entity: ^Entity,
	payload: ^Entity_Routine_Payload,
) -> bool {
	assert(entity != nil && payload != nil)
	if payload.signature_scope != nil &&
	   checker_decl_info_is_oop_method_definition(payload.signature_scope.decl_info) {
		return true
	}
	return checker_decl_info_is_oop_method_definition(entity.decl_info)
}

checker_decl_info_is_oop_method_definition :: proc(info: ^Decl_Info) -> bool {
	if info == nil || info.decl_node == nil {
		return false
	}
	oop, ok := info.decl_node.derived.(^ast.Oop_Simple_Stmt)
	return ok && (oop.kind == .Methods || oop.kind == .Class_Methods)
}

checker_method_implementation_satisfies_interface_contract :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
) -> bool {
	assert(ctx != nil)
	if entity == nil || entity.owner == nil {
		return false
	}
	qualifier, member_name, qualified := checker_qualified_member_parts(ctx, entity.name)
	if !qualified {
		return false
	}
	owner_payload, owner_ok := entity.owner.payload.(^Entity_Object_Payload)
	assert(owner_ok && owner_payload != nil)
	implements_qualifier := false
	for interface_name in owner_payload.implemented_interfaces {
		if interface_name == qualifier {
			implements_qualifier = true
			break
		}
	}
	if !implements_qualifier {
		return false
	}
	iface, iface_ok := checker_lookup_object_type_from_scope(
		ctx,
		entity.owner.scope,
		qualifier,
		.Interface,
	)
	if !iface_ok || iface == nil {
		return false
	}
	member, member_ok := checker_lookup_object_member(iface, .Routine, member_name)
	return member_ok && member != nil && member.kind == .Method
}

checker_add_method_consistency_diagnostic :: proc(
	ctx: ^Checker_Context,
	kind: Checker_Diagnostic_Kind,
	file: ^Project_File,
	range: Range,
	message: string,
	entity: ^Entity,
	decl: ^Decl_Info,
) {
	if file == nil || range.start >= range.end {
		return
	}
	if checker_diagnostic_present(ctx.info.diagnostics[:], kind, range, file, message) {
		return
	}
	local := ctx^
	local.file = file
	checker_add_diagnostic(&local, kind, range, message, entity, decl)
}

checker_method_missing_implementation_message :: proc(name: string) -> string {
	builder := strings.builder_make(context.temp_allocator)
	strings.write_string(&builder, "missing implementation for method '")
	strings.write_string(&builder, name)
	strings.write_byte(&builder, '\'')
	return strings.to_string(builder)
}

checker_method_missing_definition_message :: proc(name: string) -> string {
	builder := strings.builder_make(context.temp_allocator)
	strings.write_string(&builder, "missing definition for method implementation '")
	strings.write_string(&builder, name)
	strings.write_byte(&builder, '\'')
	return strings.to_string(builder)
}

checker_check_object_body_oop_load_stmts :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	payload: ^Entity_Object_Payload,
) {
	if entity == nil || entity.node == nil || payload == nil || payload.definition_scope == nil {
		return
	}
	body_ctx := ctx^
	body_ctx.scope = payload.definition_scope
	#partial switch n in entity.node.derived {
	case ^ast.Class_Decl:
		checker_check_oop_load_stmt_list(&body_ctx, n.body)
	case ^ast.Interface_Decl:
		checker_check_oop_load_stmt_list(&body_ctx, n.body)
	}
}

checker_check_oop_load_stmt_list :: proc(ctx: ^Checker_Context, body: [dynamic]^ast.Stmt) {
	for stmt in body {
		if stmt == nil {
			continue
		}
		if load, ok := stmt.derived_stmt.(^ast.Oop_Load_Stmt); ok {
			checker_check_oop_load_stmt(ctx, load)
		}
	}
}

checker_check_oop_load_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Oop_Load_Stmt) {
	if stmt == nil || stmt.name.text == "" {
		return
	}
	interned := project_intern_lower_ascii(ctx.project, stmt.name.text)
	if interned == "" {
		return
	}
	candidate_kind := External_Candidate_Kind.Class
	entity_kind := Entity_Kind.Class
	if stmt.kind == .Interface {
		candidate_kind = .Interface
		entity_kind = .Interface
	}
	if entity, ok := checker_lookup_type_name_from_scope(ctx, ctx.scope, interned, candidate_kind);
	   ok && entity.kind == entity_kind {
		checker_add_entity_use_at_range(ctx, &stmt.node.stmt_base, entity, stmt.name.range)
		checker_check_entity_for_operand(ctx, entity)
		return
	}
	checker_add_unresolved_candidate(
		ctx,
		interned,
		.Type,
		candidate_kind,
		.Type_Reference,
		.Unresolved_Type,
		stmt.name.range,
		&stmt.node.stmt_base,
	)
}

checker_add_unresolved_oop_type_candidate :: proc(
	ctx: ^Checker_Context,
	owner: ^Entity,
	name: string,
	kind: External_Candidate_Kind,
	range: Range = {},
) {
	if owner == nil {
		return
	}
	ref_range := range if range.end > range.start else owner.name_range
	checker_add_unresolved_candidate(
		ctx,
		name,
		.Type,
		kind,
		.Type_Reference,
		.Unresolved_Type,
		ref_range,
		owner.node,
	)
}

checker_prepare_oop_routine_signature :: proc(ctx: ^Checker_Context, routine: ^Entity) {
	if routine == nil || routine.kind != .Method {
		return
	}
	checker_apply_oop_inherited_signature(ctx, routine)
	checker_apply_oop_event_handler_signature(ctx, routine)
	checker_ensure_oop_receiver_entities(ctx, routine)
}

checker_apply_oop_inherited_signature :: proc(ctx: ^Checker_Context, routine: ^Entity) {
	if routine.owner == nil || routine.owner.kind != .Class && routine.owner.kind != .Interface {
		return
	}
	payload, ok := routine.payload.(^Entity_Routine_Payload)
	assert(ok && payload != nil)
	if !payload.is_redefinition &&
	   !(.Redefinition in routine.flags) &&
	   !checker_entity_name_is_qualified(ctx, routine.name) {
		return
	}
	inherited := checker_lookup_redefined_method(ctx, routine)
	if inherited == nil || inherited == routine {
		return
	}
	checker_check_entity_for_operand(ctx, inherited)
	inherited_payload, inherited_ok := inherited.payload.(^Entity_Routine_Payload)
	assert(inherited_ok && inherited_payload != nil)
	if len(payload.parameters) == 0 {
		for param in inherited_payload.parameters {
			checker_copy_inherited_parameter(ctx, routine, param)
		}
	}
	if len(payload.exceptions) == 0 {
		for exception in inherited_payload.exceptions {
			append(&payload.exceptions, exception)
		}
	}
}

checker_lookup_redefined_method :: proc(ctx: ^Checker_Context, routine: ^Entity) -> ^Entity {
	assert(routine != nil && routine.kind == .Method)
	owner := routine.owner
	if owner == nil {
		return nil
	}
	if qualifier, member_name, qualified := checker_qualified_member_parts(ctx, routine.name);
	   qualified {
		if member, ok := checker_lookup_qualified_interface_member(
			ctx,
			owner,
			.Routine,
			qualifier,
			member_name,
		); ok {
			return member
		}
	}
	if member, ok := checker_lookup_inherited_object_member(ctx, owner, .Routine, routine.name);
	   ok {
		return member
	}
	return nil
}

checker_lookup_inherited_object_member :: proc(
	ctx: ^Checker_Context,
	owner: ^Entity,
	namespace: Namespace,
	name: string,
	depth := 0,
) -> (
	^Entity,
	bool,
) {
	if depth > 32 || owner == nil {
		return nil, false
	}
	payload, ok := owner.payload.(^Entity_Object_Payload)
	assert(ok && payload != nil)
	for interface_name in payload.implemented_interfaces {
		if interface_name == "" {
			continue
		}
		if iface, iface_ok := checker_lookup_type_name_from_scope(
			ctx,
			owner.scope,
			interface_name,
			.Interface,
		); iface_ok && iface.kind == .Interface {
			if member, member_ok := checker_lookup_object_member(iface, namespace, name);
			   member_ok {
				return member, true
			}
		}
	}
	if owner.kind == .Class && payload.superclass_name != "" {
		if super, super_ok := checker_lookup_type_name_from_scope(
			ctx,
			owner.scope,
			payload.superclass_name,
			.Class,
		); super_ok && super.kind == .Class {
			if member, member_ok := checker_lookup_object_member(super, namespace, name);
			   member_ok {
				return member, true
			}
			return checker_lookup_inherited_object_member(ctx, super, namespace, name, depth + 1)
		}
	}
	return nil, false
}

checker_lookup_qualified_interface_member :: proc(
	ctx: ^Checker_Context,
	owner: ^Entity,
	namespace: Namespace,
	interface_name: string,
	member_name: string,
) -> (
	^Entity,
	bool,
) {
	if owner == nil || interface_name == "" || member_name == "" {
		return nil, false
	}
	iface, iface_ok := checker_lookup_type_name_from_scope(
		ctx,
		owner.scope,
		interface_name,
		.Interface,
	)
	if !iface_ok || iface.kind != .Interface {
		checker_add_unresolved_oop_type_candidate(ctx, owner, interface_name, .Interface)
		return nil, false
	}
	if !checker_type_exposes_interface(ctx, owner, interface_name) {
		return nil, false
	}
	return checker_lookup_object_member(iface, namespace, member_name)
}

checker_copy_inherited_parameter :: proc(
	ctx: ^Checker_Context,
	routine: ^Entity,
	inherited: ^Entity,
) -> ^Entity {
	if inherited == nil || inherited.kind != .Parameter {
		return nil
	}
	payload, ok := routine.payload.(^Entity_Routine_Payload)
	assert(ok && payload != nil && payload.signature_scope != nil)
	checker_check_entity_for_operand(ctx, inherited)

	entity := project_new_entity(ctx.project, .Parameter)
	entity.node = routine.node
	entity.owner = routine
	entity.source_file = routine.source_file
	entity.type = inherited.type if inherited.type != nil else project_type_unknown(ctx.project)
	entity.state = .Resolved
	entity.flags += {.Parameter}
	if .Optional in inherited.flags {
		entity.flags += {.Optional}
	}
	if .Has_Default_Value in inherited.flags {
		entity.flags += {.Has_Default_Value}
	}
	if .Has_Declared_Type in inherited.flags || !checker_type_is_unknown(entity.type) {
		entity.flags += {.Has_Declared_Type}
	}
	if .Untyped in inherited.flags && !(.Has_Declared_Type in entity.flags) {
		entity.flags += {.Untyped}
	}

	decl := project_new_decl_info(
		ctx.project,
		entity,
		payload.signature_scope,
		inherited.name,
		.Parameter,
		inherited.name_range,
		routine.node,
	)
	decl.state = .Resolved
	param_payload, param_ok := entity.payload.(^Entity_Variable_Payload)
	inherited_payload, inherited_ok := inherited.payload.(^Entity_Variable_Payload)
	assert(param_ok && param_payload != nil && inherited_ok && inherited_payload != nil)
	param_payload.section = inherited_payload.section
	param_payload.passing = inherited_payload.passing
	param_payload.param_value = inherited_payload.param_value
	_ = checker_add_entity_and_decl_info(ctx, entity, decl)
	append(&payload.parameters, entity)
	append(&ctx.info.checked_entities, entity)
	return entity
}

checker_apply_oop_event_handler_signature :: proc(ctx: ^Checker_Context, routine: ^Entity) {
	payload, ok := routine.payload.(^Entity_Routine_Payload)
	assert(ok && payload != nil)
	if !payload.for_event || payload.event_name == "" {
		return
	}
	source_type, source_entity := checker_type_from_ref_data(ctx, payload.event_source_type)
	if source_entity == nil {
		source_entity = checker_type_object_entity(source_type)
	}
	if source_entity == nil {
		return
	}
	event, event_ok := checker_lookup_object_member(source_entity, .Routine, payload.event_name)
	if !event_ok || event.kind != .Event {
		return
	}
	checker_check_entity_for_operand(ctx, event)
	event_payload, event_payload_ok := event.payload.(^Entity_Routine_Payload)
	assert(event_payload_ok && event_payload != nil)
	for param in payload.parameters {
		event_param := checker_routine_parameter_named(event_payload, param.name)
		if event_param == nil {
			continue
		}
		checker_check_entity_for_operand(ctx, event_param)
		if .Optional in event_param.flags {
			param.flags += {.Optional}
		}
		if .Has_Default_Value in event_param.flags {
			param.flags += {.Has_Default_Value}
		}
		if !(.Has_Declared_Type in param.flags && !checker_type_is_unknown(param.type)) {
			param.type =
				event_param.type if event_param.type != nil else project_type_unknown(ctx.project)
			param.flags += {.Has_Declared_Type}
			param.flags -= {.Untyped}
			param.state = .Resolved
			if param.decl_info != nil {
				param.decl_info.state = .Resolved
			}
		}
		param_payload, param_ok := param.payload.(^Entity_Variable_Payload)
		event_param_payload, event_param_ok := event_param.payload.(^Entity_Variable_Payload)
		assert(param_ok && param_payload != nil && event_param_ok && event_param_payload != nil)
		param_payload.passing = event_param_payload.passing
	}
}

checker_routine_parameter_named :: proc(
	payload: ^Entity_Routine_Payload,
	name: string,
) -> ^Entity {
	assert(payload != nil)
	for param in payload.parameters {
		if param.name == name {
			return param
		}
	}
	return nil
}

checker_ensure_oop_receiver_entities :: proc(ctx: ^Checker_Context, routine: ^Entity) {
	payload, ok := routine.payload.(^Entity_Routine_Payload)
	assert(ok && payload != nil)
	if routine.owner == nil || payload.is_static {
		return
	}
	owner := routine.owner
	checker_ensure_oop_receiver_entity(ctx, routine, "me", owner)
	if owner.kind != .Class {
		return
	}
	owner_payload, owner_ok := owner.payload.(^Entity_Object_Payload)
	assert(owner_ok && owner_payload != nil)
	if owner_payload.superclass_name == "" {
		return
	}
	if super, super_ok := checker_lookup_type_name_from_scope(
		ctx,
		owner.scope,
		owner_payload.superclass_name,
		.Class,
	); super_ok && super.kind == .Class {
		checker_ensure_oop_receiver_entity(ctx, routine, "super", super)
	}
}

checker_ensure_oop_receiver_entity :: proc(
	ctx: ^Checker_Context,
	routine: ^Entity,
	name: string,
	target: ^Entity,
) -> ^Entity {
	payload, ok := routine.payload.(^Entity_Routine_Payload)
	assert(ok && payload != nil && payload.signature_scope != nil)
	interned := project_intern_lower_ascii(ctx.project, name)
	if entity, found := scope_lookup_declaration(payload.signature_scope, .Value, interned);
	   found {
		return entity
	}
	if target.type == nil {
		target.type = project_type_class_or_interface(
			ctx.project,
			target.name,
			target,
			target.kind,
		)
	}
	entity := project_new_entity(ctx.project, .Parameter)
	entity.node = routine.node
	entity.owner = routine
	entity.source_file = routine.source_file
	entity.type = project_type_ref(ctx.project, target.type)
	entity.state = .Resolved
	entity.flags += {.Parameter, .Has_Declared_Type}
	decl := project_new_decl_info(
		ctx.project,
		entity,
		payload.signature_scope,
		interned,
		.Parameter,
		routine.name_range,
		routine.node,
	)
	decl.state = .Resolved
	param_payload, param_ok := entity.payload.(^Entity_Variable_Payload)
	assert(param_ok && param_payload != nil)
	param_payload.passing = .Reference
	_ = checker_add_entity_and_decl_info(ctx, entity, decl)
	append(&ctx.info.checked_entities, entity)
	return entity
}

checker_check_oop_receiver_ident :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	name: string,
	namespace: Namespace,
	use_range: Range = {},
) -> (
	^Entity,
	bool,
	bool,
) {
	if namespace != .Value {
		return nil, false, false
	}
	interned := project_intern_lower_ascii(ctx.project, name)
	if interned != project_intern_lower_ascii(ctx.project, "me") &&
	   interned != project_intern_lower_ascii(ctx.project, "super") {
		return nil, false, false
	}
	if _, entity, ok := checker_lookup_declaration(ctx, .Value, interned); ok {
		checker_add_entity_use_precise(ctx, node, entity, use_range)
		return entity, true, true
	}
	checker_add_diagnostic(
		ctx,
		.Invalid_Context,
		checker_use_range(node, use_range),
		"invalid object receiver",
	)
	return nil, false, true
}

checker_lookup_object_member_from_scope :: proc(
	scope: ^Scope,
	owner: ^Entity,
	namespace: Namespace,
	name: string,
	excluded: ^Entity = nil,
) -> (
	^Entity,
	bool,
) {
	if member, ok := checker_lookup_object_member(owner, namespace, name, excluded); ok {
		if checker_member_visible_from_scope(scope, member) {
			return member, true
		}
	}
	return nil, false
}

checker_lookup_object_member_visible :: proc(
	ctx: ^Checker_Context,
	owner: ^Entity,
	namespace: Namespace,
	name: string,
	range: Range = {},
) -> (
	^Entity,
	bool,
) {
	if member, ok := checker_lookup_object_member_cached(ctx, owner, namespace, name); ok {
		if checker_member_visible_from_context(ctx, member) {
			return member, true
		}
		checker_add_diagnostic(
			ctx,
			.Inaccessible_Member,
			range,
			"member is not visible",
			member,
			member.decl_info,
		)
	}
	return nil, false
}

checker_member_visible_from_context :: proc(ctx: ^Checker_Context, member: ^Entity) -> bool {
	assert(ctx != nil && ctx.scope != nil)
	if member == nil || member.owner == nil {
		return true
	}
	visibility := checker_member_visibility(member)
	if visibility == .Public {
		return true
	}
	accessor := checker_enclosing_object_owner(ctx.scope)
	if accessor == nil {
		return false
	}
	if accessor == member.owner {
		return true
	}
	if visibility == .Private {
		return checker_object_has_friend(member.owner, accessor)
	}
	if visibility == .Protected {
		return checker_class_entity_is_or_inherits_from_context(ctx, accessor, member.owner.name)
	}
	return false
}

checker_member_visible_from_scope :: proc(scope: ^Scope, member: ^Entity) -> bool {
	if member == nil || member.owner == nil {
		return true
	}
	visibility := checker_member_visibility(member)
	if visibility == .Public {
		return true
	}
	accessor := checker_enclosing_object_owner(scope)
	if accessor == nil {
		return false
	}
	if accessor == member.owner {
		return true
	}
	if visibility == .Private {
		return checker_object_has_friend(member.owner, accessor)
	}
	if visibility == .Protected {
		return checker_class_entity_is_or_inherits_from(accessor, member.owner.name)
	}
	return false
}

checker_member_visibility :: proc(member: ^Entity) -> Visibility {
	assert(member != nil)
	#partial switch member.kind {
	case .Method, .Event:
		payload, ok := member.payload.(^Entity_Routine_Payload)
		assert(ok && payload != nil)
		return payload.visibility
	case .Alias:
		payload, ok := member.payload.(^Entity_Alias_Payload)
		assert(ok && payload != nil)
		return payload.visibility
	}
	if member.owner != nil {
		return member.visibility
	}
	return .Public
}

checker_object_has_friend :: proc(owner: ^Entity, accessor: ^Entity) -> bool {
	if owner == nil || accessor == nil {
		return false
	}
	payload, ok := owner.payload.(^Entity_Object_Payload)
	assert(ok && payload != nil)
	for friend in payload.friends {
		if friend == accessor.name {
			return true
		}
	}
	return false
}

checker_class_entity_is_or_inherits_from :: proc(
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
	assert(ok && payload != nil)
	if payload.superclass_name == "" {
		return false
	}
	if _, super, super_ok := lookup_lexical_declaration_from_scope(
		class_entity.scope,
		.Type,
		payload.superclass_name,
	); super_ok && super.kind == .Class {
		return checker_class_entity_is_or_inherits_from(super, target_name, depth + 1)
	}
	return false
}

checker_class_entity_is_or_inherits_from_context :: proc(
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
	assert(ok && payload != nil)
	if payload.superclass_name == "" {
		return false
	}
	if super, super_ok := checker_lookup_object_type_from_scope(
		ctx,
		class_entity.scope,
		payload.superclass_name,
		.Class,
	); super_ok {
		return checker_class_entity_is_or_inherits_from_context(ctx, super, target_name, depth + 1)
	}
	return false
}

checker_lookup_type_name_from_scope :: proc(
	ctx: ^Checker_Context,
	scope: ^Scope,
	name: string,
	preferred_external_kind: External_Candidate_Kind = .Global_Symbol,
) -> (
	^Entity,
	bool,
) {
	if scope == nil || name == "" {
		return nil, false
	}
	local := ctx^
	local.scope = scope
	_, entity, ok := checker_lookup_reference(&local, .Type, name, preferred_external_kind)
	return entity, ok
}

checker_entity_name_is_qualified :: proc(ctx: ^Checker_Context, name: string) -> bool {
	_, _, ok := checker_qualified_member_parts(ctx, name)
	return ok
}

checker_qualified_member_parts :: proc(
	ctx: ^Checker_Context,
	name: string,
) -> (
	string,
	string,
	bool,
) {
	if name == "" {
		return {}, {}, false
	}
	text := name
	for i := 0; i < len(text); i += 1 {
		if text[i] != '~' {
			continue
		}
		if i == 0 || i + 1 >= len(text) {
			return {}, {}, false
		}
		return project_intern_lower_ascii(ctx.project, text[:i]),
			project_intern_lower_ascii(ctx.project, text[i + 1:]),
			true
	}
	return {}, {}, false
}
