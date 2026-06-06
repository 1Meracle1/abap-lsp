package abap_frontend_semantic

import "src:ast"
import string_interner "src:string_interner"

checker_check_object_semantics :: proc(ctx: ^Checker_Context, entity: ^Entity) {
	assert(entity != nil && (entity.kind == .Class || entity.kind == .Interface))
	payload, ok := entity.payload.(^Entity_Object_Payload)
	assert(ok && payload != nil)

	if entity.kind == .Class && string_interner.is_valid(payload.superclass_name) {
		if _, super, super_ok := checker_lookup_lexical_declaration_from_scope(entity.scope, .Type, payload.superclass_name);
		   super_ok && super.kind == .Class {
			checker_add_entity_use(ctx, entity.node, super)
			checker_check_entity_for_operand(ctx, super)
		}
	}

	for interface_name in payload.implemented_interfaces {
		if !string_interner.is_valid(interface_name) {
			continue
		}
		if _, iface, iface_ok := checker_lookup_lexical_declaration_from_scope(entity.scope, .Type, interface_name);
		   iface_ok && iface.kind == .Interface {
			checker_add_entity_use(ctx, entity.node, iface)
			checker_check_entity_for_operand(ctx, iface)
		}
	}
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
	if !payload.is_redefinition && !(.Redefinition in routine.flags) && !checker_entity_name_is_qualified(ctx, routine.name) {
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
	if qualifier, member_name, qualified := checker_qualified_member_parts(ctx, routine.name); qualified {
		if member, ok := checker_lookup_qualified_interface_member(ctx, owner, .Routine, qualifier, member_name); ok {
			return member
		}
	}
	if member, ok := checker_lookup_inherited_object_member(ctx, owner, .Routine, routine.name); ok {
		return member
	}
	return nil
}

checker_lookup_inherited_object_member :: proc(
	ctx: ^Checker_Context,
	owner: ^Entity,
	namespace: Namespace,
	name: string_interner.String,
	depth := 0,
) -> (^Entity, bool) {
	if depth > 32 || owner == nil {
		return nil, false
	}
	payload, ok := owner.payload.(^Entity_Object_Payload)
	assert(ok && payload != nil)
	for interface_name in payload.implemented_interfaces {
		if !string_interner.is_valid(interface_name) {
			continue
		}
		if _, iface, iface_ok := checker_lookup_lexical_declaration_from_scope(owner.scope, .Type, interface_name);
		   iface_ok && iface.kind == .Interface {
			if member, member_ok := checker_lookup_object_member(iface, namespace, name); member_ok {
				return member, true
			}
		}
	}
	if owner.kind == .Class && string_interner.is_valid(payload.superclass_name) {
		if _, super, super_ok := checker_lookup_lexical_declaration_from_scope(owner.scope, .Type, payload.superclass_name);
		   super_ok && super.kind == .Class {
			if member, member_ok := checker_lookup_object_member(super, namespace, name); member_ok {
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
	interface_name: string_interner.String,
	member_name: string_interner.String,
) -> (^Entity, bool) {
	if owner == nil || !string_interner.is_valid(interface_name) || !string_interner.is_valid(member_name) {
		return nil, false
	}
	if !checker_type_exposes_interface(ctx, owner, interface_name) {
		return nil, false
	}
	_, iface, iface_ok := checker_lookup_lexical_declaration_from_scope(owner.scope, .Type, interface_name)
	if !iface_ok || iface.kind != .Interface {
		return nil, false
	}
	return checker_lookup_object_member(iface, namespace, member_name)
}

checker_copy_inherited_parameter :: proc(ctx: ^Checker_Context, routine: ^Entity, inherited: ^Entity) -> ^Entity {
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

	decl := project_new_decl_info(ctx.project, entity, payload.signature_scope, inherited.name, .Parameter, inherited.name_range, routine.node)
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
	if !payload.for_event || !string_interner.is_valid(payload.event_name) {
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
		if .Has_Declared_Type in param.flags && !checker_type_is_unknown(param.type) {
			continue
		}
		event_param := checker_routine_parameter_named(event_payload, param.name)
		if event_param == nil {
			continue
		}
		checker_check_entity_for_operand(ctx, event_param)
		param.type = event_param.type if event_param.type != nil else project_type_unknown(ctx.project)
		param.flags += {.Has_Declared_Type}
		param.flags -= {.Untyped}
		param.state = .Resolved
		if param.decl_info != nil {
			param.decl_info.state = .Resolved
		}
		param_payload, param_ok := param.payload.(^Entity_Variable_Payload)
		event_param_payload, event_param_ok := event_param.payload.(^Entity_Variable_Payload)
		assert(param_ok && param_payload != nil && event_param_ok && event_param_payload != nil)
		param_payload.passing = event_param_payload.passing
	}
}

checker_routine_parameter_named :: proc(
	payload: ^Entity_Routine_Payload,
	name: string_interner.String,
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
	if !string_interner.is_valid(owner_payload.superclass_name) {
		return
	}
	if _, super, super_ok := checker_lookup_lexical_declaration_from_scope(owner.scope, .Type, owner_payload.superclass_name);
	   super_ok && super.kind == .Class {
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
	interned := checker_intern_name(ctx.project, name)
	if entity, found := scope_lookup_declaration(payload.signature_scope, .Value, interned); found {
		return entity
	}
	if target.type == nil {
		target.type = project_type_class_or_interface(ctx.project, target.name, target, target.kind)
	}
	entity := project_new_entity(ctx.project, .Parameter)
	entity.node = routine.node
	entity.owner = routine
	entity.source_file = routine.source_file
	entity.type = project_type_ref(ctx.project, target.type)
	entity.state = .Resolved
	entity.flags += {.Parameter, .Has_Declared_Type}
	decl := project_new_decl_info(ctx.project, entity, payload.signature_scope, interned, .Parameter, routine.name_range, routine.node)
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
) -> (^Entity, bool, bool) {
	if namespace != .Value {
		return nil, false, false
	}
	interned := checker_intern_name(ctx.project, name)
	if interned != checker_intern_name(ctx.project, "me") &&
	   interned != checker_intern_name(ctx.project, "super") {
		return nil, false, false
	}
	if _, entity, ok := checker_lookup_declaration(ctx, .Value, interned); ok {
		checker_add_entity_use(ctx, node, entity)
		return entity, true, true
	}
	checker_add_diagnostic(ctx, .Invalid_Context, node.range if node != nil else Range{}, "invalid object receiver")
	return nil, false, true
}

checker_lookup_object_member_from_scope :: proc(
	scope: ^Scope,
	owner: ^Entity,
	namespace: Namespace,
	name: string_interner.String,
) -> (^Entity, bool) {
	if member, ok := checker_lookup_object_member(owner, namespace, name); ok {
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
	name: string_interner.String,
	range: Range = {},
) -> (^Entity, bool) {
	if member, ok := checker_lookup_object_member(owner, namespace, name); ok {
		if checker_member_visible_from_scope(ctx.scope, member) {
			return member, true
		}
		checker_add_diagnostic(ctx, .Inaccessible_Member, range, "member is not visible", member, member.decl_info)
	}
	return nil, false
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
	assert(ok && payload != nil)
	if !string_interner.is_valid(payload.superclass_name) {
		return false
	}
	if _, super, super_ok := checker_lookup_lexical_declaration_from_scope(class_entity.scope, .Type, payload.superclass_name);
	   super_ok && super.kind == .Class {
		return checker_class_entity_is_or_inherits_from(super, target_name, depth + 1)
	}
	return false
}

checker_entity_name_is_qualified :: proc(ctx: ^Checker_Context, name: string_interner.String) -> bool {
	_, _, ok := checker_qualified_member_parts(ctx, name)
	return ok
}

checker_qualified_member_parts :: proc(
	ctx: ^Checker_Context,
	name: string_interner.String,
) -> (
	string_interner.String,
	string_interner.String,
	bool,
) {
	if !string_interner.is_valid(name) {
		return {}, {}, false
	}
	text := string_interner.load(ctx.project.interner, name)
	for i := 0; i < len(text); i += 1 {
		if text[i] != '~' {
			continue
		}
		if i == 0 || i + 1 >= len(text) {
			return {}, {}, false
		}
		return checker_intern_name(ctx.project, text[:i]), checker_intern_name(ctx.project, text[i + 1:]), true
	}
	return {}, {}, false
}
