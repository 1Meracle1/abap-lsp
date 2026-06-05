package abap_frontend_semantic

import "src:tokenizer"
import "src:ast"

import "core:mem"
import "core:strings"

Scope_Declaration_Key :: struct {
	namespace: Namespace,
	name:      string,
}

Scope :: struct {
	id:          Scope_Id,
	node:        ^ast.Node,
	parent:      Scope_Id,
	next:        Scope_Id,
	head_child:  Scope_Id,
	index:       i32,
	elements:    map[Scope_Declaration_Key]Entity_Id,
	imported:    [dynamic]Scope_Id,
	decl_info:   Decl_Info_Id,
	flags:       Scope_Flags,
	kind:        Scope_Kind,
	range:       tokenizer.Range,
	owner:       Entity_Id,
	declarations: [dynamic]Entity_Id,
	children:     [dynamic]Scope_Id,
}

Entity_Arena :: struct {
	source_file: Source_File_Id,
	entities:    [dynamic]Entity,
	scopes:      [dynamic]Scope,
	allocator:   mem.Allocator,
}

Entity_Desc :: struct {
	kind:         Entity_Kind,
	name:         string,
	name_range:   tokenizer.Range,
	decl_range:   tokenizer.Range,
	source_file:  Source_File_Id,
	scope:        Scope_Id,
	owner:        Entity_Id,
	type:         Type_Id,
	decl_info:    Decl_Info_Id,
	node:         ^ast.Node,
	state:        Entity_State,
	flags:        Entity_Flags,
	order_in_src: u64,
	type_shape:   Entity_Type_Shape,
	payload:      Entity_Payload,
}

entity_arena_make :: proc(allocator: mem.Allocator, source_file := INVALID_SOURCE_FILE_ID) -> Entity_Arena {
	return Entity_Arena {
		source_file = source_file,
		entities    = make([dynamic]Entity, 0, 64, allocator),
		scopes      = make([dynamic]Scope, 0, 16, allocator),
		allocator   = allocator,
	}
}

entity_arena_destroy :: proc(arena: ^Entity_Arena) {
	if arena == nil {
		return
	}
	for i in 0 ..< len(arena.entities) {
		entity_destroy(&arena.entities[i], arena.allocator)
	}
	for i in 0 ..< len(arena.scopes) {
		scope := &arena.scopes[i]
		if len(scope.declarations) > 0 {
			delete(scope.declarations)
		}
		if scope.elements != nil {
			delete(scope.elements)
		}
		if len(scope.imported) > 0 {
			delete(scope.imported)
		}
		if len(scope.children) > 0 {
			delete(scope.children)
		}
	}
	if len(arena.entities) > 0 {
		delete(arena.entities)
	}
	if len(arena.scopes) > 0 {
		delete(arena.scopes)
	}
	arena^ = {}
}

entity_desc_make :: proc(
	kind: Entity_Kind,
	name: string,
	scope := INVALID_SCOPE_ID,
) -> Entity_Desc {
	return Entity_Desc {
		kind        = kind,
		name        = name,
		source_file = INVALID_SOURCE_FILE_ID,
		scope       = scope,
		owner       = INVALID_ENTITY_ID,
		type        = UNKNOWN_TYPE_ID,
		decl_info   = INVALID_DECL_INFO_ID,
		state       = .Unresolved,
		type_shape  = entity_type_shape_default(),
	}
}

entity_type_shape_default :: #force_inline proc "contextless" () -> Entity_Type_Shape {
	return Entity_Type_Shape{structure = INVALID_STRUCTURE_ID}
}

entity_arena_add_scope :: proc(
	arena: ^Entity_Arena,
	kind: Scope_Kind,
	range: tokenizer.Range,
	parent := INVALID_SCOPE_ID,
	owner := INVALID_ENTITY_ID,
) -> Scope_Id {
	assert(arena != nil)
	id := Scope_Id(u32(len(arena.scopes)))
	append(
		&arena.scopes,
		Scope {
			id           = id,
			parent       = parent,
			next         = INVALID_SCOPE_ID,
			head_child   = INVALID_SCOPE_ID,
			decl_info    = INVALID_DECL_INFO_ID,
			flags        = scope_flags_for_kind(kind),
			kind         = kind,
			range        = range,
			owner        = owner,
			declarations = make([dynamic]Entity_Id, 0, 8, arena.allocator),
			elements     = make(map[Scope_Declaration_Key]Entity_Id, 0, arena.allocator),
			imported     = make([dynamic]Scope_Id, 0, 0, arena.allocator),
			children     = make([dynamic]Scope_Id, 0, 4, arena.allocator),
		},
	)
	if parent != INVALID_SCOPE_ID {
		parent_scope := entity_arena_scope(arena, parent)
		assert(parent_scope != nil)
		if parent_scope.head_child == INVALID_SCOPE_ID {
			parent_scope.head_child = id
		} else if len(parent_scope.children) > 0 {
			previous_child := parent_scope.children[len(parent_scope.children) - 1]
			child_scope := entity_arena_scope(arena, previous_child)
			assert(child_scope != nil)
			child_scope.next = id
		}
		append(&parent_scope.children, id)
	}
	return id
}

scope_flags_for_kind :: proc "contextless" (kind: Scope_Kind) -> Scope_Flags {
	#partial switch kind {
	case .File:
		return {.File, .Global}
	case .Form, .Module, .Method:
		return {.Procedure}
	case .Class, .Interface:
		return {.Type}
	case:
	}
	return {}
}

entity_arena_add_entity :: proc(
	arena: ^Entity_Arena,
	kind: Entity_Kind,
	name: string,
	scope := INVALID_SCOPE_ID,
	decl_range := tokenizer.Range{},
) -> Entity_Id {
	desc := entity_desc_make(kind, name, scope)
	desc.decl_range = decl_range
	return entity_arena_add_entity_desc(arena, desc)
}

entity_arena_add_entity_desc :: proc(arena: ^Entity_Arena, desc: Entity_Desc) -> Entity_Id {
	assert(arena != nil)
	id := Entity_Id(u32(len(arena.entities)))
	source_file := desc.source_file
	if source_file == INVALID_SOURCE_FILE_ID {
		source_file = arena.source_file
	}
	flags := desc.flags
	if entity_kind_is_builtin(desc.kind) {
		flags += {.Builtin}
	}
	payload: Entity_Payload
	if desc.payload != nil {
		payload = entity_payload_clone(desc.payload, arena.allocator)
	} else {
		payload = entity_default_payload(desc.kind, arena.allocator)
	}
	entity := Entity {
		id           = id,
		kind         = desc.kind,
		state        = desc.state,
		flags        = flags,
		name         = entity_canonical_name(desc.name, arena.allocator),
		name_range   = desc.name_range,
		decl_range   = desc.decl_range,
		source_file  = source_file,
		scope        = desc.scope,
		owner        = desc.owner,
		type         = desc.type,
		decl_info    = desc.decl_info,
		node         = desc.node,
		order_in_src = desc.order_in_src,
		type_shape   = entity_type_shape_clone(desc.type_shape, arena.allocator),
		payload      = payload,
	}
	append(&arena.entities, entity)
	if desc.scope != INVALID_SCOPE_ID {
		entity_arena_record_declaration(arena, desc.scope, id)
	}
	return id
}

entity_arena_entity :: proc(arena: ^Entity_Arena, id: Entity_Id) -> ^Entity {
	if arena == nil || id == INVALID_ENTITY_ID {
		return nil
	}
	index := entity_id_index(id)
	if index < 0 || index >= len(arena.entities) {
		return nil
	}
	return &arena.entities[index]
}

entity_arena_scope :: proc(arena: ^Entity_Arena, id: Scope_Id) -> ^Scope {
	if arena == nil || id == INVALID_SCOPE_ID {
		return nil
	}
	index := scope_id_index(id)
	if index < 0 || index >= len(arena.scopes) {
		return nil
	}
	return &arena.scopes[index]
}

entity_arena_record_declaration :: proc(arena: ^Entity_Arena, scope_id: Scope_Id, entity_id: Entity_Id) {
	scope := entity_arena_scope(arena, scope_id)
	entity := entity_arena_entity(arena, entity_id)
	assert(scope != nil && entity != nil)
	append(&scope.declarations, entity_id)
	namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in namespaces {
		if !entity_kind_occupies(entity.kind, namespace) {
			continue
		}
		key := Scope_Declaration_Key{namespace = namespace, name = entity.name}
		if _, exists := scope.elements[key]; !exists {
			scope.elements[key] = entity_id
		}
	}
}

entity_arena_lookup :: proc(
	arena: ^Entity_Arena,
	scope_id: Scope_Id,
	namespace: Namespace,
	name: string,
) -> (Entity_Id, bool) {
	scope := entity_arena_scope(arena, scope_id)
	if scope == nil || name == "" {
		return INVALID_ENTITY_ID, false
	}
	canonical := strings.to_lower(name, context.temp_allocator)
	if id, ok := scope.elements[Scope_Declaration_Key{namespace = namespace, name = canonical}]; ok {
		return id, true
	}
	return INVALID_ENTITY_ID, false
}

entity_arena_scope_has_declared :: proc(
	arena: ^Entity_Arena,
	scope_id: Scope_Id,
	namespace: Namespace,
	name: string,
) -> bool {
	scope := entity_arena_scope(arena, scope_id)
	if scope == nil || name == "" {
		return false
	}
	canonical := strings.to_lower(name, context.temp_allocator)
	for entity_id in scope.declarations {
		if entity := entity_arena_entity(arena, entity_id);
		   entity != nil &&
		   !entity_kind_is_builtin(entity.kind) &&
		   entity.name == canonical &&
		   entity_kind_occupies(entity.kind, namespace) {
			return true
		}
	}
	return false
}

entity_arena_find :: proc(arena: ^Entity_Arena, name: string, kind: Entity_Kind) -> ^Entity {
	if arena == nil || name == "" {
		return nil
	}
	canonical := strings.to_lower(name, context.temp_allocator)
	for &entity in arena.entities {
		if entity.kind == kind && entity.name == canonical {
			return &entity
		}
	}
	return nil
}

entity_canonical_name :: proc(name: string, allocator: mem.Allocator) -> string {
	if name == "" {
		return ""
	}
	return strings.to_lower(name, allocator)
}

entity_destroy :: proc(entity: ^Entity, allocator: mem.Allocator) {
	if entity == nil {
		return
	}
	if entity.name != "" {
		delete(entity.name, allocator)
	}
	entity_type_shape_destroy(&entity.type_shape, allocator)
	entity_payload_destroy(entity.payload, allocator)
	entity^ = {}
}

entity_type_shape_clone :: proc(shape: Entity_Type_Shape, allocator: mem.Allocator) -> Entity_Type_Shape {
	out := shape
	out.declared_type = field_type_ref_clone(shape.declared_type, allocator)
	out.type_clause_display = entity_clone_string(shape.type_clause_display, allocator)
	out.value_clause_display = entity_clone_string(shape.value_clause_display, allocator)
	return out
}

entity_type_shape_destroy :: proc(shape: ^Entity_Type_Shape, allocator: mem.Allocator) {
	if shape == nil {
		return
	}
	field_type_ref_destroy(&shape.declared_type, allocator)
	if shape.type_clause_display != "" {
		delete(shape.type_clause_display, allocator)
	}
	if shape.value_clause_display != "" {
		delete(shape.value_clause_display, allocator)
	}
	shape^ = entity_type_shape_default()
}

field_type_ref_clone :: proc(ref: Field_Type_Ref_Data, allocator: mem.Allocator) -> Field_Type_Ref_Data {
	out := ref
	out.base_name = entity_clone_string(ref.base_name, allocator)
	out.field_path = make([dynamic]string, 0, len(ref.field_path), allocator)
	for field in ref.field_path {
		append(&out.field_path, entity_clone_string(field, allocator))
	}
	out.field_ranges = make([dynamic]tokenizer.Range, 0, len(ref.field_ranges), allocator)
	for range in ref.field_ranges {
		append(&out.field_ranges, range)
	}
	out.field_derefs = make([dynamic]bool, 0, len(ref.field_derefs), allocator)
	for deref in ref.field_derefs {
		append(&out.field_derefs, deref)
	}
	if len(ref.field_selectors) > 0 {
		out.field_selectors = make([dynamic]ast.Selector_Op, 0, len(ref.field_selectors), allocator)
		for selector in ref.field_selectors {
			append(&out.field_selectors, selector)
		}
	}
	return out
}

field_type_ref_destroy :: proc(ref: ^Field_Type_Ref_Data, allocator: mem.Allocator) {
	if ref == nil {
		return
	}
	if ref.base_name != "" {
		delete(ref.base_name, allocator)
	}
	for field in ref.field_path {
		if field != "" {
			delete(field, allocator)
		}
	}
	if len(ref.field_path) > 0 {
		delete(ref.field_path)
	}
	if len(ref.field_ranges) > 0 {
		delete(ref.field_ranges)
	}
	if len(ref.field_derefs) > 0 {
		delete(ref.field_derefs)
	}
	if len(ref.field_selectors) > 0 {
		delete(ref.field_selectors)
	}
	ref^ = {}
}

entity_payload_clone :: proc(payload: Entity_Payload, allocator: mem.Allocator) -> Entity_Payload {
	if payload == nil {
		return nil
	}
	#partial switch p in payload {
	case ^Entity_Value_Payload:
		out := new(Entity_Value_Payload, allocator)
		out^ = p^
		return out
	case ^Entity_Constant_Payload:
		out := new(Entity_Constant_Payload, allocator)
		out^ = p^
		out.value_display = entity_clone_string(p.value_display, allocator)
		return out
	case ^Entity_Type_Name_Payload:
		out := new(Entity_Type_Name_Payload, allocator)
		out^ = p^
		return out
	case ^Entity_Object_Payload:
		out := new(Entity_Object_Payload, allocator)
		out^ = p^
		out.signature = entity_clone_string(p.signature, allocator)
		out.superclass_name = entity_clone_string(p.superclass_name, allocator)
		out.implemented_interfaces = make([dynamic]string, 0, len(p.implemented_interfaces), allocator)
		for interface_name in p.implemented_interfaces {
			append(&out.implemented_interfaces, entity_clone_string(interface_name, allocator))
		}
		return out
	case ^Entity_Routine_Payload:
		out := new(Entity_Routine_Payload, allocator)
		out^ = p^
		out.signature = entity_clone_string(p.signature, allocator)
		out.parameters = make([dynamic]Entity_Id, 0, len(p.parameters), allocator)
		for parameter in p.parameters {
			append(&out.parameters, parameter)
		}
		out.exceptions = make([dynamic]string, 0, len(p.exceptions), allocator)
		for exception in p.exceptions {
			append(&out.exceptions, entity_clone_string(exception, allocator))
		}
		out.event_name = entity_clone_string(p.event_name, allocator)
		out.event_source_type = field_type_ref_clone(p.event_source_type, allocator)
		return out
	case ^Entity_Field_Payload:
		out := new(Entity_Field_Payload, allocator)
		out^ = p^
		out.description = entity_clone_string(p.description, allocator)
		out.include_renaming_suffix = entity_clone_string(p.include_renaming_suffix, allocator)
		return out
	case ^Entity_Alias_Payload:
		out := new(Entity_Alias_Payload, allocator)
		out^ = p^
		out.target_interface_name = entity_clone_string(p.target_interface_name, allocator)
		out.target_member_name = entity_clone_string(p.target_member_name, allocator)
		return out
	case ^Entity_Include_Payload:
		out := new(Entity_Include_Payload, allocator)
		out^ = p^
		return out
	case ^Entity_Report_Payload:
		out := new(Entity_Report_Payload, allocator)
		out^ = p^
		out.provided_names = make([dynamic]string, 0, len(p.provided_names), allocator)
		for provided in p.provided_names {
			append(&out.provided_names, entity_clone_string(provided, allocator))
		}
		return out
	case ^Entity_Builtin_Payload:
		out := new(Entity_Builtin_Payload, allocator)
		out^ = p^
		out.description = entity_clone_string(p.description, allocator)
		return out
	}
	return nil
}

entity_payload_destroy :: proc(payload: Entity_Payload, allocator: mem.Allocator) {
	if payload == nil {
		return
	}
	#partial switch p in payload {
	case ^Entity_Value_Payload:
		free(p, allocator)
	case ^Entity_Constant_Payload:
		if p.value_display != "" {
			delete(p.value_display, allocator)
		}
		free(p, allocator)
	case ^Entity_Type_Name_Payload:
		free(p, allocator)
	case ^Entity_Object_Payload:
		if p.signature != "" {
			delete(p.signature, allocator)
		}
		if p.superclass_name != "" {
			delete(p.superclass_name, allocator)
		}
		for interface_name in p.implemented_interfaces {
			if interface_name != "" {
				delete(interface_name, allocator)
			}
		}
		if len(p.implemented_interfaces) > 0 {
			delete(p.implemented_interfaces)
		}
		free(p, allocator)
	case ^Entity_Routine_Payload:
		if p.signature != "" {
			delete(p.signature, allocator)
		}
		if len(p.parameters) > 0 {
			delete(p.parameters)
		}
		for exception in p.exceptions {
			if exception != "" {
				delete(exception, allocator)
			}
		}
		if len(p.exceptions) > 0 {
			delete(p.exceptions)
		}
		if p.event_name != "" {
			delete(p.event_name, allocator)
		}
		field_type_ref_destroy(&p.event_source_type, allocator)
		free(p, allocator)
	case ^Entity_Field_Payload:
		if p.description != "" {
			delete(p.description, allocator)
		}
		if p.include_renaming_suffix != "" {
			delete(p.include_renaming_suffix, allocator)
		}
		free(p, allocator)
	case ^Entity_Alias_Payload:
		if p.target_interface_name != "" {
			delete(p.target_interface_name, allocator)
		}
		if p.target_member_name != "" {
			delete(p.target_member_name, allocator)
		}
		free(p, allocator)
	case ^Entity_Include_Payload:
		free(p, allocator)
	case ^Entity_Report_Payload:
		for provided in p.provided_names {
			if provided != "" {
				delete(provided, allocator)
			}
		}
		if len(p.provided_names) > 0 {
			delete(p.provided_names)
		}
		free(p, allocator)
	case ^Entity_Builtin_Payload:
		if p.description != "" {
			delete(p.description, allocator)
		}
		free(p, allocator)
	}
}

entity_clone_string :: proc(value: string, allocator: mem.Allocator) -> string {
	if value == "" {
		return ""
	}
	return strings.clone(value, allocator)
}
