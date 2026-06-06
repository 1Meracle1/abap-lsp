package abap_frontend_semantic

import string_interner "src:string_interner"

Scope_Kind :: enum {
	Builtin,
	File,
	Structure,
	Class,
	Interface,
	Form,
	Module, // Function module; dialog MODULE blocks do not own local declarations.
	Method,
	Event,
	Constructor_For, // Constructor-expression helper field context.
}

Scope :: struct {
	kind:         Scope_Kind,
	parent:       ^Scope,
	next:         ^Scope,
	head_child:   ^Scope,
	elements:     map[Scope_Declaration_Key]^Entity,
	imported:     [dynamic]^Scope,
	decl_info:    ^Decl_Info,
	range:        Range,
	owner:        ^Entity,
	declarations: [dynamic]^Entity,
	children:     [dynamic]^Scope,
}

Namespace :: enum {
	Value,
	Type,
	Routine,
}

Scope_Declaration_Key :: struct {
	namespace: Namespace,
	name:      string_interner.String,
}

scope_insert_declaration :: proc(scope: ^Scope, entity: ^Entity) -> ^Entity {
	assert(scope != nil && entity != nil)

	namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in namespaces {
		if !entity_kind_occupies(entity.kind, namespace) {
			continue
		}
		key := Scope_Declaration_Key{namespace = namespace, name = entity.name}
		if existing, ok := scope.elements[key]; ok {
			return existing
		}
	}

	if entity.scope == nil {
		entity.scope = scope
	}
	append(&scope.declarations, entity)
	for namespace in namespaces {
		if !entity_kind_occupies(entity.kind, namespace) {
			continue
		}
		key := Scope_Declaration_Key{namespace = namespace, name = entity.name}
		scope.elements[key] = entity
	}
	return nil
}

scope_lookup_declaration :: proc(
	scope: ^Scope,
	namespace: Namespace,
	name: string_interner.String,
) -> (^Entity, bool) {
	assert(scope != nil)
	if entity, ok := scope.elements[Scope_Declaration_Key{namespace = namespace, name = name}]; ok {
		return entity, true
	}
	return nil, false
}
