package abap_frontend_semantic_analyze

import "src:tokenizer"

import "core:mem"

Namespace :: enum {
	Value,
	Type,
	Routine,
}

Scope_Kind :: enum {
	File,
	Form,
	Module,
	Event_Block,
	Class,
	Interface,
	Method,
	Signature,
	If_Branch,
	Elseif_Branch,
	Else_Branch,
	When_Branch,
	Catch_Clause,
	Cleanup_Clause,
	While_Block,
	Do_Block,
	Loop_Block,
	At_Block,
	Try_Block,
	Select_Block,
	Constructor_For,
}

Scope_Data :: struct {
	id:                                  Scope_Id,
	kind:                                Scope_Kind,
	range:                               tokenizer.Range,
	parent:                              Scope_Id,
	owner:                               Symbol_Id,
	declarations:                        [dynamic]Symbol_Id,
	declarations_by_name:                map[Scope_Declaration_Key]Symbol_Id,
	children:                            [dynamic]Scope_Id,
	allows_internal_table_line_selector: bool,
}

Scope_Declaration_Key :: struct {
	namespace: Namespace,
	name:      string,
}

Class_Scope_Index_Key :: struct {
	class_symbol: Symbol_Id,
	namespace:    Namespace,
	name:         string,
}

Scope_Index :: struct {
	class_symbols:            map[Class_Scope_Index_Key]Symbol_Id,
	enclosing_classes:        [dynamic]Symbol_Id,
	enclosing_methods:        [dynamic]Symbol_Id,
	enclosing_method_scopes:  [dynamic]Scope_Id,
	superclasses:             map[Symbol_Id]string,
}

scope_index_make :: proc(allocator: mem.Allocator) -> Scope_Index {
	return Scope_Index {
		class_symbols = make(map[Class_Scope_Index_Key]Symbol_Id, 0, allocator),
		superclasses = make(map[Symbol_Id]string, 0, allocator),
	}
}

scope_index_destroy :: proc(index: ^Scope_Index) {
	delete(index.class_symbols)
	if len(index.enclosing_classes) > 0 {
		delete(index.enclosing_classes)
	}
	if len(index.enclosing_methods) > 0 {
		delete(index.enclosing_methods)
	}
	if len(index.enclosing_method_scopes) > 0 {
		delete(index.enclosing_method_scopes)
	}
	delete(index.superclasses)
	index^ = {}
}

add_scope :: proc(
	unit: ^Unit_Analysis,
	kind: Scope_Kind,
	range: tokenizer.Range,
	parent := INVALID_SCOPE_ID,
	owner := INVALID_SYMBOL_ID,
	allocator: mem.Allocator,
) -> Scope_Id {
	id := Scope_Id(u32(len(unit.scopes)))
	scope := Scope_Data {
		id           = id,
		kind         = kind,
		range        = range,
		parent       = parent,
		owner        = owner,
		declarations = make([dynamic]Symbol_Id, 0, 8, allocator),
		declarations_by_name = make(map[Scope_Declaration_Key]Symbol_Id, 0, allocator),
		children     = make([dynamic]Scope_Id, 0, 4, allocator),
	}
	append(&unit.scopes, scope)
	if parent != INVALID_SCOPE_ID {
		append(&unit.scopes[scope_id_index(parent)].children, id)
	}
	return id
}

scope_record_declaration :: proc(unit: ^Unit_Analysis, scope_id: Scope_Id, symbol_id: Symbol_Id) {
	s := scope(unit, scope_id)
	item := symbol(unit, symbol_id)
	assert(s != nil && item != nil)
	append(&s.declarations, symbol_id)
	namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in namespaces {
		if !symbol_kind_occupies(item.kind, namespace) {
			continue
		}
		key := Scope_Declaration_Key{namespace = namespace, name = item.name}
		if _, exists := s.declarations_by_name[key]; !exists {
			s.declarations_by_name[key] = symbol_id
		}
	}
}

scope_lookup_declaration :: proc(
	unit: ^Unit_Analysis,
	scope_id: Scope_Id,
	namespace: Namespace,
	name: string,
) -> (
	Symbol_Id,
	bool,
) {
	if s := scope(unit, scope_id); s != nil {
		if id, ok := s.declarations_by_name[Scope_Declaration_Key{namespace = namespace, name = name}]; ok {
			return id, true
		}
	}
	return INVALID_SYMBOL_ID, false
}

scope_has_declared_declaration :: proc(
	unit: ^Unit_Analysis,
	scope_id: Scope_Id,
	namespace: Namespace,
	name: string,
) -> bool {
	if s := scope(unit, scope_id); s != nil {
		for symbol_id in s.declarations {
			if item := symbol(unit, symbol_id);
			   item != nil &&
			   !symbol_kind_is_builtin(item.kind) &&
			   item.name == name &&
			   symbol_kind_occupies(item.kind, namespace) {
				return true
			}
		}
	}
	return false
}
