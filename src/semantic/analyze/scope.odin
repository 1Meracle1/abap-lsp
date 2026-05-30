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
	children:                            [dynamic]Scope_Id,
	allows_internal_table_line_selector: bool,
}

Scope_Index_Key :: struct {
	scope:     Scope_Id,
	namespace: Namespace,
	name:      string,
}

Class_Scope_Index_Key :: struct {
	class_symbol: Symbol_Id,
	namespace:    Namespace,
	name:         string,
}

Scope_Index :: struct {
	scope_count:       int,
	symbols:           map[Scope_Index_Key]Symbol_Id,
	class_symbols:     map[Class_Scope_Index_Key]Symbol_Id,
	enclosing_classes: [dynamic]Symbol_Id,
	superclasses:      map[Symbol_Id]string,
}

scope_index_make :: proc(allocator: mem.Allocator) -> Scope_Index {
	return Scope_Index {
		symbols = make(map[Scope_Index_Key]Symbol_Id, 0, allocator),
		class_symbols = make(map[Class_Scope_Index_Key]Symbol_Id, 0, allocator),
		superclasses = make(map[Symbol_Id]string, 0, allocator),
	}
}

scope_index_destroy :: proc(index: ^Scope_Index) {
	delete(index.symbols)
	delete(index.class_symbols)
	if len(index.enclosing_classes) > 0 {
		delete(index.enclosing_classes)
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
		children     = make([dynamic]Scope_Id, 0, 4, allocator),
	}
	append(&unit.scopes, scope)
	if parent != INVALID_SCOPE_ID {
		append(&unit.scopes[scope_id_index(parent)].children, id)
	}
	return id
}
