package abap_frontend_semantic

import "../tokenizer"

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
