package abap_frontend_semantic

import string_interner "src:string_interner"

Scope_Kind :: enum {
	Builtin,
	File,
	Class,
	Interface,
	Form,
	Module, // Function module; dialog MODULE blocks do not own local declarations.
	Method,
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
