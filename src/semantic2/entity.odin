package abap_frontend_semantic

import "src:ast"
import string_interner "src:string_interner"
import "src:tokenizer"

import "core:mem"

Range :: tokenizer.Range

Type :: struct {}

Structure :: struct {}

Decl_Info :: struct {}

Entity :: struct {
	kind:         Entity_Kind,
	state:        Entity_State,
	flags:        Entity_Flags,
	name:         string_interner.String,
	name_range:   Range,
	decl_range:   Range,
	source_file:  ^Project_File,
	scope:        ^Scope,
	owner:        ^Entity,
	type:         ^Type,
	decl_info:    ^Decl_Info,
	node:         ^ast.Node,
	order_in_src: u64,
	payload:      Entity_Payload,
}

Entity_Kind :: enum {
	Invalid,
	Builtin_Type,
	Builtin_Routine,
	Builtin_Constant,
	Builtin_Variable,
	Variable,
	Constant,
	Enum_Member,
	Type_Def,
	Field_Symbol,
	Form,
	Parameter,
	Exception,
	Class,
	Interface,
	Method,
	Field,
	Include,
	Event,
	Alias,
	Module,
	Control,
	Report,
}

Entity_Payload :: union #shared_nil {
	^Entity_Value_Payload,
	^Entity_Constant_Payload,
	^Entity_Type_Name_Payload,
	^Entity_Object_Payload,
	^Entity_Routine_Payload,
	^Entity_Field_Payload,
	^Entity_Alias_Payload,
	^Entity_Include_Payload,
	^Entity_Report_Payload,
	^Entity_Builtin_Payload,
}

Entity_State :: enum {
	Unresolved,
	Resolving,
	Resolved,
	Failed,
}

Entity_Flag :: enum {
	Builtin,
	Used,
	Visited,
	Forward,
	Static,
	Abstract,
	Redefinition,
	Optional,
	Has_Default_Value,
	Has_Declared_Type,
	Untyped,
	For_Event,
	Has_Implementation,
	Global,
	Read_Only,
	Parameter,
	Field,
}
Entity_Flags :: bit_set[Entity_Flag]

Visibility :: enum {
	Public,
	Protected,
	Private,
}

Class_Member_Kind :: enum {
	None,
	Attribute,
	Method,
	Event,
}

Field_Type_Ref_Data :: struct {
	namespace:       Namespace,
	is_ref:          bool,
	base_name:       string_interner.String,
	base_range:      Range,
	field_path:      [dynamic]string_interner.String,
	field_ranges:    [dynamic]Range,
	field_derefs:    [dynamic]bool,
	field_selectors: [dynamic]ast.Selector_Op,
}

Entity_Value_Payload :: struct {
	type_expr:   ^ast.Node,
	init_expr:   ^ast.Node,
	field_index: i32,
	passing:     Entity_Parameter_Passing,
	section:     Entity_Parameter_Section,
}

Entity_Constant_Payload :: struct {
	value_display:     string,
	is_enum_implicit:  bool,
	field_group_index: i32,
}

Entity_Type_Name_Payload :: struct {
	is_alias:       bool,
	underlying:     ^Type,
	structure:      ^Structure,
	original_type:  ^Type,
	specialization: ^Type,
}

Entity_Object_Payload :: struct {
	kind:                   Entity_Object_Kind,
	definition_scope:       ^Scope,
	signature:              string,
	superclass_name:        string_interner.String,
	implemented_interfaces: [dynamic]string_interner.String,
	is_abstract:            bool,
}

Entity_Routine_Payload :: struct {
	signature_scope:      ^Scope,
	body_scope:           ^Scope,
	signature:            string,
	parameters:           [dynamic]^Entity,
	exceptions:           [dynamic]string_interner.String,
	visibility:           Visibility,
	member_kind:          Class_Member_Kind,
	event_name:           string_interner.String,
	event_range:          Range,
	event_source_type:    Field_Type_Ref_Data,
	is_static:            bool,
	is_redefinition:      bool,
	for_event:            bool,
	has_implementation:   bool,
	implementation_unit:  ^Project_File,
	implementation_range: Range,
}

Entity_Field_Payload :: struct {
	owner_structure:         ^Structure,
	decl_unit:               ^Project_File,
	field_index:             i32,
	description:             string,
	include_renaming_suffix: string,
	is_key:                  bool,
	is_include:              bool,
}

Entity_Alias_Payload :: struct {
	target_interface_name: string_interner.String,
	target_member_name:    string_interner.String,
	visibility:            Visibility,
}

Entity_Include_Payload :: struct {
	target:     ^Project_File,
	has_target: bool,
	if_found:   bool,
}

Entity_Report_Payload :: struct {
	provided_names: [dynamic]string_interner.String,
}

Entity_Builtin_Payload :: struct {
	id:          i32,
	description: string,
}

Entity_Object_Kind :: enum {
	Class,
	Interface,
}

Entity_Parameter_Section :: enum {
	None,
	Method_Importing,
	Method_Exporting,
	Method_Changing,
	Method_Receiving,
	Method_Returning,
	Form_Tables,
	Form_Using,
	Form_Changing,
	Function_Importing,
	Function_Exporting,
	Function_Changing,
	Function_Tables,
}

Entity_Parameter_Passing :: enum {
	None,
	Direct,
	Value,
	Reference,
}

entity_kind_is_builtin :: #force_inline proc(kind: Entity_Kind) -> bool {
	return(
		kind == .Builtin_Type ||
		kind == .Builtin_Routine ||
		kind == .Builtin_Constant ||
		kind == .Builtin_Variable \
	)
}

entity_kind_occupies :: proc(kind: Entity_Kind, namespace: Namespace) -> bool {
	switch kind {
	case .Builtin_Type, .Type_Def, .Class, .Interface:
		return namespace == .Type
	case .Builtin_Routine, .Form, .Method, .Module, .Event:
		return namespace == .Routine
	case .Alias:
		return false
	case .Builtin_Constant,
	     .Builtin_Variable,
	     .Variable,
	     .Constant,
	     .Enum_Member,
	     .Field_Symbol,
	     .Parameter,
	     .Exception,
	     .Field,
	     .Include,
	     .Control,
	     .Report:
		return namespace == .Value
	case .Invalid:
	}
	return false
}

entity_default_payload :: proc(kind: Entity_Kind, allocator: mem.Allocator) -> Entity_Payload {
	switch kind {
	case .Builtin_Type, .Builtin_Routine, .Builtin_Constant, .Builtin_Variable:
		payload := new(Entity_Builtin_Payload, allocator)
		return payload
	case .Variable, .Field_Symbol, .Parameter, .Exception, .Control:
		payload := new(Entity_Value_Payload, allocator)
		return payload
	case .Constant, .Enum_Member:
		payload := new(Entity_Constant_Payload, allocator)
		return payload
	case .Type_Def:
		payload := new(Entity_Type_Name_Payload, allocator)
		return payload
	case .Class:
		payload := new(Entity_Object_Payload, allocator)
		payload^ = Entity_Object_Payload {
			kind = .Class,
		}
		return payload
	case .Interface:
		payload := new(Entity_Object_Payload, allocator)
		payload^ = Entity_Object_Payload {
			kind = .Interface,
		}
		return payload
	case .Form, .Method, .Module, .Event:
		payload := new(Entity_Routine_Payload, allocator)
		return payload
	case .Field:
		payload := new(Entity_Field_Payload, allocator)
		return payload
	case .Alias:
		payload := new(Entity_Alias_Payload, allocator)
		return payload
	case .Include:
		payload := new(Entity_Include_Payload, allocator)
		return payload
	case .Report:
		payload := new(Entity_Report_Payload, allocator)
		return payload
	case .Invalid:
	}
	return nil
}
