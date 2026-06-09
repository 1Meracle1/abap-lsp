package abap_frontend_semantic2

import "src:ast"
import string_interner "src:string_interner"
import "src:tokenizer"

import "core:mem"

Range :: tokenizer.Range

Type_Kind :: enum {
	Unknown,
	Builtin,
	Named,
	Structure,
	Table,
	Ref,
	Class,
	Interface,
	Routine,
}

Type :: struct {
	kind:       Type_Kind,
	name:       string_interner.String,
	entity:     ^Entity,
	structure:  ^Structure,
	base:       ^Type,
	table_form: ast.Data_Type_Form,
	routine:    Type_Routine_Info,
}

Type_Routine_Info :: struct {
	signature_scope: ^Scope,
	parameters:      [dynamic]^Entity,
	results:         [dynamic]^Entity,
	exceptions:      [dynamic]string_interner.String,
}

Structure :: struct {
	name:             string_interner.String,
	range:            Range,
	source_file:      ^Project_File,
	origin_structure: ^Structure,
	scope:            ^Scope,
	fields:           [dynamic]^Entity,
}

Structure_Field_Flag :: enum {
	Has_Decl_Range,
	Has_Type_Ref,
	Is_Key,
	Is_Include,
}
Structure_Field_Flags :: bit_set[Structure_Field_Flag]

Decl_Info_State :: enum {
	Unresolved,
	Resolving,
	Resolved,
	Failed,
}

Decl_Info :: struct {
	entity:         ^Entity,
	scope:          ^Scope,
	decl_node:      ^ast.Node,
	type_clause:    ^ast.Data_Type_Clause,
	occurs:         ^ast.Expr,
	value_clause:   ^ast.Value_Clause,
	default_clause: ^ast.Default_Clause,
	docs:           []ast.Ast_Trivia,
	comment:        []ast.Ast_Trivia,
	state:          Decl_Info_State,
}

Entity :: struct {
	kind:         Entity_Kind,
	state:        Entity_State,
	flags:        Entity_Flags,
	name:         string_interner.String,
	name_range:   Range,
	source_file:  ^Project_File,
	scope:        ^Scope,
	owner:        ^Entity,
	member_kind:  Class_Member_Kind,
	visibility:   Visibility,
	type:         ^Type,
	decl_info:    ^Decl_Info,
	node:         ^ast.Node,
	payload:      Entity_Payload,
}

Entity_Kind :: enum {
	Invalid,
	Builtin,
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
	^Entity_Variable_Payload,
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
	namespace:                Namespace,
	is_ref:                   bool,
	allow_type_lookup:        bool,
	base_name:                string_interner.String,
	base_range:               Range,
	field_path:               [dynamic]string_interner.String,
	field_ranges:             [dynamic]Range,
	field_derefs:             [dynamic]bool,
	field_selectors:          [dynamic]ast.Selector_Op,
}

Entity_Parameter_Value_Kind :: enum {
	Invalid,
	Constant,
	Nil,
	Location,
	Expression,
	Value,
}

Entity_Parameter_Value :: struct {
	kind:          Entity_Parameter_Value_Kind,
	original_expr: ^ast.Node,
	value:         ast.Exact_Value_Id,
	expr:          ^ast.Node,
}

Entity_Variable_Payload :: struct {
	type_expr:         ^ast.Node,
	init_expr:         ^ast.Node,
	field_index:       i32,
	field_group_index: i32,
	param_value:       Entity_Parameter_Value,
	passing:           Entity_Parameter_Passing,
	section:           Entity_Parameter_Section,
	docs:              []ast.Ast_Trivia,
	comment:           []ast.Ast_Trivia,
}

Entity_Constant_Flag :: enum {
	Implicit_Enum_Value,
}
Entity_Constant_Flags :: bit_set[Entity_Constant_Flag]

Entity_Constant_Payload :: struct {
	value:             ast.Exact_Value_Id,
	constant_value:    Constant_Value,
	param_value:       Entity_Parameter_Value,
	flags:             Entity_Constant_Flags,
	field_group_index: i32,
	docs:              []ast.Ast_Trivia,
	comment:           []ast.Ast_Trivia,
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
	superclass_range:       Range,
	implemented_interfaces: [dynamic]string_interner.String,
	friends:                [dynamic]string_interner.String,
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
	decl_range:              Range,
	field_index:             i32,
	type_ref:                Field_Type_Ref_Data,
	value_clause:            ^ast.Value_Clause,
	type_clause_form:        ast.Data_Type_Form,
	has_type_clause_form:    bool,
	include_renaming_suffix: string,
	flags:                   Structure_Field_Flags,
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

Constant_Value :: union #shared_nil {
	^Constant_Integer_Value,
	^Constant_Text_Value,
}

Constant_Integer_Value :: struct {
	value: i64,
}

Constant_Text_Value :: struct {
	value: string,
}

Entity_Builtin_Payload :: struct {
	id:                  i32,
	docs:                string,
	supports_named_args: bool,
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
	return kind == .Builtin
}

entity_is_builtin :: #force_inline proc(entity: ^Entity) -> bool {
	return entity != nil && .Builtin in entity.flags
}

entity_kind_occupies :: proc(kind: Entity_Kind, namespace: Namespace) -> bool {
	switch kind {
	case .Type_Def, .Class, .Interface:
		return namespace == .Type
	case .Builtin, .Form, .Method, .Module, .Event:
		return namespace == .Routine
	case .Alias:
		return false
	case .Variable,
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

entity_set_kind :: proc(entity: ^Entity, kind: Entity_Kind, allocator: mem.Allocator) {
	assert(entity != nil)
	entity.kind = kind
	entity.payload = entity_default_payload(kind, allocator)
	if entity_kind_is_builtin(kind) {
		entity.flags += {.Builtin}
	} else {
		entity.flags -= {.Builtin}
	}
}

entity_parameter_value_invalid :: #force_inline proc() -> Entity_Parameter_Value {
	return Entity_Parameter_Value{value = ast.INVALID_EXACT_VALUE_ID}
}

entity_default_payload :: proc(kind: Entity_Kind, allocator: mem.Allocator) -> Entity_Payload {
	switch kind {
	case .Builtin:
		payload := new(Entity_Builtin_Payload, allocator)
		return payload
	case .Variable, .Field_Symbol, .Parameter, .Exception, .Control:
		payload := new(Entity_Variable_Payload, allocator)
		payload.param_value = entity_parameter_value_invalid()
		return payload
	case .Constant, .Enum_Member:
		payload := new(Entity_Constant_Payload, allocator)
		payload.value = ast.INVALID_EXACT_VALUE_ID
		payload.param_value = entity_parameter_value_invalid()
		return payload
	case .Type_Def:
		payload := new(Entity_Type_Name_Payload, allocator)
		return payload
	case .Class:
		payload := new(Entity_Object_Payload, allocator)
		payload^ = Entity_Object_Payload {
			kind = .Class,
			implemented_interfaces = make([dynamic]string_interner.String, 0, 2, allocator),
			friends = make([dynamic]string_interner.String, 0, 1, allocator),
		}
		return payload
	case .Interface:
		payload := new(Entity_Object_Payload, allocator)
		payload^ = Entity_Object_Payload {
			kind = .Interface,
			implemented_interfaces = make([dynamic]string_interner.String, 0, 2, allocator),
			friends = make([dynamic]string_interner.String, 0, 1, allocator),
		}
		return payload
	case .Form, .Method, .Module, .Event:
		payload := new(Entity_Routine_Payload, allocator)
		payload.parameters = make([dynamic]^Entity, 0, 4, allocator)
		payload.exceptions = make([dynamic]string_interner.String, 0, 1, allocator)
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
		payload.provided_names = make([dynamic]string_interner.String, 0, 4, allocator)
		return payload
	case .Invalid:
	}
	return nil
}
