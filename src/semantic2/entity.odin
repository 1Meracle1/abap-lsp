package abap_frontend_semantic

import "src:ast"
import "src:tokenizer"

import "core:mem"

Source_File_Id :: distinct u32
Scope_Id :: distinct u32
Entity_Id :: distinct u32
Structure_Id :: distinct u32
Decl_Info_Id :: distinct u32
Type_Id :: distinct u32

INVALID_SOURCE_FILE_ID :: Source_File_Id(0xffffffff)
INVALID_SCOPE_ID :: Scope_Id(0xffffffff)
INVALID_ENTITY_ID :: Entity_Id(0xffffffff)
INVALID_STRUCTURE_ID :: Structure_Id(0xffffffff)
INVALID_DECL_INFO_ID :: Decl_Info_Id(0xffffffff)
INVALID_TYPE_ID :: Type_Id(0xffffffff)
UNKNOWN_TYPE_ID :: Type_Id(0)

Entity :: struct {
	id:           Entity_Id,
	kind:         Entity_Kind,
	state:        Entity_State,
	flags:        Entity_Flags,
	name:         string,
	name_range:   tokenizer.Range,
	decl_range:   tokenizer.Range,
	source_file:  Source_File_Id,
	scope:        Scope_Id,
	owner:        Entity_Id,
	type:         Type_Id,
	decl_info:    Decl_Info_Id,
	node:         ^ast.Node,
	order_in_src: u64,
	type_shape:   Entity_Type_Shape,
	payload:      Entity_Payload,
}

Entity_Payload :: union {
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

Namespace :: enum {
	Value,
	Type,
	Routine,
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

Scope_Flag :: enum {
	Builtin,
	Global,
	File,
	Procedure,
	Type,
	Context_Defined,
	Has_Been_Imported,
}
Scope_Flags :: bit_set[Scope_Flag]

Field_Type_Ref_Data :: struct {
	namespace:       Namespace,
	is_ref:          bool,
	base_name:       string,
	base_range:      tokenizer.Range,
	field_path:      [dynamic]string,
	field_ranges:    [dynamic]tokenizer.Range,
	field_derefs:    [dynamic]bool,
	field_selectors: [dynamic]ast.Selector_Op,
}

Entity_Type_Shape :: struct {
	structure:                Structure_Id,
	declared_type:            Field_Type_Ref_Data,
	type_clause_display:      string,
	value_clause_display:     string,
	type_clause_form:         ast.Data_Type_Form,
	has_declared_type:        bool,
	has_type_clause_form:     bool,
	type_clause_table_has_of: bool,
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
	underlying:     Type_Id,
	structure:      Structure_Id,
	original_type:  Type_Id,
	specialization: Type_Id,
}

Entity_Object_Payload :: struct {
	kind:                   Entity_Object_Kind,
	definition_scope:       Scope_Id,
	signature:              string,
	superclass_name:        string,
	implemented_interfaces: [dynamic]string,
	is_abstract:            bool,
}

Entity_Routine_Payload :: struct {
	signature_scope:      Scope_Id,
	body_scope:           Scope_Id,
	signature:            string,
	parameters:           [dynamic]Entity_Id,
	exceptions:           [dynamic]string,
	visibility:           Visibility,
	member_kind:          Class_Member_Kind,
	event_name:           string,
	event_range:          tokenizer.Range,
	event_source_type:    Field_Type_Ref_Data,
	is_static:            bool,
	is_redefinition:      bool,
	for_event:            bool,
	has_implementation:   bool,
	implementation_unit:  Source_File_Id,
	implementation_range: tokenizer.Range,
}

Entity_Field_Payload :: struct {
	owner_structure:         Structure_Id,
	decl_unit:               Source_File_Id,
	field_index:             i32,
	description:             string,
	include_renaming_suffix: string,
	is_key:                  bool,
	is_include:              bool,
}

Entity_Alias_Payload :: struct {
	target_interface_name: string,
	target_member_name:    string,
	visibility:            Visibility,
}

Entity_Include_Payload :: struct {
	target:     Source_File_Id,
	has_target: bool,
	if_found:   bool,
}

Entity_Report_Payload :: struct {
	provided_names: [dynamic]string,
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
		payload^ = Entity_Object_Payload{kind = .Class, definition_scope = INVALID_SCOPE_ID}
		return payload
	case .Interface:
		payload := new(Entity_Object_Payload, allocator)
		payload^ = Entity_Object_Payload{kind = .Interface, definition_scope = INVALID_SCOPE_ID}
		return payload
	case .Form, .Method, .Module, .Event:
		payload := new(Entity_Routine_Payload, allocator)
		payload^ = Entity_Routine_Payload {
			signature_scope = INVALID_SCOPE_ID,
			body_scope = INVALID_SCOPE_ID,
			implementation_unit = INVALID_SOURCE_FILE_ID,
		}
		return payload
	case .Field:
		payload := new(Entity_Field_Payload, allocator)
		payload^ = Entity_Field_Payload {
			owner_structure = INVALID_STRUCTURE_ID,
			decl_unit = INVALID_SOURCE_FILE_ID,
		}
		return payload
	case .Alias:
		payload := new(Entity_Alias_Payload, allocator)
		return payload
	case .Include:
		payload := new(Entity_Include_Payload, allocator)
		payload^ = Entity_Include_Payload{target = INVALID_SOURCE_FILE_ID}
		return payload
	case .Report:
		payload := new(Entity_Report_Payload, allocator)
		return payload
	case .Invalid:
	}
	return nil
}

entity_id_index :: #force_inline proc "contextless" (id: Entity_Id) -> int {
	return int(id)
}

scope_id_index :: #force_inline proc "contextless" (id: Scope_Id) -> int {
	return int(id)
}
