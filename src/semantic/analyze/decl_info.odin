package abap_frontend_semantic_analyze

import "src:ast"
import "src:tokenizer"

import "core:strings"

Decl_Info_State :: enum {
	Unresolved,
	Resolving,
	Resolved,
	Failed,
}

Decl_Info_Flag :: enum {
	Is_Static,
	Is_Redefinition,
	For_Event,
	Has_Implementation,
	Is_Abstract,
	Has_Declared_Type,
	Has_Event_Derived_Type,
	Is_Optional,
	Is_Untyped,
	Has_Default_Value,
}
Decl_Info_Flags :: bit_set[Decl_Info_Flag]

Decl_Parameter_Section :: enum {
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

Decl_Parameter_Passing :: enum {
	None,
	Direct,
	Value,
	Reference,
}

Decl_Signature_Parameter_Data :: struct {
	symbol:                   Entity_Id,
	name:                     string,
	range:                    tokenizer.Range,
	section:                  Decl_Parameter_Section,
	passing:                  Decl_Parameter_Passing,
	type_id:                  Type_Id,
	declared_type:            Field_Type_Ref_Data,
	type_clause_display:      string,
	type_clause_form:         ast.Data_Type_Form,
	has_type_clause_form:     bool,
	type_clause_table_has_of: bool,
	flags:                    Decl_Info_Flags,
}

Decl_Signature_Exception_Data :: struct {
	name:  string,
	range: tokenizer.Range,
}

Decl_Info_Data :: struct {
	id:                          Decl_Info_Id,
	entity:                      Entity_Id,
	owner:                       Entity_Id,
	scope:                       Scope_Id,
	signature_scope:             Scope_Id,
	body_scope:                  Scope_Id,
	name:                        string,
	kind:                        Symbol_Kind,
	decl_range:                  tokenizer.Range,
	name_range:                  tokenizer.Range,
	signature:                   string,
	clause_kind:                 ast.Decl_Clause_Kind,
	clause_flags:                ast.Decl_Clause_Flags,
	type_clause:                 ^ast.Data_Type_Clause,
	value_clause:                ^ast.Value_Clause,
	default_clause:              ^ast.Default_Clause,
	visibility:                  Visibility,
	member_kind:                 Class_Member_Kind,
	implementation_unit:         Source_File_Id,
	implementation_range:        tokenizer.Range,
	effective_signature:         Symbol_Link,
	event_name:                  string,
	event_range:                 tokenizer.Range,
	event_source_type:           Field_Type_Ref_Data,
	event_source_type_id:        Type_Id,
	alias_target_interface_name: string,
	alias_target_member_name:    string,
	signature_parameters:        [dynamic]Decl_Signature_Parameter_Data,
	signature_exceptions:        [dynamic]Decl_Signature_Exception_Data,
	parameter_section:           Decl_Parameter_Section,
	parameter_passing:           Decl_Parameter_Passing,
	flags:                       Decl_Info_Flags,
	state:                       Decl_Info_State,
}

Method_Parameter_Section :: enum {
	Importing,
	Exporting,
	Changing,
	Receiving,
	Returning,
}

Class_Member_Parameter_Flag :: enum {
	Has_Declared_Type,
	Is_Optional,
	Has_Default_Value,
}
Class_Member_Parameter_Flags :: bit_set[Class_Member_Parameter_Flag]

Parameter_Passing_Kind :: enum {
	Direct,
	Value,
	Reference,
}

Class_Member_Parameter_Data :: struct {
	symbol:                   Symbol_Id,
	section:                  Method_Parameter_Section,
	name:                     string,
	range:                    tokenizer.Range,
	passing:                  Parameter_Passing_Kind,
	type_id:                  Type_Id,
	declared_type:            Field_Type_Ref_Data,
	type_clause_display:      string,
	type_clause_form:         ast.Data_Type_Form,
	has_type_clause_form:     bool,
	type_clause_table_has_of: bool,
	flags:                    Class_Member_Parameter_Flags,
}

Function_Module_Parameter_Section :: enum {
	Importing,
	Exporting,
	Changing,
	Tables,
}

Function_Module_Exception_Data :: struct {
	name:  string,
	range: tokenizer.Range,
}

Class_Inheritance_Data :: struct {
	class_symbol:    Symbol_Id,
	superclass_name: string,
}

Class_Friend_Data :: struct {
	class_symbol: Symbol_Id,
	friend_name:  string,
	range:        tokenizer.Range,
}

Class_Definition_Data :: struct {
	class_symbol: Symbol_Id,
	is_abstract:  bool,
}

Implemented_Interface_Data :: struct {
	owner_symbol:   Symbol_Id,
	interface_name: string,
	range:          tokenizer.Range,
}

Member_Alias_Data :: struct {
	symbol:                Symbol_Id,
	owner_symbol:          Symbol_Id,
	alias_name:            string,
	target_interface_name: string,
	target_member_name:    string,
	range:                 tokenizer.Range,
}

push_decl_info :: proc(
	decl_infos: ^[dynamic]Decl_Info_Data,
	entity: Entity_Id,
	scope: Scope_Id,
	name: string,
	kind: Symbol_Kind,
	decl_range: tokenizer.Range,
	clause_kind := ast.Decl_Clause_Kind.Normal,
	clause_flags := ast.Decl_Clause_Flags{},
	type_clause: ^ast.Data_Type_Clause = nil,
	value_clause: ^ast.Value_Clause = nil,
	default_clause: ^ast.Default_Clause = nil,
) -> Decl_Info_Id {
	id := Decl_Info_Id(u32(len(decl_infos^)))
	append(
		decl_infos,
		Decl_Info_Data {
			id = id,
			entity = entity,
			owner = INVALID_SYMBOL_ID,
			scope = scope,
			signature_scope = INVALID_SCOPE_ID,
			body_scope = INVALID_SCOPE_ID,
			name = name,
			kind = kind,
			decl_range = decl_range,
			name_range = decl_range,
			clause_kind = clause_kind,
			clause_flags = clause_flags,
			type_clause = type_clause,
			value_clause = value_clause,
			default_clause = default_clause,
			implementation_unit = INVALID_SOURCE_FILE_ID,
			effective_signature = Symbol_Link{unit = INVALID_SOURCE_FILE_ID, symbol = INVALID_SYMBOL_ID},
		},
	)
	return id
}

decl_info :: proc(unit: ^Source_File_Provider, id: Decl_Info_Id) -> ^Decl_Info_Data {
	if id == INVALID_DECL_INFO_ID || decl_info_id_index(id) >= len(unit.decl_infos) {
		return nil
	}
	return &unit.decl_infos[decl_info_id_index(id)]
}

entity_decl_info :: proc(unit: ^Source_File_Provider, id: Entity_Id) -> ^Decl_Info_Data {
	s := symbol(unit, id)
	if s == nil {
		return nil
	}
	return decl_info(unit, s.decl_info)
}

entity_signature_parameter :: proc(
	unit: ^Source_File_Provider,
	owner: Entity_Id,
	name: string,
) -> ^Decl_Signature_Parameter_Data {
	info := entity_decl_info(unit, owner)
	if info == nil {
		return nil
	}
	for &param in info.signature_parameters {
		if strings.equal_fold(param.name, name) {
			return &param
		}
	}
	return nil
}

entity_signature_parameter_symbol :: proc(
	unit: ^Source_File_Provider,
	owner: Entity_Id,
	name: string,
) -> (Symbol_Id, bool) {
	info := entity_decl_info(unit, owner)
	if info == nil {
		return INVALID_SYMBOL_ID, false
	}
	if info.signature_scope != INVALID_SCOPE_ID {
		if symbol_id, ok := scope_lookup_declaration(unit, info.signature_scope, .Value, name); ok {
			return symbol_id, true
		}
	}
	for param in info.signature_parameters {
		if param.name == name && param.symbol != INVALID_SYMBOL_ID {
			return param.symbol, true
		}
	}
	return INVALID_SYMBOL_ID, false
}
