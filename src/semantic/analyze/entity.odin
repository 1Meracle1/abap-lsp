package abap_frontend_semantic_analyze

import "src:ast"
import "src:tokenizer"

import "core:mem"
import "core:strings"

Symbol_Kind :: enum {
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

Visibility :: enum {
	Public,
	Protected,
	Private,
}

Class_Member_Kind :: enum {
	Attribute,
	Method,
	Event,
}

symbol_kind_is_builtin :: #force_inline proc(kind: Symbol_Kind) -> bool {
	return(
		kind == .Builtin_Type ||
		kind == .Builtin_Routine ||
		kind == .Builtin_Constant ||
		kind == .Builtin_Variable \
	)
}

symbol_kind_occupies :: proc(kind: Symbol_Kind, namespace: Namespace) -> bool {
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
	}
	return false
}

// Symbol_Data is the current ABAP Entity. Keep the core Entity fields first;
// the trailing declaration/type-shape payload is transitional checker data.
Symbol_Data :: struct {
	id:                       Symbol_Id,
	kind:                     Symbol_Kind,
	name:                     string,
	owner:                    Entity_Id,
	scope:                    Scope_Id,
	type_id:                  Type_Id,
	node:                     ^ast.Node,
	decl_info:                Decl_Info_Id,
	decl_range:               tokenizer.Range,
	structure:                Structure_Id,
	declared_type:            Field_Type_Ref_Data,
	type_clause_display:      string,
	value_clause_display:     string,
	type_clause_form:         ast.Data_Type_Form,
	has_declared_type:        bool,
	has_type_clause_form:     bool,
	type_clause_table_has_of: bool,
}

symbol_set_structure :: proc(s: ^Symbol_Data, structure: Structure_Id) {
	if s != nil {
		s.structure = structure
	}
}

symbol_set_type_id :: proc(s: ^Symbol_Data, type_id: Type_Id) {
	if s != nil {
		s.type_id = type_id
	}
}

symbol_refresh_type_id :: proc(unit: ^Source_File_Provider, s: ^Symbol_Data) -> Type_Id {
	if s == nil {
		return UNKNOWN_TYPE_ID
	}
	s.type_id = type_id_from_symbol_data(unit, s)
	return s.type_id
}

symbol_set_value_clause_display :: proc(s: ^Symbol_Data, value_clause_display: string) {
	if s != nil {
		s.value_clause_display = value_clause_display
	}
}

symbol_set_declared_type :: proc(
	s: ^Symbol_Data,
	declared_type: Field_Type_Ref_Data,
	type_clause_display := "",
	type_clause_form := ast.Data_Type_Form{},
	has_type_clause_form := false,
	type_clause_table_has_of := false,
) {
	if s == nil {
		return
	}
	s.declared_type = declared_type
	s.has_declared_type = true
	s.type_clause_display = type_clause_display
	s.type_clause_form = type_clause_form
	s.has_type_clause_form = has_type_clause_form
	s.type_clause_table_has_of = type_clause_table_has_of
}

symbol_clear_type_shape :: proc(s: ^Symbol_Data) {
	if s == nil {
		return
	}
	s.structure = INVALID_STRUCTURE_ID
	s.declared_type = {}
	s.has_declared_type = false
	s.type_clause_display = ""
	s.type_clause_form = {}
	s.has_type_clause_form = false
	s.type_clause_table_has_of = false
	s.type_id = UNKNOWN_TYPE_ID
}

symbol_merge_collected_shape :: proc(
	s: ^Symbol_Data,
	structure := INVALID_STRUCTURE_ID,
	declared_type := Field_Type_Ref_Data{},
	has_declared_type := false,
	type_clause_display := "",
	value_clause_display := "",
	type_clause_form := ast.Data_Type_Form{},
	has_type_clause_form := false,
	type_clause_table_has_of := false,
	type_id := UNKNOWN_TYPE_ID,
	allocator: mem.Allocator,
) {
	if s == nil {
		return
	}
	if structure != INVALID_STRUCTURE_ID {
		s.structure = structure
	}
	if has_declared_type {
		s.declared_type = declared_type
		s.has_declared_type = true
	}
	if type_id_is_known(type_id) {
		s.type_id = type_id
	}
	if type_clause_display != "" {
		s.type_clause_display = strings.clone(type_clause_display, allocator)
	}
	if value_clause_display != "" {
		s.value_clause_display = strings.clone(value_clause_display, allocator)
	}
	if has_type_clause_form {
		s.type_clause_form = type_clause_form
		s.has_type_clause_form = true
	}
	if type_clause_table_has_of {
		s.type_clause_table_has_of = true
	}
}

symbol_type_shape_differs :: proc(
	s: ^Symbol_Data,
	structure: Structure_Id,
	fact: Type_Fact_Data,
) -> bool {
	return s == nil ||
	       s.structure != structure ||
	       s.has_declared_type != fact.has_declared_type ||
	       !field_type_refs_equal(s.declared_type, fact.declared_type)
}

symbol_apply_inferred_type_fact :: proc(
	unit: ^Source_File_Provider,
	s: ^Symbol_Data,
	structure: Structure_Id,
	fact: Type_Fact_Data,
) {
	if s == nil {
		return
	}
	s.structure = structure
	if fact.has_declared_type {
		symbol_set_declared_type(s, fact.declared_type, fact.type_clause_display)
	}
	symbol_refresh_type_id(unit, s)
}

entity_set_owner :: proc(unit: ^Source_File_Provider, id: Entity_Id, owner: Entity_Id) {
	if s := symbol(unit, id); s != nil {
		s.owner = owner
	}
	if info := entity_decl_info(unit, id); info != nil {
		info.owner = owner
	}
}

entity_set_signature :: proc(
	unit: ^Source_File_Provider,
	id: Entity_Id,
	signature: string,
	allocator: mem.Allocator,
) {
	if info := entity_decl_info(unit, id); info != nil && signature != "" {
		info.signature = strings.clone(signature, allocator)
	}
}

entity_set_signature_scope :: proc(unit: ^Source_File_Provider, id: Entity_Id, scope: Scope_Id) {
	if info := entity_decl_info(unit, id); info != nil {
		info.signature_scope = scope
	}
}

entity_set_body_scope :: proc(unit: ^Source_File_Provider, id: Entity_Id, scope: Scope_Id) {
	if info := entity_decl_info(unit, id); info != nil {
		info.body_scope = scope
	}
}

entity_set_member_decl_info :: proc(
	unit: ^Source_File_Provider,
	id: Entity_Id,
	class_symbol: Entity_Id,
	visibility: Visibility,
	kind: Class_Member_Kind,
	flags: Decl_Info_Flags,
) {
	entity_set_owner(unit, id, class_symbol)
	if info := entity_decl_info(unit, id); info != nil {
		info.visibility = visibility
		info.member_kind = kind
		info.flags += flags
	}
}

entity_set_parameter_decl_info :: proc(
	unit: ^Source_File_Provider,
	id: Entity_Id,
	owner: Entity_Id,
	section: Decl_Parameter_Section,
	passing: Decl_Parameter_Passing,
	flags := Decl_Info_Flags{},
) {
	entity_set_owner(unit, id, owner)
	if info := entity_decl_info(unit, id); info != nil {
		info.parameter_section = section
		info.parameter_passing = passing
		info.flags += flags
	}
}

entity_set_event_signature :: proc(
	unit: ^Source_File_Provider,
	id: Entity_Id,
	name: string,
	range: tokenizer.Range,
	source_type: Field_Type_Ref_Data,
	allocator: mem.Allocator,
) {
	if info := entity_decl_info(unit, id); info != nil {
		info.event_name = strings.clone(name, allocator)
		info.event_range = range
		info.event_source_type = source_type
	}
}

declare_symbol :: proc(
	unit: ^Source_File_Provider,
	scope: Scope_Id,
	name: string,
	kind: Symbol_Kind,
	decl_range: tokenizer.Range,
	structure := INVALID_STRUCTURE_ID,
	declared_type := Field_Type_Ref_Data{},
	has_declared_type := false,
	type_clause_display := "",
	value_clause_display := "",
	type_clause_form := ast.Data_Type_Form{},
	has_type_clause_form := false,
	type_clause_table_has_of := false,
	type_id := UNKNOWN_TYPE_ID,
	owner := INVALID_SYMBOL_ID,
	node: ^ast.Node = nil,
	decl_clause_kind := ast.Decl_Clause_Kind.Normal,
	decl_clause_flags := ast.Decl_Clause_Flags{},
	decl_type_clause: ^ast.Data_Type_Clause = nil,
	decl_value_clause: ^ast.Value_Clause = nil,
	decl_default_clause: ^ast.Default_Clause = nil,
	resolve_type_id := true,
) -> Symbol_Id {
	scope_index := scope_id_index(scope)
	assert(scope_index >= 0 && scope_index < len(unit.scopes))
	id := Symbol_Id(u32(len(unit.symbols)))
	decl_info := INVALID_DECL_INFO_ID
	if !symbol_kind_is_builtin(kind) {
		decl_info = push_decl_info(
			&unit.decl_infos,
			id,
			scope,
			name,
			kind,
			decl_range,
			decl_clause_kind,
			decl_clause_flags,
			decl_type_clause,
			decl_value_clause,
			decl_default_clause,
		)
	}
	resolved_type_id := type_id
	if resolve_type_id && !type_id_is_known(resolved_type_id) {
		resolved_type_id = type_id_from_symbol_fields(
			unit,
			id,
			scope,
			name,
			kind,
			structure,
			declared_type,
			has_declared_type,
			type_clause_form,
			has_type_clause_form,
		)
	}
	append(
		&unit.symbols,
		Symbol_Data {
			id = id,
			name = name,
			kind = kind,
			owner = owner,
			scope = scope,
			node = node,
			decl_info = decl_info,
			type_id = resolved_type_id,
			decl_range = decl_range,
			structure = structure,
			declared_type = declared_type,
			has_declared_type = has_declared_type,
			type_clause_display = type_clause_display,
			value_clause_display = value_clause_display,
			type_clause_form = type_clause_form,
			has_type_clause_form = has_type_clause_form,
			type_clause_table_has_of = type_clause_table_has_of,
		},
	)
	scope_record_declaration(unit, scope, id)
	if owner != INVALID_SYMBOL_ID {
		entity_set_owner(unit, id, owner)
	}
	return id
}

symbol :: proc(unit: ^Source_File_Provider, id: Symbol_Id) -> ^Symbol_Data {
	if id == INVALID_SYMBOL_ID {
		return nil
	}
	if index, builtin := builtin_symbol_index(id); builtin {
		provider := shared_builtin_provider()
		if index >= 0 && index < len(provider.symbols) {
			return &provider.symbols[index]
		}
		return nil
	}
	if symbol_id_index(id) >= len(unit.symbols) {
		return nil
	}
	return &unit.symbols[symbol_id_index(id)]
}

find_symbol :: proc(unit: ^Source_File_Provider, name: string, kind: Symbol_Kind) -> ^Symbol_Data {
	for &s in unit.symbols {
		if s.kind == kind && strings.equal_fold(s.name, name) {
			return &s
		}
	}
	if !builtin_provider_is_shared(unit) {
		builtin := shared_builtin_provider()
		for &s in builtin.symbols {
			if s.kind == kind && strings.equal_fold(s.name, name) {
				return &s
			}
		}
	}
	return nil
}

class_definition_scope :: proc(unit: ^Source_File_Provider, class_symbol: Symbol_Id) -> Scope_Id {
	info := entity_decl_info(unit, class_symbol)
	if info == nil || info.body_scope == INVALID_SCOPE_ID {
		return INVALID_SCOPE_ID
	}
	scope_data := scope(unit, info.body_scope)
	if scope_data == nil ||
	   !(scope_data.kind == .Class || scope_data.kind == .Interface) ||
	   scope_data.owner != class_symbol {
		return INVALID_SCOPE_ID
	}
	return info.body_scope
}

class_definition_member :: proc(
	unit: ^Source_File_Provider,
	class_symbol: Symbol_Id,
	namespace: Namespace,
	name: string,
) -> (Symbol_Id, bool) {
	canonical := strings.to_lower(name, context.temp_allocator)
	return class_definition_member_canonical(unit, class_symbol, namespace, canonical)
}

class_definition_member_canonical :: proc(
	unit: ^Source_File_Provider,
	class_symbol: Symbol_Id,
	namespace: Namespace,
	name: string,
) -> (Symbol_Id, bool) {
	scope_id := class_definition_scope(unit, class_symbol)
	if scope_id == INVALID_SCOPE_ID {
		return INVALID_SYMBOL_ID, false
	}
	return scope_lookup_declaration(unit, scope_id, namespace, name)
}

unit_class_member_symbol :: proc(unit: ^Source_File_Provider, class_symbol: Symbol_Id, name: string) -> ^Symbol_Data {
	canonical := strings.to_lower(name, context.temp_allocator)
	return unit_class_member_symbol_canonical(unit, class_symbol, canonical)
}

unit_class_member_symbol_canonical :: proc(unit: ^Source_File_Provider, class_symbol: Symbol_Id, name: string) -> ^Symbol_Data {
	namespaces := [?]Namespace{.Value, .Routine, .Type}
	for namespace in namespaces {
		if id, ok := class_definition_member_canonical(unit, class_symbol, namespace, name); ok {
			return symbol(unit, id)
		}
	}
	return nil
}
