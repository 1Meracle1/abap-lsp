package abap_frontend_semantic_analyze

import "src:ast"
import execution "src:execution"
import "src:parser"

import "core:mem"
import "core:strings"

build_scope_index :: proc(unit: ^Unit_Analysis, allocator: mem.Allocator) -> Scope_Index {
	index := Scope_Index {
		class_symbols     = make(map[Class_Scope_Index_Key]Symbol_Id, len(unit.symbols), allocator),
		enclosing_classes = make([dynamic]Symbol_Id, len(unit.scopes), len(unit.scopes), allocator),
		superclasses      = make(map[Symbol_Id]string, len(unit.class_inheritance), allocator),
	}
	for scope_data, i in unit.scopes {
		owner := INVALID_SYMBOL_ID
		if (scope_data.kind == .Class || scope_data.kind == .Interface) &&
		   scope_data.owner != INVALID_SYMBOL_ID {
			owner = scope_data.owner
		} else {
			parent_index := scope_id_index(scope_data.parent)
			if parent_index >= 0 && parent_index < i {
				owner = index.enclosing_classes[parent_index]
			}
		}
		index.enclosing_classes[i] = owner
	}
	for inheritance in unit.class_inheritance {
		index.superclasses[inheritance.class_symbol] = inheritance.superclass_name
	}
	for symbol in unit.symbols {
		class_symbol := INVALID_SYMBOL_ID
		if scope_data := scope(unit, symbol.scope);
		   scope_data != nil &&
		   (scope_data.kind == .Class || scope_data.kind == .Interface) &&
		   scope_data.owner != INVALID_SYMBOL_ID {
			class_symbol = scope_data.owner
		}
		namespaces := [?]Namespace{.Value, .Type, .Routine}
		for namespace in namespaces {
			if symbol_kind_occupies(symbol.kind, namespace) {
				if class_symbol != INVALID_SYMBOL_ID {
					index.class_symbols[Class_Scope_Index_Key{class_symbol = class_symbol, namespace = namespace, name = symbol.name}] =
						symbol.id
				}
			}
		}
	}
	return index
}

analyze_unit :: proc(
	unit_id: Unit_Id,
	uri, source: string,
	parsed: parser.Parsed_File,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) -> Unit_Analysis {
	unit := collect_unit(unit_id, uri, source, parsed, allocator)
	resolve_unit_locally(&unit, allocator)
	units := make([dynamic]Unit_Analysis, 0, 1, allocator)
	append(&units, unit)
	project := project_analysis_from_units(units, allocator)
	finish_project_analysis(&project, pool, {}, allocator)
	return project.units[0]
}

resolve_unit_locally :: proc(unit: ^Unit_Analysis, allocator: mem.Allocator) {
	index := build_scope_index(unit, allocator)
	unit.scope_index = index
	refresh_unit_type_ids(unit)
	if expand_local_structure_includes(unit, allocator) {
		refresh_unit_type_ids(unit)
	}
	resolve_unit_with_index(unit, &unit.scope_index)
}

resolve_unit_with_index :: proc(unit: ^Unit_Analysis, index: ^Scope_Index) {
	for i in 0 ..< len(unit.references) {
		ref := &unit.references[i]
		if resolution, ok := resolve_reference(unit, index, ref^); ok {
			set_reference_resolution(unit, ref, resolution)
		}
	}
}

set_reference_resolution :: proc(
	unit: ^Unit_Analysis,
	ref: ^Reference_Data,
	resolution: Resolution,
) {
	assert(reference_resolution_allowed(unit, ref^, resolution))
	ref.resolution = resolution
	ref.has_resolution = true
}

set_project_reference_resolution :: proc(
	units: []Unit_Analysis,
	ref: ^Reference_Data,
	resolution: Resolution,
) {
	assert(project_reference_resolution_allowed(units, ref^, resolution))
	ref.resolution = resolution
	ref.has_resolution = true
}

reference_resolution_allowed :: proc(
	unit: ^Unit_Analysis,
	ref: Reference_Data,
	resolution: Resolution,
) -> bool {
	if resolution.kind != .Symbol {
		return true
	}
	if resolution.symbol.unit != unit.unit_id {
		return true
	}
	s := symbol(unit, resolution.symbol.symbol)
	if s == nil {
		return false
	}
	return reference_symbol_kind_allowed(ref, s.kind)
}

project_reference_resolution_allowed :: proc(
	units: []Unit_Analysis,
	ref: Reference_Data,
	resolution: Resolution,
) -> bool {
	if resolution.kind != .Symbol {
		return true
	}
	unit_index := unit_id_index(resolution.symbol.unit)
	if unit_index < 0 || unit_index >= len(units) {
		return false
	}
	s := symbol(&units[unit_index], resolution.symbol.symbol)
	if s == nil {
		return false
	}
	return reference_symbol_kind_allowed(ref, s.kind)
}

reference_symbol_kind_allowed :: proc(ref: Reference_Data, kind: Symbol_Kind) -> bool {
	if ref.namespace == .Value &&
	   ref.kind == .Identifier &&
	   (ref.name == "me" || ref.name == "super") &&
	   (kind == .Class || kind == .Interface) {
		return true
	}
	namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in namespaces {
		if symbol_kind_occupies(kind, namespace) &&
		   reference_namespace_allowed(ref.kind, ref.namespace, namespace) {
			return true
		}
	}
	return false
}

resolve_reference :: proc(
	unit: ^Unit_Analysis,
	index: ^Scope_Index,
	ref: Reference_Data,
) -> (
	Resolution,
	bool,
) {
	if symbol_id, ok := lookup_reference_scope_chain(
		unit,
		index,
		ref.scope,
		ref.namespace,
		ref.kind,
		ref.name,
	); ok {
		return resolution_for_symbol(unit, symbol_id), true
	}
	if symbol_id, ok := resolve_current_class_member(
		unit,
		index,
		ref.scope,
		ref.namespace,
		ref.name,
	); ok {
		return symbol_resolution(unit, symbol_id), true
	}
	if symbol_id, ok := resolve_current_class_alias(
		unit,
		index,
		ref.scope,
		ref.namespace,
		ref.name,
	); ok {
		return symbol_resolution(unit, symbol_id), true
	}
	if symbol_id, ok := resolve_inherited_class_member(
		unit,
		index,
		ref.scope,
		ref.namespace,
		ref.name,
	); ok {
		return symbol_resolution(unit, symbol_id), true
	}
	if ref.namespace == .Value && ref.name == "super" {
		if symbol_id, ok := resolve_super_reference(unit, index, ref.scope); ok {
			return symbol_resolution(unit, symbol_id), true
		}
	}
	if ref.namespace == .Type && is_builtin_type_name(ref.name) {
		return Resolution{kind = .Builtin_Type}, true
	}
	if ref.namespace == .Routine && builtin_routine_spec(ref.name) != nil {
		return Resolution{kind = .Builtin_Routine}, true
	}
	if ref.namespace == .Value &&
	   ref.kind == .Identifier &&
	   ref.name == "table_line" &&
	   innermost_loop_allows_internal_table_line_selector(unit, ref.scope) {
		return Resolution{kind = .Internal_Table_Line}, true
	}
	return Resolution{}, false
}

lookup_scope_chain :: proc(
	unit: ^Unit_Analysis,
	_: ^Scope_Index,
	start_scope: Scope_Id,
	namespace: Namespace,
	name: string,
) -> (
	Symbol_Id,
	bool,
) {
	current := start_scope
	for current != INVALID_SCOPE_ID {
		scope_idx := scope_id_index(current)
		if scope_idx >= 0 && scope_idx < len(unit.scopes) {
			if symbol_id, ok := scope_lookup_declaration(unit, current, namespace, name); ok {
				return symbol_id, true
			}
		}
		s := scope(unit, current)
		if s == nil {
			break
		}
		current = s.parent
	}
	return INVALID_SYMBOL_ID, false
}

lookup_reference_scope_chain :: proc(
	unit: ^Unit_Analysis,
	index: ^Scope_Index,
	scope: Scope_Id,
	namespace: Namespace,
	kind: Reference_Kind,
	name: string,
) -> (
	Symbol_Id,
	bool,
) {
	if symbol_id, ok := lookup_scope_chain(unit, index, scope, namespace, name); ok {
		return symbol_id, true
	}
	if kind == .Type_Ref && namespace == .Value {
		return lookup_scope_chain(unit, index, scope, .Type, name)
	}
	return INVALID_SYMBOL_ID, false
}

resolution_for_symbol :: proc(unit: ^Unit_Analysis, symbol_id: Symbol_Id) -> Resolution {
	s := symbol(unit, symbol_id)
	if s != nil {
		if s.kind == .Builtin_Type {
			return Resolution{kind = .Builtin_Type}
		}
		if s.kind == .Builtin_Routine {
			return Resolution{kind = .Builtin_Routine}
		}
	}
	return symbol_resolution(unit, symbol_id)
}

symbol_resolution :: #force_inline proc(unit: ^Unit_Analysis, symbol_id: Symbol_Id) -> Resolution {
	return Resolution {
		kind = .Symbol,
		symbol = Symbol_Handle{unit = unit.unit_id, symbol = symbol_id},
	}
}

enclosing_class_owner_unit :: proc(unit: ^Unit_Analysis, scope_id: Scope_Id) -> (Symbol_Id, bool) {
	scope_index := scope_id_index(scope_id)
	if scope_index >= 0 && scope_index < len(unit.scope_index.enclosing_classes) {
		owner := unit.scope_index.enclosing_classes[scope_index]
		return owner, owner != INVALID_SYMBOL_ID
	}
	current := scope_id
	for current != INVALID_SCOPE_ID {
		s := scope(unit, current)
		if s == nil {
			break
		}
		if (s.kind == .Class || s.kind == .Interface) && s.owner != INVALID_SYMBOL_ID {
			return s.owner, true
		}
		current = s.parent
	}
	return INVALID_SYMBOL_ID, false
}

enclosing_instance_method_class_owner_unit :: proc(
	unit: ^Unit_Analysis,
	scope_id: Scope_Id,
) -> (
	Symbol_Id,
	bool,
) {
	current := scope_id
	for current != INVALID_SCOPE_ID {
		s := scope(unit, current)
		if s == nil {
			break
		}
		if s.kind == .Method && s.owner != INVALID_SYMBOL_ID {
			class_symbol, class_ok := enclosing_class_owner_unit(unit, current)
			method := symbol(unit, s.owner)
			if !class_ok || method == nil {
				return INVALID_SYMBOL_ID, false
			}
			member := unit_class_member_symbol(unit, class_symbol, method.name)
			info := entity_decl_info(unit, member.id) if member != nil else nil
			return class_symbol, info == nil || !(.Is_Static in info.flags)
		}
		current = s.parent
	}
	return INVALID_SYMBOL_ID, false
}

resolve_current_class_member :: proc(
	unit: ^Unit_Analysis,
	index: ^Scope_Index,
	scope_id: Scope_Id,
	namespace: Namespace,
	name: string,
) -> (
	Symbol_Id,
	bool,
) {
	class_symbol, ok := enclosing_class_owner_unit(unit, scope_id)
	if !ok {
		return INVALID_SYMBOL_ID, false
	}
	return class_scope_symbol(index, class_symbol, namespace, name)
}

class_scope_symbol :: proc(
	index: ^Scope_Index,
	class_symbol: Symbol_Id,
	namespace: Namespace,
	name: string,
) -> (
	Symbol_Id,
	bool,
) {
	if symbol_id, ok :=
		   index.class_symbols[Class_Scope_Index_Key{class_symbol = class_symbol, namespace = namespace, name = name}];
	   ok {
		return symbol_id, true
	}
	return INVALID_SYMBOL_ID, false
}

resolve_current_class_alias :: proc(
	unit: ^Unit_Analysis,
	index: ^Scope_Index,
	scope_id: Scope_Id,
	namespace: Namespace,
	name: string,
) -> (
	Symbol_Id,
	bool,
) {
	class_symbol, ok := enclosing_class_owner_unit(unit, scope_id)
	if !ok {
		return INVALID_SYMBOL_ID, false
	}
	return resolve_class_alias(unit, index, scope_id, class_symbol, namespace, name)
}

resolve_class_alias :: proc(
	unit: ^Unit_Analysis,
	index: ^Scope_Index,
	scope_id: Scope_Id,
	class_symbol: Symbol_Id,
	namespace: Namespace,
	name: string,
) -> (
	Symbol_Id,
	bool,
) {
	for alias in unit.member_aliases {
		if alias.owner_symbol != class_symbol || alias.alias_name != name {
			continue
		}
		interface_symbol, interface_ok := lookup_scope_chain(
			unit,
			index,
			scope_id,
			.Type,
			alias.target_interface_name,
		)
		if !interface_ok {
			continue
		}
		member_name := alias.target_member_name
		if member_name == "" {
			member_name = name
		}
		if symbol_id, member_ok := class_scope_symbol(
			index,
			interface_symbol,
			namespace,
			member_name,
		); member_ok {
			return symbol_id, true
		}
	}
	return INVALID_SYMBOL_ID, false
}

resolve_inherited_class_member :: proc(
	unit: ^Unit_Analysis,
	index: ^Scope_Index,
	scope_id: Scope_Id,
	namespace: Namespace,
	name: string,
) -> (
	Symbol_Id,
	bool,
) {
	current_class, ok := enclosing_class_owner_unit(unit, scope_id)
	if !ok {
		return INVALID_SYMBOL_ID, false
	}
	for _ in 0 ..= len(unit.class_inheritance) {
		super_name, has_super := class_superclass_name(unit, current_class)
		if !has_super {
			return INVALID_SYMBOL_ID, false
		}
		super_symbol, super_ok := lookup_scope_chain(unit, index, scope_id, .Type, super_name)
		if !super_ok {
			return INVALID_SYMBOL_ID, false
		}
		if found, found_ok := class_scope_symbol(index, super_symbol, namespace, name); found_ok {
			return found, true
		}
		if found, found_ok := resolve_class_alias(
			unit,
			index,
			scope_id,
			super_symbol,
			namespace,
			name,
		); found_ok {
			return found, true
		}
		current_class = super_symbol
	}
	return INVALID_SYMBOL_ID, false
}

resolve_super_reference :: proc(
	unit: ^Unit_Analysis,
	index: ^Scope_Index,
	scope_id: Scope_Id,
) -> (
	Symbol_Id,
	bool,
) {
	class_symbol, ok := enclosing_instance_method_class_owner_unit(unit, scope_id)
	if !ok {
		return INVALID_SYMBOL_ID, false
	}
	super_name, has_super := class_superclass_name(unit, class_symbol)
	if !has_super {
		return INVALID_SYMBOL_ID, false
	}
	return lookup_scope_chain(unit, index, scope_id, .Type, super_name)
}

class_superclass_name :: proc(unit: ^Unit_Analysis, class_symbol: Symbol_Id) -> (string, bool) {
	if unit.scope_index.superclasses != nil {
		if name, ok := unit.scope_index.superclasses[class_symbol]; ok {
			return name, name != ""
		}
	}
	for inheritance in unit.class_inheritance {
		if inheritance.class_symbol == class_symbol {
			return inheritance.superclass_name, inheritance.superclass_name != ""
		}
	}
	return "", false
}

innermost_loop_allows_internal_table_line_selector :: proc(
	unit: ^Unit_Analysis,
	scope_id: Scope_Id,
) -> bool {
	current := scope_id
	for current != INVALID_SCOPE_ID {
		s := scope(unit, current)
		if s == nil {
			return false
		}
		if s.kind == .Loop_Block {
			return s.allows_internal_table_line_selector
		}
		current = s.parent
	}
	return false
}

Root_Symbol_Entry :: struct {
	unit:               Unit_Id,
	symbol:             Symbol_Id,
	namespace:          Namespace,
	name:               string,
	visible_by_default: bool,
}

Project_Root_Lookup :: struct {
	by_unit:        map[Root_Symbol_Key]Symbol_Handle,
	global:         map[Root_Name_Key]Symbol_Handle,
	provided_names: map[string]bool,
}

Project_Class_Member_Key :: struct {
	class_unit:   Unit_Id,
	class_symbol: Symbol_Id,
	namespace:    Namespace,
	name:         string,
}

Project_Class_Member_Entry :: struct {
	unit:   Unit_Id,
	symbol: Symbol_Id,
}

seed_event_handler_method_parameter_types :: proc(
	units: []Unit_Analysis,
	unit_index: int,
	method_scope: Scope_Id,
	member_unit_index: int,
	member_handle: Symbol_Handle,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [][dynamic]Unit_Id,
) -> bool {
	if member_unit_index < 0 ||
	   member_unit_index >= len(units) {
		return false
	}
	member_info := entity_decl_info(&units[member_unit_index], member_handle.symbol)
	if member_info == nil ||
	   member_info.event_name == "" ||
	   member_info.event_source_type.base_name == "" {
		return false
	}
	source_handle, source_ok := resolve_type_ref_handle_project(
		units,
		member_unit_index,
		member_info.event_source_type,
		roots,
		visible[member_unit_index],
	)
	if !source_ok {
		return false
	}
	event_member, _ := event_member_for_handler_source(
		units,
		source_handle,
		member_info.event_name,
		class_entries,
	)
	if event_member.symbol == INVALID_SYMBOL_ID {
		return false
	}
	event_info := entity_decl_info(&units[unit_id_index(event_member.unit)], event_member.symbol)
	if event_info == nil {
		return false
	}
	changed := false
	for &param in member_info.signature_parameters {
		event_param := class_member_parameter(event_info, param.name)
		if .Has_Event_Derived_Type in param.flags {
			if event_param != nil &&
			   .Has_Declared_Type in event_param.flags &&
			   event_derived_parameter_matches(param, event_param^) {
				continue
			}
			clear_event_derived_signature_parameter(&units[member_unit_index], &param)
			changed = true
			changed =
				clear_event_derived_method_scope_parameter(&units[unit_index], method_scope, param.name) ||
				changed
		}
		if .Has_Declared_Type in param.flags {
			continue
		}
		if event_param == nil || !(.Has_Declared_Type in event_param.flags) {
			continue
		}
		if decl_param := entity_signature_parameter(&units[member_unit_index], member_handle.symbol, param.name);
		   decl_param != nil {
			decl_param.declared_type = event_param.declared_type
			decl_param.type_clause_display = event_param.type_clause_display
			decl_param.type_clause_form = event_param.type_clause_form
			decl_param.has_type_clause_form = event_param.has_type_clause_form
			decl_param.type_clause_table_has_of = event_param.type_clause_table_has_of
			decl_param.type_id = UNKNOWN_TYPE_ID
			decl_param.flags += {.Has_Declared_Type, .Has_Event_Derived_Type}
			update_parameter_symbol_from_signature(&units[member_unit_index], decl_param.symbol, event_param^)
		}
		changed =
			update_method_scope_parameter_symbol(
				&units[unit_index],
				method_scope,
				param.name,
				event_param^,
				INVALID_STRUCTURE_ID,
			) ||
			changed
	}
	return changed
}

event_derived_parameter_matches :: proc(
	param: Decl_Signature_Parameter_Data,
	event_param: Decl_Signature_Parameter_Data,
) -> bool {
	return field_type_refs_equal(param.declared_type, event_param.declared_type) &&
	       param.type_clause_display == event_param.type_clause_display &&
	       param.type_clause_form == event_param.type_clause_form &&
	       param.has_type_clause_form == event_param.has_type_clause_form &&
	       param.type_clause_table_has_of == event_param.type_clause_table_has_of
}

clear_event_derived_signature_parameter :: proc(
	unit: ^Unit_Analysis,
	param: ^Decl_Signature_Parameter_Data,
) {
	param.declared_type = {}
	param.type_clause_display = ""
	param.type_clause_form = {}
	param.has_type_clause_form = false
	param.type_clause_table_has_of = false
	param.type_id = UNKNOWN_TYPE_ID
	param.flags -= {.Has_Declared_Type, .Has_Event_Derived_Type}
	clear_event_derived_parameter_symbol(unit, param.symbol)
}

clear_event_derived_method_scope_parameter :: proc(
	unit: ^Unit_Analysis,
	scope_id: Scope_Id,
	name: string,
) -> bool {
	s := scope(unit, scope_id)
	if s == nil {
		return false
	}
	for symbol_id in s.declarations {
		item := symbol(unit, symbol_id)
		if item == nil || item.name != name || item.kind != .Parameter {
			continue
		}
		return clear_event_derived_parameter_symbol(unit, symbol_id)
	}
	return false
}

clear_event_derived_parameter_symbol :: proc(unit: ^Unit_Analysis, symbol_id: Symbol_Id) -> bool {
	info := entity_decl_info(unit, symbol_id)
	if info == nil || !(.Has_Event_Derived_Type in info.flags) {
		return false
	}
	item := symbol(unit, symbol_id)
	if item != nil {
		item.structure = INVALID_STRUCTURE_ID
		item.declared_type = {}
		item.has_declared_type = false
		item.type_clause_display = ""
		item.type_clause_form = {}
		item.has_type_clause_form = false
		item.type_clause_table_has_of = false
		item.type_id = UNKNOWN_TYPE_ID
	}
	info.flags -= {.Has_Declared_Type, .Has_Event_Derived_Type}
	return true
}

update_parameter_symbol_from_signature :: proc(
	unit: ^Unit_Analysis,
	symbol_id: Symbol_Id,
	param: Decl_Signature_Parameter_Data,
) {
	item := symbol(unit, symbol_id)
	if item == nil {
		return
	}
	item.declared_type = param.declared_type
	item.has_declared_type = true
	item.type_clause_display = param.type_clause_display
	item.type_clause_form = param.type_clause_form
	item.has_type_clause_form = param.has_type_clause_form
	item.type_clause_table_has_of = param.type_clause_table_has_of
	item.type_id = type_id_from_symbol_data(unit, item)
	if info := entity_decl_info(unit, symbol_id); info != nil {
		info.flags += {.Has_Declared_Type, .Has_Event_Derived_Type}
	}
}

event_member_for_handler_source :: proc(
	units: []Unit_Analysis,
	source_handle: Symbol_Handle,
	event_name: string,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
) -> (
	Symbol_Handle,
	int,
) {
	event_handle, event_ok := class_member_symbol_by_handle(
		units,
		source_handle,
		.Routine,
		event_name,
		class_entries,
		false,
	)
	if !event_ok {
		return Symbol_Handle{unit = INVALID_UNIT_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	event_unit_index := unit_id_index(event_handle.unit)
	if event_unit_index < 0 || event_unit_index >= len(units) {
		return Symbol_Handle{unit = INVALID_UNIT_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	event_symbol := symbol(&units[event_unit_index], event_handle.symbol)
	if event_symbol == nil {
		return Symbol_Handle{unit = INVALID_UNIT_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	event_info := entity_decl_info(&units[event_unit_index], event_symbol.id)
	if event_info == nil || event_info.member_kind != .Event {
		return Symbol_Handle{unit = INVALID_UNIT_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	return event_handle, event_unit_index
}

class_member_parameter :: proc(
	info: ^Decl_Info_Data,
	name: string,
) -> ^Decl_Signature_Parameter_Data {
	for &param in info.signature_parameters {
		if param.name == name {
			return &param
		}
	}
	return nil
}

update_method_scope_parameter_symbol :: proc(
	unit: ^Unit_Analysis,
	scope_id: Scope_Id,
	name: string,
	param: Decl_Signature_Parameter_Data,
	structure_id: Structure_Id,
) -> bool {
	s := scope(unit, scope_id)
	if s == nil {
		return false
	}
	for symbol_id in s.declarations {
		item := symbol(unit, symbol_id)
		if item == nil || item.name != name || !symbol_kind_occupies(item.kind, .Value) {
			continue
		}
		item.structure = structure_id
		item.declared_type = param.declared_type
		item.has_declared_type = true
		item.type_clause_display = param.type_clause_display
		item.type_clause_form = param.type_clause_form
		item.has_type_clause_form = param.has_type_clause_form
		item.type_clause_table_has_of = param.type_clause_table_has_of
		item.type_id = type_id_from_symbol_data(unit, item)
		if info := entity_decl_info(unit, item.id); info != nil {
			info.flags += {.Has_Declared_Type, .Has_Event_Derived_Type}
		}
		return true
	}
	return false
}

method_signature_member_for_scope :: proc(
	units: []Unit_Analysis,
	unit_index: int,
	scope_id: Scope_Id,
	method_name: string,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [dynamic]Unit_Id,
	predecessors: [dynamic]Unit_Id,
) -> (Symbol_Handle, int) {
	if interface_name, member_name, qualified := qualified_method_parts(method_name); qualified {
		if member, member_unit_index := exposed_interface_member_for_scope(
			units,
			unit_index,
			scope_id,
			interface_name,
			member_name,
			roots,
			visible,
		); member.symbol != INVALID_SYMBOL_ID {
			return member, member_unit_index
		}
	}
	member_handle, ok := resolve_visible_class_definition_member(
		units,
		unit_index,
		scope_id,
		.Routine,
		method_name,
		roots,
		class_entries,
		visible,
		predecessors,
	)
	if !ok {
		return Symbol_Handle{unit = INVALID_UNIT_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	member_unit_index := unit_id_index(member_handle.unit)
	if member_unit_index < 0 || member_unit_index >= len(units) {
		return Symbol_Handle{unit = INVALID_UNIT_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	member_unit := &units[member_unit_index]
	member_symbol := symbol(member_unit, member_handle.symbol)
	if member_symbol == nil {
		return Symbol_Handle{unit = INVALID_UNIT_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	class_symbol, class_ok := enclosing_class_owner_unit(member_unit, member_symbol.scope)
	if !class_ok {
		return Symbol_Handle{unit = INVALID_UNIT_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	member_info := entity_decl_info(member_unit, member_handle.symbol)
	if member_info == nil ||
	   len(member_info.signature_parameters) > 0 ||
	   !(.Is_Redefinition in member_info.flags) {
		return member_handle, member_unit_index
	}
	if inherited, inherited_unit_index := inherited_project_class_member(
		units,
		Symbol_Handle{unit = member_handle.unit, symbol = class_symbol},
		method_name,
		roots,
		class_entries,
		visible,
	); inherited.symbol != INVALID_SYMBOL_ID {
		return inherited, inherited_unit_index
	}
	return member_handle, member_unit_index
}

exposed_interface_member_for_scope :: proc(
	units: []Unit_Analysis,
	unit_index: int,
	scope_id: Scope_Id,
	interface_name, member_name: string,
	roots: ^Project_Root_Lookup,
	visible: [dynamic]Unit_Id,
) -> (Symbol_Handle, int) {
	class_symbol, class_ok := enclosing_class_owner_unit(&units[unit_index], scope_id)
	if !class_ok {
		return Symbol_Handle{unit = INVALID_UNIT_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	handle, handle_ok := exposed_interface_handle(
		units,
		Symbol_Handle{unit = units[unit_index].unit_id, symbol = class_symbol},
		interface_name,
		roots,
		visible,
		0,
	)
	if !handle_ok {
		return Symbol_Handle{unit = INVALID_UNIT_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	interface_unit_index := unit_id_index(handle.unit)
	if interface_unit_index < 0 || interface_unit_index >= len(units) {
		return Symbol_Handle{unit = INVALID_UNIT_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	member := unit_class_member_symbol(&units[interface_unit_index], handle.symbol, member_name)
	if member == nil {
		return Symbol_Handle{unit = INVALID_UNIT_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	return Symbol_Handle{unit = handle.unit, symbol = member.id}, interface_unit_index
}

exposed_interface_handle :: proc(
	units: []Unit_Analysis,
	owner: Symbol_Handle,
	interface_name: string,
	roots: ^Project_Root_Lookup,
	visible: [dynamic]Unit_Id,
	depth: int,
) -> (
	Symbol_Handle,
	bool,
) {
	if depth > len(units) + 8 {
		return {}, false
	}
	unit_index := unit_id_index(owner.unit)
	if unit_index < 0 || unit_index >= len(units) {
		return {}, false
	}
	unit := &units[unit_index]
	for implemented in unit.implemented_interfaces {
		if implemented.owner_symbol != owner.symbol {
			continue
		}
		interface_handle, ok := resolve_type_name_in_project(
			units,
			unit_index,
			implemented.interface_name,
			roots,
			visible,
		)
		if !ok {
			continue
		}
		if implemented.interface_name == interface_name {
			return interface_handle, true
		}
		if found, found_ok := exposed_interface_handle(
			units,
			interface_handle,
			interface_name,
			roots,
			visible,
			depth + 1,
		); found_ok {
			return found, true
		}
	}
	if owner_symbol := symbol(unit, owner.symbol);
	   owner_symbol != nil && owner_symbol.kind == .Class {
		if superclass, ok := direct_superclass_handle(units, owner, roots, visible); ok {
			return exposed_interface_handle(
				units,
				superclass,
				interface_name,
				roots,
				visible,
				depth + 1,
			)
		}
	}
	return {}, false
}
inherited_project_class_member :: proc(
	units: []Unit_Analysis,
	class_handle: Symbol_Handle,
	name: string,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [dynamic]Unit_Id,
) -> (Symbol_Handle, int) {
	current := class_handle
	fallback := Symbol_Handle{unit = INVALID_UNIT_ID, symbol = INVALID_SYMBOL_ID}
	fallback_index := -1
	for _ in 0 ..< len(units) + 8 {
		next, ok := direct_superclass_handle(units, current, roots, visible)
		if !ok {
			return fallback, fallback_index
		}
		if member_handle, member_ok := class_member_symbol_by_handle(
			units,
			next,
			.Routine,
			name,
			class_entries,
			true,
		); member_ok {
			member_unit_index := unit_id_index(member_handle.unit)
			if member_unit_index >= 0 && member_unit_index < len(units) {
				member_unit := &units[member_unit_index]
				if s := symbol(member_unit, member_handle.symbol); s != nil {
					if class_symbol, class_ok := enclosing_class_owner_unit(member_unit, s.scope);
					   class_ok {
						if member := unit_class_member_symbol(member_unit, class_symbol, s.name); member != nil {
							info := entity_decl_info(member_unit, member.id)
							if fallback.symbol == INVALID_SYMBOL_ID {
								fallback = Symbol_Handle{unit = member_handle.unit, symbol = member.id}
								fallback_index = member_unit_index
							}
							if info != nil &&
							   len(info.signature_parameters) == 0 &&
							   .Is_Redefinition in info.flags {
								current = Symbol_Handle{unit = member_handle.unit, symbol = class_symbol}
								continue
							}
							return Symbol_Handle{unit = member_handle.unit, symbol = member.id}, member_unit_index
						}
					}
				}
			}
		}
		current = next
	}
	return fallback, fallback_index
}

method_scope_has_value_symbol :: proc(
	unit: ^Unit_Analysis,
	scope_id: Scope_Id,
	name: string,
) -> bool {
	if s := scope(unit, scope_id); s != nil {
		for symbol_id in s.declarations {
			if item := symbol(unit, symbol_id);
			   item != nil && item.name == name && symbol_kind_occupies(item.kind, .Value) {
				return true
			}
		}
	}
	return false
}

resolve_project_reference :: proc(
	units: []Unit_Analysis,
	unit_index: int,
	ref: Reference_Data,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [dynamic]Unit_Id,
	predecessors: [dynamic]Unit_Id,
) -> (
	Resolution,
	bool,
) {
	if ref.namespace == .Value && ref.name == "super" {
		if handle, ok := resolve_project_super(units, unit_index, ref.scope, roots, visible); ok {
			return Resolution{kind = .Symbol, symbol = handle}, true
		}
	}
	all_namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in all_namespaces {
		if !reference_namespace_allowed(ref.kind, ref.namespace, namespace) {
			continue
		}
		if handle, ok := resolve_inherited_project_symbol(
			units,
			unit_index,
			ref.scope,
			namespace,
			ref.name,
			roots,
			class_entries,
			visible,
		); ok {
			return Resolution{kind = .Symbol, symbol = handle}, true
		}
		if handle, ok := resolve_visible_class_definition_member(
			units,
			unit_index,
			ref.scope,
			namespace,
			ref.name,
			roots,
			class_entries,
			visible,
			predecessors,
		); ok {
			return Resolution{kind = .Symbol, symbol = handle}, true
		}
		if handle, ok := root_symbol_in_visible_units(namespace, ref.name, roots, visible); ok {
			return Resolution{kind = .Symbol, symbol = handle}, true
		}
		if handle, ok := global_visible_root_symbol(roots, namespace, ref.name); ok {
			return Resolution{kind = .Symbol, symbol = handle}, true
		}
	}
	if ref.kind == .Message_Class && ref.name in roots.provided_names {
		return Resolution{kind = .External}, true
	}
	return Resolution{}, false
}

reference_namespace_allowed :: proc(
	kind: Reference_Kind,
	requested, candidate: Namespace,
) -> bool {
	if kind == .Type_Ref && requested == .Value {
		return candidate == .Value || candidate == .Type
	}
	return candidate == requested
}

resolve_project_super :: proc(
	units: []Unit_Analysis,
	unit_index: int,
	scope_id: Scope_Id,
	roots: ^Project_Root_Lookup,
	visible: [dynamic]Unit_Id,
) -> (
	Symbol_Handle,
	bool,
) {
	class_symbol, ok := enclosing_instance_method_class_owner_unit(&units[unit_index], scope_id)
	if !ok {
		return {}, false
	}
	super_name, has_super := class_superclass_name(&units[unit_index], class_symbol)
	if !has_super {
		return {}, false
	}
	return resolve_type_name_in_project(units, unit_index, super_name, roots, visible)
}

resolve_inherited_project_symbol :: proc(
	units: []Unit_Analysis,
	unit_index: int,
	scope_id: Scope_Id,
	namespace: Namespace,
	name: string,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [dynamic]Unit_Id,
) -> (
	Symbol_Handle,
	bool,
) {
	current, ok := enclosing_class_owner_unit(&units[unit_index], scope_id)
	if !ok {
		return {}, false
	}
	current_handle := Symbol_Handle {
		unit   = units[unit_index].unit_id,
		symbol = current,
	}
	for _ in 0 ..< len(units) + 8 {
		next, next_ok := direct_superclass_handle(units, current_handle, roots, visible)
		if !next_ok {
			return {}, false
		}
		if member, member_ok := class_member_symbol_by_handle(
			units,
			next,
			namespace,
			name,
			class_entries,
			true,
		); member_ok {
			return member, true
		}
		current_handle = next
	}
	return {}, false
}
direct_superclass_handle :: proc(
	units: []Unit_Analysis,
	current: Symbol_Handle,
	roots: ^Project_Root_Lookup,
	visible: [dynamic]Unit_Id,
) -> (
	Symbol_Handle,
	bool,
) {
	unit_index := unit_id_index(current.unit)
	if unit_index < 0 || unit_index >= len(units) {
		return {}, false
	}
	super_name, ok := class_superclass_name(&units[unit_index], current.symbol)
	if !ok {
		return {}, false
	}
	return resolve_type_name_in_project(units, unit_index, super_name, roots, visible)
}

resolve_type_name_in_project :: proc(
	units: []Unit_Analysis,
	unit_index: int,
	name: string,
	roots: ^Project_Root_Lookup,
	visible: [dynamic]Unit_Id,
) -> (
	Symbol_Handle,
	bool,
) {
	if handle, ok := root_symbol_in_unit(roots, units[unit_index].unit_id, .Type, name); ok {
		return handle, true
	}
	if handle, ok := root_symbol_in_visible_units(.Type, name, roots, visible); ok {
		return handle, true
	}
	return global_visible_root_symbol(roots, .Type, name)
}

resolve_visible_class_definition_member :: proc(
	units: []Unit_Analysis,
	unit_index: int,
	scope_id: Scope_Id,
	namespace: Namespace,
	name: string,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [dynamic]Unit_Id,
	predecessors: [dynamic]Unit_Id,
) -> (
	Symbol_Handle,
	bool,
) {
	class_symbol, ok := enclosing_class_owner_unit(&units[unit_index], scope_id)
	if !ok {
		return {}, false
	}
	class_name := symbol(&units[unit_index], class_symbol).name
	if handle, found := class_member_symbol_in_unit_by_class_name(
		units,
		units[unit_index].unit_id,
		class_name,
		namespace,
		name,
		roots,
		class_entries,
		false,
	); found {
		return handle, true
	}
	for i := len(predecessors) - 1; i >= 0; i -= 1 {
		if handle, found := class_member_symbol_in_unit_by_class_name(
			units,
			predecessors[i],
			class_name,
			namespace,
			name,
			roots,
			class_entries,
			false,
		); found {
			return handle, true
		}
	}
	for unit_id in visible {
		if unit_id == units[unit_index].unit_id {
			continue
		}
		if handle, found := class_member_symbol_in_unit_by_class_name(
			units,
			unit_id,
			class_name,
			namespace,
			name,
			roots,
			class_entries,
			false,
		); found {
			return handle, true
		}
	}
	return {}, false
}
class_member_symbol_in_unit_by_class_name :: proc(
	units: []Unit_Analysis,
	unit_id: Unit_Id,
	class_name: string,
	namespace: Namespace,
	name: string,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	inherited: bool,
) -> (
	Symbol_Handle,
	bool,
) {
	class_handle, ok := root_symbol_in_unit(roots, unit_id, .Type, class_name)
	unit_index := unit_id_index(unit_id)
	if !ok || unit_index < 0 || unit_index >= len(units) {
		return {}, false
	}
	owner := symbol(&units[unit_index], class_handle.symbol)
	if owner == nil ||
	   !(owner.kind == .Class || owner.kind == .Interface) ||
	   (owner.kind == .Class && !unit_has_class_definition(&units[unit_index], class_handle.symbol)) {
		return {}, false
	}
	return class_member_symbol_by_handle(
		units,
		class_handle,
		namespace,
		name,
		class_entries,
		inherited,
	)
}

class_member_symbol_by_handle :: proc(
	units: []Unit_Analysis,
	class_handle: Symbol_Handle,
	namespace: Namespace,
	name: string,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	inherited: bool,
) -> (
	Symbol_Handle,
	bool,
) {
	unit_index := unit_id_index(class_handle.unit)
	if unit_index < 0 || unit_index >= len(units) {
		return {}, false
	}
	key := Project_Class_Member_Key {
		class_unit   = class_handle.unit,
		class_symbol = class_handle.symbol,
		namespace    = namespace,
		name         = name,
	}
	if entry, ok := class_entries[key]; ok {
		if inherited {
			member := unit_class_member_symbol(&units[unit_index], class_handle.symbol, name)
			info := entity_decl_info(&units[unit_index], member.id) if member != nil else nil
			if info != nil && info.visibility == .Private {
				return {}, false
			}
		}
		return Symbol_Handle{unit = entry.unit, symbol = entry.symbol}, true
	}
	return {}, false
}

root_symbol_in_visible_units :: proc(
	namespace: Namespace,
	name: string,
	roots: ^Project_Root_Lookup,
	visible: [dynamic]Unit_Id,
) -> (
	Symbol_Handle,
	bool,
) {
	for unit_id in visible {
		if handle, ok := root_symbol_in_unit(roots, unit_id, namespace, name); ok {
			return handle, true
		}
	}
	return {}, false
}

root_symbol_in_unit :: proc(
	roots: ^Project_Root_Lookup,
	unit_id: Unit_Id,
	namespace: Namespace,
	name: string,
) -> (
	Symbol_Handle,
	bool,
) {
	key := Root_Symbol_Key {
		unit      = unit_id,
		namespace = namespace,
		name      = name,
	}
	if handle, ok := roots.by_unit[key]; ok {
		return handle, true
	}
	return {}, false
}

global_visible_root_symbol :: proc(
	roots: ^Project_Root_Lookup,
	namespace: Namespace,
	name: string,
) -> (
	Symbol_Handle,
	bool,
) {
	key := Root_Name_Key {
		namespace = namespace,
		name      = name,
	}
	if handle, ok := roots.global[key]; ok {
		return handle, true
	}
	return {}, false
}

root_symbol_visible_by_default :: proc(unit: ^Unit_Analysis, s: ^Symbol_Data) -> bool {
	if typepool_dependency_unit(unit.uri) {
		return typepool_root_symbol_visible_by_default(s.kind)
	}
	stem := uri_file_stem(unit.uri)
	#partial switch s.kind {
	case .Class, .Interface:
		return name_is_namespaced(s.name) || root_name_matches_unit_stem(stem, s.name)
	case .Type_Def:
		return root_name_matches_unit_stem(stem, s.name)
	case .Module, .Report:
		return true
	case:
		return false
	}
}

typepool_dependency_unit :: proc(uri: string) -> bool {
	return strings.has_prefix(uri, "abapls-typepool:/")
}

typepool_root_symbol_visible_by_default :: proc(kind: Symbol_Kind) -> bool {
	return kind == .Type_Def || kind == .Constant
}

root_name_matches_unit_stem :: proc(stem, name: string) -> bool {
	if strings.equal_fold(stem, name) {
		return true
	}
	component_start := 0
	for i in 0 ..< len(name) {
		if name[i] == '/' {
			component_start = i + 1
		}
	}
	component := name[component_start:]
	return(
		component_start > 0 &&
		component != "" &&
		len(stem) >= len(component) &&
		strings.equal_fold(stem[len(stem) - len(component):], component) \
	)
}

name_is_namespaced :: proc(name: string) -> bool {
	return len(name) > 0 && name[0] == '/'
}

include_visible_units_for_units :: proc(
	units: []Unit_Analysis,
	allocator: mem.Allocator,
) -> [][dynamic]Unit_Id {
	out := make([][dynamic]Unit_Id, len(units), allocator)
	for i in 0 ..< len(units) {
		out[i] = make([dynamic]Unit_Id, allocator)
	}
	for unit in units {
		expansion := make([dynamic]Unit_Id, allocator)
		stack := make([dynamic]Unit_Id, allocator)
		collect_include_expansion(units, unit.unit_id, &stack, &expansion)
		for participant in expansion {
			idx := unit_id_index(participant)
			if idx < 0 || idx >= len(out) {
				continue
			}
			for candidate in expansion {
				if candidate != participant {
					push_unique_unit(&out[idx], candidate)
				}
			}
		}
	}
	return out
}

collect_include_expansion :: proc(
	units: []Unit_Analysis,
	unit_id: Unit_Id,
	stack, out: ^[dynamic]Unit_Id,
) {
	idx := unit_id_index(unit_id)
	if idx < 0 || idx >= len(units) || unit_list_contains(stack^[:], unit_id) {
		return
	}
	append(stack, unit_id)
	push_unique_unit(out, unit_id)
	for edge in units[idx].include_edges {
		if edge.has_target {
			collect_include_expansion(units, edge.target, stack, out)
		}
	}
	resize(stack, len(stack^) - 1)
}

include_predecessor_units_for_units :: proc(
	units: []Unit_Analysis,
	allocator: mem.Allocator,
) -> [][dynamic]Unit_Id {
	out := make([][dynamic]Unit_Id, len(units), allocator)
	for i in 0 ..< len(units) {
		out[i] = make([dynamic]Unit_Id, allocator)
	}
	for unit in units {
		stack := make([dynamic]Unit_Id, allocator)
		prior := make([dynamic]Unit_Id, allocator)
		_ = record_include_predecessors(units, unit.unit_id, prior, &out, &stack, allocator)
	}
	return out
}

record_include_predecessors :: proc(
	units: []Unit_Analysis,
	unit_id: Unit_Id,
	inherited_prior: [dynamic]Unit_Id,
	predecessors: ^[][dynamic]Unit_Id,
	stack: ^[dynamic]Unit_Id,
	allocator: mem.Allocator,
) -> [dynamic]Unit_Id {
	expansion := make([dynamic]Unit_Id, allocator)
	idx := unit_id_index(unit_id)
	if idx < 0 || idx >= len(units) || unit_list_contains(stack^[:], unit_id) {
		return expansion
	}
	append(stack, unit_id)
	push_unique_unit(&expansion, unit_id)
	prior := make([dynamic]Unit_Id, 0, len(inherited_prior) + 1, allocator)
	for item in inherited_prior {push_unique_unit(&prior, item)}
	push_unique_unit(&prior, unit_id)
	for edge in units[idx].include_edges {
		if !edge.has_target {
			continue
		}
		target_idx := unit_id_index(edge.target)
		if target_idx >= 0 && target_idx < len(predecessors^) {
			for item in prior {
				push_unique_unit(&predecessors^[target_idx], item)
			}
		}
		nested := record_include_predecessors(
			units,
			edge.target,
			prior,
			predecessors,
			stack,
			allocator,
		)
		for item in nested {
			push_unique_unit(&prior, item)
			push_unique_unit(&expansion, item)
		}
	}
	resize(stack, len(stack^) - 1)
	return expansion
}

push_unique_unit :: proc(units: ^[dynamic]Unit_Id, unit_id: Unit_Id) {
	if !unit_list_contains(units^[:], unit_id) {
		append(units, unit_id)
	}
}

unit_list_contains :: proc(units: []Unit_Id, unit_id: Unit_Id) -> bool {
	for item in units {
		if item == unit_id {
			return true
		}
	}
	return false
}

unit_has_class_definition :: proc(unit: ^Unit_Analysis, class_symbol: Symbol_Id) -> bool {
	for definition in unit.class_definitions {
		if definition.class_symbol == class_symbol {
			return true
		}
	}
	return false
}

expand_local_structure_includes :: proc(
	unit: ^Unit_Analysis,
	allocator: mem.Allocator,
) -> bool {
	any_changed := false
	changed := true
	for changed {
		changed = false
		for symbol_index in 0 ..< len(unit.symbols) {
			s := &unit.symbols[symbol_index]
			if s.structure != INVALID_STRUCTURE_ID || !s.has_declared_type {
				continue
			}
			if structure_id, ok := local_structure_for_type_ref(unit, s.scope, s.declared_type);
			   ok {
				s.structure = structure_id
				s.type_id = type_id_from_symbol_data(unit, s)
				changed = true
				any_changed = true
			}
		}
		for structure_index := 0; structure_index < len(unit.structures); structure_index += 1 {
			owner_scope := unit.structures[structure_index].scope
			if owner_scope == INVALID_SCOPE_ID {
				owner_scope = unit.root_scope
			}
			for field_index in 0 ..< len(unit.structures[structure_index].fields) {
				field := &unit.structures[structure_index].fields[field_index]
				if field.structure != INVALID_STRUCTURE_ID ||
				   !(.Has_Type_Ref in field.flags) {
					continue
				}
				if structure_id, ok := local_structure_for_type_ref(unit, owner_scope, field.type_ref);
				   ok && (!(.Is_Include in field.flags) ||
				         structure_id != unit.structures[structure_index].id) {
					field.structure = structure_id
					if !type_id_is_known(field.type_id) {
						field.type_id = type_structure(unit, structure_id)
					}
					changed = true
					any_changed = true
				}
			}
		}
		if expand_resolved_structure_includes(unit, allocator) {
			changed = true
			any_changed = true
		}
	}
	return any_changed
}

expand_resolved_structure_includes :: proc(unit: ^Unit_Analysis, allocator: mem.Allocator) -> bool {
	changed := false
	for structure_index := 0; structure_index < len(unit.structures); structure_index += 1 {
		old_fields := unit.structures[structure_index].fields
		has_include := false
		for field in old_fields {
			if .Is_Include in field.flags &&
			   field.structure != INVALID_STRUCTURE_ID &&
			   field.structure != unit.structures[structure_index].id {
				has_include = true
				break
			}
		}
		if !has_include {
			continue
		}
		new_fields := make([dynamic]Structure_Field_Data, 0, len(old_fields), allocator)
		for field in old_fields {
			if .Is_Include in field.flags &&
			   field.structure != INVALID_STRUCTURE_ID &&
			   field.structure != unit.structures[structure_index].id {
				if included := structure(unit, field.structure); included != nil {
					for included_field in included.fields {
						append(&new_fields, included_field)
					}
					changed = true
					continue
				}
			}
			append(&new_fields, field)
		}
		unit.structures[structure_index].fields = new_fields
	}
	return changed
}

local_structure_for_type_ref :: proc(
	unit: ^Unit_Analysis,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
) -> (
	Structure_Id,
	bool,
) {
	namespaces := [?]Namespace{.Type, .Value, .Routine}
	for namespace in namespaces {
		if !(namespace == type_ref.namespace ||
			   (type_ref.namespace == .Type && namespace == .Value)) {
			continue
		}
		symbol_id, ok := lookup_scope_chain(
			unit,
			&unit.scope_index,
			scope_id,
			namespace,
			type_ref.base_name,
		)
		if !ok {
			continue
		}
		if structure_id, structure_ok := local_structure_for_symbol_path(
			unit,
			symbol_id,
			type_ref.field_path[:],
			type_ref.field_selectors[:],
			type_ref.field_derefs[:],
		); structure_ok {
			return structure_id, true
		}
	}
	if type_ref.namespace == .Value || type_ref.namespace == .Type {
		if class_symbol, ok := enclosing_class_owner_unit(unit, scope_id); ok {
			if symbol_id, symbol_ok := class_scope_symbol(
				&unit.scope_index,
				class_symbol,
				type_ref.namespace,
				type_ref.base_name,
			); symbol_ok {
				return local_structure_for_symbol_path(unit, symbol_id, type_ref.field_path[:], type_ref.field_selectors[:], type_ref.field_derefs[:])
			}
		}
	}
	if type_ref.namespace == .Type {
		if symbol_id, symbol_ok := resolve_inherited_class_member(
			unit,
			&unit.scope_index,
			scope_id,
			.Type,
			type_ref.base_name,
		); symbol_ok {
			return local_structure_for_symbol_path(unit, symbol_id, type_ref.field_path[:], type_ref.field_selectors[:], type_ref.field_derefs[:])
		}
	}
	if type_ref.namespace == .Value {
		if symbol_id, symbol_ok := inherited_class_attribute_symbol_for_type_ref(
			unit,
			scope_id,
			type_ref.base_name,
		); symbol_ok {
			return local_structure_for_symbol_path(unit, symbol_id, type_ref.field_path[:], type_ref.field_selectors[:], type_ref.field_derefs[:])
		}
	}
	return INVALID_STRUCTURE_ID, false
}

inherited_class_attribute_symbol_for_type_ref :: proc(
	unit: ^Unit_Analysis,
	scope_id: Scope_Id,
	name: string,
) -> (
	Symbol_Id,
	bool,
) {
	current_class, ok := enclosing_class_owner_unit(unit, scope_id)
	if !ok {
		return INVALID_SYMBOL_ID, false
	}
	for _ in 0 ..= len(unit.class_inheritance) {
		super_name, has_super := class_superclass_name(unit, current_class)
		if !has_super {
			return INVALID_SYMBOL_ID, false
		}
		super_symbol, super_ok := lookup_scope_chain(
			unit,
			&unit.scope_index,
			scope_id,
			.Type,
			super_name,
		)
		if !super_ok {
			return INVALID_SYMBOL_ID, false
		}
		member := unit_class_member_symbol(unit, super_symbol, name)
		info := entity_decl_info(unit, member.id) if member != nil else nil
		if info != nil && info.member_kind == .Attribute && info.visibility != .Private {
			return class_scope_symbol(&unit.scope_index, super_symbol, .Value, name)
		}
		current_class = super_symbol
	}
	return INVALID_SYMBOL_ID, false
}

local_structure_for_symbol_path :: proc(
	unit: ^Unit_Analysis,
	symbol_id: Symbol_Id,
	path: []string,
	selectors: []ast.Selector_Op,
	derefs: []bool,
) -> (
	Structure_Id,
	bool,
) {
	current_symbol_id := symbol_id
	current_path := path
	s := symbol(unit, current_symbol_id)
	if s != nil && (s.kind == .Class || s.kind == .Interface) {
		if len(current_path) == 0 {
			return INVALID_STRUCTURE_ID, false
		}
		nested, nested_ok := class_type_symbol_handle_in_unit(
			unit,
			current_symbol_id,
			current_path[0],
		)
		if !nested_ok {
			return INVALID_STRUCTURE_ID, false
		}
		current_symbol_id = nested
		s = symbol(unit, current_symbol_id)
		current_path = current_path[1:]
	}
	current_derefs := derefs
	if len(derefs) > 0 {
		current_derefs = derefs[len(path) - len(current_path):]
	}
	current_selectors := selectors
	if len(selectors) > 0 {
		current_selectors = selectors[len(path) - len(current_path):]
	}
	if s != nil &&
	   s.structure == INVALID_STRUCTURE_ID &&
	   len(current_path) > 0 &&
	   selector_at(current_selectors, 0) == .Arrow &&
	   s.has_declared_type &&
	   s.declared_type.is_ref {
		if class_symbol, class_ok := local_class_symbol_for_type_ref(unit, s.scope, s.declared_type);
		   class_ok {
			return local_structure_for_class_member_path(
				unit,
				class_symbol,
				current_path,
				current_selectors,
				current_derefs,
			)
		}
	}
	if s == nil || s.structure == INVALID_STRUCTURE_ID {
		return INVALID_STRUCTURE_ID, false
	}
	return resolve_unit_structure_path(unit, s.structure, current_path, current_selectors, current_derefs)
}

local_class_symbol_for_type_ref :: proc(
	unit: ^Unit_Analysis,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
) -> (
	Symbol_Id,
	bool,
) {
	if type_ref.base_name == "" {
		return INVALID_SYMBOL_ID, false
	}
	symbol_id, ok := lookup_scope_chain(
		unit,
		&unit.scope_index,
		scope_id,
		type_ref.namespace,
		type_ref.base_name,
	)
	if !ok && type_ref.namespace == .Type {
		symbol_id, ok = lookup_scope_chain(unit, &unit.scope_index, scope_id, .Value, type_ref.base_name)
	}
	if !ok {
		return INVALID_SYMBOL_ID, false
	}
	s := symbol(unit, symbol_id)
	if s == nil || !(s.kind == .Class || s.kind == .Interface) {
		return INVALID_SYMBOL_ID, false
	}
	return symbol_id, true
}

local_structure_for_class_member_path :: proc(
	unit: ^Unit_Analysis,
	class_symbol: Symbol_Id,
	path: []string,
	selectors: []ast.Selector_Op,
	derefs: []bool,
) -> (
	Structure_Id,
	bool,
) {
	if len(path) == 0 || selector_at(selectors, 0) != .Arrow {
		return INVALID_STRUCTURE_ID, false
	}
	member := unit_class_member_symbol(unit, class_symbol, path[0])
	info := entity_decl_info(unit, member.id) if member != nil else nil
	if member == nil || info == nil || info.member_kind != .Attribute || member.structure == INVALID_STRUCTURE_ID {
		return INVALID_STRUCTURE_ID, false
	}
	if len(path) == 1 {
		return member.structure, true
	}
	next_selectors := selectors
	next_derefs := derefs
	if len(selectors) > 0 {
		next_selectors = selectors[1:]
	}
	if len(derefs) > 0 {
		next_derefs = derefs[1:]
	}
	return resolve_unit_structure_path(unit, member.structure, path[1:], next_selectors, next_derefs)
}

resolve_unit_structure_path :: proc(
	unit: ^Unit_Analysis,
	start: Structure_Id,
	path: []string,
	selectors: []ast.Selector_Op,
	derefs: []bool,
) -> (
	Structure_Id,
	bool,
) {
	current := start
	for field_name, i in path {
		if i < len(derefs) && derefs[i] {
			continue
		}
		if selector_at(selectors, i) != .Dash {
			return INVALID_STRUCTURE_ID, false
		}
		field := structure_field(unit, current, field_name)
		if field == nil || field.structure == INVALID_STRUCTURE_ID {
			return INVALID_STRUCTURE_ID, false
		}
		current = field.structure
	}
	return current, true
}

selector_at :: #force_inline proc(selectors: []ast.Selector_Op, index: int) -> ast.Selector_Op {
	return selectors[index] if index < len(selectors) else .Dash
}

class_type_symbol_handle :: proc(
	units: []Unit_Analysis,
	class_handle: Symbol_Handle,
	name: string,
) -> (
	Symbol_Handle,
	bool,
) {
	unit_index := unit_id_index(class_handle.unit)
	if unit_index < 0 || unit_index >= len(units) {
		return {}, false
	}
	if symbol_id, ok := class_type_symbol_handle_in_unit(
		&units[unit_index],
		class_handle.symbol,
		name,
	); ok {
		return Symbol_Handle{unit = class_handle.unit, symbol = symbol_id}, true
	}
	return {}, false
}

class_type_symbol_handle_in_unit :: proc(
	unit: ^Unit_Analysis,
	class_symbol: Symbol_Id,
	name: string,
) -> (
	Symbol_Id,
	bool,
) {
	key := Class_Scope_Index_Key {
		class_symbol = class_symbol,
		namespace    = .Type,
		name         = name,
	}
	if symbol_id, ok := unit.scope_index.class_symbols[key]; ok {
		return symbol_id, true
	}
	return INVALID_SYMBOL_ID, false
}

resolve_type_ref_handle_project :: proc(
	units: []Unit_Analysis,
	unit_index: int,
	type_ref: Field_Type_Ref_Data,
	roots: ^Project_Root_Lookup,
	visible: [dynamic]Unit_Id,
) -> (
	Symbol_Handle,
	bool,
) {
	all_namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in all_namespaces {
		if !(namespace == type_ref.namespace ||
			   (type_ref.namespace == .Value && namespace == .Type)) {
			continue
		}
		if handle, ok := root_symbol_in_unit(
			roots,
			units[unit_index].unit_id,
			namespace,
			type_ref.base_name,
		); ok {
			return handle, true
		}
		if handle, ok := root_symbol_in_visible_units(
			namespace,
			type_ref.base_name,
			roots,
			visible,
		); ok {
			return handle, true
		}
		if handle, ok := global_visible_root_symbol(roots, namespace, type_ref.base_name); ok {
			return handle, true
		}
	}
	return {}, false
}
