package abap_frontend_semantic_analyze

import "src:ast"
import execution "src:execution"
import "src:parser"

import "core:mem"
import "core:strings"

build_scope_index :: proc(unit: ^Unit_Analysis, allocator: mem.Allocator) -> Scope_Index {
	index := Scope_Index {
		scope_count   = len(unit.scopes),
		symbols       = make(map[Scope_Index_Key]Symbol_Id, len(unit.symbols) * 2, allocator),
		class_symbols = make(map[Class_Scope_Index_Key]Symbol_Id, len(unit.symbols), allocator),
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
				index.symbols[Scope_Index_Key{scope = symbol.scope, namespace = namespace, name = symbol.name}] =
					symbol.id
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
	resolve_unit_with_index(unit, &index)
	unit.scope_index = index
}

resolve_unit_with_index :: proc(unit: ^Unit_Analysis, index: ^Scope_Index) {
	for i in 0 ..< len(unit.references) {
		ref := &unit.references[i]
		if resolution, ok := resolve_reference(unit, index, ref^); ok {
			ref.resolution = resolution
			ref.has_resolution = true
		}
	}
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
	index: ^Scope_Index,
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
		if scope_idx >= 0 && scope_idx < index.scope_count {
			key := Scope_Index_Key {
				scope     = current,
				namespace = namespace,
				name      = name,
			}
			if symbol_id, ok := index.symbols[key]; ok {
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
			member := unit_class_member(unit, class_symbol, method.name)
			return class_symbol, member == nil || !(.Is_Static in member.flags)
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
	names:          map[string]bool,
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

resolve_project_cross_unit :: proc(units: []Unit_Analysis, allocator: mem.Allocator) {
	if len(units) == 0 {
		return
	}
	roots := build_project_root_index(units, allocator)
	root_lookup := build_project_root_lookup(units, roots[:], allocator)
	visible := include_visible_units_for_units(units, allocator)
	predecessors := include_predecessor_units_for_units(units, allocator)
	class_entries := build_project_class_scope_index(units, &root_lookup, visible, allocator)

	for unit, unit_index in units {
		for ref_index in 0 ..< len(unit.references) {
			ref := &units[unit_index].references[ref_index]
			if ref.has_resolution {
				continue
			}
			if resolution, ok := resolve_project_reference(
				units,
				unit_index,
				ref^,
				&root_lookup,
				class_entries,
				visible[unit_index],
				predecessors[unit_index],
			); ok {
				ref.resolution = resolution
				ref.has_resolution = true
			}
		}
	}

	if seed_inherited_method_scope_parameters(
		units,
		&root_lookup,
		class_entries,
		visible,
		predecessors,
		allocator,
	) {
		for unit_index in 0 ..< len(units) {
			units[unit_index].scope_index = build_scope_index(&units[unit_index], allocator)
			resolve_unit_with_index(&units[unit_index], &units[unit_index].scope_index)
		}
	}

	changed := true
	for changed {
		changed = false
		for unit_index in 0 ..< len(units) {
			changed =
				import_project_structures_for_unit(
					units,
					unit_index,
					&root_lookup,
					class_entries,
					visible[unit_index],
					allocator,
				) ||
				changed
		}
	}
}

seed_inherited_method_scope_parameters :: proc(
	units: []Unit_Analysis,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [][dynamic]Unit_Id,
	predecessors: [][dynamic]Unit_Id,
	allocator: mem.Allocator,
) -> bool {
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	changed := false
	for unit_index in 0 ..< len(units) {
		unit := &units[unit_index]
		method_scope_by_owner := make(
			[dynamic]Scope_Id,
			0,
			len(unit.symbols),
			context.temp_allocator,
		)
		for _ in 0 ..< len(unit.symbols) {
			append(&method_scope_by_owner, INVALID_SCOPE_ID)
		}
		for &s in unit.scopes {
			if s.kind != .Method || s.owner == INVALID_SYMBOL_ID {
				continue
			}
			owner_index := symbol_id_index(s.owner)
			if owner_index < len(method_scope_by_owner) &&
			   method_scope_by_owner[owner_index] == INVALID_SCOPE_ID {
				method_scope_by_owner[owner_index] = s.id
			}
		}
		symbol_count := len(unit.symbols)
		for symbol_index in 0 ..< symbol_count {
			method_symbol := &unit.symbols[symbol_index]
			if method_symbol.kind != .Method {
				continue
			}
			owner_index := symbol_id_index(method_symbol.id)
			if owner_index >= len(method_scope_by_owner) {
				continue
			}
			method_scope := method_scope_by_owner[owner_index]
			if method_scope == INVALID_SCOPE_ID {
				continue
			}
			member, member_unit_index := method_signature_member_for_scope(
				units,
				unit_index,
				method_symbol.scope,
				method_symbol.name,
				roots,
				class_entries,
				visible[unit_index],
				predecessors[unit_index],
			)
			if member == nil {
				continue
			}
			if .For_Event in member.flags {
				changed =
					seed_event_handler_method_parameter_types(
						units,
						unit_index,
						method_scope,
						member_unit_index,
						member,
						roots,
						class_entries,
						visible,
						allocator,
					) ||
					changed
			}
			for param in member.parameters {
				if method_scope_has_value_symbol(unit, method_scope, param.name) {
					continue
				}
				structure_id := seeded_method_parameter_structure(
					units,
					unit_index,
					member_unit_index,
					member,
					param,
					roots,
					class_entries,
					visible,
					allocator,
				)
				_ = declare_symbol(
					unit,
					method_scope,
					param.name,
					.Parameter,
					method_symbol.decl_range,
					structure_id,
					param.declared_type,
					.Has_Declared_Type in param.flags,
					param.type_clause_display,
					type_clause_table_has_of = param.type_clause_table_has_of,
				)
				changed = true
			}
		}
	}
	return changed
}

seed_event_handler_method_parameter_types :: proc(
	units: []Unit_Analysis,
	unit_index: int,
	method_scope: Scope_Id,
	member_unit_index: int,
	member: ^Class_Member_Data,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [][dynamic]Unit_Id,
	allocator: mem.Allocator,
) -> bool {
	if member_unit_index < 0 ||
	   member_unit_index >= len(units) ||
	   member.event_name == "" ||
	   member.event_source_type.base_name == "" {
		return false
	}
	source_handle, source_ok := resolve_type_ref_handle_project(
		units,
		member_unit_index,
		member.event_source_type,
		roots,
		visible[member_unit_index],
	)
	if !source_ok {
		return false
	}
	event_member, event_unit_index := event_member_for_handler_source(
		units,
		source_handle,
		member.event_name,
		class_entries,
	)
	if event_member == nil {
		return false
	}
	changed := false
	for &param in member.parameters {
		if .Has_Declared_Type in param.flags {
			continue
		}
		event_param := class_member_parameter(event_member, param.name)
		if event_param == nil || !(.Has_Declared_Type in event_param.flags) {
			continue
		}
		param.declared_type = event_param.declared_type
		param.type_clause_display = event_param.type_clause_display
		param.type_clause_form = event_param.type_clause_form
		param.has_type_clause_form = event_param.has_type_clause_form
		param.type_clause_table_has_of = event_param.type_clause_table_has_of
		param.flags += {.Has_Declared_Type}
		structure_id := seeded_method_parameter_structure(
			units,
			unit_index,
			event_unit_index,
			event_member,
			event_param^,
			roots,
			class_entries,
			visible,
			allocator,
		)
		changed =
			update_method_scope_parameter_symbol(
				&units[unit_index],
				method_scope,
				param.name,
				event_param^,
				structure_id,
			) ||
			changed
	}
	return changed
}

event_member_for_handler_source :: proc(
	units: []Unit_Analysis,
	source_handle: Symbol_Handle,
	event_name: string,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
) -> (
	^Class_Member_Data,
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
		return nil, -1
	}
	event_unit_index := unit_id_index(event_handle.unit)
	if event_unit_index < 0 || event_unit_index >= len(units) {
		return nil, -1
	}
	event_symbol := symbol(&units[event_unit_index], event_handle.symbol)
	if event_symbol == nil {
		return nil, -1
	}
	event_owner, owner_ok := enclosing_class_owner_unit(&units[event_unit_index], event_symbol.scope)
	if !owner_ok {
		return nil, -1
	}
	event_member := unit_class_member(&units[event_unit_index], event_owner, event_symbol.name)
	if event_member == nil || event_member.kind != .Event {
		return nil, -1
	}
	return event_member, event_unit_index
}

class_member_parameter :: proc(
	member: ^Class_Member_Data,
	name: string,
) -> ^Class_Member_Parameter_Data {
	for &param in member.parameters {
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
	param: Class_Member_Parameter_Data,
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
) -> (
	^Class_Member_Data,
	int,
) {
	if interface_name, member_name, qualified := qualified_method_parts(method_name); qualified {
		if member, member_unit_index := exposed_interface_member_for_scope(
			units,
			unit_index,
			scope_id,
			interface_name,
			member_name,
			roots,
			visible,
		); member != nil {
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
		return nil, -1
	}
	member_unit_index := unit_id_index(member_handle.unit)
	if member_unit_index < 0 || member_unit_index >= len(units) {
		return nil, -1
	}
	member_unit := &units[member_unit_index]
	member_symbol := symbol(member_unit, member_handle.symbol)
	if member_symbol == nil {
		return nil, -1
	}
	class_symbol, class_ok := enclosing_class_owner_unit(member_unit, member_symbol.scope)
	if !class_ok {
		return nil, -1
	}
	member := unit_class_member(member_unit, class_symbol, method_name)
	if member == nil || len(member.parameters) > 0 || !(.Is_Redefinition in member.flags) {
		return member, member_unit_index
	}
	if inherited, inherited_unit_index := inherited_project_class_member(
		units,
		Symbol_Handle{unit = member_handle.unit, symbol = class_symbol},
		method_name,
		roots,
		class_entries,
		visible,
	); inherited != nil {
		return inherited, inherited_unit_index
	}
	return member, member_unit_index
}

exposed_interface_member_for_scope :: proc(
	units: []Unit_Analysis,
	unit_index: int,
	scope_id: Scope_Id,
	interface_name, member_name: string,
	roots: ^Project_Root_Lookup,
	visible: [dynamic]Unit_Id,
) -> (
	^Class_Member_Data,
	int,
) {
	class_symbol, class_ok := enclosing_class_owner_unit(&units[unit_index], scope_id)
	if !class_ok {
		return nil, -1
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
		return nil, -1
	}
	interface_unit_index := unit_id_index(handle.unit)
	if interface_unit_index < 0 || interface_unit_index >= len(units) {
		return nil, -1
	}
	member := unit_class_member(&units[interface_unit_index], handle.symbol, member_name)
	return member, interface_unit_index if member != nil else -1
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
) -> (
	^Class_Member_Data,
	int,
) {
	current := class_handle
	fallback: ^Class_Member_Data
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
						if member := unit_class_member(member_unit, class_symbol, s.name); member != nil {
							if fallback == nil {
								fallback = member
								fallback_index = member_unit_index
							}
							if len(member.parameters) == 0 && .Is_Redefinition in member.flags {
								current = Symbol_Handle{unit = member_handle.unit, symbol = class_symbol}
								continue
							}
							return member, member_unit_index
						}
					}
				}
			}
		}
		current = next
	}
	return fallback, fallback_index
}

seeded_method_parameter_structure :: proc(
	units: []Unit_Analysis,
	target_unit_index, member_unit_index: int,
	member: ^Class_Member_Data,
	param: Class_Member_Parameter_Data,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [][dynamic]Unit_Id,
	allocator: mem.Allocator,
) -> Structure_Id {
	if !(.Has_Declared_Type in param.flags) {
		return INVALID_STRUCTURE_ID
	}
	source_scope := class_scope_for_owner(&units[member_unit_index], member.class_symbol)
	if source_scope == INVALID_SCOPE_ID {
		return INVALID_STRUCTURE_ID
	}
	source_structure, ok := import_structure_for_type_ref(
		units,
		member_unit_index,
		source_scope,
		param.declared_type,
		roots,
		class_entries,
		visible[member_unit_index],
		allocator,
	)
	if !ok {
		return INVALID_STRUCTURE_ID
	}
	return import_structure_to_unit(
		&units[target_unit_index],
		&units[member_unit_index],
		source_structure,
		allocator,
	)
}

class_scope_for_owner :: proc(unit: ^Unit_Analysis, owner: Symbol_Id) -> Scope_Id {
	found := INVALID_SCOPE_ID
	for s in unit.scopes {
		if (s.kind == .Class || s.kind == .Interface) && s.owner == owner {
			if found == INVALID_SCOPE_ID {
				found = s.id
			}
			if len(s.declarations) > 0 {
				return s.id
			}
		}
	}
	return found
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

build_project_root_index :: proc(
	units: []Unit_Analysis,
	allocator: mem.Allocator,
) -> [dynamic]Root_Symbol_Entry {
	roots := make([dynamic]Root_Symbol_Entry, 0, 32, allocator)
	for &unit in units {
		unit_stem := uri_file_stem(unit.uri)
		is_typepool := typepool_dependency_unit(unit.uri)
		for &symbol in unit.symbols {
			if symbol.scope != unit.root_scope {
				continue
			}
			visible_by_default := false
			if is_typepool {
				visible_by_default = typepool_root_symbol_visible_by_default(symbol.kind)
			} else {
				#partial switch symbol.kind {
				case .Class, .Interface:
					visible_by_default =
						name_is_namespaced(symbol.name) ||
						root_name_matches_unit_stem(unit_stem, symbol.name)
				case .Type_Def:
					visible_by_default = root_name_matches_unit_stem(unit_stem, symbol.name)
				case .Module, .Report:
					visible_by_default = true
				}
			}
			namespaces := [?]Namespace{.Value, .Type, .Routine}
			for namespace in namespaces {
				if symbol_kind_occupies(symbol.kind, namespace) {
					append(
						&roots,
						Root_Symbol_Entry {
							unit = unit.unit_id,
							symbol = symbol.id,
							namespace = namespace,
							name = symbol.name,
							visible_by_default = visible_by_default,
						},
					)
				}
			}
		}
	}
	return roots
}

build_project_root_lookup :: proc(
	units: []Unit_Analysis,
	roots: []Root_Symbol_Entry,
	allocator: mem.Allocator,
) -> Project_Root_Lookup {
	provided_name_count := 0
	for i in 0 ..< len(units) {
		provided_name_count += len(units[i].provided_names)
	}
	lookup := Project_Root_Lookup {
		by_unit        = make(map[Root_Symbol_Key]Symbol_Handle, len(roots), allocator),
		global         = make(map[Root_Name_Key]Symbol_Handle, len(roots), allocator),
		names          = make(map[string]bool, len(roots), allocator),
		provided_names = make(map[string]bool, provided_name_count, allocator),
	}
	for i in 0 ..< len(units) {
		for name in units[i].provided_names {
			lookup.provided_names[name] = true
		}
	}
	for entry in roots {
		handle := Symbol_Handle {
			unit   = entry.unit,
			symbol = entry.symbol,
		}
		unit_key := Root_Symbol_Key {
			unit      = entry.unit,
			namespace = entry.namespace,
			name      = entry.name,
		}
		_, slot, inserted, _ := map_entry(&lookup.by_unit, unit_key)
		if inserted {
			slot^ = handle
		}
		if entry.visible_by_default {
			lookup.names[entry.name] = true
			global_key := Root_Name_Key {
				namespace = entry.namespace,
				name      = entry.name,
			}
			_, global_slot, global_inserted, _ := map_entry(&lookup.global, global_key)
			if global_inserted {
				global_slot^ = handle
			}
		}
	}
	return lookup
}

build_project_class_scope_index :: proc(
	units: []Unit_Analysis,
	roots: ^Project_Root_Lookup,
	visible: [][dynamic]Unit_Id,
	allocator: mem.Allocator,
) -> map[Project_Class_Member_Key]Project_Class_Member_Entry {
	symbol_hint := 0
	for unit in units {
		symbol_hint += len(unit.symbols)
	}
	out := make(map[Project_Class_Member_Key]Project_Class_Member_Entry, symbol_hint, allocator)
	for &unit in units {
		for symbol in unit.symbols {
			scope_data := scope(&unit, symbol.scope)
			if scope_data == nil ||
			   !(scope_data.kind == .Class || scope_data.kind == .Interface) ||
			   scope_data.owner == INVALID_SYMBOL_ID {
				continue
			}
			namespaces := [?]Namespace{.Value, .Type, .Routine}
			for namespace in namespaces {
				if symbol_kind_occupies(symbol.kind, namespace) {
					key := Project_Class_Member_Key {
						class_unit   = unit.unit_id,
						class_symbol = scope_data.owner,
						namespace    = namespace,
						name         = symbol.name,
					}
					if key in out {
						continue
					}
					out[key] = Project_Class_Member_Entry {
						unit   = unit.unit_id,
						symbol = symbol.id,
					}
				}
			}
		}
	}
	changed := true
	for changed {
		changed = false
		for unit, unit_index in units {
			if unit_index >= len(visible) {
				continue
			}
			for alias in unit.member_aliases {
				if alias.alias_name == "" || alias.target_interface_name == "" {
					continue
				}
				target, ok := resolve_type_name_in_project(
					units,
					unit_index,
					alias.target_interface_name,
					roots,
					visible[unit_index],
				)
				if !ok {
					continue
				}
				target_name := alias.target_member_name
				if target_name == "" {
					target_name = alias.alias_name
				}
				namespaces := [?]Namespace{.Value, .Type, .Routine}
				for namespace in namespaces {
					target_key := Project_Class_Member_Key {
						class_unit   = target.unit,
						class_symbol = target.symbol,
						namespace    = namespace,
						name         = target_name,
					}
					alias_key := Project_Class_Member_Key {
						class_unit   = unit.unit_id,
						class_symbol = alias.owner_symbol,
						namespace    = namespace,
						name         = alias.alias_name,
					}
					if alias_key in out {
						continue
					}
					if target_entry, target_ok := out[target_key]; target_ok {
						out[alias_key] = target_entry
						changed = true
					}
				}
			}
		}
	}
	return out
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
	if (ref.namespace == .Type || ref.namespace == .Routine) && ref.name in roots.names {
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
			member := unit_class_member(&units[unit_index], class_handle.symbol, name)
			if member != nil && member.visibility == .Private {
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

unit_class_member :: proc(
	unit: ^Unit_Analysis,
	class_symbol: Symbol_Id,
	name: string,
) -> ^Class_Member_Data {
	for &member in unit.class_members {
		if member.class_symbol == class_symbol && member.name == name {
			return &member
		}
	}
	return nil
}

import_project_structures_for_unit :: proc(
	units: []Unit_Analysis,
	unit_index: int,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [dynamic]Unit_Id,
	allocator: mem.Allocator,
) -> bool {
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	owner_scopes := make(
		[dynamic]Scope_Id,
		0,
		len(units[unit_index].structures),
		context.temp_allocator,
	)
	owner_scope_set := make(
		[dynamic]bool,
		0,
		len(units[unit_index].structures),
		context.temp_allocator,
	)
	any_changed := false
	changed := true
	for changed {
		changed = false
		for symbol_index in 0 ..< len(units[unit_index].symbols) {
			s := &units[unit_index].symbols[symbol_index]
			if s.structure != INVALID_STRUCTURE_ID || !s.has_declared_type {
				continue
			}
			if structure_id, ok := import_structure_for_type_ref(
				units,
				unit_index,
				s.scope,
				s.declared_type,
				roots,
				class_entries,
				visible,
				allocator,
			); ok {
				s.structure = structure_id
				changed = true
				any_changed = true
			}
		}
		resize(&owner_scopes, 0)
		resize(&owner_scope_set, 0)
		for _ in 0 ..< len(units[unit_index].structures) {
			append(&owner_scopes, units[unit_index].root_scope)
			append(&owner_scope_set, false)
		}
		for &s in units[unit_index].symbols {
			if s.structure == INVALID_STRUCTURE_ID {
				continue
			}
			index := structure_id_index(s.structure)
			if index < len(owner_scopes) && !owner_scope_set[index] {
				owner_scopes[index] = s.scope
				owner_scope_set[index] = true
			}
		}
		for structure_index := 0;
		    structure_index < len(units[unit_index].structures);
		    structure_index += 1 {
			if structure_index >= len(owner_scopes) {
				append(&owner_scopes, units[unit_index].root_scope)
				append(&owner_scope_set, false)
			}
			owner_scope := owner_scopes[structure_index]
			for field_index in 0 ..< len(units[unit_index].structures[structure_index].fields) {
				field := units[unit_index].structures[structure_index].fields[field_index]
				if field.structure != INVALID_STRUCTURE_ID || !(.Has_Type_Ref in field.flags) {
					continue
				}
				if structure_id, ok := import_structure_for_type_ref(
					units,
					unit_index,
					owner_scope,
					field.type_ref,
					roots,
					class_entries,
					visible,
					allocator,
				); ok {
					units[unit_index].structures[structure_index].fields[field_index].structure =
						structure_id
					changed = true
					any_changed = true
				}
			}
		}
	}
	sync_class_member_structures_for_unit(&units[unit_index])
	return any_changed
}

sync_class_member_structures_for_unit :: proc(unit: ^Unit_Analysis) {
	for &member in unit.class_members {
		if member.kind != .Attribute {
			continue
		}
		symbol_id, ok := class_scope_symbol(
			&unit.scope_index,
			member.class_symbol,
			.Value,
			member.name,
		)
		if !ok {
			continue
		}
		if s := symbol(unit, symbol_id); s != nil {
			member.structure = s.structure
		}
	}
}

import_structure_for_type_ref :: proc(
	units: []Unit_Analysis,
	unit_index: int,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [dynamic]Unit_Id,
	allocator: mem.Allocator,
) -> (
	Structure_Id,
	bool,
) {
	if type_ref.base_name == "" || is_builtin_type_name(type_ref.base_name) {
		return INVALID_STRUCTURE_ID, false
	}
	if structure_id, ok := local_structure_for_type_ref(&units[unit_index], scope_id, type_ref);
	   ok {
		return structure_id, true
	}
	if structure_id, ok := project_structure_for_type_ref(
		units,
		unit_index,
		scope_id,
		type_ref,
		roots,
		class_entries,
		visible,
		allocator,
	); ok {
		return structure_id, true
	}
	handle, ok := resolve_type_ref_handle_project(units, unit_index, type_ref, roots, visible)
	if !ok {
		return INVALID_STRUCTURE_ID, false
	}
	path := type_ref.field_path[:]
	derefs := type_ref.field_derefs[:]
	selectors := type_ref.field_selectors[:]
	source_unit_index := unit_id_index(handle.unit)
	if source_unit_index < 0 || source_unit_index >= len(units) {
		return INVALID_STRUCTURE_ID, false
	}
	source_symbol := symbol(&units[source_unit_index], handle.symbol)
	if source_symbol != nil && (source_symbol.kind == .Class || source_symbol.kind == .Interface) {
		if len(path) == 0 {
			return INVALID_STRUCTURE_ID, false
		}
		nested, nested_ok := class_type_symbol_handle(units, handle, path[0])
		if !nested_ok {
			return INVALID_STRUCTURE_ID, false
		}
		handle = nested
		path = path[1:]
		if len(derefs) > 0 {
			derefs = derefs[1:]
		}
		if len(selectors) > 0 {
			selectors = selectors[1:]
		}
		source_unit_index = unit_id_index(handle.unit)
		if source_unit_index < 0 || source_unit_index >= len(units) {
			return INVALID_STRUCTURE_ID, false
		}
		source_symbol = symbol(&units[source_unit_index], handle.symbol)
	}
	if source_symbol == nil || source_symbol.structure == INVALID_STRUCTURE_ID {
		return INVALID_STRUCTURE_ID, false
	}
	imported := import_structure_to_unit(
		&units[unit_index],
		&units[source_unit_index],
		source_symbol.structure,
		allocator,
	)
	current := imported
	for field_name, i in path {
		if i < len(derefs) && derefs[i] {
			continue
		}
		if selector_at(selectors, i) != .Dash {
			return INVALID_STRUCTURE_ID, false
		}
		field := structure_field(&units[unit_index], current, field_name)
		if field == nil || field.structure == INVALID_STRUCTURE_ID {
			return INVALID_STRUCTURE_ID, false
		}
		current = field.structure
	}
	return current, true
}

project_structure_for_type_ref :: proc(
	units: []Unit_Analysis,
	unit_index: int,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [dynamic]Unit_Id,
	allocator: mem.Allocator,
) -> (
	Structure_Id,
	bool,
) {
	if len(type_ref.field_path) == 0 || selector_at(type_ref.field_selectors[:], 0) != .Arrow {
		return INVALID_STRUCTURE_ID, false
	}
	namespaces := [?]Namespace{.Type, .Value, .Routine}
	for namespace in namespaces {
		if !type_ref_namespace_matches(type_ref.namespace, namespace) {
			continue
		}
		symbol_id, ok := lookup_scope_chain(
			&units[unit_index],
			&units[unit_index].scope_index,
			scope_id,
			namespace,
			type_ref.base_name,
		)
		if !ok {
			continue
		}
		s := symbol(&units[unit_index], symbol_id)
		if s == nil || !s.has_declared_type || !s.declared_type.is_ref {
			continue
		}
		class_handle, class_ok := resolve_type_ref_handle_project(units, unit_index, s.declared_type, roots, visible)
		if !class_ok {
			continue
		}
		path_ok := true
		for name in s.declared_type.field_path {
			next, next_ok := class_type_symbol_handle(units, class_handle, name)
			if !next_ok {
				path_ok = false
				break
			}
			class_handle = next
		}
		if !path_ok {
			continue
		}
		class_unit_index := unit_id_index(class_handle.unit)
		if class_unit_index < 0 || class_unit_index >= len(units) {
			continue
		}
		class_symbol := symbol(&units[class_unit_index], class_handle.symbol)
		if class_symbol == nil || !(class_symbol.kind == .Class || class_symbol.kind == .Interface) {
			continue
		}
		member_handle, member_ok := class_member_symbol_by_handle(
			units,
			class_handle,
			.Value,
			type_ref.field_path[0],
			class_entries,
			true,
		)
		if !member_ok {
			continue
		}
		member_unit_index := unit_id_index(member_handle.unit)
		if member_unit_index < 0 || member_unit_index >= len(units) {
			continue
		}
		member_unit := &units[member_unit_index]
		member_symbol := symbol(member_unit, member_handle.symbol)
		if member_symbol == nil {
			continue
		}
		member_owner, owner_ok := enclosing_class_owner_unit(member_unit, member_symbol.scope)
		if !owner_ok {
			continue
		}
		member := unit_class_member(member_unit, member_owner, member_symbol.name)
		if member == nil || member.kind != .Attribute || member.structure == INVALID_STRUCTURE_ID {
			continue
		}
		structure_id := member.structure
		if len(type_ref.field_path) > 1 {
			next_selectors := type_ref.field_selectors[:]
			next_derefs := type_ref.field_derefs[:]
			if len(next_selectors) > 0 {next_selectors = next_selectors[1:]}
			if len(next_derefs) > 0 {next_derefs = next_derefs[1:]}
			nested, nested_ok := resolve_unit_structure_path(
				member_unit,
				member.structure,
				type_ref.field_path[1:],
				next_selectors,
				next_derefs,
			)
			if !nested_ok {
				continue
			}
			structure_id = nested
		}
		if member_unit_index == unit_index {
			return structure_id, true
		}
		return import_structure_to_unit(&units[unit_index], member_unit, structure_id, allocator), true
	}
	return INVALID_STRUCTURE_ID, false
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
		member := unit_class_member(unit, super_symbol, name)
		if member != nil && member.kind == .Attribute && member.visibility != .Private {
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
	member := unit_class_member(unit, class_symbol, path[0])
	if member == nil || member.kind != .Attribute || member.structure == INVALID_STRUCTURE_ID {
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

import_structure_to_unit :: proc(
	target, source: ^Unit_Analysis,
	source_structure_id: Structure_Id,
	allocator: mem.Allocator,
) -> Structure_Id {
	source_structure := structure(source, source_structure_id)
	if source_structure == nil {
		return INVALID_STRUCTURE_ID
	}
	for existing in target.structures {
		if existing.origin_unit == source_structure.origin_unit &&
		   existing.origin_structure == source_structure.origin_structure {
			return existing.id
		}
	}
	fields := make([dynamic]Structure_Field_Data, 0, len(source_structure.fields), allocator)
	id := Structure_Id(u32(len(target.structures)))
	append(
		&target.structures,
		Structure_Data {
			id = id,
			origin_unit = source_structure.origin_unit,
			origin_structure = source_structure.origin_structure,
			name = strings.clone(source_structure.name, allocator),
			fields = fields,
		},
	)
	for field in source_structure.fields {
		next := field
		next.name = strings.clone(field.name, allocator)
		next.decl_unit = field.decl_unit
		if field.structure != INVALID_STRUCTURE_ID {
			next.structure = import_structure_to_unit(target, source, field.structure, allocator)
		}
		append(&target.structures[structure_id_index(id)].fields, next)
	}
	return id
}
