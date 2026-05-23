package abap_frontend_semantic

import runtime "../runtime"
import "../parser"

import "core:mem"
import "core:strings"

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
	scope_count:   int,
	symbols:       map[Scope_Index_Key]Symbol_Id,
	class_symbols: map[Class_Scope_Index_Key]Symbol_Id,
}

scope_index_make :: proc(allocator: mem.Allocator) -> Scope_Index {
	return Scope_Index {
		symbols = make(map[Scope_Index_Key]Symbol_Id, 0, allocator),
		class_symbols = make(map[Class_Scope_Index_Key]Symbol_Id, 0, allocator),
	}
}

build_scope_index :: proc(unit: ^Unit_Analysis, allocator: mem.Allocator) -> Scope_Index {
	index := Scope_Index {
		scope_count = len(unit.scopes),
		symbols = make(map[Scope_Index_Key]Symbol_Id, len(unit.symbols) * 2, allocator),
		class_symbols = make(map[Class_Scope_Index_Key]Symbol_Id, len(unit.symbols), allocator),
	}
	for symbol in unit.symbols {
		class_symbol := INVALID_SYMBOL_ID
		if scope_data := scope(unit, symbol.scope); scope_data != nil &&
		   (scope_data.kind == .Class || scope_data.kind == .Interface) &&
		   scope_data.owner != INVALID_SYMBOL_ID {
			class_symbol = scope_data.owner
		}
		namespaces := [?]Namespace{.Value, .Type, .Routine}
		for namespace in namespaces {
			if symbol_kind_occupies(symbol.kind, namespace) {
				index.symbols[Scope_Index_Key {
					scope = symbol.scope,
					namespace = namespace,
					name = symbol.name,
				}] = symbol.id
				if class_symbol != INVALID_SYMBOL_ID {
					index.class_symbols[Class_Scope_Index_Key {
						class_symbol = class_symbol,
						namespace = namespace,
						name = symbol.name,
					}] = symbol.id
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
	allocator: mem.Allocator,
) -> Unit_Analysis {
	unit := collect_unit(unit_id, uri, source, parsed, allocator)
	resolve_unit_locally(&unit, allocator)
	units := make([dynamic]Unit_Analysis, 0, 1, allocator)
	append(&units, unit)
	project := project_analysis_from_units(units, allocator)
	pool: runtime.Pool
	assert(runtime.pool_init(&pool, runtime.Options{worker_count = 0, task_capacity = 64}, allocator) == .None)
	defer runtime.pool_destroy(&pool)
	finish_project_analysis(&project, &pool, allocator)
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
	if symbol_id, ok := resolve_current_class_member(unit, index, ref.scope, ref.namespace, ref.name);
	   ok {
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
			key := Scope_Index_Key{scope = current, namespace = namespace, name = name}
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
	if symbol_id, ok := index.class_symbols[Class_Scope_Index_Key {
		class_symbol = class_symbol,
		namespace = namespace,
		name = name,
	}]; ok {
		return symbol_id, true
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
	class_symbol, ok := enclosing_class_owner_unit(unit, scope_id)
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

is_builtin_type_name :: proc(name: string) -> bool {
	lower := strings.trim_space(name)
	if strings.equal_fold(lower, "any table") {
		return true
	}
	for builtin in BUILTIN_SCALAR_TYPES {
		if strings.equal_fold(lower, builtin) {
			return true
		}
	}
	if len(lower) <= 4 || !strings.equal_fold(lower[:4], "char") {
		return false
	}
	for i in 4 ..< len(lower) {
		if lower[i] < '0' || lower[i] > '9' {
			return false
		}
	}
	return true
}

Root_Symbol_Entry :: struct {
	unit:      Unit_Id,
	symbol:    Symbol_Id,
	namespace: Namespace,
	name:      string,
}

Project_Root_Lookup :: struct {
	by_unit: map[Root_Symbol_Key]Symbol_Handle,
	global:  map[Root_Name_Key]Symbol_Handle,
	names:   map[string]bool,
}

Project_Class_Member_Key :: struct {
	class_symbol: Symbol_Id,
	namespace:    Namespace,
	name:         string,
}

Project_Class_Member_Entry :: struct {
	symbol: Symbol_Id,
}

resolve_project_cross_unit :: proc(units: []Unit_Analysis, allocator: mem.Allocator) {
	if len(units) == 0 {
		return
	}
	roots := build_project_root_index(units, allocator)
	root_lookup := build_project_root_lookup(units, roots[:], allocator)
	class_entries := build_project_class_scope_index(units, allocator)
	visible := include_visible_units_for_units(units, allocator)
	predecessors := include_predecessor_units_for_units(units, allocator)

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

	if seed_inherited_method_scope_parameters(units, &root_lookup, class_entries, visible, predecessors, allocator) {
		for unit_index in 0 ..< len(units) {
			units[unit_index].scope_index = build_scope_index(&units[unit_index], allocator)
			resolve_unit_with_index(&units[unit_index], &units[unit_index].scope_index)
		}
	}

	for unit_index in 0 ..< len(units) {
		import_project_structures_for_unit(units, unit_index, &root_lookup, visible[unit_index], allocator)
	}
}

seed_inherited_method_scope_parameters :: proc(
	units: []Unit_Analysis,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [] [dynamic]Unit_Id,
	predecessors: [] [dynamic]Unit_Id,
	allocator: mem.Allocator,
) -> bool {
	changed := false
	for unit_index in 0 ..< len(units) {
		unit := &units[unit_index]
		symbol_count := len(unit.symbols)
		for symbol_index in 0 ..< symbol_count {
			method_symbol := unit.symbols[symbol_index]
			if method_symbol.kind != .Method {
				continue
			}
			method_scope, scope_ok := method_scope_for_owner(unit, method_symbol.id)
			if !scope_ok {
				continue
			}
			member := method_signature_member_for_scope(
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
			for param in member.parameters {
				if method_scope_has_value_symbol(unit, method_scope, param.name) {
					continue
				}
				_ = declare_symbol(
					unit,
					method_scope,
					param.name,
					.Parameter,
					method_symbol.decl_range,
					INVALID_STRUCTURE_ID,
					param.declared_type,
					.Has_Declared_Type in param.flags,
					param.type_clause_display,
				)
				changed = true
			}
		}
	}
	return changed
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
) -> ^Class_Member_Data {
	if interface_name, member_name, qualified := qualified_method_parts(method_name); qualified {
		if member, ok := exposed_interface_member_for_scope(
			units,
			unit_index,
			scope_id,
			interface_name,
			member_name,
			roots,
			visible,
		); ok {
			return member
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
		return nil
	}
	member_unit_index := unit_id_index(member_handle.unit)
	if member_unit_index < 0 || member_unit_index >= len(units) {
		return nil
	}
	member_unit := &units[member_unit_index]
	member_symbol := symbol(member_unit, member_handle.symbol)
	if member_symbol == nil {
		return nil
	}
	class_symbol, class_ok := enclosing_class_owner_unit(member_unit, member_symbol.scope)
	if !class_ok {
		return nil
	}
	member := unit_class_member(member_unit, class_symbol, method_name)
	if member == nil || len(member.parameters) > 0 || !(.Is_Redefinition in member.flags) {
		return member
	}
	if inherited, inherited_ok := inherited_project_class_member(
		units,
		Symbol_Handle{unit = member_handle.unit, symbol = class_symbol},
		method_name,
		roots,
		class_entries,
		visible,
	); inherited_ok {
		return inherited
	}
	return member
}

exposed_interface_member_for_scope :: proc(
	units: []Unit_Analysis,
	unit_index: int,
	scope_id: Scope_Id,
	interface_name, member_name: string,
	roots: ^Project_Root_Lookup,
	visible: [dynamic]Unit_Id,
) -> (^Class_Member_Data, bool) {
	class_symbol, class_ok := enclosing_class_owner_unit(&units[unit_index], scope_id)
	if !class_ok {
		return nil, false
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
		return nil, false
	}
	interface_unit_index := unit_id_index(handle.unit)
	if interface_unit_index < 0 || interface_unit_index >= len(units) {
		return nil, false
	}
	member := unit_class_member(&units[interface_unit_index], handle.symbol, member_name)
	return member, member != nil
}

exposed_interface_handle :: proc(
	units: []Unit_Analysis,
	owner: Symbol_Handle,
	interface_name: string,
	roots: ^Project_Root_Lookup,
	visible: [dynamic]Unit_Id,
	depth: int,
) -> (Symbol_Handle, bool) {
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
	if owner_symbol := symbol(unit, owner.symbol); owner_symbol != nil && owner_symbol.kind == .Class {
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
) -> (^Class_Member_Data, bool) {
	current := class_handle
	for _ in 0 ..< len(units) + 8 {
		next, ok := direct_superclass_handle(units, current, roots, visible)
		if !ok {
			return nil, false
		}
		if _, member_ok := class_member_symbol_by_handle(
			units,
			next,
			.Routine,
			name,
			class_entries,
			true,
		); member_ok {
			next_index := unit_id_index(next.unit)
			if next_index >= 0 && next_index < len(units) {
				if member := unit_class_member(&units[next_index], next.symbol, name); member != nil {
					return member, true
				}
			}
		}
		current = next
	}
	return nil, false
}

method_scope_for_owner :: proc(unit: ^Unit_Analysis, owner: Symbol_Id) -> (Scope_Id, bool) {
	for s in unit.scopes {
		if s.kind == .Method && s.owner == owner {
			return s.id, true
		}
	}
	return INVALID_SCOPE_ID, false
}

method_scope_has_value_symbol :: proc(unit: ^Unit_Analysis, scope_id: Scope_Id, name: string) -> bool {
	if s := scope(unit, scope_id); s != nil {
		for symbol_id in s.declarations {
			if item := symbol(unit, symbol_id); item != nil && item.name == name &&
			   symbol_kind_occupies(item.kind, .Value) {
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
	for unit in units {
		for symbol in unit.symbols {
			if symbol.scope != unit.root_scope {
				continue
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
	lookup := Project_Root_Lookup {
		by_unit = make(map[Root_Symbol_Key]Symbol_Handle, len(roots), allocator),
		global = make(map[Root_Name_Key]Symbol_Handle, len(roots), allocator),
		names = make(map[string]bool, len(roots), allocator),
	}
	for entry in roots {
		handle := Symbol_Handle{unit = entry.unit, symbol = entry.symbol}
		unit_key := Root_Symbol_Key{unit = entry.unit, namespace = entry.namespace, name = entry.name}
		if !(unit_key in lookup.by_unit) {
			lookup.by_unit[unit_key] = handle
		}
		unit := &units[unit_id_index(entry.unit)]
		s := symbol(unit, entry.symbol)
		global_key := Root_Name_Key{namespace = entry.namespace, name = entry.name}
		if s != nil && root_symbol_visible_by_default(unit, s^) {
			lookup.names[entry.name] = true
			if !(global_key in lookup.global) {
				lookup.global[global_key] = handle
			}
		}
	}
	return lookup
}

build_project_class_scope_index :: proc(
	units: []Unit_Analysis,
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
						class_symbol = scope_data.owner,
						namespace = namespace,
						name = symbol.name,
					}
					if key in out {
						continue
					}
					out[key] = Project_Class_Member_Entry {
						symbol = symbol.id,
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
		if handle, ok := root_symbol_in_visible_units(namespace, ref.name, roots, visible);
		   ok {
			return Resolution{kind = .Symbol, symbol = handle}, true
		}
		if handle, ok := global_visible_root_symbol(roots, namespace, ref.name); ok {
			return Resolution{kind = .Symbol, symbol = handle}, true
		}
	}
	if ref.kind == .Message_Class && provided_name_exists(units, ref.name) {
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
) -> (Symbol_Handle, bool) {
	class_symbol, ok := enclosing_class_owner_unit(&units[unit_index], scope_id)
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
) -> (Symbol_Handle, bool) {
	current, ok := enclosing_class_owner_unit(&units[unit_index], scope_id)
	if !ok {
		return {}, false
	}
	current_handle := Symbol_Handle{unit = units[unit_index].unit_id, symbol = current}
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
) -> (Symbol_Handle, bool) {
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
) -> (Symbol_Handle, bool) {
	if handle, ok := root_symbol_in_unit(roots, units[unit_index].unit_id, .Type, name);
	   ok {
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
) -> (Symbol_Handle, bool) {
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
) -> (Symbol_Handle, bool) {
	class_handle, ok := root_symbol_in_unit(roots, unit_id, .Type, class_name)
	if !ok || !unit_has_class_definition(&units[unit_id_index(unit_id)], class_handle.symbol) {
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
) -> (Symbol_Handle, bool) {
	unit_index := unit_id_index(class_handle.unit)
	if unit_index < 0 || unit_index >= len(units) {
		return {}, false
	}
	key := Project_Class_Member_Key {
		class_symbol = class_handle.symbol,
		namespace = namespace,
		name = name,
	}
	if entry, ok := class_entries[key]; ok {
		if inherited {
			member := unit_class_member(&units[unit_index], class_handle.symbol, name)
			if member != nil && member.visibility == .Private {
				return {}, false
			}
		}
		return Symbol_Handle{unit = class_handle.unit, symbol = entry.symbol}, true
	}
	return {}, false
}

root_symbol_in_visible_units :: proc(
	namespace: Namespace,
	name: string,
	roots: ^Project_Root_Lookup,
	visible: [dynamic]Unit_Id,
) -> (Symbol_Handle, bool) {
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
) -> (Symbol_Handle, bool) {
	key := Root_Symbol_Key{unit = unit_id, namespace = namespace, name = name}
	if handle, ok := roots.by_unit[key]; ok {
		return handle, true
	}
	return {}, false
}

global_visible_root_symbol :: proc(
	roots: ^Project_Root_Lookup,
	namespace: Namespace,
	name: string,
) -> (Symbol_Handle, bool) {
	key := Root_Name_Key{namespace = namespace, name = name}
	if handle, ok := roots.global[key]; ok {
		return handle, true
	}
	return {}, false
}

root_symbol_visible_by_default :: proc(unit: ^Unit_Analysis, s: Symbol_Data) -> bool {
	#partial switch s.kind {
	case .Class, .Interface:
		return root_name_matches_unit_stem(unit.uri, s.name) || name_is_namespaced(s.name)
	case .Type_Def:
		return root_name_matches_unit_stem(unit.uri, s.name)
	case .Module, .Report:
		return true
	case:
		return false
	}
}

root_name_matches_unit_stem :: proc(uri, name: string) -> bool {
	stem := uri_file_stem(uri)
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
	return component_start > 0 && component != "" && len(stem) >= len(component) &&
	       strings.equal_fold(stem[len(stem) - len(component):], component)
}

name_is_namespaced :: proc(name: string) -> bool {
	return len(name) > 0 && name[0] == '/'
}

provided_name_exists :: proc(units: []Unit_Analysis, name: string) -> bool {
	for unit in units {
		for provided in unit.provided_names {
			if provided == name {
				return true
			}
		}
	}
	return false
}

include_visible_units_for_units :: proc(
	units: []Unit_Analysis,
	allocator: mem.Allocator,
) -> [] [dynamic]Unit_Id {
	out := make([][dynamic]Unit_Id, len(units), allocator)
	for i in 0 ..< len(units) {
		out[i] = make([dynamic]Unit_Id, 0, len(units), allocator)
	}
	for unit in units {
		expansion := make([dynamic]Unit_Id, 0, len(units), allocator)
		stack := make([dynamic]Unit_Id, 0, len(units), allocator)
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
) -> [] [dynamic]Unit_Id {
	out := make([][dynamic]Unit_Id, len(units), allocator)
	for i in 0 ..< len(units) {
		out[i] = make([dynamic]Unit_Id, 0, len(units), allocator)
	}
	for unit in units {
		stack := make([dynamic]Unit_Id, 0, len(units), allocator)
		prior := make([dynamic]Unit_Id, 0, len(units), allocator)
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
	expansion := make([dynamic]Unit_Id, 0, len(units), allocator)
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
	visible: [dynamic]Unit_Id,
	allocator: mem.Allocator,
) {
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
				visible,
				allocator,
			); ok {
				s.structure = structure_id
				changed = true
			}
		}
		for structure_index := 0; structure_index < len(units[unit_index].structures); structure_index += 1 {
			owner_scope := structure_owner_scope(&units[unit_index], Structure_Id(u32(structure_index)))
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
					visible,
					allocator,
				); ok {
					units[unit_index].structures[structure_index].fields[field_index].structure = structure_id
					changed = true
				}
			}
		}
	}
}

import_structure_for_type_ref :: proc(
	units: []Unit_Analysis,
	unit_index: int,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
	roots: ^Project_Root_Lookup,
	visible: [dynamic]Unit_Id,
	allocator: mem.Allocator,
) -> (Structure_Id, bool) {
	if type_ref.base_name == "" || is_builtin_type_name(type_ref.base_name) {
		return INVALID_STRUCTURE_ID, false
	}
	if structure_id, ok := local_structure_for_type_ref(&units[unit_index], scope_id, type_ref); ok {
		return structure_id, true
	}
	handle, ok := resolve_type_ref_handle_project(units, unit_index, type_ref, roots, visible)
	if !ok {
		return INVALID_STRUCTURE_ID, false
	}
	path := type_ref.field_path[:]
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
	for field_name in path {
		field := structure_field(&units[unit_index], current, field_name)
		if field == nil || field.structure == INVALID_STRUCTURE_ID {
			return current, true
		}
		current = field.structure
	}
	return current, true
}

local_structure_for_type_ref :: proc(
	unit: ^Unit_Analysis,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
) -> (Structure_Id, bool) {
	namespaces := [?]Namespace{.Type, .Value, .Routine}
	for namespace in namespaces {
		if !(namespace == type_ref.namespace ||
		     (type_ref.namespace == .Type && namespace == .Value)) {
			continue
		}
		symbol_id, ok := lookup_scope_chain(unit, &unit.scope_index, scope_id, namespace, type_ref.base_name)
		if !ok {
			continue
		}
		s := symbol(unit, symbol_id)
		path := type_ref.field_path[:]
		if s != nil && (s.kind == .Class || s.kind == .Interface) {
			if len(path) == 0 {
				return INVALID_STRUCTURE_ID, false
			}
			nested, nested_ok := class_type_symbol_handle_in_unit(unit, symbol_id, path[0])
			if !nested_ok {
				return INVALID_STRUCTURE_ID, false
			}
			symbol_id = nested
			s = symbol(unit, symbol_id)
			path = path[1:]
		}
		if s == nil || s.structure == INVALID_STRUCTURE_ID {
			continue
		}
		return resolve_unit_structure_path(unit, s.structure, path)
	}
	return INVALID_STRUCTURE_ID, false
}

resolve_unit_structure_path :: proc(
	unit: ^Unit_Analysis,
	start: Structure_Id,
	path: []string,
) -> (Structure_Id, bool) {
	current := start
	for field_name in path {
		field := structure_field(unit, current, field_name)
		if field == nil || field.structure == INVALID_STRUCTURE_ID {
			return INVALID_STRUCTURE_ID, false
		}
		current = field.structure
	}
	return current, true
}

class_type_symbol_handle :: proc(
	units: []Unit_Analysis,
	class_handle: Symbol_Handle,
	name: string,
) -> (Symbol_Handle, bool) {
	unit_index := unit_id_index(class_handle.unit)
	if unit_index < 0 || unit_index >= len(units) {
		return {}, false
	}
	if symbol_id, ok := class_type_symbol_handle_in_unit(&units[unit_index], class_handle.symbol, name);
	   ok {
		return Symbol_Handle{unit = class_handle.unit, symbol = symbol_id}, true
	}
	return {}, false
}

class_type_symbol_handle_in_unit :: proc(
	unit: ^Unit_Analysis,
	class_symbol: Symbol_Id,
	name: string,
) -> (Symbol_Id, bool) {
	key := Class_Scope_Index_Key{class_symbol = class_symbol, namespace = .Type, name = name}
	if symbol_id, ok := unit.scope_index.class_symbols[key]; ok {
		return symbol_id, true
	}
	return INVALID_SYMBOL_ID, false
}

structure_owner_scope :: proc(unit: ^Unit_Analysis, id: Structure_Id) -> Scope_Id {
	for s in unit.symbols {
		if s.structure == id {
			return s.scope
		}
	}
	return unit.root_scope
}

resolve_type_ref_handle_project :: proc(
	units: []Unit_Analysis,
	unit_index: int,
	type_ref: Field_Type_Ref_Data,
	roots: ^Project_Root_Lookup,
	visible: [dynamic]Unit_Id,
) -> (Symbol_Handle, bool) {
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
		if handle, ok := global_visible_root_symbol(roots, namespace, type_ref.base_name);
		   ok {
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
