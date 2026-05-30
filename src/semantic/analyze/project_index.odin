package abap_frontend_semantic_analyze

import "core:mem"

Project_Index :: struct {
	root_lookup:              Project_Root_Lookup,
	global_root_candidates:   map[Root_Name_Key][dynamic]Symbol_Handle,
	root_name_counts:         map[string]int,
	provided_name_counts:     map[string]int,
	class_scope_entries:      map[Project_Class_Member_Key]Project_Class_Member_Entry,
	class_scope_candidates:   map[Project_Class_Member_Key][dynamic]Project_Class_Scope_Index_Entry,
	validation_class_members: map[Class_Member_Lookup_Key]int,
	sql_predicate_columns:    map[Sql_Predicate_Column_Key]bool,
	dependency_pair_counts:   map[Reverse_Dependency_Key]int,
	unit_entries:             [dynamic]Project_Index_Unit,
	visible:                  [][dynamic]Unit_Id,
	predecessors:             [][dynamic]Unit_Id,
	allocator:                mem.Allocator,
}

Project_Index_Unit :: struct {
	roots:                        [dynamic]Root_Symbol_Entry,
	provided_names:               [dynamic]string,
	class_scope_entries:          [dynamic]Project_Class_Scope_Index_Entry,
	validation_class_member_keys: [dynamic]Class_Member_Lookup_Key,
	sql_predicate_column_keys:    [dynamic]Sql_Predicate_Column_Key,
	include_targets:              [dynamic]Unit_Id,
	dependency_edges:             [dynamic]Project_Dependency_Edge,
	unresolved_candidates:        [dynamic]Remote_Dependency_Key,
}

Project_Class_Scope_Index_Entry :: struct {
	unit:  Unit_Id,
	key:   Project_Class_Member_Key,
	entry: Project_Class_Member_Entry,
}

project_index_make :: proc(allocator: mem.Allocator) -> Project_Index {
	return Project_Index {
		root_lookup = Project_Root_Lookup {
			by_unit = make(map[Root_Symbol_Key]Symbol_Handle, 16, allocator),
			global = make(map[Root_Name_Key]Symbol_Handle, 16, allocator),
			names = make(map[string]bool, 16, allocator),
			provided_names = make(map[string]bool, 16, allocator),
		},
		root_name_counts = make(map[string]int, 16, allocator),
		global_root_candidates = make(map[Root_Name_Key][dynamic]Symbol_Handle, 16, allocator),
		provided_name_counts = make(map[string]int, 16, allocator),
		class_scope_entries = make(map[Project_Class_Member_Key]Project_Class_Member_Entry, 16, allocator),
		class_scope_candidates = make(map[Project_Class_Member_Key][dynamic]Project_Class_Scope_Index_Entry, 16, allocator),
		validation_class_members = make(map[Class_Member_Lookup_Key]int, 16, allocator),
		sql_predicate_columns = make(map[Sql_Predicate_Column_Key]bool, 16, allocator),
		dependency_pair_counts = make(map[Reverse_Dependency_Key]int, 16, allocator),
		unit_entries = make([dynamic]Project_Index_Unit, 0, 8, allocator),
		allocator = allocator,
	}
}

project_index_ensure_unit_count :: proc(index: ^Project_Index, unit_count: int) {
	for len(index.unit_entries) < unit_count {
		append(
			&index.unit_entries,
			Project_Index_Unit {
				roots = make([dynamic]Root_Symbol_Entry, 0, 8, index.allocator),
				provided_names = make([dynamic]string, 0, 4, index.allocator),
				class_scope_entries = make([dynamic]Project_Class_Scope_Index_Entry, 0, 8, index.allocator),
				validation_class_member_keys = make([dynamic]Class_Member_Lookup_Key, 0, 8, index.allocator),
				sql_predicate_column_keys = make([dynamic]Sql_Predicate_Column_Key, 0, 8, index.allocator),
				include_targets = make([dynamic]Unit_Id, 0, 2, index.allocator),
				dependency_edges = make([dynamic]Project_Dependency_Edge, 0, 8, index.allocator),
				unresolved_candidates = make([dynamic]Remote_Dependency_Key, 0, 8, index.allocator),
			},
		)
	}
}

project_index_update_units :: proc(
	index: ^Project_Index,
	units: []Unit_Analysis,
	unit_ids: []Unit_Id,
) {
	project_index_ensure_unit_count(index, len(units))
	for unit_id in unit_ids {
		unit_index := unit_id_index(unit_id)
		if unit_index < 0 || unit_index >= len(units) {
			continue
		}
		project_index_remove_unit(index, unit_id)
		project_index_collect_unit(index, &units[unit_index], unit_index)
	}
}

project_index_remove_unit :: proc(
	index: ^Project_Index,
	unit_id: Unit_Id,
) {
	unit_index := unit_id_index(unit_id)
	data := &index.unit_entries[unit_index]
	for entry in data.roots {
		delete_key(
			&index.root_lookup.by_unit,
			Root_Symbol_Key{unit = entry.unit, namespace = entry.namespace, name = entry.name},
		)
		if entry.visible_by_default {
			project_index_decrement_name_count(&index.root_name_counts, &index.root_lookup.names, entry.name)
			project_index_remove_global_root_candidate(
				index,
				Root_Name_Key{namespace = entry.namespace, name = entry.name},
				unit_id,
			)
		}
	}
	for name in data.provided_names {
		project_index_decrement_name_count(
			&index.provided_name_counts,
			&index.root_lookup.provided_names,
			name,
		)
	}
	for entry in data.class_scope_entries {
		project_index_remove_class_scope_candidate(index, entry.key, unit_id)
	}
	for key in data.validation_class_member_keys {
		delete_key(&index.validation_class_members, key)
	}
	for key in data.sql_predicate_column_keys {
		delete_key(&index.sql_predicate_columns, key)
	}
	clear(&data.roots)
	clear(&data.provided_names)
	clear(&data.class_scope_entries)
	clear(&data.validation_class_member_keys)
	clear(&data.sql_predicate_column_keys)
}

project_index_collect_unit :: proc(
	index: ^Project_Index,
	unit: ^Unit_Analysis,
	unit_index: int,
) {
	data := &index.unit_entries[unit_index]
	unit_stem := uri_file_stem(unit.uri)
	for name in unit.provided_names {
		append(&data.provided_names, name)
		project_index_increment_name_count(
			&index.provided_name_counts,
			&index.root_lookup.provided_names,
			name,
		)
	}
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
				visible_by_default = name_is_namespaced(symbol.name) ||
				                     root_name_matches_unit_stem(unit_stem, symbol.name)
			case .Type_Def:
				visible_by_default = root_name_matches_unit_stem(unit_stem, symbol.name)
			case .Module, .Report:
				visible_by_default = true
			}
		}
		namespaces := [?]Namespace{.Value, .Type, .Routine}
		for namespace in namespaces {
			if !symbol_kind_occupies(symbol.kind, namespace) {
				continue
			}
			entry := Root_Symbol_Entry {
				unit = unit.unit_id,
				symbol = symbol.id,
				namespace = namespace,
				name = symbol.name,
				visible_by_default = visible_by_default,
			}
			append(&data.roots, entry)
			key := Root_Symbol_Key{unit = entry.unit, namespace = entry.namespace, name = entry.name}
			_, slot, inserted, _ := map_entry(&index.root_lookup.by_unit, key)
			if inserted {
				slot^ = Symbol_Handle{unit = entry.unit, symbol = entry.symbol}
			}
			if entry.visible_by_default {
				project_index_increment_name_count(&index.root_name_counts, &index.root_lookup.names, entry.name)
				project_index_add_global_root_candidate(
					index,
					Root_Name_Key{namespace = entry.namespace, name = entry.name},
					Symbol_Handle{unit = entry.unit, symbol = entry.symbol},
				)
			}
		}
	}
	for symbol in unit.symbols {
		scope_data := scope(unit, symbol.scope)
		if scope_data == nil ||
		   !(scope_data.kind == .Class || scope_data.kind == .Interface) ||
		   scope_data.owner == INVALID_SYMBOL_ID {
			continue
		}
		namespaces := [?]Namespace{.Value, .Type, .Routine}
		for namespace in namespaces {
			if !symbol_kind_occupies(symbol.kind, namespace) {
				continue
			}
			key := Project_Class_Member_Key {
				class_unit = unit.unit_id,
				class_symbol = scope_data.owner,
				namespace = namespace,
				name = symbol.name,
			}
			append(
				&data.class_scope_entries,
				Project_Class_Scope_Index_Entry {
					unit = unit.unit_id,
					key = key,
					entry = Project_Class_Member_Entry{unit = unit.unit_id, symbol = symbol.id},
				},
			)
			project_index_add_class_scope_candidate(index, data.class_scope_entries[len(data.class_scope_entries) - 1])
		}
	}
	for member, i in unit.class_members {
		key := Class_Member_Lookup_Key {
			unit = unit.unit_id,
			class_symbol = member.class_symbol,
			name = member.name,
		}
		append(&data.validation_class_member_keys, key)
		_, slot, inserted, _ := map_entry(&index.validation_class_members, key)
		if inserted {
			slot^ = i
		}
	}
}

project_index_update_sql_predicate_columns :: proc(
	index: ^Project_Index,
	units: []Unit_Analysis,
	unit_ids: []Unit_Id,
) {
	project_index_ensure_unit_count(index, len(units))
	for unit_id in unit_ids {
		unit_index := unit_id_index(unit_id)
		if unit_index < 0 || unit_index >= len(units) {
			continue
		}
		data := &index.unit_entries[unit_index]
		for key in data.sql_predicate_column_keys {
			delete_key(&index.sql_predicate_columns, key)
		}
		clear(&data.sql_predicate_column_keys)
		for ref in units[unit_index].sql_name_refs {
			if ref.kind != .Column {
				continue
			}
			key := sql_predicate_column_key(unit_id, ref.range, ref.name)
			append(&data.sql_predicate_column_keys, key)
			index.sql_predicate_columns[key] = true
		}
	}
}

project_index_update_include_graph :: proc(
	index: ^Project_Index,
	units: []Unit_Analysis,
	unit_ids: []Unit_Id,
) {
	project_index_ensure_unit_count(index, len(units))
	rebuild := len(index.visible) != len(units) || len(index.predecessors) != len(units)
	for unit_id in unit_ids {
		unit_index := unit_id_index(unit_id)
		if unit_index < 0 || unit_index >= len(units) {
			continue
		}
		if project_index_include_targets_changed(&index.unit_entries[unit_index], &units[unit_index]) {
			rebuild = true
		}
	}
	if rebuild {
		project_index_rebuild_include_graph(index, units)
	}
	project_index_rebuild_class_scope_index(index, units)
}

project_index_include_targets_changed :: proc(data: ^Project_Index_Unit, unit: ^Unit_Analysis) -> bool {
	changed := len(data.include_targets) != len(unit.include_edges)
	if !changed {
		for edge, i in unit.include_edges {
			target := edge.target if edge.has_target else INVALID_UNIT_ID
			if data.include_targets[i] != target {
				changed = true
				break
			}
		}
	}
	if changed {
		clear(&data.include_targets)
		for edge in unit.include_edges {
			append(&data.include_targets, edge.target if edge.has_target else INVALID_UNIT_ID)
		}
	}
	return changed
}

project_index_rebuild_include_graph :: proc(index: ^Project_Index, units: []Unit_Analysis) {
	project_index_destroy_include_graph(index)
	index.visible = include_visible_units_for_units(units, index.allocator)
	index.predecessors = include_predecessor_units_for_units(units, index.allocator)
}

project_index_destroy_include_graph :: proc(index: ^Project_Index) {
	for item in index.visible {
		delete(item)
	}
	for item in index.predecessors {
		delete(item)
	}
	if len(index.visible) > 0 {
		delete(index.visible, index.allocator)
	}
	if len(index.predecessors) > 0 {
		delete(index.predecessors, index.allocator)
	}
	index.visible = nil
	index.predecessors = nil
}

project_index_rebuild_class_scope_index :: proc(index: ^Project_Index, units: []Unit_Analysis) {
	delete(index.class_scope_entries)
	delete(index.class_scope_candidates)
	index.class_scope_entries = make(map[Project_Class_Member_Key]Project_Class_Member_Entry, 16, index.allocator)
	index.class_scope_candidates = make(map[Project_Class_Member_Key][dynamic]Project_Class_Scope_Index_Entry, 16, index.allocator)
	for i in 0 ..< len(units) {
		clear(&index.unit_entries[i].class_scope_entries)
	}
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
					project_index_record_class_scope_entry(
						index,
						unit.unit_id,
						Project_Class_Member_Key {
							class_unit   = unit.unit_id,
							class_symbol = scope_data.owner,
							namespace    = namespace,
							name         = symbol.name,
						},
						Project_Class_Member_Entry{unit = unit.unit_id, symbol = symbol.id},
					)
				}
			}
		}
	}
	changed := true
	for changed {
		changed = false
		for unit, unit_index in units {
			if unit_index >= len(index.visible) {
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
					&index.root_lookup,
					index.visible[unit_index],
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
					if alias_key in index.class_scope_entries {
						continue
					}
					if target_entry, target_ok := index.class_scope_entries[target_key]; target_ok {
						project_index_record_class_scope_entry(index, unit.unit_id, alias_key, target_entry)
						changed = true
					}
				}
			}
		}
	}
}

project_index_record_class_scope_entry :: proc(
	index: ^Project_Index,
	owner_unit: Unit_Id,
	key: Project_Class_Member_Key,
	entry: Project_Class_Member_Entry,
) {
	unit_index := unit_id_index(owner_unit)
	if unit_index < 0 || unit_index >= len(index.unit_entries) {
		return
	}
	data := &index.unit_entries[unit_index]
	append(
		&data.class_scope_entries,
		Project_Class_Scope_Index_Entry {
			unit = owner_unit,
			key = key,
			entry = entry,
		},
	)
	project_index_add_class_scope_candidate(index, data.class_scope_entries[len(data.class_scope_entries) - 1])
}

validation_lookup_from_project_index :: proc(index: ^Project_Index) -> Validation_Lookup {
	return Validation_Lookup {
		visible = index.visible,
		predecessors = index.predecessors,
		root_by_unit = index.root_lookup.by_unit,
		global_roots = index.root_lookup.global,
		class_members = index.validation_class_members,
		sql_predicate_columns = index.sql_predicate_columns,
	}
}

project_index_add_global_root_candidate :: proc(
	index: ^Project_Index,
	key: Root_Name_Key,
	handle: Symbol_Handle,
) {
	if candidates, ok := index.global_root_candidates[key]; ok {
		insert := len(candidates)
		for candidate, i in candidates {
			if unit_id_index(handle.unit) < unit_id_index(candidate.unit) {
				insert = i
				break
			}
		}
		append(&candidates, handle)
		for i := len(candidates) - 1; i > insert; i -= 1 {
			candidates[i] = candidates[i - 1]
		}
		candidates[insert] = handle
		index.global_root_candidates[key] = candidates
	} else {
		next := make([dynamic]Symbol_Handle, 0, 2, index.allocator)
		append(&next, handle)
		index.global_root_candidates[key] = next
	}
	index.root_lookup.global[key] = index.global_root_candidates[key][0]
}

project_index_remove_global_root_candidate :: proc(
	index: ^Project_Index,
	key: Root_Name_Key,
	unit_id: Unit_Id,
) {
	candidates, ok := index.global_root_candidates[key]
	if !ok {
		return
	}
	write := 0
	for candidate in candidates {
		if candidate.unit == unit_id {
			continue
		}
		candidates[write] = candidate
		write += 1
	}
	if write == 0 {
		delete(candidates)
		delete_key(&index.global_root_candidates, key)
		delete_key(&index.root_lookup.global, key)
		return
	}
	resize(&candidates, write)
	index.global_root_candidates[key] = candidates
	index.root_lookup.global[key] = candidates[0]
}

project_index_add_class_scope_candidate :: proc(
	index: ^Project_Index,
	entry: Project_Class_Scope_Index_Entry,
) {
	if candidates, ok := index.class_scope_candidates[entry.key]; ok {
		insert := len(candidates)
		for candidate, i in candidates {
			if unit_id_index(entry.unit) < unit_id_index(candidate.unit) {
				insert = i
				break
			}
		}
		append(&candidates, entry)
		for i := len(candidates) - 1; i > insert; i -= 1 {
			candidates[i] = candidates[i - 1]
		}
		candidates[insert] = entry
		index.class_scope_candidates[entry.key] = candidates
	} else {
		next := make([dynamic]Project_Class_Scope_Index_Entry, 0, 2, index.allocator)
		append(&next, entry)
		index.class_scope_candidates[entry.key] = next
	}
	index.class_scope_entries[entry.key] = index.class_scope_candidates[entry.key][0].entry
}

project_index_remove_class_scope_candidate :: proc(
	index: ^Project_Index,
	key: Project_Class_Member_Key,
	unit_id: Unit_Id,
) {
	candidates, ok := index.class_scope_candidates[key]
	if !ok {
		return
	}
	write := 0
	for candidate in candidates {
		if candidate.unit == unit_id {
			continue
		}
		candidates[write] = candidate
		write += 1
	}
	if write == 0 {
		delete(candidates)
		delete_key(&index.class_scope_candidates, key)
		delete_key(&index.class_scope_entries, key)
		return
	}
	resize(&candidates, write)
	index.class_scope_candidates[key] = candidates
	index.class_scope_entries[key] = candidates[0].entry
}

project_index_increment_name_count :: proc(
	counts: ^map[string]int,
	presence: ^map[string]bool,
	name: string,
) {
	if count, ok := counts^[name]; ok {
		counts^[name] = count + 1
	} else {
		counts^[name] = 1
		presence^[name] = true
	}
}

project_index_decrement_name_count :: proc(
	counts: ^map[string]int,
	presence: ^map[string]bool,
	name: string,
) {
	count, ok := counts^[name]
	if !ok {
		return
	}
	if count <= 1 {
		delete_key(counts, name)
		delete_key(presence, name)
	} else {
		counts^[name] = count - 1
	}
}
