package abap_frontend_semantic_analyze

import "core:mem"
import "core:strings"

Root_Name_Key :: struct {
	namespace: Namespace,
	name:      string,
}

Project_Index :: struct {
	root_lookup:            Project_Root_Lookup,
	global_root_candidates: map[Root_Name_Key][dynamic]Symbol_Link,
	provided_name_counts:   map[string]int,
	class_scope_entries:    map[Project_Class_Member_Key]Project_Class_Member_Entry,
	class_scope_candidates: map[Project_Class_Member_Key][dynamic]Project_Class_Scope_Index_Entry,
	names:                  map[string]string,
	source_file_entries:    [dynamic]Project_Index_Source_File,
	visible:                [][dynamic]Source_File_Id,
	predecessors:           [][dynamic]Source_File_Id,
	allocator:              mem.Allocator,
}

Project_Index_Source_File :: struct {
	roots:               [dynamic]Root_Symbol_Entry,
	provided_names:      [dynamic]string,
	exports:             [dynamic]string,
	class_scope_entries: [dynamic]Project_Class_Scope_Index_Entry,
	include_targets:     [dynamic]Source_File_Id,
	role:                Source_File_Role,
}

Project_Class_Scope_Index_Entry :: struct {
	unit:  Source_File_Id,
	key:   Project_Class_Member_Key,
	entry: Project_Class_Member_Entry,
}

project_index_make :: proc(allocator: mem.Allocator) -> Project_Index {
	return Project_Index {
		root_lookup = Project_Root_Lookup {
			global = make(map[Root_Name_Key]Symbol_Link, 16, allocator),
			summary_global = make(map[Root_Name_Key]Entity_Handle, 16, allocator),
			provided_names = make(map[string]bool, 16, allocator),
		},
		global_root_candidates = make(map[Root_Name_Key][dynamic]Symbol_Link, 16, allocator),
		provided_name_counts = make(map[string]int, 16, allocator),
		class_scope_entries = make(
			map[Project_Class_Member_Key]Project_Class_Member_Entry,
			16,
			allocator,
		),
		class_scope_candidates = make(
			map[Project_Class_Member_Key][dynamic]Project_Class_Scope_Index_Entry,
			16,
			allocator,
		),
		names = make(map[string]string, 128, allocator),
		source_file_entries = make([dynamic]Project_Index_Source_File, 0, 8, allocator),
		allocator = allocator,
	}
}

project_index_name :: proc(index: ^Project_Index, name: string) -> string {
	if name == "" {
		return ""
	}
	if interned, ok := index.names[name]; ok {
		return interned
	}
	interned := strings.clone(name, index.allocator)
	index.names[interned] = interned
	return interned
}

project_index_from_units :: proc(
	units: []Source_File_Provider,
	allocator: mem.Allocator,
) -> Project_Index {
	index := project_index_make(allocator)
	source_file_ids := make([dynamic]Source_File_Id, 0, len(units), context.temp_allocator)
	for unit in units {
		append(&source_file_ids, unit.source_file_id)
	}
	project_index_update_units(&index, units, source_file_ids[:])
	project_index_update_include_graph(&index, units, source_file_ids[:])
	return index
}

project_index_from_project :: proc(
	project: ^Project_Analysis,
	allocator: mem.Allocator,
) -> Project_Index {
	if project == nil {
		return project_index_make(allocator)
	}
	index := project_index_from_units(project.providers.source_files[:], allocator)
	project_index_update_summaries(&index, project.providers.summaries)
	return index
}

project_index_update_summaries :: proc(
	index: ^Project_Index,
	summaries: []Summary_Provider_Input,
) {
	if index.root_lookup.summary_global == nil {
		index.root_lookup.summary_global = make(
			map[Root_Name_Key]Entity_Handle,
			16,
			index.allocator,
		)
	}
	clear(&index.root_lookup.summary_global)
	for &summary, summary_index in summaries {
		provider := provider_handle_for_dependency_summary(Provider_Id(u32(summary_index)))
		for export, export_index in summary.exports {
			namespaces := [?]Namespace{.Value, .Type, .Routine}
			for namespace in namespaces {
				if summary_provider_export_occupies(export.kind, namespace) {
					project_index_add_summary_root(
						index,
						namespace,
						export.name,
						Entity_Handle {
							provider = provider,
							id = Entity_Id(Symbol_Id(u32(export_index))),
						},
					)
				}
			}
		}
		for class, class_index in summary.classes {
			project_index_add_summary_root(
				index,
				.Type,
				class.name,
				Entity_Handle{provider = provider, id = Entity_Id(Symbol_Id(u32(class_index)))},
			)
		}
		for function, function_index in summary.functions {
			project_index_add_summary_root(
				index,
				.Routine,
				function.name,
				Entity_Handle{provider = provider, id = Entity_Id(Symbol_Id(u32(function_index)))},
			)
		}
		for typ, type_index in summary.types {
			project_index_add_summary_root(
				index,
				.Type,
				typ.name,
				Entity_Handle{provider = provider, id = Entity_Id(Symbol_Id(u32(type_index)))},
			)
		}
		for symbol_name, symbol_index in summary.type_pool_symbols {
			kind := dependency_summary_typepool_symbol_kind(summary, symbol_name)
			namespaces := [?]Namespace{.Value, .Type, .Routine}
			for namespace in namespaces {
				if symbol_kind_occupies(kind, namespace) {
					project_index_add_summary_root(
						index,
						namespace,
						symbol_name,
						Entity_Handle {
							provider = provider,
							id = Entity_Id(Symbol_Id(u32(symbol_index))),
						},
					)
				}
			}
		}
		for provided in summary.provided_names {
			if entity, ok := summary_provider_entity_lookup(&summary, .Type, provided); ok {
				project_index_add_summary_root(
					index,
					.Type,
					provided,
					Entity_Handle{provider = provider, id = entity},
				)
			}
		}
		if summary.object_name != "" {
			if entity, ok := summary_provider_entity_lookup(&summary, .Type, summary.object_name);
			   ok {
				project_index_add_summary_root(
					index,
					.Type,
					summary.object_name,
					Entity_Handle{provider = provider, id = entity},
				)
			}
		}
	}
}

project_index_ensure_source_file_count :: proc(index: ^Project_Index, source_file_count: int) {
	for len(index.source_file_entries) < source_file_count {
		append(
			&index.source_file_entries,
			Project_Index_Source_File {
				roots = make([dynamic]Root_Symbol_Entry, 0, 8, index.allocator),
				provided_names = make([dynamic]string, 0, 4, index.allocator),
				exports = make([dynamic]string, 0, 8, index.allocator),
				class_scope_entries = make(
					[dynamic]Project_Class_Scope_Index_Entry,
					0,
					8,
					index.allocator,
				),
				include_targets = make([dynamic]Source_File_Id, 0, 2, index.allocator),
			},
		)
	}
}

project_index_update_units :: proc(
	index: ^Project_Index,
	units: []Source_File_Provider,
	source_file_ids: []Source_File_Id,
) {
	project_index_ensure_source_file_count(index, len(units))
	for source_file_id in source_file_ids {
		source_file_index := source_file_id_index(source_file_id)
		if source_file_index < 0 || source_file_index >= len(units) {
			continue
		}
		project_index_remove_unit(index, source_file_id)
		project_index_collect_source_file(index, &units[source_file_index], source_file_index)
	}
}

project_index_remove_unit :: proc(index: ^Project_Index, source_file_id: Source_File_Id) {
	source_file_index := source_file_id_index(source_file_id)
	data := &index.source_file_entries[source_file_index]
	for entry in data.roots {
		project_index_remove_global_root_candidate(
			index,
			Root_Name_Key{namespace = entry.namespace, name = entry.name},
			source_file_id,
		)
	}
	for name in data.provided_names {
		project_index_decrement_name_count(
			&index.provided_name_counts,
			&index.root_lookup.provided_names,
			name,
		)
	}
	clear(&data.roots)
	clear(&data.provided_names)
	clear(&data.exports)
	clear(&data.class_scope_entries)
}

project_index_collect_source_file :: proc(
	index: ^Project_Index,
	unit: ^Source_File_Provider,
	source_file_index: int,
) {
	data := &index.source_file_entries[source_file_index]
	data.role = unit.role
	unit_stem := uri_file_stem(unit.uri)
	for name in unit.provided_names {
		index_name := project_index_name(index, name)
		append(&data.provided_names, index_name)
		project_index_add_remote_export(data, index_name)
		project_index_increment_name_count(
			&index.provided_name_counts,
			&index.root_lookup.provided_names,
			index_name,
		)
	}
	is_typepool := typepool_dependency_unit(unit.uri)
	root := scope(unit, unit.root_scope)
	for symbol_id in root.declarations {
		symbol := symbol(unit, symbol_id)
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
			if !symbol_kind_occupies(symbol.kind, namespace) {
				continue
			}
			if first, ok := scope_lookup_declaration(
				unit,
				unit.root_scope,
				namespace,
				symbol.name,
			); !ok || first != symbol.id {
				continue
			}
			index_name := project_index_name(index, symbol.name)
			entry := Root_Symbol_Entry {
				unit               = unit.source_file_id,
				symbol             = symbol.id,
				namespace          = namespace,
				name               = index_name,
				visible_by_default = visible_by_default,
			}
			if !entry.visible_by_default {
				continue
			}
			append(&data.roots, entry)
			if is_typepool && symbol.kind == .Constant {
				project_index_add_remote_export(data, entry.name)
			}
			project_index_add_global_root_candidate(
				index,
				Root_Name_Key{namespace = entry.namespace, name = entry.name},
				Symbol_Link{unit = entry.unit, symbol = entry.symbol},
			)
		}
	}
}

@(private)
project_index_add_remote_export :: proc(data: ^Project_Index_Source_File, name: string) {
	if name == "" {
		return
	}
	for existing in data.exports {
		if existing == name {
			return
		}
	}
	append(&data.exports, name)
}

project_index_add_summary_root :: proc(
	index: ^Project_Index,
	namespace: Namespace,
	name: string,
	entity: Entity_Handle,
) {
	if name == "" || !provider_handle_is_valid(entity.provider) {
		return
	}
	index_name := project_index_name(index, name)
	key := Root_Name_Key {
		namespace = namespace,
		name      = index_name,
	}
	if _, exists := index.root_lookup.global[key]; exists {
		return
	}
	index.root_lookup.summary_global[key] = entity
	project_index_increment_name_count(
		&index.provided_name_counts,
		&index.root_lookup.provided_names,
		index_name,
	)
}

project_index_update_include_graph :: proc(
	index: ^Project_Index,
	units: []Source_File_Provider,
	source_file_ids: []Source_File_Id,
) {
	project_index_ensure_source_file_count(index, len(units))
	rebuild := len(index.visible) != len(units) || len(index.predecessors) != len(units)
	for source_file_id in source_file_ids {
		source_file_index := source_file_id_index(source_file_id)
		if project_index_include_targets_changed(
			&index.source_file_entries[source_file_index],
			&units[source_file_index],
		) {
			rebuild = true
		}
	}
	if rebuild {
		project_index_rebuild_include_graph(index, units)
	}
	if rebuild || project_index_class_scope_dirty(units, source_file_ids) {
		project_index_rebuild_class_scope_index(index, units)
	}
}

project_index_class_scope_dirty :: proc(
	units: []Source_File_Provider,
	source_file_ids: []Source_File_Id,
) -> bool {
	for source_file_id in source_file_ids {
		source_file_index := source_file_id_index(source_file_id)
		unit := &units[source_file_index]
		if len(unit.class_definitions) > 0 ||
		   len(unit.class_inheritance) > 0 ||
		   len(unit.implemented_interfaces) > 0 ||
		   len(unit.member_aliases) > 0 {
			return true
		}
		for symbol in unit.symbols {
			if symbol.kind == .Class || symbol.kind == .Interface {
				return true
			}
		}
	}
	return false
}

project_index_include_targets_changed :: proc(
	data: ^Project_Index_Source_File,
	unit: ^Source_File_Provider,
) -> bool {
	changed := len(data.include_targets) != len(unit.include_edges)
	if !changed {
		for edge, i in unit.include_edges {
			target := edge.target if edge.has_target else INVALID_SOURCE_FILE_ID
			if data.include_targets[i] != target {
				changed = true
				break
			}
		}
	}
	if changed {
		clear(&data.include_targets)
		for edge in unit.include_edges {
			append(
				&data.include_targets,
				edge.target if edge.has_target else INVALID_SOURCE_FILE_ID,
			)
		}
	}
	return changed
}

project_index_rebuild_include_graph :: proc(index: ^Project_Index, units: []Source_File_Provider) {
	project_index_destroy_include_graph(index)
	index.visible = include_visible_source_files_for_source_files(units, index.allocator)
	index.predecessors = include_predecessor_source_files_for_source_files(units, index.allocator)
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

project_index_rebuild_class_scope_index :: proc(
	index: ^Project_Index,
	units: []Source_File_Provider,
) {
	delete(index.class_scope_entries)
	delete(index.class_scope_candidates)
	index.class_scope_entries = make(
		map[Project_Class_Member_Key]Project_Class_Member_Entry,
		16,
		index.allocator,
	)
	index.class_scope_candidates = make(
		map[Project_Class_Member_Key][dynamic]Project_Class_Scope_Index_Entry,
		16,
		index.allocator,
	)
	for i in 0 ..< len(units) {
		clear(&index.source_file_entries[i].class_scope_entries)
	}
	for &unit in units {
		for owner in unit.symbols {
			if !(owner.kind == .Class || owner.kind == .Interface) {
				continue
			}
			scope_id := class_definition_scope(&unit, owner.id)
			scope_data := scope(&unit, scope_id)
			if scope_data == nil {
				continue
			}
			for symbol_id in scope_data.declarations {
				symbol := symbol(&unit, symbol_id)
				namespaces := [?]Namespace{.Value, .Type, .Routine}
				for namespace in namespaces {
					if symbol_kind_occupies(symbol.kind, namespace) {
						project_index_record_class_scope_entry(
							index,
							unit.source_file_id,
							Project_Class_Member_Key {
								class_unit = unit.source_file_id,
								class_symbol = owner.id,
								namespace = namespace,
								name = project_index_name(index, symbol.name),
							},
							Project_Class_Member_Entry {
								unit = unit.source_file_id,
								symbol = symbol.id,
							},
						)
					}
				}
			}
		}
	}
	changed := true
	for changed {
		changed = false
		for unit, source_file_index in units {
			if source_file_index >= len(index.visible) {
				continue
			}
			for alias in unit.member_aliases {
				if alias.alias_name == "" || alias.target_interface_name == "" {
					continue
				}
				target, ok := resolve_type_name_in_project(
					units,
					source_file_index,
					alias.target_interface_name,
					&index.root_lookup,
					index.visible[source_file_index],
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
						class_unit   = unit.source_file_id,
						class_symbol = alias.owner_symbol,
						namespace    = namespace,
						name         = alias.alias_name,
					}
					if alias_key in index.class_scope_entries {
						continue
					}
					if target_entry, target_ok := index.class_scope_entries[target_key];
					   target_ok {
						alias_key.name = project_index_name(index, alias.alias_name)
						project_index_record_class_scope_entry(
							index,
							unit.source_file_id,
							alias_key,
							target_entry,
						)
						changed = true
					}
				}
			}
		}
	}
}

project_index_record_class_scope_entry :: proc(
	index: ^Project_Index,
	owner_unit: Source_File_Id,
	key: Project_Class_Member_Key,
	entry: Project_Class_Member_Entry,
) {
	source_file_index := source_file_id_index(owner_unit)
	data := &index.source_file_entries[source_file_index]
	append(
		&data.class_scope_entries,
		Project_Class_Scope_Index_Entry{unit = owner_unit, key = key, entry = entry},
	)
	project_index_add_class_scope_candidate(
		index,
		data.class_scope_entries[len(data.class_scope_entries) - 1],
	)
}

project_index_add_global_root_candidate :: proc(
	index: ^Project_Index,
	key: Root_Name_Key,
	handle: Symbol_Link,
) {
	if candidates, ok := index.global_root_candidates[key]; ok {
		insert := len(candidates)
		for candidate, i in candidates {
			if project_index_unit_precedes(index, handle.unit, candidate.unit) {
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
		next := make([dynamic]Symbol_Link, 0, 2, index.allocator)
		append(&next, handle)
		index.global_root_candidates[key] = next
	}
	index.root_lookup.global[key] = index.global_root_candidates[key][0]
}

project_index_remove_global_root_candidate :: proc(
	index: ^Project_Index,
	key: Root_Name_Key,
	source_file_id: Source_File_Id,
) {
	candidates, ok := index.global_root_candidates[key]
	if !ok {
		return
	}
	write := 0
	for candidate in candidates {
		if candidate.unit == source_file_id {
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
			if project_index_unit_precedes(index, entry.unit, candidate.unit) {
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

project_index_unit_precedes :: proc(index: ^Project_Index, left, right: Source_File_Id) -> bool {
	left_precedence := project_index_unit_precedence(index, left)
	right_precedence := project_index_unit_precedence(index, right)
	if left_precedence != right_precedence {
		return left_precedence < right_precedence
	}
	return source_file_id_index(left) < source_file_id_index(right)
}

project_index_unit_precedence :: proc(
	index: ^Project_Index,
	source_file_id: Source_File_Id,
) -> int {
	source_file_index := source_file_id_index(source_file_id)
	if source_file_index < 0 || source_file_index >= len(index.source_file_entries) {
		return 100
	}
	if index.source_file_entries[source_file_index].role != .Dependency_Interface_Source {
		return 0
	}
	return 1
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
