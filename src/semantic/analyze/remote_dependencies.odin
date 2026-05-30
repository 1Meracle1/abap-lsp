package abap_frontend_semantic_analyze

import "core:mem"
import base_runtime "base:runtime"

collect_project_remote_dependency_candidates :: proc(
	project: ^Project_Analysis,
	allocator: mem.Allocator,
) -> [dynamic]Remote_Dependency_Candidate {
	out := make([dynamic]Remote_Dependency_Candidate, 0, 8, allocator)
	index := make(map[string]int, 64, context.temp_allocator)
	lookup := build_validation_lookup(project, context.temp_allocator)
	for &unit, unit_index in project.units {
		for &edge in unit.include_edges {
			if !edge.has_target {
				insert_remote_candidate(&out, &index, edge.name, .Include, allocator)
			}
		}
		for &ref in unit.references {
			if ref.has_resolution && ref.resolution.kind != .External {
				continue
			}
			if candidate, ok := remote_dependency_candidate_for_reference(&ref); ok {
				insert_remote_candidate(&out, &index, candidate.name, candidate.kind, allocator, candidate.hint)
			}
		}
		for &symbol in unit.symbols {
			if !symbol_kind_is_builtin(symbol.kind) &&
			   symbol.decl_range.start == symbol.decl_range.end &&
			   symbol.has_declared_type &&
			   symbol.declared_type.namespace == .Type {
				insert_remote_candidate(
					&out,
					&index,
					symbol.declared_type.base_name,
					.Type,
					allocator,
					remote_dependency_hint_for_type_ref(symbol.declared_type),
				)
			}
		}
		if unit.has_message_default_class {
			insert_remote_candidate(
				&out,
				&index,
				unit.message_default_class.name,
				.Message_Class,
				allocator,
			)
		}
		for &message in unit.message_uses {
			if message.class_name != "" {
				insert_remote_candidate(
					&out,
					&index,
					message.class_name,
					.Message_Class,
					allocator,
				)
			}
		}
		for &sql_source in unit.sql_sources {
			if sql_source_needs_remote_dependency(project, &lookup, unit_index, sql_source) {
				insert_remote_candidate(&out, &index, sql_source.name, .Type, allocator)
			}
		}
		for &call_site in unit.call_sites {
			if !call_site_needs_remote_dependency(project, &lookup, unit_index, call_site) {
				continue
			}
			#partial switch call_site.target.kind {
			case .Function:
				insert_remote_candidate(
					&out,
					&index,
					call_site.target.function_name,
					.Function,
					allocator,
				)
			case .Report:
				insert_remote_candidate(
					&out,
					&index,
					call_site.target.report_name,
					.Report,
					allocator,
				)
			}
		}
	}
	return out
}

collect_project_state_remote_dependency_candidates :: proc(
	state: ^Project_State,
	include_dependency_interfaces: bool,
	allocator: mem.Allocator,
) -> [dynamic]Remote_Dependency_Candidate {
	out := make([dynamic]Remote_Dependency_Candidate, 0, len(state.unresolved_candidates), allocator)
	index := make(map[string]int, len(state.unresolved_candidates), context.temp_allocator)
	for key, units in state.unresolved_candidates {
		if !include_dependency_interfaces &&
		   !remote_candidate_has_full_source_waiter(state, units[:]) {
			continue
		}
		insert_remote_candidate(&out, &index, key.name, key.kind, allocator, key.hint)
	}
	return out
}

@(private)
remote_candidate_has_full_source_waiter :: proc(
	state: ^Project_State,
	units: []Unit_Id,
) -> bool {
	for unit_id in units {
		unit_index := unit_id_index(unit_id)
		if unit_index >= 0 &&
		   unit_index < len(state.units) &&
		   state.units[unit_index].source_mode != .Dependency_Interface {
			return true
		}
	}
	return false
}

@(private)
record_project_unresolved_candidates :: proc(
	state: ^Project_State,
	project: ^Project_Analysis,
) {
	project_state_unresolved_candidates_destroy(state)
	candidate_allocator := base_runtime.heap_allocator()
	state.unresolved_candidates = make(map[Remote_Dependency_Key][dynamic]Unit_Id, 64, candidate_allocator)
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	recorded := make(map[Remote_Dependency_Key]Unit_Id, 64, context.temp_allocator)
	lookup := validation_lookup_from_project_index(&state.index)
	for unit, unit_index in project.units {
		for &edge in unit.include_edges {
			if !edge.has_target {
				record_remote_candidate_unit(state, &recorded, edge.name, .Include, unit.unit_id)
			}
		}
		for &ref in unit.references {
			if ref.has_resolution && ref.resolution.kind != .External {
				continue
			}
			if candidate, ok := remote_dependency_candidate_for_reference(&ref); ok {
				record_remote_candidate_unit(state, &recorded, candidate.name, candidate.kind, unit.unit_id, candidate.hint)
			}
		}
		for &symbol in unit.symbols {
			if !symbol_kind_is_builtin(symbol.kind) &&
			   symbol.decl_range.start == symbol.decl_range.end &&
			   symbol.has_declared_type &&
			   symbol.declared_type.namespace == .Type {
				record_remote_candidate_unit(
					state,
					&recorded,
					symbol.declared_type.base_name,
					.Type,
					unit.unit_id,
					remote_dependency_hint_for_type_ref(symbol.declared_type),
				)
			}
		}
		if unit.has_message_default_class {
			record_remote_candidate_unit(
				state,
				&recorded,
				unit.message_default_class.name,
				.Message_Class,
				unit.unit_id,
			)
		}
		for &message in unit.message_uses {
			if message.class_name != "" {
				record_remote_candidate_unit(
					state,
					&recorded,
					message.class_name,
					.Message_Class,
					unit.unit_id,
				)
			}
		}
		for &sql_source in unit.sql_sources {
			if sql_source_needs_remote_dependency(project, &lookup, unit_index, sql_source) {
				record_remote_candidate_unit(state, &recorded, sql_source.name, .Type, unit.unit_id)
			}
		}
		for &call_site in unit.call_sites {
			if !call_site_needs_remote_dependency(project, &lookup, unit_index, call_site) {
				continue
			}
			#partial switch call_site.target.kind {
			case .Function:
				record_remote_candidate_unit(
					state,
					&recorded,
					call_site.target.function_name,
					.Function,
					unit.unit_id,
				)
			case .Report:
				record_remote_candidate_unit(
					state,
					&recorded,
					call_site.target.report_name,
					.Report,
					unit.unit_id,
				)
			}
		}
	}
}

@(private)
record_project_unresolved_candidates_for_units :: proc(
	state: ^Project_State,
	project: ^Project_Analysis,
	unit_ids: []Unit_Id,
) {
	project_index_ensure_unit_count(&state.index, len(project.units))
	lookup := validation_lookup_from_project_index(&state.index)
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	for unit_id in unit_ids {
		unit_index := unit_id_index(unit_id)
		if unit_index < 0 || unit_index >= len(project.units) {
			continue
		}
		data := &state.index.unit_entries[unit_index]
		for key in data.unresolved_candidates {
			remove_remote_candidate_unit(state, key, unit_id)
		}
		clear(&data.unresolved_candidates)
		recorded := make(map[Remote_Dependency_Key]bool, 8, context.temp_allocator)
		unit := &project.units[unit_index]
		for &edge in unit.include_edges {
			if !edge.has_target {
				record_remote_candidate_unit_incremental(state, data, &recorded, edge.name, .Include, unit.unit_id)
			}
		}
		for &ref in unit.references {
			if ref.has_resolution && ref.resolution.kind != .External {
				continue
			}
			if candidate, ok := remote_dependency_candidate_for_reference(&ref); ok {
				record_remote_candidate_unit_incremental(
					state,
					data,
					&recorded,
					candidate.name,
					candidate.kind,
					unit.unit_id,
					candidate.hint,
				)
			}
		}
		for &symbol in unit.symbols {
			if !symbol_kind_is_builtin(symbol.kind) &&
			   symbol.decl_range.start == symbol.decl_range.end &&
			   symbol.has_declared_type &&
			   symbol.declared_type.namespace == .Type {
				record_remote_candidate_unit_incremental(
					state,
					data,
					&recorded,
					symbol.declared_type.base_name,
					.Type,
					unit.unit_id,
					remote_dependency_hint_for_type_ref(symbol.declared_type),
				)
			}
		}
		if unit.has_message_default_class {
			record_remote_candidate_unit_incremental(
				state,
				data,
				&recorded,
				unit.message_default_class.name,
				.Message_Class,
				unit.unit_id,
			)
		}
		for &message in unit.message_uses {
			if message.class_name != "" {
				record_remote_candidate_unit_incremental(
					state,
					data,
					&recorded,
					message.class_name,
					.Message_Class,
					unit.unit_id,
				)
			}
		}
		for &sql_source in unit.sql_sources {
			if sql_source_needs_remote_dependency(project, &lookup, unit_index, sql_source) {
				record_remote_candidate_unit_incremental(state, data, &recorded, sql_source.name, .Type, unit.unit_id)
			}
		}
		for &call_site in unit.call_sites {
			if !call_site_needs_remote_dependency(project, &lookup, unit_index, call_site) {
				continue
			}
			#partial switch call_site.target.kind {
			case .Function:
				record_remote_candidate_unit_incremental(
					state,
					data,
					&recorded,
					call_site.target.function_name,
					.Function,
					unit.unit_id,
				)
			case .Report:
				record_remote_candidate_unit_incremental(
					state,
					data,
					&recorded,
					call_site.target.report_name,
					.Report,
					unit.unit_id,
				)
			}
		}
	}
}

@(private)
sql_source_needs_remote_dependency :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	sql_source: Sql_Source_Data,
) -> bool {
	if sql_source.resolution != .External {
		return false
	}
	_, ok := resolve_type_name_in_project_lookup(project, lookup, unit_index, sql_source.name)
	return !ok
}

@(private)
call_site_needs_remote_dependency :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	call_site: Call_Site_Data,
) -> bool {
	#partial switch call_site.target.kind {
	case .Function:
		_, ok := resolve_function_module_in_project_lookup(project, lookup, unit_index, call_site.target.function_name)
		return !ok
	case .Report:
		_, ok := resolve_root_name_in_project_lookup(project, lookup, unit_index, .Value, call_site.target.report_name)
		return !ok
	}
	return false
}

@(private)
resolve_root_name_in_project_lookup :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_index: int,
	namespace: Namespace,
	name: string,
) -> (Symbol_Handle, bool) {
	unit_id := project.units[unit_index].unit_id
	if handle, ok := root_symbol_in_unit_lookup(lookup, unit_id, namespace, name); ok {
		return handle, true
	}
	if handle, ok := root_symbol_in_visible_units_lookup(lookup, namespace, name, lookup.visible[unit_index]); ok {
		return handle, true
	}
	return global_visible_root_symbol_lookup(lookup, namespace, name)
}

@(private)
project_state_unresolved_candidates_destroy :: proc(state: ^Project_State) {
	for _, units in state.unresolved_candidates {
		delete(units)
	}
	delete(state.unresolved_candidates)
}

@(private)
remove_remote_candidate_unit :: proc(
	state: ^Project_State,
	key: Remote_Dependency_Key,
	unit_id: Unit_Id,
) {
	if units, ok := state.unresolved_candidates[key]; ok {
		write := 0
		for waiting in units {
			if waiting == unit_id {
				continue
			}
			units[write] = waiting
			write += 1
		}
		if write == 0 {
			delete(units)
			delete_key(&state.unresolved_candidates, key)
		} else {
			resize(&units, write)
			state.unresolved_candidates[key] = units
		}
	}
}

@(private)
record_remote_candidate_unit_incremental :: proc(
	state: ^Project_State,
	data: ^Project_Index_Unit,
	recorded: ^map[Remote_Dependency_Key]bool,
	name: string,
	kind: Remote_Dependency_Kind,
	unit_id: Unit_Id,
	hint := Remote_Dependency_Hint.None,
) {
	if name == "" {
		return
	}
	key := Remote_Dependency_Key{name = name, kind = kind, hint = hint}
	if key in recorded^ {
		return
	}
	recorded^[key] = true
	append(&data.unresolved_candidates, key)
	if units, ok := state.unresolved_candidates[key]; ok {
		append(&units, unit_id)
		state.unresolved_candidates[key] = units
	} else {
		waiting_units := make([dynamic]Unit_Id, 0, 2, state.index.allocator)
		append(&waiting_units, unit_id)
		state.unresolved_candidates[key] = waiting_units
	}
}

@(private)
record_remote_candidate_unit :: proc(
	state: ^Project_State,
	recorded: ^map[Remote_Dependency_Key]Unit_Id,
	name: string,
	kind: Remote_Dependency_Kind,
	unit_id: Unit_Id,
	hint := Remote_Dependency_Hint.None,
) {
	if name == "" {
		return
	}
	key := Remote_Dependency_Key{name = name, kind = kind, hint = hint}
	if previous, ok := recorded^[key]; ok && previous == unit_id {
		return
	}
	recorded^[key] = unit_id
	if units, ok := state.unresolved_candidates[key]; ok {
		append(&units, unit_id)
		state.unresolved_candidates[key] = units
	} else {
		waiting_units := make([dynamic]Unit_Id, 0, 2, base_runtime.heap_allocator())
		append(&waiting_units, unit_id)
		state.unresolved_candidates[key] = waiting_units
	}
}

@(private)
remote_dependency_candidate_for_reference :: proc(
	ref: ^Reference_Data,
) -> (
	Remote_Dependency_Candidate,
	bool,
) {
	kind := Remote_Dependency_Kind.Type
	hint := Remote_Dependency_Hint.None
	switch ref.kind {
	case .Include, .Structured_Decl_End:
		return {}, false
	case .Static_Target:
		if ref.has_resolution && ref.resolution.kind != .External {
			return {}, false
		}
		kind = .Static
	case .Type_Ref:
		kind = .Type
		if ref.namespace == .Type {
			if ref.type_is_ref {
				hint = .Object_Type
			}
		} else if !(ref.namespace == .Value &&
		            ref.type_has_path &&
		            ref.type_first_selector == .Dash) &&
		          !(ref.namespace == .Value &&
		            ref.has_type_clause_form &&
		            (ref.type_clause_form == .Structure || ref.type_clause_form == .Like)) {
			return {}, false
		}
	case .Interface_Use:
		if ref.namespace != .Type {
			return {}, false
		}
		kind = .Type
		hint = .Interface_Type
	case .Message_Class:
		kind = .Message_Class
	case .Routine_Call:
		return {}, false
	case .Identifier:
		if ref.namespace != .Value {
			return {}, false
		}
		kind = .Symbol
	}
	return Remote_Dependency_Candidate{name = ref.name, kind = kind, hint = hint}, true
}

@(private)
insert_remote_candidate :: proc(
	out: ^[dynamic]Remote_Dependency_Candidate,
	index: ^map[string]int,
	name: string,
	kind: Remote_Dependency_Kind,
	allocator: mem.Allocator,
	hint := Remote_Dependency_Hint.None,
) {
	normalized_name := canonical_name(name, allocator)
	if normalized_name == "" {
		return
	}
	if existing_index, ok := index^[normalized_name]; ok {
		if remote_candidate_kind_priority(kind) >
		   remote_candidate_kind_priority(out^[existing_index].kind) {
			out^[existing_index].kind = kind
			out^[existing_index].hint = hint
		} else if kind == out^[existing_index].kind &&
		          remote_candidate_hint_priority(hint) >
		          remote_candidate_hint_priority(out^[existing_index].hint) {
			out^[existing_index].hint = hint
		}
		return
	}
	index^[normalized_name] = len(out^)
	append(out, Remote_Dependency_Candidate{name = normalized_name, kind = kind, hint = hint})
}

@(private)
remote_dependency_hint_for_type_ref :: proc(type_ref: Field_Type_Ref_Data) -> Remote_Dependency_Hint {
	if type_ref.is_ref {
		return .Object_Type
	}
	return .None
}

@(private)
remote_candidate_kind_priority :: proc(kind: Remote_Dependency_Kind) -> int {
	if kind == .Message_Class {return 5}
	if kind == .Include || kind == .Function {return 4}
	if kind == .Static {return 3}
	if kind == .Type {return 2}
	return 1
}

@(private)
remote_candidate_hint_priority :: proc(hint: Remote_Dependency_Hint) -> int {
	if hint == .Interface_Type {return 2}
	if hint == .Object_Type {return 1}
	return 0
}
