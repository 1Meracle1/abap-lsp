package abap_frontend_semantic_analyze

import "core:mem"

collect_project_remote_dependency_candidates :: proc(
	project: ^Project_Analysis,
	allocator: mem.Allocator,
) -> [dynamic]Remote_Dependency_Candidate {
	out := make([dynamic]Remote_Dependency_Candidate, 0, 8, allocator)
	index := make(map[string]int, 64, allocator)
	defer delete(index)
	for &unit in project.units {
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
				insert_remote_candidate(&out, &index, candidate.name, candidate.kind, allocator)
			}
		}
		for &symbol in unit.symbols {
			if symbol.decl_range.start == symbol.decl_range.end &&
			   symbol.has_declared_type &&
			   symbol.declared_type.namespace == .Type {
				insert_remote_candidate(
					&out,
					&index,
					symbol.declared_type.base_name,
					.Type,
					allocator,
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
			if sql_source.resolution == .External {
				insert_remote_candidate(&out, &index, sql_source.name, .Type, allocator)
			}
		}
		for &call_site in unit.call_sites {
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
	allocator: mem.Allocator,
) -> [dynamic]Remote_Dependency_Candidate {
	out := make([dynamic]Remote_Dependency_Candidate, 0, len(state.unresolved_candidates), allocator)
	index := make(map[string]int, len(state.unresolved_candidates) + 8, allocator)
	defer delete(index)
	for key in state.unresolved_candidates {
		insert_remote_candidate(&out, &index, key.name, key.kind, allocator)
	}
	return out
}

@(private)
record_project_unresolved_candidates :: proc(
	state: ^Project_State,
	project: ^Project_Analysis,
	allocator: mem.Allocator,
) {
	state.unresolved_candidates = make(map[Remote_Dependency_Key][dynamic]Unit_Id, 64, allocator)
	for &unit in project.units {
		for &edge in unit.include_edges {
			if !edge.has_target {
				record_remote_candidate_unit(state, edge.name, .Include, unit.unit_id, allocator)
			}
		}
		for &ref in unit.references {
			if ref.has_resolution && ref.resolution.kind != .External {
				continue
			}
			if candidate, ok := remote_dependency_candidate_for_reference(&ref); ok {
				record_remote_candidate_unit(state, candidate.name, candidate.kind, unit.unit_id, allocator)
			}
		}
		for &symbol in unit.symbols {
			if symbol.decl_range.start == symbol.decl_range.end &&
			   symbol.has_declared_type &&
			   symbol.declared_type.namespace == .Type {
				record_remote_candidate_unit(
					state,
					symbol.declared_type.base_name,
					.Type,
					unit.unit_id,
					allocator,
				)
			}
		}
		if unit.has_message_default_class {
			record_remote_candidate_unit(
				state,
				unit.message_default_class.name,
				.Message_Class,
				unit.unit_id,
				allocator,
			)
		}
		for &message in unit.message_uses {
			if message.class_name != "" {
				record_remote_candidate_unit(
					state,
					message.class_name,
					.Message_Class,
					unit.unit_id,
					allocator,
				)
			}
		}
		for &sql_source in unit.sql_sources {
			if sql_source.resolution == .External {
				record_remote_candidate_unit(state, sql_source.name, .Type, unit.unit_id, allocator)
			}
		}
		for &call_site in unit.call_sites {
			#partial switch call_site.target.kind {
			case .Function:
				record_remote_candidate_unit(
					state,
					call_site.target.function_name,
					.Function,
					unit.unit_id,
					allocator,
				)
			case .Report:
				record_remote_candidate_unit(
					state,
					call_site.target.report_name,
					.Report,
					unit.unit_id,
					allocator,
				)
			}
		}
	}
}

@(private)
record_remote_candidate_unit :: proc(
	state: ^Project_State,
	name: string,
	kind: Remote_Dependency_Kind,
	unit_id: Unit_Id,
	allocator: mem.Allocator,
) {
	normalized_name := canonical_name(name, allocator)
	if normalized_name == "" {
		return
	}
	key := Remote_Dependency_Key{name = normalized_name, kind = kind}
	if units, ok := state.unresolved_candidates[key]; ok {
		push_unique_unit(&units, unit_id)
		state.unresolved_candidates[key] = units
	} else {
		waiting_units := make([dynamic]Unit_Id, 0, 2, allocator)
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
	switch ref.kind {
	case .Include, .Structured_Decl_End:
		return {}, false
	case .Static_Target:
		if !ref.has_resolution || ref.resolution.kind != .External {
			return {}, false
		}
		kind = .Static
	case .Type_Ref:
		if ref.namespace != .Type {
			return {}, false
		}
		kind = .Type
	case .Message_Class:
		kind = .Message_Class
	case .Routine_Call:
		return {}, false
	case .Identifier:
		return {}, false
	}
	return Remote_Dependency_Candidate{name = ref.name, kind = kind}, true
}

@(private)
insert_remote_candidate :: proc(
	out: ^[dynamic]Remote_Dependency_Candidate,
	index: ^map[string]int,
	name: string,
	kind: Remote_Dependency_Kind,
	allocator: mem.Allocator,
) {
	normalized_name := canonical_name(name, allocator)
	if normalized_name == "" {
		return
	}
	if existing_index, ok := index^[normalized_name]; ok {
		if remote_candidate_kind_priority(kind) >
		   remote_candidate_kind_priority(out^[existing_index].kind) {
			out^[existing_index].kind = kind
		}
		return
	}
	index^[normalized_name] = len(out^)
	append(out, Remote_Dependency_Candidate{name = normalized_name, kind = kind})
}

@(private)
remote_candidate_kind_priority :: proc(kind: Remote_Dependency_Kind) -> int {
	if kind == .Message_Class {return 5}
	if kind == .Include || kind == .Function {return 4}
	if kind == .Static {return 3}
	if kind == .Type {return 2}
	return 1
}
