package abap_frontend_semantic_analyze

import "src:ast"
import execution "src:execution"
import "src:parser"
import deps "src:semantic/dependencies"
import uri_key "src:uri_key"

import "core:mem"
import "core:mem/virtual"
import "core:strings"

Source_Input :: struct {
	uri:    string,
	source: string,
	mode:   Source_Mode,
}

Analyze_Options :: struct {
	pool: ^execution.Pool,
}

Project_State :: struct {
	inputs:                     [dynamic]Source_Input,
	units:                      [dynamic]Unit_Analysis,
	uri_to_unit:                map[string]Unit_Id,
	reverse_edges:              map[Unit_Id][dynamic]Unit_Id,
	unresolved_candidates:      map[deps.Remote_Dependency_Key][dynamic]Unit_Id,
	unit_dependencies:          [dynamic][dynamic]Unit_Id,
	unit_unresolved_candidates: [dynamic][dynamic]deps.Remote_Dependency_Key,
	diagnostics:                [dynamic]Diagnostic,
	index:                      Project_Index,
	candidates:                 [dynamic]Project_Candidate_Input,
	candidate_to_unit:          [dynamic]Unit_Id,
	candidate_dirs:             [dynamic]string,
	unit_dirs:                  [dynamic]string,
	unit_candidate_index:       [dynamic]int,
	interface_signatures:       [dynamic]string,
	unit_allocators:            []mem.Allocator,
	allocator:                  mem.Allocator,
}

Project_Candidate_Input :: struct {
	input:       Source_Input,
	object_name: string,
}

Project_Work_State :: struct {
	units:           [dynamic]Unit_Analysis,
	inputs:          [dynamic]Source_Input,
	unit_allocators: []mem.Allocator,
	allocator:       mem.Allocator,
}

Project_Task_Payload :: struct {
	state:      ^Project_Work_State,
	unit_index: int,
}

Project_Infer_State :: struct {
	project:         ^Project_Analysis,
	lookup:          ^Project_Index,
	inferred:        []Inferred_Unit_Facts,
	unit_allocators: []mem.Allocator,
	allocator:       mem.Allocator,
}

Project_Validate_State :: struct {
	project:         ^Project_Analysis,
	lookup:          ^Project_Index,
	diagnostics:     [][dynamic]Diagnostic,
	unit_allocators: []mem.Allocator,
	allocator:       mem.Allocator,
}

Project_Infer_Payload :: struct {
	state:        ^Project_Infer_State,
	unit_index:   int,
	output_index: int,
}

Project_Validate_Payload :: struct {
	state:        ^Project_Validate_State,
	unit_index:   int,
	output_index: int,
}

Temp_Arena_Marker :: struct {
	temp:   virtual.Arena_Temp,
	active: bool,
}

Sql_Predicate_Name_Key :: struct {
	query_id: int,
	start:    int,
	end:      int,
	name:     string,
}

analyze_target :: proc(
	target: Source_Input,
	candidates: []Source_Input,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> Project_Analysis {
	wrapped := make([dynamic]Project_Candidate_Input, 0, len(candidates), allocator)
	for candidate in candidates {
		append(&wrapped, Project_Candidate_Input{input = candidate})
	}
	return analyze_target_with_candidate_inputs(target, wrapped[:], {}, options, allocator)
}

analyze_target_with_candidate_inputs :: proc(
	target: Source_Input,
	candidates: []Project_Candidate_Input,
	dependencies: []Source_Input,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> Project_Analysis {
	return analyze_target_with_candidate_inputs_allocators(
		target,
		candidates,
		dependencies,
		options,
		{},
		allocator,
	)
}

analyze_target_with_candidate_inputs_allocators :: proc(
	target: Source_Input,
	candidates: []Project_Candidate_Input,
	dependencies: []Source_Input,
	options: Analyze_Options,
	unit_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) -> Project_Analysis {
	assert(options.pool != nil)
	state := project_state_make(unit_allocators, allocator)
	return project_state_analyze_target_with_candidate_inputs(
		&state,
		target,
		candidates,
		dependencies,
		options,
		allocator,
	)
}

project_state_make :: proc(
	unit_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) -> Project_State {
	return Project_State {
		inputs = make([dynamic]Source_Input, 0, 8, allocator),
		units = make([dynamic]Unit_Analysis, 0, 8, allocator),
		uri_to_unit = make(map[string]Unit_Id, 16, allocator),
		reverse_edges = make(map[Unit_Id][dynamic]Unit_Id, 16, allocator),
		unresolved_candidates = make(
			map[deps.Remote_Dependency_Key][dynamic]Unit_Id,
			16,
			allocator,
		),
		unit_dependencies = make([dynamic][dynamic]Unit_Id, 0, 8, allocator),
		unit_unresolved_candidates = make(
			[dynamic][dynamic]deps.Remote_Dependency_Key,
			0,
			8,
			allocator,
		),
		diagnostics = make([dynamic]Diagnostic, 0, 8, allocator),
		index = project_index_make(allocator),
		candidates = make([dynamic]Project_Candidate_Input, 0, 8, allocator),
		candidate_to_unit = make([dynamic]Unit_Id, 0, 8, allocator),
		candidate_dirs = make([dynamic]string, 0, 8, allocator),
		unit_dirs = make([dynamic]string, 0, 8, allocator),
		unit_candidate_index = make([dynamic]int, 0, 8, allocator),
		interface_signatures = make([dynamic]string, 0, 8, allocator),
		unit_allocators = unit_allocators,
		allocator = allocator,
	}
}

project_state_analyze_target_with_candidate_inputs :: proc(
	state: ^Project_State,
	target: Source_Input,
	candidates: []Project_Candidate_Input,
	dependencies: []Source_Input,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> Project_Analysis {
	targets := [?]Source_Input{target}
	return project_state_analyze_targets_with_candidate_inputs(
		state,
		targets[:],
		candidates,
		dependencies,
		options,
		allocator,
	)
}

project_state_analyze_targets_with_candidate_inputs :: proc(
	state: ^Project_State,
	targets: []Source_Input,
	candidates: []Project_Candidate_Input,
	dependencies: []Source_Input,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> Project_Analysis {
	return project_state_apply_dirty_inputs(
		state,
		targets,
		candidates,
		dependencies,
		{},
		{},
		options,
		allocator,
	)
}

project_state_apply_dirty_inputs :: proc(
	state: ^Project_State,
	targets: []Source_Input,
	candidates: []Project_Candidate_Input,
	dependencies: []Source_Input,
	dirty: []Unit_Id,
	include_roots: []Unit_Id,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> Project_Analysis {
	state.allocator = allocator
	project_state_set_candidates(state, candidates, allocator)
	next_dirty := make(
		[dynamic]Unit_Id,
		0,
		len(targets) + len(dependencies) + len(dirty),
		context.temp_allocator,
	)
	next_include_roots := make(
		[dynamic]Unit_Id,
		0,
		len(targets) + len(dependencies) + len(include_roots),
		context.temp_allocator,
	)
	for unit_id in dirty {push_unique_unit(&next_dirty, unit_id)}
	for unit_id in include_roots {push_unique_unit(&next_include_roots, unit_id)}
	for target in targets {
		target_id, target_changed := project_state_upsert_input(state, target, -1, allocator)
		if target_changed {
			push_unique_unit(&next_dirty, target_id)
			push_unique_unit(&next_include_roots, target_id)
		}
	}
	for dependency in dependencies {
		unit_id, changed := project_state_upsert_input(state, dependency, -1, allocator)
		if changed {
			push_unique_unit(&next_dirty, unit_id)
			push_unique_unit(&next_include_roots, unit_id)
		}
	}
	project_state_mark_active_candidate_changes(state, &next_dirty, &next_include_roots)
	project_state_collect_include_roots_for_candidates(state, &next_include_roots)
	project_state_update(state, next_dirty[:], next_include_roots[:], options, allocator)
	return project_state_analysis(state)
}

project_state_analysis :: proc(state: ^Project_State) -> Project_Analysis {
	return Project_Analysis{units = state.units, diagnostics = state.diagnostics}
}

project_state_set_candidates :: proc(
	state: ^Project_State,
	candidates: []Project_Candidate_Input,
	allocator: mem.Allocator,
) {
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	old_candidates := state.candidates[:]
	old_candidate_dirs := state.candidate_dirs[:]
	old_candidate_to_unit := state.candidate_to_unit[:]
	clear(&state.candidates)
	clear(&state.candidate_to_unit)
	clear(&state.candidate_dirs)
	for candidate, i in candidates {
		append(&state.candidates, candidate)
		if i < len(old_candidates) && old_candidates[i].input.uri == candidate.input.uri {
			append(&state.candidate_dirs, old_candidate_dirs[i])
			append(&state.candidate_to_unit, old_candidate_to_unit[i])
			continue
		}
		append(&state.candidate_dirs, uri_parent_dir_key(candidate.input.uri, allocator))
		unit_id := INVALID_UNIT_ID
		key := normalized_uri_path_key(candidate.input.uri, context.temp_allocator)
		if existing, ok := state.uri_to_unit[key]; ok {
			unit_id = existing
			unit_index := unit_id_index(unit_id)
			if state.unit_candidate_index[unit_index] < 0 {
				state.unit_candidate_index[unit_index] = i
			}
		}
		append(&state.candidate_to_unit, unit_id)
	}
}

@(private)
project_state_mark_active_candidate_changes :: proc(
	state: ^Project_State,
	dirty: ^[dynamic]Unit_Id,
	include_roots: ^[dynamic]Unit_Id,
) {
	for candidate, i in state.candidates {
		unit_id := state.candidate_to_unit[i]
		unit_index := unit_id_index(unit_id)
		// TODO investigate in which cases it can happen
		if unit_index > len(state.inputs) {
			continue
		}
		if state.inputs[unit_index].source == candidate.input.source &&
		   state.inputs[unit_index].mode == candidate.input.mode {
			continue
		}
		state.inputs[unit_index] = candidate.input
		push_unique_unit(dirty, unit_id)
		push_unique_unit(include_roots, unit_id)
	}
}

project_state_upsert_input :: proc(
	state: ^Project_State,
	input: Source_Input,
	candidate_index: int,
	allocator: mem.Allocator,
) -> (
	Unit_Id,
	bool,
) {
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	key := normalized_uri_path_key(input.uri, context.temp_allocator)
	if unit_id, ok := state.uri_to_unit[key]; ok {
		unit_index := unit_id_index(unit_id)
		changed :=
			state.inputs[unit_index].source != input.source ||
			state.inputs[unit_index].mode != input.mode
		state.inputs[unit_index] = input
		if candidate_index >= 0 {
			state.unit_candidate_index[unit_index] = candidate_index
			state.candidate_to_unit[candidate_index] = unit_id
		}
		return unit_id, changed
	}
	key = normalized_uri_path_key(input.uri, allocator)
	unit_id := Unit_Id(u32(len(state.units)))
	state.uri_to_unit[key] = unit_id
	append(&state.inputs, input)
	append(&state.units, Unit_Analysis{})
	append(&state.unit_dirs, uri_parent_dir_key(input.uri, allocator))
	append(&state.unit_candidate_index, candidate_index)
	append(&state.interface_signatures, "")
	append(&state.unit_dependencies, make([dynamic]Unit_Id, 0, 8, allocator))
	append(
		&state.unit_unresolved_candidates,
		make([dynamic]deps.Remote_Dependency_Key, 0, 8, allocator),
	)
	if candidate_index >= 0 {
		state.candidate_to_unit[candidate_index] = unit_id
	}
	return unit_id, true
}

project_state_update :: proc(
	state: ^Project_State,
	dirty_units: []Unit_Id,
	include_roots: []Unit_Id,
	options: Analyze_Options,
	allocator: mem.Allocator,
) {
	if len(dirty_units) == 0 && len(include_roots) == 0 {
		return
	}

	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	parsed_units := make([dynamic]Unit_Id, 0, len(dirty_units), context.temp_allocator)
	project_state_parse_units(state, dirty_units, options.pool, allocator)
	project_state_refresh_candidate_units(state, allocator)
	for unit_id in dirty_units {
		push_unique_unit(&parsed_units, unit_id)
	}

	next_roots := make(
		[dynamic]Unit_Id,
		0,
		len(dirty_units) + len(include_roots),
		context.temp_allocator,
	)
	for unit_id in dirty_units {push_unique_unit(&next_roots, unit_id)}
	for unit_id in include_roots {push_unique_unit(&next_roots, unit_id)}
	for len(next_roots) > 0 {
		new_units := make([dynamic]Unit_Id, 0, 4, context.temp_allocator)
		project_state_resolve_include_edges(state, next_roots[:], &new_units, allocator)
		if len(new_units) == 0 {
			break
		}
		project_state_parse_units(state, new_units[:], options.pool, allocator)
		for unit_id in new_units {push_unique_unit(&parsed_units, unit_id)}
		clear(&next_roots)
		for unit_id in new_units {push_unique_unit(&next_roots, unit_id)}
	}

	project_state_finish(state, parsed_units[:], include_roots, options.pool, allocator)
}

@(private)
project_state_refresh_candidate_units :: proc(state: ^Project_State, allocator: mem.Allocator) {
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	for candidate, i in state.candidates {
		if state.candidate_to_unit[i] != INVALID_UNIT_ID {
			continue
		}
		key := normalized_uri_path_key(candidate.input.uri, context.temp_allocator)
		unit_id, ok := state.uri_to_unit[key]
		if !ok {
			continue
		}
		state.candidate_to_unit[i] = unit_id
		unit_index := unit_id_index(unit_id)
		if state.unit_candidate_index[unit_index] < 0 {
			state.unit_candidate_index[unit_index] = i
		}
	}
}

@(private)
project_state_parse_units :: proc(
	state: ^Project_State,
	unit_ids: []Unit_Id,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) {
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	indices := make([dynamic]int, 0, len(unit_ids), context.temp_allocator)
	for unit_id in unit_ids {
		unit_index := unit_id_index(unit_id)
		append(&indices, unit_index)
	}
	if len(indices) == 0 {
		return
	}
	for unit_index in indices {
		project_state_reset_unit_allocator(state, unit_index)
	}
	work := Project_Work_State {
		units           = state.units,
		inputs          = state.inputs,
		unit_allocators = state.unit_allocators,
		allocator       = allocator,
	}
	run_project_tasks(pool, indices[:], &work, parse_collect_task)
	state.units = work.units
}

@(private)
project_state_reset_unit_allocator :: proc(state: ^Project_State, unit_index: int) {
	if unit_index < 0 ||
	   unit_index >= len(state.unit_allocators) ||
	   state.unit_allocators[unit_index].procedure != virtual.arena_allocator_proc {
		return
	}
	arena := cast(^virtual.Arena)state.unit_allocators[unit_index].data
	virtual.arena_free_all(arena)
	state.unit_allocators[unit_index] = virtual.arena_allocator(arena)
}

@(private)
project_state_resolve_include_edges :: proc(
	state: ^Project_State,
	roots: []Unit_Id,
	new_units: ^[dynamic]Unit_Id,
	allocator: mem.Allocator,
) {
	for unit_id in roots {
		unit_index := unit_id_index(unit_id)
		source_dir := state.unit_dirs[unit_index]
		for &edge in state.units[unit_index].include_edges {
			if edge.has_target {
				continue
			}
			candidate_index, ok := project_state_resolve_include_candidate(
				state,
				edge.name,
				source_dir,
			)
			if !ok {
				continue
			}
			target_unit := state.candidate_to_unit[candidate_index]
			if target_unit == INVALID_UNIT_ID {
				target_unit, _ = project_state_upsert_input(
					state,
					state.candidates[candidate_index].input,
					candidate_index,
					allocator,
				)
				push_unique_unit(new_units, target_unit)
			}
			edge.target = target_unit
			edge.has_target = true
		}
	}
}

@(private)
project_state_resolve_include_candidate :: proc(
	state: ^Project_State,
	name, source_dir: string,
) -> (
	int,
	bool,
) {
	if source_dir != "" {
		if candidate, ok := project_state_find_candidate_in_dir(state, name, source_dir); ok {
			return candidate, true
		}
		if candidate, ok := project_state_find_candidate_in_child_dir(
			state,
			name,
			source_dir,
			"includes",
		); ok {
			return candidate, true
		}
	}
	for i in 0 ..< len(state.candidates) {
		if project_state_candidate_has_name(state, i, name) {
			return i, true
		}
	}
	return -1, false
}

@(private)
project_state_find_candidate_in_dir :: proc(
	state: ^Project_State,
	name, dir: string,
) -> (
	int,
	bool,
) {
	for candidate_dir, i in state.candidate_dirs {
		if candidate_dir == dir && project_state_candidate_has_name(state, i, name) {
			return i, true
		}
	}
	return -1, false
}

@(private)
project_state_find_candidate_in_child_dir :: proc(
	state: ^Project_State,
	name, parent, child: string,
) -> (
	int,
	bool,
) {
	for candidate_dir, i in state.candidate_dirs {
		if dir_is_child(candidate_dir, parent, child) &&
		   project_state_candidate_has_name(state, i, name) {
			return i, true
		}
	}
	return -1, false
}

@(private)
project_state_candidate_has_name :: proc(
	state: ^Project_State,
	candidate_index: int,
	name: string,
) -> bool {
	candidate := state.candidates[candidate_index]
	if strings.equal_fold(uri_file_stem(candidate.input.uri), name) ||
	   (candidate.object_name != "" && strings.equal_fold(candidate.object_name, name)) {
		return true
	}
	unit_id := state.candidate_to_unit[candidate_index]
	unit_index := unit_id_index(unit_id)
	for provided in state.units[unit_index].provided_names {
		if strings.equal_fold(provided, name) {
			return true
		}
	}
	return false
}

@(private)
project_state_collect_include_roots_for_candidates :: proc(
	state: ^Project_State,
	out: ^[dynamic]Unit_Id,
) {
	for unit in state.units {
		for edge in unit.include_edges {
			if edge.has_target {
				continue
			}
			if _, ok := project_state_resolve_include_candidate(
				state,
				edge.name,
				state.unit_dirs[unit_id_index(unit.unit_id)],
			); ok {
				push_unique_unit(out, unit.unit_id)
			}
		}
	}
}

@(private)
project_state_finish :: proc(
	state: ^Project_State,
	parsed_units: []Unit_Id,
	include_roots: []Unit_Id,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) {
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	affected := make(
		[dynamic]Unit_Id,
		0,
		len(parsed_units) + len(include_roots),
		context.temp_allocator,
	)
	interface_changed := make([dynamic]Unit_Id, 0, len(parsed_units), context.temp_allocator)
	for unit_id in parsed_units {
		push_unique_unit(&affected, unit_id)
		unit_index := unit_id_index(unit_id)
		signature := unit_interface_signature(&state.units[unit_index], allocator)
		if state.interface_signatures[unit_index] != signature {
			state.interface_signatures[unit_index] = signature
			push_unique_unit(&interface_changed, unit_id)
		}
	}
	for unit_id in include_roots {
		push_unique_unit(&affected, unit_id)
	}
	remote_waiters := make([dynamic]Unit_Id, 0, len(interface_changed), context.temp_allocator)
	project_state_collect_remote_waiters(state, interface_changed[:], &affected, &remote_waiters)
	reverse_roots := make(
		[dynamic]Unit_Id,
		0,
		len(interface_changed) + len(remote_waiters),
		context.temp_allocator,
	)
	for unit_id in interface_changed {push_unique_unit(&reverse_roots, unit_id)}
	for unit_id in remote_waiters {push_unique_unit(&reverse_roots, unit_id)}
	project_state_expand_reverse_dependents(state, reverse_roots[:], &affected)

	if len(affected) == 0 {
		return
	}
	project_state_prepare_affected_units(state, affected[:])
	project_state_build_scope_indexes(state, affected[:], pool, allocator)
	for unit_id in affected {
		unit_index := unit_id_index(unit_id)
		resolve_unit_with_index(&state.units[unit_index], &state.units[unit_index].scope_index)
	}
	add_unresolved_include_diagnostics_for_units(state.units[:], affected[:], allocator)
	diagnose_include_cycles_for_units(state.units[:], affected[:], allocator)
	project_index_update_units(&state.index, state.units[:], affected[:])
	project_index_update_include_graph(&state.index, state.units[:], affected[:])

	project := project_state_analysis(state)
	resolve_project_cross_unit_for_units(project.units[:], affected[:], &state.index)
	if project_state_linking_needed(project.units[:], affected[:]) {
		reset_cross_class_member_implementation_links(project.units[:])
		link_class_member_implementations_with_index(project.units[:], state.index.predecessors)
		project_state_add_class_definition_units(project.units[:], &affected)
	}
	resolve_project_open_sql_predicate_names_for_units(project.units[:], affected[:], &state.index)
	lookup := &state.index
	check_project_bodies_for_units(
		&project,
		lookup,
		affected[:],
		pool,
		state.unit_allocators,
		allocator,
	)
	collect_project_diagnostics(&project)
	state.units = project.units
	state.diagnostics = project.diagnostics
	project_state_update_dependency_graph_for_units(state, &project, lookup, affected[:])
	record_project_unresolved_candidates_for_units(state, &project, affected[:])
}

@(private)
project_state_collect_remote_waiters :: proc(
	state: ^Project_State,
	providers: []Unit_Id,
	affected: ^[dynamic]Unit_Id,
	waiters: ^[dynamic]Unit_Id,
) {
	for provider in providers {
		unit_index := unit_id_index(provider)
		for key, units in state.unresolved_candidates {
			if !unit_provides_name(&state.units[unit_index], key.name) {
				continue
			}
			for unit_id in units {
				push_unique_unit(affected, unit_id)
				push_unique_unit(waiters, unit_id)
			}
		}
	}
}

@(private)
project_state_expand_reverse_dependents :: proc(
	state: ^Project_State,
	roots: []Unit_Id,
	affected: ^[dynamic]Unit_Id,
) {
	queue := make([dynamic]Unit_Id, 0, len(roots), context.temp_allocator)
	for root in roots {
		push_unique_unit(&queue, root)
	}
	for cursor := 0; cursor < len(queue); cursor += 1 {
		unit_id := queue[cursor]
		if dependents, ok := state.reverse_edges[unit_id]; ok {
			for dependent in dependents {
				if !unit_list_contains(affected^[:], dependent) {
					push_unique_unit(affected, dependent)
					push_unique_unit(&queue, dependent)
				}
			}
		}
	}
}

@(private)
unit_provides_name :: proc(unit: ^Unit_Analysis, name: string) -> bool {
	for provided in unit.provided_names {
		if strings.equal_fold(provided, name) {
			return true
		}
	}
	if !typepool_dependency_unit(unit.uri) {
		return false
	}
	for &s in unit.symbols {
		if s.scope == unit.root_scope && s.kind == .Constant && strings.equal_fold(s.name, name) {
			return true
		}
	}
	return false
}

@(private)
project_state_prepare_affected_units :: proc(state: ^Project_State, affected: []Unit_Id) {
	for unit_id in affected {
		unit_index := unit_id_index(unit_id)
		unit := &state.units[unit_index]
		clear_unit_reference_resolutions(unit)
		write := 0
		for diagnostic in unit.diagnostics {
			if diagnostic.kind == .Unresolved_Include || diagnostic.kind == .Include_Cycle {
				continue
			}
			unit.diagnostics[write] = diagnostic
			write += 1
		}
		resize(&unit.diagnostics, write)
	}
}

@(private)
clear_unit_reference_resolutions :: proc(unit: ^Unit_Analysis) {
	for &ref in unit.references {
		ref.resolution = {}
		ref.has_resolution = false
	}
}

@(private)
project_state_build_scope_indexes :: proc(
	state: ^Project_State,
	affected: []Unit_Id,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) {
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	indices := unit_ids_to_indices(affected, len(state.units), context.temp_allocator)
	if len(indices) == 0 {
		return
	}
	work := Project_Work_State {
		units           = state.units,
		inputs          = state.inputs,
		unit_allocators = state.unit_allocators,
		allocator       = allocator,
	}
	run_project_tasks(pool, indices[:], &work, build_scope_index_task)
	state.units = work.units
}

@(private)
unit_ids_to_indices :: proc(
	unit_ids: []Unit_Id,
	unit_count: int,
	allocator: mem.Allocator,
) -> [dynamic]int {
	indices := make([dynamic]int, 0, len(unit_ids), allocator)
	for unit_id in unit_ids {
		unit_index := unit_id_index(unit_id)
		append(&indices, unit_index)
	}
	return indices
}

@(private)
temp_arena_begin :: proc() -> Temp_Arena_Marker {
	if context.temp_allocator.procedure != virtual.arena_allocator_proc {
		return {}
	}
	return Temp_Arena_Marker {
		temp = virtual.arena_temp_begin(cast(^virtual.Arena)context.temp_allocator.data),
		active = true,
	}
}

@(private)
temp_arena_end :: proc(marker: Temp_Arena_Marker) {
	if marker.active {
		virtual.arena_temp_end(marker.temp)
	}
}

@(private)
add_unresolved_include_diagnostics_for_units :: proc(
	units: []Unit_Analysis,
	unit_ids: []Unit_Id,
	allocator: mem.Allocator,
) {
	for unit_id in unit_ids {
		unit_index := unit_id_index(unit_id)
		for edge in units[unit_index].include_edges {
			if !edge.has_target && !edge.if_found {
				append(
					&units[unit_index].diagnostics,
					Diagnostic {
						kind = .Unresolved_Include,
						range = edge.range,
						message = diagnostic_message("unresolved include ", edge.name, allocator),
					},
				)
			}
		}
	}
}

@(private)
diagnose_include_cycles_for_units :: proc(
	units: []Unit_Analysis,
	unit_ids: []Unit_Id,
	allocator: mem.Allocator,
) {
	stack := make([dynamic]Unit_Id, 0, len(units), allocator)
	done := make([]bool, len(units), allocator)
	for unit_id in unit_ids {
		unit_index := unit_id_index(unit_id)
		if !done[unit_index] {
			diagnose_include_cycles_from(units, unit_id, &stack, done, allocator)
		}
	}
}

@(private)
resolve_project_cross_unit_for_units :: proc(
	units: []Unit_Analysis,
	affected: []Unit_Id,
	index: ^Project_Index,
) {
	if len(units) == 0 || len(affected) == 0 {
		return
	}

	derive_event_handler_signature_parameters_for_units(
		units,
		affected,
		&index.root_lookup,
		index.class_scope_entries,
		index.visible,
		index.predecessors,
	)
	resolve_project_cross_unit_references_for_units(units, affected, index)
}

@(private)
resolve_project_cross_unit_references_for_units :: proc(
	units: []Unit_Analysis,
	affected: []Unit_Id,
	index: ^Project_Index,
) {
	for unit_id in affected {
		unit_index := unit_id_index(unit_id)
		for ref_index in 0 ..< len(units[unit_index].references) {
			ref := &units[unit_index].references[ref_index]
			if ref.has_resolution {
				continue
			}
			if resolution, ok := resolve_project_reference(
				units,
				unit_index,
				ref^,
				&index.root_lookup,
				index.class_scope_entries,
				index.visible[unit_index],
				index.predecessors[unit_index],
			); ok {
				set_project_reference_resolution(units, ref, resolution)
			}
		}
	}
}

@(private)
derive_event_handler_signature_parameters_for_units :: proc(
	units: []Unit_Analysis,
	affected: []Unit_Id,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [][dynamic]Unit_Id,
	predecessors: [][dynamic]Unit_Id,
) {
	for unit_id in affected {
		unit_index := unit_id_index(unit_id)
		unit := &units[unit_index]
		for &method_symbol in unit.symbols {
			if method_symbol.kind != .Method {
				continue
			}
			method_info := entity_decl_info(unit, method_symbol.id)
			if method_info == nil || method_info.body_scope == INVALID_SCOPE_ID {
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
			if member.symbol == INVALID_SYMBOL_ID ||
			   member_unit_index < 0 ||
			   member_unit_index >= len(units) {
				continue
			}
			member_info := entity_decl_info(&units[member_unit_index], member.symbol)
			if member_info == nil || !(.For_Event in member_info.flags) {
				continue
			}
			_ = derive_event_handler_signature_parameter_types(
				units,
				member_unit_index,
				member,
				roots,
				class_entries,
				visible,
			)
		}
	}
}

@(private)
project_state_linking_needed :: proc(units: []Unit_Analysis, affected: []Unit_Id) -> bool {
	for unit_id in affected {
		unit_index := unit_id_index(unit_id)
		unit := &units[unit_index]
		if len(unit.class_definitions) > 0 {
			return true
		}
		for symbol in unit.symbols {
			if symbol.kind == .Method {
				return true
			}
		}
	}
	return false
}

@(private)
reset_cross_class_member_implementation_links :: proc(units: []Unit_Analysis) {
	for &unit in units {
		for &info in unit.decl_infos {
			if !(.Has_Implementation in info.flags) || info.implementation_unit == unit.unit_id {
				continue
			}
			info.flags -= {.Has_Implementation}
			info.implementation_unit = INVALID_UNIT_ID
			info.implementation_range = {}
		}
	}
}

@(private)
project_state_add_class_definition_units :: proc(
	units: []Unit_Analysis,
	affected: ^[dynamic]Unit_Id,
) {
	for unit in units {
		if len(unit.class_definitions) > 0 {
			push_unique_unit(affected, unit.unit_id)
		}
	}
}

@(private)
resolve_project_open_sql_predicate_names_for_units :: proc(
	units: []Unit_Analysis,
	affected: []Unit_Id,
	index: ^Project_Index,
) {
	for unit_id in affected {
		unit_index := unit_id_index(unit_id)
		unit := &units[unit_index]
		if len(unit.sql_predicate_names) == 0 {
			continue
		}

		remove_materialized_sql_predicate_columns(unit)
		for predicate_name in unit.sql_predicate_names {
			if resolution, ok := resolve_open_sql_predicate_name(
				units,
				unit_index,
				predicate_name,
				index,
			); ok {
				add_resolved_sql_predicate_reference(unit, predicate_name, resolution)
			} else {
				add_sql_predicate_column(unit, predicate_name)
			}
		}
	}
}

@(private)
remove_materialized_sql_predicate_columns :: proc(unit: ^Unit_Analysis) {
	names := make(
		map[Sql_Predicate_Name_Key]bool,
		len(unit.sql_predicate_names),
		context.temp_allocator,
	)
	for predicate_name in unit.sql_predicate_names {
		names[sql_predicate_name_key(predicate_name)] = true
	}
	write := 0
	for ref in unit.sql_name_refs {
		if ref.kind == .Column && sql_name_ref_key(ref) in names {
			continue
		}
		unit.sql_name_refs[write] = ref
		write += 1
	}
	resize(&unit.sql_name_refs, write)
}

@(private)
resolve_open_sql_predicate_name :: proc(
	units: []Unit_Analysis,
	unit_index: int,
	predicate_name: Sql_Predicate_Name_Data,
	index: ^Project_Index,
) -> (
	Resolution,
	bool,
) {
	ref := Reference_Data {
		name      = predicate_name.name,
		namespace = .Value,
		kind      = .Identifier,
		scope     = predicate_name.scope,
		range     = predicate_name.range,
	}
	if resolution, ok := resolve_reference(
		&units[unit_index],
		&units[unit_index].scope_index,
		ref,
	); ok && sql_predicate_resolution_is_host_value(units, resolution) {
		return resolution, true
	}
	if resolution, ok := resolve_project_reference(
		units,
		unit_index,
		ref,
		&index.root_lookup,
		index.class_scope_entries,
		index.visible[unit_index],
		index.predecessors[unit_index],
	); ok && sql_predicate_resolution_is_host_value(units, resolution) {
		return resolution, true
	}
	return {}, false
}

@(private)
sql_predicate_resolution_is_host_value :: proc(
	units: []Unit_Analysis,
	resolution: Resolution,
) -> bool {
	#partial switch resolution.kind {
	case .Symbol:
		unit_index := unit_id_index(resolution.symbol.unit)
		s := symbol(&units[unit_index], resolution.symbol.symbol)
		return s != nil && symbol_kind_occupies(s.kind, .Value)
	case .Internal_Table_Line:
		return true
	case:
		return false
	}
}

@(private)
add_resolved_sql_predicate_reference :: proc(
	unit: ^Unit_Analysis,
	predicate_name: Sql_Predicate_Name_Data,
	resolution: Resolution,
) {
	for &ref in unit.references {
		if ref.namespace == .Value &&
		   ref.kind == .Identifier &&
		   ref.range == predicate_name.range &&
		   ref.name == predicate_name.name {
			ref.resolution = resolution
			ref.has_resolution = true
			return
		}
	}
	id := Reference_Id(u32(len(unit.references)))
	append(
		&unit.references,
		Reference_Data {
			id = id,
			name = predicate_name.name,
			namespace = .Value,
			kind = .Identifier,
			scope = predicate_name.scope,
			range = predicate_name.range,
			resolution = resolution,
			has_resolution = true,
		},
	)
}

@(private)
add_sql_predicate_column :: proc(unit: ^Unit_Analysis, predicate_name: Sql_Predicate_Name_Data) {
	for ref in unit.sql_name_refs {
		if ref.kind == .Column && sql_name_ref_key(ref) == sql_predicate_name_key(predicate_name) {
			return
		}
	}
	append(
		&unit.sql_name_refs,
		Sql_Name_Ref_Data {
			query_id = predicate_name.query_id,
			scope = predicate_name.scope,
			range = predicate_name.range,
			name = predicate_name.name,
			kind = .Column,
			resolution = .Unresolved,
		},
	)
}

@(private)
sql_predicate_name_key :: #force_inline proc(
	name: Sql_Predicate_Name_Data,
) -> Sql_Predicate_Name_Key {
	return Sql_Predicate_Name_Key {
		query_id = name.query_id,
		start = name.range.start,
		end = name.range.end,
		name = name.name,
	}
}

@(private)
sql_name_ref_key :: #force_inline proc(ref: Sql_Name_Ref_Data) -> Sql_Predicate_Name_Key {
	return Sql_Predicate_Name_Key {
		query_id = ref.query_id,
		start = ref.range.start,
		end = ref.range.end,
		name = ref.name,
	}
}

@(private)
project_state_update_dependency_graph_for_units :: proc(
	state: ^Project_State,
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_ids: []Unit_Id,
) {
	assert(len(state.unit_dependencies) >= len(project.units))
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	for unit_id in unit_ids {
		unit_index := unit_id_index(unit_id)
		unit_dependencies := &state.unit_dependencies[unit_index]
		for dependency in unit_dependencies^ {
			project_state_remove_reverse_dependency(state, dependency, unit_id)
		}
		clear(unit_dependencies)

		unit := &project.units[unit_index]
		dependency_seen := make(map[Unit_Id]bool, len(unit.references) + len(unit.include_edges) + 8, context.temp_allocator)
		for edge in unit.include_edges {
			if edge.has_target {
				project_state_add_unit_dependency(
					state,
					unit_dependencies,
					unit.unit_id,
					edge.target,
					&dependency_seen,
				)
			}
		}
		for ref in unit.references {
			if !ref.has_resolution ||
			   ref.resolution.kind != .Symbol ||
			   ref.resolution.symbol.unit == unit.unit_id {
				continue
			}
			project_state_add_unit_dependency(
				state,
				unit_dependencies,
				unit.unit_id,
				ref.resolution.symbol.unit,
				&dependency_seen,
			)
		}
		for sql_source in unit.sql_sources {
			if handle, ok := resolve_type_name_in_project_lookup(
				project,
				lookup,
				unit_index,
				sql_source.name,
			); ok {
				project_state_add_unit_dependency(
					state,
					unit_dependencies,
					unit.unit_id,
					handle.unit,
					&dependency_seen,
				)
			}
		}
		for call_site in unit.call_sites {
			#partial switch call_site.target.kind {
			case .Function:
				if handle, ok := root_symbol_in_unit_lookup(
					project,
					unit.unit_id,
					.Routine,
					call_site.target.function_name,
				); ok {
					project_state_add_unit_dependency(
						state,
						unit_dependencies,
						unit.unit_id,
						handle.unit,
						&dependency_seen,
					)
				}
			case .Report:
				if handle, ok := root_symbol_in_unit_lookup(
					project,
					unit.unit_id,
					.Value,
					call_site.target.report_name,
				); ok {
					project_state_add_unit_dependency(
						state,
						unit_dependencies,
						unit.unit_id,
						handle.unit,
						&dependency_seen,
					)
				}
			case:
			}
		}
	}
}

@(private)
project_state_add_unit_dependency :: proc(
	state: ^Project_State,
	unit_dependencies: ^[dynamic]Unit_Id,
	from, to: Unit_Id,
	dependency_seen: ^map[Unit_Id]bool,
) {
	if from == INVALID_UNIT_ID || to == INVALID_UNIT_ID || from == to {
		return
	}
	if to in dependency_seen^ {
		return
	}
	dependency_seen^[to] = true
	append(unit_dependencies, to)
	if dependents, ok := state.reverse_edges[to]; ok {
		push_unique_unit(&dependents, from)
		state.reverse_edges[to] = dependents
	} else {
		next_dependents := make([dynamic]Unit_Id, 0, 2, state.allocator)
		append(&next_dependents, from)
		state.reverse_edges[to] = next_dependents
	}
}

@(private)
project_state_remove_reverse_dependency :: proc(state: ^Project_State, to, from: Unit_Id) {
	if dependents, dependents_ok := state.reverse_edges[to]; dependents_ok {
		write := 0
		for dependent in dependents {
			if dependent == from {
				continue
			}
			dependents[write] = dependent
			write += 1
		}
		if write == 0 {
			delete(dependents)
			delete_key(&state.reverse_edges, to)
		} else {
			resize(&dependents, write)
			state.reverse_edges[to] = dependents
		}
	}
}

@(private)
unit_interface_signature :: proc(unit: ^Unit_Analysis, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	write_signature_int(&out, int(unit.source_mode))
	for name in unit.provided_names {
		write_signature_string(&out, name)
	}
	for s in unit.symbols {
		if s.scope != unit.root_scope || symbol_kind_is_builtin(s.kind) {
			continue
		}
		write_signature_int(&out, int(s.kind))
		write_signature_string(&out, s.name)
		write_signature_type_ref(&out, s.declared_type)
		write_signature_string(&out, s.type_clause_display)
		write_signature_string(&out, s.value_clause_display)
	}
	for st in unit.structures {
		write_signature_string(&out, st.name)
		for field in st.fields {
			write_signature_string(&out, field.name)
			write_signature_type_ref(&out, field.type_ref)
			write_signature_string(&out, field.value_clause_display)
		}
	}
	for s in unit.symbols {
		info := decl_info(unit, s.decl_info)
		scope_data := scope(unit, s.scope)
		if info == nil ||
		   info.owner == INVALID_SYMBOL_ID ||
		   scope_data == nil ||
		   !(scope_data.kind == .Class || scope_data.kind == .Interface) ||
		   scope_data.owner != info.owner ||
		   info.visibility == .Private {
			continue
		}
		write_class_member_decl_interface_signature(&out, unit, info)
	}
	for s in unit.symbols {
		if s.kind != .Form {
			continue
		}
		info := decl_info(unit, s.decl_info)
		write_signature_string(&out, s.name)
		write_signature_string(&out, info.signature if info != nil else "")
	}
	for s in unit.symbols {
		if s.kind != .Module {
			continue
		}
		write_function_decl_interface_signature(&out, unit, s.id)
	}
	return strings.to_string(out)
}

@(private)
write_class_member_decl_interface_signature :: proc(
	out: ^strings.Builder,
	unit: ^Unit_Analysis,
	info: ^Decl_Info_Data,
) {
	for param in info.signature_parameters {
		_, section_ok := method_section_from_decl(param.section)
		_, passing_ok := parameter_passing_from_decl(param.passing)
		if !section_ok || !passing_ok {
			return
		}
	}
	owner := symbol(unit, info.owner)
	write_signature_string(out, owner.name if owner != nil else "")
	write_signature_int(out, int(info.member_kind))
	write_signature_int(out, int(info.visibility))
	write_signature_string(out, info.name)
	write_signature_string(out, info.signature)
	write_signature_int(out, 1 if .For_Event in info.flags else 0)
	write_signature_string(out, info.event_name)
	write_signature_type_ref(out, info.event_source_type)
	for param in info.signature_parameters {
		section, _ := method_section_from_decl(param.section)
		passing, _ := parameter_passing_from_decl(param.passing)
		write_signature_int(out, int(section))
		write_signature_int(out, int(passing))
		write_signature_string(out, param.name)
		write_signature_type_ref(out, param.declared_type)
		write_signature_string(out, param.type_clause_display)
	}
}

@(private)
write_function_decl_interface_signature :: proc(
	out: ^strings.Builder,
	unit: ^Unit_Analysis,
	symbol_id: Symbol_Id,
) {
	info := entity_decl_info(unit, symbol_id)
	if info == nil {
		return
	}
	for param in info.signature_parameters {
		_, section_ok := function_section_from_decl(param.section)
		_, passing_ok := parameter_passing_from_decl(param.passing)
		if !section_ok || !passing_ok {
			return
		}
	}
	s := symbol(unit, symbol_id)
	write_signature_string(out, s.name if s != nil else info.name)
	write_signature_string(out, info.signature)
	for param in info.signature_parameters {
		section, _ := function_section_from_decl(param.section)
		passing, _ := parameter_passing_from_decl(param.passing)
		write_signature_int(out, int(section))
		write_signature_int(out, int(passing))
		write_signature_string(out, param.name)
		write_signature_type_ref(out, param.declared_type)
		write_signature_string(out, param.type_clause_display)
	}
}

@(private)
method_section_from_decl :: proc(
	section: Decl_Parameter_Section,
) -> (
	Method_Parameter_Section,
	bool,
) {
	#partial switch section {
	case .Method_Importing:
		return .Importing, true
	case .Method_Exporting:
		return .Exporting, true
	case .Method_Changing:
		return .Changing, true
	case .Method_Receiving:
		return .Receiving, true
	case .Method_Returning:
		return .Returning, true
	}
	return .Importing, false
}

@(private)
function_section_from_decl :: proc(
	section: Decl_Parameter_Section,
) -> (
	Function_Module_Parameter_Section,
	bool,
) {
	#partial switch section {
	case .Function_Importing:
		return .Importing, true
	case .Function_Exporting:
		return .Exporting, true
	case .Function_Changing:
		return .Changing, true
	case .Function_Tables:
		return .Tables, true
	}
	return .Importing, false
}

@(private)
parameter_passing_from_decl :: proc(
	passing: Decl_Parameter_Passing,
) -> (
	Parameter_Passing_Kind,
	bool,
) {
	#partial switch passing {
	case .Direct:
		return .Direct, true
	case .Value:
		return .Value, true
	case .Reference:
		return .Reference, true
	}
	return .Direct, false
}

@(private)
write_signature_int :: proc(out: ^strings.Builder, value: int) {
	strings.write_int(out, value)
	strings.write_byte(out, '\t')
}

@(private)
write_signature_string :: proc(out: ^strings.Builder, value: string) {
	strings.write_string(out, value)
	strings.write_byte(out, '\t')
}

@(private)
write_signature_type_ref :: proc(out: ^strings.Builder, ref: Field_Type_Ref_Data) {
	write_signature_int(out, int(ref.namespace))
	write_signature_int(out, 1 if ref.is_ref else 0)
	write_signature_string(out, ref.base_name)
	for field, i in ref.field_path {
		write_signature_string(out, field)
		write_signature_int(out, 1 if i < len(ref.field_derefs) && ref.field_derefs[i] else 0)
		write_signature_int(
			out,
			int(ref.field_selectors[i]) if i < len(ref.field_selectors) else int(ast.Selector_Op.Dash),
		)
	}
	strings.write_byte(out, '\n')
}

@(private)
unit_allocator :: proc(
	unit_allocators: []mem.Allocator,
	unit_index: int,
	fallback: mem.Allocator,
) -> mem.Allocator {
	if 0 <= unit_index &&
	   unit_index < len(unit_allocators) &&
	   unit_allocators[unit_index].procedure != nil {
		return unit_allocators[unit_index]
	}
	return fallback
}

project_analysis_from_units :: proc(
	units: [dynamic]Unit_Analysis,
	allocator: mem.Allocator,
) -> Project_Analysis {
	return Project_Analysis {
		units = units,
		diagnostics = make([dynamic]Diagnostic, 0, 8, allocator),
	}
}

finish_project_analysis :: proc(
	project: ^Project_Analysis,
	pool: ^execution.Pool,
	unit_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	index := project_index_from_units(project.units[:], allocator)
	unit_ids := make([dynamic]Unit_Id, 0, len(project.units), context.temp_allocator)
	for unit in project.units {
		append(&unit_ids, unit.unit_id)
	}
	resolve_project_cross_unit_for_units(project.units[:], unit_ids[:], &index)
	link_class_member_implementations_with_index(project.units[:], index.predecessors)
	resolve_project_open_sql_predicate_names_for_units(project.units[:], unit_ids[:], &index)
	lookup := &index
	check_project_bodies(project, lookup, pool, unit_allocators, allocator)
	collect_project_diagnostics(project)
}

project_unit_by_uri :: proc(project: ^Project_Analysis, uri: string) -> ^Unit_Analysis {
	for &unit in project.units {
		if unit.uri == uri {
			return &unit
		}
	}
	return nil
}

@(private)
parse_collect_input :: proc(
	unit_id: Unit_Id,
	input: Source_Input,
	allocator: mem.Allocator,
) -> Unit_Analysis {
	parsed := parser.parse(input.source, input.uri, allocator)
	return collect_unit(unit_id, input.uri, input.source, parsed, allocator, input.mode)
}

@(private)
dir_is_child :: proc(candidate, parent, child: string) -> bool {
	if len(candidate) != len(parent) + 1 + len(child) {
		return false
	}
	return(
		candidate[:len(parent)] == parent &&
		candidate[len(parent)] == '/' &&
		candidate[len(parent) + 1:] == child \
	)
}

@(private)
run_all_unit_tasks :: proc(
	pool: ^execution.Pool,
	state: ^Project_Work_State,
	work: proc(_: Project_Task_Payload) -> execution.No_Result,
) {
	indices := make([dynamic]int, 0, len(state.units), context.temp_allocator)
	for _, i in state.units {
		append(&indices, i)
	}
	run_project_tasks(pool, indices[:], state, work)
}

@(private)
run_project_tasks :: proc(
	pool: ^execution.Pool,
	unit_indices: []int,
	state: ^Project_Work_State,
	work: proc(_: Project_Task_Payload) -> execution.No_Result,
) {
	graph: execution.Graph
	execution.graph_init(&graph, pool, context.temp_allocator)
	tasks := make(
		[dynamic]execution.Task(execution.No_Result),
		0,
		len(unit_indices),
		context.temp_allocator,
	)
	for unit_index in unit_indices {
		payload := Project_Task_Payload {
			state      = state,
			unit_index = unit_index,
		}
		task := execution.submit_value(&graph, execution.worker_executor(pool), payload, work)
		append(&tasks, task)
	}
	execution.graph_start(&graph)
	execution.graph_wait(&graph)
	execution.graph_destroy(&graph)
}

@(private)
parse_collect_task :: proc(payload: Project_Task_Payload) -> execution.No_Result {
	input := payload.state.inputs[payload.unit_index]
	payload.state.units[payload.unit_index] = parse_collect_input(
		Unit_Id(u32(payload.unit_index)),
		input,
		unit_allocator(payload.state.unit_allocators, payload.unit_index, payload.state.allocator),
	)
	return execution.No_Result{}
}

@(private)
build_scope_index_task :: proc(payload: Project_Task_Payload) -> execution.No_Result {
	unit := &payload.state.units[payload.unit_index]
	scope_index_destroy(&unit.scope_index)
	unit.scope_index = build_scope_index(
		unit,
		unit_allocator(payload.state.unit_allocators, payload.unit_index, payload.state.allocator),
	)
	expand_local_structure_includes(
		unit,
		unit_allocator(payload.state.unit_allocators, payload.unit_index, payload.state.allocator),
	)
	refresh_unit_type_ids(unit)
	return execution.No_Result{}
}

@(private)
add_unresolved_include_diagnostics :: proc(units: []Unit_Analysis, allocator: mem.Allocator) {
	for &unit in units {
		for edge in unit.include_edges {
			if !edge.has_target && !edge.if_found {
				append(
					&unit.diagnostics,
					Diagnostic {
						kind = .Unresolved_Include,
						range = edge.range,
						message = diagnostic_message("unresolved include ", edge.name, allocator),
					},
				)
			}
		}
	}
}

@(private)
diagnose_include_cycles :: proc(units: []Unit_Analysis, allocator: mem.Allocator) {
	stack := make([dynamic]Unit_Id, 0, len(units), allocator)
	done := make([]bool, len(units), allocator)
	for unit, i in units {
		if !done[i] {
			diagnose_include_cycles_from(units, unit.unit_id, &stack, done, allocator)
		}
	}
}

@(private)
diagnose_include_cycles_from :: proc(
	units: []Unit_Analysis,
	unit_id: Unit_Id,
	stack: ^[dynamic]Unit_Id,
	done: []bool,
	allocator: mem.Allocator,
) {
	unit_index := unit_id_index(unit_id)
	if done[unit_index] {
		return
	}
	if unit_in_stack(stack^[:], unit_id) {
		return
	}
	append(stack, unit_id)
	for edge in units[unit_index].include_edges {
		if !edge.has_target || edge.target == INVALID_UNIT_ID {
			continue
		}
		if unit_in_stack(stack^[:], edge.target) {
			append(
				&units[unit_index].diagnostics,
				Diagnostic {
					kind = .Include_Cycle,
					range = edge.range,
					message = diagnostic_message("include cycle at ", edge.name, allocator),
				},
			)
			continue
		}
		diagnose_include_cycles_from(units, edge.target, stack, done, allocator)
	}
	resize(stack, len(stack^) - 1)
	done[unit_index] = true
}

@(private)
unit_in_stack :: proc(stack: []Unit_Id, unit_id: Unit_Id) -> bool {
	for current in stack {
		if current == unit_id {
			return true
		}
	}
	return false
}

@(private)
collect_project_diagnostics :: proc(project: ^Project_Analysis) {
	clear(&project.diagnostics)
	hint := 0
	for unit in project.units {
		hint += len(unit.diagnostics)
	}
	if hint < 8 {
		hint = 8
	}
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	seen := make(map[Diagnostic_Key]bool, hint, context.temp_allocator)
	for unit in project.units {
		for diagnostic in unit.diagnostics {
			key := diagnostic_key(diagnostic)
			if !(key in seen) {
				seen[key] = true
				append(&project.diagnostics, diagnostic)
			}
		}
	}
}

@(private)
check_project_bodies :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	pool: ^execution.Pool,
	unit_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	infer_project_semantic_facts(project, lookup, pool, unit_allocators, allocator)
	validate_project_units(project, lookup, pool, unit_allocators, allocator)
}

@(private)
check_project_bodies_for_units :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_ids: []Unit_Id,
	pool: ^execution.Pool,
	unit_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	infer_project_semantic_facts_for_units(
		project,
		lookup,
		unit_ids,
		pool,
		unit_allocators,
		allocator,
	)
	validate_project_units_for_units(project, lookup, unit_ids, pool, unit_allocators, allocator)
}

@(private)
infer_project_semantic_facts :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	pool: ^execution.Pool,
	unit_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	graph: execution.Graph
	execution.graph_init(&graph, pool, context.temp_allocator)
	defer execution.graph_destroy(&graph)

	for {
		temp_arena := temp_arena_begin()
		inferred := make([]Inferred_Unit_Facts, len(project.units), context.temp_allocator)
		state := Project_Infer_State {
			project         = project,
			lookup          = lookup,
			inferred        = inferred,
			unit_allocators = unit_allocators,
			allocator       = allocator,
		}
		run_infer_tasks(&graph, &state)
		changed := apply_inferred_project_facts(project, inferred)
		temp_arena_end(temp_arena)
		if !changed {
			break
		}
	}
}

@(private)
infer_project_semantic_facts_for_units :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_ids: []Unit_Id,
	pool: ^execution.Pool,
	unit_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	indices := unit_ids_to_indices(unit_ids, len(project.units), context.temp_allocator)
	if len(indices) == 0 {
		return
	}
	graph: execution.Graph
	execution.graph_init(&graph, pool, context.temp_allocator)
	defer execution.graph_destroy(&graph)

	for {
		temp_arena := temp_arena_begin()
		inferred := make([]Inferred_Unit_Facts, len(indices), context.temp_allocator)
		state := Project_Infer_State {
			project         = project,
			lookup          = lookup,
			inferred        = inferred,
			unit_allocators = unit_allocators,
			allocator       = allocator,
		}
		run_infer_tasks_for_indices(&graph, &state, indices[:])
		changed := apply_inferred_project_facts_for_indices(project, inferred, indices[:])
		temp_arena_end(temp_arena)
		if !changed {
			break
		}
	}
}

@(private)
validate_project_units :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	pool: ^execution.Pool,
	unit_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	diagnostics := make([][dynamic]Diagnostic, len(project.units), context.temp_allocator)
	state := Project_Validate_State {
		project         = project,
		lookup          = lookup,
		diagnostics     = diagnostics,
		unit_allocators = unit_allocators,
		allocator       = allocator,
	}
	run_validate_tasks(pool, &state)
	for i in 0 ..< len(project.units) {
		delete(project.units[i].diagnostics)
		project.units[i].diagnostics = diagnostics[i]
	}
}

@(private)
validate_project_units_for_units :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	unit_ids: []Unit_Id,
	pool: ^execution.Pool,
	unit_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	indices := unit_ids_to_indices(unit_ids, len(project.units), context.temp_allocator)
	if len(indices) == 0 {
		return
	}
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	diagnostics := make([][dynamic]Diagnostic, len(indices), context.temp_allocator)
	state := Project_Validate_State {
		project         = project,
		lookup          = lookup,
		diagnostics     = diagnostics,
		unit_allocators = unit_allocators,
		allocator       = allocator,
	}
	run_validate_tasks_for_indices(pool, &state, indices[:])
	for unit_index, i in indices {
		delete(project.units[unit_index].diagnostics)
		project.units[unit_index].diagnostics = diagnostics[i]
	}
}

@(private)
run_infer_tasks :: proc(graph: ^execution.Graph, state: ^Project_Infer_State) {
	exec := execution.worker_executor(graph.pool)
	for unit_index in 0 ..< len(state.project.units) {
		payload := Project_Infer_Payload {
			state        = state,
			unit_index   = unit_index,
			output_index = unit_index,
		}
		_ = execution.submit_value(graph, exec, payload, infer_task)
	}
	execution.graph_start(graph)
	execution.graph_wait(graph)
	execution.graph_reset(graph)
}

@(private)
run_infer_tasks_for_indices :: proc(
	graph: ^execution.Graph,
	state: ^Project_Infer_State,
	indices: []int,
) {
	exec := execution.worker_executor(graph.pool)
	for unit_index, i in indices {
		payload := Project_Infer_Payload {
			state        = state,
			unit_index   = unit_index,
			output_index = i,
		}
		_ = execution.submit_value(graph, exec, payload, infer_task)
	}
	execution.graph_start(graph)
	execution.graph_wait(graph)
	execution.graph_reset(graph)
}

@(private)
run_validate_tasks :: proc(pool: ^execution.Pool, state: ^Project_Validate_State) {
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	graph: execution.Graph
	execution.graph_init(&graph, pool, context.temp_allocator)
	tasks := make(
		[dynamic]execution.Task(execution.No_Result),
		0,
		len(state.project.units),
		context.temp_allocator,
	)
	for unit_index in 0 ..< len(state.project.units) {
		payload := Project_Validate_Payload {
			state        = state,
			unit_index   = unit_index,
			output_index = unit_index,
		}
		task := execution.submit_value(
			&graph,
			execution.worker_executor(pool),
			payload,
			validate_task,
		)
		append(&tasks, task)
	}
	execution.graph_start(&graph)
	for task in tasks {
		_ = execution.wait(task)
	}
	execution.graph_wait(&graph)
	execution.graph_destroy(&graph)
}

@(private)
run_validate_tasks_for_indices :: proc(
	pool: ^execution.Pool,
	state: ^Project_Validate_State,
	indices: []int,
) {
	graph: execution.Graph
	execution.graph_init(&graph, pool, context.temp_allocator)
	tasks := make(
		[dynamic]execution.Task(execution.No_Result),
		0,
		len(indices),
		context.temp_allocator,
	)
	for unit_index, i in indices {
		payload := Project_Validate_Payload {
			state        = state,
			unit_index   = unit_index,
			output_index = i,
		}
		task := execution.submit_value(
			&graph,
			execution.worker_executor(pool),
			payload,
			validate_task,
		)
		append(&tasks, task)
	}
	execution.graph_start(&graph)
	for task in tasks {
		_ = execution.wait(task)
	}
	execution.graph_wait(&graph)
	execution.graph_destroy(&graph)
}

@(private)
infer_task :: proc(payload: Project_Infer_Payload) -> execution.No_Result {
	payload.state.inferred[payload.output_index] = infer_unit_semantic_facts(
		payload.state.project,
		payload.state.lookup,
		payload.unit_index,
		unit_allocator(payload.state.unit_allocators, payload.unit_index, payload.state.allocator),
	)
	return execution.No_Result{}
}

@(private)
validate_task :: proc(payload: Project_Validate_Payload) -> execution.No_Result {
	payload.state.diagnostics[payload.output_index] = validate_unit_diagnostics(
		payload.state.project,
		payload.state.lookup,
		payload.unit_index,
		unit_allocator(payload.state.unit_allocators, payload.unit_index, payload.state.allocator),
	)
	return execution.No_Result{}
}

@(private)
diagnostic_message :: proc(prefix, name: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, prefix)
	strings.write_byte(&out, '\'')
	strings.write_string(&out, name)
	strings.write_byte(&out, '\'')
	return strings.to_string(out)
}

link_class_member_implementations_with_index :: proc(
	units: []Unit_Analysis,
	predecessors: [][dynamic]Unit_Id,
) {
	for impl_unit_index in 0 ..< len(units) {
		for method_symbol in units[impl_unit_index].symbols {
			if method_symbol.kind != .Method {
				continue
			}
			class_symbol, ok := enclosing_class_owner_unit(
				&units[impl_unit_index],
				method_symbol.scope,
			)
			if !ok {
				continue
			}
			class_name := symbol(&units[impl_unit_index], class_symbol).name
			for i := len(predecessors[impl_unit_index]) - 1; i >= 0; i -= 1 {
				def_unit := predecessors[impl_unit_index][i]
				class_handle, class_ok := root_symbol_in_unit(units, def_unit, .Type, class_name)
				if !class_ok ||
				   !unit_has_class_definition(
						   &units[unit_id_index(def_unit)],
						   class_handle.symbol,
					   ) {
					continue
				}
				member := unit_class_member_symbol(
					&units[unit_id_index(def_unit)],
					class_handle.symbol,
					method_symbol.name,
				)
				def_unit_index := unit_id_index(def_unit)
				if member != nil && member.kind == .Method {
					if info := entity_decl_info(&units[def_unit_index], member.id);
					   info != nil && !(.Has_Implementation in info.flags) {
						info.implementation_unit = units[impl_unit_index].unit_id
						info.implementation_range = method_symbol.decl_range
						info.flags += {.Has_Implementation}
					}
					break
				}
			}
		}
	}
}

@(private)
uri_parent_dir_key :: proc(uri: string, allocator: mem.Allocator) -> string {
	normalized := normalized_uri_path_key(uri, allocator)
	for i := len(normalized) - 1; i >= 0; i -= 1 {
		if normalized[i] == '/' {
			return normalized[:i]
		}
	}
	return ""
}

@(private)
normalized_uri_path_key :: proc(uri: string, allocator: mem.Allocator) -> string {
	return uri_key.normalized_uri_path_key(uri, allocator)
}
