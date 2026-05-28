package abap_frontend_semantic_analyze

import execution "src:execution"
import "src:parser"
import uri_key "src:uri_key"

import base_runtime "base:runtime"
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

Remote_Dependency_Kind :: enum {
	Include,
	Message_Class,
	Report,
	Function,
	Static,
	Type,
	Symbol,
}

Remote_Dependency_Hint :: enum {
	None,
	Object_Type,
	Interface_Type,
}

Remote_Dependency_Candidate :: struct {
	name: string,
	kind: Remote_Dependency_Kind,
	hint: Remote_Dependency_Hint,
}

Remote_Dependency_Key :: struct {
	name: string,
	kind: Remote_Dependency_Kind,
	hint: Remote_Dependency_Hint,
}

Project_Dependency_Kind :: enum {
	Include,
	Type,
	Static,
	Message,
	Sql,
	Call,
}

Project_Dependency_Edge :: struct {
	from: Unit_Id,
	to:   Unit_Id,
	kind: Project_Dependency_Kind,
	name: string,
}

Reverse_Dependency_Key :: struct {
	from: Unit_Id,
	to:   Unit_Id,
}

Project_State :: struct {
	inputs:                [dynamic]Source_Input,
	units:                 [dynamic]Unit_Analysis,
	uri_to_unit:           map[string]Unit_Id,
	edges:                 [dynamic]Project_Dependency_Edge,
	reverse_edges:         map[Unit_Id][dynamic]Unit_Id,
	unresolved_candidates: map[Remote_Dependency_Key][dynamic]Unit_Id,
	diagnostics:           [dynamic]Diagnostic,
	index:                 Project_Index,
	candidates:            [dynamic]Project_Candidate_Input,
	candidate_to_unit:     [dynamic]Unit_Id,
	candidate_dirs:        [dynamic]string,
	unit_dirs:             [dynamic]string,
	unit_candidate_index:  [dynamic]int,
	interface_signatures:  [dynamic]string,
	unit_allocators:       []mem.Allocator,
	allocator:             mem.Allocator,
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
	lookup:          ^Validation_Lookup,
	inferred:        []Inferred_Unit_Facts,
	unit_allocators: []mem.Allocator,
	allocator:       mem.Allocator,
}

Project_Validate_State :: struct {
	project:         ^Project_Analysis,
	lookup:          ^Validation_Lookup,
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
		edges = make([dynamic]Project_Dependency_Edge, 0, 16, allocator),
		reverse_edges = make(map[Unit_Id][dynamic]Unit_Id, 16, allocator),
		unresolved_candidates = make(map[Remote_Dependency_Key][dynamic]Unit_Id, 16, allocator),
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
			if unit_index >= 0 &&
			   unit_index < len(state.unit_candidate_index) &&
			   state.unit_candidate_index[unit_index] < 0 {
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
		if unit_index < 0 || unit_index >= len(state.inputs) {
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
		if unit_index >= 0 &&
		   unit_index < len(state.unit_candidate_index) &&
		   state.unit_candidate_index[unit_index] < 0 {
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
		if unit_index >= 0 && unit_index < len(state.units) {
			append(&indices, unit_index)
		}
	}
	if len(indices) == 0 {
		return
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
project_state_resolve_include_edges :: proc(
	state: ^Project_State,
	roots: []Unit_Id,
	new_units: ^[dynamic]Unit_Id,
	allocator: mem.Allocator,
) {
	for unit_id in roots {
		unit_index := unit_id_index(unit_id)
		if unit_index < 0 || unit_index >= len(state.units) {
			continue
		}
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
	if unit_index < 0 || unit_index >= len(state.units) {
		return false
	}
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
		if unit_index < 0 || unit_index >= len(state.units) {
			continue
		}
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
		if unit_index >= 0 && unit_index < len(state.units) {
			resolve_unit_with_index(&state.units[unit_index], &state.units[unit_index].scope_index)
		}
	}
	add_unresolved_include_diagnostics_for_units(state.units[:], affected[:], allocator)
	diagnose_include_cycles_for_units(state.units[:], affected[:], allocator)
	project_index_update_units(&state.index, state.units[:], affected[:])
	project_index_update_include_graph(&state.index, state.units[:], affected[:])

	project := project_state_analysis(state)
	resolve_project_cross_unit_for_units(project.units[:], affected[:], &state.index, allocator)
	if project_state_linking_needed(project.units[:], affected[:]) {
		reset_cross_class_member_implementation_links(project.units[:])
		link_class_member_implementations_with_index(
			project.units[:],
			&state.index.root_lookup,
			state.index.predecessors,
		)
		project_state_add_class_definition_units(project.units[:], &affected)
	}
	reclassify_project_open_sql_predicate_host_variables_for_units(
		project.units[:],
		affected[:],
		allocator,
	)
	project_index_update_sql_predicate_columns(&state.index, project.units[:], affected[:])
	lookup := validation_lookup_from_project_index(&state.index)
	infer_project_semantic_facts_for_units(
		&project,
		&lookup,
		affected[:],
		pool,
		state.unit_allocators,
		allocator,
	)
	validate_project_units_for_units(
		&project,
		&lookup,
		affected[:],
		pool,
		state.unit_allocators,
		allocator,
	)
	rebuild_project_semantic_indexes_for_units(
		&project,
		affected[:],
		pool,
		state.unit_allocators,
		allocator,
	)
	collect_project_diagnostics(&project)
	state.units = project.units
	state.diagnostics = project.diagnostics
	project_state_update_dependency_graph_for_units(state, &project, &lookup, affected[:])
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
		if unit_index < 0 || unit_index >= len(state.units) {
			continue
		}
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
	return false
}

@(private)
project_state_prepare_affected_units :: proc(state: ^Project_State, affected: []Unit_Id) {
	for unit_id in affected {
		unit_index := unit_id_index(unit_id)
		if unit_index < 0 || unit_index >= len(state.units) {
			continue
		}
		unit := &state.units[unit_index]
		for &ref in unit.references {
			ref.resolution = {}
			ref.has_resolution = false
		}
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
		if unit_index >= 0 && unit_index < unit_count {
			append(&indices, unit_index)
		}
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
		if unit_index < 0 || unit_index >= len(units) {
			continue
		}
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
		if unit_index >= 0 && unit_index < len(units) && !done[unit_index] {
			diagnose_include_cycles_from(units, unit_id, &stack, done, allocator)
		}
	}
}

@(private)
resolve_project_cross_unit_for_units :: proc(
	units: []Unit_Analysis,
	affected: []Unit_Id,
	index: ^Project_Index,
	allocator: mem.Allocator,
) {
	if len(units) == 0 || len(affected) == 0 {
		return
	}

	for unit_id in affected {
		unit_index := unit_id_index(unit_id)
		if unit_index < 0 || unit_index >= len(units) {
			continue
		}
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
				ref.resolution = resolution
				ref.has_resolution = true
			}
		}
	}

	if seed_inherited_method_scope_parameters_for_units(
		units,
		affected,
		&index.root_lookup,
		index.class_scope_entries,
		index.visible,
		index.predecessors,
		allocator,
	) {
		for unit_id in affected {
			unit_index := unit_id_index(unit_id)
			if unit_index < 0 || unit_index >= len(units) {
				continue
			}
			scope_index_destroy(&units[unit_index].scope_index)
			units[unit_index].scope_index = build_scope_index(&units[unit_index], allocator)
			resolve_unit_with_index(&units[unit_index], &units[unit_index].scope_index)
		}
	}

	changed := true
	for changed {
		changed = false
		for unit_id in affected {
			unit_index := unit_id_index(unit_id)
			if unit_index >= 0 && unit_index < len(units) {
				changed =
					import_project_structures_for_unit(
						units,
						unit_index,
						&index.root_lookup,
						index.visible[unit_index],
						allocator,
					) ||
					changed
			}
		}
	}
}

@(private)
seed_inherited_method_scope_parameters_for_units :: proc(
	units: []Unit_Analysis,
	affected: []Unit_Id,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [][dynamic]Unit_Id,
	predecessors: [][dynamic]Unit_Id,
	allocator: mem.Allocator,
) -> bool {
	changed := false
	for unit_id in affected {
		unit_index := unit_id_index(unit_id)
		if unit_index < 0 || unit_index >= len(units) {
			continue
		}
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

@(private)
project_state_linking_needed :: proc(units: []Unit_Analysis, affected: []Unit_Id) -> bool {
	for unit_id in affected {
		unit_index := unit_id_index(unit_id)
		if unit_index < 0 || unit_index >= len(units) {
			continue
		}
		unit := &units[unit_index]
		if len(unit.class_members) > 0 || len(unit.class_definitions) > 0 {
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
		for &member in unit.class_members {
			if !(.Has_Implementation in member.flags) ||
			   member.implementation.unit == unit.unit_id {
				continue
			}
			member.flags -= {.Has_Implementation, .Has_Implementation_Range}
			member.implementation = {}
			member.implementation_range = {}
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
reclassify_project_open_sql_predicate_host_variables_for_units :: proc(
	units: []Unit_Analysis,
	affected: []Unit_Id,
	allocator: mem.Allocator,
) {
	roots := make([dynamic]Symbol_Handle, 0, 8, allocator)
	names := make([dynamic]string, 0, 8, allocator)
	for unit in units {
		for s in unit.symbols {
			if s.scope == unit.root_scope && symbol_kind_occupies(s.kind, .Value) {
				if !string_list_contains(names[:], s.name) {
					append(&names, s.name)
					append(&roots, Symbol_Handle{unit = unit.unit_id, symbol = s.id})
				}
			}
		}
	}
	for unit_id in affected {
		unit_index := unit_id_index(unit_id)
		if unit_index < 0 || unit_index >= len(units) {
			continue
		}
		next_refs := make(
			[dynamic]Sql_Name_Ref_Data,
			0,
			len(units[unit_index].sql_name_refs),
			allocator,
		)
		for sql_ref in units[unit_index].sql_name_refs {
			if sql_ref.kind == .Column &&
			   sql_ref_in_predicate(units[unit_index].sql_predicates[:], sql_ref) {
				if root_index := string_list_index(names[:], sql_ref.name); root_index >= 0 {
					add_reclassified_sql_reference(&units[unit_index], sql_ref, roots[root_index])
					continue
				}
			}
			append(&next_refs, sql_ref)
		}
		units[unit_index].sql_name_refs = next_refs
	}
}

@(private)
project_state_rebuild_dependency_graph :: proc(
	state: ^Project_State,
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
) {
	project_state_reverse_edges_destroy(state)
	clear(&state.edges)
	graph_allocator := base_runtime.heap_allocator()
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	edge_seen := make(
		map[Project_Dependency_Edge]bool,
		len(project.units) * 4 + 64,
		context.temp_allocator,
	)
	reverse_seen := make(
		map[Reverse_Dependency_Key]bool,
		len(project.units) * 2 + 8,
		context.temp_allocator,
	)
	state.reverse_edges = make(
		map[Unit_Id][dynamic]Unit_Id,
		len(project.units) * 2 + 8,
		graph_allocator,
	)
	for unit, unit_index in project.units {
		for edge in unit.include_edges {
			if edge.has_target {
				project_state_add_dependency_edge(
					state,
					unit.unit_id,
					edge.target,
					.Include,
					edge.name,
					&edge_seen,
					&reverse_seen,
					graph_allocator,
				)
			}
		}
		for ref in unit.references {
			if !ref.has_resolution ||
			   ref.resolution.kind != .Symbol ||
			   ref.resolution.symbol.unit == unit.unit_id {
				continue
			}
			project_state_add_dependency_edge(
				state,
				unit.unit_id,
				ref.resolution.symbol.unit,
				project_dependency_kind_for_reference(ref),
				ref.name,
				&edge_seen,
				&reverse_seen,
				graph_allocator,
			)
		}
		for sql_source in unit.sql_sources {
			if handle, ok := resolve_type_name_in_project_lookup(
				project,
				lookup,
				unit_index,
				sql_source.name,
			); ok {
				project_state_add_dependency_edge(
					state,
					unit.unit_id,
					handle.unit,
					.Sql,
					sql_source.name,
					&edge_seen,
					&reverse_seen,
					graph_allocator,
				)
			}
		}
		for call_site in unit.call_sites {
			#partial switch call_site.target.kind {
			case .Function:
				if handle, ok := root_symbol_in_unit_lookup(
					lookup,
					unit.unit_id,
					.Routine,
					call_site.target.function_name,
				); ok {
					project_state_add_dependency_edge(
						state,
						unit.unit_id,
						handle.unit,
						.Call,
						call_site.target.function_name,
						&edge_seen,
						&reverse_seen,
						graph_allocator,
					)
				}
			case .Report:
				if handle, ok := root_symbol_in_unit_lookup(
					lookup,
					unit.unit_id,
					.Value,
					call_site.target.report_name,
				); ok {
					project_state_add_dependency_edge(
						state,
						unit.unit_id,
						handle.unit,
						.Call,
						call_site.target.report_name,
						&edge_seen,
						&reverse_seen,
						graph_allocator,
					)
				}
			case:
			}
		}
	}
}

@(private)
project_state_update_dependency_graph_for_units :: proc(
	state: ^Project_State,
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
	unit_ids: []Unit_Id,
) {
	project_index_ensure_unit_count(&state.index, len(project.units))
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	for unit_id in unit_ids {
		unit_index := unit_id_index(unit_id)
		if unit_index < 0 || unit_index >= len(project.units) {
			continue
		}
		data := &state.index.unit_entries[unit_index]
		for edge in data.dependency_edges {
			project_state_decrement_dependency_pair(state, edge.from, edge.to)
		}
		project_state_remove_dependency_edges_from_unit(state, unit_id)
		clear(&data.dependency_edges)

		unit := &project.units[unit_index]
		graph_allocator := state.index.allocator
		edge_seen := make(
			map[Project_Dependency_Edge]bool,
			len(unit.references) + len(unit.include_edges) + 8,
			context.temp_allocator,
		)
		for edge in unit.include_edges {
			if edge.has_target {
				project_state_add_dependency_edge_for_unit(
					state,
					data,
					unit.unit_id,
					edge.target,
					.Include,
					edge.name,
					&edge_seen,
					graph_allocator,
				)
			}
		}
		for ref in unit.references {
			if !ref.has_resolution ||
			   ref.resolution.kind != .Symbol ||
			   ref.resolution.symbol.unit == unit.unit_id {
				continue
			}
			project_state_add_dependency_edge_for_unit(
				state,
				data,
				unit.unit_id,
				ref.resolution.symbol.unit,
				project_dependency_kind_for_reference(ref),
				ref.name,
				&edge_seen,
				graph_allocator,
			)
		}
		for sql_source in unit.sql_sources {
			if handle, ok := resolve_type_name_in_project_lookup(
				project,
				lookup,
				unit_index,
				sql_source.name,
			); ok {
				project_state_add_dependency_edge_for_unit(
					state,
					data,
					unit.unit_id,
					handle.unit,
					.Sql,
					sql_source.name,
					&edge_seen,
					graph_allocator,
				)
			}
		}
		for call_site in unit.call_sites {
			#partial switch call_site.target.kind {
			case .Function:
				if handle, ok := root_symbol_in_unit_lookup(
					lookup,
					unit.unit_id,
					.Routine,
					call_site.target.function_name,
				); ok {
					project_state_add_dependency_edge_for_unit(
						state,
						data,
						unit.unit_id,
						handle.unit,
						.Call,
						call_site.target.function_name,
						&edge_seen,
						graph_allocator,
					)
				}
			case .Report:
				if handle, ok := root_symbol_in_unit_lookup(
					lookup,
					unit.unit_id,
					.Value,
					call_site.target.report_name,
				); ok {
					project_state_add_dependency_edge_for_unit(
						state,
						data,
						unit.unit_id,
						handle.unit,
						.Call,
						call_site.target.report_name,
						&edge_seen,
						graph_allocator,
					)
				}
			case:
			}
		}
	}
}

@(private)
project_state_remove_dependency_edges_from_unit :: proc(state: ^Project_State, unit_id: Unit_Id) {
	write := 0
	for edge in state.edges {
		if edge.from == unit_id {
			continue
		}
		state.edges[write] = edge
		write += 1
	}
	resize(&state.edges, write)
}

@(private)
project_state_add_dependency_edge_for_unit :: proc(
	state: ^Project_State,
	data: ^Project_Index_Unit,
	from, to: Unit_Id,
	kind: Project_Dependency_Kind,
	name: string,
	edge_seen: ^map[Project_Dependency_Edge]bool,
	allocator: mem.Allocator,
) {
	if from == INVALID_UNIT_ID || to == INVALID_UNIT_ID || from == to {
		return
	}
	edge := Project_Dependency_Edge {
		from = from,
		to   = to,
		kind = kind,
		name = name,
	}
	if edge in edge_seen^ {
		return
	}
	edge_seen^[edge] = true
	append(&data.dependency_edges, edge)
	append(&state.edges, edge)
	project_state_increment_dependency_pair(state, from, to, allocator)
}

@(private)
project_state_increment_dependency_pair :: proc(
	state: ^Project_State,
	from, to: Unit_Id,
	allocator: mem.Allocator,
) {
	key := Reverse_Dependency_Key {
		from = from,
		to   = to,
	}
	if count, ok := state.index.dependency_pair_counts[key]; ok {
		state.index.dependency_pair_counts[key] = count + 1
		return
	}
	state.index.dependency_pair_counts[key] = 1
	if dependents, ok := state.reverse_edges[to]; ok {
		push_unique_unit(&dependents, from)
		state.reverse_edges[to] = dependents
	} else {
		next_dependents := make([dynamic]Unit_Id, 0, 2, allocator)
		append(&next_dependents, from)
		state.reverse_edges[to] = next_dependents
	}
}

@(private)
project_state_decrement_dependency_pair :: proc(state: ^Project_State, from, to: Unit_Id) {
	key := Reverse_Dependency_Key {
		from = from,
		to   = to,
	}
	count, ok := state.index.dependency_pair_counts[key]
	if !ok {
		return
	}
	if count > 1 {
		state.index.dependency_pair_counts[key] = count - 1
		return
	}
	delete_key(&state.index.dependency_pair_counts, key)
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
project_state_reverse_edges_destroy :: proc(state: ^Project_State) {
	for _, dependents in state.reverse_edges {
		delete(dependents)
	}
	delete(state.reverse_edges)
}

@(private)
project_state_add_dependency_edge :: proc(
	state: ^Project_State,
	from, to: Unit_Id,
	kind: Project_Dependency_Kind,
	name: string,
	edge_seen: ^map[Project_Dependency_Edge]bool,
	reverse_seen: ^map[Reverse_Dependency_Key]bool,
	allocator: mem.Allocator,
) {
	if from == INVALID_UNIT_ID || to == INVALID_UNIT_ID || from == to {
		return
	}
	edge := Project_Dependency_Edge {
		from = from,
		to   = to,
		kind = kind,
		name = name,
	}
	if edge in edge_seen^ {
		return
	}
	edge_seen^[edge] = true
	append(&state.edges, edge)
	reverse_key := Reverse_Dependency_Key {
		from = from,
		to   = to,
	}
	if reverse_key in reverse_seen^ {
		return
	}
	reverse_seen^[reverse_key] = true
	if dependents, ok := state.reverse_edges[to]; ok {
		append(&dependents, from)
		state.reverse_edges[to] = dependents
	} else {
		next_dependents := make([dynamic]Unit_Id, 0, 2, allocator)
		append(&next_dependents, from)
		state.reverse_edges[to] = next_dependents
	}
}

@(private)
project_dependency_kind_for_reference :: proc(ref: Reference_Data) -> Project_Dependency_Kind {
	#partial switch ref.kind {
	case .Static_Target:
		return .Static
	case .Message_Class:
		return .Message
	case .Routine_Call:
		return .Call
	case:
	}
	if ref.namespace == .Type {
		return .Type
	}
	return .Call
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
	for member in unit.class_members {
		if member.visibility == .Private {
			continue
		}
		write_signature_int(&out, symbol_id_index(member.class_symbol))
		write_signature_int(&out, int(member.kind))
		write_signature_int(&out, int(member.visibility))
		write_signature_string(&out, member.name)
		write_signature_string(&out, member.signature)
		for param in member.parameters {
			write_signature_int(&out, int(param.section))
			write_signature_int(&out, int(param.passing))
			write_signature_string(&out, param.name)
			write_signature_type_ref(&out, param.declared_type)
			write_signature_string(&out, param.type_clause_display)
		}
	}
	for routine in unit.form_routines {
		write_signature_int(&out, symbol_id_index(routine.symbol))
		write_signature_string(&out, routine.signature)
	}
	for module in unit.function_modules {
		write_signature_int(&out, symbol_id_index(module.symbol))
		write_signature_string(&out, module.signature)
		for param in module.parameters {
			write_signature_int(&out, int(param.section))
			write_signature_int(&out, int(param.passing))
			write_signature_string(&out, param.name)
			write_signature_type_ref(&out, param.declared_type)
			write_signature_string(&out, param.type_clause_display)
		}
	}
	return strings.to_string(out)
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
	for field in ref.field_path {
		write_signature_string(out, field)
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
	resolve_project_cross_unit(project.units[:], allocator)
	link_class_member_implementations(project.units[:], allocator)
	reclassify_project_open_sql_predicate_host_variables(project.units[:], allocator)
	lookup := build_validation_lookup(project, allocator)
	infer_project_semantic_facts(project, &lookup, pool, unit_allocators, allocator)
	validate_project_units(project, &lookup, pool, unit_allocators, allocator)
	rebuild_project_semantic_indexes(project, pool, unit_allocators, allocator)
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
	parse_arena: virtual.Arena
	_ = virtual.arena_init_growing(&parse_arena)
	defer virtual.arena_destroy(&parse_arena)
	parsed := parser.parse(input.source, input.uri, virtual.arena_allocator(&parse_arena))
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
	return execution.No_Result{}
}

@(private)
rebuild_semantic_index_task :: proc(payload: Project_Task_Payload) -> execution.No_Result {
	rebuild_semantic_index(
		&payload.state.units[payload.unit_index],
		unit_allocator(payload.state.unit_allocators, payload.unit_index, payload.state.allocator),
	)
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
	if unit_index < 0 || unit_index >= len(units) {
		return
	}
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
infer_project_semantic_facts :: proc(
	project: ^Project_Analysis,
	lookup: ^Validation_Lookup,
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
	lookup: ^Validation_Lookup,
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
	lookup: ^Validation_Lookup,
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
	lookup: ^Validation_Lookup,
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
rebuild_project_semantic_indexes :: proc(
	project: ^Project_Analysis,
	pool: ^execution.Pool,
	unit_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	state := Project_Work_State {
		units           = project.units,
		inputs          = make([dynamic]Source_Input, 0, 0, allocator),
		unit_allocators = unit_allocators,
		allocator       = allocator,
	}
	run_all_unit_tasks(pool, &state, rebuild_semantic_index_task)
	project.units = state.units
}

@(private)
rebuild_project_semantic_indexes_for_units :: proc(
	project: ^Project_Analysis,
	unit_ids: []Unit_Id,
	pool: ^execution.Pool,
	unit_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	indices := unit_ids_to_indices(unit_ids, len(project.units), context.temp_allocator)
	if len(indices) == 0 {
		return
	}
	state := Project_Work_State {
		units           = project.units,
		inputs          = make([dynamic]Source_Input, 0, 0, context.temp_allocator),
		unit_allocators = unit_allocators,
		allocator       = allocator,
	}
	run_project_tasks(pool, indices[:], &state, rebuild_semantic_index_task)
	project.units = state.units
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
	strings.write_string(&out, name)
	return strings.to_string(out)
}

@(private)
link_class_member_implementations :: proc(units: []Unit_Analysis, allocator: mem.Allocator) {
	predecessors := include_predecessor_units_for_units(units, allocator)
	roots := build_project_root_index(units, allocator)
	root_lookup := build_project_root_lookup(units, roots[:], allocator)
	link_class_member_implementations_with_index(units, &root_lookup, predecessors)
}

@(private)
link_class_member_implementations_with_index :: proc(
	units: []Unit_Analysis,
	root_lookup: ^Project_Root_Lookup,
	predecessors: [][dynamic]Unit_Id,
) {
	for unit_index in 0 ..< len(units) {
		for member_index in 0 ..< len(units[unit_index].class_members) {
			member := &units[unit_index].class_members[member_index]
			if .Has_Implementation_Range in member.flags {
				member.implementation = Class_Member_Implementation_Data {
					unit  = units[unit_index].unit_id,
					range = member.implementation_range,
				}
				member.flags += {.Has_Implementation}
			}
		}
	}
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
				class_handle, class_ok := root_symbol_in_unit(
					root_lookup,
					def_unit,
					.Type,
					class_name,
				)
				if !class_ok ||
				   !unit_has_class_definition(
						   &units[unit_id_index(def_unit)],
						   class_handle.symbol,
					   ) {
					continue
				}
				member := unit_class_member(
					&units[unit_id_index(def_unit)],
					class_handle.symbol,
					method_symbol.name,
				)
				if member != nil &&
				   member.kind == .Method &&
				   !(.Has_Implementation in member.flags) {
					member.implementation = Class_Member_Implementation_Data {
						unit  = units[impl_unit_index].unit_id,
						range = method_symbol.decl_range,
					}
					member.implementation_range = method_symbol.decl_range
					member.flags += {.Has_Implementation, .Has_Implementation_Range}
					break
				}
			}
		}
	}
}

@(private)
reclassify_project_open_sql_predicate_host_variables :: proc(
	units: []Unit_Analysis,
	allocator: mem.Allocator,
) {
	roots := make([dynamic]Symbol_Handle, 0, 8, allocator)
	names := make([dynamic]string, 0, 8, allocator)
	for unit in units {
		for s in unit.symbols {
			if s.scope == unit.root_scope && symbol_kind_occupies(s.kind, .Value) {
				if !string_list_contains(names[:], s.name) {
					append(&names, s.name)
					append(&roots, Symbol_Handle{unit = unit.unit_id, symbol = s.id})
				}
			}
		}
	}
	for unit_index in 0 ..< len(units) {
		next_refs := make(
			[dynamic]Sql_Name_Ref_Data,
			0,
			len(units[unit_index].sql_name_refs),
			allocator,
		)
		for sql_ref in units[unit_index].sql_name_refs {
			if sql_ref.kind == .Column &&
			   sql_ref_in_predicate(units[unit_index].sql_predicates[:], sql_ref) {
				if root_index := string_list_index(names[:], sql_ref.name); root_index >= 0 {
					add_reclassified_sql_reference(&units[unit_index], sql_ref, roots[root_index])
					continue
				}
			}
			append(&next_refs, sql_ref)
		}
		units[unit_index].sql_name_refs = next_refs
	}
}

@(private)
sql_ref_in_predicate :: proc(predicates: []Sql_Predicate_Data, ref: Sql_Name_Ref_Data) -> bool {
	for predicate in predicates {
		if predicate.query_id == ref.query_id &&
		   predicate.range.start <= ref.range.start &&
		   ref.range.end <= predicate.range.end {
			return true
		}
	}
	return false
}

@(private)
add_reclassified_sql_reference :: proc(
	unit: ^Unit_Analysis,
	sql_ref: Sql_Name_Ref_Data,
	handle: Symbol_Handle,
) {
	for ref in unit.references {
		if ref.namespace == .Value &&
		   ref.kind == .Identifier &&
		   ref.range == sql_ref.range &&
		   ref.name == sql_ref.name {
			return
		}
	}
	id := Reference_Id(u32(len(unit.references)))
	append(
		&unit.references,
		Reference_Data {
			id = id,
			name = sql_ref.name,
			namespace = .Value,
			kind = .Identifier,
			scope = sql_ref.scope,
			range = sql_ref.range,
			resolution = Resolution{kind = .Symbol, symbol = handle},
			has_resolution = true,
		},
	)
}

@(private)
string_list_contains :: proc(values: []string, name: string) -> bool {
	return string_list_index(values, name) >= 0
}

@(private)
string_list_index :: proc(values: []string, name: string) -> int {
	for value, i in values {
		if value == name {
			return i
		}
	}
	return -1
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
