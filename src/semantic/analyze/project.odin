package abap_frontend_semantic_analyze

import "src:ast"
import execution "src:execution"
import "src:parser"
import deps "src:semantic/dependencies"
import uri_key "src:uri_key"

import "core:mem"
import "core:mem/virtual"
import "core:strings"

Source_Input_Role :: enum {
	Full_Source,
	Dependency_Interface_Source,
}

Source_Input :: struct {
	uri:    string,
	source: string,
	role:   Source_Input_Role,
}

Analyze_Flag :: enum {
	Enable_Dependency_Diagnostics,
}
Analyze_Flags :: bit_set[Analyze_Flag]

Analyze_Options :: struct {
	flags: Analyze_Flags,
}

Project_Provider_Store :: struct {
	source_files: [dynamic]Source_File_Provider,
	summaries:    []Summary_Provider_Input,
}

Project_Analysis :: struct {
	providers:   Project_Provider_Store,
	diagnostics: [dynamic]Diagnostic,
	graph:       Project_Graph,
}

Project_Snapshot_State :: struct {
	inputs:                            [dynamic]Source_Input,
	dependency_summaries:              [dynamic]Summary_Provider_Input,
	source_files:                      [dynamic]Source_File_Provider,
	uri_to_source_file:                map[string]Source_File_Id,
	reverse_edges:                     map[Source_File_Id][dynamic]Source_File_Id,
	unresolved_candidates:             map[deps.Remote_Dependency_Key][dynamic]Source_File_Id,
	remote_waiters_by_name:            map[string][dynamic]Source_File_Id,
	source_file_dependencies:          [dynamic][dynamic]Source_File_Id,
	source_file_unresolved_candidates: [dynamic][dynamic]deps.Remote_Dependency_Key,
	diagnostics:                       [dynamic]Diagnostic,
	index:                             Project_Index,
	graph:                             Project_Graph,
	candidates:                        [dynamic]Project_Candidate_Input,
	candidate_to_unit:                 [dynamic]Source_File_Id,
	candidate_dirs:                    [dynamic]string,
	source_file_dirs:                  [dynamic]string,
	source_file_candidate_index:       [dynamic]int,
	interface_signatures:              [dynamic]string,
	source_file_allocators:            []mem.Allocator,
	allocator:                         mem.Allocator,
}

Project_Candidate_Input :: struct {
	input:       Source_Input,
	object_name: string,
}

Project_Work_State :: struct {
	source_files:           [dynamic]Source_File_Provider,
	inputs:                 [dynamic]Source_Input,
	dependency_summaries:   [dynamic]Summary_Provider_Input,
	source_file_allocators: []mem.Allocator,
	allocator:              mem.Allocator,
}

Project_Task_Payload :: struct {
	state:             ^Project_Work_State,
	source_file_index: int,
}

Project_Infer_State :: struct {
	project:                ^Project_Analysis,
	lookup:                 ^Project_Index,
	inferred:               []Inferred_Unit_Facts,
	source_file_allocators: []mem.Allocator,
	allocator:              mem.Allocator,
}

Project_Validate_State :: struct {
	project:                ^Project_Analysis,
	lookup:                 ^Project_Index,
	diagnostics:            [][dynamic]Diagnostic,
	source_file_allocators: []mem.Allocator,
	allocator:              mem.Allocator,
}

Project_Infer_Payload :: struct {
	state:             ^Project_Infer_State,
	source_file_index: int,
	output_index:      int,
}

Project_Validate_Payload :: struct {
	state:             ^Project_Validate_State,
	source_file_index: int,
	output_index:      int,
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
	pool: ^execution.Pool,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> Project_Analysis {
	wrapped := make([dynamic]Project_Candidate_Input, 0, len(candidates), allocator)
	for candidate in candidates {
		append(&wrapped, Project_Candidate_Input{input = candidate})
	}
	return analyze_target_with_candidate_inputs(target, wrapped[:], {}, pool, options, allocator)
}

analyze_target_with_candidate_inputs :: proc(
	target: Source_Input,
	candidates: []Project_Candidate_Input,
	dependencies: []Source_Input,
	pool: ^execution.Pool,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> Project_Analysis {
	return analyze_target_with_candidate_inputs_and_summaries(
		target,
		candidates,
		dependencies,
		{},
		pool,
		options,
		allocator,
	)
}

analyze_target_with_candidate_inputs_and_summaries :: proc(
	target: Source_Input,
	candidates: []Project_Candidate_Input,
	dependencies: []Source_Input,
	dependency_summaries: []Summary_Provider_Input,
	pool: ^execution.Pool,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> Project_Analysis {
	return analyze_target_with_candidate_inputs_allocators(
		target,
		candidates,
		dependencies,
		dependency_summaries,
		pool,
		options,
		{},
		allocator,
	)
}

analyze_target_with_candidate_inputs_allocators :: proc(
	target: Source_Input,
	candidates: []Project_Candidate_Input,
	dependencies: []Source_Input,
	dependency_summaries: []Summary_Provider_Input,
	pool: ^execution.Pool,
	options: Analyze_Options,
	source_file_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) -> Project_Analysis {
	assert(pool != nil)
	state := project_state_make(source_file_allocators, allocator)
	return project_state_analyze_target_with_candidate_inputs_and_summaries(
		&state,
		target,
		candidates,
		dependencies,
		dependency_summaries,
		pool,
		options,
		allocator,
	)
}

project_state_make :: proc(
	source_file_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) -> Project_Snapshot_State {
	return Project_Snapshot_State {
		inputs = make([dynamic]Source_Input, 0, 8, allocator),
		dependency_summaries = make([dynamic]Summary_Provider_Input, 0, 8, allocator),
		source_files = make([dynamic]Source_File_Provider, 0, 8, allocator),
		uri_to_source_file = make(map[string]Source_File_Id, 16, allocator),
		reverse_edges = make(map[Source_File_Id][dynamic]Source_File_Id, 16, allocator),
		unresolved_candidates = make(
			map[deps.Remote_Dependency_Key][dynamic]Source_File_Id,
			16,
			allocator,
		),
		remote_waiters_by_name = make(map[string][dynamic]Source_File_Id, 16, allocator),
		source_file_dependencies = make([dynamic][dynamic]Source_File_Id, 0, 8, allocator),
		source_file_unresolved_candidates = make(
			[dynamic][dynamic]deps.Remote_Dependency_Key,
			0,
			8,
			allocator,
		),
		diagnostics = make([dynamic]Diagnostic, 0, 8, allocator),
		index = project_index_make(allocator),
		graph = project_graph_make(allocator),
		candidates = make([dynamic]Project_Candidate_Input, 0, 8, allocator),
		candidate_to_unit = make([dynamic]Source_File_Id, 0, 8, allocator),
		candidate_dirs = make([dynamic]string, 0, 8, allocator),
		source_file_dirs = make([dynamic]string, 0, 8, allocator),
		source_file_candidate_index = make([dynamic]int, 0, 8, allocator),
		interface_signatures = make([dynamic]string, 0, 8, allocator),
		source_file_allocators = source_file_allocators,
		allocator = allocator,
	}
}

project_state_analyze_target_with_candidate_inputs :: proc(
	state: ^Project_Snapshot_State,
	target: Source_Input,
	candidates: []Project_Candidate_Input,
	dependencies: []Source_Input,
	pool: ^execution.Pool,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> Project_Analysis {
	return project_state_analyze_target_with_candidate_inputs_and_summaries(
		state,
		target,
		candidates,
		dependencies,
		{},
		pool,
		options,
		allocator,
	)
}

project_state_analyze_target_with_candidate_inputs_and_summaries :: proc(
	state: ^Project_Snapshot_State,
	target: Source_Input,
	candidates: []Project_Candidate_Input,
	dependencies: []Source_Input,
	dependency_summaries: []Summary_Provider_Input,
	pool: ^execution.Pool,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> Project_Analysis {
	targets := [?]Source_Input{target}
	return project_state_analyze_targets_with_candidate_inputs_and_summaries(
		state,
		targets[:],
		candidates,
		dependencies,
		dependency_summaries,
		pool,
		options,
		allocator,
	)
}

project_state_analyze_targets_with_candidate_inputs :: proc(
	state: ^Project_Snapshot_State,
	targets: []Source_Input,
	candidates: []Project_Candidate_Input,
	dependencies: []Source_Input,
	pool: ^execution.Pool,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> Project_Analysis {
	return project_state_analyze_targets_with_candidate_inputs_and_summaries(
		state,
		targets,
		candidates,
		dependencies,
		{},
		pool,
		options,
		allocator,
	)
}

project_state_analyze_targets_with_candidate_inputs_and_summaries :: proc(
	state: ^Project_Snapshot_State,
	targets: []Source_Input,
	candidates: []Project_Candidate_Input,
	dependencies: []Source_Input,
	dependency_summaries: []Summary_Provider_Input,
	pool: ^execution.Pool,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> Project_Analysis {
	return project_state_apply_dirty_inputs_with_summaries(
		state,
		targets,
		candidates,
		dependencies,
		dependency_summaries,
		{},
		{},
		pool,
		options,
		allocator,
	)
}

project_state_apply_dirty_inputs :: proc(
	state: ^Project_Snapshot_State,
	targets: []Source_Input,
	candidates: []Project_Candidate_Input,
	dependencies: []Source_Input,
	dirty: []Source_File_Id,
	include_roots: []Source_File_Id,
	pool: ^execution.Pool,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> Project_Analysis {
	return project_state_apply_dirty_inputs_with_summaries(
		state,
		targets,
		candidates,
		dependencies,
		{},
		dirty,
		include_roots,
		pool,
		options,
		allocator,
	)
}

project_state_apply_dirty_inputs_with_summaries :: proc(
	state: ^Project_Snapshot_State,
	targets: []Source_Input,
	candidates: []Project_Candidate_Input,
	dependencies: []Source_Input,
	dependency_summaries: []Summary_Provider_Input,
	dirty: []Source_File_Id,
	include_roots: []Source_File_Id,
	pool: ^execution.Pool,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> Project_Analysis {
	state.allocator = allocator
	project_state_set_candidates(state, candidates, allocator)
	next_dirty := make(
		[dynamic]Source_File_Id,
		0,
		len(targets) + len(dependencies) + len(dependency_summaries) + len(dirty),
		context.temp_allocator,
	)
	next_include_roots := make(
		[dynamic]Source_File_Id,
		0,
		len(targets) + len(dependencies) + len(dependency_summaries) + len(include_roots),
		context.temp_allocator,
	)
	for source_file_id in dirty {push_unique_unit(&next_dirty, source_file_id)}
	for source_file_id in include_roots {push_unique_unit(&next_include_roots, source_file_id)}
	for target in targets {
		target_id, target_changed := project_state_upsert_input(state, target, -1, allocator)
		if target_changed {
			push_unique_unit(&next_dirty, target_id)
			push_unique_unit(&next_include_roots, target_id)
		}
	}
	for dependency in dependencies {
		source_file_id, changed := project_state_upsert_input(state, dependency, -1, allocator)
		if changed {
			push_unique_unit(&next_dirty, source_file_id)
			push_unique_unit(&next_include_roots, source_file_id)
		}
	}
	for summary in dependency_summaries {
		_, changed := project_state_upsert_dependency_summary(state, summary, allocator)
		if changed {
			for unit in state.source_files {
				push_unique_unit(&next_dirty, unit.source_file_id)
			}
		}
	}
	project_index_update_summaries(&state.index, state.dependency_summaries[:])
	project_state_mark_active_candidate_changes(state, &next_dirty, &next_include_roots)
	project_state_collect_include_roots_for_candidates(state, &next_include_roots)
	project_state_update(state, next_dirty[:], next_include_roots[:], pool, options, allocator)
	return project_state_analysis(state)
}

project_state_analysis :: proc(state: ^Project_Snapshot_State) -> Project_Analysis {
	return Project_Analysis {
		providers = Project_Provider_Store {
			source_files = state.source_files,
			summaries = state.dependency_summaries[:],
		},
		diagnostics = state.diagnostics,
		graph = state.graph,
	}
}

project_state_set_candidates :: proc(
	state: ^Project_Snapshot_State,
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
		source_file_id := INVALID_SOURCE_FILE_ID
		key := normalized_uri_path_key(candidate.input.uri, context.temp_allocator)
		if existing, ok := state.uri_to_source_file[key]; ok {
			source_file_id = existing
			source_file_index := source_file_id_index(source_file_id)
			if state.source_file_candidate_index[source_file_index] < 0 {
				state.source_file_candidate_index[source_file_index] = i
			}
		}
		append(&state.candidate_to_unit, source_file_id)
	}
}

@(private)
project_state_mark_active_candidate_changes :: proc(
	state: ^Project_Snapshot_State,
	dirty: ^[dynamic]Source_File_Id,
	include_roots: ^[dynamic]Source_File_Id,
) {
	for candidate, i in state.candidates {
		source_file_id := state.candidate_to_unit[i]
		source_file_index := source_file_id_index(source_file_id)
		// TODO investigate in which cases it can happen
		if source_file_index > len(state.inputs) {
			continue
		}
		if state.inputs[source_file_index].source == candidate.input.source &&
		   state.inputs[source_file_index].role == candidate.input.role {
			continue
		}
		state.inputs[source_file_index] = candidate.input
		push_unique_unit(dirty, source_file_id)
		push_unique_unit(include_roots, source_file_id)
	}
}

project_state_upsert_input :: proc(
	state: ^Project_Snapshot_State,
	input: Source_Input,
	candidate_index: int,
	allocator: mem.Allocator,
) -> (
	Source_File_Id,
	bool,
) {
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	key := normalized_uri_path_key(input.uri, context.temp_allocator)
	if source_file_id, ok := state.uri_to_source_file[key]; ok {
		source_file_index := source_file_id_index(source_file_id)
		changed :=
			state.inputs[source_file_index].source != input.source ||
			state.inputs[source_file_index].role != input.role
		state.inputs[source_file_index] = input
		if candidate_index >= 0 {
			state.source_file_candidate_index[source_file_index] = candidate_index
			state.candidate_to_unit[candidate_index] = source_file_id
		}
		return source_file_id, changed
	}
	key = normalized_uri_path_key(input.uri, allocator)
	source_file_id := Source_File_Id(u32(len(state.source_files)))
	state.uri_to_source_file[key] = source_file_id
	append(&state.inputs, input)
	append(&state.source_files, Source_File_Provider{})
	append(&state.source_file_dirs, uri_parent_dir_key(input.uri, allocator))
	append(&state.source_file_candidate_index, candidate_index)
	append(&state.interface_signatures, "")
	append(&state.source_file_dependencies, make([dynamic]Source_File_Id, 0, 8, allocator))
	append(
		&state.source_file_unresolved_candidates,
		make([dynamic]deps.Remote_Dependency_Key, 0, 8, allocator),
	)
	if candidate_index >= 0 {
		state.candidate_to_unit[candidate_index] = source_file_id
	}
	return source_file_id, true
}

project_state_upsert_dependency_summary :: proc(
	state: ^Project_Snapshot_State,
	summary: Summary_Provider_Input,
	allocator: mem.Allocator,
) -> (
	Source_File_Id,
	bool,
) {
	uri := summary.uri
	if uri == "" {
		summary_copy := summary
		uri = dependency_summary_input_uri(&summary_copy, allocator)
	}
	owned := dependency_summary_input_clone(summary, allocator)
	if owned.uri == "" {
		owned.uri = strings.clone(uri, allocator)
	}
	for &existing in state.dependency_summaries {
		if existing.uri == uri {
			changed := existing.payload != summary.payload
			existing = owned
			return INVALID_SOURCE_FILE_ID, changed
		}
	}
	append(&state.dependency_summaries, owned)
	return INVALID_SOURCE_FILE_ID, true
}

@(private)
dependency_summary_input_uri :: proc(
	summary: ^Summary_Provider_Input,
	allocator: mem.Allocator,
) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "abapls-summary:/")
	if summary.object_kind != "" {
		strings.write_string(&out, summary.object_kind)
	} else {
		strings.write_string(&out, "dependency")
	}
	strings.write_byte(&out, '/')
	if summary.object_name != "" {
		strings.write_string(&out, summary.object_name)
	} else {
		strings.write_string(&out, "anonymous")
	}
	return strings.to_string(out)
}

project_state_update :: proc(
	state: ^Project_Snapshot_State,
	dirty_source_files: []Source_File_Id,
	include_roots: []Source_File_Id,
	pool: ^execution.Pool,
	options: Analyze_Options,
	allocator: mem.Allocator,
) {
	if len(dirty_source_files) == 0 && len(include_roots) == 0 {
		return
	}

	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	parsed_source_files := make(
		[dynamic]Source_File_Id,
		0,
		len(dirty_source_files),
		context.temp_allocator,
	)
	project_state_parse_source_files(state, dirty_source_files, pool, allocator)
	project_state_refresh_candidate_source_files(state, allocator)
	for source_file_id in dirty_source_files {
		push_unique_unit(&parsed_source_files, source_file_id)
	}

	next_roots := make(
		[dynamic]Source_File_Id,
		0,
		len(dirty_source_files) + len(include_roots),
		context.temp_allocator,
	)
	for source_file_id in dirty_source_files {push_unique_unit(&next_roots, source_file_id)}
	for source_file_id in include_roots {push_unique_unit(&next_roots, source_file_id)}
	for len(next_roots) > 0 {
		new_units := make([dynamic]Source_File_Id, 0, 4, context.temp_allocator)
		project_state_resolve_include_edges(state, next_roots[:], &new_units, allocator)
		if len(new_units) == 0 {
			break
		}
		project_state_parse_source_files(state, new_units[:], pool, allocator)
		for source_file_id in new_units {push_unique_unit(&parsed_source_files, source_file_id)}
		clear(&next_roots)
		for source_file_id in new_units {push_unique_unit(&next_roots, source_file_id)}
	}

	project_state_finish(state, parsed_source_files[:], include_roots, pool, options, allocator)
}

@(private)
project_state_refresh_candidate_source_files :: proc(
	state: ^Project_Snapshot_State,
	allocator: mem.Allocator,
) {
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	for candidate, i in state.candidates {
		if state.candidate_to_unit[i] != INVALID_SOURCE_FILE_ID {
			continue
		}
		key := normalized_uri_path_key(candidate.input.uri, context.temp_allocator)
		source_file_id, ok := state.uri_to_source_file[key]
		if !ok {
			continue
		}
		state.candidate_to_unit[i] = source_file_id
		source_file_index := source_file_id_index(source_file_id)
		if state.source_file_candidate_index[source_file_index] < 0 {
			state.source_file_candidate_index[source_file_index] = i
		}
	}
}

@(private)
project_state_parse_source_files :: proc(
	state: ^Project_Snapshot_State,
	source_file_ids: []Source_File_Id,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) {
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	indices := make([dynamic]int, 0, len(source_file_ids), context.temp_allocator)
	for source_file_id in source_file_ids {
		source_file_index := source_file_id_index(source_file_id)
		append(&indices, source_file_index)
	}
	if len(indices) == 0 {
		return
	}
	for source_file_index in indices {
		project_state_reset_unit_allocator(state, source_file_index)
	}
	work := Project_Work_State {
		source_files           = state.source_files,
		inputs                 = state.inputs,
		dependency_summaries   = state.dependency_summaries,
		source_file_allocators = state.source_file_allocators,
		allocator              = allocator,
	}
	run_project_tasks(pool, indices[:], &work, parse_collect_task)
	state.source_files = work.source_files
}

@(private)
project_state_reset_unit_allocator :: proc(
	state: ^Project_Snapshot_State,
	source_file_index: int,
) {
	if source_file_index < 0 ||
	   source_file_index >= len(state.source_file_allocators) ||
	   state.source_file_allocators[source_file_index].procedure != virtual.arena_allocator_proc {
		return
	}
	arena := cast(^virtual.Arena)state.source_file_allocators[source_file_index].data
	virtual.arena_free_all(arena)
	state.source_file_allocators[source_file_index] = virtual.arena_allocator(arena)
}

@(private)
project_state_resolve_include_edges :: proc(
	state: ^Project_Snapshot_State,
	roots: []Source_File_Id,
	new_units: ^[dynamic]Source_File_Id,
	allocator: mem.Allocator,
) {
	for source_file_id in roots {
		source_file_index := source_file_id_index(source_file_id)
		source_dir := state.source_file_dirs[source_file_index]
		for &edge in state.source_files[source_file_index].include_edges {
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
			if target_unit == INVALID_SOURCE_FILE_ID {
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
	state: ^Project_Snapshot_State,
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
	state: ^Project_Snapshot_State,
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
	state: ^Project_Snapshot_State,
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
	state: ^Project_Snapshot_State,
	candidate_index: int,
	name: string,
) -> bool {
	candidate := state.candidates[candidate_index]
	if strings.equal_fold(uri_file_stem(candidate.input.uri), name) ||
	   (candidate.object_name != "" && strings.equal_fold(candidate.object_name, name)) {
		return true
	}
	source_file_id := state.candidate_to_unit[candidate_index]
	if source_file_id == INVALID_SOURCE_FILE_ID {
		return false
	}
	source_file_index := source_file_id_index(source_file_id)
	for provided in state.source_files[source_file_index].provided_names {
		if strings.equal_fold(provided, name) {
			return true
		}
	}
	return false
}

@(private)
project_state_collect_include_roots_for_candidates :: proc(
	state: ^Project_Snapshot_State,
	out: ^[dynamic]Source_File_Id,
) {
	for unit in state.source_files {
		for edge in unit.include_edges {
			if edge.has_target {
				continue
			}
			if _, ok := project_state_resolve_include_candidate(
				state,
				edge.name,
				state.source_file_dirs[source_file_id_index(unit.source_file_id)],
			); ok {
				push_unique_unit(out, unit.source_file_id)
			}
		}
	}
}

@(private)
project_state_finish :: proc(
	state: ^Project_Snapshot_State,
	parsed_source_files: []Source_File_Id,
	include_roots: []Source_File_Id,
	pool: ^execution.Pool,
	options: Analyze_Options,
	allocator: mem.Allocator,
) {
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	affected := make(
		[dynamic]Source_File_Id,
		0,
		len(parsed_source_files) + len(include_roots),
		context.temp_allocator,
	)
	interface_changed := make(
		[dynamic]Source_File_Id,
		0,
		len(parsed_source_files),
		context.temp_allocator,
	)
	for source_file_id in parsed_source_files {
		push_unique_unit(&affected, source_file_id)
		source_file_index := source_file_id_index(source_file_id)
		signature := unit_interface_signature(&state.source_files[source_file_index], allocator)
		if state.interface_signatures[source_file_index] != signature {
			state.interface_signatures[source_file_index] = signature
			push_unique_unit(&interface_changed, source_file_id)
		}
	}
	for source_file_id in include_roots {
		push_unique_unit(&affected, source_file_id)
	}
	remote_waiters := make(
		[dynamic]Source_File_Id,
		0,
		len(interface_changed),
		context.temp_allocator,
	)
	if len(state.unresolved_candidates) > 0 {
		project_index_update_units(&state.index, state.source_files[:], interface_changed[:])
		project_state_collect_remote_waiters(
			state,
			interface_changed[:],
			&affected,
			&remote_waiters,
		)
	}
	reverse_roots := make(
		[dynamic]Source_File_Id,
		0,
		len(interface_changed) + len(remote_waiters),
		context.temp_allocator,
	)
	for source_file_id in interface_changed {push_unique_unit(&reverse_roots, source_file_id)}
	for source_file_id in remote_waiters {push_unique_unit(&reverse_roots, source_file_id)}
	project_state_expand_reverse_dependents(state, reverse_roots[:], &affected)

	if len(affected) == 0 {
		return
	}
	project_state_prepare_affected_units(state, affected[:])
	project_state_build_scope_indexes(state, affected[:], pool, allocator)
	for source_file_id in affected {
		source_file_index := source_file_id_index(source_file_id)
		resolve_unit_with_index(
			&state.source_files[source_file_index],
			&state.source_files[source_file_index].scope_index,
		)
	}
	refresh_source_file_fact_models(
		state.source_files[:],
		affected[:],
		state.source_file_allocators,
		allocator,
	)
	add_unresolved_include_diagnostics_for_units(state.source_files[:], affected[:], allocator)
	diagnose_include_cycles_for_units(state.source_files[:], affected[:], allocator)
	project_index_update_units(&state.index, state.source_files[:], affected[:])
	project_index_update_include_graph(&state.index, state.source_files[:], affected[:])

	project := project_state_analysis(state)
	resolve_project_cross_file_for_source_files(
		project.providers.source_files[:],
		affected[:],
		&state.index,
	)
	if project_state_linking_needed(project.providers.source_files[:], affected[:]) {
		reset_cross_class_member_implementation_links(project.providers.source_files[:])
		link_class_member_implementations_with_index(
			project.providers.source_files[:],
			state.index.predecessors,
		)
		project_state_add_class_definition_units(project.providers.source_files[:], &affected)
	}
	resolve_project_open_sql_predicate_names_for_source_files(
		project.providers.source_files[:],
		affected[:],
		&state.index,
	)
	lookup := &state.index
	check_project_bodies_for_units(
		&project,
		lookup,
		affected[:],
		pool,
		state.source_file_allocators,
		allocator,
	)
	collect_project_diagnostics(&project)
	if !(.Enable_Dependency_Diagnostics in options.flags) {
		filter_dependency_diagnostics(&project)
	}
	state.source_files = project.providers.source_files
	state.diagnostics = project.diagnostics
	project_state_update_dependency_graph_for_source_files(state, &project, lookup, affected[:])
	record_project_unresolved_candidates_for_units(state, &project, affected[:])
}

@(private)
project_state_collect_remote_waiters :: proc(
	state: ^Project_Snapshot_State,
	providers: []Source_File_Id,
	affected: ^[dynamic]Source_File_Id,
	waiters: ^[dynamic]Source_File_Id,
) {
	for provider in providers {
		source_file_index := source_file_id_index(provider)
		if source_file_index < 0 || source_file_index >= len(state.index.source_file_entries) {
			continue
		}
		for export in state.index.source_file_entries[source_file_index].exports {
			units, ok := state.remote_waiters_by_name[export]
			if !ok {continue}
			for source_file_id in units {
				push_unique_unit(affected, source_file_id)
				push_unique_unit(waiters, source_file_id)
			}
		}
	}
}

@(private)
project_state_expand_reverse_dependents :: proc(
	state: ^Project_Snapshot_State,
	roots: []Source_File_Id,
	affected: ^[dynamic]Source_File_Id,
) {
	queue := make([dynamic]Source_File_Id, 0, len(roots), context.temp_allocator)
	for root in roots {
		push_unique_unit(&queue, root)
	}
	for cursor := 0; cursor < len(queue); cursor += 1 {
		source_file_id := queue[cursor]
		if dependents, ok := state.reverse_edges[source_file_id]; ok {
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
project_state_prepare_affected_units :: proc(
	state: ^Project_Snapshot_State,
	affected: []Source_File_Id,
) {
	for source_file_id in affected {
		source_file_index := source_file_id_index(source_file_id)
		unit := &state.source_files[source_file_index]
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
clear_unit_reference_resolutions :: proc(unit: ^Source_File_Provider) {
	for &ref in unit.references {
		ref.resolution = {}
		ref.has_resolution = false
	}
}

@(private)
project_state_build_scope_indexes :: proc(
	state: ^Project_Snapshot_State,
	affected: []Source_File_Id,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) {
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	indices := source_file_ids_to_indices(
		affected,
		len(state.source_files),
		context.temp_allocator,
	)
	if len(indices) == 0 {
		return
	}
	work := Project_Work_State {
		source_files           = state.source_files,
		inputs                 = state.inputs,
		dependency_summaries   = state.dependency_summaries,
		source_file_allocators = state.source_file_allocators,
		allocator              = allocator,
	}
	run_project_tasks(pool, indices[:], &work, build_scope_index_task)
	state.source_files = work.source_files
}

@(private)
source_file_ids_to_indices :: proc(
	source_file_ids: []Source_File_Id,
	source_file_count: int,
	allocator: mem.Allocator,
) -> [dynamic]int {
	indices := make([dynamic]int, 0, len(source_file_ids), allocator)
	for source_file_id in source_file_ids {
		source_file_index := source_file_id_index(source_file_id)
		append(&indices, source_file_index)
	}
	return indices
}

@(private)
temp_arena_begin :: proc() -> Temp_Arena_Marker {
	if context.temp_allocator.procedure != virtual.arena_allocator_proc {
		return {}
	}
	assert(context.temp_allocator.data != nil)
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
	units: []Source_File_Provider,
	source_file_ids: []Source_File_Id,
	allocator: mem.Allocator,
) {
	for source_file_id in source_file_ids {
		source_file_index := source_file_id_index(source_file_id)
		for edge in units[source_file_index].include_edges {
			if !edge.has_target && !edge.if_found {
				append(
					&units[source_file_index].diagnostics,
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
	units: []Source_File_Provider,
	source_file_ids: []Source_File_Id,
	allocator: mem.Allocator,
) {
	stack := make([dynamic]Source_File_Id, 0, len(units), allocator)
	done := make([]bool, len(units), allocator)
	for source_file_id in source_file_ids {
		source_file_index := source_file_id_index(source_file_id)
		if !done[source_file_index] {
			diagnose_include_cycles_from(units, source_file_id, &stack, done, allocator)
		}
	}
}

@(private)
resolve_project_cross_file_for_source_files :: proc(
	units: []Source_File_Provider,
	affected: []Source_File_Id,
	index: ^Project_Index,
) {
	if len(units) == 0 || len(affected) == 0 {
		return
	}

	resolve_effective_method_signatures_for_units(
		units,
		affected,
		&index.root_lookup,
		index.class_scope_entries,
		index.visible,
		index.predecessors,
	)
	derive_event_handler_signature_parameters_for_units(
		units,
		affected,
		&index.root_lookup,
		index.class_scope_entries,
		index.visible,
	)
	resolve_project_cross_unit_references_for_units(units, affected, index)
}

@(private)
resolve_effective_method_signatures_for_units :: proc(
	units: []Source_File_Provider,
	affected: []Source_File_Id,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [][dynamic]Source_File_Id,
	predecessors: [][dynamic]Source_File_Id,
) {
	for source_file_id in affected {
		source_file_index := source_file_id_index(source_file_id)
		if source_file_index < 0 ||
		   source_file_index >= len(units) ||
		   source_file_index >= len(visible) ||
		   source_file_index >= len(predecessors) {
			continue
		}
		unit := &units[source_file_index]
		for &method_symbol in unit.symbols {
			if method_symbol.kind != .Method {
				continue
			}
			method_info := entity_decl_info(unit, method_symbol.id)
			if method_info == nil || method_info.body_scope == INVALID_SCOPE_ID {
				continue
			}
			method_info.effective_signature = Symbol_Link {
				unit   = INVALID_SOURCE_FILE_ID,
				symbol = INVALID_SYMBOL_ID,
			}
			member, member_source_file_index := method_signature_member_for_scope(
				units,
				source_file_index,
				method_symbol.scope,
				method_symbol.name,
				roots,
				class_entries,
				visible[source_file_index],
				predecessors[source_file_index],
			)
			if member.symbol == INVALID_SYMBOL_ID ||
			   member_source_file_index < 0 ||
			   member_source_file_index >= len(units) {
				continue
			}
			method_info.effective_signature = member
		}
	}
}

@(private)
resolve_project_cross_unit_references_for_units :: proc(
	units: []Source_File_Provider,
	affected: []Source_File_Id,
	index: ^Project_Index,
) {
	for source_file_id in affected {
		source_file_index := source_file_id_index(source_file_id)
		for ref_index in 0 ..< len(units[source_file_index].references) {
			ref := &units[source_file_index].references[ref_index]
			if ref.has_resolution {
				continue
			}
			if resolution, ok := resolve_project_reference(
				units,
				source_file_index,
				ref^,
				&index.root_lookup,
				index.class_scope_entries,
				index.visible[source_file_index],
				index.predecessors[source_file_index],
			); ok {
				set_project_reference_resolution(units, source_file_index, ref, resolution)
			}
		}
	}
}

@(private)
derive_event_handler_signature_parameters_for_units :: proc(
	units: []Source_File_Provider,
	affected: []Source_File_Id,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [][dynamic]Source_File_Id,
) {
	for source_file_id in affected {
		source_file_index := source_file_id_index(source_file_id)
		unit := &units[source_file_index]
		for &method_symbol in unit.symbols {
			if method_symbol.kind != .Method {
				continue
			}
			method_info := entity_decl_info(unit, method_symbol.id)
			if method_info == nil || method_info.body_scope == INVALID_SCOPE_ID {
				continue
			}
			member := method_info.effective_signature
			member_source_file_index := source_file_id_index(member.unit)
			if member.symbol == INVALID_SYMBOL_ID ||
			   member_source_file_index < 0 ||
			   member_source_file_index >= len(units) {
				continue
			}
			member_info := entity_decl_info(&units[member_source_file_index], member.symbol)
			if member_info == nil || !(.For_Event in member_info.flags) {
				continue
			}
			_ = derive_event_handler_signature_parameter_types(
				units,
				member_source_file_index,
				member,
				roots,
				class_entries,
				visible,
			)
		}
	}
}

@(private)
project_state_linking_needed :: proc(
	units: []Source_File_Provider,
	affected: []Source_File_Id,
) -> bool {
	for source_file_id in affected {
		source_file_index := source_file_id_index(source_file_id)
		unit := &units[source_file_index]
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
reset_cross_class_member_implementation_links :: proc(units: []Source_File_Provider) {
	for &unit in units {
		for &info in unit.decl_infos {
			if !(.Has_Implementation in info.flags) ||
			   info.implementation_unit == unit.source_file_id {
				continue
			}
			info.flags -= {.Has_Implementation}
			info.implementation_unit = INVALID_SOURCE_FILE_ID
			info.implementation_range = {}
		}
	}
}

@(private)
project_state_add_class_definition_units :: proc(
	units: []Source_File_Provider,
	affected: ^[dynamic]Source_File_Id,
) {
	for unit in units {
		if len(unit.class_definitions) > 0 {
			push_unique_unit(affected, unit.source_file_id)
		}
	}
}

@(private)
resolve_project_open_sql_predicate_names_for_source_files :: proc(
	units: []Source_File_Provider,
	affected: []Source_File_Id,
	index: ^Project_Index,
) {
	for source_file_id in affected {
		source_file_index := source_file_id_index(source_file_id)
		unit := &units[source_file_index]
		if len(unit.sql_predicate_names) == 0 {
			continue
		}

		remove_materialized_sql_predicate_columns(unit)
		for predicate_name in unit.sql_predicate_names {
			if resolution, ok := resolve_open_sql_predicate_name(
				units,
				source_file_index,
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
remove_materialized_sql_predicate_columns :: proc(unit: ^Source_File_Provider) {
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
	units: []Source_File_Provider,
	source_file_index: int,
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
		&units[source_file_index],
		&units[source_file_index].scope_index,
		ref,
	); ok && sql_predicate_resolution_is_host_value(units, resolution) {
		return resolution, true
	}
	if resolution, ok := resolve_project_reference(
		units,
		source_file_index,
		ref,
		&index.root_lookup,
		index.class_scope_entries,
		index.visible[source_file_index],
		index.predecessors[source_file_index],
	); ok && sql_predicate_resolution_is_host_value(units, resolution) {
		return resolution, true
	}
	return {}, false
}

@(private)
sql_predicate_resolution_is_host_value :: proc(
	units: []Source_File_Provider,
	resolution: Resolution,
) -> bool {
	#partial switch resolution.kind {
	case .Symbol:
		source_file_index := source_file_id_index(resolution.symbol.unit)
		s := symbol(&units[source_file_index], resolution.symbol.symbol)
		return s != nil && symbol_kind_occupies(s.kind, .Value)
	case .Internal_Table_Line:
		return true
	case:
		return false
	}
}

@(private)
add_resolved_sql_predicate_reference :: proc(
	unit: ^Source_File_Provider,
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
add_sql_predicate_column :: proc(
	unit: ^Source_File_Provider,
	predicate_name: Sql_Predicate_Name_Data,
) {
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
project_state_update_dependency_graph_for_source_files :: proc(
	state: ^Project_Snapshot_State,
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_ids: []Source_File_Id,
) {
	assert(len(state.source_file_dependencies) >= len(project.providers.source_files))
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	for source_file_id in source_file_ids {
		source_file_index := source_file_id_index(source_file_id)
		source_file_dependencies := &state.source_file_dependencies[source_file_index]
		for dependency in source_file_dependencies^ {
			project_state_remove_reverse_dependency(state, dependency, source_file_id)
		}
		clear(source_file_dependencies)

		unit := &project.providers.source_files[source_file_index]
		project_graph_update_unit_from_project(&state.graph, project, lookup, source_file_index)
		dependency_seen := make(map[Source_File_Id]bool, 8, context.temp_allocator)
		for edge in project_graph_provider_dependencies(
			&state.graph,
			source_file_provider_handle(unit),
		) {
			if edge.to.kind != .File {
				continue
			}
			project_state_add_source_file_dependency(
				state,
				source_file_dependencies,
				unit.source_file_id,
				Source_File_Id(u32(edge.to.id)),
				&dependency_seen,
			)
		}
	}
}

@(private)
project_state_add_source_file_dependency :: proc(
	state: ^Project_Snapshot_State,
	source_file_dependencies: ^[dynamic]Source_File_Id,
	from, to: Source_File_Id,
	dependency_seen: ^map[Source_File_Id]bool,
) {
	if from == INVALID_SOURCE_FILE_ID || to == INVALID_SOURCE_FILE_ID || from == to {
		return
	}
	if to in dependency_seen^ {
		return
	}
	dependency_seen^[to] = true
	append(source_file_dependencies, to)
	if dependents, ok := state.reverse_edges[to]; ok {
		push_unique_unit(&dependents, from)
		state.reverse_edges[to] = dependents
	} else {
		next_dependents := make([dynamic]Source_File_Id, 0, 2, state.allocator)
		append(&next_dependents, from)
		state.reverse_edges[to] = next_dependents
	}
}

@(private)
project_state_remove_reverse_dependency :: proc(
	state: ^Project_Snapshot_State,
	to, from: Source_File_Id,
) {
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
unit_interface_signature :: proc(unit: ^Source_File_Provider, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	write_signature_int(&out, int(unit.role))
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
	unit: ^Source_File_Provider,
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
	unit: ^Source_File_Provider,
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
	source_file_allocators: []mem.Allocator,
	source_file_index: int,
	fallback: mem.Allocator,
) -> mem.Allocator {
	if 0 <= source_file_index &&
	   source_file_index < len(source_file_allocators) &&
	   source_file_allocators[source_file_index].procedure != nil {
		return source_file_allocators[source_file_index]
	}
	return fallback
}

refresh_source_file_fact_models :: proc(
	units: []Source_File_Provider,
	source_file_ids: []Source_File_Id,
	source_file_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	for source_file_id in source_file_ids {
		source_file_index := source_file_id_index(source_file_id)
		if source_file_index < 0 || source_file_index >= len(units) {
			continue
		}
		source_file_refresh_fact_model(
			&units[source_file_index],
			unit_allocator(source_file_allocators, source_file_index, allocator),
		)
	}
}

project_analysis_from_source_files :: proc(
	units: [dynamic]Source_File_Provider,
	allocator: mem.Allocator,
) -> Project_Analysis {
	return Project_Analysis {
		providers = Project_Provider_Store {
			source_files = units,
			summaries = make([]Summary_Provider_Input, 0, allocator),
		},
		diagnostics = make([dynamic]Diagnostic, 0, 8, allocator),
		graph = project_graph_make(allocator),
	}
}

finish_project_analysis :: proc(
	project: ^Project_Analysis,
	pool: ^execution.Pool,
	source_file_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	index := project_index_from_project(project, allocator)
	source_file_ids := make(
		[dynamic]Source_File_Id,
		0,
		len(project.providers.source_files),
		context.temp_allocator,
	)
	for unit in project.providers.source_files {
		append(&source_file_ids, unit.source_file_id)
	}
	refresh_source_file_fact_models(
		project.providers.source_files[:],
		source_file_ids[:],
		source_file_allocators,
		allocator,
	)
	resolve_project_cross_file_for_source_files(
		project.providers.source_files[:],
		source_file_ids[:],
		&index,
	)
	link_class_member_implementations_with_index(
		project.providers.source_files[:],
		index.predecessors,
	)
	resolve_project_open_sql_predicate_names_for_source_files(
		project.providers.source_files[:],
		source_file_ids[:],
		&index,
	)
	lookup := &index
	check_project_bodies(project, lookup, pool, source_file_allocators, allocator)
	collect_project_diagnostics(project)
	project_graph_rebuild_from_project(&project.graph, project, lookup, allocator)
}

filter_dependency_diagnostics :: proc(project: ^Project_Analysis) {
	for &unit in project.providers.source_files {
		if unit.role != .Dependency_Interface_Source {
			continue
		}
		clear(&unit.diagnostics)
	}
	collect_project_diagnostics(project)
}

project_source_file_by_uri :: proc(
	project: ^Project_Analysis,
	uri: string,
) -> ^Source_File_Provider {
	for &unit in project.providers.source_files {
		if unit.uri == uri {
			return &unit
		}
	}
	return nil
}

@(private)
parse_collect_input :: proc(
	source_file_id: Source_File_Id,
	input: Source_Input,
	allocator: mem.Allocator,
) -> Source_File_Provider {
	parsed := parser.parse(input.source, input.uri, allocator)
	return collect_source_file(
		source_file_id,
		input.uri,
		input.source,
		parsed,
		allocator,
		source_file_role_from_source_input(input.role),
	)
}

@(private)
source_file_role_from_source_input :: proc "contextless" (
	role: Source_Input_Role,
) -> Source_File_Role {
	switch role {
	case .Dependency_Interface_Source:
		return .Dependency_Interface_Source
	case .Full_Source:
		return .Full_Source
	}
	return .Full_Source
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
run_all_source_file_tasks :: proc(
	pool: ^execution.Pool,
	state: ^Project_Work_State,
	work: proc(_: Project_Task_Payload) -> execution.No_Result,
) {
	indices := make([dynamic]int, 0, len(state.source_files), context.temp_allocator)
	for _, i in state.source_files {
		append(&indices, i)
	}
	run_project_tasks(pool, indices[:], state, work)
}

@(private)
run_project_tasks :: proc(
	pool: ^execution.Pool,
	source_file_indices: []int,
	state: ^Project_Work_State,
	work: proc(_: Project_Task_Payload) -> execution.No_Result,
) {
	if len(source_file_indices) == 1 {
		payload := Project_Task_Payload {
			state             = state,
			source_file_index = source_file_indices[0],
		}
		work(payload)
	} else {
		graph: execution.Graph
		execution.graph_init(&graph, pool, context.temp_allocator)
		for source_file_index in source_file_indices {
			payload := Project_Task_Payload {
				state             = state,
				source_file_index = source_file_index,
			}
			execution.submit_value(&graph, execution.worker_executor(pool), payload, work)
		}
		execution.graph_start(&graph)
		execution.graph_wait(&graph)
		execution.graph_destroy(&graph)
	}
}

@(private)
parse_collect_task :: proc(payload: Project_Task_Payload) -> execution.No_Result {
	input := payload.state.inputs[payload.source_file_index]
	allocator := unit_allocator(
		payload.state.source_file_allocators,
		payload.source_file_index,
		payload.state.allocator,
	)
	payload.state.source_files[payload.source_file_index] = parse_collect_input(
		Source_File_Id(u32(payload.source_file_index)),
		input,
		allocator,
	)
	return execution.No_Result{}
}

@(private)
build_scope_index_task :: proc(payload: Project_Task_Payload) -> execution.No_Result {
	unit := &payload.state.source_files[payload.source_file_index]
	scope_index_destroy(&unit.scope_index)
	unit.scope_index = build_scope_index(
		unit,
		unit_allocator(
			payload.state.source_file_allocators,
			payload.source_file_index,
			payload.state.allocator,
		),
	)
	resolve_local_effective_method_signatures(unit)
	expand_local_structure_includes(
		unit,
		unit_allocator(
			payload.state.source_file_allocators,
			payload.source_file_index,
			payload.state.allocator,
		),
	)
	refresh_unit_type_ids(unit)
	return execution.No_Result{}
}

@(private)
add_unresolved_include_diagnostics :: proc(
	units: []Source_File_Provider,
	allocator: mem.Allocator,
) {
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
diagnose_include_cycles :: proc(units: []Source_File_Provider, allocator: mem.Allocator) {
	stack := make([dynamic]Source_File_Id, 0, len(units), allocator)
	done := make([]bool, len(units), allocator)
	for unit, i in units {
		if !done[i] {
			diagnose_include_cycles_from(units, unit.source_file_id, &stack, done, allocator)
		}
	}
}

@(private)
diagnose_include_cycles_from :: proc(
	units: []Source_File_Provider,
	source_file_id: Source_File_Id,
	stack: ^[dynamic]Source_File_Id,
	done: []bool,
	allocator: mem.Allocator,
) {
	source_file_index := source_file_id_index(source_file_id)
	if done[source_file_index] {
		return
	}
	if unit_in_stack(stack^[:], source_file_id) {
		return
	}
	append(stack, source_file_id)
	for edge in units[source_file_index].include_edges {
		if !edge.has_target || edge.target == INVALID_SOURCE_FILE_ID {
			continue
		}
		if unit_in_stack(stack^[:], edge.target) {
			append(
				&units[source_file_index].diagnostics,
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
	done[source_file_index] = true
}

@(private)
unit_in_stack :: proc(stack: []Source_File_Id, source_file_id: Source_File_Id) -> bool {
	for current in stack {
		if current == source_file_id {
			return true
		}
	}
	return false
}

@(private)
collect_project_diagnostics :: proc(project: ^Project_Analysis) {
	clear(&project.diagnostics)
	hint := 0
	for unit in project.providers.source_files {
		hint += len(unit.diagnostics)
	}
	if hint < 8 {
		hint = 8
	}
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	seen := make(map[Diagnostic_Key]bool, hint, context.temp_allocator)
	for unit in project.providers.source_files {
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
	source_file_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	infer_project_semantic_facts(project, lookup, pool, source_file_allocators, allocator)
	validate_project_units(project, lookup, pool, source_file_allocators, allocator)
}

@(private)
check_project_bodies_for_units :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_ids: []Source_File_Id,
	pool: ^execution.Pool,
	source_file_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	infer_project_semantic_facts_for_units(
		project,
		lookup,
		source_file_ids,
		pool,
		source_file_allocators,
		allocator,
	)
	validate_project_units_for_units(
		project,
		lookup,
		source_file_ids,
		pool,
		source_file_allocators,
		allocator,
	)
}

@(private)
infer_project_semantic_facts :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	pool: ^execution.Pool,
	source_file_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	graph: execution.Graph
	execution.graph_init(&graph, pool, context.temp_allocator)
	defer execution.graph_destroy(&graph)

	for {
		temp_arena := temp_arena_begin()
		inferred := make(
			[]Inferred_Unit_Facts,
			len(project.providers.source_files),
			context.temp_allocator,
		)
		state := Project_Infer_State {
			project                = project,
			lookup                 = lookup,
			inferred               = inferred,
			source_file_allocators = source_file_allocators,
			allocator              = allocator,
		}
		run_infer_tasks(&graph, &state)
		changed := apply_inferred_project_facts(
			project,
			lookup,
			inferred,
			source_file_allocators,
			allocator,
		)
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
	source_file_ids: []Source_File_Id,
	pool: ^execution.Pool,
	source_file_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	indices := source_file_ids_to_indices(
		source_file_ids,
		len(project.providers.source_files),
		context.temp_allocator,
	)
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
			project                = project,
			lookup                 = lookup,
			inferred               = inferred,
			source_file_allocators = source_file_allocators,
			allocator              = allocator,
		}
		run_infer_tasks_for_indices(&graph, &state, indices[:])
		changed := apply_inferred_project_facts_for_indices(
			project,
			lookup,
			inferred,
			indices[:],
			source_file_allocators,
			allocator,
		)
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
	source_file_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	diagnostics := make(
		[][dynamic]Diagnostic,
		len(project.providers.source_files),
		context.temp_allocator,
	)
	state := Project_Validate_State {
		project                = project,
		lookup                 = lookup,
		diagnostics            = diagnostics,
		source_file_allocators = source_file_allocators,
		allocator              = allocator,
	}
	run_validate_tasks(pool, &state)
	for i in 0 ..< len(project.providers.source_files) {
		delete(project.providers.source_files[i].diagnostics)
		project.providers.source_files[i].diagnostics = diagnostics[i]
	}
}

@(private)
validate_project_units_for_units :: proc(
	project: ^Project_Analysis,
	lookup: ^Project_Index,
	source_file_ids: []Source_File_Id,
	pool: ^execution.Pool,
	source_file_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	indices := source_file_ids_to_indices(
		source_file_ids,
		len(project.providers.source_files),
		context.temp_allocator,
	)
	if len(indices) == 0 {
		return
	}
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	diagnostics := make([][dynamic]Diagnostic, len(indices), context.temp_allocator)
	state := Project_Validate_State {
		project                = project,
		lookup                 = lookup,
		diagnostics            = diagnostics,
		source_file_allocators = source_file_allocators,
		allocator              = allocator,
	}
	run_validate_tasks_for_indices(pool, &state, indices[:])
	for source_file_index, i in indices {
		delete(project.providers.source_files[source_file_index].diagnostics)
		project.providers.source_files[source_file_index].diagnostics = diagnostics[i]
	}
}

@(private)
run_infer_tasks :: proc(graph: ^execution.Graph, state: ^Project_Infer_State) {
	if len(state.project.providers.source_files) == 1 {
		payload := Project_Infer_Payload {
			state             = state,
			source_file_index = 0,
			output_index      = 0,
		}
		infer_task(payload)
	} else {
		exec := execution.worker_executor(graph.pool)
		for _, source_file_index in state.project.providers.source_files {
			payload := Project_Infer_Payload {
				state             = state,
				source_file_index = source_file_index,
				output_index      = source_file_index,
			}
			execution.submit_value(graph, exec, payload, infer_task)
		}
		execution.graph_start(graph)
		execution.graph_wait(graph)
		execution.graph_reset(graph)
	}
}

@(private)
run_infer_tasks_for_indices :: proc(
	graph: ^execution.Graph,
	state: ^Project_Infer_State,
	indices: []int,
) {
	if len(indices) == 1 {
		payload := Project_Infer_Payload {
			state             = state,
			source_file_index = indices[0],
			output_index      = 0,
		}
		infer_task(payload)
	} else {
		exec := execution.worker_executor(graph.pool)
		for source_file_index, i in indices {
			payload := Project_Infer_Payload {
				state             = state,
				source_file_index = source_file_index,
				output_index      = i,
			}
			execution.submit_value(graph, exec, payload, infer_task)
		}
		execution.graph_start(graph)
		execution.graph_wait(graph)
		execution.graph_reset(graph)
	}
}

@(private)
run_validate_tasks :: proc(pool: ^execution.Pool, state: ^Project_Validate_State) {
	temp_arena := temp_arena_begin()
	defer temp_arena_end(temp_arena)

	if len(state.project.providers.source_files) == 1 {
		payload := Project_Validate_Payload {
			state             = state,
			source_file_index = 0,
			output_index      = 0,
		}
		validate_task(payload)
	} else {
		graph: execution.Graph
		execution.graph_init(&graph, pool, context.temp_allocator)
		for _, source_file_index in state.project.providers.source_files {
			payload := Project_Validate_Payload {
				state             = state,
				source_file_index = source_file_index,
				output_index      = source_file_index,
			}
			execution.submit_value(&graph, execution.worker_executor(pool), payload, validate_task)
		}
		execution.graph_start(&graph)
		execution.graph_wait(&graph)
		execution.graph_destroy(&graph)
	}
}

@(private)
run_validate_tasks_for_indices :: proc(
	pool: ^execution.Pool,
	state: ^Project_Validate_State,
	indices: []int,
) {
	if len(indices) == 1 {
		payload := Project_Validate_Payload {
			state             = state,
			source_file_index = indices[0],
			output_index      = 0,
		}
		validate_task(payload)
	} else {
		graph: execution.Graph
		execution.graph_init(&graph, pool, context.temp_allocator)
		for source_file_index, i in indices {
			payload := Project_Validate_Payload {
				state             = state,
				source_file_index = source_file_index,
				output_index      = i,
			}
			execution.submit_value(&graph, execution.worker_executor(pool), payload, validate_task)
		}
		execution.graph_start(&graph)
		execution.graph_wait(&graph)
		execution.graph_destroy(&graph)
	}
}

@(private)
infer_task :: proc(payload: Project_Infer_Payload) -> execution.No_Result {
	payload.state.inferred[payload.output_index] = infer_unit_semantic_facts(
		payload.state.project,
		payload.state.lookup,
		payload.source_file_index,
		unit_allocator(
			payload.state.source_file_allocators,
			payload.source_file_index,
			payload.state.allocator,
		),
	)
	return execution.No_Result{}
}

@(private)
validate_task :: proc(payload: Project_Validate_Payload) -> execution.No_Result {
	payload.state.diagnostics[payload.output_index] = validate_unit_diagnostics(
		payload.state.project,
		payload.state.lookup,
		payload.source_file_index,
		unit_allocator(
			payload.state.source_file_allocators,
			payload.source_file_index,
			payload.state.allocator,
		),
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
	units: []Source_File_Provider,
	predecessors: [][dynamic]Source_File_Id,
) {
	for impl_source_file_index in 0 ..< len(units) {
		for method_symbol in units[impl_source_file_index].symbols {
			if method_symbol.kind != .Method {
				continue
			}
			class_symbol, ok := enclosing_class_owner_unit(
				&units[impl_source_file_index],
				method_symbol.scope,
			)
			if !ok {
				continue
			}
			class_name := symbol(&units[impl_source_file_index], class_symbol).name
			for i := len(predecessors[impl_source_file_index]) - 1; i >= 0; i -= 1 {
				def_unit := predecessors[impl_source_file_index][i]
				class_handle, class_ok := root_symbol_in_source_file(
					units,
					def_unit,
					.Type,
					class_name,
				)
				if !class_ok ||
				   !unit_has_class_definition(
						   &units[source_file_id_index(def_unit)],
						   class_handle.symbol,
					   ) {
					continue
				}
				member := unit_class_member_symbol_canonical(
					&units[source_file_id_index(def_unit)],
					class_handle.symbol,
					method_symbol.name,
				)
				def_source_file_index := source_file_id_index(def_unit)
				if member != nil && member.kind == .Method {
					if info := entity_decl_info(&units[def_source_file_index], member.id);
					   info != nil && !(.Has_Implementation in info.flags) {
						info.implementation_unit = units[impl_source_file_index].source_file_id
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
