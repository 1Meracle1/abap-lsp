package abap_frontend_semantic_session

import "src:adt"
import analyze "src:semantic/analyze"
import remote_deps "src:semantic/remote_dependencies"
import uri_key "src:uri_key"

import "core:hash"
import "core:mem"
import "core:mem/virtual"
import "core:strings"

Input_Id :: distinct int

Input_Role :: enum {
	Target,
	Candidate,
	Dependency,
}

Input_Flag :: enum {
	Active,
	Immutable,
}
Input_Flags :: bit_set[Input_Flag]

Input_Record :: struct {
	id:           Input_Id,
	role:         Input_Role,
	input:        analyze.Source_Input,
	object_name:  string,
	content_hash: u64,
	unit:         analyze.Unit_Id,
	flags:        Input_Flags,
}

Input_Change_Kind :: enum {
	Upsert,
	Delete,
}

Input_Change :: struct {
	kind:        Input_Change_Kind,
	role:        Input_Role,
	input:       analyze.Source_Input,
	object_name: string,
	immutable:   bool,
}

Remote_Dependency_State :: struct {
	seen_artifacts:           map[i64]bool,
	seen_local_candidates:    map[analyze.Remote_Dependency_Key]bool,
	seen_adt_candidates:      map[analyze.Remote_Dependency_Key]bool,
	seen_typepool_candidates: map[analyze.Remote_Dependency_Key]bool,
}

Session_Memory :: struct {
	session_arena:   virtual.Arena,
	update_arena:    virtual.Arena,
	dep_arena:       virtual.Arena,
	unit_arenas:     [dynamic]virtual.Arena,
	unit_allocators: [dynamic]mem.Allocator,

	allocator:      mem.Allocator,
	temp_allocator: mem.Allocator,
	dep_allocator:  mem.Allocator,
}

Analysis_Session :: struct {
	project_state: analyze.Project_State,
	memory:        Session_Memory,

	inputs:       [dynamic]Input_Record,
	uri_to_input: map[string]Input_Id,

	targets:      [dynamic]analyze.Source_Input,
	candidates:   [dynamic]analyze.Project_Candidate_Input,
	dependencies: [dynamic]analyze.Source_Input,

	dependency_state: Remote_Dependency_State,
	config:           remote_deps.Dependency_Config,
	options:          analyze.Analyze_Options,
}

Update_Result :: struct {
	project:            analyze.Project_Analysis,
	dirty_count:        int,
	affected_count:     int,
	added_dependencies: int,
}

analysis_session_make :: proc(
	config: remote_deps.Dependency_Config,
	options: analyze.Analyze_Options,
	backing_allocator: mem.Allocator,
) -> Analysis_Session {
	memory := session_memory_make(backing_allocator)
	return Analysis_Session {
		memory = memory,
		config = config,
		options = options,
	}
}

analysis_session_destroy :: proc(session: ^Analysis_Session) {
	session_memory_destroy(&session.memory)
	session^ = {}
}

analysis_session_apply_changes :: proc(
	session: ^Analysis_Session,
	changes: []Input_Change,
) -> Update_Result {
	analysis_session_ensure_initialized(session)
	session_memory_begin_update(&session.memory)
	previous_temp_allocator := context.temp_allocator
	context.temp_allocator = session.memory.temp_allocator
	defer context.temp_allocator = previous_temp_allocator

	dirty := make([dynamic]analyze.Unit_Id, 0, len(changes), context.temp_allocator)
	include_roots := make([dynamic]analyze.Unit_Id, 0, len(changes), context.temp_allocator)
	analysis_session_reconcile_inputs(session, changes, &dirty, &include_roots)

	project := analysis_session_update_project(session, dirty[:], include_roots[:])
	added_dependencies := 0
	for {
		added := analysis_session_resolve_new_dependencies(session, &project)
		if added == 0 {
			break
		}
		added_dependencies += added
		project = analysis_session_update_project(session, {}, {})
	}

	return Update_Result {
		project = project,
		dirty_count = len(dirty),
		affected_count = len(dirty) + len(include_roots),
		added_dependencies = added_dependencies,
	}
}

analysis_session_project :: proc(session: ^Analysis_Session) -> analyze.Project_Analysis {
	analysis_session_ensure_initialized(session)
	return analyze.project_state_analysis(&session.project_state)
}

analysis_session_analyze_once :: proc(
	targets: []analyze.Source_Input,
	candidates: []analyze.Project_Candidate_Input,
	dependencies: []analyze.Source_Input,
	config: remote_deps.Dependency_Config,
	options: analyze.Analyze_Options,
	allocator: mem.Allocator,
) -> analyze.Project_Analysis {
	session := new(Analysis_Session, allocator)
	session^ = analysis_session_make(config, options, allocator)
	changes := make(
		[dynamic]Input_Change,
		0,
		len(targets) + len(candidates) + len(dependencies),
		context.temp_allocator,
	)
	for target in targets {
		append(&changes, Input_Change{kind = .Upsert, role = .Target, input = target})
	}
	for candidate in candidates {
		append(
			&changes,
			Input_Change {
				kind = .Upsert,
				role = .Candidate,
				input = candidate.input,
				object_name = candidate.object_name,
			},
		)
	}
	for dependency in dependencies {
		append(
			&changes,
			Input_Change {
				kind = .Upsert,
				role = .Dependency,
				input = dependency,
				immutable = true,
			},
		)
	}
	return analysis_session_apply_changes(session, changes[:]).project
}

session_memory_make :: proc(backing_allocator: mem.Allocator) -> Session_Memory {
	_ = backing_allocator
	memory: Session_Memory
	_ = virtual.arena_init_growing(&memory.session_arena)
	_ = virtual.arena_init_growing(&memory.update_arena)
	_ = virtual.arena_init_growing(&memory.dep_arena)
	memory.allocator = virtual.arena_allocator(&memory.session_arena)
	memory.temp_allocator = virtual.arena_allocator(&memory.update_arena)
	memory.dep_allocator = virtual.arena_allocator(&memory.dep_arena)
	return memory
}

session_memory_destroy :: proc(memory: ^Session_Memory) {
	for i in 0 ..< len(memory.unit_arenas) {
		virtual.arena_destroy(&memory.unit_arenas[i])
	}
	virtual.arena_destroy(&memory.dep_arena)
	virtual.arena_destroy(&memory.update_arena)
	virtual.arena_destroy(&memory.session_arena)
	memory^ = {}
}

session_memory_begin_update :: proc(memory: ^Session_Memory) {
	session_memory_bind(memory)
	virtual.arena_free_all(&memory.update_arena)
	memory.temp_allocator = virtual.arena_allocator(&memory.update_arena)
}

session_memory_ensure_unit :: proc(memory: ^Session_Memory, unit_index: int) {
	session_memory_bind(memory)
	for len(memory.unit_arenas) <= unit_index {
		append(&memory.unit_arenas, virtual.Arena{})
		_ = virtual.arena_init_growing(&memory.unit_arenas[len(memory.unit_arenas) - 1])
		append(&memory.unit_allocators, mem.Allocator{})
	}
	session_memory_refresh_unit_allocators(memory)
}

session_memory_reset_unit :: proc(memory: ^Session_Memory, unit_index: int) -> mem.Allocator {
	session_memory_ensure_unit(memory, unit_index)
	virtual.arena_free_all(&memory.unit_arenas[unit_index])
	memory.unit_allocators[unit_index] = virtual.arena_allocator(&memory.unit_arenas[unit_index])
	return memory.unit_allocators[unit_index]
}

@(private)
analysis_session_ensure_initialized :: proc(session: ^Analysis_Session) {
	session_memory_bind(&session.memory)
	if session.inputs.allocator.procedure != nil {
		return
	}
	allocator := session.memory.allocator
	dep_allocator := session.memory.dep_allocator
	session.project_state = analyze.project_state_make(session.memory.unit_allocators[:], allocator)
	session.inputs = make([dynamic]Input_Record, 0, 16, allocator)
	session.uri_to_input = make(map[string]Input_Id, 32, allocator)
	session.targets = make([dynamic]analyze.Source_Input, 0, 4, allocator)
	session.candidates = make([dynamic]analyze.Project_Candidate_Input, 0, 16, allocator)
	session.dependencies = make([dynamic]analyze.Source_Input, 0, 16, allocator)
	session.dependency_state = Remote_Dependency_State {
		seen_artifacts           = make(map[i64]bool, 16, dep_allocator),
		seen_local_candidates    = make(map[analyze.Remote_Dependency_Key]bool, 64, dep_allocator),
		seen_adt_candidates      = make(map[analyze.Remote_Dependency_Key]bool, 64, dep_allocator),
		seen_typepool_candidates = make(map[analyze.Remote_Dependency_Key]bool, 64, dep_allocator),
	}
}

analysis_session_reconcile_inputs :: proc(
	session: ^Analysis_Session,
	changes: []Input_Change,
	dirty: ^[dynamic]analyze.Unit_Id,
	include_roots: ^[dynamic]analyze.Unit_Id,
) {
	analysis_session_ensure_initialized(session)
	for change in changes {
		key := uri_key.normalized_uri_path_key(change.input.uri, context.temp_allocator)
		if change.kind == .Delete {
			if id, ok := session.uri_to_input[key]; ok {
				record := &session.inputs[int(id)]
				if .Immutable in record.flags && !change.immutable {
					continue
				}
				if record.unit != analyze.INVALID_UNIT_ID {
					tombstone := record.input
					tombstone.source = ""
					unit_id, _ := analyze.project_state_upsert_input(
						&session.project_state,
						tombstone,
						-1,
						session.memory.allocator,
					)
					session_push_unique_unit(dirty, unit_id)
					session_push_unique_unit(include_roots, unit_id)
				}
				record.flags -= {.Active}
				delete_key(&session.uri_to_input, key)
			}
			continue
		}

		hash := session_source_hash(change.input)
		if id, ok := session.uri_to_input[key]; ok {
			record := &session.inputs[int(id)]
			if .Immutable in record.flags && !change.immutable {
				continue
			}
			if record.role == .Target && change.role == .Candidate {
				if record.object_name == "" && change.object_name != "" {
					record.object_name = strings.clone(change.object_name, session.memory.allocator)
				}
				continue
			}
			if record.content_hash == hash &&
			   record.role == change.role &&
			   record.object_name == change.object_name &&
			   (.Immutable in record.flags) == change.immutable {
				continue
			}
			record.role = change.role
			record.input = session_source_input_clone(change.input, session.memory.allocator)
			record.object_name = strings.clone(change.object_name, session.memory.allocator)
			record.content_hash = hash
			record.flags = {.Active}
			if change.immutable {
				record.flags += {.Immutable}
			}
			if record.unit != analyze.INVALID_UNIT_ID {
				session_push_unique_unit(dirty, record.unit)
				session_push_unique_unit(include_roots, record.unit)
			}
			continue
		}

		owned := session_source_input_clone(change.input, session.memory.allocator)
		_ = analysis_session_record_owned_input(
			session,
			change.role,
			owned,
			change.object_name,
			change.immutable,
		)
	}
	analysis_session_rebuild_role_inputs(session)
}

analysis_session_update_project :: proc(
	session: ^Analysis_Session,
	dirty: []analyze.Unit_Id,
	include_roots: []analyze.Unit_Id,
) -> analyze.Project_Analysis {
	analysis_session_ensure_initialized(session)
	required_units :=
		len(session.project_state.units) +
		len(session.targets) +
		len(session.dependencies) +
		len(session.candidates)
	if required_units > 0 {
		session_memory_ensure_unit(&session.memory, required_units - 1)
	}
	session.project_state.unit_allocators = session.memory.unit_allocators[:]
	candidates := analysis_session_project_candidates(session)
	project := analyze.project_state_apply_dirty_inputs(
		&session.project_state,
		session.targets[:],
		candidates[:],
		session.dependencies[:],
		dirty,
		include_roots,
		session.options,
		session.memory.allocator,
	)
	analysis_session_refresh_input_units(session)
	return project
}

@(private)
analysis_session_project_candidates :: proc(
	session: ^Analysis_Session,
) -> [dynamic]analyze.Project_Candidate_Input {
	candidates := make(
		[dynamic]analyze.Project_Candidate_Input,
		0,
		len(session.targets) + len(session.candidates),
		context.temp_allocator,
	)
	seen := make(map[string]bool, len(session.targets) + len(session.candidates), context.temp_allocator)
	for record in session.inputs {
		if .Active in record.flags && record.role == .Target {
			analysis_session_append_candidate(&candidates, &seen, record.input, record.object_name)
		}
	}
	for candidate in session.candidates {
		analysis_session_append_candidate(&candidates, &seen, candidate.input, candidate.object_name)
	}
	return candidates
}

@(private)
analysis_session_append_candidate :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	seen: ^map[string]bool,
	input: analyze.Source_Input,
	object_name: string,
) {
	key := uri_key.normalized_uri_path_key(input.uri, context.temp_allocator)
	if key in seen^ {
		return
	}
	seen^[key] = true
	append(candidates, analyze.Project_Candidate_Input{input = input, object_name = object_name})
}

analysis_session_resolve_new_dependencies :: proc(
	session: ^Analysis_Session,
	project: ^analyze.Project_Analysis,
) -> int {
	analysis_session_ensure_initialized(session)
	_ = project
	remote_candidates := analyze.collect_project_state_remote_dependency_candidates(
		&session.project_state,
		true,
		context.temp_allocator,
	)
	if len(remote_candidates) == 0 {
		return 0
	}

	added := 0
	has_cache := session.config.cache != nil
	has_profile := session.config.profile != nil
	cache_result := remote_deps.Cache_Phase_Result {
		adt_candidates = make(
			[dynamic]analyze.Remote_Dependency_Candidate,
			0,
			len(remote_candidates),
			context.temp_allocator,
		),
		local_candidates = make(
			[dynamic]analyze.Remote_Dependency_Candidate,
			0,
			len(remote_candidates),
			context.temp_allocator,
		),
	}
	if has_cache {
		cache_candidates := remote_deps.unseen_remote_candidates(
			remote_candidates[:],
			nil,
			context.temp_allocator,
		)
		connection_key :=
			adt.client_connection_key(session.config.adt_client, context.temp_allocator) if session.config.adt_client != nil else ""
		old_candidate_count := len(session.candidates)
		old_dependency_count := len(session.dependencies)
		cache_result = remote_deps.add_dependency_cache_matches(
			&session.candidates,
			&session.dependencies,
			cache_candidates[:],
			session.config.cache,
			session.config.profile,
			session.config.cache_any_profile || !has_profile,
			connection_key,
			&session.dependency_state.seen_artifacts,
			session.options.pool,
			session.targets[0].uri if len(session.targets) > 0 else "",
			"session_cache_any" if session.config.cache_any_profile || !has_profile else "session_cache",
		)
		added += analysis_session_record_appended_inputs(
			session,
			old_candidate_count,
			old_dependency_count,
		)
		if cache_result.added {
			return added
		}
	} else {
		for candidate in remote_candidates {
			append(&cache_result.adt_candidates, candidate)
			append(&cache_result.local_candidates, candidate)
		}
	}

	if session.config.adt_client != nil {
		adt_candidates := remote_deps.unseen_remote_candidates(
			cache_result.adt_candidates[:],
			&session.dependency_state.seen_adt_candidates,
			context.temp_allocator,
		)
		if len(adt_candidates) > 0 {
			old_candidate_count := len(session.candidates)
			old_dependency_count := len(session.dependencies)
			if remote_deps.add_adt_matches_with_client(
				&session.candidates,
				&session.dependencies,
				adt_candidates[:],
				session.config.cache if has_cache && has_profile else nil,
				session.config.profile if has_cache && has_profile else nil,
				session.config.adt_client,
				session.options.pool,
				session.targets[0].uri if len(session.targets) > 0 else "",
			) {
				added += analysis_session_record_appended_inputs(
					session,
					old_candidate_count,
					old_dependency_count,
				)
				return added
			}
			added += analysis_session_record_appended_inputs(
				session,
				old_candidate_count,
				old_dependency_count,
			)
		}
	}

	if session.config.adt_client != nil &&
	   adt.typepool_resolver_enabled(session.config.adt_client) {
		typepool_candidates := remote_deps.unseen_remote_candidates(
			cache_result.local_candidates[:],
			&session.dependency_state.seen_typepool_candidates,
			context.temp_allocator,
		)
		if len(typepool_candidates) > 0 {
			old_candidate_count := len(session.candidates)
			old_dependency_count := len(session.dependencies)
			if remote_deps.add_typepool_resolver_matches(
				&session.candidates,
				&session.dependencies,
				typepool_candidates[:],
				session.config.cache if has_cache && has_profile else nil,
				session.config.profile if has_cache && has_profile else nil,
				session.config.adt_client,
				session.options.pool,
				session.targets[0].uri if len(session.targets) > 0 else "",
			) {
				added += analysis_session_record_appended_inputs(
					session,
					old_candidate_count,
					old_dependency_count,
				)
				return added
			}
			added += analysis_session_record_appended_inputs(
				session,
				old_candidate_count,
				old_dependency_count,
			)
		}
	}

	if len(session.config.local_export_roots) > 0 {
		local_candidates := remote_deps.unseen_remote_candidates(
			cache_result.local_candidates[:],
			&session.dependency_state.seen_local_candidates,
			context.temp_allocator,
		)
		if len(local_candidates) > 0 {
			old_candidate_count := len(session.candidates)
			old_dependency_count := len(session.dependencies)
			if remote_deps.add_local_export_matches(
				&session.candidates,
				&session.dependencies,
				local_candidates[:],
				session.config.cache if has_cache && has_profile else nil,
				session.config.profile if has_cache && has_profile else nil,
				session.config.local_export_roots,
				session.targets[0].uri if len(session.targets) > 0 else "",
				session.memory.allocator,
			) {
				added += analysis_session_record_appended_inputs(
					session,
					old_candidate_count,
					old_dependency_count,
				)
				return added
			}
			added += analysis_session_record_appended_inputs(
				session,
				old_candidate_count,
				old_dependency_count,
			)
		}
	}
	return added
}

analysis_session_insert_dependency_input :: proc(
	session: ^Analysis_Session,
	input: analyze.Source_Input,
	candidate: analyze.Remote_Dependency_Candidate,
	object_name: string,
) -> bool {
	owned := session_source_input_clone(input, session.memory.allocator)
	if candidate.kind == .Include {
		append(
			&session.candidates,
			analyze.Project_Candidate_Input {
				input = owned,
				object_name = strings.clone(
					object_name if object_name != "" else candidate.name,
					session.memory.allocator,
				),
			},
		)
		return analysis_session_record_owned_input(
			session,
			.Candidate,
			owned,
			object_name if object_name != "" else candidate.name,
			true,
		)
	}
	append(&session.dependencies, owned)
	return analysis_session_record_owned_input(session, .Dependency, owned, object_name, true)
}

@(private)
session_memory_refresh_unit_allocators :: proc(memory: ^Session_Memory) {
	for i in 0 ..< len(memory.unit_arenas) {
		memory.unit_allocators[i] = virtual.arena_allocator(&memory.unit_arenas[i])
	}
}

@(private)
session_memory_bind :: proc(memory: ^Session_Memory) {
	memory.allocator = virtual.arena_allocator(&memory.session_arena)
	memory.temp_allocator = virtual.arena_allocator(&memory.update_arena)
	memory.dep_allocator = virtual.arena_allocator(&memory.dep_arena)
	if memory.unit_arenas.allocator.procedure == nil {
		memory.unit_arenas = make([dynamic]virtual.Arena, 0, 16, memory.allocator)
		memory.unit_allocators = make([dynamic]mem.Allocator, 0, 16, memory.allocator)
		return
	}
	session_memory_refresh_unit_allocators(memory)
}

@(private)
analysis_session_record_appended_inputs :: proc(
	session: ^Analysis_Session,
	old_candidate_count: int,
	old_dependency_count: int,
) -> int {
	added := 0
	for i in old_candidate_count ..< len(session.candidates) {
		candidate := session.candidates[i]
		if analysis_session_record_owned_input(
			session,
			.Candidate,
			candidate.input,
			candidate.object_name,
			true,
		) {
			added += 1
		}
	}
	for i in old_dependency_count ..< len(session.dependencies) {
		if analysis_session_record_owned_input(
			session,
			.Dependency,
			session.dependencies[i],
			"",
			true,
		) {
			added += 1
		}
	}
	return added
}

@(private)
analysis_session_record_owned_input :: proc(
	session: ^Analysis_Session,
	role: Input_Role,
	input: analyze.Source_Input,
	object_name: string,
	immutable: bool,
) -> bool {
	key := uri_key.normalized_uri_path_key(input.uri, context.temp_allocator)
	hash := session_source_hash(input)
	if id, ok := session.uri_to_input[key]; ok {
		record := &session.inputs[int(id)]
		if record.content_hash == hash && .Active in record.flags {
			return false
		}
		record.role = role
		record.input = input
		record.object_name = strings.clone(object_name, session.memory.allocator)
		record.content_hash = hash
		record.flags = {.Active}
		if immutable {
			record.flags += {.Immutable}
		}
		return true
	}

	id := Input_Id(len(session.inputs))
	stored_key := uri_key.normalized_uri_path_key(input.uri, session.memory.allocator)
	flags := Input_Flags{.Active}
	if immutable {
		flags += {.Immutable}
	}
	session.uri_to_input[stored_key] = id
	append(
		&session.inputs,
		Input_Record {
			id = id,
			role = role,
			input = input,
			object_name = strings.clone(object_name, session.memory.allocator),
			content_hash = hash,
			unit = analyze.INVALID_UNIT_ID,
			flags = flags,
		},
	)
	return true
}

@(private)
analysis_session_rebuild_role_inputs :: proc(session: ^Analysis_Session) {
	clear(&session.targets)
	clear(&session.candidates)
	clear(&session.dependencies)
	for record in session.inputs {
		if !(.Active in record.flags) {
			continue
		}
		#partial switch record.role {
		case .Target:
			append(&session.targets, record.input)
		case .Candidate:
			append(
				&session.candidates,
				analyze.Project_Candidate_Input {
					input = record.input,
					object_name = record.object_name,
				},
			)
		case .Dependency:
			append(&session.dependencies, record.input)
		}
	}
}

@(private)
analysis_session_refresh_input_units :: proc(session: ^Analysis_Session) {
	for &record in session.inputs {
		if !(.Active in record.flags) {
			continue
		}
		key := uri_key.normalized_uri_path_key(record.input.uri, context.temp_allocator)
		if unit_id, ok := session.project_state.uri_to_unit[key]; ok {
			record.unit = unit_id
		}
	}
}

@(private)
session_source_input_clone :: proc(
	input: analyze.Source_Input,
	allocator: mem.Allocator,
) -> analyze.Source_Input {
	return analyze.Source_Input {
		uri = strings.clone(input.uri, allocator),
		source = strings.clone(input.source, allocator),
		mode = input.mode,
	}
}

@(private)
session_push_unique_unit :: proc(units: ^[dynamic]analyze.Unit_Id, unit_id: analyze.Unit_Id) {
	if unit_id == analyze.INVALID_UNIT_ID {
		return
	}
	for existing in units^ {
		if existing == unit_id {
			return
		}
	}
	append(units, unit_id)
}

@(private)
session_source_hash :: proc(input: analyze.Source_Input) -> u64 {
	out := hash.fnv64a(transmute([]byte)input.source)
	mode := [?]byte{byte(input.mode)}
	return hash.fnv64a(mode[:], out)
}
