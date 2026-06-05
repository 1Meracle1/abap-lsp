package abap_frontend_semantic_session

import analyze "src:semantic/analyze"
import deps "src:semantic/dependencies"
import execution "src:execution"
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
	unit:         analyze.Source_File_Id,
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

Session_Memory :: struct {
	session_arena:   virtual.Arena,
	update_arena:    virtual.Arena,
	dep_arena:       virtual.Arena,
	unit_arenas:     [dynamic]virtual.Arena,
	source_file_allocators: [dynamic]mem.Allocator,

	allocator:      mem.Allocator,
	temp_allocator: mem.Allocator,
	dep_allocator:  mem.Allocator,
}

Analysis_Session :: struct {
	project_state: analyze.Project_Snapshot_State,
	memory:        Session_Memory,

	inputs:       [dynamic]Input_Record,
	uri_to_input: map[string]Input_Id,

	targets:      [dynamic]analyze.Source_Input,
	candidates:   [dynamic]analyze.Project_Candidate_Input,
	dependencies: [dynamic]analyze.Source_Input,
	dependency_summaries: [dynamic]analyze.Summary_Provider_Input,

	dependency_state: remote_deps.Dependency_State,
	config:           remote_deps.Dependency_Config,
	pool:             ^execution.Pool,
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
	pool: ^execution.Pool,
	options: analyze.Analyze_Options,
	backing_allocator: mem.Allocator,
) -> Analysis_Session {
	memory := session_memory_make(backing_allocator)
	return Analysis_Session {
		memory = memory,
		config = config,
		pool = pool,
		options = options,
	}
}

analysis_session_destroy :: proc(session: ^Analysis_Session) {
	session_memory_destroy(&session.memory)
	session^ = {}
}

analysis_session_seed_dependency_summaries :: proc(
	session: ^Analysis_Session,
	summaries: []analyze.Summary_Provider_Input,
) {
	if session == nil || len(summaries) == 0 {
		return
	}
	analysis_session_ensure_initialized(session)
	for summary in summaries {
		append(
			&session.dependency_summaries,
			analyze.dependency_summary_input_clone(summary, session.memory.allocator),
		)
	}
	_ = analysis_session_dedupe_dependency_summaries(session)
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

	dirty := make([dynamic]analyze.Source_File_Id, 0, len(changes), context.temp_allocator)
	include_roots := make([dynamic]analyze.Source_File_Id, 0, len(changes), context.temp_allocator)
	analysis_session_reconcile_inputs(session, changes, &dirty, &include_roots)

	project := analysis_session_update_project(session, dirty[:], include_roots[:])
	local_include_roots := make([dynamic]analyze.Source_File_Id, 0, 8, context.temp_allocator)
	if analysis_session_add_local_include_aliases(session, &local_include_roots) {
		project = analysis_session_update_project(session, {}, local_include_roots[:])
	}
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
	pool: ^execution.Pool,
	options: analyze.Analyze_Options,
	allocator: mem.Allocator,
) -> analyze.Project_Analysis {
	session := new(Analysis_Session, allocator)
	session^ = analysis_session_make(config, pool, options, allocator)
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

session_memory_ensure_unit :: proc(memory: ^Session_Memory, source_file_index: int) {
	session_memory_bind(memory)
	for len(memory.unit_arenas) <= source_file_index {
		append(&memory.unit_arenas, virtual.Arena{})
		_ = virtual.arena_init_growing(&memory.unit_arenas[len(memory.unit_arenas) - 1])
		append(&memory.source_file_allocators, mem.Allocator{})
	}
	session_memory_refresh_source_file_allocators(memory)
}

session_memory_reset_unit :: proc(memory: ^Session_Memory, source_file_index: int) -> mem.Allocator {
	session_memory_ensure_unit(memory, source_file_index)
	virtual.arena_free_all(&memory.unit_arenas[source_file_index])
	memory.source_file_allocators[source_file_index] = virtual.arena_allocator(&memory.unit_arenas[source_file_index])
	return memory.source_file_allocators[source_file_index]
}

@(private)
analysis_session_ensure_initialized :: proc(session: ^Analysis_Session) {
	session_memory_bind(&session.memory)
	if session.inputs.allocator.procedure != nil {
		return
	}
	allocator := session.memory.allocator
	dep_allocator := session.memory.dep_allocator
	session.project_state = analyze.project_state_make(session.memory.source_file_allocators[:], allocator)
	session.inputs = make([dynamic]Input_Record, 0, 16, allocator)
	session.uri_to_input = make(map[string]Input_Id, 32, allocator)
	session.targets = make([dynamic]analyze.Source_Input, 0, 4, allocator)
	session.candidates = make([dynamic]analyze.Project_Candidate_Input, 0, 16, allocator)
	session.dependencies = make([dynamic]analyze.Source_Input, 0, 16, allocator)
	session.dependency_summaries = make([dynamic]analyze.Summary_Provider_Input, 0, 16, allocator)
	session.dependency_state = remote_deps.dependency_state_make(dep_allocator)
}

analysis_session_reconcile_inputs :: proc(
	session: ^Analysis_Session,
	changes: []Input_Change,
	dirty: ^[dynamic]analyze.Source_File_Id,
	include_roots: ^[dynamic]analyze.Source_File_Id,
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
				if record.unit != analyze.INVALID_SOURCE_FILE_ID {
					tombstone := record.input
					tombstone.source = ""
					source_file_id, _ := analyze.project_state_upsert_input(
						&session.project_state,
						tombstone,
						-1,
						session.memory.allocator,
					)
					session_push_unique_unit(dirty, source_file_id)
					session_push_unique_unit(include_roots, source_file_id)
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
			if record.unit != analyze.INVALID_SOURCE_FILE_ID {
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
	dirty: []analyze.Source_File_Id,
	include_roots: []analyze.Source_File_Id,
) -> analyze.Project_Analysis {
	analysis_session_ensure_initialized(session)
	required_units :=
		len(session.project_state.source_files) +
		len(session.targets) +
		len(session.dependencies) +
		len(session.candidates) +
		len(session.dependency_summaries)
	if required_units > 0 {
		session_memory_ensure_unit(&session.memory, required_units - 1)
	}
	session.project_state.source_file_allocators = session.memory.source_file_allocators[:]
	candidates := analysis_session_project_candidates(session)
	project := analyze.project_state_apply_dirty_inputs_with_summaries(
		&session.project_state,
		session.targets[:],
		candidates[:],
		session.dependencies[:],
		session.dependency_summaries[:],
		dirty,
		include_roots,
		session.pool,
		session.options,
		session.memory.allocator,
	)
	analysis_session_refresh_input_units(session)
	return project
}

@(private)
analysis_session_add_local_include_aliases :: proc(
	session: ^Analysis_Session,
	include_roots: ^[dynamic]analyze.Source_File_Id,
) -> bool {
	changed := false
	for &record in session.inputs {
		if !(.Active in record.flags) || record.unit == analyze.INVALID_SOURCE_FILE_ID {
			continue
		}
		root := analysis_session_unit(session, record.unit)
		if root == nil || !analysis_session_unit_is_report_root(root) {
			continue
		}
		names := make([dynamic]string, 0, len(root.include_edges), context.temp_allocator)
		for edge in root.include_edges {
			if !edge.has_target && edge.name != "" {
				append(&names, edge.name)
			}
		}
		if len(names) == 0 {
			continue
		}

		dir := analysis_session_uri_parent_dir_key(record.input.uri, context.temp_allocator)
		members := make([dynamic]int, 0, len(names), context.temp_allocator)
		for member, i in session.inputs {
			if !(.Active in member.flags) ||
			   !(member.role == .Target || member.role == .Candidate) ||
			   member.unit == analyze.INVALID_SOURCE_FILE_ID ||
			   member.unit == record.unit ||
			   member.object_name != "" ||
			   analysis_session_uri_parent_dir_key(member.input.uri, context.temp_allocator) != dir {
				continue
			}
			unit := analysis_session_unit(session, member.unit)
			if unit == nil || analysis_session_unit_is_report_root(unit) {
				continue
			}
			append(&members, i)
		}
		if len(members) != len(names) {
			continue
		}

		ordered, ordered_ok := analysis_session_order_members(session, members[:], context.temp_allocator)
		if !ordered_ok {
			continue
		}
		for name, i in names {
			member := &session.inputs[ordered[i]]
			member.object_name = strings.clone(name, session.memory.allocator)
			session_push_unique_unit(include_roots, record.unit)
			session_push_unique_unit(include_roots, member.unit)
			changed = true
		}
	}
	if changed {
		analysis_session_rebuild_role_inputs(session)
	}
	return changed
}

@(private)
analysis_session_order_members :: proc(
	session: ^Analysis_Session,
	members: []int,
	allocator: mem.Allocator,
) -> ([dynamic]int, bool) {
	order := make([dynamic]int, 0, len(members), allocator)
	depends_on := make([][dynamic]int, len(members), allocator)
	for i in 0 ..< len(members) {
		depends_on[i] = make([dynamic]int, 0, 2, allocator)
		for j in 0 ..< len(members) {
			if i != j && analysis_session_member_depends_on(session, members[i], members[j]) {
				append(&depends_on[i], j)
			}
		}
	}

	selected := make([]bool, len(members), allocator)
	for len(order) < len(members) {
		next := -1
		for i in 0 ..< len(members) {
			ready := true
			for dependency in depends_on[i] {
				if !selected[dependency] {
					ready = false
					break
				}
			}
			if selected[i] || !ready {
				continue
			}
			if next >= 0 {
				return order, false
			}
			next = i
		}
		if next < 0 {
			return order, false
		}
		selected[next] = true
		append(&order, members[next])
	}
	return order, true
}

@(private)
analysis_session_member_depends_on :: proc(
	session: ^Analysis_Session,
	consumer_record, provider_record: int,
) -> bool {
	consumer := analysis_session_unit(session, session.inputs[consumer_record].unit)
	provider := analysis_session_unit(session, session.inputs[provider_record].unit)
	if consumer == nil || provider == nil {
		return false
	}
	for ref in consumer.references {
		if ref.has_resolution || ref.kind == .Include {
			continue
		}
		for symbol in provider.symbols {
			if symbol.scope == provider.root_scope &&
			   !analyze.symbol_kind_is_builtin(symbol.kind) &&
			   strings.equal_fold(symbol.name, ref.name) &&
			   analyze.symbol_kind_occupies(symbol.kind, ref.namespace) {
				return true
			}
		}
	}
	return false
}

@(private)
analysis_session_unit_is_report_root :: proc(unit: ^analyze.Source_File_Provider) -> bool {
	for symbol in unit.symbols {
		if symbol.scope == unit.root_scope && symbol.kind == .Report {
			return true
		}
	}
	return false
}

@(private)
analysis_session_unit :: proc(
	session: ^Analysis_Session,
	source_file_id: analyze.Source_File_Id,
) -> ^analyze.Source_File_Provider {
	source_file_index := analyze.source_file_id_index(source_file_id)
	if source_file_index < 0 || source_file_index >= len(session.project_state.source_files) {
		return nil
	}
	return &session.project_state.source_files[source_file_index]
}

@(private)
analysis_session_uri_parent_dir_key :: proc(uri: string, allocator: mem.Allocator) -> string {
	normalized := uri_key.normalized_uri_path_key(uri, allocator)
	for i := len(normalized) - 1; i >= 0; i -= 1 {
		if normalized[i] == '/' {
			return normalized[:i]
		}
	}
	return ""
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
		false,
		context.temp_allocator,
	)
	old_candidate_count := len(session.candidates)
	old_dependency_count := len(session.dependencies)
	old_summary_count := len(session.dependency_summaries)
	added := remote_deps.resolve_dependency_candidates(
		&session.candidates,
		&session.dependencies,
		&session.dependency_summaries,
		remote_candidates[:],
		&session.config,
		&session.dependency_state,
		session.pool,
		session.targets[0].uri if len(session.targets) > 0 else "",
	)
	summary_count := analysis_session_dedupe_dependency_summaries(session)
	if added == 0 {
		return 0
	}
	summary_added := summary_count - old_summary_count
	if summary_added < 0 {
		summary_added = 0
	}
	return analysis_session_record_appended_inputs(session, old_candidate_count, old_dependency_count) +
	       summary_added
}

@(private)
analysis_session_dedupe_dependency_summaries :: proc(session: ^Analysis_Session) -> int {
	if len(session.dependency_summaries) <= 1 {
		return len(session.dependency_summaries)
	}
	seen := make(map[string]bool, len(session.dependency_summaries), context.temp_allocator)
	write := 0
	for summary in session.dependency_summaries {
		key := analysis_session_dependency_summary_key(summary, context.temp_allocator)
		if key != "" && key in seen {
			continue
		}
		if key != "" {
			seen[key] = true
		}
		session.dependency_summaries[write] = summary
		write += 1
	}
	resize(&session.dependency_summaries, write)
	return write
}

@(private)
analysis_session_dependency_summary_key :: proc(
	summary: analyze.Summary_Provider_Input,
	allocator: mem.Allocator,
) -> string {
	if summary.uri != "" {
		return strings.clone(summary.uri, allocator)
	}
	out := strings.builder_make(allocator)
	strings.write_string(&out, summary.object_kind)
	strings.write_byte(&out, ':')
	strings.write_string(&out, summary.object_name)
	return strings.to_string(out)
}

analysis_session_insert_dependency_input :: proc(
	session: ^Analysis_Session,
	input: analyze.Source_Input,
	candidate: deps.Remote_Dependency_Candidate,
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
session_memory_refresh_source_file_allocators :: proc(memory: ^Session_Memory) {
	for i in 0 ..< len(memory.unit_arenas) {
		memory.source_file_allocators[i] = virtual.arena_allocator(&memory.unit_arenas[i])
	}
}

@(private)
session_memory_bind :: proc(memory: ^Session_Memory) {
	memory.allocator = virtual.arena_allocator(&memory.session_arena)
	memory.temp_allocator = virtual.arena_allocator(&memory.update_arena)
	memory.dep_allocator = virtual.arena_allocator(&memory.dep_arena)
	if memory.unit_arenas.allocator.procedure == nil {
		memory.unit_arenas = make([dynamic]virtual.Arena, 0, 16, memory.allocator)
		memory.source_file_allocators = make([dynamic]mem.Allocator, 0, 16, memory.allocator)
		return
	}
	session_memory_refresh_source_file_allocators(memory)
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
			unit = analyze.INVALID_SOURCE_FILE_ID,
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
		if source_file_id, ok := session.project_state.uri_to_source_file[key]; ok {
			record.unit = source_file_id
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
		role = input.role,
	}
}

@(private)
session_push_unique_unit :: proc(units: ^[dynamic]analyze.Source_File_Id, source_file_id: analyze.Source_File_Id) {
	if source_file_id == analyze.INVALID_SOURCE_FILE_ID {
		return
	}
	for existing in units^ {
		if existing == source_file_id {
			return
		}
	}
	append(units, source_file_id)
}

@(private)
session_source_hash :: proc(input: analyze.Source_Input) -> u64 {
	out := hash.fnv64a(transmute([]byte)input.source)
	mode := [?]byte{byte(input.role)}
	return hash.fnv64a(mode[:], out)
}
