package abap_frontend_semantic2

import string_interner "src:string_interner"
import trace "src:trace"

import "core:mem"
import "core:strings"

Semantic_Graph_Project_Ref :: struct {
	id:        Semantic_Project_Id,
	role:      Semantic_Project_Role,
	root_key:  Semantic_Object_Key,
	root_path: string,
}

Semantic_Graph_Update :: struct {
	changed_files:            []Workspace_File_Input,
	removed_files:            []string,
	fetched_external_objects: []External_Interface_Input,
	fetched_external_sources: []External_Source_Input,
	external_frontier_stable: bool,
	blocked_dependencies:     []Semantic_Object_Key,
}

Semantic_Graph_Update_Result :: struct {
	allocator:                       mem.Allocator,
	generation:                      u64,
	new_fetch_requests:              [dynamic]Checker_Unresolved_Candidate,
	blocked_unresolved_dependencies: [dynamic]Checker_Unresolved_Candidate,
	rebuilt_external_projects:       [dynamic]Semantic_Object_Key,
	dirty_editable_projects:         [dynamic]Semantic_Graph_Project_Ref,
	deferred_editable_projects:      [dynamic]Semantic_Graph_Project_Ref,
	rebuilt_editable_projects:       [dynamic]Semantic_Graph_Project_Ref,
}

Semantic_Graph_Session :: struct {
	allocator:                       mem.Allocator,
	interner:                        ^string_interner.Interner,
	owns_interner:                   bool,
	generation:                      u64,
	editable_files:                  [dynamic]Workspace_File_Input,
	external_inputs:                 [dynamic]External_Interface_Input,
	external_source_inputs:          [dynamic]External_Source_Input,
	external:                        External_Semantics,
	analysis:                        Workspace_Analysis,
	has_analysis:                    bool,
	pending_dirty_editable_projects: [dynamic]Semantic_Graph_Project_Ref,
	pending_full_editable_rebuild:   bool,
}

semantic_graph_session_make :: proc(
	interner: ^string_interner.Interner = nil,
	allocator: mem.Allocator = context.allocator,
) -> Semantic_Graph_Session {
	owns_interner := false
	actual_interner := interner
	if actual_interner == nil {
		actual_interner = string_interner.create()
		owns_interner = true
	}
	return Semantic_Graph_Session {
		allocator = allocator,
		interner = actual_interner,
		owns_interner = owns_interner,
		editable_files = make([dynamic]Workspace_File_Input, 0, 8, allocator),
		external_inputs = make([dynamic]External_Interface_Input, 0, 8, allocator),
		external_source_inputs = make([dynamic]External_Source_Input, 0, 8, allocator),
		external = external_semantics_make(actual_interner, allocator),
		pending_dirty_editable_projects = make(
			[dynamic]Semantic_Graph_Project_Ref,
			0,
			8,
			allocator,
		),
	}
}

semantic_graph_session_destroy :: proc(session: ^Semantic_Graph_Session) {
	if session == nil {
		return
	}
	if session.has_analysis {
		semantic_workspace_analysis_destroy(&session.analysis)
	}
	for &input in session.editable_files {
		semantic_graph_workspace_file_input_destroy(&input, session.allocator)
	}
	for &input in session.external_inputs {
		semantic_graph_external_input_destroy(&input, session.allocator)
	}
	for &input in session.external_source_inputs {
		semantic_graph_external_source_input_destroy(&input, session.allocator)
	}
	semantic_graph_project_ref_list_destroy(
		&session.pending_dirty_editable_projects,
		session.allocator,
	)
	if session.editable_files.allocator.procedure != nil {
		delete(session.editable_files)
	}
	if session.external_inputs.allocator.procedure != nil {
		delete(session.external_inputs)
	}
	if session.external_source_inputs.allocator.procedure != nil {
		delete(session.external_source_inputs)
	}
	external_semantics_destroy(&session.external)
	if session.owns_interner {
		string_interner.destroy(session.interner)
	}
	session^ = {}
}

semantic_graph_update_result_make :: proc(
	generation: u64,
	allocator: mem.Allocator = context.allocator,
) -> Semantic_Graph_Update_Result {
	return Semantic_Graph_Update_Result {
		allocator = allocator,
		generation = generation,
		new_fetch_requests = make([dynamic]Checker_Unresolved_Candidate, 0, 8, allocator),
		blocked_unresolved_dependencies = make(
			[dynamic]Checker_Unresolved_Candidate,
			0,
			8,
			allocator,
		),
		rebuilt_external_projects = make([dynamic]Semantic_Object_Key, 0, 4, allocator),
		dirty_editable_projects = make([dynamic]Semantic_Graph_Project_Ref, 0, 4, allocator),
		deferred_editable_projects = make([dynamic]Semantic_Graph_Project_Ref, 0, 4, allocator),
		rebuilt_editable_projects = make([dynamic]Semantic_Graph_Project_Ref, 0, 4, allocator),
	}
}

semantic_graph_update_result_destroy :: proc(result: ^Semantic_Graph_Update_Result) {
	if result == nil {
		return
	}
	semantic_graph_project_ref_list_destroy(&result.dirty_editable_projects, result.allocator)
	semantic_graph_project_ref_list_destroy(&result.deferred_editable_projects, result.allocator)
	semantic_graph_project_ref_list_destroy(&result.rebuilt_editable_projects, result.allocator)
	if result.new_fetch_requests.allocator.procedure != nil {
		delete(result.new_fetch_requests)
	}
	if result.blocked_unresolved_dependencies.allocator.procedure != nil {
		delete(result.blocked_unresolved_dependencies)
	}
	if result.rebuilt_external_projects.allocator.procedure != nil {
		delete(result.rebuilt_external_projects)
	}
	result^ = {}
}

semantic_graph_session_apply_update :: proc(
	session: ^Semantic_Graph_Session,
	update: Semantic_Graph_Update,
) -> Semantic_Graph_Update_Result {
	assert(session != nil && session.interner != nil)
	when trace.ENABLED {
		trace_start := trace.now()
	}
	session.generation += 1
	result := semantic_graph_update_result_make(session.generation, session.allocator)

	editable_inputs_changed := false
	for path in update.removed_files {
		removed := false
		for i := 0; i < len(session.editable_files); i += 1 {
			if session.editable_files[i].path != path {
				continue
			}
			semantic_graph_workspace_file_input_destroy(&session.editable_files[i], session.allocator)
			ordered_remove(&session.editable_files, i)
			removed = true
			break
		}
		if removed {
			editable_inputs_changed = true
			semantic_graph_session_mark_file_projects_dirty(session, path, &result)
		}
	}
	for file in update.changed_files {
		if semantic_graph_session_upsert_editable_file(session, file) {
			editable_inputs_changed = true
			semantic_graph_session_mark_file_projects_dirty(session, file.path, &result)
		}
	}

	if len(update.fetched_external_objects) > 0 {
		semantic_graph_session_apply_external_updates(
			session,
			update.fetched_external_objects,
			&result,
		)
	}
	if len(update.fetched_external_sources) > 0 {
		semantic_graph_session_apply_external_source_updates(
			session,
			update.fetched_external_sources,
			&result,
		)
	}

	rebuild_editable := editable_inputs_changed
	if update.external_frontier_stable &&
	   (session.pending_full_editable_rebuild ||
			   len(session.pending_dirty_editable_projects) > 0) {
		rebuild_editable = true
	}

	if rebuild_editable {
		rebuild_all := session.pending_full_editable_rebuild || !session.has_analysis
		pending := semantic_graph_project_ref_list_clone(
			session.pending_dirty_editable_projects[:],
			session.allocator,
		)
		semantic_graph_session_rebuild_workspace(session)
		if rebuild_all {
			semantic_graph_session_add_all_current_editable_projects(
				session,
				&result.rebuilt_editable_projects,
			)
		} else {
			semantic_graph_session_add_current_refs_for_pending(
				session,
				pending[:],
				&result.rebuilt_editable_projects,
			)
		}
		semantic_graph_project_ref_list_destroy(&pending, session.allocator)
		semantic_graph_project_ref_list_destroy(
			&session.pending_dirty_editable_projects,
			session.allocator,
		)
		session.pending_full_editable_rebuild = false
	} else if !update.external_frontier_stable {
		semantic_graph_project_ref_list_append_clone(
			&result.deferred_editable_projects,
			session.pending_dirty_editable_projects[:],
			result.allocator,
		)
	}

	semantic_graph_collect_frontier(
		session,
		update.external_frontier_stable,
		update.blocked_dependencies,
		&result,
	)
	when trace.ENABLED {
		trace.eprintf(
			"[trace - semantic] semantic graph update generation=%d changed_files=%d external_objects=%d external_sources=%d stable=%v dirty_editable=%d deferred_editable=%d rebuilt_editable=%d rebuilt_external=%d new_fetch=%d blocked=%d elapsed_ms=%.3f\n",
			session.generation,
			len(update.changed_files),
			len(update.fetched_external_objects),
			len(update.fetched_external_sources),
			update.external_frontier_stable,
			len(result.dirty_editable_projects),
			len(result.deferred_editable_projects),
			len(result.rebuilt_editable_projects),
			len(result.rebuilt_external_projects),
			len(result.new_fetch_requests),
			len(result.blocked_unresolved_dependencies),
			trace.duration_ms_since(trace_start),
		)
	}
	return result
}

semantic_graph_session_current_analysis :: proc(
	session: ^Semantic_Graph_Session,
) -> ^Workspace_Analysis {
	if session == nil || !session.has_analysis {
		return nil
	}
	return &session.analysis
}

semantic_graph_session_upsert_editable_file :: proc(
	session: ^Semantic_Graph_Session,
	input: Workspace_File_Input,
) -> bool {
	assert(input.path != "")
	for &existing in session.editable_files {
		if existing.path != input.path {
			continue
		}
		if semantic_graph_workspace_file_input_equal(existing, input) {
			return false
		}
		semantic_graph_workspace_file_input_destroy(&existing, session.allocator)
		existing = semantic_graph_workspace_file_input_clone(input, session.allocator)
		return true
	}
	append(
		&session.editable_files,
		semantic_graph_workspace_file_input_clone(input, session.allocator),
	)
	return true
}

semantic_graph_session_upsert_external_input :: proc(
	session: ^Semantic_Graph_Session,
	input: External_Interface_Input,
) -> bool {
	key := external_interface_input_key(input)
	assert(semantic_object_key_is_valid(key))
	assert(input.path != "")
	for &existing in session.external_inputs {
		if external_interface_input_key(existing) != key {
			continue
		}
		if semantic_graph_external_input_equal(existing, input) {
			return false
		}
		semantic_graph_external_input_destroy(&existing, session.allocator)
		existing = semantic_graph_external_input_clone(input, session.allocator)
		if existing.generation == 0 {
			existing.generation = session.generation
		}
		return true
	}
	next := semantic_graph_external_input_clone(input, session.allocator)
	if next.generation == 0 {
		next.generation = session.generation
	}
	append(&session.external_inputs, next)
	return true
}

semantic_graph_session_upsert_external_source_input :: proc(
	session: ^Semantic_Graph_Session,
	input: External_Source_Input,
) -> bool {
	assert(input.path != "")
	for &existing in session.external_source_inputs {
		if existing.path != input.path {
			continue
		}
		if semantic_graph_external_source_input_equal(existing, input) {
			return false
		}
		semantic_graph_external_source_input_destroy(&existing, session.allocator)
		existing = semantic_graph_external_source_input_clone(input, session.allocator)
		if existing.generation == 0 {
			existing.generation = session.generation
		}
		_ = external_semantics_upsert_source_input(&session.external, existing)
		return true
	}
	next := semantic_graph_external_source_input_clone(input, session.allocator)
	if next.generation == 0 {
		next.generation = session.generation
	}
	append(&session.external_source_inputs, next)
	_ = external_semantics_upsert_source_input(&session.external, next)
	return true
}

semantic_graph_session_apply_external_updates :: proc(
	session: ^Semantic_Graph_Session,
	inputs: []External_Interface_Input,
	result: ^Semantic_Graph_Update_Result,
) {
	when trace.ENABLED {
		trace_start := trace.now()
		reanalyze_ms := 0.0
	}
	queue := make([dynamic]Semantic_Object_Key, 0, len(inputs), context.temp_allocator)
	processed := make([dynamic]Semantic_Object_Key, 0, len(inputs), context.temp_allocator)
	for input in inputs {
		key := external_interface_input_key(input)
		assert(semantic_object_key_is_valid(key))
		_ = semantic_graph_session_upsert_external_input(session, input)
		semantic_graph_object_key_list_add(&queue, key)
	}

	for cursor := 0; cursor < len(queue); cursor += 1 {
		key := queue[cursor]
		semantic_graph_object_key_list_add(&processed, key)
		semantic_graph_session_mark_waiters_for_provider(session, key, &queue, result)
		if input, ok := semantic_graph_session_external_input_for_key(session, key); ok {
			when trace.ENABLED {
				reanalyze_start := trace.now()
			}
			semantic_graph_session_reanalyze_external_input(session, input, result)
			when trace.ENABLED {
				reanalyze_ms += trace.duration_ms_since(reanalyze_start)
			}
		}
	}

	for key in processed {
		if session.has_analysis {
			semantic_graph_clear_unresolved_waiters_for_provider(
				&session.analysis.external_index,
				key,
			)
		}
		semantic_graph_clear_unresolved_waiters_for_provider(&session.external.index, key)
	}
	when trace.ENABLED {
		trace.eprintf(
			"[trace - semantic] semantic graph external updates inputs=%d processed=%d queued=%d rebuilt_external=%d rebuild_external_ms=%.3f elapsed_ms=%.3f\n",
			len(inputs),
			len(processed),
			len(queue),
			len(result.rebuilt_external_projects),
			reanalyze_ms,
			trace.duration_ms_since(trace_start),
		)
	}
}

semantic_graph_session_apply_external_source_updates :: proc(
	session: ^Semantic_Graph_Session,
	inputs: []External_Source_Input,
	result: ^Semantic_Graph_Update_Result,
) {
	when trace.ENABLED {
		trace_start := trace.now()
		reanalyze_ms := 0.0
	}
	queue := make([dynamic]Semantic_Object_Key, 0, len(inputs), context.temp_allocator)
	processed := make([dynamic]Semantic_Object_Key, 0, len(inputs), context.temp_allocator)
	for input in inputs {
		if !semantic_graph_session_upsert_external_source_input(session, input) {
			continue
		}
		for provided in input.provided_names {
			name := string_interner.insert(
				session.interner,
				strings.to_lower(provided, context.temp_allocator),
			)
			semantic_graph_object_key_list_add(
				&queue,
				Semantic_Object_Key{kind = .Include_Source, name = name},
			)
		}
	}

	for cursor := 0; cursor < len(queue); cursor += 1 {
		key := queue[cursor]
		semantic_graph_object_key_list_add(&processed, key)
		semantic_graph_session_mark_waiters_for_provider(session, key, &queue, result)
		if input, ok := semantic_graph_session_external_input_for_key(session, key); ok {
			when trace.ENABLED {
				reanalyze_start := trace.now()
			}
			semantic_graph_session_reanalyze_external_input(session, input, result)
			when trace.ENABLED {
				reanalyze_ms += trace.duration_ms_since(reanalyze_start)
			}
		}
	}

	for key in processed {
		if session.has_analysis {
			semantic_graph_clear_unresolved_waiters_for_provider(
				&session.analysis.external_index,
				key,
			)
		}
		semantic_graph_clear_unresolved_waiters_for_provider(&session.external.index, key)
	}
	when trace.ENABLED {
		trace.eprintf(
			"[trace - semantic] semantic graph external source updates inputs=%d processed=%d queued=%d rebuilt_external=%d rebuild_external_ms=%.3f elapsed_ms=%.3f\n",
			len(inputs),
			len(processed),
			len(queue),
			len(result.rebuilt_external_projects),
			reanalyze_ms,
			trace.duration_ms_since(trace_start),
		)
	}
}

semantic_graph_session_reanalyze_external_input :: proc(
	session: ^Semantic_Graph_Session,
	input: External_Interface_Input,
	result: ^Semantic_Graph_Update_Result,
) {
	key := external_interface_input_key(input)
	assert(semantic_object_key_is_valid(key))
	semantic_graph_session_sync_external_project_ids(session)
	record := external_semantics_reanalyze_interface_input(&session.external, input)
	semantic_graph_object_key_list_add(&result.rebuilt_external_projects, key)
	if session.has_analysis && record != nil {
		imported := semantic_project_record_clone_lists(
			record^,
			session.analysis.external_index.allocator,
		)
		_ = external_semantic_index_replace_project_record(
			&session.analysis.external_index,
			imported,
		)
	}
}

semantic_graph_session_sync_external_project_ids :: proc(session: ^Semantic_Graph_Session) {
	if session.has_analysis &&
	   session.external.index.next_project_id < session.analysis.external_index.next_project_id {
		session.external.index.next_project_id = session.analysis.external_index.next_project_id
	}
}

semantic_graph_session_mark_waiters_for_provider :: proc(
	session: ^Semantic_Graph_Session,
	key: Semantic_Object_Key,
	external_queue: ^[dynamic]Semantic_Object_Key,
	result: ^Semantic_Graph_Update_Result,
) {
	index := semantic_graph_session_current_index(session)
	if index == nil {
		return
	}
	project_ids := make([dynamic]Semantic_Project_Id, 0, 4, context.temp_allocator)
	semantic_graph_collect_project_ids_for_provider(index, key, &project_ids)
	for id in project_ids {
		record, ok := external_semantic_index_project_record(index, id)
		if !ok || record == nil {
			continue
		}
		if record.role == .External_Interface {
			semantic_graph_object_key_list_add(external_queue, record.root_key)
			continue
		}
		if record.role == .Editable_Root || record.role == .Include_Fragment {
			ref := semantic_graph_project_ref_from_record(session, record)
			semantic_graph_project_ref_list_add(
				&session.pending_dirty_editable_projects,
				ref,
				session.allocator,
			)
			semantic_graph_project_ref_list_add(
				&result.dirty_editable_projects,
				ref,
				result.allocator,
			)
		}
	}
}

semantic_graph_session_mark_file_projects_dirty :: proc(
	session: ^Semantic_Graph_Session,
	path: string,
	result: ^Semantic_Graph_Update_Result,
) {
	assert(path != "")
	if !session.has_analysis {
		session.pending_full_editable_rebuild = true
		return
	}
	projects := semantic_workspace_projects_for_file(&session.analysis, path)
	if len(projects) == 0 {
		session.pending_full_editable_rebuild = true
		semantic_graph_session_add_all_current_editable_projects(
			session,
			&result.dirty_editable_projects,
		)
		return
	}
	for project in projects {
		if record, ok := semantic_graph_session_record_for_project(session, project); ok {
			ref := semantic_graph_project_ref_from_record(session, record)
			semantic_graph_project_ref_list_add(
				&session.pending_dirty_editable_projects,
				ref,
				session.allocator,
			)
			semantic_graph_project_ref_list_add(
				&result.dirty_editable_projects,
				ref,
				result.allocator,
			)
		}
	}
}

semantic_graph_session_rebuild_workspace :: proc(session: ^Semantic_Graph_Session) {
	when trace.ENABLED {
		trace_start := trace.now()
	}
	if session.has_analysis {
		semantic_workspace_analysis_destroy(&session.analysis)
		session.has_analysis = false
	}
	session.analysis = semantic_workspace_analyze(
		Workspace_Input {
			files            = session.editable_files[:],
			external         = &session.external,
			external_sources = session.external_source_inputs[:],
			interner         = session.interner,
		},
		session.allocator,
	)
	session.has_analysis = true
	if session.external.index.next_project_id < session.analysis.external_index.next_project_id {
		session.external.index.next_project_id = session.analysis.external_index.next_project_id
	}
	when trace.ENABLED {
		trace.eprintf(
			"[trace - semantic] semantic graph rebuild workspace editable_files=%d external_sources=%d external_records=%d analysis_projects=%d elapsed_ms=%.3f\n",
			len(session.editable_files),
			len(session.external_source_inputs),
			len(session.external.index.projects),
			len(session.analysis.projects),
			trace.duration_ms_since(trace_start),
		)
	}
}

semantic_graph_collect_frontier :: proc(
	session: ^Semantic_Graph_Session,
	frontier_stable: bool,
	blocked: []Semantic_Object_Key,
	result: ^Semantic_Graph_Update_Result,
) {
	when trace.ENABLED {
		trace_start := trace.now()
		record_count := 0
		candidate_count := 0
	}
	index := semantic_graph_session_current_index(session)
	if index == nil {
		return
	}
	for record in index.projects {
		when trace.ENABLED {
			record_count += 1
			candidate_count += len(record.unresolved)
		}
		for candidate in record.unresolved {
			if semantic_graph_candidate_resolved(session, index, candidate) {
				continue
			}
			if frontier_stable || semantic_graph_candidate_is_blocked(candidate, blocked) {
				semantic_graph_candidate_list_add(
					&result.blocked_unresolved_dependencies,
					candidate,
				)
			} else {
				semantic_graph_candidate_list_add(&result.new_fetch_requests, candidate)
			}
		}
	}
	when trace.ENABLED {
		trace.eprintf(
			"[trace - semantic] semantic graph collect frontier stable=%v records=%d candidates=%d new_fetch=%d blocked=%d elapsed_ms=%.3f\n",
			frontier_stable,
			record_count,
			candidate_count,
			len(result.new_fetch_requests),
			len(result.blocked_unresolved_dependencies),
			trace.duration_ms_since(trace_start),
		)
	}
}

semantic_graph_session_current_index :: proc(
	session: ^Semantic_Graph_Session,
) -> ^External_Semantic_Index {
	if session == nil {
		return nil
	}
	if session.has_analysis {
		return &session.analysis.external_index
	}
	return &session.external.index
}

semantic_graph_candidate_resolved :: proc(
	session: ^Semantic_Graph_Session,
	index: ^External_Semantic_Index,
	candidate: Checker_Unresolved_Candidate,
) -> bool {
	if index == nil || !string_interner.is_valid(candidate.name) {
		return false
	}
	if candidate.kind == .Include_Source &&
	   semantic_graph_external_source_has_name(session, candidate.name) {
		return true
	}
	_, _, ok := external_semantic_index_lookup(
		index,
		candidate.namespace,
		candidate.name,
		candidate.kind,
	)
	return ok
}

semantic_graph_external_source_has_name :: proc(
	session: ^Semantic_Graph_Session,
	name: string_interner.String,
) -> bool {
	if session == nil || !string_interner.is_valid(name) {
		return false
	}
	for source in session.external.source_files {
		for provided in source.provided_names {
			if provided == name {
				return true
			}
		}
	}
	return false
}

semantic_graph_candidate_is_blocked :: proc(
	candidate: Checker_Unresolved_Candidate,
	blocked: []Semantic_Object_Key,
) -> bool {
	candidate_key := Semantic_Object_Key {
		kind = candidate.kind,
		name = candidate.name,
	}
	for key in blocked {
		if semantic_graph_object_keys_overlap(candidate_key, key) {
			return true
		}
	}
	return false
}

semantic_graph_candidate_list_add :: proc(
	list: ^[dynamic]Checker_Unresolved_Candidate,
	candidate: Checker_Unresolved_Candidate,
) {
	for existing in list^ {
		if existing.name == candidate.name &&
		   existing.kind == candidate.kind &&
		   existing.namespace == candidate.namespace {
			return
		}
	}
	append(list, candidate)
}

semantic_graph_collect_project_ids_for_provider :: proc(
	index: ^External_Semantic_Index,
	key: Semantic_Object_Key,
	out: ^[dynamic]Semantic_Project_Id,
) {
	keys := make([dynamic]Semantic_Object_Key, 0, 4, context.temp_allocator)
	semantic_graph_equivalent_provider_keys(key, &keys)
	for waiter_key in keys {
		if projects, ok := index.unresolved_waiters_by_object[waiter_key]; ok {
			for id in projects {
				semantic_graph_project_id_list_add(out, id)
			}
		}
		if projects, ok := index.dependents_by_object[waiter_key]; ok {
			for id in projects {
				semantic_graph_project_id_list_add(out, id)
			}
		}
	}
}

semantic_graph_clear_unresolved_waiters_for_provider :: proc(
	index: ^External_Semantic_Index,
	key: Semantic_Object_Key,
) {
	if index == nil {
		return
	}
	keys := make([dynamic]Semantic_Object_Key, 0, 4, context.temp_allocator)
	semantic_graph_equivalent_provider_keys(key, &keys)
	for waiter_key in keys {
		if projects, ok := index.unresolved_waiters_by_object[waiter_key]; ok {
			delete(projects)
			delete_key(&index.unresolved_waiters_by_object, waiter_key)
		}
	}
}

semantic_graph_equivalent_provider_keys :: proc(
	key: Semantic_Object_Key,
	out: ^[dynamic]Semantic_Object_Key,
) {
	if !semantic_object_key_is_valid(key) {
		return
	}
	semantic_graph_object_key_list_add(out, key)
	if key.kind != .Global_Symbol {
		semantic_graph_object_key_list_add(
			out,
			Semantic_Object_Key{kind = .Global_Symbol, name = key.name},
		)
	}
	if key.kind == .DDIC_Table {
		semantic_graph_object_key_list_add(
			out,
			Semantic_Object_Key{kind = .DDIC_Type, name = key.name},
		)
	} else if key.kind == .DDIC_Type {
		semantic_graph_object_key_list_add(
			out,
			Semantic_Object_Key{kind = .DDIC_Table, name = key.name},
		)
	}
}

semantic_graph_object_keys_overlap :: proc(a, b: Semantic_Object_Key) -> bool {
	if a.name != b.name {
		return false
	}
	if a.kind == b.kind || a.kind == .Global_Symbol || b.kind == .Global_Symbol {
		return true
	}
	return(
		(a.kind == .DDIC_Table && b.kind == .DDIC_Type) ||
		(a.kind == .DDIC_Type && b.kind == .DDIC_Table) \
	)
}

semantic_graph_session_external_input_for_key :: proc(
	session: ^Semantic_Graph_Session,
	key: Semantic_Object_Key,
) -> (
	External_Interface_Input,
	bool,
) {
	for input in session.external_inputs {
		if external_interface_input_key(input) == key {
			return input, true
		}
	}
	return {}, false
}

semantic_graph_session_record_for_project :: proc(
	session: ^Semantic_Graph_Session,
	project: ^Project,
) -> (
	^Semantic_Project_Record,
	bool,
) {
	if !session.has_analysis {
		return nil, false
	}
	for &record in session.analysis.external_index.projects {
		if record.project == project {
			return &record, true
		}
	}
	return nil, false
}

semantic_graph_project_ref_from_record :: proc(
	session: ^Semantic_Graph_Session,
	record: ^Semantic_Project_Record,
) -> Semantic_Graph_Project_Ref {
	assert(record.role == .Editable_Root || record.role == .Include_Fragment)
	assert(session.has_analysis)
	root_path := ""
	for result in session.analysis.project_results {
		if result.record_id == record.id || result.project == record.project {
			root_path = result.root_path
			break
		}
	}
	assert(root_path != "")
	return Semantic_Graph_Project_Ref {
		id = record.id,
		role = record.role,
		root_key = record.root_key,
		root_path = root_path,
	}
}

semantic_graph_session_add_all_current_editable_projects :: proc(
	session: ^Semantic_Graph_Session,
	out: ^[dynamic]Semantic_Graph_Project_Ref,
) {
	if !session.has_analysis {
		return
	}
	for &record in session.analysis.external_index.projects {
		if record.role == .Editable_Root || record.role == .Include_Fragment {
			ref := semantic_graph_project_ref_from_record(session, &record)
			semantic_graph_project_ref_list_add(out, ref, out.allocator)
		}
	}
}

semantic_graph_session_add_current_refs_for_pending :: proc(
	session: ^Semantic_Graph_Session,
	pending: []Semantic_Graph_Project_Ref,
	out: ^[dynamic]Semantic_Graph_Project_Ref,
) {
	for ref in pending {
		if current, ok := semantic_graph_session_current_ref_for_ref(session, ref); ok {
			semantic_graph_project_ref_list_add(out, current, out.allocator)
		} else {
			semantic_graph_project_ref_list_add(out, ref, out.allocator)
		}
	}
}

semantic_graph_session_current_ref_for_ref :: proc(
	session: ^Semantic_Graph_Session,
	ref: Semantic_Graph_Project_Ref,
) -> (
	Semantic_Graph_Project_Ref,
	bool,
) {
	if !session.has_analysis {
		return {}, false
	}
	for &record in session.analysis.external_index.projects {
		if !(record.role == .Editable_Root || record.role == .Include_Fragment) {
			continue
		}
		current := semantic_graph_project_ref_from_record(session, &record)
		if semantic_graph_project_ref_same(current, ref) {
			return current, true
		}
	}
	return {}, false
}

semantic_graph_project_ref_list_add :: proc(
	list: ^[dynamic]Semantic_Graph_Project_Ref,
	ref: Semantic_Graph_Project_Ref,
	allocator: mem.Allocator,
) {
	assert(ref.root_path != "")
	for existing in list^ {
		if semantic_graph_project_ref_same(existing, ref) {
			return
		}
	}
	next := ref
	next.root_path = strings.clone(ref.root_path, allocator)
	append(list, next)
}

semantic_graph_project_ref_list_append_clone :: proc(
	list: ^[dynamic]Semantic_Graph_Project_Ref,
	refs: []Semantic_Graph_Project_Ref,
	allocator: mem.Allocator,
) {
	for ref in refs {
		semantic_graph_project_ref_list_add(list, ref, allocator)
	}
}

semantic_graph_project_ref_list_clone :: proc(
	refs: []Semantic_Graph_Project_Ref,
	allocator: mem.Allocator,
) -> [dynamic]Semantic_Graph_Project_Ref {
	out := make([dynamic]Semantic_Graph_Project_Ref, 0, len(refs), allocator)
	semantic_graph_project_ref_list_append_clone(&out, refs, allocator)
	return out
}

semantic_graph_project_ref_list_destroy :: proc(
	list: ^[dynamic]Semantic_Graph_Project_Ref,
	allocator: mem.Allocator,
) {
	if list == nil || list.allocator.procedure == nil {
		return
	}
	for &ref in list^ {
		delete(ref.root_path, allocator)
	}
	delete(list^)
	list^ = nil
}

semantic_graph_project_ref_same :: proc(a, b: Semantic_Graph_Project_Ref) -> bool {
	assert(a.root_path != "" && b.root_path != "")
	return a.role == b.role && a.root_path == b.root_path
}

semantic_graph_object_key_list_add :: proc(
	list: ^[dynamic]Semantic_Object_Key,
	key: Semantic_Object_Key,
) {
	if !semantic_object_key_is_valid(key) {
		return
	}
	for existing in list^ {
		if existing == key {
			return
		}
	}
	append(list, key)
}

semantic_graph_project_id_list_add :: proc(
	list: ^[dynamic]Semantic_Project_Id,
	id: Semantic_Project_Id,
) {
	if !semantic_project_id_is_valid(id) {
		return
	}
	for existing in list^ {
		if existing == id {
			return
		}
	}
	append(list, id)
}

semantic_graph_workspace_file_input_clone :: proc(
	input: Workspace_File_Input,
	allocator: mem.Allocator,
) -> Workspace_File_Input {
	assert(input.path != "")
	return Workspace_File_Input {
		path = strings.clone(input.path, allocator),
		root = input.root,
		kind = input.kind,
		object_name = strings.clone(input.object_name, allocator) if input.object_name != "" else "",
	}
}

semantic_graph_workspace_file_input_destroy :: proc(
	input: ^Workspace_File_Input,
	allocator: mem.Allocator,
) {
	delete(input.path, allocator)
	if input.object_name != "" {
		delete(input.object_name, allocator)
	}
	input^ = {}
}

semantic_graph_workspace_file_input_equal :: proc(a, b: Workspace_File_Input) -> bool {
	return(
		a.path == b.path &&
		a.root == b.root &&
		a.kind == b.kind &&
		a.object_name == b.object_name \
	)
}

semantic_graph_external_input_clone :: proc(
	input: External_Interface_Input,
	allocator: mem.Allocator,
) -> External_Interface_Input {
	assert(input.path != "")
	return External_Interface_Input {
		key = input.key,
		path = strings.clone(input.path, allocator),
		root = input.root,
		source_hash = input.source_hash,
		generation = input.generation,
		role = input.role,
	}
}

semantic_graph_external_input_destroy :: proc(
	input: ^External_Interface_Input,
	allocator: mem.Allocator,
) {
	delete(input.path, allocator)
	input^ = {}
}

semantic_graph_external_input_equal :: proc(a, b: External_Interface_Input) -> bool {
	return(
		external_interface_input_key(a) == external_interface_input_key(b) &&
		a.path == b.path &&
		a.root == b.root &&
		a.source_hash == b.source_hash &&
		a.generation == b.generation &&
		a.role == b.role \
	)
}

semantic_graph_external_source_input_clone :: proc(
	input: External_Source_Input,
	allocator: mem.Allocator,
) -> External_Source_Input {
	assert(input.path != "")
	provided_names := make([]string, len(input.provided_names), allocator)
	for name, i in input.provided_names {
		provided_names[i] = strings.clone(name, allocator)
	}
	return External_Source_Input {
		path           = strings.clone(input.path, allocator),
		root           = input.root,
		provided_names = provided_names,
		source_hash    = input.source_hash,
		generation     = input.generation,
	}
}

semantic_graph_external_source_input_destroy :: proc(
	input: ^External_Source_Input,
	allocator: mem.Allocator,
) {
	delete(input.path, allocator)
	for name in input.provided_names {
		delete(name, allocator)
	}
	if input.provided_names != nil {
		delete(input.provided_names, allocator)
	}
	input^ = {}
}

semantic_graph_external_source_input_equal :: proc(a, b: External_Source_Input) -> bool {
	if a.path != b.path ||
	   a.root != b.root ||
	   a.source_hash != b.source_hash ||
	   a.generation != b.generation ||
	   len(a.provided_names) != len(b.provided_names) {
		return false
	}
	for name, i in a.provided_names {
		if name != b.provided_names[i] {
			return false
		}
	}
	return true
}
