package abap_frontend_semantic

import dep_store "../dependency_store"
import "../adt"
import frontend_runtime "../runtime"

import base_runtime "base:runtime"
import "core:fmt"
import "core:mem"
import "core:mem/virtual"
import "core:os"
import "core:strings"
import "core:time"

trace_eprintf :: fmt.eprintf

Remote_Dependency_Candidate :: struct {
	name: string,
	kind: string,
}

analyze_with_manifest_dependency_drain :: proc(
	manifest: ^Workspace_Manifest,
	target: Source_Input,
	candidates: [dynamic]Project_Candidate_Input,
	dependencies: [dynamic]Source_Input,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> Project_Analysis {
	candidate_inputs := candidates
	dependency_inputs := dependencies
	project := analyze_target_with_candidate_inputs(target, candidate_inputs[:], dependency_inputs[:], options, allocator)
	store, has_store := manifest_dependency_store(manifest, options, allocator)
	roots := manifest_local_export_roots(manifest, allocator)
	has_adt := manifest_has_project_dotenv(manifest, allocator)
	if !has_store && len(roots) == 0 && !has_adt {
		return project
	}

	seen_artifacts := make(map[i64]bool, 16, allocator)
	for {
		remote_candidates := collect_project_remote_dependency_candidates(&project, allocator)
		added := false
		if has_store {
			added = add_dependency_store_matches(
				&candidate_inputs,
				&dependency_inputs,
				remote_candidates[:],
				&store,
				&manifest.dependency_store,
				&seen_artifacts,
				options.pool,
				target.uri,
				allocator,
			)
		}
		if !added && len(roots) > 0 {
			added = add_local_export_matches(
				&candidate_inputs,
				&dependency_inputs,
				remote_candidates[:],
				roots[:],
				target.uri,
				allocator,
			)
		}
		if !added && has_adt {
			cache_store: ^dep_store.Dependency_Store
			cache_profile: ^dep_store.Dependency_Profile
			if has_store {
				cache_store = &store
				cache_profile = &manifest.dependency_store
			}
			added = add_adt_matches(
				&candidate_inputs,
				&dependency_inputs,
				remote_candidates[:],
				cache_store,
				cache_profile,
				manifest,
				options.pool,
				target.uri,
				allocator,
			)
		}
		if !added {
			break
		}
		project = analyze_target_with_candidate_inputs(target, candidate_inputs[:], dependency_inputs[:], options, allocator)
	}
	return project
}

analyze_standalone_with_dependency_drain :: proc(
	target: Source_Input,
	candidates: [dynamic]Project_Candidate_Input,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> Project_Analysis {
	candidate_inputs := candidates
	dependency_inputs := make([dynamic]Source_Input, 0, 4, allocator)
	store, err := dep_store.dependency_store_from_override_path(options.dependency_store_path, allocator)
	when adt.DEPENDENCY_FETCH_TRACE {
		status := "disabled"
		if options.enable_standalone_adt {
			status = "enabled"
		}
		trace_eprintf("adt_fetch\tstandalone\toption\t%s\n", status)
		if err == .None {
			trace_eprintf("adt_fetch\tstandalone\tstore\tok\n")
		} else {
			trace_eprintf("adt_fetch\tstandalone\tstore_err\t%v\n", err)
		}
	}
	if err != .None && !options.enable_standalone_adt {
		return analyze_target_with_candidate_inputs(target, candidate_inputs[:], dependency_inputs[:], options, allocator)
	}
	cache_store: ^dep_store.Dependency_Store
	cache_profile: ^dep_store.Dependency_Profile
	standalone_profile := standalone_dependency_profile()
	if err == .None {
		cache_store = &store
		cache_profile = &standalone_profile
	}

	standalone_client: adt.Client
	standalone_config: adt.Connection_Config
	has_standalone_adt := false
	if options.enable_standalone_adt {
		dotenv, dotenv_err := adt.load_dotenv_defaults("", allocator)
		if dotenv_err == .None {
			when adt.DEPENDENCY_FETCH_TRACE {
				trace_eprintf("adt_fetch\tstandalone\tdotenv\tok\t%d\n", len(dotenv.values))
			}
			overrides := adt.Connection_Overrides{}
			config, config_err := adt.connection_config_from_sources(&overrides, &dotenv, allocator)
			adt.dotenv_defaults_destroy(&dotenv, allocator)
			if config_err == .None {
				standalone_config = config
				adt.client_init(&standalone_client, standalone_config)
				has_standalone_adt = true
				when adt.DEPENDENCY_FETCH_TRACE {
					trace_eprintf("adt_fetch\tstandalone\tconfig\tok\n")
				}
			} else {
				when adt.DEPENDENCY_FETCH_TRACE {
					trace_eprintf("adt_fetch\tstandalone\tconfig_err\t%v\n", config_err)
				}
			}
		} else {
			when adt.DEPENDENCY_FETCH_TRACE {
				trace_eprintf("adt_fetch\tstandalone\tdotenv_err\t%v\n", dotenv_err)
			}
		}
	}

	seen_artifacts := make(map[i64]bool, 16, allocator)
	project_arena: virtual.Arena
	unit_arenas: []virtual.Arena
	unit_allocators: []mem.Allocator
	project_allocator := scratch_analysis_init(
		&project_arena,
		&unit_arenas,
		&unit_allocators,
		1 + len(dependency_inputs) + len(candidate_inputs),
		allocator,
	)
	project := analyze_target_with_candidate_inputs_allocators(
		target,
		candidate_inputs[:],
		dependency_inputs[:],
		options,
		unit_allocators,
		project_allocator,
	)
	iteration := 0
	for {
		iteration += 1
		remote_candidates := collect_project_remote_dependency_candidates(&project, project_allocator)
		when adt.DEPENDENCY_FETCH_TRACE {
			trace_eprintf("adt_fetch\tdrain\titeration\t%d\tcandidates\t%d\n", iteration, len(remote_candidates))
		}
		added := false
		// Any-profile standalone cache lookup has no product/package boundary.
		if err == .None {
			added = add_dependency_store_any_profile_matches(
				&candidate_inputs,
				&dependency_inputs,
				remote_candidates[:],
				&store,
				&seen_artifacts,
				options.pool,
				target.uri,
				allocator,
			)
		}
		if !added && has_standalone_adt {
			added = add_adt_matches_with_client(
				&candidate_inputs,
				&dependency_inputs,
				remote_candidates[:],
				cache_store,
				cache_profile,
				&standalone_client,
				options.pool,
				target.uri,
				allocator,
			)
		}
		if !added {
			break
		}
		scratch_analysis_destroy(&project_arena, unit_arenas, unit_allocators, allocator)
		project_allocator = scratch_analysis_init(
			&project_arena,
			&unit_arenas,
			&unit_allocators,
			1 + len(dependency_inputs) + len(candidate_inputs),
			allocator,
		)
		project = analyze_target_with_candidate_inputs_allocators(
			target,
			candidate_inputs[:],
			dependency_inputs[:],
			options,
			unit_allocators,
			project_allocator,
		)
	}
	if has_standalone_adt {
		adt.client_destroy(&standalone_client, allocator)
		adt.connection_config_destroy(&standalone_config, allocator)
	}
	scratch_analysis_destroy(&project_arena, unit_arenas, unit_allocators, allocator)
	return analyze_target_with_candidate_inputs(target, candidate_inputs[:], dependency_inputs[:], options, allocator)
}

scratch_analysis_init :: proc(
	project_arena: ^virtual.Arena,
	unit_arenas: ^[]virtual.Arena,
	unit_allocators: ^[]mem.Allocator,
	unit_count: int,
	allocator: mem.Allocator,
) -> mem.Allocator {
	assert(virtual.arena_init_growing(project_arena) == .None)
	unit_arenas^ = make([]virtual.Arena, unit_count, allocator)
	unit_allocators^ = make([]mem.Allocator, unit_count, allocator)
	for i in 0 ..< unit_count {
		assert(virtual.arena_init_growing(&unit_arenas^[i]) == .None)
		unit_allocators^[i] = virtual.arena_allocator(&unit_arenas^[i])
	}
	return virtual.arena_allocator(project_arena)
}

scratch_analysis_destroy :: proc(
	project_arena: ^virtual.Arena,
	unit_arenas: []virtual.Arena,
	unit_allocators: []mem.Allocator,
	allocator: mem.Allocator,
) {
	for i in 0 ..< len(unit_arenas) {
		virtual.arena_destroy(&unit_arenas[i])
	}
	delete(unit_allocators, allocator)
	delete(unit_arenas, allocator)
	virtual.arena_destroy(project_arena)
}

manifest_dependency_store :: proc(
	manifest: ^Workspace_Manifest,
	options: Analyze_Options,
	allocator: mem.Allocator,
) -> (dep_store.Dependency_Store, bool) {
	if !manifest.has_dependency_store {
		return {}, false
	}
	store, err := dep_store.dependency_store_from_override_path(options.dependency_store_path, allocator)
	return store, err == .None
}

manifest_local_export_roots :: proc(
	manifest: ^Workspace_Manifest,
	allocator: mem.Allocator,
) -> [dynamic]string {
	roots := make([dynamic]string, 0, len(manifest.local_export_roots), allocator)
	if strings.to_lower(strings.trim_space(manifest.dependency_source), allocator) == "adt-first" {
		return roots
	}
	for root in manifest.local_export_roots {
		path, ok := manifest_absolute_path(manifest.root_path, root, allocator)
		if ok {
			append(&roots, path)
		}
	}
	return roots
}

collect_project_remote_dependency_candidates :: proc(
	project: ^Project_Analysis,
	allocator: mem.Allocator,
) -> [dynamic]Remote_Dependency_Candidate {
	out := make([dynamic]Remote_Dependency_Candidate, 0, 8, allocator)
	index := make(map[string]int, 64, allocator)
	defer delete(index)
	for &unit in project.units {
		for &edge in unit.include_edges {
			if !edge.has_target && is_remote_lookup_candidate(edge.name, "include") {
				insert_remote_candidate(&out, &index, edge.name, "include", allocator)
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
			if symbol.decl_range.start == symbol.decl_range.end && symbol.has_declared_type &&
			   symbol.declared_type.namespace == .Type &&
			   is_remote_lookup_candidate_after_local_resolution(symbol.declared_type.base_name, "type") {
				insert_remote_candidate(&out, &index, symbol.declared_type.base_name, "type", allocator)
			}
		}
		if unit.has_message_default_class {
			insert_message_class_candidate(&out, &index, unit.message_default_class.name, allocator)
		}
		for &message in unit.message_uses {
			if message.class_name != "" {
				insert_message_class_candidate(&out, &index, message.class_name, allocator)
			}
		}
		for &sql_source in unit.sql_sources {
			if sql_source.resolution == .External && is_remote_lookup_candidate(sql_source.name, "type") {
				insert_remote_candidate(&out, &index, sql_source.name, "type", allocator)
			}
		}
		for &call_site in unit.call_sites {
			#partial switch call_site.target.kind {
			case .Function:
				if is_remote_lookup_candidate_after_local_resolution(call_site.target.function_name, "function") {
					insert_remote_candidate(&out, &index, call_site.target.function_name, "function", allocator)
				}
			case .Report:
				if is_remote_lookup_candidate_after_local_resolution(call_site.target.report_name, "report") {
					insert_remote_candidate(&out, &index, call_site.target.report_name, "report", allocator)
				}
			}
		}
	}
	return out
}

remote_dependency_candidate_for_reference :: proc(ref: ^Reference_Data) -> (
	Remote_Dependency_Candidate,
	bool,
) {
	kind := ""
	after_local := false
	switch ref.kind {
	case .Include, .Structured_Decl_End:
		return {}, false
	case .Static_Target:
		kind = "static"
		after_local = true
	case .Type_Ref:
		kind = "type"
		after_local = true
	case .Message_Class:
		kind = "message-class"
	case .Routine_Call:
		if ref.namespace == .Routine {
			kind = "function"
			after_local = true
		} else {
			kind = "symbol"
		}
	case .Identifier:
		kind = "symbol"
	}
	if after_local {
		if !is_remote_lookup_candidate_after_local_resolution(ref.name, kind) {
			return {}, false
		}
	} else if !is_remote_lookup_candidate(ref.name, kind) {
		return {}, false
	}
	return Remote_Dependency_Candidate{name = ref.name, kind = kind}, true
}

insert_message_class_candidate :: proc(
	out: ^[dynamic]Remote_Dependency_Candidate,
	index: ^map[string]int,
	name: string,
	allocator: mem.Allocator,
) {
	if is_remote_lookup_candidate(name, "message-class") {
		insert_remote_candidate(out, index, name, "message-class", allocator)
	}
}

insert_remote_candidate :: proc(
	out: ^[dynamic]Remote_Dependency_Candidate,
	index: ^map[string]int,
	name, kind: string,
	allocator: mem.Allocator,
) {
	normalized_name := canonical_name(strings.trim_space(name), allocator)
	if normalized_name == "" {
		return
	}
	normalized_kind := canonical_name(strings.trim_space(kind), allocator)
	if existing_index, ok := index^[normalized_name]; ok {
		if remote_candidate_kind_priority(normalized_kind) > remote_candidate_kind_priority(out^[existing_index].kind) {
			out^[existing_index].kind = normalized_kind
		}
		return
	}
	index^[normalized_name] = len(out^)
	append(out, Remote_Dependency_Candidate{name = normalized_name, kind = normalized_kind})
}

remote_candidate_kind_priority :: proc(kind: string) -> int {
	if kind == "message-class" {return 5}
	if kind == "include" || kind == "function" {return 4}
	if kind == "static" {return 3}
	if kind == "type" {return 2}
	return 1
}

add_dependency_store_matches :: proc(
	candidates: ^[dynamic]Project_Candidate_Input,
	dependencies: ^[dynamic]Source_Input,
	remote_candidates: []Remote_Dependency_Candidate,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	seen_artifacts: ^map[i64]bool,
	pool: ^frontend_runtime.Pool,
	target_uri: string,
	allocator: mem.Allocator,
) -> bool {
	return add_dependency_store_matches_impl(
		candidates,
		dependencies,
		remote_candidates,
		store,
		profile,
		false,
		seen_artifacts,
		pool,
		target_uri,
		"store",
		allocator,
	)
}

add_dependency_store_any_profile_matches :: proc(
	candidates: ^[dynamic]Project_Candidate_Input,
	dependencies: ^[dynamic]Source_Input,
	remote_candidates: []Remote_Dependency_Candidate,
	store: ^dep_store.Dependency_Store,
	seen_artifacts: ^map[i64]bool,
	pool: ^frontend_runtime.Pool,
	target_uri: string,
	allocator: mem.Allocator,
) -> bool {
	return add_dependency_store_matches_impl(
		candidates,
		dependencies,
		remote_candidates,
		store,
		nil,
		true,
		seen_artifacts,
		pool,
		target_uri,
		"store_any",
		allocator,
	)
}

Dependency_Store_Task_Result :: struct {
	record: dep_store.Stored_Artifact_Record,
	ok:     bool,
	err:    dep_store.Store_Error,
}

Dependency_Store_Task_Payload :: struct {
	store:       dep_store.Dependency_Store,
	profile:     ^dep_store.Dependency_Profile,
	candidate:   Remote_Dependency_Candidate,
	any_profile: bool,
}

add_dependency_store_matches_impl :: proc(
	candidates: ^[dynamic]Project_Candidate_Input,
	dependencies: ^[dynamic]Source_Input,
	remote_candidates: []Remote_Dependency_Candidate,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	any_profile: bool,
	seen_artifacts: ^map[i64]bool,
	pool: ^frontend_runtime.Pool,
	target_uri: string,
	trace_source: string,
	allocator: mem.Allocator,
) -> bool {
	assert(pool != nil)
	uri_key_arena: mem.Dynamic_Arena
	mem.dynamic_arena_init(&uri_key_arena, allocator, allocator, alignment = 64)
	defer mem.dynamic_arena_destroy(&uri_key_arena)
	uri_key_allocator := mem.dynamic_arena_allocator(&uri_key_arena)
	uri_keys := project_input_uri_keys(target_uri, dependencies^[:], candidates^[:], len(remote_candidates), uri_key_allocator)

	added := false
	batch_size := pool.options.task_capacity
	for start := 0; start < len(remote_candidates); {
		end := start + batch_size
		if end > len(remote_candidates) {
			end = len(remote_candidates)
		}
		tasks := make([dynamic]frontend_runtime.Task(^Dependency_Store_Task_Result), 0, end - start, allocator)
		for candidate in remote_candidates[start:end] {
			payload := Dependency_Store_Task_Payload {
				store = store^,
				profile = profile,
				candidate = candidate,
				any_profile = any_profile,
			}
			task, err := frontend_runtime.submit_value(pool, payload, dependency_store_find_task)
			assert(err == .None)
			append(&tasks, task)
		}
		for task, i in tasks {
			result, _ := frontend_runtime.wait(task)
			if add_dependency_store_task_result(
				candidates,
				dependencies,
				remote_candidates[start + i],
				result,
				seen_artifacts,
				&uri_keys,
				uri_key_allocator,
				trace_source,
				allocator,
			) {
				added = true
			}
			dependency_store_task_result_destroy(result, base_runtime.heap_allocator())
		}
		delete(tasks)
		start = end
	}
	return added
}

dependency_store_find_task :: proc(payload: Dependency_Store_Task_Payload) -> ^Dependency_Store_Task_Result {
	allocator := base_runtime.heap_allocator()
	result := new(Dependency_Store_Task_Result, allocator)
	store := payload.store
	reader, reader_err := dep_store.reader(&store, allocator)
	if reader_err != .None {
		result.err = reader_err
		return result
	}
	defer dep_store.reader_destroy(&reader)
	if payload.any_profile {
		result.record, result.ok, result.err = dep_store.reader_find_artifact_for_candidate_any_profile(
			&reader,
			payload.candidate.name,
			payload.candidate.kind,
			allocator,
		)
	} else if payload.profile != nil {
		result.record, result.ok, result.err = dep_store.reader_find_artifact_for_candidate(
			&reader,
			payload.profile,
			payload.candidate.name,
			payload.candidate.kind,
			allocator,
		)
	}
	return result
}

add_dependency_store_task_result :: proc(
	candidates: ^[dynamic]Project_Candidate_Input,
	dependencies: ^[dynamic]Source_Input,
	candidate: Remote_Dependency_Candidate,
	result: ^Dependency_Store_Task_Result,
	seen_artifacts: ^map[i64]bool,
	uri_keys: ^map[string]bool,
	uri_key_allocator: mem.Allocator,
	trace_source: string,
	allocator: mem.Allocator,
) -> bool {
	if result == nil || result.err != .None || !result.ok || result.record.artifact_id in seen_artifacts^ {
		return false
	}
	seen_artifacts^[result.record.artifact_id] = true
	uri := dependency_record_uri(&result.record, allocator)
	if !project_input_uri_key_add_if_missing(uri_keys, uri, uri_key_allocator) {
		delete(uri, allocator)
		return false
	}
	input := source_input_from_dependency_record(&result.record, candidate, uri, allocator)
	append_dependency_input(candidates, dependencies, input, candidate, result.record.object_name, allocator)
	when adt.DEPENDENCY_FETCH_TRACE {
		trace_eprintf("adt_fetch\t%s\tadd\t%s\t%s\t%s\t%s\n", trace_source, candidate.kind, candidate.name, result.record.object_kind, result.record.object_name)
	}
	return true
}

dependency_store_task_result_destroy :: proc(result: ^Dependency_Store_Task_Result, allocator: mem.Allocator) {
	if result == nil {
		return
	}
	dependency_record_destroy(&result.record, allocator)
	free(result, allocator)
}

dependency_record_destroy :: proc(record: ^dep_store.Stored_Artifact_Record, allocator: mem.Allocator) {
	delete(record.package_name, allocator)
	delete(record.package_version, allocator)
	delete(record.object_kind, allocator)
	delete(record.object_name, allocator)
	delete(record.object_uri, allocator)
	delete(record.object_type, allocator)
	delete(record.description, allocator)
	delete(record.file_extension, allocator)
	delete(record.source_text, allocator)
}

source_input_from_dependency_record :: proc(
	record: ^dep_store.Stored_Artifact_Record,
	candidate: Remote_Dependency_Candidate,
	uri: string,
	allocator: mem.Allocator,
) -> Source_Input {
	if source_looks_xml(record.source_text) {
		return Source_Input {
			uri    = uri,
			source = synthetic_dependency_source(record.object_name, candidate.kind, allocator),
		}
	}
	return Source_Input {
		uri    = uri,
		source = strings.clone(record.source_text, allocator),
	}
}

dependency_record_uri :: proc(record: ^dep_store.Stored_Artifact_Record, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "abapls-cache:/")
	strings.write_string(&out, record.object_kind)
	strings.write_byte(&out, '/')
	strings.write_string(&out, record.object_name)
	strings.write_string(&out, ".abap")
	return strings.to_string(out)
}

add_local_export_matches :: proc(
	candidates: ^[dynamic]Project_Candidate_Input,
	dependencies: ^[dynamic]Source_Input,
	remote_candidates: []Remote_Dependency_Candidate,
	roots: []string,
	target_uri: string,
	allocator: mem.Allocator,
) -> bool {
	uri_key_arena: mem.Dynamic_Arena
	mem.dynamic_arena_init(&uri_key_arena, allocator, allocator, alignment = 64)
	defer mem.dynamic_arena_destroy(&uri_key_arena)
	uri_key_allocator := mem.dynamic_arena_allocator(&uri_key_arena)
	uri_keys := project_input_uri_keys(target_uri, dependencies^[:], candidates^[:], len(remote_candidates), uri_key_allocator)

	added := false
	for candidate in remote_candidates {
		file_names := local_export_candidate_file_names(candidate, allocator)
		if len(file_names) == 0 {
			continue
		}
		paths := make([dynamic]string, 0, 2, allocator)
		for root in roots {
			collect_local_export_candidate_paths(root, file_names[:], &paths, allocator)
		}
		for path in paths {
			if project_input_uri_key_exists(&uri_keys, path, uri_key_allocator) {
				continue
			}
			source, ok := read_text_file(path, allocator)
			if !ok {
				continue
			}
			if source_looks_xml(source) {
				source = synthetic_dependency_source(candidate.name, candidate.kind, allocator)
			} else if !local_export_abap_source_matches(candidate, source) {
				continue
			}
			if !project_input_uri_key_add_if_missing(&uri_keys, path, uri_key_allocator) {
				continue
			}
			append_dependency_input(
				candidates,
				dependencies,
				Source_Input{uri = path, source = source},
				candidate,
				candidate.name,
				allocator,
			)
			added = true
		}
	}
	return added
}

add_adt_matches :: proc(
	candidates: ^[dynamic]Project_Candidate_Input,
	dependencies: ^[dynamic]Source_Input,
	remote_candidates: []Remote_Dependency_Candidate,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	manifest: ^Workspace_Manifest,
	pool: ^frontend_runtime.Pool,
	target_uri: string,
	allocator: mem.Allocator,
) -> bool {
	env_path, ok := manifest_project_dotenv_path(manifest, allocator)
	if !ok {
		return false
	}
	defer delete(env_path, allocator)
	dotenv, dotenv_err := adt.parse_dotenv_file(env_path, allocator)
	if dotenv_err != .None {
		return false
	}
	defer adt.dotenv_defaults_destroy(&dotenv, allocator)

	overrides := adt.Connection_Overrides{}
	config, config_err := adt.connection_config_from_sources(&overrides, &dotenv, allocator)
	if config_err != .None {
		return false
	}
	defer adt.connection_config_destroy(&config, allocator)

	client: adt.Client
	adt.client_init(&client, config)
	defer adt.client_destroy(&client, allocator)
	return add_adt_matches_with_client(candidates, dependencies, remote_candidates, store, profile, &client, pool, target_uri, allocator)
}

Adt_Fetched_Object :: struct {
	object_ref: adt.Object_Ref,
	fetched:    adt.Dependency_Fetch_Result,
}

Adt_Fetch_Task_Result :: struct {
	fetched: [dynamic]Adt_Fetched_Object,
}

Adt_Fetch_Task_Payload :: struct {
	candidate: Remote_Dependency_Candidate,
	config:    ^adt.Connection_Config,
}

add_adt_matches_with_client :: proc(
	candidates: ^[dynamic]Project_Candidate_Input,
	dependencies: ^[dynamic]Source_Input,
	remote_candidates: []Remote_Dependency_Candidate,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	client: ^adt.Client,
	pool: ^frontend_runtime.Pool,
	target_uri: string,
	allocator: mem.Allocator,
) -> bool {
	uri_key_arena: mem.Dynamic_Arena
	mem.dynamic_arena_init(&uri_key_arena, allocator, allocator, alignment = 64)
	defer mem.dynamic_arena_destroy(&uri_key_arena)
	uri_key_allocator := mem.dynamic_arena_allocator(&uri_key_arena)
	uri_keys := project_input_uri_keys(target_uri, dependencies^[:], candidates^[:], len(remote_candidates), uri_key_allocator)

	added := false
	batch_size := pool.options.task_capacity
	for start := 0; start < len(remote_candidates); {
		end := start + batch_size
		if end > len(remote_candidates) {
			end = len(remote_candidates)
		}
		tasks := make([dynamic]frontend_runtime.Task(^Adt_Fetch_Task_Result), 0, end - start, allocator)
		for candidate in remote_candidates[start:end] {
			payload := Adt_Fetch_Task_Payload{candidate = candidate, config = &client.connection}
			task, err := frontend_runtime.submit_value(pool, payload, adt_fetch_task)
			assert(err == .None)
			append(&tasks, task)
		}
		for task, i in tasks {
			result, _ := frontend_runtime.wait(task)
			if add_adt_fetch_task_result(
				candidates,
				dependencies,
				remote_candidates[start + i],
				result,
				store,
				profile,
				&uri_keys,
				uri_key_allocator,
				allocator,
			) {
				added = true
			}
			adt_fetch_task_result_destroy(result, base_runtime.heap_allocator())
		}
		delete(tasks)
		start = end
	}
	return added
}

adt_fetch_task :: proc(payload: Adt_Fetch_Task_Payload) -> ^Adt_Fetch_Task_Result {
	allocator := base_runtime.heap_allocator()
	result := new(Adt_Fetch_Task_Result, allocator)
	result.fetched = make([dynamic]Adt_Fetched_Object, allocator)
	client: adt.Client
	adt.client_init(&client, payload.config^)
	defer adt.client_destroy(&client, allocator)

	when adt.DEPENDENCY_FETCH_TRACE {
		trace_eprintf("adt_fetch\tadt\tsearch\t%s\t%s\n", payload.candidate.kind, payload.candidate.name)
	}
	objects, err := adt.search_repository_objects(&client, payload.candidate.name, 50, allocator)
	if err != .None {
		when adt.DEPENDENCY_FETCH_TRACE {
			trace_eprintf("adt_fetch\tadt\tsearch_err\t%s\t%s\t%v\n", payload.candidate.kind, payload.candidate.name, err)
		}
		adt.object_refs_destroy(&objects, allocator)
		objects = adt.direct_dependency_object_refs(payload.candidate.name, payload.candidate.kind, allocator)
	} else {
		when adt.DEPENDENCY_FETCH_TRACE {
			trace_eprintf("adt_fetch\tadt\tsearch_ok\t%s\t%s\t%d\n", payload.candidate.kind, payload.candidate.name, len(objects))
		}
	}
	defer adt.object_refs_destroy(&objects, allocator)

	selected := adt.select_dependency_objects(payload.candidate.name, objects[:], payload.candidate.kind, allocator)
	if len(selected) == 0 {
		adt.object_refs_destroy(&selected, allocator)
		selected = adt.direct_dependency_object_refs(payload.candidate.name, payload.candidate.kind, allocator)
	}
	defer adt.object_refs_destroy(&selected, allocator)
	when adt.DEPENDENCY_FETCH_TRACE {
		trace_eprintf("adt_fetch\tadt\tselected\t%s\t%s\t%d\n", payload.candidate.kind, payload.candidate.name, len(selected))
	}

	for &object_ref in selected {
		when adt.DEPENDENCY_FETCH_TRACE {
			trace_eprintf("adt_fetch\tadt\tfetch\t%s\t%s\t%s\t%s\n", payload.candidate.kind, payload.candidate.name, object_ref.object_type, object_ref.name)
		}
		fetched, fetch_err := adt.fetch_dependency_object(&client, &object_ref, allocator)
		if fetch_err != .None {
			when adt.DEPENDENCY_FETCH_TRACE {
				trace_eprintf("adt_fetch\tadt\tfetch_err\t%s\t%s\t%s\t%s\t%v\n", payload.candidate.kind, payload.candidate.name, object_ref.object_type, object_ref.name, fetch_err)
			}
			continue
		}
		when adt.DEPENDENCY_FETCH_TRACE {
			trace_eprintf("adt_fetch\tadt\tfetch_ok\t%s\t%s\t%s\t%s\t%s\t%d\n", payload.candidate.kind, payload.candidate.name, object_ref.object_type, object_ref.name, fetched.manifest_kind, len(fetched.shared_dependencies))
			adt.trace_dependency_fetch(&object_ref, fetched.manifest_kind, fetched.file_extension)
		}
		append(&result.fetched, Adt_Fetched_Object{object_ref = adt.clone_object_ref(&object_ref, allocator), fetched = fetched})
	}
	return result
}

add_adt_fetch_task_result :: proc(
	candidates: ^[dynamic]Project_Candidate_Input,
	dependencies: ^[dynamic]Source_Input,
	candidate: Remote_Dependency_Candidate,
	result: ^Adt_Fetch_Task_Result,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	uri_keys: ^map[string]bool,
	uri_key_allocator: mem.Allocator,
	allocator: mem.Allocator,
) -> bool {
	if result == nil {
		return false
	}
	added := false
	for &entry in result.fetched {
		store_adt_dependency_fetch(store, profile, &entry.object_ref, &entry.fetched, allocator)
		input_added := add_adt_fetched_dependency_input(
			candidates,
			dependencies,
			candidate,
			&entry.object_ref,
			entry.fetched.body,
			entry.fetched.file_extension,
			uri_keys,
			uri_key_allocator,
			allocator,
		)
		when adt.DEPENDENCY_FETCH_TRACE {
			status := "skipped"
			if input_added {
				status = "added"
			}
			trace_eprintf("adt_fetch\tadt\tinput\t%s\t%s\t%s\t%s\n", status, candidate.kind, entry.object_ref.object_type, entry.object_ref.name)
		}
		if input_added {
			added = true
		}
		for &shared in entry.fetched.shared_dependencies {
			shared_candidate := Remote_Dependency_Candidate{name = shared.object_ref.name, kind = "include"}
			if add_adt_fetched_dependency_input(
				candidates,
				dependencies,
				shared_candidate,
				&shared.object_ref,
				shared.body,
				shared.file_extension,
				uri_keys,
				uri_key_allocator,
				allocator,
			) {
				when adt.DEPENDENCY_FETCH_TRACE {
					trace_eprintf("adt_fetch\tadt\tshared_input\tadded\t%s\t%s\n", shared.object_ref.object_type, shared.object_ref.name)
					adt.trace_dependency_fetch(&shared.object_ref, shared.manifest_kind, shared.file_extension)
				}
				added = true
			} else {
				when adt.DEPENDENCY_FETCH_TRACE {
					trace_eprintf("adt_fetch\tadt\tshared_input\tskipped\t%s\t%s\n", shared.object_ref.object_type, shared.object_ref.name)
				}
			}
		}
	}
	return added
}

adt_fetch_task_result_destroy :: proc(result: ^Adt_Fetch_Task_Result, allocator: mem.Allocator) {
	if result == nil {
		return
	}
	for &entry in result.fetched {
		adt.object_ref_destroy(&entry.object_ref, allocator)
		adt.dependency_fetch_result_destroy(&entry.fetched, allocator)
	}
	delete(result.fetched)
	free(result, allocator)
}

store_adt_dependency_fetch :: proc(
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	object_ref: ^adt.Object_Ref,
	fetched: ^adt.Dependency_Fetch_Result,
	allocator: mem.Allocator,
) {
	if store == nil || profile == nil {
		when adt.DEPENDENCY_FETCH_TRACE {
			trace_eprintf("adt_fetch\tadt\tcache\tskipped\t%s\t%s\n", object_ref.object_type, object_ref.name)
		}
		return
	}
	artifacts := make([dynamic]dep_store.Stored_Artifact_Input, 0, 1 + len(fetched.shared_dependencies), allocator)
	fetched_at := dependency_fetched_at(allocator)
	append(
		&artifacts,
		dependency_artifact_from_adt(object_ref, fetched.manifest_kind, fetched.file_extension, fetched.body, fetched_at, allocator),
	)
	for &shared in fetched.shared_dependencies {
		append(
			&artifacts,
			dependency_artifact_from_adt(&shared.object_ref, shared.manifest_kind, shared.file_extension, shared.body, fetched_at, allocator),
		)
	}
	ids, err := dep_store.put_artifacts(store, profile, artifacts[:], allocator)
	defer delete(ids)
	when adt.DEPENDENCY_FETCH_TRACE {
		if err == .None {
			trace_eprintf("adt_fetch\tadt\tcache\tok\t%s\t%s\t%d\n", object_ref.object_type, object_ref.name, len(artifacts))
		} else {
			trace_eprintf("adt_fetch\tadt\tcache_err\t%s\t%s\t%v\n", object_ref.object_type, object_ref.name, err)
		}
	} else {
		_ = err
	}
}

dependency_artifact_from_adt :: proc(
	object_ref: ^adt.Object_Ref,
	object_kind,
	file_extension,
	source,
	fetched_at: string,
	allocator: mem.Allocator,
) -> dep_store.Stored_Artifact_Input {
	extension := strings.trim_space(file_extension)
	source_text := source
	if source_looks_xml(source) {
		source_text = synthetic_dependency_source(object_ref.name, object_kind, allocator)
		extension = "abap"
	}
	if strings.trim_space(extension) == "" {
		extension = "abap"
	}
	return dep_store.Stored_Artifact_Input {
		package_name   = object_ref.package_name,
		object_kind    = object_kind,
		object_name    = object_ref.name,
		object_uri     = object_ref.uri,
		object_type    = object_ref.object_type,
		description    = object_ref.description,
		file_extension = extension,
		source_text    = source_text,
		fetched_at     = fetched_at,
	}
}

dependency_fetched_at :: proc(allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "odin-")
	strings.write_i64(&out, time.to_unix_seconds(time.now()))
	return strings.to_string(out)
}

standalone_dependency_profile :: proc() -> dep_store.Dependency_Profile {
	return dep_store.Dependency_Profile {
		product_version         = "adt",
		default_package_version = "default",
	}
}

add_adt_fetched_dependency_input :: proc(
	candidates: ^[dynamic]Project_Candidate_Input,
	dependencies: ^[dynamic]Source_Input,
	candidate: Remote_Dependency_Candidate,
	object_ref: ^adt.Object_Ref,
	source: string,
	file_extension: string,
	uri_keys: ^map[string]bool,
	uri_key_allocator: mem.Allocator,
	allocator: mem.Allocator,
) -> bool {
	uri := adt_dependency_uri(object_ref, file_extension, allocator)
	if !project_input_uri_key_add_if_missing(uri_keys, uri, uri_key_allocator) {
		delete(uri, allocator)
		return false
	}
	input_source: string
	if source_looks_xml(source) {
		input_source = synthetic_dependency_source(object_ref.name, candidate.kind, allocator)
	} else {
		input_source = strings.clone(source, allocator)
	}
	append_dependency_input(
		candidates,
		dependencies,
		Source_Input{uri = uri, source = input_source},
		candidate,
		object_ref.name,
		allocator,
	)
	return true
}

adt_dependency_uri :: proc(
	object_ref: ^adt.Object_Ref,
	file_extension: string,
	allocator: mem.Allocator,
) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "abapls-adt:")
	strings.write_string(&out, object_ref.uri)
	ext := strings.trim_space(file_extension)
	if ext != "" && strings.index_byte(object_ref.uri, '.') < 0 {
		strings.write_byte(&out, '.')
		strings.write_string(&out, ext)
	}
	return strings.to_string(out)
}

manifest_has_project_dotenv :: proc(manifest: ^Workspace_Manifest, allocator: mem.Allocator) -> bool {
	path, ok := manifest_project_dotenv_path(manifest, allocator)
	if ok {
		delete(path, allocator)
	}
	return ok
}

manifest_project_dotenv_path :: proc(
	manifest: ^Workspace_Manifest,
	allocator: mem.Allocator,
) -> (string, bool) {
	path, ok := join_path2(manifest.root_path, ".env", allocator)
	if !ok {
		return "", false
	}
	info, err := os.stat(path, allocator)
	if err == nil && info.type == .Regular {
		return path, true
	}
	delete(path, allocator)
	return "", false
}

append_dependency_input :: proc(
	candidates: ^[dynamic]Project_Candidate_Input,
	dependencies: ^[dynamic]Source_Input,
	input: Source_Input,
	candidate: Remote_Dependency_Candidate,
	object_name: string,
	allocator: mem.Allocator,
) {
	if candidate.kind == "include" {
		append(
			candidates,
			Project_Candidate_Input {
				input = input,
				object_name = strings.clone(object_name if object_name != "" else candidate.name, allocator),
			},
		)
	} else {
		append(dependencies, input)
	}
}

project_input_uri_keys :: proc(
	target_uri: string,
	dependencies: []Source_Input,
	candidates: []Project_Candidate_Input,
	extra: int,
	allocator: mem.Allocator,
) -> map[string]bool {
	keys := make(map[string]bool, 1 + len(dependencies) + len(candidates) + extra, allocator)
	_ = project_input_uri_key_add_if_missing(&keys, target_uri, allocator)
	for input in dependencies {
		_ = project_input_uri_key_add_if_missing(&keys, input.uri, allocator)
	}
	for candidate in candidates {
		_ = project_input_uri_key_add_if_missing(&keys, candidate.input.uri, allocator)
	}
	return keys
}

project_input_uri_key_exists :: proc(
	keys: ^map[string]bool,
	uri: string,
	allocator: mem.Allocator,
) -> bool {
	key := normalized_uri_path_key(uri, allocator)
	return key in keys^
}

project_input_uri_key_add_if_missing :: proc(
	keys: ^map[string]bool,
	uri: string,
	allocator: mem.Allocator,
) -> bool {
	key := normalized_uri_path_key(uri, allocator)
	if key in keys^ {
		return false
	}
	keys^[key] = true
	return true
}

collect_local_export_candidate_paths :: proc(
	root: string,
	file_names: []string,
	out: ^[dynamic]string,
	allocator: mem.Allocator,
) {
	entries, err := os.read_all_directory_by_path(root, allocator)
	if err != nil {
		return
	}
	defer os.file_info_slice_delete(entries, allocator)
	for entry in entries {
		#partial switch entry.type {
		case .Directory:
			collect_local_export_candidate_paths(entry.fullpath, file_names, out, allocator)
		case .Regular:
			file_name := canonical_name(path_file_name(entry.fullpath), allocator)
			for wanted in file_names {
				if file_name == wanted && !string_list_contains(out^[:], entry.fullpath) {
					append(out, strings.clone(entry.fullpath, allocator))
				}
			}
		}
	}
}

local_export_candidate_file_names :: proc(
	candidate: Remote_Dependency_Candidate,
	allocator: mem.Allocator,
) -> [dynamic]string {
	names := make([dynamic]string, 0, 2, allocator)
	encoded := encode_local_export_component(candidate.name, allocator)
	if encoded == "" {
		return names
	}
	switch candidate.kind {
	case "include", "function", "static", "report":
		append(&names, local_export_file_name(encoded, "abap", allocator))
	case "message-class":
		append(&names, local_export_file_name(encoded, "xml", allocator))
	case "symbol", "type":
		append(&names, local_export_file_name(encoded, "xml", allocator))
		append(&names, local_export_file_name(encoded, "abap", allocator))
	}
	return names
}

local_export_file_name :: proc(encoded, extension: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, encoded)
	strings.write_byte(&out, '.')
	strings.write_string(&out, extension)
	return canonical_name(strings.to_string(out), allocator)
}

local_export_abap_source_matches :: proc(candidate: Remote_Dependency_Candidate, source: string) -> bool {
	if candidate.kind != "static" {
		return true
	}
	return source_declares_class_or_interface(source, candidate.name)
}

source_declares_class_or_interface :: proc(source, name: string) -> bool {
	rest := source
	for rest != "" {
		line: string
		line, rest = split_line(rest)
		trimmed := trim_left_ascii_ws(line)
		if strings.has_prefix(trimmed, "*") || strings.has_prefix(trimmed, "\"") {
			continue
		}
		keyword, after_keyword, keyword_ok := split_first_word(trimmed)
		decl_name, rest2, name_ok := split_first_word(after_keyword)
		if !keyword_ok || !name_ok || !strings.equal_fold(trim_decl_token(decl_name), name) {
			continue
		}
		if strings.equal_fold(keyword, "INTERFACE") {
			return true
		}
		next, _, next_ok := split_first_word(rest2)
		if strings.equal_fold(keyword, "CLASS") && next_ok && strings.equal_fold(next, "DEFINITION") {
			return true
		}
	}
	return false
}

split_first_word :: proc(text: string) -> (string, string, bool) {
	trimmed := trim_left_ascii_ws(text)
	if trimmed == "" {
		return "", "", false
	}
	end := 0
	for end < len(trimmed) && trimmed[end] != ' ' && trimmed[end] != '\t' && trimmed[end] != '\r' && trimmed[end] != '\n' {
		end += 1
	}
	return trimmed[:end], trimmed[end:], true
}

trim_decl_token :: proc(token: string) -> string {
	end := len(token)
	for end > 0 && (token[end - 1] == '.' || token[end - 1] == ':') {
		end -= 1
	}
	return token[:end]
}

split_line :: proc(text: string) -> (string, string) {
	for i in 0 ..< len(text) {
		if text[i] == '\n' {
			line := text[:i]
			if len(line) > 0 && line[len(line) - 1] == '\r' {
				line = line[:len(line) - 1]
			}
			return line, text[i + 1:]
		}
	}
	return text, ""
}

trim_left_ascii_ws :: proc(text: string) -> string {
	i := 0
	for i < len(text) && (text[i] == ' ' || text[i] == '\t' || text[i] == '\r' || text[i] == '\n') {
		i += 1
	}
	return text[i:]
}

source_looks_xml :: proc(source: string) -> bool {
	return strings.has_prefix(strings.trim_space(source), "<")
}

synthetic_dependency_source :: proc(name, kind: string, allocator: mem.Allocator) -> string {
	n := canonical_name(name, allocator)
	out := strings.builder_make(allocator)
	switch kind {
	case "function":
		strings.write_string(&out, "FUNCTION ")
		strings.write_string(&out, n)
		strings.write_string(&out, ".\nENDFUNCTION.\n")
	case "static":
		strings.write_string(&out, "CLASS ")
		strings.write_string(&out, n)
		strings.write_string(&out, " DEFINITION.\nENDCLASS.\nCLASS ")
		strings.write_string(&out, n)
		strings.write_string(&out, " IMPLEMENTATION.\nENDCLASS.\n")
	case "report":
		strings.write_string(&out, "REPORT ")
		strings.write_string(&out, n)
		strings.write_string(&out, ".\n")
	case:
		strings.write_string(&out, "TYPES ")
		strings.write_string(&out, n)
		strings.write_string(&out, " TYPE string.\n")
	}
	return strings.to_string(out)
}

is_remote_lookup_candidate :: proc(name, kind: string) -> bool {
	trimmed := strings.trim_space(name)
	if trimmed == "" {
		return false
	}
	if is_remote_lookup_name(trimmed) {
		return true
	}
	if kind == "type" || kind == "static" || kind == "function" || kind == "report" {
		return is_standard_remote_type_like_name(trimmed)
	}
	if kind == "message-class" {
		return is_standard_message_class_name(trimmed)
	}
	return false
}

is_remote_lookup_candidate_after_local_resolution :: proc(name, kind: string) -> bool {
	trimmed := strings.trim_space(name)
	if trimmed == "" {
		return false
	}
	if is_remote_lookup_name(trimmed) {
		return true
	}
	if kind == "type" || kind == "static" || kind == "function" || kind == "report" {
		return is_standard_remote_type_like_name_after_local_resolution(trimmed)
	}
	if kind == "message-class" {
		return is_standard_message_class_name(trimmed)
	}
	return false
}

is_remote_lookup_name :: proc(name: string) -> bool {
	if name == "" {
		return false
	}
	if name[0] == '/' {
		return true
	}
	return strings.equal_fold(name[:1], "z") || strings.equal_fold(name[:1], "y")
}

is_standard_remote_type_like_name :: proc(name: string) -> bool {
	if name == "" || name[0] == '/' {
		return name != ""
	}
	if !ascii_alpha(name[0]) {
		return false
	}
	if is_likely_local_identifier_style(name) || is_likely_builtin_type_name(name) {
		return false
	}
	return ascii_name_bytes(name)
}

is_standard_remote_type_like_name_after_local_resolution :: proc(name: string) -> bool {
	if name == "" || name[0] == '/' {
		return name != ""
	}
	if !ascii_alpha(name[0]) {
		return false
	}
	if is_likely_builtin_type_name(name) {
		return false
	}
	return ascii_name_bytes(name)
}

is_standard_message_class_name :: proc(name: string) -> bool {
	if name == "" || name[0] == '/' {
		return name != ""
	}
	all_digits := true
	for b in transmute([]byte)name {
		if !ascii_digit(b) {
			all_digits = false
			break
		}
	}
	if all_digits {
		return true
	}
	if !ascii_alpha(name[0]) {
		return false
	}
	return !is_likely_local_identifier_style(name) && ascii_name_bytes(name)
}

is_likely_builtin_type_name :: proc(name: string) -> bool {
	builtins := [?]string {
		"i", "int1", "int2", "int4", "int8", "f", "p", "decfloat", "decfloat16",
		"decfloat34", "string", "c", "n", "d", "t", "x", "xstring", "data", "any",
		"abap_bool", "flag", "xfeld", "syst", "guid", "symsgv", "sydatum", "timestamp",
		"cursor", "tabname", "cdobjectcl", "rs38l_fnam", "memoryid", "time", "timestmp",
		"object", "standard", "table", "simple", "numeric", "csequence", "clike",
		"xsequence", "previous", "to",
	}
	for value in builtins {
		if strings.equal_fold(name, value) {
			return true
		}
	}
	if ascii_has_prefix_ignore_case(name, "char") {
		for i in 4 ..< len(name) {
			if !ascii_digit(name[i]) {
				return false
			}
		}
		return len(name) > 4
	}
	return false
}

is_likely_local_identifier_style :: proc(name: string) -> bool {
	prefixes := [?]string {
		"lv_", "ls_", "lt_", "lr_", "lo_", "li_", "lm_", "lx_", "lc_", "ld_",
		"gv_", "gs_", "gt_", "gr_", "go_", "gi_", "gm_", "gx_", "gc_", "gd_",
		"mv_", "ms_", "mt_", "mr_", "mo_", "mi_", "mm_", "mx_", "mc_", "md_",
		"iv_", "is_", "it_", "ir_", "io_", "ii_", "im_", "ix_", "ic_", "id_",
		"ev_", "es_", "et_", "er_", "eo_", "ei_", "em_", "ex_", "ec_", "ed_",
		"rv_", "rs_", "rt_", "rr_", "ro_", "ri_", "rm_", "rx_", "rc_", "rd_",
		"cv_", "cs_", "ct_", "cr_", "co_", "ci_", "cm_", "cc_", "cd_",
		"sv_", "ss_", "st_", "sr_", "so_", "si_", "sm_", "sx_", "sc_", "sd_",
		"tv_", "ts_", "tt_", "tr_", "to_", "ti_", "tm_", "tx_", "tc_", "td_",
		"uv_", "us_", "ut_", "ur_", "uo_", "ui_", "um_", "ux_", "uc_", "ud_",
		"wv_", "ws_", "wt_", "wr_", "wo_", "wi_", "wm_", "wx_", "wc_", "wd_",
		"xv_", "xs_", "xt_", "xr_", "xo_", "xi_", "xm_", "xx_", "xc_", "xd_",
		"yv_", "ys_", "yt_", "yr_", "yo_", "yi_", "ym_", "yx_", "yc_", "yd_",
		"zv_", "zs_", "zt_", "zr_", "zo_", "zi_", "zm_", "zx_", "zc_", "zd_",
	}
	for prefix in prefixes {
		if ascii_has_prefix_ignore_case(name, prefix) {
			return true
		}
	}
	return false
}

ascii_name_bytes :: proc(name: string) -> bool {
	for b in transmute([]byte)name {
		if !(ascii_alpha(b) || ascii_digit(b) || b == '_' || b == '/') {
			return false
		}
	}
	return true
}

ascii_has_prefix_ignore_case :: proc(text, prefix: string) -> bool {
	return len(text) >= len(prefix) && strings.equal_fold(text[:len(prefix)], prefix)
}

encode_local_export_component :: proc(value: string, allocator: mem.Allocator) -> string {
	upper := strings.to_upper(strings.trim_space(value), allocator)
	out := strings.builder_make(allocator)
	for b in transmute([]byte)upper {
		if ascii_alpha(b) || ascii_digit(b) ||
		   b == '-' || b == '_' || b == '.' || b == '!' || b == '~' ||
		   b == '*' || b == '\'' || b == '(' || b == ')' {
			strings.write_byte(&out, b)
		} else {
			strings.write_byte(&out, '%')
			strings.write_byte(&out, hex_digit(b >> 4))
			strings.write_byte(&out, hex_digit(b & 0x0f))
		}
	}
	return strings.to_string(out)
}

path_file_name :: proc(path: string) -> string {
	start := 0
	for i in 0 ..< len(path) {
		if path[i] == '/' || path[i] == '\\' {
			start = i + 1
		}
	}
	return path[start:]
}

hex_digit :: proc(value: byte) -> byte {
	if value <= 9 {
		return '0' + value
	}
	return 'A' + value - 10
}

ascii_alpha :: proc(value: byte) -> bool {
	return('a' <= value && value <= 'z') || ('A' <= value && value <= 'Z')
}

ascii_digit :: proc(value: byte) -> bool {
	return '0' <= value && value <= '9'
}
