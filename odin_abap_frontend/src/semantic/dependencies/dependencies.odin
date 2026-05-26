package abap_frontend_semantic_dependencies

import analyze "../analyze"
import "core:mem/virtual"

import "../../adt"
import ddic_xml "../../ddic_xml"
import dep_store "../../dependency_store"
import frontend_runtime "../../runtime"

import base_runtime "base:runtime"
import "core:fmt"
import "core:mem"
import net_url "core:net"
import "core:os"
import filepath "core:path/filepath"
import "core:strings"
import "core:time"

trace_eprintf :: fmt.eprintf

@(private)
dep_store_candidate_kind :: proc(
	kind: analyze.Remote_Dependency_Kind,
) -> dep_store.Candidate_Kind {
	switch kind {
	case .Include:
		return .Include
	case .Message_Class:
		return .Message_Class
	case .Report:
		return .Report
	case .Function:
		return .Function
	case .Static:
		return .Static
	case .Type:
		return .Type
	case .Symbol:
		return .Symbol
	}
	return .Symbol
}

@(private)
remote_candidate_kind_text :: proc(kind: analyze.Remote_Dependency_Kind) -> string {
	return dep_store.candidate_kind_text(dep_store_candidate_kind(kind))
}

Dependency_Config :: struct {
	store:              ^dep_store.Dependency_Store,
	profile:            ^dep_store.Dependency_Profile,
	store_any_profile:  bool,
	local_export_roots: []string,
	adt_client:         ^adt.Client,
}

analyze_with_dependency_drain :: proc(
	target: analyze.Source_Input,
	candidates: [dynamic]analyze.Project_Candidate_Input,
	dependencies: [dynamic]analyze.Source_Input,
	config: Dependency_Config,
	options: analyze.Analyze_Options,
	allocator: mem.Allocator,
) -> analyze.Project_Analysis {
	targets := [?]analyze.Source_Input{target}
	return analyze_inputs_with_dependency_drain(
		targets[:],
		candidates,
		dependencies,
		config,
		options,
		allocator,
	)
}

analyze_inputs_with_dependency_drain :: proc(
	targets: []analyze.Source_Input,
	candidates: [dynamic]analyze.Project_Candidate_Input,
	dependencies: [dynamic]analyze.Source_Input,
	config: Dependency_Config,
	options: analyze.Analyze_Options,
	allocator: mem.Allocator,
) -> analyze.Project_Analysis {
	candidate_inputs := candidates
	dependency_inputs := dependencies
	state := analyze.project_state_make({}, allocator)
	project := analyze_inputs_with_state(
		&state,
		targets,
		candidate_inputs[:],
		dependency_inputs[:],
		options,
		allocator,
	)
	has_store := config.store != nil
	has_profile := config.profile != nil
	if !has_store && len(config.local_export_roots) == 0 && config.adt_client == nil {
		return project
	}

	seen_temp_arena: virtual.Arena
	_ = virtual.arena_init_growing(&seen_temp_arena)
	defer virtual.arena_destroy(&seen_temp_arena)
	seen_temp_allocator := virtual.arena_allocator(&seen_temp_arena)

	seen_artifacts := make(map[i64]bool, 16, seen_temp_allocator)
	seen_store_candidates := make(map[analyze.Remote_Dependency_Key]bool, 64, seen_temp_allocator)
	seen_local_candidates := make(map[analyze.Remote_Dependency_Key]bool, 64, seen_temp_allocator)
	seen_adt_candidates := make(map[analyze.Remote_Dependency_Key]bool, 64, seen_temp_allocator)

	iter_temp_arena: virtual.Arena
	_ = virtual.arena_init_growing(&iter_temp_arena)
	defer virtual.arena_destroy(&iter_temp_arena)
	temp_allocator := virtual.arena_allocator(&iter_temp_arena)
	for {
		iteration_temp := virtual.arena_temp_begin(&iter_temp_arena)
		defer virtual.arena_temp_end(iteration_temp)

		remote_candidates := analyze.collect_project_state_remote_dependency_candidates(
			&state,
			true,
			temp_allocator,
		)
		added := false
		if has_store {
			store_candidates := unseen_remote_candidates(
				remote_candidates[:],
				&seen_store_candidates,
				temp_allocator,
				seen_temp_allocator,
			)
			if config.store_any_profile || !has_profile {
				added = add_dependency_store_any_profile_matches(
					&candidate_inputs,
					&dependency_inputs,
					store_candidates[:],
					config.store,
					&seen_artifacts,
					options.pool,
					targets[0].uri if len(targets) > 0 else "",
					allocator,
					temp_allocator,
				)
			} else {
				added = add_dependency_store_matches(
					&candidate_inputs,
					&dependency_inputs,
					store_candidates[:],
					config.store,
					config.profile,
					&seen_artifacts,
					options.pool,
					targets[0].uri if len(targets) > 0 else "",
					allocator,
					temp_allocator,
				)
			}
		}
		if !added && len(config.local_export_roots) > 0 {
			local_candidates := unseen_remote_candidates(
				remote_candidates[:],
				&seen_local_candidates,
				temp_allocator,
				seen_temp_allocator,
			)
			added = add_local_export_matches(
				&candidate_inputs,
				&dependency_inputs,
				local_candidates[:],
				config.store if has_store && has_profile else nil,
				config.profile if has_store && has_profile else nil,
				config.local_export_roots,
				targets[0].uri if len(targets) > 0 else "",
				allocator,
			)
		}
		if !added && config.adt_client != nil {
			adt_remote_candidates := analyze.collect_project_state_remote_dependency_candidates(
				&state,
				true,
				temp_allocator,
			)
			adt_candidates := unseen_remote_candidates(
				adt_remote_candidates[:],
				&seen_adt_candidates,
				temp_allocator,
				seen_temp_allocator,
			)
			attempted_adt := len(adt_candidates) > 0
			added = add_adt_matches_with_client(
				&candidate_inputs,
				&dependency_inputs,
				adt_candidates[:],
				config.store if has_store && has_profile else nil,
				config.profile if has_store && has_profile else nil,
				config.adt_client,
				options.pool,
				targets[0].uri if len(targets) > 0 else "",
				allocator,
			)
			if !added && attempted_adt {
				added = true
			}
		}
		if added {
			project = analyze_inputs_with_state(
				&state,
				targets,
				candidate_inputs[:],
				dependency_inputs[:],
				options,
				allocator,
			)
		}
		if !added {
			break
		}
	}
	return project
}

analyze_inputs_with_state :: proc(
	state: ^analyze.Project_State,
	targets: []analyze.Source_Input,
	candidates: []analyze.Project_Candidate_Input,
	dependencies: []analyze.Source_Input,
	options: analyze.Analyze_Options,
	allocator: mem.Allocator,
) -> analyze.Project_Analysis {
	if len(targets) == 0 {
		return analyze.project_state_analysis(state)
	}
	return analyze.project_state_analyze_targets_with_candidate_inputs(
		state,
		targets,
		candidates,
		dependencies,
		options,
		allocator,
	)
}

unseen_remote_candidates :: proc(
	remote_candidates: []analyze.Remote_Dependency_Candidate,
	seen: ^map[analyze.Remote_Dependency_Key]bool,
	allocator: mem.Allocator,
	seen_allocator: mem.Allocator,
) -> [dynamic]analyze.Remote_Dependency_Candidate {
	out := make([dynamic]analyze.Remote_Dependency_Candidate, 0, len(remote_candidates), allocator)
	for candidate in remote_candidates {
		if candidate.kind == .Type && analyze.is_builtin_type_name(candidate.name) {
			continue
		}
		key := analyze.Remote_Dependency_Key {
			name = candidate.name,
			kind = candidate.kind,
			hint = candidate.hint,
		}
		if key in seen^ {
			continue
		}
		seen_key := key
		seen_key.name = strings.clone(candidate.name, seen_allocator)
		seen^[seen_key] = true
		append(&out, candidate)
	}
	return out
}

add_dependency_store_matches :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	remote_candidates: []analyze.Remote_Dependency_Candidate,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	seen_artifacts: ^map[i64]bool,
	pool: ^frontend_runtime.Pool,
	target_uri: string,
	allocator: mem.Allocator,
	temp_allocator: mem.Allocator,
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
		temp_allocator,
	)
}

add_dependency_store_any_profile_matches :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	remote_candidates: []analyze.Remote_Dependency_Candidate,
	store: ^dep_store.Dependency_Store,
	seen_artifacts: ^map[i64]bool,
	pool: ^frontend_runtime.Pool,
	target_uri: string,
	allocator: mem.Allocator,
	temp_allocator: mem.Allocator,
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
		temp_allocator,
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
	candidate:   analyze.Remote_Dependency_Candidate,
	any_profile: bool,
}

add_dependency_store_matches_impl :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	remote_candidates: []analyze.Remote_Dependency_Candidate,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	any_profile: bool,
	seen_artifacts: ^map[i64]bool,
	pool: ^frontend_runtime.Pool,
	target_uri: string,
	trace_source: string,
	allocator: mem.Allocator,
	temp_allocator: mem.Allocator,
) -> bool {
	uri_keys := project_input_uri_keys(
		target_uri,
		dependencies^[:],
		candidates^[:],
		len(remote_candidates),
		temp_allocator,
	)

	added := false
	batch_size := pool.options.task_capacity
	task_allocator := base_runtime.heap_allocator()
	for start := 0; start < len(remote_candidates); {
		end := min(start + batch_size, len(remote_candidates))
		tasks := make(
			[dynamic]frontend_runtime.Task(^Dependency_Store_Task_Result),
			0,
			end - start,
			task_allocator,
		)
		defer delete(tasks)
		candidates_slice := remote_candidates[start:end]
		for candidate in candidates_slice {
			payload := Dependency_Store_Task_Payload {
				store       = store^,
				profile     = profile,
				candidate   = candidate,
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
				temp_allocator,
				trace_source,
				allocator,
			) {
				added = true
			}
			dependency_store_task_result_destroy(result, base_runtime.heap_allocator())
		}
		start = end
	}
	return added
}

dependency_store_find_task :: proc(
	payload: Dependency_Store_Task_Payload,
) -> ^Dependency_Store_Task_Result {
	result_allocator := base_runtime.heap_allocator()
	result := new(Dependency_Store_Task_Result, result_allocator)
	store := payload.store
	reader, reader_err := dep_store.reader(&store, context.temp_allocator)
	if reader_err != .None {
		result.err = reader_err
		return result
	}
	defer dep_store.reader_destroy(&reader)
	if payload.any_profile {
		record, ok, err := dep_store.reader_find_artifact_for_candidate_any_profile(
			&reader,
			payload.candidate.name,
			dep_store_candidate_kind(payload.candidate.kind),
			context.temp_allocator,
		)
		result.ok = ok
		result.err = err
		if ok && err == .None {
			result.record = clone_dependency_record(&record, result_allocator)
		}
	} else if payload.profile != nil {
		record, ok, err := dep_store.reader_find_artifact_for_candidate(
			&reader,
			payload.profile,
			payload.candidate.name,
			dep_store_candidate_kind(payload.candidate.kind),
			context.temp_allocator,
		)
		result.ok = ok
		result.err = err
		if ok && err == .None {
			result.record = clone_dependency_record(&record, result_allocator)
		}
	}
	return result
}

clone_dependency_record :: proc(
	record: ^dep_store.Stored_Artifact_Record,
	allocator: mem.Allocator,
) -> dep_store.Stored_Artifact_Record {
	return dep_store.Stored_Artifact_Record {
		artifact_id = record.artifact_id,
		package_name = strings.clone(record.package_name, allocator),
		package_version = strings.clone(record.package_version, allocator),
		object_kind = strings.clone(record.object_kind, allocator),
		object_name = strings.clone(record.object_name, allocator),
		object_uri = strings.clone(record.object_uri, allocator),
		object_type = strings.clone(record.object_type, allocator),
		description = strings.clone(record.description, allocator),
		file_extension = strings.clone(record.file_extension, allocator),
		source_text = strings.clone(record.source_text, allocator),
	}
}

add_dependency_store_task_result :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	candidate: analyze.Remote_Dependency_Candidate,
	result: ^Dependency_Store_Task_Result,
	seen_artifacts: ^map[i64]bool,
	uri_keys: ^map[string]bool,
	uri_key_allocator: mem.Allocator,
	trace_source: string,
	allocator: mem.Allocator,
) -> bool {
	if result == nil ||
	   result.err != .None ||
	   !result.ok ||
	   result.record.artifact_id in seen_artifacts^ {
		return false
	}
	seen_artifacts^[result.record.artifact_id] = true
	uri := dependency_record_uri(&result.record, allocator)
	if !project_input_uri_key_add_if_missing(uri_keys, uri, uri_key_allocator) {
		delete(uri, allocator)
		return false
	}
	input := source_input_from_dependency_record(&result.record, candidate, uri, allocator)
	append_dependency_input(
		candidates,
		dependencies,
		input,
		candidate,
		result.record.object_name,
		allocator,
	)
	when adt.DEPENDENCY_FETCH_TRACE {
		trace_eprintf(
			"adt_fetch\t%s\tadd\t%s\t%s\t%s\t%s\n",
			trace_source,
			remote_candidate_kind_text(candidate.kind),
			candidate.name,
			result.record.object_kind,
			result.record.object_name,
		)
	}
	return true
}

dependency_store_task_result_destroy :: proc(
	result: ^Dependency_Store_Task_Result,
	allocator: mem.Allocator,
) {
	if result == nil {
		return
	}
	dependency_record_destroy(&result.record, allocator)
	free(result, allocator)
}

dependency_record_destroy :: proc(
	record: ^dep_store.Stored_Artifact_Record,
	allocator: mem.Allocator,
) {
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
	candidate: analyze.Remote_Dependency_Candidate,
	uri: string,
	allocator: mem.Allocator,
) -> analyze.Source_Input {
	source: string
	if dependency_source_is_xml(record.object_kind, record.file_extension, record.source_text) {
		source =
			ddic_xml.dependency_source(record.object_name, record.object_kind, record.source_text, allocator) if candidate.kind == .Type else strings.clone("", allocator)
	} else {
		source = strings.clone(record.source_text, allocator)
	}
	return analyze.Source_Input{uri = uri, source = source, mode = .Dependency_Interface}
}

dependency_record_uri :: proc(
	record: ^dep_store.Stored_Artifact_Record,
	allocator: mem.Allocator,
) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "abapls-cache:/")
	strings.write_string(&out, record.object_kind)
	strings.write_byte(&out, '/')
	strings.write_string(&out, record.object_name)
	strings.write_string(&out, ".abap")
	return strings.to_string(out)
}

add_local_export_matches :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	remote_candidates: []analyze.Remote_Dependency_Candidate,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	roots: []string,
	target_uri: string,
	allocator: mem.Allocator,
) -> bool {
	uri_key_arena: mem.Dynamic_Arena
	uri_key_backing := base_runtime.heap_allocator()
	mem.dynamic_arena_init(&uri_key_arena, uri_key_backing, uri_key_backing, alignment = 64)
	defer mem.dynamic_arena_destroy(&uri_key_arena)
	uri_key_allocator := mem.dynamic_arena_allocator(&uri_key_arena)
	uri_keys := project_input_uri_keys(
		target_uri,
		dependencies^[:],
		candidates^[:],
		len(remote_candidates),
		uri_key_allocator,
	)

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
			input_source: string
			if dependency_source_is_xml("", strings.trim_prefix(filepath.ext(path), "."), source) {
				input_source =
					ddic_xml.dependency_source(candidate.name, "", source, allocator) if candidate.kind == .Type else strings.clone("", allocator)
			} else if !local_export_abap_source_matches(candidate, source) {
				delete(source, allocator)
				continue
			} else {
				input_source = strings.clone(source, allocator)
			}
			if !project_input_uri_key_add_if_missing(&uri_keys, path, uri_key_allocator) {
				delete(input_source, allocator)
				delete(source, allocator)
				continue
			}
			store_local_export_dependency(
				store,
				profile,
				candidate,
				path,
				roots,
				source,
				strings.trim_prefix(filepath.ext(path), "."),
				allocator,
			)
			delete(source, allocator)
			append_dependency_input(
				candidates,
				dependencies,
				analyze.Source_Input {
					uri = path,
					source = input_source,
					mode = .Dependency_Interface,
				},
				candidate,
				candidate.name,
				allocator,
			)
			added = true
		}
	}
	return added
}

store_local_export_dependency :: proc(
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	candidate: analyze.Remote_Dependency_Candidate,
	path: string,
	roots: []string,
	source: string,
	file_extension: string,
	allocator: mem.Allocator,
) {
	if store == nil || profile == nil {
		return
	}
	store_arena: mem.Dynamic_Arena
	store_backing := base_runtime.heap_allocator()
	mem.dynamic_arena_init(&store_arena, store_backing, store_backing, alignment = 64)
	defer mem.dynamic_arena_destroy(&store_arena)
	store_allocator := mem.dynamic_arena_allocator(&store_arena)
	object_kind, object_type := local_export_object_kind_type(
		candidate,
		file_extension,
		source,
		store_allocator,
	)
	package_name := local_export_package_name(path, roots, store_allocator)
	if package_name == "" {
		package_name = candidate.name
	}
	source_text := source
	extension := file_extension if file_extension != "" else "abap"
	if dependency_source_is_xml(object_kind, file_extension, source) &&
	   dependency_object_kind_is_ddic(object_kind) {
		extension = "xml"
	}
	fetched_at, _ := time.time_to_rfc3339(time.now(), allocator = store_allocator)
	artifact := dep_store.Stored_Artifact_Input {
		package_name   = package_name,
		object_kind    = object_kind,
		object_name    = candidate.name,
		object_uri     = path,
		object_type    = object_type,
		description    = "Local export dependency",
		file_extension = extension,
		source_text    = source_text,
		fetched_at     = fetched_at,
	}
	_, _ = dep_store.put_artifact(store, profile, &artifact, store_allocator)
}

read_text_file :: proc(path: string, allocator: mem.Allocator) -> (string, bool) {
	data, err := os.read_entire_file(path, allocator)
	if err != nil {
		return "", false
	}
	return string(data), true
}

local_export_object_kind_type :: proc(
	candidate: analyze.Remote_Dependency_Candidate,
	file_extension: string,
	source: string,
	allocator: mem.Allocator,
) -> (
	string,
	string,
) {
	if dependency_source_is_xml("", file_extension, source) {
		if candidate.kind == .Message_Class {
			return "message-class", "MSAG/N"
		}
		object_type := local_export_xml_attr(source, "adtcore:type", allocator)
		if object_type == "" {
			object_type = local_export_xml_attr(source, "type", allocator)
		}
		if object_type == "" {
			object_type = "TABL/DS"
		}
		return adt.infer_ddic_manifest_kind(&adt.Object_Ref{object_type = object_type}),
			object_type
	}
	switch candidate.kind {
	case .Include:
		return "include", "PROG/I"
	case .Report:
		return "report", "PROG/P"
	case .Function:
		return "function-module", "FUGR/FF"
	case .Message_Class, .Static, .Type, .Symbol:
		return "global-class", "CLAS/OC"
	}
	return "global-class", "CLAS/OC"
}

local_export_xml_attr :: proc(source, attr: string, allocator: mem.Allocator) -> string {
	needle := strings.concatenate({attr, "=\""}, allocator)
	defer delete(needle, allocator)
	start := strings.index(source, needle)
	if start < 0 {
		return ""
	}
	value_start := start + len(needle)
	end := strings.index_byte(source[value_start:], '"')
	if end < 0 {
		return ""
	}
	return strings.clone(source[value_start:value_start + end], allocator)
}

local_export_package_name :: proc(
	path: string,
	roots: []string,
	allocator: mem.Allocator,
) -> string {
	for root in roots {
		rel, err := filepath.rel(root, path, allocator)
		if err != .None {
			continue
		}
		normalized, normalize_err := filepath.replace_separators(rel, '/', allocator)
		delete(rel, allocator)
		if normalize_err != nil {
			continue
		}
		defer delete(normalized, allocator)
		component, component_ok := strings.split_by_byte_iterator(&normalized, '/')
		if !component_ok {
			continue
		}
		if component == "" || component == "." || component == ".." {
			continue
		}
		if decoded, ok := net_url.percent_decode(component, allocator); ok {
			return decoded
		}
		return strings.clone(component, allocator)
	}
	return ""
}

Adt_Fetched_Object :: struct {
	object_ref: adt.Object_Ref,
	fetched:    adt.Dependency_Fetch_Result,
}

Adt_Fetch_Task_Result :: struct {
	fetched: [dynamic]Adt_Fetched_Object,
}

Adt_Fetch_Task_Payload :: struct {
	client:           ^adt.Client,
	candidate:        analyze.Remote_Dependency_Candidate,
	result_allocator: mem.Allocator,
}

add_adt_matches_with_client :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	remote_candidates: []analyze.Remote_Dependency_Candidate,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	client: ^adt.Client,
	pool: ^frontend_runtime.Pool,
	target_uri: string,
	allocator: mem.Allocator,
) -> bool {
	temp_arena: virtual.Arena
	_ = virtual.arena_init_growing(&temp_arena)
	defer virtual.arena_destroy(&temp_arena)
	temp_allocator := virtual.arena_allocator(&temp_arena)

	uri_keys := project_input_uri_keys(
		target_uri,
		dependencies^[:],
		candidates^[:],
		len(remote_candidates),
		temp_allocator,
	)

	if client.csrf_token == "" {
		if adt.ensure_session(client, allocator) != .None {
			return false
		}
	}

	added := false
	tasks := make(
		[dynamic]frontend_runtime.Task(^Adt_Fetch_Task_Result),
		0,
		len(remote_candidates),
		temp_allocator,
	)
	for candidate in remote_candidates {
		payload := Adt_Fetch_Task_Payload {
			client           = client,
			candidate        = candidate,
			result_allocator = temp_allocator,
		}
		task, err := frontend_runtime.submit_value(pool, payload, adt_fetch_task)
		assert(err == .None)
		append(&tasks, task)
	}
	for task, i in tasks {
		result, _ := frontend_runtime.wait(task)
		if add_adt_fetch_task_result(
			candidates,
			dependencies,
			remote_candidates[i],
			result,
			store,
			profile,
			&uri_keys,
			temp_allocator,
			allocator,
		) {
			added = true
		}
	}
	return added
}

adt_fetch_task :: proc(payload: Adt_Fetch_Task_Payload) -> ^Adt_Fetch_Task_Result {
	if temp_arena := frontend_runtime.current_temp_arena(); temp_arena != nil {
		return fetch_adt_candidate(
			payload.client,
			payload.candidate,
			payload.result_allocator,
			temp_arena,
			virtual.arena_allocator(temp_arena),
		)
	}

	temp_arena: virtual.Arena
	_ = virtual.arena_init_growing(&temp_arena)
	defer virtual.arena_destroy(&temp_arena)
	temp_allocator := virtual.arena_allocator(&temp_arena)

	return fetch_adt_candidate(
		payload.client,
		payload.candidate,
		payload.result_allocator,
		&temp_arena,
		temp_allocator,
	)
}

fetch_adt_candidate :: proc(
	client: ^adt.Client,
	candidate: analyze.Remote_Dependency_Candidate,
	result_allocator: mem.Allocator,
	temp_arena: ^virtual.Arena,
	temp_allocator: mem.Allocator,
) -> ^Adt_Fetch_Task_Result {
	result := new(Adt_Fetch_Task_Result, result_allocator)
	result.fetched = make([dynamic]Adt_Fetched_Object, result_allocator)

	if adt_candidate_direct_first(candidate) {
		direct := adt.direct_dependency_object_refs(
			candidate.name,
			remote_candidate_direct_kind_text(candidate),
			temp_allocator,
		)
		count_fetched := fetch_adt_objects(
			client,
			candidate,
			direct[:],
			result,
			result_allocator,
			temp_arena,
			temp_allocator,
			true,
		)
		if count_fetched > 0 {
			return result
		}
	}
	when adt.DEPENDENCY_FETCH_TRACE {
		trace_eprintf(
			"adt_fetch\tadt\tsearch\t%s\t%s\n",
			remote_candidate_kind_text(candidate.kind),
			candidate.name,
		)
	}

	objects, err := adt.search_repository_objects(client, candidate.name, 50, temp_allocator)
	if err != .None {
		when adt.DEPENDENCY_FETCH_TRACE {
			trace_eprintf(
				"adt_fetch\tadt\tsearch_err\t%s\t%s\t%v\n",
				remote_candidate_kind_text(candidate.kind),
				candidate.name,
				err,
			)
		}
		objects = adt.direct_dependency_object_refs(
			candidate.name,
			remote_candidate_kind_text(candidate.kind),
			temp_allocator,
		)
	} else {
		when adt.DEPENDENCY_FETCH_TRACE {
			trace_eprintf(
				"adt_fetch\tadt\tsearch_ok\t%s\t%s\t%d\n",
				remote_candidate_kind_text(candidate.kind),
				candidate.name,
				len(objects),
			)
		}
	}

	selected := adt.select_dependency_objects(
		candidate.name,
		objects[:],
		remote_candidate_kind_text(candidate.kind),
		temp_allocator,
	)
	if len(selected) == 0 {
		selected = adt.direct_dependency_object_refs(
			candidate.name,
			remote_candidate_kind_text(candidate.kind),
			temp_allocator,
		)
	}
	when adt.DEPENDENCY_FETCH_TRACE {
		trace_eprintf(
			"adt_fetch\tadt\tselected\t%s\t%s\t%d\n",
			remote_candidate_kind_text(candidate.kind),
			candidate.name,
			len(selected),
		)
	}

	fetch_adt_objects(
		client,
		candidate,
		selected[:],
		result,
		result_allocator,
		temp_arena,
		temp_allocator,
	)
	return result
}

adt_candidate_direct_first :: proc(candidate: analyze.Remote_Dependency_Candidate) -> bool {
	if candidate.hint == .Object_Type || candidate.hint == .Interface_Type {
		return true
	}
	#partial switch candidate.kind {
	case .Include, .Message_Class, .Report, .Static:
		return true
	}
	return false
}

remote_candidate_direct_kind_text :: proc(
	candidate: analyze.Remote_Dependency_Candidate,
) -> string {
	if candidate.hint == .Interface_Type {
		return "interface-type"
	}
	if candidate.hint == .Object_Type {
		return "object-type"
	}
	if candidate.kind == .Type {
		return "ddic-type"
	}
	return remote_candidate_kind_text(candidate.kind)
}

fetch_adt_objects :: proc(
	client: ^adt.Client,
	candidate: analyze.Remote_Dependency_Candidate,
	objects: []adt.Object_Ref,
	result: ^Adt_Fetch_Task_Result,
	result_allocator: mem.Allocator,
	temp_arena: ^virtual.Arena,
	temp_allocator: mem.Allocator,
	stop_after_first := false,
) -> int {
	fetched_count := 0
	for &object_ref in objects {
		object_temp := virtual.arena_temp_begin(temp_arena)
		defer virtual.arena_temp_end(object_temp)

		when adt.DEPENDENCY_FETCH_TRACE {
			trace_eprintf(
				"adt_fetch\tadt\tfetch\t%s\t%s\t%s\t%s\n",
				remote_candidate_kind_text(candidate.kind),
				candidate.name,
				object_ref.object_type,
				object_ref.name,
			)
		}
		fetched, fetch_err := adt.fetch_dependency_object(client, &object_ref, temp_allocator)
		if fetch_err != .None {
			when adt.DEPENDENCY_FETCH_TRACE {
				trace_eprintf(
					"adt_fetch\tadt\tfetch_err\t%s\t%s\t%s\t%s\t%v\n",
					remote_candidate_kind_text(candidate.kind),
					candidate.name,
					object_ref.object_type,
					object_ref.name,
					fetch_err,
				)
			}
			continue
		}
		if adt.is_direct_ddic_elementinfo_object(&object_ref) {
			object_type := adt.ddic_object_type_from_xml(fetched.body)
			if object_type != "" {
				object_ref.object_type = object_type
				object_ref.uri = adt.ddic_dependency_uri_for_object_type(
					object_ref.name,
					object_type,
					temp_allocator,
				)
			}
		}
		when adt.DEPENDENCY_FETCH_TRACE {
			trace_eprintf(
				"adt_fetch\tadt\tfetch_ok\t%s\t%s\t%s\t%s\t%s\t%d\n",
				remote_candidate_kind_text(candidate.kind),
				candidate.name,
				object_ref.object_type,
				object_ref.name,
				fetched.manifest_kind,
				len(fetched.shared_dependencies),
			)
			adt.trace_dependency_fetch(&object_ref, fetched.manifest_kind, fetched.file_extension)
		}
		append(
			&result.fetched,
			Adt_Fetched_Object {
				object_ref = adt.clone_object_ref(&object_ref, result_allocator),
				fetched = adt.clone_dependency_fetch_result(&fetched, result_allocator),
			},
		)
		fetched_count += 1
		if stop_after_first {
			break
		}
	}
	return fetched_count
}

add_adt_fetch_task_result :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	candidate: analyze.Remote_Dependency_Candidate,
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
			entry.fetched.manifest_kind,
			entry.fetched.body,
			entry.fetched.file_extension,
			uri_keys,
			uri_key_allocator,
			allocator,
		)
		when adt.DEPENDENCY_FETCH_TRACE {
			status := "added" if input_added else "skipped"
			trace_eprintf(
				"adt_fetch\tadt\tinput\t%s\t%s\t%s\t%s\n",
				status,
				remote_candidate_kind_text(candidate.kind),
				entry.object_ref.object_type,
				entry.object_ref.name,
			)
		}
		if input_added {
			added = true
		}
		for &shared in entry.fetched.shared_dependencies {
			shared_candidate := analyze.Remote_Dependency_Candidate {
				name = shared.object_ref.name,
				kind = .Include,
			}
			if add_adt_fetched_dependency_input(
				candidates,
				dependencies,
				shared_candidate,
				&shared.object_ref,
				shared.manifest_kind,
				shared.body,
				shared.file_extension,
				uri_keys,
				uri_key_allocator,
				allocator,
			) {
				when adt.DEPENDENCY_FETCH_TRACE {
					trace_eprintf(
						"adt_fetch\tadt\tshared_input\tadded\t%s\t%s\n",
						shared.object_ref.object_type,
						shared.object_ref.name,
					)
					adt.trace_dependency_fetch(
						&shared.object_ref,
						shared.manifest_kind,
						shared.file_extension,
					)
				}
				added = true
			} else {
				when adt.DEPENDENCY_FETCH_TRACE {
					trace_eprintf(
						"adt_fetch\tadt\tshared_input\tskipped\t%s\t%s\n",
						shared.object_ref.object_type,
						shared.object_ref.name,
					)
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
			trace_eprintf(
				"adt_fetch\tadt\tcache\tskipped\t%s\t%s\n",
				object_ref.object_type,
				object_ref.name,
			)
		}
		return
	}
	store_arena: mem.Dynamic_Arena
	store_backing := base_runtime.heap_allocator()
	mem.dynamic_arena_init(&store_arena, store_backing, store_backing, alignment = 64)
	defer mem.dynamic_arena_destroy(&store_arena)
	store_allocator := mem.dynamic_arena_allocator(&store_arena)
	artifacts := make(
		[dynamic]dep_store.Stored_Artifact_Input,
		0,
		1 + len(fetched.shared_dependencies),
		store_allocator,
	)
	fetched_at, _ := time.time_to_rfc3339(time.now(), allocator = store_allocator)
	append(
		&artifacts,
		dependency_artifact_from_adt(
			object_ref,
			fetched.manifest_kind,
			fetched.file_extension,
			fetched.body,
			fetched_at,
			store_allocator,
		),
	)
	for &shared in fetched.shared_dependencies {
		append(
			&artifacts,
			dependency_artifact_from_adt(
				&shared.object_ref,
				shared.manifest_kind,
				shared.file_extension,
				shared.body,
				fetched_at,
				store_allocator,
			),
		)
	}
	_, err := dep_store.put_artifacts(store, profile, artifacts[:], store_allocator)
	when adt.DEPENDENCY_FETCH_TRACE {
		if err == .None {
			trace_eprintf(
				"adt_fetch\tadt\tcache\tok\t%s\t%s\t%d\n",
				object_ref.object_type,
				object_ref.name,
				len(artifacts),
			)
		} else {
			trace_eprintf(
				"adt_fetch\tadt\tcache_err\t%s\t%s\t%v\n",
				object_ref.object_type,
				object_ref.name,
				err,
			)
		}
	} else {
		_ = err
	}
}

dependency_artifact_from_adt :: proc(
	object_ref: ^adt.Object_Ref,
	object_kind, file_extension, source, fetched_at: string,
	allocator: mem.Allocator,
) -> dep_store.Stored_Artifact_Input {
	extension := file_extension
	if dependency_source_is_xml(object_kind, file_extension, source) &&
	   dependency_object_kind_is_ddic(object_kind) {
		extension = "xml"
	}
	return dep_store.Stored_Artifact_Input {
		package_name = object_ref.package_name,
		object_kind = object_kind,
		object_name = object_ref.name,
		object_uri = object_ref.uri,
		object_type = object_ref.object_type,
		description = object_ref.description,
		file_extension = extension,
		source_text = source,
		fetched_at = fetched_at,
	}
}

standalone_dependency_profile :: proc() -> dep_store.Dependency_Profile {
	return dep_store.Dependency_Profile {
		product_version = "adt",
		default_package_version = "default",
	}
}

add_adt_fetched_dependency_input :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	candidate: analyze.Remote_Dependency_Candidate,
	object_ref: ^adt.Object_Ref,
	object_kind: string,
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
	if dependency_source_is_xml(object_kind, file_extension, source) {
		input_source =
			ddic_xml.dependency_source(object_ref.name, object_kind, source, allocator) if candidate.kind == .Type else strings.clone("", allocator)
	} else {
		input_source = strings.clone(source, allocator)
	}
	append_dependency_input(
		candidates,
		dependencies,
		analyze.Source_Input{uri = uri, source = input_source, mode = .Dependency_Interface},
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
	ext := file_extension
	if ext != "" && strings.index_byte(object_ref.uri, '.') < 0 {
		strings.write_byte(&out, '.')
		strings.write_string(&out, ext)
	}
	return strings.to_string(out)
}

append_dependency_input :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	input: analyze.Source_Input,
	candidate: analyze.Remote_Dependency_Candidate,
	object_name: string,
	allocator: mem.Allocator,
) {
	if candidate.kind == .Include {
		append(
			candidates,
			analyze.Project_Candidate_Input {
				input = input,
				object_name = strings.clone(
					object_name if object_name != "" else candidate.name,
					allocator,
				),
			},
		)
	} else {
		append(dependencies, input)
	}
}

project_input_uri_keys :: proc(
	target_uri: string,
	dependencies: []analyze.Source_Input,
	candidates: []analyze.Project_Candidate_Input,
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
			file_name := canonical_name(filepath.base(entry.fullpath), allocator)
			for wanted in file_names {
				if file_name == wanted && !string_list_contains(out^[:], entry.fullpath) {
					append(out, strings.clone(entry.fullpath, allocator))
				}
			}
		}
	}
}

local_export_candidate_file_names :: proc(
	candidate: analyze.Remote_Dependency_Candidate,
	allocator: mem.Allocator,
) -> [dynamic]string {
	names := make([dynamic]string, 0, 2, allocator)
	upper := strings.to_upper(candidate.name, allocator)
	defer delete(upper, allocator)
	encoded := net_url.percent_encode(upper, allocator)
	defer delete(encoded, allocator)
	if encoded == "" {
		return names
	}
	switch candidate.kind {
	case .Include, .Function, .Static, .Report:
		append(&names, local_export_file_name(encoded, "abap", allocator))
	case .Message_Class:
		append(&names, local_export_file_name(encoded, "xml", allocator))
	case .Symbol, .Type:
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

local_export_abap_source_matches :: proc(
	candidate: analyze.Remote_Dependency_Candidate,
	source: string,
) -> bool {
	if candidate.kind != .Static {
		return true
	}
	return source_declares_class_or_interface(source, candidate.name)
}

source_declares_class_or_interface :: proc(source, name: string) -> bool {
	lines := source
	for line in strings.split_lines_iterator(&lines) {
		trimmed := strings.trim_left_space(line)
		if strings.has_prefix(trimmed, "*") || strings.has_prefix(trimmed, "\"") {
			continue
		}
		words := trimmed
		keyword, keyword_ok := strings.fields_iterator(&words)
		decl_name, name_ok := strings.fields_iterator(&words)
		if !keyword_ok || !name_ok || !strings.equal_fold(trim_decl_token(decl_name), name) {
			continue
		}
		if strings.equal_fold(keyword, "INTERFACE") {
			return true
		}
		next, next_ok := strings.fields_iterator(&words)
		if strings.equal_fold(keyword, "CLASS") &&
		   next_ok &&
		   strings.equal_fold(trim_decl_token(next), "DEFINITION") {
			return true
		}
	}
	return false
}

trim_decl_token :: proc(token: string) -> string {
	end := len(token)
	for end > 0 && (token[end - 1] == '.' || token[end - 1] == ':') {
		end -= 1
	}
	return token[:end]
}

dependency_source_is_xml :: proc(object_kind, file_extension, source: string) -> bool {
	if dependency_file_extension_is_xml(file_extension) {
		return true
	}
	if dependency_file_extension_is_abap(file_extension) &&
	   !dependency_object_kind_is_ddic(object_kind) {
		return false
	}
	return strings.has_prefix(source, "<")
}

dependency_object_kind_is_ddic :: proc(object_kind: string) -> bool {
	return len(object_kind) >= 5 && strings.equal_fold(object_kind[:5], "ddic-")
}

dependency_file_extension_is_xml :: proc(file_extension: string) -> bool {
	ext := file_extension
	if strings.has_prefix(ext, ".") {
		ext = ext[1:]
	}
	return strings.equal_fold(ext, "xml")
}

dependency_file_extension_is_abap :: proc(file_extension: string) -> bool {
	ext := file_extension
	if strings.has_prefix(ext, ".") {
		ext = ext[1:]
	}
	return strings.equal_fold(ext, "abap")
}

@(private)
canonical_name :: #force_inline proc(name: string, allocator: mem.Allocator) -> string {
	return strings.to_lower(name, allocator)
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
normalized_uri_path_key :: proc(uri: string, allocator: mem.Allocator) -> string {
	end := len(uri)
	for end > 0 && (uri[end - 1] == '/' || uri[end - 1] == '\\') {
		end -= 1
	}
	out := make([]byte, end, allocator)
	for i in 0 ..< end {
		ch := uri[i]
		if ch == '\\' {
			ch = '/'
		}
		if 'A' <= ch && ch <= 'Z' {
			ch += 'a' - 'A'
		}
		out[i] = ch
	}
	return string(out)
}
