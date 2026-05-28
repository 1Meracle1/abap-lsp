package abap_frontend_semantic_remote_dependencies

import analyze "src:semantic/analyze"

import "src:adt"
import ddic_xml "src:ddic_xml"
import dep_store "src:dependency_store"
import execution "src:execution"

import base_runtime "base:runtime"
import "core:mem"
import "core:strings"
import "core:time"

Adt_Fetched_Object :: struct {
	candidate:   analyze.Remote_Dependency_Candidate,
	object_name: string,
	object_type: string,
	input:       analyze.Source_Input,
	shared:      bool,
}

Adt_Fetch_Task_Result :: struct {
	fetched: [dynamic]Adt_Fetched_Object,
}

Adt_Fetch_Task_Payload :: struct {
	client:           ^adt.Client,
	candidate:        analyze.Remote_Dependency_Candidate,
	store:            ^dep_store.Dependency_Store,
	profile:          ^dep_store.Dependency_Profile,
	connection_key:   string,
	result_allocator: mem.Allocator,
}

add_adt_matches_with_client :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	remote_candidates: []analyze.Remote_Dependency_Candidate,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	client: ^adt.Client,
	pool: ^execution.Pool,
	target_uri: string,
) -> bool {
	uri_keys := project_input_uri_keys(
		target_uri,
		dependencies^[:],
		candidates^[:],
		len(remote_candidates),
		context.temp_allocator,
	)

	if client.csrf_token == "" {
		if adt.ensure_session(client, context.temp_allocator) != .None {
			return false
		}
	}

	added := false
	graph: execution.Graph
	execution.graph_init(&graph, pool, context.temp_allocator)
	result_arenas := make([]mem.Dynamic_Arena, len(remote_candidates), context.temp_allocator)
	tasks := make(
		[dynamic]execution.Task(^Adt_Fetch_Task_Result),
		0,
		len(remote_candidates),
		context.temp_allocator,
	)
	result_backing := base_runtime.heap_allocator()
	connection_key :=
		adt.client_connection_key(client, context.temp_allocator) if store != nil && profile != nil else ""
	for candidate, i in remote_candidates {
		mem.dynamic_arena_init(&result_arenas[i], result_backing, result_backing, alignment = 64)
		payload := Adt_Fetch_Task_Payload {
			client           = client,
			candidate        = candidate,
			store            = store,
			profile          = profile,
			connection_key   = connection_key,
			result_allocator = mem.dynamic_arena_allocator(&result_arenas[i]),
		}
		task := execution.submit_value(
			&graph,
			execution.worker_executor(pool),
			payload,
			adt_fetch_task,
		)
		append(&tasks, task)
	}
	execution.graph_start(&graph)
	for task, i in tasks {
		result := execution.wait(task)
		if add_adt_fetch_task_result(
			candidates,
			dependencies,
			remote_candidates[i],
			result,
			&uri_keys,
			context.temp_allocator,
		) {
			added = true
		}
		mem.dynamic_arena_destroy(&result_arenas[i])
	}
	execution.graph_wait(&graph)
	execution.graph_destroy(&graph)
	return added
}

record_adt_negative_lookup :: proc(
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	connection_key: string,
	candidate: analyze.Remote_Dependency_Candidate,
	allocator: mem.Allocator,
) {
	if store == nil || profile == nil {
		return
	}
	recorded_at, _ := time.time_to_rfc3339(time.now(), allocator = allocator)
	_ = dep_store.record_negative_lookup(
		store,
		profile,
		connection_key,
		candidate.name,
		dep_store_candidate_kind(candidate.kind),
		recorded_at,
		allocator,
	)
}

adt_fetch_task :: proc(payload: Adt_Fetch_Task_Payload) -> ^Adt_Fetch_Task_Result {
	temp_allocator := context.temp_allocator
	result := fetch_adt_candidate(
		payload.client,
		payload.candidate,
		payload.store,
		payload.profile,
		payload.result_allocator,
		temp_allocator,
	)
	if result == nil || len(result.fetched) == 0 {
		record_adt_negative_lookup(
			payload.store,
			payload.profile,
			payload.connection_key,
			payload.candidate,
			temp_allocator,
		)
	}
	return result
}

fetch_adt_candidate :: proc(
	client: ^adt.Client,
	candidate: analyze.Remote_Dependency_Candidate,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	result_allocator: mem.Allocator,
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
			store,
			profile,
			result_allocator,
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
		store,
		profile,
		result_allocator,
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
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	result_allocator: mem.Allocator,
	temp_allocator: mem.Allocator,
	stop_after_first := false,
) -> int {
	fetched_count := 0
	for &object_ref in objects {
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
		store_adt_dependency_fetch(store, profile, &object_ref, &fetched, temp_allocator)
		append_prepared_adt_input(
			result,
			candidate,
			&object_ref,
			fetched.manifest_kind,
			fetched.file_extension,
			fetched.body,
			false,
			result_allocator,
			temp_allocator,
		)
		for &shared in fetched.shared_dependencies {
			append_prepared_adt_input(
				result,
				analyze.Remote_Dependency_Candidate {
					name = shared.object_ref.name,
					kind = .Include,
				},
				&shared.object_ref,
				shared.manifest_kind,
				shared.file_extension,
				shared.body,
				true,
				result_allocator,
				temp_allocator,
			)
		}
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
	uri_keys: ^map[string]bool,
	temp_allocator: mem.Allocator,
) -> bool {
	if result == nil {
		return false
	}
	added := false
	for &entry in result.fetched {
		input_added := add_prepared_adt_dependency_input(
			candidates,
			dependencies,
			entry.candidate,
			entry.input,
			entry.object_name,
			uri_keys,
		)
		when adt.DEPENDENCY_FETCH_TRACE {
			status := "added" if input_added else "skipped"
			if entry.shared {
				trace_eprintf(
					"adt_fetch\tadt\tshared_input\t%s\t%s\t%s\n",
					status,
					entry.object_type,
					entry.object_name,
				)
			} else {
				trace_eprintf(
					"adt_fetch\tadt\tinput\t%s\t%s\t%s\t%s\n",
					status,
					remote_candidate_kind_text(candidate.kind),
					entry.object_type,
					entry.object_name,
				)
			}
		}
		if input_added {
			added = true
		}
	}
	return added
}

append_prepared_adt_input :: proc(
	result: ^Adt_Fetch_Task_Result,
	candidate: analyze.Remote_Dependency_Candidate,
	object_ref: ^adt.Object_Ref,
	object_kind, file_extension, source: string,
	shared: bool,
	result_allocator: mem.Allocator,
	temp_allocator: mem.Allocator,
) {
	input_source: string
	if dependency_source_is_xml(object_kind, file_extension, source) {
		if candidate.kind == .Type {
			formatted_source := ddic_xml.dependency_source(
				object_ref.name,
				object_kind,
				source,
				temp_allocator,
			)
			input_source = formatted_source
		}
	} else {
		input_source = source
	}
	prepared_candidate := candidate
	prepared_candidate.name = strings.clone(candidate.name, result_allocator)
	append(
		&result.fetched,
		Adt_Fetched_Object {
			candidate = prepared_candidate,
			object_name = strings.clone(object_ref.name, result_allocator),
			object_type = strings.clone(object_ref.object_type, result_allocator),
			input = analyze.Source_Input {
				uri = adt_dependency_uri(object_ref, file_extension, result_allocator),
				source = strings.clone(input_source, result_allocator),
				mode = .Dependency_Interface,
			},
			shared = shared,
		},
	)
}

store_adt_dependency_fetch :: proc(
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	object_ref: ^adt.Object_Ref,
	fetched: ^adt.Dependency_Fetch_Result,
	temp_allocator: mem.Allocator,
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
	artifacts := make(
		[dynamic]dep_store.Stored_Artifact_Input,
		0,
		1 + len(fetched.shared_dependencies),
		temp_allocator,
	)
	fetched_at, _ := time.time_to_rfc3339(time.now(), allocator = temp_allocator)
	append(
		&artifacts,
		dependency_artifact_from_adt(
			object_ref,
			fetched.manifest_kind,
			fetched.file_extension,
			fetched.body,
			fetched_at,
			temp_allocator,
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
				temp_allocator,
			),
		)
	}
	_, err := dep_store.put_artifacts(store, profile, artifacts[:], temp_allocator)
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
	allocator: mem.Allocator,
) -> bool {
	uri := adt_dependency_uri(object_ref, file_extension, allocator)
	return add_dependency_source_input(
		candidates,
		dependencies,
		candidate,
		uri,
		object_ref.name,
		object_kind,
		file_extension,
		source,
		uri_keys,
		allocator,
	)
}

add_prepared_adt_dependency_input :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	candidate: analyze.Remote_Dependency_Candidate,
	input: analyze.Source_Input,
	object_name: string,
	uri_keys: ^map[string]bool,
) -> bool {
	if !project_input_uri_key_add_if_missing(uri_keys, input.uri) {
		return false
	}
	append_dependency_input(candidates, dependencies, input, candidate, object_name)
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
