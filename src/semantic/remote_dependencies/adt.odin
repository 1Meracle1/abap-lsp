package abap_frontend_semantic_remote_dependencies

import analyze "src:semantic/analyze"

import "src:adt"
import ddic_xml "src:ddic_xml"
import dep_store "src:dependency_store"
import execution "src:execution"
import deps "src:semantic/dependencies"

import base_runtime "base:runtime"
import "core:mem"
import "core:strings"
import "core:time"

Adt_Fetched_Object :: struct {
	candidate:       deps.Remote_Dependency_Candidate,
	object_name:     string,
	object_type:     string,
	summary_payload: string,
	input:           analyze.Source_Input,
	has_input:       bool,
	shared:          bool,
}

Adt_Fetch_Task_Result :: struct {
	fetched: [dynamic]Adt_Fetched_Object,
}

Adt_Fetch_Task_Payload :: struct {
	client:           ^adt.Client,
	candidate:        deps.Remote_Dependency_Candidate,
	store:            ^dep_store.Dependency_Store,
	profile:          ^dep_store.Dependency_Profile,
	connection_key:   string,
	prefer_summary:   bool,
	result_allocator: mem.Allocator,
}

add_adt_matches_with_client :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	remote_candidates: []deps.Remote_Dependency_Candidate,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	client: ^adt.Client,
	pool: ^execution.Pool,
	target_uri: string,
	dependency_summaries: ^[dynamic]analyze.Summary_Provider_Input = nil,
) -> bool {
	adt_candidates := make(
		[dynamic]deps.Remote_Dependency_Candidate,
		0,
		len(remote_candidates),
		context.temp_allocator,
	)
	for candidate in remote_candidates {
		if candidate.kind != .Symbol {
			append(&adt_candidates, candidate)
		}
	}
	if len(adt_candidates) == 0 {
		return false
	}
	uri_keys := project_input_uri_keys(
		target_uri,
		dependencies^[:],
		candidates^[:],
		len(adt_candidates),
		context.temp_allocator,
	)

	if client.csrf_token == "" {
		if adt.ensure_session(client, context.temp_allocator) != .None {
			when TRACE {
				for candidate in adt_candidates {
					trace_eprintf(
						"[dep fetch] ADT miss: %s %s (session setup failed)\n",
						remote_candidate_kind_text(candidate.kind),
						candidate.name,
					)
				}
			}
			return false
		}
	}

	added := false
	graph: execution.Graph
	execution.graph_init(&graph, pool, context.temp_allocator)
	result_arenas := make([]mem.Dynamic_Arena, len(adt_candidates), context.temp_allocator)
	tasks := make(
		[dynamic]execution.Task(^Adt_Fetch_Task_Result),
		0,
		len(adt_candidates),
		context.temp_allocator,
	)
	result_backing := base_runtime.heap_allocator()
	connection_key :=
		adt.client_connection_key(client, context.temp_allocator) if store != nil && profile != nil else ""
	for candidate, i in adt_candidates {
		mem.dynamic_arena_init(&result_arenas[i], result_backing, result_backing, alignment = 64)
		payload := Adt_Fetch_Task_Payload {
			client           = client,
			candidate        = candidate,
			store            = store,
			profile          = profile,
			connection_key   = connection_key,
			prefer_summary   = dependency_summaries != nil,
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
			adt_candidates[i],
			result,
			&uri_keys,
			context.temp_allocator,
			dependency_summaries,
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
	candidate: deps.Remote_Dependency_Candidate,
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
		payload.prefer_summary,
		payload.result_allocator,
		temp_allocator,
	)
	if result == nil || len(result.fetched) == 0 {
		when TRACE {
			trace_eprintf(
				"[dep fetch] ADT miss: %s %s\n",
				remote_candidate_kind_text(payload.candidate.kind),
				payload.candidate.name,
			)
		}
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
	candidate: deps.Remote_Dependency_Candidate,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	prefer_summary: bool,
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
			prefer_summary,
			result_allocator,
			temp_allocator,
			true,
		)
		if count_fetched > 0 {
			return result
		}
	}

	objects, err := adt.search_repository_objects(client, candidate.name, 50, temp_allocator)
	if err != .None {
		objects = adt.direct_dependency_object_refs(
			candidate.name,
			remote_candidate_kind_text(candidate.kind),
			temp_allocator,
		)
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

	fetch_adt_objects(
		client,
		candidate,
		selected[:],
		result,
		store,
		profile,
		prefer_summary,
		result_allocator,
		temp_allocator,
	)
	return result
}

adt_candidate_direct_first :: proc(candidate: deps.Remote_Dependency_Candidate) -> bool {
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
	candidate: deps.Remote_Dependency_Candidate,
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
	candidate: deps.Remote_Dependency_Candidate,
	objects: []adt.Object_Ref,
	result: ^Adt_Fetch_Task_Result,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	prefer_summary: bool,
	result_allocator: mem.Allocator,
	temp_allocator: mem.Allocator,
	stop_after_first := false,
) -> int {
	fetched_count := 0
	for &object_ref in objects {
		fetched, fetch_err := adt.fetch_dependency_object(client, &object_ref, temp_allocator)
		if fetch_err != .None {
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
		when TRACE {
			trace_eprintf(
				"[dep fetch] ADT hit: %s %s -> %s %s (type=%s, ext=%s, bytes=%d, shared=%d)\n",
				remote_candidate_kind_text(candidate.kind),
				candidate.name,
				fetched.manifest_kind,
				object_ref.name,
				object_ref.object_type,
				fetched.file_extension,
				len(fetched.body),
				len(fetched.shared_dependencies),
			)
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
			prefer_summary,
		)
		for &shared in fetched.shared_dependencies {
			when TRACE {
				trace_eprintf(
					"[dep fetch] ADT hit: include %s -> %s %s (type=%s, ext=%s, bytes=%d, shared from %s)\n",
					shared.object_ref.name,
					shared.manifest_kind,
					shared.object_ref.name,
					shared.object_ref.object_type,
					shared.file_extension,
					len(shared.body),
					object_ref.name,
				)
			}
			append_prepared_adt_input(
				result,
				deps.Remote_Dependency_Candidate {
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
				prefer_summary,
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
	candidate: deps.Remote_Dependency_Candidate,
	result: ^Adt_Fetch_Task_Result,
	uri_keys: ^map[string]bool,
	temp_allocator: mem.Allocator,
	dependency_summaries: ^[dynamic]analyze.Summary_Provider_Input = nil,
) -> bool {
	if result == nil {
		return false
	}
	added := false
	for &entry in result.fetched {
		input_added := false
		if dependency_summaries != nil && entry.summary_payload != "" {
			if summary_input, ok := dependency_summary_input_from_payload(
				   entry.summary_payload,
				   entry.candidate,
				   adt_summary_dependency_uri(entry.input.uri, context.temp_allocator),
				   dependency_summaries.allocator,
			   );
			   ok {
				append(dependency_summaries, summary_input)
				input_added = true
			}
		}
		if dependency_summaries != nil && !input_added {
			continue
		}
		if !input_added {
			if !entry.has_input {
				continue
			}
			input_added = add_prepared_adt_dependency_input(
				candidates,
				dependencies,
				entry.candidate,
				entry.input,
				entry.object_name,
				uri_keys,
			)
		}
		if input_added {
			added = true
		}
	}
	return added
}

append_prepared_adt_input :: proc(
	result: ^Adt_Fetch_Task_Result,
	candidate: deps.Remote_Dependency_Candidate,
	object_ref: ^adt.Object_Ref,
	object_kind, file_extension, source: string,
	shared: bool,
	result_allocator: mem.Allocator,
	temp_allocator: mem.Allocator,
	prefer_summary := false,
) {
	summary_payload := dependency_interface_summary_payload_from_artifact(
		object_kind,
		object_ref.name,
		object_ref.uri,
		object_ref.object_type,
		file_extension,
		source,
		result_allocator,
	)
	use_summary := prefer_summary &&
	               dependency_summary_payload_satisfies_candidate(summary_payload, candidate)
	input_source: string
	if !use_summary {
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
	}
	prepared_candidate := candidate
	prepared_candidate.name = strings.clone(candidate.name, result_allocator)
	append(
		&result.fetched,
		Adt_Fetched_Object {
			candidate = prepared_candidate,
			object_name = strings.clone(object_ref.name, result_allocator),
			object_type = strings.clone(object_ref.object_type, result_allocator),
			summary_payload = summary_payload,
			input = analyze.Source_Input {
				uri = adt_dependency_uri(object_ref, file_extension, result_allocator),
				source = strings.clone(input_source, result_allocator),
				role = .Dependency_Interface_Source,
			},
			has_input = !use_summary,
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
	_, _ = dep_store.put_artifacts(store, profile, artifacts[:], temp_allocator)
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
		package_name     = object_ref.package_name,
		object_kind      = object_kind,
		object_name      = object_ref.name,
		object_uri       = object_ref.uri,
		object_type      = object_ref.object_type,
		description      = object_ref.description,
		file_extension   = extension,
		source_text      = source,
		fetched_at       = fetched_at,
		summary_payload  = dependency_interface_summary_payload_from_artifact(
			object_kind,
			object_ref.name,
			object_ref.uri,
			object_ref.object_type,
			extension,
			source,
			allocator,
		),
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
	candidate: deps.Remote_Dependency_Candidate,
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
	candidate: deps.Remote_Dependency_Candidate,
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

adt_summary_dependency_uri :: proc(uri: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "abapls-summary:/adt/")
	strings.write_string(&out, uri)
	return strings.to_string(out)
}
