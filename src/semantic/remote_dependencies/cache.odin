package abap_frontend_semantic_remote_dependencies

import "src:adt"
import dep_store "src:dependency_store"
import execution "src:execution"
import analyze "src:semantic/analyze"
import deps "src:semantic/dependencies"

import base_runtime "base:runtime"
import "core:mem"
import "core:strings"

Dependency_Store_Task_Result :: struct {
	record:          dep_store.Stored_Artifact_Record,
	input:           analyze.Source_Input,
	summary_payload: string,
	ok:              bool,
	has_input:       bool,
	negative:        bool,
	err:             dep_store.Store_Error,
}

Dependency_Store_Batch_Payload :: struct {
	store:          dep_store.Dependency_Store,
	profile:        ^dep_store.Dependency_Profile,
	candidates:     []deps.Remote_Dependency_Candidate,
	any_profile:    bool,
	connection_key: string,
	prefer_summary: bool,
	result_arenas:  []mem.Dynamic_Arena,
	results:        []^Dependency_Store_Task_Result,
	offset:         int,
}

add_dependency_cache_matches :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	remote_candidates: []deps.Remote_Dependency_Candidate,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	any_profile: bool,
	connection_key: string,
	seen_artifacts: ^map[i64]bool,
	pool: ^execution.Pool,
	target_uri: string,
	trace_source: string,
	dependency_summaries: ^[dynamic]analyze.Summary_Provider_Input = nil,
) -> Cache_Phase_Result {
	result := Cache_Phase_Result {
		adt_candidates   = make(
			[dynamic]deps.Remote_Dependency_Candidate,
			0,
			len(remote_candidates),
			context.temp_allocator,
		),
		local_candidates = make(
			[dynamic]deps.Remote_Dependency_Candidate,
			0,
			len(remote_candidates),
			context.temp_allocator,
		),
	}
	uri_keys := project_input_uri_keys(
		target_uri,
		dependencies^[:],
		candidates^[:],
		len(remote_candidates),
		context.temp_allocator,
	)
	if add_typepool_cache_matches(
		candidates,
		dependencies,
		remote_candidates,
		store,
		profile,
		any_profile,
		seen_artifacts,
		trace_source,
		&uri_keys,
		dependency_summaries,
	) {
		result.added = true
	}

	if len(remote_candidates) == 0 {
		return result
	}

	task_count := min(max(pool.options.worker_count, 1), len(remote_candidates))
	batch_size := (len(remote_candidates) + task_count - 1) / task_count
	// Worker result arenas outlive their task until the main thread consumes them.
	// Keep them heap-backed so a temp allocator reset cannot invalidate results.
	result_backing := base_runtime.heap_allocator()
	result_arenas := make([]mem.Dynamic_Arena, len(remote_candidates), context.temp_allocator)
	results := make(
		[]^Dependency_Store_Task_Result,
		len(remote_candidates),
		context.temp_allocator,
	)
	for &result_arena in result_arenas {
		mem.dynamic_arena_init(&result_arena, result_backing, result_backing, alignment = 64)
	}

	graph: execution.Graph
	execution.graph_init(&graph, pool, context.temp_allocator)
	tasks := make(
		[dynamic]execution.Task(execution.No_Result),
		0,
		task_count,
		context.temp_allocator,
	)
	for start := 0; start < len(remote_candidates); start += batch_size {
		end := min(start + batch_size, len(remote_candidates))
		payload := Dependency_Store_Batch_Payload {
			store          = store^,
			profile        = profile,
			candidates     = remote_candidates[start:end],
			any_profile    = any_profile,
			connection_key = connection_key,
			prefer_summary = dependency_summaries != nil,
			result_arenas  = result_arenas,
			results        = results,
			offset         = start,
		}
		task := execution.submit_value(
			&graph,
			execution.worker_executor(pool),
			payload,
			dependency_store_find_batch_task,
		)
		append(&tasks, task)
	}
	execution.graph_start(&graph)
	execution.graph_wait(&graph)
	for candidate, i in remote_candidates {
		task_result := results[i]

		if add_dependency_store_task_result(
			candidates,
			dependencies,
			candidate,
			task_result,
			seen_artifacts,
			trace_source,
			&uri_keys,
			dependency_summaries,
		) {
			result.added = true
		} else if task_result == nil || task_result.err != .None {
			append(&result.adt_candidates, candidate)
			append(&result.local_candidates, candidate)
		} else if !task_result.ok {
			if !task_result.negative {
				append(&result.adt_candidates, candidate)
			}
			append(&result.local_candidates, candidate)
		}
		mem.dynamic_arena_destroy(&result_arenas[i])
	}
	execution.graph_destroy(&graph)
	return result
}

add_typepool_cache_matches :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	remote_candidates: []deps.Remote_Dependency_Candidate,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	any_profile: bool,
	seen_artifacts: ^map[i64]bool,
	trace_source: string,
	uri_keys: ^map[string]bool,
	dependency_summaries: ^[dynamic]analyze.Summary_Provider_Input = nil,
) -> bool {
	names := make([dynamic]string, 0, len(remote_candidates), context.temp_allocator)
	for candidate in remote_candidates {
		if candidate.kind == .Type || candidate.kind == .Symbol {
			append(&names, candidate.name)
		}
	}
	if len(names) == 0 {
		return false
	}
	backfill_typepool_symbol_cache(store, profile, any_profile)
	records: [dynamic]dep_store.Stored_Artifact_Record
	err: dep_store.Store_Error
	if any_profile {
		records, err = dep_store.find_typepool_artifacts_for_symbols_any_profile(
			store,
			names[:],
			context.temp_allocator,
		)
	} else if profile != nil {
		records, err = dep_store.find_typepool_artifacts_for_symbols(
			store,
			profile,
			names[:],
			context.temp_allocator,
		)
	} else {
		return false
	}
	if err != .None {
		return false
	}
	added := false
	for &record in records {
		if record.artifact_id in seen_artifacts^ {
			continue
		}
		candidate := deps.Remote_Dependency_Candidate{name = record.object_name, kind = .Type}
		if dependency_summaries != nil {
			if summary_payload, summary_ok, summary_err := dep_store.read_artifact_summary_payload(
			   store,
			   record.artifact_id,
			   context.temp_allocator,
			   );
			   summary_err == .None {
				if !summary_ok {
					summary_payload = dependency_interface_summary_payload_from_artifact(
						record.object_kind,
						record.object_name,
						record.object_uri,
						record.object_type,
						record.file_extension,
						record.source_text,
						context.temp_allocator,
					)
				}
				summary_added := false
				for pending in remote_candidates {
					if summary_input, ok := dependency_summary_input_from_payload(
						   summary_payload,
						   pending,
						   dependency_record_summary_uri(&record, context.temp_allocator),
						   dependency_summaries.allocator,
					   );
					   ok {
						append(dependency_summaries, summary_input)
						seen_artifacts^[record.artifact_id] = true
						added = true
						summary_added = true
						break
					}
				}
				if summary_added {
					continue
				}
			}
			continue
		}
		if typepool_source_has_pending_expansion(record.source_text, context.temp_allocator) {
			continue
		}
		input := source_input_from_dependency_record(&record, candidate, context.temp_allocator)
		if !project_input_uri_key_add_if_missing(uri_keys, input.uri) {
			continue
		}
		seen_artifacts^[record.artifact_id] = true
		append_dependency_input(candidates, dependencies, input, candidate, record.object_name)
		added = true
		when adt.DEPENDENCY_FETCH_TRACE {
			trace_eprintf(
				"[dep fetch] Cache hit from %s: type-pool %s\n",
				trace_source,
				record.object_name,
			)
		}
	}
	return added
}

backfill_typepool_symbol_cache :: proc(
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	any_profile: bool,
) {
	records: [dynamic]dep_store.Stored_Artifact_Record
	err: dep_store.Store_Error
	if any_profile {
		records, err = dep_store.list_unindexed_typepool_artifacts_any_profile(
			store,
			context.temp_allocator,
		)
	} else if profile != nil {
		records, err = dep_store.list_unindexed_typepool_artifacts(
			store,
			profile,
			context.temp_allocator,
		)
	} else {
		return
	}
	if err != .None {
		return
	}
	for &record in records {
		symbols := typepool_source_symbols(record.source_text, context.temp_allocator)
		_ = dep_store.put_typepool_symbols(
			store,
			record.artifact_id,
			record.object_name,
			symbols[:],
			context.temp_allocator,
		)
	}
}

dependency_store_find_batch_task :: proc(
	payload: Dependency_Store_Batch_Payload,
) -> execution.No_Result {
	store := payload.store
	reader, reader_err := dep_store.reader(&store, context.temp_allocator)
	if reader_err != .None {
		for _, i in payload.candidates {
			index := payload.offset + i
			result := new(
				Dependency_Store_Task_Result,
				mem.dynamic_arena_allocator(&payload.result_arenas[index]),
			)
			result.err = reader_err
			payload.results[index] = result
		}
		return execution.No_Result{}
	}
	defer dep_store.reader_destroy(&reader)
	for candidate, i in payload.candidates {
		index := payload.offset + i
		payload.results[index] = dependency_store_find(
			&reader,
			payload.profile,
			candidate,
			payload.any_profile,
			payload.connection_key,
			payload.prefer_summary,
			mem.dynamic_arena_allocator(&payload.result_arenas[index]),
		)
	}
	return execution.No_Result{}
}

dependency_store_find :: proc(
	reader: ^dep_store.Dependency_Store_Reader,
	profile: ^dep_store.Dependency_Profile,
	candidate: deps.Remote_Dependency_Candidate,
	any_profile: bool,
	connection_key: string,
	prefer_summary: bool,
	result_allocator: mem.Allocator,
) -> ^Dependency_Store_Task_Result {
	result := new(Dependency_Store_Task_Result, result_allocator)
	if any_profile {
		record, ok, err := dep_store.reader_find_artifact_for_candidate_any_profile(
			reader,
			candidate.name,
			dep_store_candidate_kind(candidate.kind),
			context.temp_allocator,
		)
		result.ok = ok
		result.err = err
		if ok && err == .None {
			if cached_dependency_record_is_stale(&record, candidate) {
				result.ok = false
			} else {
				dependency_store_prepare_record_result(result, &record, candidate, prefer_summary, reader, result_allocator)
			}
		}
	} else if profile != nil {
		status, err := dep_store.reader_find_cached_candidate(
			reader,
			profile,
			connection_key,
			candidate.name,
			dep_store_candidate_kind(candidate.kind),
			context.temp_allocator,
		)
		result.err = err
		if err == .None && status == .Negative {
			result.negative = true
		} else if err == .None && status == .Artifact {
			record, ok, lookup_err := dep_store.reader_find_artifact_for_candidate(
				reader,
				profile,
				candidate.name,
				dep_store_candidate_kind(candidate.kind),
				context.temp_allocator,
			)
			result.ok = ok
			result.err = lookup_err
			if ok && lookup_err == .None {
				if cached_dependency_record_is_stale(&record, candidate) {
					result.ok = false
				} else {
					dependency_store_prepare_record_result(result, &record, candidate, prefer_summary, reader, result_allocator)
				}
			}
		}
	}
	return result
}

dependency_store_prepare_record_result :: proc(
	result: ^Dependency_Store_Task_Result,
	record: ^dep_store.Stored_Artifact_Record,
	candidate: deps.Remote_Dependency_Candidate,
	prefer_summary: bool,
	reader: ^dep_store.Dependency_Store_Reader,
	result_allocator: mem.Allocator,
) {
	summary_payload, summary_ok, _ := dep_store.reader_read_artifact_summary_payload(
		reader,
		record.artifact_id,
		result_allocator,
	)
	if prefer_summary {
		if !summary_ok {
			summary_payload = dependency_interface_summary_payload_from_artifact(
				record.object_kind,
				record.object_name,
				record.object_uri,
				record.object_type,
				record.file_extension,
				record.source_text,
				result_allocator,
			)
		}
		if dependency_summary_payload_satisfies_candidate(summary_payload, candidate) {
			result.record = clone_dependency_record_metadata(record, result_allocator)
			result.summary_payload = summary_payload
			return
		}
		result.record = clone_dependency_record(record, result_allocator)
		result.summary_payload = summary_payload
		result.input = source_input_from_dependency_record(
			&result.record,
			candidate,
			result_allocator,
		)
		result.has_input = true
		return
	}

	result.record = clone_dependency_record(record, result_allocator)
	result.summary_payload = summary_payload
	result.input = source_input_from_dependency_record(
		&result.record,
		candidate,
		result_allocator,
	)
	result.has_input = true
}

cached_dependency_record_is_stale :: proc(
	record: ^dep_store.Stored_Artifact_Record,
	candidate: deps.Remote_Dependency_Candidate,
) -> bool {
	return candidate.kind == .Type &&
	       dependency_object_kind_is_ddic(record.object_kind) &&
	       dependency_source_is_xml(record.object_kind, record.file_extension, record.source_text) &&
	       strings.contains(record.source_text, "ddicIncludeName")
}

clone_dependency_record :: proc(
	record: ^dep_store.Stored_Artifact_Record,
	allocator: mem.Allocator,
) -> dep_store.Stored_Artifact_Record {
	out := clone_dependency_record_metadata(record, allocator)
	out.source_text = strings.clone(record.source_text, allocator)
	return out
}

clone_dependency_record_metadata :: proc(
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
	}
}

add_dependency_store_task_result :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	candidate: deps.Remote_Dependency_Candidate,
	result: ^Dependency_Store_Task_Result,
	seen_artifacts: ^map[i64]bool,
	trace_source: string,
	uri_keys: ^map[string]bool,
	dependency_summaries: ^[dynamic]analyze.Summary_Provider_Input = nil,
) -> bool {
	if result == nil ||
	   result.err != .None ||
	   !result.ok ||
	   result.record.artifact_id in seen_artifacts^ {
		return false
	}
	if dependency_summaries != nil && result.summary_payload != "" {
		if summary_input, ok := dependency_summary_input_from_payload(
			   result.summary_payload,
			   candidate,
			   dependency_record_summary_uri(&result.record, context.temp_allocator),
			   dependency_summaries.allocator,
		   );
		   ok {
			append(dependency_summaries, summary_input)
			seen_artifacts^[result.record.artifact_id] = true
			when adt.DEPENDENCY_FETCH_TRACE {
				trace_eprintf(
					"[dep fetch] Cache summary hit from %s: %s %s -> %s %s\n",
					trace_source,
					remote_candidate_kind_text(candidate.kind),
					candidate.name,
					result.record.object_kind,
					result.record.object_name,
				)
			}
			return true
		}
	}
	if !result.has_input {
		return false
	}
	if !project_input_uri_key_add_if_missing(uri_keys, result.input.uri) {
		return false
	}
	seen_artifacts^[result.record.artifact_id] = true
	input_candidate := candidate
	if strings.equal_fold(result.record.object_kind, "include") {
		input_candidate.name = result.record.object_name
		input_candidate.kind = .Include
		input_candidate.hint = .None
	}
	append_dependency_input(
		candidates,
		dependencies,
		result.input,
		input_candidate,
		result.record.object_name,
	)
	when adt.DEPENDENCY_FETCH_TRACE {
		trace_eprintf(
			"[dep fetch] Cache hit from %s: %s %s -> %s %s\n",
			trace_source,
			remote_candidate_kind_text(candidate.kind),
			candidate.name,
			result.record.object_kind,
			result.record.object_name,
		)
	}
	return true
}

dependency_record_uri :: proc(
	record: ^dep_store.Stored_Artifact_Record,
	allocator: mem.Allocator,
) -> string {
	if strings.equal_fold(record.object_kind, TYPEPOOL_OBJECT_KIND) {
		return typepool_dependency_uri(record.object_name, allocator)
	}
	out := strings.builder_make(allocator)
	strings.write_string(&out, "abapls-cache:/")
	strings.write_string(&out, record.object_kind)
	strings.write_byte(&out, '/')
	strings.write_string(&out, record.object_name)
	strings.write_string(&out, ".abap")
	return strings.to_string(out)
}

dependency_record_summary_uri :: proc(
	record: ^dep_store.Stored_Artifact_Record,
	allocator: mem.Allocator,
) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "abapls-summary:/")
	if strings.equal_fold(record.object_kind, TYPEPOOL_OBJECT_KIND) {
		strings.write_string(&out, "type-pool")
	} else {
		strings.write_string(&out, strings.to_lower(record.object_kind, allocator))
	}
	strings.write_byte(&out, '/')
	strings.write_string(&out, strings.to_lower(record.object_name, allocator))
	return strings.to_string(out)
}

source_input_from_dependency_record :: proc(
	record: ^dep_store.Stored_Artifact_Record,
	candidate: deps.Remote_Dependency_Candidate,
	allocator: mem.Allocator,
) -> analyze.Source_Input {
	uri := dependency_record_uri(record, allocator)
	source := dependency_input_source(
		candidate,
		record.object_name,
		record.object_kind,
		record.file_extension,
		record.source_text,
		allocator,
	)
	input := analyze.Source_Input {
		uri    = uri,
		source = source,
		role = .Dependency_Interface_Source,
	}
	return input
}
