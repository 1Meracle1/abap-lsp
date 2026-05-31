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
	record:   dep_store.Stored_Artifact_Record,
	input:    analyze.Source_Input,
	ok:       bool,
	negative: bool,
	err:      dep_store.Store_Error,
}

Dependency_Store_Task_Payload :: struct {
	store:            dep_store.Dependency_Store,
	profile:          ^dep_store.Dependency_Profile,
	candidate:        deps.Remote_Dependency_Candidate,
	any_profile:      bool,
	connection_key:   string,
	result_allocator: mem.Allocator,
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
	) {
		result.added = true
	}

	graph: execution.Graph
	execution.graph_init(&graph, pool, context.temp_allocator)
	result_arenas := make([]mem.Dynamic_Arena, len(remote_candidates), context.temp_allocator)
	tasks := make(
		[dynamic]execution.Task(^Dependency_Store_Task_Result),
		0,
		len(remote_candidates),
		context.temp_allocator,
	)
	result_backing := base_runtime.heap_allocator()
	for candidate, i in remote_candidates {
		mem.dynamic_arena_init(&result_arenas[i], result_backing, result_backing, alignment = 64)
		payload := Dependency_Store_Task_Payload {
			store            = store^,
			profile          = profile,
			candidate        = candidate,
			any_profile      = any_profile,
			connection_key   = connection_key,
			result_allocator = mem.dynamic_arena_allocator(&result_arenas[i]),
		}
		task := execution.submit_value(
			&graph,
			execution.worker_executor(pool),
			payload,
			dependency_store_find_task,
		)
		append(&tasks, task)
	}
	execution.graph_start(&graph)
	for task, i in tasks {
		task_result := execution.wait(task)

		candidate := remote_candidates[i]
		if add_dependency_store_task_result(
			candidates,
			dependencies,
			candidate,
			task_result,
			seen_artifacts,
			trace_source,
			&uri_keys,
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
	execution.graph_wait(&graph)
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
		if record.artifact_id in seen_artifacts^ ||
		   typepool_source_has_pending_expansion(record.source_text, context.temp_allocator) {
			continue
		}
		candidate := deps.Remote_Dependency_Candidate{name = record.object_name, kind = .Type}
		input := source_input_from_dependency_record(&record, candidate, context.temp_allocator)
		if !project_input_uri_key_add_if_missing(uri_keys, input.uri) {
			continue
		}
		seen_artifacts^[record.artifact_id] = true
		append_dependency_input(candidates, dependencies, input, candidate, record.object_name)
		added = true
		when adt.DEPENDENCY_FETCH_TRACE {
			trace_eprintf(
				"adt_fetch\t%s\tadd\ttypepool\t%s\n",
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

dependency_store_find_task :: proc(
	payload: Dependency_Store_Task_Payload,
) -> ^Dependency_Store_Task_Result {
	result_allocator := payload.result_allocator
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
			result.input = source_input_from_dependency_record(
				&result.record,
				payload.candidate,
				result_allocator,
			)
		}
	} else if payload.profile != nil {
		status, err := dep_store.reader_find_cached_candidate(
			&reader,
			payload.profile,
			payload.connection_key,
			payload.candidate.name,
			dep_store_candidate_kind(payload.candidate.kind),
			context.temp_allocator,
		)
		result.err = err
		if err == .None && status == .Negative {
			result.negative = true
		} else if err == .None && status == .Artifact {
			record, ok, lookup_err := dep_store.reader_find_artifact_for_candidate(
				&reader,
				payload.profile,
				payload.candidate.name,
				dep_store_candidate_kind(payload.candidate.kind),
				context.temp_allocator,
			)
			result.ok = ok
			result.err = lookup_err
			if ok && lookup_err == .None {
				result.record = clone_dependency_record(&record, result_allocator)
				result.input = source_input_from_dependency_record(
					&result.record,
					payload.candidate,
					result_allocator,
				)
			}
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
	candidate: deps.Remote_Dependency_Candidate,
	result: ^Dependency_Store_Task_Result,
	seen_artifacts: ^map[i64]bool,
	trace_source: string,
	uri_keys: ^map[string]bool,
) -> bool {
	if result == nil ||
	   result.err != .None ||
	   !result.ok ||
	   result.record.artifact_id in seen_artifacts^ {
		return false
	}
	seen_artifacts^[result.record.artifact_id] = true
	if !project_input_uri_key_add_if_missing(uri_keys, result.input.uri) {
		return false
	}
	append_dependency_input(
		candidates,
		dependencies,
		result.input,
		candidate,
		result.record.object_name,
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
		mode   = .Dependency_Interface,
	}
	return input
}
