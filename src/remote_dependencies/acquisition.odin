package abap_frontend_remote_dependencies

import "src:adt"
import dep_store "src:dependency_store"
import execution "src:execution"
import "src:utils"

import base_runtime "base:runtime"
import "core:mem"
import "core:mem/virtual"
import filepath "core:path/filepath"
import "core:strings"
import "core:time"

ADT_Fetch_Task_Payload :: struct {
	client:           ^adt.Client,
	request:          Request,
	store:            ^dep_store.Dependency_Store,
	profile:          ^dep_store.Dependency_Profile,
	result_allocator: mem.Allocator,
}

ADT_Probe_Task_Payload :: struct {
	client:           ^adt.Client,
	result_allocator: mem.Allocator,
}

ADT_Probe_Result :: struct {
	bootstrap: adt.Session_Bootstrap,
	err:       adt.Error,
}

Cache_Lookup_Result :: struct {
	record: dep_store.Stored_Artifact_Record,
	ok:     bool,
	err:    dep_store.Store_Error,
}

Cache_Batch_Payload :: struct {
	store:         dep_store.Dependency_Store,
	profile:       ^dep_store.Dependency_Profile,
	requests:      []Request,
	any_profile:  bool,
	result_arenas: []mem.Dynamic_Arena,
	results:       []^Cache_Lookup_Result,
	offset:        int,
}

Cache_Prepare_Status :: enum {
	None,
	Error,
	Miss,
	Stale,
	Hit,
}

Cache_Prepare_Task_Payload :: struct {
	store:            dep_store.Dependency_Store,
	profile:          ^dep_store.Dependency_Profile,
	request:          Request,
	any_profile:     bool,
	result_allocator: mem.Allocator,
}

Cache_Prepare_Result :: struct {
	request:  Request,
	status:   Cache_Prepare_Status,
	err:      dep_store.Store_Error,
	record:   dep_store.Stored_Artifact_Record,
	prepared: Prepared_Artifact,
}

Typepool_Cache_Record_Status :: enum {
	None,
	Ready,
	Needs_Expansion,
}

Typepool_Cache_Record_Task_Payload :: struct {
	record:           dep_store.Stored_Artifact_Record,
	requests:         []Request,
	result_allocator: mem.Allocator,
}

Typepool_Cache_Record_Result :: struct {
	status:      Typepool_Cache_Record_Status,
	artifact_id: i64,
	object_name: string,
	artifacts:   [dynamic]Artifact,
}

Typepool_Cache_Record_Task :: struct {
	task:        execution.Task(Typepool_Cache_Record_Result),
	arena_index: int,
}

Local_Task_Payload :: struct {
	request:            Request,
	local_export_roots: []string,
	result_allocator:   mem.Allocator,
}

Local_Task_Result :: struct {
	artifact: Artifact,
	ok:       bool,
}

Local_Prepare_Task_Result :: struct {
	prepared: Prepared_Artifact,
	ok:       bool,
}

resolve_requests :: proc(
	requests: []Request,
	config: ^Config,
	state: ^State,
	pool: ^execution.Pool,
	allocator: mem.Allocator = context.allocator,
) -> Result {
	result := result_make(allocator)
	if config == nil {
		for request in requests {
			normalized, ok := normalize_request(request, allocator)
			if ok {
				result_add_blocked(&result, normalized)
			}
		}
		return result
	}

	normalized := normalize_requests(requests, context.temp_allocator)
	when TRACE {
		trace_eprintf("[trace - remote_dependencies] Resolving %d remote dependency request(s)\n", len(normalized))
	}
	resolved := make(map[Remote_Dependency_Key]bool, len(normalized), context.temp_allocator)
	resolve_cache_phase(&result, normalized[:], config, state, pool, &resolved, allocator)

	remaining := remaining_requests(normalized[:], resolved, context.temp_allocator)
	if config.source_order == .Local_First {
		resolve_source_phase(
			&result,
			remaining[:],
			config,
			state,
			.Local_Export,
			pool,
			&resolved,
			allocator,
		)

		remaining = remaining_requests(normalized[:], resolved, context.temp_allocator)
		resolve_source_phase(
			&result,
			remaining[:],
			config,
			state,
			.ADT,
			pool,
			&resolved,
			allocator,
		)
	} else {
		resolve_source_phase(
			&result,
			remaining[:],
			config,
			state,
			.ADT,
			pool,
			&resolved,
			allocator,
		)

		remaining = remaining_requests(normalized[:], resolved, context.temp_allocator)
		resolve_source_phase(
			&result,
			remaining[:],
			config,
			state,
			.Local_Export,
			pool,
			&resolved,
			allocator,
		)
	}

	// TODO enable when the integration is able to determine that the object is indeed a part of a typepool
	// remaining = remaining_requests(normalized[:], resolved, context.temp_allocator)
	// resolve_typepool_phase(&result, remaining[:], config, state, &resolved, allocator)

	for request in normalized {
		if !(remote_dependency_key(request) in resolved) {
			result_add_miss(&result, request)
		}
	}
	return result
}

resolve_source_phase :: proc(
	result: ^Result,
	requests: []Request,
	config: ^Config,
	state: ^State,
	phase: Source_Kind,
	pool: ^execution.Pool,
	resolved: ^map[Remote_Dependency_Key]bool,
	allocator: mem.Allocator,
) {
	#partial switch phase {
	case .Local_Export:
		resolve_local_source_phase(result, requests, config, state, pool, resolved, allocator)
	case .ADT:
		resolve_adt_source_phase(result, requests, config, state, pool, resolved, allocator)
	case:
		return
	}
}

resolve_typepool_phase :: proc(
	result: ^Result,
	requests: []Request,
	config: ^Config,
	state: ^State,
	resolved: ^map[Remote_Dependency_Key]bool,
	allocator: mem.Allocator,
) {
	artifacts := typepool_artifacts(requests, config, state, allocator)
	for &artifact in artifacts {
		resolve_artifact(result, &artifact, state, resolved, allocator)
	}
}

resolve_artifact :: proc(
	result: ^Result,
	artifact: ^Artifact,
	state: ^State,
	resolved: ^map[Remote_Dependency_Key]bool,
	allocator: mem.Allocator,
) -> bool {
	before_interfaces := len(result.interfaces)
	before_sources := len(result.sources)
	added := result_add_artifact(result, artifact, state, allocator)
	if added {
		resolve_artifact_outputs(result, artifact, before_interfaces, before_sources, resolved)
		mark_artifact_seen_after_add(state, artifact)
	}
	return added
}

resolve_prepared_artifact :: proc(
	result: ^Result,
	prepared: ^Prepared_Artifact,
	state: ^State,
	resolved: ^map[Remote_Dependency_Key]bool,
	allocator: mem.Allocator,
) -> bool {
	before_interfaces := len(result.interfaces)
	before_sources := len(result.sources)
	added := result_add_prepared_artifact(result, prepared, state, allocator)
	if added {
		resolve_artifact_outputs(
			result,
			&prepared.artifact,
			before_interfaces,
			before_sources,
			resolved,
		)
		mark_artifact_seen_after_add(state, &prepared.artifact)
	}
	return added
}

resolve_artifact_outputs :: proc(
	result: ^Result,
	artifact: ^Artifact,
	before_interfaces: int,
	before_sources: int,
	resolved: ^map[Remote_Dependency_Key]bool,
) {
	assert(result != nil && artifact != nil && resolved != nil)
	request_key := remote_dependency_key(artifact.request)
	resolved^[Remote_Dependency_Key {
		name = strings.clone(request_key.name, resolved.allocator),
		kind = request_key.kind,
	}] = true
	for i := before_interfaces; i < len(result.interfaces); i += 1 {
		key := result.interfaces[i].key
		resolved^[Remote_Dependency_Key {
			name = strings.clone(key.name, resolved.allocator),
			kind = key.kind,
		}] = true
	}
	for i := before_sources; i < len(result.sources); i += 1 {
		key := result.sources[i].key
		resolved^[Remote_Dependency_Key {
			name = strings.clone(key.name, resolved.allocator),
			kind = key.kind,
		}] = true
	}
}

mark_artifact_seen_after_add :: proc(state: ^State, artifact: ^Artifact) {
	assert(artifact != nil)
	if state == nil || artifact.artifact_id == 0 {
		return
	}
	state.seen_artifacts[artifact.artifact_id] = true
}

remaining_requests :: proc(
	requests: []Request,
	resolved: map[Remote_Dependency_Key]bool,
	allocator: mem.Allocator,
) -> [dynamic]Request {
	out := make([dynamic]Request, 0, len(requests), allocator)
	for request in requests {
		if !(remote_dependency_key(request) in resolved) {
			append(&out, request)
		}
	}
	return out
}

resolve_cache_phase :: proc(
	result: ^Result,
	requests: []Request,
	config: ^Config,
	state: ^State,
	pool: ^execution.Pool,
	resolved: ^map[Remote_Dependency_Key]bool,
	allocator: mem.Allocator,
) {
	if config.cache == nil {
		return
	}
	candidates := unseen_requests(
		requests,
		&state.seen_cache_requests if state != nil else nil,
		context.temp_allocator,
	)
	if len(candidates) > 0 {
		if pool != nil && len(candidates) > 1 {
			resolve_cache_candidates_parallel(
				result,
				candidates[:],
				config,
				state,
				pool,
				resolved,
				allocator,
			)
		} else {
			artifacts := make([dynamic]Artifact, 0, len(candidates), allocator)
			append_cache_artifacts(&artifacts, candidates[:], config, state, allocator)
			for &artifact in artifacts {
				resolve_artifact(result, &artifact, state, resolved, allocator)
			}
		}
	}

	typepool_artifacts := make([dynamic]Artifact, 0, len(candidates), allocator)
	append_typepool_cache_artifacts(&typepool_artifacts, candidates[:], config, state, pool, allocator)
	for &artifact in typepool_artifacts {
		resolve_artifact(result, &artifact, state, resolved, allocator)
	}
}

resolve_cache_candidates_parallel :: proc(
	result: ^Result,
	requests: []Request,
	config: ^Config,
	state: ^State,
	pool: ^execution.Pool,
	resolved: ^map[Remote_Dependency_Key]bool,
	allocator: mem.Allocator,
) {
	seen_artifacts := make(map[i64]bool, len(state.seen_artifacts) if state != nil else 0, context.temp_allocator)
	if state != nil {
		for artifact_id, seen in state.seen_artifacts {
			if seen {
				seen_artifacts[artifact_id] = true
			}
		}
	}

	result_arenas := make([]virtual.Arena, len(requests), context.temp_allocator)
	tasks := make(
		[dynamic]execution.Task(Cache_Prepare_Result),
		0,
		len(requests),
		context.temp_allocator,
	)

	graph: execution.Graph
	execution.graph_init(&graph, pool, context.temp_allocator)
	any_profile := config.cache_any_profile || config.profile == nil
	for request, i in requests {
		arena_err := virtual.arena_init_growing(&result_arenas[i])
		assert(arena_err == .None)
		payload := Cache_Prepare_Task_Payload {
			store            = config.cache^,
			profile          = config.profile,
			request          = request,
			any_profile     = any_profile,
			result_allocator = virtual.arena_allocator(&result_arenas[i]),
		}
		task := execution.submit_value(
			&graph,
			execution.worker_executor(pool),
			payload,
			cache_prepare_task,
		)
		append(&tasks, task)
	}
	execution.graph_start(&graph)
	for task, i in tasks {
		cache_result := execution.wait(task)
		resolve_cache_prepare_result(
			result,
			&cache_result,
			seen_artifacts,
			state,
			resolved,
			allocator,
		)
		virtual.arena_destroy(&result_arenas[i])
	}
	execution.graph_wait(&graph)
	execution.graph_destroy(&graph)
}

cache_artifacts :: proc(
	requests: []Request,
	config: ^Config,
	state: ^State,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) -> [dynamic]Artifact {
	out := make([dynamic]Artifact, 0, len(requests), allocator)
	if config.cache == nil {
		return out
	}
	candidates := unseen_requests(
		requests,
		&state.seen_cache_requests if state != nil else nil,
		context.temp_allocator,
	)
	if len(candidates) > 0 {
		if pool != nil && len(candidates) > 1 {
			append_cache_artifacts_parallel(&out, candidates[:], config, state, pool, allocator)
		} else {
			append_cache_artifacts(&out, candidates[:], config, state, allocator)
		}
	}
	append_typepool_cache_artifacts(&out, candidates[:], config, state, pool, allocator)
	return out
}

append_cache_artifacts :: proc(
	out: ^[dynamic]Artifact,
	requests: []Request,
	config: ^Config,
	state: ^State,
	allocator: mem.Allocator,
) {
	reader, reader_err := dep_store.reader(config.cache, context.temp_allocator)
	if reader_err != .None {
		for request in requests {
			result := Cache_Lookup_Result{err = reader_err}
			append_cache_lookup_result(out, request, &result, state, allocator)
		}
		return
	}
	defer dep_store.reader_destroy(&reader)

	any_profile := config.cache_any_profile || config.profile == nil
	for request in requests {
		result := cache_lookup_result(
			&reader,
			request,
			config.profile,
			any_profile,
			context.temp_allocator,
		)
		append_cache_lookup_result(out, request, result, state, allocator)
	}
}

append_cache_artifacts_parallel :: proc(
	out: ^[dynamic]Artifact,
	requests: []Request,
	config: ^Config,
	state: ^State,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) {
	task_count := min(max(pool.options.worker_count, 1), len(requests))
	batch_size := (len(requests) + task_count - 1) / task_count
	result_backing := base_runtime.heap_allocator()
	result_arenas := make([]mem.Dynamic_Arena, len(requests), context.temp_allocator)
	results := make([]^Cache_Lookup_Result, len(requests), context.temp_allocator)
	for &result_arena in result_arenas {
		mem.dynamic_arena_init(&result_arena, result_backing, result_backing, alignment = 64)
	}

	graph: execution.Graph
	execution.graph_init(&graph, pool, context.temp_allocator)
	any_profile := config.cache_any_profile || config.profile == nil
	for start := 0; start < len(requests); start += batch_size {
		end := min(start + batch_size, len(requests))
		payload := Cache_Batch_Payload {
			store         = config.cache^,
			profile       = config.profile,
			requests      = requests[start:end],
			any_profile  = any_profile,
			result_arenas = result_arenas,
			results       = results,
			offset        = start,
		}
		_ = execution.submit_value(
			&graph,
			execution.worker_executor(pool),
			payload,
			cache_lookup_batch_task,
		)
	}
	execution.graph_start(&graph)
	execution.graph_wait(&graph)
	for request, i in requests {
		append_cache_lookup_result(out, request, results[i], state, allocator)
		mem.dynamic_arena_destroy(&result_arenas[i])
	}
	execution.graph_destroy(&graph)
}

cache_lookup_batch_task :: proc(payload: Cache_Batch_Payload) -> execution.No_Result {
	store := payload.store
	reader, reader_err := dep_store.reader(&store, context.temp_allocator)
	if reader_err != .None {
		for _, i in payload.requests {
			index := payload.offset + i
			result := new(
				Cache_Lookup_Result,
				mem.dynamic_arena_allocator(&payload.result_arenas[index]),
			)
			result.err = reader_err
			payload.results[index] = result
		}
		return execution.No_Result{}
	}
	defer dep_store.reader_destroy(&reader)

	for request, i in payload.requests {
		index := payload.offset + i
		payload.results[index] = cache_lookup_result(
			&reader,
			request,
			payload.profile,
			payload.any_profile,
			mem.dynamic_arena_allocator(&payload.result_arenas[index]),
		)
	}
	return execution.No_Result{}
}

cache_prepare_task :: proc(payload: Cache_Prepare_Task_Payload) -> Cache_Prepare_Result {
	result := Cache_Prepare_Result {
		request = payload.request,
	}
	store := payload.store
	reader, reader_err := dep_store.reader(&store, context.temp_allocator)
	if reader_err != .None {
		result.status = .Error
		result.err = reader_err
		return result
	}
	defer dep_store.reader_destroy(&reader)

	lookup := cache_lookup_result(
		&reader,
		payload.request,
		payload.profile,
		payload.any_profile,
		payload.result_allocator,
	)
	if lookup.err != .None {
		result.status = .Error
		result.err = lookup.err
		return result
	}
	if !lookup.ok {
		result.status = .Miss
		return result
	}

	result.record = lookup.record
	if cached_artifact_is_stale(&result.record, payload.request) {
		result.status = .Stale
		return result
	}
	artifact := artifact_from_record(
		&result.record,
		payload.request,
		.Cache,
		payload.result_allocator,
	)
	result.prepared = prepare_artifact(&artifact, payload.result_allocator)
	result.status = .Hit
	return result
}

resolve_cache_prepare_result :: proc(
	result: ^Result,
	cache_result: ^Cache_Prepare_Result,
	seen_artifacts: map[i64]bool,
	state: ^State,
	resolved: ^map[Remote_Dependency_Key]bool,
	allocator: mem.Allocator,
) {
	switch cache_result.status {
	case .Error:
		when TRACE {
			trace_eprintf(
				"[trace - remote_dependencies] Cache lookup failed: %s %s: %v\n",
				trace_request_kind_text(cache_result.request.kind),
				cache_result.request.name,
				cache_result.err,
			)
		}
		return
	case .Miss:
		when TRACE {
			trace_eprintf(
				"[trace - remote_dependencies] Cache miss: %s %s\n",
				trace_request_kind_text(cache_result.request.kind),
				cache_result.request.name,
			)
		}
		return
	case .Stale:
		when TRACE {
			record := &cache_result.record
			trace_eprintf(
				"[trace - remote_dependencies] Cache entry is stale: %s %s -> %s %s (artifact id %d)\n",
				trace_request_kind_text(cache_result.request.kind),
				cache_result.request.name,
				record.object_kind,
				record.object_name,
				record.artifact_id,
			)
		}
		return
	case .Hit:
		record := &cache_result.record
		if record.artifact_id in seen_artifacts {
			when TRACE {
				trace_eprintf(
					"[trace - remote_dependencies] Cache hit already used: %s %s -> %s %s (artifact id %d)\n",
					trace_request_kind_text(cache_result.request.kind),
					cache_result.request.name,
					record.object_kind,
					record.object_name,
					record.artifact_id,
				)
			}
			return
		}
		when TRACE {
			trace_eprintf(
				"[trace - remote_dependencies] Cache hit: %s %s -> %s %s (artifact id %d, ext=%s)\n",
				trace_request_kind_text(cache_result.request.kind),
				cache_result.request.name,
				record.object_kind,
				record.object_name,
				record.artifact_id,
				record.file_extension,
			)
		}
		resolve_prepared_artifact(result, &cache_result.prepared, state, resolved, allocator)
		return
	case .None:
		assert(false)
	}
}

cache_lookup_result :: proc(
	reader: ^dep_store.Dependency_Store_Reader,
	request: Request,
	profile: ^dep_store.Dependency_Profile,
	any_profile: bool,
	allocator: mem.Allocator,
) -> ^Cache_Lookup_Result {
	result := new(Cache_Lookup_Result, allocator)
	if any_profile {
		result.record, result.ok, result.err = dep_store.reader_find_artifact_for_candidate_any_profile(
			reader,
			request.name,
			store_candidate_kind(request.kind),
			allocator,
		)
	} else {
		assert(profile != nil)
		result.record, result.ok, result.err = dep_store.reader_find_artifact_for_candidate(
			reader,
			profile,
			request.name,
			store_candidate_kind(request.kind),
			allocator,
		)
	}
	return result
}

append_cache_lookup_result :: proc(
	out: ^[dynamic]Artifact,
	request: Request,
	result: ^Cache_Lookup_Result,
	state: ^State,
	allocator: mem.Allocator,
) {
	assert(result != nil)
	if result.err != .None {
		when TRACE {
			trace_eprintf(
				"[trace - remote_dependencies] Cache lookup failed: %s %s: %v\n",
				trace_request_kind_text(request.kind),
				request.name,
				result.err,
			)
		}
		return
	}
	if !result.ok {
		when TRACE {
			trace_eprintf(
				"[trace - remote_dependencies] Cache miss: %s %s\n",
				trace_request_kind_text(request.kind),
				request.name,
			)
		}
		return
	}
	record := &result.record
	if cached_artifact_is_stale(record, request) {
		when TRACE {
			trace_eprintf(
				"[trace - remote_dependencies] Cache entry is stale: %s %s -> %s %s (artifact id %d)\n",
				trace_request_kind_text(request.kind),
				request.name,
				record.object_kind,
				record.object_name,
				record.artifact_id,
			)
		}
		return
	}
	if state != nil && record.artifact_id in state.seen_artifacts {
		when TRACE {
			trace_eprintf(
				"[trace - remote_dependencies] Cache hit already used: %s %s -> %s %s (artifact id %d)\n",
				trace_request_kind_text(request.kind),
				request.name,
				record.object_kind,
				record.object_name,
				record.artifact_id,
			)
		}
		return
	}
	artifact := artifact_from_record(record, request, .Cache, allocator)
	append(out, artifact)
	when TRACE {
		trace_eprintf(
			"[trace - remote_dependencies] Cache hit: %s %s -> %s %s (artifact id %d, ext=%s)\n",
			trace_request_kind_text(request.kind),
			request.name,
			record.object_kind,
			record.object_name,
			record.artifact_id,
			record.file_extension,
		)
	}
}

append_typepool_cache_artifacts :: proc(
	out: ^[dynamic]Artifact,
	requests: []Request,
	config: ^Config,
	state: ^State,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) {
	if config.cache == nil {
		return
	}
	names := make([dynamic]string, 0, len(requests), context.temp_allocator)
	for request in requests {
		if request.kind == .Type || request.kind == .Symbol {
			append(&names, request.name)
		}
	}
	if len(names) == 0 {
		return
	}
	backfill_typepool_symbol_cache(config.cache, config.profile, config.cache_any_profile)
	records: [dynamic]dep_store.Stored_Artifact_Record
	err: dep_store.Store_Error
	if config.cache_any_profile || config.profile == nil {
		records, err = dep_store.find_typepool_artifacts_for_symbols_any_profile(
			config.cache,
			names[:],
			context.temp_allocator,
		)
	} else {
		records, err = dep_store.find_typepool_artifacts_for_symbols(
			config.cache,
			config.profile,
			names[:],
			context.temp_allocator,
		)
	}
	if err != .None {
		when TRACE {
			trace_eprintf("[trace - remote_dependencies] Type-pool cache lookup failed: %v\n", err)
		}
		return
	}
	if len(records) == 0 {
		when TRACE {
			trace_eprintf(
				"[trace - remote_dependencies] Type-pool cache miss: %d symbol request(s)\n",
				len(names),
			)
		}
		return
	}
	if pool != nil && pool.options.worker_count > 0 && len(records) > 1 {
		append_typepool_cache_artifacts_parallel(out, records[:], requests, state, pool, allocator)
		return
	}
	for &record in records {
		if state != nil && record.artifact_id in state.seen_artifacts {
			when TRACE {
				trace_eprintf(
					"[trace - remote_dependencies] Type-pool cache hit already used: %s (artifact id %d)\n",
					record.object_name,
					record.artifact_id,
				)
			}
			continue
		}
		record_result := typepool_cache_record_result(record, requests, context.temp_allocator)
		append_typepool_cache_record_result(out, &record_result, state, allocator)
	}
}

append_typepool_cache_artifacts_parallel :: proc(
	out: ^[dynamic]Artifact,
	records: []dep_store.Stored_Artifact_Record,
	requests: []Request,
	state: ^State,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) {
	result_arenas := make([]virtual.Arena, len(records), context.temp_allocator)
	tasks := make(
		[dynamic]Typepool_Cache_Record_Task,
		0,
		len(records),
		context.temp_allocator,
	)

	graph: execution.Graph
	execution.graph_init(&graph, pool, context.temp_allocator)
	for &record, i in records {
		if state != nil && record.artifact_id in state.seen_artifacts {
			when TRACE {
				trace_eprintf(
					"[trace - remote_dependencies] Type-pool cache hit already used: %s (artifact id %d)\n",
					record.object_name,
					record.artifact_id,
				)
			}
			continue
		}
		arena_err := virtual.arena_init_growing(&result_arenas[i])
		assert(arena_err == .None)
		payload := Typepool_Cache_Record_Task_Payload {
			record           = record,
			requests         = requests,
			result_allocator = virtual.arena_allocator(&result_arenas[i]),
		}
		task := execution.submit_value(
			&graph,
			execution.worker_executor(pool),
			payload,
			typepool_cache_record_task,
		)
		append(&tasks, Typepool_Cache_Record_Task{task = task, arena_index = i})
	}
	execution.graph_start(&graph)
	for task_ref in tasks {
		record_result := execution.wait(task_ref.task)
		append_typepool_cache_record_result(out, &record_result, state, allocator)
		virtual.arena_destroy(&result_arenas[task_ref.arena_index])
	}
	execution.graph_wait(&graph)
	execution.graph_destroy(&graph)
}

typepool_cache_record_task :: proc(
	payload: Typepool_Cache_Record_Task_Payload,
) -> Typepool_Cache_Record_Result {
	return typepool_cache_record_result(
		payload.record,
		payload.requests,
		payload.result_allocator,
	)
}

typepool_cache_record_result :: proc(
	record: dep_store.Stored_Artifact_Record,
	requests: []Request,
	allocator: mem.Allocator,
) -> Typepool_Cache_Record_Result {
	record_copy := record
	result := Typepool_Cache_Record_Result {
		status      = .Ready,
		artifact_id = record_copy.artifact_id,
		object_name = strings.clone(record_copy.object_name, allocator),
		artifacts   = make([dynamic]Artifact, 0, 1, allocator),
	}
	analysis := typepool_source_analysis(record_copy.source_text, allocator)
	if analysis.pending_expansion {
		result.status = .Needs_Expansion
		return result
	}
	pool := utils.to_lower_ascii(record_copy.object_name, context.temp_allocator)
	for request in requests {
		if !(request.kind == .Type || request.kind == .Symbol) {
			continue
		}
		if request.name != pool && !(request.name in analysis.symbol_set) {
			continue
		}
		artifact := artifact_from_record(&record_copy, request, .Cache, allocator)
		append(&result.artifacts, artifact)
	}
	return result
}

append_typepool_cache_record_result :: proc(
	out: ^[dynamic]Artifact,
	record_result: ^Typepool_Cache_Record_Result,
	state: ^State,
	allocator: mem.Allocator,
) {
	switch record_result.status {
	case .Needs_Expansion:
		when TRACE {
			trace_eprintf(
				"[trace - remote_dependencies] Type-pool cache entry needs refetch: %s (artifact id %d)\n",
				record_result.object_name,
				record_result.artifact_id,
			)
		}
		return
	case .Ready:
		if len(record_result.artifacts) == 0 {
			return
		}
		if state != nil && record_result.artifact_id in state.seen_artifacts {
			when TRACE {
				trace_eprintf(
					"[trace - remote_dependencies] Type-pool cache hit already used: %s (artifact id %d)\n",
					record_result.object_name,
					record_result.artifact_id,
				)
			}
			return
		}
		for &artifact in record_result.artifacts {
			append(out, clone_artifact(&artifact, allocator))
			when TRACE {
				trace_eprintf(
					"[trace - remote_dependencies] Type-pool cache hit: %s provides %s %s (artifact id %d)\n",
					artifact.object_name,
					trace_request_kind_text(artifact.request.kind),
					artifact.request.name,
					artifact.artifact_id,
				)
			}
		}
		if state != nil {
			state.seen_artifacts[record_result.artifact_id] = true
		}
		return
	case .None:
		assert(false)
	}
}

backfill_typepool_symbol_cache :: proc(
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	any_profile: bool,
) {
	records: [dynamic]dep_store.Stored_Artifact_Record
	err: dep_store.Store_Error
	if any_profile || profile == nil {
		records, err = dep_store.list_unindexed_typepool_artifacts_any_profile(
			store,
			context.temp_allocator,
		)
	} else {
		records, err = dep_store.list_unindexed_typepool_artifacts(
			store,
			profile,
			context.temp_allocator,
		)
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

resolve_local_source_phase :: proc(
	result: ^Result,
	requests: []Request,
	config: ^Config,
	state: ^State,
	pool: ^execution.Pool,
	resolved: ^map[Remote_Dependency_Key]bool,
	allocator: mem.Allocator,
) {
	if len(config.local_export_roots) == 0 {
		return
	}
	candidates := unseen_requests(
		requests,
		&state.seen_local_requests if state != nil else nil,
		context.temp_allocator,
	)
	if pool != nil && len(candidates) > 1 {
		resolve_local_source_phase_parallel(
			result,
			candidates[:],
			config,
			state,
			pool,
			resolved,
			allocator,
		)
		return
	}
	for request in candidates {
		artifact, ok := local_export_artifact_for_request(
			request,
			config.local_export_roots,
			allocator,
		)
		if ok {
			when TRACE {
				trace_eprintf(
					"[trace - remote_dependencies] Local export hit: %s %s -> %s %s (%s)\n",
					trace_request_kind_text(request.kind),
					request.name,
					artifact.object_kind,
					artifact.object_name,
					artifact.object_uri,
				)
			}
			store_local_export_artifact(config, &artifact)
			resolve_artifact(result, &artifact, state, resolved, allocator)
		}
	}
}

resolve_local_source_phase_parallel :: proc(
	result: ^Result,
	requests: []Request,
	config: ^Config,
	state: ^State,
	pool: ^execution.Pool,
	resolved: ^map[Remote_Dependency_Key]bool,
	allocator: mem.Allocator,
) {
	result_backing := base_runtime.heap_allocator()
	result_arenas := make([]mem.Dynamic_Arena, len(requests), context.temp_allocator)
	tasks := make(
		[dynamic]execution.Task(Local_Prepare_Task_Result),
		0,
		len(requests),
		context.temp_allocator,
	)

	graph: execution.Graph
	execution.graph_init(&graph, pool, context.temp_allocator)
	for request, i in requests {
		mem.dynamic_arena_init(&result_arenas[i], result_backing, result_backing, alignment = 64)
		payload := Local_Task_Payload {
			request            = request,
			local_export_roots = config.local_export_roots,
			result_allocator   = mem.dynamic_arena_allocator(&result_arenas[i]),
		}
		task := execution.submit_value(
			&graph,
			execution.worker_executor(pool),
			payload,
			local_export_prepare_task,
		)
		append(&tasks, task)
	}
	execution.graph_start(&graph)
	for task, i in tasks {
		task_result := execution.wait(task)
		if task_result.ok {
			artifact := &task_result.prepared.artifact
			when TRACE {
				trace_eprintf(
					"[trace - remote_dependencies] Local export hit: %s %s -> %s %s (%s)\n",
					trace_request_kind_text(artifact.request.kind),
					artifact.request.name,
					artifact.object_kind,
					artifact.object_name,
					artifact.object_uri,
				)
			}
			store_local_export_artifact(config, artifact)
			resolve_prepared_artifact(result, &task_result.prepared, state, resolved, allocator)
		}
		mem.dynamic_arena_destroy(&result_arenas[i])
	}
	execution.graph_wait(&graph)
	execution.graph_destroy(&graph)
}

local_artifacts :: proc(
	requests: []Request,
	config: ^Config,
	state: ^State,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) -> [dynamic]Artifact {
	out := make([dynamic]Artifact, 0, len(requests), allocator)
	if len(config.local_export_roots) == 0 {
		return out
	}
	candidates := unseen_requests(
		requests,
		&state.seen_local_requests if state != nil else nil,
		context.temp_allocator,
	)
	if pool != nil && len(candidates) > 1 {
		return local_artifacts_parallel(candidates[:], config, pool, allocator)
	}
	for request in candidates {
		artifact, ok := local_export_artifact_for_request(
			request,
			config.local_export_roots,
			allocator,
		)
		if ok {
			when TRACE {
				trace_eprintf(
					"[trace - remote_dependencies] Local export hit: %s %s -> %s %s (%s)\n",
					trace_request_kind_text(request.kind),
					request.name,
					artifact.object_kind,
					artifact.object_name,
					artifact.object_uri,
				)
			}
			store_local_export_artifact(config, &artifact)
			append(&out, artifact)
		}
	}
	return out
}

local_artifacts_parallel :: proc(
	requests: []Request,
	config: ^Config,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) -> [dynamic]Artifact {
	out := make([dynamic]Artifact, 0, len(requests), allocator)
	result_backing := base_runtime.heap_allocator()
	result_arenas := make([]mem.Dynamic_Arena, len(requests), context.temp_allocator)
	tasks := make(
		[dynamic]execution.Task(Local_Task_Result),
		0,
		len(requests),
		context.temp_allocator,
	)

	graph: execution.Graph
	execution.graph_init(&graph, pool, context.temp_allocator)
	for request, i in requests {
		mem.dynamic_arena_init(&result_arenas[i], result_backing, result_backing, alignment = 64)
		payload := Local_Task_Payload {
			request            = request,
			local_export_roots = config.local_export_roots,
			result_allocator   = mem.dynamic_arena_allocator(&result_arenas[i]),
		}
		task := execution.submit_value(
			&graph,
			execution.worker_executor(pool),
			payload,
			local_export_artifact_task,
		)
		append(&tasks, task)
	}
	execution.graph_start(&graph)
	for task, i in tasks {
		task_result := execution.wait(task)
		if task_result.ok {
			artifact := clone_artifact(&task_result.artifact, allocator)
			when TRACE {
				trace_eprintf(
					"[trace - remote_dependencies] Local export hit: %s %s -> %s %s (%s)\n",
					trace_request_kind_text(artifact.request.kind),
					artifact.request.name,
					artifact.object_kind,
					artifact.object_name,
					artifact.object_uri,
				)
			}
			store_local_export_artifact(config, &artifact)
			append(&out, artifact)
		}
		mem.dynamic_arena_destroy(&result_arenas[i])
	}
	execution.graph_wait(&graph)
	execution.graph_destroy(&graph)
	return out
}

local_export_artifact_task :: proc(payload: Local_Task_Payload) -> Local_Task_Result {
	artifact, ok := local_export_artifact_for_request(
		payload.request,
		payload.local_export_roots,
		payload.result_allocator,
	)
	return Local_Task_Result{artifact = artifact, ok = ok}
}

local_export_prepare_task :: proc(payload: Local_Task_Payload) -> Local_Prepare_Task_Result {
	artifact, ok := local_export_artifact_for_request(
		payload.request,
		payload.local_export_roots,
		payload.result_allocator,
	)
	if !ok {
		return {}
	}
	prepared := prepare_artifact(&artifact, payload.result_allocator)
	return Local_Prepare_Task_Result{prepared = prepared, ok = true}
}

local_export_artifact_for_request :: proc(
	request: Request,
	local_export_roots: []string,
	allocator: mem.Allocator,
) -> (
	Artifact,
	bool,
) {
	file_names := local_export_candidate_file_names(request, context.temp_allocator)
	if len(file_names) == 0 {
		return {}, false
	}
	paths := make([dynamic]string, 0, 2, context.temp_allocator)
	for root in local_export_roots {
		collect_local_export_candidate_paths(root, file_names[:], &paths, context.temp_allocator)
	}
	for path in paths {
		source, ok := read_text_file(path, context.temp_allocator)
		if !ok {
			continue
		}
		file_extension := strings.trim_prefix(filepath.ext(path), ".")
		if !source_is_xml("", file_extension, source) &&
		   !local_export_abap_source_matches(request, source) {
			continue
		}
		object_kind, object_type := local_export_object_kind_type(
			request,
			file_extension,
			source,
			context.temp_allocator,
		)
		return Artifact {
				request = clone_request(request, allocator),
				source_kind = .Local_Export,
				object_kind = strings.clone(object_kind, allocator),
				object_name = strings.clone(request.name, allocator),
				object_uri = strings.clone(path, allocator),
				object_type = strings.clone(object_type, allocator),
				file_extension = strings.clone(file_extension, allocator),
				source_text = strings.clone(source, allocator),
			},
			true
	}
	return {}, false
}

store_local_export_artifact :: proc(config: ^Config, artifact: ^Artifact) {
	store_local_export_dependency(
		config.cache if config.cache != nil && config.profile != nil else nil,
		config.profile if config.cache != nil && config.profile != nil else nil,
		artifact.request,
		artifact.object_uri,
		artifact.source_text,
		artifact.object_kind,
		artifact.object_type,
		artifact.file_extension,
	)
}

store_local_export_dependency :: proc(
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	request: Request,
	path, source, object_kind, object_type, file_extension: string,
) {
	if store == nil || profile == nil {
		return
	}
	store_arena: mem.Dynamic_Arena
	store_backing := base_runtime.heap_allocator()
	mem.dynamic_arena_init(&store_arena, store_backing, store_backing, alignment = 64)
	defer mem.dynamic_arena_destroy(&store_arena)
	store_allocator := mem.dynamic_arena_allocator(&store_arena)
	fetched_at, _ := time.time_to_rfc3339(time.now(), allocator = store_allocator)
	extension := file_extension if file_extension != "" else "abap"
	if source_is_xml(object_kind, file_extension, source) && object_kind_is_ddic(object_kind) {
		extension = "xml"
	}
	symbols :=
		typepool_source_symbols(source, store_allocator) if strings.equal_fold(object_kind, TYPEPOOL_OBJECT_KIND) else nil
	artifact := dep_store.Stored_Artifact_Input {
		package_name     = request.name,
		object_kind      = object_kind,
		object_name      = request.name,
		object_uri       = path,
		object_type      = object_type,
		description      = "Local export dependency",
		file_extension   = extension,
		source_text      = source,
		fetched_at       = fetched_at,
		typepool_symbols = symbols[:],
	}
	_, _ = dep_store.put_artifact(store, profile, &artifact, store_allocator)
}

ensure_adt_available :: proc(
	config: ^Config,
	pool: ^execution.Pool,
) -> bool {
	if config == nil || config.adt_client == nil {
		return false
	}
	availability := config.adt_availability
	if availability != nil && availability.status == .Unavailable {
		return false
	}
	if config.adt_client.csrf_token != "" {
		if availability != nil {
			availability.status = .Available
			availability.error = .None
		}
		return true
	}

	probe: ADT_Probe_Result
	if pool != nil && pool.started && pool.options.worker_count > 0 {
		result_arena: virtual.Arena
		arena_err := virtual.arena_init_growing(&result_arena)
		assert(arena_err == .None)
		defer virtual.arena_destroy(&result_arena)

		graph: execution.Graph
		execution.graph_init(&graph, pool, context.temp_allocator)
		task := execution.submit_value(
			&graph,
			execution.worker_executor(pool),
			ADT_Probe_Task_Payload {
				client           = config.adt_client,
				result_allocator = virtual.arena_allocator(&result_arena),
			},
			adt_probe_task,
		)
		execution.graph_start(&graph)
		probe = execution.wait(task)
		execution.graph_wait(&graph)
		execution.graph_destroy(&graph)
	} else {
		probe.bootstrap, probe.err = adt.bootstrap_session(
			config.adt_client,
			context.temp_allocator,
		)
	}

	if probe.err != .None {
		if availability != nil {
			availability.status = .Unavailable
			availability.error = probe.err
		}
		when TRACE {
			trace_eprintf(
				"[trace - remote_dependencies] ADT session probe failed; disabling ADT for this session: %v\n",
				probe.err,
			)
		}
		return false
	}
	adt.apply_session_bootstrap(config.adt_client, &probe.bootstrap)
	if availability != nil {
		availability.status = .Available
		availability.error = .None
	}
	return true
}

adt_probe_task :: proc(payload: ADT_Probe_Task_Payload) -> ADT_Probe_Result {
	bootstrap, err := adt.bootstrap_session(payload.client, payload.result_allocator)
	return ADT_Probe_Result{bootstrap = bootstrap, err = err}
}

block_adt_unavailable_requests :: proc(
	result: ^Result,
	requests: []Request,
) {
	for request in requests {
		result_add_blocked(result, request)
		result_add_diagnostic(
			result,
			request,
			.ADT,
			"ADT connection unavailable for this session",
		)
		when TRACE {
			trace_eprintf(
				"[trace - remote_dependencies] ADT miss: %s %s (connection unavailable for this session)\n",
				trace_request_kind_text(request.kind),
				request.name,
			)
		}
	}
}

resolve_adt_source_phase :: proc(
	result: ^Result,
	requests: []Request,
	config: ^Config,
	state: ^State,
	pool: ^execution.Pool,
	resolved: ^map[Remote_Dependency_Key]bool,
	allocator: mem.Allocator,
) {
	if config.adt_client == nil {
		return
	}
	candidates := unseen_requests(
		requests,
		&state.seen_adt_requests if state != nil else nil,
		context.temp_allocator,
	)
	filtered := make([dynamic]Request, 0, len(candidates), context.temp_allocator)
	for request in candidates {
		if request.kind != .Symbol {
			append(&filtered, request)
		}
	}
	if len(filtered) == 0 {
		return
	}
	if !ensure_adt_available(config, pool) {
		block_adt_unavailable_requests(result, filtered[:])
		return
	}
	if pool != nil && len(filtered) > 1 {
		resolve_adt_source_phase_parallel(
			result,
			filtered[:],
			config,
			state,
			pool,
			resolved,
			allocator,
		)
		return
	}
	for request in filtered {
		fetched := fetch_adt_request(
			config.adt_client,
			request,
			config.cache if config.cache != nil && config.profile != nil else nil,
			config.profile if config.cache != nil && config.profile != nil else nil,
			allocator,
		)
		for &artifact in fetched {
			resolve_artifact(result, &artifact, state, resolved, allocator)
		}
	}
}

resolve_adt_source_phase_parallel :: proc(
	result: ^Result,
	requests: []Request,
	config: ^Config,
	state: ^State,
	pool: ^execution.Pool,
	resolved: ^map[Remote_Dependency_Key]bool,
	allocator: mem.Allocator,
) {
	result_arenas := make([]virtual.Arena, len(requests), context.temp_allocator)
	tasks := make(
		[dynamic]execution.Task([dynamic]Prepared_Artifact),
		0,
		len(requests),
		context.temp_allocator,
	)

	graph: execution.Graph
	execution.graph_init(&graph, pool, context.temp_allocator)
	store := config.cache if config.cache != nil && config.profile != nil else nil
	profile := config.profile if config.cache != nil && config.profile != nil else nil
	for request, i in requests {
		arena_err := virtual.arena_init_growing(&result_arenas[i])
		assert(arena_err == .None)
		payload := ADT_Fetch_Task_Payload {
			client           = config.adt_client,
			request          = request,
			store            = store,
			profile          = profile,
			result_allocator = virtual.arena_allocator(&result_arenas[i]),
		}
		task := execution.submit_value(
			&graph,
			execution.worker_executor(pool),
			payload,
			adt_fetch_prepare_task,
		)
		append(&tasks, task)
	}
	execution.graph_start(&graph)
	for task, i in tasks {
		prepared_artifacts := execution.wait(task)
		for &prepared in prepared_artifacts {
			resolve_prepared_artifact(result, &prepared, state, resolved, allocator)
		}
		virtual.arena_destroy(&result_arenas[i])
	}
	execution.graph_wait(&graph)
	execution.graph_destroy(&graph)
}

adt_artifacts :: proc(
	requests: []Request,
	config: ^Config,
	state: ^State,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) -> [dynamic]Artifact {
	out := make([dynamic]Artifact, 0, len(requests), allocator)
	if config.adt_client == nil {
		return out
	}
	candidates := unseen_requests(
		requests,
		&state.seen_adt_requests if state != nil else nil,
		context.temp_allocator,
	)
	filtered := make([dynamic]Request, 0, len(candidates), context.temp_allocator)
	for request in candidates {
		if request.kind != .Symbol {
			append(&filtered, request)
		}
	}
	if len(filtered) == 0 {
		return out
	}
	if !ensure_adt_available(config, pool) {
		when TRACE {
			for request in filtered {
				trace_eprintf(
					"[trace - remote_dependencies] ADT miss: %s %s (connection unavailable for this session)\n",
					trace_request_kind_text(request.kind),
					request.name,
				)
			}
		}
		return out
	}
	if pool != nil && len(filtered) > 1 {
		return adt_artifacts_parallel(filtered[:], config, pool, allocator)
	}
	for request in filtered {
		fetched := fetch_adt_request(
			config.adt_client,
			request,
			config.cache if config.cache != nil && config.profile != nil else nil,
			config.profile if config.cache != nil && config.profile != nil else nil,
			allocator,
		)
		for artifact in fetched {
			append(&out, artifact)
		}
	}
	return out
}

adt_artifacts_parallel :: proc(
	requests: []Request,
	config: ^Config,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) -> [dynamic]Artifact {
	out := make([dynamic]Artifact, 0, len(requests), allocator)
	result_backing := base_runtime.heap_allocator()
	result_arenas := make([]mem.Dynamic_Arena, len(requests), context.temp_allocator)
	tasks := make(
		[dynamic]execution.Task([dynamic]Artifact),
		0,
		len(requests),
		context.temp_allocator,
	)

	graph: execution.Graph
	execution.graph_init(&graph, pool, context.temp_allocator)
	store := config.cache if config.cache != nil && config.profile != nil else nil
	profile := config.profile if config.cache != nil && config.profile != nil else nil
	for request, i in requests {
		mem.dynamic_arena_init(&result_arenas[i], result_backing, result_backing, alignment = 64)
		payload := ADT_Fetch_Task_Payload {
			client           = config.adt_client,
			request          = request,
			store            = store,
			profile          = profile,
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
		fetched := execution.wait(task)
		for &artifact in fetched {
			append(&out, clone_artifact(&artifact, allocator))
		}
		mem.dynamic_arena_destroy(&result_arenas[i])
	}
	execution.graph_wait(&graph)
	execution.graph_destroy(&graph)
	return out
}

adt_fetch_task :: proc(payload: ADT_Fetch_Task_Payload) -> [dynamic]Artifact {
	return fetch_adt_request(
		payload.client,
		payload.request,
		payload.store,
		payload.profile,
		payload.result_allocator,
	)
}

adt_fetch_prepare_task :: proc(payload: ADT_Fetch_Task_Payload) -> [dynamic]Prepared_Artifact {
	artifacts := fetch_adt_request(
		payload.client,
		payload.request,
		payload.store,
		payload.profile,
		payload.result_allocator,
	)
	out := make([dynamic]Prepared_Artifact, 0, len(artifacts), payload.result_allocator)
	for &artifact in artifacts {
		append(&out, prepare_artifact(&artifact, payload.result_allocator))
	}
	return out
}

fetch_adt_request :: proc(
	client: ^adt.Client,
	request: Request,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	allocator: mem.Allocator,
) -> [dynamic]Artifact {
	out := make([dynamic]Artifact, 0, 2, allocator)
	if adt_request_direct_first(request) {
		direct := adt.direct_dependency_object_refs(
			request.name,
			request_kind_text(request),
			context.temp_allocator,
		)
		if fetch_adt_objects(client, request, direct[:], store, profile, &out, allocator, true) >
		   0 {
			return out
		}
	}

	objects, err := adt.search_repository_objects(client, request.name, 50, context.temp_allocator)
	if err != .None {
		objects = adt.direct_dependency_object_refs(
			request.name,
			remote_dependency_kind_text(request.kind),
			context.temp_allocator,
		)
	}
	selected := adt.select_dependency_objects(
		request.name,
		objects[:],
		remote_dependency_kind_text(request.kind),
		context.temp_allocator,
	)
	if len(selected) == 0 {
		selected = adt.direct_dependency_object_refs(
			request.name,
			remote_dependency_kind_text(request.kind),
			context.temp_allocator,
		)
	}
	_ = fetch_adt_objects(client, request, selected[:], store, profile, &out, allocator)
	when TRACE {
		if len(out) == 0 {
			trace_eprintf(
				"[trace - remote_dependencies] ADT miss: %s %s\n",
				trace_request_kind_text(request.kind),
				request.name,
			)
		}
	}
	return out
}

adt_request_direct_first :: proc(request: Request) -> bool {
	#partial switch request.kind {
	case .Include, .Message_Class, .Report, .Class, .Interface:
		return true
	}
	return false
}

fetch_adt_objects :: proc(
	client: ^adt.Client,
	request: Request,
	objects: []adt.Object_Ref,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	out: ^[dynamic]Artifact,
	allocator: mem.Allocator,
	stop_after_first := false,
) -> int {
	fetched_count := 0
	for &object_ref in objects {
		fetched, fetch_err := adt.fetch_dependency_object(
			client,
			&object_ref,
			context.temp_allocator,
		)
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
					context.temp_allocator,
				)
			}
		}
		when TRACE {
			trace_eprintf(
				"[trace - remote_dependencies] ADT hit: %s %s -> %s %s (type=%s, ext=%s, bytes=%d, shared=%d)\n",
				trace_request_kind_text(request.kind),
				request.name,
				fetched.manifest_kind,
				object_ref.name,
				object_ref.object_type,
				fetched.file_extension,
				len(fetched.body),
				len(fetched.shared_dependencies),
			)
		}
		store_adt_dependency_fetch(store, profile, &object_ref, &fetched)
		append(
			out,
			artifact_from_adt_fetch(
				request,
				&object_ref,
				fetched.manifest_kind,
				fetched.file_extension,
				fetched.body,
				false,
				allocator,
			),
		)
		for &shared in fetched.shared_dependencies {
			when TRACE {
				trace_eprintf(
					"[trace - remote_dependencies] ADT hit: include %s -> %s %s (type=%s, ext=%s, bytes=%d, shared from %s)\n",
					shared.object_ref.name,
					shared.manifest_kind,
					shared.object_ref.name,
					shared.object_ref.object_type,
					shared.file_extension,
					len(shared.body),
					object_ref.name,
				)
			}
			append(
				out,
				artifact_from_adt_fetch(
					Request {
						name = utils.to_lower_ascii(shared.object_ref.name, context.temp_allocator),
						kind = .Include,
					},
					&shared.object_ref,
					shared.manifest_kind,
					shared.file_extension,
					shared.body,
					true,
					allocator,
				),
			)
		}
		fetched_count += 1
		if stop_after_first {
			break
		}
	}
	return fetched_count
}

typepool_artifacts :: proc(
	requests: []Request,
	config: ^Config,
	state: ^State,
	allocator: mem.Allocator,
) -> [dynamic]Artifact {
	out := make([dynamic]Artifact, 0, 2, allocator)
	if config.adt_client == nil || !adt.typepool_resolver_enabled(config.adt_client) {
		return out
	}
	candidates := unseen_requests(
		requests,
		&state.seen_typepool_requests if state != nil else nil,
		context.temp_allocator,
	)
	filtered := make([dynamic]Request, 0, len(candidates), context.temp_allocator)
	for request in candidates {
		if !(request.kind == .Type || request.kind == .Symbol) {
			continue
		}
		append(&filtered, request)
	}
	if len(filtered) == 0 {
		return out
	}
	if !ensure_adt_available(config, nil) {
		when TRACE {
			for request in filtered {
				trace_eprintf(
					"[trace - remote_dependencies] Type-pool owner lookup skipped: %s %s (connection unavailable for this session)\n",
					trace_request_kind_text(request.kind),
					request.name,
				)
			}
		}
		return out
	}
	for request in filtered {
		pool, owner_err := adt.resolve_typepool_owner(
			config.adt_client,
			request.name,
			context.temp_allocator,
		)
		if owner_err != .None {
			when TRACE {
				trace_eprintf(
					"[trace - remote_dependencies] Type-pool owner lookup failed: %s %s: %v\n",
					trace_request_kind_text(request.kind),
					request.name,
					owner_err,
				)
			}
			continue
		}
		pool = utils.to_lower_ascii(strings.trim_space(pool), context.temp_allocator)
		if pool == "" {
			when TRACE {
				trace_eprintf(
					"[trace - remote_dependencies] Type-pool owner lookup returned no pool: %s %s\n",
					trace_request_kind_text(request.kind),
					request.name,
				)
			}
			continue
		}
		when TRACE {
			trace_eprintf(
				"[trace - remote_dependencies] Type-pool owner: %s %s -> %s\n",
				trace_request_kind_text(request.kind),
				request.name,
				pool,
			)
		}
		artifact, ok := cached_typepool_artifact(
			config.cache,
			config.profile,
			request,
			pool,
			allocator,
		)
		if !ok {
			when TRACE {
				trace_eprintf("[trace - remote_dependencies] Type-pool source cache miss: %s\n", pool)
			}
			source_err: adt.Error
			artifact, ok, source_err = fetch_typepool_artifact(
				config.adt_client,
				request,
				pool,
				config.cache if config.cache != nil && config.profile != nil else nil,
				config.profile if config.cache != nil && config.profile != nil else nil,
				allocator,
			)
			if source_err != .None {
				when TRACE {
					trace_eprintf(
						"[trace - remote_dependencies] Type-pool source fetch failed: %s: %v\n",
						pool,
						source_err,
					)
				}
				continue
			}
			when TRACE {
				trace_eprintf(
					"[trace - remote_dependencies] Type-pool source fetch ok: %s (bytes=%d)\n",
					pool,
					len(artifact.source_text),
				)
			}
		} else {
			when TRACE {
				trace_eprintf("[trace - remote_dependencies] Type-pool source cache hit: %s\n", pool)
			}
		}
		if ok {
			append(&out, artifact)
		}
	}
	return out
}

cached_typepool_artifact :: proc(
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	request: Request,
	pool: string,
	allocator: mem.Allocator,
) -> (
	Artifact,
	bool,
) {
	if store == nil || profile == nil {
		return {}, false
	}
	record, ok, err := dep_store.find_artifact_by_kind_name(
		store,
		profile,
		TYPEPOOL_OBJECT_KIND,
		pool,
		allocator,
	)
	if err != .None || !ok {
		return {}, false
	}
	analysis := typepool_source_analysis(record.source_text, allocator)
	if analysis.pending_expansion {
		return {}, false
	}
	return artifact_from_record(&record, request, .Cache, allocator), true
}

fetch_typepool_artifact :: proc(
	client: ^adt.Client,
	request: Request,
	pool: string,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	allocator: mem.Allocator,
) -> (
	Artifact,
	bool,
	adt.Error,
) {
	raw, err := adt.fetch_typepool_source(client, pool, context.temp_allocator)
	if err != .None {
		return {}, false, err
	}
	source := expanded_typepool_dependency_source(client, raw, context.temp_allocator)
	artifact := typepool_artifact_from_source(request, pool, source, .Type_Pool, allocator)
	store_typepool_artifact(store, profile, &artifact)
	return artifact, true, .None
}

typepool_artifact_from_source :: proc(
	request: Request,
	pool, source: string,
	source_kind: Source_Kind,
	allocator: mem.Allocator,
) -> Artifact {
	return Artifact {
		request = clone_request(request, allocator),
		source_kind = source_kind,
		object_kind = strings.clone(TYPEPOOL_OBJECT_KIND, allocator),
		object_name = strings.clone(pool, allocator),
		object_uri = typepool_object_uri(pool, allocator),
		object_type = strings.clone(TYPEPOOL_OBJECT_TYPE, allocator),
		file_extension = "abap",
		source_text = strings.clone(source, allocator),
	}
}

store_typepool_artifact :: proc(
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	typepool_artifact: ^Artifact,
) {
	if store == nil || profile == nil {
		return
	}
	assert(typepool_artifact != nil)
	store_arena: mem.Dynamic_Arena
	store_backing := base_runtime.heap_allocator()
	mem.dynamic_arena_init(&store_arena, store_backing, store_backing, alignment = 64)
	defer mem.dynamic_arena_destroy(&store_arena)
	store_allocator := mem.dynamic_arena_allocator(&store_arena)
	fetched_at, _ := time.time_to_rfc3339(time.now(), allocator = store_allocator)
	analysis := typepool_source_analysis(typepool_artifact.source_text, store_allocator)
	stored := dep_store.Stored_Artifact_Input {
		package_name     = typepool_artifact.object_name,
		object_kind      = TYPEPOOL_OBJECT_KIND,
		object_name      = typepool_artifact.object_name,
		object_uri       = typepool_artifact.object_uri,
		object_type      = TYPEPOOL_OBJECT_TYPE,
		description      = "Type-pool source",
		file_extension   = "abap",
		source_text      = typepool_artifact.source_text,
		fetched_at       = fetched_at,
		typepool_symbols = analysis.symbols[:],
	}
	_, _ = dep_store.put_artifact(store, profile, &stored, store_allocator)
}

store_adt_dependency_fetch :: proc(
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	object_ref: ^adt.Object_Ref,
	fetched: ^adt.Dependency_Fetch_Result,
) {
	if store == nil || profile == nil {
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
		stored_artifact_from_adt(
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
			stored_artifact_from_adt(
				&shared.object_ref,
				shared.manifest_kind,
				shared.file_extension,
				shared.body,
				fetched_at,
				store_allocator,
			),
		)
	}
	_, _ = dep_store.put_artifacts(store, profile, artifacts[:], store_allocator)
}

stored_artifact_from_adt :: proc(
	object_ref: ^adt.Object_Ref,
	object_kind, file_extension, source, fetched_at: string,
	allocator: mem.Allocator,
) -> dep_store.Stored_Artifact_Input {
	extension := file_extension
	if source_is_xml(object_kind, file_extension, source) && object_kind_is_ddic(object_kind) {
		extension = "xml"
	}
	symbols :=
		typepool_source_symbols(source, allocator) if strings.equal_fold(object_kind, TYPEPOOL_OBJECT_KIND) else nil
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
		typepool_symbols = symbols[:],
	}
}

clone_artifact :: proc(artifact: ^Artifact, allocator: mem.Allocator) -> Artifact {
	return Artifact {
		request = clone_request(artifact.request, allocator),
		source_kind = artifact.source_kind,
		artifact_id = artifact.artifact_id,
		object_kind = strings.clone(artifact.object_kind, allocator),
		object_name = strings.clone(artifact.object_name, allocator),
		object_uri = strings.clone(artifact.object_uri, allocator),
		object_type = strings.clone(artifact.object_type, allocator),
		file_extension = strings.clone(artifact.file_extension, allocator),
		source_text = strings.clone(artifact.source_text, allocator),
		shared = artifact.shared,
	}
}

artifact_from_record :: proc(
	record: ^dep_store.Stored_Artifact_Record,
	request: Request,
	source_kind: Source_Kind,
	allocator: mem.Allocator,
) -> Artifact {
	return Artifact {
		request = clone_request(request, allocator),
		source_kind = source_kind,
		artifact_id = record.artifact_id,
		object_kind = strings.clone(record.object_kind, allocator),
		object_name = strings.clone(record.object_name, allocator),
		object_uri = strings.clone(record.object_uri, allocator),
		object_type = strings.clone(record.object_type, allocator),
		file_extension = strings.clone(record.file_extension, allocator),
		source_text = strings.clone(record.source_text, allocator),
	}
}

artifact_from_adt_fetch :: proc(
	request: Request,
	object_ref: ^adt.Object_Ref,
	object_kind, file_extension, source: string,
	shared: bool,
	allocator: mem.Allocator,
) -> Artifact {
	normalized, ok := normalize_request(request, context.temp_allocator)
	assert(ok)
	extension := file_extension
	if source_is_xml(object_kind, file_extension, source) && object_kind_is_ddic(object_kind) {
		extension = "xml"
	}
	return Artifact {
		request = clone_request(normalized, allocator),
		source_kind = .ADT,
		object_kind = strings.clone(object_kind, allocator),
		object_name = strings.clone(object_ref.name, allocator),
		object_uri = strings.clone(object_ref.uri, allocator),
		object_type = strings.clone(object_ref.object_type, allocator),
		file_extension = strings.clone(extension, allocator),
		source_text = strings.clone(source, allocator),
		shared = shared,
	}
}

cached_artifact_is_stale :: proc(
	record: ^dep_store.Stored_Artifact_Record,
	request: Request,
) -> bool {
	_ = request
	if object_kind_is_ddic(record.object_kind) &&
	   !source_is_xml(record.object_kind, record.file_extension, record.source_text) {
		return !remote_dependency_file_extension_is_ddic(record.file_extension)
	}
	return false
}

open_source :: proc(
	config: ^Config,
	object_kind, object_name: string,
	allocator: mem.Allocator = context.allocator,
) -> (
	Open_Source,
	bool,
	string,
) {
	if config == nil || strings.trim_space(object_name) == "" {
		return {}, false, "missing dependency object"
	}
	request, request_ok := candidate_for_object(object_kind, object_name)
	if !request_ok {
		return {}, false, "unsupported dependency object kind"
	}
	if source, ok, err := open_source_from_cache(
		config,
		request,
		object_kind,
		object_name,
		allocator,
	); err != "" || ok {
		return source, ok, err
	}
	if config.source_order == .Local_First {
		if source, ok := open_remote_dependency_source_from_local(config, request, allocator); ok {
			return source, true, ""
		}
		if source, ok := open_source_from_adt(
			config,
			request,
			object_kind,
			object_name,
			allocator,
		); ok {
			return source, true, ""
		}
	} else {
		if source, ok := open_source_from_adt(
			config,
			request,
			object_kind,
			object_name,
			allocator,
		); ok {
			return source, true, ""
		}
		if source, ok := open_remote_dependency_source_from_local(config, request, allocator); ok {
			return source, true, ""
		}
	}
	return {}, false, "dependency object source not found"
}

open_source_from_cache :: proc(
	config: ^Config,
	request: Request,
	object_kind, object_name: string,
	allocator: mem.Allocator,
) -> (
	Open_Source,
	bool,
	string,
) {
	if config.cache == nil {
		return {}, false, ""
	}
	record: dep_store.Stored_Artifact_Record
	ok := false
	err := dep_store.Store_Error.None
	if config.cache_any_profile || config.profile == nil {
		record, ok, err = dep_store.find_artifact_for_candidate_any_profile(
			config.cache,
			object_name,
			store_candidate_kind(request.kind),
			context.temp_allocator,
		)
		if ok && !strings.equal_fold(record.object_kind, object_kind) {
			ok = false
		}
	} else {
		record, ok, err = dep_store.find_artifact_by_kind_name(
			config.cache,
			config.profile,
			object_kind,
			object_name,
			context.temp_allocator,
		)
	}
	if err != .None {
		return {}, false, "dependency store lookup failed"
	}
	if !ok {
		return {}, false, ""
	}
	artifact := artifact_from_record(&record, request, .Cache, allocator)
	return open_source_from_artifact(&artifact, allocator), true, ""
}

open_remote_dependency_source_from_local :: proc(
	config: ^Config,
	request: Request,
	allocator: mem.Allocator,
) -> (
	Open_Source,
	bool,
) {
	artifacts := local_artifacts({request}, config, nil, nil, allocator)
	if len(artifacts) == 0 {
		return {}, false
	}
	return open_source_from_artifact(&artifacts[0], allocator), true
}

open_source_from_adt :: proc(
	config: ^Config,
	request: Request,
	object_kind, object_name: string,
	allocator: mem.Allocator,
) -> (
	Open_Source,
	bool,
) {
	if config.adt_client == nil {
		return {}, false
	}
	if !ensure_adt_available(config, nil) {
		return {}, false
	}
	artifacts := fetch_adt_request(
		config.adt_client,
		request,
		config.cache if config.cache != nil && config.profile != nil else nil,
		config.profile if config.cache != nil && config.profile != nil else nil,
		allocator,
	)
	for &artifact in artifacts {
		if strings.equal_fold(artifact.object_name, object_name) &&
		   open_adt_artifact_matches_object_kind(&artifact, object_kind) {
			return open_source_from_artifact(&artifact, allocator), true
		}
	}
	return {}, false
}
