package abap_frontend_remote_dependencies

import "src:adt"
import dep_store "src:dependency_store"
import execution "src:execution"

import base_runtime "base:runtime"
import "core:mem"
import filepath "core:path/filepath"
import "core:slice"
import "core:strings"
import "core:time"

ADT_Fetch_Task_Payload :: struct {
	client:           ^adt.Client,
	request:          Request,
	store:            ^dep_store.Dependency_Store,
	profile:          ^dep_store.Dependency_Profile,
	result_allocator: mem.Allocator,
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

Local_Task_Payload :: struct {
	request:            Request,
	local_export_roots: []string,
	result_allocator:   mem.Allocator,
}

Local_Task_Result :: struct {
	artifact: Artifact,
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
	cache_artifacts := cache_artifacts(normalized[:], config, state, pool, allocator)
	for &artifact in cache_artifacts {
		before_interfaces := len(result.interfaces)
		before_sources := len(result.sources)
		added := result_add_artifact(&result, &artifact, state, allocator)
		if added {
			resolve_artifact_outputs(&result, &artifact, before_interfaces, before_sources, &resolved)
			mark_artifact_seen_after_add(state, &artifact)
		}
	}

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
	artifacts: [dynamic]Artifact
	#partial switch phase {
	case .Local_Export:
		artifacts = local_artifacts(requests, config, state, pool, allocator)
	case .ADT:
		artifacts = adt_artifacts(requests, config, state, pool, allocator)
	case:
		return
	}
	for &artifact in artifacts {
		before_interfaces := len(result.interfaces)
		before_sources := len(result.sources)
		added := result_add_artifact(result, &artifact, state, allocator)
		if added {
			resolve_artifact_outputs(result, &artifact, before_interfaces, before_sources, resolved)
			mark_artifact_seen_after_add(state, &artifact)
		}
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
		before_interfaces := len(result.interfaces)
		before_sources := len(result.sources)
		if result_add_artifact(result, &artifact, state, allocator) {
			resolve_artifact_outputs(result, &artifact, before_interfaces, before_sources, resolved)
			mark_artifact_seen_after_add(state, &artifact)
		}
	}
}

resolve_artifact_outputs :: proc(
	result: ^Result,
	artifact: ^Artifact,
	before_interfaces: int,
	before_sources: int,
	resolved: ^map[Remote_Dependency_Key]bool,
) {
	assert(result != nil && artifact != nil && resolved != nil)
	resolved^[remote_dependency_key(artifact.request)] = true
	for i := before_interfaces; i < len(result.interfaces); i += 1 {
		resolved^[result.interfaces[i].key] = true
	}
	for i := before_sources; i < len(result.sources); i += 1 {
		resolved^[result.sources[i].key] = true
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
	candidates := unseen_requests(requests, nil, context.temp_allocator)
	if len(candidates) > 0 {
		if pool != nil && len(candidates) > 1 {
			append_cache_artifacts_parallel(&out, candidates[:], config, state, pool, allocator)
		} else {
			append_cache_artifacts(&out, candidates[:], config, state, allocator)
		}
	}
	append_typepool_cache_artifacts(&out, candidates[:], config, state, allocator)
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
		if typepool_source_has_pending_expansion(record.source_text, context.temp_allocator) {
			when TRACE {
				trace_eprintf(
					"[trace - remote_dependencies] Type-pool cache entry needs refetch: %s (artifact id %d)\n",
					record.object_name,
					record.artifact_id,
				)
			}
			continue
		}
		symbols := typepool_source_symbols(record.source_text, context.temp_allocator)
		pool := strings.to_lower(record.object_name, context.temp_allocator)
		appended := false
		for request in requests {
			if !(request.kind == .Type || request.kind == .Symbol) {
				continue
			}
			if request.name != pool && !slice.contains(symbols[:], request.name) {
				continue
			}
			artifact := artifact_from_record(&record, request, .Cache, allocator)
			append(out, artifact)
			appended = true
			when TRACE {
				trace_eprintf(
					"[trace - remote_dependencies] Type-pool cache hit: %s provides %s %s (artifact id %d)\n",
					record.object_name,
					trace_request_kind_text(request.kind),
					request.name,
					record.artifact_id,
				)
			}
		}
		if state != nil && appended {
			state.seen_artifacts[record.artifact_id] = true
		}
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
	if config.adt_client.csrf_token == "" &&
	   adt.ensure_session(config.adt_client, context.temp_allocator) != .None {
		when TRACE {
			for request in filtered {
				trace_eprintf(
					"[trace - remote_dependencies] ADT miss: %s %s (session setup failed)\n",
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
						name = strings.to_lower(shared.object_ref.name, context.temp_allocator),
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
	for request in candidates {
		if !(request.kind == .Type || request.kind == .Symbol) {
			continue
		}
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
		pool = strings.to_lower(strings.trim_space(pool), context.temp_allocator)
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
		source, ok := cached_typepool_source(
			config.cache,
			config.profile,
			pool,
			context.temp_allocator,
		)
		if !ok {
			when TRACE {
				trace_eprintf("[trace - remote_dependencies] Type-pool source cache miss: %s\n", pool)
			}
			raw, source_err := adt.fetch_typepool_source(
				config.adt_client,
				pool,
				context.temp_allocator,
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
			source = expanded_typepool_dependency_source(
				config.adt_client,
				raw,
				context.temp_allocator,
			)
			when TRACE {
				trace_eprintf(
					"[trace - remote_dependencies] Type-pool source fetch ok: %s (bytes=%d)\n",
					pool,
					len(source),
				)
			}
			store_typepool_source(config.cache, config.profile, pool, source)
		} else {
			when TRACE {
				trace_eprintf("[trace - remote_dependencies] Type-pool source cache hit: %s\n", pool)
			}
		}
		append(
			&out,
			Artifact {
				request = clone_request(request, allocator),
				source_kind = .Type_Pool,
				object_kind = strings.clone(TYPEPOOL_OBJECT_KIND, allocator),
				object_name = strings.clone(pool, allocator),
				object_uri = typepool_object_uri(pool, allocator),
				object_type = strings.clone(TYPEPOOL_OBJECT_TYPE, allocator),
				file_extension = "abap",
				source_text = strings.clone(source, allocator),
			},
		)
	}
	return out
}

cached_typepool_source :: proc(
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	pool: string,
	allocator: mem.Allocator,
) -> (
	string,
	bool,
) {
	if store == nil || profile == nil {
		return "", false
	}
	record, ok, err := dep_store.find_artifact_by_kind_name(
		store,
		profile,
		TYPEPOOL_OBJECT_KIND,
		pool,
		allocator,
	)
	if err != .None || !ok {
		return "", false
	}
	if typepool_source_has_pending_expansion(record.source_text, allocator) {
		return "", false
	}
	return record.source_text, true
}

store_typepool_source :: proc(
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	pool, source: string,
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
	symbols := typepool_source_symbols(source, store_allocator)
	uri := typepool_object_uri(pool, store_allocator)
	artifact := dep_store.Stored_Artifact_Input {
		package_name     = pool,
		object_kind      = TYPEPOOL_OBJECT_KIND,
		object_name      = pool,
		object_uri       = uri,
		object_type      = TYPEPOOL_OBJECT_TYPE,
		description      = "Type-pool source",
		file_extension   = "abap",
		source_text      = source,
		fetched_at       = fetched_at,
		typepool_symbols = symbols[:],
	}
	_, _ = dep_store.put_artifact(store, profile, &artifact, store_allocator)
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
	if object_kind_is_ddic(record.object_kind) &&
	   !source_is_xml(record.object_kind, record.file_extension, record.source_text) {
		return !remote_dependency_file_extension_is_ddic(record.file_extension)
	}
	return(
		request.kind == .Type &&
		object_kind_is_ddic(record.object_kind) &&
		source_is_xml(record.object_kind, record.file_extension, record.source_text) &&
		strings.contains(record.source_text, "ddicIncludeName") \
	)
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
	if config.adt_client.csrf_token == "" &&
	   adt.ensure_session(config.adt_client, context.temp_allocator) != .None {
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
