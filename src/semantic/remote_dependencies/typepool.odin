package abap_frontend_semantic_remote_dependencies

import "src:adt"
import "src:ast"
import dep_store "src:dependency_store"
import execution "src:execution"
import "src:parser"
import analyze "src:semantic/analyze"

import base_runtime "base:runtime"
import "core:mem"
import "core:mem/virtual"
import "core:strings"
import "core:time"

TYPEPOOL_OBJECT_KIND :: "type-pool"
TYPEPOOL_OBJECT_TYPE :: "TYPEPOOL"

add_typepool_resolver_matches :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	remote_candidates: []analyze.Remote_Dependency_Candidate,
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	client: ^adt.Client,
	worker_pool: ^execution.Pool,
	target_uri: string,
) -> bool {
	if client == nil || worker_pool == nil || !adt.typepool_resolver_enabled(client) {
		return false
	}
	temp_arena := virtual.arena_temp_begin(cast(^virtual.Arena)context.temp_allocator.data)
	defer virtual.arena_temp_end(temp_arena)

	owner_candidates := make(
		[dynamic]analyze.Remote_Dependency_Candidate,
		0,
		len(remote_candidates),
		context.temp_allocator,
	)
	for candidate in remote_candidates {
		if candidate.kind == .Type || candidate.kind == .Symbol {
			append(&owner_candidates, candidate)
		}
	}
	if len(owner_candidates) == 0 {
		return false
	}
	if client.csrf_token == "" && adt.ensure_session(client, context.temp_allocator) != .None {
		return false
	}

	uri_keys := project_input_uri_keys(
		target_uri,
		dependencies^[:],
		candidates^[:],
		len(remote_candidates),
		context.temp_allocator,
	)

	graph: execution.Graph
	execution.graph_init(&graph, worker_pool, context.temp_allocator)
	result_arenas := make([]mem.Dynamic_Arena, len(owner_candidates), context.temp_allocator)
	owner_tasks := make(
		[dynamic]execution.Task(^Typepool_Owner_Result),
		0,
		len(owner_candidates),
		context.temp_allocator,
	)
	result_backing := base_runtime.heap_allocator()
	for candidate, i in owner_candidates {
		mem.dynamic_arena_init(&result_arenas[i], result_backing, result_backing, alignment = 64)
		task := execution.submit_value(
			&graph,
			execution.worker_executor(worker_pool),
			Typepool_Owner_Payload {
				client = client,
				candidate = candidate,
				result_allocator = mem.dynamic_arena_allocator(&result_arenas[i]),
			},
			typepool_owner_task,
		)
		append(&owner_tasks, task)
	}

	pools := make([dynamic]string, 0, 8, context.temp_allocator)
	seen_pools := make(map[string]bool, 8, context.temp_allocator)
	execution.graph_start(&graph)
	for task, i in owner_tasks {
		result := execution.wait(task)
		if result == nil || result.err != .None {
			when adt.DEPENDENCY_FETCH_TRACE {
				candidate := owner_candidates[i]
				trace_eprintf(
					"adt_fetch\ttypepool\towner_miss\t%s\t%s\t%v\n",
					remote_candidate_kind_text(candidate.kind),
					candidate.name,
					result.err if result != nil else adt.Error.Http,
				)
			}
			mem.dynamic_arena_destroy(&result_arenas[i])
			continue
		}
		pool := strings.to_lower(strings.trim_space(result.pool), context.temp_allocator)
		if pool == "" || pool in seen_pools {
			mem.dynamic_arena_destroy(&result_arenas[i])
			continue
		}
		seen_pools[pool] = true
		append(&pools, pool)
		when adt.DEPENDENCY_FETCH_TRACE {
			trace_eprintf("adt_fetch\ttypepool\towner\t%s\t%s\n", result.candidate.name, pool)
		}
		mem.dynamic_arena_destroy(&result_arenas[i])
	}
	execution.graph_wait(&graph)

	added := false
	remote_pools := make([dynamic]string, 0, len(pools), context.temp_allocator)
	for pool in pools {
		source, ok := cached_typepool_source(store, profile, pool, context.temp_allocator)
		if !ok {
			append(&remote_pools, pool)
			continue
		}
		if add_typepool_source_input(
			candidates,
			dependencies,
			pool,
			source,
			&uri_keys,
			context.temp_allocator,
		) {
			added = true
			when adt.DEPENDENCY_FETCH_TRACE {
				trace_eprintf("adt_fetch\ttypepool\tadd\tcache\t%s\n", pool)
			}
		}
	}
	if len(remote_pools) == 0 {
		execution.graph_destroy(&graph)
		return added
	}

	execution.graph_reset(&graph)
	source_arenas := make([]mem.Dynamic_Arena, len(remote_pools), context.temp_allocator)
	source_tasks := make(
		[dynamic]execution.Task(^Typepool_Source_Result),
		0,
		len(remote_pools),
		context.temp_allocator,
	)
	for pool, i in remote_pools {
		mem.dynamic_arena_init(&source_arenas[i], result_backing, result_backing, alignment = 64)
		task := execution.submit_value(
			&graph,
			execution.worker_executor(worker_pool),
			Typepool_Source_Payload {
				client = client,
				pool = pool,
				result_allocator = mem.dynamic_arena_allocator(&source_arenas[i]),
			},
			typepool_source_task,
		)
		append(&source_tasks, task)
	}
	execution.graph_start(&graph)
	for task, i in source_tasks {
		result := execution.wait(task)
		defer mem.dynamic_arena_destroy(&source_arenas[i])
		if result == nil || result.err != .None {
			when adt.DEPENDENCY_FETCH_TRACE {
				trace_eprintf(
					"adt_fetch\ttypepool\tsource_miss\t%s\t%v\n",
					remote_pools[i],
					result.err if result != nil else adt.Error.Http,
				)
			}
			continue
		}
		store_typepool_source(store, profile, result.pool, result.source, context.temp_allocator)
		if add_typepool_source_input(
			candidates,
			dependencies,
			result.pool,
			result.source,
			&uri_keys,
			context.temp_allocator,
		) {
			added = true
			when adt.DEPENDENCY_FETCH_TRACE {
				trace_eprintf("adt_fetch\ttypepool\tadd\tremote\t%s\n", result.pool)
			}
		}
	}
	execution.graph_wait(&graph)
	execution.graph_destroy(&graph)
	return added
}

Typepool_Owner_Payload :: struct {
	client:           ^adt.Client,
	candidate:        analyze.Remote_Dependency_Candidate,
	result_allocator: mem.Allocator,
}

Typepool_Owner_Result :: struct {
	candidate: analyze.Remote_Dependency_Candidate,
	pool:      string,
	err:       adt.Error,
}

typepool_owner_task :: proc(payload: Typepool_Owner_Payload) -> ^Typepool_Owner_Result {
	result := new(Typepool_Owner_Result, payload.result_allocator)
	result.candidate = payload.candidate
	result.candidate.name = strings.clone(payload.candidate.name, payload.result_allocator)
	pool, err := adt.resolve_typepool_owner(
		payload.client,
		payload.candidate.name,
		context.temp_allocator,
	)
	result.err = err
	if err == .None {
		result.pool = strings.clone(strings.trim_space(pool), payload.result_allocator)
	}
	return result
}

Typepool_Source_Payload :: struct {
	client:           ^adt.Client,
	pool:             string,
	result_allocator: mem.Allocator,
}

Typepool_Source_Result :: struct {
	pool:   string,
	source: string,
	err:    adt.Error,
}

typepool_source_task :: proc(payload: Typepool_Source_Payload) -> ^Typepool_Source_Result {
	result := new(Typepool_Source_Result, payload.result_allocator)
	result.pool = strings.clone(payload.pool, payload.result_allocator)
	raw, err := adt.fetch_typepool_source(payload.client, payload.pool, context.temp_allocator)
	result.err = err
	if err == .None {
		result.source = expanded_typepool_dependency_source(
			payload.client,
			raw,
			payload.result_allocator,
		)
	}
	return result
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
	// Old cached type-pool sources may still contain INCLUDE or macro stubs; refetch them once.
	if typepool_source_has_pending_expansion(record.source_text, allocator) {
		return "", false
	}
	return record.source_text, true
}

store_typepool_source :: proc(
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	pool, source: string,
	allocator: mem.Allocator,
) {
	if store == nil || profile == nil {
		return
	}
	fetched_at, _ := time.time_to_rfc3339(time.now(), allocator = allocator)
	artifact := dep_store.Stored_Artifact_Input {
		package_name   = pool,
		object_kind    = TYPEPOOL_OBJECT_KIND,
		object_name    = pool,
		object_uri     = typepool_object_uri(pool, allocator),
		object_type    = TYPEPOOL_OBJECT_TYPE,
		description    = "Type-pool source",
		file_extension = "abap",
		source_text    = source,
		fetched_at     = fetched_at,
	}
	_, _ = dep_store.put_artifact(store, profile, &artifact, allocator)
}

add_typepool_source_input :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	pool, source: string,
	uri_keys: ^map[string]bool,
	allocator: mem.Allocator,
) -> bool {
	uri := typepool_dependency_uri(pool, allocator)
	if !project_input_uri_key_add_if_missing(uri_keys, uri) {
		return false
	}
	input := analyze.Source_Input {
		uri    = uri,
		source = source,
		mode   = .Dependency_Interface,
	}
	append_dependency_input(
		candidates,
		dependencies,
		input,
		analyze.Remote_Dependency_Candidate{name = pool, kind = .Type},
		pool,
	)
	return true
}

typepool_dependency_source :: proc(source: string, allocator: mem.Allocator) -> string {
	trimmed := strings.trim_left_space(source)
	if !starts_with_ignore_case(trimmed, "TYPE-POOL") || len(trimmed) <= len("TYPE-POOL") {
		return strings.clone(source, allocator)
	}
	next := trimmed[len("TYPE-POOL")]
	if next != ' ' && next != '\t' && next != '\r' && next != '\n' {
		return strings.clone(source, allocator)
	}
	if dot := strings.index_byte(trimmed, '.'); dot >= 0 {
		return strings.clone(strings.trim_left_space(trimmed[dot + 1:]), allocator)
	}
	return strings.clone(source, allocator)
}

expanded_typepool_dependency_source :: proc(
	client: ^adt.Client,
	raw: string,
	allocator: mem.Allocator,
) -> string {
	source := typepool_dependency_source(raw, context.temp_allocator)
	parsed := parser.parse(source, "abapls-typepool-source", context.temp_allocator)
	if typepool_parsed_source_has_includes(parsed.root) {
		out := strings.builder_make(allocator)
		seen := make(map[string]bool, 8, context.temp_allocator)
		wrote := false
		append_expanded_typepool_source(&out, client, source, parsed.root, &seen, &wrote)
		source = strings.to_string(out)
	}
	return expanded_typepool_macro_source(source, allocator)
}

append_expanded_typepool_source :: proc(
	out: ^strings.Builder,
	client: ^adt.Client,
	source: string,
	root: ^ast.File,
	seen: ^map[string]bool,
	wrote: ^bool,
) {
	if root == nil {
		write_typepool_source_part(out, source, wrote)
		return
	}

	last := 0
	for stmt in root.stmts {
		include, ok := stmt.derived_stmt.(^ast.Include_Stmt)
		if !ok {
			continue
		}
		write_typepool_source_part(out, source[last:stmt.range.start], wrote)
		failed := false
		for name in include.names {
			key := strings.to_lower(name.name, context.temp_allocator)
			if key in seen^ {
				continue
			}
			seen^[key] = true
			fetched, err := adt.fetch_source(client, .Include, name.name, "", context.temp_allocator)
			if err != .None {
				failed = true
				continue
			}
			include_source := typepool_dependency_source(fetched.body, context.temp_allocator)
			include_parsed := parser.parse(
				include_source,
				"abapls-typepool-source",
				context.temp_allocator,
			)
			append_expanded_typepool_source(out, client, include_source, include_parsed.root, seen, wrote)
		}
		if failed {
			write_typepool_source_part(out, source[stmt.range.start:stmt.range.end], wrote)
		}
		last = stmt.range.end
	}
	write_typepool_source_part(out, source[last:], wrote)
}

write_typepool_source_part :: proc(out: ^strings.Builder, source: string, wrote: ^bool) {
	part := strings.trim_space(source)
	if part == "" {
		return
	}
	if wrote^ {
		strings.write_byte(out, '\n')
	}
	strings.write_string(out, part)
	wrote^ = true
}

expanded_typepool_macro_source :: proc(source: string, allocator: mem.Allocator) -> string {
	parsed := parser.parse(source, "abapls-typepool-source", context.temp_allocator)
	if parsed.root == nil {
		return strings.clone(source, allocator)
	}
	macros := make(map[string]string, 8, context.temp_allocator)
	out := strings.builder_make(allocator)
	last := 0
	for stmt in parsed.root.stmts {
		#partial switch n in stmt.derived_stmt {
		case ^ast.Macro_Def_Stmt:
			if n.name != "" {
				macros[strings.to_lower(n.name, context.temp_allocator)] = n.body
			}
			strings.write_string(&out, source[last:stmt.range.start])
			last = stmt.range.end
		case ^ast.Macro_Call_Stmt:
			key := strings.to_lower(n.name, context.temp_allocator)
			body, ok := macros[key]
			if !ok {
				continue
			}
			strings.write_string(&out, source[last:stmt.range.start])
			args := typepool_macro_call_args(source[stmt.range.start:stmt.range.end], n.name, context.temp_allocator)
			write_expanded_typepool_macro_body(&out, body, args[:])
			last = stmt.range.end
		}
	}
	strings.write_string(&out, source[last:])
	return strings.to_string(out)
}

write_expanded_typepool_macro_body :: proc(
	out: ^strings.Builder,
	body: string,
	args: []string,
) {
	for i := 0; i < len(body); {
		if body[i] == '&' && i + 1 < len(body) && body[i + 1] >= '1' && body[i + 1] <= '9' {
			arg_index := int(body[i + 1] - '1')
			if arg_index < len(args) {
				strings.write_string(out, args[arg_index])
			}
			i += 2
			continue
		}
		strings.write_byte(out, body[i])
		i += 1
	}
}

typepool_macro_call_args :: proc(call_source, name: string, allocator: mem.Allocator) -> [dynamic]string {
	text := strings.trim_space(call_source)
	if strings.has_suffix(text, ".") {
		text = strings.trim_space(text[:len(text) - 1])
	}
	if starts_with_ignore_case(text, name) {
		text = strings.trim_space(text[len(name):])
	}
	args := make([dynamic]string, 0, 4, allocator)
	for len(text) > 0 {
		end := 0
		for end < len(text) && !ascii_space(text[end]) {
			end += 1
		}
		if end > 0 {
			append(&args, text[:end])
		}
		text = strings.trim_left_space(text[end:])
	}
	return args
}

ascii_space :: #force_inline proc(ch: byte) -> bool {
	return ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n'
}

typepool_source_has_pending_expansion :: proc(source: string, allocator: mem.Allocator) -> bool {
	upper := strings.to_upper(source, context.temp_allocator)
	if !strings.contains(upper, "INCLUDE") &&
	   !strings.contains(upper, "DEFINE") &&
	   !strings.contains(upper, "END-OF-DEFINITION") {
		return false
	}
	parsed := parser.parse(source, "abapls-typepool-source", allocator)
	return typepool_parsed_source_has_includes(parsed.root) ||
	       typepool_parsed_source_has_macros(parsed.root)
}

typepool_parsed_source_has_includes :: proc(root: ^ast.File) -> bool {
	if root == nil {
		return false
	}
	for stmt in root.stmts {
		if _, ok := stmt.derived_stmt.(^ast.Include_Stmt); ok {
			return true
		}
	}
	return false
}

typepool_parsed_source_has_macros :: proc(root: ^ast.File) -> bool {
	if root == nil {
		return false
	}
	for stmt in root.stmts {
		#partial switch _ in stmt.derived_stmt {
		case ^ast.Macro_Def_Stmt,
		     ^ast.Macro_Call_Stmt:
			return true
		}
	}
	return false
}

starts_with_ignore_case :: proc(source, prefix: string) -> bool {
	return len(source) >= len(prefix) && strings.equal_fold(source[:len(prefix)], prefix)
}

typepool_dependency_uri :: proc(pool: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "abapls-typepool:/")
	strings.write_string(&out, strings.to_lower(pool, allocator))
	strings.write_string(&out, ".abap")
	return strings.to_string(out)
}

typepool_object_uri :: proc(pool: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, "type-pool:")
	strings.write_string(&out, strings.to_upper(pool, allocator))
	return strings.to_string(out)
}
