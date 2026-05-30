package abap_frontend_semantic_remote_dependencies

import "src:adt"
import dep_store "src:dependency_store"
import analyze "src:semantic/analyze"

import "core:mem"
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
	target_uri: string,
	temp_allocator: mem.Allocator,
) -> bool {
	if client == nil || !adt.typepool_resolver_enabled(client) {
		return false
	}

	uri_keys := project_input_uri_keys(
		target_uri,
		dependencies^[:],
		candidates^[:],
		len(remote_candidates),
		temp_allocator,
	)
	pools := make([dynamic]string, 0, 8, temp_allocator)
	seen_pools := make(map[string]bool, 8, temp_allocator)
	for candidate in remote_candidates {
		if !typepool_resolver_candidate(candidate) {
			continue
		}
		pool, err := adt.resolve_typepool_owner(client, candidate.name, temp_allocator)
		if err != .None {
			when adt.DEPENDENCY_FETCH_TRACE {
				trace_eprintf(
					"adt_fetch\ttypepool\towner_miss\t%s\t%s\t%v\n",
					remote_candidate_kind_text(candidate.kind),
					candidate.name,
					err,
				)
			}
			continue
		}
		pool = strings.to_lower(strings.trim_space(pool), temp_allocator)
		if pool == "" || pool in seen_pools {
			continue
		}
		seen_pools[pool] = true
		append(&pools, pool)
		when adt.DEPENDENCY_FETCH_TRACE {
			trace_eprintf("adt_fetch\ttypepool\towner\t%s\t%s\n", candidate.name, pool)
		}
	}

	added := false
	for pool in pools {
		source, ok := cached_typepool_source(store, profile, pool, temp_allocator)
		source_kind := "cache"
		if !ok {
			raw, err := adt.fetch_typepool_source(client, pool, temp_allocator)
			if err != .None {
				when adt.DEPENDENCY_FETCH_TRACE {
					trace_eprintf("adt_fetch\ttypepool\tsource_miss\t%s\t%v\n", pool, err)
				}
				continue
			}
			source = typepool_dependency_source(raw, temp_allocator)
			store_typepool_source(store, profile, pool, source, temp_allocator)
			source_kind = "remote"
		}
		if add_typepool_source_input(candidates, dependencies, pool, source, &uri_keys, temp_allocator) {
			added = true
			when adt.DEPENDENCY_FETCH_TRACE {
				trace_eprintf("adt_fetch\ttypepool\tadd\t%s\t%s\n", source_kind, pool)
			}
		}
	}
	return added
}

typepool_resolver_candidate :: proc(candidate: analyze.Remote_Dependency_Candidate) -> bool {
	return candidate.kind == .Type || candidate.kind == .Symbol
}

cached_typepool_source :: proc(
	store: ^dep_store.Dependency_Store,
	profile: ^dep_store.Dependency_Profile,
	pool: string,
	allocator: mem.Allocator,
) -> (string, bool) {
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
		return strings.trim_left_space(trimmed[dot + 1:])
	}
	return strings.clone(source, allocator)
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
