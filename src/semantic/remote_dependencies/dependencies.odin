package abap_frontend_semantic_remote_dependencies

import "src:adt"
import ddic_xml "src:ddic_xml"
import dep_store "src:dependency_store"
import execution "src:execution"
import analyze "src:semantic/analyze"
import deps "src:semantic/dependencies"
import uri_key "src:uri_key"

import "core:fmt"
import "core:mem"
import "core:strings"

trace_eprintf :: fmt.eprintf

@(private)
dep_store_candidate_kind :: proc(
	kind: deps.Remote_Dependency_Kind,
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
remote_candidate_kind_text :: proc(kind: deps.Remote_Dependency_Kind) -> string {
	return dep_store.candidate_kind_text(dep_store_candidate_kind(kind))
}

Dependency_Config :: struct {
	cache:              ^dep_store.Dependency_Store,
	profile:            ^dep_store.Dependency_Profile,
	cache_any_profile:  bool,
	local_export_roots: []string,
	adt_client:         ^adt.Client,
}

Dependency_State :: struct {
	seen_artifacts:           map[i64]bool,
	seen_local_candidates:    map[deps.Remote_Dependency_Key]bool,
	seen_adt_candidates:      map[deps.Remote_Dependency_Key]bool,
	seen_typepool_candidates: map[deps.Remote_Dependency_Key]bool,
}

dependency_state_make :: proc(allocator: mem.Allocator) -> Dependency_State {
	return Dependency_State {
		seen_artifacts           = make(map[i64]bool, 16, allocator),
		seen_local_candidates    = make(map[deps.Remote_Dependency_Key]bool, 64, allocator),
		seen_adt_candidates      = make(map[deps.Remote_Dependency_Key]bool, 64, allocator),
		seen_typepool_candidates = make(map[deps.Remote_Dependency_Key]bool, 64, allocator),
	}
}

Cache_Phase_Result :: struct {
	added:            bool,
	adt_candidates:   [dynamic]deps.Remote_Dependency_Candidate,
	local_candidates: [dynamic]deps.Remote_Dependency_Candidate,
}

resolve_dependency_candidates :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	remote_candidates: []deps.Remote_Dependency_Candidate,
	config: ^Dependency_Config,
	state: ^Dependency_State,
	pool: ^execution.Pool,
	target_uri: string,
) -> int {
	if len(remote_candidates) == 0 {
		return 0
	}
	old_candidate_count := len(candidates^)
	old_dependency_count := len(dependencies^)
	has_cache := config.cache != nil
	has_profile := config.profile != nil
	cache_result := Cache_Phase_Result {
		adt_candidates = make(
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
	if has_cache {
		cache_candidates := unseen_remote_candidates(remote_candidates, nil, context.temp_allocator)
		connection_key := adt.client_connection_key(config.adt_client, context.temp_allocator) if config.adt_client != nil else ""
		cache_result = add_dependency_cache_matches(
			candidates,
			dependencies,
			cache_candidates[:],
			config.cache,
			config.profile,
			config.cache_any_profile || !has_profile,
			connection_key,
			&state.seen_artifacts,
			pool,
			target_uri,
			"session_cache_any" if config.cache_any_profile || !has_profile else "session_cache",
		)
		if cache_result.added {
			return dependency_input_count_since(candidates, dependencies, old_candidate_count, old_dependency_count)
		}
	} else {
		for candidate in remote_candidates {
			append(&cache_result.adt_candidates, candidate)
			append(&cache_result.local_candidates, candidate)
		}
	}

	if config.adt_client != nil {
		adt_candidates := unseen_remote_candidates(
			cache_result.adt_candidates[:],
			&state.seen_adt_candidates,
			context.temp_allocator,
		)
		if len(adt_candidates) > 0 &&
		   add_adt_matches_with_client(
				candidates,
				dependencies,
				adt_candidates[:],
				config.cache if has_cache && has_profile else nil,
				config.profile if has_cache && has_profile else nil,
				config.adt_client,
				pool,
				target_uri,
			) {
			return dependency_input_count_since(candidates, dependencies, old_candidate_count, old_dependency_count)
		}
	}

	if config.adt_client != nil && adt.typepool_resolver_enabled(config.adt_client) {
		typepool_candidates := unseen_remote_candidates(
			cache_result.local_candidates[:],
			&state.seen_typepool_candidates,
			context.temp_allocator,
		)
		if len(typepool_candidates) > 0 &&
		   add_typepool_resolver_matches(
				candidates,
				dependencies,
				typepool_candidates[:],
				config.cache if has_cache && has_profile else nil,
				config.profile if has_cache && has_profile else nil,
				config.adt_client,
				pool,
				target_uri,
			) {
			return dependency_input_count_since(candidates, dependencies, old_candidate_count, old_dependency_count)
		}
	}

	if len(config.local_export_roots) > 0 {
		local_candidates := unseen_remote_candidates(
			cache_result.local_candidates[:],
			&state.seen_local_candidates,
			context.temp_allocator,
		)
		if len(local_candidates) > 0 &&
		   add_local_export_matches(
				candidates,
				dependencies,
				local_candidates[:],
				config.cache if has_cache && has_profile else nil,
				config.profile if has_cache && has_profile else nil,
				config.local_export_roots,
				target_uri,
				candidates.allocator,
			) {
			return dependency_input_count_since(candidates, dependencies, old_candidate_count, old_dependency_count)
		}
	}
	return dependency_input_count_since(candidates, dependencies, old_candidate_count, old_dependency_count)
}

@(private)
dependency_input_count_since :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	old_candidate_count: int,
	old_dependency_count: int,
) -> int {
	return len(candidates^) - old_candidate_count + len(dependencies^) - old_dependency_count
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
	remote_candidates: []deps.Remote_Dependency_Candidate,
	seen: ^map[deps.Remote_Dependency_Key]bool,
	temp_allocator: mem.Allocator,
) -> [dynamic]deps.Remote_Dependency_Candidate {
	out := make(
		[dynamic]deps.Remote_Dependency_Candidate,
		0,
		len(remote_candidates),
		temp_allocator,
	)
	for candidate in remote_candidates {
		if candidate.kind == .Type && analyze.is_builtin_type_name(candidate.name) {
			continue
		}
		if seen == nil {
			append(&out, candidate)
			continue
		}
		key := deps.Remote_Dependency_Key {
			name = candidate.name,
			kind = candidate.kind,
			hint = candidate.hint,
		}
		if key in seen^ {
			continue
		}
		seen_key := key
		seen_key.name = strings.clone(candidate.name, seen.allocator)
		seen^[seen_key] = true
		append(&out, candidate)
	}
	return out
}

append_dependency_input :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	input: analyze.Source_Input,
	candidate: deps.Remote_Dependency_Candidate,
	object_name: string,
) {
	if candidate.kind == .Include {
		owned := source_input_clone(input, candidates.allocator)
		append(
			candidates,
			analyze.Project_Candidate_Input {
				input = owned,
				object_name = strings.clone(
					object_name if object_name != "" else candidate.name,
					candidates.allocator,
				),
			},
		)
		return
	}

	owned := source_input_clone(input, dependencies.allocator)
	append(dependencies, owned)
}

source_input_clone :: proc(
	input: analyze.Source_Input,
	allocator: mem.Allocator,
) -> analyze.Source_Input {
	return analyze.Source_Input {
		uri = strings.clone(input.uri, allocator),
		source = strings.clone(input.source, allocator),
		mode = input.mode,
	}
}

add_dependency_source_input :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	dependencies: ^[dynamic]analyze.Source_Input,
	candidate: deps.Remote_Dependency_Candidate,
	uri, object_name, object_kind, file_extension, source: string,
	uri_keys: ^map[string]bool,
	temp_allocator: mem.Allocator,
) -> bool {
	if !project_input_uri_key_add_if_missing(uri_keys, uri) {
		return false
	}
	input_source := dependency_input_source(
		candidate,
		object_name,
		object_kind,
		file_extension,
		source,
		temp_allocator,
	)
	input := analyze.Source_Input {
		uri    = uri,
		source = input_source,
		mode   = .Dependency_Interface,
	}
	append_dependency_input(candidates, dependencies, input, candidate, object_name)
	return true
}

dependency_input_source :: proc(
	candidate: deps.Remote_Dependency_Candidate,
	object_name, object_kind, file_extension, source: string,
	allocator: mem.Allocator,
) -> string {
	if dependency_source_is_xml(object_kind, file_extension, source) {
		if candidate.kind == .Type {
			return ddic_xml.dependency_source(object_name, object_kind, source, allocator)
		}
		return ""
	}
	return source
}

project_input_uri_keys :: proc(
	target_uri: string,
	dependencies: []analyze.Source_Input,
	candidates: []analyze.Project_Candidate_Input,
	extra: int,
	allocator: mem.Allocator,
) -> map[string]bool {
	keys := make(map[string]bool, 1 + len(dependencies) + len(candidates) + extra, allocator)
	_ = project_input_uri_key_add_if_missing(&keys, target_uri)
	for input in dependencies {
		_ = project_input_uri_key_add_if_missing(&keys, input.uri)
	}
	for candidate in candidates {
		_ = project_input_uri_key_add_if_missing(&keys, candidate.input.uri)
	}
	return keys
}

project_input_uri_key_exists :: proc(
	keys: ^map[string]bool,
	uri: string,
	allocator: mem.Allocator,
) -> bool {
	key := uri_key.normalized_uri_path_key(uri, allocator)
	return key in keys^
}

project_input_uri_key_add_if_missing :: proc(keys: ^map[string]bool, uri: string) -> bool {
	key := uri_key.normalized_uri_path_key(uri, keys.allocator)
	if key in keys^ {
		return false
	}
	keys^[key] = true
	return true
}
