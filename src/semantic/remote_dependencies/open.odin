package abap_frontend_semantic_remote_dependencies

import "src:adt"
import dep_store "src:dependency_store"
import analyze "src:semantic/analyze"
import deps "src:semantic/dependencies"

import "core:mem"
import "core:strings"

open_remote_dependency_source :: proc(
	config: ^Dependency_Config,
	object_kind, object_name: string,
	allocator: mem.Allocator,
) -> (analyze.Source_Input, bool, string) {
	if config == nil || strings.trim_space(object_name) == "" {
		return {}, false, "missing dependency object"
	}
	candidate, candidate_ok := remote_dependency_candidate_for_object(
		object_kind,
		object_name,
	)
	if !candidate_ok {
		return {}, false, "unsupported dependency object kind"
	}

	if input, ok, err := open_remote_dependency_source_from_cache(
	   config,
	   candidate,
	   object_kind,
	   object_name,
	   allocator,
	   );
	   err != "" || ok {
		return input, ok, err
	}
	if input, ok := open_remote_dependency_source_from_local_exports(
	   config,
	   candidate,
	   allocator,
	   );
	   ok {
		return input, true, ""
	}
	if input, ok := open_remote_dependency_source_from_adt(
	   config,
	   candidate,
	   object_kind,
	   object_name,
	   allocator,
	   );
	   ok {
		return input, true, ""
	}
	return {}, false, "dependency object source not found"
}

open_remote_dependency_source_from_cache :: proc(
	config: ^Dependency_Config,
	candidate: deps.Remote_Dependency_Candidate,
	object_kind, object_name: string,
	allocator: mem.Allocator,
) -> (analyze.Source_Input, bool, string) {
	if config.cache == nil {
		return {}, false, ""
	}
	record: dep_store.Stored_Artifact_Record
	ok := false
	err := dep_store.Store_Error.None
	if config.cache_any_profile {
		record, ok, err = dep_store.find_artifact_for_candidate_any_profile(
			config.cache,
			object_name,
			dep_store_candidate_kind(candidate.kind),
			allocator,
		)
		if ok && !strings.equal_fold(record.object_kind, object_kind) {
			ok = false
		}
	} else if config.profile != nil {
		record, ok, err = dep_store.find_artifact_by_kind_name(
			config.cache,
			config.profile,
			object_kind,
			object_name,
			allocator,
		)
	} else {
		return {}, false, ""
	}
	if err != .None {
		return {}, false, "dependency store lookup failed"
	}
	if !ok {
		return {}, false, ""
	}
	return open_source_input_from_dependency_record(&record, candidate, allocator), true, ""
}

open_remote_dependency_source_from_local_exports :: proc(
	config: ^Dependency_Config,
	candidate: deps.Remote_Dependency_Candidate,
	allocator: mem.Allocator,
) -> (analyze.Source_Input, bool) {
	if len(config.local_export_roots) == 0 {
		return {}, false
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, 0, 1, allocator)
	dependencies := make([dynamic]analyze.Source_Input, 0, 1, allocator)
	remote := [?]deps.Remote_Dependency_Candidate{candidate}
	if !add_local_export_matches(
	   &candidates,
	   &dependencies,
	   remote[:],
	   config.cache if config.cache != nil && config.profile != nil else nil,
	   config.profile if config.cache != nil && config.profile != nil else nil,
	   config.local_export_roots,
	   "",
	   allocator,
	   ) {
		return {}, false
	}
	if len(dependencies) > 0 {
		input := source_input_clone(dependencies[0], allocator)
		input.role = .Full_Source
		return input, true
	}
	if len(candidates) > 0 {
		input := source_input_clone(candidates[0].input, allocator)
		input.role = .Full_Source
		return input, true
	}
	return {}, false
}

open_remote_dependency_source_from_adt :: proc(
	config: ^Dependency_Config,
	candidate: deps.Remote_Dependency_Candidate,
	object_kind, object_name: string,
	allocator: mem.Allocator,
) -> (analyze.Source_Input, bool) {
	if config.adt_client == nil {
		return {}, false
	}
	if config.adt_client.csrf_token == "" &&
	   adt.ensure_session(config.adt_client, context.temp_allocator) != .None {
		return {}, false
	}
	result := fetch_adt_candidate(
		config.adt_client,
		candidate,
		config.cache if config.cache != nil && config.profile != nil else nil,
		config.profile if config.cache != nil && config.profile != nil else nil,
		false,
		allocator,
		context.temp_allocator,
	)
	if result == nil {
		return {}, false
	}
	for &entry in result.fetched {
		if !strings.equal_fold(entry.object_name, object_name) ||
		   !open_adt_entry_matches_object_kind(&entry, object_kind) {
			continue
		}
		input := source_input_clone(entry.input, allocator)
		input.role = .Full_Source
		return input, true
	}
	return {}, false
}

open_source_input_from_dependency_record :: proc(
	record: ^dep_store.Stored_Artifact_Record,
	candidate: deps.Remote_Dependency_Candidate,
	allocator: mem.Allocator,
) -> analyze.Source_Input {
	input := source_input_from_dependency_record(record, candidate, allocator)
	input.role = .Full_Source
	return input
}

remote_dependency_candidate_for_object :: proc(
	object_kind, object_name: string,
) -> (deps.Remote_Dependency_Candidate, bool) {
	name := strings.trim_space(object_name)
	kind := strings.to_lower(strings.trim_space(object_kind), context.temp_allocator)
	if name == "" || kind == "" {
		return {}, false
	}
	candidate := deps.Remote_Dependency_Candidate{name = name}
	switch kind {
	case "include":
		candidate.kind = .Include
	case "message-class":
		candidate.kind = .Message_Class
	case "report":
		candidate.kind = .Report
	case "function-module", "function-group":
		candidate.kind = .Function
	case "global-interface":
		candidate.kind = .Static
		candidate.hint = .Interface_Type
	case "global-class":
		candidate.kind = .Static
		candidate.hint = .Object_Type
	case TYPEPOOL_OBJECT_KIND:
		candidate.kind = .Type
	case:
		if dependency_object_kind_is_ddic(kind) {
			candidate.kind = .Type
		} else {
			return {}, false
		}
	}
	return candidate, true
}

open_adt_entry_matches_object_kind :: proc(
	entry: ^Adt_Fetched_Object,
	object_kind: string,
) -> bool {
	kind := strings.to_lower(strings.trim_space(object_kind), context.temp_allocator)
	object_type := strings.to_lower(entry.object_type, context.temp_allocator)
	switch kind {
	case "global-class":
		return strings.has_prefix(object_type, "clas/")
	case "global-interface":
		return strings.has_prefix(object_type, "intf/")
	case "include":
		return object_type == "prog/i"
	case "report":
		return object_type == "prog/p"
	case "function-module":
		return object_type == "fugr/ff"
	case "function-group":
		return object_type == "fugr/f"
	case "message-class":
		return object_type == "msag/n"
	case TYPEPOOL_OBJECT_KIND:
		return object_type == strings.to_lower(TYPEPOOL_OBJECT_TYPE, context.temp_allocator)
	case:
		if dependency_object_kind_is_ddic(kind) {
			return strings.equal_fold(adt.infer_ddic_manifest_kind_from_object_type(entry.object_type), kind)
		}
	}
	return false
}
