package abap_frontend_workspace

import execution "src:execution"
import "src:parser"
import remote_deps2 "src:remote_dependencies"
import analyze "src:semantic/analyze"
import semantic2 "src:semantic2"
import string_interner "src:string_interner"

import "core:mem"
import "core:strings"

SEMANTIC2_REMOTE_DEPENDENCY_MAX_ITERATIONS :: 32

Semantic2_Analysis_Result :: struct {
	session:      semantic2.Semantic_Graph_Session,
	last_update:  semantic2.Semantic_Graph_Update_Result,
	remote_state: remote_deps2.State,
	remote_result: remote_deps2.Result,
	ok:           bool,
	used_manifest: bool,
	error:        string,
}

analyze_workspace_semantic2 :: proc(
	workspace: ^Workspace,
	include_paths: []string,
	pool: ^execution.Pool,
	options: Options,
	allocator: mem.Allocator,
) -> Semantic2_Analysis_Result {
	_ = options
	assert(pool != nil)
	paths := make([dynamic]string, 0, 32, context.temp_allocator)
	collect_workspace_abap_files(workspace.root_path, &paths, context.temp_allocator)
	for include_path in include_paths {
		abs_path, ok := absolute_clean_path(include_path, context.temp_allocator)
		if ok {
			append(&paths, abs_path)
		}
	}
	files := make([dynamic]semantic2.Workspace_File_Input, 0, len(paths), allocator)
	for path in paths {
		input, ok := semantic2_workspace_file_input_from_path(path, allocator)
		if !ok {
			return Semantic2_Analysis_Result{ok = false, error = "failed to read workspace file"}
		}
		append(&files, input)
	}
	result := workspace_semantic2_analyze_inputs(workspace, files[:], pool, allocator)
	result.used_manifest = workspace.has_manifest
	return result
}

analyze_path_semantic2 :: proc(
	workspace: ^Workspace,
	target_path: string,
	include_paths: []string,
	pool: ^execution.Pool,
	options: Options,
	allocator: mem.Allocator,
) -> Semantic2_Analysis_Result {
	_ = options
	assert(pool != nil)
	target_abs, target_ok := absolute_clean_path(target_path, allocator)
	if !target_ok {
		return Semantic2_Analysis_Result{ok = false, error = "invalid target path"}
	}
	files := make([dynamic]semantic2.Workspace_File_Input, 0, 1 + len(include_paths), allocator)
	target, ok := semantic2_workspace_file_input_from_path(target_abs, allocator)
	if !ok {
		return Semantic2_Analysis_Result{ok = false, error = "failed to read target file"}
	}
	append(&files, target)
	for include_path in include_paths {
		abs_path, abs_ok := absolute_clean_path(include_path, allocator)
		if !abs_ok {
			continue
		}
		include, include_ok := semantic2_workspace_file_input_from_path(abs_path, allocator)
		if include_ok {
			append(&files, include)
		}
	}
	result := workspace_semantic2_analyze_inputs(workspace, files[:], pool, allocator)
	result.used_manifest = workspace.has_manifest
	return result
}

workspace_semantic2_analyze_inputs :: proc(
	workspace: ^Workspace,
	files: []semantic2.Workspace_File_Input,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) -> Semantic2_Analysis_Result {
	remote_config := remote_dependency_config_from_workspace(workspace)
	remote_state := remote_deps2.state_make(allocator)
	session := semantic2.semantic_graph_session_make(nil, allocator)
	update := semantic2.Semantic_Graph_Update {
		changed_files            = files,
		external_frontier_stable = false,
	}
	last := semantic2.semantic_graph_session_apply_update(&session, update)
	remote_result := remote_deps2.result_make(allocator)

	for _ in 0 ..< SEMANTIC2_REMOTE_DEPENDENCY_MAX_ITERATIONS {
		if len(last.new_fetch_requests) == 0 {
			semantic2.semantic_graph_update_result_destroy(&last)
			last = semantic2.semantic_graph_session_apply_update(
				&session,
				semantic2.Semantic_Graph_Update {
					external_frontier_stable = true,
				},
			)
			break
		}

		requests := workspace_semantic2_remote_requests(
			session.interner,
			last.new_fetch_requests[:],
			context.temp_allocator,
		)
		remote_result = remote_deps2.resolve_requests(
			requests[:],
			&remote_config,
			&remote_state,
			pool,
			allocator,
		)
		external_interfaces := workspace_semantic2_external_interface_inputs(
			session.interner,
			remote_result.interfaces[:],
			context.temp_allocator,
		)
		external_sources := workspace_semantic2_external_source_inputs(
			remote_result.sources[:],
			context.temp_allocator,
		)
		semantic2.semantic_graph_update_result_destroy(&last)
		if len(external_interfaces) == 0 && len(external_sources) == 0 {
			blocked := workspace_semantic2_blocked_keys(
				session.interner,
				requests[:],
				context.temp_allocator,
			)
			last = semantic2.semantic_graph_session_apply_update(
				&session,
				semantic2.Semantic_Graph_Update {
					external_frontier_stable = true,
					blocked_dependencies = blocked[:],
				},
			)
			break
		}
		last = semantic2.semantic_graph_session_apply_update(
			&session,
			semantic2.Semantic_Graph_Update {
				fetched_external_objects = external_interfaces[:],
				fetched_external_sources = external_sources[:],
				external_frontier_stable = false,
			},
		)
	}

	return Semantic2_Analysis_Result {
		session       = session,
		last_update   = last,
		remote_state  = remote_state,
		remote_result = remote_result,
		ok            = true,
	}
}

semantic2_analysis_result_destroy :: proc(
	result: ^Semantic2_Analysis_Result,
	allocator: mem.Allocator,
) {
	_ = allocator
	semantic2.semantic_graph_update_result_destroy(&result.last_update)
	semantic2.semantic_graph_session_destroy(&result.session)
	result^ = {}
}

semantic2_workspace_file_input_from_path :: proc(
	path: string,
	allocator: mem.Allocator,
) -> (semantic2.Workspace_File_Input, bool) {
	source, source_ok := read_text_file(path, allocator)
	if !source_ok {
		return {}, false
	}
	parsed := parser.parse(source, path, allocator)
	return semantic2.Workspace_File_Input {
		path = strings.clone(path, allocator),
		root = parsed.root,
		kind = .Unknown,
	}, true
}

remote_dependency_config_from_workspace :: proc(
	workspace: ^Workspace,
) -> remote_deps2.Config {
	config := remote_deps2.Config {
		local_export_roots = workspace.local_export_roots[:],
		cache_any_profile  = !workspace.has_manifest,
		source_order       = .Local_First,
	}
	if strings.equal_fold(workspace.manifest.dependency_source, "adt-first") {
		config.source_order = .ADT_First
	}
	if workspace.has_store {
		config.cache = &workspace.store
		if workspace.has_manifest && workspace.manifest.has_dependency_store {
			config.profile = &workspace.manifest.dependency_store
		} else if !workspace.has_manifest {
			config.profile = &workspace.standalone_profile
		}
	}
	if workspace.has_adt {
		config.adt_client = &workspace.adt_client
	}
	return config
}

workspace_semantic2_remote_requests :: proc(
	interner: ^string_interner.Interner,
	candidates: []semantic2.Checker_Unresolved_Candidate,
	allocator: mem.Allocator,
) -> [dynamic]remote_deps2.Request {
	out := make([dynamic]remote_deps2.Request, 0, len(candidates), allocator)
	for candidate in candidates {
		name := string_interner.load(interner, candidate.name)
		request, ok := workspace_semantic2_remote_request_from_candidate(candidate, name)
		if !ok {
			continue
		}
		append(&out, request)
	}
	return remote_deps2.normalize_requests(out[:], allocator)
}

workspace_semantic2_remote_request_from_candidate :: proc(
	candidate: semantic2.Checker_Unresolved_Candidate,
	name: string,
) -> (remote_deps2.Request, bool) {
	if strings.trim_space(name) == "" {
		return {}, false
	}
	request := remote_deps2.Request{name = name}
	switch candidate.kind {
	case .Include_Source:
		request.kind = .Include
	case .Report:
		request.kind = .Report
	case .Function_Module:
		request.kind = .Function
	case .Class:
		request.kind = .Class
	case .Interface:
		request.kind = .Interface
	case .DDIC_Type, .DDIC_Table, .Type_Pool:
		request.kind = .Type
	case .Message_Class:
		request.kind = .Message_Class
	case .Global_Symbol:
		switch candidate.namespace {
		case .Type:
			request.kind = .Type
		case .Routine:
			request.kind = .Function
		case .Value:
			request.kind = .Symbol
		}
	}
	return request, true
}

workspace_semantic2_external_interface_inputs :: proc(
	interner: ^string_interner.Interner,
	inputs: []remote_deps2.Interface_AST,
	allocator: mem.Allocator,
) -> [dynamic]semantic2.External_Interface_Input {
	out := make([dynamic]semantic2.External_Interface_Input, 0, len(inputs), allocator)
	for input in inputs {
		name := string_interner.insert(interner, input.key.name)
		if !string_interner.is_valid(name) {
			continue
		}
		append(
			&out,
			semantic2.External_Interface_Input {
				key = semantic2.Semantic_Object_Key {
					kind = workspace_semantic2_external_kind_from_remote(input),
					name = name,
				},
				path        = input.path,
				root        = input.root,
				source_hash = input.source_hash,
				generation  = input.generation,
				role        = workspace_semantic2_external_role_from_remote(input.role),
			},
		)
	}
	return out
}

workspace_semantic2_external_source_inputs :: proc(
	inputs: []remote_deps2.Source_AST,
	allocator: mem.Allocator,
) -> [dynamic]semantic2.External_Source_Input {
	out := make([dynamic]semantic2.External_Source_Input, 0, len(inputs), allocator)
	for input in inputs {
		append(
			&out,
			semantic2.External_Source_Input {
				path           = input.path,
				root           = input.root,
				provided_names = input.provided_names[:],
				source_hash    = input.source_hash,
				generation     = input.generation,
			},
		)
	}
	return out
}

workspace_semantic2_blocked_keys :: proc(
	interner: ^string_interner.Interner,
	requests: []remote_deps2.Request,
	allocator: mem.Allocator,
) -> [dynamic]semantic2.Semantic_Object_Key {
	out := make([dynamic]semantic2.Semantic_Object_Key, 0, len(requests), allocator)
	for request in requests {
		name := string_interner.insert(interner, request.name)
		if !string_interner.is_valid(name) {
			continue
		}
		append(
			&out,
			semantic2.Semantic_Object_Key {
				kind = workspace_semantic2_kind_from_remote_key(remote_deps2.remote_dependency_key(request)),
				name = name,
			},
		)
	}
	return out
}

workspace_semantic2_external_kind_from_remote :: proc(
	input: remote_deps2.Interface_AST,
) -> semantic2.External_Candidate_Kind {
	return workspace_semantic2_kind_from_remote_role_and_key(input.role, input.key)
}

workspace_semantic2_kind_from_remote_role_and_key :: proc(
	role: remote_deps2.Remote_Dependency_Object_Role,
	key: remote_deps2.Remote_Dependency_Key,
) -> semantic2.External_Candidate_Kind {
	switch role {
	case .Report:
		return .Report
	case .Function_Module:
		return .Function_Module
	case .Class:
		return .Class
	case .Interface:
		return .Interface
	case .DDIC_Type:
		return .DDIC_Type
	case .DDIC_Table:
		return .DDIC_Table
	case .Type_Pool:
		return .Type_Pool
	case .Unknown:
	}
	return workspace_semantic2_kind_from_remote_key(key)
}

workspace_semantic2_kind_from_remote_key :: proc(
	key: remote_deps2.Remote_Dependency_Key,
) -> semantic2.External_Candidate_Kind {
	switch key.kind {
	case .Include:
		return .Include_Source
	case .Message_Class:
		return .Message_Class
	case .Report:
		return .Report
	case .Function:
		return .Function_Module
	case .Class:
		return .Class
	case .Interface:
		return .Interface
	case .Type:
		return .DDIC_Type
	case .Symbol:
		return .Global_Symbol
	}
	return .Global_Symbol
}

workspace_semantic2_external_role_from_remote :: proc(
	role: remote_deps2.Remote_Dependency_Object_Role,
) -> semantic2.External_Interface_Object_Role {
	switch role {
	case .Report:
		return .Report
	case .Function_Module:
		return .Function_Module
	case .Class:
		return .Class
	case .Interface:
		return .Interface
	case .DDIC_Type:
		return .DDIC_Type
	case .DDIC_Table:
		return .DDIC_Table
	case .Type_Pool:
		return .Type_Pool
	case .Unknown:
	}
	return .Unknown
}

workspace_semantic2_source_input_from_open_source :: proc(
	source: remote_deps2.Open_Source,
	allocator: mem.Allocator,
) -> Workspace_Source_Input {
	return Workspace_Source_Input {
		uri      = strings.clone(source.path, allocator),
		text     = strings.clone(source.source_text, allocator),
		role     = analyze.Source_Input_Role.Full_Source,
		open     = true,
	}
}
