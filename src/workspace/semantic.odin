package abap_frontend_workspace

import adt "src:adt"
import execution "src:execution"
import "src:parser"
import remote_deps "src:remote_dependencies"
import "src:semantic"
import string_interner "src:string_interner"

import "core:mem"
import "core:os"
import "core:strings"

REMOTE_DEPENDENCY_MAX_ITERATIONS :: 32

Analysis_Result :: struct {
	session:       semantic.Semantic_Graph_Session,
	last_update:   semantic.Semantic_Graph_Update_Result,
	remote_state:  remote_deps.State,
	remote_result: remote_deps.Result,
	ok:            bool,
	used_manifest: bool,
	error:         string,
}

analyze_workspace :: proc(
	workspace: ^Workspace,
	include_paths: []string,
	pool: ^execution.Pool,
	options: Options,
	allocator: mem.Allocator,
) -> Analysis_Result {
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
	files := make([dynamic]semantic.Workspace_File_Input, 0, len(paths), allocator)
	for path in paths {
		input, ok := workspace_file_input_from_path(path, allocator)
		if !ok {
			return Analysis_Result{ok = false, error = "failed to read workspace file"}
		}
		append(&files, input)
	}
	result := analyze_inputs(workspace, files[:], pool, allocator)
	result.used_manifest = workspace.has_manifest
	return result
}

analyze_path :: proc(
	workspace: ^Workspace,
	target_path: string,
	include_paths: []string,
	pool: ^execution.Pool,
	options: Options,
	allocator: mem.Allocator,
) -> Analysis_Result {
	_ = options
	assert(pool != nil)
	target_abs, target_ok := absolute_clean_path(target_path, allocator)
	if !target_ok {
		return Analysis_Result{ok = false, error = "invalid target path"}
	}
	files := make([dynamic]semantic.Workspace_File_Input, 0, 1 + len(include_paths), allocator)
	target, ok := workspace_file_input_from_path(target_abs, allocator)
	if !ok {
		return Analysis_Result{ok = false, error = "failed to read target file"}
	}
	append(&files, target)
	for include_path in include_paths {
		abs_path, abs_ok := absolute_clean_path(include_path, allocator)
		if !abs_ok {
			continue
		}
		include, include_ok := workspace_file_input_from_path(abs_path, allocator)
		if include_ok {
			append(&files, include)
		}
	}
	result := analyze_inputs(workspace, files[:], pool, allocator)
	result.used_manifest = workspace.has_manifest
	return result
}

analyze_inputs :: proc(
	workspace: ^Workspace,
	files: []semantic.Workspace_File_Input,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) -> Analysis_Result {
	remote_config := remote_dependency_config_from_workspace(workspace)
	remote_state := remote_deps.state_make(allocator)
	session := semantic.semantic_graph_session_make(nil, allocator)
	update := semantic.Semantic_Graph_Update {
		changed_files            = files,
		external_frontier_stable = false,
	}
	last := semantic.semantic_graph_session_apply_update(&session, update)
	remote_result := remote_deps.result_make(allocator)

	frontier_flushed := false
	for _ in 0 ..< REMOTE_DEPENDENCY_MAX_ITERATIONS {
		if len(last.new_fetch_requests) == 0 {
			semantic.semantic_graph_update_result_destroy(&last)
			last = semantic.semantic_graph_session_apply_update(
				&session,
				semantic.Semantic_Graph_Update{external_frontier_stable = true},
			)
			frontier_flushed = true
			break
		}

		requests := remote_requests_from_unresolved_candidates(
			session.interner,
			last.new_fetch_requests[:],
			context.temp_allocator,
		)
		remote_result = remote_deps.resolve_requests(
			requests[:],
			&remote_config,
			&remote_state,
			pool,
			allocator,
		)
		external_interfaces := external_interface_inputs_from_remote(
			session.interner,
			remote_result.interfaces[:],
			context.temp_allocator,
		)
		external_sources := external_source_inputs_from_remote(
			remote_result.sources[:],
			context.temp_allocator,
		)
		semantic.semantic_graph_update_result_destroy(&last)
		if len(external_interfaces) == 0 && len(external_sources) == 0 {
			blocked := blocked_keys_from_requests(
				session.interner,
				requests[:],
				context.temp_allocator,
			)
			last = semantic.semantic_graph_session_apply_update(
				&session,
				semantic.Semantic_Graph_Update {
					external_frontier_stable = true,
					blocked_dependencies = blocked[:],
				},
			)
			frontier_flushed = true
			break
		}
		last = semantic.semantic_graph_session_apply_update(
			&session,
			semantic.Semantic_Graph_Update {
				fetched_external_objects = external_interfaces[:],
				fetched_external_sources = external_sources[:],
				external_frontier_stable = false,
			},
		)
	}
	if !frontier_flushed {
		semantic.semantic_graph_update_result_destroy(&last)
		last = semantic.semantic_graph_session_apply_update(
			&session,
			semantic.Semantic_Graph_Update{external_frontier_stable = true},
		)
	}

	return Analysis_Result {
		session = session,
		last_update = last,
		remote_state = remote_state,
		remote_result = remote_result,
		ok = true,
	}
}

analysis_result_destroy :: proc(result: ^Analysis_Result, allocator: mem.Allocator) {
	_ = allocator
	semantic.semantic_graph_update_result_destroy(&result.last_update)
	semantic.semantic_graph_session_destroy(&result.session)
	result^ = {}
}

workspace_file_input_from_path :: proc(
	path: string,
	allocator: mem.Allocator,
) -> (
	semantic.Workspace_File_Input,
	bool,
) {
	source, source_ok := read_text_file(path, allocator)
	if !source_ok {
		return {}, false
	}
	parsed := parser.parse(source, path, allocator)
	return semantic.Workspace_File_Input {
			path = strings.clone(path, allocator),
			root = parsed.root,
			kind = .Unknown,
		},
		true
}

remote_dependency_config_from_workspace :: proc(workspace: ^Workspace) -> remote_deps.Config {
	config := remote_deps.Config {
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

remote_requests_from_unresolved_candidates :: proc(
	interner: ^string_interner.Interner,
	candidates: []semantic.Checker_Unresolved_Candidate,
	allocator: mem.Allocator,
) -> [dynamic]remote_deps.Request {
	out := make([dynamic]remote_deps.Request, 0, len(candidates), allocator)
	for candidate in candidates {
		name := string_interner.load(interner, candidate.name)
		request, ok := remote_request_from_candidate(candidate, name)
		if !ok {
			continue
		}
		append(&out, request)
	}
	return remote_deps.normalize_requests(out[:], allocator)
}

remote_request_from_candidate :: proc(
	candidate: semantic.Checker_Unresolved_Candidate,
	name: string,
) -> (
	remote_deps.Request,
	bool,
) {
	if strings.trim_space(name) == "" {
		return {}, false
	}
	request := remote_deps.Request {
		name = name,
	}
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

external_interface_inputs_from_remote :: proc(
	interner: ^string_interner.Interner,
	inputs: []remote_deps.Interface_AST,
	allocator: mem.Allocator,
) -> [dynamic]semantic.External_Interface_Input {
	out := make([dynamic]semantic.External_Interface_Input, 0, len(inputs), allocator)
	for input in inputs {
		name := string_interner.insert(interner, input.key.name)
		if !string_interner.is_valid(name) {
			continue
		}
		append(
			&out,
			semantic.External_Interface_Input {
				key = semantic.Semantic_Object_Key {
					kind = external_kind_from_remote(input),
					name = name,
				},
				path = input.path,
				root = input.root,
				source_hash = input.source_hash,
				generation = input.generation,
				role = external_role_from_remote(input.role),
			},
		)
	}
	return out
}

external_source_inputs_from_remote :: proc(
	inputs: []remote_deps.Source_AST,
	allocator: mem.Allocator,
) -> [dynamic]semantic.External_Source_Input {
	out := make([dynamic]semantic.External_Source_Input, 0, len(inputs), allocator)
	for input in inputs {
		append(
			&out,
			semantic.External_Source_Input {
				path = input.path,
				root = input.root,
				provided_names = input.provided_names[:],
				source_hash = input.source_hash,
				generation = input.generation,
			},
		)
	}
	return out
}

blocked_keys_from_requests :: proc(
	interner: ^string_interner.Interner,
	requests: []remote_deps.Request,
	allocator: mem.Allocator,
) -> [dynamic]semantic.Semantic_Object_Key {
	out := make([dynamic]semantic.Semantic_Object_Key, 0, len(requests), allocator)
	for request in requests {
		name := string_interner.insert(interner, request.name)
		if !string_interner.is_valid(name) {
			continue
		}
		append(
			&out,
			semantic.Semantic_Object_Key {
				kind = external_kind_from_remote_key(remote_deps.remote_dependency_key(request)),
				name = name,
			},
		)
	}
	return out
}

external_kind_from_remote :: proc(
	input: remote_deps.Interface_AST,
) -> semantic.External_Candidate_Kind {
	return external_kind_from_remote_role_and_key(input.role, input.key)
}

external_kind_from_remote_role_and_key :: proc(
	role: remote_deps.Remote_Dependency_Object_Role,
	key: remote_deps.Remote_Dependency_Key,
) -> semantic.External_Candidate_Kind {
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
	return external_kind_from_remote_key(key)
}

external_kind_from_remote_key :: proc(
	key: remote_deps.Remote_Dependency_Key,
) -> semantic.External_Candidate_Kind {
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

external_role_from_remote :: proc(
	role: remote_deps.Remote_Dependency_Object_Role,
) -> semantic.External_Interface_Object_Role {
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


read_text_file :: proc(path: string, allocator: mem.Allocator) -> (string, bool) {
	data, err := os.read_entire_file(path, allocator)
	if err != nil {
		return "", false
	}
	return string(data), true
}

workspace_local_export_roots :: proc(
	manifest: ^Workspace_Manifest,
	allocator: mem.Allocator,
) -> [dynamic]string {
	roots := make([dynamic]string, 0, len(manifest.local_export_roots), allocator)
	if strings.equal_fold(manifest.dependency_source, "adt-first") {
		return roots
	}
	for root in manifest.local_export_roots {
		path, ok := manifest_absolute_path(manifest.root_path, root, allocator)
		if ok {
			append(&roots, path)
		}
	}
	return roots
}

default_workspace_manifest :: proc(
	root_path: string,
	allocator: mem.Allocator,
) -> Workspace_Manifest {
	return Workspace_Manifest {
		root_path = strings.clone(root_path, allocator),
		connection = "default",
		dependency_source = "local-first",
		local_export_roots = make([dynamic]string, 0, 2, allocator),
		units = make([dynamic]Manifest_Unit, 0, 4, allocator),
	}
}

init_workspace_adt :: proc(workspace: ^Workspace, allocator: mem.Allocator) {
	dotenv, dotenv_err := adt.load_dotenv_defaults(workspace.root_path, allocator)
	if dotenv_err != .None {
		return
	}
	defer adt.dotenv_defaults_destroy(&dotenv, allocator)
	workspace.has_dotenv = len(dotenv.values) > 0

	overrides := adt.Connection_Overrides{}
	config, config_err := adt.connection_config_from_sources(&overrides, &dotenv, allocator)
	if config_err != .None {
		return
	}
	workspace.adt_config = config
	adt.client_init(&workspace.adt_client, workspace.adt_config, allocator)
	workspace.has_adt = true
}
