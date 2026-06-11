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
	assert(pool != nil)
	workspace.flags += options.flags
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
	assert(pool != nil)
	workspace.flags += options.flags
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
	result: Analysis_Result
	analysis_result_update_inputs(&result, workspace, files, nil, pool, allocator)
	return result
}

analysis_result_update_inputs :: proc(
	result: ^Analysis_Result,
	workspace: ^Workspace,
	changed_files: []semantic.Workspace_File_Input,
	removed_files: []string,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) -> bool {
	assert(result != nil)
	assert(pool != nil)
	remote_config := remote_dependency_config_from_workspace(workspace)

	if result.ok {
		semantic.semantic_graph_update_result_destroy(&result.last_update)
	} else {
		result.session = semantic.semantic_graph_session_make(nil, allocator)
		result.remote_state = remote_deps.state_make(allocator)
		result.remote_result = remote_deps.result_make(allocator)
		result.ok = true
	}

	result.used_manifest = workspace.has_manifest
	last := semantic.semantic_graph_session_apply_update(
		&result.session,
		semantic.Semantic_Graph_Update {
			changed_files            = changed_files,
			removed_files            = removed_files,
			external_frontier_stable = false,
		},
	)
	result.remote_result = remote_deps.result_make(allocator)

	frontier_flushed := false
	for _ in 0 ..< REMOTE_DEPENDENCY_MAX_ITERATIONS {
		if len(last.new_fetch_requests) == 0 {
			semantic.semantic_graph_update_result_destroy(&last)
			last = semantic.semantic_graph_session_apply_update(
				&result.session,
				semantic.Semantic_Graph_Update{external_frontier_stable = true},
			)
			frontier_flushed = true
			break
		}

		requests := remote_requests_from_unresolved_candidates(
			result.session.interner,
			last.new_fetch_requests[:],
			context.temp_allocator,
		)
		result.remote_result = remote_deps.resolve_requests(
			requests[:],
			&remote_config,
			&result.remote_state,
			pool,
			allocator,
		)
		external_interfaces := external_interface_inputs_from_remote(
			result.session.interner,
			result.remote_result.interfaces[:],
			context.temp_allocator,
		)
		external_sources := external_source_inputs_from_remote(
			result.remote_result.sources[:],
			context.temp_allocator,
		)
		semantic.semantic_graph_update_result_destroy(&last)
		if len(external_interfaces) == 0 && len(external_sources) == 0 {
			blocked := blocked_keys_from_requests(
				result.session.interner,
				requests[:],
				context.temp_allocator,
			)
			last = semantic.semantic_graph_session_apply_update(
				&result.session,
				semantic.Semantic_Graph_Update {
					external_frontier_stable = true,
					blocked_dependencies = blocked[:],
				},
			)
			frontier_flushed = true
			break
		}
		last = semantic.semantic_graph_session_apply_update(
			&result.session,
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
			&result.session,
			semantic.Semantic_Graph_Update{external_frontier_stable = true},
		)
	}

	result.last_update = last
	workspace_add_dependency_diagnostics(result, workspace)
	return result.ok
}

workspace_add_dependency_diagnostics :: proc(result: ^Analysis_Result, workspace: ^Workspace) {
	if result == nil ||
	   workspace == nil ||
	   !(.Enable_Dependency_Diagnostics in workspace.flags) {
		return
	}
	analysis := semantic.semantic_graph_session_current_analysis(&result.session)
	if analysis == nil {
		return
	}
	for &project_result in analysis.project_results {
		if project_result.project == nil || project_result.checker == nil {
			continue
		}
		for candidate in project_result.checker.info.unresolved {
			kind, message, ok := dependency_diagnostic_from_candidate(
				analysis.interner,
				candidate,
				context.temp_allocator,
			)
			if !ok || dependency_diagnostic_present(
				project_result.checker.info.diagnostics[:],
				kind,
				candidate.range,
				candidate.file,
			) {
				continue
			}
			append(
				&project_result.checker.info.diagnostics,
				semantic.Checker_Diagnostic {
					kind     = kind,
					severity = .Error,
					range    = candidate.range,
					message  = strings.clone(message, project_result.project.allocator),
					file     = candidate.file,
				},
			)
		}
	}
}

dependency_diagnostic_from_candidate :: proc(
	interner: ^string_interner.Interner,
	candidate: semantic.Checker_Unresolved_Candidate,
	allocator: mem.Allocator,
) -> (
	semantic.Checker_Diagnostic_Kind,
	string,
	bool,
) {
	if interner == nil || !string_interner.is_valid(candidate.name) {
		return {}, "", false
	}
	name := string_interner.load(interner, candidate.name)
	if strings.trim_space(name) == "" {
		return {}, "", false
	}
	switch candidate.reason {
	case .Unresolved_Include:
		return .Unresolved_Include, dependency_diagnostic_message("unresolved include ", name, allocator), true
	case .Unresolved_Type:
		return .Unresolved_Type, dependency_diagnostic_message("unresolved external type ", name, allocator), true
	case .Unresolved_Routine:
		return .Unresolved_Reference, dependency_diagnostic_message("unresolved external routine ", name, allocator), true
	case .Unresolved_SQL_Source:
		return .Unresolved_Open_Sql_Source, dependency_diagnostic_message("unresolved Open SQL source ", name, allocator), true
	case .Type_Pool_Import:
		return .Unresolved_Type, dependency_diagnostic_message("unresolved type pool ", name, allocator), true
	case .Unresolved_Reference:
		if candidate.kind == .Global_Symbol && candidate.namespace == .Value {
			return {}, "", false
		}
		return .Unresolved_Reference, dependency_diagnostic_message("unresolved external reference ", name, allocator), true
	}
	return {}, "", false
}

dependency_diagnostic_message :: proc(prefix, name: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, prefix)
	strings.write_string(&out, name)
	return strings.to_string(out)
}

dependency_diagnostic_present :: proc(
	diagnostics: []semantic.Checker_Diagnostic,
	kind: semantic.Checker_Diagnostic_Kind,
	range: semantic.Range,
	file: ^semantic.Project_File,
) -> bool {
	for diagnostic in diagnostics {
		if diagnostic.kind == kind &&
		   diagnostic.range == range &&
		   diagnostic.file == file {
			return true
		}
	}
	return false
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
		config.adt_availability = &workspace.adt_availability
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
