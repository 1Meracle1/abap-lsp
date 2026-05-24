package abap_frontend_workspace

import adt "../adt"
import dep_store "../dependency_store"
import frontend_runtime "../runtime"
import analyze "../semantic/analyze"
import deps "../semantic/dependencies"

import "core:mem"
import "core:os"
import "core:strings"

Options :: struct {
	pool:                  ^frontend_runtime.Pool,
	dependency_store_path: string,
	enable_adt:            bool,
}

Analysis_Result :: struct {
	project:       analyze.Project_Analysis,
	ok:            bool,
	used_manifest: bool,
	error:         string,
}

Workspace :: struct {
	root_path:          string,
	manifest:           Workspace_Manifest,
	has_manifest:       bool,
	store:              dep_store.Dependency_Store,
	has_store:          bool,
	standalone_profile: dep_store.Dependency_Profile,
	local_export_roots: [dynamic]string,
	adt_client:         adt.Client,
	adt_config:         adt.Connection_Config,
	has_adt:            bool,
	has_dotenv:         bool,
}

open_workspace :: proc(
	folder_path: string,
	options: Options,
	allocator: mem.Allocator,
) -> (Workspace, bool, string) {
	root_path, root_ok := absolute_clean_path(folder_path, allocator)
	if !root_ok {
		return {}, false, "invalid workspace path"
	}
	workspace := Workspace{root_path = root_path}

	manifest_path, join_err := os.join_path({root_path, MANIFEST_FILE_NAME}, allocator)
	if join_err != nil {
		return {}, false, "invalid manifest path"
	}
	info, stat_err := os.stat(manifest_path, allocator)
	if stat_err == nil && info.type == .Regular {
		source, read_ok := read_text_file(manifest_path, allocator)
		if !read_ok {
			return {}, false, "failed to read manifest"
		}
		manifest, manifest_ok, manifest_error := parse_workspace_manifest_text(root_path, manifest_path, source, allocator)
		if !manifest_ok {
			return {}, false, manifest_error
		}
		workspace.manifest = manifest
		workspace.has_manifest = true
	} else {
		workspace.manifest = default_workspace_manifest(root_path, allocator)
	}

	if workspace.manifest.has_dependency_store {
		store, err := dep_store.dependency_store_from_override_path(options.dependency_store_path, allocator)
		if err == .None {
			workspace.store = store
			workspace.has_store = true
		}
	} else if !workspace.has_manifest {
		store, err := dep_store.dependency_store_from_override_path(options.dependency_store_path, allocator)
		if err == .None {
			workspace.store = store
			workspace.has_store = true
			workspace.standalone_profile = deps.standalone_dependency_profile()
		}
	}

	workspace.local_export_roots = workspace_local_export_roots(&workspace.manifest, allocator)
	if options.enable_adt {
		init_workspace_adt(&workspace, allocator)
	}
	return workspace, true, ""
}

workspace_destroy :: proc(workspace: ^Workspace, allocator: mem.Allocator) {
	if workspace.has_adt {
		adt.client_destroy(&workspace.adt_client, allocator)
		adt.connection_config_destroy(&workspace.adt_config, allocator)
	}
	workspace^ = {}
}

analyze_filesystem_path :: proc(
	path: string,
	include_paths: []string,
	options: Options,
	allocator: mem.Allocator,
) -> Analysis_Result {
	abs_path, ok := absolute_clean_path(path, allocator)
	if !ok {
		return analysis_error("invalid path")
	}
	info, err := os.stat(abs_path, allocator)
	if err != nil {
		return analysis_error("invalid path")
	}
	if info.type == .Directory {
		return analyze_workspace_path(abs_path, include_paths, options, allocator)
	}
	return analyze_file_path(abs_path, include_paths, options, allocator)
}

analyze_workspace_path :: proc(
	workspace_folder_path: string,
	include_paths: []string,
	options: Options,
	allocator: mem.Allocator,
) -> Analysis_Result {
	assert(options.pool != nil)
	workspace, workspace_ok, workspace_error := open_workspace(workspace_folder_path, options, allocator)
	if !workspace_ok {
		return analysis_error(workspace_error)
	}
	defer workspace_destroy(&workspace, allocator)
	if workspace.has_manifest && len(workspace.manifest.units) > 0 {
		return analyze_manifest_workspace(&workspace, include_paths, options, allocator)
	}
	return analyze_workspace_files(&workspace, include_paths, options, allocator)
}

analyze_file_path :: proc(
	target_path: string,
	include_paths: []string,
	options: Options,
	allocator: mem.Allocator,
) -> Analysis_Result {
	assert(options.pool != nil)
	target_abs, target_ok := absolute_clean_path(target_path, allocator)
	if !target_ok {
		return analysis_error("invalid target path")
	}
	workspace := Workspace{root_path = strings.clone(os.dir(target_abs), allocator)}
	if strings.trim_space(options.dependency_store_path) != "" {
		store, err := dep_store.dependency_store_from_override_path(options.dependency_store_path, allocator)
		if err == .None {
			workspace.store = store
			workspace.has_store = true
			workspace.standalone_profile = deps.standalone_dependency_profile()
		}
	}
	return analyze_standalone_path(&workspace, target_abs, include_paths, options, allocator)
}

analyze_path :: proc(
	target_path: string,
	workspace_folder_path: string,
	include_paths: []string,
	options: Options,
	allocator: mem.Allocator,
) -> Analysis_Result {
	assert(options.pool != nil)
	target_abs, target_ok := absolute_clean_path(target_path, allocator)
	if !target_ok {
		return analysis_error("invalid target path")
	}
	workspace, workspace_ok, workspace_error := open_workspace(workspace_folder_path, options, allocator)
	if !workspace_ok {
		return analysis_error(workspace_error)
	}
	defer workspace_destroy(&workspace, allocator)

	if !workspace.has_manifest {
		return analyze_standalone_path(&workspace, target_abs, include_paths, options, allocator)
	}

	target_key := normalized_uri_path_key(target_abs, allocator)
	root_keys := manifest_root_keys(&workspace.manifest, allocator)
	if selected, ok := manifest_root_unit_by_key(&workspace.manifest, root_keys[:], target_key); ok {
		return analyze_manifest_unit(&workspace, selected, root_keys[:], include_paths, options, allocator)
	}
	if selected, ok := manifest_member_owner_by_key(&workspace.manifest, target_key, allocator); ok {
		return analyze_manifest_unit(&workspace, selected, root_keys[:], include_paths, options, allocator)
	}

	workspace_files := make([dynamic]string, 0, 32, allocator)
	collect_workspace_abap_files(workspace.root_path, &workspace_files, allocator)
	if selected, ok := manifest_reachable_owner_by_key(
		&workspace.manifest,
		target_key,
		workspace_files[:],
		root_keys[:],
		options,
		allocator,
	); ok {
		return analyze_manifest_unit_with_workspace_files(
			&workspace,
			selected,
			root_keys[:],
			workspace_files[:],
			include_paths,
			options,
			allocator,
		)
	}

	return analyze_standalone_path(&workspace, target_abs, include_paths, options, allocator)
}

analyze_manifest_workspace :: proc(
	workspace: ^Workspace,
	include_paths: []string,
	options: Options,
	allocator: mem.Allocator,
) -> Analysis_Result {
	workspace_files := make([dynamic]string, 0, 32, allocator)
	collect_workspace_abap_files(workspace.root_path, &workspace_files, allocator)
	root_keys := manifest_root_keys(&workspace.manifest, allocator)
	targets := make([dynamic]analyze.Source_Input, 0, len(workspace.manifest.units), allocator)
	for unit in workspace.manifest.units {
		if unit.root_file == "" {
			continue
		}
		target, ok := source_input_from_manifest_path(&workspace.manifest, unit.root_file, allocator)
		if !ok {
			return analysis_error("failed to read manifest root file")
		}
		append(&targets, target)
	}
	candidates := manifest_workspace_candidate_inputs(
		&workspace.manifest,
		workspace_files[:],
		root_keys[:],
		include_paths,
		allocator,
	)
	project := deps.analyze_inputs_with_dependency_drain(
		targets[:],
		candidates,
		make([dynamic]analyze.Source_Input, 0, 4, allocator),
		dependency_config_from_workspace(workspace),
		analyze.Analyze_Options{pool = options.pool},
		allocator,
	)
	return Analysis_Result{project = project, ok = true, used_manifest = true}
}

analyze_workspace_files :: proc(
	workspace: ^Workspace,
	include_paths: []string,
	options: Options,
	allocator: mem.Allocator,
) -> Analysis_Result {
	paths := make([dynamic]string, 0, 32, allocator)
	collect_workspace_abap_files(workspace.root_path, &paths, allocator)
	targets := make([dynamic]analyze.Source_Input, 0, len(paths), allocator)
	candidates := make([dynamic]analyze.Project_Candidate_Input, 0, len(paths) + len(include_paths), allocator)
	for path in paths {
		input, ok := source_input_from_path(path, allocator)
		if !ok {
			return analysis_error("failed to read workspace file")
		}
		append(&targets, input)
		append(&candidates, analyze.Project_Candidate_Input{input = input})
	}
	for include_path in include_paths {
		input, ok := source_input_from_path(include_path, allocator)
		if !ok {
			return analysis_error("failed to read include file")
		}
		append(&candidates, analyze.Project_Candidate_Input{input = input})
	}
	project := deps.analyze_inputs_with_dependency_drain(
		targets[:],
		candidates,
		make([dynamic]analyze.Source_Input, 0, 4, allocator),
		dependency_config_from_workspace(workspace),
		analyze.Analyze_Options{pool = options.pool},
		allocator,
	)
	return Analysis_Result{project = project, ok = true}
}

analyze_standalone_path :: proc(
	workspace: ^Workspace,
	target_path: string,
	include_paths: []string,
	options: Options,
	allocator: mem.Allocator,
) -> Analysis_Result {
	target, target_ok := source_input_from_path(target_path, allocator)
	if !target_ok {
		return analysis_error("failed to read target file")
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, 0, len(include_paths), allocator)
	for include_path in include_paths {
		include, include_ok := source_input_from_path(include_path, allocator)
		if !include_ok {
			return analysis_error("failed to read include file")
		}
		append(&candidates, analyze.Project_Candidate_Input{input = include})
	}
	project := deps.analyze_with_dependency_drain(
		target,
		candidates,
		make([dynamic]analyze.Source_Input, 0, 4, allocator),
		dependency_config_from_workspace(workspace),
		analyze.Analyze_Options{pool = options.pool},
		allocator,
	)
	return Analysis_Result{project = project, ok = true}
}

analyze_manifest_unit :: proc(
	workspace: ^Workspace,
	unit_index: int,
	root_keys: []string,
	include_paths: []string,
	options: Options,
	allocator: mem.Allocator,
) -> Analysis_Result {
	workspace_files := make([dynamic]string, 0, 32, allocator)
	collect_workspace_abap_files(workspace.root_path, &workspace_files, allocator)
	return analyze_manifest_unit_with_workspace_files(
		workspace,
		unit_index,
		root_keys,
		workspace_files[:],
		include_paths,
		options,
		allocator,
	)
}

analyze_manifest_unit_with_workspace_files :: proc(
	workspace: ^Workspace,
	unit_index: int,
	root_keys: []string,
	workspace_files: []string,
	include_paths: []string,
	options: Options,
	allocator: mem.Allocator,
) -> Analysis_Result {
	target, target_ok := source_input_from_manifest_path(&workspace.manifest, workspace.manifest.units[unit_index].root_file, allocator)
	if !target_ok {
		return analysis_error("failed to read manifest root file")
	}

	dependency_indices := manifest_dependency_indices(&workspace.manifest, unit_index, allocator)
	dependencies := make([dynamic]analyze.Source_Input, 0, len(dependency_indices), allocator)
	for dependency_index in dependency_indices {
		dependency, dependency_ok := source_input_from_manifest_path(
			&workspace.manifest,
			workspace.manifest.units[dependency_index].root_file,
			allocator,
		)
		if !dependency_ok {
			return analysis_error("failed to read manifest dependency file")
		}
		append(&dependencies, dependency)
	}

	candidates := manifest_candidate_inputs(
		&workspace.manifest,
		unit_index,
		dependency_indices[:],
		workspace_files,
		root_keys,
		include_paths,
		allocator,
	)
	project := deps.analyze_with_dependency_drain(
		target,
		candidates,
		dependencies,
		dependency_config_from_workspace(workspace),
		analyze.Analyze_Options{pool = options.pool},
		allocator,
	)
	return Analysis_Result{project = project, ok = true, used_manifest = true}
}

dependency_config_from_workspace :: proc(workspace: ^Workspace) -> deps.Dependency_Config {
	config := deps.Dependency_Config {
		local_export_roots = workspace.local_export_roots[:],
		store_any_profile  = !workspace.has_manifest,
	}
	if workspace.has_store {
		config.store = &workspace.store
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

manifest_candidate_inputs :: proc(
	manifest: ^Workspace_Manifest,
	unit_index: int,
	dependency_indices: []int,
	workspace_files: []string,
	root_keys: []string,
	include_paths: []string,
	allocator: mem.Allocator,
) -> [dynamic]analyze.Project_Candidate_Input {
	candidates := make([dynamic]analyze.Project_Candidate_Input, 0, len(workspace_files), allocator)
	keys := make([dynamic]string, 0, len(workspace_files), allocator)

	for path in workspace_files {
		add_manifest_candidate_path(&candidates, &keys, path, "", root_keys, allocator)
	}
	add_manifest_member_candidates(manifest, unit_index, &candidates, &keys, root_keys, allocator)
	for dependency_index in dependency_indices {
		add_manifest_member_candidates(manifest, dependency_index, &candidates, &keys, root_keys, allocator)
	}
	for include_path in include_paths {
		abs_path, ok := absolute_clean_path(include_path, allocator)
		if ok {
			add_manifest_candidate_path(&candidates, &keys, abs_path, "", root_keys, allocator)
		}
	}

	return candidates
}

manifest_workspace_candidate_inputs :: proc(
	manifest: ^Workspace_Manifest,
	workspace_files: []string,
	root_keys: []string,
	include_paths: []string,
	allocator: mem.Allocator,
) -> [dynamic]analyze.Project_Candidate_Input {
	candidates := make([dynamic]analyze.Project_Candidate_Input, 0, len(workspace_files), allocator)
	keys := make([dynamic]string, 0, len(workspace_files), allocator)
	for path in workspace_files {
		add_manifest_candidate_path(&candidates, &keys, path, "", root_keys, allocator)
	}
	for unit_index in 0 ..< len(manifest.units) {
		add_manifest_member_candidates(manifest, unit_index, &candidates, &keys, root_keys, allocator)
	}
	for include_path in include_paths {
		abs_path, ok := absolute_clean_path(include_path, allocator)
		if ok {
			add_manifest_candidate_path(&candidates, &keys, abs_path, "", root_keys, allocator)
		}
	}
	return candidates
}

add_manifest_member_candidates :: proc(
	manifest: ^Workspace_Manifest,
	unit_index: int,
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	keys: ^[dynamic]string,
	root_keys: []string,
	allocator: mem.Allocator,
) {
	for member in manifest.units[unit_index].members {
		path, ok := manifest_absolute_path(manifest.root_path, member.file, allocator)
		if ok {
			add_manifest_candidate_path(candidates, keys, path, member.object_name, root_keys, allocator)
		}
	}
}

add_manifest_candidate_path :: proc(
	candidates: ^[dynamic]analyze.Project_Candidate_Input,
	keys: ^[dynamic]string,
	path, object_name: string,
	root_keys: []string,
	allocator: mem.Allocator,
) -> bool {
	key := normalized_uri_path_key(path, allocator)
	if string_list_contains(root_keys, key) {
		return false
	}
	if index := string_list_index(keys^[:], key); index >= 0 {
		if object_name != "" && candidates^[index].object_name == "" {
			candidates^[index].object_name = strings.clone(object_name, allocator)
		}
		return true
	}
	input, ok := source_input_from_path(path, allocator)
	if !ok {
		return false
	}
	append(candidates, analyze.Project_Candidate_Input{input = input, object_name = strings.clone(object_name, allocator)})
	append(keys, key)
	return true
}

manifest_reachable_owner_by_key :: proc(
	manifest: ^Workspace_Manifest,
	target_key: string,
	workspace_files: []string,
	root_keys: []string,
	options: Options,
	allocator: mem.Allocator,
) -> (int, bool) {
	for unit, i in manifest.units {
		if unit.root_file == "" {
			continue
		}
		target, target_ok := source_input_from_manifest_path(manifest, unit.root_file, allocator)
		if !target_ok {
			continue
		}
		candidates := manifest_candidate_inputs(manifest, i, {}, workspace_files, root_keys, {}, allocator)
		project := analyze.analyze_target_with_candidate_inputs(
			target,
			candidates[:],
			{},
			analyze.Analyze_Options{pool = options.pool},
			allocator,
		)
		for analyzed_unit in project.units {
			if normalized_uri_path_key(analyzed_unit.uri, allocator) == target_key {
				return i, true
			}
		}
	}
	return -1, false
}

source_input_from_manifest_path :: proc(
	manifest: ^Workspace_Manifest,
	path: string,
	allocator: mem.Allocator,
) -> (analyze.Source_Input, bool) {
	abs_path, ok := manifest_absolute_path(manifest.root_path, path, allocator)
	if !ok {
		return {}, false
	}
	return source_input_from_path(abs_path, allocator)
}

source_input_from_path :: proc(path: string, allocator: mem.Allocator) -> (analyze.Source_Input, bool) {
	abs_path, ok := absolute_clean_path(path, allocator)
	if !ok {
		return {}, false
	}
	source, source_ok := read_text_file(abs_path, allocator)
	if !source_ok {
		return {}, false
	}
	return analyze.Source_Input{uri = abs_path, source = source}, true
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

default_workspace_manifest :: proc(root_path: string, allocator: mem.Allocator) -> Workspace_Manifest {
	return Workspace_Manifest {
		root_path          = strings.clone(root_path, allocator),
		connection         = "default",
		dependency_source  = "local-first",
		local_export_roots = make([dynamic]string, 0, 2, allocator),
		units              = make([dynamic]Manifest_Unit, 0, 4, allocator),
	}
}

init_workspace_adt :: proc(workspace: ^Workspace, allocator: mem.Allocator) {
	path, err := os.join_path({workspace.root_path, ".env"}, allocator)
	if err != nil {
		return
	}
	info, stat_err := os.stat(path, allocator)
	if stat_err != nil || info.type != .Regular {
		delete(path, allocator)
		return
	}
	workspace.has_dotenv = true
	dotenv, dotenv_err := adt.parse_dotenv_file(path, allocator)
	delete(path, allocator)
	if dotenv_err != .None {
		return
	}
	defer adt.dotenv_defaults_destroy(&dotenv, allocator)

	overrides := adt.Connection_Overrides{}
	config, config_err := adt.connection_config_from_sources(&overrides, &dotenv, allocator)
	if config_err != .None {
		return
	}
	workspace.adt_config = config
	adt.client_init(&workspace.adt_client, workspace.adt_config)
	workspace.has_adt = true
}

analysis_error :: proc(message: string) -> Analysis_Result {
	return Analysis_Result{ok = false, error = message}
}

@(private)
string_list_contains :: proc(values: []string, name: string) -> bool {
	return string_list_index(values, name) >= 0
}

@(private)
string_list_index :: proc(values: []string, name: string) -> int {
	for value, i in values {
		if value == name {
			return i
		}
	}
	return -1
}
