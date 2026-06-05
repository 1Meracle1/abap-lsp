package abap_frontend_workspace

import adt "src:adt"
import dep_store "src:dependency_store"
import execution "src:execution"
import analyze "src:semantic/analyze"
import remote_deps "src:semantic/remote_dependencies"

import "core:mem"
import "core:os"
import "core:strings"

Option_Flag :: enum {
	Enable_ADT,
	Enable_Dependency_Diagnostics,
}
Option_Flags :: bit_set[Option_Flag]

Options :: struct {
	dependency_store_path: string,
	flags:                 Option_Flags,
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
	projects:           [dynamic]^Project_Slot,
	projects_by_object: map[Dependency_Object_Key][dynamic]Project_Id,
}

open_workspace :: proc(
	folder_path: string,
	options: Options,
	allocator: mem.Allocator,
) -> (
	workspace: Workspace,
	ok: bool,
	error: string,
) {
	root_path, root_ok := absolute_clean_path(folder_path, allocator)
	if !root_ok {
		return {}, false, "invalid workspace path"
	}
	workspace.root_path = root_path

	manifest_path, join_err := os.join_path(
		{root_path, MANIFEST_FILE_NAME},
		context.temp_allocator,
	)
	if join_err != nil {
		return {}, false, "invalid manifest path"
	}
	info, stat_err := os.stat(manifest_path, allocator)
	if stat_err == nil && info.type == .Regular {
		source, read_ok := read_text_file(manifest_path, context.temp_allocator)
		if !read_ok {
			return {}, false, "failed to read manifest"
		}
		manifest, manifest_ok, manifest_error := parse_workspace_manifest_text(
			root_path,
			manifest_path,
			source,
			allocator,
		)
		if !manifest_ok {
			return {}, false, manifest_error
		}
		workspace.manifest = manifest
		workspace.has_manifest = true
	} else {
		workspace.manifest = default_workspace_manifest(root_path, allocator)
	}

	if workspace.manifest.has_dependency_store {
		store, err := dep_store.dependency_store_from_override_path(
			options.dependency_store_path,
			allocator,
		)
		if err == .None {
			workspace.store = store
			workspace.has_store = true
		}
	} else if !workspace.has_manifest {
		store, err := dep_store.dependency_store_from_override_path(
			options.dependency_store_path,
			allocator,
		)
		if err == .None {
			workspace.store = store
			workspace.has_store = true
			workspace.standalone_profile = remote_deps.standalone_dependency_profile()
		}
	}

	workspace.local_export_roots = workspace_local_export_roots(&workspace.manifest, allocator)
	if .Enable_ADT in options.flags {
		init_workspace_adt(&workspace, allocator)
	}
	return workspace, true, ""
}

open_standalone_workspace :: proc(
	root_path: string,
	options: Options,
	allocator: mem.Allocator,
) -> (
	workspace: Workspace,
	ok: bool,
	error: string,
) {
	abs_root, root_ok := absolute_clean_path(root_path, allocator)
	if !root_ok {
		return {}, false, "invalid workspace path"
	}
	workspace.root_path = abs_root

	if strings.trim_space(options.dependency_store_path) != "" {
		store, err := dep_store.dependency_store_from_override_path(
			options.dependency_store_path,
			allocator,
		)
		if err == .None {
			workspace.store = store
			workspace.has_store = true
			workspace.standalone_profile = remote_deps.standalone_dependency_profile()
		}
	}
	if .Enable_ADT in options.flags {
		init_workspace_adt(&workspace, allocator)
	}
	return workspace, true, ""
}

workspace_destroy :: proc(workspace: ^Workspace, allocator: mem.Allocator) {
	workspace_snapshot_state_destroy(workspace, allocator)
	if workspace.has_adt {
		adt.client_destroy(&workspace.adt_client, allocator)
		adt.connection_config_destroy(&workspace.adt_config, allocator)
	}
	workspace^ = {}
}

analyze_workspace :: proc(
	workspace: ^Workspace,
	include_paths: []string,
	pool: ^execution.Pool,
	options: Options,
	allocator: mem.Allocator,
) -> Analysis_Result {
	assert(pool != nil)
	result: Analysis_Result
	if workspace.has_manifest && len(workspace.manifest.units) > 0 {
		result = analyze_manifest_workspace(workspace, include_paths, pool, options, allocator)
	} else {
		result = analyze_workspace_files(workspace, include_paths, pool, options, allocator)
	}
	finish_workspace_analysis_result(&result, options)
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
	target_abs, target_ok := absolute_clean_path(target_path, allocator)
	if !target_ok {
		return analysis_error("invalid target path")
	}

	if !workspace.has_manifest {
		result := analyze_standalone_path(workspace, target_abs, include_paths, pool, options, allocator)
		finish_workspace_analysis_result(&result, options)
		return result
	}

	target_key := normalized_uri_path_key(target_abs, allocator)
	root_keys := manifest_root_keys(&workspace.manifest, allocator)
	if selected, ok := manifest_root_unit_by_key(&workspace.manifest, root_keys[:], target_key);
	   ok {
		result := analyze_manifest_unit(
			workspace,
			selected,
			root_keys[:],
			include_paths,
			pool,
			options,
			allocator,
		)
		finish_workspace_analysis_result(&result, options)
		return result
	}
	if selected, ok := manifest_member_owner_by_key(&workspace.manifest, target_key, allocator);
	   ok {
		result := analyze_manifest_unit(
			workspace,
			selected,
			root_keys[:],
			include_paths,
			pool,
			options,
			allocator,
		)
		finish_workspace_analysis_result(&result, options)
		return result
	}

	workspace_files := make([dynamic]string, 0, 32, allocator)
	collect_workspace_abap_files(workspace.root_path, &workspace_files, allocator)
	if selected, ok := manifest_reachable_owner_by_key(
		&workspace.manifest,
		target_key,
		workspace_files[:],
		root_keys[:],
		pool,
		options,
		allocator,
	); ok {
		result := analyze_manifest_unit_with_workspace_files(
			workspace,
			selected,
			root_keys[:],
			workspace_files[:],
			include_paths,
			pool,
			options,
			allocator,
		)
		finish_workspace_analysis_result(&result, options)
		return result
	}

	result := analyze_standalone_path(workspace, target_abs, include_paths, pool, options, allocator)
	finish_workspace_analysis_result(&result, options)
	return result
}

@(private)
finish_workspace_analysis_result :: proc(
	result: ^Analysis_Result,
	options: Options,
) {
	if !result.ok {
		return
	}
	if !(.Enable_Dependency_Diagnostics in options.flags) {
		analyze.filter_dependency_diagnostics(&result.project)
	}
}

dependency_config_from_workspace :: proc(workspace: ^Workspace) -> remote_deps.Dependency_Config {
	config := remote_deps.Dependency_Config {
		local_export_roots = workspace.local_export_roots[:],
		cache_any_profile  = !workspace.has_manifest,
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
