package abap_frontend_semantic_dependencies

import analyze "../analyze"

import dep_store "../../dependency_store"
import toml "../../encoding/toml"

import "core:mem"
import "core:os"
import "core:strings"

MANIFEST_FILE_NAME :: "abapls.toml"

Manifest_Unit_Member :: struct {
	file:        string,
	role:        string,
	object_name: string,
}

Manifest_Unit_Dependency :: struct {
	file: string,
}

Manifest_Unit :: struct {
	name:          string,
	kind:          string,
	root_file:     string,
	members:       [dynamic]Manifest_Unit_Member,
	dependency_of: [dynamic]Manifest_Unit_Dependency,
}

Workspace_Manifest :: struct {
	root_path:            string,
	manifest_path:        string,
	connection:           string,
	dependency_store:     dep_store.Dependency_Profile,
	has_dependency_store: bool,
	local_export_roots:   [dynamic]string,
	dependency_source:    string,
	units:                [dynamic]Manifest_Unit,
}

Manifest_Analysis_Result :: struct {
	project:       analyze.Project_Analysis,
	ok:            bool,
	used_manifest: bool,
	error:         string,
}

parse_workspace_manifest_text :: proc(
	root_path, manifest_path, source: string,
	allocator: mem.Allocator,
) -> (Workspace_Manifest, bool, string) {
	result := toml.parse_string(source, allocator)
	defer toml.destroy_parse_result(result, allocator)
	if len(result.errors) > 0 {
		return {}, false, "invalid TOML manifest"
	}

	manifest := Workspace_Manifest {
		root_path          = strings.clone(root_path, allocator),
		manifest_path      = strings.clone(manifest_path, allocator),
		connection         = "default",
		dependency_source  = "local-first",
		local_export_roots = make([dynamic]string, 0, 2, allocator),
		units              = make([dynamic]Manifest_Unit, 0, 4, allocator),
	}
	if connection, ok := toml.table_get_string(result.root, "connection"); ok {
		trimmed := strings.trim_space(connection)
		if trimmed != "" {
			manifest.connection = strings.to_lower(trimmed, allocator)
		}
	}
	if table, ok := toml.table_get_table(result.root, "dependency_store"); ok {
		if profile, profile_ok := decode_dependency_profile(table, allocator); profile_ok {
			manifest.dependency_store = profile
			manifest.has_dependency_store = true
		}
	}
	if table, ok := toml.table_get_table(result.root, "local_export"); ok {
		if roots, roots_ok := toml.table_get_array(table, "roots"); roots_ok {
			for i in 0 ..< len(roots) {
				if root, root_ok := toml.array_get_string(roots, i); root_ok {
					append(&manifest.local_export_roots, normalize_manifest_path(root, allocator))
				}
			}
		}
	}
	if table, ok := toml.table_get_table(result.root, "dependencies"); ok {
		if dependency_source, source_ok := toml.table_get_string(table, "source"); source_ok {
			manifest.dependency_source = strings.to_lower(strings.trim_space(dependency_source), allocator)
		}
	}

	unit_tables, ok := toml.table_get_array(result.root, "unit")
	if !ok {
		return manifest, true, ""
	}

	for i in 0 ..< len(unit_tables) {
		table, table_ok := toml.array_get_table(unit_tables, i)
		if !table_ok {
			continue
		}
		unit := Manifest_Unit {
			members       = make([dynamic]Manifest_Unit_Member, 0, 4, allocator),
			dependency_of = make([dynamic]Manifest_Unit_Dependency, 0, 1, allocator),
		}
		if name, name_ok := toml.table_get_string(table, "name"); name_ok {
			unit.name = strings.clone(name, allocator)
		}
		if kind, kind_ok := toml.table_get_string(table, "kind"); kind_ok {
			unit.kind = strings.clone(kind, allocator)
		}
		if root_file, root_ok := toml.table_get_string(table, "root_file"); root_ok {
			unit.root_file = normalize_manifest_path(root_file, allocator)
		}
		if members, members_ok := toml.table_get_array(table, "members"); members_ok {
			for member_value in members {
				if member, member_ok := decode_manifest_member(member_value, allocator); member_ok {
					append(&unit.members, member)
				}
			}
		}
		if dependencies, dependencies_ok := toml.table_get_array(table, "dependency_of"); dependencies_ok {
			for dependency_value in dependencies {
				if dependency, dependency_ok := decode_manifest_dependency(dependency_value, allocator); dependency_ok {
					append(&unit.dependency_of, dependency)
				}
			}
		}
		append(&manifest.units, unit)
	}

	return manifest, true, ""
}

decode_dependency_profile :: proc(
	table: toml.Table,
	allocator: mem.Allocator,
) -> (dep_store.Dependency_Profile, bool) {
	product, product_ok := toml.table_get_string(table, "product_version")
	default_version, default_ok := toml.table_get_string(table, "default_package_version")
	product = strings.trim_space(product)
	default_version = strings.trim_space(default_version)
	if !product_ok || !default_ok || product == "" || default_version == "" {
		return {}, false
	}
	packages := make([dynamic]dep_store.Package_Version, 0, 4, allocator)
	if package_table, packages_ok := toml.table_get_table(table, "packages"); packages_ok {
		for package_name, value in package_table.entries {
			#partial switch version in value {
			case toml.String:
				name := strings.trim_space(package_name)
				trimmed_version := strings.trim_space(version)
				if name != "" && trimmed_version != "" {
					append(
						&packages,
						dep_store.Package_Version {
							package_name = strings.clone(name, allocator),
							version      = strings.clone(trimmed_version, allocator),
						},
					)
				}
			}
		}
	}
	return dep_store.Dependency_Profile {
		product_version         = strings.clone(product, allocator),
		default_package_version = strings.clone(default_version, allocator),
		packages                = packages[:],
	}, true
}

analyze_path :: proc(
	target_path: string,
	include_paths: []string,
	options: analyze.Analyze_Options,
	allocator: mem.Allocator,
) -> Manifest_Analysis_Result {
	assert(options.pool != nil)
	target_abs, target_ok := absolute_clean_path(target_path, allocator)
	if !target_ok {
		return manifest_analysis_error("invalid target path")
	}
	root_path, manifest_path, has_manifest := find_nearest_manifest(target_abs, allocator)
	if !has_manifest {
		return analyze_standalone_path(target_abs, include_paths, options, allocator)
	}

	manifest_source, read_manifest_ok := read_text_file(manifest_path, allocator)
	if !read_manifest_ok {
		return manifest_analysis_error("failed to read manifest")
	}
	manifest, manifest_ok, manifest_error := parse_workspace_manifest_text(root_path, manifest_path, manifest_source, allocator)
	if !manifest_ok {
		return manifest_analysis_error(manifest_error)
	}

	target_key := normalized_uri_path_key(target_abs, allocator)
	root_keys := manifest_root_keys(&manifest, allocator)
	if selected, ok := manifest_root_unit_by_key(&manifest, root_keys[:], target_key); ok {
		return analyze_manifest_unit(&manifest, selected, root_keys[:], include_paths, options, allocator)
	}
	if selected, ok := manifest_member_owner_by_key(&manifest, target_key, allocator); ok {
		return analyze_manifest_unit(&manifest, selected, root_keys[:], include_paths, options, allocator)
	}

	workspace_files := make([dynamic]string, 0, 32, allocator)
	collect_workspace_abap_files(root_path, &workspace_files, allocator)
	if selected, ok := manifest_reachable_owner_by_key(
		&manifest,
		target_key,
		workspace_files[:],
		root_keys[:],
		options,
		allocator,
	); ok {
		return analyze_manifest_unit_with_workspace_files(
			&manifest,
			selected,
			root_keys[:],
			workspace_files[:],
			include_paths,
			options,
			allocator,
		)
	}

	return analyze_standalone_path(target_abs, include_paths, options, allocator)
}

decode_manifest_member :: proc(value: toml.Value, allocator: mem.Allocator) -> (Manifest_Unit_Member, bool) {
	#partial switch v in value {
	case toml.String:
		return Manifest_Unit_Member{file = normalize_manifest_path(v, allocator)}, true
	case toml.Table:
		file, ok := toml.table_get_string(v, "file")
		if !ok {
			return {}, false
		}
		member := Manifest_Unit_Member{file = normalize_manifest_path(file, allocator)}
		if role, role_ok := toml.table_get_string(v, "role"); role_ok {
			member.role = strings.clone(role, allocator)
		}
		if object_name, object_ok := toml.table_get_string(v, "object_name"); object_ok {
			member.object_name = strings.clone(object_name, allocator)
		}
		return member, true
	}
	return {}, false
}

decode_manifest_dependency :: proc(
	value: toml.Value,
	allocator: mem.Allocator,
) -> (Manifest_Unit_Dependency, bool) {
	#partial switch v in value {
	case toml.String:
		return Manifest_Unit_Dependency{file = normalize_manifest_path(v, allocator)}, true
	case toml.Table:
		file, ok := toml.table_get_string(v, "file")
		if !ok {
			return {}, false
		}
		return Manifest_Unit_Dependency{file = normalize_manifest_path(file, allocator)}, true
	}
	return {}, false
}

analyze_standalone_path :: proc(
	target_path: string,
	include_paths: []string,
	options: analyze.Analyze_Options,
	allocator: mem.Allocator,
) -> Manifest_Analysis_Result {
	target, target_ok := source_input_from_path(target_path, allocator)
	if !target_ok {
		return manifest_analysis_error("failed to read target file")
	}
	candidates := make([dynamic]analyze.Project_Candidate_Input, 0, len(include_paths), allocator)
	for include_path in include_paths {
		include, include_ok := source_input_from_path(include_path, allocator)
		if !include_ok {
			return manifest_analysis_error("failed to read include file")
		}
		append(&candidates, analyze.Project_Candidate_Input{input = include})
	}
	project := analyze_standalone_with_dependency_drain(target, candidates, options, allocator)
	return Manifest_Analysis_Result{project = project, ok = true}
}

analyze_manifest_unit :: proc(
	manifest: ^Workspace_Manifest,
	unit_index: int,
	root_keys: []string,
	include_paths: []string,
	options: analyze.Analyze_Options,
	allocator: mem.Allocator,
) -> Manifest_Analysis_Result {
	workspace_files := make([dynamic]string, 0, 32, allocator)
	collect_workspace_abap_files(manifest.root_path, &workspace_files, allocator)
	return analyze_manifest_unit_with_workspace_files(
		manifest,
		unit_index,
		root_keys,
		workspace_files[:],
		include_paths,
		options,
		allocator,
	)
}

analyze_manifest_unit_with_workspace_files :: proc(
	manifest: ^Workspace_Manifest,
	unit_index: int,
	root_keys: []string,
	workspace_files: []string,
	include_paths: []string,
	options: analyze.Analyze_Options,
	allocator: mem.Allocator,
) -> Manifest_Analysis_Result {
	target, target_ok := source_input_from_manifest_path(manifest, manifest.units[unit_index].root_file, allocator)
	if !target_ok {
		return manifest_analysis_error("failed to read manifest root file")
	}

	dependency_indices := manifest_dependency_indices(manifest, unit_index, allocator)
	dependencies := make([dynamic]analyze.Source_Input, 0, len(dependency_indices), allocator)
	for dependency_index in dependency_indices {
		dependency, dependency_ok := source_input_from_manifest_path(
			manifest,
			manifest.units[dependency_index].root_file,
			allocator,
		)
		if !dependency_ok {
			return manifest_analysis_error("failed to read manifest dependency file")
		}
		append(&dependencies, dependency)
	}

	candidates := manifest_candidate_inputs(
		manifest,
		unit_index,
		dependency_indices[:],
		workspace_files,
		root_keys,
		include_paths,
		allocator,
	)
	project := analyze_with_manifest_dependency_drain(
		manifest,
		target,
		candidates,
		dependencies,
		options,
		allocator,
	)
	return Manifest_Analysis_Result{project = project, ok = true, used_manifest = true}
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

manifest_root_keys :: proc(manifest: ^Workspace_Manifest, allocator: mem.Allocator) -> [dynamic]string {
	keys := make([dynamic]string, 0, len(manifest.units), allocator)
	for unit in manifest.units {
		if unit.root_file == "" {
			append(&keys, "")
			continue
		}
		path, ok := manifest_absolute_path(manifest.root_path, unit.root_file, allocator)
		append(&keys, normalized_uri_path_key(path, allocator) if ok else "")
	}
	return keys
}

manifest_root_unit_by_key :: proc(
	manifest: ^Workspace_Manifest,
	root_keys: []string,
	target_key: string,
) -> (int, bool) {
	for key, i in root_keys {
		if key != "" && key == target_key && manifest.units[i].root_file != "" {
			return i, true
		}
	}
	return -1, false
}

manifest_member_owner_by_key :: proc(
	manifest: ^Workspace_Manifest,
	target_key: string,
	allocator: mem.Allocator,
) -> (int, bool) {
	for unit, i in manifest.units {
		for member in unit.members {
			path, ok := manifest_absolute_path(manifest.root_path, member.file, allocator)
			if ok && normalized_uri_path_key(path, allocator) == target_key {
				return i, true
			}
		}
	}
	return -1, false
}

manifest_reachable_owner_by_key :: proc(
	manifest: ^Workspace_Manifest,
	target_key: string,
	workspace_files: []string,
	root_keys: []string,
	options: analyze.Analyze_Options,
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
		project := analyze.analyze_target_with_candidate_inputs(target, candidates[:], {}, options, allocator)
		for analyzed_unit in project.units {
			if normalized_uri_path_key(analyzed_unit.uri, allocator) == target_key {
				return i, true
			}
		}
	}
	return -1, false
}

manifest_dependency_indices :: proc(
	manifest: ^Workspace_Manifest,
	unit_index: int,
	allocator: mem.Allocator,
) -> [dynamic]int {
	selected_key := manifest_unit_root_key(manifest, unit_index, allocator)
	indices := make([dynamic]int, 0, 2, allocator)
	if selected_key == "" {
		return indices
	}
	for unit, i in manifest.units {
		if i == unit_index || unit.root_file == "" {
			continue
		}
		for dependency in unit.dependency_of {
			path, ok := manifest_absolute_path(manifest.root_path, dependency.file, allocator)
			if ok && normalized_uri_path_key(path, allocator) == selected_key {
				append(&indices, i)
				break
			}
		}
	}
	return indices
}

manifest_unit_root_key :: proc(
	manifest: ^Workspace_Manifest,
	unit_index: int,
	allocator: mem.Allocator,
) -> string {
	path, ok := manifest_absolute_path(manifest.root_path, manifest.units[unit_index].root_file, allocator)
	return normalized_uri_path_key(path, allocator) if ok else ""
}

collect_workspace_abap_files :: proc(
	root_path: string,
	files: ^[dynamic]string,
	allocator: mem.Allocator,
) {
	entries, err := os.read_all_directory_by_path(root_path, allocator)
	if err != nil {
		return
	}
	defer os.file_info_slice_delete(entries, allocator)
	for entry in entries {
		#partial switch entry.type {
		case .Directory:
			if should_skip_workspace_dir(entry.name) {
				continue
			}
			collect_workspace_abap_files(entry.fullpath, files, allocator)
		case .Regular:
			if is_abap_path(entry.name, allocator) {
				if path, ok := absolute_clean_path(entry.fullpath, allocator); ok {
					append(files, path)
				}
			}
		}
	}
}

find_nearest_manifest :: proc(
	target_abs: string,
	allocator: mem.Allocator,
) -> (string, string, bool) {
	dir := os.dir(target_abs)
	for {
		manifest_path, join_ok := join_path2(dir, MANIFEST_FILE_NAME, allocator)
		if !join_ok {
			return "", "", false
		}
		info, stat_err := os.stat(manifest_path, allocator)
		if stat_err == nil && info.type == .Regular {
			return strings.clone(dir, allocator), manifest_path, true
		}
		parent := os.dir(dir)
		if normalized_uri_path_key(parent, allocator) == normalized_uri_path_key(dir, allocator) {
			break
		}
		dir = parent
	}
	return "", "", false
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

manifest_absolute_path :: proc(
	root_path, manifest_path: string,
	allocator: mem.Allocator,
) -> (string, bool) {
	normalized := normalize_manifest_path(manifest_path, allocator)
	if os.is_absolute_path(normalized) {
		return absolute_clean_path(normalized, allocator)
	}
	joined, ok := join_path2(root_path, normalized, allocator)
	if !ok {
		return "", false
	}
	return absolute_clean_path(joined, allocator)
}

absolute_clean_path :: proc(path: string, allocator: mem.Allocator) -> (string, bool) {
	absolute, abs_err := os.get_absolute_path(path, allocator)
	if abs_err != nil {
		return "", false
	}
	cleaned, clean_err := os.clean_path(absolute, allocator)
	if clean_err != nil {
		return "", false
	}
	return cleaned, true
}

join_path2 :: proc(a, b: string, allocator: mem.Allocator) -> (string, bool) {
	path, err := os.join_path({a, b}, allocator)
	return path, err == nil
}

normalize_manifest_path :: proc(path: string, allocator: mem.Allocator) -> string {
	trimmed := strings.trim_space(path)
	for strings.has_prefix(trimmed, "./") || strings.has_prefix(trimmed, ".\\") {
		trimmed = trimmed[2:]
	}
	out := strings.builder_make(allocator)
	for ch in trimmed {
		if ch == '\\' {
			strings.write_byte(&out, '/')
		} else {
			strings.write_rune(&out, ch)
		}
	}
	return strings.to_string(out)
}

is_abap_path :: proc(path: string, allocator: mem.Allocator) -> bool {
	lower := strings.to_lower(path, allocator)
	return strings.has_suffix(lower, ".abap")
}

should_skip_workspace_dir :: proc(name: string) -> bool {
	return name == "target" || name == ".git" || (len(name) > 0 && name[0] == '.')
}

manifest_analysis_error :: proc(message: string) -> Manifest_Analysis_Result {
	return Manifest_Analysis_Result{ok = false, error = message}
}
