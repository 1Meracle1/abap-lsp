package cache

import "core:log"
import os "core:os/os2"
import "core:path/filepath"
import "core:strconv"
import "core:strings"

Manifest :: struct {
	version:    int,
	connection: string,
	resolution: Resolution_Config,
	units:      [dynamic]Semantic_Unit,
}

Resolution_Config :: struct {
	dependency_mode: string,
	cache_dir:       string,
	unknown_symbol_mode: string,
}

Unit_Kind :: enum {
	Unknown,
	Report,
	Class_Pool,
	Function_Group,
	Interface_Pool,
	Include,
	Global_Class,
	Global_Interface,
}

Semantic_Unit :: struct {
	name:      string,
	kind:      Unit_Kind,
	root_file: string,
	adt_uri:   string,
	members:   [dynamic]Unit_Member,
}

Unit_Member_Role :: enum {
	Unknown,
	Root,
	Include,
	Main,
	Test_Include,
	Function_Module,
	Class_Include,
	Dependency,
}

Unit_Member :: struct {
	role:        Unit_Member_Role,
	file:        string,
	object_name: string,
	adt_uri:     string,
}

Manifest_Section :: enum {
	Top_Level,
	Resolution,
	Unit,
	Unit_Member,
}

manifest_init :: proc() -> ^Manifest {
	manifest := new(Manifest)
	manifest.version = 1
	manifest.resolution.dependency_mode = strings.clone("remote-on-demand")
	manifest.resolution.cache_dir = strings.clone(".abapls/cache")
	manifest.resolution.unknown_symbol_mode = strings.clone("remote")
	manifest.units = make([dynamic]Semantic_Unit)
	return manifest
}

manifest_deinit :: proc(manifest: ^Manifest) {
	if manifest == nil {
		return
	}

	delete(manifest.connection)
	delete(manifest.resolution.dependency_mode)
	delete(manifest.resolution.cache_dir)
	delete(manifest.resolution.unknown_symbol_mode)

	for i in 0 ..< len(manifest.units) {
		unit := &manifest.units[i]
		delete(unit.name)
		delete(unit.root_file)
		delete(unit.adt_uri)
		for j in 0 ..< len(unit.members) {
			member := &unit.members[j]
			delete(member.file)
			delete(member.object_name)
			delete(member.adt_uri)
		}
		delete(unit.members)
	}
	delete(manifest.units)
	free(manifest)
}

workspace_manifest_path :: proc(workspace: ^Workspace, allocator := context.allocator) -> string {
	if workspace == nil || len(workspace.root_path) == 0 {
		return strings.clone("", allocator)
	}
	return filepath.join({workspace.root_path, "abapls.toml"}, allocator)
}

workspace_load_manifest :: proc(workspace: ^Workspace) {
	if workspace.manifest != nil {
		manifest_deinit(workspace.manifest)
		workspace.manifest = nil
	}

	manifest_path := workspace_manifest_path(workspace, context.temp_allocator)
	manifest, ok := manifest_load_from_path(manifest_path)
	if ok {
		workspace.manifest = manifest
		log.infof("loaded manifest for %s with %d semantic units", workspace.root_path, len(manifest.units))
	}
}

manifest_load_from_path :: proc(path: string) -> (^Manifest, bool) {
	if len(path) == 0 {
		return nil, false
	}

	data, err := os.read_entire_file_from_path(path, context.temp_allocator)
	if err != nil {
		return nil, false
	}

	manifest := manifest_parse(string(data), path)
	if manifest == nil {
		return nil, false
	}

	return manifest, true
}

manifest_parse :: proc(text: string, source_path: string = "") -> ^Manifest {
	manifest := manifest_init()
	section := Manifest_Section.Top_Level
	current_unit_index := -1
	line_no := 0
	cursor := 0

	for cursor < len(text) {
		raw_line := manifest_next_line(text, &cursor)
		line_no += 1
		line := strings.trim_space(strip_manifest_comment(raw_line))
		if len(line) == 0 {
			continue
		}

		switch line {
		case "[resolution]":
			section = .Resolution
			continue
		case "[[unit]]":
			section = .Unit
			current_unit_index = manifest_append_unit(manifest)
			continue
		case "[[unit.member]]":
			if current_unit_index < 0 {
				log.warnf("%s:%d unit member declared before unit", source_path, line_no)
				continue
			}
			section = .Unit_Member
			manifest_append_member(manifest, current_unit_index)
			continue
		}

		parts, _ := strings.split_n(line, "=", 2)
		if len(parts) != 2 {
			log.warnf("%s:%d invalid manifest line: %s", source_path, line_no, line)
			continue
		}

		key := strings.trim_space(parts[0])
		value := parse_manifest_value(parts[1])

		switch section {
		case .Top_Level:
			manifest_apply_top_level(manifest, key, value, source_path, line_no)
		case .Resolution:
			manifest_apply_resolution(manifest, key, value)
		case .Unit:
			if current_unit_index >= 0 {
				manifest_apply_unit(manifest, current_unit_index, key, value, source_path, line_no)
			}
		case .Unit_Member:
			if current_unit_index >= 0 {
				manifest_apply_member(manifest, current_unit_index, key, value, source_path, line_no)
			}
		}
	}

	return manifest
}

manifest_next_line :: proc(text: string, cursor: ^int) -> string {
	start := cursor^
	for cursor^ < len(text) && text[cursor^] != '\n' {
		cursor^ += 1
	}
	line := text[start:cursor^]
	if cursor^ < len(text) && text[cursor^] == '\n' {
		cursor^ += 1
	}
	return line
}

strip_manifest_comment :: proc(line: string) -> string {
	in_quotes := false
	for i := 0; i < len(line); i += 1 {
		if line[i] == '"' {
			in_quotes = !in_quotes
			continue
		}
		if !in_quotes && line[i] == '#' {
			return line[:i]
		}
	}
	return line
}

parse_manifest_value :: proc(raw_value: string, allocator := context.allocator) -> string {
	value := strings.trim_space(raw_value)
	if len(value) >= 2 && value[0] == '"' && value[len(value) - 1] == '"' {
		return strings.clone(value[1:len(value) - 1], allocator)
	}
	return strings.clone(value, allocator)
}

manifest_append_unit :: proc(manifest: ^Manifest) -> int {
	append(&manifest.units, Semantic_Unit {members = make([dynamic]Unit_Member)})
	return len(manifest.units) - 1
}

manifest_append_member :: proc(manifest: ^Manifest, unit_index: int) {
	append(&manifest.units[unit_index].members, Unit_Member {})
}

manifest_apply_top_level :: proc(
	manifest: ^Manifest,
	key: string,
	value: string,
	source_path: string,
	line_no: int,
) {
	switch key {
	case "version":
		parsed_version, ok := strconv.parse_int(value, 10)
		if ok {
			manifest.version = parsed_version
		} else {
			log.warnf("%s:%d invalid manifest version: %s", source_path, line_no, value)
		}
	case "connection":
		delete(manifest.connection)
		manifest.connection = value
	case:
		log.warnf("%s:%d unsupported top-level key: %s", source_path, line_no, key)
		delete(value)
	}
}

manifest_apply_resolution :: proc(manifest: ^Manifest, key: string, value: string) {
	switch key {
	case "dependency_mode":
		delete(manifest.resolution.dependency_mode)
		manifest.resolution.dependency_mode = value
	case "cache_dir":
		delete(manifest.resolution.cache_dir)
		manifest.resolution.cache_dir = value
	case "unknown_symbol_mode":
		delete(manifest.resolution.unknown_symbol_mode)
		manifest.resolution.unknown_symbol_mode = value
	case:
		delete(value)
	}
}

manifest_apply_unit :: proc(
	manifest: ^Manifest,
	unit_index: int,
	key: string,
	value: string,
	source_path: string,
	line_no: int,
) {
	unit := &manifest.units[unit_index]
	switch key {
	case "name":
		delete(unit.name)
		unit.name = value
	case "kind":
		unit.kind = parse_unit_kind(value)
		if unit.kind == .Unknown {
			log.warnf("%s:%d unknown unit kind: %s", source_path, line_no, value)
		}
		delete(value)
	case "root_file":
		delete(unit.root_file)
		unit.root_file = value
	case "adt_uri":
		delete(unit.adt_uri)
		unit.adt_uri = value
	case:
		log.warnf("%s:%d unsupported unit key: %s", source_path, line_no, key)
		delete(value)
	}
}

manifest_apply_member :: proc(
	manifest: ^Manifest,
	unit_index: int,
	key: string,
	value: string,
	source_path: string,
	line_no: int,
) {
	if len(manifest.units[unit_index].members) == 0 {
		delete(value)
		return
	}

	member := &manifest.units[unit_index].members[len(manifest.units[unit_index].members) - 1]
	switch key {
	case "role":
		member.role = parse_member_role(value)
		if member.role == .Unknown {
			log.warnf("%s:%d unknown unit member role: %s", source_path, line_no, value)
		}
		delete(value)
	case "file":
		delete(member.file)
		member.file = value
	case "object_name":
		delete(member.object_name)
		member.object_name = value
	case "adt_uri":
		delete(member.adt_uri)
		member.adt_uri = value
	case:
		log.warnf("%s:%d unsupported unit member key: %s", source_path, line_no, key)
		delete(value)
	}
}

parse_unit_kind :: proc(value: string) -> Unit_Kind {
	lowered := strings.to_lower(strings.trim_space(value), context.temp_allocator)
	switch lowered {
	case "report":
		return .Report
	case "class-pool", "class_pool":
		return .Class_Pool
	case "function-group", "function_group":
		return .Function_Group
	case "interface-pool", "interface_pool":
		return .Interface_Pool
	case "include":
		return .Include
	case "global-class", "global_class":
		return .Global_Class
	case "global-interface", "global_interface":
		return .Global_Interface
	}
	return .Unknown
}

parse_member_role :: proc(value: string) -> Unit_Member_Role {
	lowered := strings.to_lower(strings.trim_space(value), context.temp_allocator)
	switch lowered {
	case "root":
		return .Root
	case "include":
		return .Include
	case "main":
		return .Main
	case "test-include", "test_include":
		return .Test_Include
	case "function-module", "function_module":
		return .Function_Module
	case "class-include", "class_include":
		return .Class_Include
	case "dependency":
		return .Dependency
	}
	return .Unknown
}

workspace_relative_path_from_uri :: proc(
	workspace: ^Workspace,
	uri: string,
	allocator := context.allocator,
) -> string {
	if workspace == nil {
		return strings.clone("", allocator)
	}
	path := uri_to_path(uri, context.temp_allocator)
	return workspace_relative_path(workspace, path, allocator)
}

workspace_relative_path :: proc(
	workspace: ^Workspace,
	path: string,
	allocator := context.allocator,
) -> string {
	if workspace == nil || len(workspace.root_path) == 0 {
		return strings.clone("", allocator)
	}

	root_normalized := normalize_manifest_path(workspace.root_path, context.temp_allocator)
	path_normalized := normalize_manifest_path(path, context.temp_allocator)
	if path_normalized == root_normalized {
		return strings.clone("", allocator)
	}

	root_with_sep := root_normalized
	if len(root_with_sep) > 0 && root_with_sep[len(root_with_sep) - 1] != '/' {
		root_with_sep = strings.concatenate({root_with_sep, "/"}, context.temp_allocator)
	}

	if strings.has_prefix(path_normalized, root_with_sep) {
		return strings.clone(path_normalized[len(root_with_sep):], allocator)
	}

	return strings.clone(path_normalized, allocator)
}

workspace_units_for_uri :: proc(
	workspace: ^Workspace,
	uri: string,
	allocator := context.allocator,
) -> []^Semantic_Unit {
	relative_path := workspace_relative_path_from_uri(workspace, uri, context.temp_allocator)
	return workspace_units_for_relative_path(workspace, relative_path, allocator)
}

workspace_units_for_relative_path :: proc(
	workspace: ^Workspace,
	relative_path: string,
	allocator := context.allocator,
) -> []^Semantic_Unit {
	result := make([dynamic]^Semantic_Unit, allocator)
	if workspace == nil || workspace.manifest == nil || len(relative_path) == 0 {
		return result[:]
	}

	normalized_relative := normalize_manifest_path(relative_path, context.temp_allocator)
	for i in 0 ..< len(workspace.manifest.units) {
		unit := &workspace.manifest.units[i]
		if unit_contains_relative_path(unit, normalized_relative) {
			append(&result, unit)
		}
	}

	return result[:]
}

unit_contains_relative_path :: proc(unit: ^Semantic_Unit, relative_path: string) -> bool {
	if unit == nil {
		return false
	}

	if normalize_manifest_path(unit.root_file, context.temp_allocator) == relative_path {
		return true
	}

	for member in unit.members {
		if normalize_manifest_path(member.file, context.temp_allocator) == relative_path {
			return true
		}
	}

	return false
}

unit_has_member_role :: proc(unit: ^Semantic_Unit, role: Unit_Member_Role) -> bool {
	if unit == nil {
		return false
	}

	for member in unit.members {
		if member.role == role {
			return true
		}
	}

	return false
}

unit_is_dependency :: proc(unit: ^Semantic_Unit) -> bool {
	return unit_has_member_role(unit, .Dependency)
}

workspace_dependency_units :: proc(
	workspace: ^Workspace,
	allocator := context.allocator,
) -> []^Semantic_Unit {
	result := make([dynamic]^Semantic_Unit, allocator)
	if workspace == nil || workspace.manifest == nil {
		return result[:]
	}

	for i in 0 ..< len(workspace.manifest.units) {
		unit := &workspace.manifest.units[i]
		if unit_is_dependency(unit) {
			append(&result, unit)
		}
	}

	return result[:]
}

normalize_manifest_path :: proc(path: string, allocator := context.allocator) -> string {
	normalized, _ := strings.replace_all(strings.trim_space(path), "\\", "/", allocator)
	if strings.has_prefix(normalized, "./") {
		return strings.clone(normalized[2:], allocator)
	}
	return strings.clone(normalized, allocator)
}
