package cache

import "../lang/ast"
import "../lang/lexer"
import "../lang/symbols"

import "base:intrinsics"
import os "core:os/os2"
import "core:path/filepath"
import "core:strings"
import "core:sync"

workspace_uri_for_relative_path :: proc(
	workspace: ^Workspace,
	relative_path: string,
	allocator := context.allocator,
) -> string {
	if workspace == nil || len(workspace.root_path) == 0 {
		return strings.clone("", allocator)
	}

	normalized := normalize_manifest_path(relative_path, context.temp_allocator)
	full_path := filepath.join({workspace.root_path, normalized}, context.temp_allocator)
	return path_to_uri(full_path, allocator)
}

ensure_workspace_document_loaded :: proc(
	cache: ^Cache,
	workspace: ^Workspace,
	uri: string,
) -> ^Snapshot {
	if workspace == nil || len(uri) == 0 {
		return nil
	}

	if snapshot := get_snapshot(cache, uri); snapshot != nil {
		return snapshot
	}

	path := uri_to_path(uri, context.temp_allocator)
	data, err := os.read_entire_file_from_path(path, context.temp_allocator)
	if err != nil {
		return nil
	}

	refresh_document_internal(cache, uri, string(data), 0, false)
	return get_snapshot(cache, uri)
}

unit_root_relative_path :: proc(unit: ^Semantic_Unit, allocator := context.allocator) -> string {
	if unit == nil {
		return strings.clone("", allocator)
	}

	if len(unit.root_file) > 0 {
		return normalize_manifest_path(unit.root_file, allocator)
	}

	for member in unit.members {
		#partial switch member.role {
		case .Root, .Main:
			if len(member.file) > 0 {
				return normalize_manifest_path(member.file, allocator)
			}
		}
	}

	if len(unit.members) > 0 && len(unit.members[0].file) > 0 {
		return normalize_manifest_path(unit.members[0].file, allocator)
	}

	return strings.clone("", allocator)
}

append_project_diagnostic :: proc(project: ^Project, message: string) {
	if project == nil {
		return
	}
	append(
		&project.diagnostics,
		symbols.Diagnostic {
			range = lexer.TextRange{0, 0},
			message = strings.clone(message, context.allocator),
		},
	)
}

build_unit_member_uri_map :: proc(
	workspace: ^Workspace,
	unit: ^Semantic_Unit,
	allocator := context.allocator,
) -> map[string]string {
	member_uris := make(map[string]string, allocator)
	if workspace == nil || unit == nil {
		return member_uris
	}

	root_relative := unit_root_relative_path(unit, context.temp_allocator)
	if len(root_relative) > 0 {
		root_uri := workspace_uri_for_relative_path(workspace, root_relative, allocator)
		root_name := filename_from_uri(root_uri, context.temp_allocator)
		if len(root_name) > 0 {
			member_uris[root_name] = root_uri
		}
	}

	for member in unit.members {
		if len(member.file) == 0 {
			continue
		}

		member_uri := workspace_uri_for_relative_path(workspace, member.file, allocator)
		if len(member.object_name) > 0 {
			member_uris[strings.to_lower(member.object_name, context.temp_allocator)] = member_uri
		}

		filename := filename_from_uri(member_uri, context.temp_allocator)
		if len(filename) > 0 && filename not_in member_uris {
			member_uris[filename] = member_uri
		}
	}

	return member_uris
}

build_folder_include_uri_map :: proc(
	uri: string,
	allocator := context.allocator,
) -> map[string]string {
	include_uris := make(map[string]string, allocator)
	folder_path := folder_from_uri(uri, context.temp_allocator)
	files := list_abap_files(folder_path, context.temp_allocator)
	for file_path in files {
		file_uri := path_to_uri(file_path, allocator)
		filename := filename_from_uri(file_uri, context.temp_allocator)
		if len(filename) > 0 {
			include_uris[filename] = file_uri
		}
	}
	return include_uris
}

get_projects_for_uri :: proc(
	cache: ^Cache,
	uri: string,
	allocator := context.allocator,
) -> []^Project {
	result := make([dynamic]^Project, allocator)
	workspace := workspace_for_uri(cache, uri)
	if workspace == nil {
		return result[:]
	}

	units := workspace_units_for_uri(workspace, uri, context.temp_allocator)
	for unit in units {
		key := manifest_project_key(workspace, unit, context.temp_allocator)
		project := get_or_build_manifest_project(cache, workspace, unit, key)
		if project != nil {
			append(&result, project)
		}
	}

	if len(result) == 0 {
		key := local_project_key(uri, context.temp_allocator)
		project := get_or_build_local_project(cache, workspace, uri, key)
		if project != nil {
			append(&result, project)
		}
	}

	return result[:]
}

get_file_symbol_table :: proc(project: ^Project, uri: string) -> ^symbols.SymbolTable {
	if project == nil || project.resolution_result == nil {
		return nil
	}

	if table, ok := project.resolution_result.file_tables[uri]; ok {
		return table
	}

	return project.resolution_result.merged_table
}

merge_symbol_tables_for_lookup :: proc(
	target: ^symbols.SymbolTable,
	source: ^symbols.SymbolTable,
) {
	if target == nil || source == nil {
		return
	}

	symbols.merge_symbols_into(target, source)
	for diag in source.diagnostics {
		append(&target.diagnostics, diag)
	}
}

stack_contains_uri :: proc(stack: []string, uri: string) -> bool {
	for entry in stack {
		if entry == uri {
			return true
		}
	}
	return false
}

retain_project :: proc(project: ^Project) {
	if project != nil {
		_ = intrinsics.atomic_add(&project.ref_count, 1)
	}
}

release_project :: proc(project: ^Project) {
	if project == nil {
		return
	}

	old_count := intrinsics.atomic_sub(&project.ref_count, 1)
	if old_count != 1 {
		return
	}

	for snapshot in project.documents {
		release_snapshot(snapshot)
	}
	arena_slot_release(project.arena_slot)
}

project_entry_init :: proc(key: string) -> ^Project_Entry {
	entry := new(Project_Entry)
	entry.key = strings.clone(key)
	return entry
}

project_entry_deinit :: proc(entry: ^Project_Entry) {
	if entry == nil {
		return
	}

	release_project(entry.current)
	delete(entry.key)
	free(entry)
}

project_entry_get_snapshot :: proc(entry: ^Project_Entry) -> ^Project {
	if entry == nil {
		return nil
	}

	if sync.shared_guard(&entry.lock) {
		project := entry.current
		retain_project(project)
		return project
	}

	return nil
}

project_entry_publish :: proc(entry: ^Project_Entry, project: ^Project) {
	if entry == nil {
		release_project(project)
		return
	}

	old_project: ^Project
	sync.rw_mutex_lock(&entry.lock)
	old_project = entry.current
	entry.current = project
	sync.rw_mutex_unlock(&entry.lock)

	release_project(old_project)
}

workspace_get_or_create_project_entry :: proc(
	workspace: ^Workspace,
	key: string,
) -> ^Project_Entry {
	if workspace == nil {
		return nil
	}

	if sync.guard(&workspace.lock) {
		if entry, ok := workspace.projects[key]; ok {
			return entry
		}

		entry := project_entry_init(key)
		map_key := strings.clone(key)
		workspace.projects[map_key] = entry
		return entry
	}

	return nil
}

manifest_project_key :: proc(
	workspace: ^Workspace,
	unit: ^Semantic_Unit,
	allocator := context.allocator,
) -> string {
	root_relative := unit_root_relative_path(unit, context.temp_allocator)
	if len(unit.name) > 0 {
		return strings.concatenate(
			{"manifest:", strings.to_lower(unit.name, context.temp_allocator), ":", root_relative},
			allocator,
		)
	}
	return strings.concatenate({"manifest:", root_relative}, allocator)
}

local_project_key :: proc(uri: string, allocator := context.allocator) -> string {
	return strings.concatenate({"local:", uri}, allocator)
}

workspace_invalidate_projects_for_uri :: proc(workspace: ^Workspace, uri: string) {
	if workspace == nil {
		return
	}

	keys_to_remove := make([dynamic]string, context.temp_allocator)

	if workspace.manifest != nil {
		for unit in workspace_units_for_uri(workspace, uri, context.temp_allocator) {
			append(&keys_to_remove, manifest_project_key(workspace, unit, context.temp_allocator))
		}
	}

	if sync.guard(&workspace.lock) {
		for key, entry in workspace.projects {
			if strings.has_prefix(key, "local:") {
				append(&keys_to_remove, strings.clone(key, context.temp_allocator))
				continue
			}
			if project_entry_matches_uri(entry, uri) {
				append(&keys_to_remove, strings.clone(key, context.temp_allocator))
			}
		}

		for key in keys_to_remove {
			if entry, ok := workspace.projects[key]; ok {
				delete_key(&workspace.projects, key)
				project_entry_deinit(entry)
			}
		}
	}
}

project_entry_matches_uri :: proc(entry: ^Project_Entry, uri: string) -> bool {
	if entry == nil {
		return false
	}

	if sync.shared_guard(&entry.lock) {
		project := entry.current
		if project == nil {
			return false
		}
		for member_uri in project.member_uris {
			if member_uri == uri {
				return true
			}
		}
		return project.root_uri == uri
	}

	return false
}

get_or_build_manifest_project :: proc(
	cache: ^Cache,
	workspace: ^Workspace,
	unit: ^Semantic_Unit,
	key: string,
) -> ^Project {
	if workspace == nil || unit == nil {
		return nil
	}

	entry := workspace_get_or_create_project_entry(workspace, key)
	if entry == nil {
		return nil
	}

	if project := project_entry_get_snapshot(entry); project != nil {
		return project
	}

	project := build_manifest_project(cache, workspace, unit, key)
	if project == nil {
		return nil
	}

	sync.rw_mutex_lock(&entry.lock)
	if entry.current != nil {
		existing := entry.current
		retain_project(existing)
		sync.rw_mutex_unlock(&entry.lock)
		release_project(project)
		return existing
	}
	entry.current = project
	retain_project(project)
	sync.rw_mutex_unlock(&entry.lock)
	return project
}

get_or_build_local_project :: proc(
	cache: ^Cache,
	workspace: ^Workspace,
	uri: string,
	key: string,
) -> ^Project {
	if workspace == nil || len(uri) == 0 {
		return nil
	}

	entry := workspace_get_or_create_project_entry(workspace, key)
	if entry == nil {
		return nil
	}

	if project := project_entry_get_snapshot(entry); project != nil {
		return project
	}

	project := build_local_project(
		cache,
		workspace,
		uri,
		build_folder_include_uri_map(uri, context.temp_allocator),
		filename_from_uri(uri),
		key,
	)
	if project == nil {
		return nil
	}

	sync.rw_mutex_lock(&entry.lock)
	if entry.current != nil {
		existing := entry.current
		retain_project(existing)
		sync.rw_mutex_unlock(&entry.lock)
		release_project(project)
		return existing
	}
	entry.current = project
	retain_project(project)
	sync.rw_mutex_unlock(&entry.lock)
	return project
}

build_manifest_project :: proc(
	cache: ^Cache,
	workspace: ^Workspace,
	unit: ^Semantic_Unit,
	key: string,
) -> ^Project {
	root_relative := unit_root_relative_path(unit, context.temp_allocator)
	if len(root_relative) == 0 {
		return nil
	}

	root_uri := workspace_uri_for_relative_path(workspace, root_relative, context.temp_allocator)
	return build_local_project(
		cache,
		workspace,
		root_uri,
		build_unit_member_uri_map(workspace, unit, context.temp_allocator),
		unit.name,
		key,
	)
}

build_local_project :: proc(
	cache: ^Cache,
	workspace: ^Workspace,
	root_uri: string,
	include_uris: map[string]string,
	unit_name: string,
	key: string,
) -> ^Project {
	if workspace == nil || len(root_uri) == 0 {
		return nil
	}

	slot := arena_slot_acquire(workspace.project_pool)
	if slot == nil {
		return nil
	}

	old_allocator := context.allocator
	context.allocator = slot.allocator
	defer context.allocator = old_allocator

	project := new(Project, slot.allocator)
	project.ref_count = 1
	project.arena_slot = slot
	project.key = strings.clone(key)
	project.root_uri = strings.clone(root_uri)
	project.unit_name = strings.clone(unit_name)
	project.member_uris = make([dynamic]string, slot.allocator)
	project.diagnostics = make([dynamic]symbols.Diagnostic, slot.allocator)
	project.documents = make([dynamic]^Snapshot, slot.allocator)
	project.resolution_result = new(symbols.ProjectResolutionResult, slot.allocator)
	project.resolution_result.file_tables = make(map[string]^symbols.SymbolTable, slot.allocator)

	root_snapshot := ensure_workspace_document_loaded(cache, workspace, root_uri)
	if root_snapshot == nil || root_snapshot.ast == nil {
		append_project_diagnostic(
			project,
			strings.concatenate({"Unable to load root file ", root_uri}, slot.allocator),
		)
		return project
	}
	project_add_snapshot(project, root_snapshot)

	current_table := symbols.create_empty_symbol_table(slot.allocator)
	active_stack := make([dynamic]string, context.temp_allocator)

	resolve_project_file(
		cache,
		workspace,
		root_snapshot,
		include_uris,
		current_table,
		project.resolution_result,
		project,
		&active_stack,
	)

	project.resolution_result.file_tables[project.root_uri] = current_table
	project.resolution_result.merged_table = current_table
	symbols.validate_file(root_snapshot.ast, current_table)

	return project
}

project_add_snapshot :: proc(project: ^Project, snapshot: ^Snapshot) {
	if project == nil || snapshot == nil {
		return
	}

	append(&project.documents, snapshot)
	append(&project.member_uris, strings.clone(snapshot.uri, context.allocator))
}

resolve_project_file :: proc(
	cache: ^Cache,
	workspace: ^Workspace,
	snapshot: ^Snapshot,
	include_uris: map[string]string,
	table: ^symbols.SymbolTable,
	result: ^symbols.ProjectResolutionResult,
	project: ^Project,
	active_stack: ^[dynamic]string,
) {
	if snapshot == nil || snapshot.ast == nil || table == nil || result == nil {
		return
	}

	if stack_contains_uri(active_stack^[:], snapshot.uri) {
		append_project_diagnostic(
			project,
			strings.concatenate({"Cyclic INCLUDE detected for ", snapshot.uri}, context.allocator),
		)
		return
	}

	append(active_stack, strings.clone(snapshot.uri, context.temp_allocator))
	defer pop(active_stack)

	for decl in snapshot.ast.decls {
		#partial switch d in decl.derived_stmt {
		case ^ast.Include_Decl:
			if d.name == nil {
				continue
			}

			include_name := strings.to_lower(d.name.name, context.temp_allocator)
			if include_uri, ok := include_uris[include_name]; ok {
				include_snapshot := ensure_workspace_document_loaded(cache, workspace, include_uri)
				if include_snapshot != nil && include_snapshot.ast != nil {
					project_add_snapshot(project, include_snapshot)
					include_table := symbols.clone_symbol_table(table, context.allocator)
					resolve_project_file(
						cache,
						workspace,
						include_snapshot,
						include_uris,
						include_table,
						result,
						project,
						active_stack,
					)
					result.file_tables[include_uri] = include_table
					symbols.validate_file_with_lookup(include_snapshot.ast, table, include_table)
					symbols.merge_symbols_into(table, include_table)
				} else {
					append_project_diagnostic(
						project,
						strings.concatenate(
							{"Local INCLUDE file is missing: ", include_name},
							context.allocator,
						),
					)
				}
			} else {
				append_project_diagnostic(
					project,
					strings.concatenate(
						{"INCLUDE target not available locally: ", include_name},
						context.allocator,
					),
				)
			}

			symbols.resolve_include_decl(table, d)
		case:
			symbols.resolve_decl_into(table, decl)
		}
	}
}
