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
	if len(workspace.root_path) == 0 {
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
	append(
		&project.diagnostics,
		symbols.Diagnostic {
			range = lexer.TextRange{0, 0},
			message = strings.clone(message, context.allocator),
		},
	)
}

append_project_remote_candidate :: proc(
	project: ^Project,
	name: string,
	kind: symbols.Remote_Candidate_Kind,
) {
	normalized_name := strings.to_lower(strings.trim_space(name), context.temp_allocator)
	if len(normalized_name) == 0 {
		return
	}

	for candidate in project.remote_candidates {
		if candidate.kind == kind && candidate.name == normalized_name {
			return
		}
	}

	append(
		&project.remote_candidates,
		symbols.Remote_Candidate{
			name = strings.clone(normalized_name, context.allocator),
			kind = kind,
		},
	)
}

build_unit_member_uri_map :: proc(
	workspace: ^Workspace,
	unit: ^Semantic_Unit,
	allocator := context.allocator,
) -> map[string]string {
	member_uris := make(map[string]string, allocator)
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

clone_string_uri_map :: proc(
	source: map[string]string,
	allocator := context.allocator,
) -> map[string]string {
	cloned := make(map[string]string, allocator)
	for key, value in source {
		cloned[strings.clone(key, allocator)] = strings.clone(value, allocator)
	}
	return cloned
}

merge_include_uri_map :: proc(target: ^map[string]string, source: map[string]string) {
	for key, value in source {
		if key not_in target^ {
			target^[strings.clone(key, context.temp_allocator)] = strings.clone(value, context.temp_allocator)
		}
	}
}

build_workspace_dependency_include_uri_map :: proc(
	workspace: ^Workspace,
	allocator := context.allocator,
) -> map[string]string {
	include_uris := make(map[string]string, allocator)
	if !workspace_supports_remote_resolution(workspace) {
		return include_uris
	}

	for unit in workspace_dependency_units(workspace, context.temp_allocator) {
		if unit.kind != .Include {
			continue
		}

		include_uri := workspace_uri_for_relative_path(
			workspace,
			unit_root_relative_path(unit, context.temp_allocator),
			allocator,
		)
		if len(include_uri) == 0 {
			continue
		}

		if len(unit.name) > 0 {
			include_uris[strings.to_lower(unit.name, allocator)] = strings.clone(include_uri, allocator)
		}

		for member in unit.members {
			if len(member.object_name) > 0 {
				include_uris[strings.to_lower(member.object_name, allocator)] = strings.clone(include_uri, allocator)
			}
		}

		filename := filename_from_uri(include_uri, allocator)
		if len(filename) > 0 && filename not_in include_uris {
			include_uris[filename] = strings.clone(include_uri, allocator)
		}
	}

	return include_uris
}

project_source_tree_has_syntax_errors :: proc(
	cache: ^Cache,
	workspace: ^Workspace,
	root_uri: string,
	include_uris: map[string]string,
) -> bool {
	active_stack := make([dynamic]string, context.temp_allocator)
	return file_or_includes_have_syntax_errors(cache, workspace, root_uri, include_uris, &active_stack)
}

file_or_includes_have_syntax_errors :: proc(
	cache: ^Cache,
	workspace: ^Workspace,
	uri: string,
	include_uris: map[string]string,
	active_stack: ^[dynamic]string,
) -> bool {
	if cache == nil || workspace == nil || len(uri) == 0 || active_stack == nil {
		return false
	}
	if stack_contains_uri(active_stack^[:], uri) {
		return false
	}

	snapshot := ensure_workspace_document_loaded(cache, workspace, uri)
	if snapshot == nil || snapshot.ast == nil {
		return false
	}
	defer release_snapshot(snapshot)

	if snapshot_has_syntax_errors(snapshot) {
		return true
	}

	append(active_stack, strings.clone(uri, context.temp_allocator))
	defer pop(active_stack)

	for decl in snapshot.ast.decls {
		#partial switch d in decl.derived_stmt {
		case ^ast.Include_Decl:
			if d.name == nil {
				continue
			}
			include_name := strings.to_lower(d.name.name, context.temp_allocator)
			if include_uri, ok := include_uris[include_name]; ok {
				if file_or_includes_have_syntax_errors(
					cache,
					workspace,
					include_uri,
					include_uris,
					active_stack,
				) {
					return true
				}
			}
		}
	}

	return false
}

merge_remote_dependency_symbols_into_table :: proc(
	cache: ^Cache,
	workspace: ^Workspace,
	project: ^Project,
	table: ^symbols.SymbolTable,
) {
	if !workspace_supports_remote_resolution(workspace) {
		return
	}

	for unit in workspace_dependency_units(workspace, context.temp_allocator) {
		if unit.kind == .Include {
			continue
		}

		dependency_relative := unit_root_relative_path(unit, context.temp_allocator)
		if len(dependency_relative) == 0 {
			continue
		}

		dependency_uri := workspace_uri_for_relative_path(workspace, dependency_relative, context.temp_allocator)
		if len(dependency_uri) == 0 || dependency_uri == project.root_uri {
			continue
		}

		dependency_snapshot := ensure_workspace_document_loaded(cache, workspace, dependency_uri)
		if dependency_snapshot == nil || dependency_snapshot.ast == nil {
			append_project_diagnostic(
				project,
				strings.concatenate(
					{"Remote dependency file is missing: ", dependency_relative},
					context.allocator,
				),
			)
			continue
		}
		defer release_snapshot(dependency_snapshot)

		dependency_table := symbols.create_empty_symbol_table(context.allocator)
		symbols.resolve_file_into(dependency_snapshot.ast, dependency_table)
		symbols.merge_symbols_into(table, dependency_table)
	}
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
	if project.resolution_result == nil {
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
	_ = intrinsics.atomic_add(&project.ref_count, 1)
}

release_project :: proc(project: ^Project) {
	old_count := intrinsics.atomic_sub(&project.ref_count, 1)
	if old_count != 1 {
		return
	}

	for snapshot in project.documents {
		release_snapshot(snapshot)
	}
	arena_slot_release(project.arena_slot)
}

project_entry_init :: proc(workspace: ^Workspace, key: string) -> ^Project_Entry {
	entry := new(Project_Entry, workspace.persistent_allocator)
	entry.key = strings.clone(key, workspace.persistent_allocator)
	return entry
}

project_entry_deinit :: proc(entry: ^Project_Entry) {
	release_project(entry.current)
	delete(entry.key)
	free(entry)
}

project_entry_get_snapshot :: proc(entry: ^Project_Entry) -> ^Project {
	if sync.shared_guard(&entry.lock) {
		project := entry.current
		if project != nil {
			retain_project(project)
		}
		return project
	}

	return nil
}

project_entry_publish :: proc(entry: ^Project_Entry, project: ^Project) {
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
	if sync.guard(&workspace.lock) {
		if entry, ok := workspace.projects[key]; ok {
			return entry
		}

		entry := project_entry_init(workspace, key)
		map_key := strings.clone(key, workspace.persistent_allocator)
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
	entry := workspace_get_or_create_project_entry(workspace, key)
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
	slot := arena_slot_acquire(workspace.project_pool)
	context.allocator = slot.allocator

	project := new(Project, slot.allocator)
	project.ref_count = 1
	project.arena_slot = slot
	project.key = strings.clone(key)
	project.root_uri = strings.clone(root_uri)
	project.unit_name = strings.clone(unit_name)
	project.member_uris = make([dynamic]string, slot.allocator)
	project.diagnostics = make([dynamic]symbols.Diagnostic, slot.allocator)
	project.remote_candidates = make([dynamic]symbols.Remote_Candidate, slot.allocator)
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

	project_has_local_syntax_errors := project_source_tree_has_syntax_errors(
		cache,
		workspace,
		root_uri,
		include_uris,
	)
	current_table := symbols.create_empty_symbol_table(slot.allocator)
	active_stack := make([dynamic]string, context.temp_allocator)
	effective_include_uris := clone_string_uri_map(include_uris, context.temp_allocator)
	if !project_has_local_syntax_errors {
		merge_remote_dependency_symbols_into_table(cache, workspace, project, current_table)
		merge_include_uri_map(
			&effective_include_uris,
			build_workspace_dependency_include_uri_map(workspace, context.temp_allocator),
		)
	}

	resolve_project_file(
		cache,
		workspace,
		root_snapshot,
		effective_include_uris,
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
	if stack_contains_uri(active_stack^[:], snapshot.uri) {
		append_project_diagnostic(
			project,
			strings.concatenate({"Cyclic INCLUDE detected for ", snapshot.uri}, context.allocator),
		)
		return
	}

	append(active_stack, strings.clone(snapshot.uri, context.temp_allocator))
	defer pop(active_stack)
	syntax_taint := symbols.build_syntax_taint_ranges(snapshot.ast, context.temp_allocator)

	for decl in snapshot.ast.decls {
		if symbols.statement_is_syntax_tainted(decl, syntax_taint) {
			continue
		}
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
					append_project_remote_candidate(project, include_name, .Include)
					append_project_diagnostic(
						project,
						strings.concatenate(
							{"Local INCLUDE file is missing: ", include_name},
							context.allocator,
						),
					)
				}
			} else {
				append_project_remote_candidate(project, include_name, .Include)
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
			symbols.resolve_decl_into_with_syntax_taint(table, decl, syntax_taint)
		}
	}
}
