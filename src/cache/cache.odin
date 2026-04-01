package cache

import "../lang/ast"
import "../lang/symbols"

import "core:mem"
import "core:strings"
import "core:sync"

Document_Entry :: struct {
	lock:      sync.RW_Mutex,
	workspace: ^Workspace,
	uri:       string,
	path:      string,
	current:   ^Snapshot,
}

Project_Entry :: struct {
	lock:    sync.RW_Mutex,
	key:     string,
	current: ^Project,
}

Snapshot :: struct {
	ref_count:   i32,
	arena_slot:  ^Arena_Slot,
	uri:         string,
	path:        string,
	text:        string,
	version:     int,
	ast:         ^ast.File,
	symbol_table: ^symbols.SymbolTable,
}

Project :: struct {
	ref_count:         i32,
	arena_slot:        ^Arena_Slot,
	key:               string,
	unit_name:         string,
	root_uri:          string,
	member_uris:       [dynamic]string,
	diagnostics:       [dynamic]symbols.Diagnostic,
	remote_candidates: [dynamic]symbols.Remote_Candidate,
	resolution_result: ^symbols.ProjectResolutionResult,
	documents:         [dynamic]^Snapshot,
}

Workspace :: struct {
	lock:                     sync.RW_Mutex,
	persistent_allocator:     mem.Allocator,
	uri:                      string,
	name:                     string,
	root_path:                string,
	manifest:                 ^Manifest,
	documents:                map[string]^Document_Entry,
	projects:                 map[string]^Project_Entry,
	remote_resolution_seen:   map[string]bool,
	doc_pool:                 ^Arena_Pool,
	project_pool:             ^Arena_Pool,
}

Cache :: struct {
	lock:       sync.RW_Mutex,
	workspaces: [dynamic]^Workspace,
}

cache_init :: proc() -> ^Cache {
	cache := new(Cache)
	cache.workspaces = make([dynamic]^Workspace)
	return cache
}

cache_deinit :: proc(cache: ^Cache) {
	if cache == nil {
		return
	}

	if sync.guard(&cache.lock) {
		for workspace in cache.workspaces {
			workspace_deinit(workspace)
		}
		delete(cache.workspaces)
	}

	free(cache)
}

workspace_init :: proc(uri: string, name: string) -> ^Workspace {
	workspace := new(Workspace)
	workspace.persistent_allocator = context.allocator
	workspace.uri = strings.clone(uri)
	workspace.name = strings.clone(name)
	workspace.root_path = uri_to_path(uri)
	workspace.documents = make(map[string]^Document_Entry)
	workspace.projects = make(map[string]^Project_Entry)
	workspace.remote_resolution_seen = make(map[string]bool)
	workspace.doc_pool = arena_pool_init(8)
	workspace.project_pool = arena_pool_init(4)
	workspace_load_manifest(workspace)
	return workspace
}

workspace_deinit :: proc(workspace: ^Workspace) {
	if workspace == nil {
		return
	}

	if sync.guard(&workspace.lock) {
		for key, project_entry in workspace.projects {
			delete(key)
			project_entry_deinit(project_entry)
		}
		for key, document in workspace.documents {
			delete(key)
			document_entry_deinit(document)
		}
		delete(workspace.projects)
		delete(workspace.documents)
		delete(workspace.remote_resolution_seen)
	}

	if workspace.manifest != nil {
		manifest_deinit(workspace.manifest)
	}
	arena_pool_deinit(workspace.project_pool)
	arena_pool_deinit(workspace.doc_pool)
	delete(workspace.uri)
	delete(workspace.name)
	delete(workspace.root_path)
	free(workspace)
}

cache_add_workspace :: proc(cache: ^Cache, uri: string, name: string) -> ^Workspace {
	if cache == nil {
		return nil
	}

	workspace := workspace_init(uri, name)
	if sync.guard(&cache.lock) {
		append(&cache.workspaces, workspace)
	}
	return workspace
}

workspace_for_uri :: proc(cache: ^Cache, uri: string) -> ^Workspace {
	if cache == nil {
		return nil
	}

	best_match: ^Workspace
	best_match_len := -1

	if sync.shared_guard(&cache.lock) {
		for workspace in cache.workspaces {
			if len(workspace.uri) == 0 {
				if best_match == nil {
					best_match = workspace
				}
				continue
			}
			if strings.has_prefix(uri, workspace.uri) && len(workspace.uri) > best_match_len {
				best_match = workspace
				best_match_len = len(workspace.uri)
			}
		}
	}

	return best_match
}

workspace_supports_remote_resolution :: proc(workspace: ^Workspace) -> bool {
	if workspace == nil || workspace.manifest == nil {
		return false
	}

	if len(strings.trim_space(workspace.manifest.connection)) == 0 {
		return false
	}

	return strings.to_lower(workspace.manifest.resolution.dependency_mode, context.temp_allocator) ==
		"remote-on-demand"
}

workspace_unknown_symbol_mode :: proc(workspace: ^Workspace, allocator := context.allocator) -> string {
	if workspace == nil || workspace.manifest == nil {
		return strings.clone("remote", allocator)
	}

	mode := strings.to_lower(
		strings.trim_space(workspace.manifest.resolution.unknown_symbol_mode),
		context.temp_allocator,
	)
	switch mode {
	case "log":
		return strings.clone("log", allocator)
	case:
		return strings.clone("remote", allocator)
	}
}

workspace_uri_is_remote_dependency :: proc(workspace: ^Workspace, uri: string) -> bool {
	if workspace == nil || len(uri) == 0 {
		return false
	}

	for unit in workspace_units_for_uri(workspace, uri, context.temp_allocator) {
		if unit_is_dependency(unit) {
			return true
		}
	}

	return false
}

workspace_open_document_uris :: proc(
	workspace: ^Workspace,
	allocator := context.allocator,
) -> []string {
	result := make([dynamic]string, allocator)
	if workspace == nil {
		return result[:]
	}

	if sync.shared_guard(&workspace.lock) {
		for uri in workspace.documents {
			append(&result, strings.clone(uri, allocator))
		}
	}

	return result[:]
}

snapshot_has_syntax_errors :: proc(snapshot: ^Snapshot) -> bool {
	return snapshot != nil && snapshot.ast != nil && len(snapshot.ast.syntax_errors) > 0
}

project_has_syntax_errors :: proc(project: ^Project) -> bool {
	if project == nil {
		return false
	}

	for snapshot in project.documents {
		if snapshot_has_syntax_errors(snapshot) {
			return true
		}
	}

	return false
}

workspace_invalidate_all_projects :: proc(workspace: ^Workspace) {
	if workspace == nil {
		return
	}

	keys_to_remove := make([dynamic]string, context.temp_allocator)
	if sync.guard(&workspace.lock) {
		for key in workspace.projects {
			append(&keys_to_remove, strings.clone(key, context.temp_allocator))
		}
		for key in keys_to_remove {
			if entry, ok := workspace.projects[key]; ok {
				delete_key(&workspace.projects, key)
				project_entry_deinit(entry)
			}
		}
	}
}

workspace_should_request_remote_candidate :: proc(
	workspace: ^Workspace,
	candidate: symbols.Remote_Candidate,
) -> bool {
	if workspace == nil {
		return false
	}

	request_key := remote_candidate_request_key(candidate)
	if len(request_key) == 0 {
		return false
	}

	if sync.guard(&workspace.lock) {
		if request_key in workspace.remote_resolution_seen {
			return false
		}
		workspace.remote_resolution_seen[strings.clone(request_key, workspace.persistent_allocator)] = true
	}

	return true
}

remote_candidate_request_key :: proc(candidate: symbols.Remote_Candidate) -> string {
	if len(candidate.name) == 0 {
		return ""
	}

	return strings.concatenate(
		{
			remote_candidate_kind_label(candidate.kind),
			":",
			strings.to_lower(candidate.name, context.temp_allocator),
		},
		context.temp_allocator,
	)
}

remote_candidate_kind_label :: proc(kind: symbols.Remote_Candidate_Kind) -> string {
	switch kind {
	case .Unknown_Symbol:
		return "symbol"
	case .Type_Name:
		return "type"
	case .Static_Target:
		return "static"
	case .Include:
		return "include"
	}
	return "unknown"
}

get_snapshot :: proc(cache: ^Cache, uri: string) -> ^Snapshot {
	workspace := workspace_for_uri(cache, uri)
	if workspace == nil {
		return nil
	}

	entry := workspace_get_document_entry(workspace, uri)
	if entry == nil {
		return nil
	}

	if sync.shared_guard(&entry.lock) {
		snapshot := entry.current
		retain_snapshot(snapshot)
		return snapshot
	}

	return nil
}

refresh_document :: proc(cache: ^Cache, uri: string, text: string, version: int) {
	refresh_document_internal(cache, uri, text, version, true)
}

refresh_document_internal :: proc(
	cache: ^Cache,
	uri: string,
	text: string,
	version: int,
	invalidate_projects: bool,
) {
	workspace := workspace_for_uri(cache, uri)
	if workspace == nil {
		return
	}

	path := uri_to_path(uri, context.temp_allocator)
	entry := workspace_get_or_create_document_entry(workspace, uri, path)
	if entry == nil {
		return
	}

	document_entry_publish(entry, text, version)
	if invalidate_projects {
		workspace_invalidate_projects_for_uri(workspace, uri)
	}
}

get_effective_symbol_table :: proc(cache: ^Cache, uri: string) -> ^symbols.SymbolTable {
	projects := get_projects_for_uri(cache, uri, context.temp_allocator)
	defer release_projects(projects)

	if len(projects) == 0 {
		return nil
	}

	base_table := get_file_symbol_table(projects[0], uri)
	if base_table == nil {
		return symbols.create_empty_symbol_table(context.temp_allocator)
	}

	merged_table := symbols.clone_symbol_table(base_table, context.temp_allocator)
	for project in projects[1:] {
		merge_symbol_tables_for_lookup(merged_table, get_file_symbol_table(project, uri))
	}
	return merged_table
}

release_projects :: proc(projects: []^Project) {
	for project in projects {
		release_project(project)
	}
}

workspace_get_document_entry :: proc(workspace: ^Workspace, uri: string) -> ^Document_Entry {
	if workspace == nil {
		return nil
	}

	if sync.shared_guard(&workspace.lock) {
		if entry, ok := workspace.documents[uri]; ok {
			return entry
		}
	}

	return nil
}

workspace_get_or_create_document_entry :: proc(
	workspace: ^Workspace,
	uri: string,
	path: string,
) -> ^Document_Entry {
	if workspace == nil {
		return nil
	}

	if entry := workspace_get_document_entry(workspace, uri); entry != nil {
		return entry
	}

	entry := document_entry_init(workspace, uri, path)
	key := strings.clone(uri)
	if sync.guard(&workspace.lock) {
		if existing, ok := workspace.documents[uri]; ok {
			delete(key)
			document_entry_deinit(entry)
			return existing
		}
		workspace.documents[key] = entry
	}

	return entry
}