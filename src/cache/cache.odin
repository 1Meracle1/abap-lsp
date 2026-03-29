package cache

import "../lang/ast"
import "../lang/symbols"

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
	resolution_result: ^symbols.ProjectResolutionResult,
	documents:         [dynamic]^Snapshot,
}

Workspace :: struct {
	lock:         sync.RW_Mutex,
	uri:          string,
	name:         string,
	root_path:    string,
	manifest:     ^Manifest,
	documents:    map[string]^Document_Entry,
	projects:     map[string]^Project_Entry,
	doc_pool:     ^Arena_Pool,
	project_pool: ^Arena_Pool,
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
	workspace.uri = strings.clone(uri)
	workspace.name = strings.clone(name)
	workspace.root_path = uri_to_path(uri)
	workspace.documents = make(map[string]^Document_Entry)
	workspace.projects = make(map[string]^Project_Entry)
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