package cache

import "../lang/symbols"
import "core:strings"

Workspace :: struct {
	uri:       string,
	name:      string,
	root_path: string,
	manifest:  ^Manifest,
	packages:  map[string]^Package,
	documents: map[string]^Document,
}

Cache :: struct {
	workspaces: [dynamic]^Workspace,
}

Snapshot :: Document

Project :: struct {
	unit_name:         string,
	root_uri:          string,
	diagnostics:       [dynamic]symbols.Diagnostic,
	resolution_result: ^symbols.ProjectResolutionResult,
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
	for workspace in cache.workspaces {
		workspace_deinit(workspace)
	}
	delete(cache.workspaces)
	free(cache)
}

workspace_init :: proc(uri: string, name: string) -> ^Workspace {
	workspace := new(Workspace)
	workspace.uri = strings.clone(uri)
	workspace.name = strings.clone(name)
	workspace.root_path = uri_to_path(uri)
	workspace.packages = make(map[string]^Package)
	workspace.documents = make(map[string]^Document)
	workspace_load_manifest(workspace)
	return workspace
}

workspace_deinit :: proc(workspace: ^Workspace) {
	if workspace == nil {
		return
	}
	for _, document in workspace.documents {
		document_deinit(document)
	}
	if workspace.manifest != nil {
		manifest_deinit(workspace.manifest)
	}
	delete(workspace.documents)
	delete(workspace.packages)
	free(&workspace.uri)
	free(&workspace.name)
	free(&workspace.root_path)
	free(workspace)
}

cache_add_workspace :: proc(cache: ^Cache, uri: string, name: string) -> ^Workspace {
	workspace := workspace_init(uri, name)
	append(&cache.workspaces, workspace)
	return workspace
}

workspace_for_uri :: proc(cache: ^Cache, uri: string) -> ^Workspace {
	if cache == nil {
		return nil
	}

	best_match: ^Workspace
	best_match_len := -1
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

	if best_match != nil {
		return best_match
	}

	if len(cache.workspaces) > 0 {
		return cache.workspaces[0]
	}

	return nil
}

get_snapshot :: proc(cache: ^Cache, uri: string) -> ^Snapshot {
	workspace := workspace_for_uri(cache, uri)
	if workspace == nil {
		return nil
	}
	if document, ok := workspace.documents[uri]; ok {
		return document
	}
	return nil
}

release_snapshot :: proc(snapshot: ^Snapshot) {
	_ = snapshot
}

refresh_document :: proc(cache: ^Cache, uri: string, text: string, version: int) {
	workspace := workspace_for_uri(cache, uri)
	if workspace == nil {
		return
	}

	path := uri_to_path(uri)
	if document, ok := workspace.documents[uri]; ok {
		if len(document.path) == 0 {
			document.path = strings.clone(path)
		}
		document_refresh(document, text, version)
		return
	}

	document_init(workspace, uri, path, text, version)
}

get_effective_symbol_table :: proc(cache: ^Cache, uri: string) -> ^symbols.SymbolTable {
	projects := get_projects_for_uri(cache, uri, context.temp_allocator)
	if len(projects) > 0 {
		if len(projects) == 1 {
			return get_file_symbol_table(projects[0], uri)
		}

		base_table := get_file_symbol_table(projects[0], uri)
		if base_table == nil {
			base_table = symbols.create_empty_symbol_table(context.temp_allocator)
		}
		merged_table := symbols.clone_symbol_table(base_table, context.temp_allocator)
		for project in projects[1:] {
			merge_symbol_tables_for_lookup(merged_table, get_file_symbol_table(project, uri))
		}
		return merged_table
	}

	snap := get_snapshot(cache, uri)
	if snap == nil {
		return nil
	}
	return snap.symbol_table
}

get_project_for_uri :: proc(cache: ^Cache, uri: string) -> ^Project {
	projects := get_projects_for_uri(cache, uri, context.temp_allocator)
	if len(projects) > 0 {
		return projects[0]
	}
	return nil
}

invalidate_project :: proc(project: ^Project) {
	_ = project
}

resolve_project :: proc(cache: ^Cache, project: ^Project) {
	_ = cache
	_ = project
}

update_package_for_document :: proc(workspace: ^Workspace, document: ^Document) -> ^Package {
	_ = workspace
	_ = document
	return nil
}