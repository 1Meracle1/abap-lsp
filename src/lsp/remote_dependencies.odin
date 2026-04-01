package lsp

import "../cache"
import "../lang/symbols"

import "core:log"
import "core:encoding/json"

Remote_Dependency_Resolve_Notification :: "abapls/resolveRemoteDependencies"
Remote_Dependencies_Updated_Notification :: "abapls/remoteDependenciesUpdated"
Workspace_Manifest_Updated_Notification :: "abapls/workspaceManifestUpdated"

handle_remote_dependencies_updated :: proc(srv: ^Server, params: json.Value) {
	updated_params: RemoteDependenciesUpdatedParams
	if err := unmarshal(params, updated_params, context.temp_allocator); err != nil {
		log_trace(srv, "remote dependency update unmarshal failed")
		return
	}
	log.infof("remote dependencies updated for %s", updated_params.sourceUri)

	workspace := cache.workspace_for_uri(srv.storage, updated_params.workspaceUri)
	if workspace == nil {
		return
	}

	cache.workspace_load_manifest(workspace)
	cache.workspace_invalidate_all_projects(workspace)

	if len(updated_params.sourceUri) == 0 {
		return
	}

	snap := cache.get_snapshot(srv.storage, updated_params.sourceUri)
	if snap == nil {
		return
	}
	defer cache.release_snapshot(snap)

	publish_diagnostics(srv, updated_params.sourceUri, snap)
}

handle_workspace_manifest_updated :: proc(srv: ^Server, params: json.Value) {
	updated_params: WorkspaceManifestUpdatedParams
	if err := unmarshal(params, updated_params, context.temp_allocator); err != nil {
		log_trace(srv, "workspace manifest update unmarshal failed")
		return
	}

	workspace := cache.workspace_for_uri(srv.storage, updated_params.workspaceUri)
	if workspace == nil {
		return
	}

	cache.workspace_load_manifest(workspace)
	cache.workspace_invalidate_all_projects(workspace)
}

maybe_request_remote_dependency_resolution :: proc(
	srv: ^Server,
	uri: string,
	projects: []^cache.Project,
) {
	assert(srv != nil && srv.storage != nil && len(uri) > 0 && len(projects) > 0)

	workspace := cache.workspace_for_uri(srv.storage, uri)
	if workspace == nil || !cache.workspace_supports_remote_resolution(workspace) {
		return
	}
	if cache.workspace_uri_is_remote_dependency(workspace, uri) {
		return
	}
	for project in projects {
		if cache.project_has_syntax_errors(project) {
			return
		}
	}

	candidates := make([dynamic]RemoteDependencyCandidate, context.temp_allocator)
	for project in projects {
		append_project_remote_candidates_for_notification(&candidates, workspace, project)
	}

	if len(candidates) == 0 {
		return
	}

	log.infof("requesting remote dependency resolution for %s", uri)
	notify(
		srv,
		Remote_Dependency_Resolve_Notification,
		RemoteDependencyResolveParams{
			workspaceUri       = workspace.uri,
			sourceUri          = uri,
			unknownSymbolMode  = cache.workspace_unknown_symbol_mode(workspace),
			candidates         = candidates[:],
		},
	)
}

append_project_remote_candidates_for_notification :: proc(
	candidates: ^[dynamic]RemoteDependencyCandidate,
	workspace: ^cache.Workspace,
	project: ^cache.Project,
) {
	assert(candidates != nil && workspace != nil && project != nil)

	for candidate in project.remote_candidates {
		append_remote_candidate_for_notification(candidates, workspace, candidate)
	}

	if project.resolution_result == nil {
		return
	}

	for _, table in project.resolution_result.file_tables {
		if table == nil {
			continue
		}
		for candidate in symbols.collect_all_remote_candidates(table, context.temp_allocator) {
			append_remote_candidate_for_notification(candidates, workspace, candidate)
		}
	}
}

append_remote_candidate_for_notification :: proc(
	candidates: ^[dynamic]RemoteDependencyCandidate,
	workspace: ^cache.Workspace,
	candidate: symbols.Remote_Candidate,
) {
	assert(candidates != nil && workspace != nil)

	if !cache.workspace_should_request_remote_candidate(workspace, candidate) {
		return
	}

	item := RemoteDependencyCandidate{
		name = candidate.name,
		kind = remote_candidate_kind_string(candidate.kind),
	}
	for existing in candidates^ {
		if existing.name == item.name && existing.kind == item.kind {
			return
		}
	}
	append(candidates, item)
}

remote_candidate_kind_string :: proc(kind: symbols.Remote_Candidate_Kind) -> string {
	switch kind {
	case .Include:
		return "include"
	case .Type_Name:
		return "type"
	case .Static_Target:
		return "static"
	case .Unknown_Symbol:
		return "symbol"
	}
	return "unknown"
}
