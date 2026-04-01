package lsp

import "../cache"
import "../lang/symbols"

import "core:log"
import "core:encoding/json"
import "core:strings"

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

	wd, wd_ok := work_done_session_begin(srv, "ABAP: refreshing after remote dependencies")
	analysis_progress: cache.Analysis_Progress
	prog: ^cache.Analysis_Progress = nil
	if wd_ok {
		defer work_done_session_end(&wd)
		work_done_session_report(&wd, "Reloading workspace and rebuilding projects…")
		work_done_fill_analysis_progress(&wd, &analysis_progress)
		prog = &analysis_progress
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

	publish_diagnostics(srv, updated_params.sourceUri, snap, nil, prog)
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

	wd, wd_ok := work_done_session_begin(srv, "ABAP: workspace manifest updated")
	if wd_ok {
		defer work_done_session_end(&wd)
		work_done_session_report(&wd, "Reloading manifest and invalidating projects…")
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
	raw_candidates := make([dynamic]symbols.Remote_Candidate, context.temp_allocator)
	for project in projects {
		append_project_remote_candidates_for_notification(&raw_candidates, project)
	}

	for candidate in raw_candidates {
		if !cache.workspace_should_request_remote_candidate(workspace, candidate) {
			continue
		}
		append(
			&candidates,
			RemoteDependencyCandidate{
				name = candidate.name,
				kind = remote_candidate_kind_string(candidate.kind),
			},
		)
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
			remoteRequestParallelism = cache.workspace_remote_request_parallelism(workspace),
			remoteRequestsPerSecond = cache.workspace_remote_requests_per_second(workspace),
			candidates         = candidates[:],
		},
	)
}

append_project_remote_candidates_for_notification :: proc(
	candidates: ^[dynamic]symbols.Remote_Candidate,
	project: ^cache.Project,
) {
	assert(candidates != nil && project != nil)

	for candidate in project.remote_candidates {
		append_remote_candidate_for_notification(candidates, candidate)
	}

	if project.resolution_result == nil {
		return
	}

	for _, table in project.resolution_result.file_tables {
		if table == nil {
			continue
		}
		for candidate in symbols.collect_all_remote_candidates(table, context.temp_allocator) {
			append_remote_candidate_for_notification(candidates, candidate)
		}
	}
}

append_remote_candidate_for_notification :: proc(
	candidates: ^[dynamic]symbols.Remote_Candidate,
	candidate: symbols.Remote_Candidate,
) {
	assert(candidates != nil)

	normalized_name := strings.to_lower(strings.trim_space(candidate.name), context.temp_allocator)
	if len(normalized_name) == 0 {
		return
	}

	for i in 0 ..< len(candidates^) {
		existing := &candidates^[i]
		if existing.name != normalized_name {
			continue
		}
		if remote_candidate_kind_priority(candidate.kind) > remote_candidate_kind_priority(existing.kind) {
			existing.kind = candidate.kind
		}
			return
	}

	append(
		candidates,
		symbols.Remote_Candidate{
			name = strings.clone(normalized_name, context.allocator),
			kind = candidate.kind,
		},
	)
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

remote_candidate_kind_priority :: proc(kind: symbols.Remote_Candidate_Kind) -> int {
	switch kind {
	case .Include:
		return 4
	case .Static_Target:
		return 3
	case .Type_Name:
		return 2
	case .Unknown_Symbol:
		return 1
	}
	return 0
}
