package abap_frontend_lsp

import "src:semantic"
import utils "src:utils"

import json "core:encoding/json"
import "core:mem"
import "core:os"
import "core:strings"

Snapshot_Lookup :: struct {
	project_result: ^semantic.Workspace_Project_Result,
	project:        ^semantic.Project,
	checker:        ^semantic.Checker,
	file:           ^semantic.Project_File,
	provider_index: ^semantic.External_Semantic_Index,
	source:         string,
	ok:             bool,
}

Entity_Lookup :: struct {
	snapshot: Snapshot_Lookup,
	entity:   ^semantic.Entity,
	range:    semantic.Range,
	ok:       bool,
}

project_file_for_uri :: proc "contextless" (
	result: ^semantic.Workspace_Project_Result,
	uri: string,
) -> ^semantic.Project_File {
	for file in result.files {
		if file != nil && file.path == uri {
			return file
		}
	}
	return nil
}

snapshot_for_position :: proc(
	state: ^Server_State,
	params: json.Value,
) -> (
	Snapshot_Lookup,
	int,
	bool,
) {
	pos := text_document_position_from_params(params)
	if !pos.ok {
		return {}, 0, false
	}
	snapshot := snapshot_for_uri(state, pos.uri)
	if !snapshot.ok {
		return {}, 0, false
	}
	offset := position_to_offset(snapshot.source, pos.position)
	return snapshot, offset, true
}

snapshot_for_uri :: proc(state: ^Server_State, uri: string) -> Snapshot_Lookup {
	doc, doc_ok := state.documents[uri]
	if !doc_ok {
		return {}
	}
	candidate_indices := make([dynamic]int, 0, len(state.workspaces), context.temp_allocator)
	if workspace_index, workspace_ok := workspace_index_for_uri(state, uri); workspace_ok {
		append(&candidate_indices, workspace_index)
	}
	for i in 0 ..< len(state.workspaces) {
		if len(candidate_indices) > 0 && candidate_indices[0] == i {
			continue
		}
		append(&candidate_indices, i)
	}
	for workspace_index in candidate_indices {
		slot := &state.workspaces[workspace_index]
		if !slot.has_analysis {
			continue
		}
		analysis := semantic.semantic_graph_session_current_analysis(&slot.analysis.session)
		if analysis == nil {
			continue
		}
		for &result in analysis.project_results {
			if result.project == nil || result.checker == nil {
				continue
			}
			file := project_file_for_uri(&result, uri)
			if file == nil {
				continue
			}
			return Snapshot_Lookup {
				project_result = &result,
				project = result.project,
				checker = result.checker,
				file = file,
				provider_index = &analysis.external_context.index,
				source = doc.text,
				ok = true,
			}
		}
	}
	return {}
}

workspace_index_for_uri :: proc(state: ^Server_State, uri: string) -> (int, bool) {
	path, ok := file_uri_to_path(uri, context.temp_allocator)
	if !ok {
		if len(state.workspaces) > 0 {
			return 0, true
		}
		return -1, false
	}
	return workspace_index_for_path(state, path)
}

workspace_index_for_path :: proc(state: ^Server_State, path: string) -> (int, bool) {
	path_key := lsp_path_key(path, context.temp_allocator)
	best_index := -1
	best_len := -1
	for &slot, i in state.workspaces {
		root_key := lsp_path_key(slot.root.root_path, context.temp_allocator)
		under_root := path_key == root_key ||
		              (len(path_key) > len(root_key) &&
		               strings.has_prefix(path_key, root_key) &&
		               path_key[len(root_key)] == '/')
		if under_root && len(root_key) > best_len {
			best_index = i
			best_len = len(root_key)
		}
	}
	return best_index, best_index >= 0
}

lsp_path_key :: proc(path: string, allocator: mem.Allocator) -> string {
	cleaned, clean_err := os.clean_path(path, allocator)
	if clean_err == nil {
		return utils.normalized_uri_path_key(cleaned, allocator)
	}
	return utils.normalized_uri_path_key(path, allocator)
}

lsp_uri_matches_or_under :: proc(candidate_uri, root_uri: string) -> bool {
	if candidate_uri == root_uri {
		return true
	}
	candidate_path, candidate_path_ok := file_uri_to_path(candidate_uri, context.temp_allocator)
	root_path, root_path_ok := file_uri_to_path(root_uri, context.temp_allocator)
	if candidate_path_ok || root_path_ok {
		candidate_location := candidate_uri
		root_location := root_uri
		if candidate_path_ok {
			candidate_location = candidate_path
		}
		if root_path_ok {
			root_location = root_path
		}
		candidate_key := lsp_path_key(candidate_location, context.temp_allocator)
		root_key := lsp_path_key(root_location, context.temp_allocator)
		return candidate_key == root_key ||
		       (len(candidate_key) > len(root_key) &&
		        strings.has_prefix(candidate_key, root_key) &&
		        candidate_key[len(root_key)] == '/')
	}
	return len(candidate_uri) > len(root_uri) &&
	       strings.has_prefix(candidate_uri, root_uri) &&
	       candidate_uri[len(root_uri)] == '/'
}

entity_at_position :: proc(state: ^Server_State, params: json.Value) -> Entity_Lookup {
	snapshot, offset, ok := snapshot_for_position(state, params)
	if !ok {
		return {}
	}
	query := semantic.semantic_query(snapshot.project, snapshot.checker, snapshot.file)
	decl_query := semantic.semantic_query_decls(query)
	if entity := semantic.semantic_decl_entity_at_offset(decl_query, offset); entity != nil {
		return Entity_Lookup {
			snapshot = snapshot,
			entity = entity,
			range = entity.name_range,
			ok = true,
		}
	}
	if entity := semantic.semantic_decl_class_member_at_offset(decl_query, offset); entity != nil {
		return Entity_Lookup {
			snapshot = snapshot,
			entity = entity,
			range = semantic.semantic_member_query_range(entity, offset),
			ok = true,
		}
	}
	if entity := semantic.semantic_decl_structure_field_at_offset(decl_query, offset);
	   entity != nil {
		return Entity_Lookup {
			snapshot = snapshot,
			entity = entity,
			range = entity.name_range,
			ok = true,
		}
	}
	ref_query := semantic.semantic_query_refs(query)
	if use := semantic.semantic_ref_use_at_offset(ref_query, offset);
	   use != nil && use.entity != nil {
		range := semantic.semantic_entity_use_range(use^)
		return Entity_Lookup{snapshot = snapshot, entity = use.entity, range = range, ok = true}
	}
	return {}
}
