package abap_frontend_lsp

import "src:parser"
import "src:semantic"
import workspace "src:workspace"

import json "core:encoding/json"
import "core:os"
import "core:strings"

handle_notification :: proc(
	state: ^Server_State,
	output: ^os.File,
	method: string,
	params: json.Value,
) {
	switch method {
	case METHOD_INITIALIZED:
		state.initialized = true
	case METHOD_DID_OPEN:
		if update_document_from_open(state, params) {
			server_reanalyze(state)
			publish_all_diagnostics(state, output)
		}
	case METHOD_DID_CHANGE:
		if update_document_from_change(state, params) {
			server_reanalyze(state)
			publish_all_diagnostics(state, output)
		}
	case METHOD_DID_CLOSE:
		if close_document(state, params) {
			server_reanalyze(state)
			publish_all_diagnostics(state, output)
		}
	case METHOD_DID_CHANGE_WATCHED_FILES:
		changed := false
		object, object_ok := params.(json.Object)
		if !object_ok {
			return
		}
		changes, changes_ok := object_array(object, "changes")
		if !changes_ok {
			return
		}
		if state.pending_removed_uris.allocator.procedure == nil {
			state.pending_removed_uris = make([dynamic]string, 0, len(changes), state.allocator)
		}
		for value in changes {
			event, event_ok := value.(json.Object)
			if !event_ok {
				continue
			}
			uri, uri_ok := object_string(event, "uri")
			if !uri_ok {
				continue
			}
			change_type, change_type_ok := object_integer(event, "type")
			if !change_type_ok {
				continue
			}
			uri = normalize_lsp_uri(uri, context.temp_allocator)
			switch change_type {
			case FILE_CHANGE_DELETED:
				append(&state.pending_removed_uris, uri)
				to_delete := make([dynamic]string, 0, 4, context.temp_allocator)
				for doc_uri, _ in state.documents {
					if lsp_uri_matches_or_under(doc_uri, uri) {
						append(&to_delete, doc_uri)
					}
				}
				for doc_uri in to_delete {
					delete_key(&state.documents, doc_uri)
				}
				changed = true
			case FILE_CHANGE_CHANGED, FILE_CHANGE_CREATED:
				for doc_uri, &doc in state.documents {
					if !lsp_uri_matches_or_under(doc_uri, uri) {
						continue
					}
					doc.dirty = true
					changed = true
				}
			case:
			}
		}
		if changed {
			server_reanalyze(state)
			publish_all_diagnostics(state, output)
		}
	case METHOD_DID_CREATE_FILES:
		changed := false
		object, object_ok := params.(json.Object)
		if !object_ok {
			return
		}
		files, files_ok := object_array(object, "files")
		if !files_ok {
			return
		}
		for value in files {
			file, file_ok := value.(json.Object)
			if !file_ok {
				continue
			}
			uri, uri_ok := object_string(file, "uri")
			if !uri_ok {
				continue
			}
			uri = normalize_lsp_uri(uri, context.temp_allocator)
			for doc_uri, &doc in state.documents {
				if !lsp_uri_matches_or_under(doc_uri, uri) {
					continue
				}
				doc.dirty = true
				changed = true
			}
		}
		if changed {
			server_reanalyze(state)
			publish_all_diagnostics(state, output)
		}
	case METHOD_DID_DELETE_FILES:
		changed := false
		object, object_ok := params.(json.Object)
		if !object_ok {
			return
		}
		files, files_ok := object_array(object, "files")
		if !files_ok {
			return
		}
		if state.pending_removed_uris.allocator.procedure == nil {
			state.pending_removed_uris = make([dynamic]string, 0, len(files), state.allocator)
		}
		for value in files {
			file, file_ok := value.(json.Object)
			if !file_ok {
				continue
			}
			uri, uri_ok := object_string(file, "uri")
			if !uri_ok {
				continue
			}
			uri = normalize_lsp_uri(uri, context.temp_allocator)
			append(&state.pending_removed_uris, uri)
			to_delete := make([dynamic]string, 0, 4, context.temp_allocator)
			for doc_uri, _ in state.documents {
				if lsp_uri_matches_or_under(doc_uri, uri) {
					append(&to_delete, doc_uri)
				}
			}
			for doc_uri in to_delete {
				delete_key(&state.documents, doc_uri)
			}
			changed = true
		}
		if changed {
			server_reanalyze(state)
			publish_all_diagnostics(state, output)
		}
	case METHOD_DID_RENAME_FILES:
		changed := false
		object, object_ok := params.(json.Object)
		if !object_ok {
			return
		}
		files, files_ok := object_array(object, "files")
		if !files_ok {
			return
		}
		if state.pending_removed_uris.allocator.procedure == nil {
			state.pending_removed_uris = make([dynamic]string, 0, len(files), state.allocator)
		}
		for value in files {
			file, file_ok := value.(json.Object)
			if !file_ok {
				continue
			}
			old_uri, old_uri_ok := object_string(file, "oldUri")
			new_uri, new_uri_ok := object_string(file, "newUri")
			if !old_uri_ok || !new_uri_ok {
				continue
			}
			old_uri = normalize_lsp_uri(old_uri, context.temp_allocator)
			new_uri = normalize_lsp_uri(new_uri, context.temp_allocator)
			append(&state.pending_removed_uris, old_uri)
			to_delete := make([dynamic]string, 0, 4, context.temp_allocator)
			for doc_uri, _ in state.documents {
				if lsp_uri_matches_or_under(doc_uri, old_uri) {
					append(&to_delete, doc_uri)
				}
			}
			for doc_uri in to_delete {
				delete_key(&state.documents, doc_uri)
			}
			for doc_uri, &doc in state.documents {
				if !lsp_uri_matches_or_under(doc_uri, new_uri) {
					continue
				}
				doc.dirty = true
			}
			changed = true
		}
		if changed {
			server_reanalyze(state)
			publish_all_diagnostics(state, output)
		}
	case METHOD_DID_CHANGE_WORKSPACE_FOLDERS:
		changed := false
		object, object_ok := params.(json.Object)
		if !object_ok {
			return
		}
		event, event_ok := object_object(object, "event")
		if !event_ok {
			return
		}
		if removed, removed_ok := object_array(event, "removed"); removed_ok {
			for value in removed {
				folder, folder_ok := value.(json.Object)
				if !folder_ok {
					continue
				}
				uri, uri_ok := object_string(folder, "uri")
				if !uri_ok {
					continue
				}
				path, path_ok := file_uri_to_path(uri, context.temp_allocator)
				if !path_ok {
					continue
				}
				root_key := lsp_path_key(path, context.temp_allocator)
				for i := 0; i < len(state.workspaces); {
					if lsp_path_key(state.workspaces[i].root.root_path, context.temp_allocator) != root_key {
						i += 1
						continue
					}
					if state.workspaces[i].has_analysis {
						workspace.analysis_result_destroy(&state.workspaces[i].analysis, state.allocator)
					}
					workspace.workspace_destroy(&state.workspaces[i].root, state.allocator)
					ordered_remove(&state.workspaces, i)
					changed = true
				}
			}
		}
		if added, added_ok := object_array(event, "added"); added_ok {
			for value in added {
				folder, folder_ok := value.(json.Object)
				if !folder_ok {
					continue
				}
				uri, uri_ok := object_string(folder, "uri")
				if !uri_ok {
					continue
				}
				changed = open_workspace_for_uri(state, uri) || changed
			}
		}
		if changed {
			server_reanalyze(state)
			publish_all_diagnostics(state, output)
		}
	case:
	}
}

open_workspace_for_uri :: proc(state: ^Server_State, uri: string) -> bool {
	path := file_uri_to_path(uri, state.allocator) or_return
	return open_workspace_for_path(state, path)
}

open_workspace_for_path :: proc(state: ^Server_State, path: string) -> bool {
	opened, ok, _ := workspace.open(path, state.options, state.allocator)
	if !ok {
		opened, ok, _ = workspace.open_standalone(path, state.options, state.allocator)
	}
	if !ok {
		return false
	}
	root_key := lsp_path_key(opened.root_path, context.temp_allocator)
	for &existing in state.workspaces {
		if lsp_path_key(existing.root.root_path, context.temp_allocator) != root_key {
			continue
		}
		workspace.workspace_destroy(&opened, state.allocator)
		return true
	}
	append(&state.workspaces, Server_Workspace{root = opened})
	return true
}

ensure_workspace_for_document :: proc(state: ^Server_State, uri: string) {
	if _, ok := workspace_index_for_uri(state, uri); ok {
		return
	}
	path, path_ok := file_uri_to_path(uri, context.temp_allocator)
	root := "."
	if path_ok {
		root = os.dir(path)
	}
	open_workspace_for_path(state, root)
}

update_document_from_open :: proc(state: ^Server_State, params: json.Value) -> bool {
	object := params.(json.Object) or_return
	text_document := object_object(object, "textDocument") or_return
	uri := object_string(text_document, "uri") or_return
	text := object_string(text_document, "text") or_return
	version := object_integer(text_document, "version") or_return
	uri = normalize_lsp_uri(uri, state.allocator)
	ensure_workspace_for_document(state, uri)
	state.documents[uri] = Document {
		uri     = uri,
		text    = strings.clone(text, state.allocator),
		version = version,
		dirty   = true,
	}
	return true
}

update_document_from_change :: proc(state: ^Server_State, params: json.Value) -> bool {
	object := params.(json.Object) or_return
	text_document := object_object(object, "textDocument") or_return
	changes := object_array(object, "contentChanges") or_return
	uri := object_string(text_document, "uri") or_return
	version := object_integer(text_document, "version") or_return
	last_change := changes[len(changes) - 1].(json.Object) or_return
	text := object_string(last_change, "text") or_return
	uri = normalize_lsp_uri(uri, state.allocator)
	ensure_workspace_for_document(state, uri)
	state.documents[uri] = Document {
		uri     = uri,
		text    = strings.clone(text, state.allocator),
		version = version,
		dirty   = true,
	}
	return true
}

close_document :: proc(state: ^Server_State, params: json.Value) -> bool {
	object := params.(json.Object) or_return
	text_document := object_object(object, "textDocument") or_return
	uri := object_string(text_document, "uri") or_return
	uri = normalize_lsp_uri(uri, context.temp_allocator)
	delete_key(&state.documents, uri)
	return true
}

server_reanalyze :: proc(state: ^Server_State) {
	clear(&state.parse_diagnostics)
	for _, doc in state.documents {
		ensure_workspace_for_document(state, doc.uri)
	}
	for &slot, workspace_index in state.workspaces {
		inputs := make(
			[dynamic]semantic.Workspace_File_Input,
			0,
			len(state.documents),
			state.allocator,
		)
		removed_paths := make([dynamic]string, 0, 4, context.temp_allocator)
		for _, workspace_doc in state.documents {
			doc_workspace_index, doc_workspace_ok := workspace_index_for_uri(state, workspace_doc.uri)
			if !doc_workspace_ok || doc_workspace_index != workspace_index {
				continue
			}
			parsed := parser.parse(workspace_doc.text, workspace_doc.uri, state.allocator)
			append_parse_diagnostics(state, workspace_doc.uri, parsed.errors)
			if slot.has_analysis && !workspace_doc.dirty {
				continue
			}
			append(
				&inputs,
				semantic.Workspace_File_Input {
					path = strings.clone(workspace_doc.uri, state.allocator),
					root = parsed.root,
					kind = .Unknown,
				},
			)
		}
		if slot.has_analysis {
			for file in slot.analysis.session.editable_files {
				_, still_open := state.documents[file.path]
				doc_workspace_index, doc_workspace_ok := workspace_index_for_uri(state, file.path)
				removed := !(still_open && doc_workspace_ok && doc_workspace_index == workspace_index)
				if !removed {
					for removed_uri in state.pending_removed_uris {
						if lsp_uri_matches_or_under(file.path, removed_uri) {
							removed = true
							break
						}
					}
				}
				if removed {
					append(&removed_paths, file.path)
				}
			}
		}
		if len(inputs) == 0 && len(removed_paths) == 0 {
			continue
		}
		slot.has_analysis = workspace.analysis_result_update_inputs(
			&slot.analysis,
			&slot.root,
			inputs[:],
			removed_paths[:],
			&state.pool,
			state.allocator,
		)
	}
	for _, &doc in state.documents {
		doc.dirty = false
	}
	if state.pending_removed_uris.allocator.procedure != nil {
		clear(&state.pending_removed_uris)
	}
}
