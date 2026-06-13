package abap_frontend_lsp

import "src:parser"
import "src:semantic"
import workspace "src:workspace"

import json "core:encoding/json"
import "core:mem"
import "core:mem/virtual"
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
					close_document_uri(state, doc_uri)
				}
				changed = true
			case FILE_CHANGE_CHANGED, FILE_CHANGE_CREATED:
				queue_pending_uri(&state.pending_disk_refresh_uris, uri, state.allocator)
				changed = true
				for doc_uri, &doc in state.documents {
					if !lsp_uri_matches_or_under(doc_uri, uri) {
						continue
					}
					doc.dirty = true
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
			queue_pending_uri(&state.pending_disk_refresh_uris, uri, state.allocator)
			changed = true
			for doc_uri, &doc in state.documents {
				if !lsp_uri_matches_or_under(doc_uri, uri) {
					continue
				}
				doc.dirty = true
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
				close_document_uri(state, doc_uri)
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
				close_document_uri(state, doc_uri)
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
	return upsert_document_text(state, uri, text, version)
}

update_document_from_change :: proc(state: ^Server_State, params: json.Value) -> bool {
	object := params.(json.Object) or_return
	text_document := object_object(object, "textDocument") or_return
	changes := object_array(object, "contentChanges") or_return
	uri := object_string(text_document, "uri") or_return
	version := object_integer(text_document, "version") or_return
	last_change := changes[len(changes) - 1].(json.Object) or_return
	text := object_string(last_change, "text") or_return
	return upsert_document_text(state, uri, text, version)
}

close_document :: proc(state: ^Server_State, params: json.Value) -> bool {
	object := params.(json.Object) or_return
	text_document := object_object(object, "textDocument") or_return
	uri := object_string(text_document, "uri") or_return
	uri = normalize_lsp_uri(uri, context.temp_allocator)
	close_document_uri(state, uri)
	queue_disk_refresh_or_remove(state, uri)
	return true
}

server_reanalyze :: proc(state: ^Server_State) {
	clear_parse_diagnostics(state)
	retired_parse_arenas := make([dynamic]^virtual.Arena, 0, 2, context.temp_allocator)
	for _, doc in state.documents {
		ensure_workspace_for_document(state, doc.uri)
	}
	for &slot, workspace_index in state.workspaces {
		inputs := make(
			[dynamic]semantic.Workspace_File_Input,
			0,
			len(state.documents),
			context.temp_allocator,
		)
		removed_paths := make([dynamic]string, 0, 4, context.temp_allocator)
		if !slot.has_analysis {
			append_workspace_disk_inputs(state, &slot.root, &inputs)
		}
		for _, &workspace_doc in state.documents {
			doc_workspace_index, doc_workspace_ok := workspace_index_for_uri(state, workspace_doc.uri)
			if !doc_workspace_ok || doc_workspace_index != workspace_index {
				continue
			}
			if workspace_doc.dirty || !workspace_doc.has_parse {
				if retired := document_reparse(&workspace_doc, state.allocator); retired != nil {
					append(&retired_parse_arenas, retired)
				}
			}
			append_parse_diagnostics(state, workspace_doc.uri, workspace_doc.parse_errors)
			if slot.has_analysis && !workspace_doc.dirty {
				continue
			}
			append(
				&inputs,
				semantic.Workspace_File_Input {
					path = strings.clone(workspace_doc.uri, context.temp_allocator),
					root = workspace_doc.parse_root,
					kind = .Unknown,
				},
			)
		}
		if slot.has_analysis {
			for refresh_uri in state.pending_disk_refresh_uris {
				refresh_workspace_index, refresh_workspace_ok := workspace_index_for_uri(
					state,
					refresh_uri,
				)
				if !refresh_workspace_ok || refresh_workspace_index != workspace_index {
					continue
				}
				if _, still_open := state.documents[refresh_uri]; still_open {
					continue
				}
				append_disk_inputs_for_uri(state, refresh_uri, &inputs)
			}
			for file in slot.analysis.session.editable_files {
				removed := false
				for removed_uri in state.pending_removed_uris {
					if lsp_uri_matches_or_under(file.path, removed_uri) {
						removed = true
						break
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
	for arena in retired_parse_arenas {
		document_parse_arena_destroy(arena, state.allocator)
	}
	if state.pending_removed_uris.allocator.procedure != nil {
		clear(&state.pending_removed_uris)
	}
	if state.pending_disk_refresh_uris.allocator.procedure != nil {
		clear(&state.pending_disk_refresh_uris)
	}
}

upsert_document_text :: proc(
	state: ^Server_State,
	raw_uri: string,
	text: string,
	version: int,
) -> bool {
	uri := normalize_lsp_uri(raw_uri, context.temp_allocator)
	ensure_workspace_for_document(state, uri)
	if doc, ok := state.documents[uri]; ok {
		if doc.owns_text && doc.text != "" {
			delete(doc.text, state.allocator)
		}
		doc.text = strings.clone(text, state.allocator)
		doc.owns_text = true
		doc.version = version
		doc.dirty = true
		state.documents[doc.uri] = doc
		return true
	}
	owned_uri := strings.clone(uri, state.allocator)
	state.documents[owned_uri] = Document {
		uri       = owned_uri,
		text      = strings.clone(text, state.allocator),
		owns_uri  = true,
		owns_text = true,
		version   = version,
		dirty     = true,
	}
	return true
}

close_document_uri :: proc(state: ^Server_State, uri: string) -> bool {
	doc, ok := state.documents[uri]
	if !ok {
		return false
	}
	delete_key(&state.documents, uri)
	document_destroy(&doc, state.allocator)
	return true
}

document_reparse :: proc(doc: ^Document, allocator: mem.Allocator) -> ^virtual.Arena {
	assert(doc != nil && doc.uri != "")
	retired := doc.parse_arena
	arena := new(virtual.Arena, allocator)
	assert(arena != nil)
	arena_err := virtual.arena_init_growing(arena)
	assert(arena_err == .None)
	parsed := parser.parse(doc.text, doc.uri, virtual.arena_allocator(arena))
	doc.parse_arena = arena
	doc.parse_root = parsed.root
	doc.parse_errors = parsed.errors
	doc.has_parse = true
	return retired
}

document_destroy :: proc(doc: ^Document, allocator: mem.Allocator) {
	if doc == nil {
		return
	}
	document_parse_arena_destroy(doc.parse_arena, allocator)
	if doc.owns_text && doc.text != "" {
		delete(doc.text, allocator)
	}
	if doc.owns_uri && doc.uri != "" {
		delete(doc.uri, allocator)
	}
	doc^ = {}
}

document_parse_arena_destroy :: proc(arena: ^virtual.Arena, allocator: mem.Allocator) {
	if arena == nil {
		return
	}
	virtual.arena_destroy(arena)
	free(arena, allocator)
}

queue_pending_uri :: proc(list: ^[dynamic]string, uri: string, allocator: mem.Allocator) {
	if uri == "" {
		return
	}
	if list.allocator.procedure == nil {
		list^ = make([dynamic]string, 0, 8, allocator)
	}
	for existing in list^ {
		if existing == uri {
			return
		}
	}
	append(list, strings.clone(uri, allocator))
}

queue_disk_refresh_or_remove :: proc(state: ^Server_State, uri: string) {
	path, path_ok := file_uri_to_path(uri, context.temp_allocator)
	if !path_ok {
		return
	}
	info, stat_err := os.stat(path, context.temp_allocator)
	if stat_err == nil &&
	   info.type == .Regular &&
	   lsp_is_abap_path(path) {
		queue_pending_uri(&state.pending_disk_refresh_uris, uri, state.allocator)
		return
	}
	queue_pending_uri(&state.pending_removed_uris, uri, state.allocator)
}

append_workspace_disk_inputs :: proc(
	state: ^Server_State,
	root: ^workspace.Workspace,
	out: ^[dynamic]semantic.Workspace_File_Input,
) {
	paths := make([dynamic]string, 0, 32, context.temp_allocator)
	workspace.collect_workspace_abap_files(root.root_path, &paths, context.temp_allocator)
	for path in paths {
		append_disk_input_for_path(state, path, out)
	}
}

append_disk_inputs_for_uri :: proc(
	state: ^Server_State,
	uri: string,
	out: ^[dynamic]semantic.Workspace_File_Input,
) {
	path, path_ok := file_uri_to_path(uri, context.temp_allocator)
	if !path_ok {
		return
	}
	info, stat_err := os.stat(path, context.temp_allocator)
	if stat_err != nil {
		return
	}
	#partial switch info.type {
	case .Directory:
		paths := make([dynamic]string, 0, 16, context.temp_allocator)
		workspace.collect_workspace_abap_files(path, &paths, context.temp_allocator)
		for file_path in paths {
			append_disk_input_for_path(state, file_path, out)
		}
	case .Regular:
		if lsp_is_abap_path(path) {
			append_disk_input_for_path(state, path, out)
		}
	}
}

append_disk_input_for_path :: proc(
	state: ^Server_State,
	path: string,
	out: ^[dynamic]semantic.Workspace_File_Input,
) {
	source, source_ok := workspace.read_text_file(path, context.temp_allocator)
	if !source_ok {
		return
	}
	uri, uri_ok := file_uri_from_path(path, context.temp_allocator)
	if !uri_ok {
		return
	}
	parsed := parser.parse(source, uri, state.allocator)
	append(
		out,
		semantic.Workspace_File_Input {
			path = uri,
			root = parsed.root,
			kind = .Unknown,
		},
	)
}

file_uri_from_path :: proc(path: string, allocator: mem.Allocator) -> (string, bool) {
	abs_path, path_ok := workspace.absolute_clean_path(path, context.temp_allocator)
	if !path_ok {
		return "", false
	}
	normalized := normalize_lsp_uri(abs_path, context.temp_allocator)
	out := strings.builder_make(allocator)
	if len(normalized) >= 2 && normalized[1] == ':' {
		strings.write_string(&out, "file:///")
	} else {
		strings.write_string(&out, "file://")
	}
	write_file_uri_path(&out, normalized)
	return strings.to_string(out), true
}

write_file_uri_path :: proc(out: ^strings.Builder, path: string) {
	for ch in path {
		switch ch {
		case ' ':
			strings.write_string(out, "%20")
		case '#':
			strings.write_string(out, "%23")
		case '%':
			strings.write_string(out, "%25")
		case '?':
			strings.write_string(out, "%3F")
		case:
			strings.write_rune(out, ch)
		}
	}
}

lsp_is_abap_path :: proc(path: string) -> bool {
	lower := strings.to_lower(path, context.temp_allocator)
	return strings.has_suffix(lower, ".abap")
}
