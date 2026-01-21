package lsp

import "../cache"
import "core:encoding/json"
import "core:fmt"

handle_document_open :: proc(srv: ^Server, params: json.Value) {
	document_open_params: DidOpenTextDocumentParams
	if err := unmarshal(params, document_open_params, context.temp_allocator); err != nil {
		descr := fmt.tprintf("textDocument/didOpen request unmarshal failed: %v", err)
		log_trace(srv, descr)
		return
	}

	uri := document_open_params.textDocument.uri

	if len(srv.workspaces) == 0 {
		return
	}
	workspace := srv.workspaces[0]

	document := cache.document_init(
		workspace,
		uri,
		"",
		document_open_params.textDocument.text,
		document_open_params.textDocument.version,
	)

	// // Ensure project context exists for this file
	// project := cache.ensure_project_context(srv.storage, uri)

	// // If file belongs to a project, rebuild merged symbol table
	// if project != nil {
	// 	cache.invalidate_project(project)
	// 	cache.resolve_project(srv.storage, project)
	// }

	// // Publish diagnostics after refresh
	// snap := cache.get_snapshot(srv.storage, uri)
	// if snap != nil {
	// 	defer cache.release_snapshot(snap)
	// 	publish_diagnostics(srv, uri, snap, project)
	// }
}

handle_document_change :: proc(srv: ^Server, params: json.Value) {
	document_change_params: DidChangeTextDocumentParams
	if err := unmarshal(params, document_change_params, context.temp_allocator); err != nil {
		descr := fmt.tprintf("textDocument/didChange request unmarshal failed: %v", err)
		log_trace(srv, descr)
		return
	}

	uri := document_change_params.textDocument.uri

	// Get project context for this file
	project := cache.get_project_for_uri(srv.storage, uri)

	for change in document_change_params.contentChanges {
		cache.refresh_document(
			srv.storage,
			uri,
			change.text,
			document_change_params.textDocument.version,
		)
	}

	// If file belongs to a project, rebuild merged symbol table
	if project != nil {
		cache.invalidate_project(project)
		cache.resolve_project(srv.storage, project)
	}

	// Publish diagnostics after refresh
	snap := cache.get_snapshot(srv.storage, uri)
	if snap != nil {
		defer cache.release_snapshot(snap)
		publish_diagnostics(srv, uri, snap, project)
	}
}
