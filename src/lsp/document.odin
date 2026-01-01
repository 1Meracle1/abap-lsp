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
	cache.refresh_document(
		srv.storage,
		uri,
		document_open_params.textDocument.text,
		document_open_params.textDocument.version,
	)

	// Publish diagnostics after refresh
	snap := cache.get_snapshot(srv.storage, uri)
	if snap != nil {
		defer cache.release_snapshot(snap)
		publish_diagnostics(srv, uri, snap)
	}
}

handle_document_change :: proc(srv: ^Server, params: json.Value) {
	document_change_params: DidChangeTextDocumentParams
	if err := unmarshal(params, document_change_params, context.temp_allocator); err != nil {
		descr := fmt.tprintf("textDocument/didChange request unmarshal failed: %v", err)
		log_trace(srv, descr)
		return
	}

	uri := document_change_params.textDocument.uri
	for change in document_change_params.contentChanges {
		cache.refresh_document(
			srv.storage,
			uri,
			change.text,
			document_change_params.textDocument.version,
		)
	}

	// Publish diagnostics after refresh
	snap := cache.get_snapshot(srv.storage, uri)
	if snap != nil {
		defer cache.release_snapshot(snap)
		publish_diagnostics(srv, uri, snap)
	}
}