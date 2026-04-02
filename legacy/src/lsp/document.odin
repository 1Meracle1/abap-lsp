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

	if srv.storage == nil {
		return
	}

	wd, wd_ok := work_done_session_begin(srv, "ABAP: analyzing document")
	analysis_progress: cache.Analysis_Progress
	prog: ^cache.Analysis_Progress = nil
	if wd_ok {
		defer work_done_session_end(&wd)
		work_done_session_report(&wd, "Parsing and loading semantic project…")
		work_done_fill_analysis_progress(&wd, &analysis_progress)
		prog = &analysis_progress
	}

	cache.refresh_document(
		srv.storage,
		uri,
		document_open_params.textDocument.text,
		document_open_params.textDocument.version,
		prog,
	)

	// Publish diagnostics immediately on open so the client does not need
	// to wait for the first edit before syntax/semantic errors appear.
	snap := cache.get_snapshot(srv.storage, uri)
	if snap != nil {
		defer cache.release_snapshot(snap)
		publish_diagnostics(srv, uri, snap, nil, prog)
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
			nil,
		)
	}

	// Publish diagnostics after refresh (no window progress: avoid noise on each keystroke)
	snap := cache.get_snapshot(srv.storage, uri)
	if snap != nil {
		defer cache.release_snapshot(snap)
		publish_diagnostics(srv, uri, snap, nil, nil)
	}
}
