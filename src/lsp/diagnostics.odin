package lsp

import "../cache"
import "core:encoding/json"
import "core:fmt"

import "../lang/ast"
import "../lang/lexer"
import "../lang/symbols"

handle_diagnostic :: proc(srv: ^Server, id: json.Value, params: json.Value) {
	diagnostic_params: DocumentDiagnosticParams
	if err := unmarshal(params, diagnostic_params, context.temp_allocator); err != nil {
		descr := fmt.tprintf("diagnostic request unmarshal failed: %v", err)
		log_trace(srv, descr)
		reply_error(srv, id, .ParseError, descr)
		return
	}

	snap := cache.get_snapshot(srv.storage, diagnostic_params.textDocument.uri)
	if snap == nil {
		// Return empty diagnostics if document not found
		result := FullDocumentDiagnosticReport {
			kind  = DocumentDiagnosticReportKind_Full,
			items = {},
		}
		reply(srv, id, result)
		return
	}
	defer cache.release_snapshot(snap)

	diagnostics := make([dynamic]Diagnostic, context.temp_allocator)

	// Syntax errors from parser
	for err in snap.ast.syntax_errors {
		append(&diagnostics, Diagnostic{
			range    = text_range_to_lsp_range(snap.text, err.range),
			severity = .Error,
			source   = "abap-lsp",
			message  = err.message,
		})
	}

	// Semantic errors from symbol resolver (e.g., duplicate symbols)
	if snap.symbol_table != nil {
		semantic_errors := symbols.collect_all_diagnostics(snap.symbol_table, context.temp_allocator)
		for err in semantic_errors {
			append(&diagnostics, Diagnostic{
				range    = text_range_to_lsp_range(snap.text, err.range),
				severity = .Error,
				source   = "abap-lsp",
				message  = err.message,
			})
		}
	}

	result := FullDocumentDiagnosticReport {
		kind  = DocumentDiagnosticReportKind_Full,
		items = diagnostics[:],
	}
	reply(srv, id, result)
}

publish_diagnostics :: proc(srv: ^Server, uri: string, snap: ^cache.Snapshot) {
	diagnostics := make([dynamic]Diagnostic, context.temp_allocator)

	// Syntax errors from parser
	for err in snap.ast.syntax_errors {
		append(&diagnostics, Diagnostic{
			range    = text_range_to_lsp_range(snap.text, err.range),
			severity = .Error,
			source   = "abap-lsp",
			message  = err.message,
		})
	}

	// Semantic errors from symbol resolver (e.g., duplicate symbols)
	if snap.symbol_table != nil {
		semantic_errors := symbols.collect_all_diagnostics(snap.symbol_table, context.temp_allocator)
		for err in semantic_errors {
			append(&diagnostics, Diagnostic{
				range    = text_range_to_lsp_range(snap.text, err.range),
				severity = .Error,
				source   = "abap-lsp",
				message  = err.message,
			})
		}
	}

	params := PublishDiagnosticsParams{
		uri         = uri,
		version     = snap.version,
		diagnostics = diagnostics[:],
	}

	notify(srv, "textDocument/publishDiagnostics", params)
}