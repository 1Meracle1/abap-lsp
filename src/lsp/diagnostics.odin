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

	uri := diagnostic_params.textDocument.uri

	snap := cache.get_snapshot(srv.storage, uri)
	if snap == nil {
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

	// Get the effective symbol table (from project if available, otherwise from snapshot)
	effective_table := cache.get_effective_symbol_table(srv.storage, uri)
	if effective_table != nil {
		semantic_errors := symbols.collect_all_diagnostics(effective_table, context.temp_allocator)
		for err in semantic_errors {
			append(&diagnostics, Diagnostic{
				range    = text_range_to_lsp_range(snap.text, err.range),
				severity = .Error,
				source   = "abap-lsp",
				message  = err.message,
			})
		}
	} else if snap.symbol_table != nil {
		// Fall back to snapshot's symbol table for standalone files
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

	// Add project-level diagnostics
	project := cache.get_project_for_uri(srv.storage, uri)
	if project != nil && uri == project.root_uri {
		for err in project.diagnostics {
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

publish_diagnostics :: proc(
	srv: ^Server,
	uri: string,
	snap: ^cache.Snapshot,
	project: ^cache.Project = nil,
) {
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

	// Get the effective symbol table (from project if available)
	effective_table := cache.get_effective_symbol_table(srv.storage, uri)
	if effective_table != nil {
		semantic_errors := symbols.collect_all_diagnostics(effective_table, context.temp_allocator)
		for err in semantic_errors {
			append(&diagnostics, Diagnostic{
				range    = text_range_to_lsp_range(snap.text, err.range),
				severity = .Error,
				source   = "abap-lsp",
				message  = err.message,
			})
		}
	} else if snap.symbol_table != nil {
		// Fall back to snapshot's symbol table for standalone files
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

	// Project-level diagnostics (e.g., missing includes)
	// Only show for the root file of the project
	if project != nil && uri == project.root_uri {
		for err in project.diagnostics {
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