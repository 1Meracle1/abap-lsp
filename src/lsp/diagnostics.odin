package lsp

import "../cache"
import "core:encoding/json"
import "core:fmt"

import "../lang/ast"
import "../lang/lexer"
import "../lang/symbols"

append_diagnostic_unique :: proc(diagnostics: ^[dynamic]Diagnostic, diagnostic: Diagnostic) {
	for existing in diagnostics^ {
		if existing.range.start.line == diagnostic.range.start.line &&
			existing.range.start.character == diagnostic.range.start.character &&
			existing.range.end.line == diagnostic.range.end.line &&
			existing.range.end.character == diagnostic.range.end.character &&
			existing.message == diagnostic.message &&
			existing.source == diagnostic.source {
			return
		}
	}
	append(diagnostics, diagnostic)
}

append_symbol_diagnostics :: proc(
	diagnostics: ^[dynamic]Diagnostic,
	snap: ^cache.Snapshot,
	table: ^symbols.SymbolTable,
) {
	if snap == nil || table == nil {
		return
	}

	semantic_errors := symbols.collect_all_diagnostics(table, context.temp_allocator)
	for err in semantic_errors {
		append_diagnostic_unique(
			diagnostics,
			Diagnostic{
				range    = text_range_to_lsp_range(snap.text, err.range),
				severity = .Error,
				source   = "abap-lsp",
				message  = err.message,
			},
		)
	}
}

append_project_diagnostics :: proc(
	diagnostics: ^[dynamic]Diagnostic,
	snap: ^cache.Snapshot,
	uri: string,
	projects: []^cache.Project,
) {
	for project in projects {
		table := cache.get_file_symbol_table(project, uri)
		append_symbol_diagnostics(diagnostics, snap, table)

		if project != nil && uri == project.root_uri {
			for err in project.diagnostics {
				append_diagnostic_unique(
					diagnostics,
					Diagnostic{
						range    = text_range_to_lsp_range(snap.text, err.range),
						severity = .Error,
						source   = "abap-lsp",
						message  = err.message,
					},
				)
			}
		}
	}
}

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
		append_diagnostic_unique(&diagnostics, Diagnostic{
			range    = text_range_to_lsp_range(snap.text, err.range),
			severity = .Error,
			source   = "abap-lsp",
			message  = err.message,
		})
	}

	projects := cache.get_projects_for_uri(srv.storage, uri, context.temp_allocator)
	defer cache.release_projects(projects)
	if len(projects) > 0 {
		append_project_diagnostics(&diagnostics, snap, uri, projects)
	} else if snap.symbol_table != nil {
		append_symbol_diagnostics(&diagnostics, snap, snap.symbol_table)
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
	_ = project
	diagnostics := make([dynamic]Diagnostic, context.temp_allocator)

	// Syntax errors from parser
	for err in snap.ast.syntax_errors {
		append_diagnostic_unique(&diagnostics, Diagnostic{
			range    = text_range_to_lsp_range(snap.text, err.range),
			severity = .Error,
			source   = "abap-lsp",
			message  = err.message,
		})
	}

	projects := cache.get_projects_for_uri(srv.storage, uri, context.temp_allocator)
	defer cache.release_projects(projects)
	if len(projects) > 0 {
		append_project_diagnostics(&diagnostics, snap, uri, projects)
	} else if snap.symbol_table != nil {
		append_symbol_diagnostics(&diagnostics, snap, snap.symbol_table)
	}

	params := PublishDiagnosticsParams{
		uri         = uri,
		version     = snap.version,
		diagnostics = diagnostics[:],
	}

	notify(srv, "textDocument/publishDiagnostics", params)
}