package abap_frontend_lsp

import "src:parser"
import "src:semantic"

import "core:fmt"
import "core:mem"
import "core:os"
import "core:strings"

append_parse_diagnostics :: proc(state: ^Server_State, uri: string, errors: []parser.Parse_Error) {
	bucket := Parse_Diagnostic_Bucket {
		uri    = strings.clone(uri, state.allocator),
		errors = make([dynamic]parser.Parse_Error, 0, len(errors), state.allocator),
	}
	for err in errors {
		append(&bucket.errors, err)
	}
	append(&state.parse_diagnostics, bucket)
}

publish_all_diagnostics :: proc(state: ^Server_State, output: ^os.File) {
	for uri, _ in state.documents {
		diagnostics := diagnostics_for_uri(state, uri, state.allocator)
		params := Publish_Diagnostics_Params {
			uri         = uri,
			diagnostics = diagnostics,
		}
		send_notification(output, METHOD_PUBLISH_DIAGNOSTICS, params, state.allocator)
	}
}

diagnostics_for_uri :: proc(
	state: ^Server_State,
	uri: string,
	allocator: mem.Allocator,
) -> []Diagnostic {
	doc, doc_ok := state.documents[uri]
	if !doc_ok {
		return nil
	}
	out := make([dynamic]Diagnostic, 0, 8, allocator)
	for bucket in state.parse_diagnostics {
		if bucket.uri != uri {
			continue
		}
		for err in bucket.errors {
			append(
				&out,
				Diagnostic {
					range = range_from_offsets(doc.text, err.range.start, err.range.end),
					severity = DIAGNOSTIC_ERROR,
					code = "syntax",
					source = "abap-lsp",
					message = err.message,
				},
			)
		}
	}
	for &slot in state.workspaces {
		if !slot.has_analysis {
			continue
		}
		if analysis := semantic.semantic_graph_session_current_analysis(&slot.analysis.session);
		   analysis != nil {
			for &result in analysis.project_results {
				if result.project == nil || result.checker == nil {
					continue
				}
				file := project_file_for_uri(&result, uri)
				if file == nil {
					continue
				}
				query := semantic.semantic_query(result.project, result.checker, file)
				semantic_diags := semantic.semantic_diagnostic_copies(
					semantic.semantic_query_diagnostics(query),
					context.temp_allocator,
				)
				for diagnostic in semantic_diags {
					item := diagnostic_to_lsp(doc.text, diagnostic)
					if !diagnostic_present(out[:], item) {
						append(&out, item)
					}
				}
			}
		}
	}
	return out[:]
}

diagnostic_to_lsp :: proc(source: string, diagnostic: semantic.Checker_Diagnostic) -> Diagnostic {
	return Diagnostic {
		range = range_from_offsets(source, diagnostic.range.start, diagnostic.range.end),
		severity = diagnostic_severity(diagnostic.severity),
		code = fmt.tprintf("%v", diagnostic.kind),
		source = "abap-lsp",
		message = diagnostic.message,
	}
}

diagnostic_present :: proc(items: []Diagnostic, item: Diagnostic) -> bool {
	for existing in items {
		if existing.range == item.range &&
		   existing.severity == item.severity &&
		   existing.code == item.code &&
		   existing.message == item.message {
			return true
		}
	}
	return false
}

diagnostic_severity :: proc "contextless" (severity: semantic.Checker_Diagnostic_Severity) -> int {
	switch severity {
	case .Error:
		return DIAGNOSTIC_ERROR
	case .Warning:
		return DIAGNOSTIC_WARNING
	case .Note:
		return DIAGNOSTIC_INFORMATION
	}
	return DIAGNOSTIC_ERROR
}
