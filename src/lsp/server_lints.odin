package abap_frontend_lsp

import execution "src:execution"
import lints "src:lints"
import "src:semantic"

import "core:mem"
import "core:mem/virtual"
import "core:os"
import "core:strings"

LINT_REFERENCE_DOCS_URL :: "https://github.com/1Meracle1/abap-lsp/blob/main/docs/reference/lints.md"

Server_Lint_Document_Snapshot :: struct {
	uri:              string,
	source:           string,
	version:          int,
	base_diagnostics: []Diagnostic,
}

Server_Lint_Run_Payload :: struct {
	analyses: []Server_Lint_Analysis_Input,
}

Server_Lint_Analysis_Input :: struct {
	analysis: ^semantic.Workspace_Analysis,
	policy:   lints.Policy,
}

Server_Lint_Run_Result :: struct {
	arena:    virtual.Arena,
	analyses: [dynamic]lints.Analysis,
}

Server_Lint_Publish_Payload :: struct {
	output:    ^os.File,
	documents: []Server_Lint_Document_Snapshot,
}

server_start_lints_async :: proc(state: ^Server_State, output: ^os.File) {
	if state == nil ||
	   output == nil ||
	   !(.Enable_Lints in state.options.flags) ||
	   state.active_lint_graph != nil ||
	   len(state.documents) == 0 {
		return
	}

	graph := execution.graph_create(&state.pool, state.allocator)
	analyses := make(
		[dynamic]Server_Lint_Analysis_Input,
		0,
		len(state.workspaces),
		graph.allocator,
	)
	for &slot in state.workspaces {
		if !slot.has_analysis {
			continue
		}
		if analysis := semantic.semantic_graph_session_current_analysis(&slot.analysis.session);
		   analysis != nil {
			append(
				&analyses,
				Server_Lint_Analysis_Input {
					analysis = analysis,
					policy = slot.analysis.lint_policy,
				},
			)
		}
	}
	if len(analyses) == 0 {
		execution.graph_destroy(graph)
		free(graph, state.allocator)
		return
	}

	documents := make(
		[dynamic]Server_Lint_Document_Snapshot,
		0,
		len(state.documents),
		graph.allocator,
	)
	for uri, doc in state.documents {
		base_diagnostics := diagnostics_for_uri(state, uri, context.temp_allocator)
		append(
			&documents,
			Server_Lint_Document_Snapshot {
				uri = strings.clone(uri, graph.allocator),
				source = strings.clone(doc.text, graph.allocator),
				version = doc.version,
				base_diagnostics = diagnostic_list_clone(base_diagnostics, graph.allocator),
			},
		)
	}

	root := execution.submit_value(
		graph,
		execution.worker_executor(&state.pool),
		Server_Lint_Run_Payload{analyses = analyses[:]},
		server_run_lints,
	)
	_ = execution.then_with(
		graph,
		root,
		execution.worker_executor(&state.pool),
		Server_Lint_Publish_Payload{output = output, documents = documents[:]},
		server_publish_lint_result,
	)
	state.active_lint_graph = graph
	execution.graph_start(graph)
}

server_finish_active_lints :: proc(state: ^Server_State) {
	if state == nil || state.active_lint_graph == nil {
		return
	}
	graph := state.active_lint_graph
	state.active_lint_graph = nil
	execution.graph_wait(graph)
	execution.graph_destroy(graph)
	free(graph, state.allocator)
}

server_run_lints :: proc(payload: Server_Lint_Run_Payload) -> Server_Lint_Run_Result {
	result: Server_Lint_Run_Result
	arena_err := virtual.arena_init_growing(&result.arena)
	assert(arena_err == .None)
	allocator := virtual.arena_allocator(&result.arena)
	result.analyses = make([dynamic]lints.Analysis, 0, len(payload.analyses), allocator)
	for input in payload.analyses {
		policy := input.policy
		append(&result.analyses, lints.run_analysis_with_policy(input.analysis, &policy))
	}
	return result
}

server_publish_lint_result :: proc(
	result: Server_Lint_Run_Result,
	payload: Server_Lint_Publish_Payload,
) -> execution.No_Result {
	lint_result := result
	defer server_lint_run_result_destroy(&lint_result)

	for document in payload.documents {
		diagnostics := make(
			[dynamic]Diagnostic,
			0,
			len(document.base_diagnostics) + 4,
			context.temp_allocator,
		)
		for diagnostic in document.base_diagnostics {
			append(&diagnostics, diagnostic)
		}
		for lint_analysis in lint_result.analyses {
			for diagnostic in lint_analysis.diagnostics {
				if !lint_diagnostic_matches_document(diagnostic, document.uri) {
					continue
				}
				item := lint_diagnostic_to_lsp(document.source, diagnostic)
				if !diagnostic_present(diagnostics[:], item) {
					append(&diagnostics, item)
				}
			}
		}
		params := Publish_Diagnostics_Params {
			uri = document.uri,
			version = document.version,
			diagnostics = diagnostics[:],
		}
		send_notification(payload.output, METHOD_PUBLISH_DIAGNOSTICS, params, context.temp_allocator)
	}
	return execution.No_Result{}
}

server_lint_run_result_destroy :: proc(result: ^Server_Lint_Run_Result) {
	if result == nil {
		return
	}
	for &analysis in result.analyses {
		lints.analysis_destroy(&analysis)
	}
	virtual.arena_destroy(&result.arena)
	result^ = {}
}

diagnostic_list_clone :: proc(diagnostics: []Diagnostic, allocator: mem.Allocator) -> []Diagnostic {
	out := make([]Diagnostic, len(diagnostics), allocator)
	for diagnostic, i in diagnostics {
		out[i] = diagnostic_clone(diagnostic, allocator)
	}
	return out
}

diagnostic_clone :: proc(diagnostic: Diagnostic, allocator: mem.Allocator) -> Diagnostic {
	out := Diagnostic {
		range = diagnostic.range,
		severity = diagnostic.severity,
		code = strings.clone(diagnostic.code, allocator) if diagnostic.code != "" else "",
		code_description = diagnostic.code_description,
		source = strings.clone(diagnostic.source, allocator) if diagnostic.source != "" else "",
		message = strings.clone(diagnostic.message, allocator) if diagnostic.message != "" else "",
		data = diagnostic.data,
	}
	if diagnostic.tags != nil {
		out.tags = make([]int, len(diagnostic.tags), allocator)
		for tag, i in diagnostic.tags {
			out.tags[i] = tag
		}
	}
	if desc, ok := diagnostic.code_description.?; ok {
		out.code_description = Diagnostic_Code_Description {
			href = strings.clone(desc.href, allocator) if desc.href != "" else "",
		}
	}
	if data, ok := diagnostic.data.?; ok {
		out.data = diagnostic_lint_data_clone(data, allocator)
	}
	return out
}

lint_diagnostic_matches_document :: proc(
	diagnostic: lints.Diagnostic,
	uri: string,
) -> bool {
	return diagnostic.file != nil && diagnostic.file.path == uri
}

lint_diagnostic_to_lsp :: proc(source: string, diagnostic: lints.Diagnostic) -> Diagnostic {
	item := Diagnostic {
		range = range_from_offsets(source, diagnostic.range.start, diagnostic.range.end),
		severity = lint_diagnostic_severity(diagnostic.severity),
		code = diagnostic.id,
		code_description = Diagnostic_Code_Description {
			href = lint_diagnostic_docs_href(diagnostic.id, context.temp_allocator),
		},
		source = "abap-lsp-lints",
		message = diagnostic.message,
		data = lint_diagnostic_data(diagnostic, context.temp_allocator),
	}
	if diagnostic.suppressed {
		item.tags = []int{DIAGNOSTIC_TAG_UNNECESSARY}
	}
	return item
}

diagnostic_lint_data_clone :: proc(data: Diagnostic_Lint_Data, allocator: mem.Allocator) -> Diagnostic_Lint_Data {
	out := Diagnostic_Lint_Data {
		kind = strings.clone(data.kind, allocator) if data.kind != "" else "",
		lint_id = strings.clone(data.lint_id, allocator) if data.lint_id != "" else "",
		level = strings.clone(data.level, allocator) if data.level != "" else "",
		group = strings.clone(data.group, allocator) if data.group != "" else "",
		origin = strings.clone(data.origin, allocator) if data.origin != "" else "",
		suppressed = data.suppressed,
	}
	if suppression, ok := data.suppression.?; ok {
		out.suppression = Diagnostic_Lint_Suppression_Data {
			kind = strings.clone(suppression.kind, allocator) if suppression.kind != "" else "",
			token = strings.clone(suppression.token, allocator) if suppression.token != "" else "",
		}
		if suppression.range != nil {
			range := make([]int, len(suppression.range), allocator)
			for value, i in suppression.range {
				range[i] = value
			}
			suppression_data, _ := out.suppression.?
			suppression_data.range = range
			out.suppression = suppression_data
		}
	}
	return out
}

lint_diagnostic_data :: proc(
	diagnostic: lints.Diagnostic,
	allocator: mem.Allocator,
) -> Diagnostic_Lint_Data {
	data := Diagnostic_Lint_Data {
		kind = "lint",
		lint_id = diagnostic.id,
		level = lints.level_string(diagnostic.level),
		group = lints.group_string(diagnostic.group),
		origin = lints.origin_string(diagnostic.origin),
		suppressed = diagnostic.suppressed,
	}
	if diagnostic.has_suppression {
		range := make([]int, 2, allocator)
		range[0] = diagnostic.suppression.range.start
		range[1] = diagnostic.suppression.range.end
		data.suppression = Diagnostic_Lint_Suppression_Data {
			kind = lints.suppression_kind_string(diagnostic.suppression.kind),
			token = diagnostic.suppression.token,
			range = range,
		}
	}
	return data
}

lint_diagnostic_docs_href :: proc(id: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, LINT_REFERENCE_DOCS_URL)
	strings.write_byte(&out, '#')
	strings.write_string(&out, lints.docs_anchor(id, context.temp_allocator))
	return strings.to_string(out)
}

lint_diagnostic_severity :: proc "contextless" (severity: lints.Diagnostic_Severity) -> int {
	switch severity {
	case .Error:
		return DIAGNOSTIC_ERROR
	case .Warning:
		return DIAGNOSTIC_WARNING
	case .Information:
		return DIAGNOSTIC_INFORMATION
	case .Hint:
		return DIAGNOSTIC_HINT
	}
	return DIAGNOSTIC_WARNING
}
