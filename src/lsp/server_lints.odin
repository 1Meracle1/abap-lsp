package abap_frontend_lsp

import execution "src:execution"
import lints "src:lints"
import "src:semantic"
import trace "src:trace"

import "core:mem"
import "core:mem/virtual"
import "core:os"
import "core:strings"
import "core:sync"

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
	state:     ^Server_State,
	generation: u64,
	documents: []Server_Lint_Document_Snapshot,
}

server_start_lints_async :: proc(state: ^Server_State, output: ^os.File) -> bool {
	when trace.ENABLED {
		trace_start := trace.now()
	}
	if state != nil {
		state.last_reanalysis_stats.lint_start_attempts += 1
	}
	if state == nil ||
	   output == nil ||
	   !(.Enable_Lints in state.options.flags) ||
	   state.active_lint_graph != nil ||
	   len(state.documents) == 0 {
		if state != nil && state.active_lint_graph != nil {
			state.last_reanalysis_stats.lint_start_skipped_active += 1
		}
		when trace.ENABLED {
			trace.eprintf(
				"[trace - lsp] lint start skipped active=%v documents=%d elapsed_ms=%.3f\n",
				state != nil && state.active_lint_graph != nil,
				len(state.documents) if state != nil else 0,
				trace.duration_ms_since(trace_start),
			)
		}
		return false
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
		when trace.ENABLED {
			trace.eprintf(
				"[trace - lsp] lint start skipped analyses=0 documents=%d elapsed_ms=%.3f\n",
				len(state.documents),
				trace.duration_ms_since(trace_start),
			)
		}
		return false
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
		Server_Lint_Publish_Payload {
			output     = output,
			state      = state,
			generation = sync.atomic_load_explicit(&state.diagnostic_generation, .Acquire),
			documents  = documents[:],
		},
		server_publish_lint_result,
	)
	state.active_lint_graph = graph
	state.last_reanalysis_stats.lint_started += 1
	execution.graph_start(graph)
	when trace.ENABLED {
		trace.eprintf(
			"[trace - lsp] lint start generation=%d analyses=%d documents=%d elapsed_ms=%.3f\n",
			sync.atomic_load_explicit(&state.diagnostic_generation, .Acquire),
			len(analyses),
			len(documents),
			trace.duration_ms_since(trace_start),
		)
	}
	return true
}

server_finish_active_lints :: proc(state: ^Server_State, wait: bool = true) -> bool {
	if state == nil || state.active_lint_graph == nil {
		return false
	}
	graph := state.active_lint_graph
	if !wait && !execution.graph_completed(graph) {
		return false
	}
	when trace.ENABLED {
		trace_start := trace.now()
	}
	state.active_lint_graph = nil
	if wait {
		execution.graph_wait(graph)
	}
	execution.graph_destroy(graph)
	free(graph, state.allocator)
	for &slot in state.workspaces {
		if slot.has_analysis {
			semantic.semantic_graph_session_clear_retired_analyses(&slot.analysis.session)
		}
	}
	when trace.ENABLED {
		trace.eprintf(
			"[trace - lsp] lint finish waited=%v elapsed_ms=%.3f\n",
			wait,
			trace.duration_ms_since(trace_start),
		)
	}
	return true
}

server_run_lints :: proc(payload: Server_Lint_Run_Payload) -> Server_Lint_Run_Result {
	when trace.ENABLED {
		trace_start := trace.now()
	}
	result: Server_Lint_Run_Result
	arena_err := virtual.arena_init_growing(&result.arena)
	assert(arena_err == .None)
	allocator := virtual.arena_allocator(&result.arena)
	result.analyses = make([dynamic]lints.Analysis, 0, len(payload.analyses), allocator)
	for input in payload.analyses {
		policy := input.policy
		append(&result.analyses, lints.run_analysis_with_policy(input.analysis, &policy))
	}
	when trace.ENABLED {
		trace.eprintf(
			"[trace - lsp] lint run analyses=%d elapsed_ms=%.3f\n",
			len(payload.analyses),
			trace.duration_ms_since(trace_start),
		)
	}
	return result
}

server_publish_lint_result :: proc(
	result: Server_Lint_Run_Result,
	payload: Server_Lint_Publish_Payload,
) -> execution.No_Result {
	when trace.ENABLED {
		trace_start := trace.now()
	}
	lint_result := result
	defer server_lint_run_result_destroy(&lint_result)

	if payload.state != nil {
		current_generation := sync.atomic_load_explicit(
			&payload.state.diagnostic_generation,
			.Acquire,
		)
		if current_generation != payload.generation {
			stale_before := sync.atomic_add_explicit(
				&payload.state.stale_lint_publish_count,
				u64(1),
				.Acq_Rel,
			)
			_ = stale_before
			when trace.ENABLED {
				trace.eprintf(
					"[trace - lsp] lint publish skipped stale generation=%d current=%d stale_count=%d elapsed_ms=%.3f\n",
					payload.generation,
					current_generation,
					stale_before + 1,
					trace.duration_ms_since(trace_start),
				)
			}
			return execution.No_Result{}
		}
	}

	published_documents := 0
	published_diagnostics := 0
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
		published_documents += 1
		published_diagnostics += len(diagnostics)
		send_notification(payload.output, METHOD_PUBLISH_DIAGNOSTICS, params, context.temp_allocator)
	}
	when trace.ENABLED {
		trace.eprintf(
			"[trace - lsp] lint publish generation=%d documents=%d diagnostics=%d elapsed_ms=%.3f\n",
			payload.generation,
			published_documents,
			published_diagnostics,
			trace.duration_ms_since(trace_start),
		)
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
