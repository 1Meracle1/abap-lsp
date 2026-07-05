package abap_frontend_lints

import execution "src:execution"
import "src:semantic"
import trace "src:trace"

import "core:mem"
import "core:mem/virtual"

Trace_File_Timings :: struct {
	flat_facts_ms:   f64,
	control_facts_ms: f64,
}

Trace_Analysis_Counts :: struct {
	projects:                     int,
	skipped_projects:             int,
	files:                        int,
	skipped_files:                int,
	units:                        int,
	diagnostics:                  int,
	unit_diagnostics:             int,
	facts:                        int,
	value_flow_edges:             int,
	perform_calls:                int,
	find_sites:                   int,
	system_field_updates:         int,
	routine_sites:                int,
	internal_table_orders:        int,
	read_table_binary_searches:   int,
	field_symbol_state_checks:    int,
	value_state_checks:           int,
	routine_control_regions:      int,
}


Analysis :: struct {
	arena:       virtual.Arena,
	units:       [dynamic]Unit_Lints,
	diagnostics: [dynamic]Diagnostic,
}

Analysis_Task_Payload :: struct {
	analysis: ^semantic.Workspace_Analysis,
	policy:   Policy,
}

submit_analysis :: proc(
	graph: ^execution.Graph,
	exec: execution.Executor,
	analysis: ^semantic.Workspace_Analysis,
	policy: ^Policy = nil,
) -> execution.Task(Analysis) {
	payload_policy := policy^ if policy != nil else policy_default(graph.allocator)
	return execution.submit_value(
		graph,
		exec,
		Analysis_Task_Payload{analysis = analysis, policy = payload_policy},
		run_analysis_task,
	)
}

run_analysis_task :: proc(payload: Analysis_Task_Payload) -> Analysis {
	policy := payload.policy
	return run_analysis_with_policy(payload.analysis, &policy)
}

run_analysis :: proc(analysis: ^semantic.Workspace_Analysis) -> Analysis {
	policy := policy_default(context.temp_allocator)
	return run_analysis_with_policy(analysis, &policy)
}

run_analysis_with_policy :: proc(
	analysis: ^semantic.Workspace_Analysis,
	policy: ^Policy,
) -> Analysis {
	when trace.ENABLED {
		trace_start := trace.now()
		trace_counts: Trace_Analysis_Counts
		trace_collect_ms := 0.0
		trace_flat_facts_ms := 0.0
		trace_control_facts_ms := 0.0
		trace_diagnostics_ms := 0.0
	}
	result: Analysis
	arena_err := virtual.arena_init_growing(&result.arena)
	assert(arena_err == .None)
	allocator := virtual.arena_allocator(&result.arena)
	result.units = make([dynamic]Unit_Lints, 0, 16, allocator)
	result.diagnostics = make([dynamic]Diagnostic, 0, 8, allocator)
	if analysis == nil {
		when trace.ENABLED {
			trace_counts.diagnostics = len(result.diagnostics)
			trace_log_analysis(
				trace_counts,
				trace_collect_ms,
				trace_flat_facts_ms,
				trace_control_facts_ms,
				trace_diagnostics_ms,
				trace.duration_ms_since(trace_start),
			)
		}
		return result
	}
	when trace.ENABLED {
		trace_counts.projects = len(analysis.project_results)
	}
	for &project_result in analysis.project_results {
		if project_result.project == nil || project_result.checker == nil {
			when trace.ENABLED {
				trace_counts.skipped_projects += 1
			}
			continue
		}
		missing_tables_context := missing_tables_context_for_project_result(
			analysis,
			&project_result,
			allocator,
		)
		for file in project_result.files {
			if file == nil || file.root == nil {
				when trace.ENABLED {
					trace_counts.skipped_files += 1
				}
				continue
			}
			when trace.ENABLED {
				trace_counts.files += 1
			}
			unit: Unit_Lints
			when trace.ENABLED {
				trace_timings: Trace_File_Timings
				collect_start := trace.now()
				unit = collect_file(
					project_result.project,
					project_result.checker,
					file,
					allocator,
					policy,
					&missing_tables_context,
					&trace_timings,
				)
				trace_collect_ms += trace.duration_ms_since(collect_start)
				trace_flat_facts_ms += trace_timings.flat_facts_ms
				trace_control_facts_ms += trace_timings.control_facts_ms
				trace_add_unit_counts(&trace_counts, unit)
			} else {
				unit = collect_file(
					project_result.project,
					project_result.checker,
					file,
					allocator,
					policy,
					&missing_tables_context,
				)
			}
			when trace.ENABLED {
				diagnostics_start := trace.now()
			}
			for diagnostic in unit.diagnostics {
				analysis_add_diagnostic(&result, diagnostic)
			}
			when trace.ENABLED {
				trace_diagnostics_ms += trace.duration_ms_since(diagnostics_start)
			}
			append(&result.units, unit)
		}
	}
	when trace.ENABLED {
		trace_counts.diagnostics = len(result.diagnostics)
		trace_log_analysis(
			trace_counts,
			trace_collect_ms,
			trace_flat_facts_ms,
			trace_control_facts_ms,
			trace_diagnostics_ms,
			trace.duration_ms_since(trace_start),
		)
	}
	return result
}

analysis_destroy :: proc(analysis: ^Analysis) {
	if analysis == nil {
		return
	}
	virtual.arena_destroy(&analysis.arena)
	analysis^ = {}
}

collect_file :: proc(
	project: ^semantic.Project,
	checker: ^semantic.Checker,
	file: ^semantic.Project_File,
	allocator: mem.Allocator,
	policy: ^Policy = nil,
	missing_tables_context: ^Missing_Tables_Report_Context = nil,
	trace_timings: ^Trace_File_Timings = nil,
) -> Unit_Lints {
	semantic.semantic_query(project, checker, file)
	out := unit_lints_make(
		file.path if file != nil else "",
		project,
		checker,
		file,
		allocator,
	)
	if file == nil || file.root == nil {
		return out
	}
	when trace.ENABLED {
		trace_start := trace.now()
		trace_phase_start := trace.now()
		trace_flat_facts_ms := 0.0
		trace_control_facts_ms := 0.0
	}
	out.callable_summaries = routine_flow_collect_callable_summaries(&out, file.root.stmts[:], allocator)
	collect_flat_stmt_list(&out, file.root.stmts[:], allocator, policy)
	when trace.ENABLED {
		trace_flat_facts_ms = trace.duration_ms_since(trace_phase_start)
		trace_phase_start = trace.now()
	}
	collect_control_facts(&out, file.root.stmts[:], 0, policy, allocator)
	emit_routine_flow_lints(&out, project, checker, file, policy, allocator)
	emit_semantic_lint_diagnostics(&out, project, checker, file, policy, allocator)
	emit_missing_tables_declaration_lints(&out, missing_tables_context, policy, allocator)
	emit_binary_search_order_lints(&out, policy, allocator)
	apply_ast_suppressions(&out, policy, allocator)
	when trace.ENABLED {
		trace_control_facts_ms = trace.duration_ms_since(trace_phase_start)
		if trace_timings != nil {
			trace_timings.flat_facts_ms = trace_flat_facts_ms
			trace_timings.control_facts_ms = trace_control_facts_ms
		}
		trace.eprintf(
			"[trace - lints] file path=%s stmts=%d diagnostics=%d facts=%d value_flows=%d perform_calls=%d find_sites=%d system_field_updates=%d routine_sites=%d table_orders=%d binary_searches=%d field_symbol_checks=%d value_checks=%d control_regions=%d flat_facts_ms=%.3f control_facts_ms=%.3f elapsed_ms=%.3f\n",
			file.path,
			len(file.root.stmts),
			len(out.diagnostics),
			trace_unit_fact_count(out),
			len(out.value_flow_edges),
			len(out.perform_calls),
			len(out.find_sites),
			len(out.system_field_updates),
			len(out.routine_sites),
			len(out.internal_table_orders),
			len(out.read_table_binary_searches),
			len(out.field_symbol_state_checks),
			len(out.value_state_checks),
			len(out.routine_control_regions),
			trace_flat_facts_ms,
			trace_control_facts_ms,
			trace.duration_ms_since(trace_start),
		)
	}
	return out
}

unit_lints_make :: proc(
	uri: string,
	project: ^semantic.Project,
	checker: ^semantic.Checker,
	file: ^semantic.Project_File,
	allocator: mem.Allocator,
) -> Unit_Lints {
	out := Unit_Lints {
		uri = uri,
		project = project,
		checker = checker,
		file = file,
		diagnostics = make([dynamic]Diagnostic, 0, 2, allocator),
		fact_scopes = make([dynamic]Fact_Scope_Data, 0, 8, allocator),
		value_reads = make([dynamic]Value_Read_Data, 0, 8, allocator),
		value_flow_edges = make([dynamic]Value_Flow_Edge_Data, 0, 4, allocator),
		perform_calls = make([dynamic]Perform_Call_Data, 0, 1, allocator),
		find_sites = make([dynamic]Find_Site_Data, 0, 1, allocator),
		system_field_updates = make([dynamic]System_Field_Update_Data, 0, 4, allocator),
		call_function_results = make([dynamic]Call_Function_Result_Data, 0, 2, allocator),
		routine_sites = make([dynamic]Routine_Site_Data, 0, 4, allocator),
		internal_table_orders = make([dynamic]Internal_Table_Order_Data, 0, 1, allocator),
		read_table_binary_searches = make([dynamic]Read_Table_Binary_Search_Data, 0, 1, allocator),
		field_symbol_state_checks = make([dynamic]Field_Symbol_State_Check_Data, 0, 1, allocator),
		value_state_checks = make([dynamic]Value_State_Check_Data, 0, 1, allocator),
		routine_control_regions = make([dynamic]Routine_Control_Region_Data, 0, 4, allocator),
		callable_summaries = make([dynamic]Routine_Callable_Summary, 0, 4, allocator),
	}
	append(&out.fact_scopes, Fact_Scope_Data{parent = -1, range = file.root.range if file != nil && file.root != nil else {}})
	return out
}


analysis_add_diagnostic :: proc(analysis: ^Analysis, diagnostic: Diagnostic) {
	for existing in analysis.diagnostics {
		if diagnostic_same(existing, diagnostic) {
			return
		}
	}
	append(&analysis.diagnostics, diagnostic)
}

diagnostic_same :: proc(left, right: Diagnostic) -> bool {
	return left.file == right.file &&
	       left.id == right.id &&
	       left.range == right.range &&
	       left.message == right.message
}

trace_add_unit_counts :: proc(counts: ^Trace_Analysis_Counts, unit: Unit_Lints) {
	counts.units += 1
	counts.unit_diagnostics += len(unit.diagnostics)
	counts.facts += trace_unit_fact_count(unit)
	counts.value_flow_edges += len(unit.value_flow_edges)
	counts.perform_calls += len(unit.perform_calls)
	counts.find_sites += len(unit.find_sites)
	counts.system_field_updates += len(unit.system_field_updates)
	counts.routine_sites += len(unit.routine_sites)
	counts.internal_table_orders += len(unit.internal_table_orders)
	counts.read_table_binary_searches += len(unit.read_table_binary_searches)
	counts.field_symbol_state_checks += len(unit.field_symbol_state_checks)
	counts.value_state_checks += len(unit.value_state_checks)
	counts.routine_control_regions += len(unit.routine_control_regions)
}

trace_unit_fact_count :: proc(unit: Unit_Lints) -> int {
	return len(unit.value_reads) +
	       len(unit.value_flow_edges) +
	       len(unit.perform_calls) +
	       len(unit.find_sites) +
	       len(unit.system_field_updates) +
	       len(unit.routine_sites) +
	       len(unit.internal_table_orders) +
	       len(unit.read_table_binary_searches) +
	       len(unit.field_symbol_state_checks) +
	       len(unit.value_state_checks) +
	       len(unit.routine_control_regions)
}

trace_log_analysis :: proc(
	counts: Trace_Analysis_Counts,
	collect_ms: f64,
	flat_facts_ms: f64,
	control_facts_ms: f64,
	diagnostics_ms: f64,
	elapsed_ms: f64,
) {
	trace.eprintf(
		"[trace - lints] workspace analysis projects=%d skipped_projects=%d files=%d skipped_files=%d units=%d diagnostics=%d unit_diagnostics=%d facts=%d value_flows=%d perform_calls=%d find_sites=%d system_field_updates=%d routine_sites=%d table_orders=%d binary_searches=%d field_symbol_checks=%d value_checks=%d control_regions=%d collect_ms=%.3f flat_facts_ms=%.3f control_facts_ms=%.3f diagnostics_ms=%.3f elapsed_ms=%.3f\n",
		counts.projects,
		counts.skipped_projects,
		counts.files,
		counts.skipped_files,
		counts.units,
		counts.diagnostics,
		counts.unit_diagnostics,
		counts.facts,
		counts.value_flow_edges,
		counts.perform_calls,
		counts.find_sites,
		counts.system_field_updates,
		counts.routine_sites,
		counts.internal_table_orders,
		counts.read_table_binary_searches,
		counts.field_symbol_state_checks,
		counts.value_state_checks,
		counts.routine_control_regions,
		collect_ms,
		flat_facts_ms,
		control_facts_ms,
		diagnostics_ms,
		elapsed_ms,
	)
}
