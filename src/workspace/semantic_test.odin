package abap_frontend_workspace

import execution "src:execution"
import "src:parser"
import "src:semantic"

import "core:testing"

@(test)
workspace_dependency_diagnostics_report_each_unresolved_external_use :: proc(t: ^testing.T) {
	source := `REPORT zmain.
DATA(result) = zcl_remote=>get_instance( )->first( ).
zcl_remote=>get_instance( )->second( ).`
	parsed := parser.parse(source, "mem://zmain.report.abap", context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)
	files := [?]semantic.Workspace_File_Input {
		{
			path = "mem://zmain.report.abap",
			root = parsed.root,
			kind = .Unknown,
		},
	}
	opened := Workspace {
		root_path = "mem://",
		flags = Option_Flags{.Enable_Dependency_Diagnostics},
	}
	pool: execution.Pool
	execution.pool_init(
		&pool,
		execution.Options {
			worker_count = 0,
			task_capacity = 8,
			queue_capacity = 8,
			deque_capacity = 8,
			edge_capacity = 8,
		},
		context.allocator,
	)
	defer execution.pool_destroy(&pool)

	result := analyze_inputs(&opened, files[:], &pool, context.allocator)
	defer analysis_result_destroy(&result, context.allocator)
	analysis := semantic.semantic_graph_session_current_analysis(&result.session)
	testing.expect(t, analysis != nil)
	if analysis == nil || len(analysis.project_results) == 0 {
		return
	}
	checker := analysis.project_results[0].checker
	testing.expect(t, checker != nil)
	if checker == nil {
		return
	}

	count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind == .Unresolved_Type &&
		   diagnostic.range.start < diagnostic.range.end &&
		   source[diagnostic.range.start:diagnostic.range.end] == "zcl_remote" {
			count += 1
		}
	}
	testing.expect_value(t, count, 2)
}

@(test)
workspace_dependency_diagnostics_skip_generic_value_misses :: proc(t: ^testing.T) {
	source := `REPORT zmain.
unknown_value = 1.`
	parsed := parser.parse(source, "mem://zmain.report.abap", context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)
	files := [?]semantic.Workspace_File_Input {
		{
			path = "mem://zmain.report.abap",
			root = parsed.root,
			kind = .Unknown,
		},
	}
	opened := Workspace {
		root_path = "mem://",
		flags = Option_Flags{.Enable_Dependency_Diagnostics},
	}
	pool: execution.Pool
	execution.pool_init(
		&pool,
		execution.Options {
			worker_count = 0,
			task_capacity = 8,
			queue_capacity = 8,
			deque_capacity = 8,
			edge_capacity = 8,
		},
		context.allocator,
	)
	defer execution.pool_destroy(&pool)

	result := analyze_inputs(&opened, files[:], &pool, context.allocator)
	defer analysis_result_destroy(&result, context.allocator)
	analysis := semantic.semantic_graph_session_current_analysis(&result.session)
	testing.expect(t, analysis != nil)
	if analysis == nil || len(analysis.project_results) == 0 {
		return
	}
	checker := analysis.project_results[0].checker
	testing.expect(t, checker != nil)
	if checker == nil {
		return
	}

	for diagnostic in checker.info.diagnostics {
		testing.expect(t, diagnostic.kind != .Unresolved_Reference)
	}
}
