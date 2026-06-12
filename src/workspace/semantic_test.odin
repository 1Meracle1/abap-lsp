package abap_frontend_workspace

import execution "src:execution"
import "src:parser"
import "src:semantic"
import string_interner "src:string_interner"

import "core:os"
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

@(test)
analyze_path_uses_standalone_sibling_abap_files :: proc(t: ^testing.T) {
	root := `tmp\workspace_standalone_sibling`
	os.remove_all(root)
	testing.expect(t, os.make_directory_all(root) == nil)
	defer os.remove_all(root)

	report_path := `tmp\workspace_standalone_sibling\zmain.report.abap`
	class_path := `tmp\workspace_standalone_sibling\zcl_repo.abap`
	testing.expect(t, os.write_entire_file(report_path, "REPORT zmain. zcl_repo=>run( ).") == nil)
	testing.expect(
		t,
		os.write_entire_file(
			class_path,
			`CLASS zcl_repo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run.
ENDCLASS.
CLASS zcl_repo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.`,
		) == nil,
	)

	opened, workspace_ok, _ := open_standalone(root, Options{}, context.allocator)
	testing.expect(t, workspace_ok)
	if !workspace_ok {
		return
	}
	defer workspace_destroy(&opened, context.allocator)

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

	result := analyze_path(&opened, report_path, {}, &pool, Options{}, context.allocator)
	defer analysis_result_destroy(&result, context.allocator)
	testing.expect(t, result.ok)
	if !result.ok {
		return
	}

	analysis := semantic.semantic_graph_session_current_analysis(&result.session)
	testing.expect(t, analysis != nil)
	if analysis == nil {
		return
	}
	testing.expect_value(t, len(result.session.editable_files), 2)
	testing.expect_value(t, workspace_test_unresolved_count(analysis, .Global_Symbol, "zcl_repo"), 0)
	testing.expect_value(t, workspace_test_unresolved_count(analysis, .Class, "zcl_repo"), 0)
}

workspace_test_unresolved_count :: proc(
	analysis: ^semantic.Workspace_Analysis,
	kind: semantic.External_Candidate_Kind,
	name: string,
) -> int {
	count := 0
	interned := semantic_name(analysis, name)
	for candidate in analysis.unresolved {
		if candidate.kind == kind && candidate.name == interned {
			count += 1
		}
	}
	return count
}

semantic_name :: proc(
	analysis: ^semantic.Workspace_Analysis,
	name: string,
) -> string_interner.String {
	return string_interner.insert(analysis.interner, name)
}
