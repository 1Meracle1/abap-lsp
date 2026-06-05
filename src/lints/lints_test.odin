package abap_frontend_lints

import execution "src:execution"
import analyze "src:semantic/analyze"

import "core:testing"

@(test)
collects_read_table_and_state_lint_facts :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA ls_row TYPE string.
  FIELD-SYMBOLS <row> TYPE string.

  SORT lt_rows BY table_line.
  READ TABLE lt_rows INTO ls_row WITH KEY table_line = 'x' BINARY SEARCH.
  IF <row> IS ASSIGNED AND ls_row IS NOT INITIAL AND ls_row = 0.
  ENDIF.
ENDFORM.
`
	unit := collect_source("mem://lint.abap", source, context.allocator)

	testing.expect_value(t, len(unit.internal_table_orders), 1)
	testing.expect_value(t, len(unit.read_table_binary_searches), 1)
	testing.expect_value(t, len(unit.field_symbol_state_checks), 1)
	testing.expect(t, len(unit.value_state_checks) >= 2)
}

@(test)
project_lints_run_as_async_diagnostic_pass :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 16}, context.allocator)
	defer execution.pool_destroy(&pool)

	source := `FORM run.
  AT FIRST.
  ENDAT.
ENDFORM.`
	unit := analyze.source_file_provider_make(
		0,
		.Full_Source,
		"mem://at.abap",
		{0, len(source)},
		context.allocator,
	)
	project := analyze.Project_Analysis {
		providers = analyze.Project_Provider_Store {
			source_files = make([dynamic]analyze.Source_File_Provider, 0, 1, context.allocator),
			summaries = make([]analyze.Summary_Provider_Input, 0, context.allocator),
		},
		diagnostics = make([dynamic]analyze.Diagnostic, 0, 1, context.allocator),
	}
	append(&project.providers.source_files, unit)

	sources := [?]analyze.Source_Input{{uri = unit.uri, source = source}}
	run_project_async(&project, sources[:], &pool, context.allocator)

	testing.expect_value(t, len(project.providers.source_files[0].diagnostics), 1)
	testing.expect_value(t, project.providers.source_files[0].diagnostics[0].kind, analyze.Diagnostic_Kind.Invalid_Control_Break)
}
