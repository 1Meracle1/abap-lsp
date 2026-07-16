package abap_frontend_lints

import execution "src:execution"
import "src:ast"
import "src:parser"
import "src:semantic"

import "core:strings"
import "core:testing"

test_analysis :: proc(source, uri: string) -> semantic.Workspace_Analysis {
	parsed := parser.parse(source, uri, context.allocator)
	files := [?]semantic.Workspace_File_Input {
		{
			path = uri,
			root = parsed.root,
			has_syntax_errors = len(parsed.errors) > 0,
		},
	}
	return semantic.semantic_workspace_analyze(semantic.Workspace_Input{files = files[:]})
}

test_file_input :: proc(
	t: ^testing.T,
	path: string,
	source: string,
	kind: semantic.Workspace_File_Kind = .Unknown,
	object_name: string = "",
) -> semantic.Workspace_File_Input {
	parsed := parser.parse(source, path, context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)
	return semantic.Workspace_File_Input {
		path        = path,
		root        = parsed.root,
		kind        = kind,
		object_name = object_name,
	}
}

test_external_interface_input :: proc(
	t: ^testing.T,
	kind: semantic.External_Candidate_Kind,
	role: semantic.External_Interface_Object_Role,
	name: string,
	path: string,
	source: string,
) -> semantic.External_Interface_Input {
	parsed := parser.parse(source, path, context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)
	return semantic.External_Interface_Input {
		key = semantic.Semantic_Object_Key {
			kind = kind,
			name = name,
		},
		path = path,
		root = parsed.root,
		role = role,
	}
}

test_has_diagnostic :: proc(analysis: Analysis, id: string) -> bool {
	for diagnostic in analysis.diagnostics {
		if diagnostic.id == id {
			return true
		}
	}
	return false
}

test_diagnostic_count :: proc(analysis: Analysis, id: string) -> int {
	count := 0
	for diagnostic in analysis.diagnostics {
		if diagnostic.id == id {
			count += 1
		}
	}
	return count
}

test_diagnostic_count_at_or_after :: proc(analysis: Analysis, id: string, start: int) -> int {
	count := 0
	for diagnostic in analysis.diagnostics {
		if diagnostic.id == id && diagnostic.range.start >= start {
			count += 1
		}
	}
	return count
}

test_policy_with_report_suppressed :: proc() -> Policy {
	config := config_default(context.allocator)
	config.report_suppressed = true
	return policy_from_config(&config, context.allocator)
}

@(test)
registry_contains_native_lints_with_unique_ids :: proc(t: ^testing.T) {
	testing.expect_value(t, len(REGISTRY), 18)
	for metadata, i in REGISTRY {
		testing.expect(t, metadata.id != "")
		testing.expect(t, metadata.summary != "")
		for other, j in REGISTRY {
			if j > i {
				testing.expect(t, metadata.id != other.id)
			}
		}
	}
	if metadata, ok := metadata_for(EPC_UNVERIFIED_OPEN_SQL_SOURCE); ok {
		policy := policy_default(context.allocator)
		testing.expect_value(t, policy_level_for(&policy, metadata.id), Level.Deny)
	}
}

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
	analysis := test_analysis(source, "mem://lint.abap")
	defer semantic.semantic_workspace_analysis_destroy(&analysis)

	testing.expect(t, len(analysis.project_results) > 0)
	project_result := &analysis.project_results[0]
	testing.expect(t, len(project_result.files) > 0)
	unit := collect_file(
		project_result.project,
		project_result.checker,
		project_result.files[0],
		context.allocator,
	)

	testing.expect_value(t, len(unit.internal_table_orders), 1)
	testing.expect_value(t, len(unit.read_table_binary_searches), 1)
	testing.expect_value(t, len(unit.field_symbol_state_checks), 1)
	testing.expect(t, len(unit.value_state_checks) >= 2)
}

@(test)
emits_local_sql_and_result_lints :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lt_keys TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  LOOP AT lt_keys INTO DATA(lv_key).
    SELECT * FROM (lv_table) INTO TABLE @lt_rows WHERE (lv_where).
  ENDLOOP.
  SELECT matnr FROM mara INTO TABLE @lt_rows FOR ALL ENTRIES IN @lt_keys WHERE matnr = @lt_keys-table_line.
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD 'SE38'.
  CALL FUNCTION 'Z_DEMO'
    EXCEPTIONS
      failed = 1.
  READ TABLE lt_rows INTO DATA(lv_row) INDEX 1.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://sql_lints.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, test_has_diagnostic(analysis_result, SELECT_STAR))
	testing.expect(t, test_has_diagnostic(analysis_result, SELECT_IN_LOOP))
	testing.expect(t, test_has_diagnostic(analysis_result, DYNAMIC_OPEN_SQL))
	testing.expect(t, test_has_diagnostic(analysis_result, FOR_ALL_ENTRIES_WITHOUT_GUARD))
	testing.expect(t, test_has_diagnostic(analysis_result, FOR_ALL_ENTRIES_CAN_USE_IN))
	testing.expect(t, test_has_diagnostic(analysis_result, IGNORED_AUTHORITY_CHECK))
	testing.expect(t, test_has_diagnostic(analysis_result, IGNORED_CALL_FUNCTION_RESULT))
}

@(test)
select_in_loop_mentions_enclosing_loop_kind :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lv_count TYPE i.
  WHILE lv_count > 0.
    SELECT SINGLE carrid FROM scarr INTO @DATA(lv_carrid).
  ENDWHILE.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://select_in_while.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	found := false
	for diagnostic in analysis_result.diagnostics {
		if diagnostic.id == SELECT_IN_LOOP {
			testing.expect_value(
				t,
				diagnostic.message,
				"Open SQL SELECT runs inside a WHILE body; prefer bulk selection before the loop",
			)
			found = true
		}
	}
	testing.expect(t, found)
}

@(test)
select_into_positional_target_lints_report_name_and_length_mismatch :: proc(t: ^testing.T) {
	source := `
FORM run.
  TYPES ty_long TYPE c LENGTH 10.
  TYPES: BEGIN OF zsrc,
           id TYPE ty_long,
           name TYPE c LENGTH 5,
         END OF zsrc.
  TYPES: BEGIN OF ty_row,
           id TYPE zsrc-name,
           title TYPE c LENGTH 5,
         END OF ty_row.
  DATA lt_rows TYPE TABLE OF ty_row WITH EMPTY KEY.

  SELECT id, name
    FROM zsrc
    INTO TABLE @lt_rows.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://select_into_target_lints.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, SELECT_INTO_FIELD_NAME_MISMATCH), 1)
	testing.expect_value(t, test_diagnostic_count(analysis_result, SELECT_INTO_FIELD_LENGTH_NARROWING), 1)
	name_message_seen := false
	length_message_seen := false
	for diagnostic in analysis_result.diagnostics {
		if diagnostic.id == SELECT_INTO_FIELD_NAME_MISMATCH {
			name_message_seen = true
			testing.expect_value(
				t,
				diagnostic.message,
				"Open SQL SELECT field 'name' is assigned by position to target field 'title'",
			)
		}
		if diagnostic.id == SELECT_INTO_FIELD_LENGTH_NARROWING {
			length_message_seen = true
			testing.expect_value(
				t,
				diagnostic.message,
				"Open SQL SELECT field 'id' has backing length 10, but target field 'id' has length 5",
			)
		}
	}
	testing.expect(t, name_message_seen)
	testing.expect(t, length_message_seen)
}

@(test)
call_function_result_lint_stays_conservative_without_evidence :: proc(t: ^testing.T) {
	no_proof_source := `
FORM run.
  CALL FUNCTION 'Z_DEMO'
    EXCEPTIONS
      failed = 1.
  WRITE 'ok'.
ENDFORM.
`
	no_proof_semantic := test_analysis(no_proof_source, "mem://call_function_no_proof.abap")
	defer semantic.semantic_workspace_analysis_destroy(&no_proof_semantic)
	no_proof_lints := run_analysis(&no_proof_semantic)
	defer analysis_destroy(&no_proof_lints)
	testing.expect(t, !test_has_diagnostic(no_proof_lints, IGNORED_CALL_FUNCTION_RESULT))

	output_read_source := `
FORM run.
  DATA lv_result TYPE string.
  DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  CALL FUNCTION 'Z_DEMO'
    IMPORTING
      ev_result = lv_result.
  WRITE lv_result.
  READ TABLE lt_rows INTO DATA(lv_row) INDEX 1.
ENDFORM.
`
	output_read_semantic := test_analysis(output_read_source, "mem://call_function_output_read.abap")
	defer semantic.semantic_workspace_analysis_destroy(&output_read_semantic)
	output_read_lints := run_analysis(&output_read_semantic)
	defer analysis_destroy(&output_read_lints)
	testing.expect(t, !test_has_diagnostic(output_read_lints, IGNORED_CALL_FUNCTION_RESULT))

	output_ignored_source := `
FORM run.
  DATA lv_result TYPE string.
  CALL FUNCTION 'Z_DEMO'
    IMPORTING
      ev_result = lv_result.
ENDFORM.
`
	output_ignored_semantic := test_analysis(output_ignored_source, "mem://call_function_output_ignored.abap")
	defer semantic.semantic_workspace_analysis_destroy(&output_ignored_semantic)
	output_ignored_lints := run_analysis(&output_ignored_semantic)
	defer analysis_destroy(&output_ignored_lints)
	testing.expect(t, test_has_diagnostic(output_ignored_lints, IGNORED_CALL_FUNCTION_RESULT))
}

@(test)
call_function_output_result_global_target_is_potentially_handled :: proc(t: ^testing.T) {
	source := `
DATA gv_result TYPE string.

FORM run.
  CALL FUNCTION 'Z_DEMO'
    IMPORTING
      ev_result = gv_result.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://call_function_global_output.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, IGNORED_CALL_FUNCTION_RESULT))
}

@(test)
for_all_entries_accepts_lines_guards :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lt_keys TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.

  IF lines( lt_keys ) > 0.
    SELECT matnr FROM mara INTO TABLE @lt_rows FOR ALL ENTRIES IN @lt_keys WHERE matnr = @lt_keys-table_line.
  ENDIF.

  IF lines( lt_keys ) = 0.
    RETURN.
  ENDIF.
  SELECT matnr FROM mara INTO TABLE @lt_rows FOR ALL ENTRIES IN @lt_keys WHERE matnr = @lt_keys-table_line.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://fae_lines_guard.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, FOR_ALL_ENTRIES_WITHOUT_GUARD))
}

@(test)
for_all_entries_single_field_suggests_range_in :: proc(t: ^testing.T) {
	source := `
TYPES: BEGIN OF zrow,
         matnr TYPE string,
         spras TYPE string,
       END OF zrow.
DATA lt_keys TYPE STANDARD TABLE OF zrow WITH EMPTY KEY.
DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.

IF lt_keys IS NOT INITIAL.
  SELECT matnr
    FROM zrow
    INTO TABLE @lt_rows
    FOR ALL ENTRIES IN @lt_keys
    WHERE matnr = @lt_keys-matnr
      AND spras = @sy-langu.
ENDIF.
`
	semantic_analysis := test_analysis(source, "mem://fae_single_field_in.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	found := false
	for diagnostic in analysis_result.diagnostics {
		if diagnostic.id != FOR_ALL_ENTRIES_CAN_USE_IN {
			continue
		}
		found = true
		testing.expect_value(
			t,
			source[diagnostic.range.start:diagnostic.range.end],
			"FOR ALL ENTRIES IN @lt_keys",
		)
		testing.expect_value(
			t,
			diagnostic.message,
			"FOR ALL ENTRIES on 'lt_keys' only compares SQL field 'matnr' with table field 'matnr'; prefer a range table with Open SQL IN",
		)
	}
	testing.expect(t, found)
	testing.expect(t, !test_has_diagnostic(analysis_result, FOR_ALL_ENTRIES_WITHOUT_GUARD))
}

@(test)
for_all_entries_in_replacement_stays_conservative :: proc(t: ^testing.T) {
	source := `
TYPES: BEGIN OF zrow,
         matnr TYPE string,
         spras TYPE string,
       END OF zrow.
DATA lt_keys TYPE STANDARD TABLE OF zrow WITH EMPTY KEY.
DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.

IF lt_keys IS NOT INITIAL.
  SELECT matnr
    FROM zrow
    INTO TABLE @lt_rows
    FOR ALL ENTRIES IN @lt_keys
    WHERE matnr = @lt_keys-matnr
      AND spras = @lt_keys-spras.

  SELECT matnr
    FROM zrow
    INTO TABLE @lt_rows
    FOR ALL ENTRIES IN @lt_keys
    WHERE matnr LIKE @lt_keys-matnr.

  SELECT matnr
    FROM zrow
    INTO TABLE @lt_rows
    FOR ALL ENTRIES IN @lt_keys
    WHERE matnr = @lt_keys-matnr
       OR spras = @lt_keys-matnr.
ENDIF.
`
	semantic_analysis := test_analysis(source, "mem://fae_in_conservative.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, FOR_ALL_ENTRIES_CAN_USE_IN))
	testing.expect(t, !test_has_diagnostic(analysis_result, FOR_ALL_ENTRIES_WITHOUT_GUARD))
}

@(test)
select_single_uses_preserved_ddic_key_metadata :: proc(t: ^testing.T) {
	source := `
TYPES: BEGIN OF zflight,
         mandt TYPE string,
         carrid TYPE string,
         connid TYPE string,
       END OF zflight. " key fields: mandt, carrid, connid

FORM run.
  DATA lv_connid TYPE string.
  SELECT SINGLE carrid FROM zflight INTO @DATA(lv_carrid) WHERE carrid = @lv_carrid.
  SELECT SINGLE carrid FROM zflight INTO @DATA(lv_full) WHERE carrid = @lv_carrid AND connid = @lv_connid.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://select_single_key.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	project_result := &semantic_analysis.project_results[0]
	source_file := project_result.files[0]
	zflight, zflight_ok := semantic.scope_lookup_declaration(source_file.root_scope, .Type, "zflight")
	testing.expect(t, zflight_ok)
	structure := semantic.checker_type_structure(zflight.type)
	testing.expect(t, structure != nil)
	key_count := 0
	if structure != nil {
		for field in structure.fields {
			if field == nil {
				continue
			}
			payload, payload_ok := field.payload.(^semantic.Entity_Field_Payload)
			if payload_ok && payload != nil && .Is_Key in payload.flags {
				key_count += 1
			}
		}
	}
	testing.expect_value(t, key_count, 3)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	count := 0
	for diagnostic in analysis_result.diagnostics {
		if diagnostic.id == SELECT_SINGLE_WITHOUT_FULL_KEY {
			count += 1
		}
	}
	testing.expect_value(t, count, 1)
}

@(test)
missing_tables_declaration_flags_ddic_selection_screen_report_types :: proc(t: ^testing.T) {
	ddic_source := `TYPES: BEGIN OF ztab,
         field TYPE string,
       END OF ztab.`
	files := [?]semantic.Workspace_File_Input {
		test_file_input(
			t,
			"mem://zmain.report.abap",
			`REPORT zmain.
PARAMETERS p_ztab TYPE ztab.
SELECT-OPTIONS so_field FOR ztab-field.`,
		),
	}
	external_inputs := [?]semantic.External_Interface_Input {
		test_external_interface_input(
			t,
			.DDIC_Table,
			.DDIC_Table,
			"ztab",
			"adt://ztab.ddic.abap",
			ddic_source,
		),
	}
	semantic_analysis := semantic.semantic_workspace_analyze(
		semantic.Workspace_Input{files = files[:], external_interfaces = external_inputs[:]},
	)
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, EPC_MISSING_TABLES_DECLARATION), 2)
}

@(test)
missing_tables_declaration_accepts_tables_in_report_include_closure :: proc(t: ^testing.T) {
	ddic_source := `TYPES: BEGIN OF ztab,
         field TYPE string,
       END OF ztab.`
	files := [?]semantic.Workspace_File_Input {
		test_file_input(
			t,
			"mem://zmain.report.abap",
			`REPORT zmain.
INCLUDE ztop.
INCLUDE zf01.`,
		),
		test_file_input(t, "mem://ztop.include.abap", "TABLES ztab."),
		test_file_input(t, "mem://zf01.include.abap", "PARAMETERS p_ztab TYPE ztab."),
	}
	external_inputs := [?]semantic.External_Interface_Input {
		test_external_interface_input(
			t,
			.DDIC_Table,
			.DDIC_Table,
			"ztab",
			"adt://ztab.ddic.abap",
			ddic_source,
		),
	}
	semantic_analysis := semantic.semantic_workspace_analyze(
		semantic.Workspace_Input{files = files[:], external_interfaces = external_inputs[:]},
	)
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, EPC_MISSING_TABLES_DECLARATION))
}

@(test)
missing_tables_declaration_ignores_non_selection_screen_type_refs :: proc(t: ^testing.T) {
	ddic_source := `TYPES: BEGIN OF ztab,
         field TYPE string,
       END OF ztab.`
	files := [?]semantic.Workspace_File_Input {
		test_file_input(
			t,
			"mem://zmain.report.abap",
			`REPORT zmain.
DATA gs_ztab TYPE ztab.`,
		),
	}
	external_inputs := [?]semantic.External_Interface_Input {
		test_external_interface_input(
			t,
			.DDIC_Table,
			.DDIC_Table,
			"ztab",
			"adt://ztab.ddic.abap",
			ddic_source,
		),
	}
	semantic_analysis := semantic.semantic_workspace_analyze(
		semantic.Workspace_Input{files = files[:], external_interfaces = external_inputs[:]},
	)
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, EPC_MISSING_TABLES_DECLARATION))
}

@(test)
routine_flow_flags_conditional_inline_read_table_target :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lt_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
  READ TABLE lt_values INTO DATA(lv_value) INDEX 1.
  DATA lv_copy TYPE i.
  lv_copy = lv_value.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://read_table_inline_unassigned.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT), 1)
}

@(test)
routine_flow_refines_conditional_inline_read_table_target_with_subrc_guard :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lt_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
  READ TABLE lt_values INTO DATA(lv_value) INDEX 1.
  IF sy-subrc = 0.
    DATA lv_copy TYPE i.
    lv_copy = lv_value.
  ENDIF.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://read_table_inline_guarded.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT))
}

@(test)
routine_flow_treats_constructor_for_in_iterator_as_assigned :: proc(t: ^testing.T) {
	source := `
FORM process_reload.
  TYPES: BEGIN OF ty_job,
           job_status TYPE c LENGTH 1,
           jobname TYPE string,
         END OF ty_job.
  TYPES tt_jobs TYPE STANDARD TABLE OF ty_job WITH EMPTY KEY.
  TYPES: BEGIN OF ty_range,
           sign TYPE c LENGTH 1,
           option TYPE c LENGTH 2,
           low TYPE string,
           high TYPE string,
         END OF ty_range.
  TYPES tt_range TYPE STANDARD TABLE OF ty_range WITH EMPTY KEY.
  CONSTANTS lc_status_a TYPE c LENGTH 1 VALUE 'A'.
  CONSTANTS lc_sign_i TYPE c LENGTH 1 VALUE 'I'.
  CONSTANTS lc_opt_eq TYPE c LENGTH 2 VALUE 'EQ'.
  DATA lt_rel_data TYPE tt_jobs.
  DATA lr_jobname TYPE tt_range.

  lr_jobname = VALUE #(
    FOR ls_jobs IN lt_rel_data
    WHERE ( job_status = lc_status_a )
    ( sign = lc_sign_i
      option = lc_opt_eq
      low = ls_jobs-jobname
      high = '' ) ).
ENDFORM.
`
	files := [?]semantic.Workspace_File_Input {
		test_file_input(t, "mem://value_for_iterator_assignment.abap", source),
	}
	semantic_analysis := semantic.semantic_workspace_analyze(semantic.Workspace_Input{files = files[:]})
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	for diagnostic in analysis_result.diagnostics {
		testing.expect(
			t,
			diagnostic.id != USE_BEFORE_DEFINITE_ASSIGNMENT ||
			!strings.contains(diagnostic.message, "ls_jobs"),
		)
	}
}

@(test)
routine_flow_marks_constructor_for_in_iterator_assigned_in_read_state :: proc(t: ^testing.T) {
	source := `
TYPES: BEGIN OF ty_job,
         job_status TYPE c LENGTH 1,
         jobname TYPE string,
       END OF ty_job.
TYPES tt_jobs TYPE STANDARD TABLE OF ty_job WITH EMPTY KEY.
TYPES: BEGIN OF ty_range,
         low TYPE string,
       END OF ty_range.
TYPES tt_range TYPE STANDARD TABLE OF ty_range WITH EMPTY KEY.
DATA lt_rel_data TYPE tt_jobs.
DATA lr_jobname TYPE tt_range.

lr_jobname = VALUE #(
  FOR ls_jobs IN lt_rel_data
  WHERE ( job_status = 'A' )
  ( low = ls_jobs-jobname ) ).
`
	files := [?]semantic.Workspace_File_Input {
		test_file_input(t, "mem://constructor_for_iterator_state.abap", source),
	}
	semantic_analysis := semantic.semantic_workspace_analyze(semantic.Workspace_Input{files = files[:]})
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	testing.expect_value(t, len(semantic_analysis.project_results), 1)
	project_result := &semantic_analysis.project_results[0]
	testing.expect_value(t, len(project_result.files), 1)
	file := project_result.files[0]
	assign := file.root.stmts[len(file.root.stmts) - 1].derived_stmt.(^ast.Assign_Stmt)
	constructor := assign.rhs.derived_expr.(^ast.Constructor_Expr)
	for_clause := constructor.args[0].derived_expr.(^ast.Constructor_For_Clause_Expr)

	query := semantic.semantic_query(project_result.project, project_result.checker, file)
	policy := policy_default(context.allocator)
	out := unit_lints_make(file.path, project_result.project, project_result.checker, file, context.allocator)
	ctx := Routine_Flow_Context {
		out = &out,
		ref_query = semantic.semantic_query_refs(query),
		decl_query = semantic.semantic_query_decls(query),
		policy = &policy,
		allocator = context.allocator,
		tracked_values = make([dynamic]^semantic.Entity, 0, 4, context.allocator),
		dead_store_untracked_values = make([dynamic]^semantic.Entity, 0, 4, context.allocator),
	}
	state := routine_flow_state_make(context.allocator)

	routine_flow_read_expr(&ctx, &state, &for_clause.node)

	iterator := semantic.semantic_decl_entity_with_kind_and_decl_range(
		ctx.decl_query,
		.Variable,
		for_clause.variable.range,
	)
	testing.expect(t, routine_flow_entity_list_contains(state.assigned[:], iterator))
}

@(test)
routine_flow_keeps_select_single_into_target_conditional :: proc(t: ^testing.T) {
	source := `
FORM no_read.
  TYPES: BEGIN OF ty_row,
           carrid TYPE string,
         END OF ty_row.
  DATA ls_row TYPE ty_row.
  SELECT SINGLE carrid
    FROM scarr
    INTO ls_row.
ENDFORM.

FORM unguarded_read.
  TYPES: BEGIN OF ty_row,
           carrid TYPE string,
         END OF ty_row.
  DATA ls_row TYPE ty_row.
  SELECT SINGLE carrid
    FROM scarr
    INTO ls_row.
  DATA lv_copy TYPE string.
  lv_copy = ls_row-carrid.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://select_single_conditional_target.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT), 1)
}

@(test)
routine_flow_refines_select_single_target_with_sy_subrc_success_guards :: proc(t: ^testing.T) {
	source := `
FORM equals_zero.
  TYPES: BEGIN OF ty_row,
           carrid TYPE string,
         END OF ty_row.
  DATA ls_row TYPE ty_row.
  SELECT SINGLE carrid
    FROM scarr
    INTO ls_row.
  IF sy-subrc = 0.
    DATA lv_copy TYPE string.
    lv_copy = ls_row-carrid.
  ENDIF.
ENDFORM.

FORM is_initial.
  TYPES: BEGIN OF ty_row,
           carrid TYPE string,
         END OF ty_row.
  DATA ls_row TYPE ty_row.
  SELECT SINGLE carrid
    FROM scarr
    INTO ls_row.
  IF sy-subrc IS INITIAL.
    DATA lv_copy TYPE string.
    lv_copy = ls_row-carrid.
  ENDIF.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://select_single_subrc_success_guards.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT))
}

@(test)
routine_flow_does_not_refine_select_single_target_with_failure_or_irrelevant_guards :: proc(t: ^testing.T) {
	source := `
FORM subrc_not_zero.
  TYPES: BEGIN OF ty_row,
           carrid TYPE string,
         END OF ty_row.
  DATA ls_row TYPE ty_row.
  SELECT SINGLE carrid
    FROM scarr
    INTO ls_row.
  IF sy-subrc <> 0.
    DATA lv_copy TYPE string.
    lv_copy = ls_row-carrid.
  ENDIF.
ENDFORM.

FORM irrelevant_initial.
  TYPES: BEGIN OF ty_row,
           carrid TYPE string,
         END OF ty_row.
  DATA ls_row TYPE ty_row.
  DATA lv_other TYPE string.
  SELECT SINGLE carrid
    FROM scarr
    INTO ls_row.
  IF lv_other IS INITIAL.
    DATA lv_copy TYPE string.
    lv_copy = ls_row-carrid.
  ENDIF.
ENDFORM.

FORM irrelevant_not_initial.
  TYPES: BEGIN OF ty_row,
           carrid TYPE string,
         END OF ty_row.
  DATA ls_row TYPE ty_row.
  DATA lv_other TYPE string VALUE 'LH'.
  SELECT SINGLE carrid
    FROM scarr
    INTO ls_row.
  IF lv_other IS NOT INITIAL.
    DATA lv_copy TYPE string.
    lv_copy = ls_row-carrid.
  ENDIF.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://select_single_irrelevant_guards.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT), 3)
}

@(test)
routine_flow_refines_endselect_target_with_sy_subrc_success_guard :: proc(t: ^testing.T) {
	source := `
FORM unguarded_endselect.
  TYPES: BEGIN OF ty_row,
           carrid TYPE string,
         END OF ty_row.
  DATA ls_row TYPE ty_row.
  SELECT carrid
    FROM scarr
    UP TO 1 ROWS
    INTO ls_row.
  ENDSELECT.
  DATA lv_copy TYPE string.
  lv_copy = ls_row-carrid.
ENDFORM.

FORM guarded_endselect.
  TYPES: BEGIN OF ty_row,
           carrid TYPE string,
         END OF ty_row.
  DATA ls_row TYPE ty_row.
  SELECT carrid
    FROM scarr
    UP TO 1 ROWS
    INTO ls_row.
  ENDSELECT.
  IF sy-subrc IS INITIAL.
    DATA lv_copy TYPE string.
    lv_copy = ls_row-carrid.
  ENDIF.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://endselect_subrc_success_guard.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT), 1)
}

@(test)
routine_flow_refines_select_single_target_after_negative_sy_subrc_return_guards :: proc(t: ^testing.T) {
	source := `
FORM not_equal_return.
  TYPES: BEGIN OF ty_row,
           carrid TYPE string,
         END OF ty_row.
  DATA ls_row TYPE ty_row.
  SELECT SINGLE carrid
    FROM scarr
    INTO ls_row.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.
  DATA lv_copy TYPE string.
  lv_copy = ls_row-carrid.
ENDFORM.

FORM is_not_initial_return.
  TYPES: BEGIN OF ty_row,
           carrid TYPE string,
         END OF ty_row.
  DATA ls_row TYPE ty_row.
  SELECT SINGLE carrid
    FROM scarr
    INTO ls_row.
  IF sy-subrc IS NOT INITIAL.
    RETURN.
  ENDIF.
  DATA lv_copy TYPE string.
  lv_copy = ls_row-carrid.
ENDFORM.

FORM or_empty_return.
  TYPES: BEGIN OF ty_row,
           carrid TYPE string,
         END OF ty_row.
  DATA ls_row TYPE ty_row.
  DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  SELECT SINGLE carrid
    FROM scarr
    INTO ls_row.
  IF sy-subrc <> 0 OR lines( lt_rows ) = 0.
    RETURN.
  ENDIF.
  DATA lv_copy TYPE string.
  lv_copy = ls_row-carrid.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://select_single_negative_subrc_return_guards.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT))
}

@(test)
routine_flow_refines_read_table_target_after_negative_sy_subrc_raising_helper :: proc(t: ^testing.T) {
	source := `
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS build.
  PRIVATE SECTION.
    METHODS raise_bad_request RAISING cx_static_check.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD build.
    TYPES: BEGIN OF ty_order,
             odata_property TYPE string,
             sql_column TYPE string,
           END OF ty_order.
    TYPES tt_order TYPE HASHED TABLE OF ty_order WITH UNIQUE KEY odata_property.
    DATA lt_allowed_order TYPE tt_order.
    DATA lv_property TYPE string.

    READ TABLE lt_allowed_order
      WITH TABLE KEY odata_property = lv_property
      INTO DATA(ls_allowed_order).

    IF sy-subrc <> 0.
      me->raise_bad_request( ).
    ENDIF.

    DATA lv_sql_column TYPE string.
    lv_sql_column = ls_allowed_order-sql_column.
  ENDMETHOD.

  METHOD raise_bad_request.
    RAISE EXCEPTION TYPE cx_static_check.
  ENDMETHOD.
ENDCLASS.
`
	semantic_analysis := test_analysis(source, "mem://read_table_negative_subrc_raising_helper.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	for diagnostic in analysis_result.diagnostics {
		testing.expect(
			t,
			diagnostic.id != USE_BEFORE_DEFINITE_ASSIGNMENT ||
			!strings.contains(source[diagnostic.range.start:diagnostic.range.end], "ls_allowed_order"),
		)
	}
}

@(test)
routine_flow_does_not_refine_read_table_target_after_negative_sy_subrc_returning_helper :: proc(t: ^testing.T) {
	source := `
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS build.
  PRIVATE SECTION.
    METHODS maybe_raise RAISING cx_static_check.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD build.
    TYPES: BEGIN OF ty_order,
             odata_property TYPE string,
             sql_column TYPE string,
           END OF ty_order.
    TYPES tt_order TYPE HASHED TABLE OF ty_order WITH UNIQUE KEY odata_property.
    DATA lt_allowed_order TYPE tt_order.
    DATA lv_property TYPE string.

    READ TABLE lt_allowed_order
      WITH TABLE KEY odata_property = lv_property
      INTO DATA(ls_allowed_order).

    IF sy-subrc <> 0.
      me->maybe_raise( ).
    ENDIF.

    DATA lv_sql_column TYPE string.
    lv_sql_column = ls_allowed_order-sql_column.
  ENDMETHOD.

  METHOD maybe_raise.
    RETURN.
  ENDMETHOD.
ENDCLASS.
`
	semantic_analysis := test_analysis(source, "mem://read_table_negative_subrc_returning_helper.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT), 1)
}

@(test)
routine_flow_does_not_merge_at_group_branch_assignments_into_loop_body :: proc(t: ^testing.T) {
	source := `
FORM with_at.
  TYPES: BEGIN OF ty_row,
           a TYPE i,
         END OF ty_row.
  TYPES: BEGIN OF ty_state,
           x TYPE i,
         END OF ty_state.
  DATA itab TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
  DATA ls_state TYPE ty_state.
  DATA lv_sink TYPE i.
  LOOP AT itab INTO DATA(ls_row).
    AT END OF a.
      ls_state-x = 1.
    ENDAT.
    lv_sink = ls_state-x.
  ENDLOOP.
ENDFORM.

FORM without_at.
  TYPES: BEGIN OF ty_row,
           a TYPE i,
         END OF ty_row.
  TYPES: BEGIN OF ty_state,
           x TYPE i,
         END OF ty_state.
  DATA itab TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
  DATA ls_state TYPE ty_state.
  DATA lv_sink TYPE i.
  LOOP AT itab INTO DATA(ls_row).
    ls_state-x = 1.
    lv_sink = ls_state-x.
  ENDLOOP.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://at_group_dataflow.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT), 1)
}

@(test)
routine_flow_skips_loop_where_and_at_group_row_field_probes :: proc(t: ^testing.T) {
	source := `
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_evt,
             rep_evtid TYPE string,
             priority TYPE string,
             msguid_out TYPE string,
           END OF ty_evt.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lt_evt TYPE STANDARD TABLE OF ty_evt WITH EMPTY KEY.
    LOOP AT lt_evt INTO DATA(ls_evt) WHERE priority = 'X'.
      AT END OF rep_evtid.
      ENDAT.
      DATA lv_guid TYPE string.
      lv_guid = ls_evt-msguid_out.
      DELETE lt_evt WHERE rep_evtid = ls_evt-rep_evtid.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
`
	semantic_analysis := test_analysis(source, "mem://loop_where_row_field_probes.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT))
}

@(test)
routine_flow_allows_reads_of_written_structure_selectors :: proc(t: ^testing.T) {
	source := `
FORM run.
  TYPES: BEGIN OF ty_job,
           jobname TYPE string,
           username TYPE string,
         END OF ty_job.
  DATA ls_job TYPE ty_job.
  ls_job-jobname = 'BATCH'.
  DATA lv_jobname TYPE string.
  lv_jobname = ls_job-jobname.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://routine_struct_selector_read_written.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT))
}

@(test)
routine_flow_flags_unwritten_structure_selector_reads :: proc(t: ^testing.T) {
	source := `
FORM run.
  TYPES: BEGIN OF ty_job,
           jobname TYPE string,
           username TYPE string,
         END OF ty_job.
  DATA ls_job TYPE ty_job.
  ls_job-jobname = 'BATCH'.
  DATA lv_username TYPE string.
  lv_username = ls_job-username.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://routine_struct_selector_read_unwritten.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT), 1)
}

@(test)
routine_flow_reuses_structure_selector_initial_guards :: proc(t: ^testing.T) {
	source := `
FORM run.
  TYPES: BEGIN OF ty_job,
           jobname TYPE string,
           username TYPE string,
         END OF ty_job.
  DATA lt_jobs TYPE STANDARD TABLE OF ty_job WITH EMPTY KEY.
  READ TABLE lt_jobs INTO DATA(ls_job) INDEX 1.
  IF ls_job-jobname IS NOT INITIAL.
    DATA lv_copy TYPE string.
    lv_copy = ls_job-jobname.
  ENDIF.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://routine_selector_field_guard.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT))
}

@(test)
routine_flow_reuses_whole_structure_non_initial_guards_for_selectors :: proc(t: ^testing.T) {
	source := `
FORM run.
  TYPES: BEGIN OF ty_job,
           jobname TYPE string,
           username TYPE string,
         END OF ty_job.
  DATA lt_jobs TYPE STANDARD TABLE OF ty_job WITH EMPTY KEY.
  READ TABLE lt_jobs INTO DATA(ls_job) INDEX 1.
  IF ls_job IS NOT INITIAL.
    DATA lv_copy TYPE string.
    lv_copy = ls_job-jobname.
  ENDIF.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://routine_selector_after_struct_guard.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT))
}

@(test)
routine_flow_clears_read_table_success_state_after_find :: proc(t: ^testing.T) {
	source := `
FORM inline_data.
  DATA lt_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
  READ TABLE lt_values INTO DATA(lv_value) INDEX 1.
  FIND '1' IN '123'.
  IF sy-subrc = 0.
    DATA lv_copy TYPE i.
    lv_copy = lv_value.
  ENDIF.
ENDFORM.

FORM field_symbol.
  DATA lt_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
  READ TABLE lt_values ASSIGNING FIELD-SYMBOL(<lv_value>) INDEX 1.
  FIND '1' IN '123'.
  IF sy-subrc = 0.
    DATA lv_copy TYPE i.
    lv_copy = <lv_value>.
  ENDIF.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://read_table_success_cleared_by_find.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT), 1)
	testing.expect_value(t, test_diagnostic_count(analysis_result, POSSIBLY_UNBOUND_FIELD_SYMBOL), 1)
}

@(test)
routine_flow_flags_possibly_unbound_read_table_field_symbol :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lt_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
  READ TABLE lt_values ASSIGNING FIELD-SYMBOL(<lv_value>) INDEX 1.
  DATA lv_copy TYPE i.
  lv_copy = <lv_value>.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://read_table_field_symbol_unbound.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, POSSIBLY_UNBOUND_FIELD_SYMBOL), 1)
}

@(test)
routine_flow_flags_possibly_unbound_top_level_field_symbol_write :: proc(t: ^testing.T) {
	source := `
DATA lt_values TYPE STANDARD TABLE OF string WITH EMPTY KEY.
READ TABLE lt_values INDEX 1 ASSIGNING FIELD-SYMBOL(<lv_value>).
<lv_value> = 'hello'.
`
	semantic_analysis := test_analysis(source, "mem://top_level_field_symbol_write_unbound.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, POSSIBLY_UNBOUND_FIELD_SYMBOL), 1)
}

@(test)
routine_flow_refines_field_symbol_binding_with_guards :: proc(t: ^testing.T) {
	source := `
FORM run.
  TYPES: BEGIN OF ty_row,
           text TYPE string,
         END OF ty_row.
  DATA ls_row TYPE ty_row.
  DATA lv_name TYPE string VALUE 'TEXT'.
  FIELD-SYMBOLS <text> TYPE string.

  ASSIGN COMPONENT lv_name OF STRUCTURE ls_row TO <text>.
  IF NOT <text> IS ASSIGNED.
    RETURN.
  ENDIF.
  WRITE <text>.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://field_symbol_guarded.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, POSSIBLY_UNBOUND_FIELD_SYMBOL))
}

@(test)
routine_flow_treats_perform_changing_arguments_as_writes :: proc(t: ^testing.T) {
	source := `
FORM fill CHANGING cv_value TYPE string.
  cv_value = 'ok'.
ENDFORM.

FORM run.
  DATA lv_value TYPE string.
  PERFORM fill CHANGING lv_value.
  DATA lv_copy TYPE string.
  lv_copy = lv_value.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://perform_changing_write.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT))
}

@(test)
routine_flow_treats_perform_changing_read_before_write_as_read :: proc(t: ^testing.T) {
	source := `
FORM bump CHANGING cv_value TYPE i.
  cv_value = cv_value + 1.
ENDFORM.

FORM run.
  DATA lv_value TYPE i.
  PERFORM bump CHANGING lv_value.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://perform_changing_read_before_write.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT), 1)
}

@(test)
routine_flow_does_not_treat_unwritten_perform_changing_as_assignment :: proc(t: ^testing.T) {
	source := `
FORM ignore CHANGING cv_value TYPE string.
ENDFORM.

FORM run.
  DATA lv_value TYPE string.
  PERFORM ignore CHANGING lv_value.
  DATA lv_copy TYPE string.
  lv_copy = lv_value.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://perform_changing_unwritten.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT), 1)
}

@(test)
routine_flow_propagates_nested_perform_changing_writes :: proc(t: ^testing.T) {
	source := `
CLASS lcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS fill EXPORTING ev_value TYPE string.
ENDCLASS.

CLASS lcl_dep IMPLEMENTATION.
  METHOD fill.
    ev_value = 'ok'.
  ENDMETHOD.
ENDCLASS.

FORM inner CHANGING cv_value TYPE string.
  DATA lo_dep TYPE REF TO lcl_dep.
  lo_dep = NEW lcl_dep( ).
  lo_dep->fill( IMPORTING ev_value = cv_value ).
ENDFORM.

FORM outer CHANGING cv_value TYPE string.
  PERFORM inner CHANGING cv_value.
ENDFORM.

FORM run.
  DATA lv_value TYPE string.
  PERFORM outer CHANGING lv_value.
  DATA lv_copy TYPE string.
  lv_copy = lv_value.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://nested_perform_changing_write.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT))
}

@(test)
routine_flow_propagates_nested_perform_write_like_changing_writes :: proc(t: ^testing.T) {
	source := `
FORM inner CHANGING cv_value TYPE string.
  CONCATENATE 'o' 'k' INTO cv_value.
ENDFORM.

FORM outer CHANGING cv_value TYPE string.
  PERFORM inner CHANGING cv_value.
ENDFORM.

FORM run.
  DATA lv_value TYPE string.
  PERFORM outer CHANGING lv_value.
  DATA lv_copy TYPE string.
  lv_copy = lv_value.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://nested_perform_changing_concat_write.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT))
}

@(test)
routine_flow_propagates_nested_perform_changing_read_before_write :: proc(t: ^testing.T) {
	source := `
FORM inner CHANGING cv_value TYPE i.
  cv_value = cv_value + 1.
ENDFORM.

FORM outer CHANGING cv_value TYPE i.
  PERFORM inner CHANGING cv_value.
ENDFORM.

FORM run.
  DATA lv_value TYPE i.
  PERFORM outer CHANGING lv_value.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://nested_perform_changing_read_before_write.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT), 1)
}

@(test)
routine_flow_propagates_nested_perform_changing_unwritten :: proc(t: ^testing.T) {
	source := `
FORM inner CHANGING cv_value TYPE string.
ENDFORM.

FORM outer CHANGING cv_value TYPE string.
  PERFORM inner CHANGING cv_value.
ENDFORM.

FORM run.
  DATA lv_value TYPE string.
  PERFORM outer CHANGING lv_value.
  DATA lv_copy TYPE string.
  lv_copy = lv_value.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://nested_perform_changing_unwritten.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	copy_start := strings.index(source, "lv_copy = lv_value.")
	testing.expect(t, copy_start >= 0)
	testing.expect_value(t, test_diagnostic_count(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT), 1)
	testing.expect_value(t, test_diagnostic_count_at_or_after(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT, copy_start), 1)
}

@(test)
routine_flow_treats_call_expression_output_arguments_as_writes :: proc(t: ^testing.T) {
	source := `
CLASS lcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS fill EXPORTING ev_value TYPE string.
ENDCLASS.

CLASS lcl_dep IMPLEMENTATION.
  METHOD fill.
    ev_value = 'ok'.
  ENDMETHOD.
ENDCLASS.

FORM run.
  DATA lo_dep TYPE REF TO lcl_dep.
  lo_dep = NEW lcl_dep( ).
  DATA lv_value TYPE string.
  lo_dep->fill( IMPORTING ev_value = lv_value ).
  DATA lv_copy TYPE string.
  lv_copy = lv_value.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://call_expr_output_write.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT))
}

@(test)
routine_flow_treats_write_like_statement_targets_as_assignments :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lt_values TYPE STANDARD TABLE OF string WITH EMPTY KEY.

  READ TABLE lt_values INTO DATA(lv_concat) INDEX 1.
  CONCATENATE 'a' 'b' INTO lv_concat.
  DATA lv_concat_copy TYPE string.
  lv_concat_copy = lv_concat.

  READ TABLE lt_values INTO DATA(lv_split) INDEX 1.
  SPLIT 'a:b' AT ':' INTO lv_split DATA(lv_split_tail).
  DATA lv_split_copy TYPE string.
  lv_split_copy = lv_split.

  READ TABLE lt_values INTO DATA(lv_message) INDEX 1.
  MESSAGE '001' TYPE 'I' INTO lv_message.
  DATA lv_message_copy TYPE string.
  lv_message_copy = lv_message.

  READ TABLE lt_values INTO DATA(lv_import) INDEX 1.
  IMPORT row TO lv_import FROM MEMORY ID 'id'.
  IF sy-subrc = 0.
    DATA lv_import_copy TYPE string.
    lv_import_copy = lv_import.
  ENDIF.

  DATA lv_cursor TYPE cursor.
  OPEN CURSOR lv_cursor FOR SELECT carrid FROM scarr.
  READ TABLE lt_values INTO DATA(lv_fetch) INDEX 1.
  FETCH NEXT CURSOR lv_cursor INTO lv_fetch.
  IF sy-subrc = 0.
    DATA lv_fetch_copy TYPE string.
    lv_fetch_copy = lv_fetch.
  ENDIF.
  CLOSE CURSOR lv_cursor.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://write_like_statement_targets.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT))
}

@(test)
routine_flow_treats_additional_write_like_statement_targets_as_assignments :: proc(t: ^testing.T) {
	source := `
CLASS lcl_ref DEFINITION.
ENDCLASS.

CLASS lcl_ref IMPLEMENTATION.
ENDCLASS.

FORM run.
  DATA lt_strings TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA lt_ints TYPE STANDARD TABLE OF i WITH EMPTY KEY.
  DATA lt_dates TYPE STANDARD TABLE OF d WITH EMPTY KEY.
  DATA lt_times TYPE STANDARD TABLE OF t WITH EMPTY KEY.
  DATA lt_stamps TYPE STANDARD TABLE OF timestamp WITH EMPTY KEY.
  DATA lt_refs TYPE STANDARD TABLE OF REF TO lcl_ref WITH EMPTY KEY.
  DATA lt_data_refs TYPE STANDARD TABLE OF REF TO data WITH EMPTY KEY.
  DATA lv_source TYPE string.
  lv_source = 'x'.
  DATA lv_file TYPE string.
  lv_file = 'dataset.txt'.
  DATA lv_program TYPE string.
  lv_program = 'ZDEMO'.
  DATA lv_zone TYPE string.
  lv_zone = 'UTC'.
  DATA lv_stamp TYPE timestamp.
  lv_stamp = '20240101000000'.
  DATA lv_date_in TYPE d.
  lv_date_in = sy-datum.
  DATA lv_time_in TYPE t.
  lv_time_in = sy-uzeit.
  DATA lv_pool_source TYPE string.
  lv_pool_source = 'REPORT zdemo.'.

  READ TABLE lt_strings INTO DATA(lv_xml) INDEX 1.
  CALL TRANSFORMATION id SOURCE root = lv_source RESULT XML lv_xml.
  WRITE lv_xml.

  READ TABLE lt_strings INTO DATA(lv_named_result) INDEX 1.
  CALL TRANSFORMATION id SOURCE root = lv_source RESULT rv_result = lv_named_result.
  WRITE lv_named_result.

  READ TABLE lt_strings INTO DATA(lv_receive) INDEX 1.
  RECEIVE RESULTS FROM FUNCTION 'Z_DEMO' IMPORTING ev_value = lv_receive.
  WRITE lv_receive.

  READ TABLE lt_refs INTO DATA(lo_ref) INDEX 1.
  CREATE OBJECT lo_ref.
  IF lo_ref IS BOUND.
  ENDIF.

  READ TABLE lt_data_refs INTO DATA(lr_data) INDEX 1.
  CREATE DATA lr_data.
  IF lr_data IS BOUND.
  ENDIF.

  READ TABLE lt_strings INTO DATA(lv_dataset_message) INDEX 1.
  OPEN DATASET lv_file FOR INPUT IN TEXT MODE ENCODING DEFAULT MESSAGE lv_dataset_message.
  WRITE lv_dataset_message.

  READ TABLE lt_strings INTO DATA(lv_dataset_line) INDEX 1.
  READ TABLE lt_ints INTO DATA(lv_dataset_actual) INDEX 1.
  READ TABLE lt_ints INTO DATA(lv_dataset_length) INDEX 1.
  READ DATASET lv_file INTO lv_dataset_line ACTUAL LENGTH lv_dataset_actual LENGTH lv_dataset_length.
  WRITE lv_dataset_line.
  WRITE lv_dataset_actual.
  WRITE lv_dataset_length.

  READ TABLE lt_strings INTO DATA(lv_report_source) INDEX 1.
  READ REPORT lv_program INTO lv_report_source.
  WRITE lv_report_source.

  READ TABLE lt_strings INTO DATA(lv_textpool) INDEX 1.
  READ TEXTPOOL lv_program INTO lv_textpool LANGUAGE sy-langu.
  WRITE lv_textpool.

  READ TABLE lt_dates INTO DATA(lv_date) INDEX 1.
  READ TABLE lt_times INTO DATA(lv_time) INDEX 1.
  CONVERT TIME STAMP lv_stamp TIME ZONE lv_zone INTO DATE lv_date TIME lv_time.
  WRITE lv_date.
  WRITE lv_time.

  READ TABLE lt_stamps INTO DATA(lv_new_stamp) INDEX 1.
  CONVERT DATE lv_date_in TIME lv_time_in INTO TIME STAMP lv_new_stamp TIME ZONE lv_zone.
  WRITE lv_new_stamp.

  READ TABLE lt_strings INTO DATA(lv_language) INDEX 1.
  READ TABLE lt_strings INTO DATA(lv_country) INDEX 1.
  READ TABLE lt_strings INTO DATA(lv_modifier) INDEX 1.
  GET LOCALE LANGUAGE lv_language COUNTRY lv_country MODIFIER lv_modifier.
  WRITE lv_language.
  WRITE lv_country.
  WRITE lv_modifier.

  READ TABLE lt_strings INTO DATA(lv_generated_program) INDEX 1.
  READ TABLE lt_strings INTO DATA(lv_generate_message) INDEX 1.
  READ TABLE lt_ints INTO DATA(lv_generate_line) INDEX 1.
  READ TABLE lt_strings INTO DATA(lv_generate_word) INDEX 1.
  READ TABLE lt_ints INTO DATA(lv_generate_offset) INDEX 1.
  GENERATE SUBROUTINE POOL lv_pool_source NAME lv_generated_program MESSAGE lv_generate_message LINE lv_generate_line WORD lv_generate_word OFFSET lv_generate_offset.
  WRITE lv_generated_program.
  WRITE lv_generate_message.
  WRITE lv_generate_line.
  WRITE lv_generate_word.
  WRITE lv_generate_offset.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://additional_write_like_statement_targets.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT))
}

@(test)
routine_flow_propagates_additional_write_like_changing_writes :: proc(t: ^testing.T) {
	source := `
CLASS lcl_ref DEFINITION.
ENDCLASS.

CLASS lcl_ref IMPLEMENTATION.
ENDCLASS.

FORM fill_transform CHANGING cv_value TYPE string.
  DATA lv_source TYPE string.
  lv_source = 'x'.
  CALL TRANSFORMATION id SOURCE root = lv_source RESULT XML cv_value.
ENDFORM.

FORM fill_receive CHANGING cv_value TYPE string.
  RECEIVE RESULTS FROM FUNCTION 'Z_DEMO' IMPORTING ev_value = cv_value.
ENDFORM.

FORM fill_create_object CHANGING co_ref TYPE REF TO lcl_ref.
  CREATE OBJECT co_ref.
ENDFORM.

FORM fill_create_data CHANGING cr_data TYPE REF TO data.
  CREATE DATA cr_data.
ENDFORM.

FORM fill_dataset_message CHANGING cv_value TYPE string.
  DATA lv_file TYPE string.
  lv_file = 'dataset.txt'.
  OPEN DATASET lv_file FOR INPUT IN TEXT MODE ENCODING DEFAULT MESSAGE cv_value.
ENDFORM.

FORM fill_dataset_read CHANGING cv_value TYPE string.
  DATA lv_file TYPE string.
  lv_file = 'dataset.txt'.
  READ DATASET lv_file INTO cv_value.
ENDFORM.

FORM fill_report CHANGING cv_value TYPE string.
  DATA lv_program TYPE string.
  lv_program = 'ZDEMO'.
  READ REPORT lv_program INTO cv_value.
ENDFORM.

FORM fill_textpool CHANGING cv_value TYPE string.
  DATA lv_program TYPE string.
  lv_program = 'ZDEMO'.
  READ TEXTPOOL lv_program INTO cv_value LANGUAGE sy-langu.
ENDFORM.

FORM fill_convert_date CHANGING cv_value TYPE d.
  DATA lv_stamp TYPE timestamp.
  lv_stamp = '20240101000000'.
  DATA lv_zone TYPE string.
  lv_zone = 'UTC'.
  CONVERT TIME STAMP lv_stamp TIME ZONE lv_zone INTO DATE cv_value.
ENDFORM.

FORM fill_convert_stamp CHANGING cv_value TYPE timestamp.
  DATA lv_date TYPE d.
  lv_date = sy-datum.
  DATA lv_time TYPE t.
  lv_time = sy-uzeit.
  DATA lv_zone TYPE string.
  lv_zone = 'UTC'.
  CONVERT DATE lv_date TIME lv_time INTO TIME STAMP cv_value TIME ZONE lv_zone.
ENDFORM.

FORM fill_locale CHANGING cv_value TYPE string.
  GET LOCALE LANGUAGE cv_value.
ENDFORM.

FORM fill_generate CHANGING cv_value TYPE string.
  DATA lv_source TYPE string.
  lv_source = 'REPORT zdemo.'.
  GENERATE SUBROUTINE POOL lv_source MESSAGE cv_value.
ENDFORM.

FORM run.
  DATA lv_transform TYPE string.
  PERFORM fill_transform CHANGING lv_transform.
  WRITE lv_transform.

  DATA lv_receive TYPE string.
  PERFORM fill_receive CHANGING lv_receive.
  WRITE lv_receive.

  DATA lo_ref TYPE REF TO lcl_ref.
  PERFORM fill_create_object CHANGING lo_ref.
  IF lo_ref IS BOUND.
  ENDIF.

  DATA lr_data TYPE REF TO data.
  PERFORM fill_create_data CHANGING lr_data.
  IF lr_data IS BOUND.
  ENDIF.

  DATA lv_dataset_message TYPE string.
  PERFORM fill_dataset_message CHANGING lv_dataset_message.
  WRITE lv_dataset_message.

  DATA lv_dataset_line TYPE string.
  PERFORM fill_dataset_read CHANGING lv_dataset_line.
  WRITE lv_dataset_line.

  DATA lv_report TYPE string.
  PERFORM fill_report CHANGING lv_report.
  WRITE lv_report.

  DATA lv_textpool TYPE string.
  PERFORM fill_textpool CHANGING lv_textpool.
  WRITE lv_textpool.

  DATA lv_date TYPE d.
  PERFORM fill_convert_date CHANGING lv_date.
  WRITE lv_date.

  DATA lv_stamp TYPE timestamp.
  PERFORM fill_convert_stamp CHANGING lv_stamp.
  WRITE lv_stamp.

  DATA lv_language TYPE string.
  PERFORM fill_locale CHANGING lv_language.
  WRITE lv_language.

  DATA lv_generate_message TYPE string.
  PERFORM fill_generate CHANGING lv_generate_message.
  WRITE lv_generate_message.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://additional_write_like_changing_writes.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT))
}

@(test)
routine_flow_treats_write_like_overwrites_as_dead_stores :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lv_concat TYPE string.
  lv_concat = 'old'.
  CONCATENATE 'a' 'b' INTO lv_concat.
  WRITE lv_concat.

  DATA lv_split TYPE string.
  lv_split = 'old'.
  SPLIT 'a:b' AT ':' INTO lv_split.
  WRITE lv_split.

  DATA lv_message TYPE string.
  lv_message = 'old'.
  MESSAGE '001' TYPE 'I' INTO lv_message.
  WRITE lv_message.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://write_like_dead_stores.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, DEAD_STORE), 3)
}

@(test)
routine_flow_treats_set_user_command_operand_as_a_read :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA assigned_command TYPE string.
  assigned_command = 'ENTER'.
  SET USER-COMMAND assigned_command.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://set_user_command_flow.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, DEAD_STORE), 0)
}

@(test)
routine_flow_treats_additional_write_like_overwrites_as_dead_stores :: proc(t: ^testing.T) {
	source := `
CLASS lcl_ref DEFINITION.
ENDCLASS.

CLASS lcl_ref IMPLEMENTATION.
ENDCLASS.

FORM run.
  DATA lv_source TYPE string.
  lv_source = 'x'.
  DATA lv_file TYPE string.
  lv_file = 'dataset.txt'.
  DATA lv_program TYPE string.
  lv_program = 'ZDEMO'.
  DATA lv_zone TYPE string.
  lv_zone = 'UTC'.
  DATA lv_stamp TYPE timestamp.
  lv_stamp = '20240101000000'.
  DATA lv_date_in TYPE d.
  lv_date_in = sy-datum.
  DATA lv_time_in TYPE t.
  lv_time_in = sy-uzeit.
  DATA lv_pool_source TYPE string.
  lv_pool_source = 'REPORT zdemo.'.

  DATA lv_xml TYPE string.
  lv_xml = 'old'.
  CALL TRANSFORMATION id SOURCE root = lv_source RESULT XML lv_xml.
  WRITE lv_xml.

  DATA lv_named_result TYPE string.
  lv_named_result = 'old'.
  CALL TRANSFORMATION id SOURCE root = lv_source RESULT rv_result = lv_named_result.
  WRITE lv_named_result.

  DATA lv_receive TYPE string.
  lv_receive = 'old'.
  RECEIVE RESULTS FROM FUNCTION 'Z_DEMO' IMPORTING ev_value = lv_receive.
  WRITE lv_receive.

  DATA lo_ref TYPE REF TO lcl_ref.
  lo_ref = NEW lcl_ref( ).
  CREATE OBJECT lo_ref.
  IF lo_ref IS BOUND.
  ENDIF.

  DATA lr_data TYPE REF TO data.
  CREATE DATA lr_data.
  CREATE DATA lr_data.
  IF lr_data IS BOUND.
  ENDIF.

  DATA lv_dataset_message TYPE string.
  lv_dataset_message = 'old'.
  OPEN DATASET lv_file FOR INPUT IN TEXT MODE ENCODING DEFAULT MESSAGE lv_dataset_message.
  WRITE lv_dataset_message.

  DATA lv_dataset_line TYPE string.
  lv_dataset_line = 'old'.
  DATA lv_dataset_actual TYPE i.
  lv_dataset_actual = 1.
  DATA lv_dataset_length TYPE i.
  lv_dataset_length = 1.
  READ DATASET lv_file INTO lv_dataset_line ACTUAL LENGTH lv_dataset_actual LENGTH lv_dataset_length.
  WRITE lv_dataset_line.
  WRITE lv_dataset_actual.
  WRITE lv_dataset_length.

  DATA lv_report_source TYPE string.
  lv_report_source = 'old'.
  READ REPORT lv_program INTO lv_report_source.
  WRITE lv_report_source.

  DATA lv_textpool TYPE string.
  lv_textpool = 'old'.
  READ TEXTPOOL lv_program INTO lv_textpool LANGUAGE sy-langu.
  WRITE lv_textpool.

  DATA lv_date TYPE d.
  lv_date = sy-datum.
  DATA lv_time TYPE t.
  lv_time = sy-uzeit.
  CONVERT TIME STAMP lv_stamp TIME ZONE lv_zone INTO DATE lv_date TIME lv_time.
  WRITE lv_date.
  WRITE lv_time.

  DATA lv_new_stamp TYPE timestamp.
  lv_new_stamp = '20240101000000'.
  CONVERT DATE lv_date_in TIME lv_time_in INTO TIME STAMP lv_new_stamp TIME ZONE lv_zone.
  WRITE lv_new_stamp.

  DATA lv_language TYPE string.
  lv_language = 'old'.
  DATA lv_country TYPE string.
  lv_country = 'old'.
  DATA lv_modifier TYPE string.
  lv_modifier = 'old'.
  GET LOCALE LANGUAGE lv_language COUNTRY lv_country MODIFIER lv_modifier.
  WRITE lv_language.
  WRITE lv_country.
  WRITE lv_modifier.

  DATA lv_generated_program TYPE string.
  lv_generated_program = 'old'.
  DATA lv_generate_message TYPE string.
  lv_generate_message = 'old'.
  DATA lv_generate_line TYPE i.
  lv_generate_line = 1.
  DATA lv_generate_word TYPE string.
  lv_generate_word = 'old'.
  DATA lv_generate_offset TYPE i.
  lv_generate_offset = 1.
  GENERATE SUBROUTINE POOL lv_pool_source NAME lv_generated_program MESSAGE lv_generate_message LINE lv_generate_line WORD lv_generate_word OFFSET lv_generate_offset.
  WRITE lv_generated_program.
  WRITE lv_generate_message.
  WRITE lv_generate_line.
  WRITE lv_generate_word.
  WRITE lv_generate_offset.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://additional_write_like_dead_stores.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, DEAD_STORE), 22)
}

@(test)
routine_flow_treats_arithmetic_statement_targets_as_reads_and_writes :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lt_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
  DATA lv_one TYPE i.
  lv_one = 1.

  READ TABLE lt_values INTO DATA(lv_add) INDEX 1.
  ADD 1 TO lv_add.
  DATA lv_add_after TYPE i.
  lv_add_after = lv_add.

  READ TABLE lt_values INTO DATA(lv_add_giving_target) INDEX 1.
  ADD lv_one TO lv_add_giving_target GIVING DATA(lv_add_result).
  DATA lv_add_copy TYPE i.
  lv_add_copy = lv_add_result.

  READ TABLE lt_values INTO DATA(lv_subtract) INDEX 1.
  SUBTRACT 1 FROM lv_subtract.
  DATA lv_subtract_after TYPE i.
  lv_subtract_after = lv_subtract.

  READ TABLE lt_values INTO DATA(lv_subtract_giving_target) INDEX 1.
  SUBTRACT lv_one FROM lv_subtract_giving_target GIVING DATA(lv_subtract_result).
  DATA lv_subtract_copy TYPE i.
  lv_subtract_copy = lv_subtract_result.

  READ TABLE lt_values INTO DATA(lv_multiply) INDEX 1.
  MULTIPLY lv_multiply BY lv_one.
  DATA lv_multiply_after TYPE i.
  lv_multiply_after = lv_multiply.

  READ TABLE lt_values INTO DATA(lv_multiply_giving_target) INDEX 1.
  MULTIPLY lv_multiply_giving_target BY lv_one GIVING DATA(lv_multiply_result).
  DATA lv_multiply_copy TYPE i.
  lv_multiply_copy = lv_multiply_result.

  READ TABLE lt_values INTO DATA(lv_divide_by) INDEX 1.
  DIVIDE lv_divide_by BY lv_one.
  DATA lv_divide_by_after TYPE i.
  lv_divide_by_after = lv_divide_by.

  READ TABLE lt_values INTO DATA(lv_divide_by_giving_target) INDEX 1.
  DIVIDE lv_divide_by_giving_target BY lv_one GIVING DATA(lv_divide_by_result).
  DATA lv_divide_by_copy TYPE i.
  lv_divide_by_copy = lv_divide_by_result.

  READ TABLE lt_values INTO DATA(lv_divide_into) INDEX 1.
  DIVIDE lv_one INTO lv_divide_into.
  DATA lv_divide_into_after TYPE i.
  lv_divide_into_after = lv_divide_into.

  READ TABLE lt_values INTO DATA(lv_divide_into_giving_target) INDEX 1.
  DIVIDE lv_one INTO lv_divide_into_giving_target GIVING DATA(lv_divide_into_result).
  DATA lv_divide_into_copy TYPE i.
  lv_divide_into_copy = lv_divide_into_result.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://arithmetic_statement_targets.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT), 10)
}

@(test)
routine_flow_propagates_arithmetic_changing_read_before_write :: proc(t: ^testing.T) {
	source := `
FORM increment CHANGING cv_value TYPE i.
  ADD 1 TO cv_value.
ENDFORM.

FORM run.
  DATA lv_value TYPE i.
  PERFORM increment CHANGING lv_value.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://arithmetic_changing_read_before_write.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT), 1)
}

@(test)
routine_flow_treats_arithmetic_overwrites_as_dead_stores :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lv_one TYPE i.
  lv_one = 1.

  DATA lv_add TYPE i.
  lv_add = 10.
  ADD lv_one TO lv_add.
  lv_add = 20.
  WRITE lv_add.

  DATA lv_add_giving_target TYPE i.
  DATA lv_add_result TYPE i.
  lv_add_giving_target = 10.
  lv_add_result = 99.
  ADD lv_one TO lv_add_giving_target GIVING lv_add_result.
  WRITE lv_add_result.

  DATA lv_subtract TYPE i.
  lv_subtract = 10.
  SUBTRACT lv_one FROM lv_subtract.
  lv_subtract = 20.
  WRITE lv_subtract.

  DATA lv_subtract_giving_target TYPE i.
  DATA lv_subtract_result TYPE i.
  lv_subtract_giving_target = 10.
  lv_subtract_result = 99.
  SUBTRACT lv_one FROM lv_subtract_giving_target GIVING lv_subtract_result.
  WRITE lv_subtract_result.

  DATA lv_multiply TYPE i.
  lv_multiply = 10.
  MULTIPLY lv_multiply BY lv_one.
  lv_multiply = 20.
  WRITE lv_multiply.

  DATA lv_multiply_giving_target TYPE i.
  DATA lv_multiply_result TYPE i.
  lv_multiply_giving_target = 10.
  lv_multiply_result = 99.
  MULTIPLY lv_multiply_giving_target BY lv_one GIVING lv_multiply_result.
  WRITE lv_multiply_result.

  DATA lv_divide_by TYPE i.
  lv_divide_by = 10.
  DIVIDE lv_divide_by BY lv_one.
  lv_divide_by = 20.
  WRITE lv_divide_by.

  DATA lv_divide_by_giving_target TYPE i.
  DATA lv_divide_by_result TYPE i.
  lv_divide_by_giving_target = 10.
  lv_divide_by_result = 99.
  DIVIDE lv_divide_by_giving_target BY lv_one GIVING lv_divide_by_result.
  WRITE lv_divide_by_result.

  DATA lv_divide_into TYPE i.
  lv_divide_into = 10.
  DIVIDE lv_one INTO lv_divide_into.
  lv_divide_into = 20.
  WRITE lv_divide_into.

  DATA lv_divide_into_giving_target TYPE i.
  DATA lv_divide_into_result TYPE i.
  lv_divide_into_giving_target = 10.
  lv_divide_into_result = 99.
  DIVIDE lv_one INTO lv_divide_into_giving_target GIVING lv_divide_into_result.
  WRITE lv_divide_into_result.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://arithmetic_dead_stores.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, DEAD_STORE), 10)
}

@(test)
routine_flow_treats_insert_assigning_as_field_symbol_binding :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lt_values TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  INSERT INITIAL LINE INTO TABLE lt_values ASSIGNING FIELD-SYMBOL(<lv_value>).
  WRITE <lv_value>.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://insert_assigning_field_symbol_binding.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, POSSIBLY_UNBOUND_FIELD_SYMBOL))
}

@(test)
routine_flow_flags_dead_store_on_overwrite_before_read :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lv_value TYPE i.
  IF 1 = 1.
    lv_value = 1.
  ENDIF.
  lv_value = 2.
  IF lv_value > 0.
  ENDIF.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://dead_store_overwrite.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, DEAD_STORE), 1)
}

@(test)
routine_flow_flags_dead_store_on_last_write_before_return :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lv_unused TYPE i.
  lv_unused = 1.
  RETURN.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://dead_store_return.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, DEAD_STORE), 1)
}

@(test)
unreachable_lint_covers_exhaustive_branches_and_loop_tails :: proc(t: ^testing.T) {
	source := `
FORM if_branch.
  DATA lv_after_if TYPE i.
  IF lv_after_if = 1.
    RETURN.
  ELSEIF lv_after_if = 2.
    RETURN.
  ELSE.
    RETURN.
  ENDIF.
  lv_after_if = 1.
ENDFORM.

FORM case_branch.
  DATA lv_after_case TYPE i.
  CASE lv_after_case.
    WHEN 1.
      RETURN.
    WHEN OTHERS.
      RETURN.
  ENDCASE.
  lv_after_case = 1.
ENDFORM.

FORM loop_tail.
  DATA lv_total TYPE i.
  WHILE lv_total < 10.
    CONTINUE.
    lv_total = lv_total + 1.
  ENDWHILE.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://unreachable_parity.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, UNREACHABLE_CODE), 3)
}

@(test)
routine_flow_stops_definite_assignment_after_loop_exit_and_continue :: proc(t: ^testing.T) {
	source := `
FORM exit_case.
  DATA lt_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
  DATA lv_copy TYPE i.
  DO 1 TIMES.
    READ TABLE lt_values INTO DATA(lv_value) INDEX 1.
    EXIT.
    lv_copy = lv_value.
  ENDDO.
ENDFORM.

FORM continue_case.
  DATA lt_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
  DATA lv_copy TYPE i.
  DO 1 TIMES.
    READ TABLE lt_values INTO DATA(lv_value) INDEX 1.
    CONTINUE.
    lv_copy = lv_value.
  ENDDO.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://loop_exit_continue_definite_assignment.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, USE_BEFORE_DEFINITE_ASSIGNMENT))
	testing.expect_value(t, test_diagnostic_count(analysis_result, UNREACHABLE_CODE), 2)
}

@(test)
leave_list_processing_is_only_guaranteed_exit_in_report_list_events :: proc(t: ^testing.T) {
	source := `
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    LEAVE LIST-PROCESSING.
    WRITE 'method after'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  LEAVE LIST-PROCESSING.
  WRITE 'event after'.

AT SELECTION-SCREEN.
  LEAVE LIST-PROCESSING.
  WRITE 'selection after'.
`
	semantic_analysis := test_analysis(source, "mem://leave_list_processing_context.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	unreachable_count := 0
	for diagnostic in analysis_result.diagnostics {
		if diagnostic.id != UNREACHABLE_CODE {
			continue
		}
		unreachable_count += 1
		testing.expect(t, source[diagnostic.range.start:diagnostic.range.end] == "WRITE 'event after'.")
	}
	testing.expect_value(t, unreachable_count, 1)
}

@(test)
result_lints_do_not_treat_sibling_branch_subrc_check_as_observation :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lv_cond TYPE i.
  IF lv_cond = 1.
    AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD 'SE38'.
    CALL FUNCTION 'Z_DEMO'
      EXCEPTIONS
        failed = 1.
  ENDIF.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.
  SELECT SINGLE carrid FROM scarr INTO @DATA(lv_carrid).
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://branch_subrc_observation.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, IGNORED_AUTHORITY_CHECK), 1)
	testing.expect_value(t, test_diagnostic_count(analysis_result, IGNORED_CALL_FUNCTION_RESULT), 1)
}

@(test)
call_function_output_result_read_must_match_same_symbol :: proc(t: ^testing.T) {
	source := `
FORM first.
  DATA lv_result TYPE string.
  CALL FUNCTION 'Z_DEMO'
    IMPORTING
      ev_result = lv_result.
ENDFORM.

FORM second.
  DATA lv_result TYPE string.
  WRITE lv_result.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://call_output_shadow_read.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, IGNORED_CALL_FUNCTION_RESULT), 1)
}

@(test)
for_all_entries_guard_does_not_match_shadowed_table_name :: proc(t: ^testing.T) {
	source := `
DATA lt_keys TYPE STANDARD TABLE OF string WITH EMPTY KEY.

IF lt_keys IS INITIAL.
  RETURN.
ENDIF.

FORM run.
  DATA lt_keys TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA lt_rows TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  SELECT matnr FROM mara INTO TABLE @lt_rows FOR ALL ENTRIES IN @lt_keys WHERE matnr = @lt_keys-table_line.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://fae_shadowed_guard.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect_value(t, test_diagnostic_count(analysis_result, FOR_ALL_ENTRIES_WITHOUT_GUARD), 1)
}

@(test)
routine_flow_flags_dead_store_in_global_declarations :: proc(t: ^testing.T) {
	source := `
REPORT zdead_store.

DATA gv_unused TYPE i.
gv_unused = 1.
`
	semantic_analysis := test_analysis(source, "mem://global_dead_store.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	found := false
	for diagnostic in analysis_result.diagnostics {
		if diagnostic.id == DEAD_STORE && diagnostic.message == "write to global variable 'gv_unused' is never read in global declarations" {
			found = true
			break
		}
	}
	testing.expect(t, found)
}

@(test)
routine_flow_does_not_flag_routine_writes_to_global_state_as_dead_store :: proc(t: ^testing.T) {
	source := `
REPORT zdead_store_global.

DATA gv_state TYPE i.

FORM run.
  gv_state = 1.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://routine_global_dead_store.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, DEAD_STORE))
}

@(test)
routine_flow_keeps_branch_merge_writes_live_for_dead_store :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lv_value TYPE i.
  IF 1 = 1.
    lv_value = 1.
  ELSE.
    lv_value = 2.
  ENDIF.
  IF lv_value > 0.
  ENDIF.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://dead_store_branch_live.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, DEAD_STORE))
}

@(test)
routine_flow_keeps_loop_carried_writes_live_for_dead_store :: proc(t: ^testing.T) {
	source := `
FORM run.
  DATA lv_total TYPE i.
  DATA lv_remaining TYPE i.
  lv_total = 0.
  lv_remaining = 3.
  WHILE lv_remaining > 0.
    lv_total = lv_total + 1.
    lv_remaining = lv_remaining - 1.
  ENDWHILE.
  IF lv_total > 0.
  ENDIF.
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://dead_store_loop_live.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, DEAD_STORE))
}

@(test)
routine_flow_suppresses_dead_store_around_changing_call_expressions :: proc(t: ^testing.T) {
	source := `
CLASS lcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS touch CHANGING cv_value TYPE i.
ENDCLASS.

CLASS lcl_dep IMPLEMENTATION.
  METHOD touch.
    cv_value = cv_value + 1.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lo_dep TYPE REF TO lcl_dep.
    DATA lv_value TYPE i.
    lo_dep = NEW lcl_dep( ).
    lv_value = 1.
    lo_dep->touch( CHANGING cv_value = lv_value ).
    lv_value = 2.
  ENDMETHOD.
ENDCLASS.
`
	semantic_analysis := test_analysis(source, "mem://dead_store_changing_call_expr.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, !test_has_diagnostic(analysis_result, DEAD_STORE))
}

@(test)
ast_trivia_suppressions_filter_or_report_lints :: proc(t: ^testing.T) {
	source := `
FORM run.
  " abap-lsp:allow-next-line(abap-lsp.select-star)
  SELECT * FROM mara INTO TABLE @DATA(lt_rows).
  SELECT * FROM mara INTO TABLE @DATA(lt_again). "#EC CI_ALL_FIELDS_NEEDED
ENDFORM.
`
	semantic_analysis := test_analysis(source, "mem://suppression.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)
	testing.expect(t, !test_has_diagnostic(analysis_result, SELECT_STAR))

	policy := test_policy_with_report_suppressed()
	reported := run_analysis_with_policy(&semantic_analysis, &policy)
	defer analysis_destroy(&reported)
	suppressed_select_star := 0
	for diagnostic in reported.diagnostics {
		if diagnostic.id == SELECT_STAR {
			testing.expect(t, diagnostic.suppressed)
			suppressed_select_star += 1
		}
	}
	testing.expect_value(t, suppressed_select_star, 2)
}

@(test)
semantic_open_sql_errors_are_wrapped_as_epc_lints :: proc(t: ^testing.T) {
	source := `FORM run.
  SELECT carrid FROM zmissing_source INTO @DATA(lv_carrid).
ENDFORM.`
	semantic_analysis := test_analysis(source, "mem://epc.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	analysis_result := run_analysis(&semantic_analysis)
	defer analysis_destroy(&analysis_result)

	testing.expect(t, test_has_diagnostic(analysis_result, EPC_UNVERIFIED_OPEN_SQL_SOURCE))
}

@(test)
workspace_lints_run_as_async_diagnostic_pass :: proc(t: ^testing.T) {
	pool: execution.Pool
	execution.pool_init(&pool, execution.Options{worker_count = 0, task_capacity = 16}, context.allocator)
	defer execution.pool_destroy(&pool)

	source := `FORM run.
  SELECT * FROM mara INTO TABLE @DATA(lt_rows).
ENDFORM.`
	semantic_analysis := test_analysis(source, "mem://at.abap")
	defer semantic.semantic_workspace_analysis_destroy(&semantic_analysis)

	graph: execution.Graph
	execution.graph_init(&graph, &pool, context.allocator)
	defer execution.graph_destroy(&graph)

	task := submit_analysis(&graph, execution.worker_executor(&pool), &semantic_analysis)
	execution.graph_start(&graph)
	analysis_result := execution.wait(task)
	defer analysis_destroy(&analysis_result)
	execution.graph_wait(&graph)

	testing.expect(t, test_has_diagnostic(analysis_result, SELECT_STAR))
}
