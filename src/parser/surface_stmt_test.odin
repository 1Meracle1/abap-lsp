package abap_frontend_parser

import "src:ast"

import "core:testing"

parse_error_message_count :: proc(errors: []Parse_Error, message: string) -> int {
	count := 0
	for err in errors {
		if err.message == message {
			count += 1
		}
	}
	return count
}

@(test)
statement_batch_open_sql_and_data_access :: proc(t: ^testing.T) {
	source := `SELECT * FROM mara INTO wa. WRITE wa. ENDSELECT.
OPEN CURSOR cv FOR SELECT * FROM mara.
FETCH NEXT CURSOR cv INTO wa.
CLOSE CURSOR cv.
INSERT mara FROM wa.
UPDATE mara FROM wa.
DELETE FROM mara WHERE matnr = lv.
READ TABLE itab INTO wa INDEX 1.
OPEN DATASET file FOR INPUT IN TEXT MODE.
REPORT zrep.
READ REPORT prog INTO lt.
READ TEXTPOOL prog INTO lt LANGUAGE sy-langu.`
	parsed := parse(source, "data_access.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.select_stmt, 1)
	testing.expect_value(t, counts.open_cursor, 1)
	testing.expect_value(t, counts.fetch_stmt, 1)
	testing.expect_value(t, counts.close_cursor, 1)
	testing.expect_value(t, counts.insert_stmt, 1)
	testing.expect_value(t, counts.append_stmt, 0)
	testing.expect_value(t, counts.update_stmt, 1)
	testing.expect_value(t, counts.delete_stmt, 1)
	testing.expect_value(t, counts.read_table, 1)
	testing.expect_value(t, counts.dataset_stmt, 1)
	testing.expect_value(t, counts.report_stmt, 2)
	testing.expect_value(t, counts.textpool_stmt, 1)
}

@(test)
dml_statements_report_missing_targets_and_bad_tails :: proc(t: ^testing.T) {
	source := `UPDATE SET field = value.
UPDATE ztab SET a = b c.
UPDATE ztab GARBAGE.
DELETE WHERE id = value.
DELETE itab GARBAGE.`
	parsed := parse(source, "dml_missing_targets.abap", context.allocator)

	expect_error_contains(t, parsed, "expected UPDATE target")
	expect_error_contains(t, parsed, "expected '=' in SQL assignment")
	expect_error_contains(t, parsed, "unexpected token in UPDATE statement")
	expect_error_contains(t, parsed, "expected DELETE target")
	expect_error_contains(t, parsed, "unexpected token in DELETE statement")
}

@(test)
insert_statements_keep_parser_table_facts :: proc(t: ^testing.T) {
	source := `INSERT zinsert_tab FROM TABLE lt_rows ACCEPTING DUPLICATE KEYS.
INSERT INTO zinto_tab VALUES ls_row.
INSERT ls_row INTO TABLE lt_rows INDEX lv_idx.`
	parsed := parse(source, "insert_facts.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	bare_db := parsed.root.stmts[0].derived_stmt.(^ast.Insert_Stmt)
	into_db := parsed.root.stmts[1].derived_stmt.(^ast.Insert_Stmt)
	internal := parsed.root.stmts[2].derived_stmt.(^ast.Insert_Stmt)

	testing.expect_value(t, bare_db.form, ast.Insert_Form.Db_Table)
	testing.expect(t, bare_db.has_db_table_name)
	testing.expect_value(t, bare_db.db_table_name.text, "zinsert_tab")
	testing.expect_value(t, source[bare_db.db_table_name.range.start:bare_db.db_table_name.range.end], "zinsert_tab")
	testing.expect(t, bare_db.from_table)
	testing.expect(t, bare_db.accepting_duplicate_keys)
	testing.expect_value(t, into_db.form, ast.Insert_Form.Db_Table)
	testing.expect(t, into_db.has_db_table_name)
	testing.expect_value(t, into_db.db_table_name.text, "zinto_tab")
	testing.expect_value(t, internal.form, ast.Insert_Form.Internal_Table)
	testing.expect(t, !internal.has_db_table_name)
	testing.expect(t, internal.target != nil)
	testing.expect(t, internal.index != nil)
}

@(test)
report_and_program_message_id_keep_parser_facts :: proc(t: ^testing.T) {
	source := `REPORT zmain MESSAGE-ID zmsg.
PROGRAM zprog MESSAGE-ID zcls.
READ REPORT prog INTO source.
INSERT REPORT prog FROM source.
DELETE REPORT prog.`
	parsed := parse(source, "report_message_id.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	report := parsed.root.stmts[0].derived_stmt.(^ast.Report_Stmt)
	program := parsed.root.stmts[1].derived_stmt.(^ast.Report_Stmt)
	read_report := parsed.root.stmts[2].derived_stmt.(^ast.Report_Stmt)
	insert_report := parsed.root.stmts[3].derived_stmt.(^ast.Report_Stmt)
	delete_report := parsed.root.stmts[4].derived_stmt.(^ast.Report_Stmt)

	testing.expect(t, report.has_message_id)
	testing.expect_value(t, report.message_id.text, "zmsg")
	testing.expect_value(t, source[report.message_id.range.start:report.message_id.range.end], "zmsg")
	testing.expect(t, program.has_message_id)
	testing.expect_value(t, program.message_id.text, "zcls")
	testing.expect_value(t, source[program.message_id.range.start:program.message_id.range.end], "zcls")
	testing.expect(t, !read_report.has_message_id)
	testing.expect(t, !insert_report.has_message_id)
	testing.expect(t, !delete_report.has_message_id)
	testing.expect_value(t, ast.print_node(report, context.allocator), "REPORT zmain MESSAGE-ID zmsg.")
	testing.expect_value(t, ast.print_node(program, context.allocator), "PROGRAM zprog MESSAGE-ID zcls.")
}

@(test)
program_include_statements_keep_concrete_nodes :: proc(t: ^testing.T) {
	source := `INCLUDE zinc.
INCLUDE: ztop, zf01.
INCLUDE TYPE ty_line.
INCLUDE STRUCTURE textpool.`
	parsed := parse(source, "includes.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.include_stmt, 2)
	testing.expect_value(t, counts.types_decl, 2)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)

	first := parsed.root.stmts[0].derived_stmt.(^ast.Include_Stmt)
	chained := parsed.root.stmts[1].derived_stmt.(^ast.Include_Stmt)
	include_type := parsed.root.stmts[2].derived_stmt.(^ast.Types_Decl)
	include_structure := parsed.root.stmts[3].derived_stmt.(^ast.Types_Decl)

	testing.expect_value(t, len(first.names), 1)
	testing.expect_value(t, first.names[0].name.text, "zinc")
	testing.expect(t, !first.if_found)
	testing.expect_value(t, source[first.names[0].name.range.start:first.names[0].name.range.end], "zinc")
	testing.expect_value(t, len(chained.names), 2)
	testing.expect_value(t, chained.names[0].name.text, "ztop")
	testing.expect_value(t, chained.names[1].name.text, "zf01")
	testing.expect_value(t, include_type.types[0].kind, ast.Decl_Clause_Kind.Include_Type)
	testing.expect_value(t, include_structure.types[0].kind, ast.Decl_Clause_Kind.Include_Structure)
}

@(test)
program_include_if_found_keeps_concrete_node :: proc(t: ^testing.T) {
	source := `INCLUDE zabapgit_user_exit IF FOUND.`
	parsed := parse(source, "include_if_found.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Include_Stmt)
	testing.expect_value(t, stmt.names[0].name.text, "zabapgit_user_exit")
	testing.expect(t, stmt.if_found)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
aggregate_select_and_compact_cleanup_call_keep_boundaries :: proc(t: ^testing.T) {
	source := `LOOP AT rows INTO row.
  SELECT MAX( dokversion )
    INTO version
    FROM dokhl
    WHERE id = row-id.
ENDLOOP.
TRY.
  cleanup( lo_set ).
CATCH cx_root.
  cleanup( lo_set ).
ENDTRY.`
	parsed := parse(source, "select_cleanup_boundaries.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.loop_stmt, 1)
	testing.expect_value(t, counts.select_stmt, 1)
	testing.expect_value(t, counts.try_stmt, 1)
}

@(test)
internal_table_append_modify_and_sort_keep_nodes :: proc(t: ^testing.T) {
	source := `APPEND lx_error->get_text( ) TO mt_text.
APPEND LINES OF li_package->list_subpackages( ) TO lt_packages.
MODIFY (c_tabname) FROM ls_content.
MODIFY lt_table FROM ls_line TRANSPORTING value WHERE key = lv_key.
SORT lt_table BY name DESCENDING.`
	parsed := parse(source, "itab_surface.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.append_stmt, 2)
	testing.expect_value(t, counts.modify_stmt, 2)
	testing.expect_value(t, counts.sort_stmt, 1)
	modify := parsed.root.stmts[3].derived_stmt.(^ast.Modify_Stmt)
	testing.expect_value(t, len(modify.transporting), 1)
	testing.expect_value(t, modify.transporting[0].name.text, "value")
	testing.expect_value(t, len(modify.transporting[0].path), 1)
	testing.expect_value(t, modify.transporting[0].path[0].name.text, "value")
}

@(test)
modify_transporting_rejects_spaced_component_selector :: proc(t: ^testing.T) {
	source := `MODIFY lt_table FROM ls_line TRANSPORTING nested - value.`
	parsed := parse(source, "modify_transporting_invalid.abap", context.allocator)

	testing.expect(t, len(parsed.errors) > 0)
	testing.expect_value(t, parsed.errors[0].message, "syntax error: expected MODIFY TRANSPORTING component path")
}

@(test)
modify_rejects_misspelled_transporting_clause_after_value_source :: proc(t: ^testing.T) {
	source := `MODIFY lt_aif_job_header_existing
  FROM VALUE ty_aif_job_header(
    status = c_aif_job_status-created
    jobname = VALUE #( )
    jobcount = VALUE #( )
    modified_time = lv_modify_timestamp
    modified_user = sy-uname
  )
  TRANNSPORTING status jobname jobcount modified_time modified_user.`
	parsed := parse(source, "modify_transporting_typo.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 1)
	testing.expect_value(t, parsed.errors[0].message, MODIFY_TRANSPORTING_KEYWORD_MESSAGE)
	testing.expect_value(
		t,
		source[parsed.errors[0].range.start:parsed.errors[0].range.end],
		"TRANNSPORTING",
	)
	modify := parsed.root.stmts[0].derived_stmt.(^ast.Modify_Stmt)
	testing.expect_value(t, len(modify.transporting), 0)
}

@(test)
modify_rejects_unexpected_tokens_between_structural_additions :: proc(t: ^testing.T) {
	source := `MODIFY lt_aif_job_header_existing
  FROM VALUE ty_aif_job_header(
    status = c_aif_job_status-created
    jobname = VALUE #( )
    jobcount = VALUE #( )
    modified_time = lv_modify_timestamp
    modified_user = sy-uname
  )fsdfz
  TRaaANSPORTINGsdfs status jobname jobcount modified_time modified_user.`
	parsed := parse(source, "modify_unexpected_additions.abap", context.allocator)

	testing.expect(t, len(parsed.errors) >= 2)
	testing.expect_value(t, parsed.errors[0].message, MODIFY_UNEXPECTED_TOKEN_MESSAGE)
	testing.expect_value(t, source[parsed.errors[0].range.start:parsed.errors[0].range.end], "fsdfz")
	testing.expect_value(t, parsed.errors[1].message, MODIFY_UNEXPECTED_TOKEN_MESSAGE)
	testing.expect_value(
		t,
		source[parsed.errors[1].range.start:parsed.errors[1].range.end],
		"TRaaANSPORTINGsdfs",
	)
}

@(test)
sort_by_keeps_component_names :: proc(t: ^testing.T) {
	source := `SORT cs_webi-pvepparameter BY vepname version function vepparam vepparamtype.`
	parsed := parse(source, "sort_components.abap", context.allocator)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Sort_Stmt)
	names := [?]string{"vepname", "version", "function", "vepparam", "vepparamtype"}

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, len(stmt.fields), len(names))
	for name, i in names {
		testing.expect_value(t, stmt.fields[i].name.text, name)
	}
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
sort_stable_after_target_keeps_clause :: proc(t: ^testing.T) {
	source := `SORT itab STABLE BY field.`
	parsed := parse(source, "sort_stable.abap", context.allocator)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Sort_Stmt)
	target := stmt.target.derived_expr.(^ast.Ident_Expr)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect(t, stmt.stable)
	testing.expect_value(t, target.name, "itab")
	testing.expect_value(t, len(stmt.fields), 1)
	testing.expect_value(t, stmt.fields[0].name.text, "field")
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
sort_by_keeps_nested_component_exprs_and_modifiers :: proc(t: ^testing.T) {
	source := `SORT rs_component-view_metadata BY definition-component_name ASCENDING definition-view_name ASCENDING.`
	parsed := parse(source, "sort_nested_components.abap", context.allocator)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Sort_Stmt)
	first := stmt.fields[0].expr.derived_expr.(^ast.Selector_Expr)
	first_base := first.base.derived_expr.(^ast.Ident_Expr)
	first_field := first.field.derived_expr.(^ast.Ident_Expr)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, len(stmt.fields), 2)
	testing.expect_value(t, stmt.fields[0].name.text, "definition-component_name")
	testing.expect_value(t, stmt.fields[1].name.text, "definition-view_name")
	testing.expect_value(t, first_base.name, "definition")
	testing.expect_value(t, first_field.name, "component_name")
	testing.expect(t, stmt.fields[0].ascending)
	testing.expect(t, stmt.fields[1].ascending)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
delete_db_table_from_table_is_parser_modeled :: proc(t: ^testing.T) {
	source := `DELETE zdelete_tab FROM TABLE lt_rows ##SUBRC_OK.`
	parsed := parse(source, "delete_db_from_table.abap", context.allocator)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Delete_Stmt)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, stmt.form, ast.Delete_Form.Db_Table)
	testing.expect(t, stmt.from_table)
	testing.expect(t, stmt.source != nil)
	testing.expect_value(t, source[stmt.db_source_range.start:stmt.db_source_range.end], "zdelete_tab")
}

@(test)
append_initial_line_keeps_append_shape :: proc(t: ^testing.T) {
	source := `APPEND INITIAL LINE TO lt_stab ASSIGNING <ls_stab>.`
	parsed := parse(source, "append_initial_line.abap", context.allocator)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Append_Stmt)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect(t, stmt.initial_line)
	testing.expect(t, stmt.source == nil)
	testing.expect(t, stmt.target != nil)
	testing.expect(t, stmt.assigning != nil)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
append_initial_line_allows_split_inline_assigning :: proc(t: ^testing.T) {
	source := `APPEND INITIAL LINE TO lt_stab ASSIGNING
FIELD-SYMBOL(<ls_stab>).`
	parsed := parse(source, "append_initial_line_inline_assigning.abap", context.allocator)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Append_Stmt)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect(t, stmt.initial_line)
	testing.expect(t, stmt.assigning != nil)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), `APPEND INITIAL LINE TO lt_stab ASSIGNING FIELD-SYMBOL(<ls_stab>).`)
}

@(test)
unterminated_append_constructor_recovers_before_following_statement :: proc(t: ^testing.T) {
	source := `APPEND VALUE #( field = 'hello'
SELECT *
  FROM ztab
  INTO TABLE @DATA(lt_rows)
  WHERE status = 'I'.
LOOP AT lt_rows INTO DATA(ls_row).
  UPDATE ztab
    SET status = 'R'
    WHERE job_id = @ls_row-job_id.
ENDLOOP.`
	parsed := parse(source, "append_constructor_recovery.abap", context.allocator)
	counts := count_nodes(parsed.root)

	expect_error_contains(t, parsed, "expected ')' to close constructor expression")
	testing.expect(t, len(parsed.errors) <= 2)
	testing.expect_value(t, counts.select_stmt, 1)
	testing.expect_value(t, counts.loop_stmt, 1)
	testing.expect_value(t, counts.update_stmt, 1)
}

@(test)
insert_initial_line_keeps_insert_shape :: proc(t: ^testing.T) {
	source := `INSERT INITIAL LINE INTO TABLE lt_stab ASSIGNING <ls_stab> INDEX 1.`
	parsed := parse(source, "insert_initial_line.abap", context.allocator)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Insert_Stmt)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect(t, stmt.initial_line)
	testing.expect(t, stmt.source == nil)
	testing.expect(t, stmt.target != nil)
	testing.expect(t, stmt.assigning != nil)
	testing.expect(t, stmt.index != nil)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), `INSERT INITIAL LINE INTO TABLE lt_stab INDEX 1 ASSIGNING <ls_stab>.`)
}

@(test)
data_access_statements_keep_concrete_fields :: proc(t: ^testing.T) {
	source := `READ TABLE itab INTO DATA(row) WITH KEY id = lv_id TRANSPORTING NO FIELDS.
INSERT wa INTO TABLE itab INDEX idx ASSIGNING FIELD-SYMBOL(<row>).
UPDATE mara SET matnr = lv_new WHERE matnr = lv_old.
DELETE ADJACENT DUPLICATES FROM itab COMPARING matnr.`
	parsed := parse(source, "data_access_fields.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	read := parsed.root.stmts[0].derived_stmt.(^ast.Read_Table_Stmt)
	insert := parsed.root.stmts[1].derived_stmt.(^ast.Insert_Stmt)
	update := parsed.root.stmts[2].derived_stmt.(^ast.Update_Stmt)
	delete_stmt := parsed.root.stmts[3].derived_stmt.(^ast.Delete_Stmt)

	testing.expect_value(t, len(read.entries), 1)
	testing.expect(t, read.entries[0].table != nil)
	testing.expect(t, read.entries[0].into != nil)
	testing.expect_value(t, read.entries[0].key_kind, ast.Read_Table_Key_Kind.Key)
	testing.expect_value(t, len(read.entries[0].key_values), 1)
	testing.expect(t, read.entries[0].transporting_no_fields)
	testing.expect_value(t, insert.form, ast.Insert_Form.Internal_Table)
	testing.expect(t, insert.source != nil)
	testing.expect(t, insert.target != nil)
	testing.expect(t, insert.index != nil)
	testing.expect(t, insert.assigning != nil)
	testing.expect_value(t, len(update.assignments), 1)
	testing.expect(t, update.where_cond != nil)
	testing.expect_value(t, delete_stmt.form, ast.Delete_Form.Adjacent_Duplicates)
	testing.expect(t, delete_stmt.target != nil)
	testing.expect_value(t, len(delete_stmt.comparing), 1)
}

@(test)
using_key_selectors_keep_static_names_and_dynamic_exprs :: proc(t: ^testing.T) {
	source := `READ TABLE lt_rows INTO ls_row INDEX lv_idx USING KEY array_index.
READ TABLE lt_rows INTO ls_row INDEX lv_idx USING KEY (lv_key).
DELETE lt_rows USING KEY sec_from WHERE id = lv_id.`
	parsed := parse(source, "using_key_selectors.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	static_read := parsed.root.stmts[0].derived_stmt.(^ast.Read_Table_Stmt)
	dynamic_read := parsed.root.stmts[1].derived_stmt.(^ast.Read_Table_Stmt)
	delete_stmt := parsed.root.stmts[2].derived_stmt.(^ast.Delete_Stmt)

	testing.expect_value(t, static_read.entries[0].using_key.name.text, "array_index")
	testing.expect_value(
		t,
		source[static_read.entries[0].using_key.name.range.start:static_read.entries[0].using_key.name.range.end],
		"array_index",
	)
	testing.expect(t, static_read.entries[0].using_key.dynamic_name == nil)
	testing.expect_value(t, dynamic_read.entries[0].using_key.name.text, "")
	testing.expect(t, dynamic_read.entries[0].using_key.dynamic_name != nil)
	testing.expect_value(t, delete_stmt.using_key.name.text, "sec_from")
}

@(test)
using_key_requires_key_and_selector :: proc(t: ^testing.T) {
	source := `READ TABLE lt_rows USING array_index.
DELETE lt_rows USING KEY WHERE id = lv_id.`
	parsed := parse(source, "using_key_invalid.abap", context.allocator)

	testing.expect(t, len(parsed.errors) >= 2)
}

@(test)
read_table_and_delete_model_pseudo_components :: proc(t: ^testing.T) {
	source := `READ TABLE itab WITH KEY table_line = '*' TRANSPORTING NO FIELDS.
DELETE ADJACENT DUPLICATES FROM itab COMPARING ALL FIELDS.`
	parsed := parse(source, "data_access_pseudo_components.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	read := parsed.root.stmts[0].derived_stmt.(^ast.Read_Table_Stmt)
	delete_stmt := parsed.root.stmts[1].derived_stmt.(^ast.Delete_Stmt)

	testing.expect_value(t, len(read.entries[0].key_values), 1)
	key := read.entries[0].key_values[0]
	testing.expect_value(t, key.name.text, "table_line")
	testing.expect_value(t, source[key.name.range.start:key.name.range.end], "table_line")
	testing.expect(t, key.table_line)
	testing.expect_value(t, len(delete_stmt.comparing), 1)
	testing.expect(t, delete_stmt.comparing[0].all_fields)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
read_table_table_key_components_keeps_key_name :: proc(t: ^testing.T) {
	source := `READ TABLE gt_lxe_lang_cache INTO ls_lang WITH TABLE KEY iso2 COMPONENTS langshort = iv_src.`
	parsed := parse(source, "read_table_key_components.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	read := parsed.root.stmts[0].derived_stmt.(^ast.Read_Table_Stmt)

	testing.expect_value(t, len(read.entries), 1)
	entry := read.entries[0]
	testing.expect_value(t, entry.key_kind, ast.Read_Table_Key_Kind.Table_Key)
	testing.expect_value(t, entry.key_name.text, "iso2")
	testing.expect_value(t, len(entry.key_values), 1)
	testing.expect_value(t, entry.key_values[0].name.text, "langshort")
	testing.expect_value(t, source[entry.key_values[0].name.range.start:entry.key_values[0].name.range.end], "langshort")
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
read_table_transporting_keeps_component_paths :: proc(t: ^testing.T) {
	source := `READ TABLE lt_rows INTO ls_row INDEX lv_idx TRANSPORTING id nested-part.`
	parsed := parse(source, "read_table_transporting.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	read := parsed.root.stmts[0].derived_stmt.(^ast.Read_Table_Stmt)
	entry := read.entries[0]

	testing.expect(t, !entry.transporting_no_fields)
	testing.expect_value(t, len(entry.transporting_fields), 2)
	testing.expect_value(t, entry.transporting_fields[0].name.text, "id")
	testing.expect_value(t, len(entry.transporting_fields[0].path), 1)
	testing.expect_value(t, entry.transporting_fields[1].name.text, "nested-part")
	testing.expect_value(t, len(entry.transporting_fields[1].path), 2)
	testing.expect_value(t, entry.transporting_fields[1].path[0].name.text, "nested")
	testing.expect_value(t, entry.transporting_fields[1].path[1].name.text, "part")
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
read_table_key_keeps_nested_component_path :: proc(t: ^testing.T) {
	source := `READ TABLE rt_item_status REFERENCE INTO lr_item_status WITH KEY item-obj_type = ls_item_status-item-obj_type item-obj_name = ls_item_status-item-obj_name.`
	parsed := parse(source, "read_table_nested_key.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	read := parsed.root.stmts[0].derived_stmt.(^ast.Read_Table_Stmt)
	entry := read.entries[0]

	testing.expect_value(t, len(entry.key_values), 2)
	key := entry.key_values[1]
	testing.expect_value(t, key.name.text, "item-obj_name")
	testing.expect_value(t, len(key.path), 2)
	testing.expect_value(t, key.path[0].name.text, "item")
	testing.expect_value(t, key.path[1].name.text, "obj_name")
	testing.expect_value(t, source[key.path[1].name.range.start:key.path[1].name.range.end], "obj_name")
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
read_table_key_keeps_table_line_reference_path :: proc(t: ^testing.T) {
	source := `READ TABLE lt_permissions WITH KEY table_line->package_interface_name = lv_name TRANSPORTING NO FIELDS.`
	parsed := parse(source, "read_table_table_line_ref_key.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	read := parsed.root.stmts[0].derived_stmt.(^ast.Read_Table_Stmt)
	key := read.entries[0].key_values[0]

	testing.expect_value(t, key.name.text, "table_line->package_interface_name")
	testing.expect_value(t, len(key.path), 2)
	testing.expect_value(t, key.path[0].name.text, "table_line")
	testing.expect_value(t, key.path[0].selector, ast.Selector_Op.Dash)
	testing.expect_value(t, key.path[1].name.text, "package_interface_name")
	testing.expect_value(t, key.path[1].selector, ast.Selector_Op.Arrow)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
read_table_key_keeps_dynamic_component_name :: proc(t: ^testing.T) {
	source := `READ TABLE <lt_tree> WITH KEY ('NODENAME') = ms_item-obj_name ASSIGNING <ls_tree>.`
	parsed := parse(source, "read_table_dynamic_key.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	read := parsed.root.stmts[0].derived_stmt.(^ast.Read_Table_Stmt)
	key := read.entries[0].key_values[0]

	testing.expect(t, key.is_dynamic)
	testing.expect_value(t, key.name.text, "('NODENAME')")
	testing.expect(t, key.dynamic_name != nil)
	testing.expect_value(
		t,
		ast.print_node(parsed.root, context.allocator),
		`READ TABLE <lt_tree> ASSIGNING <ls_tree> WITH KEY ('NODENAME') = ms_item-obj_name.`,
	)
}

@(test)
read_table_rejects_spaced_key_component_selector :: proc(t: ^testing.T) {
	source := `READ TABLE itab WITH KEY item - obj_name = value.`
	parsed := parse(source, "read_table_bad_key_path.abap", context.allocator)

	testing.expect(t, len(parsed.errors) > 0)
	testing.expect_value(t, parsed.errors[0].message, "syntax error: expected READ TABLE key component path")
}

@(test)
read_table_index_keeps_digit_prefixed_symbol_operand :: proc(t: ^testing.T) {
	source := `READ TABLE lt_rows INDEX 1sdf INTO DATA(ls_row).`
	parsed := parse(source, "read_table_bad_index.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)

	read := parsed.root.stmts[0].derived_stmt.(^ast.Read_Table_Stmt)
	testing.expect_value(t, len(read.entries), 1)
	index, index_ok := read.entries[0].index.derived_expr.(^ast.Ident_Expr)
	testing.expect(t, index_ok)
	testing.expect_value(t, index.name, "1sdf")
	testing.expect_value(t, source[index.range.start:index.range.end], "1sdf")
	testing.expect(t, read.entries[0].into != nil)
}

@(test)
read_table_binary_search_stores_range :: proc(t: ^testing.T) {
	source := `READ TABLE itab INTO wa WITH KEY id = lv_id BINARY SEARCH.`
	parsed := parse(source, "read_table_binary_search_clause.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	read := parsed.root.stmts[0].derived_stmt.(^ast.Read_Table_Stmt)
	testing.expect_value(t, len(read.entries), 1)
	entry := read.entries[0]
	testing.expect(t, entry.binary_search)
	testing.expect_value(t, source[entry.binary_search_clause.start:entry.binary_search_clause.end], "BINARY SEARCH")
}

@(test)
open_sql_dml_update_facts_are_parser_modeled :: proc(t: ^testing.T) {
	source := `UPDATE zupdate_tab FROM TABLE lt_rows CLIENT SPECIFIED CONNECTION con.
UPDATE (lv_tab) SET status = @lv_status, changed_at = sy-datum WHERE (lv_where).`
	parsed := parse(source, "dml_update_facts.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	from_update := parsed.root.stmts[0].derived_stmt.(^ast.Update_Stmt)
	set_update := parsed.root.stmts[1].derived_stmt.(^ast.Update_Stmt)

	testing.expect(t, from_update.target != nil)
	testing.expect_value(t, source[from_update.db_source_range.start:from_update.db_source_range.end], "zupdate_tab")
	testing.expect(t, from_update.from_table)
	testing.expect(t, from_update.source != nil)
	testing.expect_value(t, source[from_update.client_clause.start:from_update.client_clause.end], "CLIENT SPECIFIED")
	testing.expect_value(t, source[from_update.connection_clause.start:from_update.connection_clause.end], "CONNECTION con")

	testing.expect(t, set_update.dynamic_source)
	testing.expect_value(t, source[set_update.db_source_range.start:set_update.db_source_range.end], "(lv_tab)")
	testing.expect_value(t, source[set_update.set_clause.start:set_update.set_clause.end], "SET status = @lv_status, changed_at = sy-datum")
	testing.expect_value(t, len(set_update.assignments), 2)
	testing.expect_value(t, set_update.assignments[0].column_name.text, "status")
	testing.expect_value(t, source[set_update.assignments[0].column_name.range.start:set_update.assignments[0].column_name.range.end], "status")
	testing.expect_value(t, set_update.assignments[1].column_name.text, "changed_at")
	testing.expect(t, set_update.dynamic_where)
	testing.expect_value(t, source[set_update.where_clause.start:set_update.where_clause.end], "WHERE (lv_where)")
}

@(test)
open_sql_update_accepts_blank_separated_set_assignments :: proc(t: ^testing.T) {
	source := `UPDATE zattp_cmo_portal SET status          = ls_cmo_portal-status
                               submit          = ls_cmo_portal-submit
                               evtid           = ls_cmo_portal-evtid
* keep interleaved comments out of the assignment stream
                               contract_number = ls_cmo_portal-contract_number
                         WHERE trnid           = ls_cmo_portal-trnid
                           AND legisl_del      = iv_legislation.`
	parsed := parse(source, "update_blank_set_assignments.abap", context.allocator)
	update := parsed.root.stmts[0].derived_stmt.(^ast.Update_Stmt)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, len(update.assignments), 4)
	testing.expect_value(t, update.assignments[1].column_name.text, "submit")
	testing.expect_value(t, update.assignments[3].column_name.text, "contract_number")
	testing.expect(t, update.where_cond != nil)
}

@(test)
open_sql_dml_delete_modify_and_insert_forms_are_parser_modeled :: proc(t: ^testing.T) {
	source := `DELETE FROM zdelete_tab WHERE (lv_where) CONNECTION con.
DELETE lt_rows WHERE objid = lv_objid.
MODIFY zmodify_tab FROM ls_row WHERE (lv_where) CLIENT SPECIFIED.
MODIFY TABLE lt_rows FROM ls_row TRANSPORTING col WHERE col = lv_col.
INSERT zinsert_tab FROM TABLE lt_rows ACCEPTING DUPLICATE KEYS.
INSERT INTO zinto_tab VALUES ls_row.
INSERT ls_row INTO TABLE lt_rows INDEX lv_idx.
INSERT REPORT prog FROM source.
INSERT TEXTPOOL prog FROM pool LANGUAGE lang.`
	parsed := parse(source, "dml_forms.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	db_delete := parsed.root.stmts[0].derived_stmt.(^ast.Delete_Stmt)
	itab_delete := parsed.root.stmts[1].derived_stmt.(^ast.Delete_Stmt)
	db_modify := parsed.root.stmts[2].derived_stmt.(^ast.Modify_Stmt)
	itab_modify := parsed.root.stmts[3].derived_stmt.(^ast.Modify_Stmt)
	bare_insert := parsed.root.stmts[4].derived_stmt.(^ast.Insert_Stmt)
	into_insert := parsed.root.stmts[5].derived_stmt.(^ast.Insert_Stmt)
	itab_insert := parsed.root.stmts[6].derived_stmt.(^ast.Insert_Stmt)
	report_insert := parsed.root.stmts[7].derived_stmt.(^ast.Report_Stmt)
	textpool_insert := parsed.root.stmts[8].derived_stmt.(^ast.Textpool_Stmt)

	testing.expect_value(t, db_delete.form, ast.Delete_Form.Db_Table)
	testing.expect(t, db_delete.explicit_from)
	testing.expect(t, db_delete.dynamic_where)
	testing.expect_value(t, source[db_delete.db_source_range.start:db_delete.db_source_range.end], "zdelete_tab")
	testing.expect_value(t, itab_delete.form, ast.Delete_Form.Internal_Table)
	testing.expect(t, !itab_delete.explicit_from)
	testing.expect(t, itab_delete.where_cond != nil)
	testing.expect(t, db_modify.target != nil)
	testing.expect(t, db_modify.dynamic_where)
	testing.expect_value(t, source[db_modify.client_clause.start:db_modify.client_clause.end], "CLIENT SPECIFIED")
	testing.expect(t, itab_modify.table_keyword)
	testing.expect_value(t, len(itab_modify.transporting), 1)
	testing.expect_value(t, itab_modify.transporting[0].name.text, "col")
	testing.expect_value(t, len(itab_modify.transporting[0].path), 1)
	testing.expect_value(t, bare_insert.form, ast.Insert_Form.Db_Table)
	testing.expect(t, bare_insert.target != nil)
	testing.expect_value(t, source[bare_insert.db_source_range.start:bare_insert.db_source_range.end], "zinsert_tab")
	testing.expect(t, bare_insert.from_table)
	testing.expect(t, bare_insert.accepting_duplicate_keys)
	testing.expect_value(t, into_insert.form, ast.Insert_Form.Db_Table)
	testing.expect(t, into_insert.into_db_table)
	testing.expect(t, into_insert.values_clause)
	testing.expect_value(t, into_insert.db_table_name.text, "zinto_tab")
	testing.expect_value(t, itab_insert.form, ast.Insert_Form.Internal_Table)
	testing.expect(t, !itab_insert.has_db_table_name)
	testing.expect_value(t, report_insert.kind, ast.Report_Kind.Insert_Report)
	testing.expect_value(t, textpool_insert.kind, ast.Textpool_Kind.Insert)
}

@(test)
open_sql_dml_duplicate_clauses_do_not_overwrite_first_facts :: proc(t: ^testing.T) {
	source := `UPDATE ztab FROM ls_old SET status = ls_new WHERE id = lv_id WHERE id = lv_other.
DELETE FROM zdelete WHERE id = lv_id WHERE id = lv_other.
INSERT zinsert FROM ls_old FROM ls_new ACCEPTING DUPLICATE KEYS ACCEPTING DUPLICATE KEYS.
MODIFY zmodify FROM ls_old WHERE id = lv_id WHERE id = lv_other.`
	parsed := parse(source, "dml_duplicate_clauses.abap", context.allocator)

	testing.expect(t, len(parsed.errors) >= 4)
	update := parsed.root.stmts[0].derived_stmt.(^ast.Update_Stmt)
	delete_stmt := parsed.root.stmts[1].derived_stmt.(^ast.Delete_Stmt)
	insert := parsed.root.stmts[2].derived_stmt.(^ast.Insert_Stmt)
	modify := parsed.root.stmts[3].derived_stmt.(^ast.Modify_Stmt)

	testing.expect(t, update.source != nil)
	testing.expect_value(t, len(update.assignments), 0)
	testing.expect_value(t, source[delete_stmt.where_clause.start:delete_stmt.where_clause.end], "WHERE id = lv_id")
	testing.expect_value(t, source[insert.from_clause.start:insert.from_clause.end], "FROM ls_old")
	testing.expect_value(t, source[insert.accepting_clause.start:insert.accepting_clause.end], "ACCEPTING DUPLICATE KEYS")
	testing.expect_value(t, source[modify.where_clause.start:modify.where_clause.end], "WHERE id = lv_id")
}

@(test)
cursor_dataset_report_and_textpool_fields :: proc(t: ^testing.T) {
	source := `SELECT SINGLE matnr FROM mara INTO @DATA(lv_matnr) WHERE matnr = @lv_key.
OPEN CURSOR WITH HOLD cv FOR SELECT matnr FROM mara WHERE matnr = lv_key.
FETCH NEXT CURSOR cv INTO TABLE lt_mara PACKAGE SIZE lv_size.
CLOSE CURSOR cv.
OPEN DATASET file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT AT POSITION pos MESSAGE msg.
READ DATASET file INTO text MAXIMUM LENGTH max ACTUAL LENGTH DATA(len).
TRANSFER text TO file LENGTH len.
REPORT zrep.
READ REPORT prog INTO source.
INSERT TEXTPOOL prog FROM pool LANGUAGE lang.`
	parsed := parse(source, "surface_fields.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	select_stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	open_cursor := parsed.root.stmts[1].derived_stmt.(^ast.Open_Cursor_Stmt)
	fetch := parsed.root.stmts[2].derived_stmt.(^ast.Fetch_Stmt)
	close_cursor := parsed.root.stmts[3].derived_stmt.(^ast.Close_Cursor_Stmt)
	open_dataset := parsed.root.stmts[4].derived_stmt.(^ast.Dataset_Stmt)
	read_dataset := parsed.root.stmts[5].derived_stmt.(^ast.Dataset_Stmt)
	transfer := parsed.root.stmts[6].derived_stmt.(^ast.Dataset_Stmt)
	report := parsed.root.stmts[7].derived_stmt.(^ast.Report_Stmt)
	read_report := parsed.root.stmts[8].derived_stmt.(^ast.Report_Stmt)
	textpool := parsed.root.stmts[9].derived_stmt.(^ast.Textpool_Stmt)

	testing.expect(t, select_stmt.query.single)
	testing.expect(t, select_stmt.query.source != nil)
	testing.expect(t, select_stmt.query.result != nil)
	testing.expect(t, select_stmt.query.where_cond != nil)
	testing.expect(t, open_cursor.with_hold)
	testing.expect(t, open_cursor.handle != nil)
	testing.expect(t, open_cursor.query.source != nil)
	testing.expect(t, fetch.handle != nil)
	testing.expect(t, fetch.result != nil)
	testing.expect(t, fetch.result.table)
	testing.expect(t, fetch.package_size != nil)
	testing.expect(t, close_cursor.handle != nil)
	testing.expect_value(t, open_dataset.kind, ast.Dataset_Kind.Open)
	testing.expect_value(t, open_dataset.access, ast.Dataset_Open_Access.Output)
	testing.expect(t, .Text_Mode in open_dataset.flags)
	testing.expect_value(t, open_dataset.encoding, "DEFAULT")
	testing.expect(t, open_dataset.position != nil)
	testing.expect(t, open_dataset.message != nil)
	testing.expect_value(t, read_dataset.kind, ast.Dataset_Kind.Read)
	testing.expect(t, read_dataset.target != nil)
	testing.expect(t, read_dataset.maximum_length != nil)
	testing.expect(t, read_dataset.actual_length != nil)
	testing.expect_value(t, transfer.kind, ast.Dataset_Kind.Transfer)
	testing.expect(t, transfer.source != nil)
	testing.expect(t, transfer.length != nil)
	testing.expect_value(t, report.kind, ast.Report_Kind.Report)
	testing.expect(t, report.name != nil)
	testing.expect_value(t, read_report.kind, ast.Report_Kind.Read_Report)
	testing.expect(t, read_report.source != nil)
	testing.expect_value(t, textpool.kind, ast.Textpool_Kind.Insert)
	testing.expect(t, textpool.table != nil)
	testing.expect(t, textpool.language != nil)
}

@(test)
dataset_open_read_close_full_forms :: proc(t: ^testing.T) {
	source := `OPEN DATASET lv_filename FOR INPUT IN TEXT MODE ENCODING UTF-8
             MESSAGE lv_message IGNORING CONVERSION ERRORS REPLACEMENT CHARACTER lv_repl.
OPEN DATASET lv_legacy FOR APPENDING IN LEGACY TEXT MODE CODE PAGE lv_code_page
             LITTLE ENDIAN WITH SMART LINEFEED TYPE lv_attr FILTER lv_filter.
READ DATASET lv_filename INTO lv_line MAXIMUM LENGTH lv_max ACTUAL LENGTH DATA(lv_length).
READ DATASET lv_filename INTO lv_line LENGTH lv_length.
CLOSE DATASET lv_filename.`
	parsed := parse(source, "dataset_full_forms.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	open := parsed.root.stmts[0].derived_stmt.(^ast.Dataset_Stmt)
	legacy_open := parsed.root.stmts[1].derived_stmt.(^ast.Dataset_Stmt)
	read_actual := parsed.root.stmts[2].derived_stmt.(^ast.Dataset_Stmt)
	read_length := parsed.root.stmts[3].derived_stmt.(^ast.Dataset_Stmt)
	close := parsed.root.stmts[4].derived_stmt.(^ast.Dataset_Stmt)

	testing.expect_value(t, open.kind, ast.Dataset_Kind.Open)
	testing.expect_value(t, open.access, ast.Dataset_Open_Access.Input)
	testing.expect(t, .Text_Mode in open.flags)
	testing.expect_value(t, open.encoding, "UTF-8")
	testing.expect(t, open.message != nil)
	testing.expect(t, .Ignoring_Conversion_Errors in open.flags)
	testing.expect(t, open.replacement != nil)

	testing.expect_value(t, legacy_open.kind, ast.Dataset_Kind.Open)
	testing.expect_value(t, legacy_open.access, ast.Dataset_Open_Access.Append)
	testing.expect(t, .Legacy_Mode in legacy_open.flags)
	testing.expect(t, .Text_Mode in legacy_open.flags)
	testing.expect(t, legacy_open.code_page != nil)
	testing.expect_value(t, legacy_open.endian, ast.Dataset_Endian.Little)
	testing.expect_value(t, legacy_open.linefeed_mode, ast.Dataset_Linefeed_Mode.Smart)
	testing.expect(t, legacy_open.file_type != nil)
	testing.expect(t, legacy_open.filter != nil)

	testing.expect_value(t, read_actual.kind, ast.Dataset_Kind.Read)
	testing.expect(t, read_actual.target != nil)
	testing.expect(t, read_actual.maximum_length != nil)
	testing.expect(t, read_actual.actual_length != nil)
	testing.expect_value(t, read_length.kind, ast.Dataset_Kind.Read)
	testing.expect(t, read_length.length != nil)
	testing.expect_value(t, close.kind, ast.Dataset_Kind.Close)
	testing.expect(t, close.dataset != nil)
}

@(test)
dataset_open_read_close_sample_preserves_source_facts :: proc(t: ^testing.T) {
	source := `OPEN DATASET lv_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT
                             MESSAGE lv_message IGNORING CONVERSION ERRORS.

    DO.
      READ DATASET lv_filename INTO lv_line.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      CHECK NOT lv_line IS INITIAL.
      APPEND lv_line TO gt_raw.
    ENDDO.

    CLOSE DATASET lv_filename.`
	parsed := parse(source, "dataset_sample.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	open := parsed.root.stmts[0].derived_stmt.(^ast.Dataset_Stmt)
	do_stmt := parsed.root.stmts[1].derived_stmt.(^ast.Do_Stmt)
	read := do_stmt.body[0].derived_stmt.(^ast.Dataset_Stmt)
	close := parsed.root.stmts[2].derived_stmt.(^ast.Dataset_Stmt)

	testing.expect_value(t, open.kind, ast.Dataset_Kind.Open)
	testing.expect_value(t, source[open.range.start:open.range.end], `OPEN DATASET lv_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT
                             MESSAGE lv_message IGNORING CONVERSION ERRORS.`)
	testing.expect_value(t, source[open.dataset.range.start:open.dataset.range.end], "lv_filename")
	testing.expect_value(t, open.access, ast.Dataset_Open_Access.Input)
	testing.expect(t, .Text_Mode in open.flags)
	testing.expect_value(t, open.encoding, "DEFAULT")
	testing.expect(t, open.message != nil)
	testing.expect_value(t, source[open.message.range.start:open.message.range.end], "lv_message")
	testing.expect(t, .Ignoring_Conversion_Errors in open.flags)

	testing.expect_value(t, read.kind, ast.Dataset_Kind.Read)
	testing.expect_value(t, source[read.dataset.range.start:read.dataset.range.end], "lv_filename")
	testing.expect(t, read.target != nil)
	testing.expect_value(t, source[read.target.range.start:read.target.range.end], "lv_line")
	testing.expect_value(t, close.kind, ast.Dataset_Kind.Close)
	testing.expect_value(t, source[close.range.start:close.range.end], "CLOSE DATASET lv_filename.")
	testing.expect_value(t, source[close.dataset.range.start:close.dataset.range.end], "lv_filename")
}

@(test)
dataset_misspelled_clause_keywords_do_not_set_clean_ast_facts :: proc(t: ^testing.T) {
	source := `OPEN DATASET lv_filename FOR INPT IN TEXT MDOE ENCODING DEFAULT
             MESSAGE lv_message IGNORING CNVERSION ERRORS
             REPLACEMENT CHAR lv_repl CODE PG lv_code_page
             LITTLE ENDAN WITH SMART LINFEED SKIPPING BYTE-ORDER MRK.
READ DATASET lv_filename INTO lv_line MAXIMUM LENGHT lv_max ACTUAL LENGHT lv_actual.
CLOSE DATASET lv_filename.`
	parsed := parse(source, "dataset_typos.abap", context.allocator)

	testing.expect_value(t, parse_error_message_count(parsed.errors, "syntax error: expected INPUT, OUTPUT, APPENDING, or UPDATE after OPEN DATASET FOR"), 1)
	testing.expect_value(t, parse_error_message_count(parsed.errors, "syntax error: expected MODE after OPEN DATASET IN TEXT"), 1)
	testing.expect_value(t, parse_error_message_count(parsed.errors, "syntax error: expected CONVERSION after OPEN DATASET IGNORING"), 1)
	testing.expect_value(t, parse_error_message_count(parsed.errors, "syntax error: expected CHARACTER after OPEN DATASET REPLACEMENT"), 1)
	testing.expect_value(t, parse_error_message_count(parsed.errors, "syntax error: expected PAGE after OPEN DATASET CODE"), 1)
	testing.expect_value(t, parse_error_message_count(parsed.errors, "syntax error: expected ENDIAN after OPEN DATASET LITTLE"), 1)
	testing.expect_value(t, parse_error_message_count(parsed.errors, "syntax error: expected LINEFEED after OPEN DATASET WITH linefeed mode"), 1)
	testing.expect_value(t, parse_error_message_count(parsed.errors, "syntax error: expected MARK after OPEN DATASET SKIPPING BYTE-ORDER"), 1)
	testing.expect_value(t, parse_error_message_count(parsed.errors, "syntax error: expected LENGTH after READ DATASET MAXIMUM"), 1)
	testing.expect_value(t, parse_error_message_count(parsed.errors, "syntax error: expected LENGTH after READ DATASET ACTUAL"), 1)

	open := parsed.root.stmts[0].derived_stmt.(^ast.Dataset_Stmt)
	read := parsed.root.stmts[1].derived_stmt.(^ast.Dataset_Stmt)
	close := parsed.root.stmts[2].derived_stmt.(^ast.Dataset_Stmt)

	testing.expect_value(t, open.access, ast.Dataset_Open_Access.Default)
	testing.expect(t, !(.Text_Mode in open.flags))
	testing.expect_value(t, open.encoding, "DEFAULT")
	testing.expect(t, open.message != nil)
	testing.expect(t, !(.Ignoring_Conversion_Errors in open.flags))
	testing.expect(t, open.replacement == nil)
	testing.expect(t, open.code_page == nil)
	testing.expect_value(t, open.endian, ast.Dataset_Endian.Default)
	testing.expect_value(t, open.linefeed_mode, ast.Dataset_Linefeed_Mode.Default)
	testing.expect_value(t, open.byte_order_mark, ast.Dataset_Byte_Order_Mark.Default)
	testing.expect(t, read.target != nil)
	testing.expect(t, read.maximum_length == nil)
	testing.expect(t, read.actual_length == nil)
	testing.expect(t, close.dataset != nil)
}

@(test)
dataset_position_and_transfer_flags_are_preserved :: proc(t: ^testing.T) {
	source := `TRANSFER text TO file LENGTH len NO END OF LINE.
GET DATASET file POSITION DATA(pos) ATTRIBUTES DATA(attrs).
SET DATASET file POSITION END OF FILE ATTRIBUTES attrs.
TRUNCATE DATASET file AT CURRENT POSITION.
TRUNCATE DATASET file AT POSITION pos.`
	parsed := parse(source, "dataset_flags.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	transfer := parsed.root.stmts[0].derived_stmt.(^ast.Dataset_Stmt)
	get := parsed.root.stmts[1].derived_stmt.(^ast.Dataset_Stmt)
	set := parsed.root.stmts[2].derived_stmt.(^ast.Dataset_Stmt)
	truncate_current := parsed.root.stmts[3].derived_stmt.(^ast.Dataset_Stmt)
	truncate_position := parsed.root.stmts[4].derived_stmt.(^ast.Dataset_Stmt)

	testing.expect(t, .No_End_Of_Line in transfer.flags)
	testing.expect(t, get.position != nil)
	testing.expect(t, get.attributes != nil)
	testing.expect(t, .Position_End_Of_File in set.flags)
	testing.expect(t, set.attributes != nil)
	testing.expect(t, .At_Current_Position in truncate_current.flags)
	testing.expect(t, truncate_position.position != nil)

	testing.expect_value(t, ast.print_node(transfer, context.allocator), "TRANSFER text TO file LENGTH len NO END OF LINE.")
	testing.expect_value(t, ast.print_node(get, context.allocator), "GET DATASET file POSITION DATA(pos) ATTRIBUTES DATA(attrs).")
	testing.expect_value(t, ast.print_node(set, context.allocator), "SET DATASET file POSITION END OF FILE ATTRIBUTES attrs.")
	testing.expect_value(t, ast.print_node(truncate_current, context.allocator), "TRUNCATE DATASET file AT CURRENT POSITION.")
	testing.expect_value(t, ast.print_node(truncate_position, context.allocator), "TRUNCATE DATASET file AT POSITION pos.")
}

@(test)
read_table_allows_inline_target_on_next_line :: proc(t: ^testing.T) {
	source := `READ TABLE lt_ctg_reprocess INTO
  DATA(ls_ctg_reprocess) WITH KEY msgguid = <fs_idx_tbl>-msgguid.`
	parsed := parse(source, "read_table_split_inline.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Read_Table_Stmt)
	testing.expect(t, stmt.entries[0].into != nil)
	testing.expect_value(t, len(stmt.entries[0].key_values), 1)
}

@(test)
islands_generated_source_and_line_statements_keep_nodes :: proc(t: ^testing.T) {
	source := `EXEC SQL.
  SELECT * FROM mara
ENDEXEC.
GENERATE SUBROUTINE POOL lt_source NAME lv_prog MESSAGE lv_msg LINE lv_line WORD lv_word OFFSET lv_off.
GENERATE DYNPRO lv_prog lv_dynpro.
READ LINE lv_line FIELD VALUE mara-matnr INTO lv_matnr.
MODIFY CURRENT LINE FIELD VALUE mara-matnr INTO lv_matnr.`
	parsed := parse(source, "islands_generated_line.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.exec_sql_stmt, 1)
	testing.expect_value(t, counts.generate_stmt, 2)
	testing.expect_value(t, counts.line_stmt, 2)

	exec_sql := parsed.root.stmts[0].derived_stmt.(^ast.Exec_Sql_Stmt)
	generate_pool := parsed.root.stmts[1].derived_stmt.(^ast.Generate_Stmt)
	generate_dynpro := parsed.root.stmts[2].derived_stmt.(^ast.Generate_Stmt)
	read_line := parsed.root.stmts[3].derived_stmt.(^ast.Line_Stmt)
	modify_line := parsed.root.stmts[4].derived_stmt.(^ast.Line_Stmt)

	testing.expect(t, exec_sql.body != "")
	testing.expect_value(t, generate_pool.kind, ast.Generate_Kind.Subroutine_Pool)
	testing.expect(t, generate_pool.source != nil)
	testing.expect(t, generate_pool.name != nil)
	testing.expect(t, generate_pool.message != nil)
	testing.expect(t, generate_pool.line != nil)
	testing.expect(t, generate_pool.word != nil)
	testing.expect(t, generate_pool.offset != nil)
	testing.expect_value(t, generate_dynpro.kind, ast.Generate_Kind.Dynpro)
	testing.expect(t, generate_dynpro.program != nil)
	testing.expect(t, generate_dynpro.dynpro != nil)
	testing.expect_value(t, read_line.kind, ast.Line_Kind.Read)
	testing.expect(t, read_line.line != nil)
	testing.expect_value(t, len(read_line.fields), 1)
	testing.expect_value(t, modify_line.kind, ast.Line_Kind.Modify)
	testing.expect(t, modify_line.current)
	testing.expect_value(t, len(modify_line.fields), 1)
}

@(test)
open_sql_host_expressions_are_ast_nodes :: proc(t: ^testing.T) {
	source := `SELECT SINGLE matnr FROM mara INTO @DATA(lv_matnr) WHERE matnr = @lv_key.`
	parsed := parse(source, "sql_host.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.host_expr, 2)
	printed := ast.print_node(parsed.root, context.allocator)
	testing.expect_value(t, printed, "SELECT SINGLE matnr FROM mara INTO @DATA(lv_matnr) WHERE matnr = @lv_key.")
}

@(test)
open_sql_projection_source_and_set_fields :: proc(t: ^testing.T) {
	source := `SELECT a~matnr AS material, b~maktx AS text FROM mara AS a INNER JOIN makt AS b ON b~matnr = a~matnr INTO TABLE @lt_rows UNION ALL SELECT matnr FROM zmara INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_shape.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect_value(t, len(stmt.query.projection_clauses), 2)
	testing.expect_value(t, stmt.query.projection_clauses[0].alias.text, "material")
	testing.expect(t, stmt.query.source_clause != nil)
	testing.expect_value(t, stmt.query.source_clause.alias.text, "a")
	testing.expect_value(t, len(stmt.query.source_clause.joins), 1)
	testing.expect_value(t, stmt.query.source_clause.joins[0].kind, ast.Select_Join_Kind.Inner)
	testing.expect_value(t, stmt.query.source_clause.joins[0].alias.text, "b")
	testing.expect(t, stmt.query.source_clause.joins[0].on != nil)
	testing.expect_value(t, len(stmt.query.set_ops), 1)
	testing.expect_value(t, stmt.query.set_ops[0].kind, ast.Select_Set_Kind.Union)
	testing.expect(t, stmt.query.set_ops[0].all)
	printed := ast.print_node(parsed.root, context.allocator)
	testing.expect_value(t, printed, "SELECT a~matnr AS material, b~maktx AS text FROM mara AS a INNER JOIN makt AS b ON b~matnr = a~matnr INTO TABLE @lt_rows UNION ALL SELECT matnr FROM zmara INTO TABLE @lt_rows.")
}

@(test)
open_sql_from_fields_clause_builds_projections :: proc(t: ^testing.T) {
	source := `SELECT
  FROM /sttp/rep_obj_rl AS a
  INNER JOIN /sttp/rep_evt AS b ON a~rep_evtid = b~rep_evtid
  FIELDS a~objid AS objid
  FOR ALL ENTRIES IN @lt_child_obj
  WHERE a~objid = @lt_child_obj-objid
  INTO TABLE @DATA(lt_rep_rel_obj).`
	parsed := parse(source, "sql_from_fields.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect_value(t, len(stmt.query.projection_clauses), 1)
	testing.expect_value(t, stmt.query.projection_clauses[0].alias.text, "objid")
	testing.expect_value(t, len(stmt.query.source_clause.joins), 1)
	testing.expect(t, stmt.query.for_all_entries != nil)
}

@(test)
open_sql_case_projection_parses_as_field :: proc(t: ^testing.T) {
	source := `SELECT rep_evtid,
       ext_ref_id,
       CASE
         WHEN ext_ref_id = 'PRIORITY1' THEN 'X'
         WHEN ext_ref_id = 'EXCLUDED' THEN 'X'
         ELSE ' '
       END AS priority,
       creation_time
  FROM /sttp/rep_evt
  INTO TABLE @DATA(lt_rep_evt)
  WHERE status_rep_evt = @lc_sts_rep.`
	parsed := parse(source, "sql_case_projection.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect_value(t, len(stmt.query.projection_clauses), 4)
	testing.expect_value(t, stmt.query.projection_clauses[2].alias.text, "priority")
}

@(test)
open_sql_aggregate_scalar_selects_do_not_wait_for_endselect :: proc(t: ^testing.T) {
	source := `SELECT COUNT(*)
  FROM zerr AS e
  INNER JOIN /sttp/dm_obj_itm AS i ON i~serno = e~serno
  INTO lv_failed_cnt
  WHERE i~objid = iv_parent
    AND ( e~error_30 = abap_true OR e~error_31 = abap_true ).
SELECT COUNT( DISTINCT rel~evtid ) AS total_events,
       COUNT( DISTINCT evt~evtid ) AS active_events
  FROM /sttp/dm_evt_rel AS rel
  LEFT OUTER JOIN /sttp/dm_evt AS evt ON rel~evtid = evt~evtid
  WHERE rel~objid = @ls_obj_ids-objid
  INTO @DATA(ls_event_summary).
SELECT objid UP TO 1 ROWS
  FROM /sttp/dm_obj_ids
  INTO lv_objid
  WHERE objid = iv_objid.
ENDSELECT.`
	parsed := parse(source, "sql_aggregate_scalar_selects.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, len(parsed.root.stmts), 3)
}

@(test)
open_sql_projection_sql_shapes_are_parser_modeled :: proc(t: ^testing.T) {
	source := `SELECT SINGLE COUNT( DISTINCT rel~evtid ) AS total_events,
       COUNT( ALL evt~evtid ) AS active_events,
       COALESCE( evt~name, @lv_name ) AS event_name,
       rel~*, evt~evtid
  FROM zrel AS rel
  INNER JOIN zevt AS evt ON evt~evtid = rel~evtid
  INTO @DATA(ls_row).`
	parsed := parse(source, "sql_projection_shapes.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect_value(t, len(stmt.query.projection_clauses), 5)

	total := stmt.query.projection_clauses[0].value.derived_expr.(^ast.Sql_Call_Expr)
	testing.expect_value(t, total.kind, ast.Sql_Call_Kind.Aggregate)
	testing.expect_value(t, total.modifier, ast.Sql_Call_Modifier.Distinct)
	testing.expect_value(t, len(total.args), 1)
	total_arg := total.args[0].derived_expr.(^ast.Sql_Column_Expr)
	testing.expect_value(t, total_arg.qualifier.text, "rel")
	testing.expect_value(t, total_arg.name.text, "evtid")

	active := stmt.query.projection_clauses[1].value.derived_expr.(^ast.Sql_Call_Expr)
	testing.expect_value(t, active.kind, ast.Sql_Call_Kind.Aggregate)
	testing.expect_value(t, active.modifier, ast.Sql_Call_Modifier.All)
	testing.expect_value(t, len(active.args), 1)
	active_arg := active.args[0].derived_expr.(^ast.Sql_Column_Expr)
	testing.expect_value(t, active_arg.qualifier.text, "evt")
	testing.expect_value(t, active_arg.name.text, "evtid")

	func := stmt.query.projection_clauses[2].value.derived_expr.(^ast.Sql_Call_Expr)
	testing.expect_value(t, func.kind, ast.Sql_Call_Kind.Function)
	testing.expect_value(t, func.modifier, ast.Sql_Call_Modifier.None)
	testing.expect_value(t, len(func.args), 2)
	_, host_arg := func.args[1].derived_expr.(^ast.Host_Expr)
	testing.expect(t, host_arg)

	star := stmt.query.projection_clauses[3].value.derived_expr.(^ast.Sql_Star_Expr)
	testing.expect_value(t, star.qualifier.text, "rel")
	column := stmt.query.projection_clauses[4].value.derived_expr.(^ast.Sql_Column_Expr)
	testing.expect_value(t, column.qualifier.text, "evt")
	testing.expect_value(t, column.name.text, "evtid")

	join_on := stmt.query.source_clause.joins[0].on.derived_expr.(^ast.Binary_Expr)
	left := join_on.left.derived_expr.(^ast.Sql_Column_Expr)
	right := join_on.right.derived_expr.(^ast.Sql_Column_Expr)
	testing.expect_value(t, left.qualifier.text, "evt")
	testing.expect_value(t, right.qualifier.text, "rel")
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), "SELECT SINGLE COUNT( DISTINCT rel~evtid ) AS total_events, COUNT( ALL evt~evtid ) AS active_events, COALESCE( evt~name, @lv_name ) AS event_name, rel~*, evt~evtid FROM zrel AS rel INNER JOIN zevt AS evt ON evt~evtid = rel~evtid INTO @DATA(ls_row).")
}

@(test)
open_sql_function_arguments_named_like_modifiers_are_not_aggregate_modifiers :: proc(t: ^testing.T) {
	source := `SELECT SINGLE COALESCE( all, @lv_value ) AS value FROM ztab INTO @DATA(ls_row).`
	parsed := parse(source, "sql_function_modifier_words.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	call := stmt.query.projection_clauses[0].value.derived_expr.(^ast.Sql_Call_Expr)
	testing.expect_value(t, call.kind, ast.Sql_Call_Kind.Function)
	testing.expect_value(t, call.modifier, ast.Sql_Call_Modifier.None)
	testing.expect_value(t, len(call.args), 2)
	arg := call.args[0].derived_expr.(^ast.Sql_Column_Expr)
	testing.expect_value(t, arg.name.text, "all")
}

@(test)
open_sql_scalar_select_with_where_owns_endselect_body :: proc(t: ^testing.T) {
	source := `SELECT * FROM snap INTO wa_snap WHERE seqno = '000'.
  CLEAR: x, cnt.
ENDSELECT.`
	parsed := parse(source, "sql_scalar_loop_where.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect_value(t, len(stmt.body), 1)
}

@(test)
open_sql_scalar_select_body_tolerates_catch_system_block :: proc(t: ^testing.T) {
	source := `SELECT * FROM snap INTO wa_snap WHERE seqno = '000'.
  ASSIGN wa_snap-flist(1600) TO <buffer> RANGE wa_snap.
  CLEAR: x, cnt.
  CATCH SYSTEM-EXCEPTIONS conversion_errors = 0 data_access_errors = 0.
    WHILE <buffer>+x(1) <> '%'.
      ADD 1 TO cnt.
    ENDWHILE.
  ENDCATCH.
ENDSELECT.`
	parsed := parse(source, "sql_loop_catch_system.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect_value(t, len(stmt.body), 3)
}

@(test)
open_sql_projection_source_and_result_operands_are_modeled :: proc(t: ^testing.T) {
	source := `SELECT a~matnr AS material, a~* FROM mara AS a INTO CORRESPONDING FIELDS OF TABLE @DATA(lt_rows).
SELECT SINGLE (lv_fields) FROM (lv_table) AS d INTO @lv_target WHERE (lv_where).
SELECT SINGLE matnr FROM @lt_source AS s INTO FIELD-SYMBOL(<row>).`
	parsed := parse(source, "sql_operand_shapes.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	qualified := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect_value(t, len(qualified.query.projection_clauses), 2)
	testing.expect_value(t, qualified.query.projection_clauses[0].alias.text, "material")
	qualified_star := qualified.query.projection_clauses[1].value.derived_expr.(^ast.Sql_Star_Expr)
	testing.expect_value(t, qualified_star.qualifier.text, "a")
	testing.expect_value(t, qualified.query.source_clause.alias.text, "a")
	testing.expect(t, qualified.query.result.table)
	testing.expect(t, qualified.query.result.corresponding_fields)
	_, inline_target := qualified.query.result.target.derived_expr.(^ast.Host_Expr)
	testing.expect(t, inline_target)

	dynamic_stmt := parsed.root.stmts[1].derived_stmt.(^ast.Select_Stmt)
	_, dynamic_projection := dynamic_stmt.query.projection_clauses[0].value.derived_expr.(^ast.Paren_Expr)
	_, dynamic_source := dynamic_stmt.query.source_clause.source.derived_expr.(^ast.Paren_Expr)
	_, dynamic_target := dynamic_stmt.query.result.target.derived_expr.(^ast.Host_Expr)
	testing.expect(t, dynamic_projection)
	testing.expect(t, dynamic_stmt.query.projection_clauses[0].is_dynamic)
	testing.expect(t, dynamic_source)
	testing.expect(t, dynamic_stmt.query.dynamic_where)
	testing.expect(t, dynamic_target)

	host_source := parsed.root.stmts[2].derived_stmt.(^ast.Select_Stmt)
	_, host_source_expr := host_source.query.source_clause.source.derived_expr.(^ast.Host_Expr)
	_, fs_target := host_source.query.result.target.derived_expr.(^ast.Field_Symbol_Inline_Name_Expr)
	testing.expect(t, host_source_expr)
	testing.expect_value(t, host_source.query.source_clause.alias.text, "s")
	testing.expect(t, fs_target)
}

@(test)
open_sql_scalar_row_select_without_endselect_is_diagnosed :: proc(t: ^testing.T) {
	source := `SELECT *
  FROM e070
  WHERE trstatus = 'S'
  INTO @DATA(lt_tbl).

SELECT *
  FROM e070
  WHERE trstatus = 'S'
  INTO lt_tbl.

SELECT *
  FROM e070
  WHERE trstatus = 'S'
   lt_tbl.`
	parsed := parse(source, "sql_missing_endselect.abap", context.allocator)

	testing.expect_value(t, parse_error_message_count(parsed.errors, OPEN_SQL_MISSING_ENDSELECT_MESSAGE), 3)
}

@(test)
open_sql_inline_data_target_requires_host_escape :: proc(t: ^testing.T) {
	source := `SELECT SINGLE *
  FROM e070
  WHERE trstatus = 'S'
  INTO DATA(lt_tbl).`
	parsed := parse(source, "sql_unescaped_inline_target.abap", context.allocator)

	testing.expect_value(t, parse_error_message_count(parsed.errors, OPEN_SQL_INLINE_DATA_TARGET_MESSAGE), 1)
}

@(test)
open_sql_invalid_parenthesized_result_targets_are_diagnosed :: proc(t: ^testing.T) {
	source := `SELECT SINGLE *
  FROM e070
  WHERE trstatus = 'S'
  INTO (lt_tbl).

SELECT SINGLE *
  FROM e070
  WHERE trstatus = 'S'
  INTO (lt_tbl.

SELECT SINGLE *
  FROM e070
  WHERE trstatus = 'S'
  INTO lt_tbl).`
	parsed := parse(source, "sql_invalid_result_targets.abap", context.allocator)

	testing.expect_value(t, parse_error_message_count(parsed.errors, OPEN_SQL_RESULT_TARGET_MESSAGE), 2)
	testing.expect_value(t, parse_error_message_count(parsed.errors, "syntax error: unmatched closing ')'"), 1)
}

@(test)
open_sql_parenthesized_static_where_keeps_alias_refs :: proc(t: ^testing.T) {
	source := `SELECT SINGLE a~trkorr FROM e070 AS a JOIN e071 AS b ON a~trkorr = b~trkorr
  INTO rv_transport
  WHERE ( a~trstatus = 'D' OR a~trstatus = 'L' )
    AND b~pgmid = iv_program_id.`
	parsed := parse(source, "sql_parenthesized_alias_where.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect(t, !stmt.query.dynamic_where)
	testing.expect_value(t, len(stmt.query.source_clause.joins), 1)
	testing.expect_value(t, stmt.query.source_clause.alias.text, "a")
	testing.expect_value(t, stmt.query.source_clause.joins[0].alias.text, "b")
	testing.expect_value(t, source[stmt.query.where_clause.start:stmt.query.where_clause.end], "WHERE ( a~trstatus = 'D' OR a~trstatus = 'L' )\n    AND b~pgmid = iv_program_id")
}

@(test)
open_sql_where_value_side_marks_classic_hosts :: proc(t: ^testing.T) {
	source := `DELETE FROM tcdobs WHERE object = mv_object.
SELECT a~matnr FROM mara AS a JOIN makt AS b ON b~matnr = a~matnr INTO TABLE lt_rows WHERE type = zcl_repo=>c_type.
DELETE FROM tcdobs WHERE objecttype = ms_item-obj_name+lv_type_pos.`
	parsed := parse(source, "sql_classic_hosts.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	delete_stmt := parsed.root.stmts[0].derived_stmt.(^ast.Delete_Stmt)
	delete_cond := delete_stmt.where_cond.derived_expr.(^ast.Binary_Expr)
	_, delete_left_host := delete_cond.left.derived_expr.(^ast.Host_Expr)
	delete_right_host, delete_right_host_ok := delete_cond.right.derived_expr.(^ast.Host_Expr)
	testing.expect(t, !delete_left_host)
	testing.expect(t, delete_right_host_ok && delete_right_host.implicit)
	if delete_right_host_ok {
		delete_right_name := delete_right_host.value.derived_expr.(^ast.Ident_Expr)
		testing.expect_value(t, delete_right_name.name, "mv_object")
	}

	select_stmt := parsed.root.stmts[1].derived_stmt.(^ast.Select_Stmt)
	join_cond := select_stmt.query.source_clause.joins[0].on.derived_expr.(^ast.Binary_Expr)
	_, join_right_host := join_cond.right.derived_expr.(^ast.Host_Expr)
	testing.expect(t, !join_right_host)
	where_cond := select_stmt.query.where_cond.derived_expr.(^ast.Binary_Expr)
	where_right_host, where_right_host_ok := where_cond.right.derived_expr.(^ast.Host_Expr)
	testing.expect(t, where_right_host_ok && where_right_host.implicit)

	offset_delete := parsed.root.stmts[2].derived_stmt.(^ast.Delete_Stmt)
	offset_cond := offset_delete.where_cond.derived_expr.(^ast.Binary_Expr)
	offset_right_host, offset_right_host_ok := offset_cond.right.derived_expr.(^ast.Host_Expr)
	testing.expect(t, offset_right_host_ok && offset_right_host.implicit)
	if offset_right_host_ok {
		substring := offset_right_host.value.derived_expr.(^ast.Substring_Expr)
		_, offset_name := substring.offset.derived_expr.(^ast.Ident_Expr)
		testing.expect(t, offset_name)
		testing.expect(t, substring.length == nil)
	}
}

@(test)
open_sql_mixed_host_escape_styles_are_diagnosed :: proc(t: ^testing.T) {
	source := `SELECT q~docnum, w~trnid, e~evtid, e~bizstep
  FROM /sttp/dm_trn_evt AS w
  INNER JOIN /sttp/dm_evt AS e ON e~evtid = w~evtid
  INNER JOIN /sttp/dm_trn AS q ON q~trnid = w~trnid
  FOR ALL ENTRIES IN @mt_trn
  WHERE w~trnid = @mt_trn-trnid
    AND e~bizstep = /sttp/cl_dm_constants=>gcs_bizstep-shipping
  ORDER BY trnid, evttime DESCENDING, creation_time DESCENDING
  INTO TABLE @mt_event.`
	parsed := parse(source, "sql_mixed_host_escapes.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 1)
	testing.expect_value(t, parsed.errors[0].message, OPEN_SQL_HOST_ESCAPE_MESSAGE)
	testing.expect_value(
		t,
		source[parsed.errors[0].range.start:parsed.errors[0].range.end],
		"/sttp/cl_dm_constants=>gcs_bizstep-shipping",
	)
}

@(test)
open_sql_mixed_host_escape_styles_include_result_and_for_all_entries_hosts :: proc(t: ^testing.T) {
	source := `SELECT matnr
  FROM mara
  WHERE matnr = @lv_matnr
  INTO TABLE lt_rows.

SELECT matnr
  FROM mara
  FOR ALL ENTRIES IN lt_keys
  WHERE matnr = @lt_keys-matnr
  INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_mixed_result_and_fae_hosts.abap", context.allocator)

	testing.expect_value(t, parse_error_message_count(parsed.errors, OPEN_SQL_HOST_ESCAPE_MESSAGE), 2)
	testing.expect_value(t, len(parsed.errors), 2)
	testing.expect_value(
		t,
		source[parsed.errors[0].range.start:parsed.errors[0].range.end],
		"lt_rows",
	)
	testing.expect_value(
		t,
		source[parsed.errors[1].range.start:parsed.errors[1].range.end],
		"lt_keys",
	)

	result_stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	result_host, result_is_host := result_stmt.query.result.target.derived_expr.(^ast.Host_Expr)
	testing.expect(t, result_is_host && result_host.implicit)

	fae_stmt := parsed.root.stmts[1].derived_stmt.(^ast.Select_Stmt)
	fae_host, fae_is_host := fae_stmt.query.for_all_entries.derived_expr.(^ast.Host_Expr)
	testing.expect(t, fae_is_host && fae_host.implicit)
}

@(test)
open_sql_order_by_rejects_table_alias_field_access :: proc(t: ^testing.T) {
	source := `SELECT q~trnid, MAX( w~creation_time ) AS creation_time
  FROM /sttp/dm_trn_evt AS q
  JOIN /sttp/dm_evt AS w ON w~evtid = q~evtid AND w~bizstep = '013'
  INTO TABLE @DATA(lt_dm_trn_evt)
  WHERE q~trnid = @lv_trnid
  ORDER BY w~creation_time DESCENDING.`
	parsed := parse(source, "sql_order_by_alias.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 1)
	testing.expect_value(t, parsed.errors[0].message, OPEN_SQL_ORDER_BY_ALIAS_MESSAGE)
	testing.expect_value(
		t,
		source[parsed.errors[0].range.start:parsed.errors[0].range.end],
		"w~creation_time",
	)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect_value(
		t,
		source[stmt.query.order_by_clause.start:stmt.query.order_by_clause.end],
		"ORDER BY w~creation_time DESCENDING",
	)
	testing.expect(t, stmt.query.order_by_has_descending)
	testing.expect_value(t, len(stmt.query.order_by_fields), 1)
	testing.expect_value(t, stmt.query.order_by_fields[0].text, "creation_time")
}

@(test)
open_sql_order_by_rejects_missing_field_comma :: proc(t: ^testing.T) {
	source := `DATA lr_trnid TYPE RANGE OF /sttp/e_trnid.

SELECT q~trnid, w~evtid, w~creation_time
  FROM /sttp/dm_trn_evt AS q
  JOIN /sttp/dm_evt AS w ON w~evtid = q~evtid AND w~bizstep = '013'
  INTO TABLE @DATA(lt_trn_evt)
  WHERE trnid IN @lr_trnid
  ORDER BY trnid creation_time DESCENDING.`
	parsed := parse(source, "sql_order_by_missing_comma.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 1)
	testing.expect_value(t, parsed.errors[0].message, OPEN_SQL_ORDER_BY_COMMA_MESSAGE)
	testing.expect_value(
		t,
		source[parsed.errors[0].range.start:parsed.errors[0].range.end],
		"creation_time",
	)
	stmt := parsed.root.stmts[1].derived_stmt.(^ast.Select_Stmt)
	testing.expect_value(
		t,
		source[stmt.query.order_by_clause.start:stmt.query.order_by_clause.end],
		"ORDER BY trnid creation_time DESCENDING",
	)
	testing.expect(t, stmt.query.order_by_has_descending)
	testing.expect_value(t, len(stmt.query.order_by_fields), 2)
	testing.expect_value(t, stmt.query.order_by_fields[0].text, "trnid")
	testing.expect_value(t, stmt.query.order_by_fields[1].text, "creation_time")
}

@(test)
open_sql_select_rejects_unexpected_tail_tokens :: proc(t: ^testing.T) {
	source := `SELECT q~trnid, w~creation_time AS creation_time
  FROM /sttp/dm_trn_evt AS q
  JOIN /sttp/dm_evt AS w ON w~evtid = q~evtid AND w~bizstep = '013'
  INTO TABLE @DATA(lt_dm_trn_evt)
  FOR ALL ENTRIES IN @lt_dm_trn
  WHERE trnid = @lt_dm_trn-trnid
  ORDE BY creation_time DESCENDIN.`
	parsed := parse(source, "sql_unexpected_tail.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 1)
	testing.expect_value(t, parsed.errors[0].message, OPEN_SQL_UNEXPECTED_TOKEN_MESSAGE)
	testing.expect_value(
		t,
		source[parsed.errors[0].range.start:parsed.errors[0].range.end],
		"ORDE BY creation_time DESCENDIN",
	)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect(t, stmt.query.where_cond != nil)
	testing.expect_value(t, stmt.query.order_by_clause.end, 0)
}

@(test)
open_sql_order_by_rejects_misspelled_direction :: proc(t: ^testing.T) {
	source := `SELECT trnid FROM /sttp/dm_trn_evt ORDER BY creation_time DESCENDIN INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_order_by_direction_typo.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 1)
	testing.expect_value(t, parsed.errors[0].message, OPEN_SQL_ORDER_BY_DIRECTION_MESSAGE)
	testing.expect_value(
		t,
		source[parsed.errors[0].range.start:parsed.errors[0].range.end],
		"DESCENDIN",
	)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect_value(t, len(stmt.query.order_by_fields), 1)
	testing.expect_value(t, stmt.query.order_by_fields[0].text, "creation_time")
}

@(test)
open_sql_invalid_aliases_and_partial_joins_are_diagnosed :: proc(t: ^testing.T) {
	source := `SELECT carrid AS FROM mara AS WHERE carrid = @lv_carrid INTO TABLE @lt_rows.
SELECT * FROM mara INNER WHERE matnr = @lv_matnr INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_invalid_alias_join.abap", context.allocator)

	testing.expect(t, len(parsed.errors) >= 3)
	alias_stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect_value(t, alias_stmt.query.projection_clauses[0].alias.text, "")
	testing.expect_value(t, alias_stmt.query.source_clause.alias.text, "")
	testing.expect(t, alias_stmt.query.where_cond != nil)
	join_stmt := parsed.root.stmts[1].derived_stmt.(^ast.Select_Stmt)
	testing.expect_value(t, len(join_stmt.query.source_clause.joins), 0)
	testing.expect(t, join_stmt.query.where_cond != nil)
}

@(test)
open_sql_join_on_stops_before_next_join_or_clause :: proc(t: ^testing.T) {
	source := `SELECT a~carrid FROM scarr AS a INNER JOIN spfli AS b ON b~carrid = a~carrid LEFT OUTER JOIN sflight AS f ON f~carrid = b~carrid INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_join_boundaries.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect_value(t, len(stmt.query.source_clause.joins), 2)
	testing.expect_value(t, stmt.query.source_clause.joins[0].kind, ast.Select_Join_Kind.Inner)
	testing.expect_value(t, stmt.query.source_clause.joins[1].kind, ast.Select_Join_Kind.Left_Outer)
	testing.expect_value(t, source[stmt.query.source_clause.joins[0].on.range.start:stmt.query.source_clause.joins[0].on.range.end], "b~carrid = a~carrid")
	testing.expect_value(t, source[stmt.query.source_clause.joins[1].on.range.start:stmt.query.source_clause.joins[1].on.range.end], "f~carrid = b~carrid")
}

@(test)
open_sql_ctes_and_dynamic_sources_keep_formatter_fields :: proc(t: ^testing.T) {
	source := `WITH +recent AS ( SELECT matnr FROM mara WHERE matnr = @lv_matnr ) SELECT matnr FROM (lv_source) AS s INNER JOIN @lt_keys AS k ON k~matnr = s~matnr INTO TABLE @lt_rows WHERE (lv_where).`
	parsed := parse(source, "sql_cte_dynamic.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect(t, stmt.with != nil)
	testing.expect_value(t, len(stmt.with.entries), 1)
	testing.expect_value(t, stmt.with.entries[0].name.text, "+recent")
	testing.expect(t, stmt.with.entries[0].query.where_cond != nil)
	testing.expect(t, stmt.query.source_clause != nil)
	testing.expect(t, stmt.query.source_clause.dynamic_source)
	testing.expect_value(t, len(stmt.query.source_clause.joins), 1)
	_, host_join := stmt.query.source_clause.joins[0].source.derived_expr.(^ast.Host_Expr)
	testing.expect(t, host_join)
	testing.expect(t, stmt.query.dynamic_where)

	printed := ast.print_node(parsed.root, context.allocator)
	testing.expect_value(t, printed, "WITH +recent AS ( SELECT matnr FROM mara WHERE matnr = @lv_matnr ) SELECT matnr FROM ( lv_source ) AS s INNER JOIN @lt_keys AS k ON k~matnr = s~matnr INTO TABLE @lt_rows WHERE ( lv_where ).")
}

@(test)
open_sql_clause_ranges_and_order_facts_are_parser_modeled :: proc(t: ^testing.T) {
	source := `SELECT a~* FROM mara AS a INTO TABLE @lt_rows WHERE a~matnr = @lv_matnr GROUP BY a~matnr HAVING COUNT( * ) > 0 ORDER BY matnr, ersda UP TO 10 ROWS PACKAGE SIZE lv_size OFFSET 2 BYPASSING BUFFER CONNECTION con CLIENT SPECIFIED.`
	parsed := parse(source, "sql_clause_facts.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	query := stmt.query
	testing.expect_value(t, len(query.projection_clauses), 1)
	testing.expect_value(t, source[query.projection_clause.start:query.projection_clause.end], "a~*")
	testing.expect_value(t, source[query.from_clause.start:query.from_clause.end], "mara AS a")
	testing.expect_value(t, source[query.into_clause.start:query.into_clause.end], "INTO TABLE @lt_rows")
	testing.expect_value(t, source[query.where_clause.start:query.where_clause.end], "WHERE a~matnr = @lv_matnr")
	testing.expect_value(t, source[query.group_by_clause.start:query.group_by_clause.end], "GROUP BY a~matnr")
	testing.expect_value(t, len(query.group_by), 1)
	testing.expect_value(t, source[query.group_by[0].range.start:query.group_by[0].range.end], "a~matnr")
	group_field := query.group_by[0].value.derived_expr.(^ast.Sql_Column_Expr)
	testing.expect_value(t, group_field.qualifier.text, "a")
	testing.expect_value(t, group_field.name.text, "matnr")
	testing.expect_value(t, source[query.having_clause.start:query.having_clause.end], "HAVING COUNT( * ) > 0")
	testing.expect_value(t, source[query.order_by_clause.start:query.order_by_clause.end], "ORDER BY matnr, ersda")
	testing.expect_value(t, len(query.order_by_fields), 2)
	testing.expect_value(t, query.order_by_fields[0].text, "matnr")
	testing.expect_value(t, query.order_by_fields[1].text, "ersda")
	testing.expect_value(t, source[query.up_to_clause.start:query.up_to_clause.end], "UP TO 10 ROWS")
	testing.expect_value(t, source[query.package_size_clause.start:query.package_size_clause.end], "PACKAGE SIZE lv_size")
	testing.expect_value(t, source[query.offset_clause.start:query.offset_clause.end], "OFFSET 2")
	testing.expect_value(t, source[query.abap_options_clause.start:query.abap_options_clause.end], "BYPASSING BUFFER CONNECTION con CLIENT SPECIFIED")
}

@(test)
open_sql_null_and_like_predicates_are_modeled :: proc(t: ^testing.T) {
	source := `SELECT * FROM mara WHERE matnr IS NULL INTO TABLE @lt_rows.
SELECT * FROM mara WHERE matnr IS NOT NULL INTO TABLE @lt_rows.
SELECT * FROM mara WHERE matnr LIKE @lv_pattern INTO TABLE @lt_rows.
SELECT * FROM mara WHERE matnr NOT LIKE lv_pattern INTO TABLE lt_rows.`
	parsed := parse(source, "sql_null_like.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	is_null := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt).query.where_cond.derived_expr.(^ast.Is_Predicate_Expr)
	is_not_null := parsed.root.stmts[1].derived_stmt.(^ast.Select_Stmt).query.where_cond.derived_expr.(^ast.Is_Predicate_Expr)
	like := parsed.root.stmts[2].derived_stmt.(^ast.Select_Stmt).query.where_cond.derived_expr.(^ast.Binary_Expr)
	not_like := parsed.root.stmts[3].derived_stmt.(^ast.Select_Stmt).query.where_cond.derived_expr.(^ast.Binary_Expr)

	testing.expect_value(t, is_null.kind, ast.Is_Predicate_Kind.Null)
	testing.expect(t, !is_null.negated)
	testing.expect_value(t, is_not_null.kind, ast.Is_Predicate_Kind.Null)
	testing.expect(t, is_not_null.negated)
	testing.expect_value(t, like.op, ast.Binary_Op.Like)
	testing.expect_value(t, not_like.op, ast.Binary_Op.Not_Like)
}

@(test)
open_sql_case_expression_is_modeled :: proc(t: ^testing.T) {
	source := `SELECT CASE WHEN carrid = @lv_carrid THEN connid ELSE carrid END AS value FROM sflight INTO TABLE @lt_rows.
SELECT CASE carrid WHEN 'AA' THEN connid ELSE carrid END AS value FROM sflight INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_case_expr.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	case_expr := stmt.query.projection_clauses[0].value.derived_expr.(^ast.Sql_Case_Expr)
	testing.expect(t, case_expr.operand == nil)
	testing.expect_value(t, len(case_expr.whens), 1)
	when_expr := case_expr.whens[0].derived_expr.(^ast.Sql_Case_When_Expr)
	_, condition_is_binary := when_expr.condition.derived_expr.(^ast.Binary_Expr)
	testing.expect(t, condition_is_binary)
	testing.expect(t, when_expr.result != nil)
	testing.expect(t, case_expr.else_expr != nil)
	testing.expect_value(t, stmt.query.projection_clauses[0].alias.text, "value")
	simple_stmt := parsed.root.stmts[1].derived_stmt.(^ast.Select_Stmt)
	simple_case := simple_stmt.query.projection_clauses[0].value.derived_expr.(^ast.Sql_Case_Expr)
	testing.expect(t, simple_case.operand != nil)
	testing.expect_value(t, len(simple_case.whens), 1)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
open_sql_invalid_clause_placement_is_diagnosed_without_modeling_where :: proc(t: ^testing.T) {
	source := `SELECT * WHERE carrid = @lv_carrid FROM mara INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_invalid_order.abap", context.allocator)

	testing.expect(t, len(parsed.errors) > 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect(t, stmt.query.source_clause != nil)
	testing.expect(t, stmt.query.where_cond == nil)
	testing.expect_value(t, stmt.query.where_clause.end, 0)
}

@(test)
open_sql_empty_where_condition_does_not_model_clause_keyword :: proc(t: ^testing.T) {
	source := `SELECT * FROM mara WHERE GROUP BY matnr INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_empty_where.abap", context.allocator)

	testing.expect(t, len(parsed.errors) > 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect(t, stmt.query.where_cond == nil)
	testing.expect_value(t, source[stmt.query.group_by_clause.start:stmt.query.group_by_clause.end], "GROUP BY matnr")
}

@(test)
open_sql_incomplete_predicates_do_not_model_clause_keywords :: proc(t: ^testing.T) {
	source := `SELECT * FROM mara WHERE matnr LIKE GROUP BY matnr INTO TABLE @lt_rows.
SELECT * FROM mara WHERE matnr IS NOT GROUP BY matnr INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_incomplete_predicates.abap", context.allocator)

	testing.expect(t, len(parsed.errors) > 0)
	for stmt in parsed.root.stmts {
		select_stmt := stmt.derived_stmt.(^ast.Select_Stmt)
		testing.expect(t, select_stmt.query.where_cond == nil)
		testing.expect_value(t, source[select_stmt.query.group_by_clause.start:select_stmt.query.group_by_clause.end], "GROUP BY matnr")
	}
}

@(test)
open_sql_duplicate_from_clause_is_diagnosed_without_overwriting_source :: proc(t: ^testing.T) {
	source := `SELECT * FROM mara FROM makt INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_duplicate_from.abap", context.allocator)

	testing.expect(t, len(parsed.errors) > 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect_value(t, source[stmt.query.from_clause.start:stmt.query.from_clause.end], "mara")
	testing.expect_value(t, len(stmt.query.source_clause.joins), 0)
}

@(test)
open_sql_classic_and_modern_result_orderings_are_valid :: proc(t: ^testing.T) {
	source := `SELECT SINGLE matnr INTO @DATA(lv_old) FROM mara WHERE matnr = @lv_key.
SELECT SINGLE matnr FROM mara INTO @DATA(lv_new) WHERE matnr = @lv_key.`
	parsed := parse(source, "sql_classic_modern_order.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	classic := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	modern := parsed.root.stmts[1].derived_stmt.(^ast.Select_Stmt)
	testing.expect_value(t, source[classic.query.into_clause.start:classic.query.into_clause.end], "INTO @DATA(lv_old)")
	testing.expect_value(t, source[classic.query.from_clause.start:classic.query.from_clause.end], "mara")
	testing.expect_value(t, source[classic.query.where_clause.start:classic.query.where_clause.end], "WHERE matnr = @lv_key")
	testing.expect_value(t, source[modern.query.from_clause.start:modern.query.from_clause.end], "mara")
	testing.expect_value(t, source[modern.query.into_clause.start:modern.query.into_clause.end], "INTO @DATA(lv_new)")
	testing.expect_value(t, source[modern.query.where_clause.start:modern.query.where_clause.end], "WHERE matnr = @lv_key")
}

@(test)
open_sql_projection_named_value_before_dynamic_source_is_valid :: proc(t: ^testing.T) {
	source := `SELECT value FROM (zcl_abapgit_persistence_db=>c_tabname)
  INTO TABLE rt_repo_ids
  WHERE type = zcl_abapgit_persistence_db=>c_type_repo.`
	parsed := parse(source, "sql_value_column_dynamic_source.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	projection := stmt.query.projection_clauses[0].value.derived_expr.(^ast.Sql_Column_Expr)
	testing.expect_value(t, projection.name.text, "value")
	testing.expect(t, stmt.query.source_clause.dynamic_source)
	testing.expect(t, stmt.query.where_cond != nil)
}

@(test)
open_sql_source_named_cross_is_not_join_without_join_keyword :: proc(t: ^testing.T) {
	source := `SELECT COUNT(*) FROM cross
  WHERE ( type = 'P' OR type = 'Q' ) AND name = lv_paramid.`
	parsed := parse(source, "sql_cross_table.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	source_expr := stmt.query.source_clause.source.derived_expr.(^ast.Ident_Expr)
	testing.expect_value(t, source_expr.name, "cross")
	testing.expect(t, stmt.query.where_cond != nil)
	testing.expect(t, !stmt.query.dynamic_where)
}

@(test)
open_sql_sap_validated_tail_orderings_are_valid :: proc(t: ^testing.T) {
	source := `SELECT trkorr FROM e070 ORDER BY trkorr INTO TABLE @lt_rows.
SELECT trkorr FROM e070 WHERE trkorr = @lv_trkorr INTO @DATA(lv_row) UP TO 1 ROWS.
SELECT trkorr FROM e070 INTO TABLE @lt_rows UP TO 10 ROWS WHERE trkorr = @lv_trkorr.
SELECT trkorr FROM e070 UP TO 10 ROWS INTO TABLE @lt_rows WHERE trkorr = @lv_trkorr.`
	parsed := parse(source, "sql_sap_valid_tail_order.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	order_before_result := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	final_result_before_up_to := parsed.root.stmts[1].derived_stmt.(^ast.Select_Stmt)
	result_before_up_to := parsed.root.stmts[2].derived_stmt.(^ast.Select_Stmt)
	up_to_before_result := parsed.root.stmts[3].derived_stmt.(^ast.Select_Stmt)

	testing.expect_value(t, source[order_before_result.query.order_by_clause.start:order_before_result.query.order_by_clause.end], "ORDER BY trkorr")
	testing.expect_value(t, source[order_before_result.query.into_clause.start:order_before_result.query.into_clause.end], "INTO TABLE @lt_rows")
	testing.expect_value(t, source[final_result_before_up_to.query.where_clause.start:final_result_before_up_to.query.where_clause.end], "WHERE trkorr = @lv_trkorr")
	testing.expect_value(t, source[final_result_before_up_to.query.into_clause.start:final_result_before_up_to.query.into_clause.end], "INTO @DATA(lv_row)")
	testing.expect_value(t, source[final_result_before_up_to.query.up_to_clause.start:final_result_before_up_to.query.up_to_clause.end], "UP TO 1 ROWS")
	testing.expect_value(t, source[result_before_up_to.query.into_clause.start:result_before_up_to.query.into_clause.end], "INTO TABLE @lt_rows")
	testing.expect_value(t, source[result_before_up_to.query.up_to_clause.start:result_before_up_to.query.up_to_clause.end], "UP TO 10 ROWS")
	testing.expect_value(t, source[result_before_up_to.query.where_clause.start:result_before_up_to.query.where_clause.end], "WHERE trkorr = @lv_trkorr")
	testing.expect_value(t, source[up_to_before_result.query.up_to_clause.start:up_to_before_result.query.up_to_clause.end], "UP TO 10 ROWS")
	testing.expect_value(t, source[up_to_before_result.query.into_clause.start:up_to_before_result.query.into_clause.end], "INTO TABLE @lt_rows")
	testing.expect_value(t, source[up_to_before_result.query.where_clause.start:up_to_before_result.query.where_clause.end], "WHERE trkorr = @lv_trkorr")
}

@(test)
open_sql_up_to_before_from_is_valid :: proc(t: ^testing.T) {
	source := `SELECT trkorr UP TO 1 ROWS
  FROM e070
  INTO @DATA(lv_row)
  WHERE trkorr = @lv_trkorr.
ENDSELECT.`
	parsed := parse(source, "sql_up_to_before_from.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	query := stmt.query
	testing.expect_value(t, source[query.projection_clause.start:query.projection_clause.end], "trkorr")
	testing.expect_value(t, source[query.up_to_clause.start:query.up_to_clause.end], "UP TO 1 ROWS")
	testing.expect_value(t, source[query.from_clause.start:query.from_clause.end], "e070")
	testing.expect_value(t, source[query.into_clause.start:query.into_clause.end], "INTO @DATA(lv_row)")
}

@(test)
open_cursor_select_accepts_where_and_order_by :: proc(t: ^testing.T) {
	source := `OPEN CURSOR cv FOR SELECT matnr FROM mara WHERE matnr = @lv_key ORDER BY matnr.`
	parsed := parse(source, "sql_cursor_order.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Open_Cursor_Stmt)
	testing.expect(t, stmt.query.where_cond != nil)
	testing.expect_value(t, source[stmt.query.order_by_clause.start:stmt.query.order_by_clause.end], "ORDER BY matnr")
	testing.expect_value(t, len(stmt.query.order_by_fields), 1)
	testing.expect_value(t, stmt.query.order_by_fields[0].text, "matnr")
}

@(test)
open_sql_cursor_package_loop_accepts_escaped_handle_and_inline_target :: proc(t: ^testing.T) {
	source := `OPEN CURSOR WITH HOLD @lv_cursor FOR
  SELECT trstatus
    FROM e070
    WHERE trstatus = @lv_value.

DO.
  FETCH NEXT CURSOR @lv_cursor
    INTO TABLE @DATA(lt_package)
    PACKAGE SIZE 100.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.


ENDDO.

CLOSE CURSOR @lv_cursor.`
	parsed := parse(source, "sql_cursor_package_loop.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, len(parsed.root.stmts), 3)

	open_cursor := parsed.root.stmts[0].derived_stmt.(^ast.Open_Cursor_Stmt)
	testing.expect(t, open_cursor.with_hold)
	_, open_handle_is_host := open_cursor.handle.derived_expr.(^ast.Host_Expr)
	testing.expect(t, open_handle_is_host)
	testing.expect(t, open_cursor.query.where_cond != nil)

	do_stmt := parsed.root.stmts[1].derived_stmt.(^ast.Do_Stmt)
	testing.expect_value(t, len(do_stmt.body), 2)

	fetch := do_stmt.body[0].derived_stmt.(^ast.Fetch_Stmt)
	_, fetch_handle_is_host := fetch.handle.derived_expr.(^ast.Host_Expr)
	testing.expect(t, fetch_handle_is_host)
	testing.expect(t, fetch.result != nil)
	testing.expect(t, fetch.result.table)
	_, fetch_target_is_host := fetch.result.target.derived_expr.(^ast.Host_Expr)
	testing.expect(t, fetch_target_is_host)
	testing.expect(t, fetch.package_size != nil)

	if_stmt := do_stmt.body[1].derived_stmt.(^ast.If_Stmt)
	testing.expect_value(t, len(if_stmt.body), 1)

	close_cursor := parsed.root.stmts[2].derived_stmt.(^ast.Close_Cursor_Stmt)
	_, close_handle_is_host := close_cursor.handle.derived_expr.(^ast.Host_Expr)
	testing.expect(t, close_handle_is_host)
}

@(test)
open_sql_cte_and_set_operator_chain_is_modeled :: proc(t: ^testing.T) {
	source := `WITH +base AS ( SELECT matnr FROM mara ) SELECT matnr FROM +base UNION SELECT matnr FROM makt INTERSECT SELECT matnr FROM zkeep EXCEPT SELECT matnr FROM zskip INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_cte_set_chain.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect(t, stmt.with != nil)
	testing.expect_value(t, len(stmt.with.entries), 1)
	testing.expect_value(t, len(stmt.query.set_ops), 1)
	testing.expect_value(t, stmt.query.set_ops[0].kind, ast.Select_Set_Kind.Union)
	intersect_query := stmt.query.set_ops[0].query
	testing.expect_value(t, len(intersect_query.set_ops), 1)
	testing.expect_value(t, intersect_query.set_ops[0].kind, ast.Select_Set_Kind.Intersect)
	except_query := intersect_query.set_ops[0].query
	testing.expect_value(t, len(except_query.set_ops), 1)
	testing.expect_value(t, except_query.set_ops[0].kind, ast.Select_Set_Kind.Except)
	testing.expect(t, except_query.set_ops[0].query.result != nil)
}

@(test)
open_sql_set_operator_distinct_is_preserved :: proc(t: ^testing.T) {
	source := `SELECT matnr FROM mara UNION DISTINCT SELECT matnr FROM makt INTERSECT DISTINCT SELECT matnr FROM zkeep EXCEPT DISTINCT SELECT matnr FROM zskip INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_set_distinct.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	union_set := stmt.query.set_ops[0]
	intersect_set := union_set.query.set_ops[0]
	except_set := intersect_set.query.set_ops[0]
	testing.expect_value(t, union_set.kind, ast.Select_Set_Kind.Union)
	testing.expect(t, union_set.is_distinct)
	testing.expect_value(t, intersect_set.kind, ast.Select_Set_Kind.Intersect)
	testing.expect(t, intersect_set.is_distinct)
	testing.expect_value(t, except_set.kind, ast.Select_Set_Kind.Except)
	testing.expect(t, except_set.is_distinct)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
}

@(test)
open_sql_set_operator_all_is_only_valid_for_union :: proc(t: ^testing.T) {
	source := `SELECT matnr FROM mara INTERSECT ALL SELECT matnr FROM makt INTO TABLE @lt_rows.
SELECT matnr FROM mara EXCEPT ALL SELECT matnr FROM makt INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_set_all_invalid.abap", context.allocator)

	testing.expect_value(t, parse_error_message_count(parsed.errors, OPEN_SQL_SET_ALL_MESSAGE), 2)
}

@(test)
open_sql_order_by_primary_requires_key :: proc(t: ^testing.T) {
	source := `SELECT matnr FROM mara ORDER BY PRIMARY INTO TABLE @lt_rows.
SELECT matnr FROM mara ORDER BY PRIMARY KEY INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_order_by_primary_key.abap", context.allocator)

	testing.expect_value(t, parse_error_message_count(parsed.errors, OPEN_SQL_ORDER_BY_PRIMARY_KEY_MESSAGE), 1)
	missing_key := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	valid := parsed.root.stmts[1].derived_stmt.(^ast.Select_Stmt)
	testing.expect(t, !missing_key.query.order_by_primary_key)
	testing.expect(t, valid.query.order_by_primary_key)
}

@(test)
open_sql_up_to_and_offset_combination_rules_are_diagnosed :: proc(t: ^testing.T) {
	source := `SELECT SINGLE matnr FROM mara UP TO 1 ROWS INTO @DATA(lv_matnr).
SELECT matnr FROM mara UNION SELECT matnr FROM makt UP TO 1 ROWS INTO TABLE @lt_rows.
SELECT matnr FROM mara OFFSET 1 INTO TABLE @lt_rows.
SELECT SINGLE matnr FROM mara ORDER BY matnr OFFSET 1 INTO @DATA(lv_matnr).
SELECT matnr FROM mara FOR ALL ENTRIES IN @lt_keys WHERE matnr = @lt_keys-matnr ORDER BY matnr OFFSET 1 INTO TABLE @lt_rows.
SELECT matnr FROM mara ORDER BY matnr OFFSET 1 UNION SELECT matnr FROM makt INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_up_to_offset_combinations.abap", context.allocator)

	testing.expect_value(t, parse_error_message_count(parsed.errors, OPEN_SQL_UP_TO_COMBINATION_MESSAGE), 2)
	testing.expect_value(t, parse_error_message_count(parsed.errors, OPEN_SQL_OFFSET_COMBINATION_MESSAGE), 4)
}

@(test)
open_sql_required_operands_report_missing_pieces :: proc(t: ^testing.T) {
	source := `WITH +base AS ( SELECT matnr FROM mara ).
WITH AS ( SELECT matnr FROM mara ) SELECT matnr FROM mara INTO TABLE @lt_rows.
WITH +bad SELECT matnr FROM mara INTO TABLE @lt_rows.
SELECT matnr FROM mara FOR ALL ENTRIES IN WHERE matnr = @lv_key INTO TABLE @lt_rows.
SELECT matnr FROM mara PACKAGE SIZE INTO TABLE @lt_rows.
SELECT matnr FROM mara UP TO ROWS INTO TABLE @lt_rows.
SELECT FROM mara FIELDS INTO TABLE @lt_rows.
SELECT matnr FROM mara GROUP BY INTO TABLE @lt_rows.
SELECT matnr FROM mara ORDER BY INTO TABLE @lt_rows.
SELECT matnr FROM mara GROUP BY matnr HAVING INTO TABLE @lt_rows.
SELECT matnr FROM mara OFFSET INTO TABLE @lt_rows.
SELECT * FROM mara AS a INNER JOIN makt AS b INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_missing_required.abap", context.allocator)

	testing.expect_value(t, parse_error_message_count(parsed.errors, "syntax error: expected SELECT"), 1)
	testing.expect_value(t, parse_error_message_count(parsed.errors, "syntax error: expected CTE name"), 1)
	testing.expect_value(t, parse_error_message_count(parsed.errors, "syntax error: expected AS in WITH clause"), 1)
	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, "syntax error: expected table after FOR ALL ENTRIES IN"),
		1,
	)
	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, "syntax error: expected PACKAGE SIZE value"),
		1,
	)
	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, "syntax error: expected row count after UP TO"),
		1,
	)
	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, "syntax error: expected SELECT field"),
		1,
	)
	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, "syntax error: expected GROUP BY field"),
		1,
	)
	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, "syntax error: expected ORDER BY field"),
		1,
	)
	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, "syntax error: expected HAVING condition"),
		1,
	)
	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, "syntax error: expected OFFSET value"),
		1,
	)
	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, "syntax error: expected ON after SELECT JOIN"),
		1,
	)
}

@(test)
open_sql_select_requires_projection_or_fields_clause :: proc(t: ^testing.T) {
	source := `SELECT FROM mara INTO TABLE @lt_rows.
SELECT matnr FROM mara FIELDS mtart INTO TABLE @lt_rows.
SELECT FROM mara FIELDS matnr INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_projection_required.abap", context.allocator)

	testing.expect_value(t, parse_error_message_count(parsed.errors, "syntax error: expected SELECT field"), 1)
	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, "syntax error: invalid SELECT FIELDS clause placement"),
		1,
	)
	valid := parsed.root.stmts[2].derived_stmt.(^ast.Select_Stmt)
	testing.expect_value(t, len(valid.query.projection_clauses), 1)
	testing.expect_value(t, source[valid.query.projection_clause.start:valid.query.projection_clause.end], "matnr")
}

@(test)
open_sql_select_requires_from_clause :: proc(t: ^testing.T) {
	source := `SELECT matnr INTO @DATA(lv_matnr).
SELECT SINGLE matnr INTO @DATA(lv_old) FROM mara.
SELECT FROM mara FIELDS matnr INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_from_required.abap", context.allocator)

	testing.expect_value(t, parse_error_message_count(parsed.errors, OPEN_SQL_MISSING_FROM_MESSAGE), 1)
	testing.expect_value(t, parse_error_message_count(parsed.errors, OPEN_SQL_MISSING_ENDSELECT_MESSAGE), 0)

	old_style := parsed.root.stmts[1].derived_stmt.(^ast.Select_Stmt)
	new_style := parsed.root.stmts[2].derived_stmt.(^ast.Select_Stmt)
	testing.expect_value(t, source[old_style.query.from_clause.start:old_style.query.from_clause.end], "mara")
	testing.expect_value(t, source[new_style.query.from_clause.start:new_style.query.from_clause.end], "mara")
	testing.expect_value(t, len(new_style.query.projection_clauses), 1)
}

@(test)
open_sql_select_result_and_package_combinations_are_diagnosed :: proc(t: ^testing.T) {
	source := `SELECT SINGLE matnr FROM mara INTO TABLE @lt_rows.
SELECT SINGLE matnr FROM mara APPENDING TABLE @lt_rows.
SELECT matnr FROM mara INTO @lv_matnr PACKAGE SIZE 10.
ENDSELECT.
SELECT matnr FROM mara INTO TABLE @lt_rows PACKAGE SIZE 10.`
	parsed := parse(source, "sql_result_package_combinations.abap", context.allocator)

	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, OPEN_SQL_SINGLE_TABLE_RESULT_MESSAGE),
		2,
	)
	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, OPEN_SQL_PACKAGE_SIZE_RESULT_MESSAGE),
		1,
	)
	valid := parsed.root.stmts[3].derived_stmt.(^ast.Select_Stmt)
	testing.expect(t, valid.query.result.table)
	testing.expect(t, valid.query.package_size != nil)
}

@(test)
open_sql_state_machine_rejects_invalid_clause_placements :: proc(t: ^testing.T) {
	source := `SELECT * FROM mara INTO @wa INTO @wb.
SELECT * FROM mara HAVING COUNT( * ) > 0 INTO TABLE @lt_rows.
SELECT trkorr FROM e070 WHERE trkorr = @lv_trkorr INTO TABLE @lt_rows ORDER BY trkorr.
SELECT * FROM mara GROUP BY matnr WHERE matnr = @lv_matnr INTO TABLE @lt_rows.
SELECT * FROM mara ORDER BY matnr WHERE matnr = @lv_matnr INTO TABLE @lt_rows.
SELECT * ORDER BY matnr FROM mara INTO TABLE @lt_rows.
SELECT * FROM mara UNION.`
	parsed := parse(source, "sql_invalid_state_machine.abap", context.allocator)

	testing.expect(t, len(parsed.errors) > 0)
	duplicate_target := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	having_without_group := parsed.root.stmts[1].derived_stmt.(^ast.Select_Stmt)
	late_result_order := parsed.root.stmts[2].derived_stmt.(^ast.Select_Stmt)
	where_after_group := parsed.root.stmts[3].derived_stmt.(^ast.Select_Stmt)
	where_after_order := parsed.root.stmts[4].derived_stmt.(^ast.Select_Stmt)
	order_before_from := parsed.root.stmts[5].derived_stmt.(^ast.Select_Stmt)
	missing_set_select := parsed.root.stmts[6].derived_stmt.(^ast.Select_Stmt)

	testing.expect_value(t, source[duplicate_target.query.into_clause.start:duplicate_target.query.into_clause.end], "INTO @wa")
	testing.expect_value(t, having_without_group.query.having_clause.end, 0)
	testing.expect(t, late_result_order.query.where_cond != nil)
	testing.expect_value(t, late_result_order.query.order_by_clause.end, 0)
	testing.expect_value(t, source[where_after_group.query.group_by_clause.start:where_after_group.query.group_by_clause.end], "GROUP BY matnr")
	testing.expect(t, where_after_group.query.where_cond == nil)
	testing.expect_value(t, where_after_group.query.where_clause.end, 0)
	testing.expect_value(t, source[where_after_order.query.order_by_clause.start:where_after_order.query.order_by_clause.end], "ORDER BY matnr")
	testing.expect(t, where_after_order.query.where_cond == nil)
	testing.expect_value(t, where_after_order.query.where_clause.end, 0)
	testing.expect_value(t, order_before_from.query.order_by_clause.end, 0)
	testing.expect_value(t, source[order_before_from.query.from_clause.start:order_before_from.query.from_clause.end], "mara")
	testing.expect_value(t, len(missing_set_select.query.set_ops), 0)
	testing.expect_value(t, missing_set_select.query.set_operator_clause.end, 0)
}

@(test)
data_access_required_operands_report_missing_pieces :: proc(t: ^testing.T) {
	source := `OPEN CURSOR FOR SELECT * FROM mara.
FETCH NEXT CURSOR INTO wa.
FETCH NEXT CURSOR cv PACKAGE SIZE.
CLOSE CURSOR.
READ TABLE INTO wa.
READ TABLE itab WITH KEY id = .
APPEND TO lt_rows.
APPEND wa TO.
APPEND INITIAL LINE TO lt_rows ASSIGNING.`
	parsed := parse(source, "data_access_missing_required.abap", context.allocator)

	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, "syntax error: expected cursor handle"),
		3,
	)
	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, "syntax error: expected PACKAGE SIZE value"),
		1,
	)
	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, "syntax error: expected READ TABLE source"),
		1,
	)
	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, "syntax error: expected READ TABLE key value"),
		1,
	)
	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, "syntax error: expected APPEND source"),
		1,
	)
	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, "syntax error: expected APPEND target"),
		1,
	)
	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, "syntax error: expected APPEND ASSIGNING target"),
		1,
	)
}

@(test)
open_sql_for_all_entries_rejects_group_by :: proc(t: ^testing.T) {
	source := `SELECT matnr FROM mara FOR ALL ENTRIES IN @lt_keys GROUP BY matnr INTO TABLE @lt_rows.
SELECT matnr FROM mara GROUP BY matnr FOR ALL ENTRIES IN @lt_keys INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_fae_group_by.abap", context.allocator)

	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, OPEN_SQL_FOR_ALL_ENTRIES_GROUP_BY_MESSAGE),
		2,
	)
	entries_before_group := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	group_before_entries := parsed.root.stmts[1].derived_stmt.(^ast.Select_Stmt)
	testing.expect(t, entries_before_group.query.for_all_entries != nil)
	testing.expect_value(t, entries_before_group.query.group_by_clause.end, 0)
	testing.expect(t, group_before_entries.query.for_all_entries == nil)
	testing.expect_value(
		t,
		source[group_before_entries.query.group_by_clause.start:group_before_entries.query.group_by_clause.end],
		"GROUP BY matnr",
	)
}

@(test)
open_sql_for_all_entries_allows_count_star :: proc(t: ^testing.T) {
	source := `SELECT COUNT( * )
  FROM mara
  FOR ALL ENTRIES IN @lt_keys
  WHERE matnr = @lt_keys-matnr
  INTO @DATA(lv_count).`
	parsed := parse(source, "sql_fae_count_star.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
}

@(test)
open_sql_for_all_entries_rejects_aggregates_except_count_star :: proc(t: ^testing.T) {
	source := `SELECT q~trnid, MAX( w~creation_time ) AS creation_time, COUNT( * ) AS count
  FROM /sttp/dm_trn_evt AS q
  JOIN /sttp/dm_evt AS w ON w~evtid = q~evtid AND w~bizstep = '013'
  INTO TABLE @DATA(lt_dm_trn_evt)
  FOR ALL ENTRIES IN @lt_dm_trn
  WHERE trnid = @lt_dm_trn-trnid.
SELECT COUNT( matnr )
  FROM mara
  FOR ALL ENTRIES IN @lt_keys
  WHERE matnr = @lt_keys-matnr
  INTO @DATA(lv_count).
SELECT COUNT( DISTINCT matnr )
  FROM mara
  FOR ALL ENTRIES IN @lt_keys
  WHERE matnr = @lt_keys-matnr
  INTO @DATA(lv_distinct_count).`
	parsed := parse(source, "sql_fae_aggregates.abap", context.allocator)

	testing.expect_value(
		t,
		parse_error_message_count(parsed.errors, OPEN_SQL_FOR_ALL_ENTRIES_AGGREGATE_MESSAGE),
		3,
	)
	testing.expect_value(t, parsed.errors[0].message, OPEN_SQL_FOR_ALL_ENTRIES_AGGREGATE_MESSAGE)
	testing.expect_value(
		t,
		source[parsed.errors[0].range.start:parsed.errors[0].range.end],
		"MAX( w~creation_time )",
	)
	testing.expect_value(t, parsed.errors[1].message, OPEN_SQL_FOR_ALL_ENTRIES_AGGREGATE_MESSAGE)
	testing.expect_value(
		t,
		source[parsed.errors[1].range.start:parsed.errors[1].range.end],
		"COUNT( matnr )",
	)
	testing.expect_value(t, parsed.errors[2].message, OPEN_SQL_FOR_ALL_ENTRIES_AGGREGATE_MESSAGE)
	testing.expect_value(
		t,
		source[parsed.errors[2].range.start:parsed.errors[2].range.end],
		"COUNT( DISTINCT matnr )",
	)
}
