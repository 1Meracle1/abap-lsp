package abap_frontend_parser

import "src:ast"

import "core:testing"

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
	testing.expect_value(t, bare_db.db_table_name, "zinsert_tab")
	testing.expect_value(t, source[bare_db.db_table_name_range.start:bare_db.db_table_name_range.end], "zinsert_tab")
	testing.expect(t, bare_db.from_table)
	testing.expect(t, bare_db.accepting_duplicate_keys)
	testing.expect_value(t, into_db.form, ast.Insert_Form.Db_Table)
	testing.expect(t, into_db.has_db_table_name)
	testing.expect_value(t, into_db.db_table_name, "zinto_tab")
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
	testing.expect_value(t, report.message_id, "zmsg")
	testing.expect_value(t, source[report.message_id_range.start:report.message_id_range.end], "zmsg")
	testing.expect(t, program.has_message_id)
	testing.expect_value(t, program.message_id, "zcls")
	testing.expect_value(t, source[program.message_id_range.start:program.message_id_range.end], "zcls")
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
	testing.expect_value(t, first.names[0].name, "zinc")
	testing.expect(t, !first.if_found)
	testing.expect_value(t, source[first.names[0].range.start:first.names[0].range.end], "zinc")
	testing.expect_value(t, len(chained.names), 2)
	testing.expect_value(t, chained.names[0].name, "ztop")
	testing.expect_value(t, chained.names[1].name, "zf01")
	testing.expect_value(t, include_type.types[0].kind, ast.Decl_Clause_Kind.Include_Type)
	testing.expect_value(t, include_structure.types[0].kind, ast.Decl_Clause_Kind.Include_Structure)
}

@(test)
program_include_if_found_keeps_concrete_node :: proc(t: ^testing.T) {
	source := `INCLUDE zabapgit_user_exit IF FOUND.`
	parsed := parse(source, "include_if_found.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Include_Stmt)
	testing.expect_value(t, stmt.names[0].name, "zabapgit_user_exit")
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
		testing.expect_value(t, stmt.fields[i].name, name)
	}
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
	testing.expect_value(t, stmt.fields[0].name, "definition-component_name")
	testing.expect_value(t, stmt.fields[1].name, "definition-view_name")
	testing.expect_value(t, first_base.name, "definition")
	testing.expect_value(t, first_field.name, "component_name")
	testing.expect(t, stmt.fields[0].ascending)
	testing.expect(t, stmt.fields[1].ascending)
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
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
read_table_and_delete_model_pseudo_components :: proc(t: ^testing.T) {
	source := `READ TABLE itab WITH KEY table_line = '*' TRANSPORTING NO FIELDS.
DELETE ADJACENT DUPLICATES FROM itab COMPARING ALL FIELDS.`
	parsed := parse(source, "data_access_pseudo_components.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	read := parsed.root.stmts[0].derived_stmt.(^ast.Read_Table_Stmt)
	delete_stmt := parsed.root.stmts[1].derived_stmt.(^ast.Delete_Stmt)

	testing.expect_value(t, len(read.entries[0].key_values), 1)
	key := read.entries[0].key_values[0]
	testing.expect_value(t, key.name, "table_line")
	testing.expect_value(t, source[key.name_range.start:key.name_range.end], "table_line")
	testing.expect(t, key.table_line)
	testing.expect_value(t, len(delete_stmt.comparing), 1)
	testing.expect(t, delete_stmt.comparing[0].all_fields)
	testing.expect_value(t, source[delete_stmt.comparing[0].range.start:delete_stmt.comparing[0].range.end], "ALL FIELDS")
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
	testing.expect_value(t, entry.key_name, "iso2")
	testing.expect_value(t, len(entry.key_values), 1)
	testing.expect_value(t, entry.key_values[0].name, "langshort")
	testing.expect_value(t, source[entry.key_values[0].name_range.start:entry.key_values[0].name_range.end], "langshort")
	testing.expect_value(t, ast.print_node(parsed.root, context.allocator), source)
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
	testing.expect_value(t, set_update.assignments[0].column_name, "status")
	testing.expect_value(t, source[set_update.assignments[0].column_range.start:set_update.assignments[0].column_range.end], "status")
	testing.expect_value(t, set_update.assignments[1].column_name, "changed_at")
	testing.expect(t, set_update.dynamic_where)
	testing.expect_value(t, source[set_update.where_clause.start:set_update.where_clause.end], "WHERE (lv_where)")
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
	testing.expect_value(t, bare_insert.form, ast.Insert_Form.Db_Table)
	testing.expect(t, bare_insert.target != nil)
	testing.expect_value(t, source[bare_insert.db_source_range.start:bare_insert.db_source_range.end], "zinsert_tab")
	testing.expect(t, bare_insert.from_table)
	testing.expect(t, bare_insert.accepting_duplicate_keys)
	testing.expect_value(t, into_insert.form, ast.Insert_Form.Db_Table)
	testing.expect(t, into_insert.into_db_table)
	testing.expect(t, into_insert.values_clause)
	testing.expect_value(t, into_insert.db_table_name, "zinto_tab")
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
	source := `SELECT SINGLE matnr FROM mara INTO DATA(lv_matnr) WHERE matnr = lv_key.
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
	testing.expect(t, open_dataset.text_mode)
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
	source := `SELECT matnr FROM mara INTO @DATA(lv_matnr) WHERE matnr = @lv_key.`
	parsed := parse(source, "sql_host.abap", context.allocator)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.host_expr, 2)
	printed := ast.print_node(parsed.root, context.allocator)
	testing.expect_value(t, printed, "SELECT matnr FROM mara INTO @DATA(lv_matnr) WHERE matnr = @lv_key.")
}

@(test)
open_sql_projection_source_and_set_fields :: proc(t: ^testing.T) {
	source := `SELECT a~matnr AS material, b~maktx AS text FROM mara AS a INNER JOIN makt AS b ON b~matnr = a~matnr INTO TABLE @lt_rows UNION ALL SELECT matnr FROM zmara INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_shape.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect_value(t, len(stmt.query.projection_clauses), 2)
	testing.expect_value(t, stmt.query.projection_clauses[0].alias, "material")
	testing.expect(t, stmt.query.source_clause != nil)
	testing.expect_value(t, stmt.query.source_clause.alias, "a")
	testing.expect_value(t, len(stmt.query.source_clause.joins), 1)
	testing.expect_value(t, stmt.query.source_clause.joins[0].kind, ast.Select_Join_Kind.Inner)
	testing.expect_value(t, stmt.query.source_clause.joins[0].alias, "b")
	testing.expect(t, stmt.query.source_clause.joins[0].on != nil)
	testing.expect_value(t, len(stmt.query.set_ops), 1)
	testing.expect_value(t, stmt.query.set_ops[0].kind, ast.Select_Set_Kind.Union)
	testing.expect(t, stmt.query.set_ops[0].all)
	printed := ast.print_node(parsed.root, context.allocator)
	testing.expect_value(t, printed, "SELECT a~matnr AS material, b~maktx AS text FROM mara AS a INNER JOIN makt AS b ON b~matnr = a~matnr INTO TABLE @lt_rows UNION ALL SELECT matnr FROM zmara INTO TABLE @lt_rows.")
}

@(test)
open_sql_projection_source_and_result_operands_are_modeled :: proc(t: ^testing.T) {
	source := `SELECT a~matnr AS material, a~* FROM mara AS a INTO CORRESPONDING FIELDS OF TABLE @DATA(lt_rows).
SELECT (lv_fields) FROM (lv_table) AS d INTO (lv_target) WHERE (lv_where).
SELECT matnr FROM @lt_source AS s INTO FIELD-SYMBOL(<row>).`
	parsed := parse(source, "sql_operand_shapes.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	qualified := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect_value(t, len(qualified.query.projection_clauses), 2)
	testing.expect_value(t, qualified.query.projection_clauses[0].alias, "material")
	_, qualified_star := qualified.query.projection_clauses[1].value.derived_expr.(^ast.Selector_Expr)
	testing.expect(t, qualified_star)
	testing.expect_value(t, qualified.query.source_clause.alias, "a")
	testing.expect(t, qualified.query.result.table)
	testing.expect(t, qualified.query.result.corresponding_fields)
	_, inline_target := qualified.query.result.target.derived_expr.(^ast.Host_Expr)
	testing.expect(t, inline_target)

	dynamic_stmt := parsed.root.stmts[1].derived_stmt.(^ast.Select_Stmt)
	_, dynamic_projection := dynamic_stmt.query.projection_clauses[0].value.derived_expr.(^ast.Paren_Expr)
	_, dynamic_source := dynamic_stmt.query.source_clause.source.derived_expr.(^ast.Paren_Expr)
	_, dynamic_target := dynamic_stmt.query.result.target.derived_expr.(^ast.Type_Ref_Expr)
	testing.expect(t, dynamic_projection)
	testing.expect(t, dynamic_source)
	testing.expect(t, dynamic_stmt.query.dynamic_where)
	testing.expect(t, dynamic_target)

	host_source := parsed.root.stmts[2].derived_stmt.(^ast.Select_Stmt)
	_, host_source_expr := host_source.query.source_clause.source.derived_expr.(^ast.Host_Expr)
	_, fs_target := host_source.query.result.target.derived_expr.(^ast.Field_Symbol_Inline_Name_Expr)
	testing.expect(t, host_source_expr)
	testing.expect_value(t, host_source.query.source_clause.alias, "s")
	testing.expect(t, fs_target)
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
	testing.expect_value(t, stmt.query.source_clause.alias, "a")
	testing.expect_value(t, stmt.query.source_clause.joins[0].alias, "b")
	testing.expect_value(t, source[stmt.query.where_clause.start:stmt.query.where_clause.end], "WHERE ( a~trstatus = 'D' OR a~trstatus = 'L' )\n    AND b~pgmid = iv_program_id")
}

@(test)
open_sql_invalid_aliases_and_partial_joins_are_diagnosed :: proc(t: ^testing.T) {
	source := `SELECT carrid AS FROM mara AS WHERE carrid = @lv_carrid INTO TABLE @lt_rows.
SELECT * FROM mara INNER WHERE matnr = @lv_matnr INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_invalid_alias_join.abap", context.allocator)

	testing.expect(t, len(parsed.errors) >= 3)
	alias_stmt := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect_value(t, alias_stmt.query.projection_clauses[0].alias, "")
	testing.expect_value(t, alias_stmt.query.source_clause.alias, "")
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
	testing.expect_value(t, stmt.with.entries[0].name, "+recent")
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
	source := `SELECT a~* FROM mara AS a INTO TABLE @lt_rows WHERE a~matnr = @lv_matnr GROUP BY a~matnr HAVING COUNT( * ) > 0 ORDER BY a~matnr, a~ersda UP TO 10 ROWS PACKAGE SIZE lv_size OFFSET 2 BYPASSING BUFFER CONNECTION con CLIENT SPECIFIED.`
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
	testing.expect_value(t, source[query.having_clause.start:query.having_clause.end], "HAVING COUNT( * ) > 0")
	testing.expect_value(t, source[query.order_by_clause.start:query.order_by_clause.end], "ORDER BY a~matnr, a~ersda")
	testing.expect_value(t, len(query.order_by_fields), 2)
	testing.expect_value(t, query.order_by_fields[0], "matnr")
	testing.expect_value(t, query.order_by_fields[1], "ersda")
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
SELECT * FROM mara WHERE matnr NOT LIKE lv_pattern INTO TABLE @lt_rows.`
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
	testing.expect_value(t, stmt.query.projection_clauses[0].alias, "value")
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
	source := `SELECT matnr INTO @DATA(lv_old) FROM mara WHERE matnr = @lv_key.
SELECT matnr FROM mara INTO @DATA(lv_new) WHERE matnr = @lv_key.`
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
	projection := stmt.query.projection_clauses[0].value.derived_expr.(^ast.Ident_Expr)
	testing.expect_value(t, projection.name, "value")
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
SELECT trkorr FROM e070 INTO TABLE @lt_rows UP TO 10 ROWS WHERE trkorr = @lv_trkorr.
SELECT trkorr FROM e070 UP TO 10 ROWS INTO TABLE @lt_rows WHERE trkorr = @lv_trkorr.`
	parsed := parse(source, "sql_sap_valid_tail_order.abap", context.allocator)

	testing.expect_value(t, len(parsed.errors), 0)
	order_before_result := parsed.root.stmts[0].derived_stmt.(^ast.Select_Stmt)
	result_before_up_to := parsed.root.stmts[1].derived_stmt.(^ast.Select_Stmt)
	up_to_before_result := parsed.root.stmts[2].derived_stmt.(^ast.Select_Stmt)

	testing.expect_value(t, source[order_before_result.query.order_by_clause.start:order_before_result.query.order_by_clause.end], "ORDER BY trkorr")
	testing.expect_value(t, source[order_before_result.query.into_clause.start:order_before_result.query.into_clause.end], "INTO TABLE @lt_rows")
	testing.expect_value(t, source[result_before_up_to.query.into_clause.start:result_before_up_to.query.into_clause.end], "INTO TABLE @lt_rows")
	testing.expect_value(t, source[result_before_up_to.query.up_to_clause.start:result_before_up_to.query.up_to_clause.end], "UP TO 10 ROWS")
	testing.expect_value(t, source[result_before_up_to.query.where_clause.start:result_before_up_to.query.where_clause.end], "WHERE trkorr = @lv_trkorr")
	testing.expect_value(t, source[up_to_before_result.query.up_to_clause.start:up_to_before_result.query.up_to_clause.end], "UP TO 10 ROWS")
	testing.expect_value(t, source[up_to_before_result.query.into_clause.start:up_to_before_result.query.into_clause.end], "INTO TABLE @lt_rows")
	testing.expect_value(t, source[up_to_before_result.query.where_clause.start:up_to_before_result.query.where_clause.end], "WHERE trkorr = @lv_trkorr")
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
	testing.expect_value(t, stmt.query.order_by_fields[0], "matnr")
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
