package abap_frontend_parser

import "../ast"

import "base:runtime"
import "core:testing"

@(test)
statement_batch_open_sql_and_data_access :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
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
	parsed := parse(source, "data_access.abap", alloc)
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
aggregate_select_and_compact_cleanup_call_keep_boundaries :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
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
	parsed := parse(source, "select_cleanup_boundaries.abap", alloc)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.loop_stmt, 1)
	testing.expect_value(t, counts.select_stmt, 1)
	testing.expect_value(t, counts.try_stmt, 1)
}

@(test)
internal_table_append_modify_and_sort_keep_nodes :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `APPEND lx_error->get_text( ) TO mt_text.
APPEND LINES OF li_package->list_subpackages( ) TO lt_packages.
MODIFY (c_tabname) FROM ls_content.
MODIFY lt_table FROM ls_line TRANSPORTING value WHERE key = lv_key.
SORT lt_table BY name DESCENDING.`
	parsed := parse(source, "itab_surface.abap", alloc)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.append_stmt, 2)
	testing.expect_value(t, counts.modify_stmt, 2)
	testing.expect_value(t, counts.sort_stmt, 1)
}

@(test)
data_access_statements_keep_concrete_fields :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `READ TABLE itab INTO DATA(row) WITH KEY id = lv_id TRANSPORTING NO FIELDS.
INSERT wa INTO TABLE itab INDEX idx ASSIGNING FIELD-SYMBOL(<row>).
UPDATE mara SET matnr = lv_new WHERE matnr = lv_old.
DELETE ADJACENT DUPLICATES FROM itab COMPARING matnr.`
	parsed := parse(source, "data_access_fields.abap", alloc)

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
cursor_dataset_report_and_textpool_fields :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
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
	parsed := parse(source, "surface_fields.abap", alloc)

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
	alloc := runtime.heap_allocator()
	source := `EXEC SQL.
  SELECT * FROM mara
ENDEXEC.
GENERATE SUBROUTINE POOL lt_source NAME lv_prog MESSAGE lv_msg LINE lv_line WORD lv_word OFFSET lv_off.
GENERATE DYNPRO lv_prog lv_dynpro.
READ LINE lv_line FIELD VALUE mara-matnr INTO lv_matnr.
MODIFY CURRENT LINE FIELD VALUE mara-matnr INTO lv_matnr.`
	parsed := parse(source, "islands_generated_line.abap", alloc)
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
	alloc := runtime.heap_allocator()
	source := `SELECT matnr FROM mara INTO @DATA(lv_matnr) WHERE matnr = @lv_key.`
	parsed := parse(source, "sql_host.abap", alloc)
	counts := count_nodes(parsed.root)

	testing.expect_value(t, len(parsed.errors), 0)
	testing.expect_value(t, counts.host_expr, 2)
	printed := ast.print_node(parsed.root, alloc)
	testing.expect_value(t, printed, "SELECT matnr FROM mara INTO @DATA(lv_matnr) WHERE matnr = @lv_key.")
}

@(test)
open_sql_projection_source_and_set_fields :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `SELECT a~matnr AS material, b~maktx AS text FROM mara AS a INNER JOIN makt AS b ON b~matnr = a~matnr INTO TABLE @lt_rows UNION ALL SELECT matnr FROM zmara INTO TABLE @lt_rows.`
	parsed := parse(source, "sql_shape.abap", alloc)

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
	printed := ast.print_node(parsed.root, alloc)
	testing.expect_value(t, printed, "SELECT a~matnr AS material, b~maktx AS text FROM mara AS a INNER JOIN makt AS b ON b~matnr = a~matnr INTO TABLE @lt_rows UNION ALL SELECT matnr FROM zmara INTO TABLE @lt_rows.")
}

@(test)
open_sql_ctes_and_dynamic_sources_keep_formatter_fields :: proc(t: ^testing.T) {
	alloc := runtime.heap_allocator()
	source := `WITH +recent AS ( SELECT matnr FROM mara WHERE matnr = @lv_matnr ) SELECT matnr FROM (lv_source) AS s INNER JOIN @lt_keys AS k ON k~matnr = s~matnr INTO TABLE @lt_rows WHERE (lv_where).`
	parsed := parse(source, "sql_cte_dynamic.abap", alloc)

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

	printed := ast.print_node(parsed.root, alloc)
	testing.expect_value(t, printed, "WITH +recent AS ( SELECT matnr FROM mara WHERE matnr = @lv_matnr ) SELECT matnr FROM ( lv_source ) AS s INNER JOIN @lt_keys AS k ON k~matnr = s~matnr INTO TABLE @lt_rows WHERE ( lv_where ).")
}
