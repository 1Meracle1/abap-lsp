package tests_parser

import "../../src/lang/ast"
import "../../src/lang/parser"
import "core:fmt"
import "core:testing"

expect_write_stmt :: proc(
	t: ^testing.T,
	src: string,
	want_operands: int,
	loc := #caller_location,
) -> ^ast.Write_Stmt {
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	if !testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
		loc = loc,
	) {
		return nil
	}
	if !testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %d", len(file.decls)),
		loc = loc,
	) {
		return nil
	}
	stmt, ok := file.decls[0].derived_stmt.(^ast.Write_Stmt)
	if !testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Write_Stmt, got %T", file.decls[0].derived_stmt),
		loc = loc,
	) {
		return nil
	}
	if !testing.expect(
		t,
		len(stmt.operands) == want_operands,
		fmt.tprintf("Expected %d operands, got %d", want_operands, len(stmt.operands)),
		loc = loc,
	) {
		return nil
	}
	return stmt
}

@(test)
write_to_multiline_options_test :: proc(t: ^testing.T) {
	src :=
		`WRITE lv_number
    TO    cs_encode_decode-obj_ids-serial
    LEFT-JUSTIFIED
    NO-GROUPING
    NO-SIGN.`
	stmt := expect_write_stmt(t, src, 1)
	if stmt == nil do return
	op := stmt.operands[0]
	testing.expect(t, op.line_feed == false, "line_feed")
	testing.expect(t, op.no_grouping, "NO-GROUPING")
	testing.expect(t, op.no_sign, "NO-SIGN")
	testing.expect(t, op.left_justified, "LEFT-JUSTIFIED")
	if id, ok := op.data.derived_expr.(^ast.Ident); ok {
		testing.expect(t, id.name == "lv_number", fmt.tprintf("data %s", id.name))
	} else {
		testing.expect(t, false, fmt.tprintf("expected Ident data, got %T", op.data.derived_expr))
	}
}

@(test)
write_chain_slash_and_format_width_test :: proc(t: ^testing.T) {
	src := `WRITE: / lv_text.`
	stmt := expect_write_stmt(t, src, 1)
	if stmt == nil do return
	op := stmt.operands[0]
	testing.expect(t, op.line_feed, "slash")
	id, ok := op.data.derived_expr.(^ast.Ident)
	if testing.expect(t, ok, "ident") {
		testing.expect(t, id.name == "lv_text")
	}

	src2 := `WRITE: /(20) ls_data-matnr,
        (30) ls_data-maktx,
        (10) ls_data-menge RIGHT-JUSTIFIED.`
	stmt2 := expect_write_stmt(t, src2, 3)
	if stmt2 == nil do return

	op0 := stmt2.operands[0]
	testing.expect(t, op0.line_feed, "op0 line feed")
	if op0.format_len != nil {
		if lit, ok2 := op0.format_len.derived_expr.(^ast.Basic_Lit); ok2 {
			testing.expect(t, lit.tok.lit == "20", fmt.tprintf("format len %s", lit.tok.lit))
		}
	} else {
		testing.expect(t, false, "missing format len")
	}

	op2 := stmt2.operands[2]
	testing.expect(t, op2.right_justified, fmt.tprintf("RIGHT-JUSTIFIED third op: flags L=%v R=%v", op2.left_justified, op2.right_justified))
}

@(test)
write_one_operand_right_justified_simple_test :: proc(t: ^testing.T) {
	src := `WRITE: (10) ls_data-menge RIGHT-JUSTIFIED.`
	stmt := expect_write_stmt(t, src, 1)
	if stmt == nil do return
	testing.expect(t, stmt.operands[0].right_justified, "simple RIGHT-JUSTIFIED")
}

@(test)
write_to_decimals_time_zone_offsets_test :: proc(t: ^testing.T) {
	src := `WRITE ls_rtime-dur TO lv_msgnum DECIMALS 1 LEFT-JUSTIFIED.`
	stmt := expect_write_stmt(t, src, 1)
	if stmt == nil do return
	op := stmt.operands[0]
	testing.expect(t, op.left_justified)
	testing.expect(t, op.decimals != nil)
	testing.expect(t, op.to_target != nil)

	src2 := `WRITE iv_timestamp TO rv_timestamp_display TIME ZONE iv_tzone.`
	stmt2 := expect_write_stmt(t, src2, 1)
	if stmt2 == nil do return
	testing.expect(t, stmt2.operands[0].time_zone != nil)

	src3 := `WRITE: iv_date+0(4) TO ls_dat-yyyy.
WRITE '0' TO ls_dat-mm+0(1).`
	file := ast.new(ast.File, {})
	file.src = src3
	p: parser.Parser
	parser.parse_file(&p, file)
	if !testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("errs %v", file.syntax_errors)) do return
	if !testing.expect(t, len(file.decls) == 2, fmt.tprintf("decls %d", len(file.decls))) do return
	_, ok1 := file.decls[0].derived_stmt.(^ast.Write_Stmt)
	_, ok2 := file.decls[1].derived_stmt.(^ast.Write_Stmt)
	testing.expect(t, ok1 && ok2, "both write stmts")
}

@(test)
write_field_symbol_and_message_vars_test :: proc(t: ^testing.T) {
	src :=
		`WRITE <ls_snr_nsp_result>-nrobj_ver LEFT-JUSTIFIED TO lv_msg_chr.
WRITE cv_utc_tstmp_old_state     TO sy-msgv1.`
	file := ast.new(ast.File, {})
	file.src = src
	p: parser.Parser
	parser.parse_file(&p, file)
	if !testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("%v", file.syntax_errors)) do return
	if !testing.expect(t, len(file.decls) == 2, fmt.tprintf("decls %d", len(file.decls))) do return
}

@(test)
write_many_one_liners_parse_test :: proc(t: ^testing.T) {
	src :=
		`WRITE lv_newmm TO  ls_dat-mm+1(1).
WRITE lv_newmm TO  ls_dat-mm.
WRITE '0' TO ls_hdat-mm+0(1).
WRITE lv_parent_bupno TO lv_msgv.
WRITE es_matmap-matnr TO lv_matnr.`
	file := ast.new(ast.File, {})
	file.src = src
	p: parser.Parser
	parser.parse_file(&p, file)
	if !testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("%v", file.syntax_errors)) do return
	if !testing.expect(t, len(file.decls) == 5, fmt.tprintf("decls %d", len(file.decls))) do return
	for ds in file.decls {
		if _, ok := ds.derived_stmt.(^ast.Write_Stmt); !ok {
			testing.expect(t, false, fmt.tprintf("want Write_Stmt, got %T", ds.derived_stmt))
			return
		}
	}
}
