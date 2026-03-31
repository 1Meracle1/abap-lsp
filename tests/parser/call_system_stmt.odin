package tests_parser

import "../../src/lang/ast"
import "../../src/lang/parser"
import "core:fmt"
import "core:testing"

@(test)
call_system_dir_read_finish_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CALL 'C_DIR_READ_FINISH'                                "#EC CI_CCALL
      ID 'ERRNO'  FIELD ls_dir_list-errno
      ID 'ERRMSG' FIELD ls_dir_list-errmsg.`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)
	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 stmt, got %d", len(file.decls))) do return

	call_stmt, ok := file.decls[0].derived_stmt.(^ast.Call_System_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Call_System_Stmt, got %T", file.decls[0].derived_stmt)) do return

	if !testing.expect(t, len(call_stmt.params) == 2, fmt.tprintf("Expected 2 ID/FIELD pairs, got %d", len(call_stmt.params))) do return

	mod_lit, m_ok := call_stmt.module.derived_expr.(^ast.Basic_Lit)
	if testing.expect(t, m_ok, "Expected module as Basic_Lit") {
		testing.expect(t, mod_lit.tok.lit == `'C_DIR_READ_FINISH'`, fmt.tprintf("module lit %s", mod_lit.tok.lit))
	}

	p0 := call_stmt.params[0]
	id0, id0_ok := p0.id_name.derived_expr.(^ast.Basic_Lit)
	if testing.expect(t, id0_ok, "param0 id literal") {
		testing.expect(t, id0.tok.lit == `'ERRNO'`, fmt.tprintf("id0 %s", id0.tok.lit))
	}
	sel0, f0_ok := p0.field.derived_expr.(^ast.Selector_Expr)
	if testing.expect(t, f0_ok, "param0 field selector") {
		base0, b0_ok := sel0.expr.derived_expr.(^ast.Ident)
		if testing.expect(t, b0_ok, "param0 base ident") {
			testing.expect(t, base0.name == "ls_dir_list", fmt.tprintf("base %s", base0.name))
		}
		testing.expect(t, sel0.field.name == "errno", fmt.tprintf("field %s", sel0.field.name))
	}

	p1 := call_stmt.params[1]
	id1, id1_ok := p1.id_name.derived_expr.(^ast.Basic_Lit)
	if testing.expect(t, id1_ok, "param1 id literal") {
		testing.expect(t, id1.tok.lit == `'ERRMSG'`, fmt.tprintf("id1 %s", id1.tok.lit))
	}
	sel1, f1_ok := p1.field.derived_expr.(^ast.Selector_Expr)
	if testing.expect(t, f1_ok, "param1 field selector") {
		testing.expect(t, sel1.field.name == "errmsg", fmt.tprintf("field %s", sel1.field.name))
	}
}

@(test)
call_system_dir_read_start_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CALL 'C_DIR_READ_START'                                 "#EC CI_CCALL
        ID 'DIR'    FIELD lv_ph_fpath
        ID 'FILE'   FIELD '*'
        ID 'ERRNO'  FIELD ls_dir_list-errno
        ID 'ERRMSG' FIELD ls_dir_list-errmsg.`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)
	call_stmt, ok := file.decls[0].derived_stmt.(^ast.Call_System_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Call_System_Stmt, got %T", file.decls[0].derived_stmt)) do return
	testing.expect(t, len(call_stmt.params) == 4, fmt.tprintf("expected 4 params got %d", len(call_stmt.params)))

	file_lit, fl_ok := call_stmt.params[1].field.derived_expr.(^ast.Basic_Lit)
	if testing.expect(t, fl_ok, "FILE field should be literal") {
		testing.expect(t, file_lit.tok.lit == `'*'`, fmt.tprintf("FILE val %s", file_lit.tok.lit))
	}
}

@(test)
call_system_module_only_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CALL 'NO_PARAMS'.`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("errors: %v", file.syntax_errors))
	call_stmt, ok := file.decls[0].derived_stmt.(^ast.Call_System_Stmt)
	if !testing.expect(t, ok, "Call_System_Stmt") do return
	testing.expect(t, len(call_stmt.params) == 0, "no ID/FIELD pairs")
}
