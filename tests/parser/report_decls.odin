package tests_parser

import "../../src/lang/ast"
import "../../src/lang/parser"
import "core:fmt"
import "core:testing"

@(test)
read_report_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `READ REPORT im_prog INTO buffer.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	read_stmt, ok := file.decls[0].derived_stmt.(^ast.Read_Report_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Read_Report_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(t, read_stmt.prog != nil, "Expected prog to be set")
	testing.expect(t, read_stmt.itab != nil, "Expected itab to be set")


	testing.expect(
		t,
		read_stmt.itab.name == "buffer",
		fmt.tprintf("Expected 'buffer', got '%s'", read_stmt.itab.name),
	)

	if prog_ident, iok := read_stmt.prog.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			prog_ident.name == "im_prog",
			fmt.tprintf("Expected 'im_prog', got '%s'", prog_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected prog to be Ident, got %T", read_stmt.prog.derived_expr),
		)
	}
}
