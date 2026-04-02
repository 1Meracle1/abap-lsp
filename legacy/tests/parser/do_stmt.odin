package tests_parser

import "../../src/lang/ast"
import "../../src/lang/parser"
import "core:fmt"
import "core:testing"

@(test)
do_if_continue_exit_enddo_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `DO.
IF sy-subrc = 0.
CONTINUE.
ELSE.
EXIT.
ENDIF.
ENDDO.`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)
	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 stmt, got %d", len(file.decls))) do return

	do_stmt, ok := file.decls[0].derived_stmt.(^ast.Do_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Do_Stmt, got %T", file.decls[0].derived_stmt)) do return
	testing.expect(t, do_stmt.times == nil, "Expected unconditional DO (no TIMES)")
	if !testing.expect(t, len(do_stmt.body) == 1, fmt.tprintf("Expected 1 body stmt, got %d", len(do_stmt.body))) do return

	if_stmt, if_ok := do_stmt.body[0].derived_stmt.(^ast.If_Stmt)
	if !testing.expect(t, if_ok, fmt.tprintf("Expected If_Stmt in DO body, got %T", do_stmt.body[0].derived_stmt)) do return

	if !testing.expect(t, len(if_stmt.body) == 1, fmt.tprintf("Expected 1 IF branch stmt, got %d", len(if_stmt.body))) do return
	_, c_ok := if_stmt.body[0].derived_stmt.(^ast.Continue_Stmt)
	testing.expect(t, c_ok, fmt.tprintf("Expected CONTINUE, got %T", if_stmt.body[0].derived_stmt))

	if !testing.expect(t, len(if_stmt.else_body) == 1, fmt.tprintf("Expected 1 ELSE stmt, got %d", len(if_stmt.else_body))) do return
	_, e_ok := if_stmt.else_body[0].derived_stmt.(^ast.Exit_Stmt)
	testing.expect(t, e_ok, fmt.tprintf("Expected EXIT, got %T", if_stmt.else_body[0].derived_stmt))
}

@(test)
do_times_enddo_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `DO 5 TIMES.
ENDDO.`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)
	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 stmt, got %d", len(file.decls))) do return

	do_stmt, ok := file.decls[0].derived_stmt.(^ast.Do_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Do_Stmt, got %T", file.decls[0].derived_stmt)) do return
	testing.expect(t, do_stmt.times != nil, "Expected TIMES expression")
	testing.expect(t, len(do_stmt.body) == 0, "Expected empty DO body")
}
