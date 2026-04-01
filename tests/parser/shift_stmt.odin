package tests_parser

import "../../src/lang/ast"
import "../../src/lang/parser"
import "core:fmt"
import "core:testing"

@(test)
shift_left_deleting_leading_space_test :: proc(t: ^testing.T) {
	src := `SHIFT lv_domvalue LEFT DELETING LEADING space.`
	file := ast.new(ast.File, {})
	file.src = src
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)
	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	stmt, ok := file.decls[0].derived_stmt.(^ast.Shift_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Shift_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(t, stmt.by_places == nil, "no BY PLACES")
	testing.expect(t, stmt.direction == ast.Shift_Direction.Left, "LEFT")
	testing.expect(t, !stmt.circular, "not circular")
	testing.expect(t, stmt.deleting == ast.Shift_Deleting_Kind.Leading, "LEADING")
	if id, iok := stmt.target.derived_expr.(^ast.Ident); iok {
		testing.expect(t, id.name == "lv_domvalue", "target")
	} else {
		testing.expect(t, false, "target ident")
	}
	if mid, mok := stmt.deleting_mask.derived_expr.(^ast.Ident); mok {
		testing.expect(t, mid.name == "space", "deleting mask")
	} else {
		testing.expect(t, false, "mask ident")
	}
}

@(test)
shift_right_deleting_trailing_literal_test :: proc(t: ^testing.T) {
	src := `SHIFT lv_x RIGHT DELETING TRAILING '.'.`
	file := ast.new(ast.File, {})
	file.src = src
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)
	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	stmt, ok := file.decls[0].derived_stmt.(^ast.Shift_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Shift_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(t, stmt.direction == ast.Shift_Direction.Right, "RIGHT")
	testing.expect(t, stmt.deleting == ast.Shift_Deleting_Kind.Trailing, "TRAILING")
	if lit, lok := stmt.deleting_mask.derived_expr.(^ast.Basic_Lit); lok {
		testing.expect(t, lit.tok.lit == "'.'", "literal mask")
	} else {
		testing.expect(t, false, "literal mask")
	}
}

@(test)
shift_by_places_left_circular_test :: proc(t: ^testing.T) {
	src := `SHIFT str BY 2 PLACES LEFT CIRCULAR.`
	file := ast.new(ast.File, {})
	file.src = src
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)
	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	stmt, ok := file.decls[0].derived_stmt.(^ast.Shift_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Shift_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(t, stmt.by_places != nil, "by places")
	testing.expect(t, stmt.direction == ast.Shift_Direction.Left, "LEFT after PLACES")
	testing.expect(t, stmt.circular, "CIRCULAR")
	testing.expect(t, stmt.deleting == ast.Shift_Deleting_Kind.None, "no deleting")
	testing.expect(t, stmt.deleting_mask == nil, "no mask")
}

@(test)
shift_simple_left_test :: proc(t: ^testing.T) {
	src := `SHIFT lv_word LEFT.`
	file := ast.new(ast.File, {})
	file.src = src
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)
	stmt, ok := file.decls[0].derived_stmt.(^ast.Shift_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Shift_Stmt, got %T", file.decls[0].derived_stmt)) do return
	testing.expect(t, stmt.direction == ast.Shift_Direction.Left, "LEFT only")
	testing.expect(t, stmt.deleting == ast.Shift_Deleting_Kind.None, "no deleting")
}
