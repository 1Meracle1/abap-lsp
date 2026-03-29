package tests_parser

import "../../src/lang/ast"
import "../../src/lang/parser"
import "core:fmt"
import "core:testing"

parse_single_assign_stmt :: proc(
	t: ^testing.T,
	src: string,
	loc := #caller_location,
) -> ^ast.Assign_Field_Symbol_Stmt {
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

	stmt, ok := file.decls[0].derived_stmt.(^ast.Assign_Field_Symbol_Stmt)
	if !testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Assign_Field_Symbol_Stmt, got %T", file.decls[0].derived_stmt),
		loc = loc,
	) {
		return nil
	}

	return stmt
}

@(test)
assign_stmt_with_object_deref_and_inline_field_symbol_test :: proc(t: ^testing.T) {
	stmt := parse_single_assign_stmt(
		t,
		`ASSIGN mo_outbound->* TO FIELD-SYMBOL(<ls_outbound>).`,
	)
	if stmt == nil do return

	testing.expect(t, !stmt.is_dynamic, "Expected static ASSIGN")
	testing.expect(t, !stmt.is_table_field, "Did not expect TABLE FIELD")

	source, ok := stmt.source.derived_expr.(^ast.Selector_Expr)
	if !testing.expect(t, ok, fmt.tprintf("Expected Selector_Expr, got %T", stmt.source.derived_expr)) do return

	base, bok := source.expr.derived_expr.(^ast.Ident)
	if testing.expect(t, bok, fmt.tprintf("Expected Ident base, got %T", source.expr.derived_expr)) {
		testing.expect(t, base.name == "mo_outbound", fmt.tprintf("Expected 'mo_outbound', got '%s'", base.name))
	}
	testing.expect(t, source.op.kind == .Arrow, fmt.tprintf("Expected Arrow, got %v", source.op.kind))
	testing.expect(t, source.field.name == "*", fmt.tprintf("Expected '*', got '%s'", source.field.name))

	target, tok := stmt.target.derived_expr.(^ast.Ident)
	if testing.expect(t, tok, fmt.tprintf("Expected Ident target, got %T", stmt.target.derived_expr)) {
		testing.expect(t, target.name == "<ls_outbound>", fmt.tprintf("Expected '<ls_outbound>', got '%s'", target.name))
	}
}

@(test)
assign_stmt_with_static_subfield_offset_and_length_test :: proc(t: ^testing.T) {
	stmt := parse_single_assign_stmt(t, `ASSIGN date+0(4) TO <year>.`)
	if stmt == nil do return

	source, sok := stmt.source.derived_expr.(^ast.Ident)
	if testing.expect(t, sok, fmt.tprintf("Expected Ident source, got %T", stmt.source.derived_expr)) {
		testing.expect(t, source.name == "date", fmt.tprintf("Expected 'date', got '%s'", source.name))
	}

	offset, ook := stmt.offset.derived_expr.(^ast.Basic_Lit)
	if testing.expect(t, ook, fmt.tprintf("Expected literal offset, got %T", stmt.offset.derived_expr)) {
		testing.expect(t, offset.tok.lit == "0", fmt.tprintf("Expected offset '0', got '%s'", offset.tok.lit))
	}

	length, lok := stmt.length.derived_expr.(^ast.Basic_Lit)
	if testing.expect(t, lok, fmt.tprintf("Expected literal length, got %T", stmt.length.derived_expr)) {
		testing.expect(t, length.tok.lit == "4", fmt.tprintf("Expected length '4', got '%s'", length.tok.lit))
	}
	testing.expect(t, !stmt.length_is_star, "Did not expect '*' length")
}

@(test)
assign_stmt_with_selector_source_test :: proc(t: ^testing.T) {
	stmt := parse_single_assign_stmt(t, `ASSIGN line2-col2 TO <f2>.`)
	if stmt == nil do return

	source, ok := stmt.source.derived_expr.(^ast.Selector_Expr)
	if !testing.expect(t, ok, fmt.tprintf("Expected Selector_Expr, got %T", stmt.source.derived_expr)) do return

	base, bok := source.expr.derived_expr.(^ast.Ident)
	if testing.expect(t, bok, fmt.tprintf("Expected Ident base, got %T", source.expr.derived_expr)) {
		testing.expect(t, base.name == "line2", fmt.tprintf("Expected 'line2', got '%s'", base.name))
	}
	testing.expect(t, source.op.kind == .Minus, fmt.tprintf("Expected Minus selector, got %v", source.op.kind))
	testing.expect(t, source.field.name == "col2", fmt.tprintf("Expected 'col2', got '%s'", source.field.name))
}

@(test)
assign_stmt_with_literal_source_test :: proc(t: ^testing.T) {
	stmt := parse_single_assign_stmt(t, `ASSIGN 'LINE2-COL2 =' TO <f1>.`)
	if stmt == nil do return

	source, ok := stmt.source.derived_expr.(^ast.Basic_Lit)
	if !testing.expect(t, ok, fmt.tprintf("Expected Basic_Lit, got %T", stmt.source.derived_expr)) do return

	testing.expect(
		t,
		source.tok.lit == "'LINE2-COL2 ='",
		fmt.tprintf("Expected literal ''LINE2-COL2 ='', got %s", source.tok.lit),
	)
}

@(test)
assign_stmt_with_variable_offset_test :: proc(t: ^testing.T) {
	stmt := parse_single_assign_stmt(t, `ASSIGN line-a+off(1) TO <fs>.`)
	if stmt == nil do return

	source, ok := stmt.source.derived_expr.(^ast.Selector_Expr)
	if !testing.expect(t, ok, fmt.tprintf("Expected Selector_Expr, got %T", stmt.source.derived_expr)) do return

	testing.expect(t, source.field.name == "a", fmt.tprintf("Expected 'a', got '%s'", source.field.name))

	offset, ook := stmt.offset.derived_expr.(^ast.Ident)
	if testing.expect(t, ook, fmt.tprintf("Expected Ident offset, got %T", stmt.offset.derived_expr)) {
		testing.expect(t, offset.name == "off", fmt.tprintf("Expected 'off', got '%s'", offset.name))
	}

	length, lok := stmt.length.derived_expr.(^ast.Basic_Lit)
	if testing.expect(t, lok, fmt.tprintf("Expected literal length, got %T", stmt.length.derived_expr)) {
		testing.expect(t, length.tok.lit == "1", fmt.tprintf("Expected length '1', got '%s'", length.tok.lit))
	}
}

@(test)
assign_stmt_table_field_dynamic_test :: proc(t: ^testing.T) {
	stmt := parse_single_assign_stmt(t, `ASSIGN TABLE FIELD (dobj) TO <fs>.`)
	if stmt == nil do return

	testing.expect(t, stmt.is_dynamic, "Expected dynamic ASSIGN")
	testing.expect(t, stmt.is_table_field, "Expected TABLE FIELD ASSIGN")

	source, ok := stmt.source.derived_expr.(^ast.Paren_Expr)
	if !testing.expect(t, ok, fmt.tprintf("Expected Paren_Expr source, got %T", stmt.source.derived_expr)) do return

	name, nok := source.expr.derived_expr.(^ast.Ident)
	if testing.expect(t, nok, fmt.tprintf("Expected Ident in dynamic source, got %T", source.expr.derived_expr)) {
		testing.expect(t, name.name == "dobj", fmt.tprintf("Expected 'dobj', got '%s'", name.name))
	}
}
