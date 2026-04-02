package tests_parser

import "../../src/lang/ast"
import "../../src/lang/parser"
import "core:fmt"
import "core:testing"

expect_translate_stmt :: proc(
	t: ^testing.T,
	src: string,
	loc := #caller_location,
) -> ^ast.Translate_Stmt {
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
	stmt, ok := file.decls[0].derived_stmt.(^ast.Translate_Stmt)
	if !testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Translate_Stmt, got %T", file.decls[0].derived_stmt),
		loc = loc,
	) {
		return nil
	}
	return stmt
}

@(test)
translate_upper_case_test :: proc(t: ^testing.T) {
	src := `TRANSLATE lv_file_ext_filter TO UPPER CASE.`
	stmt := expect_translate_stmt(t, src)
	if stmt == nil do return
	testing.expect(t, stmt.kind == ast.Translate_Kind.Upper_Case, "kind upper")
	if id, ok := stmt.target.derived_expr.(^ast.Ident); ok {
		testing.expect(t, id.name == "lv_file_ext_filter", "target ident")
	} else {
		testing.expect(t, false, "target ident")
	}
	testing.expect(t, stmt.using_pattern == nil, "no using pattern")
}

@(test)
translate_lower_case_test :: proc(t: ^testing.T) {
	src := `TRANSLATE ev_hash TO LOWER CASE.`
	stmt := expect_translate_stmt(t, src)
	if stmt == nil do return
	testing.expect(t, stmt.kind == ast.Translate_Kind.Lower_Case, "kind lower")
	if id, ok := stmt.target.derived_expr.(^ast.Ident); ok {
		testing.expect(t, id.name == "ev_hash", "target ident")
	} else {
		testing.expect(t, false, "target ident")
	}
}

@(test)
translate_using_map_test :: proc(t: ^testing.T) {
	src := `TRANSLATE lv_class_name USING ' ='.`
	stmt := expect_translate_stmt(t, src)
	if stmt == nil do return
	testing.expect(t, stmt.kind == ast.Translate_Kind.Using, "kind using")
	if id, ok := stmt.target.derived_expr.(^ast.Ident); ok {
		testing.expect(t, id.name == "lv_class_name", "target ident")
	} else {
		testing.expect(t, false, "target ident")
	}
	if lit, ok := stmt.using_pattern.derived_expr.(^ast.Basic_Lit); ok {
		testing.expect(t, lit.tok.lit == "' ='", fmt.tprintf("pattern %s", lit.tok.lit))
	} else {
		testing.expect(t, false, "using pattern literal")
	}
}
