package tests_parser

import "../../src/lang/ast"
import lexer "../../src/lang/lexer"
import "../../src/lang/parser"
import "core:fmt"
import "core:testing"

expect_replace_stmt :: proc(
	t: ^testing.T,
	src: string,
	loc := #caller_location,
) -> ^ast.Replace_Stmt {
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
	stmt, ok := file.decls[0].derived_stmt.(^ast.Replace_Stmt)
	if !testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Replace_Stmt, got %T", file.decls[0].derived_stmt),
		loc = loc,
	) {
		return nil
	}
	return stmt
}

@(test)
replace_with_into_empty_strings_test :: proc(t: ^testing.T) {
	src := `REPLACE '*' WITH '' INTO lv_ph_fpath.`
	stmt := expect_replace_stmt(t, src)
	if stmt == nil do return
	testing.expect(t, stmt.scope == .Simple, "scope")
	testing.expect(t, !stmt.is_regex, "regex")
	testing.expect(t, stmt.into_form, "into_form")
	if lit, ok := stmt.pattern.derived_expr.(^ast.Basic_Lit); ok {
		testing.expect(t, lit.tok.lit == "'*'", fmt.tprintf("pattern lit %s", lit.tok.lit))
	} else {
		testing.expect(t, false, "pattern literal")
	}
	if lit, ok := stmt.replacement.derived_expr.(^ast.Basic_Lit); ok {
		testing.expect(t, lit.tok.lit == "''", fmt.tprintf("replacement %s", lit.tok.lit))
	} else {
		testing.expect(t, false, "replacement literal")
	}
	if id, ok := stmt.subject.derived_expr.(^ast.Ident); ok {
		testing.expect(t, id.name == "lv_ph_fpath", "subject ident")
	} else {
		testing.expect(t, false, "subject ident")
	}
}

@(test)
replace_in_with_test :: proc(t: ^testing.T) {
	src := `REPLACE ',' IN ev_timestamp_iso WITH '.'.`
	stmt := expect_replace_stmt(t, src)
	if stmt == nil do return
	testing.expect(t, stmt.scope == .Simple, "scope")
	testing.expect(t, !stmt.into_form, "into_form")
}

@(test)
replace_first_occurrence_field_symbol_test :: proc(t: ^testing.T) {
	src := `REPLACE FIRST OCCURRENCE OF zattp_cl_rep_constants=>gv_url_locat_replace_from IN <fs_destination>-content WITH zattp_cl_rep_constants=>gv_url_locat_replace_to.`
	stmt := expect_replace_stmt(t, src)
	if stmt == nil do return
	testing.expect(t, stmt.scope == .First_Occurrence, "scope")
	testing.expect(t, !stmt.is_regex, "regex")
}

@(test)
replace_all_regex_multiline_test :: proc(t: ^testing.T) {
	src :=
		`REPLACE
 ALL OCCURRENCES OF REGEX '%2F|%2f'
 IN lv_path WITH '/'.`
	stmt := expect_replace_stmt(t, src)
	if stmt == nil do return
	testing.expect(t, stmt.scope == .All_Occurrences, "scope")
	testing.expect(t, stmt.is_regex, "regex")
}

@(test)
replace_first_regex_backtick_test :: proc(t: ^testing.T) {
	// Odin raw strings cannot contain `\` sequences; build ABAP source with a template literal
	src := "REPLACE FIRST OCCURRENCE OF REGEX `\\s([^\\s]*)$` IN lv_current_tag_path WITH '' ."
	stmt := expect_replace_stmt(t, src)
	if stmt == nil do return
	testing.expect(t, stmt.scope == .First_Occurrence, "scope")
	testing.expect(t, stmt.is_regex, "regex")
	if lit, ok := stmt.pattern.derived_expr.(^ast.Basic_Lit); ok {
		testing.expect(
			t,
			lit.tok.kind == .String,
			fmt.tprintf("pattern should be string token, got %v", lit.tok.kind),
		)
	} else {
		testing.expect(t, false, "pattern backtick literal")
	}
}

@(test)
replace_all_occurrences_selector_test :: proc(t: ^testing.T) {
	src := `REPLACE ALL OCCURRENCES OF ls_gs1_edel-gs1_element_delimiter IN lv_json WITH lv_esc_edel.`
	stmt := expect_replace_stmt(t, src)
	if stmt == nil do return
	testing.expect(t, stmt.scope == .All_Occurrences, "scope")
	testing.expect(t, !stmt.is_regex, "regex")
}
