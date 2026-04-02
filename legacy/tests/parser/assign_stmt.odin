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
	testing.expect(
		t,
		ast.selector_field_ident_name(source) == "*",
		fmt.tprintf("Expected '*', got '%s'", ast.selector_field_ident_name(source)),
	)

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
	testing.expect(
		t,
		ast.selector_field_ident_name(source) == "col2",
		fmt.tprintf("Expected 'col2', got '%s'", ast.selector_field_ident_name(source)),
	)
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

	testing.expect(
		t,
		ast.selector_field_ident_name(source) == "a",
		fmt.tprintf("Expected 'a', got '%s'", ast.selector_field_ident_name(source)),
	)

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
assign_stmt_static_class_dynamic_attribute_test :: proc(t: ^testing.T) {
	stmt := parse_single_assign_stmt(t, `ASSIGN if_http_status=>(lv_reason) TO <reason>.`)
	if stmt == nil do return

	source, ok := stmt.source.derived_expr.(^ast.Selector_Expr)
	if !testing.expect(t, ok, fmt.tprintf("Expected Selector_Expr, got %T", stmt.source.derived_expr)) do return

	base, bok := source.expr.derived_expr.(^ast.Ident)
	if testing.expect(t, bok, fmt.tprintf("Expected Ident base, got %T", source.expr.derived_expr)) {
		testing.expect(
			t,
			base.name == "if_http_status",
			fmt.tprintf("Expected 'if_http_status', got '%s'", base.name),
		)
	}
	testing.expect(t, source.op.kind == .FatArrow, "Expected =>")

	field_paren, pok := source.field.derived_expr.(^ast.Paren_Expr)
	if !testing.expect(t, pok, "Expected Paren_Expr for dynamic attribute name") do return
	inner, iok := field_paren.expr.derived_expr.(^ast.Ident)
	if testing.expect(t, iok, fmt.tprintf("Expected Ident in parens, got %T", field_paren.expr.derived_expr)) {
		testing.expect(t, inner.name == "lv_reason", fmt.tprintf("Expected lv_reason, got %s", inner.name))
	}
}

@(test)
assign_stmt_dynamic_class_and_attribute_test :: proc(t: ^testing.T) {
	stmt := parse_single_assign_stmt(t, `ASSIGN (lv_class_name)=>(lv_reason) TO <reason>.`)
	if stmt == nil do return

	testing.expect(t, stmt.is_dynamic, "Expected dynamic ASSIGN (leading paren)")

	source, ok := stmt.source.derived_expr.(^ast.Selector_Expr)
	if !testing.expect(t, ok, fmt.tprintf("Expected Selector_Expr, got %T", stmt.source.derived_expr)) do return

	left_paren, lok := source.expr.derived_expr.(^ast.Paren_Expr)
	if !testing.expect(t, lok, "Expected Paren_Expr for dynamic class") do return
	class_var, cok := left_paren.expr.derived_expr.(^ast.Ident)
	if testing.expect(t, cok, fmt.tprintf("Expected Ident, got %T", left_paren.expr.derived_expr)) {
		testing.expect(t, class_var.name == "lv_class_name", fmt.tprintf("class var name, got %s", class_var.name))
	}

	field_paren, pok := source.field.derived_expr.(^ast.Paren_Expr)
	if !testing.expect(t, pok, "Expected Paren_Expr for dynamic attribute") do return
	attr_var, aok := field_paren.expr.derived_expr.(^ast.Ident)
	if testing.expect(t, aok, fmt.tprintf("Expected Ident, got %T", field_paren.expr.derived_expr)) {
		testing.expect(t, attr_var.name == "lv_reason", fmt.tprintf("attr var, got %s", attr_var.name))
	}
}

@(test)
call_method_static_class_dynamic_method_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CALL METHOD zcl_foo=>(lv_meth).`
	p: parser.Parser
	parser.parse_file(&p, file)

	if !testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("errors: %v", file.syntax_errors)) do return
	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("decls %d", len(file.decls))) do return

	expr_stmt, ok := file.decls[0].derived_stmt.(^ast.Expr_Stmt)
	if !testing.expect(t, ok, "Expr_Stmt") do return

	call, cok := expr_stmt.expr.derived_expr.(^ast.Call_Expr)
	if !testing.expect(t, cok, "Call_Expr") do return

	sel, sok := call.expr.derived_expr.(^ast.Selector_Expr)
	if !testing.expect(t, sok, "Selector callee") do return
	testing.expect(t, sel.op.kind == .FatArrow, "=>")

	base, bok := sel.expr.derived_expr.(^ast.Ident)
	if testing.expect(t, bok, "class ident") {
		testing.expect(t, base.name == "zcl_foo", fmt.tprintf("class %s", base.name))
	}

	fp, pok := sel.field.derived_expr.(^ast.Paren_Expr)
	if !testing.expect(t, pok, "dynamic method name") do return
	meth_var, mok := fp.expr.derived_expr.(^ast.Ident)
	if testing.expect(t, mok, "meth ident") {
		testing.expect(t, meth_var.name == "lv_meth", fmt.tprintf("meth %s", meth_var.name))
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

@(test)
assign_component_of_structure_with_inline_field_symbol_and_pragma_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `ASSIGN COMPONENT 'EPCISDOCUMENT-EPCISBODY-EVENT_LIST-CHOICE' OF STRUCTURE <ls_outbound> TO FIELD-SYMBOL(<ls_event>) ##no_text.`

	p: parser.Parser
	parser.parse_file(&p, file)

	if !testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors)) do return

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	stmt, ok := file.decls[0].derived_stmt.(^ast.Assign_Field_Symbol_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Assign_Field_Symbol_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(t, stmt.is_component, "Expected COMPONENT ASSIGN")
	testing.expect(t, !stmt.is_dynamic, "Did not expect dynamic ASSIGN")
	testing.expect(t, !stmt.is_table_field, "Did not expect TABLE FIELD ASSIGN")

	if !testing.expect(t, stmt.component != nil, "Expected component expression") do return
	component, cok := stmt.component.derived_expr.(^ast.Basic_Lit)
	if testing.expect(t, cok, fmt.tprintf("Expected Basic_Lit component, got %T", stmt.component.derived_expr)) {
		testing.expect(
			t,
			component.tok.lit == "'EPCISDOCUMENT-EPCISBODY-EVENT_LIST-CHOICE'",
			fmt.tprintf("Expected component literal, got %s", component.tok.lit),
		)
	}

	if !testing.expect(t, stmt.structure != nil, "Expected structure expression") do return
	structure, sok := stmt.structure.derived_expr.(^ast.Ident)
	if testing.expect(t, sok, fmt.tprintf("Expected Ident structure, got %T", stmt.structure.derived_expr)) {
		testing.expect(
			t,
			structure.name == "<ls_outbound>",
			fmt.tprintf("Expected '<ls_outbound>', got '%s'", structure.name),
		)
	}

	if !testing.expect(t, stmt.target != nil, "Expected target expression") do return
	target, tok := stmt.target.derived_expr.(^ast.Ident)
	if testing.expect(t, tok, fmt.tprintf("Expected Ident target, got %T", stmt.target.derived_expr)) {
		testing.expect(t, target.name == "<ls_event>", fmt.tprintf("Expected '<ls_event>', got '%s'", target.name))
	}

	testing.expect(
		t,
		len(file.comments) >= 1,
		fmt.tprintf("Expected pragma to be collected as comment, got %d comments", len(file.comments)),
	)
}

@(test)
move_stmt_with_substring_offsets_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `MOVE '01' TO lv_date+6(2).
MOVE lv_date(4) TO lv_year.
MOVE lv_date+4(2) TO lv_month.`

	p: parser.Parser
	parser.parse_file(&p, file)

	if !testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors)) do return
	if !testing.expect(t, len(file.decls) == 3, fmt.tprintf("Expected 3 stmts, got %d", len(file.decls))) do return

	for i in 0 ..< 3 {
		assign, ok := file.decls[i].derived_stmt.(^ast.Assign_Stmt)
		if !testing.expect(t, ok, fmt.tprintf("stmt %d: expected Assign_Stmt", i)) do return
		if !testing.expect(t, assign.op.kind == .Ident, fmt.tprintf("stmt %d: expected MOVE as ident op", i)) do return
	}

	// MOVE '01' TO lv_date+6(2).
	a0 := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
	rhs0, r0ok := a0.rhs[0].derived_expr.(^ast.Basic_Lit)
	if testing.expect(t, r0ok, fmt.tprintf("rhs0: expected literal, got %T", a0.rhs[0].derived_expr)) {
		testing.expect(t, rhs0.tok.lit == "'01'", fmt.tprintf("literal %s", rhs0.tok.lit))
	}
	t0, t0ok := a0.lhs[0].derived_expr.(^ast.Substring_Expr)
	if !testing.expect(t, t0ok, fmt.tprintf("target0: expected Substring_Expr, got %T", a0.lhs[0].derived_expr)) do return
	t0base, t0b := t0.expr.derived_expr.(^ast.Ident)
	if testing.expect(t, t0b, fmt.tprintf("target0 base, got %T", t0.expr.derived_expr)) {
		testing.expect(t, t0base.name == "lv_date", fmt.tprintf("base %s", t0base.name))
	}
	t0off, t0o := t0.offset.derived_expr.(^ast.Basic_Lit)
	if testing.expect(t, t0o, fmt.tprintf("target0 offset, got %T", t0.offset.derived_expr)) {
		testing.expect(t, t0off.tok.lit == "6", fmt.tprintf("off %s", t0off.tok.lit))
	}
	t0len, t0l := t0.length.derived_expr.(^ast.Basic_Lit)
	if testing.expect(t, t0l, fmt.tprintf("target0 length, got %T", t0.length.derived_expr)) {
		testing.expect(t, t0len.tok.lit == "2", fmt.tprintf("len %s", t0len.tok.lit))
	}

	// MOVE lv_date(4) TO lv_year.
	a1 := file.decls[1].derived_stmt.(^ast.Assign_Stmt)
	s1, s1ok := a1.rhs[0].derived_expr.(^ast.Substring_Expr)
	if !testing.expect(t, s1ok, fmt.tprintf("rhs1: expected Substring_Expr, got %T", a1.rhs[0].derived_expr)) do return
	base1, b1ok := s1.expr.derived_expr.(^ast.Ident)
	if testing.expect(t, b1ok, fmt.tprintf("substring base, got %T", s1.expr.derived_expr)) {
		testing.expect(t, base1.name == "lv_date", fmt.tprintf("base %s", base1.name))
	}
	if !testing.expect(t, s1.offset == nil, "lv_date(4) should have no offset") do return
	len1, l1ok := s1.length.derived_expr.(^ast.Basic_Lit)
	if testing.expect(t, l1ok, fmt.tprintf("length, got %T", s1.length.derived_expr)) {
		testing.expect(t, len1.tok.lit == "4", fmt.tprintf("len %s", len1.tok.lit))
	}
	yy, yyok := a1.lhs[0].derived_expr.(^ast.Ident)
	if testing.expect(t, yyok, fmt.tprintf("target lv_year, got %T", a1.lhs[0].derived_expr)) {
		testing.expect(t, yy.name == "lv_year", fmt.tprintf("name %s", yy.name))
	}

	// MOVE lv_date+4(2) TO lv_month.
	a2 := file.decls[2].derived_stmt.(^ast.Assign_Stmt)
	s2, s2ok := a2.rhs[0].derived_expr.(^ast.Substring_Expr)
	if !testing.expect(t, s2ok, fmt.tprintf("rhs2: expected Substring_Expr, got %T", a2.rhs[0].derived_expr)) do return
	off2, o2ok := s2.offset.derived_expr.(^ast.Basic_Lit)
	if testing.expect(t, o2ok, fmt.tprintf("offset, got %T", s2.offset.derived_expr)) {
		testing.expect(t, off2.tok.lit == "4", fmt.tprintf("off %s", off2.tok.lit))
	}
	len2, l2ok := s2.length.derived_expr.(^ast.Basic_Lit)
	if testing.expect(t, l2ok, fmt.tprintf("length2, got %T", s2.length.derived_expr)) {
		testing.expect(t, len2.tok.lit == "2", fmt.tprintf("len %s", len2.tok.lit))
	}
}

@(test)
move_chained_colon_stmt_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `MOVE: is_syst-sysname TO <ls_rng_sysname>-low,
          'I'             TO <ls_rng_sysname>-sign,
          'EQ'            TO <ls_rng_sysname>-option.`

	p: parser.Parser
	parser.parse_file(&p, file)

	if !testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors)) do return
	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 stmt, got %d", len(file.decls))) do return

	assign, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("expected Assign_Stmt, got %T", file.decls[0].derived_stmt)) do return
	if !testing.expect(t, len(assign.lhs) == 3 && len(assign.rhs) == 3, "expected 3 lhs/rhs pairs") do return

	lit_expect :: proc(t: ^testing.T, e: ^ast.Expr, want_lit: string) -> bool {
		l, lok := e.derived_expr.(^ast.Basic_Lit)
		if !testing.expect(t, lok, fmt.tprintf("expected literal, got %T", e.derived_expr)) do return false
		return testing.expect(t, l.tok.lit == want_lit, fmt.tprintf("want %s, got %s", want_lit, l.tok.lit))
	}
	if !lit_expect(t, assign.rhs[1], "'I'") do return
	if !lit_expect(t, assign.rhs[2], "'EQ'") do return

	fs_expect :: proc(t: ^testing.T, e: ^ast.Expr, want: string) -> bool {
		id, iok := e.derived_expr.(^ast.Ident)
		if !testing.expect(t, iok, fmt.tprintf("expected Ident, got %T", e.derived_expr)) do return false
		return testing.expect(t, id.name == want, fmt.tprintf("want %s, got %s", want, id.name))
	}
	sel1, s1ok := assign.lhs[1].derived_expr.(^ast.Selector_Expr)
	if !testing.expect(t, s1ok, fmt.tprintf("lhs[1]: want Selector_Expr, got %T", assign.lhs[1].derived_expr)) do return
	if !fs_expect(t, sel1.expr, "<ls_rng_sysname>") do return
	sel2, s2ok := assign.lhs[2].derived_expr.(^ast.Selector_Expr)
	if !testing.expect(t, s2ok, fmt.tprintf("lhs[2]: want Selector_Expr, got %T", assign.lhs[2].derived_expr)) do return
	if !fs_expect(t, sel2.expr, "<ls_rng_sysname>") do return
}
