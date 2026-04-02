package lang_parser

import "../ast"
import "../lexer"
import "core:strings"

// CONSTANTS declarations parsing

// parse_optional_const_length_in_parens parses legacy length in parentheses after the name
// (e.g. CONSTANTS lcv_max_microsecs(14) TYPE p ...), before TYPE / LIKE.
parse_optional_const_length_in_parens :: proc(p: ^Parser) -> ^ast.Expr {
	if !allow_token(p, .LParen) {
		return nil
	}
	e := parse_expr(p)
	expect_token(p, .RParen)
	return e
}

parse_constants_decl :: proc(p: ^Parser) -> ^ast.Decl {
	const_tok := expect_token(p, .Ident)
	if allow_token(p, .Colon) {
		return parse_constants_chain_decl(p, const_tok)
	}
	return parse_constants_single_decl(p, const_tok)
}

parse_constants_single_decl :: proc(p: ^Parser, const_tok: lexer.Token) -> ^ast.Decl {
	ident_tok := expect_token(p, .Ident)
	length_expr := parse_optional_const_length_in_parens(p)

	// Accept TYPE or LIKE
	if check_keyword(p, "TYPE") || check_keyword(p, "LIKE") {
		advance_token(p)
	} else {
		expect_keyword_token(p, "TYPE")
	}

	type_expr := parse_type_expr(p)
	parse_optional_length_decimals(p, &length_expr)

	// CONSTANTS must have VALUE
	value_expr: ^ast.Expr = nil
	if check_keyword(p, "VALUE") {
		advance_token(p)
		value_expr = parse_expr(p)
	}

	period_tok := expect_token(p, .Period)

	const_decl := ast.new(ast.Const_Decl, const_tok, period_tok)
	const_decl.ident = ast.new_ident(ident_tok)
	const_decl.length = length_expr
	const_decl.typed = type_expr
	const_decl.value = value_expr
	const_decl.derived_stmt = const_decl
	return const_decl
}

// finish_const_chain_or_single_struct ends a CONSTANTS: chain segment after a BEGIN…END OF block:
// comma continues the chain; otherwise '.' closes it. A sole structured constant is unwrapped
// to ^Const_Struct_Decl (same idea as DATA: BEGIN OF…).
finish_const_chain_or_single_struct :: proc(
	p: ^Parser,
	chain_decl: ^ast.Const_Chain_Decl,
) -> ^ast.Decl {
	if allow_token(p, .Comma) {
		return nil
	}
	period_tok := expect_token(p, .Period)
	chain_decl.range.end = period_tok.range.end
	if len(chain_decl.parts) == 1 {
		if only_struct, ok := chain_decl.parts[0].derived_stmt.(^ast.Const_Struct_Decl); ok {
			only_struct.range.end = period_tok.range.end
			return only_struct
		}
	}
	return chain_decl
}

parse_constants_chain_decl :: proc(p: ^Parser, const_tok: lexer.Token) -> ^ast.Decl {
	chain_decl := ast.new(ast.Const_Chain_Decl, const_tok.range)
	chain_decl.parts = make([dynamic]^ast.Stmt)

	for {
		if check_keyword(p, "BEGIN") {
			struct_decl := parse_constants_struct_decl(p)
			if struct_decl != nil {
				append(&chain_decl.parts, &struct_decl.node)
			}
			if done := finish_const_chain_or_single_struct(p, chain_decl); done != nil {
				return done
			}
			continue
		}

		ident_tok := expect_token(p, .Ident)
		length_expr := parse_optional_const_length_in_parens(p)

		// Accept TYPE or LIKE
		if check_keyword(p, "TYPE") || check_keyword(p, "LIKE") {
			advance_token(p)
		} else {
			expect_keyword_token(p, "TYPE")
		}

		type_expr := parse_type_expr(p)
		parse_optional_length_decimals(p, &length_expr)

		value_expr: ^ast.Expr = nil
		if check_keyword(p, "VALUE") {
			advance_token(p)
			value_expr = parse_expr(p)
		}

		decl := ast.new(ast.Const_Decl, ident_tok, p.prev_tok)
		decl.ident = ast.new_ident(ident_tok)
		decl.length = length_expr
		decl.typed = type_expr
		decl.value = value_expr
		decl.derived_stmt = decl
		append(&chain_decl.parts, &decl.node)

		if allow_token(p, .Comma) {
			continue
		}

		period_tok := expect_token(p, .Period)
		chain_decl.range.end = period_tok.range.end
		break
	}

	chain_decl.derived_stmt = chain_decl
	if len(chain_decl.parts) == 1 {
		if only_struct, ok := chain_decl.parts[0].derived_stmt.(^ast.Const_Struct_Decl); ok {
			only_struct.range.end = chain_decl.range.end
			return only_struct
		}
	}
	return chain_decl
}

parse_constants_struct_decl :: proc(p: ^Parser) -> ^ast.Const_Struct_Decl {
	begin_tok := expect_keyword_token(p, "BEGIN")
	expect_keyword_token(p, "OF")
	ident_tok := expect_token(p, .Ident)

	struct_decl := ast.new(ast.Const_Struct_Decl, begin_tok.range)
	struct_decl.ident = ast.new_ident(ident_tok)
	struct_decl.components = make([dynamic]^ast.Stmt)

	expect_token(p, .Comma)

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "END") {
			break
		}

		if check_keyword(p, "BEGIN") {
			nested_struct := parse_constants_struct_decl(p)
			if nested_struct != nil {
				append(&struct_decl.components, &nested_struct.node)
			}
			if !allow_token(p, .Comma) {
				break
			}
			continue
		}

		field_ident_tok := expect_token(p, .Ident)
		field_length_expr := parse_optional_const_length_in_parens(p)

		// Accept TYPE or LIKE
		if check_keyword(p, "TYPE") || check_keyword(p, "LIKE") {
			advance_token(p)
		} else {
			expect_keyword_token(p, "TYPE")
		}

		type_expr := parse_type_expr(p)
		parse_optional_length_decimals(p, &field_length_expr)

		value_expr: ^ast.Expr = nil
		if check_keyword(p, "VALUE") {
			advance_token(p)
			value_expr = parse_expr(p)
		}

		field_decl := ast.new(ast.Const_Decl, field_ident_tok, p.prev_tok)
		field_decl.ident = ast.new_ident(field_ident_tok)
		field_decl.length = field_length_expr
		field_decl.typed = type_expr
		field_decl.value = value_expr
		field_decl.derived_stmt = field_decl
		append(&struct_decl.components, &field_decl.node)

		if !allow_token(p, .Comma) {
			break
		}
	}

	expect_keyword_token(p, "END")
	expect_keyword_token(p, "OF")
	end_ident_tok := expect_token(p, .Ident)
	struct_decl.range.end = end_ident_tok.range.end

	begin_name := strings.to_upper(struct_decl.ident.name, context.temp_allocator)
	end_name := strings.to_upper(end_ident_tok.lit, context.temp_allocator)
	if begin_name != end_name {
		error(
			p,
			end_ident_tok.range,
			"END OF '%s' does not match BEGIN OF '%s'",
			end_ident_tok.lit,
			struct_decl.ident.name,
		)
	}

	struct_decl.derived_stmt = struct_decl
	return struct_decl
}

parse_form_decl :: proc(p: ^Parser) -> ^ast.Decl {
	form_tok := expect_token(p, .Ident)
	ident_tok := expect_token(p, .Ident)

	form_decl := ast.new(ast.Form_Decl, form_tok.range)
	form_decl.ident = ast.new_ident(ident_tok)
	form_decl.tables_params = make([dynamic]^ast.Form_Param)
	form_decl.using_params = make([dynamic]^ast.Form_Param)
	form_decl.changing_params = make([dynamic]^ast.Form_Param)
	form_decl.body = make([dynamic]^ast.Stmt)

	optional_param_section_loop: for p.curr_tok.kind == .Ident && p.curr_tok.kind != .Period {
		if len(p.curr_tok.lit) > 0 && len(p.curr_tok.lit) < len(p.keyword_buffer) {
			keyword := to_upper(p.keyword_buffer[:], p.curr_tok.lit)
			switch keyword {
			case "TABLES":
				advance_token(p)
				parse_form_params(p, &form_decl.tables_params, .Tables)
			case "USING":
				advance_token(p)
				parse_form_params(p, &form_decl.using_params, .Using)
			case "CHANGING":
				advance_token(p)
				parse_form_params(p, &form_decl.changing_params, .Changing)
			case:
				break optional_param_section_loop
			}
		} else {
			break
		}
	}

	expect_token(p, .Period)

	for p.curr_tok.kind != .EOF {
		if p.curr_tok.kind == .Ident {
			if len(p.curr_tok.lit) > 0 && len(p.curr_tok.lit) < len(p.keyword_buffer) {
				keyword := to_upper(p.keyword_buffer[:], p.curr_tok.lit)
				if keyword == "ENDFORM" {
					break
				}
			}
		}
		stmt := parse_stmt(p)
		if stmt != nil {
			append(&form_decl.body, stmt)
		}
	}

	endform_tok := expect_keyword_token(p, "ENDFORM")
	period_tok := expect_token(p, .Period)
	form_decl.range.end = period_tok.range.end
	_ = endform_tok

	return form_decl
}

parse_form_params :: proc(
	p: ^Parser,
	params: ^[dynamic]^ast.Form_Param,
	kind: ast.Form_Param_Kind,
) {
	for p.curr_tok.kind == .Ident {
		if len(p.curr_tok.lit) > 0 && len(p.curr_tok.lit) < len(p.keyword_buffer) {
			keyword := to_upper(p.keyword_buffer[:], p.curr_tok.lit)
			if keyword == "TABLES" || keyword == "USING" || keyword == "CHANGING" {
				break
			}
		}

		param := ast.new(ast.Form_Param, p.curr_tok.range)
		param.kind = kind
		ident_tok := advance_token(p)
		param.ident = ast.new_ident(ident_tok)

		if p.curr_tok.kind == .Ident {
			if len(p.curr_tok.lit) > 0 && len(p.curr_tok.lit) < len(p.keyword_buffer) {
				keyword := to_upper(p.keyword_buffer[:], p.curr_tok.lit)
				if keyword == "TYPE" {
					advance_token(p)
					param.typed = parse_type_expr(p)
					param.range.end = p.prev_tok.range.end
				} else if keyword == "LIKE" {
					advance_token(p)
					param.is_like = true
					param.typed = parse_type_expr(p)
					param.range.end = p.prev_tok.range.end
				}
			}
		}

		append(params, param)

		if p.curr_tok.kind == .Period {
			break
		}
	}
}

// FIELD-SYMBOLS declaration parser
// Syntax: FIELD-SYMBOLS <fs> TYPE type.
// Syntax: FIELD-SYMBOLS: <fs1> TYPE type1, <fs2> TYPE type2.
// Syntax: FIELD-SYMBOLS <fs> LIKE LINE OF itab.
parse_field_symbol_decl :: proc(p: ^Parser) -> ^ast.Stmt {
	// FIELD-SYMBOLS was already consumed by check_hyphenated_keyword
	fs_tok := p.prev_tok

	if allow_token(p, .Colon) {
		return parse_field_symbol_chain_decl(p, fs_tok)
	}

	decl := parse_field_symbol_single_decl(p, fs_tok)
	if decl != nil {
		period_tok := expect_token(p, .Period)
		decl.range.end = period_tok.range.end
	}
	return decl
}

parse_field_symbol_chain_decl :: proc(p: ^Parser, fs_tok: lexer.Token) -> ^ast.Stmt {
	chain_decl := ast.new(ast.Field_Symbol_Chain_Decl, fs_tok.range)
	chain_decl.decls = make([dynamic]^ast.Field_Symbol_Decl)

	for {
		decl := parse_field_symbol_single_decl(p, fs_tok)
		if decl != nil {
			append(&chain_decl.decls, decl)
		}

		if p.curr_tok.kind == .Period {
			break
		}

		if !allow_token(p, .Comma) {
			error(p, p.curr_tok.range, "expected ',' or '.'")
			break
		}
	}

	period_tok := expect_token(p, .Period)
	chain_decl.range.end = period_tok.range.end
	return chain_decl
}

parse_field_symbol_single_decl :: proc(p: ^Parser, fs_tok: lexer.Token) -> ^ast.Field_Symbol_Decl {
	fs_decl := ast.new(ast.Field_Symbol_Decl, fs_tok.range)

	// Parse field symbol name <fs>
	fs_ident := parse_field_symbol_ref(p)
	if fs_ident == nil {
		return nil
	}
	if ident, ok := fs_ident.derived_expr.(^ast.Ident); ok {
		fs_decl.ident = ident
	} else {
		error(p, fs_ident.range, "expected field symbol identifier")
	}

	// Parse TYPE or LIKE clause
	if check_keyword(p, "TYPE") {
		advance_token(p)
		fs_decl.typed = parse_type_expr(p)
	} else if check_keyword(p, "LIKE") {
		advance_token(p)
		fs_decl.is_like = true
		fs_decl.typed = parse_type_expr(p)
	} else {
		error(p, p.curr_tok.range, "expected TYPE or LIKE after field symbol name")
	}
	return fs_decl
}

// CONTROLS declaration parser
// Syntax: CONTROLS contrl TYPE TABLEVIEW USING SCREEN dynnr.
// Syntax: CONTROLS contrl TYPE TABSTRIP.
// Syntax: CONTROLS: name1 TYPE TABSTRIP, name2 TYPE TABLEVIEW USING SCREEN 100.
parse_controls_decl :: proc(p: ^Parser) -> ^ast.Stmt {
	controls_tok := expect_keyword_token(p, "CONTROLS")

	// Check for chained declaration with colon
	if allow_token(p, .Colon) {
		return parse_controls_chain_decl(p, controls_tok)
	}

	// Single declaration
	decl := parse_controls_single_decl(p, controls_tok)
	if decl != nil {
		period_tok := expect_token(p, .Period)
		decl.range.end = period_tok.range.end
	}
	return decl
}

// parse_controls_chain_decl parses a chained CONTROLS declaration
// Syntax: CONTROLS: name1 TYPE TABSTRIP, name2 TYPE TABLEVIEW USING SCREEN 100.
parse_controls_chain_decl :: proc(p: ^Parser, controls_tok: lexer.Token) -> ^ast.Stmt {
	chain_decl := ast.new(ast.Controls_Chain_Decl, controls_tok.range)
	chain_decl.decls = make([dynamic]^ast.Controls_Decl)

	for {
		decl := parse_controls_single_decl(p, controls_tok)
		if decl != nil {
			append(&chain_decl.decls, decl)
		}

		if p.curr_tok.kind == .Period {
			break
		}

		if !allow_token(p, .Comma) {
			error(p, p.curr_tok.range, "expected ',' or '.'")
			break
		}
	}

	period_tok := expect_token(p, .Period)
	chain_decl.range.end = period_tok.range.end
	chain_decl.derived_stmt = chain_decl
	return chain_decl
}

// parse_controls_single_decl parses a single CONTROLS declaration
// Syntax: contrl TYPE TABLEVIEW USING SCREEN dynnr
// Syntax: contrl TYPE TABSTRIP
parse_controls_single_decl :: proc(p: ^Parser, controls_tok: lexer.Token) -> ^ast.Controls_Decl {
	// Parse control name
	ident_tok := expect_token(p, .Ident)

	// Expect TYPE keyword
	expect_keyword_token(p, "TYPE")

	// Parse control type (TABLEVIEW or TABSTRIP)
	type_tok := expect_token(p, .Ident)
	type_upper := to_upper(p.keyword_buffer[:], type_tok.lit)

	controls_decl := ast.new(ast.Controls_Decl, controls_tok, p.curr_tok)
	controls_decl.ident = ast.new_ident(ident_tok)

	if type_upper == "TABLEVIEW" {
		controls_decl.kind = .Tableview
		// Parse USING SCREEN dynnr
		expect_keyword_token(p, "USING")
		expect_keyword_token(p, "SCREEN")
		controls_decl.screen_dynnr = parse_expr(p)
	} else if type_upper == "TABSTRIP" {
		controls_decl.kind = .Tabstrip
		controls_decl.screen_dynnr = nil
	} else {
		error(p, type_tok.range, "expected TABLEVIEW or TABSTRIP")
		controls_decl.kind = .Tabstrip
	}

	controls_decl.derived_stmt = controls_decl
	return controls_decl
}