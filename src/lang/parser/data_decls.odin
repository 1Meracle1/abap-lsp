package lang_parser

import "../ast"
import "../lexer"

parse_data_decl :: proc(p: ^Parser) -> ^ast.Decl {
	data_tok := expect_token(p, .Ident)
	if p.curr_tok.kind == .LParen {
		return parse_data_inline_decl(p, data_tok)
	}
	return parse_data_typed_decl(p, data_tok)
}

parse_data_typed_decl :: proc(p: ^Parser, data_tok: lexer.Token) -> ^ast.Decl {
	if allow_token(p, .Colon) {
		return parse_data_typed_multiple_decl(p, data_tok)
	}
	decl := parse_data_typed_single_decl(p, data_tok)
	if decl != nil {
		period_tok := expect_token(p, .Period)
		decl.range.end = period_tok.range.end
	}
	return decl
}

// parse_data_decl_ident parses an identifier for a DATA declaration,
// which may be a simple identifier or a selector expression (e.g., screen0100-serial)
parse_data_decl_ident :: proc(p: ^Parser) -> ^ast.Expr {
	ident_tok := expect_token(p, .Ident)
	expr: ^ast.Expr = &ast.new_ident(ident_tok).node

	// Check for selector expression (using - as separator)
	for p.curr_tok.kind == .Minus {
		minus_tok := advance_token(p)
		field_tok := expect_token(p, .Ident)
		field_ident := ast.new_ident(field_tok)

		selector := ast.new(
			ast.Selector_Expr,
			lexer.TextRange{expr.range.start, field_tok.range.end},
		)
		selector.expr = expr
		selector.op = minus_tok
		selector.field = field_ident
		selector.derived_expr = selector
		expr = &selector.node
	}

	return expr
}

parse_data_typed_single_decl :: proc(p: ^Parser, data_tok: lexer.Token) -> ^ast.Decl {
	// Parse identifier, which may be a selector expression (e.g., screen0100-serial)
	ident_expr := parse_data_decl_ident(p)

	// Accept TYPE or LIKE
	if check_keyword(p, "TYPE") || check_keyword(p, "LIKE") {
		advance_token(p)
	} else {
		expect_keyword_token(p, "TYPE")
	}

	type_expr := parse_type_expr(p)

	// Parse optional LENGTH
	if check_keyword(p, "LENGTH") {
		advance_token(p)
		// Skip LENGTH expression for now, but we could store it
		parse_expr(p)
	}

	value_expr: ^ast.Expr = nil
	if check_keyword(p, "VALUE") {
		advance_token(p)
		value_expr = parse_expr(p)
	}

	data_decl := ast.new(ast.Data_Typed_Decl, data_tok, p.curr_tok)
	data_decl.ident = ident_expr
	data_decl.typed = type_expr
	data_decl.value = value_expr
	return data_decl
}

parse_data_typed_multiple_decl :: proc(p: ^Parser, data_tok: lexer.Token) -> ^ast.Decl {
	chain_decl := ast.new(ast.Data_Typed_Chain_Decl, data_tok.range)
	chain_decl.decls = make([dynamic]^ast.Data_Typed_Decl)

	for {
		// Check for BEGIN OF structure declaration
		if check_keyword(p, "BEGIN") {
			struct_decl := parse_data_struct_decl(p)
			if struct_decl != nil {
				if len(chain_decl.decls) == 0 {
					if allow_token(p, .Comma) {
					}
					if allow_token(p, .Period) {
						struct_decl.range.end = p.prev_tok.range.end
					}
					return struct_decl
				}
			}
			break
		}

		// Parse identifier, which may be a selector expression (e.g., screen0100-serial)
		ident_expr := parse_data_decl_ident(p)

		// Accept TYPE or LIKE
		if check_keyword(p, "TYPE") || check_keyword(p, "LIKE") {
			advance_token(p)
		} else {
			expect_keyword_token(p, "TYPE")
		}

		type_expr := parse_type_expr(p)

		// Parse optional LENGTH
		if check_keyword(p, "LENGTH") {
			advance_token(p)
			parse_expr(p)
		}

		value_expr: ^ast.Expr = nil
		if check_keyword(p, "VALUE") {
			advance_token(p)
			value_expr = parse_expr(p)
		}

		decl := ast.new(
			ast.Data_Typed_Decl,
			lexer.TextRange{ident_expr.range.start, p.prev_tok.range.end},
		)
		decl.ident = ident_expr
		decl.typed = type_expr
		decl.value = value_expr
		append(&chain_decl.decls, decl)

		if allow_token(p, .Comma) {
			// Continue parsing next declaration in chain
			continue
		}

		// Expect period to end the chain
		period_tok := expect_token(p, .Period)
		chain_decl.range.end = period_tok.range.end
		break
	}

	return chain_decl
}

// parse_data_struct_decl parses DATA structure declarations:
// DATA: BEGIN OF name,
//         field1 TYPE/LIKE type1 [VALUE val1],
//         field2 TYPE/LIKE type2 [VALUE val2],
//       END OF name.
parse_data_struct_decl :: proc(p: ^Parser) -> ^ast.Data_Struct_Decl {
	begin_tok := expect_keyword_token(p, "BEGIN")
	expect_keyword_token(p, "OF")
	ident_tok := expect_token(p, .Ident)

	struct_decl := ast.new(ast.Data_Struct_Decl, begin_tok.range)
	struct_decl.ident = ast.new_ident(ident_tok)
	struct_decl.components = make([dynamic]^ast.Stmt)

	expect_token(p, .Comma)

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "END") {
			break
		}

		// Check for nested BEGIN OF
		if check_keyword(p, "BEGIN") {
			nested_struct := parse_data_struct_decl(p)
			if nested_struct != nil {
				append(&struct_decl.components, &nested_struct.node)
			}
			if !allow_token(p, .Comma) {
				break
			}
			continue
		}

		// Parse identifier, which may be a selector expression (e.g., screen0100-serial)
		field_ident_expr := parse_data_decl_ident(p)

		// Accept TYPE or LIKE
		if check_keyword(p, "TYPE") || check_keyword(p, "LIKE") {
			advance_token(p)
		} else {
			expect_keyword_token(p, "TYPE")
		}

		type_expr := parse_type_expr(p)

		// Parse optional LENGTH
		if check_keyword(p, "LENGTH") {
			advance_token(p)
			parse_expr(p)
		}

		// Parse optional VALUE
		value_expr: ^ast.Expr = nil
		if check_keyword(p, "VALUE") {
			advance_token(p)
			value_expr = parse_expr(p)
		}

		field_decl := ast.new(
			ast.Data_Typed_Decl,
			lexer.TextRange{field_ident_expr.range.start, p.prev_tok.range.end},
		)
		field_decl.ident = field_ident_expr
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

	if struct_decl.ident.name != end_ident_tok.lit {
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

parse_data_inline_decl :: proc(p: ^Parser, data_tok: lexer.Token) -> ^ast.Decl {
	expect_token_space_req(p, .LParen, .WithoutLeadingSpace)
	ident_tok := expect_token_space_req(p, .Ident, .WithoutLeadingSpace)
	expect_token_space_req(p, .RParen, .WithoutLeadingSpace)
	expect_token_space_req(p, .Eq, .WithLeadingTrailingSpace)
	expr := parse_expr(p)
	period_tok := expect_token(p, .Period)

	data_decl := ast.new(ast.Data_Inline_Decl, data_tok, period_tok)
	data_decl.ident = ast.new_ident(ident_tok)
	data_decl.value = expr
	return data_decl
}