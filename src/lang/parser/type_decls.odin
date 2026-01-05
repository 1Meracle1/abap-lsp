package lang_parser

import "../ast"
import "../lexer"

parse_types_decl :: proc(p: ^Parser) -> ^ast.Decl {
	types_tok := expect_token(p, .Ident)
	if allow_token(p, .Colon) {
		return parse_types_chain_decl(p, types_tok)
	}
	return parse_types_single_decl(p, types_tok)
}

parse_types_single_decl :: proc(p: ^Parser, types_tok: lexer.Token) -> ^ast.Decl {
	ident_tok := expect_token(p, .Ident)
	expect_keyword_token(p, "TYPE")
	type_expr := parse_type_expr(p)

	length_expr: ^ast.Expr = nil
	if check_keyword(p, "LENGTH") {
		advance_token(p)
		length_expr = parse_expr(p)
	}

	period_tok := expect_token(p, .Period)

	types_decl := ast.new(ast.Types_Decl, types_tok, period_tok)
	types_decl.ident = ast.new_ident(ident_tok)
	types_decl.typed = type_expr
	types_decl.length = length_expr
	return types_decl
}

parse_types_chain_decl :: proc(p: ^Parser, types_tok: lexer.Token) -> ^ast.Decl {
	chain_decl := ast.new(ast.Types_Chain_Decl, types_tok.range)
	chain_decl.decls = make([dynamic]^ast.Types_Decl)

	for {
		if check_keyword(p, "BEGIN") {
			struct_decl := parse_types_struct_decl(p)
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

		ident_tok := expect_token(p, .Ident)
		expect_keyword_token(p, "TYPE")
		type_expr := parse_type_expr(p)

		length_expr: ^ast.Expr = nil
		if check_keyword(p, "LENGTH") {
			advance_token(p)
			length_expr = parse_expr(p)
		}

		decl := ast.new(ast.Types_Decl, ident_tok, p.prev_tok)
		decl.ident = ast.new_ident(ident_tok)
		decl.typed = type_expr
		decl.length = length_expr
		append(&chain_decl.decls, decl)

		if allow_token(p, .Comma) {
			continue
		}

		period_tok := expect_token(p, .Period)
		chain_decl.range.end = period_tok.range.end
		break
	}

	return chain_decl
}

parse_types_struct_decl :: proc(p: ^Parser) -> ^ast.Types_Struct_Decl {
	begin_tok := expect_keyword_token(p, "BEGIN")
	expect_keyword_token(p, "OF")
	ident_tok := expect_token(p, .Ident)

	struct_decl := ast.new(ast.Types_Struct_Decl, begin_tok.range)
	struct_decl.ident = ast.new_ident(ident_tok)
	struct_decl.components = make([dynamic]^ast.Stmt)

	expect_token(p, .Comma)

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "END") {
			break
		}

		if check_keyword(p, "BEGIN") {
			nested_struct := parse_types_struct_decl(p)
			if nested_struct != nil {
				append(&struct_decl.components, &nested_struct.node)
			}
			if !allow_token(p, .Comma) {
				break
			}
			continue
		}

		field_ident_tok := expect_token(p, .Ident)
		expect_keyword_token(p, "TYPE")
		type_expr := parse_type_expr(p)

		length_expr: ^ast.Expr = nil
		if check_keyword(p, "LENGTH") {
			advance_token(p)
			length_expr = parse_expr(p)
		}

		field_decl := ast.new(ast.Types_Decl, field_ident_tok, p.prev_tok)
		field_decl.ident = ast.new_ident(field_ident_tok)
		field_decl.typed = type_expr
		field_decl.length = length_expr
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

	return struct_decl
}