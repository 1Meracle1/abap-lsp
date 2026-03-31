package lang_parser

import "../ast"
import "../lexer"
import "core:strings"

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
	parse_optional_length_decimals(p, &length_expr)

	period_tok := expect_token(p, .Period)

	types_decl := ast.new(ast.Types_Decl, types_tok, period_tok)
	types_decl.ident = ast.new_ident(ident_tok)
	types_decl.typed = type_expr
	types_decl.length = length_expr
	return types_decl
}

finish_types_chain_or_single_struct :: proc(
	p: ^Parser,
	chain_decl: ^ast.Types_Chain_Decl,
) -> ^ast.Decl {
	if allow_token(p, .Comma) {
		return nil
	}
	period_tok := expect_token(p, .Period)
	chain_decl.range.end = period_tok.range.end
	if len(chain_decl.parts) == 1 {
		if only_struct, ok := chain_decl.parts[0].derived_stmt.(^ast.Types_Struct_Decl); ok {
			only_struct.range.end = period_tok.range.end
			return only_struct
		}
	}
	return chain_decl
}

parse_types_chain_decl :: proc(p: ^Parser, types_tok: lexer.Token) -> ^ast.Decl {
	chain_decl := ast.new(ast.Types_Chain_Decl, types_tok.range)
	chain_decl.parts = make([dynamic]^ast.Stmt)

	for {
		if check_keyword(p, "BEGIN") {
			struct_decl := parse_types_struct_decl(p)
			if struct_decl != nil {
				append(&chain_decl.parts, &struct_decl.node)
			}
			if done := finish_types_chain_or_single_struct(p, chain_decl); done != nil {
				return done
			}
			continue
		}

		ident_tok := expect_token(p, .Ident)
		expect_keyword_token(p, "TYPE")
		type_expr := parse_type_expr(p)

		length_expr: ^ast.Expr = nil
		parse_optional_length_decimals(p, &length_expr)

		decl := ast.new(ast.Types_Decl, ident_tok, p.prev_tok)
		decl.ident = ast.new_ident(ident_tok)
		decl.typed = type_expr
		decl.length = length_expr
		append(&chain_decl.parts, &decl.node)

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

	allow_token(p, .Comma)
	allow_token(p, .Period)

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "END") {
			break
		}

		// Optional nested "TYPES:" header inside the structure (ABAP allows chaining)
		if check_keyword(p, "TYPES") {
			advance_token(p)
			allow_token(p, .Colon)
			continue
		}

		if check_keyword(p, "INCLUDE") {
			include_start := expect_keyword_token(p, "INCLUDE")
			expect_keyword_token(p, "TYPE")
			type_expr := parse_type_expr(p)
			as_name: ^ast.Ident = nil
			if check_keyword(p, "AS") {
				advance_token(p)
				as_tok := expect_token(p, .Ident)
				as_name = ast.new_ident(as_tok)
			}
			period_tok := expect_token(p, .Period)
			inc_decl := ast.new(ast.Types_Include_Type_Decl, include_start, period_tok)
			inc_decl.included = type_expr
			inc_decl.as_name = as_name
			append(&struct_decl.components, &inc_decl.node)
			continue
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
		parse_optional_length_decimals(p, &length_expr)

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

	return struct_decl
}
