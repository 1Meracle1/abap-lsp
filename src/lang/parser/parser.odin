package lang_parser

import "../ast"
import "../lexer"
import "core:fmt"
import "core:unicode"
import "core:unicode/utf8"

Parser :: struct {
	file:           ^ast.File,
	l:              lexer.Lexer,
	prev_tok:       lexer.Token,
	curr_tok:       lexer.Token,
	keyword_buffer: [128]byte,
}

parse_file :: proc(p: ^Parser, file: ^ast.File) {
	p.prev_tok = {}
	p.curr_tok = {}

	p.file = file
	p.file.syntax_errors = make([dynamic]ast.Diagnostic)
	p.file.decls = make([dynamic]^ast.Stmt)

	lexer.init(&p.l, file.src, error, p)
	if p.l.ch <= 0 {
		return
	}
	file.range.start = 0
	file.range.end = len(file.src)

	advance_token(p)
	for p.curr_tok.kind != .EOF {
		stmt := parse_stmt(p)
		if stmt != nil {
			append(&p.file.decls, stmt)
		}
	}
}

parse_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	if len(p.curr_tok.lit) > 0 && len(p.curr_tok.lit) < len(p.keyword_buffer) {
		keyword := to_upper(p.keyword_buffer[:], p.curr_tok.lit)
		switch keyword {
		case "DATA":
			return parse_data_decl(p)
		}
	}

	start_tok := p.curr_tok
	end_tok := skip_to_new_line(p)
	error(p, lexer.range_between(start_tok, end_tok), "unexpected statement")
	bad_decl := ast.new(ast.Bad_Decl, start_tok, end_tok)
	return bad_decl
}

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

parse_data_typed_single_decl :: proc(p: ^Parser, data_tok: lexer.Token) -> ^ast.Decl {
	ident_tok := expect_token(p, .Ident)
	expect_keyword_token(p, "TYPE")
	type_expr := parse_expr(p)

	data_decl := ast.new(ast.Data_Typed_Decl, data_tok, p.curr_tok)
	data_decl.ident = ast.new_ident(ident_tok)
	data_decl.typed = type_expr
	return data_decl
}

parse_data_typed_multiple_decl :: proc(p: ^Parser, data_tok: lexer.Token) -> ^ast.Decl {
	chain_decl := ast.new(ast.Data_Typed_Chain_Decl, data_tok.range)
	chain_decl.decls = make([dynamic]^ast.Data_Typed_Decl)

	for {
		ident_tok := expect_token(p, .Ident)
		expect_keyword_token(p, "TYPE")
		type_expr := parse_expr(p)

		decl := ast.new(ast.Data_Typed_Decl, ident_tok, p.prev_tok)
		decl.ident = ast.new_ident(ident_tok)
		decl.typed = type_expr
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

skip_to_new_line :: proc(p: ^Parser) -> lexer.Token {
	line_count := p.l.line_count
	for p.curr_tok.kind != .EOF {
		tok := advance_token(p)
		if p.l.line_count > line_count {
			return tok
		}
	}
	return p.curr_tok
}

parse_expr :: proc(p: ^Parser) -> ^ast.Expr {
	return parse_binary_expr(p)
}

parse_binary_expr :: proc(p: ^Parser) -> ^ast.Expr {
	start_tok := p.curr_tok
	expr := parse_unary_expr(p)
	if expr == nil {
		return ast.new(ast.Bad_Expr, start_tok, p.curr_tok)
	}
	return expr
}

parse_unary_expr :: proc(p: ^Parser) -> ^ast.Expr {
	#partial switch p.curr_tok.kind {
	case .Plus, .Minus:
		op := advance_token(p)
		expr := parse_unary_expr(p)
		unary_expr := ast.new(ast.Unary_Expr, op.range)
		unary_expr.op = op
		unary_expr.expr = expr
		return unary_expr
	}
	return parse_atom_expr(p, parse_operand(p))
}

parse_atom_expr :: proc(p: ^Parser, value: ^ast.Expr) -> ^ast.Expr {
	return value
}

parse_operand :: proc(p: ^Parser) -> ^ast.Expr {
	#partial switch p.curr_tok.kind {
	case .Ident:
		tok := advance_token(p)
		return ast.new_ident(tok)
	case .Number, .String:
		tok := advance_token(p)
		basic_lit := ast.new(ast.Basic_Lit, tok.range)
		basic_lit.tok = tok
		return basic_lit
	}
	return nil
}

error :: proc(userptr: rawptr, range: lexer.TextRange, format: string, args: ..any) {
	p := cast(^Parser)userptr
	d: ast.Diagnostic
	d.range = range
	d.message = fmt.aprintf(format, ..args)
	append(&p.file.syntax_errors, d)
}

consume_comments :: proc(p: ^Parser) {
	for p.curr_tok.kind == .Comment {
		append(&p.file.comments, p.curr_tok)
		advance_token(p)
	}
}

advance_token :: proc(p: ^Parser) -> lexer.Token {
	p.prev_tok = p.curr_tok
	prev := p.prev_tok
	p.curr_tok = lexer.scan(&p.l)
	if p.curr_tok.kind != .EOF {
		consume_comments(p)
	}
	return prev
}

expect_keyword_token :: proc(p: ^Parser, expected: string) -> lexer.Token {
	prev := p.curr_tok
	if prev.kind != .Ident {
		error(p, prev.range, "expected identifier, got '%v'", prev.kind)
	}
	if len(p.curr_tok.lit) > 0 && len(p.curr_tok.lit) < len(p.keyword_buffer) {
		keyword := to_upper(p.keyword_buffer[:], p.curr_tok.lit)
		if keyword != expected {
			error(p, prev.range, "expected '%s', got '%s'", expected, p.curr_tok.lit)
		}
	} else {
		error(p, prev.range, "expected '%s', got '%s'", expected, p.curr_tok.lit)
	}
	advance_token(p)
	return prev
}

Space_Requirement_Kind :: enum {
	WithLeadingSpace,
	WithoutLeadingSpace,
	WithTrailingSpace,
	WithoutTrailingSpace,
	WithLeadingTrailingSpace,
	WithoutLeadingTrailingSpace,
}

expect_token_space_req :: proc(
	p: ^Parser,
	kind: lexer.TokenKind,
	space_req_kind: Space_Requirement_Kind,
) -> lexer.Token {
	expected_token_kind := p.curr_tok.kind == kind

	space_before_check := true
	if expected_token_kind {
		#partial switch space_req_kind {
		case .WithLeadingSpace:
		case .WithoutLeadingSpace:
		case .WithLeadingTrailingSpace:
		case .WithoutLeadingTrailingSpace:
			if lexer.have_space_between(p.prev_tok, p.curr_tok) {
				if space_req_kind == .WithoutLeadingSpace ||
				   space_req_kind == .WithLeadingTrailingSpace {
					error(
						p,
						lexer.range_between(p.prev_tok, p.curr_tok),
						"unexpected space between '%s' and '%s'",
						p.prev_tok.lit,
						p.curr_tok.lit,
					)
					space_before_check = false
				}
			} else {
				if space_req_kind == .WithLeadingSpace ||
				   space_req_kind == .WithLeadingTrailingSpace {
					error(
						p,
						lexer.range_between(p.prev_tok, p.curr_tok),
						"expected space between '%s' and '%s'",
						p.prev_tok.lit,
						p.curr_tok.lit,
					)
					space_before_check = false
				}
			}
		}
	}

	tok := expect_token(p, kind)
	if !space_before_check {
		return tok
	}

	if expected_token_kind {
		#partial switch space_req_kind {
		case .WithTrailingSpace:
		case .WithoutTrailingSpace:
		case .WithLeadingTrailingSpace:
		case .WithoutLeadingTrailingSpace:
			if lexer.have_space_between(p.curr_tok, p.curr_tok) {
				if space_req_kind == .WithoutTrailingSpace ||
				   space_req_kind == .WithoutLeadingTrailingSpace {
					error(
						p,
						lexer.range_between(p.prev_tok, p.curr_tok),
						"unexpected space between '%s' and '%s'",
						p.prev_tok.lit,
						p.curr_tok.lit,
					)
				}
			} else {
				if space_req_kind == .WithTrailingSpace ||
				   space_req_kind == .WithLeadingTrailingSpace {
					error(
						p,
						lexer.range_between(p.prev_tok, p.curr_tok),
						"expected space between '%s' and '%s'",
						p.prev_tok.lit,
						p.curr_tok.lit,
					)
				}
			}
		}
	}
	return tok
}

expect_token :: proc(p: ^Parser, kind: lexer.TokenKind) -> lexer.Token {
	prev := p.curr_tok
	if prev.kind != kind {
		error(
			p,
			prev.range,
			"expected '%s', got '%s'",
			lexer.token_kind_string(kind),
			lexer.token_kind_string(prev.kind),
		)
	}
	advance_token(p)
	return prev
}

allow_token :: proc(p: ^Parser, kind: lexer.TokenKind) -> bool {
	if p.curr_tok.kind == kind {
		advance_token(p)
		return true
	}
	return false
}

to_upper :: proc(buffer: []byte, s: string) -> string {
	length := 0
	for r in s {
		ur := unicode.to_upper(r)
		if r < utf8.RUNE_SELF {
			buffer[length] = byte(r)
			length += 1
		} else {
			buf, w := utf8.encode_rune(r)
			for i := 0; i < w; i += 1 {
				buffer[length] = buf[i]
				length += 1
			}
		}
	}
	return string(buffer[:length])
}
