package lang_parser

import "../ast"
import "../lexer"
import "core:fmt"
import "core:mem"
import "core:strings"
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
		// mem.set(&p.keyword_like_buffer[0], 0, len(p.keyword_like_buffer))
		keyword := to_upper(p.keyword_buffer[:], p.curr_tok.lit)
		switch keyword {
		case "DATA":
			return parse_data_decl(p)
		}
	}
	return nil
}

parse_data_decl :: proc(p: ^Parser) -> ^ast.Decl {
	data_tok := expect_token(p, .Ident)
	if p.curr_tok.kind == .LParen {
		return parse_data_inline_decl(p, data_tok)
	}
	return nil
}

parse_data_inline_decl :: proc(p: ^Parser, data_tok: lexer.Token) -> ^ast.Decl {
	if lexer.have_space_between(p.prev_tok, p.curr_tok) {
		error(
			p,
			lexer.range_between(p.prev_tok, p.curr_tok),
			"unexpected space between 'DATA' and '('",
		)
	}
	expect_token(p, .LParen)
	ident_tok := expect_token(p, .Ident)
	if p.curr_tok.kind == .RParen {
		if lexer.have_space_between(p.prev_tok, p.curr_tok) {
			error(
				p,
				lexer.range_between(p.prev_tok, p.curr_tok),
				"unexpected space between identifier and ')'",
			)
		}
	}
	expect_token(p, .RParen)
	expect_token(p, .Eq)
	expr := parse_expr(p)
	period_tok := expect_token(p, .Period)

	data_decl := ast.new(ast.Data_Inline_Decl, data_tok, period_tok)
	data_decl.ident = ast.new_ident(ident_tok)
	// data_decl.value = expr
	return data_decl
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

error :: proc(userptr: rawptr, range: lexer.TextRange, format: string, args: ..any) {
	p := cast(^Parser)userptr
	d: ast.Diagnostic
	d.range = range
	d.message = fmt.aprintf(format, args)
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

expect_token :: proc(p: ^Parser, kind: lexer.TokenKind) -> lexer.Token {
	prev := p.curr_tok
	if prev.kind != kind {
		error(p, prev.range, "expected '%v', got '%v'", kind, prev.kind)
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
