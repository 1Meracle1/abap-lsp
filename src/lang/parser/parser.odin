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
		case "TYPES":
			return parse_types_decl(p)
		case "FORM":
			return parse_form_decl(p)
		}
	}

	// Try to parse as an expression statement or assignment
	if p.curr_tok.kind == .Ident {
		return parse_expr_or_assign_stmt(p)
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

// TYPES declarations

parse_types_decl :: proc(p: ^Parser) -> ^ast.Decl {
	types_tok := expect_token(p, .Ident) // TYPES keyword
	if allow_token(p, .Colon) {
		return parse_types_chain_decl(p, types_tok)
	}
	return parse_types_single_decl(p, types_tok)
}

parse_types_single_decl :: proc(p: ^Parser, types_tok: lexer.Token) -> ^ast.Decl {
	ident_tok := expect_token(p, .Ident)
	expect_keyword_token(p, "TYPE")
	type_expr := parse_expr(p)
	period_tok := expect_token(p, .Period)

	types_decl := ast.new(ast.Types_Decl, types_tok, period_tok)
	types_decl.ident = ast.new_ident(ident_tok)
	types_decl.typed = type_expr
	return types_decl
}

parse_types_chain_decl :: proc(p: ^Parser, types_tok: lexer.Token) -> ^ast.Decl {
	chain_decl := ast.new(ast.Types_Chain_Decl, types_tok.range)
	chain_decl.decls = make([dynamic]^ast.Types_Decl)

	for {
		ident_tok := expect_token(p, .Ident)
		expect_keyword_token(p, "TYPE")
		type_expr := parse_expr(p)

		decl := ast.new(ast.Types_Decl, ident_tok, p.prev_tok)
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

parse_form_decl :: proc(p: ^Parser) -> ^ast.Decl {
	form_tok := expect_token(p, .Ident) // FORM keyword
	ident_tok := expect_token(p, .Ident) // subroutine name

	form_decl := ast.new(ast.Form_Decl, form_tok.range)
	form_decl.ident = ast.new_ident(ident_tok)
	form_decl.tables_params = make([dynamic]^ast.Form_Param)
	form_decl.using_params = make([dynamic]^ast.Form_Param)
	form_decl.changing_params = make([dynamic]^ast.Form_Param)
	form_decl.body = make([dynamic]^ast.Stmt)

	// Parse optional parameter sections: TABLES, USING, CHANGING
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

	// Expect period to end the FORM header
	expect_token(p, .Period)

	// Parse body statements until ENDFORM
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

	// Expect ENDFORM
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
		// Check if this is the start of another section or end of params
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

		// Check for optional TYPE clause
		if p.curr_tok.kind == .Ident {
			if len(p.curr_tok.lit) > 0 && len(p.curr_tok.lit) < len(p.keyword_buffer) {
				keyword := to_upper(p.keyword_buffer[:], p.curr_tok.lit)
				if keyword == "TYPE" {
					advance_token(p) // consume TYPE
					param.typed = parse_expr(p)
					param.range.end = p.prev_tok.range.end
				}
			}
		}

		append(params, param)

		// If period, stop parsing params
		if p.curr_tok.kind == .Period {
			break
		}
	}
}

parse_expr_or_assign_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start_tok := p.curr_tok
	lhs := parse_expr(p)

	// Check for assignment operator
	if p.curr_tok.kind == .Eq {
		op := advance_token(p)
		rhs := parse_expr(p)
		period_tok := expect_token(p, .Period)

		assign_stmt := ast.new(ast.Assign_Stmt, start_tok, period_tok)
		assign_stmt.lhs = make([]^ast.Expr, 1)
		assign_stmt.lhs[0] = lhs
		assign_stmt.op = op
		assign_stmt.rhs = make([]^ast.Expr, 1)
		assign_stmt.rhs[0] = rhs
		return assign_stmt
	}

	// Not an assignment, treat as expression statement
	period_tok := expect_token(p, .Period)
	expr_stmt := ast.new(ast.Expr_Stmt, start_tok, period_tok)
	expr_stmt.expr = lhs
	return expr_stmt
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
	expr := value
	loop: for {
		#partial switch p.curr_tok.kind {
		case .Minus, .FatArrow:
			// Check if this is a selector (no space before the operator and followed by identifier)
			if lexer.have_space_between(p.prev_tok, p.curr_tok) {
				break loop
			}
			op := advance_token(p)
			field_tok := expect_token(p, .Ident)
			selector := ast.new(ast.Selector_Expr, lexer.TextRange{expr.range.start, field_tok.range.end})
			selector.expr = expr
			selector.op = op
			selector.field = ast.new_ident(field_tok)
			expr = selector
		case:
			break loop
		}
	}
	return expr
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
