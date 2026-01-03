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
		case "CLASS":
			return parse_class_decl(p)
		case "INTERFACE":
			return parse_interface_decl(p)
		case "METHOD":
			return parse_method_impl(p)
		case "REPORT":
			return parse_report_decl(p)
		case "INCLUDE":
			return parse_include_decl(p)
		case "INITIALIZATION":
			return parse_event_block(p, keyword)
		case "AT":
			return parse_at_event_block(p)
		case "CALL":
			return parse_call_stmt(p)
		case "MODULE":
			return parse_module_decl(p)
		case "IF":
			return parse_if_stmt(p)
		case "START":
			if check_compound_keyword(p, "START", "OF", "SELECTION") {
				return parse_event_block(p, "START-OF-SELECTION")
			}
		case "END":
			if check_compound_keyword(p, "END", "OF", "SELECTION") {
				return parse_event_block(p, "END-OF-SELECTION")
			}
		case "TOP":
			if check_compound_keyword(p, "TOP", "OF", "PAGE") {
				return parse_event_block(p, "TOP-OF-PAGE")
			}
		case "MODIFY":
			return parse_modify_stmt(p)
		case "LEAVE":
			return parse_leave_stmt(p)
		case "SET":
			return parse_set_stmt(p)
		}
	}

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
	type_expr := parse_expr(p)

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
		type_expr := parse_expr(p)

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
		type_expr := parse_expr(p)

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

check_keyword :: proc(p: ^Parser, expected: string) -> bool {
	if p.curr_tok.kind != .Ident {
		return false
	}
	if len(p.curr_tok.lit) > 0 && len(p.curr_tok.lit) < len(p.keyword_buffer) {
		keyword := to_upper(p.keyword_buffer[:], p.curr_tok.lit)
		return keyword == expected
	}
	return false
}

// check_compound_keyword checks for a hyphenated keyword like START-OF-SELECTION
// It returns true and advances the parser if the compound keyword matches
check_compound_keyword :: proc(p: ^Parser, first: string, second: string, third: string) -> bool {
	if !check_keyword(p, first) {
		return false
	}

	saved_prev := p.prev_tok
	saved_curr := p.curr_tok
	saved_pos := p.l.pos
	saved_read_pos := p.l.read_pos
	saved_ch := p.l.ch

	advance_token(p) // consume first
	if p.curr_tok.kind != .Minus || lexer.have_space_between(saved_curr, p.curr_tok) {
		p.prev_tok = saved_prev
		p.curr_tok = saved_curr
		p.l.pos = saved_pos
		p.l.read_pos = saved_read_pos
		p.l.ch = saved_ch
		return false
	}

	advance_token(p) // consume -
	if lexer.have_space_between(p.prev_tok, p.curr_tok) || !check_keyword(p, second) {
		p.prev_tok = saved_prev
		p.curr_tok = saved_curr
		p.l.pos = saved_pos
		p.l.read_pos = saved_read_pos
		p.l.ch = saved_ch
		return false
	}

	advance_token(p) // consume second
	if p.curr_tok.kind != .Minus || lexer.have_space_between(p.prev_tok, p.curr_tok) {
		p.prev_tok = saved_prev
		p.curr_tok = saved_curr
		p.l.pos = saved_pos
		p.l.read_pos = saved_read_pos
		p.l.ch = saved_ch
		return false
	}

	advance_token(p) // consume -
	if lexer.have_space_between(p.prev_tok, p.curr_tok) || !check_keyword(p, third) {
		p.prev_tok = saved_prev
		p.curr_tok = saved_curr
		p.l.pos = saved_pos
		p.l.read_pos = saved_read_pos
		p.l.ch = saved_ch
		return false
	}

	advance_token(p) // consume third
	return true
}

check_class_keyword :: proc(p: ^Parser, first: string, second: string) -> bool {
	if !check_keyword(p, first) {
		return false
	}

	saved_prev := p.prev_tok
	saved_curr := p.curr_tok
	saved_pos := p.l.pos
	saved_read_pos := p.l.read_pos
	saved_ch := p.l.ch

	advance_token(p)
	if p.curr_tok.kind != .Minus || lexer.have_space_between(saved_curr, p.curr_tok) {
		p.prev_tok = saved_prev
		p.curr_tok = saved_curr
		p.l.pos = saved_pos
		p.l.read_pos = saved_read_pos
		p.l.ch = saved_ch
		return false
	}

	advance_token(p)
	if !check_keyword(p, second) || lexer.have_space_between(p.prev_tok, p.curr_tok) {
		p.prev_tok = saved_prev
		p.curr_tok = saved_curr
		p.l.pos = saved_pos
		p.l.read_pos = saved_read_pos
		p.l.ch = saved_ch
		return false
	}

	advance_token(p)
	return true
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
					param.typed = parse_expr(p)
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

parse_expr_or_assign_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start_tok := p.curr_tok
	lhs := parse_expr(p)

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
		// Early exit if expr is nil - can't build selector or call expressions
		if expr == nil {
			break loop
		}
		#partial switch p.curr_tok.kind {
		case .Minus, .FatArrow, .Tilde, .Arrow:
			if lexer.have_space_between(p.prev_tok, p.curr_tok) {
				break loop
			}
			op := advance_token(p)
			// Handle TEXT-nnn text symbol references where nnn is a number
			// Also allow numbers as selectors for error resilience
			field_tok: lexer.Token
			if p.curr_tok.kind == .Ident || p.curr_tok.kind == .Number {
				field_tok = advance_token(p)
			} else {
				field_tok = expect_token(p, .Ident) // Will error but still advance
			}
			selector := ast.new(
				ast.Selector_Expr,
				lexer.TextRange{expr.range.start, field_tok.range.end},
			)
			selector.expr = expr
			selector.op = op
			selector.field = ast.new_ident(field_tok)
			expr = selector
		case .LParen:
			// Call expression - parentheses immediately after expression (no space)
			if lexer.have_space_between(p.prev_tok, p.curr_tok) {
				break loop
			}
			lparen_tok := advance_token(p) // consume (
			
			call_expr := ast.new(ast.Call_Expr, lexer.TextRange{expr.range.start, 0})
			call_expr.expr = expr
			args := make([dynamic]^ast.Expr)
			
			// Parse arguments with safety limit to prevent infinite loops
			max_iterations := 1000
			iterations := 0
			if p.curr_tok.kind != .RParen {
				for iterations < max_iterations {
					iterations += 1
					
					// Save position to detect if we make progress
					prev_pos := p.curr_tok.range.start
					
					arg := parse_call_arg(p)
					if arg != nil {
						append(&args, arg)
					}
					
					if p.curr_tok.kind == .RParen {
						break
					}
					if p.curr_tok.kind == .EOF || p.curr_tok.kind == .Period {
						break
					}
					
					// If we didn't make any progress, skip the current token to avoid infinite loop
					if p.curr_tok.range.start == prev_pos {
						error(p, p.curr_tok.range, "unexpected token '%s' in function call", p.curr_tok.lit)
						advance_token(p)
						// Check again after advancing - might hit terminator
						if p.curr_tok.kind == .RParen || p.curr_tok.kind == .EOF || p.curr_tok.kind == .Period {
							break
						}
					}
				}
				
				// Safety: if we hit max iterations, skip to closing paren or statement end
				if iterations >= max_iterations {
					error(p, lparen_tok.range, "too many arguments or malformed function call")
					skip_to_matching_paren_or_period(p)
				}
			}
			
			rparen_tok := expect_token(p, .RParen)
			call_expr.args = args[:]
			call_expr.range.end = rparen_tok.range.end
			call_expr.derived_expr = call_expr
			expr = call_expr
		case:
			break loop
		}
	}
	return expr
}

// skip_to_matching_paren_or_period skips tokens until we find a closing paren or period
// Used for error recovery in malformed expressions
skip_to_matching_paren_or_period :: proc(p: ^Parser) {
	depth := 1
	for p.curr_tok.kind != .EOF {
		if p.curr_tok.kind == .LParen {
			depth += 1
		} else if p.curr_tok.kind == .RParen {
			depth -= 1
			if depth <= 0 {
				return // Stop before the closing paren so expect_token can consume it
			}
		} else if p.curr_tok.kind == .Period {
			return // Stop at period
		}
		advance_token(p)
	}
}

// parse_call_arg parses a single call argument, which may be a named argument like "param = value"
parse_call_arg :: proc(p: ^Parser) -> ^ast.Expr {
	start_tok := p.curr_tok
	
	// Check if this is a named argument (identifier followed by = with spaces)
	if p.curr_tok.kind == .Ident {
		// Save parser state
		saved_prev := p.prev_tok
		saved_curr := p.curr_tok
		saved_pos := p.l.pos
		saved_read_pos := p.l.read_pos
		saved_ch := p.l.ch
		
		ident_tok := advance_token(p)
		
		// Check if next token is = with space before it (named argument pattern)
		if p.curr_tok.kind == .Eq && lexer.have_space_between(ident_tok, p.curr_tok) {
			advance_token(p) // consume =
			value := parse_expr(p)
			
			named_arg := ast.new(ast.Named_Arg, lexer.TextRange{start_tok.range.start, value.range.end})
			named_arg.name = ast.new_ident(ident_tok)
			named_arg.value = value
			named_arg.derived_expr = named_arg
			return named_arg
		}
		
		// Not a named argument, restore parser state
		p.prev_tok = saved_prev
		p.curr_tok = saved_curr
		p.l.pos = saved_pos
		p.l.read_pos = saved_read_pos
		p.l.ch = saved_ch
	}
	
	// Regular argument
	arg := parse_expr(p)
	return arg
}

parse_operand :: proc(p: ^Parser) -> ^ast.Expr {
	#partial switch p.curr_tok.kind {
	case .Ident:
		// Check for NEW keyword
		if check_keyword(p, "NEW") {
			return parse_new_expr(p)
		}
		// Check for CONV keyword (type conversion constructor)
		if check_keyword(p, "CONV") {
			return parse_conv_expr(p)
		}
		// Check for other constructor expressions that use # syntax
		if check_keyword(p, "COND") || check_keyword(p, "SWITCH") ||
		   check_keyword(p, "VALUE") || check_keyword(p, "REF") ||
		   check_keyword(p, "CAST") || check_keyword(p, "EXACT") ||
		   check_keyword(p, "CORRESPONDING") || check_keyword(p, "REDUCE") ||
		   check_keyword(p, "FILTER") {
			return parse_constructor_expr(p)
		}
		tok := advance_token(p)
		return ast.new_ident(tok)
	case .Number, .String:
		tok := advance_token(p)
		basic_lit := ast.new(ast.Basic_Lit, tok.range)
		basic_lit.tok = tok
		return basic_lit
	case .Hash:
		// Standalone # is not valid, but consume it to avoid infinite loops
		hash_tok := advance_token(p)
		error(p, hash_tok.range, "unexpected '#' token - type inference marker must follow a constructor keyword")
		bad_expr := ast.new(ast.Bad_Expr, hash_tok.range)
		return bad_expr
	}
	return nil
}

// parse_new_expr parses a NEW instance operator expression
// Syntax: NEW type( args ) or NEW #( args )
parse_new_expr :: proc(p: ^Parser) -> ^ast.Expr {
	new_tok := expect_keyword_token(p, "NEW")

	new_expr := ast.new(ast.New_Expr, new_tok.range)
	new_expr.args = make([dynamic]^ast.Expr)

	// Check for # (type inference) or type expression
	if p.curr_tok.kind == .Hash {
		advance_token(p) // consume #
		new_expr.is_inferred = true
		new_expr.type_expr = nil
	} else {
		// Parse type expression (could be identifier or selector, but not call)
		// Use parse_new_type_expr to avoid call expression parsing interference
		new_expr.type_expr = parse_new_type_expr(p)
		new_expr.is_inferred = false
	}

	// Expect opening parenthesis (possibly without space)
	if p.curr_tok.kind == .LParen {
		lparen_tok := advance_token(p) // consume (

		// Parse arguments (if any) with safety limit
		max_iterations := 1000
		iterations := 0
		if p.curr_tok.kind != .RParen {
			for iterations < max_iterations {
				iterations += 1
				
				// Save position to detect if we make progress
				prev_pos := p.curr_tok.range.start
				
				arg := parse_expr(p)
				if arg != nil {
					append(&new_expr.args, arg)
				}

				// No comma support between args in ABAP NEW, just close paren
				if p.curr_tok.kind == .RParen {
					break
				}
				if p.curr_tok.kind == .EOF || p.curr_tok.kind == .Period {
					break
				}
				
				// If we didn't make any progress, skip the current token to avoid infinite loop
				if p.curr_tok.range.start == prev_pos {
					error(p, p.curr_tok.range, "unexpected token '%s' in NEW expression", p.curr_tok.lit)
					advance_token(p)
					// Check again after advancing
					if p.curr_tok.kind == .RParen || p.curr_tok.kind == .EOF || p.curr_tok.kind == .Period {
						break
					}
				}
			}
			
			// Safety: if we hit max iterations, skip to closing paren or statement end
			if iterations >= max_iterations {
				error(p, lparen_tok.range, "too many arguments or malformed NEW expression")
				skip_to_matching_paren_or_period(p)
			}
		}

		rparen_tok := expect_token(p, .RParen)
		new_expr.range.end = rparen_tok.range.end
	}

	return new_expr
}

// parse_new_type_expr parses a type expression for NEW without triggering call expression parsing
// This handles simple identifiers and selector expressions (e.g., NEW my_class, NEW if_interface~ty_struct)
parse_new_type_expr :: proc(p: ^Parser) -> ^ast.Expr {
	expr := parse_simple_type_expr(p)
	return expr
}

// parse_simple_type_expr parses a simple type expression (identifier or selector)
// without triggering call expression or constructor parsing
parse_simple_type_expr :: proc(p: ^Parser) -> ^ast.Expr {
	if p.curr_tok.kind != .Ident {
		return nil
	}
	
	tok := advance_token(p)
	expr: ^ast.Expr = ast.new_ident(tok)
	
	// Handle selector expressions for types like my_class~ty_type or interface~method
	// but NOT call expressions (parentheses belong to the constructor, not the type)
	loop: for {
		#partial switch p.curr_tok.kind {
		case .Minus, .FatArrow, .Tilde, .Arrow:
			if lexer.have_space_between(p.prev_tok, p.curr_tok) {
				break loop
			}
			op := advance_token(p)
			field_tok := expect_token(p, .Ident)
			selector := ast.new(
				ast.Selector_Expr,
				lexer.TextRange{expr.range.start, field_tok.range.end},
			)
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

// parse_conv_expr parses a CONV type conversion expression
// Syntax: CONV type( expr ) or CONV #( expr )
parse_conv_expr :: proc(p: ^Parser) -> ^ast.Expr {
	conv_tok := expect_keyword_token(p, "CONV")
	return parse_constructor_body(p, conv_tok)
}

// parse_constructor_expr parses various ABAP constructor expressions
// Syntax: KEYWORD type( args ) or KEYWORD #( args )
// Handles: COND, SWITCH, VALUE, REF, CAST, EXACT, CORRESPONDING, REDUCE, FILTER
parse_constructor_expr :: proc(p: ^Parser) -> ^ast.Expr {
	keyword_tok := advance_token(p) // consume the constructor keyword
	return parse_constructor_body(p, keyword_tok)
}

// parse_constructor_body parses the body of a constructor expression after the keyword
// This handles both explicit type ( args ) and inferred type #( args )
parse_constructor_body :: proc(p: ^Parser, keyword_tok: lexer.Token) -> ^ast.Expr {
	constructor_expr := ast.new(ast.Constructor_Expr, keyword_tok.range)
	constructor_expr.keyword = keyword_tok
	constructor_expr.args = make([dynamic]^ast.Expr)

	// Check for # (type inference) or type expression
	if p.curr_tok.kind == .Hash {
		advance_token(p) // consume #
		constructor_expr.is_inferred = true
		constructor_expr.type_expr = nil
	} else if p.curr_tok.kind == .Ident {
		// Parse type expression (could be identifier or selector, but not call)
		constructor_expr.type_expr = parse_simple_type_expr(p)
		constructor_expr.is_inferred = false
	} else {
		// Error: expected type or #
		error(p, p.curr_tok.range, "expected type or '#' after constructor keyword")
		constructor_expr.is_inferred = true
		constructor_expr.type_expr = nil
	}

	// Expect opening parenthesis
	if p.curr_tok.kind == .LParen {
		lparen_tok := advance_token(p) // consume (

		// Parse arguments with safety limit
		max_iterations := 1000
		iterations := 0
		if p.curr_tok.kind != .RParen {
			for iterations < max_iterations {
				iterations += 1
				
				// Save position to detect if we make progress
				prev_pos := p.curr_tok.range.start
				
				arg := parse_call_arg(p)
				if arg != nil {
					append(&constructor_expr.args, arg)
				}

				if p.curr_tok.kind == .RParen {
					break
				}
				if p.curr_tok.kind == .EOF || p.curr_tok.kind == .Period {
					break
				}
				
				// If we didn't make any progress, skip the current token to avoid infinite loop
				if p.curr_tok.range.start == prev_pos {
					error(p, p.curr_tok.range, "unexpected token '%s' in constructor expression", p.curr_tok.lit)
					advance_token(p)
					if p.curr_tok.kind == .RParen || p.curr_tok.kind == .EOF || p.curr_tok.kind == .Period {
						break
					}
				}
			}
			
			// Safety: if we hit max iterations, skip to closing paren or statement end
			if iterations >= max_iterations {
				error(p, lparen_tok.range, "too many arguments or malformed constructor expression")
				skip_to_matching_paren_or_period(p)
			}
		}

		rparen_tok := expect_token(p, .RParen)
		constructor_expr.range.end = rparen_tok.range.end
	} else {
		error(p, p.curr_tok.range, "expected '(' after constructor type")
	}

	return constructor_expr
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

parse_class_decl :: proc(p: ^Parser) -> ^ast.Decl {
	class_tok := expect_token(p, .Ident)
	ident_tok := expect_token(p, .Ident)

	if check_keyword(p, "DEFINITION") {
		return parse_class_def_decl(p, class_tok, ident_tok)
	} else if check_keyword(p, "IMPLEMENTATION") {
		return parse_class_impl_decl(p, class_tok, ident_tok)
	}

	error(p, p.curr_tok.range, "expected DEFINITION or IMPLEMENTATION after class name")
	end_tok := skip_to_new_line(p)
	bad_decl := ast.new(ast.Bad_Decl, class_tok, end_tok)
	return bad_decl
}

parse_class_def_decl :: proc(
	p: ^Parser,
	class_tok: lexer.Token,
	ident_tok: lexer.Token,
) -> ^ast.Decl {
	expect_keyword_token(p, "DEFINITION")

	class_decl := ast.new(ast.Class_Def_Decl, class_tok.range)
	class_decl.ident = ast.new_ident(ident_tok)
	class_decl.sections = make([dynamic]^ast.Class_Section)

	for p.curr_tok.kind == .Ident && p.curr_tok.kind != .Period {
		if check_keyword(p, "ABSTRACT") {
			advance_token(p)
			class_decl.is_abstract = true
		} else if check_keyword(p, "FINAL") {
			advance_token(p)
			class_decl.is_final = true
		} else if check_keyword(p, "INHERITING") {
			advance_token(p)
			expect_keyword_token(p, "FROM")
			class_decl.inheriting_from = parse_expr(p)
		} else {
			break
		}
	}

	expect_token(p, .Period)

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "ENDCLASS") {
			break
		}

		if section := parse_class_section(p); section != nil {
			append(&class_decl.sections, section)
		} else {
			skip_to_new_line(p)
		}
	}

	endclass_tok := expect_keyword_token(p, "ENDCLASS")
	period_tok := expect_token(p, .Period)
	class_decl.range.end = period_tok.range.end
	_ = endclass_tok

	return class_decl
}

parse_class_impl_decl :: proc(
	p: ^Parser,
	class_tok: lexer.Token,
	ident_tok: lexer.Token,
) -> ^ast.Decl {
	expect_keyword_token(p, "IMPLEMENTATION")
	expect_token(p, .Period)

	class_impl := ast.new(ast.Class_Impl_Decl, class_tok.range)
	class_impl.ident = ast.new_ident(ident_tok)
	class_impl.methods = make([dynamic]^ast.Stmt)

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "ENDCLASS") {
			break
		}

		if check_keyword(p, "METHOD") {
			method_impl := parse_method_impl(p)
			if method_impl != nil {
				append(&class_impl.methods, method_impl)
			}
		} else {
			skip_to_new_line(p)
		}
	}

	endclass_tok := expect_keyword_token(p, "ENDCLASS")
	period_tok := expect_token(p, .Period)
	class_impl.range.end = period_tok.range.end
	_ = endclass_tok

	return class_impl
}

parse_class_section :: proc(p: ^Parser) -> ^ast.Class_Section {
	access: ast.Access_Modifier

	if check_keyword(p, "PUBLIC") {
		advance_token(p)
		access = .Public
	} else if check_keyword(p, "PROTECTED") {
		advance_token(p)
		access = .Protected
	} else if check_keyword(p, "PRIVATE") {
		advance_token(p)
		access = .Private
	} else {
		return nil
	}

	section_tok := expect_keyword_token(p, "SECTION")
	expect_token(p, .Period)

	section := ast.new(ast.Class_Section, section_tok.range)
	section.access = access
	section.types = make([dynamic]^ast.Stmt)
	section.data = make([dynamic]^ast.Stmt)
	section.methods = make([dynamic]^ast.Stmt)
	section.interfaces = make([dynamic]^ast.Stmt)

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "PUBLIC") ||
		   check_keyword(p, "PROTECTED") ||
		   check_keyword(p, "PRIVATE") ||
		   check_keyword(p, "ENDCLASS") {
			break
		}

		if check_keyword(p, "TYPES") {
			types_decl := parse_types_decl(p)
			if types_decl != nil {
				append(&section.types, types_decl)
			}
		} else if check_keyword(p, "DATA") {
			data_decl := parse_class_data_decl(p, false)
			if data_decl != nil {
				append(&section.data, data_decl)
			}
		} else if check_class_keyword(p, "CLASS", "DATA") {
			data_decl := parse_class_data_decl(p, true)
			if data_decl != nil {
				append(&section.data, data_decl)
			}
		} else if check_keyword(p, "METHODS") {
			method_decl := parse_method_decl(p, false)
			if method_decl != nil {
				append(&section.methods, method_decl)
			}
		} else if check_class_keyword(p, "CLASS", "METHODS") {
			method_decl := parse_method_decl(p, true)
			if method_decl != nil {
				append(&section.methods, method_decl)
			}
		} else if check_keyword(p, "INTERFACES") {
			ifaces_decl := parse_interfaces_decl(p)
			if ifaces_decl != nil {
				append(&section.interfaces, ifaces_decl)
			}
		} else {
			skip_to_new_line(p)
		}
	}

	return section
}

parse_class_data_decl :: proc(p: ^Parser, is_class: bool) -> ^ast.Stmt {
	data_tok: lexer.Token
	if is_class {
		data_tok = p.prev_tok
	} else {
		data_tok = advance_token(p)
	}

	if allow_token(p, .Colon) {
		return parse_class_data_chain_decl(p, data_tok, is_class)
	}

	return parse_class_data_single_decl(p, data_tok, is_class)
}

parse_class_data_single_decl :: proc(
	p: ^Parser,
	data_tok: lexer.Token,
	is_class: bool,
) -> ^ast.Stmt {
	ident_tok := expect_token(p, .Ident)
	expect_keyword_token(p, "TYPE")
	type_expr := parse_expr(p)

	is_read_only := false
	if check_keyword(p, "READ-ONLY") {
		advance_token(p)
		is_read_only = true
	}

	period_tok := expect_token(p, .Period)

	attr_decl := ast.new(ast.Attr_Decl, data_tok, period_tok)
	attr_decl.ident = ast.new_ident(ident_tok)
	attr_decl.typed = type_expr
	attr_decl.is_class = is_class
	attr_decl.is_read_only = is_read_only
	attr_decl.derived_stmt = attr_decl
	return attr_decl
}

parse_class_data_chain_decl :: proc(
	p: ^Parser,
	data_tok: lexer.Token,
	is_class: bool,
) -> ^ast.Stmt {
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
			continue
		}

		period_tok := expect_token(p, .Period)
		chain_decl.range.end = period_tok.range.end
		break
	}

	return chain_decl
}

parse_method_decl :: proc(p: ^Parser, is_class: bool) -> ^ast.Stmt {
	method_tok: lexer.Token
	if is_class {
		method_tok = p.prev_tok
	} else {
		method_tok = advance_token(p)
	}

	if allow_token(p, .Colon) {
		return parse_method_chain_decl(p, method_tok, is_class)
	}

	return parse_method_single_decl(p, method_tok, is_class)
}

parse_method_single_decl :: proc(
	p: ^Parser,
	method_tok: lexer.Token,
	is_class: bool,
) -> ^ast.Stmt {
	ident_tok := expect_token(p, .Ident)

	method_decl := ast.new(ast.Method_Decl, method_tok.range)
	method_decl.ident = ast.new_ident(ident_tok)
	method_decl.is_class = is_class
	method_decl.params = make([dynamic]^ast.Method_Param)
	method_decl.raising = make([dynamic]^ast.Expr)

	for p.curr_tok.kind != .EOF && p.curr_tok.kind != .Period {
		if check_keyword(p, "ABSTRACT") {
			advance_token(p)
			method_decl.is_abstract = true
		} else if check_keyword(p, "FINAL") {
			advance_token(p)
			method_decl.is_final = true
		} else if check_keyword(p, "REDEFINITION") {
			advance_token(p)
			method_decl.is_redefinition = true
		} else if check_keyword(p, "IMPORTING") {
			advance_token(p)
			parse_method_params(p, &method_decl.params, .Importing)
		} else if check_keyword(p, "EXPORTING") {
			advance_token(p)
			parse_method_params(p, &method_decl.params, .Exporting)
		} else if check_keyword(p, "CHANGING") {
			advance_token(p)
			parse_method_params(p, &method_decl.params, .Changing)
		} else if check_keyword(p, "RETURNING") {
			advance_token(p)
			parse_method_params(p, &method_decl.params, .Returning)
		} else if check_keyword(p, "RAISING") {
			advance_token(p)
			parse_raising_clause(p, &method_decl.raising)
		} else {
			break
		}
	}

	period_tok := expect_token(p, .Period)
	method_decl.range.end = period_tok.range.end
	method_decl.derived_stmt = method_decl
	return method_decl
}

parse_method_chain_decl :: proc(p: ^Parser, method_tok: lexer.Token, is_class: bool) -> ^ast.Stmt {
	return parse_method_single_decl(p, method_tok, is_class)
}

parse_method_params :: proc(
	p: ^Parser,
	params: ^[dynamic]^ast.Method_Param,
	kind: ast.Method_Param_Kind,
) {
	for p.curr_tok.kind == .Ident && p.curr_tok.kind != .Period {
		if check_keyword(p, "IMPORTING") ||
		   check_keyword(p, "EXPORTING") ||
		   check_keyword(p, "CHANGING") ||
		   check_keyword(p, "RETURNING") ||
		   check_keyword(p, "RAISING") ||
		   check_keyword(p, "ABSTRACT") ||
		   check_keyword(p, "FINAL") ||
		   check_keyword(p, "REDEFINITION") {
			break
		}

		if kind == .Returning && check_keyword(p, "VALUE") {
			advance_token(p)
			expect_token(p, .LParen)
			ident_tok := expect_token(p, .Ident)
			expect_token(p, .RParen)

			param := ast.new(ast.Method_Param, ident_tok.range)
			param.kind = kind
			param.ident = ast.new_ident(ident_tok)

			if check_keyword(p, "TYPE") {
				advance_token(p)
				param.typed = parse_expr(p)
			}

			append(params, param)
			continue
		}

		ident_tok := advance_token(p)

		param := ast.new(ast.Method_Param, ident_tok.range)
		param.kind = kind
		param.ident = ast.new_ident(ident_tok)

		if check_keyword(p, "TYPE") {
			advance_token(p)
			param.typed = parse_expr(p)
		}

		if check_keyword(p, "OPTIONAL") {
			advance_token(p)
			param.optional = true
		}

		if check_keyword(p, "DEFAULT") {
			advance_token(p)
			param.default = parse_expr(p)
		}

		append(params, param)
	}
}

parse_raising_clause :: proc(p: ^Parser, raising: ^[dynamic]^ast.Expr) {
	for p.curr_tok.kind == .Ident && p.curr_tok.kind != .Period {
		if check_keyword(p, "IMPORTING") ||
		   check_keyword(p, "EXPORTING") ||
		   check_keyword(p, "CHANGING") ||
		   check_keyword(p, "RETURNING") {
			break
		}

		exception_name := parse_expr(p)
		append(raising, exception_name)
	}
}

parse_interfaces_decl :: proc(p: ^Parser) -> ^ast.Stmt {
	ifaces_tok := expect_keyword_token(p, "INTERFACES")

	ifaces_decl := ast.new(ast.Interfaces_Decl, ifaces_tok.range)
	ifaces_decl.names = make([dynamic]^ast.Ident)

	for p.curr_tok.kind == .Ident && p.curr_tok.kind != .Period {
		ident_tok := advance_token(p)
		append(&ifaces_decl.names, ast.new_ident(ident_tok))
	}

	period_tok := expect_token(p, .Period)
	ifaces_decl.range.end = period_tok.range.end
	ifaces_decl.derived_stmt = ifaces_decl
	return ifaces_decl
}

parse_method_impl :: proc(p: ^Parser) -> ^ast.Stmt {
	method_tok := expect_keyword_token(p, "METHOD")

	name_expr := parse_expr(p)

	expect_token(p, .Period)

	method_impl := ast.new(ast.Method_Impl, method_tok.range)
	method_impl.ident = name_expr
	method_impl.body = make([dynamic]^ast.Stmt)

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "ENDMETHOD") {
			break
		}

		stmt := parse_stmt(p)
		if stmt != nil {
			append(&method_impl.body, stmt)
		}
	}

	endmethod_tok := expect_keyword_token(p, "ENDMETHOD")
	period_tok := expect_token(p, .Period)
	method_impl.range.end = period_tok.range.end
	method_impl.derived_stmt = method_impl
	_ = endmethod_tok

	return method_impl
}

parse_interface_decl :: proc(p: ^Parser) -> ^ast.Decl {
	iface_tok := expect_token(p, .Ident)
	ident_tok := expect_token(p, .Ident)

	expect_token(p, .Period)

	iface_decl := ast.new(ast.Interface_Decl, iface_tok.range)
	iface_decl.ident = ast.new_ident(ident_tok)
	iface_decl.methods = make([dynamic]^ast.Stmt)
	iface_decl.types = make([dynamic]^ast.Stmt)
	iface_decl.data = make([dynamic]^ast.Stmt)

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "ENDINTERFACE") {
			break
		}

		if check_keyword(p, "METHODS") {
			method_decl := parse_method_decl(p, false)
			if method_decl != nil {
				append(&iface_decl.methods, method_decl)
			}
		} else if check_class_keyword(p, "CLASS", "METHODS") {
			method_decl := parse_method_decl(p, true)
			if method_decl != nil {
				append(&iface_decl.methods, method_decl)
			}
		} else if check_keyword(p, "TYPES") {
			types_decl := parse_types_decl(p)
			if types_decl != nil {
				append(&iface_decl.types, types_decl)
			}
		} else if check_keyword(p, "DATA") {
			data_decl := parse_class_data_decl(p, false)
			if data_decl != nil {
				append(&iface_decl.data, data_decl)
			}
		} else {
			skip_to_new_line(p)
		}
	}

	endiface_tok := expect_keyword_token(p, "ENDINTERFACE")
	period_tok := expect_token(p, .Period)
	iface_decl.range.end = period_tok.range.end
	iface_decl.derived_stmt = iface_decl
	_ = endiface_tok

	return iface_decl
}

parse_report_decl :: proc(p: ^Parser) -> ^ast.Decl {
	report_tok := expect_keyword_token(p, "REPORT")
	name_tok := expect_token(p, .Ident)
	period_tok := expect_token(p, .Period)

	report_decl := ast.new(ast.Report_Decl, report_tok, period_tok)
	report_decl.name = ast.new_ident(name_tok)
	report_decl.derived_stmt = report_decl
	return report_decl
}

parse_include_decl :: proc(p: ^Parser) -> ^ast.Decl {
	include_tok := expect_keyword_token(p, "INCLUDE")
	name_tok := expect_token(p, .Ident)
	period_tok := expect_token(p, .Period)

	include_decl := ast.new(ast.Include_Decl, include_tok, period_tok)
	include_decl.name = ast.new_ident(name_tok)
	include_decl.derived_stmt = include_decl
	return include_decl
}

parse_event_block :: proc(p: ^Parser, keyword: string) -> ^ast.Stmt {
	// Note: For compound keywords like START-OF-SELECTION, the caller (check_compound_keyword)
	// has already advanced past the entire keyword. For simple keywords like INITIALIZATION,
	// we still need to advance.
	start_tok := p.prev_tok // Use prev_tok since compound keyword was already consumed
	event_kind: ast.Event_Kind

	switch keyword {
	case "START-OF-SELECTION":
		// Already consumed by check_compound_keyword
		event_kind = .StartOfSelection
	case "END-OF-SELECTION":
		// Already consumed by check_compound_keyword
		event_kind = .EndOfSelection
	case "INITIALIZATION":
		start_tok = p.curr_tok
		advance_token(p)
		event_kind = .Initialization
	case "TOP-OF-PAGE":
		// Already consumed by check_compound_keyword
		event_kind = .TopOfPage
	case "END-OF-PAGE":
		// Already consumed by check_compound_keyword
		event_kind = .EndOfPage
	case:
		// Unknown event
		start_tok = p.curr_tok
		end_tok := skip_to_new_line(p)
		bad_decl := ast.new(ast.Bad_Decl, start_tok, end_tok)
		return bad_decl
	}

	expect_token(p, .Period)

	event_block := ast.new(ast.Event_Block, start_tok.range)
	event_block.kind = event_kind
	event_block.body = make([dynamic]^ast.Stmt)

	// Parse body until we hit another event or EOF
	for p.curr_tok.kind != .EOF {
		// Check for event keywords that would end this block
		if p.curr_tok.kind == .Ident {
			if len(p.curr_tok.lit) > 0 && len(p.curr_tok.lit) < len(p.keyword_buffer) {
				kw := to_upper(p.keyword_buffer[:], p.curr_tok.lit)
				// Check for simple keywords that end the block
				if kw == "INITIALIZATION" ||
				   kw == "AT" ||
				   kw == "FORM" ||
				   kw == "ENDFORM" ||
				   kw == "CLASS" ||
				   kw == "ENDCLASS" ||
				   kw == "REPORT" ||
				   kw == "INCLUDE" {
					break
				}
				// Check for compound keywords like START-OF-SELECTION
				if kw == "START" || kw == "END" || kw == "TOP" {
					break
				}
			}
		}

		stmt := parse_stmt(p)
		if stmt != nil {
			append(&event_block.body, stmt)
		}
	}

	event_block.range.end = p.prev_tok.range.end
	event_block.derived_stmt = event_block
	return event_block
}

parse_at_event_block :: proc(p: ^Parser) -> ^ast.Stmt {
	start_tok := p.curr_tok
	advance_token(p) // consume AT

	// Check for SELECTION-SCREEN
	if check_class_keyword(p, "SELECTION", "SCREEN") {
		// check_class_keyword already advanced past SELECTION-SCREEN
		expect_token(p, .Period)

		event_block := ast.new(ast.Event_Block, start_tok.range)
		event_block.kind = .AtSelectionScreen
		event_block.body = make([dynamic]^ast.Stmt)

		// Parse body until we hit another event or EOF
		for p.curr_tok.kind != .EOF {
			if p.curr_tok.kind == .Ident {
				if len(p.curr_tok.lit) > 0 && len(p.curr_tok.lit) < len(p.keyword_buffer) {
					kw := to_upper(p.keyword_buffer[:], p.curr_tok.lit)
					if kw == "START" ||
					   kw == "END" ||
					   kw == "INITIALIZATION" ||
					   kw == "AT" ||
					   kw == "TOP" ||
					   kw == "FORM" ||
					   kw == "ENDFORM" ||
					   kw == "CLASS" ||
					   kw == "ENDCLASS" {
						break
					}
				}
			}

			stmt := parse_stmt(p)
			if stmt != nil {
				append(&event_block.body, stmt)
			}
		}

		event_block.range.end = p.prev_tok.range.end
		event_block.derived_stmt = event_block
		return event_block
	}

	// Unknown AT event, skip line
	end_tok := skip_to_new_line(p)
	error(p, lexer.range_between(start_tok, end_tok), "unknown AT event")
	bad_decl := ast.new(ast.Bad_Decl, start_tok, end_tok)
	return bad_decl
}

parse_call_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	call_tok := expect_keyword_token(p, "CALL")

	if check_keyword(p, "SCREEN") {
		advance_token(p)
		screen_no := parse_expr(p)
		period_tok := expect_token(p, .Period)

		call_screen := ast.new(ast.Call_Screen_Stmt, call_tok, period_tok)
		call_screen.screen_no = screen_no
		call_screen.derived_stmt = call_screen
		return call_screen
	}

	// For other CALL types (FUNCTION, METHOD, etc.), treat as expression statement for now
	expr := parse_expr(p)
	period_tok := expect_token(p, .Period)
	expr_stmt := ast.new(ast.Expr_Stmt, call_tok, period_tok)
	expr_stmt.expr = expr
	return expr_stmt
}

parse_module_decl :: proc(p: ^Parser) -> ^ast.Decl {
	module_tok := expect_keyword_token(p, "MODULE")
	ident_tok := expect_token(p, .Ident)

	module_type: ast.Module_Type
	if check_keyword(p, "OUTPUT") {
		advance_token(p)
		module_type = .Output
	} else if check_keyword(p, "INPUT") {
		advance_token(p)
		module_type = .Input
	} else {
		error(p, p.curr_tok.range, "expected OUTPUT or INPUT after module name")
		end_tok := skip_to_new_line(p)
		bad_decl := ast.new(ast.Bad_Decl, module_tok, end_tok)
		return bad_decl
	}

	expect_token(p, .Period)

	module_decl := ast.new(ast.Module_Decl, module_tok.range)
	module_decl.ident = ast.new_ident(ident_tok)
	module_decl.module_type = module_type
	module_decl.body = make([dynamic]^ast.Stmt)

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "ENDMODULE") {
			break
		}

		stmt := parse_stmt(p)
		if stmt != nil {
			append(&module_decl.body, stmt)
		}
	}

	endmodule_tok := expect_keyword_token(p, "ENDMODULE")
	period_tok := expect_token(p, .Period)
	module_decl.range.end = period_tok.range.end
	module_decl.derived_stmt = module_decl
	_ = endmodule_tok

	return module_decl
}

// Syntax: IF condition. body... [ELSEIF condition. body...]* [ELSE. body...] ENDIF.
parse_if_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	if_tok := expect_keyword_token(p, "IF")
	cond := parse_logical_expr(p)
	expect_token(p, .Period)

	if_stmt := ast.new(ast.If_Stmt, if_tok.range)
	if_stmt.cond = cond
	if_stmt.body = make([dynamic]^ast.Stmt)
	if_stmt.elseif_branches = make([dynamic]^ast.Elseif_Branch)
	if_stmt.else_body = make([dynamic]^ast.Stmt)

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "ELSEIF") || check_keyword(p, "ELSE") || check_keyword(p, "ENDIF") {
			break
		}
		stmt := parse_stmt(p)
		if stmt != nil {
			append(&if_stmt.body, stmt)
		}
	}

	for check_keyword(p, "ELSEIF") {
		elseif_tok := expect_keyword_token(p, "ELSEIF")
		elseif_cond := parse_logical_expr(p)
		expect_token(p, .Period)

		elseif_branch := ast.new(ast.Elseif_Branch, elseif_tok.range)
		elseif_branch.cond = elseif_cond
		elseif_branch.body = make([dynamic]^ast.Stmt)

		for p.curr_tok.kind != .EOF {
			if check_keyword(p, "ELSEIF") ||
			   check_keyword(p, "ELSE") ||
			   check_keyword(p, "ENDIF") {
				break
			}
			stmt := parse_stmt(p)
			if stmt != nil {
				append(&elseif_branch.body, stmt)
			}
		}
		elseif_branch.range.end = p.prev_tok.range.end
		append(&if_stmt.elseif_branches, elseif_branch)
	}

	if check_keyword(p, "ELSE") {
		advance_token(p)
		expect_token(p, .Period)

		for p.curr_tok.kind != .EOF {
			if check_keyword(p, "ENDIF") {
				break
			}
			stmt := parse_stmt(p)
			if stmt != nil {
				append(&if_stmt.else_body, stmt)
			}
		}
	}

	endif_tok := expect_keyword_token(p, "ENDIF")
	period_tok := expect_token(p, .Period)
	if_stmt.range.end = period_tok.range.end
	if_stmt.derived_stmt = if_stmt
	_ = endif_tok

	return if_stmt
}

parse_logical_expr :: proc(p: ^Parser) -> ^ast.Expr {
	return parse_or_expr(p)
}

parse_or_expr :: proc(p: ^Parser) -> ^ast.Expr {
	left := parse_and_expr(p)

	for check_keyword(p, "OR") {
		op_tok := advance_token(p)
		right := parse_and_expr(p)

		binary := ast.new(ast.Binary_Expr, lexer.TextRange{left.range.start, right.range.end})
		binary.left = left
		binary.op = op_tok
		binary.right = right
		binary.derived_expr = binary
		left = binary
	}

	return left
}

parse_and_expr :: proc(p: ^Parser) -> ^ast.Expr {
	left := parse_not_expr(p)

	for check_keyword(p, "AND") {
		op_tok := advance_token(p)
		right := parse_not_expr(p)

		binary := ast.new(ast.Binary_Expr, lexer.TextRange{left.range.start, right.range.end})
		binary.left = left
		binary.op = op_tok
		binary.right = right
		binary.derived_expr = binary
		left = binary
	}

	return left
}

parse_not_expr :: proc(p: ^Parser) -> ^ast.Expr {
	if check_keyword(p, "NOT") {
		op_tok := advance_token(p)
		expr := parse_not_expr(p)

		unary := ast.new(ast.Unary_Expr, lexer.TextRange{op_tok.range.start, expr.range.end})
		unary.op = op_tok
		unary.expr = expr
		unary.derived_expr = unary
		return unary
	}

	return parse_comparison_expr(p)
}

parse_comparison_expr :: proc(p: ^Parser) -> ^ast.Expr {
	left := parse_expr(p)

	if check_keyword(p, "IS") {
		return parse_is_predicate(p, left)
	}

	if is_comparison_op(p) {
		op_tok := advance_token(p)
		right := parse_expr(p)

		binary := ast.new(ast.Binary_Expr, lexer.TextRange{left.range.start, right.range.end})
		binary.left = left
		binary.op = op_tok
		binary.right = right
		binary.derived_expr = binary
		return binary
	}

	return left
}

is_comparison_op :: proc(p: ^Parser) -> bool {
	#partial switch p.curr_tok.kind {
	case .Lt, .Gt, .Le, .Ge, .Ne, .Eq:
		return true
	}
	if check_keyword(p, "EQ") ||
	   check_keyword(p, "NE") ||
	   check_keyword(p, "LT") ||
	   check_keyword(p, "LE") ||
	   check_keyword(p, "GT") ||
	   check_keyword(p, "GE") ||
	   check_keyword(p, "CO") ||
	   check_keyword(p, "CN") ||
	   check_keyword(p, "CA") ||
	   check_keyword(p, "NA") ||
	   check_keyword(p, "CS") ||
	   check_keyword(p, "NS") ||
	   check_keyword(p, "CP") ||
	   check_keyword(p, "NP") ||
	   check_keyword(p, "BETWEEN") {
		return true
	}
	return false
}

parse_is_predicate :: proc(p: ^Parser, expr: ^ast.Expr) -> ^ast.Expr {
	is_tok := expect_keyword_token(p, "IS")

	is_negated := false
	if check_keyword(p, "NOT") {
		advance_token(p)
		is_negated = true
	}

	predicate_kind: ast.Predicate_Kind
	if check_keyword(p, "INITIAL") {
		advance_token(p)
		predicate_kind = .Initial
	} else if check_keyword(p, "SUPPLIED") {
		advance_token(p)
		predicate_kind = .Supplied
	} else if check_keyword(p, "BOUND") {
		advance_token(p)
		predicate_kind = .Bound
	} else if check_keyword(p, "ASSIGNED") {
		advance_token(p)
		predicate_kind = .Assigned
	} else if check_keyword(p, "REQUESTED") {
		advance_token(p)
		predicate_kind = .Requested
	} else if check_keyword(p, "INSTANCE") {
		advance_token(p)
		expect_keyword_token(p, "OF")
		predicate_kind = .Instance_Of
		// TODO: parse the type expression after INSTANCE OF
	} else {
		error(p, p.curr_tok.range, "expected predicate after IS")
		return expr
	}

	pred_expr := ast.new(
		ast.Predicate_Expr,
		lexer.TextRange{expr.range.start, p.prev_tok.range.end},
	)
	pred_expr.expr = expr
	pred_expr.predicate = predicate_kind
	pred_expr.is_negated = is_negated
	pred_expr.derived_expr = pred_expr
	return pred_expr
}

parse_modify_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	modify_tok := advance_token(p)
	if check_keyword(p, "SCREEN") {
		advance_token(p)
		expect_token(p, .Period)
		stmt := ast.new(ast.Modify_Screen_Stmt, modify_tok, p.curr_tok)
		return stmt
	} else {
		error(p, p.curr_tok.range, "expected SCREEN after MODIFY")
		return nil
	}
}

parse_leave_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	modify_tok := advance_token(p)
	if check_keyword(p, "PROGRAM") {
		advance_token(p)
		expect_token(p, .Period)
		stmt := ast.new(ast.Leave_Program_Stmt, modify_tok, p.curr_tok)
		return stmt
	} else {
		error(p, p.curr_tok.range, "expected PROGRAM after LEAVE")
		return nil
	}
}

parse_set_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	set_tok := advance_token(p)
	kind: ast.Set_Kind
	if check_class_keyword(p, "PF", "STATUS") {
		kind = .Pf_Status
	} else if check_keyword(p, "TITLEBAR") {
		kind = .Titlebar
		advance_token(p)
	} else if check_keyword(p, "SCREEN") {
		kind = .Screen
		advance_token(p)
	} else {
		if check_keyword(p, "CURSOR") {
			advance_token(p)
			if check_keyword(p, "FIELD") {
				advance_token(p)
				kind = .Cursor_Field
			} else {
				error(p, p.curr_tok.range, "expected FIELD after SET CURSOR")
			}
		} else {
			// FIXME retrack first?
			// return parse_expr_or_assign_stmt(p)
			error(p, p.curr_tok.range, "expected CURSOR after SET")
		}
	}
	expr := parse_expr(p)
	end_tok := p.curr_tok
	expect_token(p, .Period)
	stmt := ast.new(ast.Set_Stmt, set_tok, end_tok)
	stmt.expr = expr
	stmt.kind = kind
	return stmt
}