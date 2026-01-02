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

	// Check for optional LENGTH clause
	length_expr: ^ast.Expr = nil
	if check_keyword(p, "LENGTH") {
		advance_token(p) // consume LENGTH
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
		// Check for BEGIN OF (start of structured type)
		if check_keyword(p, "BEGIN") {
			struct_decl := parse_types_struct_decl(p)
			if struct_decl != nil {
				// Convert chain to include the struct - we need to return it differently
				// For now, if we have a BEGIN OF as the first element, return just the struct
				if len(chain_decl.decls) == 0 {
					if allow_token(p, .Comma) {
						// There's more after the struct, continue as mixed chain
						// For simplicity, we'll wrap the struct in the chain's range and return
						// Actually, we need a different approach - return the struct directly
						// and handle comma-separated structs
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

		// Check for optional LENGTH clause
		length_expr: ^ast.Expr = nil
		if check_keyword(p, "LENGTH") {
			advance_token(p) // consume LENGTH
			length_expr = parse_expr(p)
		}

		decl := ast.new(ast.Types_Decl, ident_tok, p.prev_tok)
		decl.ident = ast.new_ident(ident_tok)
		decl.typed = type_expr
		decl.length = length_expr
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

// Parses a structured type: BEGIN OF name, ... components ..., END OF name
parse_types_struct_decl :: proc(p: ^Parser) -> ^ast.Types_Struct_Decl {
	begin_tok := expect_keyword_token(p, "BEGIN")
	expect_keyword_token(p, "OF")
	ident_tok := expect_token(p, .Ident)

	struct_decl := ast.new(ast.Types_Struct_Decl, begin_tok.range)
	struct_decl.ident = ast.new_ident(ident_tok)
	struct_decl.components = make([dynamic]^ast.Stmt)

	// Expect comma after BEGIN OF name
	expect_token(p, .Comma)

	// Parse components until END OF
	for p.curr_tok.kind != .EOF {
		// Check for END OF
		if check_keyword(p, "END") {
			break
		}

		// Check for nested BEGIN OF (nested structure)
		if check_keyword(p, "BEGIN") {
			nested_struct := parse_types_struct_decl(p)
			if nested_struct != nil {
				append(&struct_decl.components, &nested_struct.node)
			}
			// Expect comma after nested struct
			if !allow_token(p, .Comma) {
				break // END OF should follow
			}
			continue
		}

		// Parse regular field: name TYPE type [LENGTH n]
		field_ident_tok := expect_token(p, .Ident)
		expect_keyword_token(p, "TYPE")
		type_expr := parse_expr(p)

		// Check for optional LENGTH clause
		length_expr: ^ast.Expr = nil
		if check_keyword(p, "LENGTH") {
			advance_token(p) // consume LENGTH
			length_expr = parse_expr(p)
		}

		field_decl := ast.new(ast.Types_Decl, field_ident_tok, p.prev_tok)
		field_decl.ident = ast.new_ident(field_ident_tok)
		field_decl.typed = type_expr
		field_decl.length = length_expr
		append(&struct_decl.components, &field_decl.node)

		if !allow_token(p, .Comma) {
			break // END OF should follow
		}
	}

	// Expect END OF name
	expect_keyword_token(p, "END")
	expect_keyword_token(p, "OF")
	end_ident_tok := expect_token(p, .Ident)
	struct_decl.range.end = end_ident_tok.range.end

	// Verify the END OF name matches BEGIN OF name
	if struct_decl.ident.name != end_ident_tok.lit {
		error(p, end_ident_tok.range, "END OF '%s' does not match BEGIN OF '%s'", end_ident_tok.lit, struct_decl.ident.name)
	}

	return struct_decl
}

// Helper to check if current token is a specific keyword (without consuming)
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

// Helper to check for compound keywords like CLASS-DATA, CLASS-METHODS
// Returns true if current token is `first` followed by `-` followed by `second`
// Also consumes the three tokens (first, minus, second) if matched
check_class_keyword :: proc(p: ^Parser, first: string, second: string) -> bool {
	if !check_keyword(p, first) {
		return false
	}
	
	// Save current position to rollback if no match
	saved_prev := p.prev_tok
	saved_curr := p.curr_tok
	saved_pos := p.l.pos
	saved_read_pos := p.l.read_pos
	saved_ch := p.l.ch
	
	// Check if next token is minus (with no space)
	advance_token(p)
	if p.curr_tok.kind != .Minus || lexer.have_space_between(saved_curr, p.curr_tok) {
		// Rollback
		p.prev_tok = saved_prev
		p.curr_tok = saved_curr
		p.l.pos = saved_pos
		p.l.read_pos = saved_read_pos
		p.l.ch = saved_ch
		return false
	}
	
	// Get the token after minus
	advance_token(p)
	if !check_keyword(p, second) || lexer.have_space_between(p.prev_tok, p.curr_tok) {
		// Rollback - this is tricky, just return false for now
		p.prev_tok = saved_prev
		p.curr_tok = saved_curr
		p.l.pos = saved_pos
		p.l.read_pos = saved_read_pos
		p.l.ch = saved_ch
		return false
	}
	
	// Consume the second keyword as well and return true
	advance_token(p)
	return true
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
		case .Minus, .FatArrow, .Tilde:
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

// ============================================================================
// CLASS and INTERFACE parsing
// ============================================================================

// Parses a CLASS declaration (either DEFINITION or IMPLEMENTATION)
parse_class_decl :: proc(p: ^Parser) -> ^ast.Decl {
	class_tok := expect_token(p, .Ident) // CLASS keyword
	ident_tok := expect_token(p, .Ident) // class name
	
	// Check if this is DEFINITION or IMPLEMENTATION
	if check_keyword(p, "DEFINITION") {
		return parse_class_def_decl(p, class_tok, ident_tok)
	} else if check_keyword(p, "IMPLEMENTATION") {
		return parse_class_impl_decl(p, class_tok, ident_tok)
	}
	
	// Error: expected DEFINITION or IMPLEMENTATION
	error(p, p.curr_tok.range, "expected DEFINITION or IMPLEMENTATION after class name")
	end_tok := skip_to_new_line(p)
	bad_decl := ast.new(ast.Bad_Decl, class_tok, end_tok)
	return bad_decl
}

// Parses CLASS ... DEFINITION
parse_class_def_decl :: proc(p: ^Parser, class_tok: lexer.Token, ident_tok: lexer.Token) -> ^ast.Decl {
	expect_keyword_token(p, "DEFINITION") // consume DEFINITION
	
	class_decl := ast.new(ast.Class_Def_Decl, class_tok.range)
	class_decl.ident = ast.new_ident(ident_tok)
	class_decl.sections = make([dynamic]^ast.Class_Section)
	
	// Parse optional modifiers: ABSTRACT, FINAL, INHERITING FROM
	for p.curr_tok.kind == .Ident && p.curr_tok.kind != .Period {
		if check_keyword(p, "ABSTRACT") {
			advance_token(p)
			class_decl.is_abstract = true
		} else if check_keyword(p, "FINAL") {
			advance_token(p)
			class_decl.is_final = true
		} else if check_keyword(p, "INHERITING") {
			advance_token(p) // consume INHERITING
			expect_keyword_token(p, "FROM")
			class_decl.inheriting_from = parse_expr(p)
		} else {
			break
		}
	}
	
	// Expect period after class header
	expect_token(p, .Period)
	
	// Parse sections until ENDCLASS
	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "ENDCLASS") {
			break
		}
		
		// Parse section (PUBLIC/PROTECTED/PRIVATE SECTION)
		if section := parse_class_section(p); section != nil {
			append(&class_decl.sections, section)
		} else {
			// Skip unknown statement
			skip_to_new_line(p)
		}
	}
	
	// Expect ENDCLASS
	endclass_tok := expect_keyword_token(p, "ENDCLASS")
	period_tok := expect_token(p, .Period)
	class_decl.range.end = period_tok.range.end
	_ = endclass_tok
	
	return class_decl
}

// Parses CLASS ... IMPLEMENTATION
parse_class_impl_decl :: proc(p: ^Parser, class_tok: lexer.Token, ident_tok: lexer.Token) -> ^ast.Decl {
	expect_keyword_token(p, "IMPLEMENTATION") // consume IMPLEMENTATION
	expect_token(p, .Period)
	
	class_impl := ast.new(ast.Class_Impl_Decl, class_tok.range)
	class_impl.ident = ast.new_ident(ident_tok)
	class_impl.methods = make([dynamic]^ast.Stmt)
	
	// Parse method implementations until ENDCLASS
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
			// Skip unknown statement
			skip_to_new_line(p)
		}
	}
	
	// Expect ENDCLASS
	endclass_tok := expect_keyword_token(p, "ENDCLASS")
	period_tok := expect_token(p, .Period)
	class_impl.range.end = period_tok.range.end
	_ = endclass_tok
	
	return class_impl
}

// Parses a class section: PUBLIC/PROTECTED/PRIVATE SECTION
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
	
	// Parse section contents until next SECTION or ENDCLASS
	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "PUBLIC") || check_keyword(p, "PROTECTED") || 
		   check_keyword(p, "PRIVATE") || check_keyword(p, "ENDCLASS") {
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
			// Skip unknown content
			skip_to_new_line(p)
		}
	}
	
	return section
}

// Parses DATA or CLASS-DATA declaration in a class
// For regular DATA, the token is not yet consumed
// For CLASS-DATA, check_class_keyword already consumed all tokens
parse_class_data_decl :: proc(p: ^Parser, is_class: bool) -> ^ast.Stmt {
	data_tok: lexer.Token
	if is_class {
		// check_class_keyword already consumed CLASS-DATA, p.prev_tok is DATA
		data_tok = p.prev_tok
	} else {
		data_tok = advance_token(p) // consume DATA
	}
	
	if allow_token(p, .Colon) {
		// Chain declaration: DATA: attr1 TYPE t1, attr2 TYPE t2.
		return parse_class_data_chain_decl(p, data_tok, is_class)
	}
	
	// Single declaration: DATA attr TYPE type.
	return parse_class_data_single_decl(p, data_tok, is_class)
}

parse_class_data_single_decl :: proc(p: ^Parser, data_tok: lexer.Token, is_class: bool) -> ^ast.Stmt {
	ident_tok := expect_token(p, .Ident)
	expect_keyword_token(p, "TYPE")
	type_expr := parse_expr(p)
	
	// Check for optional READ-ONLY
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

parse_class_data_chain_decl :: proc(p: ^Parser, data_tok: lexer.Token, is_class: bool) -> ^ast.Stmt {
	// For simplicity, parse each item and return a chain (reuse Data_Typed_Chain_Decl)
	// Actually we should create Attr_Decl for each
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

// Parses METHODS or CLASS-METHODS declaration
// For regular METHODS, the token is not yet consumed
// For CLASS-METHODS, check_class_keyword already consumed all tokens
parse_method_decl :: proc(p: ^Parser, is_class: bool) -> ^ast.Stmt {
	method_tok: lexer.Token
	if is_class {
		// check_class_keyword already consumed CLASS-METHODS, p.prev_tok is METHODS
		method_tok = p.prev_tok
	} else {
		method_tok = advance_token(p) // consume METHODS
	}
	
	if allow_token(p, .Colon) {
		// Chain declaration - parse each method
		return parse_method_chain_decl(p, method_tok, is_class)
	}
	
	return parse_method_single_decl(p, method_tok, is_class)
}

parse_method_single_decl :: proc(p: ^Parser, method_tok: lexer.Token, is_class: bool) -> ^ast.Stmt {
	ident_tok := expect_token(p, .Ident)
	
	method_decl := ast.new(ast.Method_Decl, method_tok.range)
	method_decl.ident = ast.new_ident(ident_tok)
	method_decl.is_class = is_class
	method_decl.params = make([dynamic]^ast.Method_Param)
	method_decl.raising = make([dynamic]^ast.Expr)
	
	// Parse optional modifiers and parameters
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
	// Parse first method and return it (simplified - in practice METHODS: is less common)
	return parse_method_single_decl(p, method_tok, is_class)
}

// Parses method parameters (IMPORTING/EXPORTING/CHANGING/RETURNING)
parse_method_params :: proc(
	p: ^Parser,
	params: ^[dynamic]^ast.Method_Param,
	kind: ast.Method_Param_Kind,
) {
	for p.curr_tok.kind == .Ident && p.curr_tok.kind != .Period {
		// Check for next section keyword
		if check_keyword(p, "IMPORTING") || check_keyword(p, "EXPORTING") ||
		   check_keyword(p, "CHANGING") || check_keyword(p, "RETURNING") ||
		   check_keyword(p, "RAISING") || check_keyword(p, "ABSTRACT") ||
		   check_keyword(p, "FINAL") || check_keyword(p, "REDEFINITION") {
			break
		}
		
		// For RETURNING, expect VALUE(name)
		if kind == .Returning && check_keyword(p, "VALUE") {
			advance_token(p) // consume VALUE
			expect_token(p, .LParen)
			ident_tok := expect_token(p, .Ident)
			expect_token(p, .RParen)
			
			param := ast.new(ast.Method_Param, ident_tok.range)
			param.kind = kind
			param.ident = ast.new_ident(ident_tok)
			
			// TYPE clause
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
		
		// Check for TYPE clause
		if check_keyword(p, "TYPE") {
			advance_token(p)
			param.typed = parse_expr(p)
		}
		
		// Check for OPTIONAL
		if check_keyword(p, "OPTIONAL") {
			advance_token(p)
			param.optional = true
		}
		
		// Check for DEFAULT
		if check_keyword(p, "DEFAULT") {
			advance_token(p)
			param.default = parse_expr(p)
		}
		
		append(params, param)
	}
}

// Parses RAISING clause
parse_raising_clause :: proc(p: ^Parser, raising: ^[dynamic]^ast.Expr) {
	for p.curr_tok.kind == .Ident && p.curr_tok.kind != .Period {
		if check_keyword(p, "IMPORTING") || check_keyword(p, "EXPORTING") ||
		   check_keyword(p, "CHANGING") || check_keyword(p, "RETURNING") {
			break
		}
		
		exception_name := parse_expr(p)
		append(raising, exception_name)
	}
}

// Parses INTERFACES declaration (implementing interfaces in a class)
parse_interfaces_decl :: proc(p: ^Parser) -> ^ast.Stmt {
	ifaces_tok := expect_keyword_token(p, "INTERFACES")
	
	ifaces_decl := ast.new(ast.Interfaces_Decl, ifaces_tok.range)
	ifaces_decl.names = make([dynamic]^ast.Ident)
	
	// Parse interface names
	for p.curr_tok.kind == .Ident && p.curr_tok.kind != .Period {
		ident_tok := advance_token(p)
		append(&ifaces_decl.names, ast.new_ident(ident_tok))
	}
	
	period_tok := expect_token(p, .Period)
	ifaces_decl.range.end = period_tok.range.end
	ifaces_decl.derived_stmt = ifaces_decl
	return ifaces_decl
}

// Parses METHOD implementation (inside CLASS IMPLEMENTATION)
parse_method_impl :: proc(p: ^Parser) -> ^ast.Stmt {
	method_tok := expect_keyword_token(p, "METHOD")
	
	// Method name can be simple or interface~method
	name_expr := parse_expr(p)
	
	expect_token(p, .Period)
	
	method_impl := ast.new(ast.Method_Impl, method_tok.range)
	method_impl.ident = name_expr
	method_impl.body = make([dynamic]^ast.Stmt)
	
	// Parse body until ENDMETHOD
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

// Parses INTERFACE declaration
parse_interface_decl :: proc(p: ^Parser) -> ^ast.Decl {
	iface_tok := expect_token(p, .Ident) // INTERFACE keyword
	ident_tok := expect_token(p, .Ident) // interface name
	
	expect_token(p, .Period)
	
	iface_decl := ast.new(ast.Interface_Decl, iface_tok.range)
	iface_decl.ident = ast.new_ident(ident_tok)
	iface_decl.methods = make([dynamic]^ast.Stmt)
	iface_decl.types = make([dynamic]^ast.Stmt)
	iface_decl.data = make([dynamic]^ast.Stmt)
	
	// Parse contents until ENDINTERFACE
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
			// Skip unknown content
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
