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
		case "CONSTANTS":
			return parse_constants_decl(p)
		case "FORM":
			return parse_form_decl(p)
		case "CLASS":
			return parse_class_decl(p)
		case "INTERFACE":
			return parse_interface_decl(p)
		case "REPORT":
			return parse_report_decl(p)
		case "INCLUDE":
			return parse_include_decl(p)
		case "FIELD":
			if check_hyphenated_keyword(p, "FIELD", "SYMBOLS") {
				return parse_field_symbol_decl(p)
			}
		case "CONTROLS":
			return parse_controls_decl(p)
		case "MODULE":
			return parse_module_decl(p)
		case "METHOD":
			return parse_method_impl(p)
		case "INITIALIZATION":
			return parse_event_block(p, keyword)
		case "AT":
			return parse_at_event_block(p)
		case "CALL":
			return parse_call_stmt(p)
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
		case "CASE":
			return parse_case_stmt(p)
		case "WHILE":
			return parse_while_stmt(p)
		case "LOOP":
			return parse_loop_stmt(p)
		case "CLEAR":
			return parse_clear_stmt(p)
		case "MESSAGE":
			return parse_message_stmt(p)
		case "DELETE":
			return parse_delete_stmt(p)
		case "INSERT":
			return parse_insert_stmt(p)
		case "SORT":
			return parse_sort_stmt(p)
		case "AUTHORITY":
			if check_hyphenated_keyword(p, "AUTHORITY", "CHECK") {
				return parse_authority_check_stmt(p)
			}
		case "APPEND":
			return parse_append_stmt(p)
		case "READ":
			if check_keyword_ahead(p, "TABLE") {
				return parse_read_table_stmt(p)
			}
		case "CONDENSE":
			return parse_condense_stmt(p)
		case "SELECT":
			return parse_select_stmt(p)
		}
	}

	// Check for field symbol identifiers (starting with <)
	if p.curr_tok.kind == .Lt {
		return parse_field_symbol_assign_stmt(p)
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

// parse_type_expr parses a type expression, handling complex types like:
// - STANDARD TABLE OF / SORTED TABLE OF / HASHED TABLE OF / TABLE OF
// - REF TO
// - LINE OF
// - Simple types (identifiers, selectors)
// - WITH KEY / WITH UNIQUE KEY / WITH NON-UNIQUE KEY clauses
parse_type_expr :: proc(p: ^Parser) -> ^ast.Expr {
	// Check for REF TO
	if check_keyword(p, "REF") {
		return parse_ref_type(p)
	}

	// Check for LINE OF
	if check_keyword(p, "LINE") {
		return parse_line_type(p)
	}

	// Check for table types: STANDARD TABLE OF, SORTED TABLE OF, HASHED TABLE OF, TABLE OF
	// Also handle UNIQUE prefix for hashed tables
	if check_keyword(p, "STANDARD") ||
	   check_keyword(p, "SORTED") ||
	   check_keyword(p, "HASHED") ||
	   check_keyword(p, "TABLE") ||
	   check_keyword(p, "UNIQUE") {
		return parse_table_type(p)
	}

	// Otherwise parse as a simple type expression (identifier or selector)
	return parse_simple_type_expr(p)
}

// parse_ref_type parses: REF TO type
parse_ref_type :: proc(p: ^Parser) -> ^ast.Expr {
	ref_tok := expect_keyword_token(p, "REF")
	expect_keyword_token(p, "TO")

	target := parse_simple_type_expr(p)

	ref_type := ast.new(ast.Ref_Type, lexer.TextRange{ref_tok.range.start, p.prev_tok.range.end})
	ref_type.target = target
	ref_type.derived_expr = ref_type
	return ref_type
}

// parse_line_type parses: LINE OF table_var
parse_line_type :: proc(p: ^Parser) -> ^ast.Expr {
	line_tok := expect_keyword_token(p, "LINE")
	expect_keyword_token(p, "OF")

	table_ref := parse_simple_type_expr(p)

	line_type := ast.new(
		ast.Line_Type,
		lexer.TextRange{line_tok.range.start, p.prev_tok.range.end},
	)
	line_type.table = table_ref
	line_type.derived_expr = line_type
	return line_type
}

// parse_table_type parses table types with optional key specifications:
// - STANDARD TABLE OF type [WITH key_spec]
// - SORTED TABLE OF type [WITH key_spec]
// - HASHED TABLE OF type WITH key_spec
// - TABLE OF type [WITH key_spec]
// - UNIQUE [HASHED/SORTED] TABLE OF type WITH key_spec
parse_table_type :: proc(p: ^Parser) -> ^ast.Expr {
	start_tok := p.curr_tok
	table_kind := ast.Table_Kind.Any
	is_unique := false

	// Check for UNIQUE prefix
	if check_keyword(p, "UNIQUE") {
		advance_token(p)
		is_unique = true
	}

	// Determine table kind
	if check_keyword(p, "STANDARD") {
		advance_token(p)
		table_kind = .Standard
	} else if check_keyword(p, "SORTED") {
		advance_token(p)
		table_kind = .Sorted
	} else if check_keyword(p, "HASHED") {
		advance_token(p)
		table_kind = .Hashed
	}

	// Expect TABLE keyword
	expect_keyword_token(p, "TABLE")
	expect_keyword_token(p, "OF")

	// Parse element type
	elem := parse_simple_type_expr(p)

	// Create table type node
	table_type := ast.new(
		ast.Table_Type,
		lexer.TextRange{start_tok.range.start, p.prev_tok.range.end},
	)
	table_type.kind = table_kind
	table_type.elem = elem
	table_type.derived_expr = table_type

	// Parse optional WITH KEY clause(s)
	for check_keyword(p, "WITH") {
		key := parse_table_key(p, is_unique)
		if key != nil {
			if table_type.primary_key == nil {
				table_type.primary_key = key
			} else {
				if table_type.secondary_keys == nil {
					table_type.secondary_keys = make([dynamic]^ast.Table_Key)
				}
				append(&table_type.secondary_keys, key)
			}
		}
		table_type.range.end = p.prev_tok.range.end
		// Reset is_unique for secondary keys - they specify their own uniqueness
		is_unique = false
	}

	return table_type
}

// parse_table_key parses: WITH [UNIQUE|NON-UNIQUE] [SORTED|HASHED] KEY key_spec
parse_table_key :: proc(p: ^Parser, default_unique: bool) -> ^ast.Table_Key {
	if !check_keyword(p, "WITH") {
		return nil
	}
	advance_token(p) // consume WITH

	key := new(ast.Table_Key)
	key.is_unique = default_unique
	key.components = make([dynamic]^ast.Ident)

	// Check for UNIQUE / NON-UNIQUE modifier
	if check_keyword(p, "UNIQUE") {
		advance_token(p)
		key.is_unique = true
	} else if check_hyphenated_keyword(p, "NON", "UNIQUE") {
		// NON-UNIQUE was consumed by check_compound_keyword
		key.is_unique = false
	}

	// Check for SORTED / HASHED for secondary keys
	if check_keyword(p, "SORTED") {
		advance_token(p)
	} else if check_keyword(p, "HASHED") {
		advance_token(p)
	}

	// Expect KEY keyword
	if check_keyword(p, "KEY") {
		advance_token(p)
	} else if check_keyword(p, "DEFAULT") {
		advance_token(p)
		if check_keyword(p, "KEY") {
			advance_token(p)
		}
		key.is_default = true
		return key
	} else {
		// No KEY keyword, might be just WITH for other purposes
		free(key)
		return nil
	}

	// Check for DEFAULT KEY
	if check_keyword(p, "DEFAULT") {
		advance_token(p)
		if check_keyword(p, "KEY") {
			advance_token(p)
		}
		key.is_default = true
		return key
	}

	// Check for named key (identifier before COMPONENTS or for simple key names)
	// Parse key components - can be single identifier or list separated by commas
	// Also check for COMPONENTS keyword for secondary keys
	if check_keyword(p, "COMPONENTS") {
		advance_token(p)
	}

	// Parse key field names
	for p.curr_tok.kind == .Ident {
		// Check if it's a keyword that ends the key specification
		if check_keyword(p, "WITH") ||
		   check_keyword(p, "VALUE") ||
		   check_keyword(p, "LENGTH") ||
		   check_keyword(p, "READ") {
			break
		}

		field_tok := advance_token(p)
		field_ident := ast.new_ident(field_tok)
		append(&key.components, field_ident)

		// Check for comma separator
		if !allow_token(p, .Comma) {
			break
		}
	}

	return key
}

// Helper to check two-part compound keywords like NON-UNIQUE
check_hyphenated_keyword :: proc(p: ^Parser, first: string, second: string) -> bool {
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
	return true
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
	return parse_concat_expr(p)
}

// parse_concat_expr handles string concatenation with & (lowest precedence in expressions)
parse_concat_expr :: proc(p: ^Parser) -> ^ast.Expr {
	start_tok := p.curr_tok
	expr := parse_additive_expr(p)
	if expr == nil {
		return ast.new(ast.Bad_Expr, start_tok, p.curr_tok)
	}

	// Handle string concatenation with &
	for p.curr_tok.kind == .Ampersand {
		op := advance_token(p)
		right := parse_additive_expr(p)
		if right == nil {
			break
		}

		binary := ast.new(ast.Binary_Expr, lexer.TextRange{expr.range.start, right.range.end})
		binary.left = expr
		binary.op = op
		binary.right = right
		binary.derived_expr = binary
		expr = binary
	}

	return expr
}

// parse_additive_expr handles + and - operators
parse_additive_expr :: proc(p: ^Parser) -> ^ast.Expr {
	expr := parse_multiplicative_expr(p)
	if expr == nil {
		return nil
	}

	for {
		// Check for + or - with space around them (to distinguish from selectors)
		is_additive := false
		if p.curr_tok.kind == .Plus {
			is_additive = true
		} else if p.curr_tok.kind == .Minus && lexer.have_space_between(p.prev_tok, p.curr_tok) {
			// Minus with leading space is additive, without space it could be a selector
			is_additive = true
		}

		if !is_additive {
			break
		}

		op := advance_token(p)
		right := parse_multiplicative_expr(p)
		if right == nil {
			break
		}

		binary := ast.new(ast.Binary_Expr, lexer.TextRange{expr.range.start, right.range.end})
		binary.left = expr
		binary.op = op
		binary.right = right
		binary.derived_expr = binary
		expr = binary
	}

	return expr
}

// parse_multiplicative_expr handles *, /, MOD, DIV operators
parse_multiplicative_expr :: proc(p: ^Parser) -> ^ast.Expr {
	expr := parse_unary_expr(p)
	if expr == nil {
		return nil
	}

	for {
		is_mult := false
		if p.curr_tok.kind == .Star || p.curr_tok.kind == .Slash {
			is_mult = true
		} else if check_keyword(p, "MOD") || check_keyword(p, "DIV") {
			is_mult = true
		}

		if !is_mult {
			break
		}

		op := advance_token(p)
		right := parse_unary_expr(p)
		if right == nil {
			break
		}

		binary := ast.new(ast.Binary_Expr, lexer.TextRange{expr.range.start, right.range.end})
		binary.left = expr
		binary.op = op
		binary.right = right
		binary.derived_expr = binary
		expr = binary
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
						error(
							p,
							p.curr_tok.range,
							"unexpected token '%s' in function call",
							p.curr_tok.lit,
						)
						advance_token(p)
						// Check again after advancing - might hit terminator
						if p.curr_tok.kind == .RParen ||
						   p.curr_tok.kind == .EOF ||
						   p.curr_tok.kind == .Period {
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

			named_arg := ast.new(
				ast.Named_Arg,
				lexer.TextRange{start_tok.range.start, value.range.end},
			)
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
		if check_keyword(p, "COND") ||
		   check_keyword(p, "SWITCH") ||
		   check_keyword(p, "VALUE") ||
		   check_keyword(p, "REF") ||
		   check_keyword(p, "CAST") ||
		   check_keyword(p, "EXACT") ||
		   check_keyword(p, "CORRESPONDING") ||
		   check_keyword(p, "REDUCE") ||
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
	case .Pipe:
		return parse_string_template(p)
	case .LParen:
		// Parenthesized expression for grouping (e.g., (a + b) * c)
		// Only parse as paren expr if there's a leading space (not a call)
		if lexer.have_space_between(p.prev_tok, p.curr_tok) {
			return parse_paren_expr(p)
		}
		return nil
	case .Lt:
		// Field symbol reference <fs> in an expression context
		return parse_field_symbol_ref(p)
	case .Hash:
		// Standalone # is not valid, but consume it to avoid infinite loops
		hash_tok := advance_token(p)
		error(
			p,
			hash_tok.range,
			"unexpected '#' token - type inference marker must follow a constructor keyword",
		)
		bad_expr := ast.new(ast.Bad_Expr, hash_tok.range)
		return bad_expr
	}
	return nil
}

// parse_paren_expr parses a parenthesized expression ( expr )
parse_paren_expr :: proc(p: ^Parser) -> ^ast.Expr {
	lparen_tok := expect_token(p, .LParen)
	inner := parse_expr(p)
	rparen_tok := expect_token(p, .RParen)

	paren_expr := ast.new(
		ast.Paren_Expr,
		lexer.TextRange{lparen_tok.range.start, rparen_tok.range.end},
	)
	paren_expr.expr = inner
	paren_expr.derived_expr = paren_expr
	return paren_expr
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
		if ur < utf8.RUNE_SELF {
			buffer[length] = byte(ur)
			length += 1
		} else {
			buf, w := utf8.encode_rune(ur)
			for i := 0; i < w; i += 1 {
				buffer[length] = buf[i]
				length += 1
			}
		}
	}
	return string(buffer[:length])
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

	if check_keyword(p, "FUNCTION") {
		return parse_call_function_stmt(p, call_tok)
	}

	// For other CALL types (METHOD, etc.), treat as expression statement for now
	expr := parse_expr(p)
	period_tok := expect_token(p, .Period)
	expr_stmt := ast.new(ast.Expr_Stmt, call_tok, period_tok)
	expr_stmt.expr = expr
	return expr_stmt
}

// parse_call_function_stmt parses a CALL FUNCTION statement
// Syntax: CALL FUNCTION 'func_name' [DESTINATION dest]
//         [EXPORTING param = value ...]
//         [IMPORTING param = value ...]
//         [TABLES param = value ...]
//         [CHANGING param = value ...]
//         [EXCEPTIONS name = value ...].
parse_call_function_stmt :: proc(p: ^Parser, call_tok: lexer.Token) -> ^ast.Stmt {
	expect_keyword_token(p, "FUNCTION")

	// Parse function name (typically a string literal like 'FUNC_NAME')
	func_name := parse_expr(p)

	call_func := ast.new(ast.Call_Function_Stmt, call_tok.range)
	call_func.func_name = func_name
	call_func.exporting = make([dynamic]^ast.Call_Function_Param)
	call_func.importing = make([dynamic]^ast.Call_Function_Param)
	call_func.tables = make([dynamic]^ast.Call_Function_Param)
	call_func.changing = make([dynamic]^ast.Call_Function_Param)
	call_func.exceptions = make([dynamic]^ast.Call_Function_Param)
	call_func.derived_stmt = call_func

	// Parse optional DESTINATION
	if check_keyword(p, "DESTINATION") {
		advance_token(p)
		call_func.destination = parse_expr(p)
	}

	// Parse the parameter sections (can appear in any order, each can appear at most once)
	for p.curr_tok.kind != .Period && p.curr_tok.kind != .EOF {
		if check_keyword(p, "EXPORTING") {
			advance_token(p)
			parse_call_function_params(p, &call_func.exporting, .Exporting)
		} else if check_keyword(p, "IMPORTING") {
			advance_token(p)
			parse_call_function_params(p, &call_func.importing, .Importing)
		} else if check_keyword(p, "TABLES") {
			advance_token(p)
			parse_call_function_params(p, &call_func.tables, .Tables)
		} else if check_keyword(p, "CHANGING") {
			advance_token(p)
			parse_call_function_params(p, &call_func.changing, .Changing)
		} else if check_keyword(p, "EXCEPTIONS") {
			advance_token(p)
			parse_call_function_params(p, &call_func.exceptions, .Exceptions)
		} else {
			// Unknown token, break to avoid infinite loop
			break
		}
	}

	period_tok := expect_token(p, .Period)
	call_func.range.end = period_tok.range.end

	return call_func
}

// parse_call_function_params parses a list of param = value pairs for CALL FUNCTION
parse_call_function_params :: proc(
	p: ^Parser,
	params: ^[dynamic]^ast.Call_Function_Param,
	kind: ast.Call_Function_Param_Kind,
) {
	// Parse parameters until we hit another section keyword or Period
	for p.curr_tok.kind != .Period && p.curr_tok.kind != .EOF {
		// Check if this is a new section keyword
		if check_keyword(p, "EXPORTING") ||
		   check_keyword(p, "IMPORTING") ||
		   check_keyword(p, "TABLES") ||
		   check_keyword(p, "CHANGING") ||
		   check_keyword(p, "EXCEPTIONS") {
			break
		}

		// Parse parameter name
		if p.curr_tok.kind != .Ident {
			break
		}

		param_name_tok := p.curr_tok
		advance_token(p)

		// Parse '='
		if p.curr_tok.kind != .Eq {
			// This might be the OTHERS keyword in EXCEPTIONS
			if kind == .Exceptions &&
			   to_upper(p.keyword_buffer[:], param_name_tok.lit) == "OTHERS" {
				// OTHERS = value
				// But we already consumed the token, so put back logic...
				// Actually, OTHERS is just another exception name, so continue normally
			}
			// If no '=', this might be a keyword; put it back and break
			// Since we already consumed the name, we need to handle this case
			error(
				p,
				p.curr_tok.range,
				"expected '=' after parameter name '%s'",
				param_name_tok.lit,
			)
			break
		}
		advance_token(p) // consume '='

		// Parse parameter value expression
		param_value := parse_call_function_param_value(p)

		// Skip optional pragma like ##ENH_OK
		skip_pragma(p)

		// Create the parameter node
		param := ast.new(ast.Call_Function_Param, param_name_tok.range)
		param.kind = kind
		param.name = ast.new_ident(param_name_tok)
		param.value = param_value
		if param_value != nil {
			param.range.end = param_value.range.end
		}
		param.derived = param

		append(params, param)
	}
}

// parse_call_function_param_value parses a parameter value in a CALL FUNCTION
// This can be a simple expression or a constructor like CONV string(...)
parse_call_function_param_value :: proc(p: ^Parser) -> ^ast.Expr {
	return parse_expr(p)
}

// skip_pragma skips ABAP pragmas like ##ENH_OK
skip_pragma :: proc(p: ^Parser) {
	// Pragmas start with ## - check if current token looks like a pragma
	// The lexer might handle this differently, so we check for the pattern
	if p.curr_tok.kind == .Ident && len(p.curr_tok.lit) >= 2 {
		if p.curr_tok.lit[0] == '#' && p.curr_tok.lit[1] == '#' {
			advance_token(p)
		}
	}
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

// parse_data_inline_expr parses an inline DATA declaration in expression context
// Syntax: DATA(var)
parse_data_inline_expr :: proc(p: ^Parser) -> ^ast.Expr {
	data_tok := expect_keyword_token(p, "DATA")
	expect_token_space_req(p, .LParen, .WithoutLeadingSpace)
	ident_tok := expect_token_space_req(p, .Ident, .WithoutLeadingSpace)
	expect_token_space_req(p, .RParen, .WithoutLeadingSpace)

	// Create a Data_Inline_Decl wrapped as expression
	data_decl := ast.new(ast.Data_Inline_Decl, data_tok, p.prev_tok)
	data_decl.ident = ast.new_ident(ident_tok)
	data_decl.value = nil // Value is determined by the LOOP context
	data_decl.derived_stmt = data_decl
	return data_decl.ident
}

// parse_inline_field_symbol parses an inline FIELD-SYMBOL declaration
// Syntax: FIELD-SYMBOL(<fs>)
parse_inline_field_symbol :: proc(p: ^Parser) -> ^ast.Expr {
	// FIELD-SYMBOL has already been consumed by check_hyphenated_keyword
	expect_token_space_req(p, .LParen, .WithoutLeadingSpace)
	fs_ref := parse_field_symbol_ref(p)
	expect_token_space_req(p, .RParen, .WithoutLeadingSpace)
	return fs_ref
}

// MESSAGE statement parser
// Syntax: MESSAGE { msg | text } [TYPE type] [DISPLAY LIKE display_type] [WITH v1 [v2 [v3 [v4]]]] [INTO data]
// Examples:
//   MESSAGE 'No display authorization.' TYPE 'I' DISPLAY LIKE 'E'.
//   MESSAGE e899(/sttpec/int_msg) WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO lv_dummy_msg.
//   MESSAGE iv_msg TYPE 'I' DISPLAY LIKE 'E'.
//   MESSAGE iv_msg TYPE 'I'.
parse_message_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	message_tok := expect_keyword_token(p, "MESSAGE")

	msg_stmt := ast.new(ast.Message_Stmt, message_tok.range)
	msg_stmt.with_args = make([dynamic]^ast.Expr)

	// Parse the message expression
	// This can be:
	// - A string literal: 'No display authorization.'
	// - An identifier: iv_msg
	// - A message ID: e899(/sttpec/int_msg) or e899(class_name)
	msg_stmt.msg_expr = parse_message_id_or_expr(p)

	// Parse optional clauses
	for p.curr_tok.kind != .EOF && p.curr_tok.kind != .Period {
		if check_keyword(p, "TYPE") {
			advance_token(p)
			msg_stmt.msg_type = parse_expr(p)
		} else if check_keyword(p, "DISPLAY") {
			advance_token(p)
			expect_keyword_token(p, "LIKE")
			msg_stmt.display_like = parse_expr(p)
		} else if check_keyword(p, "WITH") {
			advance_token(p)
			// Parse up to 4 WITH arguments
			for i := 0; i < 4 && p.curr_tok.kind != .EOF && p.curr_tok.kind != .Period; i += 1 {
				// Check if next token is another keyword that would end WITH args
				if check_keyword(p, "INTO") ||
				   check_keyword(p, "TYPE") ||
				   check_keyword(p, "DISPLAY") {
					break
				}
				arg := parse_expr(p)
				if arg != nil {
					append(&msg_stmt.with_args, arg)
				} else {
					break
				}
			}
		} else if check_keyword(p, "INTO") {
			advance_token(p)
			msg_stmt.into_target = parse_expr(p)
		} else {
			// Unknown token, break out
			break
		}
	}

	period_tok := expect_token(p, .Period)
	msg_stmt.range.end = period_tok.range.end
	msg_stmt.derived_stmt = msg_stmt
	return msg_stmt
}

// parse_message_id_or_expr parses a message expression which can be:
// - A string literal: 'text'
// - An identifier: var_name
// - A message ID: x999(class_name) where x is a message type (a,e,i,s,w,x)
parse_message_id_or_expr :: proc(p: ^Parser) -> ^ast.Expr {
	// Check if this is a message ID format: type+number followed by (class)
	// e.g., e899(/sttpec/int_msg) or i001(class)
	if p.curr_tok.kind == .Ident {
		// Save parser state to backtrack if needed
		saved_prev := p.prev_tok
		saved_curr := p.curr_tok
		saved_pos := p.l.pos
		saved_read_pos := p.l.read_pos
		saved_ch := p.l.ch

		ident_tok := advance_token(p)

		// Check if followed by ( without space (message class)
		if p.curr_tok.kind == .LParen && !lexer.have_space_between(ident_tok, p.curr_tok) {
			// This is a message ID with class: e899(class)
			// Parse as a call expression for now
			lparen_tok := advance_token(p) // consume (

			// Parse the class name - could be /namespace/class or simple class
			class_expr := parse_expr(p)

			rparen_tok := expect_token(p, .RParen)

			// Create a call expression to represent message_id(class)
			call_expr := ast.new(
				ast.Call_Expr,
				lexer.TextRange{ident_tok.range.start, rparen_tok.range.end},
			)
			call_expr.expr = ast.new_ident(ident_tok)
			args := make([]^ast.Expr, 1)
			args[0] = class_expr
			call_expr.args = args
			call_expr.derived_expr = call_expr
			return call_expr
		}

		// Not a message ID, restore and parse as regular expression
		p.prev_tok = saved_prev
		p.curr_tok = saved_curr
		p.l.pos = saved_pos
		p.l.read_pos = saved_read_pos
		p.l.ch = saved_ch
	}

	// Parse as a regular expression (string literal, identifier, etc.)
	return parse_expr(p)
}

// parse_field_symbol_ref parses a field symbol reference <fs>
parse_field_symbol_ref :: proc(p: ^Parser) -> ^ast.Expr {
	if p.curr_tok.kind != .Lt {
		error(p, p.curr_tok.range, "expected '<' for field symbol")
		return nil
	}

	start_tok := advance_token(p) // consume <

	// Parse the field symbol name
	if p.curr_tok.kind != .Ident {
		error(p, p.curr_tok.range, "expected field symbol name after '<'")
		return nil
	}

	name_tok := advance_token(p) // consume identifier

	// Expect closing >
	if p.curr_tok.kind != .Gt {
		error(p, p.curr_tok.range, "expected '>' to close field symbol")
		return nil
	}

	end_tok := advance_token(p) // consume >

	// Create identifier with angle brackets in the name
	fs_name := fmt.tprintf("<%s>", name_tok.lit)
	fs_ident := ast.new(ast.Ident, lexer.TextRange{start_tok.range.start, end_tok.range.end})
	fs_ident.name = fs_name
	fs_ident.derived_expr = fs_ident
	return fs_ident
}

// parse_field_symbol_assign_stmt parses assignment statements starting with field symbol
// e.g., <line>-carrid = '...'.
parse_field_symbol_assign_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start_tok := p.curr_tok
	fs_expr := parse_field_symbol_ref(p)
	if fs_expr == nil {
		end_tok := skip_to_new_line(p)
		bad_decl := ast.new(ast.Bad_Decl, start_tok, end_tok)
		return bad_decl
	}

	// Check for selector expression (field access)
	lhs := parse_atom_expr(p, fs_expr)

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

// check_keyword_ahead checks if the next token (after current) is a specific keyword
// without consuming tokens
check_keyword_ahead :: proc(p: ^Parser, expected: string) -> bool {
	if !check_keyword(p, "READ") && p.curr_tok.kind != .Ident {
		return false
	}

	// Save parser state
	saved_prev := p.prev_tok
	saved_curr := p.curr_tok
	saved_pos := p.l.pos
	saved_read_pos := p.l.read_pos
	saved_ch := p.l.ch

	advance_token(p) // Move to next token

	result := check_keyword(p, expected)

	// Restore parser state
	p.prev_tok = saved_prev
	p.curr_tok = saved_curr
	p.l.pos = saved_pos
	p.l.read_pos = saved_read_pos
	p.l.ch = saved_ch

	return result
}

parse_authority_check_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	expect_keyword_token(p, "OBJECT")
	object := parse_expr(p)

	user: ^ast.Expr = nil
	if check_keyword(p, "FOR") {
		advance_token(p)
		expect_keyword_token(p, "USER")
		user = parse_expr(p)
	}

	stmt := ast.new(ast.Authority_Check_Stmt, object.range)
	stmt.ids = make([dynamic]ast.Authority_Check_Id)

	for {
		if check_keyword(p, "ID") {
			advance_token(p)
			id_value := parse_expr(p)

			field: ^ast.Expr = nil
			is_dummy := false

			if check_keyword(p, "FIELD") {
				advance_token(p)
				field = parse_expr(p)
			} else if check_keyword(p, "DUMMY") {
				advance_token(p)
				is_dummy = true
			} else {
				error(p, p.curr_tok.range, "Expected FIELD or DUMMY after ID")
			}

			append(
				&stmt.ids,
				ast.Authority_Check_Id{id = id_value, field = field, is_dummy = is_dummy},
			)

		} else {
			break
		}
	}

	stmt.object = object
	stmt.user = user

	period_tok := expect_token(p, .Period)
	stmt.range.end = period_tok.range.end
	stmt.derived_stmt = stmt
	return stmt
}

parse_condense_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	condense_tok := expect_keyword_token(p, "CONDENSE")
	text_expr := parse_expr(p)
	period_tok := expect_token(p, .Period)
	condense_stmt := ast.new(ast.Condense_Stmt, condense_tok, period_tok)
	condense_stmt.text = text_expr
	return condense_stmt
}

