package lang_parser

import "../ast"
import "../lexer"

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

				// Check for FOR clause (e.g., FOR var IN itab WHERE (...))
				if check_keyword(p, "FOR") {
					for_expr := parse_for_expr(p)
					if for_expr != nil {
						append(&constructor_expr.args, for_expr)
					}
				} else if p.curr_tok.kind == .LParen {
					// Nested parentheses - could be a table row in VALUE constructor
					// e.g., VALUE #( ( field1 = val1 ) ( field2 = val2 ) )
					row_expr := parse_value_row_expr(p)
					if row_expr != nil {
						append(&constructor_expr.args, row_expr)
					}
				} else {
					arg := parse_call_arg(p)
					if arg != nil {
						append(&constructor_expr.args, arg)
					}
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
						"unexpected token '%s' in constructor expression",
						p.curr_tok.lit,
					)
					advance_token(p)
					if p.curr_tok.kind == .RParen ||
					   p.curr_tok.kind == .EOF ||
					   p.curr_tok.kind == .Period {
						break
					}
				}
			}

			// Safety: if we hit max iterations, skip to closing paren or statement end
			if iterations >= max_iterations {
				error(
					p,
					lparen_tok.range,
					"too many arguments or malformed constructor expression",
				)
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

// parse_for_expr parses a FOR expression in constructor expressions
// Syntax: FOR var IN itab [WHERE ( condition )] [( result_args... )]
parse_for_expr :: proc(p: ^Parser) -> ^ast.Expr {
	for_tok := expect_keyword_token(p, "FOR")
	for_expr := ast.new(ast.For_Expr, for_tok.range)
	for_expr.result_args = make([dynamic]^ast.Expr)

	// Parse loop variable name
	if p.curr_tok.kind == .Ident {
		var_tok := advance_token(p)
		for_expr.var_name = ast.new_ident(var_tok)
	} else {
		error(p, p.curr_tok.range, "expected identifier after FOR")
		return for_expr
	}

	// Expect IN keyword
	if !check_keyword(p, "IN") {
		error(p, p.curr_tok.range, "expected 'IN' after FOR variable")
		return for_expr
	}
	advance_token(p) // consume IN

	// Parse internal table expression
	for_expr.itab = parse_expr(p)

	// Check for optional WHERE clause
	if check_keyword(p, "WHERE") {
		advance_token(p) // consume WHERE
		// WHERE is followed by a parenthesized condition
		if p.curr_tok.kind == .LParen {
			advance_token(p) // consume (
			for_expr.where_cond = parse_logical_expr(p)
			expect_token(p, .RParen) // consume )
		} else {
			// WHERE without parentheses
			for_expr.where_cond = parse_logical_expr(p)
		}
	}

	// Check for result expression (parenthesized)
	// Can contain single expression or multiple named args like ( field1 = val1 field2 = val2 )
	if p.curr_tok.kind == .LParen {
		advance_token(p) // consume (

		// Parse result arguments (can be named args or regular expressions)
		max_iterations := 100
		iterations := 0
		for p.curr_tok.kind != .RParen &&
		    p.curr_tok.kind != .EOF &&
		    p.curr_tok.kind != .Period &&
		    iterations < max_iterations {
			iterations += 1
			prev_pos := p.curr_tok.range.start

			arg := parse_call_arg(p)
			if arg != nil {
				append(&for_expr.result_args, arg)
				// For backward compatibility, also set result_expr to first arg
				if for_expr.result_expr == nil {
					for_expr.result_expr = arg
				}
			}

			// If we didn't make progress, break to avoid infinite loop
			if p.curr_tok.range.start == prev_pos {
				break
			}
		}

		expect_token(p, .RParen) // consume )
	}

	for_expr.range.end = p.prev_tok.range.end
	for_expr.derived_expr = for_expr
	return for_expr
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

				// Use parse_call_arg to support named arguments (e.g., container_name = 'value')
				arg := parse_call_arg(p)
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
					error(
						p,
						p.curr_tok.range,
						"unexpected token '%s' in NEW expression",
						p.curr_tok.lit,
					)
					advance_token(p)
					// Check again after advancing
					if p.curr_tok.kind == .RParen ||
					   p.curr_tok.kind == .EOF ||
					   p.curr_tok.kind == .Period {
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

// parse_value_row_expr parses a parenthesized group of arguments in VALUE constructor
// This handles table rows like: ( field1 = val1 field2 = val2 )
parse_value_row_expr :: proc(p: ^Parser) -> ^ast.Expr {
	lparen_tok := expect_token(p, .LParen)
	row_expr := ast.new(ast.Value_Row_Expr, lparen_tok.range)
	row_expr.args = make([dynamic]^ast.Expr)

	// Parse arguments within the row
	max_iterations := 100
	iterations := 0
	for p.curr_tok.kind != .RParen &&
	    p.curr_tok.kind != .EOF &&
	    p.curr_tok.kind != .Period &&
	    iterations < max_iterations {
		iterations += 1
		prev_pos := p.curr_tok.range.start

		arg := parse_call_arg(p)
		if arg != nil {
			append(&row_expr.args, arg)
		}

		// If we didn't make progress, break to avoid infinite loop
		if p.curr_tok.range.start == prev_pos {
			break
		}
	}

	rparen_tok := expect_token(p, .RParen)
	row_expr.range.end = rparen_tok.range.end
	row_expr.derived_expr = row_expr
	return row_expr
}