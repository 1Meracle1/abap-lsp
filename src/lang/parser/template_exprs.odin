package lang_parser

import "../ast"
import "../lexer"

// parse_string_template parses a string template expression |...|
// Syntax: |literal text { embedded_expr } more text|
parse_string_template :: proc(p: ^Parser) -> ^ast.Expr {
	start_pos := p.curr_tok.range.start
	opening_pipe_end := p.curr_tok.range.end

	template_expr := ast.new(ast.String_Template_Expr, lexer.TextRange{start_pos, 0})
	template_expr.parts = make([dynamic]ast.String_Template_Part)

	// Parse content between pipes - scan the source directly
	end_pos := parse_string_template_content(p, template_expr, opening_pipe_end)

	template_expr.range.end = end_pos
	template_expr.derived_expr = template_expr

	return template_expr
}

// parse_string_template_content parses the content inside a string template
// It scans the source directly to handle literal text and embedded expressions
// Returns the position after the closing |
parse_string_template_content :: proc(
	p: ^Parser,
	template_expr: ^ast.String_Template_Expr,
	start_pos: int,
) -> int {
	pos := start_pos
	literal_start := pos

	for pos < len(p.file.src) {
		ch := p.file.src[pos]

		if ch == '|' {
			// End of string template - save any pending literal
			if pos > literal_start {
				part := ast.String_Template_Part {
					is_expr = false,
					literal = p.file.src[literal_start:pos],
					range   = lexer.TextRange{literal_start, pos},
				}
				append(&template_expr.parts, part)
			}

			// Position the lexer so that the next scan starts AFTER the closing |
			// The closing | is at position pos, so next scan should start at pos+1
			end_pos := pos + 1

			// Set up the lexer for the next token scan
			// p.l.pos needs to be where scan will record token start
			// p.l.read_pos needs to point to where we'll read the next char
			// p.l.ch needs to be the character at read_pos
			p.l.pos = end_pos
			p.l.read_pos = end_pos
			if end_pos < len(p.file.src) {
				p.l.ch = rune(p.file.src[end_pos])
				// Advance to set up pos/read_pos correctly for scan
				lexer.advance_rune(&p.l)
			} else {
				p.l.ch = -1
			}

			// Update the current token to represent the closing |
			// and then get the next token
			p.prev_tok = p.curr_tok
			p.curr_tok = lexer.scan(&p.l)
			if p.curr_tok.kind != .EOF {
				consume_comments(p)
			}

			return end_pos
		} else if ch == '{' {
			// Embedded expression - save any pending literal first
			if pos > literal_start {
				part := ast.String_Template_Part {
					is_expr = false,
					literal = p.file.src[literal_start:pos],
					range   = lexer.TextRange{literal_start, pos},
				}
				append(&template_expr.parts, part)
			}

			// Move past the { and sync lexer
			expr_start := pos
			after_brace := pos + 1

			// Position lexer after the {
			p.l.pos = after_brace
			p.l.read_pos = after_brace
			if after_brace < len(p.file.src) {
				p.l.ch = rune(p.file.src[after_brace])
				lexer.advance_rune(&p.l)
			} else {
				p.l.ch = -1
			}

			// Get the first token after {
			p.prev_tok = p.curr_tok
			p.curr_tok = lexer.scan(&p.l)
			if p.curr_tok.kind != .EOF {
				consume_comments(p)
			}

			embedded_expr := parse_expr(p)

			// Parse formatting options (e.g., ALPHA = OUT, WIDTH = 10)
			format_options := parse_embedded_format_options(p)

			// The expression should end at }
			if p.curr_tok.kind == .RBrace {
				pos = p.curr_tok.range.end
				// Advance past the }
				advance_token(p)
				// Now we need to continue scanning the string template from pos
				// The lexer is pointing somewhere after the }, but we need to scan
				// the string template content again starting from pos
			} else {
				// Error - missing closing brace
				error(
					p,
					p.curr_tok.range,
					"expected '}' after embedded expression in string template",
				)
				// Try to recover by finding } or |
				pos = p.curr_tok.range.end
				for pos < len(p.file.src) && p.file.src[pos] != '}' && p.file.src[pos] != '|' {
					pos += 1
				}
				if pos < len(p.file.src) && p.file.src[pos] == '}' {
					pos += 1
				}
			}

			// Add the embedded expression part
			part := ast.String_Template_Part {
				is_expr        = true,
				expr           = embedded_expr,
				format_options = format_options,
				range          = lexer.TextRange{expr_start, pos},
			}
			append(&template_expr.parts, part)

			// Continue scanning from after the }
			literal_start = pos
		} else {
			pos += 1
		}
	}

	// If we get here, we hit EOF without finding closing |
	error(p, lexer.TextRange{start_pos, pos}, "string template was not terminated")
	if pos > literal_start {
		part := ast.String_Template_Part {
			is_expr = false,
			literal = p.file.src[literal_start:pos],
			range   = lexer.TextRange{literal_start, pos},
		}
		append(&template_expr.parts, part)
	}
	return pos
}

// parse_embedded_format_options parses formatting options in an embedded expression
// Syntax: { expr ALPHA = OUT WIDTH = 10 ... }
// Returns a dynamic array of format options
parse_embedded_format_options :: proc(p: ^Parser) -> [dynamic]ast.Embedded_Format_Option {
	options := make([dynamic]ast.Embedded_Format_Option)

	// Parse formatting options until we hit } or EOF
	max_iterations := 20 // Safety limit
	iterations := 0

	for p.curr_tok.kind == .Ident && p.curr_tok.kind != .RBrace && p.curr_tok.kind != .EOF {
		iterations += 1
		if iterations > max_iterations {
			break
		}

		// Check if this is a known format option keyword
		opt_start := p.curr_tok.range.start
		format_kind, is_format_option := parse_format_option_kind(p)
		if !is_format_option {
			// Not a format option, might be part of expression or error
			break
		}

		// Expect = sign
		if p.curr_tok.kind != .Eq {
			// Not a valid format option, stop parsing
			break
		}
		advance_token(p) // consume =

		// Parse the value
		option := ast.Embedded_Format_Option {
			kind  = format_kind,
			range = lexer.TextRange{opt_start, 0},
		}

		// Parse the value based on kind
		if p.curr_tok.kind == .Ident {
			// Parse keyword value (OUT, IN, ISO, etc.)
			option.value = parse_format_option_value(p, format_kind)
		} else if p.curr_tok.kind == .Number {
			// Parse numeric value (for WIDTH, DECIMALS, etc.)
			option.value = .Custom
			option.num_value = parse_number_value(p)
		} else if p.curr_tok.kind == .String {
			// Parse string value (for PAD, CURRENCY, etc.)
			option.value = .Custom
			option.str_value = p.curr_tok.lit
			advance_token(p)
		} else {
			// Invalid value, try to continue
			advance_token(p)
		}

		option.range.end = p.prev_tok.range.end
		append(&options, option)
	}

	return options
}

// parse_format_option_kind parses the format option keyword and returns its kind
// Returns the kind and a boolean indicating if it was a valid format option
parse_format_option_kind :: proc(p: ^Parser) -> (ast.Embedded_Format_Kind, bool) {
	if p.curr_tok.kind != .Ident {
		return .Alpha, false
	}

	keyword := to_upper(p.keyword_buffer[:], p.curr_tok.lit)
	kind: ast.Embedded_Format_Kind
	is_valid := true

	switch keyword {
	case "ALPHA":
		kind = .Alpha
	case "DATE":
		kind = .Date
	case "TIME":
		kind = .Time
	case "WIDTH":
		kind = .Width
	case "ALIGN":
		kind = .Align
	case "PAD":
		kind = .Pad
	case "CASE":
		kind = .Case
	case "SIGN":
		kind = .Sign
	case "DECIMALS":
		kind = .Decimals
	case "EXPONENT":
		kind = .Exponent
	case "ZERO":
		kind = .Zero
	case "NUMBER":
		kind = .Number
	case "STYLE":
		kind = .Style
	case "CURRENCY":
		kind = .Currency
	case "COUNTRY":
		kind = .Country
	case "TIMESTAMP":
		kind = .Timestamp
	case "TIMEZONE":
		kind = .Timezone
	case:
		is_valid = false
	}

	if is_valid {
		advance_token(p) // consume the keyword
	}

	return kind, is_valid
}

// parse_format_option_value parses the value for a format option
parse_format_option_value :: proc(
	p: ^Parser,
	kind: ast.Embedded_Format_Kind,
) -> ast.Embedded_Format_Value {
	if p.curr_tok.kind != .Ident {
		return .Custom
	}

	keyword := to_upper(p.keyword_buffer[:], p.curr_tok.lit)
	value: ast.Embedded_Format_Value = .Custom

	switch keyword {
	case "IN":
		value = .In
	case "OUT":
		value = .Out
	case "ISO":
		value = .Iso
	case "USER":
		value = .User
	case "RAW":
		value = .Raw
	case "ENVIRONMENT":
		value = .Environment
	case "LEFT":
		value = .Left
	case "RIGHT":
		value = .Right
	case "CENTER":
		value = .Center
	case "UPPER":
		value = .Upper
	case "LOWER":
		value = .Lower
	case "YES":
		value = .Yes
	case "NO":
		value = .No
	case "SIMPLE":
		value = .Simple
	case "SCALE_PRESERVING":
		value = .Scale_Preserving
	case "SIGN_AS_POSTFIX":
		value = .Sign_As_Postfix
	case "LEFTPLUS":
		value = .Leftplus
	case "LEFTSPACE":
		value = .Leftspace
	case "RIGHTPLUS":
		value = .Rightplus
	case "RIGHTSPACE":
		value = .Rightspace
	case "SPACE":
		value = .Space
	}

	advance_token(p) // consume the value
	return value
}

// parse_number_value parses a numeric value and returns it as an int
parse_number_value :: proc(p: ^Parser) -> int {
	if p.curr_tok.kind != .Number {
		return 0
	}

	num_value := 0
	for ch in p.curr_tok.lit {
		if ch >= '0' && ch <= '9' {
			num_value = num_value * 10 + int(ch - '0')
		}
	}

	advance_token(p)
	return num_value
}