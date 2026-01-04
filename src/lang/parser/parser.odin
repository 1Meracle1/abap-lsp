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
		case "FIELD":
			if check_hyphenated_keyword(p, "FIELD", "SYMBOLS") {
				return parse_field_symbol_decl(p)
			}
		case "CONTROLS":
			return parse_controls_decl(p)
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

	// Accept TYPE or LIKE
	if check_keyword(p, "TYPE") || check_keyword(p, "LIKE") {
		advance_token(p)
	} else {
		expect_keyword_token(p, "TYPE")
	}

	type_expr := parse_type_expr(p)

	// Parse optional LENGTH
	if check_keyword(p, "LENGTH") {
		advance_token(p)
		// Skip LENGTH expression for now, but we could store it
		parse_expr(p)
	}

	value_expr: ^ast.Expr = nil
	if check_keyword(p, "VALUE") {
		advance_token(p)
		value_expr = parse_expr(p)
	}

	data_decl := ast.new(ast.Data_Typed_Decl, data_tok, p.curr_tok)
	data_decl.ident = ast.new_ident(ident_tok)
	data_decl.typed = type_expr
	data_decl.value = value_expr
	return data_decl
}

parse_data_typed_multiple_decl :: proc(p: ^Parser, data_tok: lexer.Token) -> ^ast.Decl {
	chain_decl := ast.new(ast.Data_Typed_Chain_Decl, data_tok.range)
	chain_decl.decls = make([dynamic]^ast.Data_Typed_Decl)

	for {
		ident_tok := expect_token(p, .Ident)

		// Accept TYPE or LIKE
		if check_keyword(p, "TYPE") || check_keyword(p, "LIKE") {
			advance_token(p)
		} else {
			expect_keyword_token(p, "TYPE")
		}

		type_expr := parse_type_expr(p)

		// Parse optional LENGTH
		if check_keyword(p, "LENGTH") {
			advance_token(p)
			parse_expr(p)
		}

		value_expr: ^ast.Expr = nil
		if check_keyword(p, "VALUE") {
			advance_token(p)
			value_expr = parse_expr(p)
		}

		decl := ast.new(ast.Data_Typed_Decl, ident_tok, p.prev_tok)
		decl.ident = ast.new_ident(ident_tok)
		decl.typed = type_expr
		decl.value = value_expr
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

// CONSTANTS declarations parsing

parse_constants_decl :: proc(p: ^Parser) -> ^ast.Decl {
	const_tok := expect_token(p, .Ident)
	if allow_token(p, .Colon) {
		return parse_constants_chain_decl(p, const_tok)
	}
	return parse_constants_single_decl(p, const_tok)
}

parse_constants_single_decl :: proc(p: ^Parser, const_tok: lexer.Token) -> ^ast.Decl {
	ident_tok := expect_token(p, .Ident)
	
	// Accept TYPE or LIKE
	if check_keyword(p, "TYPE") || check_keyword(p, "LIKE") {
		advance_token(p)
	} else {
		expect_keyword_token(p, "TYPE")
	}
	
	type_expr := parse_type_expr(p)

	// Parse optional LENGTH
	if check_keyword(p, "LENGTH") {
		advance_token(p)
		parse_expr(p)
	}

	// CONSTANTS must have VALUE
	value_expr: ^ast.Expr = nil
	if check_keyword(p, "VALUE") {
		advance_token(p)
		value_expr = parse_expr(p)
	}

	period_tok := expect_token(p, .Period)

	const_decl := ast.new(ast.Const_Decl, const_tok, period_tok)
	const_decl.ident = ast.new_ident(ident_tok)
	const_decl.typed = type_expr
	const_decl.value = value_expr
	const_decl.derived_stmt = const_decl
	return const_decl
}

parse_constants_chain_decl :: proc(p: ^Parser, const_tok: lexer.Token) -> ^ast.Decl {
	chain_decl := ast.new(ast.Const_Chain_Decl, const_tok.range)
	chain_decl.decls = make([dynamic]^ast.Const_Decl)

	for {
		if check_keyword(p, "BEGIN") {
			struct_decl := parse_constants_struct_decl(p)
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
		
		// Accept TYPE or LIKE
		if check_keyword(p, "TYPE") || check_keyword(p, "LIKE") {
			advance_token(p)
		} else {
			expect_keyword_token(p, "TYPE")
		}
		
		type_expr := parse_type_expr(p)

		// Parse optional LENGTH
		if check_keyword(p, "LENGTH") {
			advance_token(p)
			parse_expr(p)
		}

		value_expr: ^ast.Expr = nil
		if check_keyword(p, "VALUE") {
			advance_token(p)
			value_expr = parse_expr(p)
		}

		decl := ast.new(ast.Const_Decl, ident_tok, p.prev_tok)
		decl.ident = ast.new_ident(ident_tok)
		decl.typed = type_expr
		decl.value = value_expr
		decl.derived_stmt = decl
		append(&chain_decl.decls, decl)

		if allow_token(p, .Comma) {
			continue
		}

		period_tok := expect_token(p, .Period)
		chain_decl.range.end = period_tok.range.end
		break
	}

	chain_decl.derived_stmt = chain_decl
	return chain_decl
}

parse_constants_struct_decl :: proc(p: ^Parser) -> ^ast.Const_Struct_Decl {
	begin_tok := expect_keyword_token(p, "BEGIN")
	expect_keyword_token(p, "OF")
	ident_tok := expect_token(p, .Ident)

	struct_decl := ast.new(ast.Const_Struct_Decl, begin_tok.range)
	struct_decl.ident = ast.new_ident(ident_tok)
	struct_decl.components = make([dynamic]^ast.Stmt)

	expect_token(p, .Comma)

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "END") {
			break
		}

		if check_keyword(p, "BEGIN") {
			nested_struct := parse_constants_struct_decl(p)
			if nested_struct != nil {
				append(&struct_decl.components, &nested_struct.node)
			}
			if !allow_token(p, .Comma) {
				break
			}
			continue
		}

		field_ident_tok := expect_token(p, .Ident)
		
		// Accept TYPE or LIKE
		if check_keyword(p, "TYPE") || check_keyword(p, "LIKE") {
			advance_token(p)
		} else {
			expect_keyword_token(p, "TYPE")
		}
		
		type_expr := parse_type_expr(p)

		// Parse optional LENGTH
		if check_keyword(p, "LENGTH") {
			advance_token(p)
			parse_expr(p)
		}

		value_expr: ^ast.Expr = nil
		if check_keyword(p, "VALUE") {
			advance_token(p)
			value_expr = parse_expr(p)
		}

		field_decl := ast.new(ast.Const_Decl, field_ident_tok, p.prev_tok)
		field_decl.ident = ast.new_ident(field_ident_tok)
		field_decl.typed = type_expr
		field_decl.value = value_expr
		field_decl.derived_stmt = field_decl
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

	struct_decl.derived_stmt = struct_decl
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
				if keyword == "TYPE" || keyword == "LIKE" {
					advance_token(p)
					param.typed = parse_type_expr(p)
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
parse_format_option_value :: proc(p: ^Parser, kind: ast.Embedded_Format_Kind) -> ast.Embedded_Format_Value {
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

				// Check for FOR clause (e.g., FOR var IN itab WHERE (...))
				if check_keyword(p, "FOR") {
					for_expr := parse_for_expr(p)
					if for_expr != nil {
						append(&constructor_expr.args, for_expr)
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
		for p.curr_tok.kind != .RParen && p.curr_tok.kind != .EOF && p.curr_tok.kind != .Period && iterations < max_iterations {
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
	type_expr := parse_type_expr(p)

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
		type_expr := parse_type_expr(p)

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
				param.typed = parse_type_expr(p)
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
			param.typed = parse_type_expr(p)
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
		if check_keyword(p, "EXPORTING") || check_keyword(p, "IMPORTING") ||
		   check_keyword(p, "TABLES") || check_keyword(p, "CHANGING") ||
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
			if kind == .Exceptions && to_upper(p.keyword_buffer[:], param_name_tok.lit) == "OTHERS" {
				// OTHERS = value
				// But we already consumed the token, so put back logic...
				// Actually, OTHERS is just another exception name, so continue normally
			}
			// If no '=', this might be a keyword; put it back and break
			// Since we already consumed the name, we need to handle this case
			error(p, p.curr_tok.range, "expected '=' after parameter name '%s'", param_name_tok.lit)
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

parse_case_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	case_tok := advance_token(p)
	cond_expr := parse_expr(p)
	expect_token(p, .Period)

	branches := make([dynamic]ast.Case_When_Branch)
	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "ENDCASE") {
			break
		}
		if !check_keyword(p, "WHEN") {
			error(p, p.curr_tok.range, "expected WHEN keyword")
			break
		}
		advance_token(p)

		branch: ast.Case_When_Branch
		if check_keyword(p, "OTHERS") {
			advance_token(p)
			branch.is_others = true
		} else {
			branch.expr = parse_expr(p)
		}
		expect_token(p, .Period)

		branch.body = make([dynamic]^ast.Stmt)
		for p.curr_tok.kind != .EOF {
			if check_keyword(p, "WHEN") || check_keyword(p, "ENDCASE") {
				break
			}
			stmt := parse_stmt(p)
			if stmt != nil {
				append(&branch.body, stmt)
			}
		}

		append(&branches, branch)
	}

	endcase_tok := expect_keyword_token(p, "ENDCASE")
	period_tok := expect_token(p, .Period)
	stmt := ast.new(ast.Case_Stmt, case_tok, period_tok)
	stmt.expr = cond_expr
	stmt.branches = branches
	return stmt
}

parse_while_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	while_tok := advance_token(p)
	cond := parse_logical_expr(p)
	expect_token(p, .Period)

	while_stmt := ast.new(ast.While_Stmt, while_tok.range)
	while_stmt.cond = cond
	while_stmt.body = make([dynamic]^ast.Stmt)

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "ENDWHILE") {
			break
		}
		stmt := parse_stmt(p)
		if stmt != nil {
			append(&while_stmt.body, stmt)
		}
	}

	endwhile_tok := expect_keyword_token(p, "ENDWHILE")
	period_tok := expect_token(p, .Period)
	while_stmt.range.end = period_tok.range.end
	return while_stmt
}

// LOOP statement parser
// Syntax variations:
// - LOOP AT itab [INTO wa | ASSIGNING <fs> | TRANSPORTING NO FIELDS] [FROM idx] [TO idx] [WHERE condition]. body... ENDLOOP.
// - LOOP AT itab GROUP BY key [INTO wa | ASSIGNING <fs>]. body... ENDLOOP.
// - LOOP AT GROUP group_var [INTO wa | ASSIGNING <fs>] [WHERE condition]. body... ENDLOOP.
// - LOOP AT SCREEN. body... ENDLOOP.
parse_loop_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	loop_tok := expect_keyword_token(p, "LOOP")

	loop_stmt := ast.new(ast.Loop_Stmt, loop_tok.range)
	loop_stmt.body = make([dynamic]^ast.Stmt)

	// Expect AT keyword
	expect_keyword_token(p, "AT")

	// Check for LOOP AT SCREEN
	if check_keyword(p, "SCREEN") {
		advance_token(p)
		loop_stmt.kind = .At_Screen
		expect_token(p, .Period)
	} else if check_keyword(p, "GROUP") {
		// LOOP AT GROUP group_var
		advance_token(p) // consume GROUP
		loop_stmt.kind = .At_Group
		loop_stmt.group_var = parse_expr(p)

		// Parse optional clauses for LOOP AT GROUP
		parse_loop_clauses(p, loop_stmt)
		expect_token(p, .Period)
	} else {
		// Regular LOOP AT itab
		loop_stmt.kind = .At
		loop_stmt.itab = parse_expr(p)

		// Parse optional clauses
		parse_loop_clauses(p, loop_stmt)
		expect_token(p, .Period)
	}

	// Parse body until ENDLOOP
	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "ENDLOOP") {
			break
		}
		stmt := parse_stmt(p)
		if stmt != nil {
			append(&loop_stmt.body, stmt)
		}
	}

	endloop_tok := expect_keyword_token(p, "ENDLOOP")
	period_tok := expect_token(p, .Period)
	loop_stmt.range.end = period_tok.range.end
	loop_stmt.derived_stmt = loop_stmt
	_ = endloop_tok

	return loop_stmt
}

// parse_loop_clauses parses the optional clauses of a LOOP statement
parse_loop_clauses :: proc(p: ^Parser, loop_stmt: ^ast.Loop_Stmt) {
	for p.curr_tok.kind != .EOF && p.curr_tok.kind != .Period {
		if check_keyword(p, "INTO") {
			advance_token(p)
			// Check for inline DATA declaration: INTO DATA(var)
			if check_keyword(p, "DATA") {
				loop_stmt.into_target = parse_data_inline_expr(p)
			} else {
				loop_stmt.into_target = parse_expr(p)
			}
		} else if check_keyword(p, "ASSIGNING") {
			advance_token(p)
			// Check for inline FIELD-SYMBOL declaration: ASSIGNING FIELD-SYMBOL(<fs>)
			if check_hyphenated_keyword(p, "FIELD", "SYMBOL") {
				loop_stmt.assigning_target = parse_inline_field_symbol(p)
			} else {
				loop_stmt.assigning_target = parse_field_symbol_ref(p)
			}
		} else if check_keyword(p, "TRANSPORTING") {
			advance_token(p)
			expect_keyword_token(p, "NO")
			expect_keyword_token(p, "FIELDS")
			loop_stmt.transporting_no_fields = true
		} else if check_keyword(p, "FROM") {
			advance_token(p)
			loop_stmt.from_expr = parse_expr(p)
		} else if check_keyword(p, "TO") {
			advance_token(p)
			loop_stmt.to_expr = parse_expr(p)
		} else if check_keyword(p, "WHERE") {
			advance_token(p)
			loop_stmt.where_cond = parse_logical_expr(p)
		} else if check_keyword(p, "GROUP") {
			advance_token(p)
			expect_keyword_token(p, "BY")
			loop_stmt.group_by = parse_loop_group_by(p)
		} else {
			// Unknown clause, break out
			break
		}
	}
}

// parse_loop_group_by parses the GROUP BY clause of a LOOP statement
// Syntax: GROUP BY ( key1 = expr1 key2 = expr2 ... ) or GROUP BY expr
parse_loop_group_by :: proc(p: ^Parser) -> ^ast.Loop_Group_By {
	group_by := new(ast.Loop_Group_By)
	group_by.components = make([dynamic]^ast.Named_Arg)

	// Check if it's a parenthesized group key specification
	if p.curr_tok.kind == .LParen {
		advance_token(p) // consume (

		for p.curr_tok.kind != .EOF && p.curr_tok.kind != .RParen {
			// Parse key component: name = expr
			if p.curr_tok.kind == .Ident {
				name_tok := advance_token(p)
				if p.curr_tok.kind == .Eq {
					advance_token(p) // consume =
					value := parse_expr(p)

					named_arg := ast.new(
						ast.Named_Arg,
						lexer.TextRange{name_tok.range.start, value.range.end},
					)
					named_arg.name = ast.new_ident(name_tok)
					named_arg.value = value
					named_arg.derived_expr = named_arg
					append(&group_by.components, named_arg)
				} else {
					// Just a field name reference
					break
				}
			} else {
				break
			}
		}

		if p.curr_tok.kind == .RParen {
			advance_token(p) // consume )
		}
	} else {
		// Simple expression as group key
		// This is typically a field or a simple identifier
	}

	return group_by
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

parse_clear_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	clear_tok := advance_token(p)
	exprs := make([dynamic]^ast.Expr)
	if allow_token(p, .Colon) {
		for p.curr_tok.kind != .EOF {
			expr := parse_expr(p)
			if expr != nil {
				append(&exprs, expr)
			} else {
				break
			}
			if p.curr_tok.kind == .Period {
				break
			}
			if allow_token(p, .Comma) {
				continue
			}
			error(p, p.curr_tok.range, "expected ','")
			break
		}
	} else {
		expr := parse_expr(p)
		append(&exprs, expr)
	}
	end_tok := expect_token(p, .Period)
	clear_stmt := ast.new(ast.Clear_Stmt, clear_tok, end_tok)
	clear_stmt.exprs = exprs
	return clear_stmt
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

// INSERT statement parser
// Syntax variations:
// - INSERT VALUE #( ... ) INTO TABLE itab.
// - INSERT INTO target VALUES wa.
// - INSERT target FROM wa.
// - INSERT target FROM TABLE itab.
parse_insert_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	insert_tok := expect_keyword_token(p, "INSERT")

	insert_stmt := ast.new(ast.Insert_Stmt, insert_tok.range)

	// Check for "INSERT INTO target VALUES wa" form
	if check_keyword(p, "INTO") {
		advance_token(p) // consume INTO
		insert_stmt.target = parse_expr(p)
		expect_keyword_token(p, "VALUES")
		insert_stmt.source = parse_expr(p)
		insert_stmt.kind = .Into_Db
	} else {
		// Parse the value expression or target identifier
		value_or_target := parse_expr(p)

		// Check what comes next to determine the form
		if check_keyword(p, "INTO") {
			// INSERT expr INTO TABLE itab form
			advance_token(p) // consume INTO
			expect_keyword_token(p, "TABLE")
			insert_stmt.value_expr = value_or_target
			insert_stmt.target = parse_expr(p)
			insert_stmt.kind = .Into_Table
		} else if check_keyword(p, "FROM") {
			// INSERT target FROM [TABLE] source form
			advance_token(p) // consume FROM
			insert_stmt.target = value_or_target

			if check_keyword(p, "TABLE") {
				advance_token(p) // consume TABLE
				insert_stmt.source = parse_expr(p)
				insert_stmt.kind = .From_Table
			} else {
				insert_stmt.source = parse_expr(p)
				insert_stmt.kind = .From_Wa
			}
		} else {
			// Simple INSERT expr form - treat as insert into table
			insert_stmt.value_expr = value_or_target
			insert_stmt.kind = .Into_Table
		}
	}

	period_tok := expect_token(p, .Period)
	insert_stmt.range.end = period_tok.range.end
	insert_stmt.derived_stmt = insert_stmt
	return insert_stmt
}

parse_sort_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	sort_tok := expect_keyword_token(p, "SORT")
	itab_expr := parse_expr(p)

	order_kind: ast.Sort_Order_Kind
	if check_keyword(p, "ASCENDING") {
		advance_token(p)
		order_kind = .Ascending
	} else if check_keyword(p, "DESCENDING") {
		advance_token(p)
		order_kind = .Descending
	}

	cols_by := make([dynamic]ast.Sort_Cols_By)
	if check_keyword(p, "BY") {
		advance_token(p)
		for p.curr_tok.kind != .EOF {
			if p.curr_tok.kind == .Period {
				break
			}
			col_expr := parse_expr(p)
			col_order_kind: ast.Sort_Order_Kind
			if check_keyword(p, "ASCENDING") {
				advance_token(p)
				col_order_kind = .Ascending
			} else if check_keyword(p, "DESCENDING") {
				advance_token(p)
				col_order_kind = .Descending
			}
			append(&cols_by, ast.Sort_Cols_By{col = col_expr, order = col_order_kind})
		}
	}

	period_tok := expect_token(p, .Period)
	sort_stmt := ast.new(ast.Sort_Stmt, sort_tok, period_tok)
	sort_stmt.itab = itab_expr
	sort_stmt.cols_by = cols_by
	sort_stmt.order = order_kind
	return sort_stmt
}

// APPEND statement parser
// Syntax variations:
// - APPEND expr TO itab.
// - APPEND INITIAL LINE TO itab [ASSIGNING <fs>].
// - APPEND LINES OF itab2 TO itab1.
parse_append_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	append_tok := expect_keyword_token(p, "APPEND")

	append_stmt := ast.new(ast.Append_Stmt, append_tok.range)

	// Check for INITIAL LINE form
	if check_keyword(p, "INITIAL") {
		advance_token(p) // consume INITIAL
		expect_keyword_token(p, "LINE")
		expect_keyword_token(p, "TO")
		append_stmt.target = parse_expr(p)
		append_stmt.kind = .Initial_Line

		// Check for optional ASSIGNING clause
		if check_keyword(p, "ASSIGNING") {
			advance_token(p) // consume ASSIGNING
			// Check for inline FIELD-SYMBOL declaration: ASSIGNING FIELD-SYMBOL(<fs>)
			if check_hyphenated_keyword(p, "FIELD", "SYMBOL") {
				append_stmt.assigning_target = parse_inline_field_symbol(p)
			} else {
				append_stmt.assigning_target = parse_field_symbol_ref(p)
			}
		}

		period_tok := expect_token(p, .Period)
		append_stmt.range.end = period_tok.range.end
		append_stmt.derived_stmt = append_stmt
		return append_stmt
	}

	// Check for LINES OF form
	if check_keyword(p, "LINES") {
		advance_token(p) // consume LINES
		expect_keyword_token(p, "OF")
		append_stmt.source = parse_expr(p)
		expect_keyword_token(p, "TO")
		append_stmt.target = parse_expr(p)
		append_stmt.kind = .Lines_Of

		period_tok := expect_token(p, .Period)
		append_stmt.range.end = period_tok.range.end
		append_stmt.derived_stmt = append_stmt
		return append_stmt
	}

	// Simple APPEND expr TO itab form
	append_stmt.source = parse_expr(p)
	expect_keyword_token(p, "TO")
	append_stmt.target = parse_expr(p)
	append_stmt.kind = .Simple

	// Check for optional ASSIGNING clause
	if check_keyword(p, "ASSIGNING") {
		advance_token(p) // consume ASSIGNING
		// Check for inline FIELD-SYMBOL declaration: ASSIGNING FIELD-SYMBOL(<fs>)
		if check_hyphenated_keyword(p, "FIELD", "SYMBOL") {
			append_stmt.assigning_target = parse_inline_field_symbol(p)
		} else {
			append_stmt.assigning_target = parse_field_symbol_ref(p)
		}
	}

	period_tok := expect_token(p, .Period)
	append_stmt.range.end = period_tok.range.end
	append_stmt.derived_stmt = append_stmt
	return append_stmt
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

// FIELD-SYMBOLS declaration parser
// Syntax: FIELD-SYMBOLS <fs> TYPE type.
// Syntax: FIELD-SYMBOLS <fs> LIKE LINE OF itab.
parse_field_symbol_decl :: proc(p: ^Parser) -> ^ast.Stmt {
	// FIELD-SYMBOLS was already consumed by check_hyphenated_keyword
	fs_tok := p.prev_tok

	// Parse field symbol name <fs>
	fs_ident := parse_field_symbol_ref(p)

	fs_decl := ast.new(ast.Field_Symbol_Decl, fs_tok.range)
	if ident, ok := fs_ident.derived_expr.(^ast.Ident); ok {
		fs_decl.ident = ident
	}

	// Parse TYPE or LIKE clause
	if check_keyword(p, "TYPE") || check_keyword(p, "LIKE") {
		advance_token(p)
		fs_decl.typed = parse_type_expr(p)
	} else {
		error(p, p.curr_tok.range, "expected TYPE or LIKE after field symbol name")
	}

	period_tok := expect_token(p, .Period)
	fs_decl.range.end = period_tok.range.end
	fs_decl.derived_stmt = fs_decl
	return fs_decl
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

// CONTROLS declaration parser
// Syntax: CONTROLS contrl TYPE TABLEVIEW USING SCREEN dynnr.
// Syntax: CONTROLS contrl TYPE TABSTRIP.
// Syntax: CONTROLS: name1 TYPE TABSTRIP, name2 TYPE TABLEVIEW USING SCREEN 100.
parse_controls_decl :: proc(p: ^Parser) -> ^ast.Stmt {
	controls_tok := expect_keyword_token(p, "CONTROLS")

	// Check for chained declaration with colon
	if allow_token(p, .Colon) {
		return parse_controls_chain_decl(p, controls_tok)
	}

	// Single declaration
	decl := parse_controls_single_decl(p, controls_tok)
	if decl != nil {
		period_tok := expect_token(p, .Period)
		decl.range.end = period_tok.range.end
	}
	return decl
}

// parse_controls_chain_decl parses a chained CONTROLS declaration
// Syntax: CONTROLS: name1 TYPE TABSTRIP, name2 TYPE TABLEVIEW USING SCREEN 100.
parse_controls_chain_decl :: proc(p: ^Parser, controls_tok: lexer.Token) -> ^ast.Stmt {
	chain_decl := ast.new(ast.Controls_Chain_Decl, controls_tok.range)
	chain_decl.decls = make([dynamic]^ast.Controls_Decl)

	for {
		decl := parse_controls_single_decl(p, controls_tok)
		if decl != nil {
			append(&chain_decl.decls, decl)
		}

		if p.curr_tok.kind == .Period {
			break
		}

		if !allow_token(p, .Comma) {
			error(p, p.curr_tok.range, "expected ',' or '.'")
			break
		}
	}

	period_tok := expect_token(p, .Period)
	chain_decl.range.end = period_tok.range.end
	chain_decl.derived_stmt = chain_decl
	return chain_decl
}

// parse_controls_single_decl parses a single CONTROLS declaration
// Syntax: contrl TYPE TABLEVIEW USING SCREEN dynnr
// Syntax: contrl TYPE TABSTRIP
parse_controls_single_decl :: proc(p: ^Parser, controls_tok: lexer.Token) -> ^ast.Controls_Decl {
	// Parse control name
	ident_tok := expect_token(p, .Ident)

	// Expect TYPE keyword
	expect_keyword_token(p, "TYPE")

	// Parse control type (TABLEVIEW or TABSTRIP)
	type_tok := expect_token(p, .Ident)
	type_upper := to_upper(p.keyword_buffer[:], type_tok.lit)

	controls_decl := ast.new(ast.Controls_Decl, controls_tok, p.curr_tok)
	controls_decl.ident = ast.new_ident(ident_tok)

	if type_upper == "TABLEVIEW" {
		controls_decl.kind = .Tableview
		// Parse USING SCREEN dynnr
		expect_keyword_token(p, "USING")
		expect_keyword_token(p, "SCREEN")
		controls_decl.screen_dynnr = parse_expr(p)
	} else if type_upper == "TABSTRIP" {
		controls_decl.kind = .Tabstrip
		controls_decl.screen_dynnr = nil
	} else {
		error(p, type_tok.range, "expected TABLEVIEW or TABSTRIP")
		controls_decl.kind = .Tabstrip
	}

	controls_decl.derived_stmt = controls_decl
	return controls_decl
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

// READ TABLE statement parser
// Syntax variations:
// - READ TABLE itab WITH TABLE KEY field1 = val1 ... [INTO wa | ASSIGNING <fs> | TRANSPORTING NO FIELDS].
// - READ TABLE itab WITH KEY field1 = val1 ... [INTO wa | ASSIGNING <fs> | TRANSPORTING NO FIELDS].
// - READ TABLE itab INDEX idx [USING KEY key_name] [INTO wa | ASSIGNING <fs>].
parse_read_table_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	read_tok := expect_keyword_token(p, "READ")
	expect_keyword_token(p, "TABLE")

	read_stmt := ast.new(ast.Read_Table_Stmt, read_tok.range)
	read_stmt.itab = parse_expr(p)

	for p.curr_tok.kind != .EOF && p.curr_tok.kind != .Period {
		if check_keyword(p, "WITH") {
			advance_token(p)
			if check_keyword(p, "TABLE") {
				advance_token(p)
				if check_keyword(p, "KEY") {
					advance_token(p)
					read_stmt.kind = .With_Table_Key
					read_stmt.key = parse_read_table_key(p)
				} else {
					error(p, p.curr_tok.range, "expected KEY after WITH TABLE")
					break
				}
			} else {
				if check_keyword(p, "KEY") {
					advance_token(p)
					read_stmt.kind = .With_Key
					read_stmt.key = parse_read_table_key(p)
				} else {
					error(p, p.curr_tok.range, "expected KEY after WITH")
					break
				}
			}
		} else if check_keyword(p, "INDEX") {
			advance_token(p)
			read_stmt.kind = .Index
			read_stmt.index_expr = parse_expr(p)
		} else if check_keyword(p, "USING") {
			advance_token(p)
			expect_keyword_token(p, "KEY")
			if p.curr_tok.kind == .Ident {
				key_name_tok := advance_token(p)
				read_stmt.using_key = ast.new_ident(key_name_tok)
			}
		} else if check_keyword(p, "INTO") {
			advance_token(p) // consume INTO
			// Check for inline DATA declaration: INTO DATA(var)
			if check_keyword(p, "DATA") {
				read_stmt.into_target = parse_data_inline_expr(p)
			} else {
				read_stmt.into_target = parse_expr(p)
			}
		} else if check_keyword(p, "ASSIGNING") {
			advance_token(p) // consume ASSIGNING
			// Check for inline FIELD-SYMBOL declaration: ASSIGNING FIELD-SYMBOL(<fs>)
			if check_hyphenated_keyword(p, "FIELD", "SYMBOL") {
				read_stmt.assigning_target = parse_inline_field_symbol(p)
			} else {
				read_stmt.assigning_target = parse_field_symbol_ref(p)
			}
		} else if check_keyword(p, "TRANSPORTING") {
			advance_token(p) // consume TRANSPORTING
			expect_keyword_token(p, "NO")
			expect_keyword_token(p, "FIELDS")
			read_stmt.transporting_no_fields = true
		} else {
			// Unknown clause, break out
			break
		}
	}

	period_tok := expect_token(p, .Period)
	read_stmt.range.end = period_tok.range.end
	read_stmt.derived_stmt = read_stmt
	return read_stmt
}

// parse_read_table_key parses the WITH KEY clause of a READ TABLE statement
// Syntax: WITH KEY field1 = val1 field2 = val2 ... or WITH KEY table_line = value
parse_read_table_key :: proc(p: ^Parser) -> ^ast.Read_Table_Key {
	key := new(ast.Read_Table_Key)
	key.components = make([dynamic]^ast.Named_Arg)

	// Parse key components
	for p.curr_tok.kind == .Ident && p.curr_tok.kind != .Period {
		// Check if it's a keyword that ends the key specification
		if check_keyword(p, "INTO") ||
		   check_keyword(p, "ASSIGNING") ||
		   check_keyword(p, "TRANSPORTING") ||
		   check_keyword(p, "USING") {
			break
		}

		// Save parser state to check for named component
		saved_prev := p.prev_tok
		saved_curr := p.curr_tok
		saved_pos := p.l.pos
		saved_read_pos := p.l.read_pos
		saved_ch := p.l.ch

		name_tok := advance_token(p)

		// Check if next token is = (named component)
		if p.curr_tok.kind == .Eq {
			advance_token(p) // consume =
			value := parse_expr(p)

			named_arg := ast.new(
				ast.Named_Arg,
				lexer.TextRange{name_tok.range.start, value.range.end},
			)
			named_arg.name = ast.new_ident(name_tok)
			named_arg.value = value
			named_arg.derived_expr = named_arg
			append(&key.components, named_arg)
		} else {
			// Not a named component - could be a simple key reference
			// Restore and break
			p.prev_tok = saved_prev
			p.curr_tok = saved_curr
			p.l.pos = saved_pos
			p.l.read_pos = saved_read_pos
			p.l.ch = saved_ch
			break
		}
	}

	return key
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

parse_delete_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	delete_tok := expect_keyword_token(p, "DELETE")
	stmt := ast.new(ast.Delete_Stmt, delete_tok.range)

	// Check for ADJACENT DUPLICATES
	if check_keyword(p, "ADJACENT") {
		advance_token(p) // consume ADJACENT
		expect_keyword_token(p, "DUPLICATES")
		expect_keyword_token(p, "FROM")
		stmt.kind = .Adjacent_Duplicates
		stmt.target = parse_expr(p)

		// Parsing other clauses for ADJACENT DUPLICATES can be added here
		// For now we just check for optional comparing if strictness needed
		if check_keyword(p, "COMPARING") {
			// consume for now to avoid error, as it's common
			advance_token(p)
			for p.curr_tok.kind != .Period {
				advance_token(p)
			}
		}
	} else {
		stmt.target = parse_expr(p)

		if check_keyword(p, "WHERE") {
			advance_token(p) // consume WHERE
			stmt.kind = .Where
			// Use parse_logical_expr to properly handle comparisons like gs1_es = lv_obj_del
			stmt.where_cond = parse_logical_expr(p)
		} else if check_keyword(p, "INDEX") {
			advance_token(p) // consume INDEX
			stmt.kind = .Index
			stmt.index_expr = parse_expr(p)
		}
	}

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

// parse_select_stmt parses a SELECT Open SQL statement
// Syntax variations:
// - SELECT [SINGLE] fields FROM table [INTO target] [WHERE cond] [ORDER BY cols] [UP TO n ROWS].
// - SELECT [SINGLE] * FROM table [AS alias] [INTO target] [WHERE cond].
// - SELECT FROM table [AS alias] FIELDS field_list [WHERE cond] [INTO target].
// - SELECT ... INNER JOIN ... ON ... [WHERE cond] [INTO target].
// - SELECT ... FOR ALL ENTRIES IN itab WHERE ... [INTO target].
// - SELECT ... GROUP BY cols HAVING cond [INTO target].
parse_select_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	select_tok := expect_keyword_token(p, "SELECT")
	
	stmt := ast.new(ast.Select_Stmt, select_tok.range)
	stmt.fields = make([dynamic]^ast.Expr)
	stmt.joins = make([dynamic]^ast.Select_Join)
	stmt.order_by = make([dynamic]^ast.Expr)
	stmt.group_by = make([dynamic]^ast.Expr)
	stmt.body = make([dynamic]^ast.Stmt)
	
	// Check for SINGLE modifier
	if check_keyword(p, "SINGLE") {
		advance_token(p)
		stmt.is_single = true
	}
	
	// Parse field list or FROM keyword (for SELECT FROM table FIELDS ...)
	if !check_keyword(p, "FROM") {
		parse_select_field_list(p, stmt)
	}
	
	// Parse FROM clause
	if check_keyword(p, "FROM") {
		advance_token(p)
		stmt.from_table = parse_select_table_ref(p)
		
		// Check for AS alias
		if check_keyword(p, "AS") {
			advance_token(p)
			alias_tok := expect_token(p, .Ident)
			stmt.from_alias = ast.new_ident(alias_tok)
		}
		
		// Parse optional JOINs
		parse_select_joins(p, stmt)
	}
	
	// Parse FIELDS clause (alternative field list position)
	if check_keyword(p, "FIELDS") {
		advance_token(p)
		parse_select_field_list(p, stmt)
	}
	
	// Parse remaining clauses in any order
	parse_select_clauses(p, stmt)
	
	// Check if this is a SELECT loop (no SINGLE and has ENDSELECT)
	// vs. a SELECT single/INTO TABLE (ends with period)
	if p.curr_tok.kind == .Period {
		period_tok := expect_token(p, .Period)
		stmt.range.end = period_tok.range.end
	} else {
		// This is a SELECT loop - parse body until ENDSELECT
		for p.curr_tok.kind != .EOF {
			if check_keyword(p, "ENDSELECT") {
				break
			}
			inner_stmt := parse_stmt(p)
			if inner_stmt != nil {
				append(&stmt.body, inner_stmt)
			}
		}
		
		endselect_tok := expect_keyword_token(p, "ENDSELECT")
		period_tok := expect_token(p, .Period)
		stmt.range.end = period_tok.range.end
		_ = endselect_tok
	}
	
	stmt.derived_stmt = stmt
	return stmt
}

// parse_select_field_list parses the field list in a SELECT statement
parse_select_field_list :: proc(p: ^Parser, stmt: ^ast.Select_Stmt) {
	for p.curr_tok.kind != .EOF && !check_keyword(p, "FROM") && !check_keyword(p, "INTO") {
		field := parse_select_field_expr(p)
		if field != nil {
			append(&stmt.fields, field)
		}
		
		// Check for comma or continue if next is still a field
		if allow_token(p, .Comma) {
			continue
		}
		
		// If next is a keyword that ends field list, break
		if check_keyword(p, "FROM") || check_keyword(p, "INTO") || check_keyword(p, "WHERE") ||
		   check_keyword(p, "ORDER") || check_keyword(p, "GROUP") || check_keyword(p, "FOR") ||
		   check_keyword(p, "UP") || check_keyword(p, "HAVING") {
			break
		}
		
		// If next token looks like another field expression, continue without comma
		if p.curr_tok.kind == .Ident || p.curr_tok.kind == .Star {
			continue
		}
		
		break
	}
}

// parse_select_field_expr parses a single field expression in SELECT
// This can be: *, field, table~field, alias~field, or aggregation functions like SUM(field)
parse_select_field_expr :: proc(p: ^Parser) -> ^ast.Expr {
	// Check for * (select all)
	if p.curr_tok.kind == .Star {
		star_tok := advance_token(p)
		star := ast.new(ast.Basic_Lit, star_tok.range)
		star.tok = star_tok
		star.derived_expr = star
		return star
	}
	
	// Parse field expression with optional alias (AS alias)
	expr := parse_select_single_field(p)
	
	// Check for AS alias for the field
	if check_keyword(p, "AS") {
		advance_token(p)
		// Parse alias name - this becomes a named arg
		alias_tok := expect_token(p, .Ident)
		alias_ident := ast.new_ident(alias_tok)
		
		named := ast.new(ast.Named_Arg, lexer.TextRange{expr.range.start, alias_tok.range.end})
		named.name = alias_ident
		named.value = expr
		named.derived_expr = named
		return named
	}
	
	return expr
}

// parse_select_single_field parses a single field (with possible aggregation)
parse_select_single_field :: proc(p: ^Parser) -> ^ast.Expr {
	// Check for aggregate functions: SUM, COUNT, AVG, MIN, MAX
	if p.curr_tok.kind == .Ident {
		upper := to_upper(p.keyword_buffer[:], p.curr_tok.lit)
		if upper == "SUM" || upper == "COUNT" || upper == "AVG" || upper == "MIN" || upper == "MAX" {
			func_tok := advance_token(p)
			func_ident := ast.new_ident(func_tok)
			
			// Expect ( arg )
			expect_token(p, .LParen)
			
			// Check for * in COUNT(*)
			arg: ^ast.Expr
			if p.curr_tok.kind == .Star {
				star_tok := advance_token(p)
				star := ast.new(ast.Basic_Lit, star_tok.range)
				star.tok = star_tok
				star.derived_expr = star
				arg = star
			} else {
				arg = parse_select_single_field(p)
			}
			
			rparen_tok := expect_token(p, .RParen)
			
			call := ast.new(ast.Call_Expr, lexer.TextRange{func_tok.range.start, rparen_tok.range.end})
			call.expr = func_ident
			args := make([]^ast.Expr, 1)
			args[0] = arg
			call.args = args
			call.derived_expr = call
			return call
		}
	}
	
	// Parse regular field expression (may include table~field)
	first_tok := expect_token(p, .Ident)
	expr: ^ast.Expr = ast.new_ident(first_tok)
	
	// Check for ~ (table~field notation)
	if p.curr_tok.kind == .Tilde {
		tilde_tok := advance_token(p)
		field_tok := expect_token(p, .Ident)
		field_ident := ast.new_ident(field_tok)
		
		sel := ast.new(ast.Selector_Expr, lexer.TextRange{first_tok.range.start, field_tok.range.end})
		sel.expr = expr
		sel.op = tilde_tok
		sel.field = field_ident
		sel.derived_expr = sel
		expr = sel
	}
	
	return expr
}

// parse_select_table_ref parses a table reference in FROM clause
parse_select_table_ref :: proc(p: ^Parser) -> ^ast.Expr {
	table_tok := expect_token(p, .Ident)
	return ast.new_ident(table_tok)
}

// parse_select_joins parses JOIN clauses
parse_select_joins :: proc(p: ^Parser, stmt: ^ast.Select_Stmt) {
	for p.curr_tok.kind != .EOF {
		join_kind: ast.Select_Join_Kind
		
		if check_keyword(p, "INNER") {
			advance_token(p)
			expect_keyword_token(p, "JOIN")
			join_kind = .Inner
		} else if check_keyword(p, "LEFT") {
			advance_token(p)
			if check_keyword(p, "OUTER") {
				advance_token(p)
			}
			expect_keyword_token(p, "JOIN")
			join_kind = .Left_Outer
		} else if check_keyword(p, "RIGHT") {
			advance_token(p)
			if check_keyword(p, "OUTER") {
				advance_token(p)
			}
			expect_keyword_token(p, "JOIN")
			join_kind = .Right_Outer
		} else if check_keyword(p, "JOIN") {
			// Plain JOIN is treated as INNER JOIN
			advance_token(p)
			join_kind = .Inner
		} else {
			break
		}
		
		// Parse joined table
		join_table := parse_select_table_ref(p)
		
		// Check for AS alias
		join_alias: ^ast.Ident = nil
		if check_keyword(p, "AS") {
			advance_token(p)
			alias_tok := expect_token(p, .Ident)
			join_alias = ast.new_ident(alias_tok)
		}
		
		// Parse ON condition
		on_cond: ^ast.Expr = nil
		if check_keyword(p, "ON") {
			advance_token(p)
			on_cond = parse_select_on_condition(p)
		}
		
		join := new(ast.Select_Join)
		join.kind = join_kind
		join.table = join_table
		join.alias = join_alias
		join.on_cond = on_cond
		join.range = join_table.range
		append(&stmt.joins, join)
	}
}

// parse_select_on_condition parses an ON condition in a JOIN clause
parse_select_on_condition :: proc(p: ^Parser) -> ^ast.Expr {
	return parse_select_logical_expr(p)
}

// parse_select_logical_expr parses a logical expression in SELECT context
// This handles AND, OR, and comparison operators
parse_select_logical_expr :: proc(p: ^Parser) -> ^ast.Expr {
	left := parse_select_comparison_expr(p)
	
	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "AND") {
			op_tok := advance_token(p)
			right := parse_select_comparison_expr(p)
			
			bin := ast.new(ast.Binary_Expr, lexer.TextRange{left.range.start, right.range.end})
			bin.left = left
			bin.op = op_tok
			bin.right = right
			bin.derived_expr = bin
			left = bin
		} else if check_keyword(p, "OR") {
			op_tok := advance_token(p)
			right := parse_select_comparison_expr(p)
			
			bin := ast.new(ast.Binary_Expr, lexer.TextRange{left.range.start, right.range.end})
			bin.left = left
			bin.op = op_tok
			bin.right = right
			bin.derived_expr = bin
			left = bin
		} else {
			break
		}
	}
	
	return left
}

// parse_select_comparison_expr parses a comparison in SELECT context
parse_select_comparison_expr :: proc(p: ^Parser) -> ^ast.Expr {
	left := parse_select_operand(p)
	
	// Check for comparison operators: =, <>, <, >, <=, >=, EQ, NE, LT, GT, LE, GE
	if p.curr_tok.kind == .Eq || p.curr_tok.kind == .Lt || p.curr_tok.kind == .Gt ||
	   p.curr_tok.kind == .Le || p.curr_tok.kind == .Ge || p.curr_tok.kind == .Ne {
		op_tok := advance_token(p)
		right := parse_select_operand(p)
		
		bin := ast.new(ast.Binary_Expr, lexer.TextRange{left.range.start, right.range.end})
		bin.left = left
		bin.op = op_tok
		bin.right = right
		bin.derived_expr = bin
		return bin
	}
	
	// Check for keyword comparison operators
	if check_keyword(p, "EQ") || check_keyword(p, "NE") || check_keyword(p, "LT") ||
	   check_keyword(p, "GT") || check_keyword(p, "LE") || check_keyword(p, "GE") {
		op_tok := advance_token(p)
		right := parse_select_operand(p)
		
		bin := ast.new(ast.Binary_Expr, lexer.TextRange{left.range.start, right.range.end})
		bin.left = left
		bin.op = op_tok
		bin.right = right
		bin.derived_expr = bin
		return bin
	}
	
	return left
}

// parse_select_operand parses an operand in a SELECT condition
parse_select_operand :: proc(p: ^Parser) -> ^ast.Expr {
	// Check for @ prefix (host variable reference)
	if p.curr_tok.kind == .At {
		at_tok := advance_token(p)
		
		// Check for DATA(...) inline declaration
		if check_keyword(p, "DATA") {
			return parse_data_inline_expr(p)
		}
		
		// Parse the variable reference (may include structure access with -)
		inner := parse_select_field_with_dash(p)
		
		// Wrap in a unary expression with @ operator
		unary := ast.new(ast.Unary_Expr, lexer.TextRange{at_tok.range.start, inner.range.end})
		unary.op = at_tok
		unary.expr = inner
		unary.derived_expr = unary
		return unary
	}
	
	// Check for string literal
	if p.curr_tok.kind == .String {
		str_tok := advance_token(p)
		lit := ast.new(ast.Basic_Lit, str_tok.range)
		lit.tok = str_tok
		lit.derived_expr = lit
		return lit
	}
	
	// Check for number literal
	if p.curr_tok.kind == .Number {
		num_tok := advance_token(p)
		lit := ast.new(ast.Basic_Lit, num_tok.range)
		lit.tok = num_tok
		lit.derived_expr = lit
		return lit
	}
	
	// Parse field reference (table~field or just field, may include structure access with -)
	return parse_select_field_with_dash(p)
}

// parse_select_field_with_dash parses a field expression that may include structure access with -
// e.g., entry_tab-carrid, wa-field1-field2
parse_select_field_with_dash :: proc(p: ^Parser) -> ^ast.Expr {
	expr := parse_select_single_field(p)
	
	// Check for - (structure access)
	for p.curr_tok.kind == .Minus {
		// Check if next token is an identifier (not a space followed by something else)
		// The - must be immediately followed by an identifier for structure access
		minus_tok := advance_token(p)
		
		// If next token is an identifier without space, it's structure access
		if p.curr_tok.kind == .Ident && !lexer.have_space_between(minus_tok, p.curr_tok) {
			field_tok := advance_token(p)
			field_ident := ast.new_ident(field_tok)
			
			sel := ast.new(ast.Selector_Expr, lexer.TextRange{expr.range.start, field_tok.range.end})
			sel.expr = expr
			sel.op = minus_tok
			sel.field = field_ident
			sel.derived_expr = sel
			expr = sel
		} else {
			// It's a subtraction operator, put the minus back by creating a binary expression
			// Actually, we can't put it back, so this might cause issues.
			// For now, let's assume structure access requires no space
			// This case shouldn't happen in valid SELECT statements
			break
		}
	}
	
	return expr
}

// parse_select_clauses parses the remaining clauses of a SELECT statement
parse_select_clauses :: proc(p: ^Parser, stmt: ^ast.Select_Stmt) {
	for p.curr_tok.kind != .EOF && p.curr_tok.kind != .Period {
		if check_keyword(p, "FOR") {
			advance_token(p)
			expect_keyword_token(p, "ALL")
			expect_keyword_token(p, "ENTRIES")
			expect_keyword_token(p, "IN")
			stmt.for_all_entries = parse_select_operand(p)
		} else if check_keyword(p, "WHERE") {
			advance_token(p)
			stmt.where_cond = parse_select_logical_expr(p)
		} else if check_keyword(p, "ORDER") {
			advance_token(p)
			expect_keyword_token(p, "BY")
			parse_select_order_by(p, stmt)
		} else if check_keyword(p, "GROUP") {
			advance_token(p)
			expect_keyword_token(p, "BY")
			parse_select_group_by(p, stmt)
		} else if check_keyword(p, "HAVING") {
			advance_token(p)
			stmt.having_cond = parse_select_logical_expr(p)
		} else if check_keyword(p, "INTO") {
			advance_token(p)
			parse_select_into(p, stmt)
		} else if check_keyword(p, "UP") {
			advance_token(p)
			expect_keyword_token(p, "TO")
			stmt.up_to_rows = parse_expr(p)
			expect_keyword_token(p, "ROWS")
		} else if check_keyword(p, "ENDSELECT") {
			// End of SELECT loop - don't consume, let outer handle it
			break
		} else {
			// Unknown clause, break
			break
		}
	}
}

// parse_select_order_by parses the ORDER BY clause
parse_select_order_by :: proc(p: ^Parser, stmt: ^ast.Select_Stmt) {
	// Check for PRIMARY KEY shortcut
	if check_keyword(p, "PRIMARY") {
		pk_tok := advance_token(p)
		expect_keyword_token(p, "KEY")
		pk_ident := ast.new_ident(pk_tok)
		pk_ident.name = "PRIMARY KEY"
		append(&stmt.order_by, pk_ident)
		return
	}
	
	for p.curr_tok.kind != .EOF {
		field := parse_select_single_field(p)
		append(&stmt.order_by, field)
		
		// Check for ASCENDING/DESCENDING
		if check_keyword(p, "ASCENDING") || check_keyword(p, "DESCENDING") {
			advance_token(p)
		}
		
		// Check for comma
		if allow_token(p, .Comma) {
			continue
		}
		
		// Check for next clause keyword
		if check_keyword(p, "INTO") || check_keyword(p, "WHERE") || check_keyword(p, "UP") ||
		   check_keyword(p, "FOR") || check_keyword(p, "GROUP") || check_keyword(p, "HAVING") {
			break
		}
		
		// If next token looks like another field, continue without comma
		if p.curr_tok.kind == .Ident {
			continue
		}
		
		break
	}
}

// parse_select_group_by parses the GROUP BY clause
parse_select_group_by :: proc(p: ^Parser, stmt: ^ast.Select_Stmt) {
	for p.curr_tok.kind != .EOF {
		field := parse_select_single_field(p)
		append(&stmt.group_by, field)
		
		// Check for comma
		if allow_token(p, .Comma) {
			continue
		}
		
		// Check for next clause keyword
		if check_keyword(p, "INTO") || check_keyword(p, "WHERE") || check_keyword(p, "HAVING") ||
		   check_keyword(p, "ORDER") || check_keyword(p, "UP") {
			break
		}
		
		// If next token looks like another field, continue without comma
		if p.curr_tok.kind == .Ident {
			continue
		}
		
		break
	}
}

// parse_select_into parses the INTO clause of a SELECT statement
parse_select_into :: proc(p: ^Parser, stmt: ^ast.Select_Stmt) {
	// Check for TABLE keyword
	if check_keyword(p, "TABLE") {
		advance_token(p)
		stmt.into_kind = .Table
	} else if check_keyword(p, "CORRESPONDING") {
		advance_token(p)
		expect_keyword_token(p, "FIELDS")
		expect_keyword_token(p, "OF")
		if check_keyword(p, "TABLE") {
			advance_token(p)
		}
		stmt.into_kind = .Corresponding
	} else {
		stmt.into_kind = .Single
	}
	
	// Parse target - may have @ prefix
	if p.curr_tok.kind == .At {
		advance_token(p)
		
		// Check for DATA(...) inline declaration
		if check_keyword(p, "DATA") {
			stmt.into_target = parse_data_inline_expr(p)
		} else {
			// Regular variable reference
			stmt.into_target = parse_expr(p)
		}
	} else {
		stmt.into_target = parse_expr(p)
	}
}
