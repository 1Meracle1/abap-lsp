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

parse_file :: proc(p: ^Parser, file: ^ast.File, allocator := context.allocator) {
	context.allocator = allocator

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
	// ABAP allows empty statements (a lone period).
	if p.curr_tok.kind == .Period {
		period_tok := advance_token(p)
		return ast.new(ast.Empty_Stmt, period_tok.range)
	}

	if len(p.curr_tok.lit) > 0 && len(p.curr_tok.lit) < len(p.keyword_buffer) {
		keyword := to_upper(p.keyword_buffer[:], p.curr_tok.lit)
		switch keyword {
		case "DATA":
			return parse_data_decl(p)
		case "STATICS":
			return parse_statics_decl(p)
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
		case "CREATE":
			if check_keyword_ahead(p, "OBJECT") {
				return parse_create_object_stmt(p)
			}
			if check_keyword_ahead(p, "DATA") {
				return parse_create_data_stmt(p)
			}
		case "IF":
			return parse_if_stmt(p)
		case "TRY":
			return parse_try_stmt(p)
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
		case "ASSIGN":
			return parse_assign_field_symbol_stmt(p)
		case "LEAVE":
			return parse_leave_stmt(p)
		case "GET":
			if check_keyword_ahead(p, "BIT") {
				return parse_get_bit_stmt(p)
			}
			if check_keyword_ahead(p, "TIME") {
				return parse_get_stmt(p)
			}
			if check_keyword_ahead(p, "BADI") {
				return parse_get_badi_stmt(p)
			}
		case "CONVERT":
			if check_keyword_ahead(p, "DATE") {
				return parse_convert_date_time_to_time_stamp_stmt(p)
			}
			if check_convert_time_stamp_into_date_time_prefix(p) {
				return parse_convert_time_stamp_to_date_time_stmt(p)
			}
		case "CONTINUE":
			return parse_continue_stmt(p)
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
		case "FREE":
			// free( ). is a method call (callee "free"); FREE dobj. / FREE: ... is the memory statement.
			if free_at_stmt_start_is_memory_stmt(p) {
				return parse_free_stmt(p)
			}
		case "REFRESH":
			return parse_refresh_stmt(p)
		case "UNASSIGN":
			return parse_unassign_stmt(p)
		case "MOVE":
			move_kw := p.curr_tok
			if check_hyphenated_keyword(p, "MOVE", "CORRESPONDING") {
				return parse_move_corresponding_stmt(p, move_kw)
			}
			// MOVE-something that is not MOVE-CORRESPONDING (e.g. typo MOVE-CORRESPONDING1)
			if move_has_invalid_hyphenated_suffix(p) {
				start_tok := p.curr_tok
				end_tok := skip_to_statement_end(p)
				error(
					p,
					lexer.TextRange{start_tok.range.start, end_tok.range.end},
					"unexpected statement",
				)
				return ast.new(ast.Bad_Decl, start_tok, end_tok)
			}
			return parse_move_stmt(p, move_kw)
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
			} else if check_keyword_ahead(p, "REPORT") {
				return parse_read_report_stmt(p)
			}
		case "DESCRIBE":
			if check_keyword_ahead(p, "TABLE") {
				return parse_describe_table_stmt(p)
			}
		case "DO":
			return parse_do_stmt(p)
		case "EXIT":
			return parse_exit_stmt(p)
		case "WRITE":
			return parse_write_stmt(p)
		case "CONDENSE":
			return parse_condense_stmt(p)
		case "TRANSLATE":
			return parse_translate_stmt(p)
		case "SPLIT":
			return parse_split_stmt(p)
		case "CONCATENATE":
			return parse_concatenate_stmt(p)
		case "REPLACE":
			return parse_replace_stmt(p)
		case "COMMIT":
			return parse_commit_work_stmt(p)
		case "OPEN":
			if check_keyword_ahead(p, "CURSOR") {
				return parse_open_cursor_stmt(p)
			}
		case "FETCH":
			return parse_fetch_cursor_stmt(p)
		case "SELECT":
			return parse_select_stmt(p)
		case "RAISE":
			return parse_raise_stmt(p)
		case "CHECK":
			return parse_check_stmt(p)
		case "RETURN":
			return parse_return_stmt(p)
		case "ROLLBACK":
			return parse_rollback_work_stmt(p)
		case "ASSERT":
			return parse_assert_stmt(p)
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
	end_tok := skip_to_statement_end(p)
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
// - RANGE OF
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

	// Check for RANGE OF (selection table / ranges type)
	if check_keyword(p, "RANGE") {
		return parse_range_type(p)
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

// parse_optional_length_decimals parses optional LENGTH / DECIMALS clauses after
// elementary types (e.g. TYPE p LENGTH 7 DECIMALS 0). Clauses may appear in either order.
// If first_length is non-nil, the first LENGTH expression is stored at *first_length.
parse_optional_length_decimals :: proc(p: ^Parser, first_length: ^^ast.Expr = nil) {
	for {
		if check_keyword(p, "LENGTH") {
			advance_token(p)
			e := parse_expr(p)
			if first_length != nil && first_length^ == nil {
				first_length^ = e
			}
			continue
		}
		if check_keyword(p, "DECIMALS") {
			advance_token(p)
			parse_expr(p)
			continue
		}
		break
	}
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

// parse_range_type parses: RANGE OF type
parse_range_type :: proc(p: ^Parser) -> ^ast.Expr {
	range_tok := expect_keyword_token(p, "RANGE")
	expect_keyword_token(p, "OF")

	elem := parse_type_expr(p)

	range_type := ast.new(
		ast.Range_Type,
		lexer.TextRange{range_tok.range.start, p.prev_tok.range.end},
	)
	range_type.elem = elem
	range_type.derived_expr = range_type
	return range_type
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

	// Line type after OF is optional (e.g. TYPE STANDARD TABLE for a generic table type)
	elem: ^ast.Expr = nil
	if check_keyword(p, "OF") {
		advance_token(p)
		elem = parse_type_expr(p)
	}

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

// parse_table_key_components parses key field names after KEY (or after COMPONENTS);
// ABAP allows comma- or space-separated lists (e.g. WITH UNIQUE KEY a b c).
parse_table_key_components :: proc(p: ^Parser, key: ^ast.Table_Key) {
	for p.curr_tok.kind == .Ident {
		// Stop if a statement/type clause starts rather than another field name.
		// END/BEGIN: otherwise WITH KEY comp1 ... END OF name can treat END as a key component
		// when the comma after the last component is omitted before a new line.
		if check_keyword(p, "WITH") ||
		   check_keyword(p, "VALUE") ||
		   check_keyword(p, "LENGTH") ||
		   check_keyword(p, "DECIMALS") ||
		   check_keyword(p, "READ") ||
		   check_keyword(p, "END") ||
		   check_keyword(p, "BEGIN") {
			break
		}

		field_tok := advance_token(p)
		field_ident := ast.new_ident(field_tok)
		append(&key.components, field_ident)

		if p.curr_tok.kind == .Comma {
			// Comma either separates key components (KEY a, b) or ends the type before the
			// next struct field (... KEY k, / newline / next_field TYPE ...). Do not consume
			// a field-separator comma or we swallow the next member name as part of the key.
			saved_prev := p.prev_tok
			saved_curr := p.curr_tok
			saved_pos := p.l.pos
			saved_read_pos := p.l.read_pos
			saved_ch := p.l.ch

			advance_token(p) // comma
			if p.curr_tok.kind != .Ident {
				p.prev_tok = saved_prev
				p.curr_tok = saved_curr
				p.l.pos = saved_pos
				p.l.read_pos = saved_read_pos
				p.l.ch = saved_ch
				break
			}
			advance_token(p) // possible struct field name
			is_struct_field := check_keyword(p, "TYPE") || check_keyword(p, "LIKE")
			p.prev_tok = saved_prev
			p.curr_tok = saved_curr
			p.l.pos = saved_pos
			p.l.read_pos = saved_read_pos
			p.l.ch = saved_ch
			if is_struct_field {
				break
			}
			advance_token(p) // comma between key fields
			continue
		}
		// Space-separated key fields (no comma)
		if p.curr_tok.kind == .Ident {
			continue
		}
		break
	}
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

	// Named secondary key: ... KEY key_name COMPONENTS comp1 comp2 ...
	// vs primary table key: ... KEY comp1 comp2 ...
	if p.curr_tok.kind == .Ident {
		saved_prev := p.prev_tok
		saved_curr := p.curr_tok
		saved_pos := p.l.pos
		saved_read_pos := p.l.read_pos
		saved_ch := p.l.ch

		name_tok := advance_token(p)
		if check_keyword(p, "COMPONENTS") {
			key.name = ast.new_ident(name_tok)
			expect_keyword_token(p, "COMPONENTS")
			parse_table_key_components(p, key)
			return key
		}

		p.prev_tok = saved_prev
		p.curr_tok = saved_curr
		p.l.pos = saved_pos
		p.l.read_pos = saved_read_pos
		p.l.ch = saved_ch
	}

	// Optional leading COMPONENTS for aliases (e.g. KEY COMPONENTS f1 f2)
	if check_keyword(p, "COMPONENTS") {
		advance_token(p)
	}

	parse_table_key_components(p, key)
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

// True when current token starts MOVE-x where x is not CORRESPONDING (parser state unchanged).
move_has_invalid_hyphenated_suffix :: proc(p: ^Parser) -> bool {
	if !check_keyword(p, "MOVE") {
		return false
	}
	saved_prev := p.prev_tok
	saved_curr := p.curr_tok
	saved_pos := p.l.pos
	saved_read_pos := p.l.read_pos
	saved_ch := p.l.ch

	advance_token(p) // MOVE
	if p.curr_tok.kind != .Minus || lexer.have_space_between(p.prev_tok, p.curr_tok) {
		p.prev_tok = saved_prev
		p.curr_tok = saved_curr
		p.l.pos = saved_pos
		p.l.read_pos = saved_read_pos
		p.l.ch = saved_ch
		return false
	}
	advance_token(p) // -
	invalid := !check_keyword(p, "CORRESPONDING")

	p.prev_tok = saved_prev
	p.curr_tok = saved_curr
	p.l.pos = saved_pos
	p.l.read_pos = saved_read_pos
	p.l.ch = saved_ch
	return invalid
}

// Data object chain may use +off(len) unless an object ref (-> or =>) appears.
expr_allows_abap_substring_offset :: proc(expr: ^ast.Expr) -> bool {
	e := expr
	for e != nil {
		#partial switch x in e.derived_expr {
		case ^ast.Selector_Expr:
			if x.op.kind == .Arrow || x.op.kind == .FatArrow {
				return false
			}
			e = x.expr
		case ^ast.Index_Expr:
			e = x.expr
		case ^ast.Ident:
			return true
		case:
			return false
		}
	}
	return false
}

expr_is_bare_ident :: proc(expr: ^ast.Expr) -> bool {
	_, ok := expr.derived_expr.(^ast.Ident)
	return ok
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
	// LHS must not consume `=` as a comparison (`lv = 1` is assignment, not a boolean expr).
	lhs := parse_concat_expr(p)

	if p.curr_tok.kind == .Eq || p.curr_tok.kind == .QuestionEq {
		op := advance_token(p)
		rhs := parse_logical_expr(p)
		skip_pragma(p)
		period_tok := expect_token(p, .Period)

		assign_stmt := ast.new(ast.Assign_Stmt, start_tok, period_tok)
		assign_stmt.lhs = make([]^ast.Expr, 1)
		assign_stmt.lhs[0] = lhs
		assign_stmt.op = op
		assign_stmt.rhs = make([]^ast.Expr, 1)
		assign_stmt.rhs[0] = rhs
		return assign_stmt
	}

	if p.curr_tok.kind != .Period {
		// Classic macro call: one identifier as name, then one or more actual parameters until '.'.
		if _, ok := lhs.derived_expr.(^ast.Ident); ok {
			args: [dynamic]^ast.Expr
			macro_arg_i := 0
			for p.curr_tok.kind != .Period && p.curr_tok.kind != .EOF {
				macro_arg_i += 1
				if macro_arg_i > 512 {
					error(p, start_tok.range, "macro invocation too long or malformed")
					end_tok := skip_to_statement_end(p)
					return ast.new(ast.Bad_Decl, start_tok, end_tok)
				}
				off_before := p.curr_tok.range.start
				append(&args, parse_expr(p))
				if p.curr_tok.range.start == off_before &&
				   p.curr_tok.kind != .Period &&
				   p.curr_tok.kind != .EOF {
					error(
						p,
						p.curr_tok.range,
						"expected expression in macro invocation",
					)
					break
				}
			}
			if p.curr_tok.kind != .Period {
				end_tok := skip_to_statement_end(p)
				error(p, lexer.TextRange{start_tok.range.start, end_tok.range.end}, "expected '.' after macro invocation")
				return ast.new(ast.Bad_Decl, start_tok, end_tok)
			}
			period_tok := advance_token(p)
			macro_stmt := ast.new(ast.Macro_Call_Stmt, start_tok, period_tok)
			macro_stmt.name = lhs
			macro_stmt.args = args[:]
			return macro_stmt
		}
		end_tok := skip_to_statement_end(p)
		error(p, lexer.TextRange{start_tok.range.start, end_tok.range.end}, "unexpected tokens after expression")
		bad_decl := ast.new(ast.Bad_Decl, start_tok, end_tok)
		return bad_decl
	}

	period_tok := advance_token(p)
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

skip_to_statement_end :: proc(p: ^Parser) -> lexer.Token {
	line_count := p.l.line_count
	last_tok := p.curr_tok
	for p.curr_tok.kind != .EOF {
		tok := advance_token(p)
		last_tok = tok
		if tok.kind == .Period || p.l.line_count > line_count {
			return tok
		}
	}
	return last_tok
}

parse_expr :: proc(p: ^Parser) -> ^ast.Expr {
	// General expressions allow relational and logical operators (e.g. `a = b`, `x AND y`)
	// as in assignment RHS, COND conditions, etc. Comparison operands use parse_concat_expr
	// so string `&`/concatenation binds tighter than `=`.
	return parse_logical_expr(p)
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
		// Do not commit '&' until we know the RHS exists; otherwise stmt-level code can
		// see '.' (e.g. "lv = a &.") and mis-parse as "lv = a" + '.' → Expr_Stmt or wrong macro shape.
		saved_prev := p.prev_tok
		saved_curr := p.curr_tok
		saved_pos := p.l.pos
		saved_read_pos := p.l.read_pos
		saved_ch := p.l.ch
		saved_line_start := p.l.line_start
		saved_line_count := p.l.line_count

		op := advance_token(p)
		right := parse_additive_expr(p)
		// Adjacent && (two & with no operand between) is an empty concatenand — skip the second &.
		if right == nil && p.curr_tok.kind == .Ampersand {
			advance_token(p)
			right = parse_additive_expr(p)
		}
		if right == nil {
			p.prev_tok = saved_prev
			p.curr_tok = saved_curr
			p.l.pos = saved_pos
			p.l.read_pos = saved_read_pos
			p.l.ch = saved_ch
			p.l.line_start = saved_line_start
			p.l.line_count = saved_line_count
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
	return parse_atom_expr(p, parse_operand(p), true)
}

parse_atom_expr :: proc(p: ^Parser, value: ^ast.Expr, allow_substring: bool = true) -> ^ast.Expr {
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
			field_expr: ^ast.Expr
			end_at: int
			if p.curr_tok.kind == .LParen && !lexer.have_space_between(p.prev_tok, p.curr_tok) {
				lparen_tok := advance_token(p)
				inner := parse_expr(p)
				rparen_tok := expect_token(p, .RParen)
				paren := ast.new(
					ast.Paren_Expr,
					lexer.TextRange{lparen_tok.range.start, rparen_tok.range.end},
				)
				paren.expr = inner
				paren.derived_expr = paren
				field_expr = paren
				end_at = rparen_tok.range.end
			} else {
				// Handle TEXT-nnn text symbol references where nnn is a number
				// Also allow numbers as selectors for error resilience
				field_tok: lexer.Token
				if p.curr_tok.kind == .Ident || p.curr_tok.kind == .Number || p.curr_tok.kind == .Star {
					field_tok = advance_token(p)
				} else {
					field_tok = expect_token(p, .Ident) // Will error but still advance
				}
				field_ident := ast.new_ident(field_tok)
				if field_tok.kind == .Star {
					field_ident.name = "*"
				}
				field_expr = &field_ident.node
				end_at = field_tok.range.end
			}
			selector := ast.new(ast.Selector_Expr, lexer.TextRange{expr.range.start, end_at})
			selector.expr = expr
			selector.op = op
			selector.field = field_expr
			selector.derived_expr = selector
			expr = &selector.node
		case .Plus:
			// ABAP substring: dobj+off(len) or dobj+off(*) — '+' must touch the data object
			if !allow_substring ||
			   !expr_allows_abap_substring_offset(expr) ||
			   lexer.have_space_between(p.prev_tok, p.curr_tok) {
				break loop
			}
			saved_prev := p.prev_tok
			saved_curr := p.curr_tok
			saved_pos := p.l.pos
			saved_read_pos := p.l.read_pos
			saved_ch := p.l.ch
			saved_line_start := p.l.line_start
			saved_line_count := p.l.line_count

			advance_token(p) // +
			offset := parse_assign_subfield_component(p)
			if offset == nil ||
			   p.curr_tok.kind != .LParen ||
			   lexer.have_space_between(p.prev_tok, p.curr_tok) {
				p.prev_tok = saved_prev
				p.curr_tok = saved_curr
				p.l.pos = saved_pos
				p.l.read_pos = saved_read_pos
				p.l.ch = saved_ch
				p.l.line_start = saved_line_start
				p.l.line_count = saved_line_count
				break loop
			}

			advance_token(p) // (
			length_is_star := false
			length: ^ast.Expr
			if p.curr_tok.kind == .Star {
				length_is_star = true
				advance_token(p)
			} else {
				length = parse_assign_subfield_component(p)
			}
			rparen_tok := expect_token(p, .RParen)
			substr := ast.new(
				ast.Substring_Expr,
				lexer.TextRange{expr.range.start, rparen_tok.range.end},
			)
			substr.expr = expr
			substr.offset = offset
			substr.length = length
			substr.length_is_star = length_is_star
			expr = substr
		case .LParen:
			// Call expression - parentheses immediately after expression (no space)
			if lexer.have_space_between(p.prev_tok, p.curr_tok) {
				break loop
			}
			// ABAP substring dobj(len): only for a bare identifier (not struct-comp(len) or meth(...)).
			if allow_substring &&
			   expr_is_bare_ident(expr) &&
			   concatenate_has_substring_length(p) {
				advance_token(p) // (
				length_is_star := false
				length: ^ast.Expr
				if p.curr_tok.kind == .Star {
					length_is_star = true
					advance_token(p)
				} else {
					length = parse_assign_subfield_component(p)
				}
				rparen_tok := expect_token(p, .RParen)
				substr := ast.new(
					ast.Substring_Expr,
					lexer.TextRange{expr.range.start, rparen_tok.range.end},
				)
				substr.expr = expr
				substr.offset = nil
				substr.length = length
				substr.length_is_star = length_is_star
				expr = substr
				continue
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
		case .LBracket:
			// Table expression - square brackets for internal table access
			// Syntax: itab[ index ] or itab[ key = value ] / itab[ KEY key COMPONENTS ... ] — use logical expr so = and AND/OR parse.
			advance_token(p) // consume [

			index_expr, table_key_name, has_key_clause := parse_table_bracket_index_content(p)

			rbracket_tok := expect_token(p, .RBracket)

			table_expr := ast.new(
				ast.Index_Expr,
				lexer.TextRange{expr.range.start, rbracket_tok.range.end},
			)
			table_expr.expr = expr
			table_expr.index = index_expr
			table_expr.table_key_name = table_key_name
			table_expr.has_key_clause = has_key_clause
			table_expr.derived_expr = table_expr
			expr = table_expr
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

	// Modern method call expressions can still use ABAP parameter sections
	// inside parentheses. Flatten them by skipping the section keyword.
	if check_keyword(p, "EXPORTING") ||
	   check_keyword(p, "IMPORTING") ||
	   check_keyword(p, "CHANGING") ||
	   check_keyword(p, "RECEIVING") ||
	   check_keyword(p, "EXCEPTIONS") ||
	   check_keyword(p, "TABLES") {
		advance_token(p)
		return nil
	}

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

is_inline_data_expr_start :: proc(p: ^Parser) -> bool {
	if !check_keyword(p, "DATA") {
		return false
	}

	saved_prev := p.prev_tok
	saved_curr := p.curr_tok
	saved_pos := p.l.pos
	saved_read_pos := p.l.read_pos
	saved_ch := p.l.ch

	data_tok := advance_token(p)
	is_inline_data := p.curr_tok.kind == .LParen &&
		!lexer.have_space_between(data_tok, p.curr_tok)

	p.prev_tok = saved_prev
	p.curr_tok = saved_curr
	p.l.pos = saved_pos
	p.l.read_pos = saved_read_pos
	p.l.ch = saved_ch

	return is_inline_data
}

parse_operand :: proc(p: ^Parser) -> ^ast.Expr {
	#partial switch p.curr_tok.kind {
	case .Ident:
		if is_inline_data_expr_start(p) {
			return parse_data_inline_expr(p)
		}
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
	root_ident := ast.new_ident(tok)
	expr := &root_ident.node

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
			field_ident := ast.new_ident(field_tok)
			selector := ast.new(
				ast.Selector_Expr,
				lexer.TextRange{expr.range.start, field_tok.range.end},
			)
			selector.expr = expr
			selector.op = op
			selector.field = &field_ident.node
			selector.derived_expr = selector
			expr = &selector.node
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

	if check_keyword(p, "METHOD") {
		return parse_call_method_stmt(p, call_tok)
	}

	if check_keyword(p, "BADI") {
		return parse_call_badi_stmt(p, call_tok)
	}

	if check_keyword(p, "TRANSACTION") {
		return parse_call_transaction_stmt(p, call_tok)
	}

	if check_keyword(p, "TRANSFORMATION") {
		return parse_call_transformation_stmt(p, call_tok)
	}

	// CALL 'kernel_module' ... ID 'name' FIELD dobj ... (system C calls, directory APIs, etc.)
	if p.curr_tok.kind == .String {
		return parse_call_system_stmt(p, call_tok)
	}

	// For other CALL types, treat as expression statement for now
	expr := parse_expr(p)
	period_tok := expect_token(p, .Period)
	expr_stmt := ast.new(ast.Expr_Stmt, call_tok, period_tok)
	expr_stmt.expr = expr
	return expr_stmt
}

// skip_call_transaction_pragma_strings skips code-inspector / pragma fragments written as string literals
// after the transaction code (e.g. "#EC CI_USE_WANTED), same pattern as Open SQL JOIN clauses.
skip_call_transaction_pragma_strings :: proc(p: ^Parser) {
	for p.curr_tok.kind == .String {
		advance_token(p)
	}
}

// parse_call_transaction_stmt parses CALL TRANSACTION with optional authority, USING, MODE clauses.
parse_call_transaction_stmt :: proc(p: ^Parser, call_tok: lexer.Token) -> ^ast.Stmt {
	expect_keyword_token(p, "TRANSACTION")
	transaction := parse_expr(p)
	skip_pragma(p)
	skip_call_transaction_pragma_strings(p)

	stmt := ast.new(ast.Call_Transaction_Stmt, call_tok.range)
	stmt.transaction = transaction
	stmt.authority = .Unspecified
	stmt.bdc_tab = nil
	stmt.mode = nil

	for p.curr_tok.kind != .Period && p.curr_tok.kind != .EOF {
		skip_pragma(p)
		skip_call_transaction_pragma_strings(p)

		if check_keyword(p, "WITH") {
			advance_token(p)
			if check_hyphenated_keyword(p, "AUTHORITY", "CHECK") {
				stmt.authority = .With
				continue
			}
			error(
				p,
				p.curr_tok.range,
				"expected AUTHORITY-CHECK after WITH in CALL TRANSACTION",
			)
			break
		}

		if check_keyword(p, "WITHOUT") {
			advance_token(p)
			if check_hyphenated_keyword(p, "AUTHORITY", "CHECK") {
				stmt.authority = .Without
				continue
			}
			error(
				p,
				p.curr_tok.range,
				"expected AUTHORITY-CHECK after WITHOUT in CALL TRANSACTION",
			)
			break
		}

		if check_keyword(p, "USING") {
			advance_token(p)
			stmt.bdc_tab = parse_expr(p)
			continue
		}

		if check_keyword(p, "MODE") {
			advance_token(p)
			stmt.mode = parse_expr(p)
			continue
		}

		break
	}

	period_tok := expect_token(p, .Period)
	stmt.range.end = period_tok.range.end
	return stmt
}

call_transformation_clause_starts_here :: proc(p: ^Parser) -> bool {
	return(
		check_keyword(p, "SOURCE") ||
		check_keyword(p, "RESULT") ||
		check_keyword(p, "OPTIONS") ||
		check_keyword(p, "PARAMETERS")
	)
}

parse_call_transformation_source_operand :: proc(p: ^Parser) -> ^ast.Expr {
	if check_keyword(p, "XML") ||
	   check_keyword(p, "ASXML") ||
	   check_keyword(p, "BINARY") {
		advance_token(p)
	}
	return parse_expr(p)
}

parse_call_transformation_result_roots :: proc(
	p: ^Parser,
	roots: ^[dynamic]^ast.Named_Arg,
) {
	for p.curr_tok.kind != .Period && p.curr_tok.kind != .EOF {
		skip_pragma(p)
		if call_transformation_clause_starts_here(p) {
			break
		}
		if p.curr_tok.kind != .Ident {
			break
		}

		param_name_tok := p.curr_tok
		advance_token(p)

		if p.curr_tok.kind != .Eq {
			error(
				p,
				p.curr_tok.range,
				"expected '=' after RESULT root '%s'",
				param_name_tok.lit,
			)
			break
		}
		advance_token(p)

		param_value := parse_expr(p)
		if param_value == nil {
			break
		}

		skip_pragma(p)

		named_arg := ast.new(
			ast.Named_Arg,
			lexer.TextRange{param_name_tok.range.start, param_value.range.end},
		)
		named_arg.name = ast.new_ident(param_name_tok)
		named_arg.value = param_value
		named_arg.derived_expr = named_arg

		append(roots, named_arg)
	}
}

parse_call_transformation_result_section :: proc(
	p: ^Parser,
	stmt: ^ast.Call_Transformation_Stmt,
) {
	if check_keyword(p, "XML") ||
	   check_keyword(p, "ASXML") ||
	   check_keyword(p, "BINARY") {
		advance_token(p)
		stmt.result_stream = parse_expr(p)
		return
	}
	parse_call_transformation_result_roots(p, &stmt.result_roots)
}

// parse_call_transformation_stmt parses CALL TRANSFORMATION (XSLT / simple transformation).
parse_call_transformation_stmt :: proc(p: ^Parser, call_tok: lexer.Token) -> ^ast.Stmt {
	expect_keyword_token(p, "TRANSFORMATION")
	trans := parse_expr(p)

	stmt := ast.new(ast.Call_Transformation_Stmt, call_tok.range)
	stmt.transformation = trans
	stmt.options = nil
	stmt.source = nil
	stmt.result_stream = nil
	stmt.result_roots = make([dynamic]^ast.Named_Arg)

	for p.curr_tok.kind != .Period && p.curr_tok.kind != .EOF {
		skip_pragma(p)
		skip_call_transaction_pragma_strings(p)

		if check_keyword(p, "SOURCE") {
			advance_token(p)
			stmt.source = parse_call_transformation_source_operand(p)
			continue
		}
		if check_keyword(p, "RESULT") {
			advance_token(p)
			parse_call_transformation_result_section(p, stmt)
			continue
		}
		if check_keyword(p, "OPTIONS") {
			advance_token(p)
			stmt.options = parse_expr(p)
			continue
		}
		if check_keyword(p, "PARAMETERS") {
			advance_token(p)
			if p.curr_tok.kind == .LParen {
				advance_token(p)
				skip_to_matching_paren_or_period(p)
				if p.curr_tok.kind == .RParen {
					advance_token(p)
				}
			}
			continue
		}
		break
	}

	period_tok := expect_token(p, .Period)
	stmt.range.end = period_tok.range.end
	return stmt
}

// parse_call_method_stmt parses old-style CALL METHOD statements
// Syntax: CALL METHOD method.
//         CALL METHOD class=>method
//           EXPORTING param = value
//           IMPORTING param = value
//           CHANGING param = value.
parse_call_method_stmt :: proc(p: ^Parser, call_tok: lexer.Token) -> ^ast.Stmt {
	expect_keyword_token(p, "METHOD")
	return parse_call_keyworded_param_sections_stmt(p, call_tok)
}

// parse_call_badi_stmt parses CALL BADI badi_ref->method ...
// Sections: EXPORTING, IMPORTING, CHANGING, RECEIVING, EXCEPTIONS (no TABLES / DESTINATION).
parse_call_badi_stmt :: proc(p: ^Parser, call_tok: lexer.Token) -> ^ast.Stmt {
	expect_keyword_token(p, "BADI")
	badi_target := parse_expr(p)

	stmt := ast.new(ast.Call_Badi_Stmt, call_tok.range)
	stmt.badi_target = badi_target
	stmt.exporting = make([dynamic]^ast.Call_Function_Param)
	stmt.importing = make([dynamic]^ast.Call_Function_Param)
	stmt.changing = make([dynamic]^ast.Call_Function_Param)
	stmt.receiving = make([dynamic]^ast.Call_Function_Param)
	stmt.exceptions = make([dynamic]^ast.Call_Function_Param)
	stmt.derived_stmt = stmt

	for p.curr_tok.kind != .Period && p.curr_tok.kind != .EOF {
		if check_keyword(p, "EXPORTING") {
			advance_token(p)
			parse_call_function_params(p, &stmt.exporting, .Exporting)
		} else if check_keyword(p, "IMPORTING") {
			advance_token(p)
			parse_call_function_params(p, &stmt.importing, .Importing)
		} else if check_keyword(p, "CHANGING") {
			advance_token(p)
			parse_call_function_params(p, &stmt.changing, .Changing)
		} else if check_keyword(p, "RECEIVING") {
			advance_token(p)
			parse_call_function_params(p, &stmt.receiving, .Receiving)
		} else if check_keyword(p, "EXCEPTIONS") {
			advance_token(p)
			parse_call_function_params(p, &stmt.exceptions, .Exceptions)
		} else {
			break
		}
	}

	period_tok := expect_token(p, .Period)
	stmt.range.end = period_tok.range.end
	return stmt
}

// parse_call_system_stmt parses CALL 'name' followed by ID 'id' FIELD operand pairs.
// Optional line comments / pragmas after the module literal are skipped by advance_token.
parse_call_system_stmt :: proc(p: ^Parser, call_tok: lexer.Token) -> ^ast.Stmt {
	module_expr := parse_expr(p)

	stmt := ast.new(ast.Call_System_Stmt, call_tok.range)
	stmt.module = module_expr
	stmt.params = make([dynamic]^ast.Call_System_Param)
	stmt.derived_stmt = stmt

	for p.curr_tok.kind != .Period && p.curr_tok.kind != .EOF {
		if !check_keyword(p, "ID") {
			break
		}
		id_kw := advance_token(p)
		id_name := parse_expr(p)
		expect_keyword_token(p, "FIELD")
		field_expr := parse_expr(p)
		skip_pragma(p)

		param := ast.new(
			ast.Call_System_Param,
			lexer.TextRange{id_kw.range.start, field_expr.range.end},
		)
		param.id_name = id_name
		param.field = field_expr
		append(&stmt.params, param)
	}

	period_tok := expect_token(p, .Period)
	stmt.range.end = period_tok.range.end
	return stmt
}

// Shared by CALL METHOD and CALL BADI after the distinguishing keyword is consumed.
parse_call_keyworded_param_sections_stmt :: proc(p: ^Parser, call_tok: lexer.Token) -> ^ast.Stmt {
	callee_expr := parse_expr(p)

	call_expr := ast.new(ast.Call_Expr, call_tok.range)
	call_expr.expr = callee_expr
	method_args := make([dynamic]^ast.Expr)

	for p.curr_tok.kind != .Period && p.curr_tok.kind != .EOF {
		if check_keyword(p, "EXPORTING") ||
		   check_keyword(p, "IMPORTING") ||
		   check_keyword(p, "CHANGING") ||
		   check_keyword(p, "RECEIVING") ||
		   check_keyword(p, "EXCEPTIONS") {
			advance_token(p)
			parse_call_method_args(p, &method_args)
			continue
		}

		break
	}

	period_tok := expect_token(p, .Period)
	call_expr.args = method_args[:]
	call_expr.range.end = period_tok.range.end
	call_expr.derived_expr = call_expr

	expr_stmt := ast.new(ast.Expr_Stmt, call_tok, period_tok)
	expr_stmt.expr = call_expr
	return expr_stmt
}

// parse_call_method_args parses old-style CALL METHOD parameter assignments.
// It flattens sectioned parameters into named call arguments.
parse_call_method_args :: proc(p: ^Parser, args: ^[dynamic]^ast.Expr) {
	for p.curr_tok.kind != .Period && p.curr_tok.kind != .EOF {
		if check_keyword(p, "EXPORTING") ||
		   check_keyword(p, "IMPORTING") ||
		   check_keyword(p, "CHANGING") ||
		   check_keyword(p, "RECEIVING") ||
		   check_keyword(p, "EXCEPTIONS") {
			break
		}

		if p.curr_tok.kind != .Ident {
			break
		}

		param_name_tok := p.curr_tok
		advance_token(p)

		if p.curr_tok.kind != .Eq {
			error(
				p,
				p.curr_tok.range,
				"expected '=' after parameter name '%s'",
				param_name_tok.lit,
			)
			break
		}
		advance_token(p)

		param_value := parse_expr(p)
		if param_value == nil {
			break
		}

		skip_pragma(p)

		named_arg := ast.new(
			ast.Named_Arg,
			lexer.TextRange{param_name_tok.range.start, param_value.range.end},
		)
		named_arg.name = ast.new_ident(param_name_tok)
		named_arg.value = param_value
		named_arg.derived_expr = named_arg

		append(args, &named_arg.node)
	}
}

// try_consume_call_function_in_task_clause parses optional IN BACKGROUND TASK or IN UPDATE TASK
// after the function name (and optional first DESTINATION). Restores parser state on mismatch.
// Optional task id / title after IN BACKGROUND TASK (e.g. a string literal) before DESTINATION or param sections.
consume_optional_call_function_in_task_id :: proc(p: ^Parser) {
	if p.curr_tok.kind == .Period || p.curr_tok.kind == .EOF {
		return
	}
	if check_keyword(p, "DESTINATION") ||
	   check_keyword(p, "EXPORTING") ||
	   check_keyword(p, "IMPORTING") ||
	   check_keyword(p, "TABLES") ||
	   check_keyword(p, "CHANGING") ||
	   check_keyword(p, "RECEIVING") ||
	   check_keyword(p, "EXCEPTIONS") {
		return
	}
	// Parsed for forward progress only; AST has no field for this id yet.
	_ = parse_concat_expr(p)
}

try_consume_call_function_in_task_clause :: proc(p: ^Parser) -> bool {
	if !check_keyword(p, "IN") {
		return false
	}

	saved_prev := p.prev_tok
	saved_curr := p.curr_tok
	saved_pos := p.l.pos
	saved_read_pos := p.l.read_pos
	saved_ch := p.l.ch

	advance_token(p) // IN
	if check_keyword(p, "BACKGROUND") {
		advance_token(p)
		if check_keyword(p, "TASK") {
			advance_token(p)
			consume_optional_call_function_in_task_id(p)
			return true
		}
	} else if check_keyword(p, "UPDATE") {
		advance_token(p)
		if check_keyword(p, "TASK") {
			advance_token(p)
			consume_optional_call_function_in_task_id(p)
			return true
		}
	}

	p.prev_tok = saved_prev
	p.curr_tok = saved_curr
	p.l.pos = saved_pos
	p.l.read_pos = saved_read_pos
	p.l.ch = saved_ch
	return false
}

// try_consume_call_function_starting_new_task parses STARTING NEW TASK task_id after the function
// name; restores lexer/parser state if STARTING is not followed by NEW TASK.
try_consume_call_function_starting_new_task :: proc(p: ^Parser, call_func: ^ast.Call_Function_Stmt) -> bool {
	if !check_keyword(p, "STARTING") {
		return false
	}

	saved_prev := p.prev_tok
	saved_curr := p.curr_tok
	saved_pos := p.l.pos
	saved_read_pos := p.l.read_pos
	saved_ch := p.l.ch

	advance_token(p) // STARTING
	if !check_keyword(p, "NEW") {
		p.prev_tok = saved_prev
		p.curr_tok = saved_curr
		p.l.pos = saved_pos
		p.l.read_pos = saved_read_pos
		p.l.ch = saved_ch
		return false
	}
	advance_token(p)
	if !check_keyword(p, "TASK") {
		p.prev_tok = saved_prev
		p.curr_tok = saved_curr
		p.l.pos = saved_pos
		p.l.read_pos = saved_read_pos
		p.l.ch = saved_ch
		return false
	}
	advance_token(p)
	call_func.starting_new_task = parse_expr(p)
	return true
}

parse_call_function_destination_if_present :: proc(p: ^Parser, call_func: ^ast.Call_Function_Stmt) -> bool {
	if check_keyword(p, "DESTINATION") {
		advance_token(p)
		call_func.destination = parse_expr(p)
		return true
	}
	return false
}

// parse_call_function_stmt parses a CALL FUNCTION statement
// Syntax: CALL FUNCTION 'func_name' [DESTINATION dest]
//         [STARTING NEW TASK task_id]
//         [IN BACKGROUND TASK | IN UPDATE TASK [QUALIFIERS]]
//         [DESTINATION dest]
//         [EXPORTING param = value ...]
//         [IMPORTING param = value ...]
//         [TABLES param = value ...]
//         [CHANGING param = value ...]
//         [EXCEPTIONS name = value ...].
parse_call_function_stmt :: proc(p: ^Parser, call_tok: lexer.Token) -> ^ast.Stmt {
	expect_keyword_token(p, "FUNCTION")

	// Stop before relational IN so 'name' IN UPDATE TASK is not one comparison expression.
	func_name := parse_concat_expr(p)

	call_func := ast.new(ast.Call_Function_Stmt, call_tok.range)
	call_func.func_name = func_name
	call_func.exporting = make([dynamic]^ast.Call_Function_Param)
	call_func.importing = make([dynamic]^ast.Call_Function_Param)
	call_func.tables = make([dynamic]^ast.Call_Function_Param)
	call_func.changing = make([dynamic]^ast.Call_Function_Param)
	call_func.exceptions = make([dynamic]^ast.Call_Function_Param)
	call_func.derived_stmt = call_func

	// Optional clauses (DESTINATION, STARTING NEW TASK, IN ... TASK) may appear in several orders
	for {
		if try_consume_call_function_starting_new_task(p, call_func) {
			continue
		}
		if parse_call_function_destination_if_present(p, call_func) {
			continue
		}
		if try_consume_call_function_in_task_clause(p) {
			continue
		}
		break
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
		   check_keyword(p, "RECEIVING") ||
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

		// EXCEPTIONS exc = rc MESSAGE dobj — optional message target (RFC, communication_failure, etc.)
		message_value: ^ast.Expr
		if kind == .Exceptions && check_keyword(p, "MESSAGE") {
			advance_token(p)
			message_value = parse_call_function_param_value(p)
			skip_pragma(p)
		}

		// Create the parameter node
		param := ast.new(ast.Call_Function_Param, param_name_tok.range)
		param.kind = kind
		param.name = ast.new_ident(param_name_tok)
		if kind == .Exceptions &&
		   len(param_name_tok.lit) > 0 &&
		   len(param_name_tok.lit) < len(p.keyword_buffer) &&
		   to_upper(p.keyword_buffer[:], param_name_tok.lit) == "OTHERS" {
			param.is_others = true
		}
		param.value = param_value
		param.message_value = message_value
		if message_value != nil {
			param.range.end = message_value.range.end
		} else if param_value != nil {
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

// skip_pragma skips ABAP pragmas like ##ENH_OK (lexer emits these as .Comment)
skip_pragma :: proc(p: ^Parser) {
	for {
		if p.curr_tok.kind == .Comment && len(p.curr_tok.lit) >= 2 {
			if p.curr_tok.lit[0] == '#' && p.curr_tok.lit[1] == '#' {
				advance_token(p)
				continue
			}
		}
		if p.curr_tok.kind == .Ident && len(p.curr_tok.lit) >= 2 {
			if p.curr_tok.lit[0] == '#' && p.curr_tok.lit[1] == '#' {
				advance_token(p)
				continue
			}
		}
		break
	}
}

parse_logical_expr :: proc(p: ^Parser) -> ^ast.Expr {
	return parse_or_expr(p)
}

// parse_table_bracket_index_content parses the inside of itab[ ... ].
// Handles ABAP table key access KEY [key_name] COMPONENTS predicate as well as index / free-key logical expressions.
parse_table_bracket_index_content :: proc(
	p: ^Parser,
) -> (
	index: ^ast.Expr,
	table_key_name: ^ast.Ident,
	has_key_clause: bool,
) {
	if !check_keyword(p, "KEY") {
		return parse_table_bracket_logical_expr(p), nil, false
	}

	saved_prev := p.prev_tok
	saved_curr := p.curr_tok
	saved_pos := p.l.pos
	saved_read_pos := p.l.read_pos
	saved_ch := p.l.ch

	advance_token(p) // KEY
	if check_keyword(p, "COMPONENTS") {
		advance_token(p)
		return parse_table_bracket_logical_expr(p), nil, true
	}

	if p.curr_tok.kind == .Ident {
		saved2_prev := p.prev_tok
		saved2_curr := p.curr_tok
		saved2_pos := p.l.pos
		saved2_read_pos := p.l.read_pos
		saved2_ch := p.l.ch

		key_name_tok := advance_token(p)
		if check_keyword(p, "COMPONENTS") {
			advance_token(p)
			return parse_table_bracket_logical_expr(p), ast.new_ident(key_name_tok), true
		}

		p.prev_tok = saved2_prev
		p.curr_tok = saved2_curr
		p.l.pos = saved2_pos
		p.l.read_pos = saved2_read_pos
		p.l.ch = saved2_ch
	}

	p.prev_tok = saved_prev
	p.curr_tok = saved_curr
	p.l.pos = saved_pos
	p.l.read_pos = saved_read_pos
	p.l.ch = saved_ch
	return parse_table_bracket_logical_expr(p), nil, false
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
	// Always go through concat/additive so `( a + b ) * c` is one operand; leading `(` is
	// grouping via parse_paren_expr (inner = parse_logical_expr), not a separate parse stop.
	left := parse_concat_expr(p)

	if check_keyword(p, "IS") {
		return parse_is_predicate(p, left)
	}

	if is_comparison_op(p) {
		op_tok := advance_token(p)
		right := parse_concat_expr(p)

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
	   check_keyword(p, "IN") ||
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

		// Parse the type expression after INSTANCE OF
		type_ref := parse_simple_type_expr(p)

		pred_expr := ast.new(
			ast.Predicate_Expr,
			lexer.TextRange{expr.range.start, p.prev_tok.range.end},
		)
		pred_expr.expr = expr
		pred_expr.predicate = predicate_kind
		pred_expr.is_negated = is_negated
		pred_expr.type_ref = type_ref
		pred_expr.derived_expr = pred_expr
		return pred_expr
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

// peek_starts_table_bracket_comparison is true when another comparison begins inside itab[ ... ]
// without an explicit AND (ABAP allows space-separated table key components).
peek_starts_table_bracket_comparison :: proc(p: ^Parser) -> bool {
	if p.curr_tok.kind == .RBracket {
		return false
	}
	if check_keyword(p, "OR") || check_keyword(p, "AND") {
		return false
	}
	if p.curr_tok.kind == .LParen {
		return true
	}
	if check_keyword(p, "NOT") {
		return true
	}

	saved_prev := p.prev_tok
	saved_curr := p.curr_tok
	saved_pos := p.l.pos
	saved_read_pos := p.l.read_pos
	saved_ch := p.l.ch
	saved_line_start := p.l.line_start
	saved_line_count := p.l.line_count

	defer {
		p.prev_tok = saved_prev
		p.curr_tok = saved_curr
		p.l.pos = saved_pos
		p.l.read_pos = saved_read_pos
		p.l.ch = saved_ch
		p.l.line_start = saved_line_start
		p.l.line_count = saved_line_count
	}

	// Use concat-level parse only: full parse_expr would consume an entire comparison and
	// leave the lexer on ']', so we'd miss implicit AND between key components.
	lhs := parse_concat_expr(p)
	if lhs == nil {
		return false
	}
	if check_keyword(p, "IS") {
		return true
	}
	return is_comparison_op(p)
}

parse_table_bracket_and_chain :: proc(p: ^Parser) -> ^ast.Expr {
	left := parse_not_expr(p)

	for {
		if check_keyword(p, "AND") {
			op_tok := advance_token(p)
			right := parse_not_expr(p)

			binary := ast.new(ast.Binary_Expr, lexer.TextRange{left.range.start, right.range.end})
			binary.left = left
			binary.op = op_tok
			binary.right = right
			binary.derived_expr = binary
			left = binary
			continue
		}
		if !peek_starts_table_bracket_comparison(p) {
			break
		}
		op_tok := lexer.Token {
			kind  = .Ident,
			lit   = "AND",
			range = lexer.TextRange{left.range.end, p.curr_tok.range.start},
		}
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

// parse_table_bracket_logical_expr parses itab[ ... ] conditions: OR of AND-chains, with implicit AND
// between consecutive comparisons (table key rows).
parse_table_bracket_logical_expr :: proc(p: ^Parser) -> ^ast.Expr {
	left := parse_table_bracket_and_chain(p)

	for check_keyword(p, "OR") {
		op_tok := advance_token(p)
		right := parse_table_bracket_and_chain(p)

		binary := ast.new(ast.Binary_Expr, lexer.TextRange{left.range.start, right.range.end})
		binary.left = left
		binary.op = op_tok
		binary.right = right
		binary.derived_expr = binary
		left = binary
	}

	return left
}

parse_set_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	set_tok := advance_token(p)
	if check_keyword(p, "HANDLER") {
		advance_token(p)
		handlers := make([dynamic]^ast.Expr)
		for {
			h := parse_expr(p)
			if h == nil {
				error(p, p.curr_tok.range, "expected handler method reference in SET HANDLER")
				break
			}
			append(&handlers, h)
			if check_keyword(p, "FOR") {
				break
			}
			if p.curr_tok.kind == .Period {
				error(p, p.curr_tok.range, "expected FOR before end of SET HANDLER statement")
				break
			}
		}
		expect_keyword_token(p, "FOR")
		for_ref := parse_expr(p)
		end_tok := p.curr_tok
		expect_token(p, .Period)
		stmt := ast.new(ast.Set_Handler_Stmt, set_tok, end_tok)
		stmt.handlers = handlers
		stmt.for_ref = for_ref
		return stmt
	}
	if check_keyword(p, "BIT") {
		advance_token(p)
		bit_position := parse_expr(p)
		expect_keyword_token(p, "OF")
		of_target := parse_expr(p)
		expect_keyword_token(p, "TO")
		to_value := parse_expr(p)
		end_tok := p.curr_tok
		expect_token(p, .Period)
		bit_stmt := ast.new(ast.Set_Bit_Stmt, set_tok, end_tok)
		bit_stmt.bit_position = bit_position
		bit_stmt.of_target = of_target
		bit_stmt.to_value = to_value
		return bit_stmt
	}
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
	data_decl.ident.inline_data_decl = data_decl
	data_decl.value = nil // Value is determined by the LOOP context
	data_decl.derived_stmt = data_decl
	return data_decl.ident
}

// parse_final_inline_expr parses an inline FINAL declaration in expression context.
// Syntax: FINAL(var)
parse_final_inline_expr :: proc(p: ^Parser) -> ^ast.Expr {
	expect_keyword_token(p, "FINAL")
	expect_token_space_req(p, .LParen, .WithoutLeadingSpace)
	ident_tok := expect_token_space_req(p, .Ident, .WithoutLeadingSpace)
	expect_token_space_req(p, .RParen, .WithoutLeadingSpace)
	return ast.new_ident(ident_tok)
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

parse_assign_dynamic_source :: proc(p: ^Parser) -> ^ast.Expr {
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

parse_assign_subfield_component :: proc(p: ^Parser) -> ^ast.Expr {
	expr := parse_operand(p)
	if expr == nil {
		return nil
	}

	loop: for {
		if expr == nil {
			break loop
		}

		#partial switch p.curr_tok.kind {
		case .Minus, .FatArrow, .Tilde, .Arrow:
			if lexer.have_space_between(p.prev_tok, p.curr_tok) {
				break loop
			}
			op := advance_token(p)
			field_expr: ^ast.Expr
			end_at: int
			if p.curr_tok.kind == .LParen && !lexer.have_space_between(p.prev_tok, p.curr_tok) {
				lparen_tok := advance_token(p)
				inner := parse_expr(p)
				rparen_tok := expect_token(p, .RParen)
				paren := ast.new(
					ast.Paren_Expr,
					lexer.TextRange{lparen_tok.range.start, rparen_tok.range.end},
				)
				paren.expr = inner
				paren.derived_expr = paren
				field_expr = paren
				end_at = rparen_tok.range.end
			} else {
				// Match parse_atom_expr: TEXT-nnn text symbols use a numeric id; allow * for selections
				field_tok: lexer.Token
				if p.curr_tok.kind == .Ident || p.curr_tok.kind == .Number || p.curr_tok.kind == .Star {
					field_tok = advance_token(p)
				} else {
					field_tok = expect_token(p, .Ident)
				}
				field_ident := ast.new_ident(field_tok)
				if field_tok.kind == .Star {
					field_ident.name = "*"
				}
				field_expr = &field_ident.node
				end_at = field_tok.range.end
			}
			selector := ast.new(ast.Selector_Expr, lexer.TextRange{expr.range.start, end_at})
			selector.expr = expr
			selector.op = op
			selector.field = field_expr
			selector.derived_expr = selector
			expr = &selector.node
		case .LBracket:
			advance_token(p)
			index_expr, table_key_name, has_key_clause := parse_table_bracket_index_content(p)
			rbracket_tok := expect_token(p, .RBracket)

			table_expr := ast.new(
				ast.Index_Expr,
				lexer.TextRange{expr.range.start, rbracket_tok.range.end},
			)
			table_expr.expr = expr
			table_expr.index = index_expr
			table_expr.table_key_name = table_key_name
			table_expr.has_key_clause = has_key_clause
			table_expr.derived_expr = table_expr
			expr = table_expr
		case:
			break loop
		}
	}

	return expr
}

parse_assign_source_expr :: proc(
	p: ^Parser,
	stmt: ^ast.Assign_Field_Symbol_Stmt,
) -> ^ast.Expr {
	if p.curr_tok.kind == .LParen {
		stmt.is_dynamic = true
		paren_expr := parse_assign_dynamic_source(p)
		// ASSIGN (dobj) TO ... or ASSIGN (cls)=>(attr) TO ... — continue chains after first parens.
		return parse_atom_expr(p, paren_expr, true)
	}

	source := parse_atom_expr(p, parse_operand(p), false)
	if source == nil {
		return nil
	}

	if p.curr_tok.kind == .Plus && !lexer.have_space_between(p.prev_tok, p.curr_tok) {
		advance_token(p) // consume +
		stmt.offset = parse_assign_subfield_component(p)

		if p.curr_tok.kind == .LParen && !lexer.have_space_between(p.prev_tok, p.curr_tok) {
			advance_token(p) // consume (
			if p.curr_tok.kind == .Star {
				stmt.length_is_star = true
				advance_token(p)
			} else {
				stmt.length = parse_assign_subfield_component(p)
			}
			expect_token(p, .RParen)
		}
	}

	return source
}

parse_assign_component_structure_stmt :: proc(p: ^Parser, stmt: ^ast.Assign_Field_Symbol_Stmt) {
	expect_keyword_token(p, "COMPONENT")
	stmt.is_component = true
	stmt.component = parse_expr(p)

	expect_keyword_token(p, "OF")
	expect_keyword_token(p, "STRUCTURE")
	stmt.structure = parse_expr(p)
}

parse_assign_field_symbol_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	assign_tok := expect_keyword_token(p, "ASSIGN")

	stmt := ast.new(ast.Assign_Field_Symbol_Stmt, assign_tok.range)
	stmt.derived_stmt = stmt

	if check_keyword(p, "COMPONENT") {
		parse_assign_component_structure_stmt(p, stmt)
	} else if check_keyword(p, "TABLE") {
		advance_token(p)
		expect_keyword_token(p, "FIELD")
		stmt.is_table_field = true
		stmt.is_dynamic = true
		stmt.source = parse_assign_dynamic_source(p)
	} else {
		stmt.source = parse_assign_source_expr(p, stmt)
	}

	expect_keyword_token(p, "TO")
	if check_hyphenated_keyword(p, "FIELD", "SYMBOL") {
		stmt.target = parse_inline_field_symbol(p)
	} else {
		stmt.target = parse_field_symbol_ref(p)
	}

	period_tok := expect_token(p, .Period)
	stmt.range.end = period_tok.range.end
	return stmt
}

// MOVE [source TO target] | MOVE: source TO target [, source TO target] ...
// Plain / chained MOVE is Assign_Stmt (targets on lhs, sources on rhs) with op token MOVE.
parse_move_stmt :: proc(p: ^Parser, move_tok: lexer.Token) -> ^ast.Stmt {
	expect_keyword_token(p, "MOVE")

	is_chain := false
	if allow_token(p, .Colon) {
		is_chain = true
	}

	if is_chain &&
	   (p.curr_tok.kind == .Period || p.curr_tok.kind == .EOF) {
		error(p, p.curr_tok.range, "expected source expression after MOVE:")
		period_tok := expect_token(p, .Period)
		assign_stmt := ast.new(ast.Assign_Stmt, move_tok, period_tok)
		assign_stmt.op = move_tok
		assign_stmt.lhs = nil
		assign_stmt.rhs = nil
		return assign_stmt
	}

	lhs_list: [dynamic]^ast.Expr
	rhs_list: [dynamic]^ast.Expr
	defer delete(lhs_list)
	defer delete(rhs_list)

	for {
		source := parse_expr(p)
		expect_keyword_token(p, "TO")
		target := parse_expr(p)
		append(&rhs_list, source)
		append(&lhs_list, target)

		if p.curr_tok.kind == .Comma {
			if !is_chain {
				error(
					p,
					p.curr_tok.range,
					"use MOVE: when chaining several assignments separated by commas",
				)
			}
			advance_token(p)
			if p.curr_tok.kind == .Period {
				error(p, p.curr_tok.range, "trailing comma before period in MOVE")
				break
			}
			continue
		}
		break
	}

	period_tok := expect_token(p, .Period)

	assign_stmt := ast.new(ast.Assign_Stmt, move_tok, period_tok)
	assign_stmt.op = move_tok
	assign_stmt.lhs = make([]^ast.Expr, len(lhs_list))
	assign_stmt.rhs = make([]^ast.Expr, len(rhs_list))
	copy(assign_stmt.lhs, lhs_list[:])
	copy(assign_stmt.rhs, rhs_list[:])
	return assign_stmt
}

// MESSAGE statement parser
// Syntax: MESSAGE { msg | text | ID class TYPE type NUMBER num } [DISPLAY LIKE display_type] [WITH v1 [v2 [v3 [v4]]]] [INTO data]
// Examples:
//   MESSAGE 'No display authorization.' TYPE 'I' DISPLAY LIKE 'E'.
//   MESSAGE e899(/sttpec/int_msg) WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO lv_dummy_msg.
//   MESSAGE iv_msg TYPE 'I' DISPLAY LIKE 'E'.
//   MESSAGE iv_msg TYPE 'I'.
//   MESSAGE ID lv_msg_class TYPE iv_msg_type NUMBER 898 WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO lv_dummy_msg.
parse_message_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	message_tok := expect_keyword_token(p, "MESSAGE")

	msg_stmt := ast.new(ast.Message_Stmt, message_tok.range)
	msg_stmt.with_args = make([dynamic]^ast.Expr)

	if check_keyword(p, "ID") {
		advance_token(p)
		msg_stmt.id_class = parse_expr(p)
	} else {
		// Parse the message expression:
		// - A string literal: 'No display authorization.'
		// - An identifier: iv_msg
		// - A message ID: e899(/sttpec/int_msg) or e899(class_name)
		msg_stmt.msg_expr = parse_message_id_or_expr(p)
	}

	// Parse optional clauses (order flexible except initial ID / msg choice)
	for p.curr_tok.kind != .EOF && p.curr_tok.kind != .Period {
		if check_keyword(p, "TYPE") {
			advance_token(p)
			msg_stmt.msg_type = parse_expr(p)
		} else if check_keyword(p, "DISPLAY") {
			advance_token(p)
			expect_keyword_token(p, "LIKE")
			msg_stmt.display_like = parse_expr(p)
		} else if check_keyword(p, "NUMBER") {
			advance_token(p)
			msg_stmt.msg_number = parse_expr(p)
		} else if check_keyword(p, "WITH") {
			advance_token(p)
			// Parse up to 4 WITH arguments
			for i := 0; i < 4 && p.curr_tok.kind != .EOF && p.curr_tok.kind != .Period; i += 1 {
				if check_keyword(p, "INTO") ||
				   check_keyword(p, "TYPE") ||
				   check_keyword(p, "DISPLAY") ||
				   check_keyword(p, "NUMBER") {
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

is_field_symbol_ident_token :: proc(tok: lexer.Token) -> bool {
	return tok.kind == .Ident && len(tok.lit) >= 3 && tok.lit[0] == '<' && tok.lit[len(tok.lit)-1] == '>'
}

// parse_field_symbol_ref parses a field symbol reference <fs>
parse_field_symbol_ref :: proc(p: ^Parser) -> ^ast.Expr {
	if is_field_symbol_ident_token(p.curr_tok) {
		return ast.new_ident(advance_token(p))
	}

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

	// Support older split-token handling by reconstructing the identifier.
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
	lhs := parse_atom_expr(p, fs_expr, true)

	if p.curr_tok.kind == .Eq || p.curr_tok.kind == .QuestionEq {
		op := advance_token(p)
		rhs := parse_expr(p)
		skip_pragma(p)
		period_tok := expect_token(p, .Period)

		assign_stmt := ast.new(ast.Assign_Stmt, start_tok, period_tok)
		assign_stmt.lhs = make([]^ast.Expr, 1)
		assign_stmt.lhs[0] = lhs
		assign_stmt.op = op
		assign_stmt.rhs = make([]^ast.Expr, 1)
		assign_stmt.rhs[0] = rhs
		return assign_stmt
	}

	skip_pragma(p)
	period_tok := expect_token(p, .Period)
	expr_stmt := ast.new(ast.Expr_Stmt, start_tok, period_tok)
	expr_stmt.expr = lhs
	return expr_stmt
}

// True when current is CONVERT and the following tokens are TIME STAMP (not consumed).
check_convert_time_stamp_into_date_time_prefix :: proc(p: ^Parser) -> bool {
	saved_prev := p.prev_tok
	saved_curr := p.curr_tok
	saved_pos := p.l.pos
	saved_read_pos := p.l.read_pos
	saved_ch := p.l.ch

	advance_token(p)
	if !check_keyword(p, "TIME") {
		p.prev_tok = saved_prev
		p.curr_tok = saved_curr
		p.l.pos = saved_pos
		p.l.read_pos = saved_read_pos
		p.l.ch = saved_ch
		return false
	}
	advance_token(p)
	ok := check_keyword(p, "STAMP")
	p.prev_tok = saved_prev
	p.curr_tok = saved_curr
	p.l.pos = saved_pos
	p.l.read_pos = saved_read_pos
	p.l.ch = saved_ch
	return ok
}

// free_at_stmt_start_is_memory_stmt is false when the identifier is immediately followed by '('
// with no intervening space (e.g. free( ).), so the statement is parsed as an expression call.
// True for FREE itab., FREE ( itab )., FREE: ..., etc.
free_at_stmt_start_is_memory_stmt :: proc(p: ^Parser) -> bool {
	saved_prev := p.prev_tok
	saved_curr := p.curr_tok
	saved_pos := p.l.pos
	saved_read_pos := p.l.read_pos
	saved_ch := p.l.ch

	advance_token(p)
	next := p.curr_tok
	is_paren_call :=
		next.kind == .LParen && !lexer.have_space_between(saved_curr, next)

	p.prev_tok = saved_prev
	p.curr_tok = saved_curr
	p.l.pos = saved_pos
	p.l.read_pos = saved_read_pos
	p.l.ch = saved_ch

	return !is_paren_call
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
	no_gaps := false
	if check_hyphenated_keyword(p, "NO", "GAPS") {
		no_gaps = true
	}
	period_tok := expect_token(p, .Period)
	condense_stmt := ast.new(ast.Condense_Stmt, condense_tok, period_tok)
	condense_stmt.text = text_expr
	condense_stmt.no_gaps = no_gaps
	return condense_stmt
}

parse_translate_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	translate_tok := expect_keyword_token(p, "TRANSLATE")
	target := parse_expr(p)
	kind: ast.Translate_Kind
	using_pattern: ^ast.Expr = nil
	if check_keyword(p, "TO") {
		advance_token(p)
		if check_keyword(p, "UPPER") {
			advance_token(p)
			expect_keyword_token(p, "CASE")
			kind = .Upper_Case
		} else if check_keyword(p, "LOWER") {
			advance_token(p)
			expect_keyword_token(p, "CASE")
			kind = .Lower_Case
		} else {
			error(p, p.curr_tok.range, "expected UPPER or LOWER after TRANSLATE ... TO")
		}
	} else if check_keyword(p, "USING") {
		advance_token(p)
		using_pattern = parse_expr(p)
		kind = .Using
	} else {
		error(p, p.curr_tok.range, "expected TO or USING after TRANSLATE target")
	}
	period_tok := expect_token(p, .Period)
	stmt := ast.new(ast.Translate_Stmt, translate_tok, period_tok)
	stmt.target = target
	stmt.kind = kind
	stmt.using_pattern = using_pattern
	return stmt
}

concatenate_has_substring_length :: proc(p: ^Parser) -> bool {
	if p.curr_tok.kind != .LParen || lexer.have_space_between(p.prev_tok, p.curr_tok) {
		return false
	}

	saved_prev := p.prev_tok
	saved_curr := p.curr_tok
	saved_pos := p.l.pos
	saved_read_pos := p.l.read_pos
	saved_ch := p.l.ch

	advance_token(p) // consume (
	result := p.curr_tok.kind == .Number || p.curr_tok.kind == .Star

	p.prev_tok = saved_prev
	p.curr_tok = saved_curr
	p.l.pos = saved_pos
	p.l.read_pos = saved_read_pos
	p.l.ch = saved_ch

	return result
}

parse_concatenate_source_expr :: proc(p: ^Parser) -> ^ast.Expr {
	source := parse_assign_subfield_component(p)
	if source == nil {
		return nil
	}

	offset: ^ast.Expr
	length: ^ast.Expr
	length_is_star := false
	range_end := source.range.end
	has_substring := false

	if p.curr_tok.kind == .Plus && !lexer.have_space_between(p.prev_tok, p.curr_tok) {
		has_substring = true
		advance_token(p) // consume +
		offset = parse_assign_subfield_component(p)
		if offset != nil {
			range_end = offset.range.end
		}
	}

	if concatenate_has_substring_length(p) || (has_substring && p.curr_tok.kind == .LParen && !lexer.have_space_between(p.prev_tok, p.curr_tok)) {
		has_substring = true
		advance_token(p) // consume (
		if p.curr_tok.kind == .Star {
			length_is_star = true
			range_end = p.curr_tok.range.end
			advance_token(p)
		} else {
			length = parse_assign_subfield_component(p)
			if length != nil {
				range_end = length.range.end
			}
		}
		rparen_tok := expect_token(p, .RParen)
		range_end = rparen_tok.range.end
	}

	if !has_substring {
		if p.curr_tok.kind == .LParen && !lexer.have_space_between(p.prev_tok, p.curr_tok) {
			return parse_atom_expr(p, source)
		}
		return source
	}

	substring_expr := ast.new(ast.Substring_Expr, lexer.TextRange{source.range.start, range_end})
	substring_expr.expr = source
	substring_expr.offset = offset
	substring_expr.length = length
	substring_expr.length_is_star = length_is_star
	return substring_expr
}

parse_split_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	split_tok := expect_keyword_token(p, "SPLIT")
	stmt := ast.new(ast.Split_Stmt, split_tok.range)
	stmt.targets = make([dynamic]^ast.Expr)

	stmt.source = parse_expr(p)
	expect_keyword_token(p, "AT")
	stmt.separator = parse_expr(p)
	expect_keyword_token(p, "INTO")

	if check_keyword(p, "TABLE") {
		advance_token(p)
		if check_keyword(p, "DATA") {
			stmt.table_target = parse_data_inline_expr(p)
		} else {
			stmt.table_target = parse_expr(p)
		}
	} else {
		for p.curr_tok.kind != .EOF && p.curr_tok.kind != .Period {
			if check_keyword(p, "IN") {
				break
			}

			// Avoid parsing last target as "part IN CHARACTER" comparison before IN CHARACTER MODE.
			target := parse_concat_expr(p)
			if target == nil {
				break
			}
			append(&stmt.targets, target)
		}
	}

	if check_keyword(p, "IN") {
		advance_token(p)
		if check_keyword(p, "CHARACTER") {
			advance_token(p)
			stmt.mode = .Character
		} else if check_keyword(p, "BYTE") {
			advance_token(p)
			stmt.mode = .Byte
		} else {
			error(p, p.curr_tok.range, "expected CHARACTER or BYTE after IN")
		}
		expect_keyword_token(p, "MODE")
	}

	period_tok := expect_token(p, .Period)
	stmt.range.end = period_tok.range.end
	return stmt
}

parse_concatenate_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	concatenate_tok := expect_keyword_token(p, "CONCATENATE")
	stmt := ast.new(ast.Concatenate_Stmt, concatenate_tok.range)
	stmt.sources = make([dynamic]^ast.Expr)

	for p.curr_tok.kind != .EOF && p.curr_tok.kind != .Period && !check_keyword(p, "INTO") {
		source := parse_concatenate_source_expr(p)
		if source == nil {
			break
		}
		append(&stmt.sources, source)
	}

	expect_keyword_token(p, "INTO")
	if check_keyword(p, "DATA") {
		stmt.target = parse_data_inline_expr(p)
	} else {
		stmt.target = parse_expr(p)
	}

	for p.curr_tok.kind != .EOF && p.curr_tok.kind != .Period {
		if check_keyword(p, "SEPARATED") {
			advance_token(p)
			expect_keyword_token(p, "BY")
			stmt.separator = parse_concatenate_source_expr(p)
			continue
		}
		if check_keyword(p, "RESPECTING") {
			advance_token(p)
			expect_keyword_token(p, "BLANKS")
			stmt.respecting_blanks = true
			continue
		}
		break
	}

	period_tok := expect_token(p, .Period)
	stmt.range.end = period_tok.range.end
	return stmt
}

// REPLACE [ALL OCCURRENCES OF | FIRST OCCURRENCE OF] [REGEX] pattern
//
//	IN subject WITH replacement.
//
// REPLACE pattern WITH replacement INTO subject.
// REPLACE pattern IN subject WITH replacement.
// ... [IN CHARACTER MODE | IN BYTE MODE].
parse_replace_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	replace_tok := expect_keyword_token(p, "REPLACE")
	stmt := ast.new(ast.Replace_Stmt, replace_tok.range)
	stmt.scope = .Simple
	stmt.into_form = false

	if check_keyword(p, "ALL") {
		advance_token(p)
		expect_keyword_token(p, "OCCURRENCES")
		expect_keyword_token(p, "OF")
		stmt.scope = .All_Occurrences
	} else if check_keyword(p, "FIRST") {
		advance_token(p)
		expect_keyword_token(p, "OCCURRENCE")
		expect_keyword_token(p, "OF")
		stmt.scope = .First_Occurrence
	}

	if check_keyword(p, "REGEX") {
		advance_token(p)
		stmt.is_regex = true
	}

	// Avoid eating the following IN subject clause as relational IN (pattern IN subject).
	stmt.pattern = parse_concat_expr(p)

	if stmt.scope != .Simple {
		expect_keyword_token(p, "IN")
		stmt.subject = parse_concat_expr(p)
		expect_keyword_token(p, "WITH")
		stmt.replacement = parse_concat_expr(p)
	} else {
		if check_keyword(p, "IN") {
			advance_token(p)
			stmt.subject = parse_concat_expr(p)
			expect_keyword_token(p, "WITH")
			stmt.replacement = parse_concat_expr(p)
		} else {
			expect_keyword_token(p, "WITH")
			stmt.replacement = parse_concat_expr(p)
			expect_keyword_token(p, "INTO")
			stmt.into_form = true
			if is_inline_data_expr_start(p) {
				stmt.subject = parse_data_inline_expr(p)
			} else {
				stmt.subject = parse_concat_expr(p)
			}
		}
	}

	if check_keyword(p, "IN") {
		advance_token(p)
		if check_keyword(p, "CHARACTER") {
			advance_token(p)
		} else if check_keyword(p, "BYTE") {
			advance_token(p)
		} else {
			error(p, p.curr_tok.range, "expected CHARACTER or BYTE after IN")
		}
		expect_keyword_token(p, "MODE")
	}

	period_tok := expect_token(p, .Period)
	stmt.range.end = period_tok.range.end
	stmt.derived_stmt = stmt
	return stmt
}

parse_raise_exception_exporting_args :: proc(p: ^Parser, args: ^[dynamic]^ast.Named_Arg) {
	for p.curr_tok.kind != .Period && p.curr_tok.kind != .EOF {
		if p.curr_tok.kind != .Ident {
			break
		}

		param_name_tok := p.curr_tok
		advance_token(p)

		if p.curr_tok.kind != .Eq {
			error(
				p,
				p.curr_tok.range,
				"expected '=' after parameter name '%s'",
				param_name_tok.lit,
			)
			break
		}
		advance_token(p)

		param_value := parse_expr(p)
		if param_value == nil {
			break
		}

		skip_pragma(p)

		named_arg := ast.new(
			ast.Named_Arg,
			lexer.TextRange{param_name_tok.range.start, param_value.range.end},
		)
		named_arg.name = ast.new_ident(param_name_tok)
		named_arg.value = param_value
		named_arg.derived_expr = named_arg

		append(args, named_arg)
	}
}

parse_create_object_args :: proc(p: ^Parser, args: ^[dynamic]^ast.Named_Arg) {
	for p.curr_tok.kind != .Period && p.curr_tok.kind != .EOF {
		if check_keyword(p, "EXPORTING") ||
		   check_keyword(p, "EXCEPTIONS") ||
		   check_keyword(p, "AREA") {
			break
		}

		if p.curr_tok.kind != .Ident {
			break
		}

		param_name_tok := p.curr_tok
		advance_token(p)

		if p.curr_tok.kind != .Eq {
			error(
				p,
				p.curr_tok.range,
				"expected '=' after parameter name '%s'",
				param_name_tok.lit,
			)
			break
		}
		advance_token(p)

		param_value := parse_expr(p)
		if param_value == nil {
			break
		}

		skip_pragma(p)

		named_arg := ast.new(
			ast.Named_Arg,
			lexer.TextRange{param_name_tok.range.start, param_value.range.end},
		)
		named_arg.name = ast.new_ident(param_name_tok)
		named_arg.value = param_value
		named_arg.derived_expr = named_arg

		append(args, named_arg)
	}
}

// parse_create_object_stmt parses CREATE OBJECT statements.
// Syntax:
// - CREATE OBJECT oref.
// - CREATE OBJECT oref TYPE class [EXPORTING p1 = a1 ...] [EXCEPTIONS exc = rc ...].
// - CREATE OBJECT oref EXPORTING p1 = a1 ....
parse_create_object_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	create_tok := expect_keyword_token(p, "CREATE")
	expect_keyword_token(p, "OBJECT")

	target := parse_expr(p)

	stmt := ast.new(ast.Create_Object_Stmt, create_tok.range)
	stmt.target = target
	stmt.exporting = make([dynamic]^ast.Named_Arg)
	stmt.exceptions = make([dynamic]^ast.Named_Arg)
	stmt.derived_stmt = stmt

	if check_keyword(p, "TYPE") {
		advance_token(p)
		stmt.type_ref = parse_expr(p)
	}

	for p.curr_tok.kind != .Period && p.curr_tok.kind != .EOF {
		if check_keyword(p, "AREA") {
			advance_token(p)
			expect_keyword_token(p, "HANDLE")
			stmt.area_handle = parse_expr(p)
		} else if check_keyword(p, "EXPORTING") {
			advance_token(p)
			parse_create_object_args(p, &stmt.exporting)
		} else if check_keyword(p, "EXCEPTIONS") {
			advance_token(p)
			parse_create_object_args(p, &stmt.exceptions)
		} else {
			break
		}
	}

	period_tok := expect_token(p, .Period)
	stmt.range.end = period_tok.range.end
	return stmt
}

// parse_create_data_stmt parses CREATE DATA statements.
// Syntax (partial):
// - CREATE DATA dref TYPE type [LENGTH ...] [DECIMALS ...].
// - CREATE DATA dref LIKE dobj.
// - CREATE DATA dref TYPE HANDLE handle.
parse_create_data_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	create_tok := expect_keyword_token(p, "CREATE")
	expect_keyword_token(p, "DATA")

	target := parse_expr(p)

	stmt := ast.new(ast.Create_Data_Stmt, create_tok.range)
	stmt.target = target
	stmt.derived_stmt = stmt

	for p.curr_tok.kind != .Period && p.curr_tok.kind != .EOF {
		if check_keyword(p, "TYPE") {
			advance_token(p)
			if check_keyword(p, "HANDLE") {
				advance_token(p)
				stmt.type_handle = parse_expr(p)
			} else {
				stmt.type_ref = parse_type_expr(p)
				parse_optional_length_decimals(p)
			}
			continue
		}
		if check_keyword(p, "LIKE") {
			advance_token(p)
			like_expr := parse_simple_type_expr(p)
			if like_expr != nil {
				stmt.like_ref = like_expr
			} else {
				stmt.like_ref = parse_expr(p)
			}
			continue
		}
		break
	}

	period_tok := expect_token(p, .Period)
	stmt.range.end = period_tok.range.end
	return stmt
}

// parse_raise_stmt parses RAISE statements.
// Syntax:
// - RAISE exc. (non-class-based exception)
// - RAISE [RESUMABLE] EXCEPTION TYPE cx_class [EXPORTING p1 = a1 ...].
// - RAISE [RESUMABLE] EXCEPTION oref.
parse_raise_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	raise_tok := expect_keyword_token(p, "RAISE")

	stmt := ast.new(ast.Raise_Exception_Stmt, raise_tok.range)
	stmt.exporting = make([dynamic]^ast.Named_Arg)
	stmt.derived_stmt = stmt

	if check_keyword(p, "RESUMABLE") {
		advance_token(p)
		stmt.is_resumable = true
	}

	if check_keyword(p, "EXCEPTION") {
		advance_token(p)

		if check_keyword(p, "TYPE") {
			advance_token(p)
			stmt.type_ref = parse_simple_type_expr(p)

			if check_keyword(p, "EXPORTING") {
				advance_token(p)
				parse_raise_exception_exporting_args(p, &stmt.exporting)
			}
		} else {
			stmt.oref = parse_expr(p)
		}
	} else {
		if stmt.is_resumable {
			error(p, p.curr_tok.range, "expected EXCEPTION after RAISE RESUMABLE")
			end_tok := skip_to_new_line(p)
			stmt.range.end = end_tok.range.end
			return stmt
		}
		stmt.legacy_exception = parse_expr(p)
		if stmt.legacy_exception == nil {
			error(p, p.curr_tok.range, "expected exception name after RAISE")
			end_tok := skip_to_new_line(p)
			stmt.range.end = end_tok.range.end
			return stmt
		}
	}

	period_tok := expect_token(p, .Period)
	stmt.range.end = period_tok.range.end
	return stmt
}

// parse_check_stmt parses a CHECK statement
// Syntax: CHECK logical_expression.
// Examples:
//   CHECK io_event IS BOUND.
//   CHECK io_event IS INSTANCE OF lcl_object_event.
//   CHECK ms_context-docnum = lo_other->ms_context-docnum.
//   CHECK lines( mt_objects ) = lines( lo_other->mt_objects ).
parse_check_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	check_tok := expect_keyword_token(p, "CHECK")
	cond := parse_logical_expr(p)
	period_tok := expect_token(p, .Period)

	check_stmt := ast.new(ast.Check_Stmt, check_tok, period_tok)
	check_stmt.cond = cond
	check_stmt.derived_stmt = check_stmt
	return check_stmt
}

// parse_return_stmt parses a RETURN statement
// Syntax: RETURN [expr].
parse_return_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	return_tok := expect_keyword_token(p, "RETURN")

	return_stmt := ast.new(ast.Return_Stmt, return_tok.range)
	results := make([dynamic]^ast.Expr)

	if p.curr_tok.kind != .Period {
		expr := parse_expr(p)
		if expr != nil {
			append(&results, expr)
		}
	}

	period_tok := expect_token(p, .Period)
	return_stmt.range.end = period_tok.range.end
	return_stmt.results = results[:]
	return_stmt.derived_stmt = return_stmt
	return return_stmt
}

// parse_assert_stmt parses an ASSERT statement
// Syntax: ASSERT logical_expression.
// Examples:
//   ASSERT <ls_outbound> IS ASSIGNED.
//   ASSERT gui_flag = abap_true.
parse_assert_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	assert_tok := expect_keyword_token(p, "ASSERT")
	cond := parse_logical_expr(p)
	period_tok := expect_token(p, .Period)

	assert_stmt := ast.new(ast.Assert_Stmt, assert_tok, period_tok)
	assert_stmt.cond = cond
	assert_stmt.derived_stmt = assert_stmt
	return assert_stmt
}

