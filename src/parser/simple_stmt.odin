package abap_frontend_parser

import "src:ast"
import "src:tokenizer"

import "core:mem"
import "core:strings"

simple_stmt_starts :: proc(p: ^Parser) -> bool {
	return(
		at_keyword(p, "CLEAR") ||
		at_keyword(p, "REFRESH") ||
		at_keyword(p, "FREE") ||
		at_keyword(p, "UNASSIGN") ||
		at_keyword(p, "MOVE") ||
		at_keyword(p, "ADD") ||
		at_keyword(p, "SUBTRACT") ||
		at_keyword(p, "MULTIPLY") ||
		at_keyword(p, "DIVIDE") ||
		at_keyword(p, "COMPUTE") ||
		at_keyword(p, "CONCATENATE") ||
		at_keyword(p, "SPLIT") ||
		at_keyword(p, "CONDENSE") ||
		at_keyword(p, "REPLACE") ||
		at_keyword(p, "TRANSLATE") ||
		at_keyword(p, "SHIFT") ||
		at_keyword(p, "FIND") ||
		at_keyword(p, "SEARCH") ||
		at_keyword(p, "PERFORM") ||
		at_keyword(p, "CALL") ||
		at_keyword(p, "SUBMIT") ||
		at_keyword(p, "MESSAGE") ||
		at_keyword(p, "WRITE") ||
		at_keyword(p, "ASSERT") ||
		at_keyword(p, "CHECK") ||
		at_keyword(p, "RETURN") ||
		at_keyword(p, "CONTINUE") ||
		at_keyword(p, "EXIT") ||
		at_keyword(p, "STOP") ||
		at_keyword(p, "COMMIT") ||
		at_keyword(p, "ROLLBACK") ||
		at_keyword(p, "DESCRIBE") ||
		at_keyword(p, "EXPORT") ||
		at_keyword(p, "IMPORT") ||
		at_keyword(p, "RECEIVE") ||
		at_keyword(p, "GET") ||
		at_keyword(p, "SET") ||
		at_keyword_phrase(p, "LOG-POINT") ||
		at_keyword(p, "RAISE") ||
		at_keyword_phrase(p, "AUTHORITY-CHECK") ||
		at_keyword_phrase(p, "FIELD-GROUPS") ||
		(at_keyword(p, "INSERT") && at_keyword_index(p, p.index + 1, "DUMMY")) ||
		at_keyword(p, "FIELD") ||
		at_keyword(p, "ASSIGN") ||
		at_keyword(p, "CREATE") ||
		at_keyword(p, "OVERLAY") ||
		at_keyword(p, "PACK") ||
		at_keyword(p, "UNPACK") ||
		at_keyword(p, "CONVERT") ||
		at_keyword(p, "WAIT") ||
		at_keyword(p, "SKIP") ||
		at_keyword(p, "ULINE") ||
		at_keyword_phrase(p, "NEW-LINE") ||
		at_keyword_phrase(p, "NEW-PAGE") ||
		at_keyword(p, "RESERVE") ||
		at_keyword(p, "BACK") ||
		at_keyword(p, "FORMAT") ||
		at_keyword(p, "POSITION") ||
		at_keyword(p, "HIDE") ||
		at_keyword_phrase(p, "SELECTION-SCREEN") ||
		at_keyword(p, "DEFINE") ||
		oop_simple_stmt_starts(p) \
	)
}

parse_simple_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	if !simple_full_period_stmt_starts(p) && !stmt_period_before_boundary(p, p.index) {
		error_current(p, "syntax error: expected '.' to end statement")
		recover_to_statement_boundary(p, nil, false)
		return nil
	}
	if at_keyword(p, "CLEAR") {
		return parse_clear_stmt(p)
	}
	if at_keyword(p, "REFRESH") {
		return parse_refresh_stmt(p)
	}
	if at_keyword(p, "FREE") {
		return parse_free_stmt(p)
	}
	if at_keyword(p, "UNASSIGN") {
		return parse_unassign_stmt(p)
	}
	if at_keyword(p, "MOVE") {
		return parse_move_stmt(p)
	}
	if at_keyword(p, "ADD") {
		return parse_add_stmt(p)
	}
	if at_keyword(p, "SUBTRACT") {
		return parse_subtract_stmt(p)
	}
	if at_keyword(p, "MULTIPLY") {
		return parse_multiply_stmt(p)
	}
	if at_keyword(p, "DIVIDE") {
		return parse_divide_stmt(p)
	}
	if at_keyword(p, "COMPUTE") {
		return parse_compute_stmt(p)
	}
	if at_keyword(p, "CONCATENATE") {
		return parse_concatenate_stmt(p)
	}
	if at_keyword(p, "SPLIT") {
		return parse_split_stmt(p)
	}
	if at_keyword(p, "CONDENSE") {
		return parse_condense_stmt(p)
	}
	if at_keyword(p, "REPLACE") {
		return parse_replace_stmt(p)
	}
	if at_keyword(p, "TRANSLATE") {
		return parse_translate_stmt(p)
	}
	if at_keyword(p, "SHIFT") {
		return parse_shift_stmt(p)
	}
	if at_keyword(p, "FIND") {
		return parse_find_stmt(p)
	}
	if at_keyword(p, "SEARCH") {
		return parse_search_stmt(p)
	}
	if at_keyword(p, "PERFORM") {
		return parse_perform_stmt(p)
	}
	if at_keyword(p, "CALL") {
		return parse_call_stmt(p)
	}
	if at_keyword(p, "SUBMIT") {
		return parse_submit_stmt(p)
	}
	if at_keyword(p, "MESSAGE") {
		return parse_message_stmt(p)
	}
	if at_keyword(p, "WRITE") {
		return parse_write_stmt(p)
	}
	if at_keyword(p, "ASSERT") {
		return parse_assert_stmt(p)
	}
	if at_keyword(p, "CHECK") {
		return parse_check_stmt(p)
	}
	if flow_stmt_starts(p) {
		return parse_flow_stmt(p)
	}
	if at_keyword(p, "COMMIT") || at_keyword(p, "ROLLBACK") {
		return parse_transaction_stmt(p)
	}
	if at_keyword(p, "DESCRIBE") {
		return parse_describe_stmt(p)
	}
	if at_keyword(p, "RECEIVE") && at_keyword_index(p, p.index + 1, "RESULTS") {
		return parse_receive_results_stmt(p)
	}
	if at_keyword(p, "SET") && at_keyword_index(p, p.index + 1, "CURSOR") {
		return parse_set_cursor_stmt(p)
	}
	if (at_keyword(p, "GET") || at_keyword(p, "SET")) && at_keyword_index(p, p.index + 1, "LOCALE") {
		return parse_locale_stmt(p)
	}
	if bit_stmt_starts(p) {
		return parse_bit_stmt(p)
	}
	if import_stmt_starts(p) {
		return parse_import_stmt(p)
	}
	if export_stmt_starts(p) {
		return parse_export_stmt(p)
	}
	if set_handler_stmt_starts(p) {
		return parse_set_handler_stmt(p)
	}
	if runtime_stmt_starts(p) {
		return parse_runtime_stmt(p)
	}
	if at_keyword(p, "RAISE") {
		return parse_raise_stmt(p)
	}
	if at_keyword_phrase(p, "AUTHORITY-CHECK") {
		return parse_authority_check_stmt(p)
	}
	if at_keyword_phrase(p, "FIELD-GROUPS") {
		return parse_field_groups_stmt(p)
	}
	if at_keyword(p, "INSERT") {
		return parse_insert_dummy_stmt(p)
	}
	if at_keyword(p, "FIELD") {
		return parse_field_stmt(p)
	}
	if at_keyword(p, "ASSIGN") {
		return parse_assign_field_stmt(p)
	}
	if at_keyword(p, "CREATE") {
		return parse_create_stmt(p)
	}
	if at_keyword(p, "CONVERT") {
		return parse_convert_stmt(p)
	}
	if at_keyword(p, "WAIT") {
		return parse_wait_stmt(p)
	}
	if text_transform_stmt_starts(p) {
		return parse_text_transform_stmt(p)
	}
	if list_control_stmt_starts(p) {
		return parse_list_control_stmt(p)
	}
	if at_keyword_phrase(p, "SELECTION-SCREEN") {
		return parse_selection_screen_stmt(p)
	}
	if at_keyword(p, "DEFINE") {
		return parse_macro_def_stmt(p)
	}
	return parse_oop_simple_stmt(p)
}

simple_full_period_stmt_starts :: proc(p: ^Parser) -> bool {
	return(
		at_keyword(p, "CALL") ||
		at_keyword(p, "RAISE") ||
		runtime_stmt_starts(p) ||
		at_keyword_phrase(p, "AUTHORITY-CHECK") ||
		at_keyword(p, "ASSIGN") ||
		(at_keyword(p, "CREATE") &&
				(at_keyword_index(p, p.index + 1, "OBJECT") ||
				 at_keyword_index(p, p.index + 1, "DATA"))) ||
		oop_simple_stmt_starts(p) \
	)
}

simple_stmt_done :: proc(p: ^Parser, body_start: int) -> bool {
	tok := current_token(p)
	return(
		tok.kind == .Period ||
		tok.kind == .Eof ||
		(p.index > body_start &&
				.Has_Newline_Before in tok.flags &&
				statement_lead_starts(p, p.index) &&
				!line_continuation_starts(p, p.index)) \
	)
}

simple_stmt_range :: proc(p: ^Parser, start: Token) -> tokenizer.Range {
	period := expect_token(p, .Period)
	return tokenizer.text_range(start.range.start, statement_end(p, period))
}

simple_current_keyword_in :: proc(p: ^Parser, keywords: []string) -> bool {
	for keyword in keywords {
		if at_keyword_phrase(p, keyword) {
			return true
		}
	}
	return false
}

token_in_keywords :: proc(p: ^Parser, tok: Token, keywords: []string) -> bool {
	for keyword in keywords {
		if token_is_keyword(p, tok, keyword) {
			return true
		}
	}
	return false
}

simple_expr :: proc(p: ^Parser, body_start: int, stop_keywords: []string) -> ^ast.Expr {
	if simple_stmt_done(p, body_start) ||
	   current_token(p).kind == .Comma ||
	   current_token(p).kind == .Colon ||
	   simple_current_keyword_in(p, stop_keywords) {
		return nil
	}
	if !expr_lead_token(current_token(p)) {
		return nil
	}
	return parse_expr(p)
}

required_simple_expr :: proc(p: ^Parser, body_start: int, stop_keywords: []string) -> ^ast.Expr {
	expr := simple_expr(p, body_start, stop_keywords)
	if expr == nil {
		error_current(p, "syntax error: expected expression")
	}
	return expr
}

required_simple_logical_expr :: proc(p: ^Parser, body_start: int, stop_keywords: []string) -> ^ast.Expr {
	if simple_stmt_done(p, body_start) ||
	   current_token(p).kind == .Comma ||
	   current_token(p).kind == .Colon ||
	   simple_current_keyword_in(p, stop_keywords) ||
	   !expr_lead_token(current_token(p)) {
		error_current(p, "syntax error: expected expression")
		return nil
	}
	old_stops := p.expr_stop_keywords
	p.expr_stop_keywords = stop_keywords
	defer p.expr_stop_keywords = old_stops
	expr := parse_logical_expr(p)
	if expr == nil {
		error_current(p, "syntax error: expected expression")
	}
	return expr
}

plain_current_expr :: proc(p: ^Parser) -> ^ast.Expr {
	tok := current_token(p)
	if tok.kind == .Number || tok.kind == .String {
		bump_token(p)
		expr := ast.new(ast.Literal_Expr, tok.range, p.allocator)
		expr.value = tokenizer.token_lexeme(tok, p.source)
		return expr
	}
	if tok.kind == .Ident {
		bump_token(p)
		expr := ast.new(ast.Ident_Expr, tok.range, p.allocator)
		expr.name = tokenizer.token_lexeme(tok, p.source)
		return expr
	}
	return nil
}

parse_exprs_until :: proc(
	p: ^Parser,
	body_start: int,
	stop_keywords: []string,
) -> [dynamic]^ast.Expr {
	values := make([dynamic]^ast.Expr, 0, 2, p.allocator)
	for !simple_stmt_done(p, body_start) && !simple_current_keyword_in(p, stop_keywords) {
		if allow_token(p, .Colon) || allow_token(p, .Comma) {
			continue
		}
		start := p.index
		value := simple_expr(p, body_start, stop_keywords)
		if value == nil {
			break
		}
		append(&values, value)
		ensure_forward_progress(p, start)
	}
	return values
}

consume_simple_entry_tail :: proc(p: ^Parser, body_start: int) {
	for !simple_stmt_done(p, body_start) && current_token(p).kind != .Comma {
		bump_token(p)
	}
}

parse_generic_simple_operands :: proc(
	p: ^Parser,
	body_start: int,
	stop_keywords: []string,
) -> [dynamic]^ast.Expr {
	values := make([dynamic]^ast.Expr, 0, 4, p.allocator)
	for !simple_stmt_done(p, body_start) && !simple_current_keyword_in(p, stop_keywords) {
		if allow_token(p, .Colon) || allow_token(p, .Comma) {
			continue
		}
		value_start := p.index
		first := current_token(p)
		last := first
		paren := 0
		bracket := 0
		brace := 0
		for !simple_stmt_done(p, body_start) {
			top := paren == 0 && bracket == 0 && brace == 0
			if top && (current_token(p).kind == .Comma ||
			           current_token(p).kind == .Colon ||
			           simple_current_keyword_in(p, stop_keywords)) {
				break
			}
			tok := bump_token(p)
			last = tok
			#partial switch tok.kind {
			case .LParen:
				paren += 1
			case .RParen:
				if paren > 0 {
					paren -= 1
				}
			case .LBracket:
				bracket += 1
			case .RBracket:
				if bracket > 0 {
					bracket -= 1
				}
			case .LBrace:
				brace += 1
			case .RBrace:
				if brace > 0 {
					brace -= 1
				}
			}
		}
		if first.kind != .Eof && last.kind != .Eof && first.range.start < last.range.end {
			value := type_ref_expr_from_tokens(p, value_start, p.index, -1, false, false)
			populate_raw_operand_facts(p, value, value_start, p.index)
			append(&values, value)
		} else {
			bump_token(p)
		}
	}
	return values
}

raw_period_done :: proc(p: ^Parser) -> bool {
	tok := current_token(p)
	return tok.kind == .Period || tok.kind == .Eof
}

parse_raw_operand_to_period :: proc(
	p: ^Parser,
	stop_keywords: []string,
	fill_parts := false,
	allow_leading_stop := false,
	raw_facts := true,
	set_name := false,
	skip_leading_dynamic_group := true,
) -> ^ast.Expr {
	if raw_period_done(p) ||
	   current_token(p).kind == .Comma ||
	   current_token(p).kind == .Colon ||
	   (!allow_leading_stop && raw_operand_stop_keyword(p, stop_keywords)) {
		return nil
	}
	start := p.index
	first := current_token(p)
	last := first
	paren := 0
	bracket := 0
	brace := 0
	for !raw_period_done(p) {
		top := paren == 0 && bracket == 0 && brace == 0
		if top && (current_token(p).kind == .Comma ||
		           current_token(p).kind == .Colon ||
		           ((!allow_leading_stop || p.index > start) &&
		            raw_operand_stop_keyword(p, stop_keywords))) {
			break
		}
		tok := bump_token(p)
		last = tok
		#partial switch tok.kind {
		case .LParen:
			paren += 1
		case .RParen:
			if paren > 0 {
				paren -= 1
			}
		case .LBracket:
			bracket += 1
		case .RBracket:
			if bracket > 0 {
				bracket -= 1
			}
		case .LBrace:
			brace += 1
		case .RBrace:
			if brace > 0 {
				brace -= 1
			}
		}
	}
	if first.kind == .Eof || last.kind == .Eof || first.range.start >= last.range.end {
		return nil
	}
	value := type_ref_expr_from_tokens(p, start, p.index, -1, set_name, fill_parts)
	if raw_facts {
		populate_raw_operand_facts(
			p,
			value,
			start,
			p.index,
			skip_leading_dynamic_group,
		)
	}
	return value
}

raw_operand_stop_keyword :: proc(p: ^Parser, stop_keywords: []string) -> bool {
	return simple_current_keyword_in(p, stop_keywords) && !type_ref_selector_field(p)
}

parse_generic_operands_to_period :: proc(
	p: ^Parser,
	stop_keywords: []string,
) -> [dynamic]^ast.Expr {
	values := make([dynamic]^ast.Expr, 0, 4, p.allocator)
	for !raw_period_done(p) && !simple_current_keyword_in(p, stop_keywords) {
		if allow_token(p, .Colon) || allow_token(p, .Comma) {
			continue
		}
		start := p.index
		value := parse_raw_operand_to_period(
			p,
			stop_keywords,
			false,
			false,
			true,
			false,
		)
		if value != nil {
			append(&values, value)
		}
		ensure_forward_progress(p, start)
	}
	return values
}

parse_call_operands_to_period :: proc(p: ^Parser) -> [dynamic]^ast.Expr {
	values := make([dynamic]^ast.Expr, 0, 4, p.allocator)
	for !raw_period_done(p) {
		if allow_token(p, .Colon) || allow_token(p, .Comma) {
			continue
		}
		start := p.index
		if call_argument_section_starts(p) {
			if section := parse_call_arg_section_expr(p); section != nil {
				append(&values, section)
			}
		} else if value := parse_raw_operand_to_period(
			p,
			[]string{"EXPORTING", "IMPORTING", "CHANGING", "TABLES", "RECEIVING", "EXCEPTIONS"},
		); value != nil {
			append(&values, value)
		}
		ensure_forward_progress(p, start)
	}
	return values
}

populate_raw_operand_facts :: proc(
	p: ^Parser,
	expr: ^ast.Type_Ref_Expr,
	start, end: int,
	skip_leading_dynamic_group := true,
) {
	if expr == nil {
		return
	}
	expr.raw_operand = true
	expr.raw_decls = make([dynamic]ast.Raw_Operand_Inline_Decl, 0, 1, p.allocator)
	expr.raw_refs = make([dynamic]ast.Raw_Operand_Ref, 0, 2, p.allocator)
	populate_raw_operand_fact_lists(
		p,
		start,
		end,
		&expr.raw_decls,
		&expr.raw_refs,
		skip_leading_dynamic_group,
	)
}

populate_raw_operand_fact_lists :: proc(
	p: ^Parser,
	start, end: int,
	decls: ^[dynamic]ast.Raw_Operand_Inline_Decl,
	refs: ^[dynamic]ast.Raw_Operand_Ref,
	skip_leading_dynamic_group := true,
) {
	i := start
	for i < end {
		if decl, next, ok := raw_operand_inline_decl_at(p, i, end); ok {
			append(decls, decl)
			i = next
			continue
		}
		if skip_leading_dynamic_group && raw_operand_dynamic_group_at(p, start, i, end) {
			i += 3
			continue
		}
		tok := p.tokens[i]
		text := tokenizer.token_lexeme(tok, p.source)
		if !raw_operand_ident_like(tok) || raw_operand_skip_keyword(text) {
			i += 1
			continue
		}
		if i + 1 < end && p.tokens[i + 1].kind == .Eq {
			i += 2
			continue
		}
		if ref, next, ok := raw_operand_selector_ref(p, i, end); ok {
			append(refs, ref)
			i = next
			continue
		}
		append(
			refs,
			ast.Raw_Operand_Ref {
				name = text,
				range = tok.range,
				call_like = i + 1 < end && p.tokens[i + 1].kind == .LParen,
			},
		)
		i += 1
	}
}

raw_operand_inline_decl_at :: proc(
	p: ^Parser,
	index, end: int,
) -> (
	ast.Raw_Operand_Inline_Decl,
	int,
	bool,
) {
	if index + 3 < end &&
	   token_is_keyword(p, p.tokens[index], "DATA") &&
	   p.tokens[index + 1].kind == .LParen &&
	   raw_operand_ident_like(p.tokens[index + 2]) &&
	   p.tokens[index + 3].kind == .RParen {
		name := p.tokens[index + 2]
		return ast.Raw_Operand_Inline_Decl {
				kind = .Data,
				name = tokenizer.token_lexeme(name, p.source),
				range = name.range,
			},
			index + 4,
			true
	}
	if index + 5 < end &&
	   token_is_keyword(p, p.tokens[index], "FIELD") &&
	   p.tokens[index + 1].kind == .Minus &&
	   token_is_keyword(p, p.tokens[index + 2], "SYMBOL") &&
	   p.tokens[index + 3].kind == .LParen &&
	   raw_operand_ident_like(p.tokens[index + 4]) &&
	   p.tokens[index + 5].kind == .RParen {
		name := p.tokens[index + 4]
		return ast.Raw_Operand_Inline_Decl {
				kind = .Field_Symbol,
				name = tokenizer.token_lexeme(name, p.source),
				range = name.range,
			},
			index + 6,
			true
	}
	return {}, index, false
}

raw_operand_dynamic_group_at :: proc(p: ^Parser, start, index, end: int) -> bool {
	if index + 2 >= end ||
	   p.tokens[index].kind != .LParen ||
	   !raw_operand_ident_like(p.tokens[index + 1]) ||
	   p.tokens[index + 2].kind != .RParen {
		return false
	}
	if index == start {
		return true
	}
	prev := p.tokens[index - 1].kind
	return prev == .Minus || prev == .Arrow || prev == .FatArrow || prev == .Tilde
}

raw_operand_selector_ref :: proc(
	p: ^Parser,
	start, end: int,
) -> (
	ast.Raw_Operand_Ref,
	int,
	bool,
) {
	base := p.tokens[start]
	if !raw_operand_ident_like(base) {
		return {}, start, false
	}
	i := start + 1
	path: [dynamic]ast.Raw_Operand_Path_Segment
	path_ready := false
	type_base := false
	for i + 1 < end {
		op := p.tokens[i]
		if op.kind != .Minus && op.kind != .Arrow && op.kind != .FatArrow && op.kind != .Tilde {
			break
		}
		field := p.tokens[i + 1]
		if !raw_operand_ident_like(field) && !(op.kind == .Arrow && field.kind == .Star) {
			break
		}
		if !path_ready {
			path = make([dynamic]ast.Raw_Operand_Path_Segment, 0, 2, p.allocator)
			path_ready = true
		}
		if len(path) == 0 && (op.kind == .FatArrow || op.kind == .Tilde) {
			type_base = true
		}
		append(
			&path,
			ast.Raw_Operand_Path_Segment {
				name = tokenizer.token_lexeme(field, p.source),
				range = field.range,
				selector = selector_op(op.kind),
			},
		)
		i += 2
	}
	if !path_ready {
		return {}, start, false
	}
	return ast.Raw_Operand_Ref {
			name = tokenizer.token_lexeme(base, p.source),
			range = base.range,
			type_base = type_base,
			path = path,
		},
		i,
		true
}

raw_operand_ident_like :: #force_inline proc(tok: Token) -> bool {
	return tok.kind == .Ident
}

raw_operand_skip_keyword :: proc(text: string) -> bool {
	keywords := [?]string {
		"AND", "OR", "NOT", "IS", "IN", "LET", "FOR", "WHERE", "UNTIL", "WHILE",
		"INIT", "NEXT", "WHEN", "THEN", "ELSE", "TYPE", "LIKE", "VALUE", "DATA",
		"FIELD", "SYMBOL", "EXPORTING", "IMPORTING", "CHANGING", "TABLES",
		"RECEIVING", "EXCEPTIONS", "USING", "RAISING", "RESUMABLE", "MESSAGE",
		"COMPONENT", "OF", "STRUCTURE", "REF", "TO", "INTO", "FROM", "BY", "WITH",
		"FIELDS", "LINES", "LINE", "TABLE", "OBJECT", "EXCEPTION", "EVENT",
	}
	for keyword in keywords {
		if strings.equal_fold(text, keyword) {
			return true
		}
	}
	return false
}

source_range_text :: proc(p: ^Parser, range: tokenizer.Range) -> string {
	return strings.clone(p.source[range.start:range.end], p.allocator)
}

flow_stmt_starts :: proc(p: ^Parser) -> bool {
	return at_keyword(p, "RETURN") || at_keyword(p, "CONTINUE") || at_keyword(p, "EXIT") || at_keyword(p, "STOP")
}

parse_assert_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "ASSERT")
	condition := parse_control_condition(p, "syntax error: expected condition after ASSERT")
	if condition == nil {
		return nil
	}
	period := expect_token_message(p, .Period, "syntax error: expected '.' after ASSERT condition")
	if period.kind != .Period {
		return nil
	}
	stmt := ast.new(ast.Assert_Stmt, tokenizer.text_range(start.range.start, period.range.end), p.allocator)
	stmt.condition = condition
	return stmt
}

parse_check_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "CHECK")
	condition := parse_control_condition(p, "syntax error: expected condition after CHECK")
	if condition == nil {
		return nil
	}
	period := expect_token_message(p, .Period, "syntax error: expected '.' after CHECK condition")
	if period.kind != .Period {
		return nil
	}
	stmt := ast.new(ast.Check_Stmt, tokenizer.text_range(start.range.start, period.range.end), p.allocator)
	stmt.condition = condition
	return stmt
}

parse_flow_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := bump_token(p)
	body_start := p.index
	stmt := ast.new(ast.Flow_Stmt, start.range, p.allocator)
	if token_is_keyword(p, start, "RETURN") {
		stmt.kind = .Return
	} else if token_is_keyword(p, start, "CONTINUE") {
		stmt.kind = .Continue
	} else if token_is_keyword(p, start, "EXIT") {
		stmt.kind = .Exit
	} else {
		stmt.kind = .Stop
		if !simple_stmt_done(p, body_start) {
			error_current(p, "syntax error: STOP does not allow additions")
		}
	}
	consume_simple_entry_tail(p, body_start)
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_transaction_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := bump_token(p)
	body_start := p.index
	stmt := ast.new(ast.Transaction_Stmt, start.range, p.allocator)
	stmt.kind = .Rollback if token_is_keyword(p, start, "ROLLBACK") else .Commit
	allow_keyword(p, "WORK")
	if allow_keyword(p, "AND") {
		stmt.wait = allow_keyword(p, "WAIT")
	}
	consume_simple_entry_tail(p, body_start)
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_describe_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "DESCRIBE")
	body_start := p.index
	table_lead := allow_keyword(p, "TABLE")
	stmt := ast.new(ast.Describe_Stmt, start.range, p.allocator)
	stmt.entries = make([dynamic]ast.Describe_Entry_Clause, 0, 2, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {
			continue
		}
		entry := ast.Describe_Entry_Clause{table = table_lead || allow_keyword(p, "TABLE")}
		entry.source = simple_expr(p, body_start, []string{"LINES", "TYPE", "LENGTH", "DECIMALS", "COMPONENTS", "KIND"})
		if allow_keyword(p, "LINES") {
			entry.target = simple_expr(p, body_start, []string{})
		}
		if entry.source != nil || entry.target != nil {
			append(&stmt.entries, entry)
		}
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

runtime_stmt_starts :: proc(p: ^Parser) -> bool {
	return(
		at_keyword(p, "GET") ||
		at_keyword(p, "SET") ||
		at_keyword(p, "EXPORT") ||
		at_keyword(p, "IMPORT") ||
		at_keyword(p, "RECEIVE") ||
		at_keyword_phrase(p, "LOG-POINT") \
	)
}

set_handler_stmt_starts :: proc(p: ^Parser) -> bool {
	return at_keyword(p, "SET") && at_keyword_index(p, p.index + 1, "HANDLER")
}

bit_stmt_starts :: proc(p: ^Parser) -> bool {
	return(
		(at_keyword(p, "GET") || at_keyword(p, "SET")) &&
		at_keyword_index(p, p.index + 1, "BIT") \
	)
}

parse_bit_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := current_token(p)
	stmt := ast.new(ast.Bit_Stmt, start.range, p.allocator)
	stmt.kind = .Get if allow_keyword(p, "GET") else .Set
	if stmt.kind == .Set {
		expect_keyword(p, "SET")
	}
	expect_keyword(p, "BIT")
	body_start := p.index
	stmt.position = required_simple_expr(p, body_start, []string{"OF"})
	if !allow_keyword(p, "OF") {
		error_current(p, "syntax error: expected keyword")
		return stmt
	}
	if stmt.kind == .Get {
		stmt.source = required_simple_expr(p, body_start, []string{"INTO"})
		if !allow_keyword(p, "INTO") {
			error_current(p, "syntax error: expected keyword")
			return stmt
		}
		stmt.target = required_simple_expr(p, body_start, []string{})
	} else {
		stmt.target = required_simple_expr(p, body_start, []string{"TO"})
		if !allow_keyword(p, "TO") {
			error_current(p, "syntax error: expected keyword")
			return stmt
		}
		stmt.value = required_simple_expr(p, body_start, []string{})
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_runtime_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := current_token(p)
	kind := ast.Runtime_Kind.Get
	subject := ast.Runtime_Subject.None
	if at_keyword(p, "SET") {
		expect_keyword(p, "SET")
		kind = .Set
	} else if at_keyword(p, "EXPORT") {
		expect_keyword(p, "EXPORT")
		kind = .Export
	} else if at_keyword(p, "IMPORT") {
		expect_keyword(p, "IMPORT")
		kind = .Import
	} else if at_keyword(p, "RECEIVE") {
		expect_keyword(p, "RECEIVE")
		kind = .Receive
	} else if at_keyword(p, "GET") {
		expect_keyword(p, "GET")
		if allow_keyword(p, "BADI") {
			kind = .Get_Badi
			subject = .Badi
		}
	} else {
		expect_keyword_phrase(p, "LOG-POINT")
		kind = .Log_Point
	}
	stmt := ast.new(ast.Runtime_Stmt, start.range, p.allocator)
	stmt.kind = kind
	stmt.subject = subject
	stmt.excluding = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	stmt.operands = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	if parse_runtime_detail(p, stmt) {
		stmt.range = simple_stmt_range(p, start)
		return stmt
	}
	stmt.operands = parse_generic_operands_to_period(p, []string{})
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

import_stmt_starts :: proc(p: ^Parser) -> bool {
	return at_keyword(p, "IMPORT") && data_cluster_medium_tail_present(p, "FROM")
}

export_stmt_starts :: proc(p: ^Parser) -> bool {
	return at_keyword(p, "EXPORT") && data_cluster_medium_tail_present(p, "TO")
}

data_cluster_medium_tail_present :: proc(p: ^Parser, keyword: string) -> bool {
	for i := p.index; i + 1 < len(p.tokens); i += 1 {
		tok := p.tokens[i]
		if tok.kind == .Period || tok.kind == .Eof {
			break
		}
		if !token_is_keyword(p, tok, keyword) {
			continue
		}
		if token_is_keyword(p, p.tokens[i + 1], "DATABASE") {
			return true
		}
		if i + 2 < len(p.tokens) {
			if token_is_keyword(p, p.tokens[i + 1], "DATA") &&
			   token_is_keyword(p, p.tokens[i + 2], "BUFFER") {
				return true
			}
			if token_is_keyword(p, p.tokens[i + 1], "INTERNAL") &&
			   token_is_keyword(p, p.tokens[i + 2], "TABLE") {
				return true
			}
			if token_is_keyword(p, p.tokens[i + 1], "MEMORY") &&
			   token_is_keyword(p, p.tokens[i + 2], "ID") {
				return true
			}
			if token_is_keyword(p, p.tokens[i + 1], "SHARED") &&
			   (token_is_keyword(p, p.tokens[i + 2], "MEMORY") ||
			    token_is_keyword(p, p.tokens[i + 2], "BUFFER")) {
				return true
			}
		}
	}
	return false
}

parse_import_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "IMPORT")
	body_start := p.index
	stmt := ast.new(ast.Import_Stmt, start.range, p.allocator)
	stmt.parameters = parse_data_cluster_parameters(p, body_start, "FROM", "TO")
	expect_keyword(p, "FROM")
	stmt.medium = parse_data_cluster_medium(p, body_start, "TO")
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_export_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "EXPORT")
	body_start := p.index
	stmt := ast.new(ast.Export_Stmt, start.range, p.allocator)
	stmt.parameters = parse_data_cluster_parameters(p, body_start, "TO", "FROM")
	expect_keyword(p, "TO")
	stmt.medium = parse_data_cluster_medium(p, body_start, "FROM")
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_data_cluster_parameters :: proc(
	p: ^Parser,
	body_start: int,
	stop_keyword: string,
	parameter_keyword: string,
) -> [dynamic]ast.Data_Cluster_Parameter_Clause {
	parameters := make([dynamic]ast.Data_Cluster_Parameter_Clause, 0, 2, p.allocator)
	allow_token(p, .Colon)
	for !data_cluster_parameters_done(p, stop_keyword) {
		if allow_token(p, .Comma) {
			continue
		}
		start := p.index
		parameter, ok := parse_data_cluster_parameter(p, body_start, stop_keyword, parameter_keyword)
		if ok {
			append(&parameters, parameter)
		}
		ensure_forward_progress(p, start)
	}
	return parameters
}

data_cluster_parameters_done :: proc(p: ^Parser, stop_keyword: string) -> bool {
	tok := current_token(p)
	return tok.kind == .Period || tok.kind == .Eof || at_keyword(p, stop_keyword)
}

parse_data_cluster_medium :: proc(
	p: ^Parser,
	body_start: int,
	work_area_keyword: string,
) -> ast.Data_Cluster_Medium_Clause {
	medium: ast.Data_Cluster_Medium_Clause
	if allow_keyword(p, "DATA") {
		expect_keyword(p, "BUFFER")
		medium.kind = .Data_Buffer
		medium.object = required_simple_expr(p, body_start, []string{})
	} else if allow_keyword(p, "INTERNAL") {
		expect_keyword(p, "TABLE")
		medium.kind = .Internal_Table
		medium.object = required_simple_expr(p, body_start, []string{})
	} else if allow_keyword(p, "MEMORY") {
		expect_keyword(p, "ID")
		medium.kind = .Memory_ID
		medium.id = required_simple_expr(p, body_start, []string{})
	} else {
		if allow_keyword(p, "SHARED") {
			medium.kind = .Shared_Memory
			if allow_keyword(p, "BUFFER") {
				medium.kind = .Shared_Buffer
			} else {
				expect_keyword(p, "MEMORY")
			}
		} else {
			expect_keyword(p, "DATABASE")
			medium.kind = .Database
		}
		parse_data_cluster_database_medium(p, body_start, work_area_keyword, &medium)
	}
	consume_simple_entry_tail(p, body_start)
	return medium
}

parse_data_cluster_database_medium :: proc(
	p: ^Parser,
	body_start: int,
	work_area_keyword: string,
	medium: ^ast.Data_Cluster_Medium_Clause,
) {
	dbtab := expect_token(p, .Ident)
	medium.dbtab = tokenizer.token_lexeme(dbtab, p.source)
	medium.dbtab_range = dbtab.range
	expect_token(p, .LParen)
	area := expect_token(p, .Ident)
	medium.area = tokenizer.token_lexeme(area, p.source)
	medium.area_range = area.range
	expect_token(p, .RParen)
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, work_area_keyword) {
			medium.work_area = required_simple_expr(p, body_start, []string{"CLIENT", "ID"})
		} else if allow_keyword(p, "CLIENT") {
			medium.client = required_simple_expr(p, body_start, []string{work_area_keyword, "ID"})
		} else if allow_keyword(p, "ID") {
			medium.id = required_simple_expr(p, body_start, []string{work_area_keyword, "CLIENT"})
		} else {
			break
		}
	}
}

parse_data_cluster_parameter :: proc(
	p: ^Parser,
	body_start: int,
	stop_keyword: string,
	parameter_keyword: string,
) -> (ast.Data_Cluster_Parameter_Clause, bool) {
	if data_cluster_parameters_done(p, stop_keyword) {
		return {}, false
	}
	parameter: ast.Data_Cluster_Parameter_Clause
	stop := []string{stop_keyword}
	if current_token(p).kind == .Ident && next_token_kind(p, 1) == .Eq {
		name := bump_token(p)
		expect_token(p, .Eq)
		parameter.name = tokenizer.token_lexeme(name, p.source)
		parameter.name_range = name.range
		parameter.value = required_simple_expr(p, body_start, stop)
		return parameter, true
	}
	if current_token(p).kind == .Ident && at_keyword_index(p, p.index + 1, parameter_keyword) {
		name := bump_token(p)
		expect_keyword(p, parameter_keyword)
		parameter.name = tokenizer.token_lexeme(name, p.source)
		parameter.name_range = name.range
		parameter.value = required_simple_expr(p, body_start, stop)
		return parameter, true
	}
	parameter.value = required_simple_expr(p, body_start, stop)
	return parameter, parameter.value != nil
}

parse_runtime_detail :: proc(p: ^Parser, stmt: ^ast.Runtime_Stmt) -> bool {
	body_start := p.index
	if stmt.kind == .Get {
		if allow_keyword(p, "RUN") {
			allow_keyword(p, "TIME")
			allow_keyword(p, "FIELD")
			stmt.subject = .Run_Time_Field
			stmt.target = simple_expr(p, body_start, []string{})
			return true
		}
		if allow_keyword(p, "TIME") {
			allow_keyword(p, "STAMP")
			allow_keyword(p, "FIELD")
			stmt.subject = .Time_Stamp_Field
			stmt.target = simple_expr(p, body_start, []string{})
			return true
		}
		if allow_keyword(p, "PARAMETER") {
			allow_keyword(p, "ID")
			stmt.subject = .Parameter_ID_Field
			stmt.id = simple_expr(p, body_start, []string{"FIELD"})
			allow_keyword(p, "FIELD")
			stmt.field = simple_expr(p, body_start, []string{})
			return true
		}
		if allow_keyword(p, "CURSOR") {
			stmt.subject = .Cursor
			parse_cursor_runtime_tail(p, stmt)
			return true
		}
		if allow_keyword(p, "REFERENCE") {
			allow_keyword(p, "OF")
			stmt.subject = .Reference
			stmt.value = simple_expr(p, body_start, []string{"INTO"})
			if allow_keyword(p, "INTO") {
				stmt.target = simple_expr(p, body_start, []string{})
			}
			return true
		}
		if stmt.subject == .Badi {
			stmt.target = simple_expr(p, body_start, []string{})
			return true
		}
		return false
	}
	if stmt.kind != .Set {
		return false
	}
	if allow_keyword(p, "PARAMETER") {
		allow_keyword(p, "ID")
		stmt.subject = .Parameter_ID_Field
		stmt.id = simple_expr(p, body_start, []string{"FIELD"})
		allow_keyword(p, "FIELD")
		stmt.field = simple_expr(p, body_start, []string{})
		return true
	}
	if allow_hyphen2(p, "PF", "STATUS") {
		stmt.subject = .PF_Status
		stmt.target = simple_expr(p, body_start, []string{"EXCLUDING"})
		if allow_keyword(p, "EXCLUDING") {
			values := parse_exprs_until(p, body_start, []string{})
			for value in values {append(&stmt.excluding, value)}
		}
		return true
	}
	if allow_keyword(p, "TITLEBAR") {
		stmt.subject = .Titlebar
		stmt.target = simple_expr(p, body_start, []string{"WITH"})
		stmt.operands = parse_generic_operands_to_period(p, []string{})
		return true
	}
	if allow_keyword(p, "SCREEN") {
		stmt.subject = .Screen
		stmt.target = simple_expr(p, body_start, []string{})
		return true
	}
	if allow_hyphen2(p, "USER", "COMMAND") {
		stmt.subject = .User_Command
		stmt.target = simple_expr(p, body_start, []string{})
		return true
	}
	if allow_keyword(p, "UPDATE") {
		if allow_keyword(p, "TASK") && allow_keyword(p, "LOCAL") {
			stmt.subject = .Update_Task_Local
			return true
		}
	}
	return false
}

parse_set_handler_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "SET")
	expect_keyword(p, "HANDLER")
	stmt := ast.new(ast.Set_Handler_Stmt, start.range, p.allocator)
	body_start := p.index
	stmt.handlers = parse_exprs_until(p, body_start, []string{"FOR", "ACTIVATION"})
	if allow_keyword(p, "FOR") {
		if allow_keyword(p, "ALL") {
			allow_keyword(p, "INSTANCES")
			stmt.all_instances = true
		} else {
			stmt.sender = simple_expr(p, body_start, []string{"ACTIVATION"})
		}
	}
	if allow_keyword(p, "ACTIVATION") {
		stmt.activation = simple_expr(p, body_start, []string{})
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_locale_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := current_token(p)
	stmt := ast.new(ast.Locale_Stmt, start.range, p.allocator)
	if allow_keyword(p, "GET") {
		stmt.kind = .Get
	} else {
		expect_keyword(p, "SET")
		stmt.kind = .Set
	}
	expect_keyword(p, "LOCALE")
	parse_locale_tail(p, stmt)
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_locale_tail :: proc(p: ^Parser, stmt: ^ast.Locale_Stmt) {
	body_start := p.index
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, "LANGUAGE") {
			stmt.language = simple_expr(p, body_start, []string{"COUNTRY", "MODIFIER"})
			continue
		}
		if allow_keyword(p, "COUNTRY") {
			stmt.country = simple_expr(p, body_start, []string{"LANGUAGE", "MODIFIER"})
			continue
		}
		if allow_keyword(p, "MODIFIER") {
			stmt.modifier = simple_expr(p, body_start, []string{"LANGUAGE", "COUNTRY"})
			continue
		}
		bump_token(p)
	}
}

parse_set_cursor_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "SET")
	expect_keyword(p, "CURSOR")
	body_start := p.index
	stmt := ast.new(ast.Set_Cursor_Stmt, start.range, p.allocator)
	if allow_keyword(p, "FIELD") {
		stmt.field = simple_expr(p, body_start, []string{"OFFSET"})
		if allow_keyword(p, "OFFSET") {
			stmt.offset = simple_expr(p, body_start, []string{})
		}
	} else {
		stmt.line = simple_expr(p, body_start, []string{})
		stmt.column = simple_expr(p, body_start, []string{})
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

RECEIVE_RESULTS_FUNCTION_STOP_KEYWORDS :: []string {
	"EXPORTING",
	"IMPORTING",
	"CHANGING",
	"TABLES",
	"RECEIVING",
	"EXCEPTIONS",
}

parse_receive_results_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "RECEIVE")
	expect_keyword(p, "RESULTS")
	expect_keyword(p, "FROM")
	expect_keyword(p, "FUNCTION")
	stmt := ast.new(ast.Receive_Results_Stmt, start.range, p.allocator)
	stmt.target = parse_raw_operand_to_period(p, RECEIVE_RESULTS_FUNCTION_STOP_KEYWORDS)
	parse_raw_call_arguments(p, &stmt.arg_sections, &stmt.named_args)
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_cursor_runtime_tail :: proc(p: ^Parser, stmt: ^ast.Runtime_Stmt) {
	body_start := p.index
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, "FIELD") {
			stmt.field = simple_expr(p, body_start, []string{"LINE", "OFFSET", "VALUE"})
			continue
		}
		if allow_keyword(p, "LINE") {
			stmt.line = simple_expr(p, body_start, []string{"FIELD", "OFFSET", "VALUE"})
			continue
		}
		if allow_keyword(p, "OFFSET") {
			stmt.offset = simple_expr(p, body_start, []string{"FIELD", "LINE", "VALUE"})
			continue
		}
		if allow_keyword(p, "VALUE") {
			stmt.value = simple_expr(p, body_start, []string{"FIELD", "LINE", "OFFSET"})
			continue
		}
		bump_token(p)
	}
}

parse_raise_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "RAISE")
	stmt := ast.new(ast.Raise_Stmt, start.range, p.allocator)
	if allow_keyword(p, "EVENT") {
		stmt.kind = .Event
	} else {
		stmt.kind = .Exception
		allow_keyword(p, "EXCEPTION")
	}
	if stmt.kind == .Exception && allow_keyword(p, "TYPE") {
		stmt.target_type = true
		stmt.target = parse_raw_operand_to_period(p, []string{"EXPORTING", "MESSAGE", "RESUMABLE"})
	} else {
		stmt.target = parse_raw_operand_to_period(p, []string{"EXPORTING", "TYPE", "MESSAGE", "RESUMABLE"})
	}
	stmt.operands = parse_generic_operands_to_period(p, []string{})
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_authority_check_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword_phrase(p, "AUTHORITY-CHECK")
	stmt := ast.new(ast.Authority_Check_Stmt, start.range, p.allocator)
	body_start := p.index
	stmt.operands = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	stmt.ids = make([dynamic]ast.Authority_Check_ID_Clause, 0, 2, p.allocator)
	if allow_keyword(p, "OBJECT") {
		stmt.object = simple_expr(p, body_start, []string{"ID"})
		for !simple_stmt_done(p, body_start) {
			if allow_keyword(p, "ID") {
				id := simple_expr(p, body_start, []string{"FIELD", "ID"})
				field: ^ast.Expr
				if allow_keyword(p, "FIELD") {
					field = simple_expr(p, body_start, []string{"ID"})
				}
				append(&stmt.ids, ast.Authority_Check_ID_Clause{id = id, field = field})
				continue
			}
			bump_token(p)
		}
	} else {
		stmt.operands = parse_generic_operands_to_period(p, []string{})
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_field_groups_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword_phrase(p, "FIELD-GROUPS")
	body_start := p.index
	stmt := ast.new(ast.Field_Groups_Stmt, start.range, p.allocator)
	stmt.groups = parse_generic_simple_operands(p, body_start, []string{})
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_insert_dummy_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "INSERT")
	body_start := p.index
	allow_keyword(p, "DUMMY")
	stmt := ast.new(ast.Insert_Dummy_Stmt, start.range, p.allocator)
	if allow_keyword(p, "INTO") {
		stmt.target = simple_expr(p, body_start, []string{})
	}
	consume_simple_entry_tail(p, body_start)
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_field_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "FIELD")
	body_start := p.index
	stmt := ast.new(ast.Field_Stmt, start.range, p.allocator)
	stmt.operands = parse_generic_simple_operands(p, body_start, []string{})
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

assign_field_expect_keyword :: proc(p: ^Parser, keyword, message: string) -> bool {
	if allow_keyword(p, keyword) {
		return true
	}
	error_current(p, message)
	return false
}

assign_field_finish_after_error :: proc(p: ^Parser, stmt: ^ast.Assign_Field_Stmt, start: Token) -> ^ast.Stmt {
	recover_to_statement_boundary(p, nil, false)
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_assign_field_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "ASSIGN")
	stmt := ast.new(ast.Assign_Field_Stmt, start.range, p.allocator)
	if allow_keyword(p, "COMPONENT") {
		stmt.component = parse_raw_operand_to_period(p, []string{"OF", "TO", "CASTING"})
		if stmt.component == nil {
			error_current(p, "syntax error: expected ASSIGN COMPONENT operand")
		}
		if !assign_field_expect_keyword(p, "OF", "syntax error: expected OF after ASSIGN COMPONENT") {
			return assign_field_finish_after_error(p, stmt, start)
		}
		if !assign_field_expect_keyword(p, "STRUCTURE", "syntax error: expected STRUCTURE after ASSIGN COMPONENT OF") {
			return assign_field_finish_after_error(p, stmt, start)
		}
		stmt.structure = parse_raw_operand_to_period(p, []string{"TO", "CASTING"})
		if stmt.structure == nil {
			error_current(p, "syntax error: expected ASSIGN STRUCTURE operand")
		}
	} else {
		stmt.source = parse_raw_operand_to_period(p, []string{"TO", "CASTING"})
		if stmt.source == nil {
			error_current(p, "syntax error: expected ASSIGN source operand")
		}
	}
	if !assign_field_expect_keyword(p, "TO", "syntax error: expected TO in ASSIGN statement") {
		return assign_field_finish_after_error(p, stmt, start)
	}
	stmt.target = parse_raw_operand_to_period(p, []string{"CASTING", "TYPE", "DECIMALS"})
	if stmt.target == nil {
		error_current(p, "syntax error: expected ASSIGN target")
	}
	parse_assign_field_casting_addition(p, stmt)
	if !raw_period_done(p) {
		error_current(p, "syntax error: unexpected ASSIGN addition")
		recover_to_statement_boundary(p, nil, false)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_assign_field_casting_addition :: proc(p: ^Parser, stmt: ^ast.Assign_Field_Stmt) {
	if !allow_keyword(p, "CASTING") {
		return
	}
	stmt.casting = true
	stmt.casting_range = previous_token(p).range
	if allow_keyword(p, "TYPE") {
		type_start := p.index
		stmt.casting_type = parse_create_type_ref_expr(p, []string{"DECIMALS"})
		if stmt.casting_type == nil {
			error_current(p, "syntax error: expected type after ASSIGN CASTING TYPE")
		} else if dynamic_type := create_dynamic_type_expr_at(p, type_start); dynamic_type != nil {
			create_type_ref_use_dynamic_facts(stmt.casting_type, dynamic_type, p.allocator)
		}
	}
	if allow_keyword(p, "DECIMALS") {
		stmt.casting_decimals = parse_raw_operand_to_period(
			p,
			[]string{"TYPE", "CASTING"},
		)
		if stmt.casting_decimals == nil {
			error_current(p, "syntax error: expected decimals after ASSIGN CASTING DECIMALS")
		}
	}
}

parse_create_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "CREATE")
	if allow_keyword(p, "DATA") {
		return parse_create_data_stmt(p, start)
	}
	allow_keyword(p, "OBJECT")
	return parse_create_object_stmt(p, start)
}

parse_create_object_stmt :: proc(p: ^Parser, start: Token) -> ^ast.Stmt {
	stmt := ast.new(ast.Create_Object_Stmt, start.range, p.allocator)
	stmt.target = parse_raw_operand_to_period(p, []string{"TYPE", "EXPORTING", "EXCEPTIONS"})
	stmt.type_ref, stmt.type_clause, stmt.type_dynamic, stmt.type_dynamic_expr =
		parse_create_type_addition(p, []string{"EXPORTING", "EXCEPTIONS"})
	stmt.operands = parse_call_operands_to_period(p)
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_create_data_stmt :: proc(p: ^Parser, start: Token) -> ^ast.Stmt {
	stmt := ast.new(ast.Create_Data_Stmt, start.range, p.allocator)
	stmt.target = parse_raw_operand_to_period(p, []string{"TYPE", "EXPORTING", "EXCEPTIONS"})
	if at_keyword(p, "TYPE") && at_keyword_index(p, p.index + 1, "HANDLE") {
		bump_token(p)
		bump_token(p)
		stmt.type_handle = parse_raw_operand_to_period(p, []string{"EXPORTING", "EXCEPTIONS"})
	} else {
		stmt.type_ref, stmt.type_clause, stmt.type_dynamic, stmt.type_dynamic_expr =
			parse_create_type_addition(p, []string{"EXPORTING", "EXCEPTIONS"})
	}
	stmt.operands = parse_call_operands_to_period(p)
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_create_type_addition :: proc(
	p: ^Parser,
	stop_keywords: []string,
) -> (^ast.Expr, ^ast.Data_Type_Clause, bool, ^ast.Expr) {
	if !allow_keyword(p, "TYPE") {
		return nil, nil, false, nil
	}
	type_start := p.index
	type_clause, dynamic_expr := parse_create_type_clause_tail(p, stop_keywords)
	type_dynamic := dynamic_expr != nil
	type_ref: ^ast.Expr
	if p.index > type_start {
		type_ref = type_ref_expr_from_tokens(p, type_start, p.index, -1, false, true)
		if type_dynamic {
			create_type_ref_use_dynamic_facts(type_ref, dynamic_expr, p.allocator)
		}
	}
	return type_ref, type_clause, type_dynamic, dynamic_expr
}

parse_create_type_clause_tail :: proc(
	p: ^Parser,
	stop_keywords: []string,
) -> (^ast.Data_Type_Clause, ^ast.Expr) {
	clause, _ := mem.new(ast.Data_Type_Clause, p.allocator)
	clause.form = .Type
	if allow_keyword(p, "LINE") {
		allow_keyword(p, "OF")
		clause.form = .Type_Line_Of
	} else if allow_keyword(p, "REF") {
		allow_keyword(p, "TO")
		clause.form = .Ref_To
	} else if allow_keyword(p, "RANGE") {
		allow_keyword(p, "OF")
		clause.form = .Range_Of
	} else if space2_at(p, p.index, "ANY", "TABLE") {
		bump_token(p)
		bump_token(p)
		clause.table_has_of = allow_keyword(p, "OF")
		clause.form = .Any_Table
	} else if space2_at(p, p.index, "INDEX", "TABLE") {
		bump_token(p)
		bump_token(p)
		clause.table_has_of = allow_keyword(p, "OF")
		clause.form = .Index_Table
	} else if allow_keyword(p, "STANDARD") {
		allow_keyword(p, "TABLE")
		clause.table_has_of = allow_keyword(p, "OF")
		clause.form = .Standard_Table
	} else if allow_keyword(p, "SORTED") {
		allow_keyword(p, "TABLE")
		clause.table_has_of = allow_keyword(p, "OF")
		clause.form = .Sorted_Table
	} else if allow_keyword(p, "HASHED") {
		allow_keyword(p, "TABLE")
		clause.table_has_of = allow_keyword(p, "OF")
		clause.form = .Hashed_Table
	} else if allow_keyword(p, "TABLE") {
		clause.table_has_of = allow_keyword(p, "OF")
		clause.form = .Table
	}
	if at_keyword(p, "INITIAL") {
		initial_size, ok := parse_type_clause_initial_size_addition(p, clause.form)
		if !ok {
			return nil, nil
		}
		clause.initial_size = initial_size
		return clause, nil
	}
	dynamic_expr := create_dynamic_type_expr_at(p, p.index)
	clause.type_ref = parse_create_type_ref_expr(p, stop_keywords)
	if at_keyword(p, "INITIAL") {
		initial_size, ok := parse_type_clause_initial_size_addition(p, clause.form)
		if !ok {
			return nil, dynamic_expr
		}
		clause.initial_size = initial_size
	}
	return clause, dynamic_expr
}

parse_create_type_ref_expr :: proc(p: ^Parser, stop_keywords: []string) -> ^ast.Expr {
	start := p.index
	if create_type_ref_done(p, start, stop_keywords) {
		return nil
	}
	paren, bracket, brace := 0, 0, 0
	name_end := -1
	key_clause: ^ast.Type_Ref_Key_Clause
	key_clauses := make([dynamic]^ast.Type_Ref_Key_Clause, 0, 1, p.allocator)
	for !create_type_ref_done(p, start, stop_keywords) {
		tok := current_token(p)
		top := paren == 0 && bracket == 0 && brace == 0
		if top {
			if at_keyword(p, "WITH") {
				if !type_ref_key_clause_starts(p, p.index) {
					break
				}
				if name_end < 0 && p.index > start {
					name_end = p.tokens[p.index - 1].range.end
				}
				next_key := parse_type_ref_key_clause(p)
				if key_clause == nil {
					key_clause = next_key
				}
				append(&key_clauses, next_key)
				continue
			}
			if p.index > start && type_ref_stop_keyword(p) && !type_ref_selector_field(p) {
				break
			}
		}
		#partial switch tok.kind {
		case .LParen:
			paren += 1
		case .RParen:
			if paren == 0 {
				break
			}
			paren -= 1
		case .LBracket:
			bracket += 1
		case .RBracket:
			if bracket == 0 {
				break
			}
			bracket -= 1
		case .LBrace:
			brace += 1
		case .RBrace:
			if brace == 0 {
				break
			}
			brace -= 1
		}
		bump_token(p)
	}
	if p.index <= start {
		return nil
	}
	if name_end < 0 {
		name_end = p.tokens[p.index - 1].range.end
	}
	expr := type_ref_expr_from_tokens(p, start, p.index, name_end)
	expr.key = key_clause
	expr.keys = key_clauses
	return expr
}

create_type_ref_done :: proc(
	p: ^Parser,
	start: int,
	stop_keywords: []string,
) -> bool {
	tok := current_token(p)
	return(
		tok.kind == .Period ||
		tok.kind == .Comma ||
		tok.kind == .Eof ||
		(p.index > start && simple_current_keyword_in(p, stop_keywords)) \
	)
}

create_dynamic_type_expr_at :: proc(p: ^Parser, index: int) -> ^ast.Expr {
	if index >= len(p.tokens) || p.tokens[index].kind != .LParen {
		return nil
	}
	close := matching_group_index(p, index, .LParen, .RParen)
	if close <= index + 1 {
		return nil
	}
	expr := type_ref_expr_from_tokens(p, index + 1, close, -1, false, false)
	populate_raw_operand_facts(p, expr, index + 1, close, false)
	return expr
}

create_type_ref_use_dynamic_facts :: proc(
	type_ref: ^ast.Expr,
	dynamic_expr: ^ast.Expr,
	allocator: mem.Allocator,
) {
	if type_ref == nil || dynamic_expr == nil {
		return
	}
	ref, ref_ok := type_ref.derived_expr.(^ast.Type_Ref_Expr)
	dynamic_ref, dynamic_ok := dynamic_expr.derived_expr.(^ast.Type_Ref_Expr)
	if !ref_ok || !dynamic_ok {
		return
	}
	ref.raw_operand = true
	ref.raw_decls = make([dynamic]ast.Raw_Operand_Inline_Decl, 0, len(dynamic_ref.raw_decls), allocator)
	ref.raw_refs = make([dynamic]ast.Raw_Operand_Ref, 0, len(dynamic_ref.raw_refs), allocator)
	for decl in dynamic_ref.raw_decls {
		append(&ref.raw_decls, decl)
	}
	for raw_ref in dynamic_ref.raw_refs {
		append(&ref.raw_refs, raw_ref)
	}
}

convert_time_stamp_stmt_starts :: proc(p: ^Parser) -> bool {
	return at_keyword(p, "CONVERT") &&
	       ((at_keyword_index(p, p.index + 1, "TIME") &&
	         at_keyword_index(p, p.index + 2, "STAMP")) ||
	        at_keyword_index(p, p.index + 1, "DATE"))
}

parse_convert_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	if convert_time_stamp_stmt_starts(p) {
		return parse_convert_time_stamp_stmt(p)
	}
	return parse_text_transform_stmt(p)
}

parse_convert_time_stamp_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "CONVERT")
	stmt := ast.new(ast.Convert_Time_Stamp_Stmt, start.range, p.allocator)
	if allow_keyword(p, "TIME") {
		expect_keyword(p, "STAMP")
		body_start := p.index
		stmt.kind = .Time_Stamp_To_Date_Time
		stmt.time_stamp = required_simple_expr(p, body_start, []string{"TIME"})
		expect_keyword(p, "TIME")
		expect_keyword(p, "ZONE")
		stmt.time_zone = required_simple_expr(p, body_start, []string{"INTO"})
		expect_keyword(p, "INTO")
		expect_keyword(p, "DATE")
		stmt.date = required_simple_expr(p, body_start, []string{"TIME"})
		expect_keyword(p, "TIME")
		stmt.time = required_simple_expr(p, body_start, []string{})
	} else {
		expect_keyword(p, "DATE")
		body_start := p.index
		stmt.kind = .Date_Time_To_Time_Stamp
		stmt.date = required_simple_expr(p, body_start, []string{"TIME"})
		expect_keyword(p, "TIME")
		stmt.time = required_simple_expr(p, body_start, []string{"INTO"})
		expect_keyword(p, "INTO")
		expect_keyword(p, "TIME")
		expect_keyword(p, "STAMP")
		stmt.time_stamp = required_simple_expr(p, body_start, []string{"TIME"})
		expect_keyword(p, "TIME")
		expect_keyword(p, "ZONE")
		stmt.time_zone = required_simple_expr(p, body_start, []string{})
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

text_transform_stmt_starts :: proc(p: ^Parser) -> bool {
	return(
		at_keyword(p, "OVERLAY") ||
		at_keyword(p, "PACK") ||
		at_keyword(p, "UNPACK") ||
		at_keyword(p, "CONVERT") \
	)
}

parse_text_transform_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := bump_token(p)
	body_start := p.index
	stmt := ast.new(ast.Text_Transform_Stmt, start.range, p.allocator)
	if token_is_keyword(p, start, "OVERLAY") {
		stmt.kind = .Overlay
	} else if token_is_keyword(p, start, "PACK") {
		stmt.kind = .Pack
	} else if token_is_keyword(p, start, "UNPACK") {
		stmt.kind = .Unpack
	} else if token_is_keyword(p, start, "CONVERT") {
		stmt.kind = .Convert
	}
	stmt.operands = parse_generic_simple_operands(p, body_start, []string{})
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_wait_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "WAIT")
	body_start := p.index
	stmt := ast.new(ast.Wait_Stmt, start.range, p.allocator)
	if allow_keyword(p, "UNTIL") {
		stmt.condition = required_simple_logical_expr(p, body_start, []string{"UP"})
	}
	if allow_keyword(p, "UP") {
		expect_keyword(p, "TO")
		stmt.duration = required_simple_expr(p, body_start, []string{"SECONDS"})
		expect_keyword(p, "SECONDS")
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

list_control_stmt_starts :: proc(p: ^Parser) -> bool {
	return(
		at_keyword(p, "SKIP") ||
		at_keyword(p, "ULINE") ||
		at_keyword_phrase(p, "NEW-LINE") ||
		at_keyword_phrase(p, "NEW-PAGE") ||
		at_keyword(p, "RESERVE") ||
		at_keyword(p, "BACK") ||
		at_keyword(p, "FORMAT") ||
		at_keyword(p, "POSITION") ||
		at_keyword(p, "HIDE") \
	)
}

parse_list_control_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := current_token(p)
	stmt := ast.new(ast.List_Control_Stmt, start.range, p.allocator)
	if at_keyword_phrase(p, "NEW-LINE") {
		expect_keyword_phrase(p, "NEW-LINE")
		stmt.kind = .New_Line
	} else if at_keyword_phrase(p, "NEW-PAGE") {
		expect_keyword_phrase(p, "NEW-PAGE")
		stmt.kind = .New_Page
	} else {
		kw := bump_token(p)
		if token_is_keyword(p, kw, "SKIP") {
			stmt.kind = .Skip
		} else if token_is_keyword(p, kw, "ULINE") {
			stmt.kind = .Uline
		} else if token_is_keyword(p, kw, "RESERVE") {
			stmt.kind = .Reserve
		} else if token_is_keyword(p, kw, "BACK") {
			stmt.kind = .Back
		} else if token_is_keyword(p, kw, "FORMAT") {
			stmt.kind = .Format
		} else if token_is_keyword(p, kw, "POSITION") {
			stmt.kind = .Position
		} else {
			stmt.kind = .Hide
		}
	}
	body_start := p.index
	stmt.operands = parse_generic_simple_operands(p, body_start, []string{})
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_macro_def_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "DEFINE")
	name := first_name_token_until_period(p)
	consume_raw_until_period(p)
	header_period := expect_token(p, .Period)
	if header_period.kind != .Period {
		return nil
	}
	body_start := current_token(p).range.start
	for !at_eof(p) && !at_keyword_phrase(p, "END-OF-DEFINITION") {
		bump_token(p)
	}
	body_end := current_token(p).range.start
	end := expect_keyword_phrase(p, "END-OF-DEFINITION")
	if end.kind == .Eof {
		return nil
	}
	period := expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	stmt := ast.new(ast.Macro_Def_Stmt, tokenizer.text_range(start.range.start, period.range.end), p.allocator)
	stmt.name = tokenizer.token_lexeme(name, p.source) if name.kind != .Eof else ""
	if body_start < body_end {
		stmt.body = strings.clone(p.source[body_start:body_end], p.allocator)
	}
	return stmt
}

macro_call_stmt_starts :: proc(p: ^Parser) -> bool {
	return(
		current_token(p).kind == .Ident &&
		!known_stmt_lead_at(p, p.index) &&
		!direct_call_stmt_starts(p) &&
		stmt_period_before_boundary(p, p.index) \
	)
}

parse_macro_call_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := bump_token(p)
	body_start := p.index
	stmt := ast.new(ast.Macro_Call_Stmt, start.range, p.allocator)
	stmt.name = tokenizer.token_lexeme(start, p.source)
	stmt.args = parse_generic_simple_operands(p, body_start, []string{})
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_selection_screen_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword_phrase(p, "SELECTION-SCREEN")
	body_start := p.index
	stmt := ast.new(ast.Selection_Screen_Stmt, start.range, p.allocator)
	if allow_keyword(p, "COMMENT") {
		parse_selection_screen_comment(p, stmt, body_start)
	} else {
		for !simple_stmt_done(p, body_start) {
			if allow_keyword(p, "TITLE") {
				selection_screen_read_name(p, &stmt.title_name, &stmt.title_range)
				continue
			}
			bump_token(p)
		}
	}
	stmt.range = simple_stmt_range(p, start)
	stmt.text = source_range_text(p, stmt.range)
	return stmt
}

parse_selection_screen_comment :: proc(
	p: ^Parser,
	stmt: ^ast.Selection_Screen_Stmt,
	body_start: int,
) {
	parse_selection_screen_comment_position(p, body_start)
	selection_screen_read_name(p, &stmt.comment_name, &stmt.comment_range)
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, "FOR") {
			allow_keyword(p, "FIELD")
			selection_screen_read_name(p, &stmt.field_name, &stmt.field_range)
			continue
		}
		bump_token(p)
	}
}

parse_selection_screen_comment_position :: proc(p: ^Parser, body_start: int) {
	allow_token(p, .Slash)
	if !simple_stmt_done(p, body_start) && current_token(p).kind != .LParen {
		bump_token(p)
	}
	if allow_token(p, .LParen) {
		for !simple_stmt_done(p, body_start) && current_token(p).kind != .RParen {
			bump_token(p)
		}
		allow_token(p, .RParen)
	}
}

selection_screen_read_name :: proc(
	p: ^Parser,
	name: ^string,
	range: ^tokenizer.Range,
) -> bool {
	tok := current_token(p)
	if tok.kind != .Ident {
		return false
	}
	name^ = tokenizer.token_lexeme(tok, p.source)
	range^ = tok.range
	bump_token(p)
	return true
}

oop_simple_stmt_starts :: proc(p: ^Parser) -> bool {
	return(
		at_keyword(p, "PUBLIC") ||
		at_keyword(p, "PROTECTED") ||
		at_keyword(p, "PRIVATE") ||
		at_keyword(p, "METHODS") ||
		at_keyword_phrase(p, "CLASS-METHODS") ||
		at_keyword(p, "INTERFACES") ||
		at_keyword(p, "EVENTS") ||
		at_keyword_phrase(p, "CLASS-EVENTS") ||
		at_keyword(p, "ALIASES") \
	)
}

parse_oop_simple_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := current_token(p)
	stmt := ast.new(ast.Oop_Simple_Stmt, start.range, p.allocator)
	if at_keyword(p, "PUBLIC") {
		stmt.kind = .Class_Section
		stmt.visibility = .Public
	} else if at_keyword(p, "PROTECTED") {
		stmt.kind = .Class_Section
		stmt.visibility = .Protected
	} else if at_keyword(p, "PRIVATE") {
		stmt.kind = .Class_Section
		stmt.visibility = .Private
	} else if at_keyword_phrase(p, "CLASS-METHODS") {
		stmt.kind = .Class_Methods
	} else if at_keyword(p, "METHODS") {
		stmt.kind = .Methods
	} else if at_keyword(p, "INTERFACES") {
		stmt.kind = .Interfaces
	} else if at_keyword_phrase(p, "CLASS-EVENTS") {
		stmt.kind = .Class_Events
	} else if at_keyword(p, "EVENTS") {
		stmt.kind = .Events
	} else {
		stmt.kind = .Aliases
	}
	if stmt.kind != .Class_Section {
		switch stmt.kind {
		case .Class_Methods:
			expect_keyword_phrase(p, "CLASS-METHODS")
		case .Methods:
			expect_keyword(p, "METHODS")
		case .Interfaces:
			expect_keyword(p, "INTERFACES")
		case .Class_Events:
			expect_keyword_phrase(p, "CLASS-EVENTS")
		case .Events:
			expect_keyword(p, "EVENTS")
		case .Aliases:
			expect_keyword(p, "ALIASES")
		case .Class_Section:
		case .Class_Deferred:
		case .Interface_Deferred:
		case .Class_Load:
		case .Interface_Load:
		}
		if stmt.kind == .Aliases {
			if !parse_oop_aliases(p, stmt) {
				return nil
			}
			stmt.range = simple_stmt_range(p, start)
			stmt.text = source_range_text(p, stmt.range)
			return stmt
		}
		stmt.members = make([dynamic]ast.Oop_Member_Clause, 0, 2, p.allocator)
		parse_oop_members(p, stmt)
		stmt.range = simple_stmt_range(p, start)
		stmt.text = source_range_text(p, stmt.range)
		return stmt
	}
	consume_raw_until_top_level_period(p)
	period := expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	stmt.range = tokenizer.text_range(start.range.start, period.range.end)
	stmt.text = source_range_text(p, stmt.range)
	return stmt
}

parse_oop_aliases :: proc(p: ^Parser, stmt: ^ast.Oop_Simple_Stmt) -> bool {
	stmt.aliases = make([dynamic]ast.Oop_Alias_Clause, 0, 2, p.allocator)
	stmt.members = make([dynamic]ast.Oop_Member_Clause, 0, 2, p.allocator)
	allow_token(p, .Colon)
	for current_token(p).kind != .Period && current_token(p).kind != .Eof {
		if allow_token(p, .Comma) {
			continue
		}
		alias, ok := parse_oop_alias_clause(p)
		if !ok {
			return false
		}
		append(&stmt.aliases, alias)
		append(&stmt.members, oop_member_from_alias(alias, p.allocator))
	}
	return true
}

parse_oop_alias_clause :: proc(p: ^Parser) -> (ast.Oop_Alias_Clause, bool) {
	name := current_token(p)
	if name.kind != .Ident {
		error_current(p, "syntax error: expected alias name")
		return {}, false
	}
	bump_token(p)
	if !allow_keyword(p, "FOR") {
		error_current(p, "syntax error: expected FOR in ALIASES statement")
		return {}, false
	}
	target_start := p.index
	consume_oop_alias_target(p)
	target := type_ref_expr_from_tokens(p, target_start, p.index)
	if target == nil ||
	   target.base_name == "" ||
	   len(target.path) != 1 ||
	   target.path[0].selector != .Tilde ||
	   target.path[0].name == "" {
		error(p, name.range, "syntax error: ALIASES target must be interface~member")
		return {}, false
	}
	return ast.Oop_Alias_Clause {
			name                   = tokenizer.token_lexeme(name, p.source),
			range                  = name.range,
			target                 = target,
			target_interface_name  = target.base_name,
			target_interface_range = target.base_range,
			target_member_name     = target.path[0].name,
			target_member_range    = target.path[0].range,
		},
		true
}

consume_oop_alias_target :: proc(p: ^Parser) {
	paren, bracket, brace := 0, 0, 0
	for {
		tok := current_token(p)
		if tok.kind == .Eof {
			return
		}
		top := paren == 0 && bracket == 0 && brace == 0
		if top && (tok.kind == .Period || tok.kind == .Comma) {
			return
		}
		#partial switch tok.kind {
		case .LParen:
			paren += 1
		case .RParen:
			if paren == 0 {
				return
			}
			paren -= 1
		case .LBracket:
			bracket += 1
		case .RBracket:
			if bracket == 0 {
				return
			}
			bracket -= 1
		case .LBrace:
			brace += 1
		case .RBrace:
			if brace == 0 {
				return
			}
			brace -= 1
		}
		bump_token(p)
	}
}

oop_member_from_alias :: proc(alias: ast.Oop_Alias_Clause, allocator: mem.Allocator) -> ast.Oop_Member_Clause {
	values := make([dynamic]^ast.Expr, 0, 1, allocator)
	append(&values, alias.target)
	signatures := make([dynamic]ast.Oop_Signature_Clause, 0, 1, allocator)
	append(&signatures, ast.Oop_Signature_Clause{kind = .For, values = values})
	return ast.Oop_Member_Clause {
		name       = alias.name,
		range      = alias.range,
		signatures = signatures,
	}
}

parse_oop_members :: proc(p: ^Parser, stmt: ^ast.Oop_Simple_Stmt) {
	allow_token(p, .Colon)
	for current_token(p).kind != .Period && current_token(p).kind != .Eof {
		if allow_token(p, .Comma) {
			continue
		}
		name := current_token(p)
		if name.kind != .Ident {
			bump_token(p)
			continue
		}
		member_name, member_range, qualifier, qualifier_range, component_name, component_range, next_index, _ := qualified_ident_parts_at(p, p.index)
		p.index = next_index
		member := ast.Oop_Member_Clause {
			name            = member_name,
			range           = member_range,
			qualifier       = qualifier,
			qualifier_range = qualifier_range,
			member_name     = component_name,
			member_range    = component_range,
			signatures      = make([dynamic]ast.Oop_Signature_Clause, 0, 2, p.allocator),
		}
		for current_token(p).kind != .Period && current_token(p).kind != .Eof && current_token(p).kind != .Comma {
			if allow_keyword(p, "REDEFINITION") {
				if len(member.signatures) > 0 {
					error(p, previous_token(p).range, "syntax error: REDEFINITION method cannot declare a signature")
				}
				member.flags += {.Redefinition}
				if !oop_member_tail_done(p) {
					if _, signature_ok := oop_signature_kind(p); signature_ok {
						error_current(p, "syntax error: REDEFINITION method cannot declare a signature")
					} else {
						error_current(p, "syntax error: unexpected token after REDEFINITION")
					}
					consume_oop_member_tail(p)
					break
				}
				continue
			}
			if kind, ok := oop_signature_kind(p); ok {
				if .Redefinition in member.flags {
					error_current(p, "syntax error: REDEFINITION method cannot declare a signature")
					consume_oop_member_tail(p)
					break
				}
				bump_token(p)
				if kind == .For &&
				   (stmt.kind == .Methods || stmt.kind == .Class_Methods) &&
				   at_keyword(p, "EVENT") {
					event_handler, event_ok := parse_oop_event_handler_clause(p)
					if event_ok {
						member.event_handler = event_handler
					} else {
						consume_oop_member_tail(p)
						break
					}
					continue
				}
				append(&member.signatures, parse_oop_signature_clause(p, kind))
				continue
			}
			bump_token(p)
		}
		if member.qualifier != "" &&
		   (stmt.kind == .Methods || stmt.kind == .Class_Methods) &&
		   !(.Redefinition in member.flags) {
			error(p, member.range, "syntax error: qualified method declaration requires REDEFINITION")
		}
		append(&stmt.members, member)
	}
}

oop_member_tail_done :: proc(p: ^Parser) -> bool {
	return current_token(p).kind == .Period ||
	       current_token(p).kind == .Eof ||
	       current_token(p).kind == .Comma
}

consume_oop_member_tail :: proc(p: ^Parser) {
	for !oop_member_tail_done(p) {
		bump_token(p)
	}
}

parse_oop_signature_clause :: proc(p: ^Parser, kind: ast.Oop_Signature_Kind) -> ast.Oop_Signature_Clause {
	clause := ast.Oop_Signature_Clause {
		kind       = kind,
		values     = make([dynamic]^ast.Expr, 0, 2, p.allocator),
		parameters = make([dynamic]ast.Oop_Parameter_Clause, 0, 2, p.allocator),
	}
	if oop_signature_has_parameters(kind) {
		parse_oop_signature_parameters(p, &clause)
	} else {
		clause.values = parse_oop_signature_values(p)
	}
	return clause
}

parse_oop_event_handler_clause :: proc(p: ^Parser) -> (ast.Oop_Event_Handler_Clause, bool) {
	expect_keyword(p, "EVENT")
	event := current_token(p)
	if event.kind != .Ident {
		error_current(p, "syntax error: expected event name after FOR EVENT")
		return {}, false
	}
	bump_token(p)
	if !allow_keyword(p, "OF") {
		error_current(p, "syntax error: expected OF in FOR EVENT method declaration")
		return {}, false
	}
	source_type := parse_oop_type_ref_expr(p)
	if source_type == nil {
		error_current(p, "syntax error: expected event source type after FOR EVENT")
		return {}, false
	}
	return ast.Oop_Event_Handler_Clause {
			event_name = tokenizer.token_lexeme(event, p.source),
			event_range = event.range,
			source_type = source_type,
		},
		true
}

oop_signature_has_parameters :: proc(kind: ast.Oop_Signature_Kind) -> bool {
	return kind == .Importing ||
	       kind == .Exporting ||
	       kind == .Changing ||
	       kind == .Receiving ||
	       kind == .Returning
}

parse_oop_signature_parameters :: proc(p: ^Parser, clause: ^ast.Oop_Signature_Clause) {
	for !oop_signature_values_done(p) {
		start := p.index
		if parse_oop_signature_parameter(p, clause) {
			continue
		}
		ensure_forward_progress(p, start)
	}
}

parse_oop_signature_parameter :: proc(p: ^Parser, clause: ^ast.Oop_Signature_Clause) -> bool {
	start := p.index
	name, name_range, passing, ok := parse_oop_parameter_name(p)
	if !ok {
		return false
	}
	type_clause: ^ast.Data_Type_Clause
	if at_keyword(p, "TYPE") || at_keyword(p, "LIKE") {
		type_clause = parse_oop_parameter_type_clause(p)
	}
	optional := false
	for !oop_signature_values_done(p) {
		if allow_keyword(p, "OPTIONAL") {
			optional = true
			continue
		}
		if allow_keyword(p, "DEFAULT") {
			skip_oop_parameter_addition_value(p)
			continue
		}
		if allow_keyword(p, "PREFERRED") {
			allow_keyword(p, "PARAMETER")
			if current_token(p).kind == .Ident {
				bump_token(p)
			}
			continue
		}
		break
	}
	append(
		&clause.parameters,
		ast.Oop_Parameter_Clause {
			name = name,
			range = name_range,
			passing = passing,
			type_clause = type_clause,
			optional = optional,
		},
	)
	append_oop_signature_value(p, clause, start, p.index)
	return true
}

parse_oop_parameter_name :: proc(p: ^Parser) -> (
	string,
	tokenizer.Range,
	ast.Parameter_Passing_Kind,
	bool,
) {
	escaped := false
	if tokenizer.token_lexeme(current_token(p), p.source) == "!" {
		bump_token(p)
		escaped = true
	}
	if !escaped && (at_keyword(p, "VALUE") || at_keyword(p, "REFERENCE")) {
		passing := ast.Parameter_Passing_Kind.Value if at_keyword(p, "VALUE") else .Reference
		bump_token(p)
		allow_token(p, .LParen)
		tok := current_token(p)
		if tok.kind != .Ident {
			return "", tok.range, passing, false
		}
		bump_token(p)
		allow_token(p, .RParen)
		return tokenizer.token_lexeme(tok, p.source), tok.range, passing, true
	}
	tok := current_token(p)
	if tok.kind != .Ident {
		return "", tok.range, .Direct, false
	}
	bump_token(p)
	return tokenizer.token_lexeme(tok, p.source), tok.range, .Direct, true
}

parse_oop_parameter_type_clause :: proc(p: ^Parser) -> ^ast.Data_Type_Clause {
	keyword := bump_token(p)
	clause, _ := mem.new(ast.Data_Type_Clause, p.allocator)
	is_like := token_is_keyword(p, keyword, "LIKE")
	clause.form = .Like if is_like else .Type
	table_has_of := true
	if allow_keyword(p, "LINE") {
		allow_keyword(p, "OF")
		clause.form = .Like_Line_Of if is_like else .Type_Line_Of
	} else if !is_like && allow_keyword(p, "REF") {
		allow_keyword(p, "TO")
		clause.form = .Ref_To
	} else if !is_like && allow_keyword(p, "RANGE") {
		allow_keyword(p, "OF")
		clause.form = .Range_Of
	} else if !is_like && space2_at(p, p.index, "ANY", "TABLE") {
		bump_token(p)
		bump_token(p)
		table_has_of = allow_keyword(p, "OF")
		clause.table_has_of = table_has_of
		clause.form = .Any_Table
	} else if !is_like && space2_at(p, p.index, "INDEX", "TABLE") {
		bump_token(p)
		bump_token(p)
		table_has_of = allow_keyword(p, "OF")
		clause.table_has_of = table_has_of
		clause.form = .Index_Table
	} else if allow_keyword(p, "STANDARD") {
		allow_keyword(p, "TABLE")
		table_has_of = allow_keyword(p, "OF")
		clause.table_has_of = table_has_of
		clause.form = .Like_Standard_Table if is_like else .Standard_Table
	} else if allow_keyword(p, "SORTED") {
		allow_keyword(p, "TABLE")
		table_has_of = allow_keyword(p, "OF")
		clause.table_has_of = table_has_of
		clause.form = .Like_Sorted_Table if is_like else .Sorted_Table
	} else if allow_keyword(p, "HASHED") {
		allow_keyword(p, "TABLE")
		table_has_of = allow_keyword(p, "OF")
		clause.table_has_of = table_has_of
		clause.form = .Like_Hashed_Table if is_like else .Hashed_Table
	} else if allow_keyword(p, "TABLE") {
		table_has_of = allow_keyword(p, "OF")
		clause.table_has_of = table_has_of
		clause.form = .Like_Table if is_like else .Table
	}
	if !table_has_of {
		if at_keyword(p, "INITIAL") {
			initial_size, ok := parse_type_clause_initial_size_addition(p, clause.form)
			if !ok {
				return nil
			}
			clause.initial_size = initial_size
		}
		return clause
	}
	clause.type_ref = parse_oop_type_ref_expr(p)
	if at_keyword(p, "INITIAL") {
		initial_size, ok := parse_type_clause_initial_size_addition(p, clause.form)
		if !ok {
			return nil
		}
		clause.initial_size = initial_size
	}
	return clause
}

parse_oop_type_ref_expr :: proc(p: ^Parser) -> ^ast.Expr {
	start := p.index
	if oop_type_ref_done(p, start, false) {
		return nil
	}
	paren, bracket, brace := 0, 0, 0
	name_end := -1
	in_key := false
	for !oop_type_ref_done(p, start, in_key) {
		tok := current_token(p)
		top := paren == 0 && bracket == 0 && brace == 0
		if top && at_keyword(p, "WITH") {
			if !type_ref_key_clause_starts(p, p.index) {
				break
			}
			if name_end < 0 && p.index > start {
				name_end = previous_token(p).range.end
			}
			in_key = true
		}
		#partial switch tok.kind {
		case .LParen:
			paren += 1
		case .RParen:
			if paren == 0 {
				break
			}
			paren -= 1
		case .LBracket:
			bracket += 1
		case .RBracket:
			if bracket == 0 {
				break
			}
			bracket -= 1
		case .LBrace:
			brace += 1
		case .RBrace:
			if brace == 0 {
				break
			}
			brace -= 1
		}
		bump_token(p)
	}
	if p.index <= start {
		return nil
	}
	if name_end < 0 {
		name_end = p.tokens[p.index - 1].range.end
	}
	return type_ref_expr_from_tokens(p, start, p.index, name_end)
}

oop_type_ref_done :: proc(p: ^Parser, start: int, in_key: bool) -> bool {
	tok := current_token(p)
	if tok.kind == .Period || tok.kind == .Comma || tok.kind == .Eof {
		return true
	}
	if !type_ref_selector_field(p) &&
	   (simple_current_keyword_in(p, OOP_SIGNATURE_STOP_KEYWORDS) ||
	    at_length_keyword(p) ||
	    at_keyword(p, "INITIAL") ||
	    (at_keyword(p, "WITH") && !type_ref_key_clause_starts(p, p.index)) ||
	    at_keyword_phrase(p, "READ-ONLY") ||
	    at_keyword(p, "OPTIONAL") ||
	    at_keyword(p, "PREFERRED") ||
	    (!in_key && at_keyword(p, "DEFAULT"))) {
		return true
	}
	return p.index > start && oop_parameter_starts(p, p.index)
}

skip_oop_parameter_addition_value :: proc(p: ^Parser) {
	paren, bracket, brace := 0, 0, 0
	for !oop_signature_values_done(p) {
		top := paren == 0 && bracket == 0 && brace == 0
		if top && (at_keyword(p, "OPTIONAL") ||
		           at_keyword(p, "PREFERRED") ||
		           oop_parameter_starts(p, p.index)) {
			return
		}
		tok := bump_token(p)
		#partial switch tok.kind {
		case .LParen:
			paren += 1
		case .RParen:
			if paren > 0 {
				paren -= 1
			}
		case .LBracket:
			bracket += 1
		case .RBracket:
			if bracket > 0 {
				bracket -= 1
			}
		case .LBrace:
			brace += 1
		case .RBrace:
			if brace > 0 {
				brace -= 1
			}
		}
	}
}

append_oop_signature_value :: proc(
	p: ^Parser,
	clause: ^ast.Oop_Signature_Clause,
	start, end: int,
) {
	if start >= end {
		return
	}
	first := p.tokens[start]
	last := p.tokens[end - 1]
	if first.range.start >= last.range.end {
		return
	}
	value := type_ref_expr_from_tokens(p, start, end, -1, false)
	append(&clause.values, value)
}

oop_parameter_starts :: proc(p: ^Parser, index: int) -> bool {
	i := index
	escaped := false
	if i < len(p.tokens) && tokenizer.token_lexeme(p.tokens[i], p.source) == "!" {
		i += 1
		escaped = true
	}
	if !escaped && (at_keyword_index(p, i, "VALUE") || at_keyword_index(p, i, "REFERENCE")) {
		if i + 4 >= len(p.tokens) ||
		   p.tokens[i + 1].kind != .LParen ||
		   p.tokens[i + 2].kind != .Ident ||
		   p.tokens[i + 3].kind != .RParen {
			return false
		}
		i += 4
	} else if i < len(p.tokens) && p.tokens[i].kind == .Ident {
		i += 1
	} else {
		return false
	}
	return at_keyword_index(p, i, "TYPE") || at_keyword_index(p, i, "LIKE")
}

oop_signature_values_done :: proc(p: ^Parser) -> bool {
	return current_token(p).kind == .Period ||
	       current_token(p).kind == .Eof ||
	       current_token(p).kind == .Comma ||
	       current_token(p).kind == .Colon ||
	       simple_current_keyword_in(p, OOP_SIGNATURE_STOP_KEYWORDS)
}

parse_oop_signature_values :: proc(p: ^Parser) -> [dynamic]^ast.Expr {
	values := make([dynamic]^ast.Expr, 0, 2, p.allocator)
	for !oop_signature_values_done(p) {
		start := p.index
		value := parse_raw_operand_to_period(p, OOP_SIGNATURE_STOP_KEYWORDS, true)
		if value != nil {
			append(&values, value)
		}
		ensure_forward_progress(p, start)
	}
	return values
}

oop_signature_kind :: proc(p: ^Parser) -> (ast.Oop_Signature_Kind, bool) {
	if at_keyword(p, "IMPORTING") {
		return .Importing, true
	}
	if at_keyword(p, "EXPORTING") {
		return .Exporting, true
	}
	if at_keyword(p, "CHANGING") {
		return .Changing, true
	}
	if at_keyword(p, "RECEIVING") {
		return .Receiving, true
	}
	if at_keyword(p, "RETURNING") {
		return .Returning, true
	}
	if at_keyword(p, "RAISING") {
		return .Raising, true
	}
	if at_keyword(p, "EXCEPTIONS") {
		return .Exceptions, true
	}
	if at_keyword(p, "FOR") {
		return .For, true
	}
	return .Importing, false
}

OOP_SIGNATURE_STOP_KEYWORDS :: []string{"IMPORTING", "EXPORTING", "CHANGING", "RECEIVING", "RETURNING", "RAISING", "EXCEPTIONS", "FOR"}

parse_clear_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "CLEAR")
	body_start := p.index
	stmt := ast.new(ast.Clear_Stmt, start.range, p.allocator)
	stmt.operands = make([dynamic]ast.Clear_Operand_Clause, 0, 2, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {
			continue
		}
		target := required_simple_expr(p, body_start, []string{"WITH", "INITIAL"})
		if target == nil {
			break
		}
		clause := ast.Clear_Operand_Clause {
			target = target,
		}
		if allow_keyword(p, "WITH") {
			clause.mode = .With_Value
			clause.value = required_simple_expr(p, body_start, []string{})
		} else if allow_keyword(p, "INITIAL") {
			clause.mode = .Initial
		}
		append(&stmt.operands, clause)
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_refresh_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "REFRESH")
	body_start := p.index
	stmt := ast.new(ast.Refresh_Stmt, start.range, p.allocator)
	stmt.operands = make([dynamic]ast.Refresh_Operand_Clause, 0, 2, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {
			continue
		}
		table := allow_keyword(p, "TABLE")
		target := required_simple_expr(p, body_start, []string{})
		if target == nil {
			break
		}
		append(&stmt.operands, ast.Refresh_Operand_Clause{target = target, table = table})
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_free_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "FREE")
	body_start := p.index
	stmt := ast.new(ast.Free_Stmt, start.range, p.allocator)
	stmt.operands = make([dynamic]ast.Free_Operand_Clause, 0, 2, p.allocator)
	if allow_keyword(p, "MEMORY") {
		stmt.memory = true
		if allow_keyword(p, "ID") {
			stmt.memory_id = required_simple_expr(p, body_start, []string{})
		}
		consume_simple_entry_tail(p, body_start)
		stmt.range = simple_stmt_range(p, start)
		return stmt
	}
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {
			continue
		}
		object := allow_keyword(p, "OBJECT")
		target := required_simple_expr(p, body_start, []string{})
		if target == nil {
			break
		}
		append(&stmt.operands, ast.Free_Operand_Clause{target = target, object = object})
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_unassign_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "UNASSIGN")
	body_start := p.index
	stmt := ast.new(ast.Unassign_Stmt, start.range, p.allocator)
	stmt.operands = make([dynamic]ast.Unassign_Operand_Clause, 0, 1, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {
			continue
		}
		target := required_simple_expr(p, body_start, []string{})
		if target == nil {
			break
		}
		append(&stmt.operands, ast.Unassign_Operand_Clause{target = target})
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_move_entry :: proc(p: ^Parser, body_start: int) -> (ast.Move_Entry_Clause, bool) {
	source := required_simple_expr(p, body_start, []string{"TO"})
	if source == nil {
		return ast.Move_Entry_Clause{}, false
	}
	if !allow_keyword(p, "TO") {
		error_current(p, "syntax error: expected keyword")
		return ast.Move_Entry_Clause{}, false
	}
	target := required_simple_expr(p, body_start, []string{})
	if target == nil {
		return ast.Move_Entry_Clause{}, false
	}
	return ast.Move_Entry_Clause{source = source, target = target}, true
}

parse_move_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	if at_keyword_phrase(p, "MOVE-CORRESPONDING") {
		start := expect_keyword_phrase(p, "MOVE-CORRESPONDING")
		body_start := p.index
		stmt := ast.new(ast.Move_Corresponding_Stmt, start.range, p.allocator)
		stmt.entries = parse_move_entries(p, body_start)
		stmt.range = simple_stmt_range(p, start)
		return stmt
	}
	start := expect_keyword(p, "MOVE")
	body_start := p.index
	stmt := ast.new(ast.Move_Stmt, start.range, p.allocator)
	stmt.entries = parse_move_entries(p, body_start)
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_move_entries :: proc(p: ^Parser, body_start: int) -> [dynamic]ast.Move_Entry_Clause {
	entries := make([dynamic]ast.Move_Entry_Clause, 0, 2, p.allocator)
	allow_keyword(p, "EXACT")
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {
			continue
		}
		entry, ok := parse_move_entry(p, body_start)
		if !ok {
			break
		}
		append(&entries, entry)
		consume_simple_entry_tail(p, body_start)
	}
	return entries
}

parse_add_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "ADD")
	body_start := p.index
	stmt := ast.new(ast.Add_Stmt, start.range, p.allocator)
	stmt.entries = make([dynamic]ast.Add_Entry_Clause, 0, 2, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {
			continue
		}
		source := required_simple_expr(p, body_start, []string{"TO"})
		if source == nil {
			break
		}
		if !allow_keyword(p, "TO") {
			error_current(p, "syntax error: expected keyword")
			break
		}
		target := required_simple_expr(p, body_start, []string{"GIVING"})
		entry := ast.Add_Entry_Clause {
			source = source,
			target = target,
		}
		if allow_keyword(p, "GIVING") {
			entry.result = required_simple_expr(p, body_start, []string{})
		}
		append(&stmt.entries, entry)
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_subtract_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "SUBTRACT")
	body_start := p.index
	stmt := ast.new(ast.Subtract_Stmt, start.range, p.allocator)
	stmt.entries = make([dynamic]ast.Subtract_Entry_Clause, 0, 2, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {
			continue
		}
		source := required_simple_expr(p, body_start, []string{"FROM"})
		if source == nil {
			break
		}
		if !allow_keyword(p, "FROM") {
			error_current(p, "syntax error: expected keyword")
			break
		}
		target := required_simple_expr(p, body_start, []string{"GIVING"})
		entry := ast.Subtract_Entry_Clause {
			source = source,
			target = target,
		}
		if allow_keyword(p, "GIVING") {
			entry.result = required_simple_expr(p, body_start, []string{})
		}
		append(&stmt.entries, entry)
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_multiply_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "MULTIPLY")
	body_start := p.index
	stmt := ast.new(ast.Multiply_Stmt, start.range, p.allocator)
	stmt.entries = make([dynamic]ast.Multiply_Entry_Clause, 0, 2, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {
			continue
		}
		target := required_simple_expr(p, body_start, []string{"BY"})
		if target == nil {
			break
		}
		if !allow_keyword(p, "BY") {
			error_current(p, "syntax error: expected keyword")
			break
		}
		source := required_simple_expr(p, body_start, []string{"GIVING"})
		entry := ast.Multiply_Entry_Clause {
			target = target,
			source = source,
		}
		if allow_keyword(p, "GIVING") {
			entry.result = required_simple_expr(p, body_start, []string{})
		}
		append(&stmt.entries, entry)
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_divide_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "DIVIDE")
	body_start := p.index
	stmt := ast.new(ast.Divide_Stmt, start.range, p.allocator)
	stmt.entries = make([dynamic]ast.Divide_Entry_Clause, 0, 2, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {
			continue
		}
		first := required_simple_expr(p, body_start, []string{"BY", "INTO"})
		if first == nil {
			break
		}
		entry := ast.Divide_Entry_Clause{}
		if allow_keyword(p, "INTO") {
			entry.form = .Into
			entry.source = first
			entry.target = required_simple_expr(p, body_start, []string{"GIVING"})
		} else if allow_keyword(p, "BY") {
			entry.form = .By
			entry.target = first
			entry.source = required_simple_expr(p, body_start, []string{"GIVING"})
		} else {
			error_current(p, "syntax error: expected keyword")
			break
		}
		if allow_keyword(p, "GIVING") {
			entry.result = required_simple_expr(p, body_start, []string{})
		}
		append(&stmt.entries, entry)
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_compute_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "COMPUTE")
	body_start := p.index
	stmt := ast.new(ast.Compute_Stmt, start.range, p.allocator)
	stmt.entries = make([dynamic]ast.Compute_Entry_Clause, 0, 1, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {
			continue
		}
		entry := ast.Compute_Entry_Clause {
			exact = allow_keyword(p, "EXACT"),
		}
		entry.target = required_simple_expr(p, body_start, []string{})
		if entry.target == nil {
			break
		}
		if !allow_token(p, .Eq) {
			error_current(p, "syntax error: expected assignment operator")
			break
		}
		entry.source = required_simple_expr(p, body_start, []string{})
		append(&stmt.entries, entry)
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_concatenate_entry :: proc(
	p: ^Parser,
	body_start: int,
) -> (
	ast.Concatenate_Entry_Clause,
	bool,
	bool,
) {
	entry := ast.Concatenate_Entry_Clause{}
	byte_mode := false
	entry.sources = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	if allow_keyword(p, "LINES") {
		if !allow_keyword(
			p,
			"OF",
		) {
			error_current(p, "syntax error: expected keyword")
			return entry, false, false
		}
		entry.lines_of = true
		source := required_simple_expr(p, body_start, []string{"INTO"})
		if source == nil {
			return entry, false, false
		}
		append(&entry.sources, source)
	} else {
		entry.sources = parse_exprs_until(p, body_start, []string{"INTO"})
	}
	if !allow_keyword(p, "INTO") {
		error_current(p, "syntax error: expected keyword")
		return entry, false, false
	}
	entry.target = required_simple_expr(p, body_start, []string{"SEPARATED", "RESPECTING", "IN"})
	for !simple_stmt_done(p, body_start) && current_token(p).kind != .Comma {
		if allow_keyword(p, "SEPARATED") {
			if !allow_keyword(p, "BY") {
				error_current(p, "syntax error: expected keyword")
				break
			}
			entry.separator = required_simple_expr(p, body_start, []string{"RESPECTING", "IN"})
			continue
		}
		if allow_keyword(p, "RESPECTING") {
			entry.respecting_blanks = allow_keyword(p, "BLANKS")
			continue
		}
		if allow_keyword(p, "IN") {
			if allow_keyword(p, "BYTE") {
				byte_mode = allow_keyword(p, "MODE")
			} else if allow_keyword(p, "CHARACTER") {
				allow_keyword(p, "MODE")
			}
			continue
		}
		bump_token(p)
	}
	return entry, entry.target != nil && len(entry.sources) > 0, byte_mode
}

parse_concatenate_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "CONCATENATE")
	body_start := p.index
	stmt := ast.new(ast.Concatenate_Stmt, start.range, p.allocator)
	stmt.entries = make([dynamic]ast.Concatenate_Entry_Clause, 0, 1, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {
			continue
		}
		entry, ok, byte_mode := parse_concatenate_entry(p, body_start)
		if ok {
			append(&stmt.entries, entry)
			stmt.byte_mode = stmt.byte_mode || byte_mode
		} else {
			break
		}
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_split_entry :: proc(p: ^Parser, body_start: int) -> (ast.Split_Entry_Clause, bool) {
	entry := ast.Split_Entry_Clause{}
	entry.source = required_simple_expr(p, body_start, []string{"AT"})
	if entry.source == nil {
		return entry, false
	}
	if !allow_keyword(
		p,
		"AT",
	) {
		error_current(p, "syntax error: expected keyword")
		return entry, false
	}
	entry.separator = required_simple_expr(p, body_start, []string{"INTO"})
	if !allow_keyword(
		p,
		"INTO",
	) {
		error_current(p, "syntax error: expected keyword")
		return entry, false
	}
	entry.into_table = allow_keyword(p, "TABLE")
	entry.targets = parse_exprs_until(p, body_start, []string{"IN"})
	consume_simple_entry_tail(p, body_start)
	return entry, entry.separator != nil && len(entry.targets) > 0
}

parse_split_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "SPLIT")
	body_start := p.index
	stmt := ast.new(ast.Split_Stmt, start.range, p.allocator)
	stmt.entries = make([dynamic]ast.Split_Entry_Clause, 0, 1, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {
			continue
		}
		entry, ok := parse_split_entry(p, body_start)
		if ok {
			append(&stmt.entries, entry)
		} else {
			break
		}
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_condense_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "CONDENSE")
	body_start := p.index
	stmt := ast.new(ast.Condense_Stmt, start.range, p.allocator)
	stmt.target = required_simple_expr(p, body_start, []string{"NO"})
	if allow_keyword(p, "NO") {
		if allow_token(p, .Minus) && allow_keyword(p, "GAPS") {
			stmt.no_gaps = true
		}
	}
	consume_simple_entry_tail(p, body_start)
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

section_clause_starts :: proc(p: ^Parser) -> bool {
	return at_keyword(p, "SECTION") &&
	       (at_keyword_index(p, p.index + 1, "OFFSET") || at_keyword_index(p, p.index + 1, "LENGTH"))
}

parse_replace_section :: proc(p: ^Parser, stmt: ^ast.Replace_Stmt, body_start: int) {
	expect_keyword(p, "SECTION")
	has_bound := false
	if allow_keyword(p, "OFFSET") {
		has_bound = true
		stmt.section_offset = required_simple_expr(p, body_start, []string{"LENGTH", "OF"})
	}
	if allow_keyword(p, "LENGTH") {
		has_bound = true
		stmt.section_length = required_simple_expr(p, body_start, []string{"OF"})
	}
	if !has_bound {
		error_current(p, "syntax error: expected OFFSET or LENGTH after REPLACE SECTION")
	}
	if allow_keyword(p, "OF") {
		stmt.target = required_simple_expr(p, body_start, []string{"WITH", "IN"})
	} else {
		error_current(p, "syntax error: expected OF after REPLACE SECTION")
	}
}

parse_replace_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "REPLACE")
	body_start := p.index
	stmt := ast.new(ast.Replace_Stmt, start.range, p.allocator)
	if allow_keyword(p, "FIRST") {
		stmt.occurrence = .First
		allow_keyword(p, "OCCURRENCE")
		allow_keyword(p, "OCCURRENCES")
	} else if allow_keyword(p, "ALL") {
		stmt.occurrence = .All
		allow_keyword(p, "OCCURRENCE")
		allow_keyword(p, "OCCURRENCES")
	}
	allow_keyword(p, "OF")
	stmt.regex = allow_keyword(p, "REGEX")
	if section_clause_starts(p) {
		parse_replace_section(p, stmt, body_start)
	} else {
		stmt.pattern = required_simple_expr(p, body_start, []string{"IN", "WITH"})
	}
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, "IN") {
			if (allow_keyword(p, "CHARACTER") || allow_keyword(p, "BYTE")) &&
			   allow_keyword(p, "MODE") {
				continue
			}
			stmt.in_table = allow_keyword(p, "TABLE")
			stmt.target = required_simple_expr(p, body_start, []string{"WITH", "IN"})
			continue
		}
		if allow_keyword(p, "WITH") {
			stmt.replacement = required_simple_expr(p, body_start, []string{"IN"})
			continue
		}
		bump_token(p)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_translate_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "TRANSLATE")
	body_start := p.index
	stmt := ast.new(ast.Translate_Stmt, start.range, p.allocator)
	stmt.target = required_simple_expr(p, body_start, []string{"TO", "FROM", "USING"})
	if allow_keyword(p, "USING") {
		stmt.form = .Using
		stmt.operand = required_simple_expr(p, body_start, []string{})
	} else if allow_keyword(p, "TO") || allow_keyword(p, "FROM") {
		from_form := token_is_keyword(p, previous_token(p), "FROM")
		if allow_keyword(p, "UPPER") {
			allow_keyword(p, "CASE")
			stmt.form = .To_Upper
		} else if allow_keyword(p, "LOWER") {
			allow_keyword(p, "CASE")
			stmt.form = .To_Lower
		} else if allow_keyword(p, "CODE") {
			allow_keyword(p, "PAGE")
			stmt.form = .From_Code_Page if from_form else .To_Code_Page
			stmt.operand = required_simple_expr(p, body_start, []string{})
		} else if allow_keyword(p, "NUMBER") {
			allow_keyword(p, "FORMAT")
			stmt.form = .From_Number_Format if from_form else .To_Number_Format
			stmt.operand = required_simple_expr(p, body_start, []string{})
		}
	}
	consume_simple_entry_tail(p, body_start)
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_shift_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "SHIFT")
	body_start := p.index
	stmt := ast.new(ast.Shift_Stmt, start.range, p.allocator)
	stmt.target = required_simple_expr(
		p,
		body_start,
		[]string{"BY", "UP", "LEFT", "RIGHT", "CIRCULAR", "DELETING", "IN"},
	)
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, "LEFT") {
			stmt.direction = .Left
			continue
		}
		if allow_keyword(p, "RIGHT") {
			stmt.direction = .Right
			continue
		}
		if allow_keyword(p, "CIRCULAR") {
			stmt.circular = true
			continue
		}
		if allow_keyword(p, "BY") {
			stmt.places = required_simple_expr(
				p,
				body_start,
				[]string{"PLACES", "LEFT", "RIGHT", "CIRCULAR", "DELETING", "IN", "UP"},
			)
			allow_keyword(p, "PLACES")
			continue
		}
		if allow_keyword(p, "DELETING") {
			if allow_keyword(
				p,
				"LEADING",
			) {
				stmt.delete_direction = .Leading
			} else if allow_keyword(p, "TRAILING") {
				stmt.delete_direction = .Trailing
			}
			stmt.delete_pattern = required_simple_expr(
				p,
				body_start,
				[]string{"LEFT", "RIGHT", "CIRCULAR", "IN", "UP"},
			)
			continue
		}
		bump_token(p)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

FIND_TAIL_STOP_KEYWORDS :: []string{"MATCH", "SUBMATCHES", "RESULTS", "IGNORING", "RESPECTING"}

parse_find_section :: proc(p: ^Parser, stmt: ^ast.Find_Stmt, body_start: int) {
	expect_keyword(p, "SECTION")
	has_bound := false
	if allow_keyword(p, "OFFSET") {
		has_bound = true
		stmt.section_offset = required_simple_expr(p, body_start, []string{"LENGTH", "OF"})
	}
	if allow_keyword(p, "LENGTH") {
		has_bound = true
		stmt.section_length = required_simple_expr(p, body_start, []string{"OF"})
	}
	if !has_bound {
		error_current(p, "syntax error: expected OFFSET or LENGTH after FIND IN SECTION")
	}
	if allow_keyword(p, "OF") {
		stmt.target = required_simple_expr(p, body_start, FIND_TAIL_STOP_KEYWORDS)
	} else {
		error_current(p, "syntax error: expected OF after FIND IN SECTION")
	}
}

parse_find_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "FIND")
	body_start := p.index
	stmt := ast.new(ast.Find_Stmt, start.range, p.allocator)
	stmt.submatches = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	if allow_keyword(p, "FIRST") {
		stmt.occurrence = .First
		allow_keyword(p, "OCCURRENCE")
		allow_keyword(p, "OCCURRENCES")
	} else if allow_keyword(p, "ALL") {
		stmt.occurrence = .All
		allow_keyword(p, "OCCURRENCE")
		allow_keyword(p, "OCCURRENCES")
	}
	allow_keyword(p, "OF")
	stmt.regex = allow_keyword(p, "REGEX")
	stmt.pattern = required_simple_expr(p, body_start, []string{"IN"})
	if allow_keyword(p, "IN") {
		stmt.in_table = allow_keyword(p, "TABLE")
		if !stmt.in_table && section_clause_starts(p) {
			parse_find_section(p, stmt, body_start)
		} else {
			stmt.target = required_simple_expr(p, body_start, FIND_TAIL_STOP_KEYWORDS)
		}
	}
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, "MATCH") {
			if allow_keyword(p, "OFFSET") {
				stmt.match_offset = required_simple_expr(
					p,
					body_start,
					FIND_TAIL_STOP_KEYWORDS,
				)
			} else if allow_keyword(p, "LENGTH") {
				stmt.match_length = required_simple_expr(
					p,
					body_start,
					FIND_TAIL_STOP_KEYWORDS,
				)
			} else if allow_keyword(p, "LINE") {
				if !stmt.in_table {
					error_current(p, "syntax error: MATCH LINE requires FIND IN TABLE")
				}
				stmt.match_line = required_simple_expr(
					p,
					body_start,
					FIND_TAIL_STOP_KEYWORDS,
				)
			} else if allow_keyword(p, "COUNT") {
				stmt.match_count = required_simple_expr(
					p,
					body_start,
					FIND_TAIL_STOP_KEYWORDS,
				)
			} else {
				error_current(p, "syntax error: expected OFFSET, LENGTH, LINE, or COUNT after FIND MATCH")
			}
			continue
		}
		if allow_keyword(p, "RESULTS") {
			stmt.results = required_simple_expr(
				p,
				body_start,
				[]string{"MATCH", "SUBMATCHES", "IGNORING", "RESPECTING"},
			)
			continue
		}
		if allow_keyword(p, "SUBMATCHES") {
			more := parse_exprs_until(
				p,
				body_start,
				[]string{"MATCH", "RESULTS", "IGNORING", "RESPECTING"},
			)
			for value in more {append(&stmt.submatches, value)}
			continue
		}
		bump_token(p)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_search_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "SEARCH")
	body_start := p.index
	stmt := ast.new(ast.Search_Stmt, start.range, p.allocator)
	stmt.target = required_simple_expr(p, body_start, []string{"FOR"})
	if allow_keyword(p, "FOR") {
		stmt.pattern = required_simple_expr(
			p,
			body_start,
			[]string{"STARTING", "ENDING", "AND", "ABBREVIATED"},
		)
	}
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, "STARTING") {
			allow_keyword(p, "AT")
			stmt.starting_at = required_simple_expr(
				p,
				body_start,
				[]string{"ENDING", "AND", "ABBREVIATED"},
			)
			continue
		}
		if allow_keyword(p, "ENDING") {
			allow_keyword(p, "AT")
			stmt.ending_at = required_simple_expr(
				p,
				body_start,
				[]string{"STARTING", "AND", "ABBREVIATED"},
			)
			continue
		}
		if allow_keyword(p, "ABBREVIATED") {
			stmt.abbreviated = true
			continue
		}
		bump_token(p)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_perform_args :: proc(p: ^Parser, body_start: int, list: ^[dynamic]^ast.Expr) {
	for !simple_stmt_done(p, body_start) &&
	    !simple_current_keyword_in(p, []string{"TABLES", "USING", "CHANGING", "IF"}) {
		if allow_token(p, .Comma) {
			continue
		}
		value := simple_expr(p, body_start, []string{"TABLES", "USING", "CHANGING", "IF"})
		if value == nil {
			break
		}
		append(list, value)
	}
}

perform_expr_is_dynamic :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	_, ok := expr.derived_expr.(^ast.Paren_Expr)
	return ok
}

submit_static_target_is_valid :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	_, ok := expr.derived_expr.(^ast.Ident_Expr)
	return ok
}

submit_target_is_dynamic :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	_, ok := expr.derived_expr.(^ast.Paren_Expr)
	return ok
}

parse_perform_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "PERFORM")
	body_start := p.index
	stmt := ast.new(ast.Perform_Stmt, start.range, p.allocator)
	stmt.tables = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	stmt.using_args = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	stmt.changing = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	allow_token(p, .Colon)
	stmt.form = required_simple_expr(
		p,
		body_start,
		[]string{"IN", "TABLES", "USING", "CHANGING", "IF"},
	)
	if perform_expr_is_dynamic(stmt.form) {
		stmt.form_kind = .Dynamic
	}
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, "IN") {
			if !allow_keyword(
				p,
				"PROGRAM",
			) {
				error_current(p, "syntax error: expected keyword")
				break
			}
			stmt.has_program_clause = true
			if simple_stmt_done(p, body_start) {
				stmt.program_kind = .Omitted
				continue
			}
			if simple_current_keyword_in(p, []string{"TABLES", "USING", "CHANGING", "IF"}) {
				error_current(p, "syntax error: expected program after IN PROGRAM")
				continue
			}
			stmt.program = required_simple_expr(
				p,
				body_start,
				[]string{"TABLES", "USING", "CHANGING", "IF"},
			)
			stmt.program_kind = .Static
			if perform_expr_is_dynamic(stmt.program) {
				stmt.program_kind = .Dynamic
			}
			continue
		}
		if allow_keyword(p, "TABLES") {
			parse_perform_args(p, body_start, &stmt.tables)
			continue
		}
		if allow_keyword(p, "USING") {
			parse_perform_args(p, body_start, &stmt.using_args)
			continue
		}
		if allow_keyword(p, "CHANGING") {
			parse_perform_args(p, body_start, &stmt.changing)
			continue
		}
		if allow_keyword(p, "IF") {
			if !allow_keyword(p, "FOUND") {
				error_current(p, "syntax error: expected FOUND after IF")
				continue
			}
			stmt.if_found = true
			continue
		}
		bump_token(p)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_call_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "CALL")
	stmt := ast.new(ast.Call_Stmt, start.range, p.allocator)
	if allow_keyword(p, "METHOD") {
		stmt.kind = .Method
	} else if allow_keyword(p, "FUNCTION") {
		stmt.kind = .Function
	} else if allow_keyword(p, "CUSTOMER") {
		allow_token(p, .Minus)
		allow_keyword(p, "FUNCTION")
		stmt.kind = .Customer_Function
	} else if allow_keyword(p, "DATABASE") {
		allow_keyword(p, "PROCEDURE")
		stmt.kind = .Database_Procedure
	} else if allow_keyword(p, "TRANSFORMATION") {
		stmt.kind = .Transformation
	} else if allow_keyword(p, "BADI") {
		stmt.kind = .Badi
	} else if allow_keyword(p, "SCREEN") {
		stmt.kind = .Screen
	} else if allow_keyword(p, "SELECTION") {
		allow_token(p, .Minus)
		allow_keyword(p, "SCREEN")
		stmt.kind = .Selection_Screen
	} else if allow_keyword(p, "TRANSACTION") {
		stmt.kind = .Transaction
	} else if allow_keyword(p, "DIALOG") {
		stmt.kind = .Dialog
	} else if allow_keyword(p, "SUBSCREEN") {
		stmt.kind = .Subscreen
	}
	if stmt.kind == .Transaction {
		stmt.target = parse_raw_operand_to_period(
			p,
			[]string {
				"USING",
				"AND",
				"WITH",
				"WITHOUT",
				"MODE",
				"UPDATE",
				"MESSAGES",
				"OPTIONS",
			},
		)
	} else if stmt.kind == .Selection_Screen {
		stmt.target = parse_raw_operand_to_period(p, []string{"STARTING", "ENDING"})
	} else if stmt.kind == .Transformation {
		stmt.target = parse_raw_operand_to_period(
			p,
			CALL_TRANSFORMATION_CLAUSE_KEYWORDS,
			false,
			false,
			false,
			true,
		)
	} else if stmt.kind == .Method {
		stmt.target = parse_call_method_target_to_period(p, CALL_METHOD_TARGET_STOP_KEYWORDS)
	} else {
		stmt.target = parse_raw_operand_to_period(
			p,
			[]string {
				"EXPORTING",
				"IMPORTING",
				"CHANGING",
				"TABLES",
				"RECEIVING",
				"EXCEPTIONS",
				"USING",
				"AND",
				"WITH",
			},
		)
	}
	if stmt.kind == .Function || stmt.kind == .Method {
		parse_call_stmt_raw_arguments(p, stmt)
	} else if stmt.kind == .Transaction {
		parse_call_transaction_operands(p, stmt)
	} else if stmt.kind == .Transformation {
		parse_call_transformation_operands(p, stmt)
	} else {
		consume_raw_until_top_level_period(p)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

CALL_METHOD_TARGET_STOP_KEYWORDS :: []string {
	"EXPORTING",
	"IMPORTING",
	"CHANGING",
	"TABLES",
	"RECEIVING",
	"EXCEPTIONS",
	"USING",
	"AND",
	"WITH",
}

CALL_TRANSFORMATION_CLAUSE_KEYWORDS :: []string {
	"OPTIONS",
	"PARAMETERS",
	"SOURCE",
	"RESULT",
}

parse_call_method_target_to_period :: proc(p: ^Parser, stop_keywords: []string) -> ^ast.Expr {
	start := p.index
	raw := parse_raw_operand_to_period(p, stop_keywords)
	if raw == nil {
		return nil
	}
	if target, ok := dynamic_call_method_target_from_tokens(p, start, p.index); ok {
		return cast(^ast.Expr)target
	}
	return raw
}

dynamic_call_method_target_from_tokens :: proc(
	p: ^Parser,
	start, end: int,
) -> (
	^ast.Dynamic_Call_Method_Target_Expr,
	bool,
) {
	if op_index := call_method_dynamic_method_selector_index(p, start, end); op_index >= 0 {
		base, base_dynamic := call_method_target_receiver_part_expr(p, start, op_index)
		method, method_dynamic := call_method_target_part_expr(p, op_index + 1, end)
		if base == nil || method == nil || !method_dynamic {
			return nil, false
		}
		out := ast.new(
			ast.Dynamic_Call_Method_Target_Expr,
			tokenizer.text_range(p.tokens[start].range.start, p.tokens[end - 1].range.end),
			p.allocator,
		)
		out.base = base
		out.method = method
		out.selector = selector_op(p.tokens[op_index].kind)
		out.base_dynamic = base_dynamic
		out.method_dynamic = true
		return out, true
	}
	op_index := call_method_target_selector_index(p, start, end)
	if op_index < 0 {
		method, method_dynamic := call_method_target_part_expr(p, start, end)
		if !method_dynamic {
			return nil, false
		}
		out := ast.new(
			ast.Dynamic_Call_Method_Target_Expr,
			tokenizer.text_range(p.tokens[start].range.start, p.tokens[end - 1].range.end),
			p.allocator,
		)
		out.method = method
		out.method_dynamic = true
		return out, true
	}
	base, base_dynamic := call_method_target_part_expr(p, start, op_index)
	method, method_dynamic := call_method_target_part_expr(p, op_index + 1, end)
	if !base_dynamic && !method_dynamic {
		return nil, false
	}
	out := ast.new(
		ast.Dynamic_Call_Method_Target_Expr,
		tokenizer.text_range(p.tokens[start].range.start, p.tokens[end - 1].range.end),
		p.allocator,
	)
	out.base = base
	out.method = method
	out.selector = selector_op(p.tokens[op_index].kind)
	out.base_dynamic = base_dynamic
	out.method_dynamic = method_dynamic
	return out, true
}

call_method_target_part_expr :: proc(
	p: ^Parser,
	start, end: int,
) -> (
	^ast.Expr,
	bool,
) {
	if end <= start {
		return nil, false
	}
	inner_start, inner_end, is_dynamic := call_method_target_dynamic_group(p, start, end)
	if is_dynamic {
		if inner_end <= inner_start {
			return nil, true
		}
		expr := type_ref_expr_from_tokens(p, inner_start, inner_end, -1, true, true)
		populate_raw_operand_facts(p, expr, inner_start, inner_end, false)
		return cast(^ast.Expr)expr, true
	}
	return cast(^ast.Expr)type_ref_expr_from_tokens(p, start, end, -1, true, true), false
}

call_method_target_receiver_part_expr :: proc(
	p: ^Parser,
	start, end: int,
) -> (
	^ast.Expr,
	bool,
) {
	_, _, is_dynamic := call_method_target_dynamic_group(p, start, end)
	if is_dynamic {
		return call_method_target_part_expr(p, start, end)
	}
	if expr := parse_complete_logical_expr(p, start, end); expr != nil {
		return expr, false
	}
	return call_method_target_part_expr(p, start, end)
}

call_method_target_dynamic_group :: proc(
	p: ^Parser,
	start, end: int,
) -> (
	int,
	int,
	bool,
) {
	if start < end &&
	   p.tokens[start].kind == .LParen &&
	   matching_group_index(p, start, .LParen, .RParen) == end - 1 {
		return start + 1, end - 1, true
	}
	return start, end, false
}

call_method_dynamic_method_selector_index :: proc(p: ^Parser, start, end: int) -> int {
	paren, bracket, brace := 0, 0, 0
	for i in start ..< end {
		tok := p.tokens[i]
		top := paren == 0 && bracket == 0 && brace == 0
		if top && (tok.kind == .Arrow || tok.kind == .FatArrow) {
			_, _, method_dynamic := call_method_target_dynamic_group(p, i + 1, end)
			if method_dynamic {
				return i
			}
		}
		#partial switch tok.kind {
		case .LParen:
			paren += 1
		case .RParen:
			if paren > 0 {paren -= 1}
		case .LBracket:
			bracket += 1
		case .RBracket:
			if bracket > 0 {bracket -= 1}
		case .LBrace:
			brace += 1
		case .RBrace:
			if brace > 0 {brace -= 1}
		}
	}
	return -1
}

call_method_target_selector_index :: proc(p: ^Parser, start, end: int) -> int {
	paren, bracket, brace := 0, 0, 0
	for i in start ..< end {
		tok := p.tokens[i]
		top := paren == 0 && bracket == 0 && brace == 0
		if top && (tok.kind == .Arrow || tok.kind == .FatArrow || tok.kind == .Tilde) {
			return i
		}
		#partial switch tok.kind {
		case .LParen:
			paren += 1
		case .RParen:
			if paren > 0 {paren -= 1}
		case .LBracket:
			bracket += 1
		case .RBracket:
			if bracket > 0 {bracket -= 1}
		case .LBrace:
			brace += 1
		case .RBrace:
			if brace > 0 {brace -= 1}
		}
	}
	return -1
}

parse_call_transformation_operands :: proc(p: ^Parser, stmt: ^ast.Call_Stmt) {
	stmt.transformation_args = make([dynamic]ast.Call_Transformation_Arg, 0, 4, p.allocator)
	for !raw_period_done(p) {
		start := p.index
		if kind, ok := call_transformation_arg_kind(p, current_token(p)); ok {
			bump_token(p)
			parse_call_transformation_arg_list(p, stmt, kind)
			ensure_forward_progress(p, start)
			continue
		}
		bump_token(p)
	}
}

parse_call_transformation_arg_list :: proc(
	p: ^Parser,
	stmt: ^ast.Call_Stmt,
	kind: ast.Call_Transformation_Arg_Kind,
) {
	for !raw_period_done(p) && !simple_current_keyword_in(p, CALL_TRANSFORMATION_CLAUSE_KEYWORDS) {
		start := p.index
		if allow_token(p, .Comma) || allow_token(p, .Colon) {
			continue
		}
		append_call_transformation_arg(p, stmt, kind)
		ensure_forward_progress(p, start)
	}
}

append_call_transformation_arg :: proc(
	p: ^Parser,
	stmt: ^ast.Call_Stmt,
	kind: ast.Call_Transformation_Arg_Kind,
) {
	name := ""
	name_range := tokenizer.Range{}
	has_eq := false
	if call_transformation_named_arg_starts(p, p.index) {
		tok := bump_token(p)
		_ = expect_token(p, .Eq)
		name = tokenizer.token_lexeme(tok, p.source)
		name_range = tok.range
		has_eq = true
	} else if (kind == .Source || kind == .Result) && call_transformation_mode_token(p, current_token(p)) {
		tok := bump_token(p)
		name = tokenizer.token_lexeme(tok, p.source)
		name_range = tok.range
	}
	value_start := p.index
	value_end := call_transformation_arg_value_end(p, value_start)
	value: ^ast.Expr
	if value_start < value_end {
		value = type_ref_expr_from_tokens(p, value_start, value_end, -1, false, false)
		if raw, ok := value.derived_expr.(^ast.Type_Ref_Expr); ok {
			populate_raw_operand_facts(p, raw, value_start, value_end, false)
		}
	}
	append(
		&stmt.transformation_args,
		ast.Call_Transformation_Arg {
			kind = kind,
			name = name,
			name_range = name_range,
			has_eq = has_eq,
			value = value,
		},
	)
	for p.index < value_end {
		bump_token(p)
	}
}

call_transformation_arg_kind :: proc(
	p: ^Parser,
	tok: Token,
) -> (ast.Call_Transformation_Arg_Kind, bool) {
	if token_is_keyword(p, tok, "OPTIONS") {
		return .Options, true
	}
	if token_is_keyword(p, tok, "PARAMETERS") {
		return .Parameters, true
	}
	if token_is_keyword(p, tok, "SOURCE") {
		return .Source, true
	}
	if token_is_keyword(p, tok, "RESULT") {
		return .Result, true
	}
	return .Options, false
}

call_transformation_named_arg_starts :: proc(p: ^Parser, index: int) -> bool {
	if index + 1 >= len(p.tokens) {
		return false
	}
	return raw_operand_ident_like(p.tokens[index]) && p.tokens[index + 1].kind == .Eq
}

call_transformation_mode_token :: proc(p: ^Parser, tok: Token) -> bool {
	return token_is_keyword(p, tok, "XML") || token_is_keyword(p, tok, "JSON")
}

call_transformation_arg_value_end :: proc(p: ^Parser, start: int) -> int {
	i := start
	paren, bracket, brace := 0, 0, 0
	for i < len(p.tokens) {
		tok := p.tokens[i]
		top := paren == 0 && bracket == 0 && brace == 0
		if top {
			if tok.kind == .Period || tok.kind == .Eof || tok.kind == .Comma ||
			   token_in_keywords(p, tok, CALL_TRANSFORMATION_CLAUSE_KEYWORDS) ||
			   (i > start && call_transformation_named_arg_starts(p, i)) {
				break
			}
		}
		#partial switch tok.kind {
		case .LParen:
			paren += 1
		case .RParen:
			if paren > 0 {
				paren -= 1
			}
		case .LBracket:
			bracket += 1
		case .RBracket:
			if bracket > 0 {
				bracket -= 1
			}
		case .LBrace:
			brace += 1
		case .RBrace:
			if brace > 0 {
				brace -= 1
			}
		}
		i += 1
	}
	return i
}

CALL_TRANSACTION_OPERAND_STOP_KEYWORDS :: []string {
	"USING",
	"MODE",
	"UPDATE",
	"MESSAGES",
	"OPTIONS",
	"WITH",
	"WITHOUT",
	"AND",
}

parse_call_transaction_operands :: proc(p: ^Parser, stmt: ^ast.Call_Stmt) {
	stmt.transaction_operands = make([dynamic]^ast.Expr, 0, 4, p.allocator)
	for !raw_period_done(p) {
		start := p.index
		if allow_keyword(p, "USING") ||
		   allow_keyword(p, "MODE") ||
		   allow_keyword(p, "UPDATE") {
			append_call_transaction_operand(p, stmt)
			ensure_forward_progress(p, start)
			continue
		}
		if allow_keyword(p, "MESSAGES") {
			if allow_keyword(p, "INTO") {
				append_call_transaction_operand(p, stmt)
			}
			ensure_forward_progress(p, start)
			continue
		}
		if allow_keyword(p, "OPTIONS") {
			if allow_keyword(p, "FROM") {
				append_call_transaction_operand(p, stmt)
			}
			ensure_forward_progress(p, start)
			continue
		}
		bump_token(p)
	}
}

append_call_transaction_operand :: proc(p: ^Parser, stmt: ^ast.Call_Stmt) {
	value := parse_raw_operand_to_period(p, CALL_TRANSACTION_OPERAND_STOP_KEYWORDS, false, true)
	if value != nil {
		append(&stmt.transaction_operands, value)
	}
}

parse_call_stmt_raw_arguments :: proc(p: ^Parser, stmt: ^ast.Call_Stmt) {
	parse_raw_call_arguments(p, &stmt.arg_sections, &stmt.named_args)
}

parse_raw_call_arguments :: proc(
	p: ^Parser,
	arg_sections: ^[dynamic]ast.Call_Stmt_Arg_Section,
	named_args: ^[dynamic]ast.Call_Stmt_Named_Arg,
) {
	arg_sections^ = make([dynamic]ast.Call_Stmt_Arg_Section, 0, 2, p.allocator)
	named_args^ = make([dynamic]ast.Call_Stmt_Named_Arg, 0, 4, p.allocator)
	section := ast.Call_Arg_Section_Kind.Exporting
	has_section := false
	for !raw_period_done(p) {
		if call_argument_section_starts(p) {
			kind := call_argument_section_kind(p, current_token(p))
			tok := bump_token(p)
			section = kind
			has_section = true
			append(arg_sections, ast.Call_Stmt_Arg_Section{kind = kind, range = tok.range})
			continue
		}
		if !call_stmt_named_arg_starts(p, p.index) {
			bump_token(p)
			continue
		}
		name := bump_token(p)
		eq := expect_token(p, .Eq)
		value_start := p.index
		value_end := call_stmt_arg_value_end(p, value_start)
		value_range := tokenizer.text_range(eq.range.end, eq.range.end)
		raw_decls := make([dynamic]ast.Raw_Operand_Inline_Decl, 0, 1, p.allocator)
		raw_refs := make([dynamic]ast.Raw_Operand_Ref, 0, 2, p.allocator)
		if value_start < value_end {
			value_range = tokenizer.text_range(
				p.tokens[value_start].range.start,
				p.tokens[value_end - 1].range.end,
			)
			populate_raw_operand_fact_lists(p, value_start, value_end, &raw_decls, &raw_refs)
		}
		value := parse_complete_logical_expr(p, value_start, value_end)
		append(
			named_args,
			ast.Call_Stmt_Named_Arg {
				section = section,
				has_section = has_section,
				name = tokenizer.token_lexeme(name, p.source),
				name_range = name.range,
				value_range = value_range,
				value = value,
				raw_decls = raw_decls,
				raw_refs = raw_refs,
			},
		)
		for p.index < value_end {
			bump_token(p)
		}
	}
}

call_stmt_named_arg_starts :: proc(p: ^Parser, index: int) -> bool {
	if index + 1 >= len(p.tokens) {
		return false
	}
	tok := p.tokens[index]
	return (tok.kind == .Ident || tok.kind == .Number) && p.tokens[index + 1].kind == .Eq
}

call_stmt_arg_value_end :: proc(p: ^Parser, start: int) -> int {
	i := start
	paren, bracket, brace := 0, 0, 0
	for i < len(p.tokens) {
		tok := p.tokens[i]
		top := paren == 0 && bracket == 0 && brace == 0
		if top {
			if tok.kind == .Period || tok.kind == .Eof {
				break
			}
			if call_argument_section_starts_at(p, i) {
				break
			}
			if i > start && call_stmt_named_arg_starts(p, i) {
				break
			}
		}
		#partial switch tok.kind {
		case .LParen:
			paren += 1
		case .RParen:
			if paren > 0 {
				paren -= 1
			}
		case .LBracket:
			bracket += 1
		case .RBracket:
			if bracket > 0 {
				bracket -= 1
			}
		case .LBrace:
			brace += 1
		case .RBrace:
			if brace > 0 {
				brace -= 1
			}
		}
		i += 1
	}
	return i
}

parse_submit_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "SUBMIT")
	body_start := p.index
	stmt := ast.new(ast.Submit_Stmt, start.range, p.allocator)
	stmt.options = make([dynamic]ast.Submit_Option_Clause, 0, 2, p.allocator)
	stmt.target = simple_expr(
		p,
		body_start,
		[]string {
			"USING",
			"VIA",
			"WITH",
			"LINE",
			"EXPORTING",
			"TO",
			"SPOOL",
			"ARCHIVE",
			"WITHOUT",
			"USER",
			"NUMBER",
			"LANGUAGE",
			"AND",
		},
	)
	if stmt.target == nil {
		error_current(p, "syntax error: expected SUBMIT target")
	} else if submit_target_is_dynamic(stmt.target) {
		stmt.target_kind = .Dynamic
	} else if !submit_static_target_is_valid(stmt.target) {
		error(p, stmt.target.range, "syntax error: expected report name or parenthesized program name after SUBMIT")
	}
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, "AND") {
			if allow_keyword(p, "RETURN") {
				stmt.and_return = true
			}
			continue
		}
		if allow_keyword(p, "VIA") {
			if allow_hyphen2(
				p,
				"SELECTION",
				"SCREEN",
			) {
				stmt.via_selection_screen = true
			} else if allow_keyword(p, "JOB") {
				value := required_simple_expr(
					p,
					body_start,
					[]string{"NUMBER", "LANGUAGE", "AND", "WITH", "USER"},
				)
				append(&stmt.options, ast.Submit_Option_Clause{kind = .Via_Job, value = value})
			}
			continue
		}
		if allow_keyword(p, "EXPORTING") {
			if allow_keyword(p, "LIST") && allow_keyword(p, "TO") && allow_keyword(p, "MEMORY") {
				stmt.exporting_list_to_memory = true
			}
			continue
		}
		if allow_keyword(p, "TO") {
			if allow_keyword(p, "SAP") {
				allow_token(p, .Minus)
				stmt.to_sap_spool = allow_keyword(p, "SPOOL")
			}
			continue
		}
		if allow_keyword(p, "WITHOUT") {
			if allow_keyword(p, "SPOOL") && allow_keyword(p, "DYNPRO") {
				stmt.without_spool_dynpro = true
			}
			continue
		}
		if allow_keyword(p, "USING") {
			if allow_hyphen2(p, "SELECTION", "SCREEN") {
				value := required_simple_expr(p, body_start, []string{})
				append(
					&stmt.options,
					ast.Submit_Option_Clause{kind = .Using_Selection_Screen, value = value},
				)
			} else if allow_hyphen2(p, "SELECTION", "SET") {
				value := required_simple_expr(p, body_start, []string{})
				append(
					&stmt.options,
					ast.Submit_Option_Clause{kind = .Using_Selection_Set, value = value},
				)
			}
			continue
		}
		if allow_keyword(p, "WITH") {
			if allow_hyphen2(p, "SELECTION", "TABLE") {
				value := required_simple_expr(p, body_start, []string{})
				append(
					&stmt.options,
					ast.Submit_Option_Clause{kind = .With_Selection_Table, value = value},
				)
				continue
			}
			if allow_keyword(p, "FREE") && allow_keyword(p, "SELECTIONS") {
				value := required_simple_expr(p, body_start, []string{})
				append(
					&stmt.options,
					ast.Submit_Option_Clause{kind = .With_Free_Selections, value = value},
				)
				continue
			}
			name := current_token(p)
			if name.kind == .Ident {
				bump_token(p)
				option := ast.Submit_Option_Clause {
					kind = .With_Parameter,
					name = tokenizer.token_lexeme(name, p.source),
				}
				if current_token(p).kind == .Eq || current_token(p).kind == .Ident {
					op := bump_token(p)
					option.operator = submit_option_operator(p, op)
					option.value = simple_expr(
						p,
						body_start,
						[]string{"WITH", "AND", "VIA", "USER", "LANGUAGE"},
					)
				}
				append(&stmt.options, option)
			}
			continue
		}
		if allow_hyphen2(p, "LINE", "SIZE") || allow_keyword_phrase(p, "LINE-SIZE") {
			value := required_simple_expr(p, body_start, []string{})
			append(&stmt.options, ast.Submit_Option_Clause{kind = .Line_Size, value = value})
			continue
		}
		if allow_hyphen2(p, "LINE", "COUNT") || allow_keyword_phrase(p, "LINE-COUNT") {
			value := required_simple_expr(p, body_start, []string{})
			append(&stmt.options, ast.Submit_Option_Clause{kind = .Line_Count, value = value})
			continue
		}
		if allow_keyword(p, "USER") {
			value := required_simple_expr(p, body_start, []string{})
			append(&stmt.options, ast.Submit_Option_Clause{kind = .User, value = value})
			continue
		}
		if allow_keyword(p, "NUMBER") {
			value := required_simple_expr(p, body_start, []string{})
			append(&stmt.options, ast.Submit_Option_Clause{kind = .Number, value = value})
			continue
		}
		if allow_keyword(p, "LANGUAGE") {
			value := required_simple_expr(p, body_start, []string{})
			append(&stmt.options, ast.Submit_Option_Clause{kind = .Language, value = value})
			continue
		}
		bump_token(p)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

submit_option_operator :: proc(p: ^Parser, tok: Token) -> ast.Submit_Option_Operator {
	if tok.kind == .Eq {
		return .Assign
	}
	if token_is_keyword(p, tok, "EQ") {
		return .Eq
	}
	if token_is_keyword(p, tok, "NE") {
		return .Ne
	}
	if token_is_keyword(p, tok, "BT") {
		return .Bt
	}
	if token_is_keyword(p, tok, "NB") {
		return .Nb
	}
	if token_is_keyword(p, tok, "CP") {
		return .Cp
	}
	if token_is_keyword(p, tok, "NP") {
		return .Np
	}
	if token_is_keyword(p, tok, "GE") {
		return .Ge
	}
	if token_is_keyword(p, tok, "GT") {
		return .Gt
	}
	if token_is_keyword(p, tok, "LE") {
		return .Le
	}
	if token_is_keyword(p, tok, "LT") {
		return .Lt
	}
	return .Other
}

parse_message_head :: proc(p: ^Parser, body_start: int) -> ^ast.Message_Head_Clause {
	head, _ := mem.new(ast.Message_Head_Clause, p.allocator)
	if allow_keyword(p, "ID") {
		head.id = required_simple_expr(
			p,
			body_start,
			[]string{"TYPE", "NUMBER", "WITH", "INTO", "DISPLAY", "RAISING"},
		)
		if allow_keyword(p, "TYPE") {
			head.msg_type = required_simple_expr(
				p,
				body_start,
				[]string{"NUMBER", "WITH", "INTO", "DISPLAY", "RAISING"},
			)
		}
		if allow_keyword(p, "NUMBER") {
			head.number = required_simple_expr(
				p,
				body_start,
				[]string{"WITH", "INTO", "DISPLAY", "RAISING"},
			)
		}
		return head
	}
	head.code = parse_raw_operand_to_period(
		p,
		[]string{"TYPE", "WITH", "INTO", "DISPLAY", "RAISING"},
	)
	message_head_compact_class(p, head, body_start, p.index)
	if allow_keyword(p, "TYPE") {
		head.msg_type = required_simple_expr(
			p,
			body_start,
			[]string{"WITH", "INTO", "DISPLAY", "RAISING"},
		)
	}
	return head
}

message_head_compact_class :: proc(p: ^Parser, head: ^ast.Message_Head_Clause, start, end: int) {
	for i := start; i + 2 < end; i += 1 {
		name := p.tokens[i + 1]
		if p.tokens[i].kind == .LParen &&
		   name.kind == .Ident &&
		   p.tokens[i + 2].kind == .RParen {
			head.compact_class_name = tokenizer.token_lexeme(name, p.source)
			head.compact_class_range = name.range
			head.has_compact_class = true
			return
		}
	}
}

parse_message_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "MESSAGE")
	body_start := p.index
	stmt := ast.new(ast.Message_Stmt, start.range, p.allocator)
	stmt.with_args = make([dynamic]^ast.Expr, 0, 4, p.allocator)
	stmt.head = parse_message_head(p, body_start)
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, "WITH") {
			values := parse_exprs_until(p, body_start, []string{"INTO", "DISPLAY", "RAISING"})
			for value in values {append(&stmt.with_args, value)}
			continue
		}
		if allow_keyword(p, "INTO") {
			stmt.into = required_simple_expr(p, body_start, []string{"DISPLAY", "RAISING"})
			continue
		}
		if allow_keyword(p, "DISPLAY") {
			allow_keyword(p, "LIKE")
			stmt.display_like = required_simple_expr(p, body_start, []string{"RAISING"})
			continue
		}
		if allow_keyword(p, "RAISING") {
			stmt.raising = required_simple_expr(p, body_start, []string{})
			continue
		}
		bump_token(p)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_write_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "WRITE")
	body_start := p.index
	operands := make([dynamic]ast.Write_Operand_Clause, 0, 2, p.allocator)
	entries := make([dynamic]ast.Write_To_Entry_Clause, 0, 2, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {
			continue
		}
		clause := ast.Write_Operand_Clause{}
		if allow_token(p, .Slash) {
			clause.line_break = true
			if current_token(p).kind == .Number {
				clause.position = plain_current_expr(p)
			}
			if allow_token(p, .LParen) {
				clause.length = simple_expr(p, body_start, []string{})
				expect_token(p, .RParen)
			}
		}
		if allow_keyword(p, "AT") {
			clause.position = simple_expr(p, body_start, []string{})
			if allow_token(p, .LParen) {
				clause.length = simple_expr(p, body_start, []string{})
				expect_token(p, .RParen)
			}
		}
		clause.value = simple_expr(p, body_start, []string{})
		if allow_keyword(p, "TO") {
			append(&entries, ast.Write_To_Entry_Clause {
				source = clause.value,
				target = required_simple_expr(p, body_start, []string{}),
			})
			continue
		}
		if clause.value != nil ||
		   clause.line_break ||
		   clause.position != nil ||
		   clause.length != nil {
			append(&operands, clause)
		} else {
			bump_token(p)
		}
	}
	stmt_range := simple_stmt_range(p, start)
	if len(entries) > 0 {
		stmt := ast.new(ast.Write_To_Stmt, stmt_range, p.allocator)
		stmt.entries = entries
		return stmt
	}
	stmt := ast.new(ast.Write_Stmt, stmt_range, p.allocator)
	stmt.operands = operands
	return stmt
}

parse_assign_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	lhs := parse_expr(p)
	if lhs == nil {
		return nil
	}

	op := allow_token(p, .Eq)
	downcast := false
	if !op {
		downcast = allow_token(p, .QuestionEq)
	}
	if !op && !downcast {
		error_current(p, "syntax error: expected assignment operator")
		return nil
	}

	if !expr_lead_token(current_token(p)) {
		error_current(p, "syntax error: expected assignment value after '='")
		return nil
	}
	rhs := parse_expr(p)
	if rhs == nil {
		error_current(p, "syntax error: expected assignment value after '='")
		return nil
	}
	if closing_delimiter_error(p) {
		return nil
	}
	period := expect_token_message(
		p,
		.Period,
		"syntax error: expected '.' to end assignment statement",
	)
	if period.kind != .Period {
		return nil
	}

	stmt_range := tokenizer.text_range(lhs.range.start, statement_end(p, period))
	if downcast {
		stmt := ast.new(ast.Downcast_Assign_Stmt, stmt_range, p.allocator)
		stmt.lhs = lhs
		stmt.rhs = rhs
		return stmt
	}

	stmt := ast.new(ast.Assign_Stmt, stmt_range, p.allocator)
	stmt.lhs = lhs
	stmt.rhs = rhs
	return stmt
}

closing_delimiter_error :: proc(p: ^Parser) -> bool {
	tok := current_token(p)
	if tok.kind == .RParen {
		error(p, tok.range, "syntax error: unmatched closing ')'")
		return true
	}
	if tok.kind == .RBracket {
		error(p, tok.range, "syntax error: unmatched closing ']'")
		return true
	}
	if tok.kind == .RBrace {
		error(p, tok.range, "syntax error: unmatched closing '}'")
		return true
	}
	return false
}
