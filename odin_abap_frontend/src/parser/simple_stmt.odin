package abap_frontend_parser

import "../ast"
import "../tokenizer"

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
		return parse_create_object_stmt(p)
	}
	if text_transform_stmt_starts(p) {
		return parse_text_transform_stmt(p)
	}
	if list_control_stmt_starts(p) {
		return parse_list_control_stmt(p)
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
		(at_keyword(p, "CREATE") && at_keyword_index(p, p.index + 1, "OBJECT")) ||
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
			value := ast.new(ast.Type_Ref_Expr, tokenizer.text_range(first.range.start, last.range.end), p.allocator)
			value.text = source_range_text(p, value.range)
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
) -> ^ast.Expr {
	if raw_period_done(p) ||
	   current_token(p).kind == .Comma ||
	   current_token(p).kind == .Colon ||
	   simple_current_keyword_in(p, stop_keywords) {
		return nil
	}
	first := current_token(p)
	last := first
	paren := 0
	bracket := 0
	brace := 0
	for !raw_period_done(p) {
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
	if first.kind == .Eof || last.kind == .Eof || first.range.start >= last.range.end {
		return nil
	}
	value := ast.new(ast.Type_Ref_Expr, tokenizer.text_range(first.range.start, last.range.end), p.allocator)
	value.text = source_range_text(p, value.range)
	return value
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
		value := parse_raw_operand_to_period(p, stop_keywords)
		if value != nil {
			append(&values, value)
		}
		ensure_forward_progress(p, start)
	}
	return values
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

parse_runtime_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := current_token(p)
	kind := ast.Runtime_Kind.Get
	subject := ast.Runtime_Subject.None
	if at_keyword(p, "SET") {
		expect_keyword(p, "SET")
		if allow_keyword(p, "HANDLER") {
			kind = .Set_Handler
			subject = .Handler
		} else {
			kind = .Set
		}
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
	stmt.target = parse_raw_operand_to_period(p, []string{"EXPORTING", "TYPE", "MESSAGE", "RESUMABLE"})
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

parse_assign_field_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "ASSIGN")
	stmt := ast.new(ast.Assign_Field_Stmt, start.range, p.allocator)
	stmt.operands = parse_generic_operands_to_period(p, []string{})
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_create_object_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "CREATE")
	allow_keyword(p, "OBJECT")
	stmt := ast.new(ast.Create_Object_Stmt, start.range, p.allocator)
	stmt.operands = parse_generic_operands_to_period(p, []string{})
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

text_transform_stmt_starts :: proc(p: ^Parser) -> bool {
	return(
		at_keyword(p, "OVERLAY") ||
		at_keyword(p, "PACK") ||
		at_keyword(p, "UNPACK") ||
		at_keyword(p, "CONVERT") ||
		at_keyword(p, "WAIT") \
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
	} else {
		stmt.kind = .Wait
	}
	stmt.operands = parse_generic_simple_operands(p, body_start, []string{})
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
		bump_token(p)
		member := ast.Oop_Member_Clause {
			name       = tokenizer.token_lexeme(name, p.source),
			signatures = make([dynamic]ast.Oop_Signature_Clause, 0, 2, p.allocator),
		}
		for current_token(p).kind != .Period && current_token(p).kind != .Eof && current_token(p).kind != .Comma {
			if kind, ok := oop_signature_kind(p); ok {
				bump_token(p)
				append(&member.signatures, parse_oop_signature_clause(p, kind))
				continue
			}
			bump_token(p)
		}
		append(&stmt.members, member)
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
	name, name_range, ok := parse_oop_parameter_name(p)
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
	append(&clause.parameters, ast.Oop_Parameter_Clause{name, name_range, type_clause, optional})
	append_oop_signature_value(p, clause, start, p.index)
	return true
}

parse_oop_parameter_name :: proc(p: ^Parser) -> (string, tokenizer.Range, bool) {
	if tokenizer.token_lexeme(current_token(p), p.source) == "!" {
		bump_token(p)
	}
	if at_keyword(p, "VALUE") || at_keyword(p, "REFERENCE") {
		bump_token(p)
		allow_token(p, .LParen)
		tok := current_token(p)
		if tok.kind != .Ident {
			return "", tok.range, false
		}
		bump_token(p)
		allow_token(p, .RParen)
		return tokenizer.token_lexeme(tok, p.source), tok.range, true
	}
	tok := current_token(p)
	if tok.kind != .Ident {
		return "", tok.range, false
	}
	bump_token(p)
	return tokenizer.token_lexeme(tok, p.source), tok.range, true
}

parse_oop_parameter_type_clause :: proc(p: ^Parser) -> ^ast.Data_Type_Clause {
	keyword := bump_token(p)
	clause, _ := mem.new(ast.Data_Type_Clause, p.allocator)
	is_like := token_is_keyword(p, keyword, "LIKE")
	clause.form = .Like if is_like else .Type
	if allow_keyword(p, "LINE") {
		allow_keyword(p, "OF")
		clause.form = .Like_Line_Of if is_like else .Type_Line_Of
	} else if !is_like && allow_keyword(p, "REF") {
		allow_keyword(p, "TO")
		clause.form = .Ref_To
	} else if !is_like && allow_keyword(p, "RANGE") {
		allow_keyword(p, "OF")
		clause.form = .Range_Of
	} else if allow_keyword(p, "STANDARD") {
		allow_keyword(p, "TABLE")
		allow_keyword(p, "OF")
		clause.form = .Like_Standard_Table if is_like else .Standard_Table
	} else if allow_keyword(p, "SORTED") {
		allow_keyword(p, "TABLE")
		allow_keyword(p, "OF")
		clause.form = .Like_Sorted_Table if is_like else .Sorted_Table
	} else if allow_keyword(p, "HASHED") {
		allow_keyword(p, "TABLE")
		allow_keyword(p, "OF")
		clause.form = .Like_Hashed_Table if is_like else .Hashed_Table
	} else if allow_keyword(p, "TABLE") {
		allow_keyword(p, "OF")
		clause.form = .Like_Table if is_like else .Table
	}
	clause.type_ref = parse_oop_type_ref_expr(p)
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
	first := p.tokens[start]
	last := p.tokens[p.index - 1]
	expr := ast.new(ast.Type_Ref_Expr, tokenizer.text_range(first.range.start, last.range.end), p.allocator)
	expr.text = source_range_text(p, expr.range)
	if name_end < 0 {
		name_end = last.range.end
	}
	expr.name = strings.clone(p.source[first.range.start:name_end], p.allocator)
	return expr
}

oop_type_ref_done :: proc(p: ^Parser, start: int, in_key: bool) -> bool {
	tok := current_token(p)
	if tok.kind == .Period || tok.kind == .Comma || tok.kind == .Eof {
		return true
	}
	if simple_current_keyword_in(p, OOP_SIGNATURE_STOP_KEYWORDS) ||
	   at_length_keyword(p) ||
	   at_keyword_phrase(p, "READ-ONLY") ||
	   at_keyword(p, "OPTIONAL") ||
	   at_keyword(p, "PREFERRED") ||
	   (!in_key && at_keyword(p, "DEFAULT")) {
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
	value := ast.new(ast.Type_Ref_Expr, tokenizer.text_range(first.range.start, last.range.end), p.allocator)
	value.text = source_range_text(p, value.range)
	append(&clause.values, value)
}

oop_parameter_starts :: proc(p: ^Parser, index: int) -> bool {
	i := index
	if i < len(p.tokens) && tokenizer.token_lexeme(p.tokens[i], p.source) == "!" {
		i += 1
	}
	if at_keyword_index(p, i, "VALUE") || at_keyword_index(p, i, "REFERENCE") {
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
		value := parse_raw_operand_to_period(p, OOP_SIGNATURE_STOP_KEYWORDS)
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
	start := expect_keyword(p, "MOVE")
	body_start := p.index
	stmt := ast.new(ast.Move_Stmt, start.range, p.allocator)
	stmt.entries = make([dynamic]ast.Move_Entry_Clause, 0, 2, p.allocator)
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
		append(&stmt.entries, entry)
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
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
) {
	entry := ast.Concatenate_Entry_Clause{}
	entry.sources = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	if allow_keyword(p, "LINES") {
		if !allow_keyword(
			p,
			"OF",
		) {
			error_current(p, "syntax error: expected keyword")
			return entry, false
		}
		entry.lines_of = true
		source := required_simple_expr(p, body_start, []string{"INTO"})
		if source == nil {
			return entry, false
		}
		append(&entry.sources, source)
	} else {
		entry.sources = parse_exprs_until(p, body_start, []string{"INTO"})
	}
	if !allow_keyword(p, "INTO") {
		error_current(p, "syntax error: expected keyword")
		return entry, false
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
		bump_token(p)
	}
	return entry, entry.target != nil && len(entry.sources) > 0
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
		entry, ok := parse_concatenate_entry(p, body_start)
		if ok {
			append(&stmt.entries, entry)
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
	stmt.pattern = required_simple_expr(p, body_start, []string{"IN", "WITH"})
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
		stmt.target = required_simple_expr(
			p,
			body_start,
			[]string{"MATCH", "SUBMATCHES", "RESULTS", "IGNORING", "RESPECTING"},
		)
	}
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, "MATCH") {
			if allow_keyword(p, "OFFSET") {
				stmt.match_offset = required_simple_expr(
					p,
					body_start,
					[]string{"MATCH", "SUBMATCHES", "RESULTS", "IGNORING", "RESPECTING"},
				)
			} else if allow_keyword(p, "LENGTH") {
				stmt.match_length = required_simple_expr(
					p,
					body_start,
					[]string{"MATCH", "SUBMATCHES", "RESULTS", "IGNORING", "RESPECTING"},
				)
			} else {
				stmt.match_offset = required_simple_expr(
					p,
					body_start,
					[]string{"MATCH", "SUBMATCHES", "RESULTS", "IGNORING", "RESPECTING"},
				)
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
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, "IN") {
			if !allow_keyword(
				p,
				"PROGRAM",
			) {
				error_current(p, "syntax error: expected keyword")
				break
			}
			stmt.program = required_simple_expr(
				p,
				body_start,
				[]string{"TABLES", "USING", "CHANGING", "IF"},
			)
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
			stmt.if_found = allow_keyword(p, "FOUND")
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
	stmt.target = parse_raw_operand_to_period(
		p,
		[]string {
			"EXPORTING",
			"IMPORTING",
			"CHANGING",
			"TABLES",
			"EXCEPTIONS",
			"USING",
			"AND",
			"WITH",
		},
	)
	consume_raw_until_top_level_period(p)
	stmt.range = simple_stmt_range(p, start)
	return stmt
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
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, "AND") {
			stmt.and_return = allow_keyword(p, "RETURN")
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
	if allow_keyword(p, "TYPE") {
		head.msg_type = required_simple_expr(
			p,
			body_start,
			[]string{"WITH", "INTO", "DISPLAY", "RAISING"},
		)
	}
	return head
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
	stmt := ast.new(ast.Write_Stmt, start.range, p.allocator)
	stmt.operands = make([dynamic]ast.Write_Operand_Clause, 0, 2, p.allocator)
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
		if clause.value != nil ||
		   clause.line_break ||
		   clause.position != nil ||
		   clause.length != nil {
			append(&stmt.operands, clause)
		} else {
			bump_token(p)
		}
	}
	stmt.range = simple_stmt_range(p, start)
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
