package abap_frontend_parser

import "src:ast"
import "src:tokenizer"

import "core:mem"
import "core:strings"

program_include_stmt_starts :: proc(p: ^Parser) -> bool {
	return(
		at_keyword(p, "INCLUDE") &&
		!at_keyword_index(p, p.index + 1, "TYPE") &&
		!at_keyword_index(p, p.index + 1, "STRUCTURE") \
	)
}

data_access_stmt_starts :: proc(p: ^Parser) -> bool {
	return(
		at_keyword_phrase(p, "EXEC SQL") ||
		at_keyword(p, "SELECT") ||
		at_keyword(p, "WITH") ||
		at_keyword_phrase(p, "OPEN CURSOR") ||
		at_keyword(p, "FETCH") ||
		at_keyword_phrase(p, "CLOSE CURSOR") ||
		read_line_stmt_starts(p) ||
		at_keyword(p, "REPORT") ||
		at_keyword(p, "PROGRAM") ||
		at_keyword(p, "GENERATE") ||
		at_keyword(p, "INSERT") ||
		at_keyword(p, "APPEND") ||
		at_keyword(p, "MODIFY") ||
		at_keyword(p, "SORT") ||
		at_keyword(p, "UPDATE") ||
		at_keyword(p, "DELETE") ||
		at_keyword_phrase(p, "READ TABLE") ||
		dataset_stmt_starts(p) ||
		report_textpool_stmt_starts(p) \
	)
}

parse_data_access_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	if at_keyword_phrase(p, "EXEC SQL") {
		return parse_exec_sql_stmt(p)
	}
	if at_keyword(p, "SELECT") || at_keyword(p, "WITH") {
		return parse_select_stmt(p)
	}
	if at_keyword_phrase(p, "OPEN CURSOR") {
		return parse_open_cursor_stmt(p)
	}
	if at_keyword(p, "FETCH") {
		return parse_fetch_stmt(p)
	}
	if at_keyword_phrase(p, "CLOSE CURSOR") {
		return parse_close_cursor_stmt(p)
	}
	if at_keyword_phrase(p, "READ TABLE") {
		return parse_read_table_stmt(p)
	}
	if read_line_stmt_starts(p) {
		return parse_line_stmt(p)
	}
	if dataset_stmt_starts(p) {
		return parse_dataset_stmt(p)
	}
	if at_keyword(p, "REPORT") || at_keyword(p, "PROGRAM") {
		return parse_report_stmt(p)
	}
	if report_textpool_stmt_starts(p) {
		if at_keyword_index(p, p.index + 1, "TEXTPOOL") {
			return parse_textpool_stmt(p)
		}
		return parse_report_stmt(p)
	}
	if at_keyword(p, "INSERT") {
		return parse_insert_stmt(p)
	}
	if at_keyword(p, "APPEND") {
		return parse_append_stmt(p)
	}
	if at_keyword(p, "GENERATE") {
		return parse_generate_stmt(p)
	}
	if modify_line_stmt_starts(p) {
		return parse_line_stmt(p)
	}
	if at_keyword(p, "MODIFY") {
		return parse_modify_stmt(p)
	}
	if at_keyword(p, "SORT") {
		return parse_sort_stmt(p)
	}
	if at_keyword(p, "UPDATE") {
		return parse_update_stmt(p)
	}
	return parse_delete_stmt(p)
}

parse_include_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "INCLUDE")
	chained := allow_token(p, .Colon)
	stmt := ast.new(ast.Include_Stmt, start.range, p.allocator)
	stmt.names = make([dynamic]ast.Include_Name, 0, 2, p.allocator)

	for {
		name := current_token(p)
		if name.kind != .Ident {
			error_current(p, "syntax error: expected include name")
			return nil
		}
		bump_token(p)
		append(&stmt.names, ast.Include_Name{name = parser_ast_raw_name_token(p, name)})
		if !chained || !allow_token(p, .Comma) {
			break
		}
	}

	if allow_keyword(p, "IF") {
		_ = expect_keyword_message(p, "FOUND", "syntax error: expected FOUND after INCLUDE IF")
		stmt.if_found = true
	}

	period := expect_token_message(p, .Period, "syntax error: expected '.' after INCLUDE")
	if period.kind != .Period {
		return nil
	}
	stmt.range = tokenizer.text_range(start.range.start, statement_end(p, period))
	return stmt
}

data_stmt_done :: proc(p: ^Parser, body_start: int) -> bool {
	tok := current_token(p)
	return(
		tok.kind == .Period ||
		tok.kind == .Eof ||
		(p.index > body_start &&
				.Has_Newline_Before in tok.flags &&
				known_stmt_lead_at(p, p.index) &&
				!line_continuation_starts(p, p.index)) \
	)
}

data_stmt_range :: proc(p: ^Parser, start: Token) -> tokenizer.Range {
	period := expect_token_message(p, .Period, "syntax error: expected '.' to end statement")
	return tokenizer.text_range(start.range.start, statement_end(p, period))
}

data_current_keyword_in :: proc(p: ^Parser, keywords: []string) -> bool {
	for keyword in keywords {
		if at_keyword_phrase(p, keyword) {
			return true
		}
	}
	return false
}

data_expr :: proc(p: ^Parser, body_start: int, stop_keywords: []string) -> ^ast.Expr {
	if data_stmt_done(p, body_start) ||
	   current_token(p).kind == .Comma ||
	   data_current_keyword_in(p, stop_keywords) {
		return nil
	}
	host := current_token(p)
	has_host := false
	if host.kind == .At {
		bump_token(p)
		has_host = true
	}
	if !expr_lead_token(current_token(p)) {
		return nil
	}
	value := parse_expr(p)
	if has_host && value != nil {
		expr := ast.new(
			ast.Host_Expr,
			tokenizer.text_range(host.range.start, value.range.end),
			p.allocator,
		)
		expr.value = value
		return expr
	}
	return value
}

parse_table_key_selector :: proc(
	p: ^Parser,
	body_start: int,
	stop_keywords: []string,
) -> ast.Table_Key_Selector {
	selector := ast.Table_Key_Selector{}
	if !allow_keyword(p, "KEY") {
		error_current(p, "syntax error: expected KEY after USING")
		return selector
	}
	if data_stmt_done(p, body_start) ||
	   current_token(p).kind == .Comma ||
	   data_current_keyword_in(p, stop_keywords) {
		error_current(p, "syntax error: expected table key name")
		return selector
	}
	if current_token(p).kind == .Ident {
		key := bump_token(p)
		selector.name = parser_ast_raw_name_token(p, key)
		return selector
	}
	if current_token(p).kind == .LParen {
		open := p.index
		close := matching_group_index(p, open, .LParen, .RParen)
		if close > open {
			selector.dynamic_name = parse_required_complete_expr_with(
				p,
				open + 1,
				close,
				parse_logical_expr,
				"syntax error: expected dynamic table key expression",
			)
			for p.index <= close {
				bump_token(p)
			}
			return selector
		}
	}
	error_current(p, "syntax error: expected table key name")
	return selector
}

sql_data_expr :: proc(p: ^Parser, body_start: int, stop_keywords: []string) -> ^ast.Expr {
	old_stops := p.expr_stop_keywords
	old_open_sql := p.open_sql_expr
	p.expr_stop_keywords = stop_keywords
	p.open_sql_expr = true
	defer p.expr_stop_keywords = old_stops
	defer p.open_sql_expr = old_open_sql
	return data_expr(p, body_start, stop_keywords)
}

sql_logical_expr :: proc(p: ^Parser, body_start: int, stop_keywords: []string) -> ^ast.Expr {
	if data_stmt_done(p, body_start) || data_current_keyword_in(p, stop_keywords) {
		error_current(p, "syntax error: expected expression")
		return nil
	}
	old_stops := p.expr_stop_keywords
	old_open_sql := p.open_sql_expr
	p.expr_stop_keywords = stop_keywords
	p.open_sql_expr = true
	defer p.expr_stop_keywords = old_stops
	defer p.open_sql_expr = old_open_sql
	expr := parse_logical_expr(p)
	if expr == nil &&
	   (data_stmt_done(p, body_start) || data_current_keyword_in(p, stop_keywords)) {
		error_current(p, "syntax error: expected expression")
	}
	if expr != nil {
		sql_mark_implicit_hosts(p, expr)
	}
	return expr
}

sql_mark_implicit_hosts :: proc(p: ^Parser, expr: ^ast.Expr) {
	if expr == nil {
		return
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Binary_Expr:
		if n.op == .And || n.op == .Or {
			sql_mark_implicit_hosts(p, n.left)
			sql_mark_implicit_hosts(p, n.right)
		} else if sql_host_value_binary_op(n.op) {
			n.right = sql_implicit_host_expr(p, n.right)
		}
	case ^ast.Unary_Expr:
		if n.op == .Not {
			sql_mark_implicit_hosts(p, n.expr)
		}
	case ^ast.Paren_Expr:
		if !sql_dynamic_where_operand(n.expr) {
			sql_mark_implicit_hosts(p, n.expr)
		}
	case ^ast.Between_Expr:
		n.low = sql_implicit_host_expr(p, n.low)
		n.high = sql_implicit_host_expr(p, n.high)
	case ^ast.Sql_Case_When_Expr:
		sql_mark_implicit_hosts(p, n.condition)
	case ^ast.Sql_Case_Expr:
		for when_expr in n.whens {
			sql_mark_implicit_hosts(p, when_expr)
		}
	}
}

sql_host_value_binary_op :: proc(op: ast.Binary_Op) -> bool {
	return(
		op == .Equal ||
		op == .Not_Equal ||
		op == .Less ||
		op == .Less_Equal ||
		op == .Greater ||
		op == .Greater_Equal ||
		op == .In ||
		op == .Not_In ||
		op == .Like ||
		op == .Not_Like \
	)
}

sql_implicit_host_expr :: proc(p: ^Parser, expr: ^ast.Expr) -> ^ast.Expr {
	if expr == nil {
		return nil
	}
	if _, ok := expr.derived_expr.(^ast.Host_Expr); ok {
		return expr
	}
	if !sql_implicit_host_operand(expr) {
		sql_mark_implicit_hosts(p, expr)
		return expr
	}
	host := ast.new(ast.Host_Expr, expr.range, p.allocator)
	host.value = expr
	host.implicit = true
	return host
}

sql_implicit_host_operand :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		return n.name != ""
	case ^ast.Selector_Expr:
		return n.op != .Tilde && sql_implicit_host_operand(n.base)
	case ^ast.Table_Expr:
		return sql_implicit_host_operand(n.table)
	case ^ast.Substring_Expr:
		return sql_implicit_host_operand(n.base)
	case ^ast.Unary_Expr:
		return sql_implicit_host_operand(n.expr)
	case ^ast.Paren_Expr:
		return sql_implicit_host_operand(n.expr)
	}
	return false
}

select_sql_projection_expr :: proc(
	p: ^Parser,
	body_start: int,
	stop_keywords: []string,
) -> (
	^ast.Expr,
	bool,
) {
	value := sql_data_expr(p, body_start, stop_keywords)
	is_dynamic := sql_dynamic_operand_expr(value)
	if value != nil && !is_dynamic {
		value = sql_model_select_expr(p, value)
	}
	return value, is_dynamic
}

select_sql_logical_expr :: proc(
	p: ^Parser,
	body_start: int,
	stop_keywords: []string,
) -> (
	^ast.Expr,
	bool,
) {
	expr := sql_logical_expr(p, body_start, stop_keywords)
	is_dynamic := sql_dynamic_where_expr(expr)
	if expr != nil && !is_dynamic {
		expr = sql_model_select_expr(p, expr)
	}
	return expr, is_dynamic
}

sql_model_select_source_expr :: proc(p: ^Parser, expr: ^ast.Expr) -> ^ast.Expr {
	if expr == nil || sql_dynamic_operand_expr(expr) {
		return expr
	}
	if call, ok := expr.derived_expr.(^ast.Call_Expr); ok {
		return sql_model_call_expr(p, expr, call)
	}
	return expr
}

sql_model_select_expr :: proc(p: ^Parser, expr: ^ast.Expr) -> ^ast.Expr {
	if expr == nil {
		return nil
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Host_Expr:
		return expr
	case ^ast.Ident_Expr:
		if n.name != "" {
			return sql_column_expr(p, expr.range, "", tokenizer.Range{}, n.name, n.range)
		}
	case ^ast.Type_Ref_Expr:
		if n.name.text != "" {
			return sql_column_expr(p, expr.range, "", tokenizer.Range{}, n.name.text, n.name.range)
		}
	case ^ast.Literal_Expr:
		if n.value == "*" {
			return sql_star_expr(p, expr.range, "", tokenizer.Range{}, n.range)
		}
	case ^ast.Selector_Expr:
		if n.op == .Tilde {
			qualifier, qualifier_range, qualifier_ok := sql_expr_simple_name(n.base)
			if qualifier_ok {
				if lit, lit_ok := n.field.derived_expr.(^ast.Literal_Expr);
				   lit_ok && lit.value == "*" {
					return sql_star_expr(p, expr.range, qualifier, qualifier_range, lit.range)
				}
				name, name_range, name_ok := sql_expr_simple_name(n.field)
				if name_ok {
					return sql_column_expr(
						p,
						expr.range,
						qualifier,
						qualifier_range,
						name,
						name_range,
					)
				}
			}
		}
		n.base = sql_model_select_expr(p, n.base)
		n.field = sql_model_select_expr(p, n.field)
	case ^ast.Call_Expr:
		return sql_model_call_expr(p, expr, n)
	case ^ast.Call_Named_Arg_Expr:
		n.value = sql_model_select_expr(p, n.value)
	case ^ast.Call_Positional_Arg_Expr:
		n.value = sql_model_select_expr(p, n.value)
	case ^ast.Call_Arg_List_Expr:
		for arg, i in n.args {
			n.args[i] = sql_model_select_expr(p, arg)
		}
	case ^ast.Binary_Expr:
		n.left = sql_model_select_expr(p, n.left)
		n.right = sql_model_select_expr(p, n.right)
	case ^ast.Unary_Expr:
		n.expr = sql_model_select_expr(p, n.expr)
	case ^ast.Paren_Expr:
		n.expr = sql_model_select_expr(p, n.expr)
	case ^ast.Between_Expr:
		n.subject = sql_model_select_expr(p, n.subject)
		n.low = sql_model_select_expr(p, n.low)
		n.high = sql_model_select_expr(p, n.high)
	case ^ast.Is_Predicate_Expr:
		n.subject = sql_model_select_expr(p, n.subject)
	case ^ast.Instance_Of_Predicate_Expr:
		n.subject = sql_model_select_expr(p, n.subject)
	case ^ast.Table_Expr:
		n.table = sql_model_select_expr(p, n.table)
		for selector, i in n.selectors {
			n.selectors[i] = sql_model_select_expr(p, selector)
		}
	case ^ast.Sql_Case_When_Expr:
		n.condition = sql_model_select_expr(p, n.condition)
		n.result = sql_model_select_expr(p, n.result)
	case ^ast.Sql_Case_Expr:
		n.operand = sql_model_select_expr(p, n.operand)
		for when_expr, i in n.whens {
			n.whens[i] = sql_model_select_expr(p, when_expr)
		}
		n.else_expr = sql_model_select_expr(p, n.else_expr)
	}
	return expr
}

sql_model_call_expr :: proc(p: ^Parser, expr: ^ast.Expr, call: ^ast.Call_Expr) -> ^ast.Expr {
	name, name_range, ok := sql_expr_simple_name(call.callee)
	if !ok {
		call.callee = sql_model_select_expr(p, call.callee)
		call.args = sql_model_select_expr(p, call.args)
		return expr
	}
	sql_call := ast.new(ast.Sql_Call_Expr, expr.range, p.allocator)
	sql_call.name = parser_ast_token(parser_intern_name(p, name), name_range)
	sql_call.kind = .Aggregate if sql_aggregate_name(name) else .Function
	sql_call.args = make([dynamic]^ast.Expr, 0, 4, p.allocator)
	if args, args_ok := call.args.derived_expr.(^ast.Call_Arg_List_Expr); args_ok {
		for arg, i in args.args {
			if i == 0 && sql_call.kind == .Aggregate {
				if modifier, modifier_range, modifier_ok := sql_call_modifier_arg(arg);
				   modifier_ok {
					sql_call.modifier = modifier
					sql_call.modifier_range = modifier_range
					continue
				}
			}
			append(&sql_call.args, sql_model_call_arg_expr(p, arg))
		}
	}
	return sql_call
}

sql_model_call_arg_expr :: proc(p: ^Parser, expr: ^ast.Expr) -> ^ast.Expr {
	if pos, ok := expr.derived_expr.(^ast.Call_Positional_Arg_Expr); ok {
		return sql_model_select_expr(p, pos.value)
	}
	return sql_model_select_expr(p, expr)
}

sql_call_modifier_arg :: proc(expr: ^ast.Expr) -> (ast.Sql_Call_Modifier, tokenizer.Range, bool) {
	if pos, ok := expr.derived_expr.(^ast.Call_Positional_Arg_Expr); ok {
		name, range, name_ok := sql_expr_simple_name(pos.value)
		if name_ok && strings.equal_fold(name, "DISTINCT") {
			return .Distinct, range, true
		}
		if name_ok && strings.equal_fold(name, "ALL") {
			return .All, range, true
		}
	}
	return .None, tokenizer.Range{}, false
}

sql_column_expr :: proc(
	p: ^Parser,
	range: tokenizer.Range,
	qualifier: string,
	qualifier_range: tokenizer.Range,
	name: string,
	name_range: tokenizer.Range,
) -> ^ast.Expr {
	expr := ast.new(ast.Sql_Column_Expr, range, p.allocator)
	expr.qualifier = parser_ast_token(parser_intern_name(p, qualifier), qualifier_range)
	expr.name = parser_ast_token(parser_intern_name(p, name), name_range)
	return expr
}

sql_star_expr :: proc(
	p: ^Parser,
	range: tokenizer.Range,
	qualifier: string,
	qualifier_range: tokenizer.Range,
	star_range: tokenizer.Range,
) -> ^ast.Expr {
	expr := ast.new(ast.Sql_Star_Expr, range, p.allocator)
	expr.qualifier = parser_ast_token(parser_intern_name(p, qualifier), qualifier_range)
	expr.star_range = star_range
	return expr
}

sql_expr_simple_name :: proc(expr: ^ast.Expr) -> (string, tokenizer.Range, bool) {
	if expr == nil {
		return "", tokenizer.Range{}, false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		return n.name, n.range, n.name != ""
	case ^ast.Type_Ref_Expr:
		return n.name.text, n.name.range, n.name.text != ""
	}
	return "", tokenizer.Range{}, false
}

sql_aggregate_name :: proc(name: string) -> bool {
	return(
		strings.equal_fold(name, "avg") ||
		strings.equal_fold(name, "count") ||
		strings.equal_fold(name, "max") ||
		strings.equal_fold(name, "min") ||
		strings.equal_fold(name, "sum") ||
		strings.equal_fold(name, "median") ||
		strings.equal_fold(name, "stddev") ||
		strings.equal_fold(name, "var") ||
		strings.equal_fold(name, "corr") ||
		strings.equal_fold(name, "corr_spearman") ||
		strings.equal_fold(name, "grouping") ||
		strings.equal_fold(name, "string_agg") ||
		strings.equal_fold(name, "allow_precision_loss") \
	)
}

sql_dynamic_operand_expr :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	_, ok := expr.derived_expr.(^ast.Paren_Expr)
	return ok
}

required_data_expr :: proc(p: ^Parser, body_start: int, stop_keywords: []string) -> ^ast.Expr {
	expr := data_expr(p, body_start, stop_keywords)
	if expr == nil {
		error_current(p, "syntax error: expected expression")
	}
	return expr
}

data_exprs_until :: proc(
	p: ^Parser,
	body_start: int,
	stop_keywords: []string,
) -> [dynamic]^ast.Expr {
	values := make([dynamic]^ast.Expr, 0, 2, p.allocator)
	for !data_stmt_done(p, body_start) && !data_current_keyword_in(p, stop_keywords) {
		if allow_token(p, .Comma) {
			continue
		}
		if allow_token(p, .Colon) {
			continue
		}
		if current_token(p).kind == .Star {
			bump_token(p)
			continue
		}
		start := p.index
		value := data_expr(p, body_start, stop_keywords)
		if value != nil {
			append(&values, value)
		} else {
			bump_token(p)
		}
		ensure_forward_progress(p, start)
	}
	return values
}

consume_data_tail :: proc(p: ^Parser, body_start: int) {
	for !data_stmt_done(p, body_start) && current_token(p).kind != .Comma {
		bump_token(p)
	}
}

parse_exec_sql_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword_phrase(p, "EXEC SQL")
	header_period := expect_token(p, .Period)
	if header_period.kind != .Period {
		return nil
	}
	for !at_eof(p) && !at_keyword_phrase(p, "ENDEXEC") {
		bump_token(p)
	}
	body_end := current_token(p).range.start
	end := expect_keyword_phrase(p, "ENDEXEC")
	if end.kind == .Eof {
		return nil
	}
	period := expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	stmt := ast.new(
		ast.Exec_Sql_Stmt,
		tokenizer.text_range(start.range.start, period.range.end),
		p.allocator,
	)
	stmt.header_range = tokenizer.text_range(start.range.start, header_period.range.end)
	if header_period.range.end < body_end {
		stmt.body = parser_clone_range_text(p, tokenizer.text_range(header_period.range.end, body_end))
	}
	return stmt
}

parse_select_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := current_token(p)
	body_start := p.index
	stmt := ast.new(ast.Select_Stmt, start.range, p.allocator)
	stmt.body = make([dynamic]^ast.Stmt, 0, 2, p.allocator)
	if at_keyword(p, "WITH") {
		stmt.with = parse_select_with_clause(p, body_start)
	}
	stmt.query = parse_select_query_clause(p, body_start)
	stmt.range = data_stmt_range(p, start)
	requires_endselect := select_query_has_loop_body(stmt.query)
	if requires_endselect && !endselect_ahead(p) {
		error(p, select_missing_endselect_range(stmt), OPEN_SQL_MISSING_ENDSELECT_MESSAGE)
		return stmt
	}
	if (requires_endselect && endselect_ahead(p)) || at_keyword(p, "ENDSELECT") {
		stmt.body = parse_stmt_list_until(p, []string{"ENDSELECT"})
		end := expect_keyword(p, "ENDSELECT")
		if token_is_keyword(p, end, "ENDSELECT") {
			period := expect_token(p, .Period)
			stmt.range.end = statement_end(p, period)
		}
	}
	return stmt
}

select_missing_endselect_range :: proc(stmt: ^ast.Select_Stmt) -> tokenizer.Range {
	if select_range_valid(stmt.query.into_clause) {
		return stmt.query.into_clause
	}
	return stmt.range
}

endselect_ahead :: proc(p: ^Parser) -> bool {
	for i := p.index; i < len(p.tokens); i += 1 {
		if p.tokens[i].kind == .Eof {
			return false
		}
		if keyword_phrase_at(p, i, "ENDSELECT") {
			return true
		}
	}
	return false
}

parse_select_with_clause :: proc(p: ^Parser, body_start: int) -> ^ast.Select_With_Clause {
	start := expect_keyword(p, "WITH")
	clause, _ := mem.new(ast.Select_With_Clause, p.allocator)
	clause.entries = make([dynamic]ast.Select_Cte_Clause, 0, 2, p.allocator)
	for !data_stmt_done(p, body_start) && !at_keyword(p, "SELECT") {
		entry := ast.Select_Cte_Clause {
			name = parse_cte_name(p),
		}
		if !allow_keyword(p, "AS") {
			break
		}
		allow_token(p, .LParen)
		entry.query = parse_select_query_clause(p, body_start, true)
		allow_token(p, .RParen)
		append(&clause.entries, entry)
		if !allow_token(p, .Comma) {
			break
		}
	}
	clause.query_count = len(clause.entries)
	clause.range = tokenizer.text_range(start.range.start, previous_token(p).range.end)
	return clause
}

parse_cte_name :: proc(p: ^Parser) -> ast.Token_Text {
	start_index := p.index
	start := current_token(p)
	for !at_eof(p) && !at_keyword(p, "AS") && current_token(p).kind != .Comma {
		bump_token(p)
	}
	if p.index == start_index {
		return {}
	}
	range := tokenizer.text_range(start.range.start, previous_token(p).range.end)
	return parser_ast_token(parser_intern_name(p, p.source[range.start:range.end]), range)
}

Select_Clause_State :: struct {
	from:               bool,
	result:             bool,
	result_closes_tail: bool,
	has_where:          bool,
	group_by:           bool,
	fields:             bool,
	having:             bool,
	order_by:           bool,
	for_all_entries:    bool,
	for_update:         bool,
	up_to:              bool,
	package_size:       bool,
	offset:             bool,
	bypassing:          bool,
	connection:         bool,
	client:             bool,
}

OPEN_SQL_HOST_ESCAPE_MESSAGE :: "syntax error: when escaped, all host variables in an Open SQL statement must be escaped using @"
OPEN_SQL_INLINE_DATA_TARGET_MESSAGE :: "syntax error: Open SQL inline DATA target requires @"
OPEN_SQL_RESULT_TARGET_MESSAGE :: "syntax error: invalid SELECT result target"
OPEN_SQL_MISSING_ENDSELECT_MESSAGE :: "syntax error: SELECT without SINGLE or INTO TABLE requires ENDSELECT"
OPEN_SQL_FOR_ALL_ENTRIES_GROUP_BY_MESSAGE :: "syntax error: GROUP BY cannot be used with FOR ALL ENTRIES"
OPEN_SQL_FOR_ALL_ENTRIES_AGGREGATE_MESSAGE :: "syntax error: aggregate functions other than COUNT( * ) cannot be used with FOR ALL ENTRIES"
OPEN_SQL_ORDER_BY_ALIAS_MESSAGE :: "syntax error: Open SQL ORDER BY fields cannot be qualified with a table alias"
OPEN_SQL_UNEXPECTED_TOKEN_MESSAGE :: "syntax error: unexpected token in Open SQL SELECT statement"
OPEN_SQL_ORDER_BY_DIRECTION_MESSAGE :: "syntax error: expected ASCENDING or DESCENDING in ORDER BY"
OPEN_SQL_ORDER_BY_COMMA_MESSAGE :: "syntax error: expected ',' between ORDER BY fields"

SELECT_RESULT_TARGET_STOP_KEYWORDS :: []string {
	"PACKAGE",
	"WHERE",
	"GROUP",
	"FIELDS",
	"HAVING",
	"ORDER",
	"UP",
	"FOR",
	"FROM",
	"OFFSET",
	"BYPASSING",
	"CONNECTION",
	"CLIENT",
	"UNION",
	"INTERSECT",
	"EXCEPT",
	"SELECT",
}

select_reject_clause :: proc(
	p: ^Parser,
	start: Token,
	message: string,
	body_start: int,
	stop_at_rparen: bool,
) {
	error(p, start.range, message)
	_ = select_skip_clause(p, start, body_start, stop_at_rparen)
}

select_skip_to_query_end :: proc(p: ^Parser, body_start: int, stop_at_rparen: bool) {
	for !select_query_done(p, body_start, stop_at_rparen) {
		bump_token(p)
	}
}

parse_select_query_clause :: proc(
	p: ^Parser,
	body_start: int,
	stop_at_rparen := false,
) -> ast.Select_Query_Clause {
	query := ast.Select_Query_Clause{}
	query.projections = make([dynamic]^ast.Expr, 0, 4, p.allocator)
	query.projection_clauses = make([dynamic]ast.Select_Projection_Clause, 0, 4, p.allocator)
	query.group_by = make([dynamic]ast.Select_Group_By_Expr, 0, 2, p.allocator)
	query.set_ops = make([dynamic]ast.Select_Set_Clause, 0, 1, p.allocator)
	query.order_by_fields = make([dynamic]ast.Token_Text, 0, 2, p.allocator)
	if !allow_keyword(p, "SELECT") {
		return query
	}
	query.single = allow_keyword(p, "SINGLE")
	query.is_distinct = allow_keyword(p, "DISTINCT")
	for !select_query_done(p, body_start, stop_at_rparen) && !select_clause_starts(p) {
		if allow_token(p, .Comma) {
			continue
		}
		if star := current_token(p); allow_token(p, .Star) {
			value := sql_star_expr(p, star.range, "", tokenizer.Range{}, star.range)
			append(&query.projections, value)
			append(
				&query.projection_clauses,
				ast.Select_Projection_Clause{value = value, range = value.range},
			)
			query.projection_clause = select_merge_range(query.projection_clause, value.range)
			continue
		}
		start := p.index
		value, is_dynamic := select_sql_projection_expr(
			p,
			body_start,
			[]string {
				"FROM",
				"INTO",
				"APPENDING",
				"WHERE",
				"FOR",
				"GROUP",
				"FIELDS",
				"HAVING",
				"ORDER",
				"UP",
				"PACKAGE",
				"OFFSET",
				"BYPASSING",
				"CONNECTION",
				"CLIENT",
				"AS",
				"ON",
				"INNER",
				"LEFT",
				"RIGHT",
				"FULL",
				"CROSS",
				"JOIN",
				"UNION",
				"INTERSECT",
				"EXCEPT",
				"SELECT",
			},
		)
		if value != nil {
			alias := parse_select_alias(p)
			projection_range := tokenizer.text_range(
				value.range.start,
				previous_token(p).range.end,
			)
			append(&query.projections, value)
			append(
				&query.projection_clauses,
				ast.Select_Projection_Clause {
					value = value,
					alias = alias,
					is_dynamic = is_dynamic,
					range = projection_range,
				},
			)
			query.projection_clause = select_merge_range(query.projection_clause, projection_range)
		} else {
			bump_token(p)
		}
		ensure_forward_progress(p, start)
	}
	state := Select_Clause_State{}
	for !select_query_done(p, body_start, stop_at_rparen) {
		if state.result_closes_tail &&
		   select_clause_starts(p) &&
		   !at_keyword(p, "UP") &&
		   !at_keyword(p, "FIELDS") &&
		   !at_keyword(p, "OFFSET") &&
		   !at_keyword(p, "BYPASSING") &&
		   !at_keyword(p, "CONNECTION") &&
		   !at_keyword(p, "CLIENT") {
			start := bump_token(p)
			select_reject_clause(
				p,
				start,
				"syntax error: invalid SELECT clause after result target",
				body_start,
				stop_at_rparen,
			)
			continue
		}
		if at_keyword(p, "FROM") {
			start := bump_token(p)
			if state.from {
				select_reject_clause(
					p,
					start,
					"syntax error: duplicate SELECT FROM clause",
					body_start,
					stop_at_rparen,
				)
				continue
			}
			query.source_clause = parse_select_source_clause(p, body_start)
			if query.source_clause != nil {
				query.source = query.source_clause.source
				query.from_clause = query.source_clause.range
			}
			state.from = true
			continue
		}
		if at_keyword(p, "INTO") {
			start := bump_token(p)
			if state.result {
				select_reject_clause(
					p,
					start,
					"syntax error: duplicate SELECT result clause",
					body_start,
					stop_at_rparen,
				)
				continue
			}
			query.result = parse_select_result_tail(p, .Into, body_start, true)
			query.into_clause = query.result.range if query.result != nil else tokenizer.Range{}
			state.result = true
			state.result_closes_tail =
				state.has_where || state.group_by || state.having || state.order_by
			continue
		}
		if at_keyword(p, "APPENDING") {
			start := bump_token(p)
			if state.result {
				select_reject_clause(
					p,
					start,
					"syntax error: duplicate SELECT result clause",
					body_start,
					stop_at_rparen,
				)
				continue
			}
			query.result = parse_select_result_tail(p, .Appending, body_start, true)
			query.into_clause = query.result.range if query.result != nil else tokenizer.Range{}
			state.result = true
			state.result_closes_tail =
				state.has_where || state.group_by || state.having || state.order_by
			continue
		}
		if at_keyword(p, "WHERE") {
			start := bump_token(p)
			if !state.from || state.has_where || state.group_by || state.having || state.order_by {
				select_reject_clause(
					p,
					start,
					"syntax error: invalid SELECT WHERE clause placement",
					body_start,
					stop_at_rparen,
				)
				continue
			}
			query.where_cond, query.dynamic_where = select_sql_logical_expr(
				p,
				body_start,
				[]string {
					"INTO",
					"APPENDING",
					"WHERE",
					"FOR",
					"GROUP",
					"FIELDS",
					"HAVING",
					"ORDER",
					"UP",
					"PACKAGE",
					"OFFSET",
					"BYPASSING",
					"CONNECTION",
					"CLIENT",
					"UNION",
					"INTERSECT",
					"EXCEPT",
				},
			)
			if query.where_cond != nil {
				query.where_clause = select_clause_expr_range(p, start, query.where_cond)
				state.has_where = true
			}
			continue
		}
		if allow_keyword(p, "FOR") {
			start := previous_token(p)
			if allow_keyword(p, "ALL") && allow_keyword(p, "ENTRIES") && allow_keyword(p, "IN") {
				if state.group_by {
					select_reject_clause(
						p,
						start,
						OPEN_SQL_FOR_ALL_ENTRIES_GROUP_BY_MESSAGE,
						body_start,
						stop_at_rparen,
					)
					continue
				}
				if !state.from ||
				   state.for_all_entries ||
				   state.has_where ||
				   state.having ||
				   state.order_by {
					select_reject_clause(
						p,
						start,
						"syntax error: invalid SELECT FOR ALL ENTRIES clause placement",
						body_start,
						stop_at_rparen,
					)
					continue
				}
				query.for_all_entries = sql_data_expr(
					p,
					body_start,
					[]string {
						"INTO",
						"APPENDING",
						"WHERE",
						"GROUP",
						"FIELDS",
						"HAVING",
						"ORDER",
						"UP",
						"PACKAGE",
						"OFFSET",
						"BYPASSING",
						"CONNECTION",
						"CLIENT",
						"UNION",
						"INTERSECT",
						"EXCEPT",
					},
				)
				query.for_all_entries = sql_implicit_host_expr(p, query.for_all_entries)
				query.for_all_entries_clause = select_clause_expr_range(
					p,
					start,
					query.for_all_entries,
				)
				state.for_all_entries = true
			} else if allow_keyword(p, "UPDATE") {
				if !state.from || state.for_update {
					select_reject_clause(
						p,
						start,
						"syntax error: invalid SELECT FOR UPDATE clause placement",
						body_start,
						stop_at_rparen,
					)
					continue
				}
				query.for_update_clause = tokenizer.text_range(
					start.range.start,
					previous_token(p).range.end,
				)
				state.for_update = true
			} else {
				select_reject_clause(
					p,
					start,
					"syntax error: invalid SELECT FOR clause",
					body_start,
					stop_at_rparen,
				)
			}
			continue
		}
		if allow_keyword(p, "GROUP") {
			start := previous_token(p)
			if state.for_all_entries {
				select_reject_clause(
					p,
					start,
					OPEN_SQL_FOR_ALL_ENTRIES_GROUP_BY_MESSAGE,
					body_start,
					stop_at_rparen,
				)
				continue
			}
			if !state.from || state.group_by || state.having || state.order_by {
				select_reject_clause(
					p,
					start,
					"syntax error: invalid SELECT GROUP BY clause placement",
					body_start,
					stop_at_rparen,
				)
				continue
			}
			parse_select_group_by_clause(p, &query, start, body_start, stop_at_rparen)
			state.group_by = true
			continue
		}
		if allow_keyword(p, "FIELDS") {
			start := previous_token(p)
			if !state.from ||
			   state.fields ||
			   state.has_where ||
			   state.group_by ||
			   state.having ||
			   state.order_by {
				select_reject_clause(
					p,
					start,
					"syntax error: invalid SELECT FIELDS clause placement",
					body_start,
					stop_at_rparen,
				)
				continue
			}
			parse_select_fields_clause(p, &query, body_start, stop_at_rparen)
			state.fields = true
			continue
		}
		if allow_keyword(p, "HAVING") {
			start := previous_token(p)
			if !state.group_by || state.having || state.order_by {
				select_reject_clause(
					p,
					start,
					"syntax error: invalid SELECT HAVING clause placement",
					body_start,
					stop_at_rparen,
				)
				continue
			}
			query.having_clause = select_skip_clause(p, start, body_start, stop_at_rparen)
			state.having = true
			continue
		}
		if allow_keyword(p, "ORDER") {
			start := previous_token(p)
			if !state.from || state.order_by {
				select_reject_clause(
					p,
					start,
					"syntax error: invalid SELECT ORDER BY clause placement",
					body_start,
					stop_at_rparen,
				)
				continue
			}
			parse_select_order_by_clause(p, &query, start, body_start, stop_at_rparen)
			state.order_by = true
			continue
		}
		if allow_keyword(p, "PACKAGE") {
			start := previous_token(p)
			allow_keyword(p, "SIZE")
			if !state.from || state.package_size {
				select_reject_clause(
					p,
					start,
					"syntax error: invalid SELECT PACKAGE SIZE clause placement",
					body_start,
					stop_at_rparen,
				)
				continue
			}
			query.package_size = sql_data_expr(
				p,
				body_start,
				[]string {
					"INTO",
					"APPENDING",
					"WHERE",
					"GROUP",
					"FIELDS",
					"HAVING",
					"ORDER",
					"UP",
					"OFFSET",
					"BYPASSING",
					"CONNECTION",
					"CLIENT",
					"UNION",
					"INTERSECT",
					"EXCEPT",
				},
			)
			query.package_size_clause = select_clause_expr_range(p, start, query.package_size)
			state.package_size = true
			continue
		}
		if allow_keyword(p, "UP") {
			start := previous_token(p)
			allow_keyword(p, "TO")
			if state.up_to {
				select_reject_clause(
					p,
					start,
					"syntax error: invalid SELECT UP TO clause placement",
					body_start,
					stop_at_rparen,
				)
				continue
			}
			query.up_to_rows = sql_data_expr(
				p,
				body_start,
				[]string {
					"ROWS",
					"INTO",
					"APPENDING",
					"WHERE",
					"GROUP",
					"HAVING",
					"ORDER",
					"PACKAGE",
					"OFFSET",
					"BYPASSING",
					"CONNECTION",
					"CLIENT",
					"UNION",
					"INTERSECT",
					"EXCEPT",
				},
			)
			allow_keyword(p, "ROWS")
			query.up_to_clause = select_clause_expr_range(p, start, query.up_to_rows)
			state.up_to = true
			continue
		}
		if allow_keyword(p, "OFFSET") {
			start := previous_token(p)
			if !state.from || state.offset {
				select_reject_clause(
					p,
					start,
					"syntax error: invalid SELECT OFFSET clause placement",
					body_start,
					stop_at_rparen,
				)
				continue
			}
			query.offset_clause = select_skip_clause(p, start, body_start, stop_at_rparen)
			state.offset = true
			continue
		}
		if at_keyword(p, "BYPASSING") || at_keyword(p, "CONNECTION") || at_keyword(p, "CLIENT") {
			start := bump_token(p)
			is_bypassing := token_is_keyword(p, start, "BYPASSING")
			is_connection := token_is_keyword(p, start, "CONNECTION")
			duplicate :=
				state.bypassing if is_bypassing else (state.connection if is_connection else state.client)
			if !state.from || duplicate {
				select_reject_clause(
					p,
					start,
					"syntax error: invalid SELECT ABAP options clause placement",
					body_start,
					stop_at_rparen,
				)
				continue
			}
			if is_bypassing {
				state.bypassing = true
			} else if is_connection {
				state.connection = true
			} else {
				state.client = true
			}
			query.abap_options_clause = select_merge_range(
				query.abap_options_clause,
				select_skip_clause(p, start, body_start, stop_at_rparen),
			)
			continue
		}
		if kind, ok := select_set_kind(p); ok {
			set_start := current_token(p)
			bump_token(p)
			all := allow_keyword(p, "ALL")
			if !state.from || !at_keyword(p, "SELECT") {
				error(
					p,
					set_start.range,
					"syntax error: SELECT set operator requires following SELECT",
				)
				select_skip_to_query_end(p, body_start, stop_at_rparen)
				continue
			}
			query.set_operator_clause = select_merge_range(
				query.set_operator_clause,
				tokenizer.text_range(set_start.range.start, previous_token(p).range.end),
			)
			append(
				&query.set_ops,
				ast.Select_Set_Clause {
					kind = kind,
					all = all,
					query = parse_select_query_clause(p, body_start, stop_at_rparen),
				},
			)
			continue
		}
		select_reject_unexpected_tail(p, body_start, stop_at_rparen)
	}
	validate_select_query_for_all_entries_aggregates(p, &query)
	validate_select_query_host_escapes(p, &query)
	return query
}

select_reject_unexpected_tail :: proc(p: ^Parser, body_start: int, stop_at_rparen: bool) {
	start := current_token(p)
	for !select_query_done(p, body_start, stop_at_rparen) && !select_clause_starts(p) {
		bump_token(p)
	}
	error(
		p,
		tokenizer.text_range(start.range.start, previous_token(p).range.end),
		OPEN_SQL_UNEXPECTED_TOKEN_MESSAGE,
	)
}

validate_select_query_for_all_entries_aggregates :: proc(p: ^Parser, query: ^ast.Select_Query_Clause) {
	if query.for_all_entries == nil {
		return
	}
	visitor := ast.Visitor {
		visit = select_for_all_entries_aggregate_visit,
		data  = rawptr(p),
	}
	for projection in query.projections {
		ast.walk(&visitor, projection)
	}
}

select_for_all_entries_aggregate_visit :: proc(v: ^ast.Visitor, node: ^ast.Node) -> ^ast.Visitor {
	if node == nil {
		return v
	}
	call, ok := node.derived.(^ast.Sql_Call_Expr)
	if ok && call.kind == .Aggregate && !select_sql_call_is_count_star(call) {
		p := cast(^Parser)v.data
		error(p, call.range, OPEN_SQL_FOR_ALL_ENTRIES_AGGREGATE_MESSAGE)
	}
	return v
}

select_sql_call_is_count_star :: proc(call: ^ast.Sql_Call_Expr) -> bool {
	if call == nil ||
	   call.kind != .Aggregate ||
	   !strings.equal_fold(call.name.text, "COUNT") ||
	   call.modifier != .None ||
	   len(call.args) != 1 {
		return false
	}
	_, ok := call.args[0].derived_expr.(^ast.Sql_Star_Expr)
	return ok
}

validate_select_query_host_escapes :: proc(p: ^Parser, query: ^ast.Select_Query_Clause) {
	if !select_query_has_explicit_host(query) {
		return
	}
	for projection in query.projection_clauses {
		select_error_implicit_hosts_in_expr(p, projection.value)
	}
	for group_expr in query.group_by {
		select_error_implicit_hosts_in_expr(p, group_expr.value)
	}
	if query.source_clause != nil {
		select_error_implicit_hosts_in_expr(p, query.source_clause.source)
		for join in query.source_clause.joins {
			select_error_implicit_hosts_in_expr(p, join.source)
			select_error_implicit_hosts_in_expr(p, join.on)
		}
	}
	if query.result != nil {
		select_error_implicit_hosts_in_expr(p, query.result.target)
	}
	select_error_implicit_hosts_in_expr(p, query.where_cond)
	select_error_implicit_hosts_in_expr(p, query.for_all_entries)
	select_error_implicit_hosts_in_expr(p, query.package_size)
	select_error_implicit_hosts_in_expr(p, query.up_to_rows)
}

select_query_has_explicit_host :: proc(query: ^ast.Select_Query_Clause) -> bool {
	for projection in query.projection_clauses {
		if sql_expr_has_explicit_host(projection.value) {
			return true
		}
	}
	for group_expr in query.group_by {
		if sql_expr_has_explicit_host(group_expr.value) {
			return true
		}
	}
	if query.source_clause != nil {
		if sql_expr_has_explicit_host(query.source_clause.source) {
			return true
		}
		for join in query.source_clause.joins {
			if sql_expr_has_explicit_host(join.source) || sql_expr_has_explicit_host(join.on) {
				return true
			}
		}
	}
	if query.result != nil && sql_expr_has_explicit_host(query.result.target) {
		return true
	}
	return(
		sql_expr_has_explicit_host(query.where_cond) ||
		sql_expr_has_explicit_host(query.for_all_entries) ||
		sql_expr_has_explicit_host(query.package_size) ||
		sql_expr_has_explicit_host(query.up_to_rows) \
	)
}

sql_expr_has_explicit_host :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	has_explicit := false
	select_visit_host_escapes(nil, expr, &has_explicit, false)
	return has_explicit
}

select_error_implicit_hosts_in_expr :: proc(p: ^Parser, expr: ^ast.Expr) {
	if expr == nil {
		return
	}
	has_explicit := false
	select_visit_host_escapes(p, expr, &has_explicit, true)
}

select_visit_host_escapes :: proc(
	p: ^Parser,
	expr: ^ast.Expr,
	has_explicit: ^bool,
	error_implicit: bool,
) {
	if expr == nil {
		return
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Host_Expr:
		if n.implicit {
			if error_implicit {
				error(p, n.range, OPEN_SQL_HOST_ESCAPE_MESSAGE)
			}
		} else {
			has_explicit^ = true
		}
		return
	case ^ast.Binary_Expr:
		select_visit_host_escapes(p, n.left, has_explicit, error_implicit)
		select_visit_host_escapes(p, n.right, has_explicit, error_implicit)
	case ^ast.Unary_Expr:
		select_visit_host_escapes(p, n.expr, has_explicit, error_implicit)
	case ^ast.Paren_Expr:
		select_visit_host_escapes(p, n.expr, has_explicit, error_implicit)
	case ^ast.Selector_Expr:
		select_visit_host_escapes(p, n.base, has_explicit, error_implicit)
		select_visit_host_escapes(p, n.field, has_explicit, error_implicit)
	case ^ast.Interface_Qualified_Selector_Expr:
		select_visit_host_escapes(p, n.receiver, has_explicit, error_implicit)
		select_visit_host_escapes(p, n.interface, has_explicit, error_implicit)
		select_visit_host_escapes(p, n.member, has_explicit, error_implicit)
	case ^ast.Table_Expr:
		select_visit_host_escapes(p, n.table, has_explicit, error_implicit)
		for selector in n.selectors {
			select_visit_host_escapes(p, selector, has_explicit, error_implicit)
		}
	case ^ast.Substring_Expr:
		select_visit_host_escapes(p, n.base, has_explicit, error_implicit)
		select_visit_host_escapes(p, n.offset, has_explicit, error_implicit)
		select_visit_host_escapes(p, n.length, has_explicit, error_implicit)
	case ^ast.Call_Expr:
		select_visit_host_escapes(p, n.callee, has_explicit, error_implicit)
		select_visit_host_escapes(p, n.args, has_explicit, error_implicit)
	case ^ast.Call_Arg_List_Expr:
		for arg in n.args {
			select_visit_host_escapes(p, arg, has_explicit, error_implicit)
		}
	case ^ast.Call_Arg_Section_Expr:
		for arg in n.args {
			select_visit_host_escapes(p, arg, has_explicit, error_implicit)
		}
	case ^ast.Call_Named_Arg_Expr:
		select_visit_host_escapes(p, n.value, has_explicit, error_implicit)
	case ^ast.Call_Positional_Arg_Expr:
		select_visit_host_escapes(p, n.value, has_explicit, error_implicit)
	case ^ast.Sql_Call_Expr:
		for arg in n.args {
			select_visit_host_escapes(p, arg, has_explicit, error_implicit)
		}
	case ^ast.Is_Predicate_Expr:
		select_visit_host_escapes(p, n.subject, has_explicit, error_implicit)
	case ^ast.Instance_Of_Predicate_Expr:
		select_visit_host_escapes(p, n.subject, has_explicit, error_implicit)
		select_visit_host_escapes(p, n.type_ref, has_explicit, error_implicit)
	case ^ast.Between_Expr:
		select_visit_host_escapes(p, n.subject, has_explicit, error_implicit)
		select_visit_host_escapes(p, n.low, has_explicit, error_implicit)
		select_visit_host_escapes(p, n.high, has_explicit, error_implicit)
	case ^ast.Sql_Case_When_Expr:
		select_visit_host_escapes(p, n.condition, has_explicit, error_implicit)
		select_visit_host_escapes(p, n.result, has_explicit, error_implicit)
	case ^ast.Sql_Case_Expr:
		select_visit_host_escapes(p, n.operand, has_explicit, error_implicit)
		for when_expr in n.whens {
			select_visit_host_escapes(p, when_expr, has_explicit, error_implicit)
		}
		select_visit_host_escapes(p, n.else_expr, has_explicit, error_implicit)
	}
}

parse_select_fields_clause :: proc(
	p: ^Parser,
	query: ^ast.Select_Query_Clause,
	body_start: int,
	stop_at_rparen: bool,
) {
	for !select_query_done(p, body_start, stop_at_rparen) && !select_clause_starts(p) {
		if allow_token(p, .Comma) {
			continue
		}
		start := p.index
		value, is_dynamic := select_sql_projection_expr(
			p,
			body_start,
			[]string {
				"INTO",
				"APPENDING",
				"WHERE",
				"FOR",
				"GROUP",
				"HAVING",
				"ORDER",
				"UP",
				"PACKAGE",
				"OFFSET",
				"BYPASSING",
				"CONNECTION",
				"CLIENT",
				"UNION",
				"INTERSECT",
				"EXCEPT",
				"SELECT",
			},
		)
		if value != nil {
			alias := parse_select_alias(p)
			projection_range := tokenizer.text_range(
				value.range.start,
				previous_token(p).range.end,
			)
			append(&query.projections, value)
			append(
				&query.projection_clauses,
				ast.Select_Projection_Clause {
					value = value,
					alias = alias,
					is_dynamic = is_dynamic,
					range = projection_range,
				},
			)
			query.projection_clause = select_merge_range(query.projection_clause, projection_range)
		} else {
			bump_token(p)
		}
		ensure_forward_progress(p, start)
	}
}

parse_select_group_by_clause :: proc(
	p: ^Parser,
	query: ^ast.Select_Query_Clause,
	start: Token,
	body_start: int,
	stop_at_rparen: bool,
) {
	if !allow_keyword(p, "BY") {
		error_current(p, "syntax error: expected keyword")
		query.group_by_clause = select_skip_clause(p, start, body_start, stop_at_rparen)
		return
	}
	for !select_query_done(p, body_start, stop_at_rparen) && !select_clause_starts(p) {
		if allow_token(p, .Comma) {
			continue
		}
		expr_start := p.index
		value, is_dynamic := select_sql_projection_expr(
			p,
			body_start,
			[]string {
				"INTO",
				"APPENDING",
				"WHERE",
				"FOR",
				"GROUP",
				"FIELDS",
				"HAVING",
				"ORDER",
				"UP",
				"PACKAGE",
				"OFFSET",
				"BYPASSING",
				"CONNECTION",
				"CLIENT",
				"UNION",
				"INTERSECT",
				"EXCEPT",
				"SELECT",
			},
		)
		if value != nil {
			append(
				&query.group_by,
				ast.Select_Group_By_Expr {
					value = value,
					is_dynamic = is_dynamic,
					range = value.range,
				},
			)
		} else {
			bump_token(p)
		}
		ensure_forward_progress(p, expr_start)
	}
	query.group_by_clause = tokenizer.text_range(start.range.start, previous_token(p).range.end)
}

select_clause_expr_range :: proc(p: ^Parser, start: Token, expr: ^ast.Expr) -> tokenizer.Range {
	end := previous_token(p).range.end
	if expr != nil {
		end = max(end, expr.range.end)
	}
	return tokenizer.text_range(start.range.start, end)
}

select_skip_clause :: proc(
	p: ^Parser,
	start: Token,
	body_start: int,
	stop_at_rparen: bool,
) -> tokenizer.Range {
	for !select_query_done(p, body_start, stop_at_rparen) && !select_clause_starts(p) {
		bump_token(p)
	}
	return tokenizer.text_range(start.range.start, previous_token(p).range.end)
}

parse_select_order_by_clause :: proc(
	p: ^Parser,
	query: ^ast.Select_Query_Clause,
	start: Token,
	body_start: int,
	stop_at_rparen: bool,
) {
	if !allow_keyword(p, "BY") {
		error_current(p, "syntax error: expected keyword")
		query.order_by_clause = select_skip_clause(p, start, body_start, stop_at_rparen)
		return
	}
	if allow_keyword(p, "PRIMARY") {
		query.order_by_primary_key = allow_keyword(p, "KEY")
		query.order_by_clause = select_skip_clause(p, start, body_start, stop_at_rparen)
		return
	}
	needs_comma := false
	for !select_query_done(p, body_start, stop_at_rparen) && !select_clause_starts(p) {
		if allow_token(p, .Comma) {
			needs_comma = false
			continue
		}
		if allow_keyword(p, "ASCENDING") ||
		   allow_keyword(p, "NULLS") ||
		   allow_keyword(p, "FIRST") ||
		   allow_keyword(p, "LAST") {
			continue
		}
		if allow_keyword(p, "DESCENDING") {
			query.order_by_has_descending = true
			continue
		}
		if select_order_by_direction_typo(p, current_token(p)) {
			error_current(p, OPEN_SQL_ORDER_BY_DIRECTION_MESSAGE)
			bump_token(p)
			continue
		}
		if current_token(p).kind == .Ident {
			if needs_comma {
				error_current(p, OPEN_SQL_ORDER_BY_COMMA_MESSAGE)
			}
			if p.index + 2 < len(p.tokens) &&
			   p.tokens[p.index + 1].kind == .Tilde &&
			   p.tokens[p.index + 2].kind == .Ident {
				error(
					p,
					tokenizer.text_range(current_token(p).range.start, p.tokens[p.index + 2].range.end),
					OPEN_SQL_ORDER_BY_ALIAS_MESSAGE,
				)
				append(
					&query.order_by_fields,
					parser_ast_name_token(p, p.tokens[p.index + 2]),
				)
				bump_token(p)
				bump_token(p)
				bump_token(p)
			} else {
				append(
					&query.order_by_fields,
					parser_ast_name_token(p, current_token(p)),
				)
				bump_token(p)
			}
			needs_comma = true
			continue
		}
		bump_token(p)
	}
	query.order_by_clause = tokenizer.text_range(start.range.start, previous_token(p).range.end)
}

select_order_by_direction_typo :: proc(p: ^Parser, tok: Token) -> bool {
	if tok.kind != .Ident {
		return false
	}
	text := tokenizer.token_lexeme(tok, p.source)
	return strings.equal_fold(text, "ASCENDIN") || strings.equal_fold(text, "DESCENDIN")
}

select_merge_range :: proc(a, b: tokenizer.Range) -> tokenizer.Range {
	if b.end <= b.start {
		return a
	}
	if a.end <= a.start {
		return b
	}
	return tokenizer.text_range(min(a.start, b.start), max(a.end, b.end))
}

select_range_valid :: proc(range: tokenizer.Range) -> bool {
	return range.end > range.start
}

select_query_done :: proc(p: ^Parser, body_start: int, stop_at_rparen: bool) -> bool {
	return data_stmt_done(p, body_start) || (stop_at_rparen && current_token(p).kind == .RParen)
}

parse_select_source_clause :: proc(p: ^Parser, body_start: int) -> ^ast.Select_Source_Clause {
	clause, _ := mem.new(ast.Select_Source_Clause, p.allocator)
	clause.joins = make([dynamic]ast.Select_Join_Clause, 0, 2, p.allocator)
	start := current_token(p)
	clause.dynamic_source = current_token(p).kind == .LParen
	clause.source = parse_select_source_expr(
		p,
		body_start,
		[]string {
			"AS",
			"INNER",
			"LEFT",
			"RIGHT",
			"FULL",
			"CROSS",
			"JOIN",
			"INTO",
			"APPENDING",
			"WHERE",
			"FOR",
			"GROUP",
			"FIELDS",
			"HAVING",
			"ORDER",
			"UP",
			"PACKAGE",
			"OFFSET",
			"BYPASSING",
			"CONNECTION",
			"CLIENT",
			"UNION",
			"INTERSECT",
			"EXCEPT",
			"SELECT",
		},
	)
	clause.source = sql_model_select_source_expr(p, clause.source)
	if clause.source == nil {
		error_current(p, "syntax error: expected SELECT source")
	}
	clause.alias = parse_select_alias(p)
	for !data_stmt_done(p, body_start) && !select_clause_starts(p) {
		kind, ok := select_join_kind(p)
		if !ok {
			break
		}
		join := ast.Select_Join_Clause {
			kind = kind,
		}
		join.source = parse_select_source_expr(
			p,
			body_start,
			[]string {
				"AS",
				"ON",
				"INNER",
				"LEFT",
				"RIGHT",
				"FULL",
				"CROSS",
				"JOIN",
				"INTO",
				"APPENDING",
				"WHERE",
				"FOR",
				"GROUP",
				"FIELDS",
				"HAVING",
				"ORDER",
				"UP",
				"PACKAGE",
				"OFFSET",
				"BYPASSING",
				"CONNECTION",
				"CLIENT",
				"UNION",
				"INTERSECT",
				"EXCEPT",
				"SELECT",
			},
		)
		join.source = sql_model_select_source_expr(p, join.source)
		if join.source == nil {
			error_current(p, "syntax error: expected SELECT JOIN source")
		}
		join.alias = parse_select_alias(p)
		if allow_keyword(p, "ON") {
			join.on, _ = select_sql_logical_expr(
				p,
				body_start,
				[]string {
					"INNER",
					"LEFT",
					"RIGHT",
					"FULL",
					"CROSS",
					"JOIN",
					"INTO",
					"APPENDING",
					"WHERE",
					"FOR",
					"GROUP",
					"FIELDS",
					"HAVING",
					"ORDER",
					"UP",
					"PACKAGE",
					"OFFSET",
					"BYPASSING",
					"CONNECTION",
					"CLIENT",
					"UNION",
					"INTERSECT",
					"EXCEPT",
				},
			)
		}
		append(&clause.joins, join)
	}
	if clause.source != nil {
		clause.range = tokenizer.text_range(start.range.start, previous_token(p).range.end)
	}
	return clause
}

parse_select_source_expr :: proc(
	p: ^Parser,
	body_start: int,
	stop_keywords: []string,
) -> ^ast.Expr {
	value := sql_data_expr(p, body_start, stop_keywords)
	if value != nil ||
	   !select_join_keyword_at(p, p.index) ||
	   select_join_kind_starts_at(p, p.index) {
		return value
	}
	tok := bump_token(p)
	expr := ast.new(ast.Ident_Expr, tok.range, p.allocator)
	expr.name = parser_intern_token_name(p, tok)
	return expr
}

parse_select_alias :: proc(p: ^Parser) -> ast.Token_Text {
	if !allow_keyword(p, "AS") {
		return {}
	}
	tok := current_token(p)
	if tok.kind != .Ident || select_reserved_name_at(p, p.index) {
		error(p, tok.range, "syntax error: expected alias after AS")
		return {}
	}
	bump_token(p)
	return parser_ast_name_token(p, tok)
}

select_join_kind :: proc(p: ^Parser) -> (ast.Select_Join_Kind, bool) {
	if allow_keyword(p, "INNER") {
		if !allow_keyword(p, "JOIN") {
			error_current(p, "syntax error: expected JOIN")
			return .Inner, false
		}
		return .Inner, true
	}
	if allow_keyword(p, "LEFT") {
		allow_keyword(p, "OUTER")
		if !allow_keyword(p, "JOIN") {
			error_current(p, "syntax error: expected JOIN")
			return .Left_Outer, false
		}
		return .Left_Outer, true
	}
	if allow_keyword(p, "RIGHT") {
		allow_keyword(p, "OUTER")
		if !allow_keyword(p, "JOIN") {
			error_current(p, "syntax error: expected JOIN")
			return .Right_Outer, false
		}
		return .Right_Outer, true
	}
	if allow_keyword(p, "FULL") {
		allow_keyword(p, "OUTER")
		if !allow_keyword(p, "JOIN") {
			error_current(p, "syntax error: expected JOIN")
			return .Full_Outer, false
		}
		return .Full_Outer, true
	}
	if allow_keyword(p, "CROSS") {
		if !allow_keyword(p, "JOIN") {
			error_current(p, "syntax error: expected JOIN")
			return .Cross, false
		}
		return .Cross, true
	}
	if allow_keyword(p, "JOIN") {
		return .Inner, true
	}
	return .Inner, false
}

select_join_kind_starts_at :: proc(p: ^Parser, index: int) -> bool {
	if at_keyword_index(p, index, "INNER") {
		return at_keyword_index(p, index + 1, "JOIN")
	}
	if at_keyword_index(p, index, "LEFT") ||
	   at_keyword_index(p, index, "RIGHT") ||
	   at_keyword_index(p, index, "FULL") {
		return(
			at_keyword_index(p, index + 1, "JOIN") ||
			(at_keyword_index(p, index + 1, "OUTER") && at_keyword_index(p, index + 2, "JOIN")) \
		)
	}
	if at_keyword_index(p, index, "CROSS") {
		return at_keyword_index(p, index + 1, "JOIN")
	}
	return at_keyword_index(p, index, "JOIN")
}

select_set_kind :: proc(p: ^Parser) -> (ast.Select_Set_Kind, bool) {
	if at_keyword(p, "UNION") {
		return .Union, true
	}
	if at_keyword(p, "INTERSECT") {
		return .Intersect, true
	}
	if at_keyword(p, "EXCEPT") {
		return .Except, true
	}
	return .Union, false
}

parse_select_result_tail :: proc(
	p: ^Parser,
	kind: ast.Select_Result_Kind,
	body_start: int,
	validate_open_sql_target := false,
) -> ^ast.Select_Result_Clause {
	clause, _ := mem.new(ast.Select_Result_Clause, p.allocator)
	start := previous_token(p)
	clause.kind = kind
	if allow_keyword(p, "CORRESPONDING") {
		allow_keyword(p, "FIELDS")
		allow_keyword(p, "OF")
		clause.corresponding_fields = true
	}
	clause.table = allow_keyword(p, "TABLE")
	target_start := current_token(p)
	parenthesized_target := target_start.kind == .LParen
	if parenthesized_target && !validate_open_sql_target {
		clause.target = parse_raw_operand_to_period(
			p,
			SELECT_RESULT_TARGET_STOP_KEYWORDS,
		)
	} else {
		clause.target = sql_data_expr(
			p,
			body_start,
			SELECT_RESULT_TARGET_STOP_KEYWORDS,
		)
	}
	if validate_open_sql_target {
		clause.target = sql_implicit_host_expr(p, clause.target)
	}
	if validate_open_sql_target && parenthesized_target {
		target_range := clause.target.range if clause.target != nil else target_start.range
		error(p, target_range, OPEN_SQL_RESULT_TARGET_MESSAGE)
	}
	if validate_open_sql_target && select_result_target_unescaped_inline_data(clause.target) {
		error(p, clause.target.range, OPEN_SQL_INLINE_DATA_TARGET_MESSAGE)
	}
	if clause.target == nil {
		error_current(p, "syntax error: expected SELECT result target")
	} else if validate_open_sql_target {
		_ = closing_delimiter_error(p)
	}
	clause.range = tokenizer.text_range(start.range.start, previous_token(p).range.end)
	return clause
}

select_result_target_unescaped_inline_data :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	#partial switch _ in expr.derived_expr {
	case ^ast.Data_Inline_Name_Expr:
		return true
	case ^ast.Host_Expr:
		return false
	}
	return false
}

select_clause_starts :: proc(p: ^Parser) -> bool {
	return select_clause_starts_at(p, p.index)
}

select_clause_starts_at :: proc(p: ^Parser, index: int) -> bool {
	return(
		at_keyword_index(p, index, "FROM") ||
		at_keyword_index(p, index, "INTO") ||
		at_keyword_index(p, index, "APPENDING") ||
		at_keyword_index(p, index, "WHERE") ||
		at_keyword_index(p, index, "FOR") ||
		at_keyword_index(p, index, "GROUP") ||
		at_keyword_index(p, index, "FIELDS") ||
		at_keyword_index(p, index, "HAVING") ||
		at_keyword_index(p, index, "ORDER") ||
		at_keyword_index(p, index, "UP") ||
		at_keyword_index(p, index, "PACKAGE") ||
		at_keyword_index(p, index, "OFFSET") ||
		at_keyword_index(p, index, "BYPASSING") ||
		at_keyword_index(p, index, "CONNECTION") ||
		at_keyword_index(p, index, "CLIENT") ||
		at_keyword_index(p, index, "UNION") ||
		at_keyword_index(p, index, "INTERSECT") ||
		at_keyword_index(p, index, "EXCEPT") \
	)
}

select_reserved_name_at :: proc(p: ^Parser, index: int) -> bool {
	return(
		select_clause_starts_at(p, index) ||
		select_join_keyword_at(p, index) ||
		at_keyword_index(p, index, "AS") ||
		at_keyword_index(p, index, "ON") ||
		at_keyword_index(p, index, "SELECT") ||
		known_stmt_lead_at(p, index) \
	)
}

select_join_keyword_at :: proc(p: ^Parser, index: int) -> bool {
	return(
		at_keyword_index(p, index, "INNER") ||
		at_keyword_index(p, index, "LEFT") ||
		at_keyword_index(p, index, "RIGHT") ||
		at_keyword_index(p, index, "FULL") ||
		at_keyword_index(p, index, "CROSS") ||
		at_keyword_index(p, index, "JOIN") \
	)
}

parse_open_cursor_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword_phrase(p, "OPEN CURSOR")
	body_start := p.index
	stmt := ast.new(ast.Open_Cursor_Stmt, start.range, p.allocator)
	if allow_keyword(p, "WITH") {
		stmt.with_hold = allow_keyword(p, "HOLD")
	}
	stmt.handle = data_expr(p, body_start, []string{"FOR"})
	if allow_keyword(p, "FOR") {
		stmt.query = parse_select_query_clause(p, body_start)
	}
	stmt.range = data_stmt_range(p, start)
	return stmt
}

parse_fetch_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "FETCH")
	body_start := p.index
	stmt := ast.new(ast.Fetch_Stmt, start.range, p.allocator)
	allow_keyword(p, "NEXT")
	if !allow_keyword(p, "CURSOR") {
		error_current(p, "syntax error: expected keyword")
	}
	stmt.handle = data_expr(p, body_start, []string{"INTO", "APPENDING"})
	for !data_stmt_done(p, body_start) {
		if allow_keyword(p, "INTO") {
			stmt.result = parse_select_result_tail(p, .Into, body_start)
			continue
		}
		if allow_keyword(p, "APPENDING") {
			stmt.result = parse_select_result_tail(p, .Appending, body_start)
			continue
		}
		if allow_keyword(p, "PACKAGE") {
			allow_keyword(p, "SIZE")
			stmt.package_size = data_expr(p, body_start, []string{})
			continue
		}
		bump_token(p)
	}
	stmt.range = data_stmt_range(p, start)
	return stmt
}

parse_close_cursor_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword_phrase(p, "CLOSE CURSOR")
	body_start := p.index
	stmt := ast.new(ast.Close_Cursor_Stmt, start.range, p.allocator)
	stmt.handle = data_expr(p, body_start, []string{})
	consume_data_tail(p, body_start)
	stmt.range = data_stmt_range(p, start)
	return stmt
}

parse_read_table_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword_phrase(p, "READ TABLE")
	body_start := p.index
	stmt := ast.new(ast.Read_Table_Stmt, start.range, p.allocator)
	stmt.entries = make([dynamic]ast.Read_Table_Entry_Clause, 0, 2, p.allocator)
	allow_token(p, .Colon)
	for !data_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {
			continue
		}
		entry := parse_read_table_entry(p, body_start)
		if entry.table != nil || entry.into != nil || entry.assigning != nil {
			append(&stmt.entries, entry)
		} else {
			break
		}
	}
	stmt.range = data_stmt_range(p, start)
	return stmt
}

parse_read_table_entry :: proc(p: ^Parser, body_start: int) -> ast.Read_Table_Entry_Clause {
	entry := ast.Read_Table_Entry_Clause{}
	entry.key_values = make([dynamic]ast.Read_Table_Key_Value_Clause, 0, 2, p.allocator)
	entry.comparing = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	entry.table = data_expr(
		p,
		body_start,
		[]string {
			"INTO",
			"ASSIGNING",
			"WITH",
			"INDEX",
			"USING",
			"TRANSPORTING",
			"COMPARING",
			"BINARY",
			"REFERENCE",
		},
	)
	for !data_stmt_done(p, body_start) && current_token(p).kind != .Comma {
		if allow_keyword(p, "INTO") {
			entry.into = read_table_result_expr(
				p,
				body_start,
				[]string{"WITH", "INDEX", "USING", "TRANSPORTING", "COMPARING", "BINARY"},
			)
			continue
		}
		if allow_keyword(p, "ASSIGNING") {
			entry.assigning = read_table_result_expr(
				p,
				body_start,
				[]string{"WITH", "INDEX", "USING", "TRANSPORTING", "COMPARING", "BINARY"},
			)
			continue
		}
		if allow_keyword(p, "REFERENCE") {
			allow_keyword(p, "INTO")
			entry.reference_into = read_table_result_expr(
				p,
				body_start,
				[]string{"WITH", "INDEX", "USING", "TRANSPORTING", "COMPARING", "BINARY"},
			)
			continue
		}
		if allow_keyword(p, "INDEX") {
			entry.index = data_expr(
				p,
				body_start,
				[]string{"USING", "ASSIGNING", "INTO", "TRANSPORTING", "COMPARING", "BINARY"},
			)
			continue
		}
		if allow_keyword(p, "USING") {
			entry.using_key = parse_table_key_selector(
				p,
				body_start,
				[]string{"ASSIGNING", "INTO", "TRANSPORTING", "COMPARING", "BINARY", "REFERENCE"},
			)
			continue
		}
		if allow_keyword(p, "WITH") {
			entry.key_kind = .Key
			if allow_keyword(p, "TABLE") {
				entry.key_kind = .Table_Key
			}
			allow_keyword(p, "KEY")
			parse_read_table_key_values(p, body_start, &entry)
			continue
		}
		if allow_keyword(p, "TRANSPORTING") {
			if allow_keyword(p, "NO") {
				entry.transporting_no_fields = allow_keyword(p, "FIELDS")
			} else {
				_ = data_exprs_until(
					p,
					body_start,
					[]string{"WITH", "INDEX", "USING", "COMPARING", "BINARY"},
				)
			}
			continue
		}
		if at_keyword(p, "BINARY") {
			binary := bump_token(p)
			if at_keyword(p, "SEARCH") {
				search := bump_token(p)
				entry.binary_search = true
				entry.binary_search_clause = tokenizer.text_range(
					binary.range.start,
					search.range.end,
				)
			}
			continue
		}
		if allow_keyword(p, "COMPARING") {
			more := data_exprs_until(
				p,
				body_start,
				[]string{"WITH", "INDEX", "USING", "TRANSPORTING", "BINARY"},
			)
			for value in more {append(&entry.comparing, value)}
			continue
		}
		bump_token(p)
	}
	return entry
}

read_table_result_expr :: proc(p: ^Parser, body_start: int, stop_keywords: []string) -> ^ast.Expr {
	if (at_keyword(p, "DATA") && next_token_kind(p, 1) == .LParen) ||
	   field_symbol_inline_name_starts(p, p.index) {
		return parse_expr(p)
	}
	return data_expr(p, body_start, stop_keywords)
}

parse_read_table_key_values :: proc(
	p: ^Parser,
	body_start: int,
	entry: ^ast.Read_Table_Entry_Clause,
) {
	for !data_stmt_done(p, body_start) &&
	    current_token(p).kind != .Comma &&
	    !data_current_keyword_in(
			    p,
			    []string {
				    "INTO",
				    "ASSIGNING",
				    "INDEX",
				    "USING",
				    "TRANSPORTING",
				    "COMPARING",
				    "BINARY",
				    "REFERENCE",
			    },
		    ) {
		if allow_keyword(p, "COMPONENTS") {
			continue
		}
		if name_end := read_table_dynamic_key_name_end(p); name_end >= 0 {
			name_start := current_token(p).range.start
			name_end_byte := p.tokens[name_end].range.end
			dynamic_name := parse_required_complete_expr_with(
				p,
				p.index + 1,
				name_end,
				parse_logical_expr,
				"syntax error: expected dynamic READ TABLE key expression",
			)
			for p.index <= name_end {
				bump_token(p)
			}
			expect_token(p, .Eq)
			name_range := tokenizer.text_range(name_start, name_end_byte)
			name := parser_ast_token(parser_clone_range_text(p, name_range), name_range)
			value := data_expr(
				p,
				body_start,
				[]string {
					"INTO",
					"ASSIGNING",
					"INDEX",
					"USING",
					"TRANSPORTING",
					"COMPARING",
					"BINARY",
					"REFERENCE",
				},
			)
			if value == nil {
				error_current(p, "syntax error: expected READ TABLE key value")
			}
			append(
				&entry.key_values,
				ast.Read_Table_Key_Value_Clause {
					name = name,
					dynamic_name = dynamic_name,
					is_dynamic = true,
					value = value,
				},
			)
			continue
		}
		if name_end := read_table_key_name_eq_index(p); name_end >= 0 {
			name_start := current_token(p).range.start
			name_end_byte := p.tokens[name_end - 1].range.end
			path := make([dynamic]ast.Read_Table_Key_Name_Segment, 0, 2, p.allocator)
			selector := ast.Selector_Op.Dash
			for p.index < name_end {
				if current_token(p).kind == .Ident {
					tok := bump_token(p)
					append(
						&path,
						ast.Read_Table_Key_Name_Segment {
							name = parser_ast_raw_name_token(p, tok),
							selector = selector,
						},
					)
					selector = .Dash
					continue
				}
				op := current_token(p)
				if op.kind == .Minus || op.kind == .Arrow {
					selector = selector_op(bump_token(p).kind)
					continue
				}
				error_current(p, "syntax error: expected READ TABLE key selector")
				bump_token(p)
			}
			expect_token(p, .Eq)
			name_range := tokenizer.text_range(name_start, name_end_byte)
			name := parser_ast_token(parser_clone_range_text(p, name_range), name_range)
			value := data_expr(
				p,
				body_start,
				[]string {
					"INTO",
					"ASSIGNING",
					"INDEX",
					"USING",
					"TRANSPORTING",
					"COMPARING",
					"BINARY",
					"REFERENCE",
				},
			)
			append(
				&entry.key_values,
				ast.Read_Table_Key_Value_Clause {
					name = name,
					path = path,
					table_line = len(path) == 1 && strings.equal_fold(path[0].name.text, "table_line"),
					value = value,
				},
			)
			continue
		}
		if current_token(p).kind == .Ident && next_token_kind(p, 1) == .Minus {
			error(
				p,
				current_token(p).range,
				"syntax error: expected READ TABLE key component path",
			)
			bump_token(p)
			continue
		}
		if entry.key_name.text == "" && current_token(p).kind == .Ident {
			entry.key_name = parser_ast_name_token(p, bump_token(p))
			continue
		}
		bump_token(p)
	}
}

read_table_dynamic_key_name_end :: proc(p: ^Parser) -> int {
	if current_token(p).kind != .LParen {
		return -1
	}
	end := matching_group_index(p, p.index, .LParen, .RParen)
	if end > p.index && end + 1 < len(p.tokens) && p.tokens[end + 1].kind == .Eq {
		return end
	}
	return -1
}

read_table_key_name_eq_index :: proc(p: ^Parser) -> int {
	if current_token(p).kind != .Ident {
		return -1
	}
	i := p.index + 1
	for i + 1 < len(p.tokens) &&
	    (p.tokens[i].kind == .Minus || p.tokens[i].kind == .Arrow) &&
	    p.tokens[i + 1].kind == .Ident &&
	    tokens_touch(p.tokens[i - 1], p.tokens[i]) &&
	    tokens_touch(p.tokens[i], p.tokens[i + 1]) {
		i += 2
	}
	if i < len(p.tokens) && p.tokens[i].kind == .Eq {
		return i
	}
	return -1
}

dml_range_valid :: #force_inline proc(range: tokenizer.Range) -> bool {
	return range.end > range.start
}

dml_skip_clause :: proc(
	p: ^Parser,
	start: Token,
	body_start: int,
	stop_keywords: []string,
) -> tokenizer.Range {
	for !data_stmt_done(p, body_start) && !data_current_keyword_in(p, stop_keywords) {
		bump_token(p)
	}
	return tokenizer.text_range(start.range.start, previous_token(p).range.end)
}

dml_reject_clause :: proc(
	p: ^Parser,
	start: Token,
	message: string,
	body_start: int,
	stop_keywords: []string,
) {
	error(p, start.range, message)
	_ = dml_skip_clause(p, start, body_start, stop_keywords)
}

dml_dynamic_source :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	_, ok := expr.derived_expr.(^ast.Paren_Expr)
	return ok
}

sql_dynamic_where_expr :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	paren, ok := expr.derived_expr.(^ast.Paren_Expr)
	return ok && sql_dynamic_where_operand(paren.expr)
}

sql_dynamic_where_operand :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		return true
	case ^ast.Host_Expr:
		return sql_dynamic_where_operand(n.value)
	case ^ast.Selector_Expr:
		return sql_dynamic_where_operand(n.base) && sql_dynamic_where_operand(n.field)
	}
	return false
}

insert_set_db_source_facts :: proc(stmt: ^ast.Insert_Stmt) {
	if stmt.target == nil {
		return
	}
	stmt.db_source_range = stmt.target.range
	stmt.dynamic_source = dml_dynamic_source(stmt.target)
	if id, ok := stmt.target.derived_expr.(^ast.Ident_Expr); ok && id.name != "" {
		stmt.has_db_table_name = true
		stmt.db_table_name = parser_ast_token(id.name, id.range)
	}
}

sql_assignment_column_fact :: proc(expr: ^ast.Expr) -> (string, tokenizer.Range) {
	if expr == nil {
		return "", tokenizer.Range{}
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		return n.name, n.range
	case ^ast.Type_Ref_Expr:
		return n.name.text, n.name.range
	case ^ast.Selector_Expr:
		return sql_assignment_column_fact(n.field)
	}
	return "", tokenizer.Range{}
}

parse_insert_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "INSERT")
	body_start := p.index
	stmt := ast.new(ast.Insert_Stmt, start.range, p.allocator)
	stmt.assignments = make([dynamic]ast.Sql_Assignment_Clause, 0, 2, p.allocator)
	if allow_keyword(p, "INTO") {
		stmt.form = .Db_Table
		stmt.into_db_table = true
		stmt.target = sql_data_expr(
			p,
			body_start,
			[]string{"VALUES", "FROM", "SET", "ACCEPTING", "CLIENT", "CONNECTION"},
		)
		insert_set_db_source_facts(stmt)
		parse_insert_tail(p, body_start, stmt)
		stmt.range = data_stmt_range(p, start)
		return stmt
	}
	first_operand: ^ast.Expr
	if allow_keyword(p, "LINES") {
		allow_keyword(p, "OF")
		stmt.form = .Lines_Of
		stmt.source = data_expr(p, body_start, []string{"INTO", "FROM", "TO", "USING"})
	} else if allow_keyword(p, "INITIAL") {
		allow_keyword(p, "LINE")
		stmt.form = .Internal_Table
		stmt.initial_line = true
	} else {
		first_operand = data_expr(
			p,
			body_start,
			[]string{"INTO", "FROM", "VALUES", "SET", "ACCEPTING", "CLIENT", "CONNECTION"},
		)
		stmt.source = first_operand
	}
	parse_insert_tail(p, body_start, stmt)
	if stmt.form == .Db_Table && stmt.target == nil && first_operand != nil {
		stmt.target = first_operand
		insert_set_db_source_facts(stmt)
	}
	stmt.range = data_stmt_range(p, start)
	return stmt
}

parse_insert_tail :: proc(p: ^Parser, body_start: int, stmt: ^ast.Insert_Stmt) {
	for !data_stmt_done(p, body_start) {
		if allow_keyword(p, "INTO") {
			start := previous_token(p)
			if stmt.form == .Db_Table {
				dml_reject_clause(
					p,
					start,
					"syntax error: conflicting INSERT INTO clause",
					body_start,
					[]string {
						"FROM",
						"VALUES",
						"SET",
						"INDEX",
						"ASSIGNING",
						"REFERENCE",
						"ACCEPTING",
						"CLIENT",
						"CONNECTION",
					},
				)
				continue
			}
			stmt.form = .Internal_Table if stmt.form != .Lines_Of else .Lines_Of
			allow_keyword(p, "TABLE")
			stmt.target = data_expr(
				p,
				body_start,
				[]string{"INDEX", "ASSIGNING", "REFERENCE", "ACCEPTING", "CLIENT", "CONNECTION"},
			)
			continue
		}
		if allow_keyword(p, "FROM") {
			start := previous_token(p)
			if dml_range_valid(stmt.from_clause) ||
			   stmt.form == .Internal_Table ||
			   stmt.form == .Lines_Of {
				dml_reject_clause(
					p,
					start,
					"syntax error: duplicate or conflicting INSERT FROM clause",
					body_start,
					[]string {
						"INTO",
						"FROM",
						"VALUES",
						"SET",
						"INDEX",
						"ASSIGNING",
						"REFERENCE",
						"ACCEPTING",
						"CLIENT",
						"CONNECTION",
					},
				)
				continue
			}
			stmt.form = .Db_Table
			if stmt.target == nil && stmt.source != nil {
				stmt.target = stmt.source
				insert_set_db_source_facts(stmt)
			}
			stmt.from_table = allow_keyword(p, "TABLE")
			stmt.source = data_expr(
				p,
				body_start,
				[]string{"INTO", "FROM", "VALUES", "SET", "ACCEPTING", "CLIENT", "CONNECTION"},
			)
			stmt.from_clause = tokenizer.text_range(start.range.start, previous_token(p).range.end)
			continue
		}
		if allow_keyword(p, "VALUES") {
			start := previous_token(p)
			if stmt.values_clause ||
			   dml_range_valid(stmt.from_clause) ||
			   dml_range_valid(stmt.set_clause) {
				dml_reject_clause(
					p,
					start,
					"syntax error: duplicate or conflicting INSERT VALUES clause",
					body_start,
					[]string {
						"INTO",
						"FROM",
						"VALUES",
						"SET",
						"INDEX",
						"ASSIGNING",
						"REFERENCE",
						"ACCEPTING",
						"CLIENT",
						"CONNECTION",
					},
				)
				continue
			}
			stmt.form = .Db_Table
			if stmt.target == nil && stmt.source != nil {
				stmt.target = stmt.source
				insert_set_db_source_facts(stmt)
			}
			stmt.values_clause = true
			stmt.source = data_expr(
				p,
				body_start,
				[]string{"INTO", "FROM", "VALUES", "SET", "ACCEPTING", "CLIENT", "CONNECTION"},
			)
			continue
		}
		if allow_keyword(p, "SET") {
			start := previous_token(p)
			if dml_range_valid(stmt.set_clause) ||
			   dml_range_valid(stmt.from_clause) ||
			   stmt.values_clause {
				dml_reject_clause(
					p,
					start,
					"syntax error: duplicate or conflicting INSERT SET clause",
					body_start,
					[]string {
						"INTO",
						"FROM",
						"VALUES",
						"SET",
						"INDEX",
						"ASSIGNING",
						"REFERENCE",
						"ACCEPTING",
						"CLIENT",
						"CONNECTION",
					},
				)
				continue
			}
			stmt.form = .Db_Table
			if stmt.target == nil && stmt.source != nil {
				stmt.target = stmt.source
				insert_set_db_source_facts(stmt)
			}
			parse_sql_assignments(
				p,
				body_start,
				&stmt.assignments,
				[]string{"INTO", "FROM", "VALUES", "SET", "ACCEPTING", "CLIENT", "CONNECTION"},
			)
			stmt.set_clause = tokenizer.text_range(start.range.start, previous_token(p).range.end)
			continue
		}
		if allow_keyword(p, "INDEX") {
			stmt.index = data_expr(
				p,
				body_start,
				[]string{"ASSIGNING", "REFERENCE", "ACCEPTING", "CLIENT", "CONNECTION"},
			)
			continue
		}
		if allow_keyword(p, "ASSIGNING") {
			stmt.assigning = data_expr(
				p,
				body_start,
				[]string{"REFERENCE", "ACCEPTING", "CLIENT", "CONNECTION"},
			)
			continue
		}
		if allow_keyword(p, "REFERENCE") {
			allow_keyword(p, "INTO")
			stmt.reference_into = data_expr(
				p,
				body_start,
				[]string{"ACCEPTING", "CLIENT", "CONNECTION"},
			)
			continue
		}
		if allow_keyword(p, "ACCEPTING") {
			start := previous_token(p)
			if dml_range_valid(stmt.accepting_clause) {
				dml_reject_clause(
					p,
					start,
					"syntax error: duplicate INSERT ACCEPTING clause",
					body_start,
					[]string{"CLIENT", "CONNECTION"},
				)
				continue
			}
			if allow_keyword(p, "DUPLICATE") {
				stmt.accepting_duplicate_keys = allow_keyword(p, "KEYS")
			}
			stmt.accepting_clause = tokenizer.text_range(
				start.range.start,
				previous_token(p).range.end,
			)
			continue
		}
		if allow_keyword(p, "CLIENT") {
			start := previous_token(p)
			if dml_range_valid(stmt.client_clause) {
				dml_reject_clause(
					p,
					start,
					"syntax error: duplicate INSERT CLIENT clause",
					body_start,
					[]string{"CONNECTION"},
				)
			} else {
				stmt.client_clause = dml_skip_clause(p, start, body_start, []string{"CONNECTION"})
			}
			continue
		}
		if allow_keyword(p, "CONNECTION") {
			start := previous_token(p)
			if dml_range_valid(stmt.connection_clause) {
				dml_reject_clause(
					p,
					start,
					"syntax error: duplicate INSERT CONNECTION clause",
					body_start,
					[]string{"CLIENT"},
				)
			} else {
				stmt.connection_clause = dml_skip_clause(p, start, body_start, []string{"CLIENT"})
			}
			continue
		}
		bump_token(p)
	}
}

parse_append_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "APPEND")
	body_start := p.index
	stmt := ast.new(ast.Append_Stmt, start.range, p.allocator)
	if allow_keyword(p, "LINES") {
		allow_keyword(p, "OF")
		stmt.lines_of = true
	} else if allow_keyword(p, "INITIAL") {
		allow_keyword(p, "LINE")
		stmt.initial_line = true
	}
	if !stmt.initial_line {
		stmt.source = data_expr(p, body_start, []string{"TO", "ASSIGNING", "REFERENCE"})
	}
	for !data_stmt_done(p, body_start) {
		if allow_keyword(p, "TO") {
			stmt.sorted = allow_keyword(p, "SORTED")
			allow_keyword(p, "TABLE")
			stmt.target = data_expr(p, body_start, []string{"ASSIGNING", "REFERENCE"})
			continue
		}
		if allow_keyword(p, "ASSIGNING") {
			stmt.assigning = data_expr(p, body_start, []string{"REFERENCE"})
			continue
		}
		if allow_keyword(p, "REFERENCE") {
			allow_keyword(p, "INTO")
			stmt.reference_into = data_expr(p, body_start, []string{})
			continue
		}
		bump_token(p)
	}
	stmt.range = data_stmt_range(p, start)
	return stmt
}

read_line_stmt_starts :: proc(p: ^Parser) -> bool {
	return(
		at_keyword(p, "READ") &&
		(at_keyword_index(p, p.index + 1, "LINE") ||
				(at_keyword_index(p, p.index + 1, "CURRENT") &&
						at_keyword_index(p, p.index + 2, "LINE"))) \
	)
}

modify_line_stmt_starts :: proc(p: ^Parser) -> bool {
	return(
		at_keyword(p, "MODIFY") &&
		(at_keyword_index(p, p.index + 1, "LINE") ||
				(at_keyword_index(p, p.index + 1, "CURRENT") &&
						at_keyword_index(p, p.index + 2, "LINE"))) \
	)
}

parse_line_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := bump_token(p)
	body_start := p.index
	stmt := ast.new(ast.Line_Stmt, start.range, p.allocator)
	stmt.kind = .Read if token_is_keyword(p, start, "READ") else .Modify
	stmt.fields = make([dynamic]ast.Line_Field_Value_Clause, 0, 2, p.allocator)
	if allow_keyword(p, "CURRENT") {
		stmt.current = true
		allow_keyword(p, "LINE")
	} else {
		allow_keyword(p, "LINE")
		stmt.line = data_expr(p, body_start, []string{"INDEX", "INTO", "FIELD"})
	}
	for !data_stmt_done(p, body_start) {
		if allow_keyword(p, "INDEX") {
			stmt.index = data_expr(p, body_start, []string{"INTO", "FIELD"})
			continue
		}
		if allow_keyword(p, "INTO") {
			stmt.into = data_expr(p, body_start, []string{"FIELD"})
			continue
		}
		if allow_keyword(p, "FIELD") {
			allow_keyword(p, "VALUE")
			field := data_expr(p, body_start, []string{"INTO", "FIELD"})
			target: ^ast.Expr
			if allow_keyword(p, "INTO") {
				target = data_expr(p, body_start, []string{"FIELD"})
			}
			append(&stmt.fields, ast.Line_Field_Value_Clause{field = field, target = target})
			continue
		}
		bump_token(p)
	}
	stmt.range = data_stmt_range(p, start)
	return stmt
}

parse_generate_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "GENERATE")
	body_start := p.index
	stmt := ast.new(ast.Generate_Stmt, start.range, p.allocator)
	if allow_keyword(p, "SUBROUTINE") {
		allow_keyword(p, "POOL")
		stmt.kind = .Subroutine_Pool
		stmt.source = data_expr(
			p,
			body_start,
			[]string{"NAME", "MESSAGE", "LINE", "WORD", "OFFSET"},
		)
		for !data_stmt_done(p, body_start) {
			if allow_keyword(p, "NAME") {
				stmt.name = data_expr(p, body_start, []string{"MESSAGE", "LINE", "WORD", "OFFSET"})
				continue
			}
			if allow_keyword(p, "MESSAGE") {
				stmt.message = data_expr(p, body_start, []string{"LINE", "WORD", "OFFSET"})
				continue
			}
			if allow_keyword(p, "LINE") {
				stmt.line = data_expr(p, body_start, []string{"MESSAGE", "WORD", "OFFSET"})
				continue
			}
			if allow_keyword(p, "WORD") {
				stmt.word = data_expr(p, body_start, []string{"MESSAGE", "LINE", "OFFSET"})
				continue
			}
			if allow_keyword(p, "OFFSET") {
				stmt.offset = data_expr(p, body_start, []string{"MESSAGE", "LINE", "WORD"})
				continue
			}
			bump_token(p)
		}
	} else {
		allow_keyword(p, "DYNPRO")
		stmt.kind = .Dynpro
		stmt.program = data_expr(p, body_start, []string{})
		stmt.dynpro = data_expr(p, body_start, []string{})
		consume_data_tail(p, body_start)
	}
	stmt.range = data_stmt_range(p, start)
	return stmt
}

select_query_has_loop_body :: proc(query: ast.Select_Query_Clause) -> bool {
	if query.single {
		return false
	}
	if select_query_limits_to_one_row(query) {
		return false
	}
	if select_projection_list_is_aggregate(query.projections[:]) {
		return false
	}
	result := select_query_effective_result(query)
	return result == nil || !result.table
}

select_query_effective_result :: proc(query: ast.Select_Query_Clause) -> ^ast.Select_Result_Clause {
	if query.result != nil {
		return query.result
	}
	if len(query.set_ops) == 0 {
		return nil
	}
	return select_query_effective_result(query.set_ops[len(query.set_ops) - 1].query)
}

select_query_limits_to_one_row :: proc(query: ast.Select_Query_Clause) -> bool {
	if select_expr_is_one_literal(query.up_to_rows) {
		return true
	}
	if len(query.set_ops) == 0 {
		return false
	}
	return select_query_limits_to_one_row(query.set_ops[len(query.set_ops) - 1].query)
}

select_expr_is_one_literal :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	if literal, ok := expr.derived_expr.(^ast.Literal_Expr); ok {
		return literal.value == "1"
	}
	return false
}

select_projection_list_is_aggregate :: proc(projections: []^ast.Expr) -> bool {
	if len(projections) == 0 {
		return false
	}
	for projection in projections {
		if !select_projection_is_aggregate(projection) {
			return false
		}
	}
	return true
}

select_projection_is_aggregate :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Sql_Call_Expr:
		return n.kind == .Aggregate
	case ^ast.Call_Expr:
		if n.callee == nil {
			return false
		}
		if name, ok := n.callee.derived_expr.(^ast.Ident_Expr); ok {
			return select_aggregate_name(name.name)
		}
	case ^ast.Substring_Expr:
		if n.base != nil {
			if name, ok := n.base.derived_expr.(^ast.Ident_Expr); ok {
				return select_aggregate_name(name.name)
			}
		}
	}
	return false
}

select_aggregate_name :: proc(name: string) -> bool {
	return(
		strings.equal_fold(name, "COUNT") ||
		strings.equal_fold(name, "MAX") ||
		strings.equal_fold(name, "MIN") ||
		strings.equal_fold(name, "SUM") ||
		strings.equal_fold(name, "AVG") \
	)
}

MODIFY_TRANSPORTING_KEYWORD_MESSAGE :: "syntax error: expected TRANSPORTING in MODIFY statement"
MODIFY_UNEXPECTED_TOKEN_MESSAGE :: "syntax error: unexpected token in MODIFY statement"

parse_modify_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "MODIFY")
	body_start := p.index
	stmt := ast.new(ast.Modify_Stmt, start.range, p.allocator)
	stmt.transporting = make([dynamic]ast.Transporting_Field_Clause, 0, 2, p.allocator)
	stmt.table_keyword = allow_keyword(p, "TABLE")
	stmt.target = data_expr(
		p,
		body_start,
		[]string {
			"FROM",
			"INDEX",
			"TRANSPORTING",
			"WHERE",
			"ASSIGNING",
			"REFERENCE",
			"CLIENT",
			"CONNECTION",
		},
	)
	if stmt.target != nil {
		stmt.db_source_range = stmt.target.range
		stmt.dynamic_source = dml_dynamic_source(stmt.target)
	}
	for !data_stmt_done(p, body_start) {
		if allow_keyword(p, "FROM") {
			from_start := previous_token(p)
			if stmt.source != nil {
				dml_reject_clause(
					p,
					from_start,
					"syntax error: duplicate MODIFY FROM clause",
					body_start,
					[]string {
						"INDEX",
						"TRANSPORTING",
						"WHERE",
						"ASSIGNING",
						"REFERENCE",
						"CLIENT",
						"CONNECTION",
					},
				)
				continue
			}
			stmt.from_table = allow_keyword(p, "TABLE")
			stmt.source = data_expr(
				p,
				body_start,
				[]string {
					"INDEX",
					"TRANSPORTING",
					"WHERE",
					"ASSIGNING",
					"REFERENCE",
					"CLIENT",
					"CONNECTION",
				},
			)
			continue
		}
		if allow_keyword(p, "INDEX") {
			stmt.index = data_expr(
				p,
				body_start,
				[]string {
					"TRANSPORTING",
					"WHERE",
					"ASSIGNING",
					"REFERENCE",
					"CLIENT",
					"CONNECTION",
				},
			)
			continue
		}
		if allow_keyword(p, "TRANSPORTING") {
			parse_modify_transporting_field_list(p, body_start, &stmt.transporting)
			continue
		}
		if tok := current_token(p);
		   tok.kind == .Ident &&
		   strings.equal_fold(tokenizer.token_lexeme(tok, p.source), "TRANNSPORTING") {
			error_current(p, MODIFY_TRANSPORTING_KEYWORD_MESSAGE)
			bump_token(p)
			fields := make([dynamic]ast.Transporting_Field_Clause, 0, 2, context.temp_allocator)
			parse_modify_transporting_field_list(p, body_start, &fields)
			continue
		}
		if allow_keyword(p, "WHERE") {
			where_start := previous_token(p)
			if dml_range_valid(stmt.where_clause) {
				dml_reject_clause(
					p,
					where_start,
					"syntax error: duplicate MODIFY WHERE clause",
					body_start,
					[]string {
						"INDEX",
						"TRANSPORTING",
						"ASSIGNING",
						"REFERENCE",
						"CLIENT",
						"CONNECTION",
					},
				)
				continue
			}
			stmt.where_cond = sql_logical_expr(
				p,
				body_start,
				[]string {
					"INDEX",
					"TRANSPORTING",
					"ASSIGNING",
					"REFERENCE",
					"CLIENT",
					"CONNECTION",
				},
			)
			if stmt.where_cond != nil {
				stmt.dynamic_where = sql_dynamic_where_expr(stmt.where_cond)
				stmt.where_clause = tokenizer.text_range(
					where_start.range.start,
					previous_token(p).range.end,
				)
			}
			continue
		}
		if allow_keyword(p, "CLIENT") {
			client_start := previous_token(p)
			if dml_range_valid(stmt.client_clause) {
				dml_reject_clause(
					p,
					client_start,
					"syntax error: duplicate MODIFY CLIENT clause",
					body_start,
					[]string{"CONNECTION"},
				)
			} else {
				stmt.client_clause = dml_skip_clause(
					p,
					client_start,
					body_start,
					[]string{"CONNECTION"},
				)
			}
			continue
		}
		if allow_keyword(p, "CONNECTION") {
			connection_start := previous_token(p)
			if dml_range_valid(stmt.connection_clause) {
				dml_reject_clause(
					p,
					connection_start,
					"syntax error: duplicate MODIFY CONNECTION clause",
					body_start,
					[]string{"CLIENT"},
				)
			} else {
				stmt.connection_clause = dml_skip_clause(
					p,
					connection_start,
					body_start,
					[]string{"CLIENT"},
				)
			}
			continue
		}
		error_current(p, MODIFY_UNEXPECTED_TOKEN_MESSAGE)
		bump_token(p)
	}
	stmt.range = data_stmt_range(p, start)
	return stmt
}

parse_modify_transporting_field_list :: proc(
	p: ^Parser,
	body_start: int,
	fields: ^[dynamic]ast.Transporting_Field_Clause,
) {
	stop_keywords := []string{"WHERE", "ASSIGNING", "REFERENCE", "CLIENT", "CONNECTION"}
	for !data_stmt_done(p, body_start) && !data_current_keyword_in(p, stop_keywords) {
		if allow_token(p, .Comma) || allow_token(p, .Colon) {
			continue
		}
		start := p.index
		if field, ok := parse_transporting_field(p); ok {
			append(fields, field)
		} else {
			error_current(p, "syntax error: expected MODIFY TRANSPORTING component path")
			bump_token(p)
		}
		ensure_forward_progress(p, start)
	}
}

parse_transporting_field :: proc(
	p: ^Parser,
) -> (
	ast.Transporting_Field_Clause,
	bool,
) {
	if !transporting_segment_token(current_token(p)) {
		return {}, false
	}
	start := current_token(p).range.start
	path := make([dynamic]ast.Transporting_Field_Segment, 0, 2, p.allocator)
	for {
		tok := current_token(p)
		if !transporting_segment_token(tok) {
			break
		}
		append(
			&path,
			ast.Transporting_Field_Segment {
				name = parser_ast_raw_name_token(p, tok),
			},
		)
		bump_token(p)
		if p.index + 1 >= len(p.tokens) {
			break
		}
		dash := current_token(p)
		next := p.tokens[p.index + 1]
		if dash.kind != .Minus ||
		   !tokens_touch(tok, dash) ||
		   !transporting_segment_token(next) ||
		   !tokens_touch(dash, next) {
			break
		}
		bump_token(p)
	}
	end := path[len(path) - 1].name.range.end
	return ast.Transporting_Field_Clause {
			name = parser_ast_token(parser_clone_range_text(p, tokenizer.text_range(start, end)), tokenizer.text_range(start, end)),
			path = path,
		},
		true
}

transporting_segment_token :: #force_inline proc "contextless" (tok: Token) -> bool {
	return tok.kind == .Ident || tok.kind == .Number
}

parse_sort_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "SORT")
	body_start := p.index
	stmt := ast.new(ast.Sort_Stmt, start.range, p.allocator)
	stmt.fields = make([dynamic]ast.Sort_Field_Clause, 0, 2, p.allocator)
	stmt.stable = allow_keyword(p, "STABLE")
	stmt.target = data_expr(p, body_start, []string{"STABLE", "BY", "AS", "ASCENDING", "DESCENDING"})
	for !data_stmt_done(p, body_start) {
		if allow_keyword(p, "STABLE") {
			stmt.stable = true
			continue
		}
		if allow_keyword(p, "AS") {
			stmt.as_text = allow_keyword(p, "TEXT")
			continue
		}
		if allow_keyword(p, "BY") {
			parse_sort_by_fields(p, body_start, stmt)
			continue
		}
		if allow_keyword(p, "DESCENDING") {
			stmt.descending = true
			continue
		}
		if allow_keyword(p, "ASCENDING") {
			continue
		}
		bump_token(p)
	}
	stmt.range = data_stmt_range(p, start)
	return stmt
}

parse_sort_by_fields :: proc(p: ^Parser, body_start: int, stmt: ^ast.Sort_Stmt) {
	for !data_stmt_done(p, body_start) {
		if allow_token(p, .Comma) || allow_token(p, .Colon) {
			continue
		}
		if allow_keyword(p, "ASCENDING") {
			if len(stmt.fields) > 0 {
				stmt.fields[len(stmt.fields) - 1].ascending = true
				stmt.fields[len(stmt.fields) - 1].descending = false
			}
			continue
		}
		if allow_keyword(p, "DESCENDING") {
			if len(stmt.fields) > 0 {
				stmt.fields[len(stmt.fields) - 1].ascending = false
				stmt.fields[len(stmt.fields) - 1].descending = true
			}
			continue
		}
		if allow_keyword(p, "AS") {
			text := allow_keyword(p, "TEXT")
			if text && len(stmt.fields) > 0 {
				stmt.fields[len(stmt.fields) - 1].as_text = true
			}
			continue
		}
		start := p.index
		value := data_expr(p, body_start, []string{"AS", "ASCENDING", "DESCENDING"})
		if value != nil {
			append(&stmt.fields, sort_field_clause(p, value))
		} else {
			bump_token(p)
		}
		ensure_forward_progress(p, start)
	}
}

sort_field_clause :: proc(p: ^Parser, expr: ^ast.Expr) -> ast.Sort_Field_Clause {
	clause := ast.Sort_Field_Clause {
		expr = expr,
	}
	if sort_field_name_expr(expr) {
		clause.name = parser_ast_token(parser_clone_range_text(p, expr.range), expr.range)
	}
	return clause
}

sort_field_name_expr :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	if _, _, ok := sort_field_segment_name(expr); ok {
		return true
	}
	sel, ok := expr.derived_expr.(^ast.Selector_Expr)
	return(
		ok &&
		sel.op == .Dash &&
		sort_field_name_expr(sel.base) &&
		sort_field_name_expr(sel.field) \
	)
}

sort_field_segment_name :: proc(expr: ^ast.Expr) -> (string, tokenizer.Range, bool) {
	if expr == nil {
		return "", tokenizer.Range{}, false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		return n.name, n.range, n.name != ""
	case ^ast.Literal_Expr:
		return n.value, n.range, n.value != ""
	}
	return "", tokenizer.Range{}, false
}

parse_sql_assignments :: proc(
	p: ^Parser,
	body_start: int,
	list: ^[dynamic]ast.Sql_Assignment_Clause,
	stop_keywords: []string,
) {
	for !sql_assignment_list_done(p, body_start, stop_keywords) {
		if allow_token(p, .Comma) {
			continue
		}
		name := sql_data_expr(p, p.index, stop_keywords)
		if name == nil {
			bump_token(p)
			continue
		}
		if !allow_token(p, .Eq) {
			continue
		}
		value := sql_data_expr(p, p.index, stop_keywords)
		if value == nil {
			error_current(p, "syntax error: expected expression")
			continue
		}
		column_name, column_range := sql_assignment_column_fact(name)
		append(
			list,
			ast.Sql_Assignment_Clause {
				name = name,
				value = value,
				column_name = parser_ast_token(column_name, column_range),
			},
		)
	}
}

sql_assignment_list_done :: proc(p: ^Parser, body_start: int, stop_keywords: []string) -> bool {
	tok := current_token(p)
	if tok.kind == .Period || tok.kind == .Eof || data_current_keyword_in(p, stop_keywords) {
		return true
	}
	if p.index > body_start &&
	   .Has_Newline_Before in tok.flags &&
	   known_stmt_lead_at(p, p.index) &&
	   !line_continuation_starts(p, p.index) {
		return !assignment_starts(p, p.index)
	}
	return false
}

parse_update_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "UPDATE")
	body_start := p.index
	stmt := ast.new(ast.Update_Stmt, start.range, p.allocator)
	stmt.assignments = make([dynamic]ast.Sql_Assignment_Clause, 0, 2, p.allocator)
	stmt.target = sql_data_expr(
		p,
		body_start,
		[]string{"FROM", "SET", "WHERE", "USING", "CLIENT", "CONNECTION"},
	)
	if stmt.target != nil {
		stmt.db_source_range = stmt.target.range
		stmt.dynamic_source = dml_dynamic_source(stmt.target)
	}
	for !data_stmt_done(p, body_start) {
		if allow_keyword(p, "FROM") {
			from_start := previous_token(p)
			if stmt.source != nil ||
			   len(stmt.assignments) > 0 ||
			   dml_range_valid(stmt.set_clause) {
				dml_reject_clause(
					p,
					from_start,
					"syntax error: duplicate or conflicting UPDATE FROM clause",
					body_start,
					[]string{"FROM", "SET", "WHERE", "USING", "CLIENT", "CONNECTION"},
				)
				continue
			}
			stmt.from_table = allow_keyword(p, "TABLE")
			stmt.source = sql_data_expr(
				p,
				body_start,
				[]string{"FROM", "SET", "WHERE", "USING", "CLIENT", "CONNECTION"},
			)
			continue
		}
		if allow_keyword(p, "SET") {
			set_start := previous_token(p)
			if dml_range_valid(stmt.set_clause) || stmt.source != nil {
				dml_reject_clause(
					p,
					set_start,
					"syntax error: duplicate or conflicting UPDATE SET clause",
					body_start,
					[]string{"FROM", "SET", "WHERE", "USING", "CLIENT", "CONNECTION"},
				)
				continue
			}
			before := len(stmt.assignments)
			parse_sql_assignments(
				p,
				body_start,
				&stmt.assignments,
				[]string{"FROM", "SET", "WHERE", "USING", "CLIENT", "CONNECTION"},
			)
			if len(stmt.assignments) == before {
				error(p, set_start.range, "syntax error: expected SQL assignment")
			} else {
				stmt.set_clause = tokenizer.text_range(
					set_start.range.start,
					previous_token(p).range.end,
				)
			}
			continue
		}
		if allow_keyword(p, "WHERE") {
			where_start := previous_token(p)
			if dml_range_valid(stmt.where_clause) {
				dml_reject_clause(
					p,
					where_start,
					"syntax error: duplicate UPDATE WHERE clause",
					body_start,
					[]string{"FROM", "SET", "USING", "CLIENT", "CONNECTION"},
				)
				continue
			}
			stmt.where_cond = sql_logical_expr(
				p,
				body_start,
				[]string{"FROM", "SET", "USING", "CLIENT", "CONNECTION"},
			)
			if stmt.where_cond != nil {
				stmt.dynamic_where = sql_dynamic_where_expr(stmt.where_cond)
				stmt.where_clause = tokenizer.text_range(
					where_start.range.start,
					previous_token(p).range.end,
				)
			}
			continue
		}
		if allow_keyword(p, "CLIENT") {
			client_start := previous_token(p)
			if dml_range_valid(stmt.client_clause) {
				dml_reject_clause(
					p,
					client_start,
					"syntax error: duplicate UPDATE CLIENT clause",
					body_start,
					[]string{"CONNECTION"},
				)
			} else {
				stmt.client_clause = dml_skip_clause(
					p,
					client_start,
					body_start,
					[]string{"CONNECTION"},
				)
			}
			continue
		}
		if allow_keyword(p, "CONNECTION") {
			connection_start := previous_token(p)
			if dml_range_valid(stmt.connection_clause) {
				dml_reject_clause(
					p,
					connection_start,
					"syntax error: duplicate UPDATE CONNECTION clause",
					body_start,
					[]string{"CLIENT"},
				)
			} else {
				stmt.connection_clause = dml_skip_clause(
					p,
					connection_start,
					body_start,
					[]string{"CLIENT"},
				)
			}
			continue
		}
		if allow_keyword(p, "USING") {
			using_start := previous_token(p)
			if allow_keyword(p, "CLIENT") && !dml_range_valid(stmt.client_clause) {
				stmt.client_clause = dml_skip_clause(
					p,
					using_start,
					body_start,
					[]string{"CONNECTION"},
				)
			} else {
				dml_reject_clause(
					p,
					using_start,
					"syntax error: invalid UPDATE USING clause",
					body_start,
					[]string{"FROM", "SET", "WHERE", "CLIENT", "CONNECTION"},
				)
			}
			continue
		}
		bump_token(p)
	}
	stmt.range = data_stmt_range(p, start)
	return stmt
}

parse_delete_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "DELETE")
	body_start := p.index
	stmt := ast.new(ast.Delete_Stmt, start.range, p.allocator)
	stmt.comparing = make([dynamic]ast.Delete_Comparing_Clause, 0, 2, p.allocator)
	if allow_keyword(p, "ADJACENT") {
		stmt.form = .Adjacent_Duplicates
		allow_keyword(p, "DUPLICATES")
		allow_keyword(p, "FROM")
		stmt.target = data_expr(p, body_start, []string{"COMPARING"})
	} else if allow_keyword(p, "FROM") {
		stmt.form = .Db_Table
		stmt.explicit_from = true
		stmt.target = sql_data_expr(p, body_start, []string{"WHERE", "CLIENT", "CONNECTION"})
		if stmt.target != nil {
			stmt.db_source_range = stmt.target.range
			stmt.dynamic_source = dml_dynamic_source(stmt.target)
		}
	} else {
		stmt.form = .Internal_Table
		allow_keyword(p, "TABLE")
		stmt.target = data_expr(
			p,
			body_start,
			[]string{"FROM", "WHERE", "INDEX", "USING", "COMPARING"},
		)
	}
	for !data_stmt_done(p, body_start) {
		if allow_keyword(p, "FROM") {
			from_start := previous_token(p)
			if stmt.form == .Db_Table || stmt.source != nil {
				dml_reject_clause(
					p,
					from_start,
					"syntax error: duplicate or conflicting DELETE FROM clause",
					body_start,
					[]string{"WHERE", "INDEX", "USING", "COMPARING", "CLIENT", "CONNECTION"},
				)
				continue
			}
			stmt.from_table = allow_keyword(p, "TABLE")
			if stmt.from_table && stmt.form == .Internal_Table {
				stmt.form = .Db_Table
				if stmt.target != nil {
					stmt.db_source_range = stmt.target.range
					stmt.dynamic_source = dml_dynamic_source(stmt.target)
				}
			}
			stmt.source = data_expr(
				p,
				body_start,
				[]string{"WHERE", "INDEX", "USING", "COMPARING", "CLIENT", "CONNECTION"},
			)
			continue
		}
		if allow_keyword(p, "WHERE") {
			where_start := previous_token(p)
			if dml_range_valid(stmt.where_clause) {
				dml_reject_clause(
					p,
					where_start,
					"syntax error: duplicate DELETE WHERE clause",
					body_start,
					[]string{"INDEX", "USING", "COMPARING", "CLIENT", "CONNECTION"},
				)
				continue
			}
			stmt.where_cond = sql_logical_expr(
				p,
				body_start,
				[]string{"INDEX", "USING", "COMPARING", "CLIENT", "CONNECTION"},
			)
			if stmt.where_cond != nil {
				stmt.dynamic_where = sql_dynamic_where_expr(stmt.where_cond)
				stmt.where_clause = tokenizer.text_range(
					where_start.range.start,
					previous_token(p).range.end,
				)
			}
			continue
		}
		if allow_keyword(p, "INDEX") {
			stmt.index = data_expr(p, body_start, []string{"USING", "COMPARING"})
			continue
		}
		if allow_keyword(p, "USING") {
			stmt.using_key = parse_table_key_selector(
				p,
				body_start,
				[]string{"WHERE", "COMPARING"},
			)
			continue
		}
		if allow_keyword(p, "COMPARING") {
			if at_keyword(p, "ALL") && at_keyword_index(p, p.index + 1, "FIELDS") {
				_ = bump_token(p)
				_ = bump_token(p)
				append(
					&stmt.comparing,
					ast.Delete_Comparing_Clause {
						all_fields = true,
					},
				)
				continue
			}
			more := data_exprs_until(p, body_start, []string{})
			for value in more {append(&stmt.comparing, delete_comparing_clause(value))}
			continue
		}
		if allow_keyword(p, "CLIENT") {
			client_start := previous_token(p)
			if dml_range_valid(stmt.client_clause) {
				dml_reject_clause(
					p,
					client_start,
					"syntax error: duplicate DELETE CLIENT clause",
					body_start,
					[]string{"CONNECTION"},
				)
			} else {
				stmt.client_clause = dml_skip_clause(
					p,
					client_start,
					body_start,
					[]string{"CONNECTION"},
				)
			}
			continue
		}
		if allow_keyword(p, "CONNECTION") {
			connection_start := previous_token(p)
			if dml_range_valid(stmt.connection_clause) {
				dml_reject_clause(
					p,
					connection_start,
					"syntax error: duplicate DELETE CONNECTION clause",
					body_start,
					[]string{"CLIENT"},
				)
			} else {
				stmt.connection_clause = dml_skip_clause(
					p,
					connection_start,
					body_start,
					[]string{"CLIENT"},
				)
			}
			continue
		}
		bump_token(p)
	}
	stmt.range = data_stmt_range(p, start)
	return stmt
}

delete_comparing_clause :: proc(expr: ^ast.Expr) -> ast.Delete_Comparing_Clause {
	clause := ast.Delete_Comparing_Clause {
		expr = expr,
	}
	if id, ok := expr.derived_expr.(^ast.Ident_Expr); ok {
		clause.name = parser_ast_token(id.name, id.range)
	}
	return clause
}

DATASET_OPEN_CLAUSE_KEYWORDS :: []string {
	"FOR",
	"IN",
	"ENCODING",
	"AT",
	"TYPE",
	"FILTER",
	"MESSAGE",
	"IGNORING",
	"REPLACEMENT",
	"WITH",
	"SKIPPING",
	"CODE",
	"BIG",
	"LITTLE",
}

DATASET_POSITION_CLAUSE_KEYWORDS :: []string {
	"ATTRIBUTES",
	"MESSAGE",
	"TYPE",
	"FILTER",
	"IGNORING",
	"REPLACEMENT",
	"WITH",
	"SKIPPING",
	"CODE",
	"BIG",
	"LITTLE",
}

DATASET_READ_CLAUSE_KEYWORDS :: []string {
	"INTO",
	"MAXIMUM",
	"ACTUAL",
	"LENGTH",
}

DATASET_TRANSFER_CLAUSE_KEYWORDS :: []string {
	"LENGTH",
	"NO",
}

parse_dataset_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := current_token(p)
	body_start := p.index
	stmt := ast.new(ast.Dataset_Stmt, start.range, p.allocator)
	if allow_keyword(p, "TRANSFER") {
		stmt.kind = .Transfer
		stmt.source = dataset_required_expr(p, body_start, []string{"TO"}, "syntax error: expected source after TRANSFER")
		if dataset_expect_keyword(p, "TO", "syntax error: expected TO after TRANSFER source") {
			stmt.dataset = dataset_required_expr(
				p,
				body_start,
				DATASET_TRANSFER_CLAUSE_KEYWORDS,
				"syntax error: expected dataset name after TRANSFER TO",
			)
		}
	} else if allow_keyword(p, "OPEN") {
		stmt.kind = .Open
		dataset_expect_keyword(p, "DATASET", "syntax error: expected DATASET after OPEN")
		stmt.dataset = dataset_required_expr(
			p,
			body_start,
			DATASET_OPEN_CLAUSE_KEYWORDS,
			"syntax error: expected dataset name after OPEN DATASET",
		)
	} else if allow_keyword(p, "READ") {
		stmt.kind = .Read
		dataset_expect_keyword(p, "DATASET", "syntax error: expected DATASET after READ")
		stmt.dataset = dataset_required_expr(
			p,
			body_start,
			DATASET_READ_CLAUSE_KEYWORDS,
			"syntax error: expected dataset name after READ DATASET",
		)
	} else if allow_keyword(p, "CLOSE") {
		stmt.kind = .Close
		dataset_expect_keyword(p, "DATASET", "syntax error: expected DATASET after CLOSE")
		stmt.dataset = dataset_required_expr(p, body_start, []string{}, "syntax error: expected dataset name after CLOSE DATASET")
	} else if allow_keyword(p, "DELETE") {
		stmt.kind = .Delete
		dataset_expect_keyword(p, "DATASET", "syntax error: expected DATASET after DELETE")
		stmt.dataset = dataset_required_expr(p, body_start, []string{}, "syntax error: expected dataset name after DELETE DATASET")
	} else if allow_keyword(p, "GET") {
		stmt.kind = .Get
		dataset_expect_keyword(p, "DATASET", "syntax error: expected DATASET after GET")
		stmt.dataset = dataset_required_expr(
			p,
			body_start,
			[]string{"POSITION", "ATTRIBUTES"},
			"syntax error: expected dataset name after GET DATASET",
		)
	} else if allow_keyword(p, "SET") {
		stmt.kind = .Set
		dataset_expect_keyword(p, "DATASET", "syntax error: expected DATASET after SET")
		stmt.dataset = dataset_required_expr(
			p,
			body_start,
			[]string{"POSITION", "ATTRIBUTES"},
			"syntax error: expected dataset name after SET DATASET",
		)
	} else {
		stmt.kind = .Truncate
		allow_keyword(p, "TRUNCATE")
		dataset_expect_keyword(p, "DATASET", "syntax error: expected DATASET after TRUNCATE")
		stmt.dataset = dataset_required_expr(p, body_start, []string{"AT"}, "syntax error: expected dataset name after TRUNCATE DATASET")
	}
	parse_dataset_tail(p, body_start, stmt)
	stmt.range = data_stmt_range(p, start)
	return stmt
}

parse_dataset_tail :: proc(p: ^Parser, body_start: int, stmt: ^ast.Dataset_Stmt) {
	for !dataset_stmt_done(p, body_start) {
		if allow_keyword(p, "FOR") {
			parse_dataset_access(p, stmt)
			continue
		}
		if allow_keyword(p, "IN") {
			parse_dataset_mode(p, stmt)
			continue
		}
		if allow_keyword(p, "ENCODING") {
			stmt.encoding = parse_dataset_token_value(p, "syntax error: expected value after OPEN DATASET ENCODING")
			continue
		}
		if allow_keyword(p, "CODE") {
			if dataset_expect_keyword(p, "PAGE", "syntax error: expected PAGE after OPEN DATASET CODE") {
				stmt.code_page = dataset_required_expr(
					p,
					body_start,
					DATASET_OPEN_CLAUSE_KEYWORDS,
					"syntax error: expected expression after OPEN DATASET CODE PAGE",
				)
			}
			continue
		}
		if allow_keyword(p, "BIG") {
			if dataset_expect_keyword(p, "ENDIAN", "syntax error: expected ENDIAN after OPEN DATASET BIG") {
				stmt.endian = .Big
			}
			continue
		}
		if allow_keyword(p, "LITTLE") {
			if dataset_expect_keyword(p, "ENDIAN", "syntax error: expected ENDIAN after OPEN DATASET LITTLE") {
				stmt.endian = .Little
			}
			continue
		}
		if allow_keyword(p, "INTO") {
			stmt.target = dataset_required_expr(
				p,
				body_start,
				DATASET_READ_CLAUSE_KEYWORDS,
				"syntax error: expected target after READ DATASET INTO",
			)
			continue
		}
		if allow_keyword(p, "MAXIMUM") {
			if dataset_expect_keyword(p, "LENGTH", "syntax error: expected LENGTH after READ DATASET MAXIMUM") {
				stmt.maximum_length = dataset_required_expr(
					p,
					body_start,
					[]string{"ACTUAL", "LENGTH"},
					"syntax error: expected expression after READ DATASET MAXIMUM LENGTH",
				)
			}
			continue
		}
		if allow_keyword(p, "ACTUAL") {
			if dataset_expect_keyword(p, "LENGTH", "syntax error: expected LENGTH after READ DATASET ACTUAL") {
				stmt.actual_length = dataset_required_expr(
					p,
					body_start,
					[]string{"LENGTH"},
					"syntax error: expected target after READ DATASET ACTUAL LENGTH",
				)
			}
			continue
		}
		if allow_keyword(p, "LENGTH") {
			stmt.length = dataset_required_expr(
				p,
				body_start,
				[]string{"NO"},
				"syntax error: expected target after READ DATASET LENGTH",
			)
			continue
		}
		if allow_keyword(p, "NO") {
			if stmt.kind == .Transfer &&
			   dataset_expect_keyword(p, "END", "syntax error: expected END after TRANSFER NO") &&
			   dataset_expect_keyword(p, "OF", "syntax error: expected OF after TRANSFER NO END") &&
			   dataset_expect_keyword(p, "LINE", "syntax error: expected LINE after TRANSFER NO END OF") {
				stmt.flags += {.No_End_Of_Line}
			}
			continue
		}
		if allow_keyword(p, "AT") {
			if allow_keyword(p, "CURRENT") {
				if dataset_expect_keyword(p, "POSITION", "syntax error: expected POSITION after OPEN DATASET AT CURRENT") {
					stmt.flags += {.At_Current_Position}
				}
			} else {
				if dataset_expect_keyword(p, "POSITION", "syntax error: expected POSITION after OPEN DATASET AT") {
					stmt.position = dataset_required_expr(
						p,
						body_start,
						DATASET_POSITION_CLAUSE_KEYWORDS,
						"syntax error: expected expression after OPEN DATASET AT POSITION",
					)
				}
			}
			continue
		}
		if allow_keyword(p, "TYPE") {
			stmt.file_type = dataset_required_expr(
				p,
				body_start,
				DATASET_OPEN_CLAUSE_KEYWORDS,
				"syntax error: expected expression after OPEN DATASET TYPE",
			)
			continue
		}
		if allow_keyword(p, "FILTER") {
			stmt.filter = dataset_required_expr(
				p,
				body_start,
				DATASET_OPEN_CLAUSE_KEYWORDS,
				"syntax error: expected expression after OPEN DATASET FILTER",
			)
			continue
		}
		if allow_keyword(p, "POSITION") {
			if allow_keyword(p, "END") {
				if dataset_expect_keyword(p, "OF", "syntax error: expected OF after DATASET POSITION END") {
					if dataset_expect_keyword(p, "FILE", "syntax error: expected FILE after DATASET POSITION END OF") {
						if stmt.kind == .Set {
							stmt.flags += {.Position_End_Of_File}
						}
					}
				}
			} else {
				stmt.position = dataset_required_expr(
					p,
					body_start,
					DATASET_POSITION_CLAUSE_KEYWORDS,
					"syntax error: expected expression after DATASET POSITION",
				)
			}
			continue
		}
		if allow_keyword(p, "ATTRIBUTES") {
			stmt.attributes = dataset_required_expr(
				p,
				body_start,
				[]string{"POSITION"},
				"syntax error: expected expression after DATASET ATTRIBUTES",
			)
			continue
		}
		if allow_keyword(p, "MESSAGE") {
			stmt.message = dataset_required_expr(
				p,
				body_start,
				DATASET_OPEN_CLAUSE_KEYWORDS,
				"syntax error: expected target after OPEN DATASET MESSAGE",
			)
			continue
		}
		if allow_keyword(p, "IGNORING") {
			if dataset_expect_keyword(p, "CONVERSION", "syntax error: expected CONVERSION after OPEN DATASET IGNORING") &&
			   dataset_expect_keyword(p, "ERRORS", "syntax error: expected ERRORS after OPEN DATASET IGNORING CONVERSION") {
				stmt.flags += {.Ignoring_Conversion_Errors}
			}
			continue
		}
		if allow_keyword(p, "REPLACEMENT") {
			if dataset_expect_keyword(p, "CHARACTER", "syntax error: expected CHARACTER after OPEN DATASET REPLACEMENT") {
				stmt.replacement = dataset_required_expr(
					p,
					body_start,
					DATASET_OPEN_CLAUSE_KEYWORDS,
					"syntax error: expected expression after OPEN DATASET REPLACEMENT CHARACTER",
				)
			}
			continue
		}
		if allow_keyword(p, "WITH") {
			if matched, complete := parse_dataset_byte_order_mark(p, "syntax error: expected MARK after OPEN DATASET WITH BYTE-ORDER"); matched {
				if complete {
					stmt.byte_order_mark = .With
				}
			} else if mode, ok := parse_dataset_linefeed_mode(p); ok {
				if mode != .Default {
					stmt.linefeed_mode = mode
				}
			} else {
				error_current(p, "syntax error: expected BYTE-ORDER MARK or linefeed mode after OPEN DATASET WITH")
			}
			continue
		}
		if allow_keyword(p, "SKIPPING") {
			if matched, complete := parse_dataset_byte_order_mark(p, "syntax error: expected MARK after OPEN DATASET SKIPPING BYTE-ORDER"); matched {
				if complete {
					stmt.byte_order_mark = .Skipping
				}
			} else {
				error_current(p, "syntax error: expected BYTE-ORDER MARK after OPEN DATASET SKIPPING")
			}
			continue
		}
		bump_token(p)
	}
}

dataset_required_expr :: proc(
	p: ^Parser,
	body_start: int,
	stop_keywords: []string,
	message: string,
) -> ^ast.Expr {
	expr := data_expr(p, body_start, stop_keywords)
	if expr == nil {
		error_current(p, message)
	}
	return expr
}

dataset_expect_keyword :: proc(p: ^Parser, keyword: string, message: string) -> bool {
	if allow_keyword(p, keyword) {
		return true
	}
	error_current(p, message)
	return false
}

parse_dataset_access :: proc(p: ^Parser, stmt: ^ast.Dataset_Stmt) {
	if allow_keyword(p, "INPUT") {
		stmt.access = .Input
		return
	}
	if allow_keyword(p, "OUTPUT") {
		stmt.access = .Output
		return
	}
	if allow_keyword(p, "APPENDING") {
		stmt.access = .Append
		return
	}
	if allow_keyword(p, "UPDATE") {
		stmt.access = .Update
		return
	}
	error_current(p, "syntax error: expected INPUT, OUTPUT, APPENDING, or UPDATE after OPEN DATASET FOR")
}

parse_dataset_mode :: proc(p: ^Parser, stmt: ^ast.Dataset_Stmt) {
	legacy := allow_keyword(p, "LEGACY")
	if allow_keyword(p, "TEXT") {
		if dataset_expect_keyword(p, "MODE", "syntax error: expected MODE after OPEN DATASET IN TEXT") {
			if legacy {
				stmt.flags += {.Legacy_Mode}
			}
			stmt.flags += {.Text_Mode}
		}
		return
	}
	if allow_keyword(p, "BINARY") {
		if dataset_expect_keyword(p, "MODE", "syntax error: expected MODE after OPEN DATASET IN BINARY") {
			if legacy {
				stmt.flags += {.Legacy_Mode}
			}
			stmt.flags += {.Binary_Mode}
		}
		return
	}
	if legacy {
		error_current(p, "syntax error: expected TEXT or BINARY after OPEN DATASET IN LEGACY")
	} else {
		error_current(p, "syntax error: expected TEXT, BINARY, or LEGACY after OPEN DATASET IN")
	}
}

parse_dataset_byte_order_mark :: proc(p: ^Parser, missing_mark_message: string) -> (matched: bool, complete: bool) {
	if !allow_hyphen2(p, "BYTE", "ORDER") {
		return false, false
	}
	if !dataset_expect_keyword(p, "MARK", missing_mark_message) {
		return true, false
	}
	return true, true
}

parse_dataset_token_value :: proc(p: ^Parser, message: string) -> string {
	start := current_token(p)
	if start.kind != .Ident && start.kind != .String && start.kind != .Number {
		error_current(p, message)
		return ""
	}
	bump_token(p)
	if current_token(p).kind == .Minus &&
	   tokens_touch(previous_token(p), current_token(p)) &&
	   (next_token_kind(p, 1) == .Ident || next_token_kind(p, 1) == .Number) &&
	   tokens_touch(current_token(p), p.tokens[p.index + 1]) {
		bump_token(p)
		bump_token(p)
	}
	return parser_clone_range_text(p, tokenizer.text_range(start.range.start, previous_token(p).range.end))
}

parse_dataset_linefeed_mode :: proc(p: ^Parser) -> (ast.Dataset_Linefeed_Mode, bool) {
	mode := ast.Dataset_Linefeed_Mode.Default
	if allow_keyword(p, "NATIVE") {
		mode = .Native
	} else if allow_keyword(p, "UNIX") {
		mode = .Unix
	} else if allow_keyword(p, "WINDOWS") {
		mode = .Windows
	} else if allow_keyword(p, "SMART") {
		mode = .Smart
	} else {
		return .Default, false
	}
	if !dataset_expect_keyword(p, "LINEFEED", "syntax error: expected LINEFEED after OPEN DATASET WITH linefeed mode") {
		return .Default, true
	}
	return mode, true
}

dataset_stmt_done :: proc(p: ^Parser, body_start: int) -> bool {
	tok := current_token(p)
	return(
		tok.kind == .Period ||
		tok.kind == .Eof ||
		(p.index > body_start &&
				.Has_Newline_Before in tok.flags &&
				known_stmt_lead_at(p, p.index) &&
				!line_continuation_starts(p, p.index) &&
				!dataset_clause_starts(p)) \
	)
}

dataset_clause_starts :: proc(p: ^Parser) -> bool {
	return(
		at_keyword(p, "FOR") ||
		at_keyword(p, "IN") ||
		at_keyword(p, "ENCODING") ||
		at_keyword(p, "CODE") ||
		at_keyword(p, "BIG") ||
		at_keyword(p, "LITTLE") ||
		at_keyword(p, "INTO") ||
		at_keyword(p, "MAXIMUM") ||
		at_keyword(p, "ACTUAL") ||
		at_keyword(p, "LENGTH") ||
		at_keyword(p, "NO") ||
		at_keyword(p, "AT") ||
		at_keyword(p, "TYPE") ||
		at_keyword(p, "FILTER") ||
		at_keyword(p, "POSITION") ||
		at_keyword(p, "ATTRIBUTES") ||
		at_keyword(p, "MESSAGE") ||
		at_keyword(p, "IGNORING") ||
		at_keyword(p, "REPLACEMENT") ||
		at_keyword(p, "WITH") ||
		at_keyword(p, "SKIPPING") \
	)
}

parse_report_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := current_token(p)
	body_start := p.index
	stmt := ast.new(ast.Report_Stmt, start.range, p.allocator)
	if allow_keyword(p, "REPORT") {
		stmt.kind = .Report
		stmt.name = data_expr(
			p,
			body_start,
			[]string{"MESSAGE-ID", "LINE-SIZE", "LINE", "LINE-COUNT"},
		)
	} else if allow_keyword(p, "PROGRAM") {
		stmt.kind = .Program
		stmt.name = data_expr(
			p,
			body_start,
			[]string{"MESSAGE-ID", "LINE-SIZE", "LINE", "LINE-COUNT"},
		)
	} else if allow_keyword(p, "READ") {
		stmt.kind = .Read_Report
		allow_keyword(p, "REPORT")
		stmt.name = data_expr(p, body_start, []string{"INTO"})
	} else if allow_keyword(p, "INSERT") {
		stmt.kind = .Insert_Report
		allow_keyword(p, "REPORT")
		stmt.name = data_expr(p, body_start, []string{"FROM"})
	} else {
		allow_keyword(p, "DELETE")
		stmt.kind = .Delete_Report
		allow_keyword(p, "REPORT")
		stmt.name = data_expr(p, body_start, []string{})
	}
	for !data_stmt_done(p, body_start) {
		if (stmt.kind == .Report || stmt.kind == .Program) &&
		   allow_keyword_phrase(p, "MESSAGE-ID") {
			tok := current_token(p)
			if tok.kind == .Ident || tok.kind == .Number || tok.kind == .String {
				bump_token(p)
				stmt.has_message_id = true
				stmt.message_id = parser_ast_token(
					parser_clone_token_text(p, tok) if tok.kind == .String else parser_intern_token_name(p, tok),
					tok.range,
				)
			}
			continue
		}
		if allow_keyword(p, "INTO") || allow_keyword(p, "FROM") {
			stmt.source = data_expr(p, body_start, []string{"LINE-SIZE", "LINE", "LINE-COUNT"})
			continue
		}
		if allow_hyphen2(p, "LINE", "SIZE") || allow_keyword_phrase(p, "LINE-SIZE") {
			stmt.line_size = data_expr(p, body_start, []string{"LINE-COUNT", "LINE"})
			continue
		}
		if allow_hyphen2(p, "LINE", "COUNT") || allow_keyword_phrase(p, "LINE-COUNT") {
			stmt.line_count = data_expr(p, body_start, []string{"LINE-SIZE", "LINE"})
			continue
		}
		bump_token(p)
	}
	stmt.range = data_stmt_range(p, start)
	return stmt
}

parse_textpool_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := current_token(p)
	body_start := p.index
	stmt := ast.new(ast.Textpool_Stmt, start.range, p.allocator)
	if allow_keyword(p, "READ") {
		stmt.kind = .Read
	} else if allow_keyword(p, "INSERT") {
		stmt.kind = .Insert
	} else {
		allow_keyword(p, "DELETE")
		stmt.kind = .Delete
	}
	allow_keyword(p, "TEXTPOOL")
	stmt.program = data_expr(p, body_start, []string{"INTO", "FROM", "LANGUAGE"})
	for !data_stmt_done(p, body_start) {
		if allow_keyword(p, "INTO") || allow_keyword(p, "FROM") {
			stmt.table = data_expr(p, body_start, []string{"LANGUAGE"})
			continue
		}
		if allow_keyword(p, "LANGUAGE") {
			stmt.language = data_expr(p, body_start, []string{})
			continue
		}
		bump_token(p)
	}
	stmt.range = data_stmt_range(p, start)
	return stmt
}

parse_direct_call_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	expr := parse_expr(p)
	if expr == nil {
		return nil
	}
	period := expect_token_message(p, .Period, "syntax error: expected '.' after method call")
	if period.kind != .Period {
		return nil
	}
	stmt := ast.new(
		ast.Call_Stmt,
		tokenizer.text_range(expr.range.start, period.range.end),
		p.allocator,
	)
	stmt.kind = .Direct
	stmt.call = expr
	return stmt
}

dataset_stmt_starts :: proc(p: ^Parser) -> bool {
	return(
		(at_keyword(p, "OPEN") ||
				at_keyword(p, "CLOSE") ||
				at_keyword(p, "DELETE") ||
				at_keyword(p, "READ") ||
				at_keyword(p, "GET") ||
				at_keyword(p, "SET") ||
				at_keyword(p, "TRUNCATE")) &&
			at_keyword_index(p, p.index + 1, "DATASET") ||
		at_keyword(p, "TRANSFER") \
	)
}

report_textpool_stmt_starts :: proc(p: ^Parser) -> bool {
	if !(at_keyword(p, "READ") || at_keyword(p, "INSERT") || at_keyword(p, "DELETE")) {
		return false
	}
	return(
		at_keyword_index(p, p.index + 1, "REPORT") ||
		at_keyword_index(p, p.index + 1, "TEXTPOOL") \
	)
}

direct_call_stmt_starts :: proc(p: ^Parser) -> bool {
	if keyword_is_compact_call(p, "CLEANUP") ||
	   keyword_is_compact_call(p, "SET") ||
	   keyword_is_compact_call(p, "SORT") ||
	   keyword_is_compact_call(p, "UPDATE") {
		return true
	}
	_, stray := stray_block_boundary(p)
	if stray || !expr_lead_token(current_token(p)) {
		return false
	}
	lparen := direct_call_lparen_index(p)
	if lparen < 0 {
		return false
	}
	if known_stmt_lead_at(p, p.index) {
		return lparen == p.index + 1 && tokens_touch(current_token(p), p.tokens[lparen])
	}
	target_has_selector := false
	for i in p.index ..< lparen {
		if p.tokens[i].kind == .Arrow ||
		   p.tokens[i].kind == .FatArrow ||
		   p.tokens[i].kind == .Tilde {
			target_has_selector = true
			break
		}
	}
	return target_has_selector || lparen == p.index + 1
}

direct_call_lparen_index :: proc(p: ^Parser) -> int {
	paren := 0
	bracket := 0
	brace := 0
	for i in p.index ..< len(p.tokens) {
		tok := p.tokens[i]
		if tok.kind == .Eof || tok.kind == .Period {
			return -1
		}
		if paren == 0 && bracket == 0 && brace == 0 && tok.kind == .LParen {
			if i > p.index {
				prev := p.tokens[i - 1]
				if (prev.kind == .Arrow || prev.kind == .FatArrow) && tokens_touch(prev, tok) {
					paren += 1
					continue
				}
			}
			return i
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
	}
	return -1
}
