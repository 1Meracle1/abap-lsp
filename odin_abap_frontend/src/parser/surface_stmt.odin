package abap_frontend_parser

import "../ast"
import "../tokenizer"

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
		append(&stmt.names, ast.Include_Name{tokenizer.token_lexeme(name, p.source), name.range})
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
	period := expect_token(p, .Period)
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
		expr := ast.new(ast.Host_Expr, tokenizer.text_range(host.range.start, value.range.end), p.allocator)
		expr.value = value
		return expr
	}
	return value
}

sql_data_expr :: proc(p: ^Parser, body_start: int, stop_keywords: []string) -> ^ast.Expr {
	old_stops := p.expr_stop_keywords
	p.expr_stop_keywords = stop_keywords
	defer p.expr_stop_keywords = old_stops
	return data_expr(p, body_start, stop_keywords)
}

sql_logical_expr :: proc(p: ^Parser, body_start: int, stop_keywords: []string) -> ^ast.Expr {
	if data_stmt_done(p, body_start) || data_current_keyword_in(p, stop_keywords) {
		error_current(p, "syntax error: expected expression")
		return nil
	}
	old_stops := p.expr_stop_keywords
	p.expr_stop_keywords = stop_keywords
	defer p.expr_stop_keywords = old_stops
	expr := parse_logical_expr(p)
	if expr == nil && (data_stmt_done(p, body_start) || data_current_keyword_in(p, stop_keywords)) {
		error_current(p, "syntax error: expected expression")
	}
	return expr
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
	stmt := ast.new(ast.Exec_Sql_Stmt, tokenizer.text_range(start.range.start, period.range.end), p.allocator)
	stmt.header_range = tokenizer.text_range(start.range.start, header_period.range.end)
	if header_period.range.end < body_end {
		stmt.body = source_range_text(p, tokenizer.text_range(header_period.range.end, body_end))
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
	if select_query_has_loop_body(stmt.query) && keyword_phrase_ahead(p, "ENDSELECT") {
		stmt.body = parse_stmt_list_until(p, []string{"ENDSELECT"})
		end := expect_keyword(p, "ENDSELECT")
		if token_is_keyword(p, end, "ENDSELECT") {
			period := expect_token(p, .Period)
			stmt.range.end = statement_end(p, period)
		}
	}
	return stmt
}

parse_select_with_clause :: proc(p: ^Parser, body_start: int) -> ^ast.Select_With_Clause {
	start := expect_keyword(p, "WITH")
	clause, _ := mem.new(ast.Select_With_Clause, p.allocator)
	clause.entries = make([dynamic]ast.Select_Cte_Clause, 0, 2, p.allocator)
	for !data_stmt_done(p, body_start) && !at_keyword(p, "SELECT") {
		entry := ast.Select_Cte_Clause{name = parse_cte_name(p)}
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

parse_cte_name :: proc(p: ^Parser) -> string {
	start_index := p.index
	start := current_token(p)
	for !at_eof(p) && !at_keyword(p, "AS") && current_token(p).kind != .Comma {
		bump_token(p)
	}
	if p.index == start_index {
		return ""
	}
	return strings.clone(p.source[start.range.start:previous_token(p).range.end], p.allocator)
}

parse_select_query_clause :: proc(p: ^Parser, body_start: int, stop_at_rparen := false) -> ast.Select_Query_Clause {
	query := ast.Select_Query_Clause{}
	query.projections = make([dynamic]^ast.Expr, 0, 4, p.allocator)
	query.projection_clauses = make([dynamic]ast.Select_Projection_Clause, 0, 4, p.allocator)
	query.set_ops = make([dynamic]ast.Select_Set_Clause, 0, 1, p.allocator)
	query.order_by_fields = make([dynamic]string, 0, 2, p.allocator)
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
			value := ast.new(ast.Literal_Expr, star.range, p.allocator)
			value.value = "*"
			append(&query.projections, value)
			append(&query.projection_clauses, ast.Select_Projection_Clause{value = value, range = value.range})
			query.projection_clause = select_merge_range(query.projection_clause, value.range)
			continue
		}
		start := p.index
		value := sql_data_expr(
			p,
			body_start,
			[]string {
				"FROM",
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
				"AS",
				"UNION",
				"INTERSECT",
				"EXCEPT",
			},
		)
		if value != nil {
			alias := parse_select_alias(p)
			projection_range := tokenizer.text_range(value.range.start, previous_token(p).range.end)
			append(&query.projections, value)
			append(&query.projection_clauses, ast.Select_Projection_Clause{value = value, alias = alias, range = projection_range})
			query.projection_clause = select_merge_range(query.projection_clause, projection_range)
		} else {
			bump_token(p)
		}
		ensure_forward_progress(p, start)
	}
	for !select_query_done(p, body_start, stop_at_rparen) {
		if at_keyword(p, "FROM") {
			start := bump_token(p)
			if query.source_clause != nil {
				error(p, start.range, "syntax error: duplicate SELECT FROM clause")
				_ = select_skip_clause(p, start, body_start, stop_at_rparen)
				continue
			}
			query.source_clause = parse_select_source_clause(p, body_start)
			if query.source_clause != nil {
				query.source = query.source_clause.source
				query.from_clause = query.source_clause.range
			}
			continue
		}
		if at_keyword(p, "INTO") {
			start := bump_token(p)
			if query.result != nil {
				error(p, start.range, "syntax error: duplicate SELECT result clause")
				_ = select_skip_clause(p, start, body_start, stop_at_rparen)
				continue
			}
			query.result = parse_select_result_tail(p, .Into, body_start)
			query.into_clause = query.result.range if query.result != nil else tokenizer.Range{}
			continue
		}
		if at_keyword(p, "APPENDING") {
			start := bump_token(p)
			if query.result != nil {
				error(p, start.range, "syntax error: duplicate SELECT result clause")
				_ = select_skip_clause(p, start, body_start, stop_at_rparen)
				continue
			}
			query.result = parse_select_result_tail(p, .Appending, body_start)
			query.into_clause = query.result.range if query.result != nil else tokenizer.Range{}
			continue
		}
		if at_keyword(p, "WHERE") {
			start := bump_token(p)
			if query.source_clause == nil ||
			   query.where_cond != nil ||
			   select_range_valid(query.group_by_clause) ||
			   select_range_valid(query.having_clause) ||
			   select_range_valid(query.order_by_clause) {
				error(p, start.range, "syntax error: invalid SELECT WHERE clause placement")
				_ = select_skip_clause(p, start, body_start, stop_at_rparen)
				continue
			}
			query.dynamic_where = current_token(p).kind == .LParen
			query.where_cond = sql_logical_expr(
				p,
				body_start,
				[]string{"INTO", "APPENDING", "WHERE", "FOR", "GROUP", "HAVING", "ORDER", "UP", "PACKAGE", "OFFSET", "BYPASSING", "CONNECTION", "CLIENT", "UNION", "INTERSECT", "EXCEPT"},
			)
			if query.where_cond != nil {
				query.where_clause = select_clause_expr_range(p, start, query.where_cond)
			}
			continue
		}
		if allow_keyword(p, "FOR") {
			start := previous_token(p)
			if allow_keyword(p, "ALL") && allow_keyword(p, "ENTRIES") && allow_keyword(p, "IN") {
				query.for_all_entries = sql_data_expr(
					p,
					body_start,
					[]string{"WHERE", "GROUP", "HAVING", "ORDER", "UP", "PACKAGE", "OFFSET", "BYPASSING", "CONNECTION", "CLIENT", "UNION", "INTERSECT", "EXCEPT"},
				)
				query.for_all_entries_clause = select_clause_expr_range(p, start, query.for_all_entries)
			} else if allow_keyword(p, "UPDATE") {
				query.for_update_clause = tokenizer.text_range(start.range.start, previous_token(p).range.end)
			}
			continue
		}
		if allow_keyword(p, "GROUP") {
			start := previous_token(p)
			if query.source_clause == nil ||
			   select_range_valid(query.group_by_clause) ||
			   select_range_valid(query.having_clause) ||
			   select_range_valid(query.order_by_clause) {
				error(p, start.range, "syntax error: invalid SELECT GROUP BY clause placement")
				_ = select_skip_clause(p, start, body_start, stop_at_rparen)
				continue
			}
			if !allow_keyword(p, "BY") {
				error_current(p, "syntax error: expected keyword")
			}
			query.group_by_clause = select_skip_clause(p, start, body_start, stop_at_rparen)
			continue
		}
		if allow_keyword(p, "HAVING") {
			start := previous_token(p)
			if !select_range_valid(query.group_by_clause) ||
			   select_range_valid(query.having_clause) ||
			   select_range_valid(query.order_by_clause) {
				error(p, start.range, "syntax error: invalid SELECT HAVING clause placement")
				_ = select_skip_clause(p, start, body_start, stop_at_rparen)
				continue
			}
			query.having_clause = select_skip_clause(p, start, body_start, stop_at_rparen)
			continue
		}
		if allow_keyword(p, "ORDER") {
			if query.source_clause == nil || select_range_valid(query.order_by_clause) {
				error(p, previous_token(p).range, "syntax error: invalid SELECT ORDER BY clause placement")
				_ = select_skip_clause(p, previous_token(p), body_start, stop_at_rparen)
				continue
			}
			parse_select_order_by_clause(p, &query, previous_token(p), body_start, stop_at_rparen)
			continue
		}
		if allow_keyword(p, "PACKAGE") {
			start := previous_token(p)
			allow_keyword(p, "SIZE")
			query.package_size = sql_data_expr(
				p,
				body_start,
				[]string{"INTO", "APPENDING", "WHERE", "GROUP", "HAVING", "ORDER", "UP", "OFFSET", "BYPASSING", "CONNECTION", "CLIENT", "UNION", "INTERSECT", "EXCEPT"},
			)
			query.package_size_clause = select_clause_expr_range(p, start, query.package_size)
			continue
		}
		if allow_keyword(p, "UP") {
			start := previous_token(p)
			allow_keyword(p, "TO")
			query.up_to_rows = sql_data_expr(
				p,
				body_start,
				[]string{"ROWS", "INTO", "APPENDING", "WHERE", "GROUP", "HAVING", "ORDER", "PACKAGE", "OFFSET", "BYPASSING", "CONNECTION", "CLIENT", "UNION", "INTERSECT", "EXCEPT"},
			)
			allow_keyword(p, "ROWS")
			query.up_to_clause = select_clause_expr_range(p, start, query.up_to_rows)
			continue
		}
		if allow_keyword(p, "OFFSET") {
			start := previous_token(p)
			query.offset_clause = select_skip_clause(p, start, body_start, stop_at_rparen)
			continue
		}
		if allow_keyword(p, "BYPASSING") ||
		   allow_keyword(p, "CONNECTION") ||
		   allow_keyword(p, "CLIENT") {
			start := previous_token(p)
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
			query.set_operator_clause = select_merge_range(
				query.set_operator_clause,
				tokenizer.text_range(set_start.range.start, previous_token(p).range.end),
			)
			append(&query.set_ops, ast.Select_Set_Clause{
				kind  = kind,
				all   = all,
				query = parse_select_query_clause(p, body_start, stop_at_rparen),
			})
			continue
		}
		bump_token(p)
	}
	return query
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
	descending := false
	if allow_keyword(p, "PRIMARY") {
		query.order_by_primary_key = allow_keyword(p, "KEY")
		query.order_by_clause = select_skip_clause(p, start, body_start, stop_at_rparen)
		return
	}
	for !select_query_done(p, body_start, stop_at_rparen) && !select_clause_starts(p) {
		if allow_token(p, .Comma) ||
		   allow_keyword(p, "ASCENDING") ||
		   allow_keyword(p, "NULLS") ||
		   allow_keyword(p, "FIRST") ||
		   allow_keyword(p, "LAST") {
			continue
		}
		if allow_keyword(p, "DESCENDING") {
			descending = true
			continue
		}
		if current_token(p).kind == .Ident {
			if p.index + 2 < len(p.tokens) &&
			   p.tokens[p.index + 1].kind == .Tilde &&
			   p.tokens[p.index + 2].kind == .Ident {
				if !descending {
					append(&query.order_by_fields, tokenizer.token_lexeme(p.tokens[p.index + 2], p.source))
				}
				bump_token(p)
				bump_token(p)
				bump_token(p)
			} else {
				if !descending {
					append(&query.order_by_fields, tokenizer.token_lexeme(current_token(p), p.source))
				}
				bump_token(p)
			}
			continue
		}
		bump_token(p)
	}
	if descending {
		query.order_by_fields = make([dynamic]string, 0, 0, p.allocator)
	}
	query.order_by_clause = tokenizer.text_range(start.range.start, previous_token(p).range.end)
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
	clause.source = sql_data_expr(
		p,
		body_start,
		[]string{"AS", "INNER", "LEFT", "RIGHT", "FULL", "CROSS", "JOIN", "INTO", "APPENDING", "WHERE", "FOR", "GROUP", "HAVING", "ORDER", "UP", "PACKAGE", "OFFSET", "BYPASSING", "CONNECTION", "CLIENT", "UNION", "INTERSECT", "EXCEPT"},
	)
	clause.alias = parse_select_alias(p)
	for !data_stmt_done(p, body_start) && !select_clause_starts(p) {
		kind, ok := select_join_kind(p)
		if !ok {
			break
		}
		join := ast.Select_Join_Clause{kind = kind}
		join.source = sql_data_expr(
			p,
			body_start,
			[]string{"AS", "ON", "INNER", "LEFT", "RIGHT", "FULL", "CROSS", "JOIN", "INTO", "APPENDING", "WHERE", "FOR", "GROUP", "HAVING", "ORDER", "UP", "PACKAGE", "OFFSET", "BYPASSING", "CONNECTION", "CLIENT", "UNION", "INTERSECT", "EXCEPT"},
		)
		join.alias = parse_select_alias(p)
		if allow_keyword(p, "ON") {
			join.on = sql_logical_expr(
				p,
				body_start,
				[]string{"INNER", "LEFT", "RIGHT", "FULL", "CROSS", "JOIN", "INTO", "APPENDING", "WHERE", "FOR", "GROUP", "HAVING", "ORDER", "UP", "PACKAGE", "OFFSET", "BYPASSING", "CONNECTION", "CLIENT", "UNION", "INTERSECT", "EXCEPT"},
			)
		}
		append(&clause.joins, join)
	}
	if clause.source != nil {
		clause.range = tokenizer.text_range(start.range.start, previous_token(p).range.end)
	}
	return clause
}

parse_select_alias :: proc(p: ^Parser) -> string {
	if !allow_keyword(p, "AS") {
		return ""
	}
	tok := current_token(p)
	if tok.kind != .Ident {
		return ""
	}
	bump_token(p)
	return tokenizer.token_lexeme(tok, p.source)
}

select_join_kind :: proc(p: ^Parser) -> (ast.Select_Join_Kind, bool) {
	if allow_keyword(p, "INNER") {
		allow_keyword(p, "JOIN")
		return .Inner, true
	}
	if allow_keyword(p, "LEFT") {
		allow_keyword(p, "OUTER")
		allow_keyword(p, "JOIN")
		return .Left_Outer, true
	}
	if allow_keyword(p, "RIGHT") {
		allow_keyword(p, "OUTER")
		allow_keyword(p, "JOIN")
		return .Right_Outer, true
	}
	if allow_keyword(p, "FULL") {
		allow_keyword(p, "OUTER")
		allow_keyword(p, "JOIN")
		return .Full_Outer, true
	}
	if allow_keyword(p, "CROSS") {
		allow_keyword(p, "JOIN")
		return .Cross, true
	}
	if allow_keyword(p, "JOIN") {
		return .Inner, true
	}
	return .Inner, false
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
	if current_token(p).kind == .LParen {
		clause.target = parse_raw_operand_to_period(
			p,
			[]string{"PACKAGE", "WHERE", "GROUP", "HAVING", "ORDER", "UP", "FOR", "FROM", "OFFSET", "BYPASSING", "CONNECTION", "CLIENT", "UNION", "INTERSECT", "EXCEPT"},
		)
	} else {
		clause.target = sql_data_expr(
			p,
			body_start,
			[]string{"PACKAGE", "WHERE", "GROUP", "HAVING", "ORDER", "UP", "FOR", "FROM", "OFFSET", "BYPASSING", "CONNECTION", "CLIENT", "UNION", "INTERSECT", "EXCEPT"},
		)
	}
	clause.range = tokenizer.text_range(start.range.start, previous_token(p).range.end)
	return clause
}

select_clause_starts :: proc(p: ^Parser) -> bool {
	return(
		at_keyword(p, "FROM") ||
		at_keyword(p, "INTO") ||
		at_keyword(p, "APPENDING") ||
		at_keyword(p, "WHERE") ||
		at_keyword(p, "FOR") ||
		at_keyword(p, "GROUP") ||
		at_keyword(p, "HAVING") ||
		at_keyword(p, "ORDER") ||
		at_keyword(p, "UP") ||
		at_keyword(p, "PACKAGE") ||
		at_keyword(p, "OFFSET") ||
		at_keyword(p, "BYPASSING") ||
		at_keyword(p, "CONNECTION") ||
		at_keyword(p, "CLIENT") ||
		at_keyword(p, "UNION") ||
		at_keyword(p, "INTERSECT") ||
		at_keyword(p, "EXCEPT") \
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
			entry.into = data_expr(
				p,
				body_start,
				[]string{"WITH", "INDEX", "USING", "TRANSPORTING", "COMPARING", "BINARY"},
			)
			continue
		}
		if allow_keyword(p, "ASSIGNING") {
			entry.assigning = data_expr(
				p,
				body_start,
				[]string{"WITH", "INDEX", "USING", "TRANSPORTING", "COMPARING", "BINARY"},
			)
			continue
		}
		if allow_keyword(p, "REFERENCE") {
			allow_keyword(p, "INTO")
			entry.reference_into = data_expr(
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
			allow_keyword(p, "KEY")
			entry.using_key = data_expr(
				p,
				body_start,
				[]string{"ASSIGNING", "INTO", "TRANSPORTING", "COMPARING", "BINARY"},
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
		if allow_keyword(p, "BINARY") {
			entry.binary_search = allow_keyword(p, "SEARCH")
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
		if current_token(p).kind == .Ident && read_table_key_name_end(p) >= 0 {
			name_start := current_token(p).range.start
			name_end := read_table_key_name_end(p)
			name_end_byte := p.tokens[name_end - 1].range.end
			for p.index < name_end {
				bump_token(p)
			}
			expect_token(p, .Eq)
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
					name = p.source[name_start:name_end_byte],
					value = value,
				},
			)
			continue
		}
		if entry.key_name == "" && current_token(p).kind == .Ident {
			entry.key_name = tokenizer.token_lexeme(bump_token(p), p.source)
			continue
		}
		bump_token(p)
	}
}

read_table_key_name_end :: proc(p: ^Parser) -> int {
	paren := 0
	bracket := 0
	brace := 0
	for i in p.index ..< len(p.tokens) {
		tok := p.tokens[i]
		top := paren == 0 && bracket == 0 && brace == 0
		if top && tok.kind == .Eq {
			return i
		}
		if top && (tok.kind == .Period || tok.kind == .Comma || tok.kind == .Eof) {
			return -1
		}
		if top &&
		   i > p.index &&
		   tok.kind == .Ident &&
		   data_current_keyword_in_at(
			   p,
			   i,
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
			return -1
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

data_current_keyword_in_at :: proc(p: ^Parser, index: int, keywords: []string) -> bool {
	for keyword in keywords {
		if keyword_phrase_at(p, index, keyword) {
			return true
		}
	}
	return false
}

parse_insert_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "INSERT")
	body_start := p.index
	stmt := ast.new(ast.Insert_Stmt, start.range, p.allocator)
	stmt.assignments = make([dynamic]ast.Sql_Assignment_Clause, 0, 2, p.allocator)
	initial_table_name := ""
	initial_table_range := tokenizer.Range{}
	if current_token(p).kind == .Ident {
		initial_table_name = tokenizer.token_lexeme(current_token(p), p.source)
		initial_table_range = current_token(p).range
	}
	if allow_keyword(p, "INTO") {
		stmt.form = .Db_Table
		stmt.target = data_expr(p, body_start, []string{"VALUES", "FROM", "SET", "ACCEPTING"})
		if stmt.target != nil {
			if id, ok := stmt.target.derived_expr.(^ast.Ident_Expr); ok && id.name != "" {
				stmt.has_db_table_name = true
				stmt.db_table_name = id.name
				stmt.db_table_name_range = id.range
			}
		}
		parse_insert_tail(p, body_start, stmt)
		stmt.range = data_stmt_range(p, start)
		return stmt
	}
	if allow_keyword(p, "LINES") {
		allow_keyword(p, "OF")
		stmt.form = .Lines_Of
		stmt.source = data_expr(p, body_start, []string{"INTO", "FROM", "TO", "USING"})
	} else {
		stmt.source = data_expr(
			p,
			body_start,
			[]string{"INTO", "FROM", "VALUES", "SET", "ACCEPTING"},
		)
	}
	parse_insert_tail(p, body_start, stmt)
	if stmt.form == .Db_Table && stmt.target == nil && initial_table_name != "" {
		stmt.has_db_table_name = true
		stmt.db_table_name = initial_table_name
		stmt.db_table_name_range = initial_table_range
	}
	stmt.range = data_stmt_range(p, start)
	return stmt
}

parse_insert_tail :: proc(p: ^Parser, body_start: int, stmt: ^ast.Insert_Stmt) {
	for !data_stmt_done(p, body_start) {
		if allow_keyword(p, "INTO") {
			stmt.form = .Internal_Table if stmt.form != .Lines_Of else .Lines_Of
			allow_keyword(p, "TABLE")
			stmt.target = data_expr(
				p,
				body_start,
				[]string{"INDEX", "ASSIGNING", "REFERENCE", "ACCEPTING"},
			)
			continue
		}
		if allow_keyword(p, "FROM") {
			stmt.form = .Db_Table
			stmt.from_table = allow_keyword(p, "TABLE")
			stmt.source = data_expr(p, body_start, []string{"ACCEPTING"})
			continue
		}
		if allow_keyword(p, "VALUES") {
			stmt.form = .Db_Table
			stmt.source = data_expr(p, body_start, []string{"ACCEPTING"})
			continue
		}
		if allow_keyword(p, "SET") {
			stmt.form = .Db_Table
			parse_sql_assignments(p, body_start, &stmt.assignments, []string{"ACCEPTING"})
			continue
		}
		if allow_keyword(p, "INDEX") {
			stmt.index = data_expr(p, body_start, []string{"ASSIGNING", "REFERENCE", "ACCEPTING"})
			continue
		}
		if allow_keyword(p, "ASSIGNING") {
			stmt.assigning = data_expr(p, body_start, []string{"REFERENCE", "ACCEPTING"})
			continue
		}
		if allow_keyword(p, "REFERENCE") {
			allow_keyword(p, "INTO")
			stmt.reference_into = data_expr(p, body_start, []string{"ACCEPTING"})
			continue
		}
		if allow_keyword(p, "ACCEPTING") {
			if allow_keyword(p, "DUPLICATE") {
				stmt.accepting_duplicate_keys = allow_keyword(p, "KEYS")
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
	}
	stmt.source = data_expr(p, body_start, []string{"TO", "ASSIGNING", "REFERENCE"})
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
		stmt.source = data_expr(p, body_start, []string{"NAME", "MESSAGE", "LINE", "WORD", "OFFSET"})
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
	if len(query.projections) == 1 && select_projection_is_aggregate(query.projections[0]) {
		return false
	}
	return query.result != nil && !query.result.table
}

select_projection_is_aggregate :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Call_Expr:
		if n.callee == nil {
			return false
		}
		if name, ok := n.callee.derived_expr.(^ast.Ident_Expr); ok {
			return select_aggregate_name(name.name)
		}
	}
	return false
}

select_aggregate_name :: proc(name: string) -> bool {
	return(
		ascii_equal_ignore_case(name, "COUNT") ||
		ascii_equal_ignore_case(name, "MAX") ||
		ascii_equal_ignore_case(name, "MIN") ||
		ascii_equal_ignore_case(name, "SUM") ||
		ascii_equal_ignore_case(name, "AVG") \
	)
}

parse_modify_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "MODIFY")
	body_start := p.index
	stmt := ast.new(ast.Modify_Stmt, start.range, p.allocator)
	stmt.transporting = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	allow_keyword(p, "TABLE")
	stmt.target = data_expr(
		p,
		body_start,
		[]string{"FROM", "INDEX", "TRANSPORTING", "WHERE", "ASSIGNING", "REFERENCE"},
	)
	for !data_stmt_done(p, body_start) {
		if allow_keyword(p, "FROM") {
			stmt.from_table = allow_keyword(p, "TABLE")
			stmt.source = data_expr(
				p,
				body_start,
				[]string{"INDEX", "TRANSPORTING", "WHERE", "ASSIGNING", "REFERENCE"},
			)
			continue
		}
		if allow_keyword(p, "INDEX") {
			stmt.index = data_expr(
				p,
				body_start,
				[]string{"TRANSPORTING", "WHERE", "ASSIGNING", "REFERENCE"},
			)
			continue
		}
		if allow_keyword(p, "TRANSPORTING") {
			values := data_exprs_until(p, body_start, []string{"WHERE", "ASSIGNING", "REFERENCE"})
			for value in values {append(&stmt.transporting, value)}
			continue
		}
		if allow_keyword(p, "WHERE") {
			stmt.where_cond = sql_logical_expr(
				p,
				body_start,
				[]string{"ASSIGNING", "REFERENCE"},
			)
			continue
		}
		bump_token(p)
	}
	stmt.range = data_stmt_range(p, start)
	return stmt
}

parse_sort_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "SORT")
	body_start := p.index
	stmt := ast.new(ast.Sort_Stmt, start.range, p.allocator)
	stmt.fields = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	stmt.stable = allow_keyword(p, "STABLE")
	stmt.target = data_expr(p, body_start, []string{"BY", "AS", "ASCENDING", "DESCENDING"})
	for !data_stmt_done(p, body_start) {
		if allow_keyword(p, "AS") {
			stmt.as_text = allow_keyword(p, "TEXT")
			continue
		}
		if allow_keyword(p, "BY") {
			values := data_exprs_until(p, body_start, []string{"AS", "ASCENDING", "DESCENDING"})
			for value in values {append(&stmt.fields, value)}
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

parse_sql_assignments :: proc(
	p: ^Parser,
	body_start: int,
	list: ^[dynamic]ast.Sql_Assignment_Clause,
	stop_keywords: []string,
) {
	for !data_stmt_done(p, body_start) && !data_current_keyword_in(p, stop_keywords) {
		if allow_token(p, .Comma) {
			continue
		}
		name := sql_data_expr(p, body_start, stop_keywords)
		if name == nil {
			bump_token(p)
			continue
		}
		if !allow_token(p, .Eq) {
			continue
		}
		value := sql_data_expr(p, body_start, stop_keywords)
		if value == nil {
			error_current(p, "syntax error: expected expression")
			continue
		}
		append(list, ast.Sql_Assignment_Clause{name = name, value = value})
	}
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
	for !data_stmt_done(p, body_start) {
		if allow_keyword(p, "FROM") {
			stmt.from_table = allow_keyword(p, "TABLE")
			stmt.source = sql_data_expr(
				p,
				body_start,
				[]string{"WHERE", "USING", "CLIENT", "CONNECTION"},
			)
			continue
		}
		if allow_keyword(p, "SET") {
			parse_sql_assignments(
				p,
				body_start,
				&stmt.assignments,
				[]string{"WHERE", "USING", "CLIENT", "CONNECTION"},
			)
			continue
		}
		if allow_keyword(p, "WHERE") {
			stmt.dynamic_where = current_token(p).kind == .LParen
			stmt.where_cond = sql_logical_expr(
				p,
				body_start,
				[]string{"USING", "CLIENT", "CONNECTION"},
			)
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
	stmt.comparing = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	if allow_keyword(p, "ADJACENT") {
		stmt.form = .Adjacent_Duplicates
		allow_keyword(p, "DUPLICATES")
		allow_keyword(p, "FROM")
		stmt.target = data_expr(p, body_start, []string{"COMPARING"})
	} else if allow_keyword(p, "FROM") {
		stmt.form = .Db_Table
		stmt.target = sql_data_expr(p, body_start, []string{"WHERE", "CLIENT", "CONNECTION"})
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
			stmt.from_table = allow_keyword(p, "TABLE")
			stmt.source = data_expr(
				p,
				body_start,
				[]string{"WHERE", "INDEX", "USING", "COMPARING"},
			)
			continue
		}
		if allow_keyword(p, "WHERE") {
			stmt.where_cond = sql_logical_expr(
				p,
				body_start,
				[]string{"INDEX", "USING", "COMPARING", "CLIENT", "CONNECTION"},
			)
			continue
		}
		if allow_keyword(p, "INDEX") {
			stmt.index = data_expr(p, body_start, []string{"USING", "COMPARING"})
			continue
		}
		if allow_keyword(p, "USING") {
			allow_keyword(p, "KEY")
			stmt.using_key = data_expr(p, body_start, []string{"COMPARING"})
			continue
		}
		if allow_keyword(p, "COMPARING") {
			more := data_exprs_until(p, body_start, []string{})
			for value in more {append(&stmt.comparing, value)}
			continue
		}
		bump_token(p)
	}
	stmt.range = data_stmt_range(p, start)
	return stmt
}

parse_dataset_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := current_token(p)
	body_start := p.index
	stmt := ast.new(ast.Dataset_Stmt, start.range, p.allocator)
	if allow_keyword(p, "TRANSFER") {
		stmt.kind = .Transfer
		stmt.source = data_expr(p, body_start, []string{"TO"})
		allow_keyword(p, "TO")
		stmt.dataset = data_expr(p, body_start, []string{"LENGTH", "NO"})
	} else if allow_keyword(p, "OPEN") {
		stmt.kind = .Open
		allow_keyword(p, "DATASET")
		stmt.dataset = data_expr(
			p,
			body_start,
			[]string {
				"FOR",
				"IN",
				"AT",
				"TYPE",
				"FILTER",
				"MESSAGE",
				"IGNORING",
				"REPLACEMENT",
				"WITH",
			},
		)
	} else if allow_keyword(p, "READ") {
		stmt.kind = .Read
		allow_keyword(p, "DATASET")
		stmt.dataset = data_expr(p, body_start, []string{"INTO", "MAXIMUM", "ACTUAL", "LENGTH"})
	} else if allow_keyword(p, "CLOSE") {
		stmt.kind = .Close
		allow_keyword(p, "DATASET")
		stmt.dataset = data_expr(p, body_start, []string{})
	} else if allow_keyword(p, "DELETE") {
		stmt.kind = .Delete
		allow_keyword(p, "DATASET")
		stmt.dataset = data_expr(p, body_start, []string{})
	} else if allow_keyword(p, "GET") {
		stmt.kind = .Get
		allow_keyword(p, "DATASET")
		stmt.dataset = data_expr(p, body_start, []string{"POSITION", "ATTRIBUTES"})
	} else if allow_keyword(p, "SET") {
		stmt.kind = .Set
		allow_keyword(p, "DATASET")
		stmt.dataset = data_expr(p, body_start, []string{"POSITION", "ATTRIBUTES"})
	} else {
		stmt.kind = .Truncate
		allow_keyword(p, "TRUNCATE")
		allow_keyword(p, "DATASET")
		stmt.dataset = data_expr(p, body_start, []string{"AT"})
	}
	parse_dataset_tail(p, body_start, stmt)
	stmt.range = data_stmt_range(p, start)
	return stmt
}

parse_dataset_tail :: proc(p: ^Parser, body_start: int, stmt: ^ast.Dataset_Stmt) {
	for !data_stmt_done(p, body_start) {
		if allow_keyword(p, "FOR") {
			if allow_keyword(
				p,
				"INPUT",
			) {
				stmt.access = .Input
			} else if allow_keyword(p, "OUTPUT") {
				stmt.access = .Output
			} else if allow_keyword(p, "APPENDING") {
				stmt.access = .Append
			} else if allow_keyword(p, "UPDATE") {
				stmt.access = .Update
			}
			continue
		}
		if allow_keyword(p, "IN") {
			if allow_keyword(
				p,
				"TEXT",
			) {
				stmt.text_mode = true
				allow_keyword(p, "MODE")
			} else if allow_keyword(p, "BINARY") {
				stmt.binary_mode = true
				allow_keyword(p, "MODE")
			}
			continue
		}
		if allow_keyword(p, "ENCODING") {
			tok := current_token(p)
			if tok.kind == .Ident || tok.kind == .String {
				stmt.encoding = tokenizer.token_lexeme(bump_token(p), p.source)
			}
			continue
		}
		if allow_keyword(p, "INTO") {
			stmt.target = data_expr(p, body_start, []string{"MAXIMUM", "ACTUAL", "LENGTH"})
			continue
		}
		if allow_keyword(p, "MAXIMUM") {
			allow_keyword(p, "LENGTH")
			stmt.maximum_length = data_expr(p, body_start, []string{"ACTUAL", "LENGTH"})
			continue
		}
		if allow_keyword(p, "ACTUAL") {
			allow_keyword(p, "LENGTH")
			stmt.actual_length = data_expr(p, body_start, []string{"LENGTH"})
			continue
		}
		if allow_keyword(p, "LENGTH") {
			stmt.length = data_expr(p, body_start, []string{"NO"})
			continue
		}
		if allow_keyword(p, "AT") {
			if allow_keyword(p, "CURRENT") {
				stmt.at_current_position = allow_keyword(p, "POSITION")
			} else {
				allow_keyword(p, "POSITION")
				stmt.position = data_expr(p, body_start, []string{"MESSAGE"})
			}
			continue
		}
		if allow_keyword(p, "POSITION") {
			if allow_keyword(p, "END") {
				allow_keyword(p, "OF")
				allow_keyword(p, "FILE")
			} else {
				stmt.position = data_expr(p, body_start, []string{"ATTRIBUTES"})
			}
			continue
		}
		if allow_keyword(p, "ATTRIBUTES") {
			stmt.attributes = data_expr(p, body_start, []string{"POSITION"})
			continue
		}
		if allow_keyword(p, "MESSAGE") {
			stmt.message = data_expr(p, body_start, []string{})
			continue
		}
		bump_token(p)
	}
}

parse_report_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := current_token(p)
	body_start := p.index
	stmt := ast.new(ast.Report_Stmt, start.range, p.allocator)
	if allow_keyword(p, "REPORT") {
		stmt.kind = .Report
		stmt.name = data_expr(p, body_start, []string{"MESSAGE-ID", "LINE-SIZE", "LINE", "LINE-COUNT"})
	} else if allow_keyword(p, "PROGRAM") {
		stmt.kind = .Program
		stmt.name = data_expr(p, body_start, []string{"MESSAGE-ID", "LINE-SIZE", "LINE", "LINE-COUNT"})
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
		if (stmt.kind == .Report || stmt.kind == .Program) && allow_keyword_phrase(p, "MESSAGE-ID") {
			tok := current_token(p)
			if tok.kind == .Ident || tok.kind == .Number || tok.kind == .String {
				bump_token(p)
				stmt.has_message_id = true
				stmt.message_id = tokenizer.token_lexeme(tok, p.source)
				stmt.message_id_range = tok.range
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
	_, _, stray := stray_block_boundary(p)
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
		if p.tokens[i].kind == .Arrow || p.tokens[i].kind == .FatArrow || p.tokens[i].kind == .Tilde {
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
