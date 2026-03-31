package lang_parser

import "../ast"
import "../lexer"

// Shared shape for Open SQL INTO / APPENDING ... target (SELECT, FETCH NEXT CURSOR, ...).
Open_Sql_Into_Clause :: struct {
	into_kind:                   ast.Select_Into_Kind,
	into_corresponding_of_table: bool,
	into_target:                 ^ast.Expr,
}

// parse_open_sql_into_clause parses the part after INTO or APPENDING (keyword already consumed for INTO;
// for APPENDING, call parse_select_into_clause_body which uses this internally).
parse_open_sql_into_clause :: proc(p: ^Parser) -> Open_Sql_Into_Clause {
	clause: Open_Sql_Into_Clause
	if check_keyword(p, "TABLE") {
		advance_token(p)
		clause.into_kind = .Table
	} else if check_keyword(p, "CORRESPONDING") {
		advance_token(p)
		expect_keyword_token(p, "FIELDS")
		expect_keyword_token(p, "OF")
		clause.into_corresponding_of_table = false
		if check_keyword(p, "TABLE") {
			advance_token(p)
			clause.into_corresponding_of_table = true
		}
		clause.into_kind = .Corresponding
	} else {
		clause.into_kind = .Single
	}

	if p.curr_tok.kind == .LParen {
		clause.into_target = parse_select_into_paren_list(p)
		return clause
	}

	if p.curr_tok.kind == .At {
		advance_token(p)
		if check_keyword(p, "DATA") {
			clause.into_target = parse_data_inline_expr(p)
		} else {
			clause.into_target = parse_expr(p)
		}
		return clause
	}

	clause.into_target = parse_expr(p)
	return clause
}

// FETCH NEXT CURSOR cur INTO ... [PACKAGE SIZE n].
parse_fetch_cursor_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	fetch_tok := expect_keyword_token(p, "FETCH")
	expect_keyword_token(p, "NEXT")
	expect_keyword_token(p, "CURSOR")
	cursor_tok := expect_token(p, .Ident)
	cursor := ast.new_ident(cursor_tok)
	expect_keyword_token(p, "INTO")
	clause := parse_open_sql_into_clause(p)
	package_size: ^ast.Expr
	if check_keyword(p, "PACKAGE") {
		advance_token(p)
		expect_keyword_token(p, "SIZE")
		package_size = parse_expr(p)
	}
	period_tok := expect_token(p, .Period)
	stmt := ast.new(
		ast.Fetch_Cursor_Stmt,
		lexer.TextRange{fetch_tok.range.start, period_tok.range.end},
	)
	stmt.cursor = cursor
	stmt.into_kind = clause.into_kind
	stmt.into_corresponding_of_table = clause.into_corresponding_of_table
	stmt.into_target = clause.into_target
	stmt.package_size = package_size
	return stmt
}

// parse_select_stmt parses a SELECT Open SQL statement
// Syntax variations:
// - SELECT [SINGLE] fields FROM table [INTO target] [WHERE cond] [ORDER BY cols] [UP TO n ROWS].
// - SELECT [SINGLE] * FROM table [AS alias] [INTO target] [WHERE cond].
// - SELECT FROM table [AS alias] FIELDS field_list [WHERE cond] [INTO target].
// - SELECT ... INNER JOIN ... ON ... [WHERE cond] [INTO target].
// - SELECT ... FOR ALL ENTRIES IN itab WHERE ... [INTO target].
// - SELECT ... GROUP BY cols HAVING cond [INTO target].
// - SELECT ... WHERE ... . ENDSELECT. (period may end the last Open SQL clause line before ENDSELECT)
// - SELECT ... INTO TABLE / APPENDING ... / CORRESPONDING FIELDS OF TABLE ... . (no ENDSELECT)
// - SELECT ... FOR OPEN CURSOR (no INTO / APPENDING / TABLE INTO) — ends at '.' without ENDSELECT
parse_open_cursor_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	open_tok := expect_keyword_token(p, "OPEN")
	expect_keyword_token(p, "CURSOR")
	cursor_tok := expect_token(p, .Ident)
	cursor := ast.new_ident(cursor_tok)
	expect_keyword_token(p, "FOR")
	inner := parse_select_stmt(p)
	stmt := ast.new(
		ast.Open_Cursor_Stmt,
		lexer.TextRange{open_tok.range.start, inner.range.end},
	)
	stmt.cursor = cursor
	stmt.select_stmt = inner
	return stmt
}

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

	// Leading field list (stops at FROM, INTO, WHERE, UP TO, …)
	if !check_keyword(p, "FROM") {
		parse_select_field_list(p, stmt)
	}

	// FROM, FIELDS, INTO, WHERE, … may appear in different orders (e.g. INTO before FROM,
	// UP TO n ROWS before FROM, FOR ALL ENTRIES before/after INTO).
	parse_select_clauses(p, stmt)

	// In ABAP, periods may end individual Open SQL clause lines before ENDSELECT, e.g.
	//   WHERE ... .
	//   ENDSELECT.
	// Pragmas like ##WARN_OK sit before the closing period on the same Open SQL statement.
	skip_pragma(p)
	// Consume such a period then continue to ENDSELECT, loop body, or a true statement end.
	for p.curr_tok.kind == .Period {
		period_tok := expect_token(p, .Period)
		stmt.range.end = period_tok.range.end
		if check_keyword(p, "ENDSELECT") {
			endselect_tok := expect_keyword_token(p, "ENDSELECT")
			final_period := expect_token(p, .Period)
			stmt.range.end = final_period.range.end
			_ = endselect_tok
			return stmt
		}
		break
	}

	if stmt.is_single {
		return stmt
	}

	// One-shot reads into an internal table (or appending) end with '.'; only
	// SELECT ... INTO wa / CORRESPONDING FIELDS OF wa ... . ... ENDSELECT uses a loop body.
	// SELECT without INTO/APPENDING (e.g. after OPEN CURSOR ... FOR) ends at '.'.
	select_finishes_at_period :=
		stmt.into_kind == .Table ||
		stmt.appending ||
		(stmt.into_kind == .Corresponding && stmt.into_corresponding_of_table) ||
		(!stmt.appending && stmt.into_target == nil)
	if select_finishes_at_period {
		return stmt
	}

	// SELECT COUNT(*) / SUM(...) / ... without GROUP BY returns one row; it does not open
	// a SELECT ... ENDSELECT loop (unlike SELECT * INTO wa / ...).
	if select_finishes_as_single_aggregate_row(p, stmt) {
		return stmt
	}

	if p.curr_tok.kind == .EOF {
		return stmt
	}

	if check_keyword(p, "ENDSELECT") {
		endselect_tok := expect_keyword_token(p, "ENDSELECT")
		final_period := expect_token(p, .Period)
		stmt.range.end = final_period.range.end
		_ = endselect_tok
		return stmt
	}

	// SELECT loop with (optional) body statements until ENDSELECT
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
	return stmt
}

// select_finishes_as_single_aggregate_row is true when every selected field is an Open SQL
// aggregate (COUNT, SUM, …) and there is no GROUP BY, so the INTO target receives one row.
select_finishes_as_single_aggregate_row :: proc(p: ^Parser, stmt: ^ast.Select_Stmt) -> bool {
	if len(stmt.group_by) > 0 {
		return false
	}
	if stmt.for_all_entries != nil {
		return false
	}
	if len(stmt.fields) == 0 {
		return false
	}
	for f in stmt.fields {
		if !select_field_expr_is_sql_aggregate(p, f) {
			return false
		}
	}
	return true
}

select_field_expr_is_sql_aggregate :: proc(p: ^Parser, expr: ^ast.Expr) -> bool {
	e := expr
	for {
		#partial switch x in e.derived_expr {
		case ^ast.Named_Arg:
			e = x.value
			continue
		case ^ast.Call_Expr:
			if ident, ok := x.expr.derived_expr.(^ast.Ident); ok {
				u := to_upper(p.keyword_buffer[:], ident.name)
				return u == "COUNT" || u == "SUM" || u == "AVG" || u == "MIN" || u == "MAX"
			}
			return false
		}
		return false
	}
}

// parse_select_field_list parses the field list in a SELECT statement
parse_select_field_list :: proc(p: ^Parser, stmt: ^ast.Select_Stmt) {
	for p.curr_tok.kind != .EOF &&
	    !check_keyword(p, "FROM") &&
	    !check_keyword(p, "INTO") &&
	    !check_keyword(p, "APPENDING") {
		field := parse_select_field_expr(p)
		if field != nil {
			append(&stmt.fields, field)
		}

		// Check for comma or continue if next is still a field
		if allow_token(p, .Comma) {
			continue
		}

		// If next is a keyword that ends field list, break
		if check_keyword(p, "FROM") ||
		   check_keyword(p, "INTO") ||
		   check_keyword(p, "WHERE") ||
		   check_keyword(p, "ORDER") ||
		   check_keyword(p, "GROUP") ||
		   check_keyword(p, "FOR") ||
		   check_keyword(p, "UP") ||
		   check_keyword(p, "HAVING") ||
		   check_keyword(p, "APPENDING") {
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
		if upper == "SUM" ||
		   upper == "COUNT" ||
		   upper == "AVG" ||
		   upper == "MIN" ||
		   upper == "MAX" {
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

			call := ast.new(
				ast.Call_Expr,
				lexer.TextRange{func_tok.range.start, rparen_tok.range.end},
			)
			call.expr = func_ident
			args := make([]^ast.Expr, 1)
			args[0] = arg
			call.args = args
			call.derived_expr = call
			return call
		}
	}

	// Parse regular field expression (table~field, class=>static_attr chains)
	first_tok := expect_token(p, .Ident)
	root_ident := ast.new_ident(first_tok)
	expr := &root_ident.node

	for {
		if p.curr_tok.kind == .Tilde {
			tilde_tok := advance_token(p)
			field_tok := expect_token(p, .Ident)
			field_ident := ast.new_ident(field_tok)

			sel := ast.new(
				ast.Selector_Expr,
				lexer.TextRange{expr.range.start, field_tok.range.end},
			)
			sel.expr = expr
			sel.op = tilde_tok
			sel.field = &field_ident.node
			sel.derived_expr = sel
			expr = &sel.node
			continue
		}
		if p.curr_tok.kind == .FatArrow && !lexer.have_space_between(p.prev_tok, p.curr_tok) {
			fat_tok := advance_token(p)
			field_tok := expect_token(p, .Ident)
			field_ident := ast.new_ident(field_tok)

			sel := ast.new(
				ast.Selector_Expr,
				lexer.TextRange{expr.range.start, field_tok.range.end},
			)
			sel.expr = expr
			sel.op = fat_tok
			sel.field = &field_ident.node
			sel.derived_expr = sel
			expr = &sel.node
			continue
		}
		break
	}

	return expr
}

// parse_select_table_ref parses a table reference in FROM clause
parse_select_table_ref :: proc(p: ^Parser) -> ^ast.Expr {
	table_tok := expect_token(p, .Ident)
	return ast.new_ident(table_tok)
}

// skip_select_pragma_strings skips ABAP pragma fragments written as string literals, e.g.
// INNER JOIN t AS x "pragma text" ON ...
skip_select_pragma_strings :: proc(p: ^Parser) {
	for p.curr_tok.kind == .String {
		advance_token(p)
	}
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

		skip_select_pragma_strings(p)

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

	// col NOT IN itab (Open SQL)
	if check_keyword(p, "NOT") && check_keyword_ahead(p, "IN") {
		not_tok := advance_token(p)
		in_tok := advance_token(p)
		right := parse_select_in_rhs(p)

		in_bin := ast.new(ast.Binary_Expr, lexer.TextRange{left.range.start, right.range.end})
		in_bin.left = left
		in_bin.op = in_tok
		in_bin.right = right
		in_bin.derived_expr = in_bin

		unary := ast.new(ast.Unary_Expr, lexer.TextRange{left.range.start, right.range.end})
		unary.op = not_tok
		unary.expr = in_bin
		unary.derived_expr = unary
		return unary
	}

	// Check for comparison operators: =, <>, <, >, <=, >=, EQ, NE, LT, GT, LE, GE
	if p.curr_tok.kind == .Eq ||
	   p.curr_tok.kind == .Lt ||
	   p.curr_tok.kind == .Gt ||
	   p.curr_tok.kind == .Le ||
	   p.curr_tok.kind == .Ge ||
	   p.curr_tok.kind == .Ne {
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
	if check_keyword(p, "EQ") ||
	   check_keyword(p, "NE") ||
	   check_keyword(p, "LT") ||
	   check_keyword(p, "GT") ||
	   check_keyword(p, "LE") ||
	   check_keyword(p, "GE") {
		op_tok := advance_token(p)
		right := parse_select_operand(p)

		bin := ast.new(ast.Binary_Expr, lexer.TextRange{left.range.start, right.range.end})
		bin.left = left
		bin.op = op_tok
		bin.right = right
		bin.derived_expr = bin
		return bin
	}

	if check_keyword(p, "IN") {
		in_tok := advance_token(p)
		right := parse_select_in_rhs(p)

		bin := ast.new(ast.Binary_Expr, lexer.TextRange{left.range.start, right.range.end})
		bin.left = left
		bin.op = in_tok
		bin.right = right
		bin.derived_expr = bin
		return bin
	}

	return left
}

// parse_select_in_rhs parses the right-hand side of IN / NOT IN in Open SQL.
// Either a single operand (internal table, parenthesized subexpression, @host, …) or
// a parenthesized list of values: ( 'a', 'b', lv_x ).
parse_select_in_rhs :: proc(p: ^Parser) -> ^ast.Expr {
	if p.curr_tok.kind != .LParen {
		return parse_select_operand(p)
	}
	lparen_tok := advance_token(p)
	row_expr := ast.new(ast.Value_Row_Expr, lparen_tok.range)
	row_expr.args = make([dynamic]^ast.Expr)
	for p.curr_tok.kind != .RParen &&
	    p.curr_tok.kind != .EOF &&
	    p.curr_tok.kind != .Period {
		arg := parse_select_operand(p)
		if arg != nil {
			append(&row_expr.args, arg)
		}
		if !allow_token(p, .Comma) {
			break
		}
	}
	rparen_tok := expect_token(p, .RParen)
	row_expr.range.end = rparen_tok.range.end
	row_expr.derived_expr = row_expr
	return row_expr
}

// parse_select_operand parses an operand in a SELECT condition
parse_select_operand :: proc(p: ^Parser) -> ^ast.Expr {
	// Dynamic WHERE / grouping: (itab_condition), ( a = b AND c = d ), etc.
	if p.curr_tok.kind == .LParen {
		lparen_tok := advance_token(p)
		inner := parse_select_logical_expr(p)
		rparen_tok := expect_token(p, .RParen)
		paren := ast.new(
			ast.Paren_Expr,
			lexer.TextRange{lparen_tok.range.start, rparen_tok.range.end},
		)
		paren.expr = inner
		paren.derived_expr = paren
		return paren
	}

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

			sel := ast.new(
				ast.Selector_Expr,
				lexer.TextRange{expr.range.start, field_tok.range.end},
			)
			sel.expr = expr
			sel.op = minus_tok
			sel.field = &field_ident.node
			sel.derived_expr = sel
			expr = &sel.node
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
		skip_pragma(p)
		// Code inspector / pragma fragments as string literals between clauses, e.g.
		// WHERE (lt_cond) " #EC CI_DYNWHERE.
		skip_select_pragma_strings(p)
		if p.curr_tok.kind == .EOF || p.curr_tok.kind == .Period {
			break
		}
		if check_keyword(p, "ENDSELECT") {
			break
		} else if check_keyword(p, "FROM") {
			advance_token(p)
			stmt.from_table = parse_select_table_ref(p)
			if check_keyword(p, "AS") {
				advance_token(p)
				alias_tok := expect_token(p, .Ident)
				stmt.from_alias = ast.new_ident(alias_tok)
			}
			skip_select_pragma_strings(p)
			parse_select_joins(p, stmt)
		} else if check_keyword(p, "FIELDS") {
			advance_token(p)
			parse_select_field_list(p, stmt)
		} else if check_keyword(p, "APPENDING") {
			advance_token(p)
			stmt.appending = true
			parse_select_into_clause_body(p, stmt)
		} else if check_keyword(p, "INTO") {
			advance_token(p)
			parse_select_into_clause_body(p, stmt)
		} else if check_keyword(p, "FOR") {
			advance_token(p)
			expect_keyword_token(p, "ALL")
			expect_keyword_token(p, "ENTRIES")
			expect_keyword_token(p, "IN")
			stmt.for_all_entries = parse_select_operand(p)
			if p.curr_tok.kind == .LBracket {
				advance_token(p)
				expect_token(p, .RBracket)
			}
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
		} else if check_keyword(p, "UP") {
			advance_token(p)
			expect_keyword_token(p, "TO")
			stmt.up_to_rows = parse_expr(p)
			expect_keyword_token(p, "ROWS")
		} else {
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
		if check_keyword(p, "INTO") ||
		   check_keyword(p, "WHERE") ||
		   check_keyword(p, "UP") ||
		   check_keyword(p, "FOR") ||
		   check_keyword(p, "GROUP") ||
		   check_keyword(p, "HAVING") ||
		   check_keyword(p, "FROM") ||
		   check_keyword(p, "FIELDS") ||
		   check_keyword(p, "APPENDING") ||
		   check_keyword(p, "ENDSELECT") {
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
		if check_keyword(p, "INTO") ||
		   check_keyword(p, "WHERE") ||
		   check_keyword(p, "HAVING") ||
		   check_keyword(p, "ORDER") ||
		   check_keyword(p, "UP") ||
		   check_keyword(p, "FROM") ||
		   check_keyword(p, "FOR") ||
		   check_keyword(p, "ENDSELECT") {
			break
		}

		// If next token looks like another field, continue without comma
		if p.curr_tok.kind == .Ident {
			continue
		}

		break
	}
}

// parse_select_into_paren_list parses INTO ( wa1, wa2, ... ).
parse_select_into_paren_list :: proc(p: ^Parser) -> ^ast.Expr {
	lparen_tok := expect_token(p, .LParen)
	row_expr := ast.new(ast.Value_Row_Expr, lparen_tok.range)
	row_expr.args = make([dynamic]^ast.Expr)

	// INTO ( @host1, wa2, ... ) uses Open SQL host-variable rules (@); parse_expr does not treat `@`.
	for p.curr_tok.kind != .RParen &&
	    p.curr_tok.kind != .EOF &&
	    p.curr_tok.kind != .Period {
		arg := parse_select_operand(p)
		if arg != nil {
			append(&row_expr.args, arg)
		}
		if !allow_token(p, .Comma) {
			break
		}
	}

	rparen_tok := expect_token(p, .RParen)
	row_expr.range.end = rparen_tok.range.end
	row_expr.derived_expr = row_expr
	return row_expr
}

// parse_select_into_clause_body parses the part after INTO or APPENDING (without consuming that keyword).
parse_select_into_clause_body :: proc(p: ^Parser, stmt: ^ast.Select_Stmt) {
	clause := parse_open_sql_into_clause(p)
	stmt.into_kind = clause.into_kind
	stmt.into_corresponding_of_table = clause.into_corresponding_of_table
	stmt.into_target = clause.into_target
}
