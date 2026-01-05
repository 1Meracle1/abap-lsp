package lang_parser

import "../ast"
import "../lexer"

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
	return read_stmt
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
	return stmt
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
	return append_stmt
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