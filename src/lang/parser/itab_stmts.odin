package lang_parser

import "../ast"
import "../lexer"

// parse_read_table_key_component_assignments parses field = value pairs after WITH KEY / COMPONENTS.
parse_read_table_key_component_assignments :: proc(p: ^Parser, key: ^ast.Read_Table_Key) {
	for p.curr_tok.kind == .Ident {
		// Check if it's a keyword that ends the key specification
		if check_keyword(p, "INTO") ||
		   check_keyword(p, "ASSIGNING") ||
		   check_keyword(p, "TRANSPORTING") ||
		   check_keyword(p, "USING") ||
		   check_keyword(p, "BINARY") ||
		   check_keyword(p, "WITH") {
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
}

// parse_read_table_key parses the key specification after "WITH KEY" or "WITH TABLE KEY".
// Syntax:
// - ... KEY comp1 = val1 ... (free key; component names only)
// - ... KEY COMPONENTS comp1 = val1 ... (primary / unnamed table key)
// - ... KEY key_name COMPONENTS comp1 = val1 ... (named secondary table key)
parse_read_table_key :: proc(p: ^Parser) -> ^ast.Read_Table_Key {
	if check_keyword(p, "COMPONENTS") {
		key := new(ast.Read_Table_Key)
		key.components = make([dynamic]^ast.Named_Arg)
		advance_token(p)
		parse_read_table_key_component_assignments(p, key)
		return key
	}

	if p.curr_tok.kind == .Ident {
		saved_prev := p.prev_tok
		saved_curr := p.curr_tok
		saved_pos := p.l.pos
		saved_read_pos := p.l.read_pos
		saved_ch := p.l.ch

		name_tok := advance_token(p)
		if check_keyword(p, "COMPONENTS") {
			key := new(ast.Read_Table_Key)
			key.components = make([dynamic]^ast.Named_Arg)
			advance_token(p) // consume COMPONENTS
			key.key_name = ast.new_ident(name_tok)
			parse_read_table_key_component_assignments(p, key)
			return key
		}

		p.prev_tok = saved_prev
		p.curr_tok = saved_curr
		p.l.pos = saved_pos
		p.l.read_pos = saved_read_pos
		p.l.ch = saved_ch
	}

	key := new(ast.Read_Table_Key)
	key.components = make([dynamic]^ast.Named_Arg)
	parse_read_table_key_component_assignments(p, key)
	return key
}

// parse_read_table_with_table_key_spec parses the key part after "WITH TABLE KEY".
parse_read_table_with_table_key_spec :: proc(p: ^Parser) -> ^ast.Read_Table_Key {
	return parse_read_table_key(p)
}

// READ TABLE statement parser
// Syntax variations:
// - READ TABLE itab WITH TABLE KEY [key_name] COMPONENTS field1 = val1 ... | WITH TABLE KEY field1 = val1 ...
// - READ TABLE itab WITH KEY [key_name COMPONENTS | COMPONENTS] field1 = val1 ... | WITH KEY field1 = val1 ...
//   [INTO wa | ASSIGNING <fs> | TRANSPORTING NO FIELDS].
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
					read_stmt.key = parse_read_table_with_table_key_spec(p)
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
		} else if check_keyword(p, "BINARY") {
			advance_token(p) // consume BINARY
			expect_keyword_token(p, "SEARCH")
			read_stmt.binary_search = true
		} else {
			// Unknown clause, break out
			break
		}
	}

	period_tok := expect_token(p, .Period)
	read_stmt.range.end = period_tok.range.end
	return read_stmt
}

// DESCRIBE TABLE statement parser
// Syntax:
// - DESCRIBE TABLE itab LINES lv_lines.
// - DESCRIBE TABLE itab LINES DATA(lv_lines).
parse_describe_table_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	describe_tok := expect_keyword_token(p, "DESCRIBE")
	expect_keyword_token(p, "TABLE")

	describe_stmt := ast.new(ast.Describe_Table_Stmt, describe_tok.range)
	describe_stmt.table = parse_expr(p)

	expect_keyword_token(p, "LINES")
	if check_keyword(p, "DATA") {
		describe_stmt.lines_target = parse_data_inline_expr(p)
	} else {
		describe_stmt.lines_target = parse_expr(p)
	}

	period_tok := expect_token(p, .Period)
	describe_stmt.range.end = period_tok.range.end
	return describe_stmt
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
	} else if check_keyword(p, "TABLE") {
		// DELETE TABLE itab FROM wa.
		advance_token(p) // consume TABLE
		stmt.kind = .Table_From
		stmt.target = parse_expr(p)
		expect_keyword_token(p, "FROM")
		stmt.from_source = parse_expr(p)
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
		} else if check_keyword(p, "FROM") {
			// DELETE dbtab FROM TABLE itab. (Open SQL / DB delete from internal table)
			advance_token(p) // consume FROM
			expect_keyword_token(p, "TABLE")
			stmt.kind = .Db_From_Table
			stmt.from_source = parse_expr(p)
		}
	}

	period_tok := expect_token(p, .Period)
	stmt.range.end = period_tok.range.end
	return stmt
}

// INSERT statement parser
// Syntax variations:
// - INSERT VALUE #( ... ) INTO TABLE itab.
// - INSERT wa INTO itab [INDEX idx].
// - INSERT LINES OF itab_src INTO TABLE itab_tgt.
// - INSERT LINES OF itab_src INTO itab_tgt [INDEX idx].
// - INSERT INTO target VALUES wa.
// - INSERT target FROM wa.
// - INSERT target FROM TABLE itab.
parse_insert_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	insert_tok := expect_keyword_token(p, "INSERT")

	insert_stmt := ast.new(ast.Insert_Stmt, insert_tok.range)

	// INSERT LINES OF ... INTO [TABLE] ...
	if check_keyword(p, "LINES") {
		advance_token(p) // consume LINES
		expect_keyword_token(p, "OF")
		insert_stmt.source = parse_expr(p)
		expect_keyword_token(p, "INTO")
		if check_keyword(p, "TABLE") {
			advance_token(p) // consume TABLE
			insert_stmt.target = parse_expr(p)
			insert_stmt.kind = .Lines_Of_Into_Table
		} else {
			insert_stmt.target = parse_expr(p)
			insert_stmt.kind = .Lines_Of_Into_Itab
			if check_keyword(p, "INDEX") {
				advance_token(p)
				insert_stmt.index_expr = parse_expr(p)
			}
		}
		period_tok := expect_token(p, .Period)
		insert_stmt.range.end = period_tok.range.end
		return insert_stmt
	}

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
			advance_token(p) // consume INTO
			if check_keyword(p, "TABLE") {
				// INSERT expr INTO TABLE itab form
				advance_token(p) // consume TABLE
				insert_stmt.value_expr = value_or_target
				insert_stmt.target = parse_expr(p)
				insert_stmt.kind = .Into_Table
			} else {
				// INSERT expr INTO itab [INDEX idx].
				insert_stmt.value_expr = value_or_target
				insert_stmt.target = parse_expr(p)
				insert_stmt.kind = .Into_Itab
				if check_keyword(p, "INDEX") {
					advance_token(p)
					insert_stmt.index_expr = parse_expr(p)
				}
			}
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

	stable := false
	if check_keyword(p, "STABLE") {
		advance_token(p)
		stable = true
	}

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
	sort_stmt.stable = stable
	sort_stmt.cols_by = cols_by
	sort_stmt.order = order_kind
	return sort_stmt
}

// APPEND statement parser
// Syntax variations:
// - APPEND expr TO itab.
// - APPEND INITIAL LINE TO itab [ASSIGNING <fs>].
// - APPEND LINES OF itab2 TO itab1.
// - APPEND LINES OF itab_src [FROM idx1 TO idx2] TO itab_tgt.
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
		append_stmt.kind = .Lines_Of
		// Optional line range: FROM idx1 TO idx2 (then final TO names the target table)
		if check_keyword(p, "FROM") {
			advance_token(p)
			append_stmt.lines_from = parse_expr(p)
			expect_keyword_token(p, "TO")
			append_stmt.lines_to = parse_expr(p)
		}
		expect_keyword_token(p, "TO")
		append_stmt.target = parse_expr(p)

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
			if check_keyword(p, "WITH") {
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
	with_expr: ^ast.Expr
	if check_keyword(p, "WITH") {
		advance_token(p)
		with_expr = parse_expr(p)
	}
	end_tok := expect_token(p, .Period)
	clear_stmt := ast.new(ast.Clear_Stmt, clear_tok, end_tok)
	clear_stmt.exprs = exprs
	clear_stmt.with_expr = with_expr
	return clear_stmt
}

// parse_unassign_stmt parses UNASSIGN <fs>. or UNASSIGN: <fs1>, <fs2>, ...
parse_unassign_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	unassign_tok := advance_token(p)
	targets := make([dynamic]^ast.Expr)
	if allow_token(p, .Colon) {
		for p.curr_tok.kind != .EOF {
			fs := parse_field_symbol_ref(p)
			if fs != nil {
				append(&targets, fs)
			}
			if p.curr_tok.kind == .Period {
				break
			}
			if allow_token(p, .Comma) {
				continue
			}
			error(p, p.curr_tok.range, "expected ',' or '.'")
			break
		}
	} else {
		fs := parse_field_symbol_ref(p)
		append(&targets, fs)
	}
	end_tok := expect_token(p, .Period)
	stmt := ast.new(ast.Unassign_Stmt, unassign_tok, end_tok)
	stmt.targets = targets
	return stmt
}

// parse_move_corresponding_stmt parses MOVE-CORRESPONDING source TO target [KEEPING TARGET LINES].
// MOVE-CORRESPONDING must already be consumed by check_hyphenated_keyword; first_tok is the leading MOVE token.
parse_move_corresponding_stmt :: proc(p: ^Parser, first_tok: lexer.Token) -> ^ast.Stmt {
	source := parse_expr(p)
	expect_keyword_token(p, "TO")
	target := parse_expr(p)
	keeping_target_lines := false
	if check_keyword(p, "KEEPING") {
		advance_token(p)
		expect_keyword_token(p, "TARGET")
		expect_keyword_token(p, "LINES")
		keeping_target_lines = true
	}
	skip_pragma(p)
	period_tok := expect_token(p, .Period)
	stmt := ast.new(ast.Move_Corresponding_Stmt, first_tok, period_tok)
	stmt.source = source
	stmt.target = target
	stmt.keeping_target_lines = keeping_target_lines
	return stmt
}