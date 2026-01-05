package lang_parser

import "../ast"
import "../lexer"

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