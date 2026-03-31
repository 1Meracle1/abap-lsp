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
			// WHEN allows multiple alternatives: WHEN a OR b OR c.
			branch.expr = parse_logical_expr(p)
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

// DO [. | n TIMES.] ... ENDDO.
parse_do_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	do_tok := expect_keyword_token(p, "DO")
	stmt := ast.new(ast.Do_Stmt, do_tok.range)
	stmt.body = make([dynamic]^ast.Stmt)

	if p.curr_tok.kind == .Period {
		expect_token(p, .Period)
	} else {
		stmt.times = parse_expr(p)
		expect_keyword_token(p, "TIMES")
		expect_token(p, .Period)
	}

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "ENDDO") {
			break
		}
		s := parse_stmt(p)
		if s != nil {
			append(&stmt.body, s)
		}
	}

	enddo_tok := expect_keyword_token(p, "ENDDO")
	period_tok := expect_token(p, .Period)
	stmt.range.end = period_tok.range.end
	_ = enddo_tok
	return stmt
}

parse_continue_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	tok := expect_keyword_token(p, "CONTINUE")
	period_tok := expect_token(p, .Period)
	return ast.new(ast.Continue_Stmt, tok, period_tok)
}

parse_exit_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	tok := expect_keyword_token(p, "EXIT")
	period_tok := expect_token(p, .Period)
	return ast.new(ast.Exit_Stmt, tok, period_tok)
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

// AT FIRST|LAST|NEW|END OF ... . ... ENDAT.  (precondition: leading AT already consumed)
parse_loop_at_control_stmt :: proc(p: ^Parser, at_tok: lexer.Token, kind: ast.Loop_At_Control_Kind, field: ^ast.Expr) -> ^ast.Stmt {
	expect_token(p, .Period)

	stmt := ast.new(ast.Loop_At_Control_Stmt, at_tok.range)
	stmt.kind = kind
	stmt.field = field
	stmt.body = make([dynamic]^ast.Stmt)

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "ENDAT") {
			break
		}
		s := parse_stmt(p)
		if s != nil {
			append(&stmt.body, s)
		}
	}

	endat_tok := expect_keyword_token(p, "ENDAT")
	period_tok := expect_token(p, .Period)
	stmt.range.end = period_tok.range.end
	stmt.derived_stmt = stmt
	_ = endat_tok

	return stmt
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

// COMMIT WORK.
parse_commit_work_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	commit_tok := expect_keyword_token(p, "COMMIT")
	expect_keyword_token(p, "WORK")
	period_tok := expect_token(p, .Period)
	return ast.new(ast.Commit_Work_Stmt, commit_tok, period_tok)
}

// ROLLBACK WORK.
parse_rollback_work_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	rb_tok := expect_keyword_token(p, "ROLLBACK")
	expect_keyword_token(p, "WORK")
	period_tok := expect_token(p, .Period)
	return ast.new(ast.Rollback_Work_Stmt, rb_tok, period_tok)
}

parse_modify_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	modify_tok := advance_token(p)
	if check_keyword(p, "SCREEN") {
		advance_token(p)
		expect_token(p, .Period)
		stmt := ast.new(ast.Modify_Screen_Stmt, modify_tok, p.curr_tok)
		return stmt
	}
	// MODIFY dbtab FROM wa.
	target := parse_expr(p)
	expect_keyword_token(p, "FROM")
	source := parse_expr(p)
	period_tok := expect_token(p, .Period)
	stmt := ast.new(ast.Modify_From_Stmt, modify_tok, period_tok)
	stmt.target = target
	stmt.source = source
	return stmt
}

// Syntax:
// - GET TIME STAMP FIELD DATA(lv_current_ts).
// - GET TIME STAMP FIELD lv_ts.
parse_get_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	get_tok := expect_keyword_token(p, "GET")
	expect_keyword_token(p, "TIME")
	expect_keyword_token(p, "STAMP")
	expect_keyword_token(p, "FIELD")

	stmt := ast.new(ast.Get_Time_Stamp_Stmt, get_tok.range)
	if check_keyword(p, "DATA") {
		stmt.target = parse_data_inline_expr(p)
	} else {
		stmt.target = parse_expr(p)
	}

	period_tok := expect_token(p, .Period)
	stmt.range.end = period_tok.range.end
	stmt.derived_stmt = stmt
	return stmt
}

// CONVERT DATE dat TIME tim INTO TIME STAMP tstamp [TIME ZONE tz].
parse_convert_date_time_to_time_stamp_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	convert_tok := expect_keyword_token(p, "CONVERT")
	expect_keyword_token(p, "DATE")
	date_expr := parse_expr(p)
	expect_keyword_token(p, "TIME")
	time_expr := parse_expr(p)
	expect_keyword_token(p, "INTO")
	expect_keyword_token(p, "TIME")
	expect_keyword_token(p, "STAMP")
	stamp_target: ^ast.Expr
	if check_keyword(p, "DATA") {
		stamp_target = parse_data_inline_expr(p)
	} else {
		stamp_target = parse_expr(p)
	}
	time_zone: ^ast.Expr = nil
	if check_keyword(p, "TIME") {
		advance_token(p)
		expect_keyword_token(p, "ZONE")
		time_zone = parse_expr(p)
	}
	period_tok := expect_token(p, .Period)
	stmt := ast.new(ast.Convert_Date_Time_To_Time_Stamp_Stmt, convert_tok, period_tok)
	stmt.date = date_expr
	stmt.time = time_expr
	stmt.stamp = stamp_target
	stmt.time_zone = time_zone
	stmt.derived_stmt = stmt
	return stmt
}

// CONVERT TIME STAMP stamp [TIME ZONE tz] INTO DATE date TIME time.
parse_convert_time_stamp_to_date_time_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	convert_tok := expect_keyword_token(p, "CONVERT")
	expect_keyword_token(p, "TIME")
	expect_keyword_token(p, "STAMP")
	stamp_expr := parse_expr(p)
	time_zone: ^ast.Expr = nil
	if check_keyword(p, "TIME") {
		advance_token(p)
		expect_keyword_token(p, "ZONE")
		time_zone = parse_expr(p)
	}
	expect_keyword_token(p, "INTO")
	expect_keyword_token(p, "DATE")
	date_target: ^ast.Expr
	if check_keyword(p, "DATA") {
		date_target = parse_data_inline_expr(p)
	} else {
		date_target = parse_expr(p)
	}
	expect_keyword_token(p, "TIME")
	time_target: ^ast.Expr
	if check_keyword(p, "DATA") {
		time_target = parse_data_inline_expr(p)
	} else {
		time_target = parse_expr(p)
	}
	period_tok := expect_token(p, .Period)
	stmt := ast.new(ast.Convert_Time_Stamp_To_Date_Time_Stmt, convert_tok, period_tok)
	stmt.stamp = stamp_expr
	stmt.time_zone = time_zone
	stmt.date = date_target
	stmt.time = time_target
	stmt.derived_stmt = stmt
	return stmt
}

// GET BADI badi_ref [FILTERS name = expr ...].
parse_get_badi_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	get_tok := expect_keyword_token(p, "GET")
	expect_keyword_token(p, "BADI")
	stmt := ast.new(ast.Get_Badi_Stmt, get_tok.range)
	stmt.badi_ref = parse_expr(p)
	if check_keyword(p, "FILTERS") {
		advance_token(p)
		for p.curr_tok.kind != .EOF && p.curr_tok.kind != .Period {
			if p.curr_tok.kind != .Ident {
				break
			}
			name_tok := advance_token(p)
			expect_token(p, .Eq)
			value := parse_expr(p)
			named_arg := ast.new(
				ast.Named_Arg,
				lexer.TextRange{name_tok.range.start, value.range.end},
			)
			named_arg.name = ast.new_ident(name_tok)
			named_arg.value = value
			named_arg.derived_expr = named_arg
			append(&stmt.filters, named_arg)
		}
	}
	period_tok := expect_token(p, .Period)
	stmt.range.end = period_tok.range.end
	return stmt
}

parse_try_into_target :: proc(p: ^Parser) -> ^ast.Expr {
	if check_keyword(p, "DATA") {
		return parse_data_inline_expr(p)
	}
	if check_keyword(p, "FINAL") {
		return parse_final_inline_expr(p)
	}
	return parse_expr(p)
}

parse_try_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	try_tok := expect_keyword_token(p, "TRY")
	expect_token(p, .Period)

	try_stmt := ast.new(ast.Try_Stmt, try_tok.range)
	try_stmt.body = make([dynamic]^ast.Stmt)
	try_stmt.catch_branches = make([dynamic]^ast.Try_Catch_Branch)
	try_stmt.derived_stmt = try_stmt

	for p.curr_tok.kind != .EOF {
		if check_keyword(p, "CATCH") ||
		   check_keyword(p, "CLEANUP") ||
		   check_keyword(p, "ENDTRY") {
			break
		}
		stmt := parse_stmt(p)
		if stmt != nil {
			append(&try_stmt.body, stmt)
		}
	}

	for check_keyword(p, "CATCH") {
		catch_tok := expect_keyword_token(p, "CATCH")
		catch_branch := ast.new(ast.Try_Catch_Branch, catch_tok.range)
		catch_branch.class_refs = make([dynamic]^ast.Expr)
		catch_branch.body = make([dynamic]^ast.Stmt)
		catch_branch.derived = catch_branch

		if check_keyword(p, "BEFORE") {
			advance_token(p)
			expect_keyword_token(p, "UNWIND")
			catch_branch.before_unwind = true
		}

		// Optional chaining colon: CATCH: cx_class ... (same idea as DATA:, WRITE:, etc.)
		allow_token(p, .Colon)

		for p.curr_tok.kind != .EOF {
			skip_pragma(p)
			if check_keyword(p, "INTO") || p.curr_tok.kind == .Period {
				break
			}
			pos_before := p.curr_tok.range.start
			class_ref := parse_expr(p)
			if class_ref == nil {
				break
			}
			// parse_expr can yield Bad_Expr without consuming (e.g. stray ':'); avoid spinning.
			if p.curr_tok.range.start == pos_before {
				error(
					p,
					p.curr_tok.range,
					"expected exception class reference after CATCH",
				)
				if p.curr_tok.kind != .Period && !check_keyword(p, "INTO") {
					advance_token(p)
				}
				break
			}
			append(&catch_branch.class_refs, class_ref)
			skip_pragma(p)
			allow_token(p, .Comma)
			skip_pragma(p)
		}

		if check_keyword(p, "INTO") {
			advance_token(p)
			catch_branch.into_target = parse_try_into_target(p)
		}

		period_tok := expect_token(p, .Period)
		catch_branch.range.end = period_tok.range.end

		for p.curr_tok.kind != .EOF {
			if check_keyword(p, "CATCH") ||
			   check_keyword(p, "CLEANUP") ||
			   check_keyword(p, "ENDTRY") {
				break
			}
			stmt := parse_stmt(p)
			if stmt != nil {
				append(&catch_branch.body, stmt)
			}
		}

		catch_branch.range.end = p.prev_tok.range.end
		append(&try_stmt.catch_branches, catch_branch)
	}

	if check_keyword(p, "CLEANUP") {
		cleanup_tok := expect_keyword_token(p, "CLEANUP")
		cleanup_branch := ast.new(ast.Try_Cleanup_Branch, cleanup_tok.range)
		cleanup_branch.body = make([dynamic]^ast.Stmt)
		cleanup_branch.derived = cleanup_branch

		if check_keyword(p, "INTO") {
			advance_token(p)
			cleanup_branch.into_target = parse_try_into_target(p)
		}

		period_tok := expect_token(p, .Period)
		cleanup_branch.range.end = period_tok.range.end

		for p.curr_tok.kind != .EOF {
			if check_keyword(p, "ENDTRY") {
				break
			}
			stmt := parse_stmt(p)
			if stmt != nil {
				append(&cleanup_branch.body, stmt)
			}
		}

		cleanup_branch.range.end = p.prev_tok.range.end
		try_stmt.cleanup_branch = cleanup_branch
	}

	endtry_tok := expect_keyword_token(p, "ENDTRY")
	period_tok := expect_token(p, .Period)
	try_stmt.range.end = period_tok.range.end
	_ = endtry_tok

	return try_stmt
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