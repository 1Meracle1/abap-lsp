package abap_frontend_parser

import "../ast"
import "../tokenizer"

import "base:intrinsics"
import "core:mem"
import "core:strings"

control_stmt_starts :: proc(p: ^Parser) -> bool {
	return(
		at_keyword(p, "IF") ||
		at_keyword(p, "CASE") ||
		at_keyword(p, "WHILE") ||
		at_keyword(p, "DO") ||
		(at_keyword(p, "LOOP") && at_keyword_index(p, p.index + 1, "AT")) ||
		at_group_stmt_starts(p) ||
		at_keyword(p, "TRY") \
	)
}

structural_stmt_starts :: proc(p: ^Parser) -> bool {
	return(
		structural_block_keyword_starts(p, "CLASS") ||
		structural_block_keyword_starts(p, "INTERFACE") ||
		structural_block_keyword_starts(p, "METHOD") ||
		structural_block_keyword_starts(p, "FORM") ||
		structural_block_keyword_starts(p, "FUNCTION") ||
		structural_block_keyword_starts(p, "MODULE") ||
		event_block_starts(p) ||
		at_keyword_phrase(p, "ENHANCEMENT-SECTION") ||
		at_keyword(p, "ENHANCEMENT") ||
		at_keyword_phrase(p, "TEST-SEAM") ||
		at_keyword_phrase(p, "TEST-INJECTION") \
	)
}

structural_block_keyword_starts :: proc(p: ^Parser, keyword: string) -> bool {
	next := next_token_kind(p, 1)
	return at_keyword(p, keyword) && next != .Minus && next != .Eq && next != .QuestionEq
}

parse_control_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	if at_keyword(p, "IF") {
		return parse_if_stmt(p)
	}
	if at_keyword(p, "CASE") {
		return parse_case_stmt(p)
	}
	if at_keyword(p, "WHILE") {
		return parse_while_stmt(p)
	}
	if at_keyword(p, "DO") {
		return parse_do_stmt(p)
	}
	if at_keyword(p, "LOOP") {
		return parse_loop_stmt(p)
	}
	if at_group_stmt_starts(p) {
		return parse_at_stmt(p)
	}
	return parse_try_stmt(p)
}

parse_required_expr_after :: proc(p: ^Parser, message: string) -> ^ast.Expr {
	if !expr_lead_token(current_token(p)) {
		error_current(p, message)
		return nil
	}
	return parse_expr(p)
}

parse_control_condition :: proc(p: ^Parser, message: string) -> ^ast.Expr {
	if !expr_lead_token(current_token(p)) {
		error_current(p, message)
		return nil
	}
	return parse_logical_expr(p)
}

parse_when_operand :: proc(p: ^Parser) -> ^ast.Expr {
	if !expr_lead_token(current_token(p)) {
		error_current(p, "syntax error: expected expression after WHEN")
		return nil
	}
	start := p.index
	expr := parse_expr(p)
	if expr == nil {
		return nil
	}
	if invalid_when_operand_between(p, start, p.index) {
		error(p, expr.range, "syntax error: invalid operand after WHEN")
		return nil
	}
	return expr
}

invalid_when_operand_between :: proc(p: ^Parser, start, end: int) -> bool {
	paren := 0
	bracket := 0
	brace := 0
	seen_operand_token := false
	for i in start ..< end {
		tok := p.tokens[i]
		top := paren == 0 && bracket == 0 && brace == 0
		if top {
			#partial switch tok.kind {
			case .Eq, .Lt, .Gt, .Le, .Ge, .Ne, .Star, .Slash, .Ampersand, .LBracket:
				return true
			case .Plus, .Minus:
				if seen_operand_token && !when_dash_is_selector(p, i, start, end) {
					return true
				}
			}
			if token_is_keyword(p, tok, "AND") ||
			   token_is_keyword(p, tok, "BETWEEN") ||
			   token_is_keyword(p, tok, "DIV") ||
			   token_is_keyword(p, tok, "MOD") ||
			   token_is_keyword(p, tok, "TO") {
				return true
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
		seen_operand_token = true
	}
	return false
}

when_dash_is_selector :: proc(p: ^Parser, index, start, end: int) -> bool {
	return(
		index > start &&
		index + 1 < end &&
		p.tokens[index].kind == .Minus &&
		tokens_touch(p.tokens[index - 1], p.tokens[index]) &&
		tokens_touch(p.tokens[index], p.tokens[index + 1]) &&
		(p.tokens[index + 1].kind == .Ident || p.tokens[index + 1].kind == .Number) \
	)
}

validate_loop_header_tail :: proc(p: ^Parser) -> bool {
	for current_token(p).kind != .Period && current_token(p).kind != .Eof {
		if allow_keyword(p, "INTO") {
			if !expr_lead_token(current_token(p)) {
				error_current(p, "syntax error: expected target after INTO")
				return false
			}
			parse_expr(p)
			continue
		}
		if allow_keyword(p, "WHERE") {
			if !expr_lead_token(current_token(p)) {
				error_current(p, "syntax error: expected expression after WHERE")
				return false
			}
			parse_logical_expr(p)
			continue
		}
		bump_token(p)
	}
	return true
}

parse_if_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "IF")
	condition := parse_control_condition(p, "syntax error: expected condition after IF")
	if condition == nil {
		return nil
	}
	period := expect_token_message(p, .Period, "syntax error: expected '.' after IF condition")
	if period.kind != .Period {
		return nil
	}

	stmt := ast.new(ast.If_Stmt, start.range, p.allocator)
	stmt.condition = condition
	stmt.body = parse_stmt_list_until(p, []string{"ELSEIF", "ELSE", "ENDIF"})
	stmt.elseif_clauses = make([dynamic]^ast.Elseif_Clause, 0, 2, p.allocator)

	for at_keyword(p, "ELSEIF") {
		clause := parse_elseif_clause(p)
		if clause == nil {
			return nil
		}
		append(&stmt.elseif_clauses, clause)
	}

	if at_keyword(p, "ELSE") {
		stmt.else_clause = parse_else_clause(p)
		if stmt.else_clause == nil {
			return nil
		}
	}

	end := expect_keyword_message(p, "ENDIF", "syntax error: expected ENDIF")
	if !token_is_keyword(p, end, "ENDIF") {
		return nil
	}
	period = expect_token_message(p, .Period, "syntax error: expected '.' after ENDIF")
	if period.kind != .Period {
		return nil
	}
	stmt.range = tokenizer.text_range(start.range.start, period.range.end)
	return stmt
}

parse_elseif_clause :: proc(p: ^Parser) -> ^ast.Elseif_Clause {
	start := expect_keyword(p, "ELSEIF")
	condition := parse_control_condition(p, "syntax error: expected condition after ELSEIF")
	if condition == nil {
		return nil
	}
	period := expect_token_message(
		p,
		.Period,
		"syntax error: expected '.' after ELSEIF condition",
	)
	if period.kind != .Period {
		return nil
	}
	clause, _ := mem.new(ast.Elseif_Clause, p.allocator)
	clause.condition = condition
	clause.body = parse_stmt_list_until(p, []string{"ELSEIF", "ELSE", "ENDIF"})
	clause.range = tokenizer.text_range(
		start.range.start,
		previous_stmt_end(clause.body, period.range.end),
	)
	return clause
}

parse_else_clause :: proc(p: ^Parser) -> ^ast.Else_Clause {
	start := expect_keyword(p, "ELSE")
	period := expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	clause, _ := mem.new(ast.Else_Clause, p.allocator)
	clause.body = parse_stmt_list_until(p, []string{"ENDIF"})
	clause.range = tokenizer.text_range(
		start.range.start,
		previous_stmt_end(clause.body, period.range.end),
	)
	return clause
}

parse_case_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "CASE")
	is_type_of := false
	if allow_keyword(p, "TYPE") {
		is_type_of = true
		if !allow_keyword(p, "OF") {
			error_current(p, "syntax error: expected keyword")
			return nil
		}
	}
	expr := parse_required_expr_after(p, "syntax error: expected expression after CASE")
	if expr == nil {
		return nil
	}
	period := expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	stmt := ast.new(ast.Case_Stmt, start.range, p.allocator)
	stmt.expr = expr
	stmt.is_type_of = is_type_of
	stmt.whens = make([dynamic]^ast.When_Clause, 0, 2, p.allocator)
	stmt.recovery = make([dynamic]^ast.Stmt, 0, 1, p.allocator)
	for at_keyword(p, "WHEN") {
		mark := mark_statement_start(p)
		when_clause := parse_when_clause(p, is_type_of)
		if when_clause == nil {
			recover_to_statement_boundary(p, []string{"WHEN", "ENDCASE"}, true)
			append(&stmt.recovery, build_invalid_statement(p, mark))
			body := parse_stmt_list_until(p, []string{"WHEN", "ENDCASE"})
			for invalid_body_stmt in body {
				append(&stmt.recovery, invalid_body_stmt)
			}
			continue
		}
		append(&stmt.whens, when_clause)
	}
	end := expect_keyword_message(p, "ENDCASE", "syntax error: expected ENDCASE")
	if !token_is_keyword(p, end, "ENDCASE") {
		return nil
	}
	period = expect_token_message(p, .Period, "syntax error: expected '.' after ENDCASE")
	if period.kind != .Period {
		return nil
	}
	stmt.range = tokenizer.text_range(start.range.start, period.range.end)
	return stmt
}

parse_when_clause :: proc(p: ^Parser, is_type_of: bool) -> ^ast.When_Clause {
	start := expect_keyword(p, "WHEN")
	clause, _ := mem.new(ast.When_Clause, p.allocator)
	clause.operands = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	if at_keyword(p, "OTHERS") {
		clause.is_others = true
		bump_token(p)
	} else {
		if is_type_of {
			allow_keyword(p, "TYPE")
		}
		operand := parse_when_operand(p)
		if operand == nil {
			return nil
		}
		append(&clause.operands, operand)
		for allow_keyword(p, "OR") {
			next := parse_when_operand(p)
			if next == nil {
				return nil
			}
			append(&clause.operands, next)
		}
		if is_type_of && allow_keyword(p, "INTO") {
			target := parse_expr(p)
			if target != nil {
				append(&clause.operands, target)
			}
		}
	}
	period := expect_token_message(p, .Period, "syntax error: expected '.' after WHEN")
	if period.kind != .Period {
		return nil
	}
	clause.body = parse_stmt_list_until(p, []string{"WHEN", "ENDCASE"})
	clause.range = tokenizer.text_range(
		start.range.start,
		previous_stmt_end(clause.body, period.range.end),
	)
	return clause
}

parse_while_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "WHILE")
	condition := parse_control_condition(p, "syntax error: expected condition after WHILE")
	if condition == nil {
		return nil
	}
	period := expect_token_message(
		p,
		.Period,
		"syntax error: expected '.' after WHILE condition",
	)
	if period.kind != .Period {
		return nil
	}
	stmt := ast.new(ast.While_Stmt, start.range, p.allocator)
	stmt.condition = condition
	stmt.body = parse_stmt_list_until(p, []string{"ENDWHILE"})
	end := expect_keyword_message(p, "ENDWHILE", "syntax error: expected ENDWHILE")
	if !token_is_keyword(p, end, "ENDWHILE") {
		return nil
	}
	period = expect_token_message(p, .Period, "syntax error: expected '.' after ENDWHILE")
	if period.kind != .Period {
		return nil
	}
	stmt.range = tokenizer.text_range(start.range.start, period.range.end)
	return stmt
}

parse_do_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "DO")
	stmt := ast.new(ast.Do_Stmt, start.range, p.allocator)
	if current_token(p).kind != .Period {
		stmt.count = parse_expr(p)
		if stmt.count == nil {
			return nil
		}
		if !allow_keyword(p, "TIMES") {
			error_current(p, "syntax error: expected keyword")
			return nil
		}
	}
	period := expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	stmt.body = parse_stmt_list_until(p, []string{"ENDDO"})
	end := expect_keyword_message(p, "ENDDO", "syntax error: expected ENDDO")
	if !token_is_keyword(p, end, "ENDDO") {
		return nil
	}
	period = expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	stmt.range = tokenizer.text_range(start.range.start, period.range.end)
	return stmt
}

parse_loop_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "LOOP")
	if !allow_keyword(p, "AT") {
		error_current(p, "syntax error: expected keyword")
		return nil
	}
	source := parse_required_expr_after(p, "syntax error: expected loop source after LOOP AT")
	if source == nil {
		return nil
	}
	if !validate_loop_header_tail(p) {
		return nil
	}
	header_start := start.range.start
	consume_raw_until_period(p)
	period := expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	stmt := ast.new(ast.Loop_Stmt, start.range, p.allocator)
	stmt.source = source
	stmt.header_range = tokenizer.text_range(header_start, period.range.end)
	stmt.header_text = strings.clone(p.source[header_start:period.range.start], p.allocator)
	stmt.body = parse_stmt_list_until(p, []string{"ENDLOOP"})
	end := expect_keyword_message(p, "ENDLOOP", "syntax error: expected ENDLOOP")
	if !token_is_keyword(p, end, "ENDLOOP") {
		return nil
	}
	period = expect_token_message(p, .Period, "syntax error: expected '.' after ENDLOOP")
	if period.kind != .Period {
		return nil
	}
	stmt.range = tokenizer.text_range(start.range.start, period.range.end)
	return stmt
}

parse_at_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "AT")
	stmt := ast.new(ast.At_Stmt, start.range, p.allocator)
	if allow_keyword(p, "FIRST") {
		stmt.kind = .First
	} else if allow_keyword(p, "LAST") {
		stmt.kind = .Last
	} else if allow_keyword(p, "NEW") {
		stmt.kind = .New
		stmt.expr = parse_expr(p)
		if stmt.expr == nil {
			return nil
		}
	} else {
		if !allow_keyword(p, "END") || !allow_keyword(p, "OF") {
			error_current(p, "syntax error: expected group processing header")
			return nil
		}
		stmt.kind = .End_Of
		stmt.expr = parse_expr(p)
		if stmt.expr == nil {
			return nil
		}
	}
	period := expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	stmt.body = parse_stmt_list_until(p, []string{"ENDAT"})
	end := expect_keyword(p, "ENDAT")
	if !token_is_keyword(p, end, "ENDAT") {
		return nil
	}
	period = expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	stmt.range = tokenizer.text_range(start.range.start, period.range.end)
	return stmt
}

parse_try_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "TRY")
	period := expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	stmt := ast.new(ast.Try_Stmt, start.range, p.allocator)
	stmt.body = parse_stmt_list_until(p, []string{"CATCH", "CLEANUP", "ENDTRY"})
	stmt.catches = make([dynamic]^ast.Catch_Clause, 0, 2, p.allocator)
	for at_keyword(p, "CATCH") {
		clause := parse_catch_clause(p)
		if clause == nil {
			return nil
		}
		append(&stmt.catches, clause)
	}
	if at_keyword(p, "CLEANUP") {
		stmt.cleanup = parse_cleanup_clause(p)
		if stmt.cleanup == nil {
			return nil
		}
	}
	end := expect_keyword_message(p, "ENDTRY", "syntax error: expected ENDTRY")
	if !token_is_keyword(p, end, "ENDTRY") {
		return nil
	}
	period = expect_token_message(p, .Period, "syntax error: expected '.' after ENDTRY")
	if period.kind != .Period {
		return nil
	}
	stmt.range = tokenizer.text_range(start.range.start, period.range.end)
	return stmt
}

parse_catch_clause :: proc(p: ^Parser) -> ^ast.Catch_Clause {
	start := expect_keyword(p, "CATCH")
	clause, _ := mem.new(ast.Catch_Clause, p.allocator)
	clause.exceptions = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	for current_token(p).kind != .Period && current_token(p).kind != .Eof {
		if allow_keyword(p, "INTO") {
			clause.into = parse_expr(p)
			if clause.into == nil {
				return nil
			}
			continue
		}
		if at_keyword(p, "BEFORE") || at_keyword(p, "UNWIND") {
			bump_token(p)
			continue
		}
		ex := parse_expr(p)
		if ex == nil {
			bump_token(p)
		} else {
			append(&clause.exceptions, ex)
		}
	}
	if len(clause.exceptions) == 0 {
		error_current(p, "syntax error: expected exception class after CATCH")
		return nil
	}
	period := expect_token_message(p, .Period, "syntax error: expected '.' after CATCH clause")
	if period.kind != .Period {
		return nil
	}
	clause.body = parse_stmt_list_until(p, []string{"CATCH", "CLEANUP", "ENDTRY"})
	clause.range = tokenizer.text_range(
		start.range.start,
		previous_stmt_end(clause.body, period.range.end),
	)
	return clause
}

parse_cleanup_clause :: proc(p: ^Parser) -> ^ast.Cleanup_Clause {
	start := expect_keyword(p, "CLEANUP")
	period := expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	clause, _ := mem.new(ast.Cleanup_Clause, p.allocator)
	clause.body = parse_stmt_list_until(p, []string{"ENDTRY"})
	clause.range = tokenizer.text_range(
		start.range.start,
		previous_stmt_end(clause.body, period.range.end),
	)
	return clause
}

parse_structural_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	if structural_block_keyword_starts(p, "CLASS") {
		return parse_named_block_stmt(p, ast.Class_Decl, "CLASS", "ENDCLASS")
	}
	if at_keyword(
		p,
		"INTERFACE",
	) {
		return parse_named_block_stmt(p, ast.Interface_Decl, "INTERFACE", "ENDINTERFACE")
	}
	if at_keyword(
		p,
		"METHOD",
	) {
		return parse_method_block_stmt(p)
	}
	if at_keyword(p, "FORM") {
		return parse_named_block_stmt(p, ast.Form_Decl, "FORM", "ENDFORM")
	}
	if structural_block_keyword_starts(p, "FUNCTION") {
		return parse_named_block_stmt(p, ast.Function_Decl, "FUNCTION", "ENDFUNCTION")
	}
	if at_keyword(
		p,
		"MODULE",
	) {
		return parse_named_block_stmt(p, ast.Module_Decl, "MODULE", "ENDMODULE")
	}
	if event_block_starts(p) {
		return parse_event_block_stmt(p)
	}
	if at_keyword_phrase(p, "ENHANCEMENT-SECTION") {
		return parse_named_block_stmt(
			p,
			ast.Enhancement_Section_Stmt,
			"ENHANCEMENT-SECTION",
			"END-ENHANCEMENT-SECTION",
		)
	}
	if at_keyword(p, "ENHANCEMENT") {
		return parse_named_block_stmt(p, ast.Enhancement_Stmt, "ENHANCEMENT", "ENDENHANCEMENT")
	}
	if at_keyword_phrase(p, "TEST-SEAM") {
		return parse_named_block_stmt(p, ast.Test_Seam_Stmt, "TEST-SEAM", "END-TEST-SEAM")
	}
	return parse_named_block_stmt(
		p,
		ast.Test_Injection_Stmt,
		"TEST-INJECTION",
		"END-TEST-INJECTION",
	)
}

parse_method_block_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start_index := p.index
	start := expect_keyword(p, "METHOD")
	name := first_qualified_name_until_period(p)
	consume_raw_until_top_level_period(p)
	period := expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	stmt := ast.new(ast.Method_Decl, start.range, p.allocator)
	stmt.name = name
	stmt.header_range = tokenizer.text_range(start.range.start, period.range.end)
	stmt.header_text = strings.clone(p.source[start.range.start:period.range.start], p.allocator)
	if method_header_is_amdp(p, start_index, p.previous_index) {
		stmt.is_amdp = true
		stmt.body = make([dynamic]^ast.Stmt, 0, 0, p.allocator)
		for !at_eof(p) && !at_keyword_phrase(p, "ENDMETHOD") {
			bump_token(p)
		}
		body_end := current_token(p).range.start
		end := expect_keyword_phrase(p, "ENDMETHOD")
		if end.kind == .Eof {
			return nil
		}
		end_period := expect_token(p, .Period)
		if end_period.kind != .Period {
			return nil
		}
		if period.range.end < body_end {
			stmt.amdp_body = strings.clone(p.source[period.range.end:body_end], p.allocator)
		}
		stmt.range = tokenizer.text_range(start.range.start, end_period.range.end)
		return stmt
	}
	stmt.body = parse_stmt_list_until(p, []string{"ENDMETHOD"})
	end := expect_keyword_phrase(p, "ENDMETHOD")
	if end.kind == .Eof {
		return nil
	}
	period = expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	stmt.range = tokenizer.text_range(start.range.start, period.range.end)
	return stmt
}

method_header_is_amdp :: proc(p: ^Parser, start_index, period_index: int) -> bool {
	has_database := false
	has_language := false
	for i in start_index ..< period_index {
		has_database = has_database || token_is_keyword(p, p.tokens[i], "DATABASE")
		has_language = has_language || token_is_keyword(p, p.tokens[i], "SQLSCRIPT")
	}
	return has_database && has_language
}

parse_named_block_stmt :: proc(
	p: ^Parser,
	$T: typeid,
	start_keyword, end_keyword: string,
) -> ^ast.Stmt {
	start_index := p.index
	start := expect_keyword_phrase(p, start_keyword)
	name := first_name_token_until_period(p)
	consume_raw_until_top_level_period(p)
	period := expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	stmt := ast.new(T, start.range, p.allocator)
	stmt.name = tokenizer.token_lexeme(name, p.source) if name.kind != .Eof else ""
	stmt.header_range = tokenizer.text_range(start.range.start, period.range.end)
	stmt.header_text = strings.clone(p.source[start.range.start:period.range.start], p.allocator)
	period_index := p.previous_index
	bodyless := named_block_header_is_bodyless(p, start_keyword, start_index, period_index)
	when intrinsics.type_has_field(T, "is_bodyless") {
		stmt.is_bodyless = bodyless
	}
	when intrinsics.type_has_field(T, "flags") {
		if bodyless {
			stmt.flags += {.Bodyless}
		}
		if named_block_header_has_keyword(p, start_index, period_index, "IMPLEMENTATION") {
			stmt.flags += {.Implementation}
		}
		if named_block_header_has_keyword(p, start_index, period_index, "ABSTRACT") {
			stmt.flags += {.Abstract}
		}
		stmt.superclass_name, stmt.superclass_range = named_block_header_superclass(p, start_index, period_index)
	}
	when intrinsics.type_has_field(T, "form_parameters") {
		stmt.form_parameters = parse_form_header_parameters(p, start_index, period_index)
	}
	when intrinsics.type_has_field(T, "function_parameters") {
		stmt.function_parameters, stmt.exceptions = parse_function_header_parameters(p, start_index, period_index)
	}
	if bodyless {
		stmt.body = make([dynamic]^ast.Stmt, 0, 0, p.allocator)
		stmt.range = stmt.header_range
		return stmt
	}
	stmt.body = parse_stmt_list_until(p, []string{end_keyword})
	end := expect_keyword_phrase(p, end_keyword)
	if end.kind == .Eof {
		return nil
	}
	period = expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	stmt.range = tokenizer.text_range(start.range.start, period.range.end)
	return stmt
}

named_block_header_is_bodyless :: proc(
	p: ^Parser,
	start_keyword: string,
	start_index, period_index: int,
) -> bool {
	if start_keyword != "CLASS" && start_keyword != "INTERFACE" {
		return false
	}
	return(
		named_block_header_has_keyword(p, start_index, period_index, "DEFERRED") ||
		named_block_header_has_keyword(p, start_index, period_index, "LOAD") \
	)
}

named_block_header_has_keyword :: proc(
	p: ^Parser,
	start_index, period_index: int,
	keyword: string,
) -> bool {
	for i in start_index ..< period_index {
		if token_is_keyword(p, p.tokens[i], keyword) {
			return true
		}
	}
	return false
}

named_block_header_superclass :: proc(
	p: ^Parser,
	start_index, period_index: int,
) -> (string, tokenizer.Range) {
	for i in start_index ..< period_index {
		if i + 2 >= period_index {
			break
		}
		if token_is_keyword(p, p.tokens[i], "INHERITING") &&
		   token_is_keyword(p, p.tokens[i + 1], "FROM") {
			tok := p.tokens[i + 2]
			if tok.kind == .Ident || tok.kind == .Number {
				return tokenizer.token_lexeme(tok, p.source), tok.range
			}
			return "", tokenizer.text_range(tok.range.start, tok.range.start)
		}
	}
	return "", tokenizer.text_range(0, 0)
}

parse_form_header_parameters :: proc(
	p: ^Parser,
	start_index, period_index: int,
) -> [dynamic]ast.Form_Parameter_Clause {
	params := make([dynamic]ast.Form_Parameter_Clause, 0, 2, p.allocator)
	i := header_body_start(start_index, period_index, "FORM")
	section := ast.Form_Parameter_Section.Using
	stop_keywords := []string{"TABLES", "USING", "CHANGING"}
	for i < period_index {
		if form_header_section(p, i, &section) {
			i += 1
			continue
		}
		name, name_range, passing, next, ok := parse_header_param_name(p, i, period_index)
		if !ok {
			i += 1
			continue
		}
		param := ast.Form_Parameter_Clause {
			section = section,
			name    = name,
			range   = name_range,
			passing = passing,
		}
		i = next
		if header_type_clause_starts(p, i, period_index) {
			param.type_clause, i = parse_header_type_clause(p, i, period_index, stop_keywords)
		}
		append(&params, param)
	}
	return params
}

parse_function_header_parameters :: proc(
	p: ^Parser,
	start_index, period_index: int,
) -> (
	[dynamic]ast.Function_Parameter_Clause,
	[dynamic]ast.Function_Exception_Clause,
) {
	params := make([dynamic]ast.Function_Parameter_Clause, 0, 2, p.allocator)
	exceptions := make([dynamic]ast.Function_Exception_Clause, 0, 1, p.allocator)
	i := header_body_start(start_index, period_index, "FUNCTION")
	section := ast.Function_Parameter_Section.Importing
	in_exceptions := false
	stop_keywords := []string{"IMPORTING", "EXPORTING", "CHANGING", "TABLES", "EXCEPTIONS"}
	for i < period_index {
		if function_header_section(p, i, &section, &in_exceptions) {
			i += 1
			continue
		}
		name, name_range, passing, next, ok := parse_header_param_name(p, i, period_index)
		if !ok {
			i += 1
			continue
		}
		i = next
		if in_exceptions {
			append(&exceptions, ast.Function_Exception_Clause{name, name_range})
			if i + 1 < period_index && p.tokens[i].kind == .Eq {
				i += 2
			}
			continue
		}
		param := ast.Function_Parameter_Clause {
			section = section,
			name    = name,
			range   = name_range,
			passing = passing,
		}
		if header_type_clause_starts(p, i, period_index) {
			param.type_clause, i = parse_header_type_clause(p, i, period_index, stop_keywords)
		}
		for i < period_index {
			if at_keyword_index(p, i, "OPTIONAL") {
				param.flags += {.Is_Optional}
				i += 1
				continue
			}
			if at_keyword_index(p, i, "DEFAULT") {
				param.flags += {.Has_Default_Value}
				i = skip_header_addition_value(p, i + 1, period_index, stop_keywords)
				continue
			}
			break
		}
		append(&params, param)
	}
	return params, exceptions
}

header_body_start :: proc(start_index, period_index: int, keyword: string) -> int {
	i := start_index + keyword_phrase_token_count(keyword)
	if i < period_index {
		i += 1
	}
	return i
}

form_header_section :: proc(
	p: ^Parser,
	index: int,
	section: ^ast.Form_Parameter_Section,
) -> bool {
	if at_keyword_index(p, index, "TABLES") {section^ = .Tables; return true}
	if at_keyword_index(p, index, "USING") {section^ = .Using; return true}
	if at_keyword_index(p, index, "CHANGING") {section^ = .Changing; return true}
	return false
}

function_header_section :: proc(
	p: ^Parser,
	index: int,
	section: ^ast.Function_Parameter_Section,
	in_exceptions: ^bool,
) -> bool {
	if at_keyword_index(p, index, "IMPORTING") {section^ = .Importing; in_exceptions^ = false; return true}
	if at_keyword_index(p, index, "EXPORTING") {section^ = .Exporting; in_exceptions^ = false; return true}
	if at_keyword_index(p, index, "CHANGING") {section^ = .Changing; in_exceptions^ = false; return true}
	if at_keyword_index(p, index, "TABLES") {section^ = .Tables; in_exceptions^ = false; return true}
	if at_keyword_index(p, index, "EXCEPTIONS") {in_exceptions^ = true; return true}
	return false
}

parse_header_param_name :: proc(
	p: ^Parser,
	index, period_index: int,
) -> (
	string,
	tokenizer.Range,
	ast.Parameter_Passing_Kind,
	int,
	bool,
) {
	i := index
	if i < period_index && tokenizer.token_lexeme(p.tokens[i], p.source) == "!" {
		i += 1
	}
	passing := ast.Parameter_Passing_Kind.Direct
	if at_keyword_index(p, i, "VALUE") || at_keyword_index(p, i, "REFERENCE") {
		passing = .Value if at_keyword_index(p, i, "VALUE") else .Reference
		i += 1
		if i < period_index && p.tokens[i].kind == .LParen {
			i += 1
		}
		if i >= period_index || !header_name_token_like(p.tokens[i]) {
			return "", tokenizer.Range{}, passing, i, false
		}
		tok := p.tokens[i]
		i += 1
		if i < period_index && p.tokens[i].kind == .RParen {
			i += 1
		}
		return strip_header_bang(tokenizer.token_lexeme(tok, p.source)), tok.range, passing, i, true
	}
	if i >= period_index || !header_name_token_like(p.tokens[i]) {
		return "", tokenizer.Range{}, passing, i, false
	}
	tok := p.tokens[i]
	name := strip_header_bang(tokenizer.token_lexeme(tok, p.source))
	return name, tok.range, passing, i + 1, name != ""
}

header_name_token_like :: proc(token: tokenizer.Token) -> bool {
	return token.kind == .Ident || token.kind == .Number
}

strip_header_bang :: proc(text: string) -> string {
	if len(text) > 0 && text[0] == '!' {
		return text[1:]
	}
	return text
}

header_type_clause_starts :: proc(p: ^Parser, index, period_index: int) -> bool {
	return index < period_index &&
	       (at_keyword_index(p, index, "TYPE") ||
	        at_keyword_index(p, index, "LIKE") ||
	        at_keyword_index(p, index, "STRUCTURE"))
}

parse_header_type_clause :: proc(
	p: ^Parser,
	index, period_index: int,
	stop_keywords: []string,
) -> (^ast.Data_Type_Clause, int) {
	i := index
	keyword := p.tokens[i]
	i += 1
	clause, _ := mem.new(ast.Data_Type_Clause, p.allocator)
	is_like := token_is_keyword(p, keyword, "LIKE")
	is_structure := token_is_keyword(p, keyword, "STRUCTURE")
	clause.form = .Structure if is_structure else (.Like if is_like else .Type)
	if !is_structure {
		if header_allow_keyword(p, &i, period_index, "LINE") {
			header_allow_keyword(p, &i, period_index, "OF")
			clause.form = .Like_Line_Of if is_like else .Type_Line_Of
		} else if !is_like && header_allow_keyword(p, &i, period_index, "REF") {
			header_allow_keyword(p, &i, period_index, "TO")
			clause.form = .Ref_To
		} else if !is_like && header_allow_keyword(p, &i, period_index, "RANGE") {
			header_allow_keyword(p, &i, period_index, "OF")
			clause.form = .Range_Of
		} else if header_allow_keyword(p, &i, period_index, "STANDARD") {
			header_allow_keyword(p, &i, period_index, "TABLE")
			header_allow_keyword(p, &i, period_index, "OF")
			clause.form = .Like_Standard_Table if is_like else .Standard_Table
		} else if header_allow_keyword(p, &i, period_index, "SORTED") {
			header_allow_keyword(p, &i, period_index, "TABLE")
			header_allow_keyword(p, &i, period_index, "OF")
			clause.form = .Like_Sorted_Table if is_like else .Sorted_Table
		} else if header_allow_keyword(p, &i, period_index, "HASHED") {
			header_allow_keyword(p, &i, period_index, "TABLE")
			header_allow_keyword(p, &i, period_index, "OF")
			clause.form = .Like_Hashed_Table if is_like else .Hashed_Table
		} else if header_allow_keyword(p, &i, period_index, "TABLE") {
			header_allow_keyword(p, &i, period_index, "OF")
			clause.form = .Like_Table if is_like else .Table
		}
	}
	clause.type_ref, i = parse_header_type_ref_expr(p, i, period_index, stop_keywords)
	return clause, i
}

header_allow_keyword :: proc(
	p: ^Parser,
	index: ^int,
	period_index: int,
	keyword: string,
) -> bool {
	if index^ < period_index && at_keyword_index(p, index^, keyword) {
		index^ += 1
		return true
	}
	return false
}

parse_header_type_ref_expr :: proc(
	p: ^Parser,
	start, period_index: int,
	stop_keywords: []string,
) -> (^ast.Expr, int) {
	i := start
	if header_type_ref_done(p, i, start, period_index, stop_keywords, false) {
		return nil, i
	}
	paren, bracket, brace := 0, 0, 0
	name_end := -1
	in_key := false
	for !header_type_ref_done(p, i, start, period_index, stop_keywords, in_key) {
		tok := p.tokens[i]
		top := paren == 0 && bracket == 0 && brace == 0
		if top && at_keyword_index(p, i, "WITH") {
			if !type_ref_key_clause_starts(p, i) {
				break
			}
			if name_end < 0 && i > start {
				name_end = p.tokens[i - 1].range.end
			}
			in_key = true
		}
		#partial switch tok.kind {
		case .LParen:
			paren += 1
		case .RParen:
			if paren == 0 {break}
			paren -= 1
		case .LBracket:
			bracket += 1
		case .RBracket:
			if bracket == 0 {break}
			bracket -= 1
		case .LBrace:
			brace += 1
		case .RBrace:
			if brace == 0 {break}
			brace -= 1
		}
		i += 1
	}
	if i <= start {
		return nil, i
	}
	if name_end < 0 {
		name_end = p.tokens[i - 1].range.end
	}
	expr := type_ref_expr_from_tokens(p, start, i, name_end)
	return expr, i
}

header_type_ref_done :: proc(
	p: ^Parser,
	index, start, period_index: int,
	stop_keywords: []string,
	in_key: bool,
) -> bool {
	if index >= period_index {
		return true
	}
	for keyword in stop_keywords {
		if at_keyword_index(p, index, keyword) {
			return true
		}
	}
	if at_keyword_index(p, index, "WITH") && !type_ref_key_clause_starts(p, index) {
		return true
	}
	if at_keyword_index(p, index, "OPTIONAL") || (!in_key && at_keyword_index(p, index, "DEFAULT")) {
		return true
	}
	return index > start && header_parameter_starts(p, index, period_index)
}

header_parameter_starts :: proc(p: ^Parser, index, period_index: int) -> bool {
	_, _, _, next, ok := parse_header_param_name(p, index, period_index)
	return ok && header_type_clause_starts(p, next, period_index)
}

skip_header_addition_value :: proc(
	p: ^Parser,
	index, period_index: int,
	stop_keywords: []string,
) -> int {
	i := index
	paren, bracket, brace := 0, 0, 0
	for i < period_index {
		top := paren == 0 && bracket == 0 && brace == 0
		if top {
			for keyword in stop_keywords {
				if at_keyword_index(p, i, keyword) {
					return i
				}
			}
			if at_keyword_index(p, i, "OPTIONAL") || header_parameter_starts(p, i, period_index) {
				return i
			}
		}
		tok := p.tokens[i]
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
		i += 1
	}
	return i
}

parse_event_block_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := current_token(p)
	kind := event_block_kind(p)
	consume_event_header(p)
	period := expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	stmt := ast.new(
		ast.Event_Block_Stmt,
		tokenizer.text_range(start.range.start, period.range.end),
		p.allocator,
	)
	stmt.kind = kind
	stmt.header_range = stmt.range
	stmt.header_text = strings.clone(p.source[start.range.start:period.range.start], p.allocator)
	stmt.body = parse_stmt_list_until(
		p,
		[]string {
			"AT SELECTION-SCREEN",
			"INITIALIZATION",
			"LOAD-OF-PROGRAM",
			"START-OF-SELECTION",
			"END-OF-SELECTION",
			"TOP-OF-PAGE",
			"END-OF-PAGE",
			"CLASS",
			"INTERFACE",
			"FORM",
			"FUNCTION",
			"MODULE",
		},
	)
	stmt.range = tokenizer.text_range(
		start.range.start,
		previous_stmt_end(stmt.body, period.range.end),
	)
	return stmt
}

at_group_stmt_starts :: proc(p: ^Parser) -> bool {
	if !at_keyword(p, "AT") {
		return false
	}
	return(
		at_keyword_index(p, p.index + 1, "FIRST") ||
		at_keyword_index(p, p.index + 1, "LAST") ||
		at_keyword_index(p, p.index + 1, "NEW") ||
		(at_keyword_index(p, p.index + 1, "END") && at_keyword_index(p, p.index + 2, "OF")) \
	)
}

event_block_starts :: proc(p: ^Parser) -> bool {
	return(
		at_keyword_phrase(p, "AT SELECTION-SCREEN") ||
		at_keyword(p, "INITIALIZATION") ||
		at_keyword_phrase(p, "LOAD-OF-PROGRAM") ||
		at_keyword_phrase(p, "START-OF-SELECTION") ||
		at_keyword_phrase(p, "END-OF-SELECTION") ||
		at_keyword_phrase(p, "TOP-OF-PAGE") ||
		at_keyword_phrase(p, "END-OF-PAGE") \
	)
}

event_block_kind :: proc(p: ^Parser) -> string {
	if at_keyword_phrase(p, "AT SELECTION-SCREEN") {
		return "AT SELECTION-SCREEN"
	}
	if at_keyword(p, "INITIALIZATION") {
		return "INITIALIZATION"
	}
	if at_keyword_phrase(p, "LOAD-OF-PROGRAM") {
		return "LOAD-OF-PROGRAM"
	}
	if at_keyword_phrase(p, "START-OF-SELECTION") {
		return "START-OF-SELECTION"
	}
	if at_keyword_phrase(p, "END-OF-SELECTION") {
		return "END-OF-SELECTION"
	}
	if at_keyword_phrase(p, "TOP-OF-PAGE") {
		return "TOP-OF-PAGE"
	}
	return "END-OF-PAGE"
}

consume_event_header :: proc(p: ^Parser) {
	kind := event_block_kind(p)
	expect_keyword_phrase(p, kind)
	consume_raw_until_period(p)
}
