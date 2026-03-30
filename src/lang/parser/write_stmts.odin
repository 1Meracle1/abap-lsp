package lang_parser

import "../ast"
import "../lexer"

// WRITE [{ / }][(len)] dobj
//   [LEFT-JUSTIFIED | RIGHT-JUSTIFIED | NO-GROUPING | NO-SIGN | DECIMALS n | TIME ZONE tz] ...
//   [TO target]
//   [same options again] ...
// WRITE: oper, oper, ...
parse_write_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	write_tok := expect_keyword_token(p, "WRITE")
	stmt := ast.new(ast.Write_Stmt, write_tok.range)
	stmt.operands = make([dynamic]ast.Write_Operand)

	is_chain := false
	if p.curr_tok.kind == .Colon {
		is_chain = true
		advance_token(p)
	}

	if is_chain &&
	   (p.curr_tok.kind == .Period || p.curr_tok.kind == .EOF) {
		error(p, p.curr_tok.range, "expected operand after WRITE:")
		period_tok := expect_token(p, .Period)
		stmt.range.end = period_tok.range.end
		stmt.derived_stmt = stmt
		return stmt
	}

	for {
		append(&stmt.operands, ast.Write_Operand{})
		parse_write_operand(p, &stmt.operands[len(stmt.operands) - 1])
		if p.curr_tok.kind == .Comma {
			if !is_chain {
				error(
					p,
					p.curr_tok.range,
					"use WRITE: when writing several operands separated by commas",
				)
			}
			advance_token(p)
			if p.curr_tok.kind == .Period {
				error(p, p.curr_tok.range, "trailing comma before period in WRITE")
				break
			}
			continue
		}
		break
	}

	period_tok := expect_token(p, .Period)
	stmt.range.end = period_tok.range.end
	stmt.derived_stmt = stmt
	return stmt
}

parse_write_operand :: proc(p: ^Parser, op: ^ast.Write_Operand) {
	start := p.curr_tok.range.start

	if p.curr_tok.kind == .Slash {
		op.line_feed = true
		advance_token(p)
	}

	if p.curr_tok.kind == .LParen {
		advance_token(p)
		op.format_len = parse_expr(p)
		expect_token(p, .RParen)
	}

	if p.curr_tok.kind == .Comma || p.curr_tok.kind == .Period {
		error(p, p.curr_tok.range, "expected data object in WRITE operand")
		op.range = lexer.TextRange{start, p.curr_tok.range.start}
		return
	}

	op.data = parse_concatenate_source_expr(p)
	if op.data == nil {
		error(p, p.curr_tok.range, "expected expression in WRITE")
		op.data = ast.new(ast.Bad_Expr, p.curr_tok.range)
		op.range = lexer.TextRange{start, p.curr_tok.range.start}
		return
	}

	end := op.data.range.end
	to_seen := false

	itr := 0
	for p.curr_tok.kind != .Comma &&
	    p.curr_tok.kind != .Period &&
	    p.curr_tok.kind != .EOF &&
	    itr < 256 {
		itr += 1

		if check_keyword(p, "TO") {
			if to_seen {
				break
			}
			to_seen = true
			advance_token(p)
			op.to_target = parse_concatenate_source_expr(p)
			if op.to_target != nil {
				end = max(end, op.to_target.range.end)
			}
			continue
		}

		if check_hyphenated_keyword(p, "LEFT", "JUSTIFIED") {
			op.left_justified = true
			continue
		}
		if check_hyphenated_keyword(p, "RIGHT", "JUSTIFIED") {
			op.right_justified = true
			continue
		}
		if check_hyphenated_keyword(p, "NO", "GROUPING") {
			op.no_grouping = true
			continue
		}
		if check_hyphenated_keyword(p, "NO", "SIGN") {
			op.no_sign = true
			continue
		}

		if check_keyword(p, "DECIMALS") {
			advance_token(p)
			op.decimals = parse_concatenate_source_expr(p)
			if op.decimals != nil {
				end = max(end, op.decimals.range.end)
			}
			continue
		}

		if check_keyword(p, "TIME") {
			advance_token(p)
			expect_keyword_token(p, "ZONE")
			op.time_zone = parse_concatenate_source_expr(p)
			if op.time_zone != nil {
				end = max(end, op.time_zone.range.end)
			}
			continue
		}

		break
	}

	if op.format_len != nil {
		end = max(end, op.format_len.range.end)
	}
	end = max(end, op.data.range.end)
	end = max(end, p.prev_tok.range.end)

	op.range = lexer.TextRange{start, end}
}
