package abap_frontend_parser

import "src:ast"
import "src:tokenizer"

Constructor_Body_Kind :: enum {
	Value,
	Corresponding,
	Cond,
	Switch,
	Reduce,
	Single,
}

parse_expr_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	expr := parse_expr(p)
	if expr == nil {
		return nil
	}
	period := expect_token(p, .Period)
	stmt := ast.new(
		ast.Expr_Stmt,
		tokenizer.text_range(expr.range.start, statement_end(p, period)),
		p.allocator,
	)
	stmt.expr = expr
	return stmt
}

parse_expr :: proc(p: ^Parser) -> ^ast.Expr {
	return parse_concat_expr(p)
}

parse_logical_expr :: proc(p: ^Parser) -> ^ast.Expr {
	return parse_or_expr(p)
}

expr_stop_keyword :: proc(p: ^Parser) -> bool {
	for keyword in p.expr_stop_keywords {
		if at_keyword_phrase(p, keyword) {
			return true
		}
	}
	for keyword in p.expr_extra_stop_keywords {
		if at_keyword_phrase(p, keyword) {
			return true
		}
	}
	return false
}

parse_or_expr :: proc(p: ^Parser) -> ^ast.Expr {
	left := parse_and_expr(p)
	if left == nil {
		return nil
	}

	for at_keyword(p, "OR") {
		bump_token(p)
		right := parse_and_expr(p)
		if right == nil {
			return nil
		}
		left = build_binary_expr(p, left, .Or, right)
	}
	return left
}

parse_and_expr :: proc(p: ^Parser) -> ^ast.Expr {
	left := parse_not_expr(p)
	if left == nil {
		return nil
	}

	for at_keyword(p, "AND") {
		bump_token(p)
		right := parse_not_expr(p)
		if right == nil {
			return nil
		}
		left = build_binary_expr(p, left, .And, right)
	}
	return left
}

parse_not_expr :: proc(p: ^Parser) -> ^ast.Expr {
	if at_keyword(p, "NOT") {
		op := bump_token(p)
		inner := parse_not_expr(p)
		if inner == nil {
			return nil
		}
		expr := ast.new(
			ast.Unary_Expr,
			tokenizer.text_range(op.range.start, inner.range.end),
			p.allocator,
		)
		expr.op = .Not
		expr.expr = inner
		return expr
	}
	return parse_comparison_expr(p)
}

parse_comparison_expr :: proc(p: ^Parser) -> ^ast.Expr {
	left := parse_concat_expr(p)
	if left == nil {
		return nil
	}

	if at_keyword(p, "IS") {
		return parse_is_predicate_expr(p, left)
	}

	if at_keyword(p, "BETWEEN") {
		return parse_between_expr(p, left)
	}

	if p.open_sql_expr && at_keyword(p, "NOT") && at_keyword_index(p, p.index + 1, "LIKE") {
		bump_token(p)
		bump_token(p)
		right := parse_concat_expr(p)
		if right == nil {
			return nil
		}
		return build_binary_expr(p, left, .Not_Like, right)
	}

	if at_keyword(p, "NOT") && at_keyword_index(p, p.index + 1, "IN") {
		bump_token(p)
		bump_token(p)
		right := parse_parenthesized_raw_expr(p) if current_token(p).kind == .LParen else parse_concat_expr(p)
		if right == nil {
			return nil
		}
		return build_binary_expr(p, left, .Not_In, right)
	}

	if p.open_sql_expr && at_keyword(p, "LIKE") {
		bump_token(p)
		right := parse_concat_expr(p)
		if right == nil {
			return nil
		}
		return build_binary_expr(p, left, .Like, right)
	}

	if op, ok := comparison_op(p, current_token(p)); ok {
		bump_token(p)
		right := parse_parenthesized_raw_expr(p) if (op == .In || op == .Not_In) && current_token(p).kind == .LParen else parse_concat_expr(p)
		if right == nil {
			return nil
		}
		return build_binary_expr(p, left, op, right)
	}

	return left
}

parse_concat_expr :: proc(p: ^Parser) -> ^ast.Expr {
	left := parse_additive_expr(p)
	if left == nil {
		return nil
	}

	for current_token(p).kind == .Ampersand {
		bump_token(p)
		if current_token(p).kind == .Ampersand {
			bump_token(p)
		}
		right := parse_additive_expr(p)
		if right == nil {
			return nil
		}
		left = build_binary_expr(p, left, .Concatenate, right)
	}
	for current_token(p).kind == .String && tokens_touch(previous_token(p), current_token(p)) {
		right := parse_additive_expr(p)
		if right == nil {
			return nil
		}
		left = build_binary_expr(p, left, .Concatenate, right)
	}
	return left
}

parse_parenthesized_raw_expr :: proc(p: ^Parser) -> ^ast.Expr {
	start := p.index
	close_i := matching_group_index(p, p.index, .LParen, .RParen)
	if close_i < 0 {
		return parse_paren_expr(p)
	}
	for p.index <= close_i {
		bump_token(p)
	}
	return type_ref_expr_from_tokens(p, start, p.index, -1, false, false)
}

parse_additive_expr :: proc(p: ^Parser) -> ^ast.Expr {
	left := parse_multiplicative_expr(p)
	if left == nil {
		return nil
	}

	for {
		tok := current_token(p)
		op: ast.Binary_Op
		if tok.kind == .Plus {
			op = .Add
		} else if tok.kind == .Minus && has_space_between(previous_token(p), tok) {
			op = .Subtract
		} else {
			break
		}

		bump_token(p)
		right := parse_multiplicative_expr(p)
		if right == nil {
			return nil
		}
		left = build_binary_expr(p, left, op, right)
	}
	return left
}

parse_multiplicative_expr :: proc(p: ^Parser) -> ^ast.Expr {
	left := parse_unary_expr(p)
	if left == nil {
		return nil
	}

	for {
		tok := current_token(p)
		op: ast.Binary_Op
		if tok.kind == .Star {
			op = .Multiply
		} else if tok.kind == .Slash {
			op = .Divide
		} else if token_is_keyword(p, tok, "DIV") {
			op = .Integer_Divide
		} else if token_is_keyword(p, tok, "MOD") {
			op = .Modulo
		} else if at_keyword_phrase(p, "BIT-AND") {
			op = .Bit_And
		} else if at_keyword_phrase(p, "BIT-OR") {
			op = .Bit_Or
		} else if at_keyword_phrase(p, "BIT-XOR") {
			op = .Bit_Xor
		} else {
			break
		}

		if op == .Bit_And {
			expect_keyword_phrase(p, "BIT-AND")
		} else if op == .Bit_Or {
			expect_keyword_phrase(p, "BIT-OR")
		} else if op == .Bit_Xor {
			expect_keyword_phrase(p, "BIT-XOR")
		} else {
			bump_token(p)
		}
		right := parse_unary_expr(p)
		if right == nil {
			return nil
		}
		left = build_binary_expr(p, left, op, right)
	}
	return left
}

parse_unary_expr :: proc(p: ^Parser) -> ^ast.Expr {
	tok := current_token(p)
	if tok.kind == .Plus || tok.kind == .Minus {
		op_tok := bump_token(p)
		inner := parse_unary_expr(p)
		if inner == nil {
			return nil
		}
		expr := ast.new(
			ast.Unary_Expr,
			tokenizer.text_range(op_tok.range.start, inner.range.end),
			p.allocator,
		)
		expr.op = .Plus if op_tok.kind == .Plus else .Minus
		expr.expr = inner
		return expr
	}
	return parse_postfix_expr(p)
}

parse_postfix_expr :: proc(p: ^Parser) -> ^ast.Expr {
	value := parse_primary_expr(p)
	if value == nil {
		return nil
	}

	for {
		tok := current_token(p)
		prev := previous_token(p)
		if tok.kind == .LBracket {
			value = parse_table_expr(p, value)
			if value == nil {
				return nil
			}
			continue
		}
		if selector_operator_starts(prev, tok) {
			value = parse_selector_expr(p, value)
			if value == nil {
				return nil
			}
			continue
		}
		if tok.kind == .Plus && tokens_touch(prev, tok) {
			sub := parse_substring_with_offset_expr(p, value)
			if sub != nil {
				value = sub
				continue
			}
			break
		}
		if tok.kind == .LParen && tokens_touch(prev, tok) {
			if call_padding_is_valid(
				p,
				p.index,
				matching_group_index(p, p.index, .LParen, .RParen),
			) {
				value = parse_call_expr(p, value)
				if value == nil {
					return nil
				}
				continue
			}
			sub := parse_substring_without_offset_expr(p, value)
			if sub != nil {
				value = sub
				continue
			}
			value = parse_call_expr(p, value)
			if value == nil {
				return nil
			}
			continue
		}
		break
	}

	return value
}

parse_primary_expr :: proc(p: ^Parser) -> ^ast.Expr {
	if expr_stop_keyword(p) {
		return nil
	}
	if p.open_sql_expr && sql_case_keyword(p) {
		return nil
	}
	tok := current_token(p)
	#partial switch tok.kind {
	case .Ident:
		if at_keyword(p, "LET") {
			return parse_let_expr(p, .Single)
		}
		if p.open_sql_expr && at_keyword(p, "CASE") {
			return parse_sql_case_expr(p)
		}
		if p.open_sql_expr && at_keyword(p, "NULL") {
			null_tok := bump_token(p)
			expr := ast.new(ast.Literal_Expr, null_tok.range, p.allocator)
			expr.value = "NULL"
			return expr
		}
		if at_keyword(p, "DATA") && next_token_kind(p, 1) == .LParen {
			return parse_data_inline_name_expr(p)
		}
		if at_keyword(p, "FIELD") &&
		   next_token_kind(p, 1) == .Minus &&
		   at_keyword_index(p, p.index + 2, "SYMBOL") {
			return parse_field_symbol_inline_name_expr(p)
		}
		if constructor_expr_starts(p, tok) {
			return parse_constructor_expr(p)
		}
		bump_token(p)
		expr := ast.new(ast.Ident_Expr, tok.range, p.allocator)
		expr.name = tokenizer.token_lexeme(tok, p.source)
		return expr
	case .Number, .String, .Star:
		bump_token(p)
		expr := ast.new(ast.Literal_Expr, tok.range, p.allocator)
		expr.value = tokenizer.token_lexeme(tok, p.source)
		return expr
	case .StringTemplate:
		return parse_char_string_template_expr(p)
	case .Hash:
		bump_token(p)
		expr := ast.new(ast.Ident_Expr, tok.range, p.allocator)
		expr.name = tokenizer.token_lexeme(tok, p.source)
		return expr
	case .At:
		bump_token(p)
		value := parse_unary_expr(p)
		if value == nil {
			return nil
		}
		expr := ast.new(ast.Host_Expr, tokenizer.text_range(tok.range.start, value.range.end), p.allocator)
		expr.value = value
		return expr
	case .LParen:
		return parse_paren_expr(p)
	}
	error_current(p, "syntax error: expected expression")
	return nil
}

parse_paren_expr :: proc(p: ^Parser) -> ^ast.Expr {
	open := expect_token(p, .LParen)
	if open.kind != .LParen {
		return nil
	}
	inner := parse_logical_expr(p)
	if inner == nil {
		return nil
	}
	close := expect_token(p, .RParen)
	if close.kind != .RParen {
		return nil
	}
	expr := ast.new(
		ast.Paren_Expr,
		tokenizer.text_range(open.range.start, close.range.end),
		p.allocator,
	)
	expr.expr = inner
	return expr
}

parse_table_expr :: proc(p: ^Parser, table: ^ast.Expr) -> ^ast.Expr {
	open := expect_token(p, .LBracket)
	if open.kind != .LBracket {
		return nil
	}
	selectors := make([dynamic]^ast.Expr, 0, 2, p.allocator)
	for current_token(p).kind != .RBracket && current_token(p).kind != .Eof {
		if allow_token(p, .Comma) {
			continue
		}
		start := p.index
		item := parse_logical_expr(p)
		if item != nil {
			append(&selectors, item)
		} else {
			bump_token(p)
		}
		ensure_forward_progress(p, start)
	}
	close := expect_token(p, .RBracket)
	if close.kind != .RBracket {
		return nil
	}
	expr := ast.new(
		ast.Table_Expr,
		tokenizer.text_range(table.range.start, close.range.end),
		p.allocator,
	)
	expr.table = table
	expr.selectors = selectors
	return expr
}

parse_selector_expr :: proc(p: ^Parser, base: ^ast.Expr) -> ^ast.Expr {
	op_tok := bump_token(p)
	field_tok := current_token(p)
	if field_tok.kind != .Ident &&
	   field_tok.kind != .Number &&
	   !(op_tok.kind == .Tilde && field_tok.kind == .Star) &&
	   !(op_tok.kind == .Arrow && field_tok.kind == .Star) {
		error_current(p, "syntax error: expected selector field")
		return nil
	}
	bump_token(p)

	field: ^ast.Expr
	if field_tok.kind == .Number || field_tok.kind == .Star {
		lit := ast.new(ast.Literal_Expr, field_tok.range, p.allocator)
		lit.value = tokenizer.token_lexeme(field_tok, p.source)
		field = lit
	} else {
		name := ast.new(ast.Ident_Expr, field_tok.range, p.allocator)
		name.name = tokenizer.token_lexeme(field_tok, p.source)
		field = name
	}

	expr := ast.new(
		ast.Selector_Expr,
		tokenizer.text_range(base.range.start, field.range.end),
		p.allocator,
	)
	expr.base = base
	expr.op = selector_op(op_tok.kind)
	expr.field = field
	return expr
}

parse_substring_with_offset_expr :: proc(p: ^Parser, base: ^ast.Expr) -> ^ast.Expr {
	if !node_can_start_substring(base) {
		return nil
	}
	save_index := p.index
	save_prev := p.previous_index
	bump_token(p)
	offset_start := p.index
	lparen := find_tight_lparen_for_substring(p, offset_start)
	if lparen < 0 {
		p.index = save_index
		p.previous_index = save_prev
		return nil
	}
	offset := parse_complete_concat_expr(p, offset_start, lparen)
	if offset == nil {
		p.index = save_index
		p.previous_index = save_prev
		return nil
	}
	p.index = lparen
	p.previous_index = lparen - 1
	bump_token(p)
	length := parse_substring_length_expr(p)
	if length == nil {
		p.index = save_index
		p.previous_index = save_prev
		return nil
	}
	close := expect_token(p, .RParen)
	if close.kind != .RParen {
		p.index = save_index
		p.previous_index = save_prev
		return nil
	}
	expr := ast.new(
		ast.Substring_Expr,
		tokenizer.text_range(base.range.start, close.range.end),
		p.allocator,
	)
	expr.base = base
	expr.offset = offset
	expr.length = length
	return expr
}

parse_substring_without_offset_expr :: proc(p: ^Parser, base: ^ast.Expr) -> ^ast.Expr {
	if !node_can_start_substring(base) {
		return nil
	}
	save_index := p.index
	save_prev := p.previous_index
	bump_token(p)
	length := parse_substring_length_expr(p)
	if length == nil {
		p.index = save_index
		p.previous_index = save_prev
		return nil
	}
	close := expect_token(p, .RParen)
	if close.kind != .RParen {
		p.index = save_index
		p.previous_index = save_prev
		return nil
	}
	expr := ast.new(
		ast.Substring_Expr,
		tokenizer.text_range(base.range.start, close.range.end),
		p.allocator,
	)
	expr.base = base
	expr.offset = nil
	expr.length = length
	return expr
}

parse_call_expr :: proc(p: ^Parser, callee: ^ast.Expr) -> ^ast.Expr {
	open := expect_token(p, .LParen)
	if open.kind != .LParen {
		return nil
	}
	args := ast.new(ast.Call_Arg_List_Expr, open.range, p.allocator)
	args.args = make([dynamic]^ast.Expr, 0, 4, p.allocator)

	for current_token(p).kind != .RParen && current_token(p).kind != .Eof {
		if allow_token(p, .Comma) {
			continue
		}
		start := p.index
		if call_argument_section_starts(p) {
			section := parse_call_arg_section_expr(p)
			if section != nil {
				append(&args.args, section)
			}
		} else {
			arg := parse_call_arg_expr(p)
			if arg != nil {
				append(&args.args, arg)
			}
		}
		ensure_forward_progress(p, start)
	}

	close := expect_token(p, .RParen)
	if close.kind != .RParen {
		return nil
	}
	args.range = tokenizer.text_range(open.range.start, close.range.end)

	call := ast.new(
		ast.Call_Expr,
		tokenizer.text_range(callee.range.start, close.range.end),
		p.allocator,
	)
	call.callee = callee
	call.args = args
	return call
}

parse_call_arg_section_expr :: proc(p: ^Parser) -> ^ast.Expr {
	name := bump_token(p)
	section := ast.new(ast.Call_Arg_Section_Expr, name.range, p.allocator)
	section.kind = call_argument_section_kind(p, name)
	section.name = tokenizer.token_lexeme(name, p.source)
	section.args = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	for current_token(p).kind != .RParen &&
	    current_token(p).kind != .Eof &&
	    !call_argument_section_starts(p) {
		if allow_token(p, .Comma) {
			continue
		}
		start := p.index
		arg := parse_call_arg_expr(p)
		if arg != nil {
			append(&section.args, arg)
			section.range.end = arg.range.end
		} else {
			bump_token(p)
		}
		ensure_forward_progress(p, start)
	}
	return section
}

parse_call_arg_expr :: proc(p: ^Parser) -> ^ast.Expr {
	if current_token(p).kind == .Ident && next_token_kind(p, 1) == .Eq {
		name := bump_token(p)
		expect_token(p, .Eq)
		value := parse_logical_expr(p)
		if value == nil {
			return nil
		}
		arg := ast.new(
			ast.Call_Named_Arg_Expr,
			tokenizer.text_range(name.range.start, value.range.end),
			p.allocator,
		)
		arg.name = tokenizer.token_lexeme(name, p.source)
		arg.value = value
		return arg
	}
	value := parse_logical_expr(p)
	if value == nil {
		return nil
	}
	arg := ast.new(ast.Call_Positional_Arg_Expr, value.range, p.allocator)
	arg.value = value
	return arg
}

parse_constructor_expr :: proc(p: ^Parser) -> ^ast.Expr {
	kw := bump_token(p)
	type_ref := parse_constructor_type_ref(p)
	if type_ref == nil {
		return nil
	}

	args := make([dynamic]^ast.Expr, 0, 4, p.allocator)
	if allow_token(p, .LParen) {
		parse_constructor_body_sequence(p, constructor_body_kind(p, kw), &args)
		close := expect_token(p, .RParen)
		if close.kind != .RParen {
			return nil
		}
		expr := ast.new(
			ast.Constructor_Expr,
			tokenizer.text_range(kw.range.start, close.range.end),
			p.allocator,
		)
		expr.kind = constructor_kind(p, kw)
		expr.type_ref = type_ref
		expr.args = args
		return expr
	}

	expr := ast.new(
		ast.Constructor_Expr,
		tokenizer.text_range(kw.range.start, type_ref.range.end),
		p.allocator,
	)
	expr.kind = constructor_kind(p, kw)
	expr.type_ref = type_ref
	expr.args = args
	return expr
}

parse_substring_length_expr :: proc(p: ^Parser) -> ^ast.Expr {
	if current_token(p).kind == .Star {
		tok := bump_token(p)
		expr := ast.new(ast.Ident_Expr, tok.range, p.allocator)
		expr.name = tokenizer.token_lexeme(tok, p.source)
		return expr
	}
	return parse_concat_expr(p)
}

parse_constructor_body_sequence :: proc(
	p: ^Parser,
	kind: Constructor_Body_Kind,
	out: ^[dynamic]^ast.Expr,
) {
	#partial switch kind {
	case .Corresponding:
		parse_corresponding_constructor_sequence(p, out)
	case .Cond:
		parse_cond_constructor_sequence(p, out)
	case .Switch:
		parse_switch_constructor_sequence(p, out)
	case .Reduce:
		parse_reduce_constructor_sequence(p, out)
	case .Single:
		if expr := parse_constructor_value_expr(p); expr != nil {
			append(out, expr)
		}
	case:
		parse_value_constructor_sequence(p, out)
	}
}

parse_value_constructor_sequence :: proc(p: ^Parser, out: ^[dynamic]^ast.Expr) {
	for !constructor_args_done(p) {
		if allow_token(p, .Comma) {
			continue
		}
		start := p.index
		if at_keyword(p, "LET") {
			append_if_expr(out, parse_let_expr(p, .Value))
		} else if at_keyword(p, "FOR") {
			append_if_expr(out, parse_constructor_for_clause_expr(p, .Value))
		} else if at_keyword(p, "BASE") {
			append_if_expr(out, parse_constructor_base_clause_expr(p))
		} else if at_keyword(p, "LINES") && at_keyword_index(p, p.index + 1, "OF") {
			append_if_expr(out, parse_constructor_lines_of_clause_expr(p))
		} else if current_token(p).kind == .Ident && next_token_kind(p, 1) == .Eq {
			append_if_expr(out, parse_constructor_named_assignment_expr(p))
		} else if current_token(p).kind == .LParen {
			append_if_expr(out, parse_constructor_row_expr(p, .Value))
		} else {
			append_if_expr(out, parse_constructor_value_expr(p))
		}
		ensure_forward_progress(p, start)
	}
}

parse_corresponding_constructor_sequence :: proc(p: ^Parser, out: ^[dynamic]^ast.Expr) {
	for !constructor_args_done(p) {
		if allow_token(p, .Comma) {
			continue
		}
		start := p.index
		if at_keyword(p, "BASE") {
			append_if_expr(out, parse_constructor_base_clause_expr(p))
		} else if at_keyword(p, "MAPPING") {
			append_if_expr(out, parse_constructor_mapping_clause_expr(p))
		} else if at_keyword(p, "EXCEPT") {
			append_if_expr(out, parse_constructor_except_clause_expr(p))
		} else {
			append_if_expr(out, parse_constructor_value_expr(p))
		}
		ensure_forward_progress(p, start)
	}
}

parse_cond_constructor_sequence :: proc(p: ^Parser, out: ^[dynamic]^ast.Expr) {
	if at_keyword(p, "LET") {
		append_if_expr(out, parse_let_expr(p, .Cond))
		return
	}
	for !constructor_args_done(p) {
		start := p.index
		if at_keyword(p, "WHEN") {
			append_if_expr(out, parse_constructor_when_clause_expr(p, false))
		} else if at_keyword(p, "ELSE") {
			append_if_expr(out, parse_constructor_else_clause_expr(p))
		} else {
			append_if_expr(out, parse_constructor_value_expr(p))
		}
		ensure_forward_progress(p, start)
	}
}

parse_switch_constructor_sequence :: proc(p: ^Parser, out: ^[dynamic]^ast.Expr) {
	if at_keyword(p, "LET") {
		append_if_expr(out, parse_let_expr(p, .Switch))
		return
	}
	if !constructor_args_done(p) && !at_keyword(p, "WHEN") && !at_keyword(p, "ELSE") {
		append_if_expr(out, parse_constructor_value_expr(p))
	}
	for !constructor_args_done(p) {
		start := p.index
		if at_keyword(p, "WHEN") {
			append_if_expr(out, parse_constructor_when_clause_expr(p, true))
		} else if at_keyword(p, "ELSE") {
			append_if_expr(out, parse_constructor_else_clause_expr(p))
		} else {
			append_if_expr(out, parse_constructor_value_expr(p))
		}
		ensure_forward_progress(p, start)
	}
}

parse_reduce_constructor_sequence :: proc(p: ^Parser, out: ^[dynamic]^ast.Expr) {
	for !constructor_args_done(p) {
		start := p.index
		if at_keyword(p, "LET") {
			append_if_expr(out, parse_let_expr(p, .Reduce))
		} else if at_keyword(p, "INIT") {
			append_if_expr(out, parse_constructor_init_clause_expr(p))
		} else if at_keyword(p, "FOR") {
			append_if_expr(out, parse_constructor_for_clause_expr(p, .Reduce))
		} else if at_keyword(p, "NEXT") {
			append_if_expr(out, parse_constructor_next_clause_expr(p))
		} else {
			append_if_expr(out, parse_constructor_value_expr(p))
		}
		ensure_forward_progress(p, start)
	}
}

parse_constructor_row_expr :: proc(p: ^Parser, kind: Constructor_Body_Kind) -> ^ast.Expr {
	open := expect_token(p, .LParen)
	if open.kind != .LParen {
		return nil
	}
	row := ast.new(ast.Call_Arg_List_Expr, open.range, p.allocator)
	row.args = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	parse_constructor_body_sequence(p, kind, &row.args)
	close := expect_token(p, .RParen)
	if close.kind != .RParen {
		return nil
	}
	row.range = tokenizer.text_range(open.range.start, close.range.end)
	return row
}

parse_constructor_value_expr :: proc(p: ^Parser) -> ^ast.Expr {
	if constructor_args_done(p) || constructor_clause_boundary(p) {
		return nil
	}
	expr := parse_expr(p)
	if expr == nil {
		return nil
	}
	if allow_keyword(p, "OPTIONAL") {
		opt := ast.new(
			ast.Constructor_Optional_Expr,
			tokenizer.text_range(expr.range.start, previous_token(p).range.end),
			p.allocator,
		)
		opt.value = expr
		return opt
	}
	return expr
}

parse_complete_concat_expr :: proc(p: ^Parser, start, end: int) -> ^ast.Expr {
	if start >= end {
		return nil
	}
	count := end - start
	tokens := make([]tokenizer.Token, count + 1, context.temp_allocator)
	for i in 0 ..< count {
		tokens[i] = p.tokens[start + i]
	}
	tokens[count] = tokenizer.Token{kind = .Eof, range = p.tokens[end].range}

	nested := Parser {
		source = p.source,
		path = p.path,
		tokens = tokens,
		previous_index = -1,
		expr_stop_keywords = p.expr_stop_keywords,
		expr_extra_stop_keywords = p.expr_extra_stop_keywords,
		open_sql_expr = p.open_sql_expr,
		errors = make([dynamic]Parse_Error, 0, 1, context.temp_allocator),
		allocator = p.allocator,
	}
	expr := parse_concat_expr(&nested)
	if expr == nil || current_token(&nested).kind != .Eof || len(nested.errors) > 0 {
		return nil
	}
	return expr
}

parse_let_expr :: proc(p: ^Parser, body_kind: Constructor_Body_Kind) -> ^ast.Expr {
	start := expect_keyword(p, "LET")
	if start.kind != .Ident {
		return nil
	}
	expr := ast.new(ast.Let_Expr, start.range, p.allocator)
	expr.bindings = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	expr.body = make([dynamic]^ast.Expr, 0, 2, p.allocator)

	for !constructor_args_done(p) && !at_keyword(p, "IN") {
		start_idx := p.index
		if current_token(p).kind == .Ident && next_token_kind(p, 1) == .Eq {
			append_if_expr(&expr.bindings, parse_constructor_let_binding_expr(p))
		} else {
			bump_token(p)
		}
		ensure_forward_progress(p, start_idx)
	}
	if !allow_keyword(p, "IN") {
		error_current(p, "syntax error: expected keyword")
		return nil
	}
	parse_constructor_body_sequence(p, body_kind, &expr.body)
	expr.range = tokenizer.text_range(start.range.start, previous_token(p).range.end)
	return expr
}

parse_constructor_let_binding_expr :: proc(p: ^Parser) -> ^ast.Expr {
	name := expect_token(p, .Ident)
	expect_token(p, .Eq)
	value := parse_expr(p)
	if value == nil {
		return nil
	}
	expr := ast.new(
		ast.Constructor_Let_Binding_Expr,
		tokenizer.text_range(name.range.start, value.range.end),
		p.allocator,
	)
	expr.name = tokenizer.token_lexeme(name, p.source)
	expr.value = value
	return expr
}

parse_constructor_when_clause_expr :: proc(p: ^Parser, is_switch: bool) -> ^ast.Expr {
	start := expect_keyword(p, "WHEN")
	condition := parse_concat_expr(p) if is_switch else parse_logical_expr(p)
	if condition == nil {
		return nil
	}
	if !allow_keyword(p, "THEN") {
		error_current(p, "syntax error: expected keyword")
		return nil
	}
	result := parse_constructor_value_expr(p)
	if result == nil {
		return nil
	}
	expr := ast.new(
		ast.Constructor_When_Clause_Expr,
		tokenizer.text_range(start.range.start, result.range.end),
		p.allocator,
	)
	expr.condition = condition
	expr.result = result
	return expr
}

parse_constructor_else_clause_expr :: proc(p: ^Parser) -> ^ast.Expr {
	start := expect_keyword(p, "ELSE")
	result := parse_constructor_value_expr(p)
	if result == nil {
		return nil
	}
	expr := ast.new(
		ast.Constructor_Else_Clause_Expr,
		tokenizer.text_range(start.range.start, result.range.end),
		p.allocator,
	)
	expr.result = result
	return expr
}

parse_constructor_for_clause_expr :: proc(p: ^Parser, body_kind: Constructor_Body_Kind) -> ^ast.Expr {
	start := expect_keyword(p, "FOR")
	name := expect_token(p, .Ident)
	if name.kind != .Ident {
		return nil
	}
	expr := ast.new(ast.Constructor_For_Clause_Expr, start.range, p.allocator)
	expr.variable = tokenizer.token_lexeme(name, p.source)
	expr.body = make([dynamic]^ast.Expr, 0, 2, p.allocator)

	if allow_token(p, .Eq) {
		expr.init = parse_expr(p)
		if allow_keyword(p, "THEN") {
			expr.then_expr = parse_expr(p)
		}
		if allow_keyword(p, "WHILE") {
			expr.kind = .For_Then_While
		} else {
			if !allow_keyword(p, "UNTIL") {
				error_current(p, "syntax error: expected keyword")
				return nil
			}
			expr.kind = .For_Then_Until
		}
		expr.condition = parse_logical_expr(p)
	} else if allow_keyword(p, "IN") {
		expr.kind = .For_In
		expr.source = parse_expr(p)
		if at_keyword(p, "WHERE") {
			expr.where_clause = parse_constructor_where_clause_expr(p)
		}
	} else {
		error_current(p, "syntax error: expected keyword")
		return nil
	}

	parse_constructor_body_sequence(p, body_kind, &expr.body)
	expr.range = tokenizer.text_range(start.range.start, previous_token(p).range.end)
	return expr
}

parse_constructor_where_clause_expr :: proc(p: ^Parser) -> ^ast.Expr {
	start := expect_keyword(p, "WHERE")
	if start.kind != .Ident {
		return nil
	}
	open := allow_token(p, .LParen)
	condition := parse_logical_expr(p)
	if condition == nil {
		return nil
	}
	end := condition.range.end
	if open {
		close := expect_token(p, .RParen)
		if close.kind != .RParen {
			return nil
		}
		end = close.range.end
	}
	expr := ast.new(
		ast.Constructor_Where_Clause_Expr,
		tokenizer.text_range(start.range.start, end),
		p.allocator,
	)
	expr.condition = condition
	return expr
}

parse_constructor_init_clause_expr :: proc(p: ^Parser) -> ^ast.Expr {
	start := expect_keyword(p, "INIT")
	expr := ast.new(ast.Constructor_Init_Clause_Expr, start.range, p.allocator)
	expr.assignments = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	parse_constructor_assignment_list(p, &expr.assignments)
	expr.range = tokenizer.text_range(start.range.start, previous_token(p).range.end)
	return expr
}

parse_constructor_next_clause_expr :: proc(p: ^Parser) -> ^ast.Expr {
	start := expect_keyword(p, "NEXT")
	expr := ast.new(ast.Constructor_Next_Clause_Expr, start.range, p.allocator)
	expr.assignments = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	parse_constructor_assignment_list(p, &expr.assignments)
	expr.range = tokenizer.text_range(start.range.start, previous_token(p).range.end)
	return expr
}

parse_constructor_assignment_list :: proc(p: ^Parser, out: ^[dynamic]^ast.Expr) {
	for !constructor_args_done(p) && !constructor_clause_boundary(p) {
		start := p.index
		if current_token(p).kind == .Ident && next_token_kind(p, 1) == .Eq {
			append_if_expr(out, parse_constructor_named_assignment_expr(p))
		} else {
			bump_token(p)
		}
		ensure_forward_progress(p, start)
	}
}

parse_constructor_named_assignment_expr :: proc(p: ^Parser) -> ^ast.Expr {
	name := expect_token(p, .Ident)
	expect_token(p, .Eq)
	value := parse_constructor_value_expr(p)
	if value == nil {
		return nil
	}
	expr := ast.new(
		ast.Constructor_Named_Assignment_Expr,
		tokenizer.text_range(name.range.start, value.range.end),
		p.allocator,
	)
	expr.name = tokenizer.token_lexeme(name, p.source)
	expr.value = value
	return expr
}

parse_constructor_base_clause_expr :: proc(p: ^Parser) -> ^ast.Expr {
	start := expect_keyword(p, "BASE")
	value := parse_constructor_value_expr(p)
	if value == nil {
		return nil
	}
	expr := ast.new(
		ast.Constructor_Base_Clause_Expr,
		tokenizer.text_range(start.range.start, value.range.end),
		p.allocator,
	)
	expr.value = value
	return expr
}

parse_constructor_lines_of_clause_expr :: proc(p: ^Parser) -> ^ast.Expr {
	start := expect_keyword(p, "LINES")
	if !allow_keyword(p, "OF") {
		error_current(p, "syntax error: expected keyword")
		return nil
	}
	expr := ast.new(ast.Constructor_Lines_Of_Clause_Expr, start.range, p.allocator)
	expr.source = parse_expr(p)
	for !constructor_args_done(p) && !constructor_clause_boundary(p) {
		if allow_keyword(p, "FROM") {
			expr.from = parse_expr(p)
		} else if allow_keyword(p, "TO") {
			expr.to = parse_expr(p)
		} else if allow_keyword(p, "USING") {
			allow_keyword(p, "KEY")
			key := expect_token(p, .Ident)
			if key.kind == .Ident {
				expr.using_key = tokenizer.token_lexeme(key, p.source)
			}
		} else {
			break
		}
	}
	expr.range = tokenizer.text_range(start.range.start, previous_token(p).range.end)
	return expr
}

parse_constructor_mapping_clause_expr :: proc(p: ^Parser) -> ^ast.Expr {
	start := expect_keyword(p, "MAPPING")
	expr := ast.new(ast.Constructor_Corresponding_Mapping_Clause_Expr, start.range, p.allocator)
	expr.assignments = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	for !constructor_args_done(p) && !at_keyword(p, "EXCEPT") {
		if allow_token(p, .LParen) {
			append_if_expr(&expr.assignments, parse_constructor_mapping_assignment_expr(p))
			expect_token(p, .RParen)
			continue
		}
		start_idx := p.index
		append_if_expr(&expr.assignments, parse_constructor_mapping_assignment_expr(p))
		ensure_forward_progress(p, start_idx)
	}
	expr.range = tokenizer.text_range(start.range.start, previous_token(p).range.end)
	return expr
}

parse_constructor_mapping_assignment_expr :: proc(p: ^Parser) -> ^ast.Expr {
	name := expect_token(p, .Ident)
	if name.kind != .Ident {
		return nil
	}
	expect_token(p, .Eq)
	expr := ast.new(ast.Constructor_Corresponding_Mapping_Assignment_Expr, name.range, p.allocator)
	expr.target = tokenizer.token_lexeme(name, p.source)

	if allow_keyword(p, "DEFAULT") {
		expr.default_value = parse_expr(p)
	} else if !constructor_args_done(p) && !constructor_mapping_tail_starts(p) {
		expr.source = parse_expr(p)
	}
	if allow_keyword(p, "DISCARDING") {
		allow_keyword(p, "DUPLICATES")
		expr.discarding_duplicates = true
	}
	if allow_keyword(p, "DEFAULT") {
		expr.default_value = parse_expr(p)
	}
	if at_keyword(p, "MAPPING") {
		expr.mapping = parse_constructor_mapping_clause_expr(p)
	}
	if at_keyword(p, "EXCEPT") {
		expr.except = parse_constructor_except_clause_expr(p)
	}
	expr.range = tokenizer.text_range(name.range.start, previous_token(p).range.end)
	return expr
}

parse_constructor_except_clause_expr :: proc(p: ^Parser) -> ^ast.Expr {
	start := expect_keyword(p, "EXCEPT")
	expr := ast.new(ast.Constructor_Corresponding_Except_Clause_Expr, start.range, p.allocator)
	expr.names = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	for !constructor_args_done(p) && !at_keyword(p, "MAPPING") {
		tok := current_token(p)
		if tok.kind != .Ident {
			break
		}
		bump_token(p)
		name := ast.new(ast.Ident_Expr, tok.range, p.allocator)
		name.name = tokenizer.token_lexeme(tok, p.source)
		append(&expr.names, name)
	}
	expr.range = tokenizer.text_range(start.range.start, previous_token(p).range.end)
	return expr
}

parse_constructor_type_ref :: proc(p: ^Parser) -> ^ast.Expr {
	tok := current_token(p)
	if tok.kind == .Hash || tok.kind == .Ident {
		bump_token(p)
		expr := ast.new(ast.Ident_Expr, tok.range, p.allocator)
		expr.name = tokenizer.token_lexeme(tok, p.source)
		return expr
	}
	error_current(p, "syntax error: expected expression")
	return nil
}

parse_is_predicate_expr :: proc(p: ^Parser, subject: ^ast.Expr) -> ^ast.Expr {
	start := subject.range.start
	expect_keyword(p, "IS")
	negated := allow_keyword(p, "NOT")
	if at_keyword(p, "INSTANCE") {
		bump_token(p)
		if !allow_keyword(p, "OF") {
			error_current(p, "syntax error: expected keyword")
			return nil
		}
		type_ref := parse_concat_expr(p)
		if type_ref == nil {
			return nil
		}
		expr := ast.new(
			ast.Instance_Of_Predicate_Expr,
			tokenizer.text_range(start, type_ref.range.end),
			p.allocator,
		)
		expr.subject = subject
		expr.negated = negated
		expr.type_ref = type_ref
		return expr
	}
	if kind, ok := is_predicate_kind(p, current_token(p)); ok {
		end := bump_token(p).range.end
		expr := ast.new(
			ast.Is_Predicate_Expr,
			tokenizer.text_range(start, end),
			p.allocator,
		)
		expr.subject = subject
		expr.negated = negated
		expr.kind = kind
		return expr
	}
	error_current(p, "syntax error: expected predicate")
	return nil
}

parse_between_expr :: proc(p: ^Parser, subject: ^ast.Expr) -> ^ast.Expr {
	start := subject.range.start
	expect_keyword(p, "BETWEEN")
	low := parse_concat_expr(p)
	if low == nil {
		return nil
	}
	if !allow_keyword(p, "AND") {
		error_current(p, "syntax error: expected keyword")
		return nil
	}
	high := parse_concat_expr(p)
	if high == nil {
		return nil
	}
	expr := ast.new(
		ast.Between_Expr,
		tokenizer.text_range(start, high.range.end),
		p.allocator,
	)
	expr.subject = subject
	expr.low = low
	expr.high = high
	return expr
}

parse_sql_case_expr :: proc(p: ^Parser) -> ^ast.Expr {
	start := expect_keyword(p, "CASE")
	expr := ast.new(ast.Sql_Case_Expr, start.range, p.allocator)
	expr.whens = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	if !at_keyword(p, "WHEN") {
		expr.operand = parse_sql_case_part_expr(p, []string{"WHEN"}, false)
	}
	for at_keyword(p, "WHEN") {
		when_start := bump_token(p)
		condition := parse_sql_case_part_expr(p, []string{"THEN"}, expr.operand == nil)
		if condition == nil {
			error_current(p, "syntax error: expected expression")
			break
		}
		if !allow_keyword(p, "THEN") {
			error_current(p, "syntax error: expected keyword")
			break
		}
		result := parse_sql_case_part_expr(p, []string{"WHEN", "ELSE", "END"}, false)
		if result == nil {
			error_current(p, "syntax error: expected expression")
			break
		}
		when_expr := ast.new(
			ast.Sql_Case_When_Expr,
			tokenizer.text_range(when_start.range.start, result.range.end),
			p.allocator,
		)
		when_expr.condition = condition
		when_expr.result = result
		append(&expr.whens, when_expr)
	}
	if allow_keyword(p, "ELSE") {
		expr.else_expr = parse_sql_case_part_expr(p, []string{"END"}, false)
		if expr.else_expr == nil {
			error_current(p, "syntax error: expected expression")
		}
	}
	if !allow_keyword(p, "END") {
		error_current(p, "syntax error: expected keyword")
	}
	expr.range = tokenizer.text_range(start.range.start, previous_token(p).range.end)
	return expr
}

parse_sql_case_part_expr :: proc(p: ^Parser, stop_keywords: []string, logical: bool) -> ^ast.Expr {
	old_stops := p.expr_extra_stop_keywords
	p.expr_extra_stop_keywords = stop_keywords
	defer p.expr_extra_stop_keywords = old_stops
	if logical {
		return parse_logical_expr(p)
	}
	return parse_expr(p)
}

is_predicate_kind :: proc(p: ^Parser, tok: Token) -> (ast.Is_Predicate_Kind, bool) {
	if token_is_keyword(p, tok, "INITIAL") {
		return .Initial, true
	}
	if token_is_keyword(p, tok, "BOUND") {
		return .Bound, true
	}
	if token_is_keyword(p, tok, "ASSIGNED") {
		return .Assigned, true
	}
	if token_is_keyword(p, tok, "REQUESTED") {
		return .Requested, true
	}
	if token_is_keyword(p, tok, "SUPPLIED") {
		return .Supplied, true
	}
	if p.open_sql_expr && token_is_keyword(p, tok, "NULL") {
		return .Null, true
	}
	return .Initial, false
}

sql_case_keyword :: proc(p: ^Parser) -> bool {
	return(
		at_keyword(p, "WHEN") ||
		at_keyword(p, "THEN") ||
		at_keyword(p, "ELSE") ||
		at_keyword(p, "END") \
	)
}

append_if_expr :: proc(list: ^[dynamic]^ast.Expr, expr: ^ast.Expr) {
	if expr != nil {
		append(list, expr)
	}
}

constructor_args_done :: proc(p: ^Parser) -> bool {
	tok := current_token(p)
	return tok.kind == .RParen || tok.kind == .Eof
}

constructor_clause_boundary :: proc(p: ^Parser) -> bool {
	return(
		at_keyword(p, "WHEN") ||
		at_keyword(p, "ELSE") ||
		at_keyword(p, "FOR") ||
		at_keyword(p, "LET") ||
		at_keyword(p, "BASE") ||
		at_keyword(p, "INIT") ||
		at_keyword(p, "NEXT") ||
		at_keyword(p, "WHERE") ||
		at_keyword(p, "MAPPING") ||
		at_keyword(p, "EXCEPT") ||
		at_keyword(p, "UNTIL") ||
		at_keyword(p, "WHILE") ||
		at_keyword(p, "THEN") \
	)
}

constructor_mapping_tail_starts :: proc(p: ^Parser) -> bool {
	return(
		at_keyword(p, "DEFAULT") ||
		at_keyword(p, "DISCARDING") ||
		at_keyword(p, "MAPPING") ||
		at_keyword(p, "EXCEPT") \
	)
}

constructor_body_kind :: proc(p: ^Parser, tok: Token) -> Constructor_Body_Kind {
	if token_is_keyword(p, tok, "CORRESPONDING") {
		return .Corresponding
	}
	if token_is_keyword(p, tok, "COND") {
		return .Cond
	}
	if token_is_keyword(p, tok, "SWITCH") {
		return .Switch
	}
	if token_is_keyword(p, tok, "REDUCE") {
		return .Reduce
	}
	return .Value
}

parse_data_inline_name_expr :: proc(p: ^Parser) -> ^ast.Expr {
	start := expect_keyword(p, "DATA")
	expect_token(p, .LParen)
	name := expect_token(p, .Ident)
	close := expect_token(p, .RParen)
	if close.kind != .RParen {
		return nil
	}
	expr := ast.new(
		ast.Data_Inline_Name_Expr,
		tokenizer.text_range(start.range.start, close.range.end),
		p.allocator,
	)
	expr.name = tokenizer.token_lexeme(name, p.source)
	return expr
}

parse_field_symbol_inline_name_expr :: proc(p: ^Parser) -> ^ast.Expr {
	start := expect_keyword(p, "FIELD")
	expect_token(p, .Minus)
	expect_keyword(p, "SYMBOL")
	expect_token(p, .LParen)
	name := expect_token(p, .Ident)
	close := expect_token(p, .RParen)
	if close.kind != .RParen {
		return nil
	}
	expr := ast.new(
		ast.Field_Symbol_Inline_Name_Expr,
		tokenizer.text_range(start.range.start, close.range.end),
		p.allocator,
	)
	expr.name = tokenizer.token_lexeme(name, p.source)
	return expr
}

parse_char_string_template_expr :: proc(p: ^Parser) -> ^ast.Expr {
	open := expect_token(p, .StringTemplate)
	if open.kind != .StringTemplate {
		return nil
	}

	expr := ast.new(ast.Char_String_Template_Expr, open.range, p.allocator)
	expr.parts = make([dynamic]^ast.Expr, 0, 4, p.allocator)

	for {
		tok := current_token(p)
		#partial switch tok.kind {
		case .StringTemplateLit:
			bump_token(p)
			lit := ast.new(ast.Template_Literal_Expr, tok.range, p.allocator)
			lit.literal = tokenizer.token_lexeme(tok, p.source)
			append(&expr.parts, lit)
		case .LBrace:
			interp := parse_template_interpolation_expr(p)
			if interp == nil {
				return nil
			}
			append(&expr.parts, interp)
		case .StringTemplate:
			close := bump_token(p)
			expr.range = tokenizer.text_range(open.range.start, close.range.end)
			return expr
		case .Eof:
			error_current(p, "syntax error: expected string template close")
			return nil
		case:
			bump_token(p)
		}
	}
}

parse_template_interpolation_expr :: proc(p: ^Parser) -> ^ast.Expr {
	open := expect_token(p, .LBrace)
	if open.kind != .LBrace {
		return nil
	}

	interp := ast.new(ast.Template_Interpolation_Expr, open.range, p.allocator)
	interp.format_specs = make([dynamic]^ast.Expr, 0, 2, p.allocator)

	body: ^ast.Expr
	if !template_format_spec_starts(p) {
		body = parse_expr(p)
	}
	if body == nil {
		bad := ast.new(ast.Bad_Expr, current_token(p).range, p.allocator)
		interp.expr = bad
	} else {
		wrapper := ast.new(ast.Template_Expr, body.range, p.allocator)
		wrapper.expr = body
		interp.expr = wrapper
	}

	for current_token(p).kind != .RBrace && current_token(p).kind != .Eof {
		if template_format_spec_starts(p) {
			spec := parse_template_format_spec_expr(p)
			if spec != nil {
				append(&interp.format_specs, spec)
				continue
			}
		}
		bump_token(p)
	}

	close := expect_token(p, .RBrace)
	if close.kind != .RBrace {
		return nil
	}
	interp.range = tokenizer.text_range(open.range.start, close.range.end)
	return interp
}

parse_template_format_spec_expr :: proc(p: ^Parser) -> ^ast.Expr {
	name := expect_token(p, .Ident)
	if name.kind != .Ident {
		return nil
	}
	eq := expect_token(p, .Eq)
	if eq.kind != .Eq {
		return nil
	}
	value := parse_expr(p)
	if value == nil {
		return value
	}

	spec := ast.new(
		ast.Template_Format_Spec_Expr,
		tokenizer.text_range(name.range.start, value.range.end),
		p.allocator,
	)
	spec.name = tokenizer.token_lexeme(name, p.source)
	spec.value = value
	return spec
}

template_format_spec_starts :: proc(p: ^Parser) -> bool {
	if current_token(p).kind != .Ident {
		return false
	}
	next := next_significant_index(p.index + 1)
	return(
		next < len(p.tokens) &&
		p.tokens[next].kind == .Eq &&
		template_format_name(p, current_token(p)) \
	)
}

build_binary_expr :: proc(
	p: ^Parser,
	left: ^ast.Expr,
	op: ast.Binary_Op,
	right: ^ast.Expr,
) -> ^ast.Expr {
	expr := ast.new(
		ast.Binary_Expr,
		tokenizer.text_range(left.range.start, right.range.end),
		p.allocator,
	)
	expr.left = left
	expr.op = op
	expr.right = right
	return expr
}

comparison_op :: proc(p: ^Parser, tok: Token) -> (ast.Binary_Op, bool) {
	#partial switch tok.kind {
	case .Eq:
		return .Equal, true
	case .Ne:
		return .Not_Equal, true
	case .Lt:
		return .Less, true
	case .Le:
		return .Less_Equal, true
	case .Gt:
		return .Greater, true
	case .Ge:
		return .Greater_Equal, true
	}
	if token_is_keyword(p, tok, "EQ") {
		return .Equal, true
	}
	if token_is_keyword(p, tok, "NE") {
		return .Not_Equal, true
	}
	if token_is_keyword(p, tok, "LT") {
		return .Less, true
	}
	if token_is_keyword(p, tok, "LE") {
		return .Less_Equal, true
	}
	if token_is_keyword(p, tok, "GT") {
		return .Greater, true
	}
	if token_is_keyword(p, tok, "GE") {
		return .Greater_Equal, true
	}
	if token_is_keyword(p, tok, "CO") {
		return .Contains_Only, true
	}
	if token_is_keyword(p, tok, "CN") {
		return .Contains_Not_Only, true
	}
	if token_is_keyword(p, tok, "CA") {
		return .Contains_Any, true
	}
	if token_is_keyword(p, tok, "NA") {
		return .Contains_Not_Any, true
	}
	if token_is_keyword(p, tok, "CS") {
		return .Contains_String, true
	}
	if token_is_keyword(p, tok, "NS") {
		return .Contains_No_String, true
	}
	if token_is_keyword(p, tok, "CP") {
		return .Covers_Pattern, true
	}
	if token_is_keyword(p, tok, "NP") {
		return .Covers_No_Pattern, true
	}
	if token_is_keyword(p, tok, "IN") {
		return .In, true
	}
	if token_is_keyword(p, tok, "O") {
		return .Bit_O, true
	}
	if token_is_keyword(p, tok, "Z") {
		return .Bit_Z, true
	}
	if token_is_keyword(p, tok, "M") {
		return .Bit_M, true
	}
	return .Add, false
}

selector_operator_starts :: proc(prev, tok: Token) -> bool {
	if tok.kind == .Arrow || tok.kind == .FatArrow || tok.kind == .Tilde {
		return true
	}
	return tok.kind == .Minus && tokens_touch(prev, tok)
}

selector_op :: proc(kind: tokenizer.Token_Kind) -> ast.Selector_Op {
	#partial switch kind {
	case .Arrow:
		return .Arrow
	case .FatArrow:
		return .Fat_Arrow
	case .Tilde:
		return .Tilde
	}
	return .Dash
}

tokens_touch :: proc(lhs, rhs: Token) -> bool {
	return lhs.kind != .Eof && rhs.kind != .Eof && lhs.range.end == rhs.range.start
}

has_space_between :: proc(lhs, rhs: Token) -> bool {
	return lhs.kind != .Eof && rhs.kind != .Eof && lhs.range.end < rhs.range.start
}

matching_group_index :: proc(p: ^Parser, start: int, open, close: tokenizer.Token_Kind) -> int {
	if start >= len(p.tokens) || p.tokens[start].kind != open {
		return -1
	}
	depth := 0
	for i in start ..< len(p.tokens) {
		if p.tokens[i].kind == open {
			depth += 1
		} else if p.tokens[i].kind == close {
			depth -= 1
			if depth == 0 {
				return i
			}
		}
	}
	return -1
}

find_tight_lparen_for_substring :: proc(p: ^Parser, start: int) -> int {
	for i in start ..< len(p.tokens) {
		tok := p.tokens[i]
		if tok.kind == .LParen && i > start && tokens_touch(p.tokens[i - 1], tok) {
			return i
		}
		if tok.kind == .Period ||
		   tok.kind == .Comma ||
		   tok.kind == .Eq ||
		   tok.kind == .QuestionEq ||
		   tok.kind == .RParen {
			return -1
		}
	}
	return -1
}

call_padding_is_valid :: proc(p: ^Parser, lparen_idx, rparen_idx: int) -> bool {
	if lparen_idx < 0 ||
	   rparen_idx < 0 ||
	   lparen_idx >= len(p.tokens) ||
	   rparen_idx >= len(p.tokens) {
		return false
	}
	lparen := p.tokens[lparen_idx]
	if rparen_idx == lparen_idx + 1 {
		return has_space_between(lparen, p.tokens[rparen_idx])
	}
	return has_space_between(lparen, p.tokens[lparen_idx + 1])
}

node_can_start_substring :: proc(node: ^ast.Expr) -> bool {
	if node == nil {
		return false
	}
	#partial switch _ in node.derived_expr {
	case ^ast.Ident_Expr:
		return true
	case ^ast.Selector_Expr:
		return true
	case ^ast.Table_Expr:
		return true
	}
	return false
}

constructor_keyword :: proc(p: ^Parser, tok: Token) -> bool {
	return(
		token_is_keyword(p, tok, "NEW") ||
		token_is_keyword(p, tok, "VALUE") ||
		token_is_keyword(p, tok, "CONV") ||
		token_is_keyword(p, tok, "REF") ||
		token_is_keyword(p, tok, "CAST") ||
		token_is_keyword(p, tok, "EXACT") ||
		token_is_keyword(p, tok, "CORRESPONDING") ||
		token_is_keyword(p, tok, "FILTER") ||
		token_is_keyword(p, tok, "REDUCE") ||
		token_is_keyword(p, tok, "SWITCH") ||
		token_is_keyword(p, tok, "COND") ||
		token_is_keyword(p, tok, "THROW") \
	)
}

constructor_expr_starts :: proc(p: ^Parser, tok: Token) -> bool {
	if !constructor_keyword(p, tok) || compact_lparen_after_current(p) {
		return false
	}
	next := p.tokens[p.index + 1] if p.index + 1 < len(p.tokens) else tokenizer.Token{}
	if next.kind == .Eq ||
	   next.kind == .Period ||
	   next.kind == .Comma ||
	   next.kind == .RParen ||
	   next.kind == .RBracket {
		return false
	}
	if _, ok := comparison_op(p, next); ok {
		return false
	}
	return true
}

compact_lparen_after_current :: proc(p: ^Parser) -> bool {
	return next_token_kind(p, 1) == .LParen && tokens_touch(current_token(p), p.tokens[p.index + 1])
}

constructor_kind :: proc(p: ^Parser, tok: Token) -> ast.Constructor_Kind {
	if token_is_keyword(p, tok, "NEW") {
		return .New
	}
	if token_is_keyword(p, tok, "VALUE") {
		return .Value
	}
	if token_is_keyword(p, tok, "CONV") {
		return .Conv
	}
	if token_is_keyword(p, tok, "REF") {
		return .Ref
	}
	if token_is_keyword(p, tok, "CAST") {
		return .Cast
	}
	if token_is_keyword(p, tok, "EXACT") {
		return .Exact
	}
	if token_is_keyword(p, tok, "CORRESPONDING") {
		return .Corresponding
	}
	if token_is_keyword(p, tok, "FILTER") {
		return .Filter
	}
	if token_is_keyword(p, tok, "REDUCE") {
		return .Reduce
	}
	if token_is_keyword(p, tok, "SWITCH") {
		return .Switch
	}
	if token_is_keyword(p, tok, "THROW") {
		return .Throw
	}
	return .Cond
}

call_argument_section_starts :: proc(p: ^Parser) -> bool {
	return call_argument_section_kind(p, current_token(p)) != .Unknown
}

call_argument_section_kind :: proc(p: ^Parser, tok: Token) -> ast.Call_Arg_Section_Kind {
	if token_is_keyword(p, tok, "EXPORTING") {return .Exporting}
	if token_is_keyword(p, tok, "IMPORTING") {return .Importing}
	if token_is_keyword(p, tok, "CHANGING") {return .Changing}
	if token_is_keyword(p, tok, "TABLES") {return .Tables}
	if token_is_keyword(p, tok, "RECEIVING") {return .Receiving}
	if token_is_keyword(p, tok, "EXCEPTIONS") {return .Exceptions}
	return .Unknown
}

template_format_name :: proc(p: ^Parser, tok: Token) -> bool {
	return(
		token_is_keyword(p, tok, "WIDTH") ||
		token_is_keyword(p, tok, "ALIGN") ||
		token_is_keyword(p, tok, "DECIMALS") ||
		token_is_keyword(p, tok, "ALPHA") ||
		token_is_keyword(p, tok, "TIMESTAMP") ||
		token_is_keyword(p, tok, "DATE") ||
		token_is_keyword(p, tok, "TIME") \
	)
}
