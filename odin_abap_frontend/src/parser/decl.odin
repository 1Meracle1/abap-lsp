package abap_frontend_parser

import "../ast"
import "../tokenizer"

import "core:mem"
import "core:strings"

decl_stmt_starts :: proc(p: ^Parser) -> bool {
	return(
		at_keyword(p, "DATA") ||
		at_keyword(p, "TYPES") ||
		at_keyword(p, "CONSTANTS") ||
		at_keyword_phrase(p, "FIELD-SYMBOLS") ||
		at_keyword(p, "STATICS") ||
		at_keyword(p, "TABLES") ||
		at_keyword(p, "RANGES") ||
		at_keyword(p, "PARAMETERS") ||
		at_keyword(p, "PARAMETER") ||
		at_keyword_phrase(p, "SELECT-OPTIONS") ||
		at_keyword(p, "CONTROLS") ||
		at_keyword_phrase(p, "CLASS-DATA") ||
		at_keyword_phrase(p, "TYPE-POOLS") ||
		at_keyword_phrase(p, "FUNCTION-POOL") ||
		(at_keyword(p, "INCLUDE") &&
				(at_keyword_index(p, p.index + 1, "TYPE") ||
				 at_keyword_index(p, p.index + 1, "STRUCTURE"))) \
	)
}

parse_decl_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	if at_keyword(p, "DATA") {
		return parse_data_decl_stmt(p)
	}
	if at_keyword(p, "INCLUDE") {
		return parse_standalone_include_decl_stmt(p)
	}
	if at_keyword(p, "TYPES") {
		return parse_types_decl_stmt(p)
	}
	if at_keyword(p, "CONSTANTS") {
		return parse_constants_decl_stmt(p)
	}
	if at_keyword_phrase(p, "FIELD-SYMBOLS") {
		return parse_field_symbols_decl_stmt(p)
	}
	if at_keyword(p, "STATICS") {
		return parse_statics_decl_stmt(p)
	}
	if at_keyword(p, "TABLES") {
		return parse_tables_decl_stmt(p)
	}
	if at_keyword(p, "RANGES") {
		return parse_ranges_decl_stmt(p)
	}
	if at_keyword(p, "PARAMETERS") || at_keyword(p, "PARAMETER") {
		return parse_parameters_decl_stmt(p)
	}
	if at_keyword_phrase(p, "SELECT-OPTIONS") {
		return parse_select_options_decl_stmt(p)
	}
	if at_keyword(p, "CONTROLS") {
		return parse_controls_decl_stmt(p)
	}
	if at_keyword_phrase(p, "CLASS-DATA") {
		return parse_class_data_decl_stmt(p)
	}
	if at_keyword_phrase(p, "TYPE-POOLS") {
		return parse_type_pools_decl_stmt(p)
	}
	return parse_function_pool_decl_stmt(p)
}

parse_data_decl_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "DATA")
	if !token_is_keyword(p, start, "DATA") {
		return nil
	}

	if current_token(p).kind == .LParen {
		return parse_data_inline_decl_stmt(p, start)
	}

	has_colon := allow_token(p, .Colon)
	branch, ok := parse_data_decl_clause(p)
	if !ok {
		return nil
	}

	if has_colon {
		stmt := ast.new(ast.Data_Chained_Decl, start.range, p.allocator)
		stmt.decls = make([dynamic]ast.Data_Chained_Branch, 0, 2, p.allocator)
		append(&stmt.decls, branch)
		for allow_token(p, .Comma) {
			if current_token(p).kind == .Period || current_token(p).kind == .Eof {
				error_current(p, "syntax error: expected declaration after ','")
				break
			}
			next_branch, next_ok := parse_data_decl_clause(p)
			if !next_ok {
				return nil
			}
			append(&stmt.decls, next_branch)
		}
		period := expect_token_message(p, .Period, "syntax error: expected '.'")
		if period.kind != .Period {
			return nil
		}
		assign_decl_depths(&stmt.decls)
		stmt.range = tokenizer.text_range(start.range.start, statement_end(p, period))
		return stmt
	}

	period := expect_token_message(p, .Period, "syntax error: expected '.'")
	if period.kind != .Period {
		return nil
	}
	stmt := ast.new(
		ast.Data_Decl,
		tokenizer.text_range(start.range.start, statement_end(p, period)),
		p.allocator,
	)
	stmt.kind = branch.kind
	stmt.flags = branch.flags
	stmt.name = branch.name
	stmt.paren_length = branch.paren_length
	stmt.length_clauses = branch.length_clauses
	stmt.type_clause = branch.type_clause
	stmt.value_clause = branch.value_clause
	stmt.occurs = branch.occurs
	stmt.include_ref = branch.include_ref
	stmt.as_name = branch.as_name
	stmt.renaming_suffix = branch.renaming_suffix
	stmt.read_only = branch.read_only
	return stmt
}

parse_data_inline_decl_stmt :: proc(p: ^Parser, start: Token) -> ^ast.Stmt {
	expect_token(p, .LParen)
	name := expect_token(p, .Ident)
	close := expect_token(p, .RParen)
	if close.kind != .RParen {
		return nil
	}
	if !allow_token(p, .Eq) {
		error_current(p, "syntax error: expected assignment operator")
		return nil
	}
	if !expr_lead_token(current_token(p)) {
		error_current(p, "syntax error: expected expression after '=' in inline DATA declaration")
		return nil
	}
	value := parse_expr(p)
	if value == nil {
		error_current(p, "syntax error: expected expression after '=' in inline DATA declaration")
		return nil
	}
	period := expect_token_message(
		p,
		.Period,
		"syntax error: expected '.' after inline DATA declaration",
	)
	if period.kind != .Period {
		return nil
	}
	stmt := ast.new(
		ast.Data_Inline_Decl,
		tokenizer.text_range(start.range.start, statement_end(p, period)),
		p.allocator,
	)
	stmt.name = tokenizer.token_lexeme(name, p.source)
	stmt.expr = value
	return stmt
}

parse_types_decl_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "TYPES")
	allow_token(p, .Colon)
	stmt := ast.new(ast.Types_Decl, start.range, p.allocator)
	stmt.types = make([dynamic]ast.Types_Clause, 0, 2, p.allocator)
	for !decl_clause_boundary(p) {
		clause, ok := parse_types_clause(p)
		if !ok {
			return nil
		}
		append(&stmt.types, clause)
		if !allow_token(p, .Comma) {
			break
		}
	}
	period := expect_token(p, .Period)
	assign_decl_depths(&stmt.types)
	stmt.range = tokenizer.text_range(start.range.start, statement_end(p, period))
	return stmt
}

parse_constants_decl_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "CONSTANTS")
	allow_token(p, .Colon)
	stmt := ast.new(ast.Constants_Decl, start.range, p.allocator)
	stmt.constants = make([dynamic]ast.Constants_Clause, 0, 2, p.allocator)
	for !decl_clause_boundary(p) {
		clause, ok := parse_constants_clause(p)
		if !ok {
			return nil
		}
		append(&stmt.constants, clause)
		if !allow_token(p, .Comma) {
			break
		}
	}
	period := expect_token(p, .Period)
	assign_decl_depths(&stmt.constants)
	stmt.range = tokenizer.text_range(start.range.start, statement_end(p, period))
	return stmt
}

parse_field_symbols_decl_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword_phrase(p, "FIELD-SYMBOLS")
	allow_token(p, .Colon)
	stmt := ast.new(ast.Field_Symbols_Decl, start.range, p.allocator)
	stmt.field_symbols = make([dynamic]ast.Field_Symbols_Clause, 0, 2, p.allocator)
	for !decl_clause_boundary(p) {
		clause, ok := parse_field_symbols_clause(p)
		if !ok {
			return nil
		}
		append(&stmt.field_symbols, clause)
		if !allow_token(p, .Comma) {
			break
		}
	}
	period := expect_token(p, .Period)
	stmt.range = tokenizer.text_range(start.range.start, statement_end(p, period))
	return stmt
}

parse_statics_decl_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "STATICS")
	allow_token(p, .Colon)
	stmt := ast.new(ast.Statics_Decl, start.range, p.allocator)
	stmt.statics = make([dynamic]ast.Statics_Clause, 0, 2, p.allocator)
	for !decl_clause_boundary(p) {
		clause, ok := parse_statics_clause(p)
		if !ok {
			return nil
		}
		append(&stmt.statics, clause)
		if !allow_token(p, .Comma) {
			break
		}
	}
	period := expect_token(p, .Period)
	assign_decl_depths(&stmt.statics)
	stmt.range = tokenizer.text_range(start.range.start, statement_end(p, period))
	return stmt
}

parse_tables_decl_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "TABLES")
	allow_token(p, .Colon)
	stmt := ast.new(ast.Tables_Decl, start.range, p.allocator)
	stmt.tables = make([dynamic]ast.Tables_Clause, 0, 2, p.allocator)
	for !decl_clause_boundary(p) {
		clause, ok := parse_tables_clause(p)
		if !ok {
			return nil
		}
		append(&stmt.tables, clause)
		if !allow_token(p, .Comma) {
			break
		}
	}
	period := expect_token(p, .Period)
	stmt.range = tokenizer.text_range(start.range.start, statement_end(p, period))
	return stmt
}

parse_ranges_decl_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "RANGES")
	allow_token(p, .Colon)
	stmt := ast.new(ast.Ranges_Decl, start.range, p.allocator)
	stmt.ranges = make([dynamic]ast.Ranges_Clause, 0, 2, p.allocator)
	for !decl_clause_boundary(p) {
		clause, ok := parse_ranges_clause(p)
		if !ok {
			return nil
		}
		append(&stmt.ranges, clause)
		if !allow_token(p, .Comma) {
			break
		}
	}
	period := expect_token(p, .Period)
	stmt.range = tokenizer.text_range(start.range.start, statement_end(p, period))
	return stmt
}

parse_parameters_decl_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := bump_token(p)
	allow_token(p, .Colon)
	stmt := ast.new(ast.Parameters_Decl, start.range, p.allocator)
	stmt.parameters = make([dynamic]ast.Parameters_Clause, 0, 2, p.allocator)
	for !decl_clause_boundary(p) {
		clause, ok := parse_parameters_clause(p)
		if !ok {
			return nil
		}
		append(&stmt.parameters, clause)
		if !allow_token(p, .Comma) {
			break
		}
	}
	period := expect_token(p, .Period)
	stmt.range = tokenizer.text_range(start.range.start, statement_end(p, period))
	return stmt
}

parse_select_options_decl_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword_phrase(p, "SELECT-OPTIONS")
	allow_token(p, .Colon)
	stmt := ast.new(ast.Select_Options_Decl, start.range, p.allocator)
	stmt.options = make([dynamic]ast.Select_Options_Clause, 0, 2, p.allocator)
	for !decl_clause_boundary(p) {
		clause, ok := parse_select_options_clause(p)
		if !ok {
			return nil
		}
		append(&stmt.options, clause)
		if !allow_token(p, .Comma) {
			break
		}
	}
	period := expect_token(p, .Period)
	stmt.range = tokenizer.text_range(start.range.start, statement_end(p, period))
	return stmt
}

parse_controls_decl_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "CONTROLS")
	allow_token(p, .Colon)
	stmt := ast.new(ast.Controls_Decl, start.range, p.allocator)
	stmt.controls = make([dynamic]ast.Controls_Clause, 0, 2, p.allocator)
	for !decl_clause_boundary(p) {
		clause, ok := parse_controls_clause(p)
		if !ok {
			return nil
		}
		append(&stmt.controls, clause)
		if !allow_token(p, .Comma) {
			break
		}
	}
	period := expect_token(p, .Period)
	stmt.range = tokenizer.text_range(start.range.start, statement_end(p, period))
	return stmt
}

parse_class_data_decl_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword_phrase(p, "CLASS-DATA")
	allow_token(p, .Colon)
	stmt := ast.new(ast.Class_Data_Decl, start.range, p.allocator)
	stmt.decls = make([dynamic]ast.Class_Data_Clause, 0, 2, p.allocator)
	for !decl_clause_boundary(p) {
		clause, ok := parse_class_data_clause(p)
		if !ok {
			return nil
		}
		append(&stmt.decls, clause)
		if !allow_token(p, .Comma) {
			break
		}
	}
	period := expect_token(p, .Period)
	assign_decl_depths(&stmt.decls)
	stmt.range = tokenizer.text_range(start.range.start, statement_end(p, period))
	return stmt
}

parse_type_pools_decl_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword_phrase(p, "TYPE-POOLS")
	allow_token(p, .Colon)
	stmt := ast.new(ast.Type_Pools_Decl, start.range, p.allocator)
	stmt.pools = make([dynamic]string, 0, 2, p.allocator)
	for !decl_clause_boundary(p) {
		name, _, ok := parse_decl_name(p)
		if !ok {
			return nil
		}
		append(&stmt.pools, tokenizer.token_lexeme(name, p.source))
		if !allow_token(p, .Comma) {
			break
		}
	}
	period := expect_token(p, .Period)
	stmt.range = tokenizer.text_range(start.range.start, statement_end(p, period))
	return stmt
}

parse_standalone_include_decl_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := current_token(p)
	clause, ok := parse_types_clause(p)
	if !ok {
		return nil
	}
	period := expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	stmt := ast.new(ast.Types_Decl, tokenizer.text_range(start.range.start, statement_end(p, period)), p.allocator)
	stmt.types = make([dynamic]ast.Types_Clause, 0, 1, p.allocator)
	append(&stmt.types, clause)
	assign_decl_depths(&stmt.types)
	return stmt
}

parse_function_pool_decl_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword_phrase(p, "FUNCTION-POOL")
	stmt := ast.new(ast.Function_Pool_Decl, start.range, p.allocator)
	name, _, ok := parse_decl_name(p)
	if !ok {
		return nil
	}
	stmt.name = tokenizer.token_lexeme(name, p.source)
	for !decl_clause_boundary(p) {
		if at_keyword_phrase(p, "MESSAGE-ID") {
			expect_keyword_phrase(p, "MESSAGE-ID")
			id, id_ok := parse_required_addition_name(p)
			if !id_ok {
				return nil
			}
			stmt.message_id = id
			continue
		}
		bump_token(p)
	}
	period := expect_token(p, .Period)
	stmt.range = tokenizer.text_range(start.range.start, statement_end(p, period))
	return stmt
}

assign_decl_depths :: proc(list: ^[dynamic]$T) {
	depth := 0
	for i in 0 ..< len(list^) {
		if list^[i].kind == .End_Group && depth > 0 {
			depth -= 1
		}
		list^[i].depth = depth
		if list^[i].kind == .Begin_Group {
			depth += 1
		}
	}
}

parse_data_decl_clause :: proc(p: ^Parser) -> (ast.Data_Chained_Branch, bool) {
	kind, is_common_part_delimiter, name, include_ref, name_index, ok := parse_decl_clause_head(p)
	if !ok {
		return ast.Data_Chained_Branch{}, false
	}
	clause := ast.Data_Chained_Branch {
		kind           = kind,
		name           = name,
		include_ref    = include_ref,
		length_clauses = make([dynamic]ast.Length_Clause, 0, 2, p.allocator),
	}
	if is_common_part_delimiter {
		clause.flags += {.Common_Part_Delimiter}
	}
	if current_token(p).kind == .LParen {
		clause.paren_length = parse_required_paren_length_clause(p)
		if clause.paren_length == nil {
			return ast.Data_Chained_Branch{}, false
		}
	}
	for !decl_clause_end(p, name_index) {
		if parse_group_or_include_addition(p, &clause.occurs, &clause.as_name, &clause.renaming_suffix) {
			continue
		}
		if at_length_keyword(p) {
			length_clause, length_ok := parse_required_length_clause(p)
			if !length_ok {
				return ast.Data_Chained_Branch{}, false
			}
			append(&clause.length_clauses, length_clause)
			continue
		}
		if at_keyword(p, "TYPE") || at_keyword(p, "LIKE") {
			clause.type_clause = parse_required_type_clause(p)
			if clause.type_clause == nil {
				return ast.Data_Chained_Branch{}, false
			}
			continue
		}
		if at_keyword(p, "VALUE") {
			clause.value_clause = parse_required_value_clause(p)
			if clause.value_clause == nil {
				return ast.Data_Chained_Branch{}, false
			}
			continue
		}
		if at_keyword_phrase(p, "READ-ONLY") {
			expect_keyword_phrase(p, "READ-ONLY")
			clause.read_only = true
			continue
		}
		bump_token(p)
	}
	return clause, true
}

parse_decl_clause_head :: proc(
	p: ^Parser,
) -> (
	ast.Decl_Clause_Kind,
	bool,
	string,
	^ast.Expr,
	int,
	bool,
) {
	index := p.index
	if allow_keyword(p, "BEGIN") {
		if !allow_keyword(p, "OF") {
			error_current(p, "syntax error: expected keyword")
			return .Normal, false, "", nil, index, false
		}
		if name, ok := parse_common_part_delimiter_tail(p); ok {
			return .Begin_Group, true, name, nil, index, true
		}
		name, _, ok := parse_decl_name(p)
		if !ok {
			return .Normal, false, "", nil, index, false
		}
		return .Begin_Group, false, tokenizer.token_lexeme(name, p.source), nil, index, true
	}
	if allow_keyword(p, "END") {
		if !allow_keyword(p, "OF") {
			error_current(p, "syntax error: expected keyword")
			return .Normal, false, "", nil, index, false
		}
		if name, ok := parse_common_part_delimiter_tail(p); ok {
			return .End_Group, true, name, nil, index, true
		}
		name, _, ok := parse_decl_name(p)
		if !ok {
			return .Normal, false, "", nil, index, false
		}
		return .End_Group, false, tokenizer.token_lexeme(name, p.source), nil, index, true
	}
	if allow_keyword(p, "INCLUDE") {
		kind := ast.Decl_Clause_Kind.Include_Type
		if allow_keyword(p, "TYPE") {
			kind = .Include_Type
		} else if allow_keyword(p, "STRUCTURE") {
			kind = .Include_Structure
		} else {
			error_current(p, "syntax error: expected keyword")
			return .Normal, false, "", nil, index, false
		}
		ref := parse_type_ref_expr(p)
		if ref == nil {
			return .Normal, false, "", nil, index, false
		}
		return kind, false, "", ref, index, true
	}
	name, name_index, ok := parse_decl_name(p)
	if !ok {
		return .Normal, false, "", nil, index, false
	}
	return .Normal, false, tokenizer.token_lexeme(name, p.source), nil, name_index, true
}

parse_common_part_delimiter_tail :: proc(p: ^Parser) -> (string, bool) {
	if !at_keyword(p, "COMMON") || !at_keyword_index(p, p.index + 1, "PART") {
		return "", false
	}
	expect_keyword(p, "COMMON")
	expect_keyword(p, "PART")
	tok := current_token(p)
	if tok.kind == .Ident || tok.kind == .Number || tok.kind == .Star {
		name, _, ok := parse_decl_name(p)
		if ok {
			return tokenizer.token_lexeme(name, p.source), true
		}
	}
	return "", true
}

parse_types_clause :: proc(p: ^Parser) -> (ast.Types_Clause, bool) {
	kind, is_common_part_delimiter, name, include_ref, name_index, ok := parse_decl_clause_head(p)
	if !ok {
		return ast.Types_Clause{}, false
	}
	clause := ast.Types_Clause {
		kind           = kind,
		name           = name,
		include_ref    = include_ref,
		length_clauses = make([dynamic]ast.Length_Clause, 0, 2, p.allocator),
	}
	if is_common_part_delimiter {
		clause.flags += {.Common_Part_Delimiter}
	}
	if current_token(p).kind == .LParen {
		clause.paren_length = parse_required_paren_length_clause(p)
		if clause.paren_length == nil {
			return ast.Types_Clause{}, false
		}
	}
	for !decl_clause_end(p, name_index) {
		if parse_group_or_include_addition(p, &clause.occurs, &clause.as_name, &clause.renaming_suffix) {
			continue
		}
		if at_length_keyword(p) {
			length_clause, length_ok := parse_required_length_clause(p)
			if !length_ok {
				return ast.Types_Clause{}, false
			}
			append(&clause.length_clauses, length_clause)
			continue
		}
		if at_keyword(p, "TYPE") || at_keyword(p, "LIKE") {
			clause.type_clause = parse_required_type_clause(p)
			if clause.type_clause == nil {
				return ast.Types_Clause{}, false
			}
			continue
		}
		bump_token(p)
	}
	return clause, true
}

parse_constants_clause :: proc(p: ^Parser) -> (ast.Constants_Clause, bool) {
	kind, is_common_part_delimiter, name, include_ref, name_index, ok := parse_decl_clause_head(p)
	if !ok {
		return ast.Constants_Clause{}, false
	}
	clause := ast.Constants_Clause {
		kind           = kind,
		name           = name,
		include_ref    = include_ref,
		length_clauses = make([dynamic]ast.Length_Clause, 0, 2, p.allocator),
	}
	if is_common_part_delimiter {
		clause.flags += {.Common_Part_Delimiter}
	}
	if current_token(p).kind == .LParen {
		clause.paren_length = parse_required_paren_length_clause(p)
		if clause.paren_length == nil {
			return ast.Constants_Clause{}, false
		}
	}
	for !decl_clause_end(p, name_index) {
		if parse_group_or_include_addition(p, &clause.occurs, &clause.as_name, &clause.renaming_suffix) {
			continue
		}
		if at_length_keyword(p) {
			length_clause, length_ok := parse_required_length_clause(p)
			if !length_ok {
				return ast.Constants_Clause{}, false
			}
			append(&clause.length_clauses, length_clause)
			continue
		}
		if at_keyword(p, "TYPE") || at_keyword(p, "LIKE") {
			clause.type_clause = parse_required_type_clause(p)
			if clause.type_clause == nil {
				return ast.Constants_Clause{}, false
			}
			continue
		}
		if at_keyword(p, "VALUE") {
			clause.value_clause = parse_required_value_clause(p)
			if clause.value_clause == nil {
				return ast.Constants_Clause{}, false
			}
			continue
		}
		bump_token(p)
	}
	return clause, true
}

parse_field_symbols_clause :: proc(p: ^Parser) -> (ast.Field_Symbols_Clause, bool) {
	name, name_index, ok := parse_decl_name(p)
	if !ok {
		return ast.Field_Symbols_Clause{}, false
	}
	clause := ast.Field_Symbols_Clause {
		name = tokenizer.token_lexeme(name, p.source),
	}
	for !decl_clause_end(p, name_index) {
		if at_keyword(p, "TYPE") || at_keyword(p, "LIKE") {
			clause.type_clause = parse_required_type_clause(p)
			if clause.type_clause == nil {
				return ast.Field_Symbols_Clause{}, false
			}
			continue
		}
		bump_token(p)
	}
	return clause, true
}

parse_statics_clause :: proc(p: ^Parser) -> (ast.Statics_Clause, bool) {
	kind, is_common_part_delimiter, name, include_ref, name_index, ok := parse_decl_clause_head(p)
	if !ok {
		return ast.Statics_Clause{}, false
	}
	clause := ast.Statics_Clause {
		kind           = kind,
		name           = name,
		include_ref    = include_ref,
		length_clauses = make([dynamic]ast.Length_Clause, 0, 2, p.allocator),
	}
	if is_common_part_delimiter {
		clause.flags += {.Common_Part_Delimiter}
	}
	if current_token(p).kind == .LParen {
		clause.paren_length = parse_required_paren_length_clause(p)
		if clause.paren_length == nil {
			return ast.Statics_Clause{}, false
		}
	}
	for !decl_clause_end(p, name_index) {
		if parse_group_or_include_addition(p, &clause.occurs, &clause.as_name, &clause.renaming_suffix) {
			continue
		}
		if at_length_keyword(p) {
			length_clause, length_ok := parse_required_length_clause(p)
			if !length_ok {
				return ast.Statics_Clause{}, false
			}
			append(&clause.length_clauses, length_clause)
			continue
		}
		if at_keyword(p, "TYPE") || at_keyword(p, "LIKE") {
			clause.type_clause = parse_required_type_clause(p)
			if clause.type_clause == nil {
				return ast.Statics_Clause{}, false
			}
			continue
		}
		if at_keyword(p, "VALUE") {
			clause.value_clause = parse_required_value_clause(p)
			if clause.value_clause == nil {
				return ast.Statics_Clause{}, false
			}
			continue
		}
		bump_token(p)
	}
	return clause, true
}

parse_tables_clause :: proc(p: ^Parser) -> (ast.Tables_Clause, bool) {
	name, name_index, ok := parse_decl_name(p)
	if !ok {
		return ast.Tables_Clause{}, false
	}
	clause := ast.Tables_Clause {
		name = tokenizer.token_lexeme(name, p.source),
	}
	for !decl_clause_end(p, name_index) {
		bump_token(p)
	}
	return clause, true
}

parse_ranges_clause :: proc(p: ^Parser) -> (ast.Ranges_Clause, bool) {
	name, name_index, ok := parse_decl_name(p)
	if !ok {
		return ast.Ranges_Clause{}, false
	}
	clause := ast.Ranges_Clause {
		name = tokenizer.token_lexeme(name, p.source),
	}
	for !decl_clause_end(p, name_index) {
		if at_keyword(p, "FOR") {
			clause.for_clause = parse_required_for_clause(p)
			if clause.for_clause == nil {
				return ast.Ranges_Clause{}, false
			}
			continue
		}
		bump_token(p)
	}
	return clause, true
}

parse_parameters_clause :: proc(p: ^Parser) -> (ast.Parameters_Clause, bool) {
	name, name_index, ok := parse_decl_name(p)
	if !ok {
		return ast.Parameters_Clause{}, false
	}
	clause := ast.Parameters_Clause {
		name = tokenizer.token_lexeme(name, p.source),
	}
	clause.length_clauses = make([dynamic]ast.Length_Clause, 0, 2, p.allocator)
	if current_token(p).kind == .LParen {
		clause.paren_length = parse_required_paren_length_clause(p)
		if clause.paren_length == nil {
			return ast.Parameters_Clause{}, false
		}
	}
	for !decl_clause_end(p, name_index) {
		if at_keyword(p, "TYPE") || at_keyword(p, "LIKE") {
			clause.type_clause = parse_required_type_clause(p)
			if clause.type_clause == nil {
				return ast.Parameters_Clause{}, false
			}
			continue
		}
		if at_keyword(p, "DEFAULT") {
			clause.default_clause = parse_required_default_clause(p)
			if clause.default_clause == nil {
				return ast.Parameters_Clause{}, false
			}
			continue
		}
		matched, add_ok := parse_parameter_addition(p, &clause)
		if matched {
			if !add_ok {
				return ast.Parameters_Clause{}, false
			}
			continue
		}
		bump_token(p)
	}
	return clause, true
}

parse_select_options_clause :: proc(p: ^Parser) -> (ast.Select_Options_Clause, bool) {
	name, name_index, ok := parse_decl_name(p)
	if !ok {
		return ast.Select_Options_Clause{}, false
	}
	clause := ast.Select_Options_Clause {
		name = tokenizer.token_lexeme(name, p.source),
	}
	for !decl_clause_end(p, name_index) {
		if at_keyword(p, "FOR") {
			clause.for_clause = parse_required_for_clause(p)
			if clause.for_clause == nil {
				return ast.Select_Options_Clause{}, false
			}
			continue
		}
		if at_keyword(p, "DEFAULT") {
			clause.default_clause = parse_required_default_clause(p)
			if clause.default_clause == nil {
				return ast.Select_Options_Clause{}, false
			}
			continue
		}
		matched, add_ok := parse_select_option_addition(p, &clause)
		if matched {
			if !add_ok {
				return ast.Select_Options_Clause{}, false
			}
			continue
		}
		bump_token(p)
	}
	return clause, true
}

parse_controls_clause :: proc(p: ^Parser) -> (ast.Controls_Clause, bool) {
	name, name_index, ok := parse_decl_name(p)
	if !ok {
		return ast.Controls_Clause{}, false
	}
	clause := ast.Controls_Clause {
		name = tokenizer.token_lexeme(name, p.source),
	}
	for !decl_clause_end(p, name_index) {
		if at_keyword(p, "TYPE") {
			clause.type_clause = parse_required_type_clause(p)
			if clause.type_clause == nil {
				return ast.Controls_Clause{}, false
			}
			continue
		}
		if at_keyword(p, "USING") {
			clause.using_screen = parse_required_using_screen_clause(p)
			if clause.using_screen == nil {
				return ast.Controls_Clause{}, false
			}
			continue
		}
		bump_token(p)
	}
	return clause, true
}

parse_class_data_clause :: proc(p: ^Parser) -> (ast.Class_Data_Clause, bool) {
	kind, is_common_part_delimiter, name, include_ref, name_index, ok := parse_decl_clause_head(p)
	if !ok {
		return ast.Class_Data_Clause{}, false
	}
	clause := ast.Class_Data_Clause {
		kind           = kind,
		name           = name,
		include_ref    = include_ref,
		length_clauses = make([dynamic]ast.Length_Clause, 0, 2, p.allocator),
	}
	if is_common_part_delimiter {
		clause.flags += {.Common_Part_Delimiter}
	}
	if current_token(p).kind == .LParen {
		clause.paren_length = parse_required_paren_length_clause(p)
		if clause.paren_length == nil {
			return ast.Class_Data_Clause{}, false
		}
	}
	for !decl_clause_end(p, name_index) {
		if parse_group_or_include_addition(p, &clause.occurs, &clause.as_name, &clause.renaming_suffix) {
			continue
		}
		if at_length_keyword(p) {
			length_clause, length_ok := parse_required_length_clause(p)
			if !length_ok {
				return ast.Class_Data_Clause{}, false
			}
			append(&clause.length_clauses, length_clause)
			continue
		}
		if at_keyword(p, "TYPE") || at_keyword(p, "LIKE") {
			clause.type_clause = parse_required_type_clause(p)
			if clause.type_clause == nil {
				return ast.Class_Data_Clause{}, false
			}
			continue
		}
		if at_keyword(p, "VALUE") {
			clause.value_clause = parse_required_value_clause(p)
			if clause.value_clause == nil {
				return ast.Class_Data_Clause{}, false
			}
			continue
		}
		if at_keyword_phrase(p, "READ-ONLY") {
			expect_keyword_phrase(p, "READ-ONLY")
			clause.read_only = true
			continue
		}
		bump_token(p)
	}
	return clause, true
}

parse_decl_name :: proc(p: ^Parser) -> (Token, int, bool) {
	index := p.index
	tok := current_token(p)
	if tok.kind != .Ident && tok.kind != .Number && tok.kind != .Star {
		error_current(p, "syntax error: expected declaration name")
		return tok, index, false
	}
	bump_token(p)
	if tok.kind == .Number && current_token(p).kind == .Ident && current_token(p).range.start == tok.range.end {
		tail := bump_token(p)
		tok.range.end = tail.range.end
	}
	return tok, index, true
}

parse_required_type_clause :: proc(p: ^Parser) -> ^ast.Data_Type_Clause {
	keyword := bump_token(p)
	clause, _ := mem.new(ast.Data_Type_Clause, p.allocator)
	is_like := token_is_keyword(p, keyword, "LIKE")
	clause.form = .Like if is_like else .Type

	if allow_keyword(p, "LINE") {
		if !allow_keyword(p, "OF") {
			error_current(p, "syntax error: expected keyword")
			return nil
		}
		clause.form = .Like_Line_Of if is_like else .Type_Line_Of
	} else if !is_like && allow_keyword(p, "REF") {
		if !allow_keyword(p, "TO") {
			error_current(p, "syntax error: expected keyword")
			return nil
		}
		clause.form = .Ref_To
	} else if !is_like && allow_keyword(p, "RANGE") {
		if !allow_keyword(p, "OF") {
			error_current(p, "syntax error: expected keyword")
			return nil
		}
		clause.form = .Range_Of
	} else if allow_keyword(p, "STANDARD") {
		if !allow_keyword(p, "TABLE") {
			error_current(p, "syntax error: expected keyword")
			return nil
		}
		allow_keyword(p, "OF")
		clause.form = .Like_Standard_Table if is_like else .Standard_Table
	} else if allow_keyword(p, "SORTED") {
		if !allow_keyword(p, "TABLE") {
			error_current(p, "syntax error: expected keyword")
			return nil
		}
		allow_keyword(p, "OF")
		clause.form = .Like_Sorted_Table if is_like else .Sorted_Table
	} else if allow_keyword(p, "HASHED") {
		if !allow_keyword(p, "TABLE") {
			error_current(p, "syntax error: expected keyword")
			return nil
		}
		allow_keyword(p, "OF")
		clause.form = .Like_Hashed_Table if is_like else .Hashed_Table
	} else if allow_keyword(p, "TABLE") {
		allow_keyword(p, "OF")
		clause.form = .Like_Table if is_like else .Table
	}

	if decl_clause_boundary(p) || type_ref_stop_keyword(p) {
		if clause.form == .Table ||
		   clause.form == .Like_Table ||
		   clause.form == .Standard_Table ||
		   clause.form == .Sorted_Table ||
		   clause.form == .Hashed_Table ||
		   clause.form == .Like_Standard_Table ||
		   clause.form == .Like_Sorted_Table ||
		   clause.form == .Like_Hashed_Table {
			return clause
		}
		error_current(p, "syntax error: expected type name")
		return nil
	}

	type_ref := parse_type_ref_expr(p)
	if type_ref == nil {
		if clause.form == .Table ||
		   clause.form == .Like_Table ||
		   clause.form == .Standard_Table ||
		   clause.form == .Sorted_Table ||
		   clause.form == .Hashed_Table ||
		   clause.form == .Like_Standard_Table ||
		   clause.form == .Like_Sorted_Table ||
		   clause.form == .Like_Hashed_Table {
			return clause
		}
		return nil
	}
	clause.type_ref = type_ref
	return clause
}

parse_type_ref_expr :: proc(p: ^Parser) -> ^ast.Expr {
	start := p.index
	if decl_clause_boundary(p) || type_ref_stop_keyword(p) {
		error_current(p, "syntax error: expected type name")
		return nil
	}
	paren, bracket, brace := 0, 0, 0
	name_end := -1
	key_clause: ^ast.Type_Ref_Key_Clause
	key_clauses := make([dynamic]^ast.Type_Ref_Key_Clause, 0, 1, p.allocator)
	for {
		tok := current_token(p)
		if tok.kind == .Eof {
			break
		}
		top := paren == 0 && bracket == 0 && brace == 0
		if top {
			if decl_clause_boundary(p) {
				break
			}
			if at_keyword(p, "WITH") {
				if name_end < 0 {
					name_end = previous_token(p).range.end
				}
				next_key := parse_type_ref_key_clause(p)
				if key_clause == nil {
					key_clause = next_key
				}
				append(&key_clauses, next_key)
				continue
			}
			if p.index > start && type_ref_stop_keyword(p) && !type_ref_selector_field(p) {
				break
			}
		}
		#partial switch tok.kind {
		case .LParen:
			paren += 1
		case .RParen:
			if paren == 0 {
				break
			}
			paren -= 1
		case .LBracket:
			bracket += 1
		case .RBracket:
			if bracket == 0 {
				break
			}
			bracket -= 1
		case .LBrace:
			brace += 1
		case .RBrace:
			if brace == 0 {
				break
			}
			brace -= 1
		}
		bump_token(p)
	}
	if p.index <= start {
		error_current(p, "syntax error: expected type name")
		return nil
	}
	first := p.tokens[start]
	last := p.tokens[p.index - 1]
	expr := ast.new(ast.Type_Ref_Expr, tokenizer.text_range(first.range.start, last.range.end), p.allocator)
	expr.text = strings.clone(p.source[first.range.start:last.range.end], p.allocator)
	if name_end < 0 {
		name_end = last.range.end
	}
	expr.name = strings.clone(p.source[first.range.start:name_end], p.allocator)
	expr.key = key_clause
	expr.keys = key_clauses
	return expr
}

parse_type_ref_key_clause :: proc(p: ^Parser) -> ^ast.Type_Ref_Key_Clause {
	expect_keyword(p, "WITH")
	clause, _ := mem.new(ast.Type_Ref_Key_Clause, p.allocator)
	clause.kind = .Generic
	clause.components = make([dynamic]string, 0, 2, p.allocator)
	if allow_keyword(p, "DEFAULT") {
		allow_keyword(p, "KEY")
		clause.kind = .Default
		return clause
	}
	if allow_keyword(p, "EMPTY") {
		allow_keyword(p, "KEY")
		clause.kind = .Empty
		return clause
	}
	if allow_keyword(p, "UNIQUE") {
		clause.kind = .Unique
	} else if allow_hyphen2(p, "NON", "UNIQUE") {
		clause.kind = .Non_Unique
	}
	if clause.kind != .Generic && allow_keyword(p, "DEFAULT") {
		allow_keyword(p, "KEY")
		clause.default_key = true
		return clause
	}
	clause.sorted = allow_keyword(p, "SORTED")
	clause.hashed = allow_keyword(p, "HASHED")
	allow_keyword(p, "KEY")
	in_components := false
	for !decl_clause_boundary(p) && !type_ref_stop_keyword(p) {
		if allow_token(p, .Comma) {
			continue
		}
		if allow_keyword(p, "COMPONENTS") {
			in_components = true
			continue
		}
		tok := current_token(p)
		if tok.kind != .Ident && tok.kind != .Number && tok.kind != .Star {
			break
		}
		name := tokenizer.token_lexeme(tok, p.source)
		if !in_components && (clause.sorted || clause.hashed) && clause.name == "" {
			clause.name = name
		} else {
			append(&clause.components, name)
		}
		bump_token(p)
	}
	return clause
}

type_ref_selector_field :: proc(p: ^Parser) -> bool {
	return(
		current_token(p).kind == .Ident &&
		previous_token(p).kind == .Minus &&
		tokens_touch(previous_token(p), current_token(p)) \
	)
}

consume_type_ref_key_addition :: proc(p: ^Parser) {
	expect_keyword(p, "WITH")
	for !decl_clause_boundary(p) {
		if at_keyword_phrase(p, "READ-ONLY") || at_keyword(p, "VALUE") {
			return
		}
		tok := bump_token(p)
		if token_is_keyword(p, tok, "KEY") && (
			token_is_keyword(p, previous_token(p), "DEFAULT") ||
			token_is_keyword(p, previous_token(p), "EMPTY")
		) {
			return
		}
	}
}

parse_group_or_include_addition :: proc(
	p: ^Parser,
	occurs: ^^ast.Expr,
	as_name: ^string,
	renaming_suffix: ^string,
) -> bool {
	if allow_keyword(p, "OCCURS") {
		occurs^ = parse_expr(p)
		return true
	}
	if allow_keyword(p, "AS") {
		name, ok := parse_required_addition_name(p)
		if ok {
			as_name^ = name
		}
		return true
	}
	if allow_keyword(p, "RENAMING") {
		allow_keyword(p, "WITH")
		allow_keyword(p, "SUFFIX")
		suffix, ok := parse_required_addition_name(p)
		if ok {
			renaming_suffix^ = suffix
		}
		return true
	}
	return false
}

parse_required_paren_length_clause :: proc(p: ^Parser) -> ^ast.Paren_Length_Clause {
	expect_token(p, .LParen)
	value := parse_expr(p)
	if value == nil {
		return nil
	}
	close := expect_token(p, .RParen)
	if close.kind != .RParen {
		return nil
	}
	clause, _ := mem.new(ast.Paren_Length_Clause, p.allocator)
	clause.expr = value
	return clause
}

parse_required_length_clause :: proc(p: ^Parser) -> (ast.Length_Clause, bool) {
	keyword := bump_token(p)
	value := parse_expr(p)
	if value == nil {
		return ast.Length_Clause{}, false
	}
	kind := ast.Length_Clause_Kind.Length
	if token_is_keyword(p, keyword, "DECIMALS") {
		kind = .Decimals
	}
	return ast.Length_Clause{kind = kind, expr = value}, true
}

parse_required_value_clause :: proc(p: ^Parser) -> ^ast.Value_Clause {
	expect_keyword(p, "VALUE")
	clause, _ := mem.new(ast.Value_Clause, p.allocator)
	if allow_keyword(p, "IS") {
		if !allow_keyword(p, "INITIAL") {
			error_current(p, "syntax error: expected keyword")
			return nil
		}
		clause.is_initial = true
		return clause
	}
	value := parse_expr(p)
	if value == nil {
		return nil
	}
	clause.expr = value
	return clause
}

parse_required_default_clause :: proc(p: ^Parser) -> ^ast.Default_Clause {
	expect_keyword(p, "DEFAULT")
	value := parse_expr(p)
	if value == nil {
		return nil
	}
	clause, _ := mem.new(ast.Default_Clause, p.allocator)
	clause.expr = value
	return clause
}

parse_required_for_clause :: proc(p: ^Parser) -> ^ast.For_Clause {
	expect_keyword(p, "FOR")
	value := parse_expr(p)
	if value == nil {
		return nil
	}
	clause, _ := mem.new(ast.For_Clause, p.allocator)
	clause.expr = value
	return clause
}

parse_required_using_screen_clause :: proc(p: ^Parser) -> ^ast.Using_Screen_Clause {
	expect_keyword(p, "USING")
	if !allow_keyword(p, "SCREEN") {
		error_current(p, "syntax error: expected keyword")
		return nil
	}
	screen := parse_expr(p)
	if screen == nil {
		return nil
	}
	clause, _ := mem.new(ast.Using_Screen_Clause, p.allocator)
	clause.screen = screen
	return clause
}

parse_parameter_addition :: proc(p: ^Parser, clause: ^ast.Parameters_Clause) -> (bool, bool) {
	if at_length_keyword(p) {
		length_clause, ok := parse_required_length_clause(p)
		if ok {
			append(&clause.length_clauses, length_clause)
		}
		return true, ok
	}
	if at_keyword_phrase(p, "AS CHECKBOX") {
		expect_keyword_phrase(p, "AS CHECKBOX")
		clause.flags += {.As_Checkbox}
		return true, true
	}
	if at_keyword_phrase(p, "LOWER CASE") {
		expect_keyword_phrase(p, "LOWER CASE")
		clause.flags += {.Lower_Case}
		return true, true
	}
	if allow_keyword(p, "OBLIGATORY") {
		clause.flags += {.Obligatory}
		return true, true
	}
	if at_keyword_phrase(p, "NO-DISPLAY") {
		expect_keyword_phrase(p, "NO-DISPLAY")
		clause.flags += {.No_Display}
		return true, true
	}
	if at_keyword_phrase(p, "VALUE CHECK") {
		expect_keyword_phrase(p, "VALUE CHECK")
		clause.flags += {.Value_Check}
		return true, true
	}
	if at_keyword_phrase(p, "HELP-REQUEST") {
		expect_keyword_phrase(p, "HELP-REQUEST")
		clause.flags += {.Help_Request}
		return true, true
	}
	if at_keyword_phrase(p, "VALUE-REQUEST") {
		expect_keyword_phrase(p, "VALUE-REQUEST")
		clause.flags += {.Value_Request}
		return true, true
	}
	if at_keyword_phrase(p, "RADIOBUTTON GROUP") {
		clause.radiobutton_group = parse_required_radiobutton_group_clause(p)
		return true, clause.radiobutton_group != nil
	}
	if at_keyword_phrase(p, "USER-COMMAND") {
		clause.user_command = parse_required_user_command_clause(p)
		return true, clause.user_command != nil
	}
	if at_keyword_phrase(p, "MODIF ID") {
		clause.modif_id = parse_required_modif_id_clause(p)
		return true, clause.modif_id != nil
	}
	if at_keyword_phrase(p, "MEMORY ID") {
		clause.memory_id = parse_required_memory_id_clause(p)
		return true, clause.memory_id != nil
	}
	if at_keyword_phrase(p, "MATCHCODE OBJECT") {
		clause.matchcode_object = parse_required_matchcode_object_clause(p)
		return true, clause.matchcode_object != nil
	}
	if at_keyword_phrase(p, "VISIBLE LENGTH") {
		clause.visible_length = parse_required_visible_length_clause(p)
		return true, clause.visible_length != nil
	}
	return false, true
}

parse_select_option_addition :: proc(
	p: ^Parser,
	clause: ^ast.Select_Options_Clause,
) -> (
	bool,
	bool,
) {
	if at_keyword(p, "TO") {
		clause.to_clause = parse_required_to_clause(p)
		return true, clause.to_clause != nil
	}
	if at_keyword(p, "OPTION") {
		clause.option_clause = parse_required_option_clause(p)
		return true, clause.option_clause != nil
	}
	if at_keyword(p, "SIGN") {
		clause.sign_clause = parse_required_sign_clause(p)
		return true, clause.sign_clause != nil
	}
	if at_keyword_phrase(p, "LOWER CASE") {
		expect_keyword_phrase(p, "LOWER CASE")
		clause.flags += {.Lower_Case}
		return true, true
	}
	if allow_keyword(p, "OBLIGATORY") {
		clause.flags += {.Obligatory}
		return true, true
	}
	if at_keyword_phrase(p, "NO-DISPLAY") {
		expect_keyword_phrase(p, "NO-DISPLAY")
		clause.flags += {.No_Display}
		return true, true
	}
	if at_keyword_phrase(p, "NO-EXTENSION") {
		expect_keyword_phrase(p, "NO-EXTENSION")
		clause.flags += {.No_Extension}
		return true, true
	}
	if at_keyword_phrase(p, "NO INTERVALS") {
		expect_keyword_phrase(p, "NO INTERVALS")
		clause.flags += {.No_Intervals}
		return true, true
	}
	if at_keyword_phrase(p, "NO DATABASE SELECTION") {
		expect_keyword_phrase(p, "NO DATABASE SELECTION")
		clause.flags += {.No_Database_Selection}
		return true, true
	}
	if at_keyword_phrase(p, "MODIF ID") {
		clause.modif_id = parse_required_modif_id_clause(p)
		return true, clause.modif_id != nil
	}
	if at_keyword_phrase(p, "MEMORY ID") {
		clause.memory_id = parse_required_memory_id_clause(p)
		return true, clause.memory_id != nil
	}
	if at_keyword_phrase(p, "MATCHCODE OBJECT") {
		clause.matchcode_object = parse_required_matchcode_object_clause(p)
		return true, clause.matchcode_object != nil
	}
	if at_keyword_phrase(p, "VISIBLE LENGTH") {
		clause.visible_length = parse_required_visible_length_clause(p)
		return true, clause.visible_length != nil
	}
	if at_keyword_phrase(p, "HELP-REQUEST") {
		clause.help_request = parse_required_selection_request_clause(p, .Help_Request)
		return true, clause.help_request != nil
	}
	if at_keyword_phrase(p, "VALUE-REQUEST") {
		clause.value_request = parse_required_selection_request_clause(p, .Value_Request)
		return true, clause.value_request != nil
	}
	return false, true
}

parse_required_to_clause :: proc(p: ^Parser) -> ^ast.To_Clause {
	expect_keyword(p, "TO")
	value := parse_expr(p)
	if value == nil {
		return nil
	}
	clause, _ := mem.new(ast.To_Clause, p.allocator)
	clause.expr = value
	return clause
}

parse_required_option_clause :: proc(p: ^Parser) -> ^ast.Option_Clause {
	expect_keyword(p, "OPTION")
	option, ok := parse_required_addition_name(p)
	if !ok {
		return nil
	}
	clause, _ := mem.new(ast.Option_Clause, p.allocator)
	clause.option = option
	return clause
}

parse_required_sign_clause :: proc(p: ^Parser) -> ^ast.Sign_Clause {
	expect_keyword(p, "SIGN")
	sign, ok := parse_required_addition_name(p)
	if !ok {
		return nil
	}
	clause, _ := mem.new(ast.Sign_Clause, p.allocator)
	clause.sign = sign
	return clause
}

parse_required_radiobutton_group_clause :: proc(p: ^Parser) -> ^ast.Radiobutton_Group_Clause {
	expect_keyword_phrase(p, "RADIOBUTTON GROUP")
	group, ok := parse_required_addition_name(p)
	if !ok {
		return nil
	}
	clause, _ := mem.new(ast.Radiobutton_Group_Clause, p.allocator)
	clause.group = group
	return clause
}

parse_required_user_command_clause :: proc(p: ^Parser) -> ^ast.User_Command_Clause {
	expect_keyword_phrase(p, "USER-COMMAND")
	command, ok := parse_required_addition_name(p)
	if !ok {
		return nil
	}
	clause, _ := mem.new(ast.User_Command_Clause, p.allocator)
	clause.command = command
	return clause
}

parse_required_modif_id_clause :: proc(p: ^Parser) -> ^ast.Modif_Id_Clause {
	expect_keyword_phrase(p, "MODIF ID")
	id, ok := parse_required_addition_name(p)
	if !ok {
		return nil
	}
	clause, _ := mem.new(ast.Modif_Id_Clause, p.allocator)
	clause.id = id
	return clause
}

parse_required_memory_id_clause :: proc(p: ^Parser) -> ^ast.Memory_Id_Clause {
	expect_keyword_phrase(p, "MEMORY ID")
	id := parse_expr(p)
	if id == nil {
		return nil
	}
	clause, _ := mem.new(ast.Memory_Id_Clause, p.allocator)
	clause.id = id
	return clause
}

parse_required_matchcode_object_clause :: proc(p: ^Parser) -> ^ast.Matchcode_Object_Clause {
	expect_keyword_phrase(p, "MATCHCODE OBJECT")
	object := parse_expr(p)
	if object == nil {
		return nil
	}
	clause, _ := mem.new(ast.Matchcode_Object_Clause, p.allocator)
	clause.object = object
	return clause
}

parse_required_visible_length_clause :: proc(p: ^Parser) -> ^ast.Visible_Length_Clause {
	expect_keyword_phrase(p, "VISIBLE LENGTH")
	length := parse_expr(p)
	if length == nil {
		return nil
	}
	clause, _ := mem.new(ast.Visible_Length_Clause, p.allocator)
	clause.length = length
	return clause
}

parse_required_selection_request_clause :: proc(
	p: ^Parser,
	kind: ast.Selection_Request_Kind,
) -> ^ast.Selection_Request_Clause {
	expect_keyword_phrase(p, "HELP-REQUEST" if kind == .Help_Request else "VALUE-REQUEST")
	if !allow_keyword(p, "FOR") {
		error_current(p, "syntax error: expected keyword")
		return nil
	}
	target, ok := parse_required_addition_name(p)
	if !ok {
		return nil
	}
	clause, _ := mem.new(ast.Selection_Request_Clause, p.allocator)
	clause.kind = kind
	clause.target = target
	return clause
}

parse_required_addition_name :: proc(p: ^Parser) -> (string, bool) {
	tok := current_token(p)
	if tok.kind == .Ident || tok.kind == .Number || tok.kind == .String {
		bump_token(p)
		return tokenizer.token_lexeme(tok, p.source), true
	}
	error_current(p, "syntax error: expected addition value")
	return "", false
}

at_length_keyword :: proc(p: ^Parser) -> bool {
	return at_keyword(p, "LENGTH") || at_keyword(p, "DECIMALS")
}

type_ref_stop_keyword :: proc(p: ^Parser) -> bool {
	return(
		at_length_keyword(p) ||
		at_keyword(p, "VALUE") ||
		at_keyword(p, "DEFAULT") ||
		at_keyword(p, "FOR") ||
		at_keyword(p, "AS") ||
		at_keyword(p, "LOWER") ||
		at_keyword(p, "MATCHCODE") ||
		at_keyword(p, "MEMORY") ||
		at_keyword(p, "MODIF") ||
		at_keyword(p, "NO") ||
		at_keyword(p, "OBLIGATORY") ||
		at_keyword(p, "RADIOBUTTON") ||
		at_keyword(p, "USER") ||
		at_keyword(p, "USING") ||
		at_keyword(p, "VISIBLE") ||
		at_keyword_phrase(p, "READ-ONLY") ||
		at_keyword(p, "WITH") \
	)
}

decl_clause_boundary :: proc(p: ^Parser) -> bool {
	tok := current_token(p)
	return tok.kind == .Comma || tok.kind == .Period || tok.kind == .Eof
}

decl_clause_end :: proc(p: ^Parser, clause_start: int) -> bool {
	tok := current_token(p)
	return(
		decl_clause_boundary(p) ||
		(p.index > clause_start &&
				.Has_Newline_Before in tok.flags &&
				statement_lead_starts(p, p.index)) \
	)
}
