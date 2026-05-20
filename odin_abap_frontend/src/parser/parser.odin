package abap_frontend_parser

import "../ast"
import "../tokenizer"

import "core:mem"

Token :: tokenizer.Token
Range :: tokenizer.Range
Trivia_Piece :: tokenizer.Trivia_Piece
Lex_Error :: tokenizer.Lex_Error

Parse_Error :: struct {
	message: string,
	range:   tokenizer.Range,
}

Parsed_File :: struct {
	root:   ^ast.File,
	errors: []Parse_Error,
	path:   string,
}

Parse_Result :: Parsed_File

Parser :: struct {
	source:         string,
	path:           string,
	tokens:         []tokenizer.Token,
	trivia:         []tokenizer.Trivia_Piece,
	lex_errors:     []tokenizer.Lex_Error,
	index:          int,
	previous_index: int,
	errors:         [dynamic]Parse_Error,
	allocator:      mem.Allocator,
	root:           ^ast.File,
}

Stmt_Mark :: struct {
	index: int,
}

parse :: proc(source: string, path: string, allocator: mem.Allocator) -> Parsed_File {
	lexed := tokenizer.tokenize(source, context.temp_allocator)
	parser := Parser {
		source         = source,
		path           = path,
		tokens         = lexed.tokens,
		trivia         = lexed.trivia,
		lex_errors     = lexed.errors,
		previous_index = -1,
		errors         = make([dynamic]Parse_Error, 0, len(lexed.errors) + 4, allocator),
		allocator      = allocator,
	}

	for e in lexed.errors {
		append(&parser.errors, Parse_Error{e.message, e.range})
	}

	parser.root = ast.new(ast.File, tokenizer.text_range(0, len(source)), allocator)
	parser.root.allocator = allocator
	parser.root.stmts = make([dynamic]^ast.Stmt, 0, 8, allocator)
	parse_top_level(&parser)

	return Parsed_File{parser.root, parser.errors[:], path}
}

init_parser :: proc(source: string, path: string, allocator: mem.Allocator) -> Parser {
	lexed := tokenizer.tokenize(source, context.temp_allocator)
	return Parser {
		source = source,
		path = path,
		tokens = lexed.tokens,
		trivia = lexed.trivia,
		lex_errors = lexed.errors,
		previous_index = -1,
		errors = make([dynamic]Parse_Error, 0, len(lexed.errors) + 4, allocator),
		allocator = allocator,
	}
}

parse_top_level :: proc(p: ^Parser) {
	for {
		if at_eof(p) {
			return
		}
		start := p.index
		stmt := parse_stmt(p, nil)
		if stmt != nil {
			append(&p.root.stmts, stmt)
		}
		ensure_forward_progress(p, start)
	}
}

parse_stmt_list_until :: proc(p: ^Parser, stop_keywords: []string) -> [dynamic]^ast.Stmt {
	stmts := make([dynamic]^ast.Stmt, 0, 4, p.allocator)
	for {
		if at_eof(p) || at_any_keyword(p, stop_keywords) {
			return stmts
		}
		start := p.index
		stmt := parse_stmt(p, stop_keywords)
		if stmt != nil {
			append(&stmts, stmt)
		}
		ensure_forward_progress(p, start)
	}
}

parse_stmt :: proc(p: ^Parser, stop_keywords: []string) -> ^ast.Stmt {
	mark := mark_statement_start(p)
	stmt := parse_stmt_result(p)
	if stmt != nil {
		return stmt
	}

	if !consumed_significant_since(p, mark) {
		if !at_eof(p) && !at_any_keyword(p, stop_keywords) {
			bump_token(p)
		}
	}
	recover_to_statement_boundary(p, stop_keywords, true)
	return build_invalid_statement(p, mark)
}

parse_stmt_result :: proc(p: ^Parser) -> ^ast.Stmt {
	if at_eof(p) {
		error_current(p, "syntax error: unexpected token")
		return nil
	}
	if current_token(p).kind == .StringTemplate {
		return parse_expr_stmt(p)
	}
	if decl_stmt_starts(p) {
		return parse_decl_stmt(p)
	}
	if control_stmt_starts(p) {
		return parse_control_stmt(p)
	}
	if structural_stmt_starts(p) {
		return parse_structural_stmt(p)
	}
	if data_access_stmt_starts(p) {
		return parse_data_access_stmt(p)
	}
	if assignment_starts(p, p.index) {
		return parse_assign_stmt(p)
	}
	if simple_stmt_starts(p) {
		return parse_simple_stmt(p)
	}
	if direct_call_stmt_starts(p) {
		return parse_direct_call_stmt(p)
	}
	error_current(p, "syntax error: unexpected token")
	return nil
}

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
		at_keyword_phrase(p, "CLASS-DATA") \
	)
}

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
		at_keyword(p, "CLASS") ||
		at_keyword(p, "INTERFACE") ||
		at_keyword(p, "METHOD") ||
		at_keyword(p, "FORM") ||
		at_keyword(p, "FUNCTION") ||
		at_keyword(p, "MODULE") ||
		event_block_starts(p) ||
		at_keyword_phrase(p, "ENHANCEMENT-SECTION") ||
		at_keyword(p, "ENHANCEMENT") ||
		at_keyword_phrase(p, "TEST-SEAM") ||
		at_keyword_phrase(p, "TEST-INJECTION") \
	)
}

data_access_stmt_starts :: proc(p: ^Parser) -> bool {
	return(
		at_keyword(p, "SELECT") ||
		at_keyword(p, "WITH") ||
		at_keyword_phrase(p, "OPEN CURSOR") ||
		at_keyword(p, "FETCH") ||
		at_keyword_phrase(p, "CLOSE CURSOR") ||
		at_keyword(p, "REPORT") ||
		at_keyword(p, "PROGRAM") ||
		at_keyword(p, "INSERT") ||
		at_keyword(p, "UPDATE") ||
		at_keyword(p, "DELETE") ||
		at_keyword_phrase(p, "READ TABLE") ||
		dataset_stmt_starts(p) ||
		report_textpool_stmt_starts(p) \
	)
}

simple_stmt_starts :: proc(p: ^Parser) -> bool {
	return(
		at_keyword(p, "CLEAR") ||
		at_keyword(p, "REFRESH") ||
		at_keyword(p, "FREE") ||
		at_keyword(p, "UNASSIGN") ||
		at_keyword(p, "MOVE") ||
		at_keyword(p, "ADD") ||
		at_keyword(p, "SUBTRACT") ||
		at_keyword(p, "MULTIPLY") ||
		at_keyword(p, "DIVIDE") ||
		at_keyword(p, "COMPUTE") ||
		at_keyword(p, "CONCATENATE") ||
		at_keyword(p, "SPLIT") ||
		at_keyword(p, "CONDENSE") ||
		at_keyword(p, "REPLACE") ||
		at_keyword(p, "TRANSLATE") ||
		at_keyword(p, "SHIFT") ||
		at_keyword(p, "FIND") ||
		at_keyword(p, "SEARCH") ||
		at_keyword(p, "PERFORM") ||
		at_keyword(p, "CALL") ||
		at_keyword(p, "SUBMIT") ||
		at_keyword(p, "MESSAGE") ||
		at_keyword(p, "WRITE") \
	)
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

parse_decl_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	if at_keyword(p, "DATA") {
		return parse_data_decl_stmt(p)
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
	return parse_class_data_decl_stmt(p)
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
	name, type_clause, ok := parse_data_decl_clause(p)
	if !ok {
		return nil
	}

	if has_colon {
		stmt := ast.new(ast.Data_Chained_Decl, start.range, p.allocator)
		stmt.decls = make([dynamic]ast.Data_Chained_Branch, 0, 2, p.allocator)
		append(&stmt.decls, ast.Data_Chained_Branch{name, type_clause})
		for allow_token(p, .Comma) {
			if current_token(p).kind == .Period || current_token(p).kind == .Eof {
				error_current(p, "syntax error: expected declaration after ','")
				break
			}
			next_name, next_type_clause, next_ok := parse_data_decl_clause(p)
			if !next_ok {
				return nil
			}
			append(&stmt.decls, ast.Data_Chained_Branch{next_name, next_type_clause})
		}
		period := expect_token(p, .Period)
		stmt.range = tokenizer.text_range(start.range.start, statement_end(p, period))
		return stmt
	}

	period := expect_token(p, .Period)
	stmt := ast.new(
		ast.Data_Decl,
		tokenizer.text_range(start.range.start, statement_end(p, period)),
		p.allocator,
	)
	stmt.name = name
	stmt.type_clause = type_clause
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
	value := parse_expr(p)
	if value == nil {
		return nil
	}
	period := expect_token(p, .Period)
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
		if !ok {return nil}
		append(&stmt.types, clause)
		if !allow_token(p, .Comma) {break}
	}
	period := expect_token(p, .Period)
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
		if !ok {return nil}
		append(&stmt.constants, clause)
		if !allow_token(p, .Comma) {break}
	}
	period := expect_token(p, .Period)
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
		if !ok {return nil}
		append(&stmt.field_symbols, clause)
		if !allow_token(p, .Comma) {break}
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
		if !ok {return nil}
		append(&stmt.statics, clause)
		if !allow_token(p, .Comma) {break}
	}
	period := expect_token(p, .Period)
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
		if !ok {return nil}
		append(&stmt.tables, clause)
		if !allow_token(p, .Comma) {break}
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
		if !ok {return nil}
		append(&stmt.ranges, clause)
		if !allow_token(p, .Comma) {break}
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
		if !ok {return nil}
		append(&stmt.parameters, clause)
		if !allow_token(p, .Comma) {break}
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
		if !ok {return nil}
		append(&stmt.options, clause)
		if !allow_token(p, .Comma) {break}
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
		if !ok {return nil}
		append(&stmt.controls, clause)
		if !allow_token(p, .Comma) {break}
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
		if !ok {return nil}
		append(&stmt.decls, clause)
		if !allow_token(p, .Comma) {break}
	}
	period := expect_token(p, .Period)
	stmt.range = tokenizer.text_range(start.range.start, statement_end(p, period))
	return stmt
}

parse_data_decl_clause :: proc(p: ^Parser) -> (string, ^ast.Data_Type_Clause, bool) {
	name, name_index, ok := parse_decl_name(p)
	if !ok {return "", nil, false}
	type_clause: ^ast.Data_Type_Clause
	for !decl_clause_end(p, name_index) {
		if at_keyword(p, "TYPE") || at_keyword(p, "LIKE") {
			type_clause = parse_required_type_clause(p)
			if type_clause == nil {return "", nil, false}
			continue
		}
		bump_token(p)
	}
	return tokenizer.token_lexeme(name, p.source), type_clause, true
}

parse_types_clause :: proc(p: ^Parser) -> (ast.Types_Clause, bool) {
	name, name_index, ok := parse_decl_name(p)
	if !ok {return ast.Types_Clause{}, false}
	clause := ast.Types_Clause {
		name = tokenizer.token_lexeme(name, p.source),
	}
	clause.length_clauses = make([dynamic]ast.Length_Clause, 0, 2, p.allocator)
	if current_token(p).kind == .LParen {
		clause.paren_length = parse_required_paren_length_clause(p)
		if clause.paren_length == nil {return ast.Types_Clause{}, false}
	}
	for !decl_clause_end(p, name_index) {
		if at_length_keyword(p) {
			length_clause, length_ok := parse_required_length_clause(p)
			if !length_ok {return ast.Types_Clause{}, false}
			append(&clause.length_clauses, length_clause)
			continue
		}
		if at_keyword(p, "TYPE") || at_keyword(p, "LIKE") {
			clause.type_clause = parse_required_type_clause(p)
			if clause.type_clause == nil {return ast.Types_Clause{}, false}
			continue
		}
		bump_token(p)
	}
	return clause, true
}

parse_constants_clause :: proc(p: ^Parser) -> (ast.Constants_Clause, bool) {
	name, name_index, ok := parse_decl_name(p)
	if !ok {return ast.Constants_Clause{}, false}
	clause := ast.Constants_Clause {
		name = tokenizer.token_lexeme(name, p.source),
	}
	clause.length_clauses = make([dynamic]ast.Length_Clause, 0, 2, p.allocator)
	if current_token(p).kind == .LParen {
		clause.paren_length = parse_required_paren_length_clause(p)
		if clause.paren_length == nil {return ast.Constants_Clause{}, false}
	}
	for !decl_clause_end(p, name_index) {
		if at_length_keyword(p) {
			length_clause, length_ok := parse_required_length_clause(p)
			if !length_ok {return ast.Constants_Clause{}, false}
			append(&clause.length_clauses, length_clause)
			continue
		}
		if at_keyword(p, "TYPE") || at_keyword(p, "LIKE") {
			clause.type_clause = parse_required_type_clause(p)
			if clause.type_clause == nil {return ast.Constants_Clause{}, false}
			continue
		}
		if at_keyword(p, "VALUE") {
			clause.value_clause = parse_required_value_clause(p)
			if clause.value_clause == nil {return ast.Constants_Clause{}, false}
			continue
		}
		bump_token(p)
	}
	return clause, true
}

parse_field_symbols_clause :: proc(p: ^Parser) -> (ast.Field_Symbols_Clause, bool) {
	name, name_index, ok := parse_decl_name(p)
	if !ok {return ast.Field_Symbols_Clause{}, false}
	clause := ast.Field_Symbols_Clause {
		name = tokenizer.token_lexeme(name, p.source),
	}
	for !decl_clause_end(p, name_index) {
		if at_keyword(p, "TYPE") || at_keyword(p, "LIKE") {
			clause.type_clause = parse_required_type_clause(p)
			if clause.type_clause == nil {return ast.Field_Symbols_Clause{}, false}
			continue
		}
		bump_token(p)
	}
	return clause, true
}

parse_statics_clause :: proc(p: ^Parser) -> (ast.Statics_Clause, bool) {
	name, name_index, ok := parse_decl_name(p)
	if !ok {return ast.Statics_Clause{}, false}
	clause := ast.Statics_Clause {
		name = tokenizer.token_lexeme(name, p.source),
	}
	clause.length_clauses = make([dynamic]ast.Length_Clause, 0, 2, p.allocator)
	if current_token(p).kind == .LParen {
		clause.paren_length = parse_required_paren_length_clause(p)
		if clause.paren_length == nil {return ast.Statics_Clause{}, false}
	}
	for !decl_clause_end(p, name_index) {
		if at_length_keyword(p) {
			length_clause, length_ok := parse_required_length_clause(p)
			if !length_ok {return ast.Statics_Clause{}, false}
			append(&clause.length_clauses, length_clause)
			continue
		}
		if at_keyword(p, "TYPE") || at_keyword(p, "LIKE") {
			clause.type_clause = parse_required_type_clause(p)
			if clause.type_clause == nil {return ast.Statics_Clause{}, false}
			continue
		}
		if at_keyword(p, "VALUE") {
			clause.value_clause = parse_required_value_clause(p)
			if clause.value_clause == nil {return ast.Statics_Clause{}, false}
			continue
		}
		bump_token(p)
	}
	return clause, true
}

parse_tables_clause :: proc(p: ^Parser) -> (ast.Tables_Clause, bool) {
	name, name_index, ok := parse_decl_name(p)
	if !ok {return ast.Tables_Clause{}, false}
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
	if !ok {return ast.Ranges_Clause{}, false}
	clause := ast.Ranges_Clause {
		name = tokenizer.token_lexeme(name, p.source),
	}
	for !decl_clause_end(p, name_index) {
		if at_keyword(p, "FOR") {
			clause.for_clause = parse_required_for_clause(p)
			if clause.for_clause == nil {return ast.Ranges_Clause{}, false}
			continue
		}
		bump_token(p)
	}
	return clause, true
}

parse_parameters_clause :: proc(p: ^Parser) -> (ast.Parameters_Clause, bool) {
	name, name_index, ok := parse_decl_name(p)
	if !ok {return ast.Parameters_Clause{}, false}
	clause := ast.Parameters_Clause {
		name = tokenizer.token_lexeme(name, p.source),
	}
	clause.length_clauses = make([dynamic]ast.Length_Clause, 0, 2, p.allocator)
	if current_token(p).kind == .LParen {
		clause.paren_length = parse_required_paren_length_clause(p)
		if clause.paren_length == nil {return ast.Parameters_Clause{}, false}
	}
	for !decl_clause_end(p, name_index) {
		if at_keyword(p, "TYPE") || at_keyword(p, "LIKE") {
			clause.type_clause = parse_required_type_clause(p)
			if clause.type_clause == nil {return ast.Parameters_Clause{}, false}
			continue
		}
		if at_keyword(p, "DEFAULT") {
			clause.default_clause = parse_required_default_clause(p)
			if clause.default_clause == nil {return ast.Parameters_Clause{}, false}
			continue
		}
		matched, add_ok := parse_parameter_addition(p, &clause)
		if matched {
			if !add_ok {return ast.Parameters_Clause{}, false}
			continue
		}
		bump_token(p)
	}
	return clause, true
}

parse_select_options_clause :: proc(p: ^Parser) -> (ast.Select_Options_Clause, bool) {
	name, name_index, ok := parse_decl_name(p)
	if !ok {return ast.Select_Options_Clause{}, false}
	clause := ast.Select_Options_Clause {
		name = tokenizer.token_lexeme(name, p.source),
	}
	for !decl_clause_end(p, name_index) {
		if at_keyword(p, "FOR") {
			clause.for_clause = parse_required_for_clause(p)
			if clause.for_clause == nil {return ast.Select_Options_Clause{}, false}
			continue
		}
		if at_keyword(p, "DEFAULT") {
			clause.default_clause = parse_required_default_clause(p)
			if clause.default_clause == nil {return ast.Select_Options_Clause{}, false}
			continue
		}
		matched, add_ok := parse_select_option_addition(p, &clause)
		if matched {
			if !add_ok {return ast.Select_Options_Clause{}, false}
			continue
		}
		bump_token(p)
	}
	return clause, true
}

parse_controls_clause :: proc(p: ^Parser) -> (ast.Controls_Clause, bool) {
	name, name_index, ok := parse_decl_name(p)
	if !ok {return ast.Controls_Clause{}, false}
	clause := ast.Controls_Clause {
		name = tokenizer.token_lexeme(name, p.source),
	}
	for !decl_clause_end(p, name_index) {
		if at_keyword(p, "TYPE") {
			clause.type_clause = parse_required_type_clause(p)
			if clause.type_clause == nil {return ast.Controls_Clause{}, false}
			continue
		}
		if at_keyword(p, "USING") {
			clause.using_screen = parse_required_using_screen_clause(p)
			if clause.using_screen == nil {return ast.Controls_Clause{}, false}
			continue
		}
		bump_token(p)
	}
	return clause, true
}

parse_class_data_clause :: proc(p: ^Parser) -> (ast.Class_Data_Clause, bool) {
	name, name_index, ok := parse_decl_name(p)
	if !ok {return ast.Class_Data_Clause{}, false}
	clause := ast.Class_Data_Clause {
		name = tokenizer.token_lexeme(name, p.source),
	}
	clause.length_clauses = make([dynamic]ast.Length_Clause, 0, 2, p.allocator)
	if current_token(p).kind == .LParen {
		clause.paren_length = parse_required_paren_length_clause(p)
		if clause.paren_length == nil {return ast.Class_Data_Clause{}, false}
	}
	for !decl_clause_end(p, name_index) {
		if at_length_keyword(p) {
			length_clause, length_ok := parse_required_length_clause(p)
			if !length_ok {return ast.Class_Data_Clause{}, false}
			append(&clause.length_clauses, length_clause)
			continue
		}
		if at_keyword(p, "TYPE") || at_keyword(p, "LIKE") {
			clause.type_clause = parse_required_type_clause(p)
			if clause.type_clause == nil {return ast.Class_Data_Clause{}, false}
			continue
		}
		if at_keyword(p, "VALUE") {
			clause.value_clause = parse_required_value_clause(p)
			if clause.value_clause == nil {return ast.Class_Data_Clause{}, false}
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
	return tok, index, true
}

parse_required_type_clause :: proc(p: ^Parser) -> ^ast.Data_Type_Clause {
	keyword := bump_token(p)
	clause, _ := mem.new(ast.Data_Type_Clause, p.allocator)
	clause.form = .Like if token_is_keyword(p, keyword, "LIKE") else .Type

	if token_is_keyword(p, keyword, "LIKE") && allow_keyword(p, "LINE") {
		if !allow_keyword(p, "OF") {
			error_current(p, "syntax error: expected keyword")
			return nil
		}
		clause.form = .Like_Line_Of
	} else if token_is_keyword(p, keyword, "TYPE") {
		if allow_keyword(p, "REF") {
			if !allow_keyword(p, "TO") {
				error_current(p, "syntax error: expected keyword")
				return nil
			}
			clause.form = .Ref_To
		} else if allow_keyword(p, "STANDARD") {
			if !allow_keyword(p, "TABLE") {
				error_current(p, "syntax error: expected keyword")
				return nil
			}
			allow_keyword(p, "OF")
			clause.form = .Standard_Table
		} else if allow_keyword(p, "SORTED") {
			if !allow_keyword(p, "TABLE") {
				error_current(p, "syntax error: expected keyword")
				return nil
			}
			allow_keyword(p, "OF")
			clause.form = .Sorted_Table
		} else if allow_keyword(p, "HASHED") {
			if !allow_keyword(p, "TABLE") {
				error_current(p, "syntax error: expected keyword")
				return nil
			}
			allow_keyword(p, "OF")
			clause.form = .Hashed_Table
		} else if allow_keyword(p, "TABLE") {
			allow_keyword(p, "OF")
			clause.form = .Standard_Table
		}
	}

	if decl_clause_boundary(p) || type_ref_stop_keyword(p) {
		if clause.form == .Standard_Table ||
		   clause.form == .Sorted_Table ||
		   clause.form == .Hashed_Table {
			return clause
		}
		error_current(p, "syntax error: expected type name")
		return nil
	}

	type_ref := parse_expr(p)
	if type_ref == nil {
		if clause.form == .Standard_Table ||
		   clause.form == .Sorted_Table ||
		   clause.form == .Hashed_Table {
			return clause
		}
		return nil
	}
	clause.type_ref = type_ref
	return clause
}

parse_required_paren_length_clause :: proc(p: ^Parser) -> ^ast.Paren_Length_Clause {
	expect_token(p, .LParen)
	value := parse_expr(p)
	if value == nil {return nil}
	close := expect_token(p, .RParen)
	if close.kind != .RParen {return nil}
	clause, _ := mem.new(ast.Paren_Length_Clause, p.allocator)
	clause.expr = value
	return clause
}

parse_required_length_clause :: proc(p: ^Parser) -> (ast.Length_Clause, bool) {
	keyword := bump_token(p)
	value := parse_expr(p)
	if value == nil {return ast.Length_Clause{}, false}
	kind := ast.Length_Clause_Kind.Length
	if token_is_keyword(p, keyword, "DECIMALS") {
		kind = .Decimals
	}
	return ast.Length_Clause{kind = kind, expr = value}, true
}

parse_required_value_clause :: proc(p: ^Parser) -> ^ast.Value_Clause {
	expect_keyword(p, "VALUE")
	value := parse_expr(p)
	if value == nil {return nil}
	clause, _ := mem.new(ast.Value_Clause, p.allocator)
	clause.expr = value
	return clause
}

parse_required_default_clause :: proc(p: ^Parser) -> ^ast.Default_Clause {
	expect_keyword(p, "DEFAULT")
	value := parse_expr(p)
	if value == nil {return nil}
	clause, _ := mem.new(ast.Default_Clause, p.allocator)
	clause.expr = value
	return clause
}

parse_required_for_clause :: proc(p: ^Parser) -> ^ast.For_Clause {
	expect_keyword(p, "FOR")
	value := parse_expr(p)
	if value == nil {return nil}
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
	if screen == nil {return nil}
	clause, _ := mem.new(ast.Using_Screen_Clause, p.allocator)
	clause.screen = screen
	return clause
}

parse_parameter_addition :: proc(p: ^Parser, clause: ^ast.Parameters_Clause) -> (bool, bool) {
	if at_length_keyword(p) {
		length_clause, ok := parse_required_length_clause(p)
		if ok {append(&clause.length_clauses, length_clause)}
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
	if value == nil {return nil}
	clause, _ := mem.new(ast.To_Clause, p.allocator)
	clause.expr = value
	return clause
}

parse_required_option_clause :: proc(p: ^Parser) -> ^ast.Option_Clause {
	expect_keyword(p, "OPTION")
	option, ok := parse_required_addition_name(p)
	if !ok {return nil}
	clause, _ := mem.new(ast.Option_Clause, p.allocator)
	clause.option = option
	return clause
}

parse_required_sign_clause :: proc(p: ^Parser) -> ^ast.Sign_Clause {
	expect_keyword(p, "SIGN")
	sign, ok := parse_required_addition_name(p)
	if !ok {return nil}
	clause, _ := mem.new(ast.Sign_Clause, p.allocator)
	clause.sign = sign
	return clause
}

parse_required_radiobutton_group_clause :: proc(p: ^Parser) -> ^ast.Radiobutton_Group_Clause {
	expect_keyword_phrase(p, "RADIOBUTTON GROUP")
	group, ok := parse_required_addition_name(p)
	if !ok {return nil}
	clause, _ := mem.new(ast.Radiobutton_Group_Clause, p.allocator)
	clause.group = group
	return clause
}

parse_required_user_command_clause :: proc(p: ^Parser) -> ^ast.User_Command_Clause {
	expect_keyword_phrase(p, "USER-COMMAND")
	command, ok := parse_required_addition_name(p)
	if !ok {return nil}
	clause, _ := mem.new(ast.User_Command_Clause, p.allocator)
	clause.command = command
	return clause
}

parse_required_modif_id_clause :: proc(p: ^Parser) -> ^ast.Modif_Id_Clause {
	expect_keyword_phrase(p, "MODIF ID")
	id, ok := parse_required_addition_name(p)
	if !ok {return nil}
	clause, _ := mem.new(ast.Modif_Id_Clause, p.allocator)
	clause.id = id
	return clause
}

parse_required_memory_id_clause :: proc(p: ^Parser) -> ^ast.Memory_Id_Clause {
	expect_keyword_phrase(p, "MEMORY ID")
	id := parse_expr(p)
	if id == nil {return nil}
	clause, _ := mem.new(ast.Memory_Id_Clause, p.allocator)
	clause.id = id
	return clause
}

parse_required_matchcode_object_clause :: proc(p: ^Parser) -> ^ast.Matchcode_Object_Clause {
	expect_keyword_phrase(p, "MATCHCODE OBJECT")
	object := parse_expr(p)
	if object == nil {return nil}
	clause, _ := mem.new(ast.Matchcode_Object_Clause, p.allocator)
	clause.object = object
	return clause
}

parse_required_visible_length_clause :: proc(p: ^Parser) -> ^ast.Visible_Length_Clause {
	expect_keyword_phrase(p, "VISIBLE LENGTH")
	length := parse_expr(p)
	if length == nil {return nil}
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
	if !ok {return nil}
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
		at_keyword(p, "VISIBLE") ||
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

parse_simple_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	if at_keyword(p, "CLEAR") {return parse_clear_stmt(p)}
	if at_keyword(p, "REFRESH") {return parse_refresh_stmt(p)}
	if at_keyword(p, "FREE") {return parse_free_stmt(p)}
	if at_keyword(p, "UNASSIGN") {return parse_unassign_stmt(p)}
	if at_keyword(p, "MOVE") {return parse_move_stmt(p)}
	if at_keyword(p, "ADD") {return parse_add_stmt(p)}
	if at_keyword(p, "SUBTRACT") {return parse_subtract_stmt(p)}
	if at_keyword(p, "MULTIPLY") {return parse_multiply_stmt(p)}
	if at_keyword(p, "DIVIDE") {return parse_divide_stmt(p)}
	if at_keyword(p, "COMPUTE") {return parse_compute_stmt(p)}
	if at_keyword(p, "CONCATENATE") {return parse_concatenate_stmt(p)}
	if at_keyword(p, "SPLIT") {return parse_split_stmt(p)}
	if at_keyword(p, "CONDENSE") {return parse_condense_stmt(p)}
	if at_keyword(p, "REPLACE") {return parse_replace_stmt(p)}
	if at_keyword(p, "TRANSLATE") {return parse_translate_stmt(p)}
	if at_keyword(p, "SHIFT") {return parse_shift_stmt(p)}
	if at_keyword(p, "FIND") {return parse_find_stmt(p)}
	if at_keyword(p, "SEARCH") {return parse_search_stmt(p)}
	if at_keyword(p, "PERFORM") {return parse_perform_stmt(p)}
	if at_keyword(p, "CALL") {return parse_call_stmt(p)}
	if at_keyword(p, "SUBMIT") {return parse_submit_stmt(p)}
	if at_keyword(p, "MESSAGE") {return parse_message_stmt(p)}
	return parse_write_stmt(p)
}

simple_stmt_done :: proc(p: ^Parser, body_start: int) -> bool {
	tok := current_token(p)
	return(
		tok.kind == .Period ||
		tok.kind == .Eof ||
		(p.index > body_start &&
				.Has_Newline_Before in tok.flags &&
				statement_lead_starts(p, p.index)) \
	)
}

simple_stmt_range :: proc(p: ^Parser, start: Token) -> tokenizer.Range {
	period := expect_token(p, .Period)
	return tokenizer.text_range(start.range.start, statement_end(p, period))
}

simple_current_keyword_in :: proc(p: ^Parser, keywords: []string) -> bool {
	for keyword in keywords {
		if at_keyword_phrase(p, keyword) {
			return true
		}
	}
	return false
}

simple_expr :: proc(p: ^Parser, body_start: int, stop_keywords: []string) -> ^ast.Expr {
	if simple_stmt_done(p, body_start) ||
	   current_token(p).kind == .Comma ||
	   current_token(p).kind == .Colon ||
	   simple_current_keyword_in(p, stop_keywords) {
		return nil
	}
	if !expr_lead_token(current_token(p)) {
		return nil
	}
	return parse_expr(p)
}

required_simple_expr :: proc(p: ^Parser, body_start: int, stop_keywords: []string) -> ^ast.Expr {
	expr := simple_expr(p, body_start, stop_keywords)
	if expr == nil {
		error_current(p, "syntax error: expected expression")
	}
	return expr
}

plain_current_expr :: proc(p: ^Parser) -> ^ast.Expr {
	tok := current_token(p)
	if tok.kind == .Number || tok.kind == .String {
		bump_token(p)
		expr := ast.new(ast.Literal_Expr, tok.range, p.allocator)
		expr.value = tokenizer.token_lexeme(tok, p.source)
		return expr
	}
	if tok.kind == .Ident {
		bump_token(p)
		expr := ast.new(ast.Ident_Expr, tok.range, p.allocator)
		expr.name = tokenizer.token_lexeme(tok, p.source)
		return expr
	}
	return nil
}

parse_exprs_until :: proc(
	p: ^Parser,
	body_start: int,
	stop_keywords: []string,
) -> [dynamic]^ast.Expr {
	values := make([dynamic]^ast.Expr, 0, 2, p.allocator)
	for !simple_stmt_done(p, body_start) && !simple_current_keyword_in(p, stop_keywords) {
		if allow_token(p, .Colon) || allow_token(p, .Comma) {
			continue
		}
		start := p.index
		value := simple_expr(p, body_start, stop_keywords)
		if value == nil {
			break
		}
		append(&values, value)
		ensure_forward_progress(p, start)
	}
	return values
}

consume_simple_entry_tail :: proc(p: ^Parser, body_start: int) {
	for !simple_stmt_done(p, body_start) && current_token(p).kind != .Comma {
		bump_token(p)
	}
}

allow_keyword_phrase :: proc(p: ^Parser, keyword: string) -> bool {
	if at_keyword_phrase(p, keyword) {
		expect_keyword_phrase(p, keyword)
		return true
	}
	return false
}

allow_hyphen2 :: proc(p: ^Parser, a, b: string) -> bool {
	if !hyphen2_at(p, p.index, a, b) {
		return false
	}
	bump_token(p)
	bump_token(p)
	bump_token(p)
	return true
}

parse_clear_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "CLEAR")
	body_start := p.index
	stmt := ast.new(ast.Clear_Stmt, start.range, p.allocator)
	stmt.operands = make([dynamic]ast.Clear_Operand_Clause, 0, 2, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {continue}
		target := required_simple_expr(p, body_start, []string{"WITH", "INITIAL"})
		if target == nil {break}
		clause := ast.Clear_Operand_Clause {
			target = target,
		}
		if allow_keyword(p, "WITH") {
			clause.mode = .With_Value
			clause.value = required_simple_expr(p, body_start, []string{})
		} else if allow_keyword(p, "INITIAL") {
			clause.mode = .Initial
		}
		append(&stmt.operands, clause)
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_refresh_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "REFRESH")
	body_start := p.index
	stmt := ast.new(ast.Refresh_Stmt, start.range, p.allocator)
	stmt.operands = make([dynamic]ast.Refresh_Operand_Clause, 0, 2, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {continue}
		table := allow_keyword(p, "TABLE")
		target := required_simple_expr(p, body_start, []string{})
		if target == nil {break}
		append(&stmt.operands, ast.Refresh_Operand_Clause{target = target, table = table})
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_free_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "FREE")
	body_start := p.index
	stmt := ast.new(ast.Free_Stmt, start.range, p.allocator)
	stmt.operands = make([dynamic]ast.Free_Operand_Clause, 0, 2, p.allocator)
	if allow_keyword(p, "MEMORY") {
		stmt.memory = true
		if allow_keyword(p, "ID") {
			stmt.memory_id = required_simple_expr(p, body_start, []string{})
		}
		consume_simple_entry_tail(p, body_start)
		stmt.range = simple_stmt_range(p, start)
		return stmt
	}
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {continue}
		object := allow_keyword(p, "OBJECT")
		target := required_simple_expr(p, body_start, []string{})
		if target == nil {break}
		append(&stmt.operands, ast.Free_Operand_Clause{target = target, object = object})
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_unassign_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "UNASSIGN")
	body_start := p.index
	stmt := ast.new(ast.Unassign_Stmt, start.range, p.allocator)
	stmt.operands = make([dynamic]ast.Unassign_Operand_Clause, 0, 1, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {continue}
		target := required_simple_expr(p, body_start, []string{})
		if target == nil {break}
		append(&stmt.operands, ast.Unassign_Operand_Clause{target = target})
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_move_entry :: proc(p: ^Parser, body_start: int) -> (ast.Move_Entry_Clause, bool) {
	source := required_simple_expr(p, body_start, []string{"TO"})
	if source == nil {return ast.Move_Entry_Clause{}, false}
	if !allow_keyword(p, "TO") {
		error_current(p, "syntax error: expected keyword")
		return ast.Move_Entry_Clause{}, false
	}
	target := required_simple_expr(p, body_start, []string{})
	if target == nil {return ast.Move_Entry_Clause{}, false}
	return ast.Move_Entry_Clause{source = source, target = target}, true
}

parse_move_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "MOVE")
	body_start := p.index
	stmt := ast.new(ast.Move_Stmt, start.range, p.allocator)
	stmt.entries = make([dynamic]ast.Move_Entry_Clause, 0, 2, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {continue}
		entry, ok := parse_move_entry(p, body_start)
		if !ok {break}
		append(&stmt.entries, entry)
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_add_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "ADD")
	body_start := p.index
	stmt := ast.new(ast.Add_Stmt, start.range, p.allocator)
	stmt.entries = make([dynamic]ast.Add_Entry_Clause, 0, 2, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {continue}
		source := required_simple_expr(p, body_start, []string{"TO"})
		if source == nil {break}
		if !allow_keyword(p, "TO") {error_current(p, "syntax error: expected keyword"); break}
		target := required_simple_expr(p, body_start, []string{"GIVING"})
		entry := ast.Add_Entry_Clause {
			source = source,
			target = target,
		}
		if allow_keyword(p, "GIVING") {
			entry.result = required_simple_expr(p, body_start, []string{})
		}
		append(&stmt.entries, entry)
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_subtract_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "SUBTRACT")
	body_start := p.index
	stmt := ast.new(ast.Subtract_Stmt, start.range, p.allocator)
	stmt.entries = make([dynamic]ast.Subtract_Entry_Clause, 0, 2, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {continue}
		source := required_simple_expr(p, body_start, []string{"FROM"})
		if source == nil {break}
		if !allow_keyword(p, "FROM") {error_current(p, "syntax error: expected keyword"); break}
		target := required_simple_expr(p, body_start, []string{"GIVING"})
		entry := ast.Subtract_Entry_Clause {
			source = source,
			target = target,
		}
		if allow_keyword(p, "GIVING") {
			entry.result = required_simple_expr(p, body_start, []string{})
		}
		append(&stmt.entries, entry)
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_multiply_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "MULTIPLY")
	body_start := p.index
	stmt := ast.new(ast.Multiply_Stmt, start.range, p.allocator)
	stmt.entries = make([dynamic]ast.Multiply_Entry_Clause, 0, 2, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {continue}
		target := required_simple_expr(p, body_start, []string{"BY"})
		if target == nil {break}
		if !allow_keyword(p, "BY") {error_current(p, "syntax error: expected keyword"); break}
		source := required_simple_expr(p, body_start, []string{"GIVING"})
		entry := ast.Multiply_Entry_Clause {
			target = target,
			source = source,
		}
		if allow_keyword(p, "GIVING") {
			entry.result = required_simple_expr(p, body_start, []string{})
		}
		append(&stmt.entries, entry)
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_divide_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "DIVIDE")
	body_start := p.index
	stmt := ast.new(ast.Divide_Stmt, start.range, p.allocator)
	stmt.entries = make([dynamic]ast.Divide_Entry_Clause, 0, 2, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {continue}
		first := required_simple_expr(p, body_start, []string{"BY", "INTO"})
		if first == nil {break}
		entry := ast.Divide_Entry_Clause{}
		if allow_keyword(p, "INTO") {
			entry.form = .Into
			entry.source = first
			entry.target = required_simple_expr(p, body_start, []string{"GIVING"})
		} else if allow_keyword(p, "BY") {
			entry.form = .By
			entry.target = first
			entry.source = required_simple_expr(p, body_start, []string{"GIVING"})
		} else {
			error_current(p, "syntax error: expected keyword")
			break
		}
		if allow_keyword(p, "GIVING") {
			entry.result = required_simple_expr(p, body_start, []string{})
		}
		append(&stmt.entries, entry)
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_compute_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "COMPUTE")
	body_start := p.index
	stmt := ast.new(ast.Compute_Stmt, start.range, p.allocator)
	stmt.entries = make([dynamic]ast.Compute_Entry_Clause, 0, 1, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {continue}
		entry := ast.Compute_Entry_Clause {
			exact = allow_keyword(p, "EXACT"),
		}
		entry.target = required_simple_expr(p, body_start, []string{})
		if entry.target == nil {break}
		if !allow_token(p, .Eq) {
			error_current(p, "syntax error: expected assignment operator")
			break
		}
		entry.source = required_simple_expr(p, body_start, []string{})
		append(&stmt.entries, entry)
		consume_simple_entry_tail(p, body_start)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_concatenate_entry :: proc(
	p: ^Parser,
	body_start: int,
) -> (
	ast.Concatenate_Entry_Clause,
	bool,
) {
	entry := ast.Concatenate_Entry_Clause{}
	entry.sources = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	if allow_keyword(p, "LINES") {
		if !allow_keyword(
			p,
			"OF",
		) {error_current(p, "syntax error: expected keyword"); return entry, false}
		entry.lines_of = true
		source := required_simple_expr(p, body_start, []string{"INTO"})
		if source == nil {return entry, false}
		append(&entry.sources, source)
	} else {
		entry.sources = parse_exprs_until(p, body_start, []string{"INTO"})
	}
	if !allow_keyword(p, "INTO") {
		error_current(p, "syntax error: expected keyword")
		return entry, false
	}
	entry.target = required_simple_expr(p, body_start, []string{"SEPARATED", "RESPECTING", "IN"})
	for !simple_stmt_done(p, body_start) && current_token(p).kind != .Comma {
		if allow_keyword(p, "SEPARATED") {
			if !allow_keyword(p, "BY") {error_current(p, "syntax error: expected keyword"); break}
			entry.separator = required_simple_expr(p, body_start, []string{"RESPECTING", "IN"})
			continue
		}
		if allow_keyword(p, "RESPECTING") {
			entry.respecting_blanks = allow_keyword(p, "BLANKS")
			continue
		}
		bump_token(p)
	}
	return entry, entry.target != nil && len(entry.sources) > 0
}

parse_concatenate_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "CONCATENATE")
	body_start := p.index
	stmt := ast.new(ast.Concatenate_Stmt, start.range, p.allocator)
	stmt.entries = make([dynamic]ast.Concatenate_Entry_Clause, 0, 1, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {continue}
		entry, ok := parse_concatenate_entry(p, body_start)
		if ok {append(&stmt.entries, entry)} else {break}
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_split_entry :: proc(p: ^Parser, body_start: int) -> (ast.Split_Entry_Clause, bool) {
	entry := ast.Split_Entry_Clause{}
	entry.source = required_simple_expr(p, body_start, []string{"AT"})
	if entry.source == nil {return entry, false}
	if !allow_keyword(
		p,
		"AT",
	) {error_current(p, "syntax error: expected keyword"); return entry, false}
	entry.separator = required_simple_expr(p, body_start, []string{"INTO"})
	if !allow_keyword(
		p,
		"INTO",
	) {error_current(p, "syntax error: expected keyword"); return entry, false}
	entry.into_table = allow_keyword(p, "TABLE")
	entry.targets = parse_exprs_until(p, body_start, []string{"IN"})
	consume_simple_entry_tail(p, body_start)
	return entry, entry.separator != nil && len(entry.targets) > 0
}

parse_split_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "SPLIT")
	body_start := p.index
	stmt := ast.new(ast.Split_Stmt, start.range, p.allocator)
	stmt.entries = make([dynamic]ast.Split_Entry_Clause, 0, 1, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {continue}
		entry, ok := parse_split_entry(p, body_start)
		if ok {append(&stmt.entries, entry)} else {break}
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_condense_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "CONDENSE")
	body_start := p.index
	stmt := ast.new(ast.Condense_Stmt, start.range, p.allocator)
	stmt.target = required_simple_expr(p, body_start, []string{"NO"})
	if allow_keyword(p, "NO") {
		if allow_token(p, .Minus) && allow_keyword(p, "GAPS") {
			stmt.no_gaps = true
		}
	}
	consume_simple_entry_tail(p, body_start)
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_replace_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "REPLACE")
	body_start := p.index
	stmt := ast.new(ast.Replace_Stmt, start.range, p.allocator)
	if allow_keyword(p, "FIRST") {
		stmt.occurrence = .First
		allow_keyword(p, "OCCURRENCE")
		allow_keyword(p, "OCCURRENCES")
	} else if allow_keyword(p, "ALL") {
		stmt.occurrence = .All
		allow_keyword(p, "OCCURRENCE")
		allow_keyword(p, "OCCURRENCES")
	}
	allow_keyword(p, "OF")
	stmt.regex = allow_keyword(p, "REGEX")
	stmt.pattern = required_simple_expr(p, body_start, []string{"IN", "WITH"})
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, "IN") {
			if (allow_keyword(p, "CHARACTER") || allow_keyword(p, "BYTE")) &&
			   allow_keyword(p, "MODE") {
				continue
			}
			stmt.in_table = allow_keyword(p, "TABLE")
			stmt.target = required_simple_expr(p, body_start, []string{"WITH", "IN"})
			continue
		}
		if allow_keyword(p, "WITH") {
			stmt.replacement = required_simple_expr(p, body_start, []string{"IN"})
			continue
		}
		bump_token(p)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_translate_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "TRANSLATE")
	body_start := p.index
	stmt := ast.new(ast.Translate_Stmt, start.range, p.allocator)
	stmt.target = required_simple_expr(p, body_start, []string{"TO", "FROM", "USING"})
	if allow_keyword(p, "USING") {
		stmt.form = .Using
		stmt.operand = required_simple_expr(p, body_start, []string{})
	} else if allow_keyword(p, "TO") || allow_keyword(p, "FROM") {
		from_form := token_is_keyword(p, previous_token(p), "FROM")
		if allow_keyword(p, "UPPER") {
			allow_keyword(p, "CASE")
			stmt.form = .To_Upper
		} else if allow_keyword(p, "LOWER") {
			allow_keyword(p, "CASE")
			stmt.form = .To_Lower
		} else if allow_keyword(p, "CODE") {
			allow_keyword(p, "PAGE")
			stmt.form = .From_Code_Page if from_form else .To_Code_Page
			stmt.operand = required_simple_expr(p, body_start, []string{})
		} else if allow_keyword(p, "NUMBER") {
			allow_keyword(p, "FORMAT")
			stmt.form = .From_Number_Format if from_form else .To_Number_Format
			stmt.operand = required_simple_expr(p, body_start, []string{})
		}
	}
	consume_simple_entry_tail(p, body_start)
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_shift_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "SHIFT")
	body_start := p.index
	stmt := ast.new(ast.Shift_Stmt, start.range, p.allocator)
	stmt.target = required_simple_expr(
		p,
		body_start,
		[]string{"BY", "UP", "LEFT", "RIGHT", "CIRCULAR", "DELETING", "IN"},
	)
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, "LEFT") {
			stmt.direction = .Left
			continue
		}
		if allow_keyword(p, "RIGHT") {
			stmt.direction = .Right
			continue
		}
		if allow_keyword(p, "CIRCULAR") {
			stmt.circular = true
			continue
		}
		if allow_keyword(p, "BY") {
			stmt.places = required_simple_expr(
				p,
				body_start,
				[]string{"PLACES", "LEFT", "RIGHT", "CIRCULAR", "DELETING", "IN", "UP"},
			)
			allow_keyword(p, "PLACES")
			continue
		}
		if allow_keyword(p, "DELETING") {
			if allow_keyword(
				p,
				"LEADING",
			) {stmt.delete_direction = .Leading} else if allow_keyword(p, "TRAILING") {stmt.delete_direction = .Trailing}
			stmt.delete_pattern = required_simple_expr(
				p,
				body_start,
				[]string{"LEFT", "RIGHT", "CIRCULAR", "IN", "UP"},
			)
			continue
		}
		bump_token(p)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_find_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "FIND")
	body_start := p.index
	stmt := ast.new(ast.Find_Stmt, start.range, p.allocator)
	stmt.submatches = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	if allow_keyword(p, "FIRST") {
		stmt.occurrence = .First
		allow_keyword(p, "OCCURRENCE")
		allow_keyword(p, "OCCURRENCES")
	} else if allow_keyword(p, "ALL") {
		stmt.occurrence = .All
		allow_keyword(p, "OCCURRENCE")
		allow_keyword(p, "OCCURRENCES")
	}
	allow_keyword(p, "OF")
	stmt.regex = allow_keyword(p, "REGEX")
	stmt.pattern = required_simple_expr(p, body_start, []string{"IN"})
	if allow_keyword(p, "IN") {
		stmt.target = required_simple_expr(
			p,
			body_start,
			[]string{"MATCH", "SUBMATCHES", "RESULTS", "IGNORING", "RESPECTING"},
		)
	}
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, "MATCH") {
			if allow_keyword(p, "OFFSET") {
				stmt.match_offset = required_simple_expr(
					p,
					body_start,
					[]string{"MATCH", "SUBMATCHES", "RESULTS", "IGNORING", "RESPECTING"},
				)
			} else if allow_keyword(p, "LENGTH") {
				stmt.match_length = required_simple_expr(
					p,
					body_start,
					[]string{"MATCH", "SUBMATCHES", "RESULTS", "IGNORING", "RESPECTING"},
				)
			} else {
				stmt.match_offset = required_simple_expr(
					p,
					body_start,
					[]string{"MATCH", "SUBMATCHES", "RESULTS", "IGNORING", "RESPECTING"},
				)
			}
			continue
		}
		if allow_keyword(p, "RESULTS") {
			stmt.results = required_simple_expr(
				p,
				body_start,
				[]string{"MATCH", "SUBMATCHES", "IGNORING", "RESPECTING"},
			)
			continue
		}
		if allow_keyword(p, "SUBMATCHES") {
			more := parse_exprs_until(
				p,
				body_start,
				[]string{"MATCH", "RESULTS", "IGNORING", "RESPECTING"},
			)
			for value in more {append(&stmt.submatches, value)}
			continue
		}
		bump_token(p)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_search_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "SEARCH")
	body_start := p.index
	stmt := ast.new(ast.Search_Stmt, start.range, p.allocator)
	stmt.target = required_simple_expr(p, body_start, []string{"FOR"})
	if allow_keyword(p, "FOR") {
		stmt.pattern = required_simple_expr(
			p,
			body_start,
			[]string{"STARTING", "ENDING", "AND", "ABBREVIATED"},
		)
	}
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, "STARTING") {
			allow_keyword(p, "AT")
			stmt.starting_at = required_simple_expr(
				p,
				body_start,
				[]string{"ENDING", "AND", "ABBREVIATED"},
			)
			continue
		}
		if allow_keyword(p, "ENDING") {
			allow_keyword(p, "AT")
			stmt.ending_at = required_simple_expr(
				p,
				body_start,
				[]string{"STARTING", "AND", "ABBREVIATED"},
			)
			continue
		}
		if allow_keyword(p, "ABBREVIATED") {
			stmt.abbreviated = true
			continue
		}
		bump_token(p)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_perform_args :: proc(p: ^Parser, body_start: int, list: ^[dynamic]^ast.Expr) {
	for !simple_stmt_done(p, body_start) &&
	    !simple_current_keyword_in(p, []string{"TABLES", "USING", "CHANGING", "IF"}) {
		if allow_token(p, .Comma) {continue}
		value := simple_expr(p, body_start, []string{"TABLES", "USING", "CHANGING", "IF"})
		if value == nil {break}
		append(list, value)
	}
}

parse_perform_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "PERFORM")
	body_start := p.index
	stmt := ast.new(ast.Perform_Stmt, start.range, p.allocator)
	stmt.tables = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	stmt.using_args = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	stmt.changing = make([dynamic]^ast.Expr, 0, 2, p.allocator)
	allow_token(p, .Colon)
	stmt.form = required_simple_expr(
		p,
		body_start,
		[]string{"IN", "TABLES", "USING", "CHANGING", "IF"},
	)
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, "IN") {
			if !allow_keyword(
				p,
				"PROGRAM",
			) {error_current(p, "syntax error: expected keyword"); break}
			stmt.program = required_simple_expr(
				p,
				body_start,
				[]string{"TABLES", "USING", "CHANGING", "IF"},
			)
			continue
		}
		if allow_keyword(p, "TABLES") {
			parse_perform_args(p, body_start, &stmt.tables)
			continue
		}
		if allow_keyword(p, "USING") {
			parse_perform_args(p, body_start, &stmt.using_args)
			continue
		}
		if allow_keyword(p, "CHANGING") {
			parse_perform_args(p, body_start, &stmt.changing)
			continue
		}
		if allow_keyword(p, "IF") {
			stmt.if_found = allow_keyword(p, "FOUND")
			continue
		}
		bump_token(p)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_call_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "CALL")
	body_start := p.index
	stmt := ast.new(ast.Call_Stmt, start.range, p.allocator)
	if allow_keyword(p, "METHOD") {
		stmt.kind = .Method
	} else if allow_keyword(p, "FUNCTION") {
		stmt.kind = .Function
	} else if allow_keyword(p, "CUSTOMER") {
		allow_token(p, .Minus)
		allow_keyword(p, "FUNCTION")
		stmt.kind = .Customer_Function
	} else if allow_keyword(p, "DATABASE") {
		allow_keyword(p, "PROCEDURE")
		stmt.kind = .Database_Procedure
	} else if allow_keyword(p, "TRANSFORMATION") {
		stmt.kind = .Transformation
	} else if allow_keyword(p, "BADI") {
		stmt.kind = .Badi
	} else if allow_keyword(p, "SCREEN") {
		stmt.kind = .Screen
	} else if allow_keyword(p, "SELECTION") {
		allow_token(p, .Minus)
		allow_keyword(p, "SCREEN")
		stmt.kind = .Selection_Screen
	} else if allow_keyword(p, "TRANSACTION") {
		stmt.kind = .Transaction
	} else if allow_keyword(p, "DIALOG") {
		stmt.kind = .Dialog
	} else if allow_keyword(p, "SUBSCREEN") {
		stmt.kind = .Subscreen
	}
	stmt.target = simple_expr(
		p,
		body_start,
		[]string {
			"EXPORTING",
			"IMPORTING",
			"CHANGING",
			"TABLES",
			"EXCEPTIONS",
			"USING",
			"AND",
			"WITH",
		},
	)
	consume_simple_entry_tail(p, body_start)
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_submit_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "SUBMIT")
	body_start := p.index
	stmt := ast.new(ast.Submit_Stmt, start.range, p.allocator)
	stmt.options = make([dynamic]ast.Submit_Option_Clause, 0, 2, p.allocator)
	stmt.target = simple_expr(
		p,
		body_start,
		[]string {
			"USING",
			"VIA",
			"WITH",
			"LINE",
			"EXPORTING",
			"TO",
			"SPOOL",
			"ARCHIVE",
			"WITHOUT",
			"USER",
			"NUMBER",
			"LANGUAGE",
			"AND",
		},
	)
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, "AND") {
			stmt.and_return = allow_keyword(p, "RETURN")
			continue
		}
		if allow_keyword(p, "VIA") {
			if allow_hyphen2(
				p,
				"SELECTION",
				"SCREEN",
			) {stmt.via_selection_screen = true} else if allow_keyword(p, "JOB") {
				value := required_simple_expr(
					p,
					body_start,
					[]string{"NUMBER", "LANGUAGE", "AND", "WITH", "USER"},
				)
				append(&stmt.options, ast.Submit_Option_Clause{kind = .Via_Job, value = value})
			}
			continue
		}
		if allow_keyword(p, "EXPORTING") {
			if allow_keyword(p, "LIST") && allow_keyword(p, "TO") && allow_keyword(p, "MEMORY") {
				stmt.exporting_list_to_memory = true
			}
			continue
		}
		if allow_keyword(p, "TO") {
			if allow_keyword(p, "SAP") {
				allow_token(p, .Minus)
				stmt.to_sap_spool = allow_keyword(p, "SPOOL")
			}
			continue
		}
		if allow_keyword(p, "WITHOUT") {
			if allow_keyword(p, "SPOOL") && allow_keyword(p, "DYNPRO") {
				stmt.without_spool_dynpro = true
			}
			continue
		}
		if allow_keyword(p, "USING") {
			if allow_hyphen2(p, "SELECTION", "SCREEN") {
				value := required_simple_expr(p, body_start, []string{})
				append(
					&stmt.options,
					ast.Submit_Option_Clause{kind = .Using_Selection_Screen, value = value},
				)
			} else if allow_hyphen2(p, "SELECTION", "SET") {
				value := required_simple_expr(p, body_start, []string{})
				append(
					&stmt.options,
					ast.Submit_Option_Clause{kind = .Using_Selection_Set, value = value},
				)
			}
			continue
		}
		if allow_keyword(p, "WITH") {
			if allow_hyphen2(p, "SELECTION", "TABLE") {
				value := required_simple_expr(p, body_start, []string{})
				append(
					&stmt.options,
					ast.Submit_Option_Clause{kind = .With_Selection_Table, value = value},
				)
				continue
			}
			if allow_keyword(p, "FREE") && allow_keyword(p, "SELECTIONS") {
				value := required_simple_expr(p, body_start, []string{})
				append(
					&stmt.options,
					ast.Submit_Option_Clause{kind = .With_Free_Selections, value = value},
				)
				continue
			}
			name := current_token(p)
			if name.kind == .Ident {
				bump_token(p)
				option := ast.Submit_Option_Clause {
					kind = .With_Parameter,
					name = tokenizer.token_lexeme(name, p.source),
				}
				if current_token(p).kind == .Eq || current_token(p).kind == .Ident {
					op := bump_token(p)
					option.operator = submit_option_operator(p, op)
					option.value = simple_expr(
						p,
						body_start,
						[]string{"WITH", "AND", "VIA", "USER", "LANGUAGE"},
					)
				}
				append(&stmt.options, option)
			}
			continue
		}
		if allow_hyphen2(p, "LINE", "SIZE") || allow_keyword_phrase(p, "LINE-SIZE") {
			value := required_simple_expr(p, body_start, []string{})
			append(&stmt.options, ast.Submit_Option_Clause{kind = .Line_Size, value = value})
			continue
		}
		if allow_hyphen2(p, "LINE", "COUNT") || allow_keyword_phrase(p, "LINE-COUNT") {
			value := required_simple_expr(p, body_start, []string{})
			append(&stmt.options, ast.Submit_Option_Clause{kind = .Line_Count, value = value})
			continue
		}
		if allow_keyword(p, "USER") {
			value := required_simple_expr(p, body_start, []string{})
			append(&stmt.options, ast.Submit_Option_Clause{kind = .User, value = value})
			continue
		}
		if allow_keyword(p, "NUMBER") {
			value := required_simple_expr(p, body_start, []string{})
			append(&stmt.options, ast.Submit_Option_Clause{kind = .Number, value = value})
			continue
		}
		if allow_keyword(p, "LANGUAGE") {
			value := required_simple_expr(p, body_start, []string{})
			append(&stmt.options, ast.Submit_Option_Clause{kind = .Language, value = value})
			continue
		}
		bump_token(p)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

submit_option_operator :: proc(p: ^Parser, tok: Token) -> ast.Submit_Option_Operator {
	if tok.kind == .Eq {return .Assign}
	if token_is_keyword(p, tok, "EQ") {return .Eq}
	if token_is_keyword(p, tok, "NE") {return .Ne}
	if token_is_keyword(p, tok, "BT") {return .Bt}
	if token_is_keyword(p, tok, "NB") {return .Nb}
	if token_is_keyword(p, tok, "CP") {return .Cp}
	if token_is_keyword(p, tok, "NP") {return .Np}
	if token_is_keyword(p, tok, "GE") {return .Ge}
	if token_is_keyword(p, tok, "GT") {return .Gt}
	if token_is_keyword(p, tok, "LE") {return .Le}
	if token_is_keyword(p, tok, "LT") {return .Lt}
	return .Other
}

parse_message_head :: proc(p: ^Parser, body_start: int) -> ^ast.Message_Head_Clause {
	head, _ := mem.new(ast.Message_Head_Clause, p.allocator)
	if allow_keyword(p, "ID") {
		head.id = required_simple_expr(
			p,
			body_start,
			[]string{"TYPE", "NUMBER", "WITH", "INTO", "DISPLAY", "RAISING"},
		)
		if allow_keyword(p, "TYPE") {
			head.msg_type = required_simple_expr(
				p,
				body_start,
				[]string{"NUMBER", "WITH", "INTO", "DISPLAY", "RAISING"},
			)
		}
		if allow_keyword(p, "NUMBER") {
			head.number = required_simple_expr(
				p,
				body_start,
				[]string{"WITH", "INTO", "DISPLAY", "RAISING"},
			)
		}
		return head
	}
	head.code = simple_expr(p, body_start, []string{"TYPE", "WITH", "INTO", "DISPLAY", "RAISING"})
	if allow_keyword(p, "TYPE") {
		head.msg_type = required_simple_expr(
			p,
			body_start,
			[]string{"WITH", "INTO", "DISPLAY", "RAISING"},
		)
	}
	return head
}

parse_message_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "MESSAGE")
	body_start := p.index
	stmt := ast.new(ast.Message_Stmt, start.range, p.allocator)
	stmt.with_args = make([dynamic]^ast.Expr, 0, 4, p.allocator)
	stmt.head = parse_message_head(p, body_start)
	for !simple_stmt_done(p, body_start) {
		if allow_keyword(p, "WITH") {
			values := parse_exprs_until(p, body_start, []string{"INTO", "DISPLAY", "RAISING"})
			for value in values {append(&stmt.with_args, value)}
			continue
		}
		if allow_keyword(p, "INTO") {
			stmt.into = required_simple_expr(p, body_start, []string{"DISPLAY", "RAISING"})
			continue
		}
		if allow_keyword(p, "DISPLAY") {
			allow_keyword(p, "LIKE")
			stmt.display_like = required_simple_expr(p, body_start, []string{"RAISING"})
			continue
		}
		if allow_keyword(p, "RAISING") {
			stmt.raising = required_simple_expr(p, body_start, []string{})
			continue
		}
		bump_token(p)
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_write_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "WRITE")
	body_start := p.index
	stmt := ast.new(ast.Write_Stmt, start.range, p.allocator)
	stmt.operands = make([dynamic]ast.Write_Operand_Clause, 0, 2, p.allocator)
	allow_token(p, .Colon)
	for !simple_stmt_done(p, body_start) {
		if allow_token(p, .Comma) {continue}
		clause := ast.Write_Operand_Clause{}
		if allow_token(p, .Slash) {
			clause.line_break = true
			if current_token(p).kind == .Number {
				clause.position = plain_current_expr(p)
			}
			if allow_token(p, .LParen) {
				clause.length = simple_expr(p, body_start, []string{})
				expect_token(p, .RParen)
			}
		}
		if allow_keyword(p, "AT") {
			clause.position = simple_expr(p, body_start, []string{})
			if allow_token(p, .LParen) {
				clause.length = simple_expr(p, body_start, []string{})
				expect_token(p, .RParen)
			}
		}
		clause.value = simple_expr(p, body_start, []string{})
		if clause.value != nil ||
		   clause.line_break ||
		   clause.position != nil ||
		   clause.length != nil {
			append(&stmt.operands, clause)
		} else {
			bump_token(p)
		}
	}
	stmt.range = simple_stmt_range(p, start)
	return stmt
}

parse_control_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	if at_keyword(p, "IF") {return parse_if_stmt(p)}
	if at_keyword(p, "CASE") {return parse_case_stmt(p)}
	if at_keyword(p, "WHILE") {return parse_while_stmt(p)}
	if at_keyword(p, "DO") {return parse_do_stmt(p)}
	if at_keyword(p, "LOOP") {return parse_loop_stmt(p)}
	if at_group_stmt_starts(p) {return parse_at_stmt(p)}
	return parse_try_stmt(p)
}

parse_if_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "IF")
	condition := parse_logical_expr(p)
	if condition == nil {
		return nil
	}
	period := expect_token(p, .Period)
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

	end := expect_keyword(p, "ENDIF")
	if !token_is_keyword(p, end, "ENDIF") {
		return nil
	}
	period = expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	stmt.range = tokenizer.text_range(start.range.start, period.range.end)
	return stmt
}

parse_elseif_clause :: proc(p: ^Parser) -> ^ast.Elseif_Clause {
	start := expect_keyword(p, "ELSEIF")
	condition := parse_logical_expr(p)
	if condition == nil {
		return nil
	}
	period := expect_token(p, .Period)
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
	expr := parse_expr(p)
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
	for at_keyword(p, "WHEN") {
		when_clause := parse_when_clause(p, is_type_of)
		if when_clause == nil {
			return nil
		}
		append(&stmt.whens, when_clause)
	}
	end := expect_keyword(p, "ENDCASE")
	if !token_is_keyword(p, end, "ENDCASE") {
		return nil
	}
	period = expect_token(p, .Period)
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
		operand := parse_expr(p)
		if operand == nil {
			return nil
		}
		append(&clause.operands, operand)
		for allow_keyword(p, "OR") {
			next := parse_expr(p)
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
	period := expect_token(p, .Period)
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
	condition := parse_logical_expr(p)
	if condition == nil {
		return nil
	}
	period := expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	stmt := ast.new(ast.While_Stmt, start.range, p.allocator)
	stmt.condition = condition
	stmt.body = parse_stmt_list_until(p, []string{"ENDWHILE"})
	end := expect_keyword(p, "ENDWHILE")
	if !token_is_keyword(p, end, "ENDWHILE") {
		return nil
	}
	period = expect_token(p, .Period)
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
	end := expect_keyword(p, "ENDDO")
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
	source := parse_expr(p)
	if source == nil {
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
	stmt.body = parse_stmt_list_until(p, []string{"ENDLOOP"})
	end := expect_keyword(p, "ENDLOOP")
	if !token_is_keyword(p, end, "ENDLOOP") {
		return nil
	}
	period = expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	stmt.range = tokenizer.text_range(start.range.start, period.range.end)
	return stmt
}

parse_at_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "AT")
	stmt := ast.new(ast.At_Stmt, start.range, p.allocator)
	if at_keyword(p, "FIRST") || at_keyword(p, "LAST") {
		kw := bump_token(p)
		stmt.kind = tokenizer.token_lexeme(kw, p.source)
	} else if allow_keyword(p, "NEW") {
		stmt.kind = "NEW"
		stmt.expr = parse_expr(p)
		if stmt.expr == nil {
			return nil
		}
	} else {
		if !allow_keyword(p, "END") || !allow_keyword(p, "OF") {
			error_current(p, "syntax error: expected group processing header")
			return nil
		}
		stmt.kind = "END OF"
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
	end := expect_keyword(p, "ENDTRY")
	if !token_is_keyword(p, end, "ENDTRY") {
		return nil
	}
	period = expect_token(p, .Period)
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
	period := expect_token(p, .Period)
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
	if at_keyword(
		p,
		"CLASS",
	) {return parse_named_block_stmt(p, ast.Class_Decl, "CLASS", "ENDCLASS")}
	if at_keyword(
		p,
		"INTERFACE",
	) {return parse_named_block_stmt(p, ast.Interface_Decl, "INTERFACE", "ENDINTERFACE")}
	if at_keyword(
		p,
		"METHOD",
	) {return parse_named_block_stmt(p, ast.Method_Decl, "METHOD", "ENDMETHOD")}
	if at_keyword(p, "FORM") {return parse_named_block_stmt(p, ast.Form_Decl, "FORM", "ENDFORM")}
	if at_keyword(
		p,
		"FUNCTION",
	) {return parse_named_block_stmt(p, ast.Function_Decl, "FUNCTION", "ENDFUNCTION")}
	if at_keyword(
		p,
		"MODULE",
	) {return parse_named_block_stmt(p, ast.Module_Decl, "MODULE", "ENDMODULE")}
	if event_block_starts(p) {return parse_event_block_stmt(p)}
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

parse_named_block_stmt :: proc(
	p: ^Parser,
	$T: typeid,
	start_keyword, end_keyword: string,
) -> ^ast.Stmt {
	start := expect_keyword_phrase(p, start_keyword)
	name := first_name_token_until_period(p)
	consume_raw_until_period(p)
	period := expect_token(p, .Period)
	if period.kind != .Period {
		return nil
	}
	stmt := ast.new(T, start.range, p.allocator)
	stmt.name = tokenizer.token_lexeme(name, p.source) if name.kind != .Eof else ""
	stmt.header_range = tokenizer.text_range(start.range.start, period.range.end)
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

parse_data_access_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	if at_keyword(p, "SELECT") || at_keyword(p, "WITH") {
		return parse_select_stmt(p)
	}
	if at_keyword_phrase(p, "OPEN CURSOR") {return parse_open_cursor_stmt(p)}
	if at_keyword(p, "FETCH") {return parse_fetch_stmt(p)}
	if at_keyword_phrase(p, "CLOSE CURSOR") {return parse_close_cursor_stmt(p)}
	if at_keyword_phrase(p, "READ TABLE") {return parse_read_table_stmt(p)}
	if dataset_stmt_starts(p) {return parse_dataset_stmt(p)}
	if at_keyword(p, "REPORT") || at_keyword(p, "PROGRAM") {return parse_report_stmt(p)}
	if report_textpool_stmt_starts(p) {
		if at_keyword_index(p, p.index + 1, "TEXTPOOL") {
			return parse_textpool_stmt(p)
		}
		return parse_report_stmt(p)
	}
	if at_keyword(p, "INSERT") {return parse_insert_stmt(p)}
	if at_keyword(p, "UPDATE") {return parse_update_stmt(p)}
	return parse_delete_stmt(p)
}

data_stmt_done :: proc(p: ^Parser, body_start: int) -> bool {
	tok := current_token(p)
	return(
		tok.kind == .Period ||
		tok.kind == .Eof ||
		(p.index > body_start &&
				.Has_Newline_Before in tok.flags &&
				statement_lead_starts(p, p.index)) \
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
	if current_token(p).kind == .At {
		bump_token(p)
	}
	if !expr_lead_token(current_token(p)) {
		return nil
	}
	return parse_expr(p)
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
		if allow_token(p, .Comma) {continue}
		if allow_token(p, .Colon) {continue}
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

parse_select_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := current_token(p)
	body_start := p.index
	stmt := ast.new(ast.Select_Stmt, start.range, p.allocator)
	stmt.body = make([dynamic]^ast.Stmt, 0, 2, p.allocator)
	if at_keyword(p, "WITH") {
		with_start := bump_token(p)
		query_count := 0
		for !data_stmt_done(p, body_start) && !at_keyword(p, "SELECT") {
			if at_keyword(p, "AS") {
				query_count += 1
			}
			bump_token(p)
		}
		with, _ := mem.new(ast.Select_With_Clause, p.allocator)
		with.range = tokenizer.text_range(with_start.range.start, previous_token(p).range.end)
		with.query_count = query_count
		stmt.with = with
	}
	stmt.query = parse_select_query_clause(p, body_start)
	stmt.range = data_stmt_range(p, start)
	if keyword_phrase_ahead(p, "ENDSELECT") {
		stmt.body = parse_stmt_list_until(p, []string{"ENDSELECT"})
		end := expect_keyword(p, "ENDSELECT")
		if token_is_keyword(p, end, "ENDSELECT") {
			period := expect_token(p, .Period)
			stmt.range.end = statement_end(p, period)
		}
	}
	return stmt
}

parse_select_query_clause :: proc(p: ^Parser, body_start: int) -> ast.Select_Query_Clause {
	query := ast.Select_Query_Clause{}
	query.projections = make([dynamic]^ast.Expr, 0, 4, p.allocator)
	if !allow_keyword(p, "SELECT") {
		return query
	}
	query.single = allow_keyword(p, "SINGLE")
	query.is_distinct = allow_keyword(p, "DISTINCT")
	for !data_stmt_done(p, body_start) && !select_clause_starts(p) {
		if allow_token(p, .Comma) || allow_token(p, .Star) {
			continue
		}
		start := p.index
		value := data_expr(
			p,
			body_start,
			[]string {
				"FROM",
				"INTO",
				"APPENDING",
				"WHERE",
				"FOR",
				"GROUP",
				"ORDER",
				"UP",
				"PACKAGE",
			},
		)
		if value != nil {
			append(&query.projections, value)
		} else {
			bump_token(p)
		}
		ensure_forward_progress(p, start)
	}
	for !data_stmt_done(p, body_start) {
		if allow_keyword(p, "FROM") {
			query.source = data_expr(
				p,
				body_start,
				[]string{"INTO", "APPENDING", "WHERE", "FOR", "GROUP", "ORDER", "UP", "PACKAGE"},
			)
			continue
		}
		if allow_keyword(p, "INTO") {
			query.result = parse_select_result_tail(p, .Into, body_start)
			continue
		}
		if allow_keyword(p, "APPENDING") {
			query.result = parse_select_result_tail(p, .Appending, body_start)
			continue
		}
		if allow_keyword(p, "WHERE") {
			query.dynamic_where = current_token(p).kind == .LParen
			query.where_cond = data_expr(
				p,
				body_start,
				[]string{"GROUP", "ORDER", "UP", "PACKAGE", "FOR"},
			)
			continue
		}
		if allow_keyword(p, "FOR") {
			if allow_keyword(p, "ALL") && allow_keyword(p, "ENTRIES") && allow_keyword(p, "IN") {
				query.for_all_entries = data_expr(
					p,
					body_start,
					[]string{"WHERE", "GROUP", "ORDER", "UP", "PACKAGE"},
				)
			}
			continue
		}
		if allow_keyword(p, "PACKAGE") {
			allow_keyword(p, "SIZE")
			query.package_size = data_expr(
				p,
				body_start,
				[]string{"INTO", "APPENDING", "WHERE", "GROUP", "ORDER", "UP"},
			)
			continue
		}
		if allow_keyword(p, "UP") {
			allow_keyword(p, "TO")
			query.up_to_rows = data_expr(
				p,
				body_start,
				[]string{"ROWS", "INTO", "APPENDING", "WHERE", "GROUP", "ORDER", "PACKAGE"},
			)
			allow_keyword(p, "ROWS")
			continue
		}
		bump_token(p)
	}
	return query
}

parse_select_result_tail :: proc(
	p: ^Parser,
	kind: ast.Select_Result_Kind,
	body_start: int,
) -> ^ast.Select_Result_Clause {
	clause, _ := mem.new(ast.Select_Result_Clause, p.allocator)
	clause.kind = kind
	if allow_keyword(p, "CORRESPONDING") {
		allow_keyword(p, "FIELDS")
		allow_keyword(p, "OF")
		clause.corresponding_fields = true
	}
	clause.table = allow_keyword(p, "TABLE")
	clause.target = data_expr(
		p,
		body_start,
		[]string{"PACKAGE", "WHERE", "GROUP", "ORDER", "UP", "FOR"},
	)
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
		at_keyword(p, "ORDER") ||
		at_keyword(p, "UP") ||
		at_keyword(p, "PACKAGE") \
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
		if allow_token(p, .Comma) {continue}
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
		if current_token(p).kind == .Ident && next_token_kind(p, 1) == .Eq {
			name := bump_token(p)
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
					name = tokenizer.token_lexeme(name, p.source),
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

parse_insert_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "INSERT")
	body_start := p.index
	stmt := ast.new(ast.Insert_Stmt, start.range, p.allocator)
	stmt.assignments = make([dynamic]ast.Sql_Assignment_Clause, 0, 2, p.allocator)
	if allow_keyword(p, "INTO") {
		stmt.form = .Db_Table
		stmt.target = data_expr(p, body_start, []string{"VALUES", "FROM", "SET", "ACCEPTING"})
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

parse_sql_assignments :: proc(
	p: ^Parser,
	body_start: int,
	list: ^[dynamic]ast.Sql_Assignment_Clause,
	stop_keywords: []string,
) {
	for !data_stmt_done(p, body_start) && !data_current_keyword_in(p, stop_keywords) {
		if allow_token(p, .Comma) {continue}
		name := data_expr(p, body_start, stop_keywords)
		if name == nil {
			bump_token(p)
			continue
		}
		if !allow_token(p, .Eq) {
			continue
		}
		value := data_expr(p, body_start, stop_keywords)
		append(list, ast.Sql_Assignment_Clause{name = name, value = value})
	}
}

parse_update_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	start := expect_keyword(p, "UPDATE")
	body_start := p.index
	stmt := ast.new(ast.Update_Stmt, start.range, p.allocator)
	stmt.assignments = make([dynamic]ast.Sql_Assignment_Clause, 0, 2, p.allocator)
	stmt.target = data_expr(
		p,
		body_start,
		[]string{"FROM", "SET", "WHERE", "USING", "CLIENT", "CONNECTION"},
	)
	for !data_stmt_done(p, body_start) {
		if allow_keyword(p, "FROM") {
			stmt.from_table = allow_keyword(p, "TABLE")
			stmt.source = data_expr(
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
			stmt.where_cond = data_expr(p, body_start, []string{"USING", "CLIENT", "CONNECTION"})
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
		stmt.target = data_expr(p, body_start, []string{"WHERE", "CLIENT", "CONNECTION"})
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
			stmt.where_cond = data_expr(p, body_start, []string{"INDEX", "USING", "COMPARING"})
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
			) {stmt.access = .Input} else if allow_keyword(p, "OUTPUT") {stmt.access = .Output} else if allow_keyword(p, "APPENDING") {stmt.access = .Append} else if allow_keyword(p, "UPDATE") {stmt.access = .Update}
			continue
		}
		if allow_keyword(p, "IN") {
			if allow_keyword(
				p,
				"TEXT",
			) {stmt.text_mode = true; allow_keyword(p, "MODE")} else if allow_keyword(p, "BINARY") {stmt.binary_mode = true; allow_keyword(p, "MODE")}
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
		stmt.name = data_expr(p, body_start, []string{"LINE-SIZE", "LINE", "LINE-COUNT"})
	} else if allow_keyword(p, "PROGRAM") {
		stmt.kind = .Program
		stmt.name = data_expr(p, body_start, []string{"LINE-SIZE", "LINE", "LINE-COUNT"})
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
	period := expect_token(p, .Period)
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

parse_assign_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	lhs := parse_expr(p)
	if lhs == nil {
		return nil
	}

	op := allow_token(p, .Eq)
	downcast := false
	if !op {
		downcast = allow_token(p, .QuestionEq)
	}
	if !op && !downcast {
		error_current(p, "syntax error: expected assignment operator")
		return nil
	}

	rhs := parse_expr(p)
	if rhs == nil {
		return nil
	}
	period := expect_token(p, .Period)

	stmt_range := tokenizer.text_range(lhs.range.start, statement_end(p, period))
	if downcast {
		stmt := ast.new(ast.Downcast_Assign_Stmt, stmt_range, p.allocator)
		stmt.lhs = lhs
		stmt.rhs = rhs
		return stmt
	}

	stmt := ast.new(ast.Assign_Stmt, stmt_range, p.allocator)
	stmt.lhs = lhs
	stmt.rhs = rhs
	return stmt
}

parse_expr :: proc(p: ^Parser) -> ^ast.Expr {
	return parse_concat_expr(p)
}

parse_logical_expr :: proc(p: ^Parser) -> ^ast.Expr {
	return parse_or_expr(p)
}

parse_or_expr :: proc(p: ^Parser) -> ^ast.Expr {
	left := parse_and_expr(p)
	if left == nil {
		return nil
	}

	for at_keyword(p, "OR") {
		op := bump_token(p)
		right := parse_and_expr(p)
		if right == nil {
			return nil
		}
		left = build_binary_expr(p, left, .Or, right, op)
	}
	return left
}

parse_and_expr :: proc(p: ^Parser) -> ^ast.Expr {
	left := parse_not_expr(p)
	if left == nil {
		return nil
	}

	for at_keyword(p, "AND") {
		op := bump_token(p)
		right := parse_not_expr(p)
		if right == nil {
			return nil
		}
		left = build_binary_expr(p, left, .And, right, op)
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
		op := bump_token(p)
		right: ^ast.Expr
		if at_keyword(p, "NOT") {
			not_tok := bump_token(p)
			predicate := parse_concat_expr(p)
			if predicate == nil {
				return nil
			}
			not_expr := ast.new(
				ast.Unary_Expr,
				tokenizer.text_range(not_tok.range.start, predicate.range.end),
				p.allocator,
			)
			not_expr.op = .Not
			not_expr.expr = predicate
			right = not_expr
		} else {
			right = parse_concat_expr(p)
			if right == nil {
				return nil
			}
		}
		return build_binary_expr(p, left, .Is, right, op)
	}

	if at_keyword(p, "BETWEEN") {
		op := bump_token(p)
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
		span := ast.new(
			ast.Binary_Expr,
			tokenizer.text_range(low.range.start, high.range.end),
			p.allocator,
		)
		span.left = low
		span.op = .And
		span.right = high
		return build_binary_expr(p, left, .Between, span, op)
	}

	if op, ok := comparison_op(p, current_token(p)); ok {
		op_tok := bump_token(p)
		right := parse_concat_expr(p)
		if right == nil {
			return nil
		}
		return build_binary_expr(p, left, op, right, op_tok)
	}

	return left
}

parse_concat_expr :: proc(p: ^Parser) -> ^ast.Expr {
	left := parse_additive_expr(p)
	if left == nil {
		return nil
	}

	for current_token(p).kind == .Ampersand {
		op := bump_token(p)
		right := parse_additive_expr(p)
		if right == nil {
			return nil
		}
		left = build_binary_expr(p, left, .Concatenate, right, op)
	}
	return left
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

		op_tok := bump_token(p)
		right := parse_multiplicative_expr(p)
		if right == nil {
			return nil
		}
		left = build_binary_expr(p, left, op, right, op_tok)
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
		} else {
			break
		}

		op_tok := bump_token(p)
		right := parse_unary_expr(p)
		if right == nil {
			return nil
		}
		left = build_binary_expr(p, left, op, right, op_tok)
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
	tok := current_token(p)
	#partial switch tok.kind {
	case .Ident:
		if at_keyword(p, "DATA") && next_token_kind(p, 1) == .LParen {
			return parse_data_inline_name_expr(p)
		}
		if at_keyword(p, "FIELD") &&
		   next_token_kind(p, 1) == .Minus &&
		   at_keyword_index(p, p.index + 2, "SYMBOL") {
			return parse_field_symbol_inline_name_expr(p)
		}
		if constructor_keyword(p, tok) {
			return parse_constructor_expr(p)
		}
		bump_token(p)
		expr := ast.new(ast.Ident_Expr, tok.range, p.allocator)
		expr.name = tokenizer.token_lexeme(tok, p.source)
		return expr
	case .Number, .String:
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
	inner := parse_expr(p)
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
	   !(op_tok.kind == .Arrow && field_tok.kind == .Star) {
		error_current(p, "syntax error: expected selector field")
		return nil
	}
	bump_token(p)

	field: ^ast.Expr
	if field_tok.kind == .Number {
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
	offset := parse_concat_expr(p)
	if offset == nil ||
	   current_token(p).kind != .LParen ||
	   !tokens_touch(previous_token(p), current_token(p)) {
		p.index = save_index
		p.previous_index = save_prev
		return nil
	}
	bump_token(p)
	length := parse_concat_expr(p)
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
	length := parse_concat_expr(p)
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
		value := parse_expr(p)
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
	value := parse_expr(p)
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
		for current_token(p).kind != .RParen && current_token(p).kind != .Eof {
			if allow_token(p, .Comma) {
				continue
			}
			start := p.index
			arg := parse_logical_expr(p)
			if arg != nil {
				append(&args, arg)
			} else {
				bump_token(p)
			}
			ensure_forward_progress(p, start)
		}
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
	next := next_significant_index(p, p.index + 1)
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
	op_token: Token,
) -> ^ast.Expr {
	_ = op_token
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
		token_is_keyword(p, tok, "COND") \
	)
}

constructor_kind :: proc(p: ^Parser, tok: Token) -> ast.Constructor_Kind {
	if token_is_keyword(p, tok, "NEW") {return .New}
	if token_is_keyword(p, tok, "VALUE") {return .Value}
	if token_is_keyword(p, tok, "CONV") {return .Conv}
	if token_is_keyword(p, tok, "REF") {return .Ref}
	if token_is_keyword(p, tok, "CAST") {return .Cast}
	if token_is_keyword(p, tok, "EXACT") {return .Exact}
	if token_is_keyword(p, tok, "CORRESPONDING") {return .Corresponding}
	if token_is_keyword(p, tok, "FILTER") {return .Filter}
	if token_is_keyword(p, tok, "REDUCE") {return .Reduce}
	if token_is_keyword(p, tok, "SWITCH") {return .Switch}
	return .Cond
}

call_argument_section_starts :: proc(p: ^Parser) -> bool {
	return(
		at_keyword(p, "EXPORTING") ||
		at_keyword(p, "IMPORTING") ||
		at_keyword(p, "CHANGING") ||
		at_keyword(p, "RECEIVING") ||
		at_keyword(p, "EXCEPTIONS") \
	)
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

previous_stmt_end :: proc(stmts: [dynamic]^ast.Stmt, fallback: int) -> int {
	if len(stmts) == 0 {
		return fallback
	}
	return stmts[len(stmts) - 1].range.end
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
	if at_keyword_phrase(p, "AT SELECTION-SCREEN") {return "AT SELECTION-SCREEN"}
	if at_keyword(p, "INITIALIZATION") {return "INITIALIZATION"}
	if at_keyword_phrase(p, "LOAD-OF-PROGRAM") {return "LOAD-OF-PROGRAM"}
	if at_keyword_phrase(p, "START-OF-SELECTION") {return "START-OF-SELECTION"}
	if at_keyword_phrase(p, "END-OF-SELECTION") {return "END-OF-SELECTION"}
	if at_keyword_phrase(p, "TOP-OF-PAGE") {return "TOP-OF-PAGE"}
	return "END-OF-PAGE"
}

consume_event_header :: proc(p: ^Parser) {
	kind := event_block_kind(p)
	expect_keyword_phrase(p, kind)
	consume_raw_until_period(p)
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
	return expr_lead_token(current_token(p)) && stmt_period_ahead(p, p.index)
}

stmt_period_ahead :: proc(p: ^Parser, start: int) -> bool {
	paren := 0
	bracket := 0
	brace := 0
	for i in start ..< len(p.tokens) {
		tok := p.tokens[i]
		if tok.kind == .Eof {
			return false
		}
		if paren == 0 && bracket == 0 && brace == 0 && tok.kind == .Period {
			return true
		}
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
	}
	return false
}

keyword_phrase_ahead :: proc(p: ^Parser, keyword: string) -> bool {
	paren := 0
	bracket := 0
	brace := 0
	for i in p.index ..< len(p.tokens) {
		tok := p.tokens[i]
		if tok.kind == .Eof {
			return false
		}
		top := paren == 0 && bracket == 0 && brace == 0
		if top && keyword_phrase_at(p, i, keyword) {
			return true
		}
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
	}
	return false
}

consume_raw_until_period :: proc(p: ^Parser) {
	start := p.index
	paren := 0
	bracket := 0
	brace := 0
	for {
		tok := current_token(p)
		if tok.kind == .Eof {
			return
		}
		top := paren == 0 && bracket == 0 && brace == 0
		if top && tok.kind == .Period {
			return
		}
		if top &&
		   p.index > start &&
		   .Has_Newline_Before in tok.flags &&
		   statement_lead_starts(p, p.index) {
			return
		}
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
		bump_token(p)
	}
}

first_name_token_until_period :: proc(p: ^Parser) -> Token {
	for i in p.index ..< len(p.tokens) {
		tok := p.tokens[i]
		if tok.kind == .Period || tok.kind == .Eof {
			break
		}
		if tok.kind == .Ident || tok.kind == .String || tok.kind == .Number {
			return tok
		}
	}
	return Token{kind = .Eof}
}

next_token_kind :: proc(p: ^Parser, offset: int) -> tokenizer.Token_Kind {
	index := p.index + offset
	if index >= 0 && index < len(p.tokens) {
		return p.tokens[index].kind
	}
	return .Eof
}

current_token :: proc(p: ^Parser) -> tokenizer.Token {
	if p.index < len(p.tokens) {
		return p.tokens[p.index]
	}
	if len(p.tokens) > 0 {
		return p.tokens[len(p.tokens) - 1]
	}
	return tokenizer.Token{kind = .Eof}
}

previous_token :: proc(p: ^Parser) -> tokenizer.Token {
	if p.previous_index >= 0 && p.previous_index < len(p.tokens) {
		return p.tokens[p.previous_index]
	}
	return tokenizer.Token{}
}

bump_token :: proc(p: ^Parser) -> tokenizer.Token {
	tok := current_token(p)
	if p.index < len(p.tokens) && tok.kind != .Eof {
		p.previous_index = p.index
		p.index += 1
	}
	return tok
}

allow_token :: proc(p: ^Parser, kind: tokenizer.Token_Kind) -> bool {
	if current_token(p).kind == kind {
		bump_token(p)
		return true
	}
	return false
}

allow_keyword :: proc(p: ^Parser, keyword: string) -> bool {
	if at_keyword(p, keyword) {
		bump_token(p)
		return true
	}
	return false
}

expect_token :: proc(p: ^Parser, kind: tokenizer.Token_Kind) -> Token {
	tok := current_token(p)
	if tok.kind == kind {
		return bump_token(p)
	}
	error(p, tok.range, "syntax error: expected token")
	return tok
}

expect_keyword :: proc(p: ^Parser, keyword: string) -> Token {
	tok := current_token(p)
	if at_keyword(p, keyword) {
		return bump_token(p)
	}
	error(p, tok.range, "syntax error: expected keyword")
	return tok
}

statement_end :: proc(p: ^Parser, token: Token) -> int {
	if token.kind == .Period {
		return token.range.end
	}
	return previous_token(p).range.end
}

mark_statement_start :: proc(p: ^Parser) -> Stmt_Mark {
	return Stmt_Mark{p.index}
}

build_invalid_statement :: proc(p: ^Parser, mark: Stmt_Mark) -> ^ast.Stmt {
	range, ok := consumed_range(p, mark.index, p.index)
	if !ok {
		range = current_token(p).range
	}
	stmt := ast.new(ast.Invalid_Stmt, range, p.allocator)
	return stmt
}

recover_to_statement_boundary :: proc(p: ^Parser, stop_keywords: []string, consume_period: bool) {
	paren_depth := 0
	bracket_depth := 0
	brace_depth := 0

	for {
		tok := current_token(p)
		if tok.kind == .Eof {
			return
		}
		at_top := paren_depth == 0 && bracket_depth == 0 && brace_depth == 0
		if at_top {
			if at_any_keyword(p, stop_keywords) {
				return
			}
			if tok.kind == .Period {
				if consume_period {
					bump_token(p)
				}
				return
			}
			if .Has_Newline_Before in tok.flags && statement_lead_starts(p, p.index) {
				return
			}
		}

		#partial switch tok.kind {
		case .LParen:
			paren_depth += 1
		case .RParen:
			if paren_depth > 0 {
				paren_depth -= 1
			}
		case .LBracket:
			bracket_depth += 1
		case .RBracket:
			if bracket_depth > 0 {
				bracket_depth -= 1
			}
		case .LBrace:
			brace_depth += 1
		case .RBrace:
			if brace_depth > 0 {
				brace_depth -= 1
			}
		}
		bump_token(p)
	}
}

ensure_forward_progress :: proc(p: ^Parser, start: int) {
	if p.index <= start && !at_eof(p) {
		bump_token(p)
	}
}

consumed_significant_since :: proc(p: ^Parser, mark: Stmt_Mark) -> bool {
	end := min(p.index, len(p.tokens))
	for i in mark.index ..< end {
		if p.tokens[i].kind != .Eof {
			return true
		}
	}
	return false
}

consumed_range :: proc(p: ^Parser, start, end: int) -> (tokenizer.Range, bool) {
	stop := min(end, len(p.tokens))
	found := false
	range := tokenizer.Range{}
	for i in start ..< stop {
		tok := p.tokens[i]
		if tok.kind == .Eof {
			continue
		}
		if !found {
			range.start = tok.range.start
			found = true
		}
		range.end = tok.range.end
	}
	return range, found
}

at_eof :: proc(p: ^Parser) -> bool {
	return current_token(p).kind == .Eof || p.index >= len(p.tokens)
}

at_keyword :: proc(p: ^Parser, keyword: string) -> bool {
	return keyword_phrase_at(p, p.index, keyword)
}

at_keyword_index :: proc(p: ^Parser, index: int, keyword: string) -> bool {
	if index >= len(p.tokens) {
		return false
	}
	return token_is_keyword(p, p.tokens[index], keyword)
}

at_keyword_phrase :: proc(p: ^Parser, keyword: string) -> bool {
	return keyword_phrase_at(p, p.index, keyword)
}

keyword_phrase_at :: proc(p: ^Parser, index: int, keyword: string) -> bool {
	if index >= len(p.tokens) {
		return false
	}
	if keyword == "FIELD-SYMBOLS" {return hyphen2_at(p, index, "FIELD", "SYMBOLS")}
	if keyword == "SELECT-OPTIONS" {return hyphen2_at(p, index, "SELECT", "OPTIONS")}
	if keyword == "CLASS-DATA" {return hyphen2_at(p, index, "CLASS", "DATA")}
	if keyword == "NO-DISPLAY" {return hyphen2_at(p, index, "NO", "DISPLAY")}
	if keyword == "NO-EXTENSION" {return hyphen2_at(p, index, "NO", "EXTENSION")}
	if keyword == "USER-COMMAND" {return hyphen2_at(p, index, "USER", "COMMAND")}
	if keyword == "LINE-SIZE" {return hyphen2_at(p, index, "LINE", "SIZE")}
	if keyword == "LINE-COUNT" {return hyphen2_at(p, index, "LINE", "COUNT")}
	if keyword == "HELP-REQUEST" {return hyphen2_at(p, index, "HELP", "REQUEST")}
	if keyword == "VALUE-REQUEST" {return hyphen2_at(p, index, "VALUE", "REQUEST")}
	if keyword == "OPEN CURSOR" {return space2_at(p, index, "OPEN", "CURSOR")}
	if keyword == "CLOSE CURSOR" {return space2_at(p, index, "CLOSE", "CURSOR")}
	if keyword == "READ TABLE" {return space2_at(p, index, "READ", "TABLE")}
	if keyword == "AS CHECKBOX" {return space2_at(p, index, "AS", "CHECKBOX")}
	if keyword == "LOWER CASE" {return space2_at(p, index, "LOWER", "CASE")}
	if keyword == "VALUE CHECK" {return space2_at(p, index, "VALUE", "CHECK")}
	if keyword == "RADIOBUTTON GROUP" {return space2_at(p, index, "RADIOBUTTON", "GROUP")}
	if keyword == "MODIF ID" {return space2_at(p, index, "MODIF", "ID")}
	if keyword == "MEMORY ID" {return space2_at(p, index, "MEMORY", "ID")}
	if keyword == "MATCHCODE OBJECT" {return space2_at(p, index, "MATCHCODE", "OBJECT")}
	if keyword == "VISIBLE LENGTH" {return space2_at(p, index, "VISIBLE", "LENGTH")}
	if keyword == "NO INTERVALS" {return space2_at(p, index, "NO", "INTERVALS")}
	if keyword ==
	   "NO DATABASE SELECTION" {return space3_at(p, index, "NO", "DATABASE", "SELECTION")}
	if keyword == "AT SELECTION-SCREEN" {
		return(
			at_keyword_index(p, index, "AT") &&
			at_keyword_index(p, index + 1, "SELECTION") &&
			index + 2 < len(p.tokens) &&
			p.tokens[index + 2].kind == .Minus &&
			at_keyword_index(p, index + 3, "SCREEN") \
		)
	}
	if keyword == "LOAD-OF-PROGRAM" {return hyphen3_at(p, index, "LOAD", "OF", "PROGRAM")}
	if keyword == "START-OF-SELECTION" {return hyphen3_at(p, index, "START", "OF", "SELECTION")}
	if keyword == "END-OF-SELECTION" {return hyphen3_at(p, index, "END", "OF", "SELECTION")}
	if keyword == "TOP-OF-PAGE" {return hyphen3_at(p, index, "TOP", "OF", "PAGE")}
	if keyword == "END-OF-PAGE" {return hyphen3_at(p, index, "END", "OF", "PAGE")}
	if keyword == "ENHANCEMENT-SECTION" {return hyphen2_at(p, index, "ENHANCEMENT", "SECTION")}
	if keyword ==
	   "END-ENHANCEMENT-SECTION" {return hyphen3_at(p, index, "END", "ENHANCEMENT", "SECTION")}
	if keyword == "TEST-SEAM" {return hyphen2_at(p, index, "TEST", "SEAM")}
	if keyword == "END-TEST-SEAM" {return hyphen3_at(p, index, "END", "TEST", "SEAM")}
	if keyword == "TEST-INJECTION" {return hyphen2_at(p, index, "TEST", "INJECTION")}
	if keyword == "END-TEST-INJECTION" {return hyphen3_at(p, index, "END", "TEST", "INJECTION")}
	return token_is_keyword(p, p.tokens[index], keyword)
}

space2_at :: proc(p: ^Parser, index: int, a, b: string) -> bool {
	return at_keyword_index(p, index, a) && at_keyword_index(p, index + 1, b)
}

space3_at :: proc(p: ^Parser, index: int, a, b, c: string) -> bool {
	return(
		at_keyword_index(p, index, a) &&
		at_keyword_index(p, index + 1, b) &&
		at_keyword_index(p, index + 2, c) \
	)
}

hyphen2_at :: proc(p: ^Parser, index: int, a, b: string) -> bool {
	return(
		at_keyword_index(p, index, a) &&
		index + 2 < len(p.tokens) &&
		p.tokens[index + 1].kind == .Minus &&
		at_keyword_index(p, index + 2, b) \
	)
}

hyphen3_at :: proc(p: ^Parser, index: int, a, b, c: string) -> bool {
	return(
		at_keyword_index(p, index, a) &&
		index + 4 < len(p.tokens) &&
		p.tokens[index + 1].kind == .Minus &&
		at_keyword_index(p, index + 2, b) &&
		p.tokens[index + 3].kind == .Minus &&
		at_keyword_index(p, index + 4, c) \
	)
}

expect_keyword_phrase :: proc(p: ^Parser, keyword: string) -> Token {
	tok := current_token(p)
	if !at_keyword_phrase(p, keyword) {
		error(p, tok.range, "syntax error: expected keyword")
		return Token{kind = .Eof, range = tok.range}
	}
	count := keyword_phrase_token_count(keyword)
	for _ in 0 ..< count {
		bump_token(p)
	}
	return tok
}

keyword_phrase_token_count :: proc(keyword: string) -> int {
	if keyword == "FIELD-SYMBOLS" ||
	   keyword == "SELECT-OPTIONS" ||
	   keyword == "CLASS-DATA" ||
	   keyword == "ENHANCEMENT-SECTION" ||
	   keyword == "TEST-SEAM" ||
	   keyword == "TEST-INJECTION" {
		return 3
	}
	if keyword == "OPEN CURSOR" ||
	   keyword == "CLOSE CURSOR" ||
	   keyword == "READ TABLE" ||
	   keyword == "AS CHECKBOX" ||
	   keyword == "LOWER CASE" ||
	   keyword == "VALUE CHECK" ||
	   keyword == "RADIOBUTTON GROUP" ||
	   keyword == "MODIF ID" ||
	   keyword == "MEMORY ID" ||
	   keyword == "MATCHCODE OBJECT" ||
	   keyword == "VISIBLE LENGTH" ||
	   keyword == "NO INTERVALS" {
		return 2
	}
	if keyword == "NO-DISPLAY" ||
	   keyword == "NO-EXTENSION" ||
	   keyword == "USER-COMMAND" ||
	   keyword == "LINE-SIZE" ||
	   keyword == "LINE-COUNT" ||
	   keyword == "HELP-REQUEST" ||
	   keyword == "VALUE-REQUEST" {
		return 3
	}
	if keyword == "NO DATABASE SELECTION" {
		return 3
	}
	if keyword == "AT SELECTION-SCREEN" {
		return 4
	}
	if keyword == "LOAD-OF-PROGRAM" ||
	   keyword == "START-OF-SELECTION" ||
	   keyword == "END-OF-SELECTION" ||
	   keyword == "TOP-OF-PAGE" ||
	   keyword == "END-OF-PAGE" ||
	   keyword == "END-ENHANCEMENT-SECTION" ||
	   keyword == "END-TEST-SEAM" ||
	   keyword == "END-TEST-INJECTION" {
		return 5
	}
	return 1
}

token_is_keyword :: proc(p: ^Parser, token: Token, keyword: string) -> bool {
	return(
		token.kind == .Ident &&
		ascii_equal_ignore_case(tokenizer.token_lexeme(token, p.source), keyword) \
	)
}

at_any_keyword :: proc(p: ^Parser, keywords: []string) -> bool {
	for kw in keywords {
		if at_keyword_phrase(p, kw) {
			return true
		}
	}
	return false
}

assignment_starts :: proc(p: ^Parser, index: int) -> bool {
	return assignment_operator_index(p, index) >= 0
}

assignment_operator_index :: proc(p: ^Parser, index: int) -> int {
	if index >= len(p.tokens) || !expr_lead_token(p.tokens[index]) {
		return -1
	}
	if known_stmt_lead_at(p, index) {
		return -1
	}
	paren_depth := 0
	bracket_depth := 0
	brace_depth := 0
	for i in index ..< len(p.tokens) {
		tok := p.tokens[i]
		if tok.kind == .Eof || tok.kind == .Period {
			return -1
		}
		if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 {
			if tok.kind == .Eq || tok.kind == .QuestionEq {
				return i
			}
		}
		#partial switch tok.kind {
		case .LParen:
			paren_depth += 1
		case .RParen:
			if paren_depth > 0 {
				paren_depth -= 1
			}
		case .LBracket:
			bracket_depth += 1
		case .RBracket:
			if bracket_depth > 0 {
				bracket_depth -= 1
			}
		case .LBrace:
			brace_depth += 1
		case .RBrace:
			if brace_depth > 0 {
				brace_depth -= 1
			}
		}
	}
	return -1
}

expr_lead_token :: proc(tok: Token) -> bool {
	#partial switch tok.kind {
	case .Ident, .Number, .String, .StringTemplate, .Hash, .LParen, .Plus, .Minus:
		return true
	}
	return false
}

statement_lead_starts :: proc(p: ^Parser, index: int) -> bool {
	if index >= len(p.tokens) {
		return false
	}
	tok := p.tokens[index]
	if tok.kind == .StringTemplate {
		return true
	}
	return known_stmt_lead_at(p, index) || assignment_starts(p, index)
}

known_stmt_lead_at :: proc(p: ^Parser, index: int) -> bool {
	return(
		keyword_phrase_at(p, index, "DATA") ||
		keyword_phrase_at(p, index, "TYPES") ||
		keyword_phrase_at(p, index, "CONSTANTS") ||
		keyword_phrase_at(p, index, "FIELD-SYMBOLS") ||
		keyword_phrase_at(p, index, "STATICS") ||
		keyword_phrase_at(p, index, "TABLES") ||
		keyword_phrase_at(p, index, "RANGES") ||
		keyword_phrase_at(p, index, "PARAMETERS") ||
		keyword_phrase_at(p, index, "PARAMETER") ||
		keyword_phrase_at(p, index, "SELECT-OPTIONS") ||
		keyword_phrase_at(p, index, "CONTROLS") ||
		keyword_phrase_at(p, index, "CLASS-DATA") ||
		keyword_phrase_at(p, index, "IF") ||
		keyword_phrase_at(p, index, "ELSEIF") ||
		keyword_phrase_at(p, index, "ELSE") ||
		keyword_phrase_at(p, index, "ENDIF") ||
		keyword_phrase_at(p, index, "CASE") ||
		keyword_phrase_at(p, index, "WHEN") ||
		keyword_phrase_at(p, index, "ENDCASE") ||
		keyword_phrase_at(p, index, "WHILE") ||
		keyword_phrase_at(p, index, "ENDWHILE") ||
		keyword_phrase_at(p, index, "DO") ||
		keyword_phrase_at(p, index, "ENDDO") ||
		keyword_phrase_at(p, index, "LOOP") ||
		keyword_phrase_at(p, index, "ENDLOOP") ||
		keyword_phrase_at(p, index, "AT") ||
		keyword_phrase_at(p, index, "ENDAT") ||
		keyword_phrase_at(p, index, "TRY") ||
		keyword_phrase_at(p, index, "CATCH") ||
		keyword_phrase_at(p, index, "CLEANUP") ||
		keyword_phrase_at(p, index, "ENDTRY") ||
		keyword_phrase_at(p, index, "CLASS") ||
		keyword_phrase_at(p, index, "INTERFACE") ||
		keyword_phrase_at(p, index, "METHOD") ||
		keyword_phrase_at(p, index, "FORM") ||
		keyword_phrase_at(p, index, "FUNCTION") ||
		keyword_phrase_at(p, index, "MODULE") ||
		keyword_phrase_at(p, index, "REPORT") ||
		keyword_phrase_at(p, index, "PROGRAM") ||
		keyword_phrase_at(p, index, "SELECT") ||
		keyword_phrase_at(p, index, "WITH") ||
		keyword_phrase_at(p, index, "OPEN CURSOR") ||
		keyword_phrase_at(p, index, "FETCH") ||
		keyword_phrase_at(p, index, "CLOSE CURSOR") ||
		keyword_phrase_at(p, index, "READ TABLE") ||
		keyword_phrase_at(p, index, "INSERT") ||
		keyword_phrase_at(p, index, "UPDATE") ||
		keyword_phrase_at(p, index, "DELETE") ||
		keyword_phrase_at(p, index, "CLEAR") ||
		keyword_phrase_at(p, index, "REFRESH") ||
		keyword_phrase_at(p, index, "FREE") ||
		keyword_phrase_at(p, index, "UNASSIGN") ||
		keyword_phrase_at(p, index, "MOVE") ||
		keyword_phrase_at(p, index, "ADD") ||
		keyword_phrase_at(p, index, "SUBTRACT") ||
		keyword_phrase_at(p, index, "MULTIPLY") ||
		keyword_phrase_at(p, index, "DIVIDE") ||
		keyword_phrase_at(p, index, "COMPUTE") ||
		keyword_phrase_at(p, index, "CONCATENATE") ||
		keyword_phrase_at(p, index, "SPLIT") ||
		keyword_phrase_at(p, index, "CONDENSE") ||
		keyword_phrase_at(p, index, "REPLACE") ||
		keyword_phrase_at(p, index, "TRANSLATE") ||
		keyword_phrase_at(p, index, "SHIFT") ||
		keyword_phrase_at(p, index, "FIND") ||
		keyword_phrase_at(p, index, "SEARCH") ||
		keyword_phrase_at(p, index, "PERFORM") ||
		keyword_phrase_at(p, index, "CALL") ||
		keyword_phrase_at(p, index, "SUBMIT") ||
		keyword_phrase_at(p, index, "MESSAGE") ||
		keyword_phrase_at(p, index, "WRITE") \
	)
}

next_significant_index :: proc(p: ^Parser, index: int) -> int {
	return index
}

error :: proc(p: ^Parser, range: Range, message: string) {
	append(&p.errors, Parse_Error{message, range})
}

error_current :: proc(p: ^Parser, message: string) {
	error(p, current_token(p).range, message)
}

ascii_equal_ignore_case :: proc(a, b: string) -> bool {
	if len(a) != len(b) {
		return false
	}
	for i in 0 ..< len(a) {
		if ascii_upper(a[i]) != ascii_upper(b[i]) {
			return false
		}
	}
	return true
}

ascii_upper :: proc(b: byte) -> byte {
	if 'a' <= b && b <= 'z' {
		return b - ('a' - 'A')
	}
	return b
}
