package abap_frontend_parser

import "../ast"
import "../tokenizer"

import "core:mem"
import "core:strings"

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

Parse_Diagnostic_Policy :: enum {
	Strict,
	Include_Fragment,
}

Parser :: struct {
	source:         string,
	path:           string,
	tokens:         []tokenizer.Token,
	trivia:         []tokenizer.Trivia_Piece,
	lex_errors:     []tokenizer.Lex_Error,
	index:          int,
	previous_index: int,
	expr_stop_keywords: []string,
	expr_extra_stop_keywords: []string,
	open_sql_expr: bool,
	errors:         [dynamic]Parse_Error,
	allocator:      mem.Allocator,
	root:           ^ast.File,
}

Stmt_Mark :: struct {
	index: int,
}

parse :: proc(source: string, path: string, allocator: mem.Allocator) -> Parsed_File {
	return parse_with_diagnostic_policy(source, path, allocator, .Strict)
}

parse_with_diagnostic_policy :: proc(
	source: string,
	path: string,
	allocator: mem.Allocator,
	policy: Parse_Diagnostic_Policy,
) -> Parsed_File {
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
	if policy == .Include_Fragment {
		filtered := make([dynamic]Parse_Error, 0, len(parser.errors), allocator)
		for e in parser.errors {
			if !parse_error_is_include_fragment_boundary(e) {
				append(&filtered, e)
			}
		}
		parser.errors = filtered
	}

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
		if at_eof(p) ||
		   at_any_keyword(p, stop_keywords) ||
		   at_outer_boundary_for_stops(p, stop_keywords) {
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
		attach_stmt_trivia(p, stmt, mark)
		return stmt
	}

	if !consumed_significant_since(p, mark) {
		if !at_eof(p) && !at_any_keyword(p, stop_keywords) {
			bump_token(p)
		}
		stmt = build_invalid_statement(p, mark)
		attach_stmt_trivia(p, stmt, mark)
		return stmt
	}
	recover_to_statement_boundary(p, stop_keywords, true)
	stmt = build_invalid_statement(p, mark)
	attach_stmt_trivia(p, stmt, mark)
	return stmt
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
	if program_include_stmt_starts(p) {
		return parse_include_stmt(p)
	}
	if at_keyword(p, "INSERT") && at_keyword_index(p, p.index + 1, "DUMMY") {
		return parse_simple_stmt(p)
	}
	if assignment_starts(p, p.index) {
		return parse_assign_stmt(p)
	}
	if direct_call_stmt_starts(p) {
		return parse_direct_call_stmt(p)
	}
	if data_access_stmt_starts(p) {
		return parse_data_access_stmt(p)
	}
	if stmt := parse_stray_block_boundary_stmt(p); stmt != nil {
		return stmt
	}
	if simple_stmt_starts(p) {
		return parse_simple_stmt(p)
	}
	if macro_call_stmt_starts(p) {
		return parse_macro_call_stmt(p)
	}
	error_current(p, "syntax error: unexpected token")
	return nil
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

previous_stmt_end :: proc(stmts: [dynamic]^ast.Stmt, fallback: int) -> int {
	if len(stmts) == 0 {
		return fallback
	}
	return stmts[len(stmts) - 1].range.end
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
		bump_token(p)
	}
}

consume_raw_until_top_level_period :: proc(p: ^Parser) {
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

qualified_ident_name_at :: proc(p: ^Parser, index: int) -> (string, int, bool) {
	name, _, _, _, _, _, next, ok := qualified_ident_parts_at(p, index)
	return name, next, ok
}

qualified_ident_parts_at :: proc(
	p: ^Parser,
	index: int,
) -> (
	name: string,
	name_range: Range,
	qualifier: string,
	qualifier_range: Range,
	member_name: string,
	member_range: Range,
	next_index: int,
	ok: bool,
) {
	if index < 0 || index >= len(p.tokens) {
		return "", {}, "", {}, "", {}, index, false
	}
	first := p.tokens[index]
	if first.kind != .Ident {
		return "", {}, "", {}, "", {}, index, false
	}
	if index + 2 < len(p.tokens) &&
	   p.tokens[index + 1].kind == .Tilde &&
	   p.tokens[index + 2].kind == .Ident {
		member := p.tokens[index + 2]
		out := strings.builder_make(p.allocator)
		strings.write_string(&out, tokenizer.token_lexeme(first, p.source))
		strings.write_byte(&out, '~')
		strings.write_string(&out, tokenizer.token_lexeme(member, p.source))
		return strings.to_string(out),
			tokenizer.text_range(first.range.start, member.range.end),
			tokenizer.token_lexeme(first, p.source),
			first.range,
			tokenizer.token_lexeme(member, p.source),
			member.range,
			index + 3,
			true
	}
	text := tokenizer.token_lexeme(first, p.source)
	return text, first.range, "", {}, text, first.range, index + 1, true
}

first_qualified_name_until_period :: proc(p: ^Parser) -> string {
	name, _, _, _, _, _, _ := first_qualified_name_parts_until_period(p)
	return name
}

first_qualified_name_parts_until_period :: proc(
	p: ^Parser,
) -> (
	name: string,
	name_range: Range,
	qualifier: string,
	qualifier_range: Range,
	member_name: string,
	member_range: Range,
	ok: bool,
) {
	for i in p.index ..< len(p.tokens) {
		tok := p.tokens[i]
		if tok.kind == .Period || tok.kind == .Eof {
			break
		}
		if tok.kind == .Ident {
			part_name, part_range, part_qualifier, part_qualifier_range, part_member, part_member_range, _, part_ok := qualified_ident_parts_at(p, i)
			return part_name, part_range, part_qualifier, part_qualifier_range, part_member, part_member_range, part_ok
		}
		if tok.kind == .String || tok.kind == .Number {
			text := tokenizer.token_lexeme(tok, p.source)
			return text, tok.range, "", {}, text, tok.range, true
		}
	}
	return "", {}, "", {}, "", {}, false
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
	return expect_token_message(p, kind, "syntax error: expected token")
}

expect_token_message :: proc(p: ^Parser, kind: tokenizer.Token_Kind, message: string) -> Token {
	tok := current_token(p)
	if tok.kind == kind {
		return bump_token(p)
	}
	error(p, tok.range, message)
	return tok
}

expect_keyword :: proc(p: ^Parser, keyword: string) -> Token {
	return expect_keyword_message(p, keyword, "syntax error: expected keyword")
}

expect_keyword_message :: proc(p: ^Parser, keyword: string, message: string) -> Token {
	tok := current_token(p)
	if at_keyword(p, keyword) {
		return bump_token(p)
	}
	error(p, tok.range, message)
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

attach_stmt_trivia :: proc(p: ^Parser, stmt: ^ast.Stmt, mark: Stmt_Mark) {
	if stmt == nil || mark.index < 0 || mark.index >= len(p.tokens) {
		return
	}
	first := p.tokens[mark.index]
	for piece in p.trivia[first.leading_trivia.start:first.leading_trivia.end] {
		if piece.kind == .Comment || piece.kind == .Pragma {
			if cap(stmt.leading_comments) == 0 {
				stmt.leading_comments = make([dynamic]string, 0, 1, p.allocator)
			}
			append(&stmt.leading_comments, strings.clone(p.source[piece.range.start:piece.range.end], p.allocator))
		}
	}
	last := previous_token(p)
	for piece in p.trivia[last.trailing_trivia.start:last.trailing_trivia.end] {
		if piece.kind == .Comment || piece.kind == .Pragma {
			stmt.trailing_comment = strings.clone(p.source[piece.range.start:piece.range.end], p.allocator)
			return
		}
	}
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

stmt_period_before_boundary :: proc(p: ^Parser, start: int) -> bool {
	paren := 0
	bracket := 0
	brace := 0
	for i in start ..< len(p.tokens) {
		tok := p.tokens[i]
		if tok.kind == .Eof {
			return false
		}
		top := paren == 0 && bracket == 0 && brace == 0
		if top {
			if tok.kind == .Period {
				return true
			}
			if i > start &&
			   .Has_Newline_Before in tok.flags &&
			   statement_lead_starts(p, i) &&
			   !line_continuation_starts(p, i) {
				return false
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
	}
	return false
}

line_continuation_starts :: proc(p: ^Parser, index: int) -> bool {
	if index < len(p.tokens) && p.tokens[index].kind == .StringTemplate {
		return true
	}
	return(
		keyword_phrase_at(p, index, "WITH") ||
		keyword_phrase_at(p, index, "OF") ||
		keyword_phrase_at(p, index, "IN") ||
		keyword_phrase_at(p, index, "INTO") ||
		keyword_phrase_at(p, index, "FROM") ||
		keyword_phrase_at(p, index, "TO") ||
		keyword_phrase_at(p, index, "ASSIGNING") ||
		keyword_phrase_at(p, index, "REFERENCE") ||
		keyword_phrase_at(p, index, "TABLE") ||
		keyword_phrase_at(p, index, "KEY") ||
		keyword_phrase_at(p, index, "METHOD") ||
		keyword_phrase_at(p, index, "WHERE") ||
		keyword_phrase_at(p, index, "SET") ||
		keyword_phrase_at(p, index, "STATE") ||
		keyword_phrase_at(p, index, "PROGRAM") ||
		keyword_phrase_at(p, index, "EXTENSION") ||
		keyword_phrase_at(p, index, "USING") ||
		keyword_phrase_at(p, index, "FOR") ||
		keyword_phrase_at(p, index, "AND") ||
		keyword_phrase_at(p, index, "OR") ||
		keyword_phrase_at(p, index, "EXPORTING") ||
		keyword_phrase_at(p, index, "IMPORTING") ||
		keyword_phrase_at(p, index, "CHANGING") ||
		keyword_phrase_at(p, index, "TABLES") ||
		keyword_phrase_at(p, index, "EXCEPTIONS") \
	)
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

keyword_phrase_at :: proc{keyword_phrase_at_static, keyword_phrase_at_dynamic}

keyword_phrase_at_static :: #force_inline proc(p: ^Parser, index: int, $keyword: string) -> bool {
	if index >= len(p.tokens) {
		return false
	}
	when keyword == "FIELD-SYMBOLS" {
		return hyphen2_at(p, index, "FIELD", "SYMBOLS")
	}
	when keyword == "SELECT-OPTIONS" {
		return hyphen2_at(p, index, "SELECT", "OPTIONS")
	}
	when keyword == "SELECTION-SCREEN" {
		return hyphen2_at(p, index, "SELECTION", "SCREEN")
	}
	when keyword == "CLASS-DATA" {
		return hyphen2_at(p, index, "CLASS", "DATA")
	}
	when keyword == "TYPE-POOLS" {
		return hyphen2_at(p, index, "TYPE", "POOLS")
	}
	when keyword == "FUNCTION-POOL" {
		return hyphen2_at(p, index, "FUNCTION", "POOL")
	}
	when keyword == "AUTHORITY-CHECK" {
		return hyphen2_at(p, index, "AUTHORITY", "CHECK")
	}
	when keyword == "FIELD-GROUPS" {
		return hyphen2_at(p, index, "FIELD", "GROUPS")
	}
	when keyword == "LOG-POINT" {
		return hyphen2_at(p, index, "LOG", "POINT")
	}
	when keyword == "CLASS-METHODS" {
		return hyphen2_at(p, index, "CLASS", "METHODS")
	}
	when keyword == "CLASS-EVENTS" {
		return hyphen2_at(p, index, "CLASS", "EVENTS")
	}
	when keyword == "NEW-LINE" {
		return hyphen2_at(p, index, "NEW", "LINE")
	}
	when keyword == "NEW-PAGE" {
		return hyphen2_at(p, index, "NEW", "PAGE")
	}
	when keyword == "MESSAGE-ID" {
		return hyphen2_at(p, index, "MESSAGE", "ID")
	}
	when keyword == "READ-ONLY" {
		return hyphen2_at(p, index, "READ", "ONLY")
	}
	when keyword == "BIT-AND" {
		return hyphen2_at(p, index, "BIT", "AND")
	}
	when keyword == "BIT-OR" {
		return hyphen2_at(p, index, "BIT", "OR")
	}
	when keyword == "BIT-XOR" {
		return hyphen2_at(p, index, "BIT", "XOR")
	}
	when keyword == "NON-UNIQUE" {
		return hyphen2_at(p, index, "NON", "UNIQUE")
	}
	when keyword == "NO-DISPLAY" {
		return hyphen2_at(p, index, "NO", "DISPLAY")
	}
	when keyword == "NO-EXTENSION" {
		return hyphen2_at(p, index, "NO", "EXTENSION")
	}
	when keyword == "USER-COMMAND" {
		return hyphen2_at(p, index, "USER", "COMMAND")
	}
	when keyword == "LINE-SIZE" {
		return hyphen2_at(p, index, "LINE", "SIZE")
	}
	when keyword == "LINE-COUNT" {
		return hyphen2_at(p, index, "LINE", "COUNT")
	}
	when keyword == "HELP-REQUEST" {
		return hyphen2_at(p, index, "HELP", "REQUEST")
	}
	when keyword == "VALUE-REQUEST" {
		return hyphen2_at(p, index, "VALUE", "REQUEST")
	}
	when keyword == "OPEN CURSOR" {
		return space2_at(p, index, "OPEN", "CURSOR")
	}
	when keyword == "CLOSE CURSOR" {
		return space2_at(p, index, "CLOSE", "CURSOR")
	}
	when keyword == "READ TABLE" {
		return space2_at(p, index, "READ", "TABLE")
	}
	when keyword == "EXEC SQL" {
		return space2_at(p, index, "EXEC", "SQL")
	}
	when keyword == "AS CHECKBOX" {
		return space2_at(p, index, "AS", "CHECKBOX")
	}
	when keyword == "LOWER CASE" {
		return space2_at(p, index, "LOWER", "CASE")
	}
	when keyword == "VALUE CHECK" {
		return space2_at(p, index, "VALUE", "CHECK")
	}
	when keyword == "RADIOBUTTON GROUP" {
		return space2_at(p, index, "RADIOBUTTON", "GROUP")
	}
	when keyword == "MODIF ID" {
		return space2_at(p, index, "MODIF", "ID")
	}
	when keyword == "MEMORY ID" {
		return space2_at(p, index, "MEMORY", "ID")
	}
	when keyword == "MATCHCODE OBJECT" {
		return space2_at(p, index, "MATCHCODE", "OBJECT")
	}
	when keyword == "VISIBLE LENGTH" {
		return space2_at(p, index, "VISIBLE", "LENGTH")
	}
	when keyword == "NO INTERVALS" {
		return space2_at(p, index, "NO", "INTERVALS")
	}
	when keyword == "NO DATABASE SELECTION" {
		return space3_at(p, index, "NO", "DATABASE", "SELECTION")
	}
	when keyword == "AT SELECTION-SCREEN" {
		return(
			at_keyword_index(p, index, "AT") &&
			at_keyword_index(p, index + 1, "SELECTION") &&
			index + 2 < len(p.tokens) &&
			p.tokens[index + 2].kind == .Minus &&
			at_keyword_index(p, index + 3, "SCREEN") \
		)
	}
	when keyword == "LOAD-OF-PROGRAM" {
		return hyphen3_at(p, index, "LOAD", "OF", "PROGRAM")
	}
	when keyword == "START-OF-SELECTION" {
		return hyphen3_at(p, index, "START", "OF", "SELECTION")
	}
	when keyword == "END-OF-SELECTION" {
		return hyphen3_at(p, index, "END", "OF", "SELECTION")
	}
	when keyword == "TOP-OF-PAGE" {
		return hyphen3_at(p, index, "TOP", "OF", "PAGE")
	}
	when keyword == "END-OF-PAGE" {
		return hyphen3_at(p, index, "END", "OF", "PAGE")
	}
	when keyword == "END-OF-DEFINITION" {
		return hyphen3_at(p, index, "END", "OF", "DEFINITION")
	}
	when keyword == "ENHANCEMENT-SECTION" {
		return hyphen2_at(p, index, "ENHANCEMENT", "SECTION")
	}
	when keyword == "END-ENHANCEMENT-SECTION" {
		return hyphen3_at(p, index, "END", "ENHANCEMENT", "SECTION")
	}
	when keyword == "TEST-SEAM" {
		return hyphen2_at(p, index, "TEST", "SEAM")
	}
	when keyword == "END-TEST-SEAM" {
		return hyphen3_at(p, index, "END", "TEST", "SEAM")
	}
	when keyword == "TEST-INJECTION" {
		return hyphen2_at(p, index, "TEST", "INJECTION")
	}
	when keyword == "END-TEST-INJECTION" {
		return hyphen3_at(p, index, "END", "TEST", "INJECTION")
	}
	return token_is_keyword(p, p.tokens[index], keyword)
}

keyword_phrase_at_dynamic :: proc(p: ^Parser, index: int, keyword: string) -> bool {
	if index >= len(p.tokens) {
		return false
	}
	if keyword_phrase_is_simple(keyword) {
		return token_is_keyword(p, p.tokens[index], keyword)
	}
	if keyword == "FIELD-SYMBOLS" {
		return hyphen2_at(p, index, "FIELD", "SYMBOLS")
	}
	if keyword == "SELECT-OPTIONS" {
		return hyphen2_at(p, index, "SELECT", "OPTIONS")
	}
	if keyword == "SELECTION-SCREEN" {
		return hyphen2_at(p, index, "SELECTION", "SCREEN")
	}
	if keyword == "CLASS-DATA" {
		return hyphen2_at(p, index, "CLASS", "DATA")
	}
	if keyword == "TYPE-POOLS" {
		return hyphen2_at(p, index, "TYPE", "POOLS")
	}
	if keyword == "FUNCTION-POOL" {
		return hyphen2_at(p, index, "FUNCTION", "POOL")
	}
	if keyword == "AUTHORITY-CHECK" {
		return hyphen2_at(p, index, "AUTHORITY", "CHECK")
	}
	if keyword == "FIELD-GROUPS" {
		return hyphen2_at(p, index, "FIELD", "GROUPS")
	}
	if keyword == "LOG-POINT" {
		return hyphen2_at(p, index, "LOG", "POINT")
	}
	if keyword == "CLASS-METHODS" {
		return hyphen2_at(p, index, "CLASS", "METHODS")
	}
	if keyword == "CLASS-EVENTS" {
		return hyphen2_at(p, index, "CLASS", "EVENTS")
	}
	if keyword == "NEW-LINE" {
		return hyphen2_at(p, index, "NEW", "LINE")
	}
	if keyword == "NEW-PAGE" {
		return hyphen2_at(p, index, "NEW", "PAGE")
	}
	if keyword == "MESSAGE-ID" {
		return hyphen2_at(p, index, "MESSAGE", "ID")
	}
	if keyword == "READ-ONLY" {
		return hyphen2_at(p, index, "READ", "ONLY")
	}
	if keyword == "BIT-AND" {
		return hyphen2_at(p, index, "BIT", "AND")
	}
	if keyword == "BIT-OR" {
		return hyphen2_at(p, index, "BIT", "OR")
	}
	if keyword == "BIT-XOR" {
		return hyphen2_at(p, index, "BIT", "XOR")
	}
	if keyword == "NON-UNIQUE" {
		return hyphen2_at(p, index, "NON", "UNIQUE")
	}
	if keyword == "NO-DISPLAY" {
		return hyphen2_at(p, index, "NO", "DISPLAY")
	}
	if keyword == "NO-EXTENSION" {
		return hyphen2_at(p, index, "NO", "EXTENSION")
	}
	if keyword == "USER-COMMAND" {
		return hyphen2_at(p, index, "USER", "COMMAND")
	}
	if keyword == "LINE-SIZE" {
		return hyphen2_at(p, index, "LINE", "SIZE")
	}
	if keyword == "LINE-COUNT" {
		return hyphen2_at(p, index, "LINE", "COUNT")
	}
	if keyword == "HELP-REQUEST" {
		return hyphen2_at(p, index, "HELP", "REQUEST")
	}
	if keyword == "VALUE-REQUEST" {
		return hyphen2_at(p, index, "VALUE", "REQUEST")
	}
	if keyword == "OPEN CURSOR" {
		return space2_at(p, index, "OPEN", "CURSOR")
	}
	if keyword == "CLOSE CURSOR" {
		return space2_at(p, index, "CLOSE", "CURSOR")
	}
	if keyword == "READ TABLE" {
		return space2_at(p, index, "READ", "TABLE")
	}
	if keyword == "EXEC SQL" {
		return space2_at(p, index, "EXEC", "SQL")
	}
	if keyword == "AS CHECKBOX" {
		return space2_at(p, index, "AS", "CHECKBOX")
	}
	if keyword == "LOWER CASE" {
		return space2_at(p, index, "LOWER", "CASE")
	}
	if keyword == "VALUE CHECK" {
		return space2_at(p, index, "VALUE", "CHECK")
	}
	if keyword == "RADIOBUTTON GROUP" {
		return space2_at(p, index, "RADIOBUTTON", "GROUP")
	}
	if keyword == "MODIF ID" {
		return space2_at(p, index, "MODIF", "ID")
	}
	if keyword == "MEMORY ID" {
		return space2_at(p, index, "MEMORY", "ID")
	}
	if keyword == "MATCHCODE OBJECT" {
		return space2_at(p, index, "MATCHCODE", "OBJECT")
	}
	if keyword == "VISIBLE LENGTH" {
		return space2_at(p, index, "VISIBLE", "LENGTH")
	}
	if keyword == "NO INTERVALS" {
		return space2_at(p, index, "NO", "INTERVALS")
	}
	if keyword == "NO DATABASE SELECTION" {
		return space3_at(p, index, "NO", "DATABASE", "SELECTION")
	}
	if keyword == "AT SELECTION-SCREEN" {
		return(
			at_keyword_index(p, index, "AT") &&
			at_keyword_index(p, index + 1, "SELECTION") &&
			index + 2 < len(p.tokens) &&
			p.tokens[index + 2].kind == .Minus &&
			at_keyword_index(p, index + 3, "SCREEN") \
		)
	}
	if keyword == "LOAD-OF-PROGRAM" {
		return hyphen3_at(p, index, "LOAD", "OF", "PROGRAM")
	}
	if keyword == "START-OF-SELECTION" {
		return hyphen3_at(p, index, "START", "OF", "SELECTION")
	}
	if keyword == "END-OF-SELECTION" {
		return hyphen3_at(p, index, "END", "OF", "SELECTION")
	}
	if keyword == "TOP-OF-PAGE" {
		return hyphen3_at(p, index, "TOP", "OF", "PAGE")
	}
	if keyword == "END-OF-PAGE" {
		return hyphen3_at(p, index, "END", "OF", "PAGE")
	}
	if keyword == "END-OF-DEFINITION" {
		return hyphen3_at(p, index, "END", "OF", "DEFINITION")
	}
	if keyword == "ENHANCEMENT-SECTION" {
		return hyphen2_at(p, index, "ENHANCEMENT", "SECTION")
	}
	if keyword == "END-ENHANCEMENT-SECTION" {
		return hyphen3_at(p, index, "END", "ENHANCEMENT", "SECTION")
	}
	if keyword == "TEST-SEAM" {
		return hyphen2_at(p, index, "TEST", "SEAM")
	}
	if keyword == "END-TEST-SEAM" {
		return hyphen3_at(p, index, "END", "TEST", "SEAM")
	}
	if keyword == "TEST-INJECTION" {
		return hyphen2_at(p, index, "TEST", "INJECTION")
	}
	if keyword == "END-TEST-INJECTION" {
		return hyphen3_at(p, index, "END", "TEST", "INJECTION")
	}
	return token_is_keyword(p, p.tokens[index], keyword)
}

keyword_phrase_is_simple :: #force_inline proc(keyword: string) -> bool {
	for i in 0 ..< len(keyword) {
		if keyword[i] == '-' || keyword[i] == ' ' {
			return false
		}
	}
	return true
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
	if keyword_phrase_is_simple(keyword) {
		return 1
	}
	if keyword == "FIELD-SYMBOLS" ||
	   keyword == "SELECT-OPTIONS" ||
	   keyword == "SELECTION-SCREEN" ||
	   keyword == "CLASS-DATA" ||
	   keyword == "TYPE-POOLS" ||
	   keyword == "FUNCTION-POOL" ||
	   keyword == "AUTHORITY-CHECK" ||
	   keyword == "FIELD-GROUPS" ||
	   keyword == "LOG-POINT" ||
	   keyword == "CLASS-METHODS" ||
	   keyword == "CLASS-EVENTS" ||
	   keyword == "NEW-LINE" ||
	   keyword == "NEW-PAGE" ||
	   keyword == "MESSAGE-ID" ||
	   keyword == "READ-ONLY" ||
	   keyword == "BIT-AND" ||
	   keyword == "BIT-OR" ||
	   keyword == "BIT-XOR" ||
	   keyword == "NON-UNIQUE" ||
	   keyword == "ENHANCEMENT-SECTION" ||
	   keyword == "TEST-SEAM" ||
	   keyword == "TEST-INJECTION" {
		return 3
	}
	if keyword == "OPEN CURSOR" ||
	   keyword == "CLOSE CURSOR" ||
	   keyword == "READ TABLE" ||
	   keyword == "EXEC SQL" ||
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
	   keyword == "END-OF-DEFINITION" ||
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
		strings.equal_fold(tokenizer.token_lexeme(token, p.source), keyword) \
	)
}

type_ref_expr_from_tokens :: proc(
	p: ^Parser,
	start, end: int,
	name_end := -1,
	set_name := true,
	fill_parts := true,
) -> ^ast.Type_Ref_Expr {
	if end <= start {
		return nil
	}
	first := p.tokens[start]
	last := p.tokens[end - 1]
	expr := ast.new(ast.Type_Ref_Expr, tokenizer.text_range(first.range.start, last.range.end), p.allocator)
	expr.is_ref = type_ref_starts_with_ref_to(p, start, end)
	expr.text = strings.clone(p.source[expr.range.start:expr.range.end], p.allocator)
	path_end := last.range.end
	if name_end >= 0 {
		path_end = name_end
	}
	if set_name {
		expr.name = strings.clone(p.source[first.range.start:path_end], p.allocator)
	}
	if fill_parts {
		type_ref_fill_base_path(p, expr, start, end, path_end)
	}
	return expr
}

type_ref_fill_base_path :: proc(
	p: ^Parser,
	expr: ^ast.Type_Ref_Expr,
	start, end: int,
	path_end: int,
) {
	paren, bracket, brace := 0, 0, 0
	base_end := path_end
	found_selector := false
	path_ready := false
	path_start := start
	if expr.is_ref {
		path_start = start + 2
	}
	for i := path_start; i < end && p.tokens[i].range.start < path_end; i += 1 {
		tok := p.tokens[i]
		top := paren == 0 && bracket == 0 && brace == 0
		if top && type_ref_selector_token(tok.kind) {
			if !found_selector {
				base_end = tok.range.start
				if i > path_start {
					base_end = p.tokens[i - 1].range.end
				}
				found_selector = true
			}
			next := i + 1
			if next < end &&
			   p.tokens[next].range.start < path_end &&
			   type_ref_path_token(p.tokens[next]) {
				if !path_ready {
					expr.path = make([dynamic]ast.Type_Ref_Path_Segment, 0, 2, p.allocator)
					path_ready = true
				}
				field := p.tokens[next]
				append(
					&expr.path,
					ast.Type_Ref_Path_Segment {
						name = tokenizer.token_lexeme(field, p.source),
						range = field.range,
						selector = selector_op(tok.kind),
						selector_range = tok.range,
					},
				)
				i = next
				continue
			}
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
	if path_start < end && p.tokens[path_start].range.start < base_end {
		first := p.tokens[path_start]
		expr.base_range = tokenizer.text_range(first.range.start, base_end)
		expr.base_name = strings.clone(p.source[expr.base_range.start:expr.base_range.end], p.allocator)
	}
}

type_ref_starts_with_ref_to :: proc(p: ^Parser, start, end: int) -> bool {
	return start + 2 < end &&
	       at_keyword_index(p, start, "REF") &&
	       at_keyword_index(p, start + 1, "TO")
}

type_ref_selector_token :: #force_inline proc(kind: tokenizer.Token_Kind) -> bool {
	return kind == .Minus || kind == .Arrow || kind == .FatArrow || kind == .Tilde
}

type_ref_path_token :: #force_inline proc(tok: Token) -> bool {
	return tok.kind == .Ident || tok.kind == .Number || tok.kind == .Star
}

type_ref_key_clause_starts :: proc(p: ^Parser, index: int) -> bool {
	return at_keyword_index(p, index, "WITH") &&
	       (at_keyword_index(p, index + 1, "DEFAULT") ||
	        at_keyword_index(p, index + 1, "EMPTY") ||
	        at_keyword_index(p, index + 1, "UNIQUE") ||
	        keyword_phrase_at(p, index + 1, "NON-UNIQUE") ||
	        at_keyword_index(p, index + 1, "KEY"))
}

at_any_keyword :: proc(p: ^Parser, keywords: []string) -> bool {
	for kw in keywords {
		if at_keyword_phrase(p, kw) && !keyword_is_compact_call(p, kw) {
			return true
		}
	}
	return false
}

keyword_is_compact_call :: proc(p: ^Parser, keyword: string) -> bool {
	return(
		(keyword == "CLEANUP" || keyword == "SET" || keyword == "SORT" || keyword == "UPDATE") &&
		next_token_kind(p, 1) == .LParen &&
		tokens_touch(current_token(p), p.tokens[p.index + 1]) \
	)
}

at_outer_boundary_for_stops :: proc(p: ^Parser, stop_keywords: []string) -> bool {
	if len(stop_keywords) == 0 {
		return false
	}
	if stop_keywords_include_any(stop_keywords, CONTROL_BODY_STOP_MARKERS) {
		return at_any_keyword(p, CONTROL_OUTER_BOUNDARIES)
	}
	if stop_keywords_include_any(stop_keywords, STRUCTURAL_BODY_STOP_MARKERS) {
		return at_any_keyword(p, STRUCTURAL_OUTER_BOUNDARIES)
	}
	return false
}

stop_keywords_include_any :: proc(stop_keywords, markers: []string) -> bool {
	for kw in stop_keywords {
		for marker in markers {
			if kw == marker {
				return true
			}
		}
	}
	return false
}

CONTROL_BODY_STOP_MARKERS :: []string {
	"ENDIF",
	"ENDCASE",
	"ENDWHILE",
	"ENDDO",
	"ENDLOOP",
	"ENDAT",
	"ENDTRY",
	"ENDSELECT",
	"ENDCATCH",
}

CONTROL_OUTER_BOUNDARIES :: []string {
	"ELSEIF",
	"ELSE",
	"ENDIF",
	"WHEN",
	"ENDCASE",
	"ENDWHILE",
	"ENDDO",
	"ENDLOOP",
	"ENDAT",
	"CATCH",
	"CLEANUP",
	"ENDTRY",
	"ENDSELECT",
	"ENDCATCH",
	"ENDMETHOD",
	"ENDCLASS",
	"ENDINTERFACE",
	"ENDFORM",
	"ENDFUNCTION",
	"ENDMODULE",
	"ENDENHANCEMENT",
	"END-ENHANCEMENT-SECTION",
	"END-TEST-SEAM",
	"END-TEST-INJECTION",
}

STRUCTURAL_BODY_STOP_MARKERS :: []string {
	"ENDMETHOD",
	"ENDCLASS",
	"ENDINTERFACE",
	"ENDFORM",
	"ENDFUNCTION",
	"ENDMODULE",
	"ENDENHANCEMENT",
	"END-ENHANCEMENT-SECTION",
	"END-TEST-SEAM",
	"END-TEST-INJECTION",
}

STRUCTURAL_OUTER_BOUNDARIES :: []string {
	"ENDCLASS",
	"ENDINTERFACE",
	"ENDMETHOD",
	"ENDFORM",
	"ENDFUNCTION",
	"ENDMODULE",
	"ENDENHANCEMENT",
	"END-ENHANCEMENT-SECTION",
	"END-TEST-SEAM",
	"END-TEST-INJECTION",
}

assignment_starts :: proc(p: ^Parser, index: int) -> bool {
	return assignment_operator_index(p, index) >= 0
}

assignment_operator_index :: proc(p: ^Parser, index: int) -> int {
	if index >= len(p.tokens) || !expr_lead_token(p.tokens[index]) {
		return -1
	}
	if index + 1 < len(p.tokens) &&
	   (p.tokens[index + 1].kind == .Eq || p.tokens[index + 1].kind == .QuestionEq) {
		return index + 1
	}
	if known_stmt_lead_at(p, index) && !keyword_like_assignment_lhs_continues(p, index) {
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

keyword_like_assignment_lhs_continues :: proc(p: ^Parser, index: int) -> bool {
	if index + 1 >= len(p.tokens) {
		return false
	}
	next := p.tokens[index + 1]
	if selector_operator_starts(p.tokens[index], next) || next.kind == .LBracket {
		return true
	}
	return (next.kind == .LParen || next.kind == .Plus) && tokens_touch(p.tokens[index], next)
}

expr_lead_token :: proc(tok: Token) -> bool {
	#partial switch tok.kind {
	case .Ident, .Number, .String, .StringTemplate, .Hash, .At, .LParen, .Plus, .Minus:
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
		keyword_phrase_at(p, index, "SELECTION-SCREEN") ||
		keyword_phrase_at(p, index, "CONTROLS") ||
		keyword_phrase_at(p, index, "CLASS-DATA") ||
		keyword_phrase_at(p, index, "TYPE-POOLS") ||
		keyword_phrase_at(p, index, "FUNCTION-POOL") ||
		keyword_phrase_at(p, index, "INCLUDE") ||
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
		keyword_phrase_at(p, index, "EXEC SQL") ||
		keyword_phrase_at(p, index, "SELECT") ||
		keyword_phrase_at(p, index, "WITH") ||
		keyword_phrase_at(p, index, "OPEN CURSOR") ||
		keyword_phrase_at(p, index, "FETCH") ||
		keyword_phrase_at(p, index, "CLOSE CURSOR") ||
		keyword_phrase_at(p, index, "READ TABLE") ||
		keyword_phrase_at(p, index, "READ") ||
		keyword_phrase_at(p, index, "GENERATE") ||
		keyword_phrase_at(p, index, "INSERT") ||
		keyword_phrase_at(p, index, "APPEND") ||
		keyword_phrase_at(p, index, "MODIFY") ||
		keyword_phrase_at(p, index, "SORT") ||
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
		keyword_phrase_at(p, index, "WRITE") ||
		keyword_phrase_at(p, index, "ASSERT") ||
		keyword_phrase_at(p, index, "CHECK") ||
		keyword_phrase_at(p, index, "RETURN") ||
		keyword_phrase_at(p, index, "CONTINUE") ||
		keyword_phrase_at(p, index, "EXIT") ||
		keyword_phrase_at(p, index, "STOP") ||
		keyword_phrase_at(p, index, "COMMIT") ||
		keyword_phrase_at(p, index, "ROLLBACK") ||
		keyword_phrase_at(p, index, "DESCRIBE") ||
		keyword_phrase_at(p, index, "EXPORT") ||
		keyword_phrase_at(p, index, "IMPORT") ||
		keyword_phrase_at(p, index, "RECEIVE") ||
		keyword_phrase_at(p, index, "GET") ||
		keyword_phrase_at(p, index, "SET") ||
		keyword_phrase_at(p, index, "LOG-POINT") ||
		keyword_phrase_at(p, index, "RAISE") ||
		keyword_phrase_at(p, index, "AUTHORITY-CHECK") ||
		keyword_phrase_at(p, index, "FIELD-GROUPS") ||
		keyword_phrase_at(p, index, "FIELD") ||
		keyword_phrase_at(p, index, "ASSIGN") ||
		keyword_phrase_at(p, index, "CREATE") ||
		keyword_phrase_at(p, index, "OVERLAY") ||
		keyword_phrase_at(p, index, "PACK") ||
		keyword_phrase_at(p, index, "UNPACK") ||
		keyword_phrase_at(p, index, "CONVERT") ||
		keyword_phrase_at(p, index, "WAIT") ||
		keyword_phrase_at(p, index, "SKIP") ||
		keyword_phrase_at(p, index, "ULINE") ||
		keyword_phrase_at(p, index, "NEW-LINE") ||
		keyword_phrase_at(p, index, "NEW-PAGE") ||
		keyword_phrase_at(p, index, "RESERVE") ||
		keyword_phrase_at(p, index, "BACK") ||
		keyword_phrase_at(p, index, "FORMAT") ||
		keyword_phrase_at(p, index, "POSITION") ||
		keyword_phrase_at(p, index, "HIDE") ||
		keyword_phrase_at(p, index, "DEFINE") ||
		keyword_phrase_at(p, index, "PUBLIC") ||
		keyword_phrase_at(p, index, "PROTECTED") ||
		keyword_phrase_at(p, index, "PRIVATE") ||
		keyword_phrase_at(p, index, "METHODS") ||
		keyword_phrase_at(p, index, "CLASS-METHODS") ||
		keyword_phrase_at(p, index, "INTERFACES") ||
		keyword_phrase_at(p, index, "EVENTS") ||
		keyword_phrase_at(p, index, "CLASS-EVENTS") ||
		keyword_phrase_at(p, index, "ALIASES") \
	)
}

parse_stray_block_boundary_stmt :: proc(p: ^Parser) -> ^ast.Stmt {
	boundary, ok := stray_block_boundary(p)
	if !ok {
		return nil
	}
	mark := mark_statement_start(p)
	start := current_token(p)
	count := keyword_phrase_token_count(boundary)
	for _ in 0 ..< count {
		bump_token(p)
	}
	recover_to_statement_boundary(p, nil, true)
	error(
		p,
		tokenizer.text_range(start.range.start, previous_token(p).range.end),
		stray_block_boundary_message(boundary),
	)
	return build_invalid_statement(p, mark)
}

stray_block_boundary :: proc(p: ^Parser) -> (string, bool) {
	for boundary in STRAY_BLOCK_BOUNDARIES {
		if at_keyword_phrase(p, boundary) {
			return boundary, true
		}
	}
	return "", false
}

STRAY_BLOCK_BOUNDARIES :: []string {
	"ELSEIF",
	"ELSE",
	"ENDIF",
	"WHEN",
	"ENDCASE",
	"ENDWHILE",
	"ENDDO",
	"ENDLOOP",
	"CATCH",
	"CLEANUP",
	"ENDTRY",
	"ENDCATCH",
	"ENDCLASS",
	"ENDINTERFACE",
	"ENDMETHOD",
	"ENDEXEC",
	"ENDFORM",
	"ENDFUNCTION",
	"ENDMODULE",
	"ENDENHANCEMENT",
	"ENDSELECT",
	"END-ENHANCEMENT-SECTION",
	"END-TEST-SEAM",
	"END-TEST-INJECTION",
}

stray_block_boundary_message :: proc(boundary: string) -> string {
	if boundary == "ENDIF" {
		return "syntax error: unexpected ENDIF without matching IF"
	}
	if boundary == "CATCH" {
		return "syntax error: unexpected CATCH without matching TRY"
	}
	if boundary == "WHEN" {
		return "syntax error: unexpected WHEN without matching CASE"
	}
	if boundary == "ENDCASE" {
		return "syntax error: unexpected ENDCASE without matching CASE"
	}
	if boundary == "ENDWHILE" {
		return "syntax error: unexpected ENDWHILE without matching WHILE"
	}
	if boundary == "ENDLOOP" {
		return "syntax error: unexpected ENDLOOP without matching LOOP"
	}
	if boundary == "ENDTRY" {
		return "syntax error: unexpected ENDTRY without matching TRY"
	}
	if boundary == "ELSEIF" {
		return "syntax error: unexpected ELSEIF without matching IF"
	}
	if boundary == "ELSE" {
		return "syntax error: unexpected ELSE without matching IF"
	}
	if boundary == "ENDDO" {
		return "syntax error: unexpected ENDDO without matching DO"
	}
	if boundary == "CLEANUP" {
		return "syntax error: unexpected CLEANUP without matching TRY"
	}
	if boundary == "ENDCLASS" {
		return "syntax error: unexpected ENDCLASS without matching CLASS"
	}
	if boundary == "ENDINTERFACE" {
		return "syntax error: unexpected ENDINTERFACE without matching INTERFACE"
	}
	if boundary == "ENDMETHOD" {
		return "syntax error: unexpected ENDMETHOD without matching METHOD"
	}
	if boundary == "ENDFORM" {
		return "syntax error: unexpected ENDFORM without matching FORM"
	}
	if boundary == "ENDFUNCTION" {
		return "syntax error: unexpected ENDFUNCTION without matching FUNCTION"
	}
	if boundary == "ENDMODULE" {
		return "syntax error: unexpected ENDMODULE without matching MODULE"
	}
	if boundary == "ENDENHANCEMENT" {
		return "syntax error: unexpected ENDENHANCEMENT without matching ENHANCEMENT"
	}
	if boundary == "ENDSELECT" {
		return "syntax error: unexpected ENDSELECT without matching SELECT"
	}
	if boundary == "ENDEXEC" {
		return "syntax error: unexpected ENDEXEC without matching EXEC SQL"
	}
	if boundary == "ENDCATCH" {
		return "syntax error: unexpected ENDCATCH without matching CATCH SYSTEM-EXCEPTIONS"
	}
	if boundary == "END-ENHANCEMENT-SECTION" {
		return "syntax error: unexpected END-ENHANCEMENT-SECTION without matching ENHANCEMENT-SECTION"
	}
	if boundary == "END-TEST-SEAM" {
		return "syntax error: unexpected END-TEST-SEAM without matching TEST-SEAM"
	}
	if boundary == "END-TEST-INJECTION" {
		return "syntax error: unexpected END-TEST-INJECTION without matching TEST-INJECTION"
	}
	return "syntax error: unexpected block boundary without matching opener"
}

parse_error_is_include_fragment_boundary :: proc(e: Parse_Error) -> bool {
	if strings.has_prefix(e.message, "syntax error: unexpected ") &&
	   strings.contains(e.message, " without matching ") {
		return true
	}
	if !strings.has_prefix(e.message, "syntax error: expected ") {
		return false
	}
	boundary := e.message[len("syntax error: expected "):]
	for expected in MISSING_FRAGMENT_BOUNDARIES {
		if boundary == expected {
			return true
		}
	}
	return false
}

MISSING_FRAGMENT_BOUNDARIES :: []string {
	"ENDAT",
	"ENDCASE",
	"ENDCATCH",
	"ENDCLASS",
	"ENDDO",
	"ENDENHANCEMENT",
	"END-ENHANCEMENT-SECTION",
	"END-TEST-SEAM",
	"END-TEST-INJECTION",
	"ENDFORM",
	"ENDFUNCTION",
	"ENDIF",
	"ENDINTERFACE",
	"ENDLOOP",
	"ENDMETHOD",
	"ENDEXEC",
	"ENDMODULE",
	"ENDSELECT",
	"ENDTRY",
	"ENDWHILE",
}

next_significant_index :: proc(index: int) -> int {
	return index
}

error :: proc(p: ^Parser, range: Range, message: string) {
	append(&p.errors, Parse_Error{message, range})
}

error_current :: proc(p: ^Parser, message: string) {
	error(p, current_token(p).range, message)
}
