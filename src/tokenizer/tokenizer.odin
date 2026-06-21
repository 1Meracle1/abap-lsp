package abap_frontend_tokenizer

import "core:mem"
import "core:unicode"
import "core:unicode/utf8"

Lexer :: struct {
	src:           string,
	ch:            rune,
	offset:        int,
	read_offset:   int,
	line_start:    int,
	pending:       [dynamic]Token,
	pending_index: int,
	trivia:        [dynamic]Trivia_Piece,
	errors:        [dynamic]Lex_Error,
	allocator:     mem.Allocator,
}

tokenize :: proc(source: string, allocator: mem.Allocator) -> Tokenize_Result {
	lexer := init(source, allocator)
	raw_tokens := make([dynamic]Token, 0, 64, allocator)

	for {
		token := scan_raw(&lexer)
		done := token.kind == .Eof
		append(&raw_tokens, token)
		if done {
			break
		}
	}

	result := build_tokenize_result(raw_tokens[:], lexer.trivia[:], lexer.errors[:], allocator)
	delete(raw_tokens)
	delete(lexer.pending)
	return result
}

init :: proc(source: string, allocator: mem.Allocator) -> Lexer {
	lexer := Lexer {
		src       = source,
		ch        = ' ',
		pending   = make([dynamic]Token, 0, 16, allocator),
		trivia    = make([dynamic]Trivia_Piece, 0, 32, allocator),
		errors    = make([dynamic]Lex_Error, 0, 8, allocator),
		allocator = allocator,
	}
	advance_rune(&lexer)
	if lexer.ch == utf8.RUNE_BOM {
		advance_rune(&lexer)
	}
	return lexer
}

scan_raw :: proc(lexer: ^Lexer) -> Token {
	if token, ok := pending_pop(lexer); ok {
		return token
	}

	skip_trivia(lexer)
	start := lexer.offset

	if lexer.ch == utf8.RUNE_EOF {
		return token_new(.Eof, text_range(start, lexer.offset))
	}

	if lexer.ch == '|' {
		return lex_string_template(lexer, start)
	}

	return scan_next_token(lexer)
}

scan_next_token :: proc(lexer: ^Lexer) -> Token {
	start := lexer.offset
	ch := lexer.ch

	if ch == '!' && is_escaped_identifier_start(lexer) {
		advance_rune(lexer)
		scan_identifier(lexer)
		return token_new(.Ident, text_range(start, lexer.offset))
	}

	if ch == '/' {
		if is_namespace_start(lexer) {
			scan_identifier(lexer)
			return token_new(.Ident, text_range(start, lexer.offset))
		}
		advance_rune(lexer)
		return token_new(.Slash, text_range(start, lexer.offset))
	}

	if ch == '<' && is_field_symbol_identifier_start(lexer) {
		scan_field_symbol_identifier(lexer)
		return token_new(.Ident, text_range(start, lexer.offset))
	}

	if is_letter(ch) {
		scan_identifier(lexer)
		return token_new(.Ident, text_range(start, lexer.offset))
	}

	if is_digit(ch) {
		scan_number(lexer)
		if is_letter(lexer.ch) {
			scan_identifier(lexer)
			return token_new(.Ident, text_range(start, lexer.offset))
		}
		return token_new(.Number, text_range(start, lexer.offset))
	}

	advance_rune(lexer)
	kind: Token_Kind
	switch ch {
	case '\'':
		scan_string(lexer)
		kind = .String
	case '`':
		scan_backtick_string(lexer)
		kind = .String
	case '.':
		kind = .Period
	case ',':
		kind = .Comma
	case ':':
		kind = .Colon
	case '(':
		kind = .LParen
	case ')':
		kind = .RParen
	case '+':
		kind = .Plus
	case '*':
		kind = .Star
	case '=':
		if lexer.ch == '>' {
			advance_rune(lexer)
			kind = .FatArrow
		} else {
			kind = .Eq
		}
	case '-':
		if lexer.ch == '>' {
			advance_rune(lexer)
			kind = .Arrow
		} else {
			kind = .Minus
		}
	case '~':
		kind = .Tilde
	case '#':
		kind = .Hash
	case '@':
		kind = .At
	case '&':
		kind = .Ampersand
	case '{':
		kind = .LBrace
	case '}':
		kind = .RBrace
	case '[':
		kind = .LBracket
	case ']':
		kind = .RBracket
	case '<':
		if lexer.ch == '=' {
			advance_rune(lexer)
			kind = .Le
		} else if lexer.ch == '>' {
			advance_rune(lexer)
			kind = .Ne
		} else {
			kind = .Lt
		}
	case '>':
		if lexer.ch == '=' {
			advance_rune(lexer)
			kind = .Ge
		} else {
			kind = .Gt
		}
	case '?':
		if lexer.ch == '=' {
			advance_rune(lexer)
			kind = .QuestionEq
		} else {
			kind = .Other
		}
	case '/':
		kind = .Slash
	case '|':
		kind = .Pipe
	case:
		kind = .Other
	}

	return token_new(kind, text_range(start, lexer.offset))
}

lex_string_template :: proc(lexer: ^Lexer, template_start: int) -> Token {
	out := make([dynamic]Token, 0, 8, lexer.allocator)
	open := lexer.offset
	append(&out, token_new(.StringTemplate, text_range(open, open + 1)))
	advance_rune(lexer)

	for {
		lit_start := lexer.offset
		consume_template_literal_fragment(lexer)
		if lexer.offset > lit_start {
			append(&out, token_new(.StringTemplateLit, text_range(lit_start, lexer.offset)))
		}

		switch lexer.ch {
		case '|':
			close := lexer.offset
			advance_rune(lexer)
			append(&out, token_new(.StringTemplate, text_range(close, close + 1)))
			return defer_tokens(lexer, out[:])
		case '{':
			brace := lexer.offset
			advance_rune(lexer)
			append(&out, token_new(.LBrace, text_range(brace, brace + 1)))
			scan_embedded_expression(lexer, &out)
		case utf8.RUNE_EOF:
			push_error(
				lexer,
				template_start,
				lexer.read_offset,
				"string template was not terminated",
			)
			return defer_tokens(lexer, out[:])
		case:
			push_error(
				lexer,
				lexer.offset,
				lexer.read_offset,
				"unexpected character in string template literal",
			)
			advance_rune(lexer)
		}
	}
}

defer_tokens :: proc(lexer: ^Lexer, tokens: []Token) -> Token {
	if len(tokens) == 0 {
		return token_new(.Eof, text_range(lexer.offset, lexer.offset))
	}
	for i in 1 ..< len(tokens) {
		append(&lexer.pending, tokens[i])
	}
	return tokens[0]
}

scan_embedded_expression :: proc(lexer: ^Lexer, out: ^[dynamic]Token) {
	depth := 1
	for depth > 0 {
		token := scan_raw(lexer)
		if token.kind == .Eof {
			append(out, token)
			return
		}

		batch := make([dynamic]Token, 0, 4, lexer.allocator)
		append(&batch, token)
		drain_pending(lexer, &batch)

		for t in batch {
			if t.kind == .LBrace {
				depth += 1
			} else if t.kind == .RBrace {
				depth -= 1
			}
			append(out, t)
			if depth == 0 {
				break
			}
		}
	}
}

consume_template_literal_fragment :: proc(lexer: ^Lexer) {
	for {
		switch lexer.ch {
		case utf8.RUNE_EOF:
			return
		case '|', '{':
			return
		case '\n':
			push_error(
				lexer,
				lexer.offset,
				lexer.read_offset,
				"unescaped newline in string template",
			)
			advance_rune(lexer)
			return
		case '\\':
			advance_rune(lexer)
			switch lexer.ch {
			case utf8.RUNE_EOF:
				push_error(
					lexer,
					lexer.offset,
					lexer.read_offset,
					"string template escape incomplete",
				)
				return
			case '|', '{', '}', '\\', 'n', 'r', 't':
				advance_rune(lexer)
			case:
				push_error(
					lexer,
					lexer.offset,
					lexer.read_offset,
					"invalid escape in string template",
				)
				advance_rune(lexer)
			}
		case:
			advance_rune(lexer)
		}
	}
}

scan_string :: proc(lexer: ^Lexer) {
	start := lexer.offset - 1
	for {
		ch := lexer.ch
		if ch == utf8.RUNE_EOF || ch == '\n' {
			push_error(lexer, start, lexer.read_offset, "string literal was not terminated")
			return
		}
		advance_rune(lexer)
		if ch == '\'' {
			return
		}
	}
}

scan_backtick_string :: proc(lexer: ^Lexer) {
	start := lexer.offset - 1
	for {
		ch := lexer.ch
		if ch == utf8.RUNE_EOF || ch == '\n' {
			push_error(lexer, start, lexer.read_offset, "string template was not terminated")
			return
		}
		if ch == '`' {
			advance_rune(lexer)
			if lexer.ch == '`' {
				advance_rune(lexer)
				continue
			}
			return
		}
		advance_rune(lexer)
	}
}

scan_comment :: proc(lexer: ^Lexer) {
	for lexer.ch != utf8.RUNE_EOF && lexer.ch != '\n' {
		advance_rune(lexer)
	}
}

scan_pragma :: proc(lexer: ^Lexer) {
	advance_rune(lexer)
	advance_rune(lexer)
	for is_pragma_char(lexer.ch) {
		advance_rune(lexer)
	}
	if lexer.ch == '[' {
		depth := 0
		for {
			if lexer.ch == utf8.RUNE_EOF || lexer.ch == '\n' {
				return
			}
			if lexer.ch == '[' {
				depth += 1
			} else if lexer.ch == ']' {
				depth -= 1
			}
			advance_rune(lexer)
			if depth == 0 {
				return
			}
		}
	}
}

scan_number :: proc(lexer: ^Lexer) {
	for is_digit(lexer.ch) {
		advance_rune(lexer)
	}
}

scan_identifier :: proc(lexer: ^Lexer) {
	for is_letter(lexer.ch) || is_digit(lexer.ch) {
		advance_rune(lexer)
	}
}

scan_field_symbol_identifier :: proc(lexer: ^Lexer) {
	advance_rune(lexer)
	for is_letter(lexer.ch) || is_digit(lexer.ch) {
		advance_rune(lexer)
	}
	if lexer.ch == '>' {
		advance_rune(lexer)
	}
}

skip_trivia :: proc(lexer: ^Lexer) {
	for {
		switch lexer.ch {
		case ' ', '\t':
			start := lexer.offset
			for lexer.ch == ' ' || lexer.ch == '\t' {
				advance_rune(lexer)
			}
			append(&lexer.trivia, Trivia_Piece{.Whitespace, text_range(start, lexer.offset)})
		case '\r', '\n':
			start := lexer.offset
			if lexer.ch == '\r' {
				advance_rune(lexer)
				if lexer.ch == '\n' {
					advance_rune(lexer)
				}
			} else {
				advance_rune(lexer)
			}
			append(&lexer.trivia, Trivia_Piece{.Newline, text_range(start, lexer.offset)})
		case '*':
			if lexer.offset != lexer.line_start {
				return
			}
			start := lexer.offset
			scan_comment(lexer)
			append(&lexer.trivia, Trivia_Piece{.Comment, text_range(start, lexer.offset)})
		case '"':
			start := lexer.offset
			scan_comment(lexer)
			append(&lexer.trivia, Trivia_Piece{.Comment, text_range(start, lexer.offset)})
		case '#':
			if peek_byte(lexer) != '#' {
				return
			}
			start := lexer.offset
			scan_pragma(lexer)
			append(&lexer.trivia, Trivia_Piece{.Pragma, text_range(start, lexer.offset)})
		case:
			return
		}
	}
}

advance_rune :: proc(lexer: ^Lexer) {
	if lexer.read_offset >= len(lexer.src) {
		if lexer.ch == '\n' {
			lexer.line_start = len(lexer.src)
		}
		lexer.offset = len(lexer.src)
		lexer.ch = utf8.RUNE_EOF
		return
	}

	if lexer.ch == '\n' {
		lexer.line_start = lexer.read_offset
	}

	lexer.offset = lexer.read_offset
	r := rune(lexer.src[lexer.read_offset])
	w := 1

	if r >= utf8.RUNE_SELF {
		r, w = utf8.decode_rune_in_string(lexer.src[lexer.read_offset:])
		if r == utf8.RUNE_ERROR && w == 1 {
			push_error(lexer, lexer.offset, lexer.read_offset, "illegal UTF-8 encoding")
		} else if r == utf8.RUNE_BOM && lexer.offset > 0 {
			push_error(lexer, lexer.offset, lexer.offset + w, "illegal byte order mark")
		}
	} else if r == 0 {
		push_error(lexer, lexer.offset, lexer.offset + 1, "illegal character NUL")
	}

	lexer.read_offset += w
	lexer.ch = r
}

peek_byte :: proc(lexer: ^Lexer, offset := 0) -> byte {
	index := lexer.read_offset + offset
	if index < len(lexer.src) {
		return lexer.src[index]
	}
	return 0
}

is_namespace_start :: proc(lexer: ^Lexer) -> bool {
	return lexer.ch == '/' && is_ascii_alpha(peek_byte(lexer))
}

is_escaped_identifier_start :: proc(lexer: ^Lexer) -> bool {
	if lexer.read_offset >= len(lexer.src) {
		return false
	}
	r, _ := utf8.decode_rune_in_string(lexer.src[lexer.read_offset:])
	return is_letter(r)
}

is_field_symbol_identifier_start :: proc(lexer: ^Lexer) -> bool {
	if lexer.ch != '<' || lexer.read_offset >= len(lexer.src) {
		return false
	}

	i := lexer.read_offset
	r, w := utf8.decode_rune_in_string(lexer.src[i:])
	if !is_letter(r) {
		return false
	}
	i += w

	for i < len(lexer.src) {
		r, w = utf8.decode_rune_in_string(lexer.src[i:])
		if is_letter(r) || is_digit(r) {
			i += w
			continue
		}
		return r == '>'
	}

	return false
}

push_error :: proc(lexer: ^Lexer, start, end: int, message: string) {
	append(&lexer.errors, Lex_Error{text_range(start, end), message})
}

pending_pop :: proc(lexer: ^Lexer) -> (Token, bool) {
	if lexer.pending_index < len(lexer.pending) {
		token := lexer.pending[lexer.pending_index]
		lexer.pending_index += 1
		return token, true
	}
	return Token{}, false
}

drain_pending :: proc(lexer: ^Lexer, out: ^[dynamic]Token) {
	for {
		if token, ok := pending_pop(lexer); ok {
			append(out, token)
		} else {
			return
		}
	}
}

build_tokenize_result :: proc(
	raw_tokens: []Token,
	trivia: []Trivia_Piece,
	errors: []Lex_Error,
	allocator: mem.Allocator,
) -> Tokenize_Result {
	tokens := make([dynamic]Token, 0, len(raw_tokens), allocator)
	previous_significant := -1
	trivia_cursor := 0

	for raw in raw_tokens {
		interstitial_start := trivia_cursor
		for trivia_cursor < len(trivia) && trivia[trivia_cursor].range.end <= raw.range.start {
			trivia_cursor += 1
		}

		token := raw
		token.index = len(tokens)
		interstitial := trivia_span(interstitial_start, trivia_cursor)

		if previous_significant >= 0 {
			split := first_newline_piece(trivia[interstitial.start:interstitial.end])
			if split >= 0 {
				split += interstitial.start
			} else {
				split = interstitial.end
			}

			tokens[previous_significant].trailing_trivia = trivia_span(interstitial.start, split)
			if has_comment_or_pragma(trivia[interstitial.start:split]) {
				tokens[previous_significant].flags += {.Has_Trailing_Inline_Comment}
			}
			token.leading_trivia = trivia_span(split, interstitial.end)
			if has_newline(trivia[split:interstitial.end]) {
				token.flags += {.Has_Newline_Before}
			}
		} else {
			token.leading_trivia = interstitial
			if has_newline(trivia[interstitial.start:interstitial.end]) {
				token.flags += {.Has_Newline_Before}
			}
		}

		append(&tokens, token)
		previous_significant = len(tokens) - 1
	}

	return Tokenize_Result{tokens[:], trivia[:], errors}
}

first_newline_piece :: proc(trivia: []Trivia_Piece) -> int {
	for piece, i in trivia {
		if piece.kind == .Newline {
			return i
		}
	}
	return -1
}

has_newline :: proc(trivia: []Trivia_Piece) -> bool {
	for piece in trivia {
		if piece.kind == .Newline {
			return true
		}
	}
	return false
}

has_comment_or_pragma :: proc(trivia: []Trivia_Piece) -> bool {
	for piece in trivia {
		if piece.kind == .Comment || piece.kind == .Pragma {
			return true
		}
	}
	return false
}

is_letter :: proc(r: rune) -> bool {
	if r == '_' || r == '/' || r == '%' {
		return true
	}
	if 'A' <= r && r <= 'Z' || 'a' <= r && r <= 'z' {
		return true
	}
	return r >= utf8.RUNE_SELF && unicode.is_letter(r)
}

is_digit :: proc(r: rune) -> bool {
	return '0' <= r && r <= '9'
}

is_ascii_alpha :: proc(b: byte) -> bool {
	return 'A' <= b && b <= 'Z' || 'a' <= b && b <= 'z'
}

is_pragma_char :: proc(r: rune) -> bool {
	return r == '_' || '0' <= r && r <= '9' || 'A' <= r && r <= 'Z' || 'a' <= r && r <= 'z'
}
