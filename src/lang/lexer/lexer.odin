package lang_lexer

import "core:unicode"
import "core:unicode/utf8"


Error_Handler :: #type proc(userptr: rawptr, range: TextRange, format: string, args: ..any)

Lexer :: struct {
	err:         Error_Handler,
	err_userptr: rawptr,
	src:         string,
	pos:         int,
	read_pos:    int,
	ch:          rune,
	line_start:  int,
	line_count: int,
	error_count: int,
}

init :: proc(l: ^Lexer, src: string, err: Error_Handler, err_userptr: rawptr) {
	l.err = err
	l.err_userptr = err_userptr
	l.src = src
	l.pos = 0
	l.read_pos = 0
	l.ch = ' '
	l.line_start = 0
	l.line_count = 0
	l.error_count = 0

	advance_rune(l)
	if l.ch == utf8.RUNE_BOM {
		advance_rune(l)
	}
}

scan :: proc(l: ^Lexer) -> Token {
	skip_whitespace(l)

	start := l.pos

	kind: TokenKind
	lit: string

	switch ch := l.ch; true {
	case is_letter(ch):
		lit = scan_identifier(l)
		kind = .Ident
	case '0' <= ch && ch <= '9':
		kind, lit = scan_number(l)
	case ch == '*' && l.pos == l.line_start || ch == '"':
		kind, lit = scan_comment(l)
	case:
		advance_rune(l)
		switch ch {
		case -1:
			kind = .EOF
		case '\'':
			kind = .String
			lit = scan_string(l)
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
		case '/':
			kind = .Slash
		case '=':
			if l.ch == '>' {
				advance_rune(l)
				kind = .FatArrow
			} else {
				kind = .Eq
			}
		case '-':
			if l.ch == '>' {
				advance_rune(l)
				kind = .Arrow
			} else {
				kind = .Minus
			}
		}
	}

	return Token{kind, lit, TextRange{start, l.pos}}
}

scan_string :: proc(l: ^Lexer) -> string {
	start := l.pos - 1

	for {
		ch := l.ch
		if ch == '\n' || ch < 0 {
			error(l, start, l.read_pos, "string literal was not terminated")
			break
		}
		advance_rune(l)
		if ch == '\'' {
			break
		}
	}

	return string(l.src[start:l.pos])
}

scan_comment :: proc(l: ^Lexer) -> (TokenKind, string) {
	start := l.pos
	for l.ch != -1 {
		advance_rune(l)
		if l.ch == '\n' {
			break
		}
	}
	return .Comment, string(l.src[start:l.pos])
}

scan_number :: proc(l: ^Lexer) -> (TokenKind, string) {
	start := l.pos
	for is_digit(l.ch) {
		advance_rune(l)
	}
	return .Number, string(l.src[start:l.pos])
}

scan_identifier :: proc(l: ^Lexer) -> string {
	start := l.pos
	for is_letter(l.ch) || is_digit(l.ch) {
		advance_rune(l)
	}
	return string(l.src[start:l.pos])
}

advance_rune :: proc(l: ^Lexer) {
	if l.read_pos < len(l.src) {
		l.pos = l.read_pos
		if l.ch == '\n' {
			l.line_start = l.pos
			l.line_count += 1
		}
		r, w := rune(l.src[l.read_pos]), 1
		switch {
		case r == 0:
			error(l, l.pos, l.read_pos, "illegal character NUL")
		case r >= utf8.RUNE_SELF:
			r, w = utf8.decode_rune_in_string(l.src[l.read_pos:])
			if r == utf8.RUNE_ERROR && w == 1 {
				error(l, l.pos, l.read_pos, "illegal UTF-8 encoding")
			} else if r == utf8.RUNE_BOM && l.pos > 0 {
				error(l, l.pos, l.read_pos, "illegal byte order mark")
			}
		}
		l.read_pos += w
		l.ch = r
	} else {
		l.pos = len(l.src)
		if l.ch == '\n' {
			l.line_start = l.pos
			l.line_count += 1
		}
		l.ch = -1
	}
}

error :: proc(l: ^Lexer, start: int, end: int, msg: string, args: ..any) {
	if l.err != nil {
		l.err(l.err_userptr, TextRange{start, end}, msg, ..args)
	}
	l.error_count += 1
}

peek_byte :: proc(l: ^Lexer, offset := 0) -> byte {
	if l.read_pos + offset < len(l.src) {
		return l.src[l.read_pos + offset]
	}
	return 0
}

skip_whitespace :: proc(l: ^Lexer) {
	for {
		switch l.ch {
		case ' ', '\t', '\r', '\n':
			advance_rune(l)
		case:
			return
		}
	}
}

is_letter :: proc(r: rune) -> bool {
	if r < utf8.RUNE_SELF {
		switch r {
		case '_', '/':
			return true
		case 'A' ..= 'Z', 'a' ..= 'z':
			return true
		}
	}
	return unicode.is_letter(r)
}

is_digit :: proc(r: rune) -> bool {
	if '0' <= r && r <= '9' {
		return true
	}
	return unicode.is_digit(r)
}
