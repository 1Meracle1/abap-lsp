package lang_lexer

TokenKind :: enum u8 {
	Ident,
	Number,
	Comment,
	String, // '...'
	Period, // .
	Comma, // ,
	Colon, // :
	LParen, // (
	RParen, // )
	Arrow, // ->
	FatArrow, // =>
	Tilde, // ~ (interface component access)
	Hash, // # (type inference marker)
	Eq, // =
	Minus, // -
	Plus, // +
	Star, // *
	Slash, // /
	Other, // Invalid?
	EOF,
}

Token :: struct {
	kind:  TokenKind,
	lit:   string,
	range: TextRange,
}

TextRange :: struct {
	start: int,
	end:   int,
}

have_space_between :: proc(lhs, rhs: Token) -> bool {
	return lhs.range.end < rhs.range.start
}

range_between :: proc(lhs, rhs: Token) -> TextRange {
	start := min(lhs.range.end, rhs.range.start)
	end := max(lhs.range.end, rhs.range.start)
	return TextRange{start, end}
}

token_kind_string :: proc(token_kind: TokenKind) -> string {
	res: string
	switch token_kind {
	case .Ident:
		res = "identifier"
	case .Number:
		res = "number"
	case .Comment:
		res = "comment"
	case .String:
		res = "string"
	case .Period:
		res = "."
	case .Comma:
		res = ","
	case .Colon:
		res = ":"
	case .LParen:
		res = "("
	case .RParen:
		res = ")"
	case .Arrow:
		res = "->"
	case .FatArrow:
		res = "=>"
	case .Tilde:
		res = "~"
	case .Hash:
		res = "#"
	case .Eq:
		res = "="
	case .Minus:
		res = "-"
	case .Plus:
		res = "+"
	case .Star:
		res = "*"
	case .Slash:
		res = "/"
	case .Other:
		res = "other"
	case .EOF:
		res = "eof"
	}
	return res
}
