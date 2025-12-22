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
	Eq, // =
	Minus, // -
	Plus, // +
	Star, // *
	Slash, // /
	Other, // Invalid?
	Data,
	Type,
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