package lang_lexer

TokenKind :: enum u8 {
	Ident,
	Number,
	Comment,
	String, // '...'
	StringTemplate, // |...|
	StringTemplateLit, // literal part inside string template
	Period, // .
	Comma, // ,
	Colon, // :
	LParen, // (
	RParen, // )
	LBrace, // { (for string template expressions)
	RBrace, // } (for string template expressions)
	LBracket, // [ (for table expressions)
	RBracket, // ] (for table expressions)
	Arrow, // ->
	FatArrow, // =>
	Tilde, // ~ (interface component access)
	Hash, // # (type inference marker)
	At, // @ (host variable reference in Open SQL)
	Eq, // =
	Minus, // -
	Plus, // +
	Star, // *
	Slash, // /
	Lt, // <
	Gt, // >
	Le, // <=
	Ge, // >=
	Ne, // <>
	QuestionEq, // ?= (downcast assignment)
	Pipe, // | (for string templates)
	Ampersand, // & (for string concatenation)
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
	case .StringTemplate:
		res = "string template"
	case .StringTemplateLit:
		res = "string template literal"
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
	case .LBrace:
		res = "{"
	case .RBrace:
		res = "}"
	case .LBracket:
		res = "["
	case .RBracket:
		res = "]"
	case .Arrow:
		res = "->"
	case .FatArrow:
		res = "=>"
	case .Tilde:
		res = "~"
	case .Hash:
		res = "#"
	case .At:
		res = "@"
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
	case .Lt:
		res = "<"
	case .Gt:
		res = ">"
	case .Le:
		res = "<="
	case .Ge:
		res = ">="
	case .Ne:
		res = "<>"
	case .QuestionEq:
		res = "?="
	case .Pipe:
		res = "|"
	case .Ampersand:
		res = "&"
	case .Other:
		res = "other"
	case .EOF:
		res = "eof"
	}
	return res
}
