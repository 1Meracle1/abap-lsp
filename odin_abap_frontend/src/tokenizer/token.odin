package abap_frontend_tokenizer

Range :: struct {
	start: int,
	end:   int,
}

Token_Kind :: enum {
	Ident,
	Number,
	Comment,
	String,
	StringTemplate,
	StringTemplateLit,
	Period,
	Comma,
	Colon,
	LParen,
	RParen,
	LBrace,
	RBrace,
	LBracket,
	RBracket,
	Arrow,
	FatArrow,
	Tilde,
	Hash,
	At,
	Eq,
	Minus,
	Plus,
	Star,
	Slash,
	Lt,
	Gt,
	Le,
	Ge,
	Ne,
	QuestionEq,
	Pipe,
	Ampersand,
	Other,
	Eof,
}

Trivia_Kind :: enum {
	Whitespace,
	Newline,
	Comment,
	Pragma,
}

Trivia_Piece :: struct {
	kind:  Trivia_Kind,
	range: Range,
}

Trivia_Span :: struct {
	start: int,
	end:   int,
}

Token_Flag :: enum {
	Has_Newline_Before,
	Has_Trailing_Inline_Comment,
}
Token_Flags :: bit_set[Token_Flag]

Token :: struct {
	kind:            Token_Kind,
	range:           Range,
	index:           int,
	leading_trivia:  Trivia_Span,
	trailing_trivia: Trivia_Span,
	flags:           Token_Flags,
}

Lex_Error :: struct {
	range:   Range,
	message: string,
}

Tokenize_Result :: struct {
	tokens: []Token,
	trivia: []Trivia_Piece,
	errors: []Lex_Error,
}

text_range :: proc(start, end: int) -> Range {
	return Range{start, end}
}

trivia_span_empty :: proc() -> Trivia_Span {
	return Trivia_Span{}
}

trivia_span :: proc(start, end: int) -> Trivia_Span {
	return Trivia_Span{start, end}
}

token_new :: proc(kind: Token_Kind, range: Range) -> Token {
	return Token {
		kind = kind,
		range = range,
		index = -1,
		leading_trivia = trivia_span_empty(),
		trailing_trivia = trivia_span_empty(),
	}
}

token_lexeme :: proc(token: Token, source: string) -> string {
	return source[token.range.start:token.range.end]
}

trivia_lexeme :: proc(piece: Trivia_Piece, source: string) -> string {
	return source[piece.range.start:piece.range.end]
}

leading_trivia :: proc(result: Tokenize_Result, token: Token) -> []Trivia_Piece {
	return result.trivia[token.leading_trivia.start:token.leading_trivia.end]
}

trailing_trivia :: proc(result: Tokenize_Result, token: Token) -> []Trivia_Piece {
	return result.trivia[token.trailing_trivia.start:token.trailing_trivia.end]
}

trivia_in_range :: proc(result: Tokenize_Result, range: Range) -> []Trivia_Piece {
	start := len(result.trivia)
	end := len(result.trivia)
	for piece, i in result.trivia {
		if piece.range.end <= range.start {
			continue
		}
		if piece.range.start >= range.end {
			end = i
			break
		}
		if start == len(result.trivia) {
			start = i
		}
	}
	if start == len(result.trivia) {
		start = end
	}
	return result.trivia[start:end]
}
