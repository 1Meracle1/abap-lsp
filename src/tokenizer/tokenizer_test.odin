package abap_frontend_tokenizer

import "core:testing"

@(test)
comments_are_trivia_not_tokens :: proc(t: ^testing.T) {
	source := `DATA lv. " inline
* full
DATA lv2.`
	result := tokenize(source, context.allocator)
	defer delete(result.tokens, context.allocator)
	defer delete(result.trivia, context.allocator)
	defer delete(result.errors, context.allocator)

	for token in result.tokens {
		testing.expect(t, token.kind != .Comment)
	}

	comment_count := 0
	for piece in result.trivia {
		if piece.kind == .Comment {
			comment_count += 1
		}
	}
	testing.expect_value(t, comment_count, 2)

	pieces := trivia_in_range(result, text_range(8, 25))
	testing.expect(t, len(pieces) >= 2)
}

@(test)
star_comment_requires_column_one :: proc(t: ^testing.T) {
	source := "* full\n  * multiply"
	result := tokenize(source, context.allocator)
	defer delete(result.tokens, context.allocator)
	defer delete(result.trivia, context.allocator)
	defer delete(result.errors, context.allocator)

	comment_count := 0
	star_tokens := 0
	for piece in result.trivia {
		if piece.kind == .Comment {
			comment_count += 1
		}
	}
	for token in result.tokens {
		if token.kind == .Star {
			star_tokens += 1
		}
	}

	testing.expect_value(t, comment_count, 1)
	testing.expect_value(t, star_tokens, 1)
}

@(test)
escaped_identifier_is_one_token :: proc(t: ^testing.T) {
	source := `!include TYPE string`
	result := tokenize(source, context.allocator)
	defer delete(result.tokens, context.allocator)
	defer delete(result.trivia, context.allocator)
	defer delete(result.errors, context.allocator)

	testing.expect_value(t, len(result.errors), 0)
	testing.expect_value(t, result.tokens[0].kind, Token_Kind.Ident)
	testing.expect_value(t, token_lexeme(result.tokens[0], source), "!include")
}

@(test)
digit_prefixed_identifier_is_one_token :: proc(t: ^testing.T) {
	source := `1sdf 123`
	result := tokenize(source, context.allocator)
	defer delete(result.tokens, context.allocator)
	defer delete(result.trivia, context.allocator)
	defer delete(result.errors, context.allocator)

	testing.expect_value(t, len(result.errors), 0)
	testing.expect_value(t, result.tokens[0].kind, Token_Kind.Ident)
	testing.expect_value(t, token_lexeme(result.tokens[0], source), "1sdf")
	testing.expect_value(t, result.tokens[1].kind, Token_Kind.Number)
	testing.expect_value(t, token_lexeme(result.tokens[1], source), "123")
}

@(test)
pragma_arguments_are_trivia_not_tokens :: proc(t: ^testing.T) {
	source := `LOOP AT lt WHERE field = lv ##PRIMKEY[FILE_PATH].`
	result := tokenize(source, context.allocator)
	defer delete(result.tokens, context.allocator)
	defer delete(result.trivia, context.allocator)
	defer delete(result.errors, context.allocator)

	pragma_seen := false
	for piece in result.trivia {
		if piece.kind == .Pragma {
			pragma_seen = true
			testing.expect_value(t, trivia_lexeme(piece, source), "##PRIMKEY[FILE_PATH]")
		}
	}
	for token in result.tokens {
		testing.expect(t, token_lexeme(token, source) != "FILE_PATH")
	}
	testing.expect(t, pragma_seen)
}
