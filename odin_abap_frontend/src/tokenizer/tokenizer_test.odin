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
