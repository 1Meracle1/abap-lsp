package tests_lexer

import "../../src/lang/lexer"
import "core:fmt"
import "core:testing"

collect_tokens :: proc(src: string) -> []lexer.Token {
	l: lexer.Lexer
	lexer.init(&l, src, nil, nil)

	tokens := make([dynamic]lexer.Token)
	for {
		tok := lexer.scan(&l)
		append(&tokens, tok)
		if tok.kind == .EOF {
			break
		}
	}

	return tokens[:]
}

@(test)
field_symbol_identifier_is_single_token_test :: proc(t: ^testing.T) {
	tokens := collect_tokens(`ASSERT <ls_outbound> IS ASSIGNED.`)

	if !testing.expect(t, len(tokens) == 6, fmt.tprintf("Expected 6 tokens, got %d", len(tokens))) do return

	testing.expect(t, tokens[0].kind == .Ident, fmt.tprintf("Expected ASSERT to be Ident, got %v", tokens[0].kind))
	testing.expect(t, tokens[0].lit == "ASSERT", fmt.tprintf("Expected 'ASSERT', got '%s'", tokens[0].lit))

	testing.expect(
		t,
		tokens[1].kind == .Ident,
		fmt.tprintf("Expected field symbol to be Ident, got %v", tokens[1].kind),
	)
	testing.expect(
		t,
		tokens[1].lit == "<ls_outbound>",
		fmt.tprintf("Expected '<ls_outbound>', got '%s'", tokens[1].lit),
	)

	testing.expect(t, tokens[2].kind == .Ident, fmt.tprintf("Expected IS to be Ident, got %v", tokens[2].kind))
	testing.expect(t, tokens[2].lit == "IS", fmt.tprintf("Expected 'IS', got '%s'", tokens[2].lit))

	testing.expect(
		t,
		tokens[3].kind == .Ident,
		fmt.tprintf("Expected ASSIGNED to be Ident, got %v", tokens[3].kind),
	)
	testing.expect(
		t,
		tokens[3].lit == "ASSIGNED",
		fmt.tprintf("Expected 'ASSIGNED', got '%s'", tokens[3].lit),
	)

	testing.expect(t, tokens[4].kind == .Period, fmt.tprintf("Expected period, got %v", tokens[4].kind))
	testing.expect(t, tokens[5].kind == .EOF, fmt.tprintf("Expected EOF, got %v", tokens[5].kind))
}
