package lang_ast

import "../lexer"
import "base:intrinsics"
import "core:mem"

new_from_range :: proc($T: typeid, range: lexer.TextRange) -> ^T {
	node, _ := mem.new(T)
	node.range = range
	node.derived = node
	base: ^Node = node // dummy check
	_ = base // "Use" type to make -vet happy
	when intrinsics.type_has_field(T, "derived_expr") {
		node.derived_expr = node
	}
	when intrinsics.type_has_field(T, "derived_stmt") {
		node.derived_stmt = node
	}
	return node
}

new_from_tokens :: proc($T: typeid, start_token: lexer.Token, end_token: lexer.Token) -> ^T {
	range: lexer.TextRange
	range.start = start_token.range.start
	range.end = end_token.range.end
	return new_from_range(T, range)
}

new :: proc {
	new_from_range,
	new_from_tokens,
}

new_ident :: proc(name_tok: lexer.Token) -> ^Ident {
	node := new_from_range(Ident, name_tok.range)
	node.name = name_tok.lit
	return node
}

new_data_inline_decl :: proc(
	start_tok, end_tok: lexer.Token,
	ident: ^Ident,
	value: ^Expr,
) -> ^Data_Inline_Decl {
	node := new(Data_Inline_Decl, start_tok, end_tok)
	node.ident = ident
	node.value = value
	return node
}
