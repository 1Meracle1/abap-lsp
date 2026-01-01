package lang_symbols

import "../lexer"

SymbolKind :: enum {
	Variable,
}

Symbol :: struct {
	name:  string,
	kind:  SymbolKind,
	range: lexer.TextRange,
}

SymbolTable :: struct {
	symbols: map[string]Symbol,
}
