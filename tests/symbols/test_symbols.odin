package tests_symbols

import "../../src/lang/ast"
import "../../src/lang/parser"
import "../../src/lang/symbols"
import "core:fmt"
import "core:testing"

@(test)
test_symbol_resolution :: proc(t: ^testing.T) {
	src := "DATA(my_var) = 1."
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)

	sym, ok := table.symbols["my_var"]

	if table == nil {
		testing.expect(t, false, "symbol table should not be nil")
		return
	}

	if !ok {
		// Fallback or debug print
		msg := fmt.tprintf("expected symbol 'my_var' to be found, got map: %v", table.symbols)
		testing.expect(t, false, msg)
	} else {
		if sym.kind != .Variable {
			msg := fmt.tprintf("expected symbol kind Variable, got %v", sym.kind)
			testing.expect(t, false, msg)
		}
	}
}
