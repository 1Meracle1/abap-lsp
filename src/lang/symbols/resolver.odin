package lang_symbols

import "../ast"

resolve_file :: proc(file: ^ast.File) -> ^SymbolTable {
	table := new(SymbolTable)
	table.symbols = make(map[string]Symbol)

	for decl in file.decls {
		#partial switch d in decl.derived_stmt {
		case ^ast.Data_Inline_Decl:
			name := d.ident.name
			sym := Symbol {
				name  = name,
				kind  = .Variable,
				range = d.ident.range,
			}
			table.symbols[name] = sym
		}
	}

	return table
}
