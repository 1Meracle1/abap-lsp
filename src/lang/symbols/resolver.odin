package lang_symbols

import "../ast"

resolve_file :: proc(file: ^ast.File) -> ^SymbolTable {
	table := new(SymbolTable)
	table.symbols = make(map[string]Symbol)
	table.types = make([dynamic]^Type)

	for decl in file.decls {
		#partial switch d in decl.derived_stmt {
		case ^ast.Data_Inline_Decl:
			resolve_inline_decl(table, d)
		case ^ast.Data_Typed_Decl:
			resolve_typed_decl(table, d, false)
		case ^ast.Data_Typed_Chain_Decl:
			resolve_chain_decl(table, d)
		}
	}

	return table
}

// DATA(var) = expression.
// Type is inferred from the expression.
resolve_inline_decl :: proc(table: ^SymbolTable, decl: ^ast.Data_Inline_Decl) {
	name := decl.ident.name
	
	// Create an inferred type - actual type resolution happens later
	// based on the value expression
	type_info := make_inferred_type(table, decl.value)
	
	sym := Symbol {
		name      = name,
		kind      = .Variable,
		range     = decl.ident.range,
		type_info = type_info,
		is_chained = false,
	}
	table.symbols[name] = sym
}

// DATA var TYPE typename.
// Type is explicitly specified.
resolve_typed_decl :: proc(table: ^SymbolTable, decl: ^ast.Data_Typed_Decl, is_chained: bool) {
	name := decl.ident.name
	
	// Extract type from the typed expression
	type_info := resolve_type_expr(table, decl.typed)
	
	sym := Symbol {
		name       = name,
		kind       = .Variable,
		range      = decl.ident.range,
		type_info  = type_info,
		is_chained = is_chained,
	}
	table.symbols[name] = sym
}

// DATA: var1 TYPE t1, var2 TYPE t2, ...
// Chain of typed declarations.
resolve_chain_decl :: proc(table: ^SymbolTable, chain: ^ast.Data_Typed_Chain_Decl) {
	for decl in chain.decls {
		resolve_typed_decl(table, decl, true)
	}
}

// Resolves a type expression AST node into a Type structure.
// This handles patterns like:
//   - Simple type names: i, string, my_structure
//   - Table types: TABLE OF typename
//   - Reference types: REF TO typename
resolve_type_expr :: proc(table: ^SymbolTable, expr: ^ast.Expr) -> ^Type {
	if expr == nil {
		return make_unknown_type(table)
	}
	
	#partial switch e in expr.derived_expr {
	case ^ast.Ident:
		// Simple type name - check for built-in types first
		type_kind := builtin_type_from_name(e.name)
		if type_kind != .Unknown {
			t := make_type(table, type_kind)
			t.ast_node = expr
			return t
		}
		// Otherwise it's a named/user-defined type
		return make_named_type(table, e.name, expr)
		
	case ^ast.Table_Type:
		// TABLE OF elem_type
		elem_type := resolve_type_expr(table, e.elem)
		t := make_table_type(table, elem_type)
		t.ast_node = expr
		return t
		
	case ^ast.Selector_Expr:
		// Could be a qualified type name like package~typename or similar
		// For now, treat the whole thing as a named type
		// TODO: Handle properly when we have package/namespace resolution
		return make_named_type(table, selector_to_string(e), expr)
	}
	
	// Fallback: unknown type
	return make_unknown_type(table)
}

// Maps ABAP built-in type names to TypeKind
builtin_type_from_name :: proc(name: string) -> TypeKind {
	// ABAP type names are case-insensitive, but we store lowercase
	// Caller should normalize if needed
	switch name {
	case "i", "int4", "int8":
		return .Integer
	case "f", "p", "decfloat16", "decfloat34":
		return .Float
	case "string":
		return .String
	case "c":
		return .Char
	case "n":
		return .Numeric
	case "d":
		return .Date
	case "t":
		return .Time
	case "x":
		return .Hex
	case "xstring":
		return .XString
	}
	return .Unknown
}

// Helper to convert a selector expression chain to a string
selector_to_string :: proc(sel: ^ast.Selector_Expr) -> string {
	// Simple implementation - just use the field name for now
	// TODO: Build full qualified name
	if sel.field != nil {
		return sel.field.name
	}
	return ""
}
