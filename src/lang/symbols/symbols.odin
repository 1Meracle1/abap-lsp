package lang_symbols

import "../lexer"
import "../ast"

SymbolKind :: enum {
	Variable,
	Constant,
	Parameter,
	Field,
	Method,
	Class,
	Interface,
	Form,           // FORM subroutine
	FormParameter,  // FORM parameter (TABLES, USING, CHANGING)
}

// Form parameter passing modes (mirrors ast.Form_Param_Kind)
FormParamKind :: enum {
	None,     // Not a form parameter
	Tables,   // TABLES parameter
	Using,    // USING parameter
	Changing, // CHANGING parameter
}

Symbol :: struct {
	name:            string,
	kind:            SymbolKind,
	range:           lexer.TextRange,
	type_info:       ^Type,
	// For chain declarations, track if this is part of a chain
	is_chained:      bool,
	// For Form symbols: child symbol table containing parameters and locals
	child_scope:     ^SymbolTable,
	// For FormParameter symbols: the passing mode
	form_param_kind: FormParamKind,
}

SymbolTable :: struct {
	symbols: map[string]Symbol,
	// Allocator for types (so they can be freed together)
	types:   [dynamic]^Type,
}

// Helper to create types managed by the symbol table

make_type :: proc(table: ^SymbolTable, kind: TypeKind) -> ^Type {
	t := new(Type)
	t.kind = kind
	append(&table.types, t)
	return t
}

make_unknown_type :: proc(table: ^SymbolTable) -> ^Type {
	return make_type(table, .Unknown)
}

make_inferred_type :: proc(table: ^SymbolTable, source_expr: ^ast.Expr) -> ^Type {
	t := make_type(table, .Inferred)
	t.infer_source = source_expr
	return t
}

make_named_type :: proc(table: ^SymbolTable, name: string, ast_node: ^ast.Expr = nil) -> ^Type {
	t := make_type(table, .Named)
	t.name = name
	t.ast_node = ast_node
	return t
}

make_table_type :: proc(table: ^SymbolTable, elem: ^Type) -> ^Type {
	t := make_type(table, .Table)
	t.elem_type = elem
	return t
}

make_reference_type :: proc(table: ^SymbolTable, target: ^Type) -> ^Type {
	t := make_type(table, .Reference)
	t.target_type = target
	return t
}

// Cleanup

destroy_symbol_table :: proc(table: ^SymbolTable) {
	// Recursively destroy child scopes (e.g., from Form declarations)
	for _, sym in table.symbols {
		if sym.child_scope != nil {
			destroy_symbol_table(sym.child_scope)
		}
	}
	for t in table.types {
		free(t)
	}
	delete(table.types)
	delete(table.symbols)
	free(table)
}
