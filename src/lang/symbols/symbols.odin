package lang_symbols

import "../lexer"
import "../ast"

// Type Kinds

TypeKind :: enum {
	Unknown,     // Type not yet resolved or unresolvable
	Inferred,    // Type to be inferred from expression (DATA(x) = ...)
	// Basic ABAP types
	Integer,     // i
	Float,       // f, p (packed/decimal)
	String,      // string
	Char,        // c
	Numeric,     // n
	Date,        // d
	Time,        // t
	Hex,         // x
	XString,     // xstring
	// Complex types
	Table,       // TABLE OF ...
	Structure,   // structured type
	Reference,   // REF TO ...
	// Named/user-defined
	Named,       // reference to a named type (class, interface, etc.)
}

Type :: struct {
	kind:         TypeKind,
	// For Named types: the type name
	name:         string,
	// For Table types: element type
	elem_type:    ^Type,
	// For Reference types: target type
	target_type:  ^Type,
	// For Inferred types: the expression to infer from (kept for later resolution)
	infer_source: ^ast.Expr,
	// Original AST node that defined this type (for diagnostics/navigation)
	ast_node:     ^ast.Expr,
}

// Symbol Kinds

SymbolKind :: enum {
	Variable,
	Constant,
	Parameter,
	Field,
	Method,
	Class,
	Interface,
}

Symbol :: struct {
	name:       string,
	kind:       SymbolKind,
	range:      lexer.TextRange,
	type_info:  ^Type,
	// For chain declarations, track if this is part of a chain
	is_chained: bool,
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
	for t in table.types {
		free(t)
	}
	delete(table.types)
	delete(table.symbols)
	free(table)
}
