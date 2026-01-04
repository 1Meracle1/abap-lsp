package lang_symbols

import "../ast"
import "../lexer"

import "core:fmt"
import "core:mem"
import "core:strings"

Diagnostic :: struct {
	range:   lexer.TextRange,
	message: string,
}

SymbolKind :: enum {
	Variable,
	Constant,
	Parameter,
	Field,
	Method,
	Class,
	Interface,
	Form,
	FormParameter,
	TypeDef,
	Report,
	Include,
	Event,
	Module,
	FieldSymbol,
	Control,
}

FormParamKind :: enum {
	None,
	Tables,
	Using,
	Changing,
}

Symbol :: struct {
	name:            string,
	kind:            SymbolKind,
	range:           lexer.TextRange,
	type_info:       ^Type,
	is_chained:      bool,
	child_scope:     ^SymbolTable,
	form_param_kind: FormParamKind,
}

SymbolTable :: struct {
	symbols:     map[string]Symbol,
	types:       [dynamic]^Type,
	diagnostics: [dynamic]Diagnostic,
}

add_diagnostic :: proc(table: ^SymbolTable, range: lexer.TextRange, message: string) {
	append(&table.diagnostics, Diagnostic{range = range, message = message})
}

add_symbol :: proc(table: ^SymbolTable, sym: Symbol, allow_shadowing: bool = false) -> bool {
	upper_name := strings.to_lower(sym.name)
	if existing, found := table.symbols[upper_name]; found {
		if !allow_shadowing {
			add_diagnostic(table, sym.range, fmt.tprintf("Duplicate symbol '%s'", upper_name))
		}
		modified_sym := sym
		modified_sym.name = upper_name
		table.symbols[upper_name] = modified_sym
		return false
	}
	modified_sym := sym
	modified_sym.name = upper_name
	table.symbols[upper_name] = modified_sym
	return true
}

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
	t.name = strings.to_lower(name)
	t.ast_node = ast_node
	return t
}

make_table_type :: proc(table: ^SymbolTable, elem: ^Type, kind: TableTypeKind = .Any) -> ^Type {
	t := make_type(table, .Table)
	t.elem_type = elem
	t.table_kind = kind
	return t
}

make_line_of_type :: proc(table: ^SymbolTable, target: ^Type) -> ^Type {
	t := make_type(table, .LineOf)
	t.target_type = target
	return t
}

make_table_key_info :: proc(table: ^SymbolTable, is_unique: bool = false, is_default: bool = false) -> ^TableKeyInfo {
	key := new(TableKeyInfo)
	key.is_unique = is_unique
	key.is_default = is_default
	key.components = make([dynamic]string)
	return key
}

add_key_component :: proc(key: ^TableKeyInfo, component: string) {
	if key != nil {
		append(&key.components, strings.to_lower(component))
	}
}

make_reference_type :: proc(table: ^SymbolTable, target: ^Type) -> ^Type {
	t := make_type(table, .Reference)
	t.target_type = target
	return t
}

make_structure_type :: proc(table: ^SymbolTable, name: string) -> ^Type {
	t := make_type(table, .Structure)
	t.name = strings.to_lower(name)
	t.fields = make([dynamic]StructField)
	return t
}

add_struct_field :: proc(t: ^Type, name: string, type_info: ^Type, length: int = 0) {
	if t == nil || t.kind != .Structure {
		return
	}
	append(
		&t.fields,
		StructField{name = strings.to_lower(name), type_info = type_info, length = length},
	)
}

collect_all_diagnostics :: proc(
	table: ^SymbolTable,
	allocator: mem.Allocator = context.allocator,
) -> []Diagnostic {
	result := make([dynamic]Diagnostic, allocator)
	collect_diagnostics_recursive(table, &result)
	return result[:]
}

collect_diagnostics_recursive :: proc(table: ^SymbolTable, result: ^[dynamic]Diagnostic) {
	// Add diagnostics from this table
	for diag in table.diagnostics {
		append(result, diag)
	}
	// Recurse into child scopes
	for _, sym in table.symbols {
		if sym.child_scope != nil {
			collect_diagnostics_recursive(sym.child_scope, result)
		}
	}
}

destroy_symbol_table :: proc(table: ^SymbolTable) {
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
	delete(table.diagnostics)
	free(table)
}
