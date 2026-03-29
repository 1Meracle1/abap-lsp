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

Visibility :: enum {
	None,
	Public,
	Protected,
	Private,
}

Symbol :: struct {
	name:            string,
	kind:            SymbolKind,
	range:           lexer.TextRange,
	type_info:       ^Type,
	is_chained:      bool,
	child_scope:     ^SymbolTable,
	form_param_kind: FormParamKind,
	visibility:      Visibility,
	is_static:       bool,
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

// clone_symbol_table creates a shallow copy of a symbol table for include processing.
clone_symbol_table :: proc(source: ^SymbolTable, allocator := context.allocator) -> ^SymbolTable {
	if source == nil {
		return nil
	}

	cloned := new(SymbolTable, allocator)
	cloned.symbols = make(map[string]Symbol, len(source.symbols), allocator)
	cloned.types = make([dynamic]^Type, len(source.types), allocator)
	cloned.diagnostics = make([dynamic]Diagnostic, allocator)

	// Copy all symbols (shallow copy - child_scope references are shared)
	for name, sym in source.symbols {
		cloned.symbols[name] = sym
	}

	// Copy type references (not deep copy - types are shared)
	for t in source.types {
		append(&cloned.types, t)
	}

	// Diagnostics are NOT copied - each file accumulates its own diagnostics

	return cloned
}

create_empty_symbol_table :: proc(allocator := context.allocator) -> ^SymbolTable {
	table := new(SymbolTable, allocator)
	table.symbols = make(map[string]Symbol, allocator)
	table.types = make([dynamic]^Type, allocator)
	table.diagnostics = make([dynamic]Diagnostic, allocator)
	register_builtin_symbols(table)
	return table
}

make_builtin_char_type :: proc(table: ^SymbolTable, length: int) -> ^Type {
	t := make_type(table, .Char)
	t.length = length
	return t
}

add_builtin_constant :: proc(table: ^SymbolTable, name: string, type_info: ^Type) {
	add_symbol(
		table,
		Symbol{
			name      = name,
			kind      = .Constant,
			type_info = type_info,
		},
	)
}

register_builtin_symbols :: proc(table: ^SymbolTable) {
	if table == nil {
		return
	}

	syst_type := make_structure_type(table, "syst")
	add_struct_field(syst_type, "subrc", make_type(table, .Integer))
	add_struct_field(syst_type, "tabix", make_type(table, .Integer))
	add_struct_field(syst_type, "index", make_type(table, .Integer))
	add_struct_field(syst_type, "tfill", make_type(table, .Integer))
	add_struct_field(syst_type, "tleng", make_type(table, .Integer))
	add_struct_field(syst_type, "datum", make_type(table, .Date))
	add_struct_field(syst_type, "uzeit", make_type(table, .Time))
	add_struct_field(syst_type, "mandt", make_builtin_char_type(table, 3))
	add_struct_field(syst_type, "uname", make_builtin_char_type(table, 12))
	add_struct_field(syst_type, "langu", make_builtin_char_type(table, 1))
	add_struct_field(syst_type, "batch", make_builtin_char_type(table, 1))
	add_struct_field(syst_type, "cprog", make_builtin_char_type(table, 40))
	add_struct_field(syst_type, "repid", make_builtin_char_type(table, 40))

	add_symbol(
		table,
		Symbol{
			name      = "syst",
			kind      = .TypeDef,
			type_info = syst_type,
		},
	)
	add_symbol(
		table,
		Symbol{
			name      = "sy",
			kind      = .Variable,
			type_info = syst_type,
		},
	)

	abap_bool_type := make_builtin_char_type(table, 1)
	add_builtin_constant(table, "abap_true", abap_bool_type)
	add_builtin_constant(table, "abap_false", abap_bool_type)
}
