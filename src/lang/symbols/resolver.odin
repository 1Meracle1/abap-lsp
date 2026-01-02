package lang_symbols

import "../ast"
import "core:strings"

resolve_file :: proc(file: ^ast.File) -> ^SymbolTable {
	table := new(SymbolTable)
	table.symbols = make(map[string]Symbol)
	table.types = make([dynamic]^Type)
	table.diagnostics = make([dynamic]Diagnostic)

	for decl in file.decls {
		#partial switch d in decl.derived_stmt {
		case ^ast.Data_Inline_Decl:
			resolve_inline_decl(table, d)
		case ^ast.Data_Typed_Decl:
			resolve_typed_decl(table, d, false)
		case ^ast.Data_Typed_Chain_Decl:
			resolve_chain_decl(table, d)
		case ^ast.Types_Decl:
			resolve_types_decl(table, d, false)
		case ^ast.Types_Chain_Decl:
			resolve_types_chain_decl(table, d)
		case ^ast.Types_Struct_Decl:
			resolve_types_struct_decl(table, d)
		case ^ast.Form_Decl:
			resolve_form_decl(table, d)
		}
	}

	return table
}

// DATA(var) = expression.
// Type is inferred from the expression.
// is_global indicates if this is at file/global scope (where shadowing may be allowed)
resolve_inline_decl :: proc(table: ^SymbolTable, decl: ^ast.Data_Inline_Decl, is_global: bool = true) {
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
	// Global DATA variables may allow shadowing (to be confirmed),
	// but local variables (in FORM etc.) should not allow duplicates
	add_symbol(table, sym, allow_shadowing = is_global)
}

// DATA var TYPE typename.
// Type is explicitly specified.
// is_global indicates if this is at file/global scope (where shadowing may be allowed)
resolve_typed_decl :: proc(table: ^SymbolTable, decl: ^ast.Data_Typed_Decl, is_chained: bool, is_global: bool = true) {
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
	// Global DATA variables may allow shadowing (to be confirmed),
	// but local variables (in FORM etc.) should not allow duplicates
	add_symbol(table, sym, allow_shadowing = is_global)
}

// DATA: var1 TYPE t1, var2 TYPE t2, ...
// Chain of typed declarations.
// is_global indicates if this is at file/global scope (where shadowing may be allowed)
resolve_chain_decl :: proc(table: ^SymbolTable, chain: ^ast.Data_Typed_Chain_Decl, is_global: bool = true) {
	for decl in chain.decls {
		resolve_typed_decl(table, decl, true, is_global)
	}
}

// TYPES ty_name TYPE typename.
// Type definition/alias.
// is_global indicates if this is at file/global scope (where shadowing may be allowed)
resolve_types_decl :: proc(table: ^SymbolTable, decl: ^ast.Types_Decl, is_chained: bool, is_global: bool = true) {
	name := decl.ident.name
	
	// Extract type from the typed expression
	type_info := resolve_type_expr(table, decl.typed)
	
	sym := Symbol {
		name       = name,
		kind       = .TypeDef,
		range      = decl.ident.range,
		type_info  = type_info,
		is_chained = is_chained,
	}
	// Type names should generally not allow duplicates
	add_symbol(table, sym, allow_shadowing = false)
}

// TYPES: ty1 TYPE t1, ty2 TYPE t2, ...
// Chain of type definitions.
// is_global indicates if this is at file/global scope (where shadowing may be allowed)
resolve_types_chain_decl :: proc(table: ^SymbolTable, chain: ^ast.Types_Chain_Decl, is_global: bool = true) {
	for decl in chain.decls {
		resolve_types_decl(table, decl, true, is_global)
	}
}

// TYPES: BEGIN OF struct_name, ... END OF struct_name.
// Resolves a structured type definition.
resolve_types_struct_decl :: proc(table: ^SymbolTable, struct_decl: ^ast.Types_Struct_Decl) {
	name := struct_decl.ident.name
	
	// Create a structure type
	struct_type := make_structure_type(table, name)
	
	// Resolve each component (field or nested structure)
	resolve_struct_components(table, struct_type, struct_decl.components[:])
	
	sym := Symbol {
		name       = name,
		kind       = .TypeDef,
		range      = struct_decl.ident.range,
		type_info  = struct_type,
		is_chained = false,
	}
	add_symbol(table, sym, allow_shadowing = false)
}

// Recursively resolves structure components (fields and nested structures)
resolve_struct_components :: proc(table: ^SymbolTable, struct_type: ^Type, components: []^ast.Stmt) {
	for comp in components {
		#partial switch c in comp.derived_stmt {
		case ^ast.Types_Decl:
			// Regular field
			field_type := resolve_type_expr(table, c.typed)
			
			// Parse length if present
			length_val := 0
			if c.length != nil {
				if lit, ok := c.length.derived_expr.(^ast.Basic_Lit); ok {
					// Parse the number from the literal
					for ch in lit.tok.lit {
						if ch >= '0' && ch <= '9' {
							length_val = length_val * 10 + int(ch - '0')
						}
					}
				}
			}
			field_type.length = length_val
			
			add_struct_field(struct_type, c.ident.name, field_type, length_val)
			
		case ^ast.Types_Struct_Decl:
			// Nested structure - create a nested structure type
			nested_type := make_structure_type(table, c.ident.name)
			resolve_struct_components(table, nested_type, c.components[:])
			add_struct_field(struct_type, c.ident.name, nested_type, 0)
		}
	}
}

// FORM formname [TABLES ...] [USING ...] [CHANGING ...].
// Resolves a FORM subroutine declaration and its parameters.
resolve_form_decl :: proc(table: ^SymbolTable, form: ^ast.Form_Decl) {
	name := form.ident.name
	
	// Create child scope for form's local symbols (parameters + locals)
	child_table := new(SymbolTable)
	child_table.symbols = make(map[string]Symbol)
	child_table.types = make([dynamic]^Type)
	child_table.diagnostics = make([dynamic]Diagnostic)
	
	// Resolve TABLES parameters
	for param in form.tables_params {
		resolve_form_param(child_table, param, .Tables)
	}
	
	// Resolve USING parameters
	for param in form.using_params {
		resolve_form_param(child_table, param, .Using)
	}
	
	// Resolve CHANGING parameters
	for param in form.changing_params {
		resolve_form_param(child_table, param, .Changing)
	}
	
	// Resolve local declarations in the body (is_global=false: no shadowing allowed)
	for stmt in form.body {
		#partial switch s in stmt.derived_stmt {
		case ^ast.Data_Inline_Decl:
			resolve_inline_decl(child_table, s, is_global = false)
		case ^ast.Data_Typed_Decl:
			resolve_typed_decl(child_table, s, false, is_global = false)
		case ^ast.Data_Typed_Chain_Decl:
			resolve_chain_decl(child_table, s, is_global = false)
		}
	}
	
	// Create the form symbol with its child scope
	// FORM names should not be duplicated
	sym := Symbol {
		name        = name,
		kind        = .Form,
		range       = form.ident.range,
		type_info   = nil,  // Forms don't have a return type in ABAP
		child_scope = child_table,
	}
	add_symbol(table, sym, allow_shadowing = false)
}

// Resolves a single FORM parameter (TABLES, USING, or CHANGING).
// Parameters should not allow duplicates.
resolve_form_param :: proc(table: ^SymbolTable, param: ^ast.Form_Param, param_kind: FormParamKind) {
	name := param.ident.name
	
	// Resolve parameter type if specified
	type_info: ^Type
	if param.typed != nil {
		type_info = resolve_type_expr(table, param.typed)
	} else {
		type_info = make_unknown_type(table)
	}
	
	sym := Symbol {
		name            = name,
		kind            = .FormParameter,
		range           = param.ident.range,
		type_info       = type_info,
		form_param_kind = param_kind,
	}
	add_symbol(table, sym, allow_shadowing = false)
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
// Input is normalized to uppercase for case-insensitive matching.
builtin_type_from_name :: proc(name: string) -> TypeKind {
	upper_name := strings.to_lower(name, context.temp_allocator)
	switch upper_name {
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
