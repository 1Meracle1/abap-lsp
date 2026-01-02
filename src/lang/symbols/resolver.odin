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
		case ^ast.Class_Def_Decl:
			resolve_class_def_decl(table, d)
		case ^ast.Class_Impl_Decl:
			resolve_class_impl_decl(table, d)
		case ^ast.Interface_Decl:
			resolve_interface_decl(table, d)
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

// ============================================================================
// CLASS and INTERFACE resolution
// ============================================================================

// Resolves a CLASS DEFINITION declaration.
resolve_class_def_decl :: proc(table: ^SymbolTable, class_def: ^ast.Class_Def_Decl) {
	name := class_def.ident.name
	
	// Create child scope for class members
	child_table := new(SymbolTable)
	child_table.symbols = make(map[string]Symbol)
	child_table.types = make([dynamic]^Type)
	child_table.diagnostics = make([dynamic]Diagnostic)
	
	// Resolve each section
	for section in class_def.sections {
		resolve_class_section(child_table, section)
	}
	
	// Create the class type
	class_type := make_type(table, .Named)
	class_type.name = strings.to_lower(name)
	
	// Create the class symbol
	sym := Symbol {
		name        = name,
		kind        = .Class,
		range       = class_def.ident.range,
		type_info   = class_type,
		child_scope = child_table,
	}
	add_symbol(table, sym, allow_shadowing = false)
}

// Resolves a class section (PUBLIC/PROTECTED/PRIVATE SECTION)
resolve_class_section :: proc(table: ^SymbolTable, section: ^ast.Class_Section) {
	// Resolve types
	for type_decl in section.types {
		#partial switch t in type_decl.derived_stmt {
		case ^ast.Types_Decl:
			resolve_types_decl(table, t, false, false)
		case ^ast.Types_Chain_Decl:
			resolve_types_chain_decl(table, t, false)
		case ^ast.Types_Struct_Decl:
			resolve_types_struct_decl(table, t)
		}
	}
	
	// Resolve data/attributes
	for data_decl in section.data {
		#partial switch d in data_decl.derived_stmt {
		case ^ast.Attr_Decl:
			resolve_attr_decl(table, d)
		case ^ast.Data_Typed_Decl:
			resolve_typed_decl(table, d, false, false)
		case ^ast.Data_Typed_Chain_Decl:
			resolve_chain_decl(table, d, false)
		}
	}
	
	// Resolve methods
	for method_decl in section.methods {
		#partial switch m in method_decl.derived_stmt {
		case ^ast.Method_Decl:
			resolve_method_decl(table, m)
		}
	}
	
	// Resolve interfaces (INTERFACES keyword)
	for iface_decl in section.interfaces {
		#partial switch i in iface_decl.derived_stmt {
		case ^ast.Interfaces_Decl:
			// Note: interface implementations are handled separately
			// For now, just acknowledge they're there
		}
	}
}

// Resolves an attribute declaration (DATA or CLASS-DATA in class)
resolve_attr_decl :: proc(table: ^SymbolTable, attr: ^ast.Attr_Decl) {
	name := attr.ident.name
	
	type_info := resolve_type_expr(table, attr.typed)
	
	sym := Symbol {
		name      = name,
		kind      = .Field,
		range     = attr.ident.range,
		type_info = type_info,
	}
	add_symbol(table, sym, allow_shadowing = false)
}

// Resolves a method declaration (METHODS or CLASS-METHODS)
resolve_method_decl :: proc(table: ^SymbolTable, method: ^ast.Method_Decl) {
	name := method.ident.name
	
	// Create child scope for method parameters
	child_table := new(SymbolTable)
	child_table.symbols = make(map[string]Symbol)
	child_table.types = make([dynamic]^Type)
	child_table.diagnostics = make([dynamic]Diagnostic)
	
	// Resolve parameters
	for param in method.params {
		resolve_method_param(child_table, param)
	}
	
	sym := Symbol {
		name        = name,
		kind        = .Method,
		range       = method.ident.range,
		type_info   = nil, // Methods don't have a simple type
		child_scope = child_table,
	}
	add_symbol(table, sym, allow_shadowing = false)
}

// Resolves a method parameter
resolve_method_param :: proc(table: ^SymbolTable, param: ^ast.Method_Param) {
	name := param.ident.name
	
	type_info: ^Type
	if param.typed != nil {
		type_info = resolve_type_expr(table, param.typed)
	} else {
		type_info = make_unknown_type(table)
	}
	
	sym := Symbol {
		name      = name,
		kind      = .Parameter,
		range     = param.ident.range,
		type_info = type_info,
	}
	add_symbol(table, sym, allow_shadowing = false)
}

// Resolves a CLASS IMPLEMENTATION declaration.
resolve_class_impl_decl :: proc(table: ^SymbolTable, class_impl: ^ast.Class_Impl_Decl) {
	// For implementation, we primarily need to resolve local symbols in method bodies
	// The class itself should already be registered from its DEFINITION
	
	for method in class_impl.methods {
		#partial switch m in method.derived_stmt {
		case ^ast.Method_Impl:
			resolve_method_impl(table, m)
		}
	}
}

// Resolves a method implementation
resolve_method_impl :: proc(table: ^SymbolTable, method_impl: ^ast.Method_Impl) {
	// Create a local scope for the method body
	// Note: we would ideally link this to the method declaration's scope
	
	child_table := new(SymbolTable)
	child_table.symbols = make(map[string]Symbol)
	child_table.types = make([dynamic]^Type)
	child_table.diagnostics = make([dynamic]Diagnostic)
	
	// Resolve local declarations in the body
	for stmt in method_impl.body {
		#partial switch s in stmt.derived_stmt {
		case ^ast.Data_Inline_Decl:
			resolve_inline_decl(child_table, s, is_global = false)
		case ^ast.Data_Typed_Decl:
			resolve_typed_decl(child_table, s, false, is_global = false)
		case ^ast.Data_Typed_Chain_Decl:
			resolve_chain_decl(child_table, s, is_global = false)
		}
	}
	
	// Get method name for the symbol
	method_name := ""
	#partial switch n in method_impl.ident.derived_expr {
	case ^ast.Ident:
		method_name = n.name
	case ^ast.Selector_Expr:
		// For interface~method, use the full expression as name
		if n.field != nil {
			method_name = n.field.name
		}
	}
	
	// We don't add the method implementation as a new symbol since the method
	// should already exist from the DEFINITION. Just store locals if needed.
}

// Resolves an INTERFACE declaration.
resolve_interface_decl :: proc(table: ^SymbolTable, iface: ^ast.Interface_Decl) {
	name := iface.ident.name
	
	// Create child scope for interface members
	child_table := new(SymbolTable)
	child_table.symbols = make(map[string]Symbol)
	child_table.types = make([dynamic]^Type)
	child_table.diagnostics = make([dynamic]Diagnostic)
	
	// Resolve methods
	for method_decl in iface.methods {
		#partial switch m in method_decl.derived_stmt {
		case ^ast.Method_Decl:
			resolve_method_decl(child_table, m)
		}
	}
	
	// Resolve types
	for type_decl in iface.types {
		#partial switch t in type_decl.derived_stmt {
		case ^ast.Types_Decl:
			resolve_types_decl(child_table, t, false, false)
		case ^ast.Types_Chain_Decl:
			resolve_types_chain_decl(child_table, t, false)
		case ^ast.Types_Struct_Decl:
			resolve_types_struct_decl(child_table, t)
		}
	}
	
	// Resolve data
	for data_decl in iface.data {
		#partial switch d in data_decl.derived_stmt {
		case ^ast.Attr_Decl:
			resolve_attr_decl(child_table, d)
		case ^ast.Data_Typed_Decl:
			resolve_typed_decl(child_table, d, false, false)
		}
	}
	
	// Create the interface type
	iface_type := make_type(table, .Named)
	iface_type.name = strings.to_lower(name)
	
	// Create the interface symbol
	sym := Symbol {
		name        = name,
		kind        = .Interface,
		range       = iface.ident.range,
		type_info   = iface_type,
		child_scope = child_table,
	}
	add_symbol(table, sym, allow_shadowing = false)
}
