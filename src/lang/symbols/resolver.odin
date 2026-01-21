package lang_symbols

import "../ast"
import "../lexer"
import "core:strings"

// Callback type for include resolution during project resolution
// Returns the AST for the include file, or nil if not found
Include_Resolver :: #type proc(include_name: string) -> ^ast.File

// ProjectResolutionResult contains the symbol tables for each file in the project
// after processing includes as text inclusions (like C/C++)
ProjectResolutionResult :: struct {
	// Symbol tables keyed by URI - each file gets its own symbol table
	// that reflects the state AFTER processing that file (including accumulated symbols)
	file_tables: map[string]^SymbolTable,
	// The final merged symbol table after processing all files
	merged_table: ^SymbolTable,
}

// resolve_project_files resolves symbols for a project treating INCLUDEs as text inclusions.
// This implements the "State Cloning" pattern:
// 1) Start with empty Symbol Table
// 2) Process Root file declarations until INCLUDE
// 3) Clone ST -> Pass to Include file
// 4) Include file adds its symbols
// 5) Save Resulting ST for Include file
// 6) Continue in Root with the Resulting ST (accumulated symbols)
//
// Parameters:
// - root_file: The AST of the root file (e.g., REPORT)
// - root_uri: URI of the root file
// - include_resolver: Callback to get AST for include files by name
// - include_uris: Map from include name (lowercase) to URI
resolve_project_files :: proc(
	root_file: ^ast.File,
	root_uri: string,
	include_resolver: Include_Resolver,
	include_uris: map[string]string,
	allocator := context.allocator,
) -> ^ProjectResolutionResult {
	result := new(ProjectResolutionResult, allocator)
	result.file_tables = make(map[string]^SymbolTable, allocator)
	
	// Start with empty symbol table
	current_table := create_empty_symbol_table(allocator)
	
	// Process root file with include handling
	if root_file != nil {
		resolve_file_with_includes(
			root_file,
			root_uri,
			current_table,
			include_resolver,
			include_uris,
			result,
			allocator,
		)
	}
	
	// Store the final table for the root file and as merged table
	result.file_tables[root_uri] = current_table
	result.merged_table = current_table
	
	// Run validation on root file with the merged table
	validate_file(root_file, current_table)
	
	// Run validation on each include file with the merged table for lookups
	// but their own table for diagnostics
	for uri, table in result.file_tables {
		if uri != root_uri {
			// Get the AST for this include file
			// We need to extract include name from URI - this is a bit awkward
			// For now, we validate during resolution
		}
	}
	
	return result
}

// resolve_file_with_includes processes a file's declarations, handling INCLUDEs inline
resolve_file_with_includes :: proc(
	file: ^ast.File,
	file_uri: string,
	table: ^SymbolTable,
	include_resolver: Include_Resolver,
	include_uris: map[string]string,
	result: ^ProjectResolutionResult,
	allocator := context.allocator,
) {
	if file == nil || table == nil {
		return
	}
	
	for decl in file.decls {
		#partial switch d in decl.derived_stmt {
		case ^ast.Include_Decl:
			// Process include: clone current state, resolve include, continue with result
			if d.name != nil {
				include_name := strings.to_lower(d.name.name, context.temp_allocator)
				
				// Get include file AST
				include_ast := include_resolver(include_name)
				if include_ast != nil {
					// Get include URI from our map
					include_uri := include_uris[include_name] if include_name in include_uris else ""
					
					if include_uri != "" {
						// Clone current symbol table state for include file
						include_table := clone_symbol_table(table, allocator)
						
						// Resolve include file into the cloned table
						resolve_file_with_includes(
							include_ast,
							include_uri,
							include_table,
							include_resolver,
							include_uris,
							result,
							allocator,
						)
						
						// Store the include's resulting symbol table
						result.file_tables[include_uri] = include_table
						
						// Run validation on include file
						// Use merged table for lookups, include_table for diagnostics
						validate_file_with_lookup(include_ast, table, include_table)
						
						// CRITICAL: Merge include's symbols back into current table
						// This is what makes symbols from include visible in the rest of root
						merge_symbols_into(table, include_table)
					}
				}
				
				// Also add the include itself as a symbol
				resolve_include_decl(table, d)
			}
		case:
			// Process normal declaration
			resolve_decl_into(table, decl)
		}
	}
}

// merge_symbols_into copies new symbols from source into target
// This is used to propagate symbols from include files back to the main file
merge_symbols_into :: proc(target: ^SymbolTable, source: ^SymbolTable) {
	if target == nil || source == nil {
		return
	}
	
	for name, sym in source.symbols {
		if name not_in target.symbols {
			target.symbols[name] = sym
		}
	}
	
	// Also merge types
	for t in source.types {
		// Check if type already exists (by pointer)
		found := false
		for existing_t in target.types {
			if existing_t == t {
				found = true
				break
			}
		}
		if !found {
			append(&target.types, t)
		}
	}
}

// resolve_decl_into resolves a single top-level declaration into a symbol table
resolve_decl_into :: proc(table: ^SymbolTable, decl: ^ast.Stmt) {
	if decl == nil {
		return
	}
	
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
	case ^ast.Const_Decl:
		resolve_const_decl(table, d, false)
	case ^ast.Const_Chain_Decl:
		resolve_const_chain_decl(table, d)
	case ^ast.Const_Struct_Decl:
		resolve_const_struct_decl(table, d)
	case ^ast.Data_Struct_Decl:
		resolve_data_struct_decl(table, d)
	case ^ast.Form_Decl:
		resolve_form_decl(table, d)
	case ^ast.Class_Def_Decl:
		resolve_class_def_decl(table, d)
	case ^ast.Class_Impl_Decl:
		resolve_class_impl_decl(table, d)
	case ^ast.Interface_Decl:
		resolve_interface_decl(table, d)
	case ^ast.Report_Decl:
		resolve_report_decl(table, d)
	case ^ast.Include_Decl:
		resolve_include_decl(table, d)
	case ^ast.Event_Block:
		resolve_event_block(table, d)
	case ^ast.Module_Decl:
		resolve_module_decl(table, d)
	case ^ast.Field_Symbol_Decl:
		resolve_field_symbol_decl(table, d, is_global = true)
	case ^ast.Controls_Decl:
		resolve_controls_decl(table, d, is_global = true)
	case ^ast.Controls_Chain_Decl:
		resolve_controls_chain_decl(table, d, is_global = true)
	}
}

// destroy_project_resolution_result frees all resources from project resolution
destroy_project_resolution_result :: proc(result: ^ProjectResolutionResult) {
	if result == nil {
		return
	}
	
	// Note: We don't destroy individual tables here because they share type references
	// and child scopes. The merged_table is also in file_tables.
	// In practice, these tables are allocated in the snapshot's arena and will be
	// freed when the snapshot is released.
	
	delete(result.file_tables)
	free(result)
}

resolve_file :: proc(file: ^ast.File) -> ^SymbolTable {
	table := new(SymbolTable)
	table.symbols = make(map[string]Symbol)
	table.types = make([dynamic]^Type)
	table.diagnostics = make([dynamic]Diagnostic)

	resolve_file_into(file, table)

	// Run semantic validation after symbol resolution (for single-file mode)
	// For multi-file projects, validation is run separately with the merged symbol table
	validate_file(file, table)

	return table
}

// Resolve a file's declarations into an existing symbol table
// Used for multi-file projects where symbols accumulate across files
resolve_file_into :: proc(file: ^ast.File, table: ^SymbolTable) {
	if file == nil || table == nil {
		return
	}

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
		case ^ast.Const_Decl:
			resolve_const_decl(table, d, false)
		case ^ast.Const_Chain_Decl:
			resolve_const_chain_decl(table, d)
		case ^ast.Const_Struct_Decl:
			resolve_const_struct_decl(table, d)
		case ^ast.Data_Struct_Decl:
			resolve_data_struct_decl(table, d)
		case ^ast.Form_Decl:
			resolve_form_decl(table, d)
		case ^ast.Class_Def_Decl:
			resolve_class_def_decl(table, d)
		case ^ast.Class_Impl_Decl:
			resolve_class_impl_decl(table, d)
		case ^ast.Interface_Decl:
			resolve_interface_decl(table, d)
		case ^ast.Report_Decl:
			resolve_report_decl(table, d)
		case ^ast.Include_Decl:
			resolve_include_decl(table, d)
		case ^ast.Event_Block:
			resolve_event_block(table, d)
		case ^ast.Module_Decl:
			resolve_module_decl(table, d)
		case ^ast.Field_Symbol_Decl:
			resolve_field_symbol_decl(table, d, is_global = true)
		case ^ast.Controls_Decl:
			resolve_controls_decl(table, d, is_global = true)
		case ^ast.Controls_Chain_Decl:
			resolve_controls_chain_decl(table, d, is_global = true)
		}
	}
}

resolve_inline_decl :: proc(
	table: ^SymbolTable,
	decl: ^ast.Data_Inline_Decl,
	is_global: bool = true,
) {
	name := decl.ident.name

	type_info := make_inferred_type(table, decl.value)

	sym := Symbol {
		name       = name,
		kind       = .Variable,
		range      = decl.ident.range,
		type_info  = type_info,
		is_chained = false,
	}
	add_symbol(table, sym, allow_shadowing = is_global)
}

resolve_typed_decl :: proc(
	table: ^SymbolTable,
	decl: ^ast.Data_Typed_Decl,
	is_chained: bool,
	is_global: bool = true,
) {
	name := get_decl_name(decl.ident)

	type_info := resolve_type_expr(table, decl.typed)

	sym := Symbol {
		name       = name,
		kind       = .Variable,
		range      = decl.ident.range,
		type_info  = type_info,
		is_chained = is_chained,
	}
	add_symbol(table, sym, allow_shadowing = is_global)
}

resolve_chain_decl :: proc(
	table: ^SymbolTable,
	chain: ^ast.Data_Typed_Chain_Decl,
	is_global: bool = true,
) {
	for decl in chain.decls {
		resolve_typed_decl(table, decl, true, is_global)
	}
}

resolve_types_decl :: proc(
	table: ^SymbolTable,
	decl: ^ast.Types_Decl,
	is_chained: bool,
	is_global: bool = true,
) {
	name := decl.ident.name

	type_info := resolve_type_expr(table, decl.typed)

	sym := Symbol {
		name       = name,
		kind       = .TypeDef,
		range      = decl.ident.range,
		type_info  = type_info,
		is_chained = is_chained,
	}
	add_symbol(table, sym, allow_shadowing = false)
}

resolve_types_chain_decl :: proc(
	table: ^SymbolTable,
	chain: ^ast.Types_Chain_Decl,
	is_global: bool = true,
) {
	for decl in chain.decls {
		resolve_types_decl(table, decl, true, is_global)
	}
}

resolve_types_struct_decl :: proc(table: ^SymbolTable, struct_decl: ^ast.Types_Struct_Decl) {
	name := struct_decl.ident.name

	struct_type := make_structure_type(table, name)

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

// CONSTANTS resolution

resolve_const_decl :: proc(
	table: ^SymbolTable,
	decl: ^ast.Const_Decl,
	is_chained: bool,
	is_global: bool = true,
) {
	name := decl.ident.name

	type_info := resolve_type_expr(table, decl.typed)

	sym := Symbol {
		name       = name,
		kind       = .Constant,
		range      = decl.ident.range,
		type_info  = type_info,
		is_chained = is_chained,
	}
	add_symbol(table, sym, allow_shadowing = is_global)
}

resolve_const_chain_decl :: proc(
	table: ^SymbolTable,
	chain: ^ast.Const_Chain_Decl,
	is_global: bool = true,
) {
	for decl in chain.decls {
		resolve_const_decl(table, decl, true, is_global)
	}
}

resolve_const_struct_decl :: proc(table: ^SymbolTable, struct_decl: ^ast.Const_Struct_Decl) {
	name := struct_decl.ident.name

	struct_type := make_structure_type(table, name)

	resolve_const_struct_components(table, struct_type, struct_decl.components[:])

	sym := Symbol {
		name       = name,
		kind       = .Constant,
		range      = struct_decl.ident.range,
		type_info  = struct_type,
		is_chained = false,
	}
	add_symbol(table, sym, allow_shadowing = false)
}

resolve_const_struct_components :: proc(
	table: ^SymbolTable,
	struct_type: ^Type,
	components: []^ast.Stmt,
) {
	for comp in components {
		#partial switch c in comp.derived_stmt {
		case ^ast.Const_Decl:
			field_type := resolve_type_expr(table, c.typed)
			add_struct_field(struct_type, c.ident.name, field_type, 0)

		case ^ast.Const_Struct_Decl:
			nested_type := make_structure_type(table, c.ident.name)
			resolve_const_struct_components(table, nested_type, c.components[:])
			add_struct_field(struct_type, c.ident.name, nested_type, 0)
		}
	}
}

// DATA structure declaration resolution

resolve_data_struct_decl :: proc(table: ^SymbolTable, struct_decl: ^ast.Data_Struct_Decl) {
	name := struct_decl.ident.name

	struct_type := make_structure_type(table, name)

	resolve_data_struct_components(table, struct_type, struct_decl.components[:])

	sym := Symbol {
		name       = name,
		kind       = .Variable,
		range      = struct_decl.ident.range,
		type_info  = struct_type,
		is_chained = false,
	}
	add_symbol(table, sym, allow_shadowing = false)
}

resolve_data_struct_components :: proc(
	table: ^SymbolTable,
	struct_type: ^Type,
	components: []^ast.Stmt,
) {
	for comp in components {
		#partial switch c in comp.derived_stmt {
		case ^ast.Data_Typed_Decl:
			field_type := resolve_type_expr(table, c.typed)
			add_struct_field(struct_type, get_decl_name(c.ident), field_type, 0)

		case ^ast.Data_Struct_Decl:
			nested_type := make_structure_type(table, c.ident.name)
			resolve_data_struct_components(table, nested_type, c.components[:])
			add_struct_field(struct_type, c.ident.name, nested_type, 0)
		}
	}
}

resolve_struct_components :: proc(
	table: ^SymbolTable,
	struct_type: ^Type,
	components: []^ast.Stmt,
) {
	for comp in components {
		#partial switch c in comp.derived_stmt {
		case ^ast.Types_Decl:
			field_type := resolve_type_expr(table, c.typed)

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
			nested_type := make_structure_type(table, c.ident.name)
			resolve_struct_components(table, nested_type, c.components[:])
			add_struct_field(struct_type, c.ident.name, nested_type, 0)
		}
	}
}

resolve_form_decl :: proc(table: ^SymbolTable, form: ^ast.Form_Decl) {
	name := form.ident.name

	child_table := new(SymbolTable)
	child_table.symbols = make(map[string]Symbol)
	child_table.types = make([dynamic]^Type)
	child_table.diagnostics = make([dynamic]Diagnostic)

	for param in form.tables_params {
		resolve_form_param(child_table, param, .Tables)
	}

	for param in form.using_params {
		resolve_form_param(child_table, param, .Using)
	}

	for param in form.changing_params {
		resolve_form_param(child_table, param, .Changing)
	}

	resolve_stmt_list(child_table, form.body[:])

	sym := Symbol {
		name        = name,
		kind        = .Form,
		range       = form.ident.range,
		type_info   = nil,
		child_scope = child_table,
	}
	add_symbol(table, sym, allow_shadowing = false)
}

resolve_form_param :: proc(
	table: ^SymbolTable,
	param: ^ast.Form_Param,
	param_kind: FormParamKind,
) {
	name := param.ident.name

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

resolve_type_expr :: proc(table: ^SymbolTable, expr: ^ast.Expr) -> ^Type {
	if expr == nil {
		return make_unknown_type(table)
	}

	#partial switch e in expr.derived_expr {
	case ^ast.Ident:
		type_kind := builtin_type_from_name(e.name)
		if type_kind != .Unknown {
			t := make_type(table, type_kind)
			t.ast_node = expr
			return t
		}
		return make_named_type(table, e.name, expr)

	case ^ast.Table_Type:
		elem_type := resolve_type_expr(table, e.elem)
		table_kind: TableTypeKind
		switch e.kind {
		case .Standard:
			table_kind = .Standard
		case .Sorted:
			table_kind = .Sorted
		case .Hashed:
			table_kind = .Hashed
		case .Any:
			table_kind = .Any
		}
		t := make_table_type(table, elem_type, table_kind)
		t.ast_node = expr
		// Copy key information
		if e.primary_key != nil {
			key_info := make_table_key_info(
				table,
				e.primary_key.is_unique,
				e.primary_key.is_default,
			)
			for comp in e.primary_key.components {
				add_key_component(key_info, comp.name)
			}
			t.primary_key = key_info
		}
		return t

	case ^ast.Ref_Type:
		target_type := resolve_type_expr(table, e.target)
		t := make_reference_type(table, target_type)
		t.ast_node = expr
		return t

	case ^ast.Line_Type:
		table_type := resolve_type_expr(table, e.table)
		t := make_line_of_type(table, table_type)
		t.ast_node = expr
		return t

	case ^ast.Selector_Expr:
		return make_named_type(table, selector_to_string(e), expr)

	case ^ast.New_Expr:
		// For NEW expressions, the type is either explicit or inferred
		if e.is_inferred {
			// Type is inferred from context (NEW #(...))
			return make_inferred_type(table, expr)
		} else if e.type_expr != nil {
			// Type is explicitly specified (NEW type(...))
			target_type := resolve_type_expr(table, e.type_expr)
			return make_reference_type(table, target_type)
		}
		return make_unknown_type(table)

	case ^ast.Call_Expr:
		// For call expressions, we would need to resolve the return type of the method
		// For now, return unknown type as we need more context to resolve method return types
		return make_unknown_type(table)

	case ^ast.String_Template_Expr:
		// String templates always result in a string type
		return make_type(table, .StringTemplate)

	case ^ast.Binary_Expr:
		// Check if this is a string concatenation
		if e.op.kind == .Ampersand {
			// String concatenation results in a string
			return make_type(table, .String)
		}
		// Check for arithmetic operations
		if e.op.kind == .Plus || e.op.kind == .Minus || e.op.kind == .Star || e.op.kind == .Slash {
			// Arithmetic operations - try to infer from operands
			left_type := resolve_type_expr(table, e.left)
			right_type := resolve_type_expr(table, e.right)

			// If both are numeric types, result is numeric
			if is_numeric_type(left_type) && is_numeric_type(right_type) {
				// Division always returns float
				if e.op.kind == .Slash {
					return make_type(table, .Float)
				}
				// If either is float, result is float
				if left_type.kind == .Float || right_type.kind == .Float {
					return make_type(table, .Float)
				}
				return make_type(table, .Integer)
			}
			// Default to numeric type for arithmetic
			return make_type(table, .Integer)
		}
		// Check for MOD/DIV keyword operators
		if e.op.kind == .Ident {
			op_upper := strings.to_upper(e.op.lit, context.temp_allocator)
			if op_upper == "MOD" || op_upper == "DIV" {
				return make_type(table, .Integer)
			}
		}
		return make_unknown_type(table)

	case ^ast.Paren_Expr:
		// Parenthesized expression has the type of its inner expression
		return resolve_type_expr(table, e.expr)

	case ^ast.Constructor_Expr:
		// For VALUE, COND, SWITCH, etc. constructor expressions
		if e.is_inferred {
			return make_inferred_type(table, expr)
		} else if e.type_expr != nil {
			return resolve_type_expr(table, e.type_expr)
		}
		return make_unknown_type(table)

	case ^ast.For_Expr:
		// FOR expressions produce elements of the iterable's type
		if e.itab != nil {
			return make_inferred_type(table, e.itab)
		}
		return make_unknown_type(table)
	}

	return make_unknown_type(table)
}

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

selector_to_string :: proc(sel: ^ast.Selector_Expr) -> string {
	if sel.field != nil {
		return sel.field.name
	}
	return ""
}

get_decl_name :: proc(expr: ^ast.Expr) -> string {
	if expr == nil {
		return ""
	}
	#partial switch e in expr.derived_expr {
	case ^ast.Ident:
		return e.name
	case ^ast.Selector_Expr:
		// Build name: lhs-rhs
		left := get_decl_name(e.expr)
		right := ""
		if e.field != nil {
			right = e.field.name
		}
		if left == "" {
			return right
		}
		// Allocate on temp allocator since this string needs to persist only for symbol creation
		return strings.concatenate({left, "-", right}, context.temp_allocator)
	}
	return ""
}

resolve_class_def_decl :: proc(table: ^SymbolTable, class_def: ^ast.Class_Def_Decl) {
	name := class_def.ident.name

	child_table := new(SymbolTable)
	child_table.symbols = make(map[string]Symbol)
	child_table.types = make([dynamic]^Type)
	child_table.diagnostics = make([dynamic]Diagnostic)

	for section in class_def.sections {
		resolve_class_section(child_table, section)
	}

	class_type := make_type(table, .Named)
	class_type.name = strings.to_lower(name)

	sym := Symbol {
		name        = name,
		kind        = .Class,
		range       = class_def.ident.range,
		type_info   = class_type,
		child_scope = child_table,
	}
	add_symbol(table, sym, allow_shadowing = false)
}

resolve_class_section :: proc(table: ^SymbolTable, section: ^ast.Class_Section) {
	// Map AST access modifier to symbol visibility
	visibility := access_to_visibility(section.access)

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

	for data_decl in section.data {
		#partial switch d in data_decl.derived_stmt {
		case ^ast.Attr_Decl:
			resolve_attr_decl(table, d, visibility)
		case ^ast.Data_Typed_Decl:
			resolve_typed_decl(table, d, false, false)
		case ^ast.Data_Typed_Chain_Decl:
			resolve_chain_decl(table, d, false)
		}
	}

	for method_decl in section.methods {
		#partial switch m in method_decl.derived_stmt {
		case ^ast.Method_Decl:
			resolve_method_decl(table, m, visibility)
		case ^ast.Method_Chain_Decl:
			for decl in m.decls {
				resolve_method_decl(table, decl, visibility)
			}
		}
	}

	for iface_decl in section.interfaces {
		#partial switch i in iface_decl.derived_stmt {
		case ^ast.Interfaces_Decl:
		}
	}
}

access_to_visibility :: proc(access: ast.Access_Modifier) -> Visibility {
	switch access {
	case .Public:
		return .Public
	case .Protected:
		return .Protected
	case .Private:
		return .Private
	}
	return .None
}

resolve_attr_decl :: proc(table: ^SymbolTable, attr: ^ast.Attr_Decl, visibility: Visibility = .None) {
	name := attr.ident.name

	type_info := resolve_type_expr(table, attr.typed)

	sym := Symbol {
		name       = name,
		kind       = .Field,
		range      = attr.ident.range,
		type_info  = type_info,
		visibility = visibility,
		is_static  = attr.is_class,
	}
	add_symbol(table, sym, allow_shadowing = false)
}

resolve_method_decl :: proc(table: ^SymbolTable, method: ^ast.Method_Decl, visibility: Visibility = .None) {
	name := method.ident.name

	child_table := new(SymbolTable)
	child_table.symbols = make(map[string]Symbol)
	child_table.types = make([dynamic]^Type)
	child_table.diagnostics = make([dynamic]Diagnostic)

	for param in method.params {
		resolve_method_param(child_table, param)
	}

	sym := Symbol {
		name        = name,
		kind        = .Method,
		range       = method.ident.range,
		type_info   = nil,
		child_scope = child_table,
		visibility  = visibility,
		is_static   = .Class in method.flags,
	}
	add_symbol(table, sym, allow_shadowing = false)
}

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

resolve_class_impl_decl :: proc(table: ^SymbolTable, class_impl: ^ast.Class_Impl_Decl) {
	for method in class_impl.methods {
		#partial switch m in method.derived_stmt {
		case ^ast.Method_Impl:
			resolve_method_impl(table, m)
		}
	}
}

resolve_method_impl :: proc(table: ^SymbolTable, method_impl: ^ast.Method_Impl) {
	child_table := new(SymbolTable)
	child_table.symbols = make(map[string]Symbol)
	child_table.types = make([dynamic]^Type)
	child_table.diagnostics = make([dynamic]Diagnostic)

	resolve_stmt_list(child_table, method_impl.body[:])
}

// resolve_stmt_list resolves all statements in a list, recursively handling control structures
resolve_stmt_list :: proc(table: ^SymbolTable, stmts: []^ast.Stmt) {
	for stmt in stmts {
		resolve_stmt(table, stmt)
	}
}

// resolve_stmt resolves declarations in a single statement
resolve_stmt :: proc(table: ^SymbolTable, stmt: ^ast.Stmt) {
	if stmt == nil {
		return
	}

	#partial switch s in stmt.derived_stmt {
	case ^ast.Data_Inline_Decl:
		resolve_inline_decl(table, s, is_global = false)
	case ^ast.Data_Typed_Decl:
		resolve_typed_decl(table, s, false, is_global = false)
	case ^ast.Data_Typed_Chain_Decl:
		resolve_chain_decl(table, s, is_global = false)
	case ^ast.Const_Decl:
		resolve_const_decl(table, s, false, is_global = false)
	case ^ast.Const_Chain_Decl:
		resolve_const_chain_decl(table, s, is_global = false)
	case ^ast.Const_Struct_Decl:
		resolve_const_struct_decl(table, s)
	case ^ast.Data_Struct_Decl:
		resolve_data_struct_decl(table, s)
	case ^ast.Field_Symbol_Decl:
		resolve_field_symbol_decl(table, s, is_global = false)
	case ^ast.If_Stmt:
		resolve_if_stmt(table, s)
	case ^ast.Case_Stmt:
		resolve_case_stmt(table, s)
	case ^ast.While_Stmt:
		resolve_while_stmt(table, s)
	case ^ast.Loop_Stmt:
		resolve_loop_stmt(table, s)
	case ^ast.Read_Table_Stmt:
		resolve_read_table_stmt(table, s)
	case ^ast.Call_Function_Stmt:
		resolve_call_function_stmt(table, s)
	case ^ast.Select_Stmt:
		resolve_select_stmt(table, s)
	}
}

resolve_if_stmt :: proc(table: ^SymbolTable, if_stmt: ^ast.If_Stmt) {
	resolve_stmt_list(table, if_stmt.body[:])

	for branch in if_stmt.elseif_branches {
		resolve_stmt_list(table, branch.body[:])
	}

	resolve_stmt_list(table, if_stmt.else_body[:])
}

resolve_interface_decl :: proc(table: ^SymbolTable, iface: ^ast.Interface_Decl) {
	name := iface.ident.name

	child_table := new(SymbolTable)
	child_table.symbols = make(map[string]Symbol)
	child_table.types = make([dynamic]^Type)
	child_table.diagnostics = make([dynamic]Diagnostic)

	// Interface members are implicitly public
	for method_decl in iface.methods {
		#partial switch m in method_decl.derived_stmt {
		case ^ast.Method_Decl:
			resolve_method_decl(child_table, m, .Public)
		case ^ast.Method_Chain_Decl:
			for decl in m.decls {
				resolve_method_decl(child_table, decl, .Public)
			}
		}
	}

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

	for data_decl in iface.data {
		#partial switch d in data_decl.derived_stmt {
		case ^ast.Attr_Decl:
			resolve_attr_decl(child_table, d, .Public)
		case ^ast.Data_Typed_Decl:
			resolve_typed_decl(child_table, d, false, false)
		}
	}

	iface_type := make_type(table, .Named)
	iface_type.name = strings.to_lower(name)

	sym := Symbol {
		name        = name,
		kind        = .Interface,
		range       = iface.ident.range,
		type_info   = iface_type,
		child_scope = child_table,
	}
	add_symbol(table, sym, allow_shadowing = false)
}

resolve_report_decl :: proc(table: ^SymbolTable, report: ^ast.Report_Decl) {
	if report.name == nil {
		return
	}
	name := report.name.name

	sym := Symbol {
		name      = name,
		kind      = .Report,
		range     = report.name.range,
		type_info = nil,
	}
	add_symbol(table, sym, allow_shadowing = false)
}

resolve_include_decl :: proc(table: ^SymbolTable, include: ^ast.Include_Decl) {
	if include.name == nil {
		return
	}
	name := include.name.name

	sym := Symbol {
		name      = name,
		kind      = .Include,
		range     = include.name.range,
		type_info = nil,
	}
	add_symbol(table, sym, allow_shadowing = true) // Allow shadowing for includes
}

resolve_event_block :: proc(table: ^SymbolTable, event: ^ast.Event_Block) {
	// Create a child scope for the event block's local variables
	child_table := new(SymbolTable)
	child_table.symbols = make(map[string]Symbol)
	child_table.types = make([dynamic]^Type)
	child_table.diagnostics = make([dynamic]Diagnostic)

	// Resolve declarations in the event body
	resolve_stmt_list(child_table, event.body[:])

	// Create a symbol for the event with a generated name based on kind
	event_name := get_event_name(event.kind)

	sym := Symbol {
		name        = event_name,
		kind        = .Event,
		range       = event.range,
		type_info   = nil,
		child_scope = child_table,
	}
	add_symbol(table, sym, allow_shadowing = true)
}

get_event_name :: proc(kind: ast.Event_Kind) -> string {
	switch kind {
	case .StartOfSelection:
		return "start-of-selection"
	case .EndOfSelection:
		return "end-of-selection"
	case .Initialization:
		return "initialization"
	case .AtSelectionScreen:
		return "at-selection-screen"
	case .TopOfPage:
		return "top-of-page"
	case .EndOfPage:
		return "end-of-page"
	}
	return "unknown-event"
}

resolve_module_decl :: proc(table: ^SymbolTable, module: ^ast.Module_Decl) {
	if module.ident == nil {
		return
	}
	name := module.ident.name

	// Create a child scope for the module's local variables
	child_table := new(SymbolTable)
	child_table.symbols = make(map[string]Symbol)
	child_table.types = make([dynamic]^Type)
	child_table.diagnostics = make([dynamic]Diagnostic)

	// Resolve declarations in the module body
	resolve_stmt_list(child_table, module.body[:])

	sym := Symbol {
		name        = name,
		kind        = .Module,
		range       = module.ident.range,
		type_info   = nil,
		child_scope = child_table,
	}
	add_symbol(table, sym, allow_shadowing = true)
}

resolve_field_symbol_decl :: proc(
	table: ^SymbolTable,
	fs_decl: ^ast.Field_Symbol_Decl,
	is_global: bool = true,
) {
	if fs_decl.ident == nil {
		return
	}
	name := fs_decl.ident.name

	type_info: ^Type
	if fs_decl.typed != nil {
		type_info = resolve_type_expr(table, fs_decl.typed)
	} else {
		type_info = make_unknown_type(table)
	}

	sym := Symbol {
		name      = name,
		kind      = .FieldSymbol,
		range     = fs_decl.ident.range,
		type_info = type_info,
	}
	add_symbol(table, sym, allow_shadowing = is_global)
}

resolve_case_stmt :: proc(table: ^SymbolTable, case_stmt: ^ast.Case_Stmt) {
	for branch in case_stmt.branches {
		resolve_stmt_list(table, branch.body[:])
	}
}

resolve_while_stmt :: proc(table: ^SymbolTable, while_stmt: ^ast.While_Stmt) {
	resolve_stmt_list(table, while_stmt.body[:])
}

resolve_loop_stmt :: proc(table: ^SymbolTable, loop_stmt: ^ast.Loop_Stmt) {
	// Handle inline DATA declaration in INTO clause
	if loop_stmt.into_target != nil {
		// Check if into_target is from an inline DATA declaration
		// The into_target will be an Ident from the inline DATA parsing
		if ident, ok := loop_stmt.into_target.derived_expr.(^ast.Ident); ok {
			// Create inferred type from the loop table
			type_info := make_inferred_type(table, loop_stmt.itab)

			sym := Symbol {
				name      = ident.name,
				kind      = .Variable,
				range     = ident.range,
				type_info = type_info,
			}
			add_symbol(table, sym, allow_shadowing = false)
		}
	}

	// Handle inline FIELD-SYMBOL declaration in ASSIGNING clause
	if loop_stmt.assigning_target != nil {
		if ident, ok := loop_stmt.assigning_target.derived_expr.(^ast.Ident); ok {
			// Field symbols get the line type of the internal table
			type_info := make_inferred_type(table, loop_stmt.itab)

			sym := Symbol {
				name      = ident.name,
				kind      = .FieldSymbol,
				range     = ident.range,
				type_info = type_info,
			}
			add_symbol(table, sym, allow_shadowing = false)
		}
	}

	// Resolve statements in the loop body
	resolve_stmt_list(table, loop_stmt.body[:])
}

// is_numeric_type checks if a type is a numeric type (integer, float, numeric)
is_numeric_type :: proc(t: ^Type) -> bool {
	if t == nil {
		return false
	}
	#partial switch t.kind {
	case .Integer, .Float, .Numeric:
		return true
	}
	return false
}

resolve_read_table_stmt :: proc(table: ^SymbolTable, read_stmt: ^ast.Read_Table_Stmt) {
	// Handle inline DATA declaration in INTO clause
	if read_stmt.into_target != nil {
		// Check if into_target is from an inline DATA declaration
		if ident, ok := read_stmt.into_target.derived_expr.(^ast.Ident); ok {
			// Create inferred type from the internal table (line type)
			type_info := make_inferred_type(table, read_stmt.itab)

			sym := Symbol {
				name      = ident.name,
				kind      = .Variable,
				range     = ident.range,
				type_info = type_info,
			}
			add_symbol(table, sym, allow_shadowing = false)
		}
	}

	// Handle inline FIELD-SYMBOL declaration in ASSIGNING clause
	if read_stmt.assigning_target != nil {
		if ident, ok := read_stmt.assigning_target.derived_expr.(^ast.Ident); ok {
			// Field symbols get the line type of the internal table
			type_info := make_inferred_type(table, read_stmt.itab)

			sym := Symbol {
				name      = ident.name,
				kind      = .FieldSymbol,
				range     = ident.range,
				type_info = type_info,
			}
			add_symbol(table, sym, allow_shadowing = false)
		}
	}
}

resolve_call_function_stmt :: proc(table: ^SymbolTable, call_func: ^ast.Call_Function_Stmt) {
	// CALL FUNCTION doesn't typically introduce new symbols itself,
	// but we need to check for any inline declarations in parameter values
	// (e.g., DATA(lv_result) could theoretically appear in an importing parameter)

	// Check importing parameters for inline declarations
	for param in call_func.importing {
		if param.value != nil {
			resolve_param_value_decl(table, param.value)
		}
	}

	// Check changing parameters for inline declarations
	for param in call_func.changing {
		if param.value != nil {
			resolve_param_value_decl(table, param.value)
		}
	}

	// Check tables parameters for inline declarations
	for param in call_func.tables {
		if param.value != nil {
			resolve_param_value_decl(table, param.value)
		}
	}
}

resolve_param_value_decl :: proc(table: ^SymbolTable, expr: ^ast.Expr) {
	if expr == nil {
		return
	}

	// Check if this is an inline DATA declaration
	if ident, ok := expr.derived_expr.(^ast.Ident); ok {
		// Could be a simple variable reference, nothing to declare
		return
	}

	// For now, we don't handle inline declarations in CALL FUNCTION parameters
	// as they are quite rare. This can be extended if needed.
}

resolve_select_stmt :: proc(table: ^SymbolTable, select_stmt: ^ast.Select_Stmt) {
	// Handle inline DATA declaration in INTO clause
	if select_stmt.into_target != nil {
		// Check if into_target is from an inline DATA declaration
		if ident, ok := select_stmt.into_target.derived_expr.(^ast.Ident); ok {
			// Create inferred type - SELECT result type would be inferred from context
			type_info := make_unknown_type(table)

			sym := Symbol {
				name      = ident.name,
				kind      = .Variable,
				range     = ident.range,
				type_info = type_info,
			}
			add_symbol(table, sym, allow_shadowing = false)
		}
	}

	// Resolve statements in the SELECT loop body (for non-SINGLE selects)
	resolve_stmt_list(table, select_stmt.body[:])
}

// resolve_controls_decl resolves a CONTROLS declaration
resolve_controls_decl :: proc(
	table: ^SymbolTable,
	controls_decl: ^ast.Controls_Decl,
	is_global: bool = true,
) {
	if controls_decl.ident == nil {
		return
	}
	name := controls_decl.ident.name

	// Controls don't have a traditional type, but we can create a named type
	type_info := make_unknown_type(table)

	sym := Symbol {
		name      = name,
		kind      = .Control,
		range     = controls_decl.ident.range,
		type_info = type_info,
	}
	add_symbol(table, sym, allow_shadowing = is_global)
}

// resolve_controls_chain_decl resolves a chained CONTROLS declaration
resolve_controls_chain_decl :: proc(
	table: ^SymbolTable,
	chain: ^ast.Controls_Chain_Decl,
	is_global: bool = true,
) {
	for decl in chain.decls {
		resolve_controls_decl(table, decl, is_global)
	}
}
