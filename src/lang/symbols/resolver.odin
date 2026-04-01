package lang_symbols

import "../ast"
import "../lexer"
import "core:fmt"
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

	syntax_taint := build_syntax_taint_ranges(file, context.temp_allocator)
	
	for decl in file.decls {
		if statement_is_syntax_tainted(decl, syntax_taint) {
			continue
		}
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
			resolve_decl_into_with_syntax_taint(table, decl, syntax_taint)
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
	resolve_decl_into_with_syntax_taint(table, decl, nil)
}

resolve_decl_into_with_syntax_taint :: proc(
	table: ^SymbolTable,
	decl: ^ast.Stmt,
	syntax_taint: []lexer.TextRange,
) {
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
		resolve_form_decl(table, d, syntax_taint)
	case ^ast.Class_Def_Decl:
		resolve_class_def_decl(table, d, syntax_taint)
	case ^ast.Class_Impl_Decl:
		resolve_class_impl_decl(table, d, syntax_taint)
	case ^ast.Interface_Decl:
		resolve_interface_decl(table, d, syntax_taint)
	case ^ast.Report_Decl:
		resolve_report_decl(table, d)
	case ^ast.Include_Decl:
		resolve_include_decl(table, d)
	case ^ast.Event_Block:
		resolve_event_block(table, d, syntax_taint)
	case ^ast.Module_Decl:
		resolve_module_decl(table, d, syntax_taint)
	case ^ast.Field_Symbol_Decl:
		resolve_field_symbol_decl(table, d, is_global = true)
	case ^ast.Field_Symbol_Chain_Decl:
		resolve_field_symbol_chain_decl(table, d, is_global = true)
	case ^ast.Controls_Decl:
		resolve_controls_decl(table, d, is_global = true)
	case ^ast.Controls_Chain_Decl:
		resolve_controls_chain_decl(table, d, is_global = true)
	case:
		// Executable statements at program top-level (INSERT, READ TABLE, LOOP, etc.) live in
		// file.decls like declarations; they must still run through resolve_stmt so inline
		// DATA / FIELD-SYMBOL bindings are registered.
		resolve_stmt(table, decl, syntax_taint)
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
	table := create_empty_symbol_table()

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

	syntax_taint := build_syntax_taint_ranges(file, context.temp_allocator)

	for decl in file.decls {
		if statement_is_syntax_tainted(decl, syntax_taint) {
			continue
		}
		resolve_decl_into_with_syntax_taint(table, decl, syntax_taint)
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
	name := decl_name_from_expr(decl.ident)

	type_info: ^Type
	if decl.is_like {
		type_info = resolve_like_type_expr(table, decl.typed)
	} else {
		type_info = resolve_type_expr(table, decl.typed)
	}

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
	for part in chain.parts {
		#partial switch d in part.derived_stmt {
		case ^ast.Data_Typed_Decl:
			resolve_typed_decl(table, d, true, is_global)
		case ^ast.Data_Struct_Decl:
			resolve_data_struct_decl(table, d)
		}
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
	for part in chain.parts {
		#partial switch d in part.derived_stmt {
		case ^ast.Types_Decl:
			resolve_types_decl(table, d, true, is_global)
		case ^ast.Types_Struct_Decl:
			resolve_types_struct_decl(table, d)
		}
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

// Follow Named typedef symbols until we hit a concrete structure type (or give up).
unwrap_typedef_structure :: proc(table: ^SymbolTable, start: ^Type) -> ^Type {
	if start == nil {
		return nil
	}
	cur := start
	for _ in 0 ..< 16 {
		if cur.kind == .Structure {
			return cur
		}
		if cur.kind != .Named {
			return nil
		}
		name := cur.name
		if sym, ok := table.symbols[name]; ok && sym.kind == .TypeDef && sym.type_info != nil {
			cur = sym.type_info
			continue
		}
		return nil
	}
	return nil
}

// Table row type for `-comp` access: follow Named to typedef, then use table line (element) structure.
named_or_table_row_structure :: proc(table: ^SymbolTable, value_ty: ^Type) -> ^Type {
	if value_ty == nil {
		return nil
	}
	t := value_ty
	if t.kind == .Named {
		if sym, ok := table.symbols[t.name]; ok && sym.kind == .TypeDef && sym.type_info != nil {
			t = sym.type_info
		} else {
			return nil
		}
	}
	if t.kind == .Table && t.elem_type != nil {
		return structure_for_field_lookup(table, t.elem_type)
	}
	return nil
}

// Underlying structure for component access (a-b, a~b, a->b): concrete STRUCTURE, or typedef resolved to one,
// LINE OF table row type, table typed variable (Named → Table), or Inferred row from LOOP/READ/INSERT ASSIGNING.
structure_for_field_lookup :: proc(table: ^SymbolTable, value_ty: ^Type) -> ^Type {
	if value_ty == nil {
		return nil
	}
	if value_ty.kind == .Structure {
		return value_ty
	}
	if value_ty.kind == .Inferred && value_ty.infer_source != nil {
		src_ty := expr_value_type(table, value_ty.infer_source)
		return named_or_table_row_structure(table, src_ty)
	}
	if value_ty.kind == .Named {
		if u := unwrap_typedef_structure(table, value_ty); u != nil {
			return u
		}
		return named_or_table_row_structure(table, value_ty)
	}
	if value_ty.kind == .LineOf && value_ty.target_type != nil {
		return named_or_table_row_structure(table, value_ty.target_type)
	}
	return nil
}

// Type of an expression for validating component selectors. Does not cover all expression forms.
expr_value_type :: proc(table: ^SymbolTable, expr: ^ast.Expr) -> ^Type {
	if expr == nil || table == nil {
		return nil
	}
	#partial switch e in expr.derived_expr {
	case ^ast.Ident:
		name := strings.to_lower(e.name)
		if sym, ok := table.symbols[name]; ok && sym.type_info != nil {
			return sym.type_info
		}
		return nil
	case ^ast.Selector_Expr:
		if e.op.kind != .Minus && e.op.kind != .Tilde && e.op.kind != .Arrow {
			return nil
		}
		base_ty := expr_value_type(table, e.expr)
		struct_ty := structure_for_field_lookup(table, base_ty)
		if struct_ty == nil {
			return nil
		}
		field_name := ast.selector_field_ident_name(e)
		if field_name == "" {
			return nil
		}
		ln := strings.to_lower(field_name)
		for f in struct_ty.fields {
			if f.name == ln {
				return f.type_info
			}
		}
		return nil
	case ^ast.Paren_Expr:
		return expr_value_type(table, e.expr)
	}
	return nil
}

// Flatten INCLUDE TYPE into the parent structure (ABAP: components with optional AS name prefix).
resolve_types_include_into_struct :: proc(
	table: ^SymbolTable,
	struct_type: ^Type,
	inc: ^ast.Types_Include_Type_Decl,
) {
	if inc == nil || inc.included == nil {
		return
	}
	included_ty := resolve_type_expr(table, inc.included)
	concrete := unwrap_typedef_structure(table, included_ty)
	if concrete == nil || concrete.kind != .Structure {
		return
	}
	prefix := ""
	if inc.as_name != nil {
		prefix = strings.to_lower(inc.as_name.name, context.temp_allocator)
	}
	for f in concrete.fields {
		field_name := f.name
		if len(prefix) > 0 {
			field_name = fmt.tprintf("%s-%s", prefix, f.name)
		}
		add_struct_field(struct_type, field_name, f.type_info, f.length, f.const_init)
	}
}

// CONSTANTS resolution

resolve_const_decl :: proc(
	table: ^SymbolTable,
	decl: ^ast.Const_Decl,
	is_chained: bool,
	is_global: bool = true,
	visibility: Visibility = .None,
) {
	name := decl.ident.name

	type_info := resolve_type_expr(table, decl.typed)

	sym := Symbol {
		name       = name,
		kind       = .Constant,
		range      = decl.ident.range,
		type_info  = type_info,
		is_chained = is_chained,
		visibility = visibility,
		const_init = decl.value,
	}
	add_symbol(table, sym, allow_shadowing = is_global)
}

resolve_const_chain_decl :: proc(
	table: ^SymbolTable,
	chain: ^ast.Const_Chain_Decl,
	is_global: bool = true,
	visibility: Visibility = .None,
) {
	for part in chain.parts {
		#partial switch p in part.derived_stmt {
		case ^ast.Const_Decl:
			resolve_const_decl(table, p, true, is_global, visibility)
		case ^ast.Const_Struct_Decl:
			resolve_const_struct_decl(table, p, visibility)
		}
	}
}

resolve_const_struct_decl :: proc(
	table: ^SymbolTable,
	struct_decl: ^ast.Const_Struct_Decl,
	visibility: Visibility = .None,
) {
	name := struct_decl.ident.name

	struct_type := make_structure_type(table, name)

	resolve_const_struct_components(table, struct_type, struct_decl.components[:])

	sym := Symbol {
		name       = name,
		kind       = .Constant,
		range      = struct_decl.ident.range,
		type_info  = struct_type,
		is_chained = false,
		visibility = visibility,
		const_init = nil,
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
			add_struct_field(struct_type, c.ident.name, field_type, 0, c.value)

		case ^ast.Const_Struct_Decl:
			nested_type := make_structure_type(table, c.ident.name)
			resolve_const_struct_components(table, nested_type, c.components[:])
			add_struct_field(struct_type, c.ident.name, nested_type, 0, nil)
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
			field_type: ^Type
			if c.is_like {
				field_type = resolve_like_type_expr(table, c.typed)
			} else {
				field_type = resolve_type_expr(table, c.typed)
			}
			add_struct_field(struct_type, decl_name_from_expr(c.ident), field_type, 0)

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

			// Only apply LENGTH clause when present; do not overwrite type-embedded lengths (e.g. char70).
			if c.length != nil {
				length_val := 0
				if lit, ok := c.length.derived_expr.(^ast.Basic_Lit); ok {
					for ch in lit.tok.lit {
						if ch >= '0' && ch <= '9' {
							length_val = length_val * 10 + int(ch - '0')
						}
					}
				}
				field_type.length = length_val
			}

			add_struct_field(struct_type, c.ident.name, field_type, field_type.length)

		case ^ast.Types_Struct_Decl:
			nested_type := make_structure_type(table, c.ident.name)
			resolve_struct_components(table, nested_type, c.components[:])
			add_struct_field(struct_type, c.ident.name, nested_type, 0)

		case ^ast.Types_Include_Type_Decl:
			resolve_types_include_into_struct(table, struct_type, c)
		}
	}
}

resolve_form_decl :: proc(
	table: ^SymbolTable,
	form: ^ast.Form_Decl,
	syntax_taint: []lexer.TextRange,
) {
	name := form.ident.name

	child_table := create_empty_symbol_table(context.allocator)

	for param in form.tables_params {
		resolve_form_param(child_table, param, .Tables)
	}

	for param in form.using_params {
		resolve_form_param(child_table, param, .Using)
	}

	for param in form.changing_params {
		resolve_form_param(child_table, param, .Changing)
	}

	resolve_stmt_list(child_table, form.body[:], syntax_taint)

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
		if param.is_like {
			type_info = resolve_like_type_expr(table, param.typed)
		} else {
			type_info = resolve_type_expr(table, param.typed)
		}
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
		if t, ok := resolve_char_builtin_ident(table, expr, e.name); ok {
			return t
		}
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

	case ^ast.Range_Type:
		elem_type := resolve_type_expr(table, e.elem)
		t := make_range_of_type(table, elem_type)
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
		if id, ok := e.expr.derived_expr.(^ast.Ident); ok && is_builtin_function_name(id.name) {
			t := make_type(table, .Integer)
			t.ast_node = expr
			return t
		}
		// User-defined calls: return type needs full call resolution
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

// resolve_like_type_expr resolves TYPE clauses that use LIKE: operands are data objects (or
// LINE OF / TABLE OF built from them), not only type names.
resolve_like_type_expr :: proc(table: ^SymbolTable, expr: ^ast.Expr) -> ^Type {
	if expr == nil {
		return make_unknown_type(table)
	}

	#partial switch e in expr.derived_expr {
	case ^ast.Ident:
		name := strings.to_lower(e.name)
		if sym, ok := table.symbols[name]; ok && sym.type_info != nil {
			return sym.type_info
		}
		return make_unknown_type(table)

	case ^ast.Table_Type:
		elem_type := resolve_like_type_expr(table, e.elem)
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

	case ^ast.Line_Type:
		table_type := resolve_like_type_expr(table, e.table)
		t := make_line_of_type(table, table_type)
		t.ast_node = expr
		return t

	case ^ast.Ref_Type:
		target_type := resolve_like_type_expr(table, e.target)
		t := make_reference_type(table, target_type)
		t.ast_node = expr
		return t

	case ^ast.Range_Type:
		elem_type := resolve_like_type_expr(table, e.elem)
		t := make_range_of_type(table, elem_type)
		t.ast_node = expr
		return t

	case ^ast.Selector_Expr:
		ty := expr_value_type(table, expr)
		if ty != nil {
			return ty
		}
		return make_unknown_type(table)

	case ^ast.Paren_Expr:
		return resolve_like_type_expr(table, e.expr)

	case:
		return resolve_type_expr(table, expr)
	}
}

// ABAP built-in CHAR spelled as CHAR, CHAR70, CHAR01, ... (length in the numeric suffix).
is_char_builtin_type_name :: proc(name: string) -> bool {
	lower := strings.to_lower(name, context.temp_allocator)
	if lower == "char" {
		return true
	}
	CHAR_PREFIX :: "char"
	if len(lower) <= len(CHAR_PREFIX) || !strings.has_prefix(lower, CHAR_PREFIX) {
		return false
	}
	suffix := lower[len(CHAR_PREFIX):]
	if len(suffix) == 0 {
		return false
	}
	for i in 0 ..< len(suffix) {
		c := suffix[i]
		if c < '0' || c > '9' {
			return false
		}
	}
	return true
}

resolve_char_builtin_ident :: proc(table: ^SymbolTable, ast_node: ^ast.Expr, name: string) -> (^Type, bool) {
	if !is_char_builtin_type_name(name) {
		return nil, false
	}
	lower := strings.to_lower(name, context.temp_allocator)
	if lower == "char" {
		t := make_type(table, .Char)
		t.ast_node = ast_node
		return t, true
	}
	CHAR_PREFIX :: "char"
	suffix := lower[len(CHAR_PREFIX):]
	n := 0
	for i in 0 ..< len(suffix) {
		n = n * 10 + int(suffix[i] - '0')
	}
	t := make_builtin_char_type(table, n)
	t.ast_node = ast_node
	return t, true
}

builtin_type_from_name :: proc(name: string) -> TypeKind {
	upper_name := strings.to_lower(name, context.temp_allocator)
	switch upper_name {
	case "i", "int1", "int2", "int4", "int8":
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
	case "data":
		return .Data
	}
	return .Unknown
}

// Built-in ABAP functions invoked as ident( ... ). Not declared in user programs; recognized for validation and typing.
is_builtin_function_name :: proc(name: string) -> bool {
	lower := strings.to_lower(name, context.temp_allocator)
	switch lower {
	case "strlen", "numofchar", "xstrlen", "lines", "charlen", "dbmaxlen":
		return true
	case:
		return false
	}
}

// builtin_function_hover_markdown returns documentation hover text for built-ins (empty string if not a known built-in).
builtin_function_hover_markdown :: proc(name: string) -> string {
	lower := strings.to_lower(name, context.temp_allocator)
	sig: string
	desc: string
	switch lower {
	case "charlen":
		sig = "charlen( arg )"
		desc = "Length of the first character of arg in the code page used: 1 for a single Unicode character; 2 for surrogate pairs."
	case "dbmaxlen":
		sig = "dbmaxlen( arg )"
		desc = "Maximum length of a string defined in the ABAP Dictionary (RAWSTRING, SSTRING, STRING, or GEOM_EWKB). If the string is unrestricted, the constant abap_max_db_string_ln or abap_max_db_rawstring_ln from the type pool ABAP is returned. The latter is also returned for the built-in ABAP types string and xstring."
	case "numofchar":
		sig = "numofchar( arg )"
		desc = "Number of characters in arg, where trailing blanks are neither counted in data objects with fixed lengths nor in data objects with the type string."
	case "strlen":
		sig = "strlen( arg )"
		desc = "Number of characters in arg, where trailing blanks in data objects with fixed lengths are not counted, whereas in data objects with the type string they are."
	case "xstrlen":
		sig = "xstrlen( arg )"
		desc = "Number of bytes in the byte string arg (xstring or byte-like type); trailing bytes with hexadecimal value 0 are not counted for fixed-length objects."
	case "lines":
		sig = "lines( arg )"
		desc = "Number of rows currently in the internal table arg."
	case:
		return ""
	}
	b: strings.Builder
	strings.builder_init(&b, context.temp_allocator)
	strings.write_string(&b, "```abap\nBuilt-in: ")
	strings.write_string(&b, sig)
	strings.write_string(&b, " -> i\n```\n\n")
	strings.write_string(&b, desc)
	return strings.to_string(b)
}

selector_to_string :: proc(sel: ^ast.Selector_Expr) -> string {
	if sel == nil {
		return ""
	}
	left := decl_name_from_expr(sel.expr)
	right := ast.selector_field_ident_name(sel)
	if left == "" {
		return right
	}
	if right == "" {
		return left
	}
	return strings.concatenate({left, "-", right}, context.temp_allocator)
}

decl_name_from_expr :: proc(expr: ^ast.Expr) -> string {
	if expr == nil {
		return ""
	}
	#partial switch e in expr.derived_expr {
	case ^ast.Ident:
		return e.name
	case ^ast.Selector_Expr:
		// Build name: lhs-rhs
		left := decl_name_from_expr(e.expr)
		right := ast.selector_field_ident_name(e)
		if left == "" {
			return right
		}
		// Allocate on temp allocator since this string needs to persist only for symbol creation
		return strings.concatenate({left, "-", right}, context.temp_allocator)
	}
	return ""
}

resolve_class_def_decl :: proc(
	table: ^SymbolTable,
	class_def: ^ast.Class_Def_Decl,
	syntax_taint: []lexer.TextRange,
) {
	name := class_def.ident.name

	child_table := create_empty_symbol_table(context.allocator)

	for section in class_def.sections {
		resolve_class_section(child_table, section, syntax_taint)
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

resolve_class_section :: proc(
	table: ^SymbolTable,
	section: ^ast.Class_Section,
	syntax_taint: []lexer.TextRange,
) {
	// Map AST access modifier to symbol visibility
	visibility := access_to_visibility(section.access)

	for type_decl in section.types {
		if statement_is_syntax_tainted(type_decl, syntax_taint) {
			continue
		}
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
		if statement_is_syntax_tainted(data_decl, syntax_taint) {
			continue
		}
		#partial switch d in data_decl.derived_stmt {
		case ^ast.Attr_Decl:
			resolve_attr_decl(table, d, visibility)
		case ^ast.Data_Typed_Decl:
			resolve_typed_decl(table, d, false, false)
		case ^ast.Data_Typed_Chain_Decl:
			resolve_chain_decl(table, d, false)
		case ^ast.Const_Decl:
			resolve_const_decl(table, d, false, false, visibility)
		case ^ast.Const_Chain_Decl:
			resolve_const_chain_decl(table, d, false, visibility)
		case ^ast.Const_Struct_Decl:
			resolve_const_struct_decl(table, d, visibility)
		}
	}

	for method_decl in section.methods {
		if statement_is_syntax_tainted(method_decl, syntax_taint) {
			continue
		}
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

	child_table := create_empty_symbol_table(context.allocator)

	for param in method.params {
		resolve_method_param(child_table, param)
	}
	// Exception names in RAISING are resolved as type expressions (Named / structured types).
	for r in method.raising {
		_ = resolve_type_expr(child_table, r)
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
	} else if param.likes != nil {
		type_info = resolve_like_type_expr(table, param.likes)
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

resolve_class_impl_decl :: proc(
	table: ^SymbolTable,
	class_impl: ^ast.Class_Impl_Decl,
	syntax_taint: []lexer.TextRange,
) {
	if class_impl.ident == nil {
		return
	}

	class_name := strings.to_lower(class_impl.ident.name, context.temp_allocator)
	if class_sym, ok := table.symbols[class_name]; ok && class_sym.child_scope != nil {
		for method in class_impl.methods {
			if statement_is_syntax_tainted(method, syntax_taint) {
				continue
			}
			#partial switch m in method.derived_stmt {
			case ^ast.Method_Impl:
				resolve_method_impl(class_sym.child_scope, m, syntax_taint)
			}
		}
		return
	}

	for method in class_impl.methods {
		if statement_is_syntax_tainted(method, syntax_taint) {
			continue
		}
		#partial switch m in method.derived_stmt {
		case ^ast.Method_Impl:
			fallback_scope := create_empty_symbol_table(context.allocator)
			resolve_stmt_list(fallback_scope, m.body[:], syntax_taint)
		}
	}
}

resolve_method_impl :: proc(
	class_scope: ^SymbolTable,
	method_impl: ^ast.Method_Impl,
	syntax_taint: []lexer.TextRange,
) {
	if class_scope == nil || method_impl.ident == nil {
		return
	}

	method_name := strings.to_lower(decl_name_from_expr(method_impl.ident), context.temp_allocator)
	if method_sym, ok := class_scope.symbols[method_name]; ok {
		if method_sym.child_scope == nil {
			method_sym.child_scope = create_empty_symbol_table(context.allocator)
			class_scope.symbols[method_name] = method_sym
		}
		resolve_stmt_list(method_sym.child_scope, method_impl.body[:], syntax_taint)
		return
	}

	child_table := create_empty_symbol_table(context.allocator)
	resolve_stmt_list(child_table, method_impl.body[:], syntax_taint)
	class_scope.symbols[method_name] = Symbol{
		name        = method_name,
		kind        = .Method,
		range       = method_impl.ident.range,
		child_scope = child_table,
	}
}

// resolve_stmt_list resolves all statements in a list, recursively handling control structures
resolve_stmt_list :: proc(
	table: ^SymbolTable,
	stmts: []^ast.Stmt,
	syntax_taint: []lexer.TextRange,
) {
	for stmt in stmts {
		resolve_stmt(table, stmt, syntax_taint)
	}
}

// resolve_stmt resolves declarations in a single statement
resolve_stmt :: proc(
	table: ^SymbolTable,
	stmt: ^ast.Stmt,
	syntax_taint: []lexer.TextRange,
) {
	if stmt == nil {
		return
	}
	if statement_is_syntax_tainted(stmt, syntax_taint) {
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
	case ^ast.Field_Symbol_Chain_Decl:
		resolve_field_symbol_chain_decl(table, s, is_global = false)
	case ^ast.Types_Decl:
		resolve_types_decl(table, s, false, is_global = false)
	case ^ast.Types_Chain_Decl:
		resolve_types_chain_decl(table, s, false)
	case ^ast.Types_Struct_Decl:
		resolve_types_struct_decl(table, s)
	case ^ast.Try_Stmt:
		resolve_try_stmt(table, s, syntax_taint)
	case ^ast.If_Stmt:
		resolve_if_stmt(table, s, syntax_taint)
	case ^ast.Case_Stmt:
		resolve_case_stmt(table, s, syntax_taint)
	case ^ast.While_Stmt:
		resolve_while_stmt(table, s, syntax_taint)
	case ^ast.Do_Stmt:
		resolve_do_stmt(table, s, syntax_taint)
	case ^ast.Loop_Stmt:
		resolve_loop_stmt(table, s, syntax_taint)
	case ^ast.Loop_At_Control_Stmt:
		resolve_loop_at_control_stmt(table, s, syntax_taint)
	case ^ast.Read_Table_Stmt:
		resolve_read_table_stmt(table, s)
	case ^ast.Insert_Stmt:
		resolve_insert_stmt(table, s)
	case ^ast.Describe_Table_Stmt:
		resolve_describe_table_stmt(table, s)
	case ^ast.Call_Function_Stmt:
		resolve_call_function_stmt(table, s)
	case ^ast.Call_Badi_Stmt:
		resolve_call_badi_stmt(table, s)
	case ^ast.Call_System_Stmt:
		resolve_call_system_stmt(table, s)
	case ^ast.Call_Transformation_Stmt:
		resolve_call_transformation_stmt(table, s)
	case ^ast.Select_Stmt:
		resolve_select_stmt(table, s, syntax_taint)
	case ^ast.Open_Cursor_Stmt:
		resolve_open_cursor_stmt(table, s, syntax_taint)
	case ^ast.Fetch_Cursor_Stmt:
		resolve_fetch_cursor_stmt(table, s, syntax_taint)
	case ^ast.Expr_Stmt:
		resolve_expr_inline_declarations(table, s.expr)
	case ^ast.Assign_Stmt:
		for rhs_expr in s.rhs {
			resolve_expr_inline_declarations(table, rhs_expr)
		}
	case ^ast.Message_Stmt:
		if s.into_target != nil {
			resolve_expr_inline_declarations(table, s.into_target)
		}
	}
}

resolve_if_stmt :: proc(table: ^SymbolTable, if_stmt: ^ast.If_Stmt, syntax_taint: []lexer.TextRange) {
	resolve_stmt_list(table, if_stmt.body[:], syntax_taint)

	for branch in if_stmt.elseif_branches {
		resolve_stmt_list(table, branch.body[:], syntax_taint)
	}

	resolve_stmt_list(table, if_stmt.else_body[:], syntax_taint)
}

resolve_try_stmt :: proc(table: ^SymbolTable, try_stmt: ^ast.Try_Stmt, syntax_taint: []lexer.TextRange) {
	resolve_stmt_list(table, try_stmt.body[:], syntax_taint)

	for branch in try_stmt.catch_branches {
		if branch.into_target != nil {
			if ident, ok := branch.into_target.derived_expr.(^ast.Ident); ok {
				type_info := make_unknown_type(table)
				if len(branch.class_refs) > 0 {
					type_info = resolve_type_expr(table, branch.class_refs[0])
				}

				sym := Symbol {
					name      = ident.name,
					kind      = .Variable,
					range     = ident.range,
					type_info = type_info,
				}
				add_symbol(table, sym, allow_shadowing = false)
			}
		}

		resolve_stmt_list(table, branch.body[:], syntax_taint)
	}

	if try_stmt.cleanup_branch != nil {
		if try_stmt.cleanup_branch.into_target != nil {
			if ident, ok := try_stmt.cleanup_branch.into_target.derived_expr.(^ast.Ident); ok {
				sym := Symbol {
					name      = ident.name,
					kind      = .Variable,
					range     = ident.range,
					type_info = make_unknown_type(table),
				}
				add_symbol(table, sym, allow_shadowing = false)
			}
		}

		resolve_stmt_list(table, try_stmt.cleanup_branch.body[:], syntax_taint)
	}
}

resolve_interface_decl :: proc(
	table: ^SymbolTable,
	iface: ^ast.Interface_Decl,
	syntax_taint: []lexer.TextRange,
) {
	name := iface.ident.name

	child_table := create_empty_symbol_table(context.allocator)

	// Interface members are implicitly public
	for method_decl in iface.methods {
		if statement_is_syntax_tainted(method_decl, syntax_taint) {
			continue
		}
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
		if statement_is_syntax_tainted(type_decl, syntax_taint) {
			continue
		}
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
		if statement_is_syntax_tainted(data_decl, syntax_taint) {
			continue
		}
		#partial switch d in data_decl.derived_stmt {
		case ^ast.Attr_Decl:
			resolve_attr_decl(child_table, d, .Public)
		case ^ast.Data_Typed_Decl:
			resolve_typed_decl(child_table, d, false, false)
		case ^ast.Const_Decl:
			resolve_const_decl(child_table, d, false, false, .Public)
		case ^ast.Const_Chain_Decl:
			resolve_const_chain_decl(child_table, d, false, .Public)
		case ^ast.Const_Struct_Decl:
			resolve_const_struct_decl(child_table, d, .Public)
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

resolve_event_block :: proc(
	table: ^SymbolTable,
	event: ^ast.Event_Block,
	syntax_taint: []lexer.TextRange,
) {
	// Create a child scope for the event block's local variables
	child_table := create_empty_symbol_table(context.allocator)

	// Resolve declarations in the event body
	resolve_stmt_list(child_table, event.body[:], syntax_taint)

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

resolve_module_decl :: proc(
	table: ^SymbolTable,
	module: ^ast.Module_Decl,
	syntax_taint: []lexer.TextRange,
) {
	if module.ident == nil {
		return
	}
	name := module.ident.name

	// Create a child scope for the module's local variables
	child_table := create_empty_symbol_table(context.allocator)

	// Resolve declarations in the module body
	resolve_stmt_list(child_table, module.body[:], syntax_taint)

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
		if fs_decl.is_like {
			type_info = resolve_like_type_expr(table, fs_decl.typed)
		} else {
			type_info = resolve_type_expr(table, fs_decl.typed)
		}
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

resolve_field_symbol_chain_decl :: proc(
	table: ^SymbolTable,
	chain: ^ast.Field_Symbol_Chain_Decl,
	is_global: bool = true,
) {
	for decl in chain.decls {
		resolve_field_symbol_decl(table, decl, is_global)
	}
}

resolve_case_stmt :: proc(table: ^SymbolTable, case_stmt: ^ast.Case_Stmt, syntax_taint: []lexer.TextRange) {
	for branch in case_stmt.branches {
		resolve_stmt_list(table, branch.body[:], syntax_taint)
	}
}

resolve_while_stmt :: proc(table: ^SymbolTable, while_stmt: ^ast.While_Stmt, syntax_taint: []lexer.TextRange) {
	resolve_stmt_list(table, while_stmt.body[:], syntax_taint)
}

resolve_do_stmt :: proc(table: ^SymbolTable, do_stmt: ^ast.Do_Stmt, syntax_taint: []lexer.TextRange) {
	resolve_stmt_list(table, do_stmt.body[:], syntax_taint)
}

resolve_loop_at_control_stmt :: proc(
	table: ^SymbolTable,
	stmt: ^ast.Loop_At_Control_Stmt,
	syntax_taint: []lexer.TextRange,
) {
	resolve_stmt_list(table, stmt.body[:], syntax_taint)
}

resolve_loop_stmt :: proc(table: ^SymbolTable, loop_stmt: ^ast.Loop_Stmt, syntax_taint: []lexer.TextRange) {
	// Handle inline DATA declaration in INTO clause (INTO DATA(wa); bare INTO wa is an existing variable)
	if loop_stmt.into_target != nil {
		if ident, ok := loop_stmt.into_target.derived_expr.(^ast.Ident); ok && ident.inline_data_decl != nil {
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

	// Inline FIELD-SYMBOL only: ASSIGNING <fs> uses an existing FIELD-SYMBOL declaration.
	if loop_stmt.assigning_target != nil {
		if ident, ok := loop_stmt.assigning_target.derived_expr.(^ast.Ident); ok &&
		   ident.is_inline_field_symbol_decl {
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
	resolve_stmt_list(table, loop_stmt.body[:], syntax_taint)
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
	// Handle inline DATA declaration in INTO clause (INTO DATA(wa); bare INTO wa is an existing variable)
	if read_stmt.into_target != nil {
		if ident, ok := read_stmt.into_target.derived_expr.(^ast.Ident); ok && ident.inline_data_decl != nil {
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

	// Inline FIELD-SYMBOL only: ASSIGNING <fs> uses an existing FIELD-SYMBOL declaration.
	if read_stmt.assigning_target != nil {
		if ident, ok := read_stmt.assigning_target.derived_expr.(^ast.Ident); ok &&
		   ident.is_inline_field_symbol_decl {
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

resolve_insert_stmt :: proc(table: ^SymbolTable, insert_stmt: ^ast.Insert_Stmt) {
	if insert_stmt.kind != .Initial_Line_Into_Itab {
		return
	}
	if insert_stmt.assigning_target == nil {
		return
	}
	if ident, ok := insert_stmt.assigning_target.derived_expr.(^ast.Ident); ok &&
	   ident.is_inline_field_symbol_decl {
		type_info := make_inferred_type(table, insert_stmt.target)
		sym := Symbol {
			name      = ident.name,
			kind      = .FieldSymbol,
			range     = ident.range,
			type_info = type_info,
		}
		add_symbol(table, sym, allow_shadowing = false)
	}
}

resolve_describe_table_stmt :: proc(table: ^SymbolTable, describe_stmt: ^ast.Describe_Table_Stmt) {
	if describe_stmt.lines_target == nil {
		return
	}

	if ident, ok := describe_stmt.lines_target.derived_expr.(^ast.Ident); ok && ident.inline_data_decl != nil {
		sym := Symbol {
			name      = ident.name,
			kind      = .Variable,
			range     = ident.range,
			type_info = make_type(table, .Integer),
		}
		add_symbol(table, sym, allow_shadowing = false)
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

	for param in call_func.exceptions {
		if param.message_value != nil {
			resolve_param_value_decl(table, param.message_value)
		}
	}
}

// resolve_expr_inline_declarations registers procedure-local variables introduced by
// inline DATA(name) anywhere inside an expression (method call parameters, assignments, etc.).
resolve_expr_inline_declarations :: proc(table: ^SymbolTable, expr: ^ast.Expr) {
	if expr == nil {
		return
	}

	#partial switch e in expr.derived_expr {
	case ^ast.Ident:
		if e.inline_data_decl != nil {
			resolve_inline_decl(table, e.inline_data_decl, is_global = false)
		}
	case ^ast.Selector_Expr:
		resolve_expr_inline_declarations(table, e.expr)
		if e.field != nil {
			resolve_expr_inline_declarations(table, e.field)
		}
	case ^ast.Unary_Expr:
		resolve_expr_inline_declarations(table, e.expr)
	case ^ast.Binary_Expr:
		resolve_expr_inline_declarations(table, e.left)
		resolve_expr_inline_declarations(table, e.right)
	case ^ast.Paren_Expr:
		resolve_expr_inline_declarations(table, e.expr)
	case ^ast.Index_Expr:
		resolve_expr_inline_declarations(table, e.expr)
		resolve_expr_inline_declarations(table, e.index)
	case ^ast.Substring_Expr:
		resolve_expr_inline_declarations(table, e.expr)
		if e.offset != nil {
			resolve_expr_inline_declarations(table, e.offset)
		}
		if e.length != nil {
			resolve_expr_inline_declarations(table, e.length)
		}
	case ^ast.Call_Expr:
		resolve_expr_inline_declarations(table, e.expr)
		for arg in e.args {
			resolve_expr_inline_declarations(table, arg)
		}
	case ^ast.New_Expr:
		if e.type_expr != nil {
			resolve_expr_inline_declarations(table, e.type_expr)
		}
		for arg in e.args {
			resolve_expr_inline_declarations(table, arg)
		}
	case ^ast.Constructor_Expr:
		if e.type_expr != nil {
			resolve_expr_inline_declarations(table, e.type_expr)
		}
		for arg in e.args {
			resolve_expr_inline_declarations(table, arg)
		}
	case ^ast.Named_Arg:
		resolve_expr_inline_declarations(table, e.value)
	case ^ast.For_Expr:
		resolve_expr_inline_declarations(table, e.itab)
		if e.where_cond != nil {
			resolve_expr_inline_declarations(table, e.where_cond)
		}
		if e.result_expr != nil {
			resolve_expr_inline_declarations(table, e.result_expr)
		}
		for arg in e.result_args {
			resolve_expr_inline_declarations(table, arg)
		}
	case ^ast.Value_Row_Expr:
		for arg in e.args {
			resolve_expr_inline_declarations(table, arg)
		}
	case ^ast.String_Template_Expr:
		for part in e.parts {
			if part.is_expr && part.expr != nil {
				resolve_expr_inline_declarations(table, part.expr)
			}
		}
	case ^ast.Predicate_Expr:
		resolve_expr_inline_declarations(table, e.expr)
		if e.type_ref != nil {
			resolve_expr_inline_declarations(table, e.type_ref)
		}
	case ^ast.Table_Type:
		resolve_expr_inline_declarations(table, e.elem)
	case ^ast.Ref_Type:
		resolve_expr_inline_declarations(table, e.target)
	case ^ast.Line_Type:
		resolve_expr_inline_declarations(table, e.table)
	case ^ast.Range_Type:
		resolve_expr_inline_declarations(table, e.elem)
	case ^ast.Bad_Expr, ^ast.Basic_Lit:
	// No sub-expressions
	}
}

resolve_param_value_decl :: proc(table: ^SymbolTable, expr: ^ast.Expr) {
	resolve_expr_inline_declarations(table, expr)
}

resolve_call_badi_stmt :: proc(table: ^SymbolTable, stmt: ^ast.Call_Badi_Stmt) {
	for param in stmt.exporting {
		if param.value != nil {
			resolve_param_value_decl(table, param.value)
		}
	}
	for param in stmt.importing {
		if param.value != nil {
			resolve_param_value_decl(table, param.value)
		}
	}
	for param in stmt.changing {
		if param.value != nil {
			resolve_param_value_decl(table, param.value)
		}
	}
	for param in stmt.receiving {
		if param.value != nil {
			resolve_param_value_decl(table, param.value)
		}
	}
	for param in stmt.exceptions {
		if param.message_value != nil {
			resolve_param_value_decl(table, param.message_value)
		}
	}
}

resolve_call_system_stmt :: proc(table: ^SymbolTable, stmt: ^ast.Call_System_Stmt) {
	for param in stmt.params {
		if param.id_name != nil {
			resolve_param_value_decl(table, param.id_name)
		}
		if param.field != nil {
			resolve_param_value_decl(table, param.field)
		}
	}
}

resolve_call_transformation_stmt :: proc(table: ^SymbolTable, stmt: ^ast.Call_Transformation_Stmt) {
	if stmt.transformation != nil {
		resolve_param_value_decl(table, stmt.transformation)
	}
	if stmt.options != nil {
		resolve_param_value_decl(table, stmt.options)
	}
	if stmt.source != nil {
		resolve_param_value_decl(table, stmt.source)
	}
	if stmt.result_stream != nil {
		resolve_param_value_decl(table, stmt.result_stream)
	}
	for root in stmt.result_roots {
		if root.value != nil {
			resolve_param_value_decl(table, root.value)
		}
	}
}

resolve_open_cursor_stmt :: proc(
	table: ^SymbolTable,
	stmt: ^ast.Open_Cursor_Stmt,
	syntax_taint: []lexer.TextRange,
) {
	if stmt.cursor != nil {
		sym := Symbol {
			name      = stmt.cursor.name,
			kind      = .Variable,
			range     = stmt.cursor.range,
			type_info = make_unknown_type(table),
		}
		add_symbol(table, sym, allow_shadowing = false)
	}
	if stmt.select_stmt != nil {
		if sel, ok := stmt.select_stmt.derived_stmt.(^ast.Select_Stmt); ok {
			resolve_select_stmt(table, sel, syntax_taint)
		}
	}
}

// Registers variables only for Open SQL @DATA(...) / INTO DATA(...) targets (Ident.inline_data_decl).
// Bare INTO host variables and @host are existing data objects and must not introduce a new symbol.
add_symbols_for_open_sql_into_inline_data :: proc(table: ^SymbolTable, into_target: ^ast.Expr, type_info: ^Type) {
	if into_target == nil || type_info == nil {
		return
	}
	#partial switch e in into_target.derived_expr {
	case ^ast.Value_Row_Expr:
		for arg in e.args {
			add_symbols_for_open_sql_into_inline_data(table, arg, type_info)
		}
	case ^ast.Ident:
		if e.inline_data_decl == nil {
			return
		}
		sym := Symbol {
			name      = e.name,
			kind      = .Variable,
			range     = e.range,
			type_info = type_info,
		}
		add_symbol(table, sym, allow_shadowing = false)
	}
}

resolve_fetch_cursor_stmt :: proc(
	table: ^SymbolTable,
	stmt: ^ast.Fetch_Cursor_Stmt,
	syntax_taint: []lexer.TextRange,
) {
	_ = syntax_taint
	if stmt.into_target != nil {
		add_symbols_for_open_sql_into_inline_data(table, stmt.into_target, make_unknown_type(table))
	}
}

resolve_select_stmt :: proc(
	table: ^SymbolTable,
	select_stmt: ^ast.Select_Stmt,
	syntax_taint: []lexer.TextRange,
) {
	if select_stmt.into_target != nil {
		add_symbols_for_open_sql_into_inline_data(table, select_stmt.into_target, make_unknown_type(table))
	}

	// Resolve statements in the SELECT loop body (for non-SINGLE selects)
	resolve_stmt_list(table, select_stmt.body[:], syntax_taint)
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
