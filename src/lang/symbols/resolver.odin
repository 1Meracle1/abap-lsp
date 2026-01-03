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
		case ^ast.Report_Decl:
			resolve_report_decl(table, d)
		case ^ast.Include_Decl:
			resolve_include_decl(table, d)
		case ^ast.Event_Block:
			resolve_event_block(table, d)
		case ^ast.Module_Decl:
			resolve_module_decl(table, d)
		}
	}

	return table
}

resolve_inline_decl :: proc(table: ^SymbolTable, decl: ^ast.Data_Inline_Decl, is_global: bool = true) {
	name := decl.ident.name
	
	type_info := make_inferred_type(table, decl.value)
	
	sym := Symbol {
		name      = name,
		kind      = .Variable,
		range     = decl.ident.range,
		type_info = type_info,
		is_chained = false,
	}
	add_symbol(table, sym, allow_shadowing = is_global)
}

resolve_typed_decl :: proc(table: ^SymbolTable, decl: ^ast.Data_Typed_Decl, is_chained: bool, is_global: bool = true) {
	name := decl.ident.name
	
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

resolve_chain_decl :: proc(table: ^SymbolTable, chain: ^ast.Data_Typed_Chain_Decl, is_global: bool = true) {
	for decl in chain.decls {
		resolve_typed_decl(table, decl, true, is_global)
	}
}

resolve_types_decl :: proc(table: ^SymbolTable, decl: ^ast.Types_Decl, is_chained: bool, is_global: bool = true) {
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

resolve_types_chain_decl :: proc(table: ^SymbolTable, chain: ^ast.Types_Chain_Decl, is_global: bool = true) {
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

resolve_struct_components :: proc(table: ^SymbolTable, struct_type: ^Type, components: []^ast.Stmt) {
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

resolve_form_param :: proc(table: ^SymbolTable, param: ^ast.Form_Param, param_kind: FormParamKind) {
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
		t := make_table_type(table, elem_type)
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
			resolve_attr_decl(table, d)
		case ^ast.Data_Typed_Decl:
			resolve_typed_decl(table, d, false, false)
		case ^ast.Data_Typed_Chain_Decl:
			resolve_chain_decl(table, d, false)
		}
	}
	
	for method_decl in section.methods {
		#partial switch m in method_decl.derived_stmt {
		case ^ast.Method_Decl:
			resolve_method_decl(table, m)
		}
	}
	
	for iface_decl in section.interfaces {
		#partial switch i in iface_decl.derived_stmt {
		case ^ast.Interfaces_Decl:
		}
	}
}

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

resolve_method_decl :: proc(table: ^SymbolTable, method: ^ast.Method_Decl) {
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
	case ^ast.If_Stmt:
		resolve_if_stmt(table, s)
	}
}

// resolve_if_stmt resolves declarations inside IF statement bodies
resolve_if_stmt :: proc(table: ^SymbolTable, if_stmt: ^ast.If_Stmt) {
	// Resolve declarations in the main IF body
	resolve_stmt_list(table, if_stmt.body[:])
	
	// Resolve declarations in ELSEIF branches
	for branch in if_stmt.elseif_branches {
		resolve_stmt_list(table, branch.body[:])
	}
	
	// Resolve declarations in ELSE body
	resolve_stmt_list(table, if_stmt.else_body[:])
}

resolve_interface_decl :: proc(table: ^SymbolTable, iface: ^ast.Interface_Decl) {
	name := iface.ident.name
	
	child_table := new(SymbolTable)
	child_table.symbols = make(map[string]Symbol)
	child_table.types = make([dynamic]^Type)
	child_table.diagnostics = make([dynamic]Diagnostic)
	
	for method_decl in iface.methods {
		#partial switch m in method_decl.derived_stmt {
		case ^ast.Method_Decl:
			resolve_method_decl(child_table, m)
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
			resolve_attr_decl(child_table, d)
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
	add_symbol(table, sym, allow_shadowing = true)  // Allow shadowing for includes
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
