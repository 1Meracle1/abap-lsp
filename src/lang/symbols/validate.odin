package lang_symbols

import "../ast"
import "../lexer"
import "core:strings"

// validate_file runs semantic validation on the file after symbol resolution
// Uses the same table for both symbol lookup and storing diagnostics
validate_file :: proc(file: ^ast.File, table: ^SymbolTable) {
	validate_file_with_lookup(file, table, table)
}

// validate_file_with_lookup runs semantic validation on a file
// lookup_table: used to look up symbols (e.g., merged project symbol table)
// diag_table: used to store diagnostics (can be same as lookup_table or separate)
// This allows validating a file against a merged symbol table from a multi-file project
validate_file_with_lookup :: proc(file: ^ast.File, lookup_table: ^SymbolTable, diag_table: ^SymbolTable) {
	if file == nil || lookup_table == nil || diag_table == nil {
		return
	}

	ctx := Validation_Context{
		lookup_table  = lookup_table,
		diag_table    = diag_table,
		syntax_taint  = build_syntax_taint_ranges(file, context.temp_allocator),
		module_lookup = lookup_table,
	}

	for decl in file.decls {
		validate_stmt_ctx(&ctx, decl)
	}
}

// Validation context holds references to symbol tables for lookup and diagnostics
Validation_Context :: struct {
	lookup_table: ^SymbolTable, // Current scope (class body, form, …)
	diag_table:   ^SymbolTable, // Table to store diagnostics in
	syntax_taint: []lexer.TextRange,
	// Module/file scope for names outside the current lookup_table (e.g. INHERITING FROM, INTERFACES).
	// When nil, `lookup_table` is used for both.
	module_lookup: ^SymbolTable,
	// METHOD importing parameter names in the current signature; LIKE may refer to these without a workspace type.
	method_param_names_for_like: ^map[string]bool,
	// Class/interface member scope when validating inside a method implementation (DATA, CLASS-DATA, methods, …).
	// Lookups check lookup_table first (parameters, locals), then enclosing_scope.
	enclosing_scope: ^SymbolTable,
	// Set in instance method bodies: ABAP `me` references the current object (not in CLASS-METHODS).
	allow_me_identifier: bool,
	// Class symbol's Named type (class_sym.type_info); used to resolve me->attribute against the class scope.
	self_class_type: ^Type,
	// True only while validating expressions that may introduce FIELD-SYMBOL(<fs>) (ASSIGNING / ASSIGN ... TO FIELD-SYMBOL).
	allow_inline_field_symbol_ident: bool,
}

// symbol_defined_in_validation_scope reports if `lower_name` is a symbol in the current scope chain.
symbol_defined_in_validation_scope :: proc(ctx: ^Validation_Context, lower_name: string) -> bool {
	if ctx == nil || len(lower_name) == 0 {
		return false
	}
	if ctx.lookup_table != nil && lower_name in ctx.lookup_table.symbols {
		return true
	}
	if ctx.enclosing_scope != nil && lower_name in ctx.enclosing_scope.symbols {
		return true
	}
	return false
}

module_scope_lookup :: proc(ctx: ^Validation_Context) -> ^SymbolTable {
	if ctx == nil {
		return nil
	}
	if ctx.module_lookup != nil {
		return ctx.module_lookup
	}
	return ctx.lookup_table
}

// Context that uses module/file symbol table for lookups (same diagnostics and taint).
module_expr_ctx :: proc(ctx: ^Validation_Context) -> Validation_Context {
	out := ctx^
	out.lookup_table = module_scope_lookup(ctx)
	return out
}

typeexpr_root_lookup_key :: proc(expr: ^ast.Expr) -> string {
	if expr == nil {
		return ""
	}
	#partial switch e in expr.derived_expr {
	case ^ast.Ident:
		return strings.to_lower(e.name, context.temp_allocator)
	case ^ast.Selector_Expr:
		return strings.to_lower(ast.selector_field_ident_name(e), context.temp_allocator)
	}
	return ""
}

maybe_diagnostic_inherit_must_be_class :: proc(
	module_ctx: ^Validation_Context,
	diag_table: ^SymbolTable,
	expr: ^ast.Expr,
) {
	if module_ctx == nil ||
	   module_ctx.lookup_table == nil ||
	   diag_table == nil ||
	   expr == nil {
		return
	}
	key := typeexpr_root_lookup_key(expr)
	if key == "" {
		return
	}
	if sym, ok := module_ctx.lookup_table.symbols[key]; ok && sym.kind != .Class {
		add_diagnostic(diag_table, expr.range, "INHERITING FROM must reference a class")
	}
}

maybe_diagnostic_behavior_must_be_interface :: proc(
	module_ctx: ^Validation_Context,
	diag_table: ^SymbolTable,
	expr: ^ast.Expr,
) {
	if module_ctx == nil ||
	   module_ctx.lookup_table == nil ||
	   diag_table == nil ||
	   expr == nil {
		return
	}
	key := typeexpr_root_lookup_key(expr)
	if key == "" {
		return
	}
	if sym, ok := module_ctx.lookup_table.symbols[key]; ok && sym.kind != .Interface {
		add_diagnostic(diag_table, expr.range, "BEHAVIOR OF must reference an interface")
	}
}

maybe_diagnostic_friends_class_or_interface :: proc(
	module_ctx: ^Validation_Context,
	diag_table: ^SymbolTable,
	expr: ^ast.Expr,
) {
	if module_ctx == nil ||
	   module_ctx.lookup_table == nil ||
	   diag_table == nil ||
	   expr == nil {
		return
	}
	key := typeexpr_root_lookup_key(expr)
	if key == "" {
		return
	}
	if sym, ok := module_ctx.lookup_table.symbols[key]; ok {
		if sym.kind != .Class && sym.kind != .Interface {
			add_diagnostic(
				diag_table,
				expr.range,
				"FRIENDS must reference a class or interface",
			)
		}
	}
}

validate_class_def_header_ctx :: proc(ctx: ^Validation_Context, decl: ^ast.Class_Def_Decl) {
	if decl == nil {
		return
	}
	mc := module_expr_ctx(ctx)
	if decl.inheriting_from != nil {
		validate_type_expr_ctx(&mc, decl.inheriting_from)
		maybe_diagnostic_inherit_must_be_class(&mc, ctx.diag_table, decl.inheriting_from)
	}
	if decl.behavior_of != nil {
		validate_type_expr_ctx(&mc, decl.behavior_of)
		maybe_diagnostic_behavior_must_be_interface(&mc, ctx.diag_table, decl.behavior_of)
	}
	for f in decl.friends {
		validate_type_expr_ctx(&mc, f)
		maybe_diagnostic_friends_class_or_interface(&mc, ctx.diag_table, f)
	}
}

validate_method_decl_ctx :: proc(ctx: ^Validation_Context, decl: ^ast.Method_Decl) {
	if decl == nil {
		return
	}
	like_names := make(map[string]bool, context.temp_allocator)
	for param in decl.params {
		if param != nil && param.ident != nil {
			like_names[strings.to_lower(param.ident.name)] = true
		}
	}
	mctx := ctx^
	if len(like_names) > 0 {
		mctx.method_param_names_for_like = &like_names
	}
	for param in decl.params {
		if param == nil {
			continue
		}
		validate_type_expr_ctx(&mctx, param.typed)
		validate_type_expr_ctx(&mctx, param.likes)
		validate_expr_ctx(ctx, param.default)
	}
	raise_ctx := module_expr_ctx(ctx)
	for r in decl.raising {
		validate_raising_type_expr_ctx(&raise_ctx, r)
	}
}

// validate_raising_type_expr_ctx checks method/interface RAISING clauses: each name must resolve to a class
// (exception class) in the module/workspace table — not a built-in, interface, typedef, or non-type symbol.
validate_raising_type_expr_ctx :: proc(ctx: ^Validation_Context, expr: ^ast.Expr) {
	if expr == nil {
		return
	}
	#partial switch e in expr.derived_expr {
	case ^ast.Ident:
		validate_raising_type_ident_ctx(ctx, e)
	case ^ast.Table_Type:
		validate_raising_type_expr_ctx(ctx, e.elem)
	case ^ast.Ref_Type:
		validate_raising_type_expr_ctx(ctx, e.target)
	case ^ast.Line_Type:
		validate_raising_type_expr_ctx(ctx, e.table)
	case ^ast.Range_Type:
		validate_raising_type_expr_ctx(ctx, e.elem)
	case ^ast.Selector_Expr:
		validate_raising_type_expr_ctx(ctx, e.expr)
		if e.field != nil {
			validate_raising_type_expr_ctx(ctx, e.field)
		}
	case ^ast.Paren_Expr:
		validate_raising_type_expr_ctx(ctx, e.expr)
	case:
		validate_type_expr_ctx(ctx, expr)
	}
}

validate_raising_type_ident_ctx :: proc(ctx: ^Validation_Context, ident: ^ast.Ident) {
	if ctx == nil || ident == nil || ctx.diag_table == nil {
		return
	}
	mod_table := module_scope_lookup(ctx)
	if mod_table == nil {
		return
	}
	name := ident.name
	lower := strings.to_lower(name, context.temp_allocator)
	if len(lower) == 0 {
		return
	}
	if ctx.method_param_names_for_like != nil && lower in ctx.method_param_names_for_like {
		return
	}
	if builtin_type_from_name(name) != .Unknown || is_char_builtin_type_name(name) {
		add_diagnostic(
			ctx.diag_table,
			ident.range,
			strings.concatenate(
				{"RAISING must list an exception class; '", name, "' is a built-in type"},
				context.temp_allocator,
			),
		)
		return
	}
	if sym, ok := mod_table.symbols[lower]; ok {
		if sym.kind == .Class {
			return
		}
		kind_msg := "symbol"
		#partial switch sym.kind {
		case .Interface:
			kind_msg = "an interface"
		case .TypeDef:
			kind_msg = "a type (TYPES)"
		case .Variable, .Constant, .Parameter, .Field:
			kind_msg = "a data object"
		case .Method:
			kind_msg = "a method"
		case .Form, .FormParameter:
			kind_msg = "a form or parameter"
		}
		add_diagnostic(
			ctx.diag_table,
			ident.range,
			strings.concatenate(
				{
					"'",
					name,
					"' cannot be used in RAISING (expect an exception class; found ",
					kind_msg,
					")",
				},
				context.temp_allocator,
			),
		)
		return
	}
	remote_ctx := ctx^
	remote_ctx.lookup_table = mod_table
	maybe_add_remote_candidate(&remote_ctx, name, .Type_Name)
	add_diagnostic(
		ctx.diag_table,
		ident.range,
		strings.concatenate({"Unknown exception class '", name, "'"}, context.temp_allocator),
	)
}

// validate_type_ident_ctx checks TYPE / LIKE simple identifier spellings.
// Consults current scope (e.g. class PUBLIC SECTION), then enclosing class/interface scope (method bodies), then module/file scope.
// Emits "Unknown type" when not built-in and not a valid type denoter; still records remote candidates for Z/Y/ RFC-style names.
// Structure-typed data objects (built-in `sy`, DATA BEGIN OF locals) name a flat struct and may prefix component types (TYPE sy-tabix).
validate_type_ident_ctx :: proc(ctx: ^Validation_Context, ident: ^ast.Ident) {
	if ctx == nil || ident == nil || ctx.diag_table == nil {
		return
	}
	name := ident.name
	lower := strings.to_lower(name, context.temp_allocator)
	if len(lower) == 0 {
		return
	}
	if ctx.method_param_names_for_like != nil && lower in ctx.method_param_names_for_like {
		return
	}
	if builtin_type_from_name(name) != .Unknown {
		return
	}
	if is_char_builtin_type_name(name) {
		return
	}
	mod_table := module_scope_lookup(ctx)
	type_lookup_tables := [?]^SymbolTable{ctx.lookup_table, ctx.enclosing_scope, mod_table}
	for tab in type_lookup_tables {
		if tab == nil {
			continue
		}
		if sym, ok := tab.symbols[lower]; ok {
			#partial switch sym.kind {
			case .TypeDef, .Class, .Interface:
				return
			case .Variable, .Constant:
				if sym.type_info != nil && sym.type_info.kind == .Structure {
					return
				}
				fallthrough
			case:
				add_diagnostic(
					ctx.diag_table,
					ident.range,
					strings.concatenate(
						{"'", name, "' cannot be used as a type here"},
						context.temp_allocator,
					),
				)
				return
			}
		}
	}
	if mod_table != nil {
		remote_ctx := ctx^
		remote_ctx.lookup_table = mod_table
		maybe_add_remote_candidate(&remote_ctx, name, .Type_Name)
	}
	add_diagnostic(
		ctx.diag_table,
		ident.range,
		strings.concatenate({"Unknown type '", name, "'"}, context.temp_allocator),
	)
}

// validate_stmt_list validates a list of statements
validate_stmt_list :: proc(table: ^SymbolTable, stmts: []^ast.Stmt) {
	ctx := Validation_Context{lookup_table = table, diag_table = table}
	validate_stmt_list_ctx(&ctx, stmts)
}

validate_stmt_list_ctx :: proc(ctx: ^Validation_Context, stmts: []^ast.Stmt) {
	for stmt in stmts {
		validate_stmt_ctx(ctx, stmt)
	}
}

// validate_stmt validates a single statement and its expressions
validate_stmt :: proc(table: ^SymbolTable, stmt: ^ast.Stmt) {
	ctx := Validation_Context{lookup_table = table, diag_table = table}
	validate_stmt_ctx(&ctx, stmt)
}

validate_stmt_ctx :: proc(ctx: ^Validation_Context, stmt: ^ast.Stmt) {
	if stmt == nil {
		return
	}
	if statement_is_syntax_tainted(stmt, ctx.syntax_taint) {
		return
	}

	#partial switch s in stmt.derived_stmt {
	case ^ast.Data_Inline_Decl:
		validate_expr_ctx(ctx, s.value)
	case ^ast.Data_Typed_Decl:
		validate_expr_ctx(ctx, s.length)
		validate_type_expr_ctx(ctx, s.typed)
		validate_expr_ctx(ctx, s.value)
	case ^ast.Data_Typed_Chain_Decl:
		for part in s.parts {
			validate_stmt_ctx(ctx, part)
		}
	case ^ast.Data_Struct_Decl:
		for comp in s.components {
			validate_stmt_ctx(ctx, comp)
		}
	case ^ast.Const_Decl:
		validate_expr_ctx(ctx, s.length)
		validate_type_expr_ctx(ctx, s.typed)
		validate_expr_ctx(ctx, s.value)
	case ^ast.Types_Decl:
		validate_type_expr_ctx(ctx, s.typed)
	case ^ast.Types_Chain_Decl:
		for part in s.parts {
			validate_stmt_ctx(ctx, part)
		}
	case ^ast.Types_Struct_Decl:
		validate_stmt_list_ctx(ctx, s.components[:])
	case ^ast.Types_Include_Type_Decl:
		validate_type_expr_ctx(ctx, s.included)
	case ^ast.Attr_Decl:
		validate_type_expr_ctx(ctx, s.typed)
		validate_expr_ctx(ctx, s.value)
	case ^ast.Assign_Stmt:
		for lhs in s.lhs {
			validate_expr_ctx(ctx, lhs)
		}
		for rhs in s.rhs {
			validate_expr_ctx(ctx, rhs)
		}
	case ^ast.Move_Corresponding_Stmt:
		validate_expr_ctx(ctx, s.source)
		validate_expr_ctx(ctx, s.target)
	case ^ast.Assign_Field_Symbol_Stmt:
		validate_expr_ctx(ctx, s.component)
		validate_expr_ctx(ctx, s.structure)
		validate_expr_ctx(ctx, s.source)
		validate_expr_ctx(ctx, s.offset)
		validate_expr_ctx(ctx, s.length)
		validate_expr_ctx_allow_inline_field_symbol(ctx, s.target)
	case ^ast.Unassign_Stmt:
		for target in s.targets {
			validate_expr_ctx(ctx, target)
		}
	case ^ast.Expr_Stmt:
		validate_expr_ctx(ctx, s.expr)
	case ^ast.Macro_Call_Stmt:
		validate_expr_ctx(ctx, s.name)
		for arg in s.args {
			validate_expr_ctx(ctx, arg)
		}
	case ^ast.Try_Stmt:
		validate_stmt_list_ctx(ctx, s.body[:])
		for branch in s.catch_branches {
			for class_ref in branch.class_refs {
				validate_expr_ctx(ctx, class_ref)
			}
			validate_expr_ctx(ctx, branch.into_target)
			validate_stmt_list_ctx(ctx, branch.body[:])
		}
		if s.cleanup_branch != nil {
			validate_expr_ctx(ctx, s.cleanup_branch.into_target)
			validate_stmt_list_ctx(ctx, s.cleanup_branch.body[:])
		}
	case ^ast.If_Stmt:
		validate_expr_ctx(ctx, s.cond)
		validate_stmt_list_ctx(ctx, s.body[:])
		for branch in s.elseif_branches {
			validate_expr_ctx(ctx, branch.cond)
			validate_stmt_list_ctx(ctx, branch.body[:])
		}
		validate_stmt_list_ctx(ctx, s.else_body[:])
	case ^ast.Case_Stmt:
		validate_expr_ctx(ctx, s.expr)
		for branch in s.branches {
			validate_expr_ctx(ctx, branch.expr)
			validate_stmt_list_ctx(ctx, branch.body[:])
		}
	case ^ast.While_Stmt:
		validate_expr_ctx(ctx, s.cond)
		validate_stmt_list_ctx(ctx, s.body[:])
	case ^ast.Do_Stmt:
		validate_expr_ctx(ctx, s.times)
		validate_stmt_list_ctx(ctx, s.body[:])
	case ^ast.Loop_Stmt:
		validate_expr_ctx(ctx, s.itab)
		validate_expr_ctx(ctx, s.into_target)
		validate_expr_ctx_allow_inline_field_symbol(ctx, s.assigning_target)
		validate_stmt_list_ctx(ctx, s.body[:])
	case ^ast.Loop_At_Control_Stmt:
		validate_expr_ctx(ctx, s.field)
		validate_stmt_list_ctx(ctx, s.body[:])
	case ^ast.Read_Table_Stmt:
		validate_expr_ctx(ctx, s.itab)
		validate_expr_ctx(ctx, s.into_target)
		validate_expr_ctx_allow_inline_field_symbol(ctx, s.assigning_target)
	case ^ast.Get_Badi_Stmt:
		validate_expr_ctx(ctx, s.badi_ref)
		for f in s.filters {
			validate_expr_ctx(ctx, f.value)
		}
	case ^ast.Set_Handler_Stmt:
		for h in s.handlers {
			validate_expr_ctx(ctx, h)
		}
		validate_expr_ctx(ctx, s.for_ref)
	case ^ast.Set_Bit_Stmt:
		validate_expr_ctx(ctx, s.bit_position)
		validate_expr_ctx(ctx, s.of_target)
		validate_expr_ctx(ctx, s.to_value)
	case ^ast.Get_Bit_Stmt:
		validate_expr_ctx(ctx, s.bit_position)
		validate_expr_ctx(ctx, s.of_target)
		validate_expr_ctx(ctx, s.into_target)
	case ^ast.Call_Badi_Stmt:
		validate_expr_ctx(ctx, s.badi_target)
		for param in s.exporting {
			validate_expr_ctx(ctx, param.value)
		}
		for param in s.importing {
			validate_expr_ctx(ctx, param.value)
		}
		for param in s.changing {
			validate_expr_ctx(ctx, param.value)
		}
		for param in s.receiving {
			validate_expr_ctx(ctx, param.value)
		}
		for param in s.exceptions {
			validate_expr_ctx(ctx, param.value)
		}
	case ^ast.Call_System_Stmt:
		validate_expr_ctx(ctx, s.module)
		for param in s.params {
			validate_expr_ctx(ctx, param.id_name)
			validate_expr_ctx(ctx, param.field)
		}
	case ^ast.Call_Transaction_Stmt:
		validate_expr_ctx(ctx, s.transaction)
		if s.bdc_tab != nil {
			validate_expr_ctx(ctx, s.bdc_tab)
		}
		if s.mode != nil {
			validate_expr_ctx(ctx, s.mode)
		}
	case ^ast.Call_Transformation_Stmt:
		validate_expr_ctx(ctx, s.transformation)
		if s.options != nil {
			validate_expr_ctx(ctx, s.options)
		}
		if s.source != nil {
			validate_expr_ctx(ctx, s.source)
		}
		if s.result_stream != nil {
			validate_expr_ctx(ctx, s.result_stream)
		}
		for root in s.result_roots {
			validate_expr_ctx(ctx, &root.node)
		}
	case ^ast.Describe_Table_Stmt:
		validate_expr_ctx(ctx, s.table)
		validate_expr_ctx(ctx, s.lines_target)
	case ^ast.Open_Cursor_Stmt:
		validate_expr_ctx(ctx, &s.cursor.node)
		validate_stmt_ctx(ctx, s.select_stmt)
	case ^ast.Fetch_Cursor_Stmt:
		validate_expr_ctx(ctx, &s.cursor.node)
		validate_expr_ctx(ctx, s.into_target)
		validate_expr_ctx(ctx, s.package_size)
	case ^ast.Select_Stmt:
		validate_expr_ctx(ctx, s.into_target)
		validate_stmt_list_ctx(ctx, s.body[:])
	case ^ast.Method_Decl:
		validate_method_decl_ctx(ctx, s)
	case ^ast.Method_Chain_Decl:
		for d in s.decls {
			validate_method_decl_ctx(ctx, d)
		}
	case ^ast.Interfaces_Decl:
		mc := module_expr_ctx(ctx)
		for id in s.names {
			if id == nil {
				continue
			}
			lc := strings.to_lower(id.name)
			if sym, ok := mc.lookup_table.symbols[lc]; ok {
				if sym.kind != .Interface {
					add_diagnostic(
						ctx.diag_table,
						id.range,
						strings.concatenate(
							{
								"'",
								id.name,
								"' is not an interface; INTERFACES expects an interface",
							},
							context.temp_allocator,
						),
					)
				}
			} else {
				remote_ctx := ctx^
				remote_ctx.lookup_table = module_scope_lookup(ctx)
				maybe_add_remote_candidate(&remote_ctx, id.name, .Type_Name)
			}
		}
	case ^ast.Form_Decl:
		// Get child scope for form - use lookup_table for finding the scope
		form_name := strings.to_lower(s.ident.name)
		if sym, found := ctx.lookup_table.symbols[form_name]; found && sym.child_scope != nil {
			mod_lookup := ctx.module_lookup if ctx.module_lookup != nil else ctx.lookup_table
			// Create new context with child scope for lookups, but keep same diag_table
			child_ctx := Validation_Context{
				lookup_table  = sym.child_scope,
				diag_table    = ctx.diag_table,
				syntax_taint  = ctx.syntax_taint,
				module_lookup = mod_lookup,
			}
			validate_stmt_list_ctx(&child_ctx, s.body[:])
		}
	case ^ast.Class_Def_Decl:
		validate_class_def_header_ctx(ctx, s)
		// Validate class definition body
		class_name := strings.to_lower(s.ident.name)
		if sym, found := ctx.lookup_table.symbols[class_name]; found && sym.child_scope != nil {
			mod_lookup := ctx.module_lookup if ctx.module_lookup != nil else ctx.lookup_table
			child_ctx := Validation_Context{
				lookup_table  = sym.child_scope,
				diag_table    = ctx.diag_table,
				syntax_taint  = ctx.syntax_taint,
				module_lookup = mod_lookup,
			}
			for section in s.sections {
				validate_class_section_ctx(&child_ctx, section)
			}
		}
	case ^ast.Class_Impl_Decl:
		// Validate class implementation methods
		class_name := strings.to_lower(s.ident.name)
		if class_sym, found := ctx.lookup_table.symbols[class_name]; found && class_sym.child_scope != nil {
			for method in s.methods {
				#partial switch m in method.derived_stmt {
				case ^ast.Method_Impl:
					method_name := strings.to_lower(decl_name_from_expr(m.ident), context.temp_allocator)
					if method_sym, ok := class_sym.child_scope.symbols[method_name]; ok &&
					   method_sym.child_scope != nil {
						mod_lookup := ctx.module_lookup if ctx.module_lookup != nil else ctx.lookup_table
						instance_method := method_sym.kind == .Method && !method_sym.is_static
						child_ctx := Validation_Context{
							lookup_table        = method_sym.child_scope,
							diag_table          = ctx.diag_table,
							syntax_taint        = ctx.syntax_taint,
							module_lookup       = mod_lookup,
							enclosing_scope     = class_sym.child_scope,
							allow_me_identifier = instance_method,
							self_class_type     = class_sym.type_info if instance_method else nil,
						}
						validate_stmt_list_ctx(&child_ctx, m.body[:])
					} else {
						validate_stmt_list_ctx(ctx, m.body[:])
					}
				}
			}
		} else {
			for method in s.methods {
				#partial switch m in method.derived_stmt {
				case ^ast.Method_Impl:
					validate_stmt_list_ctx(ctx, m.body[:])
				}
			}
		}
	case ^ast.Interface_Decl:
		// Validate interface members
		iface_name := strings.to_lower(s.ident.name)
		if sym, found := ctx.lookup_table.symbols[iface_name]; found && sym.child_scope != nil {
			mod_lookup := ctx.module_lookup if ctx.module_lookup != nil else ctx.lookup_table
			child_ctx := Validation_Context{
				lookup_table  = sym.child_scope,
				diag_table    = ctx.diag_table,
				syntax_taint  = ctx.syntax_taint,
				module_lookup = mod_lookup,
			}
			for data_decl in s.data {
				validate_stmt_ctx(&child_ctx, data_decl)
			}
			for method_decl in s.methods {
				validate_stmt_ctx(&child_ctx, method_decl)
			}
		}
	case ^ast.Event_Block:
		// Get event child scope
		event_name := get_event_name(s.kind)
		if sym, found := ctx.lookup_table.symbols[event_name]; found && sym.child_scope != nil {
			mod_lookup := ctx.module_lookup if ctx.module_lookup != nil else ctx.lookup_table
			child_ctx := Validation_Context{
				lookup_table  = sym.child_scope,
				diag_table    = ctx.diag_table,
				syntax_taint  = ctx.syntax_taint,
				module_lookup = mod_lookup,
			}
			validate_stmt_list_ctx(&child_ctx, s.body[:])
		}
	case ^ast.Module_Decl:
		if s.ident != nil {
			module_name := strings.to_lower(s.ident.name)
			if sym, found := ctx.lookup_table.symbols[module_name]; found && sym.child_scope != nil {
				mod_lookup := ctx.module_lookup if ctx.module_lookup != nil else ctx.lookup_table
				child_ctx := Validation_Context{
					lookup_table  = sym.child_scope,
					diag_table    = ctx.diag_table,
					syntax_taint  = ctx.syntax_taint,
					module_lookup = mod_lookup,
				}
				validate_stmt_list_ctx(&child_ctx, s.body[:])
			}
		}
	}
}

// validate_class_section validates a class section's members
validate_class_section :: proc(table: ^SymbolTable, section: ^ast.Class_Section) {
	ctx := Validation_Context{lookup_table = table, diag_table = table}
	validate_class_section_ctx(&ctx, section)
}

validate_class_section_ctx :: proc(ctx: ^Validation_Context, section: ^ast.Class_Section) {
	for type_decl in section.types {
		validate_stmt_ctx(ctx, type_decl)
	}
	for data_decl in section.data {
		validate_stmt_ctx(ctx, data_decl)
	}
	for method_decl in section.methods {
		validate_stmt_ctx(ctx, method_decl)
	}
	for iface_decl in section.interfaces {
		validate_stmt_ctx(ctx, iface_decl)
	}
}

// validate_expr validates an expression and its sub-expressions
validate_expr :: proc(table: ^SymbolTable, expr: ^ast.Expr) {
	ctx := Validation_Context{lookup_table = table, diag_table = table}
	validate_expr_ctx(&ctx, expr)
}

// validate_expr_ctx_allow_inline_field_symbol validates an expression where FIELD-SYMBOL(<fs>) declares <fs>.
validate_expr_ctx_allow_inline_field_symbol :: proc(ctx: ^Validation_Context, expr: ^ast.Expr) {
	if ctx == nil || expr == nil {
		return
	}
	c := ctx^
	c.allow_inline_field_symbol_ident = true
	validate_expr_ctx(&c, expr)
}

validate_expr_ctx :: proc(ctx: ^Validation_Context, expr: ^ast.Expr) {
	if expr == nil {
		return
	}

	#partial switch e in expr.derived_expr {
	case ^ast.Ident:
		validate_ident_expr_ctx(ctx, e)

	case ^ast.Selector_Expr:
		// Validate the selector expression
		validate_selector_expr_ctx(ctx, e)
		// Also validate sub-expressions
		validate_expr_ctx(ctx, e.expr)
		if e.field != nil {
			// Component fields (sy-subrc, str-comp) are not global symbols; '=>' rhs isn't either.
			if field_id, fid_ok := e.field.derived_expr.(^ast.Ident); fid_ok {
				#partial switch e.op.kind {
				case .Minus, .Tilde, .Arrow:
					validate_component_selector_field(ctx, e, field_id)
				case .FatArrow:
				// static member name — not looked up as a scoped identifier
				case:
					validate_expr_ctx(ctx, e.field)
				}
			} else {
				validate_expr_ctx(ctx, e.field)
			}
		}

	case ^ast.Binary_Expr:
		validate_expr_ctx(ctx, e.left)
		validate_expr_ctx(ctx, e.right)

	case ^ast.Unary_Expr:
		validate_expr_ctx(ctx, e.expr)

	case ^ast.Paren_Expr:
		validate_expr_ctx(ctx, e.expr)

	case ^ast.Index_Expr:
		validate_expr_ctx(ctx, e.expr)
		if e.table_key_name != nil {
			validate_ident_expr_ctx(ctx, e.table_key_name)
		}
		validate_expr_ctx(ctx, e.index)

	case ^ast.Call_Expr:
		#partial switch callee in e.expr.derived_expr {
		case ^ast.Ident:
			if !is_builtin_function_name(callee.name) {
				validate_ident_expr_ctx(ctx, callee)
			}
		case:
			validate_expr_ctx(ctx, e.expr)
		}
		for arg in e.args {
			validate_expr_ctx(ctx, arg)
		}

	case ^ast.New_Expr:
		validate_type_expr_ctx(ctx, e.type_expr)
		for arg in e.args {
			validate_expr_ctx(ctx, arg)
		}

	case ^ast.Constructor_Expr:
		validate_type_expr_ctx(ctx, e.type_expr)
		for arg in e.args {
			validate_expr_ctx(ctx, arg)
		}

	case ^ast.Named_Arg:
		validate_expr_ctx(ctx, e.value)

	case ^ast.For_Expr:
		validate_expr_ctx(ctx, e.itab)
		validate_expr_ctx(ctx, e.where_cond)
		validate_expr_ctx(ctx, e.result_expr)
		for arg in e.result_args {
			validate_expr_ctx(ctx, arg)
		}

	case ^ast.Value_Row_Expr:
		for arg in e.args {
			validate_expr_ctx(ctx, arg)
		}

	case ^ast.Table_Type:
		validate_type_expr_ctx(ctx, e.elem)

	case ^ast.Ref_Type:
		validate_type_expr_ctx(ctx, e.target)

	case ^ast.Line_Type:
		validate_type_expr_ctx(ctx, e.table)
	case ^ast.Range_Type:
		validate_type_expr_ctx(ctx, e.elem)
	}
}

// validate_type_selector_expr_ctx validates ABAP type component chains (e.g. address_type-city-zipcode):
// the root must be a known type; each `-` suffix must be a structure component, not a standalone type name.
validate_type_selector_expr_ctx :: proc(ctx: ^Validation_Context, e: ^ast.Selector_Expr) {
	if ctx == nil || e == nil {
		return
	}
	#partial switch lhs in e.expr.derived_expr {
	case ^ast.Selector_Expr:
		validate_type_selector_expr_ctx(ctx, lhs)
	case ^ast.Ident:
		validate_type_ident_ctx(ctx, lhs)
	case:
		validate_type_expr_ctx(ctx, e.expr)
	}
	if e.field != nil {
		if id, ok := e.field.derived_expr.(^ast.Ident); ok {
			validate_component_selector_field(ctx, e, id)
		} else {
			validate_type_expr_ctx(ctx, e.field)
		}
	}
}

validate_type_expr_ctx :: proc(ctx: ^Validation_Context, expr: ^ast.Expr) {
	if expr == nil {
		return
	}

	#partial switch e in expr.derived_expr {
	case ^ast.Ident:
		validate_type_ident_ctx(ctx, e)
	case ^ast.Table_Type:
		validate_type_expr_ctx(ctx, e.elem)
	case ^ast.Ref_Type:
		validate_type_expr_ctx(ctx, e.target)
	case ^ast.Line_Type:
		validate_type_expr_ctx(ctx, e.table)
	case ^ast.Range_Type:
		validate_type_expr_ctx(ctx, e.elem)
	case ^ast.Selector_Expr:
		validate_type_selector_expr_ctx(ctx, e)
	}
}

validate_component_selector_field :: proc(ctx: ^Validation_Context, sel: ^ast.Selector_Expr, field_ident: ^ast.Ident) {
	if ctx == nil || sel == nil || field_ident == nil || ctx.lookup_table == nil || ctx.diag_table == nil {
		return
	}
	if sel.expr == nil {
		return
	}
	base_ty := expr_value_type(ctx.lookup_table, sel.expr)
	struct_ty := structure_for_field_lookup(ctx.lookup_table, base_ty)
	// Instance attribute access: me->attr (class members live on the class scope, not a Structure type).
	if struct_ty == nil &&
	   sel.op.kind == .Arrow &&
	   ctx.allow_me_identifier &&
	   ctx.self_class_type != nil {
		if id, ok := sel.expr.derived_expr.(^ast.Ident); ok && strings.to_lower(id.name) == "me" {
			mod := module_scope_lookup(ctx)
			if mod != nil &&
			   ctx.self_class_type.kind == .Named &&
			   len(ctx.self_class_type.name) > 0 {
				if csym, ok2 := mod.symbols[ctx.self_class_type.name]; ok2 &&
				   csym.kind == .Class &&
				   csym.child_scope != nil {
					field_lc := strings.to_lower(field_ident.name)
					for _, mem in csym.child_scope.symbols {
						if mem.kind != .Field {
							continue
						}
						if strings.to_lower(mem.name) == field_lc {
							return
						}
					}
					add_diagnostic(
						ctx.diag_table,
						field_ident.range,
						strings.concatenate(
							{"Unknown field '", field_ident.name, "' for class"},
							context.temp_allocator,
						),
					)
					return
				}
			}
		}
	}
	if struct_ty == nil {
		return
	}
	field_lc := strings.to_lower(field_ident.name)
	for f in struct_ty.fields {
		if f.name == field_lc {
			return
		}
	}
	add_diagnostic(
		ctx.diag_table,
		field_ident.range,
		strings.concatenate(
			{"Unknown field '", field_ident.name, "' for structure"},
			context.temp_allocator,
		),
	)
}

validate_ident_expr_ctx :: proc(ctx: ^Validation_Context, ident: ^ast.Ident) {
	if ctx == nil || ident == nil || ctx.lookup_table == nil || ctx.diag_table == nil {
		return
	}

	if ident.is_inline_field_symbol_decl {
		if !ctx.allow_inline_field_symbol_ident {
			add_diagnostic(
				ctx.diag_table,
				ident.range,
				"Inline FIELD-SYMBOL(...) is only valid after ASSIGNING (LOOP AT, READ TABLE, …) or as the target of ASSIGN ... TO FIELD-SYMBOL(...)",
			)
			return
		}
		// Declaration site: do not require an existing symbol (resolver may also register it).
		return
	}

	name := strings.to_lower(ident.name)
	if len(name) == 0 {
		return
	}

	if symbol_defined_in_validation_scope(ctx, name) {
		return
	}

	if name == "me" && ctx.allow_me_identifier {
		return
	}

	maybe_add_remote_candidate(ctx, ident.name, .Unknown_Symbol)
	add_diagnostic(
		ctx.diag_table,
		ident.range,
		strings.concatenate({"Unknown symbol '", ident.name, "'"}, context.temp_allocator),
	)
}

// validate_selector_expr validates that fat arrow (=>) is only used with class/interface names
validate_selector_expr :: proc(table: ^SymbolTable, sel: ^ast.Selector_Expr) {
	ctx := Validation_Context{lookup_table = table, diag_table = table}
	validate_selector_expr_ctx(&ctx, sel)
}

validate_selector_expr_ctx :: proc(ctx: ^Validation_Context, sel: ^ast.Selector_Expr) {
	if sel == nil {
		return
	}

	// Only validate fat arrow operator
	if sel.op.kind != .FatArrow {
		return
	}

	if sel.expr != nil {
		if _, ok := sel.expr.derived_expr.(^ast.Paren_Expr); ok {
			return
		}
	}

	// Get the left-hand side identifier
	left_name := get_selector_left_name(sel.expr)
	if left_name == "" {
		// Could not determine the name - might be a complex expression
		// Add a diagnostic since => requires a class/interface name on the left
		add_diagnostic(
			ctx.diag_table,
			sel.op.range,
			"Static access operator '=>' requires a class or interface name on the left side",
		)
		return
	}

	// Look up the name in the lookup table (may be merged project table)
	lower_name := strings.to_lower(left_name)
	if sym, found := ctx.lookup_table.symbols[lower_name]; found {
		// Check if it's a class or interface
		if sym.kind != .Class && sym.kind != .Interface {
			add_diagnostic(
				ctx.diag_table,
				sel.expr.range,
				strings.concatenate(
					{"'", left_name, "' is not a class or interface; '=>' can only be used for static access"},
					context.temp_allocator,
				),
			)
		}
	}
	// If not found in symbol table, it might be defined elsewhere (external class)
	// so we don't report an error for unknown symbols
	maybe_add_remote_candidate(ctx, left_name, .Static_Target)
}

// get_selector_left_name extracts the identifier name from the left side of a selector
get_selector_left_name :: proc(expr: ^ast.Expr) -> string {
	if expr == nil {
		return ""
	}

	#partial switch e in expr.derived_expr {
	case ^ast.Ident:
		return e.name
	case ^ast.Selector_Expr:
		// For nested selectors like package~class, get the rightmost field
		return ast.selector_field_ident_name(e)
	}

	return ""
}

maybe_add_remote_candidate :: proc(
	ctx: ^Validation_Context,
	name: string,
	kind: Remote_Candidate_Kind,
) {
	if !should_attempt_remote_lookup_name(name) {
		return
	}

	lower_name := strings.to_lower(strings.trim_space(name), context.temp_allocator)
	if len(lower_name) == 0 {
		return
	}
	if symbol_defined_in_validation_scope(ctx, lower_name) {
		return
	}

	add_remote_candidate(ctx.diag_table, name, kind)
}

should_attempt_remote_lookup_name :: proc(name: string) -> bool {
	trimmed := strings.trim_space(name)
	if len(trimmed) == 0 {
		return false
	}

	if trimmed[0] == '/' {
		return true
	}

	lower_name := strings.to_lower(trimmed, context.temp_allocator)
	return strings.has_prefix(lower_name, "z") || strings.has_prefix(lower_name, "y")
}
