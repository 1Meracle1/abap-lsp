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
		lookup_table = lookup_table,
		diag_table   = diag_table,
		syntax_taint = build_syntax_taint_ranges(file, context.temp_allocator),
	}

	for decl in file.decls {
		validate_stmt_ctx(&ctx, decl)
	}
}

// Validation context holds references to symbol tables for lookup and diagnostics
Validation_Context :: struct {
	lookup_table: ^SymbolTable, // Table to use for symbol lookups
	diag_table:   ^SymbolTable, // Table to store diagnostics in
	syntax_taint: []lexer.TextRange,
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
		validate_expr_ctx(ctx, s.target)
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
		validate_expr_ctx(ctx, s.assigning_target)
		validate_stmt_list_ctx(ctx, s.body[:])
	case ^ast.Loop_At_Control_Stmt:
		validate_expr_ctx(ctx, s.field)
		validate_stmt_list_ctx(ctx, s.body[:])
	case ^ast.Read_Table_Stmt:
		validate_expr_ctx(ctx, s.itab)
		validate_expr_ctx(ctx, s.into_target)
		validate_expr_ctx(ctx, s.assigning_target)
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
	case ^ast.Form_Decl:
		// Get child scope for form - use lookup_table for finding the scope
		form_name := strings.to_lower(s.ident.name)
		if sym, found := ctx.lookup_table.symbols[form_name]; found && sym.child_scope != nil {
			// Create new context with child scope for lookups, but keep same diag_table
			child_ctx := Validation_Context{
				lookup_table = sym.child_scope,
				diag_table   = ctx.diag_table,
				syntax_taint = ctx.syntax_taint,
			}
			validate_stmt_list_ctx(&child_ctx, s.body[:])
		}
	case ^ast.Class_Def_Decl:
		// Validate class definition
		class_name := strings.to_lower(s.ident.name)
		if sym, found := ctx.lookup_table.symbols[class_name]; found && sym.child_scope != nil {
			child_ctx := Validation_Context{
				lookup_table = sym.child_scope,
				diag_table   = ctx.diag_table,
				syntax_taint = ctx.syntax_taint,
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
					method_name := strings.to_lower(get_decl_name(m.ident), context.temp_allocator)
					if method_sym, ok := class_sym.child_scope.symbols[method_name]; ok &&
					   method_sym.child_scope != nil {
						child_ctx := Validation_Context{
							lookup_table = method_sym.child_scope,
							diag_table   = ctx.diag_table,
							syntax_taint = ctx.syntax_taint,
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
			child_ctx := Validation_Context{
				lookup_table = sym.child_scope,
				diag_table   = ctx.diag_table,
				syntax_taint = ctx.syntax_taint,
			}
			for data_decl in s.data {
				validate_stmt_ctx(&child_ctx, data_decl)
			}
		}
	case ^ast.Event_Block:
		// Get event child scope
		event_name := get_event_name(s.kind)
		if sym, found := ctx.lookup_table.symbols[event_name]; found && sym.child_scope != nil {
			child_ctx := Validation_Context{
				lookup_table = sym.child_scope,
				diag_table   = ctx.diag_table,
				syntax_taint = ctx.syntax_taint,
			}
			validate_stmt_list_ctx(&child_ctx, s.body[:])
		}
	case ^ast.Module_Decl:
		if s.ident != nil {
			module_name := strings.to_lower(s.ident.name)
			if sym, found := ctx.lookup_table.symbols[module_name]; found && sym.child_scope != nil {
				child_ctx := Validation_Context{
					lookup_table = sym.child_scope,
					diag_table   = ctx.diag_table,
					syntax_taint = ctx.syntax_taint,
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
}

// validate_expr validates an expression and its sub-expressions
validate_expr :: proc(table: ^SymbolTable, expr: ^ast.Expr) {
	ctx := Validation_Context{lookup_table = table, diag_table = table}
	validate_expr_ctx(&ctx, expr)
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
			validate_expr_ctx(ctx, e.field)
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
		validate_expr_ctx(ctx, e.expr)
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

validate_type_expr_ctx :: proc(ctx: ^Validation_Context, expr: ^ast.Expr) {
	if expr == nil {
		return
	}

	#partial switch e in expr.derived_expr {
	case ^ast.Ident:
		maybe_add_remote_candidate(ctx, e.name, .Type_Name)
	case ^ast.Table_Type:
		validate_type_expr_ctx(ctx, e.elem)
	case ^ast.Ref_Type:
		validate_type_expr_ctx(ctx, e.target)
	case ^ast.Line_Type:
		validate_type_expr_ctx(ctx, e.table)
	case ^ast.Range_Type:
		validate_type_expr_ctx(ctx, e.elem)
	case ^ast.Selector_Expr:
		validate_type_expr_ctx(ctx, e.expr)
		if e.field != nil {
			validate_type_expr_ctx(ctx, e.field)
		}
	}
}

validate_ident_expr_ctx :: proc(ctx: ^Validation_Context, ident: ^ast.Ident) {
	if ctx == nil || ident == nil || ctx.lookup_table == nil || ctx.diag_table == nil {
		return
	}

	name := strings.to_lower(ident.name)
	if len(name) == 0 {
		return
	}

	if name in ctx.lookup_table.symbols {
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
	if ctx.lookup_table != nil && lower_name in ctx.lookup_table.symbols {
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
