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
		diag_table = diag_table,
	}

	for decl in file.decls {
		validate_stmt_ctx(&ctx, decl)
	}
}

// Validation context holds references to symbol tables for lookup and diagnostics
Validation_Context :: struct {
	lookup_table: ^SymbolTable, // Table to use for symbol lookups
	diag_table:   ^SymbolTable, // Table to store diagnostics in
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

	#partial switch s in stmt.derived_stmt {
	case ^ast.Data_Inline_Decl:
		validate_expr_ctx(ctx, s.value)
	case ^ast.Data_Typed_Decl:
		validate_expr_ctx(ctx, s.typed)
		validate_expr_ctx(ctx, s.value)
	case ^ast.Const_Decl:
		validate_expr_ctx(ctx, s.typed)
		validate_expr_ctx(ctx, s.value)
	case ^ast.Types_Decl:
		validate_expr_ctx(ctx, s.typed)
	case ^ast.Attr_Decl:
		validate_expr_ctx(ctx, s.typed)
		validate_expr_ctx(ctx, s.value)
	case ^ast.Assign_Stmt:
		for lhs in s.lhs {
			validate_expr_ctx(ctx, lhs)
		}
		for rhs in s.rhs {
			validate_expr_ctx(ctx, rhs)
		}
	case ^ast.Expr_Stmt:
		validate_expr_ctx(ctx, s.expr)
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
	case ^ast.Loop_Stmt:
		validate_expr_ctx(ctx, s.itab)
		validate_expr_ctx(ctx, s.into_target)
		validate_expr_ctx(ctx, s.assigning_target)
		validate_stmt_list_ctx(ctx, s.body[:])
	case ^ast.Read_Table_Stmt:
		validate_expr_ctx(ctx, s.itab)
		validate_expr_ctx(ctx, s.into_target)
		validate_expr_ctx(ctx, s.assigning_target)
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
				diag_table = ctx.diag_table,
			}
			validate_stmt_list_ctx(&child_ctx, s.body[:])
		}
	case ^ast.Class_Def_Decl:
		// Validate class definition
		class_name := strings.to_lower(s.ident.name)
		if sym, found := ctx.lookup_table.symbols[class_name]; found && sym.child_scope != nil {
			child_ctx := Validation_Context{
				lookup_table = sym.child_scope,
				diag_table = ctx.diag_table,
			}
			for section in s.sections {
				validate_class_section_ctx(&child_ctx, section)
			}
		}
	case ^ast.Class_Impl_Decl:
		// Validate class implementation methods
		for method in s.methods {
			#partial switch m in method.derived_stmt {
			case ^ast.Method_Impl:
				validate_stmt_list_ctx(ctx, m.body[:])
			}
		}
	case ^ast.Interface_Decl:
		// Validate interface members
		iface_name := strings.to_lower(s.ident.name)
		if sym, found := ctx.lookup_table.symbols[iface_name]; found && sym.child_scope != nil {
			child_ctx := Validation_Context{
				lookup_table = sym.child_scope,
				diag_table = ctx.diag_table,
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
				diag_table = ctx.diag_table,
			}
			validate_stmt_list_ctx(&child_ctx, s.body[:])
		}
	case ^ast.Module_Decl:
		if s.ident != nil {
			module_name := strings.to_lower(s.ident.name)
			if sym, found := ctx.lookup_table.symbols[module_name]; found && sym.child_scope != nil {
				child_ctx := Validation_Context{
					lookup_table = sym.child_scope,
					diag_table = ctx.diag_table,
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
	case ^ast.Selector_Expr:
		// Validate the selector expression
		validate_selector_expr_ctx(ctx, e)
		// Also validate sub-expressions
		validate_expr_ctx(ctx, e.expr)

	case ^ast.Binary_Expr:
		validate_expr_ctx(ctx, e.left)
		validate_expr_ctx(ctx, e.right)

	case ^ast.Unary_Expr:
		validate_expr_ctx(ctx, e.expr)

	case ^ast.Paren_Expr:
		validate_expr_ctx(ctx, e.expr)

	case ^ast.Index_Expr:
		validate_expr_ctx(ctx, e.expr)
		validate_expr_ctx(ctx, e.index)

	case ^ast.Call_Expr:
		validate_expr_ctx(ctx, e.expr)
		for arg in e.args {
			validate_expr_ctx(ctx, arg)
		}

	case ^ast.New_Expr:
		validate_expr_ctx(ctx, e.type_expr)
		for arg in e.args {
			validate_expr_ctx(ctx, arg)
		}

	case ^ast.Constructor_Expr:
		validate_expr_ctx(ctx, e.type_expr)
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

	case ^ast.Table_Type:
		validate_expr_ctx(ctx, e.elem)

	case ^ast.Ref_Type:
		validate_expr_ctx(ctx, e.target)

	case ^ast.Line_Type:
		validate_expr_ctx(ctx, e.table)
	}
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
		if e.field != nil {
			return e.field.name
		}
	}

	return ""
}
