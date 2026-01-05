package tests_parser

import "../../src/lang/ast"
import lexer "../../src/lang/lexer"
import "core:fmt"


ident :: proc(name: string) -> ^ast.Ident {
	node := ast.new(ast.Ident, {})
	node.name = name
	node.derived_expr = node
	return node
}

lit :: proc(lit: string) -> ^ast.Basic_Lit {
	node := ast.new(ast.Basic_Lit, {})
	node.tok.lit = lit
	node.derived_expr = node
	return node
}

data_inline :: proc(name: string, value: ast.Any_Expr) -> ^ast.Data_Inline_Decl {
	node := ast.new(ast.Data_Inline_Decl, {})
	node.ident = ident(name)

	#partial switch v in value {
	case ^ast.Basic_Lit:
		node.value = &v.node
	case ^ast.Ident:
		node.value = &v.node
	case:
		fmt.println("Unsupported type in data_inline builder:", value)
	}

	node.derived_stmt = node
	return node
}

data_single_typed :: proc(name: string, type_name: string) -> ^ast.Data_Typed_Decl {
	node := ast.new(ast.Data_Typed_Decl, {})
	node.ident = ident(name)
	node.typed = ident(type_name)
	node.derived_stmt = node
	return node
}

data_chain_typed :: proc(decls: ..struct {
		name:      string,
		type_name: string,
	}) -> ^ast.Data_Typed_Chain_Decl {
	node := ast.new(ast.Data_Typed_Chain_Decl, {})
	node.decls = make([dynamic]^ast.Data_Typed_Decl)
	for d in decls {
		append(&node.decls, data_single_typed(d.name, d.type_name))
	}
	node.derived_stmt = node
	return node
}

types_single :: proc(name: string, type_name: string) -> ^ast.Types_Decl {
	node := ast.new(ast.Types_Decl, {})
	node.ident = ident(name)
	node.typed = ident(type_name)
	node.derived_stmt = node
	return node
}

types_chain :: proc(decls: ..struct {
		name:      string,
		type_name: string,
	}) -> ^ast.Types_Chain_Decl {
	node := ast.new(ast.Types_Chain_Decl, {})
	node.decls = make([dynamic]^ast.Types_Decl)
	for d in decls {
		append(&node.decls, types_single(d.name, d.type_name))
	}
	node.derived_stmt = node
	return node
}

// Builder for structured types
Types_Struct_Builder :: struct {
	name:       string,
	components: [dynamic]^ast.Stmt,
}

types_struct_builder :: proc(name: string) -> Types_Struct_Builder {
	return Types_Struct_Builder{name = name, components = make([dynamic]^ast.Stmt)}
}

types_struct_with_field :: proc(
	builder: Types_Struct_Builder,
	field_name: string,
	field_type: string,
) -> Types_Struct_Builder {
	b := builder
	field := types_single(field_name, field_type)
	append(&b.components, &field.node)
	return b
}

types_struct_with_nested :: proc(
	builder: Types_Struct_Builder,
	nested: ^ast.Types_Struct_Decl,
) -> Types_Struct_Builder {
	b := builder
	append(&b.components, &nested.node)
	return b
}

types_struct_build :: proc(builder: Types_Struct_Builder) -> ^ast.Types_Struct_Decl {
	node := ast.new(ast.Types_Struct_Decl, {})
	node.ident = ident(builder.name)
	node.components = builder.components
	node.derived_stmt = node
	return node
}

// CONSTANTS builders

const_single :: proc(
	name: string,
	type_name: string,
	value: ast.Any_Expr = nil,
) -> ^ast.Const_Decl {
	node := ast.new(ast.Const_Decl, {})
	node.ident = ident(name)
	node.typed = ident(type_name)
	if value != nil {
		#partial switch v in value {
		case ^ast.Basic_Lit:
			node.value = &v.node
		case ^ast.Ident:
			node.value = &v.node
		}
	}
	node.derived_stmt = node
	return node
}

const_chain :: proc(decls: ..^ast.Const_Decl) -> ^ast.Const_Chain_Decl {
	node := ast.new(ast.Const_Chain_Decl, {})
	node.decls = make([dynamic]^ast.Const_Decl)
	for d in decls {
		append(&node.decls, d)
	}
	node.derived_stmt = node
	return node
}

// Builder for CONSTANTS structured types
Const_Struct_Builder :: struct {
	name:       string,
	components: [dynamic]^ast.Stmt,
}

const_struct_builder :: proc(name: string) -> Const_Struct_Builder {
	return Const_Struct_Builder{name = name, components = make([dynamic]^ast.Stmt)}
}

const_struct_with_field :: proc(
	builder: Const_Struct_Builder,
	field_name: string,
	field_type: string,
	value: ast.Any_Expr = nil,
) -> Const_Struct_Builder {
	b := builder
	field := const_single(field_name, field_type, value)
	append(&b.components, &field.node)
	return b
}

const_struct_build :: proc(builder: Const_Struct_Builder) -> ^ast.Const_Struct_Decl {
	node := ast.new(ast.Const_Struct_Decl, {})
	node.ident = ident(builder.name)
	node.components = builder.components
	node.derived_stmt = node
	return node
}

// Builder for DATA structured types
Data_Struct_Builder :: struct {
	name:       string,
	components: [dynamic]^ast.Stmt,
}

data_struct_builder :: proc(name: string) -> Data_Struct_Builder {
	return Data_Struct_Builder{name = name, components = make([dynamic]^ast.Stmt)}
}

data_struct_with_field :: proc(
	builder: Data_Struct_Builder,
	field_name: string,
	field_type: string,
	value: ast.Any_Expr = nil,
) -> Data_Struct_Builder {
	b := builder
	field := data_single_typed(field_name, field_type)
	if value != nil {
		#partial switch v in value {
		case ^ast.Basic_Lit:
			field.value = &v.node
		case ^ast.Ident:
			field.value = &v.node
		case ^ast.Selector_Expr:
			field.value = &v.node
		}
	}
	append(&b.components, &field.node)
	return b
}

data_struct_build :: proc(builder: Data_Struct_Builder) -> ^ast.Data_Struct_Decl {
	node := ast.new(ast.Data_Struct_Decl, {})
	node.ident = ident(builder.name)
	node.components = builder.components
	node.derived_stmt = node
	return node
}

selector :: proc(
	expr: ast.Any_Expr,
	op_kind: lexer.TokenKind,
	field_name: string,
) -> ^ast.Selector_Expr {
	node := ast.new(ast.Selector_Expr, {})
	#partial switch e in expr {
	case ^ast.Ident:
		node.expr = &e.node
	case ^ast.Selector_Expr:
		node.expr = &e.node
	}
	node.op.kind = op_kind
	node.field = ident(field_name)
	node.derived_expr = node
	return node
}

// Builder for NEW expressions
new_expr :: proc(
	type_name: string,
	is_inferred: bool = false,
	args: ..ast.Any_Expr,
) -> ^ast.New_Expr {
	node := ast.new(ast.New_Expr, {})
	node.is_inferred = is_inferred
	if !is_inferred && type_name != "" {
		node.type_expr = ident(type_name)
	}
	node.args = make([dynamic]^ast.Expr)
	for arg in args {
		#partial switch a in arg {
		case ^ast.Basic_Lit:
			append(&node.args, &a.node)
		case ^ast.Ident:
			append(&node.args, &a.node)
		}
	}
	node.derived_expr = node
	return node
}

// Builder for Named argument expressions (param = value)
named_arg :: proc(param_name: string, value: ast.Any_Expr) -> ^ast.Named_Arg {
	node := ast.new(ast.Named_Arg, {})
	node.name = ident(param_name)
	#partial switch v in value {
	case ^ast.Basic_Lit:
		node.value = &v.node
	case ^ast.Ident:
		node.value = &v.node
	case ^ast.Selector_Expr:
		node.value = &v.node
	case ^ast.Call_Expr:
		node.value = &v.node
	}
	node.derived_expr = node
	return node
}

// Builder for For expressions (FOR var IN itab WHERE (...))
for_expr :: proc(
	var_name: string,
	itab: ast.Any_Expr,
	where_cond: ast.Any_Expr = nil,
	result: ast.Any_Expr = nil,
) -> ^ast.For_Expr {
	node := ast.new(ast.For_Expr, {})
	node.var_name = ident(var_name)
	node.result_args = make([dynamic]^ast.Expr)
	#partial switch v in itab {
	case ^ast.Ident:
		node.itab = &v.node
	case ^ast.Selector_Expr:
		node.itab = &v.node
	}
	if where_cond != nil {
		#partial switch w in where_cond {
		case ^ast.Binary_Expr:
			node.where_cond = &w.node
		case ^ast.Ident:
			node.where_cond = &w.node
		}
	}
	if result != nil {
		#partial switch r in result {
		case ^ast.Ident:
			node.result_expr = &r.node
			append(&node.result_args, &r.node)
		case ^ast.Selector_Expr:
			node.result_expr = &r.node
			append(&node.result_args, &r.node)
		case ^ast.Named_Arg:
			node.result_expr = &r.node
			append(&node.result_args, &r.node)
		}
	}
	node.derived_expr = node
	return node
}

// Builder for Call expressions
call_expr :: proc(expr: ast.Any_Expr, args: ..ast.Any_Expr) -> ^ast.Call_Expr {
	node := ast.new(ast.Call_Expr, {})
	#partial switch e in expr {
	case ^ast.Ident:
		node.expr = &e.node
	case ^ast.Selector_Expr:
		node.expr = &e.node
	case ^ast.Call_Expr:
		node.expr = &e.node
	}
	arg_list := make([]^ast.Expr, len(args))
	for arg, i in args {
		#partial switch a in arg {
		case ^ast.Basic_Lit:
			arg_list[i] = &a.node
		case ^ast.Ident:
			arg_list[i] = &a.node
		case ^ast.Selector_Expr:
			arg_list[i] = &a.node
		case ^ast.Named_Arg:
			arg_list[i] = &a.node
		}
	}
	node.args = arg_list
	node.derived_expr = node
	return node
}

new_expr_inferred :: proc(args: ..ast.Any_Expr) -> ^ast.New_Expr {
	return new_expr("", true, ..args)
}

// Builder for binary expressions (for logical/comparison/arithmetic expressions)
binary_expr :: proc(left: ast.Any_Expr, op_lit: string, right: ast.Any_Expr) -> ^ast.Binary_Expr {
	node := ast.new(ast.Binary_Expr, {})
	#partial switch l in left {
	case ^ast.Ident:
		node.left = &l.node
	case ^ast.Basic_Lit:
		node.left = &l.node
	case ^ast.Binary_Expr:
		node.left = &l.node
	case ^ast.Predicate_Expr:
		node.left = &l.node
	case ^ast.Unary_Expr:
		node.left = &l.node
	case ^ast.Paren_Expr:
		node.left = &l.node
	}
	node.op.lit = op_lit
	// Set the op kind based on the literal
	switch op_lit {
	case "+":
		node.op.kind = .Plus
	case "-":
		node.op.kind = .Minus
	case "*":
		node.op.kind = .Star
	case "/":
		node.op.kind = .Slash
	case "&":
		node.op.kind = .Ampersand
	case:
		node.op.kind = .Ident // For keyword operators like MOD, DIV, AND, OR
	}
	#partial switch r in right {
	case ^ast.Ident:
		node.right = &r.node
	case ^ast.Basic_Lit:
		node.right = &r.node
	case ^ast.Binary_Expr:
		node.right = &r.node
	case ^ast.Predicate_Expr:
		node.right = &r.node
	case ^ast.Unary_Expr:
		node.right = &r.node
	case ^ast.Paren_Expr:
		node.right = &r.node
	}
	node.derived_expr = node
	return node
}

// Builder for unary expressions
unary_expr :: proc(op_lit: string, expr: ast.Any_Expr) -> ^ast.Unary_Expr {
	node := ast.new(ast.Unary_Expr, {})
	node.op.lit = op_lit
	switch op_lit {
	case "+":
		node.op.kind = .Plus
	case "-":
		node.op.kind = .Minus
	case:
		node.op.kind = .Ident
	}
	#partial switch e in expr {
	case ^ast.Ident:
		node.expr = &e.node
	case ^ast.Basic_Lit:
		node.expr = &e.node
	case ^ast.Binary_Expr:
		node.expr = &e.node
	case ^ast.Unary_Expr:
		node.expr = &e.node
	case ^ast.Paren_Expr:
		node.expr = &e.node
	}
	node.derived_expr = node
	return node
}

// Builder for parenthesized expressions
paren_expr :: proc(inner: ast.Any_Expr) -> ^ast.Paren_Expr {
	node := ast.new(ast.Paren_Expr, {})
	#partial switch e in inner {
	case ^ast.Ident:
		node.expr = &e.node
	case ^ast.Basic_Lit:
		node.expr = &e.node
	case ^ast.Binary_Expr:
		node.expr = &e.node
	case ^ast.Unary_Expr:
		node.expr = &e.node
	case ^ast.Paren_Expr:
		node.expr = &e.node
	}
	node.derived_expr = node
	return node
}

// Builder for predicate expressions (IS INITIAL, IS SUPPLIED, etc.)
predicate_expr :: proc(
	expr: ast.Any_Expr,
	predicate: ast.Predicate_Kind,
	is_negated: bool = false,
) -> ^ast.Predicate_Expr {
	node := ast.new(ast.Predicate_Expr, {})
	#partial switch e in expr {
	case ^ast.Ident:
		node.expr = &e.node
	case ^ast.Basic_Lit:
		node.expr = &e.node
	}
	node.predicate = predicate
	node.is_negated = is_negated
	node.derived_expr = node
	return node
}

// Builder for IF statements
If_Stmt_Builder :: struct {
	cond:            ast.Any_Expr,
	body:            [dynamic]^ast.Stmt,
	elseif_branches: [dynamic]^ast.Elseif_Branch,
	else_body:       [dynamic]^ast.Stmt,
}

if_stmt_builder :: proc(cond: ast.Any_Expr) -> If_Stmt_Builder {
	return If_Stmt_Builder {
		cond = cond,
		body = make([dynamic]^ast.Stmt),
		elseif_branches = make([dynamic]^ast.Elseif_Branch),
		else_body = make([dynamic]^ast.Stmt),
	}
}

if_with_body :: proc(builder: If_Stmt_Builder, stmts: ..ast.Any_Stmt) -> If_Stmt_Builder {
	b := builder
	for stmt in stmts {
		#partial switch s in stmt {
		case ^ast.Assign_Stmt:
			append(&b.body, &s.node)
		case ^ast.Data_Typed_Decl:
			append(&b.body, &s.node)
		case ^ast.Data_Inline_Decl:
			append(&b.body, &s.node)
		case ^ast.Expr_Stmt:
			append(&b.body, &s.node)
		case ^ast.If_Stmt:
			append(&b.body, &s.node)
		}
	}
	return b
}

if_with_elseif :: proc(
	builder: If_Stmt_Builder,
	cond: ast.Any_Expr,
	stmts: ..ast.Any_Stmt,
) -> If_Stmt_Builder {
	b := builder
	branch := ast.new(ast.Elseif_Branch, {})
	#partial switch c in cond {
	case ^ast.Ident:
		branch.cond = &c.node
	case ^ast.Basic_Lit:
		branch.cond = &c.node
	case ^ast.Binary_Expr:
		branch.cond = &c.node
	case ^ast.Predicate_Expr:
		branch.cond = &c.node
	}
	branch.body = make([dynamic]^ast.Stmt)
	for stmt in stmts {
		#partial switch s in stmt {
		case ^ast.Assign_Stmt:
			append(&branch.body, &s.node)
		case ^ast.Data_Typed_Decl:
			append(&branch.body, &s.node)
		case ^ast.Expr_Stmt:
			append(&branch.body, &s.node)
		}
	}
	append(&b.elseif_branches, branch)
	return b
}

if_with_else :: proc(builder: If_Stmt_Builder, stmts: ..ast.Any_Stmt) -> If_Stmt_Builder {
	b := builder
	for stmt in stmts {
		#partial switch s in stmt {
		case ^ast.Assign_Stmt:
			append(&b.else_body, &s.node)
		case ^ast.Data_Typed_Decl:
			append(&b.else_body, &s.node)
		case ^ast.Expr_Stmt:
			append(&b.else_body, &s.node)
		}
	}
	return b
}

if_build :: proc(builder: If_Stmt_Builder) -> ^ast.If_Stmt {
	node := ast.new(ast.If_Stmt, {})
	#partial switch c in builder.cond {
	case ^ast.Ident:
		node.cond = &c.node
	case ^ast.Basic_Lit:
		node.cond = &c.node
	case ^ast.Binary_Expr:
		node.cond = &c.node
	case ^ast.Predicate_Expr:
		node.cond = &c.node
	}
	node.body = builder.body
	node.elseif_branches = builder.elseif_branches
	node.else_body = builder.else_body
	node.derived_stmt = node
	return node
}

assign :: proc(lhs: ast.Any_Expr, rhs: ast.Any_Expr) -> ^ast.Assign_Stmt {
	node := ast.new(ast.Assign_Stmt, {})
	node.lhs = make([]^ast.Expr, 1)
	node.rhs = make([]^ast.Expr, 1)

	#partial switch l in lhs {
	case ^ast.Ident:
		node.lhs[0] = &l.node
	case ^ast.Selector_Expr:
		node.lhs[0] = &l.node
	}

	#partial switch r in rhs {
	case ^ast.Basic_Lit:
		node.rhs[0] = &r.node
	case ^ast.Ident:
		node.rhs[0] = &r.node
	case ^ast.Selector_Expr:
		node.rhs[0] = &r.node
	}

	node.op.kind = .Eq
	node.derived_stmt = node
	return node
}

downcast_assign :: proc(lhs: ast.Any_Expr, rhs: ast.Any_Expr) -> ^ast.Assign_Stmt {
	node := ast.new(ast.Assign_Stmt, {})
	node.lhs = make([]^ast.Expr, 1)
	node.rhs = make([]^ast.Expr, 1)

	#partial switch l in lhs {
	case ^ast.Ident:
		node.lhs[0] = &l.node
	case ^ast.Selector_Expr:
		node.lhs[0] = &l.node
	}

	#partial switch r in rhs {
	case ^ast.Basic_Lit:
		node.rhs[0] = &r.node
	case ^ast.Ident:
		node.rhs[0] = &r.node
	case ^ast.Selector_Expr:
		node.rhs[0] = &r.node
	}

	node.op.kind = .QuestionEq
	node.derived_stmt = node
	return node
}

form_param :: proc(
	name: string,
	type_name: string = "",
	kind: ast.Form_Param_Kind = .Using,
) -> ^ast.Form_Param {
	node := ast.new(ast.Form_Param, {})
	node.kind = kind
	node.ident = ident(name)
	if type_name != "" {
		node.typed = ident(type_name)
	}
	return node
}

Form_Decl_Builder :: struct {
	name:            string,
	tables_params:   [dynamic]^ast.Form_Param,
	using_params:    [dynamic]^ast.Form_Param,
	changing_params: [dynamic]^ast.Form_Param,
	body:            [dynamic]^ast.Stmt,
}

form_decl_builder :: proc(name: string) -> Form_Decl_Builder {
	return Form_Decl_Builder {
		name = name,
		tables_params = make([dynamic]^ast.Form_Param),
		using_params = make([dynamic]^ast.Form_Param),
		changing_params = make([dynamic]^ast.Form_Param),
		body = make([dynamic]^ast.Stmt),
	}
}

form_with_tables :: proc(
	builder: Form_Decl_Builder,
	params: ..^ast.Form_Param,
) -> Form_Decl_Builder {
	b := builder
	for param in params {
		param.kind = .Tables
		append(&b.tables_params, param)
	}
	return b
}

form_with_using :: proc(
	builder: Form_Decl_Builder,
	params: ..^ast.Form_Param,
) -> Form_Decl_Builder {
	b := builder
	for param in params {
		param.kind = .Using
		append(&b.using_params, param)
	}
	return b
}

form_with_changing :: proc(
	builder: Form_Decl_Builder,
	params: ..^ast.Form_Param,
) -> Form_Decl_Builder {
	b := builder
	for param in params {
		param.kind = .Changing
		append(&b.changing_params, param)
	}
	return b
}

form_with_body :: proc(builder: Form_Decl_Builder, stmts: ..ast.Any_Stmt) -> Form_Decl_Builder {
	b := builder
	for stmt in stmts {
		#partial switch s in stmt {
		case ^ast.Assign_Stmt:
			append(&b.body, &s.node)
		case ^ast.Data_Typed_Decl:
			append(&b.body, &s.node)
		case ^ast.Data_Inline_Decl:
			append(&b.body, &s.node)
		}
	}
	return b
}

form_build :: proc(builder: Form_Decl_Builder) -> ^ast.Form_Decl {
	node := ast.new(ast.Form_Decl, {})
	node.ident = ident(builder.name)
	node.tables_params = builder.tables_params
	node.using_params = builder.using_params
	node.changing_params = builder.changing_params
	node.body = builder.body
	node.derived_stmt = node
	return node
}

delete_stmt_where :: proc(target: ast.Any_Expr, cond: ast.Any_Expr) -> ^ast.Delete_Stmt {
	node := ast.new(ast.Delete_Stmt, {})
	node.kind = .Where

	#partial switch t in target {
	case ^ast.Ident:
		node.target = &t.node
	}

	#partial switch c in cond {
	case ^ast.Binary_Expr:
		node.where_cond = &c.node
	case ^ast.Predicate_Expr:
		node.where_cond = &c.node
	}

	node.derived_stmt = node
	return node
}