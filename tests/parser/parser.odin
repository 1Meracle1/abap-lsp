package tests_parser

import "../../src/lang/ast"
import lexer "../../src/lang/lexer"
import "../../src/lang/parser"
import "core:fmt"
import "core:testing"

// --- Builders ---

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
	return Types_Struct_Builder{
		name       = name,
		components = make([dynamic]^ast.Stmt),
	}
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

// --- Checkers ---

check_expr :: proc(
	t: ^testing.T,
	expected: ast.Any_Expr,
	actual: ^ast.Expr,
	loc := #caller_location,
) {
	if actual == nil {
		testing.expect(t, expected == nil, "Expected non-nil expr, got nil", loc = loc)
		return
	}

	if expected == nil {
		testing.expect(t, actual == nil, "Expected nil expr, got non-nil", loc = loc)
		return
	}

	actual_derived := actual.derived_expr

	#partial switch ex in expected {
	case ^ast.Basic_Lit:
		ac, ok := actual_derived.(^ast.Basic_Lit)
		if !testing.expect(t, ok, fmt.tprintf("Expected Basic_Lit, got %T", actual_derived), loc = loc) do return
		testing.expect(
			t,
			ex.tok.lit == ac.tok.lit,
			fmt.tprintf("Expected lit '%s', got '%s'", ex.tok.lit, ac.tok.lit),
			loc = loc,
		)

	case ^ast.Ident:
		ac, ok := actual_derived.(^ast.Ident)
		if !testing.expect(t, ok, fmt.tprintf("Expected Ident, got %T", actual_derived), loc = loc) do return
		testing.expect(
			t,
			ex.name == ac.name,
			fmt.tprintf("Expected ident '%s', got '%s'", ex.name, ac.name),
			loc = loc,
		)

	case ^ast.Selector_Expr:
		ac, ok := actual_derived.(^ast.Selector_Expr)
		if !testing.expect(t, ok, fmt.tprintf("Expected Selector_Expr, got %T", actual_derived), loc = loc) do return
		testing.expect(
			t,
			ex.op.kind == ac.op.kind,
			fmt.tprintf("Expected selector op '%v', got '%v'", ex.op.kind, ac.op.kind),
			loc = loc,
		)
		if testing.expect(t, ac.field != nil, "Actual selector field is nil", loc = loc) {
			testing.expect(
				t,
				ex.field.name == ac.field.name,
				fmt.tprintf(
					"Expected selector field '%s', got '%s'",
					ex.field.name,
					ac.field.name,
				),
				loc = loc,
			)
		}
		check_expr(t, ex.expr.derived_expr, ac.expr, loc = loc)

	case:
		testing.expect(
			t,
			false,
			fmt.tprintf("Unsupported expected type in check_expr: %T", expected),
			loc = loc,
		)
	}
}

check_stmt :: proc(
	t: ^testing.T,
	expected: ast.Any_Stmt,
	actual: ^ast.Stmt,
	loc := #caller_location,
) {
	if actual == nil {
		testing.expect(t, expected == nil, "Expected non-nil stmt, got nil", loc = loc)
		return
	}

	if expected == nil {
		testing.expect(t, actual == nil, "Expected nil stmt, got non-nil", loc = loc)
		return
	}

	actual_derived := actual.derived_stmt

	#partial switch ex in expected {
	case ^ast.Data_Inline_Decl:
		ac, ok := actual_derived.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Data_Inline_Decl, got %T", actual_derived), loc = loc) do return

		if testing.expect(t, ac.ident != nil, "Actual ident is nil", loc = loc) {
			testing.expect(t, ex.ident != nil, "Expected ident is nil") // Should be enforced by builder
			testing.expect(
				t,
				ex.ident.name == ac.ident.name,
				fmt.tprintf("Expected Decl ident '%s', got '%s'", ex.ident.name, ac.ident.name),
				loc = loc,
			)
		}

		check_expr(t, ex.value.derived_expr, ac.value, loc = loc)

	case ^ast.Data_Typed_Decl:
		ac, ok := actual_derived.(^ast.Data_Typed_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Data_Typed_Decl, got %T", actual_derived), loc = loc) do return

		if testing.expect(t, ac.ident != nil, "Actual ident is nil", loc = loc) {
			testing.expect(t, ex.ident != nil, "Expected ident is nil") // Should be enforced by builder
			testing.expect(
				t,
				ex.ident.name == ac.ident.name,
				fmt.tprintf("Expected Decl ident '%s', got '%s'", ex.ident.name, ac.ident.name),
				loc = loc,
			)
		}

		check_expr(t, ex.typed.derived_expr, ac.typed, loc = loc)

	case ^ast.Data_Typed_Chain_Decl:
		ac, ok := actual_derived.(^ast.Data_Typed_Chain_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Data_Typed_Chain_Decl, got %T", actual_derived), loc = loc) do return

		if !testing.expect(t, len(ex.decls) == len(ac.decls), fmt.tprintf("Expected %d decls in chain, got %d", len(ex.decls), len(ac.decls)), loc = loc) do return

		for i := 0; i < len(ex.decls); i += 1 {
			ex_decl := ex.decls[i]
			ac_decl := ac.decls[i]

			if testing.expect(
				t,
				ac_decl.ident != nil,
				fmt.tprintf("Actual ident[%d] is nil", i),
				loc = loc,
			) {
				testing.expect(
					t,
					ex_decl.ident.name == ac_decl.ident.name,
					fmt.tprintf(
						"Expected chain decl[%d] ident '%s', got '%s'",
						i,
						ex_decl.ident.name,
						ac_decl.ident.name,
					),
					loc = loc,
				)
			}

			check_expr(t, ex_decl.typed.derived_expr, ac_decl.typed, loc = loc)
		}

	case ^ast.Types_Decl:
		ac, ok := actual_derived.(^ast.Types_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Types_Decl, got %T", actual_derived), loc = loc) do return

		if testing.expect(t, ac.ident != nil, "Actual ident is nil", loc = loc) {
			testing.expect(t, ex.ident != nil, "Expected ident is nil")
			testing.expect(
				t,
				ex.ident.name == ac.ident.name,
				fmt.tprintf("Expected Types_Decl ident '%s', got '%s'", ex.ident.name, ac.ident.name),
				loc = loc,
			)
		}

		check_expr(t, ex.typed.derived_expr, ac.typed, loc = loc)

	case ^ast.Types_Chain_Decl:
		ac, ok := actual_derived.(^ast.Types_Chain_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Types_Chain_Decl, got %T", actual_derived), loc = loc) do return

		if !testing.expect(t, len(ex.decls) == len(ac.decls), fmt.tprintf("Expected %d decls in types chain, got %d", len(ex.decls), len(ac.decls)), loc = loc) do return

		for i := 0; i < len(ex.decls); i += 1 {
			ex_decl := ex.decls[i]
			ac_decl := ac.decls[i]

			if testing.expect(
				t,
				ac_decl.ident != nil,
				fmt.tprintf("Actual types ident[%d] is nil", i),
				loc = loc,
			) {
				testing.expect(
					t,
					ex_decl.ident.name == ac_decl.ident.name,
					fmt.tprintf(
						"Expected types chain decl[%d] ident '%s', got '%s'",
						i,
						ex_decl.ident.name,
						ac_decl.ident.name,
					),
					loc = loc,
				)
			}

			check_expr(t, ex_decl.typed.derived_expr, ac_decl.typed, loc = loc)
		}

	case ^ast.Types_Struct_Decl:
		ac, ok := actual_derived.(^ast.Types_Struct_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Types_Struct_Decl, got %T", actual_derived), loc = loc) do return

		if testing.expect(t, ac.ident != nil, "Actual struct ident is nil", loc = loc) {
			testing.expect(
				t,
				ex.ident.name == ac.ident.name,
				fmt.tprintf("Expected struct name '%s', got '%s'", ex.ident.name, ac.ident.name),
				loc = loc,
			)
		}

		if !testing.expect(t, len(ex.components) == len(ac.components), fmt.tprintf("Expected %d components, got %d", len(ex.components), len(ac.components)), loc = loc) do return

		for i := 0; i < len(ex.components); i += 1 {
			check_stmt(t, ex.components[i].derived_stmt, ac.components[i], loc = loc)
		}

	case ^ast.Assign_Stmt:
		ac, ok := actual_derived.(^ast.Assign_Stmt)
		if !testing.expect(t, ok, fmt.tprintf("Expected Assign_Stmt, got %T", actual_derived), loc = loc) do return

		if !testing.expect(t, len(ex.lhs) == len(ac.lhs), fmt.tprintf("Expected %d lhs, got %d", len(ex.lhs), len(ac.lhs)), loc = loc) do return
		if !testing.expect(t, len(ex.rhs) == len(ac.rhs), fmt.tprintf("Expected %d rhs, got %d", len(ex.rhs), len(ac.rhs)), loc = loc) do return

		for i := 0; i < len(ex.lhs); i += 1 {
			check_expr(t, ex.lhs[i].derived_expr, ac.lhs[i], loc = loc)
		}

		testing.expect(
			t,
			ex.op.kind == ac.op.kind,
			fmt.tprintf("Expected assign op '%v', got '%v'", ex.op.kind, ac.op.kind),
			loc = loc,
		)

		for i := 0; i < len(ex.rhs); i += 1 {
			check_expr(t, ex.rhs[i].derived_expr, ac.rhs[i], loc = loc)
		}

	case ^ast.Form_Decl:
		ac, ok := actual_derived.(^ast.Form_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Form_Decl, got %T", actual_derived), loc = loc) do return

		if testing.expect(t, ac.ident != nil, "Actual form ident is nil", loc = loc) {
			testing.expect(
				t,
				ex.ident.name == ac.ident.name,
				fmt.tprintf("Expected form name '%s', got '%s'", ex.ident.name, ac.ident.name),
				loc = loc,
			)
		}

		// Check TABLES params
		if !testing.expect(t, len(ex.tables_params) == len(ac.tables_params), fmt.tprintf("Expected %d TABLES params, got %d", len(ex.tables_params), len(ac.tables_params)), loc = loc) do return
		for i := 0; i < len(ex.tables_params); i += 1 {
			check_form_param(t, ex.tables_params[i], ac.tables_params[i], loc = loc)
		}

		// Check USING params
		if !testing.expect(t, len(ex.using_params) == len(ac.using_params), fmt.tprintf("Expected %d USING params, got %d", len(ex.using_params), len(ac.using_params)), loc = loc) do return
		for i := 0; i < len(ex.using_params); i += 1 {
			check_form_param(t, ex.using_params[i], ac.using_params[i], loc = loc)
		}

		// Check CHANGING params
		if !testing.expect(t, len(ex.changing_params) == len(ac.changing_params), fmt.tprintf("Expected %d CHANGING params, got %d", len(ex.changing_params), len(ac.changing_params)), loc = loc) do return
		for i := 0; i < len(ex.changing_params); i += 1 {
			check_form_param(t, ex.changing_params[i], ac.changing_params[i], loc = loc)
		}

		// Check body statements
		if !testing.expect(t, len(ex.body) == len(ac.body), fmt.tprintf("Expected %d body statements, got %d", len(ex.body), len(ac.body)), loc = loc) do return
		for i := 0; i < len(ex.body); i += 1 {
			check_stmt(t, ex.body[i].derived_stmt, ac.body[i], loc = loc)
		}

	case:
		testing.expect(
			t,
			false,
			fmt.tprintf("Unsupported expected type in check_stmt: %T", expected),
			loc = loc,
		)
	}
}

check_form_param :: proc(
	t: ^testing.T,
	expected: ^ast.Form_Param,
	actual: ^ast.Form_Param,
	loc := #caller_location,
) {
	if actual == nil {
		testing.expect(t, expected == nil, "Expected non-nil form param, got nil", loc = loc)
		return
	}

	testing.expect(
		t,
		expected.kind == actual.kind,
		fmt.tprintf("Expected param kind '%v', got '%v'", expected.kind, actual.kind),
		loc = loc,
	)

	if testing.expect(t, actual.ident != nil, "Actual param ident is nil", loc = loc) {
		testing.expect(
			t,
			expected.ident.name == actual.ident.name,
			fmt.tprintf(
				"Expected param name '%s', got '%s'",
				expected.ident.name,
				actual.ident.name,
			),
			loc = loc,
		)
	}

	if expected.typed != nil {
		check_expr(t, expected.typed.derived_expr, actual.typed, loc = loc)
	} else {
		testing.expect(t, actual.typed == nil, "Expected nil typed, got non-nil", loc = loc)
	}
}

// --- Tests ---

@(test)
basic_inline_data_decl_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(lv_value) = 1.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := data_inline("lv_value", lit("1"))
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
basic_inline_data_decl_with_preceding_comment_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `*DATA lv_val TYPE i. 
DATA(lv_value) = 1.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := data_inline("lv_value", lit("1"))
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
basic_single_data_typed_decl_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA lv_value TYPE i.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := data_single_typed("lv_value", "i")
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
basic_chain_data_typed_decl_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA: lv_var1 TYPE i,
      lv_var2 TYPE f.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := data_chain_typed({"lv_var1", "i"}, {"lv_var2", "f"})
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
chain_data_typed_decl_three_vars_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA: lv_int TYPE i, lv_float TYPE f, lv_string TYPE string.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := data_chain_typed({"lv_int", "i"}, {"lv_float", "f"}, {"lv_string", "string"})
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
simple_assignment_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `lv_var = 1.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := assign(ident("lv_var"), lit("1"))
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
assignment_with_struct_field_access_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `lv_struct-field1 = 1.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := assign(selector(ident("lv_struct"), .Minus, "field1"), lit("1"))
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
assignment_with_static_field_access_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `lv_class=>field1 = 1.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := assign(selector(ident("lv_class"), .FatArrow, "field1"), lit("1"))
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
assignment_with_chained_field_access_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `lv_struct-nested-field = 1.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		// lv_struct-nested-field is parsed as ((lv_struct-nested)-field)
		inner_selector := selector(ident("lv_struct"), .Minus, "nested")
		outer_selector := selector(inner_selector, .Minus, "field")
		expected := assign(outer_selector, lit("1"))
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
assignment_with_mixed_access_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `lv_class=>struct-field = 1.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		// lv_class=>struct-field is parsed as ((lv_class=>struct)-field)
		static_selector := selector(ident("lv_class"), .FatArrow, "struct")
		field_selector := selector(static_selector, .Minus, "field")
		expected := assign(field_selector, lit("1"))
		check_stmt(t, expected, file.decls[0])
	}
}

// --- FORM tests ---

@(test)
basic_empty_form_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `FORM my_subroutine.
ENDFORM.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := form_build(form_decl_builder("my_subroutine"))
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
form_with_using_params_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `FORM my_sub USING p_value1 p_value2.
ENDFORM.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := form_build(
			form_with_using(
				form_decl_builder("my_sub"),
				form_param("p_value1"),
				form_param("p_value2"),
			),
		)
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
form_with_changing_params_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `FORM my_sub CHANGING c_result.
ENDFORM.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := form_build(
			form_with_changing(form_decl_builder("my_sub"), form_param("c_result")),
		)
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
form_with_tables_params_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `FORM my_sub TABLES it_data.
ENDFORM.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := form_build(
			form_with_tables(form_decl_builder("my_sub"), form_param("it_data")),
		)
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
form_with_typed_params_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `FORM my_sub USING p_value TYPE i.
ENDFORM.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := form_build(
			form_with_using(form_decl_builder("my_sub"), form_param("p_value", "i")),
		)
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
form_with_all_param_sections_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `FORM process_data TABLES it_input
                   USING p_mode TYPE string
                   CHANGING c_count TYPE i.
ENDFORM.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := form_build(
			form_with_changing(
				form_with_using(
					form_with_tables(form_decl_builder("process_data"), form_param("it_input")),
					form_param("p_mode", "string"),
				),
				form_param("c_count", "i"),
			),
		)
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
form_with_body_statements_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `FORM my_sub USING p_val.
  lv_result = p_val.
ENDFORM.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := form_build(
			form_with_body(
				form_with_using(form_decl_builder("my_sub"), form_param("p_val")),
				assign(ident("lv_result"), ident("p_val")),
			),
		)
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
form_with_multiple_body_statements_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `FORM calculate USING p_a p_b CHANGING c_result.
  DATA lv_temp TYPE i.
  lv_temp = p_a.
  c_result = lv_temp.
ENDFORM.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := form_build(
			form_with_body(
				form_with_changing(
					form_with_using(
						form_decl_builder("calculate"),
						form_param("p_a"),
						form_param("p_b"),
					),
					form_param("c_result"),
				),
				data_single_typed("lv_temp", "i"),
				assign(ident("lv_temp"), ident("p_a")),
				assign(ident("c_result"), ident("lv_temp")),
			),
		)
		check_stmt(t, expected, file.decls[0])
	}
}

// --- TYPES tests ---

@(test)
basic_single_types_decl_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `TYPES ty_counter TYPE i.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := types_single("ty_counter", "i")
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
basic_chain_types_decl_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `TYPES: ty_int TYPE i,
       ty_str TYPE string.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := types_chain({"ty_int", "i"}, {"ty_str", "string"})
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
chain_types_decl_three_types_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `TYPES: ty_id TYPE i, ty_name TYPE string, ty_amount TYPE f.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := types_chain({"ty_id", "i"}, {"ty_name", "string"}, {"ty_amount", "f"})
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
types_with_custom_type_reference_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `TYPES ty_material TYPE matnr.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := types_single("ty_material", "matnr")
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
mixed_data_and_types_decl_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `TYPES ty_counter TYPE i.
DATA lv_counter TYPE ty_counter.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 2,
		fmt.tprintf("Expected 2 decls, got %v", len(file.decls)),
	)
	if len(file.decls) >= 2 {
		expected_types := types_single("ty_counter", "i")
		check_stmt(t, expected_types, file.decls[0])
		expected_data := data_single_typed("lv_counter", "ty_counter")
		check_stmt(t, expected_data, file.decls[1])
	}
}

@(test)
form_errornous_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = "*DATA lv_val TYPE i.\r\nDATA(lv_data) = 'hello'.\r\n\r\nDATA lv_var1 TYPE i.\r\n\r\nDATA: \r\n    lv_var2 TYPE i,\r\n    lv_var3 TYPE f.\r\n\r\nlv_var2 = 10.\r\n\r\nlv_var3 = lv_var2.\r\n\r\nFORM process_data TABLES it_input\r\n                   USING p_mode TYPE string\r\n                   CHANGING c_count TYPE i.\r\nENDFORM.\r\n\r\nFORM some_form\r\n    C.\r\n    DATA: \r\n        lv_var TYPE i,\r\n        lv_var2 TYPE string.\r\n    \r\nENDFORM."

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) > 0,
		fmt.tprintf("Expected syntax errors: %v", file.syntax_errors),
	)
}

// --- Structured TYPES tests ---

@(test)
basic_struct_type_decl_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `TYPES: BEGIN OF street_type,
         name TYPE c,
         no   TYPE c,
       END OF street_type.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := types_struct_build(
			types_struct_with_field(
				types_struct_with_field(
					types_struct_builder("street_type"),
					"name",
					"c",
				),
				"no",
				"c",
			),
		)
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
struct_type_with_nested_struct_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `TYPES: BEGIN OF address_type,
         name   TYPE c,
         BEGIN OF city,
           zipcode TYPE n,
           name TYPE c,
         END OF city,
       END OF address_type.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		// Build nested city structure
		nested_city := types_struct_build(
			types_struct_with_field(
				types_struct_with_field(
					types_struct_builder("city"),
					"zipcode",
					"n",
				),
				"name",
				"c",
			),
		)
		
		// Build outer address_type structure with nested city
		expected := types_struct_build(
			types_struct_with_nested(
				types_struct_with_field(
					types_struct_builder("address_type"),
					"name",
					"c",
				),
				nested_city,
			),
		)
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
struct_type_with_reference_to_another_struct_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `TYPES: BEGIN OF address_type,
         name   TYPE c,
         street TYPE street_type,
       END OF address_type.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		expected := types_struct_build(
			types_struct_with_field(
				types_struct_with_field(
					types_struct_builder("address_type"),
					"name",
					"c",
				),
				"street",
				"street_type",
			),
		)
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
type_reference_with_path_selector_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `TYPES zipcode_type TYPE address_type-city-zipcode.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		// The type is a selector expression: address_type-city-zipcode
		ac, ok := file.decls[0].derived_stmt.(^ast.Types_Decl)
		if !testing.expect(t, ok, "Expected Types_Decl") do return
		
		testing.expect(t, ac.ident != nil && ac.ident.name == "zipcode_type", "Expected ident 'zipcode_type'")
		
		// Check that the typed expression is a selector
		selector, sok := ac.typed.derived_expr.(^ast.Selector_Expr)
		testing.expect(t, sok, "Expected Selector_Expr for type")
		if sok {
			testing.expect(t, selector.field.name == "zipcode", fmt.tprintf("Expected field 'zipcode', got '%s'", selector.field.name))
		}
	}
}

@(test)
complex_nested_struct_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `TYPES: BEGIN OF address_type,
         name   TYPE c,
         street TYPE street_type,
         BEGIN OF city,
           zipcode TYPE n,
           name TYPE c,
         END OF city,
       END OF address_type.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 decl, got %v", len(file.decls)),
	)
	if len(file.decls) > 0 {
		ac, ok := file.decls[0].derived_stmt.(^ast.Types_Struct_Decl)
		if !testing.expect(t, ok, "Expected Types_Struct_Decl") do return
		
		testing.expect(t, ac.ident.name == "address_type", fmt.tprintf("Expected 'address_type', got '%s'", ac.ident.name))
		testing.expect(t, len(ac.components) == 3, fmt.tprintf("Expected 3 components, got %d", len(ac.components)))
	}
}
