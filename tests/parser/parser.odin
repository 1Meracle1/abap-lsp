package tests_parser

import "../../src/lang/ast"
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

data_chain_typed :: proc(decls: ..struct{name: string, type_name: string}) -> ^ast.Data_Typed_Chain_Decl {
	node := ast.new(ast.Data_Typed_Chain_Decl, {})
	node.decls = make([dynamic]^ast.Data_Typed_Decl)
	for d in decls {
		append(&node.decls, data_single_typed(d.name, d.type_name))
	}
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

			if testing.expect(t, ac_decl.ident != nil, fmt.tprintf("Actual ident[%d] is nil", i), loc = loc) {
				testing.expect(
					t,
					ex_decl.ident.name == ac_decl.ident.name,
					fmt.tprintf("Expected chain decl[%d] ident '%s', got '%s'", i, ex_decl.ident.name, ac_decl.ident.name),
					loc = loc,
				)
			}

			check_expr(t, ex_decl.typed.derived_expr, ac_decl.typed, loc = loc)
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
