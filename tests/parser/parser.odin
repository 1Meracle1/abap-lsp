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

	// Wrap value in generic Expr if needed
	// We need to point to the embedded 'node' (Expr) field within the leaf struct.
	// Since Basic_Lit has `using node: Expr`, we access it as `v.node`.

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

		// Check Ident
		if testing.expect(t, ac.ident != nil, "Actual ident is nil", loc = loc) {
			testing.expect(t, ex.ident != nil, "Expected ident is nil") // Should be enforced by builder
			testing.expect(
				t,
				ex.ident.name == ac.ident.name,
				fmt.tprintf("Expected Decl ident '%s', got '%s'", ex.ident.name, ac.ident.name),
				loc = loc,
			)
		}

		// Check Value
		check_expr(t, ex.value.derived_expr, ac.value, loc = loc)

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
