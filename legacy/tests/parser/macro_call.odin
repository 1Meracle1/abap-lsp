package tests_parser

import "../../src/lang/ast"
import "../../src/lang/parser"
import "core:fmt"
import "core:testing"

@(test)
macro_call_with_args_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = "convert_from_uri lv_serial some_other_arg."
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)
	testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 stmt, got %d", len(file.decls)))
	if len(file.decls) == 1 {
		ex := macro_call_stmt("convert_from_uri", ident("lv_serial"), ident("some_other_arg"))
		check_stmt(t, ex, file.decls[0])
	}
}

@(test)
assign_multiline_rhs_double_ampersand_concat_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src =
		`cs_encode_decode-obj_code-code_char
                                =    gc_s_uri_header-epcglobal_cbv_bt
                                  && lv_gs1_gln
                                  && '.'
                                  && cs_encode_decode-obj_ids-serial.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)
	testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 stmt, got %d", len(file.decls)))
	if len(file.decls) == 1 {
		assign, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
		if !testing.expect(t, ok, "expected Assign_Stmt") do return
		testing.expect(t, len(assign.rhs) == 1, "expected single rhs expr")
	}
}

@(test)
macro_call_stops_when_arg_does_not_advance_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = "quux &."
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) > 0,
		fmt.tprintf("expected syntax errors, got %v", file.syntax_errors),
	)
}

@(test)
macro_call_without_args_is_expr_stmt_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = "some_macro_without_args."
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)
	testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 stmt, got %d", len(file.decls)))
	if len(file.decls) == 1 {
		ex := ast.new(ast.Expr_Stmt, {})
		ex.expr = ident("some_macro_without_args")
		ex.derived_stmt = ex
		check_stmt(t, ex, file.decls[0])
	}
}
