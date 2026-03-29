package tests_parser

import "../../src/lang/ast"
import "../../src/lang/parser"
import "core:fmt"
import "core:testing"

@(test)
try_catch_into_final_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `TRY.
  out->write( 1 / 0 ).
CATCH cx_sy_arithmetic_error INTO FINAL(exc).
  out->write( exc->get_text( ) ).
ENDTRY.`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)
	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 stmt, got %d", len(file.decls))) do return

	try_stmt, ok := file.decls[0].derived_stmt.(^ast.Try_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Try_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(t, len(try_stmt.body) == 1, fmt.tprintf("Expected 1 TRY body stmt, got %d", len(try_stmt.body)))
	if !testing.expect(t, len(try_stmt.catch_branches) == 1, fmt.tprintf("Expected 1 CATCH branch, got %d", len(try_stmt.catch_branches))) do return

	catch_branch := try_stmt.catch_branches[0]
	testing.expect(t, !catch_branch.before_unwind, "Did not expect BEFORE UNWIND")
	if !testing.expect(t, len(catch_branch.class_refs) == 1, fmt.tprintf("Expected 1 class ref, got %d", len(catch_branch.class_refs))) do return

	class_ident, class_ok := catch_branch.class_refs[0].derived_expr.(^ast.Ident)
	if testing.expect(t, class_ok, fmt.tprintf("Expected catch class ident, got %T", catch_branch.class_refs[0].derived_expr)) {
		testing.expect(
			t,
			class_ident.name == "cx_sy_arithmetic_error",
			fmt.tprintf("Expected cx_sy_arithmetic_error, got %s", class_ident.name),
		)
	}

	into_ident, into_ok := catch_branch.into_target.derived_expr.(^ast.Ident)
	if testing.expect(t, into_ok, fmt.tprintf("Expected catch INTO ident, got %T", catch_branch.into_target.derived_expr)) {
		testing.expect(t, into_ident.name == "exc", fmt.tprintf("Expected exc, got %s", into_ident.name))
	}

	testing.expect(t, len(catch_branch.body) == 1, fmt.tprintf("Expected 1 CATCH body stmt, got %d", len(catch_branch.body)))
	testing.expect(t, try_stmt.cleanup_branch == nil, "Did not expect CLEANUP branch")
}

@(test)
try_catch_namespaced_class_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `TRY.
  lv_result = 1.
CATCH /sttp/cx_rr_ru_rest_client INTO lo_cx_rest.
  lv_result = 2.
ENDTRY.`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)
	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 stmt, got %d", len(file.decls))) do return

	try_stmt, ok := file.decls[0].derived_stmt.(^ast.Try_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Try_Stmt, got %T", file.decls[0].derived_stmt)) do return
	if !testing.expect(t, len(try_stmt.catch_branches) == 1, fmt.tprintf("Expected 1 CATCH branch, got %d", len(try_stmt.catch_branches))) do return

	catch_branch := try_stmt.catch_branches[0]
	class_ident, class_ok := catch_branch.class_refs[0].derived_expr.(^ast.Ident)
	if testing.expect(t, class_ok, fmt.tprintf("Expected catch class ident, got %T", catch_branch.class_refs[0].derived_expr)) {
		testing.expect(
			t,
			class_ident.name == "/sttp/cx_rr_ru_rest_client",
			fmt.tprintf("Expected /sttp/cx_rr_ru_rest_client, got %s", class_ident.name),
		)
	}

	into_ident, into_ok := catch_branch.into_target.derived_expr.(^ast.Ident)
	if testing.expect(t, into_ok, fmt.tprintf("Expected catch INTO ident, got %T", catch_branch.into_target.derived_expr)) {
		testing.expect(t, into_ident.name == "lo_cx_rest", fmt.tprintf("Expected lo_cx_rest, got %s", into_ident.name))
	}
}

@(test)
try_before_unwind_cleanup_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `TRY.
  IF lv_ts IS NOT INITIAL.
    lv_string = lv_ts.
  ENDIF.
CATCH BEFORE UNWIND cx_sy_arithmetic_error cx_root INTO FINAL(lx_error).
  lv_string = 'error'.
CLEANUP INTO FINAL(lx_cleanup).
  lv_string = 'cleanup'.
ENDTRY.`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)
	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 stmt, got %d", len(file.decls))) do return

	try_stmt, ok := file.decls[0].derived_stmt.(^ast.Try_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Try_Stmt, got %T", file.decls[0].derived_stmt)) do return
	if !testing.expect(t, len(try_stmt.body) == 1, fmt.tprintf("Expected 1 TRY body stmt, got %d", len(try_stmt.body))) do return
	_, if_ok := try_stmt.body[0].derived_stmt.(^ast.If_Stmt)
	testing.expect(t, if_ok, fmt.tprintf("Expected IF stmt in TRY body, got %T", try_stmt.body[0].derived_stmt))

	if !testing.expect(t, len(try_stmt.catch_branches) == 1, fmt.tprintf("Expected 1 CATCH branch, got %d", len(try_stmt.catch_branches))) do return
	catch_branch := try_stmt.catch_branches[0]
	testing.expect(t, catch_branch.before_unwind, "Expected BEFORE UNWIND")
	if !testing.expect(t, len(catch_branch.class_refs) == 2, fmt.tprintf("Expected 2 class refs, got %d", len(catch_branch.class_refs))) do return

	first_class, first_ok := catch_branch.class_refs[0].derived_expr.(^ast.Ident)
	if testing.expect(t, first_ok, fmt.tprintf("Expected first catch class ident, got %T", catch_branch.class_refs[0].derived_expr)) {
		testing.expect(t, first_class.name == "cx_sy_arithmetic_error", fmt.tprintf("Expected cx_sy_arithmetic_error, got %s", first_class.name))
	}

	second_class, second_ok := catch_branch.class_refs[1].derived_expr.(^ast.Ident)
	if testing.expect(t, second_ok, fmt.tprintf("Expected second catch class ident, got %T", catch_branch.class_refs[1].derived_expr)) {
		testing.expect(t, second_class.name == "cx_root", fmt.tprintf("Expected cx_root, got %s", second_class.name))
	}

	catch_into_ident, catch_into_ok := catch_branch.into_target.derived_expr.(^ast.Ident)
	if testing.expect(t, catch_into_ok, fmt.tprintf("Expected catch INTO ident, got %T", catch_branch.into_target.derived_expr)) {
		testing.expect(t, catch_into_ident.name == "lx_error", fmt.tprintf("Expected lx_error, got %s", catch_into_ident.name))
	}

	if !testing.expect(t, try_stmt.cleanup_branch != nil, "Expected CLEANUP branch") do return
	cleanup_into_ident, cleanup_into_ok := try_stmt.cleanup_branch.into_target.derived_expr.(^ast.Ident)
	if testing.expect(t, cleanup_into_ok, fmt.tprintf("Expected cleanup INTO ident, got %T", try_stmt.cleanup_branch.into_target.derived_expr)) {
		testing.expect(t, cleanup_into_ident.name == "lx_cleanup", fmt.tprintf("Expected lx_cleanup, got %s", cleanup_into_ident.name))
	}
	testing.expect(
		t,
		len(try_stmt.cleanup_branch.body) == 1,
		fmt.tprintf("Expected 1 CLEANUP body stmt, got %d", len(try_stmt.cleanup_branch.body)),
	)
}
