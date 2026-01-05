package tests_parser

import "../../src/lang/ast"
import lexer "../../src/lang/lexer"
import "../../src/lang/parser"
import "core:fmt"
import "core:testing"

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
basic_inline_data_decl_with_preceding_pragma_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `##ENH_OK
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
pragma_statements_ignored_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `##ENH_OK
DATA lv_a TYPE i.
##SOME_PRAGMA
DATA lv_b TYPE string.`


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

	// Pragma lines should be stored as comments
	testing.expect(
		t,
		len(file.comments) == 2,
		fmt.tprintf("Expected 2 comments (pragmas), got %v", len(file.comments)),
	)
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
simple_downcast_assignment_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `lo_other ?= io_event.`
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
		expected := downcast_assign(ident("lo_other"), ident("io_event"))
		check_stmt(t, expected, file.decls[0])
	}
}

@(test)
downcast_assignment_with_method_call_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `lo_object ?= cl_factory=>get_instance( ).`
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
		assign, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
		if !testing.expect(t, ok, fmt.tprintf("Expected Assign_Stmt, got %T", file.decls[0].derived_stmt)) do return

		testing.expect(
			t,
			assign.op.kind == .QuestionEq,
			fmt.tprintf("Expected QuestionEq operator, got %v", assign.op.kind),
		)
	}
}

@(test)
downcast_assignment_with_field_access_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `lo_data ?= io_container->mo_data.`
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
		assign, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
		if !testing.expect(t, ok, fmt.tprintf("Expected Assign_Stmt, got %T", file.decls[0].derived_stmt)) do return

		testing.expect(
			t,
			assign.op.kind == .QuestionEq,
			fmt.tprintf("Expected QuestionEq operator, got %v", assign.op.kind),
		)
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
	file.src =
	`FORM process_data TABLES it_input
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
	file.src =
	`FORM calculate USING p_a p_b CHANGING c_result.
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
	file.src =
	"*DATA lv_val TYPE i.\r\nDATA(lv_data) = 'hello'.\r\n\r\nDATA lv_var1 TYPE i.\r\n\r\nDATA: \r\n    lv_var2 TYPE i,\r\n    lv_var3 TYPE f.\r\n\r\nlv_var2 = 10.\r\n\r\nlv_var3 = lv_var2.\r\n\r\nFORM process_data TABLES it_input\r\n                   USING p_mode TYPE string\r\n                   CHANGING c_count TYPE i.\r\nENDFORM.\r\n\r\nFORM some_form\r\n    C.\r\n    DATA: \r\n        lv_var TYPE i,\r\n        lv_var2 TYPE string.\r\n    \r\nENDFORM."

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
	file.src =
	`TYPES: BEGIN OF street_type,
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
				types_struct_with_field(types_struct_builder("street_type"), "name", "c"),
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
	file.src =
	`TYPES: BEGIN OF address_type,
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
				types_struct_with_field(types_struct_builder("city"), "zipcode", "n"),
				"name",
				"c",
			),
		)

		// Build outer address_type structure with nested city
		expected := types_struct_build(
			types_struct_with_nested(
				types_struct_with_field(types_struct_builder("address_type"), "name", "c"),
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
	file.src =
	`TYPES: BEGIN OF address_type,
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
				types_struct_with_field(types_struct_builder("address_type"), "name", "c"),
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

		testing.expect(
			t,
			ac.ident != nil && ac.ident.name == "zipcode_type",
			"Expected ident 'zipcode_type'",
		)

		// Check that the typed expression is a selector
		selector, sok := ac.typed.derived_expr.(^ast.Selector_Expr)
		testing.expect(t, sok, "Expected Selector_Expr for type")
		if sok {
			testing.expect(
				t,
				selector.field.name == "zipcode",
				fmt.tprintf("Expected field 'zipcode', got '%s'", selector.field.name),
			)
		}
	}
}

@(test)
complex_nested_struct_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`TYPES: BEGIN OF address_type,
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

		testing.expect(
			t,
			ac.ident.name == "address_type",
			fmt.tprintf("Expected 'address_type', got '%s'", ac.ident.name),
		)
		testing.expect(
			t,
			len(ac.components) == 3,
			fmt.tprintf("Expected 3 components, got %d", len(ac.components)),
		)
	}
}

// --- REPORT, INCLUDE, EVENT, CALL SCREEN tests ---

@(test)
basic_report_decl_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `REPORT zttrp001_us_sn_reset.`
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
		report, ok := file.decls[0].derived_stmt.(^ast.Report_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Report_Decl, got %T", file.decls[0].derived_stmt)) do return

		testing.expect(t, report.name != nil, "Report name should not be nil")
		if report.name != nil {
			testing.expect(
				t,
				report.name.name == "zttrp001_us_sn_reset",
				fmt.tprintf("Expected 'zttrp001_us_sn_reset', got '%s'", report.name.name),
			)
		}
	}
}

@(test)
basic_include_decl_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `INCLUDE zttrp001_us_sn_reset_cl1.`
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
		include, ok := file.decls[0].derived_stmt.(^ast.Include_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Include_Decl, got %T", file.decls[0].derived_stmt)) do return

		testing.expect(t, include.name != nil, "Include name should not be nil")
		if include.name != nil {
			testing.expect(
				t,
				include.name.name == "zttrp001_us_sn_reset_cl1",
				fmt.tprintf("Expected 'zttrp001_us_sn_reset_cl1', got '%s'", include.name.name),
			)
		}
	}
}

@(test)
multiple_include_decls_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`INCLUDE zttrp001_us_sn_reset_top.
INCLUDE zttrp001_us_sn_reset_sel.
INCLUDE zttrp001_us_sn_reset_f01.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 3,
		fmt.tprintf("Expected 3 decls, got %v", len(file.decls)),
	)

	expected_names := []string {
		"zttrp001_us_sn_reset_top",
		"zttrp001_us_sn_reset_sel",
		"zttrp001_us_sn_reset_f01",
	}
	for i := 0; i < min(len(file.decls), 3); i += 1 {
		include, ok := file.decls[i].derived_stmt.(^ast.Include_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Include_Decl at %d", i)) do continue
		if include.name != nil {
			testing.expect(
				t,
				include.name.name == expected_names[i],
				fmt.tprintf("Expected '%s', got '%s'", expected_names[i], include.name.name),
			)
		}
	}
}

@(test)
start_of_selection_event_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `START-OF-SELECTION.
  DATA lv_var TYPE i.
  lv_var = 1.`


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
		event, ok := file.decls[0].derived_stmt.(^ast.Event_Block)
		if !testing.expect(t, ok, fmt.tprintf("Expected Event_Block, got %T", file.decls[0].derived_stmt)) do return

		testing.expect(
			t,
			event.kind == .StartOfSelection,
			fmt.tprintf("Expected StartOfSelection, got %v", event.kind),
		)
		testing.expect(
			t,
			len(event.body) == 2,
			fmt.tprintf("Expected 2 body statements, got %d", len(event.body)),
		)
	}
}

@(test)
initialization_event_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `INITIALIZATION.
  DATA lv_init TYPE string.`


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
		event, ok := file.decls[0].derived_stmt.(^ast.Event_Block)
		if !testing.expect(t, ok, fmt.tprintf("Expected Event_Block, got %T", file.decls[0].derived_stmt)) do return

		testing.expect(
			t,
			event.kind == .Initialization,
			fmt.tprintf("Expected Initialization, got %v", event.kind),
		)
		testing.expect(
			t,
			len(event.body) == 1,
			fmt.tprintf("Expected 1 body statement, got %d", len(event.body)),
		)
	}
}

@(test)
call_screen_stmt_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `CALL SCREEN 100.`
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
		call_screen, ok := file.decls[0].derived_stmt.(^ast.Call_Screen_Stmt)
		if !testing.expect(t, ok, fmt.tprintf("Expected Call_Screen_Stmt, got %T", file.decls[0].derived_stmt)) do return

		testing.expect(t, call_screen.screen_no != nil, "Screen number should not be nil")
		if call_screen.screen_no != nil {
			if lit, lok := call_screen.screen_no.derived_expr.(^ast.Basic_Lit); lok {
				testing.expect(
					t,
					lit.tok.lit == "100",
					fmt.tprintf("Expected '100', got '%s'", lit.tok.lit),
				)
			}
		}
	}
}

@(test)
full_report_program_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`REPORT zttrp001_us_sn_reset.

INCLUDE zttrp001_us_sn_reset_cl1.
INCLUDE zttrp001_us_sn_reset_top.
INCLUDE zttrp001_us_sn_reset_sel.
INCLUDE zttrp001_us_sn_reset_f01.
INCLUDE zttrp001_us_sn_reset_tst1.

START-OF-SELECTION.
  DATA(lv_var) = 1.

  CALL SCREEN 100.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	// Expected: 1 REPORT + 5 INCLUDEs + 1 START-OF-SELECTION = 7 top-level decls
	testing.expect(
		t,
		len(file.decls) == 7,
		fmt.tprintf("Expected 7 decls, got %v", len(file.decls)),
	)

	if len(file.decls) >= 7 {
		// Check REPORT
		report, rok := file.decls[0].derived_stmt.(^ast.Report_Decl)
		testing.expect(t, rok, "First decl should be Report_Decl")
		if rok {
			testing.expect(
				t,
				report.name.name == "zttrp001_us_sn_reset",
				fmt.tprintf(
					"Expected report name 'zttrp001_us_sn_reset', got '%s'",
					report.name.name,
				),
			)
		}

		// Check INCLUDEs (indices 1-5)
		for i := 1; i <= 5; i += 1 {
			_, iok := file.decls[i].derived_stmt.(^ast.Include_Decl)
			testing.expect(t, iok, fmt.tprintf("Decl %d should be Include_Decl", i))
		}

		// Check START-OF-SELECTION event
		event, eok := file.decls[6].derived_stmt.(^ast.Event_Block)
		testing.expect(t, eok, "Last decl should be Event_Block")
		if eok {
			testing.expect(t, event.kind == .StartOfSelection, "Event should be StartOfSelection")
			// Event body should have DATA inline + CALL SCREEN = 2 statements
			testing.expect(
				t,
				len(event.body) == 2,
				fmt.tprintf("Expected 2 body statements, got %d", len(event.body)),
			)
		}
	}
}

// --- NEW instance operator tests ---

@(test)
new_expr_with_type_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `dref = NEW i( 555 ).`
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
		assign_stmt, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
		if !testing.expect(t, ok, fmt.tprintf("Expected Assign_Stmt, got %T", file.decls[0].derived_stmt)) do return

		testing.expect(t, len(assign_stmt.rhs) == 1, "Expected 1 rhs expression")
		if len(assign_stmt.rhs) > 0 {
			new_e, nok := assign_stmt.rhs[0].derived_expr.(^ast.New_Expr)
			if !testing.expect(t, nok, fmt.tprintf("Expected New_Expr, got %T", assign_stmt.rhs[0].derived_expr)) do return

			testing.expect(t, !new_e.is_inferred, "Expected is_inferred to be false")
			testing.expect(t, new_e.type_expr != nil, "Expected type_expr to be non-nil")
			if new_e.type_expr != nil {
				type_ident, iok := new_e.type_expr.derived_expr.(^ast.Ident)
				if testing.expect(t, iok, "Expected type_expr to be Ident") {
					testing.expect(
						t,
						type_ident.name == "i",
						fmt.tprintf("Expected type 'i', got '%s'", type_ident.name),
					)
				}
			}
			testing.expect(
				t,
				len(new_e.args) == 1,
				fmt.tprintf("Expected 1 arg, got %d", len(new_e.args)),
			)
			if len(new_e.args) > 0 {
				arg_lit, lok := new_e.args[0].derived_expr.(^ast.Basic_Lit)
				if testing.expect(t, lok, "Expected arg to be Basic_Lit") {
					testing.expect(
						t,
						arg_lit.tok.lit == "555",
						fmt.tprintf("Expected '555', got '%s'", arg_lit.tok.lit),
					)
				}
			}
		}
	}
}

@(test)
new_expr_inferred_type_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `oref = NEW #( ).`
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
		assign_stmt, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
		if !testing.expect(t, ok, fmt.tprintf("Expected Assign_Stmt, got %T", file.decls[0].derived_stmt)) do return

		testing.expect(t, len(assign_stmt.rhs) == 1, "Expected 1 rhs expression")
		if len(assign_stmt.rhs) > 0 {
			new_e, nok := assign_stmt.rhs[0].derived_expr.(^ast.New_Expr)
			if !testing.expect(t, nok, fmt.tprintf("Expected New_Expr, got %T", assign_stmt.rhs[0].derived_expr)) do return

			testing.expect(t, new_e.is_inferred, "Expected is_inferred to be true")
			testing.expect(
				t,
				new_e.type_expr == nil,
				"Expected type_expr to be nil for inferred type",
			)
			testing.expect(
				t,
				len(new_e.args) == 0,
				fmt.tprintf("Expected 0 args, got %d", len(new_e.args)),
			)
		}
	}
}

@(test)
new_expr_class_type_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `oref = NEW cls( ).`
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
		assign_stmt, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
		if !testing.expect(t, ok, fmt.tprintf("Expected Assign_Stmt, got %T", file.decls[0].derived_stmt)) do return

		testing.expect(t, len(assign_stmt.rhs) == 1, "Expected 1 rhs expression")
		if len(assign_stmt.rhs) > 0 {
			new_e, nok := assign_stmt.rhs[0].derived_expr.(^ast.New_Expr)
			if !testing.expect(t, nok, fmt.tprintf("Expected New_Expr, got %T", assign_stmt.rhs[0].derived_expr)) do return

			testing.expect(t, !new_e.is_inferred, "Expected is_inferred to be false")
			testing.expect(t, new_e.type_expr != nil, "Expected type_expr to be non-nil")
			if new_e.type_expr != nil {
				type_ident, iok := new_e.type_expr.derived_expr.(^ast.Ident)
				if testing.expect(t, iok, "Expected type_expr to be Ident") {
					testing.expect(
						t,
						type_ident.name == "cls",
						fmt.tprintf("Expected type 'cls', got '%s'", type_ident.name),
					)
				}
			}
			testing.expect(
				t,
				len(new_e.args) == 0,
				fmt.tprintf("Expected 0 args, got %d", len(new_e.args)),
			)
		}
	}
}

@(test)
new_expr_in_inline_data_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(lo_obj) = NEW zcl_myclass( ).`
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
		data_inline, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Data_Inline_Decl, got %T", file.decls[0].derived_stmt)) do return

		testing.expect(
			t,
			data_inline.ident.name == "lo_obj",
			fmt.tprintf("Expected 'lo_obj', got '%s'", data_inline.ident.name),
		)

		if data_inline.value != nil {
			new_e, nok := data_inline.value.derived_expr.(^ast.New_Expr)
			if !testing.expect(t, nok, fmt.tprintf("Expected New_Expr, got %T", data_inline.value.derived_expr)) do return

			testing.expect(t, !new_e.is_inferred, "Expected is_inferred to be false")
			testing.expect(t, new_e.type_expr != nil, "Expected type_expr to be non-nil")
			if new_e.type_expr != nil {
				type_ident, iok := new_e.type_expr.derived_expr.(^ast.Ident)
				if testing.expect(t, iok, "Expected type_expr to be Ident") {
					testing.expect(
						t,
						type_ident.name == "zcl_myclass",
						fmt.tprintf("Expected type 'zcl_myclass', got '%s'", type_ident.name),
					)
				}
			}
		}
	}
}

@(test)
new_expr_full_class_example_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`CLASS cls DEFINITION. 
ENDCLASS.

CLASS exa DEFINITION. 
  PUBLIC SECTION. 
    CLASS-METHODS main. 
ENDCLASS.

CLASS exa IMPLEMENTATION. 
  METHOD main. 
    DATA: dref TYPE i, 
          oref TYPE i.

    dref = NEW i( 555 ). 
    oref = NEW #( ). 
  ENDMETHOD. 
ENDCLASS.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	// Expected: CLASS cls DEFINITION + CLASS exa DEFINITION + CLASS exa IMPLEMENTATION = 3 decls
	testing.expect(
		t,
		len(file.decls) == 3,
		fmt.tprintf("Expected 3 decls, got %v", len(file.decls)),
	)

	if len(file.decls) >= 3 {
		// First: CLASS cls DEFINITION
		cls_def, ok1 := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
		testing.expect(t, ok1, "First decl should be Class_Def_Decl")
		if ok1 {
			testing.expect(
				t,
				cls_def.ident.name == "cls",
				fmt.tprintf("Expected 'cls', got '%s'", cls_def.ident.name),
			)
		}

		// Second: CLASS exa DEFINITION
		exa_def, ok2 := file.decls[1].derived_stmt.(^ast.Class_Def_Decl)
		testing.expect(t, ok2, "Second decl should be Class_Def_Decl")
		if ok2 {
			testing.expect(
				t,
				exa_def.ident.name == "exa",
				fmt.tprintf("Expected 'exa', got '%s'", exa_def.ident.name),
			)
		}

		// Third: CLASS exa IMPLEMENTATION
		exa_impl, ok3 := file.decls[2].derived_stmt.(^ast.Class_Impl_Decl)
		testing.expect(t, ok3, "Third decl should be Class_Impl_Decl")
		if ok3 {
			testing.expect(
				t,
				exa_impl.ident.name == "exa",
				fmt.tprintf("Expected 'exa', got '%s'", exa_impl.ident.name),
			)

			// Check that the method implementation exists
			testing.expect(
				t,
				len(exa_impl.methods) == 1,
				fmt.tprintf("Expected 1 method, got %d", len(exa_impl.methods)),
			)
			if len(exa_impl.methods) > 0 {
				method_impl, mok := exa_impl.methods[0].derived_stmt.(^ast.Method_Impl)
				if testing.expect(t, mok, "Expected Method_Impl") {
					// Method body should have DATA chain + 2 assignments = 3 statements
					testing.expect(
						t,
						len(method_impl.body) == 3,
						fmt.tprintf("Expected 3 body statements, got %d", len(method_impl.body)),
					)

					// Check the second assignment has a NEW i( 555 )
					if len(method_impl.body) >= 2 {
						assign1, aok := method_impl.body[1].derived_stmt.(^ast.Assign_Stmt)
						if testing.expect(t, aok, "Expected second body stmt to be Assign_Stmt") {
							if len(assign1.rhs) > 0 {
								new_e, nok := assign1.rhs[0].derived_expr.(^ast.New_Expr)
								if testing.expect(
									t,
									nok,
									"Expected New_Expr in second assignment",
								) {
									testing.expect(
										t,
										!new_e.is_inferred,
										"Expected explicit type for first NEW",
									)
								}
							}
						}
					}

					// Check the third assignment has a NEW #( )
					if len(method_impl.body) >= 3 {
						assign2, a2ok := method_impl.body[2].derived_stmt.(^ast.Assign_Stmt)
						if testing.expect(t, a2ok, "Expected third body stmt to be Assign_Stmt") {
							if len(assign2.rhs) > 0 {
								new_e, n2ok := assign2.rhs[0].derived_expr.(^ast.New_Expr)
								if testing.expect(
									t,
									n2ok,
									"Expected New_Expr in third assignment",
								) {
									testing.expect(
										t,
										new_e.is_inferred,
										"Expected inferred type for second NEW",
									)
								}
							}
						}
					}
				}
			}
		}
	}
}

// --- MODULE tests ---

@(test)
basic_module_output_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `MODULE status_0100 OUTPUT.
ENDMODULE.`


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
		module, ok := file.decls[0].derived_stmt.(^ast.Module_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Module_Decl, got %T", file.decls[0].derived_stmt)) do return

		testing.expect(t, module.ident != nil, "Module ident should not be nil")
		if module.ident != nil {
			testing.expect(
				t,
				module.ident.name == "status_0100",
				fmt.tprintf("Expected 'status_0100', got '%s'", module.ident.name),
			)
		}
		testing.expect(
			t,
			module.module_type == .Output,
			fmt.tprintf("Expected Output, got %v", module.module_type),
		)
		testing.expect(
			t,
			len(module.body) == 0,
			fmt.tprintf("Expected 0 body statements, got %d", len(module.body)),
		)
	}
}

@(test)
basic_module_input_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `MODULE user_command_0100 INPUT.
ENDMODULE.`


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
		module, ok := file.decls[0].derived_stmt.(^ast.Module_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Module_Decl, got %T", file.decls[0].derived_stmt)) do return

		testing.expect(t, module.ident != nil, "Module ident should not be nil")
		if module.ident != nil {
			testing.expect(
				t,
				module.ident.name == "user_command_0100",
				fmt.tprintf("Expected 'user_command_0100', got '%s'", module.ident.name),
			)
		}
		testing.expect(
			t,
			module.module_type == .Input,
			fmt.tprintf("Expected Input, got %v", module.module_type),
		)
		testing.expect(
			t,
			len(module.body) == 0,
			fmt.tprintf("Expected 0 body statements, got %d", len(module.body)),
		)
	}
}

@(test)
basic_module_default_input_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `MODULE user_command_0100.
ENDMODULE.`


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
		module, ok := file.decls[0].derived_stmt.(^ast.Module_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Module_Decl, got %T", file.decls[0].derived_stmt)) do return

		testing.expect(t, module.ident != nil, "Module ident should not be nil")
		if module.ident != nil {
			testing.expect(
				t,
				module.ident.name == "user_command_0100",
				fmt.tprintf("Expected 'user_command_0100', got '%s'", module.ident.name),
			)
		}
		testing.expect(
			t,
			module.module_type == .Input,
			fmt.tprintf("Expected Input, got %v", module.module_type),
		)
		testing.expect(
			t,
			len(module.body) == 0,
			fmt.tprintf("Expected 0 body statements, got %d", len(module.body)),
		)
	}
}

@(test)
module_with_body_statements_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`MODULE status_0100 OUTPUT.
  DATA lv_text TYPE string.
  lv_text = 'Hello'.
ENDMODULE.`


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
		module, ok := file.decls[0].derived_stmt.(^ast.Module_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Module_Decl, got %T", file.decls[0].derived_stmt)) do return

		testing.expect(
			t,
			module.ident.name == "status_0100",
			fmt.tprintf("Expected 'status_0100', got '%s'", module.ident.name),
		)
		testing.expect(
			t,
			module.module_type == .Output,
			fmt.tprintf("Expected Output, got %v", module.module_type),
		)
		testing.expect(
			t,
			len(module.body) == 2,
			fmt.tprintf("Expected 2 body statements, got %d", len(module.body)),
		)
	}
}

@(test)
multiple_modules_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`MODULE status_0100 OUTPUT.
ENDMODULE.

MODULE user_command_0100 INPUT.
  DATA lv_ok_code TYPE sy.
ENDMODULE.`


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
		// First module: OUTPUT
		module1, ok1 := file.decls[0].derived_stmt.(^ast.Module_Decl)
		if testing.expect(t, ok1, "First decl should be Module_Decl") {
			testing.expect(
				t,
				module1.ident.name == "status_0100",
				fmt.tprintf("Expected 'status_0100', got '%s'", module1.ident.name),
			)
			testing.expect(t, module1.module_type == .Output, "First module should be OUTPUT")
		}

		// Second module: INPUT
		module2, ok2 := file.decls[1].derived_stmt.(^ast.Module_Decl)
		if testing.expect(t, ok2, "Second decl should be Module_Decl") {
			testing.expect(
				t,
				module2.ident.name == "user_command_0100",
				fmt.tprintf("Expected 'user_command_0100', got '%s'", module2.ident.name),
			)
			testing.expect(t, module2.module_type == .Input, "Second module should be INPUT")
			testing.expect(
				t,
				len(module2.body) == 1,
				fmt.tprintf("Expected 1 body statement, got %d", len(module2.body)),
			)
		}
	}
}

@(test)
module_with_screen_program_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`REPORT ztest_screen.

MODULE status_0100 OUTPUT.
  DATA lv_title TYPE string.
ENDMODULE.

MODULE user_command_0100 INPUT.
  DATA lv_ok_code TYPE sy.
ENDMODULE.

START-OF-SELECTION.
  CALL SCREEN 100.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	// Expected: REPORT + MODULE OUTPUT + MODULE INPUT + START-OF-SELECTION = 4 decls
	testing.expect(
		t,
		len(file.decls) == 4,
		fmt.tprintf("Expected 4 decls, got %v", len(file.decls)),
	)

	if len(file.decls) >= 4 {
		// First: REPORT
		_, rok := file.decls[0].derived_stmt.(^ast.Report_Decl)
		testing.expect(t, rok, "First decl should be Report_Decl")

		// Second: MODULE OUTPUT
		module1, ok1 := file.decls[1].derived_stmt.(^ast.Module_Decl)
		if testing.expect(t, ok1, "Second decl should be Module_Decl") {
			testing.expect(t, module1.module_type == .Output, "Second module should be OUTPUT")
		}

		// Third: MODULE INPUT
		module2, ok2 := file.decls[2].derived_stmt.(^ast.Module_Decl)
		if testing.expect(t, ok2, "Third decl should be Module_Decl") {
			testing.expect(t, module2.module_type == .Input, "Third module should be INPUT")
		}

		// Fourth: START-OF-SELECTION
		event, eok := file.decls[3].derived_stmt.(^ast.Event_Block)
		if testing.expect(t, eok, "Fourth decl should be Event_Block") {
			testing.expect(t, event.kind == .StartOfSelection, "Event should be StartOfSelection")
		}
	}
}

// --- IF statement tests ---

@(test)
basic_if_statement_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `IF lv_var = 1.
  lv_result = 10.
ENDIF.`


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
		if_stmt, ok := file.decls[0].derived_stmt.(^ast.If_Stmt)
		if !testing.expect(t, ok, fmt.tprintf("Expected If_Stmt, got %T", file.decls[0].derived_stmt)) do return

		// Check condition is a binary expression
		testing.expect(t, if_stmt.cond != nil, "Condition should not be nil")
		_, cond_ok := if_stmt.cond.derived_expr.(^ast.Binary_Expr)
		testing.expect(t, cond_ok, "Condition should be Binary_Expr")

		// Check body has one statement
		testing.expect(
			t,
			len(if_stmt.body) == 1,
			fmt.tprintf("Expected 1 body statement, got %d", len(if_stmt.body)),
		)

		// Check no ELSEIF branches
		testing.expect(
			t,
			len(if_stmt.elseif_branches) == 0,
			fmt.tprintf("Expected 0 ELSEIF branches, got %d", len(if_stmt.elseif_branches)),
		)

		// Check no ELSE body
		testing.expect(
			t,
			len(if_stmt.else_body) == 0,
			fmt.tprintf("Expected 0 ELSE body statements, got %d", len(if_stmt.else_body)),
		)
	}
}

@(test)
if_with_else_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `IF lv_var > 0.
  lv_result = 1.
ELSE.
  lv_result = 0.
ENDIF.`


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
		if_stmt, ok := file.decls[0].derived_stmt.(^ast.If_Stmt)
		if !testing.expect(t, ok, "Expected If_Stmt") do return

		testing.expect(
			t,
			len(if_stmt.body) == 1,
			fmt.tprintf("Expected 1 IF body statement, got %d", len(if_stmt.body)),
		)
		testing.expect(
			t,
			len(if_stmt.elseif_branches) == 0,
			fmt.tprintf("Expected 0 ELSEIF branches, got %d", len(if_stmt.elseif_branches)),
		)
		testing.expect(
			t,
			len(if_stmt.else_body) == 1,
			fmt.tprintf("Expected 1 ELSE body statement, got %d", len(if_stmt.else_body)),
		)
	}
}

@(test)
if_with_elseif_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`IF time < '120000'.
  lv_period = 'AM'.
ELSEIF time > '120000'.
  lv_period = 'PM'.
ELSE.
  lv_period = 'NOON'.
ENDIF.`


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
		if_stmt, ok := file.decls[0].derived_stmt.(^ast.If_Stmt)
		if !testing.expect(t, ok, "Expected If_Stmt") do return

		testing.expect(
			t,
			len(if_stmt.body) == 1,
			fmt.tprintf("Expected 1 IF body statement, got %d", len(if_stmt.body)),
		)
		testing.expect(
			t,
			len(if_stmt.elseif_branches) == 1,
			fmt.tprintf("Expected 1 ELSEIF branch, got %d", len(if_stmt.elseif_branches)),
		)
		if len(if_stmt.elseif_branches) > 0 {
			testing.expect(
				t,
				len(if_stmt.elseif_branches[0].body) == 1,
				fmt.tprintf(
					"Expected 1 ELSEIF body statement, got %d",
					len(if_stmt.elseif_branches[0].body),
				),
			)
		}
		testing.expect(
			t,
			len(if_stmt.else_body) == 1,
			fmt.tprintf("Expected 1 ELSE body statement, got %d", len(if_stmt.else_body)),
		)
	}
}

@(test)
if_with_multiple_elseif_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`IF time < '120000'.
  lv_period = 'AM'.
ELSEIF time > '120000' AND time < '180000'.
  lv_period = 'PM'.
ELSEIF time >= '180000'.
  lv_period = 'EVENING'.
ELSE.
  lv_period = 'UNKNOWN'.
ENDIF.`


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
		if_stmt, ok := file.decls[0].derived_stmt.(^ast.If_Stmt)
		if !testing.expect(t, ok, "Expected If_Stmt") do return

		testing.expect(
			t,
			len(if_stmt.elseif_branches) == 2,
			fmt.tprintf("Expected 2 ELSEIF branches, got %d", len(if_stmt.elseif_branches)),
		)
		testing.expect(
			t,
			len(if_stmt.else_body) == 1,
			fmt.tprintf("Expected 1 ELSE body statement, got %d", len(if_stmt.else_body)),
		)
	}
}

@(test)
if_with_and_condition_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `IF lv_a > 0 AND lv_b < 10.
  lv_result = 1.
ENDIF.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		if_stmt, ok := file.decls[0].derived_stmt.(^ast.If_Stmt)
		if !testing.expect(t, ok, "Expected If_Stmt") do return

		// Check that condition is a binary expression with AND
		binary, bok := if_stmt.cond.derived_expr.(^ast.Binary_Expr)
		if testing.expect(t, bok, "Condition should be Binary_Expr") {
			testing.expect(
				t,
				binary.op.lit == "AND",
				fmt.tprintf("Expected AND operator, got '%s'", binary.op.lit),
			)
		}
	}
}

@(test)
if_with_or_condition_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `IF lv_a = 1 OR lv_b = 2.
  lv_result = 1.
ENDIF.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		if_stmt, ok := file.decls[0].derived_stmt.(^ast.If_Stmt)
		if !testing.expect(t, ok, "Expected If_Stmt") do return

		// Check that condition is a binary expression with OR
		binary, bok := if_stmt.cond.derived_expr.(^ast.Binary_Expr)
		if testing.expect(t, bok, "Condition should be Binary_Expr") {
			testing.expect(
				t,
				binary.op.lit == "OR",
				fmt.tprintf("Expected OR operator, got '%s'", binary.op.lit),
			)
		}
	}
}

@(test)
if_with_not_condition_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `IF NOT lv_flag = 'X'.
  lv_result = 1.
ENDIF.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		if_stmt, ok := file.decls[0].derived_stmt.(^ast.If_Stmt)
		if !testing.expect(t, ok, "Expected If_Stmt") do return

		// Check that condition is a unary expression with NOT
		unary, uok := if_stmt.cond.derived_expr.(^ast.Unary_Expr)
		if testing.expect(t, uok, "Condition should be Unary_Expr") {
			testing.expect(
				t,
				unary.op.lit == "NOT",
				fmt.tprintf("Expected NOT operator, got '%s'", unary.op.lit),
			)
		}
	}
}

@(test)
if_with_is_initial_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `IF lv_var IS INITIAL.
  lv_result = 1.
ENDIF.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		if_stmt, ok := file.decls[0].derived_stmt.(^ast.If_Stmt)
		if !testing.expect(t, ok, "Expected If_Stmt") do return

		pred, pok := if_stmt.cond.derived_expr.(^ast.Predicate_Expr)
		if testing.expect(t, pok, "Condition should be Predicate_Expr") {
			testing.expect(
				t,
				pred.predicate == .Initial,
				fmt.tprintf("Expected Initial predicate, got %v", pred.predicate),
			)
			testing.expect(t, !pred.is_negated, "Expected is_negated to be false")
		}
	}
}

@(test)
if_with_is_not_initial_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `IF lv_var IS NOT INITIAL.
  lv_result = 1.
ENDIF.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		if_stmt, ok := file.decls[0].derived_stmt.(^ast.If_Stmt)
		if !testing.expect(t, ok, "Expected If_Stmt") do return

		pred, pok := if_stmt.cond.derived_expr.(^ast.Predicate_Expr)
		if testing.expect(t, pok, "Condition should be Predicate_Expr") {
			testing.expect(
				t,
				pred.predicate == .Initial,
				fmt.tprintf("Expected Initial predicate, got %v", pred.predicate),
			)
			testing.expect(t, pred.is_negated, "Expected is_negated to be true")
		}
	}
}

@(test)
if_with_is_supplied_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `IF p1 IS SUPPLIED.
  lv_result = p1.
ENDIF.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		if_stmt, ok := file.decls[0].derived_stmt.(^ast.If_Stmt)
		if !testing.expect(t, ok, "Expected If_Stmt") do return

		pred, pok := if_stmt.cond.derived_expr.(^ast.Predicate_Expr)
		if testing.expect(t, pok, "Condition should be Predicate_Expr") {
			testing.expect(
				t,
				pred.predicate == .Supplied,
				fmt.tprintf("Expected Supplied predicate, got %v", pred.predicate),
			)
		}
	}
}

@(test)
if_with_is_bound_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `IF lo_ref IS BOUND.
  lv_result = 1.
ENDIF.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		if_stmt, ok := file.decls[0].derived_stmt.(^ast.If_Stmt)
		if !testing.expect(t, ok, "Expected If_Stmt") do return

		pred, pok := if_stmt.cond.derived_expr.(^ast.Predicate_Expr)
		if testing.expect(t, pok, "Condition should be Predicate_Expr") {
			testing.expect(
				t,
				pred.predicate == .Bound,
				fmt.tprintf("Expected Bound predicate, got %v", pred.predicate),
			)
		}
	}
}

@(test)
if_with_complex_condition_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `IF p1 IS SUPPLIED AND p1 <= upper_limit.
  lv_result = p1.
ENDIF.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		if_stmt, ok := file.decls[0].derived_stmt.(^ast.If_Stmt)
		if !testing.expect(t, ok, "Expected If_Stmt") do return

		// Should be AND with left = IS SUPPLIED predicate, right = comparison
		binary, bok := if_stmt.cond.derived_expr.(^ast.Binary_Expr)
		if testing.expect(t, bok, "Condition should be Binary_Expr") {
			testing.expect(
				t,
				binary.op.lit == "AND",
				fmt.tprintf("Expected AND operator, got '%s'", binary.op.lit),
			)

			// Left should be IS SUPPLIED predicate
			_, lpok := binary.left.derived_expr.(^ast.Predicate_Expr)
			testing.expect(t, lpok, "Left operand should be Predicate_Expr")

			// Right should be comparison
			_, rpok := binary.right.derived_expr.(^ast.Binary_Expr)
			testing.expect(t, rpok, "Right operand should be Binary_Expr")
		}
	}
}

@(test)
if_with_not_is_initial_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `IF NOT p IS INITIAL.
  lv_result = 1.
ENDIF.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		if_stmt, ok := file.decls[0].derived_stmt.(^ast.If_Stmt)
		if !testing.expect(t, ok, "Expected If_Stmt") do return

		// Should be NOT with inner IS INITIAL
		unary, uok := if_stmt.cond.derived_expr.(^ast.Unary_Expr)
		if testing.expect(t, uok, "Condition should be Unary_Expr") {
			testing.expect(
				t,
				unary.op.lit == "NOT",
				fmt.tprintf("Expected NOT operator, got '%s'", unary.op.lit),
			)

			pred, pok := unary.expr.derived_expr.(^ast.Predicate_Expr)
			if testing.expect(t, pok, "Inner expression should be Predicate_Expr") {
				testing.expect(
					t,
					pred.predicate == .Initial,
					fmt.tprintf("Expected Initial predicate, got %v", pred.predicate),
				)
			}
		}
	}
}

@(test)
if_with_comparison_operators_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`IF lv_a < 10.
ENDIF.
IF lv_b > 20.
ENDIF.
IF lv_c <= 30.
ENDIF.
IF lv_d >= 40.
ENDIF.
IF lv_e <> 50.
ENDIF.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 5,
		fmt.tprintf("Expected 5 decls, got %v", len(file.decls)),
	)

	// Just verify all parsed as If_Stmt with Binary_Expr conditions
	for i := 0; i < min(len(file.decls), 5); i += 1 {
		if_stmt, ok := file.decls[i].derived_stmt.(^ast.If_Stmt)
		if testing.expect(t, ok, fmt.tprintf("Decl[%d] should be If_Stmt", i)) {
			_, bok := if_stmt.cond.derived_expr.(^ast.Binary_Expr)
			testing.expect(t, bok, fmt.tprintf("Decl[%d] condition should be Binary_Expr", i))
		}
	}
}

@(test)
if_nested_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `IF lv_a = 1.
  IF lv_b = 2.
    lv_result = 1.
  ENDIF.
ENDIF.`


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
		outer_if, ok := file.decls[0].derived_stmt.(^ast.If_Stmt)
		if !testing.expect(t, ok, "Expected If_Stmt") do return

		testing.expect(
			t,
			len(outer_if.body) == 1,
			fmt.tprintf("Expected 1 body statement, got %d", len(outer_if.body)),
		)

		if len(outer_if.body) > 0 {
			inner_if, iok := outer_if.body[0].derived_stmt.(^ast.If_Stmt)
			if testing.expect(t, iok, "Inner statement should be If_Stmt") {
				testing.expect(
					t,
					len(inner_if.body) == 1,
					fmt.tprintf("Expected 1 nested body statement, got %d", len(inner_if.body)),
				)
			}
		}
	}
}

@(test)
if_with_data_declarations_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `IF lv_flag = 'X'.
  DATA lv_local TYPE i.
  lv_local = 10.
ENDIF.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		if_stmt, ok := file.decls[0].derived_stmt.(^ast.If_Stmt)
		if !testing.expect(t, ok, "Expected If_Stmt") do return

		testing.expect(
			t,
			len(if_stmt.body) == 2,
			fmt.tprintf("Expected 2 body statements, got %d", len(if_stmt.body)),
		)

		if len(if_stmt.body) >= 2 {
			_, dok := if_stmt.body[0].derived_stmt.(^ast.Data_Typed_Decl)
			testing.expect(t, dok, "First body statement should be Data_Typed_Decl")

			_, aok := if_stmt.body[1].derived_stmt.(^ast.Assign_Stmt)
			testing.expect(t, aok, "Second body statement should be Assign_Stmt")
		}
	}
}

@(test)
if_inside_form_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`FORM check_value USING p_val TYPE i.
  IF p_val > 0.
    lv_result = 1.
  ELSE.
    lv_result = 0.
  ENDIF.
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
		form, ok := file.decls[0].derived_stmt.(^ast.Form_Decl)
		if !testing.expect(t, ok, "Expected Form_Decl") do return

		testing.expect(
			t,
			len(form.body) == 1,
			fmt.tprintf("Expected 1 body statement, got %d", len(form.body)),
		)

		if len(form.body) > 0 {
			if_stmt, iok := form.body[0].derived_stmt.(^ast.If_Stmt)
			if testing.expect(t, iok, "Body statement should be If_Stmt") {
				testing.expect(
					t,
					len(if_stmt.body) == 1,
					fmt.tprintf("Expected 1 IF body statement, got %d", len(if_stmt.body)),
				)
				testing.expect(
					t,
					len(if_stmt.else_body) == 1,
					fmt.tprintf("Expected 1 ELSE body statement, got %d", len(if_stmt.else_body)),
				)
			}
		}
	}
}

@(test)
if_inside_method_impl_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`CLASS c1 IMPLEMENTATION.
  METHOD m1.
    IF iv_input IS NOT INITIAL.
      DATA lv_result TYPE i.
      lv_result = iv_input.
    ENDIF.
  ENDMETHOD.
ENDCLASS.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		class_impl, ok := file.decls[0].derived_stmt.(^ast.Class_Impl_Decl)
		if !testing.expect(t, ok, "Expected Class_Impl_Decl") do return

		if len(class_impl.methods) > 0 {
			method, mok := class_impl.methods[0].derived_stmt.(^ast.Method_Impl)
			if testing.expect(t, mok, "Expected Method_Impl") {
				testing.expect(
					t,
					len(method.body) == 1,
					fmt.tprintf("Expected 1 method body statement, got %d", len(method.body)),
				)

				if len(method.body) > 0 {
					_, iok := method.body[0].derived_stmt.(^ast.If_Stmt)
					testing.expect(t, iok, "Method body should contain If_Stmt")
				}
			}
		}
	}
}

// --- Arrow (->) Expression Tests ---

@(test)
arrow_selector_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `lv_result = lo_object->get_value.`
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
		assign, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
		if !testing.expect(t, ok, "Expected Assign_Stmt") do return

		if len(assign.rhs) > 0 {
			sel, sok := assign.rhs[0].derived_expr.(^ast.Selector_Expr)
			if !testing.expect(t, sok, "Expected Selector_Expr on RHS") do return

			testing.expect(
				t,
				sel.op.kind == .Arrow,
				fmt.tprintf("Expected Arrow op, got %v", sel.op.kind),
			)
			testing.expect(
				t,
				sel.field.name == "get_value",
				fmt.tprintf("Expected field 'get_value', got '%s'", sel.field.name),
			)

			// Check the base expression
			base_ident, iok := sel.expr.derived_expr.(^ast.Ident)
			if testing.expect(t, iok, "Expected Ident as base expression") {
				testing.expect(
					t,
					base_ident.name == "lo_object",
					fmt.tprintf("Expected 'lo_object', got '%s'", base_ident.name),
				)
			}
		}
	}
}

@(test)
chained_arrow_selector_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `lv_result = lo_app->get_controller->get_view.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		assign, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
		if !testing.expect(t, ok, "Expected Assign_Stmt") do return

		if len(assign.rhs) > 0 {
			outer_sel, sok := assign.rhs[0].derived_expr.(^ast.Selector_Expr)
			if !testing.expect(t, sok, "Expected outer Selector_Expr") do return

			testing.expect(t, outer_sel.op.kind == .Arrow, "Expected Arrow op on outer selector")
			testing.expect(t, outer_sel.field.name == "get_view", "Expected field 'get_view'")

			inner_sel, isok := outer_sel.expr.derived_expr.(^ast.Selector_Expr)
			if testing.expect(t, isok, "Expected inner Selector_Expr") {
				testing.expect(
					t,
					inner_sel.op.kind == .Arrow,
					"Expected Arrow op on inner selector",
				)
				testing.expect(
					t,
					inner_sel.field.name == "get_controller",
					"Expected field 'get_controller'",
				)
			}
		}
	}
}

// --- Call Expression Tests ---

@(test)
simple_call_expr_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `lv_result = lo_object->get_value( ).`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		assign, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
		if !testing.expect(t, ok, "Expected Assign_Stmt") do return

		if len(assign.rhs) > 0 {
			call, cok := assign.rhs[0].derived_expr.(^ast.Call_Expr)
			if !testing.expect(t, cok, "Expected Call_Expr on RHS") do return

			testing.expect(t, len(call.args) == 0, "Expected 0 args")

			// The callee should be a selector expression
			sel, sok := call.expr.derived_expr.(^ast.Selector_Expr)
			if testing.expect(t, sok, "Expected Selector_Expr as callee") {
				testing.expect(t, sel.op.kind == .Arrow, "Expected Arrow op")
				testing.expect(t, sel.field.name == "get_value", "Expected field 'get_value'")
			}
		}
	}
}

@(test)
call_expr_with_args_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `lv_result = lo_object->set_value( iv_value ).`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		assign, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
		if !testing.expect(t, ok, "Expected Assign_Stmt") do return

		if len(assign.rhs) > 0 {
			call, cok := assign.rhs[0].derived_expr.(^ast.Call_Expr)
			if !testing.expect(t, cok, "Expected Call_Expr on RHS") do return

			testing.expect(
				t,
				len(call.args) == 1,
				fmt.tprintf("Expected 1 arg, got %d", len(call.args)),
			)

			if len(call.args) > 0 {
				arg_ident, aok := call.args[0].derived_expr.(^ast.Ident)
				if testing.expect(t, aok, "Expected Ident arg") {
					testing.expect(t, arg_ident.name == "iv_value", "Expected arg 'iv_value'")
				}
			}
		}
	}
}

@(test)
chained_call_expr_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `lv_result = lo_builder->set_name( lv_name )->build( ).`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		assign, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
		if !testing.expect(t, ok, "Expected Assign_Stmt") do return

		if len(assign.rhs) > 0 {
			// Outer call: ->build( )
			outer_call, ocok := assign.rhs[0].derived_expr.(^ast.Call_Expr)
			if !testing.expect(t, ocok, "Expected outer Call_Expr") do return

			testing.expect(t, len(outer_call.args) == 0, "Expected 0 args for build()")

			// The callee of build() is a selector
			build_sel, bsok := outer_call.expr.derived_expr.(^ast.Selector_Expr)
			if !testing.expect(t, bsok, "Expected Selector_Expr for build") do return

			testing.expect(t, build_sel.field.name == "build", "Expected field 'build'")

			// Inner call: ->set_name( lv_name )
			inner_call, icok := build_sel.expr.derived_expr.(^ast.Call_Expr)
			if !testing.expect(t, icok, "Expected inner Call_Expr for set_name") do return

			testing.expect(
				t,
				len(inner_call.args) == 1,
				fmt.tprintf("Expected 1 arg for set_name, got %d", len(inner_call.args)),
			)
		}
	}
}

@(test)
method_call_standalone_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `lo_object->process( ).`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		expr_stmt, ok := file.decls[0].derived_stmt.(^ast.Expr_Stmt)
		if !testing.expect(t, ok, "Expected Expr_Stmt") do return

		call, cok := expr_stmt.expr.derived_expr.(^ast.Call_Expr)
		if !testing.expect(t, cok, "Expected Call_Expr") do return

		sel, sok := call.expr.derived_expr.(^ast.Selector_Expr)
		if testing.expect(t, sok, "Expected Selector_Expr as callee") {
			testing.expect(t, sel.op.kind == .Arrow, "Expected Arrow op")
			testing.expect(t, sel.field.name == "process", "Expected field 'process'")
		}
	}
}

@(test)
call_with_literal_arg_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `lv_result = lo_calc->add( 42 ).`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		assign, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
		if !testing.expect(t, ok, "Expected Assign_Stmt") do return

		if len(assign.rhs) > 0 {
			call, cok := assign.rhs[0].derived_expr.(^ast.Call_Expr)
			if !testing.expect(t, cok, "Expected Call_Expr") do return

			if len(call.args) > 0 {
				lit, lok := call.args[0].derived_expr.(^ast.Basic_Lit)
				if testing.expect(t, lok, "Expected Basic_Lit arg") {
					testing.expect(t, lit.tok.lit == "42", "Expected arg '42'")
				}
			}
		}
	}
}

@(test)
named_arg_single_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `lv_result = cl_class=>method( iv_param = 'value' ).`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		assign, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
		if !testing.expect(t, ok, "Expected Assign_Stmt") do return

		if len(assign.rhs) > 0 {
			call, cok := assign.rhs[0].derived_expr.(^ast.Call_Expr)
			if !testing.expect(t, cok, "Expected Call_Expr") do return

			testing.expect(
				t,
				len(call.args) == 1,
				fmt.tprintf("Expected 1 arg, got %d", len(call.args)),
			)

			if len(call.args) > 0 {
				named, nok := call.args[0].derived_expr.(^ast.Named_Arg)
				if testing.expect(t, nok, "Expected Named_Arg") {
					testing.expect(
						t,
						named.name.name == "iv_param",
						"Expected param name 'iv_param'",
					)
					lit, lok := named.value.derived_expr.(^ast.Basic_Lit)
					if testing.expect(t, lok, "Expected Basic_Lit value") {
						testing.expect(t, lit.tok.lit == "'value'", "Expected value \"'value'\"")
					}
				}
			}
		}
	}
}

@(test)
named_arg_multiple_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `lv_result = cl_class=>method( iv_param1 = 'value1' iv_param2 = 42 ).`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		assign, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
		if !testing.expect(t, ok, "Expected Assign_Stmt") do return

		if len(assign.rhs) > 0 {
			call, cok := assign.rhs[0].derived_expr.(^ast.Call_Expr)
			if !testing.expect(t, cok, "Expected Call_Expr") do return

			testing.expect(
				t,
				len(call.args) == 2,
				fmt.tprintf("Expected 2 args, got %d", len(call.args)),
			)

			if len(call.args) >= 2 {
				// Check first named arg
				named1, n1ok := call.args[0].derived_expr.(^ast.Named_Arg)
				if testing.expect(t, n1ok, "Expected first Named_Arg") {
					testing.expect(
						t,
						named1.name.name == "iv_param1",
						"Expected param name 'iv_param1'",
					)
				}

				// Check second named arg
				named2, n2ok := call.args[1].derived_expr.(^ast.Named_Arg)
				if testing.expect(t, n2ok, "Expected second Named_Arg") {
					testing.expect(
						t,
						named2.name.name == "iv_param2",
						"Expected param name 'iv_param2'",
					)
				}
			}
		}
	}
}

@(test)
named_arg_multiline_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`mo_messages = /sttpec/cl_message_ctrl=>create(
		iv_object    = 'ZATTP'
		iv_subobject = 'SN_RESET'
	).`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		assign, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
		if !testing.expect(t, ok, "Expected Assign_Stmt") do return

		if len(assign.rhs) > 0 {
			call, cok := assign.rhs[0].derived_expr.(^ast.Call_Expr)
			if !testing.expect(t, cok, "Expected Call_Expr") do return

			testing.expect(
				t,
				len(call.args) == 2,
				fmt.tprintf("Expected 2 args, got %d", len(call.args)),
			)

			if len(call.args) >= 2 {
				named1, n1ok := call.args[0].derived_expr.(^ast.Named_Arg)
				if testing.expect(t, n1ok, "Expected first Named_Arg") {
					testing.expect(
						t,
						named1.name.name == "iv_object",
						"Expected param name 'iv_object'",
					)
				}

				named2, n2ok := call.args[1].derived_expr.(^ast.Named_Arg)
				if testing.expect(t, n2ok, "Expected second Named_Arg") {
					testing.expect(
						t,
						named2.name.name == "iv_subobject",
						"Expected param name 'iv_subobject'",
					)
				}
			}
		}
	}
}

@(test)
named_arg_with_ident_value_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `lv_result = cl_class=>method( iv_param = lv_value ).`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		assign, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
		if !testing.expect(t, ok, "Expected Assign_Stmt") do return

		if len(assign.rhs) > 0 {
			call, cok := assign.rhs[0].derived_expr.(^ast.Call_Expr)
			if !testing.expect(t, cok, "Expected Call_Expr") do return

			if len(call.args) > 0 {
				named, nok := call.args[0].derived_expr.(^ast.Named_Arg)
				if testing.expect(t, nok, "Expected Named_Arg") {
					testing.expect(
						t,
						named.name.name == "iv_param",
						"Expected param name 'iv_param'",
					)
					ident_val, iok := named.value.derived_expr.(^ast.Ident)
					if testing.expect(t, iok, "Expected Ident value") {
						testing.expect(
							t,
							ident_val.name == "lv_value",
							"Expected value 'lv_value'",
						)
					}
				}
			}
		}
	}
}

@(test)
while_loop_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`
    WHILE lv_string IS NOT INITIAL AND lv_index <= 4.
    	lv_index = lv_index + 1.
    ENDWHILE.
`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)
}

// --- String Template Tests ---

// Builder for string template expressions
string_template :: proc(parts: ..ast.String_Template_Part) -> ^ast.String_Template_Expr {
	node := ast.new(ast.String_Template_Expr, {})
	node.parts = make([dynamic]ast.String_Template_Part)
	for part in parts {
		append(&node.parts, part)
	}
	node.derived_expr = node
	return node
}

string_template_lit :: proc(literal: string) -> ast.String_Template_Part {
	return ast.String_Template_Part{is_expr = false, literal = literal}
}

string_template_expr :: proc(expr: ast.Any_Expr) -> ast.String_Template_Part {
	part := ast.String_Template_Part {
		is_expr = true,
	}
	#partial switch e in expr {
	case ^ast.Ident:
		part.expr = &e.node
	case ^ast.Selector_Expr:
		part.expr = &e.node
	case ^ast.Basic_Lit:
		part.expr = &e.node
	}
	return part
}

// Check string template expression
check_string_template :: proc(
	t: ^testing.T,
	expected: ^ast.String_Template_Expr,
	actual: ^ast.String_Template_Expr,
	loc := #caller_location,
) {
	if !testing.expect(t, len(expected.parts) == len(actual.parts), fmt.tprintf("Expected %d parts, got %d", len(expected.parts), len(actual.parts)), loc = loc) do return

	for i := 0; i < len(expected.parts); i += 1 {
		ex_part := expected.parts[i]
		ac_part := actual.parts[i]

		testing.expect(
			t,
			ex_part.is_expr == ac_part.is_expr,
			fmt.tprintf(
				"Part[%d] is_expr mismatch: expected %v, got %v",
				i,
				ex_part.is_expr,
				ac_part.is_expr,
			),
			loc = loc,
		)

		if !ex_part.is_expr {
			testing.expect(
				t,
				ex_part.literal == ac_part.literal,
				fmt.tprintf(
					"Part[%d] literal mismatch: expected '%s', got '%s'",
					i,
					ex_part.literal,
					ac_part.literal,
				),
				loc = loc,
			)
		} else {
			if ex_part.expr != nil {
				check_expr(t, ex_part.expr.derived_expr, ac_part.expr, loc = loc)
			}
		}
	}
}

@(test)
basic_string_template_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = |Hello World!|.`
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
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		template, tok := decl.value.derived_expr.(^ast.String_Template_Expr)
		if !testing.expect(t, tok, "Expected String_Template_Expr") do return

		testing.expect(
			t,
			len(template.parts) == 1,
			fmt.tprintf("Expected 1 part, got %d", len(template.parts)),
		)
		if len(template.parts) > 0 {
			testing.expect(t, !template.parts[0].is_expr, "Expected literal part")
			testing.expect(
				t,
				template.parts[0].literal == "Hello World!",
				fmt.tprintf("Expected 'Hello World!', got '%s'", template.parts[0].literal),
			)
		}
	}
}

@(test)
string_template_concatenation_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = |Hello| & | | & |World|.`
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
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		// Should be a binary expression (concatenation)
		binary, bok := decl.value.derived_expr.(^ast.Binary_Expr)
		if !testing.expect(t, bok, fmt.tprintf("Expected Binary_Expr, got %T", decl.value.derived_expr)) do return

		testing.expect(
			t,
			binary.op.kind == .Ampersand,
			fmt.tprintf("Expected Ampersand operator, got %v", binary.op.kind),
		)
	}
}

@(test)
string_template_with_embedded_expr_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = |Hello { lv_name }!|.`
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
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		template, tok := decl.value.derived_expr.(^ast.String_Template_Expr)
		if !testing.expect(t, tok, fmt.tprintf("Expected String_Template_Expr, got %T", decl.value.derived_expr)) do return

		// Should have 3 parts: "Hello ", embedded expr, "!"
		testing.expect(
			t,
			len(template.parts) == 3,
			fmt.tprintf("Expected 3 parts, got %d", len(template.parts)),
		)

		if len(template.parts) >= 3 {
			// First part: "Hello "
			testing.expect(t, !template.parts[0].is_expr, "Part 0 should be literal")
			testing.expect(
				t,
				template.parts[0].literal == "Hello ",
				fmt.tprintf("Expected 'Hello ', got '%s'", template.parts[0].literal),
			)

			// Second part: embedded expression
			testing.expect(t, template.parts[1].is_expr, "Part 1 should be expression")
			if template.parts[1].expr != nil {
				ident, iok := template.parts[1].expr.derived_expr.(^ast.Ident)
				if testing.expect(t, iok, "Expected Ident expression") {
					testing.expect(
						t,
						ident.name == "lv_name",
						fmt.tprintf("Expected 'lv_name', got '%s'", ident.name),
					)
				}
			}

			// Third part: "!"
			testing.expect(t, !template.parts[2].is_expr, "Part 2 should be literal")
			testing.expect(
				t,
				template.parts[2].literal == "!",
				fmt.tprintf("Expected '!', got '%s'", template.parts[2].literal),
			)
		}
	}
}

@(test)
string_template_with_selector_expr_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = |Hello { sy-uname }!|.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		template, tok := decl.value.derived_expr.(^ast.String_Template_Expr)
		if !testing.expect(t, tok, fmt.tprintf("Expected String_Template_Expr, got %T", decl.value.derived_expr)) do return

		// Should have 3 parts
		if len(template.parts) >= 2 {
			// Second part should be a selector expression
			testing.expect(t, template.parts[1].is_expr, "Part 1 should be expression")
			if template.parts[1].expr != nil {
				sel, sok := template.parts[1].expr.derived_expr.(^ast.Selector_Expr)
				if testing.expect(
					t,
					sok,
					fmt.tprintf(
						"Expected Selector_Expr, got %T",
						template.parts[1].expr.derived_expr,
					),
				) {
					testing.expect(
						t,
						sel.field.name == "uname",
						fmt.tprintf("Expected 'uname', got '%s'", sel.field.name),
					)
				}
			}
		}
	}
}

@(test)
empty_string_template_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = ||.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		template, tok := decl.value.derived_expr.(^ast.String_Template_Expr)
		if !testing.expect(t, tok, fmt.tprintf("Expected String_Template_Expr, got %T", decl.value.derived_expr)) do return

		// Empty template should have 0 parts
		testing.expect(
			t,
			len(template.parts) == 0,
			fmt.tprintf("Expected 0 parts, got %d", len(template.parts)),
		)
	}
}

@(test)
string_template_only_embedded_expr_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = |{ lv_value }|.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		template, tok := decl.value.derived_expr.(^ast.String_Template_Expr)
		if !testing.expect(t, tok, fmt.tprintf("Expected String_Template_Expr, got %T", decl.value.derived_expr)) do return

		// Should have 1 part: the embedded expression
		testing.expect(
			t,
			len(template.parts) == 1,
			fmt.tprintf("Expected 1 part, got %d", len(template.parts)),
		)

		if len(template.parts) > 0 {
			testing.expect(t, template.parts[0].is_expr, "Part should be expression")
		}
	}
}

@(test)
string_template_embedded_expr_alpha_out_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = |{ ls_item_result-matnr ALPHA = OUT }|.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		template, tok := decl.value.derived_expr.(^ast.String_Template_Expr)
		if !testing.expect(t, tok, fmt.tprintf("Expected String_Template_Expr, got %T", decl.value.derived_expr)) do return

		// Should have 1 part: the embedded expression
		testing.expect(
			t,
			len(template.parts) == 1,
			fmt.tprintf("Expected 1 part, got %d", len(template.parts)),
		)

		if len(template.parts) > 0 {
			part := template.parts[0]
			testing.expect(t, part.is_expr, "Part should be expression")

			// Check the expression is a selector expression (ls_item_result-matnr)
			if sel, sok := part.expr.derived_expr.(^ast.Selector_Expr); sok {
				if sel.field != nil {
					testing.expect(
						t,
						sel.field.name == "matnr",
						fmt.tprintf("Expected field 'matnr', got '%s'", sel.field.name),
					)
				}
			}

			// Check formatting options
			testing.expect(
				t,
				len(part.format_options) == 1,
				fmt.tprintf("Expected 1 format option, got %d", len(part.format_options)),
			)

			if len(part.format_options) > 0 {
				opt := part.format_options[0]
				testing.expect(
					t,
					opt.kind == .Alpha,
					fmt.tprintf("Expected Alpha format kind, got %v", opt.kind),
				)
				testing.expect(
					t,
					opt.value == .Out,
					fmt.tprintf("Expected Out value, got %v", opt.value),
				)
			}
		}
	}
}

@(test)
string_template_embedded_expr_alpha_in_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = |{ ls_item_result-matnr ALPHA = IN }|.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		template, tok := decl.value.derived_expr.(^ast.String_Template_Expr)
		if !testing.expect(t, tok, fmt.tprintf("Expected String_Template_Expr, got %T", decl.value.derived_expr)) do return

		// Should have 1 part: the embedded expression
		testing.expect(
			t,
			len(template.parts) == 1,
			fmt.tprintf("Expected 1 part, got %d", len(template.parts)),
		)

		if len(template.parts) > 0 {
			part := template.parts[0]
			testing.expect(t, part.is_expr, "Part should be expression")

			// Check formatting options
			testing.expect(
				t,
				len(part.format_options) == 1,
				fmt.tprintf("Expected 1 format option, got %d", len(part.format_options)),
			)

			if len(part.format_options) > 0 {
				opt := part.format_options[0]
				testing.expect(
					t,
					opt.kind == .Alpha,
					fmt.tprintf("Expected Alpha format kind, got %v", opt.kind),
				)
				testing.expect(
					t,
					opt.value == .In,
					fmt.tprintf("Expected In value, got %v", opt.value),
				)
			}
		}
	}
}

@(test)
string_template_embedded_expr_in_value_constructor_test :: proc(t: ^testing.T) {
	// Test the usage example from the user: embedded expression inside VALUE constructor
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`APPEND VALUE #(
        gs1_es      = ls_item_result-gs1_es_b
        objtype     = ev_objtype
        gtin        = ls_item_result-gtin
        matnr       = |{ ls_item_result-matnr ALPHA = OUT }|
        lotno       = ls_item_result-lotno
        status_pack = ls_item_result-status_pack
        gln         = ls_item_result-gln
        gln_ext     = ls_item_result-gln_ext
        locno       = ls_item_result-locno
        status      = iv_status
      ) TO mt_object_info.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	// Should parse as an APPEND statement
	testing.expect(
		t,
		len(file.decls) == 1,
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) > 0 {
		append_stmt, ok := file.decls[0].derived_stmt.(^ast.Append_Stmt)
		testing.expect(
			t,
			ok,
			fmt.tprintf("Expected Append_Stmt, got %T", file.decls[0].derived_stmt),
		)

		if ok && append_stmt.source != nil {
			// The source should be a Constructor_Expr (VALUE #(...))
			constr, cok := append_stmt.source.derived_expr.(^ast.Constructor_Expr)
			if testing.expect(
				t,
				cok,
				fmt.tprintf("Expected Constructor_Expr, got %T", append_stmt.source.derived_expr),
			) {
				// Find the matnr argument which should have a string template
				for arg in constr.args {
					if named, nok := arg.derived_expr.(^ast.Named_Arg); nok {
						if named.name != nil && named.name.name == "matnr" {
							// Check the value is a string template
							if template, tok := named.value.derived_expr.(^ast.String_Template_Expr);
							   tok {
								testing.expect(
									t,
									len(template.parts) == 1,
									fmt.tprintf("Expected 1 part, got %d", len(template.parts)),
								)

								if len(template.parts) > 0 {
									part := template.parts[0]
									testing.expect(t, part.is_expr, "Part should be expression")
									testing.expect(
										t,
										len(part.format_options) == 1,
										fmt.tprintf(
											"Expected 1 format option, got %d",
											len(part.format_options),
										),
									)

									if len(part.format_options) > 0 {
										opt := part.format_options[0]
										testing.expect(
											t,
											opt.kind == .Alpha,
											"Expected Alpha format kind",
										)
										testing.expect(t, opt.value == .Out, "Expected Out value")
									}
								}
							}
						}
					}
				}
			}
		}
	}
}

@(test)
string_template_multiple_format_options_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = |{ lv_date DATE = ISO }|.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		template, tok := decl.value.derived_expr.(^ast.String_Template_Expr)
		if !testing.expect(t, tok, fmt.tprintf("Expected String_Template_Expr, got %T", decl.value.derived_expr)) do return

		if len(template.parts) > 0 {
			part := template.parts[0]
			testing.expect(t, part.is_expr, "Part should be expression")

			// Check formatting options
			testing.expect(
				t,
				len(part.format_options) == 1,
				fmt.tprintf("Expected 1 format option, got %d", len(part.format_options)),
			)

			if len(part.format_options) > 0 {
				opt := part.format_options[0]
				testing.expect(
					t,
					opt.kind == .Date,
					fmt.tprintf("Expected Date format kind, got %v", opt.kind),
				)
				testing.expect(
					t,
					opt.value == .Iso,
					fmt.tprintf("Expected Iso value, got %v", opt.value),
				)
			}
		}
	}
}

@(test)
string_template_width_format_option_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = |{ lv_number WIDTH = 10 }|.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		template, tok := decl.value.derived_expr.(^ast.String_Template_Expr)
		if !testing.expect(t, tok, fmt.tprintf("Expected String_Template_Expr, got %T", decl.value.derived_expr)) do return

		if len(template.parts) > 0 {
			part := template.parts[0]
			testing.expect(t, part.is_expr, "Part should be expression")

			// Check formatting options
			testing.expect(
				t,
				len(part.format_options) == 1,
				fmt.tprintf("Expected 1 format option, got %d", len(part.format_options)),
			)

			if len(part.format_options) > 0 {
				opt := part.format_options[0]
				testing.expect(
					t,
					opt.kind == .Width,
					fmt.tprintf("Expected Width format kind, got %v", opt.kind),
				)
				testing.expect(
					t,
					opt.value == .Custom,
					fmt.tprintf("Expected Custom value, got %v", opt.value),
				)
				testing.expect(
					t,
					opt.num_value == 10,
					fmt.tprintf("Expected num_value 10, got %d", opt.num_value),
				)
			}
		}
	}
}

// --- Arithmetic Expression Tests ---

@(test)
arithmetic_simple_addition_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = a + b.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		binary, bok := decl.value.derived_expr.(^ast.Binary_Expr)
		if !testing.expect(t, bok, fmt.tprintf("Expected Binary_Expr, got %T", decl.value.derived_expr)) do return

		testing.expect(
			t,
			binary.op.kind == .Plus,
			fmt.tprintf("Expected Plus operator, got %v", binary.op.kind),
		)

		left_ident, lok := binary.left.derived_expr.(^ast.Ident)
		if testing.expect(t, lok, "Left should be Ident") {
			testing.expect(
				t,
				left_ident.name == "a",
				fmt.tprintf("Expected 'a', got '%s'", left_ident.name),
			)
		}

		right_ident, rok := binary.right.derived_expr.(^ast.Ident)
		if testing.expect(t, rok, "Right should be Ident") {
			testing.expect(
				t,
				right_ident.name == "b",
				fmt.tprintf("Expected 'b', got '%s'", right_ident.name),
			)
		}
	}
}

@(test)
arithmetic_simple_subtraction_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = x - y.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		binary, bok := decl.value.derived_expr.(^ast.Binary_Expr)
		if !testing.expect(t, bok, fmt.tprintf("Expected Binary_Expr, got %T", decl.value.derived_expr)) do return

		testing.expect(
			t,
			binary.op.kind == .Minus,
			fmt.tprintf("Expected Minus operator, got %v", binary.op.kind),
		)
	}
}

@(test)
arithmetic_multiplication_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = a * b.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		binary, bok := decl.value.derived_expr.(^ast.Binary_Expr)
		if !testing.expect(t, bok, fmt.tprintf("Expected Binary_Expr, got %T", decl.value.derived_expr)) do return

		testing.expect(
			t,
			binary.op.kind == .Star,
			fmt.tprintf("Expected Star operator, got %v", binary.op.kind),
		)
	}
}

@(test)
arithmetic_division_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = a / b.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		binary, bok := decl.value.derived_expr.(^ast.Binary_Expr)
		if !testing.expect(t, bok, fmt.tprintf("Expected Binary_Expr, got %T", decl.value.derived_expr)) do return

		testing.expect(
			t,
			binary.op.kind == .Slash,
			fmt.tprintf("Expected Slash operator, got %v", binary.op.kind),
		)
	}
}

@(test)
arithmetic_mod_operator_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = a MOD b.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		binary, bok := decl.value.derived_expr.(^ast.Binary_Expr)
		if !testing.expect(t, bok, fmt.tprintf("Expected Binary_Expr, got %T", decl.value.derived_expr)) do return

		// MOD is an identifier token
		testing.expect(
			t,
			binary.op.kind == .Ident,
			fmt.tprintf("Expected Ident operator (MOD), got %v", binary.op.kind),
		)
	}
}

@(test)
arithmetic_div_operator_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = a DIV b.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		binary, bok := decl.value.derived_expr.(^ast.Binary_Expr)
		if !testing.expect(t, bok, fmt.tprintf("Expected Binary_Expr, got %T", decl.value.derived_expr)) do return

		// DIV is an identifier token
		testing.expect(
			t,
			binary.op.kind == .Ident,
			fmt.tprintf("Expected Ident operator (DIV), got %v", binary.op.kind),
		)
	}
}

@(test)
arithmetic_operator_precedence_test :: proc(t: ^testing.T) {
	// a + b * c should parse as a + (b * c) due to precedence
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = a + b * c.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		// Top level should be addition (lower precedence)
		binary, bok := decl.value.derived_expr.(^ast.Binary_Expr)
		if !testing.expect(t, bok, fmt.tprintf("Expected Binary_Expr, got %T", decl.value.derived_expr)) do return

		testing.expect(
			t,
			binary.op.kind == .Plus,
			fmt.tprintf("Top level should be Plus, got %v", binary.op.kind),
		)

		// Left should be 'a'
		left_ident, lok := binary.left.derived_expr.(^ast.Ident)
		if testing.expect(t, lok, "Left should be Ident") {
			testing.expect(
				t,
				left_ident.name == "a",
				fmt.tprintf("Expected 'a', got '%s'", left_ident.name),
			)
		}

		// Right should be b * c (multiplication)
		right_binary, rbok := binary.right.derived_expr.(^ast.Binary_Expr)
		if testing.expect(t, rbok, "Right should be Binary_Expr (multiplication)") {
			testing.expect(
				t,
				right_binary.op.kind == .Star,
				fmt.tprintf("Right Binary should be Star, got %v", right_binary.op.kind),
			)
		}
	}
}

@(test)
arithmetic_left_associativity_test :: proc(t: ^testing.T) {
	// a - b - c should parse as (a - b) - c (left to right)
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = a - b - c.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		// Top level should be subtraction
		binary, bok := decl.value.derived_expr.(^ast.Binary_Expr)
		if !testing.expect(t, bok, fmt.tprintf("Expected Binary_Expr, got %T", decl.value.derived_expr)) do return

		testing.expect(
			t,
			binary.op.kind == .Minus,
			fmt.tprintf("Top level should be Minus, got %v", binary.op.kind),
		)

		// Right should be 'c'
		right_ident, rok := binary.right.derived_expr.(^ast.Ident)
		if testing.expect(t, rok, "Right should be Ident") {
			testing.expect(
				t,
				right_ident.name == "c",
				fmt.tprintf("Expected 'c', got '%s'", right_ident.name),
			)
		}

		// Left should be (a - b) - another binary expr
		left_binary, lbok := binary.left.derived_expr.(^ast.Binary_Expr)
		if testing.expect(t, lbok, "Left should be Binary_Expr") {
			testing.expect(
				t,
				left_binary.op.kind == .Minus,
				fmt.tprintf("Left Binary should be Minus, got %v", left_binary.op.kind),
			)
		}
	}
}

@(test)
arithmetic_with_numbers_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = 10 + 20.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		binary, bok := decl.value.derived_expr.(^ast.Binary_Expr)
		if !testing.expect(t, bok, fmt.tprintf("Expected Binary_Expr, got %T", decl.value.derived_expr)) do return

		left_lit, lok := binary.left.derived_expr.(^ast.Basic_Lit)
		if testing.expect(t, lok, "Left should be Basic_Lit") {
			testing.expect(
				t,
				left_lit.tok.lit == "10",
				fmt.tprintf("Expected '10', got '%s'", left_lit.tok.lit),
			)
		}

		right_lit, rlok := binary.right.derived_expr.(^ast.Basic_Lit)
		if testing.expect(t, rlok, "Right should be Basic_Lit") {
			testing.expect(
				t,
				right_lit.tok.lit == "20",
				fmt.tprintf("Expected '20', got '%s'", right_lit.tok.lit),
			)
		}
	}
}

@(test)
arithmetic_unary_negation_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = -a.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		unary, uok := decl.value.derived_expr.(^ast.Unary_Expr)
		if !testing.expect(t, uok, fmt.tprintf("Expected Unary_Expr, got %T", decl.value.derived_expr)) do return

		testing.expect(
			t,
			unary.op.kind == .Minus,
			fmt.tprintf("Expected Minus operator, got %v", unary.op.kind),
		)

		inner_ident, iok := unary.expr.derived_expr.(^ast.Ident)
		if testing.expect(t, iok, "Inner should be Ident") {
			testing.expect(
				t,
				inner_ident.name == "a",
				fmt.tprintf("Expected 'a', got '%s'", inner_ident.name),
			)
		}
	}
}

@(test)
arithmetic_parenthesized_expr_test :: proc(t: ^testing.T) {
	// (a + b) * c - parentheses should change precedence
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = ( a + b ) * c.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		// Top level should be multiplication
		binary, bok := decl.value.derived_expr.(^ast.Binary_Expr)
		if !testing.expect(t, bok, fmt.tprintf("Expected Binary_Expr, got %T", decl.value.derived_expr)) do return

		testing.expect(
			t,
			binary.op.kind == .Star,
			fmt.tprintf("Top level should be Star, got %v", binary.op.kind),
		)

		// Left should be a parenthesized expression
		paren, pok := binary.left.derived_expr.(^ast.Paren_Expr)
		if testing.expect(t, pok, "Left should be Paren_Expr") {
			// Inside paren should be addition
			inner_binary, ibok := paren.expr.derived_expr.(^ast.Binary_Expr)
			if testing.expect(t, ibok, "Inner should be Binary_Expr") {
				testing.expect(
					t,
					inner_binary.op.kind == .Plus,
					fmt.tprintf("Inner should be Plus, got %v", inner_binary.op.kind),
				)
			}
		}

		// Right should be 'c'
		right_ident, rok := binary.right.derived_expr.(^ast.Ident)
		if testing.expect(t, rok, "Right should be Ident") {
			testing.expect(
				t,
				right_ident.name == "c",
				fmt.tprintf("Expected 'c', got '%s'", right_ident.name),
			)
		}
	}
}

@(test)
arithmetic_complex_expression_test :: proc(t: ^testing.T) {
	// a * b + c / d - e should parse correctly with precedence
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(result) = a * b + c / d - e.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
		if !testing.expect(t, ok, "Expected Data_Inline_Decl") do return

		// Expression should parse as ((a * b) + (c / d)) - e
		binary, bok := decl.value.derived_expr.(^ast.Binary_Expr)
		testing.expect(
			t,
			bok,
			fmt.tprintf("Expected Binary_Expr, got %T", decl.value.derived_expr),
		)
		// Just verify it parses without error - the exact structure is complex
	}
}

@(test)
arithmetic_in_assignment_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `result = a + b * c.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		assign, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
		if !testing.expect(t, ok, fmt.tprintf("Expected Assign_Stmt, got %T", file.decls[0].derived_stmt)) do return

		// RHS should be a binary expression
		if len(assign.rhs) > 0 {
			binary, bok := assign.rhs[0].derived_expr.(^ast.Binary_Expr)
			testing.expect(
				t,
				bok,
				fmt.tprintf("RHS should be Binary_Expr, got %T", assign.rhs[0].derived_expr),
			)
		}
	}
}

// ========== MESSAGE statement tests ==========

@(test)
message_simple_text_test :: proc(t: ^testing.T) {
	// MESSAGE 'No display authorization.' TYPE 'I' DISPLAY LIKE 'E'.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `MESSAGE 'No display authorization.' TYPE 'I' DISPLAY LIKE 'E'.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) > 0, "Expected at least one declaration") do return

	msg_stmt, ok := file.decls[0].derived_stmt.(^ast.Message_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Message_Stmt, got %T", file.decls[0].derived_stmt)) do return

	// Check msg_expr is a string literal
	msg_lit, mok := msg_stmt.msg_expr.derived_expr.(^ast.Basic_Lit)
	if testing.expect(
		t,
		mok,
		fmt.tprintf("Expected msg_expr to be Basic_Lit, got %T", msg_stmt.msg_expr.derived_expr),
	) {
		testing.expect(
			t,
			msg_lit.tok.lit == "'No display authorization.'",
			fmt.tprintf("Expected message text, got '%s'", msg_lit.tok.lit),
		)
	}

	// Check TYPE
	if testing.expect(t, msg_stmt.msg_type != nil, "Expected msg_type to be set") {
		type_lit, tok := msg_stmt.msg_type.derived_expr.(^ast.Basic_Lit)
		if testing.expect(t, tok, "Expected msg_type to be Basic_Lit") {
			testing.expect(
				t,
				type_lit.tok.lit == "'I'",
				fmt.tprintf("Expected 'I', got '%s'", type_lit.tok.lit),
			)
		}
	}

	// Check DISPLAY LIKE
	if testing.expect(t, msg_stmt.display_like != nil, "Expected display_like to be set") {
		disp_lit, dok := msg_stmt.display_like.derived_expr.(^ast.Basic_Lit)
		if testing.expect(t, dok, "Expected display_like to be Basic_Lit") {
			testing.expect(
				t,
				disp_lit.tok.lit == "'E'",
				fmt.tprintf("Expected 'E', got '%s'", disp_lit.tok.lit),
			)
		}
	}
}

@(test)
message_with_class_and_with_into_test :: proc(t: ^testing.T) {
	// MESSAGE e899(/sttpec/int_msg) WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO lv_dummy_msg.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`MESSAGE e899(/sttpec/int_msg) WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO lv_dummy_msg.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) > 0, "Expected at least one declaration") do return

	msg_stmt, ok := file.decls[0].derived_stmt.(^ast.Message_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Message_Stmt, got %T", file.decls[0].derived_stmt)) do return

	// Check msg_expr is a call expression (message_id(class))
	call_expr, cok := msg_stmt.msg_expr.derived_expr.(^ast.Call_Expr)
	if testing.expect(
		t,
		cok,
		fmt.tprintf("Expected msg_expr to be Call_Expr, got %T", msg_stmt.msg_expr.derived_expr),
	) {
		// Check the message ID
		msg_ident, iok := call_expr.expr.derived_expr.(^ast.Ident)
		if testing.expect(t, iok, "Expected call expr to be Ident") {
			testing.expect(
				t,
				msg_ident.name == "e899",
				fmt.tprintf("Expected 'e899', got '%s'", msg_ident.name),
			)
		}
		// Check the class argument
		testing.expect(
			t,
			len(call_expr.args) == 1,
			fmt.tprintf("Expected 1 argument, got %d", len(call_expr.args)),
		)
	}

	// Check WITH arguments (4 args)
	testing.expect(
		t,
		len(msg_stmt.with_args) == 4,
		fmt.tprintf("Expected 4 WITH args, got %d", len(msg_stmt.with_args)),
	)

	// Check INTO target
	if testing.expect(t, msg_stmt.into_target != nil, "Expected into_target to be set") {
		into_ident, iok := msg_stmt.into_target.derived_expr.(^ast.Ident)
		if testing.expect(t, iok, "Expected into_target to be Ident") {
			testing.expect(
				t,
				into_ident.name == "lv_dummy_msg",
				fmt.tprintf("Expected 'lv_dummy_msg', got '%s'", into_ident.name),
			)
		}
	}
}

@(test)
message_info_with_class_test :: proc(t: ^testing.T) {
	// MESSAGE i899(/sttpec/int_msg) WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO lv_dummy_msg.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`MESSAGE i899(/sttpec/int_msg) WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4 INTO lv_dummy_msg.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) > 0, "Expected at least one declaration") do return

	msg_stmt, ok := file.decls[0].derived_stmt.(^ast.Message_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Message_Stmt, got %T", file.decls[0].derived_stmt)) do return

	// Check msg_expr is a call expression (message_id(class))
	call_expr, cok := msg_stmt.msg_expr.derived_expr.(^ast.Call_Expr)
	if testing.expect(
		t,
		cok,
		fmt.tprintf("Expected msg_expr to be Call_Expr, got %T", msg_stmt.msg_expr.derived_expr),
	) {
		// Check the message ID
		msg_ident, iok := call_expr.expr.derived_expr.(^ast.Ident)
		if testing.expect(t, iok, "Expected call expr to be Ident") {
			testing.expect(
				t,
				msg_ident.name == "i899",
				fmt.tprintf("Expected 'i899', got '%s'", msg_ident.name),
			)
		}
	}
}

@(test)
message_variable_with_type_display_test :: proc(t: ^testing.T) {
	// MESSAGE iv_msg TYPE 'I' DISPLAY LIKE 'E'.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `MESSAGE iv_msg TYPE 'I' DISPLAY LIKE 'E'.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) > 0, "Expected at least one declaration") do return

	msg_stmt, ok := file.decls[0].derived_stmt.(^ast.Message_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Message_Stmt, got %T", file.decls[0].derived_stmt)) do return

	// Check msg_expr is an identifier
	msg_ident, mok := msg_stmt.msg_expr.derived_expr.(^ast.Ident)
	if testing.expect(
		t,
		mok,
		fmt.tprintf("Expected msg_expr to be Ident, got %T", msg_stmt.msg_expr.derived_expr),
	) {
		testing.expect(
			t,
			msg_ident.name == "iv_msg",
			fmt.tprintf("Expected 'iv_msg', got '%s'", msg_ident.name),
		)
	}

	// Check TYPE
	testing.expect(t, msg_stmt.msg_type != nil, "Expected msg_type to be set")

	// Check DISPLAY LIKE
	testing.expect(t, msg_stmt.display_like != nil, "Expected display_like to be set")
}

@(test)
message_variable_type_only_test :: proc(t: ^testing.T) {
	// MESSAGE iv_msg TYPE 'I'.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `MESSAGE iv_msg TYPE 'I'.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) > 0, "Expected at least one declaration") do return

	msg_stmt, ok := file.decls[0].derived_stmt.(^ast.Message_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Message_Stmt, got %T", file.decls[0].derived_stmt)) do return

	// Check msg_expr is an identifier
	msg_ident, mok := msg_stmt.msg_expr.derived_expr.(^ast.Ident)
	if testing.expect(
		t,
		mok,
		fmt.tprintf("Expected msg_expr to be Ident, got %T", msg_stmt.msg_expr.derived_expr),
	) {
		testing.expect(
			t,
			msg_ident.name == "iv_msg",
			fmt.tprintf("Expected 'iv_msg', got '%s'", msg_ident.name),
		)
	}

	// Check TYPE
	testing.expect(t, msg_stmt.msg_type != nil, "Expected msg_type to be set")

	// Check DISPLAY LIKE is not set
	testing.expect(t, msg_stmt.display_like == nil, "Expected display_like to be nil")

	// Check no WITH args
	testing.expect(
		t,
		len(msg_stmt.with_args) == 0,
		fmt.tprintf("Expected 0 WITH args, got %d", len(msg_stmt.with_args)),
	)

	// Check INTO is not set
	testing.expect(t, msg_stmt.into_target == nil, "Expected into_target to be nil")
}

@(test)
message_simple_class_only_test :: proc(t: ^testing.T) {
	// MESSAGE e001(myclass).
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `MESSAGE e001(myclass).`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) > 0, "Expected at least one declaration") do return

	msg_stmt, ok := file.decls[0].derived_stmt.(^ast.Message_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Message_Stmt, got %T", file.decls[0].derived_stmt)) do return

	// Check msg_expr is a call expression (message_id(class))
	call_expr, cok := msg_stmt.msg_expr.derived_expr.(^ast.Call_Expr)
	if testing.expect(
		t,
		cok,
		fmt.tprintf("Expected msg_expr to be Call_Expr, got %T", msg_stmt.msg_expr.derived_expr),
	) {
		// Check the message ID
		msg_ident, iok := call_expr.expr.derived_expr.(^ast.Ident)
		if testing.expect(t, iok, "Expected call expr to be Ident") {
			testing.expect(
				t,
				msg_ident.name == "e001",
				fmt.tprintf("Expected 'e001', got '%s'", msg_ident.name),
			)
		}
		// Check the class argument
		if testing.expect(
			t,
			len(call_expr.args) == 1,
			fmt.tprintf("Expected 1 argument, got %d", len(call_expr.args)),
		) {
			class_ident, ciok := call_expr.args[0].derived_expr.(^ast.Ident)
			if testing.expect(t, ciok, "Expected class to be Ident") {
				testing.expect(
					t,
					class_ident.name == "myclass",
					fmt.tprintf("Expected 'myclass', got '%s'", class_ident.name),
				)
			}
		}
	}
}

// --- Complex Type Tests ---

@(test)
data_standard_table_of_test :: proc(t: ^testing.T) {
	// DATA: lt_idx_to_del TYPE STANDARD TABLE OF lvc_index.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA: lt_idx_to_del TYPE STANDARD TABLE OF lvc_index.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) > 0, "Expected at least one declaration") do return

	chain, ok := file.decls[0].derived_stmt.(^ast.Data_Typed_Chain_Decl)
	if !testing.expect(t, ok, fmt.tprintf("Expected Data_Typed_Chain_Decl, got %T", file.decls[0].derived_stmt)) do return

	if !testing.expect(t, len(chain.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(chain.decls))) do return

	decl := chain.decls[0]
	testing.expect(
		t,
		decl.ident.derived_expr.(^ast.Ident).name == "lt_idx_to_del",
		fmt.tprintf(
			"Expected 'lt_idx_to_del', got '%s'",
			decl.ident.derived_expr.(^ast.Ident).name,
		),
	)

	// Check that the type is a Table_Type
	table_type, tok := decl.typed.derived_expr.(^ast.Table_Type)
	if !testing.expect(t, tok, fmt.tprintf("Expected Table_Type, got %T", decl.typed.derived_expr)) do return

	testing.expect(
		t,
		table_type.kind == .Standard,
		fmt.tprintf("Expected Standard, got %v", table_type.kind),
	)

	// Check element type
	elem_ident, eok := table_type.elem.derived_expr.(^ast.Ident)
	if testing.expect(
		t,
		eok,
		fmt.tprintf("Expected Ident for elem, got %T", table_type.elem.derived_expr),
	) {
		testing.expect(
			t,
			elem_ident.name == "lvc_index",
			fmt.tprintf("Expected 'lvc_index', got '%s'", elem_ident.name),
		)
	}
}

@(test)
data_table_of_simple_test :: proc(t: ^testing.T) {
	// DATA: lt_data TYPE TABLE OF mytype.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA: lt_data TYPE TABLE OF mytype.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) > 0, "Expected at least one declaration") do return

	chain, ok := file.decls[0].derived_stmt.(^ast.Data_Typed_Chain_Decl)
	if !testing.expect(t, ok, fmt.tprintf("Expected Data_Typed_Chain_Decl, got %T", file.decls[0].derived_stmt)) do return

	decl := chain.decls[0]
	table_type, tok := decl.typed.derived_expr.(^ast.Table_Type)
	if !testing.expect(t, tok, fmt.tprintf("Expected Table_Type, got %T", decl.typed.derived_expr)) do return

	testing.expect(
		t,
		table_type.kind == .Any,
		fmt.tprintf("Expected Any, got %v", table_type.kind),
	)
}

@(test)
data_hashed_table_with_key_test :: proc(t: ^testing.T) {
	// DATA: lt_idx_to_del TYPE HASHED TABLE OF lvc_index WITH UNIQUE KEY col1.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA: lt_idx_to_del TYPE HASHED TABLE OF lvc_index WITH UNIQUE KEY col1.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) > 0, "Expected at least one declaration") do return

	chain, ok := file.decls[0].derived_stmt.(^ast.Data_Typed_Chain_Decl)
	if !testing.expect(t, ok, fmt.tprintf("Expected Data_Typed_Chain_Decl, got %T", file.decls[0].derived_stmt)) do return

	decl := chain.decls[0]
	table_type, tok := decl.typed.derived_expr.(^ast.Table_Type)
	if !testing.expect(t, tok, fmt.tprintf("Expected Table_Type, got %T", decl.typed.derived_expr)) do return

	testing.expect(
		t,
		table_type.kind == .Hashed,
		fmt.tprintf("Expected Hashed, got %v", table_type.kind),
	)

	// Check primary key
	if testing.expect(t, table_type.primary_key != nil, "Expected primary_key to be set") {
		testing.expect(t, table_type.primary_key.is_unique, "Expected key to be unique")
		if testing.expect(
			t,
			len(table_type.primary_key.components) == 1,
			fmt.tprintf(
				"Expected 1 key component, got %d",
				len(table_type.primary_key.components),
			),
		) {
			testing.expect(
				t,
				table_type.primary_key.components[0].name == "col1",
				fmt.tprintf(
					"Expected 'col1', got '%s'",
					table_type.primary_key.components[0].name,
				),
			)
		}
	}
}

@(test)
data_ref_to_test :: proc(t: ^testing.T) {
	// DATA lo_event TYPE REF TO lcl_event.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA lo_event TYPE REF TO lcl_event.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) > 0, "Expected at least one declaration") do return

	decl, ok := file.decls[0].derived_stmt.(^ast.Data_Typed_Decl)
	if !testing.expect(t, ok, fmt.tprintf("Expected Data_Typed_Decl, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		decl.ident.derived_expr.(^ast.Ident).name == "lo_event",
		fmt.tprintf("Expected 'lo_event', got '%s'", decl.ident.derived_expr.(^ast.Ident).name),
	)

	// Check that the type is a Ref_Type
	ref_type, rok := decl.typed.derived_expr.(^ast.Ref_Type)
	if !testing.expect(t, rok, fmt.tprintf("Expected Ref_Type, got %T", decl.typed.derived_expr)) do return

	// Check target type
	target_ident, tok := ref_type.target.derived_expr.(^ast.Ident)
	if testing.expect(
		t,
		tok,
		fmt.tprintf("Expected Ident for target, got %T", ref_type.target.derived_expr),
	) {
		testing.expect(
			t,
			target_ident.name == "lcl_event",
			fmt.tprintf("Expected 'lcl_event', got '%s'", target_ident.name),
		)
	}
}

@(test)
data_like_ref_to_test :: proc(t: ^testing.T) {
	// DATA lo_event LIKE REF TO lcl_event.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA lo_event LIKE REF TO lcl_event.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) > 0, "Expected at least one declaration") do return

	decl, ok := file.decls[0].derived_stmt.(^ast.Data_Typed_Decl)
	if !testing.expect(t, ok, fmt.tprintf("Expected Data_Typed_Decl, got %T", file.decls[0].derived_stmt)) do return

	// Check that the type is a Ref_Type
	ref_type, rok := decl.typed.derived_expr.(^ast.Ref_Type)
	if !testing.expect(t, rok, fmt.tprintf("Expected Ref_Type, got %T", decl.typed.derived_expr)) do return

	// Check target type
	target_ident, tok := ref_type.target.derived_expr.(^ast.Ident)
	if testing.expect(
		t,
		tok,
		fmt.tprintf("Expected Ident for target, got %T", ref_type.target.derived_expr),
	) {
		testing.expect(
			t,
			target_ident.name == "lcl_event",
			fmt.tprintf("Expected 'lcl_event', got '%s'", target_ident.name),
		)
	}
}

@(test)
data_line_of_test :: proc(t: ^testing.T) {
	// DATA lv_var1 TYPE LINE OF lt_idx_to_del.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA lv_var1 TYPE LINE OF lt_idx_to_del.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) > 0, "Expected at least one declaration") do return

	decl, ok := file.decls[0].derived_stmt.(^ast.Data_Typed_Decl)
	if !testing.expect(t, ok, fmt.tprintf("Expected Data_Typed_Decl, got %T", file.decls[0].derived_stmt)) do return

	// Check that the type is a Line_Type
	line_type, lok := decl.typed.derived_expr.(^ast.Line_Type)
	if !testing.expect(t, lok, fmt.tprintf("Expected Line_Type, got %T", decl.typed.derived_expr)) do return

	// Check table reference
	table_ident, tok := line_type.table.derived_expr.(^ast.Ident)
	if testing.expect(
		t,
		tok,
		fmt.tprintf("Expected Ident for table, got %T", line_type.table.derived_expr),
	) {
		testing.expect(
			t,
			table_ident.name == "lt_idx_to_del",
			fmt.tprintf("Expected 'lt_idx_to_del', got '%s'", table_ident.name),
		)
	}
}

@(test)
data_like_line_of_test :: proc(t: ^testing.T) {
	// DATA lv_var1 LIKE LINE OF lt_idx_to_del.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA lv_var1 LIKE LINE OF lt_idx_to_del.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) > 0, "Expected at least one declaration") do return

	decl, ok := file.decls[0].derived_stmt.(^ast.Data_Typed_Decl)
	if !testing.expect(t, ok, fmt.tprintf("Expected Data_Typed_Decl, got %T", file.decls[0].derived_stmt)) do return

	// Check that the type is a Line_Type
	line_type, lok := decl.typed.derived_expr.(^ast.Line_Type)
	testing.expect(t, lok, fmt.tprintf("Expected Line_Type, got %T", decl.typed.derived_expr))
}

@(test)
data_with_length_value_test :: proc(t: ^testing.T) {
	// DATA lv_val TYPE i LENGTH 4 VALUE 1.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA lv_val TYPE i LENGTH 4 VALUE 1.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) > 0, "Expected at least one declaration") do return

	decl, ok := file.decls[0].derived_stmt.(^ast.Data_Typed_Decl)
	if !testing.expect(t, ok, fmt.tprintf("Expected Data_Typed_Decl, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		decl.ident.derived_expr.(^ast.Ident).name == "lv_val",
		fmt.tprintf("Expected 'lv_val', got '%s'", decl.ident.derived_expr.(^ast.Ident).name),
	)

	// Check value is set
	testing.expect(t, decl.value != nil, "Expected value to be set")
}

@(test)
types_table_with_default_key_test :: proc(t: ^testing.T) {
	// TYPES: src_line TYPE c LENGTH 72,
	//        src TYPE STANDARD TABLE OF src_line WITH NON-UNIQUE DEFAULT KEY.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`TYPES: src_line TYPE c LENGTH 72,
           src TYPE STANDARD TABLE OF src_line
                WITH NON-UNIQUE DEFAULT KEY.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) > 0, "Expected at least one declaration") do return

	chain, ok := file.decls[0].derived_stmt.(^ast.Types_Chain_Decl)
	if !testing.expect(t, ok, fmt.tprintf("Expected Types_Chain_Decl, got %T", file.decls[0].derived_stmt)) do return

	if !testing.expect(t, len(chain.decls) == 2, fmt.tprintf("Expected 2 decls, got %d", len(chain.decls))) do return

	// Check second decl (the table type)
	decl := chain.decls[1]
	testing.expect(
		t,
		decl.ident.name == "src",
		fmt.tprintf("Expected 'src', got '%s'", decl.ident.name),
	)

	table_type, tok := decl.typed.derived_expr.(^ast.Table_Type)
	if !testing.expect(t, tok, fmt.tprintf("Expected Table_Type, got %T", decl.typed.derived_expr)) do return

	testing.expect(
		t,
		table_type.kind == .Standard,
		fmt.tprintf("Expected Standard, got %v", table_type.kind),
	)

	if testing.expect(t, table_type.primary_key != nil, "Expected primary_key to be set") {
		testing.expect(t, table_type.primary_key.is_default, "Expected default key")
		testing.expect(t, !table_type.primary_key.is_unique, "Expected non-unique key")
	}
}

@(test)
data_hashed_table_selector_type_test :: proc(t: ^testing.T) {
	// DATA: itab TYPE HASHED TABLE OF example_data=>struc WITH UNIQUE KEY idx.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA: itab TYPE HASHED TABLE OF example_data=>struc WITH UNIQUE KEY idx.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) > 0, "Expected at least one declaration") do return

	chain, ok := file.decls[0].derived_stmt.(^ast.Data_Typed_Chain_Decl)
	if !testing.expect(t, ok, fmt.tprintf("Expected Data_Typed_Chain_Decl, got %T", file.decls[0].derived_stmt)) do return

	decl := chain.decls[0]
	table_type, tok := decl.typed.derived_expr.(^ast.Table_Type)
	if !testing.expect(t, tok, fmt.tprintf("Expected Table_Type, got %T", decl.typed.derived_expr)) do return

	testing.expect(
		t,
		table_type.kind == .Hashed,
		fmt.tprintf("Expected Hashed, got %v", table_type.kind),
	)

	// Check element type is a selector expression
	sel_expr, sok := table_type.elem.derived_expr.(^ast.Selector_Expr)
	if testing.expect(
		t,
		sok,
		fmt.tprintf("Expected Selector_Expr for elem, got %T", table_type.elem.derived_expr),
	) {
		testing.expect(
			t,
			sel_expr.field.name == "struc",
			fmt.tprintf("Expected 'struc', got '%s'", sel_expr.field.name),
		)
	}
}

@(test)
data_sorted_table_test :: proc(t: ^testing.T) {
	// DATA: lt_sorted TYPE SORTED TABLE OF mytype WITH UNIQUE KEY name.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA: lt_sorted TYPE SORTED TABLE OF mytype WITH UNIQUE KEY name.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) > 0, "Expected at least one declaration") do return

	chain, ok := file.decls[0].derived_stmt.(^ast.Data_Typed_Chain_Decl)
	if !testing.expect(t, ok, fmt.tprintf("Expected Data_Typed_Chain_Decl, got %T", file.decls[0].derived_stmt)) do return

	decl := chain.decls[0]
	table_type, tok := decl.typed.derived_expr.(^ast.Table_Type)
	if !testing.expect(t, tok, fmt.tprintf("Expected Table_Type, got %T", decl.typed.derived_expr)) do return

	testing.expect(
		t,
		table_type.kind == .Sorted,
		fmt.tprintf("Expected Sorted, got %v", table_type.kind),
	)
}

// --- INSERT statement tests ---

@(test)
insert_value_into_table_test :: proc(t: ^testing.T) {
	// INSERT VALUE #( dispno = '001' descr = '001 - active' ) INTO TABLE mt_cdispos_map.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `INSERT VALUE #( dispno = '001' descr = '001 - active' ) INTO TABLE mt_cdispos_map.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	insert_stmt, ok := file.decls[0].derived_stmt.(^ast.Insert_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Insert_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		insert_stmt.kind == .Into_Table,
		fmt.tprintf("Expected Into_Table kind, got %v", insert_stmt.kind),
	)
	testing.expect(t, insert_stmt.value_expr != nil, "Expected value_expr to be set")
	testing.expect(t, insert_stmt.target != nil, "Expected target to be set")

	// Check target is identifier
	if target_ident, iok := insert_stmt.target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			target_ident.name == "mt_cdispos_map",
			fmt.tprintf("Expected 'mt_cdispos_map', got '%s'", target_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected target to be Ident, got %T", insert_stmt.target.derived_expr),
		)
	}
}

@(test)
insert_into_db_values_test :: proc(t: ^testing.T) {
	// INSERT INTO target VALUES wa.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `INSERT INTO ztable VALUES wa.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	insert_stmt, ok := file.decls[0].derived_stmt.(^ast.Insert_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Insert_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		insert_stmt.kind == .Into_Db,
		fmt.tprintf("Expected Into_Db kind, got %v", insert_stmt.kind),
	)
	testing.expect(t, insert_stmt.target != nil, "Expected target to be set")
	testing.expect(t, insert_stmt.source != nil, "Expected source to be set")

	// Check target is identifier
	if target_ident, iok := insert_stmt.target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			target_ident.name == "ztable",
			fmt.tprintf("Expected 'ztable', got '%s'", target_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected target to be Ident, got %T", insert_stmt.target.derived_expr),
		)
	}

	// Check source is identifier
	if source_ident, sok := insert_stmt.source.derived_expr.(^ast.Ident); sok {
		testing.expect(
			t,
			source_ident.name == "wa",
			fmt.tprintf("Expected 'wa', got '%s'", source_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected source to be Ident, got %T", insert_stmt.source.derived_expr),
		)
	}
}

@(test)
insert_from_wa_test :: proc(t: ^testing.T) {
	// INSERT target FROM wa.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `INSERT ztable FROM wa.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	insert_stmt, ok := file.decls[0].derived_stmt.(^ast.Insert_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Insert_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		insert_stmt.kind == .From_Wa,
		fmt.tprintf("Expected From_Wa kind, got %v", insert_stmt.kind),
	)
	testing.expect(t, insert_stmt.target != nil, "Expected target to be set")
	testing.expect(t, insert_stmt.source != nil, "Expected source to be set")

	// Check target is identifier
	if target_ident, iok := insert_stmt.target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			target_ident.name == "ztable",
			fmt.tprintf("Expected 'ztable', got '%s'", target_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected target to be Ident, got %T", insert_stmt.target.derived_expr),
		)
	}

	// Check source is identifier
	if source_ident, sok := insert_stmt.source.derived_expr.(^ast.Ident); sok {
		testing.expect(
			t,
			source_ident.name == "wa",
			fmt.tprintf("Expected 'wa', got '%s'", source_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected source to be Ident, got %T", insert_stmt.source.derived_expr),
		)
	}
}

@(test)
insert_from_table_test :: proc(t: ^testing.T) {
	// INSERT target FROM TABLE itab.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `INSERT ztable FROM TABLE itab.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	insert_stmt, ok := file.decls[0].derived_stmt.(^ast.Insert_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Insert_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		insert_stmt.kind == .From_Table,
		fmt.tprintf("Expected From_Table kind, got %v", insert_stmt.kind),
	)
	testing.expect(t, insert_stmt.target != nil, "Expected target to be set")
	testing.expect(t, insert_stmt.source != nil, "Expected source to be set")

	// Check target is identifier
	if target_ident, iok := insert_stmt.target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			target_ident.name == "ztable",
			fmt.tprintf("Expected 'ztable', got '%s'", target_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected target to be Ident, got %T", insert_stmt.target.derived_expr),
		)
	}

	// Check source is identifier
	if source_ident, sok := insert_stmt.source.derived_expr.(^ast.Ident); sok {
		testing.expect(
			t,
			source_ident.name == "itab",
			fmt.tprintf("Expected 'itab', got '%s'", source_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected source to be Ident, got %T", insert_stmt.source.derived_expr),
		)
	}
}

@(test)
insert_in_method_body_test :: proc(t: ^testing.T) {
	// Test INSERT statement inside a method body
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`CLASS zcl_test IMPLEMENTATION.
  METHOD test_insert.
    INSERT VALUE #( id = 1 ) INTO TABLE lt_data.
    INSERT ztable FROM ls_data.
  ENDMETHOD.
ENDCLASS.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	class_impl, ok := file.decls[0].derived_stmt.(^ast.Class_Impl_Decl)
	if !testing.expect(t, ok, fmt.tprintf("Expected Class_Impl_Decl, got %T", file.decls[0].derived_stmt)) do return

	if !testing.expect(t, len(class_impl.methods) == 1, fmt.tprintf("Expected 1 method, got %d", len(class_impl.methods))) do return

	method_impl, mok := class_impl.methods[0].derived_stmt.(^ast.Method_Impl)
	if !testing.expect(t, mok, fmt.tprintf("Expected Method_Impl, got %T", class_impl.methods[0].derived_stmt)) do return

	// Method body should have 2 INSERT statements
	if !testing.expect(t, len(method_impl.body) == 2, fmt.tprintf("Expected 2 body statements, got %d", len(method_impl.body))) do return

	// First INSERT
	insert1, i1ok := method_impl.body[0].derived_stmt.(^ast.Insert_Stmt)
	if testing.expect(
		t,
		i1ok,
		fmt.tprintf("Expected first Insert_Stmt, got %T", method_impl.body[0].derived_stmt),
	) {
		testing.expect(
			t,
			insert1.kind == .Into_Table,
			fmt.tprintf("Expected Into_Table, got %v", insert1.kind),
		)
	}

	// Second INSERT
	insert2, i2ok := method_impl.body[1].derived_stmt.(^ast.Insert_Stmt)
	if testing.expect(
		t,
		i2ok,
		fmt.tprintf("Expected second Insert_Stmt, got %T", method_impl.body[1].derived_stmt),
	) {
		testing.expect(
			t,
			insert2.kind == .From_Wa,
			fmt.tprintf("Expected From_Wa, got %v", insert2.kind),
		)
	}
}

// ============================================================================
// APPEND Statement Tests
// ============================================================================

@(test)
append_simple_test :: proc(t: ^testing.T) {
	// APPEND int TO itab.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `APPEND int TO itab.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	append_stmt, ok := file.decls[0].derived_stmt.(^ast.Append_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Append_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		append_stmt.kind == .Simple,
		fmt.tprintf("Expected Simple kind, got %v", append_stmt.kind),
	)
	testing.expect(t, append_stmt.source != nil, "Expected source to be set")
	testing.expect(t, append_stmt.target != nil, "Expected target to be set")

	// Check source is identifier
	if source_ident, sok := append_stmt.source.derived_expr.(^ast.Ident); sok {
		testing.expect(
			t,
			source_ident.name == "int",
			fmt.tprintf("Expected 'int', got '%s'", source_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected source to be Ident, got %T", append_stmt.source.derived_expr),
		)
	}

	// Check target is identifier
	if target_ident, iok := append_stmt.target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			target_ident.name == "itab",
			fmt.tprintf("Expected 'itab', got '%s'", target_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected target to be Ident, got %T", append_stmt.target.derived_expr),
		)
	}
}

@(test)
append_initial_line_test :: proc(t: ^testing.T) {
	// APPEND INITIAL LINE TO itab.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `APPEND INITIAL LINE TO itab.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	append_stmt, ok := file.decls[0].derived_stmt.(^ast.Append_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Append_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		append_stmt.kind == .Initial_Line,
		fmt.tprintf("Expected Initial_Line kind, got %v", append_stmt.kind),
	)
	testing.expect(t, append_stmt.source == nil, "Expected source to be nil for Initial_Line")
	testing.expect(t, append_stmt.target != nil, "Expected target to be set")

	// Check target is identifier
	if target_ident, iok := append_stmt.target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			target_ident.name == "itab",
			fmt.tprintf("Expected 'itab', got '%s'", target_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected target to be Ident, got %T", append_stmt.target.derived_expr),
		)
	}
}

@(test)
append_initial_line_assigning_test :: proc(t: ^testing.T) {
	// APPEND INITIAL LINE TO itab ASSIGNING <line>.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `APPEND INITIAL LINE TO itab ASSIGNING <line>.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	append_stmt, ok := file.decls[0].derived_stmt.(^ast.Append_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Append_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		append_stmt.kind == .Initial_Line,
		fmt.tprintf("Expected Initial_Line kind, got %v", append_stmt.kind),
	)
	testing.expect(t, append_stmt.target != nil, "Expected target to be set")
	testing.expect(t, append_stmt.assigning_target != nil, "Expected assigning_target to be set")

	// Check assigning target is field symbol
	if fs_ident, fok := append_stmt.assigning_target.derived_expr.(^ast.Ident); fok {
		testing.expect(
			t,
			fs_ident.name == "<line>",
			fmt.tprintf("Expected '<line>', got '%s'", fs_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf(
				"Expected assigning_target to be Ident, got %T",
				append_stmt.assigning_target.derived_expr,
			),
		)
	}
}

@(test)
append_lines_of_test :: proc(t: ^testing.T) {
	// APPEND LINES OF itab2 TO itab1.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `APPEND LINES OF itab2 TO itab1.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	append_stmt, ok := file.decls[0].derived_stmt.(^ast.Append_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Append_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		append_stmt.kind == .Lines_Of,
		fmt.tprintf("Expected Lines_Of kind, got %v", append_stmt.kind),
	)
	testing.expect(t, append_stmt.source != nil, "Expected source to be set")
	testing.expect(t, append_stmt.target != nil, "Expected target to be set")

	// Check source is identifier
	if source_ident, sok := append_stmt.source.derived_expr.(^ast.Ident); sok {
		testing.expect(
			t,
			source_ident.name == "itab2",
			fmt.tprintf("Expected 'itab2', got '%s'", source_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected source to be Ident, got %T", append_stmt.source.derived_expr),
		)
	}

	// Check target is identifier
	if target_ident, iok := append_stmt.target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			target_ident.name == "itab1",
			fmt.tprintf("Expected 'itab1', got '%s'", target_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected target to be Ident, got %T", append_stmt.target.derived_expr),
		)
	}
}

@(test)
append_to_field_symbol_selector_test :: proc(t: ^testing.T) {
	// APPEND lv_epc TO <fs_unpack_data>-children.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `APPEND lv_epc TO <fs_unpack_data>-children.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	append_stmt, ok := file.decls[0].derived_stmt.(^ast.Append_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Append_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		append_stmt.kind == .Simple,
		fmt.tprintf("Expected Simple kind, got %v", append_stmt.kind),
	)
	testing.expect(t, append_stmt.source != nil, "Expected source to be set")
	testing.expect(t, append_stmt.target != nil, "Expected target to be set")

	// Check source is identifier
	if source_ident, sok := append_stmt.source.derived_expr.(^ast.Ident); sok {
		testing.expect(
			t,
			source_ident.name == "lv_epc",
			fmt.tprintf("Expected 'lv_epc', got '%s'", source_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected source to be Ident, got %T", append_stmt.source.derived_expr),
		)
	}

	// Check target is a selector expression
	sel_expr, selok := append_stmt.target.derived_expr.(^ast.Selector_Expr)
	if !testing.expect(t, selok, fmt.tprintf("Expected target to be Selector_Expr, got %T", append_stmt.target.derived_expr)) do return

	// Check base is field symbol
	if base_ident, bok := sel_expr.expr.derived_expr.(^ast.Ident); bok {
		testing.expect(
			t,
			base_ident.name == "<fs_unpack_data>",
			fmt.tprintf("Expected '<fs_unpack_data>', got '%s'", base_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected base to be Ident, got %T", sel_expr.expr.derived_expr),
		)
	}

	// Check field name
	testing.expect(t, sel_expr.field != nil, "Expected field to be set")
	testing.expect(
		t,
		sel_expr.field.name == "children",
		fmt.tprintf("Expected 'children', got '%s'", sel_expr.field.name),
	)
}

@(test)
append_value_constructor_test :: proc(t: ^testing.T) {
	// APPEND VALUE #( code_urn = <fs_doc_info>-gs1_es ) TO lt_recv_objects.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `APPEND VALUE #( code_urn = <fs_doc_info>-gs1_es ) TO lt_recv_objects.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	append_stmt, ok := file.decls[0].derived_stmt.(^ast.Append_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Append_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		append_stmt.kind == .Simple,
		fmt.tprintf("Expected Simple kind, got %v", append_stmt.kind),
	)
	testing.expect(t, append_stmt.source != nil, "Expected source to be set")
	testing.expect(t, append_stmt.target != nil, "Expected target to be set")

	// Check source is a Constructor_Expr (VALUE #(...))
	constr_expr, cok := append_stmt.source.derived_expr.(^ast.Constructor_Expr)
	if !testing.expect(t, cok, fmt.tprintf("Expected source to be Constructor_Expr, got %T", append_stmt.source.derived_expr)) do return

	testing.expect(t, constr_expr.is_inferred, "Expected VALUE expression to be inferred (#)")
	testing.expect(
		t,
		len(constr_expr.args) == 1,
		fmt.tprintf("Expected 1 argument, got %d", len(constr_expr.args)),
	)

	// Check target is identifier
	if target_ident, iok := append_stmt.target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			target_ident.name == "lt_recv_objects",
			fmt.tprintf("Expected 'lt_recv_objects', got '%s'", target_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected target to be Ident, got %T", append_stmt.target.derived_expr),
		)
	}
}

@(test)
append_selector_expr_test :: proc(t: ^testing.T) {
	// APPEND <fs_doc_info>-gs1_es TO lt_upd_objs.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `APPEND <fs_doc_info>-gs1_es TO lt_upd_objs.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	append_stmt, ok := file.decls[0].derived_stmt.(^ast.Append_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Append_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		append_stmt.kind == .Simple,
		fmt.tprintf("Expected Simple kind, got %v", append_stmt.kind),
	)
	testing.expect(t, append_stmt.source != nil, "Expected source to be set")
	testing.expect(t, append_stmt.target != nil, "Expected target to be set")

	// Check source is a selector expression
	sel_expr, selok := append_stmt.source.derived_expr.(^ast.Selector_Expr)
	if !testing.expect(t, selok, fmt.tprintf("Expected source to be Selector_Expr, got %T", append_stmt.source.derived_expr)) do return

	// Check base is field symbol
	if base_ident, bok := sel_expr.expr.derived_expr.(^ast.Ident); bok {
		testing.expect(
			t,
			base_ident.name == "<fs_doc_info>",
			fmt.tprintf("Expected '<fs_doc_info>', got '%s'", base_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected base to be Ident, got %T", sel_expr.expr.derived_expr),
		)
	}

	// Check field name
	testing.expect(t, sel_expr.field != nil, "Expected field to be set")
	testing.expect(
		t,
		sel_expr.field.name == "gs1_es",
		fmt.tprintf("Expected 'gs1_es', got '%s'", sel_expr.field.name),
	)

	// Check target is identifier
	if target_ident, iok := append_stmt.target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			target_ident.name == "lt_upd_objs",
			fmt.tprintf("Expected 'lt_upd_objs', got '%s'", target_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected target to be Ident, got %T", append_stmt.target.derived_expr),
		)
	}
}

@(test)
append_value_nested_constructor_test :: proc(t: ^testing.T) {
	// APPEND VALUE #(
	//   parent   = ls_ser_par-gs1_es_parent
	//   pack_lvl = <fs_doc_info>-status_pack
	//   children = VALUE #( ( lv_epc ) )
	// ) TO lt_unpack_lvls.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`APPEND VALUE #(
                parent   = ls_ser_par-gs1_es_parent
                pack_lvl = <fs_doc_info>-status_pack
                children = VALUE #( ( lv_epc ) )
              ) TO lt_unpack_lvls.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	append_stmt, ok := file.decls[0].derived_stmt.(^ast.Append_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Append_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		append_stmt.kind == .Simple,
		fmt.tprintf("Expected Simple kind, got %v", append_stmt.kind),
	)
	testing.expect(t, append_stmt.source != nil, "Expected source to be set")
	testing.expect(t, append_stmt.target != nil, "Expected target to be set")

	// Check source is a Constructor_Expr (VALUE #(...))
	constr_expr, cok := append_stmt.source.derived_expr.(^ast.Constructor_Expr)
	if !testing.expect(t, cok, fmt.tprintf("Expected source to be Constructor_Expr, got %T", append_stmt.source.derived_expr)) do return

	testing.expect(t, constr_expr.is_inferred, "Expected VALUE expression to be inferred (#)")
	testing.expect(
		t,
		len(constr_expr.args) == 3,
		fmt.tprintf("Expected 3 arguments, got %d", len(constr_expr.args)),
	)

	// Verify the third argument is a nested VALUE constructor
	if len(constr_expr.args) >= 3 {
		third_arg, naok := constr_expr.args[2].derived_expr.(^ast.Named_Arg)
		if testing.expect(
			t,
			naok,
			fmt.tprintf(
				"Expected third arg to be Named_Arg, got %T",
				constr_expr.args[2].derived_expr,
			),
		) {
			testing.expect(
				t,
				third_arg.name.name == "children",
				fmt.tprintf("Expected 'children', got '%s'", third_arg.name.name),
			)
			// Check it's a nested VALUE constructor
			_, nested_cok := third_arg.value.derived_expr.(^ast.Constructor_Expr)
			testing.expect(
				t,
				nested_cok,
				fmt.tprintf(
					"Expected nested Constructor_Expr, got %T",
					third_arg.value.derived_expr,
				),
			)
		}
	}

	// Check target is identifier
	if target_ident, iok := append_stmt.target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			target_ident.name == "lt_unpack_lvls",
			fmt.tprintf("Expected 'lt_unpack_lvls', got '%s'", target_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected target to be Ident, got %T", append_stmt.target.derived_expr),
		)
	}
}

// ============================================================================
// FIELD-SYMBOLS Declaration Tests
// ============================================================================

@(test)
field_symbol_type_test :: proc(t: ^testing.T) {
	// FIELD-SYMBOLS <fs> TYPE string.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `FIELD-SYMBOLS <fs> TYPE string.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	fs_decl, ok := file.decls[0].derived_stmt.(^ast.Field_Symbol_Decl)
	if !testing.expect(t, ok, fmt.tprintf("Expected Field_Symbol_Decl, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(t, fs_decl.ident != nil, "Expected ident to be set")
	testing.expect(t, fs_decl.typed != nil, "Expected typed to be set")

	// Check field symbol name
	testing.expect(
		t,
		fs_decl.ident.name == "<fs>",
		fmt.tprintf("Expected '<fs>', got '%s'", fs_decl.ident.name),
	)

	// Check type is identifier
	if type_ident, iok := fs_decl.typed.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			type_ident.name == "string",
			fmt.tprintf("Expected 'string', got '%s'", type_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected typed to be Ident, got %T", fs_decl.typed.derived_expr),
		)
	}
}

@(test)
field_symbol_like_line_of_test :: proc(t: ^testing.T) {
	// FIELD-SYMBOLS <line> LIKE LINE OF itab.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `FIELD-SYMBOLS <line> LIKE LINE OF itab.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	fs_decl, ok := file.decls[0].derived_stmt.(^ast.Field_Symbol_Decl)
	if !testing.expect(t, ok, fmt.tprintf("Expected Field_Symbol_Decl, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(t, fs_decl.ident != nil, "Expected ident to be set")
	testing.expect(t, fs_decl.typed != nil, "Expected typed to be set")

	// Check field symbol name
	testing.expect(
		t,
		fs_decl.ident.name == "<line>",
		fmt.tprintf("Expected '<line>', got '%s'", fs_decl.ident.name),
	)

	// Check type is Line_Type
	line_type, lok := fs_decl.typed.derived_expr.(^ast.Line_Type)
	if !testing.expect(t, lok, fmt.tprintf("Expected typed to be Line_Type, got %T", fs_decl.typed.derived_expr)) do return

	// Check table reference
	if table_ident, iok := line_type.table.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			table_ident.name == "itab",
			fmt.tprintf("Expected 'itab', got '%s'", table_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected table to be Ident, got %T", line_type.table.derived_expr),
		)
	}
}

@(test)
field_symbol_assignment_test :: proc(t: ^testing.T) {
	// <line>-carrid = 'LH'.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `<line>-carrid = 'LH'.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	assign_stmt, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Assign_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(t, len(assign_stmt.lhs) == 1, "Expected 1 lhs expression")
	testing.expect(t, len(assign_stmt.rhs) == 1, "Expected 1 rhs expression")

	// Check lhs is a selector expression
	sel_expr, sok := assign_stmt.lhs[0].derived_expr.(^ast.Selector_Expr)
	if !testing.expect(t, sok, fmt.tprintf("Expected lhs to be Selector_Expr, got %T", assign_stmt.lhs[0].derived_expr)) do return

	// Check base is field symbol
	if base_ident, bok := sel_expr.expr.derived_expr.(^ast.Ident); bok {
		testing.expect(
			t,
			base_ident.name == "<line>",
			fmt.tprintf("Expected '<line>', got '%s'", base_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected base to be Ident, got %T", sel_expr.expr.derived_expr),
		)
	}

	// Check field name
	testing.expect(t, sel_expr.field != nil, "Expected field to be set")
	testing.expect(
		t,
		sel_expr.field.name == "carrid",
		fmt.tprintf("Expected 'carrid', got '%s'", sel_expr.field.name),
	)
}

@(test)
field_symbol_with_append_test :: proc(t: ^testing.T) {
	// Combined test: DATA, FIELD-SYMBOLS, APPEND, assignment
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`DATA itab TYPE TABLE OF spfli.
FIELD-SYMBOLS <line> LIKE LINE OF itab.
APPEND INITIAL LINE TO itab ASSIGNING <line>.
<line>-carrid = 'LH'.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 4, fmt.tprintf("Expected 4 decls, got %d", len(file.decls))) do return

	// First: DATA declaration
	data_decl, dok := file.decls[0].derived_stmt.(^ast.Data_Typed_Decl)
	if testing.expect(
		t,
		dok,
		fmt.tprintf("Expected Data_Typed_Decl, got %T", file.decls[0].derived_stmt),
	) {
		testing.expect(
			t,
			data_decl.ident.derived_expr.(^ast.Ident).name == "itab",
			fmt.tprintf(
				"Expected 'itab', got '%s'",
				data_decl.ident.derived_expr.(^ast.Ident).name,
			),
		)
	}

	// Second: FIELD-SYMBOLS declaration
	fs_decl, fok := file.decls[1].derived_stmt.(^ast.Field_Symbol_Decl)
	if testing.expect(
		t,
		fok,
		fmt.tprintf("Expected Field_Symbol_Decl, got %T", file.decls[1].derived_stmt),
	) {
		testing.expect(
			t,
			fs_decl.ident.name == "<line>",
			fmt.tprintf("Expected '<line>', got '%s'", fs_decl.ident.name),
		)
	}

	// Third: APPEND statement
	append_stmt, aok := file.decls[2].derived_stmt.(^ast.Append_Stmt)
	if testing.expect(
		t,
		aok,
		fmt.tprintf("Expected Append_Stmt, got %T", file.decls[2].derived_stmt),
	) {
		testing.expect(
			t,
			append_stmt.kind == .Initial_Line,
			fmt.tprintf("Expected Initial_Line, got %v", append_stmt.kind),
		)
		testing.expect(
			t,
			append_stmt.assigning_target != nil,
			"Expected assigning_target to be set",
		)
	}

	// Fourth: Assignment statement
	_, asok := file.decls[3].derived_stmt.(^ast.Assign_Stmt)
	testing.expect(
		t,
		asok,
		fmt.tprintf("Expected Assign_Stmt, got %T", file.decls[3].derived_stmt),
	)
}

// ============================================================================
// LOOP Statement Tests
// ============================================================================

@(test)
loop_at_screen_test :: proc(t: ^testing.T) {
	// LOOP AT SCREEN. ENDLOOP.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`LOOP AT SCREEN.
      IF screen-name = 'SCREEN0100-RESET'.
        screen-input = abap_false.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	loop_stmt, ok := file.decls[0].derived_stmt.(^ast.Loop_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Loop_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		loop_stmt.kind == .At_Screen,
		fmt.tprintf("Expected At_Screen kind, got %v", loop_stmt.kind),
	)
	testing.expect(t, len(loop_stmt.body) > 0, "Expected loop body to have statements")
}

@(test)
loop_at_into_inline_data_test :: proc(t: ^testing.T) {
	// LOOP AT lt_obj_del INTO DATA(lv_obj_del). ENDLOOP.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`LOOP AT lt_obj_del INTO DATA(lv_obj_del).
      log_info( |Deleted item { lv_obj_del }| ).
    ENDLOOP.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	loop_stmt, ok := file.decls[0].derived_stmt.(^ast.Loop_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Loop_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		loop_stmt.kind == .At,
		fmt.tprintf("Expected At kind, got %v", loop_stmt.kind),
	)
	testing.expect(t, loop_stmt.itab != nil, "Expected itab to be set")
	testing.expect(t, loop_stmt.into_target != nil, "Expected into_target to be set")

	// Check itab is identifier
	if itab_ident, iok := loop_stmt.itab.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			itab_ident.name == "lt_obj_del",
			fmt.tprintf("Expected 'lt_obj_del', got '%s'", itab_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected itab to be Ident, got %T", loop_stmt.itab.derived_expr),
		)
	}

	// Check into_target is identifier from inline DATA
	if into_ident, iok := loop_stmt.into_target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			into_ident.name == "lv_obj_del",
			fmt.tprintf("Expected 'lv_obj_del', got '%s'", into_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf(
				"Expected into_target to be Ident, got %T",
				loop_stmt.into_target.derived_expr,
			),
		)
	}
}

@(test)
loop_at_assigning_inline_field_symbol_test :: proc(t: ^testing.T) {
	// LOOP AT mt_object_info ASSIGNING FIELD-SYMBOL(<fs_obj_info>). ENDLOOP.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`LOOP AT mt_object_info ASSIGNING FIELD-SYMBOL(<fs_obj_info>).
      <fs_obj_info>-status = '@09@N/A'.
    ENDLOOP.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	loop_stmt, ok := file.decls[0].derived_stmt.(^ast.Loop_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Loop_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		loop_stmt.kind == .At,
		fmt.tprintf("Expected At kind, got %v", loop_stmt.kind),
	)
	testing.expect(t, loop_stmt.itab != nil, "Expected itab to be set")
	testing.expect(t, loop_stmt.assigning_target != nil, "Expected assigning_target to be set")

	// Check itab is identifier
	if itab_ident, iok := loop_stmt.itab.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			itab_ident.name == "mt_object_info",
			fmt.tprintf("Expected 'mt_object_info', got '%s'", itab_ident.name),
		)
	}

	// Check assigning_target is identifier (field symbol)
	if fs_ident, iok := loop_stmt.assigning_target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			fs_ident.name == "<fs_obj_info>",
			fmt.tprintf("Expected '<fs_obj_info>', got '%s'", fs_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf(
				"Expected assigning_target to be Ident, got %T",
				loop_stmt.assigning_target.derived_expr,
			),
		)
	}
}

@(test)
loop_at_into_variable_test :: proc(t: ^testing.T) {
	// LOOP AT ls_deliv_evt-objs INTO lv_epc. ENDLOOP.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`LOOP AT ls_deliv_evt-objs INTO lv_epc.
      IF sy-subrc = 0.
        <fs_obj_info>-status = '@03@Failed'.
      ENDIF.
    ENDLOOP.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	loop_stmt, ok := file.decls[0].derived_stmt.(^ast.Loop_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Loop_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		loop_stmt.kind == .At,
		fmt.tprintf("Expected At kind, got %v", loop_stmt.kind),
	)
	testing.expect(t, loop_stmt.itab != nil, "Expected itab to be set")
	testing.expect(t, loop_stmt.into_target != nil, "Expected into_target to be set")

	// Check itab is selector expression (ls_deliv_evt-objs)
	sel_expr, sok := loop_stmt.itab.derived_expr.(^ast.Selector_Expr)
	if !testing.expect(t, sok, fmt.tprintf("Expected itab to be Selector_Expr, got %T", loop_stmt.itab.derived_expr)) do return

	testing.expect(t, sel_expr.field != nil, "Expected field to be set")
	testing.expect(
		t,
		sel_expr.field.name == "objs",
		fmt.tprintf("Expected 'objs', got '%s'", sel_expr.field.name),
	)

	// Check into_target is identifier
	if into_ident, iok := loop_stmt.into_target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			into_ident.name == "lv_epc",
			fmt.tprintf("Expected 'lv_epc', got '%s'", into_ident.name),
		)
	}
}

@(test)
loop_at_transporting_no_fields_where_test :: proc(t: ^testing.T) {
	// LOOP AT buffer TRANSPORTING NO FIELDS WHERE table_line CS 'pattern'. ENDLOOP.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`LOOP AT buffer TRANSPORTING NO FIELDS WHERE table_line CS 'CLASS measure IMPLEMENTATION'.
      tabix = sy-tabix + 1.
      EXIT.
    ENDLOOP.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	loop_stmt, ok := file.decls[0].derived_stmt.(^ast.Loop_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Loop_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		loop_stmt.kind == .At,
		fmt.tprintf("Expected At kind, got %v", loop_stmt.kind),
	)
	testing.expect(
		t,
		loop_stmt.transporting_no_fields,
		"Expected transporting_no_fields to be true",
	)
	testing.expect(t, loop_stmt.where_cond != nil, "Expected where_cond to be set")

	// Check itab is identifier
	if itab_ident, iok := loop_stmt.itab.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			itab_ident.name == "buffer",
			fmt.tprintf("Expected 'buffer', got '%s'", itab_ident.name),
		)
	}
}

@(test)
loop_at_from_into_test :: proc(t: ^testing.T) {
	// LOOP AT buffer FROM tabix INTO line. ENDLOOP.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`LOOP AT buffer FROM tabix INTO line.
      IF line CS 'ENDCLASS.'.
        EXIT.
      ENDIF.
      APPEND line TO source.
    ENDLOOP.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	loop_stmt, ok := file.decls[0].derived_stmt.(^ast.Loop_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Loop_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		loop_stmt.kind == .At,
		fmt.tprintf("Expected At kind, got %v", loop_stmt.kind),
	)
	testing.expect(t, loop_stmt.itab != nil, "Expected itab to be set")
	testing.expect(t, loop_stmt.from_expr != nil, "Expected from_expr to be set")
	testing.expect(t, loop_stmt.into_target != nil, "Expected into_target to be set")

	// Check from_expr is identifier
	if from_ident, iok := loop_stmt.from_expr.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			from_ident.name == "tabix",
			fmt.tprintf("Expected 'tabix', got '%s'", from_ident.name),
		)
	}

	// Check into_target is identifier
	if into_ident, iok := loop_stmt.into_target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			into_ident.name == "line",
			fmt.tprintf("Expected 'line', got '%s'", into_ident.name),
		)
	}
}

@(test)
loop_at_group_by_test :: proc(t: ^testing.T) {
	// LOOP AT lt_mod_objs INTO ls_obj_info GROUP BY ( key = value ) ASSIGNING FIELD-SYMBOL(<fs>). ENDLOOP.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`LOOP AT lt_mod_objs INTO ls_obj_info
      GROUP BY ( locno   = ls_obj_info-locno
                 docnum  = ls_obj_info-docnum
                 doctpe  = ls_obj_info-doctpe
                 objtype = ls_obj_info-objtype
                 status_pack = ls_obj_info-status_pack )
      ASSIGNING FIELD-SYMBOL(<fs_doc_grp>).
    ENDLOOP.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	loop_stmt, ok := file.decls[0].derived_stmt.(^ast.Loop_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Loop_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		loop_stmt.kind == .At,
		fmt.tprintf("Expected At kind, got %v", loop_stmt.kind),
	)
	testing.expect(t, loop_stmt.itab != nil, "Expected itab to be set")
	testing.expect(t, loop_stmt.into_target != nil, "Expected into_target to be set")
	testing.expect(t, loop_stmt.group_by != nil, "Expected group_by to be set")
	testing.expect(t, loop_stmt.assigning_target != nil, "Expected assigning_target to be set")

	// Check group_by has components
	if loop_stmt.group_by != nil {
		testing.expect(
			t,
			len(loop_stmt.group_by.components) == 5,
			fmt.tprintf("Expected 5 group components, got %d", len(loop_stmt.group_by.components)),
		)
	}
}

@(test)
loop_at_group_test :: proc(t: ^testing.T) {
	// LOOP AT GROUP <fs_doc_grp> ASSIGNING FIELD-SYMBOL(<fs_doc_info>) WHERE condition. ENDLOOP.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`LOOP AT GROUP <fs_doc_grp> ASSIGNING FIELD-SYMBOL(<fs_doc_info>)
      WHERE status_pack > 0 AND objtype = '2'.
    ENDLOOP.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	loop_stmt, ok := file.decls[0].derived_stmt.(^ast.Loop_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Loop_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		loop_stmt.kind == .At_Group,
		fmt.tprintf("Expected At_Group kind, got %v", loop_stmt.kind),
	)
	testing.expect(t, loop_stmt.group_var != nil, "Expected group_var to be set")
	testing.expect(t, loop_stmt.assigning_target != nil, "Expected assigning_target to be set")
	testing.expect(t, loop_stmt.where_cond != nil, "Expected where_cond to be set")

	// Check group_var is identifier (field symbol)
	if grp_ident, iok := loop_stmt.group_var.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			grp_ident.name == "<fs_doc_grp>",
			fmt.tprintf("Expected '<fs_doc_grp>', got '%s'", grp_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf(
				"Expected group_var to be Ident, got %T",
				loop_stmt.group_var.derived_expr,
			),
		)
	}

	// Check assigning_target is identifier (field symbol)
	if fs_ident, iok := loop_stmt.assigning_target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			fs_ident.name == "<fs_doc_info>",
			fmt.tprintf("Expected '<fs_doc_info>', got '%s'", fs_ident.name),
		)
	}
}

@(test)
loop_nested_test :: proc(t: ^testing.T) {
	// Nested LOOP statements
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`LOOP AT outer_tab INTO wa_outer.
      LOOP AT inner_tab INTO wa_inner.
        process( wa_inner ).
      ENDLOOP.
    ENDLOOP.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	outer_loop, ok := file.decls[0].derived_stmt.(^ast.Loop_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Loop_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		len(outer_loop.body) == 1,
		fmt.tprintf("Expected 1 statement in outer loop body, got %d", len(outer_loop.body)),
	)

	// Check inner loop
	if len(outer_loop.body) == 1 {
		inner_loop, iok := outer_loop.body[0].derived_stmt.(^ast.Loop_Stmt)
		if testing.expect(
			t,
			iok,
			fmt.tprintf("Expected inner Loop_Stmt, got %T", outer_loop.body[0].derived_stmt),
		) {
			testing.expect(
				t,
				len(inner_loop.body) == 1,
				fmt.tprintf(
					"Expected 1 statement in inner loop body, got %d",
					len(inner_loop.body),
				),
			)
		}
	}
}

@(test)
append_initial_line_assigning_field_symbol_test :: proc(t: ^testing.T) {
	// APPEND INITIAL LINE TO gt_serdet_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `APPEND INITIAL LINE TO gt_serdet_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	append_stmt, ok := file.decls[0].derived_stmt.(^ast.Append_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Append_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		append_stmt.kind == .Initial_Line,
		fmt.tprintf("Expected Initial_Line kind, got %v", append_stmt.kind),
	)
	testing.expect(t, append_stmt.target != nil, "Expected target to be set")
	testing.expect(t, append_stmt.assigning_target != nil, "Expected assigning_target to be set")

	// Check assigning_target is identifier (field symbol)
	if fs_ident, iok := append_stmt.assigning_target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			fs_ident.name == "<fs_fcat>",
			fmt.tprintf("Expected '<fs_fcat>', got '%s'", fs_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf(
				"Expected assigning_target to be Ident, got %T",
				append_stmt.assigning_target.derived_expr,
			),
		)
	}
}

@(test)
authority_check_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`AUTHORITY-CHECK OBJECT '/SCWM/QDOC'
      ID 'ACTVT' FIELD '02'.

AUTHORITY-CHECK OBJECT 'S_CARRID'
    ID 'CARRID' FIELD carr
    ID 'ACTVT'  FIELD '03'.`


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

	if len(file.decls) == 2 {
		// First Statement
		stmt1 := ast.new(ast.Authority_Check_Stmt, {})
		stmt1.object = lit("'/SCWM/QDOC'")
		stmt1.ids = make([dynamic]ast.Authority_Check_Id)
		append(&stmt1.ids, ast.Authority_Check_Id{id = lit("'ACTVT'"), field = lit("'02'")})
		stmt1.derived_stmt = stmt1
		check_stmt(t, stmt1, file.decls[0])

		// Second Statement
		stmt2 := ast.new(ast.Authority_Check_Stmt, {})
		stmt2.object = lit("'S_CARRID'")
		stmt2.ids = make([dynamic]ast.Authority_Check_Id)
		append(&stmt2.ids, ast.Authority_Check_Id{id = lit("'CARRID'"), field = ident("carr")})
		append(&stmt2.ids, ast.Authority_Check_Id{id = lit("'ACTVT'"), field = lit("'03'")})
		stmt2.derived_stmt = stmt2
		check_stmt(t, stmt2, file.decls[1])
	}
}

// ============================================================================
// READ TABLE Statement Tests
// ============================================================================

@(test)
read_table_with_key_assigning_test :: proc(t: ^testing.T) {
	// READ TABLE mt_object_info WITH KEY gs1_es = lv_epc ASSIGNING <fs_obj_info>.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `READ TABLE mt_object_info WITH KEY gs1_es = lv_epc ASSIGNING <fs_obj_info>.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	read_stmt, ok := file.decls[0].derived_stmt.(^ast.Read_Table_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Read_Table_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		read_stmt.kind == .With_Key,
		fmt.tprintf("Expected With_Key kind, got %v", read_stmt.kind),
	)
	testing.expect(t, read_stmt.itab != nil, "Expected itab to be set")
	testing.expect(t, read_stmt.key != nil, "Expected key to be set")
	testing.expect(t, read_stmt.assigning_target != nil, "Expected assigning_target to be set")

	// Check itab is identifier
	if itab_ident, iok := read_stmt.itab.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			itab_ident.name == "mt_object_info",
			fmt.tprintf("Expected 'mt_object_info', got '%s'", itab_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected itab to be Ident, got %T", read_stmt.itab.derived_expr),
		)
	}

	// Check key has one component
	testing.expect(
		t,
		len(read_stmt.key.components) == 1,
		fmt.tprintf("Expected 1 key component, got %d", len(read_stmt.key.components)),
	)

	// Check key component
	if len(read_stmt.key.components) >= 1 {
		comp := read_stmt.key.components[0]
		testing.expect(
			t,
			comp.name.name == "gs1_es",
			fmt.tprintf("Expected key name 'gs1_es', got '%s'", comp.name.name),
		)
	}

	// Check assigning_target is identifier (field symbol)
	if fs_ident, iok := read_stmt.assigning_target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			fs_ident.name == "<fs_obj_info>",
			fmt.tprintf("Expected '<fs_obj_info>', got '%s'", fs_ident.name),
		)
	}
}

@(test)
read_table_with_key_into_inline_data_test :: proc(t: ^testing.T) {
	// READ TABLE lt_obj_hier_upd WITH KEY gs1_es = lv_epc INTO DATA(ls_ser_par).
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `READ TABLE lt_obj_hier_upd WITH KEY gs1_es = lv_epc INTO DATA(ls_ser_par).`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	read_stmt, ok := file.decls[0].derived_stmt.(^ast.Read_Table_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Read_Table_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		read_stmt.kind == .With_Key,
		fmt.tprintf("Expected With_Key kind, got %v", read_stmt.kind),
	)
	testing.expect(t, read_stmt.itab != nil, "Expected itab to be set")
	testing.expect(t, read_stmt.key != nil, "Expected key to be set")
	testing.expect(t, read_stmt.into_target != nil, "Expected into_target to be set")

	// Check into_target is identifier from inline DATA
	if into_ident, iok := read_stmt.into_target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			into_ident.name == "ls_ser_par",
			fmt.tprintf("Expected 'ls_ser_par', got '%s'", into_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf(
				"Expected into_target to be Ident, got %T",
				read_stmt.into_target.derived_expr,
			),
		)
	}
}

@(test)
read_table_with_key_inline_field_symbol_test :: proc(t: ^testing.T) {
	// READ TABLE lt_unpack_lvls WITH KEY parent = ls_ser_par-gs1_es_parent ASSIGNING FIELD-SYMBOL(<fs_unpack_data>).
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`READ TABLE lt_unpack_lvls WITH KEY parent = ls_ser_par-gs1_es_parent ASSIGNING FIELD-SYMBOL(<fs_unpack_data>).`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	read_stmt, ok := file.decls[0].derived_stmt.(^ast.Read_Table_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Read_Table_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		read_stmt.kind == .With_Key,
		fmt.tprintf("Expected With_Key kind, got %v", read_stmt.kind),
	)
	testing.expect(t, read_stmt.assigning_target != nil, "Expected assigning_target to be set")

	// Check assigning_target is identifier (field symbol)
	if fs_ident, iok := read_stmt.assigning_target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			fs_ident.name == "<fs_unpack_data>",
			fmt.tprintf("Expected '<fs_unpack_data>', got '%s'", fs_ident.name),
		)
	}
}

@(test)
read_table_transporting_no_fields_test :: proc(t: ^testing.T) {
	// READ TABLE <fs_unpack_data>-children WITH KEY table_line = lv_epc TRANSPORTING NO FIELDS.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`READ TABLE <fs_unpack_data>-children WITH KEY table_line = lv_epc TRANSPORTING NO FIELDS.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	read_stmt, ok := file.decls[0].derived_stmt.(^ast.Read_Table_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Read_Table_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		read_stmt.kind == .With_Key,
		fmt.tprintf("Expected With_Key kind, got %v", read_stmt.kind),
	)
	testing.expect(
		t,
		read_stmt.transporting_no_fields,
		"Expected transporting_no_fields to be true",
	)

	// Check itab is selector expression (<fs_unpack_data>-children)
	sel_expr, sok := read_stmt.itab.derived_expr.(^ast.Selector_Expr)
	if !testing.expect(t, sok, fmt.tprintf("Expected itab to be Selector_Expr, got %T", read_stmt.itab.derived_expr)) do return

	// Check base is field symbol
	if base_ident, bok := sel_expr.expr.derived_expr.(^ast.Ident); bok {
		testing.expect(
			t,
			base_ident.name == "<fs_unpack_data>",
			fmt.tprintf("Expected '<fs_unpack_data>', got '%s'", base_ident.name),
		)
	}

	// Check field name
	testing.expect(
		t,
		sel_expr.field.name == "children",
		fmt.tprintf("Expected 'children', got '%s'", sel_expr.field.name),
	)

	// Check key component
	if len(read_stmt.key.components) >= 1 {
		comp := read_stmt.key.components[0]
		testing.expect(
			t,
			comp.name.name == "table_line",
			fmt.tprintf("Expected key name 'table_line', got '%s'", comp.name.name),
		)
	}
}

@(test)
read_table_index_using_key_test :: proc(t: ^testing.T) {
	// READ TABLE itab INDEX idx USING KEY sort_key ASSIGNING FIELD-SYMBOL(<fs>).
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `READ TABLE itab INDEX idx USING KEY sort_key ASSIGNING FIELD-SYMBOL(<fs>).`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	read_stmt, ok := file.decls[0].derived_stmt.(^ast.Read_Table_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Read_Table_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		read_stmt.kind == .Index,
		fmt.tprintf("Expected Index kind, got %v", read_stmt.kind),
	)
	testing.expect(t, read_stmt.itab != nil, "Expected itab to be set")
	testing.expect(t, read_stmt.index_expr != nil, "Expected index_expr to be set")
	testing.expect(t, read_stmt.using_key != nil, "Expected using_key to be set")
	testing.expect(t, read_stmt.assigning_target != nil, "Expected assigning_target to be set")

	// Check itab is identifier
	if itab_ident, iok := read_stmt.itab.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			itab_ident.name == "itab",
			fmt.tprintf("Expected 'itab', got '%s'", itab_ident.name),
		)
	}

	// Check index_expr is identifier
	if idx_ident, iok := read_stmt.index_expr.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			idx_ident.name == "idx",
			fmt.tprintf("Expected 'idx', got '%s'", idx_ident.name),
		)
	}

	// Check using_key
	testing.expect(
		t,
		read_stmt.using_key.name == "sort_key",
		fmt.tprintf("Expected 'sort_key', got '%s'", read_stmt.using_key.name),
	)

	// Check assigning_target is identifier (field symbol)
	if fs_ident, iok := read_stmt.assigning_target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			fs_ident.name == "<fs>",
			fmt.tprintf("Expected '<fs>', got '%s'", fs_ident.name),
		)
	}
}

@(test)
read_table_multiple_key_components_test :: proc(t: ^testing.T) {
	// READ TABLE lt_data WITH KEY field1 = val1 field2 = val2 INTO wa.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `READ TABLE lt_data WITH KEY field1 = val1 field2 = val2 INTO wa.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	read_stmt, ok := file.decls[0].derived_stmt.(^ast.Read_Table_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Read_Table_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		read_stmt.kind == .With_Key,
		fmt.tprintf("Expected With_Key kind, got %v", read_stmt.kind),
	)
	testing.expect(t, read_stmt.key != nil, "Expected key to be set")
	testing.expect(t, read_stmt.into_target != nil, "Expected into_target to be set")

	// Check key has two components
	testing.expect(
		t,
		len(read_stmt.key.components) == 2,
		fmt.tprintf("Expected 2 key components, got %d", len(read_stmt.key.components)),
	)

	// Check first key component
	if len(read_stmt.key.components) >= 2 {
		comp1 := read_stmt.key.components[0]
		testing.expect(
			t,
			comp1.name.name == "field1",
			fmt.tprintf("Expected first key name 'field1', got '%s'", comp1.name.name),
		)

		comp2 := read_stmt.key.components[1]
		testing.expect(
			t,
			comp2.name.name == "field2",
			fmt.tprintf("Expected second key name 'field2', got '%s'", comp2.name.name),
		)
	}

	// Check into_target is identifier
	if into_ident, iok := read_stmt.into_target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			into_ident.name == "wa",
			fmt.tprintf("Expected 'wa', got '%s'", into_ident.name),
		)
	}
}

// ============================================================================
// NEW Expression with Named Arguments Tests
// ============================================================================

@(test)
new_expr_with_named_arg_test :: proc(t: ^testing.T) {
	// go_serdet_cont = NEW #( container_name = 'CCONTAINER_SER_DET' ).
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `go_serdet_cont = NEW #( container_name = 'CCONTAINER_SER_DET' ).`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	assign, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Assign_Stmt, got %T", file.decls[0].derived_stmt)) do return

	if !testing.expect(t, len(assign.rhs) > 0, "Expected RHS") do return

	new_expr, nok := assign.rhs[0].derived_expr.(^ast.New_Expr)
	if !testing.expect(t, nok, fmt.tprintf("Expected New_Expr, got %T", assign.rhs[0].derived_expr)) do return

	testing.expect(t, new_expr.is_inferred, "Expected inferred type (#)")
	testing.expect(
		t,
		len(new_expr.args) == 1,
		fmt.tprintf("Expected 1 argument, got %d", len(new_expr.args)),
	)

	// Check the argument is a named argument
	if len(new_expr.args) > 0 {
		named_arg, naok := new_expr.args[0].derived_expr.(^ast.Named_Arg)
		if testing.expect(
			t,
			naok,
			fmt.tprintf("Expected Named_Arg, got %T", new_expr.args[0].derived_expr),
		) {
			testing.expect(
				t,
				named_arg.name.name == "container_name",
				fmt.tprintf("Expected 'container_name', got '%s'", named_arg.name.name),
			)
			lit, lok := named_arg.value.derived_expr.(^ast.Basic_Lit)
			if testing.expect(t, lok, "Expected Basic_Lit value") {
				testing.expect(
					t,
					lit.tok.lit == "'CCONTAINER_SER_DET'",
					fmt.tprintf("Expected \"'CCONTAINER_SER_DET'\", got '%s'", lit.tok.lit),
				)
			}
		}
	}
}

@(test)
new_expr_with_ident_named_arg_test :: proc(t: ^testing.T) {
	// go_serdet_alv = NEW #( i_parent = go_serdet_cont ).
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `go_serdet_alv = NEW #( i_parent = go_serdet_cont ).`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	assign, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Assign_Stmt, got %T", file.decls[0].derived_stmt)) do return

	if !testing.expect(t, len(assign.rhs) > 0, "Expected RHS") do return

	new_expr, nok := assign.rhs[0].derived_expr.(^ast.New_Expr)
	if !testing.expect(t, nok, fmt.tprintf("Expected New_Expr, got %T", assign.rhs[0].derived_expr)) do return

	testing.expect(t, new_expr.is_inferred, "Expected inferred type (#)")
	testing.expect(
		t,
		len(new_expr.args) == 1,
		fmt.tprintf("Expected 1 argument, got %d", len(new_expr.args)),
	)

	// Check the argument is a named argument with identifier value
	if len(new_expr.args) > 0 {
		named_arg, naok := new_expr.args[0].derived_expr.(^ast.Named_Arg)
		if testing.expect(
			t,
			naok,
			fmt.tprintf("Expected Named_Arg, got %T", new_expr.args[0].derived_expr),
		) {
			testing.expect(
				t,
				named_arg.name.name == "i_parent",
				fmt.tprintf("Expected 'i_parent', got '%s'", named_arg.name.name),
			)
			ident_val, iok := named_arg.value.derived_expr.(^ast.Ident)
			if testing.expect(t, iok, "Expected Ident value") {
				testing.expect(
					t,
					ident_val.name == "go_serdet_cont",
					fmt.tprintf("Expected 'go_serdet_cont', got '%s'", ident_val.name),
				)
			}
		}
	}
}

// ============================================================================
// VALUE FOR WHERE Expression Tests
// ============================================================================

@(test)
value_for_where_test :: proc(t: ^testing.T) {
	// lt_rel_req = VALUE #(
	//         FOR ls_hier IN mt_obj_hier
	//         WHERE ( requested = abap_true AND gs1_es_root = ls_obj_hier-gs1_es_root )
	//         ( ls_hier )
	//       ).
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`lt_rel_req = VALUE #(
        FOR ls_hier IN mt_obj_hier
        WHERE ( requested = abap_true AND
                gs1_es_root = ls_obj_hier-gs1_es_root )
        ( ls_hier )
      ).`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	assign, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Assign_Stmt, got %T", file.decls[0].derived_stmt)) do return

	if !testing.expect(t, len(assign.rhs) > 0, "Expected RHS") do return

	constr_expr, cok := assign.rhs[0].derived_expr.(^ast.Constructor_Expr)
	if !testing.expect(t, cok, fmt.tprintf("Expected Constructor_Expr, got %T", assign.rhs[0].derived_expr)) do return

	testing.expect(t, constr_expr.is_inferred, "Expected inferred type (#)")
	testing.expect(
		t,
		len(constr_expr.args) >= 1,
		fmt.tprintf("Expected at least 1 argument, got %d", len(constr_expr.args)),
	)

	// Check the first argument is a FOR expression
	if len(constr_expr.args) > 0 {
		for_expr, fok := constr_expr.args[0].derived_expr.(^ast.For_Expr)
		if testing.expect(
			t,
			fok,
			fmt.tprintf("Expected For_Expr, got %T", constr_expr.args[0].derived_expr),
		) {
			// Check loop variable
			testing.expect(
				t,
				for_expr.var_name != nil && for_expr.var_name.name == "ls_hier",
				fmt.tprintf("Expected 'ls_hier', got '%v'", for_expr.var_name),
			)

			// Check itab
			testing.expect(t, for_expr.itab != nil, "Expected itab to be set")
			if for_expr.itab != nil {
				itab_ident, iok := for_expr.itab.derived_expr.(^ast.Ident)
				if testing.expect(t, iok, "Expected itab to be Ident") {
					testing.expect(
						t,
						itab_ident.name == "mt_obj_hier",
						fmt.tprintf("Expected 'mt_obj_hier', got '%s'", itab_ident.name),
					)
				}
			}

			// Check WHERE condition is set
			testing.expect(t, for_expr.where_cond != nil, "Expected where_cond to be set")

			// Check result expression
			testing.expect(t, for_expr.result_expr != nil, "Expected result_expr to be set")
			if for_expr.result_expr != nil {
				result_ident, rok := for_expr.result_expr.derived_expr.(^ast.Ident)
				if testing.expect(t, rok, "Expected result_expr to be Ident") {
					testing.expect(
						t,
						result_ident.name == "ls_hier",
						fmt.tprintf("Expected 'ls_hier', got '%s'", result_ident.name),
					)
				}
			}
		}
	}
}

@(test)
value_for_named_arg_result_test :: proc(t: ^testing.T) {
	// lt_unpack_chil = VALUE #( FOR ls_child IN ls_unpack_level-children ( code_urn = ls_child ) ).
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`lt_unpack_chil = VALUE #( FOR ls_child IN ls_unpack_level-children ( code_urn = ls_child ) ).`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	assign, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Assign_Stmt, got %T", file.decls[0].derived_stmt)) do return

	if !testing.expect(t, len(assign.rhs) > 0, "Expected RHS") do return

	constr_expr, cok := assign.rhs[0].derived_expr.(^ast.Constructor_Expr)
	if !testing.expect(t, cok, fmt.tprintf("Expected Constructor_Expr, got %T", assign.rhs[0].derived_expr)) do return

	testing.expect(t, constr_expr.is_inferred, "Expected inferred type (#)")
	testing.expect(
		t,
		len(constr_expr.args) >= 1,
		fmt.tprintf("Expected at least 1 argument, got %d", len(constr_expr.args)),
	)

	// Check the first argument is a FOR expression
	if len(constr_expr.args) > 0 {
		for_expr, fok := constr_expr.args[0].derived_expr.(^ast.For_Expr)
		if testing.expect(
			t,
			fok,
			fmt.tprintf("Expected For_Expr, got %T", constr_expr.args[0].derived_expr),
		) {
			// Check loop variable
			testing.expect(
				t,
				for_expr.var_name != nil && for_expr.var_name.name == "ls_child",
				fmt.tprintf("Expected 'ls_child', got '%v'", for_expr.var_name),
			)

			// Check itab - should be a selector expression (ls_unpack_level-children)
			testing.expect(t, for_expr.itab != nil, "Expected itab to be set")
			if for_expr.itab != nil {
				itab_sel, iok := for_expr.itab.derived_expr.(^ast.Selector_Expr)
				if testing.expect(
					t,
					iok,
					fmt.tprintf(
						"Expected itab to be Selector_Expr, got %T",
						for_expr.itab.derived_expr,
					),
				) {
					testing.expect(
						t,
						itab_sel.field != nil && itab_sel.field.name == "children",
						fmt.tprintf("Expected 'children', got '%v'", itab_sel.field),
					)
				}
			}

			// Check result_args - should have one Named_Arg (code_urn = ls_child)
			testing.expect(
				t,
				len(for_expr.result_args) >= 1,
				fmt.tprintf("Expected at least 1 result_arg, got %d", len(for_expr.result_args)),
			)
			if len(for_expr.result_args) > 0 {
				named_arg, nok := for_expr.result_args[0].derived_expr.(^ast.Named_Arg)
				if testing.expect(
					t,
					nok,
					fmt.tprintf(
						"Expected result_args[0] to be Named_Arg, got %T",
						for_expr.result_args[0].derived_expr,
					),
				) {
					testing.expect(
						t,
						named_arg.name != nil && named_arg.name.name == "code_urn",
						fmt.tprintf("Expected name 'code_urn', got '%v'", named_arg.name),
					)
					if named_arg.value != nil {
						val_ident, vok := named_arg.value.derived_expr.(^ast.Ident)
						if testing.expect(t, vok, "Expected value to be Ident") {
							testing.expect(
								t,
								val_ident.name == "ls_child",
								fmt.tprintf("Expected 'ls_child', got '%s'", val_ident.name),
							)
						}
					}
				}
			}
		}
	}
}

@(test)
value_for_multiple_named_args_test :: proc(t: ^testing.T) {
	// Test FOR with multiple named arguments in result: ( field1 = val1 field2 = val2 )
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`lt_result = VALUE #( FOR ls_row IN lt_data ( field1 = ls_row-a field2 = ls_row-b ) ).`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	assign, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Assign_Stmt, got %T", file.decls[0].derived_stmt)) do return

	if !testing.expect(t, len(assign.rhs) > 0, "Expected RHS") do return

	constr_expr, cok := assign.rhs[0].derived_expr.(^ast.Constructor_Expr)
	if !testing.expect(t, cok, fmt.tprintf("Expected Constructor_Expr, got %T", assign.rhs[0].derived_expr)) do return

	testing.expect(t, constr_expr.is_inferred, "Expected inferred type (#)")
	testing.expect(
		t,
		len(constr_expr.args) >= 1,
		fmt.tprintf("Expected at least 1 argument, got %d", len(constr_expr.args)),
	)

	// Check the first argument is a FOR expression
	if len(constr_expr.args) > 0 {
		for_expr, fok := constr_expr.args[0].derived_expr.(^ast.For_Expr)
		if testing.expect(
			t,
			fok,
			fmt.tprintf("Expected For_Expr, got %T", constr_expr.args[0].derived_expr),
		) {
			// Check loop variable
			testing.expect(
				t,
				for_expr.var_name != nil && for_expr.var_name.name == "ls_row",
				fmt.tprintf("Expected 'ls_row', got '%v'", for_expr.var_name),
			)

			// Check itab
			testing.expect(t, for_expr.itab != nil, "Expected itab to be set")

			// Check result_args - should have two Named_Args
			testing.expect(
				t,
				len(for_expr.result_args) == 2,
				fmt.tprintf("Expected 2 result_args, got %d", len(for_expr.result_args)),
			)

			if len(for_expr.result_args) >= 1 {
				named_arg1, nok1 := for_expr.result_args[0].derived_expr.(^ast.Named_Arg)
				if testing.expect(
					t,
					nok1,
					fmt.tprintf(
						"Expected result_args[0] to be Named_Arg, got %T",
						for_expr.result_args[0].derived_expr,
					),
				) {
					testing.expect(
						t,
						named_arg1.name != nil && named_arg1.name.name == "field1",
						fmt.tprintf("Expected name 'field1', got '%v'", named_arg1.name),
					)
				}
			}

			if len(for_expr.result_args) >= 2 {
				named_arg2, nok2 := for_expr.result_args[1].derived_expr.(^ast.Named_Arg)
				if testing.expect(
					t,
					nok2,
					fmt.tprintf(
						"Expected result_args[1] to be Named_Arg, got %T",
						for_expr.result_args[1].derived_expr,
					),
				) {
					testing.expect(
						t,
						named_arg2.name != nil && named_arg2.name.name == "field2",
						fmt.tprintf("Expected name 'field2', got '%v'", named_arg2.name),
					)
				}
			}
		}
	}
}

@(test)
delete_where_test :: proc(t: ^testing.T) {
	// DELETE mt_object_info WHERE gs1_es = lv_obj_del.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DELETE mt_object_info WHERE gs1_es = lv_obj_del.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	delete_stmt, ok := file.decls[0].derived_stmt.(^ast.Delete_Stmt)
	if !testing.expect(t, ok, fmt.tprintf("Expected Delete_Stmt, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(
		t,
		delete_stmt.kind == .Where,
		fmt.tprintf("Expected Where kind, got %v", delete_stmt.kind),
	)
	testing.expect(t, delete_stmt.target != nil, "Expected target to be set")
	testing.expect(t, delete_stmt.where_cond != nil, "Expected where_cond to be set")

	// Check target is identifier
	if target_ident, iok := delete_stmt.target.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			target_ident.name == "mt_object_info",
			fmt.tprintf("Expected 'mt_object_info', got '%s'", target_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf("Expected target to be Ident, got %T", delete_stmt.target.derived_expr),
		)
	}

	// Check where_cond is a binary expression (comparison)
	if binary, bok := delete_stmt.where_cond.derived_expr.(^ast.Binary_Expr); bok {
		// Check left side
		if left_ident, lok := binary.left.derived_expr.(^ast.Ident); lok {
			testing.expect(
				t,
				left_ident.name == "gs1_es",
				fmt.tprintf("Expected 'gs1_es', got '%s'", left_ident.name),
			)
		}
		// Check right side
		if right_ident, rok := binary.right.derived_expr.(^ast.Ident); rok {
			testing.expect(
				t,
				right_ident.name == "lv_obj_del",
				fmt.tprintf("Expected 'lv_obj_del', got '%s'", right_ident.name),
			)
		}
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf(
				"Expected where_cond to be Binary_Expr, got %T",
				delete_stmt.where_cond.derived_expr,
			),
		)
	}
}

@(test)
condense_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `CONDENSE text.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)
}

@(test)
call_function_simple_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = '/STTPEC/D_DOCTPE'
        text           = abap_true
        langu          = sy-langu
      TABLES
        dd07v_tab      = mt_dd_doctpe
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.`


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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	// Check it's a Call_Function_Stmt
	call_func, ok := file.decls[0].derived_stmt.(^ast.Call_Function_Stmt)
	testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Call_Function_Stmt, got %T", file.decls[0].derived_stmt),
	)

	if !ok {
		return
	}

	// Check function name
	testing.expect(t, call_func.func_name != nil, "Expected func_name to be set")
	if func_lit, fok := call_func.func_name.derived_expr.(^ast.Basic_Lit); fok {
		testing.expect(
			t,
			func_lit.tok.lit == "'DD_DOMVALUES_GET'",
			fmt.tprintf("Expected 'DD_DOMVALUES_GET', got '%s'", func_lit.tok.lit),
		)
	}

	// Check EXPORTING parameters
	testing.expect(
		t,
		len(call_func.exporting) == 3,
		fmt.tprintf("Expected 3 EXPORTING parameters, got %d", len(call_func.exporting)),
	)

	// Check TABLES parameters
	testing.expect(
		t,
		len(call_func.tables) == 1,
		fmt.tprintf("Expected 1 TABLES parameter, got %d", len(call_func.tables)),
	)

	// Check EXCEPTIONS parameters
	testing.expect(
		t,
		len(call_func.exceptions) == 2,
		fmt.tprintf("Expected 2 EXCEPTIONS parameters, got %d", len(call_func.exceptions)),
	)
}

@(test)
call_function_with_destination_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`CALL FUNCTION 'ZTT_DM_QUERY_HIERARCHY' DESTINATION mv_attp_dest
        EXPORTING
          iv_objcode       = ls_rel_req-gs1_es
          iv_retrieve_hier = abap_true
        IMPORTING
          ev_objtype       = lv_objtype
          es_lot_result    = ls_lot_result
          es_item_result   = ls_item_result
          es_cont_result   = ls_cont_result
          et_hierarchy     = lt_hierarchy_res.`


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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	// Check it's a Call_Function_Stmt
	call_func, ok := file.decls[0].derived_stmt.(^ast.Call_Function_Stmt)
	testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Call_Function_Stmt, got %T", file.decls[0].derived_stmt),
	)

	if !ok {
		return
	}

	// Check function name
	testing.expect(t, call_func.func_name != nil, "Expected func_name to be set")

	// Check destination
	testing.expect(t, call_func.destination != nil, "Expected destination to be set")
	if dest_ident, dok := call_func.destination.derived_expr.(^ast.Ident); dok {
		testing.expect(
			t,
			dest_ident.name == "mv_attp_dest",
			fmt.tprintf("Expected 'mv_attp_dest', got '%s'", dest_ident.name),
		)
	}

	// Check EXPORTING parameters
	testing.expect(
		t,
		len(call_func.exporting) == 2,
		fmt.tprintf("Expected 2 EXPORTING parameters, got %d", len(call_func.exporting)),
	)

	// Check IMPORTING parameters
	testing.expect(
		t,
		len(call_func.importing) == 5,
		fmt.tprintf("Expected 5 IMPORTING parameters, got %d", len(call_func.importing)),
	)
}

@(test)
call_function_with_changing_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`CALL FUNCTION 'ZTT_RFC_GET_OBJECT_DETLS' DESTINATION mv_attp_dest
      EXPORTING
        i_objcode   = lv_gs1_es
      IMPORTING
        es_trn_data = ls_trn_data
      CHANGING
        et_bapiret  = lt_return.`


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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	// Check it's a Call_Function_Stmt
	call_func, ok := file.decls[0].derived_stmt.(^ast.Call_Function_Stmt)
	testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Call_Function_Stmt, got %T", file.decls[0].derived_stmt),
	)

	if !ok {
		return
	}

	// Check EXPORTING parameters
	testing.expect(
		t,
		len(call_func.exporting) == 1,
		fmt.tprintf("Expected 1 EXPORTING parameter, got %d", len(call_func.exporting)),
	)

	// Check IMPORTING parameters
	testing.expect(
		t,
		len(call_func.importing) == 1,
		fmt.tprintf("Expected 1 IMPORTING parameter, got %d", len(call_func.importing)),
	)

	// Check CHANGING parameters
	testing.expect(
		t,
		len(call_func.changing) == 1,
		fmt.tprintf("Expected 1 CHANGING parameter, got %d", len(call_func.changing)),
	)

	// Check the changing parameter name
	if len(call_func.changing) > 0 {
		param := call_func.changing[0]
		testing.expect(t, param.name != nil, "Expected parameter name to be set")
		if param.name != nil {
			testing.expect(
				t,
				param.name.name == "et_bapiret",
				fmt.tprintf("Expected 'et_bapiret', got '%s'", param.name.name),
			)
		}
	}
}

@(test)
call_function_with_conv_expr_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`CALL FUNCTION 'ZTT_RFC_GET_OBJECT_DETLS' DESTINATION mv_attp_dest
      EXPORTING
        i_objcode   = CONV string( iv_gs1_es )
      IMPORTING
        es_trn_data = ls_trn_data.`


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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	// Check it's a Call_Function_Stmt
	call_func, ok := file.decls[0].derived_stmt.(^ast.Call_Function_Stmt)
	testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Call_Function_Stmt, got %T", file.decls[0].derived_stmt),
	)

	if !ok {
		return
	}

	// Check EXPORTING parameters has a CONV expression
	testing.expect(
		t,
		len(call_func.exporting) == 1,
		fmt.tprintf("Expected 1 EXPORTING parameter, got %d", len(call_func.exporting)),
	)

	if len(call_func.exporting) > 0 {
		param := call_func.exporting[0]
		testing.expect(t, param.value != nil, "Expected parameter value to be set")
		// Check if value is a Constructor_Expr (CONV)
		if param.value != nil {
			_, is_constructor := param.value.derived_expr.(^ast.Constructor_Expr)
			testing.expect(
				t,
				is_constructor,
				fmt.tprintf("Expected Constructor_Expr (CONV), got %T", param.value.derived_expr),
			)
		}
	}
}

// =====================================================
// SELECT Statement Tests
// =====================================================

@(test)
select_single_simple_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`SELECT SINGLE *
      FROM tvarvc
      INTO @DATA(ls_dummy_gln)
      WHERE name EQ 'ZTT_DUMMY_GLN'
      AND type EQ 'P'.`


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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	// Check it's a Select_Stmt
	select_stmt, ok := file.decls[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect(t, ok, fmt.tprintf("Expected Select_Stmt, got %T", file.decls[0].derived_stmt))

	if !ok {
		return
	}

	// Check SINGLE modifier
	testing.expect(t, select_stmt.is_single, "Expected is_single to be true")

	// Check field list (should have * for SELECT *)
	testing.expect(
		t,
		len(select_stmt.fields) == 1,
		fmt.tprintf("Expected 1 field (*), got %d", len(select_stmt.fields)),
	)

	// Check FROM table
	testing.expect(t, select_stmt.from_table != nil, "Expected from_table to be set")
	if from_ident, fok := select_stmt.from_table.derived_expr.(^ast.Ident); fok {
		testing.expect(
			t,
			from_ident.name == "tvarvc",
			fmt.tprintf("Expected 'tvarvc', got '%s'", from_ident.name),
		)
	}

	// Check INTO target
	testing.expect(t, select_stmt.into_target != nil, "Expected into_target to be set")
	testing.expect(t, select_stmt.into_kind == .Single, "Expected Single into_kind")

	// Check WHERE condition
	testing.expect(t, select_stmt.where_cond != nil, "Expected where_cond to be set")
}

@(test)
select_with_join_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`SELECT c~carrname, p~connid, p~cityfrom, p~cityto
       FROM scarr AS c
            INNER JOIN spfli AS p
                  ON c~carrid = p~carrid
       WHERE p~cityfrom = @cityfrom
       ORDER BY c~carrname, p~connid, p~cityfrom, p~cityto
       INTO TABLE @DATA(result1)
       UP TO 10 ROWS.`


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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	// Check it's a Select_Stmt
	select_stmt, ok := file.decls[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect(t, ok, fmt.tprintf("Expected Select_Stmt, got %T", file.decls[0].derived_stmt))

	if !ok {
		return
	}

	// Check not SINGLE
	testing.expect(t, !select_stmt.is_single, "Expected is_single to be false")

	// Check field list (should have 4 fields)
	testing.expect(
		t,
		len(select_stmt.fields) == 4,
		fmt.tprintf("Expected 4 fields, got %d", len(select_stmt.fields)),
	)

	// Check FROM table with alias
	testing.expect(t, select_stmt.from_table != nil, "Expected from_table to be set")
	if from_ident, fok := select_stmt.from_table.derived_expr.(^ast.Ident); fok {
		testing.expect(
			t,
			from_ident.name == "scarr",
			fmt.tprintf("Expected 'scarr', got '%s'", from_ident.name),
		)
	}
	testing.expect(t, select_stmt.from_alias != nil, "Expected from_alias to be set")
	if select_stmt.from_alias != nil {
		testing.expect(
			t,
			select_stmt.from_alias.name == "c",
			fmt.tprintf("Expected alias 'c', got '%s'", select_stmt.from_alias.name),
		)
	}

	// Check JOIN
	testing.expect(
		t,
		len(select_stmt.joins) == 1,
		fmt.tprintf("Expected 1 join, got %d", len(select_stmt.joins)),
	)
	if len(select_stmt.joins) > 0 {
		join := select_stmt.joins[0]
		testing.expect(t, join.kind == .Inner, "Expected Inner join")
		if join_table, jok := join.table.derived_expr.(^ast.Ident); jok {
			testing.expect(
				t,
				join_table.name == "spfli",
				fmt.tprintf("Expected 'spfli', got '%s'", join_table.name),
			)
		}
		testing.expect(t, join.alias != nil, "Expected join alias to be set")
		testing.expect(t, join.on_cond != nil, "Expected ON condition to be set")
	}

	// Check INTO TABLE
	testing.expect(t, select_stmt.into_target != nil, "Expected into_target to be set")
	testing.expect(t, select_stmt.into_kind == .Table, "Expected Table into_kind")

	// Check WHERE
	testing.expect(t, select_stmt.where_cond != nil, "Expected where_cond to be set")

	// Check ORDER BY
	testing.expect(
		t,
		len(select_stmt.order_by) == 4,
		fmt.tprintf("Expected 4 ORDER BY columns, got %d", len(select_stmt.order_by)),
	)

	// Check UP TO ROWS
	testing.expect(t, select_stmt.up_to_rows != nil, "Expected up_to_rows to be set")
}

@(test)
select_with_fields_clause_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`SELECT FROM scarr AS c
            INNER JOIN spfli AS p
                  ON c~carrid = p~carrid
       FIELDS c~carrname, p~connid, p~cityfrom, p~cityto
       WHERE p~cityfrom = @cityfrom
       ORDER BY c~carrname, p~connid, p~cityfrom, p~cityto
       INTO TABLE @DATA(result2)
       UP TO 10 ROWS.`


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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	// Check it's a Select_Stmt
	select_stmt, ok := file.decls[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect(t, ok, fmt.tprintf("Expected Select_Stmt, got %T", file.decls[0].derived_stmt))

	if !ok {
		return
	}

	// Check field list (should have 4 fields from FIELDS clause)
	testing.expect(
		t,
		len(select_stmt.fields) == 4,
		fmt.tprintf("Expected 4 fields, got %d", len(select_stmt.fields)),
	)
}

@(test)
select_with_aggregate_functions_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`SELECT FROM sflight
       FIELDS carrid,
              connid,
              SUM( seatsocc ) AS seatsocc
       WHERE carrid = 'LH'
       GROUP BY carrid, connid
       HAVING SUM( seatsocc ) > 1000
       ORDER BY carrid, connid
       INTO TABLE @DATA(result).`


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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	// Check it's a Select_Stmt
	select_stmt, ok := file.decls[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect(t, ok, fmt.tprintf("Expected Select_Stmt, got %T", file.decls[0].derived_stmt))

	if !ok {
		return
	}

	// Check field list (should have 3 fields including aggregate)
	testing.expect(
		t,
		len(select_stmt.fields) == 3,
		fmt.tprintf("Expected 3 fields, got %d", len(select_stmt.fields)),
	)

	// Check GROUP BY
	testing.expect(
		t,
		len(select_stmt.group_by) == 2,
		fmt.tprintf("Expected 2 GROUP BY columns, got %d", len(select_stmt.group_by)),
	)

	// Check HAVING
	testing.expect(t, select_stmt.having_cond != nil, "Expected having_cond to be set")
}

@(test)
select_for_all_entries_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`SELECT carrid, connid, fldate
         FROM sflight
         FOR ALL ENTRIES IN @entry_tab
         WHERE carrid = @entry_tab-carrid AND
               connid = @entry_tab-connid
         ORDER BY PRIMARY KEY
         INTO TABLE @DATA(result_tab).`


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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	// Check it's a Select_Stmt
	select_stmt, ok := file.decls[0].derived_stmt.(^ast.Select_Stmt)
	testing.expect(t, ok, fmt.tprintf("Expected Select_Stmt, got %T", file.decls[0].derived_stmt))

	if !ok {
		return
	}

	// Check field list
	testing.expect(
		t,
		len(select_stmt.fields) == 3,
		fmt.tprintf("Expected 3 fields, got %d", len(select_stmt.fields)),
	)

	// Check FOR ALL ENTRIES
	testing.expect(t, select_stmt.for_all_entries != nil, "Expected for_all_entries to be set")

	// Check WHERE
	testing.expect(t, select_stmt.where_cond != nil, "Expected where_cond to be set")

	// Check ORDER BY PRIMARY KEY
	testing.expect(
		t,
		len(select_stmt.order_by) == 1,
		fmt.tprintf("Expected 1 ORDER BY (PRIMARY KEY), got %d", len(select_stmt.order_by)),
	)
}

// --- CONSTANTS Parsing Tests ---

@(test)
constants_single_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `constants con_sflight type lvc_fname value 'ALV_T_T2'.`
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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	const_decl, ok := file.decls[0].derived_stmt.(^ast.Const_Decl)
	if !ok {
		// Print what we actually got
		#partial switch v in file.decls[0].derived_stmt {
		case ^ast.Data_Typed_Decl:
			testing.expect(t, false, "Got Data_Typed_Decl instead of Const_Decl")
		case ^ast.Assign_Stmt:
			testing.expect(t, false, "Got Assign_Stmt instead of Const_Decl")
		case ^ast.Expr_Stmt:
			testing.expect(t, false, "Got Expr_Stmt instead of Const_Decl")
		case ^ast.Bad_Decl:
			testing.expect(t, false, "Got Bad_Decl instead of Const_Decl")
		case:
			testing.expect(
				t,
				false,
				fmt.tprintf("Expected Const_Decl, got %T", file.decls[0].derived_stmt),
			)
		}
		return
	}

	testing.expect(t, const_decl.ident != nil, "Expected ident")
	if const_decl.ident != nil {
		testing.expect(
			t,
			const_decl.ident.name == "con_sflight",
			fmt.tprintf("Expected 'con_sflight', got '%s'", const_decl.ident.name),
		)
	}

	testing.expect(t, const_decl.value != nil, "Expected value")
}

@(test)
constants_chain_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`CONSTANTS: lc_one_fetch_size     TYPE i VALUE 50,
               lc_total_records_size TYPE i VALUE 1000,
               lc_date_initial       TYPE d VALUE '00000000'.`


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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	chain_decl, ok := file.decls[0].derived_stmt.(^ast.Const_Chain_Decl)
	testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Const_Chain_Decl, got %T", file.decls[0].derived_stmt),
	)

	if !ok {
		return
	}

	testing.expect(
		t,
		len(chain_decl.decls) == 3,
		fmt.tprintf("Expected 3 decls in chain, got %d", len(chain_decl.decls)),
	)

	if len(chain_decl.decls) >= 3 {
		testing.expect(
			t,
			chain_decl.decls[0].ident.name == "lc_one_fetch_size",
			"First const name mismatch",
		)
		testing.expect(
			t,
			chain_decl.decls[1].ident.name == "lc_total_records_size",
			"Second const name mismatch",
		)
		testing.expect(
			t,
			chain_decl.decls[2].ident.name == "lc_date_initial",
			"Third const name mismatch",
		)
	}
}

@(test)
constants_struct_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`CONSTANTS: BEGIN OF c_sn_reset_ts,
             tab1 LIKE sy-ucomm VALUE 'SN_RESET_TS_FC1',
             tab2 LIKE sy-ucomm VALUE 'SN_RESET_TS_FC2',
           END OF c_sn_reset_ts.`


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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	struct_decl, ok := file.decls[0].derived_stmt.(^ast.Const_Struct_Decl)
	testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Const_Struct_Decl, got %T", file.decls[0].derived_stmt),
	)

	if !ok {
		return
	}

	testing.expect(t, struct_decl.ident != nil, "Expected ident")
	if struct_decl.ident != nil {
		testing.expect(
			t,
			struct_decl.ident.name == "c_sn_reset_ts",
			fmt.tprintf("Expected 'c_sn_reset_ts', got '%s'", struct_decl.ident.name),
		)
	}

	testing.expect(
		t,
		len(struct_decl.components) == 2,
		fmt.tprintf("Expected 2 components, got %d", len(struct_decl.components)),
	)
}

@(test)
constants_with_pragma_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `CONSTANTS: lc_date_initial TYPE d VALUE '00000000' ##NEEDED.`
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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	chain_decl, ok := file.decls[0].derived_stmt.(^ast.Const_Chain_Decl)
	testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Const_Chain_Decl, got %T", file.decls[0].derived_stmt),
	)

	if !ok {
		return
	}

	testing.expect(
		t,
		len(chain_decl.decls) == 1,
		fmt.tprintf("Expected 1 decl in chain, got %d", len(chain_decl.decls)),
	)

	// Check that the pragma was lexed as a comment and didn't break parsing
	testing.expect(
		t,
		len(file.comments) >= 1,
		fmt.tprintf("Expected at least 1 comment (pragma), got %d", len(file.comments)),
	)
}

// ============================================================================
// CONTROLS Declaration Tests
// ============================================================================

@(test)
controls_tableview_test :: proc(t: ^testing.T) {
	// CONTROLS contrl TYPE TABLEVIEW USING SCREEN dynnr.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `CONTROLS tc_main TYPE TABLEVIEW USING SCREEN 100.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	controls_decl, ok := file.decls[0].derived_stmt.(^ast.Controls_Decl)
	if !testing.expect(t, ok, fmt.tprintf("Expected Controls_Decl, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(t, controls_decl.ident != nil, "Expected ident to be set")
	testing.expect(
		t,
		controls_decl.ident.name == "tc_main",
		fmt.tprintf("Expected 'tc_main', got '%s'", controls_decl.ident.name),
	)

	testing.expect(
		t,
		controls_decl.kind == .Tableview,
		fmt.tprintf("Expected Tableview, got %v", controls_decl.kind),
	)

	testing.expect(t, controls_decl.screen_dynnr != nil, "Expected screen_dynnr to be set")
}

@(test)
controls_tabstrip_test :: proc(t: ^testing.T) {
	// CONTROLS contrl TYPE TABSTRIP.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `CONTROLS tab_main TYPE TABSTRIP.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	controls_decl, ok := file.decls[0].derived_stmt.(^ast.Controls_Decl)
	if !testing.expect(t, ok, fmt.tprintf("Expected Controls_Decl, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(t, controls_decl.ident != nil, "Expected ident to be set")
	testing.expect(
		t,
		controls_decl.ident.name == "tab_main",
		fmt.tprintf("Expected 'tab_main', got '%s'", controls_decl.ident.name),
	)

	testing.expect(
		t,
		controls_decl.kind == .Tabstrip,
		fmt.tprintf("Expected Tabstrip, got %v", controls_decl.kind),
	)

	testing.expect(
		t,
		controls_decl.screen_dynnr == nil,
		"Expected screen_dynnr to be nil for TABSTRIP",
	)
}

@(test)
controls_chain_test :: proc(t: ^testing.T) {
	// CONTROLS: name1 TYPE TABSTRIP.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `CONTROLS: sn_reset_ts TYPE TABSTRIP.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	chain_decl, ok := file.decls[0].derived_stmt.(^ast.Controls_Chain_Decl)
	if !testing.expect(t, ok, fmt.tprintf("Expected Controls_Chain_Decl, got %T", file.decls[0].derived_stmt)) do return

	if !testing.expect(t, len(chain_decl.decls) == 1, fmt.tprintf("Expected 1 decl in chain, got %d", len(chain_decl.decls))) do return

	controls_decl := chain_decl.decls[0]
	testing.expect(t, controls_decl.ident != nil, "Expected ident to be set")
	testing.expect(
		t,
		controls_decl.ident.name == "sn_reset_ts",
		fmt.tprintf("Expected 'sn_reset_ts', got '%s'", controls_decl.ident.name),
	)

	testing.expect(
		t,
		controls_decl.kind == .Tabstrip,
		fmt.tprintf("Expected Tabstrip, got %v", controls_decl.kind),
	)
}

@(test)
controls_chain_multiple_test :: proc(t: ^testing.T) {
	// CONTROLS: ts1 TYPE TABSTRIP, tc1 TYPE TABLEVIEW USING SCREEN 200.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `CONTROLS: ts1 TYPE TABSTRIP, tc1 TYPE TABLEVIEW USING SCREEN 200.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	chain_decl, ok := file.decls[0].derived_stmt.(^ast.Controls_Chain_Decl)
	if !testing.expect(t, ok, fmt.tprintf("Expected Controls_Chain_Decl, got %T", file.decls[0].derived_stmt)) do return

	if !testing.expect(t, len(chain_decl.decls) == 2, fmt.tprintf("Expected 2 decls in chain, got %d", len(chain_decl.decls))) do return

	// Check first decl (TABSTRIP)
	decl1 := chain_decl.decls[0]
	testing.expect(
		t,
		decl1.ident.name == "ts1",
		fmt.tprintf("Expected 'ts1', got '%s'", decl1.ident.name),
	)
	testing.expect(
		t,
		decl1.kind == .Tabstrip,
		fmt.tprintf("Expected Tabstrip for first decl, got %v", decl1.kind),
	)

	// Check second decl (TABLEVIEW)
	decl2 := chain_decl.decls[1]
	testing.expect(
		t,
		decl2.ident.name == "tc1",
		fmt.tprintf("Expected 'tc1', got '%s'", decl2.ident.name),
	)
	testing.expect(
		t,
		decl2.kind == .Tableview,
		fmt.tprintf("Expected Tableview for second decl, got %v", decl2.kind),
	)
	testing.expect(t, decl2.screen_dynnr != nil, "Expected screen_dynnr to be set for TABLEVIEW")
}

@(test)
controls_tableview_with_variable_screen_test :: proc(t: ^testing.T) {
	// CONTROLS contrl TYPE TABLEVIEW USING SCREEN dynnr.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `CONTROLS tc_orders TYPE TABLEVIEW USING SCREEN gv_dynnr.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) == 1, fmt.tprintf("Expected 1 decl, got %d", len(file.decls))) do return

	controls_decl, ok := file.decls[0].derived_stmt.(^ast.Controls_Decl)
	if !testing.expect(t, ok, fmt.tprintf("Expected Controls_Decl, got %T", file.decls[0].derived_stmt)) do return

	testing.expect(t, controls_decl.ident != nil, "Expected ident to be set")
	testing.expect(
		t,
		controls_decl.ident.name == "tc_orders",
		fmt.tprintf("Expected 'tc_orders', got '%s'", controls_decl.ident.name),
	)

	testing.expect(
		t,
		controls_decl.kind == .Tableview,
		fmt.tprintf("Expected Tableview, got %v", controls_decl.kind),
	)

	testing.expect(t, controls_decl.screen_dynnr != nil, "Expected screen_dynnr to be set")

	// Verify it's a variable (identifier), not a literal number
	if dynnr_ident, iok := controls_decl.screen_dynnr.derived_expr.(^ast.Ident); iok {
		testing.expect(
			t,
			dynnr_ident.name == "gv_dynnr",
			fmt.tprintf("Expected 'gv_dynnr', got '%s'", dynnr_ident.name),
		)
	} else {
		testing.expect(
			t,
			false,
			fmt.tprintf(
				"Expected screen_dynnr to be Ident, got %T",
				controls_decl.screen_dynnr.derived_expr,
			),
		)
	}
}

// ============================================================================
// DATA Structure Declaration Tests
// ============================================================================

@(test)
data_struct_simple_test :: proc(t: ^testing.T) {
	// DATA: BEGIN OF name, field1 TYPE type1, field2 TYPE type2, END OF name.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`DATA: BEGIN OF ls_data,
        field1 TYPE i,
        field2 TYPE string,
      END OF ls_data.`


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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	struct_decl, ok := file.decls[0].derived_stmt.(^ast.Data_Struct_Decl)
	testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Data_Struct_Decl, got %T", file.decls[0].derived_stmt),
	)

	if !ok {
		return
	}

	testing.expect(t, struct_decl.ident != nil, "Expected ident")
	if struct_decl.ident != nil {
		testing.expect(
			t,
			struct_decl.ident.name == "ls_data",
			fmt.tprintf("Expected 'ls_data', got '%s'", struct_decl.ident.name),
		)
	}

	testing.expect(
		t,
		len(struct_decl.components) == 2,
		fmt.tprintf("Expected 2 components, got %d", len(struct_decl.components)),
	)
}

@(test)
data_struct_with_like_test :: proc(t: ^testing.T) {
	// DATA: BEGIN OF name, field LIKE sy-field, END OF name.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`DATA: BEGIN OF g_sn_reset_ts,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZTTRP001_US_SN_RESET',
        pressed_tab LIKE sy-ucomm VALUE c_sn_reset_ts-tab1,
      END OF g_sn_reset_ts.`


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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	struct_decl, ok := file.decls[0].derived_stmt.(^ast.Data_Struct_Decl)
	testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Data_Struct_Decl, got %T", file.decls[0].derived_stmt),
	)

	if !ok {
		return
	}

	testing.expect(t, struct_decl.ident != nil, "Expected ident")
	if struct_decl.ident != nil {
		testing.expect(
			t,
			struct_decl.ident.name == "g_sn_reset_ts",
			fmt.tprintf("Expected 'g_sn_reset_ts', got '%s'", struct_decl.ident.name),
		)
	}

	testing.expect(
		t,
		len(struct_decl.components) == 3,
		fmt.tprintf("Expected 3 components, got %d", len(struct_decl.components)),
	)

	// Check first component
	if len(struct_decl.components) >= 1 {
		comp1, ok1 := struct_decl.components[0].derived_stmt.(^ast.Data_Typed_Decl)
		testing.expect(t, ok1, "Expected first component to be Data_Typed_Decl")
		if ok1 && comp1.ident != nil {
			ident_expr, ident_ok := comp1.ident.derived_expr.(^ast.Ident)
			testing.expect(t, ident_ok, "Expected variable name to be Ident")
			testing.expect(
				t,
				ident_expr.name == "subscreen",
				fmt.tprintf("Expected 'subscreen', got '%s'", ident_expr.name),
			)
		}
	}

	// Check second component has VALUE
	if len(struct_decl.components) >= 2 {
		comp2, ok2 := struct_decl.components[1].derived_stmt.(^ast.Data_Typed_Decl)
		testing.expect(t, ok2, "Expected second component to be Data_Typed_Decl")
		if ok2 {
			testing.expect(t, comp2.value != nil, "Expected second component to have VALUE")
		}
	}

	// Check third component has VALUE
	if len(struct_decl.components) >= 3 {
		comp3, ok3 := struct_decl.components[2].derived_stmt.(^ast.Data_Typed_Decl)
		testing.expect(t, ok3, "Expected third component to be Data_Typed_Decl")
		if ok3 {
			testing.expect(t, comp3.value != nil, "Expected third component to have VALUE")
		}
	}
}

@(test)
data_struct_nested_test :: proc(t: ^testing.T) {
	// DATA: BEGIN OF outer, BEGIN OF inner, f TYPE i, END OF inner, END OF outer.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`DATA: BEGIN OF ls_outer,
        BEGIN OF ls_inner,
          field1 TYPE i,
        END OF ls_inner,
        field2 TYPE string,
      END OF ls_outer.`


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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	struct_decl, ok := file.decls[0].derived_stmt.(^ast.Data_Struct_Decl)
	testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Data_Struct_Decl, got %T", file.decls[0].derived_stmt),
	)

	if !ok {
		return
	}

	testing.expect(t, struct_decl.ident != nil, "Expected ident")
	if struct_decl.ident != nil {
		testing.expect(
			t,
			struct_decl.ident.name == "ls_outer",
			fmt.tprintf("Expected 'ls_outer', got '%s'", struct_decl.ident.name),
		)
	}

	testing.expect(
		t,
		len(struct_decl.components) == 2,
		fmt.tprintf(
			"Expected 2 components (nested struct + field2), got %d",
			len(struct_decl.components),
		),
	)

	// Check first component is a nested struct
	if len(struct_decl.components) >= 1 {
		nested, ok_nested := struct_decl.components[0].derived_stmt.(^ast.Data_Struct_Decl)
		testing.expect(
			t,
			ok_nested,
			fmt.tprintf(
				"Expected first component to be Data_Struct_Decl, got %T",
				struct_decl.components[0].derived_stmt,
			),
		)
		if ok_nested && nested.ident != nil {
			testing.expect(
				t,
				nested.ident.name == "ls_inner",
				fmt.tprintf("Expected 'ls_inner', got '%s'", nested.ident.name),
			)
			testing.expect(
				t,
				len(nested.components) == 1,
				fmt.tprintf(
					"Expected 1 component in nested struct, got %d",
					len(nested.components),
				),
			)
		}
	}
}

@(test)
data_struct_with_type_test :: proc(t: ^testing.T) {
	// DATA: BEGIN OF name, field TYPE type, END OF name.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
	`DATA: BEGIN OF ls_result,
        matnr TYPE mara-matnr,
        werks TYPE marc-werks,
        lgort TYPE mard-lgort,
      END OF ls_result.`


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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	struct_decl, ok := file.decls[0].derived_stmt.(^ast.Data_Struct_Decl)
	testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Data_Struct_Decl, got %T", file.decls[0].derived_stmt),
	)

	if !ok {
		return
	}

	testing.expect(
		t,
		len(struct_decl.components) == 3,
		fmt.tprintf("Expected 3 components, got %d", len(struct_decl.components)),
	)
}

@(test)
value_constructor_with_nested_rows_test :: proc(t: ^testing.T) {
	// VALUE #( ( field = 'value1' ) ( field = 'value2' ) ) for internal table with row expressions
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src =
		`lt_data = VALUE #( 
        ( code_urn = '(01)00370461024038(21)0000000000002' )
        ( code_urn = '(01)00370461024038(21)0000000000003' )
      ).`
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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	// Get the assignment statement
	assign_stmt, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
	testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Assign_Stmt, got %T", file.decls[0].derived_stmt),
	)

	if !ok || len(assign_stmt.rhs) == 0 {
		return
	}

	// Check that the RHS is a Constructor_Expr (VALUE #(...))
	constr, cok := assign_stmt.rhs[0].derived_expr.(^ast.Constructor_Expr)
	testing.expect(
		t,
		cok,
		fmt.tprintf("Expected Constructor_Expr, got %T", assign_stmt.rhs[0].derived_expr),
	)

	if !cok {
		return
	}

	// Should have 2 row expressions as args
	testing.expect(
		t,
		len(constr.args) == 2,
		fmt.tprintf("Expected 2 row arguments, got %d", len(constr.args)),
	)

	if len(constr.args) < 2 {
		return
	}

	// Each arg should be a Value_Row_Expr
	row1, row1_ok := constr.args[0].derived_expr.(^ast.Value_Row_Expr)
	testing.expect(
		t,
		row1_ok,
		fmt.tprintf("Expected Value_Row_Expr for arg 0, got %T", constr.args[0].derived_expr),
	)

	row2, row2_ok := constr.args[1].derived_expr.(^ast.Value_Row_Expr)
	testing.expect(
		t,
		row2_ok,
		fmt.tprintf("Expected Value_Row_Expr for arg 1, got %T", constr.args[1].derived_expr),
	)

	// Each row should have 1 named argument
	if row1_ok {
		testing.expect(
			t,
			len(row1.args) == 1,
			fmt.tprintf("Expected 1 arg in row 1, got %d", len(row1.args)),
		)
	}

	if row2_ok {
		testing.expect(
			t,
			len(row2.args) == 1,
			fmt.tprintf("Expected 1 arg in row 2, got %d", len(row2.args)),
		)
	}
}

@(test)
table_expression_index_access_test :: proc(t: ^testing.T) {
	// Test: DATA(ls_exp) = it_exp[ lv_idx ].
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(ls_exp) = it_exp[ lv_idx ].`

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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	// Get the data inline declaration
	data_decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
	testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Data_Inline_Decl, got %T", file.decls[0].derived_stmt),
	)

	if !ok {
		return
	}

	testing.expect(
		t,
		data_decl.ident != nil && data_decl.ident.name == "ls_exp",
		fmt.tprintf("Expected ident 'ls_exp', got %v", data_decl.ident),
	)

	// Check that value is an Index_Expr
	index_expr, iok := data_decl.value.derived_expr.(^ast.Index_Expr)
	testing.expect(
		t,
		iok,
		fmt.tprintf("Expected Index_Expr, got %T", data_decl.value.derived_expr),
	)

	if !iok {
		return
	}

	// Check the table expression
	table_ident, tok := index_expr.expr.derived_expr.(^ast.Ident)
	testing.expect(
		t,
		tok && table_ident.name == "it_exp",
		fmt.tprintf("Expected table ident 'it_exp', got %v", index_expr.expr),
	)

	// Check the index expression
	idx_ident, idxok := index_expr.index.derived_expr.(^ast.Ident)
	testing.expect(
		t,
		idxok && idx_ident.name == "lv_idx",
		fmt.tprintf("Expected index ident 'lv_idx', got %v", index_expr.index),
	)
}

@(test)
table_expression_chained_access_test :: proc(t: ^testing.T) {
	// Test: DATA(lo_exp_evt) = ls_exp-evts[ lv_evt_idx ].
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `DATA(lo_exp_evt) = ls_exp-evts[ lv_evt_idx ].`

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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	// Get the data inline declaration
	data_decl, ok := file.decls[0].derived_stmt.(^ast.Data_Inline_Decl)
	testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Data_Inline_Decl, got %T", file.decls[0].derived_stmt),
	)

	if !ok {
		return
	}

	testing.expect(
		t,
		data_decl.ident != nil && data_decl.ident.name == "lo_exp_evt",
		fmt.tprintf("Expected ident 'lo_exp_evt', got %v", data_decl.ident),
	)

	// Check that value is an Index_Expr
	index_expr, iok := data_decl.value.derived_expr.(^ast.Index_Expr)
	testing.expect(
		t,
		iok,
		fmt.tprintf("Expected Index_Expr, got %T", data_decl.value.derived_expr),
	)

	if !iok {
		return
	}

	// Check the table expression - should be a Selector_Expr (ls_exp-evts)
	selector_expr, sok := index_expr.expr.derived_expr.(^ast.Selector_Expr)
	testing.expect(
		t,
		sok,
		fmt.tprintf("Expected Selector_Expr, got %T", index_expr.expr.derived_expr),
	)

	if sok {
		// Check the base expression (ls_exp)
		base_ident, bok := selector_expr.expr.derived_expr.(^ast.Ident)
		testing.expect(
			t,
			bok && base_ident.name == "ls_exp",
			fmt.tprintf("Expected base ident 'ls_exp', got %v", selector_expr.expr),
		)

		// Check the field (evts)
		testing.expect(
			t,
			selector_expr.field != nil && selector_expr.field.name == "evts",
			fmt.tprintf("Expected field 'evts', got %v", selector_expr.field),
		)
	}

	// Check the index expression
	idx_ident, idxok := index_expr.index.derived_expr.(^ast.Ident)
	testing.expect(
		t,
		idxok && idx_ident.name == "lv_evt_idx",
		fmt.tprintf("Expected index ident 'lv_evt_idx', got %v", index_expr.index),
	)
}

@(test)
table_expression_field_access_test :: proc(t: ^testing.T) {
	// Test: lv_value = it_data[ 1 ]-field.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `lv_value = it_data[ 1 ]-field.`

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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	// Get the assignment statement
	assign_stmt, ok := file.decls[0].derived_stmt.(^ast.Assign_Stmt)
	testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Assign_Stmt, got %T", file.decls[0].derived_stmt),
	)

	if !ok || len(assign_stmt.rhs) == 0 {
		return
	}

	// Check that RHS is a Selector_Expr (table expression followed by field access)
	selector_expr, sok := assign_stmt.rhs[0].derived_expr.(^ast.Selector_Expr)
	testing.expect(
		t,
		sok,
		fmt.tprintf("Expected Selector_Expr, got %T", assign_stmt.rhs[0].derived_expr),
	)

	if !sok {
		return
	}

	// Check the field (field)
	testing.expect(
		t,
		selector_expr.field != nil && selector_expr.field.name == "field",
		fmt.tprintf("Expected field 'field', got %v", selector_expr.field),
	)

	// The base expression should be an Index_Expr
	index_expr, iok := selector_expr.expr.derived_expr.(^ast.Index_Expr)
	testing.expect(
		t,
		iok,
		fmt.tprintf("Expected Index_Expr, got %T", selector_expr.expr.derived_expr),
	)

	if iok {
		// Check the table (it_data)
		table_ident, tok := index_expr.expr.derived_expr.(^ast.Ident)
		testing.expect(
			t,
			tok && table_ident.name == "it_data",
			fmt.tprintf("Expected table ident 'it_data', got %v", index_expr.expr),
		)

		// Check the index (1)
		idx_lit, litok := index_expr.index.derived_expr.(^ast.Basic_Lit)
		testing.expect(
			t,
			litok && idx_lit.tok.lit == "1",
			fmt.tprintf("Expected index '1', got %v", index_expr.index),
		)
	}
}

@(test)
types_table_of_ref_to_test :: proc(t: ^testing.T) {
	// TYPES: tt_events TYPE STANDARD TABLE OF REF TO lcl_event WITH DEFAULT KEY.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `TYPES: tt_events TYPE STANDARD TABLE OF REF TO lcl_event WITH DEFAULT KEY.`
	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if !testing.expect(t, len(file.decls) > 0, "Expected at least one declaration") do return

	// Should be a Types_Chain_Decl
	chain_decl, ok := file.decls[0].derived_stmt.(^ast.Types_Chain_Decl)
	testing.expect(t, ok, fmt.tprintf("Expected Types_Chain_Decl, got %T", file.decls[0].derived_stmt))
	if !ok do return

	if !testing.expect(t, len(chain_decl.decls) == 1, "Expected 1 type declaration in chain") do return

	types_decl := chain_decl.decls[0]
	testing.expect(
		t,
		types_decl.ident.name == "tt_events",
		fmt.tprintf("Expected ident 'tt_events', got %s", types_decl.ident.name),
	)

	// The typed field should be a Table_Type
	table_type, tok := types_decl.typed.derived_expr.(^ast.Table_Type)
	testing.expect(t, tok, fmt.tprintf("Expected Table_Type, got %T", types_decl.typed.derived_expr))
	if !tok do return

	testing.expect(
		t,
		table_type.kind == .Standard,
		fmt.tprintf("Expected Standard table kind, got %v", table_type.kind),
	)

	// The element type should be a Ref_Type
	ref_type, rok := table_type.elem.derived_expr.(^ast.Ref_Type)
	testing.expect(t, rok, fmt.tprintf("Expected Ref_Type as element, got %T", table_type.elem.derived_expr))
	if !rok do return

	// The target of REF TO should be lcl_event
	target_ident, iok := ref_type.target.derived_expr.(^ast.Ident)
	testing.expect(t, iok, fmt.tprintf("Expected Ident as REF TO target, got %T", ref_type.target.derived_expr))
	if iok {
		testing.expect(
			t,
			target_ident.name == "lcl_event",
			fmt.tprintf("Expected target 'lcl_event', got %s", target_ident.name),
		)
	}

	// Check that the table has a default key
	testing.expect(
		t,
		table_type.primary_key != nil,
		"Expected primary_key to be set",
	)
	if table_type.primary_key != nil {
		testing.expect(
			t,
			table_type.primary_key.is_default,
			"Expected primary key to be DEFAULT KEY",
		)
	}
}

// --- CHECK statement tests ---

@(test)
check_stmt_is_bound_test :: proc(t: ^testing.T) {
	// Test: CHECK io_event IS BOUND.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `CHECK io_event IS BOUND.`

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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	// Check that we got a Check_Stmt
	check_stmt, ok := file.decls[0].derived_stmt.(^ast.Check_Stmt)
	testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Check_Stmt, got %T", file.decls[0].derived_stmt),
	)

	if !ok {
		return
	}

	// Check that the condition is a Predicate_Expr (IS BOUND)
	pred_expr, pok := check_stmt.cond.derived_expr.(^ast.Predicate_Expr)
	testing.expect(
		t,
		pok,
		fmt.tprintf("Expected Predicate_Expr, got %T", check_stmt.cond.derived_expr),
	)

	if pok {
		testing.expect(
			t,
			pred_expr.predicate == .Bound,
			fmt.tprintf("Expected predicate Bound, got %v", pred_expr.predicate),
		)

		// Check the base expression (io_event)
		base_ident, bok := pred_expr.expr.derived_expr.(^ast.Ident)
		testing.expect(
			t,
			bok && base_ident.name == "io_event",
			fmt.tprintf("Expected ident 'io_event', got %v", pred_expr.expr),
		)
	}
}

@(test)
check_stmt_is_instance_of_test :: proc(t: ^testing.T) {
	// Test: CHECK io_event IS INSTANCE OF lcl_object_event.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `CHECK io_event IS INSTANCE OF lcl_object_event.`

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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	// Check that we got a Check_Stmt
	check_stmt, ok := file.decls[0].derived_stmt.(^ast.Check_Stmt)
	testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Check_Stmt, got %T", file.decls[0].derived_stmt),
	)

	if !ok {
		return
	}

	// Check that the condition is a Predicate_Expr (IS INSTANCE OF)
	pred_expr, pok := check_stmt.cond.derived_expr.(^ast.Predicate_Expr)
	testing.expect(
		t,
		pok,
		fmt.tprintf("Expected Predicate_Expr, got %T", check_stmt.cond.derived_expr),
	)

	if pok {
		testing.expect(
			t,
			pred_expr.predicate == .Instance_Of,
			fmt.tprintf("Expected predicate Instance_Of, got %v", pred_expr.predicate),
		)
	}
}

@(test)
check_stmt_equality_test :: proc(t: ^testing.T) {
	// Test: CHECK ms_context-docnum = lo_other->ms_context-docnum.
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `CHECK ms_context-docnum = lo_other->ms_context-docnum.`

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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	// Check that we got a Check_Stmt
	check_stmt, ok := file.decls[0].derived_stmt.(^ast.Check_Stmt)
	testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Check_Stmt, got %T", file.decls[0].derived_stmt),
	)

	if !ok {
		return
	}

	// Check that the condition is a Binary_Expr (equality comparison)
	binary_expr, bok := check_stmt.cond.derived_expr.(^ast.Binary_Expr)
	testing.expect(
		t,
		bok,
		fmt.tprintf("Expected Binary_Expr, got %T", check_stmt.cond.derived_expr),
	)

	if bok {
		// Check the operator is '='
		testing.expect(
			t,
			binary_expr.op.kind == .Eq,
			fmt.tprintf("Expected Eq operator, got %v", binary_expr.op.kind),
		)

		// LHS should be a selector expression (ms_context-docnum)
		lhs_selector, lsok := binary_expr.left.derived_expr.(^ast.Selector_Expr)
		testing.expect(
			t,
			lsok,
			fmt.tprintf("Expected Selector_Expr for LHS, got %T", binary_expr.left.derived_expr),
		)

		if lsok {
			testing.expect(
				t,
				lhs_selector.field.name == "docnum",
				fmt.tprintf("Expected field 'docnum', got %s", lhs_selector.field.name),
			)
		}

		// RHS should be a selector expression (lo_other->ms_context-docnum)
		rhs_selector, rsok := binary_expr.right.derived_expr.(^ast.Selector_Expr)
		testing.expect(
			t,
			rsok,
			fmt.tprintf("Expected Selector_Expr for RHS, got %T", binary_expr.right.derived_expr),
		)

		if rsok {
			testing.expect(
				t,
				rhs_selector.field.name == "docnum",
				fmt.tprintf("Expected field 'docnum', got %s", rhs_selector.field.name),
			)
		}
	}
}

@(test)
check_stmt_function_call_equality_test :: proc(t: ^testing.T) {
	// Test: CHECK lines( mt_objects ) = lines( lo_other->mt_objects ).
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
	file.src = `CHECK lines( mt_objects ) = lines( lo_other->mt_objects ).`

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
		fmt.tprintf("Expected 1 declaration, got %d", len(file.decls)),
	)

	if len(file.decls) < 1 {
		return
	}

	// Check that we got a Check_Stmt
	check_stmt, ok := file.decls[0].derived_stmt.(^ast.Check_Stmt)
	testing.expect(
		t,
		ok,
		fmt.tprintf("Expected Check_Stmt, got %T", file.decls[0].derived_stmt),
	)

	if !ok {
		return
	}

	// Check that the condition is a Binary_Expr (equality comparison)
	binary_expr, bok := check_stmt.cond.derived_expr.(^ast.Binary_Expr)
	testing.expect(
		t,
		bok,
		fmt.tprintf("Expected Binary_Expr, got %T", check_stmt.cond.derived_expr),
	)

	if bok {
		// Check the operator is '='
		testing.expect(
			t,
			binary_expr.op.kind == .Eq,
			fmt.tprintf("Expected Eq operator, got %v", binary_expr.op.kind),
		)

		// LHS should be a call expression (lines( mt_objects ))
		lhs_call, lcok := binary_expr.left.derived_expr.(^ast.Call_Expr)
		testing.expect(
			t,
			lcok,
			fmt.tprintf("Expected Call_Expr for LHS, got %T", binary_expr.left.derived_expr),
		)

		if lcok {
			lhs_ident, liok := lhs_call.expr.derived_expr.(^ast.Ident)
			testing.expect(
				t,
				liok && lhs_ident.name == "lines",
				fmt.tprintf("Expected function 'lines', got %v", lhs_call.expr),
			)
		}

		// RHS should also be a call expression (lines( lo_other->mt_objects ))
		rhs_call, rcok := binary_expr.right.derived_expr.(^ast.Call_Expr)
		testing.expect(
			t,
			rcok,
			fmt.tprintf("Expected Call_Expr for RHS, got %T", binary_expr.right.derived_expr),
		)

		if rcok {
			rhs_ident, riok := rhs_call.expr.derived_expr.(^ast.Ident)
			testing.expect(
				t,
				riok && rhs_ident.name == "lines",
				fmt.tprintf("Expected function 'lines', got %v", rhs_call.expr),
			)
		}
	}
}