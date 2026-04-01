package tests_symbols

import "../../src/lang/ast"
import "../../src/lang/parser"
import "../../src/lang/symbols"
import "core:fmt"
import "core:strings"
import "core:testing"

@(test)
test_inline_decl_symbol :: proc(t: ^testing.T) {
	src := "DATA(my_var) = 1."
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	if table == nil {
		testing.expect(t, false, "symbol table should not be nil")
		return
	}

	sym, ok := table.symbols["my_var"]

	if !ok {
		msg := fmt.tprintf("expected symbol 'my_var' to be found, got map: %v", table.symbols)
		testing.expect(t, false, msg)
		return
	}

	testing.expect(t, sym.kind == .Variable, fmt.tprintf("expected Variable, got %v", sym.kind))
	testing.expect(t, sym.type_info != nil, "expected type_info to be set")
	
	if sym.type_info != nil {
		testing.expect(
			t, 
			sym.type_info.kind == .Inferred, 
			fmt.tprintf("expected Inferred type for inline decl, got %v", sym.type_info.kind),
		)
		testing.expect(
			t, 
			sym.type_info.infer_source != nil, 
			"expected infer_source to reference the value expression",
		)
	}
}

@(test)
test_typed_decl_symbol :: proc(t: ^testing.T) {
	src := "DATA my_var TYPE i."
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	if table == nil {
		testing.expect(t, false, "symbol table should not be nil")
		return
	}

	sym, ok := table.symbols["my_var"]

	if !ok {
		msg := fmt.tprintf("expected symbol 'my_var' to be found, got map: %v", table.symbols)
		testing.expect(t, false, msg)
		return
	}

	testing.expect(t, sym.kind == .Variable, fmt.tprintf("expected Variable, got %v", sym.kind))
	testing.expect(t, !sym.is_chained, "expected is_chained to be false")
	testing.expect(t, sym.type_info != nil, "expected type_info to be set")
	
	if sym.type_info != nil {
		testing.expect(
			t, 
			sym.type_info.kind == .Integer, 
			fmt.tprintf("expected Integer type, got %v", sym.type_info.kind),
		)
	}
}

@(test)
test_typed_decl_string :: proc(t: ^testing.T) {
	src := "DATA my_str TYPE string."
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	if table == nil {
		testing.expect(t, false, "symbol table should not be nil")
		return
	}

	sym, ok := table.symbols["my_str"]

	if !ok {
		msg := fmt.tprintf("expected symbol 'my_str' to be found")
		testing.expect(t, false, msg)
		return
	}

	testing.expect(t, sym.type_info != nil, "expected type_info to be set")
	
	if sym.type_info != nil {
		testing.expect(
			t, 
			sym.type_info.kind == .String, 
			fmt.tprintf("expected String type, got %v", sym.type_info.kind),
		)
	}
}

@(test)
test_typed_decl_named_type :: proc(t: ^testing.T) {
	src := "DATA my_obj TYPE my_custom_type."
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	if table == nil {
		testing.expect(t, false, "symbol table should not be nil")
		return
	}

	sym, ok := table.symbols["my_obj"]

	if !ok {
		testing.expect(t, false, "expected symbol 'my_obj' to be found")
		return
	}

	testing.expect(t, sym.type_info != nil, "expected type_info to be set")
	
	if sym.type_info != nil {
		testing.expect(
			t, 
			sym.type_info.kind == .Named, 
			fmt.tprintf("expected Named type, got %v", sym.type_info.kind),
		)
		testing.expect(
			t, 
			sym.type_info.name == "my_custom_type", 
			fmt.tprintf("expected type name 'my_custom_type', got '%v'", sym.type_info.name),
		)
	}
}

@(test)
test_chain_decl_symbols :: proc(t: ^testing.T) {
	src := "DATA: var1 TYPE i, var2 TYPE string."
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	if table == nil {
		testing.expect(t, false, "symbol table should not be nil")
		return
	}

	// Check var1
	sym1, ok1 := table.symbols["var1"]
	if !ok1 {
		testing.expect(t, false, "expected symbol 'var1' to be found")
		return
	}
	
	testing.expect(t, sym1.is_chained, "expected var1 is_chained to be true")
	if sym1.type_info != nil {
		testing.expect(
			t, 
			sym1.type_info.kind == .Integer, 
			fmt.tprintf("expected var1 Integer type, got %v", sym1.type_info.kind),
		)
	}

	// Check var2
	sym2, ok2 := table.symbols["var2"]
	if !ok2 {
		testing.expect(t, false, "expected symbol 'var2' to be found")
		return
	}
	
	testing.expect(t, sym2.is_chained, "expected var2 is_chained to be true")
	if sym2.type_info != nil {
		testing.expect(
			t, 
			sym2.type_info.kind == .String, 
			fmt.tprintf("expected var2 String type, got %v", sym2.type_info.kind),
		)
	}
}

@(test)
test_fat_arrow_error_with_variable :: proc(t: ^testing.T) {
	// Using => with a variable (not a class) should produce an error
	src := `DATA my_var TYPE i.
my_var=>something( ).`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	if table == nil {
		testing.expect(t, false, "symbol table should not be nil")
		return
	}

	// Should have a diagnostic error because my_var is not a class
	diags := symbols.collect_all_diagnostics(table)
	testing.expect(
		t,
		len(diags) > 0,
		"expected diagnostic error for using => with non-class symbol",
	)

	if len(diags) > 0 {
		// Verify it's the right kind of error
		found_fat_arrow_error := false
		for diag in diags {
			if strings.contains(diag.message, "class") || strings.contains(diag.message, "interface") {
				found_fat_arrow_error = true
				break
			}
		}
		testing.expect(
			t,
			found_fat_arrow_error,
			fmt.tprintf("expected error about class/interface, got: %v", diags[0].message),
		)
	}
}

@(test)
test_fat_arrow_ok_with_class :: proc(t: ^testing.T) {
	// Using => with a class should NOT produce an error
	src := `CLASS my_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS some_method.
ENDCLASS.
my_class=>some_method( ).`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	if table == nil {
		testing.expect(t, false, "symbol table should not be nil")
		return
	}

	// Should NOT have a diagnostic error because my_class is a class
	diags := symbols.collect_all_diagnostics(table)
	fat_arrow_errors := 0
	for diag in diags {
		if strings.contains(diag.message, "'=>'") {
			fat_arrow_errors += 1
		}
	}
	testing.expect(
		t,
		fat_arrow_errors == 0,
		fmt.tprintf("expected no fat arrow errors with class, got %d errors", fat_arrow_errors),
	)
}

@(test)
test_fat_arrow_ok_with_interface :: proc(t: ^testing.T) {
	// Using => with an interface should NOT produce an error
	src := `INTERFACE my_interface.
  CLASS-METHODS some_method.
ENDINTERFACE.
my_interface=>some_method( ).`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	if table == nil {
		testing.expect(t, false, "symbol table should not be nil")
		return
	}

	// Should NOT have a diagnostic error because my_interface is an interface
	diags := symbols.collect_all_diagnostics(table)
	fat_arrow_errors := 0
	for diag in diags {
		if strings.contains(diag.message, "'=>'") {
			fat_arrow_errors += 1
		}
	}
	testing.expect(
		t,
		fat_arrow_errors == 0,
		fmt.tprintf("expected no fat arrow errors with interface, got %d errors", fat_arrow_errors),
	)
}

@(test)
test_unknown_symbol_in_expression :: proc(t: ^testing.T) {
	src := `DATA lv_value TYPE i.
lv_value = missing_var.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	diags := symbols.collect_all_diagnostics(table)
	found_unknown := false
	for diag in diags {
		if strings.contains(diag.message, "Unknown symbol 'missing_var'") {
			found_unknown = true
			break
		}
	}

	testing.expect(t, found_unknown, "expected unresolved identifier diagnostic for missing_var")
}

@(test)
test_unknown_local_symbol_does_not_request_remote_resolution :: proc(t: ^testing.T) {
	src := `DATA lv_value TYPE i.
lv_value = missing_var.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	candidates := symbols.collect_all_remote_candidates(table)
	testing.expect(t, len(candidates) == 0, "expected local missing_var to avoid remote lookup candidates")
}

@(test)
test_unknown_custom_type_records_remote_candidate :: proc(t: ^testing.T) {
	src := `DATA lo_demo TYPE zcl_remote_demo.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	candidates := symbols.collect_all_remote_candidates(table)
	if !testing.expect(t, len(candidates) == 1, fmt.tprintf("expected 1 remote candidate, got %d", len(candidates))) do return

	testing.expect(t, candidates[0].name == "zcl_remote_demo", fmt.tprintf("unexpected candidate name %q", candidates[0].name))
	testing.expect(t, candidates[0].kind == .Type_Name, fmt.tprintf("unexpected candidate kind %v", candidates[0].kind))
}

@(test)
test_nested_structure_type_component_chain_no_false_unknown_type :: proc(t: ^testing.T) {
	// ABAP: TYPES ... TYPE structure-comp-subcomp — component names are not standalone types.
	src := `TYPES:
  BEGIN OF street_type,
    name TYPE c LENGTH 40,
    no   TYPE c LENGTH 4,
  END OF street_type.

TYPES:
  BEGIN OF address_type,
    name   TYPE c LENGTH 30,
    street TYPE street_type,
    BEGIN OF city,
      zipcode TYPE n LENGTH 5,
      name TYPE c LENGTH 40,
    END OF city,
  END OF address_type.

TYPES zipcode_type TYPE address_type-city-zipcode.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	diags := symbols.collect_all_diagnostics(table)
	for diag in diags {
		if strings.contains(diag.message, "Unknown type 'city'") ||
		   strings.contains(diag.message, "Unknown type 'zipcode'") {
			testing.expect(
				t,
				false,
				fmt.tprintf("unexpected diagnostic: %s", diag.message),
			)
			return
		}
	}
}

@(test)
test_char_length_suffix_builtin_type :: proc(t: ^testing.T) {
	src := `TYPES:
  BEGIN OF ts_ui_funcs,
    fcode    TYPE char70,
    textid   TYPE char3,
    text_add TYPE string,
    disabled TYPE char01,
  END OF ts_ui_funcs.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	sym, ok := table.symbols["ts_ui_funcs"]
	if !testing.expect(t, ok, "expected typedef ts_ui_funcs") do return
	if !testing.expect(t, sym.type_info != nil, "expected structure type") do return
	if !testing.expect(t, sym.type_info.kind == .Structure, "expected Structure kind") do return

	find_field :: proc(st: ^symbols.Type, field: string) -> ^symbols.StructField {
		ln := strings.to_lower(field, context.temp_allocator)
		for i in 0 ..< len(st.fields) {
			if st.fields[i].name == ln {
				return &st.fields[i]
			}
		}
		return nil
	}

	st := sym.type_info
	f_fcode := find_field(st, "fcode")
	if testing.expect(t, f_fcode != nil, "field fcode") && f_fcode != nil {
		testing.expect(t, f_fcode.type_info != nil && f_fcode.type_info.kind == .Char, "fcode is Char")
		if f_fcode.type_info != nil {
			testing.expect(t, f_fcode.type_info.length == 70, fmt.tprintf("fcode length 70, got %d", f_fcode.type_info.length))
		}
	}
	f_textid := find_field(st, "textid")
	if testing.expect(t, f_textid != nil, "field textid") && f_textid != nil && f_textid.type_info != nil {
		testing.expect(t, f_textid.type_info.kind == .Char && f_textid.type_info.length == 3, "textid char3")
	}
	f_text_add := find_field(st, "text_add")
	if testing.expect(t, f_text_add != nil, "field text_add") && f_text_add != nil && f_text_add.type_info != nil {
		testing.expect(t, f_text_add.type_info.kind == .String, "text_add string")
	}
	f_disabled := find_field(st, "disabled")
	if testing.expect(t, f_disabled != nil, "field disabled") && f_disabled != nil && f_disabled.type_info != nil {
		testing.expect(t, f_disabled.type_info.kind == .Char && f_disabled.type_info.length == 1, "disabled char01")
	}

	candidates := symbols.collect_all_remote_candidates(table)
	testing.expect(t, len(candidates) == 0, "charNN should not trigger remote type candidates")

	diags := symbols.collect_all_diagnostics(table)
	for diag in diags {
		msg_l := strings.to_lower(diag.message, context.temp_allocator)
		if strings.contains(msg_l, "unknown type 'char70'") ||
		   strings.contains(msg_l, "unknown type 'char3'") ||
		   strings.contains(msg_l, "unknown type 'char01'") {
			testing.expect(
				t,
				false,
				fmt.tprintf("char built-in must not be reported as unknown type: %s", diag.message),
			)
			return
		}
	}
}

@(test)
test_builtin_sy_symbol_resolves :: proc(t: ^testing.T) {
	src := `DATA lv_subrc TYPE i.
lv_subrc = sy-subrc.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	sy_sym, ok := table.symbols["sy"]
	if !testing.expect(t, ok, "expected built-in symbol 'sy' to be present") do return

	testing.expect(t, sy_sym.type_info != nil, "expected 'sy' to have type information")
	if sy_sym.type_info != nil {
		testing.expect(
			t,
			sy_sym.type_info.kind == .Structure,
			fmt.tprintf("expected 'sy' to have structure type, got %v", sy_sym.type_info.kind),
		)

		has_subrc := false
		for field in sy_sym.type_info.fields {
			if field.name == "subrc" {
				has_subrc = true
				testing.expect(
					t,
					field.type_info != nil && field.type_info.kind == .Integer,
					"expected sy-subrc to be an integer field",
				)
				break
			}
		}
		testing.expect(t, has_subrc, "expected 'sy' structure to expose field 'subrc'")
	}

	diags := symbols.collect_all_diagnostics(table)
	found_sy_or_subrc_error := false
	for diag in diags {
		if strings.contains(diag.message, "Unknown symbol 'sy'") ||
		   strings.contains(diag.message, "Unknown symbol 'subrc'") ||
		   strings.contains(diag.message, "Unknown field 'subrc'") {
			found_sy_or_subrc_error = true
			break
		}
	}

	testing.expect(
		t,
		!found_sy_or_subrc_error,
		"expected built-in 'sy' and component 'sy-subrc' to resolve without diagnostics",
	)
}

@(test)
test_data_type_sy_tabix_no_cannot_use_as_type_diagnostic :: proc(t: ^testing.T) {
	src := `DATA lv_tabix TYPE sy-tabix.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	for diag in symbols.collect_all_diagnostics(table) {
		if strings.contains(diag.message, "cannot be used as a type") &&
		   strings.contains(diag.message, "sy") {
			testing.expect(t, false, fmt.tprintf("unexpected diagnostic: %s", diag.message))
			return
		}
	}
}

@(test)
test_if_sy_subrc_no_unknown_component_diagnostic :: proc(t: ^testing.T) {
	src := `IF sy-subrc = 0.

ENDIF.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	for diag in symbols.collect_all_diagnostics(table) {
		if strings.contains(diag.message, "subrc") {
			testing.expect(
				t,
				false,
				fmt.tprintf("unexpected diagnostic mentioning subrc: %s", diag.message),
			)
			return
		}
	}
}

@(test)
test_builtin_abap_bool_constants_resolve :: proc(t: ^testing.T) {
	src := `CONSTANTS lc_true TYPE c LENGTH 1 VALUE abap_true.
CONSTANTS lc_false TYPE c LENGTH 1 VALUE abap_false.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	true_sym, ok := table.symbols["abap_true"]
	if !testing.expect(t, ok, "expected built-in symbol 'abap_true' to be present") do return

	false_sym, ok_false := table.symbols["abap_false"]
	if !testing.expect(t, ok_false, "expected built-in symbol 'abap_false' to be present") do return

	testing.expect(t, true_sym.kind == .Constant, fmt.tprintf("expected 'abap_true' to be a constant, got %v", true_sym.kind))
	testing.expect(t, false_sym.kind == .Constant, fmt.tprintf("expected 'abap_false' to be a constant, got %v", false_sym.kind))

	testing.expect(t, true_sym.type_info != nil, "expected 'abap_true' to have type information")
	if true_sym.type_info != nil {
		testing.expect(
			t,
			true_sym.type_info.kind == .Char && true_sym.type_info.length == 1,
			fmt.tprintf("expected 'abap_true' to have type c length 1, got %v length %d", true_sym.type_info.kind, true_sym.type_info.length),
		)
	}

	testing.expect(t, false_sym.type_info != nil, "expected 'abap_false' to have type information")
	if false_sym.type_info != nil {
		testing.expect(
			t,
			false_sym.type_info.kind == .Char && false_sym.type_info.length == 1,
			fmt.tprintf("expected 'abap_false' to have type c length 1, got %v length %d", false_sym.type_info.kind, false_sym.type_info.length),
		)
	}

	diags := symbols.collect_all_diagnostics(table)
	found_true_error := false
	found_false_error := false
	for diag in diags {
		if strings.contains(diag.message, "Unknown symbol 'abap_true'") {
			found_true_error = true
		}
		if strings.contains(diag.message, "Unknown symbol 'abap_false'") {
			found_false_error = true
		}
	}

	testing.expect(t, !found_true_error, "expected built-in symbol 'abap_true' to resolve without diagnostics")
	testing.expect(t, !found_false_error, "expected built-in symbol 'abap_false' to resolve without diagnostics")
}

@(test)
test_builtin_abap_bool_type_in_data_no_unknown_type_diagnostic :: proc(t: ^testing.T) {
	src := `DATA lv_flag TYPE abap_bool VALUE abap_false.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	bool_sym, ok := table.symbols["abap_bool"]
	if !testing.expect(t, ok, "expected built-in type 'abap_bool' in symbol table") do return
	testing.expect(t, bool_sym.kind == .TypeDef, fmt.tprintf("expected 'abap_bool' to be TypeDef, got %v", bool_sym.kind))

	for diag in symbols.collect_all_diagnostics(table) {
		if strings.contains(diag.message, "Unknown type 'abap_bool'") {
			testing.expect(t, false, fmt.tprintf("unexpected diagnostic: %s", diag.message))
			return
		}
	}
}

@(test)
test_builtin_strlen_call_no_unknown_symbol_diagnostic :: proc(t: ^testing.T) {
	src := `DATA lv_text_add TYPE string.
DATA lv_long_descr TYPE abap_bool.
IF strlen( lv_text_add ) > 18.
  lv_long_descr = abap_true.
ENDIF.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	for diag in symbols.collect_all_diagnostics(table) {
		if strings.contains(diag.message, "Unknown symbol 'strlen'") {
			testing.expect(t, false, fmt.tprintf("unexpected diagnostic: %s", diag.message))
			return
		}
	}
}

@(test)
test_builtin_string_measure_calls_no_unknown_symbol_diagnostics :: proc(t: ^testing.T) {
	src := `DATA lv_c(10) TYPE c.
DATA lv_s TYPE string.
DATA lv_n TYPE i.
lv_n = charlen( lv_s ) + dbmaxlen( lv_s ) + numofchar( lv_c ).`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	for name in ([]string{"charlen", "dbmaxlen", "numofchar"}) {
		for diag in symbols.collect_all_diagnostics(table) {
			if strings.contains(diag.message, fmt.tprintf("Unknown symbol '%s'", name)) {
				testing.expect(t, false, fmt.tprintf("unexpected diagnostic: %s", diag.message))
				return
			}
		}
	}
}

@(test)
test_builtin_function_hover_markdown :: proc(t: ^testing.T) {
	h_len := symbols.builtin_function_hover_markdown("strlen")
	if !testing.expect(t, len(h_len) > 0, "strlen hover") do return
	testing.expect(t, strings.contains(h_len, "strlen"), h_len)
	testing.expect(t, strings.contains(h_len, "type string"), h_len)

	h_db := symbols.builtin_function_hover_markdown("dbmaxlen")
	if !testing.expect(t, len(h_db) > 0, "dbmaxlen hover") do return
	testing.expect(t, strings.contains(h_db, "ABAP Dictionary"), h_db)

	h_ch := symbols.builtin_function_hover_markdown("charlen")
	if !testing.expect(t, len(h_ch) > 0, "charlen hover") do return
	testing.expect(t, strings.contains(h_ch, "surrogate"), h_ch)

	testing.expect(t, len(symbols.builtin_function_hover_markdown("not_a_builtin")) == 0, "unknown builtin")
}

@(test)
test_method_impl_uses_declared_params :: proc(t: ^testing.T) {
	src := `CLASS lcl_calc DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_input TYPE i.
ENDCLASS.
CLASS lcl_calc IMPLEMENTATION.
  METHOD run.
    DATA lv_copy TYPE i.
    lv_copy = iv_input.
  ENDMETHOD.
ENDCLASS.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	diags := symbols.collect_all_diagnostics(table)
	found_param_error := false
	for diag in diags {
		if strings.contains(diag.message, "iv_input") {
			found_param_error = true
			break
		}
	}

	testing.expect(t, !found_param_error, "expected method parameter iv_input to resolve inside implementation")
}

@(test)
test_instance_method_me_object_ref_resolves :: proc(t: ^testing.T) {
	src := `class /sttp/cl_ui_helper definition
  public
  final
  create public .
public section.
  methods get_ref_ui_cockpit
    exporting
      !eo_ui_cockpit type ref to /sttp/cl_ui_cockpit .
private section.
  class-data so_ui_cockpit type ref to /sttp/cl_ui_cockpit .
endclass.
class /sttp/cl_ui_helper implementation.
  method get_ref_ui_cockpit.
    eo_ui_cockpit = me->so_ui_cockpit.
  endmethod.
endclass.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	for diag in symbols.collect_all_diagnostics(table) {
		if strings.contains(diag.message, "Unknown symbol 'me'") ||
		   strings.contains(diag.message, "Unknown field 'so_ui_cockpit'") {
			testing.expect(t, false, fmt.tprintf("unexpected diagnostic: %s", diag.message))
			return
		}
	}
}

@(test)
test_class_method_me_still_unknown :: proc(t: ^testing.T) {
	src := `class lcl_t definition.
  public section.
    class-methods test.
    class-data so_x type i.
endclass.
class lcl_t implementation.
  method test.
    so_x = me->so_x.
  endmethod.
endclass.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	found_me_unknown := false
	for diag in symbols.collect_all_diagnostics(table) {
		if strings.contains(diag.message, "Unknown symbol 'me'") {
			found_me_unknown = true
			break
		}
	}
	testing.expect(t, found_me_unknown, "expected 'me' to be unknown inside CLASS-METHODS implementation")
}

@(test)
test_select_into_exporting_param_not_duplicate_symbol :: proc(t: ^testing.T) {
	src := `CLASS lcl_ui DEFINITION.
  PUBLIC SECTION.
    METHODS get_user_full_name
      IMPORTING iv_user TYPE string
      EXPORTING ev_name_text TYPE string.
ENDCLASS.
CLASS lcl_ui IMPLEMENTATION.
  METHOD get_user_full_name.
    SELECT SINGLE name_textc FROM user_addr INTO ev_name_text WHERE bname = iv_user.
  ENDMETHOD.
ENDCLASS.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	for diag in symbols.collect_all_diagnostics(table) {
		if strings.contains(diag.message, "Duplicate symbol") &&
		   strings.contains(diag.message, "ev_name_text") {
			testing.expect(
				t,
				false,
				fmt.tprintf(
					"SELECT INTO must not declare a new symbol for an existing exporting parameter: %s",
					diag.message,
				),
			)
			return
		}
	}
}

@(test)
test_loop_assigning_existing_field_symbol_not_duplicate :: proc(t: ^testing.T) {
	src := `CLASS some_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS create_entry_cusset.
ENDCLASS.
CLASS some_class IMPLEMENTATION.
  METHOD create_entry_cusset.
    TYPES:
      BEGIN OF ts_ui_funcs,
        textid   TYPE char3,
        text_add TYPE string,
        disabled TYPE char01,
      END OF ts_ui_funcs,
      tt_ui_funcs TYPE STANDARD TABLE OF ts_ui_funcs.
    DATA lt_rep_response TYPE tt_ui_funcs.
    FIELD-SYMBOLS: <ls_rep_response> TYPE ts_ui_funcs.
    LOOP AT lt_rep_response ASSIGNING <ls_rep_response>.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	for diag in symbols.collect_all_diagnostics(table) {
		if strings.contains(diag.message, "Duplicate symbol") &&
		   strings.contains(diag.message, "<ls_rep_response>") {
			testing.expect(
				t,
				false,
				fmt.tprintf(
					"LOOP ASSIGNING to an existing field symbol must not introduce a duplicate: %s",
					diag.message,
				),
			)
			return
		}
	}
}

@(test)
test_loop_assigning_inline_field_symbol_still_declares :: proc(t: ^testing.T) {
	src := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
    TYPES ty_line TYPE string.
    DATA lt TYPE STANDARD TABLE OF ty_line WITH DEFAULT KEY.
    LOOP AT lt ASSIGNING FIELD-SYMBOL(<lv_line>).
      <lv_line> = 'x'.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	for diag in symbols.collect_all_diagnostics(table) {
		if strings.contains(diag.message, "Unknown symbol '<lv_line>'") ||
		   strings.contains(diag.message, "Duplicate symbol '<lv_line>'") ||
		   strings.contains(diag.message, "Inline FIELD-SYMBOL(...) is only valid") {
			testing.expect(
				t,
				false,
				fmt.tprintf("inline ASSIGNING FIELD-SYMBOL should declare <lv_line>: %s", diag.message),
			)
			return
		}
	}
}

@(test)
test_loop_assigning_inline_field_symbol_without_prior_fs_decl :: proc(t: ^testing.T) {
	src := `CLASS some_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS create_entry_cusset.
ENDCLASS.
CLASS some_class IMPLEMENTATION.
  METHOD create_entry_cusset.
    TYPES:
      BEGIN OF ts_ui_funcs,
        textid   TYPE char3,
        text_add TYPE string,
        disabled TYPE char01,
      END OF ts_ui_funcs,
      tt_ui_funcs TYPE STANDARD TABLE OF ts_ui_funcs.
    DATA lt_rep_response TYPE tt_ui_funcs.
    " FIELD-SYMBOLS: <ls_rep_response> TYPE ts_ui_funcs.
    LOOP AT lt_rep_response ASSIGNING FIELD-SYMBOL(<ls_rep_response>).
      <ls_rep_response>-textid = 'a01'.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	for diag in symbols.collect_all_diagnostics(table) {
		if strings.contains(diag.message, "Inline FIELD-SYMBOL(...) is only valid") {
			testing.expect(
				t,
				false,
				fmt.tprintf(
					"inline FIELD-SYMBOL in LOOP ASSIGNING must be allowed: %s",
					diag.message,
				),
			)
			return
		}
		if strings.contains(diag.message, "Unknown symbol '<ls_rep_response>'") {
			testing.expect(
				t,
				false,
				fmt.tprintf(
					"loop body should see ASSIGNING FIELD-SYMBOL binding: %s",
					diag.message,
				),
			)
			return
		}
	}
}

@(test)
test_select_into_inline_data_still_declares_symbol :: proc(t: ^testing.T) {
	src := `CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
    SELECT SINGLE field FROM some_table INTO @DATA(lv_row) WHERE key = 'x'.
    DATA lv_copy TYPE string.
    lv_copy = lv_row.
  ENDMETHOD.
ENDCLASS.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	for diag in symbols.collect_all_diagnostics(table) {
		if strings.contains(diag.message, "Unknown symbol 'lv_row'") {
			testing.expect(t, false, fmt.tprintf("lv_row should resolve after inline SELECT INTO: %s", diag.message))
			return
		}
	}
}

@(test)
test_method_call_importing_inline_data_symbol :: proc(t: ^testing.T) {
	src := `CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    lcl_api=>get_msg( IMPORTING es_message = DATA(ls_message) ).
    DATA lv_copy TYPE string.
    lv_copy = ls_message.
  ENDMETHOD.
ENDCLASS.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	for diag in symbols.collect_all_diagnostics(table) {
		if strings.contains(diag.message, "Unknown symbol 'ls_message'") {
			testing.expect(
				t,
				false,
				fmt.tprintf("expected ls_message from IMPORTING = DATA(ls_message) to resolve: %s", diag.message),
			)
			return
		}
	}
}

@(test)
test_syntax_tainted_top_level_statement_preserves_clean_symbols :: proc(t: ^testing.T) {
	src := `DATA lv_before TYPE i.
lv_item->attr noise_token.
DATA lv_after TYPE i.
lv_before = lv_after.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	if !testing.expect(t, len(file.syntax_errors) > 0, "expected syntax errors for unsupported statement") do return

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	_, has_before := table.symbols["lv_before"]
	_, has_after := table.symbols["lv_after"]
	testing.expect(t, has_before, "expected lv_before to remain resolvable")
	testing.expect(t, has_after, "expected lv_after to remain resolvable")

	diags := symbols.collect_all_diagnostics(table)
	found_noise := false
	for diag in diags {
		if strings.contains(diag.message, "noise_token") || strings.contains(diag.message, "lv_item") {
			found_noise = true
			break
		}
	}
	testing.expect(t, !found_noise, "expected no semantic diagnostics inside tainted statement")

	candidates := symbols.collect_all_remote_candidates(table)
	for candidate in candidates {
		if candidate.name == "noise_token" || candidate.name == "lv_item" {
			found_noise = true
			break
		}
	}
	testing.expect(t, !found_noise, "expected no remote lookup candidates from tainted statement")
}

@(test)
test_syntax_tainted_form_declaration_is_skipped_entirely :: proc(t: ^testing.T) {
	src := `FORM run.
  DATA lv_before TYPE i.
  lv_item->attr noise_token.
  DATA lv_after TYPE i.
  lv_before = lv_after.
ENDFORM.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)

	if !testing.expect(t, len(file.syntax_errors) > 0, "expected syntax errors for unsupported nested statement") do return

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	_, has_form := table.symbols["run"]
	testing.expect(t, !has_form, "expected tainted FORM declaration to be skipped entirely")

	diags := symbols.collect_all_diagnostics(table)
	found_noise := false
	for diag in diags {
		if strings.contains(diag.message, "noise_token") || strings.contains(diag.message, "lv_item") {
			found_noise = true
			break
		}
	}
	testing.expect(t, !found_noise, "expected no semantic diagnostics from tainted nested statement")
}

@(test)
test_class_def_inheriting_from_interface_diagnostic :: proc(t: ^testing.T) {
	src := `INTERFACE zif_i.
ENDINTERFACE.
CLASS zcl_c DEFINITION INHERITING FROM zif_i.
  PUBLIC SECTION.
ENDCLASS.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)
	testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("syntax errors: %v", file.syntax_errors))

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	diags := symbols.collect_all_diagnostics(table)
	found := false
	for diag in diags {
		if strings.contains(diag.message, "INHERITING FROM must reference a class") {
			found = true
			break
		}
	}
	testing.expect(t, found, "expected INHERITING FROM / class kind diagnostic")
}

@(test)
test_class_def_interfaces_clause_non_interface_diagnostic :: proc(t: ^testing.T) {
	src := `CLASS zcl_c DEFINITION.
  PUBLIC SECTION.
ENDCLASS.
CLASS zcl_d DEFINITION.
  PUBLIC SECTION.
    INTERFACES zcl_c.
ENDCLASS.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)
	testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("syntax errors: %v", file.syntax_errors))

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	diags := symbols.collect_all_diagnostics(table)
	found := false
	for diag in diags {
		if strings.contains(diag.message, "not an interface") &&
		   strings.contains(diag.message, "INTERFACES") {
			found = true
			break
		}
	}
	testing.expect(t, found, "expected INTERFACES / interface kind diagnostic")
}

@(test)
test_class_def_friends_non_class_diagnostic :: proc(t: ^testing.T) {
	src := `DATA gv TYPE i.
CLASS zcl_c DEFINITION FRIENDS gv.
  PUBLIC SECTION.
ENDCLASS.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)
	testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("syntax errors: %v", file.syntax_errors))

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	diags := symbols.collect_all_diagnostics(table)
	found := false
	for diag in diags {
		if strings.contains(diag.message, "FRIENDS must reference a class or interface") {
			found = true
			break
		}
	}
	testing.expect(t, found, "expected FRIENDS kind diagnostic")
}

@(test)
test_class_def_behavior_of_non_interface_diagnostic :: proc(t: ^testing.T) {
	src := `CLASS zcl_c DEFINITION.
  PUBLIC SECTION.
ENDCLASS.
CLASS zcl_b DEFINITION FOR BEHAVIOR OF zcl_c.
  PUBLIC SECTION.
ENDCLASS.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)
	testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("syntax errors: %v", file.syntax_errors))

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	diags := symbols.collect_all_diagnostics(table)
	found := false
	for diag in diags {
		if strings.contains(diag.message, "BEHAVIOR OF must reference an interface") {
			found = true
			break
		}
	}
	testing.expect(t, found, "expected BEHAVIOR OF / interface kind diagnostic")
}

@(test)
test_exception_class_workspace_unknown_types_diag :: proc(t: ^testing.T) {
	// Names like /STTP/..., CX_ROOT, DDIC rows are absent from a minimal workspace — expect Unknown type diagnostics,
	// while INT2 is treated as a built-in and LIKE … TO sibling parameters are not required to resolve globally.
	src := `CLASS /sttp/cx_rr_ru_rest_client DEFINITION
  PUBLIC
  INHERITING FROM /sttp/cx_base_exception
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
  METHODS constructor
    IMPORTING
      !textid LIKE textid OPTIONAL
      !previous LIKE previous OPTIONAL
      !messages TYPE REF TO /sttp/cl_messages OPTIONAL
      !message TYPE bal_s_msg OPTIONAL
      !message_text TYPE bapi_msg OPTIONAL
      !returncode TYPE int2 OPTIONAL .
  CLASS-METHODS raise_from_cx
    IMPORTING
      !io_previous TYPE REF TO cx_root
    RAISING
      /sttp/cx_rr_ru_rest_client .
  CLASS-METHODS raise_with_sy_msg
    RAISING
      /sttp/cx_rr_ru_rest_client .
  METHODS add_message_from_exception
    IMPORTING
      !io_messages TYPE REF TO /sttp/cl_messages .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)
	if !testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("syntax: %v", file.syntax_errors)) do return

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	diags := symbols.collect_all_diagnostics(table)
	unknown_type := 0
	has_cx_root := false
	has_base_exc := false
	for d in diags {
		if !strings.contains(d.message, "Unknown type '") {
			continue
		}
		unknown_type += 1
		if strings.contains(d.message, "cx_root") {
			has_cx_root = true
		}
		if strings.contains(d.message, "cx_base_exception") {
			has_base_exc = true
		}
	}
	testing.expect(t, unknown_type >= 5, fmt.tprintf("expected several Unknown type diagnostics, got %d", unknown_type))
	testing.expect(t, has_cx_root, "expected Unknown type for CX_ROOT (REF TO)")
	testing.expect(t, has_base_exc, "expected Unknown type for /sttp/cx_base_exception (INHERITING FROM)")
	found_int2 := false
	for d in diags {
		if strings.contains(d.message, "int2") {
			found_int2 = true
			break
		}
	}
	testing.expect(t, !found_int2, "INT2 is built-in; should not produce Unknown type diagnostic")
}

@(test)
test_ref_to_builtin_generic_data_no_unknown_type_diag :: proc(t: ^testing.T) {
	src := "DATA lo_str TYPE REF TO data."
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)
	testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("syntax: %v", file.syntax_errors))

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	diags := symbols.collect_all_diagnostics(table)
	for d in diags {
		lower := strings.to_lower(d.message, context.temp_allocator)
		if strings.contains(lower, "unknown type 'data'") {
			testing.expect(t, false, fmt.tprintf("REF TO data uses built-in generic type: %s", d.message))
			return
		}
	}
}

@(test)
test_class_section_types_table_of_local_struct_no_unknown_type_diag :: proc(t: ^testing.T) {
	// TYPES in class definition: later entry may reference an earlier struct in the same chain (ts_ui_funcs -> tt_ui_funcs).
	src := `CLASS some_class DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_ui_funcs,
        fcode    TYPE string,
        textid   TYPE char3,
        text_add TYPE string,
        disabled TYPE char01,
      END OF ts_ui_funcs,
      tt_ui_funcs TYPE STANDARD TABLE OF ts_ui_funcs.
ENDCLASS.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)
	if !testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("syntax: %v", file.syntax_errors)) do return

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	diags := symbols.collect_all_diagnostics(table)
	for d in diags {
		if strings.contains(d.message, "Unknown type 'ts_ui_funcs'") {
			testing.expect(t, false, fmt.tprintf("class-local TYPES should resolve: %s", d.message))
			return
		}
	}
}

@(test)
test_method_body_types_table_of_local_struct_no_unknown_type_diag :: proc(t: ^testing.T) {
	// TYPES inside METHOD implementation: table type may reference a struct from the same chain (ABAP forward refs in one TYPES: block).
	src := `CLASS some_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS create_entry_cusset.
ENDCLASS.

CLASS some_class IMPLEMENTATION.
  METHOD create_entry_cusset.
    TYPES:
      BEGIN OF ts_ui_funcs,
        textid   TYPE char3,
        text_add TYPE string,
        disabled TYPE char01,
      END OF ts_ui_funcs,
      tt_ui_funcs TYPE STANDARD TABLE OF ts_ui_funcs.
  ENDMETHOD.
ENDCLASS.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)
	if !testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("syntax: %v", file.syntax_errors)) do return

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	diags := symbols.collect_all_diagnostics(table)
	for d in diags {
		if strings.contains(d.message, "Unknown type 'ts_ui_funcs'") {
			testing.expect(t, false, fmt.tprintf("method-local TYPES should resolve: %s", d.message))
			return
		}
	}
}

@(test)
test_data_typed_chain_unknown_type_in_class_method_diag :: proc(t: ^testing.T) {
	// DATA: ... (chain) must run type validation; DDIC name /cdbasis/cusset is absent from workspace.
	src := `CLASS some_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS create_entry_cusset
      IMPORTING
        !is_cusset TYPE /cdbasis/cusset.
ENDCLASS.

CLASS some_class IMPLEMENTATION.
  METHOD create_entry_cusset.
    DATA: ls_cusset TYPE /cdbasis/cusset.
  ENDMETHOD.
ENDCLASS.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)
	if !testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("syntax: %v", file.syntax_errors)) do return

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	diags := symbols.collect_all_diagnostics(table)
	// Importing parameter and chained DATA local should each produce unknown type for the DDIC name.
	count_cusset_unknown := 0
	for d in diags {
		if strings.contains(d.message, "Unknown type '") &&
		   strings.contains(strings.to_lower(d.message, context.temp_allocator), "/cdbasis/cusset") {
			count_cusset_unknown += 1
		}
	}
	testing.expect(t, count_cusset_unknown >= 2, fmt.tprintf("expected >=2 Unknown type for /cdbasis/cusset, got %d", count_cusset_unknown))
}

@(test)
test_method_raising_same_class_no_unknown_exception_diag :: proc(t: ^testing.T) {
	src := `CLASS zcx_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS raise_me RAISING zcx_demo .
ENDCLASS.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)
	testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("syntax: %v", file.syntax_errors))

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	diags := symbols.collect_all_diagnostics(table)
	for d in diags {
		if strings.contains(d.message, "Unknown exception class") ||
		   strings.contains(d.message, "RAISING must list") ||
		   strings.contains(d.message, "cannot be used in RAISING") {
			testing.expect(t, false, fmt.tprintf("unexpected RAISING diagnostic: %s", d.message))
			return
		}
	}
}

@(test)
test_method_raising_unknown_exception_class_diag :: proc(t: ^testing.T) {
	src := `CLASS zcl_a DEFINITION.
  PUBLIC SECTION.
    METHODS m RAISING zcx_not_in_workspace .
ENDCLASS.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)
	testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("syntax: %v", file.syntax_errors))

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	diags := symbols.collect_all_diagnostics(table)
	found := false
	for d in diags {
		if strings.contains(d.message, "Unknown exception class 'zcx_not_in_workspace'") {
			found = true
			break
		}
	}
	testing.expect(t, found, "expected Unknown exception class diagnostic for RAISING")
}

@(test)
test_method_raising_builtin_rejected :: proc(t: ^testing.T) {
	src := `CLASS zcl_a DEFINITION.
  PUBLIC SECTION.
    METHODS m RAISING i .
ENDCLASS.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)
	testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("syntax: %v", file.syntax_errors))

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	diags := symbols.collect_all_diagnostics(table)
	found := false
	for d in diags {
		if strings.contains(d.message, "RAISING must list an exception class") &&
		   strings.contains(d.message, "built-in") {
			found = true
			break
		}
	}
	testing.expect(t, found, "expected built-in rejected in RAISING")
}

@(test)
test_interface_method_raising_resolves_class :: proc(t: ^testing.T) {
	src := `CLASS zcx_i DEFINITION.
  PUBLIC SECTION.
ENDCLASS.
INTERFACE zif_raisers.
  METHODS m RAISING zcx_i .
ENDINTERFACE.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)
	testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("syntax: %v", file.syntax_errors))

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	diags := symbols.collect_all_diagnostics(table)
	for d in diags {
		if strings.contains(d.message, "Unknown exception class") {
			testing.expect(t, false, fmt.tprintf("unexpected unknown exception: %s", d.message))
			return
		}
	}
}

@(test)
test_class_def_valid_inheritance_and_interfaces_no_kind_errors :: proc(t: ^testing.T) {
	src := `INTERFACE zif_i.
ENDINTERFACE.
CLASS zcl_parent DEFINITION.
  PUBLIC SECTION.
ENDCLASS.
CLASS zcl_child DEFINITION INHERITING FROM zcl_parent.
  PUBLIC SECTION.
    INTERFACES zif_i.
    METHODS m IMPORTING iv TYPE i.
ENDCLASS.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)
	testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("syntax errors: %v", file.syntax_errors))

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	diags := symbols.collect_all_diagnostics(table)
	for diag in diags {
		if strings.contains(diag.message, "INHERITING FROM must reference") ||
		   strings.contains(diag.message, "INTERFACES expects") ||
		   strings.contains(diag.message, "BEHAVIOR OF must") ||
		   strings.contains(diag.message, "FRIENDS must") {
			testing.expect(t, false, fmt.tprintf("unexpected class-def kind diagnostic: %s", diag.message))
			return
		}
	}
}

@(test)
test_class_method_sees_class_data_in_body :: proc(t: ^testing.T) {
	src := `CLASS lcl_helper DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_ref EXPORTING eo_r TYPE i.
  PRIVATE SECTION.
    CLASS-DATA so_cockpit TYPE i.
ENDCLASS.

CLASS lcl_helper IMPLEMENTATION.
  METHOD get_ref.
    eo_r = so_cockpit.
  ENDMETHOD.
ENDCLASS.`
	file := ast.new(ast.File, {})
	file.src = src

	p: parser.Parser
	parser.parse_file(&p, file)
	testing.expect(t, len(file.syntax_errors) == 0, fmt.tprintf("syntax: %v", file.syntax_errors))

	table := symbols.resolve_file(file)
	defer symbols.destroy_symbol_table(table)

	for diag in symbols.collect_all_diagnostics(table) {
		if strings.contains(diag.message, "so_cockpit") {
			testing.expect(
				t,
				false,
				fmt.tprintf("CLASS-DATA should resolve in class method body: %s", diag.message),
			)
			return
		}
	}
}