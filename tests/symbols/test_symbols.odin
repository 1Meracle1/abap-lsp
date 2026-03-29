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
	found_sy_error := false
	for diag in diags {
		if strings.contains(diag.message, "Unknown symbol 'sy'") {
			found_sy_error = true
			break
		}
	}

	testing.expect(t, !found_sy_error, "expected built-in symbol 'sy' to resolve without diagnostics")
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