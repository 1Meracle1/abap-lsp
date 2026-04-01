package tests_lsp

import "../../src/cache"
import "../../src/lang/ast"
import "../../src/lang/parser"
import "../../src/lang/symbols"
import "../../src/lsp"
import "core:fmt"
import "core:strings"
import "core:testing"

parse_class_definition :: proc(t: ^testing.T, source: string) -> ^ast.Class_Def_Decl {
	file := ast.new(ast.File, {})
	file.src = source

	p: parser.Parser
	parser.parse_file(&p, file)

	if !testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("unexpected syntax errors: %v", file.syntax_errors),
	) {
		return nil
	}
	if !testing.expect(t, len(file.decls) == 1, "expected one declaration") {
		return nil
	}

	class_def, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
	if !testing.expect(t, ok, "expected class definition") {
		return nil
	}
	return class_def
}

make_snapshot :: proc(t: ^testing.T, source: string) -> ^cache.Snapshot {
	file := ast.new(ast.File, {})
	file.src = source

	p: parser.Parser
	parser.parse_file(&p, file)

	if !testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("unexpected syntax errors: %v", file.syntax_errors),
	) {
		return nil
	}

	table := symbols.resolve_file(file)
	if !testing.expect(t, table != nil, "expected symbol table") {
		return nil
	}

	snap := new(cache.Snapshot)
	snap.text = source
	snap.ast = file
	snap.symbol_table = table
	return snap
}

@(test)
hover_for_class_attribute_definition_test :: proc(t: ^testing.T) {
	source := `CLASS LCL_DEMO DEFINITION.
  PRIVATE SECTION.
    CLASS-DATA MV_COUNT TYPE i.
ENDCLASS.`

	class_def := parse_class_definition(t, source)
	if class_def == nil do return
	if !testing.expect(t, len(class_def.sections) == 1, "expected one class section") do return

	attr_decl, attr_ok := class_def.sections[0].data[0].derived_stmt.(^ast.Attr_Decl)
	if !testing.expect(t, attr_ok, "expected class attribute declaration") do return

	hover_text := lsp.format_class_attr_decl_signature(
		attr_decl,
		class_def.sections[0].access,
		source,
		class_def.sections[0].data[0].range,
	)

	testing.expect(
		t,
		strings.contains(hover_text, "PRIVATE SECTION."),
		fmt.tprintf("expected private section in hover, got %q", hover_text),
	)
	testing.expect(
		t,
		strings.contains(hover_text, "CLASS-DATA MV_COUNT TYPE i"),
		fmt.tprintf("expected class-data signature in hover, got %q", hover_text),
	)
}

@(test)
hover_for_class_method_definition_test :: proc(t: ^testing.T) {
	source := `CLASS LCL_DEMO DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS RUN
      IMPORTING IV_INPUT TYPE string
      RETURNING VALUE(RV_OUTPUT) TYPE string
      RAISING CX_STATIC_CHECK.
ENDCLASS.`

	class_def := parse_class_definition(t, source)
	if class_def == nil do return
	if !testing.expect(t, len(class_def.sections) == 1, "expected one class section") do return

	method_decl, method_ok := class_def.sections[0].methods[0].derived_stmt.(^ast.Method_Decl)
	if !testing.expect(t, method_ok, "expected class method declaration") do return

	hover_text := lsp.format_class_method_decl_signature(
		method_decl,
		class_def.sections[0].access,
		source,
	)

	testing.expect(
		t,
		strings.contains(hover_text, "PUBLIC SECTION."),
		fmt.tprintf("expected public section in hover, got %q", hover_text),
	)
	testing.expect(
		t,
		strings.contains(hover_text, "CLASS-METHODS RUN"),
		fmt.tprintf("expected class-methods signature in hover, got %q", hover_text),
	)
	testing.expect(
		t,
		strings.contains(hover_text, "IMPORTING IV_INPUT TYPE string"),
		fmt.tprintf("expected importing parameter in hover, got %q", hover_text),
	)
	testing.expect(
		t,
		strings.contains(hover_text, "RETURNING VALUE(RV_OUTPUT) TYPE string"),
		fmt.tprintf("expected returning parameter in hover, got %q", hover_text),
	)
	testing.expect(
		t,
		strings.contains(hover_text, "RAISING CX_STATIC_CHECK"),
		fmt.tprintf("expected raising clause in hover, got %q", hover_text),
	)
}

@(test)
hover_lookup_for_realistic_class_definition_members_test :: proc(t: ^testing.T) {
	source := `class ZATTP_CL_AIF_SEND_API_RCL_5 definition
  public
  inheriting from ZATTP_CL_RR_EU_AIF_PROC
  create public .

public section.

  data MV_EO_ID type CHAR40 .

  methods GENERATE_INBOUND_DATA
    redefinition .
  methods PROCESS_DATA
    redefinition .
  methods RAISE_SUCCESSFUL_MESSAGE
    redefinition .
  methods SET_DATA
    redefinition .
  methods SET_NOTIF_ATTR_AFTER_SEND
    redefinition .
  methods SET_NOTIF_ATTR_BEFORE_SEND
    redefinition .
protected section.

  data MS_REP_SYST type /STTP/S_SYST .
  data MS_CERTIFICATE type /STTP/S_CERTIFICATE .
  data MV_REQUEST type STRING .
  data MV_NTF_REVISION type CHAR30 .

  methods COMPOSE_REQUEST
    raising
      /STTP/CX_REP_EXCEPTION .
  methods SEND_NOTIFICATION
    exporting
      !EV_RETURN_CODE type STRING
      !EV_RESPONSE_STRING type STRING
      !EV_REASON type STRING
    raising
      /STTP/CX_REP_EXCEPTION .

  methods GENERATE_NOTIF_XML
    redefinition .
  PRIVATE SECTION.
ENDCLASS.`

	snap := make_snapshot(t, source)
	if snap == nil do return
	defer symbols.destroy_symbol_table(snap.symbol_table)

	class_def, ok := snap.ast.decls[0].derived_stmt.(^ast.Class_Def_Decl)
	if !testing.expect(t, ok, "expected class definition") do return
	if !testing.expect(t, len(class_def.sections) == 3, fmt.tprintf("expected 3 sections, got %d", len(class_def.sections))) do return

	public_attr, attr_ok := class_def.sections[0].data[0].derived_stmt.(^ast.Attr_Decl)
	if !testing.expect(t, attr_ok, "expected public attribute") do return
	attr_hover, attr_found := lsp.lookup_class_member_hover_at_offset(snap, public_attr.ident.range.start)
	if !testing.expect(t, attr_found, "expected hover for public attribute") do return
	testing.expect(
		t,
		strings.contains(attr_hover, "DATA MV_EO_ID TYPE CHAR40"),
		fmt.tprintf("expected MV_EO_ID hover, got %q", attr_hover),
	)

	public_method, method_ok := class_def.sections[0].methods[0].derived_stmt.(^ast.Method_Decl)
	if !testing.expect(t, method_ok, "expected public method") do return
	method_hover, method_found := lsp.lookup_class_member_hover_at_offset(snap, public_method.ident.range.start)
	if !testing.expect(t, method_found, "expected hover for redefinition method") do return
	method_hover_upper := strings.to_upper(method_hover, context.temp_allocator)
	testing.expect(
		t,
		strings.contains(method_hover_upper, "METHODS GENERATE_INBOUND_DATA"),
		fmt.tprintf("expected GENERATE_INBOUND_DATA hover, got %q", method_hover),
	)
	testing.expect(
		t,
		strings.contains(method_hover_upper, "REDEFINITION"),
		fmt.tprintf("expected redefinition in hover, got %q", method_hover),
	)

	protected_method, protected_ok := class_def.sections[1].methods[1].derived_stmt.(^ast.Method_Decl)
	if !testing.expect(t, protected_ok, "expected protected method") do return
	protected_hover, protected_found := lsp.lookup_class_member_hover_at_offset(
		snap,
		protected_method.ident.range.start,
	)
	if !testing.expect(t, protected_found, "expected hover for exporting method") do return
	protected_hover_upper := strings.to_upper(protected_hover, context.temp_allocator)
	testing.expect(
		t,
		strings.contains(protected_hover_upper, "METHODS SEND_NOTIFICATION"),
		fmt.tprintf("expected SEND_NOTIFICATION hover, got %q", protected_hover),
	)
	testing.expect(
		t,
		strings.contains(protected_hover_upper, "EXPORTING"),
		fmt.tprintf("expected exporting clause in hover, got %q", protected_hover),
	)
	testing.expect(
		t,
		strings.contains(protected_hover_upper, "!EV_RETURN_CODE TYPE STRING"),
		fmt.tprintf("expected exporting parameter in hover, got %q", protected_hover),
	)

	protected_param := protected_method.params[0]
	if !testing.expect(t, protected_param != nil && protected_param.ident != nil, "expected first exporting parameter") do return
	param_hover, param_found := lsp.lookup_method_param_hover_at_offset(
		snap,
		protected_param.ident.range.start,
	)
	if !testing.expect(t, param_found, "expected hover for exporting parameter") do return
	param_hover_upper := strings.to_upper(param_hover, context.temp_allocator)
	testing.expect(
		t,
		strings.contains(param_hover_upper, "EXPORTING !EV_RETURN_CODE TYPE STRING"),
		fmt.tprintf("expected parameter hover, got %q", param_hover),
	)
}

@(test)
hover_lookup_for_class_constants_nested_in_definition_test :: proc(t: ^testing.T) {
	source := `CLASS ZATTP_CL_REP_CONSTANTS DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF gcs_aif_ifname,
        BEGIN OF europe,
          aggregation_epa_32 TYPE string VALUE 'ZEU_EPA_32',
          dispatch_edp_33       TYPE string VALUE 'ZEU_EDP_33',
        END OF europe,
      END OF gcs_aif_ifname .
ENDCLASS.`

	snap := make_snapshot(t, source)
	if snap == nil do return
	defer symbols.destroy_symbol_table(snap.symbol_table)

	class_def, ok := snap.ast.decls[0].derived_stmt.(^ast.Class_Def_Decl)
	if !testing.expect(t, ok, "expected class definition") do return
	if !testing.expect(t, len(class_def.sections) == 1, "expected one section") do return

	outer, outer_ok := class_def.sections[0].data[0].derived_stmt.(^ast.Const_Struct_Decl)
	if !testing.expect(t, outer_ok, "expected outer CONSTANTS struct") do return
	if !testing.expect(t, len(outer.components) >= 1, "expected nested components") do return

	europe, europe_ok := outer.components[0].derived_stmt.(^ast.Const_Struct_Decl)
	if !testing.expect(t, europe_ok, "expected inner europe CONSTANTS struct") do return
	if !testing.expect(t, len(europe.components) >= 1, "expected leaf constants") do return

	leaf, leaf_ok := europe.components[0].derived_stmt.(^ast.Const_Decl)
	if !testing.expect(t, leaf_ok, "expected leaf CONSTANTS decl") do return
	if !testing.expect(t, leaf.ident != nil, "expected leaf ident") do return

	leaf_hover, leaf_found := lsp.lookup_class_member_hover_at_offset(snap, leaf.ident.range.start)
	if !testing.expect(t, leaf_found, "expected hover for nested class constant") do return
	leaf_upper := strings.to_upper(leaf_hover, context.temp_allocator)
	testing.expect(
		t,
		strings.contains(leaf_upper, "PUBLIC SECTION.") &&
		strings.contains(leaf_upper, "CONSTANTS AGGREGATION_EPA_32 TYPE STRING") &&
		strings.contains(leaf_upper, "VALUE 'ZEU_EPA_32'"),
		fmt.tprintf("expected CONSTANTS leaf hover, got %q", leaf_hover),
	)

	if europe.ident != nil {
		europe_hover, europe_found := lsp.lookup_class_member_hover_at_offset(
			snap,
			europe.ident.range.start,
		)
		if !testing.expect(t, europe_found, "expected hover for inner struct name") do return
		testing.expect(
			t,
			strings.contains(europe_hover, "(constants structure) europe"),
			fmt.tprintf("expected structure hover, got %q", europe_hover),
		)
	}
}

@(test)
lookup_symbol_in_class_impl_method_body_test :: proc(t: ^testing.T) {
	source :=
		"CLASS /STTP/CL_UI_HELPER DEFINITION.\n" +
		"  PUBLIC SECTION.\n" +
		"    CLASS-METHODS GET_REF_UI_COCKPIT\n" +
		"      EXPORTING\n" +
		"        !EO_UI_COCKPIT TYPE REF TO /STTP/CL_UI_COCKPIT.\n" +
		"  PRIVATE SECTION.\n" +
		"    CLASS-DATA SO_UI_COCKPIT TYPE REF TO /STTP/CL_UI_COCKPIT.\n" +
		"ENDCLASS.\n" +
		"\n" +
		"CLASS /STTP/CL_UI_HELPER IMPLEMENTATION.\n" +
		"  METHOD GET_REF_UI_COCKPIT.\n" +
		"    EO_UI_COCKPIT = SO_UI_COCKPIT.\n" +
		"  ENDMETHOD.\n" +
		"ENDCLASS.\n"

	snap := make_snapshot(t, source)
	if snap == nil do return
	defer symbols.destroy_symbol_table(snap.symbol_table)

	assign_pos := strings.index(source, "EO_UI_COCKPIT =")
	if !testing.expect(t, assign_pos >= 0, "expected assignment in implementation") do return
	eo_sym, eo_ok := lsp.lookup_symbol_at_offset(
		snap,
		"eo_ui_cockpit",
		assign_pos + 3,
		snap.symbol_table,
	)
	if !testing.expect(t, eo_ok, "expected lookup for exporting parameter in method body") do return
	if !testing.expect(t, eo_sym.kind == .Parameter, "expected parameter symbol for EO_UI_COCKPIT") do return
	if !testing.expect(t, eo_sym.type_info != nil, "expected type for EO_UI_COCKPIT") do return
	eo_type_upper := strings.to_upper(symbols.format_type(eo_sym.type_info), context.temp_allocator)
	testing.expect(
		t,
		strings.contains(eo_type_upper, "CL_UI_COCKPIT"),
		fmt.tprintf("expected cockpit class in parameter type, got %q", eo_type_upper),
	)

	so_pos := strings.index(source, "= SO_UI_COCKPIT")
	if !testing.expect(t, so_pos >= 0, "expected SO_UI_COCKPIT reference") do return
	so_sym, so_ok := lsp.lookup_symbol_at_offset(
		snap,
		"so_ui_cockpit",
		so_pos + 4,
		snap.symbol_table,
	)
	if !testing.expect(t, so_ok, "expected lookup for class-data in method body") do return
	if !testing.expect(t, so_sym.kind == .Field, "expected field symbol for SO_UI_COCKPIT") do return
	if !testing.expect(t, so_sym.type_info != nil, "expected type for SO_UI_COCKPIT") do return
	so_type_upper := strings.to_upper(symbols.format_type(so_sym.type_info), context.temp_allocator)
	testing.expect(
		t,
		strings.contains(so_type_upper, "CL_UI_COCKPIT"),
		fmt.tprintf("expected cockpit class in class-data type, got %q", so_type_upper),
	)
}

@(test)
hover_nested_constants_usage_includes_value_test :: proc(t: ^testing.T) {
	source :=
		"CLASS zcl_demo DEFINITION\n" +
		"  FINAL\n" +
		"  CREATE PUBLIC .\n" +
		"\n" +
		"PUBLIC SECTION.\n" +
		"  METHODS exec.\n" +
		"\n" +
		"PRIVATE SECTION.\n" +
		"  CONSTANTS:\n" +
		"    BEGIN OF gcs_const_level1,\n" +
		"        BEGIN OF const_level2,\n" +
		"          const_level3_1 TYPE string  VALUE 'VALUE1',\n" +
		"          const_level3_2 TYPE string  VALUE 'VALUE2',\n" +
		"        END OF const_level2,\n" +
		"      END OF gcs_const_level1 .\n" +
		"ENDCLASS.\n" +
		"\n" +
		"CLASS zcl_demo IMPLEMENTATION.\n" +
		"  METHOD exec.\n" +
		"      DATA(lv_some_val) = gcs_const_level1-const_level2-const_level3_1.\n" +
		"  ENDMETHOD.\n" +
		"ENDCLASS.\n"

	snap := make_snapshot(t, source)
	if snap == nil do return
	defer symbols.destroy_symbol_table(snap.symbol_table)

	use_idx := strings.last_index(source, "const_level3_1")
	if !testing.expect(t, use_idx >= 0, "expected usage of const_level3_1") do return
	cursor := use_idx + 8

	field_name, field_type, const_init, ok := lsp.lookup_selector_field_at_offset(
		snap,
		cursor,
		snap.symbol_table,
	)
	if !testing.expect(t, ok, "expected selector field lookup for nested constant") do return
	if !testing.expect(t, field_type != nil, "expected field type") do return
	if !testing.expect(t, const_init != nil, "expected const VALUE on struct field") do return

	hover := lsp.format_field_hover_type_and_const(snap.text, field_name, field_type, const_init)
	hover_u := strings.to_upper(hover, context.temp_allocator)
	testing.expect(
		t,
		strings.contains(hover_u, "CONST_LEVEL3_1") &&
		strings.contains(hover_u, "STRING") &&
		strings.contains(hover_u, "'VALUE1'"),
		fmt.tprintf("expected hover with VALUE, got %q", hover),
	)
}

@(test)
format_field_hover_builtin_constant_shows_literal :: proc(t: ^testing.T) {
	table := symbols.create_empty_symbol_table()
	defer symbols.destroy_symbol_table(table)

	bool_sym, ok := table.symbols["abap_bool"]
	if !testing.expect(t, ok, "abap_bool typedef") do return
	if !testing.expect(t, bool_sym.type_info != nil, "abap_bool type") do return

	h_true := lsp.format_field_hover_type_and_const(
		"",
		"abap_true",
		bool_sym.type_info,
		nil,
		true,
		"'X'",
	)
	testing.expect(t, strings.contains(h_true, "'X'"), fmt.tprintf("abap_true hover: %q", h_true))

	h_false := lsp.format_field_hover_type_and_const(
		"",
		"abap_false",
		bool_sym.type_info,
		nil,
		true,
		"''",
	)
	testing.expect(t, strings.contains(h_false, "''"), fmt.tprintf("abap_false hover: %q", h_false))
}
