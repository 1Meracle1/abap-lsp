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
