package tests_parser

import "../../src/lang/ast"
import "../../src/lang/parser"
import "core:fmt"
import "core:testing"

@(test)
basic_interface_decl_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `INTERFACE i1.
  METHODS m1.
ENDINTERFACE.`


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
		iface, ok := file.decls[0].derived_stmt.(^ast.Interface_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Interface_Decl, got %T", file.decls[0].derived_stmt)) do return

		testing.expect(
			t,
			iface.ident.name == "i1",
			fmt.tprintf("Expected interface name 'i1', got '%s'", iface.ident.name),
		)
		testing.expect(
			t,
			len(iface.methods) == 1,
			fmt.tprintf("Expected 1 method, got %d", len(iface.methods)),
		)

		if len(iface.methods) > 0 {
			method, mok := iface.methods[0].derived_stmt.(^ast.Method_Decl)
			if testing.expect(t, mok, "Expected Method_Decl") {
				testing.expect(
					t,
					method.ident.name == "m1",
					fmt.tprintf("Expected method name 'm1', got '%s'", method.ident.name),
				)
			}
		}
	}
}

@(test)
basic_class_definition_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS c1 DEFINITION.
  PUBLIC SECTION.
    METHODS m1.
ENDCLASS.`


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
		class, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
		if !testing.expect(t, ok, fmt.tprintf("Expected Class_Def_Decl, got %T", file.decls[0].derived_stmt)) do return

		testing.expect(
			t,
			class.ident.name == "c1",
			fmt.tprintf("Expected class name 'c1', got '%s'", class.ident.name),
		)
		testing.expect(
			t,
			len(class.sections) == 1,
			fmt.tprintf("Expected 1 section, got %d", len(class.sections)),
		)

		if len(class.sections) > 0 {
			section := class.sections[0]
			testing.expect(
				t,
				section.access == .Public,
				fmt.tprintf("Expected PUBLIC section, got %v", section.access),
			)
			testing.expect(
				t,
				len(section.methods) == 1,
				fmt.tprintf("Expected 1 method, got %d", len(section.methods)),
			)
		}
	}
}

@(test)
class_definition_with_modifiers_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS c1 DEFINITION ABSTRACT FINAL.
  PUBLIC SECTION.
ENDCLASS.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		class, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
		if !testing.expect(t, ok, "Expected Class_Def_Decl") do return

		testing.expect(t, .Abstract in class.flags, "Expected class to be ABSTRACT")
		testing.expect(t, .Final in class.flags, "Expected class to be FINAL")
	}
}

@(test)
class_definition_inheriting_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS c2 DEFINITION INHERITING FROM c1.
  PUBLIC SECTION.
ENDCLASS.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		class, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
		if !testing.expect(t, ok, "Expected Class_Def_Decl") do return

		testing.expect(
			t,
			class.ident.name == "c2",
			fmt.tprintf("Expected 'c2', got '%s'", class.ident.name),
		)
		testing.expect(t, class.inheriting_from != nil, "Expected INHERITING FROM clause")
		if class.inheriting_from != nil {
			if parent_ident, pok := class.inheriting_from.derived_expr.(^ast.Ident); pok {
				testing.expect(
					t,
					parent_ident.name == "c1",
					fmt.tprintf("Expected parent 'c1', got '%s'", parent_ident.name),
				)
			}
		}
	}
}

@(test)
class_with_multiple_sections_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS cls DEFINITION.
  PUBLIC SECTION.
    DATA attr1 TYPE i.
  PROTECTED SECTION.
    METHODS m1.
  PRIVATE SECTION.
    DATA attr2 TYPE string.
ENDCLASS.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		class, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
		if !testing.expect(t, ok, "Expected Class_Def_Decl") do return

		testing.expect(
			t,
			len(class.sections) == 3,
			fmt.tprintf("Expected 3 sections, got %d", len(class.sections)),
		)

		if len(class.sections) >= 3 {
			testing.expect(
				t,
				class.sections[0].access == .Public,
				"First section should be PUBLIC",
			)
			testing.expect(
				t,
				class.sections[1].access == .Protected,
				"Second section should be PROTECTED",
			)
			testing.expect(
				t,
				class.sections[2].access == .Private,
				"Third section should be PRIVATE",
			)
		}
	}
}

@(test)
class_with_class_data_and_methods_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS cls DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-DATA attr1 TYPE i.
    CLASS-METHODS meth1.
    DATA attr2 TYPE string.
    METHODS meth2.
ENDCLASS.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		class, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
		if !testing.expect(t, ok, "Expected Class_Def_Decl") do return

		testing.expect(t, .Final in class.flags, "Expected class to be FINAL")
		testing.expect(
			t,
			len(class.sections) == 1,
			fmt.tprintf("Expected 1 section, got %d", len(class.sections)),
		)

		if len(class.sections) > 0 {
			section := class.sections[0]
			testing.expect(
				t,
				len(section.data) == 2,
				fmt.tprintf("Expected 2 data declarations, got %d", len(section.data)),
			)
			testing.expect(
				t,
				len(section.methods) == 2,
				fmt.tprintf("Expected 2 method declarations, got %d", len(section.methods)),
			)
		}
	}
}

@(test)
class_with_interfaces_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS c2 DEFINITION.
  PUBLIC SECTION.
    INTERFACES i1.
ENDCLASS.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		class, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
		if !testing.expect(t, ok, "Expected Class_Def_Decl") do return

		if len(class.sections) > 0 {
			section := class.sections[0]
			testing.expect(
				t,
				len(section.interfaces) == 1,
				fmt.tprintf("Expected 1 interface, got %d", len(section.interfaces)),
			)

			if len(section.interfaces) > 0 {
				ifaces, iok := section.interfaces[0].derived_stmt.(^ast.Interfaces_Decl)
				if testing.expect(t, iok, "Expected Interfaces_Decl") {
					testing.expect(
						t,
						len(ifaces.names) == 1,
						fmt.tprintf("Expected 1 interface name, got %d", len(ifaces.names)),
					)
					if len(ifaces.names) > 0 {
						testing.expect(
							t,
							ifaces.names[0].name == "i1",
							fmt.tprintf("Expected 'i1', got '%s'", ifaces.names[0].name),
						)
					}
				}
			}
		}
	}
}

@(test)
class_implementation_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS c1 IMPLEMENTATION.
  METHOD m1.
    DATA lv_temp TYPE i.
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
		if !testing.expect(t, ok, fmt.tprintf("Expected Class_Impl_Decl, got %T", file.decls[0].derived_stmt)) do return

		testing.expect(
			t,
			class_impl.ident.name == "c1",
			fmt.tprintf("Expected 'c1', got '%s'", class_impl.ident.name),
		)
		testing.expect(
			t,
			len(class_impl.methods) == 1,
			fmt.tprintf("Expected 1 method, got %d", len(class_impl.methods)),
		)

		if len(class_impl.methods) > 0 {
			method, mok := class_impl.methods[0].derived_stmt.(^ast.Method_Impl)
			if testing.expect(t, mok, "Expected Method_Impl") {
				if method_ident, iok := method.ident.derived_expr.(^ast.Ident); iok {
					testing.expect(
						t,
						method_ident.name == "m1",
						fmt.tprintf("Expected 'm1', got '%s'", method_ident.name),
					)
				}
				testing.expect(
					t,
					len(method.body) == 1,
					fmt.tprintf("Expected 1 body statement, got %d", len(method.body)),
				)
			}
		}
	}
}

@(test)
class_implementation_interface_method_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS c2 IMPLEMENTATION.
  METHOD i1~m1.
    DATA lv_val TYPE i.
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
				// The method name should be a selector expression (i1~m1 -> i1->m1 with FatArrow)
				// Check it's either a selector or ident
				testing.expect(t, method.ident != nil, "Method ident should not be nil")
			}
		}
	}
}

@(test)
method_with_parameters_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `INTERFACE i1.
  METHODS process
    IMPORTING iv_input TYPE string
    RETURNING VALUE(rv_result) TYPE i.
ENDINTERFACE.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		iface, ok := file.decls[0].derived_stmt.(^ast.Interface_Decl)
		if !testing.expect(t, ok, "Expected Interface_Decl") do return

		if len(iface.methods) > 0 {
			method, mok := iface.methods[0].derived_stmt.(^ast.Method_Decl)
			if testing.expect(t, mok, "Expected Method_Decl") {
				testing.expect(
					t,
					method.ident.name == "process",
					fmt.tprintf("Expected 'process', got '%s'", method.ident.name),
				)
				testing.expect(
					t,
					len(method.params) == 2,
					fmt.tprintf("Expected 2 parameters, got %d", len(method.params)),
				)
			}
		}
	}
}

@(test)
class_methods_sap_helper_style_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS /STTP/CL_HELPER_UTILITIES DEFINITION
  PUBLIC
  CREATE PUBLIC .
  PUBLIC SECTION.
    CLASS-METHODS GET_HTTP_DESTINATION
      IMPORTING
        !IV_LOGSYS TYPE LOGSYS
      EXPORTING
        VALUE(EV_RFCDEST) TYPE RFCDEST
        VALUE(EV_CLIENT) TYPE MANDT .
    CLASS-METHODS DISPLAY_ALV_TABLE
      IMPORTING
        !IV_STRUCTURE_NAME TYPE TABNAME
        !IV_CLIENT_NEVER_DISPLAY TYPE FLAG DEFAULT ABAP_TRUE
        !IV_TITLE TYPE LVC_TITLE DEFAULT TEXT-002
        !IV_START_COLUMN TYPE I OPTIONAL
        !IV_SELECTION_MODE TYPE SALV_DE_CONSTANT DEFAULT IF_SALV_C_SELECTION_MODE=>CELL
      EXPORTING
        !EV_ALV_ROW TYPE SALV_DE_ROW
        VALUE(EV_ALV_COLUMN) TYPE SALV_DE_COLUMN
      CHANGING
        !CT_GRID_DATA TYPE STANDARD TABLE
        !CT_SELECTED_ROWS TYPE SALV_T_ROW OPTIONAL .
ENDCLASS.`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		class, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
		if !testing.expect(t, ok, "Expected Class_Def_Decl") do return

		testing.expect(t, class.ident.name == "/STTP/CL_HELPER_UTILITIES")
		if len(class.sections) == 0 do return
		section := class.sections[0]
		testing.expect(t, len(section.methods) == 2, fmt.tprintf("Expected 2 methods, got %d", len(section.methods)))

		get_http, mok := section.methods[0].derived_stmt.(^ast.Method_Decl)
		if testing.expect(t, mok, "GET_HTTP_DESTINATION Method_Decl") {
			testing.expect(t, .Class in get_http.flags)
			testing.expect(t, get_http.ident.name == "GET_HTTP_DESTINATION")
			testing.expect(t, len(get_http.params) == 3)
			if len(get_http.params) >= 3 {
				testing.expect(t, get_http.params[0].ident.name == "IV_LOGSYS")
				testing.expect(t, get_http.params[0].kind == .Importing)
				testing.expect(t, get_http.params[1].ident.name == "EV_RFCDEST")
				testing.expect(t, get_http.params[1].kind == .Exporting)
				testing.expect(t, get_http.params[2].ident.name == "EV_CLIENT")
				testing.expect(t, get_http.params[2].kind == .Exporting)
			}
		}

		display, dok := section.methods[1].derived_stmt.(^ast.Method_Decl)
		if testing.expect(t, dok, "DISPLAY_ALV_TABLE Method_Decl") {
			testing.expect(t, .Class in display.flags)
			testing.expect(t, len(display.params) == 9, fmt.tprintf("param count %d", len(display.params)))
			// CHANGING generic table: STANDARD TABLE without OF
			last_changing := display.params[8]
			testing.expect(t, last_changing.ident.name == "CT_SELECTED_ROWS")
			testing.expect(t, last_changing.kind == .Changing)
			testing.expect(t, last_changing.optional)

			gen_table_param := display.params[7]
			testing.expect(t, gen_table_param.ident.name == "CT_GRID_DATA")
			tt, tt_ok := gen_table_param.typed.derived_expr.(^ast.Table_Type)
			if testing.expect(t, tt_ok, "STANDARD TABLE type") {
				testing.expect(t, tt.kind == .Standard)
				testing.expect(t, tt.elem == nil, "generic STANDARD TABLE has no line type")
			}
		}
	}
}

@(test)
class_public_constants_struct_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS /sttp/cl_snr_constants DEFINITION
  PUBLIC
  INHERITING FROM /sttp/cl_constants
  CREATE PUBLIC .

  PUBLIC SECTION.
CONSTANTS:
      BEGIN OF gcs_snr_pool_status,
        created   TYPE /sttp/e_snr_status_pool VALUE '1', " Serial Number Pool Created
        active    TYPE /sttp/e_snr_status_pool VALUE '2', " Serial Number Pool Active
        protected TYPE /sttp/e_snr_status_pool VALUE '3', " Serial Number Pool Protected
        closed    TYPE /sttp/e_snr_status_pool VALUE '4', " Serial Number Pool Closed
      END OF gcs_snr_pool_status .
  PROTECTED SECTION.
private section.
ENDCLASS.`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) < 1 {
		return
	}
	class_def, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
	if !testing.expect(t, ok, fmt.tprintf("Expected Class_Def_Decl, got %T", file.decls[0].derived_stmt)) {
		return
	}
	if !testing.expect(t, len(class_def.sections) >= 2, "Expected at least PUBLIC and PROTECTED sections") {
		return
	}
	pub := class_def.sections[0]
	testing.expect(t, pub.access == .Public)
	testing.expect(t, len(pub.data) == 1, fmt.tprintf("Expected 1 data member in PUBLIC, got %d", len(pub.data)))
	if len(pub.data) < 1 {
		return
	}
	cs, cs_ok := pub.data[0].derived_stmt.(^ast.Const_Struct_Decl)
	testing.expect(t, cs_ok, fmt.tprintf("Expected Const_Struct_Decl, got %T", pub.data[0].derived_stmt))
	if cs_ok && cs.ident != nil {
		testing.expect(t, cs.ident.name == "gcs_snr_pool_status")
		testing.expect(t, len(cs.components) == 4)
	}
}

@(test)
class_methods_value_on_importing_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS ZATTP_CL_REP_UTILS DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
PUBLIC SECTION.
CLASS-METHODS PROCESS_ORDER_CREATION
    IMPORTING
      VALUE(IS_GENERAL) TYPE ZATTP_RFC_GENERAL
      VALUE(IT_ORD_HEADER) TYPE ZATTP_T_ORDER_HEADER
      VALUE(IV_LOGSYS) TYPE LOGSYS
      !IV_COMMIT TYPE XFELD
    EXPORTING
      !ES_RETURN TYPE BAPIRET2
      VALUE(ET_RETURN) TYPE BAPIRET2_T
      !ET_ERRORS_HDR TYPE ZATTP_T_DM_TRN_ORD
    CHANGING
      !CO_MESSAGES TYPE REF TO /STTP/CL_MESSAGES .
ENDCLASS.`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		class, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
		if !testing.expect(t, ok, "Expected Class_Def_Decl") do return
		testing.expect(t, class.ident.name == "ZATTP_CL_REP_UTILS")
		if len(class.sections) == 0 do return
		section := class.sections[0]
		if len(section.methods) == 0 do return
		method, mok := section.methods[0].derived_stmt.(^ast.Method_Decl)
		if !testing.expect(t, mok, "Method_Decl") do return
		testing.expect(t, method.ident.name == "PROCESS_ORDER_CREATION")
		testing.expect(t, len(method.params) == 8, fmt.tprintf("param count %d", len(method.params)))
		if len(method.params) >= 8 {
			testing.expect(t, method.params[0].ident.name == "IS_GENERAL")
			testing.expect(t, method.params[0].kind == .Importing)
			testing.expect(t, method.params[1].ident.name == "IT_ORD_HEADER")
			testing.expect(t, method.params[1].kind == .Importing)
			testing.expect(t, method.params[2].ident.name == "IV_LOGSYS")
			testing.expect(t, method.params[2].kind == .Importing)
			testing.expect(t, method.params[3].ident.name == "IV_COMMIT")
			testing.expect(t, method.params[3].kind == .Importing)
			testing.expect(t, method.params[4].ident.name == "ES_RETURN")
			testing.expect(t, method.params[4].kind == .Exporting)
			testing.expect(t, method.params[5].ident.name == "ET_RETURN")
			testing.expect(t, method.params[5].kind == .Exporting)
			testing.expect(t, method.params[6].ident.name == "ET_ERRORS_HDR")
			testing.expect(t, method.params[6].kind == .Exporting)
			testing.expect(t, method.params[7].ident.name == "CO_MESSAGES")
			testing.expect(t, method.params[7].kind == .Changing)
			_, ref_ok := method.params[7].typed.derived_expr.(^ast.Ref_Type)
			testing.expect(t, ref_ok, "CHANGING param should be REF TO type")
		}
	}
}

@(test)
class_method_with_reference_parameters_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS zcl_notifier DEFINITION.
  PUBLIC SECTION.
    METHODS send_notification
      EXPORTING
        !ev_return_code TYPE string
        !ev_response_string TYPE string
        !ev_reason TYPE string
      RAISING
        /sttp/cx_rep_exception.
ENDCLASS.`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		class, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
		if !testing.expect(t, ok, "Expected Class_Def_Decl") do return

		if len(class.sections) > 0 && len(class.sections[0].methods) > 0 {
			method, mok := class.sections[0].methods[0].derived_stmt.(^ast.Method_Decl)
			if !testing.expect(t, mok, "Expected Method_Decl") do return

			testing.expect(t, len(method.params) == 3, fmt.tprintf("Expected 3 parameters, got %d", len(method.params)))
			if len(method.params) >= 3 {
				testing.expect(t, method.params[0].ident.name == "ev_return_code")
				testing.expect(t, method.params[1].ident.name == "ev_response_string")
				testing.expect(t, method.params[2].ident.name == "ev_reason")
				testing.expect(t, method.params[0].kind == .Exporting)
				testing.expect(t, method.params[1].kind == .Exporting)
				testing.expect(t, method.params[2].kind == .Exporting)
			}

			testing.expect(t, len(method.raising) == 1, fmt.tprintf("Expected 1 raising type, got %d", len(method.raising)))
		}
	}
}

@(test)
method_abstract_redefinition_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS c1 DEFINITION ABSTRACT.
  PROTECTED SECTION.
    METHODS m1 ABSTRACT.
ENDCLASS.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		class, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
		if !testing.expect(t, ok, "Expected Class_Def_Decl") do return

		testing.expect(t, .Abstract in class.flags, "Class should be abstract")

		if len(class.sections) > 0 && len(class.sections[0].methods) > 0 {
			method, mok := class.sections[0].methods[0].derived_stmt.(^ast.Method_Decl)
			if testing.expect(t, mok, "Expected Method_Decl") {
				testing.expect(t, .Abstract in method.flags, "Method should be abstract")
			}
		}
	}
}

@(test)
class_with_types_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS cls DEFINITION.
  PUBLIC SECTION.
    TYPES ty_int TYPE i.
ENDCLASS.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		class, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
		if !testing.expect(t, ok, "Expected Class_Def_Decl") do return

		if len(class.sections) > 0 {
			section := class.sections[0]
			testing.expect(
				t,
				len(section.types) == 1,
				fmt.tprintf("Expected 1 type, got %d", len(section.types)),
			)
		}
	}
}

@(test)
full_class_and_interface_example_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `INTERFACE i1.
  METHODS m1.
ENDINTERFACE.

CLASS c1 DEFINITION ABSTRACT.
  PROTECTED SECTION.
    METHODS m1 ABSTRACT.
ENDCLASS.

CLASS c2 DEFINITION INHERITING FROM c1.
  PUBLIC SECTION.
    INTERFACES i1.
    METHODS m2.
  PROTECTED SECTION.
    METHODS m1 REDEFINITION.
ENDCLASS.

CLASS c2 IMPLEMENTATION.
  METHOD m1.
  ENDMETHOD.
  METHOD m2.
  ENDMETHOD.
ENDCLASS.`


	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	testing.expect(
		t,
		len(file.decls) == 4,
		fmt.tprintf("Expected 4 declarations, got %d", len(file.decls)),
	)

	// Check each declaration type
	if len(file.decls) >= 4 {
		// First: INTERFACE i1
		_, ok1 := file.decls[0].derived_stmt.(^ast.Interface_Decl)
		testing.expect(t, ok1, "First decl should be Interface_Decl")

		// Second: CLASS c1 DEFINITION ABSTRACT
		class1, ok2 := file.decls[1].derived_stmt.(^ast.Class_Def_Decl)
		testing.expect(t, ok2, "Second decl should be Class_Def_Decl")
		if ok2 {
			testing.expect(t, .Abstract in class1.flags, "c1 should be abstract")
		}

		// Third: CLASS c2 DEFINITION INHERITING FROM c1
		class2, ok3 := file.decls[2].derived_stmt.(^ast.Class_Def_Decl)
		testing.expect(t, ok3, "Third decl should be Class_Def_Decl")
		if ok3 {
			testing.expect(t, class2.inheriting_from != nil, "c2 should inherit from c1")
		}

		// Fourth: CLASS c2 IMPLEMENTATION
		_, ok4 := file.decls[3].derived_stmt.(^ast.Class_Impl_Decl)
		testing.expect(t, ok4, "Fourth decl should be Class_Impl_Decl")
	}
}

@(test)
class_definition_create_private :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS lcl_sn_reset DEFINITION CREATE PRIVATE.
ENDCLASS.`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		class, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
		if !testing.expect(t, ok, "Expected Class_Def_Decl") do return

		testing.expect(
			t,
			class.create_kind == .Private,
			fmt.tprintf("Expected create_kind == .Private, got %v", class.create_kind),
		)
	}
}

@(test)
class_definition_create_default :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS lcl_sn_reset DEFINITION.
ENDCLASS.`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		class, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
		if !testing.expect(t, ok, "Expected Class_Def_Decl") do return

		testing.expect(
			t,
			class.create_kind == .Public,
			fmt.tprintf("Expected create_kind == .Public, got %v", class.create_kind),
		)
	}
}

@(test)
class_definition_with_full_header_options_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS zcl_my_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  ABSTRACT
  SHARED MEMORY
  INHERITING FROM zcl_super_class
  FRIENDS zcl_friend1 zcl_friend2.
ENDCLASS.`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		class, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
		if !testing.expect(t, ok, "Expected Class_Def_Decl") do return

		testing.expect(t, class.visibility == .Public)
		testing.expect(t, .Final in class.flags, "Expected class to be FINAL")
		testing.expect(t, .Abstract in class.flags, "Expected class to be ABSTRACT")
		testing.expect(
			t,
			.Shared_Memory in class.flags,
			"Expected class to be marked as SHARED MEMORY",
		)
		testing.expect(t, class.create_kind == .Public)
		testing.expect(t, class.inheriting_from != nil, "Expected INHERITING FROM clause")
		testing.expect(
			t,
			len(class.friends) == 2,
			fmt.tprintf("Expected 2 friends, got %d", len(class.friends)),
		)
	}
}

@(test)
class_definition_for_behavior_with_global_friends_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS lhc_behavior_handler DEFINITION
  FOR BEHAVIOR OF zi_travel
  GLOBAL FRIENDS zcl_behavior_test.
ENDCLASS.`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		class, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
		if !testing.expect(t, ok, "Expected Class_Def_Decl") do return

		testing.expect(t, class.behavior_of != nil, "Expected FOR BEHAVIOR OF clause")
		testing.expect(t, class.global_friends, "Expected GLOBAL FRIENDS flag")
		testing.expect(
			t,
			len(class.friends) == 1,
			fmt.tprintf("Expected 1 friend, got %d", len(class.friends)),
		)
	}
}

@(test)
class_definition_multiple_methods :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS lcl_sn_reset DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor,
      dispose,
      handle_add_serial,
      handle_reset,
      handle_delete,
      display_serdet_alv,
      display_hier_alv,
      prepare_serdet_field_cat,
      prepare_hier_field_cat.
ENDCLASS.`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		class, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
		testing.expect(t, ok, "Expected Class_Def_Decl")

		testing.expect(
			t,
			len(class.sections) == 1,
			fmt.tprintf("Expected one class section, actual %d", len(class.sections)),
		)

		section := class.sections[0]
		testing.expect(
			t,
			section.access == .Public,
			fmt.tprintf("Expected public class section, actual %v", section.access),
		)

		testing.expect(
			t,
			len(section.methods) == 1,
			fmt.tprintf(
				"Expected one method stmt in public class section, actual %d",
				len(section.methods),
			),
		)
		method_chain_decl, method_chain_decl_ok := section.methods[0].derived_stmt.(^ast.Method_Chain_Decl)
		testing.expect(t, method_chain_decl_ok, "Expected Method_Chain_Decl")
		testing.expect(
			t,
			len(method_chain_decl.decls) == 9,
			fmt.tprintf(
				"Expected 9 methods in Method_Chain_Decl, actual %d",
				len(method_chain_decl.decls),
			),
		)
	}
}

@(test)
class_for_testing_definition_duration_risk :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS ltcl_testing_class DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  INHERITING FROM c1.
ENDCLASS.`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		class, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
		testing.expect(t, ok, "Expected Class_Def_Decl")

		testing.expect(t, .Testing in class.flags, "Expected class destined for testing")
		testing.expect(t, class.duration == .Short)
		testing.expect(t, class.risk_level == .Harmless)

		testing.expect(t, class.inheriting_from != nil, "Expected INHERITING FROM clause")
		if class.inheriting_from != nil {
			if parent_ident, pok := class.inheriting_from.derived_expr.(^ast.Ident); pok {
				testing.expect(
					t,
					parent_ident.name == "c1",
					fmt.tprintf("Expected parent 'c1', got '%s'", parent_ident.name),
				)
			}
		}
	}
}

@(test)
class_for_testing_with_methods_for_testing :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS ltcl_testing_class DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
        setup,
        teardown,
        test_n1 FOR TESTING,
        test_n2 FOR TESTING.
ENDCLASS.`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	if len(file.decls) > 0 {
		class, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
		testing.expect(t, ok, "Expected Class_Def_Decl")

		testing.expect(t, .Testing in class.flags, "Expected class destined for testing")
		testing.expect(t, class.duration == .Short)
		testing.expect(t, class.risk_level == .Harmless)

		testing.expect(t, len(class.sections) == 1)
		section := class.sections[0]
		testing.expect(t, len(section.methods) == 1)
		method_chain_decl, method_chain_decl_ok := section.methods[0].derived_stmt.(^ast.Method_Chain_Decl)
		testing.expect(t, len(method_chain_decl.decls) == 4)

		{
			method_decl, method_decl_ok := method_chain_decl.decls[0].derived_stmt.(^ast.Method_Decl)
			testing.expect(t, method_decl_ok)
            testing.expect(t, method_decl.ident.name == "setup")
            testing.expect(t, method_decl.flags == {}, fmt.tprintf("Expected empty flags for setup method, actual %v", method_decl.flags))
		}
		{
			method_decl, method_decl_ok := method_chain_decl.decls[1].derived_stmt.(^ast.Method_Decl)
			testing.expect(t, method_decl_ok)
            testing.expect(t, method_decl.ident.name == "teardown")
            testing.expect(t, method_decl.flags == {})
            testing.expect(t, method_decl.flags == {}, fmt.tprintf("Expected empty flags for teardown method, actual %v", method_decl.flags))
		}
		{
			method_decl, method_decl_ok := method_chain_decl.decls[2].derived_stmt.(^ast.Method_Decl)
			testing.expect(t, method_decl_ok)
            testing.expect(t, method_decl.ident.name == "test_n1")
            testing.expect(t, .Testing in method_decl.flags)
		}
		{
			method_decl, method_decl_ok := method_chain_decl.decls[3].derived_stmt.(^ast.Method_Decl)
			testing.expect(t, method_decl_ok)
            testing.expect(t, method_decl.ident.name == "test_n2")
            testing.expect(t, .Testing in method_decl.flags)
		}
	}
}

@(test)
class_definition_exception_like_params_and_class_methods_raising :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.src = `CLASS /STTP/CX_RR_RU_REST_CLIENT DEFINITION
  PUBLIC
  INHERITING FROM /STTP/CX_BASE_EXCEPTION
  FINAL
  CREATE PUBLIC .

PUBLIC SECTION.

  METHODS CONSTRUCTOR
    IMPORTING
      !TEXTID LIKE TEXTID OPTIONAL
      !PREVIOUS LIKE PREVIOUS OPTIONAL
      !MESSAGES TYPE REF TO /STTP/CL_MESSAGES OPTIONAL
      !MESSAGE TYPE BAL_S_MSG OPTIONAL
      !MESSAGE_TEXT TYPE BAPI_MSG OPTIONAL
      !RETURNCODE TYPE INT2 OPTIONAL .
  CLASS-METHODS RAISE_FROM_CX
    IMPORTING
      !IO_PREVIOUS TYPE REF TO CX_ROOT
    RAISING
      /STTP/CX_RR_RU_REST_CLIENT .
  CLASS-METHODS RAISE_WITH_SY_MSG
    RAISING
      /STTP/CX_RR_RU_REST_CLIENT .
  METHODS ADD_MESSAGE_FROM_EXCEPTION
    IMPORTING
      !IO_MESSAGES TYPE REF TO /STTP/CL_MESSAGES .
PROTECTED SECTION.
PRIVATE SECTION.
ENDCLASS.`

	p: parser.Parser
	parser.parse_file(&p, file)

	testing.expect(
		t,
		len(file.syntax_errors) == 0,
		fmt.tprintf("Unexpected syntax errors: %v", file.syntax_errors),
	)

	class, ok := file.decls[0].derived_stmt.(^ast.Class_Def_Decl)
	if !testing.expect(t, ok, "Class_Def_Decl") do return

	testing.expect(t, .Final in class.flags)
	testing.expect(t, len(class.sections) == 3)
	testing.expect(t, class.sections[0].access == .Public)
	testing.expect(t, len(class.sections[0].methods) == 4)

	ctor, ctor_ok := class.sections[0].methods[0].derived_stmt.(^ast.Method_Decl)
	if !testing.expect(t, ctor_ok, "CONSTRUCTOR Method_Decl") do return
	testing.expect(t, ctor.ident.name == "CONSTRUCTOR")
	testing.expect(t, len(ctor.params) == 6)

	for i in 0 ..< 6 {
		testing.expect(t, ctor.params[i].kind == .Importing)
	}

	p0 := ctor.params[0]
	testing.expect(t, p0.ident.name == "TEXTID")
	testing.expect(t, p0.likes != nil && p0.typed == nil)
	if id, ik := p0.likes.derived_expr.(^ast.Ident); ik {
		testing.expect(t, id.name == "TEXTID")
	}
	testing.expect(t, p0.optional)

	p1 := ctor.params[1]
	testing.expect(t, p1.ident.name == "PREVIOUS")
	testing.expect(t, p1.likes != nil && p1.typed == nil)

	p2 := ctor.params[2]
	testing.expect(t, p2.ident.name == "MESSAGES")
	testing.expect(t, p2.typed != nil && p2.likes == nil)

	raise_cx, rcx_ok := class.sections[0].methods[1].derived_stmt.(^ast.Method_Decl)
	if !testing.expect(t, rcx_ok, "RAISE_FROM_CX") do return
	testing.expect(t, raise_cx.ident.name == "RAISE_FROM_CX")
	testing.expect(t, .Class in raise_cx.flags)
	testing.expect(t, len(raise_cx.params) == 1)
	testing.expect(t, raise_cx.params[0].ident.name == "IO_PREVIOUS")
	testing.expect(t, len(raise_cx.raising) == 1)

	raise_sy, rsy_ok := class.sections[0].methods[2].derived_stmt.(^ast.Method_Decl)
	if !testing.expect(t, rsy_ok, "RAISE_WITH_SY_MSG") do return
	testing.expect(t, raise_sy.ident.name == "RAISE_WITH_SY_MSG")
	testing.expect(t, .Class in raise_sy.flags)
	testing.expect(t, len(raise_sy.params) == 0)
	testing.expect(t, len(raise_sy.raising) == 1)

	add_msg, am_ok := class.sections[0].methods[3].derived_stmt.(^ast.Method_Decl)
	if !testing.expect(t, am_ok, "ADD_MESSAGE_FROM_EXCEPTION") do return
	testing.expect(t, add_msg.ident.name == "ADD_MESSAGE_FROM_EXCEPTION")
	testing.expect(t, len(add_msg.params) == 1)
}
