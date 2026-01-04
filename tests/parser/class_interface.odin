package tests_parser

import "../../src/lang/ast"
import "../../src/lang/parser"
import "core:fmt"
import "core:testing"

@(test)
basic_interface_decl_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
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
	file.fullpath = "test.abap"
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
	file.fullpath = "test.abap"
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
	file.fullpath = "test.abap"
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
	file.fullpath = "test.abap"
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
	file.fullpath = "test.abap"
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
	file.fullpath = "test.abap"
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
	file.fullpath = "test.abap"
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
	file.fullpath = "test.abap"
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
	file.fullpath = "test.abap"
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
method_abstract_redefinition_test :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
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
	file.fullpath = "test.abap"
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
	file.fullpath = "test.abap"
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
	file.fullpath = "test.abap"
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
	file.fullpath = "test.abap"
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
class_definition_multiple_methods :: proc(t: ^testing.T) {
	file := ast.new(ast.File, {})
	file.fullpath = "test.abap"
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
	file.fullpath = "test.abap"
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
	file.fullpath = "test.abap"
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
