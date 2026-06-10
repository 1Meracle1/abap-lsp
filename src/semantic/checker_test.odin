package abap_frontend_semantic2

import "src:ast"
import "src:parser"
import string_interner "src:string_interner"
import "src:tokenizer"

import "core:testing"

checker_test_check_source :: proc(
	t: ^testing.T,
	project: ^Project,
	source: string,
	path := "mem://semantic2_test.abap",
) -> (Checker, ^Project_File) {
	parsed := parser.parse(source, path, context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)

	checker := checker_make(project)
	file := checker_add_file(&checker, parsed.path, parsed.root)
	checker_check_file(&checker, file)
	return checker, file
}

checker_test_lookup :: proc(
	t: ^testing.T,
	project: ^Project,
	scope: ^Scope,
	namespace: Namespace,
	name: string,
	kind: Entity_Kind,
) -> ^Entity {
	interned := string_interner.insert(project.interner, name)
	_, entity, ok := checker_lookup_declaration_from_scope(scope, namespace, interned)
	testing.expect(t, ok)
	if ok {
		testing.expect_value(t, entity.kind, kind)
	}
	return entity
}

checker_test_find_scope_entity :: proc(
	t: ^testing.T,
	project: ^Project,
	scope: ^Scope,
	name: string,
	kind: Entity_Kind,
) -> ^Entity {
	interned := string_interner.insert(project.interner, name)
	for entity in scope.declarations {
		if entity.name == interned && entity.kind == kind {
			return entity
		}
	}
	testing.expect(t, false)
	return nil
}

checker_test_structure_field :: proc(
	t: ^testing.T,
	project: ^Project,
	structure: ^Structure,
	name: string,
) -> ^Entity {
	interned := string_interner.insert(project.interner, name)
	field, ok := checker_lookup_structure_field(structure, interned)
	testing.expect(t, ok)
	return field if ok else nil
}

checker_test_type_name :: proc(project: ^Project, typ: ^Type) -> string {
	if typ == nil || !string_interner.is_valid(typ.name) {
		return ""
	}
	return string_interner.load(project.interner, typ.name)
}

checker_test_diagnostic_count :: proc(checker: ^Checker, kind: Checker_Diagnostic_Kind) -> int {
	count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind == kind {
			count += 1
		}
	}
	return count
}

checker_test_unresolved_candidate_count :: proc(
	checker: ^Checker,
	project: ^Project,
	kind: External_Candidate_Kind,
	name: string,
) -> int {
	interned := checker_intern_name(project, name)
	count := 0
	for candidate in checker.info.unresolved {
		if candidate.kind == kind && candidate.name == interned {
			count += 1
		}
	}
	return count
}

checker_test_unresolved_candidate_namespace_count :: proc(
	checker: ^Checker,
	project: ^Project,
	kind: External_Candidate_Kind,
	namespace: Namespace,
	name: string,
) -> int {
	interned := checker_intern_name(project, name)
	count := 0
	for candidate in checker.info.unresolved {
		if candidate.kind == kind && candidate.namespace == namespace && candidate.name == interned {
			count += 1
		}
	}
	return count
}

checker_test_expr_info_for_node :: proc(
	t: ^testing.T,
	checker: ^Checker,
	node: ^ast.Node,
) -> (Checker_Expr_Info, bool) {
	for record in checker.info.expr_infos {
		if record.node == node {
			return record.info, true
		}
	}
	testing.expect(t, false)
	return {}, false
}

checker_test_find_text :: proc(source, needle: string) -> int {
	if needle == "" || len(needle) > len(source) {
		return -1
	}
	for i in 0 ..= len(source) - len(needle) {
		if source[i:i + len(needle)] == needle {
			return i
		}
	}
	return -1
}

checker_test_find_text_last :: proc(source, needle: string) -> int {
	if needle == "" || len(needle) > len(source) {
		return -1
	}
	found := -1
	for i in 0 ..= len(source) - len(needle) {
		if source[i:i + len(needle)] == needle {
			found = i
		}
	}
	return found
}

@(test)
root_semantic_checker_creates_builtin_and_file_scopes :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	checker := checker_make(&project)
	file := checker_add_file(&checker, "ZPROG.abap")

	testing.expect(t, checker.info.builtin_scope != nil)
	testing.expect_value(t, checker.info.builtin_scope.kind, Scope_Kind.Builtin)
	testing.expect(t, checker.builtin_context.scope == checker.info.builtin_scope)

	testing.expect(t, file.root_scope != nil)
	testing.expect_value(t, file.root_scope.kind, Scope_Kind.File)
	testing.expect(t, file.root_scope.parent == checker.info.builtin_scope)
	testing.expect(t, checker.info.builtin_scope.head_child == file.root_scope)
	testing.expect_value(t, len(checker.info.files), 1)
}

@(test)
root_semantic_entity_kind_namespace_occupancy :: proc(t: ^testing.T) {
	testing.expect(t, entity_kind_occupies(.Type_Def, .Type))
	testing.expect(t, !entity_kind_occupies(.Type_Def, .Value))
	testing.expect(t, entity_kind_occupies(.Builtin, .Routine))
	testing.expect(t, !entity_kind_occupies(.Builtin, .Type))
	testing.expect(t, entity_kind_occupies(.Field, .Value))
	testing.expect(t, entity_kind_occupies(.Variable, .Value))
	testing.expect(t, entity_kind_occupies(.Report, .Value))
	testing.expect(t, entity_kind_occupies(.Method, .Routine))
}

@(test)
root_semantic_builtin_registry_registers_project_owned_entities :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	checker := checker_make(&project)

	type_names := [?]string{"i", "%_c_pointer", "simple", "numeric", "any table", "abap_bool", "syst"}
	for name in type_names {
		entity, ok := checker_lookup_builtin_entity(&checker, .Type, name)
		testing.expect(t, ok)
		testing.expect(t, entity != nil)
		if entity == nil {
			continue
		}
		testing.expect_value(t, entity.kind, Entity_Kind.Type_Def)
		testing.expect(t, .Builtin in entity.flags)
		testing.expect(t, entity.source_file == nil)
		testing.expect(t, entity.scope == checker.info.builtin_scope)
		testing.expect_value(t, entity.state, Entity_State.Resolved)
		testing.expect(t, entity.type != nil)
		testing.expect(t, entity.decl_info != nil)
		testing.expect_value(t, entity.decl_info.state, Decl_Info_State.Resolved)
	}

	abap_bool, abap_bool_ok := checker_lookup_builtin_entity(&checker, .Type, "abap_bool")
	testing.expect(t, abap_bool_ok)
	if abap_bool_ok {
		testing.expect(t, abap_bool.type != nil)
		if abap_bool.type != nil {
			testing.expect_value(t, abap_bool.type.kind, Type_Kind.Named)
			testing.expect(t, abap_bool.type.base != nil)
			if abap_bool.type.base != nil {
				testing.expect_value(t, string_interner.load(project.interner, abap_bool.type.base.name), "c")
			}
		}
	}

	abap_true, abap_true_ok := checker_lookup_builtin_entity(&checker, .Value, "abap_true")
	testing.expect(t, abap_true_ok)
	if abap_true_ok {
		testing.expect_value(t, abap_true.kind, Entity_Kind.Constant)
		testing.expect(t, .Builtin in abap_true.flags)
		payload, payload_ok := abap_true.payload.(^Entity_Constant_Payload)
		testing.expect(t, payload_ok)
		if payload_ok {
			value, value_ok := payload.constant_value.(^Constant_Text_Value)
			testing.expect(t, value_ok)
			if value_ok {
				testing.expect_value(t, value.value, "X")
			}
		}
		testing.expect(t, abap_true.type != nil)
		if abap_true.type != nil {
			testing.expect_value(t, string_interner.load(project.interner, abap_true.type.name), "abap_bool")
		}
	}

	abap_func_exporting, abap_func_exporting_ok := checker_lookup_builtin_entity(&checker, .Value, "abap_func_exporting")
	testing.expect(t, abap_func_exporting_ok)
	if abap_func_exporting_ok {
		testing.expect_value(t, abap_func_exporting.kind, Entity_Kind.Constant)
		testing.expect(t, .Builtin in abap_func_exporting.flags)
		payload, payload_ok := abap_func_exporting.payload.(^Entity_Constant_Payload)
		testing.expect(t, payload_ok)
		if payload_ok {
			value, value_ok := payload.constant_value.(^Constant_Integer_Value)
			testing.expect(t, value_ok)
			if value_ok {
				testing.expect_value(t, value.value, 10)
			}
		}
		testing.expect(t, abap_func_exporting.type != nil)
		if abap_func_exporting.type != nil {
			testing.expect_value(t, string_interner.load(project.interner, abap_func_exporting.type.name), "i")
		}
	}

	sy, sy_ok := checker_lookup_builtin_entity(&checker, .Value, "sy")
	testing.expect(t, sy_ok)
	if sy_ok {
		testing.expect_value(t, sy.kind, Entity_Kind.Variable)
		testing.expect(t, .Builtin in sy.flags)
		payload, payload_ok := sy.payload.(^Entity_Variable_Payload)
		testing.expect(t, payload_ok)
		testing.expect(t, payload != nil)
		testing.expect(t, sy.type != nil)
	}

	strlen, strlen_ok := checker_lookup_builtin_entity(&checker, .Routine, "strlen")
	testing.expect(t, strlen_ok)
	if strlen_ok {
		testing.expect_value(t, strlen.kind, Entity_Kind.Builtin)
		payload, payload_ok := strlen.payload.(^Entity_Builtin_Payload)
		testing.expect(t, payload_ok)
		if payload_ok {
			testing.expect_value(t, Builtin_Proc_Id(payload.id), Builtin_Proc_Id.Strlen)
			testing.expect_value(t, payload.docs, "Number of characters in a text value.")
		}
		testing.expect(t, strlen.type != nil)
		testing.expect_value(t, strlen.type.kind, Type_Kind.Routine)
		testing.expect(t, strlen.type.base != nil)
		testing.expect_value(t, string_interner.load(project.interner, strlen.type.base.name), "i")
	}

	nmin, nmin_ok := checker_builtin_proc_metadata_by_name("nmin")
	testing.expect(t, nmin_ok)
	if nmin_ok {
		testing.expect_value(t, len(nmin.params), 9)
		testing.expect_value(t, nmin.params[0].name, "val1")
		testing.expect(t, nmin.supports_named_args)
	}
}

@(test)
root_semantic_builtin_structures_create_project_owned_fields :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	checker := checker_make(&project)
	syst, syst_ok := checker_lookup_builtin_entity(&checker, .Type, "syst")
	screen, screen_ok := checker_lookup_builtin_entity(&checker, .Type, "screen")
	testing.expect(t, syst_ok)
	testing.expect(t, screen_ok)

	syst_payload: ^Entity_Type_Name_Payload
	screen_payload: ^Entity_Type_Name_Payload
	if syst_ok {
		payload, payload_ok := syst.payload.(^Entity_Type_Name_Payload)
		testing.expect(t, payload_ok)
		if payload_ok {
			syst_payload = payload
		}
	}
	if screen_ok {
		payload, payload_ok := screen.payload.(^Entity_Type_Name_Payload)
		testing.expect(t, payload_ok)
		if payload_ok {
			screen_payload = payload
		}
	}
	testing.expect(t, syst_payload != nil && syst_payload.structure != nil)
	testing.expect(t, screen_payload != nil && screen_payload.structure != nil)
	if syst_payload == nil || syst_payload.structure == nil || screen_payload == nil || screen_payload.structure == nil {
		return
	}

	subrc_name := string_interner.insert(project.interner, "subrc")
	subrc, subrc_ok := checker_lookup_structure_field(syst_payload.structure, subrc_name)
	testing.expect(t, subrc_ok)
	if subrc_ok {
		testing.expect_value(t, subrc.kind, Entity_Kind.Field)
		testing.expect(t, .Builtin in subrc.flags)
		field, field_ok := subrc.payload.(^Entity_Field_Payload)
		testing.expect(t, field_ok)
		if field_ok {
			testing.expect(t, .Has_Type_Ref in field.flags)
			testing.expect_value(t, string_interner.load(project.interner, field.type_ref.base_name), "i")
			testing.expect_value(t, checker_builtin_structure_field_description("syst", "subrc"), "Return code set by many ABAP statements; 0 usually indicates success for the documented statement.")
		}
	}

	screen_name_key := string_interner.insert(project.interner, "name")
	screen_name, screen_name_ok := checker_lookup_structure_field(screen_payload.structure, screen_name_key)
	testing.expect(t, screen_name_ok)
	if screen_name_ok {
		testing.expect_value(t, screen_name.kind, Entity_Kind.Field)
		testing.expect(t, .Builtin in screen_name.flags)
		field, field_ok := screen_name.payload.(^Entity_Field_Payload)
		testing.expect(t, field_ok)
		if field_ok {
			testing.expect_value(t, string_interner.load(project.interner, field.type_ref.base_name), "c")
			testing.expect_value(t, checker_builtin_structure_field_description("screen", "name"), "Name of the current dynpro field or screen element.")
		}
	}
}

@(test)
root_semantic_builtin_type_refs_and_calls_resolve_through_registered_entities :: proc(t: ^testing.T) {
	source := `DATA gv_ptr TYPE %_C_POINTER.
DATA gv_len TYPE i.
FORM run.
  gv_len = strlen( 'abc' ).
ENDFORM.`

	parsed := parser.parse(source, "mem://builtin_use.abap", context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)

	project := project_make()
	defer project_destroy(&project)

	checker := checker_make(&project)
	file := checker_add_file(&checker, parsed.path, parsed.root)
	checker_check_file(&checker, file)

	pointer, pointer_ok := checker_lookup_builtin_entity(&checker, .Type, "%_c_pointer")
	testing.expect(t, pointer_ok)
	if pointer_ok {
		testing.expect(t, .Used in pointer.flags)
	}

	strlen, strlen_ok := checker_lookup_builtin_entity(&checker, .Routine, "strlen")
	testing.expect(t, strlen_ok)
	if strlen_ok {
		testing.expect(t, .Used in strlen.flags)
	}

	form, form_ok := parsed.root.stmts[2].derived_stmt.(^ast.Form_Decl)
	testing.expect(t, form_ok)
	if !form_ok {
		return
	}
	assign, assign_ok := form.body[0].derived_stmt.(^ast.Assign_Stmt)
	testing.expect(t, assign_ok)
	if !assign_ok {
		return
	}
	_, call_ok := assign.rhs.derived_expr.(^ast.Call_Expr)
	testing.expect(t, call_ok)

	call_info_found := false
	for record in checker.info.expr_infos {
		if record.node != &assign.rhs.expr_base {
			continue
		}
		call_info_found = true
		testing.expect_value(t, record.info.mode, ast.Addressing_Mode.Value)
		testing.expect(t, record.info.type != nil)
		if record.info.type != nil {
			testing.expect_value(t, string_interner.load(project.interner, record.info.type.name), "i")
		}
	}
	testing.expect(t, call_info_found)
}

@(test)
root_semantic_type_checker_resolves_declared_type_shapes :: proc(t: ^testing.T) {
	source := `CLASS lcl_demo DEFINITION.
ENDCLASS.
INTERFACE lif_demo.
ENDINTERFACE.
TYPES: BEGIN OF ty_line,
         text TYPE string,
       END OF ty_line.
TYPES ty_lines TYPE STANDARD TABLE OF ty_line WITH DEFAULT KEY.
DATA ls_line TYPE ty_line.
DATA lt_lines TYPE ty_lines.
DATA lr_demo TYPE REF TO lcl_demo.
DATA lr_if TYPE REF TO lif_demo.`

	project := project_make()
	defer project_destroy(&project)

	_, file := checker_test_check_source(t, &project, source, "mem://type_shapes.abap")

	ty_line := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_line", .Type_Def)
	ty_lines := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_lines", .Type_Def)
	ls_line := checker_test_lookup(t, &project, file.root_scope, .Value, "ls_line", .Variable)
	lt_lines := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_lines", .Variable)
	lr_demo := checker_test_lookup(t, &project, file.root_scope, .Value, "lr_demo", .Variable)
	lr_if := checker_test_lookup(t, &project, file.root_scope, .Value, "lr_if", .Variable)
	testing.expect(t, ty_line != nil && ty_lines != nil && ls_line != nil && lt_lines != nil)
	testing.expect(t, lr_demo != nil && lr_if != nil)
	if ty_line == nil || ty_lines == nil || ls_line == nil || lt_lines == nil || lr_demo == nil || lr_if == nil {
		return
	}

	testing.expect_value(t, ty_line.type.kind, Type_Kind.Named)
	line_structure := checker_type_structure(ty_line.type)
	testing.expect(t, line_structure != nil)
	if line_structure != nil {
		text := checker_test_structure_field(t, &project, line_structure, "text")
		testing.expect(t, text != nil && text.type != nil)
		if text != nil && text.type != nil {
			testing.expect_value(t, text.type.kind, Type_Kind.Builtin)
			testing.expect_value(t, checker_test_type_name(&project, text.type), "string")
		}
	}
	testing.expect(t, checker_type_same(ls_line.type, ty_line.type))

	testing.expect_value(t, ty_lines.type.kind, Type_Kind.Named)
	testing.expect(t, ty_lines.type.base != nil)
	if ty_lines.type.base != nil {
		testing.expect_value(t, ty_lines.type.base.kind, Type_Kind.Table)
		testing.expect_value(t, ty_lines.type.base.table_form, ast.Data_Type_Form.Standard_Table)
		testing.expect(t, checker_type_same(ty_lines.type.base.base, ty_line.type))
	}
	testing.expect(t, checker_type_same(lt_lines.type, ty_lines.type))

	testing.expect_value(t, lr_demo.type.kind, Type_Kind.Ref)
	if lr_demo.type.base != nil {
		testing.expect_value(t, lr_demo.type.base.kind, Type_Kind.Class)
		testing.expect_value(t, checker_test_type_name(&project, lr_demo.type.base), "lcl_demo")
	}
	testing.expect_value(t, lr_if.type.kind, Type_Kind.Ref)
	if lr_if.type.base != nil {
		testing.expect_value(t, lr_if.type.base.kind, Type_Kind.Interface)
		testing.expect_value(t, checker_test_type_name(&project, lr_if.type.base), "lif_demo")
	}
}

@(test)
root_semantic_type_checker_diagnoses_unresolved_structure_component_type_refs :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_input_po,
         sort_idx  TYPE i,
         ebeln     TYPE ekpo-ebeln,
         vendor_po TYPE /sttpec/e_docnum,
       END OF ty_input_po.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://unresolved_structure_component_types.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Type), 2)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_namespace_count(
			&checker,
			&project,
			.Global_Symbol,
			.Type,
			"ekpo",
		),
		1,
	)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_namespace_count(
			&checker,
			&project,
			.Global_Symbol,
			.Type,
			"/sttpec/e_docnum",
		),
		1,
	)

	ekpo_diag := false
	docnum_diag := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Unresolved_Type {
			continue
		}
		text := source[diagnostic.range.start:diagnostic.range.end]
		if text == "ekpo" {
			ekpo_diag = true
		} else if text == "/sttpec/e_docnum" {
			docnum_diag = true
		}
	}
	testing.expect(t, ekpo_diag)
	testing.expect(t, docnum_diag)
}

@(test)
root_semantic_type_checker_bounds_recursive_aliases :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, `TYPES ty_self TYPE ty_self.`, "mem://recursive_type_alias.abap")

	ty_self := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_self", .Type_Def)
	testing.expect(t, ty_self != nil)
	if ty_self == nil {
		return
	}
	testing.expect_value(t, ty_self.state, Entity_State.Failed)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Declaration_Cycle), 1)
	testing.expect(t, ty_self.type != nil)
	if ty_self.type != nil {
		testing.expect_value(t, ty_self.type.kind, Type_Kind.Named)
		testing.expect(t, ty_self.type.base != nil)
		if ty_self.type.base != nil {
			testing.expect_value(t, ty_self.type.base.kind, Type_Kind.Unknown)
		}
	}
}

@(test)
root_semantic_type_checker_expands_structured_include_members :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_base,
         a TYPE i,
       END OF ty_base.
TYPES: BEGIN OF ty_wrap,
         INCLUDE TYPE ty_base,
         b TYPE string,
       END OF ty_wrap.`

	project := project_make()
	defer project_destroy(&project)

	_, file := checker_test_check_source(t, &project, source, "mem://structured_include.abap")

	wrap := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_wrap", .Type_Def)
	testing.expect(t, wrap != nil)
	if wrap == nil {
		return
	}
	structure := checker_type_structure(wrap.type)
	testing.expect(t, structure != nil)
	if structure == nil {
		return
	}
	testing.expect_value(t, len(structure.fields), 2)
	testing.expect_value(t, string_interner.load(project.interner, structure.fields[0].name), "a")
	testing.expect_value(t, string_interner.load(project.interner, structure.fields[1].name), "b")
	testing.expect_value(t, checker_test_type_name(&project, structure.fields[0].type), "i")
	testing.expect_value(t, checker_test_type_name(&project, structure.fields[1].type), "string")

	include_name := string_interner.insert(project.interner, "include")
	_, include_found := checker_lookup_structure_field(structure, include_name)
	testing.expect(t, !include_found)
}

@(test)
root_semantic_type_checker_handles_split_structured_include_members :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF etobj_key,
         key TYPE string,
       END OF etobj_key.
TYPES:
  BEGIN OF ty_bus_msg.
  INCLUDE TYPE etobj_key.
TYPES:
  bus_msg_no TYPE c LENGTH 1,
  arbgb TYPE string,
  END OF ty_bus_msg,
  ty_bus_msgs TYPE STANDARD TABLE OF ty_bus_msg.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://split_structured_include.abap")
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Declaration_Cycle), 0)

	bus_msg := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_bus_msg", .Type_Def)
	testing.expect(t, bus_msg != nil)
	if bus_msg == nil {
		return
	}
	structure := checker_type_structure(bus_msg.type)
	testing.expect(t, structure != nil)
	if structure == nil {
		return
	}
	testing.expect_value(t, len(structure.fields), 3)
	testing.expect_value(t, string_interner.load(project.interner, structure.fields[0].name), "key")
	testing.expect_value(t, string_interner.load(project.interner, structure.fields[1].name), "bus_msg_no")
	testing.expect_value(t, string_interner.load(project.interner, structure.fields[2].name), "arbgb")
}

@(test)
root_semantic_type_checker_resolves_structured_components_named_begin_and_end :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_code_range,
         begin TYPE i,
         end TYPE i,
       END OF ty_code_range.
TYPES ty_code_ranges TYPE SORTED TABLE OF ty_code_range WITH UNIQUE KEY begin.
DATA lt_ranges TYPE ty_code_ranges.`

	project := project_make()
	defer project_destroy(&project)

	_, file := checker_test_check_source(t, &project, source, "mem://keyword_component_type_refs.abap")

	code_range := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_code_range", .Type_Def)
	code_ranges := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_code_ranges", .Type_Def)
	lt_ranges := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_ranges", .Variable)
	testing.expect(t, code_range != nil && code_ranges != nil && lt_ranges != nil)
	if code_range == nil || code_ranges == nil || lt_ranges == nil {
		return
	}
	structure := checker_type_structure(code_range.type)
	testing.expect(t, structure != nil)
	if structure != nil {
		testing.expect_value(t, len(structure.fields), 2)
		testing.expect_value(t, string_interner.load(project.interner, structure.fields[0].name), "begin")
		testing.expect_value(t, string_interner.load(project.interner, structure.fields[1].name), "end")
	}
	testing.expect(t, code_ranges.type != nil && code_ranges.type.base != nil)
	if code_ranges.type != nil && code_ranges.type.base != nil {
		testing.expect_value(t, code_ranges.type.base.kind, Type_Kind.Table)
		testing.expect_value(t, code_ranges.type.base.table_form, ast.Data_Type_Form.Sorted_Table)
		testing.expect(t, checker_type_same(code_ranges.type.base.base, code_range.type))
	}
	testing.expect(t, checker_type_same(lt_ranges.type, code_ranges.type))
}

@(test)
root_semantic_type_checker_resolves_like_line_of_and_ranges :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_line,
         text TYPE string,
       END OF ty_line.
TYPES ty_lines TYPE STANDARD TABLE OF ty_line WITH DEFAULT KEY.
TYPES ty_range TYPE RANGE OF string.
DATA lt_lines TYPE ty_lines.
FIELD-SYMBOLS <line> LIKE LINE OF lt_lines.
DATA beket TYPE i.
DATA int_eket LIKE beket OCCURS 0 WITH HEADER LINE.`

	project := project_make()
	defer project_destroy(&project)

	_, file := checker_test_check_source(t, &project, source, "mem://like_line_of.abap")

	ty_line := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_line", .Type_Def)
	ty_range := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_range", .Type_Def)
	line := checker_test_lookup(t, &project, file.root_scope, .Value, "<line>", .Field_Symbol)
	int_eket := checker_test_lookup(t, &project, file.root_scope, .Value, "int_eket", .Variable)
	testing.expect(t, ty_line != nil && ty_range != nil && line != nil && int_eket != nil)
	if ty_line == nil || ty_range == nil || line == nil || int_eket == nil {
		return
	}
	testing.expect(t, checker_type_same(line.type, ty_line.type))
	testing.expect(t, ty_range.type != nil && ty_range.type.base != nil)
	if ty_range.type != nil && ty_range.type.base != nil {
		testing.expect_value(t, ty_range.type.base.kind, Type_Kind.Table)
		testing.expect_value(t, ty_range.type.base.table_form, ast.Data_Type_Form.Range_Of)
		testing.expect_value(t, checker_test_type_name(&project, ty_range.type.base.base), "string")
	}
	testing.expect_value(t, int_eket.type.kind, Type_Kind.Table)
	testing.expect_value(t, int_eket.type.table_form, ast.Data_Type_Form.Like_Table)
	testing.expect_value(t, checker_test_type_name(&project, int_eket.type.base), "i")
}

@(test)
root_semantic_type_checker_resolves_ast_type_ref_paths :: proc(t: ^testing.T) {
	source := `INTERFACE lif_demo.
  TYPES ty_line TYPE i.
ENDINTERFACE.
DATA lv_date LIKE sy-datum.
DATA lr_item TYPE REF TO lif_demo=>ty_line.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://type_ref_paths.abap")

	lv_date := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_date", .Variable)
	lr_item := checker_test_lookup(t, &project, file.root_scope, .Value, "lr_item", .Variable)
	testing.expect(t, lv_date != nil && lr_item != nil)
	if lv_date == nil || lr_item == nil {
		return
	}
	testing.expect_value(t, checker_test_type_name(&project, lv_date.type), "d")
	testing.expect_value(t, lr_item.type.kind, Type_Kind.Ref)
	if lr_item.type.base != nil {
		testing.expect_value(t, checker_test_type_name(&project, lr_item.type.base), "ty_line")
	}

	sy, sy_ok := checker_lookup_builtin_entity(&checker, .Value, "sy")
	testing.expect(t, sy_ok && .Used in sy.flags)
	lif_demo := checker_test_lookup(t, &project, file.root_scope, .Type, "lif_demo", .Interface)
	testing.expect(t, lif_demo != nil && .Used in lif_demo.flags)
}

@(test)
root_semantic_like_clause_adds_type_fallback_candidate :: proc(t: ^testing.T) {
	source := `DATA lv_field LIKE zmissing_table-field.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://like_type_fallback.abap")

	testing.expect_value(
		t,
		checker_test_unresolved_candidate_namespace_count(
			&checker,
			&project,
			.Global_Symbol,
			.Type,
			"zmissing_table",
		),
		1,
	)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_namespace_count(
			&checker,
			&project,
			.Global_Symbol,
			.Value,
			"zmissing_table",
		),
		1,
	)
}

@(test)
root_semantic_like_clause_skips_current_parameter_for_shadowed_member :: proc(t: ^testing.T) {
	source := `CLASS lcl_base DEFINITION.
  PUBLIC SECTION.
    DATA previous TYPE REF TO lcl_base.
ENDCLASS.
CLASS lcl_child DEFINITION INHERITING FROM lcl_base.
  PUBLIC SECTION.
    METHODS constructor IMPORTING previous LIKE previous.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://like_shadowed_parameter.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Declaration_Cycle), 0)
	child := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl_child", .Class)
	testing.expect(t, child != nil)
	if child == nil {
		return
	}
	child_payload := child.payload.(^Entity_Object_Payload)
	constructor := checker_test_lookup(t, &project, child_payload.definition_scope, .Routine, "constructor", .Method)
	constructor_payload := constructor.payload.(^Entity_Routine_Payload)
	param := checker_test_lookup(t, &project, constructor_payload.signature_scope, .Value, "previous", .Parameter)
	testing.expect_value(t, param.type.kind, Type_Kind.Ref)
	if param.type.base != nil {
		testing.expect_value(t, checker_test_type_name(&project, param.type.base), "lcl_base")
	}
}

@(test)
root_semantic_like_clause_skips_current_structure_field_for_outer_member :: proc(t: ^testing.T) {
	source := `CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    DATA a1 TYPE string.
    TYPES: BEGIN OF ty_message_parts,
             a1 LIKE a1,
             a2 LIKE a1,
           END OF ty_message_parts.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://like_shadowed_field.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Declaration_Cycle), 0)
	class := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl_demo", .Class)
	testing.expect(t, class != nil)
	if class == nil {
		return
	}
	class_payload := class.payload.(^Entity_Object_Payload)
	row := checker_test_lookup(t, &project, class_payload.definition_scope, .Type, "ty_message_parts", .Type_Def)
	structure := checker_type_structure(row.type)
	testing.expect(t, structure != nil)
	if structure == nil {
		return
	}
	a1 := checker_test_structure_field(t, &project, structure, "a1")
	a2 := checker_test_structure_field(t, &project, structure, "a2")
	testing.expect_value(t, checker_test_type_name(&project, a1.type), "string")
	testing.expect_value(t, checker_test_type_name(&project, a2.type), "string")
}

@(test)
root_semantic_type_checker_validates_generic_builtin_contexts :: proc(t: ^testing.T) {
	source := `FIELD-SYMBOLS <value> TYPE simple.
FORM demo USING iv_number TYPE numeric CHANGING cv_data TYPE data.
ENDFORM.
DATA lr_data TYPE REF TO data.
DATA lr_object TYPE REF TO object.
DATA lv_simple TYPE simple.
TYPES ty_numeric TYPE numeric.
CONSTANTS c_any TYPE any VALUE IS INITIAL.
DATA lr_simple TYPE REF TO simple.
DATA lo_object TYPE object.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://generic_builtin_contexts.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Generic_Builtin_Type), 4)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Object_Type_Reference), 1)
}

@(test)
root_semantic_checker_context_registers_project_file_and_scoped_blocks :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	checker := checker_make(&project)
	file := project_add_file(&project, "ZRAW.abap")
	testing.expect(t, file.root_scope == nil)

	ctx := checker_context_make(&checker, file)
	testing.expect(t, file.root_scope != nil)
	testing.expect(t, ctx.file == file)
	testing.expect(t, ctx.scope == file.root_scope)
	testing.expect_value(t, len(checker.info.files), 1)

	again := checker_context_make(&checker, file)
	testing.expect(t, again.scope == file.root_scope)
	testing.expect_value(t, len(checker.info.files), 1)

	child := checker_open_scope(&ctx, .Form)
	testing.expect(t, child.parent == file.root_scope)
	testing.expect(t, file.root_scope.head_child == child)
	testing.expect_value(t, len(file.root_scope.children), 1)
	checker_close_scope(&ctx)
	testing.expect(t, ctx.scope == file.root_scope)
}

@(test)
root_semantic_checker_registers_checks_and_records_entity_uses :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	checker := checker_make(&project)
	file := checker_add_file(&checker, "ZPROG.abap")
	ctx := checker_context_make(&checker, file)
	definitions_before := len(checker.info.definitions)
	queue_before := len(checker.info.entity_queue)
	checked_before := len(checker.info.checked_entities)

	name := string_interner.insert(project.interner, "gv_value")
	entity := project_new_entity(&project, .Variable)
	decl := project_new_decl_info(
		&project,
		entity,
		file.root_scope,
		name,
		.Variable,
		tokenizer.text_range(0, 8),
	)

	testing.expect(t, checker_add_entity_and_decl_info(&ctx, entity, decl))
	testing.expect_value(t, len(checker.info.definitions), definitions_before + 1)
	testing.expect_value(t, len(checker.info.entity_queue), queue_before + 1)

	found_scope, found, found_ok := checker_lookup_declaration(&ctx, .Value, name)
	testing.expect(t, found_ok)
	testing.expect(t, found_scope == file.root_scope)
	testing.expect(t, found == entity)

	checker_check_queued_entities(&ctx)
	testing.expect_value(t, len(checker.info.entity_queue), 0)
	testing.expect_value(t, len(checker.info.checked_entities), checked_before + 1)
	testing.expect_value(t, entity.state, Entity_State.Resolved)
	testing.expect_value(t, decl.state, Decl_Info_State.Resolved)
	testing.expect(t, entity.type == project_type_unknown(&project))

	ctx.decl = decl
	checker_add_entity_use(&ctx, nil, entity)
	checker_add_entity_use(&ctx, nil, entity)
	testing.expect(t, .Used in entity.flags)
	testing.expect_value(t, len(checker.info.dependencies), 1)
	testing.expect_value(t, len(checker.info.uses), 2)
}

@(test)
root_semantic_checker_reports_duplicate_declarations :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	checker := checker_make(&project)
	file := checker_add_file(&checker, "ZPROG.abap")
	ctx := checker_context_make(&checker, file)

	name := string_interner.insert(project.interner, "gv_value")
	first := project_new_entity(&project, .Variable)
	first_decl := project_new_decl_info(&project, first, file.root_scope, name, .Variable)
	second := project_new_entity(&project, .Variable)
	second_decl := project_new_decl_info(&project, second, file.root_scope, name, .Variable)

	testing.expect(t, checker_add_entity_and_decl_info(&ctx, first, first_decl))
	testing.expect(t, !checker_add_entity_and_decl_info(&ctx, second, second_decl))
	testing.expect_value(t, len(checker.info.diagnostics), 1)
	testing.expect_value(t, checker.info.diagnostics[0].kind, Checker_Diagnostic_Kind.Duplicate_Declaration)
	testing.expect(t, checker.info.diagnostics[0].entity == second)
}

@(test)
root_semantic_scope_lookup_keeps_namespaces_and_reports_shadowing :: proc(t: ^testing.T) {
	source := `DATA shared TYPE i.
TYPES shared TYPE i.
FORM shared.
ENDFORM.
FORM run.
  DATA shared TYPE i.
  shared = 1.
ENDFORM.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://scope_lookup.abap")

	_ = checker_test_lookup(t, &project, file.root_scope, .Value, "shared", .Variable)
	_ = checker_test_lookup(t, &project, file.root_scope, .Type, "shared", .Type_Def)
	_ = checker_test_lookup(t, &project, file.root_scope, .Routine, "shared", .Form)
	run := checker_test_lookup(t, &project, file.root_scope, .Routine, "run", .Form)
	testing.expect(t, run != nil)
	if run == nil {
		return
	}
	run_payload := run.payload.(^Entity_Routine_Payload)
	name := string_interner.insert(project.interner, "shared")

	_, local_value, value_ok := checker_lookup_declaration_from_scope(run_payload.body_scope, .Value, name)
	testing.expect(t, value_ok)
	if value_ok {
		testing.expect_value(t, local_value.kind, Entity_Kind.Variable)
		testing.expect(t, local_value.scope == run_payload.body_scope)
	}
	_, typ, type_ok := checker_lookup_declaration_from_scope(run_payload.body_scope, .Type, name)
	testing.expect(t, type_ok)
	if type_ok {
		testing.expect_value(t, typ.kind, Entity_Kind.Type_Def)
		testing.expect(t, typ.scope == file.root_scope)
	}

	shadow_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind == .Shadowed_Declaration {
			shadow_count += 1
			testing.expect_value(t, diagnostic.severity, Checker_Diagnostic_Severity.Warning)
			testing.expect(t, diagnostic.entity == local_value)
		}
		testing.expect(t, diagnostic.kind != .Duplicate_Declaration)
	}
	testing.expect_value(t, shadow_count, 1)
}

@(test)
root_semantic_member_scopes_do_not_report_lexical_shadowing :: proc(t: ^testing.T) {
	source := `DATA name TYPE i.
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    DATA name TYPE i.
    TYPES: BEGIN OF ty_row,
             name TYPE i,
           END OF ty_row.
    CONSTANTS: BEGIN OF c_row,
                 name TYPE i VALUE 1,
               END OF c_row.
ENDCLASS.
FORM run.
  DATA name TYPE i.
ENDFORM.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://member_shadowing.abap")

	shadow_count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind == .Shadowed_Declaration {
			shadow_count += 1
			testing.expect_value(t, diagnostic.severity, Checker_Diagnostic_Severity.Warning)
		}
		testing.expect(t, diagnostic.kind != .Duplicate_Declaration)
	}
	testing.expect_value(t, shadow_count, 1)
}

@(test)
root_semantic_scope_lookup_prefers_local_then_imported_then_parent :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	checker := checker_make(&project)
	file := checker_add_file(&checker, "ZPROG.abap")
	ctx := checker_context_make(&checker, file)

	imported := checker_create_scope(&checker, checker.info.builtin_scope, .File)
	append(&file.root_scope.imported, imported)

	name := string_interner.insert(project.interner, "remote_value")
	imported_entity := project_new_entity(&project, .Variable)
	imported_decl := project_new_decl_info(&project, imported_entity, imported, name, .Variable)
	imported_entity.source_file = file
	testing.expect(t, checker_add_entity_and_decl_info(&ctx, imported_entity, imported_decl))

	found_scope, found, found_ok := checker_lookup_declaration_from_scope(file.root_scope, .Value, name)
	testing.expect(t, found_ok)
	testing.expect(t, found_scope == imported)
	testing.expect(t, found == imported_entity)

	local_entity := project_new_entity(&project, .Variable)
	local_decl := project_new_decl_info(&project, local_entity, file.root_scope, name, .Variable)
	testing.expect(t, checker_add_entity_and_decl_info(&ctx, local_entity, local_decl))

	found_scope, found, found_ok = checker_lookup_declaration_from_scope(file.root_scope, .Value, name)
	testing.expect(t, found_ok)
	testing.expect(t, found_scope == file.root_scope)
	testing.expect(t, found == local_entity)
}

@(test)
root_semantic_checker_walks_file_declarations_routine_body_and_expressions :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_line,
         value TYPE i,
       END OF ty_line.
DATA gv TYPE i.
FORM add USING iv TYPE i.
  DATA lv TYPE i.
  lv = iv + gv.
ENDFORM.`

	parsed := parser.parse(source, "mem://zprog.abap", context.allocator)
	testing.expect_value(t, len(parsed.errors), 0)

	project := project_make()
	defer project_destroy(&project)

	checker := checker_make(&project)
	file := checker_add_file(&checker, parsed.path, parsed.root)
	checker_check_file(&checker, file)

	ty_name := string_interner.insert(project.interner, "ty_line")
	_, ty_entity, ty_ok := checker_lookup_declaration_from_scope(file.root_scope, .Type, ty_name)
	testing.expect(t, ty_ok)
	testing.expect_value(t, ty_entity.kind, Entity_Kind.Type_Def)
	testing.expect(t, ty_entity.type != nil)
	testing.expect_value(t, ty_entity.type.kind, Type_Kind.Named)

	type_payload, type_payload_ok := ty_entity.payload.(^Entity_Type_Name_Payload)
	testing.expect(t, type_payload_ok)
	testing.expect(t, type_payload.structure != nil)
	testing.expect_value(t, len(type_payload.structure.fields), 1)
	testing.expect_value(t, type_payload.structure.fields[0].kind, Entity_Kind.Field)

	form_name := string_interner.insert(project.interner, "add")
	_, form_entity, form_ok := checker_lookup_declaration_from_scope(file.root_scope, .Routine, form_name)
	testing.expect(t, form_ok)
	testing.expect_value(t, form_entity.kind, Entity_Kind.Form)
	form_payload, form_payload_ok := form_entity.payload.(^Entity_Routine_Payload)
	testing.expect(t, form_payload_ok)
	testing.expect(t, form_payload.body_scope != nil)
	testing.expect_value(t, len(form_payload.parameters), 1)

	iv_name := string_interner.insert(project.interner, "iv")
	_, iv_entity, iv_ok := checker_lookup_declaration_from_scope(form_payload.body_scope, .Value, iv_name)
	testing.expect(t, iv_ok)
	testing.expect_value(t, iv_entity.kind, Entity_Kind.Parameter)

	lv_name := string_interner.insert(project.interner, "lv")
	_, lv_entity, lv_ok := checker_lookup_declaration_from_scope(form_payload.body_scope, .Value, lv_name)
	testing.expect(t, lv_ok)
	testing.expect_value(t, lv_entity.kind, Entity_Kind.Variable)
	testing.expect_value(t, lv_entity.state, Entity_State.Resolved)

	gv_name := string_interner.insert(project.interner, "gv")
	_, gv_entity, gv_ok := checker_lookup_declaration_from_scope(file.root_scope, .Value, gv_name)
	testing.expect(t, gv_ok)
	testing.expect(t, .Used in gv_entity.flags)
	testing.expect(t, .Used in iv_entity.flags)
	testing.expect(t, .Used in lv_entity.flags)
	testing.expect(t, len(checker.info.uses) >= 3)
	testing.expect(t, len(checker.info.expr_infos) >= 4)
}

@(test)
root_semantic_scope_lookup_resolves_oop_aliases_and_qualified_methods :: proc(t: ^testing.T) {
	source := `INTERFACE lif_object.
  METHODS copy IMPORTING iv_value TYPE i.
  METHODS rename.
ENDINTERFACE.
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES alias_copy FOR lif_object~copy.
    CLASS-METHODS copy.
    METHODS lif_object~copy REDEFINITION.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD copy.
  ENDMETHOD.
  METHOD lif_object~copy.
    DATA lv_value TYPE i.
    lv_value = iv_value.
  ENDMETHOD.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://oop_lookup.abap")

	for diagnostic in checker.info.diagnostics {
		testing.expect(t, diagnostic.kind != .Duplicate_Declaration)
	}

	iface := checker_test_lookup(t, &project, file.root_scope, .Type, "lif_object", .Interface)
	class := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl", .Class)
	testing.expect(t, iface != nil && class != nil)
	if class == nil {
		return
	}
	class_payload := class.payload.(^Entity_Object_Payload)
	class_scope := class_payload.definition_scope

	local_copy := checker_test_lookup(t, &project, class_scope, .Routine, "copy", .Method)
	qualified_copy := checker_test_lookup(t, &project, class_scope, .Routine, "lif_object~copy", .Method)
	testing.expect(t, local_copy != nil && qualified_copy != nil && local_copy != qualified_copy)
	if local_copy != nil {
		testing.expect(t, .Static in local_copy.flags)
		local_payload := local_copy.payload.(^Entity_Routine_Payload)
		testing.expect(t, local_payload.has_implementation)
	}
	if qualified_copy != nil {
		testing.expect(t, !(.Static in qualified_copy.flags))
		qualified_payload := qualified_copy.payload.(^Entity_Routine_Payload)
		testing.expect(t, qualified_payload.has_implementation)
		local := checker_test_lookup(t, &project, qualified_payload.body_scope, .Value, "lv_value", .Variable)
		testing.expect(t, local != nil && .Used in local.flags)
	}

	alias_name := string_interner.insert(project.interner, "alias_copy")
	alias_target, alias_ok := checker_lookup_object_member(class, .Routine, alias_name)
	testing.expect(t, alias_ok)
	if alias_ok {
		testing.expect(t, alias_target.owner == iface)
		testing.expect_value(t, string_interner.load(project.interner, alias_target.name), "copy")
	}

	rename_name := string_interner.insert(project.interner, "rename")
	rename, rename_ok := checker_lookup_object_member(class, .Routine, rename_name)
	testing.expect(t, rename_ok)
	if rename_ok {
		testing.expect(t, rename.owner == iface)
		testing.expect_value(t, rename.kind, Entity_Kind.Method)
	}
}

@(test)
root_semantic_collects_normal_amdp_and_kernel_method_implementations :: proc(t: ^testing.T) {
	source := `CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
    METHODS select_rows.
    METHODS kernel_run.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_value TYPE i.
  ENDMETHOD.
  METHOD select_rows BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT OPTIONS READ-ONLY USING mara.
    lt_rows = SELECT matnr FROM mara;
  ENDMETHOD.
  METHOD kernel_run BY KERNEL MODULE zkernel.
  ENDMETHOD.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	_, file := checker_test_check_source(t, &project, source, "mem://method_impl_headers.abap")

	class := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl_demo", .Class)
	if class == nil {
		return
	}
	class_payload := class.payload.(^Entity_Object_Payload)
	scope := class_payload.definition_scope
	run := checker_test_lookup(t, &project, scope, .Routine, "run", .Method)
	amdp := checker_test_lookup(t, &project, scope, .Routine, "select_rows", .Method)
	kernel := checker_test_lookup(t, &project, scope, .Routine, "kernel_run", .Method)
	methods := [?]^Entity{run, amdp, kernel}
	for method in methods {
		if method == nil {
			continue
		}
		payload := method.payload.(^Entity_Routine_Payload)
		testing.expect(t, payload.has_implementation)
		testing.expect(t, payload.implementation_range.end > payload.implementation_range.start)
		testing.expect_value(t, payload.signature, "")
	}
}

@(test)
root_semantic_oop_checker_inherits_redefinition_signature_and_receivers :: proc(t: ^testing.T) {
	source := `CLASS lcl_root DEFINITION.
  PUBLIC SECTION.
    METHODS get_source_position
      EXPORTING
        program_name TYPE string
        include_name TYPE string
        source_line TYPE i.
ENDCLASS.
CLASS lcl_child DEFINITION INHERITING FROM lcl_root.
  PUBLIC SECTION.
    METHODS get_source_position REDEFINITION.
    METHODS own.
ENDCLASS.
CLASS lcl_root IMPLEMENTATION.
  METHOD get_source_position.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_child IMPLEMENTATION.
  METHOD get_source_position.
    include_name = program_name.
    source_line = source_line.
    me->own( ).
    super->get_source_position( ).
  ENDMETHOD.
  METHOD own.
  ENDMETHOD.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://oop_redefinition_signature.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Context), 0)
	child := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl_child", .Class)
	testing.expect(t, child != nil)
	if child == nil {
		return
	}
	child_payload := child.payload.(^Entity_Object_Payload)
	method := checker_test_lookup(t, &project, child_payload.definition_scope, .Routine, "get_source_position", .Method)
	testing.expect(t, method != nil)
	if method == nil {
		return
	}
	method_payload := method.payload.(^Entity_Routine_Payload)
	testing.expect_value(t, len(method_payload.parameters), 3)
	program_name := checker_test_lookup(t, &project, method_payload.body_scope, .Value, "program_name", .Parameter)
	include_name := checker_test_lookup(t, &project, method_payload.body_scope, .Value, "include_name", .Parameter)
	source_line := checker_test_lookup(t, &project, method_payload.body_scope, .Value, "source_line", .Parameter)
	me := checker_test_lookup(t, &project, method_payload.body_scope, .Value, "me", .Parameter)
	super := checker_test_lookup(t, &project, method_payload.body_scope, .Value, "super", .Parameter)
	testing.expect(t, .Used in program_name.flags)
	testing.expect(t, .Used in include_name.flags)
	testing.expect(t, .Used in source_line.flags)
	testing.expect(t, .Used in me.flags)
	testing.expect(t, .Used in super.flags)
}

@(test)
root_semantic_oop_checker_reuses_multi_level_redefinition_signatures_in_calls :: proc(t: ^testing.T) {
	source := `CLASS lcl_helper DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS show_object IMPORTING im_obj_type TYPE string im_name TYPE string.
ENDCLASS.
CLASS lcl_root DEFINITION.
  PUBLIC SECTION.
    METHODS download IMPORTING im_object_type TYPE string im_object_name TYPE string.
ENDCLASS.
CLASS lcl_mid DEFINITION INHERITING FROM lcl_root.
  PUBLIC SECTION.
    METHODS download REDEFINITION.
ENDCLASS.
CLASS lcl_leaf DEFINITION INHERITING FROM lcl_mid.
  PUBLIC SECTION.
    METHODS download REDEFINITION.
ENDCLASS.
CLASS lcl_helper IMPLEMENTATION.
  METHOD show_object.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_leaf IMPLEMENTATION.
  METHOD download.
    lcl_helper=>show_object(
      im_obj_type = im_object_type
      im_name = im_object_name ).
  ENDMETHOD.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://oop_multi_redefinition_signature.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Named_Parameter), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Missing_Required_Parameter), 0)
	leaf := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl_leaf", .Class)
	testing.expect(t, leaf != nil)
	if leaf == nil {
		return
	}
	leaf_payload := leaf.payload.(^Entity_Object_Payload)
	method := checker_test_lookup(t, &project, leaf_payload.definition_scope, .Routine, "download", .Method)
	method_payload := method.payload.(^Entity_Routine_Payload)
	im_object_type := checker_test_lookup(t, &project, method_payload.body_scope, .Value, "im_object_type", .Parameter)
	im_object_name := checker_test_lookup(t, &project, method_payload.body_scope, .Value, "im_object_name", .Parameter)
	testing.expect(t, .Used in im_object_type.flags)
	testing.expect(t, .Used in im_object_name.flags)
}

@(test)
root_semantic_oop_checker_derives_event_handler_parameter_types :: proc(t: ^testing.T) {
	source := `CLASS lcl_source DEFINITION.
  PUBLIC SECTION.
    DATA object_type TYPE string.
    EVENTS saved EXPORTING VALUE(ex_object) TYPE REF TO lcl_source.
ENDCLASS.
CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    METHODS on_saved FOR EVENT saved OF lcl_source IMPORTING ex_object.
ENDCLASS.
CLASS lcl_handler IMPLEMENTATION.
  METHOD on_saved.
    DATA lv_type TYPE string.
    lv_type = ex_object->object_type.
  ENDMETHOD.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	_, file := checker_test_check_source(t, &project, source, "mem://oop_event_handler.abap")

	source_class := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl_source", .Class)
	handler_class := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl_handler", .Class)
	testing.expect(t, source_class != nil && handler_class != nil)
	if source_class == nil || handler_class == nil {
		return
	}
	handler_payload := handler_class.payload.(^Entity_Object_Payload)
	method := checker_test_lookup(t, &project, handler_payload.definition_scope, .Routine, "on_saved", .Method)
	method_payload := method.payload.(^Entity_Routine_Payload)
	testing.expect_value(t, len(method_payload.parameters), 1)
	param := method_payload.parameters[0]
	testing.expect(t, .Has_Declared_Type in param.flags)
	testing.expect_value(t, param.type.kind, Type_Kind.Ref)
	testing.expect(t, checker_type_object_entity(param.type) == source_class)

	source_payload := source_class.payload.(^Entity_Object_Payload)
	attr := checker_test_lookup(t, &project, source_payload.definition_scope, .Value, "object_type", .Variable)
	testing.expect(t, .Used in attr.flags)
}

@(test)
root_semantic_oop_checker_derives_event_handler_optional_parameters :: proc(t: ^testing.T) {
	source := `CLASS lcl_source DEFINITION.
  PUBLIC SECTION.
    EVENTS html_event EXPORTING
      VALUE(action) TYPE string
      VALUE(frame) TYPE string OPTIONAL.
ENDCLASS.
CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    METHODS on_event FOR EVENT html_event OF lcl_source IMPORTING action frame.
    METHODS run.
ENDCLASS.
CLASS lcl_handler IMPLEMENTATION.
  METHOD on_event.
  ENDMETHOD.
  METHOD run.
    on_event( action = 'refresh' ).
  ENDMETHOD.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://oop_event_handler_optional.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Missing_Required_Parameter), 0)
	handler_class := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl_handler", .Class)
	testing.expect(t, handler_class != nil)
	if handler_class == nil {
		return
	}
	handler_payload := handler_class.payload.(^Entity_Object_Payload)
	method := checker_test_lookup(t, &project, handler_payload.definition_scope, .Routine, "on_event", .Method)
	method_payload := method.payload.(^Entity_Routine_Payload)
	frame := checker_routine_parameter_named(method_payload, checker_intern_name(&project, "frame"))
	testing.expect(t, frame != nil && .Optional in frame.flags)
}

@(test)
root_semantic_oop_checker_resolves_qualified_interface_signature :: proc(t: ^testing.T) {
	source := `INTERFACE lif_message.
  METHODS get_longtext IMPORTING preserve_newlines TYPE abap_bool.
ENDINTERFACE.
INTERFACE lif_t100_message.
  INTERFACES lif_message.
ENDINTERFACE.
CLASS lcl_exception DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_t100_message.
    METHODS lif_message~get_longtext REDEFINITION.
ENDCLASS.
CLASS lcl_exception IMPLEMENTATION.
  METHOD lif_message~get_longtext.
    DATA lv_keep TYPE abap_bool.
    lv_keep = preserve_newlines.
  ENDMETHOD.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	_, file := checker_test_check_source(t, &project, source, "mem://oop_qualified_interface_signature.abap")

	class := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl_exception", .Class)
	testing.expect(t, class != nil)
	if class == nil {
		return
	}
	class_payload := class.payload.(^Entity_Object_Payload)
	method := checker_test_lookup(t, &project, class_payload.definition_scope, .Routine, "lif_message~get_longtext", .Method)
	method_payload := method.payload.(^Entity_Routine_Payload)
	testing.expect_value(t, len(method_payload.parameters), 1)
	param := method_payload.parameters[0]
	testing.expect_value(t, string_interner.load(project.interner, param.name), "preserve_newlines")
	testing.expect_value(t, checker_test_type_name(&project, param.type), "abap_bool")
	testing.expect(t, .Used in param.flags)
}

@(test)
root_semantic_oop_checker_types_me_in_interface_method_implementation :: proc(t: ^testing.T) {
	source := `INTERFACE lif_log.
  METHODS merge RETURNING VALUE(ro_log) TYPE REF TO lif_log.
ENDINTERFACE.
CLASS lcl_log DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_log.
    METHODS lif_log~merge REDEFINITION.
ENDCLASS.
CLASS lcl_log IMPLEMENTATION.
  METHOD lif_log~merge.
    ro_log = me.
  ENDMETHOD.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://oop_interface_me.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 0)
	class := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl_log", .Class)
	testing.expect(t, class != nil)
	if class == nil {
		return
	}
	class_payload := class.payload.(^Entity_Object_Payload)
	method := checker_test_lookup(t, &project, class_payload.definition_scope, .Routine, "lif_log~merge", .Method)
	method_payload := method.payload.(^Entity_Routine_Payload)
	ro_log := checker_test_lookup(t, &project, method_payload.body_scope, .Value, "ro_log", .Parameter)
	me := checker_test_lookup(t, &project, method_payload.body_scope, .Value, "me", .Parameter)
	testing.expect(t, checker_type_object_entity(ro_log.type) != nil)
	testing.expect(t, checker_type_object_entity(me.type) == class)
	testing.expect(t, .Used in ro_log.flags)
	testing.expect(t, .Used in me.flags)
}

@(test)
root_semantic_oop_checker_types_me_in_implicit_interface_method_implementation :: proc(t: ^testing.T) {
	source := `INTERFACE lif_log.
  METHODS merge_with
    IMPORTING ii_log TYPE REF TO lif_log
    RETURNING VALUE(ri_log) TYPE REF TO lif_log.
ENDINTERFACE.
CLASS lcl_log DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_log.
ENDCLASS.
CLASS lcl_log IMPLEMENTATION.
  METHOD lif_log~merge_with.
    ri_log = me.
  ENDMETHOD.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://oop_implicit_interface_me.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Context), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 0)
	class := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl_log", .Class)
	testing.expect(t, class != nil)
	if class == nil {
		return
	}
	class_payload := class.payload.(^Entity_Object_Payload)
	method := checker_test_lookup(t, &project, class_payload.definition_scope, .Routine, "lif_log~merge_with", .Method)
	method_payload := method.payload.(^Entity_Routine_Payload)
	ri_log := checker_test_lookup(t, &project, method_payload.body_scope, .Value, "ri_log", .Parameter)
	ii_log := checker_test_lookup(t, &project, method_payload.body_scope, .Value, "ii_log", .Parameter)
	me := checker_test_lookup(t, &project, method_payload.body_scope, .Value, "me", .Parameter)
	testing.expect(t, method.owner == class)
	testing.expect(t, method_payload.has_implementation)
	testing.expect_value(t, len(method_payload.parameters), 2)
	testing.expect(t, checker_type_object_entity(ri_log.type) != nil)
	testing.expect(t, checker_type_object_entity(ii_log.type) != nil)
	testing.expect(t, checker_type_object_entity(me.type) == class)
	testing.expect(t, .Used in ri_log.flags)
	testing.expect(t, .Used in me.flags)
}

@(test)
root_semantic_oop_checker_enforces_visibility_and_friends :: proc(t: ^testing.T) {
	source := `CLASS lcl_target DEFINITION FRIENDS lcl_friend.
  PRIVATE SECTION.
    CLASS-DATA gv_value TYPE i.
ENDCLASS.
CLASS lcl_friend DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run.
ENDCLASS.
CLASS lcl_other DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run.
ENDCLASS.
CLASS lcl_friend IMPLEMENTATION.
  METHOD run.
    lcl_target=>gv_value = 1.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_other IMPLEMENTATION.
  METHOD run.
    lcl_target=>gv_value = 2.
  ENDMETHOD.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://oop_visibility_friends.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Inaccessible_Member), 1)
}

@(test)
root_semantic_oop_checker_rejects_me_super_outside_instance_methods :: proc(t: ^testing.T) {
	source := `CLASS lcl_parent DEFINITION.
  PUBLIC SECTION.
    METHODS base.
ENDCLASS.
CLASS lcl_child DEFINITION INHERITING FROM lcl_parent.
  PUBLIC SECTION.
    CLASS-METHODS stat.
ENDCLASS.
CLASS lcl_child IMPLEMENTATION.
  METHOD stat.
    me->stat( ).
    super->base( ).
  ENDMETHOD.
ENDCLASS.
FORM run.
  me->stat( ).
ENDFORM.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://oop_invalid_receivers.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Context), 3)
}

@(test)
root_semantic_decl_split_collects_broadened_file_declarations :: proc(t: ^testing.T) {
	source := `REPORT zdecl.
INCLUDE zinc IF FOUND.
DATA gv_value TYPE i.
DATA(lv_inline) = 1.
CONSTANTS gc_limit TYPE i VALUE 1.
FIELD-SYMBOLS <fs_row> TYPE any.
STATICS sv_count TYPE i.
TABLES mara.
RANGES r_matnr FOR mara-matnr.
PARAMETERS p_count TYPE i DEFAULT 1.
SELECT-OPTIONS s_matnr FOR mara-matnr DEFAULT 'A' TO 'Z'.
CONTROLS tc_main TYPE tableview USING SCREEN 100.
TYPES: BEGIN OF ty_line,
         id TYPE i,
       END OF ty_line.
DATA: BEGIN OF gs_row,
        id TYPE i,
      END OF gs_row.
CONSTANTS: BEGIN OF gc_pair,
             a TYPE i VALUE 1,
             b TYPE i VALUE 2,
           END OF gc_pair.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://decl_split.abap")
	_ = checker

	report := checker_test_lookup(t, &project, file.root_scope, .Value, "zdecl", .Report)
	include := checker_test_lookup(t, &project, file.root_scope, .Value, "zinc", .Include)
	_ = checker_test_lookup(t, &project, file.root_scope, .Value, "gv_value", .Variable)
	_ = checker_test_lookup(t, &project, file.root_scope, .Value, "lv_inline", .Variable)
	_ = checker_test_lookup(t, &project, file.root_scope, .Value, "gc_limit", .Constant)
	_ = checker_test_lookup(t, &project, file.root_scope, .Value, "<fs_row>", .Field_Symbol)
	statics := checker_test_lookup(t, &project, file.root_scope, .Value, "sv_count", .Variable)
	tables := checker_test_lookup(t, &project, file.root_scope, .Value, "mara", .Variable)
	ranges := checker_test_lookup(t, &project, file.root_scope, .Value, "r_matnr", .Variable)
	param := checker_test_lookup(t, &project, file.root_scope, .Value, "p_count", .Variable)
	select_option := checker_test_lookup(t, &project, file.root_scope, .Value, "s_matnr", .Variable)
	_ = checker_test_lookup(t, &project, file.root_scope, .Value, "tc_main", .Control)
	typ := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_line", .Type_Def)
	data_struct := checker_test_lookup(t, &project, file.root_scope, .Value, "gs_row", .Variable)
	const_struct := checker_test_lookup(t, &project, file.root_scope, .Value, "gc_pair", .Constant)

	if report != nil {
		payload := report.payload.(^Entity_Report_Payload)
		testing.expect_value(t, len(payload.provided_names), 1)
	}
	if include != nil {
		payload := include.payload.(^Entity_Include_Payload)
		testing.expect(t, payload.if_found)
	}
	testing.expect(t, statics != nil && .Static in statics.flags)
	testing.expect(t, tables != nil && .Has_Declared_Type in tables.flags)
	testing.expect(t, param != nil && param.decl_info.default_expr != nil)
	if ranges != nil && ranges.type != nil {
		testing.expect_value(t, ranges.type.kind, Type_Kind.Structure)
		testing.expect_value(t, len(ranges.type.structure.fields), 4)
	}
	if select_option != nil && select_option.type != nil {
		testing.expect_value(t, select_option.type.kind, Type_Kind.Structure)
		testing.expect_value(t, len(select_option.type.structure.fields), 4)
	}
	if typ != nil {
		payload := typ.payload.(^Entity_Type_Name_Payload)
		testing.expect(t, payload.structure != nil)
		if payload.structure != nil {
			testing.expect_value(t, len(payload.structure.fields), 1)
		}
	}
	if data_struct != nil && data_struct.type != nil {
		testing.expect_value(t, data_struct.type.kind, Type_Kind.Structure)
		testing.expect_value(t, len(data_struct.type.structure.fields), 1)
	}
	if const_struct != nil && const_struct.type != nil {
		testing.expect_value(t, const_struct.type.kind, Type_Kind.Structure)
		testing.expect_value(t, len(const_struct.type.structure.fields), 2)
	}
}

@(test)
root_semantic_collects_declarations_inside_enhancement_blocks :: proc(t: ^testing.T) {
	source := `ENHANCEMENT enh.
  DATA lv_inside TYPE i.
ENDENHANCEMENT.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://enhancement_block_decl.abap")
	entity := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_inside", .Variable)

	name_offset := checker_test_find_text(source, "lv_inside")
	testing.expect(t, name_offset >= 0)
	query := semantic_query(&project, &checker, file)
	decl := semantic_decl_entity_at_offset(semantic_query_decls(query), name_offset)
	testing.expect(t, decl == entity)
}

@(test)
root_semantic_collects_declarations_inside_enhancement_sections :: proc(t: ^testing.T) {
	source := `ENHANCEMENT-SECTION z_sec SPOTS es_demo INCLUDE BOUND.
  DATA lv_section TYPE i.
END-ENHANCEMENT-SECTION.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://enhancement_section_decl.abap")
	entity := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_section", .Variable)

	name_offset := checker_test_find_text(source, "lv_section")
	testing.expect(t, name_offset >= 0)
	query := semantic_query(&project, &checker, file)
	decl := semantic_decl_entity_at_offset(semantic_query_decls(query), name_offset)
	testing.expect(t, decl == entity)
}

@(test)
root_semantic_collects_declarations_inside_test_blocks :: proc(t: ^testing.T) {
	source := `TEST-SEAM seam.
  DATA lv_seam TYPE i.
END-TEST-SEAM.
TEST-INJECTION seam.
  DATA lv_injection TYPE i.
END-TEST-INJECTION.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://test_block_decl.abap")
	seam_entity := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_seam", .Variable)
	injection_entity := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_injection", .Variable)

	query := semantic_query(&project, &checker, file)
	seam_offset := checker_test_find_text(source, "lv_seam")
	injection_offset := checker_test_find_text(source, "lv_injection")
	testing.expect(t, seam_offset >= 0)
	testing.expect(t, injection_offset >= 0)
	seam_decl := semantic_decl_entity_at_offset(semantic_query_decls(query), seam_offset)
	injection_decl := semantic_decl_entity_at_offset(semantic_query_decls(query), injection_offset)
	testing.expect(t, seam_decl == seam_entity)
	testing.expect(t, injection_decl == injection_entity)
}

@(test)
root_semantic_selection_screen_text_fields_record_value_uses :: proc(t: ^testing.T) {
	source := `DATA sc_title TYPE c.
DATA sc_url TYPE c.
DATA pb_text TYPE c.
PARAMETERS p_url TYPE string.
SELECTION-SCREEN BEGIN OF SCREEN 1002 TITLE sc_title.
SELECTION-SCREEN COMMENT 1(18) sc_url FOR FIELD p_url.
SELECTION-SCREEN PUSHBUTTON 20(10) pb_text USER-COMMAND run MODIF ID md2.
SELECTION-SCREEN END OF SCREEN 1002.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://selection_screen_text_fields.abap")

	title := checker_test_lookup(t, &project, file.root_scope, .Value, "sc_title", .Variable)
	comment := checker_test_lookup(t, &project, file.root_scope, .Value, "sc_url", .Variable)
	pushbutton := checker_test_lookup(t, &project, file.root_scope, .Value, "pb_text", .Variable)
	field := checker_test_lookup(t, &project, file.root_scope, .Value, "p_url", .Variable)

	testing.expect(t, .Used in title.flags)
	testing.expect(t, .Used in comment.flags)
	testing.expect(t, .Used in pushbutton.flags)
	testing.expect(t, .Used in field.flags)
	testing.expect_value(t, checker_test_unresolved_candidate_namespace_count(&checker, &project, .Global_Symbol, .Value, "run"), 0)
	testing.expect_value(t, checker_test_unresolved_candidate_namespace_count(&checker, &project, .Global_Symbol, .Value, "md2"), 0)
}

@(test)
root_semantic_selection_screen_event_variants_are_distinct :: proc(t: ^testing.T) {
	source := `PARAMETERS p_field TYPE string.
AT SELECTION-SCREEN OUTPUT.
  DATA lv_output TYPE i.
AT   SELECTION-SCREEN   ON   EXIT-COMMAND.
  DATA lv_exit TYPE i.
AT SELECTION-SCREEN ON p_field.
  DATA lv_field TYPE i.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_field.
  DATA lv_value TYPE i.
AT SELECTION-SCREEN.
  DATA lv_event TYPE i.
START-OF-SELECTION.
  DATA lv_start TYPE i.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://selection_screen_events.abap")

	p_field := checker_test_lookup(t, &project, file.root_scope, .Value, "p_field", .Variable)
	_ = checker_test_lookup(t, &project, file.root_scope, .Routine, "at selection-screen output", .Event)
	_ = checker_test_lookup(t, &project, file.root_scope, .Routine, "at selection-screen on exit-command", .Event)
	_ = checker_test_lookup(t, &project, file.root_scope, .Routine, "at selection-screen on p_field", .Event)
	_ = checker_test_lookup(t, &project, file.root_scope, .Routine, "at selection-screen on value-request for p_field", .Event)
	_ = checker_test_lookup(t, &project, file.root_scope, .Routine, "at selection-screen", .Event)
	_ = checker_test_lookup(t, &project, file.root_scope, .Routine, "start-of-selection", .Event)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Duplicate_Declaration), 0)

	target_offset := checker_test_find_text(source, "ON p_field") + len("ON ")
	testing.expect(t, target_offset >= len("ON "))
	if target_offset < len("ON ") {
		return
	}
	target_range := tokenizer.text_range(target_offset, target_offset + len("p_field"))
	query := semantic_query(&project, &checker, file)
	use := semantic_ref_use_at_range(semantic_query_refs(query), target_range)
	testing.expect(t, use != nil)
	if use != nil {
		testing.expect(t, use.entity == p_field)
		testing.expect_value(t, source[use.range.start:use.range.end], "p_field")
	}
}

@(test)
root_semantic_decl_split_collects_class_interface_and_oop_members :: proc(t: ^testing.T) {
	source := `INTERFACE lif_demo.
  METHODS get_value RETURNING VALUE(rv_value) TYPE string.
  EVENTS changed EXPORTING VALUE(ev_value) TYPE string.
ENDINTERFACE.
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_demo.
    ALIASES get_value FOR lif_demo~get_value.
    DATA mv_value TYPE string READ-ONLY.
    CLASS-DATA gv_count TYPE i.
    METHODS run IMPORTING iv_value TYPE string.
    CLASS-METHODS create RETURNING VALUE(ro_demo) TYPE REF TO lcl_demo.
    EVENTS done EXPORTING VALUE(ev_value) TYPE string.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_local TYPE string.
    lv_local = iv_value.
  ENDMETHOD.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	_, file := checker_test_check_source(t, &project, source, "mem://oop_decl_split.abap")

	iface := checker_test_lookup(t, &project, file.root_scope, .Type, "lif_demo", .Interface)
	class := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl_demo", .Class)
	testing.expect(t, iface != nil && class != nil)
	if class == nil {
		return
	}
	class_payload := class.payload.(^Entity_Object_Payload)
	testing.expect(t, class_payload.definition_scope != nil)
	testing.expect_value(t, len(class_payload.implemented_interfaces), 1)
	class_scope := class_payload.definition_scope

	alias := checker_test_find_scope_entity(t, &project, class_scope, "get_value", .Alias)
	attr := checker_test_lookup(t, &project, class_scope, .Value, "mv_value", .Variable)
	static_attr := checker_test_lookup(t, &project, class_scope, .Value, "gv_count", .Variable)
	run := checker_test_lookup(t, &project, class_scope, .Routine, "run", .Method)
	create := checker_test_lookup(t, &project, class_scope, .Routine, "create", .Method)
	event := checker_test_lookup(t, &project, class_scope, .Routine, "done", .Event)

	if alias != nil {
		payload := alias.payload.(^Entity_Alias_Payload)
		testing.expect_value(t, string_interner.load(project.interner, payload.target_interface_name), "lif_demo")
		testing.expect_value(t, string_interner.load(project.interner, payload.target_member_name), "get_value")
	}
	testing.expect(t, attr != nil && .Read_Only in attr.flags)
	testing.expect(t, static_attr != nil && .Static in static_attr.flags)
	if run != nil {
		payload := run.payload.(^Entity_Routine_Payload)
		testing.expect(t, payload.has_implementation)
		testing.expect_value(t, len(payload.parameters), 1)
		local := checker_test_lookup(t, &project, payload.body_scope, .Value, "lv_local", .Variable)
		testing.expect(t, local != nil && local.state == .Resolved)
	}
	if create != nil {
		payload := create.payload.(^Entity_Routine_Payload)
		testing.expect(t, payload.is_static)
		testing.expect_value(t, len(payload.parameters), 1)
	}
	if event != nil {
		payload := event.payload.(^Entity_Routine_Payload)
		testing.expect_value(t, payload.member_kind, Class_Member_Kind.Event)
		testing.expect_value(t, len(payload.parameters), 1)
	}
}

@(test)
root_semantic_data_and_class_data_keep_static_boundary :: proc(t: ^testing.T) {
	source := `CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    DATA mv_value TYPE i READ-ONLY.
    CLASS-DATA gv_value TYPE i READ-ONLY.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	_, file := checker_test_check_source(t, &project, source, "mem://data_class_data_static_boundary.abap")

	class := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl_demo", .Class)
	testing.expect(t, class != nil)
	if class == nil {
		return
	}
	class_payload := class.payload.(^Entity_Object_Payload)
	attr := checker_test_lookup(t, &project, class_payload.definition_scope, .Value, "mv_value", .Variable)
	static_attr := checker_test_lookup(t, &project, class_payload.definition_scope, .Value, "gv_value", .Variable)

	testing.expect(t, attr != nil && .Read_Only in attr.flags)
	testing.expect(t, attr != nil && !(.Static in attr.flags))
	testing.expect(t, static_attr != nil && .Read_Only in static_attr.flags)
	testing.expect(t, static_attr != nil && .Static in static_attr.flags)
}

@(test)
root_semantic_oop_member_additions_and_parameter_defaults_are_collected :: proc(t: ^testing.T) {
	source := `CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS choose ABSTRACT
      IMPORTING !iv_value TYPE string DEFAULT 'x' PREFERRED PARAMETER iv_value.
    METHODS done FINAL.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	_, file := checker_test_check_source(t, &project, source, "mem://oop_member_additions.abap")

	class := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl_demo", .Class)
	testing.expect(t, class != nil)
	if class == nil {
		return
	}
	class_payload := class.payload.(^Entity_Object_Payload)
	choose := checker_test_lookup(t, &project, class_payload.definition_scope, .Routine, "choose", .Method)
	done := checker_test_lookup(t, &project, class_payload.definition_scope, .Routine, "done", .Method)
	testing.expect(t, choose != nil && done != nil)
	if choose == nil || done == nil {
		return
	}

	choose_payload := choose.payload.(^Entity_Routine_Payload)
	param := checker_test_lookup(t, &project, choose_payload.signature_scope, .Value, "iv_value", .Parameter)
	testing.expect(t, .Static in choose.flags)
	testing.expect(t, .Abstract in choose.flags)
	testing.expect(t, .Final in done.flags)
	testing.expect(t, param != nil && param.decl_info.default_expr != nil)
}

@(test)
root_semantic_expr_checker_resolves_structure_selectors_and_table_keys :: proc(t: ^testing.T) {
	source := `FORM run.
  TYPES: BEGIN OF ty_status,
           exist_attp TYPE abap_bool,
           text TYPE string,
         END OF ty_status.
  DATA lt_status TYPE STANDARD TABLE OF ty_status WITH DEFAULT KEY.
  FIELD-SYMBOLS <ls_status> LIKE LINE OF lt_status.
  DATA ls_status TYPE ty_status.
  DATA lv_text TYPE string.
  DATA lv_subrc TYPE i.

  lv_text = ls_status-text.
  lv_text = <ls_status>-text.
  lv_subrc = sy-subrc.
  IF line_exists( lt_status[ exist_attp = abap_undefined ] ).
  ENDIF.
ENDFORM.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://expr_selectors.abap")

	run := checker_test_lookup(t, &project, file.root_scope, .Routine, "run", .Form)
	testing.expect(t, run != nil)
	if run == nil {
		return
	}
	run_payload := run.payload.(^Entity_Routine_Payload)
	ty_status := checker_test_lookup(t, &project, run_payload.body_scope, .Type, "ty_status", .Type_Def)
	lv_text := checker_test_lookup(t, &project, run_payload.body_scope, .Value, "lv_text", .Variable)
	testing.expect(t, ty_status != nil && lv_text != nil)
	if ty_status == nil || lv_text == nil {
		return
	}
	structure := checker_type_structure(ty_status.type)
	testing.expect(t, structure != nil)
	if structure == nil {
		return
	}
	exist_attp := checker_test_structure_field(t, &project, structure, "exist_attp")
	text := checker_test_structure_field(t, &project, structure, "text")
	testing.expect(t, .Used in exist_attp.flags)
	testing.expect(t, .Used in text.flags)

	syst, syst_ok := checker_lookup_builtin_entity(&checker, .Type, "syst")
	testing.expect(t, syst_ok)
	if syst_ok {
		payload := syst.payload.(^Entity_Type_Name_Payload)
		subrc := checker_test_structure_field(t, &project, payload.structure, "subrc")
		testing.expect(t, .Used in subrc.flags)
	}

	form_decl := run.decl_info.decl_node.derived.(^ast.Form_Decl)
	assign_stmt := form_decl.body[6].derived_stmt.(^ast.Assign_Stmt)
	selector_info, selector_ok := checker_test_expr_info_for_node(t, &checker, &assign_stmt.rhs.expr_base)
	testing.expect(t, selector_ok)
	if selector_ok {
		testing.expect_value(t, selector_info.mode, ast.Addressing_Mode.Field)
		testing.expect(t, checker_type_same(selector_info.type, lv_text.type))
	}
}

@(test)
root_semantic_expr_checker_records_call_inline_and_constructor_types :: proc(t: ^testing.T) {
	source := `CLASS lcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS get_text RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.
CLASS lcl_dep IMPLEMENTATION.
  METHOD get_text.
    rv_text = 'x'.
  ENDMETHOD.
ENDCLASS.
FORM run.
  DATA lo_dep TYPE REF TO lcl_dep.
  DATA(lv_text) = lo_dep->get_text( ).
  DATA lv_num TYPE i.
  lv_num = VALUE #( ).
ENDFORM.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://expr_call_inline.abap")

	run := checker_test_lookup(t, &project, file.root_scope, .Routine, "run", .Form)
	class := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl_dep", .Class)
	testing.expect(t, run != nil && class != nil)
	if run == nil || class == nil {
		return
	}
	run_payload := run.payload.(^Entity_Routine_Payload)
	class_payload := class.payload.(^Entity_Object_Payload)
	method := checker_test_lookup(t, &project, class_payload.definition_scope, .Routine, "get_text", .Method)
	lv_text := checker_test_lookup(t, &project, run_payload.body_scope, .Value, "lv_text", .Variable)
	lv_num := checker_test_lookup(t, &project, run_payload.body_scope, .Value, "lv_num", .Variable)
	testing.expect(t, method != nil && lv_text != nil && lv_num != nil)
	if method == nil || lv_text == nil || lv_num == nil {
		return
	}
	testing.expect(t, .Used in method.flags)
	testing.expect_value(t, checker_test_type_name(&project, lv_text.type), "string")
	testing.expect_value(t, checker_test_type_name(&project, lv_num.type), "i")

	form_decl := run.decl_info.decl_node.derived.(^ast.Form_Decl)
	inline_stmt := form_decl.body[1].derived_stmt.(^ast.Data_Inline_Decl)
	call_info, call_ok := checker_test_expr_info_for_node(t, &checker, &inline_stmt.expr.expr_base)
	testing.expect(t, call_ok)
	if call_ok {
		testing.expect_value(t, call_info.mode, ast.Addressing_Mode.Value)
		testing.expect_value(t, checker_test_type_name(&project, call_info.type), "string")
	}
	assign_stmt := form_decl.body[3].derived_stmt.(^ast.Assign_Stmt)
	constructor_info, constructor_ok := checker_test_expr_info_for_node(t, &checker, &assign_stmt.rhs.expr_base)
	testing.expect(t, constructor_ok)
	if constructor_ok {
		testing.expect_value(t, constructor_info.mode, ast.Addressing_Mode.Value)
		testing.expect(t, checker_type_same(constructor_info.type, lv_num.type))
	}
}

@(test)
root_semantic_stmt_checker_reports_assignment_conversion_failures :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row,
         value TYPE c,
       END OF ty_row.
DATA lv_date TYPE d.
DATA lv_time TYPE t.
DATA lr_i TYPE REF TO i.
DATA lr_data TYPE REF TO data.
DATA lr_row TYPE REF TO ty_row.

lv_date = lv_time.
lr_data = lr_row.
lr_i = lr_data.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_assignment.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 2)
}

@(test)
root_semantic_stmt_checker_accepts_dynamic_object_assign_to_typed_field_symbol :: proc(t: ^testing.T) {
	source := `CLASS lcl_params DEFINITION.
ENDCLASS.
DATA lo_object TYPE REF TO object.
FIELD-SYMBOLS <lo_params> TYPE REF TO lcl_params.

ASSIGN lo_object->('PARAMS') TO <lo_params>.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://stmt_assign_dynamic_object_component.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 0)
	lo_object := checker_test_lookup(t, &project, file.root_scope, .Value, "lo_object", .Variable)
	lo_params := checker_test_lookup(t, &project, file.root_scope, .Value, "<lo_params>", .Field_Symbol)
	testing.expect(t, .Used in lo_object.flags)
	testing.expect(t, .Used in lo_params.flags)
}

@(test)
root_semantic_stmt_checker_reports_method_argument_failures :: proc(t: ^testing.T) {
	source := `CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING iv_index TYPE i.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.
DATA lv_text TYPE string.
lcl_demo=>run( lv_text ).
lcl_demo=>run( 'abc' ).
lcl_demo=>run( ).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_method_args.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Argument_Type), 2)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Missing_Required_Parameter), 1)
	missing_message_found := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind == .Missing_Required_Parameter {
			missing_message_found = true
			testing.expect_value(t, diagnostic.message, "missing required parameter 'iv_index'")
		}
	}
	testing.expect(t, missing_message_found)
}

@(test)
root_semantic_stmt_checker_ignores_form_raising_for_perform_arguments :: proc(t: ^testing.T) {
	source := `PERFORM open_gui.
FORM open_gui RAISING zcx_abapgit_exception.
ENDFORM.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_perform_form_raising.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Missing_Required_Parameter), 0)
}

@(test)
root_semantic_checker_collects_structured_form_header_parameters :: proc(t: ^testing.T) {
	source := `FORM plain.
ENDFORM.
FORM with_tables TABLES rows TYPE STANDARD TABLE OF string rawtab.
ENDFORM.
FORM with_using USING !VALUE TYPE string iv_untyped.
ENDFORM.
FORM with_changing CHANGING cv_count TYPE i cv_untyped.
ENDFORM.`

	project := project_make()
	defer project_destroy(&project)

	_, file := checker_test_check_source(t, &project, source, "mem://form_header_parameters.abap")

	plain := checker_test_lookup(t, &project, file.root_scope, .Routine, "plain", .Form)
	tables := checker_test_lookup(t, &project, file.root_scope, .Routine, "with_tables", .Form)
	using_form := checker_test_lookup(t, &project, file.root_scope, .Routine, "with_using", .Form)
	changing := checker_test_lookup(t, &project, file.root_scope, .Routine, "with_changing", .Form)
	testing.expect(t, plain != nil && tables != nil && using_form != nil && changing != nil)
	if plain == nil || tables == nil || using_form == nil || changing == nil {
		return
	}

	plain_payload := plain.payload.(^Entity_Routine_Payload)
	testing.expect_value(t, len(plain_payload.parameters), 0)

	tables_payload := tables.payload.(^Entity_Routine_Payload)
	rows := checker_test_lookup(t, &project, tables_payload.signature_scope, .Value, "rows", .Parameter)
	rawtab := checker_test_lookup(t, &project, tables_payload.signature_scope, .Value, "rawtab", .Parameter)
	testing.expect(t, rows != nil && rawtab != nil)
	if rows != nil {
		rows_payload := rows.payload.(^Entity_Variable_Payload)
		testing.expect_value(t, rows_payload.section, Entity_Parameter_Section.Form_Tables)
		testing.expect_value(t, rows_payload.passing, Entity_Parameter_Passing.Direct)
		testing.expect_value(t, rows.decl_info.type_clause.form, ast.Data_Type_Form.Standard_Table)
	}
	if rawtab != nil {
		rawtab_payload := rawtab.payload.(^Entity_Variable_Payload)
		testing.expect_value(t, rawtab_payload.section, Entity_Parameter_Section.Form_Tables)
		testing.expect(t, rawtab.decl_info.type_clause == nil)
	}

	using_payload := using_form.payload.(^Entity_Routine_Payload)
	value := checker_test_lookup(t, &project, using_payload.signature_scope, .Value, "value", .Parameter)
	iv_untyped := checker_test_lookup(t, &project, using_payload.signature_scope, .Value, "iv_untyped", .Parameter)
	testing.expect(t, value != nil && iv_untyped != nil)
	if value != nil {
		value_payload := value.payload.(^Entity_Variable_Payload)
		testing.expect_value(t, value_payload.section, Entity_Parameter_Section.Form_Using)
		testing.expect_value(t, value_payload.passing, Entity_Parameter_Passing.Direct)
		testing.expect_value(t, checker_test_type_name(&project, value.type), "string")
	}
	if iv_untyped != nil {
		iv_untyped_payload := iv_untyped.payload.(^Entity_Variable_Payload)
		testing.expect_value(t, iv_untyped_payload.section, Entity_Parameter_Section.Form_Using)
		testing.expect(t, iv_untyped.decl_info.type_clause == nil)
	}

	changing_payload := changing.payload.(^Entity_Routine_Payload)
	cv_count := checker_test_lookup(t, &project, changing_payload.signature_scope, .Value, "cv_count", .Parameter)
	cv_untyped := checker_test_lookup(t, &project, changing_payload.signature_scope, .Value, "cv_untyped", .Parameter)
	testing.expect(t, cv_count != nil && cv_untyped != nil)
	if cv_count != nil {
		cv_count_payload := cv_count.payload.(^Entity_Variable_Payload)
		testing.expect_value(t, cv_count_payload.section, Entity_Parameter_Section.Form_Changing)
		testing.expect_value(t, checker_test_type_name(&project, cv_count.type), "i")
	}
	if cv_untyped != nil {
		cv_untyped_payload := cv_untyped.payload.(^Entity_Variable_Payload)
		testing.expect_value(t, cv_untyped_payload.section, Entity_Parameter_Section.Form_Changing)
		testing.expect(t, cv_untyped.decl_info.type_clause == nil)
	}
}

@(test)
root_semantic_checker_collects_structured_function_header_parameters :: proc(t: ^testing.T) {
	source := `FUNCTION z_plain.
ENDFUNCTION.
FUNCTION z_header
  IMPORTING iv_typed TYPE i iv_untyped VALUE(iv_value) TYPE string OPTIONAL iv_default TYPE string DEFAULT 'x'
  EXPORTING ev_text LIKE sy-uname ev_untyped
  CHANGING REFERENCE(cv_ref) TYPE REF TO object cv_untyped
  TABLES et_return STRUCTURE bapiret2 et_untyped
  EXCEPTIONS failed = 1 not_found.
ENDFUNCTION.`

	project := project_make()
	defer project_destroy(&project)

	_, file := checker_test_check_source(t, &project, source, "mem://function_header_parameters.abap")

	plain := checker_test_lookup(t, &project, file.root_scope, .Routine, "z_plain", .Module)
	header := checker_test_lookup(t, &project, file.root_scope, .Routine, "z_header", .Module)
	testing.expect(t, plain != nil && header != nil)
	if plain == nil || header == nil {
		return
	}

	plain_payload := plain.payload.(^Entity_Routine_Payload)
	testing.expect_value(t, len(plain_payload.parameters), 0)

	payload := header.payload.(^Entity_Routine_Payload)
	iv_typed := checker_test_lookup(t, &project, payload.signature_scope, .Value, "iv_typed", .Parameter)
	iv_untyped := checker_test_lookup(t, &project, payload.signature_scope, .Value, "iv_untyped", .Parameter)
	iv_value := checker_test_lookup(t, &project, payload.signature_scope, .Value, "iv_value", .Parameter)
	iv_default := checker_test_lookup(t, &project, payload.signature_scope, .Value, "iv_default", .Parameter)
	ev_text := checker_test_lookup(t, &project, payload.signature_scope, .Value, "ev_text", .Parameter)
	ev_untyped := checker_test_lookup(t, &project, payload.signature_scope, .Value, "ev_untyped", .Parameter)
	cv_ref := checker_test_lookup(t, &project, payload.signature_scope, .Value, "cv_ref", .Parameter)
	cv_untyped := checker_test_lookup(t, &project, payload.signature_scope, .Value, "cv_untyped", .Parameter)
	et_return := checker_test_lookup(t, &project, payload.signature_scope, .Value, "et_return", .Parameter)
	et_untyped := checker_test_lookup(t, &project, payload.signature_scope, .Value, "et_untyped", .Parameter)
	failed := checker_test_lookup(t, &project, payload.signature_scope, .Value, "failed", .Exception)
	not_found := checker_test_lookup(t, &project, payload.signature_scope, .Value, "not_found", .Exception)
	testing.expect(t, iv_typed != nil && iv_untyped != nil && iv_value != nil && iv_default != nil)
	testing.expect(t, ev_text != nil && ev_untyped != nil && cv_ref != nil && cv_untyped != nil)
	testing.expect(t, et_return != nil && et_untyped != nil && failed != nil && not_found != nil)

	if iv_typed != nil {
		iv_typed_payload := iv_typed.payload.(^Entity_Variable_Payload)
		testing.expect_value(t, iv_typed_payload.section, Entity_Parameter_Section.Function_Importing)
		testing.expect_value(t, iv_typed_payload.passing, Entity_Parameter_Passing.Direct)
		testing.expect_value(t, checker_test_type_name(&project, iv_typed.type), "i")
	}
	if iv_untyped != nil {
		testing.expect(t, iv_untyped.decl_info.type_clause == nil)
	}
	if iv_value != nil {
		iv_value_payload := iv_value.payload.(^Entity_Variable_Payload)
		testing.expect_value(t, iv_value_payload.section, Entity_Parameter_Section.Function_Importing)
		testing.expect_value(t, iv_value_payload.passing, Entity_Parameter_Passing.Value)
		testing.expect(t, .Optional in iv_value.flags)
	}
	if iv_default != nil {
		iv_default_payload := iv_default.payload.(^Entity_Variable_Payload)
		testing.expect_value(t, iv_default_payload.section, Entity_Parameter_Section.Function_Importing)
		testing.expect(t, .Has_Default_Value in iv_default.flags)
		testing.expect(t, iv_default.decl_info.default_expr != nil)
	}
	if ev_text != nil {
		ev_text_payload := ev_text.payload.(^Entity_Variable_Payload)
		testing.expect_value(t, ev_text_payload.section, Entity_Parameter_Section.Function_Exporting)
		testing.expect_value(t, ev_text.decl_info.type_clause.form, ast.Data_Type_Form.Like)
	}
	if ev_untyped != nil {
		testing.expect(t, ev_untyped.decl_info.type_clause == nil)
	}
	if cv_ref != nil {
		cv_ref_payload := cv_ref.payload.(^Entity_Variable_Payload)
		testing.expect_value(t, cv_ref_payload.section, Entity_Parameter_Section.Function_Changing)
		testing.expect_value(t, cv_ref_payload.passing, Entity_Parameter_Passing.Reference)
		testing.expect_value(t, cv_ref.decl_info.type_clause.form, ast.Data_Type_Form.Ref_To)
	}
	if cv_untyped != nil {
		testing.expect(t, cv_untyped.decl_info.type_clause == nil)
	}
	if et_return != nil {
		et_return_payload := et_return.payload.(^Entity_Variable_Payload)
		testing.expect_value(t, et_return_payload.section, Entity_Parameter_Section.Function_Tables)
		testing.expect_value(t, et_return.decl_info.type_clause.form, ast.Data_Type_Form.Structure)
	}
	if et_untyped != nil {
		testing.expect(t, et_untyped.decl_info.type_clause == nil)
	}
	testing.expect_value(t, len(payload.exceptions), 2)
}

@(test)
root_semantic_checker_collects_module_decl_at_name_range :: proc(t: ^testing.T) {
	source := `MODULE z_pai INPUT.
ENDMODULE.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://module_decl_range.abap")
	module := checker_test_lookup(t, &project, file.root_scope, .Routine, "z_pai", .Module)
	if module == nil {
		return
	}

	testing.expect_value(t, source[module.name_range.start:module.name_range.end], "z_pai")

	query := semantic_query(&project, &checker, file)
	decl_query := semantic_query_decls(query)
	name_offset := checker_test_find_text(source, "z_pai")
	input_offset := checker_test_find_text(source, "INPUT")
	testing.expect(t, name_offset >= 0 && input_offset >= 0)
	testing.expect(t, semantic_decl_entity_at_offset(decl_query, name_offset) == module)
	testing.expect(t, semantic_decl_entity_at_offset(decl_query, input_offset) == nil)
	testing.expect(t, semantic_decl_entity_with_kind_and_decl_range(decl_query, .Module, module.name_range) == module)
}

@(test)
root_semantic_stmt_checker_accepts_numeric_literals_for_numeric_text_arguments :: proc(t: ^testing.T) {
	source := `TYPES lvc_outlen TYPE n.
CLASS lcl_column DEFINITION.
  PUBLIC SECTION.
    METHODS set_output_length IMPORTING value TYPE lvc_outlen.
ENDCLASS.
CLASS lcl_column IMPLEMENTATION.
  METHOD set_output_length.
  ENDMETHOD.
ENDCLASS.
DATA lv_length TYPE i.
DATA lo_column TYPE REF TO lcl_column.
lo_column->set_output_length( 20 ).
lo_column->set_output_length( lv_length ).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_numeric_text_arg_conversions.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Argument_Type), 1)
}

@(test)
root_semantic_stmt_checker_accepts_string_expression_and_text_literal_arguments :: proc(t: ^testing.T) {
	source := `TYPES enddatum TYPE d.
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS set_value IMPORTING iv_value TYPE string.
    CLASS-METHODS read_date IMPORTING endda TYPE enddatum.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD set_value.
  ENDMETHOD.
  METHOD read_date.
  ENDMETHOD.
ENDCLASS.
lcl_demo=>set_value( sy-datum(4) && '-' && sy-datum+4(2) && '-' && sy-datum+6(2) ).
lcl_demo=>set_value( sy-uzeit(2) && ':' && sy-uzeit+2(2) && ':' && sy-uzeit+4(2) ).
lcl_demo=>read_date( endda = '99991231' ).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_arg_conversions.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Argument_Type), 0)
}

@(test)
root_semantic_stmt_checker_reports_call_function_exception_message_type :: proc(t: ^testing.T) {
	source := `DATA lv_msg TYPE string.
DATA lv_text TYPE c.
CALL FUNCTION 'Z_DEMO'
  EXCEPTIONS
    system_failure = 1 MESSAGE lv_msg
    communication_failure = 2 MESSAGE lv_text
    failed = 3.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_func_exception_message.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Argument_Type), 1)
}

@(test)
root_semantic_stmt_checker_skips_required_function_parameters_for_parameter_table :: proc(t: ^testing.T) {
	source := `FUNCTION z_required
  IMPORTING iv_required TYPE i
  CHANGING cv_required TYPE i.
ENDFUNCTION.
DATA lt_params TYPE STANDARD TABLE OF string.
DATA lt_exceptions TYPE STANDARD TABLE OF string.
CALL FUNCTION 'Z_REQUIRED'
  PARAMETER-TABLE lt_params
  EXCEPTION-TABLE lt_exceptions.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://stmt_func_parameter_table.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Missing_Required_Parameter), 0)
	lt_params := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_params", .Variable)
	lt_exceptions := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_exceptions", .Variable)
	testing.expect(t, .Used in lt_params.flags)
	testing.expect(t, .Used in lt_exceptions.flags)
}

@(test)
root_semantic_stmt_checker_accepts_writable_raw_selector_function_arguments :: proc(t: ^testing.T) {
	source := `TYPES ty_texts TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
TYPES: BEGIN OF ty_function,
         tables TYPE ty_texts,
       END OF ty_function.
FUNCTION z_write_tables
  TABLES tables_parameter TYPE ty_texts.
ENDFUNCTION.
DATA lt_functions TYPE STANDARD TABLE OF ty_function WITH DEFAULT KEY.
FIELD-SYMBOLS <ls_func> LIKE LINE OF lt_functions.
LOOP AT lt_functions ASSIGNING <ls_func>.
  CALL FUNCTION 'Z_WRITE_TABLES'
    TABLES
      tables_parameter = <ls_func>-tables.
ENDLOOP.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_func_raw_selector_args.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Argument_Type), 0)
}

@(test)
root_semantic_stmt_checker_checks_internal_table_row_targets :: proc(t: ^testing.T) {
	source := `TYPES ty_times TYPE STANDARD TABLE OF t WITH DEFAULT KEY.
DATA lt_times TYPE ty_times.
DATA lv_time TYPE t.
FIELD-SYMBOLS <lv_time> TYPE t.
FIELD-SYMBOLS <lv_date> TYPE d.

LOOP AT lt_times INTO lv_time.
ENDLOOP.
READ TABLE lt_times ASSIGNING <lv_time> INDEX 1.
LOOP AT lt_times ASSIGNING <lv_date>.
ENDLOOP.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_table_rows.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 1)
}

@(test)
root_semantic_stmt_checker_dispatches_control_and_write_operands :: proc(t: ^testing.T) {
	source := `DATA lv_count TYPE i.
DATA lv_target TYPE string.

DO lv_count TIMES.
ENDDO.
CASE lv_count.
  WHEN lv_count.
ENDCASE.
WRITE lv_count TO lv_target.`

	project := project_make()
	defer project_destroy(&project)

	_, file := checker_test_check_source(t, &project, source, "mem://stmt_dispatch.abap")

	lv_count := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_count", .Variable)
	lv_target := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_target", .Variable)
	testing.expect(t, lv_count != nil && .Used in lv_count.flags)
	testing.expect(t, lv_target != nil && .Used in lv_target.flags)
}

@(test)
root_semantic_sql_checker_reports_local_source_and_field_diagnostics :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF zflight,
         carrid TYPE string,
       END OF zflight.
DATA lv_text TYPE string.

SELECT connid FROM zflight INTO @lv_text.
SELECT carrid FROM zmissing INTO @DATA(lt_missing).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://sql_local_diagnostics.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Open_Sql_Source), 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Generic_Table_Type), 0)
}

@(test)
root_semantic_sql_checker_infers_inline_table_row_fields :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF scarr,
         carrid TYPE string,
         carrname TYPE string,
       END OF scarr.
DATA lv_carrid TYPE string.

SELECT carrid, carrname
  FROM scarr
  INTO TABLE @DATA(lt_scarr).
READ TABLE lt_scarr INTO DATA(ls_scarr) INDEX 1.
lv_carrid = ls_scarr-carrid.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://sql_inline_table.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 0)
	lt_scarr := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_scarr", .Variable)
	testing.expect(t, lt_scarr != nil && lt_scarr.type != nil)
	if lt_scarr == nil || lt_scarr.type == nil {
		return
	}
	testing.expect_value(t, lt_scarr.type.kind, Type_Kind.Table)
	row_structure := checker_type_structure(checker_type_row(&checker.builtin_context, lt_scarr.type))
	testing.expect(t, row_structure != nil)
	if row_structure == nil {
		return
	}
	carrid := checker_test_structure_field(t, &project, row_structure, "carrid")
	carrname := checker_test_structure_field(t, &project, row_structure, "carrname")
	testing.expect(t, carrid != nil && carrname != nil)
	testing.expect_value(t, checker_test_type_name(&project, carrid.type), "string")
	testing.expect_value(t, checker_test_type_name(&project, carrname.type), "string")
}

@(test)
root_semantic_sql_checker_reports_scalar_target_conversion_failure :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF e070,
         as4date TYPE d,
       END OF e070.
DATA lv_time TYPE t.

SELECT SINGLE as4date FROM e070 INTO @lv_time.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://sql_scalar_target.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Open_Sql_Into_Target), 1)
}

@(test)
root_semantic_sql_checker_infers_aggregate_alias_inline_structure :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF zrel,
         evtid TYPE string,
         objid TYPE string,
       END OF zrel.
TYPES: BEGIN OF zevt,
         evtid TYPE string,
       END OF zevt.
DATA ls_obj_ids TYPE zrel.

SELECT COUNT( DISTINCT rel~evtid ) AS total_events,
       COUNT( DISTINCT evt~evtid ) AS active_events
  FROM zrel AS rel
  LEFT OUTER JOIN zevt AS evt ON rel~evtid = evt~evtid
  WHERE rel~objid = @ls_obj_ids-objid
  INTO @DATA(ls_event_summary).
DATA lv_total TYPE i.
lv_total = ls_event_summary-total_events.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://sql_aggregate_inline.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 0)
	ls_event_summary := checker_test_lookup(t, &project, file.root_scope, .Value, "ls_event_summary", .Variable)
	testing.expect(t, ls_event_summary != nil && ls_event_summary.type != nil)
	if ls_event_summary == nil || ls_event_summary.type == nil {
		return
	}
	structure := checker_type_structure(ls_event_summary.type)
	testing.expect(t, structure != nil)
	if structure == nil {
		return
	}
	total := checker_test_structure_field(t, &project, structure, "total_events")
	active := checker_test_structure_field(t, &project, structure, "active_events")
	testing.expect_value(t, checker_test_type_name(&project, total.type), "i")
	testing.expect_value(t, checker_test_type_name(&project, active.type), "i")
}

@(test)
root_semantic_sql_checker_combines_join_star_inline_rows :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF zhead,
         id TYPE i,
         text TYPE string,
       END OF zhead.
TYPES: BEGIN OF zitem,
         head_id TYPE i,
         qty TYPE i,
       END OF zitem.

SELECT *
  FROM zhead AS h
  INNER JOIN zitem AS i ON i~head_id = h~id
  INTO TABLE @DATA(lt_rows).
READ TABLE lt_rows INTO DATA(ls_row) INDEX 1.
DATA lv_text TYPE string.
DATA lv_qty TYPE i.
lv_text = ls_row-text.
lv_qty = ls_row-qty.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://sql_join_star.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 0)
	lt_rows := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_rows", .Variable)
	testing.expect(t, lt_rows != nil && lt_rows.type != nil)
	if lt_rows == nil || lt_rows.type == nil {
		return
	}
	row_structure := checker_type_structure(checker_type_row(&checker.builtin_context, lt_rows.type))
	testing.expect(t, row_structure != nil)
	if row_structure == nil {
		return
	}
	fields := [?]string{"id", "text", "head_id", "qty"}
	for name in fields {
		testing.expect(t, checker_test_structure_field(t, &project, row_structure, name) != nil)
	}
}

@(test)
root_semantic_sql_checker_keeps_classic_hosts_out_of_sql_scope :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF tcdobs,
         object TYPE string,
       END OF tcdobs.
DATA mv_object TYPE string.

DELETE FROM tcdobs WHERE object = mv_object.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://sql_classic_host.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 0)
	mv_object := checker_test_lookup(t, &project, file.root_scope, .Value, "mv_object", .Variable)
	testing.expect(t, mv_object != nil && .Used in mv_object.flags)
}

@(test)
root_semantic_sql_checker_keeps_offset_hosts_out_of_sql_scope :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF tcdobs,
         objecttype TYPE string,
       END OF tcdobs.
TYPES: BEGIN OF ty_item,
         obj_name TYPE string,
       END OF ty_item.
DATA ms_item TYPE ty_item.
DATA lv_type_pos TYPE i.

DELETE FROM tcdobs WHERE objecttype = ms_item-obj_name+lv_type_pos.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://sql_offset_host.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 0)
	ms_item := checker_test_lookup(t, &project, file.root_scope, .Value, "ms_item", .Variable)
	lv_type_pos := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_type_pos", .Variable)
	testing.expect(t, ms_item != nil && .Used in ms_item.flags)
	testing.expect(t, lv_type_pos != nil && .Used in lv_type_pos.flags)
}

@(test)
root_semantic_sql_checker_resolves_escaped_ddic_include_field :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF d010inc,
         master TYPE string,
         !include TYPE string,
       END OF d010inc.
DATA iv_prog_name TYPE string.

SELECT SINGLE include
  FROM d010inc
  WHERE include = @iv_prog_name
  INTO @DATA(lv_include).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://sql_include_field.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 0)
}

@(test)
root_semantic_sql_checker_checks_db_dml_sources_and_hosts :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF zinsert_tab,
         id TYPE string,
         status TYPE string,
       END OF zinsert_tab.
TYPES: BEGIN OF zmodify_tab,
         id TYPE string,
         status TYPE string,
       END OF zmodify_tab.
TYPES: BEGIN OF zupdate_tab,
         id TYPE string,
         status TYPE string,
       END OF zupdate_tab.
TYPES: BEGIN OF zdelete_tab,
         id TYPE string,
       END OF zdelete_tab.
DATA lt_insert TYPE STANDARD TABLE OF zinsert_tab WITH EMPTY KEY.
DATA ls_modify TYPE zmodify_tab.
DATA lt_delete TYPE STANDARD TABLE OF zdelete_tab WITH EMPTY KEY.
DATA lv_status TYPE string.
DATA lv_id TYPE string.

INSERT zinsert_tab FROM TABLE lt_insert.
MODIFY zmodify_tab FROM ls_modify WHERE id = lv_id.
UPDATE zupdate_tab SET status = lv_status WHERE id = lv_id.
DELETE FROM zdelete_tab WHERE id = lv_id.
DELETE zdelete_tab FROM TABLE lt_delete.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://sql_db_dml.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Open_Sql_Source), 0)
	zupdate_tab := checker_test_lookup(t, &project, file.root_scope, .Type, "zupdate_tab", .Type_Def)
	lv_status := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_status", .Variable)
	lv_id := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_id", .Variable)
	lt_delete := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_delete", .Variable)
	testing.expect(t, zupdate_tab != nil && .Used in zupdate_tab.flags)
	testing.expect(t, lv_status != nil && .Used in lv_status.flags)
	testing.expect(t, lv_id != nil && .Used in lv_id.flags)
	testing.expect(t, lt_delete != nil && .Used in lt_delete.flags)
}

@(test)
root_semantic_internal_table_components_do_not_emit_symbol_candidates :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_nested,
         part TYPE string,
       END OF ty_nested.
TYPES: BEGIN OF ty_row,
         trnid TYPE string,
         evttime TYPE string,
         evtid TYPE string,
         docpos TYPE string,
         item_ref TYPE string,
         s4_status TYPE string,
         docnum TYPE string,
         nested TYPE ty_nested,
       END OF ty_row.
DATA mt_event TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lv_trnid TYPE string.

SORT mt_event BY trnid evttime DESCENDING evtid docpos nested-part.
LOOP AT mt_event INTO DATA(ls_event) WHERE trnid = lv_trnid AND item_ref IS INITIAL.
ENDLOOP.
LOOP AT mt_event INTO DATA(ls_transport) TRANSPORTING docnum nested-part.
ENDLOOP.
DELETE mt_event WHERE s4_status IS INITIAL.
DELETE ADJACENT DUPLICATES FROM mt_event COMPARING docnum nested-part.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://internal_table_components.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 0)
	component_names := [?]string{"trnid", "evttime", "evtid", "docpos", "item_ref", "s4_status", "docnum", "nested"}
	for name in component_names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 0)
	}
	mt_event := checker_test_lookup(t, &project, file.root_scope, .Value, "mt_event", .Variable)
	lv_trnid := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_trnid", .Variable)
	testing.expect(t, mt_event != nil && .Used in mt_event.flags)
	testing.expect(t, lv_trnid != nil && .Used in lv_trnid.flags)
}

@(test)
root_semantic_internal_table_where_rhs_uses_value_resolution :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_component,
         cmptype TYPE string,
         descript TYPE string,
       END OF ty_component.
DATA is_components TYPE STANDARD TABLE OF ty_component WITH EMPTY KEY.
CONSTANTS seoo_cmptype_method TYPE string VALUE '1'.

LOOP AT is_components ASSIGNING FIELD-SYMBOL(<lo_attribute>)
  WHERE cmptype = seoo_cmptype_attribute AND descript IS NOT INITIAL.
ENDLOOP.
LOOP AT is_components ASSIGNING FIELD-SYMBOL(<ls_component>)
  WHERE cmptype = seoo_cmptype_method.
ENDLOOP.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://internal_table_where_rhs.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 0)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "seoo_cmptype_attribute"), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "cmptype"), 0)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "descript"), 0)
	seoo_cmptype_method := checker_test_lookup(t, &project, file.root_scope, .Value, "seoo_cmptype_method", .Constant)
	testing.expect(t, seoo_cmptype_method != nil && .Used in seoo_cmptype_method.flags)
}

@(test)
root_semantic_unknown_internal_table_row_components_stay_local :: proc(t: ^testing.T) {
	source := `DATA mt_event TYPE STANDARD TABLE OF zmissing_row WITH EMPTY KEY.

SORT mt_event BY trnid evttime evtid docpos.
LOOP AT mt_event WHERE item_ref IS INITIAL.
ENDLOOP.
DELETE ADJACENT DUPLICATES FROM mt_event COMPARING docpos.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://unknown_internal_table_row_components.abap")

	component_names := [?]string{"trnid", "evttime", "evtid", "docpos", "item_ref"}
	for name in component_names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 0)
	}
}

@(test)
root_semantic_missing_internal_table_components_diagnose_without_symbol_candidates :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row,
         id TYPE string,
       END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lv_id TYPE string.

SORT lt_rows BY absent.
LOOP AT lt_rows INTO DATA(ls_row) TRANSPORTING lost.
ENDLOOP.
DELETE lt_rows WHERE missing = lv_id.
DELETE ADJACENT DUPLICATES FROM lt_rows COMPARING gone.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://missing_internal_table_components.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 4)
	missing_names := [?]string{"absent", "lost", "missing", "gone"}
	for name in missing_names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 0)
	}
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "lv_id"), 0)
}

@(test)
root_semantic_dynamic_create_type_names_do_not_emit_parenthesized_candidates :: proc(t: ^testing.T) {
	source := `CONSTANTS c_class TYPE string VALUE 'ZCL_CONST_CLASS'.
DATA lr_data TYPE REF TO data.
DATA lo_object TYPE REF TO object.
DATA lv_class TYPE string.
FIELD-SYMBOLS <ls_list> TYPE any.

CREATE OBJECT lo_object TYPE ('ZCL_LITERAL_CLASS').
CREATE OBJECT lo_object TYPE (c_class).
CREATE OBJECT lo_object TYPE (lv_class).
CREATE OBJECT lo_object TYPE (<ls_list>-method).
CREATE DATA lr_data TYPE ('ZTY_LITERAL').
CREATE DATA lr_data TYPE REF TO ('ZIF_LITERAL').
CREATE DATA lr_data TYPE STANDARD TABLE OF ('ZTY_ROW') WITH DEFAULT KEY.
CREATE DATA lr_data TYPE ('I').`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://dynamic_create_type_names.abap")

	expected_static := [?]string{"zcl_literal_class", "zcl_const_class", "zty_literal", "zif_literal", "zty_row"}
	for name in expected_static {
		testing.expect_value(t, checker_test_unresolved_candidate_namespace_count(&checker, &project, .Global_Symbol, .Type, name), 1)
	}
	rejected := [?]string{
		"('zcl_literal_class')",
		"(c_class)",
		"(lv_class)",
		"(<ls_list>-method)",
		"('zty_literal')",
		"('zif_literal')",
		"('zty_row')",
		"('i')",
		"lv_class",
		"method",
	}
	for name in rejected {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 0)
	}
	c_class := checker_test_lookup(t, &project, file.root_scope, .Value, "c_class", .Constant)
	lv_class := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_class", .Variable)
	ls_list := checker_test_lookup(t, &project, file.root_scope, .Value, "<ls_list>", .Field_Symbol)
	testing.expect(t, c_class != nil && .Used in c_class.flags)
	testing.expect(t, lv_class != nil && .Used in lv_class.flags)
	testing.expect(t, ls_list != nil && .Used in ls_list.flags)
}

@(test)
root_semantic_dynamic_assign_casting_type_names_do_not_emit_parenthesized_candidates :: proc(t: ^testing.T) {
	source := `CONSTANTS c_cast TYPE string VALUE 'ZTY_CAST_CONST'.
DATA lv_cast TYPE string.
DATA hex TYPE x LENGTH 10.
FIELD-SYMBOLS <fs> TYPE any.

ASSIGN hex TO <fs> CASTING TYPE ('ZTY_CAST_LITERAL').
ASSIGN hex TO <fs> CASTING TYPE (c_cast).
ASSIGN hex TO <fs> CASTING TYPE (lv_cast).`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://dynamic_assign_casting_type_names.abap")

	testing.expect_value(t, checker_test_unresolved_candidate_namespace_count(&checker, &project, .Global_Symbol, .Type, "zty_cast_literal"), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_namespace_count(&checker, &project, .Global_Symbol, .Type, "zty_cast_const"), 1)
	rejected := [?]string{"('zty_cast_literal')", "(c_cast)", "(lv_cast)", "lv_cast"}
	for name in rejected {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 0)
	}
	c_cast := checker_test_lookup(t, &project, file.root_scope, .Value, "c_cast", .Constant)
	lv_cast := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_cast", .Variable)
	testing.expect(t, c_cast != nil && .Used in c_cast.flags)
	testing.expect(t, lv_cast != nil && .Used in lv_cast.flags)
}

@(test)
root_semantic_query_finds_declarations_references_and_expr_info :: proc(t: ^testing.T) {
	source := `DATA lv_value TYPE i.
DATA lv_copy TYPE i.
lv_copy = lv_value + 1.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://query_core.abap")
	query := semantic_query(&project, &checker, file)
	decl_query := semantic_query_decls(query)
	ref_query := semantic_query_refs(query)
	fact_query := semantic_query_facts(query)

	decl_offset := checker_test_find_text(source, "lv_value")
	use_offset := checker_test_find_text_last(source, "lv_value")
	literal_offset := checker_test_find_text(source, "1")
	testing.expect(t, decl_offset >= 0 && use_offset > decl_offset && literal_offset >= 0)

	decl := semantic_decl_entity_at_offset(decl_query, decl_offset)
	testing.expect(t, decl != nil)
	if decl == nil {
		return
	}
	testing.expect_value(t, decl.kind, Entity_Kind.Variable)
	testing.expect_value(t, string_interner.load(project.interner, decl.name), "lv_value")
	testing.expect_value(t, source[decl.name_range.start:decl.name_range.end], "lv_value")

	by_range := semantic_decl_entity_with_kind_and_decl_range(decl_query, .Variable, decl.name_range)
	testing.expect(t, by_range == decl)
	type_keyword_offset := checker_test_find_text(source, "TYPE")
	testing.expect(t, type_keyword_offset >= 0)
	testing.expect(t, semantic_decl_entity_at_offset(decl_query, type_keyword_offset) == nil)

	use := semantic_ref_use_at_offset(ref_query, use_offset)
	testing.expect(t, use != nil)
	if use == nil {
		return
	}
	testing.expect(t, use.entity == decl)
	testing.expect(t, use.scope == file.root_scope)

	exact_use := semantic_ref_use_at_range(ref_query, use.node.range)
	testing.expect(t, exact_use == use)

	uses := semantic_ref_resolving_to_entity(ref_query, decl, context.allocator)
	testing.expect_value(t, len(uses), 1)
	if len(uses) > 0 {
		testing.expect(t, uses[0] == use)
	}

	info, info_ok := semantic_fact_expression_info_at_offset(fact_query, use_offset)
	testing.expect(t, info_ok)
	if info_ok {
		testing.expect_value(t, info.kind, Semantic_Expression_Info_Kind.Reference)
		testing.expect_value(t, info.info.mode, ast.Addressing_Mode.Variable)
		testing.expect(t, checker_type_same(info.info.type, decl.type))
		testing.expect(t, info.scope == file.root_scope)
	}

	literal, literal_ok := semantic_fact_operand_info_at_offset(fact_query, literal_offset)
	testing.expect(t, literal_ok)
	if literal_ok {
		testing.expect_value(t, literal.mode, ast.Addressing_Mode.Constant)
		testing.expect_value(t, checker_test_type_name(&project, literal.type), "i")
	}
}

@(test)
root_semantic_query_records_structure_end_name_as_use :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_input_po,
         sort_idx TYPE i,
       END OF ty_input_po.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://query_structure_end_name.abap")
	query := semantic_query(&project, &checker, file)
	decl_query := semantic_query_decls(query)
	ref_query := semantic_query_refs(query)

	decl_offset := checker_test_find_text(source, "ty_input_po")
	end_offset := checker_test_find_text_last(source, "ty_input_po")
	testing.expect(t, decl_offset >= 0 && end_offset > decl_offset)

	decl := semantic_decl_entity_at_offset(decl_query, decl_offset)
	testing.expect(t, decl != nil)
	if decl == nil {
		return
	}
	use := semantic_ref_use_at_offset(ref_query, end_offset)
	testing.expect(t, use != nil)
	if use != nil {
		testing.expect(t, use.entity == decl)
		range := semantic_entity_use_range(use^)
		testing.expect_value(t, source[range.start:range.end], "ty_input_po")
	}

	uses := semantic_ref_resolving_to_entity(ref_query, decl, context.allocator)
	testing.expect_value(t, len(uses), 1)
}

@(test)
root_semantic_query_finds_named_call_argument_parameter_reference_range :: proc(t: ^testing.T) {
	source := `CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING iv_value TYPE i.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.
lcl_demo=>run( iv_value = 1 ).`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://query_named_call_argument.abap")
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Named_Parameter), 0)

	class := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl_demo", .Class)
	if class == nil {
		return
	}
	class_payload := class.payload.(^Entity_Object_Payload)
	method := checker_test_lookup(t, &project, class_payload.definition_scope, .Routine, "run", .Method)
	if method == nil {
		return
	}
	method_payload := method.payload.(^Entity_Routine_Payload)
	parameter := checker_test_lookup(t, &project, method_payload.signature_scope, .Value, "iv_value", .Parameter)
	if parameter == nil {
		return
	}

	arg_offset := checker_test_find_text(source, "iv_value =")
	testing.expect(t, arg_offset >= 0)
	if arg_offset < 0 {
		return
	}

	query := semantic_query(&project, &checker, file)
	use := semantic_ref_use_at_offset(semantic_query_refs(query), arg_offset)
	testing.expect(t, use != nil)
	if use == nil {
		return
	}
	testing.expect(t, use.entity == parameter)
	range := semantic_entity_use_range(use^)
	testing.expect_value(t, source[range.start:range.end], "iv_value")
	testing.expect(t, semantic_ref_use_at_range(semantic_query_refs(query), range) == use)
}

@(test)
root_semantic_query_uses_precise_class_header_ranges :: proc(t: ^testing.T) {
	source := `CLASS lcl_parent DEFINITION.
ENDCLASS.
CLASS lcl_child DEFINITION
  INHERITING FROM lcl_parent
  CREATE PUBLIC.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://class_header_ranges.abap")
	query := semantic_query(&project, &checker, file)
	decl_query := semantic_query_decls(query)
	ref_query := semantic_query_refs(query)

	child_offset := checker_test_find_text(source, "lcl_child")
	parent_decl_offset := checker_test_find_text(source, "lcl_parent")
	parent_ref_offset := checker_test_find_text_last(source, "lcl_parent")
	testing.expect(t, child_offset >= 0 && parent_decl_offset >= 0 && parent_ref_offset > parent_decl_offset)

	child := semantic_decl_entity_at_offset(decl_query, child_offset)
	testing.expect(t, child != nil)
	if child != nil {
		testing.expect_value(t, child.kind, Entity_Kind.Class)
		testing.expect_value(t, source[child.name_range.start:child.name_range.end], "lcl_child")
	}

	parent := semantic_decl_entity_at_offset(decl_query, parent_decl_offset)
	parent_use := semantic_ref_use_at_offset(ref_query, parent_ref_offset)
	testing.expect(t, parent != nil)
	testing.expect(t, parent_use != nil)
	if parent != nil && parent_use != nil {
		testing.expect(t, parent_use.entity == parent)
		range := semantic_entity_use_range(parent_use^)
		testing.expect_value(t, source[range.start:range.end], "lcl_parent")
		testing.expect(t, semantic_ref_use_at_range(ref_query, range) == parent_use)
	}
}

@(test)
root_semantic_class_payload_keeps_structured_header_metadata :: proc(t: ^testing.T) {
	source := `CLASS lcl_meta DEFINITION PUBLIC FINAL CREATE PRIVATE
  SHARED MEMORY ENABLED
  FOR TESTING RISK LEVEL DANGEROUS DURATION LONG.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	_, file := checker_test_check_source(t, &project, source, "mem://class_header_metadata.abap")
	entity := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl_meta", .Class)
	payload, ok := entity.payload.(^Entity_Object_Payload)
	testing.expect(t, ok)
	if ok && payload != nil {
		testing.expect(t, payload.is_public)
		testing.expect(t, payload.is_final)
		testing.expect(t, payload.is_shared_memory_enabled)
		testing.expect(t, payload.is_for_testing)
		testing.expect_value(t, payload.create_visibility, ast.Oop_Visibility.Private)
		testing.expect_value(t, payload.test_risk_level, ast.Class_Test_Risk_Level.Dangerous)
		testing.expect_value(t, payload.test_duration, ast.Class_Test_Duration.Long)
	}
}

@(test)
root_semantic_interface_payload_keeps_structured_header_metadata :: proc(t: ^testing.T) {
	source := `INTERFACE lif_meta PUBLIC.
ENDINTERFACE.`

	project := project_make()
	defer project_destroy(&project)

	_, file := checker_test_check_source(t, &project, source, "mem://interface_header_metadata.abap")
	entity := checker_test_lookup(t, &project, file.root_scope, .Type, "lif_meta", .Interface)
	payload, ok := entity.payload.(^Entity_Object_Payload)
	testing.expect(t, ok)
	if ok && payload != nil {
		testing.expect(t, payload.is_public)
	}
}

@(test)
root_semantic_oop_load_records_type_uses_and_candidates :: proc(t: ^testing.T) {
	source := `CLASS lcl_target DEFINITION.
ENDCLASS.
INTERFACE lif_demo.
  CLASS lcl_target DEFINITION LOAD.
ENDINTERFACE.
INTERFACE lif_missing LOAD.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://oop_load_semantic.abap")
	target := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl_target", .Class)
	query := semantic_query(&project, &checker, file)
	ref_query := semantic_query_refs(query)

	load_offset := checker_test_find_text_last(source, "lcl_target")
	testing.expect(t, load_offset >= 0)
	load_use := semantic_ref_use_at_offset(ref_query, load_offset)
	testing.expect(t, load_use != nil)
	if load_use != nil {
		testing.expect(t, load_use.entity == target)
		range := semantic_entity_use_range(load_use^)
		testing.expect_value(t, source[range.start:range.end], "lcl_target")
	}

	testing.expect_value(
		t,
		checker_test_unresolved_candidate_namespace_count(
			&checker,
			&project,
			.Interface,
			.Type,
			"lif_missing",
		),
		1,
	)
}

@(test)
root_semantic_query_uses_precise_table_type_key_ranges :: proc(t: ^testing.T) {
	source := `TYPES:
  BEGIN OF ty_order_map,
    odata_property TYPE string,
  END OF ty_order_map,
  tt_order_map TYPE HASHED TABLE OF ty_order_map
    WITH UNIQUE KEY odata_property.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://table_key_ranges.abap")
	query := semantic_query(&project, &checker, file)
	ref_query := semantic_query_refs(query)

	row_type := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_order_map", .Type_Def)
	structure := checker_type_structure(row_type.type)
	field := checker_test_structure_field(t, &project, structure, "odata_property")

	base_ref_offset := checker_test_find_text_last(source, "ty_order_map")
	key_offset := checker_test_find_text_last(source, "odata_property")
	testing.expect(t, base_ref_offset >= 0 && key_offset > base_ref_offset)

	base_use := semantic_ref_use_at_offset(ref_query, base_ref_offset)
	testing.expect(t, base_use != nil)
	if base_use != nil {
		testing.expect(t, base_use.entity == row_type)
		range := semantic_entity_use_range(base_use^)
		testing.expect_value(t, source[range.start:range.end], "ty_order_map")
	}

	key_use := semantic_ref_use_at_offset(ref_query, key_offset)
	testing.expect(t, key_use != nil)
	if key_use != nil {
		testing.expect(t, key_use.entity == field)
		range := semantic_entity_use_range(key_use^)
		testing.expect_value(t, source[range.start:range.end], "odata_property")
	}
}

@(test)
root_semantic_query_returns_project_owned_decl_and_builtin_use_pointers :: proc(t: ^testing.T) {
	source := `DATA gv_value TYPE i.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://query_provider_replacement.abap")
	query := semantic_query(&project, &checker, file)

	decl_offset := checker_test_find_text(source, "gv_value")
	type_offset := checker_test_find_text(source, "i")
	testing.expect(t, decl_offset >= 0 && type_offset >= 0)

	decl := semantic_decl_entity_at_offset(semantic_query_decls(query), decl_offset)
	testing.expect(t, decl != nil)
	if decl != nil {
		testing.expect_value(t, decl.kind, Entity_Kind.Variable)
		testing.expect(t, decl.source_file == file)
	}

	type_use := semantic_ref_use_at_offset(semantic_query_refs(query), type_offset)
	testing.expect(t, type_use != nil)
	if type_use != nil {
		testing.expect_value(t, type_use.entity.kind, Entity_Kind.Type_Def)
		testing.expect(t, .Builtin in type_use.entity.flags)
		testing.expect(t, type_use.entity.source_file == nil)
	}

	type_info, type_info_ok := semantic_fact_operand_info_at_offset(semantic_query_facts(query), type_offset)
	testing.expect(t, type_info_ok)
	if type_info_ok {
		testing.expect_value(t, type_info.mode, ast.Addressing_Mode.Type)
		testing.expect_value(t, checker_test_type_name(&project, type_info.type), "i")
	}
}

@(test)
root_semantic_query_finds_class_members_and_structure_fields :: proc(t: ^testing.T) {
	source := `CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

TYPES: BEGIN OF ty_demo,
         comp TYPE i,
       END OF ty_demo.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://query_decls.abap")
	query := semantic_query(&project, &checker, file)
	decl_query := semantic_query_decls(query)

	method_offset := checker_test_find_text(source, "run")
	member := semantic_decl_class_member_at_offset(decl_query, method_offset)
	testing.expect(t, member != nil)
	if member != nil {
		testing.expect_value(t, member.kind, Entity_Kind.Method)
		testing.expect_value(t, string_interner.load(project.interner, member.name), "run")
	}

	class := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl_demo", .Class)
	testing.expect(t, class != nil)
	member_by_name := semantic_decl_class_member(decl_query, class, "RUN")
	testing.expect(t, member_by_name == member)

	field_offset := checker_test_find_text(source, "comp")
	field := semantic_decl_structure_field_at_offset(decl_query, field_offset)
	testing.expect(t, field != nil)
	if field == nil {
		return
	}
	testing.expect_value(t, field.kind, Entity_Kind.Field)
	testing.expect_value(t, string_interner.load(project.interner, field.name), "comp")
	testing.expect_value(t, source[field.name_range.start:field.name_range.end], "comp")
	type_offset := checker_test_find_text(source, "TYPE")
	testing.expect(t, type_offset >= 0)
	testing.expect(t, semantic_decl_structure_field_at_offset(decl_query, type_offset) == nil)

	ty_demo := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_demo", .Type_Def)
	testing.expect(t, ty_demo != nil)
	if ty_demo != nil {
		structure := checker_type_structure(ty_demo.type)
		direct := semantic_decl_structure_field(decl_query, structure, "COMP")
		testing.expect(t, direct == field)
	}
}

@(test)
root_semantic_query_uses_method_implementation_name_range :: proc(t: ^testing.T) {
	source := `CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_value TYPE i.
  ENDMETHOD.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://query_method_impl_range.abap")
	query := semantic_query(&project, &checker, file)
	decl_query := semantic_query_decls(query)

	decl_offset := checker_test_find_text(source, "run.")
	impl_offset := checker_test_find_text(source, "METHOD run") + len("METHOD ")
	body_offset := checker_test_find_text(source, "lv_value")
	testing.expect(t, decl_offset >= 0 && impl_offset >= len("METHOD ") && body_offset >= 0)

	decl_member := semantic_decl_class_member_at_offset(decl_query, decl_offset)
	impl_member := semantic_decl_class_member_at_offset(decl_query, impl_offset)
	body_member := semantic_decl_class_member_at_offset(decl_query, body_offset)

	testing.expect(t, decl_member != nil)
	testing.expect(t, impl_member == decl_member)
	testing.expect(t, body_member == nil)
	if impl_member != nil {
		range := semantic_member_query_range(impl_member, impl_offset)
		testing.expect_value(t, source[range.start:range.end], "run")
		payload := impl_member.payload.(^Entity_Routine_Payload)
		testing.expect_value(
			t,
			source[payload.implementation_name_range.start:payload.implementation_name_range.end],
			"run",
		)
	}
}

@(test)
root_semantic_query_copies_diagnostics_from_checker_snapshot :: proc(t: ^testing.T) {
	source := `DATA lv_dup TYPE i.
DATA lv_dup TYPE i.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://query_diagnostics.abap")
	query := semantic_query(&project, &checker, file)

	diagnostics := semantic_diagnostic_copies(semantic_query_diagnostics(query), context.allocator)
	testing.expect_value(t, len(diagnostics), len(checker.info.diagnostics))
	testing.expect_value(t, len(diagnostics), 1)
	if len(diagnostics) > 0 {
		testing.expect_value(t, diagnostics[0].kind, Checker_Diagnostic_Kind.Duplicate_Declaration)
		testing.expect_value(t, diagnostics[0].range, checker.info.diagnostics[0].range)
	}
}

@(test)
root_semantic_query_orders_diagnostics_by_file_and_range :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	checker := checker_make(&project)
	file_b := checker_add_file(&checker, "mem://b.abap")
	file_a := checker_add_file(&checker, "mem://a.abap")
	append(
		&checker.info.diagnostics,
		Checker_Diagnostic {
			kind  = .Duplicate_Declaration,
			range = Range{start = 20, end = 21},
			file  = file_b,
		},
	)
	append(
		&checker.info.diagnostics,
		Checker_Diagnostic {
			kind  = .Shadowed_Declaration,
			range = Range{start = 10, end = 11},
			file  = file_a,
		},
	)
	append(
		&checker.info.diagnostics,
		Checker_Diagnostic {
			kind  = .Declaration_Cycle,
			range = Range{start = 5, end = 6},
			file  = file_b,
		},
	)

	query := semantic_query(&project, &checker)
	diagnostics := semantic_diagnostic_copies(semantic_query_diagnostics(query), context.allocator)

	testing.expect_value(t, len(diagnostics), 3)
	if len(diagnostics) == 3 {
		testing.expect(t, diagnostics[0].file == file_a)
		testing.expect_value(t, diagnostics[0].range.start, 10)
		testing.expect(t, diagnostics[1].file == file_b)
		testing.expect_value(t, diagnostics[1].range.start, 5)
		testing.expect(t, diagnostics[2].file == file_b)
		testing.expect_value(t, diagnostics[2].range.start, 20)
	}
}

@(test)
root_semantic_query_completion_reads_lexical_scope_chain :: proc(t: ^testing.T) {
	source := `DATA gv_global TYPE string.
FORM run.
  DATA lv_local TYPE i.
  lv_local = 1.
ENDFORM.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://query_completion.abap")
	query := semantic_query(&project, &checker, file)
	offset := checker_test_find_text_last(source, "lv_local")
	testing.expect(t, offset >= 0)

	items := semantic_completion_items_at_offset(
		semantic_query_completion(query),
		offset,
		"",
		context.allocator,
	)
	local_found := false
	global_found := false
	builtin_found := false
	for item in items {
		name := string_interner.load(project.interner, item.name)
		if name == "lv_local" && item.namespace == .Value && item.source == .Lexical_Scope {
			local_found = true
		}
		if name == "gv_global" && item.namespace == .Value && item.source == .Lexical_Scope {
			global_found = true
		}
		if name == "strlen" && item.namespace == .Routine && item.source == .Builtin_Scope {
			builtin_found = true
		}
	}
	testing.expect(t, local_found)
	testing.expect(t, global_found)
	testing.expect(t, builtin_found)
}
