package abap_frontend_semantic2

import "src:ast"
import "src:parser"
import "src:tokenizer"

import "core:strings"
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
	interned := project_intern_lower_ascii(project, name)
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
	interned := project_intern_lower_ascii(project, name)
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
	interned := project_intern_lower_ascii(project, name)
	field, ok := checker_lookup_structure_field(structure, interned)
	testing.expect(t, ok)
	return field if ok else nil
}

checker_test_type_name :: proc(project: ^Project, typ: ^Type) -> string {
	if typ == nil || typ.name == "" {
		return ""
	}
	return typ.name
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

checker_test_diagnostic_message_count :: proc(
	checker: ^Checker,
	kind: Checker_Diagnostic_Kind,
	message: string,
) -> int {
	count := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind == kind && diagnostic.message == message {
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
	interned := project_intern_lower_ascii(project, name)
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
	interned := project_intern_lower_ascii(project, name)
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
				testing.expect_value(t, abap_bool.type.base.name, "c")
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
			testing.expect_value(t, abap_true.type.name, "abap_bool")
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
			testing.expect_value(t, abap_func_exporting.type.name, "i")
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
		testing.expect_value(t, strlen.type.base.name, "i")
	}

	find_builtin, find_builtin_ok := checker_lookup_builtin_entity(&checker, .Routine, "find")
	testing.expect(t, find_builtin_ok)
	if find_builtin_ok {
		payload, payload_ok := find_builtin.payload.(^Entity_Builtin_Payload)
		testing.expect(t, payload_ok)
		if payload_ok {
			testing.expect_value(t, Builtin_Proc_Id(payload.id), Builtin_Proc_Id.Find)
		}
		testing.expect(t, find_builtin.type != nil)
		testing.expect(t, find_builtin.type.base != nil)
		testing.expect_value(t, find_builtin.type.base.name, "i")
	}

	nmin, nmin_ok := checker_builtin_proc_metadata_by_name("nmin")
	testing.expect(t, nmin_ok)
	if nmin_ok {
		testing.expect_value(t, len(nmin.params), 9)
		testing.expect_value(t, nmin.params[0].name, "val1")
		testing.expect(t, nmin.supports_named_args)
	}

	find, find_ok := checker_builtin_proc_metadata_by_name("find")
	testing.expect(t, find_ok)
	if find_ok {
		testing.expect_value(t, len(find.params), 5)
		testing.expect_value(t, find.params[3].name, "occ")
		testing.expect_value(t, find.return_type, "i")
	}
	find_by_id, find_by_id_ok := checker_builtin_proc_metadata(.Find)
	testing.expect(t, find_by_id_ok)
	if find_by_id_ok {
		testing.expect_value(t, find_by_id.name, "find")
		testing.expect_value(t, len(find_by_id.params), 5)
		testing.expect_value(t, find_by_id.params[3].name, "occ")
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

	subrc_name := project_intern_lower_ascii(&project, "subrc")
	subrc, subrc_ok := checker_lookup_structure_field(syst_payload.structure, subrc_name)
	testing.expect(t, subrc_ok)
	if subrc_ok {
		testing.expect_value(t, subrc.kind, Entity_Kind.Field)
		testing.expect(t, .Builtin in subrc.flags)
		field, field_ok := subrc.payload.(^Entity_Field_Payload)
		testing.expect(t, field_ok)
		if field_ok {
			testing.expect(t, .Has_Type_Ref in field.flags)
			testing.expect_value(t, field.type_ref.base_name, "i")
			testing.expect_value(t, checker_builtin_structure_field_description("syst", "subrc"), "Return code set by many ABAP statements; 0 usually indicates success for the documented statement.")
		}
	}

	screen_name_key := project_intern_lower_ascii(&project, "name")
	screen_name, screen_name_ok := checker_lookup_structure_field(screen_payload.structure, screen_name_key)
	testing.expect(t, screen_name_ok)
	if screen_name_ok {
		testing.expect_value(t, screen_name.kind, Entity_Kind.Field)
		testing.expect(t, .Builtin in screen_name.flags)
		field, field_ok := screen_name.payload.(^Entity_Field_Payload)
		testing.expect(t, field_ok)
		if field_ok {
			testing.expect_value(t, field.type_ref.base_name, "c")
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
			testing.expect_value(t, record.info.type.name, "i")
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
	testing.expect_value(t, structure.fields[0].name, "a")
	testing.expect_value(t, structure.fields[1].name, "b")
	testing.expect_value(t, checker_test_type_name(&project, structure.fields[0].type), "i")
	testing.expect_value(t, checker_test_type_name(&project, structure.fields[1].type), "string")

	include_name := project_intern_lower_ascii(&project, "include")
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
	testing.expect_value(t, structure.fields[0].name, "key")
	testing.expect_value(t, structure.fields[1].name, "bus_msg_no")
	testing.expect_value(t, structure.fields[2].name, "arbgb")
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
		testing.expect_value(t, structure.fields[0].name, "begin")
		testing.expect_value(t, structure.fields[1].name, "end")
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

	checker, file := checker_test_check_source(t, &project, source, "mem://like_line_of.abap")

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
		row_structure := checker_type_structure(checker_type_row(&checker.builtin_context, ty_range.type))
		testing.expect(t, row_structure != nil)
		if row_structure != nil {
			testing.expect_value(t, len(row_structure.fields), 4)
			sign := checker_test_structure_field(t, &project, row_structure, "sign")
			option := checker_test_structure_field(t, &project, row_structure, "option")
			low := checker_test_structure_field(t, &project, row_structure, "low")
			high := checker_test_structure_field(t, &project, row_structure, "high")
			testing.expect_value(t, checker_test_type_name(&project, sign.type), "c")
			testing.expect_value(t, checker_test_type_name(&project, option.type), "c")
			testing.expect_value(t, checker_test_type_name(&project, low.type), "string")
			testing.expect_value(t, checker_test_type_name(&project, high.type), "string")
		}
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

	name := project_intern_lower_ascii(&project, "gv_value")
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
	testing.expect_value(t, len(checker.info.uses), 2)
}

@(test)
root_semantic_checker_reports_duplicate_declarations :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	checker := checker_make(&project)
	file := checker_add_file(&checker, "ZPROG.abap")
	ctx := checker_context_make(&checker, file)

	name := project_intern_lower_ascii(&project, "gv_value")
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
	name := project_intern_lower_ascii(&project, "shared")

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
root_semantic_reports_mismatched_begin_and_end_structure_names :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_line,
         field TYPE string,
       END OF ty_lin.

CONSTANTS: BEGIN OF c_values,
            name TYPE string VALUE '',
          END OF c_value.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://structure_end_names.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Mismatched_Structure_End), 2)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Mismatched_Structure_End,
			"END OF ty_lin does not match BEGIN OF ty_line",
		),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Mismatched_Structure_End,
			"END OF c_value does not match BEGIN OF c_values",
		),
		1,
	)
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Mismatched_Structure_End {
			continue
		}
		text := source[diagnostic.range.start:diagnostic.range.end]
		testing.expect(t, text == "ty_lin" || text == "c_value")
	}
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

	name := project_intern_lower_ascii(&project, "remote_value")
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

	ty_name := project_intern_lower_ascii(&project, "ty_line")
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

	form_name := project_intern_lower_ascii(&project, "add")
	_, form_entity, form_ok := checker_lookup_declaration_from_scope(file.root_scope, .Routine, form_name)
	testing.expect(t, form_ok)
	testing.expect_value(t, form_entity.kind, Entity_Kind.Form)
	form_payload, form_payload_ok := form_entity.payload.(^Entity_Routine_Payload)
	testing.expect(t, form_payload_ok)
	testing.expect(t, form_payload.body_scope != nil)
	testing.expect_value(t, len(form_payload.parameters), 1)

	iv_name := project_intern_lower_ascii(&project, "iv")
	_, iv_entity, iv_ok := checker_lookup_declaration_from_scope(form_payload.body_scope, .Value, iv_name)
	testing.expect(t, iv_ok)
	testing.expect_value(t, iv_entity.kind, Entity_Kind.Parameter)

	lv_name := project_intern_lower_ascii(&project, "lv")
	_, lv_entity, lv_ok := checker_lookup_declaration_from_scope(form_payload.body_scope, .Value, lv_name)
	testing.expect(t, lv_ok)
	testing.expect_value(t, lv_entity.kind, Entity_Kind.Variable)
	testing.expect_value(t, lv_entity.state, Entity_State.Resolved)

	gv_name := project_intern_lower_ascii(&project, "gv")
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

	alias_name := project_intern_lower_ascii(&project, "alias_copy")
	alias_target, alias_ok := checker_lookup_object_member(class, .Routine, alias_name)
	testing.expect(t, alias_ok)
	if alias_ok {
		testing.expect(t, alias_target.owner == iface)
		testing.expect_value(t, alias_target.name, "copy")
	}

	rename_name := project_intern_lower_ascii(&project, "rename")
	_, rename_ok := checker_lookup_object_member(class, .Routine, rename_name)
	testing.expect(t, !rename_ok)
}

@(test)
root_semantic_requires_alias_or_qualified_access_for_interface_methods :: proc(t: ^testing.T) {
	source := `INTERFACE lif_interface.
  METHODS method_name
    IMPORTING
      iv_value TYPE string.
ENDINTERFACE.

CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_interface.
    ALIASES short_name FOR lif_interface~method_name.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
ENDCLASS.

DATA(lo_inst) = NEW lcl_class( ).
lo_inst->method_name(
  iv_value = 'hello'
).
lo_inst->lif_interface~method_name(
  iv_value = 'hello'
).
lo_inst->short_name(
  iv_value = 'hello'
).`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://oop_interface_alias_access.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Inaccessible_Member), 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Named_Parameter), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Missing_Required_Parameter), 0)

	method_offset := checker_test_find_text(source, "lo_inst->method_name") + len("lo_inst->")
	method_range := tokenizer.text_range(method_offset, method_offset + len("method_name"))
	diagnostic_found := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind == .Inaccessible_Member && diagnostic.range == method_range {
			diagnostic_found = strings.contains(diagnostic.message, "ALIASES")
			break
		}
	}
	testing.expect(t, diagnostic_found)

	iface := checker_test_lookup(t, &project, file.root_scope, .Type, "lif_interface", .Interface)
	query := semantic_query(&project, &checker, file)
	short_offset := checker_test_find_text_last(source, "short_name")
	short_range := tokenizer.text_range(short_offset, short_offset + len("short_name"))
	short_use := semantic_ref_use_at_range(semantic_query_refs(query), short_range)
	testing.expect(t, short_use != nil)
	if short_use != nil {
		testing.expect(t, short_use.entity.owner == iface)
		testing.expect_value(t, short_use.entity.name, "method_name")
	}
}

@(test)
root_semantic_checks_oop_alias_targets :: proc(t: ^testing.T) {
	source := `INTERFACE lif_demo.
  METHODS run.
ENDINTERFACE.
INTERFACE lif_other.
  METHODS run.
ENDINTERFACE.
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_demo.
    ALIASES missing_member FOR lif_demo~missing.
    ALIASES other_run FOR lif_other~run.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://oop_alias_target_checks.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Inaccessible_Member), 1)
}

@(test)
root_semantic_reports_missing_interface_method_implementations_on_interfaces_and_aliases :: proc(
	t: ^testing.T,
) {
	source := `INTERFACE lif_interface.
  METHODS method_name
    IMPORTING
      iv_value TYPE string.
ENDINTERFACE.

CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_interface.
    ALIASES short_name FOR lif_interface~method_name.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(
		t,
		&project,
		source,
		"mem://missing_interface_method_impl.abap",
	)

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Missing_Method_Implementation), 2)

	class := checker_test_lookup(t, &project, file.root_scope, .Type, "lcl_class", .Class)
	interfaces_diagnostic_found := false
	aliases_diagnostic_found := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Missing_Method_Implementation {
			continue
		}
		text := source[diagnostic.range.start:diagnostic.range.end]
		testing.expect_value(
			t,
			diagnostic.message,
			"missing implementation for method 'lif_interface~method_name'",
		)
		testing.expect(t, diagnostic.entity != nil && diagnostic.entity.kind == .Method)
		if diagnostic.entity != nil {
			testing.expect_value(t, diagnostic.entity.name, "lif_interface~method_name")
			testing.expect(t, diagnostic.entity.owner == class)
		}
		if text == "lif_interface" {
			interfaces_diagnostic_found = true
		}
		if text == "short_name" {
			aliases_diagnostic_found = true
		}
	}
	testing.expect(t, interfaces_diagnostic_found)
	testing.expect(t, aliases_diagnostic_found)
}

@(test)
root_semantic_accepts_implemented_interface_method_aliases :: proc(t: ^testing.T) {
	source := `INTERFACE lif_interface.
  METHODS method_name.
ENDINTERFACE.

CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_interface.
    ALIASES short_name FOR lif_interface~method_name.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD lif_interface~method_name.
  ENDMETHOD.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(
		t,
		&project,
		source,
		"mem://implemented_interface_method_alias.abap",
	)

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Missing_Method_Implementation), 0)
}

@(test)
root_semantic_rejects_freestanding_oop_aliases :: proc(t: ^testing.T) {
	source := `ALIASES short_name FOR lif_interface~method_name.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://freestanding_alias.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Context), 1)
	diagnostic_found := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind == .Invalid_Context {
			text := source[diagnostic.range.start:diagnostic.range.end]
			diagnostic_found = text == "short_name" &&
			                   diagnostic.message == "ALIASES statement must be declared in a class or interface"
			break
		}
	}
	testing.expect(t, diagnostic_found)
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
	frame := checker_routine_parameter_named(method_payload, project_intern_lower_ascii(&project, "frame"))
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
	testing.expect_value(t, param.name, "preserve_newlines")
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
root_semantic_constant_payload_records_integer_literal_values :: proc(t: ^testing.T) {
	source := `CONSTANTS gc_limit TYPE i VALUE 42.`

	project := project_make()
	defer project_destroy(&project)

	_, file := checker_test_check_source(t, &project, source, "mem://constant_integer_value.abap")
	gc_limit := checker_test_lookup(t, &project, file.root_scope, .Value, "gc_limit", .Constant)
	payload, payload_ok := gc_limit.payload.(^Entity_Constant_Payload)
	testing.expect(t, payload_ok)
	if payload_ok {
		value, value_ok := payload.constant_value.(^Constant_Integer_Value)
		testing.expect(t, value_ok)
		if value_ok {
			testing.expect_value(t, value.value, 42)
		}
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
		testing.expect_value(t, payload.target_interface_name, "lif_demo")
		testing.expect_value(t, payload.target_member_name, "get_value")
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
root_semantic_reports_method_implementation_consistency :: proc(t: ^testing.T) {
	source := `CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS do_something
      IMPORTING
        iv_param TYPE string
      RETURNING
        VALUE(rv_res) TYPE string.

    METHODS method_name
      IMPORTING
        !iv_value TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD do_something1.
    rv_res = iv_param.
  ENDMETHOD.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://method_impl_consistency.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Missing_Method_Implementation), 2)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Missing_Method_Definition), 1)

	missing_do_something := false
	missing_method_name := false
	missing_definition := false
	for diagnostic in checker.info.diagnostics {
		text := source[diagnostic.range.start:diagnostic.range.end]
		if diagnostic.kind == .Missing_Method_Implementation && text == "do_something" {
			missing_do_something = true
			testing.expect_value(t, diagnostic.message, "missing implementation for method 'do_something'")
		}
		if diagnostic.kind == .Missing_Method_Implementation && text == "method_name" {
			missing_method_name = true
		}
		if diagnostic.kind == .Missing_Method_Definition && text == "do_something1" {
			missing_definition = true
			testing.expect_value(t, diagnostic.message, "missing definition for method implementation 'do_something1'")
		}
	}
	testing.expect(t, missing_do_something)
	testing.expect(t, missing_method_name)
	testing.expect(t, missing_definition)
}

@(test)
root_semantic_method_implementation_consistency_accepts_abstract_and_interface_methods :: proc(t: ^testing.T) {
	source := `INTERFACE lif_demo.
  METHODS run.
ENDINTERFACE.

CLASS lcl_abstract DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS optional ABSTRACT.
ENDCLASS.

CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_demo.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD lif_demo~run.
  ENDMETHOD.
ENDCLASS.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://method_impl_consistency_valid.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Missing_Method_Implementation), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Missing_Method_Definition), 0)
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
root_semantic_stmt_checker_rejects_inferred_value_constructor_inline_data :: proc(t: ^testing.T) {
	source := `FORM run.
  DATA(lv_bad) = VALUE #( ).
  DATA(lv_parenthesized) = ( VALUE #( ) ).
  DATA(lv_typed) = VALUE i( ).
  DATA lv_num TYPE i.
  lv_num = VALUE #( ).
ENDFORM.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://inline_value_constructor.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Syntax_Form), 2)
	direct_diagnostic_found := false
	paren_diagnostic_found := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form {
			continue
		}
		text := source[diagnostic.range.start:diagnostic.range.end]
		if text == "VALUE #( )" {
			direct_diagnostic_found = true
		} else if text == "( VALUE #( ) )" {
			paren_diagnostic_found = true
		} else {
			testing.expect(t, false)
		}
		testing.expect_value(t, diagnostic.message, "inline DATA declaration cannot use VALUE #(...)")
	}
	testing.expect(t, direct_diagnostic_found)
	testing.expect(t, paren_diagnostic_found)
}

@(test)
root_semantic_stmt_checker_allows_inferred_value_constructor_inline_data_with_base :: proc(t: ^testing.T) {
	source := `FORM run.
  TYPES itab1 TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
  DATA(base1) = VALUE itab1(
    ( 'x1y1z1' )
    ( 'x2y2z2' )
  ).
  DATA(tab1) = VALUE #( BASE base1
    ( 'A1B1B1' )
    ( 'A2B2B2' )
  ).
ENDFORM.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://inline_value_constructor_base.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Syntax_Form), 0)
	run := checker_test_lookup(t, &project, file.root_scope, .Routine, "run", .Form)
	testing.expect(t, run != nil)
	if run == nil {
		return
	}
	run_payload := run.payload.(^Entity_Routine_Payload)
	base1 := checker_test_lookup(t, &project, run_payload.body_scope, .Value, "base1", .Variable)
	tab1 := checker_test_lookup(t, &project, run_payload.body_scope, .Value, "tab1", .Variable)
	testing.expect(t, base1 != nil && tab1 != nil)
	if base1 != nil && tab1 != nil {
		testing.expect(t, checker_type_same(tab1.type, base1.type))
	}
}

@(test)
root_semantic_expr_checker_accepts_template_format_literals :: proc(t: ^testing.T) {
	source := `FORM run.
  DATA lv_docnum TYPE string.
  DATA lv_width TYPE i.
  lv_docnum = |{ lv_docnum
    ALPHA = IN
    ALIGN = LEFT
    DATE = ISO
    TIME = ENVIRONMENT
    TIMESTAMP = USER
    WIDTH = lv_width
    DECIMALS = 2 }|.
ENDFORM.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://template_format_literals.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "IN"), 0)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "LEFT"), 0)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "ISO"), 0)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "ENVIRONMENT"), 0)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "USER"), 0)

	run := checker_test_lookup(t, &project, file.root_scope, .Routine, "run", .Form)
	testing.expect(t, run != nil)
	if run == nil {
		return
	}
	run_payload := run.payload.(^Entity_Routine_Payload)
	lv_docnum := checker_test_lookup(t, &project, run_payload.body_scope, .Value, "lv_docnum", .Variable)
	lv_width := checker_test_lookup(t, &project, run_payload.body_scope, .Value, "lv_width", .Variable)
	testing.expect(t, lv_docnum != nil && .Used in lv_docnum.flags)
	testing.expect(t, lv_width != nil && .Used in lv_width.flags)
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
root_semantic_stmt_checker_treats_empty_table_expression_as_table_body :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_mseg,
         matnr TYPE string,
       END OF ty_mseg.
DATA gt_mseg TYPE STANDARD TABLE OF ty_mseg WITH EMPTY KEY.
DATA lt_mseg TYPE STANDARD TABLE OF ty_mseg WITH EMPTY KEY.

gt_mseg[] = lt_mseg[].`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://stmt_table_body_assignment.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	gt_mseg := checker_test_lookup(t, &project, file.root_scope, .Value, "gt_mseg", .Variable)
	lt_mseg := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_mseg", .Variable)
	testing.expect(t, gt_mseg != nil && lt_mseg != nil)
	if gt_mseg == nil || lt_mseg == nil {
		return
	}

	assign := file.root.stmts[len(file.root.stmts) - 1].derived_stmt.(^ast.Assign_Stmt)
	lhs := assign.lhs.derived_expr.(^ast.Table_Expr)
	rhs := assign.rhs.derived_expr.(^ast.Table_Expr)
	lhs_info, lhs_ok := checker_test_expr_info_for_node(t, &checker, &lhs.expr_base)
	rhs_info, rhs_ok := checker_test_expr_info_for_node(t, &checker, &rhs.expr_base)
	testing.expect(t, lhs_ok && rhs_ok)
	if lhs_ok {
		testing.expect_value(t, lhs_info.mode, ast.Addressing_Mode.Variable)
		testing.expect(t, lhs_info.is_lhs)
		testing.expect(t, checker_type_same(lhs_info.type, gt_mseg.type))
	}
	if rhs_ok {
		testing.expect_value(t, rhs_info.mode, ast.Addressing_Mode.Value)
		testing.expect(t, !rhs_info.is_lhs)
		testing.expect(t, checker_type_same(rhs_info.type, lt_mseg.type))
	}
}

@(test)
root_semantic_stmt_checker_rejects_table_body_assignment_from_table_line :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_mseg,
         matnr TYPE string,
       END OF ty_mseg.
DATA gt_mseg TYPE STANDARD TABLE OF ty_mseg WITH EMPTY KEY.
DATA lt_mseg TYPE STANDARD TABLE OF ty_mseg WITH EMPTY KEY.

gt_mseg[] = lt_mseg[ 1 ].`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_table_body_from_line_assignment.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 1)
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Incompatible_Assignment_Type {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "lt_mseg[ 1 ]")
	}
}

@(test)
root_semantic_stmt_checker_checks_chained_assignment_targets :: proc(t: ^testing.T) {
	source := `DATA:
  BEGIN OF struct,
    col1 TYPE i VALUE 1,
  END OF struct,
  struct1 LIKE struct,
  struct2 LIKE struct.

struct1 = struct2 = struct.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://stmt_chained_assignment.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	testing.expect_value(t, len(file.root.stmts), 2)
	assign := file.root.stmts[1].derived_stmt.(^ast.Assign_Stmt)
	testing.expect_value(t, len(assign.chain_lhs), 1)
	middle_info, middle_ok := checker_test_expr_info_for_node(t, &checker, &assign.chain_lhs[0].expr_base)
	rhs_info, rhs_ok := checker_test_expr_info_for_node(t, &checker, &assign.rhs.expr_base)
	testing.expect(t, middle_ok && rhs_ok)
	if middle_ok {
		testing.expect(t, middle_info.is_lhs)
	}
	if middle_ok && rhs_ok {
		testing.expect(t, checker_type_same(middle_info.type, rhs_info.type))
	}
}

@(test)
root_semantic_stmt_checker_accepts_move_corresponding_structures :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_mseg_tmp,
         kdauf TYPE string,
         matnr TYPE string,
       END OF ty_mseg_tmp.
TYPES: BEGIN OF ty_output,
         kdauf TYPE string,
         maktx TYPE string,
       END OF ty_output.
DATA ls_mseg_tmp TYPE ty_mseg_tmp.
DATA gs_output TYPE ty_output.

MOVE-CORRESPONDING ls_mseg_tmp TO gs_output.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://stmt_move_corresponding.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	ls_mseg_tmp := checker_test_lookup(t, &project, file.root_scope, .Value, "ls_mseg_tmp", .Variable)
	gs_output := checker_test_lookup(t, &project, file.root_scope, .Value, "gs_output", .Variable)
	testing.expect(t, ls_mseg_tmp != nil && .Used in ls_mseg_tmp.flags)
	testing.expect(t, gs_output != nil && .Used in gs_output.flags)
}

@(test)
root_semantic_stmt_checker_validates_move_corresponding_matching_components :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_mseg_tmp,
         kdauf TYPE string,
         budat TYPE d,
         matnr TYPE string,
       END OF ty_mseg_tmp.
TYPES: BEGIN OF ty_output,
         kdauf TYPE string,
         budat TYPE t,
         maktx TYPE string,
       END OF ty_output.
DATA ls_mseg_tmp TYPE ty_mseg_tmp.
DATA gs_output TYPE ty_output.

MOVE-CORRESPONDING ls_mseg_tmp TO gs_output.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_move_corresponding_mismatch.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 1)
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Incompatible_Assignment_Type {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "ls_mseg_tmp")
	}
}

@(test)
root_semantic_stmt_checker_reports_unresolved_assignment_operands :: proc(t: ^testing.T) {
	source := `FORM run.
DATA lv_text TYPE string.

rv_ok = abap_true.
lv_text = lv_missing.
ENDFORM.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_assignment_unresolved.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)
	seen_rv_ok := false
	seen_missing := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Unresolved_Reference {
			continue
		}
		text := source[diagnostic.range.start:diagnostic.range.end]
		testing.expect_value(t, diagnostic.message, checker_unresolved_variable_message(text))
		if text == "rv_ok" {
			seen_rv_ok = true
		} else if text == "lv_missing" {
			seen_missing = true
		}
	}
	testing.expect(t, seen_rv_ok)
	testing.expect(t, seen_missing)
}

@(test)
root_semantic_stmt_checker_reports_unresolved_clear_operands :: proc(t: ^testing.T) {
	source := `DATA lv_text TYPE string.
CLEAR missing_target.
CLEAR lv_text WITH missing_value.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_clear_unresolved.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_target"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "missing_value"),
		1,
	)

	seen_target := false
	seen_value := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Unresolved_Reference {
			continue
		}
		text := source[diagnostic.range.start:diagnostic.range.end]
		testing.expect_value(t, diagnostic.message, checker_unresolved_variable_message(text))
		if text == "missing_target" {
			seen_target = true
		} else if text == "missing_value" {
			seen_value = true
		}
	}
	testing.expect(t, seen_target)
	testing.expect(t, seen_value)
}

@(test)
root_semantic_stmt_checker_reports_unresolved_if_condition_operands :: proc(t: ^testing.T) {
	source := `IF sdfsdf IS NOT INITIAL.
ENDIF.
IF abap_true = abap_true.
ELSEIF sdfaaa IS INITIAL.
ENDIF.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_if_unresolved.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "sdfsdf"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "sdfaaa"),
		1,
	)

	seen_sdfsdf := false
	seen_sdfaaa := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Unresolved_Reference {
			continue
		}
		text := source[diagnostic.range.start:diagnostic.range.end]
		testing.expect_value(t, diagnostic.message, checker_unresolved_variable_message(text))
		if text == "sdfsdf" {
			seen_sdfsdf = true
		} else if text == "sdfaaa" {
			seen_sdfaaa = true
		}
	}
	testing.expect(t, seen_sdfsdf)
	testing.expect(t, seen_sdfaaa)
}

@(test)
root_semantic_stmt_checker_reports_forward_inline_data_initializer_reference :: proc(t: ^testing.T) {
	source := `DATA(lv_val) = lv_val1.
DATA(lv_val1) = 1.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://stmt_inline_forward_ref.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	_ = checker_test_lookup(t, &project, file.root_scope, .Value, "lv_val", .Variable)
	_ = checker_test_lookup(t, &project, file.root_scope, .Value, "lv_val1", .Variable)

	seen_forward_ref := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Unresolved_Reference {
			continue
		}
		text := source[diagnostic.range.start:diagnostic.range.end]
		testing.expect_value(t, diagnostic.message, checker_unresolved_variable_message(text))
		if text == "lv_val1" {
			seen_forward_ref = true
		}
	}
	testing.expect(t, seen_forward_ref)
}

@(test)
root_semantic_stmt_checker_resolves_describe_operands :: proc(t: ^testing.T) {
	source := `DATA lv_value TYPE string.
DATA lv_lines TYPE i.
DATA lv_length TYPE i.
DATA lv_type TYPE c.
DATA itab TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DESCRIBE TABLE itab LINES lv_lines.
DESCRIBE FIELD lv_value LENGTH lv_length IN CHARACTER MODE.
DESCRIBE FIELD lv_value TYPE lv_type.
DESCRIBE TABLE itab LINES DATA(lv_inline).`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://stmt_describe.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	itab := checker_test_lookup(t, &project, file.root_scope, .Value, "itab", .Variable)
	lv_lines := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_lines", .Variable)
	lv_length := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_length", .Variable)
	lv_type := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_type", .Variable)
	lv_value := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_value", .Variable)
	lv_inline := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_inline", .Variable)
	testing.expect(t, itab != nil && .Used in itab.flags)
	testing.expect(t, lv_lines != nil && .Used in lv_lines.flags)
	testing.expect(t, lv_length != nil && .Used in lv_length.flags)
	testing.expect(t, lv_type != nil && .Used in lv_type.flags)
	testing.expect(t, lv_value != nil && .Used in lv_value.flags)
	testing.expect(t, lv_inline != nil && lv_inline.type != nil)
	if lv_inline != nil && lv_inline.type != nil {
		testing.expect_value(t, checker_test_type_name(&project, lv_inline.type), "i")
	}
}

@(test)
root_semantic_stmt_checker_resolves_dataset_open_read_close_operands :: proc(t: ^testing.T) {
	source := `DATA lv_filename TYPE string.
DATA lv_line TYPE string.
DATA lv_message TYPE string.
DATA lv_repl TYPE c LENGTH 1.
DATA lv_attr TYPE string.
DATA lv_filter TYPE string.
DATA lv_code_page TYPE string.
DATA lv_max_length TYPE i.
DATA lv_length TYPE i.
DATA gt_raw TYPE STANDARD TABLE OF string WITH EMPTY KEY.

OPEN DATASET lv_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT
             MESSAGE lv_message IGNORING CONVERSION ERRORS
             REPLACEMENT CHARACTER lv_repl.
OPEN DATASET lv_filename FOR INPUT IN LEGACY TEXT MODE CODE PAGE lv_code_page
             TYPE lv_attr FILTER lv_filter.
DO.
  READ DATASET lv_filename INTO lv_line MAXIMUM LENGTH lv_max_length ACTUAL LENGTH DATA(lv_actual_length).
  READ DATASET lv_filename INTO lv_line LENGTH lv_length.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  CHECK NOT lv_line IS INITIAL.
  APPEND lv_line TO gt_raw.
ENDDO.

CLOSE DATASET lv_filename.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://stmt_dataset.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	lv_filename := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_filename", .Variable)
	lv_line := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_line", .Variable)
	lv_message := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_message", .Variable)
	lv_repl := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_repl", .Variable)
	lv_attr := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_attr", .Variable)
	lv_filter := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_filter", .Variable)
	lv_code_page := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_code_page", .Variable)
	lv_max_length := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_max_length", .Variable)
	lv_length := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_length", .Variable)
	gt_raw := checker_test_lookup(t, &project, file.root_scope, .Value, "gt_raw", .Variable)
	lv_actual_length := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_actual_length", .Variable)
	entities := [?]^Entity {
		lv_filename,
		lv_line,
		lv_message,
		lv_repl,
		lv_attr,
		lv_filter,
		lv_code_page,
		lv_max_length,
		lv_length,
		gt_raw,
	}
	for entity in entities {
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
	testing.expect(t, lv_actual_length != nil && lv_actual_length.type != nil)
	if lv_actual_length != nil && lv_actual_length.type != nil {
		testing.expect_value(t, checker_test_type_name(&project, lv_actual_length.type), "i")
	}

	open := file.root.stmts[10].derived_stmt.(^ast.Dataset_Stmt)
	do_stmt := file.root.stmts[12].derived_stmt.(^ast.Do_Stmt)
	read_actual := do_stmt.body[0].derived_stmt.(^ast.Dataset_Stmt)
	read_length := do_stmt.body[1].derived_stmt.(^ast.Dataset_Stmt)
	close := file.root.stmts[13].derived_stmt.(^ast.Dataset_Stmt)
	message_info, message_ok := checker_test_expr_info_for_node(t, &checker, &open.message.expr_base)
	read_target_info, read_target_ok := checker_test_expr_info_for_node(t, &checker, &read_actual.target.expr_base)
	actual_length_info, actual_length_ok := checker_test_expr_info_for_node(t, &checker, &read_actual.actual_length.expr_base)
	length_info, length_ok := checker_test_expr_info_for_node(t, &checker, &read_length.length.expr_base)
	close_dataset_info, close_dataset_ok := checker_test_expr_info_for_node(t, &checker, &close.dataset.expr_base)
	testing.expect(t, message_ok && read_target_ok && actual_length_ok && length_ok && close_dataset_ok)
	if message_ok {
		testing.expect(t, message_info.is_lhs)
	}
	if read_target_ok {
		testing.expect(t, read_target_info.is_lhs)
	}
	if actual_length_ok {
		testing.expect(t, actual_length_info.is_lhs)
		testing.expect_value(t, checker_test_type_name(&project, actual_length_info.type), "i")
	}
	if length_ok {
		testing.expect(t, length_info.is_lhs)
	}
	if close_dataset_ok {
		testing.expect(t, !close_dataset_info.is_lhs)
	}
}

@(test)
root_semantic_stmt_checker_diagnoses_invalid_dataset_operands :: proc(t: ^testing.T) {
	source := `DATA lt_text TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA lv_text TYPE string.
DATA lv_count TYPE i.
CONSTANTS gc_text TYPE string VALUE ''.

OPEN DATASET lt_text FOR INPUT IN TEXT MODE MESSAGE gc_text AT POSITION lv_text.
READ DATASET missing_file INTO gc_text MAXIMUM LENGTH lt_text ACTUAL LENGTH lv_text.
READ DATASET lv_text INTO lt_text LENGTH missing_len.
CLOSE DATASET missing_close.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_dataset_invalid.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Syntax_Form), 7)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 3)

	seen_filename_type := false
	seen_message_writable := false
	seen_open_position := false
	seen_missing_file := false
	seen_read_target_writable := false
	seen_maximum_length := false
	seen_actual_length := false
	seen_read_target_type := false
	seen_missing_len := false
	seen_missing_close := false
	for diagnostic in checker.info.diagnostics {
		text := source[diagnostic.range.start:diagnostic.range.end]
		if diagnostic.kind == .Invalid_Syntax_Form && text == "lt_text" &&
		   diagnostic.message == "DATASET filename is not character-like" {
			seen_filename_type = true
		} else if diagnostic.kind == .Invalid_Syntax_Form && text == "gc_text" &&
		          diagnostic.message == "OPEN DATASET MESSAGE target is not writable" {
			seen_message_writable = true
		} else if diagnostic.kind == .Invalid_Syntax_Form && text == "lv_text" &&
		          diagnostic.message == "OPEN DATASET POSITION operand is not integer-compatible" {
			seen_open_position = true
		} else if diagnostic.kind == .Unresolved_Reference && text == "missing_file" {
			seen_missing_file = true
			testing.expect_value(t, diagnostic.message, "unresolved variable missing_file")
		} else if diagnostic.kind == .Invalid_Syntax_Form && text == "gc_text" &&
		          diagnostic.message == "READ DATASET INTO target is not writable" {
			seen_read_target_writable = true
		} else if diagnostic.kind == .Invalid_Syntax_Form && text == "lt_text" &&
		          diagnostic.message == "READ DATASET MAXIMUM LENGTH operand is not integer-compatible" {
			seen_maximum_length = true
		} else if diagnostic.kind == .Invalid_Syntax_Form && text == "lv_text" &&
		          diagnostic.message == "READ DATASET ACTUAL LENGTH target is not integer-compatible" {
			seen_actual_length = true
		} else if diagnostic.kind == .Invalid_Syntax_Form && text == "lt_text" &&
		          diagnostic.message == "READ DATASET INTO target is not character-like or byte-like" {
			seen_read_target_type = true
		} else if diagnostic.kind == .Unresolved_Reference && text == "missing_len" {
			seen_missing_len = true
			testing.expect_value(t, diagnostic.message, "unresolved variable missing_len")
		} else if diagnostic.kind == .Unresolved_Reference && text == "missing_close" {
			seen_missing_close = true
			testing.expect_value(t, diagnostic.message, "unresolved variable missing_close")
		}
	}
	testing.expect(t, seen_filename_type)
	testing.expect(t, seen_message_writable)
	testing.expect(t, seen_open_position)
	testing.expect(t, seen_missing_file)
	testing.expect(t, seen_read_target_writable)
	testing.expect(t, seen_maximum_length)
	testing.expect(t, seen_actual_length)
	testing.expect(t, seen_read_target_type)
	testing.expect(t, seen_missing_len)
	testing.expect(t, seen_missing_close)
}

@(test)
root_semantic_stmt_checker_infers_convert_time_stamp_inline_targets :: proc(t: ^testing.T) {
	source := `DATA lv_date TYPE d.
DATA lv_time TYPE t.
DATA lv_dst TYPE c.
DATA lv_ts TYPE timestamp.
DATA lv_zone TYPE string.
CONVERT DATE lv_date
        TIME lv_time
        INTO TIME STAMP DATA(lv_inline_ts)
        TIME ZONE lv_zone.
CONVERT DATE lv_date
        TIME lv_time
        DAYLIGHT SAVING TIME lv_dst
        INTO TIME STAMP DATA(lv_inline_ts_dst)
        TIME ZONE lv_zone.
CONVERT TIME STAMP lv_ts TIME ZONE lv_zone INTO DATE DATA(lv_inline_date).
CONVERT TIME STAMP lv_ts TIME ZONE lv_zone INTO TIME DATA(lv_inline_time).
CONVERT TIME STAMP lv_ts TIME ZONE lv_zone INTO DATE lv_date TIME lv_time DAYLIGHT SAVING TIME DATA(lv_inline_dst).`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://stmt_convert_timestamp_inline.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	lv_inline_ts := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_inline_ts", .Variable)
	lv_inline_ts_dst := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_inline_ts_dst", .Variable)
	lv_inline_date := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_inline_date", .Variable)
	lv_inline_time := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_inline_time", .Variable)
	lv_inline_dst := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_inline_dst", .Variable)
	testing.expect(
		t,
		lv_inline_ts != nil &&
		lv_inline_ts_dst != nil &&
		lv_inline_date != nil &&
		lv_inline_time != nil &&
		lv_inline_dst != nil,
	)
	if lv_inline_ts == nil ||
	   lv_inline_ts_dst == nil ||
	   lv_inline_date == nil ||
	   lv_inline_time == nil ||
	   lv_inline_dst == nil {
		return
	}

	testing.expect_value(t, checker_test_type_name(&project, lv_inline_ts.type), "timestamp")
	testing.expect_value(t, checker_test_type_name(&project, lv_inline_ts_dst.type), "timestamp")
	testing.expect_value(t, checker_test_type_name(&project, lv_inline_date.type), "d")
	testing.expect_value(t, checker_test_type_name(&project, lv_inline_time.type), "t")
	testing.expect_value(t, checker_test_type_name(&project, lv_inline_dst.type), "c")
}

@(test)
root_semantic_stmt_checker_infers_get_time_stamp_inline_target :: proc(t: ^testing.T) {
	source := `GET TIME STAMP FIELD DATA(lv_modify_timestamp).`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://stmt_get_timestamp_inline.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	lv_modify_timestamp := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_modify_timestamp", .Variable)
	testing.expect(t, lv_modify_timestamp != nil && lv_modify_timestamp.type != nil)
	if lv_modify_timestamp == nil || lv_modify_timestamp.type == nil {
		return
	}
	testing.expect_value(t, checker_test_type_name(&project, lv_modify_timestamp.type), "timestamp")
}

@(test)
root_semantic_stmt_checker_infers_catch_inline_exception_ref_type :: proc(t: ^testing.T) {
	source := `TRY.
CATCH cx_root INTO DATA(lx_error).
ENDTRY.`

	project := project_make()
	defer project_destroy(&project)

	_, file := checker_test_check_source(t, &project, source, "mem://stmt_catch_inline.abap")

	lx_error := checker_test_lookup(t, &project, file.root_scope, .Value, "lx_error", .Variable)
	testing.expect(t, lx_error != nil && lx_error.type != nil)
	if lx_error == nil || lx_error.type == nil {
		return
	}
	testing.expect_value(t, lx_error.type.kind, Type_Kind.Ref)
	testing.expect(t, lx_error.type.base != nil)
	if lx_error.type.base != nil {
		testing.expect_value(t, lx_error.type.base.name, "cx_root")
	}
}

@(test)
root_semantic_stmt_checker_rejects_catch_into_non_ref_target :: proc(t: ^testing.T) {
	source := `DATA lv_str TYPE string.
TRY.
CATCH cx_root INTO lv_str.
ENDTRY.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_catch_into_string.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 1)
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Incompatible_Assignment_Type {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "lv_str")
		testing.expect(t, strings.contains(diagnostic.message, "REF TO cx_root"))
		testing.expect(t, strings.contains(diagnostic.message, "string"))
	}
}

@(test)
root_semantic_stmt_checker_accepts_catch_into_object_ref_target :: proc(t: ^testing.T) {
	source := `DATA lo_error TYPE REF TO object.
TRY.
CATCH cx_root INTO lo_error.
ENDTRY.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_catch_into_object_ref.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 0)
}

@(test)
root_semantic_stmt_checker_reports_unresolved_describe_operands :: proc(t: ^testing.T) {
	source := `DATA lv_value TYPE string.
DESCRIBE TABLE itab LINES lv_lines.
DESCRIBE FIELD lv_value LENGTH lv_length IN CHARACTER MODE.
DESCRIBE FIELD lv_value TYPE lv_type.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_describe_unresolved.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 4)
	seen_itab := false
	seen_lines := false
	seen_length := false
	seen_type := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Unresolved_Reference {
			continue
		}
		text := source[diagnostic.range.start:diagnostic.range.end]
		testing.expect_value(t, diagnostic.message, checker_unresolved_variable_message(text))
		if text == "itab" {
			seen_itab = true
		} else if text == "lv_lines" {
			seen_lines = true
		} else if text == "lv_length" {
			seen_length = true
		} else if text == "lv_type" {
			seen_type = true
		}
	}
	testing.expect(t, seen_itab)
	testing.expect(t, seen_lines)
	testing.expect(t, seen_length)
	testing.expect(t, seen_type)
}

@(test)
root_semantic_stmt_checker_resolves_append_operands :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row,
         text TYPE string,
       END OF ty_row.
DATA ls_row TYPE ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
APPEND ls_row TO lt_rows.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://stmt_append.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	ls_row := checker_test_lookup(t, &project, file.root_scope, .Value, "ls_row", .Variable)
	lt_rows := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_rows", .Variable)
	testing.expect(t, ls_row != nil && .Used in ls_row.flags)
	testing.expect(t, lt_rows != nil && .Used in lt_rows.flags)
}

@(test)
root_semantic_stmt_checker_diagnoses_invalid_append_operands :: proc(t: ^testing.T) {
	source := `DATA lv_text TYPE string.
DATA lv_not_table TYPE string.
DATA lt_text TYPE STANDARD TABLE OF string WITH EMPTY KEY.
CONSTANTS gc_text TYPE string VALUE ''.
APPEND lv_text TO lv_not_table.
APPEND lv_text TO gc_text.
APPEND lv_missing TO lt_text.
APPEND lv_text TO lt_missing.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_append_invalid.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Append_Operand), 2)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)

	seen_not_table := false
	seen_not_writable := false
	seen_missing_source := false
	seen_missing_target := false
	for diagnostic in checker.info.diagnostics {
		text := source[diagnostic.range.start:diagnostic.range.end]
		if diagnostic.kind == .Invalid_Append_Operand && text == "lv_not_table" {
			seen_not_table = true
			testing.expect_value(t, diagnostic.message, "APPEND target is not an internal table")
		} else if diagnostic.kind == .Invalid_Append_Operand && text == "gc_text" {
			seen_not_writable = true
			testing.expect_value(t, diagnostic.message, "APPEND target is not writable")
		} else if diagnostic.kind == .Unresolved_Reference && text == "lv_missing" {
			seen_missing_source = true
			testing.expect_value(t, diagnostic.message, "unresolved variable lv_missing")
		} else if diagnostic.kind == .Unresolved_Reference && text == "lt_missing" {
			seen_missing_target = true
			testing.expect_value(t, diagnostic.message, "unresolved variable lt_missing")
		}
	}
	testing.expect(t, seen_not_table)
	testing.expect(t, seen_not_writable)
	testing.expect(t, seen_missing_source)
	testing.expect(t, seen_missing_target)
}

@(test)
root_semantic_stmt_checker_validates_append_value_constructor_components :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row,
         sign TYPE string,
         option TYPE string,
         low TYPE string,
         high TYPE string,
       END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lv_high TYPE string.

APPEND VALUE #(
  sign = 'I'
  option1 = 'BT'
  low = ls_missing-low
  high = lv_high
) TO lt_rows.

LOOP AT lt_rows INTO DATA(ls_row).
  APPEND VALUE #(
    sign = ls_del-sign
    option = ls_del-option
    low = ls_del-low
    high = ls_del-high
  ) TO lt_rows.
ENDLOOP.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_append_value_components.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 5)
	seen_option1 := false
	seen_missing := false
	seen_ls_del := 0
	for diagnostic in checker.info.diagnostics {
		text := source[diagnostic.range.start:diagnostic.range.end]
		if diagnostic.kind == .Unknown_Field {
			testing.expect_value(t, text, "option1")
			testing.expect_value(t, diagnostic.message, "unknown structure field option1")
			seen_option1 = true
		} else if diagnostic.kind == .Unresolved_Reference {
			testing.expect_value(t, diagnostic.message, checker_unresolved_variable_message(text))
			if text == "ls_missing" {
				seen_missing = true
			} else if text == "ls_del" {
				seen_ls_del += 1
			}
		}
	}
	testing.expect(t, seen_option1)
	testing.expect(t, seen_missing)
	testing.expect_value(t, seen_ls_del, 4)
}

@(test)
root_semantic_expr_checker_resolves_value_constructor_let_body_fields :: proc(t: ^testing.T) {
	source := `DATA:
  BEGIN OF struct,
    col1 TYPE i VALUE 1,
    col2 TYPE i VALUE 2,
    col3 TYPE i VALUE 3,
    col4 TYPE i VALUE 4,
  END OF struct,
  struct2 LIKE struct.

struct2 = VALUE #( LET x = struct2 IN
                   col1 = x-col2
                   col4 = 5 ).`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://value_constructor_let_fields.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	struct2 := checker_test_lookup(t, &project, file.root_scope, .Value, "struct2", .Variable)
	testing.expect(t, struct2 != nil)
	if struct2 == nil {
		return
	}
	structure := checker_type_structure(struct2.type)
	testing.expect(t, structure != nil)
	if structure == nil {
		return
	}
	col1 := checker_test_structure_field(t, &project, structure, "col1")
	col2 := checker_test_structure_field(t, &project, structure, "col2")
	col4 := checker_test_structure_field(t, &project, structure, "col4")
	testing.expect(t, .Used in col1.flags)
	testing.expect(t, .Used in col2.flags)
	testing.expect(t, .Used in col4.flags)
}

@(test)
root_semantic_stmt_checker_diagnoses_invalid_insert_targets :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ztt_osmm_aif_job,
         id TYPE string,
       END OF ztt_osmm_aif_job.
DATA lv_text TYPE string.
DATA lt_text TYPE STANDARD TABLE OF string WITH EMPTY KEY.
CONSTANTS gc_text TYPE string VALUE ''.
INSERT VALUE #(  ) INTO TABLE lv_text.
INSERT VALUE #(  ) INTO TABLE gc_text.
INSERT VALUE #(  ) INTO TABLE ztt_osmm_aif_job.
INSERT VALUE #(  ) INTO TABLE lt_missing.
INSERT VALUE #(  ) INTO TABLE lt_text.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_insert_invalid_target.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Insert_Operand), 3)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)

	seen_not_table := false
	seen_constant := false
	seen_type_target := false
	seen_missing := false
	for diagnostic in checker.info.diagnostics {
		text := source[diagnostic.range.start:diagnostic.range.end]
		if diagnostic.kind == .Invalid_Insert_Operand && text == "lv_text" {
			seen_not_table = true
			testing.expect_value(t, diagnostic.message, "INSERT target is not an internal table")
		} else if diagnostic.kind == .Invalid_Insert_Operand && text == "gc_text" {
			seen_constant = true
			testing.expect_value(t, diagnostic.message, "INSERT target is not writable")
		} else if diagnostic.kind == .Invalid_Insert_Operand && text == "ztt_osmm_aif_job" {
			seen_type_target = true
			testing.expect_value(t, diagnostic.message, "INSERT target is not writable")
		} else if diagnostic.kind == .Unresolved_Reference && text == "lt_missing" {
			seen_missing = true
			testing.expect_value(t, diagnostic.message, "unresolved variable lt_missing")
		}
	}
	testing.expect(t, seen_not_table)
	testing.expect(t, seen_constant)
	testing.expect(t, seen_type_target)
	testing.expect(t, seen_missing)
}

@(test)
root_semantic_stmt_checker_diagnoses_invalid_sort_operands :: proc(t: ^testing.T) {
	source := `DATA lv_not_table TYPE string.
CONSTANTS gc_text TYPE string VALUE ''.
SORT lv_not_table BY field.
SORT gc_text BY field.
SORT itab BY field DESCENDING.
SORT itab BY field.
SORT itab STABLE BY field.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_sort_invalid.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Sort_Operand), 2)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 3)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 0)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "field"), 0)

	seen_not_table := false
	seen_not_writable := false
	seen_unresolved := 0
	for diagnostic in checker.info.diagnostics {
		text := source[diagnostic.range.start:diagnostic.range.end]
		if diagnostic.kind == .Invalid_Sort_Operand && text == "lv_not_table" {
			seen_not_table = true
			testing.expect_value(t, diagnostic.message, "SORT target is not an internal table")
		} else if diagnostic.kind == .Invalid_Sort_Operand && text == "gc_text" {
			seen_not_writable = true
			testing.expect_value(t, diagnostic.message, "SORT target is not writable")
		} else if diagnostic.kind == .Unresolved_Reference && text == "itab" {
			seen_unresolved += 1
			testing.expect_value(t, diagnostic.message, "unresolved variable itab")
		}
	}
	testing.expect(t, seen_not_table)
	testing.expect(t, seen_not_writable)
	testing.expect_value(t, seen_unresolved, 3)
}

@(test)
root_semantic_stmt_checker_resolves_concatenate_lines_of_operands :: proc(t: ^testing.T) {
	source := `CLASS cl_abap_char_utilities DEFINITION.
  PUBLIC SECTION.
    CONSTANTS newline TYPE string VALUE '\n'.
ENDCLASS.
DATA lt_lines TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA lv_text TYPE string.
CONCATENATE LINES OF lt_lines INTO lv_text SEPARATED BY cl_abap_char_utilities=>newline.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://stmt_concatenate_lines_of.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	lt_lines := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_lines", .Variable)
	lv_text := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_text", .Variable)
	char_utilities := checker_test_lookup(t, &project, file.root_scope, .Type, "cl_abap_char_utilities", .Class)
	testing.expect(t, lt_lines != nil && .Used in lt_lines.flags)
	testing.expect(t, lv_text != nil && .Used in lv_text.flags)
	testing.expect(t, char_utilities != nil && .Used in char_utilities.flags)
	if char_utilities == nil {
		return
	}
	char_utilities_payload := char_utilities.payload.(^Entity_Object_Payload)
	newline := checker_test_lookup(t, &project, char_utilities_payload.definition_scope, .Value, "newline", .Constant)
	testing.expect(t, newline != nil && .Used in newline.flags)
}

@(test)
root_semantic_stmt_checker_diagnoses_invalid_concatenate_lines_of_operands :: proc(t: ^testing.T) {
	source := `DATA lv_not_table TYPE string.
CONSTANTS gc_text TYPE string VALUE ''.
CONCATENATE LINES OF lv_not_table INTO gc_text.
CONCATENATE LINES OF lt_missing INTO lv_missing.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_concatenate_lines_of_invalid.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Concatenate_Operand), 2)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)

	seen_not_table := false
	seen_target := false
	seen_missing_source := false
	seen_missing_target := false
	for diagnostic in checker.info.diagnostics {
		text := source[diagnostic.range.start:diagnostic.range.end]
		if diagnostic.kind == .Invalid_Concatenate_Operand && text == "lv_not_table" {
			seen_not_table = true
			testing.expect_value(t, diagnostic.message, "CONCATENATE LINES OF source is not an internal table")
		} else if diagnostic.kind == .Invalid_Concatenate_Operand && text == "gc_text" {
			seen_target = true
			testing.expect_value(t, diagnostic.message, "CONCATENATE INTO target is not writable")
		} else if diagnostic.kind == .Unresolved_Reference && text == "lt_missing" {
			seen_missing_source = true
			testing.expect_value(t, diagnostic.message, "unresolved variable lt_missing")
		} else if diagnostic.kind == .Unresolved_Reference && text == "lv_missing" {
			seen_missing_target = true
			testing.expect_value(t, diagnostic.message, "unresolved variable lv_missing")
		}
	}
	testing.expect(t, seen_not_table)
	testing.expect(t, seen_target)
	testing.expect(t, seen_missing_source)
	testing.expect(t, seen_missing_target)
}

@(test)
root_semantic_stmt_checker_resolves_split_into_table_operands :: proc(t: ^testing.T) {
	source := `DATA lv_text TYPE string.
DATA lt_parts TYPE STANDARD TABLE OF string WITH EMPTY KEY.
SPLIT lv_text AT ',' INTO TABLE lt_parts.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://stmt_split_into_table.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	lv_text := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_text", .Variable)
	lt_parts := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_parts", .Variable)
	testing.expect(t, lv_text != nil && .Used in lv_text.flags)
	testing.expect(t, lt_parts != nil && .Used in lt_parts.flags)
}

@(test)
root_semantic_stmt_checker_diagnoses_invalid_split_operands :: proc(t: ^testing.T) {
	source := `DATA lt_text TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA lv_not_table TYPE string.
CONSTANTS gc_text TYPE string VALUE ''.
SPLIT lt_text AT ',' INTO TABLE lv_not_table.
SPLIT lv_missing AT ',' INTO TABLE lt_missing.
SPLIT 'a,b' AT ',' INTO gc_text.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_split_invalid.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Split_Operand), 3)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)

	seen_source := false
	seen_table_target := false
	seen_scalar_target := false
	seen_missing_source := false
	seen_missing_target := false
	for diagnostic in checker.info.diagnostics {
		text := source[diagnostic.range.start:diagnostic.range.end]
		if diagnostic.kind == .Invalid_Split_Operand && text == "lt_text" {
			seen_source = true
			testing.expect_value(t, diagnostic.message, "SPLIT source is not character-like or byte-like")
		} else if diagnostic.kind == .Invalid_Split_Operand && text == "lv_not_table" {
			seen_table_target = true
			testing.expect_value(t, diagnostic.message, "SPLIT INTO TABLE target is not an internal table")
		} else if diagnostic.kind == .Invalid_Split_Operand && text == "gc_text" {
			seen_scalar_target = true
			testing.expect_value(t, diagnostic.message, "SPLIT INTO target is not writable")
		} else if diagnostic.kind == .Unresolved_Reference && text == "lv_missing" {
			seen_missing_source = true
			testing.expect_value(t, diagnostic.message, "unresolved variable lv_missing")
		} else if diagnostic.kind == .Unresolved_Reference && text == "lt_missing" {
			seen_missing_target = true
			testing.expect_value(t, diagnostic.message, "unresolved variable lt_missing")
		}
	}
	testing.expect(t, seen_source)
	testing.expect(t, seen_table_target)
	testing.expect(t, seen_scalar_target)
	testing.expect(t, seen_missing_source)
	testing.expect(t, seen_missing_target)
}

@(test)
root_semantic_stmt_checker_accepts_shift_operands :: proc(t: ^testing.T) {
	source := `DATA lv_po_number TYPE string.
DATA lv_places TYPE i.
SHIFT lv_po_number LEFT DELETING LEADING '0'.
SHIFT lv_po_number RIGHT BY lv_places PLACES.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://stmt_shift.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	lv_po_number := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_po_number", .Variable)
	lv_places := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_places", .Variable)
	testing.expect(t, lv_po_number != nil && .Used in lv_po_number.flags)
	testing.expect(t, lv_places != nil && .Used in lv_places.flags)
	testing.expect_value(t, len(file.root.stmts), 4)
	shift := file.root.stmts[2].derived_stmt.(^ast.Shift_Stmt)
	target_info, target_ok := checker_test_expr_info_for_node(t, &checker, &shift.target.expr_base)
	pattern_info, pattern_ok := checker_test_expr_info_for_node(t, &checker, &shift.delete_pattern.expr_base)
	testing.expect(t, target_ok && pattern_ok)
	if target_ok {
		testing.expect(t, target_info.is_lhs)
	}
	if pattern_ok {
		testing.expect_value(t, checker_test_type_name(&project, pattern_info.type), "string")
	}
}

@(test)
root_semantic_stmt_checker_diagnoses_invalid_shift_operands :: proc(t: ^testing.T) {
	source := `DATA lt_text TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA lv_text TYPE string.
CONSTANTS gc_text TYPE string VALUE ''.
SHIFT lt_text LEFT.
SHIFT gc_text LEFT.
SHIFT lv_text BY lt_text PLACES.
SHIFT lv_text LEFT DELETING LEADING lt_text.
SHIFT missing_text LEFT DELETING LEADING missing_pattern.
SHIFT lv_text BY missing_places PLACES.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_shift_invalid.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Syntax_Form), 4)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 3)

	seen_target_type := false
	seen_target_writable := false
	seen_places := false
	seen_pattern := false
	seen_missing_target := false
	seen_missing_pattern := false
	seen_missing_places := false
	for diagnostic in checker.info.diagnostics {
		text := source[diagnostic.range.start:diagnostic.range.end]
		if diagnostic.kind == .Invalid_Syntax_Form && text == "lt_text" &&
		   diagnostic.message == "SHIFT target is not character-like or byte-like" {
			seen_target_type = true
		} else if diagnostic.kind == .Invalid_Syntax_Form && text == "gc_text" {
			seen_target_writable = true
			testing.expect_value(t, diagnostic.message, "SHIFT target is not writable")
		} else if diagnostic.kind == .Invalid_Syntax_Form && text == "lt_text" &&
		          diagnostic.message == "SHIFT BY operand is not integer-compatible" {
			seen_places = true
		} else if diagnostic.kind == .Invalid_Syntax_Form && text == "lt_text" &&
		          diagnostic.message == "SHIFT DELETING pattern is not character-like or byte-like" {
			seen_pattern = true
		} else if diagnostic.kind == .Unresolved_Reference && text == "missing_text" {
			seen_missing_target = true
			testing.expect_value(t, diagnostic.message, "unresolved variable missing_text")
		} else if diagnostic.kind == .Unresolved_Reference && text == "missing_pattern" {
			seen_missing_pattern = true
			testing.expect_value(t, diagnostic.message, "unresolved variable missing_pattern")
		} else if diagnostic.kind == .Unresolved_Reference && text == "missing_places" {
			seen_missing_places = true
			testing.expect_value(t, diagnostic.message, "unresolved variable missing_places")
		}
	}
	testing.expect(t, seen_target_type)
	testing.expect(t, seen_target_writable)
	testing.expect(t, seen_places)
	testing.expect(t, seen_pattern)
	testing.expect(t, seen_missing_target)
	testing.expect(t, seen_missing_pattern)
	testing.expect(t, seen_missing_places)
}

@(test)
root_semantic_checker_accepts_find_condense_cond_and_type_forms :: proc(t: ^testing.T) {
	source := `TYPE-POOLS abap.
TYPES ty_text TYPE string.
TYPES ty_ref TYPE REF TO object.
TYPES ty_char TYPE c LENGTH 10.
TYPES ty_amount TYPE p LENGTH 8 DECIMALS 2.
TYPES ty_table TYPE STANDARD TABLE OF string WITH EMPTY KEY.
TYPES ty_sorted TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
TYPES ty_hashed TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
TYPES ty_range TYPE RANGE OF string.
DATA lv_text TYPE ty_text.
DATA lv_condensed TYPE string.
DATA lv_offset TYPE i.
DATA lv_length TYPE i.
DATA lv_count TYPE i.
DATA lr_ref TYPE ty_ref.
DATA lv_code TYPE ty_char.
DATA lv_amount TYPE ty_amount.
DATA lt_text TYPE ty_table.
DATA lt_results TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA lt_range TYPE ty_range.
CONDENSE lv_text.
CONDENSE lv_text NO-GAPS.
FIND FIRST OCCURRENCE OF 'A' IN lv_text MATCH OFFSET lv_offset MATCH LENGTH lv_length.
FIND ALL OCCURRENCES OF REGEX 'A' IN TABLE lt_text MATCH COUNT lv_count RESULTS lt_results.
lv_condensed = condense( val = lv_text del = ' ' ).
lv_offset = find( val = lv_text sub = 'A' occ = 1 ).
lv_condensed = COND #( WHEN lv_offset = 0 THEN lv_text ELSE lv_condensed ).`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://find_condense_cond_types_valid.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	type_names := [?]string{"ty_text", "ty_ref", "ty_char", "ty_amount", "ty_table", "ty_sorted", "ty_hashed", "ty_range"}
	for name in type_names {
		testing.expect(t, checker_test_lookup(t, &project, file.root_scope, .Type, name, .Type_Def) != nil)
	}
	value_names := [?]string{"lv_text", "lv_condensed", "lv_offset", "lv_length", "lv_count", "lt_text", "lt_results"}
	for name in value_names {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
	}
	testing.expect(t, checker_test_lookup(t, &project, file.root_scope, .Value, "lr_ref", .Variable) != nil)
	testing.expect(t, checker_test_lookup(t, &project, file.root_scope, .Value, "lv_code", .Variable) != nil)
	testing.expect(t, checker_test_lookup(t, &project, file.root_scope, .Value, "lv_amount", .Variable) != nil)
	testing.expect(t, checker_test_lookup(t, &project, file.root_scope, .Value, "lt_range", .Variable) != nil)
}

@(test)
root_semantic_checker_diagnoses_invalid_find_condense_cond_and_builtin_forms :: proc(t: ^testing.T) {
	source := `DATA lv_text TYPE string.
DATA lv_num TYPE i.
DATA lt_text TYPE STANDARD TABLE OF string WITH EMPTY KEY.
CONSTANTS gc_text TYPE string VALUE ''.
CONSTANTS gc_index TYPE i VALUE 0.
CONDENSE lt_text.
CONDENSE gc_text.
FIND lv_num IN lv_text.
FIND 'A' IN lt_text.
FIND 'A' IN TABLE lv_text.
FIND 'A' IN lv_text MATCH OFFSET gc_index.
FIND 'A' IN lv_text MATCH COUNT lv_text.
lv_num = COND i( ELSE 1 ).
lv_num = COND i( ELSE 1 WHEN lv_num = 1 THEN lv_num ).
lv_num = COND i( WHEN lv_num = 1 THEN lt_text ELSE lv_num ).
lv_text = condense( val = lt_text ).
lv_num = find( val = lv_text bogus = lv_text ).
lv_num = find( val = lv_text occ = lv_text ).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://find_condense_cond_types_invalid.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "CONDENSE target is not character-like"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "CONDENSE target is not writable"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "FIND pattern is not character-like or byte-like"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "FIND target is not character-like or byte-like"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "FIND IN TABLE target is not an internal table"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "FIND MATCH target is not writable"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "FIND numeric operand is not integer-compatible"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "COND requires at least one WHEN clause"),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "COND ELSE requires a preceding WHEN clause"),
		2,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(&checker, .Invalid_Syntax_Form, "COND WHEN must precede ELSE"),
		1,
	)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Named_Parameter), 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Argument_Type), 2)

	seen_cond_result := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind == .Incompatible_Assignment_Type &&
		   strings.contains(diagnostic.message, "COND branch result is not compatible") {
			seen_cond_result = true
		}
	}
	testing.expect(t, seen_cond_result)
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
root_semantic_stmt_checker_names_unknown_method_parameter :: proc(t: ^testing.T) {
	source := `CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING iv_known TYPE i.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.
DATA lv_value TYPE i.
lcl_demo=>run( EXPORTING iv_missing = lv_value ).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_unknown_method_arg.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Named_Parameter), 1)
	found := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind == .Unknown_Named_Parameter {
			found = true
			testing.expect_value(
				t,
				diagnostic.message,
				"unknown named parameter 'iv_missing' in EXPORTING section for method 'run'",
			)
		}
	}
	testing.expect(t, found)
}

@(test)
root_semantic_stmt_checker_reports_missing_constructor_arguments :: proc(t: ^testing.T) {
	source := `CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_param TYPE string
        iv_param1 TYPE i OPTIONAL.
ENDCLASS.
CLASS lcl_class IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
ENDCLASS.

DATA(lo_inst) = NEW lcl_class( ).
DATA lo_inst1 TYPE REF TO lcl_class.
lo_inst1 = NEW #( ).
CREATE OBJECT lo_inst1.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_constructor_args.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Missing_Required_Parameter), 3)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Missing_Required_Parameter,
			"missing required parameter 'iv_param'",
		),
		3,
	)
}

@(test)
root_semantic_stmt_checker_accepts_supplied_constructor_arguments :: proc(t: ^testing.T) {
	source := `CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_param TYPE string
        iv_param1 TYPE i OPTIONAL.
ENDCLASS.
CLASS lcl_class IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
ENDCLASS.

DATA(lo_inst) = NEW lcl_class( iv_param = 'ok' ).
DATA lo_inst1 TYPE REF TO lcl_class.
lo_inst1 = NEW #( iv_param = 'ok' ).
CREATE OBJECT lo_inst1 EXPORTING iv_param = 'ok'.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_constructor_args_valid.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Missing_Required_Parameter), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Named_Parameter), 0)
}

@(test)
root_semantic_stmt_checker_names_unknown_constructor_parameter :: proc(t: ^testing.T) {
	source := `CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING iv_known TYPE string.
ENDCLASS.
CLASS lcl_class IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
ENDCLASS.

DATA(lo_inst) = NEW lcl_class( iv_missing = 'bad' ).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_constructor_unknown_arg.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Named_Parameter), 1)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Unknown_Named_Parameter,
			"unknown named parameter 'iv_missing' in EXPORTING section for method 'constructor'",
		),
		1,
	)
}

@(test)
root_semantic_stmt_checker_checks_unnamed_constructor_arguments_as_values :: proc(t: ^testing.T) {
	source := `CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_param TYPE string
        iv_param1 TYPE i OPTIONAL.
ENDCLASS.
CLASS lcl_class IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
ENDCLASS.

DATA(lo_literal) = NEW lcl_class( 'some_literal' ).
DATA lv_str TYPE string.
lv_str = 'hello'.
DATA lo_inst TYPE REF TO lcl_class.
lo_inst = NEW lcl_class( lv_str ).
lo_inst = NEW lcl_class( iv_param11111 ).
lo_inst = NEW lcl_class( lv_str 1 ).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_constructor_positional_arg.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Unresolved_Reference,
			"unresolved variable iv_param11111",
		),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"method call allows only one unnamed argument",
		),
		1,
	)
	unresolved_found := false
	too_many_found := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind == .Unresolved_Reference &&
		   diagnostic.message == "unresolved variable iv_param11111" {
			unresolved_found = true
			testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "iv_param11111")
		} else if diagnostic.kind == .Invalid_Syntax_Form &&
		          diagnostic.message == "method call allows only one unnamed argument" {
			too_many_found = true
			testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "1")
		}
	}
	testing.expect(t, unresolved_found)
	testing.expect(t, too_many_found)
}

@(test)
root_semantic_stmt_checker_checks_unnamed_method_arguments_as_values :: proc(t: ^testing.T) {
	source := `CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run
      IMPORTING
        iv_param TYPE string
        iv_param1 TYPE i OPTIONAL.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.

lcl_demo=>run( 'some_literal' ).
DATA lv_str TYPE string.
lv_str = 'hello'.
lcl_demo=>run( lv_str ).
lcl_demo=>run( iv_param11111 ).
lcl_demo=>run( lv_str 1 ).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_method_positional_arg.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Unresolved_Reference,
			"unresolved variable iv_param11111",
		),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"method call allows only one unnamed argument",
		),
		1,
	)
}

@(test)
root_semantic_checker_diagnoses_invalid_constructor_definition_forms :: proc(t: ^testing.T) {
	source := `CLASS lcl_static DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS constructor.
ENDCLASS.

CLASS lcl_exporting DEFINITION.
  PUBLIC SECTION.
    METHODS constructor EXPORTING ev_value TYPE i.
ENDCLASS.

CLASS lcl_changing DEFINITION.
  PUBLIC SECTION.
    METHODS constructor CHANGING cv_value TYPE i.
ENDCLASS.

CLASS lcl_returning DEFINITION.
  PUBLIC SECTION.
    METHODS constructor RETURNING VALUE(rv_value) TYPE i.
ENDCLASS.

CLASS lcl_event DEFINITION.
  PUBLIC SECTION.
    EVENTS changed.
    METHODS constructor FOR EVENT changed OF lcl_event.
ENDCLASS.

CLASS lcl_cc_wrong DEFINITION.
  PUBLIC SECTION.
    METHODS class_constructor.
ENDCLASS.

CLASS lcl_cc_sig DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor IMPORTING iv_value TYPE i.
ENDCLASS.

INTERFACE lif_bad.
  METHODS constructor.
ENDINTERFACE.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://invalid_constructor_forms.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"constructor cannot be declared with CLASS-METHODS",
		),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"constructor allows only IMPORTING parameters and exceptions",
		),
		3,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"constructor cannot be an event handler",
		),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"class constructor must be declared with CLASS-METHODS",
		),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"class constructor cannot declare a signature",
		),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"constructor can only be declared in a class",
		),
		1,
	)
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
root_semantic_stmt_checker_resolves_submit_operands :: proc(t: ^testing.T) {
	source := `REPORT zsubmit_full.
DATA lv_report TYPE string.
DATA lv_variant TYPE string.
DATA lv_prog TYPE string.
DATA lt_rspar TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA lv_bukrs TYPE string.
DATA lv_low TYPE string.
DATA lv_high TYPE string.
DATA lv_sign TYPE string.
DATA lt_vkorg TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA lt_texpr TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA lv_width TYPE i.
DATA lv_lines TYPE i.
DATA ls_pri TYPE string.
DATA ls_arc TYPE string.
DATA lv_user TYPE string.
DATA lv_job TYPE string.
DATA lv_count TYPE string.
DATA lv_lang TYPE string.

START-OF-SELECTION.
  SUBMIT (lv_report)
    USING SELECTION-SCREEN '1100'
    USING SELECTION-SET lv_variant
    USING SELECTION-SETS OF PROGRAM lv_prog
    WITH SELECTION-TABLE lt_rspar
    WITH p_bukrs EQ lv_bukrs
    WITH s_erdat NOT BETWEEN lv_low AND lv_high SIGN lv_sign
    WITH s_vkorg IN lt_vkorg
    WITH FREE SELECTIONS lt_texpr
    LINE-SIZE lv_width
    LINE-COUNT lv_lines
    TO SAP-SPOOL
    SPOOL PARAMETERS ls_pri
    ARCHIVE PARAMETERS ls_arc
    WITHOUT SPOOL DYNPRO
    USER lv_user
    VIA JOB lv_job NUMBER lv_count LANGUAGE lv_lang
    AND RETURN.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://submit_operands.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	names := [?]string {
		"lv_report",
		"lv_variant",
		"lv_prog",
		"lt_rspar",
		"lv_bukrs",
		"lv_low",
		"lv_high",
		"lv_sign",
		"lt_vkorg",
		"lt_texpr",
		"lv_width",
		"lv_lines",
		"ls_pri",
		"ls_arc",
		"lv_user",
		"lv_job",
		"lv_count",
		"lv_lang",
	}
	for name in names {
		entity := checker_test_lookup(t, &project, file.root_scope, .Value, name, .Variable)
		testing.expect(t, entity != nil && .Used in entity.flags)
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 0)
	}
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
root_semantic_stmt_checker_reports_unresolved_call_function_operands :: proc(t: ^testing.T) {
	source := `DATA lv_msg TYPE c.
CALL FUNCTION 'Z_DEMO' DESTINATION c_s4_dest
  TABLES
    return = lt_return_
  EXCEPTIONS
    system_failure = 1 MESSAGE lv_msg1
    communication_failure = 2 MESSAGE lv_msg.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_func_unresolved_operands.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 3)
	seen_dest := false
	seen_return := false
	seen_message := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Unresolved_Reference {
			continue
		}
		text := source[diagnostic.range.start:diagnostic.range.end]
		testing.expect_value(t, diagnostic.message, checker_unresolved_variable_message(text))
		if text == "c_s4_dest" {
			seen_dest = true
		} else if text == "lt_return_" {
			seen_return = true
		} else if text == "lv_msg1" {
			seen_message = true
		}
	}
	testing.expect(t, seen_dest)
	testing.expect(t, seen_return)
	testing.expect(t, seen_message)
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
root_semantic_stmt_checker_rejects_non_iterable_loop_sources :: proc(t: ^testing.T) {
	source := `DATA lv_text TYPE string.
DATA lv_count TYPE i.
DATA lt_text TYPE STANDARD TABLE OF string WITH EMPTY KEY.
RANGES r_text FOR lv_text.

LOOP AT lv_text INTO DATA(lv_char).
ENDLOOP.
LOOP AT lv_count INTO DATA(lv_num).
ENDLOOP.
LOOP AT lt_jobs INTO DATA(ls_job).
ENDLOOP.
LOOP AT lt_text INTO DATA(lv_ok).
ENDLOOP.
LOOP AT r_text INTO DATA(ls_range).
ENDLOOP.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_loop_sources.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Loop_Source), 2)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind == .Invalid_Loop_Source {
			text := source[diagnostic.range.start:diagnostic.range.end]
			testing.expect(t, text == "lv_text" || text == "lv_count")
		} else if diagnostic.kind == .Unresolved_Reference {
			testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "lt_jobs")
			testing.expect_value(t, diagnostic.message, "unresolved variable lt_jobs")
		}
	}
}

@(test)
root_semantic_stmt_checker_accepts_loop_at_screen :: proc(t: ^testing.T) {
	source := `LOOP AT SCREEN.
  IF screen-group1 = 'XYZ'.
    screen-intensified = '1'.
    MODIFY SCREEN.
  ENDIF.
ENDLOOP.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_loop_at_screen.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	screen, screen_ok := checker_lookup_builtin_entity(&checker, .Value, "screen")
	testing.expect(t, screen_ok)
	if screen_ok {
		testing.expect(t, .Used in screen.flags)
	}
	screen_type, screen_type_ok := checker_lookup_builtin_entity(&checker, .Type, "screen")
	testing.expect(t, screen_type_ok)
	if !screen_type_ok {
		return
	}
	structure := checker_type_structure(screen_type.type)
	testing.expect(t, structure != nil)
	if structure == nil {
		return
	}
	screen_field_names := [?]string{"group1", "intensified"}
	for name in screen_field_names {
		field, field_ok := checker_lookup_structure_field(structure, project_intern_lower_ascii(&project, name))
		testing.expect(t, field_ok)
		if field_ok {
			testing.expect(t, .Used in field.flags)
		}
	}
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
root_semantic_checker_accepts_case_filter_reduce_and_constructor_for_forms :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row,
         id TYPE i,
         text TYPE string,
       END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row
  WITH EMPTY KEY
  WITH NON-UNIQUE SORTED KEY by_id COMPONENTS id.
DATA lt_filtered TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lv_id TYPE i.
DATA lv_sum TYPE i.

CASE lv_id.
  WHEN 1.
  WHEN OTHERS.
ENDCASE.

lt_filtered = FILTER #( lt_rows USING KEY by_id WHERE id = lv_id ).
lv_sum = REDUCE i( INIT total = 0 FOR row IN lt_rows WHERE ( id = lv_id ) NEXT total = total + row-id ).
lv_sum = REDUCE i( INIT total = 0 FOR idx = 1 THEN idx + 1 UNTIL idx > 3 NEXT total = total + idx ).
lv_sum = REDUCE i( INIT total = 0 FOR idx = 1 UNTIL idx > 3 NEXT total = total + idx ).`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://constructor_forms_valid.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	lt_rows := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_rows", .Variable)
	lt_filtered := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_filtered", .Variable)
	lv_id := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_id", .Variable)
	lv_sum := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_sum", .Variable)
	testing.expect(t, lt_rows != nil && .Used in lt_rows.flags)
	testing.expect(t, lt_filtered != nil && .Used in lt_filtered.flags)
	testing.expect(t, lv_id != nil && .Used in lv_id.flags)
	testing.expect(t, lv_sum != nil && .Used in lv_sum.flags)
}

@(test)
root_semantic_filter_except_in_where_uses_left_and_right_row_fields :: proc(t: ^testing.T) {
	source := `TYPES:
  BEGIN OF ty_header,
    docnum TYPE string,
  END OF ty_header,
  tt_docnum TYPE STANDARD TABLE OF string WITH EMPTY KEY,
  tt_header TYPE SORTED TABLE OF ty_header WITH NON-UNIQUE KEY docnum.

DATA lt_docnum TYPE tt_docnum.
DATA lt_header TYPE tt_header.
DATA lt_delta TYPE tt_docnum.

lt_delta = FILTER #(
  lt_docnum EXCEPT IN lt_header
  WHERE table_line = docnum
).`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://filter_except_in_where_fields.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "table_line"), 0)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "docnum"), 0)
	lt_docnum := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_docnum", .Variable)
	lt_header := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_header", .Variable)
	lt_delta := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_delta", .Variable)
	testing.expect(t, lt_docnum != nil && .Used in lt_docnum.flags)
	testing.expect(t, lt_header != nil && .Used in lt_header.flags)
	testing.expect(t, lt_delta != nil && .Used in lt_delta.flags)

	where_offset := checker_test_find_text(source, "WHERE table_line = docnum")
	testing.expect(t, where_offset >= 0)
	if where_offset < 0 {
		return
	}
	table_line_offset := where_offset + len("WHERE ")
	docnum_offset := where_offset + len("WHERE table_line = ")
	query := semantic_query(&project, &checker, file)
	fact_query := semantic_query_facts(query)

	table_line_info, table_line_ok := semantic_fact_operand_info_at_offset(fact_query, table_line_offset)
	testing.expect(t, table_line_ok)
	if table_line_ok {
		testing.expect_value(t, table_line_info.mode, ast.Addressing_Mode.Table_Line)
		testing.expect(t, table_line_info.type != nil)
		if table_line_info.type != nil {
			testing.expect_value(t, table_line_info.type.kind, Type_Kind.Builtin)
			testing.expect_value(t, table_line_info.type.name, "string")
		}
	}

	docnum_info, docnum_info_ok := semantic_fact_operand_info_at_offset(fact_query, docnum_offset)
	testing.expect(t, docnum_info_ok)
	if docnum_info_ok {
		testing.expect_value(t, docnum_info.mode, ast.Addressing_Mode.Field)
		testing.expect(t, docnum_info.type != nil)
		if docnum_info.type != nil {
			testing.expect_value(t, docnum_info.type.kind, Type_Kind.Builtin)
			testing.expect_value(t, docnum_info.type.name, "string")
		}
	}

	ty_header := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_header", .Type_Def)
	docnum_field := checker_test_structure_field(t, &project, checker_type_structure(ty_header.type), "docnum")
	docnum_use := semantic_ref_use_at_offset(semantic_query_refs(query), docnum_offset)
	testing.expect(t, docnum_use != nil)
	if docnum_use != nil {
		testing.expect(t, docnum_use.entity == docnum_field)
		range := semantic_entity_use_range(docnum_use^)
		testing.expect_value(t, source[range.start:range.end], "docnum")
	}
}

@(test)
root_semantic_filter_warns_when_explicit_result_row_differs_from_source :: proc(t: ^testing.T) {
	source := `TYPES:
  BEGIN OF ty_aif_job_header,
    mandt  TYPE string,
    docnum TYPE string,
  END OF ty_aif_job_header,
  tt_docnum TYPE STANDARD TABLE OF string WITH EMPTY KEY,
  tt_aif_job_header_sorted TYPE SORTED TABLE OF ty_aif_job_header WITH UNIQUE KEY docnum.

DATA lt_aif_job_header_existing TYPE tt_aif_job_header_sorted.
DATA lt_docnum_wt_ship TYPE tt_docnum.

DATA(lt_delta_docnum_wt_ship1) = FILTER tt_aif_job_header_sorted(
  lt_docnum_wt_ship EXCEPT IN lt_aif_job_header_existing
  WHERE table_line = docnum
).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://filter_explicit_result_shape_warning.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 1)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"FILTER explicit result type is structurally different from the source table",
		),
		1,
	)
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form ||
		   diagnostic.message != "FILTER explicit result type is structurally different from the source table" {
			continue
		}
		testing.expect_value(t, diagnostic.severity, Checker_Diagnostic_Severity.Warning)
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "tt_aif_job_header_sorted")
	}
}

@(test)
root_semantic_filter_inferred_result_uses_source_table_type :: proc(t: ^testing.T) {
	source := `TYPES:
  BEGIN OF ty_aif_job_header,
    mandt  TYPE string,
    docnum TYPE string,
  END OF ty_aif_job_header,
  tt_docnum TYPE STANDARD TABLE OF string WITH EMPTY KEY,
  tt_aif_job_header_sorted TYPE SORTED TABLE OF ty_aif_job_header WITH UNIQUE KEY docnum.

DATA lt_aif_job_header_existing TYPE tt_aif_job_header_sorted.
DATA lt_docnum_wt_ship TYPE tt_docnum.

DATA(lt_delta_docnum_wt_ship1) = FILTER #(
  lt_docnum_wt_ship EXCEPT IN lt_aif_job_header_existing
  WHERE table_line = docnum
).`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://filter_inferred_result_source_type.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	lt_docnum_wt_ship := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_docnum_wt_ship", .Variable)
	lt_delta_docnum_wt_ship1 := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_delta_docnum_wt_ship1", .Variable)
	testing.expect(t, lt_docnum_wt_ship != nil && lt_delta_docnum_wt_ship1 != nil)
	if lt_docnum_wt_ship != nil && lt_delta_docnum_wt_ship1 != nil {
		testing.expect(t, checker_type_same(lt_delta_docnum_wt_ship1.type, lt_docnum_wt_ship.type))
	}
}

@(test)
root_semantic_filter_except_in_where_diagnoses_unknown_left_table_line :: proc(t: ^testing.T) {
	source := `TYPES:
  BEGIN OF ty_header,
    docnum TYPE string,
  END OF ty_header,
  tt_docnum TYPE STANDARD TABLE OF string WITH EMPTY KEY,
  tt_header TYPE SORTED TABLE OF ty_header WITH NON-UNIQUE KEY docnum.

DATA lt_docnum TYPE tt_docnum.
DATA lt_header TYPE tt_header.
DATA lt_delta TYPE tt_docnum.

lt_delta = FILTER #(
  lt_docnum EXCEPT IN lt_header
  WHERE table_line1 = docnum
).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://filter_except_in_where_unknown_left.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 1)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "table_line1"), 0)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "docnum"), 0)
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Unknown_Field {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "table_line1")
		testing.expect_value(t, diagnostic.message, "unknown internal table field table_line1")
	}
}

@(test)
root_semantic_filter_accepts_sorted_hashed_and_secondary_keys :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row,
         id TYPE i,
       END OF ty_row,
       ty_secondary TYPE STANDARD TABLE OF ty_row
         WITH EMPTY KEY
         WITH NON-UNIQUE SORTED KEY by_id COMPONENTS id.
DATA lt_sorted TYPE SORTED TABLE OF ty_row WITH NON-UNIQUE KEY id.
DATA lt_hashed TYPE HASHED TABLE OF ty_row WITH UNIQUE KEY id.
DATA lt_secondary TYPE ty_secondary.
DATA lt_filtered TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lv_id TYPE i.

lt_filtered = FILTER #( lt_sorted WHERE id = lv_id ).
lt_filtered = FILTER #( lt_hashed WHERE id = lv_id ).
lt_filtered = FILTER #( lt_secondary USING KEY by_id WHERE id = lv_id ).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://filter_keyed_tables_valid.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
}

@(test)
root_semantic_filter_rejects_plain_standard_table_lookup :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row,
         id TYPE i,
       END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lt_filtered TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lv_id TYPE i.

lt_filtered = FILTER #( lt_rows WHERE id = lv_id ).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://filter_plain_standard_invalid.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"FILTER requires a sorted or hashed table key",
		),
		1,
	)
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form {
			continue
		}
		if diagnostic.message != "FILTER requires a sorted or hashed table key" {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "lt_rows")
	}
}

@(test)
root_semantic_filter_rejects_unselected_secondary_key :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row,
         id TYPE i,
       END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row
  WITH EMPTY KEY
  WITH NON-UNIQUE SORTED KEY by_id COMPONENTS id.
DATA lt_filtered TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lv_id TYPE i.

lt_filtered = FILTER #( lt_rows WHERE id = lv_id ).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://filter_unselected_secondary_invalid.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"FILTER requires a sorted or hashed table key",
		),
		1,
	)
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form ||
		   diagnostic.message != "FILTER requires a sorted or hashed table key" {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "lt_rows")
	}
}

@(test)
root_semantic_filter_in_validates_membership_table_key :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row,
         id TYPE i,
       END OF ty_row.
DATA lt_source TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lt_filter TYPE SORTED TABLE OF ty_row WITH UNIQUE KEY id.
DATA lt_filter_by_id TYPE STANDARD TABLE OF ty_row
  WITH EMPTY KEY
  WITH UNIQUE HASHED KEY by_id COMPONENTS id.
DATA lt_bad_filter TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lt_filtered TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

lt_filtered = FILTER #( lt_source IN lt_filter WHERE id = id ).
lt_filtered = FILTER #( lt_source IN lt_filter_by_id USING KEY by_id WHERE id = id ).
lt_filtered = FILTER #( lt_source IN lt_bad_filter WHERE id = id ).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://filter_in_membership_key.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"FILTER requires a sorted or hashed table key",
		),
		1,
	)
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form ||
		   diagnostic.message != "FILTER requires a sorted or hashed table key" {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "lt_bad_filter")
	}
}

@(test)
root_semantic_filter_accepts_plain_explicit_result_table_type :: proc(t: ^testing.T) {
	source := `TYPES:
  BEGIN OF ty_aif_job_header,
    mandt  TYPE string,
    docnum TYPE string,
  END OF ty_aif_job_header,
  tt_docnum         TYPE STANDARD TABLE OF string WITH EMPTY KEY,
  tt_docnum_sorted  TYPE SORTED TABLE OF string WITH NON-UNIQUE KEY table_line,
  tt_aif_job_header TYPE SORTED TABLE OF ty_aif_job_header WITH UNIQUE KEY docnum.

DATA lt_aif_job_header_existing TYPE tt_aif_job_header.

DATA lt_docnum_wt_ship TYPE tt_docnum_sorted.
APPEND '100123' TO lt_docnum_wt_ship.

DATA(lt_delta_docnum_wt_ship) = FILTER tt_docnum(
  lt_docnum_wt_ship EXCEPT IN lt_aif_job_header_existing
  WHERE table_line = docnum
).
DATA(lt_delta_docnum_wt_ship1) = FILTER tt_docnum(
  lt_docnum_wt_ship
  WHERE table_line = 'docnum'
).`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://filter_plain_explicit_result_type_valid.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	lt_delta_docnum_wt_ship := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_delta_docnum_wt_ship", .Variable)
	lt_delta_docnum_wt_ship1 := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_delta_docnum_wt_ship1", .Variable)
	testing.expect(t, lt_delta_docnum_wt_ship != nil && lt_delta_docnum_wt_ship1 != nil)
	if lt_delta_docnum_wt_ship != nil && lt_delta_docnum_wt_ship1 != nil {
		testing.expect(t, checker_type_same(lt_delta_docnum_wt_ship.type, lt_delta_docnum_wt_ship1.type))
	}
}

@(test)
root_semantic_checker_rejects_value_constructor_for_in_with_non_table_result_type :: proc(t: ^testing.T) {
	source := `TYPES:
  BEGIN OF ty_line,
    docnum TYPE string,
  END OF ty_line,
  tt_lines TYPE STANDARD TABLE OF ty_line WITH EMPTY KEY.

DATA lt_other_lines TYPE tt_lines.

DATA(lt_lines) = VALUE ty_line(
  FOR ls_line IN lt_other_lines
  ( )
).
DATA(lt_valid_lines) = VALUE tt_lines(
  FOR ls_other IN lt_other_lines
  ( docnum = ls_other-docnum )
).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://value_for_in_result_type.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Syntax_Form), 1)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"VALUE constructor with FOR IN requires an internal table result type",
		),
		1,
	)
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "ty_line")
	}
}

@(test)
root_semantic_checker_reports_unresolved_reduce_for_in_where_values :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_line,
         docnum TYPE string,
       END OF ty_line.
DATA lt_dm_trn TYPE TABLE OF ty_line WITH EMPTY KEY.
DATA(lv_count) =
  REDUCE i(
    INIT count = 0
    FOR item IN lt_dm_trn
    WHERE ( docnum = sdfaaa )
    NEXT count = count + 1 ).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://reduce_for_in_where_unresolved.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	testing.expect_value(
		t,
		checker_test_unresolved_candidate_namespace_count(
			&checker,
			&project,
			.Global_Symbol,
			.Value,
			"sdfaaa",
		),
		1,
	)
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Unresolved_Reference {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "sdfaaa")
		testing.expect_value(t, diagnostic.message, "unresolved variable sdfaaa")
	}
}

@(test)
root_semantic_checker_accepts_constructor_for_groups_group_by_values :: proc(t: ^testing.T) {
	source := `TYPES:
  BEGIN OF ty_struct,
    ebeln TYPE c LENGTH 20,
  END OF ty_struct,
  ty_order TYPE c LENGTH 20,
  tr_orders TYPE RANGE OF ty_order.

DATA lt_all_items TYPE STANDARD TABLE OF ty_struct WITH EMPTY KEY.

DATA(lr_orders) = VALUE tr_orders(
  FOR GROUPS order OF ls_item IN lt_all_items
  GROUP BY ls_item-ebeln
  ( sign = 'I' option = 'EQ' low = order )
).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://constructor_for_groups.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Syntax_Form), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 0)
}

@(test)
root_semantic_checker_reports_incompatible_reduce_for_in_where_values :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_line,
         docnum TYPE string,
       END OF ty_line.
DATA lt_dm_trn TYPE TABLE OF ty_line WITH EMPTY KEY.
DATA ls_line TYPE ty_line.
DATA(lv_count) = REDUCE i(
  INIT count = 0
  FOR item IN lt_dm_trn
  WHERE ( docnum = ls_line )
  NEXT count = count + 1
).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://reduce_for_in_where_incompatible.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Argument_Type), 1)

	diagnostic_found := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Incompatible_Argument_Type {
			continue
		}
		diagnostic_found = true
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "ls_line")
		testing.expect(t, strings.contains(diagnostic.message, "incompatible WHERE operand"))
		testing.expect(t, strings.contains(diagnostic.message, "current type 'ty_line'"))
		testing.expect(t, strings.contains(diagnostic.message, "expected type 'string'"))
	}
	testing.expect(t, diagnostic_found)
}

@(test)
root_semantic_checker_accepts_constructor_for_iterator_reuse_with_same_table_type :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row,
         id TYPE string,
       END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lt_more_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lt_ids TYPE STANDARD TABLE OF string WITH EMPTY KEY.

lt_ids = VALUE #( FOR row IN lt_rows ( row-id ) ).
lt_ids = VALUE #( FOR row IN lt_more_rows ( row-id ) ).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://constructor_for_iterator_reuse_valid.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Constructor_For_Iterator_Reuse), 0)
}

@(test)
root_semantic_checker_rejects_constructor_for_iterator_reuse_with_different_table_type :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row,
         id TYPE string,
       END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lt_sorted_rows TYPE SORTED TABLE OF ty_row WITH UNIQUE KEY id.
DATA lt_ids TYPE STANDARD TABLE OF string WITH EMPTY KEY.

lt_ids = VALUE #( FOR row IN lt_rows ( row-id ) ).
lt_ids = VALUE #( FOR row IN lt_sorted_rows ( row-id ) ).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://constructor_for_iterator_reuse_different_table.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Constructor_For_Iterator_Reuse), 1)
}

@(test)
root_semantic_checker_rejects_constructor_for_iterator_loop_target_reuse :: proc(t: ^testing.T) {
	source := `TYPES:
  tr_docnum TYPE RANGE OF string,
  BEGIN OF ty_delivery_header,
         vbeln TYPE string,
       END OF ty_delivery_header.
DATA lt_delivery_header TYPE STANDARD TABLE OF ty_delivery_header.

DATA(lr_docnum) = VALUE tr_docnum(
  FOR ls_del_hdr IN lt_delivery_header
  ( sign = 'I' option = 'EQ' low = CONV #( ls_del_hdr-vbeln ) )
).
DATA(lr_docnum_2) = VALUE tr_docnum(
  FOR ls_plain IN lt_delivery_header
  ( sign = 'I' option = 'EQ' low = CONV #( ls_plain-vbeln ) )
).

LOOP AT lt_delivery_header INTO DATA(ls_del_hdr).
ENDLOOP.

LOOP AT lt_delivery_header INTO ls_plain.
ENDLOOP.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://constructor_for_iterator_loop_target_reuse.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Constructor_For_Iterator_Reuse), 2)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Duplicate_Declaration), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 0)
	inline_target_offset := checker_test_find_text(source, "DATA(ls_del_hdr)") + len("DATA(")
	testing.expect(t, inline_target_offset >= len("DATA(") - 1)
	inline_target_seen := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind == .Invalid_Constructor_For_Iterator_Reuse &&
		   diagnostic.range.start == inline_target_offset {
			inline_target_seen = true
			testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "ls_del_hdr")
		}
	}
	testing.expect(t, inline_target_seen)
}

@(test)
root_semantic_checker_rejects_constructor_for_iterator_reuse_with_different_row_type :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row_a,
         id TYPE string,
       END OF ty_row_a,
       BEGIN OF ty_row_b,
         id TYPE string,
       END OF ty_row_b.
DATA lt_rows_a TYPE STANDARD TABLE OF ty_row_a WITH EMPTY KEY.
DATA lt_rows_b TYPE STANDARD TABLE OF ty_row_b WITH EMPTY KEY.
DATA lt_ids TYPE STANDARD TABLE OF string WITH EMPTY KEY.

lt_ids = VALUE #( FOR row IN lt_rows_a ( row-id ) ).
lt_ids = VALUE #( FOR row IN lt_rows_b ( row-id ) ).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://constructor_for_iterator_reuse_different_row.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Constructor_For_Iterator_Reuse), 1)
}

@(test)
root_semantic_checker_diagnoses_case_filter_reduce_and_constructor_for_forms :: proc(t: ^testing.T) {
	source := `DATA lv_scalar TYPE i.
DATA lv_result TYPE i.

CASE lv_scalar.
  WHEN OTHERS.
  WHEN 1.
ENDCASE.

lv_result = FILTER i( lv_scalar ).
lv_result = REDUCE i( FOR x IN lv_scalar NEXT total = total + x ).
lv_result = REDUCE i( INIT total = 0 NEXT total = total + 1 ).
lv_result = VALUE i( FOR x IN lv_scalar ( x ) ).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://constructor_forms_invalid.abap")

	testing.expect(t, checker_test_diagnostic_count(&checker, .Invalid_Syntax_Form) >= 9)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"WHEN OTHERS must be the last CASE branch",
		),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"FILTER result type is not an internal table",
		),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"FILTER source is not an internal table",
		),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"FILTER requires a WHERE clause",
		),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"FOR IN source is not an internal table",
		),
		2,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"REDUCE requires an INIT clause",
		),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"REDUCE NEXT assignment must target an INIT variable",
		),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"REDUCE NEXT requires a preceding FOR clause",
		),
		1,
	)
	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"REDUCE requires a FOR clause",
		),
		1,
	)
}

@(test)
root_semantic_checker_reports_unresolved_reduce_for_then_operands :: proc(t: ^testing.T) {
	source := `DATA(lv_val) = REDUCE i( INIT result = 0 FOR i1 = 1 THEN i + 1 UNTIL i > limit NEXT result = result + i ).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://reduce_for_then_unresolved.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 4)
	seen_i := 0
	seen_limit := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Unresolved_Reference {
			continue
		}
		text := source[diagnostic.range.start:diagnostic.range.end]
		testing.expect_value(t, diagnostic.message, checker_unresolved_variable_message(text))
		if text == "i" {
			seen_i += 1
		} else if text == "limit" {
			seen_limit += 1
		}
	}
	testing.expect_value(t, seen_i, 3)
	testing.expect_value(t, seen_limit, 1)
}

@(test)
root_semantic_stmt_checker_resolves_data_cluster_memory_operands :: proc(t: ^testing.T) {
	source := `DATA lv_export TYPE string.
DATA lv_import TYPE string.
DATA lv_id TYPE c LENGTH 10.
EXPORT cluster_name = lv_export TO MEMORY ID lv_id.
IMPORT cluster_name = lv_import FROM MEMORY ID lv_id.
EXPORT cluster_name FROM lv_export TO MEMORY ID 'ID'.
IMPORT cluster_name TO lv_import FROM MEMORY ID 'ID'.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://stmt_data_cluster_memory.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	lv_export := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_export", .Variable)
	lv_import := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_import", .Variable)
	lv_id := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_id", .Variable)
	testing.expect(t, lv_export != nil && .Used in lv_export.flags)
	testing.expect(t, lv_import != nil && .Used in lv_import.flags)
	testing.expect(t, lv_id != nil && .Used in lv_id.flags)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "cluster_name"), 0)
}

@(test)
root_semantic_stmt_checker_reports_unresolved_data_cluster_memory_operands :: proc(t: ^testing.T) {
	source := `EXPORT cluster_name = lv_missing TO MEMORY ID lv_missing_id.
IMPORT cluster_name = lv_target FROM MEMORY ID lv_missing_id.
EXPORT cluster_name = lv_other TO MEMORY ID 'ID'.
IMPORT cluster_name = lv_other_target FROM MEMORY ID 'ID'.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://stmt_data_cluster_memory_unresolved.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 6)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "cluster_name"), 0)
	seen_missing := false
	seen_target := false
	seen_other := false
	seen_other_target := false
	seen_missing_id := 0
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Unresolved_Reference {
			continue
		}
		text := source[diagnostic.range.start:diagnostic.range.end]
		testing.expect_value(t, diagnostic.message, checker_unresolved_variable_message(text))
		if text == "lv_missing" {
			seen_missing = true
		} else if text == "lv_target" {
			seen_target = true
		} else if text == "lv_other" {
			seen_other = true
		} else if text == "lv_other_target" {
			seen_other_target = true
		} else if text == "lv_missing_id" {
			seen_missing_id += 1
		} else {
			testing.expect(t, false)
		}
	}
	testing.expect(t, seen_missing)
	testing.expect(t, seen_target)
	testing.expect(t, seen_other)
	testing.expect(t, seen_other_target)
	testing.expect_value(t, seen_missing_id, 2)
}

@(test)
root_semantic_sql_checker_reports_local_source_and_field_diagnostics :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF zflight,
         carrid TYPE string,
       END OF zflight.
DATA lv_text TYPE string.

SELECT SINGLE connid FROM zflight INTO @lv_text.
SELECT SINGLE carrid FROM zmissing INTO @DATA(lt_missing).`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://sql_local_diagnostics.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Open_Sql_Source), 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Generic_Table_Type), 0)
}

@(test)
root_semantic_sql_cursor_checks_escaped_host_variables :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF e070,
         trstatus TYPE string,
       END OF e070.

OPEN CURSOR WITH HOLD @lv_cursor FOR
  SELECT trstatus
    FROM e070
    WHERE trstatus = @lv_value.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://sql_cursor_hosts.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Unresolved_Reference {
			continue
		}
		text := source[diagnostic.range.start:diagnostic.range.end]
		testing.expect(t, text == "lv_cursor" || text == "lv_value")
		testing.expect_value(t, diagnostic.message, checker_unresolved_variable_message(text))
	}
}

@(test)
root_semantic_sql_cursor_infers_inline_handle_type :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF e070,
         trstatus TYPE string,
       END OF e070.
DATA lv_status TYPE string.

OPEN CURSOR WITH HOLD @DATA(lv_cursor) FOR
  SELECT trstatus
    FROM e070
    WHERE trstatus = '1'.

DO.
  FETCH NEXT CURSOR @lv_cursor
    INTO TABLE @DATA(lt_package)
    PACKAGE SIZE 100.

  lv_status = lt_package[ 1 ]-trstatus.
  lv_status = lt_package[ 1 ]-trstatus1.
ENDDO.

CLOSE CURSOR @lv_cursor.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://sql_cursor_inline.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 1)
	lv_cursor := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_cursor", .Variable)
	testing.expect(t, lv_cursor != nil && lv_cursor.type != nil)
	if lv_cursor == nil || lv_cursor.type == nil {
		return
	}
	testing.expect_value(t, checker_test_type_name(&project, lv_cursor.type), "cursor")

	lt_package := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_package", .Variable)
	testing.expect(t, lt_package != nil && lt_package.type != nil)
	if lt_package == nil || lt_package.type == nil {
		return
	}
	testing.expect_value(t, lt_package.type.kind, Type_Kind.Table)
	row_structure := checker_type_structure(checker_type_row(&checker.builtin_context, lt_package.type))
	testing.expect(t, row_structure != nil)
	if row_structure == nil {
		return
	}
	trstatus := checker_test_structure_field(t, &project, row_structure, "trstatus")
	testing.expect(t, trstatus != nil)
	if trstatus != nil {
		testing.expect_value(t, checker_test_type_name(&project, trstatus.type), "string")
		testing.expect(t, .Used in trstatus.flags)
	}
	found_bad_field := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Unknown_Field {
			continue
		}
		found_bad_field = true
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "trstatus1")
	}
	testing.expect(t, found_bad_field)
}

@(test)
root_semantic_sql_cursor_reports_non_cursor_handle_type :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF e070,
         trstatus TYPE string,
       END OF e070.
DATA lv_cursor TYPE string.

OPEN CURSOR WITH HOLD @lv_cursor FOR
  SELECT trstatus
    FROM e070.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://sql_cursor_mismatch.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 1)
	found := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Incompatible_Assignment_Type {
			continue
		}
		found = true
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "@lv_cursor")
		testing.expect(t, strings.contains(diagnostic.message, "cursor handle is not compatible"))
		testing.expect(t, strings.contains(diagnostic.message, "current type 'string'"))
		testing.expect(t, strings.contains(diagnostic.message, "expected type 'cursor'"))
	}
	testing.expect(t, found)
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
root_semantic_sql_checker_requires_group_by_for_aggregate_with_plain_field :: proc(
	t: ^testing.T,
) {
	source := `TYPES: BEGIN OF ztrn_evt,
         trnid TYPE string,
         evtid TYPE string,
       END OF ztrn_evt.
TYPES: BEGIN OF zevt,
         evtid TYPE string,
         creation_time TYPE t,
         bizstep TYPE string,
       END OF zevt.
DATA lr_trnid TYPE RANGE OF string.

SELECT q~trnid, MAX( w~creation_time ) AS creation_time, COUNT( * ) AS count
  FROM ztrn_evt AS q
  JOIN zevt AS w ON w~evtid = q~evtid AND w~bizstep = '013'
  INTO TABLE @DATA(lt_trn_evt)
  WHERE trnid IN @lr_trnid
  ORDER BY creation_time DESCENDING.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://sql_missing_group_by.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Open_Sql_Group_By), 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 0)
	found := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Open_Sql_Group_By {
			continue
		}
		found = true
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "trnid")
		testing.expect_value(t, diagnostic.message, OPEN_SQL_REQUIRED_GROUP_BY_MESSAGE)
	}
	testing.expect(t, found)
}

@(test)
root_semantic_sql_checker_accepts_group_by_for_aggregate_with_plain_field :: proc(
	t: ^testing.T,
) {
	source := `TYPES: BEGIN OF ztrn_evt,
         trnid TYPE string,
         evtid TYPE string,
       END OF ztrn_evt.
TYPES: BEGIN OF zevt,
         evtid TYPE string,
         creation_time TYPE t,
         bizstep TYPE string,
       END OF zevt.
DATA lr_trnid TYPE RANGE OF string.

SELECT q~trnid, MAX( w~creation_time ) AS creation_time, COUNT( * ) AS count
  FROM ztrn_evt AS q
  JOIN zevt AS w ON w~evtid = q~evtid AND w~bizstep = '013'
  INTO TABLE @DATA(lt_trn_evt)
  WHERE trnid IN @lr_trnid
  GROUP BY q~trnid
  ORDER BY creation_time DESCENDING.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://sql_group_by_present.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Open_Sql_Group_By), 0)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 0)
}

@(test)
root_semantic_sql_checker_warns_order_by_field_not_selected :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF zrep_evt,
         evtid TYPE string,
         status_rep_evt TYPE string,
         modified_time TYPE t,
         creation_time TYPE t,
       END OF zrep_evt.

SELECT evtid, status_rep_evt, modified_time
  FROM zrep_evt
  INTO TABLE @DATA(lt_rep_evt)
  ORDER BY creation_time DESCENDING.

SELECT *
  FROM zrep_evt
  INTO TABLE @DATA(lt_all)
  ORDER BY creation_time DESCENDING.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://sql_order_by_projection.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Open_Sql_Order_By), 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 0)
	found := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Open_Sql_Order_By {
			continue
		}
		found = true
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "creation_time")
		testing.expect_value(t, diagnostic.severity, Checker_Diagnostic_Severity.Warning)
		testing.expect_value(t, diagnostic.message, OPEN_SQL_ORDER_BY_FIELD_NOT_SELECTED_MESSAGE)
	}
	testing.expect(t, found)
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
root_semantic_sql_where_rejects_internal_table_component_without_for_all_entries :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF zrow,
         trnid TYPE string,
         docnum TYPE string,
       END OF zrow.
DATA lt_rows TYPE STANDARD TABLE OF zrow WITH EMPTY KEY.

SELECT trnid
  FROM zrow
  INTO TABLE @DATA(lt_out)
  WHERE trnid = @lt_rows-trnid.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://sql_where_itab_without_fae.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Open_Sql_Where_Operand), 1)
	found := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Open_Sql_Where_Operand {
			continue
		}
		found = true
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "@lt_rows-trnid")
		testing.expect_value(t, diagnostic.message, OPEN_SQL_INTERNAL_TABLE_WHERE_HOST_MESSAGE)
	}
	testing.expect(t, found)
}

@(test)
root_semantic_sql_where_allows_matching_for_all_entries_table_component :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF zrow,
         trnid TYPE string,
         docnum TYPE string,
       END OF zrow.
DATA lt_rows TYPE STANDARD TABLE OF zrow WITH EMPTY KEY.
DATA lt_other TYPE STANDARD TABLE OF zrow WITH EMPTY KEY.

SELECT trnid
  FROM zrow
  INTO TABLE @DATA(lt_out)
  FOR ALL ENTRIES IN @lt_rows
  WHERE trnid = @lt_rows-trnid
    AND docnum = @lt_other-docnum.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://sql_where_itab_with_fae.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Open_Sql_Where_Operand), 1)
	found_other := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Open_Sql_Where_Operand {
			continue
		}
		found_other = true
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "@lt_other-docnum")
	}
	testing.expect(t, found_other)
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
root_semantic_delete_adjacent_duplicates_comparing_resolves_row_field :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_mseg_tmp,
         kdauf TYPE string,
         matnr TYPE string,
       END OF ty_mseg_tmp.
DATA gt_mseg_tmp TYPE STANDARD TABLE OF ty_mseg_tmp WITH EMPTY KEY.

DELETE ADJACENT DUPLICATES FROM gt_mseg_tmp COMPARING kdauf.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://delete_adjacent_kdauf.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "kdauf"), 0)
	gt_mseg_tmp := checker_test_lookup(t, &project, file.root_scope, .Value, "gt_mseg_tmp", .Variable)
	ty_mseg_tmp := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_mseg_tmp", .Type_Def)
	kdauf_field := checker_test_structure_field(t, &project, checker_type_structure(ty_mseg_tmp.type), "kdauf")
	testing.expect(t, gt_mseg_tmp != nil && .Used in gt_mseg_tmp.flags)
	testing.expect(t, kdauf_field != nil && .Used in kdauf_field.flags)

	kdauf_offset := checker_test_find_text(source, "COMPARING kdauf")
	if kdauf_offset >= 0 {
		kdauf_offset += len("COMPARING ")
	}
	testing.expect(t, kdauf_offset >= 0)

	use := semantic_ref_use_at_offset(semantic_query_refs(semantic_query(&project, &checker, file)), kdauf_offset)
	testing.expect(t, use != nil)
	if use != nil {
		testing.expect(t, use.entity == kdauf_field)
		range := semantic_entity_use_range(use^)
		testing.expect_value(t, source[range.start:range.end], "kdauf")
	}
}

@(test)
root_semantic_read_table_forms_validate_row_components :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_nested,
         part TYPE string,
       END OF ty_nested.
TYPES: BEGIN OF ty_row,
         id TYPE i,
         docnum TYPE string,
         trnid TYPE string,
         docpos TYPE string,
         nested TYPE ty_nested,
       END OF ty_row.
TYPES ty_ref_rows TYPE STANDARD TABLE OF REF TO ty_nested WITH EMPTY KEY.
DATA mt_event TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA mt_refs TYPE ty_ref_rows.
DATA lv_id TYPE i.
DATA lv_trnid TYPE string.
DATA lv_docnum TYPE string.
DATA lv_part TYPE string.
DATA lv_index TYPE i.
DATA lv_key TYPE string.
DATA lv_component TYPE string.
DATA lr_event TYPE REF TO ty_row.
FIELD-SYMBOLS <ls_event> LIKE LINE OF mt_event.

READ TABLE mt_event WITH KEY id = lv_id INTO DATA(ls_by_key).
READ TABLE mt_event INTO DATA(ls_by_index) INDEX lv_index USING KEY (lv_key).
READ TABLE mt_event WITH TABLE KEY primary_key COMPONENTS docnum = lv_docnum REFERENCE INTO lr_event.
READ TABLE mt_event WITH KEY nested-part = lv_part ASSIGNING <ls_event> BINARY SEARCH COMPARING docpos.
READ TABLE mt_event WITH KEY (lv_component) = lv_trnid TRANSPORTING NO FIELDS.
READ TABLE mt_refs WITH KEY table_line->part = lv_part TRANSPORTING NO FIELDS.
READ TABLE mt_event WITH KEY trnid = lv_trnid INTO DATA(ls_all) COMPARING ALL FIELDS.
DATA lv_text TYPE string.
lv_text = ls_by_key-docnum.
lv_text = ls_by_index-trnid.
lv_text = ls_all-docpos.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://read_table_forms.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	component_names := [?]string{"id", "docnum", "trnid", "docpos", "nested", "part"}
	for name in component_names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 0)
	}
	mt_event := checker_test_lookup(t, &project, file.root_scope, .Value, "mt_event", .Variable)
	lv_id := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_id", .Variable)
	lv_component := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_component", .Variable)
	testing.expect(t, mt_event != nil && .Used in mt_event.flags)
	testing.expect(t, lv_id != nil && .Used in lv_id.flags)
	testing.expect(t, lv_component != nil && .Used in lv_component.flags)
}

@(test)
root_semantic_read_table_reports_invalid_source :: proc(t: ^testing.T) {
	source := `DATA lv_text TYPE string.
READ TABLE lv_text INTO DATA(ls_row) INDEX 1.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://read_table_source.abap")

	testing.expect_value(
		t,
		checker_test_diagnostic_message_count(
			&checker,
			.Invalid_Syntax_Form,
			"READ TABLE source is not an internal table",
		),
		1,
	)
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Invalid_Syntax_Form {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "lv_text")
	}
}

@(test)
root_semantic_read_table_reports_key_component_errors :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_nested,
         part TYPE string,
       END OF ty_nested.
TYPES: BEGIN OF ty_row,
         id TYPE i,
         date TYPE d,
         nested TYPE ty_nested,
       END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lv_id TYPE i.
DATA lv_time TYPE t.

READ TABLE lt_rows WITH KEY missing = lv_id INTO DATA(ls_missing).
READ TABLE lt_rows WITH KEY date = lv_time TRANSPORTING NO FIELDS.
READ TABLE lt_rows WITH KEY nested-missing = lv_id TRANSPORTING NO FIELDS.
READ TABLE lt_rows WITH KEY id = lv_missing TRANSPORTING NO FIELDS.
READ TABLE lt_rows INTO DATA(ls_cmp) INDEX 1 COMPARING gone.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://read_table_key_errors.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 3)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Incompatible_Assignment_Type), 1)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)
	missing_names := [?]string{"missing", "gone"}
	for name in missing_names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 0)
	}
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "lv_missing"), 1)
}

@(test)
root_semantic_modify_forms_validate_row_components :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_nested,
         part TYPE string,
       END OF ty_nested.
TYPES: BEGIN OF ty_row,
         id TYPE string,
         status TYPE string,
         nested TYPE ty_nested,
       END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lt_more TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA ls_row TYPE ty_row.
DATA lv_id TYPE string.
DATA lv_index TYPE i.

MODIFY lt_rows FROM ls_row INDEX lv_index TRANSPORTING id nested-part.
MODIFY TABLE lt_rows FROM ls_row TRANSPORTING status.
MODIFY lt_rows FROM TABLE lt_more.
MODIFY lt_rows FROM ls_row TRANSPORTING status WHERE id = lv_id AND nested-part IS NOT INITIAL.
MODIFY SCREEN.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://modify_forms.abap")

	testing.expect_value(t, len(checker.info.diagnostics), 0)
	component_names := [?]string{"id", "status", "nested", "part"}
	for name in component_names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 0)
	}
	lt_rows := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_rows", .Variable)
	lt_more := checker_test_lookup(t, &project, file.root_scope, .Value, "lt_more", .Variable)
	ls_row := checker_test_lookup(t, &project, file.root_scope, .Value, "ls_row", .Variable)
	lv_id := checker_test_lookup(t, &project, file.root_scope, .Value, "lv_id", .Variable)
	testing.expect(t, lt_rows != nil && .Used in lt_rows.flags)
	testing.expect(t, lt_more != nil && .Used in lt_more.flags)
	testing.expect(t, ls_row != nil && .Used in ls_row.flags)
	testing.expect(t, lv_id != nil && .Used in lv_id.flags)

	query := semantic_query(&project, &checker, file)
	ref_query := semantic_query_refs(query)
	ty_row := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_row", .Type_Def)
	ty_nested := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_nested", .Type_Def)
	id_field := checker_test_structure_field(t, &project, checker_type_structure(ty_row.type), "id")
	part_field := checker_test_structure_field(t, &project, checker_type_structure(ty_nested.type), "part")

	id_offset := checker_test_find_text(source, "TRANSPORTING id")
	if id_offset >= 0 {
		id_offset += len("TRANSPORTING ")
	}
	part_offset := checker_test_find_text(source, "nested-part")
	if part_offset >= 0 {
		part_offset += len("nested-")
	}
	testing.expect(t, id_offset >= 0 && part_offset >= 0)

	id_use := semantic_ref_use_at_offset(ref_query, id_offset)
	testing.expect(t, id_use != nil)
	if id_use != nil {
		testing.expect(t, id_use.entity == id_field)
		range := semantic_entity_use_range(id_use^)
		testing.expect_value(t, source[range.start:range.end], "id")
	}
	part_use := semantic_ref_use_at_offset(ref_query, part_offset)
	testing.expect(t, part_use != nil)
	if part_use != nil {
		testing.expect(t, part_use.entity == part_field)
		range := semantic_entity_use_range(part_use^)
		testing.expect_value(t, source[range.start:range.end], "part")
	}
}

@(test)
root_semantic_modify_transporting_uses_known_source_when_target_row_is_unknown :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row,
         id TYPE string,
         status TYPE string,
       END OF ty_row.

SELECT *
  FROM zmissing_rows
  INTO TABLE @DATA(lt_rows).

MODIFY lt_rows
  FROM VALUE ty_row(
    id = '1'
    status = '2'
  )
  TRANSPORTING status111 status.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://modify_unknown_target.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 1)
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind == .Unknown_Field {
			testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "status111")
		}
	}

	query := semantic_query(&project, &checker, file)
	ref_query := semantic_query_refs(query)
	ty_row := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_row", .Type_Def)
	status_field := checker_test_structure_field(t, &project, checker_type_structure(ty_row.type), "status")

	status_offset := checker_test_find_text(source, "status111 status")
	if status_offset >= 0 {
		status_offset += len("status111 ")
	}
	testing.expect(t, status_offset >= 0)

	status_use := semantic_ref_use_at_offset(ref_query, status_offset)
	testing.expect(t, status_use != nil)
	if status_use != nil {
		testing.expect(t, status_use.entity == status_field)
		range := semantic_entity_use_range(status_use^)
		testing.expect_value(t, source[range.start:range.end], "status")
	}
}

@(test)
root_semantic_modify_reports_invalid_targets_and_unresolved_operands :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row,
         id TYPE string,
       END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA ls_row TYPE ty_row.
DATA lv_text TYPE string.
CONSTANTS gc_text TYPE string VALUE ''.

MODIFY lv_text FROM ls_row.
MODIFY gc_text FROM ls_row.
MODIFY TABLE lt_missing FROM ls_row.
MODIFY lt_rows FROM ls_missing.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://modify_invalid_targets.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Modify_Operand), 2)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 2)

	seen_not_table := false
	seen_not_writable := false
	seen_missing_target := false
	seen_missing_source := false
	for diagnostic in checker.info.diagnostics {
		text := source[diagnostic.range.start:diagnostic.range.end]
		if diagnostic.kind == .Invalid_Modify_Operand && text == "lv_text" {
			seen_not_table = true
			testing.expect_value(t, diagnostic.message, "MODIFY target is not an internal table")
		} else if diagnostic.kind == .Invalid_Modify_Operand && text == "gc_text" {
			seen_not_writable = true
			testing.expect_value(t, diagnostic.message, "MODIFY target is not writable")
		} else if diagnostic.kind == .Unresolved_Reference && text == "lt_missing" {
			seen_missing_target = true
			testing.expect_value(t, diagnostic.message, "unresolved variable lt_missing")
		} else if diagnostic.kind == .Unresolved_Reference && text == "ls_missing" {
			seen_missing_source = true
			testing.expect_value(t, diagnostic.message, "unresolved variable ls_missing")
		}
	}
	testing.expect(t, seen_not_table)
	testing.expect(t, seen_not_writable)
	testing.expect(t, seen_missing_target)
	testing.expect(t, seen_missing_source)
}

@(test)
root_semantic_modify_reports_unknown_row_components :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row,
         id TYPE string,
       END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA ls_row TYPE ty_row.
DATA lv_id TYPE string.

MODIFY lt_rows FROM ls_row TRANSPORTING lost WHERE missing = lv_id.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://modify_components.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 2)
	missing_names := [?]string{"lost", "missing"}
	for name in missing_names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 0)
	}
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "lv_id"), 0)
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
root_semantic_delete_reports_invalid_targets :: proc(t: ^testing.T) {
	source := `DATA lv_text TYPE string.
DATA lt_text TYPE STANDARD TABLE OF string WITH EMPTY KEY.
CONSTANTS gc_text TYPE string VALUE ''.

DELETE lv_text.
DELETE ADJACENT DUPLICATES FROM gc_text COMPARING table_line.
DELETE ADJACENT DUPLICATES FROM lt_missing COMPARING table_line.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://delete_invalid_targets.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Invalid_Delete_Operand), 2)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 1)

	seen_not_table := false
	seen_not_writable := false
	seen_missing := false
	for diagnostic in checker.info.diagnostics {
		text := source[diagnostic.range.start:diagnostic.range.end]
		if diagnostic.kind == .Invalid_Delete_Operand && text == "lv_text" {
			seen_not_table = true
			testing.expect_value(t, diagnostic.message, "DELETE target is not an internal table")
		} else if diagnostic.kind == .Invalid_Delete_Operand && text == "gc_text" {
			seen_not_writable = true
			testing.expect_value(t, diagnostic.message, "DELETE target is not writable")
		} else if diagnostic.kind == .Unresolved_Reference && text == "lt_missing" {
			seen_missing = true
			testing.expect_value(t, diagnostic.message, "unresolved variable lt_missing")
		}
	}
	testing.expect(t, seen_not_table)
	testing.expect(t, seen_not_writable)
	testing.expect(t, seen_missing)
}

@(test)
root_semantic_missing_elementary_table_components_diagnose_without_symbol_candidates :: proc(
	t: ^testing.T,
) {
	source := `DATA lt_text TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA lv_text TYPE string.

SORT lt_text BY table_line1.
READ TABLE lt_text WITH KEY table_line2 = lv_text TRANSPORTING NO FIELDS.
LOOP AT lt_text TRANSPORTING table_line3 WHERE table_line4 = lv_text.
ENDLOOP.
DELETE lt_text WHERE table_line5 = lv_text.
DELETE ADJACENT DUPLICATES FROM lt_text COMPARING table_line6.`

	project := project_make()
	defer project_destroy(&project)

	checker, _ := checker_test_check_source(t, &project, source, "mem://missing_elementary_table_components.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 6)
	missing_names := [?]string{"table_line1", "table_line2", "table_line3", "table_line4", "table_line5", "table_line6"}
	for name in missing_names {
		testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, name), 0)
	}
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "lv_text"), 0)
}

@(test)
root_semantic_sort_by_fields_validate_row_components :: proc(t: ^testing.T) {
	source := `TYPES: BEGIN OF ty_row,
         id TYPE string,
       END OF ty_row.
DATA itab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

SORT itab BY field DESCENDING.
SORT itab BY field.
SORT itab STABLE BY field.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://sort_by_fields.abap")

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 3)
	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unresolved_Reference), 0)
	testing.expect_value(t, checker_test_unresolved_candidate_count(&checker, &project, .Global_Symbol, "field"), 0)
	itab := checker_test_lookup(t, &project, file.root_scope, .Value, "itab", .Variable)
	testing.expect(t, itab != nil && .Used in itab.flags)
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Unknown_Field {
			continue
		}
		testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "field")
		testing.expect_value(t, diagnostic.message, "unknown internal table field field")
	}
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
	testing.expect_value(t, decl.name, "lv_value")
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
root_semantic_table_type_keys_check_data_decl_components :: proc(t: ^testing.T) {
	source := `TYPES:
  BEGIN OF ty_so_link,
    ebeln TYPE i,
    vbeln TYPE i,
    posnr TYPE i,
  END OF ty_so_link,
  BEGIN OF ty_vbpa,
    vbeln TYPE i,
    posnr TYPE i,
    parvw TYPE i,
    kunnr TYPE i,
  END OF ty_vbpa.
DATA:
  lt_so_link TYPE SORTED TABLE OF ty_so_link WITH UNIQUE KEY ebeln vbeln posnr bad_component,
  lt_vbpa TYPE SORTED TABLE OF ty_vbpa WITH NON-UNIQUE KEY vbeln posnr parvw kunnr bad_partner.`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://data_decl_table_keys.abap")
	query := semantic_query(&project, &checker, file)
	ref_query := semantic_query_refs(query)

	so_link := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_so_link", .Type_Def)
	vbpa := checker_test_lookup(t, &project, file.root_scope, .Type, "ty_vbpa", .Type_Def)
	testing.expect(t, so_link != nil && vbpa != nil)
	if so_link == nil || vbpa == nil {
		return
	}
	so_link_structure := checker_type_structure(so_link.type)
	vbpa_structure := checker_type_structure(vbpa.type)
	ebeln_field := checker_test_structure_field(t, &project, so_link_structure, "ebeln")
	parvw_field := checker_test_structure_field(t, &project, vbpa_structure, "parvw")

	ebeln_offset := checker_test_find_text(source, "WITH UNIQUE KEY ebeln")
	if ebeln_offset >= 0 {
		ebeln_offset += len("WITH UNIQUE KEY ")
	}
	parvw_offset := checker_test_find_text(source, "WITH NON-UNIQUE KEY vbeln posnr parvw")
	if parvw_offset >= 0 {
		parvw_offset += len("WITH NON-UNIQUE KEY vbeln posnr ")
	}
	testing.expect(t, ebeln_offset >= 0 && parvw_offset >= 0)

	ebeln_use := semantic_ref_use_at_offset(ref_query, ebeln_offset)
	testing.expect(t, ebeln_use != nil)
	if ebeln_use != nil {
		testing.expect(t, ebeln_use.entity == ebeln_field)
		range := semantic_entity_use_range(ebeln_use^)
		testing.expect_value(t, source[range.start:range.end], "ebeln")
	}
	parvw_use := semantic_ref_use_at_offset(ref_query, parvw_offset)
	testing.expect(t, parvw_use != nil)
	if parvw_use != nil {
		testing.expect(t, parvw_use.entity == parvw_field)
		range := semantic_entity_use_range(parvw_use^)
		testing.expect_value(t, source[range.start:range.end], "parvw")
	}

	testing.expect_value(t, checker_test_diagnostic_count(&checker, .Unknown_Field), 2)
	bad_component_offset := checker_test_find_text(source, "bad_component")
	bad_partner_offset := checker_test_find_text(source, "bad_partner")
	bad_component_found := false
	bad_partner_found := false
	for diagnostic in checker.info.diagnostics {
		if diagnostic.kind != .Unknown_Field {
			continue
		}
		if diagnostic.range.start == bad_component_offset {
			bad_component_found = true
			testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "bad_component")
		}
		if diagnostic.range.start == bad_partner_offset {
			bad_partner_found = true
			testing.expect_value(t, source[diagnostic.range.start:diagnostic.range.end], "bad_partner")
		}
	}
	testing.expect(t, bad_component_found)
	testing.expect(t, bad_partner_found)
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
		testing.expect_value(t, member.name, "run")
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
	testing.expect_value(t, field.name, "comp")
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
		name := item.name
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

@(test)
root_semantic_query_completion_in_method_implementation_reads_signature_scope :: proc(t: ^testing.T) {
	source := `CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS do_something
      IMPORTING
        iv_param TYPE string
      RETURNING
        VALUE(rv_res) TYPE string.
ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD do_something.
    rv_
ENDMETHOD.
ENDCLASS.`

	// Keep the source incomplete to match editor completion while typing.
	parsed := parser.parse(source, "mem://query_method_impl_completion.abap", context.allocator)

	project := project_make()
	defer project_destroy(&project)

	checker := checker_make(&project)
	file := checker_add_file(&checker, parsed.path, parsed.root)
	checker_check_file(&checker, file)

	query := semantic_query(&project, &checker, file)
	offset := checker_test_find_text_last(source, "rv_") + len("rv_")
	testing.expect(t, offset >= len("rv_"))

	items := semantic_completion_items_at_offset(
		semantic_query_completion(query),
		offset,
		"rv_",
		context.allocator,
		source,
	)

	returning_found := false
	unrelated_found := false
	for item in items {
		name := item.name
		if name == "rv_res" &&
		   item.namespace == .Value &&
		   item.source == .Lexical_Scope &&
		   item.entity != nil &&
		   item.entity.kind == .Parameter {
			returning_found = true
		}
		if name == "iv_param" || name == "lcl_class" || name == "strlen" {
			unrelated_found = true
		}
	}
	testing.expect(t, returning_found)
	testing.expect(t, !unrelated_found)
}

@(test)
root_semantic_query_completion_after_me_selector_in_method_implementation_returns_members :: proc(t: ^testing.T) {
	source := `CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    METHODS do_something
      IMPORTING
        iv_param TYPE string
      RETURNING
        VALUE(rv_res) TYPE string.

    METHODS method_name
      IMPORTING
        iv_input TYPE string
      RETURNING
        VALUE(rv_result) TYPE string.

ENDCLASS.

CLASS lcl_class IMPLEMENTATION.
  METHOD do_something.
    me->
  ENDMETHOD.
  METHOD method_name.
  ENDMETHOD.
ENDCLASS.`

	// Keep the selector incomplete to match editor completion after typing `me->`.
	parsed := parser.parse(source, "mem://query_method_me_completion.abap", context.allocator)

	project := project_make()
	defer project_destroy(&project)

	checker := checker_make(&project)
	file := checker_add_file(&checker, parsed.path, parsed.root)
	checker_check_file(&checker, file)

	query := semantic_query(&project, &checker, file)
	offset := checker_test_find_text_last(source, "me->") + len("me->")
	testing.expect(t, offset >= len("me->"))

	items := semantic_completion_items_at_offset(
		semantic_query_completion(query),
		offset,
		"",
		context.allocator,
		source,
	)

	do_something_found := false
	method_name_found := false
	unrelated_found := false
	for item in items {
		name := item.name
		if name == "do_something" && item.namespace == .Routine && item.source == .Selector_Member {
			do_something_found = true
		}
		if name == "method_name" && item.namespace == .Routine && item.source == .Selector_Member {
			method_name_found = true
		}
		if name == "iv_param" ||
		   name == "rv_res" ||
		   name == "lcl_class" ||
		   name == "strlen" {
			unrelated_found = true
		}
	}
	testing.expect(t, do_something_found)
	testing.expect(t, method_name_found)
	testing.expect(t, !unrelated_found)
}

@(test)
root_semantic_query_completion_after_static_selector_returns_accessible_static_members :: proc(t: ^testing.T) {
	source := `REPORT zmain.
CLASS lcl_repo DEFINITION.
  PUBLIC SECTION.
    TYPES ty_public TYPE string.
    CONSTANTS c_public TYPE string VALUE 'x'.
    CLASS-DATA gv_public TYPE string.
    DATA mv_instance TYPE string.
    CLASS-METHODS class_constructor.
    CLASS-METHODS get_instance.
    METHODS scan.
  PRIVATE SECTION.
    CLASS-DATA gv_private TYPE string.
ENDCLASS.
CLASS lcl_repo IMPLEMENTATION.
  METHOD class_constructor.
  ENDMETHOD.
  METHOD get_instance.
  ENDMETHOD.
  METHOD scan.
  ENDMETHOD.
ENDCLASS.
DATA lv_local TYPE i.
lcl_repo=>get_instance( ).`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://query_static_completion.abap")
	query := semantic_query(&project, &checker, file)
	offset := checker_test_find_text(source, "lcl_repo=>") + len("lcl_repo=>")
	testing.expect(t, offset >= len("lcl_repo=>"))

	items := semantic_completion_items_at_offset(
		semantic_query_completion(query),
		offset,
		"",
		context.allocator,
		source,
	)

	get_instance_found := false
	gv_public_found := false
	ty_public_found := false
	c_public_found := false
	unrelated_found := false
	for item in items {
		name := item.name
		if name == "get_instance" && item.namespace == .Routine && item.source == .Selector_Member {
			get_instance_found = true
		}
		if name == "gv_public" && item.namespace == .Value && item.source == .Selector_Member {
			gv_public_found = true
		}
		if name == "ty_public" && item.namespace == .Type && item.source == .Selector_Member {
			ty_public_found = true
		}
		if name == "c_public" && item.namespace == .Value && item.source == .Selector_Member {
			c_public_found = true
		}
		if name == "scan" ||
		   name == "class_constructor" ||
		   name == "mv_instance" ||
		   name == "gv_private" ||
		   name == "lv_local" ||
		   name == "strlen" ||
		   name == "lcl_repo" {
			unrelated_found = true
		}
	}
	testing.expect(t, get_instance_found)
	testing.expect(t, gv_public_found)
	testing.expect(t, ty_public_found)
	testing.expect(t, c_public_found)
	testing.expect(t, !unrelated_found)
}

@(test)
root_semantic_query_completion_after_instance_selector_returns_accessible_instance_members :: proc(t: ^testing.T) {
	source := `REPORT zmain.
CLASS lcl_repo DEFINITION.
  PUBLIC SECTION.
    DATA mv_public TYPE string.
    CLASS-DATA gv_static TYPE string.
    CONSTANTS c_static TYPE string VALUE 'x'.
    CLASS-METHODS get_instance RETURNING VALUE(ro_repo) TYPE REF TO lcl_repo.
    METHODS scan.
  PRIVATE SECTION.
    DATA mv_private TYPE string.
    METHODS private_scan.
ENDCLASS.
CLASS lcl_repo IMPLEMENTATION.
  METHOD get_instance.
  ENDMETHOD.
  METHOD scan.
  ENDMETHOD.
  METHOD private_scan.
  ENDMETHOD.
ENDCLASS.
DATA lv_local TYPE i.
lcl_repo=>get_instance( )->scan( ).`

	project := project_make()
	defer project_destroy(&project)

	checker, file := checker_test_check_source(t, &project, source, "mem://query_instance_completion.abap")
	query := semantic_query(&project, &checker, file)
	offset := checker_test_find_text(source, ")->scan") + len(")->")
	testing.expect(t, offset >= len(")->"))

	items := semantic_completion_items_at_offset(
		semantic_query_completion(query),
		offset,
		"",
		context.allocator,
		source,
	)

	scan_found := false
	mv_public_found := false
	unrelated_found := false
	for item in items {
		name := item.name
		if name == "scan" && item.namespace == .Routine && item.source == .Selector_Member {
			scan_found = true
		}
		if name == "mv_public" && item.namespace == .Value && item.source == .Selector_Member {
			mv_public_found = true
		}
		if name == "get_instance" ||
		   name == "gv_static" ||
		   name == "c_static" ||
		   name == "mv_private" ||
		   name == "private_scan" ||
		   name == "lv_local" ||
		   name == "strlen" ||
		   name == "lcl_repo" {
			unrelated_found = true
		}
	}
	testing.expect(t, scan_found)
	testing.expect(t, mv_public_found)
	testing.expect(t, !unrelated_found)
}

@(test)
root_semantic_query_completion_after_instance_selector_uses_aliases_and_interface_names :: proc(
	t: ^testing.T,
) {
	source := `INTERFACE lif_interface.
  METHODS method_name
    IMPORTING
      iv_value TYPE string.
ENDINTERFACE.
CLASS lcl_class DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_interface.
    ALIASES short_name FOR lif_interface~method_name.
    METHODS local_method.
ENDCLASS.
CLASS lcl_class IMPLEMENTATION.
  METHOD local_method.
  ENDMETHOD.
ENDCLASS.
DATA(lo_inst) = NEW lcl_class( ).
lo_inst->
lo_inst->lif_interface~`

	project := project_make()
	defer project_destroy(&project)

	parsed := parser.parse(source, "mem://query_interface_alias_completion.abap", context.allocator)
	checker := checker_make(&project)
	file := checker_add_file(&checker, parsed.path, parsed.root)
	checker_check_file(&checker, file)

	query := semantic_query(&project, &checker, file)
	arrow_offset := checker_test_find_text(source, "lo_inst->\n") + len("lo_inst->")
	testing.expect(t, arrow_offset >= len("lo_inst->"))

	arrow_items := semantic_completion_items_at_offset(
		semantic_query_completion(query),
		arrow_offset,
		"",
		context.allocator,
		source,
	)

	local_method_found := false
	short_name_found := false
	interface_found := false
	method_name_found := false
	for item in arrow_items {
		if item.name == "local_method" && item.namespace == .Routine && item.source == .Selector_Member {
			local_method_found = true
		}
		if item.name == "short_name" &&
		   item.namespace == .Routine &&
		   item.source == .Selector_Member &&
		   item.entity != nil &&
		   item.entity.kind == .Method {
			short_name_found = true
		}
		if item.name == "lif_interface" &&
		   item.namespace == .Type &&
		   item.source == .Selector_Member &&
		   item.entity != nil &&
		   item.entity.kind == .Interface {
			interface_found = true
		}
		if item.name == "method_name" && item.namespace == .Routine && item.source == .Selector_Member {
			method_name_found = true
		}
	}
	testing.expect(t, local_method_found)
	testing.expect(t, short_name_found)
	testing.expect(t, interface_found)
	testing.expect(t, !method_name_found)

	tilde_offset := checker_test_find_text(source, "lo_inst->lif_interface~") + len("lo_inst->lif_interface~")
	testing.expect(t, tilde_offset >= len("lo_inst->lif_interface~"))
	tilde_items := semantic_completion_items_at_offset(
		semantic_query_completion(query),
		tilde_offset,
		"",
		context.allocator,
		source,
	)

	qualified_method_found := false
	for item in tilde_items {
		if item.name == "method_name" && item.namespace == .Routine && item.source == .Selector_Member {
			qualified_method_found = true
			break
		}
	}
	testing.expect(t, qualified_method_found)
}

@(test)
root_semantic_query_completion_after_pending_instance_arrow_returns_accessible_instance_members :: proc(
	t: ^testing.T,
) {
	source := `REPORT zmain.
CLASS lcl_repo DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING iv_value TYPE string.
    METHODS scan.
ENDCLASS.
CLASS lcl_repo IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
  METHOD scan.
  ENDMETHOD.
ENDCLASS.
DATA(lo_repo) = NEW lcl_repo( 'value' ).
lo_repo-`

	project := project_make()
	defer project_destroy(&project)

	parsed := parser.parse(source, "mem://query_pending_instance_arrow_completion.abap", context.allocator)
	checker := checker_make(&project)
	file := checker_add_file(&checker, parsed.path, parsed.root)
	checker_check_file(&checker, file)

	query := semantic_query(&project, &checker, file)
	offset := checker_test_find_text(source, "lo_repo-") + len("lo_repo-")
	testing.expect(t, offset >= len("lo_repo-"))

	items := semantic_completion_items_at_offset(
		semantic_query_completion(query),
		offset,
		"",
		context.allocator,
		source,
	)

	scan_found := false
	constructor_found := false
	unrelated_found := false
	for item in items {
		name := item.name
		if name == "scan" &&
		   item.namespace == .Routine &&
		   item.source == .Selector_Member &&
		   item.selector_op == .Arrow {
			scan_found = true
		}
		if name == "constructor" &&
		   item.namespace == .Routine &&
		   item.source == .Selector_Member &&
		   item.selector_op == .Arrow {
			constructor_found = true
		}
		if name == "lcl_repo" || name == "strlen" || name == "lo_repo" {
			unrelated_found = true
		}
	}
	testing.expect(t, scan_found)
	testing.expect(t, !constructor_found)
	testing.expect(t, !unrelated_found)
}

@(test)
root_semantic_query_completion_after_pending_structure_dash_inside_value_constructor_uses_receiver_type :: proc(
	t: ^testing.T,
) {
	source := `TYPES:
  BEGIN OF ty_line,
    field1 TYPE string,
    field2 TYPE string,
  END OF ty_line,
  BEGIN OF ty_line2,
    field TYPE string,
  END OF ty_line2,
  tt_table TYPE TABLE OF ty_line WITH EMPTY KEY.

DATA lt_table TYPE tt_table.
DATA ls_line TYPE ty_line2.

APPEND VALUE #(
  field1 = VALUE #( )
  field2 = ls_line-
) TO lt_table.`

	project := project_make()
	defer project_destroy(&project)

	parsed := parser.parse(source, "mem://query_pending_structure_dash_in_constructor.abap", context.allocator)
	checker := checker_make(&project)
	file := checker_add_file(&checker, parsed.path, parsed.root)
	checker_check_file(&checker, file)

	query := semantic_query(&project, &checker, file)
	offset := checker_test_find_text(source, "ls_line-") + len("ls_line-")
	testing.expect(t, offset >= len("ls_line-"))

	items := semantic_completion_items_at_offset(
		semantic_query_completion(query),
		offset,
		"",
		context.allocator,
		source,
	)

	receiver_field_found := false
	populated_field_found := false
	for item in items {
		if item.name == "field" &&
		   item.namespace == .Value &&
		   item.source == .Selector_Member &&
		   item.selector_op == .Dash {
			receiver_field_found = true
		}
		if item.name == "field1" || item.name == "field2" {
			populated_field_found = true
		}
	}
	testing.expect(t, receiver_field_found)
	testing.expect(t, !populated_field_found)
}
