package abap_frontend_semantic

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
	source :=
		"DATA gv_ptr TYPE %_C_POINTER.\n" +
		"DATA gv_len TYPE i.\n" +
		"FORM run.\n" +
		"  gv_len = strlen( 'abc' ).\n" +
		"ENDFORM.\n"

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
	source :=
		"DATA shared TYPE i.\n" +
		"TYPES shared TYPE i.\n" +
		"FORM shared.\n" +
		"ENDFORM.\n" +
		"FORM run.\n" +
		"  DATA shared TYPE i.\n" +
		"  shared = 1.\n" +
		"ENDFORM.\n"

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
			testing.expect(t, diagnostic.entity == local_value)
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
	source :=
		"TYPES: BEGIN OF ty_line,\n" +
		"         value TYPE i,\n" +
		"       END OF ty_line.\n" +
		"DATA gv TYPE i.\n" +
		"FORM add USING iv TYPE i.\n" +
		"  DATA lv TYPE i.\n" +
		"  lv = iv + gv.\n" +
		"ENDFORM.\n"

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
	source :=
		"INTERFACE lif_object.\n" +
		"  METHODS copy IMPORTING iv_value TYPE i.\n" +
		"  METHODS rename.\n" +
		"ENDINTERFACE.\n" +
		"CLASS lcl DEFINITION.\n" +
		"  PUBLIC SECTION.\n" +
		"    INTERFACES lif_object.\n" +
		"    ALIASES alias_copy FOR lif_object~copy.\n" +
		"    CLASS-METHODS copy.\n" +
		"    METHODS lif_object~copy REDEFINITION.\n" +
		"ENDCLASS.\n" +
		"CLASS lcl IMPLEMENTATION.\n" +
		"  METHOD copy.\n" +
		"  ENDMETHOD.\n" +
		"  METHOD lif_object~copy.\n" +
		"    DATA lv_value TYPE i.\n" +
		"    lv_value = iv_value.\n" +
		"  ENDMETHOD.\n" +
		"ENDCLASS.\n"

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
root_semantic_decl_split_collects_broadened_file_declarations :: proc(t: ^testing.T) {
	source :=
		"REPORT zdecl.\n" +
		"INCLUDE zinc IF FOUND.\n" +
		"DATA gv_value TYPE i.\n" +
		"DATA(lv_inline) = 1.\n" +
		"CONSTANTS gc_limit TYPE i VALUE 1.\n" +
		"FIELD-SYMBOLS <fs_row> TYPE any.\n" +
		"STATICS sv_count TYPE i.\n" +
		"TABLES mara.\n" +
		"RANGES r_matnr FOR mara-matnr.\n" +
		"PARAMETERS p_count TYPE i DEFAULT 1.\n" +
		"SELECT-OPTIONS s_matnr FOR mara-matnr DEFAULT 'A' TO 'Z'.\n" +
		"CONTROLS tc_main TYPE tableview USING SCREEN 100.\n" +
		"TYPES: BEGIN OF ty_line,\n" +
		"         id TYPE i,\n" +
		"       END OF ty_line.\n" +
		"DATA: BEGIN OF gs_row,\n" +
		"        id TYPE i,\n" +
		"      END OF gs_row.\n" +
		"CONSTANTS: BEGIN OF gc_pair,\n" +
		"             a TYPE i VALUE 1,\n" +
		"             b TYPE i VALUE 2,\n" +
		"           END OF gc_pair.\n"

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
	testing.expect(t, param != nil && param.decl_info.default_clause != nil)
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
root_semantic_decl_split_collects_class_interface_and_oop_members :: proc(t: ^testing.T) {
	source :=
		"INTERFACE lif_demo.\n" +
		"  METHODS get_value RETURNING VALUE(rv_value) TYPE string.\n" +
		"  EVENTS changed EXPORTING VALUE(ev_value) TYPE string.\n" +
		"ENDINTERFACE.\n" +
		"CLASS lcl_demo DEFINITION.\n" +
		"  PUBLIC SECTION.\n" +
		"    INTERFACES lif_demo.\n" +
		"    ALIASES get_value FOR lif_demo~get_value.\n" +
		"    DATA mv_value TYPE string READ-ONLY.\n" +
		"    CLASS-DATA gv_count TYPE i.\n" +
		"    METHODS run IMPORTING iv_value TYPE string.\n" +
		"    CLASS-METHODS create RETURNING VALUE(ro_demo) TYPE REF TO lcl_demo.\n" +
		"    EVENTS done EXPORTING VALUE(ev_value) TYPE string.\n" +
		"ENDCLASS.\n" +
		"CLASS lcl_demo IMPLEMENTATION.\n" +
		"  METHOD run.\n" +
		"    DATA lv_local TYPE string.\n" +
		"    lv_local = iv_value.\n" +
		"  ENDMETHOD.\n" +
		"ENDCLASS.\n"

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
