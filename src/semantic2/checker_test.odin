package abap_frontend_semantic

import "src:parser"
import string_interner "src:string_interner"
import "src:tokenizer"

import "core:testing"

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
root_semantic_checker_registers_checks_and_records_entity_uses :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	checker := checker_make(&project)
	file := checker_add_file(&checker, "ZPROG.abap")
	ctx := checker_context_make(&checker, file)

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
	testing.expect_value(t, len(checker.info.definitions), 1)
	testing.expect_value(t, len(checker.info.entity_queue), 1)

	found_scope, found, found_ok := checker_lookup_declaration(&ctx, .Value, name)
	testing.expect(t, found_ok)
	testing.expect(t, found_scope == file.root_scope)
	testing.expect(t, found == entity)

	checker_check_queued_entities(&ctx)
	testing.expect_value(t, len(checker.info.entity_queue), 0)
	testing.expect_value(t, len(checker.info.checked_entities), 1)
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
