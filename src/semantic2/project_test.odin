package abap_frontend_semantic2

import "src:ast"
import string_interner "src:string_interner"
import "src:tokenizer"

import "core:container/xar"
import "core:testing"

@(test)
root_semantic_project_storage_keeps_entity_scope_and_file_pointers_stable :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	file := project_add_file(&project, "ZPROG.abap")
	entity := project_new_entity(&project, .Variable)
	scope := project_new_scope(&project)

	entity.source_file = file
	entity.scope = scope
	scope.kind = .File
	scope.owner = entity
	file.root_scope = scope

	name := string_interner.insert(project.interner, "gv_value")
	info := project_new_decl_info(
		&project,
		entity,
		scope,
		name,
		.Variable,
		tokenizer.text_range(0, 8),
	)
	previous := scope_insert_declaration(scope, entity)
	testing.expect(t, previous == nil)

	for _ in 0 ..< 300 {
		_ = project_add_file(&project)
		_ = project_new_entity(&project)
		_ = project_new_scope(&project)
	}

	testing.expect_value(t, xar.len(project.files), 301)
	testing.expect_value(t, xar.len(project.entities), 301)
	testing.expect_value(t, xar.len(project.scopes), 301)

	testing.expect_value(t, file.path, "ZPROG.abap")
	testing.expect_value(t, entity.kind, Entity_Kind.Variable)
	testing.expect_value(t, scope.kind, Scope_Kind.File)
	testing.expect(t, entity.source_file == file)
	testing.expect(t, entity.scope == scope)
	testing.expect(t, entity.decl_info == info)
	testing.expect(t, info.entity == entity)
	testing.expect(t, info.scope == scope)
	testing.expect_value(t, entity.name_range, tokenizer.text_range(0, 8))
	testing.expect(t, scope.owner == entity)
	testing.expect(t, file.root_scope == scope)
	testing.expect(t, scope.declarations[0] == entity)

	found, ok := scope_lookup_declaration(scope, .Value, name)
	testing.expect(t, ok)
	testing.expect(t, found == entity)
}

@(test)
root_semantic_project_owns_an_interner :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	name := string_interner.insert(project.interner, "gv_value")
	name_again := string_interner.insert(project.interner, "gv_value")

	testing.expect_value(t, name, name_again)
	testing.expect_value(t, string_interner.load(project.interner, name), "gv_value")
}

@(test)
root_semantic_project_storage_keeps_type_structure_and_decl_pointers_stable :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	file := project_add_file(&project, "ZTYPE.abap")
	scope := project_new_scope(&project)
	name := string_interner.insert(project.interner, "ty_line")
	entity := project_new_entity(&project, .Type_Def)
	info := project_new_decl_info(&project, entity, scope, name, .Type_Def, tokenizer.text_range(5, 12))
	structure := project_new_structure(&project, name, file, scope, tokenizer.text_range(5, 40))
	structure_type := project_type_structure(&project, structure)

	entity.source_file = file
	entity.type = project_type_named(&project, name, entity, structure_type)

	for _ in 0 ..< 300 {
		_ = project_new_type(&project)
		_ = project_new_structure(&project)
		_ = project_new_decl_info(&project, nil, nil, string_interner.String(0), .Invalid, {})
	}

	unknown := project_type_unknown(&project)
	testing.expect(t, unknown != nil)
	testing.expect(t, unknown == project_type_unknown())
	testing.expect_value(t, unknown.kind, Type_Kind.Unknown)
	testing.expect(t, entity.decl_info == info)
	testing.expect(t, entity.type.entity == entity)
	testing.expect(t, entity.type.base == structure_type)
	testing.expect(t, structure_type.structure == structure)
	testing.expect(t, structure.origin_structure == structure)
	testing.expect(t, structure.source_file == file)
	testing.expect(t, structure.scope == scope)
}

@(test)
root_semantic_entity_kind_change_replaces_payload :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	entity := project_new_entity(&project, .Variable)
	variable_payload, variable_ok := entity.payload.(^Entity_Variable_Payload)
	testing.expect(t, variable_ok)
	testing.expect(t, variable_payload != nil)
	testing.expect_value(t, variable_payload.param_value.kind, Entity_Parameter_Value_Kind.Invalid)
	testing.expect_value(t, variable_payload.param_value.value, ast.INVALID_EXACT_VALUE_ID)

	entity_set_kind(entity, .Method, project.allocator)
	routine_payload, routine_ok := entity.payload.(^Entity_Routine_Payload)
	testing.expect(t, routine_ok)
	testing.expect(t, routine_payload != nil)
	testing.expect_value(t, entity.kind, Entity_Kind.Method)
	testing.expect_value(t, len(routine_payload.parameters), 0)

	entity_set_kind(entity, .Builtin, project.allocator)
	builtin_payload, builtin_ok := entity.payload.(^Entity_Builtin_Payload)
	testing.expect(t, builtin_ok)
	testing.expect(t, builtin_payload != nil)
	testing.expect(t, .Builtin in entity.flags)
}

@(test)
root_semantic_constant_payload_carries_exact_value_parameter_value_and_comments :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	entity := project_new_entity(&project, .Constant)
	payload, ok := entity.payload.(^Entity_Constant_Payload)
	testing.expect(t, ok)
	testing.expect(t, payload != nil)
	testing.expect_value(t, payload.value, ast.INVALID_EXACT_VALUE_ID)
	testing.expect_value(t, payload.param_value.kind, Entity_Parameter_Value_Kind.Invalid)
	testing.expect_value(t, payload.param_value.value, ast.INVALID_EXACT_VALUE_ID)
	testing.expect_value(t, len(payload.docs), 0)
	testing.expect_value(t, len(payload.comment), 0)
}
