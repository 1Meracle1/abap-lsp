package abap_frontend_semantic

import string_interner "src:string_interner"

import "core:container/xar"
import "core:testing"

@(test)
root_semantic_project_storage_keeps_entity_scope_and_file_pointers_stable :: proc(t: ^testing.T) {
	project := project_make()
	defer project_destroy(&project)

	file := project_add_file(&project, "ZPROG.abap")
	entity := project_new_entity(&project)
	scope := project_new_scope(&project)

	entity.kind = .Variable
	entity.source_file = file
	entity.scope = scope
	scope.kind = .File
	scope.owner = entity
	file.root_scope = scope

	name := string_interner.insert(project.interner, "gv_value")
	entity.name = name
	scope.declarations = make([dynamic]^Entity, 0, 1, project.allocator)
	scope.elements = make(map[Scope_Declaration_Key]^Entity, 0, project.allocator)
	append(&scope.declarations, entity)
	scope.elements[Scope_Declaration_Key{namespace = .Value, name = name}] = entity

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
	testing.expect(t, scope.owner == entity)
	testing.expect(t, file.root_scope == scope)
	testing.expect(t, scope.declarations[0] == entity)

	found, ok := scope.elements[Scope_Declaration_Key{namespace = .Value, name = name}]
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
