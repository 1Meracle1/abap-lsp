package abap_frontend_semantic

import "src:ast"
import string_interner "src:string_interner"

import "core:container/xar"
import "core:mem"
import virtual "core:mem/virtual"
import "core:strings"

Project_File :: struct {
	path:       string,
	root:       ^ast.File,
	root_scope: ^Scope,
}

Project :: struct {
	arena:     virtual.Arena,
	allocator: mem.Allocator,
	interner:  ^string_interner.Interner,
	files:     xar.Array(Project_File, 4),
	entities:  xar.Array(Entity, 8),
	scopes:    xar.Array(Scope, 6),
}

project_make :: proc() -> Project {
	project: Project
	arena_err := virtual.arena_init_growing(&project.arena)
	assert(arena_err == .None)

	project.allocator = virtual.arena_allocator(&project.arena)
	project.interner = string_interner.create()
	xar.init(&project.files, project.allocator)
	xar.init(&project.entities, project.allocator)
	xar.init(&project.scopes, project.allocator)
	return project
}

project_destroy :: proc(project: ^Project) {
	string_interner.destroy(project.interner)
	virtual.arena_destroy(&project.arena)
	project^ = {}
}

project_add_file :: proc(
	project: ^Project,
	path: string = "",
	root: ^ast.File = nil,
) -> ^Project_File {
	file, err := xar.push_back_elem_and_get_ptr(
		&project.files,
		Project_File {
			path = strings.clone(path, project.allocator) if path != "" else "",
			root = root,
		},
	)
	assert(err == .None && file != nil)
	return file
}

project_new_entity :: proc(project: ^Project) -> ^Entity {
	entity, err := xar.push_back_elem_and_get_ptr(&project.entities, Entity{})
	assert(err == .None && entity != nil)
	return entity
}

project_new_scope :: proc(project: ^Project) -> ^Scope {
	scope, err := xar.push_back_elem_and_get_ptr(&project.scopes, Scope{})
	assert(err == .None && scope != nil)
	return scope
}
