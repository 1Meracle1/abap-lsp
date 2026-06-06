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
	// Project snapshots own semantic objects through this arena; consumers use
	// pointer identity only within the owning snapshot.
	arena:          ^virtual.Arena,
	host_allocator: mem.Allocator,
	allocator:      mem.Allocator,
	interner:       ^string_interner.Interner,
	owns_interner:  bool,
	files:          xar.Array(Project_File, 4),
	entities:       xar.Array(Entity, 8),
	scopes:         xar.Array(Scope, 6),
}

project_make :: proc() -> (project: Project) {
	return project_make_with_interner(nil)
}

project_make_with_interner :: proc(interner: ^string_interner.Interner) -> (project: Project) {
	project.host_allocator = context.allocator
	project.arena = new(virtual.Arena, project.host_allocator)
	assert(project.arena != nil)

	arena_err := virtual.arena_init_growing(project.arena)
	assert(arena_err == .None)

	project.allocator = virtual.arena_allocator(project.arena)
	if interner != nil {
		project.interner = interner
	} else {
		project.interner = string_interner.create()
		project.owns_interner = true
	}
	xar.init(&project.files, project.allocator)
	xar.init(&project.entities, project.allocator)
	xar.init(&project.scopes, project.allocator)
	return
}

project_destroy :: proc(project: ^Project) {
	if project.owns_interner {
		string_interner.destroy(project.interner)
	}
	if project.arena != nil {
		virtual.arena_destroy(project.arena)
		free(project.arena, project.host_allocator)
	}
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

project_new_entity :: proc(project: ^Project, kind: Entity_Kind = .Invalid) -> ^Entity {
	value := Entity{kind = kind}
	if kind != .Invalid {
		value.payload = entity_default_payload(kind, project.allocator)
		if entity_kind_is_builtin(kind) {
			value.flags += {.Builtin}
		}
	}
	entity, err := xar.push_back_elem_and_get_ptr(&project.entities, value)
	assert(err == .None && entity != nil)
	return entity
}

project_new_scope :: proc(project: ^Project) -> ^Scope {
	value := Scope {
		elements     = make(map[Scope_Declaration_Key]^Entity, 0, project.allocator),
		imported     = make([dynamic]^Scope, 0, 1, project.allocator),
		declarations = make([dynamic]^Entity, 0, 8, project.allocator),
		children     = make([dynamic]^Scope, 0, 4, project.allocator),
	}
	scope, err := xar.push_back_elem_and_get_ptr(&project.scopes, value)
	assert(err == .None && scope != nil)
	return scope
}

project_new_type :: proc(project: ^Project, kind: Type_Kind = .Unknown) -> ^Type {
	value := Type{kind = kind}
	if kind == .Routine {
		value.routine.parameters = make([dynamic]^Entity, 0, 4, project.allocator)
		value.routine.results = make([dynamic]^Entity, 0, 1, project.allocator)
		value.routine.exceptions = make([dynamic]string_interner.String, 0, 1, project.allocator)
	}
	typ := new(Type, project.allocator)
	assert(typ != nil)
	typ^ = value
	return typ
}

@(private)
project_unknown_type: Type = Type{kind = .Unknown}

project_type_unknown :: #force_inline proc(_: ^Project = nil) -> ^Type {
	return &project_unknown_type
}

project_type_builtin :: proc(project: ^Project, name: string_interner.String, entity: ^Entity = nil) -> ^Type {
	typ := project_new_type(project, .Builtin)
	typ.name = name
	typ.entity = entity
	return typ
}

project_type_named :: proc(
	project: ^Project,
	name: string_interner.String,
	entity: ^Entity,
	base: ^Type = nil,
) -> ^Type {
	typ := project_new_type(project, .Named)
	typ.name = name
	typ.entity = entity
	typ.base = base
	return typ
}

project_type_structure :: proc(project: ^Project, structure: ^Structure) -> ^Type {
	assert(structure != nil)
	typ := project_new_type(project, .Structure)
	typ.name = structure.name
	typ.structure = structure
	return typ
}

project_type_table :: proc(
	project: ^Project,
	row: ^Type,
	form: ast.Data_Type_Form,
) -> ^Type {
	typ := project_new_type(project, .Table)
	typ.base = row if row != nil else project_type_unknown(project)
	typ.table_form = form
	return typ
}

project_type_ref :: proc(project: ^Project, target: ^Type) -> ^Type {
	typ := project_new_type(project, .Ref)
	typ.base = target if target != nil else project_type_unknown(project)
	return typ
}

project_type_class_or_interface :: proc(
	project: ^Project,
	name: string_interner.String,
	entity: ^Entity,
	kind: Entity_Kind,
) -> ^Type {
	assert(kind == .Class || kind == .Interface)
	typ := project_new_type(project, .Class if kind == .Class else .Interface)
	typ.name = name
	typ.entity = entity
	return typ
}

project_type_routine :: proc(project: ^Project, signature_scope: ^Scope = nil) -> ^Type {
	typ := project_new_type(project, .Routine)
	typ.routine.signature_scope = signature_scope
	return typ
}

project_new_structure :: proc(
	project: ^Project,
	name: string_interner.String = string_interner.String(0),
	source_file: ^Project_File = nil,
	scope: ^Scope = nil,
	range: Range = {},
) -> ^Structure {
	value := Structure {
		name        = name,
		range       = range,
		source_file = source_file,
		scope       = scope,
		fields      = make([dynamic]^Entity, 0, 4, project.allocator),
	}
	structure := new(Structure, project.allocator)
	assert(structure != nil)
	structure^ = value
	structure.origin_structure = structure
	return structure
}

project_new_decl_info :: proc(
	project: ^Project,
	entity: ^Entity,
	scope: ^Scope,
	name: string_interner.String,
	kind: Entity_Kind,
	name_range: Range = {},
	decl_node: ^ast.Node = nil,
	type_clause: ^ast.Data_Type_Clause = nil,
	occurs: ^ast.Expr = nil,
	value_clause: ^ast.Value_Clause = nil,
	default_clause: ^ast.Default_Clause = nil,
	docs: []ast.Ast_Trivia = nil,
	comment: []ast.Ast_Trivia = nil,
) -> ^Decl_Info {
	value := Decl_Info {
		entity         = entity,
		scope          = scope,
		decl_node      = decl_node,
		type_clause    = type_clause,
		occurs         = occurs,
		value_clause   = value_clause,
		default_clause = default_clause,
		docs           = docs,
		comment        = comment,
	}
	info := new(Decl_Info, project.allocator)
	assert(info != nil)
	info^ = value
	if entity != nil {
		if kind != .Invalid {
			if entity.kind == .Invalid {
				entity_set_kind(entity, kind, project.allocator)
			} else {
				assert(entity.kind == kind)
			}
		}
		entity.decl_info = info
		entity.name = name
		entity.name_range = name_range
		entity.scope = scope
	}
	return info
}
