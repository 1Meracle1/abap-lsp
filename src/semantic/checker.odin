package abap_frontend_semantic2

import "src:ast"
import string_interner "src:string_interner"

import "core:strings"

Checker_Diagnostic_Kind :: enum {
	Duplicate_Declaration,
	Shadowed_Declaration,
	Declaration_Cycle,
	Missing_Declaration_Info,
	Invalid_Context,
	Unresolved_Reference,
	Unresolved_Type,
	Invalid_Object_Type_Reference,
	Invalid_Generic_Builtin_Type,
	Invalid_Generic_Table_Type,
	Incompatible_Assignment_Type,
	Incompatible_Argument_Type,
	Unknown_Named_Parameter,
	Missing_Required_Parameter,
	Duplicate_Named_Parameter,
	Inaccessible_Member,
	Unresolved_Open_Sql_Source,
	Unknown_Field,
	Invalid_Open_Sql_Into_Target,
	Invalid_Loop_Source,
	Invalid_Append_Operand,
	Invalid_Sort_Operand,
	Invalid_Concatenate_Operand,
	Invalid_Split_Operand,
	Invalid_Syntax_Form,
	Unresolved_Include,
	Unresolved_Include_If_Found,
	Include_Cycle,
	Root_File_Included,
}

Checker_Diagnostic_Severity :: enum {
	Error,
	Warning,
	Note,
}

Checker_Diagnostic :: struct {
	kind:     Checker_Diagnostic_Kind,
	severity: Checker_Diagnostic_Severity,
	range:    Range,
	message:  string,
	file:     ^Project_File,
	entity:   ^Entity,
	decl:     ^Decl_Info,
}

Checker_Expr_Info :: struct {
	mode:   ast.Addressing_Mode,
	is_lhs: bool,
	type:   ^Type,
	value:  ast.Exact_Value_Id,
}

Checker_Expr_Record :: struct {
	node: ^ast.Node,
	file: ^Project_File,
	info: Checker_Expr_Info,
}

Checker_Dependency :: struct {
	decl:   ^Decl_Info,
	entity: ^Entity,
}

Checker_Entity_Use :: struct {
	node:   ^ast.Node,
	file:   ^Project_File,
	scope:  ^Scope,
	decl:   ^Decl_Info,
	entity: ^Entity,
	range:  Range,
}

Checker_Info :: struct {
	checker:          ^Checker,
	project:          ^Project,
	builtin_scope:    ^Scope,
	files:            [dynamic]^Project_File,
	definitions:      [dynamic]^Entity,
	entity_queue:     [dynamic]^Entity,
	checked_entities: [dynamic]^Entity,
	dependencies:     [dynamic]Checker_Dependency,
	uses:             [dynamic]Checker_Entity_Use,
	expr_infos:       [dynamic]Checker_Expr_Record,
	diagnostics:      [dynamic]Checker_Diagnostic,
	unresolved:       [dynamic]Checker_Unresolved_Candidate,
	resolved_external_dependencies:   [dynamic]Semantic_Dependency_Edge,
	unresolved_external_dependencies: [dynamic]Semantic_Dependency_Edge,
	external:         ^External_Semantics,
}

Checker_Context :: struct {
	checker:           ^Checker,
	info:              ^Checker_Info,
	project:           ^Project,
	file:              ^Project_File,
	scope:             ^Scope,
	decl:              ^Decl_Info,
	type_hint:         ^Type,
	type_hint_expr:    ^ast.Node,
	diagnose_unresolved_value_refs: bool,
	cursor_shapes:     [dynamic]Checker_Cursor_Query,
	current_decl:      ^Decl_Info,
	current_routine:   ^Entity,
	current_signature: ^Type,
	in_signature:      bool,
	type_path:         [dynamic]^Entity,
}

Checker :: struct {
	project:         ^Project,
	info:            Checker_Info,
	builtin_context: Checker_Context,
}

checker_make :: proc(project: ^Project, external: ^External_Semantics = nil) -> (checker: Checker) {
	checker_init(&checker, project, external)
	return
}

checker_init :: proc(checker: ^Checker, project: ^Project, external: ^External_Semantics = nil) {
	checker_init_with_builtins(checker, project, external)
}

checker_init_with_builtins :: proc(
	checker: ^Checker,
	project: ^Project,
	external: ^External_Semantics = nil,
	shared_builtin_scope: ^Scope = nil,
) {
	assert(project != nil)
	checker^ = {}
	checker.project = project
	checker.info = checker_info_make(checker, project, external)
	checker.info.builtin_scope = checker_ensure_builtin_scope(checker)
	if shared_builtin_scope != nil {
		assert(shared_builtin_scope.kind == .Builtin)
		assert(shared_builtin_scope != checker.info.builtin_scope)
		append(&checker.info.builtin_scope.imported, shared_builtin_scope)
	}
	checker.builtin_context = checker_context_make(checker)
	checker.builtin_context.scope = checker.info.builtin_scope
	if shared_builtin_scope == nil {
		checker_register_builtins(checker)
	}
}

checker_info_make :: proc(
	checker: ^Checker,
	project: ^Project,
	external: ^External_Semantics = nil,
) -> Checker_Info {
	return Checker_Info {
		checker          = checker,
		project          = project,
		files            = make([dynamic]^Project_File, 0, 8, project.allocator),
		definitions      = make([dynamic]^Entity, 0, 16, project.allocator),
		entity_queue     = make([dynamic]^Entity, 0, 16, project.allocator),
		checked_entities = make([dynamic]^Entity, 0, 16, project.allocator),
		dependencies     = make([dynamic]Checker_Dependency, 0, 16, project.allocator),
		uses             = make([dynamic]Checker_Entity_Use, 0, 32, project.allocator),
		expr_infos       = make([dynamic]Checker_Expr_Record, 0, 32, project.allocator),
		diagnostics      = make([dynamic]Checker_Diagnostic, 0, 8, project.allocator),
		unresolved       = make([dynamic]Checker_Unresolved_Candidate, 0, 8, project.allocator),
		resolved_external_dependencies   = make([dynamic]Semantic_Dependency_Edge, 0, 8, project.allocator),
		unresolved_external_dependencies = make([dynamic]Semantic_Dependency_Edge, 0, 8, project.allocator),
		external         = external,
	}
}

checker_context_make :: proc(checker: ^Checker, file: ^Project_File = nil) -> Checker_Context {
	assert(checker != nil && checker.project != nil && checker.info.builtin_scope != nil)
	ctx := Checker_Context {
		checker       = checker,
		info          = &checker.info,
		project       = checker.project,
		scope         = checker.info.builtin_scope,
		cursor_shapes = make([dynamic]Checker_Cursor_Query, 0, 4, checker.project.allocator),
		type_path     = make([dynamic]^Entity, 0, 16, checker.project.allocator),
	}
	if file != nil {
		checker_context_set_file(&ctx, file)
	}
	return ctx
}

checker_context_reset :: proc(ctx: ^Checker_Context, file: ^Project_File = nil) {
	assert(ctx != nil && ctx.checker != nil)
	checker := ctx.checker

	cursor_shapes := ctx.cursor_shapes
	clear(&cursor_shapes)
	type_path := ctx.type_path
	clear(&type_path)
	ctx^ = Checker_Context {
		checker       = checker,
		info          = &checker.info,
		project       = checker.project,
		scope         = checker.info.builtin_scope,
		cursor_shapes = cursor_shapes,
		type_path     = type_path,
	}
	if file != nil {
		checker_context_set_file(ctx, file)
	}
}

checker_context_set_file :: proc(ctx: ^Checker_Context, file: ^Project_File) {
	assert(ctx != nil && file != nil)
	checker_register_file(ctx.checker, file)
	assert(file.root_scope != nil)
	ctx.file = file
	ctx.scope = file.root_scope
	ctx.decl = nil
}

checker_ensure_builtin_scope :: proc(checker: ^Checker) -> ^Scope {
	assert(checker != nil)
	if checker.info.builtin_scope != nil {
		return checker.info.builtin_scope
	}
	scope := checker_create_scope(checker, nil, .Builtin)
	checker.info.builtin_scope = scope
	return scope
}

checker_add_file :: proc(
	checker: ^Checker,
	path: string = "",
	root: ^ast.File = nil,
) -> ^Project_File {
	assert(checker != nil)
	file := project_add_file(checker.project, path, root)
	checker_register_file(checker, file)
	return file
}

checker_register_file :: proc(checker: ^Checker, file: ^Project_File) -> bool {
	assert(checker != nil && file != nil)
	_ = checker_ensure_file_scope(checker, file)
	for registered in checker.info.files {
		if registered == file {
			return true
		}
	}
	append(&checker.info.files, file)
	return true
}

checker_ensure_file_scope :: proc(checker: ^Checker, file: ^Project_File) -> ^Scope {
	assert(checker != nil && file != nil)
	if file.root_scope == nil {
		file.root_scope = checker_create_file_scope(checker, file)
	}
	assert(file.root_scope != nil)
	assert(file.root_scope.parent == checker.info.builtin_scope)
	return file.root_scope
}

checker_create_file_scope :: proc(checker: ^Checker, file: ^Project_File) -> ^Scope {
	assert(checker != nil && file != nil)
	range := Range{}
	if file.root != nil {
		range = file.root.range
	}
	return checker_create_scope(checker, checker_ensure_builtin_scope(checker), .File, range)
}

checker_create_scope :: proc(
	checker: ^Checker,
	parent: ^Scope,
	kind: Scope_Kind,
	range: Range = {},
	owner: ^Entity = nil,
	decl_info: ^Decl_Info = nil,
) -> ^Scope {
	assert(checker != nil && checker.project != nil)
	assert(kind == .Builtin || parent != nil)
	scope := project_new_scope(checker.project)
	scope.kind = kind
	scope.parent = parent
	scope.range = range
	scope.owner = owner
	scope.decl_info = decl_info
	if parent != nil {
		scope.next = parent.head_child
		parent.head_child = scope
		append(&parent.children, scope)
	}
	return scope
}

checker_open_scope :: proc(
	ctx: ^Checker_Context,
	kind: Scope_Kind,
	range: Range = {},
	owner: ^Entity = nil,
	decl_info: ^Decl_Info = nil,
) -> ^Scope {
	assert(ctx != nil && ctx.scope != nil)
	scope := checker_create_scope(ctx.checker, ctx.scope, kind, range, owner, decl_info)
	ctx.scope = scope
	return scope
}

checker_close_scope :: proc(ctx: ^Checker_Context) {
	assert(ctx != nil && ctx.scope != nil && ctx.scope.parent != nil)
	ctx.scope = ctx.scope.parent
}

checker_lookup_declaration :: proc(
	ctx: ^Checker_Context,
	namespace: Namespace,
	name: string_interner.String,
) -> (
	^Scope,
	^Entity,
	bool,
) {
	assert(ctx != nil && ctx.scope != nil)
	return checker_lookup_declaration_from_scope(ctx.scope, namespace, name)
}

checker_lookup_declaration_from_scope :: proc(
	scope: ^Scope,
	namespace: Namespace,
	name: string_interner.String,
	excluded: ^Entity = nil,
) -> (
	^Scope,
	^Entity,
	bool,
) {
	if found_scope, entity, ok := checker_lookup_lexical_declaration_from_scope(scope, namespace, name, excluded); ok {
		return found_scope, entity, true
	}

	if owner := checker_enclosing_object_owner(scope); owner != nil {
		if entity, ok := checker_lookup_object_member_from_scope(scope, owner, namespace, name, excluded); ok {
			return entity.scope, entity, true
		}
	}

	return nil, nil, false
}

checker_lookup_lexical_declaration_from_scope :: proc(
	scope: ^Scope,
	namespace: Namespace,
	name: string_interner.String,
	excluded: ^Entity = nil,
) -> (
	^Scope,
	^Entity,
	bool,
) {
	assert(scope != nil)
	for current := scope; current != nil; current = current.parent {
		if entity, ok := scope_lookup_declaration(current, namespace, name); ok && entity != excluded {
			return current, entity, true
		}
		for imported in current.imported {
			if entity, ok := scope_lookup_declaration(imported, namespace, name); ok && entity != excluded {
				return imported, entity, true
			}
		}
	}
	return nil, nil, false
}

checker_lookup_reference :: proc(
	ctx: ^Checker_Context,
	namespace: Namespace,
	name: string_interner.String,
	preferred_external_kind: External_Candidate_Kind = .Global_Symbol,
	excluded: ^Entity = nil,
) -> (
	^Scope,
	^Entity,
	bool,
) {
	assert(ctx != nil && ctx.scope != nil)
	if scope, entity, ok := checker_lookup_declaration_from_scope(ctx.scope, namespace, name, excluded); ok {
		return scope, entity, true
	}
	if owner := checker_enclosing_object_owner(ctx.scope); owner != nil {
		if entity, ok := checker_lookup_object_member_visible(ctx, owner, namespace, name); ok {
			return entity.scope, entity, true
		}
	}
	if namespace == .Value {
		if scope, entity, ok := checker_lookup_declaration_from_scope(ctx.scope, .Type, name, excluded); ok {
			return scope, entity, true
		}
	}
	if ctx.info.external != nil {
		if key, binding, ok := external_semantic_index_lookup(&ctx.info.external.index, namespace, name, preferred_external_kind); ok {
			checker_add_resolved_external_dependency(ctx, key, binding)
			return binding.entity.scope, binding.entity, true
		}
		if namespace == .Value {
			if key, binding, ok := external_semantic_index_lookup(&ctx.info.external.index, .Type, name, preferred_external_kind); ok {
				checker_add_resolved_external_dependency(ctx, key, binding)
				return binding.entity.scope, binding.entity, true
			}
		}
	}
	return nil, nil, false
}

checker_lookup_object_member :: proc(
	owner: ^Entity,
	namespace: Namespace,
	name: string_interner.String,
	excluded: ^Entity = nil,
) -> (^Entity, bool) {
	return checker_lookup_object_member_internal(owner, namespace, name, 0, excluded)
}

checker_lookup_object_member_checked :: proc(
	ctx: ^Checker_Context,
	owner: ^Entity,
	namespace: Namespace,
	name: string_interner.String,
	excluded: ^Entity = nil,
) -> (^Entity, bool) {
	return checker_lookup_object_member_internal(owner, namespace, name, 0, excluded, ctx)
}

checker_lookup_structure_field :: proc(
	structure: ^Structure,
	name: string_interner.String,
) -> (^Entity, bool) {
	assert(structure != nil && structure.scope != nil)
	return scope_lookup_declaration(structure.scope, .Value, name)
}

checker_lookup_object_member_internal :: proc(
	owner: ^Entity,
	namespace: Namespace,
	name: string_interner.String,
	depth: int,
	excluded: ^Entity = nil,
	ctx: ^Checker_Context = nil,
) -> (^Entity, bool) {
	assert(owner != nil)
	if depth > 64 {
		return nil, false
	}
	payload, ok := owner.payload.(^Entity_Object_Payload)
	assert(ok && payload != nil)
	if payload.definition_scope == nil {
		return nil, false
	}
	if entity, found := scope_lookup_declaration(payload.definition_scope, namespace, name); found && entity != excluded {
		return entity, true
	}
	if entity, found := checker_lookup_object_alias_member(owner, namespace, name, depth + 1, excluded, ctx); found {
		return entity, true
	}
	if entity, found := checker_lookup_implemented_interface_member(owner, namespace, name, depth + 1, excluded, ctx); found {
		return entity, true
	}
	if string_interner.is_valid(payload.superclass_name) {
		if super, super_ok := checker_lookup_object_type_from_scope(ctx, owner.scope, payload.superclass_name, .Class); super_ok {
			if entity, found := checker_lookup_object_member_internal(super, namespace, name, depth + 1, excluded, ctx); found {
				return entity, true
			}
		}
	}
	return nil, false
}

checker_lookup_object_alias_member :: proc(
	owner: ^Entity,
	namespace: Namespace,
	name: string_interner.String,
	depth: int,
	excluded: ^Entity = nil,
	ctx: ^Checker_Context = nil,
) -> (^Entity, bool) {
	payload, ok := owner.payload.(^Entity_Object_Payload)
	assert(ok && payload != nil)
	if payload.definition_scope == nil {
		return nil, false
	}
	for alias in payload.definition_scope.declarations {
		if alias == excluded || alias.kind != .Alias || alias.name != name {
			continue
		}
		alias_payload, alias_ok := alias.payload.(^Entity_Alias_Payload)
		assert(alias_ok && alias_payload != nil)
		if !string_interner.is_valid(alias_payload.target_interface_name) {
			continue
		}
		target_interface, interface_ok := checker_lookup_object_type_from_scope(ctx, owner.scope, alias_payload.target_interface_name, .Interface)
		if !interface_ok {
			continue
		}
		target_name := alias_payload.target_member_name
		if !string_interner.is_valid(target_name) {
			target_name = name
		}
		if entity, found := checker_lookup_object_member_internal(target_interface, namespace, target_name, depth + 1, excluded, ctx); found {
			return entity, true
		}
	}
	return nil, false
}

checker_lookup_implemented_interface_member :: proc(
	owner: ^Entity,
	namespace: Namespace,
	name: string_interner.String,
	depth: int,
	excluded: ^Entity = nil,
	ctx: ^Checker_Context = nil,
) -> (^Entity, bool) {
	payload, ok := owner.payload.(^Entity_Object_Payload)
	assert(ok && payload != nil)
	for interface_name in payload.implemented_interfaces {
		if !string_interner.is_valid(interface_name) {
			continue
		}
		interface_entity, interface_ok := checker_lookup_object_type_from_scope(ctx, owner.scope, interface_name, .Interface)
		if !interface_ok {
			continue
		}
		if entity, found := checker_lookup_object_member_internal(interface_entity, namespace, name, depth + 1, excluded, ctx); found {
			return entity, true
		}
	}
	return nil, false
}

checker_lookup_object_type_from_scope :: proc(
	ctx: ^Checker_Context,
	scope: ^Scope,
	name: string_interner.String,
	kind: Entity_Kind,
) -> (^Entity, bool) {
	assert(kind == .Class || kind == .Interface)
	if scope == nil || !string_interner.is_valid(name) {
		return nil, false
	}
	if ctx != nil {
		preferred := External_Candidate_Kind.Class if kind == .Class else External_Candidate_Kind.Interface
		if entity, ok := checker_lookup_type_name_from_scope(ctx, scope, name, preferred); ok && entity.kind == kind {
			return entity, true
		}
		return nil, false
	}
	if _, entity, ok := checker_lookup_lexical_declaration_from_scope(scope, .Type, name); ok && entity.kind == kind {
		return entity, true
	}
	return nil, false
}

checker_enclosing_object_owner :: proc(scope: ^Scope) -> ^Entity {
	assert(scope != nil)
	for current := scope; current != nil; current = current.parent {
		if (current.kind == .Class || current.kind == .Interface) && current.owner != nil {
			return current.owner
		}
	}
	return nil
}

checker_add_entity_and_decl_info :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	decl: ^Decl_Info,
	insert_in_scope := true,
) -> bool {
	assert(ctx != nil && entity != nil && decl != nil)
	assert(ctx.scope != nil && decl.scope != nil)
	scope := decl.scope

	decl.scope = scope
	decl.entity = entity
	entity.decl_info = decl
	if entity.scope == nil {
		entity.scope = scope
	} else {
		assert(entity.scope == scope)
	}
	if entity.source_file == nil {
		entity.source_file = ctx.file
	}
	assert(entity_is_builtin(entity) || entity.source_file != nil)

	if insert_in_scope {
		if previous := scope_insert_declaration(scope, entity); previous != nil && previous != entity {
			checker_add_diagnostic(ctx, .Duplicate_Declaration, entity.name_range, "duplicate declaration", entity, decl)
			return false
		}
		if shadowed := checker_shadowed_declaration(scope, entity); shadowed != nil {
			checker_add_diagnostic(
				ctx,
				.Shadowed_Declaration,
				entity.name_range,
				"declaration shadows outer symbol",
				entity,
				decl,
				severity = .Warning,
			)
		}
	}

	checker_add_definition(ctx.info, entity)
	if !entity_is_builtin(entity) && entity.state == .Unresolved {
		checker_enqueue_entity(ctx.info, entity)
	}
	return true
}

checker_shadowed_declaration :: proc(scope: ^Scope, entity: ^Entity) -> ^Entity {
	assert(scope != nil && entity != nil)
	if entity_is_builtin(entity) {
		return nil
	}
	#partial switch scope.kind {
	case .Class, .Interface, .Structure:
		return nil
	}
	namespaces := [?]Namespace{.Value, .Type, .Routine}
	for parent := scope.parent; parent != nil; parent = parent.parent {
		for namespace in namespaces {
			if !entity_kind_occupies(entity.kind, namespace) {
				continue
			}
			if existing, ok := scope_lookup_declaration(parent, namespace, entity.name); ok && !entity_is_builtin(existing) {
				return existing
			}
		}
	}
	return nil
}

checker_add_definition :: proc(info: ^Checker_Info, entity: ^Entity) {
	for existing in info.definitions {
		if existing == entity {
			return
		}
	}
	append(&info.definitions, entity)
}

checker_enqueue_entity :: proc(info: ^Checker_Info, entity: ^Entity) {
	for existing in info.entity_queue {
		if existing == entity {
			return
		}
	}
	append(&info.entity_queue, entity)
}

checker_add_entity_use :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	entity: ^Entity,
) {
	range := node.range if node != nil else Range{}
	checker_add_entity_use_at_range(ctx, node, entity, range)
}

checker_add_entity_use_at_range :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	entity: ^Entity,
	range: Range,
) {
	assert(entity != nil)
	entity.flags += {.Used}
	checker_add_dependency(ctx, entity)
	append(
		&ctx.info.uses,
		Checker_Entity_Use {
			node   = node,
			file   = ctx.file,
			scope  = ctx.scope,
			decl   = ctx.decl,
			entity = entity,
			range  = range,
		},
	)
}

checker_add_dependency :: proc(ctx: ^Checker_Context, entity: ^Entity) {
	assert(ctx != nil && entity != nil)
	if ctx.decl == nil {
		return
	}
	for dep in ctx.info.dependencies {
		if dep.decl == ctx.decl && dep.entity == entity {
			return
		}
	}
	append(&ctx.info.dependencies, Checker_Dependency{decl = ctx.decl, entity = entity})
}

checker_check_file :: proc(checker: ^Checker, file: ^Project_File) {
	checker_register_file(checker, file)
	ctx := checker_context_make(checker, file)
	checker_collect_file_entities(&ctx, file)
	checker_check_queued_entities(&ctx)
	if file.root != nil {
		checker_check_stmt_list(&ctx, file.root.stmts, collect_declarations = false)
	}
}

checker_intern_name :: proc(project: ^Project, name: string) -> string_interner.String {
	canonical := strings.to_lower(name, context.temp_allocator)
	return string_interner.insert(project.interner, canonical)
}

checker_add_diagnostic :: proc(
	ctx: ^Checker_Context,
	kind: Checker_Diagnostic_Kind,
	range: Range,
	message: string,
	entity: ^Entity = nil,
	decl: ^Decl_Info = nil,
	severity: Checker_Diagnostic_Severity = .Error,
) {
	append(
		&ctx.info.diagnostics,
		Checker_Diagnostic {
			kind     = kind,
			severity = severity,
			range    = range,
			message  = strings.clone(message, ctx.project.allocator) if message != "" else "",
			file     = ctx.file,
			entity   = entity,
			decl     = decl,
		},
	)
}
