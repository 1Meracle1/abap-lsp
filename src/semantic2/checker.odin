package abap_frontend_semantic

import "src:ast"
import string_interner "src:string_interner"

import "core:strings"

Checker_Diagnostic_Kind :: enum {
	Duplicate_Declaration,
	Declaration_Cycle,
	Missing_Declaration_Info,
	Invalid_Context,
}

Checker_Diagnostic :: struct {
	kind:    Checker_Diagnostic_Kind,
	range:   Range,
	message: string,
	entity:  ^Entity,
	decl:    ^Decl_Info,
}

Checker_Expr_Info :: struct {
	mode:   ast.Addressing_Mode,
	is_lhs: bool,
	type:   ^Type,
	value:  ast.Exact_Value_Id,
}

Checker_Expr_Record :: struct {
	node: ^ast.Node,
	info: Checker_Expr_Info,
}

Operand :: struct {
	mode:   ast.Addressing_Mode,
	type:   ^Type,
	value:  ast.Exact_Value_Id,
	expr:   ^ast.Node,
	entity: ^Entity,
}

Checker_Dependency :: struct {
	decl:   ^Decl_Info,
	entity: ^Entity,
}

Checker_Entity_Use :: struct {
	node:   ^ast.Node,
	scope:  ^Scope,
	decl:   ^Decl_Info,
	entity: ^Entity,
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

checker_make :: proc(project: ^Project) -> (checker: Checker) {
	checker_init(&checker, project)
	return
}

checker_init :: proc(checker: ^Checker, project: ^Project) {
	checker^ = {}
	checker.project = project
	checker.info = checker_info_make(checker, project)
	checker.info.builtin_scope = checker_ensure_builtin_scope(checker)
	checker.builtin_context = checker_context_make(checker)
	checker.builtin_context.scope = checker.info.builtin_scope
}

checker_info_make :: proc(checker: ^Checker, project: ^Project) -> Checker_Info {
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
	}
}

checker_context_make :: proc(checker: ^Checker, file: ^Project_File = nil) -> Checker_Context {
	ctx := Checker_Context {
		checker   = checker,
		info      = &checker.info,
		project   = checker.project,
		scope     = checker.info.builtin_scope,
		type_path = make([dynamic]^Entity, 0, 16, checker.project.allocator),
	}
	if file != nil {
		checker_context_set_file(&ctx, file)
	}
	return ctx
}

checker_context_reset :: proc(ctx: ^Checker_Context, file: ^Project_File = nil) {
	checker := ctx.checker

	type_path := ctx.type_path
	clear(&type_path)
	ctx^ = Checker_Context {
		checker   = checker,
		info      = &checker.info,
		project   = checker.project,
		scope     = checker.info.builtin_scope,
		type_path = type_path,
	}
	if file != nil {
		checker_context_set_file(ctx, file)
	}
}

checker_context_set_file :: proc(ctx: ^Checker_Context, file: ^Project_File) -> bool {
	ctx.file = file
	ctx.scope = file.root_scope if file.root_scope != nil else ctx.info.builtin_scope
	ctx.decl = nil
	return true
}

checker_ensure_builtin_scope :: proc(checker: ^Checker) -> ^Scope {
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
	file := project_add_file(checker.project, path, root)
	checker_register_file(checker, file)
	return file
}

checker_register_file :: proc(checker: ^Checker, file: ^Project_File) -> bool {
	for registered in checker.info.files {
		if registered == file {
			if file.root_scope == nil {
				file.root_scope = checker_create_file_scope(checker, file)
			}
			return true
		}
	}
	if file.root_scope == nil {
		file.root_scope = checker_create_file_scope(checker, file)
	}
	append(&checker.info.files, file)
	return true
}

checker_create_file_scope :: proc(checker: ^Checker, file: ^Project_File) -> ^Scope {
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
	scope := checker_create_scope(ctx.checker, ctx.scope, kind, range, owner, decl_info)
	ctx.scope = scope
	return scope
}

checker_close_scope :: proc(ctx: ^Checker_Context) {
	if ctx.scope.parent != nil {
		ctx.scope = ctx.scope.parent
	}
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
	return checker_lookup_declaration_from_scope(ctx.scope, namespace, name)
}

checker_lookup_declaration_from_scope :: proc(
	scope: ^Scope,
	namespace: Namespace,
	name: string_interner.String,
) -> (
	^Scope,
	^Entity,
	bool,
) {
	for current := scope; current != nil; current = current.parent {
		if entity, ok := scope_lookup_declaration(current, namespace, name); ok {
			return current, entity, true
		}
		for imported in current.imported {
			if entity, ok := scope_lookup_declaration(imported, namespace, name); ok {
				return imported, entity, true
			}
		}
	}
	return nil, nil, false
}

checker_add_entity_and_decl_info :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	decl: ^Decl_Info,
	insert_in_scope := true,
) -> bool {
	scope := decl.scope if decl.scope != nil else ctx.scope

	decl.scope = scope
	decl.entity = entity
	entity.decl_info = decl
	if entity.scope == nil {
		entity.scope = scope
	}
	if entity.source_file == nil {
		entity.source_file = ctx.file
	}

	if insert_in_scope {
		if previous := scope_insert_declaration(scope, entity); previous != nil && previous != entity {
			checker_add_diagnostic(ctx, .Duplicate_Declaration, entity.name_range, "duplicate declaration", entity, decl)
			return false
		}
	}

	checker_add_definition(ctx.info, entity)
	if !entity_kind_is_builtin(entity.kind) && entity.state == .Unresolved {
		checker_enqueue_entity(ctx.info, entity)
	}
	return true
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
	if entity == nil {
		return
	}
	entity.flags += {.Used}
	checker_add_dependency(ctx, entity)
	append(
		&ctx.info.uses,
		Checker_Entity_Use {
			node   = node,
			scope  = ctx.scope,
			decl   = ctx.decl,
			entity = entity,
		},
	)
}

checker_add_dependency :: proc(ctx: ^Checker_Context, entity: ^Entity) {
	if ctx.decl == nil || entity == nil {
		return
	}
	for dep in ctx.info.dependencies {
		if dep.decl == ctx.decl && dep.entity == entity {
			return
		}
	}
	append(&ctx.info.dependencies, Checker_Dependency{decl = ctx.decl, entity = entity})
}

checker_record_expr_info :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	mode: ast.Addressing_Mode,
	typ: ^Type,
	value: ast.Exact_Value_Id = ast.INVALID_EXACT_VALUE_ID,
	is_lhs := false,
) -> Checker_Expr_Info {
	info := Checker_Expr_Info {
		mode   = mode,
		is_lhs = is_lhs,
		type   = typ,
		value  = value,
	}
	if node != nil {
		append(&ctx.info.expr_infos, Checker_Expr_Record{node = node, info = info})
	}
	return info
}

checker_check_file :: proc(checker: ^Checker, file: ^Project_File) {
	checker_register_file(checker, file)
	ctx := checker_context_make(checker, file)
	checker_collect_file_entities(&ctx, file)
	checker_check_queued_entities(&ctx)
}

checker_collect_file_entities :: proc(ctx: ^Checker_Context, file: ^Project_File) {
	checker_context_set_file(ctx, file)
	if file.root == nil {
		return
	}
	for stmt in file.root.stmts {
		checker_collect_stmt_entities(ctx, stmt)
	}
}

checker_collect_stmt_entities :: proc(ctx: ^Checker_Context, stmt: ^ast.Stmt) {
	if stmt == nil {
		return
	}
	#partial switch n in stmt.derived_stmt {
	case ^ast.Data_Decl:
		_ = checker_collect_variable_decl(
			ctx,
			ctx.scope,
			n.name,
			.Variable,
			n.range,
			&n.node.decl_base.stmt_base,
			n.type_clause,
			n.value_clause,
		)
	case ^ast.Data_Chained_Decl:
		for clause in n.decls {
			_ = checker_collect_variable_decl(
				ctx,
				ctx.scope,
				clause.name,
				.Variable,
				n.range,
				&n.node.decl_base.stmt_base,
				clause.type_clause,
				clause.value_clause,
			)
		}
	case ^ast.Types_Decl:
		checker_collect_types_decl(ctx, n)
	case ^ast.Form_Decl:
		checker_collect_routine_decl(ctx, n.name, .Form, n.range, n.header_range, n.header_text, &n.node.stmt_base)
	case ^ast.Method_Decl:
		checker_collect_routine_decl(ctx, n.name, .Method, n.range, n.header_range, n.header_text, &n.node.stmt_base)
	case ^ast.Function_Decl:
		checker_collect_routine_decl(ctx, n.name, .Module, n.range, n.header_range, n.header_text, &n.node.stmt_base)
	case ^ast.Module_Decl:
		checker_collect_routine_decl(ctx, n.name, .Module, n.range, n.header_range, n.header_text, &n.node.stmt_base)
	case ^ast.Event_Block_Stmt:
		checker_collect_routine_decl(ctx, n.kind, .Event, n.range, n.header_range, n.header_text, &n.node.stmt_base)
	}
}

checker_collect_types_decl :: proc(ctx: ^Checker_Context, decl: ^ast.Types_Decl) {
	current_structure: ^Structure
	current_structure_scope: ^Scope
	current_owner: ^Entity
	for clause in decl.types {
		#partial switch clause.kind {
		case .Begin_Group:
			entity := checker_collect_type_decl(ctx, ctx.scope, clause.name, decl.range, &decl.node.decl_base.stmt_base, clause.type_clause)
			structure_scope := checker_create_scope(ctx.checker, ctx.scope, .Structure, decl.range, entity, entity.decl_info)
			structure := project_new_structure(ctx.project, entity.name, ctx.file, structure_scope, decl.range)
			structure_type := project_type_structure(ctx.project, structure)
			entity.type = project_type_named(ctx.project, entity.name, entity, structure_type)
			if payload, ok := entity.payload.(^Entity_Type_Name_Payload); ok && payload != nil {
				payload.structure = structure
				payload.underlying = structure_type
				payload.original_type = entity.type
			}
			current_structure = structure
			current_structure_scope = structure_scope
			current_owner = entity
		case .End_Group:
			current_structure = nil
			current_structure_scope = nil
			current_owner = nil
		case:
			if current_structure != nil {
				checker_collect_structure_field(ctx, current_structure, current_structure_scope, current_owner, clause, decl.range, &decl.node.decl_base.stmt_base)
			} else {
				_ = checker_collect_type_decl(ctx, ctx.scope, clause.name, decl.range, &decl.node.decl_base.stmt_base, clause.type_clause)
			}
		}
	}
}

checker_collect_type_decl :: proc(
	ctx: ^Checker_Context,
	scope: ^Scope,
	name: string,
	range: Range,
	node: ^ast.Node,
	type_clause: ^ast.Data_Type_Clause,
) -> ^Entity {
	entity := project_new_entity(ctx.project, .Type_Def)
	entity.node = node
	interned := checker_intern_name(ctx.project, name)
	decl := project_new_decl_info(ctx.project, entity, scope, interned, .Type_Def, range, node, type_clause)
	_ = checker_add_entity_and_decl_info(ctx, entity, decl)
	return entity
}

checker_collect_structure_field :: proc(
	ctx: ^Checker_Context,
	structure: ^Structure,
	scope: ^Scope,
	owner: ^Entity,
	clause: ast.Types_Clause,
	range: Range,
	node: ^ast.Node,
) -> ^Entity {
	entity := project_new_entity(ctx.project, .Field)
	entity.node = node
	entity.owner = owner
	entity.source_file = ctx.file
	interned := checker_intern_name(ctx.project, clause.name)
	decl := project_new_decl_info(ctx.project, entity, scope, interned, .Field, range, node, clause.type_clause)
	payload, ok := entity.payload.(^Entity_Field_Payload)
	if ok && payload != nil {
		payload.owner_structure = structure
		payload.decl_unit = ctx.file
		payload.decl_range = range
		payload.field_index = i32(len(structure.fields))
		payload.type_clause_form = clause.type_clause.form if clause.type_clause != nil else ast.Data_Type_Form.Type
		payload.has_type_clause_form = clause.type_clause != nil
		if clause.type_clause != nil {
			payload.flags += {.Has_Type_Ref}
		}
	}
	append(&structure.fields, entity)
	_ = checker_add_entity_and_decl_info(ctx, entity, decl)
	return entity
}

checker_collect_variable_decl :: proc(
	ctx: ^Checker_Context,
	scope: ^Scope,
	name: string,
	kind: Entity_Kind,
	range: Range,
	node: ^ast.Node,
	type_clause: ^ast.Data_Type_Clause,
	value_clause: ^ast.Value_Clause,
) -> ^Entity {
	entity := project_new_entity(ctx.project, kind)
	entity.node = node
	interned := checker_intern_name(ctx.project, name)
	decl := project_new_decl_info(ctx.project, entity, scope, interned, kind, range, node, type_clause, value_clause)
	_ = checker_add_entity_and_decl_info(ctx, entity, decl)
	return entity
}

checker_collect_routine_decl :: proc(
	ctx: ^Checker_Context,
	name: string,
	kind: Entity_Kind,
	range: Range,
	header_range: Range,
	signature: string,
	node: ^ast.Node,
) -> ^Entity {
	entity := project_new_entity(ctx.project, kind)
	entity.node = node
	interned := checker_intern_name(ctx.project, name)
	decl := project_new_decl_info(ctx.project, entity, ctx.scope, interned, kind, header_range, node)
	_ = checker_add_entity_and_decl_info(ctx, entity, decl)
	payload, ok := entity.payload.(^Entity_Routine_Payload)
	if ok && payload != nil {
		payload.signature = strings.clone(signature, ctx.project.allocator) if signature != "" else ""
		payload.signature_scope = checker_create_scope(ctx.checker, ctx.scope, checker_scope_kind_for_routine(kind), range, entity, decl)
		payload.body_scope = payload.signature_scope
	}
	checker_collect_routine_parameters(ctx, entity, node)
	return entity
}

checker_collect_routine_parameters :: proc(ctx: ^Checker_Context, owner: ^Entity, node: ^ast.Node) {
	payload, ok := owner.payload.(^Entity_Routine_Payload)
	if !ok || payload == nil || payload.signature_scope == nil || node == nil {
		return
	}
	#partial switch n in node.derived {
	case ^ast.Form_Decl:
		for param in n.form_parameters {
			checker_collect_parameter_decl(
				ctx,
				payload.signature_scope,
				owner,
				param.name,
				param.range,
				param.type_clause,
				checker_form_parameter_section_from_ast(param.section),
				checker_parameter_passing_from_ast(param.passing),
			)
		}
	case ^ast.Function_Decl:
		for param in n.function_parameters {
			checker_collect_parameter_decl(
				ctx,
				payload.signature_scope,
				owner,
				param.name,
				param.range,
				param.type_clause,
				checker_function_parameter_section_from_ast(param.section),
				checker_parameter_passing_from_ast(param.passing),
			)
		}
		for exception in n.exceptions {
			exc := checker_collect_variable_decl(ctx, payload.signature_scope, exception.name, .Exception, exception.range, node, nil, nil)
			exc.owner = owner
			append(&payload.exceptions, exc.name)
		}
	}
}

checker_collect_parameter_decl :: proc(
	ctx: ^Checker_Context,
	scope: ^Scope,
	owner: ^Entity,
	name: string,
	range: Range,
	type_clause: ^ast.Data_Type_Clause,
	section: Entity_Parameter_Section,
	passing: Entity_Parameter_Passing,
) -> ^Entity {
	entity := checker_collect_variable_decl(ctx, scope, name, .Parameter, range, owner.node, type_clause, nil)
	entity.owner = owner
	if payload, ok := entity.payload.(^Entity_Variable_Payload); ok && payload != nil {
		payload.section = section
		payload.passing = passing
	}
	if routine, ok := owner.payload.(^Entity_Routine_Payload); ok && routine != nil {
		append(&routine.parameters, entity)
	}
	return entity
}

checker_check_queued_entities :: proc(ctx: ^Checker_Context) {
	for index := 0; index < len(ctx.info.entity_queue); index += 1 {
		entity := ctx.info.entity_queue[index]
		checker_check_entity_decl(ctx, entity)
	}
	clear(&ctx.info.entity_queue)
}

checker_check_entity_decl :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	decl: ^Decl_Info = nil,
	named_type: ^Type = nil,
) {
	if entity.state == .Resolved || entity.state == .Failed {
		return
	}
	current_decl := decl
	if entity.state == .Resolving || checker_type_path_contains(ctx, entity) {
		entity.state = .Failed
		checker_add_diagnostic(ctx, .Declaration_Cycle, entity.name_range, "declaration cycle", entity, current_decl)
		return
	}
	if current_decl == nil {
		current_decl = entity.decl_info
	}
	if current_decl == nil {
		entity.state = .Failed
		checker_add_diagnostic(ctx, .Missing_Declaration_Info, entity.name_range, "missing declaration info", entity)
		return
	}

	local := ctx^
	local.scope = current_decl.scope if current_decl.scope != nil else entity.scope
	local.decl = current_decl
	local.current_decl = current_decl
	entity.state = .Resolving
	current_decl.state = .Resolving

	track_path := checker_entity_tracks_type_path(entity)
	if track_path {
		append(&local.type_path, entity)
	}
	defer if track_path {
		_ = pop(&local.type_path)
	}

	switch entity.kind {
	case .Builtin_Type:
		checker_check_builtin_type_decl(&local, entity)
	case .Builtin_Routine, .Builtin_Constant, .Builtin_Variable:
		checker_check_builtin_value_decl(&local, entity)
	case .Variable, .Field_Symbol, .Parameter, .Exception, .Control:
		checker_check_variable_decl(&local, entity, current_decl)
	case .Constant, .Enum_Member:
		checker_check_constant_decl(&local, entity, current_decl)
	case .Type_Def:
		checker_check_type_decl(&local, entity, current_decl, named_type)
	case .Form, .Method, .Module, .Event:
		checker_check_routine_decl(&local, entity, current_decl)
	case .Class, .Interface:
		checker_check_object_decl(&local, entity, current_decl)
	case .Field, .Include, .Alias, .Report:
		checker_check_metadata_decl(&local, entity, current_decl)
	case .Invalid:
		entity.state = .Failed
		current_decl.state = .Failed
		return
	}

	if entity.state == .Resolving {
		entity.state = .Resolved
	}
	if current_decl.state == .Resolving {
		current_decl.state = .Resolved if entity.state == .Resolved else .Failed
	}
	if entity.state == .Resolved {
		append(&ctx.info.checked_entities, entity)
	}
}

checker_check_builtin_type_decl :: proc(ctx: ^Checker_Context, entity: ^Entity) {
	if entity.type == nil {
		entity.type = project_type_builtin(ctx.project, entity.name, entity)
	}
}

checker_check_builtin_value_decl :: proc(ctx: ^Checker_Context, entity: ^Entity) {
	if entity.type == nil {
		entity.type = project_type_unknown(ctx.project)
	}
}

checker_check_variable_decl :: proc(ctx: ^Checker_Context, entity: ^Entity, decl: ^Decl_Info) {
	checker_check_type_clause(ctx, decl.type_clause)
	checker_check_value_clause(ctx, decl.value_clause)
	if entity.type == nil {
		entity.type = project_type_unknown(ctx.project)
	}
}

checker_check_constant_decl :: proc(ctx: ^Checker_Context, entity: ^Entity, decl: ^Decl_Info) {
	checker_check_type_clause(ctx, decl.type_clause)
	checker_check_value_clause(ctx, decl.value_clause)
	if entity.type == nil {
		entity.type = project_type_unknown(ctx.project)
	}
}

checker_check_type_decl :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	decl: ^Decl_Info,
	named_type: ^Type,
) {
	checker_check_type_clause(ctx, decl.type_clause)
	if named_type != nil {
		entity.type = named_type
		return
	}
	if entity.type == nil {
		entity.type = project_type_named(ctx.project, entity.name, entity, project_type_unknown(ctx.project))
	}
}

checker_check_routine_decl :: proc(ctx: ^Checker_Context, entity: ^Entity, decl: ^Decl_Info) {
	payload, ok := entity.payload.(^Entity_Routine_Payload)
	if ok && payload != nil {
		if payload.signature_scope == nil {
			payload.signature_scope = checker_create_scope(
				ctx.checker,
				entity.scope,
				checker_scope_kind_for_routine(entity.kind),
				owner = entity,
			)
		}
		if entity.type == nil || entity.type.kind != .Routine {
			entity.type = project_type_routine(ctx.project, payload.signature_scope)
		}
		entity.type.routine.parameters = payload.parameters
		entity.type.routine.exceptions = payload.exceptions
		body := checker_routine_body_from_decl(decl)
		if len(body) > 0 {
			body_ctx := ctx^
			body_ctx.scope = payload.body_scope if payload.body_scope != nil else payload.signature_scope
			body_ctx.current_routine = entity
			body_ctx.current_signature = entity.type
			checker_check_stmt_list(&body_ctx, body)
		}
		return
	}
	if entity.type == nil {
		entity.type = project_type_routine(ctx.project)
	}
}

checker_scope_kind_for_routine :: proc(kind: Entity_Kind) -> Scope_Kind {
	#partial switch kind {
	case .Form:
		return .Form
	case .Module:
		return .Module
	case .Event:
		return .Event
	case .Method:
		return .Method
	case:
		unreachable()
	}
	return .Method
}

checker_check_object_decl :: proc(ctx: ^Checker_Context, entity: ^Entity, decl: ^Decl_Info) {
	_ = decl
	payload, ok := entity.payload.(^Entity_Object_Payload)
	if ok && payload != nil && payload.definition_scope == nil {
		scope_kind := Scope_Kind.Class if entity.kind == .Class else Scope_Kind.Interface
		payload.definition_scope = checker_create_scope(ctx.checker, entity.scope, scope_kind, owner = entity)
	}
	if entity.type == nil {
		entity.type = project_type_class_or_interface(ctx.project, entity.name, entity, entity.kind)
	}
}

checker_check_metadata_decl :: proc(ctx: ^Checker_Context, entity: ^Entity, decl: ^Decl_Info) {
	checker_check_type_clause(ctx, decl.type_clause)
	checker_check_value_clause(ctx, decl.value_clause)
	if entity.type == nil {
		entity.type = project_type_unknown(ctx.project)
	}
}

checker_check_type_clause :: proc(ctx: ^Checker_Context, clause: ^ast.Data_Type_Clause) {
	if clause == nil {
		return
	}
	checker_check_expr(ctx, clause.type_ref, .Type)
	checker_check_expr(ctx, clause.initial_size, .Value)
}

checker_check_value_clause :: proc(ctx: ^Checker_Context, clause: ^ast.Value_Clause) {
	if clause == nil {
		return
	}
	checker_check_expr(ctx, clause.expr)
}

checker_check_stmt_list :: proc(ctx: ^Checker_Context, body: [dynamic]^ast.Stmt) {
	for stmt in body {
		checker_check_stmt(ctx, stmt)
	}
}

checker_check_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Stmt) {
	if stmt == nil {
		return
	}
	#partial switch n in stmt.derived_stmt {
	case ^ast.Data_Decl:
		entity := checker_collect_variable_decl(
			ctx,
			ctx.scope,
			n.name,
			.Variable,
			n.range,
			&n.node.decl_base.stmt_base,
			n.type_clause,
			n.value_clause,
		)
		checker_check_entity_decl(ctx, entity)
	case ^ast.Data_Chained_Decl:
		for clause in n.decls {
			entity := checker_collect_variable_decl(
				ctx,
				ctx.scope,
				clause.name,
				.Variable,
				n.range,
				&n.node.decl_base.stmt_base,
				clause.type_clause,
				clause.value_clause,
			)
			checker_check_entity_decl(ctx, entity)
		}
	case ^ast.Data_Inline_Decl:
		entity := checker_collect_variable_decl(ctx, ctx.scope, n.name, .Variable, n.range, &n.node.decl_base.stmt_base, nil, nil)
		checker_check_expr(ctx, n.expr)
		checker_check_entity_decl(ctx, entity)
	case ^ast.Types_Decl:
		checker_collect_types_decl(ctx, n)
	case ^ast.Assign_Stmt:
		checker_check_expr(ctx, n.lhs, .Value, true)
		checker_check_expr(ctx, n.rhs)
	case ^ast.Downcast_Assign_Stmt:
		checker_check_expr(ctx, n.lhs, .Value, true)
		checker_check_expr(ctx, n.rhs)
	case ^ast.Expr_Stmt:
		checker_check_expr(ctx, n.expr)
	case ^ast.If_Stmt:
		checker_check_expr(ctx, n.condition)
		checker_check_stmt_list(ctx, n.body)
		for clause in n.elseif_clauses {
			checker_check_expr(ctx, clause.condition)
			checker_check_stmt_list(ctx, clause.body)
		}
		if n.else_clause != nil {
			checker_check_stmt_list(ctx, n.else_clause.body)
		}
	case ^ast.While_Stmt:
		checker_check_expr(ctx, n.condition)
		checker_check_stmt_list(ctx, n.body)
	case ^ast.Do_Stmt:
		checker_check_expr(ctx, n.count)
		checker_check_stmt_list(ctx, n.body)
	case ^ast.Loop_Stmt:
		checker_check_expr(ctx, n.source)
		checker_check_expr(ctx, n.target, .Value, true)
		checker_check_expr(ctx, n.from)
		checker_check_expr(ctx, n.to)
		checker_check_expr(ctx, n.where_cond)
		checker_check_expr(ctx, n.group_by)
		checker_check_expr(ctx, n.group_target, .Value, true)
		checker_check_stmt_list(ctx, n.body)
	}
}

checker_check_expr :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	namespace: Namespace = .Value,
	lhs := false,
) -> Operand {
	if expr == nil {
		return Operand{mode = .Invalid, value = ast.INVALID_EXACT_VALUE_ID}
	}
	node := &expr.expr_base
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		return checker_check_ident_expr(ctx, node, n.name, namespace, lhs)
	case ^ast.Literal_Expr:
		return checker_record_operand(ctx, node, .Constant, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Type_Ref_Expr:
		name := n.base_name
		if name == "" {
			name = n.name
		}
		operand := checker_check_ident_expr(ctx, node, name, .Type, lhs)
		for raw_ref in n.raw_refs {
			ref_namespace := Namespace.Type if raw_ref.type_base else Namespace.Value
			checker_check_ident_name(ctx, node, raw_ref.name, ref_namespace, false)
		}
		return operand
	case ^ast.Binary_Expr:
		checker_check_expr(ctx, n.left)
		checker_check_expr(ctx, n.right)
		return checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Unary_Expr:
		checker_check_expr(ctx, n.expr)
		return checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Paren_Expr:
		return checker_check_expr(ctx, n.expr, namespace, lhs)
	case ^ast.Selector_Expr:
		checker_check_expr(ctx, n.base, namespace, lhs)
		checker_check_expr(ctx, n.field, .Value, lhs)
		return checker_record_operand(ctx, node, .Field, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Interface_Qualified_Selector_Expr:
		checker_check_expr(ctx, n.receiver, namespace, lhs)
		checker_check_expr(ctx, n.interface, .Type)
		checker_check_expr(ctx, n.member, .Value, lhs)
		return checker_record_operand(ctx, node, .Field, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Call_Expr:
		checker_check_expr(ctx, n.callee, .Routine)
		checker_check_expr(ctx, n.args)
		return checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Call_Arg_List_Expr:
		for arg in n.args {
			checker_check_expr(ctx, arg)
		}
		return checker_record_operand(ctx, node, .No_Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Call_Arg_Section_Expr:
		for arg in n.args {
			checker_check_expr(ctx, arg)
		}
		return checker_record_operand(ctx, node, .No_Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Call_Named_Arg_Expr:
		checker_check_expr(ctx, n.value)
		return checker_record_operand(ctx, node, .No_Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Call_Positional_Arg_Expr:
		checker_check_expr(ctx, n.value)
		return checker_record_operand(ctx, node, .No_Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Constructor_Expr:
		checker_check_expr(ctx, n.type_ref, .Type)
		for arg in n.args {
			checker_check_expr(ctx, arg)
		}
		return checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Is_Predicate_Expr:
		checker_check_expr(ctx, n.subject)
		return checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Instance_Of_Predicate_Expr:
		checker_check_expr(ctx, n.subject)
		checker_check_expr(ctx, n.type_ref, .Type)
		return checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Between_Expr:
		checker_check_expr(ctx, n.subject)
		checker_check_expr(ctx, n.low)
		checker_check_expr(ctx, n.high)
		return checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project), lhs = lhs)
	case ^ast.Data_Inline_Name_Expr:
		entity := checker_collect_variable_decl(ctx, ctx.scope, n.name, .Variable, n.range, node, nil, nil)
		checker_check_entity_decl(ctx, entity)
		return checker_record_operand(ctx, node, .Variable, entity.type, entity, lhs)
	case ^ast.Field_Symbol_Inline_Name_Expr:
		entity := checker_collect_variable_decl(ctx, ctx.scope, n.name, .Field_Symbol, n.range, node, nil, nil)
		checker_check_entity_decl(ctx, entity)
		return checker_record_operand(ctx, node, .Variable, entity.type, entity, lhs)
	}
	return checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project), lhs = lhs)
}

checker_check_ident_expr :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	name: string,
	namespace: Namespace,
	lhs: bool,
) -> Operand {
	entity, ok := checker_check_ident_name(ctx, node, name, namespace, lhs)
	if !ok {
		return checker_record_operand(ctx, node, .Value, project_type_unknown(ctx.project), lhs = lhs)
	}
	mode := checker_addressing_mode_for_entity(entity)
	return checker_record_operand(ctx, node, mode, entity.type if entity.type != nil else project_type_unknown(ctx.project), entity, lhs)
}

checker_check_ident_name :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	name: string,
	namespace: Namespace,
	lhs: bool,
) -> (^Entity, bool) {
	interned := checker_intern_name(ctx.project, name)
	if !string_interner.is_valid(interned) {
		return nil, false
	}
	_, entity, ok := checker_lookup_declaration(ctx, namespace, interned)
	if !ok && namespace == .Value {
		_, entity, ok = checker_lookup_declaration(ctx, .Type, interned)
	}
	if !ok {
		return nil, false
	}
	checker_add_entity_use(ctx, node, entity)
	return entity, true
}

checker_record_operand :: proc(
	ctx: ^Checker_Context,
	node: ^ast.Node,
	mode: ast.Addressing_Mode,
	typ: ^Type,
	entity: ^Entity = nil,
	lhs := false,
) -> Operand {
	info := checker_record_expr_info(ctx, node, mode, typ, is_lhs = lhs)
	return Operand {
		mode   = info.mode,
		type   = info.type,
		value  = info.value,
		expr   = node,
		entity = entity,
	}
}

checker_entity_tracks_type_path :: proc(entity: ^Entity) -> bool {
	#partial switch entity.kind {
	case .Variable, .Constant, .Enum_Member, .Type_Def, .Field:
		return true
	case:
		return false
	}
}

checker_type_path_contains :: proc(ctx: ^Checker_Context, entity: ^Entity) -> bool {
	for item in ctx.type_path {
		if item == entity {
			return true
		}
	}
	return false
}

checker_routine_body_from_decl :: proc(decl: ^Decl_Info) -> [dynamic]^ast.Stmt {
	if decl == nil || decl.decl_node == nil {
		return nil
	}
	#partial switch n in decl.decl_node.derived {
	case ^ast.Form_Decl:
		return n.body
	case ^ast.Method_Decl:
		return n.body
	case ^ast.Function_Decl:
		return n.body
	case ^ast.Module_Decl:
		return n.body
	case ^ast.Event_Block_Stmt:
		return n.body
	}
	return nil
}

checker_addressing_mode_for_entity :: proc(entity: ^Entity) -> ast.Addressing_Mode {
	#partial switch entity.kind {
	case .Type_Def, .Builtin_Type, .Class, .Interface:
		return .Type
	case .Form, .Method, .Module, .Event, .Builtin_Routine:
		return .Routine
	case .Constant, .Enum_Member, .Builtin_Constant:
		return .Constant
	case .Field:
		return .Field
	case .Variable, .Field_Symbol, .Parameter, .Exception, .Control, .Builtin_Variable:
		return .Variable
	}
	return .Value
}

checker_intern_name :: proc(project: ^Project, name: string) -> string_interner.String {
	canonical := strings.to_lower(name, context.temp_allocator)
	return string_interner.insert(project.interner, canonical)
}

checker_form_parameter_section_from_ast :: proc(section: ast.Form_Parameter_Section) -> Entity_Parameter_Section {
	switch section {
	case .Tables:
		return .Form_Tables
	case .Using:
		return .Form_Using
	case .Changing:
		return .Form_Changing
	}
	return .None
}

checker_function_parameter_section_from_ast :: proc(
	section: ast.Function_Parameter_Section,
) -> Entity_Parameter_Section {
	switch section {
	case .Importing:
		return .Function_Importing
	case .Exporting:
		return .Function_Exporting
	case .Changing:
		return .Function_Changing
	case .Tables:
		return .Function_Tables
	}
	return .None
}

checker_parameter_passing_from_ast :: proc(passing: ast.Parameter_Passing_Kind) -> Entity_Parameter_Passing {
	switch passing {
	case .Direct:
		return .Direct
	case .Value:
		return .Value
	case .Reference:
		return .Reference
	}
	return .None
}

checker_add_diagnostic :: proc(
	ctx: ^Checker_Context,
	kind: Checker_Diagnostic_Kind,
	range: Range,
	message: string,
	entity: ^Entity = nil,
	decl: ^Decl_Info = nil,
) {
	append(
		&ctx.info.diagnostics,
		Checker_Diagnostic {
			kind    = kind,
			range   = range,
			message = strings.clone(message, ctx.project.allocator) if message != "" else "",
			entity  = entity,
			decl    = decl,
		},
	)
}
