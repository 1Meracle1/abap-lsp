package abap_frontend_semantic

import "src:ast"
import string_interner "src:string_interner"

import "core:strings"

Checker_Diagnostic_Kind :: enum {
	Duplicate_Declaration,
	Shadowed_Declaration,
	Declaration_Cycle,
	Missing_Declaration_Info,
	Invalid_Context,
	Invalid_Object_Type_Reference,
	Invalid_Generic_Builtin_Type,
	Invalid_Generic_Table_Type,
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
	assert(project != nil)
	checker^ = {}
	checker.project = project
	checker.info = checker_info_make(checker, project)
	checker.info.builtin_scope = checker_ensure_builtin_scope(checker)
	checker.builtin_context = checker_context_make(checker)
	checker.builtin_context.scope = checker.info.builtin_scope
	checker_register_builtins(checker)
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
	assert(checker != nil && checker.project != nil && checker.info.builtin_scope != nil)
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
	assert(ctx != nil && ctx.checker != nil)
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
) -> (
	^Scope,
	^Entity,
	bool,
) {
	if found_scope, entity, ok := checker_lookup_lexical_declaration_from_scope(scope, namespace, name); ok {
		return found_scope, entity, true
	}

	if owner := checker_enclosing_object_owner(scope); owner != nil {
		if entity, ok := checker_lookup_object_member(owner, namespace, name); ok {
			return entity.scope, entity, true
		}
	}

	return nil, nil, false
}

checker_lookup_lexical_declaration_from_scope :: proc(
	scope: ^Scope,
	namespace: Namespace,
	name: string_interner.String,
) -> (
	^Scope,
	^Entity,
	bool,
) {
	assert(scope != nil)
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

checker_lookup_reference :: proc(
	ctx: ^Checker_Context,
	namespace: Namespace,
	name: string_interner.String,
) -> (
	^Scope,
	^Entity,
	bool,
) {
	assert(ctx != nil && ctx.scope != nil)
	if scope, entity, ok := checker_lookup_declaration(ctx, namespace, name); ok {
		return scope, entity, true
	}
	if namespace == .Value {
		return checker_lookup_declaration(ctx, .Type, name)
	}
	return nil, nil, false
}

checker_lookup_object_member :: proc(
	owner: ^Entity,
	namespace: Namespace,
	name: string_interner.String,
) -> (^Entity, bool) {
	return checker_lookup_object_member_internal(owner, namespace, name, 0)
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
	if entity, found := scope_lookup_declaration(payload.definition_scope, namespace, name); found {
		return entity, true
	}
	if entity, found := checker_lookup_object_alias_member(owner, namespace, name, depth + 1); found {
		return entity, true
	}
	if entity, found := checker_lookup_implemented_interface_member(owner, namespace, name, depth + 1); found {
		return entity, true
	}
	if string_interner.is_valid(payload.superclass_name) {
		if _, super, super_ok := checker_lookup_lexical_declaration_from_scope(owner.scope, .Type, payload.superclass_name);
		   super_ok && super.kind == .Class {
			if entity, found := checker_lookup_object_member_internal(super, namespace, name, depth + 1); found {
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
) -> (^Entity, bool) {
	payload, ok := owner.payload.(^Entity_Object_Payload)
	assert(ok && payload != nil)
	if payload.definition_scope == nil {
		return nil, false
	}
	for alias in payload.definition_scope.declarations {
		if alias.kind != .Alias || alias.name != name {
			continue
		}
		alias_payload, alias_ok := alias.payload.(^Entity_Alias_Payload)
		assert(alias_ok && alias_payload != nil)
		if !string_interner.is_valid(alias_payload.target_interface_name) {
			continue
		}
		_, target_interface, interface_ok := checker_lookup_lexical_declaration_from_scope(
			owner.scope,
			.Type,
			alias_payload.target_interface_name,
		)
		if !interface_ok || target_interface.kind != .Interface {
			continue
		}
		target_name := alias_payload.target_member_name
		if !string_interner.is_valid(target_name) {
			target_name = name
		}
		if entity, found := checker_lookup_object_member_internal(target_interface, namespace, target_name, depth + 1); found {
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
) -> (^Entity, bool) {
	payload, ok := owner.payload.(^Entity_Object_Payload)
	assert(ok && payload != nil)
	for interface_name in payload.implemented_interfaces {
		if !string_interner.is_valid(interface_name) {
			continue
		}
		_, interface_entity, interface_ok := checker_lookup_lexical_declaration_from_scope(
			owner.scope,
			.Type,
			interface_name,
		)
		if !interface_ok || interface_entity.kind != .Interface {
			continue
		}
		if entity, found := checker_lookup_object_member_internal(interface_entity, namespace, name, depth + 1); found {
			return entity, true
		}
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
			checker_add_diagnostic(ctx, .Shadowed_Declaration, entity.name_range, "declaration shadows outer symbol", entity, decl)
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
	assert(entity != nil)
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
	case ^ast.Data_Decl,
	     ^ast.Data_Chained_Decl,
	     ^ast.Types_Decl,
	     ^ast.Constants_Decl,
	     ^ast.Field_Symbols_Decl,
	     ^ast.Statics_Decl,
	     ^ast.Tables_Decl,
	     ^ast.Ranges_Decl,
	     ^ast.Parameters_Decl,
	     ^ast.Select_Options_Decl,
	     ^ast.Controls_Decl,
	     ^ast.Class_Data_Decl,
	     ^ast.Function_Pool_Decl,
	     ^ast.Include_Stmt,
	     ^ast.Report_Stmt,
	     ^ast.Class_Decl,
	     ^ast.Interface_Decl,
	     ^ast.Method_Decl,
	     ^ast.Form_Decl,
	     ^ast.Function_Decl,
	     ^ast.Module_Decl,
	     ^ast.Event_Block_Stmt,
	     ^ast.Oop_Simple_Stmt:
		checker_collect_stmt_entities(ctx, stmt)
	case ^ast.Data_Inline_Decl:
		checker_collect_stmt_entities(ctx, stmt)
		checker_check_expr(ctx, n.expr)
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
		if namespace == .Type {
			typ, entity := checker_type_from_expr(ctx, expr, .Type)
			return checker_record_operand(ctx, node, .Type, typ, entity, lhs)
		}
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
		callee := checker_check_expr(ctx, n.callee, .Routine)
		checker_check_expr(ctx, n.args)
		if callee.entity != nil && callee.entity.kind == .Builtin {
			return checker_check_builtin_call(ctx, node, callee.entity, n, lhs)
		}
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
	_, entity, ok := checker_lookup_reference(ctx, namespace, interned)
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

checker_addressing_mode_for_entity :: proc(entity: ^Entity) -> ast.Addressing_Mode {
	#partial switch entity.kind {
	case .Type_Def, .Class, .Interface:
		return .Type
	case .Form, .Method, .Module, .Event, .Builtin:
		return .Routine
	case .Constant, .Enum_Member:
		return .Constant
	case .Field:
		return .Field
	case .Variable, .Field_Symbol, .Parameter, .Exception, .Control:
		return .Variable
	}
	return .Value
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
