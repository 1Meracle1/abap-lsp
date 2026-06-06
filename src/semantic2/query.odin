package abap_frontend_semantic2

import "src:ast"
import string_interner "src:string_interner"

import "core:mem"
import "core:strings"

Semantic_Query :: struct {
	project: ^Project,
	checker: ^Checker,
	file:    ^Project_File,
}

Semantic_Decl_Query :: struct {
	project: ^Project,
	checker: ^Checker,
	file:    ^Project_File,
}

Semantic_Ref_Query :: struct {
	project: ^Project,
	checker: ^Checker,
	file:    ^Project_File,
}

Semantic_Fact_Query :: struct {
	project: ^Project,
	checker: ^Checker,
	file:    ^Project_File,
}

Semantic_Diagnostic_Query :: struct {
	project: ^Project,
	checker: ^Checker,
	file:    ^Project_File,
}

Semantic_Completion_Query :: struct {
	project: ^Project,
	checker: ^Checker,
	file:    ^Project_File,
}

Semantic_Expression_Info_Kind :: enum {
	Reference,
	Selector,
	Call_Result,
}

Semantic_Expression_Info :: struct {
	node:  ^ast.Node,
	kind:  Semantic_Expression_Info_Kind,
	scope: ^Scope,
	range: Range,
	info:  Checker_Expr_Info,
}

Semantic_Completion_Item_Source :: enum {
	Lexical_Scope,
	Builtin_Scope,
}

Semantic_Completion_Item :: struct {
	name:      string_interner.String,
	namespace: Namespace,
	entity:    ^Entity,
	source:    Semantic_Completion_Item_Source,
	range:     Range,
}

Semantic_Completion_Item_Key :: struct {
	name:      string_interner.String,
	namespace: Namespace,
	source:    Semantic_Completion_Item_Source,
}

semantic_query :: proc(
	project: ^Project,
	checker: ^Checker,
	file: ^Project_File = nil,
) -> Semantic_Query {
	assert(project != nil && checker != nil && checker.project == project)
	return Semantic_Query{project = project, checker = checker, file = file}
}

semantic_query_decls :: proc(q: Semantic_Query) -> Semantic_Decl_Query {
	assert(q.project != nil && q.checker != nil)
	return Semantic_Decl_Query{project = q.project, checker = q.checker, file = q.file}
}

semantic_query_refs :: proc(q: Semantic_Query) -> Semantic_Ref_Query {
	assert(q.project != nil && q.checker != nil)
	return Semantic_Ref_Query{project = q.project, checker = q.checker, file = q.file}
}

semantic_query_facts :: proc(q: Semantic_Query) -> Semantic_Fact_Query {
	assert(q.project != nil && q.checker != nil)
	return Semantic_Fact_Query{project = q.project, checker = q.checker, file = q.file}
}

semantic_query_diagnostics :: proc(q: Semantic_Query) -> Semantic_Diagnostic_Query {
	assert(q.project != nil && q.checker != nil)
	return Semantic_Diagnostic_Query{project = q.project, checker = q.checker, file = q.file}
}

semantic_query_completion :: proc(q: Semantic_Query) -> Semantic_Completion_Query {
	assert(q.project != nil && q.checker != nil)
	return Semantic_Completion_Query{project = q.project, checker = q.checker, file = q.file}
}

semantic_decl_entity_at_offset :: proc(q: Semantic_Decl_Query, offset: int) -> ^Entity {
	best: ^Entity
	best_width := 0
	for entity in q.checker.info.definitions {
		if entity == nil || !semantic_query_entity_matches_file(entity, q.file) {
			continue
		}
		if !semantic_range_contains_offset(entity.name_range, offset) {
			continue
		}
		width := semantic_range_width(entity.name_range)
		if best == nil || width < best_width {
			best = entity
			best_width = width
		}
	}
	return best
}

semantic_decl_entity_with_kind_and_decl_range :: proc(
	q: Semantic_Decl_Query,
	kind: Entity_Kind,
	range: Range,
) -> ^Entity {
	for entity in q.checker.info.definitions {
		if entity != nil &&
		   semantic_query_entity_matches_file(entity, q.file) &&
		   entity.kind == kind &&
		   entity.name_range == range {
			return entity
		}
	}
	return nil
}

semantic_decl_class_member_at_offset :: proc(q: Semantic_Decl_Query, offset: int) -> ^Entity {
	best: ^Entity
	best_width := 0
	for entity in q.checker.info.definitions {
		if entity == nil ||
		   !semantic_query_entity_matches_file(entity, q.file) ||
		   !semantic_entity_is_object_member(entity) {
			continue
		}
		range := semantic_member_query_range(entity, offset)
		if !semantic_range_contains_offset(range, offset) {
			continue
		}
		width := semantic_range_width(range)
		if best == nil || width < best_width {
			best = entity
			best_width = width
		}
	}
	return best
}

semantic_decl_class_member :: proc(
	q: Semantic_Decl_Query,
	owner: ^Entity,
	name: string,
	namespace: Namespace = .Routine,
) -> ^Entity {
	if owner == nil {
		return nil
	}
	payload, ok := owner.payload.(^Entity_Object_Payload)
	if !ok || payload == nil || payload.definition_scope == nil {
		return nil
	}
	interned := semantic_query_intern_name(q.project, name)
	if !string_interner.is_valid(interned) {
		return nil
	}
	entity, found := scope_lookup_declaration(payload.definition_scope, namespace, interned)
	return entity if found else nil
}

semantic_decl_structure_field :: proc(
	q: Semantic_Decl_Query,
	structure: ^Structure,
	name: string,
) -> ^Entity {
	if structure == nil {
		return nil
	}
	interned := semantic_query_intern_name(q.project, name)
	if !string_interner.is_valid(interned) {
		return nil
	}
	field, ok := checker_lookup_structure_field(structure, interned)
	return field if ok else nil
}

semantic_decl_structure_field_at_offset :: proc(q: Semantic_Decl_Query, offset: int) -> ^Entity {
	best: ^Entity
	best_width := 0
	for entity in q.checker.info.definitions {
		if entity == nil ||
		   entity.kind != .Field ||
		   !semantic_query_entity_matches_file(entity, q.file) {
			continue
		}
		field, ok := entity.payload.(^Entity_Field_Payload)
		if !ok || field == nil || !semantic_range_contains_offset(field.decl_range, offset) {
			continue
		}
		width := semantic_range_width(field.decl_range)
		if best == nil || width < best_width {
			best = entity
			best_width = width
		}
	}
	return best
}

semantic_ref_use_at_offset :: proc(q: Semantic_Ref_Query, offset: int) -> ^Checker_Entity_Use {
	best := -1
	best_width := 0
	for use, i in q.checker.info.uses {
		if use.node == nil || !semantic_query_use_matches_file(use, q.file) {
			continue
		}
		if !semantic_range_contains_offset(use.node.range, offset) {
			continue
		}
		width := semantic_range_width(use.node.range)
		if best < 0 || width < best_width {
			best = i
			best_width = width
		}
	}
	return &q.checker.info.uses[best] if best >= 0 else nil
}

semantic_ref_use_at_range :: proc(q: Semantic_Ref_Query, range: Range) -> ^Checker_Entity_Use {
	for &use in q.checker.info.uses {
		if use.node != nil &&
		   semantic_query_use_matches_file(use, q.file) &&
		   use.node.range == range {
			return &use
		}
	}
	return nil
}

semantic_ref_resolving_to_entity :: proc(
	q: Semantic_Ref_Query,
	entity: ^Entity,
	allocator: mem.Allocator,
) -> [dynamic]^Checker_Entity_Use {
	out := make([dynamic]^Checker_Entity_Use, 0, 4, allocator)
	if entity == nil {
		return out
	}
	for &use in q.checker.info.uses {
		if use.entity == entity && semantic_query_use_matches_file(use, q.file) {
			append(&out, &use)
		}
	}
	return out
}

semantic_fact_expression_info_at_offset :: proc(
	q: Semantic_Fact_Query,
	offset: int,
) -> (Semantic_Expression_Info, bool) {
	record, kind, ok := semantic_fact_expr_record_at_offset(q, offset)
	if !ok {
		return {}, false
	}
	return Semantic_Expression_Info {
			node  = record.node,
			kind  = kind,
			scope = semantic_fact_scope_for_node(q, record.node),
			range = record.node.range if record.node != nil else Range{},
			info  = record.info,
		},
		true
}

semantic_fact_operand_info_at_offset :: proc(
	q: Semantic_Fact_Query,
	offset: int,
) -> (Checker_Expr_Info, bool) {
	record, _, ok := semantic_fact_expr_record_at_offset(q, offset)
	if !ok {
		return {}, false
	}
	return record.info, true
}

semantic_fact_type_at_offset :: proc(q: Semantic_Fact_Query, offset: int) -> (^Type, bool) {
	if info, ok := semantic_fact_operand_info_at_offset(q, offset); ok {
		return info.type, true
	}
	return nil, false
}

semantic_diagnostic_copies :: proc(
	q: Semantic_Diagnostic_Query,
	allocator: mem.Allocator,
) -> [dynamic]Checker_Diagnostic {
	count := len(q.checker.info.diagnostics) if q.checker != nil else 0
	out := make([dynamic]Checker_Diagnostic, 0, count, allocator)
	if q.checker == nil {
		return out
	}
	for diagnostic in q.checker.info.diagnostics {
		if q.file != nil && diagnostic.entity != nil && !semantic_query_entity_matches_file(diagnostic.entity, q.file) {
			continue
		}
		append(&out, diagnostic)
	}
	return out
}

semantic_completion_items_at_offset :: proc(
	q: Semantic_Completion_Query,
	offset: int,
	prefix: string,
	allocator: mem.Allocator,
) -> [dynamic]Semantic_Completion_Item {
	out := make([dynamic]Semantic_Completion_Item, 0, 32, allocator)
	seen := make(map[Semantic_Completion_Item_Key]bool, 64, allocator)
	canonical_prefix := strings.to_lower(prefix, context.temp_allocator)

	scope := semantic_query_scope_at_offset(q.project, q.file, offset)
	for current := scope; current != nil; current = current.parent {
		source := Semantic_Completion_Item_Source.Builtin_Scope if current.kind == .Builtin else Semantic_Completion_Item_Source.Lexical_Scope
		for entity in current.declarations {
			semantic_completion_append_entity(
				q.project,
				entity,
				source,
				canonical_prefix,
				&seen,
				&out,
			)
		}
	}
	return out
}

semantic_query_scope_at_offset :: proc(
	project: ^Project,
	file: ^Project_File,
	offset: int,
) -> ^Scope {
	_ = project
	if file == nil {
		return nil
	}
	best := file.root_scope
	best_width := semantic_range_width(file.root_scope.range) if file.root_scope != nil else 0
	semantic_query_scope_at_offset_walk(file.root_scope, offset, &best, &best_width)
	return best
}

semantic_query_scope_at_offset_walk :: proc(
	scope: ^Scope,
	offset: int,
	best: ^^Scope,
	best_width: ^int,
) {
	if scope == nil {
		return
	}
	if semantic_range_contains_offset(scope.range, offset) {
		width := semantic_range_width(scope.range)
		if best^ == nil || best_width^ == 0 || width < best_width^ {
			best^ = scope
			best_width^ = width
		}
	}
	for child in scope.children {
		semantic_query_scope_at_offset_walk(child, offset, best, best_width)
	}
}

semantic_fact_expr_record_at_offset :: proc(
	q: Semantic_Fact_Query,
	offset: int,
) -> (^Checker_Expr_Record, Semantic_Expression_Info_Kind, bool) {
	best := -1
	best_kind := Semantic_Expression_Info_Kind.Reference
	best_priority := 0
	best_width := 0
	for record, i in q.checker.info.expr_infos {
		if record.node == nil || !semantic_query_record_matches_file(record, q.file) {
			continue
		}
		if !semantic_range_contains_offset(record.node.range, offset) {
			continue
		}
		kind := semantic_expression_info_kind_from_node(record.node)
		priority := semantic_expression_info_priority(kind)
		width := semantic_range_width(record.node.range)
		if best < 0 || priority < best_priority || (priority == best_priority && width < best_width) {
			best = i
			best_kind = kind
			best_priority = priority
			best_width = width
		}
	}
	if best < 0 {
		return nil, {}, false
	}
	return &q.checker.info.expr_infos[best], best_kind, true
}

semantic_fact_scope_for_node :: proc(q: Semantic_Fact_Query, node: ^ast.Node) -> ^Scope {
	if node == nil {
		return nil
	}
	for use in q.checker.info.uses {
		if use.node == node {
			return use.scope
		}
	}
	return semantic_query_scope_at_offset(q.project, q.file, node.range.start)
}

semantic_expression_info_kind_from_node :: proc(node: ^ast.Node) -> Semantic_Expression_Info_Kind {
	if node == nil {
		return .Reference
	}
	#partial switch _ in node.derived {
	case ^ast.Call_Expr,
	     ^ast.Dynamic_Call_Method_Target_Expr,
	     ^ast.Ole_Call_Method_Target_Expr:
		return .Call_Result
	case ^ast.Selector_Expr,
	     ^ast.Interface_Qualified_Selector_Expr,
	     ^ast.Sql_Column_Expr:
		return .Selector
	case:
	}
	return .Reference
}

semantic_expression_info_priority :: proc(kind: Semantic_Expression_Info_Kind) -> int {
	switch kind {
	case .Call_Result:
		return 0
	case .Selector:
		return 1
	case .Reference:
		return 2
	}
	return 3
}

semantic_member_query_range :: proc(entity: ^Entity, offset: int) -> Range {
	if semantic_range_contains_offset(entity.name_range, offset) {
		return entity.name_range
	}
	if routine, ok := entity.payload.(^Entity_Routine_Payload); ok && routine != nil {
		if semantic_range_contains_offset(routine.implementation_range, offset) {
			return routine.implementation_range
		}
	}
	return entity.name_range
}

semantic_entity_is_object_member :: proc(entity: ^Entity) -> bool {
	if entity == nil || entity.owner == nil {
		return false
	}
	return entity.owner.kind == .Class || entity.owner.kind == .Interface
}

semantic_query_entity_matches_file :: proc(entity: ^Entity, file: ^Project_File) -> bool {
	return file == nil || entity.source_file == file
}

semantic_query_use_matches_file :: proc(use: Checker_Entity_Use, file: ^Project_File) -> bool {
	return file == nil || use.file == file
}

semantic_query_record_matches_file :: proc(record: Checker_Expr_Record, file: ^Project_File) -> bool {
	return file == nil || record.file == file
}

semantic_range_contains_offset :: #force_inline proc(range: Range, offset: int) -> bool {
	return range.start <= offset && offset < range.end
}

semantic_range_width :: #force_inline proc(range: Range) -> int {
	return range.end - range.start
}

semantic_query_intern_name :: proc(project: ^Project, name: string) -> string_interner.String {
	assert(project != nil)
	return checker_intern_name(project, name)
}

semantic_completion_append_entity :: proc(
	project: ^Project,
	entity: ^Entity,
	source: Semantic_Completion_Item_Source,
	prefix: string,
	seen: ^map[Semantic_Completion_Item_Key]bool,
	out: ^[dynamic]Semantic_Completion_Item,
) {
	if entity == nil || !string_interner.is_valid(entity.name) {
		return
	}
	if prefix != "" {
		name := strings.to_lower(string_interner.load(project.interner, entity.name), context.temp_allocator)
		if !strings.has_prefix(name, prefix) {
			return
		}
	}
	namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in namespaces {
		if !entity_kind_occupies(entity.kind, namespace) {
			continue
		}
		key := Semantic_Completion_Item_Key {
			name      = entity.name,
			namespace = namespace,
			source    = source,
		}
		if key in seen^ {
			continue
		}
		seen^[key] = true
		append(
			out,
			Semantic_Completion_Item {
				name      = entity.name,
				namespace = namespace,
				entity    = entity,
				source    = source,
				range     = entity.name_range,
			},
		)
	}
}
