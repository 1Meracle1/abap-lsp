package abap_frontend_semantic2

import "src:ast"

import "core:mem"
import "core:slice"
import "core:strings"

Semantic_Query :: struct {
	project:        ^Project,
	checker:        ^Checker,
	file:           ^Project_File,
	provider_index: ^External_Semantic_Index,
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
	project:        ^Project,
	checker:        ^Checker,
	file:           ^Project_File,
	provider_index: ^External_Semantic_Index,
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

Semantic_Value_Constructor_Info :: struct {
	node:      ^ast.Node,
	expr:      ^ast.Constructor_Expr,
	range:     Range,
	info:      Checker_Expr_Info,
	structure: ^Structure,
}

Semantic_Completion_Item_Source :: enum {
	Lexical_Scope,
	Builtin_Scope,
	Provider_Index,
	Selector_Member,
}

Semantic_Completion_Item :: struct {
	name:        string,
	namespace:   Namespace,
	entity:      ^Entity,
	source:      Semantic_Completion_Item_Source,
	selector_op: ast.Selector_Op,
	range:       Range,
}

Semantic_Completion_Item_Key :: struct {
	name:      string,
	namespace: Namespace,
	source:    Semantic_Completion_Item_Source,
}

Semantic_Completion_Selector_Context :: struct {
	op:             ast.Selector_Op,
	receiver_op:    ast.Selector_Op,
	base_name:      string,
	base_end:       int,
	interface_name: string,
}

semantic_query :: proc(
	project: ^Project,
	checker: ^Checker,
	file: ^Project_File = nil,
	provider_index: ^External_Semantic_Index = nil,
) -> Semantic_Query {
	assert(project != nil && checker != nil && checker.project == project)
	return Semantic_Query {
		project = project,
		checker = checker,
		file = file,
		provider_index = provider_index,
	}
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
	return Semantic_Completion_Query {
		project = q.project,
		checker = q.checker,
		file = q.file,
		provider_index = q.provider_index,
	}
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
		   !semantic_query_member_matches_file(entity, q.file) ||
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
	interned := project_intern_lower_ascii(q.project, name)
	if interned == "" {
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
	interned := project_intern_lower_ascii(q.project, name)
	if interned == "" {
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
		range := semantic_entity_use_range(use)
		if range.start >= range.end || !semantic_query_use_matches_file(use, q.file) {
			continue
		}
		if !semantic_range_contains_offset(range, offset) {
			continue
		}
		width := semantic_range_width(range)
		if best < 0 || width < best_width {
			best = i
			best_width = width
		}
	}
	return &q.checker.info.uses[best] if best >= 0 else nil
}

semantic_ref_use_at_range :: proc(q: Semantic_Ref_Query, range: Range) -> ^Checker_Entity_Use {
	for &use in q.checker.info.uses {
		if semantic_query_use_matches_file(use, q.file) &&
		   semantic_entity_use_range(use) == range {
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
) -> (
	Semantic_Expression_Info,
	bool,
) {
	record, kind, ok := semantic_fact_expr_record_at_offset(q, offset)
	if !ok {
		return {}, false
	}
	return Semantic_Expression_Info {
			node = record.node,
			kind = kind,
			scope = semantic_fact_scope_for_node(q, record.node),
			range = record.node.range if record.node != nil else Range{},
			info = record.info,
		},
		true
}

semantic_fact_operand_info_at_offset :: proc(
	q: Semantic_Fact_Query,
	offset: int,
) -> (
	Checker_Expr_Info,
	bool,
) {
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

semantic_fact_value_constructor_structure_at_range :: proc(
	q: Semantic_Fact_Query,
	request: Range,
) -> (
	Semantic_Value_Constructor_Info,
	bool,
) {
	best := -1
	best_width := 0
	for record, i in q.checker.info.expr_infos {
		if record.node == nil || !semantic_query_record_matches_file(record, q.file) {
			continue
		}
		constructor, constructor_ok := record.node.derived.(^ast.Constructor_Expr)
		if !constructor_ok || constructor.kind != .Value {
			continue
		}
		if !semantic_range_applies_to_query(request, record.node.range) {
			continue
		}
		if checker_type_structure(record.info.type) == nil {
			continue
		}
		width := semantic_range_width(record.node.range)
		if best < 0 || width < best_width {
			best = i
			best_width = width
		}
	}
	if best < 0 {
		return {}, false
	}
	record := q.checker.info.expr_infos[best]
	constructor := record.node.derived.(^ast.Constructor_Expr)
	structure := checker_type_structure(record.info.type)
	if structure == nil {
		return {}, false
	}
	return Semantic_Value_Constructor_Info {
			node = record.node,
			expr = constructor,
			range = record.node.range,
			info = record.info,
			structure = structure,
		},
		true
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
		if q.file != nil && diagnostic.file != nil && diagnostic.file != q.file {
			continue
		}
		if q.file != nil &&
		   diagnostic.file == nil &&
		   diagnostic.entity != nil &&
		   !semantic_query_entity_matches_file(diagnostic.entity, q.file) {
			continue
		}
		append(&out, diagnostic)
	}
	slice.sort_by(out[:], semantic_diagnostic_less)
	return out
}

semantic_diagnostic_less :: proc(left, right: Checker_Diagnostic) -> bool {
	left_path := ""
	if left.file != nil {
		left_path = left.file.path
	}
	right_path := ""
	if right.file != nil {
		right_path = right.file.path
	}

	return semantic_diagnostic_less_with_paths(left, left_path, right, right_path)
}

semantic_diagnostic_less_with_paths :: proc(
	left: Checker_Diagnostic,
	left_path: string,
	right: Checker_Diagnostic,
	right_path: string,
) -> bool {
	path_cmp := strings.compare(left_path, right_path)
	if path_cmp != 0 {
		return path_cmp < 0
	}
	if left.range.start != right.range.start {
		return left.range.start < right.range.start
	}
	if left.range.end != right.range.end {
		return left.range.end < right.range.end
	}
	if left.severity != right.severity {
		return int(left.severity) < int(right.severity)
	}
	if left.kind != right.kind {
		return int(left.kind) < int(right.kind)
	}
	return strings.compare(left.message, right.message) < 0
}

semantic_completion_items_at_offset :: proc(
	q: Semantic_Completion_Query,
	offset: int,
	prefix: string,
	allocator: mem.Allocator,
	source: string = "",
) -> [dynamic]Semantic_Completion_Item {
	out := make([dynamic]Semantic_Completion_Item, 0, 32, allocator)
	seen := make(map[Semantic_Completion_Item_Key]bool, 64, allocator)
	canonical_prefix := project_intern_lower_ascii(q.project, prefix)

	scope := semantic_query_scope_at_offset(q.file, offset, q.checker)
	if selector, selector_ok := semantic_completion_selector_context_at_offset(source, offset);
	   selector_ok {
		semantic_completion_append_selector_entities(
			q,
			selector,
			scope,
			canonical_prefix,
			&seen,
			&out,
		)
		return out
	}

	for current := scope; current != nil; current = current.parent {
		source :=
			Semantic_Completion_Item_Source.Builtin_Scope if current.kind == .Builtin else Semantic_Completion_Item_Source.Lexical_Scope
		semantic_completion_append_scope_entities(
			q.project,
			current,
			source,
			canonical_prefix,
			&seen,
			&out,
		)
	}
	semantic_completion_append_provider_entities(
		q.project,
		q.provider_index,
		canonical_prefix,
		&seen,
		&out,
	)
	return out
}

semantic_completion_selector_context_at_offset :: proc(
	source: string,
	offset: int,
) -> (
	Semantic_Completion_Selector_Context,
	bool,
) {
	if source == "" {
		return {}, false
	}
	prefix_start := semantic_completion_prefix_start(source, offset)
	i := semantic_completion_skip_space_backward(source, prefix_start)
	if i < 2 {
		return {}, false
	}
	if source[i - 1] == '~' {
		interface_end := i - 1
		interface_start := interface_end
		for interface_start > 0 && semantic_completion_name_char(source[interface_start - 1]) {
			interface_start -= 1
		}
		if interface_start == interface_end {
			return {}, false
		}
		op_end := semantic_completion_skip_space_backward(source, interface_start)
		if op_end < 2 {
			return {}, false
		}
		receiver_op := ast.Selector_Op{}
		if source[op_end - 2:op_end] == "=>" {
			receiver_op = .Fat_Arrow
		} else if source[op_end - 2:op_end] == "->" {
			receiver_op = .Arrow
		} else {
			return {}, false
		}
		op_start := op_end - 2
		base_end := semantic_completion_skip_space_backward(source, op_start)
		base_name_end := base_end
		base_start := base_name_end
		for base_start > 0 && semantic_completion_name_char(source[base_start - 1]) {
			base_start -= 1
		}
		if receiver_op == .Fat_Arrow && base_start == base_name_end {
			return {}, false
		}
		return Semantic_Completion_Selector_Context {
				op             = .Tilde,
				receiver_op    = receiver_op,
				base_name      = source[base_start:base_name_end],
				base_end       = base_end,
				interface_name = source[interface_start:interface_end],
			},
			true
	}
	op := ast.Selector_Op{}
	if source[i - 2:i] == "=>" {
		op = .Fat_Arrow
	} else if source[i - 2:i] == "->" {
		op = .Arrow
	} else if source[i - 1] == '-' && !semantic_completion_space_char(source[i - 2]) {
		op = .Dash
	} else {
		return {}, false
	}
	op_start := i - 2 if op != .Dash else i - 1
	base_end := semantic_completion_skip_space_backward(source, op_start)
	base_name_end := base_end
	base_start := base_name_end
	for base_start > 0 && semantic_completion_name_char(source[base_start - 1]) {
		base_start -= 1
	}
	if op == .Dash && base_start == base_name_end {
		table_base_end := semantic_completion_table_expr_base_end(source, base_end)
		if table_base_end < base_end {
			base_name_end = table_base_end
			base_start = base_name_end
			for base_start > 0 && semantic_completion_name_char(source[base_start - 1]) {
				base_start -= 1
			}
		}
	}
	if op == .Fat_Arrow && base_start == base_name_end {
		return {}, false
	}
	return Semantic_Completion_Selector_Context {
			op          = op,
			receiver_op = op,
			base_name   = source[base_start:base_name_end],
			base_end    = base_end,
		},
		true
}

semantic_completion_prefix_start :: proc(source: string, offset: int) -> int {
	start := clamp(offset, 0, len(source))
	for start > 0 && semantic_completion_name_char(source[start - 1]) {
		start -= 1
	}
	return start
}

semantic_completion_skip_space_backward :: proc(source: string, offset: int) -> int {
	i := clamp(offset, 0, len(source))
	for i > 0 {
		if semantic_completion_space_char(source[i - 1]) {
			i -= 1
			continue
		}
		break
	}
	return i
}

semantic_completion_table_expr_base_end :: proc(source: string, offset: int) -> int {
	i := semantic_completion_skip_space_backward(source, offset)
	if i == 0 || source[i - 1] != ']' {
		return offset
	}
	depth := 0
	for i > 0 {
		i -= 1
		switch source[i] {
		case ']':
			depth += 1
		case '[':
			depth -= 1
			if depth == 0 {
				return semantic_completion_skip_space_backward(source, i)
			}
		}
	}
	return offset
}

semantic_completion_space_char :: proc "contextless" (ch: u8) -> bool {
	return ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n'
}

semantic_completion_name_char :: proc "contextless" (ch: u8) -> bool {
	return ('a' <= ch && ch <= 'z') ||
	       ('A' <= ch && ch <= 'Z') ||
	       ('0' <= ch && ch <= '9') ||
	       ch == '_' ||
	       ch == '/'
}

semantic_query_scope_at_offset :: proc(
	file: ^Project_File,
	offset: int,
	checker: ^Checker = nil,
) -> ^Scope {
	if file == nil {
		return nil
	}
	best := file.root_scope
	best_width := semantic_range_width(file.root_scope.range) if file.root_scope != nil else 0
	semantic_query_scope_at_offset_walk(file.root_scope, offset, &best, &best_width)
	semantic_query_scope_at_offset_routine_implementations(checker, file, offset, &best, &best_width)
	return best
}

semantic_completion_append_selector_entities :: proc(
	q: Semantic_Completion_Query,
	selector: Semantic_Completion_Selector_Context,
	scope: ^Scope,
	prefix: string,
	seen: ^map[Semantic_Completion_Item_Key]bool,
	out: ^[dynamic]Semantic_Completion_Item,
) {
	if selector.op == .Fat_Arrow {
		owner := semantic_completion_resolve_type_owner(q, scope, selector.base_name)
		if owner == nil || (owner.kind != .Class && owner.kind != .Interface) {
			return
		}
		semantic_completion_append_object_members(q, owner, scope, prefix, seen, out, .Fat_Arrow)
		return
	}
	if selector.op == .Arrow {
		owner := semantic_completion_resolve_instance_owner(q, scope, selector)
		if owner == nil || (owner.kind != .Class && owner.kind != .Interface) {
			return
		}
		semantic_completion_append_object_members(q, owner, scope, prefix, seen, out, .Arrow)
		return
	}
	if selector.op == .Tilde {
		owner: ^Entity
		if selector.receiver_op == .Fat_Arrow {
			owner = semantic_completion_resolve_type_owner(q, scope, selector.base_name)
		} else {
			owner = semantic_completion_resolve_instance_owner(q, scope, selector)
		}
		if owner == nil || (owner.kind != .Class && owner.kind != .Interface) {
			return
		}
		iface := semantic_completion_resolve_type_owner(q, owner.scope, selector.interface_name)
		if iface == nil || iface.kind != .Interface {
			return
		}
		if q.checker != nil && !checker_type_exposes_interface(&q.checker.builtin_context, owner, iface.name) {
			return
		}
		semantic_completion_append_object_members(
			q,
			iface,
			scope,
			prefix,
			seen,
			out,
			selector.receiver_op,
		)
		return
	}
	if selector.op == .Dash {
		structure := semantic_completion_resolve_structure(q, scope, selector)
		if structure != nil {
			semantic_completion_append_structure_fields(q.project, structure, prefix, seen, out)
			return
		}
		owner := semantic_completion_resolve_instance_owner(q, scope, selector)
		if owner == nil || (owner.kind != .Class && owner.kind != .Interface) {
			return
		}
		semantic_completion_append_object_members(q, owner, scope, prefix, seen, out, .Arrow)
		return
	}
}

semantic_completion_resolve_instance_owner :: proc(
	q: Semantic_Completion_Query,
	scope: ^Scope,
	selector: Semantic_Completion_Selector_Context,
) -> ^Entity {
	if q.checker == nil {
		return nil
	}
	if info, ok := semantic_completion_operand_info_before_offset(q, selector.base_end); ok {
		target := checker_type_ref_target(&q.checker.builtin_context, info.type)
		if owner := checker_type_object_entity(target); owner != nil {
			return owner
		}
	}
	if selector.base_name == "" || scope == nil {
		return nil
	}
	interned := project_intern_lower_ascii(q.project, selector.base_name)
	if interned == "" {
		return nil
	}
	_, entity, ok := checker_lookup_declaration_from_scope(scope, .Value, interned)
	if !ok || entity == nil || entity.type == nil {
		return nil
	}
	target := checker_type_ref_target(&q.checker.builtin_context, entity.type)
	return checker_type_object_entity(target)
}

semantic_completion_resolve_structure :: proc(
	q: Semantic_Completion_Query,
	scope: ^Scope,
	selector: Semantic_Completion_Selector_Context,
) -> ^Structure {
	if q.checker == nil {
		return nil
	}
	if info, ok := semantic_completion_operand_info_before_offset(q, selector.base_end); ok {
		if structure := checker_type_structure(info.type); structure != nil {
			return structure
		}
	}
	if selector.base_name == "" || scope == nil {
		return nil
	}
	interned := project_intern_lower_ascii(q.project, selector.base_name)
	if interned == "" {
		return nil
	}
	_, entity, ok := checker_lookup_declaration_from_scope(scope, .Value, interned)
	if !ok || entity == nil || entity.type == nil {
		return nil
	}
	if row_structure := checker_type_structure(checker_type_row(&q.checker.builtin_context, entity.type)); row_structure != nil {
		return row_structure
	}
	return checker_type_structure(entity.type)
}

semantic_completion_operand_info_before_offset :: proc(
	q: Semantic_Completion_Query,
	offset: int,
) -> (Checker_Expr_Info, bool) {
	best := -1
	best_exact := false
	best_priority := 0
	best_width := 0
	probe := offset - 1
	for record, i in q.checker.info.expr_infos {
		if record.node == nil || !semantic_query_record_matches_file(record, q.file) {
			continue
		}
		range := record.node.range
		if range.start >= range.end {
			continue
		}
		exact := range.end == offset
		contains := range.start <= probe && probe < range.end
		if !exact && !contains {
			continue
		}
		kind := semantic_expression_info_kind_from_node(record.node)
		priority := semantic_expression_info_priority(kind)
		width := semantic_range_width(range)
		if best < 0 ||
		   (exact && !best_exact) ||
		   (exact == best_exact &&
		    (priority < best_priority ||
		     (priority == best_priority && width < best_width))) {
			best = i
			best_exact = exact
			best_priority = priority
			best_width = width
		}
	}
	if best < 0 {
		return {}, false
	}
	return q.checker.info.expr_infos[best].info, true
}

semantic_completion_resolve_type_owner :: proc(
	q: Semantic_Completion_Query,
	scope: ^Scope,
	name: string,
) -> ^Entity {
	interned := project_intern_lower_ascii(q.project, name)
	if interned == "" {
		return nil
	}
	if scope != nil {
		if _, entity, ok := checker_lookup_declaration_from_scope(scope, .Type, interned);
		   ok && entity != nil && (entity.kind == .Class || entity.kind == .Interface) {
			return entity
		}
	}
	if entity := semantic_completion_lookup_provider_type(q.provider_index, interned); entity != nil {
		return entity
	}
	if q.checker != nil && q.checker.info.external != nil {
		return semantic_completion_lookup_provider_type(&q.checker.info.external.index, interned)
	}
	return nil
}

semantic_completion_lookup_provider_type :: proc(
	index: ^External_Semantic_Index,
	name: string,
) -> ^Entity {
	if index == nil {
		return nil
	}
	kinds := [?]External_Candidate_Kind{.Class, .Interface, .Global_Symbol}
	for kind in kinds {
		if _, binding, ok := external_semantic_index_lookup(index, .Type, name, kind);
		   ok &&
		   binding.entity != nil &&
		   (binding.entity.kind == .Class || binding.entity.kind == .Interface) {
			return binding.entity
		}
	}
	return nil
}

semantic_completion_append_object_members :: proc(
	q: Semantic_Completion_Query,
	owner: ^Entity,
	access_scope: ^Scope,
	prefix: string,
	seen: ^map[Semantic_Completion_Item_Key]bool,
	out: ^[dynamic]Semantic_Completion_Item,
	op: ast.Selector_Op,
	depth := 0,
) {
	if owner == nil || depth > 64 {
		return
	}
	payload, ok := owner.payload.(^Entity_Object_Payload)
	if !ok || payload == nil || payload.definition_scope == nil {
		return
	}
	for member in payload.definition_scope.declarations {
		if member.kind == .Alias {
			semantic_completion_append_object_alias_member(
				q,
				owner,
				member,
				access_scope,
				prefix,
				seen,
				out,
				op,
			)
			continue
		}
		if !semantic_completion_object_member_accessible(member, access_scope, op) {
			continue
		}
		semantic_completion_append_entity(q.project, member, .Selector_Member, prefix, seen, out, op)
	}
	for interface_name in payload.implemented_interfaces {
		if interface_name == "" {
			continue
		}
		if iface := semantic_completion_resolve_type_owner(q, owner.scope, interface_name);
		   iface != nil && iface.kind == .Interface {
			if owner.kind == .Interface {
				semantic_completion_append_object_members(
					q,
					iface,
					access_scope,
					prefix,
					seen,
					out,
					op,
					depth + 1,
				)
			} else {
				semantic_completion_append_entity(q.project, iface, .Selector_Member, prefix, seen, out, op)
			}
		}
	}
	if owner.kind == .Class && payload.superclass_name != "" {
		if super := semantic_completion_resolve_type_owner(q, owner.scope, payload.superclass_name);
		   super != nil && super.kind == .Class {
			semantic_completion_append_object_members(
				q,
				super,
				access_scope,
				prefix,
				seen,
				out,
				op,
				depth + 1,
			)
		}
	}
}

semantic_completion_append_object_alias_member :: proc(
	q: Semantic_Completion_Query,
	owner: ^Entity,
	alias: ^Entity,
	access_scope: ^Scope,
	prefix: string,
	seen: ^map[Semantic_Completion_Item_Key]bool,
	out: ^[dynamic]Semantic_Completion_Item,
	op: ast.Selector_Op,
) {
	if owner == nil || alias == nil || alias.kind != .Alias {
		return
	}
	if checker_member_visibility(alias) != .Public &&
	   (access_scope == nil || !checker_member_visible_from_scope(access_scope, alias)) {
		return
	}
	payload, ok := alias.payload.(^Entity_Alias_Payload)
	if !ok || payload == nil || payload.target_interface_name == "" {
		return
	}
	iface := semantic_completion_resolve_type_owner(q, owner.scope, payload.target_interface_name)
	if iface == nil || iface.kind != .Interface {
		return
	}
	if q.checker != nil && !checker_type_exposes_interface(&q.checker.builtin_context, owner, iface.name) {
		return
	}
	target_name := payload.target_member_name
	if target_name == "" {
		target_name = alias.name
	}
	if target := semantic_completion_alias_target_member(iface, target_name, access_scope, op); target != nil {
		semantic_completion_append_named_entity(
			q.project,
			alias.name,
			target,
			.Selector_Member,
			prefix,
			seen,
			out,
			op,
			alias.name_range,
		)
	}
}

semantic_completion_alias_target_member :: proc(
	iface: ^Entity,
	name: string,
	access_scope: ^Scope,
	op: ast.Selector_Op,
) -> ^Entity {
	if iface == nil || iface.kind != .Interface || name == "" {
		return nil
	}
	namespaces := [?]Namespace{.Routine, .Value, .Type}
	for namespace in namespaces {
		member, ok := checker_lookup_object_member(iface, namespace, name)
		if !ok || !semantic_completion_object_member_accessible(member, access_scope, op) {
			continue
		}
		return member
	}
	return nil
}

semantic_completion_append_structure_fields :: proc(
	project: ^Project,
	structure: ^Structure,
	prefix: string,
	seen: ^map[Semantic_Completion_Item_Key]bool,
	out: ^[dynamic]Semantic_Completion_Item,
) {
	if structure == nil {
		return
	}
	for field in structure.fields {
		semantic_completion_append_entity(project, field, .Selector_Member, prefix, seen, out, .Dash)
	}
}

semantic_completion_object_member_accessible :: proc(
	member: ^Entity,
	access_scope: ^Scope,
	op: ast.Selector_Op,
) -> bool {
	if member == nil {
		return false
	}
	if semantic_completion_constructor_method(member) {
		return false
	}
	if op == .Fat_Arrow && !semantic_completion_static_member(member) {
		return false
	}
	if op == .Arrow && !semantic_completion_instance_member(member) {
		return false
	}
	if checker_member_visibility(member) == .Public {
		return true
	}
	return access_scope != nil && checker_member_visible_from_scope(access_scope, member)
}

semantic_completion_constructor_method :: proc(member: ^Entity) -> bool {
	if member == nil || member.kind != .Method {
		return false
	}
	return strings.equal_fold(member.name, "constructor") ||
	       strings.equal_fold(member.name, "class_constructor")
}

semantic_completion_static_member :: proc(member: ^Entity) -> bool {
	if member == nil {
		return false
	}
	if .Static in member.flags {
		return true
	}
	#partial switch member.kind {
	case .Constant, .Enum_Member, .Type_Def:
		return true
	case:
	}
	return false
}

semantic_completion_instance_member :: proc(member: ^Entity) -> bool {
	if member == nil || .Static in member.flags {
		return false
	}
	#partial switch member.kind {
	case .Variable, .Method, .Event:
		return true
	case:
	}
	return false
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
	semantic_query_scope_at_offset_consider(scope, scope.range, offset, best, best_width)
	for child in scope.children {
		semantic_query_scope_at_offset_walk(child, offset, best, best_width)
	}
}

semantic_query_scope_at_offset_routine_implementations :: proc(
	checker: ^Checker,
	file: ^Project_File,
	offset: int,
	best: ^^Scope,
	best_width: ^int,
) {
	if checker == nil {
		return
	}
	for entity in checker.info.definitions {
		if entity == nil {
			continue
		}
		payload, ok := entity.payload.(^Entity_Routine_Payload)
		if !ok ||
		   payload == nil ||
		   payload.body_scope == nil ||
		   payload.implementation_unit != file {
			continue
		}
		semantic_query_scope_at_offset_consider(
			payload.body_scope,
			payload.implementation_range,
			offset,
			best,
			best_width,
		)
	}
}

semantic_query_scope_at_offset_consider :: proc(
	scope: ^Scope,
	range: Range,
	offset: int,
	best: ^^Scope,
	best_width: ^int,
) {
	if scope == nil || !semantic_range_contains_offset(range, offset) {
		return
	}
	width := semantic_range_width(range)
	if best^ == nil || best_width^ == 0 || width < best_width^ {
		best^ = scope
		best_width^ = width
	}
}

semantic_fact_expr_record_at_offset :: proc(
	q: Semantic_Fact_Query,
	offset: int,
) -> (
	^Checker_Expr_Record,
	Semantic_Expression_Info_Kind,
	bool,
) {
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
		if best < 0 ||
		   priority < best_priority ||
		   (priority == best_priority && width < best_width) {
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
	return semantic_query_scope_at_offset(q.file, node.range.start, q.checker)
}

semantic_expression_info_kind_from_node :: proc(node: ^ast.Node) -> Semantic_Expression_Info_Kind {
	if node == nil {
		return .Reference
	}
	#partial switch _ in node.derived {
	case ^ast.Call_Expr, ^ast.Dynamic_Call_Method_Target_Expr, ^ast.Ole_Call_Method_Target_Expr:
		return .Call_Result
	case ^ast.Selector_Expr, ^ast.Interface_Qualified_Selector_Expr, ^ast.Sql_Column_Expr:
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
		if semantic_range_contains_offset(routine.implementation_name_range, offset) {
			return routine.implementation_name_range
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

semantic_query_member_matches_file :: proc(entity: ^Entity, file: ^Project_File) -> bool {
	if file == nil || entity.source_file == file {
		return true
	}
	if routine, ok := entity.payload.(^Entity_Routine_Payload); ok && routine != nil {
		return routine.implementation_unit == file
	}
	return false
}

semantic_query_use_matches_file :: proc(use: Checker_Entity_Use, file: ^Project_File) -> bool {
	return file == nil || use.file == file
}

semantic_entity_use_range :: proc(use: Checker_Entity_Use) -> Range {
	if use.range.end > use.range.start {
		return use.range
	}
	if use.node != nil {
		return use.node.range
	}
	return {}
}

semantic_query_record_matches_file :: proc(
	record: Checker_Expr_Record,
	file: ^Project_File,
) -> bool {
	return file == nil || record.file == file
}

semantic_range_contains_offset :: #force_inline proc(range: Range, offset: int) -> bool {
	return range.start <= offset && offset < range.end
}

semantic_range_applies_to_query :: proc(request, candidate: Range) -> bool {
	if request.start == request.end {
		return candidate.start <= request.start && request.start <= candidate.end
	}
	return request.start < candidate.end && candidate.start < request.end
}

semantic_range_width :: #force_inline proc(range: Range) -> int {
	return range.end - range.start
}

semantic_completion_append_entity :: proc(
	project: ^Project,
	entity: ^Entity,
	source: Semantic_Completion_Item_Source,
	prefix: string,
	seen: ^map[Semantic_Completion_Item_Key]bool,
	out: ^[dynamic]Semantic_Completion_Item,
	selector_op: ast.Selector_Op = .Dash,
) {
	if entity == nil {
		return
	}
	semantic_completion_append_named_entity(
		project,
		entity.name,
		entity,
		source,
		prefix,
		seen,
		out,
		selector_op,
		entity.name_range,
	)
}

semantic_completion_append_named_entity :: proc(
	project: ^Project,
	name: string,
	entity: ^Entity,
	source: Semantic_Completion_Item_Source,
	prefix: string,
	seen: ^map[Semantic_Completion_Item_Key]bool,
	out: ^[dynamic]Semantic_Completion_Item,
	selector_op: ast.Selector_Op = .Dash,
	range: Range = {},
) {
	if entity == nil || name == "" {
		return
	}
	if prefix != "" {
		canonical_name := project_intern_lower_ascii(project, name)
		if !strings.has_prefix(canonical_name, prefix) {
			return
		}
	}
	namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in namespaces {
		if !entity_kind_occupies(entity.kind, namespace) {
			continue
		}
		key := Semantic_Completion_Item_Key {
			name      = name,
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
				name = name,
				namespace = namespace,
				entity = entity,
				source = source,
				selector_op = selector_op,
				range = range,
			},
		)
	}
}

semantic_completion_append_scope_entities :: proc(
	project: ^Project,
	scope: ^Scope,
	source: Semantic_Completion_Item_Source,
	prefix: string,
	seen: ^map[Semantic_Completion_Item_Key]bool,
	out: ^[dynamic]Semantic_Completion_Item,
	depth := 0,
) {
	if scope == nil {
		return
	}
	assert(depth < 16)
	for entity in scope.declarations {
		semantic_completion_append_entity(project, entity, source, prefix, seen, out)
	}
	for imported in scope.imported {
		semantic_completion_append_scope_entities(
			project,
			imported,
			source,
			prefix,
			seen,
			out,
			depth + 1,
		)
	}
}

semantic_completion_append_provider_entities :: proc(
	project: ^Project,
	index: ^External_Semantic_Index,
	prefix: string,
	seen: ^map[Semantic_Completion_Item_Key]bool,
	out: ^[dynamic]Semantic_Completion_Item,
) {
	if index == nil {
		return
	}
	for _, binding in index.providers {
		semantic_completion_append_entity(
			project,
			binding.entity,
			.Provider_Index,
			prefix,
			seen,
			out,
		)
	}
}
