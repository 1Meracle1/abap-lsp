package abap_frontend_semantic_analyze

import "src:ast"
import execution "src:execution"
import "src:parser"
import "src:tokenizer"

import "core:mem"
import "core:strings"

Use_Range_Key :: struct {
	start: int,
	end:   int,
}

Use_Resolver :: struct {
	unit:       ^Source_File_Provider,
	allocator:  mem.Allocator,
	skip_nodes: map[Use_Range_Key]bool,
}

collect_source_file_uses :: proc(unit: ^Source_File_Provider, allocator: mem.Allocator) {
	if unit == nil || unit.root == nil {
		return
	}
	if unit.role == .Dependency_Interface_Source {
		return
	}
	clear(&unit.references)
	clear(&unit.include_edges)

	resolver := Use_Resolver {
		unit = unit,
		allocator = allocator,
		skip_nodes = make(map[Use_Range_Key]bool, 64, allocator),
	}
	visitor := ast.Visitor{visit = use_resolver_visit, data = &resolver}
	ast.walk(&visitor, &unit.root.node)
}

use_resolver_visit :: proc(v: ^ast.Visitor, node: ^ast.Node) -> ^ast.Visitor {
	if node == nil {
		return v
	}
	r := cast(^Use_Resolver)v.data
	if use_resolver_is_skipped(r, node.range) {
		return nil
	}
	scope_id := use_resolver_scope_at_range(r.unit, node.range)
	#partial switch n in node.derived {
	case ^ast.Ident_Expr:
		if n.name != "#" {
			use_resolver_add_reference(r, scope_id, n.name, .Value, .Identifier, n.range, node)
		}
	case ^ast.Type_Ref_Expr:
		use_resolver_collect_type_ref_expr(r, n, scope_id, node)
		return nil
	case ^ast.Selector_Expr:
		if use_resolver_collect_selector_expr(r, &n.node, scope_id) {
			return nil
		}
	case ^ast.Interface_Qualified_Selector_Expr:
		if use_resolver_collect_selector_expr(r, &n.node, scope_id) {
			return nil
		}
	case ^ast.Dynamic_Call_Method_Target_Expr:
		use_resolver_collect_dynamic_call_method_target(r, n, scope_id)
	case ^ast.Ole_Call_Method_Target_Expr:
		// OLE call targets are expression-valued; child traversal records their uses.
	case ^ast.Call_Expr:
		if use_resolver_collect_call_target(r, n.callee, scope_id, node) {
			use_resolver_skip_node(r, n.callee)
		}
	case ^ast.Sql_Column_Expr,
	     ^ast.Sql_Star_Expr:
		return nil
	case ^ast.Sql_Call_Expr:
		// SQL function names are not ABAP lexical references; arguments still are.
	case ^ast.Include_Stmt:
		use_resolver_collect_include_stmt(r, n, scope_id)
		return nil
	case ^ast.Report_Stmt:
		use_resolver_collect_report_stmt(r, n, scope_id)
	case ^ast.Function_Pool_Decl:
		if n.message_id != "" {
			use_resolver_set_message_default_class(r, n.message_id, n.range, scope_id)
		}
	case ^ast.Class_Decl:
		use_resolver_collect_class_decl_header(r, n, scope_id)
	case ^ast.Method_Decl:
		use_resolver_collect_method_decl_header(r, n, scope_id)
	case ^ast.Oop_Load_Stmt:
		if n.name != "" {
			use_resolver_add_reference(r, scope_id, n.name, .Type, .Type_Ref, n.name_range)
		}
	case ^ast.Oop_Simple_Stmt:
		use_resolver_collect_oop_simple_stmt(r, n, scope_id)
	case ^ast.Perform_Stmt:
		use_resolver_collect_perform_stmt(r, n, scope_id)
	case ^ast.Call_Stmt:
		use_resolver_collect_call_stmt(r, n, scope_id)
	case ^ast.Submit_Stmt:
		use_resolver_collect_submit_stmt(r, n, scope_id)
	case ^ast.Message_Stmt:
		use_resolver_collect_message_stmt(r, n, scope_id)
	case ^ast.Selection_Screen_Stmt:
		if n.field_name != "" {
			use_resolver_add_reference(r, scope_id, n.field_name, .Value, .Identifier, n.field_range)
		}
	case ^ast.Constructor_For_Clause_Expr:
		if n.group_source != "" {
			use_resolver_add_reference(r, scope_id, n.group_source, .Value, .Identifier, n.group_source_range)
		}
	case ^ast.Select_Stmt:
		use_resolver_mark_sql_query_sources(r, n.query)
		if n.with != nil {
			for entry in n.with.entries {
				use_resolver_mark_sql_query_sources(r, entry.query)
			}
		}
	case ^ast.Open_Cursor_Stmt:
		use_resolver_mark_sql_query_sources(r, n.query)
	}
	return v
}

use_resolver_add_reference :: proc(
	r: ^Use_Resolver,
	scope: Scope_Id,
	name: string,
	namespace: Namespace,
	kind: Reference_Kind,
	range: tokenizer.Range,
	node: ^ast.Node = nil,
	type_is_ref := false,
	type_has_path := false,
	type_first_selector := ast.Selector_Op.Dash,
	type_clause_form := ast.Data_Type_Form{},
	has_type_clause_form := false,
) -> Reference_Id {
	if r == nil || r.unit == nil || name == "" {
		return INVALID_REFERENCE_ID
	}
	id := Reference_Id(u32(len(r.unit.references)))
	if node != nil {
		node.sem.scope = semantic_scope_handle(r.unit.source_file_id, scope)
		node.sem.flags += {.Has_Scope}
	}
	append(
		&r.unit.references,
		Reference_Data {
			id = id,
			name = canonical_name(name, r.allocator),
			namespace = namespace,
			kind = kind,
			scope = scope,
			range = range,
			node = node,
			type_is_ref = type_is_ref,
			type_has_path = type_has_path,
			type_first_selector = type_first_selector,
			type_clause_form = type_clause_form,
			has_type_clause_form = has_type_clause_form,
		},
	)
	return id
}

use_resolver_collect_type_ref_expr :: proc(
	r: ^Use_Resolver,
	expr: ^ast.Type_Ref_Expr,
	scope: Scope_Id,
	node: ^ast.Node,
) {
	if expr == nil {
		return
	}
	if expr.raw_operand {
		use_resolver_collect_raw_operand_refs(r, expr.raw_decls[:], expr.raw_refs[:], scope)
		return
	}
	name := expr.base_name
	range := expr.base_range
	if name == "" {
		name = expr.name
		range = expr.range
	}
	if name == "" {
		return
	}
	if range.start >= range.end {
		range = expr.range
	}
	first_selector := ast.Selector_Op.Dash
	if len(expr.path) > 0 {
		first_selector = expr.path[0].selector
	}
	use_resolver_add_reference(
		r,
		scope,
		name,
		.Type,
		.Type_Ref,
		range,
		node,
		expr.is_ref,
		len(expr.path) > 0,
		first_selector,
	)
}

use_resolver_collect_raw_operand_refs :: proc(
	r: ^Use_Resolver,
	decls: []ast.Raw_Operand_Inline_Decl,
	refs: []ast.Raw_Operand_Ref,
	scope: Scope_Id,
) {
	_ = decls
	for ref in refs {
		if ref.name == "" {
			continue
		}
		namespace := Namespace.Value
		kind := Reference_Kind.Identifier
		if ref.type_base {
			namespace = .Type
			kind = .Static_Target
		}
		name := canonical_name(ref.name, r.allocator)
		if ref.call_like &&
		   !ref.type_base &&
		   len(ref.path) == 0 &&
		   builtin_routine_spec(name) != nil {
			use_resolver_add_reference(r, scope, name, .Routine, .Routine_Call, ref.range)
			continue
		}
		use_resolver_add_reference(r, scope, name, namespace, kind, ref.range)
	}
}

use_resolver_collect_selector_expr :: proc(
	r: ^Use_Resolver,
	expr: ^ast.Expr,
	scope: Scope_Id,
	in_type_position := false,
) -> bool {
	access, ok := use_resolver_selector_access_from_expr(r, expr, scope, in_type_position)
	if !ok {
		return false
	}
	kind := Reference_Kind.Identifier
	if access.base_namespace == .Type {
		kind = .Static_Target
	}
	use_resolver_add_reference(r, scope, access.base_name, access.base_namespace, kind, access.base_range)
	for segment in access.field_path {
		if segment.interface_qualified {
			use_resolver_add_reference(
				r,
				scope,
				segment.interface_name,
				.Type,
				.Type_Ref,
				segment.interface_range,
			)
		}
	}
	return true
}

use_resolver_selector_access_from_expr :: proc(
	r: ^Use_Resolver,
	expr: ^ast.Expr,
	scope: Scope_Id,
	in_type_position: bool,
) -> (Field_Access, bool) {
	if expr == nil {
		return {}, false
	}
	if id, ok := expr.derived_expr.(^ast.Ident_Expr); ok {
		return Field_Access {
				scope = scope,
				base_namespace = .Value,
				base_name = canonical_name(id.name, r.allocator),
				base_range = id.range,
				field_path = make([dynamic]Field_Access_Segment, 0, 2, r.allocator),
				in_type_position = in_type_position,
			},
			id.name != ""
	}
	if q, ok := expr.derived_expr.(^ast.Interface_Qualified_Selector_Expr); ok {
		return use_resolver_interface_qualified_selector_access_from_expr(r, q, scope, in_type_position)
	}
	sel, ok := expr.derived_expr.(^ast.Selector_Expr)
	if !ok {
		return {}, false
	}
	access, access_ok := use_resolver_selector_access_from_expr(r, sel.base, scope, in_type_position)
	if !access_ok {
		return {}, false
	}
	if len(access.field_path) == 0 && (sel.op == .Fat_Arrow || sel.op == .Tilde) {
		access.base_namespace = .Type
	}
	name, range, name_ok := expr_name(sel.field)
	if !name_ok {
		return {}, false
	}
	append(
		&access.field_path,
		Field_Access_Segment {
			name = canonical_name(name, r.allocator),
			range = range,
			selector = sel.op,
			deref = sel.op == .Arrow && name == "*",
		},
	)
	access.node = semantic_node_from_expr(expr)
	return access, true
}

use_resolver_interface_qualified_selector_access_from_expr :: proc(
	r: ^Use_Resolver,
	expr: ^ast.Interface_Qualified_Selector_Expr,
	scope: Scope_Id,
	in_type_position: bool,
) -> (Field_Access, bool) {
	access, access_ok := use_resolver_selector_access_from_expr(r, expr.receiver, scope, in_type_position)
	if !access_ok {
		return {}, false
	}
	if len(access.field_path) == 0 && expr.receiver_op == .Fat_Arrow {
		access.base_namespace = .Type
	}
	interface_name, interface_range, interface_ok := expr_name(expr.interface)
	member_name, member_range, member_ok := expr_name(expr.member)
	if !interface_ok || !member_ok {
		return {}, false
	}
	append(
		&access.field_path,
		Field_Access_Segment {
			name = canonical_name(member_name, r.allocator),
			range = member_range,
			selector = expr.receiver_op,
			interface_name = canonical_name(interface_name, r.allocator),
			interface_range = interface_range,
			interface_qualified = true,
		},
	)
	access.node = semantic_node_from_expr(&expr.node)
	return access, true
}

use_resolver_collect_call_target :: proc(
	r: ^Use_Resolver,
	target: ^ast.Expr,
	scope: Scope_Id,
	node: ^ast.Node = nil,
) -> bool {
	if target == nil {
		return false
	}
	if dyn, ok := target.derived_expr.(^ast.Dynamic_Call_Method_Target_Expr); ok {
		use_resolver_collect_dynamic_call_method_target(r, dyn, scope)
		return false
	}
	if _, ok := target.derived_expr.(^ast.Ole_Call_Method_Target_Expr); ok {
		return false
	}
	if raw, ok := target.derived_expr.(^ast.Type_Ref_Expr); ok && raw.raw_operand {
		use_resolver_collect_raw_operand_refs(r, raw.raw_decls[:], raw.raw_refs[:], scope)
		return true
	}
	if use_resolver_collect_call_method_selector_target(r, target, scope) {
		return true
	}
	if name, range, ok := expr_name(target); ok {
		use_resolver_add_reference(r, scope, name, .Routine, .Routine_Call, range, node)
		return true
	}
	return false
}

use_resolver_collect_call_method_selector_target :: proc(
	r: ^Use_Resolver,
	target: ^ast.Expr,
	scope: Scope_Id,
) -> bool {
	if receiver, receiver_op, interface_name, interface_range, _, _, qualified :=
		interface_qualified_method_parts(target);
	   qualified {
		if id, id_ok := receiver.derived_expr.(^ast.Ident_Expr);
		   id_ok && receiver_op == .Fat_Arrow {
			use_resolver_add_reference(r, scope, id.name, .Type, .Static_Target, id.range)
		}
		use_resolver_add_reference(r, scope, interface_name, .Type, .Type_Ref, interface_range)
		return true
	}
	sel, ok := target.derived_expr.(^ast.Selector_Expr)
	if !ok {
		return false
	}
	if id, id_ok := sel.base.derived_expr.(^ast.Ident_Expr); id_ok {
		namespace := Namespace.Value
		kind := Reference_Kind.Identifier
		if sel.op == .Fat_Arrow || sel.op == .Tilde {
			namespace = .Type
			kind = .Static_Target
		}
		use_resolver_add_reference(r, scope, id.name, namespace, kind, id.range)
		return true
	}
	if use_resolver_collect_selector_expr(r, sel.base, scope) {
		return true
	}
	return false
}

use_resolver_collect_dynamic_call_method_target :: proc(
	r: ^Use_Resolver,
	expr: ^ast.Dynamic_Call_Method_Target_Expr,
	scope: Scope_Id,
) {
	if expr == nil || expr.base == nil {
		return
	}
	if expr.base_dynamic {
		return
	}
	if access, access_ok := use_resolver_call_method_receiver_access_from_expr(r, expr.base, scope, expr.selector);
	   access_ok {
		kind := Reference_Kind.Identifier
		if access.base_namespace == .Type {
			kind = .Static_Target
		}
		use_resolver_add_reference(r, scope, access.base_name, access.base_namespace, kind, access.base_range)
		for segment in access.field_path {
			if segment.interface_qualified {
				use_resolver_add_reference(r, scope, segment.interface_name, .Type, .Type_Ref, segment.interface_range)
			}
		}
		use_resolver_skip_node(r, expr.base)
		return
	}
	if name, range, ok := expr_name(expr.base); ok {
		namespace := Namespace.Value
		kind := Reference_Kind.Identifier
		if expr.selector == .Fat_Arrow || expr.selector == .Tilde {
			namespace = .Type
			kind = .Static_Target
		}
		use_resolver_add_reference(r, scope, name, namespace, kind, range)
		use_resolver_skip_node(r, expr.base)
	}
}

use_resolver_call_method_receiver_access_from_expr :: proc(
	r: ^Use_Resolver,
	expr: ^ast.Expr,
	scope: Scope_Id,
	selector: ast.Selector_Op,
) -> (Field_Access, bool) {
	access, ok := use_resolver_selector_access_from_expr(r, expr, scope, false)
	if !ok {
		return {}, false
	}
	if len(access.field_path) == 0 && (selector == .Fat_Arrow || selector == .Tilde) {
		access.base_namespace = .Type
	}
	return access, true
}

use_resolver_collect_include_stmt :: proc(
	r: ^Use_Resolver,
	stmt: ^ast.Include_Stmt,
	scope: Scope_Id,
) {
	for include_name in stmt.names {
		if include_name.name == "" {
			continue
		}
		name := canonical_name(include_name.name, r.allocator)
		ref_id := use_resolver_add_reference(r, scope, name, .Value, .Include, include_name.range)
		_ = ref_id
		append(
			&r.unit.include_edges,
			Include_Edge {
				name = name,
				range = include_name.range,
				target = INVALID_SOURCE_FILE_ID,
				if_found = stmt.if_found,
			},
		)
	}
}

use_resolver_collect_report_stmt :: proc(
	r: ^Use_Resolver,
	stmt: ^ast.Report_Stmt,
	scope: Scope_Id,
) {
	if stmt.kind == .Report || stmt.kind == .Program {
		use_resolver_skip_node(r, stmt.name)
	}
	if stmt.has_message_id {
		use_resolver_set_message_default_class(r, stmt.message_id, stmt.message_id_range, scope)
		return
	}
	#partial switch stmt.kind {
	case .Read_Report,
	     .Insert_Report,
	     .Delete_Report:
	case:
		use_resolver_skip_node(r, stmt.name)
	}
}

use_resolver_collect_class_decl_header :: proc(
	r: ^Use_Resolver,
	stmt: ^ast.Class_Decl,
	scope: Scope_Id,
) {
	if .Implementation in stmt.flags && stmt.name != "" {
		use_resolver_add_reference(r, scope, stmt.name, .Type, .Type_Ref, stmt.header_range)
	}
	if !(.Implementation in stmt.flags) &&
	   !(.Bodyless in stmt.flags) &&
	   stmt.superclass_name != "" {
		use_resolver_add_reference(r, scope, stmt.superclass_name, .Type, .Type_Ref, stmt.superclass_range)
	}
}

use_resolver_collect_method_decl_header :: proc(
	r: ^Use_Resolver,
	stmt: ^ast.Method_Decl,
	scope: Scope_Id,
) {
	if stmt.qualifier != "" {
		use_resolver_add_reference(r, scope, stmt.qualifier, .Type, .Interface_Use, stmt.qualifier_range)
	}
	if !stmt.is_amdp {
		return
	}
	tokens := use_resolver_header_tokens(r, stmt.header_text, stmt.header_range.start)
	using_index := -1
	for token, i in tokens {
		if use_resolver_token_eq(token, "USING") {
			using_index = i
			break
		}
	}
	if using_index < 0 {
		return
	}
	for i in using_index + 1 ..< len(tokens) {
		if use_resolver_token_ident_like(tokens[i]) {
			use_resolver_add_reference(r, scope, tokens[i].text, .Type, .Type_Ref, tokens[i].range)
		}
	}
}

use_resolver_collect_oop_simple_stmt :: proc(
	r: ^Use_Resolver,
	stmt: ^ast.Oop_Simple_Stmt,
	scope: Scope_Id,
) {
	for alias in stmt.aliases {
		if alias.target_interface_name != "" {
			use_resolver_add_reference(
				r,
				scope,
				alias.target_interface_name,
				.Type,
				.Interface_Use,
				alias.target_interface_range,
			)
		}
	}
	for member in stmt.members {
		if member.qualifier != "" {
			use_resolver_add_reference(r, scope, member.qualifier, .Type, .Interface_Use, member.qualifier_range)
		}
		if member.event_handler.source_type != nil {
			use_resolver_collect_type_expr(r, member.event_handler.source_type, scope, .Type)
			use_resolver_skip_node(r, member.event_handler.source_type)
		}
		for clause in member.signatures {
			for param in clause.parameters {
				use_resolver_collect_type_clause(r, param.type_clause, scope)
			}
		}
	}
}

use_resolver_collect_type_clause :: proc(
	r: ^Use_Resolver,
	clause: ^ast.Data_Type_Clause,
	scope: Scope_Id,
) {
	if clause == nil {
		return
	}
	if clause.type_ref != nil {
		use_resolver_collect_type_expr(r, clause.type_ref, scope, .Type)
		use_resolver_skip_node(r, clause.type_ref)
	}
}

use_resolver_collect_type_expr :: proc(
	r: ^Use_Resolver,
	expr: ^ast.Expr,
	scope: Scope_Id,
	namespace: Namespace,
) {
	if expr == nil {
		return
	}
	if type_ref, ok := use_resolver_type_ref_from_expr(r, expr, namespace); ok {
		base_range := type_ref.base_range
		if base_range.start >= base_range.end {
			base_range = expr.range
		}
		use_resolver_add_reference(
			r,
			scope,
			type_ref.base_name,
			type_ref.namespace,
			.Type_Ref,
			base_range,
			semantic_node_from_expr(expr),
			type_ref.is_ref,
			len(type_ref.field_path) > 0,
			type_ref_path_selector(type_ref, 0),
		)
	}
}

use_resolver_type_ref_from_expr :: proc(
	r: ^Use_Resolver,
	expr: ^ast.Expr,
	namespace: Namespace,
	is_ref := false,
) -> (Field_Type_Ref_Data, bool) {
	if expr == nil {
		return {}, false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Type_Ref_Expr:
		return use_resolver_type_ref_from_type_ref_expr(r, n, namespace, is_ref)
	case ^ast.Ident_Expr:
		if n.name == "#" || n.name == "" {
			return {}, false
		}
		return Field_Type_Ref_Data {
				namespace = namespace,
				is_ref = is_ref,
				base_name = canonical_name(n.name, r.allocator),
				base_range = n.range,
			},
			true
	case ^ast.Selector_Expr:
		type_ref, ok := use_resolver_type_ref_from_expr(r, n.base, namespace, is_ref)
		if !ok {
			return {}, false
		}
		name, range, name_ok := expr_name(n.field)
		if !name_ok {
			return {}, false
		}
		if len(type_ref.field_path) == 0 {
			type_ref.field_path = make([dynamic]string, 0, 2, r.allocator)
			type_ref.field_ranges = make([dynamic]tokenizer.Range, 0, 2, r.allocator)
			type_ref.field_derefs = make([dynamic]bool, 0, 2, r.allocator)
			type_ref.field_selectors = make([dynamic]ast.Selector_Op, 0, 2, r.allocator)
		}
		append(&type_ref.field_path, canonical_name(name, r.allocator))
		append(&type_ref.field_ranges, range)
		append(&type_ref.field_derefs, n.op == .Arrow && name == "*")
		append(&type_ref.field_selectors, n.op)
		return type_ref, true
	}
	return {}, false
}

use_resolver_type_ref_from_type_ref_expr :: proc(
	r: ^Use_Resolver,
	expr: ^ast.Type_Ref_Expr,
	namespace: Namespace,
	is_ref: bool,
) -> (Field_Type_Ref_Data, bool) {
	base := expr.base_name
	base_range := expr.base_range
	if base == "" && expr.name != "" {
		base = expr.name
		base_range = expr.range
	}
	if base == "" || base == "#" {
		return {}, false
	}
	ns := namespace
	if len(expr.path) > 0 &&
	   (expr.path[0].selector == .Fat_Arrow || expr.path[0].selector == .Tilde) {
		ns = .Type
	}
	field_path := make([dynamic]string, 0, len(expr.path), r.allocator)
	field_ranges := make([dynamic]tokenizer.Range, 0, len(expr.path), r.allocator)
	field_derefs := make([dynamic]bool, 0, len(expr.path), r.allocator)
	field_selectors := make([dynamic]ast.Selector_Op, 0, len(expr.path), r.allocator)
	for segment in expr.path {
		append(&field_path, canonical_name(segment.name, r.allocator))
		append(&field_ranges, segment.range)
		append(&field_derefs, segment.selector == .Arrow && segment.name == "*")
		append(&field_selectors, segment.selector)
	}
	return Field_Type_Ref_Data {
			namespace = ns,
			is_ref = is_ref || expr.is_ref,
			base_name = canonical_name(base, r.allocator),
			base_range = base_range,
			field_path = field_path,
			field_ranges = field_ranges,
			field_derefs = field_derefs,
			field_selectors = field_selectors,
		},
		true
}

use_resolver_collect_perform_stmt :: proc(
	r: ^Use_Resolver,
	stmt: ^ast.Perform_Stmt,
	scope: Scope_Id,
) {
	external_program := stmt.has_program_clause && stmt.program_kind != .Omitted
	if name, range, ok := expr_name(stmt.form); ok && stmt.form_kind == .Static {
		if !external_program {
			use_resolver_add_reference(r, scope, name, .Routine, .Routine_Call, range)
		}
		use_resolver_skip_node(r, stmt.form)
	}
	if stmt.program != nil && stmt.program_kind == .Static {
		use_resolver_skip_node(r, stmt.program)
	}
}

use_resolver_collect_call_stmt :: proc(
	r: ^Use_Resolver,
	stmt: ^ast.Call_Stmt,
	scope: Scope_Id,
) {
	#partial switch stmt.kind {
	case .Direct:
		if use_resolver_collect_call_target(r, stmt.call, scope) {
			use_resolver_skip_node(r, stmt.call)
		}
	case .Function, .Customer_Function:
		if name, range, ok := use_resolver_static_call_name(stmt.target); ok {
			use_resolver_add_reference(r, scope, name, .Routine, .Routine_Call, range)
			use_resolver_skip_node(r, stmt.target)
		}
	case .Method:
		if use_resolver_collect_call_target(r, stmt.target, scope) {
			use_resolver_skip_node(r, stmt.target)
		}
	case .Transformation:
		use_resolver_skip_node(r, stmt.target)
	case:
	}
	for arg in stmt.named_args {
		use_resolver_collect_raw_operand_refs(r, arg.raw_decls[:], arg.raw_refs[:], scope)
	}
}

use_resolver_collect_submit_stmt :: proc(
	r: ^Use_Resolver,
	stmt: ^ast.Submit_Stmt,
	scope: Scope_Id,
) {
	if report_name, range, ok := use_resolver_submit_report_name(stmt); ok {
		use_resolver_add_reference(r, scope, report_name, .Value, .Routine_Call, range)
		use_resolver_skip_node(r, stmt.target)
	}
}

use_resolver_collect_message_stmt :: proc(
	r: ^Use_Resolver,
	stmt: ^ast.Message_Stmt,
	scope: Scope_Id,
) {
	if stmt.head == nil {
		return
	}
	if stmt.head.id != nil {
		if name, range, ok := expr_name(stmt.head.id); ok {
			use_resolver_add_reference(r, scope, strip_quotes(name), .Value, .Message_Class, range)
			use_resolver_skip_node(r, stmt.head.id)
		}
		return
	}
	if stmt.head.has_compact_class {
		use_resolver_add_reference(
			r,
			scope,
			stmt.head.compact_class_name,
			.Value,
			.Message_Class,
			stmt.head.compact_class_range,
		)
	}
}

use_resolver_set_message_default_class :: proc(
	r: ^Use_Resolver,
	name: string,
	range: tokenizer.Range,
	scope: Scope_Id,
) {
	class_name := canonical_name(strip_quotes(name), r.allocator)
	if class_name == "" {
		return
	}
	r.unit.message_default_class = Message_Class_Use_Data{name = class_name, range = range}
	r.unit.has_message_default_class = true
	use_resolver_add_reference(r, scope, class_name, .Value, .Message_Class, range)
}

use_resolver_static_call_name :: proc(expr: ^ast.Expr) -> (string, tokenizer.Range, bool) {
	if expr == nil {
		return "", tokenizer.Range{}, false
	}
	if name, range, ok := expr_name(expr); ok {
		return strip_quotes(name), range, name != ""
	}
	if paren, ok := expr.derived_expr.(^ast.Paren_Expr); ok {
		if name, range, name_ok := expr_name(paren.expr); name_ok {
			return strip_quotes(name), range, name != ""
		}
	}
	return "", tokenizer.Range{}, false
}

use_resolver_submit_report_name :: proc(stmt: ^ast.Submit_Stmt) -> (string, tokenizer.Range, bool) {
	if stmt == nil || stmt.target == nil {
		return "", tokenizer.Range{}, false
	}
	if stmt.target_kind == .Static {
		if ident, ok := stmt.target.derived_expr.(^ast.Ident_Expr); ok {
			return ident.name, ident.range, ident.name != ""
		}
		return "", tokenizer.Range{}, false
	}
	if paren, ok := stmt.target.derived_expr.(^ast.Paren_Expr); ok {
		if lit, lit_ok := paren.expr.derived_expr.(^ast.Literal_Expr); lit_ok {
			name := strip_quotes(lit.value)
			return name, lit.range, name != ""
		}
	}
	return "", tokenizer.Range{}, false
}

use_resolver_mark_sql_query_sources :: proc(r: ^Use_Resolver, query: ast.Select_Query_Clause) {
	if query.source_clause != nil {
		use_resolver_mark_sql_source_clause(r, query.source_clause)
	} else {
		use_resolver_mark_sql_source_expr(r, query.source)
	}
	for set_op in query.set_ops {
		use_resolver_mark_sql_query_sources(r, set_op.query)
	}
}

use_resolver_mark_sql_source_clause :: proc(r: ^Use_Resolver, clause: ^ast.Select_Source_Clause) {
	if clause == nil {
		return
	}
	use_resolver_mark_sql_source_expr(r, clause.source)
	for join in clause.joins {
		use_resolver_mark_sql_source_expr(r, join.source)
	}
}

use_resolver_mark_sql_source_expr :: proc(r: ^Use_Resolver, expr: ^ast.Expr) {
	if expr == nil {
		return
	}
	if host, ok := expr.derived_expr.(^ast.Host_Expr); ok {
		_ = host
		return
	}
	use_resolver_skip_node(r, expr)
}

use_resolver_header_tokens :: proc(r: ^Use_Resolver, text: string, base: int) -> [dynamic]Header_Token {
	result := tokenizer.tokenize(text, r.allocator)
	tokens := make([dynamic]Header_Token, 0, len(result.tokens), r.allocator)
	for tok in result.tokens {
		if tok.kind == .Eof {
			continue
		}
		append(
			&tokens,
			Header_Token {
				text = tokenizer.token_lexeme(tok, text),
				range = tokenizer.text_range(base + tok.range.start, base + tok.range.end),
				kind = tok.kind,
			},
		)
	}
	return tokens
}

use_resolver_token_eq :: proc(token: Header_Token, expected: string) -> bool {
	return strings.equal_fold(token.text, expected)
}

use_resolver_token_ident_like :: proc(token: Header_Token) -> bool {
	return token.kind == .Ident || token.kind == .Number
}

use_resolver_scope_at_range :: proc(unit: ^Source_File_Provider, range: tokenizer.Range) -> Scope_Id {
	if unit == nil {
		return INVALID_SCOPE_ID
	}
	best := unit.root_scope
	best_width := 0
	for scope_data in unit.scopes {
		if !use_range_contains(scope_data.range, range) {
			continue
		}
		width := scope_data.range.end - scope_data.range.start
		if best == unit.root_scope || width < best_width {
			best = scope_data.id
			best_width = width
		}
	}
	return best
}

use_range_contains :: proc(outer, inner: tokenizer.Range) -> bool {
	if inner.start >= inner.end {
		return range_contains_offset(outer, inner.start)
	}
	return outer.start <= inner.start && inner.end <= outer.end
}

use_resolver_skip_node :: proc(r: ^Use_Resolver, expr: ^ast.Expr) {
	if r == nil || expr == nil {
		return
	}
	r.skip_nodes[use_range_key(expr.range)] = true
}

use_resolver_is_skipped :: proc(r: ^Use_Resolver, range: tokenizer.Range) -> bool {
	if r == nil || r.skip_nodes == nil {
		return false
	}
	_, ok := r.skip_nodes[use_range_key(range)]
	return ok
}

use_range_key :: #force_inline proc(range: tokenizer.Range) -> Use_Range_Key {
	return Use_Range_Key{start = range.start, end = range.end}
}

build_scope_index :: proc(unit: ^Source_File_Provider, allocator: mem.Allocator) -> Scope_Index {
	index := Scope_Index {
		class_symbols = make(map[Class_Scope_Index_Key]Symbol_Id, len(unit.symbols), allocator),
		enclosing_classes = make(
			[dynamic]Symbol_Id,
			len(unit.scopes),
			len(unit.scopes),
			allocator,
		),
		enclosing_methods = make(
			[dynamic]Symbol_Id,
			len(unit.scopes),
			len(unit.scopes),
			allocator,
		),
		enclosing_method_scopes = make(
			[dynamic]Scope_Id,
			len(unit.scopes),
			len(unit.scopes),
			allocator,
		),
		superclasses = make(map[Symbol_Id]string, len(unit.class_inheritance), allocator),
	}
	for scope_data, i in unit.scopes {
		owner := INVALID_SYMBOL_ID
		if (scope_data.kind == .Class || scope_data.kind == .Interface) &&
		   scope_data.owner != INVALID_SYMBOL_ID {
			owner = scope_data.owner
		} else {
			parent_index := scope_id_index(scope_data.parent)
			if parent_index >= 0 && parent_index < i {
				owner = index.enclosing_classes[parent_index]
			}
		}
		index.enclosing_classes[i] = owner

		method := INVALID_SYMBOL_ID
		method_scope := INVALID_SCOPE_ID
		if scope_data.kind == .Method && scope_data.owner != INVALID_SYMBOL_ID {
			method = scope_data.owner
			method_scope = scope_data.id
		} else {
			parent_index := scope_id_index(scope_data.parent)
			if parent_index >= 0 && parent_index < i {
				method = index.enclosing_methods[parent_index]
				method_scope = index.enclosing_method_scopes[parent_index]
			}
		}
		index.enclosing_methods[i] = method
		index.enclosing_method_scopes[i] = method_scope
	}
	for inheritance in unit.class_inheritance {
		index.superclasses[inheritance.class_symbol] = inheritance.superclass_name
	}
	for owner in unit.symbols {
		if !(owner.kind == .Class || owner.kind == .Interface) {
			continue
		}
		scope_id := class_definition_scope(unit, owner.id)
		scope_data := scope(unit, scope_id)
		if scope_data == nil {
			continue
		}
		for symbol_id in scope_data.declarations {
			member := symbol(unit, symbol_id)
			if member == nil {
				continue
			}
			namespaces := [?]Namespace{.Value, .Type, .Routine}
			for namespace in namespaces {
				if symbol_kind_occupies(member.kind, namespace) {
					index.class_symbols[Class_Scope_Index_Key {
						class_symbol = owner.id,
						namespace = namespace,
						name = member.name,
					}] = member.id
				}
			}
		}
	}
	return index
}

analyze_unit :: proc(
	source_file_id: Source_File_Id,
	uri, source: string,
	parsed: parser.Parsed_File,
	pool: ^execution.Pool,
	allocator: mem.Allocator,
) -> Source_File_Provider {
	unit := collect_source_file(source_file_id, uri, source, parsed, allocator)
	resolve_unit_locally(&unit, allocator)
	units := make([dynamic]Source_File_Provider, 0, 1, allocator)
	append(&units, unit)
	project := project_analysis_from_source_files(units, allocator)
	finish_project_analysis(&project, pool, {}, allocator)
	return project.providers.source_files[0]
}

resolve_unit_locally :: proc(unit: ^Source_File_Provider, allocator: mem.Allocator) {
	index := build_scope_index(unit, allocator)
	unit.scope_index = index
	resolve_local_effective_method_signatures(unit)
	expand_local_structure_includes(unit, allocator)
	refresh_unit_type_ids(unit)
	resolve_unit_with_index(unit, &unit.scope_index)
}

resolve_unit_with_index :: proc(unit: ^Source_File_Provider, index: ^Scope_Index) {
	for i in 0 ..< len(unit.references) {
		ref := &unit.references[i]
		if resolution, ok := resolve_reference(unit, index, ref^); ok {
			set_reference_resolution(unit, ref, resolution)
		}
	}
}

set_reference_resolution :: proc(
	unit: ^Source_File_Provider,
	ref: ^Reference_Data,
	resolution: Resolution,
) {
	assert(reference_resolution_allowed(unit, ref^, resolution))
	ref.resolution = resolution
	ref.has_resolution = true
	add_entity_use(unit, ref.node, ref.scope, ref.id, ref.resolution, ref.has_resolution)
}

set_project_reference_resolution :: proc(
	units: []Source_File_Provider,
	source_file_index: int,
	ref: ^Reference_Data,
	resolution: Resolution,
) {
	assert(project_reference_resolution_allowed(units, ref^, resolution))
	ref.resolution = resolution
	ref.has_resolution = true
	if source_file_index >= 0 && source_file_index < len(units) {
		add_entity_use(&units[source_file_index], ref.node, ref.scope, ref.id, ref.resolution, ref.has_resolution)
	}
}

reference_resolution_allowed :: proc(
	unit: ^Source_File_Provider,
	ref: Reference_Data,
	resolution: Resolution,
) -> bool {
	if resolution.kind != .Symbol {
		return true
	}
	if resolution.symbol.unit != unit.source_file_id {
		return true
	}
	s := symbol(unit, resolution.symbol.symbol)
	if s == nil {
		return false
	}
	return reference_symbol_kind_allowed(ref, s.kind)
}

project_reference_resolution_allowed :: proc(
	units: []Source_File_Provider,
	ref: Reference_Data,
	resolution: Resolution,
) -> bool {
	if resolution.kind != .Symbol {
		return true
	}
	source_file_index := source_file_id_index(resolution.symbol.unit)
	if source_file_index < 0 || source_file_index >= len(units) {
		return false
	}
	s := symbol(&units[source_file_index], resolution.symbol.symbol)
	if s == nil {
		return false
	}
	return reference_symbol_kind_allowed(ref, s.kind)
}

reference_symbol_kind_allowed :: proc(ref: Reference_Data, kind: Symbol_Kind) -> bool {
	if ref.namespace == .Value &&
	   ref.kind == .Identifier &&
	   (ref.name == "me" || ref.name == "super") &&
	   (kind == .Class || kind == .Interface) {
		return true
	}
	namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in namespaces {
		if symbol_kind_occupies(kind, namespace) &&
		   reference_namespace_allowed(ref.kind, ref.namespace, namespace) {
			return true
		}
	}
	return false
}

resolve_reference :: proc(
	unit: ^Source_File_Provider,
	index: ^Scope_Index,
	ref: Reference_Data,
) -> (
	Resolution,
	bool,
) {
	if symbol_id, ok := lookup_reference_scope_chain(
		unit,
		index,
		ref.scope,
		ref.namespace,
		ref.kind,
		ref.name,
	); ok {
		if resolution, effective_ok := resolve_cached_local_method_parameter(unit, ref, symbol_id);
		   effective_ok {
			return resolution, true
		}
		return resolution_for_symbol(unit, symbol_id), true
	}
	if resolution, ok := resolve_cached_local_method_parameter(unit, ref, INVALID_SYMBOL_ID); ok {
		return resolution, true
	}
	if symbol_id, ok := resolve_current_class_member(
		unit,
		index,
		ref.scope,
		ref.namespace,
		ref.name,
	); ok {
		return symbol_resolution(unit, symbol_id), true
	}
	if symbol_id, ok := resolve_current_class_alias(
		unit,
		index,
		ref.scope,
		ref.namespace,
		ref.name,
	); ok {
		return symbol_resolution(unit, symbol_id), true
	}
	if symbol_id, ok := resolve_inherited_class_member(
		unit,
		index,
		ref.scope,
		ref.namespace,
		ref.name,
	); ok {
		return symbol_resolution(unit, symbol_id), true
	}
	if ref.namespace == .Value && ref.name == "super" {
		if symbol_id, ok := resolve_super_reference(unit, index, ref.scope); ok {
			return symbol_resolution(unit, symbol_id), true
		}
	}
	if ref.namespace == .Type && is_builtin_type_name(ref.name) {
		return Resolution{kind = .Builtin_Type}, true
	}
	if ref.namespace == .Routine && builtin_routine_spec(ref.name) != nil {
		return Resolution{kind = .Builtin_Routine}, true
	}
	if entity, ok := builtin_entity_handle(ref.namespace, ref.name); ok {
		return Resolution{kind = .Provider_Entity, entity = entity}, true
	}
	if ref.namespace == .Value &&
	   ref.kind == .Identifier &&
	   ref.name == "table_line" &&
	   innermost_loop_allows_internal_table_line_selector(unit, ref.scope) {
		return Resolution{kind = .Internal_Table_Line}, true
	}
	return Resolution{}, false
}

lookup_scope_chain :: proc(
	unit: ^Source_File_Provider,
	_: ^Scope_Index,
	start_scope: Scope_Id,
	namespace: Namespace,
	name: string,
) -> (
	Symbol_Id,
	bool,
) {
	current := start_scope
	for current != INVALID_SCOPE_ID {
		scope_idx := scope_id_index(current)
		if scope_idx >= 0 && scope_idx < len(unit.scopes) {
			if symbol_id, ok := scope_lookup_declaration(unit, current, namespace, name); ok {
				return symbol_id, true
			}
		}
		s := scope(unit, current)
		if s == nil {
			break
		}
		current = s.parent
	}
	return INVALID_SYMBOL_ID, false
}

lookup_reference_scope_chain :: proc(
	unit: ^Source_File_Provider,
	index: ^Scope_Index,
	scope: Scope_Id,
	namespace: Namespace,
	kind: Reference_Kind,
	name: string,
) -> (
	Symbol_Id,
	bool,
) {
	if symbol_id, ok := lookup_scope_chain(unit, index, scope, namespace, name); ok {
		return symbol_id, true
	}
	if kind == .Type_Ref && namespace == .Value {
		return lookup_scope_chain(unit, index, scope, .Type, name)
	}
	return INVALID_SYMBOL_ID, false
}

resolution_for_symbol :: proc(unit: ^Source_File_Provider, symbol_id: Symbol_Id) -> Resolution {
	s := symbol(unit, symbol_id)
	if s != nil {
		if s.kind == .Builtin_Type {
			return Resolution{kind = .Builtin_Type}
		}
		if s.kind == .Builtin_Routine {
			return Resolution{kind = .Builtin_Routine}
		}
	}
	return symbol_resolution(unit, symbol_id)
}

symbol_resolution :: #force_inline proc(unit: ^Source_File_Provider, symbol_id: Symbol_Id) -> Resolution {
	return Resolution {
		kind = .Symbol,
		symbol = Symbol_Link{unit = unit.source_file_id, symbol = symbol_id},
	}
}

enclosing_class_owner_unit :: proc(unit: ^Source_File_Provider, scope_id: Scope_Id) -> (Symbol_Id, bool) {
	scope_index := scope_id_index(scope_id)
	if scope_index >= 0 && scope_index < len(unit.scope_index.enclosing_classes) {
		owner := unit.scope_index.enclosing_classes[scope_index]
		return owner, owner != INVALID_SYMBOL_ID
	}
	current := scope_id
	for current != INVALID_SCOPE_ID {
		s := scope(unit, current)
		if s == nil {
			break
		}
		if (s.kind == .Class || s.kind == .Interface) && s.owner != INVALID_SYMBOL_ID {
			return s.owner, true
		}
		current = s.parent
	}
	return INVALID_SYMBOL_ID, false
}

enclosing_method_scope :: proc(unit: ^Source_File_Provider, scope_id: Scope_Id) -> (Scope_Id, Symbol_Id, bool) {
	scope_index := scope_id_index(scope_id)
	if scope_index >= 0 && scope_index < len(unit.scope_index.enclosing_methods) {
		method := unit.scope_index.enclosing_methods[scope_index]
		method_scope := unit.scope_index.enclosing_method_scopes[scope_index]
		return method_scope, method, method != INVALID_SYMBOL_ID
	}
	current := scope_id
	for current != INVALID_SCOPE_ID {
		s := scope(unit, current)
		if s == nil {
			break
		}
		if s.kind == .Method && s.owner != INVALID_SYMBOL_ID {
			return current, s.owner, true
		}
		current = s.parent
	}
	return INVALID_SCOPE_ID, INVALID_SYMBOL_ID, false
}

resolve_local_effective_method_signatures :: proc(unit: ^Source_File_Provider) {
	for &method in unit.symbols {
		if method.kind != .Method {
			continue
		}
		info := entity_decl_info(unit, method.id)
		if info == nil || info.body_scope == INVALID_SCOPE_ID {
			continue
		}
		info.effective_signature = Symbol_Link{unit = INVALID_SOURCE_FILE_ID, symbol = INVALID_SYMBOL_ID}
		class_symbol, class_ok := enclosing_class_owner_unit(unit, method.scope)
		if !class_ok {
			continue
		}
		if member_symbol, member_ok := class_definition_member_canonical(
			unit,
			class_symbol,
			.Routine,
			method.name,
		); member_ok {
			info.effective_signature = Symbol_Link{unit = unit.source_file_id, symbol = member_symbol}
		}
	}
}

enclosing_instance_method_class_owner_unit :: proc(
	unit: ^Source_File_Provider,
	scope_id: Scope_Id,
) -> (
	Symbol_Id,
	bool,
) {
	current := scope_id
	for current != INVALID_SCOPE_ID {
		s := scope(unit, current)
		if s == nil {
			break
		}
		if s.kind == .Method && s.owner != INVALID_SYMBOL_ID {
			class_symbol, class_ok := enclosing_class_owner_unit(unit, current)
			method := symbol(unit, s.owner)
			if !class_ok || method == nil {
				return INVALID_SYMBOL_ID, false
			}
			member, _ := class_definition_member_canonical(unit, class_symbol, .Routine, method.name)
			info := entity_decl_info(unit, member)
			return class_symbol, info == nil || !(.Is_Static in info.flags)
		}
		current = s.parent
	}
	return INVALID_SYMBOL_ID, false
}

resolve_current_class_member :: proc(
	unit: ^Source_File_Provider,
	index: ^Scope_Index,
	scope_id: Scope_Id,
	namespace: Namespace,
	name: string,
) -> (
	Symbol_Id,
	bool,
) {
	class_symbol, ok := enclosing_class_owner_unit(unit, scope_id)
	if !ok {
		return INVALID_SYMBOL_ID, false
	}
	return class_scope_symbol(index, class_symbol, namespace, name)
}

class_scope_symbol :: proc(
	index: ^Scope_Index,
	class_symbol: Symbol_Id,
	namespace: Namespace,
	name: string,
) -> (
	Symbol_Id,
	bool,
) {
	if symbol_id, ok :=
		   index.class_symbols[Class_Scope_Index_Key{class_symbol = class_symbol, namespace = namespace, name = name}];
	   ok {
		return symbol_id, true
	}
	return INVALID_SYMBOL_ID, false
}

resolve_current_class_alias :: proc(
	unit: ^Source_File_Provider,
	index: ^Scope_Index,
	scope_id: Scope_Id,
	namespace: Namespace,
	name: string,
) -> (
	Symbol_Id,
	bool,
) {
	class_symbol, ok := enclosing_class_owner_unit(unit, scope_id)
	if !ok {
		return INVALID_SYMBOL_ID, false
	}
	return resolve_class_alias(unit, index, scope_id, class_symbol, namespace, name)
}

resolve_class_alias :: proc(
	unit: ^Source_File_Provider,
	index: ^Scope_Index,
	scope_id: Scope_Id,
	class_symbol: Symbol_Id,
	namespace: Namespace,
	name: string,
) -> (
	Symbol_Id,
	bool,
) {
	for alias in unit.member_aliases {
		if alias.owner_symbol != class_symbol || alias.alias_name != name {
			continue
		}
		interface_symbol, interface_ok := lookup_scope_chain(
			unit,
			index,
			scope_id,
			.Type,
			alias.target_interface_name,
		)
		if !interface_ok {
			continue
		}
		member_name := alias.target_member_name
		if member_name == "" {
			member_name = name
		}
		if symbol_id, member_ok := class_scope_symbol(
			index,
			interface_symbol,
			namespace,
			member_name,
		); member_ok {
			return symbol_id, true
		}
	}
	return INVALID_SYMBOL_ID, false
}

resolve_inherited_class_member :: proc(
	unit: ^Source_File_Provider,
	index: ^Scope_Index,
	scope_id: Scope_Id,
	namespace: Namespace,
	name: string,
) -> (
	Symbol_Id,
	bool,
) {
	current_class, ok := enclosing_class_owner_unit(unit, scope_id)
	if !ok {
		return INVALID_SYMBOL_ID, false
	}
	for _ in 0 ..= len(unit.class_inheritance) {
		super_name, has_super := class_superclass_name(unit, current_class)
		if !has_super {
			return INVALID_SYMBOL_ID, false
		}
		super_symbol, super_ok := lookup_scope_chain(unit, index, scope_id, .Type, super_name)
		if !super_ok {
			return INVALID_SYMBOL_ID, false
		}
		if found, found_ok := class_scope_symbol(index, super_symbol, namespace, name); found_ok {
			return found, true
		}
		if found, found_ok := resolve_class_alias(
			unit,
			index,
			scope_id,
			super_symbol,
			namespace,
			name,
		); found_ok {
			return found, true
		}
		current_class = super_symbol
	}
	return INVALID_SYMBOL_ID, false
}

resolve_super_reference :: proc(
	unit: ^Source_File_Provider,
	index: ^Scope_Index,
	scope_id: Scope_Id,
) -> (
	Symbol_Id,
	bool,
) {
	class_symbol, ok := enclosing_instance_method_class_owner_unit(unit, scope_id)
	if !ok {
		return INVALID_SYMBOL_ID, false
	}
	super_name, has_super := class_superclass_name(unit, class_symbol)
	if !has_super {
		return INVALID_SYMBOL_ID, false
	}
	return lookup_scope_chain(unit, index, scope_id, .Type, super_name)
}

class_superclass_name :: proc(unit: ^Source_File_Provider, class_symbol: Symbol_Id) -> (string, bool) {
	if unit.scope_index.superclasses != nil {
		if name, ok := unit.scope_index.superclasses[class_symbol]; ok {
			return name, name != ""
		}
	}
	for inheritance in unit.class_inheritance {
		if inheritance.class_symbol == class_symbol {
			return inheritance.superclass_name, inheritance.superclass_name != ""
		}
	}
	return "", false
}

innermost_loop_allows_internal_table_line_selector :: proc(
	unit: ^Source_File_Provider,
	scope_id: Scope_Id,
) -> bool {
	current := scope_id
	for current != INVALID_SCOPE_ID {
		s := scope(unit, current)
		if s == nil {
			return false
		}
		if s.kind == .Loop_Block {
			return s.allows_internal_table_line_selector
		}
		current = s.parent
	}
	return false
}

Root_Symbol_Entry :: struct {
	unit:               Source_File_Id,
	symbol:             Symbol_Id,
	namespace:          Namespace,
	name:               string,
	visible_by_default: bool,
}

Project_Root_Lookup :: struct {
	global:          map[Root_Name_Key]Symbol_Link,
	summary_global:  map[Root_Name_Key]Entity_Handle,
	provided_names:  map[string]bool,
}

Project_Class_Member_Key :: struct {
	class_unit:   Source_File_Id,
	class_symbol: Symbol_Id,
	namespace:    Namespace,
	name:         string,
}

Project_Class_Member_Entry :: struct {
	unit:   Source_File_Id,
	symbol: Symbol_Id,
}

derive_event_handler_signature_parameter_types :: proc(
	units: []Source_File_Provider,
	member_source_file_index: int,
	member_handle: Symbol_Link,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [][dynamic]Source_File_Id,
) -> bool {
	if member_source_file_index < 0 ||
	   member_source_file_index >= len(units) {
		return false
	}
	member_info := entity_decl_info(&units[member_source_file_index], member_handle.symbol)
	if member_info == nil ||
	   member_info.event_name == "" ||
	   member_info.event_source_type.base_name == "" {
		return false
	}
	source_handle, source_ok := resolve_type_ref_handle_project(
		units,
		member_source_file_index,
		member_info.event_source_type,
		roots,
		visible[member_source_file_index],
	)
	if !source_ok {
		return false
	}
	event_member, _ := event_member_for_handler_source(
		units,
		source_handle,
		member_info.event_name,
		class_entries,
	)
	if event_member.symbol == INVALID_SYMBOL_ID {
		return false
	}
	event_info := entity_decl_info(&units[source_file_id_index(event_member.unit)], event_member.symbol)
	if event_info == nil {
		return false
	}
	changed := false
	for &param in member_info.signature_parameters {
		event_param := class_member_parameter(event_info, param.name)
		if .Has_Event_Derived_Type in param.flags {
			if event_param != nil &&
			   .Has_Declared_Type in event_param.flags &&
			   event_derived_parameter_matches(param, event_param^) {
				continue
			}
			clear_event_derived_signature_parameter(&units[member_source_file_index], &param)
			changed = true
		}
		if .Has_Declared_Type in param.flags {
			continue
		}
		if event_param == nil || !(.Has_Declared_Type in event_param.flags) {
			continue
		}
		if decl_param := entity_signature_parameter(&units[member_source_file_index], member_handle.symbol, param.name);
		   decl_param != nil {
			decl_param.declared_type = event_param.declared_type
			decl_param.type_clause_display = event_param.type_clause_display
			decl_param.type_clause_form = event_param.type_clause_form
			decl_param.has_type_clause_form = event_param.has_type_clause_form
			decl_param.type_clause_table_has_of = event_param.type_clause_table_has_of
			decl_param.type_id = UNKNOWN_TYPE_ID
			decl_param.flags += {.Has_Declared_Type, .Has_Event_Derived_Type}
			update_parameter_symbol_from_signature(&units[member_source_file_index], decl_param.symbol, event_param^)
			changed = true
		}
	}
	return changed
}

event_derived_parameter_matches :: proc(
	param: Decl_Signature_Parameter_Data,
	event_param: Decl_Signature_Parameter_Data,
) -> bool {
	return field_type_refs_equal(param.declared_type, event_param.declared_type) &&
	       param.type_clause_display == event_param.type_clause_display &&
	       param.type_clause_form == event_param.type_clause_form &&
	       param.has_type_clause_form == event_param.has_type_clause_form &&
	       param.type_clause_table_has_of == event_param.type_clause_table_has_of
}

clear_event_derived_signature_parameter :: proc(
	unit: ^Source_File_Provider,
	param: ^Decl_Signature_Parameter_Data,
) {
	param.declared_type = {}
	param.type_clause_display = ""
	param.type_clause_form = {}
	param.has_type_clause_form = false
	param.type_clause_table_has_of = false
	param.type_id = UNKNOWN_TYPE_ID
	param.flags -= {.Has_Declared_Type, .Has_Event_Derived_Type}
	clear_event_derived_parameter_symbol(unit, param.symbol)
}

clear_event_derived_parameter_symbol :: proc(unit: ^Source_File_Provider, symbol_id: Symbol_Id) -> bool {
	info := entity_decl_info(unit, symbol_id)
	if info == nil || !(.Has_Event_Derived_Type in info.flags) {
		return false
	}
	item := symbol(unit, symbol_id)
	if item != nil {
		symbol_clear_type_shape(item)
	}
	info.flags -= {.Has_Declared_Type, .Has_Event_Derived_Type}
	return true
}

update_parameter_symbol_from_signature :: proc(
	unit: ^Source_File_Provider,
	symbol_id: Symbol_Id,
	param: Decl_Signature_Parameter_Data,
) {
	item := symbol(unit, symbol_id)
	if item == nil {
		return
	}
	symbol_set_declared_type(
		item,
		param.declared_type,
		param.type_clause_display,
		param.type_clause_form,
		param.has_type_clause_form,
		param.type_clause_table_has_of,
	)
	symbol_refresh_type_id(unit, item)
	if info := entity_decl_info(unit, symbol_id); info != nil {
		info.flags += {.Has_Declared_Type, .Has_Event_Derived_Type}
	}
}

event_member_for_handler_source :: proc(
	units: []Source_File_Provider,
	source_handle: Symbol_Link,
	event_name: string,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
) -> (
	Symbol_Link,
	int,
) {
	event_handle, event_ok := class_member_symbol_by_handle(
		units,
		source_handle,
		.Routine,
		event_name,
		class_entries,
		false,
	)
	if !event_ok {
		return Symbol_Link{unit = INVALID_SOURCE_FILE_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	event_source_file_index := source_file_id_index(event_handle.unit)
	if event_source_file_index < 0 || event_source_file_index >= len(units) {
		return Symbol_Link{unit = INVALID_SOURCE_FILE_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	event_symbol := symbol(&units[event_source_file_index], event_handle.symbol)
	if event_symbol == nil {
		return Symbol_Link{unit = INVALID_SOURCE_FILE_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	event_info := entity_decl_info(&units[event_source_file_index], event_symbol.id)
	if event_info == nil || event_info.member_kind != .Event {
		return Symbol_Link{unit = INVALID_SOURCE_FILE_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	return event_handle, event_source_file_index
}

class_member_parameter :: proc(
	info: ^Decl_Info_Data,
	name: string,
) -> ^Decl_Signature_Parameter_Data {
	for &param in info.signature_parameters {
		if param.name == name {
			return &param
		}
	}
	return nil
}

method_signature_member_for_scope :: proc(
	units: []Source_File_Provider,
	source_file_index: int,
	scope_id: Scope_Id,
	method_name: string,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [dynamic]Source_File_Id,
	predecessors: [dynamic]Source_File_Id,
) -> (Symbol_Link, int) {
	if interface_name, member_name, qualified := qualified_method_parts(method_name); qualified {
		if member, member_source_file_index := exposed_interface_member_for_scope(
			units,
			source_file_index,
			scope_id,
			interface_name,
			member_name,
			roots,
			visible,
		); member.symbol != INVALID_SYMBOL_ID {
			return member, member_source_file_index
		}
	}
	member_handle, ok := resolve_visible_class_definition_member(
		units,
		source_file_index,
		scope_id,
		.Routine,
		method_name,
		roots,
		class_entries,
		visible,
		predecessors,
	)
	if !ok {
		return Symbol_Link{unit = INVALID_SOURCE_FILE_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	member_source_file_index := source_file_id_index(member_handle.unit)
	if member_source_file_index < 0 || member_source_file_index >= len(units) {
		return Symbol_Link{unit = INVALID_SOURCE_FILE_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	member_unit := &units[member_source_file_index]
	member_symbol := symbol(member_unit, member_handle.symbol)
	if member_symbol == nil {
		return Symbol_Link{unit = INVALID_SOURCE_FILE_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	class_symbol, class_ok := enclosing_class_owner_unit(member_unit, member_symbol.scope)
	if !class_ok {
		return Symbol_Link{unit = INVALID_SOURCE_FILE_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	member_info := entity_decl_info(member_unit, member_handle.symbol)
	if member_info == nil ||
	   len(member_info.signature_parameters) > 0 ||
	   !(.Is_Redefinition in member_info.flags) {
		return member_handle, member_source_file_index
	}
	if inherited, inherited_source_file_index := inherited_project_class_member(
		units,
		Symbol_Link{unit = member_handle.unit, symbol = class_symbol},
		method_name,
		roots,
		class_entries,
		visible,
	); inherited.symbol != INVALID_SYMBOL_ID {
		return inherited, inherited_source_file_index
	}
	return member_handle, member_source_file_index
}

exposed_interface_member_for_scope :: proc(
	units: []Source_File_Provider,
	source_file_index: int,
	scope_id: Scope_Id,
	interface_name, member_name: string,
	roots: ^Project_Root_Lookup,
	visible: [dynamic]Source_File_Id,
) -> (Symbol_Link, int) {
	class_symbol, class_ok := enclosing_class_owner_unit(&units[source_file_index], scope_id)
	if !class_ok {
		return Symbol_Link{unit = INVALID_SOURCE_FILE_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	handle, handle_ok := exposed_interface_handle(
		units,
		Symbol_Link{unit = units[source_file_index].source_file_id, symbol = class_symbol},
		interface_name,
		roots,
		visible,
		0,
	)
	if !handle_ok {
		return Symbol_Link{unit = INVALID_SOURCE_FILE_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	interface_source_file_index := source_file_id_index(handle.unit)
	if interface_source_file_index < 0 || interface_source_file_index >= len(units) {
		return Symbol_Link{unit = INVALID_SOURCE_FILE_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	member := unit_class_member_symbol_canonical(&units[interface_source_file_index], handle.symbol, member_name)
	if member == nil {
		return Symbol_Link{unit = INVALID_SOURCE_FILE_ID, symbol = INVALID_SYMBOL_ID}, -1
	}
	return Symbol_Link{unit = handle.unit, symbol = member.id}, interface_source_file_index
}

exposed_interface_handle :: proc(
	units: []Source_File_Provider,
	owner: Symbol_Link,
	interface_name: string,
	roots: ^Project_Root_Lookup,
	visible: [dynamic]Source_File_Id,
	depth: int,
) -> (
	Symbol_Link,
	bool,
) {
	if depth > len(units) + 8 {
		return {}, false
	}
	source_file_index := source_file_id_index(owner.unit)
	if source_file_index < 0 || source_file_index >= len(units) {
		return {}, false
	}
	unit := &units[source_file_index]
	for implemented in unit.implemented_interfaces {
		if implemented.owner_symbol != owner.symbol {
			continue
		}
		interface_handle, ok := resolve_type_name_in_project(
			units,
			source_file_index,
			implemented.interface_name,
			roots,
			visible,
		)
		if !ok {
			continue
		}
		if implemented.interface_name == interface_name {
			return interface_handle, true
		}
		if found, found_ok := exposed_interface_handle(
			units,
			interface_handle,
			interface_name,
			roots,
			visible,
			depth + 1,
		); found_ok {
			return found, true
		}
	}
	if owner_symbol := symbol(unit, owner.symbol);
	   owner_symbol != nil && owner_symbol.kind == .Class {
		if superclass, ok := direct_superclass_handle(units, owner, roots, visible); ok {
			return exposed_interface_handle(
				units,
				superclass,
				interface_name,
				roots,
				visible,
				depth + 1,
			)
		}
	}
	return {}, false
}
inherited_project_class_member :: proc(
	units: []Source_File_Provider,
	class_handle: Symbol_Link,
	name: string,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [dynamic]Source_File_Id,
) -> (Symbol_Link, int) {
	current := class_handle
	fallback := Symbol_Link{unit = INVALID_SOURCE_FILE_ID, symbol = INVALID_SYMBOL_ID}
	fallback_index := -1
	for _ in 0 ..< len(units) + 8 {
		next, ok := direct_superclass_handle(units, current, roots, visible)
		if !ok {
			return fallback, fallback_index
		}
		if member_handle, member_ok := class_member_symbol_by_handle(
			units,
			next,
			.Routine,
			name,
			class_entries,
			true,
		); member_ok {
			member_source_file_index := source_file_id_index(member_handle.unit)
			if member_source_file_index >= 0 && member_source_file_index < len(units) {
				member_unit := &units[member_source_file_index]
				if s := symbol(member_unit, member_handle.symbol); s != nil {
					if class_symbol, class_ok := enclosing_class_owner_unit(member_unit, s.scope);
					   class_ok {
						if member := unit_class_member_symbol_canonical(member_unit, class_symbol, s.name);
						   member != nil {
							info := entity_decl_info(member_unit, member.id)
							if fallback.symbol == INVALID_SYMBOL_ID {
								fallback = Symbol_Link{unit = member_handle.unit, symbol = member.id}
								fallback_index = member_source_file_index
							}
							if info != nil &&
							   len(info.signature_parameters) == 0 &&
							   .Is_Redefinition in info.flags {
								current = Symbol_Link{unit = member_handle.unit, symbol = class_symbol}
								continue
							}
							return Symbol_Link{unit = member_handle.unit, symbol = member.id}, member_source_file_index
						}
					}
				}
			}
		}
		current = next
	}
	return fallback, fallback_index
}

resolve_project_reference :: proc(
	units: []Source_File_Provider,
	source_file_index: int,
	ref: Reference_Data,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [dynamic]Source_File_Id,
	predecessors: [dynamic]Source_File_Id,
) -> (
	Resolution,
	bool,
) {
	if ref.namespace == .Value && ref.name == "super" {
		if handle, ok := resolve_project_super(units, source_file_index, ref.scope, roots, visible); ok {
			return Resolution{kind = .Symbol, symbol = handle}, true
		}
	}
	if resolution, ok := resolve_project_effective_method_parameter(
		units,
		source_file_index,
		ref,
	); ok {
		return resolution, true
	}
	all_namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in all_namespaces {
		if !reference_namespace_allowed(ref.kind, ref.namespace, namespace) {
			continue
		}
		if handle, ok := resolve_inherited_project_symbol(
			units,
			source_file_index,
			ref.scope,
			namespace,
			ref.name,
			roots,
			class_entries,
			visible,
		); ok {
			return Resolution{kind = .Symbol, symbol = handle}, true
		}
		if handle, ok := resolve_visible_class_definition_member(
			units,
			source_file_index,
			ref.scope,
			namespace,
			ref.name,
			roots,
			class_entries,
			visible,
			predecessors,
		); ok {
			return Resolution{kind = .Symbol, symbol = handle}, true
		}
		if handle, ok := root_symbol_in_visible_units(units, namespace, ref.name, visible); ok {
			return Resolution{kind = .Symbol, symbol = handle}, true
		}
		if handle, ok := global_visible_root_symbol(roots, namespace, ref.name); ok {
			return Resolution{kind = .Symbol, symbol = handle}, true
		}
		if entity, ok := global_visible_summary_entity(roots, namespace, ref.name); ok {
			return Resolution{kind = .Provider_Entity, entity = entity}, true
		}
	}
	if ref.kind == .Message_Class && ref.name in roots.provided_names {
		return Resolution{kind = .External}, true
	}
	return Resolution{}, false
}

resolve_project_effective_method_parameter :: proc(
	units: []Source_File_Provider,
	source_file_index: int,
	ref: Reference_Data,
) -> (Resolution, bool) {
	if ref.namespace != .Value || !(ref.kind == .Identifier || ref.kind == .Type_Ref) {
		return {}, false
	}
	handle, ok := effective_project_method_parameter_handle(
		units,
		source_file_index,
		ref.scope,
		ref.name,
	)
	if !ok {
		return {}, false
	}
	return Resolution{kind = .Symbol, symbol = handle}, true
}

effective_project_method_parameter_handle :: proc(
	units: []Source_File_Provider,
	source_file_index: int,
	scope_id: Scope_Id,
	name: string,
	found_symbol := INVALID_SYMBOL_ID,
) -> (Symbol_Link, bool) {
	if source_file_index < 0 || source_file_index >= len(units) {
		return {}, false
	}
	method_scope, method_symbol, ok := enclosing_method_scope(&units[source_file_index], scope_id)
	if !ok {
		return {}, false
	}
	if found_symbol != INVALID_SYMBOL_ID {
		found := symbol(&units[source_file_index], found_symbol)
		if found == nil || found.kind != .Parameter || found.scope != method_scope {
			return {}, false
		}
	}
	method_info := entity_decl_info(&units[source_file_index], method_symbol)
	if method_info == nil ||
	   method_info.effective_signature.unit == INVALID_SOURCE_FILE_ID ||
	   method_info.effective_signature.symbol == INVALID_SYMBOL_ID {
		return {}, false
	}
	member := method_info.effective_signature
	member_source_file_index := source_file_id_index(member.unit)
	if member_source_file_index < 0 || member_source_file_index >= len(units) {
		return {}, false
	}
	param_symbol, param_ok := entity_signature_parameter_symbol(
		&units[member_source_file_index],
		member.symbol,
		name,
	)
	if !param_ok {
		return {}, false
	}
	return Symbol_Link{unit = member.unit, symbol = param_symbol}, true
}

resolve_cached_local_method_parameter :: proc(
	unit: ^Source_File_Provider,
	ref: Reference_Data,
	found_symbol: Symbol_Id,
) -> (Resolution, bool) {
	if ref.namespace != .Value || !(ref.kind == .Identifier || ref.kind == .Type_Ref) {
		return {}, false
	}
	method_scope, method_symbol, ok := enclosing_method_scope(unit, ref.scope)
	if !ok {
		return {}, false
	}
	if found_symbol != INVALID_SYMBOL_ID {
		found := symbol(unit, found_symbol)
		if found == nil || found.kind != .Parameter || found.scope != method_scope {
			return {}, false
		}
	}
	method_info := entity_decl_info(unit, method_symbol)
	if method_info == nil ||
	   method_info.effective_signature.unit != unit.source_file_id ||
	   method_info.effective_signature.symbol == INVALID_SYMBOL_ID {
		return {}, false
	}
	param_symbol, param_ok := entity_signature_parameter_symbol(
		unit,
		method_info.effective_signature.symbol,
		ref.name,
	)
	if !param_ok {
		return {}, false
	}
	return symbol_resolution(unit, param_symbol), true
}

reference_namespace_allowed :: proc(
	kind: Reference_Kind,
	requested, candidate: Namespace,
) -> bool {
	if kind == .Type_Ref && requested == .Value {
		return candidate == .Value || candidate == .Type
	}
	return candidate == requested
}

resolve_project_super :: proc(
	units: []Source_File_Provider,
	source_file_index: int,
	scope_id: Scope_Id,
	roots: ^Project_Root_Lookup,
	visible: [dynamic]Source_File_Id,
) -> (
	Symbol_Link,
	bool,
) {
	class_symbol, ok := enclosing_instance_method_class_owner_unit(&units[source_file_index], scope_id)
	if !ok {
		return {}, false
	}
	super_name, has_super := class_superclass_name(&units[source_file_index], class_symbol)
	if !has_super {
		return {}, false
	}
	return resolve_type_name_in_project(units, source_file_index, super_name, roots, visible)
}

resolve_inherited_project_symbol :: proc(
	units: []Source_File_Provider,
	source_file_index: int,
	scope_id: Scope_Id,
	namespace: Namespace,
	name: string,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [dynamic]Source_File_Id,
) -> (
	Symbol_Link,
	bool,
) {
	current, ok := enclosing_class_owner_unit(&units[source_file_index], scope_id)
	if !ok {
		return {}, false
	}
	current_handle := Symbol_Link {
		unit   = units[source_file_index].source_file_id,
		symbol = current,
	}
	for _ in 0 ..< len(units) + 8 {
		next, next_ok := direct_superclass_handle(units, current_handle, roots, visible)
		if !next_ok {
			return {}, false
		}
		if member, member_ok := class_member_symbol_by_handle(
			units,
			next,
			namespace,
			name,
			class_entries,
			true,
		); member_ok {
			return member, true
		}
		current_handle = next
	}
	return {}, false
}
direct_superclass_handle :: proc(
	units: []Source_File_Provider,
	current: Symbol_Link,
	roots: ^Project_Root_Lookup,
	visible: [dynamic]Source_File_Id,
) -> (
	Symbol_Link,
	bool,
) {
	source_file_index := source_file_id_index(current.unit)
	if source_file_index < 0 || source_file_index >= len(units) {
		return {}, false
	}
	super_name, ok := class_superclass_name(&units[source_file_index], current.symbol)
	if !ok {
		return {}, false
	}
	return resolve_type_name_in_project(units, source_file_index, super_name, roots, visible)
}

resolve_type_name_in_project :: proc(
	units: []Source_File_Provider,
	source_file_index: int,
	name: string,
	roots: ^Project_Root_Lookup,
	visible: [dynamic]Source_File_Id,
) -> (
	Symbol_Link,
	bool,
) {
	if handle, ok := root_symbol_in_source_file(units, units[source_file_index].source_file_id, .Type, name); ok {
		return handle, true
	}
	if handle, ok := root_symbol_in_visible_units(units, .Type, name, visible); ok {
		return handle, true
	}
	return global_visible_root_symbol(roots, .Type, name)
}

resolve_visible_class_definition_member :: proc(
	units: []Source_File_Provider,
	source_file_index: int,
	scope_id: Scope_Id,
	namespace: Namespace,
	name: string,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	visible: [dynamic]Source_File_Id,
	predecessors: [dynamic]Source_File_Id,
) -> (
	Symbol_Link,
	bool,
) {
	class_symbol, ok := enclosing_class_owner_unit(&units[source_file_index], scope_id)
	if !ok {
		return {}, false
	}
	class_name := symbol(&units[source_file_index], class_symbol).name
	if handle, found := class_member_symbol_in_unit_by_class_name(
		units,
		units[source_file_index].source_file_id,
		class_name,
		namespace,
		name,
		roots,
		class_entries,
		false,
	); found {
		return handle, true
	}
	for i := len(predecessors) - 1; i >= 0; i -= 1 {
		if handle, found := class_member_symbol_in_unit_by_class_name(
			units,
			predecessors[i],
			class_name,
			namespace,
			name,
			roots,
			class_entries,
			false,
		); found {
			return handle, true
		}
	}
	for source_file_id in visible {
		if source_file_id == units[source_file_index].source_file_id {
			continue
		}
		if handle, found := class_member_symbol_in_unit_by_class_name(
			units,
			source_file_id,
			class_name,
			namespace,
			name,
			roots,
			class_entries,
			false,
		); found {
			return handle, true
		}
	}
	return {}, false
}
class_member_symbol_in_unit_by_class_name :: proc(
	units: []Source_File_Provider,
	source_file_id: Source_File_Id,
	class_name: string,
	namespace: Namespace,
	name: string,
	roots: ^Project_Root_Lookup,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	inherited: bool,
) -> (
	Symbol_Link,
	bool,
) {
	class_handle, ok := root_symbol_in_source_file(units, source_file_id, .Type, class_name)
	source_file_index := source_file_id_index(source_file_id)
	if !ok || source_file_index < 0 || source_file_index >= len(units) {
		return {}, false
	}
	owner := symbol(&units[source_file_index], class_handle.symbol)
	if owner == nil ||
	   !(owner.kind == .Class || owner.kind == .Interface) ||
	   (owner.kind == .Class && !unit_has_class_definition(&units[source_file_index], class_handle.symbol)) {
		return {}, false
	}
	return class_member_symbol_by_handle(
		units,
		class_handle,
		namespace,
		name,
		class_entries,
		inherited,
	)
}

class_member_symbol_by_handle :: proc(
	units: []Source_File_Provider,
	class_handle: Symbol_Link,
	namespace: Namespace,
	name: string,
	class_entries: map[Project_Class_Member_Key]Project_Class_Member_Entry,
	inherited: bool,
) -> (
	Symbol_Link,
	bool,
) {
	source_file_index := source_file_id_index(class_handle.unit)
	if source_file_index < 0 || source_file_index >= len(units) {
		return {}, false
	}
	key := Project_Class_Member_Key {
		class_unit   = class_handle.unit,
		class_symbol = class_handle.symbol,
		namespace    = namespace,
		name         = name,
	}
	if entry, ok := class_entries[key]; ok {
		if inherited {
			member := unit_class_member_symbol_canonical(&units[source_file_index], class_handle.symbol, name)
			info := entity_decl_info(&units[source_file_index], member.id) if member != nil else nil
			if info != nil && info.visibility == .Private {
				return {}, false
			}
		}
		return Symbol_Link{unit = entry.unit, symbol = entry.symbol}, true
	}
	return {}, false
}

root_symbol_in_visible_units :: proc(
	units: []Source_File_Provider,
	namespace: Namespace,
	name: string,
	visible: [dynamic]Source_File_Id,
) -> (
	Symbol_Link,
	bool,
) {
	for source_file_id in visible {
		if handle, ok := root_symbol_in_source_file(units, source_file_id, namespace, name); ok {
			return handle, true
		}
	}
	return {}, false
}

root_symbol_in_source_file :: proc(
	units: []Source_File_Provider,
	source_file_id: Source_File_Id,
	namespace: Namespace,
	name: string,
) -> (
	Symbol_Link,
	bool,
) {
	source_file_index := source_file_id_index(source_file_id)
	unit := &units[source_file_index]
	if symbol_id, ok := scope_lookup_declaration(unit, unit.root_scope, namespace, name); ok {
		return Symbol_Link{unit = source_file_id, symbol = symbol_id}, true
	}
	return {}, false
}

global_visible_root_symbol :: proc(
	roots: ^Project_Root_Lookup,
	namespace: Namespace,
	name: string,
) -> (
	Symbol_Link,
	bool,
) {
	key := Root_Name_Key {
		namespace = namespace,
		name      = name,
	}
	if handle, ok := roots.global[key]; ok {
		return handle, true
	}
	return {}, false
}

global_visible_summary_entity :: proc(
	roots: ^Project_Root_Lookup,
	namespace: Namespace,
	name: string,
) -> (
	Entity_Handle,
	bool,
) {
	if roots == nil || roots.summary_global == nil {
		return {}, false
	}
	key := Root_Name_Key {
		namespace = namespace,
		name      = name,
	}
	if handle, ok := roots.summary_global[key]; ok {
		return handle, true
	}
	return {}, false
}

root_symbol_visible_by_default :: proc(unit: ^Source_File_Provider, s: ^Symbol_Data) -> bool {
	if typepool_dependency_unit(unit.uri) {
		return typepool_root_symbol_visible_by_default(s.kind)
	}
	stem := uri_file_stem(unit.uri)
	#partial switch s.kind {
	case .Class, .Interface:
		return name_is_namespaced(s.name) || root_name_matches_unit_stem(stem, s.name)
	case .Type_Def:
		return root_name_matches_unit_stem(stem, s.name)
	case .Module, .Report:
		return true
	case:
		return false
	}
}

typepool_dependency_unit :: proc(uri: string) -> bool {
	return strings.has_prefix(uri, "abapls-typepool:/") ||
	       strings.has_prefix(uri, "abapls-summary:/type-pool/")
}

typepool_root_symbol_visible_by_default :: proc(kind: Symbol_Kind) -> bool {
	return kind == .Type_Def || kind == .Constant
}

root_name_matches_unit_stem :: proc(stem, name: string) -> bool {
	if strings.equal_fold(stem, name) {
		return true
	}
	component_start := 0
	for i in 0 ..< len(name) {
		if name[i] == '/' {
			component_start = i + 1
		}
	}
	component := name[component_start:]
	return(
		component_start > 0 &&
		component != "" &&
		len(stem) >= len(component) &&
		strings.equal_fold(stem[len(stem) - len(component):], component) \
	)
}

name_is_namespaced :: proc(name: string) -> bool {
	return len(name) > 0 && name[0] == '/'
}

include_visible_source_files_for_source_files :: proc(
	units: []Source_File_Provider,
	allocator: mem.Allocator,
) -> [][dynamic]Source_File_Id {
	out := make([][dynamic]Source_File_Id, len(units), allocator)
	for i in 0 ..< len(units) {
		out[i] = make([dynamic]Source_File_Id, allocator)
	}
	for unit in units {
		expansion := make([dynamic]Source_File_Id, allocator)
		stack := make([dynamic]Source_File_Id, allocator)
		collect_include_expansion(units, unit.source_file_id, &stack, &expansion)
		for participant in expansion {
			idx := source_file_id_index(participant)
			if idx < 0 || idx >= len(out) {
				continue
			}
			for candidate in expansion {
				if candidate != participant {
					push_unique_unit(&out[idx], candidate)
				}
			}
		}
	}
	return out
}

collect_include_expansion :: proc(
	units: []Source_File_Provider,
	source_file_id: Source_File_Id,
	stack, out: ^[dynamic]Source_File_Id,
) {
	idx := source_file_id_index(source_file_id)
	if idx < 0 || idx >= len(units) || unit_list_contains(stack^[:], source_file_id) {
		return
	}
	append(stack, source_file_id)
	push_unique_unit(out, source_file_id)
	for edge in units[idx].include_edges {
		if edge.has_target {
			collect_include_expansion(units, edge.target, stack, out)
		}
	}
	resize(stack, len(stack^) - 1)
}

include_predecessor_source_files_for_source_files :: proc(
	units: []Source_File_Provider,
	allocator: mem.Allocator,
) -> [][dynamic]Source_File_Id {
	out := make([][dynamic]Source_File_Id, len(units), allocator)
	for i in 0 ..< len(units) {
		out[i] = make([dynamic]Source_File_Id, allocator)
	}
	for unit in units {
		stack := make([dynamic]Source_File_Id, allocator)
		prior := make([dynamic]Source_File_Id, allocator)
		_ = record_include_predecessors(units, unit.source_file_id, prior, &out, &stack, allocator)
	}
	return out
}

record_include_predecessors :: proc(
	units: []Source_File_Provider,
	source_file_id: Source_File_Id,
	inherited_prior: [dynamic]Source_File_Id,
	predecessors: ^[][dynamic]Source_File_Id,
	stack: ^[dynamic]Source_File_Id,
	allocator: mem.Allocator,
) -> [dynamic]Source_File_Id {
	expansion := make([dynamic]Source_File_Id, allocator)
	idx := source_file_id_index(source_file_id)
	if idx < 0 || idx >= len(units) || unit_list_contains(stack^[:], source_file_id) {
		return expansion
	}
	append(stack, source_file_id)
	push_unique_unit(&expansion, source_file_id)
	prior := make([dynamic]Source_File_Id, 0, len(inherited_prior) + 1, allocator)
	for item in inherited_prior {push_unique_unit(&prior, item)}
	push_unique_unit(&prior, source_file_id)
	for edge in units[idx].include_edges {
		if !edge.has_target {
			continue
		}
		target_idx := source_file_id_index(edge.target)
		if target_idx >= 0 && target_idx < len(predecessors^) {
			for item in prior {
				push_unique_unit(&predecessors^[target_idx], item)
			}
		}
		nested := record_include_predecessors(
			units,
			edge.target,
			prior,
			predecessors,
			stack,
			allocator,
		)
		for item in nested {
			push_unique_unit(&prior, item)
			push_unique_unit(&expansion, item)
		}
	}
	resize(stack, len(stack^) - 1)
	return expansion
}

push_unique_unit :: proc(units: ^[dynamic]Source_File_Id, source_file_id: Source_File_Id) {
	if !unit_list_contains(units^[:], source_file_id) {
		append(units, source_file_id)
	}
}

unit_list_contains :: proc(units: []Source_File_Id, source_file_id: Source_File_Id) -> bool {
	for item in units {
		if item == source_file_id {
			return true
		}
	}
	return false
}

unit_has_class_definition :: proc(unit: ^Source_File_Provider, class_symbol: Symbol_Id) -> bool {
	for definition in unit.class_definitions {
		if definition.class_symbol == class_symbol {
			return true
		}
	}
	return false
}

expand_local_structure_includes :: proc(
	unit: ^Source_File_Provider,
	allocator: mem.Allocator,
) -> bool {
	any_changed := false
	changed := true
	for changed {
		changed = false
		for symbol_index in 0 ..< len(unit.symbols) {
			s := &unit.symbols[symbol_index]
			if s.structure != INVALID_STRUCTURE_ID || !s.has_declared_type {
				continue
			}
			if structure_id, ok := local_structure_for_type_ref(unit, s.scope, s.declared_type);
			   ok {
				symbol_set_structure(s, structure_id)
				changed = true
				any_changed = true
			}
		}
		for structure_index := 0; structure_index < len(unit.structures); structure_index += 1 {
			owner_scope := unit.structures[structure_index].scope
			if owner_scope == INVALID_SCOPE_ID {
				owner_scope = unit.root_scope
			}
			for field_index in 0 ..< len(unit.structures[structure_index].fields) {
				field := &unit.structures[structure_index].fields[field_index]
				if field.structure != INVALID_STRUCTURE_ID ||
				   !(.Has_Type_Ref in field.flags) {
					continue
				}
				if structure_id, ok := local_structure_for_type_ref(unit, owner_scope, field.type_ref);
				   ok && (!(.Is_Include in field.flags) ||
				         structure_id != unit.structures[structure_index].id) {
					field.structure = structure_id
					if !type_id_is_known(field.type_id) {
						field.type_id = type_structure(unit, structure_id)
					}
					changed = true
					any_changed = true
				}
			}
		}
		if expand_resolved_structure_includes(unit, allocator) {
			changed = true
			any_changed = true
		}
	}
	return any_changed
}

expand_resolved_structure_includes :: proc(unit: ^Source_File_Provider, allocator: mem.Allocator) -> bool {
	changed := false
	for structure_index := 0; structure_index < len(unit.structures); structure_index += 1 {
		old_fields := unit.structures[structure_index].fields
		has_include := false
		for field in old_fields {
			if .Is_Include in field.flags &&
			   field.structure != INVALID_STRUCTURE_ID &&
			   field.structure != unit.structures[structure_index].id {
				has_include = true
				break
			}
		}
		if !has_include {
			continue
		}
		new_fields := make([dynamic]Structure_Field_Data, 0, len(old_fields), allocator)
		for field in old_fields {
			if .Is_Include in field.flags &&
			   field.structure != INVALID_STRUCTURE_ID &&
			   field.structure != unit.structures[structure_index].id {
				if included := structure(unit, field.structure); included != nil {
					for included_field in included.fields {
						next := included_field
						if field.include_renaming_suffix != "" {
							next.name = append_structure_field_suffix(
								included_field.name,
								field.include_renaming_suffix,
								allocator,
							)
						}
						append(&new_fields, next)
					}
					changed = true
					continue
				}
			}
			append(&new_fields, field)
		}
		unit.structures[structure_index].fields = new_fields
	}
	return changed
}

append_structure_field_suffix :: proc(name, suffix: string, allocator: mem.Allocator) -> string {
	out := strings.builder_make(allocator)
	strings.write_string(&out, name)
	strings.write_string(&out, suffix)
	return strings.to_string(out)
}

local_structure_for_type_ref :: proc(
	unit: ^Source_File_Provider,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
) -> (
	Structure_Id,
	bool,
) {
	namespaces := [?]Namespace{.Type, .Value, .Routine}
	for namespace in namespaces {
		if !(namespace == type_ref.namespace ||
			   (type_ref.namespace == .Type && namespace == .Value)) {
			continue
		}
		symbol_id, ok := lookup_scope_chain(
			unit,
			&unit.scope_index,
			scope_id,
			namespace,
			type_ref.base_name,
		)
		if !ok {
			continue
		}
		if structure_id, structure_ok := local_structure_for_symbol_path(
			unit,
			symbol_id,
			type_ref.field_path[:],
			type_ref.field_selectors[:],
			type_ref.field_derefs[:],
		); structure_ok {
			return structure_id, true
		}
	}
	if type_ref.namespace == .Value || type_ref.namespace == .Type {
		if class_symbol, ok := enclosing_class_owner_unit(unit, scope_id); ok {
			if symbol_id, symbol_ok := class_scope_symbol(
				&unit.scope_index,
				class_symbol,
				type_ref.namespace,
				type_ref.base_name,
			); symbol_ok {
				return local_structure_for_symbol_path(unit, symbol_id, type_ref.field_path[:], type_ref.field_selectors[:], type_ref.field_derefs[:])
			}
		}
	}
	if type_ref.namespace == .Type {
		if symbol_id, symbol_ok := resolve_inherited_class_member(
			unit,
			&unit.scope_index,
			scope_id,
			.Type,
			type_ref.base_name,
		); symbol_ok {
			return local_structure_for_symbol_path(unit, symbol_id, type_ref.field_path[:], type_ref.field_selectors[:], type_ref.field_derefs[:])
		}
	}
	if type_ref.namespace == .Value {
		if symbol_id, symbol_ok := inherited_class_attribute_symbol_for_type_ref(
			unit,
			scope_id,
			type_ref.base_name,
		); symbol_ok {
			return local_structure_for_symbol_path(unit, symbol_id, type_ref.field_path[:], type_ref.field_selectors[:], type_ref.field_derefs[:])
		}
	}
	return INVALID_STRUCTURE_ID, false
}

inherited_class_attribute_symbol_for_type_ref :: proc(
	unit: ^Source_File_Provider,
	scope_id: Scope_Id,
	name: string,
) -> (
	Symbol_Id,
	bool,
) {
	current_class, ok := enclosing_class_owner_unit(unit, scope_id)
	if !ok {
		return INVALID_SYMBOL_ID, false
	}
	for _ in 0 ..= len(unit.class_inheritance) {
		super_name, has_super := class_superclass_name(unit, current_class)
		if !has_super {
			return INVALID_SYMBOL_ID, false
		}
		super_symbol, super_ok := lookup_scope_chain(
			unit,
			&unit.scope_index,
			scope_id,
			.Type,
			super_name,
		)
		if !super_ok {
			return INVALID_SYMBOL_ID, false
		}
		member := unit_class_member_symbol_canonical(unit, super_symbol, name)
		info := entity_decl_info(unit, member.id) if member != nil else nil
		if info != nil && info.member_kind == .Attribute && info.visibility != .Private {
			return class_scope_symbol(&unit.scope_index, super_symbol, .Value, name)
		}
		current_class = super_symbol
	}
	return INVALID_SYMBOL_ID, false
}

local_structure_for_symbol_path :: proc(
	unit: ^Source_File_Provider,
	symbol_id: Symbol_Id,
	path: []string,
	selectors: []ast.Selector_Op,
	derefs: []bool,
) -> (
	Structure_Id,
	bool,
) {
	current_symbol_id := symbol_id
	current_path := path
	s := symbol(unit, current_symbol_id)
	if s != nil && (s.kind == .Class || s.kind == .Interface) {
		if len(current_path) == 0 {
			return INVALID_STRUCTURE_ID, false
		}
		nested, nested_ok := class_type_symbol_handle_in_unit(
			unit,
			current_symbol_id,
			current_path[0],
		)
		if !nested_ok {
			return INVALID_STRUCTURE_ID, false
		}
		current_symbol_id = nested
		s = symbol(unit, current_symbol_id)
		current_path = current_path[1:]
	}
	current_derefs := derefs
	if len(derefs) > 0 {
		current_derefs = derefs[len(path) - len(current_path):]
	}
	current_selectors := selectors
	if len(selectors) > 0 {
		current_selectors = selectors[len(path) - len(current_path):]
	}
	if s != nil &&
	   s.structure == INVALID_STRUCTURE_ID &&
	   len(current_path) > 0 &&
	   selector_at(current_selectors, 0) == .Arrow &&
	   s.has_declared_type &&
	   s.declared_type.is_ref {
		if class_symbol, class_ok := local_class_symbol_for_type_ref(unit, s.scope, s.declared_type);
		   class_ok {
			return local_structure_for_class_member_path(
				unit,
				class_symbol,
				current_path,
				current_selectors,
				current_derefs,
			)
		}
	}
	if s == nil || s.structure == INVALID_STRUCTURE_ID {
		return INVALID_STRUCTURE_ID, false
	}
	return resolve_unit_structure_path(unit, s.structure, current_path, current_selectors, current_derefs)
}

local_class_symbol_for_type_ref :: proc(
	unit: ^Source_File_Provider,
	scope_id: Scope_Id,
	type_ref: Field_Type_Ref_Data,
) -> (
	Symbol_Id,
	bool,
) {
	if type_ref.base_name == "" {
		return INVALID_SYMBOL_ID, false
	}
	symbol_id, ok := lookup_scope_chain(
		unit,
		&unit.scope_index,
		scope_id,
		type_ref.namespace,
		type_ref.base_name,
	)
	if !ok && type_ref.namespace == .Type {
		symbol_id, ok = lookup_scope_chain(unit, &unit.scope_index, scope_id, .Value, type_ref.base_name)
	}
	if !ok {
		return INVALID_SYMBOL_ID, false
	}
	s := symbol(unit, symbol_id)
	if s == nil || !(s.kind == .Class || s.kind == .Interface) {
		return INVALID_SYMBOL_ID, false
	}
	return symbol_id, true
}

local_structure_for_class_member_path :: proc(
	unit: ^Source_File_Provider,
	class_symbol: Symbol_Id,
	path: []string,
	selectors: []ast.Selector_Op,
	derefs: []bool,
) -> (
	Structure_Id,
	bool,
) {
	if len(path) == 0 || selector_at(selectors, 0) != .Arrow {
		return INVALID_STRUCTURE_ID, false
	}
	member := unit_class_member_symbol_canonical(unit, class_symbol, path[0])
	info := entity_decl_info(unit, member.id) if member != nil else nil
	if member == nil || info == nil || info.member_kind != .Attribute || member.structure == INVALID_STRUCTURE_ID {
		return INVALID_STRUCTURE_ID, false
	}
	if len(path) == 1 {
		return member.structure, true
	}
	next_selectors := selectors
	next_derefs := derefs
	if len(selectors) > 0 {
		next_selectors = selectors[1:]
	}
	if len(derefs) > 0 {
		next_derefs = derefs[1:]
	}
	return resolve_unit_structure_path(unit, member.structure, path[1:], next_selectors, next_derefs)
}

resolve_unit_structure_path :: proc(
	unit: ^Source_File_Provider,
	start: Structure_Id,
	path: []string,
	selectors: []ast.Selector_Op,
	derefs: []bool,
) -> (
	Structure_Id,
	bool,
) {
	current := start
	for field_name, i in path {
		if i < len(derefs) && derefs[i] {
			continue
		}
		if selector_at(selectors, i) != .Dash {
			return INVALID_STRUCTURE_ID, false
		}
		field := structure_field(unit, current, field_name)
		if field == nil || field.structure == INVALID_STRUCTURE_ID {
			return INVALID_STRUCTURE_ID, false
		}
		current = field.structure
	}
	return current, true
}

selector_at :: #force_inline proc(selectors: []ast.Selector_Op, index: int) -> ast.Selector_Op {
	return selectors[index] if index < len(selectors) else .Dash
}

class_type_symbol_handle :: proc(
	units: []Source_File_Provider,
	class_handle: Symbol_Link,
	name: string,
) -> (
	Symbol_Link,
	bool,
) {
	source_file_index := source_file_id_index(class_handle.unit)
	if source_file_index < 0 || source_file_index >= len(units) {
		return {}, false
	}
	if symbol_id, ok := class_type_symbol_handle_in_unit(
		&units[source_file_index],
		class_handle.symbol,
		name,
	); ok {
		return Symbol_Link{unit = class_handle.unit, symbol = symbol_id}, true
	}
	return {}, false
}

class_type_symbol_handle_in_unit :: proc(
	unit: ^Source_File_Provider,
	class_symbol: Symbol_Id,
	name: string,
) -> (
	Symbol_Id,
	bool,
) {
	key := Class_Scope_Index_Key {
		class_symbol = class_symbol,
		namespace    = .Type,
		name         = name,
	}
	if symbol_id, ok := unit.scope_index.class_symbols[key]; ok {
		return symbol_id, true
	}
	return INVALID_SYMBOL_ID, false
}

resolve_type_ref_handle_project :: proc(
	units: []Source_File_Provider,
	source_file_index: int,
	type_ref: Field_Type_Ref_Data,
	roots: ^Project_Root_Lookup,
	visible: [dynamic]Source_File_Id,
) -> (
	Symbol_Link,
	bool,
) {
	all_namespaces := [?]Namespace{.Value, .Type, .Routine}
	for namespace in all_namespaces {
		if !(namespace == type_ref.namespace ||
			   (type_ref.namespace == .Value && namespace == .Type)) {
			continue
		}
		if handle, ok := root_symbol_in_source_file(
			units,
			units[source_file_index].source_file_id,
			namespace,
			type_ref.base_name,
		); ok {
			return handle, true
		}
		if handle, ok := root_symbol_in_visible_units(
			units,
			namespace,
			type_ref.base_name,
			visible,
		); ok {
			return handle, true
		}
		if handle, ok := global_visible_root_symbol(roots, namespace, type_ref.base_name); ok {
			return handle, true
		}
	}
	return {}, false
}
