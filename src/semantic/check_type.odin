package abap_frontend_semantic2

import "src:ast"
import string_interner "src:string_interner"

import "core:strings"

checker_check_decl_type_clause :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	clause: ^ast.Data_Type_Clause,
	occurs: ^ast.Expr = nil,
) -> ^Type {
	if clause == nil {
		return nil
	}
	type_ref, has_ref := checker_type_ref_data_from_clause(ctx, clause)
	node := &clause.type_ref.expr_base if clause.type_ref != nil else nil
	form := checker_type_form_with_occurs(clause.form, occurs)
	typ := checker_type_from_ref_data_with_form(ctx, type_ref, has_ref, form, true, node, entity)
	if has_ref {
		checker_record_type_ref_raw_uses(ctx, clause.type_ref)
		checker_record_type_expr_info(ctx, clause.type_ref, typ)
		checker_record_type_ref_key_uses(ctx, clause.type_ref, typ, form)
		checker_validate_decl_type_ref(ctx, entity, type_ref)
	} else if checker_type_clause_is_generic_table(clause, form) {
		checker_validate_generic_table_type_form(ctx, entity, clause, form)
	}
	checker_check_expr(ctx, clause.initial_size, .Value)
	checker_check_expr(ctx, occurs, .Value)
	return typ
}

checker_check_field_type :: proc(ctx: ^Checker_Context, entity: ^Entity, decl: ^Decl_Info) -> ^Type {
	assert(entity != nil && entity.kind == .Field)
	payload, ok := entity.payload.(^Entity_Field_Payload)
	assert(ok && payload != nil)

	typ: ^Type
	if decl != nil && decl.type_clause != nil {
		type_ref, has_ref := checker_type_ref_data_from_clause(ctx, decl.type_clause)
		payload.type_ref = type_ref
		payload.type_clause_form = checker_type_form_with_occurs(decl.type_clause.form, decl.occurs)
		payload.has_type_clause_form = true
		if has_ref {
			payload.flags += {.Has_Type_Ref}
		}
		node := &decl.type_clause.type_ref.expr_base if decl.type_clause.type_ref != nil else nil
		typ = checker_type_from_ref_data_with_form(ctx, type_ref, has_ref, payload.type_clause_form, true, node, entity)
		if has_ref {
			checker_record_type_ref_raw_uses(ctx, decl.type_clause.type_ref)
			checker_record_type_expr_info(ctx, decl.type_clause.type_ref, typ)
			checker_record_type_ref_key_uses(ctx, decl.type_clause.type_ref, typ, payload.type_clause_form)
			checker_validate_decl_type_ref(ctx, entity, type_ref)
		}
	} else if .Has_Type_Ref in payload.flags {
		typ = checker_type_from_ref_data_with_form(
			ctx,
			payload.type_ref,
			true,
			payload.type_clause_form,
			payload.has_type_clause_form,
			entity.node,
			entity,
		)
		checker_validate_decl_type_ref(ctx, entity, payload.type_ref)
	}
	if typ == nil {
		typ = entity.type
	}
	if typ == nil {
		typ = project_type_unknown(ctx.project)
	}
	entity.type = typ
	if .Is_Include in payload.flags {
		checker_expand_structure_include(ctx, entity)
	}
	return typ
}

checker_type_from_expr :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	namespace: Namespace,
	is_ref := false,
) -> (^Type, ^Entity) {
	if expr == nil {
		return project_type_unknown(ctx.project), nil
	}
	type_ref := checker_type_ref_data_from_expr(ctx, expr, namespace, is_ref)
	typ, entity := checker_type_from_ref_data(ctx, type_ref, &expr.expr_base)
	checker_record_type_ref_raw_uses(ctx, expr)
	return typ, entity
}

checker_type_from_ref_data_with_form :: proc(
	ctx: ^Checker_Context,
	type_ref: Field_Type_Ref_Data,
	has_ref: bool,
	form: ast.Data_Type_Form,
	has_form: bool,
	node: ^ast.Node,
	current_decl_entity: ^Entity = nil,
) -> ^Type {
	base: ^Type
	if has_ref {
		base, _ = checker_type_from_ref_data(ctx, type_ref, node, current_decl_entity)
	} else {
		base = project_type_unknown(ctx.project)
	}
	if has_form {
		#partial switch form {
		case .Type_Line_Of, .Like_Line_Of:
			return checker_type_row(ctx, base)
		case .Range_Of:
			return project_type_table(ctx.project, base, form)
		case .Any_Table,
		     .Table,
		     .Like_Table,
		     .Index_Table,
		     .Standard_Table,
		     .Sorted_Table,
		     .Hashed_Table,
		     .Like_Standard_Table,
		     .Like_Sorted_Table,
		     .Like_Hashed_Table:
			return project_type_table(ctx.project, base, form)
		}
	}
	return base
}

checker_type_from_ref_data :: proc(
	ctx: ^Checker_Context,
	type_ref: Field_Type_Ref_Data,
	node: ^ast.Node = nil,
	current_decl_entity: ^Entity = nil,
	preferred_external_kind: External_Candidate_Kind = .Global_Symbol,
) -> (^Type, ^Entity) {
	if !string_interner.is_valid(type_ref.base_name) {
		return project_type_unknown(ctx.project), nil
	}
	skip_current := checker_type_ref_should_skip_current_decl(type_ref, current_decl_entity)
	entity: ^Entity
	ok: bool
	if skip_current {
		_, entity, ok = checker_lookup_reference(
			ctx,
			type_ref.namespace,
			type_ref.base_name,
			preferred_external_kind,
			excluded = current_decl_entity,
		)
	} else {
		_, entity, ok = checker_lookup_reference(ctx, type_ref.namespace, type_ref.base_name, preferred_external_kind)
	}
	if !ok && type_ref.namespace == .Value && type_ref.allow_type_lookup {
		_, entity, ok = checker_lookup_reference(ctx, .Type, type_ref.base_name, preferred_external_kind)
		if !ok {
			checker_add_unresolved_candidate(
				ctx,
				type_ref.base_name,
				.Type,
				preferred_external_kind,
				.Type_Reference,
				.Unresolved_Type,
				type_ref.base_range,
				node,
			)
		}
	}
	if !ok {
		kind := External_Candidate_Kind.Global_Symbol
		reason := External_Candidate_Reason.Unresolved_Reference
		if type_ref.namespace == .Type {
			reason = .Unresolved_Type
			checker_add_unresolved_type_diagnostic(ctx, type_ref, current_decl_entity)
		}
		checker_add_unresolved_candidate(
			ctx,
			type_ref.base_name,
			type_ref.namespace,
			preferred_external_kind if preferred_external_kind != .Global_Symbol else kind,
			.Type_Reference,
			reason,
			type_ref.base_range,
			node,
		)
		if type_ref.namespace == .Type && len(type_ref.field_path) == 0 {
			return project_type_named(ctx.project, type_ref.base_name, nil, project_type_unknown(ctx.project)), nil
		}
		return project_type_unknown(ctx.project), nil
	}

	current_entity := entity
	current := checker_type_from_entity(ctx, current_entity, node, type_ref.base_range)
	for i := 0; i < len(type_ref.field_path); i += 1 {
		selector := checker_type_selector_at(type_ref.field_selectors[:], i)
		name := type_ref.field_path[i]
		range := type_ref.field_ranges[i] if i < len(type_ref.field_ranges) else Range{}
		if selector == .Arrow {
			target := checker_type_ref_target(ctx, current)
			if checker_type_path_segment_is_deref(type_ref, i) {
				current = target
				current_entity = checker_type_entity(current)
				continue
			}
			if owner := checker_type_object_entity(target); owner != nil {
				member, member_ok := checker_lookup_object_member_visible(
					ctx,
					owner,
					.Value,
					name,
					range,
				)
				if !member_ok {
					return project_type_unknown(ctx.project), current_entity
				}
				current_entity = member
				current = checker_type_from_entity(ctx, current_entity, node, range)
				continue
			}
			return project_type_unknown(ctx.project), current_entity
		}
		if selector == .Fat_Arrow || selector == .Tilde {
			owner := checker_type_object_entity(current)
			if owner == nil && (current_entity.kind == .Class || current_entity.kind == .Interface) {
				owner = current_entity
			}
			if owner == nil {
				return project_type_unknown(ctx.project), current_entity
			}
			member, member_ok := checker_lookup_object_member_visible(
				ctx,
				owner,
				.Type,
				name,
				range,
			)
			if !member_ok {
				return project_type_unknown(ctx.project), current_entity
			}
			current_entity = member
			current = checker_type_from_entity(ctx, current_entity, node, range)
			continue
		}
		if structure := checker_type_structure(current); structure != nil {
			field, field_ok := checker_lookup_structure_field(structure, name)
			if !field_ok {
				return project_type_unknown(ctx.project), current_entity
			}
			current_entity = field
			current = checker_type_from_entity(ctx, current_entity, node, range)
			continue
		}
		return project_type_unknown(ctx.project), current_entity
	}

	if type_ref.is_ref {
		current = project_type_ref(ctx.project, current)
	}
	return current, current_entity
}

checker_add_unresolved_type_diagnostic :: proc(
	ctx: ^Checker_Context,
	type_ref: Field_Type_Ref_Data,
	entity: ^Entity,
) {
	if entity == nil || type_ref.namespace != .Type || !string_interner.is_valid(type_ref.base_name) {
		return
	}
	name := string_interner.load(ctx.project.interner, type_ref.base_name)
	message := "unresolved type"
	if name != "" {
		builder := strings.builder_make(context.temp_allocator)
		strings.write_string(&builder, "unresolved type ")
		strings.write_string(&builder, name)
		message = strings.to_string(builder)
	}
	checker_add_diagnostic(
		ctx,
		.Unresolved_Type,
		type_ref.base_range,
		message,
		entity,
		entity.decl_info,
	)
}

checker_type_ref_should_skip_current_decl :: proc(
	type_ref: Field_Type_Ref_Data,
	entity: ^Entity,
) -> bool {
	return entity != nil &&
	       type_ref.namespace == .Value &&
	       entity_kind_occupies(entity.kind, .Value)
}

checker_type_from_entity :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	node: ^ast.Node = nil,
	range: Range = {},
) -> ^Type {
	if entity == nil {
		return project_type_unknown(ctx.project)
	}
	if range.end > range.start {
		checker_add_entity_use_at_range(ctx, node, entity, range)
	} else {
		checker_add_entity_use(ctx, node, entity)
	}
	if !entity_is_builtin(entity) && entity.state != .Resolved && entity.state != .Failed {
		checker_check_entity_decl(ctx, entity)
	}
	if entity.state == .Failed {
		return project_type_unknown(ctx.project)
	}
	if entity.type == nil {
		return project_type_unknown(ctx.project)
	}
	return entity.type
}

checker_type_ref_data_from_clause :: proc(
	ctx: ^Checker_Context,
	clause: ^ast.Data_Type_Clause,
) -> (Field_Type_Ref_Data, bool) {
	if clause == nil || clause.type_ref == nil {
		return Field_Type_Ref_Data{}, false
	}
	namespace := Namespace.Type
	#partial switch clause.form {
	case .Like,
	     .Structure,
	     .Like_Line_Of,
	     .Like_Table,
	     .Like_Standard_Table,
	     .Like_Sorted_Table,
	     .Like_Hashed_Table:
		namespace = .Value
	}
	type_ref := checker_type_ref_data_from_expr(
		ctx,
		clause.type_ref,
		namespace,
		clause.form == .Ref_To || checker_type_ref_expr_is_ref(clause.type_ref),
	)
	type_ref.allow_type_lookup = checker_type_clause_form_allows_like_type_lookup(clause.form)
	return type_ref, true
}

checker_type_clause_form_allows_like_type_lookup :: proc(form: ast.Data_Type_Form) -> bool {
	#partial switch form {
	case .Like,
	     .Like_Line_Of,
	     .Like_Table,
	     .Like_Standard_Table,
	     .Like_Sorted_Table,
	     .Like_Hashed_Table:
		return true
	}
	return false
}

checker_type_ref_expr_is_ref :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	if n, ok := expr.derived_expr.(^ast.Type_Ref_Expr); ok {
		return n.is_ref
	}
	return false
}

checker_type_ref_data_from_expr :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	namespace: Namespace,
	is_ref := false,
) -> Field_Type_Ref_Data {
	data := Field_Type_Ref_Data {
		namespace       = namespace,
		is_ref          = is_ref,
		field_path      = make([dynamic]string_interner.String, 0, 2, ctx.project.allocator),
		field_ranges    = make([dynamic]Range, 0, 2, ctx.project.allocator),
		field_derefs    = make([dynamic]bool, 0, 2, ctx.project.allocator),
		field_selectors = make([dynamic]ast.Selector_Op, 0, 2, ctx.project.allocator),
	}
	if expr == nil {
		return data
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Type_Ref_Expr:
		base := n.base_name
		if base.text == "" {
			base = n.name
		}
		if base.text != "" {
			data.base_name = checker_intern_name(ctx.project, base.text)
			data.base_range = base.range if base.range.end > base.range.start else n.range
		}
		data.is_ref = data.is_ref || n.is_ref
		if len(n.path) > 0 && (n.path[0].selector == .Fat_Arrow || n.path[0].selector == .Tilde) {
			data.namespace = .Type
		}
		for segment in n.path {
			append(&data.field_path, checker_intern_name(ctx.project, segment.name.text))
			append(&data.field_ranges, segment.name.range)
			append(&data.field_derefs, segment.selector == .Arrow && segment.name.text == "*")
			append(&data.field_selectors, segment.selector)
		}
	case ^ast.Ident_Expr:
		data.base_name = checker_intern_name(ctx.project, n.name)
		data.base_range = n.range
	case ^ast.Literal_Expr:
		data.base_name = checker_intern_name(ctx.project, n.value)
		data.base_range = n.range
	}
	return data
}

checker_record_type_ref_raw_uses :: proc(ctx: ^Checker_Context, expr: ^ast.Expr) {
	if expr == nil {
		return
	}
	if n, ok := expr.derived_expr.(^ast.Type_Ref_Expr); ok {
		for raw_ref in n.raw_refs {
			ref_namespace := Namespace.Type if raw_ref.type_base else Namespace.Value
			checker_check_ident_name(ctx, &expr.expr_base, raw_ref.name.text, ref_namespace, false)
		}
	}
}

checker_record_type_ref_key_uses :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	typ: ^Type,
	form: ast.Data_Type_Form,
) {
	if expr == nil || typ == nil || !checker_type_form_is_table_category(form) {
		return
	}
	n, ok := expr.derived_expr.(^ast.Type_Ref_Expr)
	if !ok {
		return
	}
	row_structure := checker_type_structure(checker_type_row(ctx, typ))
	if row_structure == nil {
		return
	}
	if len(n.keys) > 0 {
		for key in n.keys {
			checker_record_type_ref_key_clause_uses(ctx, key, row_structure, &expr.expr_base)
		}
		return
	}
	checker_record_type_ref_key_clause_uses(ctx, n.key, row_structure, &expr.expr_base)
}

checker_record_type_ref_key_clause_uses :: proc(
	ctx: ^Checker_Context,
	key: ^ast.Type_Ref_Key_Clause,
	row_structure: ^Structure,
	node: ^ast.Node,
) {
	if key == nil || row_structure == nil {
		return
	}
	for component in key.components {
		range := component.range
		if range.end <= range.start {
			continue
		}
		name := checker_intern_name(ctx.project, component.text)
		if !string_interner.is_valid(name) {
			continue
		}
		if field, ok := checker_lookup_structure_field(row_structure, name); ok {
			checker_add_entity_use_at_range(ctx, node, field, range)
		}
	}
}

checker_record_type_expr_info :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, typ: ^Type) {
	if expr != nil {
		checker_record_expr_info(ctx, &expr.expr_base, .Type, typ)
	}
}

checker_validate_decl_type_ref :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	type_ref: Field_Type_Ref_Data,
) {
	if entity == nil ||
	   type_ref.namespace != .Type ||
	   len(type_ref.field_path) > 0 ||
	   !string_interner.is_valid(type_ref.base_name) {
		return
	}
	name := string_interner.load(ctx.project.interner, type_ref.base_name)
	if name == "object" && !type_ref.is_ref {
		checker_add_diagnostic(
			ctx,
			.Invalid_Object_Type_Reference,
			type_ref.base_range,
			"object type needs REF TO",
			entity,
			entity.decl_info,
		)
		return
	}
	if !checker_generic_builtin_type_name(name) {
		if !type_ref.is_ref {
			_, target, ok := checker_lookup_reference(ctx, .Type, type_ref.base_name)
			if ok && (target.kind == .Class || target.kind == .Interface) {
				checker_add_diagnostic(
					ctx,
					.Invalid_Object_Type_Reference,
					type_ref.base_range,
					"object type needs REF TO",
					entity,
					entity.decl_info,
				)
			}
		}
		return
	}
	if type_ref.is_ref {
		if checker_generic_builtin_ref_type_name(name) {
			return
		}
		checker_add_diagnostic(
			ctx,
			.Invalid_Generic_Builtin_Type,
			type_ref.base_range,
			"generic type not allowed after REF TO",
			entity,
			entity.decl_info,
		)
		return
	}
	if entity.kind == .Parameter || entity.kind == .Field_Symbol {
		return
	}
	checker_add_diagnostic(
		ctx,
		.Invalid_Generic_Builtin_Type,
		type_ref.base_range,
		"generic type only allowed for parameters and field symbols",
		entity,
		entity.decl_info,
	)
}

checker_validate_generic_table_type_form :: proc(
	ctx: ^Checker_Context,
	entity: ^Entity,
	clause: ^ast.Data_Type_Clause,
	form: ast.Data_Type_Form = .Type,
) {
	if entity == nil || clause == nil || !checker_type_clause_is_generic_table(clause, form) {
		return
	}
	if entity.kind == .Parameter || entity.kind == .Field_Symbol {
		return
	}
	if entity.kind == .Type_Def && clause.table_has_of && clause.type_ref != nil {
		return
	}
	checker_add_diagnostic(
		ctx,
		.Invalid_Generic_Table_Type,
		entity.name_range,
		"generic table type only allowed for parameters and field symbols",
		entity,
		entity.decl_info,
	)
}

checker_generic_builtin_type_name :: proc(name: string) -> bool {
	switch name {
	case "xsequence",
	     "data",
	     "any",
	     "simple",
	     "decfloat",
	     "numeric",
	     "clike",
	     "csequence",
	     "object":
		return true
	}
	return false
}

checker_generic_builtin_ref_type_name :: #force_inline proc(name: string) -> bool {
	return name == "data" || name == "object"
}

checker_type_clause_is_generic_table :: proc(clause: ^ast.Data_Type_Clause, form: ast.Data_Type_Form) -> bool {
	if clause == nil {
		return false
	}
	#partial switch form {
	case .Any_Table,
	     .Index_Table:
		return true
	case .Table,
	     .Standard_Table,
	     .Sorted_Table,
	     .Hashed_Table:
		return clause.type_ref == nil
	}
	return false
}

checker_type_form_with_occurs :: proc(form: ast.Data_Type_Form, occurs: ^ast.Expr) -> ast.Data_Type_Form {
	if occurs == nil {
		return form
	}
	#partial switch form {
	case .Like:
		return .Like_Table
	case .Type:
		return .Standard_Table
	}
	return form
}

checker_type_form_is_table_category :: proc(form: ast.Data_Type_Form) -> bool {
	#partial switch form {
	case .Any_Table,
	     .Table,
	     .Like_Table,
	     .Index_Table,
	     .Standard_Table,
	     .Sorted_Table,
	     .Hashed_Table,
	     .Like_Standard_Table,
	     .Like_Sorted_Table,
	     .Like_Hashed_Table:
		return true
	}
	return false
}

checker_type_row :: proc(ctx: ^Checker_Context, typ: ^Type, depth := 0) -> ^Type {
	if depth > 16 || typ == nil {
		return project_type_unknown(ctx.project)
	}
	#partial switch typ.kind {
	case .Table:
		return typ.base if typ.base != nil else project_type_unknown(ctx.project)
	case .Named:
		return checker_type_row(ctx, typ.base, depth + 1)
	}
	return project_type_unknown(ctx.project)
}

checker_type_ref_target :: proc(ctx: ^Checker_Context, typ: ^Type, depth := 0) -> ^Type {
	if depth > 16 || typ == nil {
		return project_type_unknown(ctx.project)
	}
	#partial switch typ.kind {
	case .Ref:
		return typ.base if typ.base != nil else project_type_unknown(ctx.project)
	case .Named:
		return checker_type_ref_target(ctx, typ.base, depth + 1)
	}
	return project_type_unknown(ctx.project)
}

checker_type_structure :: proc(typ: ^Type, depth := 0) -> ^Structure {
	if depth > 16 || typ == nil {
		return nil
	}
	#partial switch typ.kind {
	case .Structure:
		return typ.structure
	case .Named:
		return checker_type_structure(typ.base, depth + 1)
	}
	return nil
}

checker_type_object_entity :: proc(typ: ^Type, depth := 0) -> ^Entity {
	if depth > 16 || typ == nil {
		return nil
	}
	#partial switch typ.kind {
	case .Class, .Interface:
		return typ.entity
	case .Named, .Ref:
		return checker_type_object_entity(typ.base, depth + 1)
	}
	return nil
}

checker_type_entity :: proc(typ: ^Type) -> ^Entity {
	if typ == nil {
		return nil
	}
	if typ.entity != nil {
		return typ.entity
	}
	if typ.base != nil {
		return checker_type_entity(typ.base)
	}
	return nil
}

checker_type_selector_at :: proc(selectors: []ast.Selector_Op, index: int) -> ast.Selector_Op {
	return selectors[index] if index < len(selectors) else .Dash
}

checker_type_path_segment_is_deref :: proc(type_ref: Field_Type_Ref_Data, index: int) -> bool {
	return index < len(type_ref.field_derefs) && type_ref.field_derefs[index]
}

checker_type_same :: proc(a, b: ^Type, depth := 0) -> bool {
	if a == b {
		return true
	}
	if depth > 32 || a == nil || b == nil || a.kind != b.kind {
		return false
	}
	#partial switch a.kind {
	case .Unknown:
		return true
	case .Builtin:
		return a.name == b.name
	case .Named:
		if a.entity != nil || b.entity != nil {
			return a.entity == b.entity
		}
		return a.name == b.name && checker_type_same(a.base, b.base, depth + 1)
	case .Structure:
		return a.structure == b.structure
	case .Table:
		return a.table_form == b.table_form && checker_type_same(a.base, b.base, depth + 1)
	case .Ref:
		return checker_type_same(a.base, b.base, depth + 1)
	case .Class, .Interface:
		if a.entity != nil || b.entity != nil {
			return a.entity == b.entity
		}
		return a.name == b.name
	case .Routine:
		return a.routine.signature_scope == b.routine.signature_scope
	}
	return false
}

checker_expand_structure_include :: proc(ctx: ^Checker_Context, include_entity: ^Entity) {
	assert(include_entity != nil && include_entity.kind == .Field)
	payload, ok := include_entity.payload.(^Entity_Field_Payload)
	assert(ok && payload != nil)
	owner := payload.owner_structure
	included := checker_type_structure(include_entity.type)
	if owner == nil || included == nil || included == owner {
		return
	}

	include_index := -1
	for field, index in owner.fields {
		if field == include_entity {
			include_index = index
			break
		}
	}
	if include_index < 0 {
		return
	}

	next_fields := make([dynamic]^Entity, 0, len(owner.fields) + len(included.fields), ctx.project.allocator)
	for field in owner.fields {
		if field != include_entity {
			append(&next_fields, field)
			continue
		}
		for included_field in included.fields {
			if included_payload, included_ok := included_field.payload.(^Entity_Field_Payload);
			   included_ok && included_payload != nil && .Is_Include in included_payload.flags {
				continue
			}
			copy := checker_copy_included_structure_field(ctx, owner, include_entity, included_field)
			append(&next_fields, copy)
		}
	}
	owner.fields = next_fields
	for field, index in owner.fields {
		if field_payload, field_ok := field.payload.(^Entity_Field_Payload); field_ok && field_payload != nil {
			field_payload.field_index = i32(index)
		}
	}
}

checker_copy_included_structure_field :: proc(
	ctx: ^Checker_Context,
	owner_structure: ^Structure,
	include_entity: ^Entity,
	included_field: ^Entity,
) -> ^Entity {
	assert(owner_structure != nil && include_entity != nil && included_field != nil)
	include_payload, include_ok := include_entity.payload.(^Entity_Field_Payload)
	assert(include_ok && include_payload != nil)
	included_payload, included_ok := included_field.payload.(^Entity_Field_Payload)
	assert(included_ok && included_payload != nil)

	entity := project_new_entity(ctx.project, .Field)
	entity.owner = include_entity.owner
	entity.source_file = include_entity.source_file
	entity.node = include_entity.node
	entity.type = included_field.type
	entity.state = .Resolved
	name := included_field.name
	if include_payload.include_renaming_suffix != "" {
		name = checker_intern_suffix_name(ctx, name, include_payload.include_renaming_suffix)
	}
	decl := project_new_decl_info(
		ctx.project,
		entity,
		owner_structure.scope,
		name,
		.Field,
		include_entity.name_range,
		include_entity.node,
	)
	decl.state = .Resolved
	payload, payload_ok := entity.payload.(^Entity_Field_Payload)
	assert(payload_ok && payload != nil)
	payload^ = included_payload^
	payload.owner_structure = owner_structure
	payload.decl_unit = include_payload.decl_unit
	payload.decl_range = include_payload.decl_range
	payload.flags -= {.Is_Include}
	payload.include_renaming_suffix = ""
	if previous := scope_insert_declaration(owner_structure.scope, entity); previous != nil && previous != entity {
		checker_add_diagnostic(ctx, .Duplicate_Declaration, include_entity.name_range, "duplicate declaration", entity, decl)
	}
	checker_add_definition(ctx.info, entity)
	append(&ctx.info.checked_entities, entity)
	return entity
}

checker_intern_suffix_name :: proc(
	ctx: ^Checker_Context,
	name: string_interner.String,
	suffix: string,
) -> string_interner.String {
	base := string_interner.load(ctx.project.interner, name)
	builder := strings.builder_make(context.temp_allocator)
	strings.write_string(&builder, base)
	strings.write_string(&builder, suffix)
	return checker_intern_name(ctx.project, strings.to_string(builder))
}
