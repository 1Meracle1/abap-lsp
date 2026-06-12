package abap_frontend_semantic2

import "src:ast"
import string_interner "src:string_interner"

import "core:strings"

Sql_Source_Info :: struct {
	name:      string_interner.String,
	alias:     string_interner.String,
	range:     Range,
	entity:    ^Entity,
	typ:       ^Type,
	row_type:  ^Type,
	structure: ^Structure,
	resolved:  bool,
	internal:  bool,
	is_dynamic: bool,
}

Sql_Source_Scope :: struct {
	sources: [dynamic]Sql_Source_Info,
}

Sql_Output_Field :: struct {
	name:  string_interner.String,
	range: Range,
	typ:   ^Type,
	field: ^Entity,
}

Sql_Query_Shape :: struct {
	fields:      [dynamic]Sql_Output_Field,
	row_type:    ^Type,
	scalar_type: ^Type,
}

Checker_Cursor_Query :: struct {
	handle: ^Entity,
	shape:  Sql_Query_Shape,
}

checker_sql_unknown_query_shape :: proc(ctx: ^Checker_Context) -> Sql_Query_Shape {
	return Sql_Query_Shape {
		row_type    = project_type_unknown(ctx.project),
		scalar_type = project_type_unknown(ctx.project),
	}
}

checker_sql_register_cursor_query :: proc(
	ctx: ^Checker_Context,
	handle: ^Entity,
	shape: Sql_Query_Shape,
) {
	if handle == nil {
		return
	}
	for &cursor_query in ctx.cursor_shapes {
		if cursor_query.handle == handle {
			cursor_query.shape = shape
			return
		}
	}
	append(&ctx.cursor_shapes, Checker_Cursor_Query{handle = handle, shape = shape})
}

checker_sql_cursor_query_shape :: proc(
	ctx: ^Checker_Context,
	handle: ^Entity,
) -> (Sql_Query_Shape, bool) {
	if handle == nil {
		return {}, false
	}
	for cursor_query in ctx.cursor_shapes {
		if cursor_query.handle == handle {
			return cursor_query.shape, true
		}
	}
	return {}, false
}

checker_check_sql_select_query :: proc(ctx: ^Checker_Context, query: ast.Select_Query_Clause) -> Sql_Query_Shape {
	sql := checker_sql_source_scope_make()
	defer delete(sql.sources)

	if query.source_clause != nil {
		checker_sql_add_select_source(ctx, &sql, query.source_clause.source, query.source_clause.alias, query.source_clause.range, false, query.source_clause.dynamic_source)
		for join in query.source_clause.joins {
			checker_sql_add_select_source(ctx, &sql, join.source, join.alias, join.source.range if join.source != nil else Range{}, true)
			checker_check_sql_expr(ctx, &sql, join.on, true)
		}
	} else {
		checker_sql_add_select_source(ctx, &sql, query.source, {}, checker_expr_range(query.source), false)
	}

	fields := checker_sql_check_projection_list(ctx, &sql, query)
	shape := checker_sql_query_shape(ctx, fields[:], query.result)
	checker_check_sql_select_result(ctx, query.result, shape)
	checker_check_sql_expr(ctx, &sql, query.where_cond, true)
	checker_check_expr(ctx, query.for_all_entries)
	checker_check_expr(ctx, query.package_size)
	checker_check_expr(ctx, query.up_to_rows)
	for set in query.set_ops {
		_ = checker_check_sql_select_query(ctx, set.query)
	}
	return shape
}

checker_check_sql_insert_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Insert_Stmt) {
	sql := checker_sql_source_scope_make()
	defer delete(sql.sources)

	source := checker_sql_add_db_source(ctx, &sql, stmt.target, stmt.db_table_name.text, stmt.db_table_name.range, stmt.dynamic_source)
	row_type := source.row_type if source != nil else project_type_unknown(ctx.project)
	if stmt.source != nil {
		value := checker_check_expr(ctx, stmt.source)
		expected := project_type_table(ctx.project, row_type, .Standard_Table) if stmt.from_table else row_type
		checker_check_assignment_compatibility(ctx, value.type, expected, checker_expr_range(stmt.source))
	}
	for assignment in stmt.assignments {
		checker_check_sql_assignment(ctx, &sql, assignment)
	}
}

checker_check_sql_update_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Update_Stmt) {
	sql := checker_sql_source_scope_make()
	defer delete(sql.sources)

	source := checker_sql_add_db_source(ctx, &sql, stmt.target, "", Range{}, stmt.dynamic_source)
	row_type := source.row_type if source != nil else project_type_unknown(ctx.project)
	if stmt.source != nil {
		value := checker_check_expr(ctx, stmt.source)
		expected := project_type_table(ctx.project, row_type, .Standard_Table) if stmt.from_table else row_type
		checker_check_assignment_compatibility(ctx, value.type, expected, checker_expr_range(stmt.source))
	}
	for assignment in stmt.assignments {
		checker_check_sql_assignment(ctx, &sql, assignment)
	}
	checker_check_sql_expr(ctx, &sql, stmt.where_cond, true)
}

checker_check_sql_modify_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Modify_Stmt) {
	sql := checker_sql_source_scope_make()
	defer delete(sql.sources)

	source := checker_sql_add_db_source(ctx, &sql, stmt.target, "", Range{}, stmt.dynamic_source)
	row_type := source.row_type if source != nil else project_type_unknown(ctx.project)
	if stmt.source != nil {
		value := checker_check_expr(ctx, stmt.source)
		expected := project_type_table(ctx.project, row_type, .Standard_Table) if stmt.from_table else row_type
		checker_check_assignment_compatibility(ctx, value.type, expected, checker_expr_range(stmt.source))
	}
	checker_check_sql_expr(ctx, &sql, stmt.where_cond, true)
}

checker_check_sql_delete_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Delete_Stmt) {
	sql := checker_sql_source_scope_make()
	defer delete(sql.sources)

	_ = checker_sql_add_db_source(ctx, &sql, stmt.target, "", Range{}, stmt.dynamic_source)
	checker_check_expr(ctx, stmt.source)
	checker_check_sql_expr(ctx, &sql, stmt.where_cond, true)
}

checker_sql_source_scope_make :: proc() -> Sql_Source_Scope {
	return Sql_Source_Scope {
		sources = make([dynamic]Sql_Source_Info, 0, 4, context.temp_allocator),
	}
}

checker_sql_add_select_source :: proc(
	ctx: ^Checker_Context,
	sql: ^Sql_Source_Scope,
	expr: ^ast.Expr,
	alias: ast.Token_Text,
	range: Range,
	is_join: bool,
	is_dynamic := false,
) -> ^Sql_Source_Info {
	_ = is_join
	if expr == nil {
		return nil
	}
	if is_dynamic || checker_sql_expr_is_dynamic_operand(expr) {
		checker_check_sql_dynamic_expr(ctx, expr)
		append(&sql.sources, Sql_Source_Info{range = range, is_dynamic = true})
		return &sql.sources[len(sql.sources) - 1]
	}
	if host, ok := expr.derived_expr.(^ast.Host_Expr); ok {
		operand := checker_check_host_expr(ctx, &expr.expr_base, host, .Value, false)
		source := Sql_Source_Info {
			alias     = checker_intern_name(ctx.project, alias.text),
			range     = expr.range,
			typ       = operand.type,
			row_type  = checker_type_row(ctx, operand.type),
			internal  = true,
			resolved  = true,
		}
		source.structure = checker_type_structure(source.row_type)
		source.name = checker_sql_simple_expr_name(ctx, host.value)
		append(&sql.sources, source)
		return &sql.sources[len(sql.sources) - 1]
	}
	name, name_range, ok := checker_sql_source_expr_name(ctx, expr)
	if !ok {
		checker_check_sql_dynamic_expr(ctx, expr)
		return nil
	}
	source := checker_sql_resolve_source(ctx, name, name_range)
	source.alias = checker_intern_name(ctx.project, alias.text)
	if range.end > range.start {
		source.range = range
	}
	append(&sql.sources, source)
	return &sql.sources[len(sql.sources) - 1]
}

checker_sql_add_db_source :: proc(
	ctx: ^Checker_Context,
	sql: ^Sql_Source_Scope,
	expr: ^ast.Expr,
	name_hint: string,
	name_hint_range: Range,
	is_dynamic := false,
) -> ^Sql_Source_Info {
	if expr == nil {
		return nil
	}
	if is_dynamic || checker_sql_expr_is_dynamic_operand(expr) {
		checker_check_sql_dynamic_expr(ctx, expr)
		append(&sql.sources, Sql_Source_Info{range = checker_expr_range(expr), is_dynamic = true})
		return &sql.sources[len(sql.sources) - 1]
	}
	name := name_hint
	name_range := name_hint_range
	if name == "" {
		ok: bool
		name, name_range, ok = checker_sql_source_expr_name(ctx, expr)
		if !ok {
			checker_check_expr(ctx, expr)
			return nil
		}
	}
	source := checker_sql_resolve_source(ctx, name, name_range)
	append(&sql.sources, source)
	return &sql.sources[len(sql.sources) - 1]
}

checker_sql_resolve_source :: proc(ctx: ^Checker_Context, name: string, range: Range) -> Sql_Source_Info {
	source := Sql_Source_Info {
		name  = checker_intern_name(ctx.project, name),
		range = range,
	}
	if !string_interner.is_valid(source.name) {
		return source
	}
	_, entity, ok := checker_lookup_reference(ctx, .Type, source.name, .DDIC_Table)
	if !ok {
		checker_add_diagnostic(ctx, .Unresolved_Open_Sql_Source, range, checker_sql_source_message(ctx, "unresolved Open SQL source ", source.name))
		checker_add_unresolved_candidate(
			ctx,
			source.name,
			.Type,
			.DDIC_Table,
			.Open_SQL_Source,
			.Unresolved_SQL_Source,
			range,
		)
		return source
	}
	checker_add_entity_use_at_range(ctx, nil, entity, range)
	checker_check_entity_for_operand(ctx, entity)
	source.entity = entity
	source.typ = entity.type if entity.type != nil else project_type_unknown(ctx.project)
	source.row_type = checker_type_row(ctx, source.typ)
	if checker_type_is_unknown(source.row_type) {
		source.row_type = source.typ
	}
	source.structure = checker_type_structure(source.row_type)
	source.resolved = true
	return source
}

checker_sql_check_projection_list :: proc(
	ctx: ^Checker_Context,
	sql: ^Sql_Source_Scope,
	query: ast.Select_Query_Clause,
) -> [dynamic]Sql_Output_Field {
	fields := make([dynamic]Sql_Output_Field, 0, 4, context.temp_allocator)
	if len(query.projection_clauses) > 0 {
		for projection in query.projection_clauses {
			checker_sql_append_projection(ctx, sql, &fields, projection.value, projection.alias, projection.range, projection.is_dynamic)
		}
		return fields
	}
	for projection in query.projections {
		checker_sql_append_projection(ctx, sql, &fields, projection, {}, checker_expr_range(projection), false)
	}
	return fields
}

checker_sql_append_projection :: proc(
	ctx: ^Checker_Context,
	sql: ^Sql_Source_Scope,
	fields: ^[dynamic]Sql_Output_Field,
	expr: ^ast.Expr,
	alias: ast.Token_Text,
	range: Range,
	is_dynamic: bool,
) {
	if expr == nil {
		return
	}
	if is_dynamic || checker_sql_expr_is_dynamic_operand(expr) {
		checker_check_sql_dynamic_expr(ctx, expr)
		return
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Sql_Star_Expr:
		checker_sql_append_star_fields(ctx, sql, fields, n.qualifier.text, n.star_range if n.star_range.end > n.star_range.start else expr.range)
		return
	}
	typ := checker_check_sql_expr(ctx, sql, expr, false)
	name, name_range := checker_sql_projection_name(ctx, expr, alias, range)
	if !string_interner.is_valid(name) {
		return
	}
	append(
		fields,
		Sql_Output_Field {
			name  = name,
			range = name_range,
			typ   = typ if typ != nil else project_type_unknown(ctx.project),
		},
	)
}

checker_sql_projection_name :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
	alias: ast.Token_Text,
	range: Range,
) -> (string_interner.String, Range) {
	if alias.text != "" {
		return checker_intern_name(ctx.project, alias.text), alias.range
	}
	if expr == nil {
		return string_interner.String(0), range
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Sql_Column_Expr:
		return checker_intern_name(ctx.project, n.name.text), n.name.range
	case ^ast.Sql_Call_Expr:
		return checker_intern_name(ctx.project, n.name.text), n.name.range
	case ^ast.Ident_Expr:
		return checker_intern_name(ctx.project, n.name), n.range
	case ^ast.Type_Ref_Expr:
		return checker_intern_name(ctx.project, n.name.text), n.name.range
	}
	return string_interner.String(0), range
}

checker_sql_append_star_fields :: proc(
	ctx: ^Checker_Context,
	sql: ^Sql_Source_Scope,
	fields: ^[dynamic]Sql_Output_Field,
	qualifier: string,
	range: Range,
) {
	if qualifier != "" {
		source, ok := checker_sql_source_for_qualifier(ctx, sql, checker_intern_name(ctx.project, qualifier))
		if !ok {
			return
		}
		checker_sql_append_source_fields(ctx, fields, source, range)
		return
	}
	for &source in sql.sources {
		checker_sql_append_source_fields(ctx, fields, &source, range)
	}
}

checker_sql_append_source_fields :: proc(
	ctx: ^Checker_Context,
	fields: ^[dynamic]Sql_Output_Field,
	source: ^Sql_Source_Info,
	range: Range,
) {
	if source == nil || source.structure == nil {
		return
	}
	for field in source.structure.fields {
		if field == nil {
			continue
		}
		checker_add_entity_use(ctx, nil, field)
		append(
			fields,
			Sql_Output_Field {
				name  = field.name,
				range = field.name_range if field.name_range.end > field.name_range.start else range,
				typ   = field.type if field.type != nil else project_type_unknown(ctx.project),
				field = field,
			},
		)
	}
}

checker_check_sql_expr :: proc(
	ctx: ^Checker_Context,
	sql: ^Sql_Source_Scope,
	expr: ^ast.Expr,
	predicate: bool,
) -> ^Type {
	if expr == nil {
		return project_type_unknown(ctx.project)
	}
	_ = predicate
	#partial switch n in expr.derived_expr {
	case ^ast.Host_Expr:
		return checker_check_host_expr(ctx, &expr.expr_base, n, .Value, false).type
	case ^ast.Sql_Column_Expr:
		field, ok := checker_sql_lookup_column(ctx, sql, n.name.text, n.qualifier.text, n.name.range)
		if ok {
			checker_add_entity_use(ctx, &expr.expr_base, field)
			return field.type if field.type != nil else project_type_unknown(ctx.project)
		}
		return project_type_unknown(ctx.project)
	case ^ast.Sql_Star_Expr:
		if n.qualifier.text != "" {
			_, _ = checker_sql_source_for_qualifier(ctx, sql, checker_intern_name(ctx.project, n.qualifier.text))
		}
		return project_type_unknown(ctx.project)
	case ^ast.Sql_Call_Expr:
		result := project_type_unknown(ctx.project)
		for arg in n.args {
			arg_type := checker_check_sql_expr(ctx, sql, arg, false)
			if checker_type_is_unknown(result) {
				result = arg_type
			}
		}
		if strings.equal_fold(n.name.text, "count") {
			return checker_builtin_type_from_name(ctx.checker, "i")
		}
		return result
	case ^ast.Sql_Case_When_Expr:
		checker_check_sql_expr(ctx, sql, n.condition, true)
		return checker_check_sql_expr(ctx, sql, n.result, false)
	case ^ast.Sql_Case_Expr:
		checker_check_sql_expr(ctx, sql, n.operand, false)
		result := project_type_unknown(ctx.project)
		for when_expr in n.whens {
			when_type := checker_check_sql_expr(ctx, sql, when_expr, false)
			if checker_type_is_unknown(result) {
				result = when_type
			}
		}
		else_type := checker_check_sql_expr(ctx, sql, n.else_expr, false)
		if checker_type_is_unknown(result) {
			result = else_type
		}
		return result
	case ^ast.Binary_Expr:
		checker_check_sql_expr(ctx, sql, n.left, true)
		checker_check_sql_expr(ctx, sql, n.right, true)
		return checker_builtin_type_from_name(ctx.checker, "abap_bool")
	case ^ast.Unary_Expr:
		return checker_check_sql_expr(ctx, sql, n.expr, predicate)
	case ^ast.Paren_Expr:
		return checker_check_sql_expr(ctx, sql, n.expr, predicate)
	case ^ast.Between_Expr:
		checker_check_sql_expr(ctx, sql, n.subject, true)
		checker_check_sql_expr(ctx, sql, n.low, true)
		checker_check_sql_expr(ctx, sql, n.high, true)
		return checker_builtin_type_from_name(ctx.checker, "abap_bool")
	case ^ast.Is_Predicate_Expr:
		checker_check_sql_expr(ctx, sql, n.subject, true)
		return checker_builtin_type_from_name(ctx.checker, "abap_bool")
	case ^ast.Instance_Of_Predicate_Expr:
		checker_check_sql_expr(ctx, sql, n.subject, true)
		checker_check_expr(ctx, n.type_ref, .Type)
		return checker_builtin_type_from_name(ctx.checker, "abap_bool")
	case ^ast.Table_Expr:
		operand := checker_check_expr(ctx, n.table)
		for selector in n.selectors {
			checker_check_expr(ctx, selector)
		}
		return checker_type_row(ctx, operand.type)
	case ^ast.Selector_Expr:
		if n.op == .Tilde {
			qualifier := checker_sql_simple_expr_name(ctx, n.base)
			name := checker_sql_simple_expr_name(ctx, n.field)
			if string_interner.is_valid(qualifier) && string_interner.is_valid(name) {
				field, ok := checker_sql_lookup_column_by_name(ctx, sql, name, qualifier, expr.range)
				if ok {
					checker_add_entity_use(ctx, &expr.expr_base, field)
					return field.type if field.type != nil else project_type_unknown(ctx.project)
				}
				return project_type_unknown(ctx.project)
			}
		}
		return checker_check_expr(ctx, expr).type
	case ^ast.Call_Expr:
		checker_check_expr(ctx, n.callee)
		checker_check_sql_expr(ctx, sql, n.args, false)
		return project_type_unknown(ctx.project)
	case ^ast.Call_Arg_List_Expr:
		for arg in n.args {
			checker_check_sql_expr(ctx, sql, arg, false)
		}
	case ^ast.Call_Named_Arg_Expr:
		return checker_check_sql_expr(ctx, sql, n.value, false)
	case ^ast.Call_Positional_Arg_Expr:
		return checker_check_sql_expr(ctx, sql, n.value, false)
	case:
		return checker_check_expr(ctx, expr).type
	}
	return project_type_unknown(ctx.project)
}

checker_sql_lookup_column :: proc(
	ctx: ^Checker_Context,
	sql: ^Sql_Source_Scope,
	name: string,
	qualifier: string,
	range: Range,
) -> (^Entity, bool) {
	return checker_sql_lookup_column_by_name(
		ctx,
		sql,
		checker_intern_name(ctx.project, name),
		checker_intern_name(ctx.project, qualifier),
		range,
	)
}

checker_sql_lookup_column_by_name :: proc(
	ctx: ^Checker_Context,
	sql: ^Sql_Source_Scope,
	name: string_interner.String,
	qualifier: string_interner.String,
	range: Range,
) -> (^Entity, bool) {
	if !string_interner.is_valid(name) {
		return nil, false
	}
	if string_interner.is_valid(qualifier) {
		source, source_ok := checker_sql_source_for_qualifier(ctx, sql, qualifier)
		if !source_ok {
			return nil, false
		}
		return checker_sql_source_field(ctx, source, name, range)
	}
	found: ^Entity
	resolved_source_count := 0
	for &source in sql.sources {
		if !source.resolved || source.structure == nil {
			continue
		}
		resolved_source_count += 1
		if field, ok := checker_sql_structure_field(source.structure, name); ok {
			if found != nil {
				return found, true
			}
			found = field
		}
	}
	if found != nil {
		return found, true
	}
	if resolved_source_count > 0 {
		checker_add_diagnostic(ctx, .Unknown_Field, range, checker_sql_source_message(ctx, "unknown Open SQL field ", name))
	}
	return nil, false
}

checker_sql_source_for_qualifier :: proc(
	ctx: ^Checker_Context,
	sql: ^Sql_Source_Scope,
	qualifier: string_interner.String,
) -> (^Sql_Source_Info, bool) {
	_ = ctx
	if !string_interner.is_valid(qualifier) {
		return nil, false
	}
	for &source in sql.sources {
		if source.alias == qualifier || source.name == qualifier {
			return &source, true
		}
	}
	return nil, false
}

checker_sql_source_field :: proc(
	ctx: ^Checker_Context,
	source: ^Sql_Source_Info,
	name: string_interner.String,
	range: Range,
) -> (^Entity, bool) {
	if source == nil || source.structure == nil {
		return nil, false
	}
	if field, ok := checker_sql_structure_field(source.structure, name); ok {
		return field, true
	}
	checker_add_diagnostic(ctx, .Unknown_Field, range, checker_sql_source_message(ctx, "unknown Open SQL field ", name))
	return nil, false
}

checker_sql_structure_field :: proc(structure: ^Structure, name: string_interner.String) -> (^Entity, bool) {
	if structure == nil {
		return nil, false
	}
	return checker_lookup_structure_field(structure, name)
}

checker_check_sql_select_result :: proc(
	ctx: ^Checker_Context,
	result: ^ast.Select_Result_Clause,
	shape: Sql_Query_Shape,
) {
	if result == nil || result.kind == .None || result.target == nil {
		return
	}
	expected := checker_sql_expected_result_type(ctx, result, shape)
	local := ctx^
	local.type_hint = expected
	local.type_hint_expr = result.target
	target := checker_check_expr(&local, result.target, .Value, true)
	if result.corresponding_fields || checker_type_is_unknown(expected) || checker_type_is_unknown(target.type) {
		return
	}
	if ok, known := checker_type_assignment_compatible(ctx, expected, target.type); known && !ok {
		checker_add_diagnostic(
			ctx,
			.Invalid_Open_Sql_Into_Target,
			checker_expr_range(result.target),
			checker_type_mismatch_message(ctx, "Open SQL target is not compatible", expected, target.type),
		)
	}
}

checker_sql_expected_result_type :: proc(
	ctx: ^Checker_Context,
	result: ^ast.Select_Result_Clause,
	shape: Sql_Query_Shape,
) -> ^Type {
	if result.table {
		row_type := shape.row_type
		if checker_type_is_unknown(row_type) {
			row_type = project_type_unknown(ctx.project)
		}
		return project_type_table(ctx.project, row_type, .Standard_Table)
	}
	if !checker_type_is_unknown(shape.scalar_type) {
		return shape.scalar_type
	}
	return shape.row_type if shape.row_type != nil else project_type_unknown(ctx.project)
}

checker_sql_query_shape :: proc(
	ctx: ^Checker_Context,
	fields: []Sql_Output_Field,
	result: ^ast.Select_Result_Clause,
) -> Sql_Query_Shape {
	shape := Sql_Query_Shape {
		fields      = make([dynamic]Sql_Output_Field, 0, len(fields), ctx.project.allocator),
		row_type    = project_type_unknown(ctx.project),
		scalar_type = project_type_unknown(ctx.project),
	}
	for field in fields {
		append(&shape.fields, field)
	}
	if len(fields) == 1 {
		shape.scalar_type = fields[0].typ if fields[0].typ != nil else project_type_unknown(ctx.project)
	}
	if len(fields) == 0 {
		return shape
	}
	if result != nil && !result.table && len(fields) == 1 {
		shape.row_type = shape.scalar_type
		return shape
	}
	shape.row_type = checker_sql_structure_type_from_fields(ctx, fields, result)
	return shape
}

checker_sql_structure_type_from_fields :: proc(
	ctx: ^Checker_Context,
	fields: []Sql_Output_Field,
	result: ^ast.Select_Result_Clause,
) -> ^Type {
	name := checker_sql_inline_structure_name(ctx, result)
	scope := checker_create_scope(ctx.checker, ctx.scope, .Structure, result.range if result != nil else Range{})
	structure := project_new_structure(ctx.project, name, ctx.file, scope, result.range if result != nil else Range{})
	for field in fields {
		checker_sql_add_structure_field(ctx, structure, field)
	}
	return project_type_structure(ctx.project, structure)
}

checker_sql_add_structure_field :: proc(
	ctx: ^Checker_Context,
	structure: ^Structure,
	field: Sql_Output_Field,
) -> ^Entity {
	entity := project_new_entity(ctx.project, .Field)
	entity.source_file = ctx.file
	entity.type = field.typ if field.typ != nil else project_type_unknown(ctx.project)
	entity.state = .Resolved
	decl := project_new_decl_info(ctx.project, entity, structure.scope, field.name, .Field, field.range, nil)
	decl.state = .Resolved
	payload, ok := entity.payload.(^Entity_Field_Payload)
	assert(ok && payload != nil)
	payload.owner_structure = structure
	payload.decl_unit = ctx.file
	payload.decl_range = field.range
	payload.field_index = i32(len(structure.fields))
	payload.flags += {.Has_Decl_Range}
	append(&structure.fields, entity)
	_ = scope_insert_declaration(structure.scope, entity)
	checker_add_definition(ctx.info, entity)
	append(&ctx.info.checked_entities, entity)
	return entity
}

checker_sql_inline_structure_name :: proc(ctx: ^Checker_Context, result: ^ast.Select_Result_Clause) -> string_interner.String {
	target_name := ""
	if result != nil {
		target_name = checker_sql_target_name(result.target)
	}
	if target_name == "" {
		return checker_intern_name(ctx.project, "<open_sql_inline>")
	}
	builder := strings.builder_make(context.temp_allocator)
	strings.write_string(&builder, "<open_sql_inline:")
	strings.write_string(&builder, target_name)
	strings.write_byte(&builder, '>')
	return checker_intern_name(ctx.project, strings.to_string(builder))
}

checker_check_sql_assignment :: proc(
	ctx: ^Checker_Context,
	sql: ^Sql_Source_Scope,
	assignment: ast.Sql_Assignment_Clause,
) {
	field_name := checker_intern_name(ctx.project, assignment.column_name.text)
	range := assignment.column_name.range
	if !string_interner.is_valid(field_name) {
		field_name = checker_sql_simple_expr_name(ctx, assignment.name)
		range = checker_expr_range(assignment.name)
	}
	field, field_ok := checker_sql_lookup_column_by_name(ctx, sql, field_name, string_interner.String(0), range)
	value_type := checker_check_sql_expr(ctx, sql, assignment.value, false)
	if field_ok {
		checker_check_assignment_compatibility(ctx, value_type, field.type, checker_expr_range(assignment.value))
	}
}

checker_sql_source_expr_name :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
) -> (string, Range, bool) {
	if expr == nil {
		return "", Range{}, false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		return n.name, n.range, n.name != ""
	case ^ast.Type_Ref_Expr:
		return n.name.text, n.name.range, n.name.text != ""
	case ^ast.Sql_Column_Expr:
		if n.qualifier.text == "" {
			return n.name.text, n.name.range, n.name.text != ""
		}
	case ^ast.Unary_Expr:
		if n.op == .Plus {
			name, _, ok := checker_sql_source_expr_name(ctx, n.expr)
			if ok {
				builder := strings.builder_make(context.temp_allocator)
				strings.write_byte(&builder, '+')
				strings.write_string(&builder, name)
				return strings.to_string(builder), expr.range, true
			}
		}
	case ^ast.Host_Expr:
		name := checker_sql_simple_expr_name(ctx, n.value)
		if string_interner.is_valid(name) {
			return string_interner.load(ctx.project.interner, name), n.value.range, true
		}
	}
	return "", Range{}, false
}

checker_sql_simple_expr_name :: proc(ctx: ^Checker_Context, expr: ^ast.Expr) -> string_interner.String {
	if expr == nil {
		return string_interner.String(0)
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		return checker_intern_name(ctx.project, n.name)
	case ^ast.Type_Ref_Expr:
		return checker_intern_name(ctx.project, n.name.text)
	case ^ast.Sql_Column_Expr:
		if n.qualifier.text == "" {
			return checker_intern_name(ctx.project, n.name.text)
		}
	case ^ast.Host_Expr:
		return checker_sql_simple_expr_name(ctx, n.value)
	}
	return string_interner.String(0)
}

checker_sql_target_name :: proc(expr: ^ast.Expr) -> string {
	if expr == nil {
		return ""
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Host_Expr:
		return checker_sql_target_name(n.value)
	case ^ast.Data_Inline_Name_Expr:
		return n.name.text
	case ^ast.Field_Symbol_Inline_Name_Expr:
		return n.name.text
	case ^ast.Ident_Expr:
		return n.name
	case ^ast.Type_Ref_Expr:
		return n.name.text
	}
	return ""
}

checker_sql_expr_is_dynamic_operand :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	_, ok := expr.derived_expr.(^ast.Paren_Expr)
	return ok
}

checker_check_sql_dynamic_expr :: proc(ctx: ^Checker_Context, expr: ^ast.Expr) {
	if expr == nil {
		return
	}
	if paren, ok := expr.derived_expr.(^ast.Paren_Expr); ok {
		checker_check_expr(ctx, paren.expr)
		return
	}
	checker_check_expr(ctx, expr)
}

checker_sql_source_message :: proc(
	ctx: ^Checker_Context,
	prefix: string,
	name: string_interner.String,
) -> string {
	if !string_interner.is_valid(name) {
		return prefix
	}
	builder := strings.builder_make(context.temp_allocator)
	strings.write_string(&builder, prefix)
	strings.write_string(&builder, string_interner.load(ctx.project.interner, name))
	return strings.to_string(builder)
}
