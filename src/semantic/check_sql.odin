package abap_frontend_semantic2

import "src:ast"

import "core:strings"

Sql_Source_Info :: struct {
	name:      string,
	alias:     string,
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
	name:  string,
	range: Range,
	typ:   ^Type,
	field: ^Entity,
}

Sql_Query_Shape :: struct {
	fields:      [dynamic]Sql_Output_Field,
	row_type:    ^Type,
	scalar_type: ^Type,
}

Sql_For_All_Entries_Info :: struct {
	present: bool,
	name:    string,
	entity:  ^Entity,
}

Sql_Dml_Kind :: enum {
	Insert,
	Update,
	Delete,
	Modify,
}

Checker_Sql_Query_Fact :: struct {
	file:            ^Project_File,
	query:           ^ast.Select_Query_Clause,
	shape:           Sql_Query_Shape,
	sources:         [dynamic]Sql_Source_Info,
	for_all_entries: Sql_For_All_Entries_Info,
}

Checker_Sql_Dml_Fact :: struct {
	file:             ^Project_File,
	stmt:             ^ast.Stmt,
	kind:             Sql_Dml_Kind,
	source:           Sql_Source_Info,
	assignment_count: int,
	from_table:       bool,
	dynamic_where:    bool,
}

Checker_Cursor_Query :: struct {
	file:   ^Project_File,
	handle: ^Entity,
	shape:  Sql_Query_Shape,
}

OPEN_SQL_REQUIRED_GROUP_BY_MESSAGE :: "Open SQL SELECT with aggregate expressions must use GROUP BY for unaggregated fields"
OPEN_SQL_INTERNAL_TABLE_WHERE_HOST_MESSAGE :: "Open SQL WHERE can reference an internal table row field only with FOR ALL ENTRIES IN the same table"
OPEN_SQL_ORDER_BY_FIELD_NOT_SELECTED_MESSAGE :: "Open SQL ORDER BY field must be part of the SELECT list"

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
	found := false
	for &cursor_query in ctx.cursor_shapes {
		if cursor_query.handle == handle {
			cursor_query.shape = shape
			cursor_query.file = ctx.file
			found = true
			break
		}
	}
	if !found {
		append(&ctx.cursor_shapes, Checker_Cursor_Query{file = ctx.file, handle = handle, shape = shape})
	}
	checker_sql_record_cursor_query(ctx, handle, shape)
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

checker_sql_record_cursor_query :: proc(
	ctx: ^Checker_Context,
	handle: ^Entity,
	shape: Sql_Query_Shape,
) {
	assert(ctx != nil && ctx.info != nil)
	if handle == nil {
		return
	}
	for &record in ctx.info.sql_cursor_queries {
		if record.file == ctx.file && record.handle == handle {
			record.shape = shape
			return
		}
	}
	append(
		&ctx.info.sql_cursor_queries,
		Checker_Cursor_Query {
			file   = ctx.file,
			handle = handle,
			shape  = shape,
		},
	)
}

checker_sql_record_query_fact :: proc(
	ctx: ^Checker_Context,
	query: ^ast.Select_Query_Clause,
	shape: Sql_Query_Shape,
	sources: []Sql_Source_Info,
	for_all_entries: Sql_For_All_Entries_Info,
) {
	assert(ctx != nil && ctx.info != nil && query != nil)
	copied_sources := make([dynamic]Sql_Source_Info, 0, len(sources), ctx.project.allocator)
	for source in sources {
		append(&copied_sources, source)
	}
	for &record in ctx.info.sql_queries {
		if record.file == ctx.file && record.query == query {
			record.shape = shape
			record.sources = copied_sources
			record.for_all_entries = for_all_entries
			return
		}
	}
	append(
		&ctx.info.sql_queries,
		Checker_Sql_Query_Fact {
			file            = ctx.file,
			query           = query,
			shape           = shape,
			sources         = copied_sources,
			for_all_entries = for_all_entries,
		},
	)
}

checker_sql_record_dml_fact :: proc(
	ctx: ^Checker_Context,
	stmt: ^ast.Stmt,
	kind: Sql_Dml_Kind,
	source: ^Sql_Source_Info,
	assignment_count: int = 0,
	from_table: bool = false,
	dynamic_where: bool = false,
) {
	assert(ctx != nil && ctx.info != nil && stmt != nil)
	source_value := source^ if source != nil else Sql_Source_Info{}
	for &record in ctx.info.sql_dml {
		if record.file == ctx.file && record.stmt == stmt {
			record.kind = kind
			record.source = source_value
			record.assignment_count = assignment_count
			record.from_table = from_table
			record.dynamic_where = dynamic_where
			return
		}
	}
	append(
		&ctx.info.sql_dml,
		Checker_Sql_Dml_Fact {
			file             = ctx.file,
			stmt             = stmt,
			kind             = kind,
			source           = source_value,
			assignment_count = assignment_count,
			from_table       = from_table,
			dynamic_where    = dynamic_where,
		},
	)
}

checker_check_sql_select_query :: proc(ctx: ^Checker_Context, query: ^ast.Select_Query_Clause) -> Sql_Query_Shape {
	assert(query != nil)
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

	fields := checker_sql_check_projection_list(ctx, &sql, query^)
	checker_check_sql_order_by(ctx, query^, fields[:])
	checker_check_sql_group_by(ctx, &sql, query^)
	shape := checker_sql_query_shape(ctx, fields[:], query.result)
	checker_check_sql_select_result(ctx, query.result, shape)
	for_all_entries := checker_check_sql_for_all_entries(ctx, query.for_all_entries)
	checker_check_sql_expr(ctx, &sql, query.where_cond, !query.dynamic_where, validate_where_hosts = true, for_all_entries = for_all_entries)
	if query.package_size != nil {
		package_size := checker_check_expr(ctx, query.package_size)
		if !checker_type_is_unknown(package_size.type) {
			name, ok := checker_type_builtin_name(ctx, package_size.type)
			if checker_type_structure(package_size.type) != nil ||
			   checker_type_is_table_like(ctx, package_size.type) ||
			   checker_type_is_ref(package_size.type) ||
			   ok && name != "int1" && name != "int2" && name != "i" && name != "int4" && name != "int8" {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					checker_expr_range(query.package_size),
					"Open SQL PACKAGE SIZE operand is not an integer row count",
				)
			}
		}
	}
	if query.up_to_rows != nil {
		row_limit := checker_check_expr(ctx, query.up_to_rows)
		if !checker_type_is_unknown(row_limit.type) {
			name, ok := checker_type_builtin_name(ctx, row_limit.type)
			if checker_type_structure(row_limit.type) != nil ||
			   checker_type_is_table_like(ctx, row_limit.type) ||
			   checker_type_is_ref(row_limit.type) ||
			   ok && name != "int1" && name != "int2" && name != "i" && name != "int4" && name != "int8" {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					checker_expr_range(query.up_to_rows),
					"Open SQL UP TO operand is not an integer row count",
				)
			}
		}
	}
	checker_sql_record_query_fact(ctx, query, shape, sql.sources[:], for_all_entries)
	for _, i in query.set_ops {
		checker_check_sql_select_query(ctx, &query.set_ops[i].query)
	}
	return shape
}

checker_check_sql_for_all_entries :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
) -> Sql_For_All_Entries_Info {
	info := Sql_For_All_Entries_Info{present = expr != nil}
	if expr == nil {
		return info
	}
	operand := checker_check_expr(ctx, expr)
	info.name = checker_sql_simple_expr_name(ctx, expr)
	info.entity = operand.entity
	return info
}

checker_check_sql_insert_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Insert_Stmt) {
	sql := checker_sql_source_scope_make()
	defer delete(sql.sources)

	source := checker_sql_add_db_source(ctx, &sql, stmt.target, stmt.db_table_name.text, stmt.db_table_name.range, stmt.dynamic_source)
	checker_sql_record_dml_fact(ctx, &stmt.node, .Insert, source, len(stmt.assignments), stmt.from_table)
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
	checker_sql_record_dml_fact(ctx, &stmt.node, .Update, source, len(stmt.assignments), stmt.from_table, stmt.dynamic_where)
	row_type := source.row_type if source != nil else project_type_unknown(ctx.project)
	if stmt.source != nil {
		value := checker_check_expr(ctx, stmt.source)
		expected := project_type_table(ctx.project, row_type, .Standard_Table) if stmt.from_table else row_type
		checker_check_assignment_compatibility(ctx, value.type, expected, checker_expr_range(stmt.source))
	}
	for assignment in stmt.assignments {
		checker_check_sql_assignment(ctx, &sql, assignment)
	}
	checker_check_sql_expr(ctx, &sql, stmt.where_cond, !stmt.dynamic_where)
}

checker_check_sql_modify_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Modify_Stmt) {
	sql := checker_sql_source_scope_make()
	defer delete(sql.sources)

	source := checker_sql_add_db_source(ctx, &sql, stmt.target, "", Range{}, stmt.dynamic_source)
	checker_sql_record_dml_fact(ctx, &stmt.node, .Modify, source, 0, stmt.from_table, stmt.dynamic_where)
	row_type := source.row_type if source != nil else project_type_unknown(ctx.project)
	if stmt.source != nil {
		value := checker_check_expr(ctx, stmt.source)
		expected := project_type_table(ctx.project, row_type, .Standard_Table) if stmt.from_table else row_type
		checker_check_assignment_compatibility(ctx, value.type, expected, checker_expr_range(stmt.source))
	}
	checker_check_sql_expr(ctx, &sql, stmt.where_cond, !stmt.dynamic_where)
}

checker_check_sql_delete_stmt :: proc(ctx: ^Checker_Context, stmt: ^ast.Delete_Stmt) {
	sql := checker_sql_source_scope_make()
	defer delete(sql.sources)

	source := checker_sql_add_db_source(ctx, &sql, stmt.target, "", Range{}, stmt.dynamic_source)
	checker_sql_record_dml_fact(ctx, &stmt.node, .Delete, source, 0, stmt.from_table, stmt.dynamic_where)
	checker_check_expr_with_unresolved_value_diagnostics(ctx, stmt.source)
	checker_check_sql_expr(ctx, &sql, stmt.where_cond, !stmt.dynamic_where)
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
			alias     = project_intern_lower_ascii(ctx.project, alias.text),
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
	source.alias = project_intern_lower_ascii(ctx.project, alias.text)
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
		name  = project_intern_lower_ascii(ctx.project, name),
		range = range,
	}
	if source.name == "" {
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

checker_check_sql_order_by :: proc(
	ctx: ^Checker_Context,
	query: ast.Select_Query_Clause,
	fields: []Sql_Output_Field,
) {
	if query.order_by_primary_key || len(query.order_by_fields) == 0 || len(fields) == 0 {
		return
	}
	for field in query.order_by_fields {
		name := project_intern_lower_ascii(ctx.project, field.text)
		if name == "" || checker_sql_output_field_present(fields, name) {
			continue
		}
		checker_add_diagnostic(
			ctx,
			.Invalid_Open_Sql_Order_By,
			field.range,
			OPEN_SQL_ORDER_BY_FIELD_NOT_SELECTED_MESSAGE,
			severity = .Warning,
		)
	}
}

checker_sql_output_field_present :: proc(fields: []Sql_Output_Field, name: string) -> bool {
	if name == "" {
		return false
	}
	for field in fields {
		if field.name == name {
			return true
		}
	}
	return false
}

checker_check_sql_group_by :: proc(
	ctx: ^Checker_Context,
	sql: ^Sql_Source_Scope,
	query: ast.Select_Query_Clause,
) {
	for group_expr in query.group_by {
		if group_expr.value == nil {
			continue
		}
		if group_expr.is_dynamic {
			checker_check_sql_dynamic_expr(ctx, group_expr.value)
		} else {
			typ := checker_check_sql_expr(ctx, sql, group_expr.value, false)
			if !checker_type_resolves_to_unknown(typ) &&
			   (checker_type_structure(typ) != nil ||
			    checker_type_is_table_like(ctx, typ) ||
			    checker_type_is_ref(typ)) {
				checker_add_diagnostic(
					ctx,
					.Invalid_Open_Sql_Group_By,
					checker_expr_range(group_expr.value),
					"Open SQL GROUP BY expression is not scalar",
				)
			}
			visitor := ast.Visitor {
				visit = checker_sql_group_by_aggregate_visit,
				data  = rawptr(ctx),
			}
			ast.walk(&visitor, group_expr.value)
		}
	}
	if len(query.group_by) > 0 {
		return
	}
	if field_range, ok := checker_sql_required_group_by_field(query); ok {
		checker_add_diagnostic(
			ctx,
			.Invalid_Open_Sql_Group_By,
			field_range,
			OPEN_SQL_REQUIRED_GROUP_BY_MESSAGE,
		)
	}
}

checker_sql_group_by_aggregate_visit :: proc(v: ^ast.Visitor, node: ^ast.Node) -> ^ast.Visitor {
	if node == nil {
		return v
	}
	call, ok := node.derived.(^ast.Sql_Call_Expr)
	if ok && call.kind == .Aggregate {
		ctx := (^Checker_Context)(v.data)
		checker_add_diagnostic(
			ctx,
			.Invalid_Open_Sql_Group_By,
			node.range,
			"Open SQL GROUP BY cannot contain aggregate expressions",
		)
	}
	return v
}

checker_sql_required_group_by_field :: proc(query: ast.Select_Query_Clause) -> (Range, bool) {
	has_aggregate := false
	field_range := Range{}
	field_ok := false

	if len(query.projection_clauses) > 0 {
		for projection in query.projection_clauses {
			if projection.is_dynamic {
				continue
			}
			if checker_sql_expr_has_aggregate(projection.value) {
				has_aggregate = true
			}
			if !field_ok {
				field_range, field_ok = checker_sql_unaggregated_field_range(projection.value)
			}
		}
	} else {
		for projection in query.projections {
			if checker_sql_expr_has_aggregate(projection) {
				has_aggregate = true
			}
			if !field_ok {
				field_range, field_ok = checker_sql_unaggregated_field_range(projection)
			}
		}
	}

	return field_range, has_aggregate && field_ok
}

checker_sql_expr_has_aggregate :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	has_aggregate := false
	visitor := ast.Visitor {
		visit = checker_sql_aggregate_visit,
		data  = rawptr(&has_aggregate),
	}
	ast.walk(&visitor, expr)
	return has_aggregate
}

checker_sql_aggregate_visit :: proc(v: ^ast.Visitor, node: ^ast.Node) -> ^ast.Visitor {
	if node == nil {
		return v
	}
	has_aggregate := (^bool)(v.data)
	if has_aggregate^ {
		return nil
	}
	call, ok := node.derived.(^ast.Sql_Call_Expr)
	if ok && call.kind == .Aggregate {
		has_aggregate^ = true
		return nil
	}
	return v
}

Sql_Unaggregated_Field_Search :: struct {
	range: Range,
	found: bool,
}

checker_sql_unaggregated_field_range :: proc(expr: ^ast.Expr) -> (Range, bool) {
	if expr == nil {
		return {}, false
	}
	search := Sql_Unaggregated_Field_Search{}
	visitor := ast.Visitor {
		visit = checker_sql_unaggregated_field_visit,
		data  = rawptr(&search),
	}
	ast.walk(&visitor, expr)
	return search.range, search.found
}

checker_sql_unaggregated_field_visit :: proc(v: ^ast.Visitor, node: ^ast.Node) -> ^ast.Visitor {
	if node == nil {
		return v
	}
	search := (^Sql_Unaggregated_Field_Search)(v.data)
	if search.found {
		return nil
	}
	#partial switch n in node.derived {
	case ^ast.Sql_Column_Expr:
		search.range = n.name.range if n.name.range.end > n.name.range.start else node.range
		search.found = true
		return nil
	case ^ast.Sql_Star_Expr:
		search.range = n.star_range if n.star_range.end > n.star_range.start else node.range
		search.found = true
		return nil
	case ^ast.Sql_Call_Expr:
		if n.kind == .Aggregate {
			return nil
		}
	case ^ast.Host_Expr:
		return nil
	case ^ast.Selector_Expr:
		if n.op == .Tilde {
			search.range = node.range
			search.found = true
			return nil
		}
	}
	return v
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
	if !checker_type_resolves_to_unknown(typ) &&
	   (checker_type_structure(typ) != nil ||
	    checker_type_is_table_like(ctx, typ) ||
	    checker_type_is_ref(typ)) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			checker_expr_range(expr),
			"Open SQL projection expression is not scalar",
		)
	}
	name, name_range := checker_sql_projection_name(ctx, expr, alias, range)
	if name == "" {
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
) -> (string, Range) {
	if alias.text != "" {
		return project_intern_lower_ascii(ctx.project, alias.text), alias.range
	}
	if expr == nil {
		return "", range
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Sql_Column_Expr:
		return project_intern_lower_ascii(ctx.project, n.name.text), n.name.range
	case ^ast.Sql_Call_Expr:
		return project_intern_lower_ascii(ctx.project, n.name.text), n.name.range
	case ^ast.Ident_Expr:
		return project_intern_lower_ascii(ctx.project, n.name), n.range
	case ^ast.Type_Ref_Expr:
		return project_intern_lower_ascii(ctx.project, n.name.text), n.name.range
	}
	return "", range
}

checker_sql_append_star_fields :: proc(
	ctx: ^Checker_Context,
	sql: ^Sql_Source_Scope,
	fields: ^[dynamic]Sql_Output_Field,
	qualifier: string,
	range: Range,
) {
	if qualifier != "" {
		source, ok := checker_sql_source_for_qualifier(ctx, sql, project_intern_lower_ascii(ctx.project, qualifier))
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
	validate_where_hosts := false,
	for_all_entries: Sql_For_All_Entries_Info = {},
) -> ^Type {
	typ := checker_check_sql_expr_type(ctx, sql, expr, validate_where_hosts, for_all_entries)
	if predicate {
		checker_validate_logical_condition_type(ctx, expr, typ, "SQL")
	}
	return typ
}

checker_check_sql_arithmetic_operand :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, typ: ^Type) {
	if checker_type_is_unknown(typ) {
		return
	}
	name, ok := checker_type_builtin_name(ctx, typ)
	if checker_type_structure(typ) != nil ||
	   checker_type_is_table_like(ctx, typ) ||
	   checker_type_is_ref(typ) ||
	   ok && checker_scalar_group(name) != .Numeric && name != "numeric" {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			checker_expr_range(expr),
			"SQL arithmetic operand is not numeric",
		)
	}
}

checker_check_sql_concatenate_operand :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, typ: ^Type) {
	if ok, known := checker_character_like_type_supported(ctx, typ); known && !ok {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			checker_expr_range(expr),
			"SQL concatenation operand is not character-like",
		)
	}
}

checker_check_sql_like_operand :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, typ: ^Type) {
	if ok, known := checker_character_like_type_supported(ctx, typ); known && !ok {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			checker_expr_range(expr),
			"SQL LIKE operand is not character-like",
		)
	}
}

checker_check_sql_in_collection :: proc(ctx: ^Checker_Context, expr: ^ast.Expr, typ: ^Type) -> ^Type {
	if checker_type_resolves_to_unknown(typ) {
		return project_type_unknown(ctx.project)
	}
	if !checker_type_is_table_like(ctx, typ) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Syntax_Form,
			checker_expr_range(expr),
			"SQL IN operand is not an internal table",
		)
		return project_type_unknown(ctx.project)
	}
	row_type := checker_type_row(ctx, typ)
	if structure := checker_type_structure(row_type); structure != nil {
		if low, ok := checker_lookup_structure_field(structure, project_intern_lower_ascii(ctx.project, "low")); ok {
			return low.type
		}
	}
	return row_type
}

checker_check_sql_expr_type :: proc(
	ctx: ^Checker_Context,
	sql: ^Sql_Source_Scope,
	expr: ^ast.Expr,
	validate_where_hosts := false,
	for_all_entries: Sql_For_All_Entries_Info = {},
	allow_star := false,
) -> ^Type {
	if expr == nil {
		return project_type_unknown(ctx.project)
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Host_Expr:
		operand := checker_check_host_expr(ctx, &expr.expr_base, n, .Value, false)
		if validate_where_hosts {
			checker_check_sql_where_host_expr(ctx, n, for_all_entries)
		}
		return operand.type
	case ^ast.Sql_Column_Expr:
		field, ok := checker_sql_lookup_column(ctx, sql, n.name.text, n.qualifier.text, n.name.range)
		if ok {
			checker_add_entity_use(ctx, &expr.expr_base, field)
			return field.type if field.type != nil else project_type_unknown(ctx.project)
		}
		return project_type_unknown(ctx.project)
	case ^ast.Sql_Star_Expr:
		if n.qualifier.text != "" {
			checker_sql_source_for_qualifier(ctx, sql, project_intern_lower_ascii(ctx.project, n.qualifier.text))
		}
		if !allow_star {
			checker_add_diagnostic(
				ctx,
				.Invalid_Syntax_Form,
				n.star_range if n.star_range.end > n.star_range.start else expr.range,
				"SQL star is only valid as a projection or COUNT(*) argument",
			)
		}
		return project_type_unknown(ctx.project)
	case ^ast.Sql_Call_Expr:
		result := project_type_unknown(ctx.project)
		for arg in n.args {
			_, star_arg := arg.derived_expr.(^ast.Sql_Star_Expr)
			count_star := n.kind == .Aggregate &&
			               strings.equal_fold(n.name.text, "count") &&
			               n.modifier == .None &&
			               len(n.args) == 1 &&
			               star_arg
			arg_type := checker_check_sql_expr_type(
				ctx,
				sql,
				arg,
				validate_where_hosts,
				for_all_entries,
				allow_star = count_star,
			)
			if n.kind == .Function &&
			   !checker_type_resolves_to_unknown(arg_type) &&
			   (checker_type_structure(arg_type) != nil ||
			    checker_type_is_table_like(ctx, arg_type) ||
			    checker_type_is_ref(arg_type)) {
				checker_add_diagnostic(
					ctx,
					.Invalid_Syntax_Form,
					checker_expr_range(arg),
					"SQL function argument is not scalar",
				)
			}
			if checker_type_is_unknown(result) {
				result = arg_type
			} else if strings.equal_fold(n.name.text, "coalesce") {
				checker_check_branch_result_compatibility(
					ctx,
					arg_type,
					result,
					checker_expr_range(arg),
					"SQL COALESCE operand is not compatible",
				)
			}
		}
		if strings.equal_fold(n.name.text, "count") {
			return checker_builtin_type_from_name(ctx.checker, "i")
		}
		return result
	case ^ast.Sql_Case_When_Expr:
		condition_type := checker_check_sql_expr_type(ctx, sql, n.condition, validate_where_hosts, for_all_entries)
		checker_validate_logical_condition_type(ctx, n.condition, condition_type, "SQL CASE WHEN")
		return checker_check_sql_expr_type(ctx, sql, n.result, validate_where_hosts, for_all_entries)
	case ^ast.Sql_Case_Expr:
		selector_type := checker_check_sql_expr_type(ctx, sql, n.operand, validate_where_hosts, for_all_entries)
		result := project_type_unknown(ctx.project)
		for when_expr in n.whens {
			arm := when_expr.derived_expr.(^ast.Sql_Case_When_Expr)
			condition_type := checker_check_sql_expr_type(ctx, sql, arm.condition, validate_where_hosts, for_all_entries)
			if n.operand == nil {
				checker_validate_logical_condition_type(ctx, arm.condition, condition_type, "SQL CASE WHEN")
			} else if ok, known := checker_type_assignment_compatible(ctx, condition_type, selector_type); known && !ok {
				checker_add_diagnostic(
					ctx,
					.Incompatible_Argument_Type,
					checker_expr_range(arm.condition),
					checker_type_mismatch_message(
						ctx,
						"SQL CASE WHEN operand is not compatible",
						condition_type,
						selector_type,
					),
				)
			}
			when_type := checker_check_sql_expr_type(ctx, sql, arm.result, validate_where_hosts, for_all_entries)
			if checker_type_is_unknown(result) {
				result = when_type
			} else {
				checker_check_branch_result_compatibility(
					ctx,
					when_type,
					result,
					checker_expr_range(arm.result),
					"SQL CASE branch result is not compatible",
				)
			}
		}
		else_type := checker_check_sql_expr_type(ctx, sql, n.else_expr, validate_where_hosts, for_all_entries)
		if checker_type_is_unknown(result) {
			result = else_type
		} else {
			checker_check_branch_result_compatibility(
				ctx,
				else_type,
				result,
				checker_expr_range(n.else_expr),
				"SQL CASE branch result is not compatible",
			)
		}
		return result
	case ^ast.Binary_Expr:
		left_type := checker_check_sql_expr_type(ctx, sql, n.left, validate_where_hosts, for_all_entries)
		right_type := checker_check_sql_expr_type(ctx, sql, n.right, validate_where_hosts, for_all_entries)
		switch n.op {
		case .Add, .Subtract, .Multiply, .Divide, .Integer_Divide, .Modulo:
			checker_check_sql_arithmetic_operand(ctx, n.left, left_type)
			checker_check_sql_arithmetic_operand(ctx, n.right, right_type)
			return checker_binary_result_type(
				ctx,
				n.op,
				Operand{type = left_type},
				Operand{type = right_type},
			)
		case .Equal, .Not_Equal, .Less, .Less_Equal, .Greater, .Greater_Equal:
			checker_check_branch_result_compatibility(
				ctx,
				right_type,
				left_type,
				checker_expr_range(n.right),
				"SQL comparison operand is not compatible",
			)
		case .And, .Or:
			operator := "SQL AND" if n.op == .And else "SQL OR"
			checker_validate_logical_condition_type(ctx, n.left, left_type, operator)
			checker_validate_logical_condition_type(ctx, n.right, right_type, operator)
		case .Concatenate:
			checker_check_sql_concatenate_operand(ctx, n.left, left_type)
			checker_check_sql_concatenate_operand(ctx, n.right, right_type)
			return checker_binary_result_type(
				ctx,
				n.op,
				Operand{type = left_type},
				Operand{type = right_type},
			)
		case .Like, .Not_Like:
			checker_check_sql_like_operand(ctx, n.left, left_type)
			checker_check_sql_like_operand(ctx, n.right, right_type)
		case .In, .Not_In:
			if raw, ok := n.right.derived_expr.(^ast.Type_Ref_Expr); !ok || !raw.raw_operand {
				item_type := checker_check_sql_in_collection(ctx, n.right, right_type)
				checker_check_branch_result_compatibility(
					ctx,
					item_type,
					left_type,
					checker_expr_range(n.right),
					"SQL IN table row is not compatible",
				)
			}
		case .Contains_Only, .Contains_Not_Only, .Contains_Any,
		     .Contains_Not_Any, .Contains_String, .Contains_No_String,
		     .Covers_Pattern, .Covers_No_Pattern, .Bit_And,
		     .Bit_Or, .Bit_Xor, .Bit_O, .Bit_Z, .Bit_M, .Is,
		     .Between:
		}
		return checker_builtin_type_from_name(ctx.checker, "abap_bool")
	case ^ast.Unary_Expr:
		operand_type := checker_check_sql_expr_type(ctx, sql, n.expr, validate_where_hosts, for_all_entries)
		switch n.op {
		case .Plus, .Minus:
			checker_check_sql_arithmetic_operand(ctx, n.expr, operand_type)
		case .Not:
			checker_validate_logical_condition_type(ctx, n.expr, operand_type, "SQL NOT")
			return checker_builtin_type_from_name(ctx.checker, "abap_bool")
		}
		return operand_type
	case ^ast.Paren_Expr:
		return checker_check_sql_expr_type(ctx, sql, n.expr, validate_where_hosts, for_all_entries)
	case ^ast.Between_Expr:
		subject_type := checker_check_sql_expr_type(ctx, sql, n.subject, validate_where_hosts, for_all_entries)
		low_type := checker_check_sql_expr_type(ctx, sql, n.low, validate_where_hosts, for_all_entries)
		checker_check_branch_result_compatibility(
			ctx,
			low_type,
			subject_type,
			checker_expr_range(n.low),
			"SQL BETWEEN bound is not compatible",
		)
		high_type := checker_check_sql_expr_type(ctx, sql, n.high, validate_where_hosts, for_all_entries)
		checker_check_branch_result_compatibility(
			ctx,
			high_type,
			subject_type,
			checker_expr_range(n.high),
			"SQL BETWEEN bound is not compatible",
		)
		return checker_builtin_type_from_name(ctx.checker, "abap_bool")
	case ^ast.Is_Predicate_Expr:
		subject_type := checker_check_sql_expr_type(ctx, sql, n.subject, validate_where_hosts, for_all_entries)
		if n.kind == .Null &&
		   !checker_type_resolves_to_unknown(subject_type) &&
		   (checker_type_structure(subject_type) != nil ||
		    checker_type_is_table_like(ctx, subject_type) ||
		    checker_type_is_ref(subject_type)) {
			checker_add_diagnostic(
				ctx,
				.Invalid_Syntax_Form,
				checker_expr_range(n.subject),
				"SQL IS NULL subject is not scalar",
			)
		}
		return checker_builtin_type_from_name(ctx.checker, "abap_bool")
	case ^ast.Instance_Of_Predicate_Expr:
		checker_check_sql_expr_type(ctx, sql, n.subject, validate_where_hosts, for_all_entries)
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
			if qualifier != "" && name != "" {
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
		checker_check_sql_expr_type(ctx, sql, n.args, validate_where_hosts, for_all_entries)
		return project_type_unknown(ctx.project)
	case ^ast.Call_Arg_List_Expr:
		for arg in n.args {
			checker_check_sql_expr_type(ctx, sql, arg, validate_where_hosts, for_all_entries)
		}
	case ^ast.Call_Named_Arg_Expr:
		return checker_check_sql_expr_type(ctx, sql, n.value, validate_where_hosts, for_all_entries)
	case ^ast.Call_Positional_Arg_Expr:
		return checker_check_sql_expr_type(ctx, sql, n.value, validate_where_hosts, for_all_entries)
	case:
		return checker_check_expr(ctx, expr).type
	}
	return project_type_unknown(ctx.project)
}

checker_check_sql_where_host_expr :: proc(
	ctx: ^Checker_Context,
	host: ^ast.Host_Expr,
	for_all_entries: Sql_For_All_Entries_Info,
) {
	if host == nil {
		return
	}
	base, base_name, ok := checker_sql_for_all_entries_like_base(ctx, host.value)
	if !ok {
		return
	}
	if !checker_type_is_table_like(ctx, base.type) {
		return
	}
	if checker_sql_where_host_matches_for_all_entries(base, base_name, for_all_entries) {
		return
	}
	checker_add_diagnostic(
		ctx,
		.Invalid_Open_Sql_Where_Operand,
		host.range,
		OPEN_SQL_INTERNAL_TABLE_WHERE_HOST_MESSAGE,
	)
}

checker_sql_where_host_matches_for_all_entries :: proc(
	base: Operand,
	base_name: string,
	for_all_entries: Sql_For_All_Entries_Info,
) -> bool {
	if !for_all_entries.present {
		return false
	}
	if for_all_entries.entity != nil && base.entity == for_all_entries.entity {
		return true
	}
	return base_name != "" && base_name == for_all_entries.name
}

checker_sql_for_all_entries_like_base :: proc(
	ctx: ^Checker_Context,
	expr: ^ast.Expr,
) -> (
	Operand,
	string,
	bool,
) {
	if expr == nil {
		return checker_invalid_operand(), "", false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Paren_Expr:
		return checker_sql_for_all_entries_like_base(ctx, n.expr)
	case ^ast.Substring_Expr:
		return checker_sql_for_all_entries_like_base(ctx, n.base)
	case ^ast.Selector_Expr:
		if n.op != .Dash {
			return checker_invalid_operand(), "", false
		}
		base_expr := checker_sql_leftmost_dash_base_expr(n.base)
		if base_expr == nil {
			return checker_invalid_operand(), "", false
		}
		base := checker_check_expr(ctx, base_expr)
		return base, checker_sql_simple_expr_name(ctx, base_expr), true
	case ^ast.Type_Ref_Expr:
		if n.raw_operand || len(n.path) == 0 || n.path[0].selector != .Dash {
			return checker_invalid_operand(), "", false
		}
		name := n.base_name
		if name.text == "" {
			name = n.name
		}
		if name.text == "" {
			return checker_invalid_operand(), "", false
		}
		base := checker_sql_value_operand(ctx, name)
		return base, project_intern_lower_ascii(ctx.project, name.text), true
	}
	return checker_invalid_operand(), "", false
}

checker_sql_leftmost_dash_base_expr :: proc(expr: ^ast.Expr) -> ^ast.Expr {
	if expr == nil {
		return nil
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Paren_Expr:
		return checker_sql_leftmost_dash_base_expr(n.expr)
	case ^ast.Selector_Expr:
		if n.op == .Dash {
			return checker_sql_leftmost_dash_base_expr(n.base)
		}
	}
	return expr
}

checker_sql_value_operand :: proc(ctx: ^Checker_Context, name: ast.Token_Text) -> Operand {
	interned := project_intern_lower_ascii(ctx.project, name.text)
	if interned == "" {
		return checker_invalid_operand()
	}
	_, entity, ok := checker_lookup_reference(ctx, .Value, interned)
	if !ok {
		return Operand{mode = .Value, type = project_type_unknown(ctx.project)}
	}
	checker_check_entity_for_operand(ctx, entity)
	typ := entity.type if entity.type != nil else project_type_unknown(ctx.project)
	return Operand {
		mode   = checker_addressing_mode_for_entity(entity),
		type   = typ,
		entity = entity,
	}
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
		project_intern_lower_ascii(ctx.project, name),
		project_intern_lower_ascii(ctx.project, qualifier),
		range,
	)
}

checker_sql_lookup_column_by_name :: proc(
	ctx: ^Checker_Context,
	sql: ^Sql_Source_Scope,
	name: string,
	qualifier: string,
	range: Range,
) -> (^Entity, bool) {
	if name == "" {
		return nil, false
	}
	if qualifier != "" {
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
	qualifier: string,
) -> (^Sql_Source_Info, bool) {
	_ = ctx
	if qualifier == "" {
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
	name: string,
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

checker_sql_structure_field :: proc(structure: ^Structure, name: string) -> (^Entity, bool) {
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
	if !checker_check_unresolved_variable_operand(ctx, result.target, target) &&
	   !checker_type_is_unknown(target.type) &&
	   !checker_operand_is_writable(target) {
		checker_add_diagnostic(
			ctx,
			.Invalid_Open_Sql_Into_Target,
			checker_expr_range(result.target),
			"Open SQL target is not writable",
		)
	}
	if result.corresponding_fields || checker_type_is_unknown(expected) || checker_type_is_unknown(target.type) {
		return
	}
	checker_check_sql_select_result_field_count(ctx, result, target.type, shape)
	if ok, known := checker_type_assignment_compatible(ctx, expected, target.type); known && !ok {
		checker_add_diagnostic(
			ctx,
			.Invalid_Open_Sql_Into_Target,
			checker_expr_range(result.target),
			checker_type_mismatch_message(ctx, "Open SQL target is not compatible", expected, target.type),
		)
	}
}

checker_check_sql_select_result_field_count :: proc(
	ctx: ^Checker_Context,
	result: ^ast.Select_Result_Clause,
	target_type: ^Type,
	shape: Sql_Query_Shape,
) {
	if result == nil || result.corresponding_fields || len(shape.fields) <= 1 {
		return
	}
	capacity, ok := checker_sql_select_target_field_capacity(ctx, result, target_type)
	if !ok || len(shape.fields) <= capacity {
		return
	}
	range := checker_expr_range(result.target)
	if capacity >= 0 && capacity < len(shape.fields) {
		field_range := shape.fields[capacity].range
		if field_range.end > field_range.start {
			range = field_range
		}
	}
	checker_add_diagnostic(
		ctx,
		.Invalid_Open_Sql_Into_Target,
		range,
		checker_sql_field_count_message(len(shape.fields), capacity),
	)
}

checker_sql_select_target_field_capacity :: proc(
	ctx: ^Checker_Context,
	result: ^ast.Select_Result_Clause,
	target_type: ^Type,
) -> (int, bool) {
	if result == nil || checker_type_is_unknown(target_type) {
		return 0, false
	}
	if result.table {
		if !checker_type_is_table_like(ctx, target_type) {
			return 0, false
		}
		row_type := checker_type_row(ctx, target_type)
		if checker_type_is_unknown(row_type) {
			return 0, false
		}
		if structure := checker_type_structure(row_type); structure != nil {
			return len(structure.fields), true
		}
		return 1, true
	}
	if structure := checker_type_structure(target_type); structure != nil {
		return len(structure.fields), true
	}
	return 1, true
}

checker_sql_field_count_message :: proc(select_count, target_count: int) -> string {
	builder := strings.builder_make(context.temp_allocator)
	strings.write_string(&builder, "Open SQL SELECT returns ")
	strings.write_int(&builder, select_count)
	strings.write_string(&builder, " fields, but target has ")
	strings.write_int(&builder, target_count)
	strings.write_string(&builder, " fields")
	return strings.to_string(builder)
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
	payload.field_index = len(structure.fields)
	payload.flags += {.Has_Decl_Range}
	append(&structure.fields, entity)
	scope_insert_declaration(structure.scope, entity)
	checker_add_definition(ctx.info, entity)
	append(&ctx.info.checked_entities, entity)
	return entity
}

checker_sql_inline_structure_name :: proc(ctx: ^Checker_Context, result: ^ast.Select_Result_Clause) -> string {
	target_name := ""
	if result != nil {
		target_name = checker_sql_target_name(result.target)
	}
	if target_name == "" {
		return project_intern_lower_ascii(ctx.project, "<open_sql_inline>")
	}
	builder := strings.builder_make(context.temp_allocator)
	strings.write_string(&builder, "<open_sql_inline:")
	strings.write_string(&builder, target_name)
	strings.write_byte(&builder, '>')
	return project_intern_lower_ascii(ctx.project, strings.to_string(builder))
}

checker_check_sql_assignment :: proc(
	ctx: ^Checker_Context,
	sql: ^Sql_Source_Scope,
	assignment: ast.Sql_Assignment_Clause,
) {
	field_name := project_intern_lower_ascii(ctx.project, assignment.column_name.text)
	range := assignment.column_name.range
	if field_name == "" {
		field_name = checker_sql_simple_expr_name(ctx, assignment.name)
		range = checker_expr_range(assignment.name)
	}
	field, field_ok := checker_sql_lookup_column_by_name(ctx, sql, field_name, "", range)
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
		if name != "" {
			return name, n.value.range, true
		}
	}
	return "", Range{}, false
}

checker_sql_simple_expr_name :: proc(ctx: ^Checker_Context, expr: ^ast.Expr) -> string {
	if expr == nil {
		return ""
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Ident_Expr:
		return project_intern_lower_ascii(ctx.project, n.name)
	case ^ast.Type_Ref_Expr:
		return project_intern_lower_ascii(ctx.project, n.name.text)
	case ^ast.Sql_Column_Expr:
		if n.qualifier.text == "" {
			return project_intern_lower_ascii(ctx.project, n.name.text)
		}
	case ^ast.Host_Expr:
		return checker_sql_simple_expr_name(ctx, n.value)
	}
	return ""
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
	name: string,
) -> string {
	if name == "" {
		return prefix
	}
	builder := strings.builder_make(context.temp_allocator)
	strings.write_string(&builder, prefix)
	strings.write_string(&builder, name)
	return strings.to_string(builder)
}
