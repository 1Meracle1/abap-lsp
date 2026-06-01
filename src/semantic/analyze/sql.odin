#+private
package abap_frontend_semantic_analyze

import "src:ast"
import "src:tokenizer"

import "core:strings"

collect_select_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Select_Stmt, scope: Scope_Id) {
	cte_names := make([dynamic]string, 0, 2, c.allocator)
	if stmt.with != nil {
		for entry in stmt.with.entries {
			if entry.name != "" {
				append(&cte_names, canonical_name(entry.name, c.allocator))
			}
		}
	}

	query_scope := scope
	if len(stmt.body) > 0 {
		previous := c.current_scope
		c.current_scope = scope
		query_scope = push_scope(c, .Select_Block, stmt.range)
		if stmt.with != nil {
			for i in 0 ..< len(stmt.with.entries) {
				collect_select_query_facts(
					c,
					&stmt.with.entries[i].query,
					stmt.range,
					query_scope,
					false,
					cte_names[:],
				)
			}
		}
		collect_select_query_facts(c, &stmt.query, stmt.range, query_scope, true, cte_names[:])
		walk_stmt_list(c, stmt.body, query_scope)
		c.current_scope = query_scope
		pop_scope(c)
		c.current_scope = previous
		return
	}

	if stmt.with != nil {
		for i in 0 ..< len(stmt.with.entries) {
			collect_select_query_facts(
				c,
				&stmt.with.entries[i].query,
				stmt.range,
				scope,
				false,
				cte_names[:],
			)
		}
	}
	collect_select_query_facts(c, &stmt.query, stmt.range, scope, false, cte_names[:])
}

collect_open_cursor_stmt_facts :: proc(
	c: ^Collector,
	stmt: ^ast.Open_Cursor_Stmt,
	scope: Scope_Id,
) {
	collect_expr_refs(c, stmt.handle, scope)
	collect_select_query_facts(c, &stmt.query, stmt.range, scope, false, nil)
}

collect_fetch_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Fetch_Stmt, scope: Scope_Id) {
	collect_expr_refs(c, stmt.handle, scope)
	if stmt.result != nil {
		query_id := len(c.unit.sql_queries)
		collect_select_result_clause(c, query_id, stmt.result, scope)
	}
	collect_expr_refs(c, stmt.package_size, scope)
}

collect_db_table_sql_source :: proc(
	c: ^Collector,
	range: tokenizer.Range,
	target: ^ast.Expr,
	scope: Scope_Id,
	where_cond: ^ast.Expr,
	where_clause: tokenizer.Range,
	dynamic_where: bool,
) -> (
	int,
	bool,
) {
	if target == nil {
		return -1, false
	}
	query_id := len(c.unit.sql_queries)
	flags := Sql_Query_Flags{.Has_From_Clause}
	if where_cond != nil {
		flags += {.Has_Where_Clause}
	}
	if dynamic_where {
		flags += {.Has_Dynamic_Where}
	}
	query := Sql_Query_Data {
		id           = query_id,
		scope        = scope,
		range        = range,
		from_clause  = target.range,
		where_clause = where_clause if range_valid(where_clause) else expr_range(where_cond),
		flags        = flags,
	}
	if sql_expr_is_dynamic_operand(target) {
		collect_sql_dynamic_operand_refs(c, target, scope)
		append(
			&c.unit.sql_dynamic_fragments,
			Sql_Dynamic_Fragment_Data {
				query_id = query_id,
				scope = scope,
				range = target.range,
				kind = .Source,
			},
		)
		append(&c.unit.sql_queries, query)
		return query_id, true
	}
	name, name_range, ok := sql_simple_name(target)
	if !ok {
		return -1, false
	}
	canonical := canonical_name(name, c.allocator)
	if _, local_value := lookup_symbol_in_scope_chain(c, scope, canonical, .Value); local_value {
		return -1, false
	}
	return collect_db_table_sql_source_name(
		c,
		range,
		canonical,
		name_range,
		scope,
		where_cond,
		where_clause,
		dynamic_where,
	)
}

collect_db_table_sql_source_name :: proc(
	c: ^Collector,
	range: tokenizer.Range,
	name: string,
	name_range: tokenizer.Range,
	scope: Scope_Id,
	where_cond: ^ast.Expr,
	where_clause: tokenizer.Range,
	dynamic_where: bool,
) -> (
	int,
	bool,
) {
	if name == "" {
		return -1, false
	}
	canonical := canonical_name(name, c.allocator)
	if _, local_value := lookup_symbol_in_scope_chain(c, scope, canonical, .Value); local_value {
		return -1, false
	}
	query_id := len(c.unit.sql_queries)
	flags := Sql_Query_Flags{.Has_From_Clause}
	if where_cond != nil {
		flags += {.Has_Where_Clause}
	}
	if dynamic_where {
		flags += {.Has_Dynamic_Where}
	}
	append(
		&c.unit.sql_sources,
		Sql_Source_Data {
			query_id = query_id,
			range = name_range,
			source_kind = .From,
			name = canonical,
			resolution = .External,
		},
	)
	push_sql_name_ref(c, query_id, scope, name_range, canonical, "", .Source, .External)
	if where_cond != nil {
		collect_sql_predicate_expr(
			c,
			query_id,
			where_cond,
			scope,
			.Dynamic_Where if dynamic_where else .Where,
		)
	}
	append(
		&c.unit.sql_queries,
		Sql_Query_Data {
			id = query_id,
			scope = scope,
			range = range,
			from_clause = name_range,
			where_clause = where_clause if range_valid(where_clause) else expr_range(where_cond),
			flags = flags,
		},
	)
	return query_id, true
}

collect_select_query_facts :: proc(
	c: ^Collector,
	query: ^ast.Select_Query_Clause,
	fallback: tokenizer.Range,
	scope: Scope_Id,
	has_endselect: bool,
	cte_names: []string,
) {
	query_id := len(c.unit.sql_queries)
	query_range := select_query_range(query, fallback)
	collect_select_query_parts(c, query_id, query, scope, cte_names)
	order_by_fields := select_order_by_fields(c, query)
	for_all_entries_name := ""
	if access, ok := value_access_from_expr(c, sql_unwrap_host(query.for_all_entries), scope);
	   ok && len(access.field_path) == 0 {
		for_all_entries_name = access.base_name
	}

	flags := Sql_Query_Flags{}
	if range_valid(query.projection_clause) ||
	   len(query.projection_clauses) > 0 {flags += {.Has_Projection_Clause}}
	if range_valid(query.from_clause) || query.source_clause != nil {flags += {.Has_From_Clause}}
	if range_valid(query.into_clause) || query.result != nil {flags += {.Has_Into_Clause}}
	if range_valid(query.where_clause) || query.where_cond != nil {flags += {.Has_Where_Clause}}
	if range_valid(query.group_by_clause) {flags += {.Has_Group_By_Clause}}
	if range_valid(query.having_clause) {flags += {.Has_Having_Clause}}
	if range_valid(query.order_by_clause) {flags += {.Has_Order_By_Clause}}
	if query.order_by_primary_key {flags += {.Order_By_Primary_Key}}
	if range_valid(query.for_all_entries_clause) ||
	   query.for_all_entries != nil {flags += {.Has_For_All_Entries}}
	if range_valid(query.for_update_clause) {flags += {.Has_For_Update, .Is_For_Update}}
	if range_valid(query.up_to_clause) || query.up_to_rows != nil {flags += {.Has_Up_To_Clause}}
	if range_valid(query.package_size_clause) ||
	   query.package_size != nil {flags += {.Has_Package_Size_Clause, .Has_Package_Size}}
	if range_valid(query.offset_clause) {flags += {.Has_Offset_Clause}}
	if range_valid(query.abap_options_clause) {flags += {.Has_Abap_Options_Clause}}
	if range_valid(query.set_operator_clause) ||
	   len(query.set_ops) > 0 {flags += {.Has_Set_Operator_Clause, .Has_Set_Operators}}
	if query.single {flags += {.Is_Single}}
	if query.is_distinct {flags += {.Is_Distinct}}
	if has_endselect {flags += {.Has_Endselect}}
	if query.dynamic_where {flags += {.Has_Dynamic_Where}}

	append(
		&c.unit.sql_queries,
		Sql_Query_Data {
			id = query_id,
			scope = scope,
			range = query_range,
			projection_clause = query.projection_clause if range_valid(query.projection_clause) else select_projection_range(query),
			from_clause = query.from_clause if range_valid(query.from_clause) else expr_range(query.source),
			into_clause = query.into_clause if range_valid(query.into_clause) else select_result_range(query.result),
			where_clause = query.where_clause if range_valid(query.where_clause) else expr_range(query.where_cond),
			group_by_clause = query.group_by_clause,
			having_clause = query.having_clause,
			order_by_clause = query.order_by_clause,
			order_by_fields = order_by_fields,
			for_all_entries_clause = query.for_all_entries_clause if range_valid(query.for_all_entries_clause) else expr_range(query.for_all_entries),
			for_all_entries_name = for_all_entries_name,
			for_update_clause = query.for_update_clause,
			up_to_clause = query.up_to_clause if range_valid(query.up_to_clause) else expr_range(query.up_to_rows),
			package_size_clause = query.package_size_clause if range_valid(query.package_size_clause) else expr_range(query.package_size),
			offset_clause = query.offset_clause,
			abap_options_clause = query.abap_options_clause,
			set_operator_clause = query.set_operator_clause,
			flags = flags,
		},
	)
}

collect_select_query_parts :: proc(
	c: ^Collector,
	query_id: int,
	query: ^ast.Select_Query_Clause,
	scope: Scope_Id,
	cte_names: []string,
) {
	for projection in query.projection_clauses {
		collect_sql_projection(c, query_id, projection, scope)
	}
	if query.source_clause != nil {
		collect_select_source_clause(c, query_id, query.source_clause, scope, cte_names)
	}
	if query.where_cond != nil {
		collect_sql_predicate_expr(
			c,
			query_id,
			query.where_cond,
			scope,
			.Dynamic_Where if query.dynamic_where else .Where,
		)
	}
	if query.for_all_entries != nil {
		append(
			&c.unit.sql_predicates,
			Sql_Predicate_Data {
				query_id = query_id,
				range = query.for_all_entries.range,
				kind = .For_All_Entries,
			},
		)
		collect_expr_refs(c, query.for_all_entries, scope)
	}
	collect_select_result_clause(c, query_id, query.result, scope)
	for i in 0 ..< len(query.set_ops) {
		collect_select_query_parts(c, query_id, &query.set_ops[i].query, scope, cte_names)
	}
}

collect_sql_projection :: proc(
	c: ^Collector,
	query_id: int,
	projection: ast.Select_Projection_Clause,
	scope: Scope_Id,
) {
	if projection.value == nil {
		return
	}
	alias := canonical_name(projection.alias, c.allocator) if projection.alias != "" else ""
	if sql_expr_is_dynamic_operand(projection.value) {
		collect_sql_dynamic_operand_refs(c, projection.value, scope)
		append(
			&c.unit.sql_dynamic_fragments,
			Sql_Dynamic_Fragment_Data {
				query_id = query_id,
				scope = scope,
				range = projection.value.range,
				kind = .Projection,
			},
		)
		append(
			&c.unit.sql_projections,
			Sql_Projection_Data {
				query_id = query_id,
				range = projection.value.range,
				kind = .Expression,
				alias = alias,
			},
		)
		return
	}

	kind := Sql_Projection_Kind.Expression
	name := ""
	source_alias := ""
	if lit, lit_ok := projection.value.derived_expr.(^ast.Literal_Expr);
	   lit_ok && lit.value == "*" {
		kind = .Star
		name = "*"
		push_sql_name_ref(c, query_id, scope, lit.range, "*", "", .Star, .Unresolved)
	} else if qualifier, column, column_range, qual_ok := sql_qualified_column(projection.value);
	   qual_ok {
		source_alias = canonical_name(qualifier, c.allocator)
		name = canonical_name(column, c.allocator)
		if column == "*" {
			kind = .Qualified_Star
			push_sql_name_ref(
				c,
				query_id,
				scope,
				column_range,
				"*",
				source_alias,
				.Qualified_Star,
				.Unresolved,
			)
		} else {
			kind = .Column
			push_sql_name_ref(
				c,
				query_id,
				scope,
				column_range,
				name,
				source_alias,
				.Qualified_Column,
				.Unresolved,
			)
		}
	} else if simple, simple_range, simple_ok := sql_simple_name(projection.value); simple_ok {
		kind = .Column
		name = canonical_name(simple, c.allocator)
		push_sql_name_ref(c, query_id, scope, simple_range, name, "", .Column, .Unresolved)
	} else if call_name, call_range, call_ok := sql_call_name(projection.value); call_ok {
		name = canonical_name(call_name, c.allocator)
		ref_kind := sql_call_ref_kind(name)
		kind = .Aggregate if ref_kind == .Aggregate else .Expression
		push_sql_name_ref(c, query_id, scope, call_range, name, "", ref_kind, .External)
		collect_sql_name_refs_from_expr(c, query_id, projection.value, scope, false)
	} else {
		collect_sql_name_refs_from_expr(c, query_id, projection.value, scope, false)
	}
	collect_sql_host_refs_from_expr(c, projection.value, scope)
	append(
		&c.unit.sql_projections,
		Sql_Projection_Data {
			query_id = query_id,
			range = projection.value.range,
			kind = kind,
			source_alias = source_alias,
			name = name,
			alias = alias,
		},
	)
}

collect_select_source_clause :: proc(
	c: ^Collector,
	query_id: int,
	clause: ^ast.Select_Source_Clause,
	scope: Scope_Id,
	cte_names: []string,
) {
	collect_sql_source(
		c,
		query_id,
		clause.source,
		clause.alias,
		"",
		.From,
		scope,
		cte_names,
		clause.dynamic_source,
	)
	for join in clause.joins {
		collect_sql_source(
			c,
			query_id,
			join.source,
			join.alias,
			select_join_kind_text(join.kind),
			.Join,
			scope,
			cte_names,
			false,
		)
		if join.on != nil {
			collect_sql_predicate_expr(c, query_id, join.on, scope, .Join_On)
		}
	}
}

collect_sql_source :: proc(
	c: ^Collector,
	query_id: int,
	expr: ^ast.Expr,
	alias, join_kind: string,
	source_kind: Sql_Source_Kind,
	scope: Scope_Id,
	cte_names: []string,
	force_dynamic: bool,
) {
	if expr == nil {
		return
	}
	alias_name := canonical_name(alias, c.allocator) if alias != "" else ""
	if force_dynamic || sql_expr_is_dynamic_operand(expr) {
		collect_sql_dynamic_operand_refs(c, expr, scope)
		append(
			&c.unit.sql_dynamic_fragments,
			Sql_Dynamic_Fragment_Data {
				query_id = query_id,
				scope = scope,
				range = expr.range,
				kind = .Source,
			},
		)
		if alias_name != "" {
			push_sql_name_ref(c, query_id, scope, expr.range, alias_name, "", .Alias, .Unresolved)
		}
		return
	}

	name := ""
	name_range := expr.range
	resolution := Sql_Resolution.External
	if host, ok := expr.derived_expr.(^ast.Host_Expr); ok {
		collect_expr_refs(c, host.value, scope)
		if simple, range, simple_ok := sql_simple_name(host.value); simple_ok {
			name = canonical_name(simple, c.allocator)
			name_range = range
			resolution = .Internal_Table
		}
	} else if hierarchy_name, hierarchy_range, hierarchy_ok := sql_call_name(expr);
	   hierarchy_ok && strings.equal_fold(hierarchy_name, "HIERARCHY") {
		name = alias_name if alias_name != "" else "hierarchy"
		name_range = hierarchy_range
		resolution = .Hierarchy
		collect_sql_host_refs_from_expr(c, expr, scope)
	} else if unary, unary_ok := expr.derived_expr.(^ast.Unary_Expr);
	   unary_ok && unary.op == .Plus {
		if simple, _, simple_ok := sql_simple_name(unary.expr); simple_ok {
			name = concat3(c, "+", simple, "")
			name_range = expr.range
			if sql_name_in_list(name, cte_names) {
				resolution = .Local_Cte
			}
		}
	} else if simple, range, simple_ok := sql_simple_name(expr); simple_ok {
		name = canonical_name(simple, c.allocator)
		name_range = range
		if sql_name_in_list(name, cte_names) {
			resolution = .Local_Cte
		}
	} else if source_call_name, source_call_range, source_call_ok := sql_call_name(expr);
	   source_call_ok {
		name = canonical_name(source_call_name, c.allocator)
		name_range = source_call_range
		collect_sql_host_refs_from_expr(c, expr, scope)
	}
	if name == "" {
		collect_sql_host_refs_from_expr(c, expr, scope)
		return
	}
	append(
		&c.unit.sql_sources,
		Sql_Source_Data {
			query_id = query_id,
			range = expr.range,
			source_kind = source_kind,
			name = name,
			alias = alias_name,
			join_kind = join_kind,
			resolution = resolution,
		},
	)
	if resolution == .External {
		push_sql_name_ref(c, query_id, scope, name_range, name, "", .Source, .External)
	}
	if alias_name != "" {
		push_sql_name_ref(c, query_id, scope, expr.range, alias_name, "", .Alias, .Unresolved)
	}
}

collect_select_result_clause :: proc(
	c: ^Collector,
	query_id: int,
	result: ^ast.Select_Result_Clause,
	scope: Scope_Id,
) {
	if result == nil || result.kind == .None {
		return
	}
	flags := Sql_Target_Flags{}
	if result.table {flags += {.Is_Table}}
	if result.corresponding_fields {flags += {.Is_Corresponding}}
	target_name := ""
	target_range := tokenizer.Range{}
	is_inline := false
	if result.target != nil {
		flags += {.Has_Target_Range}
		target_range = result.target.range
	}

	if name, range, inline_kind, ok := sql_inline_target(result.target); ok {
		target_name = canonical_name(name, c.allocator)
		target_range = range
		is_inline = true
		flags += {.Is_Inline}
		if inline_kind == .Variable {
			structure_id := INVALID_STRUCTURE_ID
			declared_type := Field_Type_Ref_Data{}
			has_type := false
			type_display := ""
			type_form := ast.Data_Type_Form{}
			has_type_form := false
			if result.table {
				structure_id = inline_select_target_structure(c, query_id, target_name, scope)
				type_form = .Standard_Table
				has_type_form = true
			} else if type_ref, type_ok := inline_select_target_type(c, query_id); type_ok {
				declared_type = type_ref
				has_type = true
				type_display = type_ref.base_name
			}
			_ = declare_collected_symbol(
				c,
				scope,
				target_name,
				.Variable,
				range,
				structure_id,
				declared_type,
				has_type,
				type_display,
				"",
				type_form,
				has_type_form,
			)
		} else {
			_ = declare_collected_symbol(c, scope, target_name, .Field_Symbol, range)
		}
	} else {
		target_name = sql_target_name_from_expr(c, result.target)
		collect_expr_refs(c, result.target, scope)
	}
	if result.target != nil {
		lhs_access, has_lhs := value_access_from_expr(c, sql_unwrap_host(result.target), scope)
		add_assignment_site(
			c,
			scope,
			result.target.range,
			target_range,
			tokenizer.Range{},
			lhs_access,
			has_lhs,
			type_fact_from_expr(c, sql_unwrap_host(result.target), scope),
			unknown_type_fact(),
		)
	}
	append(
		&c.unit.sql_targets,
		Sql_Target_Data {
			query_id = query_id,
			scope = scope,
			range = select_result_range(result),
			target_range = target_range,
			kind = .Appending if result.kind == .Appending else .Into,
			target_name = target_name,
			flags = flags,
		},
	)
}

collect_sql_predicate_expr :: proc(
	c: ^Collector,
	query_id: int,
	expr: ^ast.Expr,
	scope: Scope_Id,
	kind: Sql_Predicate_Kind,
) {
	if expr == nil {
		return
	}
	append(
		&c.unit.sql_predicates,
		Sql_Predicate_Data{query_id = query_id, range = expr.range, kind = kind},
	)
	if kind == .Dynamic_Where {
		append(
			&c.unit.sql_dynamic_fragments,
			Sql_Dynamic_Fragment_Data {
				query_id = query_id,
				scope = scope,
				range = expr.range,
				kind = .Where,
			},
		)
		collect_sql_dynamic_operand_refs(c, expr, scope)
		return
	}
	collect_sql_host_refs_from_expr(c, expr, scope)
	collect_sql_name_refs_from_expr(c, query_id, expr, scope, true)
}

collect_sql_name_refs_from_expr :: proc(
	c: ^Collector,
	query_id: int,
	expr: ^ast.Expr,
	scope: Scope_Id,
	open_sql_predicate: bool,
) {
	if expr == nil {
		return
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Host_Expr:
		collect_expr_refs(c, n.value, scope)
	case ^ast.Ident_Expr:
		name := canonical_name(n.name, c.allocator)
		if open_sql_predicate {
			push_sql_predicate_name(c, query_id, scope, n.range, name)
		} else {
			push_sql_name_ref(c, query_id, scope, n.range, name, "", .Column, .Unresolved)
		}
	case ^ast.Literal_Expr:
		if n.value == "*" {
			push_sql_name_ref(c, query_id, scope, n.range, "*", "", .Star, .Unresolved)
		}
	case ^ast.Selector_Expr:
		if qualifier, column, range, ok := sql_qualified_column(expr); ok {
			kind := Sql_Name_Ref_Kind.Qualified_Column
			if column == "*" {
				kind = .Qualified_Star
			}
			push_sql_name_ref(
				c,
				query_id,
				scope,
				range,
				canonical_name(column, c.allocator),
				canonical_name(qualifier, c.allocator),
				kind,
				.Unresolved,
			)
		} else if open_sql_predicate {
			access, access_ok := value_access_from_expr(c, expr, scope)
			if access_ok && sql_local_value_exists(c, scope, access.base_name) {
				collect_expr_refs(c, expr, scope)
			}
		}
	case ^ast.Call_Expr:
		if name, range, ok := sql_call_name(expr); ok {
			lower := canonical_name(name, c.allocator)
			push_sql_name_ref(
				c,
				query_id,
				scope,
				range,
				lower,
				"",
				sql_call_ref_kind(lower),
				.External,
			)
		}
		if args, ok := n.args.derived_expr.(^ast.Call_Arg_List_Expr); ok {
			for arg in args.args {
				collect_sql_name_refs_from_expr(c, query_id, arg, scope, open_sql_predicate)
			}
		}
	case ^ast.Call_Named_Arg_Expr:
		collect_sql_name_refs_from_expr(c, query_id, n.value, scope, open_sql_predicate)
	case ^ast.Call_Positional_Arg_Expr:
		collect_sql_name_refs_from_expr(c, query_id, n.value, scope, open_sql_predicate)
	case ^ast.Binary_Expr:
		collect_sql_name_refs_from_expr(c, query_id, n.left, scope, open_sql_predicate)
		collect_sql_name_refs_from_expr(c, query_id, n.right, scope, open_sql_predicate)
	case ^ast.Unary_Expr:
		collect_sql_name_refs_from_expr(c, query_id, n.expr, scope, open_sql_predicate)
	case ^ast.Paren_Expr:
		collect_sql_name_refs_from_expr(c, query_id, n.expr, scope, open_sql_predicate)
	case ^ast.Between_Expr:
		collect_sql_name_refs_from_expr(c, query_id, n.subject, scope, open_sql_predicate)
		collect_sql_name_refs_from_expr(c, query_id, n.low, scope, open_sql_predicate)
		collect_sql_name_refs_from_expr(c, query_id, n.high, scope, open_sql_predicate)
	case ^ast.Sql_Case_When_Expr:
		collect_sql_name_refs_from_expr(c, query_id, n.condition, scope, open_sql_predicate)
		collect_sql_name_refs_from_expr(c, query_id, n.result, scope, open_sql_predicate)
	case ^ast.Sql_Case_Expr:
		collect_sql_name_refs_from_expr(c, query_id, n.operand, scope, open_sql_predicate)
		for when_expr in n.whens {
			collect_sql_name_refs_from_expr(c, query_id, when_expr, scope, open_sql_predicate)
		}
		collect_sql_name_refs_from_expr(c, query_id, n.else_expr, scope, open_sql_predicate)
	case ^ast.Is_Predicate_Expr:
		collect_sql_name_refs_from_expr(c, query_id, n.subject, scope, open_sql_predicate)
	case ^ast.Instance_Of_Predicate_Expr:
		collect_sql_name_refs_from_expr(c, query_id, n.subject, scope, open_sql_predicate)
	case ^ast.Table_Expr:
		collect_sql_name_refs_from_expr(c, query_id, n.table, scope, open_sql_predicate)
		for selector in n.selectors {
			collect_sql_name_refs_from_expr(c, query_id, selector, scope, open_sql_predicate)
		}
	}
}

collect_sql_host_refs_from_expr :: proc(c: ^Collector, expr: ^ast.Expr, scope: Scope_Id) {
	if expr == nil {
		return
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Host_Expr:
		collect_expr_refs(c, n.value, scope)
	case ^ast.Binary_Expr:
		collect_sql_host_refs_from_expr(c, n.left, scope)
		collect_sql_host_refs_from_expr(c, n.right, scope)
	case ^ast.Unary_Expr:
		collect_sql_host_refs_from_expr(c, n.expr, scope)
	case ^ast.Paren_Expr:
		collect_sql_host_refs_from_expr(c, n.expr, scope)
	case ^ast.Selector_Expr:
		collect_sql_host_refs_from_expr(c, n.base, scope)
		collect_sql_host_refs_from_expr(c, n.field, scope)
	case ^ast.Call_Expr:
		collect_sql_host_refs_from_expr(c, n.callee, scope)
		collect_sql_host_refs_from_expr(c, n.args, scope)
	case ^ast.Call_Arg_List_Expr:
		for arg in n.args {collect_sql_host_refs_from_expr(c, arg, scope)}
	case ^ast.Call_Named_Arg_Expr:
		collect_sql_host_refs_from_expr(c, n.value, scope)
	case ^ast.Call_Positional_Arg_Expr:
		collect_sql_host_refs_from_expr(c, n.value, scope)
	case ^ast.Table_Expr:
		collect_sql_host_refs_from_expr(c, n.table, scope)
		for selector in n.selectors {collect_sql_host_refs_from_expr(c, selector, scope)}
	case ^ast.Between_Expr:
		collect_sql_host_refs_from_expr(c, n.subject, scope)
		collect_sql_host_refs_from_expr(c, n.low, scope)
		collect_sql_host_refs_from_expr(c, n.high, scope)
	case ^ast.Is_Predicate_Expr:
		collect_sql_host_refs_from_expr(c, n.subject, scope)
	case ^ast.Instance_Of_Predicate_Expr:
		collect_sql_host_refs_from_expr(c, n.subject, scope)
		collect_sql_host_refs_from_expr(c, n.type_ref, scope)
	case ^ast.Sql_Case_When_Expr:
		collect_sql_host_refs_from_expr(c, n.condition, scope)
		collect_sql_host_refs_from_expr(c, n.result, scope)
	case ^ast.Sql_Case_Expr:
		collect_sql_host_refs_from_expr(c, n.operand, scope)
		for when_expr in n.whens {collect_sql_host_refs_from_expr(c, when_expr, scope)}
		collect_sql_host_refs_from_expr(c, n.else_expr, scope)
	}
}

collect_sql_dynamic_operand_refs :: proc(c: ^Collector, expr: ^ast.Expr, scope: Scope_Id) {
	if paren, ok := expr.derived_expr.(^ast.Paren_Expr); ok {
		collect_expr_refs(c, paren.expr, scope)
		return
	}
	collect_expr_refs(c, expr, scope)
}

push_sql_predicate_name :: proc(
	c: ^Collector,
	query_id: int,
	scope: Scope_Id,
	range: tokenizer.Range,
	name: string,
) {
	append(
		&c.unit.sql_predicate_names,
		Sql_Predicate_Name_Data {
			query_id = query_id,
			scope = scope,
			range = range,
			name = canonical_name(name, c.allocator),
		},
	)
}

push_sql_name_ref :: proc(
	c: ^Collector,
	query_id: int,
	scope: Scope_Id,
	range: tokenizer.Range,
	name, qualifier: string,
	kind: Sql_Name_Ref_Kind,
	resolution: Sql_Resolution,
) {
	append(
		&c.unit.sql_name_refs,
		Sql_Name_Ref_Data {
			query_id = query_id,
			scope = scope,
			range = range,
			name = canonical_name(name, c.allocator),
			qualifier = canonical_name(qualifier, c.allocator) if qualifier != "" else "",
			kind = kind,
			resolution = resolution,
		},
	)
}

sql_expr_is_dynamic_operand :: proc(expr: ^ast.Expr) -> bool {
	if expr == nil {
		return false
	}
	_, ok := expr.derived_expr.(^ast.Paren_Expr)
	return ok
}

sql_simple_name :: proc(expr: ^ast.Expr) -> (string, tokenizer.Range, bool) {
	if expr == nil {
		return "", tokenizer.Range{}, false
	}
	if id, ok := expr.derived_expr.(^ast.Ident_Expr); ok {
		return id.name, id.range, id.name != ""
	}
	if typ, ok := expr.derived_expr.(^ast.Type_Ref_Expr); ok {
		return typ.name, typ.range, typ.name != ""
	}
	return "", tokenizer.Range{}, false
}

sql_call_name :: proc(expr: ^ast.Expr) -> (string, tokenizer.Range, bool) {
	if expr == nil {
		return "", tokenizer.Range{}, false
	}
	call, ok := expr.derived_expr.(^ast.Call_Expr)
	if !ok || call.callee == nil {
		return "", tokenizer.Range{}, false
	}
	return sql_simple_name(call.callee)
}

sql_qualified_column :: proc(expr: ^ast.Expr) -> (string, string, tokenizer.Range, bool) {
	if expr == nil {
		return "", "", tokenizer.Range{}, false
	}
	sel, ok := expr.derived_expr.(^ast.Selector_Expr)
	if !ok || sel.op != .Tilde {
		return "", "", tokenizer.Range{}, false
	}
	qualifier, _, qualifier_ok := expr_name(sel.base)
	column, _, column_ok := expr_name(sel.field)
	if !column_ok {
		if lit, lit_ok := sel.field.derived_expr.(^ast.Literal_Expr); lit_ok && lit.value == "*" {
			column = "*"
			column_ok = true
		}
	}
	if !qualifier_ok || !column_ok {
		return "", "", tokenizer.Range{}, false
	}
	return qualifier, column, expr.range, true
}

sql_call_ref_kind :: proc(name: string) -> Sql_Name_Ref_Kind {
	if strings.equal_fold(name, "avg") ||
	   strings.equal_fold(name, "count") ||
	   strings.equal_fold(name, "max") ||
	   strings.equal_fold(name, "min") ||
	   strings.equal_fold(name, "sum") ||
	   strings.equal_fold(name, "median") ||
	   strings.equal_fold(name, "stddev") ||
	   strings.equal_fold(name, "var") ||
	   strings.equal_fold(name, "corr") ||
	   strings.equal_fold(name, "corr_spearman") ||
	   strings.equal_fold(name, "grouping") ||
	   strings.equal_fold(name, "string_agg") ||
	   strings.equal_fold(name, "allow_precision_loss") {
		return .Aggregate
	}
	return .Function
}

sql_inline_target :: proc(expr: ^ast.Expr) -> (string, tokenizer.Range, Symbol_Kind, bool) {
	unwrapped := sql_unwrap_host(expr)
	if unwrapped == nil {
		return "", tokenizer.Range{}, .Variable, false
	}
	if data, ok := unwrapped.derived_expr.(^ast.Data_Inline_Name_Expr); ok {
		return data.name, data.range, .Variable, data.name != ""
	}
	if fs, ok := unwrapped.derived_expr.(^ast.Field_Symbol_Inline_Name_Expr); ok {
		return fs.name, fs.range, .Field_Symbol, fs.name != ""
	}
	return "", tokenizer.Range{}, .Variable, false
}

sql_unwrap_host :: proc(expr: ^ast.Expr) -> ^ast.Expr {
	if expr == nil {
		return nil
	}
	if host, ok := expr.derived_expr.(^ast.Host_Expr); ok {
		return host.value
	}
	return expr
}

sql_target_name_from_expr :: proc(c: ^Collector, expr: ^ast.Expr) -> string {
	unwrapped := sql_unwrap_host(expr)
	if unwrapped == nil {
		return ""
	}
	if name, _, _, ok := sql_inline_target(unwrapped); ok {
		return canonical_name(name, c.allocator)
	}
	if access, ok := value_access_from_expr(c, unwrapped, c.current_scope); ok {
		return table_order_name_from_access(c, access)
	}
	if name, _, ok := sql_simple_name(unwrapped); ok {
		return canonical_name(name, c.allocator)
	}
	return ""
}

inline_select_target_structure :: proc(
	c: ^Collector,
	query_id: int,
	target_name: string,
	scope: Scope_Id,
) -> Structure_Id {
	fields := make([dynamic]Structure_Field_Data, 0, 4, c.allocator)
	for projection in c.unit.sql_projections {
		if projection.query_id != query_id {
			continue
		}
		field_name := projection.alias
		if field_name == "" && projection.kind == .Column {
			field_name = projection.name
		}
		if field_name == "" || field_name == "*" {
			continue
		}
		flags := Structure_Field_Flags{.Has_Decl_Range}
		type_ref := Field_Type_Ref_Data{}
		if projection.kind == .Aggregate && projection.name == "count" {
			type_ref = builtin_type_ref("i")
			flags += {.Has_Type_Ref}
		}
		append(
			&fields,
			Structure_Field_Data {
				name = field_name,
				decl_range = projection.range,
				decl_unit = c.unit.unit_id,
				structure = INVALID_STRUCTURE_ID,
				type_ref = type_ref,
				flags = flags,
			},
		)
	}
	if len(fields) == 0 {
		return INVALID_STRUCTURE_ID
	}
	return push_collected_structure(c, concat3(c, "<open_sql_inline:", target_name, ">"), fields, scope)
}

inline_select_target_type :: proc(c: ^Collector, query_id: int) -> (Field_Type_Ref_Data, bool) {
	count := 0
	out := Field_Type_Ref_Data{}
	for projection in c.unit.sql_projections {
		if projection.query_id != query_id {
			continue
		}
		count += 1
		if projection.kind == .Aggregate && projection.name == "count" {
			out = builtin_type_ref("i")
		}
	}
	return out, count == 1 && out.base_name != ""
}

select_order_by_fields :: proc(c: ^Collector, query: ^ast.Select_Query_Clause) -> [dynamic]string {
	fields := make([dynamic]string, 0, len(query.order_by_fields), c.allocator)
	for field in query.order_by_fields {
		append(&fields, canonical_name(field, c.allocator))
	}
	return fields
}

select_join_kind_text :: proc(kind: ast.Select_Join_Kind) -> string {
	#partial switch kind {
	case .Inner:
		return "inner"
	case .Left_Outer:
		return "left outer"
	case .Right_Outer:
		return "right outer"
	case .Full_Outer:
		return "full outer"
	case .Cross:
		return "cross"
	}
	return ""
}

select_query_range :: proc(
	query: ^ast.Select_Query_Clause,
	fallback: tokenizer.Range,
) -> tokenizer.Range {
	out := tokenizer.Range {
		start = fallback.end,
		end   = fallback.start,
	}
	for value in query.projections {
		out = merge_range(out, expr_range(value))
	}
	out = merge_range(out, expr_range(query.source))
	out = merge_range(out, select_result_range(query.result))
	out = merge_range(out, expr_range(query.where_cond))
	out = merge_range(out, expr_range(query.for_all_entries))
	out = merge_range(out, expr_range(query.package_size))
	out = merge_range(out, expr_range(query.up_to_rows))
	out = merge_range(out, query.projection_clause)
	out = merge_range(out, query.from_clause)
	out = merge_range(out, query.into_clause)
	out = merge_range(out, query.where_clause)
	out = merge_range(out, query.group_by_clause)
	out = merge_range(out, query.having_clause)
	out = merge_range(out, query.order_by_clause)
	out = merge_range(out, query.for_all_entries_clause)
	out = merge_range(out, query.for_update_clause)
	out = merge_range(out, query.up_to_clause)
	out = merge_range(out, query.package_size_clause)
	out = merge_range(out, query.offset_clause)
	out = merge_range(out, query.abap_options_clause)
	out = merge_range(out, query.set_operator_clause)
	for i in 0 ..< len(query.set_ops) {
		out = merge_range(out, select_query_range(&query.set_ops[i].query, fallback))
	}
	if !range_valid(out) {
		return fallback
	}
	return out
}

select_projection_range :: proc(query: ^ast.Select_Query_Clause) -> tokenizer.Range {
	out := tokenizer.Range{}
	for projection in query.projection_clauses {
		if range_valid(projection.range) {
			out = merge_range(out, projection.range)
		} else {
			out = merge_range(out, expr_range(projection.value))
		}
	}
	return out
}

select_result_range :: proc(result: ^ast.Select_Result_Clause) -> tokenizer.Range {
	if result == nil || result.target == nil {
		return tokenizer.Range{}
	}
	return result.target.range
}

expr_range :: proc(expr: ^ast.Expr) -> tokenizer.Range {
	if expr == nil {
		return tokenizer.Range{}
	}
	return expr.range
}

merge_range :: proc(a, b: tokenizer.Range) -> tokenizer.Range {
	if !range_valid(a) {
		return b
	}
	if !range_valid(b) {
		return a
	}
	return tokenizer.text_range(min(a.start, b.start), max(a.end, b.end))
}

range_valid :: #force_inline proc(range: tokenizer.Range) -> bool {
	return range.end > range.start
}

sql_name_in_list :: proc(name: string, names: []string) -> bool {
	for n in names {
		if strings.equal_fold(name, n) {
			return true
		}
	}
	return false
}

sql_local_value_exists :: proc(c: ^Collector, scope: Scope_Id, name: string) -> bool {
	_, ok := lookup_symbol_in_scope_chain(c, scope, name, .Value)
	return ok
}

table_order_name_from_access :: proc(c: ^Collector, access: Field_Access) -> string {
	if len(access.field_path) == 0 {
		return access.base_name
	}
	out := strings.builder_make(c.allocator)
	strings.write_string(&out, access.base_name)
	for segment in access.field_path {
		strings.write_byte(&out, '-')
		strings.write_string(&out, segment.name)
	}
	return strings.to_string(out)
}
