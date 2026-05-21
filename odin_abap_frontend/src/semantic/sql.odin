package abap_frontend_semantic

import "../ast"
import "../tokenizer"

import "core:strings"

Sql_Query_Scan :: struct {
	projection_clause:      tokenizer.Range,
	from_clause:            tokenizer.Range,
	into_clause:            tokenizer.Range,
	where_clause:           tokenizer.Range,
	group_by_clause:        tokenizer.Range,
	having_clause:          tokenizer.Range,
	order_by_clause:        tokenizer.Range,
	order_by_primary_key:   bool,
	order_by_fields:        [dynamic]string,
	for_all_entries_clause: tokenizer.Range,
	for_update_clause:      tokenizer.Range,
	up_to_clause:           tokenizer.Range,
	package_size_clause:    tokenizer.Range,
	offset_clause:          tokenizer.Range,
	abap_options_clause:    tokenizer.Range,
	set_operator_clause:    tokenizer.Range,
	has_dynamic_where:      bool,
}

collect_select_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Select_Stmt, scope: Scope_Id) {
	add_system_field_update(c, scope, stmt.range, .Select, "subrc")
	add_system_field_update(c, scope, stmt.range, .Select, "dbcnt")

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
	add_routine_site(c, scope, stmt.range, .Unknown_Effect)
}

collect_fetch_stmt_facts :: proc(c: ^Collector, stmt: ^ast.Fetch_Stmt, scope: Scope_Id) {
	collect_expr_refs(c, stmt.handle, scope)
	if stmt.result != nil {
		query_id := len(c.sql_queries)
		collect_select_result_clause(c, query_id, stmt.result, scope)
	}
	collect_expr_refs(c, stmt.package_size, scope)
	add_routine_site(c, scope, stmt.range, .Unknown_Effect)
}

collect_db_table_sql_source :: proc(
	c: ^Collector,
	range: tokenizer.Range,
	target: ^ast.Expr,
	scope: Scope_Id,
	where_cond: ^ast.Expr,
	dynamic_where: bool,
) -> (
	int,
	bool,
) {
	if target == nil {
		return -1, false
	}
	query_id := len(c.sql_queries)
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
		where_clause = expr_range(where_cond),
		flags        = flags,
	}
	if sql_expr_is_dynamic_operand(target) {
		collect_sql_dynamic_operand_refs(c, target, scope)
		append(
			&c.sql_dynamic_fragments,
			Sql_Dynamic_Fragment_Data {
				query_id = query_id,
				scope = scope,
				range = target.range,
				kind = .Source,
			},
		)
		append(&c.sql_queries, query)
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
	query_id := len(c.sql_queries)
	flags := Sql_Query_Flags{.Has_From_Clause}
	if where_cond != nil {
		flags += {.Has_Where_Clause}
	}
	if dynamic_where {
		flags += {.Has_Dynamic_Where}
	}
	append(
		&c.sql_sources,
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
		&c.sql_queries,
		Sql_Query_Data {
			id = query_id,
			scope = scope,
			range = range,
			from_clause = name_range,
			where_clause = expr_range(where_cond),
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
	query_id := len(c.sql_queries)
	query_range := select_query_range(query, fallback)
	scan_range := fallback if range_valid(fallback) else query_range
	scan := sql_scan_query(c, scan_range)
	collect_sql_projection_scan_facts(c, query_id, scan.projection_clause, scope)
	collect_select_query_parts(c, query_id, query, scope, cte_names)

	flags := Sql_Query_Flags{}
	if range_valid(scan.projection_clause) ||
	   len(query.projection_clauses) > 0 {flags += {.Has_Projection_Clause}}
	if range_valid(scan.from_clause) || query.source_clause != nil {flags += {.Has_From_Clause}}
	if range_valid(scan.into_clause) || query.result != nil {flags += {.Has_Into_Clause}}
	if range_valid(scan.where_clause) || query.where_cond != nil {flags += {.Has_Where_Clause}}
	if range_valid(scan.group_by_clause) {flags += {.Has_Group_By_Clause}}
	if range_valid(scan.having_clause) {flags += {.Has_Having_Clause}}
	if range_valid(scan.order_by_clause) {flags += {.Has_Order_By_Clause}}
	if scan.order_by_primary_key {flags += {.Order_By_Primary_Key}}
	if range_valid(scan.for_all_entries_clause) ||
	   query.for_all_entries != nil {flags += {.Has_For_All_Entries}}
	if range_valid(scan.for_update_clause) {flags += {.Has_For_Update, .Is_For_Update}}
	if range_valid(scan.up_to_clause) || query.up_to_rows != nil {flags += {.Has_Up_To_Clause}}
	if range_valid(scan.package_size_clause) ||
	   query.package_size != nil {flags += {.Has_Package_Size_Clause, .Has_Package_Size}}
	if range_valid(scan.offset_clause) {flags += {.Has_Offset_Clause}}
	if range_valid(scan.abap_options_clause) {flags += {.Has_Abap_Options_Clause}}
	if range_valid(scan.set_operator_clause) ||
	   len(query.set_ops) > 0 {flags += {.Has_Set_Operator_Clause, .Has_Set_Operators}}
	if query.single {flags += {.Is_Single}}
	if query.is_distinct {flags += {.Is_Distinct}}
	if has_endselect {flags += {.Has_Endselect}}
	if query.dynamic_where || scan.has_dynamic_where {flags += {.Has_Dynamic_Where}}

	if len(scan.order_by_fields) > 0 {
		for target in c.sql_targets {
			if target.query_id == query_id &&
			   .Is_Table in target.flags &&
			   target.target_name != "" {
				record_internal_table_order(
					c,
					scope,
					query_range,
					target.target_name,
					scan.order_by_fields[:],
				)
			}
		}
	}

	append(
		&c.sql_queries,
		Sql_Query_Data {
			id = query_id,
			scope = scope,
			range = query_range,
			projection_clause = scan.projection_clause if range_valid(scan.projection_clause) else select_projection_range(query),
			from_clause = scan.from_clause if range_valid(scan.from_clause) else expr_range(query.source),
			into_clause = scan.into_clause if range_valid(scan.into_clause) else select_result_range(query.result),
			where_clause = scan.where_clause if range_valid(scan.where_clause) else expr_range(query.where_cond),
			group_by_clause = scan.group_by_clause,
			having_clause = scan.having_clause,
			order_by_clause = scan.order_by_clause,
			order_by_fields = scan.order_by_fields,
			for_all_entries_clause = scan.for_all_entries_clause if range_valid(scan.for_all_entries_clause) else expr_range(query.for_all_entries),
			for_update_clause = scan.for_update_clause,
			up_to_clause = scan.up_to_clause if range_valid(scan.up_to_clause) else expr_range(query.up_to_rows),
			package_size_clause = scan.package_size_clause if range_valid(scan.package_size_clause) else expr_range(query.package_size),
			offset_clause = scan.offset_clause,
			abap_options_clause = scan.abap_options_clause,
			set_operator_clause = scan.set_operator_clause,
			flags = flags,
		},
	)
}

collect_sql_projection_scan_facts :: proc(
	c: ^Collector,
	query_id: int,
	range: tokenizer.Range,
	scope: Scope_Id,
) {
	if !range_valid(range) {
		return
	}
	text := source_text(c, range)
	tokens := header_tokens(c, text, range.start)
	for i in 0 ..< len(tokens) - 2 {
		if tokens[i].kind == .Ident &&
		   tokens[i + 1].kind == .Tilde &&
		   tokens[i + 2].kind == .Star {
			qualifier := canonical_name(tokens[i].text, c.allocator)
			push_sql_name_ref(
				c,
				query_id,
				scope,
				tokenizer.text_range(tokens[i].range.start, tokens[i + 2].range.end),
				"*",
				qualifier,
				.Qualified_Star,
				.Unresolved,
			)
			append(
				&c.sql_projections,
				Sql_Projection_Data {
					query_id = query_id,
					range = tokenizer.text_range(tokens[i].range.start, tokens[i + 2].range.end),
					kind = .Qualified_Star,
					source_alias = qualifier,
					name = "*",
				},
			)
		}
	}
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
			&c.sql_predicates,
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
			&c.sql_dynamic_fragments,
			Sql_Dynamic_Fragment_Data {
				query_id = query_id,
				scope = scope,
				range = projection.value.range,
				kind = .Projection,
			},
		)
		append(
			&c.sql_projections,
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
	} else if star_qualifier, star_range, star_ok := sql_qualified_star_text(c, projection.value);
	   star_ok {
		source_alias = canonical_name(star_qualifier, c.allocator)
		name = "*"
		kind = .Qualified_Star
		push_sql_name_ref(
			c,
			query_id,
			scope,
			star_range,
			"*",
			source_alias,
			.Qualified_Star,
			.Unresolved,
		)
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
		&c.sql_projections,
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
			&c.sql_dynamic_fragments,
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
	   hierarchy_ok && ascii_equal_ignore_case(hierarchy_name, "HIERARCHY") {
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
		&c.sql_sources,
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
			if result.table {
				structure_id = inline_select_target_structure(c, query_id, target_name)
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
		&c.sql_targets,
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
		&c.sql_predicates,
		Sql_Predicate_Data{query_id = query_id, range = expr.range, kind = kind},
	)
	if kind == .Dynamic_Where {
		append(
			&c.sql_dynamic_fragments,
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
		if open_sql_predicate && sql_local_value_exists(c, scope, name) {
			add_reference(c, scope, name, .Value, .Identifier, n.range)
		} else if !sql_token_is_keyword_text(name) {
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
	}
}

collect_sql_dynamic_operand_refs :: proc(c: ^Collector, expr: ^ast.Expr, scope: Scope_Id) {
	if paren, ok := expr.derived_expr.(^ast.Paren_Expr); ok {
		collect_expr_refs(c, paren.expr, scope)
		return
	}
	collect_expr_refs(c, expr, scope)
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
		&c.sql_name_refs,
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

sql_scan_query :: proc(c: ^Collector, range: tokenizer.Range) -> Sql_Query_Scan {
	scan := Sql_Query_Scan {
		order_by_fields = make([dynamic]string, 0, 2, c.allocator),
	}
	text := source_text(c, range)
	tokens := header_tokens(c, text, range.start)
	if len(tokens) == 0 {
		return scan
	}
	projection_start := sql_projection_start(tokens[:])
	fields_idx := sql_find_keyword(tokens[:], "FIELDS")
	if fields_idx >= 0 {
		projection_start = fields_idx + 1
	}
	if projection_start >= 0 {
		projection_end := sql_next_clause_index(tokens[:], projection_start)
		if projection_start < projection_end {
			scan.projection_clause = tokenizer.text_range(
				tokens[projection_start].range.start,
				tokens[projection_end - 1].range.end,
			)
		}
	}
	if idx := sql_find_keyword(tokens[:], "FROM"); idx >= 0 {
		scan.from_clause = sql_clause_range(tokens[:], idx + 1)
	}
	into_idx := sql_find_keyword(tokens[:], "INTO")
	if into_idx >= 0 {
		scan.into_clause = sql_clause_range(tokens[:], into_idx)
	} else {
		appending_idx := sql_find_keyword(tokens[:], "APPENDING")
		if appending_idx >= 0 {
			scan.into_clause = sql_clause_range(tokens[:], appending_idx)
		}
	}
	if idx := sql_find_keyword(tokens[:], "WHERE"); idx >= 0 {
		scan.where_clause = sql_clause_range(tokens[:], idx)
		predicate := sql_clause_tokens(tokens[:], idx + 1)
		scan.has_dynamic_where = sql_tokens_are_dynamic_operand(predicate)
	}
	if idx := sql_find_phrase(tokens[:], []string{"GROUP", "BY"}); idx >= 0 {
		scan.group_by_clause = sql_clause_range(tokens[:], idx)
	}
	if idx := sql_find_keyword(tokens[:], "HAVING"); idx >= 0 {
		scan.having_clause = sql_clause_range(tokens[:], idx)
	}
	if idx := sql_find_phrase(tokens[:], []string{"ORDER", "BY"}); idx >= 0 {
		scan.order_by_clause = sql_clause_range(tokens[:], idx)
		scan.order_by_primary_key, scan.order_by_fields = sql_order_by_info(c, tokens[:], idx)
	}
	if idx := sql_find_phrase(tokens[:], []string{"FOR", "ALL", "ENTRIES"}); idx >= 0 {
		scan.for_all_entries_clause = sql_clause_range(tokens[:], idx)
	}
	if idx := sql_find_phrase(tokens[:], []string{"FOR", "UPDATE"}); idx >= 0 {
		end := idx + 2
		scan.for_update_clause = tokenizer.text_range(
			tokens[idx].range.start,
			tokens[end - 1].range.end,
		)
	}
	if idx := sql_find_phrase(tokens[:], []string{"UP", "TO"}); idx >= 0 {
		scan.up_to_clause = sql_clause_range(tokens[:], idx)
	}
	if idx := sql_find_phrase(tokens[:], []string{"PACKAGE", "SIZE"}); idx >= 0 {
		scan.package_size_clause = sql_clause_range(tokens[:], idx)
	}
	if idx := sql_find_keyword(tokens[:], "OFFSET"); idx >= 0 {
		scan.offset_clause = sql_clause_range(tokens[:], idx)
	}
	abap_option_keywords := [?]string{"BYPASSING", "CONNECTION", "CLIENT"}
	for keyword in abap_option_keywords {
		if idx := sql_find_keyword(tokens[:], keyword); idx >= 0 {
			scan.abap_options_clause = merge_range(
				scan.abap_options_clause,
				sql_clause_range(tokens[:], idx),
			)
		}
	}
	set_keywords := [?]string{"UNION", "INTERSECT", "EXCEPT"}
	for keyword in set_keywords {
		if idx := sql_find_keyword(tokens[:], keyword); idx >= 0 {
			scan.set_operator_clause = merge_range(
				scan.set_operator_clause,
				sql_clause_range(tokens[:], idx),
			)
		}
	}
	return scan
}

sql_projection_start :: proc(tokens: []Header_Token) -> int {
	i := sql_find_keyword(tokens, "SELECT")
	if i < 0 {
		return -1
	}
	i += 1
	for i < len(tokens) {
		if token_eq(tokens[i], "SINGLE") || token_eq(tokens[i], "DISTINCT") {
			i += 1
			continue
		}
		if i + 1 < len(tokens) && token_eq(tokens[i], "FOR") && token_eq(tokens[i + 1], "UPDATE") {
			i += 2
			continue
		}
		break
	}
	return i
}

sql_clause_range :: proc(tokens: []Header_Token, start: int) -> tokenizer.Range {
	if start < 0 || start >= len(tokens) {
		return tokenizer.Range{}
	}
	end := sql_next_clause_index(tokens, start + 1)
	if end <= start {
		end = start + 1
	}
	return tokenizer.text_range(tokens[start].range.start, tokens[end - 1].range.end)
}

sql_clause_tokens :: proc(tokens: []Header_Token, start: int) -> []Header_Token {
	if start < 0 || start >= len(tokens) {
		return nil
	}
	end := sql_next_clause_index(tokens, start)
	if end <= start {
		return nil
	}
	return tokens[start:end]
}

sql_next_clause_index :: proc(tokens: []Header_Token, start: int) -> int {
	for i in start ..< len(tokens) {
		if tokens[i].kind == .Period || tokens[i].kind == .RParen {
			return i
		}
		if sql_clause_start_at(tokens, i) {
			return i
		}
	}
	return len(tokens)
}

sql_clause_start_at :: proc(tokens: []Header_Token, i: int) -> bool {
	if token_eq(tokens[i], "FROM") ||
	   token_eq(tokens[i], "FIELDS") ||
	   token_eq(tokens[i], "INTO") ||
	   token_eq(tokens[i], "APPENDING") ||
	   token_eq(tokens[i], "WHERE") ||
	   token_eq(tokens[i], "HAVING") ||
	   token_eq(tokens[i], "OFFSET") ||
	   token_eq(tokens[i], "BYPASSING") ||
	   token_eq(tokens[i], "CONNECTION") ||
	   token_eq(tokens[i], "CLIENT") ||
	   token_eq(tokens[i], "UNION") ||
	   token_eq(tokens[i], "INTERSECT") ||
	   token_eq(tokens[i], "EXCEPT") {
		return true
	}
	if i + 1 < len(tokens) {
		return(
			(token_eq(tokens[i], "GROUP") && token_eq(tokens[i + 1], "BY")) ||
			(token_eq(tokens[i], "ORDER") && token_eq(tokens[i + 1], "BY")) ||
			(token_eq(tokens[i], "UP") && token_eq(tokens[i + 1], "TO")) ||
			(token_eq(tokens[i], "PACKAGE") && token_eq(tokens[i + 1], "SIZE")) ||
			(token_eq(tokens[i], "FOR") &&
					(token_eq(tokens[i + 1], "ALL") || token_eq(tokens[i + 1], "UPDATE"))) \
		)
	}
	return false
}

sql_find_keyword :: proc(tokens: []Header_Token, keyword: string) -> int {
	for token, i in tokens {
		if token_eq(token, keyword) {
			return i
		}
	}
	return -1
}

sql_find_phrase :: proc(tokens: []Header_Token, phrase: []string) -> int {
	if len(phrase) == 0 || len(phrase) > len(tokens) {
		return -1
	}
	for i in 0 ..= len(tokens) - len(phrase) {
		ok := true
		for j in 0 ..< len(phrase) {
			if !token_eq(tokens[i + j], phrase[j]) {
				ok = false
				break
			}
		}
		if ok {
			return i
		}
	}
	return -1
}

sql_order_by_info :: proc(
	c: ^Collector,
	tokens: []Header_Token,
	order_idx: int,
) -> (
	bool,
	[dynamic]string,
) {
	fields := make([dynamic]string, 0, 2, c.allocator)
	i := order_idx + 2
	if i + 1 < len(tokens) && token_eq(tokens[i], "PRIMARY") && token_eq(tokens[i + 1], "KEY") {
		return true, fields
	}
	end := sql_next_clause_index(tokens, i)
	for i < end {
		if tokens[i].kind == .Comma ||
		   token_eq(tokens[i], "ASCENDING") ||
		   token_eq(tokens[i], "NULLS") ||
		   token_eq(tokens[i], "FIRST") ||
		   token_eq(tokens[i], "LAST") {
			i += 1
			continue
		}
		if token_eq(tokens[i], "DESCENDING") {
			return false, make([dynamic]string, 0, 0, c.allocator)
		}
		if tokens[i].kind == .Ident {
			if i + 2 < end && tokens[i + 1].kind == .Tilde && tokens[i + 2].kind == .Ident {
				append(&fields, canonical_name(tokens[i + 2].text, c.allocator))
				i += 3
			} else {
				append(&fields, canonical_name(tokens[i].text, c.allocator))
				i += 1
			}
			continue
		}
		i += 1
	}
	return false, fields
}

sql_tokens_are_dynamic_operand :: proc(tokens: []Header_Token) -> bool {
	if len(tokens) < 3 || tokens[0].kind != .LParen || tokens[len(tokens) - 1].kind != .RParen {
		return false
	}
	depth := 0
	for token, i in tokens {
		if token.kind == .LParen {
			depth += 1
		} else if token.kind == .RParen {
			depth -= 1
			if depth == 0 && i + 1 != len(tokens) {
				return false
			}
		}
		if depth < 0 {
			return false
		}
	}
	return depth == 0
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

sql_qualified_star_text :: proc(
	c: ^Collector,
	expr: ^ast.Expr,
) -> (
	string,
	tokenizer.Range,
	bool,
) {
	if expr == nil {
		return "", tokenizer.Range{}, false
	}
	text := source_text(c, expr.range)
	tokens := header_tokens(c, text, expr.range.start)
	if len(tokens) == 3 &&
	   tokens[0].kind == .Ident &&
	   tokens[1].kind == .Tilde &&
	   tokens[2].kind == .Star {
		return tokens[0].text, expr.range, true
	}
	return "", tokenizer.Range{}, false
}

sql_call_ref_kind :: proc(name: string) -> Sql_Name_Ref_Kind {
	if ascii_equal_ignore_case(name, "avg") ||
	   ascii_equal_ignore_case(name, "count") ||
	   ascii_equal_ignore_case(name, "max") ||
	   ascii_equal_ignore_case(name, "min") ||
	   ascii_equal_ignore_case(name, "sum") ||
	   ascii_equal_ignore_case(name, "median") ||
	   ascii_equal_ignore_case(name, "stddev") ||
	   ascii_equal_ignore_case(name, "var") ||
	   ascii_equal_ignore_case(name, "corr") ||
	   ascii_equal_ignore_case(name, "corr_spearman") ||
	   ascii_equal_ignore_case(name, "grouping") ||
	   ascii_equal_ignore_case(name, "string_agg") ||
	   ascii_equal_ignore_case(name, "allow_precision_loss") {
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
) -> Structure_Id {
	fields := make([dynamic]Structure_Field_Data, 0, 4, c.allocator)
	for projection in c.sql_projections {
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
				decl_unit = c.unit_id,
				structure = INVALID_STRUCTURE_ID,
				type_ref = type_ref,
				flags = flags,
			},
		)
	}
	if len(fields) == 0 {
		return INVALID_STRUCTURE_ID
	}
	return push_collected_structure(c, concat3(c, "<open_sql_inline:", target_name, ">"), fields)
}

inline_select_target_type :: proc(c: ^Collector, query_id: int) -> (Field_Type_Ref_Data, bool) {
	count := 0
	out := Field_Type_Ref_Data{}
	for projection in c.sql_projections {
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
		out = merge_range(out, expr_range(projection.value))
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
		if ascii_equal_ignore_case(name, n) {
			return true
		}
	}
	return false
}

sql_local_value_exists :: proc(c: ^Collector, scope: Scope_Id, name: string) -> bool {
	_, ok := lookup_symbol_in_scope_chain(c, scope, name, .Value)
	return ok
}

sql_token_is_keyword_text :: proc(text: string) -> bool {
	keywords := [?]string {
		"select",
		"single",
		"distinct",
		"case",
		"when",
		"then",
		"else",
		"end",
		"from",
		"into",
		"appending",
		"where",
		"with",
		"group",
		"by",
		"having",
		"order",
		"for",
		"update",
		"all",
		"entries",
		"in",
		"up",
		"to",
		"rows",
		"package",
		"size",
		"offset",
		"bypassing",
		"buffer",
		"connection",
		"client",
		"specified",
		"privileged",
		"access",
		"union",
		"intersect",
		"except",
		"as",
		"join",
		"inner",
		"left",
		"right",
		"cross",
		"on",
		"and",
		"or",
		"not",
		"eq",
		"ne",
		"lt",
		"le",
		"gt",
		"ge",
		"co",
		"cn",
		"ca",
		"na",
		"cs",
		"ns",
		"cp",
		"np",
		"like",
		"between",
		"is",
		"null",
		"nulls",
		"first",
		"last",
		"table",
		"corresponding",
		"fields",
		"of",
		"primary",
		"key",
	}
	for keyword in keywords {
		if ascii_equal_ignore_case(text, keyword) {
			return true
		}
	}
	return false
}

record_internal_table_order :: proc(
	c: ^Collector,
	scope: Scope_Id,
	range: tokenizer.Range,
	table_name: string,
	key_fields: []string,
) {
	if table_name == "" || len(key_fields) == 0 {
		return
	}
	keys := make([dynamic]string, 0, len(key_fields), c.allocator)
	for key in key_fields {
		append(&keys, canonical_name(key, c.allocator))
	}
	append(
		&c.internal_table_orders,
		Internal_Table_Order_Data {
			scope = scope,
			range = range,
			table_name = canonical_name(table_name, c.allocator),
			key_fields = keys,
		},
	)
}

record_read_table_binary_search :: proc(
	c: ^Collector,
	scope: Scope_Id,
	range: tokenizer.Range,
	table_name: string,
	key_fields: []string,
) {
	keys := make([dynamic]string, 0, len(key_fields), c.allocator)
	for key in key_fields {
		append(&keys, canonical_name(key, c.allocator))
	}
	append(
		&c.read_table_binary_searches,
		Read_Table_Binary_Search_Data {
			scope = scope,
			range = range,
			table_name = canonical_name(table_name, c.allocator),
			key_fields = keys,
		},
	)
}

binary_search_range :: proc(c: ^Collector, range: tokenizer.Range) -> tokenizer.Range {
	text := source_text(c, range)
	tokens := header_tokens(c, text, range.start)
	for i in 0 ..< len(tokens) - 1 {
		if token_eq(tokens[i], "BINARY") && token_eq(tokens[i + 1], "SEARCH") {
			return tokenizer.text_range(tokens[i].range.start, tokens[i + 1].range.end)
		}
	}
	return range
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
