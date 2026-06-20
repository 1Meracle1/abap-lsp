package abap_frontend_lints

import "src:ast"
import "src:semantic"
import "src:utils"

import "core:mem"
import "core:strings"

Missing_Tables_Report_Context :: struct {
	enabled:        bool,
	provider_index: ^semantic.External_Semantic_Index,
	table_names:    [dynamic]string,
}


missing_tables_context_for_project_result :: proc(
	analysis: ^semantic.Workspace_Analysis,
	project_result: ^semantic.Workspace_Project_Result,
	allocator: mem.Allocator,
) -> Missing_Tables_Report_Context {
	ctx := Missing_Tables_Report_Context {
		provider_index = &analysis.external_index if analysis != nil else nil,
		table_names    = make([dynamic]string, 0, 2, allocator),
	}
	if analysis == nil || project_result == nil || !missing_tables_project_result_is_report(analysis, project_result) {
		return ctx
	}
	ctx.enabled = true
	for file in project_result.files {
		missing_tables_collect_file_tables(&ctx, file, allocator)
	}
	return ctx
}

missing_tables_project_result_is_report :: proc(
	analysis: ^semantic.Workspace_Analysis,
	project_result: ^semantic.Workspace_Project_Result,
) -> bool {
	if analysis == nil || project_result == nil || project_result.kind != .Root {
		return false
	}
	for facts in analysis.discovery.facts {
		if facts.path == project_result.root_path {
			return facts.kind == .Report
		}
	}
	if len(project_result.files) == 0 || project_result.files[0] == nil || project_result.files[0].root == nil {
		return false
	}
	for stmt in project_result.files[0].root.stmts {
		if report, ok := stmt.derived_stmt.(^ast.Report_Stmt); ok && (report.kind == .Report || report.kind == .Program) {
			return true
		}
	}
	return false
}

missing_tables_collect_file_tables :: proc(
	ctx: ^Missing_Tables_Report_Context,
	file: ^semantic.Project_File,
	allocator: mem.Allocator,
) {
	if ctx == nil || file == nil || file.root == nil {
		return
	}
	for stmt in file.root.stmts {
		decl, ok := stmt.derived_stmt.(^ast.Tables_Decl)
		if !ok {
			continue
		}
		for clause in decl.tables {
			name := utils.to_lower_ascii(clause.name.text, allocator)
			if name != "" && !guard_list_contains(ctx.table_names[:], name) {
				append(&ctx.table_names, name)
			}
		}
	}
}

emit_missing_tables_declaration_lints :: proc(
	out: ^Unit_Lints,
	report_context: ^Missing_Tables_Report_Context,
	policy: ^Policy,
	allocator: mem.Allocator,
) {
	if out == nil || out.project == nil || out.checker == nil || out.file == nil || out.file.root == nil {
		return
	}
	if report_context == nil || !report_context.enabled {
		return
	}
	metadata, metadata_ok := metadata_for(EPC_MISSING_TABLES_DECLARATION)
	if !metadata_ok {
		return
	}
	query := semantic.semantic_query(out.project, out.checker, out.file)
	ref_query := semantic.semantic_query_refs(query)
	for stmt in out.file.root.stmts {
		missing_tables_visit_selection_screen_stmt(
			out,
			report_context,
			ref_query,
			metadata,
			stmt,
			policy,
			allocator,
		)
	}
}

missing_tables_visit_selection_screen_stmt :: proc(
	out: ^Unit_Lints,
	report_context: ^Missing_Tables_Report_Context,
	ref_query: semantic.Semantic_Ref_Query,
	metadata: Metadata,
	stmt: ^ast.Stmt,
	policy: ^Policy,
	allocator: mem.Allocator,
) {
	if stmt == nil {
		return
	}
	#partial switch n in stmt.derived_stmt {
	case ^ast.Parameters_Decl:
		for clause in n.parameters {
			missing_tables_emit_for_type_clause(
				out,
				report_context,
				ref_query,
				metadata,
				clause.type_clause,
				policy,
				allocator,
			)
		}
	case ^ast.Select_Options_Decl:
		for clause in n.options {
			missing_tables_emit_for_report_type_expr(
				out,
				report_context,
				ref_query,
				metadata,
				clause.for_expr,
				policy,
				allocator,
			)
		}
	case ^ast.Ranges_Decl:
		for clause in n.ranges {
			missing_tables_emit_for_report_type_expr(
				out,
				report_context,
				ref_query,
				metadata,
				clause.for_expr,
				policy,
				allocator,
			)
		}
	}
}

missing_tables_emit_for_type_clause :: proc(
	out: ^Unit_Lints,
	report_context: ^Missing_Tables_Report_Context,
	ref_query: semantic.Semantic_Ref_Query,
	metadata: Metadata,
	type_clause: ^ast.Data_Type_Clause,
	policy: ^Policy,
	allocator: mem.Allocator,
) {
	if type_clause == nil || type_clause.type_ref == nil {
		return
	}
	name, range, ok := missing_tables_type_ref_base(type_clause.type_ref, allocator)
	if !ok {
		return
	}
	missing_tables_emit_for_ref(out, report_context, ref_query, metadata, name, range, policy, allocator)
}

missing_tables_emit_for_report_type_expr :: proc(
	out: ^Unit_Lints,
	report_context: ^Missing_Tables_Report_Context,
	ref_query: semantic.Semantic_Ref_Query,
	metadata: Metadata,
	expr: ^ast.Expr,
	policy: ^Policy,
	allocator: mem.Allocator,
) {
	if name, range, ok := missing_tables_type_ref_base(expr, allocator); ok {
		missing_tables_emit_for_ref(
			out,
			report_context,
			ref_query,
			metadata,
			name,
			range,
			policy,
			allocator,
		)
		return
	}
	access, ok := value_access_from_expr(expr, allocator)
	if !ok {
		return
	}
	missing_tables_emit_for_ref(
		out,
		report_context,
		ref_query,
		metadata,
		access.base_name,
		access.base_range,
		policy,
		allocator,
	)
}

missing_tables_type_ref_base :: proc(
	expr: ^ast.Expr,
	allocator: mem.Allocator,
) -> (
	string,
	semantic.Range,
	bool,
) {
	if expr == nil {
		return "", {}, false
	}
	#partial switch n in expr.derived_expr {
	case ^ast.Type_Ref_Expr:
		base := n.base_name
		if base.text == "" {
			base = n.name
		}
		if base.text == "" {
			return "", {}, false
		}
		range := base.range if base.range.end > base.range.start else n.range
		return utils.to_lower_ascii(base.text, allocator), range, true
	case ^ast.Ident_Expr:
		if n.name == "" {
			return "", {}, false
		}
		return utils.to_lower_ascii(n.name, allocator), n.range, true
	case ^ast.Literal_Expr:
		if n.value == "" {
			return "", {}, false
		}
		return utils.to_lower_ascii(n.value, allocator), n.range, true
	}
	return "", {}, false
}

missing_tables_emit_for_ref :: proc(
	out: ^Unit_Lints,
	report_context: ^Missing_Tables_Report_Context,
	ref_query: semantic.Semantic_Ref_Query,
	metadata: Metadata,
	name: string,
	range: semantic.Range,
	policy: ^Policy,
	allocator: mem.Allocator,
) {
	if name == "" || range.end <= range.start || guard_list_contains(report_context.table_names[:], name) {
		return
	}
	use := semantic.semantic_ref_use_at_range(ref_query, range)
	if use != nil && use.entity != nil {
		if !missing_tables_entity_is_ddic_table_like(report_context, use.entity) {
			return
		}
	} else {
		if missing_tables_local_declaration_exists(out, name) ||
		   !missing_tables_provider_has_ddic_table(report_context, name) {
			return
		}
	}
	builder := strings.builder_make(context.temp_allocator)
	strings.write_string(&builder, "DDIC table/view '")
	strings.write_string(&builder, name)
	strings.write_string(&builder, "' is used as a report type without a top-level TABLES ")
	strings.write_string(&builder, name)
	strings.write_string(&builder, " declaration in the report or its includes")
	emit_diagnostic(out, metadata, range, strings.to_string(builder), policy, allocator)
}

missing_tables_local_declaration_exists :: proc(out: ^Unit_Lints, name: string) -> bool {
	if out == nil || out.file == nil || out.file.root_scope == nil || name == "" {
		return false
	}
	if _, found := semantic.scope_lookup_declaration(out.file.root_scope, .Value, name); found {
		return true
	}
	if _, found := semantic.scope_lookup_declaration(out.file.root_scope, .Type, name); found {
		return true
	}
	return false
}

missing_tables_provider_has_ddic_table :: proc(
	report_context: ^Missing_Tables_Report_Context,
	name: string,
) -> bool {
	if report_context == nil || report_context.provider_index == nil || name == "" {
		return false
	}
	_, ok := report_context.provider_index.providers[semantic.Semantic_Object_Key {
		kind = .DDIC_Table,
		name = name,
	}]
	return ok
}

missing_tables_entity_is_ddic_table_like :: proc(
	report_context: ^Missing_Tables_Report_Context,
	entity: ^semantic.Entity,
) -> bool {
	if report_context == nil || report_context.provider_index == nil || entity == nil {
		return false
	}
	for key, binding in report_context.provider_index.providers {
		if key.kind == .DDIC_Table && binding.entity == entity {
			return true
		}
	}
	return false
}
