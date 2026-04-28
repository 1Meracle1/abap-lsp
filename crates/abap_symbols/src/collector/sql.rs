use std::sync::Arc;

use abap_ast::SyntaxKind;
use abap_ast::arena::NodeId;
use abap_ast::ast::{
    AstNode, DataDeclName, SelectIntoClause, SelectJoinClause, SelectProjectionList, SelectQuery,
    SelectStmt, SelectWithClause, SqlAggregateCall, SqlColumnRef, SqlDataSource, SqlProjectionItem,
    SqlQualifiedColumnRef, SqlQualifiedStar,
};
use abap_lexer::TextRange;

use crate::def_map::{
    Diagnostic, DiagnosticKind, ReferenceKind, SqlDynamicFragmentData, SqlDynamicFragmentKind,
    SqlNameRefData, SqlNameRefKind, SqlPredicateData, SqlPredicateKind, SqlProjectionData,
    SqlProjectionKind, SqlQueryData, SqlResolution, SqlSourceData, SqlSourceKind, SqlTargetData,
    SqlTargetKind, SystemFieldStatementKind,
};
use crate::ids::ScopeId;
use crate::ids::StructureId;
use crate::scope::{Namespace, ScopeKind};

use super::context::SqlContext;
use super::{
    Collector, PendingStructure, PendingStructureField, PendingStructureMember, SqlClauseKind,
    SyntaxTokenInfo,
};

pub(super) struct SqlLowering<'ctx, 'a> {
    ctx: SqlContext<'ctx, 'a>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
struct SelectOrderByInfo {
    primary_key: bool,
    fields: Vec<Arc<str>>,
}

impl<'a> Collector<'a> {
    pub(super) fn sql_lowering(&mut self) -> SqlLowering<'_, 'a> {
        SqlLowering {
            ctx: SqlContext::new(self),
        }
    }

    pub(super) fn sql_target_name_from_expr(&self, node: NodeId) -> Option<Arc<str>> {
        match self.file.kind(node) {
            SyntaxKind::ExprIdent => self.node_name(node).map(|(name, _)| name),
            SyntaxKind::SelectorExpr => self
                .selector_access_chain(node)
                .map(|(_, base_name, _, _)| base_name),
            _ => None,
        }
    }
}

impl<'ctx, 'a> SqlLowering<'ctx, 'a> {
    fn lower_arc(text: &str) -> Arc<str> {
        Arc::<str>::from(text.to_ascii_lowercase())
    }

    fn emit_dynamic_fragment(
        &mut self,
        query_id: usize,
        scope: ScopeId,
        range: TextRange,
        kind: SqlDynamicFragmentKind,
    ) {
        self.ctx.emit_sql_dynamic_fragment(SqlDynamicFragmentData {
            query_id,
            scope,
            range,
            kind,
        });
    }

    fn record_system_field_updates(
        &mut self,
        scope: ScopeId,
        node: NodeId,
        statement: SystemFieldStatementKind,
    ) {
        let range = self.ctx.file().range(node);
        for field_name in ["subrc", "dbcnt"] {
            self.ctx
                .add_system_field_update(scope, range.clone(), statement, field_name);
        }
    }

    pub(super) fn collect_select_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_system_field_updates(scope, node, SystemFieldStatementKind::Select);
        let Some(stmt) = SelectStmt::cast(self.ctx.syntax(node)) else {
            self.ctx.walk_children(node, scope);
            return;
        };
        let query_node = stmt.query().map(|query| query.syntax().id());
        let with_clause_node = stmt
            .with_clause()
            .map(|with_clause| with_clause.syntax().id());
        let cte_names = with_clause_node
            .map(|with_clause| self.select_cte_names(with_clause))
            .unwrap_or_default();
        let cte_query_nodes = with_clause_node
            .map(|with_clause| self.select_cte_query_nodes(with_clause))
            .unwrap_or_default();
        let non_query_children: Vec<_> = stmt
            .non_query_children()
            .map(|child| child.id())
            .filter(|child| Some(*child) != with_clause_node)
            .collect();
        let has_endselect = self.ctx.control_lowering().select_stmt_has_endselect(node);
        if has_endselect {
            let range = self.ctx.file().range(node);
            let child_scope = self
                .ctx
                .push_scope(ScopeKind::SelectBlock, range, Some(scope), None);
            for cte_query_node in &cte_query_nodes {
                self.collect_select_query(*cte_query_node, child_scope, false, &cte_names);
            }
            if let Some(query_node) = query_node {
                self.collect_select_query(query_node, child_scope, true, &cte_names);
            }
            for child in non_query_children {
                self.ctx.walk_node(child, child_scope);
            }
        } else {
            for cte_query_node in &cte_query_nodes {
                self.collect_select_query(*cte_query_node, scope, false, &cte_names);
            }
            if let Some(query_node) = query_node {
                self.collect_select_query(query_node, scope, false, &cte_names);
            }
            for child in non_query_children {
                self.ctx.walk_node(child, scope);
            }
        }
    }

    fn select_cte_names(&self, with_clause_node: NodeId) -> Vec<Arc<str>> {
        let Some(with_clause) = SelectWithClause::cast(self.ctx.syntax(with_clause_node)) else {
            return Vec::new();
        };
        with_clause
            .definitions()
            .filter_map(|definition| self.select_cte_definition_name(definition.syntax().id()))
            .collect()
    }

    fn select_cte_query_nodes(&self, with_clause_node: NodeId) -> Vec<NodeId> {
        let Some(with_clause) = SelectWithClause::cast(self.ctx.syntax(with_clause_node)) else {
            return Vec::new();
        };
        with_clause
            .definitions()
            .filter_map(|definition| definition.query().map(|query| query.syntax().id()))
            .collect()
    }

    fn select_cte_definition_name(&self, definition_node: NodeId) -> Option<Arc<str>> {
        let tokens: Vec<_> = self
            .ctx
            .syntax_token_nodes(definition_node)
            .into_iter()
            .filter(|token| !self.ctx.syntax_token_is_comment(token))
            .collect();
        let first = tokens.first()?;
        if first.text.as_ref() == "+"
            && let Some(name) = tokens
                .get(1)
                .filter(|token| self.ctx.syntax_token_is_ident_like(token))
        {
            return Some(Arc::<str>::from(format!(
                "+{}",
                name.text.to_ascii_lowercase()
            )));
        }
        self.ctx
            .syntax_token_is_ident_like(first)
            .then(|| Self::lower_arc(first.text.as_ref()))
    }

    pub(super) fn collect_insert_db_table_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_system_field_updates(scope, node, SystemFieldStatementKind::InsertDbTable);
        let query_id = self.ctx.sql_queries_len();
        let range = self.ctx.file().range(node);
        self.ctx.emit_sql_query(SqlQueryData {
            id: query_id,
            scope,
            range: range.clone(),
            projection_clause: None,
            from_clause: Some(range),
            into_clause: None,
            where_clause: None,
            group_by_clause: None,
            having_clause: None,
            order_by_clause: None,
            order_by_primary_key: false,
            order_by_fields: Vec::new(),
            for_all_entries_clause: None,
            for_update_clause: None,
            up_to_clause: None,
            package_size_clause: None,
            offset_clause: None,
            abap_options_clause: None,
            set_operator_clause: None,
            is_single: false,
            is_distinct: false,
            is_for_update: false,
            has_package_size: false,
            has_set_operators: false,
            has_endselect: false,
            has_dynamic_where: false,
        });

        let children: Vec<_> = self
            .ctx
            .syntax(node)
            .children()
            .map(|child| (child.id(), child.kind()))
            .collect();
        for (child, kind_syntax) in children {
            match kind_syntax {
                SyntaxKind::SqlDataSource => {
                    self.collect_insert_db_table_target(query_id, child, scope);
                }
                SyntaxKind::ExprIdent
                | SyntaxKind::SelectorExpr
                | SyntaxKind::CallExpr
                | SyntaxKind::BinaryExpr
                | SyntaxKind::UnaryExpr
                | SyntaxKind::ParenExpr
                | SyntaxKind::ConstructorExpr
                | SyntaxKind::TemplateExpr => self.ctx.expr_lowering().collect_expr(child, scope),
                SyntaxKind::SqlHostExpr => self.collect_sql_host_refs_from_node(child, scope),
                _ => {}
            }
        }
    }

    pub(super) fn collect_delete_db_table_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_system_field_updates(scope, node, SystemFieldStatementKind::DeleteDbTable);
        let mut head_expr = None;
        let mut from_expr = None;
        let mut where_expr = None;
        let mut saw_from = false;
        let mut saw_where = false;
        let mut expect_from_expr = false;

        let children: Vec<_> = self
            .ctx
            .syntax(node)
            .children()
            .map(|child| (child.id(), child.kind()))
            .collect();
        for (child, kind_syntax) in children {
            if kind_syntax == SyntaxKind::Token {
                if let Some(text) = self.ctx.syntax(child).text(self.ctx.source()) {
                    if text.eq_ignore_ascii_case("from") {
                        saw_from = true;
                        saw_where = false;
                        expect_from_expr = true;
                    } else if text.eq_ignore_ascii_case("where") {
                        saw_where = true;
                        expect_from_expr = false;
                    } else if expect_from_expr && text.eq_ignore_ascii_case("table") {
                        continue;
                    }
                }
                continue;
            }

            if head_expr.is_none() {
                head_expr = Some(child);
                continue;
            }
            if expect_from_expr {
                from_expr = Some(child);
                expect_from_expr = false;
                continue;
            }
            if saw_where && where_expr.is_none() {
                where_expr = Some(child);
            }
        }

        let static_head = head_expr.and_then(|expr| self.simple_sql_source_name_from_expr(expr));
        let dynamic_head = if static_head.is_none() {
            head_expr.and_then(|expr| self.dynamic_parenthesized_operand_tokens_from_node(expr))
        } else {
            None
        };
        if static_head.is_none() && dynamic_head.is_none() {
            self.ctx.walk_children(node, scope);
            return;
        };

        let query_id = self.ctx.sql_queries_len();
        let range = self.ctx.file().range(node);
        let has_dynamic_where = where_expr.is_some_and(|expr| {
            let predicate_tokens = self.ctx.syntax_token_nodes(expr);
            Self::sql_tokens_are_dynamic_where(&predicate_tokens)
        });
        self.ctx.emit_sql_query(SqlQueryData {
            id: query_id,
            scope,
            range: range.clone(),
            projection_clause: None,
            from_clause: saw_from.then_some(range.clone()),
            into_clause: None,
            where_clause: where_expr.map(|expr| self.ctx.file().range(expr)),
            group_by_clause: None,
            having_clause: None,
            order_by_clause: None,
            order_by_primary_key: false,
            order_by_fields: Vec::new(),
            for_all_entries_clause: None,
            for_update_clause: None,
            up_to_clause: None,
            package_size_clause: None,
            offset_clause: None,
            abap_options_clause: None,
            set_operator_clause: None,
            is_single: false,
            is_distinct: false,
            is_for_update: false,
            has_package_size: false,
            has_set_operators: false,
            has_endselect: false,
            has_dynamic_where,
        });

        let source_range = head_expr
            .map(|expr| self.ctx.file().range(expr))
            .unwrap_or_else(|| range.clone());
        if let Some((name, name_range)) = static_head {
            self.ctx.emit_sql_source(SqlSourceData {
                query_id,
                range: source_range,
                source_kind: SqlSourceKind::From,
                name: Arc::clone(&name),
                alias: None,
                join_kind: None,
                resolution: SqlResolution::External,
            });
            self.push_sql_name_ref(
                query_id,
                scope,
                name_range,
                name,
                None,
                SqlNameRefKind::Source,
            );
        } else if let Some(dynamic_tokens) = dynamic_head {
            self.ctx
                .collect_token_expression_refs_infos(&dynamic_tokens, scope, true);
            self.emit_dynamic_fragment(
                query_id,
                scope,
                source_range,
                SqlDynamicFragmentKind::Source,
            );
        }

        if let Some(from_expr) = from_expr {
            self.ctx.walk_node(from_expr, scope);
        }
        if let Some(where_expr) = where_expr {
            let predicate_tokens = self.ctx.syntax_token_nodes(where_expr);
            let predicate_kind = if Self::sql_tokens_are_dynamic_where(&predicate_tokens) {
                SqlPredicateKind::DynamicWhere
            } else {
                SqlPredicateKind::Where
            };
            self.ctx.emit_sql_predicate(SqlPredicateData {
                query_id,
                range: self.ctx.file().range(where_expr),
                kind: predicate_kind,
            });
            if predicate_kind == SqlPredicateKind::DynamicWhere {
                self.emit_dynamic_fragment(
                    query_id,
                    scope,
                    self.ctx.file().range(where_expr),
                    SqlDynamicFragmentKind::Where,
                );
                if let Some(dynamic_tokens) =
                    Self::dynamic_parenthesized_operand_tokens(&predicate_tokens)
                {
                    self.ctx
                        .collect_token_expression_refs_infos(dynamic_tokens, scope, true);
                }
            } else {
                self.collect_sql_host_and_name_refs_in_node(query_id, where_expr, scope, true);
            }
        }
    }

    pub(super) fn collect_modify_db_table_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_system_field_updates(scope, node, SystemFieldStatementKind::ModifyDbTable);
        let mut head_expr = None;
        let mut from_expr = None;
        let mut saw_from = false;
        let mut expect_from_expr = false;

        let children: Vec<_> = self
            .ctx
            .syntax(node)
            .children()
            .map(|child| (child.id(), child.kind()))
            .collect();
        for (child, kind_syntax) in children {
            if kind_syntax == SyntaxKind::Token {
                if let Some(text) = self.ctx.syntax(child).text(self.ctx.source()) {
                    if text.eq_ignore_ascii_case("from") {
                        saw_from = true;
                        expect_from_expr = true;
                    } else if expect_from_expr && text.eq_ignore_ascii_case("table") {
                        continue;
                    }
                }
                continue;
            }

            if head_expr.is_none() {
                head_expr = Some(child);
                continue;
            }
            if expect_from_expr {
                from_expr = Some(child);
                expect_from_expr = false;
            }
        }

        let static_head = head_expr.and_then(|expr| self.simple_sql_source_name_from_expr(expr));
        let dynamic_head = if static_head.is_none() {
            head_expr.and_then(|expr| self.dynamic_parenthesized_operand_tokens_from_node(expr))
        } else {
            None
        };
        if static_head.is_none() && dynamic_head.is_none() {
            self.ctx.walk_children(node, scope);
            return;
        };

        let query_id = self.ctx.sql_queries_len();
        let range = self.ctx.file().range(node);
        self.ctx.emit_sql_query(SqlQueryData {
            id: query_id,
            scope,
            range: range.clone(),
            projection_clause: None,
            from_clause: saw_from.then_some(range.clone()),
            into_clause: None,
            where_clause: None,
            group_by_clause: None,
            having_clause: None,
            order_by_clause: None,
            order_by_primary_key: false,
            order_by_fields: Vec::new(),
            for_all_entries_clause: None,
            for_update_clause: None,
            up_to_clause: None,
            package_size_clause: None,
            offset_clause: None,
            abap_options_clause: None,
            set_operator_clause: None,
            is_single: false,
            is_distinct: false,
            is_for_update: false,
            has_package_size: false,
            has_set_operators: false,
            has_endselect: false,
            has_dynamic_where: false,
        });

        let source_range = head_expr
            .map(|expr| self.ctx.file().range(expr))
            .unwrap_or_else(|| range.clone());
        if let Some((name, name_range)) = static_head {
            self.ctx.emit_sql_source(SqlSourceData {
                query_id,
                range: source_range,
                source_kind: SqlSourceKind::From,
                name: Arc::clone(&name),
                alias: None,
                join_kind: None,
                resolution: SqlResolution::External,
            });
            self.push_sql_name_ref(
                query_id,
                scope,
                name_range,
                name,
                None,
                SqlNameRefKind::Source,
            );
        } else if let Some(dynamic_tokens) = dynamic_head {
            self.ctx
                .collect_token_expression_refs_infos(&dynamic_tokens, scope, true);
            self.emit_dynamic_fragment(
                query_id,
                scope,
                source_range,
                SqlDynamicFragmentKind::Source,
            );
        }

        if let Some(from_expr) = from_expr {
            self.ctx.walk_node(from_expr, scope);
        }
    }

    pub(super) fn collect_update_db_table_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_system_field_updates(scope, node, SystemFieldStatementKind::UpdateDbTable);
        let mut target_node = None;
        let mut from_node = None;
        let mut set_assignments = Vec::new();
        let mut set_value_nodes = Vec::new();

        let children: Vec<_> = self
            .ctx
            .syntax(node)
            .children()
            .map(|child| (child.id(), child.kind()))
            .collect();
        for (child, kind_syntax) in children {
            match kind_syntax {
                SyntaxKind::UpdateTarget => {
                    target_node = self
                        .ctx
                        .syntax(child)
                        .first_non_token_child()
                        .map(|target| target.id());
                }
                SyntaxKind::UpdateSetClause => {
                    for assignment in self.ctx.syntax(child).children() {
                        if assignment.kind() != SyntaxKind::UpdateSetAssignment {
                            continue;
                        }
                        set_assignments.push(assignment.id());
                        if let Some(value_node) = self
                            .ctx
                            .syntax(assignment.id())
                            .child_by_kind(SyntaxKind::UpdateSetValueOperand)
                            .and_then(|value| value.first_non_token_child())
                        {
                            set_value_nodes.push(value_node.id());
                        }
                    }
                }
                SyntaxKind::UpdateFromOperand => {
                    from_node = self
                        .ctx
                        .syntax(child)
                        .first_non_token_child()
                        .map(|value| value.id());
                }
                _ => {}
            }
        }

        let stmt_tokens: Vec<_> = self
            .ctx
            .syntax_token_nodes(node)
            .into_iter()
            .filter(|token| !self.ctx.syntax_token_is_comment(token))
            .collect();
        let where_idx = stmt_tokens
            .iter()
            .position(|token| token.text.eq_ignore_ascii_case("where"));
        let where_end = stmt_tokens
            .last()
            .filter(|token| token.text.as_ref() == ".")
            .map(|_| stmt_tokens.len().saturating_sub(1))
            .unwrap_or(stmt_tokens.len());
        let where_range = where_idx.and_then(|where_idx| {
            (where_idx + 1 < where_end)
                .then(|| stmt_tokens[where_idx].range.start..stmt_tokens[where_end - 1].range.end)
        });
        let has_dynamic_where = where_idx
            .and_then(|where_idx| stmt_tokens.get(where_idx + 1..where_end))
            .is_some_and(Self::sql_tokens_are_dynamic_where);

        let query_id = self.ctx.sql_queries_len();
        let range = self.ctx.file().range(node);
        let target_range = target_node.map(|target| self.ctx.file().range(target));
        self.ctx.emit_sql_query(SqlQueryData {
            id: query_id,
            scope,
            range: range,
            projection_clause: None,
            from_clause: target_range,
            into_clause: None,
            where_clause: where_range,
            group_by_clause: None,
            having_clause: None,
            order_by_clause: None,
            order_by_primary_key: false,
            order_by_fields: Vec::new(),
            for_all_entries_clause: None,
            for_update_clause: None,
            up_to_clause: None,
            package_size_clause: None,
            offset_clause: None,
            abap_options_clause: None,
            set_operator_clause: None,
            is_single: false,
            is_distinct: false,
            is_for_update: false,
            has_package_size: false,
            has_set_operators: false,
            has_endselect: false,
            has_dynamic_where,
        });

        if let Some(target_node) = target_node {
            self.collect_insert_db_table_target(query_id, target_node, scope);
        }

        for assignment in set_assignments {
            let tokens = self.ctx.syntax_token_nodes(assignment);
            let eq_idx = tokens
                .iter()
                .position(|token| matches!(token.text.as_ref(), "=" | "?="));
            if let Some(eq_idx) = eq_idx {
                self.collect_sql_name_refs_from_syntax_tokens(
                    query_id,
                    scope,
                    &tokens[..eq_idx],
                    false,
                );
            }
        }

        for value_node in set_value_nodes {
            self.collect_sql_host_refs_from_node(value_node, scope);
            self.ctx.walk_node(value_node, scope);
        }

        if let Some(from_node) = from_node {
            self.collect_sql_host_refs_from_node(from_node, scope);
            self.ctx.walk_node(from_node, scope);
        }

        if let Some(where_idx) = where_idx
            && where_idx + 1 < where_end
        {
            let predicate_tokens = &stmt_tokens[where_idx + 1..where_end];
            let predicate_range =
                stmt_tokens[where_idx].range.start..stmt_tokens[where_end - 1].range.end;
            let predicate_kind = if Self::sql_tokens_are_dynamic_where(predicate_tokens) {
                SqlPredicateKind::DynamicWhere
            } else {
                SqlPredicateKind::Where
            };
            self.ctx.emit_sql_predicate(SqlPredicateData {
                query_id,
                range: predicate_range,
                kind: predicate_kind,
            });
            if predicate_kind == SqlPredicateKind::DynamicWhere {
                self.emit_dynamic_fragment(
                    query_id,
                    scope,
                    stmt_tokens[where_idx + 1].range.start..stmt_tokens[where_end - 1].range.end,
                    SqlDynamicFragmentKind::Where,
                );
                if predicate_tokens.len() > 2 {
                    self.ctx.collect_token_expression_refs_infos(
                        &predicate_tokens[1..predicate_tokens.len() - 1],
                        scope,
                        true,
                    );
                }
            } else {
                self.collect_sql_host_refs_from_syntax_tokens(predicate_tokens, scope);
                self.collect_sql_name_refs_from_syntax_tokens(
                    query_id,
                    scope,
                    predicate_tokens,
                    true,
                );
            }
        }
    }

    pub(super) fn collect_select_query(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        has_endselect: bool,
        local_cte_names: &[Arc<str>],
    ) {
        let Some(query) = SelectQuery::cast(self.ctx.syntax(node)) else {
            return;
        };
        let children: Vec<_> = query
            .syntax()
            .children()
            .map(|child| (child.id(), child.kind(), child.range()))
            .collect();
        let query_id = self.ctx.sql_queries_len();
        let mut projection_clause = None;
        let mut from_clause = None;
        let mut into_clause = None;
        let mut where_clause = None;
        let mut group_by_clause = None;
        let mut having_clause = None;
        let mut order_by_clause = None;
        let mut order_by_clause_node = None;
        let mut for_all_entries_clause = None;
        let mut for_update_clause = None;
        let mut up_to_clause = None;
        let mut package_size_clause = None;
        let mut offset_clause = None;
        let mut abap_options_clause: Option<abap_lexer::TextRange> = None;
        let mut set_operator_clause: Option<abap_lexer::TextRange> = None;
        let mut is_single = false;
        let mut is_distinct = false;
        let mut is_for_update = false;
        let mut has_package_size = false;
        let mut has_set_operators = false;
        let mut has_dynamic_where = false;

        self.validate_select_syntax(node);

        for (child_id, child_kind, child_range) in children {
            match child_kind {
                SyntaxKind::Token
                    if self
                        .ctx
                        .syntax(child_id)
                        .text(self.ctx.source())
                        .is_some_and(|text| text.eq_ignore_ascii_case("single")) =>
                {
                    is_single = true;
                }
                SyntaxKind::SelectDistinctClause => {
                    is_distinct = true;
                }
                SyntaxKind::SelectProjectionList => {
                    projection_clause = Some(child_range);
                    self.collect_select_projection_list(query_id, child_id, scope);
                }
                SyntaxKind::SelectFromClause => {
                    from_clause = Some(child_range);
                    self.collect_select_from_clause(query_id, child_id, scope, local_cte_names);
                }
                SyntaxKind::SelectIntoClause => {
                    into_clause = Some(child_range);
                    self.collect_select_into_clause(query_id, child_id, scope);
                }
                SyntaxKind::SelectWhereClause => {
                    where_clause = Some(child_range);
                    has_dynamic_where =
                        self.ctx.count_kind(child_id, SyntaxKind::SqlDynamicWhere) > 0;
                    self.collect_sql_clause(query_id, child_id, scope, SqlClauseKind::Where);
                }
                SyntaxKind::SelectGroupByClause => {
                    group_by_clause = Some(child_range);
                    self.collect_sql_host_and_name_refs_in_node(query_id, child_id, scope, false);
                }
                SyntaxKind::SelectHavingClause => {
                    having_clause = Some(child_range);
                    self.collect_sql_clause(query_id, child_id, scope, SqlClauseKind::Having);
                }
                SyntaxKind::SelectOrderByClause => {
                    order_by_clause = Some(child_range);
                    order_by_clause_node = Some(child_id);
                    self.collect_sql_host_and_name_refs_in_node(query_id, child_id, scope, false);
                }
                SyntaxKind::SelectForAllEntriesClause => {
                    for_all_entries_clause = Some(child_range);
                    self.collect_sql_clause(
                        query_id,
                        child_id,
                        scope,
                        SqlClauseKind::ForAllEntries,
                    );
                }
                SyntaxKind::SelectForUpdateClause => {
                    for_update_clause = Some(child_range);
                    is_for_update = true;
                }
                SyntaxKind::SelectUpToClause => {
                    up_to_clause = Some(child_range);
                    self.collect_sql_host_refs_in_node(child_id, scope);
                }
                SyntaxKind::SelectPackageSizeClause => {
                    package_size_clause = Some(child_range);
                    has_package_size = true;
                    self.collect_sql_host_refs_in_node(child_id, scope);
                }
                SyntaxKind::SelectOffsetClause => {
                    offset_clause = Some(child_range);
                    self.collect_sql_host_refs_in_node(child_id, scope);
                }
                SyntaxKind::SelectAbapOptionsClause => {
                    abap_options_clause = Some(match abap_options_clause {
                        Some(existing) => {
                            existing.start.min(child_range.start)..existing.end.max(child_range.end)
                        }
                        None => child_range.clone(),
                    });
                    self.collect_sql_host_refs_in_node(child_id, scope);
                }
                SyntaxKind::SelectSetOperatorClause => {
                    set_operator_clause = Some(match set_operator_clause {
                        Some(existing) => {
                            existing.start.min(child_range.start)..existing.end.max(child_range.end)
                        }
                        None => child_range.clone(),
                    });
                    has_set_operators = true;
                    self.collect_sql_host_refs_in_node(child_id, scope);
                }
                _ => {}
            }
        }

        let query_range = self.ctx.file().range(node);
        let order_by_info = order_by_clause_node
            .map(|order_by_node| self.select_order_by_info(order_by_node))
            .unwrap_or_default();
        if !order_by_info.primary_key && !order_by_info.fields.is_empty() {
            for target in self.ctx.sql_targets_for_query(query_id) {
                if target.kind == SqlTargetKind::Into
                    && target.is_table
                    && let Some(table_name) = target.target_name
                {
                    self.ctx.record_internal_table_order(
                        scope,
                        query_range.clone(),
                        table_name,
                        order_by_info.fields.clone(),
                    );
                }
            }
        }

        self.ctx.emit_sql_query(SqlQueryData {
            id: query_id,
            scope,
            range: query_range,
            projection_clause,
            from_clause,
            into_clause,
            where_clause,
            group_by_clause,
            having_clause,
            order_by_clause,
            order_by_primary_key: order_by_info.primary_key,
            order_by_fields: order_by_info.fields,
            for_all_entries_clause,
            for_update_clause,
            up_to_clause,
            package_size_clause,
            offset_clause,
            abap_options_clause,
            set_operator_clause,
            is_single,
            is_distinct,
            is_for_update,
            has_package_size,
            has_set_operators,
            has_endselect,
            has_dynamic_where,
        });
    }

    fn validate_select_syntax(&mut self, query_node: NodeId) {
        self.validate_unescaped_inline_select_targets(query_node);
        if self.select_query_uses_strict_open_sql(query_node) {
            self.validate_strict_projection_commas(query_node);
        }
    }

    fn select_query_uses_strict_open_sql(&self, query_node: NodeId) -> bool {
        self.ctx
            .syntax_token_nodes(query_node)
            .iter()
            .any(|token| token.text.as_ref() == "@")
    }

    fn validate_unescaped_inline_select_targets(&mut self, query_node: NodeId) {
        let into_clauses = self.descendants_by_kind(query_node, SyntaxKind::SelectIntoClause);
        for into_clause in into_clauses {
            let clause_tokens = self.ctx.syntax_token_nodes(into_clause);
            let inline_targets = self.descendants_matching(into_clause, |node| {
                matches!(
                    self.ctx.file().kind(node),
                    SyntaxKind::DataInlineDecl | SyntaxKind::FieldSymbolInlineDecl
                )
            });
            for inline_target in inline_targets {
                let range = self.ctx.file().range(inline_target);
                let escaped = clause_tokens
                    .iter()
                    .rev()
                    .find(|token| {
                        token.range.end <= range.start && !self.ctx.syntax_token_is_comment(token)
                    })
                    .is_some_and(|token| token.text.as_ref() == "@");
                if escaped {
                    continue;
                }
                self.ctx.add_diagnostic(Diagnostic {
                    kind: DiagnosticKind::InvalidOpenSqlSyntax,
                    range,
                    message: "Open SQL inline target declarations must be escaped with '@'"
                        .to_string(),
                });
            }
        }
    }

    fn descendants_by_kind(&self, node: NodeId, kind: SyntaxKind) -> Vec<NodeId> {
        self.descendants_matching(node, |child| self.ctx.file().kind(child) == kind)
    }

    fn descendants_matching<F>(&self, node: NodeId, mut predicate: F) -> Vec<NodeId>
    where
        F: FnMut(NodeId) -> bool,
    {
        let mut out = Vec::new();
        let mut stack: Vec<_> = self.ctx.file().children(node).rev().collect();
        while let Some(child) = stack.pop() {
            if predicate(child) {
                out.push(child);
            }
            for grandchild in self.ctx.file().children(child).rev() {
                stack.push(grandchild);
            }
        }
        out
    }

    fn validate_strict_projection_commas(&mut self, query_node: NodeId) {
        let projection_items = self.descendants_by_kind(query_node, SyntaxKind::SqlProjectionItem);
        for item in projection_items {
            if let Some((range, name)) = self.strict_projection_missing_comma(item) {
                self.ctx.add_diagnostic(Diagnostic {
                    kind: DiagnosticKind::InvalidOpenSqlSyntax,
                    range,
                    message: format!(
                        "Open SQL strict mode requires commas between projection fields; insert ',' before '{}'",
                        name
                    ),
                });
            }
        }
    }

    fn strict_projection_missing_comma(&self, item: NodeId) -> Option<(TextRange, Arc<str>)> {
        let tokens: Vec<_> = self
            .ctx
            .syntax_token_nodes(item)
            .into_iter()
            .filter(|token| !self.ctx.syntax_token_is_comment(token))
            .collect();
        let mut columns = Vec::<(TextRange, Arc<str>)>::new();
        let mut idx = 0usize;
        while idx < tokens.len() {
            let token = &tokens[idx];
            let text = token.text.as_ref();
            if self.sql_token_is_keyword_text(text) || self.ctx.syntax_token_is_literal_like(token)
            {
                return None;
            }
            if !self.ctx.syntax_token_is_ident_like(token) {
                return None;
            }
            if tokens.get(idx + 1).map(|next| next.text.as_ref()) == Some("~") {
                let field = tokens.get(idx + 2)?;
                if field.text.as_ref() == "*" {
                    return None;
                }
                if !self.ctx.syntax_token_is_ident_like(field)
                    || self.sql_token_is_keyword_text(field.text.as_ref())
                    || self.ctx.syntax_token_is_literal_like(field)
                {
                    return None;
                }
                columns.push((
                    token.range.start..field.range.end,
                    Self::lower_arc(field.text.as_ref()),
                ));
                idx += 3;
                continue;
            }

            columns.push((token.range.clone(), Self::lower_arc(text)));
            idx += 1;
        }
        (columns.len() > 1).then(|| columns[1].clone())
    }

    fn select_order_by_info(&self, node: NodeId) -> SelectOrderByInfo {
        let tokens: Vec<_> = self
            .ctx
            .syntax_token_nodes(node)
            .into_iter()
            .filter(|token| !self.ctx.syntax_token_is_comment(token))
            .collect();
        let Some(by_idx) = tokens
            .iter()
            .position(|token| token.text.eq_ignore_ascii_case("by"))
        else {
            return SelectOrderByInfo::default();
        };
        if tokens.get(by_idx + 1..by_idx + 3).is_some_and(|window| {
            window[0].text.eq_ignore_ascii_case("primary")
                && window[1].text.eq_ignore_ascii_case("key")
        }) {
            return SelectOrderByInfo {
                primary_key: true,
                fields: Vec::new(),
            };
        }
        let mut fields = Vec::new();
        let mut idx = by_idx + 1;
        while idx < tokens.len() {
            let token = &tokens[idx];
            if token.text.as_ref() == "." || token.text.as_ref() == "," {
                idx += 1;
                continue;
            }
            if token.text.eq_ignore_ascii_case("ascending")
                || token.text.eq_ignore_ascii_case("nulls")
                || token.text.eq_ignore_ascii_case("first")
                || token.text.eq_ignore_ascii_case("last")
            {
                idx += 1;
                continue;
            }
            if token.text.eq_ignore_ascii_case("descending")
                || token.text.eq_ignore_ascii_case("primary")
                || token.text.eq_ignore_ascii_case("key")
            {
                return SelectOrderByInfo::default();
            }
            let Some((field, next_idx)) = self.select_order_field_from_tokens(&tokens, idx) else {
                return SelectOrderByInfo::default();
            };
            fields.push(field);
            idx = next_idx;
        }
        SelectOrderByInfo {
            primary_key: false,
            fields,
        }
    }

    fn select_order_field_from_tokens(
        &self,
        tokens: &[SyntaxTokenInfo],
        start: usize,
    ) -> Option<(Arc<str>, usize)> {
        let token = tokens.get(start)?;
        if token.kind != abap_lexer::TokenKind::Ident {
            return None;
        }
        if tokens
            .get(start + 1)
            .is_some_and(|next| next.text.as_ref() == "~")
        {
            let field = tokens.get(start + 2)?;
            if field.kind != abap_lexer::TokenKind::Ident {
                return None;
            }
            return Some((Self::lower_arc(field.text.as_ref()), start + 3));
        }
        Some((Self::lower_arc(token.text.as_ref()), start + 1))
    }

    fn collect_select_projection_list(&mut self, query_id: usize, node: NodeId, scope: ScopeId) {
        let children: Vec<_> = SelectProjectionList::cast(self.ctx.syntax(node))
            .map(|list| list.items().map(|item| item.syntax().id()).collect())
            .unwrap_or_default();
        for child in children {
            self.collect_sql_projection_item(query_id, child, scope);
        }
    }

    fn collect_sql_projection_item(&mut self, query_id: usize, node: NodeId, scope: ScopeId) {
        let (alias, alias_clause_start) = SqlProjectionItem::cast(self.ctx.syntax(node))
            .map(|item| {
                let alias_clause = item.alias_clause();
                let alias = alias_clause
                    .and_then(|clause| clause.alias())
                    .and_then(|alias_node| self.ctx.node_name(alias_node.syntax().id()));
                let alias_clause_start = alias_clause.map(|clause| clause.syntax().range().start);
                (alias, alias_clause_start)
            })
            .unwrap_or((None, None));
        let syntax_tokens = self.ctx.syntax_token_nodes(node);

        if let Some((dynamic_tokens, dynamic_range)) =
            self.dynamic_sql_projection_operand_tokens(&syntax_tokens, alias_clause_start)
        {
            self.ctx
                .collect_token_expression_refs_infos(&dynamic_tokens, scope, true);
            self.emit_dynamic_fragment(
                query_id,
                scope,
                dynamic_range,
                SqlDynamicFragmentKind::Projection,
            );
            self.ctx.emit_sql_projection(SqlProjectionData {
                query_id,
                range: self.ctx.file().range(node),
                kind: SqlProjectionKind::Expression,
                source_alias: None,
                name: None,
                alias: alias.map(|(name, _)| name),
            });
            return;
        }

        self.collect_sql_host_refs_from_node(node, scope);

        let mut kind = SqlProjectionKind::Expression;
        let mut source_alias = None;
        let mut name = None;

        let children: Vec<_> = self
            .ctx
            .syntax(node)
            .children()
            .map(|child| (child.id(), child.kind()))
            .collect();
        for (child, kind_syntax) in children {
            match kind_syntax {
                SyntaxKind::SqlStar => {
                    kind = SqlProjectionKind::Star;
                    self.push_sql_name_ref(
                        query_id,
                        scope,
                        self.ctx.file().range(child),
                        Arc::<str>::from("*"),
                        None,
                        SqlNameRefKind::Star,
                    );
                }
                SyntaxKind::SqlQualifiedStar => {
                    kind = SqlProjectionKind::QualifiedStar;
                    if let Some((qualifier, range)) = SqlQualifiedStar::cast(self.ctx.syntax(child))
                        .and_then(|star| star.qualifier(self.ctx.source()))
                    {
                        source_alias = Some(Arc::clone(&qualifier));
                        self.push_sql_name_ref(
                            query_id,
                            scope,
                            range,
                            Arc::<str>::from("*"),
                            Some(qualifier),
                            SqlNameRefKind::QualifiedStar,
                        );
                    }
                }
                SyntaxKind::SqlColumnRef => {
                    kind = SqlProjectionKind::Column;
                    if let Some((qualifier, column, range)) =
                        SqlColumnRef::cast(self.ctx.syntax(child))
                            .and_then(|column_ref| column_ref.parts(self.ctx.source()))
                    {
                        source_alias = qualifier.clone();
                        name = Some(Arc::clone(&column));
                        self.push_sql_name_ref(
                            query_id,
                            scope,
                            range,
                            column,
                            qualifier,
                            if source_alias.is_some() {
                                SqlNameRefKind::QualifiedColumn
                            } else {
                                SqlNameRefKind::Column
                            },
                        );
                    }
                }
                SyntaxKind::SqlQualifiedColumnRef => {
                    kind = SqlProjectionKind::Column;
                    if let Some((qualifier, column, range)) =
                        SqlQualifiedColumnRef::cast(self.ctx.syntax(child))
                            .and_then(|column_ref| column_ref.parts(self.ctx.source()))
                    {
                        source_alias = Some(Arc::clone(&qualifier));
                        name = Some(Arc::clone(&column));
                        self.push_sql_name_ref(
                            query_id,
                            scope,
                            range,
                            column,
                            Some(qualifier),
                            SqlNameRefKind::QualifiedColumn,
                        );
                    }
                }
                SyntaxKind::SqlAggregateCall => {
                    kind = SqlProjectionKind::Aggregate;
                    if let Some((aggregate, range)) = SqlAggregateCall::cast(self.ctx.syntax(child))
                        .and_then(|call| call.name(self.ctx.source()))
                    {
                        self.push_sql_name_ref(
                            query_id,
                            scope,
                            range,
                            aggregate,
                            None,
                            SqlNameRefKind::Aggregate,
                        );
                    }
                }
                _ => {}
            }
        }

        if matches!(kind, SqlProjectionKind::Expression) {
            let value_tokens: Vec<_> = syntax_tokens
                .iter()
                .filter(|token| !self.ctx.syntax_token_is_comment(token))
                .filter(|token| alias_clause_start.is_none_or(|start| token.range.end <= start))
                .filter(|token| token.text.as_ref() != "as")
                .collect();

            if value_tokens.len() == 1
                && !self.sql_token_is_keyword_text(value_tokens[0].text.as_ref())
            {
                kind = SqlProjectionKind::Column;
                name = Some(Self::lower_arc(value_tokens[0].text.as_ref()));
                self.push_sql_name_ref(
                    query_id,
                    scope,
                    value_tokens[0].range.clone(),
                    Arc::clone(name.as_ref().expect("projection name")),
                    None,
                    SqlNameRefKind::Column,
                );
            } else if value_tokens.len() == 3
                && value_tokens[0]
                    .text
                    .chars()
                    .next()
                    .is_some_and(|ch| ch.is_ascii_alphanumeric() || ch == '/')
                && value_tokens[1].text.as_ref() == "~"
                && !self.sql_token_is_keyword_text(value_tokens[2].text.as_ref())
            {
                kind = SqlProjectionKind::Column;
                source_alias = Some(Self::lower_arc(value_tokens[0].text.as_ref()));
                name = Some(Self::lower_arc(value_tokens[2].text.as_ref()));
                self.push_sql_name_ref(
                    query_id,
                    scope,
                    value_tokens[0].range.start..value_tokens[2].range.end,
                    Arc::clone(name.as_ref().expect("projection name")),
                    source_alias.clone(),
                    SqlNameRefKind::QualifiedColumn,
                );
            }
        }

        if matches!(kind, SqlProjectionKind::Expression) {
            self.collect_sql_name_refs_from_node(query_id, scope, node, false);
        }
        let projection_range = self.ctx.file().range(node);
        self.ctx.emit_sql_projection(SqlProjectionData {
            query_id,
            range: projection_range,
            kind,
            source_alias,
            name,
            alias: alias.map(|(name, _)| name),
        });
    }

    fn collect_select_from_clause(
        &mut self,
        query_id: usize,
        node: NodeId,
        scope: ScopeId,
        local_cte_names: &[Arc<str>],
    ) {
        let mut saw_base_source = false;
        let children: Vec<_> = self
            .ctx
            .syntax(node)
            .children()
            .map(|child| (child.id(), child.kind()))
            .collect();
        for (child, kind_syntax) in children {
            match kind_syntax {
                SyntaxKind::SqlDataSource => {
                    let source_kind = if saw_base_source {
                        SqlSourceKind::Join
                    } else {
                        SqlSourceKind::From
                    };
                    saw_base_source = true;
                    self.collect_sql_data_source(
                        query_id,
                        child,
                        scope,
                        source_kind,
                        None,
                        local_cte_names,
                    );
                }
                SyntaxKind::SelectJoinClause => {
                    self.collect_select_join_clause(query_id, child, scope, local_cte_names)
                }
                _ => {}
            }
        }
    }

    fn collect_insert_db_table_target(&mut self, query_id: usize, node: NodeId, scope: ScopeId) {
        let tokens: Vec<_> = self
            .ctx
            .syntax_token_nodes(node)
            .into_iter()
            .filter(|token| !self.ctx.syntax_token_is_comment(token))
            .collect();
        if tokens.len() >= 2
            && tokens
                .first()
                .is_some_and(|token| token.text.as_ref() == "(")
            && tokens
                .last()
                .is_some_and(|token| token.text.as_ref() == ")")
        {
            if tokens.len() > 2 {
                self.ctx.collect_token_expression_refs_infos(
                    &tokens[1..tokens.len() - 1],
                    scope,
                    true,
                );
            }
            self.emit_dynamic_fragment(
                query_id,
                scope,
                self.ctx.file().range(node),
                SqlDynamicFragmentKind::Source,
            );
            return;
        }

        self.collect_sql_data_source(query_id, node, scope, SqlSourceKind::From, None, &[]);
    }

    fn collect_select_join_clause(
        &mut self,
        query_id: usize,
        node: NodeId,
        scope: ScopeId,
        local_cte_names: &[Arc<str>],
    ) {
        let Some(join_clause) = SelectJoinClause::cast(self.ctx.syntax(node)) else {
            return;
        };
        let join_kind = join_clause
            .join_kind_text(self.ctx.source())
            .map(|text| Arc::<str>::from(text.to_ascii_lowercase()));
        let source_id = join_clause.data_source().map(|source| source.syntax().id());
        let predicate_id = join_clause
            .predicate()
            .map(|predicate| predicate.syntax().id());
        if let Some(source_id) = source_id {
            self.collect_sql_data_source(
                query_id,
                source_id,
                scope,
                SqlSourceKind::Join,
                join_kind,
                local_cte_names,
            );
        }
        if let Some(predicate_id) = predicate_id {
            self.collect_sql_clause(query_id, predicate_id, scope, SqlClauseKind::JoinOn);
        }
    }

    fn collect_sql_data_source(
        &mut self,
        query_id: usize,
        node: NodeId,
        scope: ScopeId,
        source_kind: SqlSourceKind,
        join_kind: Option<Arc<str>>,
        local_cte_names: &[Arc<str>],
    ) {
        let alias_info = SqlDataSource::cast(self.ctx.syntax(node))
            .and_then(|source| source.alias())
            .and_then(|alias_node| {
                self.ctx
                    .node_name(alias_node.syntax().id())
                    .map(|(name, _)| (name, alias_node.syntax().range()))
            });
        if let Some(dynamic_source_tokens) = self.dynamic_sql_source_operand_tokens(node) {
            self.ctx
                .collect_token_expression_refs_infos(&dynamic_source_tokens, scope, true);
            self.emit_dynamic_fragment(
                query_id,
                scope,
                self.ctx.file().range(node),
                SqlDynamicFragmentKind::Source,
            );
            if let Some((alias_name, alias_range)) = alias_info {
                self.push_sql_name_ref(
                    query_id,
                    scope,
                    alias_range,
                    alias_name,
                    None,
                    SqlNameRefKind::Alias,
                );
            }
            return;
        }
        let Some((name_text, name_range)) = SqlDataSource::cast(self.ctx.syntax(node))
            .and_then(|source| source.source_name(self.ctx.source()))
        else {
            return;
        };
        let name = Arc::<str>::from(name_text.to_ascii_lowercase());
        let alias = alias_info.as_ref().map(|(name, _)| Arc::clone(name));
        let is_local_cte_source = local_cte_names.iter().any(|cte| cte == &name);

        let source_range = self.ctx.file().range(node);
        self.ctx.emit_sql_source(SqlSourceData {
            query_id,
            range: source_range,
            source_kind,
            name: Arc::clone(&name),
            alias: alias.clone(),
            join_kind,
            resolution: if is_local_cte_source {
                SqlResolution::LocalCte
            } else {
                SqlResolution::External
            },
        });
        if !is_local_cte_source {
            self.push_sql_name_ref(
                query_id,
                scope,
                name_range,
                Arc::clone(&name),
                None,
                SqlNameRefKind::Source,
            );
        }
        if let Some(alias_name) = alias {
            let alias_range = alias_info
                .as_ref()
                .map(|(_, range)| range.clone())
                .unwrap_or_else(|| self.ctx.file().range(node));
            self.push_sql_name_ref(
                query_id,
                scope,
                alias_range,
                alias_name,
                None,
                SqlNameRefKind::Alias,
            );
        }
    }

    fn collect_select_into_clause(&mut self, query_id: usize, node: NodeId, scope: ScopeId) {
        let Some(into_clause) = SelectIntoClause::cast(self.ctx.syntax(node)) else {
            return;
        };
        let is_appending = into_clause.has_keyword(self.ctx.source(), "appending");
        let is_table = into_clause.has_keyword(self.ctx.source(), "table");
        let is_corresponding = into_clause.has_keyword(self.ctx.source(), "corresponding");
        let target_kind = if is_appending {
            SqlTargetKind::Appending
        } else {
            SqlTargetKind::Into
        };
        let children: Vec<_> = into_clause
            .target_children()
            .map(|child| child.id())
            .collect();
        let clause_range = self.ctx.file().range(node);
        let clause_tokens = self.ctx.syntax_token_nodes(node);
        if let Some(target_segments) = self.parenthesized_select_target_segments_from_tokens(
            self.select_into_clause_target_tokens(&clause_tokens),
        ) {
            for target_tokens in target_segments {
                self.collect_select_target_tokens(
                    query_id,
                    scope,
                    &clause_range,
                    target_kind,
                    &target_tokens,
                    is_table,
                    is_corresponding,
                );
            }
            return;
        }

        for &child in &children {
            match self.ctx.file().kind(child) {
                SyntaxKind::DataInlineDecl => {
                    let target_name = self.inline_decl_name(child);
                    let inferred_metadata = if is_table {
                        target_name
                            .as_ref()
                            .and_then(|target_name| {
                                self.inline_select_target_structure(query_id, scope, target_name)
                            })
                            .map(|structure| (Some(structure), None))
                            .unwrap_or((None, None))
                    } else {
                        self.inline_select_target_metadata(query_id, scope)
                    };
                    if inferred_metadata.0.is_some() || inferred_metadata.1.is_some() {
                        self.ctx.decl_lowering().declare_inline_variable_decl(
                            child,
                            scope,
                            inferred_metadata.0,
                            inferred_metadata.1,
                        );
                    } else {
                        self.ctx.decl_lowering().walk_inline_decl(child, scope);
                    }
                    self.ctx.emit_sql_target(SqlTargetData {
                        query_id,
                        scope,
                        range: clause_range.clone(),
                        target_range: Some(self.ctx.file().range(child)),
                        kind: target_kind,
                        target_name,
                        is_table,
                        is_corresponding,
                        is_inline: true,
                    });
                }
                SyntaxKind::FieldSymbolInlineDecl => {
                    let target_name = self.inline_decl_name(child);
                    self.ctx
                        .decl_lowering()
                        .declare_inline_field_symbol_decl(child, scope, None, None, None);
                    self.ctx.emit_sql_target(SqlTargetData {
                        query_id,
                        scope,
                        range: clause_range.clone(),
                        target_range: Some(self.ctx.file().range(child)),
                        kind: target_kind,
                        target_name,
                        is_table,
                        is_corresponding,
                        is_inline: true,
                    });
                }
                SyntaxKind::ExprIdent
                | SyntaxKind::SelectorExpr
                | SyntaxKind::CallExpr
                | SyntaxKind::BinaryExpr
                | SyntaxKind::UnaryExpr
                | SyntaxKind::ParenExpr
                | SyntaxKind::ConstructorExpr => {
                    if let Some(target_segments) = self.parenthesized_select_target_segments(child)
                    {
                        for target_tokens in target_segments {
                            self.collect_select_target_tokens(
                                query_id,
                                scope,
                                &clause_range,
                                target_kind,
                                &target_tokens,
                                is_table,
                                is_corresponding,
                            );
                        }
                        continue;
                    }
                    let target_name = self.ctx.sql_target_name_from_expr(child);
                    self.ctx.expr_lowering().collect_expr(child, scope);
                    self.ctx.emit_sql_target(SqlTargetData {
                        query_id,
                        scope,
                        range: clause_range.clone(),
                        target_range: Some(self.ctx.file().range(child)),
                        kind: target_kind,
                        target_name,
                        is_table,
                        is_corresponding,
                        is_inline: false,
                    });
                }
                SyntaxKind::TemplateExpr => {
                    if let Some(target_segments) = self.parenthesized_select_target_segments(child)
                    {
                        for target_tokens in target_segments {
                            self.collect_select_target_tokens(
                                query_id,
                                scope,
                                &clause_range,
                                target_kind,
                                &target_tokens,
                                is_table,
                                is_corresponding,
                            );
                        }
                        continue;
                    }
                    let mut target_name = None;
                    for grandchild in self.ctx.file().children(child) {
                        if target_name.is_none() {
                            target_name = self.ctx.sql_target_name_from_expr(grandchild);
                        }
                        self.ctx.expr_lowering().collect_expr(grandchild, scope);
                    }
                    self.ctx.emit_sql_target(SqlTargetData {
                        query_id,
                        scope,
                        range: clause_range.clone(),
                        target_range: Some(self.ctx.file().range(child)),
                        kind: target_kind,
                        target_name,
                        is_table,
                        is_corresponding,
                        is_inline: false,
                    });
                }
                _ => {}
            }
        }
    }

    fn parenthesized_select_target_segments(
        &self,
        node: NodeId,
    ) -> Option<Vec<Vec<SyntaxTokenInfo>>> {
        let tokens = self.ctx.syntax_token_nodes(node);
        self.parenthesized_select_target_segments_from_tokens(&tokens)
    }

    fn select_into_clause_target_tokens<'b>(
        &self,
        tokens: &'b [SyntaxTokenInfo],
    ) -> &'b [SyntaxTokenInfo] {
        let mut idx = 0usize;
        idx = self.skip_comment_tokens(tokens, idx);
        if tokens.get(idx).is_some_and(|token| {
            matches!(
                token.text.to_ascii_lowercase().as_str(),
                "into" | "appending"
            )
        }) {
            idx += 1;
            idx = self.skip_comment_tokens(tokens, idx);
        }
        if tokens
            .get(idx)
            .is_some_and(|token| token.text.eq_ignore_ascii_case("corresponding"))
        {
            idx += 1;
            idx = self.skip_comment_tokens(tokens, idx);
            if tokens
                .get(idx)
                .is_some_and(|token| token.text.eq_ignore_ascii_case("fields"))
            {
                idx += 1;
                idx = self.skip_comment_tokens(tokens, idx);
            }
            if tokens
                .get(idx)
                .is_some_and(|token| token.text.eq_ignore_ascii_case("of"))
            {
                idx += 1;
                idx = self.skip_comment_tokens(tokens, idx);
            }
        }
        if tokens
            .get(idx)
            .is_some_and(|token| token.text.eq_ignore_ascii_case("table"))
        {
            idx += 1;
            idx = self.skip_comment_tokens(tokens, idx);
        }
        &tokens[idx..]
    }

    fn skip_comment_tokens(&self, tokens: &[SyntaxTokenInfo], mut idx: usize) -> usize {
        while idx < tokens.len() && self.ctx.syntax_token_is_comment(&tokens[idx]) {
            idx += 1;
        }
        idx
    }

    fn parenthesized_select_target_segments_from_tokens(
        &self,
        tokens: &[SyntaxTokenInfo],
    ) -> Option<Vec<Vec<SyntaxTokenInfo>>> {
        let tokens: Vec<_> = tokens
            .iter()
            .filter(|token| !self.ctx.syntax_token_is_comment(token))
            .cloned()
            .collect();
        let inner = Self::dynamic_parenthesized_operand_tokens(&tokens)?;
        let mut out = Vec::new();
        let mut start = 0usize;
        let mut paren = 0i32;
        let mut bracket = 0i32;
        let mut brace = 0i32;
        for (idx, token) in inner.iter().enumerate() {
            match token.text.as_ref() {
                "(" => paren += 1,
                ")" => paren -= 1,
                "[" => bracket += 1,
                "]" => bracket -= 1,
                "{" => brace += 1,
                "}" => brace -= 1,
                "," if paren == 0 && bracket == 0 && brace == 0 => {
                    if let Some(segment) = Self::trim_select_target_tokens(&inner[start..idx]) {
                        out.push(segment.to_vec());
                    }
                    start = idx + 1;
                }
                _ => {}
            }
        }
        if let Some(segment) = Self::trim_select_target_tokens(&inner[start..]) {
            out.push(segment.to_vec());
        }
        (!out.is_empty()).then_some(out)
    }

    fn trim_select_target_tokens(tokens: &[SyntaxTokenInfo]) -> Option<&[SyntaxTokenInfo]> {
        let mut start = 0usize;
        let mut end = tokens.len();
        while start < end && tokens[start].text.as_ref() == "@" {
            start += 1;
        }
        while start < end && tokens[end - 1].text.as_ref() == "@" {
            end -= 1;
        }
        (start < end).then_some(&tokens[start..end])
    }

    fn sql_target_name_from_infos(&self, tokens: &[SyntaxTokenInfo]) -> Option<Arc<str>> {
        let first_ident_idx = tokens
            .iter()
            .position(|token| self.ctx.syntax_token_is_ident_like(token))?;
        if let Some((_next_idx, namespace, base_name, _base_range, _field_path, _groups)) = self
            .ctx
            .consume_selector_access_from_infos(tokens, first_ident_idx)
            && namespace == Namespace::Value
        {
            return Some(base_name);
        }
        (tokens.len() == first_ident_idx + 1)
            .then(|| Arc::<str>::from(tokens[first_ident_idx].text.to_ascii_lowercase()))
    }

    fn collect_select_target_tokens(
        &mut self,
        query_id: usize,
        scope: ScopeId,
        clause_range: &TextRange,
        target_kind: SqlTargetKind,
        tokens: &[SyntaxTokenInfo],
        is_table: bool,
        is_corresponding: bool,
    ) {
        let Some(tokens) = Self::trim_select_target_tokens(tokens) else {
            return;
        };
        self.ctx
            .collect_token_expression_refs_infos(tokens, scope, true);
        let target_range = tokens.first().unwrap().range.start..tokens.last().unwrap().range.end;
        self.ctx.emit_sql_target(SqlTargetData {
            query_id,
            scope,
            range: clause_range.clone(),
            target_range: Some(target_range),
            kind: target_kind,
            target_name: self.sql_target_name_from_infos(tokens),
            is_table,
            is_corresponding,
            is_inline: false,
        });
    }

    fn collect_sql_clause(
        &mut self,
        query_id: usize,
        node: NodeId,
        scope: ScopeId,
        kind: SqlClauseKind,
    ) {
        let predicate_range = self.ctx.file().range(node);
        let predicate_kind = match kind {
            SqlClauseKind::Where => {
                if self.ctx.count_kind(node, SyntaxKind::SqlDynamicWhere) > 0 {
                    SqlPredicateKind::DynamicWhere
                } else {
                    SqlPredicateKind::Where
                }
            }
            SqlClauseKind::JoinOn => SqlPredicateKind::JoinOn,
            SqlClauseKind::Having => SqlPredicateKind::Having,
            SqlClauseKind::ForAllEntries => SqlPredicateKind::ForAllEntries,
        };
        self.ctx.emit_sql_predicate(SqlPredicateData {
            query_id,
            range: predicate_range,
            kind: predicate_kind,
        });

        let syntax_tokens = self.ctx.syntax_token_nodes(node);
        if predicate_kind == SqlPredicateKind::DynamicWhere {
            self.emit_dynamic_fragment(
                query_id,
                scope,
                self.ctx.file().range(node),
                SqlDynamicFragmentKind::Where,
            );
            if let Some(dynamic_tokens) = self.dynamic_sql_where_operand_tokens(&syntax_tokens) {
                self.ctx
                    .collect_token_expression_refs_infos(&dynamic_tokens, scope, true);
            }
            return;
        }

        match kind {
            SqlClauseKind::ForAllEntries => {
                if let Some(in_idx) = syntax_tokens
                    .iter()
                    .position(|token| token.text.eq_ignore_ascii_case("in"))
                {
                    let expr_start = in_idx + 1;
                    if expr_start < syntax_tokens.len() {
                        self.ctx.collect_token_expression_refs_infos(
                            &syntax_tokens[expr_start..],
                            scope,
                            true,
                        );
                    }
                }
            }
            _ => {
                self.collect_sql_host_refs_from_node(node, scope);
                self.collect_sql_name_refs_from_node(query_id, scope, node, true);
            }
        }
    }

    fn collect_sql_host_refs_in_node(&mut self, node: NodeId, scope: ScopeId) {
        let tokens = self.ctx.syntax_token_nodes(node);
        self.collect_sql_host_refs_from_syntax_tokens(&tokens, scope);
    }

    fn collect_sql_host_and_name_refs_in_node(
        &mut self,
        query_id: usize,
        node: NodeId,
        scope: ScopeId,
        open_sql_predicate: bool,
    ) {
        let tokens = self.ctx.syntax_token_nodes(node);
        self.collect_sql_host_refs_from_syntax_tokens(&tokens, scope);
        self.collect_sql_name_refs_from_syntax_tokens(query_id, scope, &tokens, open_sql_predicate);
    }

    fn collect_sql_host_refs_from_node(&mut self, node: NodeId, scope: ScopeId) {
        match self.ctx.file().kind(node) {
            SyntaxKind::SqlHostExpr => {
                let tokens = self.ctx.syntax_token_nodes(node);
                if tokens.len() > 1 {
                    self.ctx
                        .collect_token_expression_refs_infos(&tokens[1..], scope, true);
                }
            }
            _ => {
                let children: Vec<_> = self.ctx.file().children(node).collect();
                for child in children {
                    self.collect_sql_host_refs_from_node(child, scope);
                }
            }
        }
    }

    fn collect_sql_name_refs_from_node(
        &mut self,
        query_id: usize,
        scope: ScopeId,
        node: NodeId,
        open_sql_predicate: bool,
    ) {
        match self.ctx.file().kind(node) {
            SyntaxKind::SqlQualifiedStar => {
                if let Some((qualifier, range)) = SqlQualifiedStar::cast(self.ctx.syntax(node))
                    .and_then(|star| star.qualifier(self.ctx.source()))
                {
                    self.push_sql_name_ref(
                        query_id,
                        scope,
                        range,
                        Arc::<str>::from("*"),
                        Some(qualifier),
                        SqlNameRefKind::QualifiedStar,
                    );
                }
            }
            SyntaxKind::SqlQualifiedColumnRef => {
                if let Some((qualifier, column, range)) =
                    SqlQualifiedColumnRef::cast(self.ctx.syntax(node))
                        .and_then(|column_ref| column_ref.parts(self.ctx.source()))
                {
                    self.push_sql_name_ref(
                        query_id,
                        scope,
                        range,
                        column,
                        Some(qualifier),
                        SqlNameRefKind::QualifiedColumn,
                    );
                }
            }
            SyntaxKind::SqlColumnRef => {
                if let Some((qualifier, column, range)) = SqlColumnRef::cast(self.ctx.syntax(node))
                    .and_then(|column_ref| column_ref.parts(self.ctx.source()))
                {
                    let kind = if qualifier.is_some() {
                        SqlNameRefKind::QualifiedColumn
                    } else {
                        SqlNameRefKind::Column
                    };
                    self.push_sql_name_ref(query_id, scope, range, column, qualifier, kind);
                }
            }
            SyntaxKind::SqlAggregateCall => {
                if let Some((aggregate, range)) = SqlAggregateCall::cast(self.ctx.syntax(node))
                    .and_then(|call| call.name(self.ctx.source()))
                {
                    self.push_sql_name_ref(
                        query_id,
                        scope,
                        range,
                        aggregate,
                        None,
                        SqlNameRefKind::Aggregate,
                    );
                }
                let children: Vec<_> = self.ctx.file().children(node).collect();
                for child in children {
                    self.collect_sql_name_refs_from_node(query_id, scope, child, false);
                }
            }
            SyntaxKind::SqlPredicateOperand => {
                let child_kinds: Vec<_> = self
                    .ctx
                    .file()
                    .children(node)
                    .map(|child| self.ctx.file().kind(child))
                    .filter(|kind| *kind != SyntaxKind::Token)
                    .collect();
                let has_only_ambiguous_children = child_kinds.iter().all(|kind| {
                    matches!(
                        kind,
                        SyntaxKind::SqlHostExpr | SyntaxKind::SqlParenGroup | SyntaxKind::Token
                    )
                });
                if child_kinds.is_empty() || has_only_ambiguous_children {
                    let tokens = self.ctx.syntax_token_nodes(node);
                    self.collect_sql_name_refs_from_syntax_tokens(
                        query_id,
                        scope,
                        &tokens,
                        open_sql_predicate,
                    );
                } else {
                    let children: Vec<_> = self.ctx.file().children(node).collect();
                    for child in children {
                        self.collect_sql_name_refs_from_node(
                            query_id,
                            scope,
                            child,
                            open_sql_predicate,
                        );
                    }
                }
            }
            SyntaxKind::SqlAlias | SyntaxKind::SqlAliasClause | SyntaxKind::SqlHostExpr => {}
            _ => {
                let children: Vec<_> = self.ctx.file().children(node).collect();
                for child in children {
                    self.collect_sql_name_refs_from_node(
                        query_id,
                        scope,
                        child,
                        open_sql_predicate,
                    );
                }
            }
        }
    }

    fn collect_sql_host_refs_from_syntax_tokens(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
    ) {
        let mut idx = 0usize;
        while idx < tokens.len() {
            if tokens[idx].text.as_ref() == "@" {
                let expr_start = idx + 1;
                let expr_end = self.sql_host_expr_end_syntax_tokens(tokens, expr_start);
                if expr_start < expr_end {
                    self.ctx.collect_token_expression_refs_infos(
                        &tokens[expr_start..expr_end],
                        scope,
                        true,
                    );
                }
                idx = expr_end.max(expr_start);
            } else {
                idx += 1;
            }
        }
    }

    fn sql_tokens_are_dynamic_where(tokens: &[SyntaxTokenInfo]) -> bool {
        if tokens.len() < 2
            || tokens.first().map(|token| token.text.as_ref()) != Some("(")
            || tokens.last().map(|token| token.text.as_ref()) != Some(")")
        {
            return false;
        }
        let mut paren = 0i32;
        for (idx, token) in tokens.iter().enumerate() {
            match token.text.as_ref() {
                "(" => paren += 1,
                ")" => {
                    paren -= 1;
                    if paren == 0 && idx + 1 != tokens.len() {
                        return false;
                    }
                }
                _ => {}
            }
            if paren < 0 {
                return false;
            }
        }
        paren == 0
    }

    fn collect_sql_name_refs_from_syntax_tokens(
        &mut self,
        query_id: usize,
        scope: ScopeId,
        tokens: &[SyntaxTokenInfo],
        open_sql_predicate: bool,
    ) {
        let mut idx = 0usize;
        while idx < tokens.len() {
            let token = &tokens[idx];
            let text = token.text.as_ref();
            match text {
                "@" => {
                    idx = self.sql_host_expr_end_syntax_tokens(tokens, idx + 1);
                }
                "*" => {
                    self.push_sql_name_ref(
                        query_id,
                        scope,
                        token.range.clone(),
                        Arc::<str>::from("*"),
                        None,
                        SqlNameRefKind::Star,
                    );
                    idx += 1;
                }
                ":" | "," | "." | "(" | ")" | "[" | "]" | "{" | "}" | "=" | "->" | "=>" | "-" => {
                    idx += 1;
                }
                _ => {
                    if self.ctx.syntax_token_is_comment(token) {
                        idx += 1;
                        continue;
                    }
                    if self.sql_token_is_keyword_text(text)
                        || self.ctx.syntax_token_is_literal_like(token)
                    {
                        idx += 1;
                        continue;
                    }
                    let lowered = Self::lower_arc(text);
                    if tokens.get(idx + 1).map(|next| next.text.as_ref()) == Some("~")
                        && let Some(third) = tokens.get(idx + 2)
                    {
                        let third_text = third.text.as_ref();
                        if third_text == "*" {
                            self.push_sql_name_ref(
                                query_id,
                                scope,
                                token.range.start..third.range.end,
                                Arc::<str>::from("*"),
                                Some(Arc::clone(&lowered)),
                                SqlNameRefKind::QualifiedStar,
                            );
                            idx += 3;
                            continue;
                        }
                        if !self.sql_token_is_keyword_text(third_text)
                            && !self.ctx.syntax_token_is_literal_like(third)
                            && !matches!(
                                third_text,
                                ":" | "," | "." | "(" | ")" | "[" | "]" | "{" | "}"
                            )
                        {
                            self.push_sql_name_ref(
                                query_id,
                                scope,
                                token.range.start..third.range.end,
                                Self::lower_arc(third_text),
                                Some(Arc::clone(&lowered)),
                                SqlNameRefKind::QualifiedColumn,
                            );
                            idx += 3;
                            continue;
                        }
                    }
                    if tokens.get(idx + 1).map(|next| next.text.as_ref()) == Some("(") {
                        self.push_sql_name_ref(
                            query_id,
                            scope,
                            token.range.clone(),
                            lowered,
                            None,
                            SqlNameRefKind::Aggregate,
                        );
                        idx += 1;
                        continue;
                    }
                    if idx > 0 && tokens[idx - 1].text.eq_ignore_ascii_case("as") {
                        idx += 1;
                        continue;
                    }
                    let name = lowered;
                    if open_sql_predicate {
                        let next_text = tokens.get(idx + 1).map(|next| next.text.as_ref());
                        if matches!(next_text, Some("-" | "->"))
                            && self
                                .ctx
                                .lookup_symbol_in_scope_chain(
                                    scope,
                                    Namespace::Value,
                                    name.as_ref(),
                                )
                                .is_some()
                        {
                            let expr_end = self.sql_host_expr_end_syntax_tokens(tokens, idx);
                            self.ctx.collect_token_expression_refs_infos(
                                &tokens[idx..expr_end],
                                scope,
                                true,
                            );
                            idx = expr_end.max(idx + 1);
                            continue;
                        }
                        if !matches!(next_text, Some("~" | "-" | "->" | "=>"))
                            && self
                                .ctx
                                .lookup_symbol_in_scope_chain(
                                    scope,
                                    Namespace::Value,
                                    name.as_ref(),
                                )
                                .is_some()
                        {
                            self.ctx.add_reference(
                                scope,
                                name,
                                Namespace::Value,
                                ReferenceKind::Identifier,
                                token.range.clone(),
                            );
                            idx += 1;
                            continue;
                        }
                    }
                    self.push_sql_name_ref(
                        query_id,
                        scope,
                        token.range.clone(),
                        name,
                        None,
                        SqlNameRefKind::Column,
                    );
                    idx += 1;
                }
            }
        }
    }

    fn push_sql_name_ref(
        &mut self,
        query_id: usize,
        scope: ScopeId,
        range: TextRange,
        name: Arc<str>,
        qualifier: Option<Arc<str>>,
        kind: SqlNameRefKind,
    ) {
        self.ctx.emit_sql_name_ref(SqlNameRefData {
            query_id,
            scope,
            range,
            name,
            qualifier,
            kind,
            resolution: SqlResolution::External,
        });
    }

    fn simple_sql_source_name_from_expr(&self, node: NodeId) -> Option<(Arc<str>, TextRange)> {
        let tokens: Vec<_> = self
            .ctx
            .syntax_token_nodes(node)
            .into_iter()
            .filter(|token| !self.ctx.syntax_token_is_comment(token))
            .collect();
        if tokens.len() != 1 || !self.ctx.syntax_token_is_ident_like(&tokens[0]) {
            return None;
        }
        Some((
            Arc::<str>::from(tokens[0].text.to_ascii_lowercase()),
            tokens[0].range.clone(),
        ))
    }

    fn dynamic_sql_projection_operand_tokens(
        &self,
        tokens: &[SyntaxTokenInfo],
        alias_clause_start: Option<usize>,
    ) -> Option<(Vec<SyntaxTokenInfo>, TextRange)> {
        let value_tokens: Vec<_> = tokens
            .iter()
            .filter(|token| !self.ctx.syntax_token_is_comment(token))
            .filter(|token| alias_clause_start.is_none_or(|start| token.range.end <= start))
            .cloned()
            .collect();
        let fragment_range = value_tokens.first()?.range.start..value_tokens.last()?.range.end;
        let inner = Self::dynamic_parenthesized_operand_tokens(&value_tokens)?;
        self.dynamic_sql_operand_is_value_reference(inner)
            .then(|| (inner.to_vec(), fragment_range))
    }

    fn dynamic_sql_where_operand_tokens(
        &self,
        tokens: &[SyntaxTokenInfo],
    ) -> Option<Vec<SyntaxTokenInfo>> {
        let tokens: Vec<_> = tokens
            .iter()
            .filter(|token| !self.ctx.syntax_token_is_comment(token))
            .cloned()
            .collect();
        let start = tokens.iter().position(|token| token.text.as_ref() == "(")?;
        Self::dynamic_parenthesized_operand_tokens(&tokens[start..]).map(|tokens| tokens.to_vec())
    }

    fn dynamic_parenthesized_operand_tokens_from_node(
        &self,
        node: NodeId,
    ) -> Option<Vec<SyntaxTokenInfo>> {
        let tokens: Vec<_> = self
            .ctx
            .syntax_token_nodes(node)
            .into_iter()
            .filter(|token| !self.ctx.syntax_token_is_comment(token))
            .collect();
        Self::dynamic_parenthesized_operand_tokens(&tokens).map(|tokens| tokens.to_vec())
    }

    fn dynamic_sql_operand_is_value_reference(&self, tokens: &[SyntaxTokenInfo]) -> bool {
        if tokens.len() == 1 {
            return self.ctx.syntax_token_is_ident_like(&tokens[0]);
        }
        self.ctx
            .consume_selector_access_from_infos(tokens, 0)
            .is_some_and(
                |(next_idx, namespace, _base_name, _base_range, _fields, _groups)| {
                    namespace == Namespace::Value && next_idx == tokens.len()
                },
            )
    }

    fn dynamic_sql_source_operand_tokens(&self, node: NodeId) -> Option<Vec<SyntaxTokenInfo>> {
        let alias_clause_range = SqlDataSource::cast(self.ctx.syntax(node))
            .and_then(|source| source.alias_clause())
            .map(|alias| alias.syntax().range());
        let tokens: Vec<_> = self
            .ctx
            .syntax_token_nodes(node)
            .into_iter()
            .filter(|token| !self.ctx.syntax_token_is_comment(token))
            .collect();
        let source_end = tokens
            .iter()
            .position(|token| {
                alias_clause_range
                    .as_ref()
                    .is_some_and(|alias| token.range.start >= alias.start)
            })
            .unwrap_or(tokens.len());
        let source_tokens = &tokens[..source_end];
        Self::dynamic_parenthesized_operand_tokens(source_tokens).map(|tokens| tokens.to_vec())
    }

    fn dynamic_parenthesized_operand_tokens(
        tokens: &[SyntaxTokenInfo],
    ) -> Option<&[SyntaxTokenInfo]> {
        if tokens.len() < 3
            || tokens.first().map(|token| token.text.as_ref()) != Some("(")
            || tokens.last().map(|token| token.text.as_ref()) != Some(")")
        {
            return None;
        }
        let mut paren = 0i32;
        for (idx, token) in tokens.iter().enumerate() {
            match token.text.as_ref() {
                "(" => paren += 1,
                ")" => {
                    paren -= 1;
                    if paren == 0 && idx + 1 != tokens.len() {
                        return None;
                    }
                }
                _ => {}
            }
            if paren < 0 {
                return None;
            }
        }
        (paren == 0).then_some(&tokens[1..tokens.len() - 1])
    }

    fn inline_decl_name(&self, node: NodeId) -> Option<Arc<str>> {
        self.ctx
            .syntax(node)
            .child_by_kind(SyntaxKind::DataDeclName)
            .and_then(DataDeclName::cast)
            .and_then(|name| name.name(self.ctx.source()))
    }

    fn inline_select_target_structure(
        &mut self,
        query_id: usize,
        scope: ScopeId,
        target_name: &Arc<str>,
    ) -> Option<crate::ids::StructureId> {
        let mut members = Vec::new();
        for projection in self.ctx.sql_projections_for_query(query_id) {
            let field_name = match projection.kind {
                SqlProjectionKind::Column => {
                    projection.alias.clone().or_else(|| projection.name.clone())
                }
                SqlProjectionKind::Aggregate | SqlProjectionKind::Expression => {
                    projection.alias.clone()
                }
                SqlProjectionKind::Star | SqlProjectionKind::QualifiedStar => None,
            };
            let Some(field_name) = field_name else {
                continue;
            };
            let already_present = members.iter().any(|member| match member {
                PendingStructureMember::Field(field) => field.name == field_name,
                PendingStructureMember::Include(_) => false,
            });
            if already_present {
                continue;
            }
            let (_structure, type_ref) = self
                .inline_select_projection_metadata(query_id, scope, &projection)
                .unwrap_or((None, None));
            members.push(PendingStructureMember::Field(PendingStructureField {
                name: field_name,
                decl_range: projection.range.clone(),
                structure: None,
                type_ref,
                is_key: false,
                value_clause_display: None,
            }));
        }
        if members.is_empty() {
            return None;
        }
        Some(self.ctx.register_structure(
            scope,
            PendingStructure {
                name: Arc::from(format!("<open_sql_inline:{}>", target_name.as_ref())),
                members,
            },
        ))
    }

    fn inline_select_target_metadata(
        &mut self,
        query_id: usize,
        scope: ScopeId,
    ) -> (
        Option<StructureId>,
        Option<crate::def_map::FieldTypeRefData>,
    ) {
        let projections = self.ctx.sql_projections_for_query(query_id);
        if projections.len() == 1
            && let Some(metadata) =
                self.inline_select_projection_metadata(query_id, scope, &projections[0])
        {
            return metadata;
        }

        (None, None)
    }

    fn inline_select_projection_metadata(
        &mut self,
        query_id: usize,
        scope: ScopeId,
        projection: &SqlProjectionData,
    ) -> Option<(
        Option<StructureId>,
        Option<crate::def_map::FieldTypeRefData>,
    )> {
        if projection.kind != SqlProjectionKind::Column {
            return None;
        }
        let field_name = projection.name.as_ref()?;
        let source_name =
            self.projection_source_name(query_id, projection.source_alias.as_ref())?;
        let symbol_id = self
            .ctx
            .lookup_symbol_in_scope_chain(scope, Namespace::Type, source_name.as_ref())
            .or_else(|| {
                self.ctx
                    .lookup_symbol_in_scope_chain(scope, Namespace::Value, source_name.as_ref())
            })?;
        let structure_id = self.ctx.symbol_structure(symbol_id)?;
        let field = self
            .ctx
            .structure_field(structure_id, field_name.as_ref())?;
        Some((field.structure, field.type_ref))
    }

    fn projection_source_name(
        &self,
        query_id: usize,
        source_alias: Option<&Arc<str>>,
    ) -> Option<Arc<str>> {
        let sources = self.ctx.sql_sources_for_query(query_id);
        if let Some(source_alias) = source_alias {
            return sources
                .iter()
                .find(|source| {
                    source.alias.as_ref() == Some(source_alias) || &source.name == source_alias
                })
                .map(|source| Arc::clone(&source.name));
        }
        if sources.len() == 1 {
            return sources.first().map(|source| Arc::clone(&source.name));
        }
        None
    }

    fn sql_token_is_keyword_text(&self, text: &str) -> bool {
        matches!(
            text.to_ascii_lowercase().as_str(),
            "select"
                | "single"
                | "distinct"
                | "case"
                | "when"
                | "then"
                | "else"
                | "end"
                | "from"
                | "into"
                | "appending"
                | "where"
                | "with"
                | "group"
                | "by"
                | "having"
                | "order"
                | "for"
                | "update"
                | "all"
                | "entries"
                | "in"
                | "up"
                | "to"
                | "rows"
                | "package"
                | "size"
                | "offset"
                | "bypassing"
                | "buffer"
                | "connection"
                | "client"
                | "specified"
                | "privileged"
                | "access"
                | "union"
                | "intersect"
                | "except"
                | "as"
                | "join"
                | "inner"
                | "left"
                | "right"
                | "cross"
                | "on"
                | "and"
                | "or"
                | "not"
                | "eq"
                | "ne"
                | "lt"
                | "le"
                | "gt"
                | "ge"
                | "co"
                | "cn"
                | "ca"
                | "na"
                | "cs"
                | "ns"
                | "cp"
                | "np"
                | "like"
                | "between"
                | "is"
                | "null"
                | "nulls"
                | "first"
                | "last"
                | "table"
                | "corresponding"
                | "fields"
                | "of"
                | "primary"
                | "key"
        )
    }

    fn sql_host_expr_end_syntax_tokens(&self, tokens: &[SyntaxTokenInfo], start: usize) -> usize {
        let mut paren = 0i32;
        let mut bracket = 0i32;
        let mut brace = 0i32;
        let mut idx = start;
        while idx < tokens.len() {
            let text = tokens[idx].text.as_ref();
            if paren == 0
                && bracket == 0
                && brace == 0
                && (matches!(text, "," | ".") || self.sql_token_is_keyword_text(text))
            {
                break;
            }
            match text {
                "(" => paren += 1,
                ")" => paren -= 1,
                "[" => bracket += 1,
                "]" => bracket -= 1,
                "{" => brace += 1,
                "}" => brace -= 1,
                _ => {}
            }
            idx += 1;
        }
        idx
    }
}
