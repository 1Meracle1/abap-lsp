use std::sync::Arc;

use abap_ast::SyntaxKind;
use abap_ast::arena::NodeId;
use abap_ast::ast::{
    AstNode, DataDeclName, SelectIntoClause, SelectJoinClause, SelectProjectionList, SelectQuery,
    SelectStmt, SqlColumnRef, SqlDataSource, SqlProjectionItem, SqlQualifiedStar,
};
use abap_lexer::TextRange;

use crate::def_map::{
    ReferenceKind, SqlNameRefData, SqlNameRefKind, SqlPredicateData, SqlPredicateKind,
    SqlProjectionData, SqlProjectionKind, SqlQueryData, SqlResolution, SqlSourceData,
    SqlSourceKind, SqlTargetData, SqlTargetKind,
};
use crate::ids::ScopeId;
use crate::scope::{Namespace, ScopeKind};

use super::context::SqlContext;
use super::{Collector, SqlClauseKind, SyntaxTokenInfo};

pub(super) struct SqlLowering<'ctx, 'a> {
    ctx: SqlContext<'ctx, 'a>,
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

    pub(super) fn collect_select_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let Some(stmt) = SelectStmt::cast(self.ctx.syntax(node)) else {
            self.ctx.walk_children(node, scope);
            return;
        };
        let query_node = stmt.query().map(|query| query.syntax().id());
        let non_query_children: Vec<_> =
            stmt.non_query_children().map(|child| child.id()).collect();
        let has_endselect = self.ctx.control_lowering().select_stmt_has_endselect(node);
        if has_endselect {
            let range = self.ctx.file().range(node);
            let child_scope = self
                .ctx
                .push_scope(ScopeKind::SelectBlock, range, Some(scope), None);
            if let Some(query_node) = query_node {
                self.collect_select_query(query_node, child_scope, true);
            }
            for child in non_query_children {
                self.ctx.walk_node(child, child_scope);
            }
        } else {
            if let Some(query_node) = query_node {
                self.collect_select_query(query_node, scope, false);
            }
            for child in non_query_children {
                self.ctx.walk_node(child, scope);
            }
        }
    }

    pub(super) fn collect_select_query(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        has_endselect: bool,
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
        let mut for_all_entries_clause = None;
        let mut up_to_clause = None;
        let mut is_single = false;
        let mut is_distinct = false;
        let mut has_dynamic_where = false;

        for (child_id, child_kind, child_range) in children {
            match child_kind {
                SyntaxKind::Token => {
                    if self
                        .ctx
                        .syntax(child_id)
                        .text(self.ctx.source())
                        .is_some_and(|text| text.eq_ignore_ascii_case("single"))
                    {
                        is_single = true;
                    }
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
                    self.collect_select_from_clause(query_id, child_id, scope);
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
                SyntaxKind::SelectUpToClause => {
                    up_to_clause = Some(child_range);
                    self.collect_sql_host_refs_in_node(child_id, scope);
                }
                _ => {}
            }
        }

        let query_range = self.ctx.file().range(node);
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
            for_all_entries_clause,
            up_to_clause,
            is_single,
            is_distinct,
            has_endselect,
            has_dynamic_where,
        });
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
        let alias = SqlProjectionItem::cast(self.ctx.syntax(node))
            .and_then(|item| item.alias())
            .and_then(|alias_node| self.ctx.node_name(alias_node.syntax().id()));
        let syntax_tokens = self.ctx.syntax_token_nodes(node);
        self.collect_sql_host_refs_from_syntax_tokens(&syntax_tokens, scope);

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
                _ => {}
            }
        }

        if matches!(kind, SqlProjectionKind::Expression)
            && let Some(token) = syntax_tokens.first()
            && !self.sql_token_is_keyword_text(token.text.as_ref())
            && syntax_tokens.get(1).map(|next| next.text.as_ref()) == Some("(")
        {
            kind = SqlProjectionKind::Aggregate;
            let text = Arc::<str>::from(token.text.to_ascii_lowercase());
            self.push_sql_name_ref(
                query_id,
                scope,
                token.range.clone(),
                text,
                None,
                SqlNameRefKind::Aggregate,
            );
        }

        if matches!(kind, SqlProjectionKind::Expression) {
            self.collect_sql_name_refs_from_syntax_tokens(query_id, scope, &syntax_tokens, false);
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

    fn collect_select_from_clause(&mut self, query_id: usize, node: NodeId, scope: ScopeId) {
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
                    self.collect_sql_data_source(query_id, child, scope, source_kind, None);
                }
                SyntaxKind::SelectJoinClause => {
                    self.collect_select_join_clause(query_id, child, scope)
                }
                _ => {}
            }
        }
    }

    fn collect_select_join_clause(&mut self, query_id: usize, node: NodeId, scope: ScopeId) {
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
    ) {
        let alias_info = SqlDataSource::cast(self.ctx.syntax(node))
            .and_then(|source| source.alias())
            .and_then(|alias_node| {
                self.ctx
                    .node_name(alias_node.syntax().id())
                    .map(|(name, _)| (name, alias_node.syntax().range()))
            });
        let Some((name_text, name_range)) = SqlDataSource::cast(self.ctx.syntax(node))
            .and_then(|source| source.source_name(self.ctx.source()))
        else {
            return;
        };
        let name = Arc::<str>::from(name_text.to_ascii_lowercase());
        let alias = alias_info.as_ref().map(|(name, _)| Arc::clone(name));

        let source_range = self.ctx.file().range(node);
        self.ctx.emit_sql_source(SqlSourceData {
            query_id,
            range: source_range,
            source_kind,
            name: Arc::clone(&name),
            alias: alias.clone(),
            join_kind,
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

        let mut target_name = None;
        let mut is_inline = false;
        let children: Vec<_> = into_clause
            .target_children()
            .map(|child| child.id())
            .collect();
        for child in children {
            match self.ctx.file().kind(child) {
                SyntaxKind::DataInlineDecl => {
                    is_inline = true;
                    target_name = self.inline_decl_name(child);
                    self.ctx.decl_lowering().walk_inline_decl(child, scope);
                }
                SyntaxKind::FieldSymbolInlineDecl => {
                    is_inline = true;
                    target_name = self.inline_decl_name(child);
                    self.ctx
                        .decl_lowering()
                        .declare_inline_field_symbol_decl(child, scope, None, None);
                }
                SyntaxKind::ExprIdent
                | SyntaxKind::SelectorExpr
                | SyntaxKind::CallExpr
                | SyntaxKind::BinaryExpr
                | SyntaxKind::UnaryExpr
                | SyntaxKind::ParenExpr
                | SyntaxKind::ConstructorExpr => {
                    if target_name.is_none() {
                        target_name = self.ctx.sql_target_name_from_expr(child);
                    }
                    self.ctx.expr_lowering().collect_expr(child, scope);
                }
                SyntaxKind::TemplateExpr => {
                    for grandchild in self.ctx.file().children(child) {
                        if target_name.is_none() {
                            target_name = self.ctx.sql_target_name_from_expr(grandchild);
                        }
                        self.ctx.expr_lowering().collect_expr(grandchild, scope);
                    }
                }
                _ => {}
            }
        }

        let target_range = self.ctx.file().range(node);
        self.ctx.emit_sql_target(SqlTargetData {
            query_id,
            scope,
            range: target_range,
            kind: if is_appending {
                SqlTargetKind::Appending
            } else {
                SqlTargetKind::Into
            },
            target_name,
            is_table,
            is_corresponding,
            is_inline,
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
                self.collect_sql_host_refs_from_syntax_tokens(&syntax_tokens, scope);
                self.collect_sql_name_refs_from_syntax_tokens(
                    query_id,
                    scope,
                    &syntax_tokens,
                    true,
                );
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
                    if self.sql_token_is_keyword_text(text) {
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

    fn inline_decl_name(&self, node: NodeId) -> Option<Arc<str>> {
        self.ctx
            .syntax(node)
            .child_by_kind(SyntaxKind::DataDeclName)
            .and_then(DataDeclName::cast)
            .and_then(|name| name.name(self.ctx.source()))
    }
    fn sql_token_is_keyword_text(&self, text: &str) -> bool {
        matches!(
            text.to_ascii_lowercase().as_str(),
            "select"
                | "single"
                | "distinct"
                | "from"
                | "into"
                | "appending"
                | "where"
                | "group"
                | "by"
                | "having"
                | "order"
                | "for"
                | "all"
                | "entries"
                | "in"
                | "up"
                | "to"
                | "rows"
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
                | "like"
                | "between"
                | "is"
                | "null"
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
