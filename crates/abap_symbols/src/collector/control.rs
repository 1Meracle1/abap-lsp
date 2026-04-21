use std::sync::Arc;

use abap_ast::SyntaxKind;
use abap_ast::arena::NodeId;
use abap_ast::ast::{AstNode, ConstructorBaseClause, ConstructorExpr, SortStmt, TableExpr};
use abap_lexer::TextRange;

use crate::def_map::{
    AtGroupKind, AtRegionData, CaseRegionData, FieldAccess, FieldAccessSegment, FieldTypeRefData,
    IfRegionData, LoopAtFieldContext, LoopRegionData, LoopWhereFieldContext,
    RoutineControlRegionData, RoutineLoopKind, SymbolKind, SystemFieldStatementKind, TryRegionData,
    TypeFactData, ValueFlowEdgeData, ValueFlowKind, ValueFlowTargetData, ValueStateCheckData,
    ValueStateCheckKind,
};
use crate::ids::{ScopeId, StructureId, SymbolId};
use crate::scope::{Namespace, ScopeKind};

use super::emit::RefSink;
use super::{Collector, LoopGroupContext, SyntaxTokenInfo};

pub(super) struct ControlLowering<'ctx, 'a> {
    collector: &'ctx mut Collector<'a>,
}

impl<'a> Collector<'a> {
    pub(super) fn control_lowering(&mut self) -> ControlLowering<'_, 'a> {
        ControlLowering { collector: self }
    }
}

impl<'ctx, 'a> ControlLowering<'ctx, 'a> {
    fn record_system_field_updates(
        &mut self,
        scope: ScopeId,
        node: NodeId,
        statement: SystemFieldStatementKind,
        field_names: &[&'static str],
    ) {
        let range = self.collector.file.range(node);
        for &field_name in field_names {
            self.collector
                .add_system_field_update(scope, range.clone(), statement, field_name);
        }
    }

    pub(super) fn walk_if_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let node_range = self.collector.file.range(node);
        let then_scope =
            self.collector
                .push_scope(ScopeKind::IfBranch, node_range.clone(), Some(scope), None);
        self.collect_condition_probe_refs(node, then_scope);
        let mut elseif_scopes = Vec::new();
        let mut else_scope = None;
        for child in self.collector.file.children(node) {
            match self.collector.file.kind(child) {
                SyntaxKind::ElseifClause => {
                    elseif_scopes.push(self.walk_nested_block(
                        child,
                        scope,
                        ScopeKind::ElseifBranch,
                    ));
                }
                SyntaxKind::ElseClause => {
                    else_scope = Some(self.walk_nested_block(child, scope, ScopeKind::ElseBranch));
                }
                _ => self.collector.walk_node(child, then_scope),
            }
        }
        self.collector
            .add_routine_control_region(RoutineControlRegionData::If(IfRegionData {
                scope,
                range: node_range,
                then_scope,
                elseif_scopes,
                else_scope,
            }));
    }

    pub(super) fn walk_case_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let node_range = self.collector.file.range(node);
        let mut when_scopes = Vec::new();
        let mut has_when_others = false;
        for child in self.collector.file.children(node) {
            match self.collector.file.kind(child) {
                SyntaxKind::WhenClause => {
                    let (when_scope, is_when_others) = self.walk_when_clause(child, scope);
                    when_scopes.push(when_scope);
                    has_when_others |= is_when_others;
                }
                _ => self.collector.walk_node(child, scope),
            }
        }
        self.collector
            .add_routine_control_region(RoutineControlRegionData::Case(CaseRegionData {
                scope,
                range: node_range,
                when_scopes,
                has_when_others,
            }));
    }

    pub(super) fn walk_while_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_system_field_updates(scope, node, SystemFieldStatementKind::While, &["index"]);
        self.walk_loop_like_stmt(node, scope, ScopeKind::WhileBlock, RoutineLoopKind::While);
    }

    pub(super) fn walk_do_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_system_field_updates(scope, node, SystemFieldStatementKind::Do, &["index"]);
        self.walk_loop_like_stmt(node, scope, ScopeKind::DoBlock, RoutineLoopKind::Do);
    }

    pub(super) fn walk_loop_stmt(&mut self, node: NodeId, scope: ScopeId) {
        self.record_system_field_updates(
            scope,
            node,
            SystemFieldStatementKind::LoopAt,
            &["subrc", "tabix", "tfill", "tleng"],
        );
        let node_range = self.collector.file.range(node);
        let child_scope =
            self.collector
                .push_scope(ScopeKind::LoopBlock, node_range.clone(), Some(scope), None);
        let loop_context = self.collect_loop_header_node(node, child_scope);
        self.collector.loop_group_stack.push(loop_context.clone());
        for child in self.collector.file.children(node) {
            match self.collector.file.kind(child) {
                SyntaxKind::LoopSourceClause
                | SyntaxKind::LoopIntoClause
                | SyntaxKind::LoopAssigningClause
                | SyntaxKind::LoopReferenceIntoClause
                | SyntaxKind::LoopWhereClause
                | SyntaxKind::LoopFromClause
                | SyntaxKind::LoopToClause
                | SyntaxKind::LoopStepClause
                | SyntaxKind::Token => {}
                _ => self.collector.walk_node(child, child_scope),
            }
        }
        self.collector.loop_group_stack.pop();
        self.collector
            .add_routine_control_region(RoutineControlRegionData::Loop(LoopRegionData {
                scope,
                range: node_range,
                kind: RoutineLoopKind::Loop,
                body_scope: child_scope,
                source_access: loop_context.source_access,
                target_access: loop_context.target_access,
            }));
    }

    pub(super) fn walk_at_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let Some(header) = self.at_stmt_header(node) else {
            self.collector.walk_children(node, scope);
            return;
        };

        let node_range = self.collector.file.range(node);
        let child_scope =
            self.collector
                .push_scope(ScopeKind::AtBlock, node_range.clone(), Some(scope), None);
        if let Some(range) = header.key_range.clone()
            && let Some(loop_context) = self.collector.loop_group_stack.last()
            && let Some(source_access) = loop_context.source_access.clone()
        {
            self.collector
                .loop_at_field_contexts
                .push(LoopAtFieldContext {
                    scope: child_scope,
                    range,
                    source_access,
                    target_access: loop_context.target_access.clone(),
                });
        }
        if !header.key_tokens.is_empty() {
            self.collector.collect_token_expression_refs_infos(
                &header.key_tokens,
                child_scope,
                true,
            );
        }

        let mut body_started = false;
        for child in self.collector.file.children(node) {
            if !body_started {
                if self.collector.file.kind(child) == SyntaxKind::Token {
                    let tokens = self.collector.syntax_token_nodes(child);
                    if tokens.iter().any(|token| token.text.as_ref() == ".") {
                        body_started = true;
                    }
                }
                continue;
            }
            if self.collector.file.kind(child) != SyntaxKind::Token {
                self.collector.walk_node(child, child_scope);
            }
        }

        self.collector
            .add_routine_control_region(RoutineControlRegionData::At(AtRegionData {
                scope,
                range: node_range,
                kind: header.kind,
                body_scope: child_scope,
            }));
    }

    pub(super) fn walk_try_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let node_range = self.collector.file.range(node);
        let body_scope =
            self.collector
                .push_scope(ScopeKind::TryBlock, node_range.clone(), Some(scope), None);
        let mut catch_scopes = Vec::new();
        let mut cleanup_scope = None;
        for child in self.collector.file.children(node) {
            match self.collector.file.kind(child) {
                SyntaxKind::CatchClause => {
                    catch_scopes.push(self.walk_catch_clause(child, body_scope));
                }
                SyntaxKind::CleanupClause => {
                    cleanup_scope =
                        Some(self.walk_nested_block(child, body_scope, ScopeKind::CleanupClause));
                }
                _ => self.collector.walk_node(child, body_scope),
            }
        }
        self.collector
            .add_routine_control_region(RoutineControlRegionData::Try(TryRegionData {
                scope,
                range: node_range,
                body_scope,
                catch_scopes,
                cleanup_scope,
            }));
    }

    pub(super) fn walk_catch_clause(&mut self, node: NodeId, scope: ScopeId) -> ScopeId {
        let node_range = self.collector.file.range(node);
        let child_scope =
            self.collector
                .push_scope(ScopeKind::CatchClause, node_range, Some(scope), None);
        let inline_decl_metadata = self.catch_inline_decl_metadata(node, child_scope);
        for child in self.collector.file.children(node) {
            match self.collector.file.kind(child) {
                SyntaxKind::DataInlineDecl => {
                    self.declare_inline_variable_target(child, child_scope, &inline_decl_metadata);
                }
                _ => self.collector.walk_node(child, child_scope),
            }
        }
        child_scope
    }

    pub(super) fn walk_nested_block(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        kind: ScopeKind,
    ) -> ScopeId {
        let node_range = self.collector.file.range(node);
        let child_scope = self
            .collector
            .push_scope(kind, node_range, Some(scope), None);
        if self.collector.file.kind(node) == SyntaxKind::ElseifClause {
            self.collect_condition_probe_refs(node, child_scope);
        }
        for child in self.collector.file.children(node) {
            self.collector.walk_node(child, child_scope);
        }
        child_scope
    }

    pub(super) fn walk_when_clause(&mut self, node: NodeId, scope: ScopeId) -> (ScopeId, bool) {
        let node_range = self.collector.file.range(node);
        let child_scope =
            self.collector
                .push_scope(ScopeKind::WhenBranch, node_range, Some(scope), None);
        let mut header_tokens = Vec::new();
        let mut before_period = true;

        for child in self.collector.file.children(node) {
            if before_period {
                if self.collector.file.kind(child) == SyntaxKind::Token {
                    let tokens = self.collector.syntax_token_nodes(child);
                    before_period = !tokens.iter().any(|token| token.text.as_ref() == ".");
                    header_tokens.extend(tokens);
                    continue;
                }
            }
            self.collector.walk_node(child, child_scope);
        }

        let meaningful_header: Vec<_> = header_tokens
            .into_iter()
            .filter(|token| !self.collector.syntax_token_is_comment(token))
            .take_while(|token| token.text.as_ref() != ".")
            .collect();
        let is_when_others = meaningful_header.len() > 1
            && meaningful_header[1..]
                .iter()
                .all(|token| token.text.eq_ignore_ascii_case("others"));
        if meaningful_header.len() > 1 && !is_when_others {
            self.collector.collect_token_expression_refs_infos(
                &meaningful_header[1..],
                child_scope,
                true,
            );
        }
        (child_scope, is_when_others)
    }

    pub(super) fn select_stmt_has_endselect(&self, node: NodeId) -> bool {
        self.collector.file.children(node).any(|child| {
            self.collector.file.kind(child) == SyntaxKind::Token
                && self
                    .collector
                    .syntax(child)
                    .text(self.collector.source)
                    .is_some_and(|text| text.eq_ignore_ascii_case("endselect"))
        })
    }

    fn internal_table_line_selector_allowed_for_source(
        &self,
        expr: NodeId,
        scope: ScopeId,
    ) -> bool {
        let (structure, _) = self.loop_source_line_metadata_from_node(expr, scope);
        match structure {
            None => true,
            Some(structure_id) => self
                .collector
                .structure(structure_id)
                .is_some_and(|structure| structure.fields.len() == 1),
        }
    }

    fn collect_loop_header_node(&mut self, node: NodeId, scope: ScopeId) -> LoopGroupContext {
        let mut source_metadata = (None, None);
        let mut source_access = None;
        let mut source_range = None;
        let mut target_access = None;
        let mut allows_internal_table_line_selector = false;
        for child in self.collector.file.children(node) {
            match self.collector.file.kind(child) {
                SyntaxKind::LoopSourceClause => {
                    if let Some(expr) = self.collector.first_non_token_child(child) {
                        allows_internal_table_line_selector =
                            self.internal_table_line_selector_allowed_for_source(expr, scope);
                        self.collector.expr_lowering().collect_expr(expr, scope);
                        source_metadata = self.loop_source_line_metadata_from_node(expr, scope);
                        source_access = self.collector.value_access_from_node(expr, scope);
                        source_range = Some(self.collector.file.range(expr));
                    }
                }
                SyntaxKind::LoopIntoClause => {
                    if let Some(target) = self.collector.first_non_token_child(child) {
                        target_access = self.loop_target_access_from_node(target, scope);
                        self.collect_loop_target_node(
                            target,
                            scope,
                            SymbolKind::Variable,
                            &source_metadata,
                        );
                    }
                }
                SyntaxKind::LoopAssigningClause => {
                    if let Some(target) = self.collector.first_non_token_child(child) {
                        target_access = self.loop_target_access_from_node(target, scope);
                        self.collect_loop_target_node(
                            target,
                            scope,
                            SymbolKind::FieldSymbol,
                            &source_metadata,
                        );
                        if let (Some(source_access), Some(source_range), Some(target_access)) = (
                            source_access.as_ref(),
                            source_range.clone(),
                            target_access.as_ref(),
                        ) {
                            self.emit_loop_field_symbol_binding_edge(
                                scope,
                                source_access,
                                source_range,
                                &source_metadata,
                                target_access,
                            );
                        }
                    }
                }
                SyntaxKind::LoopReferenceIntoClause => {
                    if let Some(target) = self.collector.last_non_token_child(child) {
                        target_access = self.loop_target_access_from_node(target, scope);
                        self.collect_loop_target_node(
                            target,
                            scope,
                            SymbolKind::Variable,
                            &(None, None),
                        );
                    }
                }
                SyntaxKind::LoopWhereClause
                | SyntaxKind::LoopFromClause
                | SyntaxKind::LoopToClause
                | SyntaxKind::LoopStepClause => {
                    if let Some(expr) = self.collector.first_non_token_child(child) {
                        if self.collector.file.kind(child) == SyntaxKind::LoopWhereClause
                            && let Some(source_access) = source_access.clone()
                        {
                            self.collector
                                .loop_where_field_contexts
                                .push(LoopWhereFieldContext {
                                    scope,
                                    range: self.collector.file.range(child),
                                    source_access,
                                    target_access: target_access.clone(),
                                });
                        }
                        self.collector.expr_lowering().collect_expr(expr, scope);
                    }
                }
                _ => {}
            }
        }
        self.collector.scopes[scope.as_usize()].allows_internal_table_line_selector =
            allows_internal_table_line_selector;
        LoopGroupContext {
            source_access,
            target_access,
        }
    }

    fn emit_loop_field_symbol_binding_edge(
        &mut self,
        scope: ScopeId,
        source_access: &FieldAccess,
        source_range: TextRange,
        source_metadata: &(Option<StructureId>, Option<FieldTypeRefData>),
        target_access: &FieldAccess,
    ) {
        let source_type = TypeFactData {
            structure: source_metadata.0,
            declared_type: source_metadata.1.clone(),
            type_clause_display: None,
            table_line: None,
        };
        let kind = if source_access.base_namespace == Namespace::Value
            && source_access.field_path.is_empty()
        {
            ValueFlowKind::FieldSymbolAssignment
        } else {
            ValueFlowKind::ConditionalFieldSymbolAssignment
        };
        self.collector.emit_value_flow_edge(ValueFlowEdgeData {
            scope,
            kind,
            source_range,
            source_type: source_type.clone(),
            target: ValueFlowTargetData::FieldSymbol {
                range: target_access.base_range.clone(),
                name: Some(Arc::clone(&target_access.base_name)),
            },
            target_type: source_type,
        });
    }

    fn walk_loop_like_stmt(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        scope_kind: ScopeKind,
        loop_kind: RoutineLoopKind,
    ) {
        let node_range = self.collector.file.range(node);
        let body_scope =
            self.collector
                .push_scope(scope_kind, node_range.clone(), Some(scope), None);
        self.collect_condition_probe_refs(node, body_scope);
        for child in self.collector.file.children(node) {
            self.collector.walk_node(child, body_scope);
        }
        self.collector
            .add_routine_control_region(RoutineControlRegionData::Loop(LoopRegionData {
                scope,
                range: node_range,
                kind: loop_kind,
                body_scope,
                source_access: None,
                target_access: None,
            }));
    }

    fn loop_target_access_from_node(&self, node: NodeId, scope: ScopeId) -> Option<FieldAccess> {
        match self.collector.file.kind(node) {
            SyntaxKind::DataInlineDecl | SyntaxKind::FieldSymbolInlineDecl => self
                .collector
                .file
                .children(node)
                .find(|&child| self.collector.file.kind(child) == SyntaxKind::DataDeclName)
                .and_then(|name_node| self.collector.node_name(name_node))
                .map(|(name, range)| FieldAccess {
                    scope,
                    base_namespace: Namespace::Value,
                    base_name: name,
                    base_range: range,
                    field_path: Vec::new(),
                    in_type_position: false,
                }),
            _ => self.collector.value_access_from_node(node, scope),
        }
    }

    fn collect_condition_probe_refs(&mut self, node: NodeId, scope: ScopeId) {
        let Some(condition) = self
            .collector
            .syntax(node)
            .non_token_children()
            .next()
            .map(|child| child.id())
        else {
            return;
        };
        self.collect_direct_value_probe_refs(condition, scope);
    }

    fn collect_direct_value_probe_refs(&mut self, node: NodeId, scope: ScopeId) {
        match self.collector.file.kind(node) {
            SyntaxKind::ExprIdent => {
                let Some((name, range)) = self.collector.node_name(node) else {
                    return;
                };
                if name.starts_with('<') && name.ends_with('>') {
                    return;
                }
                self.collector.add_value_state_check(ValueStateCheckData {
                    scope,
                    range: range.clone(),
                    symbol_name: name,
                    symbol_range: range,
                    field_name: None,
                    kind: ValueStateCheckKind::ConditionProbe,
                });
            }
            SyntaxKind::TemplateExpr
            | SyntaxKind::ParenExpr
            | SyntaxKind::UnaryExpr
            | SyntaxKind::BinaryExpr
            | SyntaxKind::IsPredicate
            | SyntaxKind::BetweenExpr
            | SyntaxKind::InstanceOfPredicate => {
                let children: Vec<_> = self
                    .collector
                    .syntax(node)
                    .non_token_children()
                    .map(|child| child.id())
                    .collect();
                for child in children {
                    self.collect_direct_value_probe_refs(child, scope);
                }
            }
            _ => {}
        }
    }

    fn collect_loop_target_node(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        symbol_kind: SymbolKind,
        inferred_metadata: &(Option<StructureId>, Option<FieldTypeRefData>),
    ) {
        match self.collector.file.kind(node) {
            SyntaxKind::DataInlineDecl if symbol_kind == SymbolKind::Variable => {
                self.declare_inline_variable_target(node, scope, inferred_metadata);
            }
            SyntaxKind::FieldSymbolInlineDecl if symbol_kind == SymbolKind::FieldSymbol => {
                self.collector
                    .decl_lowering()
                    .declare_inline_field_symbol_decl(
                        node,
                        scope,
                        inferred_metadata.0,
                        inferred_metadata.1.clone(),
                    );
            }
            _ => self.collector.expr_lowering().collect_expr(node, scope),
        }
    }

    fn declare_inline_variable_target(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        inferred_metadata: &(Option<StructureId>, Option<FieldTypeRefData>),
    ) {
        let decl_scope = self.collector.declaration_scope(scope);
        if let Some(name_node) = self
            .collector
            .file
            .children(node)
            .find(|&child| self.collector.file.kind(child) == SyntaxKind::DataDeclName)
            && let Some((name, range)) = self.collector.node_name(name_node)
        {
            self.collector.declare_symbol(
                decl_scope,
                name,
                SymbolKind::Variable,
                range,
                inferred_metadata.0,
                inferred_metadata.1.clone(),
                None,
                None,
            );
        }
    }

    fn catch_inline_decl_metadata(
        &self,
        node: NodeId,
        scope: ScopeId,
    ) -> (Option<StructureId>, Option<FieldTypeRefData>) {
        let type_refs = self.collector.direct_type_ref_children(node);
        if type_refs.len() != 1 {
            return (None, None);
        }
        let Some(mut declared_type) = self
            .collector
            .field_type_ref_from_node(type_refs[0], Namespace::Type)
        else {
            return (None, None);
        };
        declared_type.is_ref = true;
        let structure = if declared_type.field_path.is_empty() {
            self.collector
                .lookup_symbol_in_scope_chain(
                    scope,
                    Namespace::Type,
                    declared_type.base_name.as_ref(),
                )
                .and_then(|symbol_id| self.collector.symbol(symbol_id).structure)
        } else {
            None
        };
        (structure, Some(declared_type))
    }

    pub(super) fn loop_source_line_metadata_from_node(
        &self,
        node: NodeId,
        scope: ScopeId,
    ) -> (Option<StructureId>, Option<FieldTypeRefData>) {
        let node = self.collector.unwrap_simple_expr_wrapper(node);
        match self.collector.file.kind(node) {
            SyntaxKind::ConstructorExpr => {
                let Some(constructor) = ConstructorExpr::cast(self.collector.syntax(node)) else {
                    return (None, None);
                };
                if let Some(type_ref) = constructor.type_ref()
                    && let Some(display_text) = type_ref.display_text(self.collector.source)
                    && display_text != "#"
                    && let Some(mut declared_type) = self
                        .collector
                        .field_type_ref_from_node(type_ref.syntax().id(), Namespace::Type)
                {
                    if matches!(
                        constructor.keyword(self.collector.source).as_deref(),
                        Some("new" | "ref")
                    ) {
                        declared_type.is_ref = true;
                    }
                    let (structure, declared_type) =
                        self.normalize_inferred_metadata(scope, None, Some(declared_type));
                    return self.collector.internal_table_line_metadata(
                        scope,
                        structure,
                        declared_type,
                    );
                }
                if let Some(base_value) = constructor
                    .arg_list()
                    .and_then(|arg_list| {
                        self.collector
                            .file
                            .find_first_kind(
                                arg_list.syntax().id(),
                                SyntaxKind::ConstructorBaseClause,
                            )
                            .and_then(|node| {
                                ConstructorBaseClause::cast(self.collector.syntax(node))
                            })
                    })
                    .and_then(|clause| clause.value())
                {
                    return self.loop_source_line_metadata_from_node(base_value.id(), scope);
                }
                if let Some(arg_list) = constructor.arg_list() {
                    let tokens = self.collector.syntax_token_nodes(arg_list.syntax().id());
                    if tokens.len() >= 3 && tokens[1].text.eq_ignore_ascii_case("BASE") {
                        let inner = &tokens[1..tokens.len() - 1];
                        if let Some((_, namespace, base_name, _, field_path, _)) =
                            self.collector.consume_selector_access_from_infos(inner, 1)
                            && namespace == Namespace::Value
                        {
                            if field_path.is_empty() {
                                if let Some(symbol_id) =
                                    self.collector.lookup_symbol_in_scope_chain(
                                        scope,
                                        Namespace::Value,
                                        base_name.as_ref(),
                                    )
                                {
                                    let symbol = self.collector.symbol(symbol_id);
                                    let (structure, declared_type) = self
                                        .normalize_inferred_metadata(
                                            scope,
                                            symbol.structure,
                                            symbol.declared_type.clone(),
                                        );
                                    return self.collector.internal_table_line_metadata(
                                        scope,
                                        structure,
                                        declared_type,
                                    );
                                }
                            } else if let Some(symbol_id) =
                                self.collector.lookup_symbol_in_scope_chain(
                                    scope,
                                    Namespace::Value,
                                    base_name.as_ref(),
                                )
                            {
                                if let Some((structure, declared_type)) =
                                    self.loop_source_field_metadata(scope, symbol_id, &field_path)
                                {
                                    let (structure, declared_type) = self
                                        .normalize_inferred_metadata(
                                            scope,
                                            structure,
                                            declared_type,
                                        );
                                    return self.collector.internal_table_line_metadata(
                                        scope,
                                        structure,
                                        declared_type,
                                    );
                                }
                            }
                        }
                    }
                }
                (None, None)
            }
            SyntaxKind::TemplateExpr => {
                if let Some(child) = self.collector.first_non_token_child(node) {
                    return self.loop_source_line_metadata_from_node(child, scope);
                }
                let tokens = self.collector.syntax_token_nodes(node);
                if tokens.len() == 1
                    && self.collector.syntax_token_is_ident_like(&tokens[0])
                    && let Some(symbol_id) = self.collector.lookup_symbol_in_scope_chain(
                        scope,
                        Namespace::Value,
                        tokens[0].text.as_ref(),
                    )
                {
                    let symbol = self.collector.symbol(symbol_id);
                    let (structure, declared_type) = self.normalize_inferred_metadata(
                        scope,
                        symbol.structure,
                        symbol.declared_type.clone(),
                    );
                    return self.collector.internal_table_line_metadata(
                        scope,
                        structure,
                        declared_type,
                    );
                }
                (None, None)
            }
            SyntaxKind::ExprIdent => {
                let Some((name, _)) = self.collector.node_name(node) else {
                    return (None, None);
                };
                let Some(symbol_id) = self.collector.lookup_symbol_in_scope_chain(
                    scope,
                    Namespace::Value,
                    name.as_ref(),
                ) else {
                    return (None, None);
                };
                let symbol = self.collector.symbol(symbol_id);
                let (structure, declared_type) = self.normalize_inferred_metadata(
                    scope,
                    symbol.structure,
                    symbol.declared_type.clone(),
                );
                self.collector
                    .internal_table_line_metadata(scope, structure, declared_type)
            }
            SyntaxKind::SelectorExpr => {
                let Some((namespace, base_name, _, field_path)) =
                    self.collector.selector_access_chain(node)
                else {
                    return (None, None);
                };
                if namespace != Namespace::Value {
                    return (None, None);
                }
                let Some(symbol_id) = self.collector.lookup_symbol_in_scope_chain(
                    scope,
                    Namespace::Value,
                    base_name.as_ref(),
                ) else {
                    return (None, None);
                };
                if field_path.is_empty() {
                    let symbol = self.collector.symbol(symbol_id);
                    let (structure, declared_type) = self.normalize_inferred_metadata(
                        scope,
                        symbol.structure,
                        symbol.declared_type.clone(),
                    );
                    return self.collector.internal_table_line_metadata(
                        scope,
                        structure,
                        declared_type,
                    );
                }
                self.loop_source_field_metadata(scope, symbol_id, &field_path)
                    .map(|(structure, declared_type)| {
                        let (structure, declared_type) =
                            self.normalize_inferred_metadata(scope, structure, declared_type);
                        self.collector
                            .internal_table_line_metadata(scope, structure, declared_type)
                    })
                    .unwrap_or((None, None))
            }
            SyntaxKind::TableExpr => {
                let Some(base) =
                    TableExpr::cast(self.collector.syntax(node)).and_then(|expr| expr.base())
                else {
                    return (None, None);
                };
                let (structure, declared_type) =
                    self.loop_source_line_metadata_from_node(base.id(), scope);
                self.collector
                    .internal_table_line_metadata(scope, structure, declared_type)
            }
            _ => (None, None),
        }
    }

    fn loop_source_field_metadata(
        &self,
        scope: ScopeId,
        symbol_id: SymbolId,
        field_path: &[FieldAccessSegment],
    ) -> Option<(Option<StructureId>, Option<FieldTypeRefData>)> {
        let mut structure = self.collector.symbol(symbol_id).structure;
        let mut declared_type = self.collector.symbol(symbol_id).declared_type.clone();
        for segment in field_path {
            if segment.is_deref() {
                let (next_structure, next_declared_type) =
                    self.dereference_metadata(scope, structure, declared_type)?;
                structure = next_structure;
                declared_type = next_declared_type;
                continue;
            }
            let structure_id = structure?;
            let field = self
                .collector
                .structure(structure_id)?
                .fields
                .iter()
                .find(|field| field.name.as_ref() == segment.name.as_ref())?;
            structure = field.structure;
            declared_type = field.type_ref.clone();
        }
        Some((structure, declared_type))
    }

    fn dereference_metadata(
        &self,
        scope: ScopeId,
        structure: Option<StructureId>,
        declared_type: Option<FieldTypeRefData>,
    ) -> Option<(Option<StructureId>, Option<FieldTypeRefData>)> {
        let type_ref = declared_type?;
        if !type_ref.is_ref {
            return None;
        }
        let structure = structure.or_else(|| {
            if type_ref.namespace != Namespace::Type || !type_ref.field_path.is_empty() {
                return None;
            }
            self.collector
                .lookup_symbol_in_scope_chain(scope, Namespace::Type, type_ref.base_name.as_ref())
                .and_then(|symbol_id| self.collector.symbol(symbol_id).structure)
        });
        Some((
            structure,
            Some(FieldTypeRefData {
                namespace: type_ref.namespace,
                is_ref: false,
                base_name: type_ref.base_name,
                field_path: type_ref.field_path,
            }),
        ))
    }

    fn normalize_inferred_metadata(
        &self,
        scope: ScopeId,
        mut structure: Option<StructureId>,
        mut declared_type: Option<FieldTypeRefData>,
    ) -> (Option<StructureId>, Option<FieldTypeRefData>) {
        for _ in 0..8 {
            if structure.is_some() {
                break;
            }
            let Some(type_ref) = declared_type.as_ref() else {
                break;
            };
            if type_ref.namespace != Namespace::Type
                || type_ref.is_ref
                || !type_ref.field_path.is_empty()
            {
                break;
            }
            let Some(symbol_id) = self.collector.lookup_symbol_in_scope_chain(
                scope,
                Namespace::Type,
                type_ref.base_name.as_ref(),
            ) else {
                break;
            };
            let symbol = self.collector.symbol(symbol_id);
            if symbol.structure.is_none() && symbol.declared_type.is_none() {
                break;
            }
            structure = symbol.structure;
            declared_type = symbol.declared_type.clone();
        }
        (structure, declared_type)
    }

    pub(super) fn collect_sort_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let Some((source, by_operands)) = SortStmt::cast(self.collector.syntax(node)).map(|stmt| {
            (
                stmt.source(self.collector.source).map(|source| source.id()),
                stmt.by_operands(self.collector.source)
                    .into_iter()
                    .map(|child| child.id())
                    .collect::<Vec<_>>(),
            )
        }) else {
            self.collector.walk_children(node, scope);
            return;
        };
        let source_expr = source
            .map(|source| self.sort_operand_expr_node(source))
            .unwrap_or(node);
        let itab_base = source.and_then(|_| self.collector.sql_target_name_from_expr(source_expr));

        if let Some(source) = source {
            self.collector.walk_node(source, scope);
        }
        let Some(itab_base) = itab_base else {
            for child in by_operands {
                self.collector.walk_node(child, scope);
            }
            return;
        };

        for child in by_operands {
            let expr = self.sort_operand_expr_node(child);
            if let Some(field_path) = self.sort_by_field_segments_from_expr(expr) {
                self.collector.emit_field_access(FieldAccess {
                    scope,
                    base_namespace: Namespace::Value,
                    base_name: Arc::clone(&itab_base),
                    base_range: self.collector.file.range(expr),
                    field_path,
                    in_type_position: false,
                });
                continue;
            }
            self.collector.walk_node(child, scope);
        }
    }

    fn sort_operand_expr_node(&self, node: NodeId) -> NodeId {
        if self.collector.file.kind(node) == SyntaxKind::TemplateExpr {
            self.collector.first_non_token_child(node).unwrap_or(node)
        } else {
            node
        }
    }

    fn sort_by_field_segments_from_expr(&self, inner: NodeId) -> Option<Vec<FieldAccessSegment>> {
        match self.collector.file.kind(inner) {
            SyntaxKind::ExprIdent => {
                let (name, range) = self.collector.node_name(inner)?;
                Some(vec![FieldAccessSegment { name, range }])
            }
            SyntaxKind::SelectorExpr => {
                let (namespace, base_name, base_range, mut path) =
                    self.collector.selector_access_chain(inner)?;
                if namespace != Namespace::Value {
                    return None;
                }
                let mut out = vec![FieldAccessSegment {
                    name: base_name,
                    range: base_range,
                }];
                out.append(&mut path);
                Some(out)
            }
            _ => None,
        }
    }

    fn at_stmt_header(&self, node: NodeId) -> Option<AtStmtHeader> {
        let mut header_tokens = Vec::new();
        for child in self.collector.file.children(node) {
            if self.collector.file.kind(child) != SyntaxKind::Token {
                break;
            }
            let tokens = self.collector.syntax_token_nodes(child);
            let saw_period = tokens.iter().any(|token| token.text.as_ref() == ".");
            header_tokens.extend(tokens.into_iter().filter(|token| {
                !self.collector.syntax_token_is_comment(token) && token.text.as_ref() != "."
            }));
            if saw_period {
                break;
            }
        }
        if header_tokens.len() < 2 || !header_tokens[0].text.eq_ignore_ascii_case("at") {
            return None;
        }

        if header_tokens[1].text.eq_ignore_ascii_case("first") {
            return Some(AtStmtHeader {
                kind: AtGroupKind::First,
                key_tokens: Vec::new(),
                key_range: None,
            });
        }
        if header_tokens[1].text.eq_ignore_ascii_case("last") {
            return Some(AtStmtHeader {
                kind: AtGroupKind::Last,
                key_tokens: Vec::new(),
                key_range: None,
            });
        }

        let (kind, key_tokens) = if header_tokens[1].text.eq_ignore_ascii_case("new") {
            (AtGroupKind::New, header_tokens[2..].to_vec())
        } else if header_tokens.len() >= 4
            && header_tokens[1].text.eq_ignore_ascii_case("end")
            && header_tokens[2].text.eq_ignore_ascii_case("of")
        {
            (AtGroupKind::EndOf, header_tokens[3..].to_vec())
        } else {
            return None;
        };

        let key_range = match (key_tokens.first(), key_tokens.last()) {
            (Some(first), Some(last)) => Some(first.range.start..last.range.end),
            _ => None,
        };
        Some(AtStmtHeader {
            kind,
            key_tokens,
            key_range,
        })
    }
}

#[derive(Debug, Clone)]
struct AtStmtHeader {
    kind: AtGroupKind,
    key_tokens: Vec<SyntaxTokenInfo>,
    key_range: Option<TextRange>,
}
