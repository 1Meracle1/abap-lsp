use abap_ast::File;
use abap_ast::arena::NodeId;
use abap_ast::ast::SyntaxNodeRef;

use crate::def_map::{
    FieldAccess, FieldTypeRefData, FormRoutineData, IncludeEdge, NamedArgumentAccess,
    NamedArgumentSection, NamedArgumentTarget, ReferenceKind, SqlNameRefData, SqlPredicateData,
    SqlProjectionData, SqlQueryData, SqlSourceData, SqlTargetData, StructureFieldData, SymbolKind,
};
use crate::ids::{ScopeId, StructureId};
use crate::scope::Namespace;

use super::decls::DeclLowering;
use super::emit::{FormSink, RefSink, SqlSink};
use super::{Collector, PendingStructure, SyntaxTokenInfo};

pub(super) struct CollectorContext<'ctx, 'a> {
    pub(super) collector: &'ctx mut Collector<'a>,
}

impl<'ctx, 'a> CollectorContext<'ctx, 'a> {
    pub(super) fn new(collector: &'ctx mut Collector<'a>) -> Self {
        Self { collector }
    }
}

pub(super) struct ExprContext<'ctx, 'a> {
    collector: &'ctx mut Collector<'a>,
}

impl<'ctx, 'a> ExprContext<'ctx, 'a> {
    pub(super) fn new(collector: &'ctx mut Collector<'a>) -> Self {
        Self { collector }
    }

    pub(super) fn file(&self) -> &'a File {
        self.collector.file
    }

    pub(super) fn source(&self) -> &'a str {
        self.collector.source
    }

    pub(super) fn syntax(&self, node: NodeId) -> SyntaxNodeRef<'_> {
        self.collector.syntax(node)
    }

    pub(super) fn walk_node(&mut self, node: NodeId, scope: ScopeId) {
        self.collector.walk_node(node, scope);
    }

    pub(super) fn add_reference(
        &mut self,
        scope: ScopeId,
        name: std::sync::Arc<str>,
        namespace: Namespace,
        kind: crate::def_map::ReferenceKind,
        range: abap_lexer::TextRange,
    ) {
        self.collector
            .add_reference(scope, name, namespace, kind, range);
    }

    pub(super) fn push_loop_where_field_context(
        &mut self,
        context: crate::def_map::LoopWhereFieldContext,
    ) {
        self.collector.loop_where_field_contexts.push(context);
    }

    pub(super) fn node_name(
        &self,
        node: NodeId,
    ) -> Option<(std::sync::Arc<str>, abap_lexer::TextRange)> {
        self.collector.node_name(node)
    }

    pub(super) fn constructor_type_ref(
        &self,
        node: NodeId,
    ) -> Option<(std::sync::Arc<str>, abap_lexer::TextRange)> {
        self.collector.constructor_type_ref(node)
    }

    pub(super) fn decl_lowering(&mut self) -> DeclLowering<'_, 'a> {
        self.collector.decl_lowering()
    }

    pub(super) fn syntax_token_is_comment(&self, token: &SyntaxTokenInfo) -> bool {
        self.collector.syntax_token_is_comment(token)
    }

    pub(super) fn syntax_token_is_ident_like(&self, token: &SyntaxTokenInfo) -> bool {
        self.collector.syntax_token_is_ident_like(token)
    }

    pub(super) fn named_argument_section_from_text(
        &self,
        text: &str,
    ) -> Option<NamedArgumentSection> {
        self.collector.named_argument_section_from_text(text)
    }

    pub(super) fn collect_token_expression_refs_infos(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
        in_parens: bool,
    ) {
        self.collector
            .collect_token_expression_refs_infos(tokens, scope, in_parens);
    }

    pub(super) fn emit_named_argument(&mut self, access: NamedArgumentAccess) {
        self.collector.emit_named_argument(access);
    }

    pub(super) fn emit_field_access(&mut self, access: FieldAccess) {
        self.collector.emit_field_access(access);
    }

    pub(super) fn declare_inline_named_argument_target_infos(
        &mut self,
        scope: ScopeId,
        target: &NamedArgumentTarget,
        section: Option<NamedArgumentSection>,
        argument_name: std::sync::Arc<str>,
        tokens: &[SyntaxTokenInfo],
    ) -> bool {
        self.collector.declare_inline_named_argument_target_infos(
            scope,
            target,
            section,
            argument_name,
            tokens,
        )
    }

    pub(super) fn declare_inline_named_argument_target_from_nodes(
        &mut self,
        scope: ScopeId,
        target: &NamedArgumentTarget,
        section: Option<NamedArgumentSection>,
        argument_name: std::sync::Arc<str>,
        nodes: &[NodeId],
    ) -> Option<bool> {
        self.collector
            .declare_inline_named_argument_target_from_nodes(
                scope,
                target,
                section,
                argument_name,
                nodes,
            )
    }

    pub(super) fn call_argument_value_end_infos(
        &self,
        tokens: &[SyntaxTokenInfo],
        value_start: usize,
    ) -> usize {
        self.collector
            .call_argument_value_end_infos(tokens, value_start)
    }

    pub(super) fn syntax_token_nodes(&self, node: NodeId) -> Vec<SyntaxTokenInfo> {
        self.collector.syntax_token_nodes(node)
    }

    pub(super) fn syntax_tokens_have_space_between(
        &self,
        left: &SyntaxTokenInfo,
        right: &SyntaxTokenInfo,
    ) -> bool {
        self.collector.syntax_tokens_have_space_between(left, right)
    }

    pub(super) fn find_matching_group_end_infos(
        &self,
        tokens: &[SyntaxTokenInfo],
        start_idx: usize,
        open_text: &str,
        close_text: &str,
    ) -> Option<usize> {
        self.collector
            .find_matching_group_end_infos(tokens, start_idx, open_text, close_text)
    }

    pub(super) fn selector_access_chain(
        &self,
        node: NodeId,
    ) -> Option<(
        Namespace,
        std::sync::Arc<str>,
        abap_lexer::TextRange,
        Vec<crate::def_map::FieldAccessSegment>,
    )> {
        self.collector.selector_access_chain(node)
    }

    pub(super) fn consume_selector_access_from_infos(
        &self,
        tokens: &[SyntaxTokenInfo],
        idx: usize,
    ) -> Option<(
        usize,
        Namespace,
        std::sync::Arc<str>,
        abap_lexer::TextRange,
        Vec<crate::def_map::FieldAccessSegment>,
        Vec<(usize, usize, bool)>,
    )> {
        self.collector
            .consume_selector_access_from_infos(tokens, idx)
    }

    pub(super) fn lookup_symbol_in_scope_chain(
        &self,
        scope: ScopeId,
        namespace: Namespace,
        name: &str,
    ) -> Option<crate::ids::SymbolId> {
        self.collector
            .lookup_symbol_in_scope_chain(scope, namespace, name)
    }

    pub(super) fn symbol_declared_type(
        &self,
        symbol_id: crate::ids::SymbolId,
    ) -> Option<FieldTypeRefData> {
        self.collector.symbol(symbol_id).declared_type.clone()
    }

    pub(super) fn symbol_structure(&self, symbol_id: crate::ids::SymbolId) -> Option<StructureId> {
        self.collector.symbol(symbol_id).structure
    }

    pub(super) fn named_argument_target_for_callee(
        &self,
        callee: NodeId,
    ) -> Option<NamedArgumentTarget> {
        self.collector.named_argument_target_for_callee(callee)
    }

    pub(super) fn control_lowering(&mut self) -> super::control::ControlLowering<'_, 'a> {
        self.collector.control_lowering()
    }

    pub(super) fn declaration_scope(&self, scope: ScopeId) -> ScopeId {
        self.collector.declaration_scope(scope)
    }

    pub(super) fn push_scope(
        &mut self,
        kind: crate::scope::ScopeKind,
        range: abap_lexer::TextRange,
        parent: Option<ScopeId>,
        owner: Option<crate::ids::SymbolId>,
    ) -> ScopeId {
        self.collector.push_scope(kind, range, parent, owner)
    }

    pub(super) fn declare_symbol(
        &mut self,
        scope: ScopeId,
        name: std::sync::Arc<str>,
        kind: SymbolKind,
        decl_range: abap_lexer::TextRange,
        structure: Option<StructureId>,
        declared_type: Option<FieldTypeRefData>,
        type_clause_display: Option<std::sync::Arc<str>>,
        value_clause_display: Option<std::sync::Arc<str>>,
    ) -> crate::ids::SymbolId {
        self.collector.declare_symbol(
            scope,
            name,
            kind,
            decl_range,
            structure,
            declared_type,
            type_clause_display,
            value_clause_display,
        )
    }
}

pub(super) struct DeclContext<'ctx, 'a> {
    collector: &'ctx mut Collector<'a>,
}

impl<'ctx, 'a> DeclContext<'ctx, 'a> {
    pub(super) fn new(collector: &'ctx mut Collector<'a>) -> Self {
        Self { collector }
    }

    pub(super) fn source(&self) -> &'a str {
        self.collector.source
    }

    pub(super) fn file(&self) -> &'a File {
        self.collector.file
    }

    pub(super) fn syntax(&self, node: NodeId) -> SyntaxNodeRef<'_> {
        self.collector.syntax(node)
    }

    pub(super) fn walk_children(&mut self, node: NodeId, scope: ScopeId) {
        self.collector.walk_children(node, scope);
    }

    pub(super) fn walk_node(&mut self, node: NodeId, scope: ScopeId) {
        self.collector.walk_node(node, scope);
    }

    pub(super) fn header_ident_after_keyword(
        &self,
        node: NodeId,
    ) -> Option<(std::sync::Arc<str>, abap_lexer::TextRange)> {
        self.collector.header_ident_after_keyword(node)
    }

    pub(super) fn event_block_header_name(
        &self,
        node: NodeId,
    ) -> Option<(std::sync::Arc<str>, abap_lexer::TextRange)> {
        self.collector.event_block_header_name(node)
    }

    pub(super) fn declare_plain_symbol(
        &mut self,
        scope: ScopeId,
        name: std::sync::Arc<str>,
        kind: SymbolKind,
        decl_range: abap_lexer::TextRange,
    ) -> crate::ids::SymbolId {
        self.collector
            .declare_plain_symbol(scope, name, kind, decl_range)
    }

    pub(super) fn declare_symbol(
        &mut self,
        scope: ScopeId,
        name: std::sync::Arc<str>,
        kind: SymbolKind,
        decl_range: abap_lexer::TextRange,
        structure: Option<StructureId>,
        declared_type: Option<FieldTypeRefData>,
        type_clause_display: Option<std::sync::Arc<str>>,
        value_clause_display: Option<std::sync::Arc<str>>,
    ) -> crate::ids::SymbolId {
        self.collector.declare_symbol(
            scope,
            name,
            kind,
            decl_range,
            structure,
            declared_type,
            type_clause_display,
            value_clause_display,
        )
    }

    pub(super) fn declaration_scope(&self, scope: ScopeId) -> ScopeId {
        self.collector.declaration_scope(scope)
    }

    pub(super) fn include_edges_mut(&mut self) -> &mut Vec<IncludeEdge> {
        &mut self.collector.include_edges
    }

    pub(super) fn add_reference(
        &mut self,
        scope: ScopeId,
        name: std::sync::Arc<str>,
        namespace: Namespace,
        kind: ReferenceKind,
        range: abap_lexer::TextRange,
    ) {
        self.collector
            .add_reference(scope, name, namespace, kind, range);
    }

    pub(super) fn push_scope(
        &mut self,
        kind: crate::scope::ScopeKind,
        range: abap_lexer::TextRange,
        parent: Option<ScopeId>,
        owner: Option<crate::ids::SymbolId>,
    ) -> ScopeId {
        self.collector.push_scope(kind, range, parent, owner)
    }

    pub(super) fn forms_lowering(&mut self) -> super::forms::FormsLowering<'_, 'a> {
        self.collector.forms_lowering()
    }

    pub(super) fn emit_form_routine(&mut self, routine: FormRoutineData) {
        self.collector.emit_form_routine(routine);
    }

    pub(super) fn class_lowering(&mut self) -> super::class::ClassLowering<'_, 'a> {
        self.collector.class_lowering()
    }

    pub(super) fn node_name(
        &self,
        node: NodeId,
    ) -> Option<(std::sync::Arc<str>, abap_lexer::TextRange)> {
        self.collector.node_name(node)
    }

    pub(super) fn begin_of_clause_parts(
        &self,
        node: NodeId,
        scope: ScopeId,
    ) -> Option<(
        std::sync::Arc<str>,
        abap_lexer::TextRange,
        Vec<super::PendingStructureMember>,
    )> {
        self.collector.begin_of_clause_parts(node, scope)
    }

    pub(super) fn register_structure(
        &mut self,
        scope: ScopeId,
        structure: PendingStructure,
    ) -> StructureId {
        self.collector.register_structure(scope, structure)
    }

    pub(super) fn structure_from_typed_clause(
        &self,
        node: NodeId,
        scope: ScopeId,
    ) -> Option<StructureId> {
        self.collector.structure_from_typed_clause(node, scope)
    }

    pub(super) fn type_ref_from_typed_clause(&self, node: NodeId) -> Option<FieldTypeRefData> {
        self.collector.type_ref_from_typed_clause(node)
    }

    pub(super) fn type_clause_display_from_typed_clause(
        &self,
        node: NodeId,
    ) -> Option<std::sync::Arc<str>> {
        self.collector.type_clause_display_from_typed_clause(node)
    }

    pub(super) fn value_clause_display_from_typed_clause(
        &self,
        node: NodeId,
    ) -> Option<std::sync::Arc<str>> {
        self.collector.value_clause_display_from_typed_clause(node)
    }

    pub(super) fn inline_decl_inferred_type(
        &self,
        node: NodeId,
        scope: ScopeId,
    ) -> (Option<StructureId>, Option<FieldTypeRefData>) {
        self.collector.inline_decl_inferred_type(node, scope)
    }

    pub(super) fn type_clause_ns_stack_mut(&mut self) -> &mut Vec<Namespace> {
        &mut self.collector.type_clause_ns_stack
    }

    pub(super) fn namespace_from_type_clause_kind(
        &self,
        kind: abap_ast::ast::TypeClauseKind,
    ) -> Namespace {
        self.collector.namespace_from_type_clause_kind(kind)
    }

    pub(super) fn typed_clause_namespace_hint(&self, node: NodeId) -> Option<Namespace> {
        self.collector.typed_clause_namespace_hint(node)
    }

    pub(super) fn type_ref_access_chain(
        &self,
        node: NodeId,
        namespace: Namespace,
    ) -> Option<(
        Namespace,
        bool,
        std::sync::Arc<str>,
        abap_lexer::TextRange,
        Vec<crate::def_map::FieldAccessSegment>,
    )> {
        self.collector.type_ref_access_chain(node, namespace)
    }

    pub(super) fn emit_field_access(&mut self, access: FieldAccess) {
        self.collector.emit_field_access(access);
    }

    pub(super) fn significant_stmt_token_infos(&self, node: NodeId) -> Vec<SyntaxTokenInfo> {
        self.collector.significant_stmt_token_infos(node)
    }

    pub(super) fn syntax_token_is_ident_like(&self, token: &SyntaxTokenInfo) -> bool {
        self.collector.syntax_token_is_ident_like(token)
    }
}

pub(super) struct SqlContext<'ctx, 'a> {
    collector: &'ctx mut Collector<'a>,
}

impl<'ctx, 'a> SqlContext<'ctx, 'a> {
    pub(super) fn new(collector: &'ctx mut Collector<'a>) -> Self {
        Self { collector }
    }

    pub(super) fn source(&self) -> &'a str {
        self.collector.source
    }

    pub(super) fn file(&self) -> &'a File {
        self.collector.file
    }

    pub(super) fn syntax(&self, node: NodeId) -> SyntaxNodeRef<'_> {
        self.collector.syntax(node)
    }

    pub(super) fn walk_children(&mut self, node: NodeId, scope: ScopeId) {
        self.collector.walk_children(node, scope);
    }

    pub(super) fn walk_node(&mut self, node: NodeId, scope: ScopeId) {
        self.collector.walk_node(node, scope);
    }

    pub(super) fn control_lowering(&mut self) -> super::control::ControlLowering<'_, 'a> {
        self.collector.control_lowering()
    }

    pub(super) fn push_scope(
        &mut self,
        kind: crate::scope::ScopeKind,
        range: abap_lexer::TextRange,
        parent: Option<ScopeId>,
        owner: Option<crate::ids::SymbolId>,
    ) -> ScopeId {
        self.collector.push_scope(kind, range, parent, owner)
    }

    pub(super) fn emit_sql_query(&mut self, query: SqlQueryData) {
        self.collector.emit_sql_query(query);
    }

    pub(super) fn emit_sql_projection(&mut self, projection: SqlProjectionData) {
        self.collector.emit_sql_projection(projection);
    }

    pub(super) fn emit_sql_source(&mut self, source: SqlSourceData) {
        self.collector.emit_sql_source(source);
    }

    pub(super) fn emit_sql_target(&mut self, target: SqlTargetData) {
        self.collector.emit_sql_target(target);
    }

    pub(super) fn emit_sql_predicate(&mut self, predicate: SqlPredicateData) {
        self.collector.emit_sql_predicate(predicate);
    }

    pub(super) fn emit_sql_name_ref(&mut self, name_ref: SqlNameRefData) {
        self.collector.emit_sql_name_ref(name_ref);
    }

    pub(super) fn sql_queries_len(&self) -> usize {
        self.collector.sql_queries.len()
    }

    pub(super) fn sql_projections_for_query(&self, query_id: usize) -> Vec<SqlProjectionData> {
        self.collector
            .sql_projections
            .iter()
            .filter(|projection| projection.query_id == query_id)
            .cloned()
            .collect()
    }

    pub(super) fn sql_sources_for_query(&self, query_id: usize) -> Vec<SqlSourceData> {
        self.collector
            .sql_sources
            .iter()
            .filter(|source| source.query_id == query_id)
            .cloned()
            .collect()
    }

    pub(super) fn structure_field(
        &self,
        structure_id: StructureId,
        field_name: &str,
    ) -> Option<StructureFieldData> {
        self.collector
            .structure(structure_id)
            .and_then(|structure| {
                structure
                    .fields
                    .iter()
                    .find(|field| field.name.as_ref() == field_name)
            })
            .cloned()
    }

    pub(super) fn node_name(
        &self,
        node: NodeId,
    ) -> Option<(std::sync::Arc<str>, abap_lexer::TextRange)> {
        self.collector.node_name(node)
    }

    pub(super) fn syntax_token_nodes(&self, node: NodeId) -> Vec<SyntaxTokenInfo> {
        self.collector.syntax_token_nodes(node)
    }

    pub(super) fn sql_target_name_from_expr(&self, node: NodeId) -> Option<std::sync::Arc<str>> {
        self.collector.sql_target_name_from_expr(node)
    }

    pub(super) fn expr_lowering(&mut self) -> super::exprs::ExprLowering<'_, 'a> {
        self.collector.expr_lowering()
    }

    pub(super) fn decl_lowering(&mut self) -> DeclLowering<'_, 'a> {
        self.collector.decl_lowering()
    }

    pub(super) fn register_structure(
        &mut self,
        scope: ScopeId,
        structure: super::PendingStructure,
    ) -> StructureId {
        self.collector.register_structure(scope, structure)
    }

    pub(super) fn collect_token_expression_refs_infos(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
        in_parens: bool,
    ) {
        self.collector
            .collect_token_expression_refs_infos(tokens, scope, in_parens);
    }

    pub(super) fn syntax_token_is_literal_like(&self, token: &SyntaxTokenInfo) -> bool {
        self.collector.syntax_token_is_literal_like(token)
    }

    pub(super) fn syntax_token_is_comment(&self, token: &SyntaxTokenInfo) -> bool {
        self.collector.syntax_token_is_comment(token)
    }

    pub(super) fn count_kind(&self, node: NodeId, kind: abap_ast::SyntaxKind) -> usize {
        self.collector.file.count_kind(node, kind)
    }

    pub(super) fn lookup_symbol_in_scope_chain(
        &self,
        scope: ScopeId,
        namespace: Namespace,
        name: &str,
    ) -> Option<crate::ids::SymbolId> {
        self.collector
            .lookup_symbol_in_scope_chain(scope, namespace, name)
    }

    pub(super) fn symbol_structure(&self, symbol_id: crate::ids::SymbolId) -> Option<StructureId> {
        self.collector.symbol(symbol_id).structure
    }

    pub(super) fn add_reference(
        &mut self,
        scope: ScopeId,
        name: std::sync::Arc<str>,
        namespace: Namespace,
        kind: ReferenceKind,
        range: abap_lexer::TextRange,
    ) {
        self.collector
            .add_reference(scope, name, namespace, kind, range);
    }
}
