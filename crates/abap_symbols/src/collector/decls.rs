use std::sync::Arc;

use crate::def_map::{
    FieldAccess, FieldTypeRefData, FormRoutineData, IncludeEdge, ReferenceKind, SymbolKind,
};
use crate::ids::{ScopeId, StructureId};
use crate::scope::{Namespace, ScopeKind};
use abap_ast::{
    SyntaxKind,
    ast::{AstNode, DataDecl, DataLikeDecl, DeclClause},
};
use abap_lexer::TextRange;

use super::context::DeclContext;
use super::{Collector, PendingStructure};

pub(super) struct DeclLowering<'ctx, 'a> {
    ctx: DeclContext<'ctx, 'a>,
}

impl<'a> Collector<'a> {
    pub(super) fn decl_lowering(&mut self) -> DeclLowering<'_, 'a> {
        DeclLowering {
            ctx: DeclContext::new(self),
        }
    }
}

impl<'ctx, 'a> DeclLowering<'ctx, 'a> {
    fn include_stmt_names(
        &self,
        node: abap_ast::arena::NodeId,
    ) -> Vec<(Arc<str>, TextRange)> {
        let tokens = self.ctx.significant_stmt_token_infos(node);
        let Some(first) = tokens.first() else {
            return Vec::new();
        };
        if !first.text.eq_ignore_ascii_case("include") {
            return Vec::new();
        }

        let mut names = Vec::new();
        let mut expect_name = true;
        for token in tokens.iter().skip(1) {
            match token.text.as_ref() {
                "." => break,
                ":" | "," => {
                    expect_name = true;
                }
                _ if expect_name && self.ctx.syntax_token_is_ident_like(token) => {
                    names.push((Arc::<str>::from(token.text.to_ascii_lowercase()), token.range.clone()));
                    expect_name = false;
                }
                _ => {}
            }
        }
        names
    }

    fn method_decl_header_name_parts(
        &self,
        node: abap_ast::arena::NodeId,
    ) -> Option<(Arc<str>, Option<Arc<str>>, Arc<str>, TextRange)> {
        let tokens = self.ctx.significant_stmt_token_infos(node);
        let method_tok = tokens.first()?;
        if !method_tok.text.eq_ignore_ascii_case("method") {
            return None;
        }
        let first = tokens.get(1)?;
        if !self.ctx.syntax_token_is_ident_like(first) {
            return None;
        }
        let mut full_name = first.text.to_string();
        let mut qualifier = None;
        let mut last_name = Arc::<str>::from(first.text.to_ascii_lowercase());
        let mut end = first.range.end;
        if tokens
            .get(2)
            .is_some_and(|token| token.text.as_ref() == "~")
            && let Some(second) = tokens.get(3)
            && self.ctx.syntax_token_is_ident_like(second)
        {
            full_name.push('~');
            full_name.push_str(second.text.as_ref());
            qualifier = Some(Arc::<str>::from(first.text.to_ascii_lowercase()));
            last_name = Arc::<str>::from(second.text.to_ascii_lowercase());
            end = second.range.end;
        }
        Some((
            Arc::<str>::from(full_name.to_ascii_lowercase()),
            qualifier,
            last_name,
            first.range.start..end,
        ))
    }

    pub(super) fn walk_include_stmt(&mut self, node: abap_ast::arena::NodeId, scope: ScopeId) {
        for (name, range) in self.include_stmt_names(node) {
            self.ctx.declare_plain_symbol(
                scope,
                Arc::clone(&name),
                SymbolKind::Include,
                range.clone(),
            );
            self.ctx.include_edges_mut().push(IncludeEdge {
                name: Arc::clone(&name),
                range: range.clone(),
                target: None,
            });
            self.ctx
                .add_reference(scope, name, Namespace::Value, ReferenceKind::Include, range);
        }
    }

    pub(super) fn walk_named_header_decl(
        &mut self,
        node: abap_ast::arena::NodeId,
        scope: ScopeId,
        kind: SymbolKind,
        fallback_scope_kind: ScopeKind,
    ) {
        if let Some((name, range)) = self.ctx.header_ident_after_keyword(node) {
            let owner = self.ctx.declare_plain_symbol(scope, name, kind, range);
            let block_scope = if matches!(
                kind,
                SymbolKind::Form | SymbolKind::Module | SymbolKind::Event
            ) {
                let node_range = self.ctx.file().range(node);
                self.ctx
                    .push_scope(fallback_scope_kind, node_range, Some(scope), Some(owner))
            } else {
                scope
            };
            for child in self.ctx.file().children(node) {
                self.ctx.walk_node(child, block_scope);
            }
        }
    }

    pub(super) fn walk_block_decl(
        &mut self,
        node: abap_ast::arena::NodeId,
        scope: ScopeId,
        kind: SymbolKind,
        scope_kind: ScopeKind,
    ) {
        let Some((name, range)) = self.ctx.header_ident_after_keyword(node) else {
            self.ctx.walk_children(node, scope);
            return;
        };
        let owner = self.ctx.declare_plain_symbol(scope, name, kind, range);
        let node_range = self.ctx.file().range(node);
        let child_scope = self
            .ctx
            .push_scope(scope_kind, node_range, Some(scope), Some(owner));
        if scope_kind == ScopeKind::Form {
            let parameters = self
                .ctx
                .forms_lowering()
                .declare_form_parameters_from_header(node, child_scope);
            self.ctx.emit_form_routine(FormRoutineData {
                symbol: owner,
                parameters,
            });
        }
        for child in self.ctx.file().children(node) {
            self.ctx.walk_node(child, child_scope);
        }
    }

    pub(super) fn walk_method_decl(&mut self, node: abap_ast::arena::NodeId, scope: ScopeId) {
        let Some((name, qualifier, member_name, range)) = self.method_decl_header_name_parts(node)
        else {
            self.ctx.walk_children(node, scope);
            return;
        };
        let owner = self.ctx.declare_plain_symbol(
            scope,
            Arc::clone(&name),
            SymbolKind::Method,
            range.clone(),
        );
        let node_range = self.ctx.file().range(node);
        let child_scope =
            self.ctx
                .push_scope(ScopeKind::Method, node_range, Some(scope), Some(owner));
        if let Some(class_symbol) = self.ctx.class_lowering().enclosing_class_owner(scope) {
            self.ctx
                .class_lowering()
                .note_method_implementation_target_range(
                    class_symbol,
                    qualifier.as_deref(),
                    member_name.as_ref(),
                    scope,
                    range.clone(),
                );
            self.ctx
                .class_lowering()
                .declare_method_target_signature_parameters(
                    class_symbol,
                    qualifier.as_deref(),
                    member_name.as_ref(),
                    child_scope,
                    scope,
                );
            self.ctx.class_lowering().declare_implicit_me_symbol(
                class_symbol,
                member_name.as_ref(),
                child_scope,
                &range,
            );
        }
        for child in self.ctx.file().children(node) {
            self.ctx.walk_node(child, child_scope);
        }
    }

    pub(super) fn walk_event_block(&mut self, node: abap_ast::arena::NodeId, scope: ScopeId) {
        let Some((name, range)) = self.ctx.event_block_header_name(node) else {
            self.ctx.walk_children(node, scope);
            return;
        };
        let owner = self
            .ctx
            .declare_plain_symbol(scope, name, SymbolKind::Event, range);
        let node_range = self.ctx.file().range(node);
        let child_scope =
            self.ctx
                .push_scope(ScopeKind::EventBlock, node_range, Some(scope), Some(owner));
        for child in self.ctx.file().children(node) {
            self.ctx.walk_node(child, child_scope);
        }
    }

    pub(super) fn walk_data_like_decl(
        &mut self,
        node: abap_ast::arena::NodeId,
        scope: ScopeId,
        kind: SymbolKind,
    ) {
        let decl_scope = self.ctx.declaration_scope(scope);
        if let Some(data_decl) = DataDecl::cast(self.ctx.syntax(node)) {
            let clauses = data_decl
                .clauses()
                .map(|clause| {
                    let child_id = clause.syntax().id();
                    let hint = clause
                        .type_clause_kind(self.ctx.source())
                        .map(|kind| self.ctx.namespace_from_type_clause_kind(kind));
                    (child_id, hint)
                })
                .collect::<Vec<_>>();
            for (child_id, hint) in clauses {
                if let Some(ns) = hint {
                    self.ctx.type_clause_ns_stack_mut().push(ns);
                }
                self.declare_decl_clause_symbol(child_id, decl_scope, kind);
                self.ctx.walk_children(child_id, scope);
                if hint.is_some() {
                    self.ctx.type_clause_ns_stack_mut().pop();
                }
            }
            return;
        }
        let Some(decl) = DataLikeDecl::cast(self.ctx.syntax(node)) else {
            self.ctx.walk_children(node, scope);
            return;
        };
        let children: Vec<_> = decl
            .syntax()
            .children()
            .map(|child| (child.id(), child.kind()))
            .collect();
        for (child_id, child_kind) in children {
            if abap_ast::ast::DeclClause::can_cast(child_kind) {
                match child_kind {
                    SyntaxKind::DataTypedClause
                    | SyntaxKind::TypesTypedClause
                    | SyntaxKind::ConstantClause
                    | SyntaxKind::FieldSymbolClause => {
                        let hint = self.ctx.typed_clause_namespace_hint(child_id);
                        if let Some(ns) = hint {
                            self.ctx.type_clause_ns_stack_mut().push(ns);
                        }
                        self.declare_decl_clause_symbol(child_id, decl_scope, kind);
                        self.ctx.walk_children(child_id, scope);
                        if hint.is_some() {
                            self.ctx.type_clause_ns_stack_mut().pop();
                        }
                    }
                    SyntaxKind::StructuredDecl => {
                        self.declare_structured_decl_symbol(child_id, decl_scope, kind);
                        self.ctx.walk_children(child_id, scope);
                    }
                    _ => self.ctx.walk_node(child_id, scope),
                }
            } else {
                self.ctx.walk_node(child_id, scope);
            }
        }
    }

    pub(super) fn declare_decl_clause_symbol(
        &mut self,
        node: abap_ast::arena::NodeId,
        scope: ScopeId,
        kind: SymbolKind,
    ) {
        if let Some((name, range, members)) = self.ctx.begin_of_clause_parts(node, scope) {
            let structure = self.ctx.register_structure(
                scope,
                PendingStructure {
                    name: std::sync::Arc::clone(&name),
                    members,
                },
            );
            self.ctx
                .declare_symbol(scope, name, kind, range, Some(structure), None, None);
            return;
        }

        if let Some(clause) = DeclClause::cast(self.ctx.syntax(node))
            && let Some(name_node) = clause.name()
            && let Some(name) = name_node.name(self.ctx.source())
        {
            let range = name_node.range();
            let structure = self.ctx.structure_from_typed_clause(node, scope);
            let declared_type = self.ctx.type_ref_from_typed_clause(node);
            let type_clause_display = self.ctx.type_clause_display_from_typed_clause(node);
            self.ctx.declare_symbol(
                scope,
                name,
                kind,
                range,
                structure,
                declared_type,
                type_clause_display,
            );
        }
    }

    pub(super) fn declare_structured_decl_symbol(
        &mut self,
        node: abap_ast::arena::NodeId,
        scope: ScopeId,
        kind: SymbolKind,
    ) {
        if let Some((name, range, members)) = self.ctx.begin_of_clause_parts(node, scope) {
            let structure = self.ctx.register_structure(
                scope,
                PendingStructure {
                    name: std::sync::Arc::clone(&name),
                    members,
                },
            );
            self.ctx
                .declare_symbol(scope, name, kind, range, Some(structure), None, None);
        }
    }

    pub(super) fn walk_inline_decl(&mut self, node: abap_ast::arena::NodeId, scope: ScopeId) {
        let decl_scope = self.ctx.declaration_scope(scope);
        let (structure, declared_type) = self.ctx.inline_decl_inferred_type(node, scope);
        for child in self.ctx.file().children(node) {
            if self.ctx.file().kind(child) == SyntaxKind::DataDeclName
                && let Some((name, range)) = self.ctx.node_name(child)
            {
                self.ctx.declare_symbol(
                    decl_scope,
                    name,
                    SymbolKind::Variable,
                    range,
                    structure,
                    declared_type.clone(),
                    None,
                );
            }
        }
        self.ctx.walk_children(node, scope);
    }

    pub(super) fn walk_inline_field_symbol_decl(
        &mut self,
        node: abap_ast::arena::NodeId,
        scope: ScopeId,
    ) {
        self.declare_inline_field_symbol_decl(node, scope, None, None);
    }

    pub(super) fn declare_inline_field_symbol_decl(
        &mut self,
        node: abap_ast::arena::NodeId,
        scope: ScopeId,
        structure: Option<StructureId>,
        declared_type: Option<FieldTypeRefData>,
    ) {
        let decl_scope = self.ctx.declaration_scope(scope);
        for child in self.ctx.file().children(node) {
            if self.ctx.file().kind(child) == SyntaxKind::DataDeclName
                && let Some((name, range)) = self.ctx.node_name(child)
            {
                self.ctx.declare_symbol(
                    decl_scope,
                    name,
                    SymbolKind::FieldSymbol,
                    range,
                    structure,
                    declared_type.clone(),
                    None,
                );
                break;
            }
        }
    }

    pub(super) fn collect_type_ref(&mut self, node: abap_ast::arena::NodeId, scope: ScopeId) {
        let simple_ns = self
            .ctx
            .type_clause_ns_stack_mut()
            .last()
            .copied()
            .unwrap_or(Namespace::Type);
        if let Some((namespace, _, base_name, range, field_path)) =
            self.ctx.type_ref_access_chain(node, simple_ns)
        {
            self.ctx.add_reference(
                scope,
                std::sync::Arc::clone(&base_name),
                namespace,
                ReferenceKind::TypeRef,
                range,
            );
            if !field_path.is_empty() {
                self.ctx.emit_field_access(FieldAccess {
                    scope,
                    base_namespace: namespace,
                    base_name,
                    field_path,
                    in_type_position: true,
                });
            }
        }
    }
}
