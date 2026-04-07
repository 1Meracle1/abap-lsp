use std::ops::{Deref, DerefMut};
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

use super::emit::{FormSink, RefSink};
use super::{Collector, PendingStructure};

pub(super) struct DeclLowering<'ctx, 'a> {
    collector: &'ctx mut Collector<'a>,
}

impl<'ctx, 'a> Deref for DeclLowering<'ctx, 'a> {
    type Target = Collector<'a>;

    fn deref(&self) -> &Self::Target {
        self.collector
    }
}

impl<'ctx, 'a> DerefMut for DeclLowering<'ctx, 'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.collector
    }
}

impl<'a> Collector<'a> {
    pub(super) fn decl_lowering(&mut self) -> DeclLowering<'_, 'a> {
        DeclLowering { collector: self }
    }
}

impl<'ctx, 'a> DeclLowering<'ctx, 'a> {
    pub(super) fn walk_include_stmt(&mut self, node: abap_ast::arena::NodeId, scope: ScopeId) {
        if let Some((name, range)) = self.header_ident_after_keyword(node) {
            self.declare_plain_symbol(scope, Arc::clone(&name), SymbolKind::Include, range.clone());
            self.include_edges.push(IncludeEdge {
                name: Arc::clone(&name),
                range: range.clone(),
                target: None,
            });
            self.add_reference(scope, name, Namespace::Value, ReferenceKind::Include, range);
        }
    }

    pub(super) fn walk_named_header_decl(
        &mut self,
        node: abap_ast::arena::NodeId,
        scope: ScopeId,
        kind: SymbolKind,
        fallback_scope_kind: ScopeKind,
    ) {
        if let Some((name, range)) = self.header_ident_after_keyword(node) {
            let owner = self.declare_plain_symbol(scope, name, kind, range);
            let block_scope = if matches!(
                kind,
                SymbolKind::Form | SymbolKind::Module | SymbolKind::Event
            ) {
                let node_range = self.file.range(node);
                self.push_scope(fallback_scope_kind, node_range, Some(scope), Some(owner))
            } else {
                scope
            };
            for child in self.file.children(node) {
                self.walk_node(child, block_scope);
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
        let Some((name, range)) = self.header_ident_after_keyword(node) else {
            self.walk_children(node, scope);
            return;
        };
        let owner = self.declare_plain_symbol(scope, name, kind, range);
        let node_range = self.file.range(node);
        let child_scope = self.push_scope(scope_kind, node_range, Some(scope), Some(owner));
        if scope_kind == ScopeKind::Form {
            let parameters = self
                .forms_lowering()
                .declare_form_parameters_from_header(node, child_scope);
            self.emit_form_routine(FormRoutineData {
                symbol: owner,
                parameters,
            });
        }
        for child in self.file.children(node) {
            self.walk_node(child, child_scope);
        }
    }

    pub(super) fn walk_method_decl(&mut self, node: abap_ast::arena::NodeId, scope: ScopeId) {
        let Some((name, range)) = self.header_ident_after_keyword(node) else {
            self.walk_children(node, scope);
            return;
        };
        let owner =
            self.declare_plain_symbol(scope, Arc::clone(&name), SymbolKind::Method, range.clone());
        let node_range = self.file.range(node);
        let child_scope = self.push_scope(ScopeKind::Method, node_range, Some(scope), Some(owner));
        if let Some(class_symbol) = self.class_lowering().enclosing_class_owner(scope) {
            self.class_lowering().declare_method_signature_parameters(
                class_symbol,
                name.as_ref(),
                child_scope,
                scope,
            );
            self.class_lowering().declare_implicit_me_symbol(
                class_symbol,
                name.as_ref(),
                child_scope,
                &range,
            );
        }
        for child in self.file.children(node) {
            self.walk_node(child, child_scope);
        }
    }

    pub(super) fn walk_event_block(&mut self, node: abap_ast::arena::NodeId, scope: ScopeId) {
        let Some((name, range)) = self.event_block_header_name(node) else {
            self.walk_children(node, scope);
            return;
        };
        let owner = self.declare_plain_symbol(scope, name, SymbolKind::Event, range);
        let node_range = self.file.range(node);
        let child_scope =
            self.push_scope(ScopeKind::EventBlock, node_range, Some(scope), Some(owner));
        for child in self.file.children(node) {
            self.walk_node(child, child_scope);
        }
    }

    pub(super) fn walk_data_like_decl(
        &mut self,
        node: abap_ast::arena::NodeId,
        scope: ScopeId,
        kind: SymbolKind,
    ) {
        if let Some(data_decl) = DataDecl::cast(self.syntax(node)) {
            let clauses = data_decl
                .clauses()
                .map(|clause| {
                    let child_id = clause.syntax().id();
                    let hint = clause
                        .type_clause_kind(self.source)
                        .map(|kind| self.namespace_from_type_clause_kind(kind));
                    (child_id, hint)
                })
                .collect::<Vec<_>>();
            for (child_id, hint) in clauses {
                if let Some(ns) = hint {
                    self.type_clause_ns_stack.push(ns);
                }
                self.declare_decl_clause_symbol(child_id, scope, kind);
                self.walk_children(child_id, scope);
                if hint.is_some() {
                    self.type_clause_ns_stack.pop();
                }
            }
            return;
        }
        let Some(decl) = DataLikeDecl::cast(self.syntax(node)) else {
            self.walk_children(node, scope);
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
                        let hint = self.typed_clause_namespace_hint(child_id);
                        if let Some(ns) = hint {
                            self.type_clause_ns_stack.push(ns);
                        }
                        self.declare_decl_clause_symbol(child_id, scope, kind);
                        self.walk_children(child_id, scope);
                        if hint.is_some() {
                            self.type_clause_ns_stack.pop();
                        }
                    }
                    SyntaxKind::StructuredDecl => {
                        self.declare_structured_decl_symbol(child_id, scope, kind);
                        self.walk_children(child_id, scope);
                    }
                    _ => self.walk_node(child_id, scope),
                }
            } else {
                self.walk_node(child_id, scope);
            }
        }
    }

    pub(super) fn declare_decl_clause_symbol(
        &mut self,
        node: abap_ast::arena::NodeId,
        scope: ScopeId,
        kind: SymbolKind,
    ) {
        if let Some((name, range, members)) = self.begin_of_clause_parts(node, scope) {
            let structure = self.register_structure(
                scope,
                PendingStructure {
                    name: std::sync::Arc::clone(&name),
                    members,
                },
            );
            self.declare_symbol(scope, name, kind, range, Some(structure), None, None);
            return;
        }

        if let Some(clause) = DeclClause::cast(self.syntax(node))
            && let Some(name_node) = clause.name()
            && let Some(name) = name_node.name(self.source)
        {
            let range = name_node.range();
            let structure = self.structure_from_typed_clause(node, scope);
            let declared_type = self.type_ref_from_typed_clause(node);
            let type_clause_display = self.type_clause_display_from_typed_clause(node);
            self.declare_symbol(
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
        if let Some((name, range, members)) = self.begin_of_clause_parts(node, scope) {
            let structure = self.register_structure(
                scope,
                PendingStructure {
                    name: std::sync::Arc::clone(&name),
                    members,
                },
            );
            self.declare_symbol(scope, name, kind, range, Some(structure), None, None);
        }
    }

    pub(super) fn walk_inline_decl(&mut self, node: abap_ast::arena::NodeId, scope: ScopeId) {
        let (structure, declared_type) = self.inline_decl_inferred_type(node, scope);
        for child in self.file.children(node) {
            if self.file.kind(child) == SyntaxKind::DataDeclName
                && let Some((name, range)) = self.node_name(child)
            {
                self.declare_symbol(
                    scope,
                    name,
                    SymbolKind::Variable,
                    range,
                    structure,
                    declared_type.clone(),
                    None,
                );
            }
        }
        self.walk_children(node, scope);
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
        for child in self.file.children(node) {
            if self.file.kind(child) == SyntaxKind::DataDeclName
                && let Some((name, range)) = self.node_name(child)
            {
                self.declare_symbol(
                    scope,
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
            .type_clause_ns_stack
            .last()
            .copied()
            .unwrap_or(Namespace::Type);
        if let Some((namespace, _, base_name, range, field_path)) =
            self.type_ref_access_chain(node, simple_ns)
        {
            self.add_reference(
                scope,
                std::sync::Arc::clone(&base_name),
                namespace,
                ReferenceKind::TypeRef,
                range,
            );
            if !field_path.is_empty() {
                self.emit_field_access(FieldAccess {
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
