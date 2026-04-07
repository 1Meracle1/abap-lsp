use std::ops::{Deref, DerefMut};
use std::sync::Arc;

use abap_ast::SyntaxKind;
use abap_ast::arena::NodeId;

use crate::def_map::{FieldAccess, FieldAccessSegment, FieldTypeRefData, SymbolKind};
use crate::ids::{ScopeId, StructureId, SymbolId};
use crate::scope::{Namespace, ScopeKind};

use super::Collector;
use super::emit::RefSink;

pub(super) struct ControlLowering<'ctx, 'a> {
    collector: &'ctx mut Collector<'a>,
}

impl<'ctx, 'a> Deref for ControlLowering<'ctx, 'a> {
    type Target = Collector<'a>;

    fn deref(&self) -> &Self::Target {
        self.collector
    }
}

impl<'ctx, 'a> DerefMut for ControlLowering<'ctx, 'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.collector
    }
}

impl<'a> Collector<'a> {
    pub(super) fn control_lowering(&mut self) -> ControlLowering<'_, 'a> {
        ControlLowering { collector: self }
    }
}

impl<'ctx, 'a> ControlLowering<'ctx, 'a> {
    pub(super) fn walk_if_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let node_range = self.file.range(node);
        let branch_scope = self.push_scope(ScopeKind::IfBranch, node_range, Some(scope), None);
        for child in self.file.children(node) {
            match self.file.kind(child) {
                SyntaxKind::ElseifClause | SyntaxKind::ElseClause => self.walk_node(child, scope),
                _ => self.walk_node(child, branch_scope),
            }
        }
    }

    pub(super) fn walk_loop_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let node_range = self.file.range(node);
        let child_scope = self.push_scope(ScopeKind::LoopBlock, node_range, Some(scope), None);
        self.collect_loop_header_node(node, child_scope);
        for child in self.file.children(node) {
            match self.file.kind(child) {
                SyntaxKind::LoopSourceClause
                | SyntaxKind::LoopIntoClause
                | SyntaxKind::LoopAssigningClause
                | SyntaxKind::LoopReferenceIntoClause
                | SyntaxKind::LoopWhereClause
                | SyntaxKind::LoopFromClause
                | SyntaxKind::LoopToClause
                | SyntaxKind::LoopStepClause
                | SyntaxKind::Token => {}
                _ => self.walk_node(child, child_scope),
            }
        }
    }

    pub(super) fn walk_nested_block(&mut self, node: NodeId, scope: ScopeId, kind: ScopeKind) {
        let node_range = self.file.range(node);
        let child_scope = self.push_scope(kind, node_range, Some(scope), None);
        for child in self.file.children(node) {
            self.walk_node(child, child_scope);
        }
    }

    pub(super) fn select_stmt_has_endselect(&self, node: NodeId) -> bool {
        self.file.children(node).any(|child| {
            self.file.kind(child) == SyntaxKind::Token
                && self
                    .syntax(child)
                    .text(self.source)
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
                .structure(structure_id)
                .is_some_and(|structure| structure.fields.len() == 1),
        }
    }

    fn collect_loop_header_node(&mut self, node: NodeId, scope: ScopeId) {
        let mut source_metadata = (None, None);
        let mut allows_internal_table_line_selector = false;
        for child in self.file.children(node) {
            match self.file.kind(child) {
                SyntaxKind::LoopSourceClause => {
                    if let Some(expr) = self.first_non_token_child(child) {
                        allows_internal_table_line_selector =
                            self.internal_table_line_selector_allowed_for_source(expr, scope);
                        self.expr_lowering().collect_expr(expr, scope);
                        source_metadata = self.loop_source_line_metadata_from_node(expr, scope);
                    }
                }
                SyntaxKind::LoopIntoClause => {
                    if let Some(target) = self.first_non_token_child(child) {
                        self.collect_loop_target_node(
                            target,
                            scope,
                            SymbolKind::Variable,
                            &source_metadata,
                        );
                    }
                }
                SyntaxKind::LoopAssigningClause => {
                    if let Some(target) = self.first_non_token_child(child) {
                        self.collect_loop_target_node(
                            target,
                            scope,
                            SymbolKind::FieldSymbol,
                            &source_metadata,
                        );
                    }
                }
                SyntaxKind::LoopReferenceIntoClause => {
                    if let Some(target) = self.last_non_token_child(child) {
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
                    if let Some(expr) = self.first_non_token_child(child) {
                        self.expr_lowering().collect_expr(expr, scope);
                    }
                }
                _ => {}
            }
        }
        self.scopes[scope.as_usize()].allows_internal_table_line_selector =
            allows_internal_table_line_selector;
    }

    fn collect_loop_target_node(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        symbol_kind: SymbolKind,
        inferred_metadata: &(Option<StructureId>, Option<FieldTypeRefData>),
    ) {
        match self.file.kind(node) {
            SyntaxKind::DataInlineDecl if symbol_kind == SymbolKind::Variable => {
                if let Some(name_node) = self
                    .file
                    .children(node)
                    .find(|&child| self.file.kind(child) == SyntaxKind::DataDeclName)
                    && let Some((name, range)) = self.node_name(name_node)
                {
                    self.declare_symbol(
                        scope,
                        name,
                        SymbolKind::Variable,
                        range,
                        inferred_metadata.0,
                        inferred_metadata.1.clone(),
                        None,
                    );
                }
            }
            SyntaxKind::FieldSymbolInlineDecl if symbol_kind == SymbolKind::FieldSymbol => {
                self.decl_lowering().declare_inline_field_symbol_decl(
                    node,
                    scope,
                    inferred_metadata.0,
                    inferred_metadata.1.clone(),
                );
            }
            _ => self.expr_lowering().collect_expr(node, scope),
        }
    }

    pub(super) fn loop_source_line_metadata_from_node(
        &self,
        node: NodeId,
        scope: ScopeId,
    ) -> (Option<StructureId>, Option<FieldTypeRefData>) {
        match self.file.kind(node) {
            SyntaxKind::TemplateExpr => self
                .first_non_token_child(node)
                .map(|child| self.loop_source_line_metadata_from_node(child, scope))
                .unwrap_or((None, None)),
            SyntaxKind::ExprIdent => {
                let Some((name, _)) = self.node_name(node) else {
                    return (None, None);
                };
                let Some(symbol_id) =
                    self.lookup_symbol_in_scope_chain(scope, Namespace::Value, name.as_ref())
                else {
                    return (None, None);
                };
                let symbol = self.symbol(symbol_id);
                self.normalize_inferred_metadata(
                    scope,
                    symbol.structure,
                    symbol.declared_type.clone(),
                )
            }
            SyntaxKind::SelectorExpr => {
                let Some((namespace, base_name, _, field_path)) = self.selector_access_chain(node)
                else {
                    return (None, None);
                };
                if namespace != Namespace::Value {
                    return (None, None);
                }
                let Some(symbol_id) =
                    self.lookup_symbol_in_scope_chain(scope, Namespace::Value, base_name.as_ref())
                else {
                    return (None, None);
                };
                if field_path.is_empty() {
                    let symbol = self.symbol(symbol_id);
                    return self.normalize_inferred_metadata(
                        scope,
                        symbol.structure,
                        symbol.declared_type.clone(),
                    );
                }
                self.loop_source_field_metadata(scope, symbol_id, &field_path)
                    .map(|(structure, declared_type)| {
                        self.normalize_inferred_metadata(scope, structure, declared_type)
                    })
                    .unwrap_or((None, None))
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
        let mut structure = self.symbol(symbol_id).structure;
        let mut declared_type = self.symbol(symbol_id).declared_type.clone();
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
            self.lookup_symbol_in_scope_chain(scope, Namespace::Type, type_ref.base_name.as_ref())
                .and_then(|symbol_id| self.symbol(symbol_id).structure)
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
            let Some(symbol_id) = self.lookup_symbol_in_scope_chain(
                scope,
                Namespace::Type,
                type_ref.base_name.as_ref(),
            ) else {
                break;
            };
            let symbol = self.symbol(symbol_id);
            if symbol.structure.is_none() && symbol.declared_type.is_none() {
                break;
            }
            structure = symbol.structure;
            declared_type = symbol.declared_type.clone();
        }
        (structure, declared_type)
    }

    pub(super) fn collect_sort_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let children: Vec<NodeId> = self.file.children(node).collect();
        let by_idx = children.iter().position(|&c| {
            self.file.kind(c) == SyntaxKind::Token
                && self
                    .syntax(c)
                    .text(self.source)
                    .is_some_and(|text| text.eq_ignore_ascii_case("by"))
        });
        let Some(by_idx) = by_idx else {
            self.walk_children(node, scope);
            return;
        };

        let mut itab_base = None;
        for &tmpl in children[..by_idx].iter() {
            if self.file.kind(tmpl) == SyntaxKind::TemplateExpr {
                itab_base = self
                    .file
                    .children(tmpl)
                    .next()
                    .and_then(|inner| self.sql_target_name_from_expr(inner));
                break;
            }
        }

        for &child in &children[..by_idx] {
            self.walk_node(child, scope);
        }

        let Some(itab_base) = itab_base else {
            for &child in &children[by_idx + 1..] {
                self.walk_node(child, scope);
            }
            return;
        };

        for &child in &children[by_idx + 1..] {
            if self.file.kind(child) == SyntaxKind::TemplateExpr {
                let Some(inner) = self.file.children(child).next() else {
                    self.walk_node(child, scope);
                    continue;
                };
                if let Some(field_path) = self.sort_by_field_segments_from_expr(inner) {
                    self.emit_field_access(FieldAccess {
                        scope,
                        base_namespace: Namespace::Value,
                        base_name: Arc::clone(&itab_base),
                        field_path,
                        in_type_position: false,
                    });
                    continue;
                }
            }
            self.walk_node(child, scope);
        }
    }

    fn sort_by_field_segments_from_expr(&self, inner: NodeId) -> Option<Vec<FieldAccessSegment>> {
        match self.file.kind(inner) {
            SyntaxKind::ExprIdent => {
                let (name, range) = self.node_name(inner)?;
                Some(vec![FieldAccessSegment { name, range }])
            }
            SyntaxKind::SelectorExpr => {
                let (namespace, base_name, base_range, mut path) =
                    self.selector_access_chain(inner)?;
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
}
