use std::sync::Arc;

use crate::def_map::{
    AssignmentSiteData, FieldAccess, FieldAccessSegment, FieldTypeRefData, FormRoutineData,
    IncludeEdge, ReferenceKind, SymbolKind, TableWorkAreaData, TypeFactData,
};
use crate::ids::{ScopeId, StructureId};
use crate::scope::{Namespace, ScopeKind};
use abap_ast::{
    SyntaxKind,
    ast::{AstNode, DataDecl, DataLikeDecl, DeclClause, IncludeStmt, MethodDecl},
};
use abap_lexer::TextRange;

use super::context::DeclContext;
use super::{Collector, PendingStructure, SyntaxTokenInfo};

pub(super) struct DeclLowering<'ctx, 'a> {
    ctx: DeclContext<'ctx, 'a>,
}

struct MethodDeclHeaderInfo {
    full_name: Arc<str>,
    qualifier: Option<(Arc<str>, TextRange)>,
    member_name: Arc<str>,
    member_range: TextRange,
    full_range: TextRange,
}

impl<'a> Collector<'a> {
    pub(super) fn decl_lowering(&mut self) -> DeclLowering<'_, 'a> {
        DeclLowering {
            ctx: DeclContext::new(self),
        }
    }
}

impl<'ctx, 'a> DeclLowering<'ctx, 'a> {
    fn hyphenated_keyword_end(
        tokens: &[super::SyntaxTokenInfo],
        idx: usize,
        parts: &[&str],
    ) -> Option<usize> {
        let mut i = idx;
        for (part_idx, part) in parts.iter().enumerate() {
            let token = tokens.get(i)?;
            if !token.text.eq_ignore_ascii_case(part) {
                return None;
            }
            i += 1;
            if part_idx + 1 < parts.len() {
                let hyphen = tokens.get(i)?;
                if hyphen.text.as_ref() != "-" {
                    return None;
                }
                i += 1;
            }
        }
        Some(i)
    }

    fn checkbox_parameter_type_from_clause(
        &self,
        node: abap_ast::arena::NodeId,
    ) -> Option<FieldTypeRefData> {
        let clause = DeclClause::cast(self.ctx.syntax(node))?;
        if clause.type_clause_kind(self.ctx.source()).is_some() {
            return None;
        }

        let tokens = self
            .ctx
            .syntax_token_nodes(node)
            .into_iter()
            .filter(|token| !self.ctx.syntax_token_is_comment(token))
            .collect::<Vec<_>>();
        for idx in 0..tokens.len() {
            if tokens[idx].text.eq_ignore_ascii_case("as")
                && tokens
                    .get(idx + 1)
                    .is_some_and(|token| token.text.eq_ignore_ascii_case("checkbox"))
            {
                return Some(FieldTypeRefData {
                    namespace: Namespace::Type,
                    is_ref: false,
                    base_name: Arc::from("abap_bool"),
                    field_path: Vec::new(),
                });
            }
        }
        None
    }

    fn report_message_id_reference(
        &self,
        node: abap_ast::arena::NodeId,
    ) -> Option<(Arc<str>, TextRange)> {
        let tokens: Vec<_> = self
            .ctx
            .syntax_token_nodes(node)
            .into_iter()
            .filter(|token| !self.ctx.syntax_token_is_comment(token) && token.text.as_ref() != ".")
            .collect();
        let mut idx = 0usize;
        while idx < tokens.len() {
            let Some(next_idx) = (if tokens[idx].text.eq_ignore_ascii_case("message-id") {
                Some(idx + 1)
            } else {
                Self::hyphenated_keyword_end(&tokens, idx, &["message", "id"])
            }) else {
                idx += 1;
                continue;
            };
            return self
                .ctx
                .simple_type_ref_base_from_infos(&tokens[next_idx..]);
        }
        None
    }

    fn collect_select_options_dynamic_for_reference(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
    ) {
        for idx in 0..tokens.len() {
            if !tokens[idx].text.eq_ignore_ascii_case("for") {
                continue;
            }
            let Some(lparen) = tokens.get(idx + 1) else {
                continue;
            };
            let Some(name) = tokens.get(idx + 2) else {
                continue;
            };
            let Some(rparen) = tokens.get(idx + 3) else {
                continue;
            };
            if lparen.text.as_ref() != "("
                || rparen.text.as_ref() != ")"
                || !self.ctx.syntax_token_is_ident_like(name)
            {
                continue;
            }
            self.ctx.add_reference(
                scope,
                Arc::<str>::from(name.text.to_ascii_lowercase()),
                Namespace::Value,
                ReferenceKind::Identifier,
                name.range.clone(),
            );
            return;
        }
    }

    fn collect_select_options_matchcode_references(
        &mut self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
    ) {
        let mut idx = 0usize;
        while idx < tokens.len() {
            if !tokens[idx].text.eq_ignore_ascii_case("matchcode")
                || !tokens
                    .get(idx + 1)
                    .is_some_and(|token| token.text.eq_ignore_ascii_case("object"))
            {
                idx += 1;
                continue;
            }
            if let Some(search_help) = tokens.get(idx + 2)
                && self.ctx.syntax_token_is_ident_like(search_help)
            {
                self.ctx.add_reference(
                    scope,
                    Arc::<str>::from(search_help.text.to_ascii_lowercase()),
                    Namespace::Type,
                    ReferenceKind::TypeRef,
                    search_help.range.clone(),
                );
            }
            idx += 3;
        }
    }

    fn collect_select_options_clause_references(
        &mut self,
        node: abap_ast::arena::NodeId,
        scope: ScopeId,
    ) {
        let tokens = self
            .ctx
            .syntax_token_nodes(node)
            .into_iter()
            .filter(|token| !self.ctx.syntax_token_is_comment(token))
            .collect::<Vec<_>>();
        self.collect_select_options_dynamic_for_reference(&tokens, scope);
        self.collect_select_options_matchcode_references(&tokens, scope);
    }

    fn include_stmt_is_structured_include(&self, include_stmt: IncludeStmt<'_>) -> bool {
        let mut tokens = include_stmt.syntax().children_by_kind(SyntaxKind::Token);
        let Some(include_kw) = tokens.next() else {
            return false;
        };
        if !include_kw
            .text(self.ctx.source())
            .is_some_and(|text| text.eq_ignore_ascii_case("include"))
        {
            return false;
        }
        tokens.next().is_some_and(|token| {
            token.text(self.ctx.source()).is_some_and(|text| {
                text.eq_ignore_ascii_case("type") || text.eq_ignore_ascii_case("structure")
            })
        })
    }

    fn method_decl_header_name_parts(
        &self,
        node: abap_ast::arena::NodeId,
    ) -> Option<MethodDeclHeaderInfo> {
        let decl = MethodDecl::cast(self.ctx.syntax(node))?;
        let target = decl.target()?;
        let member = target.member_name()?;
        let member_name = member.name(self.ctx.source())?;
        let member_range = member.range();
        let qualifier = target.qualifier().map(|type_ref| {
            (
                Arc::<str>::from(
                    type_ref
                        .display_text(self.ctx.source())
                        .unwrap_or_default()
                        .to_ascii_lowercase(),
                ),
                type_ref.syntax().range(),
            )
        });
        let (full_name, full_range) = if let Some((qualifier_name, qualifier_range)) = &qualifier {
            (
                Arc::<str>::from(format!("{qualifier_name}~{member_name}")),
                qualifier_range.start..member_range.end,
            )
        } else {
            (Arc::clone(&member_name), member_range.clone())
        };
        Some(MethodDeclHeaderInfo {
            full_name,
            qualifier,
            member_name,
            member_range,
            full_range,
        })
    }

    pub(super) fn walk_include_stmt(&mut self, node: abap_ast::arena::NodeId, scope: ScopeId) {
        let Some(include_stmt) = IncludeStmt::cast(self.ctx.syntax(node)) else {
            return;
        };
        if self.include_stmt_is_structured_include(include_stmt) {
            return;
        }
        let names = include_stmt
            .names()
            .filter_map(|include_name| {
                Some((include_name.name(self.ctx.source())?, include_name.range()))
            })
            .collect::<Vec<_>>();
        for (name, range) in names {
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

    pub(super) fn walk_report_decl(&mut self, node: abap_ast::arena::NodeId, scope: ScopeId) {
        self.walk_named_header_decl(node, scope, SymbolKind::Report, crate::ScopeKind::File);
        let Some((name, range)) = self.report_message_id_reference(node) else {
            return;
        };
        self.ctx.add_reference(
            scope,
            name,
            Namespace::Type,
            ReferenceKind::MessageClass,
            range,
        );
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
        } else if scope_kind == ScopeKind::Module && kind == SymbolKind::Module {
            let function_module = self
                .ctx
                .forms_lowering()
                .declare_function_parameters_from_header(node, child_scope, owner);
            self.ctx.emit_function_module(function_module);
        }
        for child in self.ctx.file().children(node) {
            self.ctx.walk_node(child, child_scope);
        }
    }

    pub(super) fn walk_method_decl(&mut self, node: abap_ast::arena::NodeId, scope: ScopeId) {
        let Some(header) = self.method_decl_header_name_parts(node) else {
            self.ctx.walk_children(node, scope);
            return;
        };
        if let Some((interface_name, interface_range)) = &header.qualifier {
            self.ctx.add_reference(
                scope,
                Arc::clone(interface_name),
                Namespace::Type,
                ReferenceKind::TypeRef,
                interface_range.clone(),
            );
            self.ctx.emit_field_access(FieldAccess {
                scope,
                base_namespace: Namespace::Type,
                base_name: Arc::clone(interface_name),
                base_range: interface_range.clone(),
                field_path: vec![FieldAccessSegment {
                    name: Arc::clone(&header.member_name),
                    range: header.member_range.clone(),
                }],
                in_type_position: false,
            });
        }
        let owner = self.ctx.declare_plain_symbol(
            scope,
            Arc::clone(&header.full_name),
            SymbolKind::Method,
            header.full_range.clone(),
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
                    header.qualifier.as_ref().map(|(name, _)| name.as_ref()),
                    header.member_name.as_ref(),
                    scope,
                    header.full_range.clone(),
                );
            self.ctx
                .class_lowering()
                .declare_method_target_signature_parameters(
                    class_symbol,
                    header.qualifier.as_ref().map(|(name, _)| name.as_ref()),
                    header.member_name.as_ref(),
                    child_scope,
                    scope,
                );
            self.ctx.class_lowering().declare_implicit_me_symbol(
                class_symbol,
                header.qualifier.as_ref().map(|(name, _)| name.as_ref()),
                header.member_name.as_ref(),
                child_scope,
                &header.full_range,
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
        for (name, range) in self.ctx.event_block_header_value_references(node) {
            self.ctx.add_reference(
                child_scope,
                name,
                Namespace::Value,
                ReferenceKind::Identifier,
                range,
            );
        }
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
        if self.ctx.file().kind(node) == SyntaxKind::TablesDecl {
            self.declare_tables_decl_symbols(node, scope, decl_scope, kind);
            return;
        }
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
        let is_select_options_decl = self.ctx.file().kind(node) == SyntaxKind::SelectOptionsDecl;
        let is_selection_screen_decl = matches!(
            self.ctx.file().kind(node),
            SyntaxKind::ParametersDecl | SyntaxKind::SelectOptionsDecl
        );
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
                        let track_report_type_position = is_selection_screen_decl && hint.is_some();
                        if track_report_type_position {
                            *self.ctx.selection_screen_report_type_depth_mut() += 1;
                        }
                        self.declare_decl_clause_symbol(child_id, decl_scope, kind);
                        self.ctx.walk_children(child_id, scope);
                        if track_report_type_position {
                            *self.ctx.selection_screen_report_type_depth_mut() -= 1;
                        }
                        if is_select_options_decl {
                            self.collect_select_options_clause_references(child_id, scope);
                        }
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

    fn declare_tables_decl_symbols(
        &mut self,
        node: abap_ast::arena::NodeId,
        scope: ScopeId,
        decl_scope: ScopeId,
        kind: SymbolKind,
    ) {
        let Some(decl) = DataLikeDecl::cast(self.ctx.syntax(node)) else {
            self.ctx.walk_children(node, scope);
            return;
        };

        let clauses = decl
            .clauses()
            .map(|clause| {
                let child_id = clause.syntax().id();
                let name = clause.name().and_then(|name_node| {
                    Some((name_node.name(self.ctx.source())?, name_node.range()))
                });
                (child_id, name)
            })
            .collect::<Vec<_>>();

        for (child_id, name) in clauses {
            if let Some((name, range)) = name {
                let declared_type = FieldTypeRefData {
                    namespace: Namespace::Type,
                    is_ref: false,
                    base_name: Arc::clone(&name),
                    field_path: Vec::new(),
                };
                self.ctx.add_reference(
                    scope,
                    Arc::clone(&name),
                    Namespace::Type,
                    ReferenceKind::TypeRef,
                    range.clone(),
                );
                self.ctx.declare_symbol(
                    decl_scope,
                    Arc::clone(&name),
                    kind,
                    range.clone(),
                    None,
                    Some(declared_type),
                    Some(Arc::clone(&name)),
                    None,
                );
                self.ctx.emit_table_work_area(TableWorkAreaData {
                    name,
                    scope: decl_scope,
                    range,
                });
            }
            self.ctx.walk_children(child_id, scope);
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
                .declare_symbol(scope, name, kind, range, Some(structure), None, None, None);
            self.ctx
                .add_structured_decl_end_reference(node, scope, kind);
            return;
        }

        if let Some(clause) = DeclClause::cast(self.ctx.syntax(node))
            && let Some(name_node) = clause.name()
            && let Some(name) = name_node.name(self.ctx.source())
        {
            let range = name_node.range();
            let structure = self.ctx.structure_from_typed_clause(node, scope);
            let declared_type = self
                .ctx
                .type_ref_from_typed_clause(node)
                .or_else(|| self.checkbox_parameter_type_from_clause(node));
            let type_clause_display = self.ctx.type_clause_display_from_typed_clause(node);
            let value_clause_display = self.ctx.value_clause_display_from_typed_clause(node);
            self.ctx.declare_symbol(
                scope,
                name,
                kind,
                range,
                structure,
                declared_type,
                type_clause_display,
                value_clause_display,
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
                .declare_symbol(scope, name, kind, range, Some(structure), None, None, None);
            self.ctx
                .add_structured_decl_end_reference(node, scope, kind);
        }
    }

    pub(super) fn walk_inline_decl(&mut self, node: abap_ast::arena::NodeId, scope: ScopeId) {
        let (structure, declared_type) = self.ctx.inline_decl_inferred_type(node, scope);
        self.declare_inline_variable_decl(node, scope, structure, declared_type.clone());
        self.emit_inline_decl_assignment_site(node, scope, structure, declared_type);
        self.ctx.walk_children(node, scope);
    }

    fn emit_inline_decl_assignment_site(
        &mut self,
        node: abap_ast::arena::NodeId,
        scope: ScopeId,
        structure: Option<StructureId>,
        declared_type: Option<FieldTypeRefData>,
    ) {
        if self.ctx.file().kind(node) != SyntaxKind::DataInlineDecl {
            return;
        }
        let lhs_range = self
            .ctx
            .file()
            .children(node)
            .find(|&child| self.ctx.file().kind(child) == SyntaxKind::DataDeclName)
            .map(|child| self.ctx.file().range(child));
        let rhs_expr = self.ctx.file().children(node).find(|&child| {
            !matches!(
                self.ctx.file().kind(child),
                SyntaxKind::Token | SyntaxKind::DataDeclName
            )
        });
        let Some(lhs_range) = lhs_range else {
            return;
        };
        let rhs_range = rhs_expr
            .map(|rhs_expr| self.ctx.file().range(rhs_expr))
            .unwrap_or_else(|| lhs_range.end..lhs_range.end);
        let (rhs_structure, rhs_declared_type) = if rhs_expr.is_some() {
            self.ctx.inline_decl_inferred_type(node, scope)
        } else {
            (None, None)
        };
        self.ctx.emit_assignment_site(AssignmentSiteData {
            scope,
            range: self.ctx.file().range(node),
            lhs_range,
            rhs_range,
            lhs_target_access: None,
            lhs: TypeFactData {
                structure,
                declared_type,
                type_clause_display: None,
                table_line: None,
            },
            rhs: TypeFactData {
                structure: rhs_structure,
                declared_type: rhs_declared_type,
                type_clause_display: None,
                table_line: None,
            },
            rhs_is_top_level_sum: rhs_expr
                .is_some_and(|rhs_expr| self.ctx.rhs_is_top_level_sum(rhs_expr)),
        });
    }

    pub(super) fn declare_inline_variable_decl(
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
                    SymbolKind::Variable,
                    range,
                    structure,
                    declared_type.clone(),
                    None,
                    None,
                );
            }
        }
    }

    pub(super) fn walk_inline_field_symbol_decl(
        &mut self,
        node: abap_ast::arena::NodeId,
        scope: ScopeId,
    ) {
        self.declare_inline_field_symbol_decl(node, scope, None, None, None);
    }

    pub(super) fn declare_inline_field_symbol_decl(
        &mut self,
        node: abap_ast::arena::NodeId,
        scope: ScopeId,
        structure: Option<StructureId>,
        declared_type: Option<FieldTypeRefData>,
        type_clause_display: Option<Arc<str>>,
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
                    type_clause_display.clone(),
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
                range.clone(),
            );
            if self.ctx.selection_screen_report_type_active() {
                self.ctx
                    .record_selection_screen_report_type_position(range.clone());
            }
            if !field_path.is_empty() {
                self.ctx.emit_field_access(FieldAccess {
                    scope,
                    base_namespace: namespace,
                    base_name,
                    base_range: range.clone(),
                    field_path,
                    in_type_position: true,
                });
            }
        }
    }
}
