use std::sync::Arc;

use abap_ast::SyntaxKind;
use abap_ast::arena::NodeId;
use abap_ast::ast::{AstNode, CallArgList, CallExpr, CallNamedArg, CallPositionalArg};

use crate::builtins::builtin_routine_spec;
use crate::def_map::{
    FieldAccess, FieldTypeRefData, NamedArgumentAccess, NamedArgumentSection, NamedArgumentTarget,
    ReferenceKind, SymbolKind,
};
use crate::ids::ScopeId;
use crate::ids::StructureId;
use crate::scope::Namespace;

use super::Collector;
use super::context::ExprContext;

pub(super) struct ExprLowering<'ctx, 'a> {
    ctx: ExprContext<'ctx, 'a>,
}

impl<'a> Collector<'a> {
    pub(super) fn expr_lowering(&mut self) -> ExprLowering<'_, 'a> {
        ExprLowering {
            ctx: ExprContext::new(self),
        }
    }
}

impl<'ctx, 'a> ExprLowering<'ctx, 'a> {
    fn kind(&self, node: NodeId) -> SyntaxKind {
        self.ctx.file().kind(node)
    }

    fn source(&self) -> &'a str {
        self.ctx.source()
    }

    pub(super) fn collect_expr(&mut self, node: NodeId, scope: ScopeId) {
        match self.kind(node) {
            SyntaxKind::AssignStmt => self.collect_assign_stmt(node, scope),
            SyntaxKind::ExprIdent => {
                if let Some((name, range)) = self.ctx.node_name(node) {
                    self.ctx.add_reference(
                        scope,
                        name,
                        Namespace::Value,
                        ReferenceKind::Identifier,
                        range,
                    );
                }
            }
            SyntaxKind::SelectorExpr => self.collect_selector_expr(node, scope),
            SyntaxKind::SubstringExpr => self.collect_substring_expr(node, scope),
            SyntaxKind::CallExpr => self.collect_call_expr(node, scope),
            SyntaxKind::ConstructorExpr => {
                let mut arg_list = None;
                for child in self.ctx.file().children(node) {
                    match self.kind(child) {
                        SyntaxKind::TypeRefSimple => {
                            self.ctx.decl_lowering().collect_type_ref(child, scope)
                        }
                        SyntaxKind::CallArgList => arg_list = Some(child),
                        SyntaxKind::Token => {}
                        _ => self.collect_expr(child, scope),
                    }
                }
                if let Some(arg_list) = arg_list {
                    if let Some((type_name, _)) = self.ctx.constructor_type_ref(node) {
                        self.collect_call_argument_list(
                            arg_list,
                            scope,
                            NamedArgumentTarget::Constructor { type_name },
                        );
                    } else {
                        self.collect_structured_argument_values_from_children(arg_list, scope);
                    }
                }
            }
            SyntaxKind::TypeRefSimple => self.ctx.decl_lowering().collect_type_ref(node, scope),
            _ => {
                let token_children = self.ctx.syntax_token_nodes(node);
                if self.kind(node) == SyntaxKind::TemplateExpr
                    && token_children
                        .iter()
                        .any(|token| matches!(token.text.as_ref(), "[" | "]"))
                    && self
                        .ctx
                        .file()
                        .find_first_kind(node, SyntaxKind::CallExpr)
                        .is_none()
                {
                    self.ctx
                        .collect_token_expression_refs_infos(&token_children, scope, true);
                    return;
                }
                if !token_children.is_empty()
                    && self
                        .ctx
                        .file()
                        .children(node)
                        .all(|child| self.kind(child) == SyntaxKind::Token)
                {
                    self.ctx
                        .collect_token_expression_refs_infos(&token_children, scope, true);
                    return;
                }
                for child in self.ctx.file().children(node) {
                    match self.kind(child) {
                        SyntaxKind::ExprIdent
                        | SyntaxKind::SelectorExpr
                        | SyntaxKind::SubstringExpr
                        | SyntaxKind::CallExpr
                        | SyntaxKind::BinaryExpr
                        | SyntaxKind::UnaryExpr
                        | SyntaxKind::ParenExpr
                        | SyntaxKind::ConstructorExpr
                        | SyntaxKind::TemplateExpr
                        | SyntaxKind::TemplateInterpolation
                        | SyntaxKind::TemplateFormatSpec
                        | SyntaxKind::IsPredicate
                        | SyntaxKind::InstanceOfPredicate
                        | SyntaxKind::BetweenExpr
                        | SyntaxKind::AssignStmt
                        | SyntaxKind::TypeRefSimple => self.collect_expr(child, scope),
                        _ => self.ctx.walk_node(child, scope),
                    }
                }
            }
        }
    }

    fn collect_assign_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let mut non_token_children = self
            .ctx
            .file()
            .children(node)
            .filter(|&child| self.kind(child) != SyntaxKind::Token);
        let Some(lhs) = non_token_children.next() else {
            return;
        };
        let Some(rhs) = non_token_children.next() else {
            self.collect_expr(lhs, scope);
            return;
        };

        let inferred_metadata = self
            .ctx
            .control_lowering()
            .loop_source_line_metadata_from_node(rhs, scope);

        let lhs_inline = if self.kind(lhs) == SyntaxKind::DataInlineDecl {
            Some(lhs)
        } else {
            self.ctx
                .file()
                .find_first_kind(lhs, SyntaxKind::DataInlineDecl)
        };

        if let Some(lhs_inline) = lhs_inline {
            self.declare_inline_assign_target(lhs_inline, scope, &inferred_metadata);
        } else {
            self.collect_expr(lhs, scope);
        }
        self.collect_expr(rhs, scope);
    }

    fn declare_inline_assign_target(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        inferred_metadata: &(Option<StructureId>, Option<FieldTypeRefData>),
    ) {
        let decl_scope = self.ctx.declaration_scope(scope);
        for child in self.ctx.file().children(node) {
            if self.kind(child) == SyntaxKind::DataDeclName
                && let Some((name, range)) = self.ctx.node_name(child)
            {
                self.ctx.declare_symbol(
                    decl_scope,
                    name,
                    SymbolKind::Variable,
                    range,
                    inferred_metadata.0,
                    inferred_metadata.1.clone(),
                    None,
                );
                break;
            }
        }
    }

    pub(super) fn collect_named_arguments_from_infos(
        &mut self,
        tokens: &[super::SyntaxTokenInfo],
        scope: ScopeId,
        target: NamedArgumentTarget,
    ) {
        let mut idx = 0usize;
        let mut segment_start = 0usize;
        let mut current_section = None;
        while idx < tokens.len() {
            let token = &tokens[idx];
            if self.ctx.syntax_token_is_comment(token) {
                idx += 1;
                continue;
            }
            if self.ctx.syntax_token_is_ident_like(token)
                && let Some(section) = self
                    .ctx
                    .named_argument_section_from_text(token.text.as_ref())
            {
                if segment_start < idx {
                    self.ctx.collect_token_expression_refs_infos(
                        &tokens[segment_start..idx],
                        scope,
                        true,
                    );
                }
                current_section = Some(section);
                idx += 1;
                segment_start = idx;
                continue;
            }
            if self.ctx.syntax_token_is_ident_like(token)
                && tokens.get(idx + 1).map(|next| next.text.as_ref()) == Some("=")
            {
                if segment_start < idx {
                    self.ctx.collect_token_expression_refs_infos(
                        &tokens[segment_start..idx],
                        scope,
                        true,
                    );
                }
                let argument_name = Arc::<str>::from(token.text.to_ascii_lowercase());
                let value_start = idx + 2;
                let value_end = self.ctx.call_argument_value_end_infos(tokens, value_start);
                self.ctx.emit_named_argument(NamedArgumentAccess {
                    scope,
                    name: Arc::clone(&argument_name),
                    range: token.range.clone(),
                    section: current_section,
                    target: target.clone(),
                });
                let consumed_inline_target = self.ctx.declare_inline_named_argument_target_infos(
                    scope,
                    &target,
                    current_section,
                    argument_name,
                    &tokens[value_start..value_end],
                );
                if !consumed_inline_target {
                    self.ctx.collect_token_expression_refs_infos(
                        &tokens[value_start..value_end],
                        scope,
                        true,
                    );
                }
                idx = value_end;
                segment_start = idx;
                continue;
            }
            idx += 1;
        }
        if segment_start < tokens.len() {
            self.ctx
                .collect_token_expression_refs_infos(&tokens[segment_start..], scope, true);
        }
    }

    pub(super) fn call_arg_section_from_node(&self, node: NodeId) -> Option<NamedArgumentSection> {
        abap_ast::ast::CallArgSection::cast(self.ctx.syntax(node))
            .and_then(|section| section.first_token())
            .and_then(|token| token.text(self.source()))
            .and_then(|text| self.ctx.named_argument_section_from_text(text))
    }

    pub(super) fn collect_structured_argument_values(&mut self, nodes: &[NodeId], scope: ScopeId) {
        if nodes.is_empty() {
            return;
        }
        if nodes
            .iter()
            .all(|&node| self.kind(node) == SyntaxKind::Token)
        {
            let tokens = nodes
                .iter()
                .flat_map(|&node| self.ctx.syntax_token_nodes(node))
                .collect::<Vec<_>>();
            self.ctx
                .collect_token_expression_refs_infos(&tokens, scope, true);
            return;
        }

        for &node in nodes {
            match self.kind(node) {
                SyntaxKind::DataInlineDecl => {
                    self.ctx.decl_lowering().walk_inline_decl(node, scope)
                }
                SyntaxKind::FieldSymbolInlineDecl => self
                    .ctx
                    .decl_lowering()
                    .walk_inline_field_symbol_decl(node, scope),
                SyntaxKind::Token => {}
                _ => self.collect_expr(node, scope),
            }
        }
    }

    pub(super) fn collect_structured_argument_values_from_children(
        &mut self,
        node: NodeId,
        scope: ScopeId,
    ) {
        let mut all_tokens = true;
        for child in self.ctx.file().children(node) {
            if self.kind(child) != SyntaxKind::Token {
                all_tokens = false;
                break;
            }
        }
        if all_tokens {
            let mut tokens = Vec::new();
            for child in self.ctx.file().children(node) {
                tokens.extend(self.ctx.syntax_token_nodes(child));
            }
            self.ctx
                .collect_token_expression_refs_infos(&tokens, scope, true);
            return;
        }

        for child in self.ctx.file().children(node) {
            match self.kind(child) {
                SyntaxKind::CallArgSection => {}
                SyntaxKind::CallNamedArg => {
                    let value_children: Vec<_> = CallNamedArg::cast(self.ctx.syntax(child))
                        .map(|arg| {
                            arg.value_children()
                                .into_iter()
                                .map(|child| child.id())
                                .collect()
                        })
                        .unwrap_or_default();
                    self.collect_structured_argument_values(&value_children, scope);
                }
                SyntaxKind::CallPositionalArg => {
                    let value_children: Vec<_> = CallPositionalArg::cast(self.ctx.syntax(child))
                        .map(|arg| {
                            arg.value_children()
                                .into_iter()
                                .map(|child| child.id())
                                .collect()
                        })
                        .unwrap_or_default();
                    self.collect_structured_argument_values(&value_children, scope);
                }
                SyntaxKind::DataInlineDecl => {
                    self.ctx.decl_lowering().walk_inline_decl(child, scope)
                }
                SyntaxKind::FieldSymbolInlineDecl => self
                    .ctx
                    .decl_lowering()
                    .walk_inline_field_symbol_decl(child, scope),
                SyntaxKind::Token => {}
                _ => self.collect_expr(child, scope),
            }
        }
    }

    pub(super) fn collect_structured_named_argument(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        target: &NamedArgumentTarget,
        section: Option<NamedArgumentSection>,
    ) {
        let Some(named_arg) = CallNamedArg::cast(self.ctx.syntax(node)) else {
            return;
        };
        let value_children: Vec<_> = named_arg
            .value_children()
            .into_iter()
            .map(|child| child.id())
            .collect();
        let (argument_name, argument_range) = {
            let Some(name_token) = named_arg.name_token() else {
                return;
            };
            let Some(name_text) = name_token.text(self.source()) else {
                return;
            };
            (
                Arc::<str>::from(name_text.to_ascii_lowercase()),
                name_token.range(),
            )
        };
        self.ctx.emit_named_argument(NamedArgumentAccess {
            scope,
            name: Arc::clone(&argument_name),
            range: argument_range,
            section,
            target: target.clone(),
        });

        let consumed_inline_target = self
            .ctx
            .declare_inline_named_argument_target_from_nodes(
                scope,
                target,
                section,
                Arc::clone(&argument_name),
                &value_children,
            )
            .unwrap_or_else(|| {
                let value_tokens = value_children
                    .iter()
                    .flat_map(|&child| self.ctx.syntax_token_nodes(child))
                    .collect::<Vec<_>>();
                self.ctx.declare_inline_named_argument_target_infos(
                    scope,
                    target,
                    section,
                    argument_name,
                    &value_tokens,
                )
            });
        if !consumed_inline_target {
            self.collect_structured_argument_values(&value_children, scope);
        }
    }

    pub(super) fn collect_call_argument_list(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        target: NamedArgumentTarget,
    ) {
        let Some(arg_list) = CallArgList::cast(self.ctx.syntax(node)) else {
            return;
        };
        let mut current_section = None;
        let items: Vec<_> = arg_list
            .items()
            .map(|child| (child.id(), child.kind()))
            .collect();
        for (child, kind_syntax) in items {
            match kind_syntax {
                SyntaxKind::CallArgSection => {
                    current_section = self.call_arg_section_from_node(child);
                }
                SyntaxKind::CallNamedArg => {
                    self.collect_structured_named_argument(child, scope, &target, current_section);
                }
                SyntaxKind::CallPositionalArg => {
                    let value_children: Vec<_> = CallPositionalArg::cast(self.ctx.syntax(child))
                        .map(|arg| {
                            arg.value_children()
                                .into_iter()
                                .map(|child| child.id())
                                .collect()
                        })
                        .unwrap_or_default();
                    self.collect_structured_argument_values(&value_children, scope);
                }
                _ => {}
            }
        }
    }

    pub(super) fn collect_selector_expr(&mut self, node: NodeId, scope: ScopeId) {
        if let Some((namespace, base_name, base_range, field_path)) =
            self.ctx.selector_access_chain(node)
        {
            let kind = if namespace == Namespace::Type {
                ReferenceKind::StaticTarget
            } else {
                ReferenceKind::Identifier
            };
            self.ctx
                .add_reference(scope, Arc::clone(&base_name), namespace, kind, base_range);
            if !field_path.is_empty() {
                self.ctx.emit_field_access(FieldAccess {
                    scope,
                    base_namespace: namespace,
                    base_name,
                    field_path,
                    in_type_position: false,
                });
            }
            return;
        }

        let mut children = self.ctx.file().children(node);
        let base = children.next();
        let op = children.next();
        let field = children.next();
        let Some(base) = base else {
            return;
        };
        let namespace = match op.and_then(|op_node| self.ctx.syntax(op_node).text(self.source())) {
            Some("=>") => Namespace::Type,
            _ => Namespace::Value,
        };
        match self.kind(base) {
            SyntaxKind::ExprIdent => {
                if let Some((name, range)) = self.ctx.node_name(base) {
                    let kind = if namespace == Namespace::Type {
                        ReferenceKind::StaticTarget
                    } else {
                        ReferenceKind::Identifier
                    };
                    self.ctx.add_reference(scope, name, namespace, kind, range);
                }
            }
            _ => self.collect_expr(base, scope),
        }
        if let Some(field_node) = field
            && self.kind(field_node) != SyntaxKind::ExprIdent
        {
            self.collect_expr(field_node, scope);
        }
    }

    pub(super) fn collect_substring_expr(&mut self, node: NodeId, scope: ScopeId) {
        let mut children = self.ctx.file().children(node);
        let Some(base) = children.next() else {
            return;
        };

        match self.kind(base) {
            SyntaxKind::ExprIdent => {
                if let Some((name, range)) = self.ctx.node_name(base) {
                    let namespace = if self
                        .ctx
                        .lookup_symbol_in_scope_chain(scope, Namespace::Value, name.as_ref())
                        .is_some()
                        || builtin_routine_spec(name.as_ref()).is_none()
                    {
                        Namespace::Value
                    } else {
                        Namespace::Routine
                    };
                    let kind = if namespace == Namespace::Routine {
                        ReferenceKind::RoutineCall
                    } else {
                        ReferenceKind::Identifier
                    };
                    self.ctx.add_reference(scope, name, namespace, kind, range);
                }
            }
            SyntaxKind::SelectorExpr => self.collect_selector_expr(base, scope),
            _ => self.collect_expr(base, scope),
        }

        for child in children {
            if self.kind(child) != SyntaxKind::Token {
                self.collect_expr(child, scope);
            }
        }
    }

    pub(super) fn collect_call_expr(&mut self, node: NodeId, scope: ScopeId) {
        let Some(call) = CallExpr::cast(self.ctx.syntax(node)) else {
            return;
        };
        let callee = call.callee().map(|callee| (callee.id(), callee.kind()));
        let arg_list = call.arg_list().map(|arg_list| arg_list.syntax().id());
        if let Some((callee_id, callee_kind)) = callee {
            match callee_kind {
                SyntaxKind::ExprIdent => {
                    if let Some((name, range)) = self.ctx.node_name(callee_id) {
                        self.ctx.add_reference(
                            scope,
                            Arc::clone(&name),
                            Namespace::Routine,
                            ReferenceKind::RoutineCall,
                            range,
                        );
                        if let Some(arg_list) = arg_list {
                            self.collect_call_argument_list(
                                arg_list,
                                scope,
                                NamedArgumentTarget::Routine { routine_name: name },
                            );
                        }
                    }
                }
                _ => self.collect_expr(callee_id, scope),
            }
            if let Some(target) = self.ctx.named_argument_target_for_callee(callee_id)
                && let Some(arg_list) = arg_list
            {
                self.collect_call_argument_list(arg_list, scope, target);
            }
        }
    }
}
