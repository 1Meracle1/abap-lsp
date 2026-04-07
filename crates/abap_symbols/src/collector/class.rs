use std::sync::Arc;

use abap_ast::arena::NodeId;
use abap_ast::ast::{AstNode, MethodsStmt, MethodsStmtKind, MethodsTypeClauseKind};
use abap_lexer::TextRange;

use crate::def_map::{
    ClassMemberData, ClassMemberKind, ClassMemberParameterData, FieldTypeRefData, ReferenceKind,
    SymbolKind, Visibility,
};
use crate::ids::{ScopeId, StructureId, SymbolId};
use crate::scope::{Namespace, ScopeKind};

use super::emit::ClassSink;
use super::{
    Collector, PendingMethodParameter, PendingMethodSignature, PendingStructure, SyntaxTokenInfo,
};

pub(super) struct ClassLowering<'ctx, 'a> {
    collector: &'ctx mut Collector<'a>,
}

impl<'a> Collector<'a> {
    pub(super) fn class_lowering(&mut self) -> ClassLowering<'_, 'a> {
        ClassLowering { collector: self }
    }

    pub(super) fn enclosing_class_owner(&self, scope: ScopeId) -> Option<SymbolId> {
        let mut current = Some(scope);
        while let Some(scope_id) = current {
            let scope = &self.scopes[scope_id.as_usize()];
            if scope.kind == ScopeKind::Class {
                return scope.owner;
            }
            current = scope.parent;
        }
        None
    }

    pub(super) fn class_method_signature(
        &self,
        class_symbol: SymbolId,
        method_name: &str,
        lookup_scope: ScopeId,
    ) -> Option<&PendingMethodSignature> {
        self.class_method_signature_inner(class_symbol, method_name, lookup_scope, &mut Vec::new())
    }

    fn class_method_signature_inner<'b>(
        &'b self,
        class_symbol: SymbolId,
        method_name: &str,
        lookup_scope: ScopeId,
        visited: &mut Vec<SymbolId>,
    ) -> Option<&'b PendingMethodSignature> {
        if visited.contains(&class_symbol) {
            return None;
        }
        visited.push(class_symbol);

        if let Some(signature) = self
            .class_method_signatures
            .get(&class_symbol)
            .and_then(|methods| methods.get(method_name))
        {
            if !signature.is_redefinition || !signature.parameters.is_empty() {
                return Some(signature);
            }
        }

        let superclass_name = self.class_superclasses.get(&class_symbol)?;
        let superclass_symbol = self
            .lookup_symbol_in_scope_chain(lookup_scope, Namespace::Type, superclass_name.as_ref())
            .filter(|&symbol_id| self.symbol(symbol_id).kind == SymbolKind::Class)?;
        self.class_method_signature_inner(superclass_symbol, method_name, lookup_scope, visited)
    }
}

impl<'ctx, 'a> ClassLowering<'ctx, 'a> {
    pub(super) fn enclosing_class_owner(&self, scope: ScopeId) -> Option<SymbolId> {
        self.collector.enclosing_class_owner(scope)
    }

    pub(super) fn walk_class_decl(&mut self, node: NodeId, scope: ScopeId) {
        let is_implementation = self.class_header_has_implementation(node);
        let Some((name, range)) = self.collector.header_ident_after_keyword(node) else {
            self.collector.walk_children(node, scope);
            return;
        };
        let mut impl_header_refs_class = false;
        let owner = if is_implementation {
            if let Some(existing) = self
                .collector
                .lookup_symbol_in_scope_chain(scope, Namespace::Type, name.as_ref())
                .filter(|&id| self.collector.symbol(id).kind == SymbolKind::Class)
            {
                impl_header_refs_class = true;
                existing
            } else {
                self.collector.declare_plain_symbol(
                    scope,
                    Arc::clone(&name),
                    SymbolKind::Class,
                    range.clone(),
                )
            }
        } else {
            self.collector.declare_plain_symbol(
                scope,
                Arc::clone(&name),
                SymbolKind::Class,
                range.clone(),
            )
        };
        if impl_header_refs_class {
            self.collector.add_reference(
                scope,
                name,
                Namespace::Type,
                ReferenceKind::TypeRef,
                range,
            );
        }
        if !is_implementation && let Some((superclass, range)) = self.class_superclass_name(node) {
            self.collector
                .class_superclasses
                .insert(owner, Arc::clone(&superclass));
            self.collector.add_reference(
                scope,
                superclass,
                Namespace::Type,
                ReferenceKind::TypeRef,
                range,
            );
        }
        let parent_scope = if is_implementation {
            self.collector
                .class_definition_scopes
                .get(&owner)
                .copied()
                .or(Some(scope))
        } else {
            Some(scope)
        };
        let node_range = self.collector.file.range(node);
        let child_scope =
            self.collector
                .push_scope(ScopeKind::Class, node_range, parent_scope, Some(owner));
        if !is_implementation {
            self.collector
                .class_definition_scopes
                .insert(owner, child_scope);
        }
        if !is_implementation {
            self.collect_class_definition_members(node, owner, child_scope);
        }
        for child in self.collector.file.children(node) {
            self.collector.walk_node(child, child_scope);
        }
    }

    fn class_header_has_implementation(&self, node: NodeId) -> bool {
        for child in self.collector.file.children(node) {
            let Some(text) = self.collector.syntax(child).text(self.collector.source) else {
                continue;
            };
            if text == "." {
                break;
            }
            if text.eq_ignore_ascii_case("implementation") {
                return true;
            }
        }
        false
    }

    fn collect_class_definition_members(
        &mut self,
        node: NodeId,
        class_symbol: SymbolId,
        class_scope: ScopeId,
    ) {
        let mut visibility = Visibility::Private;
        let mut stack: Vec<_> = self.collector.file.children(node).rev().collect();
        while let Some(child) = stack.pop() {
            match self.collector.file.kind(child) {
                abap_ast::SyntaxKind::ClassSectionStmt => {
                    let tokens = self.collector.significant_stmt_token_infos(child);
                    if let Some(section_visibility) = self.class_section_visibility_infos(&tokens) {
                        visibility = section_visibility;
                    }
                }
                abap_ast::SyntaxKind::MethodsStmt => {
                    let Some(methods_stmt) = MethodsStmt::cast(self.collector.syntax(child)) else {
                        continue;
                    };
                    if let Some(mut member) =
                        self.class_member_from_methods_stmt(class_symbol, visibility, methods_stmt)
                    {
                        if member.kind == ClassMemberKind::Method {
                            let signature = self.parse_method_signature(methods_stmt);
                            member.parameters = self.class_member_parameters(&signature);
                            self.declare_method_signature_parameter_symbols(
                                self.collector.file.range(child),
                                &signature,
                            );
                            self.collector
                                .class_method_signatures
                                .entry(class_symbol)
                                .or_default()
                                .insert(Arc::clone(&member.name), signature);
                        }
                        self.collector.emit_class_member(member);
                    }
                }
                abap_ast::SyntaxKind::DataDecl
                | abap_ast::SyntaxKind::StaticsDecl
                | abap_ast::SyntaxKind::ConstantsDecl => {
                    self.collect_class_attribute_members(
                        child,
                        class_symbol,
                        visibility,
                        class_scope,
                    );
                }
                _ => {
                    for nested in self.collector.file.children(child).rev() {
                        stack.push(nested);
                    }
                }
            }
        }
    }

    fn class_superclass_name(&self, node: NodeId) -> Option<(Arc<str>, TextRange)> {
        let significant = self.collector.significant_stmt_token_infos(node);
        for window in significant.windows(3) {
            if window[0].text.eq_ignore_ascii_case("inheriting")
                && window[1].text.eq_ignore_ascii_case("from")
                && self.collector.syntax_token_is_ident_like(&window[2])
            {
                return Some((
                    Arc::<str>::from(window[2].text.to_ascii_lowercase()),
                    window[2].range.clone(),
                ));
            }
        }
        None
    }

    fn class_section_visibility_infos(&self, tokens: &[SyntaxTokenInfo]) -> Option<Visibility> {
        if tokens.len() < 3 || tokens[2].text.as_ref() != "." {
            return None;
        }
        if !tokens[1].text.eq_ignore_ascii_case("section") {
            return None;
        }
        if tokens[0].text.eq_ignore_ascii_case("public") {
            return Some(Visibility::Public);
        }
        if tokens[0].text.eq_ignore_ascii_case("protected") {
            return Some(Visibility::Protected);
        }
        if tokens[0].text.eq_ignore_ascii_case("private") {
            return Some(Visibility::Private);
        }
        None
    }

    fn class_member_from_methods_stmt(
        &self,
        class_symbol: SymbolId,
        visibility: Visibility,
        methods_stmt: MethodsStmt<'_>,
    ) -> Option<ClassMemberData> {
        let (kind, is_static) = match methods_stmt.member_kind(self.collector.source)? {
            MethodsStmtKind::Instance => (ClassMemberKind::Method, false),
            MethodsStmtKind::Class => (ClassMemberKind::Method, true),
        };
        let name_tok = methods_stmt.name_token(self.collector.source)?;
        Some(ClassMemberData {
            class_symbol,
            name: Arc::<str>::from(
                name_tok
                    .text(self.collector.source)
                    .unwrap_or_default()
                    .to_ascii_lowercase(),
            ),
            kind,
            visibility,
            is_static,
            decl_range: name_tok.range(),
            implementation_range: None,
            signature: Arc::<str>::from(methods_stmt.signature_text(self.collector.source)),
            parameters: Vec::new(),
            structure: None,
        })
    }

    fn collect_class_attribute_members(
        &mut self,
        node: NodeId,
        class_symbol: SymbolId,
        visibility: Visibility,
        scope: ScopeId,
    ) {
        let is_static = self.class_attribute_decl_is_static(node);
        let signature = Arc::<str>::from(
            self.collector
                .render_statement_signature_infos(&self.collector.simple_stmt_token_infos(node)),
        );
        for child in self.collector.file.children(node) {
            match self.collector.file.kind(child) {
                abap_ast::SyntaxKind::DataTypedClause
                | abap_ast::SyntaxKind::ConstantClause
                | abap_ast::SyntaxKind::StructuredDecl => {
                    if let Some(mut member) = self.class_attribute_member_from_clause(
                        child,
                        class_symbol,
                        visibility,
                        is_static,
                        Arc::clone(&signature),
                    ) {
                        member.structure = self.class_attribute_structure_for_clause(child, scope);
                        self.collector.emit_class_member(member);
                    }
                }
                _ => {}
            }
        }
    }

    fn class_attribute_structure_for_clause(
        &mut self,
        node: NodeId,
        scope: ScopeId,
    ) -> Option<StructureId> {
        let (name, _, members) = self.collector.begin_of_clause_parts(node, scope)?;
        Some(
            self.collector
                .register_structure(scope, PendingStructure { name, members }),
        )
    }

    fn class_attribute_decl_is_static(&self, node: NodeId) -> bool {
        let tokens = self.collector.significant_stmt_token_infos(node);
        let Some(first) = tokens.first() else {
            return false;
        };
        if first.text.eq_ignore_ascii_case("constants")
            || first.text.eq_ignore_ascii_case("statics")
        {
            return true;
        }
        let Some(second) = tokens.get(1) else {
            return false;
        };
        let Some(third) = tokens.get(2) else {
            return false;
        };
        first.text.eq_ignore_ascii_case("class")
            && second.text.as_ref() == "-"
            && third.text.eq_ignore_ascii_case("data")
    }

    fn class_attribute_structured_clause_name_parts(
        &self,
        node: NodeId,
    ) -> Option<(Arc<str>, TextRange)> {
        let name_node = self
            .collector
            .file
            .children(node)
            .filter(|&child| self.collector.file.kind(child) == abap_ast::SyntaxKind::Token)
            .nth(2)?;
        let (name, _) = self.collector.node_name(name_node)?;
        let decl_range = self.collector.structured_decl_name_range(node)?;
        Some((name, decl_range))
    }

    fn class_attribute_member_from_clause(
        &self,
        node: NodeId,
        class_symbol: SymbolId,
        visibility: Visibility,
        is_static: bool,
        signature: Arc<str>,
    ) -> Option<ClassMemberData> {
        let (name, decl_range) = match self.collector.file.kind(node) {
            abap_ast::SyntaxKind::StructuredDecl => {
                self.class_attribute_structured_clause_name_parts(node)?
            }
            abap_ast::SyntaxKind::DataTypedClause | abap_ast::SyntaxKind::ConstantClause => self
                .class_attribute_structured_clause_name_parts(node)
                .or_else(|| {
                    let name_node = self.collector.file.children(node).find(|&child| {
                        self.collector.file.kind(child) == abap_ast::SyntaxKind::DataDeclName
                    })?;
                    self.collector.node_name(name_node)
                })?,
            _ => return None,
        };
        Some(ClassMemberData {
            class_symbol,
            name,
            kind: ClassMemberKind::Attribute,
            visibility,
            is_static,
            decl_range,
            implementation_range: None,
            signature,
            parameters: Vec::new(),
            structure: None,
        })
    }

    fn class_member_parameters(
        &self,
        signature: &PendingMethodSignature,
    ) -> Vec<crate::def_map::ClassMemberParameterData> {
        signature
            .parameters
            .iter()
            .map(|param| ClassMemberParameterData {
                name: Arc::clone(&param.name),
                range: param.range.clone(),
                declared_type: param.declared_type.clone(),
            })
            .collect()
    }

    fn class_member(&self, class_symbol: SymbolId, member_name: &str) -> Option<&ClassMemberData> {
        self.collector.class_members.iter().find(|member| {
            member.class_symbol == class_symbol && member.name.as_ref() == member_name
        })
    }

    pub(super) fn note_method_implementation_range(
        &mut self,
        class_symbol: SymbolId,
        method_name: &str,
        range: TextRange,
    ) {
        let Some(member) = self.collector.class_members.iter_mut().find(|member| {
            member.class_symbol == class_symbol
                && member.kind == ClassMemberKind::Method
                && member.name.as_ref() == method_name
        }) else {
            return;
        };
        member.implementation_range = Some(range);
    }

    pub(super) fn declare_implicit_me_symbol(
        &mut self,
        class_symbol: SymbolId,
        method_name: &str,
        method_scope: ScopeId,
        fallback_range: &TextRange,
    ) {
        let Some(member) = self.class_member(class_symbol, method_name) else {
            return;
        };
        if member.kind != ClassMemberKind::Method || member.is_static {
            return;
        }
        let class_name = Arc::clone(&self.collector.symbol(class_symbol).name);
        self.collector.declare_symbol(
            method_scope,
            Arc::<str>::from("me"),
            SymbolKind::Variable,
            fallback_range.clone(),
            None,
            Some(FieldTypeRefData {
                namespace: Namespace::Type,
                is_ref: true,
                base_name: class_name,
                field_path: Vec::new(),
            }),
            None,
        );
    }

    fn parse_method_signature(&self, methods_stmt: MethodsStmt<'_>) -> PendingMethodSignature {
        let parsed = methods_stmt.signature(self.collector.source);
        let mut signature = PendingMethodSignature {
            is_redefinition: parsed.is_redefinition(),
            ..PendingMethodSignature::default()
        };
        for param in parsed.parameters() {
            let clause_ns = match param.type_clause() {
                MethodsTypeClauseKind::Type => Namespace::Type,
                MethodsTypeClauseKind::Like => Namespace::Value,
            };
            let name = Arc::<str>::from(
                param
                    .name_token()
                    .text(self.collector.source)
                    .unwrap_or_default()
                    .to_ascii_lowercase(),
            );
            signature.parameters.push(PendingMethodParameter {
                name,
                range: param.name_token().range(),
                declared_type: param.type_ref().and_then(|type_ref| {
                    self.collector
                        .field_type_ref_from_node(type_ref.syntax().id(), clause_ns)
                }),
            });
        }
        signature
    }

    pub(super) fn declare_method_signature_parameters(
        &mut self,
        class_symbol: SymbolId,
        method_name: &str,
        method_scope: ScopeId,
        lookup_scope: ScopeId,
    ) {
        let Some(parameters) = self
            .collector
            .class_method_signature(class_symbol, method_name, lookup_scope)
            .map(|signature| signature.parameters.clone())
        else {
            return;
        };
        for param in parameters {
            self.collector.declare_symbol(
                method_scope,
                param.name,
                SymbolKind::Parameter,
                param.range,
                None,
                param.declared_type,
                None,
            );
        }
    }

    fn declare_method_signature_parameter_symbols(
        &mut self,
        signature_range: TextRange,
        signature: &PendingMethodSignature,
    ) {
        if signature.parameters.is_empty() {
            return;
        }
        let signature_scope =
            self.collector
                .push_scope(ScopeKind::Method, signature_range, None, None);
        for param in &signature.parameters {
            self.collector.declare_symbol(
                signature_scope,
                Arc::clone(&param.name),
                SymbolKind::Parameter,
                param.range.clone(),
                None,
                param.declared_type.clone(),
                None,
            );
        }
    }
}
