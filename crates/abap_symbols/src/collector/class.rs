use std::sync::Arc;

use abap_ast::arena::NodeId;
use abap_ast::ast::{
    AstNode, ClassDecl, ClassSectionStmt, ClassSectionVisibilityKind, DataDeclName, DataLikeDecl,
    DataLikeStorageKind, EventsStmt, EventsStmtEntry, EventsStmtKind, InterfaceDecl,
    MethodsParamSectionKind, MethodsStmt, MethodsStmtEntry, MethodsStmtKind, MethodsTypeClauseKind,
};
use abap_lexer::TextRange;

use crate::def_map::{
    ClassMemberData, ClassMemberKind, ClassMemberParameterData, FieldTypeRefData,
    MethodParameterSection, ReferenceKind, SymbolKind, Visibility,
};
use crate::ids::{ScopeId, StructureId, SymbolId};
use crate::scope::{Namespace, ScopeKind};

use super::emit::ClassSink;
use super::{Collector, PendingMethodParameter, PendingMethodSignature, PendingStructure};

fn method_parameter_section(section: MethodsParamSectionKind) -> MethodParameterSection {
    match section {
        MethodsParamSectionKind::Importing => MethodParameterSection::Importing,
        MethodsParamSectionKind::Exporting => MethodParameterSection::Exporting,
        MethodsParamSectionKind::Changing => MethodParameterSection::Changing,
        MethodsParamSectionKind::Receiving => MethodParameterSection::Receiving,
        MethodsParamSectionKind::Returning => MethodParameterSection::Returning,
    }
}

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

    pub(super) fn enclosing_type_owner(&self, scope: ScopeId) -> Option<SymbolId> {
        let mut current = Some(scope);
        while let Some(scope_id) = current {
            let scope = &self.scopes[scope_id.as_usize()];
            if matches!(scope.kind, ScopeKind::Class | ScopeKind::Interface) {
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
        self.class_method_signature_target(class_symbol, None, method_name, lookup_scope)
    }

    pub(super) fn class_method_signature_target(
        &self,
        owner_symbol: SymbolId,
        qualifier: Option<&str>,
        method_name: &str,
        lookup_scope: ScopeId,
    ) -> Option<&PendingMethodSignature> {
        self.class_method_signature_target_inner(
            owner_symbol,
            qualifier,
            method_name,
            lookup_scope,
            &mut Vec::new(),
        )
    }

    pub(super) fn class_member_target_data(
        &self,
        owner_symbol: SymbolId,
        qualifier: Option<&str>,
        member_name: &str,
        lookup_scope: ScopeId,
    ) -> Option<ClassMemberData> {
        self.class_member_target_data_inner(
            owner_symbol,
            qualifier,
            member_name,
            lookup_scope,
            &mut Vec::new(),
        )
    }

    pub(super) fn class_event_target_data(
        &self,
        owner_symbol: SymbolId,
        qualifier: Option<&str>,
        event_name: &str,
        lookup_scope: ScopeId,
    ) -> Option<ClassMemberData> {
        self.class_event_target_data_inner(
            owner_symbol,
            qualifier,
            event_name,
            lookup_scope,
            &mut Vec::new(),
        )
    }

    fn class_method_signature_target_inner<'b>(
        &'b self,
        owner_symbol: SymbolId,
        qualifier: Option<&str>,
        method_name: &str,
        lookup_scope: ScopeId,
        visited: &mut Vec<(SymbolId, Option<Arc<str>>, Arc<str>)>,
    ) -> Option<&'b PendingMethodSignature> {
        let key = (
            owner_symbol,
            qualifier.map(Arc::<str>::from),
            Arc::<str>::from(method_name),
        );
        if visited.contains(&key) {
            return None;
        }
        visited.push(key);

        let direct_owner = if let Some(interface_name) = qualifier {
            self.resolve_exposed_interface_symbol(owner_symbol, lookup_scope, interface_name)?
        } else {
            owner_symbol
        };

        if let Some(signature) = self
            .class_method_signatures
            .get(&direct_owner)
            .and_then(|methods| methods.get(method_name))
        {
            if !signature.is_redefinition || !signature.parameters.is_empty() {
                return Some(signature);
            }
        }

        if let Some(alias) = self.member_alias(direct_owner, method_name) {
            return self.class_method_signature_target_inner(
                direct_owner,
                Some(alias.target_interface_name.as_ref()),
                alias.target_member_name.as_ref(),
                lookup_scope,
                visited,
            );
        }

        if qualifier.is_none() && self.symbol(direct_owner).kind == SymbolKind::Class {
            let superclass_name = self.class_superclasses.get(&direct_owner)?;
            let superclass_symbol = self
                .lookup_symbol_in_scope_chain(
                    lookup_scope,
                    Namespace::Type,
                    superclass_name.as_ref(),
                )
                .filter(|&symbol_id| self.symbol(symbol_id).kind == SymbolKind::Class)?;
            return self.class_method_signature_target_inner(
                superclass_symbol,
                None,
                method_name,
                lookup_scope,
                visited,
            );
        }

        None
    }

    fn class_member_target_data_inner(
        &self,
        owner_symbol: SymbolId,
        qualifier: Option<&str>,
        member_name: &str,
        lookup_scope: ScopeId,
        visited: &mut Vec<(SymbolId, Option<Arc<str>>, Arc<str>)>,
    ) -> Option<ClassMemberData> {
        let key = (
            owner_symbol,
            qualifier.map(Arc::<str>::from),
            Arc::<str>::from(member_name),
        );
        if visited.contains(&key) {
            return None;
        }
        visited.push(key);

        let direct_owner = if let Some(interface_name) = qualifier {
            self.resolve_exposed_interface_symbol(owner_symbol, lookup_scope, interface_name)?
        } else {
            owner_symbol
        };

        if let Some(index) = self.class_member_index(direct_owner, member_name) {
            return Some(self.class_members[index].clone());
        }

        if let Some(alias) = self.member_alias(direct_owner, member_name) {
            return self.class_member_target_data_inner(
                direct_owner,
                Some(alias.target_interface_name.as_ref()),
                alias.target_member_name.as_ref(),
                lookup_scope,
                visited,
            );
        }

        if qualifier.is_none() && self.symbol(direct_owner).kind == SymbolKind::Class {
            let superclass_name = self.class_superclasses.get(&direct_owner)?;
            let superclass_symbol = self
                .lookup_symbol_in_scope_chain(
                    lookup_scope,
                    Namespace::Type,
                    superclass_name.as_ref(),
                )
                .filter(|&symbol_id| self.symbol(symbol_id).kind == SymbolKind::Class)?;
            return self.class_member_target_data_inner(
                superclass_symbol,
                None,
                member_name,
                lookup_scope,
                visited,
            );
        }

        None
    }

    fn class_event_target_data_inner(
        &self,
        owner_symbol: SymbolId,
        qualifier: Option<&str>,
        event_name: &str,
        lookup_scope: ScopeId,
        visited: &mut Vec<(SymbolId, Option<Arc<str>>, Arc<str>)>,
    ) -> Option<ClassMemberData> {
        let key = (
            owner_symbol,
            qualifier.map(Arc::<str>::from),
            Arc::<str>::from(event_name),
        );
        if visited.contains(&key) {
            return None;
        }
        visited.push(key);

        let direct_owner = if let Some(interface_name) = qualifier {
            self.resolve_exposed_interface_symbol(owner_symbol, lookup_scope, interface_name)?
        } else {
            owner_symbol
        };

        if let Some(index) = self.class_member_index(direct_owner, event_name) {
            let member = &self.class_members[index];
            if member.kind == ClassMemberKind::Event {
                return Some(member.clone());
            }
        }
        if let Some(member) = self.class_members.iter().find(|member| {
            member.class_symbol == direct_owner
                && member.kind == ClassMemberKind::Event
                && member.name.as_ref() == event_name
        }) {
            return Some(member.clone());
        }

        if let Some(alias) = self.member_alias(direct_owner, event_name) {
            return self.class_event_target_data_inner(
                direct_owner,
                Some(alias.target_interface_name.as_ref()),
                alias.target_member_name.as_ref(),
                lookup_scope,
                visited,
            );
        }

        if qualifier.is_none() {
            for implemented in self
                .implemented_interfaces
                .iter()
                .filter(|implemented| implemented.owner_symbol == direct_owner)
            {
                let Some(interface_symbol) = self
                    .lookup_symbol_in_scope_chain(
                        lookup_scope,
                        Namespace::Type,
                        implemented.interface_name.as_ref(),
                    )
                    .filter(|&symbol_id| self.symbol(symbol_id).kind == SymbolKind::Interface)
                else {
                    continue;
                };
                if let Some(found) = self.class_event_target_data_inner(
                    interface_symbol,
                    None,
                    event_name,
                    lookup_scope,
                    visited,
                ) {
                    return Some(found);
                }
            }
        }

        if qualifier.is_none() && self.symbol(direct_owner).kind == SymbolKind::Class {
            let superclass_name = self.class_superclasses.get(&direct_owner)?;
            let superclass_symbol = self
                .lookup_symbol_in_scope_chain(
                    lookup_scope,
                    Namespace::Type,
                    superclass_name.as_ref(),
                )
                .filter(|&symbol_id| self.symbol(symbol_id).kind == SymbolKind::Class)?;
            return self.class_event_target_data_inner(
                superclass_symbol,
                None,
                event_name,
                lookup_scope,
                visited,
            );
        }

        None
    }

    fn member_alias(
        &self,
        owner_symbol: SymbolId,
        alias_name: &str,
    ) -> Option<&crate::MemberAliasData> {
        self.member_aliases.iter().find(|alias| {
            alias.owner_symbol == owner_symbol && alias.alias_name.as_ref() == alias_name
        })
    }

    fn resolve_exposed_interface_symbol(
        &self,
        owner_symbol: SymbolId,
        lookup_scope: ScopeId,
        interface_name: &str,
    ) -> Option<SymbolId> {
        self.resolve_exposed_interface_symbol_inner(
            owner_symbol,
            lookup_scope,
            interface_name,
            &mut Vec::new(),
        )
    }

    fn resolve_exposed_interface_symbol_inner(
        &self,
        owner_symbol: SymbolId,
        lookup_scope: ScopeId,
        interface_name: &str,
        visited: &mut Vec<SymbolId>,
    ) -> Option<SymbolId> {
        if visited.contains(&owner_symbol) {
            return None;
        }
        visited.push(owner_symbol);
        for implemented in self
            .implemented_interfaces
            .iter()
            .filter(|implemented| implemented.owner_symbol == owner_symbol)
        {
            let Some(interface_symbol) = self
                .lookup_symbol_in_scope_chain(
                    lookup_scope,
                    Namespace::Type,
                    implemented.interface_name.as_ref(),
                )
                .filter(|&symbol_id| self.symbol(symbol_id).kind == SymbolKind::Interface)
            else {
                continue;
            };
            if implemented
                .interface_name
                .as_ref()
                .eq_ignore_ascii_case(interface_name)
            {
                return Some(interface_symbol);
            }
            if let Some(found) = self.resolve_exposed_interface_symbol_inner(
                interface_symbol,
                lookup_scope,
                interface_name,
                visited,
            ) {
                return Some(found);
            }
        }

        if self.symbol(owner_symbol).kind == SymbolKind::Class
            && let Some(superclass_name) = self.class_superclasses.get(&owner_symbol)
            && let Some(superclass_symbol) = self
                .lookup_symbol_in_scope_chain(
                    lookup_scope,
                    Namespace::Type,
                    superclass_name.as_ref(),
                )
                .filter(|&symbol_id| self.symbol(symbol_id).kind == SymbolKind::Class)
        {
            return self.resolve_exposed_interface_symbol_inner(
                superclass_symbol,
                lookup_scope,
                interface_name,
                visited,
            );
        }

        None
    }
}

fn qualified_method_signature_matches(signature: &str, method_name: &str) -> bool {
    let mut parts = signature.split_ascii_whitespace();
    let _keyword = parts.next();
    let Some(first) = parts.next() else {
        return false;
    };
    if let Some((_, member)) = first.rsplit_once('~') {
        return member.eq_ignore_ascii_case(method_name);
    }
    parts.next().is_some_and(|part| part == "~")
        && parts
            .next()
            .is_some_and(|part| part.eq_ignore_ascii_case(method_name))
}

impl<'ctx, 'a> ClassLowering<'ctx, 'a> {
    pub(super) fn enclosing_class_owner(&self, scope: ScopeId) -> Option<SymbolId> {
        self.collector.enclosing_class_owner(scope)
    }

    pub(super) fn enclosing_type_owner(&self, scope: ScopeId) -> Option<SymbolId> {
        self.collector.enclosing_type_owner(scope)
    }

    pub(super) fn walk_interface_decl(&mut self, node: NodeId, scope: ScopeId) {
        let Some(interface_decl) = InterfaceDecl::cast(self.collector.syntax(node)) else {
            self.collector.walk_children(node, scope);
            return;
        };
        let Some(name_tok) = interface_decl.name_token() else {
            self.collector.walk_children(node, scope);
            return;
        };
        let Some(name) = name_tok.name(self.collector.source) else {
            self.collector.walk_children(node, scope);
            return;
        };
        let range = name_tok.range();
        let owner = self
            .collector
            .lookup_symbol_in_scope_chain(scope, Namespace::Type, name.as_ref())
            .filter(|&id| {
                self.collector.symbol(id).kind == SymbolKind::Interface
                    && self.collector.deferred_interface_symbols.contains(&id)
                    && !self.collector.interface_definition_symbols.contains(&id)
            })
            .unwrap_or_else(|| {
                self.collector.declare_plain_symbol(
                    scope,
                    Arc::clone(&name),
                    SymbolKind::Interface,
                    range.clone(),
                )
            });
        self.collector.interface_definition_symbols.insert(owner);
        let node_range = self.collector.file.range(node);
        let child_scope =
            self.collector
                .push_scope(ScopeKind::Interface, node_range, Some(scope), Some(owner));
        self.collect_class_definition_members(node, owner, child_scope, Visibility::Public);
        for child in self.collector.file.children(node) {
            self.collector.walk_node(child, child_scope);
        }
    }

    pub(super) fn walk_class_deferred_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let names = self.deferred_stmt_names(node);
        if names.is_empty() {
            self.collector.walk_children(node, scope);
            return;
        }
        for (name, range) in names {
            let symbol = self
                .collector
                .declare_plain_symbol(scope, name, SymbolKind::Class, range);
            self.collector.deferred_class_symbols.insert(symbol);
        }
    }

    pub(super) fn walk_interface_deferred_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let names = self.deferred_stmt_names(node);
        if names.is_empty() {
            self.collector.walk_children(node, scope);
            return;
        }
        for (name, range) in names {
            let symbol =
                self.collector
                    .declare_plain_symbol(scope, name, SymbolKind::Interface, range);
            self.collector.deferred_interface_symbols.insert(symbol);
        }
    }

    fn deferred_stmt_names(&self, node: NodeId) -> Vec<(Arc<str>, TextRange)> {
        let mut names = self
            .collector
            .file
            .children(node)
            .filter_map(|child| DataDeclName::cast(self.collector.syntax(child)))
            .filter_map(|name_node| {
                Some((name_node.name(self.collector.source)?, name_node.range()))
            })
            .collect::<Vec<_>>();
        if names.is_empty()
            && let Some((name, range)) = self.collector.header_ident_after_keyword(node)
        {
            names.push((name, range));
        }
        names
    }

    pub(super) fn walk_class_decl(&mut self, node: NodeId, scope: ScopeId) {
        let Some((is_implementation, is_abstract, name, range, superclass_info)) = (|| {
            let class_decl = ClassDecl::cast(self.collector.syntax(node))?;
            let name_tok = class_decl.name_token()?;
            let name = name_tok.name(self.collector.source)?;
            let range = name_tok.range();
            let is_abstract =
                !class_decl.is_implementation() && class_decl.is_abstract(self.collector.source);
            let superclass_info = if class_decl.is_implementation() {
                None
            } else {
                class_decl.superclass().and_then(|type_ref| {
                    type_ref.display_text(self.collector.source).map(|text| {
                        (
                            Arc::<str>::from(text.to_ascii_lowercase()),
                            type_ref.syntax().range(),
                        )
                    })
                })
            };
            Some((
                class_decl.is_implementation(),
                is_abstract,
                name,
                range,
                superclass_info,
            ))
        })() else {
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
            self.collector
                .lookup_symbol_in_scope_chain(scope, Namespace::Type, name.as_ref())
                .filter(|&id| {
                    self.collector.symbol(id).kind == SymbolKind::Class
                        && self.collector.deferred_class_symbols.contains(&id)
                        && !self.collector.class_definition_symbols.contains(&id)
                })
                .unwrap_or_else(|| {
                    self.collector.declare_plain_symbol(
                        scope,
                        Arc::clone(&name),
                        SymbolKind::Class,
                        range.clone(),
                    )
                })
        };
        if impl_header_refs_class {
            self.collector.add_reference(
                scope,
                Arc::clone(&name),
                Namespace::Type,
                ReferenceKind::TypeRef,
                range,
            );
        }
        if !is_implementation && let Some((superclass, superclass_range)) = superclass_info {
            self.collector
                .class_superclasses
                .insert(owner, Arc::clone(&superclass));
            self.collector.add_reference(
                scope,
                superclass,
                Namespace::Type,
                ReferenceKind::TypeRef,
                superclass_range,
            );
        }
        if !is_implementation {
            self.collector.class_definition_symbols.insert(owner);
        }
        if !is_implementation && is_abstract {
            self.collector.abstract_classes.insert(owner);
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
            self.collect_class_definition_members(node, owner, child_scope, Visibility::Private);
        }
        for child in self.collector.file.children(node) {
            self.collector.walk_node(child, child_scope);
        }
    }

    fn collect_class_definition_members(
        &mut self,
        node: NodeId,
        class_symbol: SymbolId,
        class_scope: ScopeId,
        default_visibility: Visibility,
    ) {
        let mut visibility = default_visibility;
        let mut pending_methods = Vec::new();
        let mut pending_events = Vec::new();
        let mut stack: Vec<_> = self.collector.file.children(node).rev().collect();
        while let Some(child) = stack.pop() {
            match self.collector.file.kind(child) {
                abap_ast::SyntaxKind::ClassSectionStmt => {
                    if let Some(section_visibility) =
                        ClassSectionStmt::cast(self.collector.syntax(child))
                            .and_then(|stmt| stmt.visibility())
                            .and_then(|visibility| visibility.kind(self.collector.source))
                            .map(|visibility| match visibility {
                                ClassSectionVisibilityKind::Public => Visibility::Public,
                                ClassSectionVisibilityKind::Protected => Visibility::Protected,
                                ClassSectionVisibilityKind::Private => Visibility::Private,
                            })
                    {
                        visibility = section_visibility;
                    }
                }
                abap_ast::SyntaxKind::InterfacesStmt => {
                    self.collect_interface_exposure_stmt(child, class_symbol, class_scope);
                }
                abap_ast::SyntaxKind::MethodsStmt => {
                    let Some(methods_stmt) = MethodsStmt::cast(self.collector.syntax(child)) else {
                        continue;
                    };
                    for entry in methods_stmt.entries(self.collector.source) {
                        let Some(member) =
                            self.class_member_from_methods_stmt(class_symbol, visibility, &entry)
                        else {
                            continue;
                        };
                        let signature = self.parse_method_signature(&entry);
                        pending_methods.push((member, signature, self.collector.file.range(child)));
                    }
                }
                abap_ast::SyntaxKind::EventsStmt => {
                    let Some(events_stmt) = EventsStmt::cast(self.collector.syntax(child)) else {
                        continue;
                    };
                    for entry in events_stmt.entries(self.collector.source) {
                        let Some(member) = self.class_member_from_events_stmt(
                            class_symbol,
                            visibility,
                            &entry,
                            class_scope,
                        ) else {
                            continue;
                        };
                        pending_events.push(member);
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

        for member in pending_events {
            self.declare_event_member_symbol(class_scope, &member);
            self.collector.emit_class_member(member);
        }

        for (mut member, mut signature, stmt_range) in pending_methods {
            self.resolve_event_handler_signature_parameters(
                class_symbol,
                class_scope,
                &mut signature,
            );
            member.parameters = self.class_member_parameters(&signature);
            self.declare_method_signature_parameter_symbols(stmt_range, &signature);
            self.collector
                .class_method_signatures
                .entry(class_symbol)
                .or_default()
                .insert(Arc::clone(&member.name), signature);
            self.collector.emit_class_member(member);
        }
    }

    fn class_member_from_methods_stmt(
        &self,
        class_symbol: SymbolId,
        visibility: Visibility,
        entry: &MethodsStmtEntry<'_>,
    ) -> Option<ClassMemberData> {
        let (kind, is_static) = match entry.member_kind() {
            MethodsStmtKind::Instance => (ClassMemberKind::Method, false),
            MethodsStmtKind::Class => (ClassMemberKind::Method, true),
        };
        let name_tok = entry.name_token(self.collector.source)?;
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
            implementation: None,
            signature: Arc::<str>::from(entry.signature_text(self.collector.source)),
            parameters: Vec::new(),
            structure: None,
        })
    }

    fn class_member_from_events_stmt(
        &self,
        class_symbol: SymbolId,
        visibility: Visibility,
        entry: &EventsStmtEntry<'_>,
        scope: ScopeId,
    ) -> Option<ClassMemberData> {
        let is_static = matches!(entry.member_kind(), EventsStmtKind::Class);
        let name_tok = entry.name_token(self.collector.source)?;
        Some(ClassMemberData {
            class_symbol,
            name: Arc::<str>::from(
                name_tok
                    .text(self.collector.source)
                    .unwrap_or_default()
                    .to_ascii_lowercase(),
            ),
            kind: ClassMemberKind::Event,
            visibility,
            is_static,
            decl_range: name_tok.range(),
            implementation_range: None,
            implementation: None,
            signature: Arc::<str>::from(entry.signature_text(self.collector.source)),
            parameters: self.class_event_parameters(entry, scope),
            structure: None,
        })
    }

    fn collect_interface_exposure_stmt(
        &mut self,
        node: NodeId,
        owner_symbol: SymbolId,
        scope: ScopeId,
    ) {
        let Some(stmt) = abap_ast::ast::InterfacesStmt::cast(self.collector.syntax(node)) else {
            return;
        };
        let interfaces = stmt
            .type_refs()
            .filter_map(|type_ref| {
                self.collector
                    .type_ref_access_chain(type_ref.syntax().id(), Namespace::Type)
                    .map(|(_, _, interface_name, range, _)| (interface_name, range))
            })
            .collect::<Vec<_>>();
        for (interface_name, range) in interfaces {
            if !self
                .collector
                .implemented_interfaces
                .iter()
                .any(|implemented| {
                    implemented.owner_symbol == owner_symbol
                        && implemented.interface_name == interface_name
                })
            {
                self.collector
                    .implemented_interfaces
                    .push(crate::ImplementedInterfaceData {
                        owner_symbol,
                        interface_name,
                        range,
                    });
            }
        }
        let _ = scope;
    }

    fn declare_event_member_symbol(&mut self, scope: ScopeId, member: &ClassMemberData) {
        self.collector.declare_symbol(
            scope,
            Arc::clone(&member.name),
            SymbolKind::Event,
            member.decl_range.clone(),
            None,
            None,
            None,
            None,
        );
    }

    fn collect_class_attribute_members(
        &mut self,
        node: NodeId,
        class_symbol: SymbolId,
        visibility: Visibility,
        scope: ScopeId,
    ) {
        let Some(decl) = DataLikeDecl::cast(self.collector.syntax(node)) else {
            return;
        };
        let Some(storage_kind) = decl.storage_kind(self.collector.source) else {
            return;
        };
        let is_static = !matches!(storage_kind, DataLikeStorageKind::Instance);
        let signature = Arc::<str>::from(decl.signature_text(self.collector.source));
        let clause_infos = decl
            .clauses()
            .filter_map(|clause| {
                let (name, decl_range) = clause.declared_name(self.collector.source)?;
                Some((clause.syntax().id(), name, decl_range))
            })
            .collect::<Vec<_>>();
        for (clause_id, name, decl_range) in clause_infos {
            let mut member = self.class_attribute_member_from_name(
                class_symbol,
                visibility,
                is_static,
                Arc::clone(&signature),
                name,
                decl_range,
            );
            member.structure = self.class_attribute_structure_for_clause(clause_id, scope);
            self.collector.emit_class_member(member);
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

    fn class_attribute_member_from_name(
        &self,
        class_symbol: SymbolId,
        visibility: Visibility,
        is_static: bool,
        signature: Arc<str>,
        name: Arc<str>,
        decl_range: TextRange,
    ) -> ClassMemberData {
        ClassMemberData {
            class_symbol,
            name,
            kind: ClassMemberKind::Attribute,
            visibility,
            is_static,
            decl_range,
            implementation_range: None,
            implementation: None,
            signature,
            parameters: Vec::new(),
            structure: None,
        }
    }

    fn class_member_parameters(
        &self,
        signature: &PendingMethodSignature,
    ) -> Vec<crate::def_map::ClassMemberParameterData> {
        signature
            .parameters
            .iter()
            .map(|param| ClassMemberParameterData {
                section: method_parameter_section(param.section),
                name: Arc::clone(&param.name),
                range: param.range.clone(),
                declared_type: param.declared_type.clone(),
                type_clause_display: param.type_clause_display.clone(),
                is_optional: param.is_optional,
            })
            .collect()
    }

    fn class_event_parameters(
        &self,
        entry: &EventsStmtEntry<'_>,
        _scope: ScopeId,
    ) -> Vec<crate::def_map::ClassMemberParameterData> {
        entry
            .signature(self.collector.source)
            .parameters()
            .iter()
            .map(|param| {
                let clause_ns = match param.type_clause() {
                    MethodsTypeClauseKind::Type => Namespace::Type,
                    MethodsTypeClauseKind::Like => Namespace::Value,
                };
                crate::def_map::ClassMemberParameterData {
                    section: MethodParameterSection::Exporting,
                    name: Arc::<str>::from(
                        param
                            .name_token()
                            .text(self.collector.source)
                            .unwrap_or_default()
                            .to_ascii_lowercase(),
                    ),
                    range: param.name_token().range(),
                    declared_type: param.type_ref().and_then(|type_ref| {
                        self.collector
                            .field_type_ref_from_node(type_ref.syntax().id(), clause_ns)
                    }),
                    type_clause_display: param
                        .type_display_text(self.collector.source)
                        .map(Arc::from),
                    is_optional: param.is_optional(),
                }
            })
            .collect()
    }

    pub(super) fn note_method_implementation_target_range(
        &mut self,
        owner_symbol: SymbolId,
        qualifier: Option<&str>,
        method_name: &str,
        lookup_scope: ScopeId,
        range: TextRange,
    ) {
        let implementation = crate::ClassMemberImplementationData {
            unit: self.collector.unit_id,
            range: range.clone(),
        };
        if qualifier.is_some()
            && let Some(index) = self.collector.class_member_index(owner_symbol, method_name)
        {
            let member = &mut self.collector.class_members[index];
            if member.kind == ClassMemberKind::Method
                && qualified_method_signature_matches(member.signature.as_ref(), method_name)
            {
                member.implementation_range = Some(range.clone());
                member.implementation = Some(implementation.clone());
            }
        }
        let target_owner = if let Some(interface_name) = qualifier {
            let Some(interface_symbol) = self.collector.resolve_exposed_interface_symbol(
                owner_symbol,
                lookup_scope,
                interface_name,
            ) else {
                return;
            };
            interface_symbol
        } else {
            owner_symbol
        };
        if let Some(index) = self.collector.class_member_index(target_owner, method_name) {
            let member = &mut self.collector.class_members[index];
            if member.kind == ClassMemberKind::Method {
                member.implementation_range = Some(range.clone());
                member.implementation = Some(implementation);
                return;
            }
        }
        let Some(member) = self.collector.class_members.iter_mut().find(|member| {
            member.class_symbol == target_owner
                && member.kind == ClassMemberKind::Method
                && member.name.as_ref() == method_name
        }) else {
            return;
        };
        member.implementation_range = Some(range.clone());
        member.implementation = Some(implementation);
    }

    pub(super) fn declare_implicit_me_symbol(
        &mut self,
        class_symbol: SymbolId,
        qualifier: Option<&str>,
        method_name: &str,
        method_scope: ScopeId,
        fallback_range: &TextRange,
    ) {
        let Some(member) = self.collector.class_member_target_data(
            class_symbol,
            qualifier,
            method_name,
            method_scope,
        ) else {
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
            None,
        );
    }

    fn parse_method_signature(&self, entry: &MethodsStmtEntry<'_>) -> PendingMethodSignature {
        let parsed = entry.signature(self.collector.source);
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
                section: param.section(),
                name,
                range: param.name_token().range(),
                declared_type: param.type_ref().and_then(|type_ref| {
                    self.collector
                        .field_type_ref_from_node(type_ref.syntax().id(), clause_ns)
                }),
                type_clause_display: param
                    .type_display_text(self.collector.source)
                    .map(Arc::from),
                is_optional: param.is_optional(),
            });
        }
        if let Some(handler) = entry.event_handler(self.collector.source)
            && let Some(spec) = self.pending_event_handler_spec(&handler)
        {
            signature.event_handler = Some(spec);
        }
        signature
    }

    fn pending_event_handler_spec(
        &self,
        handler: &abap_ast::ast::MethodsStmtEventHandler<'_>,
    ) -> Option<super::PendingEventHandlerSpec> {
        let source_type_ref = handler.source_type_ref();
        let source_type_display = Arc::<str>::from(
            source_type_ref
                .display_text(self.collector.source)?
                .trim()
                .to_string(),
        );
        let source_type_name = Arc::<str>::from(source_type_display.to_ascii_lowercase());
        let event_qualifier = handler.event_qualifier().and_then(|token| {
            token
                .text(self.collector.source)
                .map(|text| Arc::<str>::from(text.to_ascii_lowercase()))
        });
        let event_name = Arc::<str>::from(
            handler
                .event_name()
                .text(self.collector.source)?
                .to_ascii_lowercase(),
        );
        let importing_names = handler
            .importing_names()
            .iter()
            .filter_map(|token| {
                Some((
                    Arc::<str>::from(token.text(self.collector.source)?.to_ascii_lowercase()),
                    token.range(),
                ))
            })
            .collect::<Vec<_>>();
        Some(super::PendingEventHandlerSpec {
            event_qualifier,
            event_name,
            source_type_name,
            source_type_display,
            importing_names,
        })
    }

    fn resolve_event_handler_signature_parameters(
        &self,
        _owner_symbol: SymbolId,
        lookup_scope: ScopeId,
        signature: &mut PendingMethodSignature,
    ) {
        if !signature.parameters.is_empty() {
            return;
        }
        let Some(spec) = signature.event_handler.as_ref() else {
            return;
        };
        let Some(source_symbol) = self
            .collector
            .lookup_symbol_in_scope_chain(
                lookup_scope,
                Namespace::Type,
                spec.source_type_name.as_ref(),
            )
            .filter(|&symbol_id| {
                matches!(
                    self.collector.symbol(symbol_id).kind,
                    SymbolKind::Class | SymbolKind::Interface
                )
            })
        else {
            return;
        };
        let Some(event_member) = self.collector.class_event_target_data(
            source_symbol,
            spec.event_qualifier.as_deref(),
            spec.event_name.as_ref(),
            lookup_scope,
        ) else {
            return;
        };

        signature.parameters = spec
            .importing_names
            .iter()
            .filter_map(|(name, range)| {
                if name.as_ref() == "sender" {
                    if event_member.is_static {
                        return None;
                    }
                    return Some(PendingMethodParameter {
                        section: MethodsParamSectionKind::Importing,
                        name: Arc::clone(name),
                        range: range.clone(),
                        declared_type: Some(FieldTypeRefData {
                            namespace: Namespace::Type,
                            is_ref: true,
                            base_name: Arc::clone(&spec.source_type_name),
                            field_path: Vec::new(),
                        }),
                        type_clause_display: Some(Arc::<str>::from(format!(
                            "REF TO {}",
                            spec.source_type_display
                        ))),
                        is_optional: false,
                    });
                }
                let parameter = event_member
                    .parameters
                    .iter()
                    .find(|parameter| parameter.name == *name);
                Some(PendingMethodParameter {
                    section: MethodsParamSectionKind::Importing,
                    name: Arc::clone(name),
                    range: range.clone(),
                    declared_type: parameter.and_then(|parameter| parameter.declared_type.clone()),
                    type_clause_display: parameter
                        .and_then(|parameter| parameter.type_clause_display.clone()),
                    is_optional: parameter.is_some_and(|parameter| parameter.is_optional),
                })
            })
            .collect();
    }

    pub(super) fn declare_method_target_signature_parameters(
        &mut self,
        owner_symbol: SymbolId,
        qualifier: Option<&str>,
        method_name: &str,
        method_scope: ScopeId,
        lookup_scope: ScopeId,
    ) {
        let Some(parameters) = self
            .collector
            .class_method_signature_target(owner_symbol, qualifier, method_name, lookup_scope)
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
                param.type_clause_display,
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
                param.type_clause_display.clone(),
                None,
            );
        }
    }
}
