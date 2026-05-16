mod class;
mod context;
mod control;
mod decls;
mod emit;
mod exprs;
mod forms;
mod query;
mod sql;
mod state;
mod stmts;
mod token_expr;
mod traverse;

use std::sync::Arc;

use abap_ast::arena::NodeId;
use abap_ast::ast::{
    AstNode, CallArgList, CallExpr, ConstructorBaseClause, ConstructorExpr, ConstructorForClause,
    ConstructorLinesOfClause, DeclClause, MethodsParamSectionKind, StructuredIncludeClause,
    StructuredIncludeKind, TableExpr, TypeClauseKind, TypeRefSimple,
};
use abap_ast::{File, SyntaxKind};
use abap_lexer::{TextRange, Token, TokenKind};

use crate::builtins::builtin_routine_spec;
use crate::def_map::{
    AssignmentSiteData, CallSiteData, ClassDefinitionData, ClassInheritanceData, ClassMemberData,
    ConstructorForBindingData, Diagnostic, DiagnosticKind, ExpressionFactData, FieldAccess,
    FieldSymbolStateCheckData, FieldTypeRefData, FindSiteData, FormRoutineData, FunctionModuleData,
    ImplementedInterfaceData, IncludeEdge, InternalTableOrderData, LoopAtFieldContext,
    LoopWhereFieldContext, MemberAliasData, MessageClassEntryData, MessageClassUseData,
    MessageUseData, NamedArgumentAccess, PerformCallData, ReadTableBinarySearchData, ReferenceData,
    ReferenceKind, RoutineControlRegionData, RoutineSiteData, SqlDynamicFragmentData,
    SqlNameRefData, SqlPredicateData, SqlProjectionData, SqlQueryData, SqlSourceData,
    SqlTargetData, StructureData, StructureFieldData, SymbolData, SymbolKind,
    SystemFieldUpdateData, TableWorkAreaData, UnitAnalysis, ValueFlowEdgeData, ValueStateCheckData,
};
use crate::ids::{ScopeId, StructureId, SymbolId, UnitId};
use crate::scope::{Namespace, ScopeData, ScopeKind};
use context::CollectorContext;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ScopeLookupKey {
    namespace: Namespace,
    name: Arc<str>,
}

#[derive(Debug, Clone)]
struct PendingStructureField {
    name: Arc<str>,
    decl_range: TextRange,
    structure: Option<PendingStructure>,
    type_ref: Option<FieldTypeRefData>,
    is_key: bool,
    value_clause_display: Option<Arc<str>>,
}

#[derive(Debug, Clone)]
pub(super) struct PendingStructureInclude {
    type_ref: FieldTypeRefData,
    alias: Option<Arc<str>>,
    suffix: Option<Arc<str>>,
}

#[derive(Debug, Clone)]
enum PendingStructureMember {
    Field(PendingStructureField),
    Include(PendingStructureInclude),
}

#[derive(Debug, Clone)]
struct PendingStructure {
    name: Arc<str>,
    members: Vec<PendingStructureMember>,
}

#[derive(Debug, Clone)]
struct PendingMethodParameter {
    section: MethodsParamSectionKind,
    name: Arc<str>,
    range: TextRange,
    declared_type: Option<FieldTypeRefData>,
    type_clause_display: Option<Arc<str>>,
    is_optional: bool,
}

#[derive(Debug, Clone)]
struct PendingEventHandlerSpec {
    event_qualifier: Option<Arc<str>>,
    event_name: Arc<str>,
    source_type_name: Arc<str>,
    source_type_display: Arc<str>,
    importing_names: Vec<(Arc<str>, TextRange)>,
}

#[derive(Debug, Clone, Default)]
struct PendingMethodSignature {
    parameters: Vec<PendingMethodParameter>,
    is_redefinition: bool,
    event_handler: Option<PendingEventHandlerSpec>,
}

#[derive(Debug, Clone)]
pub(super) struct SyntaxTokenInfo {
    range: TextRange,
    text: Arc<str>,
    _index: usize,
    kind: TokenKind,
}

fn collect_message_class_entries(source: &str) -> Vec<MessageClassEntryData> {
    let Some(class_name) = message_class_name_from_source(source) else {
        return Vec::new();
    };
    let mut entries = Vec::new();
    let mut line_start = 0usize;
    for line in source.split_inclusive('\n') {
        let line_body = line.trim_end_matches(['\r', '\n']);
        if let Some((id, text, rel_start, rel_end)) = message_class_comment_entry(line_body) {
            entries.push(MessageClassEntryData {
                class_name: Arc::clone(&class_name),
                id: Arc::from(id),
                text: Arc::from(text),
                range: line_start + rel_start..line_start + rel_end,
            });
        }
        line_start += line.len();
    }
    entries
}

fn message_class_name_from_source(source: &str) -> Option<Arc<str>> {
    for line in source.lines() {
        let trimmed = line.trim_start();
        if !trimmed
            .get(..6)
            .is_some_and(|head| head.eq_ignore_ascii_case("TYPES "))
        {
            continue;
        }
        let rest = &trimmed[6..];
        let name = rest.split_ascii_whitespace().next()?;
        return Some(Arc::from(name.trim_end_matches('.').to_ascii_lowercase()));
    }
    None
}

fn message_class_comment_entry(line: &str) -> Option<(String, String, usize, usize)> {
    let quote = line.find('"')?;
    let after_quote = &line[quote + 1..];
    let message = after_quote.trim_start();
    let skipped = after_quote.len() - message.len();
    if !message
        .get(..8)
        .is_some_and(|head| head.eq_ignore_ascii_case("MESSAGE "))
    {
        return None;
    }
    let rest = &message[8..];
    let colon = rest.find(':')?;
    let id = rest[..colon].trim();
    if id.is_empty() || !id.chars().all(|ch| ch.is_ascii_digit()) {
        return None;
    }
    let text = rest[colon + 1..].trim();
    let id_rel_start = quote + 1 + skipped + "MESSAGE ".len() + rest[..colon].len()
        - rest[..colon].trim_start().len();
    let id_rel_end = id_rel_start + id.len();
    Some((
        normalize_message_id(id),
        text.to_string(),
        id_rel_start,
        id_rel_end,
    ))
}

fn normalize_message_id(id: &str) -> String {
    if id.chars().all(|ch| ch.is_ascii_digit()) && id.len() < 3 {
        format!("{id:0>3}")
    } else {
        id.to_ascii_lowercase()
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SqlClauseKind {
    Where,
    JoinOn,
    Having,
    ForAllEntries,
}

#[derive(Debug, Clone)]
struct LoopGroupContext {
    source_access: Option<FieldAccess>,
    target_access: Option<FieldAccess>,
    source_structure: Option<StructureId>,
    source_declared_type: Option<FieldTypeRefData>,
    source_type_clause_display: Option<Arc<str>>,
    allows_internal_table_line_selector: bool,
}

pub struct Collector<'a> {
    source: &'a str,
    file: &'a File,
    unit_id: UnitId,
    uri: Arc<str>,
    scopes: Vec<ScopeData>,
    symbols: Vec<SymbolData>,
    structures: Vec<StructureData>,
    references: Vec<ReferenceData>,
    message_default_class: Option<MessageClassUseData>,
    message_uses: Vec<MessageUseData>,
    diagnostics: Vec<Diagnostic>,
    include_edges: Vec<IncludeEdge>,
    table_work_areas: Vec<TableWorkAreaData>,
    selection_screen_report_type_positions: Vec<TextRange>,
    field_accesses: Vec<FieldAccess>,
    loop_where_field_contexts: Vec<LoopWhereFieldContext>,
    loop_at_field_contexts: Vec<LoopAtFieldContext>,
    constructor_for_bindings: Vec<ConstructorForBindingData>,
    class_members: Vec<ClassMemberData>,
    class_definition_symbols: std::collections::HashSet<SymbolId>,
    deferred_class_symbols: std::collections::HashSet<SymbolId>,
    interface_definition_symbols: std::collections::HashSet<SymbolId>,
    deferred_interface_symbols: std::collections::HashSet<SymbolId>,
    abstract_classes: std::collections::HashSet<SymbolId>,
    implemented_interfaces: Vec<ImplementedInterfaceData>,
    member_aliases: Vec<MemberAliasData>,
    form_routines: Vec<FormRoutineData>,
    function_modules: Vec<FunctionModuleData>,
    named_arguments: Vec<NamedArgumentAccess>,
    call_sites: Vec<CallSiteData>,
    assignment_sites: Vec<AssignmentSiteData>,
    expression_facts: Vec<ExpressionFactData>,
    value_flow_edges: Vec<ValueFlowEdgeData>,
    perform_calls: Vec<PerformCallData>,
    find_sites: Vec<FindSiteData>,
    system_field_updates: Vec<SystemFieldUpdateData>,
    routine_sites: Vec<RoutineSiteData>,
    field_symbol_state_checks: Vec<FieldSymbolStateCheckData>,
    value_state_checks: Vec<ValueStateCheckData>,
    routine_control_regions: Vec<RoutineControlRegionData>,
    internal_table_orders: Vec<InternalTableOrderData>,
    read_table_binary_searches: Vec<ReadTableBinarySearchData>,
    sql_queries: Vec<SqlQueryData>,
    sql_sources: Vec<SqlSourceData>,
    sql_dynamic_fragments: Vec<SqlDynamicFragmentData>,
    sql_projections: Vec<SqlProjectionData>,
    sql_name_refs: Vec<SqlNameRefData>,
    sql_predicates: Vec<SqlPredicateData>,
    sql_targets: Vec<SqlTargetData>,
    class_member_index:
        std::collections::HashMap<SymbolId, std::collections::HashMap<Arc<str>, usize>>,
    class_definition_scopes: std::collections::HashMap<SymbolId, ScopeId>,
    class_superclasses: std::collections::HashMap<SymbolId, Arc<str>>,
    class_method_signatures: std::collections::HashMap<
        SymbolId,
        std::collections::HashMap<Arc<str>, PendingMethodSignature>,
    >,
    scope_symbols: Vec<std::collections::HashMap<ScopeLookupKey, Vec<SymbolId>>>,
    /// `TYPE` vs `LIKE` for the innermost typed declaration clause being walked; drives whether
    /// simple names in `TypeRefSimple` (e.g. after `LINE OF`) resolve as types or data objects.
    type_clause_ns_stack: Vec<Namespace>,
    selection_screen_report_type_depth: usize,
    loop_group_stack: Vec<LoopGroupContext>,
}

impl<'a> Collector<'a> {
    pub fn new(
        unit_id: UnitId,
        uri: Arc<str>,
        source: &'a str,
        file: &'a File,
        _tokens: &'a [Token],
    ) -> Self {
        Self {
            source,
            file,
            unit_id,
            uri,
            scopes: Vec::new(),
            symbols: Vec::new(),
            structures: Vec::new(),
            references: Vec::new(),
            message_default_class: None,
            message_uses: Vec::new(),
            diagnostics: Vec::new(),
            include_edges: Vec::new(),
            table_work_areas: Vec::new(),
            selection_screen_report_type_positions: Vec::new(),
            field_accesses: Vec::new(),
            loop_where_field_contexts: Vec::new(),
            loop_at_field_contexts: Vec::new(),
            constructor_for_bindings: Vec::new(),
            class_members: Vec::new(),
            class_definition_symbols: std::collections::HashSet::new(),
            deferred_class_symbols: std::collections::HashSet::new(),
            interface_definition_symbols: std::collections::HashSet::new(),
            deferred_interface_symbols: std::collections::HashSet::new(),
            abstract_classes: std::collections::HashSet::new(),
            implemented_interfaces: Vec::new(),
            member_aliases: Vec::new(),
            form_routines: Vec::new(),
            function_modules: Vec::new(),
            named_arguments: Vec::new(),
            call_sites: Vec::new(),
            assignment_sites: Vec::new(),
            expression_facts: Vec::new(),
            value_flow_edges: Vec::new(),
            perform_calls: Vec::new(),
            find_sites: Vec::new(),
            system_field_updates: Vec::new(),
            routine_sites: Vec::new(),
            field_symbol_state_checks: Vec::new(),
            value_state_checks: Vec::new(),
            routine_control_regions: Vec::new(),
            internal_table_orders: Vec::new(),
            read_table_binary_searches: Vec::new(),
            sql_queries: Vec::new(),
            sql_sources: Vec::new(),
            sql_dynamic_fragments: Vec::new(),
            sql_projections: Vec::new(),
            sql_name_refs: Vec::new(),
            sql_predicates: Vec::new(),
            sql_targets: Vec::new(),
            class_member_index: std::collections::HashMap::new(),
            class_definition_scopes: std::collections::HashMap::new(),
            class_superclasses: std::collections::HashMap::new(),
            class_method_signatures: std::collections::HashMap::new(),
            scope_symbols: Vec::new(),
            type_clause_ns_stack: Vec::new(),
            selection_screen_report_type_depth: 0,
            loop_group_stack: Vec::new(),
        }
    }

    pub fn collect(mut self) -> UnitAnalysis {
        let root = self.file.root();
        let root_scope = self.push_scope(ScopeKind::File, self.file.range(root), None, None);
        self.install_builtin_symbols(root_scope);
        let mut ctx = self.context();
        traverse::walk_root(&mut ctx, root, root_scope);
        self.materialize_alias_members();
        let provided_names = self.provided_names();
        let message_class_entries = collect_message_class_entries(self.source);
        let class_inheritance = self
            .class_superclasses
            .into_iter()
            .map(|(class_symbol, superclass_name)| ClassInheritanceData {
                class_symbol,
                superclass_name,
            })
            .collect();
        let mut class_definitions: Vec<_> = self
            .class_definition_symbols
            .into_iter()
            .map(|class_symbol| ClassDefinitionData {
                class_symbol,
                is_abstract: self.abstract_classes.contains(&class_symbol),
            })
            .collect();
        class_definitions.sort_by_key(|definition| definition.class_symbol.as_usize());
        UnitAnalysis {
            unit_id: self.unit_id,
            uri: self.uri,
            root_scope,
            scopes: self.scopes,
            symbols: self.symbols,
            structures: self.structures,
            references: self.references,
            message_default_class: self.message_default_class,
            message_uses: self.message_uses,
            message_class_entries,
            diagnostics: self.diagnostics,
            include_edges: self.include_edges,
            table_work_areas: self.table_work_areas,
            selection_screen_report_type_positions: self.selection_screen_report_type_positions,
            field_accesses: self.field_accesses,
            loop_where_field_contexts: self.loop_where_field_contexts,
            loop_at_field_contexts: self.loop_at_field_contexts,
            constructor_for_bindings: self.constructor_for_bindings,
            class_members: self.class_members,
            class_definitions,
            class_inheritance,
            implemented_interfaces: self.implemented_interfaces,
            member_aliases: self.member_aliases,
            form_routines: self.form_routines,
            function_modules: self.function_modules,
            named_arguments: self.named_arguments,
            call_sites: self.call_sites,
            assignment_sites: self.assignment_sites,
            expression_facts: self.expression_facts,
            value_flow_edges: self.value_flow_edges,
            perform_calls: self.perform_calls,
            find_sites: self.find_sites,
            system_field_updates: self.system_field_updates,
            routine_sites: self.routine_sites,
            internal_table_orders: self.internal_table_orders,
            read_table_binary_searches: self.read_table_binary_searches,
            field_symbol_state_checks: self.field_symbol_state_checks,
            value_state_checks: self.value_state_checks,
            routine_control_regions: self.routine_control_regions,
            sql_queries: self.sql_queries,
            sql_sources: self.sql_sources,
            sql_dynamic_fragments: self.sql_dynamic_fragments,
            sql_projections: self.sql_projections,
            sql_name_refs: self.sql_name_refs,
            sql_predicates: self.sql_predicates,
            sql_targets: self.sql_targets,
            provided_names,
            semantic_index: Default::default(),
        }
        .with_semantic_index()
    }

    fn context(&mut self) -> CollectorContext<'_, 'a> {
        CollectorContext::new(self)
    }

    fn materialize_alias_members(&mut self) {
        let aliases = self.member_aliases.clone();
        for alias in aliases {
            if self.class_members.iter().any(|member| {
                member.class_symbol == alias.owner_symbol && member.name == alias.alias_name
            }) {
                continue;
            }

            let lookup_scope = self.symbol(alias.owner_symbol).scope;
            let Some(mut target_member) = self.class_member_target_data(
                alias.owner_symbol,
                Some(alias.target_interface_name.as_ref()),
                alias.target_member_name.as_ref(),
                lookup_scope,
            ) else {
                continue;
            };

            target_member.class_symbol = alias.owner_symbol;
            target_member.name = alias.alias_name.clone();
            target_member.decl_range = alias.range.clone();
            target_member.implementation_range = None;
            target_member.implementation = None;

            if target_member.kind == crate::ClassMemberKind::Method
                && let Some(signature) = self.class_method_signature_target(
                    alias.owner_symbol,
                    Some(alias.target_interface_name.as_ref()),
                    alias.target_member_name.as_ref(),
                    lookup_scope,
                )
            {
                let signature = signature.clone();
                self.class_method_signatures
                    .entry(alias.owner_symbol)
                    .or_default()
                    .insert(alias.alias_name.clone(), signature);
            }

            self.push_class_member(target_member);
        }
    }

    fn push_class_member(&mut self, member: ClassMemberData) {
        let index = self.class_members.len();
        self.class_member_index
            .entry(member.class_symbol)
            .or_default()
            .entry(Arc::clone(&member.name))
            .or_insert(index);
        self.class_members.push(member);
    }

    fn class_member_index(&self, class_symbol: SymbolId, name: &str) -> Option<usize> {
        self.class_member_index
            .get(&class_symbol)
            .and_then(|members| members.get(name))
            .copied()
    }

    fn node_has_structured_children(&self, node: NodeId) -> bool {
        self.file
            .children(node)
            .any(|child| self.file.kind(child) != SyntaxKind::Token)
    }

    fn inline_decl_inferred_type(
        &self,
        node: NodeId,
        scope: ScopeId,
    ) -> (Option<StructureId>, Option<FieldTypeRefData>) {
        if self.file.kind(node) == SyntaxKind::DataInlineDecl {
            let rhs_expr = self.file.children(node).find(|&child| {
                !matches!(
                    self.file.kind(child),
                    SyntaxKind::Token | SyntaxKind::DataDeclName
                )
            });
            if let Some(rhs_expr) = rhs_expr {
                let inferred = self.inline_decl_assignment_source_metadata(rhs_expr, scope);
                if inferred.0.is_some() || inferred.1.is_some() {
                    return inferred;
                }
            }
        }
        let mut stack = vec![node];
        while let Some(current) = stack.pop() {
            if self.file.kind(current) == SyntaxKind::ConstructorExpr {
                let inferred = self.constructor_expr_inferred_metadata(current, scope);
                if inferred.0.is_some() || inferred.1.is_some() {
                    return inferred;
                }
            }
            if self.file.kind(current) == SyntaxKind::SubstringExpr {
                let inferred = self.inline_decl_assignment_source_metadata(current, scope);
                if inferred.0.is_some() || inferred.1.is_some() {
                    return inferred;
                }
            }
            for child in self.file.children(current) {
                stack.push(child);
            }
        }
        (None, None)
    }

    fn constructor_expr_inferred_metadata(
        &self,
        node: NodeId,
        scope: ScopeId,
    ) -> (Option<StructureId>, Option<FieldTypeRefData>) {
        let keyword = self.constructor_keyword(node);
        if let Some(constructor) = ConstructorExpr::cast(self.syntax(node))
            && let Some(type_ref) = constructor.type_ref()
            && let Some(display_text) = type_ref.display_text(self.source)
            && display_text != "#"
            && let Some(mut declared_type) =
                self.field_type_ref_from_node(type_ref.syntax().id(), Namespace::Type)
        {
            declared_type.is_ref = keyword.as_deref() == Some("new");
            let structure = declared_type
                .field_path
                .is_empty()
                .then(|| {
                    self.lookup_structure_symbol(
                        scope,
                        declared_type.namespace,
                        declared_type.base_name.as_ref(),
                        false,
                    )
                    .and_then(|symbol_id| self.symbol(symbol_id).structure)
                })
                .flatten();
            return self.normalize_inferred_metadata(scope, structure, Some(declared_type));
        }

        if keyword.as_deref() == Some("value") {
            return self.value_constructor_inferred_metadata(node, scope);
        }

        (None, None)
    }

    fn inline_decl_assignment_source_metadata(
        &self,
        node: NodeId,
        scope: ScopeId,
    ) -> (Option<StructureId>, Option<FieldTypeRefData>) {
        let node = self.unwrap_simple_expr_wrapper(node);
        match self.file.kind(node) {
            SyntaxKind::CharStringTemplate => (
                None,
                Some(FieldTypeRefData {
                    namespace: Namespace::Type,
                    is_ref: false,
                    base_name: Arc::<str>::from("string"),
                    field_path: Vec::new(),
                }),
            ),
            SyntaxKind::TemplateExpr => {
                if let Some(child) = self.first_non_token_child(node) {
                    return self.inline_decl_assignment_source_metadata(child, scope);
                }
                let tokens = self.syntax_token_nodes(node);
                if tokens.len() == 1
                    && self.syntax_token_is_ident_like(&tokens[0])
                    && let Some(symbol_id) = self.lookup_symbol_in_scope_chain(
                        scope,
                        Namespace::Value,
                        tokens[0].text.as_ref(),
                    )
                {
                    let symbol = self.symbol(symbol_id);
                    return (symbol.structure, symbol.declared_type.clone());
                }
                (None, None)
            }
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
                (symbol.structure, symbol.declared_type.clone())
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
                    return (symbol.structure, symbol.declared_type.clone());
                }
                let mut structure = self.symbol(symbol_id).structure;
                let mut declared_type = self.symbol(symbol_id).declared_type.clone();
                for (idx, segment) in field_path.iter().enumerate() {
                    if segment.is_deref() {
                        return (None, None);
                    }
                    if let Some(structure_id) = structure {
                        let Some(field) = self.structure(structure_id).and_then(|structure| {
                            structure
                                .fields
                                .iter()
                                .find(|field| field.name.as_ref() == segment.name.as_ref())
                        }) else {
                            return (None, None);
                        };
                        structure = field.structure;
                        declared_type = field.type_ref.clone();
                        continue;
                    }

                    let Some(type_ref) = declared_type.as_mut() else {
                        return (None, None);
                    };
                    if type_ref.namespace != Namespace::Type || type_ref.is_ref {
                        return (None, None);
                    }
                    type_ref.field_path.push(Arc::clone(&segment.name));
                    for trailing in field_path.iter().skip(idx + 1) {
                        if trailing.is_deref() {
                            return (None, None);
                        }
                        type_ref.field_path.push(Arc::clone(&trailing.name));
                    }
                    return (None, declared_type);
                }
                (structure, declared_type)
            }
            SyntaxKind::TableExpr => {
                let Some(base) = TableExpr::cast(self.syntax(node)).and_then(|expr| expr.base())
                else {
                    return (None, None);
                };
                let (structure, declared_type) =
                    self.inline_decl_assignment_source_metadata(base.id(), scope);
                self.internal_table_line_metadata(scope, structure, declared_type)
            }
            SyntaxKind::CallExpr => self.call_expr_inferred_metadata(node, scope),
            SyntaxKind::ExprLiteral => {
                let Some(token) = self.syntax_token_nodes(node).into_iter().next() else {
                    return (None, None);
                };
                let type_name = match token.kind {
                    TokenKind::String | TokenKind::StringTemplate => "string",
                    TokenKind::Number => "i",
                    _ => return (None, None),
                };
                (
                    None,
                    Some(FieldTypeRefData {
                        namespace: Namespace::Type,
                        is_ref: false,
                        base_name: Arc::<str>::from(type_name),
                        field_path: Vec::new(),
                    }),
                )
            }
            SyntaxKind::SubstringExpr => {
                let Some(base) = self.first_non_token_child(node) else {
                    return (None, None);
                };
                let (structure, declared_type) =
                    self.inline_decl_assignment_source_metadata(base, scope);
                let Some(base_type) = declared_type.as_ref() else {
                    return (structure, declared_type);
                };
                if base_type.namespace != Namespace::Type
                    || base_type.is_ref
                    || !base_type.field_path.is_empty()
                {
                    return (structure, declared_type);
                }
                let result_type = if base_type.base_name.as_ref().eq_ignore_ascii_case("xstring") {
                    "xstring"
                } else {
                    "string"
                };
                (
                    None,
                    Some(FieldTypeRefData {
                        namespace: Namespace::Type,
                        is_ref: false,
                        base_name: Arc::<str>::from(result_type),
                        field_path: Vec::new(),
                    }),
                )
            }
            _ => self.token_expr_inferred_metadata(&self.syntax_token_nodes(node), scope),
        }
    }

    fn token_expr_inferred_metadata(
        &self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
    ) -> (Option<StructureId>, Option<FieldTypeRefData>) {
        let tokens: Vec<_> = tokens
            .iter()
            .filter(|token| !self.syntax_token_is_comment(token))
            .cloned()
            .collect();
        let Some(first) = tokens.first() else {
            return (None, None);
        };

        if tokens.len() == 1 {
            let text = first.text.as_ref();
            if text.chars().all(|ch| ch.is_ascii_digit()) {
                return (
                    None,
                    Some(FieldTypeRefData {
                        namespace: Namespace::Type,
                        is_ref: false,
                        base_name: Arc::<str>::from("i"),
                        field_path: Vec::new(),
                    }),
                );
            }
            if self.syntax_token_is_ident_like(first)
                && let Some(symbol_id) =
                    self.lookup_symbol_in_scope_chain(scope, Namespace::Value, text)
            {
                let symbol = self.symbol(symbol_id);
                return self.normalize_inferred_metadata(
                    scope,
                    symbol.structure,
                    symbol.declared_type.clone(),
                );
            }
        }

        let plus_indices = self.top_level_sum_indices(&tokens);
        if plus_indices.is_empty() {
            return (None, None);
        }

        let mut operand_start = 0usize;
        let mut declared_type: Option<FieldTypeRefData> = None;
        for plus_idx in plus_indices
            .into_iter()
            .chain(std::iter::once(tokens.len()))
        {
            let operand =
                self.token_expr_inferred_metadata(&tokens[operand_start..plus_idx], scope);
            let Some(operand_type) = operand.1 else {
                return (None, None);
            };
            if operand_type.namespace != Namespace::Type
                || operand_type.is_ref
                || !operand_type.field_path.is_empty()
            {
                return (None, None);
            }
            if let Some(existing_type) = declared_type.as_ref() {
                if existing_type != &operand_type {
                    return (None, None);
                }
            } else {
                declared_type = Some(operand_type);
            }
            operand_start = plus_idx + 1;
        }

        self.normalize_inferred_metadata(scope, None, declared_type)
    }

    fn internal_table_line_metadata(
        &self,
        scope: ScopeId,
        structure: Option<StructureId>,
        declared_type: Option<FieldTypeRefData>,
    ) -> (Option<StructureId>, Option<FieldTypeRefData>) {
        let Some(type_ref) = declared_type.as_ref() else {
            return (structure, declared_type);
        };
        if type_ref.namespace != Namespace::Type
            || type_ref.is_ref
            || !type_ref.field_path.is_empty()
        {
            return (structure, declared_type);
        }
        let Some(type_symbol_id) =
            self.lookup_symbol_in_scope_chain(scope, Namespace::Type, type_ref.base_name.as_ref())
        else {
            return (structure, declared_type);
        };
        let type_symbol = self.symbol(type_symbol_id);
        if !type_symbol
            .type_clause_display
            .as_deref()
            .is_some_and(is_internal_table_type_display)
        {
            return (structure, declared_type);
        }
        (
            type_symbol.structure.or(structure),
            type_symbol.declared_type.clone(),
        )
    }

    fn rhs_is_top_level_sum(&self, node: NodeId) -> bool {
        !self
            .top_level_sum_indices(&self.syntax_token_nodes(node))
            .is_empty()
    }

    fn top_level_sum_indices(&self, tokens: &[SyntaxTokenInfo]) -> Vec<usize> {
        let mut plus_indices = Vec::new();
        let mut paren_depth = 0i32;
        let mut bracket_depth = 0i32;
        let mut brace_depth = 0i32;
        for (idx, token) in tokens.iter().enumerate() {
            if self.syntax_token_is_comment(token) {
                continue;
            }
            match token.text.as_ref() {
                "(" => paren_depth += 1,
                ")" => paren_depth -= 1,
                "[" => bracket_depth += 1,
                "]" => bracket_depth -= 1,
                "{" => brace_depth += 1,
                "}" => brace_depth -= 1,
                "+" if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 => {
                    plus_indices.push(idx);
                }
                _ => {}
            }
        }
        plus_indices
    }

    fn call_expr_inferred_metadata(
        &self,
        node: NodeId,
        scope: ScopeId,
    ) -> (Option<StructureId>, Option<FieldTypeRefData>) {
        let Some(call) = CallExpr::cast(self.syntax(node)) else {
            return (None, None);
        };
        let Some(callee) = call.callee() else {
            return (None, None);
        };

        match callee.kind() {
            SyntaxKind::ExprIdent => {
                let Some((name, _)) = self.node_name(callee.id()) else {
                    return (None, None);
                };
                if let Some(spec) = builtin_routine_spec(name.as_ref()) {
                    return self.normalize_inferred_metadata(
                        scope,
                        None,
                        Some(FieldTypeRefData {
                            namespace: Namespace::Type,
                            is_ref: false,
                            base_name: Arc::from(spec.return_type),
                            field_path: Vec::new(),
                        }),
                    );
                }

                let Some(owner_symbol) = self.enclosing_class_owner(scope) else {
                    return (None, None);
                };
                let Some(signature) =
                    self.class_method_signature_target(owner_symbol, None, name.as_ref(), scope)
                else {
                    return (None, None);
                };
                self.method_return_inferred_metadata(scope, signature)
            }
            _ => {
                let Some(target) = self.named_argument_target_for_callee(callee.id()) else {
                    return (None, None);
                };
                let crate::def_map::NamedArgumentTarget::Method {
                    base_namespace,
                    base_name,
                    method_name,
                    interface_qualified,
                } = target
                else {
                    return (None, None);
                };
                if interface_qualified {
                    let Some(owner_symbol) = self.enclosing_class_owner(scope) else {
                        return (None, None);
                    };
                    let Some(signature) = self.class_method_signature_target(
                        owner_symbol,
                        Some(base_name.as_ref()),
                        method_name.as_ref(),
                        scope,
                    ) else {
                        return (None, None);
                    };
                    return self.method_return_inferred_metadata(scope, signature);
                }
                let Some(class_symbol) =
                    self.inline_call_target_class_symbol(scope, base_namespace, &base_name)
                else {
                    return (None, None);
                };
                let Some(signature) = self.class_method_signature_target(
                    class_symbol,
                    None,
                    method_name.as_ref(),
                    scope,
                ) else {
                    return (None, None);
                };
                self.method_return_inferred_metadata(scope, signature)
            }
        }
    }

    fn method_return_inferred_metadata(
        &self,
        scope: ScopeId,
        signature: &PendingMethodSignature,
    ) -> (Option<StructureId>, Option<FieldTypeRefData>) {
        let declared_type = signature
            .parameters
            .iter()
            .find(|param| {
                matches!(
                    param.section,
                    MethodsParamSectionKind::Returning | MethodsParamSectionKind::Receiving
                )
            })
            .and_then(|param| param.declared_type.clone());
        self.normalize_inferred_metadata(scope, None, declared_type)
    }

    fn inline_call_target_class_symbol(
        &self,
        scope: ScopeId,
        base_namespace: Namespace,
        base_name: &Arc<str>,
    ) -> Option<SymbolId> {
        match base_namespace {
            Namespace::Type => self
                .lookup_symbol_in_scope_chain(scope, Namespace::Type, base_name.as_ref())
                .filter(|&symbol_id| self.symbol(symbol_id).kind == crate::SymbolKind::Class),
            Namespace::Value => {
                let symbol_id =
                    self.lookup_symbol_in_scope_chain(scope, Namespace::Value, base_name.as_ref())?;
                let declared_type = self.symbol(symbol_id).declared_type.as_ref()?;
                if !declared_type.is_ref || !declared_type.field_path.is_empty() {
                    return None;
                }
                self.lookup_symbol_in_scope_chain(
                    scope,
                    declared_type.namespace,
                    declared_type.base_name.as_ref(),
                )
                .filter(|&class_symbol_id| {
                    self.symbol(class_symbol_id).kind == crate::SymbolKind::Class
                })
            }
            Namespace::Routine => None,
        }
    }

    fn constructor_keyword(&self, node: NodeId) -> Option<Arc<str>> {
        self.file
            .children(node)
            .find(|&child| self.file.kind(child) == SyntaxKind::Token)
            .and_then(|child| self.syntax(child).text(self.source))
            .map(|text| Arc::<str>::from(text.to_ascii_lowercase()))
    }

    fn value_constructor_inferred_metadata(
        &self,
        node: NodeId,
        scope: ScopeId,
    ) -> (Option<StructureId>, Option<FieldTypeRefData>) {
        let Some(arg_list) = self
            .syntax(node)
            .child_by_kind(SyntaxKind::CallArgList)
            .and_then(CallArgList::cast)
        else {
            return (None, None);
        };

        if let Some(base_value) = self
            .file
            .find_first_kind(arg_list.syntax().id(), SyntaxKind::ConstructorBaseClause)
            .and_then(|node| ConstructorBaseClause::cast(self.syntax(node)))
            .and_then(|clause| clause.value())
        {
            let inferred = self.inline_decl_assignment_source_metadata(base_value.id(), scope);
            if inferred.0.is_some() || inferred.1.is_some() {
                return inferred;
            }
        }

        if let Some(source) = self
            .file
            .find_first_kind(arg_list.syntax().id(), SyntaxKind::ConstructorLinesOfClause)
            .and_then(|node| ConstructorLinesOfClause::cast(self.syntax(node)))
            .and_then(|clause| clause.source())
        {
            let inferred = self.inline_decl_assignment_source_metadata(source.id(), scope);
            if inferred.0.is_some() || inferred.1.is_some() {
                return inferred;
            }
        }

        if let Some(source) = self
            .file
            .find_first_kind(arg_list.syntax().id(), SyntaxKind::ConstructorForClause)
            .and_then(|node| ConstructorForClause::cast(self.syntax(node)))
            .and_then(|clause| clause.source_expr(self.source))
        {
            let inferred = self.inline_decl_assignment_source_metadata(source.id(), scope);
            if inferred.0.is_some() || inferred.1.is_some() {
                return inferred;
            }
        }

        for positional in arg_list.positional_args() {
            let non_token_children: Vec<_> = positional
                .value_children()
                .into_iter()
                .filter(|child| child.kind() != SyntaxKind::Token)
                .collect();
            if let [single] = non_token_children.as_slice() {
                let inferred = self.inline_decl_assignment_source_metadata(single.id(), scope);
                if inferred.0.is_some() || inferred.1.is_some() {
                    return inferred;
                }
            }
            let tokens = positional
                .value_children()
                .into_iter()
                .flat_map(|child| self.syntax_token_nodes(child.id()))
                .collect::<Vec<_>>();
            if let Some(metadata) = self.value_constructor_metadata_from_tokens(&tokens, scope) {
                return metadata;
            }
        }

        (None, None)
    }

    fn value_constructor_metadata_from_tokens(
        &self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
    ) -> Option<(Option<StructureId>, Option<FieldTypeRefData>)> {
        let mut idx = 0usize;
        while idx < tokens.len() {
            let token = &tokens[idx];
            if self.syntax_token_is_comment(token) {
                idx += 1;
                continue;
            }

            if token.text.eq_ignore_ascii_case("base") {
                let operand_start = idx + 1;
                let operand_end = self.value_base_operand_end(tokens, operand_start);
                return self
                    .metadata_from_value_source_tokens(&tokens[operand_start..operand_end], scope);
            }

            if token.text.eq_ignore_ascii_case("lines")
                && tokens
                    .get(idx + 1)
                    .is_some_and(|next| next.text.eq_ignore_ascii_case("of"))
            {
                let source_start = idx + 2;
                let source_end = self.value_lines_of_source_end(tokens, source_start);
                return self
                    .metadata_from_value_source_tokens(&tokens[source_start..source_end], scope);
            }

            if token.text.eq_ignore_ascii_case("for") {
                let source_start = idx + 3;
                let source_end = self.value_for_source_end(tokens, source_start);
                return self
                    .metadata_from_value_source_tokens(&tokens[source_start..source_end], scope);
            }

            if self.syntax_token_is_ident_like(token) {
                return self.metadata_from_value_source_tokens(&tokens[idx..], scope);
            }

            idx += 1;
        }
        None
    }

    fn metadata_from_value_source_tokens(
        &self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
    ) -> Option<(Option<StructureId>, Option<FieldTypeRefData>)> {
        let first = tokens
            .iter()
            .find(|token| !self.syntax_token_is_comment(token))?;
        if !self.syntax_token_is_ident_like(first) {
            return None;
        }
        let symbol_id =
            self.lookup_symbol_in_scope_chain(scope, Namespace::Value, first.text.as_ref())?;
        let symbol = self.symbol(symbol_id);
        Some(self.normalize_inferred_metadata(
            scope,
            symbol.structure,
            symbol.declared_type.clone(),
        ))
    }

    fn value_base_operand_end(&self, tokens: &[SyntaxTokenInfo], start: usize) -> usize {
        let mut idx = start;
        while idx < tokens.len() {
            let token = &tokens[idx];
            if self.syntax_token_is_comment(token) {
                idx += 1;
                continue;
            }
            if token.text.eq_ignore_ascii_case("for") {
                break;
            }
            if token.text.as_ref() == "("
                && idx > start
                && tokens
                    .get(idx - 1)
                    .is_some_and(|prev| self.syntax_tokens_have_space_between(prev, token))
            {
                break;
            }
            if self.syntax_token_is_ident_like(token)
                && tokens.get(idx + 1).map(|next| next.text.as_ref()) == Some("=")
            {
                break;
            }
            idx += 1;
        }
        idx
    }

    fn value_for_source_end(&self, tokens: &[SyntaxTokenInfo], start: usize) -> usize {
        let mut idx = start;
        while idx < tokens.len() {
            let token = &tokens[idx];
            if self.syntax_token_is_comment(token) {
                idx += 1;
                continue;
            }
            if token.text.eq_ignore_ascii_case("for")
                || token.text.eq_ignore_ascii_case("let")
                || token.text.eq_ignore_ascii_case("where")
                || token.text.eq_ignore_ascii_case("until")
                || token.text.eq_ignore_ascii_case("while")
            {
                break;
            }
            if token.text.as_ref() == "("
                && idx > start
                && tokens
                    .get(idx - 1)
                    .is_some_and(|prev| self.syntax_tokens_have_space_between(prev, token))
            {
                break;
            }
            idx += 1;
        }
        idx
    }

    fn value_lines_of_source_end(&self, tokens: &[SyntaxTokenInfo], start: usize) -> usize {
        let mut idx = start;
        while idx < tokens.len() {
            let token = &tokens[idx];
            if self.syntax_token_is_comment(token) {
                idx += 1;
                continue;
            }
            if token.text.eq_ignore_ascii_case("from")
                || token.text.eq_ignore_ascii_case("to")
                || token.text.eq_ignore_ascii_case("using")
            {
                break;
            }
            if token.text.as_ref() == "("
                && idx > start
                && tokens
                    .get(idx - 1)
                    .is_some_and(|prev| self.syntax_tokens_have_space_between(prev, token))
            {
                break;
            }
            idx += 1;
        }
        idx
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

    fn significant_stmt_token_infos(&self, node: NodeId) -> Vec<SyntaxTokenInfo> {
        let mut tokens = Vec::new();
        for child in self.file.children(node) {
            for token in self.syntax_token_nodes(child) {
                if !self.syntax_token_is_comment(&token) {
                    tokens.push(token);
                }
            }
        }
        tokens
    }

    fn render_token_infos(&self, tokens: &[SyntaxTokenInfo]) -> String {
        let mut rendered = String::new();
        let mut prev_text: Option<&str> = None;
        for token in tokens {
            if self.syntax_token_is_comment(token) {
                continue;
            }
            let text = token.text.as_ref();
            let needs_space = !rendered.is_empty()
                && !matches!(text, "," | ":" | "-" | ")" | "]")
                && !matches!(prev_text, Some("(" | "[" | ":" | "-"));
            if needs_space {
                rendered.push(' ');
            }
            rendered.push_str(text);
            prev_text = Some(text);
        }
        rendered
    }

    fn syntax_token_is_comment(&self, token: &SyntaxTokenInfo) -> bool {
        token.kind == TokenKind::Comment
    }

    fn syntax_token_is_ident_like(&self, token: &SyntaxTokenInfo) -> bool {
        !matches!(
            token.text.as_ref(),
            ":" | ","
                | "."
                | "-"
                | "+"
                | "*"
                | "/"
                | "("
                | ")"
                | "["
                | "]"
                | "{"
                | "}"
                | "="
                | "<"
                | ">"
                | "<="
                | ">="
                | "<>"
                | "?="
                | "->"
                | "=>"
                | "~"
                | "#"
                | "@"
                | "&"
                | "|"
        ) && !self.syntax_token_is_comment(token)
    }

    fn syntax_token_is_literal_like(&self, token: &SyntaxTokenInfo) -> bool {
        token
            .text
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_digit() || matches!(ch, '\'' | '`' | '|'))
    }

    fn begin_of_clause_parts(
        &self,
        node: NodeId,
        scope: ScopeId,
    ) -> Option<(Arc<str>, TextRange, Vec<PendingStructureMember>)> {
        let name_range = self.structured_decl_name_range(node)?;
        let structure = self.pending_structure_from_node(node, scope)?;
        Some((structure.name, name_range, structure.members))
    }

    fn structured_decl_name_pair(
        &self,
        node: NodeId,
    ) -> Option<((Arc<str>, TextRange), (Arc<str>, TextRange))> {
        let tokens = self.structured_decl_token_nodes(node);
        let begin_tok = tokens.first().copied()?;
        let of_tok = tokens.get(1).copied()?;
        let name_tok = self.structured_decl_name_token(&tokens)?;
        let begin_text = self.syntax(begin_tok).text(self.source)?;
        let of_text = self.syntax(of_tok).text(self.source)?;
        if !begin_text.eq_ignore_ascii_case("begin") || !of_text.eq_ignore_ascii_case("of") {
            return None;
        }
        let (begin_name, begin_range) = self.node_name(name_tok)?;
        if !self.syntax_token_is_identifier_node(name_tok) {
            return None;
        }

        let end_name_tok = self.structured_decl_end_name_token(&tokens)?;
        if !self.syntax_token_is_identifier_node(end_name_tok) {
            return None;
        }
        let (end_name, end_range) = self.node_name(end_name_tok)?;
        Some(((begin_name, begin_range), (end_name, end_range)))
    }

    fn syntax_token_is_identifier_node(&self, node: NodeId) -> bool {
        self.file.kind(node) == SyntaxKind::Token
            && self
                .syntax(node)
                .token_kind()
                .is_some_and(|kind| kind == TokenKind::Ident)
    }

    fn check_structured_decl_end_name(&mut self, node: NodeId) -> bool {
        let Some(((begin_name, _), (end_name, end_range))) = self.structured_decl_name_pair(node)
        else {
            return false;
        };
        if begin_name.eq_ignore_ascii_case(end_name.as_ref()) {
            return true;
        }
        self.diagnostics.push(Diagnostic {
            kind: DiagnosticKind::MismatchedStructuredDeclaration,
            range: end_range,
            message: format!(
                "structured declaration ends with '{}', but began with '{}'",
                end_name, begin_name
            ),
        });
        false
    }

    fn add_structured_decl_end_reference(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        kind: SymbolKind,
    ) {
        if !self.check_structured_decl_end_name(node) {
            return;
        }
        let Some((_, (end_name, end_range))) = self.structured_decl_name_pair(node) else {
            return;
        };
        let Some(namespace) = kind.namespaces().first().copied() else {
            return;
        };
        self.add_reference(
            scope,
            end_name,
            namespace,
            ReferenceKind::StructuredDeclEnd,
            end_range,
        );
    }

    fn structured_decl_name_range(&self, node: NodeId) -> Option<TextRange> {
        let tokens = self.structured_decl_token_nodes(node);
        let begin_tok = tokens.first().copied()?;
        let of_tok = tokens.get(1).copied()?;
        let name_tok = self.structured_decl_name_token(&tokens)?;
        let part_tok = tokens.get(3).copied();
        let begin_text = self.syntax(begin_tok).text(self.source)?;
        let of_text = self.syntax(of_tok).text(self.source)?;
        let name_text = self.syntax(name_tok).text(self.source)?;
        if !begin_text.eq_ignore_ascii_case("begin")
            || !of_text.eq_ignore_ascii_case("of")
            || (name_text.eq_ignore_ascii_case("common")
                && part_tok
                    .and_then(|token| self.syntax(token).text(self.source))
                    .is_some_and(|text| text.eq_ignore_ascii_case("part")))
            || matches!(name_text, "." | "," | ":" | "-")
        {
            return None;
        }
        Some(self.file.range(name_tok))
    }

    fn pending_structure_from_node(
        &self,
        node: NodeId,
        scope: ScopeId,
    ) -> Option<PendingStructure> {
        let tokens = self.structured_decl_token_nodes(node);
        let (name, _) = self.node_name(self.structured_decl_name_token(&tokens)?)?;
        let mut members = Vec::new();
        for child in self.file.children(node) {
            match self.file.kind(child) {
                SyntaxKind::StructuredDecl => {
                    let nested = self.pending_structure_from_node(child, scope)?;
                    let decl_range = self.structured_decl_name_range(child)?;
                    members.push(PendingStructureMember::Field(PendingStructureField {
                        name: Arc::clone(&nested.name),
                        decl_range,
                        structure: Some(nested),
                        type_ref: None,
                        is_key: false,
                        value_clause_display: None,
                    }));
                }
                SyntaxKind::StructuredFieldClause | SyntaxKind::TypesTypedClause => {
                    let field = self.pending_structure_field_from_clause(child, scope)?;
                    members.push(PendingStructureMember::Field(field));
                }
                SyntaxKind::StructuredIncludeClause => {
                    let include = self.pending_structure_include_from_clause(child)?;
                    members.push(PendingStructureMember::Include(include));
                }
                _ => {}
            }
        }
        Some(PendingStructure { name, members })
    }

    fn structured_decl_token_nodes(&self, node: NodeId) -> Vec<NodeId> {
        self.file
            .children(node)
            .filter(|&child| {
                self.file.kind(child) == SyntaxKind::Token
                    && self.syntax(child).token_kind() != Some(TokenKind::Comment)
            })
            .collect()
    }

    fn structured_decl_marker(&self, tokens: &[NodeId]) -> Option<&'static str> {
        let marker = tokens.get(2).copied()?;
        let name = tokens.get(3).copied()?;
        if !self.syntax_token_is_identifier_node(name) {
            return None;
        }
        let marker_text = self.syntax(marker).text(self.source)?;
        if marker_text.eq_ignore_ascii_case("enum") {
            Some("enum")
        } else if marker_text.eq_ignore_ascii_case("mesh") {
            Some("mesh")
        } else {
            None
        }
    }

    fn structured_decl_name_token(&self, tokens: &[NodeId]) -> Option<NodeId> {
        if self.structured_decl_marker(tokens).is_some() {
            tokens.get(3).copied()
        } else {
            tokens.get(2).copied()
        }
    }

    fn structured_decl_end_name_token(&self, tokens: &[NodeId]) -> Option<NodeId> {
        if let Some(marker) = self.structured_decl_marker(tokens) {
            return tokens.windows(4).rev().find_map(|window| {
                let end_text = self.syntax(window[0]).text(self.source)?;
                let of_text = self.syntax(window[1]).text(self.source)?;
                let marker_text = self.syntax(window[2]).text(self.source)?;
                (end_text.eq_ignore_ascii_case("end")
                    && of_text.eq_ignore_ascii_case("of")
                    && marker_text.eq_ignore_ascii_case(marker))
                .then_some(window[3])
            });
        }
        tokens.windows(3).rev().find_map(|window| {
            let end_text = self.syntax(window[0]).text(self.source)?;
            let of_text = self.syntax(window[1]).text(self.source)?;
            (end_text.eq_ignore_ascii_case("end") && of_text.eq_ignore_ascii_case("of"))
                .then_some(window[2])
        })
    }

    fn pending_structure_field_from_clause(
        &self,
        node: NodeId,
        _scope: ScopeId,
    ) -> Option<PendingStructureField> {
        let clause = DeclClause::cast(self.syntax(node))?;
        let name_node = clause.name()?;
        let name = name_node.name(self.source)?;
        let decl_range = name_node.range();
        Some(PendingStructureField {
            name,
            decl_range,
            structure: None,
            type_ref: self.type_ref_from_typed_clause(node),
            is_key: self.structured_field_clause_has_key_comment(node),
            value_clause_display: self.value_clause_display_from_typed_clause(node),
        })
    }

    fn pending_structure_include_from_clause(
        &self,
        node: NodeId,
    ) -> Option<PendingStructureInclude> {
        let clause = StructuredIncludeClause::cast(self.syntax(node))?;
        let namespace = match clause.kind(self.source)? {
            StructuredIncludeKind::Type => Namespace::Type,
            StructuredIncludeKind::Structure => Namespace::Value,
        };
        let type_ref = clause.type_ref().and_then(|type_ref| {
            self.field_type_ref_from_node(type_ref.syntax().id(), namespace)
        })?;
        Some(PendingStructureInclude {
            type_ref,
            alias: clause.alias_name(self.source),
            suffix: clause.suffix(self.source),
        })
    }

    fn push_range_table_structure(
        &mut self,
        scope: ScopeId,
        name_suffix: &str,
        low_high_type: FieldTypeRefData,
    ) -> StructureId {
        let sign_type = FieldTypeRefData {
            namespace: Namespace::Type,
            is_ref: false,
            base_name: Arc::from("ddsign"),
            field_path: Vec::new(),
        };
        let option_type = FieldTypeRefData {
            namespace: Namespace::Type,
            is_ref: false,
            base_name: Arc::from("ddoption"),
            field_path: Vec::new(),
        };
        self.push_structure(
            Arc::from(format!("<range:{name_suffix}>")),
            [
                StructureFieldData {
                    name: Arc::from("sign"),
                    decl_range: None,
                    decl_unit: self.unit_id,
                    structure: self.resolve_field_type_ref(scope, &sign_type),
                    type_ref: Some(sign_type),
                    is_key: false,
                    value_clause_display: None,
                },
                StructureFieldData {
                    name: Arc::from("option"),
                    decl_range: None,
                    decl_unit: self.unit_id,
                    structure: self.resolve_field_type_ref(scope, &option_type),
                    type_ref: Some(option_type),
                    is_key: false,
                    value_clause_display: None,
                },
                StructureFieldData {
                    name: Arc::from("low"),
                    decl_range: None,
                    decl_unit: self.unit_id,
                    structure: self.resolve_field_type_ref(scope, &low_high_type),
                    type_ref: Some(low_high_type.clone()),
                    is_key: false,
                    value_clause_display: None,
                },
                StructureFieldData {
                    name: Arc::from("high"),
                    decl_range: None,
                    decl_unit: self.unit_id,
                    structure: self.resolve_field_type_ref(scope, &low_high_type),
                    type_ref: Some(low_high_type),
                    is_key: false,
                    value_clause_display: None,
                },
            ],
        )
    }

    fn direct_type_ref_children(&self, node: NodeId) -> Vec<NodeId> {
        self.syntax(node)
            .children()
            .filter_map(TypeRefSimple::cast)
            .map(|type_ref| type_ref.syntax().id())
            .collect()
    }

    fn field_type_ref_from_node(
        &self,
        node: NodeId,
        namespace: Namespace,
    ) -> Option<FieldTypeRefData> {
        let (_, is_ref, base_name, _, field_path) = self.type_ref_access_chain(node, namespace)?;
        Some(FieldTypeRefData {
            namespace,
            is_ref,
            base_name,
            field_path: field_path.into_iter().map(|segment| segment.name).collect(),
        })
    }

    fn type_clause_display_from_typed_clause(&self, node: NodeId) -> Option<Arc<str>> {
        let clause = DeclClause::cast(self.syntax(node))?;
        let (type_ref, _) = clause.type_ref_with_namespace(self.source)?;
        let rendered = self.render_type_ref_display(type_ref.syntax().id())?;
        match clause.type_clause_kind(self.source) {
            Some(TypeClauseKind::For) => Some(Arc::from(format!("RANGE OF {rendered}"))),
            Some(TypeClauseKind::Like) if self.typed_clause_uses_line_of(node) => {
                if rendered
                    .as_ref()
                    .to_ascii_uppercase()
                    .starts_with("LINE OF ")
                {
                    Some(rendered)
                } else {
                    Some(Arc::from(format!("LINE OF {rendered}")))
                }
            }
            _ => Some(rendered),
        }
    }

    fn typed_clause_uses_line_of(&self, node: NodeId) -> bool {
        let tokens = self
            .syntax_token_nodes(node)
            .into_iter()
            .filter(|token| !self.syntax_token_is_comment(token))
            .collect::<Vec<_>>();
        tokens.windows(3).any(|window| {
            window[0].text.eq_ignore_ascii_case("like")
                && window[1].text.eq_ignore_ascii_case("line")
                && window[2].text.eq_ignore_ascii_case("of")
        })
    }

    fn value_clause_display_from_typed_clause(&self, node: NodeId) -> Option<Arc<str>> {
        let clause = DeclClause::cast(self.syntax(node))?;
        let value_clause = clause.syntax().child_by_kind(SyntaxKind::ValueClause)?;
        let tokens = self.syntax_token_nodes(value_clause.id());
        let rendered = self.render_token_infos(
            tokens
                .get(1..)
                .unwrap_or_default()
                .iter()
                .filter(|token| !self.syntax_token_is_comment(token))
                .cloned()
                .collect::<Vec<_>>()
                .as_slice(),
        );
        (!rendered.is_empty()).then(|| Arc::from(rendered))
    }

    fn structured_field_clause_has_key_comment(&self, node: NodeId) -> bool {
        let range = self.file.range(node);
        let Some(after_clause) = self.source.get(range.end..) else {
            return false;
        };
        let line_tail = after_clause
            .split_once('\n')
            .map(|(line, _)| line)
            .unwrap_or(after_clause);
        let Some((_, comment)) = line_tail.split_once('"') else {
            return false;
        };
        let comment = comment.trim_start();
        let lowered = comment.to_ascii_lowercase();
        matches!(lowered.as_str(), "key" | "key field" | "primary key")
            || lowered.starts_with("key;")
            || lowered.starts_with("key field;")
            || lowered.starts_with("primary key;")
    }

    fn typed_clause_type_ref_node(&self, node: NodeId) -> Option<(NodeId, Namespace)> {
        let clause = DeclClause::cast(self.syntax(node))?;
        let (type_ref, namespace) = clause.type_ref_with_namespace(self.source)?;
        Some((
            type_ref.syntax().id(),
            self.namespace_from_type_clause_kind(namespace),
        ))
    }

    fn typed_clause_namespace_hint(&self, clause_node: NodeId) -> Option<Namespace> {
        let clause = DeclClause::cast(self.syntax(clause_node))?;
        let kind = clause.type_clause_kind(self.source)?;
        Some(self.namespace_from_type_clause_kind(kind))
    }

    fn structured_include_namespace_hint(&self, clause_node: NodeId) -> Option<Namespace> {
        let clause = StructuredIncludeClause::cast(self.syntax(clause_node))?;
        let kind = clause.kind(self.source)?;
        Some(match kind {
            StructuredIncludeKind::Type => Namespace::Type,
            StructuredIncludeKind::Structure => Namespace::Value,
        })
    }

    fn type_ref_from_typed_clause(&self, node: NodeId) -> Option<FieldTypeRefData> {
        let clause = DeclClause::cast(self.syntax(node))?;
        if clause.type_clause_kind(self.source) == Some(TypeClauseKind::For) {
            return None;
        }
        let (type_ref_node, namespace) = self.typed_clause_type_ref_node(node)?;
        let (_, is_ref, base_name, _, field_path) =
            self.type_ref_access_chain(type_ref_node, namespace)?;
        Some(FieldTypeRefData {
            namespace,
            is_ref,
            base_name,
            field_path: field_path.into_iter().map(|segment| segment.name).collect(),
        })
    }

    fn render_type_ref_display(&self, node: NodeId) -> Option<Arc<str>> {
        let mut rendered = String::new();
        let mut prev_text: Option<&str> = None;
        let mut paren = 0i32;
        let mut bracket = 0i32;
        let mut brace = 0i32;

        for token in self.syntax(node).token_descendants() {
            if token.token_kind() == Some(TokenKind::Comment) {
                continue;
            }
            let text = token.text(self.source)?;

            if paren == 0
                && bracket == 0
                && brace == 0
                && token.token_kind() == Some(TokenKind::Ident)
            {
                if text.eq_ignore_ascii_case("initial")
                    || text.eq_ignore_ascii_case("length")
                    || text.eq_ignore_ascii_case("decimals")
                {
                    break;
                }
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

            let needs_space = !rendered.is_empty()
                && !matches!(text, "," | ":" | "-" | ")" | "]")
                && !matches!(prev_text, Some("(" | "[" | ":" | "-"));
            if needs_space {
                rendered.push(' ');
            }
            rendered.push_str(text);
            prev_text = Some(text);
        }

        (!rendered.is_empty()).then(|| Arc::from(rendered))
    }

    fn structure_from_typed_clause(&mut self, node: NodeId, scope: ScopeId) -> Option<StructureId> {
        let clause = DeclClause::cast(self.syntax(node))?;
        let (type_ref_node, namespace) = self.typed_clause_type_ref_node(node)?;
        let (_, is_ref, base_name, _, field_path) =
            self.type_ref_access_chain(type_ref_node, namespace)?;
        if clause.type_clause_kind(self.source) == Some(TypeClauseKind::For) {
            let low_high_type = FieldTypeRefData {
                namespace,
                is_ref,
                base_name: Arc::clone(&base_name),
                field_path: field_path
                    .iter()
                    .map(|segment| Arc::clone(&segment.name))
                    .collect(),
            };
            let range_name = self
                .render_type_ref_display(type_ref_node)
                .unwrap_or_else(|| Arc::clone(&base_name));
            return Some(self.push_range_table_structure(
                scope,
                range_name.as_ref(),
                low_high_type,
            ));
        }
        if field_path.is_empty()
            && self
                .render_type_ref_display(type_ref_node)
                .is_some_and(|display| {
                    display
                        .as_ref()
                        .to_ascii_uppercase()
                        .starts_with("RANGE OF ")
                })
        {
            let low_high_type = FieldTypeRefData {
                namespace,
                is_ref,
                base_name: Arc::clone(&base_name),
                field_path: Vec::new(),
            };
            return Some(self.push_range_table_structure(scope, base_name.as_ref(), low_high_type));
        }
        let field_path_names = field_path
            .iter()
            .map(|segment| Arc::clone(&segment.name))
            .collect::<Vec<_>>();
        let symbol_id = self.lookup_structure_symbol(
            scope,
            namespace,
            base_name.as_ref(),
            !field_path_names.is_empty(),
        )?;
        let structure_id = self.symbol(symbol_id).structure?;
        self.resolve_structure_path(structure_id, &field_path_names)
    }

    fn namespace_from_type_clause_kind(&self, kind: TypeClauseKind) -> Namespace {
        match kind {
            TypeClauseKind::Type => Namespace::Type,
            TypeClauseKind::Like => Namespace::Value,
            TypeClauseKind::For => Namespace::Value,
        }
    }

    fn lookup_structure_symbol(
        &self,
        scope: ScopeId,
        namespace: Namespace,
        name: &str,
        in_type_position: bool,
    ) -> Option<SymbolId> {
        self.lookup_symbol_in_scope_chain(scope, namespace, name)
            .or_else(|| {
                if !in_type_position {
                    return None;
                }
                let fallback = match namespace {
                    Namespace::Type => Namespace::Value,
                    Namespace::Value => Namespace::Type,
                    Namespace::Routine => return None,
                };
                self.lookup_symbol_in_scope_chain(scope, fallback, name)
            })
    }

    fn resolve_structure_path(
        &self,
        mut structure_id: StructureId,
        field_path: &[Arc<str>],
    ) -> Option<StructureId> {
        if field_path.is_empty() {
            return Some(structure_id);
        }
        for field_name in field_path {
            let field = self
                .structure(structure_id)?
                .fields
                .iter()
                .find(|field| field.name.as_ref() == field_name.as_ref())?;
            structure_id = field.structure?;
        }
        Some(structure_id)
    }

    fn resolve_field_type_ref(
        &self,
        scope: ScopeId,
        type_ref: &FieldTypeRefData,
    ) -> Option<StructureId> {
        let symbol_id = self.lookup_structure_symbol(
            scope,
            type_ref.namespace,
            type_ref.base_name.as_ref(),
            !type_ref.field_path.is_empty(),
        )?;
        let structure_id = self.symbol(symbol_id).structure?;
        if type_ref.field_path.is_empty() {
            return Some(structure_id);
        }
        self.resolve_structure_path(structure_id, &type_ref.field_path)
    }

    fn simple_type_ref_base_from_infos(
        &self,
        tokens: &[SyntaxTokenInfo],
    ) -> Option<(Arc<str>, TextRange)> {
        if is_generic_internal_table_type_display(&self.render_token_infos(tokens)) {
            return None;
        }
        let mut i = 0usize;
        if tokens
            .get(i)
            .is_some_and(|tok| tok.text.eq_ignore_ascii_case("ref"))
        {
            let to_tok = tokens.get(i + 1)?;
            if !to_tok.text.eq_ignore_ascii_case("to") {
                return None;
            }
            i += 2;
        }
        let token = tokens.get(i)?;
        if !self.syntax_token_is_ident_like(token) {
            return None;
        }
        Some((
            Arc::<str>::from(token.text.to_ascii_lowercase()),
            token.range.clone(),
        ))
    }
}

fn is_internal_table_type_display(display: &str) -> bool {
    let trimmed = display.trim();
    let upper = trimmed.to_ascii_uppercase();
    [
        "STANDARD TABLE",
        "SORTED TABLE",
        "HASHED TABLE",
        "ANY TABLE",
        "INDEX TABLE",
        "TABLE",
    ]
    .into_iter()
    .any(|prefix| upper.starts_with(prefix))
        || upper.starts_with("RANGE OF ")
}

fn is_generic_internal_table_type_display(display: &str) -> bool {
    let display = display.trim();
    display.eq_ignore_ascii_case("STANDARD TABLE")
        || display.eq_ignore_ascii_case("SORTED TABLE")
        || display.eq_ignore_ascii_case("HASHED TABLE")
        || display.eq_ignore_ascii_case("ANY TABLE")
        || display.eq_ignore_ascii_case("INDEX TABLE")
        || display.eq_ignore_ascii_case("TABLE")
}

pub fn collect_unit(
    unit_id: UnitId,
    uri: Arc<str>,
    source: &str,
    file: &File,
    tokens: &[Token],
) -> UnitAnalysis {
    Collector::new(unit_id, uri, source, file, tokens).collect()
}
