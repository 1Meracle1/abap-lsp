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
use abap_ast::ast::{AstNode, DeclClause, TypeClauseKind, TypeRefSimple};
use abap_ast::{File, SyntaxKind};
use abap_lexer::{TextRange, Token};

use crate::def_map::{
    ClassInheritanceData, ClassMemberData, Diagnostic, FieldAccess, FieldTypeRefData,
    FormRoutineData, IncludeEdge, LoopWhereFieldContext, NamedArgumentAccess, PerformCallData,
    ReferenceData, SqlNameRefData, SqlPredicateData, SqlProjectionData, SqlQueryData,
    SqlSourceData, SqlTargetData, StructureData, SymbolData, UnitAnalysis,
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
}

#[derive(Debug, Clone)]
enum PendingStructureMember {
    Field(PendingStructureField),
    Include { type_ref: FieldTypeRefData },
}

#[derive(Debug, Clone)]
struct PendingStructure {
    name: Arc<str>,
    members: Vec<PendingStructureMember>,
}

#[derive(Debug, Clone)]
struct PendingMethodParameter {
    name: Arc<str>,
    range: TextRange,
    declared_type: Option<FieldTypeRefData>,
}

#[derive(Debug, Clone, Default)]
struct PendingMethodSignature {
    parameters: Vec<PendingMethodParameter>,
    is_redefinition: bool,
}

#[derive(Clone)]
pub(super) struct SyntaxTokenInfo {
    range: TextRange,
    text: Arc<str>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SqlClauseKind {
    Where,
    JoinOn,
    Having,
    ForAllEntries,
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
    diagnostics: Vec<Diagnostic>,
    include_edges: Vec<IncludeEdge>,
    field_accesses: Vec<FieldAccess>,
    loop_where_field_contexts: Vec<LoopWhereFieldContext>,
    class_members: Vec<ClassMemberData>,
    form_routines: Vec<FormRoutineData>,
    named_arguments: Vec<NamedArgumentAccess>,
    perform_calls: Vec<PerformCallData>,
    sql_queries: Vec<SqlQueryData>,
    sql_sources: Vec<SqlSourceData>,
    sql_projections: Vec<SqlProjectionData>,
    sql_name_refs: Vec<SqlNameRefData>,
    sql_predicates: Vec<SqlPredicateData>,
    sql_targets: Vec<SqlTargetData>,
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
            diagnostics: Vec::new(),
            include_edges: Vec::new(),
            field_accesses: Vec::new(),
            loop_where_field_contexts: Vec::new(),
            class_members: Vec::new(),
            form_routines: Vec::new(),
            named_arguments: Vec::new(),
            perform_calls: Vec::new(),
            sql_queries: Vec::new(),
            sql_sources: Vec::new(),
            sql_projections: Vec::new(),
            sql_name_refs: Vec::new(),
            sql_predicates: Vec::new(),
            sql_targets: Vec::new(),
            class_definition_scopes: std::collections::HashMap::new(),
            class_superclasses: std::collections::HashMap::new(),
            class_method_signatures: std::collections::HashMap::new(),
            scope_symbols: Vec::new(),
            type_clause_ns_stack: Vec::new(),
        }
    }

    pub fn collect(mut self) -> UnitAnalysis {
        let root = self.file.root();
        let root_scope = self.push_scope(ScopeKind::File, self.file.range(root), None, None);
        self.install_builtin_symbols(root_scope);
        let mut ctx = self.context();
        traverse::walk_root(&mut ctx, root, root_scope);
        let provided_names = self.provided_names();
        let class_inheritance = self
            .class_superclasses
            .into_iter()
            .map(|(class_symbol, superclass_name)| ClassInheritanceData {
                class_symbol,
                superclass_name,
            })
            .collect();
        UnitAnalysis {
            unit_id: self.unit_id,
            uri: self.uri,
            root_scope,
            scopes: self.scopes,
            symbols: self.symbols,
            structures: self.structures,
            references: self.references,
            diagnostics: self.diagnostics,
            include_edges: self.include_edges,
            field_accesses: self.field_accesses,
            loop_where_field_contexts: self.loop_where_field_contexts,
            class_members: self.class_members,
            class_inheritance,
            form_routines: self.form_routines,
            named_arguments: self.named_arguments,
            perform_calls: self.perform_calls,
            sql_queries: self.sql_queries,
            sql_sources: self.sql_sources,
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
        let mut stack = vec![node];
        while let Some(current) = stack.pop() {
            if self.file.kind(current) == SyntaxKind::ConstructorExpr
                && let Some((type_name, _)) = self.constructor_type_ref(current)
            {
                let declared_type = FieldTypeRefData {
                    namespace: Namespace::Type,
                    is_ref: true,
                    base_name: Arc::clone(&type_name),
                    field_path: Vec::new(),
                };
                let structure = self
                    .lookup_structure_symbol(scope, Namespace::Type, type_name.as_ref(), false)
                    .and_then(|symbol_id| self.symbol(symbol_id).structure);
                return (structure, Some(declared_type));
            }
            for child in self.file.children(current) {
                stack.push(child);
            }
        }
        (None, None)
    }

    fn simple_stmt_token_infos(&self, node: NodeId) -> Vec<SyntaxTokenInfo> {
        self.file
            .children(node)
            .flat_map(|child| self.syntax_token_nodes(child))
            .collect()
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

    fn render_statement_signature_infos(&self, tokens: &[SyntaxTokenInfo]) -> String {
        let mut rendered = String::new();
        let mut prev_text: Option<&str> = None;
        for token in tokens {
            if self.syntax_token_is_comment(token) {
                continue;
            }
            if token.text.as_ref() == "." {
                break;
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
        token.text.trim_start().starts_with('"')
    }

    fn syntax_token_is_ident_like(&self, token: &SyntaxTokenInfo) -> bool {
        !matches!(
            token.text.as_ref(),
            ":" | "," | "." | "-" | "(" | ")" | "[" | "]" | "{" | "}" | "=" | "->" | "=>" | "~"
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

    fn structured_decl_name_range(&self, node: NodeId) -> Option<TextRange> {
        let mut tokens = self
            .file
            .children(node)
            .filter(|&child| self.file.kind(child) == SyntaxKind::Token)
            .map(|child| self.syntax(child));
        let begin_tok = tokens.next()?;
        let of_tok = tokens.next()?;
        let name_tok = tokens.next()?;
        let begin_text = begin_tok.text(self.source)?;
        let of_text = of_tok.text(self.source)?;
        let name_text = name_tok.text(self.source)?;
        if !begin_text.eq_ignore_ascii_case("begin")
            || !of_text.eq_ignore_ascii_case("of")
            || matches!(name_text, "." | "," | ":" | "-")
        {
            return None;
        }
        Some(name_tok.range())
    }

    fn pending_structure_from_node(
        &self,
        node: NodeId,
        scope: ScopeId,
    ) -> Option<PendingStructure> {
        let (name, _) = self.node_name(
            self.file
                .children(node)
                .filter(|&child| self.file.kind(child) == SyntaxKind::Token)
                .nth(2)?,
        )?;
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
                    }));
                }
                SyntaxKind::StructuredFieldClause | SyntaxKind::TypesTypedClause => {
                    let field = self.pending_structure_field_from_clause(child, scope)?;
                    members.push(PendingStructureMember::Field(field));
                }
                SyntaxKind::StructuredIncludeClause => {
                    let type_ref = self.type_ref_from_structured_include_clause(child)?;
                    members.push(PendingStructureMember::Include { type_ref });
                }
                _ => {}
            }
        }
        Some(PendingStructure { name, members })
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
        })
    }

    fn type_ref_from_structured_include_clause(&self, node: NodeId) -> Option<FieldTypeRefData> {
        self.syntax(node)
            .child_by_kind(SyntaxKind::TypeRefSimple)
            .and_then(|type_ref| self.field_type_ref_from_node(type_ref.id(), Namespace::Type))
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
        Some(Arc::from(type_ref.display_text(self.source)?))
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

    fn type_ref_from_typed_clause(&self, node: NodeId) -> Option<FieldTypeRefData> {
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

    fn structure_from_typed_clause(&self, node: NodeId, scope: ScopeId) -> Option<StructureId> {
        let (type_ref_node, namespace) = self.typed_clause_type_ref_node(node)?;
        let (_, _, base_name, _, field_path) =
            self.type_ref_access_chain(type_ref_node, namespace)?;
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

pub fn collect_unit(
    unit_id: UnitId,
    uri: Arc<str>,
    source: &str,
    file: &File,
    tokens: &[Token],
) -> UnitAnalysis {
    Collector::new(unit_id, uri, source, file, tokens).collect()
}
