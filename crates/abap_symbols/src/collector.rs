use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

use abap_ast::arena::NodeId;
use abap_ast::ast::{
    AstNode, CallArgList, CallExpr, CallNamedArg, CallPositionalArg, DataDecl, DataDeclName,
    DataLikeDecl, DeclClause, ExprIdent, MethodsStmt, MethodsStmtKind, MethodsTypeClauseKind,
    SelectQuery, SelectStmt, SelectorExpr, SqlDataSource, SqlProjectionItem, SyntaxNodeRef,
    TypeClauseKind, TypeRefSimple,
};
use abap_ast::{File, SyntaxKind};
use abap_lexer::{TextRange, Token, TokenKind, have_space_between};

use crate::builtins::{BUILTIN_STRUCTURES, BUILTIN_SYMBOLS, BuiltinTypeKind, builtin_routine_spec};
use crate::def_map::{
    ClassInheritanceData, ClassMemberData, ClassMemberKind, ClassMemberParameterData, Diagnostic,
    DiagnosticKind, FieldAccess, FieldAccessSegment, FieldTypeRefData, FormParameterData,
    FormParameterPassingKind, FormParameterSection, FormRoutineData, IncludeEdge,
    NamedArgumentAccess, NamedArgumentSection, NamedArgumentTarget, PerformArgumentData,
    PerformCallData, PerformParameterSection, ReferenceData, ReferenceKind, SqlNameRefData,
    SqlNameRefKind, SqlPredicateData, SqlPredicateKind, SqlProjectionData, SqlProjectionKind,
    SqlQueryData, SqlResolution, SqlSourceData, SqlSourceKind, SqlTargetData, SqlTargetKind,
    StructureData, StructureFieldData, SymbolData, SymbolKind, UnitAnalysis, Visibility,
};
use crate::ids::{ReferenceId, ScopeId, StructureId, SymbolId, UnitId};
use crate::scope::{Namespace, ScopeData, ScopeKind};

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

#[derive(Clone, Copy)]
enum FormHeaderParamSection {
    Tables,
    Using,
    Changing,
}

impl FormHeaderParamSection {
    fn as_form_parameter_section(self) -> FormParameterSection {
        match self {
            Self::Tables => FormParameterSection::Tables,
            Self::Using => FormParameterSection::Using,
            Self::Changing => FormParameterSection::Changing,
        }
    }
}

struct FormConsumedParameter {
    next_idx: usize,
    symbol: SymbolId,
    passing: FormParameterPassingKind,
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
    tokens: &'a [Token],
    token_index_by_range: HashMap<(usize, usize), usize>,
    unit_id: UnitId,
    uri: Arc<str>,
    scopes: Vec<ScopeData>,
    symbols: Vec<SymbolData>,
    structures: Vec<StructureData>,
    references: Vec<ReferenceData>,
    diagnostics: Vec<Diagnostic>,
    include_edges: Vec<IncludeEdge>,
    field_accesses: Vec<FieldAccess>,
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
    class_definition_scopes: HashMap<SymbolId, ScopeId>,
    class_superclasses: HashMap<SymbolId, Arc<str>>,
    class_method_signatures: HashMap<SymbolId, HashMap<Arc<str>, PendingMethodSignature>>,
    scope_symbols: Vec<HashMap<ScopeLookupKey, Vec<SymbolId>>>,
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
        tokens: &'a [Token],
    ) -> Self {
        let token_index_by_range = tokens
            .iter()
            .enumerate()
            .map(|(idx, token)| ((token.range.start, token.range.end), idx))
            .collect();
        Self {
            source,
            file,
            tokens,
            token_index_by_range,
            unit_id,
            uri,
            scopes: Vec::new(),
            symbols: Vec::new(),
            structures: Vec::new(),
            references: Vec::new(),
            diagnostics: Vec::new(),
            include_edges: Vec::new(),
            field_accesses: Vec::new(),
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
            class_definition_scopes: HashMap::new(),
            class_superclasses: HashMap::new(),
            class_method_signatures: HashMap::new(),
            scope_symbols: Vec::new(),
            type_clause_ns_stack: Vec::new(),
        }
    }

    pub fn collect(mut self) -> UnitAnalysis {
        let root = self.file.root();
        let root_scope = self.push_scope(ScopeKind::File, self.file.range(root), None, None);
        self.install_builtin_symbols(root_scope);
        self.walk_children(root, root_scope);
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
        }
    }

    fn push_scope(
        &mut self,
        kind: ScopeKind,
        range: TextRange,
        parent: Option<ScopeId>,
        owner: Option<SymbolId>,
    ) -> ScopeId {
        let id = ScopeId(self.scopes.len() as u32);
        self.scopes.push(ScopeData {
            id,
            kind,
            range,
            parent,
            owner,
            declarations: Vec::new(),
            children: Vec::new(),
            allows_internal_table_line_selector: false,
        });
        self.scope_symbols.push(HashMap::new());
        if let Some(parent_id) = parent {
            self.scopes[parent_id.as_usize()].children.push(id);
        }
        id
    }

    fn declare_symbol(
        &mut self,
        scope: ScopeId,
        name: Arc<str>,
        kind: SymbolKind,
        decl_range: TextRange,
        structure: Option<StructureId>,
        declared_type: Option<FieldTypeRefData>,
        type_clause_display: Option<Arc<str>>,
    ) -> SymbolId {
        let id = SymbolId(self.symbols.len() as u32);
        self.symbols.push(SymbolData {
            id,
            name: Arc::clone(&name),
            kind,
            scope,
            decl_range: decl_range.clone(),
            structure,
            declared_type,
            type_clause_display,
        });
        self.scopes[scope.as_usize()].declarations.push(id);
        for &namespace in kind.namespaces() {
            let key = ScopeLookupKey {
                namespace,
                name: Arc::clone(&name),
            };
            if let Some(existing) = self.scope_symbols[scope.as_usize()].get(&key)
                && !existing.is_empty()
                && !kind.is_builtin()
                && existing
                    .iter()
                    .any(|existing_id| !self.symbols[existing_id.as_usize()].kind.is_builtin())
            {
                self.diagnostics.push(Diagnostic {
                    kind: DiagnosticKind::DuplicateDeclaration,
                    range: decl_range.clone(),
                    message: format!("duplicate declaration of '{}'", name),
                });
            } else if !kind.is_builtin()
                && self
                    .find_ancestor_symbol(scope, namespace, name.as_ref())
                    .is_some_and(|symbol_id| !self.symbol(symbol_id).kind.is_builtin())
            {
                self.diagnostics.push(Diagnostic {
                    kind: DiagnosticKind::ShadowedSymbol,
                    range: decl_range.clone(),
                    message: format!("'{}' shadows an outer declaration", name),
                });
            }
            self.scope_symbols[scope.as_usize()]
                .entry(key)
                .or_default()
                .push(id);
        }
        id
    }

    fn declare_plain_symbol(
        &mut self,
        scope: ScopeId,
        name: Arc<str>,
        kind: SymbolKind,
        decl_range: TextRange,
    ) -> SymbolId {
        self.declare_symbol(scope, name, kind, decl_range, None, None, None)
    }

    fn push_structure(
        &mut self,
        name: Arc<str>,
        fields: impl IntoIterator<Item = StructureFieldData>,
    ) -> StructureId {
        let id = StructureId(self.structures.len() as u32);
        self.structures.push(StructureData {
            id,
            name,
            fields: fields.into_iter().collect(),
        });
        id
    }

    fn register_structure(&mut self, scope: ScopeId, structure: PendingStructure) -> StructureId {
        let mut fields = Vec::new();
        for member in structure.members {
            match member {
                PendingStructureMember::Field(field) => {
                    fields.push(StructureFieldData {
                        name: field.name,
                        decl_range: Some(field.decl_range),
                        structure: field
                            .structure
                            .map(|nested| self.register_structure(scope, nested))
                            .or_else(|| {
                                field.type_ref.as_ref().and_then(|type_ref| {
                                    self.resolve_field_type_ref(scope, type_ref)
                                })
                            }),
                        type_ref: field.type_ref,
                    });
                }
                PendingStructureMember::Include { type_ref } => {
                    if let Some(structure_id) = self.resolve_field_type_ref(scope, &type_ref)
                        && let Some(included) = self.structure(structure_id)
                    {
                        fields.extend(included.fields.iter().cloned());
                    }
                }
            }
        }
        self.push_structure(structure.name, fields)
    }

    fn install_builtin_symbols(&mut self, root_scope: ScopeId) {
        let mut structure_ids = HashMap::new();
        for structure in BUILTIN_STRUCTURES {
            let id = self.push_structure(
                Arc::<str>::from(structure.name),
                structure.fields.iter().map(|field| StructureFieldData {
                    name: Arc::<str>::from(field.name),
                    decl_range: None,
                    structure: None,
                    type_ref: None,
                }),
            );
            structure_ids.insert(structure.name, id);
        }

        for symbol in BUILTIN_SYMBOLS {
            let kind = match symbol.kind {
                BuiltinTypeKind::Type => SymbolKind::BuiltinType,
                BuiltinTypeKind::Constant => SymbolKind::BuiltinConstant,
                BuiltinTypeKind::Variable => SymbolKind::BuiltinVariable,
            };
            self.declare_symbol(
                root_scope,
                Arc::<str>::from(symbol.name),
                kind,
                0..0,
                symbol
                    .structure_name
                    .and_then(|name| structure_ids.get(name).copied()),
                None,
                None,
            );
        }
    }

    fn find_ancestor_symbol(
        &self,
        scope: ScopeId,
        namespace: Namespace,
        name: &str,
    ) -> Option<SymbolId> {
        let key = ScopeLookupKey {
            namespace,
            name: Arc::<str>::from(name),
        };
        let mut current = self.scopes[scope.as_usize()].parent;
        while let Some(scope_id) = current {
            if let Some(symbols) = self.scope_symbols[scope_id.as_usize()].get(&key)
                && let Some(symbol_id) = symbols.last().copied()
            {
                return Some(symbol_id);
            }
            current = self.scopes[scope_id.as_usize()].parent;
        }
        None
    }

    fn lookup_symbol_in_scope_chain(
        &self,
        scope: ScopeId,
        namespace: Namespace,
        name: &str,
    ) -> Option<SymbolId> {
        let key = ScopeLookupKey {
            namespace,
            name: Arc::<str>::from(name),
        };
        let mut current = Some(scope);
        while let Some(scope_id) = current {
            if let Some(symbols) = self.scope_symbols[scope_id.as_usize()].get(&key)
                && let Some(symbol_id) = symbols.last().copied()
            {
                return Some(symbol_id);
            }
            current = self.scopes[scope_id.as_usize()].parent;
        }
        None
    }

    fn add_reference(
        &mut self,
        scope: ScopeId,
        name: Arc<str>,
        namespace: Namespace,
        kind: ReferenceKind,
        range: TextRange,
    ) {
        let id = ReferenceId(self.references.len() as u32);
        self.references.push(ReferenceData {
            id,
            name,
            namespace,
            kind,
            scope,
            range,
            resolution: None,
        });
    }

    fn walk_children(&mut self, node: NodeId, scope: ScopeId) {
        for child in self.file.children(node) {
            self.walk_node(child, scope);
        }
    }

    fn node_has_structured_children(&self, node: NodeId) -> bool {
        self.file
            .children(node)
            .any(|child| self.file.kind(child) != SyntaxKind::Token)
    }

    fn walk_node(&mut self, node: NodeId, scope: ScopeId) {
        match self.file.kind(node) {
            SyntaxKind::Token | SyntaxKind::Error => {}
            SyntaxKind::DataDecl | SyntaxKind::StaticsDecl => {
                self.walk_data_like_decl(node, scope, SymbolKind::Variable)
            }
            SyntaxKind::TypesDecl => self.walk_data_like_decl(node, scope, SymbolKind::TypeDef),
            SyntaxKind::ConstantsDecl => {
                self.walk_data_like_decl(node, scope, SymbolKind::Constant)
            }
            SyntaxKind::FieldSymbolsDecl => {
                self.walk_data_like_decl(node, scope, SymbolKind::FieldSymbol)
            }
            SyntaxKind::DataInlineDecl => self.walk_inline_decl(node, scope),
            SyntaxKind::IncludeStmt => self.walk_include_stmt(node, scope),
            SyntaxKind::ReportStmt => {
                self.walk_named_header_decl(node, scope, SymbolKind::Report, ScopeKind::File)
            }
            SyntaxKind::FormDecl => {
                self.walk_block_decl(node, scope, SymbolKind::Form, ScopeKind::Form)
            }
            SyntaxKind::ModuleDecl => {
                self.walk_block_decl(node, scope, SymbolKind::Module, ScopeKind::Module)
            }
            SyntaxKind::EventBlock => self.walk_event_block(node, scope),
            SyntaxKind::ClassDecl => self.walk_class_decl(node, scope),
            SyntaxKind::InterfaceDecl => {
                self.walk_block_decl(node, scope, SymbolKind::Interface, ScopeKind::Interface)
            }
            SyntaxKind::MethodDecl => self.walk_method_decl(node, scope),
            SyntaxKind::IfStmt => self.walk_if_stmt(node, scope),
            SyntaxKind::ElseifClause => {
                self.walk_nested_block(node, scope, ScopeKind::ElseifBranch);
            }
            SyntaxKind::ElseClause => {
                self.walk_nested_block(node, scope, ScopeKind::ElseBranch);
            }
            SyntaxKind::WhenClause => {
                self.walk_nested_block(node, scope, ScopeKind::WhenBranch);
            }
            SyntaxKind::WhileStmt => self.walk_nested_block(node, scope, ScopeKind::WhileBlock),
            SyntaxKind::DoStmt => self.walk_nested_block(node, scope, ScopeKind::DoBlock),
            SyntaxKind::LoopStmt => self.walk_loop_stmt(node, scope),
            SyntaxKind::TryStmt => self.walk_nested_block(node, scope, ScopeKind::TryBlock),
            SyntaxKind::CatchClause => self.walk_nested_block(node, scope, ScopeKind::CatchClause),
            SyntaxKind::CleanupClause => {
                self.walk_nested_block(node, scope, ScopeKind::CleanupClause)
            }
            SyntaxKind::SelectStmt => self.collect_select_stmt(node, scope),
            SyntaxKind::AppendStmt
            | SyntaxKind::InsertTableStmt
            | SyntaxKind::MoveCorrespondingStmt
            | SyntaxKind::MoveStmt
            | SyntaxKind::ModifyStmt
            | SyntaxKind::DeleteStmt
            | SyntaxKind::DeleteDbTableStmt
            | SyntaxKind::ReadTableStmt
            | SyntaxKind::GetBitStmt
            | SyntaxKind::SetBitStmt => self.walk_children(node, scope),
            SyntaxKind::SortStmt => self.collect_sort_stmt(node, scope),
            SyntaxKind::TypeRefSimple => self.collect_type_ref(node, scope),
            SyntaxKind::ExprIdent
            | SyntaxKind::SelectorExpr
            | SyntaxKind::CallExpr
            | SyntaxKind::BinaryExpr
            | SyntaxKind::UnaryExpr
            | SyntaxKind::ParenExpr
            | SyntaxKind::ConstructorExpr
            | SyntaxKind::TemplateExpr
            | SyntaxKind::TemplateInterpolation
            | SyntaxKind::TemplateFormatSpec
            | SyntaxKind::CharStringTemplate
            | SyntaxKind::IsPredicate
            | SyntaxKind::InstanceOfPredicate
            | SyntaxKind::BetweenExpr
            | SyntaxKind::AssignStmt => self.collect_expr(node, scope),
            SyntaxKind::AssignSourceExpr | SyntaxKind::CallMethodTarget => {
                self.walk_children(node, scope)
            }
            SyntaxKind::AssignKeywordStmt => self.collect_assign_keyword_stmt(node, scope),
            SyntaxKind::FieldSymbolInlineDecl => self.walk_inline_field_symbol_decl(node, scope),
            SyntaxKind::GetTimeStampStmt => self.collect_get_time_stamp_stmt(node, scope),
            SyntaxKind::CallStmt => self.collect_call_stmt(node, scope),
            SyntaxKind::MessageStmt => self.collect_message_stmt(node, scope),
            SyntaxKind::UnparsedStmt | SyntaxKind::RaiseStmt | SyntaxKind::EndAtStmt => {
                self.collect_generic_simple_stmt(node, scope)
            }
            SyntaxKind::MethodsStmt => self.collect_methods_stmt(node, scope),
            SyntaxKind::AssertStmt | SyntaxKind::CheckStmt => {
                self.collect_assert_or_check_stmt(node, scope)
            }
            SyntaxKind::PerformStmt => self.collect_perform_stmt_node(node, scope),
            SyntaxKind::CreateObjectStmt => self.collect_create_object_stmt_node(node, scope),
            SyntaxKind::CallMethodStmt => self.collect_call_method_stmt_node(node, scope),
            SyntaxKind::WriteStmt => self.collect_write_stmt(node, scope),
            SyntaxKind::ConcatenateStmt => self.collect_concatenate_stmt(node, scope),
            SyntaxKind::StructuredFieldClause => {
                let hint = self.typed_clause_namespace_hint(node);
                if let Some(ns) = hint {
                    self.type_clause_ns_stack.push(ns);
                }
                self.walk_children(node, scope);
                if hint.is_some() {
                    self.type_clause_ns_stack.pop();
                }
            }
            _ => self.walk_children(node, scope),
        }
    }

    fn walk_data_like_decl(&mut self, node: NodeId, scope: ScopeId, kind: SymbolKind) {
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

    fn declare_decl_clause_symbol(&mut self, node: NodeId, scope: ScopeId, kind: SymbolKind) {
        if let Some((name, range, members)) = self.begin_of_clause_parts(node, scope) {
            let structure = self.register_structure(
                scope,
                PendingStructure {
                    name: Arc::clone(&name),
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

    fn declare_structured_decl_symbol(&mut self, node: NodeId, scope: ScopeId, kind: SymbolKind) {
        if let Some((name, range, members)) = self.begin_of_clause_parts(node, scope) {
            let structure = self.register_structure(
                scope,
                PendingStructure {
                    name: Arc::clone(&name),
                    members,
                },
            );
            self.declare_symbol(scope, name, kind, range, Some(structure), None, None);
        }
    }

    fn walk_inline_decl(&mut self, node: NodeId, scope: ScopeId) {
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

    fn walk_inline_field_symbol_decl(&mut self, node: NodeId, scope: ScopeId) {
        self.declare_inline_field_symbol_decl(node, scope, None, None);
    }

    fn declare_inline_field_symbol_decl(
        &mut self,
        node: NodeId,
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

    fn walk_include_stmt(&mut self, node: NodeId, scope: ScopeId) {
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

    fn walk_named_header_decl(
        &mut self,
        node: NodeId,
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
                self.push_scope(
                    fallback_scope_kind,
                    self.file.range(node),
                    Some(scope),
                    Some(owner),
                )
            } else {
                scope
            };
            for child in self.file.children(node) {
                self.walk_node(child, block_scope);
            }
        }
    }

    fn walk_class_decl(&mut self, node: NodeId, scope: ScopeId) {
        let is_implementation = self.class_header_has_implementation(node);
        let Some((name, range)) = self.header_ident_after_keyword(node) else {
            self.walk_children(node, scope);
            return;
        };
        let mut impl_header_refs_class = false;
        let owner = if is_implementation {
            if let Some(existing) = self
                .lookup_symbol_in_scope_chain(scope, Namespace::Type, name.as_ref())
                .filter(|&id| self.symbol(id).kind == SymbolKind::Class)
            {
                impl_header_refs_class = true;
                existing
            } else {
                self.declare_plain_symbol(
                    scope,
                    Arc::clone(&name),
                    SymbolKind::Class,
                    range.clone(),
                )
            }
        } else {
            self.declare_plain_symbol(scope, Arc::clone(&name), SymbolKind::Class, range.clone())
        };
        if impl_header_refs_class {
            self.add_reference(scope, name, Namespace::Type, ReferenceKind::TypeRef, range);
        }
        if !is_implementation && let Some((superclass, range)) = self.class_superclass_name(node) {
            self.class_superclasses
                .insert(owner, Arc::clone(&superclass));
            self.add_reference(
                scope,
                superclass,
                Namespace::Type,
                ReferenceKind::TypeRef,
                range,
            );
        }
        let parent_scope = if is_implementation {
            self.class_definition_scopes
                .get(&owner)
                .copied()
                .or(Some(scope))
        } else {
            Some(scope)
        };
        let child_scope = self.push_scope(
            ScopeKind::Class,
            self.file.range(node),
            parent_scope,
            Some(owner),
        );
        if !is_implementation {
            self.class_definition_scopes.insert(owner, child_scope);
        }
        if !is_implementation {
            self.collect_class_definition_members(node, owner, child_scope);
        }
        for child in self.file.children(node) {
            self.walk_node(child, child_scope);
        }
    }

    fn walk_block_decl(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        kind: SymbolKind,
        scope_kind: ScopeKind,
    ) {
        let Some((name, range)) = self.header_ident_after_keyword(node) else {
            self.walk_children(node, scope);
            return;
        };
        let owner = self.declare_plain_symbol(scope, name, kind, range);
        let child_scope =
            self.push_scope(scope_kind, self.file.range(node), Some(scope), Some(owner));
        if scope_kind == ScopeKind::Form {
            let parameters = self.declare_form_parameters_from_header(node, child_scope);
            self.form_routines.push(FormRoutineData {
                symbol: owner,
                parameters,
            });
        }
        for child in self.file.children(node) {
            self.walk_node(child, child_scope);
        }
    }

    fn walk_method_decl(&mut self, node: NodeId, scope: ScopeId) {
        let Some((name, range)) = self.header_ident_after_keyword(node) else {
            self.walk_children(node, scope);
            return;
        };
        let owner =
            self.declare_plain_symbol(scope, Arc::clone(&name), SymbolKind::Method, range.clone());
        let child_scope = self.push_scope(
            ScopeKind::Method,
            self.file.range(node),
            Some(scope),
            Some(owner),
        );
        if let Some(class_symbol) = self.enclosing_class_owner(scope) {
            self.declare_method_signature_parameters(
                class_symbol,
                name.as_ref(),
                child_scope,
                scope,
            );
            self.declare_implicit_me_symbol(class_symbol, name.as_ref(), child_scope, &range);
        }
        for child in self.file.children(node) {
            self.walk_node(child, child_scope);
        }
    }

    fn walk_event_block(&mut self, node: NodeId, scope: ScopeId) {
        let Some((name, range)) = self.event_block_header_name(node) else {
            self.walk_children(node, scope);
            return;
        };
        let owner = self.declare_plain_symbol(scope, name, SymbolKind::Event, range);
        let child_scope = self.push_scope(
            ScopeKind::EventBlock,
            self.file.range(node),
            Some(scope),
            Some(owner),
        );
        for child in self.file.children(node) {
            self.walk_node(child, child_scope);
        }
    }

    fn class_header_has_implementation(&self, node: NodeId) -> bool {
        for child in self.file.children(node) {
            let Some(token) = self.token_for_node(child) else {
                continue;
            };
            if token.kind == TokenKind::Period {
                break;
            }
            if self.token_matches_keyword(token, "implementation") {
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
        let mut stack: Vec<_> = self.file.children(node).rev().collect();
        while let Some(child) = stack.pop() {
            match self.file.kind(child) {
                SyntaxKind::ClassSectionStmt => {
                    let tokens = self.simple_stmt_tokens(child);
                    if let Some(section_visibility) = self.class_section_visibility(&tokens) {
                        visibility = section_visibility;
                    }
                }
                SyntaxKind::MethodsStmt => {
                    let Some(methods_stmt) = MethodsStmt::cast(self.syntax(child)) else {
                        continue;
                    };
                    if let Some(mut member) =
                        self.class_member_from_methods_stmt(class_symbol, visibility, methods_stmt)
                    {
                        if member.kind == ClassMemberKind::Method {
                            let signature = self.parse_method_signature(methods_stmt);
                            member.parameters = self.class_member_parameters(&signature);
                            self.declare_method_signature_parameter_symbols(
                                self.file.range(child),
                                &signature,
                            );
                            self.class_method_signatures
                                .entry(class_symbol)
                                .or_default()
                                .insert(Arc::clone(&member.name), signature);
                        }
                        self.class_members.push(member);
                    }
                }
                SyntaxKind::DataDecl | SyntaxKind::StaticsDecl | SyntaxKind::ConstantsDecl => {
                    self.collect_class_attribute_members(
                        child,
                        class_symbol,
                        visibility,
                        class_scope,
                    );
                }
                _ => {
                    for nested in self.file.children(child).rev() {
                        stack.push(nested);
                    }
                }
            }
        }
    }

    fn class_superclass_name(&self, node: NodeId) -> Option<(Arc<str>, TextRange)> {
        let tokens = self.simple_stmt_tokens(node);
        let significant: Vec<_> = tokens
            .iter()
            .copied()
            .filter(|token| token.kind != TokenKind::Comment)
            .collect();
        for window in significant.windows(3) {
            if self.token_matches_keyword(window[0], "inheriting")
                && self.token_matches_keyword(window[1], "from")
                && window[2].kind == TokenKind::Ident
            {
                return Some((
                    Arc::<str>::from(window[2].lexeme(self.source).to_ascii_lowercase()),
                    window[2].range.clone(),
                ));
            }
        }
        None
    }

    fn simple_stmt_tokens(&self, node: NodeId) -> Vec<&'a Token> {
        self.token_refs_from_child_nodes(node)
    }

    fn significant_stmt_tokens(&self, node: NodeId) -> Vec<&'a Token> {
        self.simple_stmt_tokens(node)
            .into_iter()
            .filter(|token| token.kind != TokenKind::Comment)
            .collect()
    }

    fn class_section_visibility(&self, tokens: &[&Token]) -> Option<Visibility> {
        let significant: Vec<_> = tokens
            .iter()
            .copied()
            .filter(|token| token.kind != TokenKind::Comment)
            .collect();
        if significant.len() < 3 || significant[2].kind != TokenKind::Period {
            return None;
        }
        if !self.token_matches_keyword(significant[1], "section") {
            return None;
        }
        if self.token_matches_keyword(significant[0], "public") {
            return Some(Visibility::Public);
        }
        if self.token_matches_keyword(significant[0], "protected") {
            return Some(Visibility::Protected);
        }
        if self.token_matches_keyword(significant[0], "private") {
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
        let (kind, is_static) = match methods_stmt.member_kind(self.source)? {
            MethodsStmtKind::Instance => (ClassMemberKind::Method, false),
            MethodsStmtKind::Class => (ClassMemberKind::Method, true),
        };
        let name_tok = methods_stmt.name_token(self.source)?;
        Some(ClassMemberData {
            class_symbol,
            name: Arc::<str>::from(
                name_tok
                    .text(self.source)
                    .unwrap_or_default()
                    .to_ascii_lowercase(),
            ),
            kind,
            visibility,
            is_static,
            decl_range: name_tok.range(),
            signature: Arc::<str>::from(methods_stmt.signature_text(self.source)),
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
        let signature =
            Arc::<str>::from(self.render_statement_signature(&self.simple_stmt_tokens(node)));
        for child in self.file.children(node) {
            match self.file.kind(child) {
                SyntaxKind::DataTypedClause
                | SyntaxKind::ConstantClause
                | SyntaxKind::StructuredDecl => {
                    if let Some(mut member) = self.class_attribute_member_from_clause(
                        child,
                        class_symbol,
                        visibility,
                        is_static,
                        Arc::clone(&signature),
                    ) {
                        member.structure = self.class_attribute_structure_for_clause(child, scope);
                        self.class_members.push(member);
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
        let (name, _, members) = self.begin_of_clause_parts(node, scope)?;
        Some(self.register_structure(scope, PendingStructure { name, members }))
    }

    fn class_attribute_decl_is_static(&self, node: NodeId) -> bool {
        let tokens = self.significant_stmt_tokens(node);
        let Some(first) = tokens.first().copied() else {
            return false;
        };
        if self.token_matches_keyword(first, "constants")
            || self.token_matches_keyword(first, "statics")
        {
            return true;
        }
        let Some(second) = tokens.get(1).copied() else {
            return false;
        };
        let Some(third) = tokens.get(2).copied() else {
            return false;
        };
        self.token_matches_keyword(first, "class")
            && second.kind == TokenKind::Minus
            && self.token_matches_keyword(third, "data")
    }

    fn class_attribute_structured_clause_name_parts(
        &self,
        node: NodeId,
    ) -> Option<(Arc<str>, TextRange)> {
        let name_node = self
            .file
            .children(node)
            .filter(|&child| self.file.kind(child) == SyntaxKind::Token)
            .nth(2)?;
        let (name, _) = self.node_name(name_node)?;
        let decl_range = self.structured_decl_name_range(node)?;
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
        let (name, decl_range) = match self.file.kind(node) {
            SyntaxKind::StructuredDecl => {
                self.class_attribute_structured_clause_name_parts(node)?
            }
            SyntaxKind::DataTypedClause | SyntaxKind::ConstantClause => self
                .class_attribute_structured_clause_name_parts(node)
                .or_else(|| {
                    let name_node = self
                        .file
                        .children(node)
                        .find(|&child| self.file.kind(child) == SyntaxKind::DataDeclName)?;
                    self.node_name(name_node)
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
            signature,
            parameters: Vec::new(),
            structure: None,
        })
    }

    fn class_member_parameters(
        &self,
        signature: &PendingMethodSignature,
    ) -> Vec<ClassMemberParameterData> {
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
        self.class_members.iter().find(|member| {
            member.class_symbol == class_symbol && member.name.as_ref() == member_name
        })
    }

    fn declare_implicit_me_symbol(
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
        let class_name = Arc::clone(&self.symbol(class_symbol).name);
        self.declare_symbol(
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

    fn render_statement_signature(&self, tokens: &[&Token]) -> String {
        let mut rendered = String::new();
        let mut prev_kind = None;
        for token in tokens {
            if token.kind == TokenKind::Comment {
                continue;
            }
            if token.kind == TokenKind::Period {
                break;
            }
            let needs_space = !rendered.is_empty()
                && !matches!(
                    token.kind,
                    TokenKind::Comma
                        | TokenKind::Colon
                        | TokenKind::Minus
                        | TokenKind::RParen
                        | TokenKind::RBracket
                )
                && !matches!(
                    prev_kind,
                    Some(
                        TokenKind::LParen
                            | TokenKind::LBracket
                            | TokenKind::Colon
                            | TokenKind::Minus
                    )
                );
            if needs_space {
                rendered.push(' ');
            }
            rendered.push_str(token.lexeme(self.source));
            prev_kind = Some(token.kind);
        }
        rendered
    }

    fn parse_method_signature(&self, methods_stmt: MethodsStmt<'_>) -> PendingMethodSignature {
        let parsed = methods_stmt.signature(self.source);
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
                    .text(self.source)
                    .unwrap_or_default()
                    .to_ascii_lowercase(),
            );
            signature.parameters.push(PendingMethodParameter {
                name,
                range: param.name_token().range(),
                declared_type: param.type_ref().and_then(|type_ref| {
                    self.field_type_ref_from_node(type_ref.syntax().id(), clause_ns)
                }),
            });
        }
        signature
    }

    fn enclosing_class_owner(&self, scope: ScopeId) -> Option<SymbolId> {
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

    fn declare_method_signature_parameters(
        &mut self,
        class_symbol: SymbolId,
        method_name: &str,
        method_scope: ScopeId,
        lookup_scope: ScopeId,
    ) {
        let Some(parameters) = self
            .class_method_signature(class_symbol, method_name, lookup_scope)
            .map(|signature| signature.parameters.clone())
        else {
            return;
        };
        for param in parameters {
            self.declare_symbol(
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
        // Signature-only parameters should highlight and hover in the definition section,
        // but they must not participate in body resolution for unrelated methods.
        let signature_scope = self.push_scope(ScopeKind::Method, signature_range, None, None);
        for param in &signature.parameters {
            self.declare_symbol(
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

    fn class_method_signature(
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

    fn form_header_token_refs(&self, form_node: NodeId) -> Vec<&'a Token> {
        let mut out = Vec::new();
        for child in self.file.children(form_node) {
            match self.file.kind(child) {
                SyntaxKind::Token => {
                    if let Some(token) = self.token_for_node(child) {
                        let is_period = token.kind == TokenKind::Period;
                        out.push(token);
                        if is_period {
                            break;
                        }
                    }
                }
                SyntaxKind::TypeRefSimple => out.extend(self.token_refs(child)),
                _ => break,
            }
        }
        out
    }

    fn declare_form_parameters_from_header(
        &mut self,
        form_node: NodeId,
        form_scope: ScopeId,
    ) -> Vec<FormParameterData> {
        let tokens = self.form_header_token_refs(form_node);
        let type_ref_nodes = self.direct_type_ref_children(form_node);
        let mut type_ref_idx = 0usize;
        if tokens.len() < 2 {
            return Vec::new();
        }
        if !self.token_matches_keyword(tokens[0], "form") {
            return Vec::new();
        }
        let mut i = 1usize;
        while i < tokens.len() && tokens[i].kind == TokenKind::Comment {
            i += 1;
        }
        if tokens.get(i).map(|t| t.kind) != Some(TokenKind::Ident) {
            return Vec::new();
        }
        i += 1;

        let mut section: Option<FormHeaderParamSection> = None;
        let mut depth = 0i32;
        let mut parameters = Vec::new();

        while i < tokens.len() {
            let t = tokens[i];
            if t.kind == TokenKind::Comment {
                i += 1;
                continue;
            }
            match t.kind {
                TokenKind::LParen => {
                    depth += 1;
                    i += 1;
                }
                TokenKind::RParen => {
                    depth -= 1;
                    i += 1;
                }
                TokenKind::Period if depth == 0 => break,
                _ if depth == 0 && t.kind == TokenKind::Ident => {
                    let lit = t.lexeme(self.source);
                    if lit.eq_ignore_ascii_case("tables") {
                        section = Some(FormHeaderParamSection::Tables);
                        i += 1;
                        continue;
                    }
                    if lit.eq_ignore_ascii_case("using") {
                        section = Some(FormHeaderParamSection::Using);
                        i += 1;
                        continue;
                    }
                    if lit.eq_ignore_ascii_case("changing") {
                        section = Some(FormHeaderParamSection::Changing);
                        i += 1;
                        continue;
                    }
                    if lit.eq_ignore_ascii_case("raises") {
                        section = None;
                        i += 1;
                        continue;
                    }

                    match section {
                        Some(FormHeaderParamSection::Using)
                        | Some(FormHeaderParamSection::Changing) => {
                            if let Some(consumed) = self.try_consume_form_value_or_reference_param(
                                &tokens,
                                i,
                                form_scope,
                                type_ref_nodes.get(type_ref_idx).copied(),
                            ) {
                                if self.symbol(consumed.symbol).declared_type.is_some() {
                                    type_ref_idx += 1;
                                }
                                let current_section = section.expect("parameter section");
                                parameters.push(FormParameterData {
                                    symbol: consumed.symbol,
                                    section: current_section.as_form_parameter_section(),
                                    passing: consumed.passing,
                                });
                                i = consumed.next_idx;
                                continue;
                            }
                            if self.form_header_starts_typed_param(&tokens, i) {
                                let range = t.range.clone();
                                let name = Arc::<str>::from(lit.to_ascii_lowercase());
                                let mut j = i + 1;
                                while j < tokens.len() && tokens[j].kind == TokenKind::Comment {
                                    j += 1;
                                }
                                let declared_type = match tokens.get(j) {
                                    Some(tok) if self.token_matches_keyword(tok, "type") => {
                                        j += 1;
                                        while j < tokens.len()
                                            && tokens[j].kind == TokenKind::Comment
                                        {
                                            j += 1;
                                        }
                                        let expr_end =
                                            self.skip_form_header_type_expression(&tokens, j);
                                        let dt = type_ref_nodes
                                            .get(type_ref_idx)
                                            .copied()
                                            .and_then(|node| {
                                                self.field_type_ref_from_node(node, Namespace::Type)
                                            });
                                        if dt.is_some() {
                                            type_ref_idx += 1;
                                        }
                                        j = expr_end;
                                        dt
                                    }
                                    Some(tok) if self.token_matches_keyword(tok, "like") => {
                                        j += 1;
                                        while j < tokens.len()
                                            && tokens[j].kind == TokenKind::Comment
                                        {
                                            j += 1;
                                        }
                                        let expr_end =
                                            self.skip_form_header_type_expression(&tokens, j);
                                        let dt = type_ref_nodes
                                            .get(type_ref_idx)
                                            .copied()
                                            .and_then(|node| {
                                                self.field_type_ref_from_node(
                                                    node,
                                                    Namespace::Value,
                                                )
                                            });
                                        if dt.is_some() {
                                            type_ref_idx += 1;
                                        }
                                        j = expr_end;
                                        dt
                                    }
                                    _ => None,
                                };
                                let symbol = self.declare_symbol(
                                    form_scope,
                                    name,
                                    SymbolKind::Parameter,
                                    range,
                                    None,
                                    declared_type,
                                    None,
                                );
                                parameters.push(FormParameterData {
                                    symbol,
                                    section: section
                                        .expect("parameter section")
                                        .as_form_parameter_section(),
                                    passing: FormParameterPassingKind::Direct,
                                });
                                i = j;
                                continue;
                            }
                            i += 1;
                        }
                        Some(FormHeaderParamSection::Tables) => {
                            if t.kind == TokenKind::Ident {
                                let symbol = self.declare_symbol(
                                    form_scope,
                                    Arc::<str>::from(lit.to_ascii_lowercase()),
                                    SymbolKind::Parameter,
                                    t.range.clone(),
                                    None,
                                    None,
                                    None,
                                );
                                parameters.push(FormParameterData {
                                    symbol,
                                    section: FormParameterSection::Tables,
                                    passing: FormParameterPassingKind::Direct,
                                });
                            }
                            i += 1;
                        }
                        None => {
                            i += 1;
                        }
                    }
                }
                _ => {
                    i += 1;
                }
            }
        }
        parameters
    }

    fn form_header_section_keyword(&self, token: &Token) -> bool {
        token.kind == TokenKind::Ident
            && matches!(
                token.lexeme(self.source).to_ascii_uppercase().as_str(),
                "TABLES" | "USING" | "CHANGING" | "RAISES"
            )
    }

    fn form_header_starts_typed_param(&self, tokens: &[&Token], idx: usize) -> bool {
        let name = match tokens.get(idx) {
            Some(t) if t.kind == TokenKind::Ident => *t,
            _ => return false,
        };
        if self.token_matches_keyword(name, "value")
            || self.token_matches_keyword(name, "reference")
        {
            return false;
        }
        let mut j = idx + 1;
        while j < tokens.len() && tokens[j].kind == TokenKind::Comment {
            j += 1;
        }
        tokens.get(j).is_some_and(|tok| {
            self.token_matches_keyword(tok, "type") || self.token_matches_keyword(tok, "like")
        })
    }

    fn try_consume_form_value_or_reference_param(
        &mut self,
        tokens: &[&Token],
        i: usize,
        scope: ScopeId,
        type_ref_node: Option<NodeId>,
    ) -> Option<FormConsumedParameter> {
        let kw = tokens.get(i)?;
        let passing = if self.token_matches_keyword(kw, "value") {
            FormParameterPassingKind::Value
        } else if self.token_matches_keyword(kw, "reference") {
            FormParameterPassingKind::Reference
        } else {
            return None;
        };
        let mut j = i + 1;
        while j < tokens.len() && tokens[j].kind == TokenKind::Comment {
            j += 1;
        }
        let (name, range) = if tokens.get(j).map(|t| t.kind) == Some(TokenKind::LParen) {
            j += 1;
            while j < tokens.len() && tokens[j].kind == TokenKind::Comment {
                j += 1;
            }
            let inner = tokens.get(j)?;
            if inner.kind != TokenKind::Ident {
                return None;
            }
            let name = Arc::<str>::from(inner.lexeme(self.source).to_ascii_lowercase());
            let range = inner.range.clone();
            j += 1;
            while j < tokens.len() && tokens[j].kind == TokenKind::Comment {
                j += 1;
            }
            if tokens.get(j).map(|t| t.kind) != Some(TokenKind::RParen) {
                return None;
            }
            j += 1;
            (name, range)
        } else {
            let inner = tokens.get(j)?;
            if inner.kind != TokenKind::Ident {
                return None;
            }
            let name = Arc::<str>::from(inner.lexeme(self.source).to_ascii_lowercase());
            let range = inner.range.clone();
            j += 1;
            (name, range)
        };
        while j < tokens.len() && tokens[j].kind == TokenKind::Comment {
            j += 1;
        }
        let type_tok = tokens.get(j)?;
        let clause_ns = if self.token_matches_keyword(type_tok, "type") {
            Namespace::Type
        } else if self.token_matches_keyword(type_tok, "like") {
            Namespace::Value
        } else {
            return None;
        };
        j += 1;
        while j < tokens.len() && tokens[j].kind == TokenKind::Comment {
            j += 1;
        }
        let expr_start = j;
        let expr_end = self.skip_form_header_type_expression(tokens, expr_start);
        let declared_type =
            type_ref_node.and_then(|node| self.field_type_ref_from_node(node, clause_ns));
        let symbol = self.declare_symbol(
            scope,
            name,
            SymbolKind::Parameter,
            range,
            None,
            declared_type,
            None,
        );
        Some(FormConsumedParameter {
            next_idx: expr_end,
            symbol,
            passing,
        })
    }

    fn skip_form_header_type_expression(&self, tokens: &[&Token], mut i: usize) -> usize {
        let mut depth = 0i32;
        while i < tokens.len() {
            let t = tokens[i];
            if t.kind == TokenKind::Comment {
                i += 1;
                continue;
            }
            match t.kind {
                TokenKind::LParen => {
                    depth += 1;
                    i += 1;
                }
                TokenKind::RParen => {
                    depth -= 1;
                    i += 1;
                }
                TokenKind::Period if depth == 0 => return i,
                _ if depth == 0 && self.form_header_section_keyword(t) => return i,
                _ if depth == 0 && self.form_header_starts_typed_param(tokens, i) => return i,
                _ => i += 1,
            }
        }
        i
    }

    fn walk_if_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let branch_scope = self.push_scope(
            ScopeKind::IfBranch,
            self.file.range(node),
            Some(scope),
            None,
        );
        for child in self.file.children(node) {
            match self.file.kind(child) {
                SyntaxKind::ElseifClause | SyntaxKind::ElseClause => self.walk_node(child, scope),
                _ => self.walk_node(child, branch_scope),
            }
        }
    }

    fn walk_loop_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let child_scope = self.push_scope(
            ScopeKind::LoopBlock,
            self.file.range(node),
            Some(scope),
            None,
        );
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

    fn walk_nested_block(&mut self, node: NodeId, scope: ScopeId, kind: ScopeKind) {
        let child_scope = self.push_scope(kind, self.file.range(node), Some(scope), None);
        for child in self.file.children(node) {
            self.walk_node(child, child_scope);
        }
    }

    fn select_stmt_has_endselect(&self, node: NodeId) -> bool {
        self.file.children(node).any(|child| {
            self.file.kind(child) == SyntaxKind::Token
                && self.token_for_node(child).is_some_and(|token| {
                    token.kind == TokenKind::Ident
                        && token.lexeme(self.source).eq_ignore_ascii_case("endselect")
                })
        })
    }

    fn collect_select_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let Some(stmt) = SelectStmt::cast(self.syntax(node)) else {
            self.walk_children(node, scope);
            return;
        };
        let query_node = stmt.query().map(|query| query.syntax().id());
        let non_query_children: Vec<_> =
            stmt.non_query_children().map(|child| child.id()).collect();
        let has_endselect = self.select_stmt_has_endselect(node);
        if has_endselect {
            let child_scope = self.push_scope(
                ScopeKind::SelectBlock,
                self.file.range(node),
                Some(scope),
                None,
            );
            if let Some(query_node) = query_node {
                self.collect_select_query(query_node, child_scope, true);
            }
            for child in non_query_children {
                self.walk_node(child, child_scope);
            }
        } else {
            if let Some(query_node) = query_node {
                self.collect_select_query(query_node, scope, false);
            }
            for child in non_query_children {
                self.walk_node(child, scope);
            }
        }
    }

    fn collect_select_query(&mut self, node: NodeId, scope: ScopeId, has_endselect: bool) {
        let Some(query) = SelectQuery::cast(self.syntax(node)) else {
            return;
        };
        let children: Vec<_> = query
            .syntax()
            .children()
            .map(|child| (child.id(), child.kind(), child.range()))
            .collect();
        let query_id = self.sql_queries.len();
        let mut projection_clause = None;
        let mut from_clause = None;
        let mut into_clause = None;
        let mut where_clause = None;
        let mut group_by_clause = None;
        let mut having_clause = None;
        let mut order_by_clause = None;
        let mut for_all_entries_clause = None;
        let mut up_to_clause = None;
        let mut is_single = false;
        let mut is_distinct = false;
        let mut has_dynamic_where = false;

        for (child_id, child_kind, child_range) in children {
            match child_kind {
                SyntaxKind::Token => {
                    if self
                        .token_for_node(child_id)
                        .is_some_and(|token| self.token_matches_keyword(token, "single"))
                    {
                        is_single = true;
                    }
                }
                SyntaxKind::SelectDistinctClause => {
                    is_distinct = true;
                }
                SyntaxKind::SelectProjectionList => {
                    projection_clause = Some(child_range);
                    self.collect_select_projection_list(query_id, child_id, scope);
                }
                SyntaxKind::SelectFromClause => {
                    from_clause = Some(child_range);
                    self.collect_select_from_clause(query_id, child_id, scope);
                }
                SyntaxKind::SelectIntoClause => {
                    into_clause = Some(child_range);
                    self.collect_select_into_clause(query_id, child_id, scope);
                }
                SyntaxKind::SelectWhereClause => {
                    where_clause = Some(child_range);
                    has_dynamic_where =
                        self.file.count_kind(child_id, SyntaxKind::SqlDynamicWhere) > 0;
                    self.collect_sql_clause(query_id, child_id, scope, SqlClauseKind::Where);
                }
                SyntaxKind::SelectGroupByClause => {
                    group_by_clause = Some(child_range);
                    self.collect_sql_host_refs_in_node(child_id, scope);
                    self.collect_sql_name_refs_in_node(query_id, child_id, scope);
                }
                SyntaxKind::SelectHavingClause => {
                    having_clause = Some(child_range);
                    self.collect_sql_clause(query_id, child_id, scope, SqlClauseKind::Having);
                }
                SyntaxKind::SelectOrderByClause => {
                    order_by_clause = Some(child_range);
                    self.collect_sql_host_refs_in_node(child_id, scope);
                    self.collect_sql_name_refs_in_node(query_id, child_id, scope);
                }
                SyntaxKind::SelectForAllEntriesClause => {
                    for_all_entries_clause = Some(child_range);
                    self.collect_sql_clause(
                        query_id,
                        child_id,
                        scope,
                        SqlClauseKind::ForAllEntries,
                    );
                }
                SyntaxKind::SelectUpToClause => {
                    up_to_clause = Some(child_range);
                    self.collect_sql_host_refs_in_node(child_id, scope);
                }
                _ => {}
            }
        }

        self.sql_queries.push(SqlQueryData {
            id: query_id,
            scope,
            range: self.file.range(node),
            projection_clause,
            from_clause,
            into_clause,
            where_clause,
            group_by_clause,
            having_clause,
            order_by_clause,
            for_all_entries_clause,
            up_to_clause,
            is_single,
            is_distinct,
            has_endselect,
            has_dynamic_where,
        });
    }

    fn collect_select_projection_list(&mut self, query_id: usize, node: NodeId, scope: ScopeId) {
        let children: Vec<_> = self
            .syntax(node)
            .children()
            .filter_map(SqlProjectionItem::cast)
            .map(|item| item.syntax().id())
            .collect();
        for child in children {
            self.collect_sql_projection_item(query_id, child, scope);
        }
    }

    fn collect_sql_projection_item(&mut self, query_id: usize, node: NodeId, scope: ScopeId) {
        let alias = SqlProjectionItem::cast(self.syntax(node))
            .and_then(|item| item.alias())
            .and_then(|alias_node| self.node_name(alias_node.syntax().id()));
        let tokens = self.token_refs(node);
        self.collect_sql_host_refs(&tokens, scope);

        let mut kind = SqlProjectionKind::Expression;
        let mut source_alias = None;
        let mut name = None;

        let children: Vec<_> = self
            .syntax(node)
            .children()
            .map(|child| (child.id(), child.kind()))
            .collect();
        for (child, kind_syntax) in children {
            match kind_syntax {
                SyntaxKind::SqlStar => {
                    kind = SqlProjectionKind::Star;
                    self.push_sql_name_ref(
                        query_id,
                        scope,
                        self.file.range(child),
                        Arc::<str>::from("*"),
                        None,
                        SqlNameRefKind::Star,
                    );
                }
                SyntaxKind::SqlQualifiedStar => {
                    kind = SqlProjectionKind::QualifiedStar;
                    if let Some((qualifier, range)) = self.sql_qualified_name_parts(child, true) {
                        source_alias = Some(Arc::clone(&qualifier));
                        self.push_sql_name_ref(
                            query_id,
                            scope,
                            range,
                            Arc::<str>::from("*"),
                            Some(qualifier),
                            SqlNameRefKind::QualifiedStar,
                        );
                    }
                }
                SyntaxKind::SqlColumnRef => {
                    kind = SqlProjectionKind::Column;
                    if let Some((qualifier, column, range)) = self.sql_column_ref_parts(child) {
                        source_alias = qualifier.clone();
                        name = Some(Arc::clone(&column));
                        self.push_sql_name_ref(
                            query_id,
                            scope,
                            range,
                            column,
                            qualifier,
                            if source_alias.is_some() {
                                SqlNameRefKind::QualifiedColumn
                            } else {
                                SqlNameRefKind::Column
                            },
                        );
                    }
                }
                _ => {}
            }
        }

        if matches!(kind, SqlProjectionKind::Expression)
            && let Some(token) = tokens.first()
            && token.kind == TokenKind::Ident
            && tokens
                .get(1)
                .is_some_and(|next| next.kind == TokenKind::LParen)
        {
            kind = SqlProjectionKind::Aggregate;
            self.push_sql_name_ref(
                query_id,
                scope,
                token.range.clone(),
                Arc::<str>::from(token.lexeme(self.source).to_ascii_lowercase()),
                None,
                SqlNameRefKind::Aggregate,
            );
        }

        if matches!(kind, SqlProjectionKind::Expression) {
            self.collect_sql_name_refs_from_tokens(query_id, scope, &tokens, false);
        }
        self.sql_projections.push(SqlProjectionData {
            query_id,
            range: self.file.range(node),
            kind,
            source_alias,
            name,
            alias: alias.map(|(name, _)| name),
        });
    }

    fn collect_select_from_clause(&mut self, query_id: usize, node: NodeId, scope: ScopeId) {
        let mut saw_base_source = false;
        let children: Vec<_> = self
            .syntax(node)
            .children()
            .map(|child| (child.id(), child.kind()))
            .collect();
        for (child, kind_syntax) in children {
            match kind_syntax {
                SyntaxKind::SqlDataSource => {
                    let source_kind = if saw_base_source {
                        SqlSourceKind::Join
                    } else {
                        SqlSourceKind::From
                    };
                    saw_base_source = true;
                    self.collect_sql_data_source(query_id, child, scope, source_kind, None);
                }
                SyntaxKind::SelectJoinClause => {
                    self.collect_select_join_clause(query_id, child, scope)
                }
                _ => {}
            }
        }
    }

    fn collect_select_join_clause(&mut self, query_id: usize, node: NodeId, scope: ScopeId) {
        let mut join_tokens = Vec::new();
        for child in self.file.children(node) {
            match self.file.kind(child) {
                SyntaxKind::Token => {
                    if let Some(token) = self.token_for_node(child) {
                        join_tokens.push(token);
                    }
                }
                SyntaxKind::SqlDataSource => {
                    let join_kind = self.token_span_text(&join_tokens);
                    self.collect_sql_data_source(
                        query_id,
                        child,
                        scope,
                        SqlSourceKind::Join,
                        join_kind,
                    );
                }
                SyntaxKind::SqlPredicateExpr => {
                    self.collect_sql_clause(query_id, child, scope, SqlClauseKind::JoinOn);
                }
                _ => {}
            }
        }
    }

    fn collect_sql_data_source(
        &mut self,
        query_id: usize,
        node: NodeId,
        scope: ScopeId,
        source_kind: SqlSourceKind,
        join_kind: Option<Arc<str>>,
    ) {
        let alias_info = SqlDataSource::cast(self.syntax(node))
            .and_then(|source| source.alias())
            .and_then(|alias_node| {
                self.node_name(alias_node.syntax().id())
                    .map(|(name, _)| (name, alias_node.syntax().range()))
            });
        let tokens = self.token_refs(node);
        let alias_idx = tokens
            .iter()
            .position(|token| self.token_matches_keyword(token, "as"));
        let name_tokens = alias_idx.map(|idx| &tokens[..idx]).unwrap_or(&tokens[..]);
        let Some(name) = self.token_span_text(name_tokens) else {
            return;
        };
        let name_range = self
            .token_span_range(name_tokens)
            .unwrap_or_else(|| self.file.range(node));
        let alias = alias_info.as_ref().map(|(name, _)| Arc::clone(name));

        self.sql_sources.push(SqlSourceData {
            query_id,
            range: self.file.range(node),
            source_kind,
            name: Arc::clone(&name),
            alias: alias.clone(),
            join_kind,
            resolution: SqlResolution::External,
        });
        self.push_sql_name_ref(
            query_id,
            scope,
            name_range,
            name,
            None,
            SqlNameRefKind::Source,
        );
        if let Some(alias_name) = alias {
            let alias_range = alias_info
                .as_ref()
                .map(|(_, range)| range.clone())
                .unwrap_or_else(|| self.file.range(node));
            self.push_sql_name_ref(
                query_id,
                scope,
                alias_range,
                alias_name,
                None,
                SqlNameRefKind::Alias,
            );
        }
    }

    fn collect_select_into_clause(&mut self, query_id: usize, node: NodeId, scope: ScopeId) {
        let tokens = self.token_refs(node);
        let is_appending = tokens
            .first()
            .is_some_and(|token| self.token_matches_keyword(token, "appending"));
        let is_table = tokens
            .iter()
            .any(|token| self.token_matches_keyword(token, "table"));
        let is_corresponding = tokens
            .iter()
            .any(|token| self.token_matches_keyword(token, "corresponding"));

        let mut target_name = None;
        let mut is_inline = false;
        for child in self.file.children(node) {
            match self.file.kind(child) {
                SyntaxKind::DataInlineDecl => {
                    is_inline = true;
                    target_name = self.inline_decl_name(child);
                    self.walk_inline_decl(child, scope);
                }
                SyntaxKind::FieldSymbolInlineDecl => {
                    is_inline = true;
                    target_name = self.inline_decl_name(child);
                    self.declare_inline_field_symbol_decl(child, scope, None, None);
                }
                SyntaxKind::ExprIdent
                | SyntaxKind::SelectorExpr
                | SyntaxKind::CallExpr
                | SyntaxKind::BinaryExpr
                | SyntaxKind::UnaryExpr
                | SyntaxKind::ParenExpr
                | SyntaxKind::ConstructorExpr => {
                    if target_name.is_none() {
                        target_name = self.sql_target_name_from_expr(child);
                    }
                    self.collect_expr(child, scope);
                }
                // `push_expr_child` wraps `parse_arithmetic_expr` results in `TemplateExpr`.
                SyntaxKind::TemplateExpr => {
                    for grandchild in self.file.children(child) {
                        if target_name.is_none() {
                            target_name = self.sql_target_name_from_expr(grandchild);
                        }
                        self.collect_expr(grandchild, scope);
                    }
                }
                _ => {}
            }
        }

        self.sql_targets.push(SqlTargetData {
            query_id,
            scope,
            range: self.file.range(node),
            kind: if is_appending {
                SqlTargetKind::Appending
            } else {
                SqlTargetKind::Into
            },
            target_name,
            is_table,
            is_corresponding,
            is_inline,
        });
    }

    fn collect_sql_clause(
        &mut self,
        query_id: usize,
        node: NodeId,
        scope: ScopeId,
        kind: SqlClauseKind,
    ) {
        self.sql_predicates.push(SqlPredicateData {
            query_id,
            range: self.file.range(node),
            kind: match kind {
                SqlClauseKind::Where => {
                    if self.file.count_kind(node, SyntaxKind::SqlDynamicWhere) > 0 {
                        SqlPredicateKind::DynamicWhere
                    } else {
                        SqlPredicateKind::Where
                    }
                }
                SqlClauseKind::JoinOn => SqlPredicateKind::JoinOn,
                SqlClauseKind::Having => SqlPredicateKind::Having,
                SqlClauseKind::ForAllEntries => SqlPredicateKind::ForAllEntries,
            },
        });

        let tokens = self.token_refs(node);
        match kind {
            SqlClauseKind::ForAllEntries => {
                if let Some(in_idx) = tokens
                    .iter()
                    .position(|token| self.token_matches_keyword(token, "in"))
                {
                    let expr_start = in_idx + 1;
                    if expr_start < tokens.len() {
                        self.collect_token_expression_refs(&tokens[expr_start..], scope, true);
                    }
                }
            }
            _ => {
                self.collect_sql_host_refs(&tokens, scope);
                self.collect_sql_name_refs_from_tokens(query_id, scope, &tokens, true);
            }
        }
    }

    fn collect_sql_host_refs_in_node(&mut self, node: NodeId, scope: ScopeId) {
        let tokens = self.token_refs(node);
        self.collect_sql_host_refs(&tokens, scope);
    }

    fn collect_sql_name_refs_in_node(&mut self, query_id: usize, node: NodeId, scope: ScopeId) {
        let tokens = self.token_refs(node);
        self.collect_sql_name_refs_from_tokens(query_id, scope, &tokens, false);
    }

    fn collect_sql_host_refs(&mut self, tokens: &[&'a Token], scope: ScopeId) {
        let mut idx = 0usize;
        while idx < tokens.len() {
            if tokens[idx].kind == TokenKind::At {
                let expr_start = idx + 1;
                let expr_end = self.sql_host_expr_end(tokens, expr_start);
                if expr_start < expr_end {
                    self.collect_token_expression_refs(&tokens[expr_start..expr_end], scope, true);
                }
                idx = expr_end.max(expr_start);
            } else {
                idx += 1;
            }
        }
    }

    fn collect_sql_name_refs_from_tokens(
        &mut self,
        query_id: usize,
        scope: ScopeId,
        tokens: &[&'a Token],
        // When true (WHERE / HAVING / JOIN ON), bare identifiers that resolve as ABAP data objects
        // are host variables, not unqualified SQL column names.
        open_sql_predicate: bool,
    ) {
        let mut idx = 0usize;
        while idx < tokens.len() {
            let token = tokens[idx];
            match token.kind {
                TokenKind::At => {
                    idx = self.sql_host_expr_end(tokens, idx + 1);
                }
                TokenKind::Star => {
                    self.push_sql_name_ref(
                        query_id,
                        scope,
                        token.range.clone(),
                        Arc::<str>::from("*"),
                        None,
                        SqlNameRefKind::Star,
                    );
                    idx += 1;
                }
                TokenKind::Ident => {
                    if self.sql_token_is_keyword(token) {
                        idx += 1;
                        continue;
                    }
                    if tokens
                        .get(idx + 1)
                        .is_some_and(|next| next.kind == TokenKind::Tilde)
                        && let Some(third) = tokens.get(idx + 2)
                    {
                        if third.kind == TokenKind::Star {
                            self.push_sql_name_ref(
                                query_id,
                                scope,
                                token.range.start..third.range.end,
                                Arc::<str>::from("*"),
                                Some(Arc::<str>::from(
                                    token.lexeme(self.source).to_ascii_lowercase(),
                                )),
                                SqlNameRefKind::QualifiedStar,
                            );
                            idx += 3;
                            continue;
                        }
                        if third.kind == TokenKind::Ident {
                            self.push_sql_name_ref(
                                query_id,
                                scope,
                                token.range.start..third.range.end,
                                Arc::<str>::from(third.lexeme(self.source).to_ascii_lowercase()),
                                Some(Arc::<str>::from(
                                    token.lexeme(self.source).to_ascii_lowercase(),
                                )),
                                SqlNameRefKind::QualifiedColumn,
                            );
                            idx += 3;
                            continue;
                        }
                    }
                    if tokens
                        .get(idx + 1)
                        .is_some_and(|next| next.kind == TokenKind::LParen)
                    {
                        self.push_sql_name_ref(
                            query_id,
                            scope,
                            token.range.clone(),
                            Arc::<str>::from(token.lexeme(self.source).to_ascii_lowercase()),
                            None,
                            SqlNameRefKind::Aggregate,
                        );
                        idx += 1;
                        continue;
                    }
                    if idx > 0 && self.token_matches_keyword(tokens[idx - 1], "as") {
                        idx += 1;
                        continue;
                    }
                    let name = Arc::<str>::from(token.lexeme(self.source).to_ascii_lowercase());
                    if open_sql_predicate {
                        let next_kind = tokens.get(idx + 1).map(|next| next.kind);
                        if !matches!(
                            next_kind,
                            Some(
                                TokenKind::Tilde
                                    | TokenKind::Minus
                                    | TokenKind::Arrow
                                    | TokenKind::FatArrow
                            )
                        ) && self
                            .lookup_symbol_in_scope_chain(scope, Namespace::Value, name.as_ref())
                            .is_some()
                        {
                            self.add_reference(
                                scope,
                                name,
                                Namespace::Value,
                                ReferenceKind::Identifier,
                                token.range.clone(),
                            );
                            idx += 1;
                            continue;
                        }
                    }
                    self.push_sql_name_ref(
                        query_id,
                        scope,
                        token.range.clone(),
                        name,
                        None,
                        SqlNameRefKind::Column,
                    );
                    idx += 1;
                }
                _ => idx += 1,
            }
        }
    }

    fn push_sql_name_ref(
        &mut self,
        query_id: usize,
        scope: ScopeId,
        range: TextRange,
        name: Arc<str>,
        qualifier: Option<Arc<str>>,
        kind: SqlNameRefKind,
    ) {
        self.sql_name_refs.push(SqlNameRefData {
            query_id,
            scope,
            range,
            name,
            qualifier,
            kind,
            resolution: SqlResolution::External,
        });
    }

    fn inline_decl_name(&self, node: NodeId) -> Option<Arc<str>> {
        self.syntax(node)
            .child_by_kind(SyntaxKind::DataDeclName)
            .and_then(DataDeclName::cast)
            .and_then(|name| name.name(self.source))
    }

    fn sql_target_name_from_expr(&self, node: NodeId) -> Option<Arc<str>> {
        match self.file.kind(node) {
            SyntaxKind::ExprIdent => self.node_name(node).map(|(name, _)| name),
            SyntaxKind::SelectorExpr => self
                .selector_access_chain(node)
                .map(|(_, base_name, _, _)| base_name),
            _ => None,
        }
    }

    fn sql_column_ref_parts(
        &self,
        node: NodeId,
    ) -> Option<(Option<Arc<str>>, Arc<str>, TextRange)> {
        let tokens = self.syntax(node).token_descendants();
        if tokens.len() == 1
            && let Some(text) = tokens[0].text(self.source)
        {
            return Some((
                None,
                Arc::<str>::from(text.to_ascii_lowercase()),
                tokens[0].range(),
            ));
        }
        if tokens.len() == 3
            && let (Some(qualifier), Some(separator), Some(column)) = (
                tokens[0].text(self.source),
                tokens[1].text(self.source),
                tokens[2].text(self.source),
            )
            && separator == "~"
        {
            return Some((
                Some(Arc::<str>::from(qualifier.to_ascii_lowercase())),
                Arc::<str>::from(column.to_ascii_lowercase()),
                tokens[0].range().start..tokens[2].range().end,
            ));
        }
        None
    }

    fn sql_qualified_name_parts(&self, node: NodeId, star: bool) -> Option<(Arc<str>, TextRange)> {
        let tokens = self.syntax(node).token_descendants();
        if tokens.len() == 3
            && let (Some(qualifier), Some(separator), Some(last)) = (
                tokens[0].text(self.source),
                tokens[1].text(self.source),
                tokens[2].text(self.source),
            )
            && separator == "~"
            && ((star && last == "*") || (!star && last != "*"))
        {
            return Some((
                Arc::<str>::from(qualifier.to_ascii_lowercase()),
                tokens[0].range().start..tokens[2].range().end,
            ));
        }
        None
    }

    fn token_span_range(&self, tokens: &[&'a Token]) -> Option<TextRange> {
        let first = tokens.first()?;
        let last = tokens.last()?;
        Some(first.range.start..last.range.end)
    }

    fn token_span_text(&self, tokens: &[&'a Token]) -> Option<Arc<str>> {
        let range = self.token_span_range(tokens)?;
        let text = self.source.get(range)?;
        let lowered = text.trim().to_ascii_lowercase();
        if lowered.is_empty() {
            return None;
        }
        Some(Arc::<str>::from(lowered))
    }

    fn sql_token_is_keyword(&self, token: &Token) -> bool {
        if token.kind != TokenKind::Ident {
            return false;
        }
        matches!(
            token.lexeme(self.source).to_ascii_lowercase().as_str(),
            "select"
                | "single"
                | "distinct"
                | "from"
                | "into"
                | "appending"
                | "where"
                | "group"
                | "by"
                | "having"
                | "order"
                | "for"
                | "all"
                | "entries"
                | "in"
                | "up"
                | "to"
                | "rows"
                | "as"
                | "join"
                | "inner"
                | "left"
                | "right"
                | "cross"
                | "on"
                | "and"
                | "or"
                | "not"
                | "like"
                | "between"
                | "is"
                | "null"
                | "table"
                | "corresponding"
                | "fields"
                | "of"
                | "primary"
                | "key"
        )
    }

    fn sql_host_expr_end(&self, tokens: &[&'a Token], start: usize) -> usize {
        let mut paren = 0i32;
        let mut bracket = 0i32;
        let mut brace = 0i32;
        let mut idx = start;
        while idx < tokens.len() {
            let token = tokens[idx];
            if paren == 0 && bracket == 0 && brace == 0 {
                if token.kind == TokenKind::Comma
                    || token.kind == TokenKind::Eq
                    || token.kind == TokenKind::Lt
                    || token.kind == TokenKind::Gt
                    || token.kind == TokenKind::Le
                    || token.kind == TokenKind::Ge
                    || token.kind == TokenKind::Ne
                    || self.sql_token_is_keyword(token)
                {
                    break;
                }
            }
            match token.kind {
                TokenKind::LParen => paren += 1,
                TokenKind::RParen => {
                    if paren == 0 {
                        break;
                    }
                    paren -= 1;
                }
                TokenKind::LBracket => bracket += 1,
                TokenKind::RBracket => {
                    if bracket == 0 {
                        break;
                    }
                    bracket -= 1;
                }
                TokenKind::LBrace => brace += 1,
                TokenKind::RBrace => {
                    if brace == 0 {
                        break;
                    }
                    brace -= 1;
                }
                _ => {}
            }
            idx += 1;
        }
        idx
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
                        self.collect_expr(expr, scope);
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
                        self.collect_expr(expr, scope);
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
                self.declare_inline_field_symbol_decl(
                    node,
                    scope,
                    inferred_metadata.0,
                    inferred_metadata.1.clone(),
                );
            }
            _ => self.collect_expr(node, scope),
        }
    }

    fn loop_source_line_metadata_from_node(
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

    fn collect_type_ref(&mut self, node: NodeId, scope: ScopeId) {
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
                Arc::clone(&base_name),
                namespace,
                ReferenceKind::TypeRef,
                range,
            );
            if !field_path.is_empty() {
                self.field_accesses.push(FieldAccess {
                    scope,
                    base_namespace: namespace,
                    base_name,
                    field_path,
                    in_type_position: true,
                });
            }
        }
    }

    fn collect_expr(&mut self, node: NodeId, scope: ScopeId) {
        match self.file.kind(node) {
            SyntaxKind::ExprIdent => {
                if let Some((name, range)) = self.node_name(node) {
                    self.add_reference(
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
                for child in self.file.children(node) {
                    match self.file.kind(child) {
                        SyntaxKind::TypeRefSimple => self.collect_type_ref(child, scope),
                        SyntaxKind::CallArgList => arg_list = Some(child),
                        SyntaxKind::Token => {}
                        _ => self.collect_expr(child, scope),
                    }
                }
                if let Some(arg_list) = arg_list {
                    if let Some((type_name, _)) = self.constructor_type_ref(node) {
                        self.collect_call_argument_list(
                            arg_list,
                            scope,
                            NamedArgumentTarget::Constructor { type_name },
                        );
                    } else {
                        self.collect_structured_argument_values(
                            &self.file.children(arg_list).collect::<Vec<_>>(),
                            scope,
                        );
                    }
                }
            }
            SyntaxKind::TypeRefSimple => self.collect_type_ref(node, scope),
            _ => {
                for child in self.file.children(node) {
                    match self.file.kind(child) {
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
                        _ => self.walk_node(child, scope),
                    }
                }
            }
        }
    }

    /// `BY` components are row fields of the sorted internal table, not standalone value symbols.
    /// When the line type has no local structure (e.g. unresolved DDIC row type), field validation
    /// is skipped — unlike an unresolved `Identifier` reference.
    fn collect_sort_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let children: Vec<NodeId> = self.file.children(node).collect();
        let by_idx = children.iter().position(|&c| {
            self.file.kind(c) == SyntaxKind::Token
                && self
                    .token_for_node(c)
                    .is_some_and(|t| self.token_matches_keyword(t, "by"))
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
                    self.field_accesses.push(FieldAccess {
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

    /// `MESSAGE` variants used in real programs: static codes, `ID … TYPE … NUMBER …`, dynamic
    /// `TYPE`, `DISPLAY LIKE`, `WITH` placeholders, `INTO` / `INTO DATA( )`.
    fn collect_message_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let sig = self.significant_stmt_tokens(node);
        if sig.is_empty() || !self.token_matches_keyword(sig[0], "message") {
            return;
        }
        let period_pos = sig
            .iter()
            .position(|t| t.kind == TokenKind::Period)
            .unwrap_or(sig.len());

        let with_ix = self
            .find_top_level_keyword_index(&sig, 1, "with")
            .filter(|&ix| ix < period_pos);
        let into_ix = self
            .find_top_level_keyword_index(&sig, 1, "into")
            .filter(|&ix| ix < period_pos);

        let head_end = [with_ix, into_ix, Some(period_pos)]
            .into_iter()
            .flatten()
            .min()
            .unwrap();

        // --- Head: ID … TYPE … NUMBER … or MESSAGE dobj TYPE … [DISPLAY LIKE …] or static code ---
        if sig
            .get(1)
            .is_some_and(|t| self.token_matches_keyword(t, "id"))
        {
            let mut i = 2usize;
            let end_mid = self.consume_concatenate_operand(
                &sig,
                i,
                &["type", "with", "into", "display", "raising"],
            );
            if end_mid > i {
                self.collect_token_expression_refs(&sig[i..end_mid], scope, true);
            }
            i = end_mid;
            if i < head_end
                && sig
                    .get(i)
                    .is_some_and(|t| self.token_matches_keyword(t, "type"))
            {
                i += 1;
                let end_ty = self.consume_concatenate_operand(
                    &sig,
                    i,
                    &["number", "with", "into", "display", "raising"],
                );
                if end_ty > i {
                    self.collect_token_expression_refs(&sig[i..end_ty], scope, true);
                }
                i = end_ty;
            }
            if i < head_end
                && sig
                    .get(i)
                    .is_some_and(|t| self.token_matches_keyword(t, "number"))
            {
                i += 1;
                let end_num = self.consume_concatenate_operand(
                    &sig,
                    i,
                    &["with", "into", "display", "raising"],
                );
                if end_num > i {
                    self.collect_token_expression_refs(&sig[i..end_num], scope, true);
                }
            }
        } else if let Some(ti) = self
            .find_top_level_keyword_index(&sig, 1, "type")
            .filter(|&ti| ti < head_end)
        {
            if ti > 1 {
                self.collect_token_expression_refs(&sig[1..ti], scope, true);
            }
            let mut i = ti + 1;
            let end_mty =
                self.consume_concatenate_operand(&sig, i, &["display", "with", "into", "raising"]);
            if end_mty > i {
                self.collect_token_expression_refs(&sig[i..end_mty], scope, true);
            }
            i = end_mty;
            if i < head_end
                && sig
                    .get(i)
                    .is_some_and(|t| self.token_matches_keyword(t, "display"))
            {
                i += 1;
                if i < head_end
                    && sig
                        .get(i)
                        .is_some_and(|t| self.token_matches_keyword(t, "like"))
                {
                    i += 1;
                    let end_like =
                        self.consume_concatenate_operand(&sig, i, &["with", "into", "raising"]);
                    if end_like > i {
                        self.collect_token_expression_refs(&sig[i..end_like], scope, true);
                    }
                }
            }
        }

        // --- WITH operands ---
        if let Some(wi) = with_ix {
            let end_with = [
                self.find_top_level_keyword_index(&sig, wi + 1, "into"),
                self.find_top_level_keyword_index(&sig, wi + 1, "display"),
                Some(period_pos),
            ]
            .into_iter()
            .flatten()
            .min()
            .unwrap();

            let mut idx = wi + 1;
            while idx < end_with {
                let raw_end = self.consume_concatenate_operand(&sig, idx, &["into", "display"]);
                let end_op = raw_end.min(end_with);
                if end_op <= idx {
                    idx += 1;
                    continue;
                }
                self.collect_token_expression_refs(&sig[idx..end_op], scope, true);
                idx = end_op;
            }
        }

        // --- INTO target or INTO DATA( … ) ---
        if let Some(ii) = into_ix {
            let mut idx = ii + 1;
            if idx >= period_pos {
                return;
            }
            if self.token_matches_keyword(sig[idx], "data") {
                idx += 1;
                if sig.get(idx).map(|t| t.kind) == Some(TokenKind::LParen) {
                    let lpar = idx;
                    if let Some(rpar) = self.find_matching_group_end(
                        &sig,
                        lpar,
                        TokenKind::LParen,
                        TokenKind::RParen,
                    ) {
                        let inner_start = lpar + 1;
                        for &t in &sig[inner_start..rpar] {
                            if t.kind == TokenKind::Ident {
                                let name = Arc::from(t.lexeme(self.source).to_ascii_lowercase());
                                self.declare_symbol(
                                    scope,
                                    name,
                                    SymbolKind::Variable,
                                    t.range.clone(),
                                    None,
                                    None,
                                    None,
                                );
                            }
                        }
                    }
                }
            } else {
                let raw_end = self.consume_concatenate_operand(&sig, idx, &["raising", "display"]);
                let end_op = raw_end.min(period_pos);
                if end_op > idx {
                    self.collect_token_expression_refs(&sig[idx..end_op], scope, true);
                }
            }
        }
    }

    fn collect_generic_simple_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let significant = self.significant_stmt_tokens(node);
        if significant
            .first()
            .is_some_and(|token| self.token_matches_keyword(token, "clear"))
        {
            self.collect_clear_stmt(&significant, scope);
            return;
        }
        if significant
            .first()
            .is_some_and(|token| self.token_matches_keyword(token, "convert"))
        {
            self.collect_convert_stmt(&significant, scope);
            return;
        }
        if significant
            .first()
            .is_some_and(|token| self.token_matches_keyword(token, "replace"))
        {
            self.collect_replace_stmt(&significant, scope);
            return;
        }
        self.collect_token_expression_refs(&significant, scope, false);
    }

    fn collect_clear_stmt(&mut self, tokens: &[&Token], scope: ScopeId) {
        if tokens.is_empty() || !self.token_matches_keyword(tokens[0], "clear") {
            return;
        }

        let start_idx = if tokens
            .get(1)
            .is_some_and(|token| token.kind == TokenKind::Colon)
        {
            2
        } else {
            1
        };
        if start_idx >= tokens.len() {
            return;
        }

        self.collect_token_expression_refs(&tokens[start_idx..], scope, true);
    }

    fn collect_convert_stmt(&mut self, tokens: &[&Token], scope: ScopeId) {
        if tokens.is_empty() || !self.token_matches_keyword(tokens[0], "convert") {
            return;
        }

        let mut idx = 1usize;
        if tokens
            .get(idx)
            .is_some_and(|token| self.token_matches_keyword(token, "date"))
        {
            idx += 1;
            let end_idx = self.consume_concatenate_operand(tokens, idx, &["time", "into"]);
            if end_idx > idx {
                self.collect_token_expression_refs(&tokens[idx..end_idx], scope, true);
            }
            idx = end_idx;
        }

        if tokens
            .get(idx)
            .is_some_and(|token| self.token_matches_keyword(token, "time"))
            && !tokens
                .get(idx + 1)
                .is_some_and(|token| self.token_matches_keyword(token, "zone"))
        {
            idx += 1;
            let end_idx = self.consume_concatenate_operand(tokens, idx, &["into"]);
            if end_idx > idx {
                self.collect_token_expression_refs(&tokens[idx..end_idx], scope, true);
            }
            idx = end_idx;
        }

        if tokens
            .get(idx)
            .is_some_and(|token| self.token_matches_keyword(token, "into"))
        {
            idx += 1;
        }
        if tokens
            .get(idx)
            .is_some_and(|token| self.token_matches_keyword(token, "time"))
        {
            idx += 1;
        }
        if tokens
            .get(idx)
            .is_some_and(|token| self.token_matches_keyword(token, "stamp"))
        {
            idx += 1;
        }

        let target_end = self.consume_concatenate_operand(tokens, idx, &["time"]);
        if target_end > idx {
            self.collect_token_expression_refs(&tokens[idx..target_end], scope, true);
        }
        idx = target_end;

        if tokens
            .get(idx)
            .is_some_and(|token| self.token_matches_keyword(token, "time"))
            && tokens
                .get(idx + 1)
                .is_some_and(|token| self.token_matches_keyword(token, "zone"))
        {
            idx += 2;
            let end_idx = self.consume_concatenate_operand(tokens, idx, &[]);
            if end_idx > idx {
                self.collect_token_expression_refs(&tokens[idx..end_idx], scope, true);
            }
        }
    }

    fn collect_replace_stmt(&mut self, tokens: &[&Token], scope: ScopeId) {
        if tokens.is_empty() || !self.token_matches_keyword(tokens[0], "replace") {
            return;
        }

        let mut idx = 1usize;
        if tokens.get(idx).is_some_and(|token| {
            self.token_matches_keyword(token, "first") || self.token_matches_keyword(token, "all")
        }) {
            idx += 1;
            if tokens.get(idx).is_some_and(|token| {
                self.token_matches_keyword(token, "occurrence")
                    || self.token_matches_keyword(token, "occurrences")
            }) {
                idx += 1;
            }
        }
        if tokens
            .get(idx)
            .is_some_and(|token| self.token_matches_keyword(token, "of"))
        {
            idx += 1;
        }
        if tokens
            .get(idx)
            .is_some_and(|token| self.token_matches_keyword(token, "regex"))
        {
            idx += 1;
        }

        let source_end = self.consume_concatenate_operand(tokens, idx, &["in", "with"]);
        if source_end > idx {
            self.collect_token_expression_refs(&tokens[idx..source_end], scope, true);
        }
        idx = source_end;

        while idx < tokens.len() {
            let token = tokens[idx];
            if token.kind == TokenKind::Period {
                break;
            }
            if self.token_matches_keyword(token, "in") {
                if tokens.get(idx + 1).is_some_and(|next| {
                    self.token_matches_keyword(next, "character")
                        || self.token_matches_keyword(next, "byte")
                }) && tokens
                    .get(idx + 2)
                    .is_some_and(|next| self.token_matches_keyword(next, "mode"))
                {
                    idx += 3;
                    continue;
                }

                let end_idx = self.consume_concatenate_operand(tokens, idx + 1, &["with", "in"]);
                if end_idx > idx + 1 {
                    self.collect_token_expression_refs(&tokens[idx + 1..end_idx], scope, true);
                }
                idx = end_idx;
                continue;
            }
            if self.token_matches_keyword(token, "with") {
                let end_idx = self.consume_concatenate_operand(tokens, idx + 1, &["in"]);
                if end_idx > idx + 1 {
                    self.collect_token_expression_refs(&tokens[idx + 1..end_idx], scope, true);
                }
                idx = end_idx;
                continue;
            }
            idx += 1;
        }
    }

    fn collect_get_time_stamp_stmt(&mut self, node: NodeId, scope: ScopeId) {
        if self.node_has_structured_children(node) {
            self.walk_children(node, scope);
            return;
        }
        let mut significant = Vec::new();
        let mut inline_target = None;
        for child in self.file.children(node) {
            match self.file.kind(child) {
                SyntaxKind::Token => {
                    if let Some(token) = self.token_for_node(child)
                        && token.kind != TokenKind::Comment
                    {
                        significant.push(token);
                    }
                }
                SyntaxKind::DataInlineDecl => inline_target = Some(child),
                _ => self.walk_node(child, scope),
            }
        }

        if let Some(inline_decl) = inline_target {
            self.walk_inline_decl(inline_decl, scope);
            return;
        }

        if significant.len() > 4 {
            self.collect_token_expression_refs(&significant[4..], scope, true);
        }
    }

    fn collect_methods_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let methods_stmt = MethodsStmt::cast(self.syntax(node)).expect("methods stmt");
        let type_refs: Vec<_> = methods_stmt
            .type_refs()
            .map(|type_ref| type_ref.syntax().id())
            .collect();
        for type_ref in type_refs {
            self.collect_type_ref(type_ref, scope);
        }
    }

    fn collect_assert_or_check_stmt(&mut self, node: NodeId, scope: ScopeId) {
        if self.node_has_structured_children(node) {
            self.walk_children(node, scope);
            return;
        }
        let significant = self.significant_stmt_tokens(node);
        let Some((_, tail)) = significant.split_first() else {
            return;
        };
        self.collect_token_expression_refs(tail, scope, true);
    }

    fn collect_perform_stmt_node(&mut self, node: NodeId, scope: ScopeId) {
        let significant = self.significant_stmt_tokens(node);
        self.collect_perform_stmt(&significant, scope);
    }

    fn collect_create_object_stmt_node(&mut self, node: NodeId, scope: ScopeId) {
        if self.node_has_structured_children(node) {
            self.walk_children(node, scope);
            return;
        }
        let significant = self.significant_stmt_tokens(node);
        self.collect_create_object_stmt(&significant, scope);
    }

    fn collect_call_method_stmt_node(&mut self, node: NodeId, scope: ScopeId) {
        let mut target = None;
        let mut arg_list = None;

        for child in self.file.children(node) {
            match self.file.kind(child) {
                SyntaxKind::CallMethodTarget => {
                    let Some(mut callee) = self.first_non_token_child(child) else {
                        continue;
                    };
                    while self.file.kind(callee) == SyntaxKind::TemplateExpr {
                        let Some(inner) = self.first_non_token_child(callee) else {
                            break;
                        };
                        callee = inner;
                    }
                    match self.file.kind(callee) {
                        SyntaxKind::ExprIdent => {
                            let Some((method_name, _)) = self.node_name(callee) else {
                                continue;
                            };
                            target = Some(NamedArgumentTarget::ImplicitMethod { method_name });
                        }
                        SyntaxKind::SelectorExpr => {
                            self.collect_selector_expr(callee, scope);
                            target = self.named_argument_target_for_callee(callee);
                        }
                        _ => self.collect_expr(callee, scope),
                    }
                }
                SyntaxKind::CallArgList => arg_list = Some(child),
                _ => {}
            }
        }

        if let (Some(target), Some(arg_list)) = (target, arg_list) {
            self.collect_call_argument_list(arg_list, scope, target);
        }
    }

    fn collect_call_stmt(&mut self, node: NodeId, scope: ScopeId) {
        if self.node_has_structured_children(node) {
            self.walk_children(node, scope);
            return;
        }
        self.collect_generic_simple_stmt(node, scope);
    }

    fn collect_assign_keyword_stmt(&mut self, node: NodeId, scope: ScopeId) {
        let mut source_expr = None;
        let mut inline_targets = Vec::new();
        for child in self.file.children(node) {
            match self.file.kind(child) {
                SyntaxKind::Token => {}
                SyntaxKind::AssignSourceExpr => {
                    let Some(expr) = self.first_non_token_child(child) else {
                        continue;
                    };
                    source_expr = Some(expr);
                    self.collect_expr(expr, scope);
                }
                SyntaxKind::FieldSymbolInlineDecl => inline_targets.push(child),
                _ => self.walk_node(child, scope),
            }
        }

        if inline_targets.is_empty() {
            return;
        }

        let inferred_metadata = source_expr
            .map(|expr| self.loop_source_line_metadata_from_node(expr, scope))
            .unwrap_or((None, None));
        for target in inline_targets {
            self.declare_inline_field_symbol_decl(
                target,
                scope,
                inferred_metadata.0,
                inferred_metadata.1.clone(),
            );
        }
    }

    fn collect_write_stmt(&mut self, node: NodeId, scope: ScopeId) {
        if self.node_has_structured_children(node) {
            self.walk_children(node, scope);
            return;
        }
        let tokens = self.simple_stmt_tokens(node);
        let significant: Vec<_> = tokens
            .into_iter()
            .filter(|token| token.kind != TokenKind::Comment)
            .collect();
        let Some((_, tail)) = significant.split_first() else {
            return;
        };
        self.collect_token_expression_refs(tail, scope, true);
    }

    fn collect_concatenate_stmt(&mut self, node: NodeId, scope: ScopeId) {
        if self.node_has_structured_children(node) {
            self.walk_children(node, scope);
            return;
        }
        let significant = self.significant_stmt_tokens(node);
        if significant.is_empty() || !self.token_matches_keyword(significant[0], "concatenate") {
            return;
        }

        let Some(into_idx) = self.find_top_level_keyword_index(&significant, 1, "into") else {
            self.collect_token_expression_refs(&significant[1..], scope, true);
            return;
        };

        let mut idx = 1usize;
        while idx < into_idx {
            let end_idx = self.consume_concatenate_operand(&significant, idx, &["into"]);
            if end_idx == idx {
                idx += 1;
                continue;
            }
            self.collect_token_expression_refs(&significant[idx..end_idx], scope, true);
            idx = end_idx;
        }

        idx = into_idx + 1;
        let target_end =
            self.consume_concatenate_operand(&significant, idx, &["separated", "respecting", "in"]);
        if target_end > idx {
            self.collect_token_expression_refs(&significant[idx..target_end], scope, true);
        }
        idx = target_end;

        while idx < significant.len() {
            let token = significant[idx];
            if token.kind == TokenKind::Period {
                break;
            }
            if self.token_matches_keyword(token, "separated")
                && significant
                    .get(idx + 1)
                    .is_some_and(|next| self.token_matches_keyword(next, "by"))
            {
                let sep_start = idx + 2;
                let sep_end = self.consume_concatenate_operand(
                    &significant,
                    sep_start,
                    &["respecting", "in"],
                );
                if sep_end > sep_start {
                    self.collect_token_expression_refs(
                        &significant[sep_start..sep_end],
                        scope,
                        true,
                    );
                }
                idx = sep_end;
                continue;
            }
            if self.token_matches_keyword(token, "respecting") {
                idx += 1;
                if significant
                    .get(idx)
                    .is_some_and(|next| self.token_matches_keyword(next, "blanks"))
                {
                    idx += 1;
                }
                continue;
            }
            if self.token_matches_keyword(token, "in") {
                idx += 1;
                if significant.get(idx).is_some_and(|next| {
                    self.token_matches_keyword(next, "character")
                        || self.token_matches_keyword(next, "byte")
                }) {
                    idx += 1;
                }
                if significant
                    .get(idx)
                    .is_some_and(|next| self.token_matches_keyword(next, "mode"))
                {
                    idx += 1;
                }
                continue;
            }
            idx += 1;
        }
    }

    fn collect_perform_stmt(&mut self, tokens: &[&Token], scope: ScopeId) {
        if tokens.len() < 2 || !self.token_matches_keyword(tokens[0], "perform") {
            return;
        }
        let routine = tokens[1];
        if routine.kind != TokenKind::Ident {
            return;
        }

        let routine_name = Arc::<str>::from(routine.lexeme(self.source).to_ascii_lowercase());
        self.add_reference(
            scope,
            Arc::clone(&routine_name),
            Namespace::Routine,
            ReferenceKind::RoutineCall,
            routine.range.clone(),
        );

        let mut parameters = Vec::new();
        let mut arguments = Vec::new();
        let mut section: Option<PerformParameterSection> = None;
        let mut highest_section_rank = 0u8;
        let mut section_order_invalid = false;
        let mut tables_ordinal = 0usize;
        let mut using_ordinal = 0usize;
        let mut changing_ordinal = 0usize;
        let mut idx = 2usize;

        while idx < tokens.len() {
            let token = tokens[idx];
            if token.kind == TokenKind::Period {
                break;
            }

            if token.kind == TokenKind::Ident {
                let next_section = if self.token_matches_keyword(token, "tables") {
                    Some((PerformParameterSection::Tables, 1))
                } else if self.token_matches_keyword(token, "using") {
                    Some((PerformParameterSection::Using, 2))
                } else if self.token_matches_keyword(token, "changing") {
                    Some((PerformParameterSection::Changing, 3))
                } else {
                    None
                };
                if let Some((next_section, rank)) = next_section {
                    if rank <= highest_section_rank {
                        section_order_invalid = true;
                    } else {
                        highest_section_rank = rank;
                    }
                    section = Some(next_section);
                    idx += 1;
                    continue;
                }
            }

            let Some(current_section) = section else {
                idx += 1;
                continue;
            };
            let next_idx = self.consume_perform_argument(tokens, idx);
            if next_idx == idx {
                idx += 1;
                continue;
            }
            parameters.push(current_section);
            let ordinal_in_section = match current_section {
                PerformParameterSection::Tables => {
                    let current = tables_ordinal;
                    tables_ordinal += 1;
                    current
                }
                PerformParameterSection::Using => {
                    let current = using_ordinal;
                    using_ordinal += 1;
                    current
                }
                PerformParameterSection::Changing => {
                    let current = changing_ordinal;
                    changing_ordinal += 1;
                    current
                }
            };
            if let Some(last_token) = tokens.get(next_idx.saturating_sub(1)) {
                arguments.push(PerformArgumentData {
                    range: tokens[idx].range.start..last_token.range.end,
                    section: current_section,
                    ordinal_in_section,
                });
            }
            idx = next_idx;
        }

        let end = tokens
            .last()
            .map(|token| token.range.end)
            .unwrap_or(routine.range.end);
        self.perform_calls.push(PerformCallData {
            scope,
            range: tokens[0].range.start..end,
            routine_name,
            routine_range: routine.range.clone(),
            parameters,
            arguments,
            section_order_invalid,
        });
    }

    fn collect_token_expression_refs(
        &mut self,
        tokens: &[&Token],
        scope: ScopeId,
        allow_leading_value_ident: bool,
    ) {
        let mut idx = 0usize;
        while idx < tokens.len() {
            let token = tokens[idx];
            match token.kind {
                TokenKind::Comment => {
                    idx += 1;
                }
                TokenKind::LParen => {
                    if let Some(end_idx) = self.find_matching_group_end(
                        tokens,
                        idx,
                        TokenKind::LParen,
                        TokenKind::RParen,
                    ) {
                        self.collect_token_expression_refs(&tokens[idx + 1..end_idx], scope, true);
                        idx = end_idx + 1;
                    } else {
                        idx += 1;
                    }
                }
                TokenKind::LBracket => {
                    if let Some(end_idx) = self.find_matching_group_end(
                        tokens,
                        idx,
                        TokenKind::LBracket,
                        TokenKind::RBracket,
                    ) {
                        self.collect_token_expression_refs(&tokens[idx + 1..end_idx], scope, true);
                        idx = end_idx + 1;
                    } else {
                        idx += 1;
                    }
                }
                TokenKind::LBrace => {
                    if let Some(end_idx) = self.find_matching_group_end(
                        tokens,
                        idx,
                        TokenKind::LBrace,
                        TokenKind::RBrace,
                    ) {
                        self.collect_token_expression_refs(&tokens[idx + 1..end_idx], scope, true);
                        idx = end_idx + 1;
                    } else {
                        idx += 1;
                    }
                }
                TokenKind::Ident => {
                    if self.token_matches_keyword(token, "new") {
                        idx = self.collect_new_expression_tokens(tokens, idx, scope);
                        continue;
                    }
                    if let Some((next_idx, namespace, base_name, base_range, field_path)) =
                        self.consume_selector_access_from_tokens(tokens, idx)
                    {
                        let method_name =
                            field_path.last().map(|segment| Arc::clone(&segment.name));
                        let kind = if namespace == Namespace::Type {
                            ReferenceKind::StaticTarget
                        } else {
                            ReferenceKind::Identifier
                        };
                        self.add_reference(scope, base_name.clone(), namespace, kind, base_range);
                        if !field_path.is_empty() {
                            self.field_accesses.push(FieldAccess {
                                scope,
                                base_namespace: namespace,
                                base_name: Arc::clone(&base_name),
                                field_path,
                                in_type_position: false,
                            });
                        }
                        idx = next_idx;
                        if tokens.get(idx).map(|token| token.kind) == Some(TokenKind::LParen)
                            && let Some(end_idx) = self.find_matching_group_end(
                                tokens,
                                idx,
                                TokenKind::LParen,
                                TokenKind::RParen,
                            )
                        {
                            if let Some(method_name) = method_name {
                                self.collect_named_arguments_from_tokens(
                                    &tokens[idx + 1..end_idx],
                                    scope,
                                    NamedArgumentTarget::Method {
                                        base_namespace: namespace,
                                        base_name: Arc::clone(&base_name),
                                        method_name,
                                    },
                                );
                            } else {
                                self.collect_token_expression_refs(
                                    &tokens[idx + 1..end_idx],
                                    scope,
                                    true,
                                );
                            }
                            idx = end_idx + 1;
                        }
                        continue;
                    }
                    if self.token_is_expression_value_ident(tokens, idx, allow_leading_value_ident)
                    {
                        self.add_reference(
                            scope,
                            Arc::<str>::from(token.lexeme(self.source).to_ascii_lowercase()),
                            Namespace::Value,
                            ReferenceKind::Identifier,
                            token.range.clone(),
                        );
                    }
                    idx += 1;
                }
                _ => {
                    idx += 1;
                }
            }
        }
    }

    fn collect_new_expression_tokens(
        &mut self,
        tokens: &[&Token],
        idx: usize,
        scope: ScopeId,
    ) -> usize {
        let mut cursor = idx + 1;
        while matches!(
            tokens.get(cursor).map(|token| token.kind),
            Some(TokenKind::Comment)
        ) {
            cursor += 1;
        }
        let Some(lparen_idx) = tokens[cursor..]
            .iter()
            .position(|token| token.kind == TokenKind::LParen)
            .map(|relative| cursor + relative)
        else {
            return idx + 1;
        };
        if let Some((name, range)) =
            self.simple_type_ref_base_from_tokens(&tokens[cursor..lparen_idx])
        {
            self.add_reference(scope, name, Namespace::Type, ReferenceKind::TypeRef, range);
        }
        if let Some(rparen_idx) =
            self.find_matching_group_end(tokens, lparen_idx, TokenKind::LParen, TokenKind::RParen)
        {
            if let Some((name, _)) =
                self.simple_type_ref_base_from_tokens(&tokens[cursor..lparen_idx])
            {
                self.collect_named_arguments_from_tokens(
                    &tokens[lparen_idx + 1..rparen_idx],
                    scope,
                    NamedArgumentTarget::Constructor { type_name: name },
                );
            } else {
                self.collect_token_expression_refs(
                    &tokens[lparen_idx + 1..rparen_idx],
                    scope,
                    true,
                );
            }
            return rparen_idx + 1;
        }
        lparen_idx + 1
    }

    fn named_argument_target_for_callee(&self, callee: NodeId) -> Option<NamedArgumentTarget> {
        let (base_namespace, base_name, _, field_path) = self.selector_access_chain(callee)?;
        let method_name = field_path.last()?.name.clone();
        Some(NamedArgumentTarget::Method {
            base_namespace,
            base_name,
            method_name,
        })
    }

    fn named_argument_section(&self, token: &Token) -> Option<NamedArgumentSection> {
        self.named_argument_section_from_text(token.lexeme(self.source))
    }

    fn named_argument_section_from_text(&self, text: &str) -> Option<NamedArgumentSection> {
        if text.eq_ignore_ascii_case("exporting") {
            return Some(NamedArgumentSection::Exporting);
        }
        if text.eq_ignore_ascii_case("importing") {
            return Some(NamedArgumentSection::Importing);
        }
        if text.eq_ignore_ascii_case("changing") {
            return Some(NamedArgumentSection::Changing);
        }
        if text.eq_ignore_ascii_case("receiving") {
            return Some(NamedArgumentSection::Receiving);
        }
        if text.eq_ignore_ascii_case("exceptions") {
            return Some(NamedArgumentSection::Exceptions);
        }
        None
    }

    fn named_argument_section_allows_inline_target(
        &self,
        section: Option<NamedArgumentSection>,
    ) -> bool {
        matches!(
            section,
            Some(
                NamedArgumentSection::Importing
                    | NamedArgumentSection::Changing
                    | NamedArgumentSection::Receiving
            )
        )
    }

    fn call_argument_value_end(&self, tokens: &[&Token], start_idx: usize) -> usize {
        let mut paren = 0i32;
        let mut bracket = 0i32;
        let mut brace = 0i32;
        let mut idx = start_idx;
        while idx < tokens.len() {
            let token = tokens[idx];
            match token.kind {
                TokenKind::LParen => paren += 1,
                TokenKind::RParen => paren -= 1,
                TokenKind::LBracket => bracket += 1,
                TokenKind::RBracket => bracket -= 1,
                TokenKind::LBrace => brace += 1,
                TokenKind::RBrace => brace -= 1,
                _ => {}
            }
            if paren == 0 && bracket == 0 && brace == 0 {
                if token.kind == TokenKind::Ident && self.named_argument_section(token).is_some() {
                    break;
                }
                if token.kind == TokenKind::Ident
                    && tokens.get(idx + 1).map(|next| next.kind) == Some(TokenKind::Eq)
                {
                    break;
                }
            }
            idx += 1;
        }
        idx
    }

    fn resolve_method_target_class_symbol(
        &self,
        scope: ScopeId,
        base_namespace: Namespace,
        base_name: &Arc<str>,
    ) -> Option<SymbolId> {
        match base_namespace {
            Namespace::Type => self
                .lookup_symbol_in_scope_chain(scope, Namespace::Type, base_name.as_ref())
                .filter(|&symbol_id| self.symbol(symbol_id).kind == SymbolKind::Class),
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
                .filter(|&class_symbol_id| self.symbol(class_symbol_id).kind == SymbolKind::Class)
            }
            Namespace::Routine => None,
        }
    }

    fn resolve_named_argument_declared_type(
        &self,
        scope: ScopeId,
        target: &NamedArgumentTarget,
        argument_name: &Arc<str>,
    ) -> Option<FieldTypeRefData> {
        match target {
            NamedArgumentTarget::Constructor { type_name } => {
                let class_symbol =
                    self.lookup_symbol_in_scope_chain(scope, Namespace::Type, type_name.as_ref())?;
                let signature = self.class_method_signature(class_symbol, "constructor", scope)?;
                signature
                    .parameters
                    .iter()
                    .find(|param| param.name == *argument_name)
                    .and_then(|param| param.declared_type.clone())
            }
            NamedArgumentTarget::Routine { .. } => None,
            NamedArgumentTarget::ImplicitMethod { method_name } => {
                let class_symbol = self.enclosing_class_owner(scope)?;
                let signature =
                    self.class_method_signature(class_symbol, method_name.as_ref(), scope)?;
                signature
                    .parameters
                    .iter()
                    .find(|param| param.name == *argument_name)
                    .and_then(|param| param.declared_type.clone())
            }
            NamedArgumentTarget::Method {
                base_namespace,
                base_name,
                method_name,
            } => {
                let class_symbol =
                    self.resolve_method_target_class_symbol(scope, *base_namespace, base_name)?;
                let signature =
                    self.class_method_signature(class_symbol, method_name.as_ref(), scope)?;
                signature
                    .parameters
                    .iter()
                    .find(|param| param.name == *argument_name)
                    .and_then(|param| param.declared_type.clone())
            }
        }
    }

    fn declare_inline_named_argument_target(
        &mut self,
        scope: ScopeId,
        target: &NamedArgumentTarget,
        section: Option<NamedArgumentSection>,
        argument_name: Arc<str>,
        value_tokens: &[&Token],
    ) -> bool {
        if !self.named_argument_section_allows_inline_target(section) {
            return false;
        }
        let mut idx = 0usize;
        while matches!(
            value_tokens.get(idx).map(|token| token.kind),
            Some(TokenKind::Comment)
        ) {
            idx += 1;
        }
        let Some(token) = value_tokens.get(idx) else {
            return false;
        };
        let declared_type =
            self.resolve_named_argument_declared_type(scope, target, &argument_name);
        let structure = declared_type
            .as_ref()
            .and_then(|type_ref| self.resolve_field_type_ref(scope, type_ref));
        if self.token_matches_keyword(token, "data")
            && value_tokens.get(idx + 1).map(|token| token.kind) == Some(TokenKind::LParen)
            && let Some(name_tok) = value_tokens.get(idx + 2)
            && name_tok.kind == TokenKind::Ident
            && value_tokens.get(idx + 3).map(|token| token.kind) == Some(TokenKind::RParen)
        {
            self.declare_symbol(
                scope,
                Arc::<str>::from(name_tok.lexeme(self.source).to_ascii_lowercase()),
                SymbolKind::Variable,
                name_tok.range.clone(),
                structure,
                declared_type,
                None,
            );
            return true;
        }
        if self.token_matches_keyword(token, "field")
            && value_tokens.get(idx + 1).map(|token| token.kind) == Some(TokenKind::Minus)
            && value_tokens
                .get(idx + 2)
                .is_some_and(|token| self.token_matches_keyword(token, "symbol"))
            && value_tokens.get(idx + 3).map(|token| token.kind) == Some(TokenKind::LParen)
            && let Some(name_tok) = value_tokens.get(idx + 4)
            && name_tok.kind == TokenKind::Ident
            && value_tokens.get(idx + 5).map(|token| token.kind) == Some(TokenKind::RParen)
        {
            self.declare_symbol(
                scope,
                Arc::<str>::from(name_tok.lexeme(self.source).to_ascii_lowercase()),
                SymbolKind::FieldSymbol,
                name_tok.range.clone(),
                structure,
                declared_type,
                None,
            );
            return true;
        }
        false
    }

    fn declare_inline_named_argument_target_from_nodes(
        &mut self,
        scope: ScopeId,
        target: &NamedArgumentTarget,
        section: Option<NamedArgumentSection>,
        argument_name: Arc<str>,
        value_children: &[NodeId],
    ) -> Option<bool> {
        if !self.named_argument_section_allows_inline_target(section) {
            return Some(false);
        }
        let first_value = value_children
            .iter()
            .copied()
            .find(|&node| self.file.kind(node) != SyntaxKind::Token)?;
        let declared_type =
            self.resolve_named_argument_declared_type(scope, target, &argument_name);
        let structure = declared_type
            .as_ref()
            .and_then(|type_ref| self.resolve_field_type_ref(scope, type_ref));
        let Some(name_node) = self
            .syntax(first_value)
            .child_by_kind(SyntaxKind::DataDeclName)
            .and_then(DataDeclName::cast)
        else {
            return Some(false);
        };
        let Some(name) = name_node.name(self.source) else {
            return Some(false);
        };
        match self.file.kind(first_value) {
            SyntaxKind::DataInlineDecl => {
                self.declare_symbol(
                    scope,
                    name,
                    SymbolKind::Variable,
                    name_node.range(),
                    structure,
                    declared_type,
                    None,
                );
                Some(true)
            }
            SyntaxKind::FieldSymbolInlineDecl => {
                self.declare_symbol(
                    scope,
                    name,
                    SymbolKind::FieldSymbol,
                    name_node.range(),
                    structure,
                    declared_type,
                    None,
                );
                Some(true)
            }
            _ => Some(false),
        }
    }

    fn collect_call_argument_tokens(
        &mut self,
        tokens: &[&Token],
        scope: ScopeId,
        target: NamedArgumentTarget,
    ) {
        let mut idx = 0usize;
        let mut segment_start = 0usize;
        let mut current_section = None;
        while idx < tokens.len() {
            let token = tokens[idx];
            if token.kind == TokenKind::Comment {
                idx += 1;
                continue;
            }
            if token.kind == TokenKind::Ident
                && let Some(section) = self.named_argument_section(token)
            {
                if segment_start < idx {
                    self.collect_token_expression_refs(&tokens[segment_start..idx], scope, true);
                }
                current_section = Some(section);
                idx += 1;
                segment_start = idx;
                continue;
            }
            if token.kind == TokenKind::Ident
                && tokens.get(idx + 1).map(|next| next.kind) == Some(TokenKind::Eq)
            {
                if segment_start < idx {
                    self.collect_token_expression_refs(&tokens[segment_start..idx], scope, true);
                }
                let argument_name =
                    Arc::<str>::from(token.lexeme(self.source).to_ascii_lowercase());
                let value_start = idx + 2;
                let value_end = self.call_argument_value_end(tokens, value_start);
                self.named_arguments.push(NamedArgumentAccess {
                    scope,
                    name: Arc::clone(&argument_name),
                    range: token.range.clone(),
                    section: current_section,
                    target: target.clone(),
                });
                let consumed_inline_target = self.declare_inline_named_argument_target(
                    scope,
                    &target,
                    current_section,
                    argument_name,
                    &tokens[value_start..value_end],
                );
                if !consumed_inline_target {
                    self.collect_token_expression_refs(
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
            self.collect_token_expression_refs(&tokens[segment_start..], scope, true);
        }
    }

    fn collect_named_arguments_from_tokens(
        &mut self,
        tokens: &[&Token],
        scope: ScopeId,
        target: NamedArgumentTarget,
    ) {
        self.collect_call_argument_tokens(tokens, scope, target);
    }

    fn call_arg_section_from_node(&self, node: NodeId) -> Option<NamedArgumentSection> {
        abap_ast::ast::CallArgSection::cast(self.syntax(node))
            .and_then(|section| section.first_token())
            .and_then(|token| token.text(self.source))
            .and_then(|text| self.named_argument_section_from_text(text))
    }

    fn collect_structured_argument_values(&mut self, nodes: &[NodeId], scope: ScopeId) {
        if nodes.is_empty() {
            return;
        }
        if nodes
            .iter()
            .all(|&node| self.file.kind(node) == SyntaxKind::Token)
        {
            let tokens = self.token_refs_for_nodes(nodes);
            self.collect_token_expression_refs(&tokens, scope, true);
            return;
        }

        for &node in nodes {
            match self.file.kind(node) {
                SyntaxKind::DataInlineDecl => self.walk_inline_decl(node, scope),
                SyntaxKind::FieldSymbolInlineDecl => {
                    self.walk_inline_field_symbol_decl(node, scope)
                }
                SyntaxKind::Token => {}
                _ => self.collect_expr(node, scope),
            }
        }
    }

    fn collect_structured_named_argument(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        target: &NamedArgumentTarget,
        section: Option<NamedArgumentSection>,
    ) {
        let Some(named_arg) = CallNamedArg::cast(self.syntax(node)) else {
            return;
        };
        let value_children: Vec<_> = named_arg
            .value_children()
            .into_iter()
            .map(|child| child.id())
            .collect();
        let Some(name_token) = named_arg.name_token() else {
            return;
        };
        let Some(name_text) = name_token.text(self.source) else {
            return;
        };
        let argument_name = Arc::<str>::from(name_text.to_ascii_lowercase());
        self.named_arguments.push(NamedArgumentAccess {
            scope,
            name: Arc::clone(&argument_name),
            range: name_token.range(),
            section,
            target: target.clone(),
        });

        let consumed_inline_target = self
            .declare_inline_named_argument_target_from_nodes(
                scope,
                target,
                section,
                Arc::clone(&argument_name),
                &value_children,
            )
            .unwrap_or_else(|| {
                let value_tokens = self.token_refs_for_nodes(&value_children);
                self.declare_inline_named_argument_target(
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

    fn collect_call_argument_list(
        &mut self,
        node: NodeId,
        scope: ScopeId,
        target: NamedArgumentTarget,
    ) {
        let Some(arg_list) = CallArgList::cast(self.syntax(node)) else {
            return;
        };
        let items: Vec<_> = arg_list
            .items()
            .map(|child| (child.id(), child.kind()))
            .collect();
        let mut current_section = None;
        for (child, kind_syntax) in items {
            match kind_syntax {
                SyntaxKind::CallArgSection => {
                    current_section = self.call_arg_section_from_node(child);
                }
                SyntaxKind::CallNamedArg => {
                    self.collect_structured_named_argument(child, scope, &target, current_section);
                }
                SyntaxKind::CallPositionalArg => {
                    let value_children: Vec<_> = CallPositionalArg::cast(self.syntax(child))
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

    fn token_is_expression_value_ident(
        &self,
        tokens: &[&Token],
        idx: usize,
        allow_leading_value_ident: bool,
    ) -> bool {
        let token = tokens[idx];
        if token.kind != TokenKind::Ident
            || self.token_matches_keyword(token, "new")
            || self.token_matches_keyword(token, "ref")
            || self.token_matches_keyword(token, "to")
        {
            return false;
        }
        if matches!(
            tokens.get(idx + 1).map(|token| token.kind),
            Some(
                TokenKind::Eq
                    | TokenKind::Arrow
                    | TokenKind::FatArrow
                    | TokenKind::Tilde
                    | TokenKind::Minus
            )
        ) {
            return false;
        }
        let prev_kind = idx
            .checked_sub(1)
            .and_then(|prev| tokens.get(prev))
            .map(|token| token.kind);
        allow_leading_value_ident && idx == 0
            || matches!(
                prev_kind,
                Some(
                    TokenKind::Eq
                        | TokenKind::Comma
                        | TokenKind::LParen
                        | TokenKind::LBracket
                        | TokenKind::LBrace
                        | TokenKind::Slash
                        | TokenKind::Plus
                        | TokenKind::Minus
                        | TokenKind::Star
                        | TokenKind::Ampersand
                        | TokenKind::Lt
                        | TokenKind::Gt
                        | TokenKind::Le
                        | TokenKind::Ge
                        | TokenKind::Ne
                        | TokenKind::QuestionEq
                )
            )
    }

    fn consume_perform_argument(&self, tokens: &[&Token], start: usize) -> usize {
        let mut idx = start;
        let mut paren = 0i32;
        let mut bracket = 0i32;
        let mut brace = 0i32;
        let mut consumed_any = false;

        while idx < tokens.len() {
            let token = tokens[idx];
            if paren == 0 && bracket == 0 && brace == 0 {
                if token.kind == TokenKind::Period {
                    break;
                }
                if token.kind == TokenKind::Ident
                    && (self.token_matches_keyword(token, "tables")
                        || self.token_matches_keyword(token, "using")
                        || self.token_matches_keyword(token, "changing"))
                {
                    break;
                }
                if consumed_any && self.token_starts_perform_argument(tokens, idx) {
                    break;
                }
            }

            consumed_any = true;
            match token.kind {
                TokenKind::LParen => paren += 1,
                TokenKind::RParen => paren -= 1,
                TokenKind::LBracket => bracket += 1,
                TokenKind::RBracket => bracket -= 1,
                TokenKind::LBrace => brace += 1,
                TokenKind::RBrace => brace -= 1,
                _ => {}
            }
            idx += 1;
        }

        idx
    }

    fn find_top_level_keyword_index(
        &self,
        tokens: &[&Token],
        start: usize,
        keyword: &str,
    ) -> Option<usize> {
        let mut paren = 0i32;
        let mut bracket = 0i32;
        let mut brace = 0i32;
        let mut idx = start;
        while idx < tokens.len() {
            let token = tokens[idx];
            if paren == 0
                && bracket == 0
                && brace == 0
                && self.token_matches_keyword(token, keyword)
            {
                return Some(idx);
            }
            match token.kind {
                TokenKind::LParen => paren += 1,
                TokenKind::RParen => paren -= 1,
                TokenKind::LBracket => bracket += 1,
                TokenKind::RBracket => bracket -= 1,
                TokenKind::LBrace => brace += 1,
                TokenKind::RBrace => brace -= 1,
                _ => {}
            }
            idx += 1;
        }
        None
    }

    fn consume_concatenate_operand(
        &self,
        tokens: &[&Token],
        start: usize,
        clause_keywords: &[&str],
    ) -> usize {
        let mut idx = start;
        let mut paren = 0i32;
        let mut bracket = 0i32;
        let mut brace = 0i32;
        let mut consumed_any = false;

        while idx < tokens.len() {
            let token = tokens[idx];
            if paren == 0 && bracket == 0 && brace == 0 {
                if token.kind == TokenKind::Period {
                    break;
                }
                if token.kind == TokenKind::Ident
                    && clause_keywords
                        .iter()
                        .any(|keyword| self.token_matches_keyword(token, keyword))
                {
                    break;
                }
                if consumed_any && self.token_starts_concatenate_operand(tokens, idx) {
                    break;
                }
            }

            consumed_any = true;
            match token.kind {
                TokenKind::LParen => paren += 1,
                TokenKind::RParen => paren -= 1,
                TokenKind::LBracket => bracket += 1,
                TokenKind::RBracket => bracket -= 1,
                TokenKind::LBrace => brace += 1,
                TokenKind::RBrace => brace -= 1,
                _ => {}
            }
            idx += 1;
        }

        idx
    }

    fn token_starts_concatenate_operand(&self, tokens: &[&Token], idx: usize) -> bool {
        if !self.token_starts_perform_argument(tokens, idx) {
            return false;
        }
        let Some(prev) = idx.checked_sub(1).and_then(|prev_idx| tokens.get(prev_idx)) else {
            return true;
        };
        !(prev.kind == TokenKind::Ident
            && (self.token_matches_keyword(prev, "new")
                || self.token_matches_keyword(prev, "ref")
                || self.token_matches_keyword(prev, "to")))
    }

    fn token_starts_perform_argument(&self, tokens: &[&Token], idx: usize) -> bool {
        let Some(token) = tokens.get(idx) else {
            return false;
        };
        if !matches!(
            token.kind,
            TokenKind::Ident
                | TokenKind::Number
                | TokenKind::String
                | TokenKind::StringTemplate
                | TokenKind::LParen
                | TokenKind::LBracket
                | TokenKind::LBrace
                | TokenKind::At
                | TokenKind::Hash
        ) {
            return false;
        }
        if token.kind == TokenKind::Ident
            && (self.token_matches_keyword(token, "tables")
                || self.token_matches_keyword(token, "using")
                || self.token_matches_keyword(token, "changing"))
        {
            return false;
        }
        let Some(prev) = idx.checked_sub(1).and_then(|prev_idx| tokens.get(prev_idx)) else {
            return true;
        };
        have_space_between(prev, token)
            && !matches!(
                prev.kind,
                TokenKind::Arrow
                    | TokenKind::FatArrow
                    | TokenKind::Tilde
                    | TokenKind::Eq
                    | TokenKind::Minus
                    | TokenKind::Plus
                    | TokenKind::Star
                    | TokenKind::Slash
                    | TokenKind::Lt
                    | TokenKind::Gt
                    | TokenKind::Le
                    | TokenKind::Ge
                    | TokenKind::Ne
                    | TokenKind::QuestionEq
                    | TokenKind::LParen
                    | TokenKind::LBracket
                    | TokenKind::LBrace
                    | TokenKind::At
                    | TokenKind::Hash
                    | TokenKind::Ampersand
                    | TokenKind::Pipe
            )
    }

    fn consume_selector_access_from_tokens(
        &self,
        tokens: &[&Token],
        idx: usize,
    ) -> Option<(
        usize,
        Namespace,
        Arc<str>,
        TextRange,
        Vec<FieldAccessSegment>,
    )> {
        let base = *tokens.get(idx)?;
        if base.kind != TokenKind::Ident {
            return None;
        }
        let mut cursor = idx;
        let mut namespace = None;
        let mut field_path = Vec::new();
        while cursor + 2 < tokens.len() {
            let op = tokens[cursor + 1];
            let field = tokens[cursor + 2];
            if field.kind != TokenKind::Ident
                && !(op.kind == TokenKind::Arrow && field.kind == TokenKind::Star)
            {
                break;
            }
            let step_namespace = match op.kind {
                TokenKind::FatArrow => Namespace::Type,
                TokenKind::Arrow | TokenKind::Tilde => Namespace::Value,
                TokenKind::Minus
                    if !have_space_between(tokens[cursor], op)
                        && !have_space_between(op, field) =>
                {
                    Namespace::Value
                }
                _ => break,
            };
            namespace.get_or_insert(step_namespace);
            field_path.push(FieldAccessSegment {
                name: Arc::<str>::from(field.lexeme(self.source).to_ascii_lowercase()),
                range: field.range.clone(),
            });
            cursor += 2;
        }
        Some((
            cursor + 1,
            namespace?,
            Arc::<str>::from(base.lexeme(self.source).to_ascii_lowercase()),
            base.range.clone(),
            field_path,
        ))
    }

    fn find_matching_group_end(
        &self,
        tokens: &[&Token],
        start_idx: usize,
        open_kind: TokenKind,
        close_kind: TokenKind,
    ) -> Option<usize> {
        let mut depth = 0i32;
        for (idx, token) in tokens.iter().enumerate().skip(start_idx) {
            if token.kind == open_kind {
                depth += 1;
            } else if token.kind == close_kind {
                depth -= 1;
                if depth == 0 {
                    return Some(idx);
                }
            }
        }
        None
    }

    fn collect_create_object_stmt(&mut self, tokens: &[&Token], scope: ScopeId) {
        if tokens.len() < 3
            || !self.token_matches_keyword(tokens[0], "create")
            || !self.token_matches_keyword(tokens[1], "object")
        {
            return;
        }

        let target = tokens[2];
        if target.kind == TokenKind::Ident {
            let name = Arc::<str>::from(target.lexeme(self.source).to_ascii_lowercase());
            self.add_reference(
                scope,
                name,
                Namespace::Value,
                ReferenceKind::Identifier,
                target.range.clone(),
            );
        }

        for idx in 3..tokens.len() {
            let token = tokens[idx];
            if !self.token_matches_keyword(token, "type") {
                continue;
            }
            if let Some((name, range)) = self.simple_type_ref_base_from_tokens(&tokens[idx + 1..]) {
                self.add_reference(scope, name, Namespace::Type, ReferenceKind::TypeRef, range);
            }
            break;
        }
    }

    fn collect_selector_expr(&mut self, node: NodeId, scope: ScopeId) {
        if let Some((namespace, base_name, base_range, field_path)) =
            self.selector_access_chain(node)
        {
            let kind = if namespace == Namespace::Type {
                ReferenceKind::StaticTarget
            } else {
                ReferenceKind::Identifier
            };
            self.add_reference(scope, Arc::clone(&base_name), namespace, kind, base_range);
            if !field_path.is_empty() {
                self.field_accesses.push(FieldAccess {
                    scope,
                    base_namespace: namespace,
                    base_name,
                    field_path,
                    in_type_position: false,
                });
            }
            return;
        }

        let mut children = self.file.children(node);
        let base = children.next();
        let op = children.next();
        let field = children.next();
        let Some(base) = base else {
            return;
        };
        let namespace = match op.and_then(|op_node| self.token_for_node(op_node)) {
            Some(token) if token.kind == TokenKind::FatArrow => Namespace::Type,
            _ => Namespace::Value,
        };
        match self.file.kind(base) {
            SyntaxKind::ExprIdent => {
                if let Some((name, range)) = self.node_name(base) {
                    let kind = if namespace == Namespace::Type {
                        ReferenceKind::StaticTarget
                    } else {
                        ReferenceKind::Identifier
                    };
                    self.add_reference(scope, name, namespace, kind, range);
                }
            }
            _ => self.collect_expr(base, scope),
        }
        if let Some(field_node) = field
            && self.file.kind(field_node) != SyntaxKind::ExprIdent
        {
            self.collect_expr(field_node, scope);
        }
    }

    fn collect_substring_expr(&mut self, node: NodeId, scope: ScopeId) {
        let mut children = self.file.children(node);
        let Some(base) = children.next() else {
            return;
        };

        match self.file.kind(base) {
            SyntaxKind::ExprIdent => {
                if let Some((name, range)) = self.node_name(base) {
                    let namespace = if self
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
                    self.add_reference(scope, name, namespace, kind, range);
                }
            }
            SyntaxKind::SelectorExpr => self.collect_selector_expr(base, scope),
            _ => self.collect_expr(base, scope),
        }

        for child in children {
            if self.file.kind(child) != SyntaxKind::Token {
                self.collect_expr(child, scope);
            }
        }
    }

    fn collect_call_expr(&mut self, node: NodeId, scope: ScopeId) {
        let Some(call) = CallExpr::cast(self.syntax(node)) else {
            return;
        };
        let callee = call.callee().map(|callee| (callee.id(), callee.kind()));
        let arg_list = call.arg_list().map(|arg_list| arg_list.syntax().id());
        if let Some((callee_id, callee_kind)) = callee {
            match callee_kind {
                SyntaxKind::ExprIdent => {
                    if let Some((name, range)) = self.node_name(callee_id) {
                        self.add_reference(
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
            if let Some(target) = self.named_argument_target_for_callee(callee_id)
                && let Some(arg_list) = arg_list
            {
                self.collect_call_argument_list(arg_list, scope, target);
            }
        }
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
            .filter_map(|child| self.token_for_node(child));
        let begin_tok = tokens.next()?;
        let of_tok = tokens.next()?;
        let name_tok = tokens.next()?;
        if !self.token_matches_keyword(begin_tok, "begin")
            || !self.token_matches_keyword(of_tok, "of")
            || name_tok.kind != TokenKind::Ident
        {
            return None;
        }
        Some(name_tok.range.clone())
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

    fn structure(&self, id: StructureId) -> Option<&StructureData> {
        self.structures.get(id.as_usize())
    }

    fn provided_names(&self) -> Vec<Arc<str>> {
        let mut names = Vec::new();
        for symbol in &self.symbols {
            if symbol.scope == ScopeId(0)
                && !symbol.kind.is_builtin()
                && matches!(
                    symbol.kind,
                    SymbolKind::Class
                        | SymbolKind::Interface
                        | SymbolKind::Report
                        | SymbolKind::TypeDef
                )
            {
                names.push(Arc::clone(&symbol.name));
            }
        }
        if let Some(stem) = Path::new(self.uri.as_ref())
            .file_stem()
            .and_then(|s| s.to_str())
        {
            names.push(Arc::<str>::from(stem.to_ascii_lowercase()));
        }
        names.sort();
        names.dedup();
        names
    }

    fn symbol(&self, id: SymbolId) -> &SymbolData {
        &self.symbols[id.as_usize()]
    }

    fn header_ident_after_keyword(&self, node: NodeId) -> Option<(Arc<str>, TextRange)> {
        let mut saw_keyword = false;
        for child in self.file.children(node) {
            let Some(token) = self.token_for_node(child) else {
                continue;
            };
            if token.kind == TokenKind::Period {
                break;
            }
            if !saw_keyword {
                saw_keyword = token.kind == TokenKind::Ident;
                continue;
            }
            if token.kind == TokenKind::Ident {
                let text = token.lexeme(self.source);
                if !matches!(
                    text.to_ascii_uppercase().as_str(),
                    "DEFINITION"
                        | "IMPLEMENTATION"
                        | "PUBLIC"
                        | "PROTECTED"
                        | "PRIVATE"
                        | "SECTION"
                ) {
                    return Some((
                        Arc::<str>::from(text.to_ascii_lowercase()),
                        token.range.clone(),
                    ));
                }
            }
        }
        None
    }

    fn event_block_header_name(&self, node: NodeId) -> Option<(Arc<str>, TextRange)> {
        let tokens: Vec<_> = self
            .file
            .children(node)
            .filter_map(|child| self.token_for_node(child))
            .take_while(|token| token.kind != TokenKind::Period)
            .filter(|token| token.kind != TokenKind::Comment)
            .collect();
        let (first, last) = match tokens.as_slice() {
            [token] if self.token_matches_keyword(token, "initialization") => (*token, *token),
            [start, minus_1, of, minus_2, end]
                if self.token_matches_keyword(start, "start")
                    && minus_1.kind == TokenKind::Minus
                    && self.token_matches_keyword(of, "of")
                    && minus_2.kind == TokenKind::Minus
                    && self.token_matches_keyword(end, "selection") =>
            {
                (*start, *end)
            }
            [start, minus_1, of, minus_2, end]
                if self.token_matches_keyword(start, "end")
                    && minus_1.kind == TokenKind::Minus
                    && self.token_matches_keyword(of, "of")
                    && minus_2.kind == TokenKind::Minus
                    && self.token_matches_keyword(end, "selection") =>
            {
                (*start, *end)
            }
            [start, minus_1, of, minus_2, end]
                if self.token_matches_keyword(start, "top")
                    && minus_1.kind == TokenKind::Minus
                    && self.token_matches_keyword(of, "of")
                    && minus_2.kind == TokenKind::Minus
                    && self.token_matches_keyword(end, "page") =>
            {
                (*start, *end)
            }
            [start, minus_1, of, minus_2, end]
                if self.token_matches_keyword(start, "end")
                    && minus_1.kind == TokenKind::Minus
                    && self.token_matches_keyword(of, "of")
                    && minus_2.kind == TokenKind::Minus
                    && self.token_matches_keyword(end, "page") =>
            {
                (*start, *end)
            }
            _ => return None,
        };
        Some((
            Arc::<str>::from(self.source[first.range.start..last.range.end].to_ascii_lowercase()),
            first.range.start..last.range.end,
        ))
    }

    fn constructor_type_ref(&self, node: NodeId) -> Option<(Arc<str>, TextRange)> {
        let type_ref = self.syntax(node).child_by_kind(SyntaxKind::TypeRefSimple)?;
        let (_, _, base_name, range, _) =
            self.type_ref_access_chain(type_ref.id(), Namespace::Type)?;
        Some((base_name, range))
    }

    fn simple_type_ref_base_from_tokens(&self, tokens: &[&Token]) -> Option<(Arc<str>, TextRange)> {
        let mut i = 0usize;
        if tokens
            .get(i)
            .is_some_and(|tok| self.token_matches_keyword(tok, "ref"))
        {
            let to_tok = tokens.get(i + 1)?;
            if !self.token_matches_keyword(to_tok, "to") {
                return None;
            }
            i += 2;
        }
        let token = tokens.get(i)?;
        if token.kind != TokenKind::Ident {
            return None;
        }
        Some((
            Arc::<str>::from(token.lexeme(self.source).to_ascii_lowercase()),
            token.range.clone(),
        ))
    }

    fn node_name(&self, node: NodeId) -> Option<(Arc<str>, TextRange)> {
        let syntax = self.syntax(node);
        Some((syntax.lower_trimmed_text(self.source)?, syntax.range()))
    }

    fn token_for_node(&self, node: NodeId) -> Option<&'a Token> {
        if self.file.kind(node) != SyntaxKind::Token {
            return None;
        }
        let range = self.file.range(node);
        let idx = self.token_index_by_range.get(&(range.start, range.end))?;
        self.tokens.get(*idx)
    }

    fn first_non_token_child(&self, node: NodeId) -> Option<NodeId> {
        self.syntax(node)
            .first_non_token_child()
            .map(|child| child.id())
    }

    fn last_non_token_child(&self, node: NodeId) -> Option<NodeId> {
        self.syntax(node)
            .last_non_token_child()
            .map(|child| child.id())
    }

    fn token_refs(&self, node: NodeId) -> Vec<&'a Token> {
        self.syntax(node)
            .token_descendants()
            .into_iter()
            .filter_map(|token_node| self.token_for_node(token_node.id()))
            .collect()
    }

    fn token_refs_for_nodes(&self, nodes: &[NodeId]) -> Vec<&'a Token> {
        let mut out = Vec::new();
        for &node in nodes {
            out.extend(self.token_refs(node));
        }
        out
    }

    fn token_refs_from_child_nodes(&self, node: NodeId) -> Vec<&'a Token> {
        let child_nodes = self.file.children(node).collect::<Vec<_>>();
        self.token_refs_for_nodes(&child_nodes)
    }

    fn token_matches_keyword(&self, token: &Token, keyword: &str) -> bool {
        token.kind == TokenKind::Ident && token.lexeme(self.source).eq_ignore_ascii_case(keyword)
    }

    fn selector_access_chain(
        &self,
        node: NodeId,
    ) -> Option<(Namespace, Arc<str>, TextRange, Vec<FieldAccessSegment>)> {
        let selector = SelectorExpr::cast(self.syntax(node))?;
        let base = selector.base()?;
        let op = selector.operator()?;
        let field = selector.field()?;
        let field_name = field.name(self.source)?;
        let field_range = field.range();
        let namespace = match self.token_for_node(op.id()) {
            Some(token) if token.kind == TokenKind::FatArrow => Namespace::Type,
            _ => Namespace::Value,
        };
        match base.kind() {
            SyntaxKind::ExprIdent => {
                let ident = ExprIdent::cast(base)?;
                let base_name = ident.name(self.source)?;
                let base_range = ident.range();
                Some((
                    namespace,
                    base_name,
                    base_range,
                    vec![FieldAccessSegment {
                        name: field_name,
                        range: field_range,
                    }],
                ))
            }
            SyntaxKind::SelectorExpr => {
                let (base_namespace, base_name, base_range, mut field_path) =
                    self.selector_access_chain(base.id())?;
                field_path.push(FieldAccessSegment {
                    name: field_name,
                    range: field_range,
                });
                Some((base_namespace, base_name, base_range, field_path))
            }
            _ => None,
        }
    }

    fn syntax(&self, node: NodeId) -> SyntaxNodeRef<'_> {
        SyntaxNodeRef::new(self.file, node)
    }

    fn type_ref_selector_chain_access_chain(
        &self,
        node: NodeId,
        simple_name_ns: Namespace,
    ) -> Option<(Namespace, Arc<str>, TextRange, Vec<FieldAccessSegment>)> {
        let mut children = self.file.children(node);
        let base = children.next()?;
        let (base_name, base_range) = self.node_name(base)?;
        let mut namespace = None;
        let mut field_path = Vec::new();
        while let Some(op_node) = children.next() {
            let segment_node = children.next()?;
            let op = self.token_for_node(op_node)?;
            if namespace.is_none() {
                namespace = Some(match op.kind {
                    TokenKind::FatArrow => Namespace::Type,
                    _ => Namespace::Value,
                });
            }
            let (name, range) = self.node_name(segment_node)?;
            field_path.push(FieldAccessSegment { name, range });
        }
        Some((
            namespace.unwrap_or(simple_name_ns),
            base_name,
            base_range,
            field_path,
        ))
    }

    fn type_ref_access_chain(
        &self,
        node: NodeId,
        simple_name_ns: Namespace,
    ) -> Option<(
        Namespace,
        bool,
        Arc<str>,
        TextRange,
        Vec<FieldAccessSegment>,
    )> {
        match self.file.kind(node) {
            SyntaxKind::TypeRefName => {
                let (name, range) = self.node_name(node)?;
                Some((simple_name_ns, false, name, range, Vec::new()))
            }
            SyntaxKind::TypeRefSelectorChain => {
                let (namespace, base_name, base_range, field_path) =
                    self.type_ref_selector_chain_access_chain(node, simple_name_ns)?;
                Some((namespace, false, base_name, base_range, field_path))
            }
            SyntaxKind::TypeRefSimple => {
                let mut is_ref = false;
                for child in self.file.children(node) {
                    if let Some(token) = self.token_for_node(child) {
                        if self.token_matches_keyword(token, "ref") {
                            is_ref = true;
                        }
                        continue;
                    }
                    if matches!(
                        self.file.kind(child),
                        SyntaxKind::TypeRefSimple
                            | SyntaxKind::TypeRefName
                            | SyntaxKind::TypeRefSelectorChain
                    ) {
                        let (namespace, nested_ref, base_name, base_range, field_path) =
                            self.type_ref_access_chain(child, simple_name_ns)?;
                        return Some((
                            namespace,
                            is_ref || nested_ref,
                            base_name,
                            base_range,
                            field_path,
                        ));
                    }
                }
                None
            }
            _ => None,
        }
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
