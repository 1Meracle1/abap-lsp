use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

use abap_ast::arena::NodeId;
use abap_ast::{File, SyntaxKind};
use abap_lexer::{TextRange, Token, TokenKind};

use crate::builtins::{BUILTIN_STRUCTURES, BUILTIN_SYMBOLS, BuiltinTypeKind};
use crate::def_map::{
    Diagnostic, DiagnosticKind, FieldAccess, FieldAccessSegment, FieldTypeRefData, IncludeEdge,
    ReferenceData, ReferenceKind, StructureData, StructureFieldData, SymbolData, SymbolKind, UnitAnalysis,
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
    structure: Option<PendingStructure>,
    type_ref: Option<FieldTypeRefData>,
}

#[derive(Debug, Clone)]
struct PendingStructure {
    name: Arc<str>,
    fields: Vec<PendingStructureField>,
}

#[derive(Clone, Copy)]
enum FormHeaderParamSection {
    Tables,
    UsingOrChanging,
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
    scope_symbols: Vec<HashMap<ScopeLookupKey, Vec<SymbolId>>>,
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
            scope_symbols: Vec::new(),
        }
    }

    pub fn collect(mut self) -> UnitAnalysis {
        let root = self.file.root();
        let root_scope = self.push_scope(ScopeKind::File, self.file.range(root), None, None);
        self.install_builtin_symbols(root_scope);
        self.walk_children(root, root_scope);
        let provided_names = self.provided_names();
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
    ) -> SymbolId {
        let id = SymbolId(self.symbols.len() as u32);
        self.symbols.push(SymbolData {
            id,
            name: Arc::clone(&name),
            kind,
            scope,
            decl_range: decl_range.clone(),
            structure,
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
        self.declare_symbol(scope, name, kind, decl_range, None)
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
        let fields = structure
            .fields
            .into_iter()
            .map(|field| StructureFieldData {
                name: field.name,
                structure: field
                    .structure
                    .map(|nested| self.register_structure(scope, nested))
                    .or_else(|| field.type_ref.as_ref().and_then(|type_ref| self.resolve_field_type_ref(scope, type_ref))),
                type_ref: field.type_ref,
            })
            .collect::<Vec<_>>();
        self.push_structure(structure.name, fields)
    }

    fn install_builtin_symbols(&mut self, root_scope: ScopeId) {
        let mut structure_ids = HashMap::new();
        for structure in BUILTIN_STRUCTURES {
            let id = self.push_structure(
                Arc::<str>::from(structure.name),
                structure
                    .fields
                    .iter()
                    .map(|field| StructureFieldData {
                        name: Arc::<str>::from(field.name),
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
            );
        }
    }

    fn find_ancestor_symbol(
        &self,
        scope: ScopeId,
        namespace: Namespace,
        name: &str,
    ) -> Option<SymbolId> {
        let mut current = self.scopes[scope.as_usize()].parent;
        while let Some(scope_id) = current {
            let key = ScopeLookupKey {
                namespace,
                name: Arc::<str>::from(name),
            };
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
        let mut current = Some(scope);
        while let Some(scope_id) = current {
            let key = ScopeLookupKey {
                namespace,
                name: Arc::<str>::from(name),
            };
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

    fn walk_node(&mut self, node: NodeId, scope: ScopeId) {
        match self.file.kind(node) {
            SyntaxKind::Token | SyntaxKind::Error => {}
            SyntaxKind::DataDecl | SyntaxKind::StaticsDecl => self.walk_data_like_decl(node, scope, SymbolKind::Variable),
            SyntaxKind::TypesDecl => self.walk_data_like_decl(node, scope, SymbolKind::TypeDef),
            SyntaxKind::ConstantsDecl => self.walk_data_like_decl(node, scope, SymbolKind::Constant),
            SyntaxKind::FieldSymbolsDecl => self.walk_data_like_decl(node, scope, SymbolKind::FieldSymbol),
            SyntaxKind::DataInlineDecl => self.walk_inline_decl(node, scope),
            SyntaxKind::IncludeStmt => self.walk_include_stmt(node, scope),
            SyntaxKind::ReportStmt => self.walk_named_header_decl(node, scope, SymbolKind::Report, ScopeKind::File),
            SyntaxKind::FormDecl => self.walk_block_decl(node, scope, SymbolKind::Form, ScopeKind::Form),
            SyntaxKind::ModuleDecl => self.walk_block_decl(node, scope, SymbolKind::Module, ScopeKind::Module),
            SyntaxKind::EventBlock => self.walk_block_decl(node, scope, SymbolKind::Event, ScopeKind::EventBlock),
            SyntaxKind::ClassDecl => self.walk_block_decl(node, scope, SymbolKind::Class, ScopeKind::Class),
            SyntaxKind::InterfaceDecl => self.walk_block_decl(node, scope, SymbolKind::Interface, ScopeKind::Interface),
            SyntaxKind::MethodDecl => self.walk_block_decl(node, scope, SymbolKind::Method, ScopeKind::Method),
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
            SyntaxKind::LoopStmt => self.walk_nested_block(node, scope, ScopeKind::LoopBlock),
            SyntaxKind::TryStmt => self.walk_nested_block(node, scope, ScopeKind::TryBlock),
            SyntaxKind::CatchClause => self.walk_nested_block(node, scope, ScopeKind::CatchClause),
            SyntaxKind::CleanupClause => self.walk_nested_block(node, scope, ScopeKind::CleanupClause),
            SyntaxKind::SelectStmt => self.walk_nested_block(node, scope, ScopeKind::SelectBlock),
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
            _ => self.walk_children(node, scope),
        }
    }

    fn walk_data_like_decl(&mut self, node: NodeId, scope: ScopeId, kind: SymbolKind) {
        for child in self.file.children(node) {
            match self.file.kind(child) {
                SyntaxKind::DataTypedClause
                | SyntaxKind::TypesTypedClause
                | SyntaxKind::ConstantClause
                | SyntaxKind::FieldSymbolClause => {
                    self.declare_decl_clause_symbol(child, scope, kind);
                    self.walk_children(child, scope);
                }
                _ => self.walk_node(child, scope),
            }
        }
    }

    fn declare_decl_clause_symbol(&mut self, node: NodeId, scope: ScopeId, kind: SymbolKind) {
        if let Some((name, range, fields)) = self.begin_of_clause_parts(node) {
            let structure = self.register_structure(scope, PendingStructure {
                name: Arc::clone(&name),
                fields,
            });
            self.declare_symbol(scope, name, kind, range, Some(structure));
            return;
        }

        if let Some(name_node) = self.file.children(node).next()
            && let Some((name, range)) = self.node_name(name_node)
        {
            let structure = self.structure_from_typed_clause(node, scope);
            self.declare_symbol(scope, name, kind, range, structure);
        }
    }

    fn walk_inline_decl(&mut self, node: NodeId, scope: ScopeId) {
        for child in self.file.children(node) {
            if self.file.kind(child) == SyntaxKind::DataDeclName
                && let Some((name, range)) = self.node_name(child)
            {
                self.declare_plain_symbol(scope, name, SymbolKind::Variable, range);
            }
        }
        self.walk_children(node, scope);
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
            let block_scope = if matches!(kind, SymbolKind::Form | SymbolKind::Module | SymbolKind::Event) {
                self.push_scope(fallback_scope_kind, self.file.range(node), Some(scope), Some(owner))
            } else {
                scope
            };
            for child in self.file.children(node) {
                self.walk_node(child, block_scope);
            }
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
        let child_scope = self.push_scope(scope_kind, self.file.range(node), Some(scope), Some(owner));
        if scope_kind == ScopeKind::Form {
            self.declare_form_parameters_from_header(node, child_scope);
        }
        for child in self.file.children(node) {
            self.walk_node(child, child_scope);
        }
    }

    fn form_header_token_refs(&self, form_node: NodeId) -> Vec<&'a Token> {
        let mut out = Vec::new();
        for child in self.file.children(form_node) {
            if self.file.kind(child) != SyntaxKind::Token {
                break;
            }
            if let Some(token) = self.token_for_node(child) {
                out.push(token);
            }
        }
        out
    }

    fn declare_form_parameters_from_header(&mut self, form_node: NodeId, form_scope: ScopeId) {
        let tokens = self.form_header_token_refs(form_node);
        if tokens.len() < 2 {
            return;
        }
        if !self.token_matches_keyword(tokens[0], "form") {
            return;
        }
        let mut i = 1usize;
        while i < tokens.len() && tokens[i].kind == TokenKind::Comment {
            i += 1;
        }
        if tokens.get(i).map(|t| t.kind) != Some(TokenKind::Ident) {
            return;
        }
        i += 1;

        let mut section: Option<FormHeaderParamSection> = None;
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
                TokenKind::Period if depth == 0 => break,
                _ if depth == 0 && t.kind == TokenKind::Ident => {
                    let lit = t.lexeme(self.source);
                    if lit.eq_ignore_ascii_case("tables") {
                        section = Some(FormHeaderParamSection::Tables);
                        i += 1;
                        continue;
                    }
                    if lit.eq_ignore_ascii_case("using") || lit.eq_ignore_ascii_case("changing") {
                        section = Some(FormHeaderParamSection::UsingOrChanging);
                        i += 1;
                        continue;
                    }
                    if lit.eq_ignore_ascii_case("raises") {
                        section = None;
                        i += 1;
                        continue;
                    }

                    match section {
                        Some(FormHeaderParamSection::UsingOrChanging) => {
                            if let Some(next_i) =
                                self.try_consume_form_value_or_reference_param(&tokens, i, form_scope)
                            {
                                i = next_i;
                                continue;
                            }
                            if self.form_header_starts_typed_param(&tokens, i) {
                                let range = t.range.clone();
                                let name = Arc::<str>::from(lit.to_ascii_lowercase());
                                self.declare_plain_symbol(form_scope, name, SymbolKind::Parameter, range);
                                i += 1;
                                while i < tokens.len() && tokens[i].kind == TokenKind::Comment {
                                    i += 1;
                                }
                                if i < tokens.len()
                                    && (self.token_matches_keyword(tokens[i], "type")
                                        || self.token_matches_keyword(tokens[i], "like"))
                                {
                                    i += 1;
                                    i = self.skip_form_header_type_expression(&tokens, i);
                                }
                                continue;
                            }
                            i += 1;
                        }
                        Some(FormHeaderParamSection::Tables) => {
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
        if self.token_matches_keyword(name, "value") || self.token_matches_keyword(name, "reference") {
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
    ) -> Option<usize> {
        let kw = tokens.get(i)?;
        if !self.token_matches_keyword(kw, "value") && !self.token_matches_keyword(kw, "reference") {
            return None;
        }
        let mut j = i + 1;
        while j < tokens.len() && tokens[j].kind == TokenKind::Comment {
            j += 1;
        }
        if tokens.get(j).map(|t| t.kind) == Some(TokenKind::LParen) {
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
            self.declare_plain_symbol(scope, name, SymbolKind::Parameter, range);
            j += 1;
            while j < tokens.len() && tokens[j].kind == TokenKind::Comment {
                j += 1;
            }
            if tokens.get(j).map(|t| t.kind) != Some(TokenKind::RParen) {
                return None;
            }
            j += 1;
        } else {
            let inner = tokens.get(j)?;
            if inner.kind != TokenKind::Ident {
                return None;
            }
            let name = Arc::<str>::from(inner.lexeme(self.source).to_ascii_lowercase());
            let range = inner.range.clone();
            self.declare_plain_symbol(scope, name, SymbolKind::Parameter, range);
            j += 1;
        }
        while j < tokens.len() && tokens[j].kind == TokenKind::Comment {
            j += 1;
        }
        if !self.token_matches_keyword(tokens.get(j)?, "type") && !self.token_matches_keyword(tokens.get(j)?, "like") {
            return None;
        }
        j += 1;
        Some(self.skip_form_header_type_expression(tokens, j))
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
        let branch_scope = self.push_scope(ScopeKind::IfBranch, self.file.range(node), Some(scope), None);
        for child in self.file.children(node) {
            match self.file.kind(child) {
                SyntaxKind::ElseifClause | SyntaxKind::ElseClause => self.walk_node(child, scope),
                _ => self.walk_node(child, branch_scope),
            }
        }
    }

    fn walk_nested_block(&mut self, node: NodeId, scope: ScopeId, kind: ScopeKind) {
        let child_scope = self.push_scope(kind, self.file.range(node), Some(scope), None);
        for child in self.file.children(node) {
            self.walk_node(child, child_scope);
        }
    }

    fn collect_type_ref(&mut self, node: NodeId, scope: ScopeId) {
        if let Some((namespace, base_name, range, field_path)) = self.type_ref_access_chain(node) {
            self.add_reference(scope, Arc::clone(&base_name), namespace, ReferenceKind::TypeRef, range);
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
                    self.add_reference(scope, name, Namespace::Value, ReferenceKind::Identifier, range);
                }
            }
            SyntaxKind::SelectorExpr => self.collect_selector_expr(node, scope),
            SyntaxKind::CallExpr => self.collect_call_expr(node, scope),
            SyntaxKind::ConstructorExpr => {
                if let Some((name, range)) = self.first_ident_in(node) {
                    self.add_reference(scope, name, Namespace::Type, ReferenceKind::TypeRef, range);
                }
                for child in self.file.children(node) {
                    if self.file.kind(child) != SyntaxKind::Token {
                        self.collect_expr(child, scope);
                    }
                }
            }
            SyntaxKind::TypeRefSimple => self.collect_type_ref(node, scope),
            _ => {
                for child in self.file.children(node) {
                    match self.file.kind(child) {
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

    fn collect_selector_expr(&mut self, node: NodeId, scope: ScopeId) {
        if let Some((namespace, base_name, base_range, field_path)) = self.selector_access_chain(node) {
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

    fn collect_call_expr(&mut self, node: NodeId, scope: ScopeId) {
        let mut children = self.file.children(node);
        if let Some(callee) = children.next() {
            match self.file.kind(callee) {
                SyntaxKind::ExprIdent => {
                    if let Some((name, range)) = self.node_name(callee) {
                        self.add_reference(scope, name, Namespace::Routine, ReferenceKind::RoutineCall, range);
                    }
                }
                _ => self.collect_expr(callee, scope),
            }
        }
        for child in children {
            if self.file.kind(child) != SyntaxKind::Token {
                self.collect_expr(child, scope);
            }
        }
    }

    fn begin_of_clause_parts(
        &self,
        node: NodeId,
    ) -> Option<(Arc<str>, TextRange, Vec<PendingStructureField>)> {
        let tokens: Vec<_> = self
            .file
            .children(node)
            .filter_map(|child| self.token_for_node(child))
            .collect();
        let (structure, consumed) = self.parse_begin_of_structure_tokens(&tokens, 0)?;
        if consumed != tokens.len() {
            return None;
        }
        Some((structure.name, self.file.range(node), structure.fields))
    }

    fn structure_from_typed_clause(&self, node: NodeId, scope: ScopeId) -> Option<StructureId> {
        let mut type_namespace = None;
        for child in self.file.children(node) {
            if let Some(token) = self.token_for_node(child)
                && token.kind == TokenKind::Ident
            {
                if token.lexeme(self.source).eq_ignore_ascii_case("type") {
                    type_namespace = Some(Namespace::Type);
                } else if token.lexeme(self.source).eq_ignore_ascii_case("like") {
                    type_namespace = Some(Namespace::Value);
                }
                continue;
            }

            if self.file.kind(child) != SyntaxKind::TypeRefSimple {
                continue;
            }

            let namespace = type_namespace.unwrap_or(Namespace::Type);
            let (base_name, field_path) = self.type_ref_lookup_parts(child)?;
            let symbol_id = self.lookup_structure_symbol(scope, namespace, base_name.as_ref(), !field_path.is_empty())?;
            let structure_id = self.symbol(symbol_id).structure?;
            return self.resolve_structure_path(structure_id, &field_path);
        }
        None
    }

    fn lookup_structure_symbol(
        &self,
        scope: ScopeId,
        namespace: Namespace,
        name: &str,
        in_type_position: bool,
    ) -> Option<SymbolId> {
        self.lookup_symbol_in_scope_chain(scope, namespace, name).or_else(|| {
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

    fn resolve_field_type_ref(&self, scope: ScopeId, type_ref: &FieldTypeRefData) -> Option<StructureId> {
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

    fn type_ref_lookup_parts(&self, node: NodeId) -> Option<(Arc<str>, Vec<Arc<str>>)> {
        let (_, base_name, _, field_path) = self.type_ref_access_chain(node)?;
        Some((
            base_name,
            field_path.into_iter().map(|segment| segment.name).collect(),
        ))
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
        if let Some(stem) = Path::new(self.uri.as_ref()).file_stem().and_then(|s| s.to_str()) {
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
                    return Some((Arc::<str>::from(text.to_ascii_lowercase()), token.range.clone()));
                }
            }
        }
        None
    }

    fn first_ident_in(&self, node: NodeId) -> Option<(Arc<str>, TextRange)> {
        let mut stack = vec![node];
        while let Some(current) = stack.pop() {
            if let Some(token) = self.token_for_node(current)
                && token.kind == TokenKind::Ident
            {
                return Some((Arc::<str>::from(token.lexeme(self.source).to_ascii_lowercase()), token.range.clone()));
            }
            for child in self.file.children(current).rev() {
                stack.push(child);
            }
        }
        None
    }

    fn node_name(&self, node: NodeId) -> Option<(Arc<str>, TextRange)> {
        let range = self.file.range(node);
        let text = self.source.get(range.clone())?;
        let lowered = text.trim().to_ascii_lowercase();
        if lowered.is_empty() {
            return None;
        }
        Some((Arc::<str>::from(lowered), range))
    }

    fn token_for_node(&self, node: NodeId) -> Option<&'a Token> {
        if self.file.kind(node) != SyntaxKind::Token {
            return None;
        }
        let range = self.file.range(node);
        let idx = self.token_index_by_range.get(&(range.start, range.end))?;
        self.tokens.get(*idx)
    }

    fn token_matches_keyword(&self, token: &Token, keyword: &str) -> bool {
        token.kind == TokenKind::Ident && token.lexeme(self.source).eq_ignore_ascii_case(keyword)
    }

    fn parse_begin_of_structure_tokens(
        &self,
        tokens: &[&Token],
        idx: usize,
    ) -> Option<(PendingStructure, usize)> {
        let begin_tok = tokens.get(idx)?;
        let of_tok = tokens.get(idx + 1)?;
        let name_tok = tokens.get(idx + 2)?;
        if !self.token_matches_keyword(begin_tok, "begin")
            || !self.token_matches_keyword(of_tok, "of")
            || name_tok.kind != TokenKind::Ident
        {
            return None;
        }

        let mut fields = Vec::new();
        let mut i = idx + 3;
        while i < tokens.len() {
            let token = tokens[i];
            if token.kind == TokenKind::Comment || token.kind == TokenKind::Comma {
                i += 1;
                continue;
            }
            if self.token_matches_keyword(token, "end")
                && tokens
                    .get(i + 1)
                    .is_some_and(|next| self.token_matches_keyword(next, "of"))
            {
                let end_name = tokens.get(i + 2)?;
                if end_name.kind != TokenKind::Ident {
                    return None;
                }
                return Some((
                    PendingStructure {
                        name: Arc::<str>::from(name_tok.lexeme(self.source).to_ascii_lowercase()),
                        fields,
                    },
                    i + 3,
                ));
            }

            if self.token_matches_keyword(token, "begin")
                && tokens
                    .get(i + 1)
                    .is_some_and(|next| self.token_matches_keyword(next, "of"))
            {
                let (nested, next_i) = self.parse_begin_of_structure_tokens(tokens, i)?;
                fields.push(PendingStructureField {
                    name: Arc::clone(&nested.name),
                    structure: Some(nested),
                    type_ref: None,
                });
                i = next_i;
                continue;
            }

            if token.kind != TokenKind::Ident {
                i += 1;
                continue;
            }

            let field_name = Arc::<str>::from(token.lexeme(self.source).to_ascii_lowercase());
            let next_i = self.skip_begin_of_field_clause(tokens, i + 1);
            let type_ref = self.parse_begin_of_field_type_ref(&tokens[i + 1..next_i]);
            i = next_i;
            fields.push(PendingStructureField {
                name: field_name,
                structure: None,
                type_ref,
            });
        }
        None
    }

    fn skip_begin_of_field_clause(&self, tokens: &[&Token], mut idx: usize) -> usize {
        let mut paren_depth = 0i32;
        let mut bracket_depth = 0i32;
        let mut brace_depth = 0i32;
        while idx < tokens.len() {
            let token = tokens[idx];
            if paren_depth == 0
                && bracket_depth == 0
                && brace_depth == 0
                && (token.kind == TokenKind::Comma
                    || (self.token_matches_keyword(token, "begin")
                        && tokens
                            .get(idx + 1)
                            .is_some_and(|next| self.token_matches_keyword(next, "of")))
                    || (self.token_matches_keyword(token, "end")
                        && tokens
                            .get(idx + 1)
                            .is_some_and(|next| self.token_matches_keyword(next, "of"))))
            {
                break;
            }
            match token.kind {
                TokenKind::LParen => paren_depth += 1,
                TokenKind::RParen => paren_depth -= 1,
                TokenKind::LBracket => bracket_depth += 1,
                TokenKind::RBracket => bracket_depth -= 1,
                TokenKind::LBrace => brace_depth += 1,
                TokenKind::RBrace => brace_depth -= 1,
                _ => {}
            }
            idx += 1;
        }
        idx
    }

    fn parse_begin_of_field_type_ref(&self, tokens: &[&Token]) -> Option<FieldTypeRefData> {
        let mut namespace = None;
        let mut idx = 0usize;
        while idx < tokens.len() {
            let token = tokens[idx];
            if self.token_matches_keyword(token, "type") {
                namespace = Some(Namespace::Type);
                idx += 1;
                break;
            }
            if self.token_matches_keyword(token, "like") {
                namespace = Some(Namespace::Value);
                idx += 1;
                break;
            }
            idx += 1;
        }
        let namespace = namespace?;
        let mut base_name = None;
        let mut field_path = Vec::new();
        let mut saw_selector = false;
        while idx < tokens.len() {
            let token = tokens[idx];
            if token.kind == TokenKind::Comment {
                idx += 1;
                continue;
            }
            if token.kind == TokenKind::Ident {
                let name = Arc::<str>::from(token.lexeme(self.source).to_ascii_lowercase());
                if base_name.is_none() {
                    base_name = Some(name);
                } else if saw_selector {
                    field_path.push(name);
                    saw_selector = false;
                } else {
                    break;
                }
                idx += 1;
                continue;
            }
            if matches!(
                token.kind,
                TokenKind::Minus | TokenKind::Arrow | TokenKind::Tilde | TokenKind::FatArrow
            ) {
                if base_name.is_some() {
                    saw_selector = true;
                    idx += 1;
                    continue;
                }
                break;
            }
            break;
        }
        Some(FieldTypeRefData {
            namespace,
            base_name: base_name?,
            field_path,
        })
    }

    fn selector_access_chain(
        &self,
        node: NodeId,
    ) -> Option<(Namespace, Arc<str>, TextRange, Vec<FieldAccessSegment>)> {
        let mut children = self.file.children(node);
        let base = children.next()?;
        let op = children.next()?;
        let field = children.next()?;
        let field_kind = self.file.kind(field);
        if field_kind != SyntaxKind::ExprIdent {
            return None;
        }
        let (field_name, field_range) = self.node_name(field)?;
        let namespace = match self.token_for_node(op) {
            Some(token) if token.kind == TokenKind::FatArrow => Namespace::Type,
            _ => Namespace::Value,
        };
        match self.file.kind(base) {
            SyntaxKind::ExprIdent => {
                let (base_name, base_range) = self.node_name(base)?;
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
                let (base_namespace, base_name, base_range, mut field_path) = self.selector_access_chain(base)?;
                field_path.push(FieldAccessSegment {
                    name: field_name,
                    range: field_range,
                });
                Some((base_namespace, base_name, base_range, field_path))
            }
            _ => None,
        }
    }

    fn type_ref_access_chain(
        &self,
        node: NodeId,
    ) -> Option<(Namespace, Arc<str>, TextRange, Vec<FieldAccessSegment>)> {
        let mut base_name = None;
        let mut base_range = None;
        let mut field_path = Vec::new();
        let mut namespace = Namespace::Type;
        let mut saw_selector = false;
        for child in self.file.children(node) {
            let token = self.token_for_node(child)?;
            if token.kind == TokenKind::Ident {
                let name = Arc::<str>::from(token.lexeme(self.source).to_ascii_lowercase());
                if base_name.is_none() {
                    base_range = Some(token.range.clone());
                    base_name = Some(name);
                } else if saw_selector {
                    field_path.push(FieldAccessSegment {
                        name,
                        range: token.range.clone(),
                    });
                    saw_selector = false;
                } else {
                    return None;
                }
                continue;
            }
            if matches!(
                token.kind,
                TokenKind::Minus | TokenKind::Arrow | TokenKind::Tilde | TokenKind::FatArrow
            ) {
                if field_path.is_empty() && token.kind != TokenKind::FatArrow {
                    namespace = Namespace::Value;
                }
                saw_selector = true;
                continue;
            }
            return None;
        }
        Some((namespace, base_name?, base_range?, field_path))
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
