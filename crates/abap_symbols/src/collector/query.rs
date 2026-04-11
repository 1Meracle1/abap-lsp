use std::path::Path;
use std::sync::Arc;

use abap_ast::SyntaxKind;
use abap_ast::arena::NodeId;
use abap_ast::ast::{
    AstNode, ConstructorExpr, ExprIdent, ParenExpr, SelectorExpr, SyntaxNodeRef, TemplateExpr,
};
use abap_lexer::{TextRange, TokenKind};

use crate::def_map::{FieldAccess, FieldAccessSegment, StructureData, SymbolData, SymbolKind};
use crate::ids::{ScopeId, StructureId, SymbolId};
use crate::scope::{Namespace, ScopeKind};

use super::{Collector, ScopeLookupKey, SyntaxTokenInfo};

impl<'a> Collector<'a> {
    fn unwrap_simple_expr_wrapper(&self, node: NodeId) -> NodeId {
        let mut current = node;
        loop {
            let next = match self.file.kind(current) {
                SyntaxKind::TemplateExpr => TemplateExpr::cast(self.syntax(current))
                    .and_then(|expr| expr.wrapped_expr())
                    .map(|child| child.id()),
                SyntaxKind::ParenExpr => ParenExpr::cast(self.syntax(current))
                    .and_then(|expr| expr.inner_expr())
                    .map(|child| child.id()),
                _ => None,
            };
            if let Some(next) = next {
                current = next;
            } else {
                return current;
            }
        }
    }

    fn legacy_table_body_value_access_from_tokens(
        &self,
        tokens: &[SyntaxTokenInfo],
        scope: ScopeId,
    ) -> Option<FieldAccess> {
        if let Some((next_idx, namespace, base_name, _, field_path, bracket_groups)) =
            self.consume_selector_access_from_infos(tokens, 0)
        {
            let covered_end = bracket_groups
                .last()
                .map(|(_, group_end, _)| group_end + 1)
                .unwrap_or(next_idx);
            if namespace == Namespace::Value
                && covered_end == tokens.len()
                && !bracket_groups.is_empty()
                && bracket_groups
                    .iter()
                    .all(|(_, _, is_legacy_table_body)| *is_legacy_table_body)
            {
                return Some(FieldAccess {
                    scope,
                    base_namespace: namespace,
                    base_name,
                    field_path,
                    in_type_position: false,
                });
            }
        }

        let first = tokens.first()?;
        if !self.syntax_token_is_ident_like(first) {
            return None;
        }
        let mut idx = 1usize;
        while idx < tokens.len() {
            if tokens.get(idx)?.text.as_ref() != "[" {
                return None;
            }
            let end_idx = self.find_matching_group_end_infos(tokens, idx, "[", "]")?;
            if end_idx != idx + 1 {
                return None;
            }
            idx = end_idx + 1;
        }
        (idx == tokens.len()).then(|| FieldAccess {
            scope,
            base_namespace: Namespace::Value,
            base_name: Arc::<str>::from(first.text.to_ascii_lowercase()),
            field_path: Vec::new(),
            in_type_position: false,
        })
    }

    pub(super) fn find_ancestor_symbol(
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

    pub(super) fn lookup_symbol_in_scope_chain(
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

    pub(super) fn declaration_scope(&self, scope: ScopeId) -> ScopeId {
        let mut current = scope;
        loop {
            match self.scopes[current.as_usize()].kind {
                ScopeKind::File
                | ScopeKind::Form
                | ScopeKind::Module
                | ScopeKind::EventBlock
                | ScopeKind::Class
                | ScopeKind::Interface
                | ScopeKind::Method => return current,
                _ => {
                    let Some(parent) = self.scopes[current.as_usize()].parent else {
                        return current;
                    };
                    current = parent;
                }
            }
        }
    }

    pub(super) fn structure(&self, id: StructureId) -> Option<&StructureData> {
        self.structures.get(id.as_usize())
    }

    pub(super) fn provided_names(&self) -> Vec<Arc<str>> {
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

    pub(super) fn symbol(&self, id: SymbolId) -> &SymbolData {
        &self.symbols[id.as_usize()]
    }

    pub(super) fn header_ident_after_keyword(&self, node: NodeId) -> Option<(Arc<str>, TextRange)> {
        if let Some(name_node) = self
            .syntax(node)
            .child_by_kind(SyntaxKind::DataDeclName)
            .and_then(abap_ast::ast::DataDeclName::cast)
        {
            return Some((name_node.name(self.source)?, name_node.range()));
        }
        let mut saw_keyword = false;
        for child in self.file.children(node) {
            if self.file.kind(child) != SyntaxKind::Token {
                continue;
            }
            let Some(text) = self.syntax(child).text(self.source) else {
                continue;
            };
            if text == "." {
                break;
            }
            if !saw_keyword {
                saw_keyword = !matches!(text, ":" | "," | "." | "-" | "(" | ")");
                continue;
            }
            if !matches!(
                text.to_ascii_uppercase().as_str(),
                "DEFINITION" | "IMPLEMENTATION" | "PUBLIC" | "PROTECTED" | "PRIVATE" | "SECTION"
            ) && !matches!(text, ":" | "," | "." | "-" | "(" | ")")
            {
                return Some((
                    Arc::<str>::from(text.to_ascii_lowercase()),
                    self.file.range(child),
                ));
            }
        }
        None
    }

    pub(super) fn event_block_header_name(&self, node: NodeId) -> Option<(Arc<str>, TextRange)> {
        let tokens: Vec<_> = self
            .file
            .children(node)
            .filter(|&child| self.file.kind(child) == SyntaxKind::Token)
            .map(|child| self.syntax(child))
            .take_while(|token| token.text(self.source) != Some("."))
            .collect();
        let (first, last) = match tokens.as_slice() {
            [token]
                if token
                    .text(self.source)
                    .is_some_and(|text| text.eq_ignore_ascii_case("initialization")) =>
            {
                (*token, *token)
            }
            [start, minus_1, of, minus_2, end]
                if start
                    .text(self.source)
                    .is_some_and(|text| text.eq_ignore_ascii_case("start"))
                    && minus_1.text(self.source) == Some("-")
                    && of
                        .text(self.source)
                        .is_some_and(|text| text.eq_ignore_ascii_case("of"))
                    && minus_2.text(self.source) == Some("-")
                    && end
                        .text(self.source)
                        .is_some_and(|text| text.eq_ignore_ascii_case("selection")) =>
            {
                (*start, *end)
            }
            [start, minus_1, of, minus_2, end]
                if start
                    .text(self.source)
                    .is_some_and(|text| text.eq_ignore_ascii_case("end"))
                    && minus_1.text(self.source) == Some("-")
                    && of
                        .text(self.source)
                        .is_some_and(|text| text.eq_ignore_ascii_case("of"))
                    && minus_2.text(self.source) == Some("-")
                    && end
                        .text(self.source)
                        .is_some_and(|text| text.eq_ignore_ascii_case("selection")) =>
            {
                (*start, *end)
            }
            [start, minus_1, of, minus_2, end]
                if start
                    .text(self.source)
                    .is_some_and(|text| text.eq_ignore_ascii_case("top"))
                    && minus_1.text(self.source) == Some("-")
                    && of
                        .text(self.source)
                        .is_some_and(|text| text.eq_ignore_ascii_case("of"))
                    && minus_2.text(self.source) == Some("-")
                    && end
                        .text(self.source)
                        .is_some_and(|text| text.eq_ignore_ascii_case("page")) =>
            {
                (*start, *end)
            }
            [start, minus_1, of, minus_2, end]
                if start
                    .text(self.source)
                    .is_some_and(|text| text.eq_ignore_ascii_case("end"))
                    && minus_1.text(self.source) == Some("-")
                    && of
                        .text(self.source)
                        .is_some_and(|text| text.eq_ignore_ascii_case("of"))
                    && minus_2.text(self.source) == Some("-")
                    && end
                        .text(self.source)
                        .is_some_and(|text| text.eq_ignore_ascii_case("page")) =>
            {
                (*start, *end)
            }
            _ => return None,
        };
        Some((
            Arc::<str>::from(
                self.source[first.range().start..last.range().end].to_ascii_lowercase(),
            ),
            first.range().start..last.range().end,
        ))
    }

    pub(super) fn constructor_type_ref(&self, node: NodeId) -> Option<(Arc<str>, TextRange)> {
        let type_ref = ConstructorExpr::cast(self.syntax(node))?
            .type_ref()?
            .syntax();
        let (_, _, base_name, range, _) =
            self.type_ref_access_chain(type_ref.id(), Namespace::Type)?;
        Some((base_name, range))
    }

    pub(super) fn node_name(&self, node: NodeId) -> Option<(Arc<str>, TextRange)> {
        let syntax = self.syntax(node);
        Some((syntax.lower_trimmed_text(self.source)?, syntax.range()))
    }

    pub(super) fn first_non_token_child(&self, node: NodeId) -> Option<NodeId> {
        self.syntax(node)
            .first_non_token_child()
            .map(|child| child.id())
    }

    pub(super) fn last_non_token_child(&self, node: NodeId) -> Option<NodeId> {
        self.syntax(node)
            .last_non_token_child()
            .map(|child| child.id())
    }

    pub(super) fn syntax_token_nodes(&self, node: NodeId) -> Vec<SyntaxTokenInfo> {
        self.syntax(node)
            .token_descendants()
            .into_iter()
            .filter_map(|token_node| {
                let text = token_node.text(self.source)?;
                Some(SyntaxTokenInfo {
                    range: token_node.range(),
                    text: Arc::<str>::from(text),
                    _index: token_node.token_index()?,
                    kind: token_node.token_kind().unwrap_or(TokenKind::Other),
                })
            })
            .collect()
    }

    pub(super) fn syntax_tokens_have_space_between(
        &self,
        left: &SyntaxTokenInfo,
        right: &SyntaxTokenInfo,
    ) -> bool {
        left.range.end < right.range.start
    }

    pub(super) fn selector_access_chain(
        &self,
        node: NodeId,
    ) -> Option<(Namespace, Arc<str>, TextRange, Vec<FieldAccessSegment>)> {
        let node = self.unwrap_simple_expr_wrapper(node);
        let selector = SelectorExpr::cast(self.syntax(node))?;
        let base = selector.base()?;
        let op = selector.operator()?;
        let field = selector.field()?;
        let field_name = field.name(self.source)?;
        let field_range = field.range();
        let namespace = match op.text(self.source) {
            Some("=>") => Namespace::Type,
            _ => Namespace::Value,
        };
        let base_id = self.unwrap_simple_expr_wrapper(base.id());
        match self.file.kind(base_id) {
            SyntaxKind::ExprIdent => {
                let ident = ExprIdent::cast(self.syntax(base_id))?;
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
                    self.selector_access_chain(base_id)?;
                field_path.push(FieldAccessSegment {
                    name: field_name,
                    range: field_range,
                });
                Some((base_namespace, base_name, base_range, field_path))
            }
            _ => None,
        }
    }

    pub(super) fn value_access_from_node(
        &self,
        node: NodeId,
        scope: ScopeId,
    ) -> Option<FieldAccess> {
        let node = self.unwrap_simple_expr_wrapper(node);
        match self.file.kind(node) {
            SyntaxKind::ExprIdent => {
                let (name, _) = self.node_name(node)?;
                Some(FieldAccess {
                    scope,
                    base_namespace: Namespace::Value,
                    base_name: name,
                    field_path: Vec::new(),
                    in_type_position: false,
                })
            }
            SyntaxKind::SelectorExpr => {
                let (namespace, base_name, _, field_path) = self.selector_access_chain(node)?;
                (namespace == Namespace::Value).then_some(FieldAccess {
                    scope,
                    base_namespace: namespace,
                    base_name,
                    field_path,
                    in_type_position: false,
                })
            }
            _ => self
                .legacy_table_body_value_access_from_tokens(&self.syntax_token_nodes(node), scope),
        }
    }

    pub(super) fn syntax(&self, node: NodeId) -> SyntaxNodeRef<'_> {
        SyntaxNodeRef::new(self.file, node)
    }

    pub(super) fn type_ref_selector_chain_access_chain(
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
            if namespace.is_none() {
                namespace = Some(match self.syntax(op_node).text(self.source) {
                    Some("=>") => Namespace::Type,
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

    pub(super) fn type_ref_access_chain(
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
                    if self.file.kind(child) == SyntaxKind::Token
                        && let Some(text) = self.syntax(child).text(self.source)
                    {
                        if text.eq_ignore_ascii_case("ref") {
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
