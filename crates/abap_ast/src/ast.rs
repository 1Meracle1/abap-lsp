use std::sync::Arc;

use crate::SyntaxKind;
use crate::arena::{NodeId, SyntaxTree};
use abap_lexer::TextRange;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MethodsStmtKind {
    Instance,
    Class,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MethodsParamSectionKind {
    Importing,
    Exporting,
    Changing,
    Receiving,
    Returning,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MethodsTypeClauseKind {
    Type,
    Like,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeClauseKind {
    Type,
    Like,
}

#[derive(Clone, Copy)]
pub struct MethodsStmtParameter<'a> {
    section: MethodsParamSectionKind,
    name: SyntaxNodeRef<'a>,
    type_clause: MethodsTypeClauseKind,
    type_ref: Option<TypeRefSimple<'a>>,
}

impl<'a> MethodsStmtParameter<'a> {
    pub fn section(self) -> MethodsParamSectionKind {
        self.section
    }

    pub fn name_token(self) -> SyntaxNodeRef<'a> {
        self.name
    }

    pub fn type_clause(self) -> MethodsTypeClauseKind {
        self.type_clause
    }

    pub fn type_ref(self) -> Option<TypeRefSimple<'a>> {
        self.type_ref
    }
}

pub struct MethodsStmtSignature<'a> {
    is_redefinition: bool,
    parameters: Vec<MethodsStmtParameter<'a>>,
}

impl<'a> MethodsStmtSignature<'a> {
    pub fn is_redefinition(&self) -> bool {
        self.is_redefinition
    }

    pub fn parameters(&self) -> &[MethodsStmtParameter<'a>] {
        &self.parameters
    }
}

#[derive(Clone, Copy)]
pub struct SyntaxNodeRef<'a> {
    tree: &'a SyntaxTree,
    id: NodeId,
}

impl<'a> SyntaxNodeRef<'a> {
    pub fn new(tree: &'a SyntaxTree, id: NodeId) -> Self {
        Self { tree, id }
    }

    pub fn id(self) -> NodeId {
        self.id
    }

    pub fn tree(self) -> &'a SyntaxTree {
        self.tree
    }

    pub fn kind(self) -> SyntaxKind {
        self.tree.kind(self.id)
    }

    pub fn range(self) -> TextRange {
        self.tree.range(self.id)
    }

    pub fn text(self, source: &'a str) -> Option<&'a str> {
        source.get(self.range())
    }

    pub fn lower_trimmed_text(self, source: &str) -> Option<Arc<str>> {
        let lowered = self.text(source)?.trim().to_ascii_lowercase();
        if lowered.is_empty() {
            return None;
        }
        Some(Arc::from(lowered))
    }

    pub fn children(self) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.tree
            .children(self.id)
            .map(|id| SyntaxNodeRef::new(self.tree, id))
    }

    pub fn non_token_children(
        self,
    ) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.children()
            .filter(|child| child.kind() != SyntaxKind::Token)
    }

    pub fn child_by_kind(self, kind: SyntaxKind) -> Option<SyntaxNodeRef<'a>> {
        self.tree
            .child_by_kind(self.id, kind)
            .map(|id| SyntaxNodeRef::new(self.tree, id))
    }

    pub fn children_by_kind(
        self,
        kind: SyntaxKind,
    ) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.children().filter(move |child| child.kind() == kind)
    }

    pub fn first_non_token_child(self) -> Option<SyntaxNodeRef<'a>> {
        self.non_token_children().next()
    }

    pub fn last_non_token_child(self) -> Option<SyntaxNodeRef<'a>> {
        self.non_token_children().last()
    }

    pub fn token_descendants(self) -> Vec<SyntaxNodeRef<'a>> {
        let mut out = Vec::new();
        self.push_token_descendants(&mut out);
        out
    }

    fn push_token_descendants(self, out: &mut Vec<SyntaxNodeRef<'a>>) {
        if self.kind() == SyntaxKind::Token {
            out.push(self);
            return;
        }
        for child in self.children() {
            child.push_token_descendants(out);
        }
    }
}

pub trait AstNode<'a>: Sized {
    fn can_cast(kind: SyntaxKind) -> bool;

    fn cast(syntax: SyntaxNodeRef<'a>) -> Option<Self>;

    fn syntax(&self) -> SyntaxNodeRef<'a>;
}

macro_rules! ast_node {
    ($name:ident, $kind:expr) => {
        #[derive(Clone, Copy)]
        pub struct $name<'a> {
            syntax: SyntaxNodeRef<'a>,
        }

        impl<'a> AstNode<'a> for $name<'a> {
            fn can_cast(kind: SyntaxKind) -> bool {
                kind == $kind
            }

            fn cast(syntax: SyntaxNodeRef<'a>) -> Option<Self> {
                Self::can_cast(syntax.kind()).then_some(Self { syntax })
            }

            fn syntax(&self) -> SyntaxNodeRef<'a> {
                self.syntax
            }
        }
    };
}

ast_node!(ExprIdent, SyntaxKind::ExprIdent);
ast_node!(DataDecl, SyntaxKind::DataDecl);
ast_node!(DataDeclName, SyntaxKind::DataDeclName);
ast_node!(TypeRefSimple, SyntaxKind::TypeRefSimple);
ast_node!(SelectorExpr, SyntaxKind::SelectorExpr);
ast_node!(CallExpr, SyntaxKind::CallExpr);
ast_node!(CallArgList, SyntaxKind::CallArgList);
ast_node!(CallArgSection, SyntaxKind::CallArgSection);
ast_node!(CallNamedArg, SyntaxKind::CallNamedArg);
ast_node!(CallPositionalArg, SyntaxKind::CallPositionalArg);
ast_node!(MethodsStmt, SyntaxKind::MethodsStmt);
ast_node!(SelectStmt, SyntaxKind::SelectStmt);
ast_node!(SelectQuery, SyntaxKind::SelectQuery);
ast_node!(SqlProjectionItem, SyntaxKind::SqlProjectionItem);
ast_node!(SqlDataSource, SyntaxKind::SqlDataSource);
ast_node!(SqlAlias, SyntaxKind::SqlAlias);

#[derive(Clone, Copy)]
pub struct DeclClause<'a> {
    syntax: SyntaxNodeRef<'a>,
}

impl<'a> AstNode<'a> for DeclClause<'a> {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::DataTypedClause
                | SyntaxKind::TypesTypedClause
                | SyntaxKind::ConstantClause
                | SyntaxKind::FieldSymbolClause
                | SyntaxKind::StructuredFieldClause
                | SyntaxKind::StructuredDecl
        )
    }

    fn cast(syntax: SyntaxNodeRef<'a>) -> Option<Self> {
        Self::can_cast(syntax.kind()).then_some(Self { syntax })
    }

    fn syntax(&self) -> SyntaxNodeRef<'a> {
        self.syntax
    }
}

#[derive(Clone, Copy)]
pub struct DataLikeDecl<'a> {
    syntax: SyntaxNodeRef<'a>,
}

impl<'a> AstNode<'a> for DataLikeDecl<'a> {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::DataDecl
                | SyntaxKind::TypesDecl
                | SyntaxKind::ConstantsDecl
                | SyntaxKind::FieldSymbolsDecl
                | SyntaxKind::StaticsDecl
        )
    }

    fn cast(syntax: SyntaxNodeRef<'a>) -> Option<Self> {
        Self::can_cast(syntax.kind()).then_some(Self { syntax })
    }

    fn syntax(&self) -> SyntaxNodeRef<'a> {
        self.syntax
    }
}

impl<'a> ExprIdent<'a> {
    pub fn name(&self, source: &str) -> Option<Arc<str>> {
        self.syntax.lower_trimmed_text(source)
    }

    pub fn range(&self) -> TextRange {
        self.syntax.range()
    }
}

impl<'a> DataDeclName<'a> {
    pub fn name(&self, source: &str) -> Option<Arc<str>> {
        self.syntax.lower_trimmed_text(source)
    }

    pub fn range(&self) -> TextRange {
        self.syntax.range()
    }
}

impl<'a> TypeRefSimple<'a> {
    pub fn non_token_children(
        &self,
    ) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax.non_token_children()
    }

    pub fn display_text(self, source: &'a str) -> Option<&'a str> {
        let range = self.syntax.range();
        let text = source.get(range.start..range.end)?;
        let trimmed = text.trim();
        (!trimmed.is_empty()).then_some(trimmed)
    }
}

impl<'a> DataDecl<'a> {
    pub fn clauses(&self) -> impl DoubleEndedIterator<Item = DeclClause<'a>> + Clone + 'a {
        self.syntax.children().filter_map(DeclClause::cast)
    }
}

impl<'a> DataLikeDecl<'a> {
    pub fn clauses(&self) -> impl DoubleEndedIterator<Item = DeclClause<'a>> + Clone + 'a {
        self.syntax.children().filter_map(DeclClause::cast)
    }
}

impl<'a> DeclClause<'a> {
    pub fn name(&self) -> Option<DataDeclName<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::DataDeclName)
            .and_then(DataDeclName::cast)
    }

    pub fn type_refs(&self) -> impl DoubleEndedIterator<Item = TypeRefSimple<'a>> + Clone + 'a {
        self.syntax.children().filter_map(TypeRefSimple::cast)
    }

    pub fn first_type_ref(&self) -> Option<TypeRefSimple<'a>> {
        self.type_refs().next()
    }

    pub fn type_or_like_keyword(self, source: &str) -> Option<SyntaxNodeRef<'a>> {
        self.syntax
            .children_by_kind(SyntaxKind::Token)
            .find(|token| {
                token.text(source).is_some_and(|text| {
                    text.eq_ignore_ascii_case("type") || text.eq_ignore_ascii_case("like")
                })
            })
    }

    pub fn type_clause_kind(self, source: &str) -> Option<TypeClauseKind> {
        let keyword = self.type_or_like_keyword(source)?;
        let text = keyword.text(source)?;
        if text.eq_ignore_ascii_case("type") {
            Some(TypeClauseKind::Type)
        } else if text.eq_ignore_ascii_case("like") {
            Some(TypeClauseKind::Like)
        } else {
            None
        }
    }

    pub fn type_ref_with_namespace(
        self,
        source: &str,
    ) -> Option<(TypeRefSimple<'a>, TypeClauseKind)> {
        let mut namespace = None;
        for child in self.syntax.children() {
            if child.kind() == SyntaxKind::Token
                && let Some(text) = child.text(source)
            {
                if text.eq_ignore_ascii_case("type") {
                    namespace = Some(TypeClauseKind::Type);
                    continue;
                }
                if text.eq_ignore_ascii_case("like") {
                    namespace = Some(TypeClauseKind::Like);
                    continue;
                }
            }
            if let Some(namespace) = namespace
                && let Some(type_ref) = TypeRefSimple::cast(child)
            {
                return Some((type_ref, namespace));
            }
        }
        None
    }
}

impl<'a> SelectorExpr<'a> {
    pub fn base(&self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.children().next()
    }

    pub fn operator(&self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.children().nth(1)
    }

    pub fn field(&self) -> Option<ExprIdent<'a>> {
        self.syntax.children().nth(2).and_then(ExprIdent::cast)
    }
}

impl<'a> CallExpr<'a> {
    pub fn callee(&self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.children().next()
    }

    pub fn arg_list(&self) -> Option<CallArgList<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::CallArgList)
            .and_then(CallArgList::cast)
    }
}

impl<'a> CallArgList<'a> {
    pub fn items(self) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax.non_token_children()
    }

    pub fn sections(self) -> impl DoubleEndedIterator<Item = CallArgSection<'a>> + Clone + 'a {
        self.syntax.children().filter_map(CallArgSection::cast)
    }

    pub fn named_args(self) -> impl DoubleEndedIterator<Item = CallNamedArg<'a>> + Clone + 'a {
        self.syntax.children().filter_map(CallNamedArg::cast)
    }

    pub fn positional_args(
        self,
    ) -> impl DoubleEndedIterator<Item = CallPositionalArg<'a>> + Clone + 'a {
        self.syntax.children().filter_map(CallPositionalArg::cast)
    }
}

impl<'a> CallArgSection<'a> {
    pub fn first_token(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.children_by_kind(SyntaxKind::Token).next()
    }
}

impl<'a> CallNamedArg<'a> {
    pub fn name_token(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.children_by_kind(SyntaxKind::Token).next()
    }

    pub fn value_children(self) -> Vec<SyntaxNodeRef<'a>> {
        self.syntax.children().skip(2).collect()
    }
}

impl<'a> CallPositionalArg<'a> {
    pub fn value_children(self) -> Vec<SyntaxNodeRef<'a>> {
        self.syntax.children().collect()
    }
}

impl<'a> MethodsStmt<'a> {
    pub fn type_refs(&self) -> impl DoubleEndedIterator<Item = TypeRefSimple<'a>> + Clone + 'a {
        self.syntax.children().filter_map(TypeRefSimple::cast)
    }

    pub fn member_kind(self, source: &str) -> Option<MethodsStmtKind> {
        let items = self.significant_children(source);
        let first = *items.first()?;
        if Self::token_text_is(first, source, "methods") {
            return Some(MethodsStmtKind::Instance);
        }
        if items.len() >= 3
            && Self::token_text_is(items[0], source, "class")
            && Self::token_text_is(items[1], source, "-")
            && Self::token_text_is(items[2], source, "methods")
        {
            return Some(MethodsStmtKind::Class);
        }
        None
    }

    pub fn name_token(self, source: &str) -> Option<SyntaxNodeRef<'a>> {
        let items = self.significant_children(source);
        let mut idx = match self.member_kind(source)? {
            MethodsStmtKind::Instance => 1,
            MethodsStmtKind::Class => 3,
        };
        while let Some(item) = items.get(idx).copied() {
            if Self::is_punctuation(item, source) {
                idx += 1;
                continue;
            }
            if Self::is_ident_token(item, source) {
                return Some(item);
            }
            break;
        }
        None
    }

    pub fn signature_text(self, source: &str) -> String {
        let mut rendered = String::new();
        let mut prev_text: Option<&str> = None;
        for child in self.syntax.children() {
            let Some(text) = child.text(source) else {
                continue;
            };
            if Self::is_comment_text(text) {
                continue;
            }
            if text == "." {
                break;
            }
            let needs_space = !rendered.is_empty()
                && !matches!(text, "," | ":" | "-" | "(" | "[" | ")" | "]")
                && !matches!(prev_text, Some("(" | "[" | ":" | "-"));
            if needs_space {
                rendered.push(' ');
            }
            rendered.push_str(text);
            prev_text = Some(text);
        }
        rendered
    }

    pub fn signature(self, source: &str) -> MethodsStmtSignature<'a> {
        let items = self.significant_children(source);
        let mut signature = MethodsStmtSignature {
            is_redefinition: false,
            parameters: Vec::new(),
        };
        let mut idx = match self.member_kind(source) {
            Some(MethodsStmtKind::Instance) => 1,
            Some(MethodsStmtKind::Class) => 3,
            None => return signature,
        };
        while let Some(item) = items.get(idx).copied() {
            if Self::is_punctuation(item, source) {
                idx += 1;
                continue;
            }
            if Self::is_ident_token(item, source) {
                idx += 1;
            }
            break;
        }

        let mut section = None;
        let mut saw_parameter_section = false;
        while idx < items.len() {
            let item = items[idx];
            if Self::token_text_is(item, source, ".") {
                break;
            }
            if let Some(next_idx) = self.header_modifier_span(&items, idx, source) {
                if saw_parameter_section {
                    break;
                }
                if Self::token_text_is(item, source, "redefinition") {
                    signature.is_redefinition = true;
                }
                idx = next_idx;
                continue;
            }
            section = match Self::parameter_section(item, source) {
                Some(next_section) => {
                    saw_parameter_section = true;
                    idx += 1;
                    Some(next_section)
                }
                None => section,
            };
            if self.stops_parameter_scan(item, source) {
                break;
            }
            if let Some(param_section) = section
                && let Some((param, next_idx)) =
                    self.try_consume_parameter(&items, idx, param_section, source)
            {
                signature.parameters.push(param);
                idx = next_idx;
                continue;
            }
            idx += 1;
        }

        signature
    }

    fn significant_children(self, source: &str) -> Vec<SyntaxNodeRef<'a>> {
        self.syntax
            .children()
            .filter(|child| {
                child
                    .text(source)
                    .map(|text| !Self::is_comment_text(text))
                    .unwrap_or(true)
            })
            .collect()
    }

    fn is_comment_text(text: &str) -> bool {
        text.trim_start().starts_with('"')
    }

    fn token_text_is(node: SyntaxNodeRef<'a>, source: &str, expected: &str) -> bool {
        node.text(source)
            .is_some_and(|text| text.eq_ignore_ascii_case(expected))
    }

    fn is_ident_token(node: SyntaxNodeRef<'a>, source: &str) -> bool {
        node.kind() == SyntaxKind::Token
            && !matches!(
                node.text(source),
                Some("(" | ")" | "[" | "]" | ":" | "," | "." | "-")
            )
    }

    fn is_punctuation(node: SyntaxNodeRef<'a>, source: &str) -> bool {
        node.kind() == SyntaxKind::Token
            && node
                .text(source)
                .is_some_and(|text| matches!(text, ":" | "," | "."))
    }

    fn parameter_section(item: SyntaxNodeRef<'a>, source: &str) -> Option<MethodsParamSectionKind> {
        if Self::token_text_is(item, source, "importing") {
            return Some(MethodsParamSectionKind::Importing);
        }
        if Self::token_text_is(item, source, "exporting") {
            return Some(MethodsParamSectionKind::Exporting);
        }
        if Self::token_text_is(item, source, "changing") {
            return Some(MethodsParamSectionKind::Changing);
        }
        if Self::token_text_is(item, source, "receiving") {
            return Some(MethodsParamSectionKind::Receiving);
        }
        if Self::token_text_is(item, source, "returning") {
            return Some(MethodsParamSectionKind::Returning);
        }
        None
    }

    fn header_modifier_span(
        self,
        items: &[SyntaxNodeRef<'a>],
        idx: usize,
        source: &str,
    ) -> Option<usize> {
        let item = *items.get(idx)?;
        if Self::token_text_is(item, source, "abstract")
            || Self::token_text_is(item, source, "final")
            || Self::token_text_is(item, source, "redefinition")
        {
            return Some(idx + 1);
        }
        if Self::token_text_is(item, source, "for")
            && items
                .get(idx + 1)
                .is_some_and(|next| Self::token_text_is(*next, source, "testing"))
        {
            return Some(idx + 2);
        }
        None
    }

    fn stops_parameter_scan(self, item: SyntaxNodeRef<'a>, source: &str) -> bool {
        Self::token_text_is(item, source, ".")
            || Self::token_text_is(item, source, "raising")
            || Self::token_text_is(item, source, "exceptions")
    }

    fn try_consume_parameter(
        self,
        items: &[SyntaxNodeRef<'a>],
        idx: usize,
        section: MethodsParamSectionKind,
        source: &str,
    ) -> Option<(MethodsStmtParameter<'a>, usize)> {
        let mut j = idx;
        while items
            .get(j)
            .is_some_and(|item| Self::is_punctuation(*item, source))
        {
            j += 1;
        }

        let (name, mut j) = Self::parameter_name(items, j, source)?;
        while items
            .get(j)
            .is_some_and(|item| Self::is_punctuation(*item, source))
        {
            j += 1;
        }

        let type_clause = match items.get(j).copied() {
            Some(item) if Self::token_text_is(item, source, "type") => MethodsTypeClauseKind::Type,
            Some(item) if Self::token_text_is(item, source, "like") => MethodsTypeClauseKind::Like,
            _ => match section {
                MethodsParamSectionKind::Returning | MethodsParamSectionKind::Receiving => {
                    return None;
                }
                _ => return None,
            },
        };
        j += 1;

        let type_ref = items.get(j).copied().and_then(TypeRefSimple::cast);
        let next_idx = self.skip_type_expression(items, j, source);
        Some((
            MethodsStmtParameter {
                section,
                name,
                type_clause,
                type_ref,
            },
            next_idx,
        ))
    }

    fn parameter_name(
        items: &[SyntaxNodeRef<'a>],
        idx: usize,
        source: &str,
    ) -> Option<(SyntaxNodeRef<'a>, usize)> {
        let item = *items.get(idx)?;
        if Self::token_text_is(item, source, "value")
            || Self::token_text_is(item, source, "reference")
        {
            let lparen = *items.get(idx + 1)?;
            let ident = *items.get(idx + 2)?;
            let rparen = *items.get(idx + 3)?;
            if !Self::token_text_is(lparen, source, "(")
                || !Self::is_ident_token(ident, source)
                || !Self::token_text_is(rparen, source, ")")
            {
                return None;
            }
            return Some((ident, idx + 4));
        }
        if !Self::is_ident_token(item, source) {
            return None;
        }
        Some((item, idx + 1))
    }

    fn skip_type_expression(
        self,
        items: &[SyntaxNodeRef<'a>],
        mut idx: usize,
        source: &str,
    ) -> usize {
        let mut depth = 0i32;
        while idx < items.len() {
            let item = items[idx];
            if item.kind() == SyntaxKind::TypeRefSimple {
                idx += 1;
                continue;
            }
            if Self::token_text_is(item, source, "(") {
                depth += 1;
                idx += 1;
                continue;
            }
            if Self::token_text_is(item, source, ")") {
                depth -= 1;
                idx += 1;
                continue;
            }
            if depth == 0
                && (self.stops_parameter_scan(item, source)
                    || Self::parameter_section(item, source).is_some()
                    || self.header_modifier_span(items, idx, source).is_some()
                    || Self::token_text_is(item, source, "optional")
                    || Self::token_text_is(item, source, "default")
                    || Self::token_text_is(item, source, "preferred")
                    || self.starts_parameter(items, idx, source))
            {
                return idx;
            }
            idx += 1;
        }
        idx
    }

    fn starts_parameter(self, items: &[SyntaxNodeRef<'a>], idx: usize, source: &str) -> bool {
        Self::parameter_name(items, idx, source)
            .and_then(|(_, next_idx)| items.get(next_idx).copied())
            .is_some_and(|next| {
                Self::token_text_is(next, source, "type")
                    || Self::token_text_is(next, source, "like")
            })
    }
}

impl<'a> SelectStmt<'a> {
    pub fn query(&self) -> Option<SelectQuery<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SelectQuery)
            .and_then(SelectQuery::cast)
    }

    pub fn non_query_children(
        &self,
    ) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax
            .non_token_children()
            .filter(|child| child.kind() != SyntaxKind::SelectQuery)
    }
}

impl<'a> SelectQuery<'a> {
    pub fn clauses(self) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax.non_token_children()
    }

    pub fn clauses_by_kind(
        self,
        kind: SyntaxKind,
    ) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax.children_by_kind(kind)
    }
}

impl<'a> SqlProjectionItem<'a> {
    pub fn alias(self) -> Option<SqlAlias<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SqlAlias)
            .and_then(SqlAlias::cast)
    }

    pub fn non_token_children(
        self,
    ) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax.non_token_children()
    }
}

impl<'a> SqlDataSource<'a> {
    pub fn alias(self) -> Option<SqlAlias<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SqlAlias)
            .and_then(SqlAlias::cast)
    }
}

#[cfg(test)]
mod tests {
    use crate::SyntaxKind;
    use crate::arena::SyntaxTreeBuilder;

    use super::{
        AstNode, CallArgList, CallExpr, DataDecl, DataDeclName, DataLikeDecl, ExprIdent,
        MethodsParamSectionKind, MethodsStmt, MethodsStmtKind, MethodsTypeClauseKind, SelectStmt,
        SelectorExpr, SyntaxNodeRef,
    };

    #[test]
    fn token_descendants_preserve_order() {
        let mut b = SyntaxTreeBuilder::default();
        let data = b.leaf(SyntaxKind::Token, 0..4);
        let name = b.leaf(SyntaxKind::Token, 5..12);
        let ident = b.branch(SyntaxKind::DataDeclName, 5..12, &[name]);
        let root = b.branch(SyntaxKind::DataDecl, 0..13, &[data, ident]);
        let tree = b.finish(root);

        let tokens = SyntaxNodeRef::new(&tree, root).token_descendants();
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].range(), 0..4);
        assert_eq!(tokens[1].range(), 5..12);
    }

    #[test]
    fn typed_nodes_expose_names_and_structure() {
        let mut b = SyntaxTreeBuilder::default();
        let base_tok = b.leaf(SyntaxKind::Token, 0..2);
        let base = b.branch(SyntaxKind::ExprIdent, 0..2, &[base_tok]);
        let op = b.leaf(SyntaxKind::Token, 2..4);
        let field_tok = b.leaf(SyntaxKind::Token, 4..9);
        let field = b.branch(SyntaxKind::ExprIdent, 4..9, &[field_tok]);
        let selector = b.branch(SyntaxKind::SelectorExpr, 0..9, &[base, op, field]);
        let tree = b.finish(selector);
        let node = SyntaxNodeRef::new(&tree, selector);
        let selector = SelectorExpr::cast(node).expect("selector");

        assert_eq!(
            ExprIdent::cast(selector.base().expect("base"))
                .and_then(|ident| ident.name("lo->field"))
                .as_deref(),
            Some("lo")
        );
        assert_eq!(
            selector
                .field()
                .and_then(|ident| ident.name("lo->field"))
                .as_deref(),
            Some("field")
        );
    }

    #[test]
    fn data_decl_name_casts() {
        let mut b = SyntaxTreeBuilder::default();
        let tok = b.leaf(SyntaxKind::Token, 0..8);
        let name = b.branch(SyntaxKind::DataDeclName, 0..8, &[tok]);
        let tree = b.finish(name);
        let name = DataDeclName::cast(SyntaxNodeRef::new(&tree, name)).expect("data name");

        assert_eq!(name.name("lv_value").as_deref(), Some("lv_value"));
    }

    #[test]
    fn data_decl_exposes_typed_clauses() {
        let mut b = SyntaxTreeBuilder::default();
        let data_tok = b.leaf(SyntaxKind::Token, 0..4);
        let name_tok = b.leaf(SyntaxKind::Token, 5..13);
        let name = b.branch(SyntaxKind::DataDeclName, 5..13, &[name_tok]);
        let type_tok = b.leaf(SyntaxKind::Token, 14..18);
        let ty_tok = b.leaf(SyntaxKind::Token, 19..20);
        let ty = b.branch(SyntaxKind::TypeRefSimple, 19..20, &[ty_tok]);
        let clause = b.branch(SyntaxKind::DataTypedClause, 5..20, &[name, type_tok, ty]);
        let decl = b.branch(SyntaxKind::DataDecl, 0..21, &[data_tok, clause]);
        let tree = b.finish(decl);

        let decl = DataDecl::cast(SyntaxNodeRef::new(&tree, decl)).expect("data decl");
        let clauses = decl.clauses().collect::<Vec<_>>();
        assert_eq!(clauses.len(), 1);
        assert_eq!(
            clauses[0]
                .name()
                .and_then(|name| name.name("DATA lv_value TYPE i."))
                .as_deref(),
            Some("lv_value")
        );
        assert_eq!(
            clauses[0]
                .type_ref_with_namespace("DATA lv_value TYPE i.")
                .and_then(|(type_ref, _)| type_ref.display_text("DATA lv_value TYPE i."))
                .map(str::to_ascii_lowercase)
                .as_deref(),
            Some("i")
        );
    }

    #[test]
    fn declaration_and_select_wrappers_find_key_children() {
        let mut b = SyntaxTreeBuilder::default();
        let data_tok = b.leaf(SyntaxKind::Token, 0..4);
        let name_tok = b.leaf(SyntaxKind::Token, 5..13);
        let name = b.branch(SyntaxKind::DataDeclName, 5..13, &[name_tok]);
        let clause = b.branch(SyntaxKind::DataTypedClause, 5..13, &[name]);
        let data_decl = b.branch(SyntaxKind::DataDecl, 0..14, &[data_tok, clause]);

        let select_tok = b.leaf(SyntaxKind::Token, 15..21);
        let proj = b.branch(SyntaxKind::SelectProjectionList, 22..28, &[]);
        let from = b.branch(SyntaxKind::SelectFromClause, 29..36, &[]);
        let query = b.branch(SyntaxKind::SelectQuery, 15..36, &[select_tok, proj, from]);
        let stmt = b.branch(SyntaxKind::SelectStmt, 15..37, &[query]);

        let root = b.branch(SyntaxKind::File, 0..37, &[data_decl, stmt]);
        let tree = b.finish(root);

        let decl = DataLikeDecl::cast(SyntaxNodeRef::new(&tree, data_decl)).expect("decl");
        assert_eq!(decl.clauses().count(), 1);

        let stmt = SelectStmt::cast(SyntaxNodeRef::new(&tree, stmt)).expect("select stmt");
        let query = stmt.query().expect("query");
        assert_eq!(query.clauses().count(), 2);
        assert_eq!(
            query.clauses_by_kind(SyntaxKind::SelectFromClause).count(),
            1
        );
    }

    #[test]
    fn call_wrappers_expose_callee_and_args() {
        let mut b = SyntaxTreeBuilder::default();
        let callee_tok = b.leaf(SyntaxKind::Token, 0..4);
        let callee = b.branch(SyntaxKind::ExprIdent, 0..4, &[callee_tok]);
        let arg = b.branch(SyntaxKind::CallNamedArg, 5..10, &[]);
        let args = b.branch(SyntaxKind::CallArgList, 4..11, &[arg]);
        let call = b.branch(SyntaxKind::CallExpr, 0..11, &[callee, args]);
        let tree = b.finish(call);

        let call = CallExpr::cast(SyntaxNodeRef::new(&tree, call)).expect("call");
        assert!(ExprIdent::cast(call.callee().expect("callee")).is_some());
        assert_eq!(
            call.arg_list()
                .and_then(|arg_list| Some(CallArgList::items(arg_list).count())),
            Some(1)
        );
    }

    #[test]
    fn methods_stmt_wrappers_parse_signature_sections_and_types() {
        let source =
            "CLASS-METHODS run IMPORTING VALUE(iv_x) TYPE i RETURNING VALUE(rv_y) LIKE foo.";
        let mut b = SyntaxTreeBuilder::default();
        let mut cursor = 0usize;
        let mut last_range = 0..0;
        let mut take = |needle: &str, builder: &mut SyntaxTreeBuilder| {
            let rel = source[cursor..].find(needle).expect("token text");
            let start = cursor + rel;
            let end = start + needle.len();
            cursor = end;
            last_range = start..end;
            (
                builder.leaf(SyntaxKind::Token, start..end),
                last_range.clone(),
            )
        };

        let (class_tok, class_range) = take("CLASS", &mut b);
        let (minus_tok, _) = take("-", &mut b);
        let (methods_tok, _) = take("METHODS", &mut b);
        let (name_tok, _) = take("run", &mut b);
        let (importing_tok, _) = take("IMPORTING", &mut b);
        let (value_tok, _) = take("VALUE", &mut b);
        let (lparen_1, _) = take("(", &mut b);
        let (iv_x_tok, iv_x_range) = take("iv_x", &mut b);
        let (rparen_1, _) = take(")", &mut b);
        let (type_tok, _) = take("TYPE", &mut b);
        let (i_tok, i_range) = take("i", &mut b);
        let i_type = b.branch(SyntaxKind::TypeRefSimple, i_range.clone(), &[i_tok]);
        let (returning_tok, _) = take("RETURNING", &mut b);
        let (value_tok_2, _) = take("VALUE", &mut b);
        let (lparen_2, _) = take("(", &mut b);
        let (rv_y_tok, rv_y_range) = take("rv_y", &mut b);
        let (rparen_2, _) = take(")", &mut b);
        let (like_tok, _) = take("LIKE", &mut b);
        let (foo_tok, foo_range) = take("foo", &mut b);
        let foo_type = b.branch(SyntaxKind::TypeRefSimple, foo_range.clone(), &[foo_tok]);
        let (period_tok, period_range) = take(".", &mut b);

        let methods_stmt = b.branch(
            SyntaxKind::MethodsStmt,
            class_range.start..period_range.end,
            &[
                class_tok,
                minus_tok,
                methods_tok,
                name_tok,
                importing_tok,
                value_tok,
                lparen_1,
                iv_x_tok,
                rparen_1,
                type_tok,
                i_type,
                returning_tok,
                value_tok_2,
                lparen_2,
                rv_y_tok,
                rparen_2,
                like_tok,
                foo_type,
                period_tok,
            ],
        );
        let tree = b.finish(methods_stmt);
        let methods_stmt =
            MethodsStmt::cast(SyntaxNodeRef::new(&tree, methods_stmt)).expect("methods stmt");

        assert_eq!(
            methods_stmt.member_kind(source),
            Some(MethodsStmtKind::Class)
        );
        assert_eq!(
            methods_stmt
                .name_token(source)
                .and_then(|token| token.text(source))
                .as_deref(),
            Some("run")
        );
        assert_eq!(
            methods_stmt.signature_text(source),
            "CLASS-METHODS run IMPORTING VALUE(iv_x) TYPE i RETURNING VALUE(rv_y) LIKE foo"
        );

        let signature = methods_stmt.signature(source);
        assert!(!signature.is_redefinition());
        assert_eq!(signature.parameters().len(), 2);
        assert_eq!(
            signature.parameters()[0].section(),
            MethodsParamSectionKind::Importing
        );
        assert_eq!(
            signature.parameters()[0].type_clause(),
            MethodsTypeClauseKind::Type
        );
        assert_eq!(
            signature.parameters()[0]
                .name_token()
                .text(source)
                .as_deref(),
            Some("iv_x")
        );
        assert_eq!(signature.parameters()[0].name_token().range(), iv_x_range);
        assert!(signature.parameters()[0].type_ref().is_some());
        assert_eq!(
            signature.parameters()[1].section(),
            MethodsParamSectionKind::Returning
        );
        assert_eq!(
            signature.parameters()[1].type_clause(),
            MethodsTypeClauseKind::Like
        );
        assert_eq!(
            signature.parameters()[1]
                .name_token()
                .text(source)
                .as_deref(),
            Some("rv_y")
        );
        assert_eq!(signature.parameters()[1].name_token().range(), rv_y_range);
        assert!(signature.parameters()[1].type_ref().is_some());
    }
}
