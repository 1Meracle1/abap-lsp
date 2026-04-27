use std::sync::Arc;

use crate::SyntaxKind;
use crate::arena::{NodeId, SyntaxTree};
use abap_lexer::{LexedSource, TextRange, Token, TokenKind, TriviaPiece};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MethodsStmtKind {
    Instance,
    Class,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EventsStmtKind {
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
pub enum MethodsRaiseKind {
    Raising,
    Resumable,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeClauseKind {
    Type,
    Like,
    For,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StructuredIncludeKind {
    Type,
    Structure,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FormParamSectionKind {
    Tables,
    Using,
    Changing,
    Raising,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FormParamPassingKind {
    Direct,
    Value,
    Reference,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FunctionParamSectionKind {
    Importing,
    Exporting,
    Changing,
    Tables,
    Raising,
    Exceptions,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CallStmtKind {
    Function,
    SystemFunction,
    Transformation,
    Badi,
    Screen,
    Transaction,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ClassSectionVisibilityKind {
    Public,
    Protected,
    Private,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DataLikeStorageKind {
    Instance,
    Static,
    Constant,
}

#[derive(Clone, Copy)]
pub struct MethodsStmtParameter<'a> {
    section: MethodsParamSectionKind,
    name: SyntaxNodeRef<'a>,
    type_clause: MethodsTypeClauseKind,
    type_ref: Option<TypeRefSimple<'a>>,
    type_display_range: Option<(usize, usize)>,
    is_optional: bool,
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

    pub fn type_display_text(self, source: &'a str) -> Option<&'a str> {
        let (start, end) = self.type_display_range?;
        let text = source.get(start..end)?;
        let mut trimmed = text.trim();
        if let Some(cutoff) = trimmed.match_indices('\n').find_map(|(idx, _)| {
            let continuation = trimmed
                .get(idx + 1..)?
                .trim_start_matches(|ch| matches!(ch, ' ' | '\t' | '\r'));
            continuation.starts_with('!').then_some(idx)
        }) {
            trimmed = trimmed.get(..cutoff)?.trim_end();
        }
        (!trimmed.is_empty()).then_some(trimmed)
    }

    pub fn is_optional(self) -> bool {
        self.is_optional
    }
}

#[derive(Clone, Copy)]
pub struct MethodsStmtRaising<'a> {
    kind: MethodsRaiseKind,
    type_ref: TypeRefSimple<'a>,
}

impl<'a> MethodsStmtRaising<'a> {
    pub fn kind(self) -> MethodsRaiseKind {
        self.kind
    }

    pub fn type_ref(self) -> TypeRefSimple<'a> {
        self.type_ref
    }
}

pub struct MethodsStmtSignature<'a> {
    is_redefinition: bool,
    parameters: Vec<MethodsStmtParameter<'a>>,
    raising: Vec<MethodsStmtRaising<'a>>,
}

impl<'a> MethodsStmtSignature<'a> {
    pub fn is_redefinition(&self) -> bool {
        self.is_redefinition
    }

    pub fn parameters(&self) -> &[MethodsStmtParameter<'a>] {
        &self.parameters
    }

    pub fn raising(&self) -> &[MethodsStmtRaising<'a>] {
        &self.raising
    }
}

#[derive(Clone)]
pub struct MethodsStmtEventHandler<'a> {
    event_qualifier: Option<SyntaxNodeRef<'a>>,
    event_name: SyntaxNodeRef<'a>,
    source_type_ref: TypeRefSimple<'a>,
    importing: Vec<SyntaxNodeRef<'a>>,
}

impl<'a> MethodsStmtEventHandler<'a> {
    pub fn event_qualifier(&self) -> Option<SyntaxNodeRef<'a>> {
        self.event_qualifier
    }

    pub fn event_name(&self) -> SyntaxNodeRef<'a> {
        self.event_name
    }

    pub fn source_type_ref(&self) -> TypeRefSimple<'a> {
        self.source_type_ref
    }

    pub fn importing_names(&self) -> &[SyntaxNodeRef<'a>] {
        &self.importing
    }
}

#[derive(Clone)]
pub struct MethodsStmtEntry<'a> {
    kind: MethodsStmtKind,
    items: Vec<SyntaxNodeRef<'a>>,
}

impl<'a> MethodsStmtEntry<'a> {
    pub fn member_kind(&self) -> MethodsStmtKind {
        self.kind
    }

    pub fn name_token(&self, source: &str) -> Option<SyntaxNodeRef<'a>> {
        self.items
            .iter()
            .copied()
            .find(|item| MethodsStmt::is_ident_token(*item, source))
    }

    pub fn signature_text(&self, source: &str) -> String {
        let keyword = match self.kind {
            MethodsStmtKind::Instance => "METHODS",
            MethodsStmtKind::Class => "CLASS-METHODS",
        };
        let body = signature_text_from_tokens(&self.items, source);
        if body.is_empty() {
            keyword.to_string()
        } else {
            format!("{keyword} {body}")
        }
    }

    pub fn signature(&self, source: &str) -> MethodsStmtSignature<'a> {
        MethodsStmt::parse_signature_items(&self.items, source)
    }

    pub fn event_handler(&self, source: &str) -> Option<MethodsStmtEventHandler<'a>> {
        MethodsStmt::parse_event_handler_items(&self.items, source)
    }
}

#[derive(Clone, Copy)]
pub struct EventsStmtParameter<'a> {
    name: SyntaxNodeRef<'a>,
    type_clause: MethodsTypeClauseKind,
    type_ref: Option<TypeRefSimple<'a>>,
    type_display_range: Option<(usize, usize)>,
    is_optional: bool,
}

impl<'a> EventsStmtParameter<'a> {
    pub fn name_token(self) -> SyntaxNodeRef<'a> {
        self.name
    }

    pub fn type_clause(self) -> MethodsTypeClauseKind {
        self.type_clause
    }

    pub fn type_ref(self) -> Option<TypeRefSimple<'a>> {
        self.type_ref
    }

    pub fn type_display_text(self, source: &'a str) -> Option<&'a str> {
        let (start, end) = self.type_display_range?;
        let text = source.get(start..end)?;
        let trimmed = text.trim();
        (!trimmed.is_empty()).then_some(trimmed)
    }

    pub fn is_optional(self) -> bool {
        self.is_optional
    }
}

pub struct EventsStmtSignature<'a> {
    parameters: Vec<EventsStmtParameter<'a>>,
}

impl<'a> EventsStmtSignature<'a> {
    pub fn parameters(&self) -> &[EventsStmtParameter<'a>] {
        &self.parameters
    }
}

#[derive(Clone)]
pub struct EventsStmtEntry<'a> {
    kind: EventsStmtKind,
    items: Vec<SyntaxNodeRef<'a>>,
}

impl<'a> EventsStmtEntry<'a> {
    pub fn member_kind(&self) -> EventsStmtKind {
        self.kind
    }

    pub fn name_token(&self, source: &str) -> Option<SyntaxNodeRef<'a>> {
        self.items
            .iter()
            .copied()
            .find(|item| EventsStmt::is_ident_token(*item, source))
    }

    pub fn signature_text(&self, source: &str) -> String {
        let keyword = match self.kind {
            EventsStmtKind::Instance => "EVENTS",
            EventsStmtKind::Class => "CLASS-EVENTS",
        };
        let body = signature_text_from_tokens(&self.items, source);
        if body.is_empty() {
            keyword.to_string()
        } else {
            format!("{keyword} {body}")
        }
    }

    pub fn signature(&self, source: &str) -> EventsStmtSignature<'a> {
        EventsStmt::parse_signature_items(&self.items, source)
    }
}

fn signature_text_from_tokens<'a>(items: &[SyntaxNodeRef<'a>], source: &str) -> String {
    let mut rendered = String::new();
    let mut prev_text: Option<&str> = None;
    for item in items {
        let Some(text) = item.text(source) else {
            continue;
        };
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

    pub fn token_index(self) -> Option<usize> {
        self.tree.token_index(self.id)
    }

    pub fn token_kind(self) -> Option<TokenKind> {
        self.tree.token_kind(self.id)
    }

    pub fn as_token(self, lexed: &'a LexedSource) -> Option<SyntaxTokenRef<'a>> {
        let index = self.token_index()?;
        let token = lexed.token(index)?;
        Some(SyntaxTokenRef {
            syntax: self,
            lexed,
            token,
        })
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

#[derive(Clone, Copy)]
pub struct SyntaxTokenRef<'a> {
    syntax: SyntaxNodeRef<'a>,
    lexed: &'a LexedSource,
    token: &'a Token,
}

impl<'a> SyntaxTokenRef<'a> {
    pub fn syntax(self) -> SyntaxNodeRef<'a> {
        self.syntax
    }

    pub fn token(self) -> &'a Token {
        self.token
    }

    pub fn kind(self) -> TokenKind {
        self.token.kind
    }

    pub fn text(self, source: &'a str) -> &'a str {
        self.token.lexeme(source)
    }

    pub fn has_newline_before(self) -> bool {
        self.lexed.has_newline_before(self.token)
    }

    pub fn has_trailing_inline_comment(self) -> bool {
        self.token.has_trailing_inline_comment()
    }

    pub fn leading_trivia(self) -> &'a [TriviaPiece] {
        self.lexed.leading_trivia(self.token)
    }

    pub fn trailing_trivia(self) -> &'a [TriviaPiece] {
        self.lexed.trailing_trivia(self.token)
    }

    pub fn leading_comments(self) -> impl Iterator<Item = &'a TriviaPiece> + 'a {
        self.lexed.leading_comments(self.token)
    }

    pub fn trailing_comments(self) -> impl Iterator<Item = &'a TriviaPiece> + 'a {
        self.lexed.trailing_comments(self.token)
    }

    pub fn trailing_inline_comment(self) -> Option<&'a TriviaPiece> {
        self.lexed.trailing_inline_comment(self.token)
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
ast_node!(StructuredIncludeClause, SyntaxKind::StructuredIncludeClause);
ast_node!(TypeRefSimple, SyntaxKind::TypeRefSimple);
ast_node!(TemplateExpr, SyntaxKind::TemplateExpr);
ast_node!(TemplateInterpolation, SyntaxKind::TemplateInterpolation);
ast_node!(TableExpr, SyntaxKind::TableExpr);
ast_node!(SelectorExpr, SyntaxKind::SelectorExpr);
ast_node!(ParenExpr, SyntaxKind::ParenExpr);
ast_node!(CallExpr, SyntaxKind::CallExpr);
ast_node!(ConstructorExpr, SyntaxKind::ConstructorExpr);
ast_node!(LetExpr, SyntaxKind::LetExpr);
ast_node!(ConstructorLetBinding, SyntaxKind::ConstructorLetBinding);
ast_node!(CallArgList, SyntaxKind::CallArgList);
ast_node!(CallArgSection, SyntaxKind::CallArgSection);
ast_node!(CallNamedArg, SyntaxKind::CallNamedArg);
ast_node!(CallPositionalArg, SyntaxKind::CallPositionalArg);
ast_node!(ConstructorWhenClause, SyntaxKind::ConstructorWhenClause);
ast_node!(ConstructorElseClause, SyntaxKind::ConstructorElseClause);
ast_node!(ConstructorForClause, SyntaxKind::ConstructorForClause);
ast_node!(ConstructorWhereClause, SyntaxKind::ConstructorWhereClause);
ast_node!(ConstructorInitClause, SyntaxKind::ConstructorInitClause);
ast_node!(ConstructorNextClause, SyntaxKind::ConstructorNextClause);
ast_node!(
    ConstructorNamedAssignment,
    SyntaxKind::ConstructorNamedAssignment
);
ast_node!(
    ConstructorCorrespondingMappingClause,
    SyntaxKind::ConstructorCorrespondingMappingClause
);
ast_node!(
    ConstructorCorrespondingMappingAssignment,
    SyntaxKind::ConstructorCorrespondingMappingAssignment
);
ast_node!(
    ConstructorCorrespondingExceptClause,
    SyntaxKind::ConstructorCorrespondingExceptClause
);
ast_node!(ConstructorBaseClause, SyntaxKind::ConstructorBaseClause);
ast_node!(
    ConstructorLinesOfClause,
    SyntaxKind::ConstructorLinesOfClause
);
ast_node!(ConstructorOptionalExpr, SyntaxKind::ConstructorOptionalExpr);
ast_node!(AliasesStmt, SyntaxKind::AliasesStmt);
ast_node!(AliasEntry, SyntaxKind::AliasEntry);
ast_node!(AliasName, SyntaxKind::AliasName);
ast_node!(AliasMember, SyntaxKind::AliasMember);
ast_node!(IncludeStmt, SyntaxKind::IncludeStmt);
ast_node!(IncludeName, SyntaxKind::IncludeName);
ast_node!(FormDecl, SyntaxKind::FormDecl);
ast_node!(FormParamSection, SyntaxKind::FormParamSection);
ast_node!(FormParam, SyntaxKind::FormParam);
ast_node!(FunctionDecl, SyntaxKind::FunctionDecl);
ast_node!(FunctionParamSection, SyntaxKind::FunctionParamSection);
ast_node!(FunctionParam, SyntaxKind::FunctionParam);
ast_node!(ClassDecl, SyntaxKind::ClassDecl);
ast_node!(ClassInheritanceClause, SyntaxKind::ClassInheritanceClause);
ast_node!(
    ClassImplementationMarker,
    SyntaxKind::ClassImplementationMarker
);
ast_node!(InterfaceDecl, SyntaxKind::InterfaceDecl);
ast_node!(MethodDecl, SyntaxKind::MethodDecl);
ast_node!(MethodDeclTarget, SyntaxKind::MethodDeclTarget);
ast_node!(SqlScriptIsland, SyntaxKind::SqlScriptIsland);
ast_node!(ClassSectionStmt, SyntaxKind::ClassSectionStmt);
ast_node!(ClassSectionVisibility, SyntaxKind::ClassSectionVisibility);
ast_node!(MacroDef, SyntaxKind::MacroDef);
ast_node!(MacroCallStmt, SyntaxKind::MacroCallStmt);
ast_node!(MethodsStmt, SyntaxKind::MethodsStmt);
ast_node!(EventsStmt, SyntaxKind::EventsStmt);
ast_node!(InterfacesStmt, SyntaxKind::InterfacesStmt);
ast_node!(ClearStmt, SyntaxKind::ClearStmt);
ast_node!(ClearOperand, SyntaxKind::ClearOperand);
ast_node!(RefreshStmt, SyntaxKind::RefreshStmt);
ast_node!(RefreshOperand, SyntaxKind::RefreshOperand);
ast_node!(CollectStmt, SyntaxKind::CollectStmt);
ast_node!(CollectSourceOperand, SyntaxKind::CollectSourceOperand);
ast_node!(CollectTargetOperand, SyntaxKind::CollectTargetOperand);
ast_node!(FreeStmt, SyntaxKind::FreeStmt);
ast_node!(FreeOperand, SyntaxKind::FreeOperand);
ast_node!(UnassignStmt, SyntaxKind::UnassignStmt);
ast_node!(UnassignOperand, SyntaxKind::UnassignOperand);
ast_node!(ImportMemoryStmt, SyntaxKind::ImportMemoryStmt);
ast_node!(
    ImportMemorySourceOperand,
    SyntaxKind::ImportMemorySourceOperand
);
ast_node!(
    ImportMemoryTargetOperand,
    SyntaxKind::ImportMemoryTargetOperand
);
ast_node!(ExportMemoryStmt, SyntaxKind::ExportMemoryStmt);
ast_node!(ExportMemoryNameOperand, SyntaxKind::ExportMemoryNameOperand);
ast_node!(
    ExportMemorySourceOperand,
    SyntaxKind::ExportMemorySourceOperand
);
ast_node!(MemoryIdOperand, SyntaxKind::MemoryIdOperand);
ast_node!(DataBufferOperand, SyntaxKind::DataBufferOperand);
ast_node!(ConvertStmt, SyntaxKind::ConvertStmt);
ast_node!(ConvertOperand, SyntaxKind::ConvertOperand);
ast_node!(ConvertTargetOperand, SyntaxKind::ConvertTargetOperand);
ast_node!(ConvertTimeZoneOperand, SyntaxKind::ConvertTimeZoneOperand);
ast_node!(ConvertDateTarget, SyntaxKind::ConvertDateTarget);
ast_node!(ConvertTimeTarget, SyntaxKind::ConvertTimeTarget);
ast_node!(
    ConvertDaylightSavingTarget,
    SyntaxKind::ConvertDaylightSavingTarget
);
ast_node!(DescribeStmt, SyntaxKind::DescribeStmt);
ast_node!(DescribeTableOperand, SyntaxKind::DescribeTableOperand);
ast_node!(DescribeLinesTarget, SyntaxKind::DescribeLinesTarget);
ast_node!(ReplaceStmt, SyntaxKind::ReplaceStmt);
ast_node!(ReplacePatternOperand, SyntaxKind::ReplacePatternOperand);
ast_node!(ReplaceTargetOperand, SyntaxKind::ReplaceTargetOperand);
ast_node!(ReplaceWithOperand, SyntaxKind::ReplaceWithOperand);
ast_node!(WaitStmt, SyntaxKind::WaitStmt);
ast_node!(WaitOperand, SyntaxKind::WaitOperand);
ast_node!(PerformStmt, SyntaxKind::PerformStmt);
ast_node!(SubmitStmt, SyntaxKind::SubmitStmt);
ast_node!(SubmitTarget, SyntaxKind::SubmitTarget);
ast_node!(
    SubmitSelectionScreenOperand,
    SyntaxKind::SubmitSelectionScreenOperand
);
ast_node!(
    SubmitSelectionSetOperand,
    SyntaxKind::SubmitSelectionSetOperand
);
ast_node!(
    SubmitSelectionSetsProgramOperand,
    SyntaxKind::SubmitSelectionSetsProgramOperand
);
ast_node!(
    SubmitSelectionTableOperand,
    SyntaxKind::SubmitSelectionTableOperand
);
ast_node!(SubmitWithClause, SyntaxKind::SubmitWithClause);
ast_node!(
    SubmitFreeSelectionsOperand,
    SyntaxKind::SubmitFreeSelectionsOperand
);
ast_node!(SubmitLineSizeOperand, SyntaxKind::SubmitLineSizeOperand);
ast_node!(SubmitLineCountOperand, SyntaxKind::SubmitLineCountOperand);
ast_node!(
    SubmitSpoolParametersOperand,
    SyntaxKind::SubmitSpoolParametersOperand
);
ast_node!(
    SubmitArchiveParametersOperand,
    SyntaxKind::SubmitArchiveParametersOperand
);
ast_node!(SubmitUserOperand, SyntaxKind::SubmitUserOperand);
ast_node!(SubmitJobOperand, SyntaxKind::SubmitJobOperand);
ast_node!(SubmitJobNumberOperand, SyntaxKind::SubmitJobNumberOperand);
ast_node!(SubmitLanguageOperand, SyntaxKind::SubmitLanguageOperand);
ast_node!(CreateObjectStmt, SyntaxKind::CreateObjectStmt);
ast_node!(CreateDataStmt, SyntaxKind::CreateDataStmt);
ast_node!(CallStmt, SyntaxKind::CallStmt);
ast_node!(CallMethodStmt, SyntaxKind::CallMethodStmt);
ast_node!(CallMethodTarget, SyntaxKind::CallMethodTarget);
ast_node!(RaiseStmt, SyntaxKind::RaiseStmt);
ast_node!(MessageStmt, SyntaxKind::MessageStmt);
ast_node!(MessageHeadClause, SyntaxKind::MessageHeadClause);
ast_node!(MessageIdOperand, SyntaxKind::MessageIdOperand);
ast_node!(MessageTypeOperand, SyntaxKind::MessageTypeOperand);
ast_node!(MessageNumberOperand, SyntaxKind::MessageNumberOperand);
ast_node!(MessageCodeOperand, SyntaxKind::MessageCodeOperand);
ast_node!(MessageWithClause, SyntaxKind::MessageWithClause);
ast_node!(MessageOperand, SyntaxKind::MessageOperand);
ast_node!(MessageTextPoolId, SyntaxKind::MessageTextPoolId);
ast_node!(MessageIntoClause, SyntaxKind::MessageIntoClause);
ast_node!(
    MessageDisplayLikeClause,
    SyntaxKind::MessageDisplayLikeClause
);
ast_node!(MessageRaisingClause, SyntaxKind::MessageRaisingClause);
ast_node!(FindStmt, SyntaxKind::FindStmt);
ast_node!(ContinueStmt, SyntaxKind::ContinueStmt);
ast_node!(ExitStmt, SyntaxKind::ExitStmt);
ast_node!(ReturnStmt, SyntaxKind::ReturnStmt);
ast_node!(StopStmt, SyntaxKind::StopStmt);
ast_node!(FindPatternOperand, SyntaxKind::FindPatternOperand);
ast_node!(FindInOperand, SyntaxKind::FindInOperand);
ast_node!(FindMatchTarget, SyntaxKind::FindMatchTarget);
ast_node!(FindSubmatchTarget, SyntaxKind::FindSubmatchTarget);
ast_node!(FindResultsTarget, SyntaxKind::FindResultsTarget);
ast_node!(ReadTableStmt, SyntaxKind::ReadTableStmt);
ast_node!(ReadDatasetStmt, SyntaxKind::ReadDatasetStmt);
ast_node!(ReadTextpoolStmt, SyntaxKind::ReadTextpoolStmt);
ast_node!(AuthorityCheckStmt, SyntaxKind::AuthorityCheckStmt);
ast_node!(
    AuthorityCheckObjectOperand,
    SyntaxKind::AuthorityCheckObjectOperand
);
ast_node!(
    AuthorityCheckUserOperand,
    SyntaxKind::AuthorityCheckUserOperand
);
ast_node!(AuthorityCheckIdClause, SyntaxKind::AuthorityCheckIdClause);
ast_node!(AuthorityCheckIdOperand, SyntaxKind::AuthorityCheckIdOperand);
ast_node!(
    AuthorityCheckFieldOperand,
    SyntaxKind::AuthorityCheckFieldOperand
);
ast_node!(GetBadiStmt, SyntaxKind::GetBadiStmt);
ast_node!(GetCursorStmt, SyntaxKind::GetCursorStmt);
ast_node!(SetPfStatusStmt, SyntaxKind::SetPfStatusStmt);
ast_node!(SetTitlebarStmt, SyntaxKind::SetTitlebarStmt);
ast_node!(SetScreenStmt, SyntaxKind::SetScreenStmt);
ast_node!(SetCursorStmt, SyntaxKind::SetCursorStmt);
ast_node!(SetHandlerStmt, SyntaxKind::SetHandlerStmt);
ast_node!(WriteStmt, SyntaxKind::WriteStmt);
ast_node!(TransferStmt, SyntaxKind::TransferStmt);
ast_node!(
    GenerateSubroutinePoolStmt,
    SyntaxKind::GenerateSubroutinePoolStmt
);
ast_node!(GenerateDynproStmt, SyntaxKind::GenerateDynproStmt);
ast_node!(SplitStmt, SyntaxKind::SplitStmt);
ast_node!(SplitSourceOperand, SyntaxKind::SplitSourceOperand);
ast_node!(SplitSeparatorOperand, SyntaxKind::SplitSeparatorOperand);
ast_node!(SplitTargetOperand, SyntaxKind::SplitTargetOperand);
ast_node!(ConcatenateStmt, SyntaxKind::ConcatenateStmt);
ast_node!(
    ConcatenateSourceOperand,
    SyntaxKind::ConcatenateSourceOperand
);
ast_node!(
    ConcatenateTargetOperand,
    SyntaxKind::ConcatenateTargetOperand
);
ast_node!(
    ConcatenateSeparatorOperand,
    SyntaxKind::ConcatenateSeparatorOperand
);
ast_node!(SelectStmt, SyntaxKind::SelectStmt);
ast_node!(OpenCursorStmt, SyntaxKind::OpenCursorStmt);
ast_node!(FetchCursorStmt, SyntaxKind::FetchCursorStmt);
ast_node!(CloseCursorStmt, SyntaxKind::CloseCursorStmt);
ast_node!(OpenDatasetStmt, SyntaxKind::OpenDatasetStmt);
ast_node!(CloseDatasetStmt, SyntaxKind::CloseDatasetStmt);
ast_node!(CursorHandleOperand, SyntaxKind::CursorHandleOperand);
ast_node!(SelectQuery, SyntaxKind::SelectQuery);
ast_node!(SelectProjectionList, SyntaxKind::SelectProjectionList);
ast_node!(SelectFromClause, SyntaxKind::SelectFromClause);
ast_node!(SelectIntoClause, SyntaxKind::SelectIntoClause);
ast_node!(SelectJoinClause, SyntaxKind::SelectJoinClause);
ast_node!(SelectForUpdateClause, SyntaxKind::SelectForUpdateClause);
ast_node!(SelectPackageSizeClause, SyntaxKind::SelectPackageSizeClause);
ast_node!(SelectOffsetClause, SyntaxKind::SelectOffsetClause);
ast_node!(SelectAbapOptionsClause, SyntaxKind::SelectAbapOptionsClause);
ast_node!(SelectSetOperatorClause, SyntaxKind::SelectSetOperatorClause);
ast_node!(SqlPredicateExpr, SyntaxKind::SqlPredicateExpr);
ast_node!(SqlPredicateOperand, SyntaxKind::SqlPredicateOperand);
ast_node!(SqlProjectionItem, SyntaxKind::SqlProjectionItem);
ast_node!(SqlDataSource, SyntaxKind::SqlDataSource);
ast_node!(SqlAliasClause, SyntaxKind::SqlAliasClause);
ast_node!(SqlAlias, SyntaxKind::SqlAlias);
ast_node!(SqlColumnRef, SyntaxKind::SqlColumnRef);
ast_node!(SqlQualifiedColumnRef, SyntaxKind::SqlQualifiedColumnRef);
ast_node!(SqlStar, SyntaxKind::SqlStar);
ast_node!(SqlQualifiedStar, SyntaxKind::SqlQualifiedStar);
ast_node!(SqlAggregateCall, SyntaxKind::SqlAggregateCall);
ast_node!(SqlHostExpr, SyntaxKind::SqlHostExpr);
ast_node!(SqlParenGroup, SyntaxKind::SqlParenGroup);
ast_node!(DeleteStmt, SyntaxKind::DeleteStmt);
ast_node!(SortStmt, SyntaxKind::SortStmt);
ast_node!(AddStmt, SyntaxKind::AddStmt);
ast_node!(SubtractStmt, SyntaxKind::SubtractStmt);
ast_node!(TranslateStmt, SyntaxKind::TranslateStmt);
ast_node!(ShiftStmt, SyntaxKind::ShiftStmt);
ast_node!(SearchStmt, SyntaxKind::SearchStmt);
ast_node!(FormatStmt, SyntaxKind::FormatStmt);
ast_node!(PositionStmt, SyntaxKind::PositionStmt);
ast_node!(HideStmt, SyntaxKind::HideStmt);
ast_node!(ReadLineStmt, SyntaxKind::ReadLineStmt);
ast_node!(ModifyLineStmt, SyntaxKind::ModifyLineStmt);
ast_node!(FieldStmt, SyntaxKind::FieldStmt);
ast_node!(SuppressDialogStmt, SyntaxKind::SuppressDialogStmt);
ast_node!(FieldGroupsStmt, SyntaxKind::FieldGroupsStmt);
ast_node!(InsertExtractStmt, SyntaxKind::InsertExtractStmt);
ast_node!(UpdateStmt, SyntaxKind::UpdateStmt);
ast_node!(UpdateTarget, SyntaxKind::UpdateTarget);
ast_node!(UpdateSetClause, SyntaxKind::UpdateSetClause);
ast_node!(UpdateSetAssignment, SyntaxKind::UpdateSetAssignment);
ast_node!(UpdateSetValueOperand, SyntaxKind::UpdateSetValueOperand);
ast_node!(UpdateFromOperand, SyntaxKind::UpdateFromOperand);
ast_node!(UpdateWhereClause, SyntaxKind::UpdateWhereClause);

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
                | SyntaxKind::ParametersDecl
                | SyntaxKind::TablesDecl
                | SyntaxKind::RangesDecl
                | SyntaxKind::ControlsDecl
                | SyntaxKind::SelectOptionsDecl
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

impl<'a> IncludeName<'a> {
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

fn block_header_signature_text<'a>(syntax: SyntaxNodeRef<'a>, source: &str) -> String {
    let mut header_tokens = Vec::new();
    for child in syntax.children() {
        let child_tokens = child.token_descendants();
        let stop_after_child = child_tokens
            .iter()
            .any(|token| token.text(source).is_some_and(|text| text == "."));
        for token in child_tokens {
            if token.text(source).is_some_and(|text| text == ".") {
                return signature_text_from_tokens(&header_tokens, source);
            }
            header_tokens.push(token);
        }
        if stop_after_child {
            break;
        }
    }
    signature_text_from_tokens(&header_tokens, source)
}

impl<'a> FormDecl<'a> {
    pub fn name_token(self) -> Option<DataDeclName<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::DataDeclName)
            .and_then(DataDeclName::cast)
    }

    pub fn signature_text(self, source: &str) -> String {
        block_header_signature_text(self.syntax, source)
    }

    pub fn param_sections(
        self,
    ) -> impl DoubleEndedIterator<Item = FormParamSection<'a>> + Clone + 'a {
        self.syntax.children().filter_map(FormParamSection::cast)
    }
}

impl<'a> FormParamSection<'a> {
    pub fn kind(self, source: &str) -> Option<FormParamSectionKind> {
        let token = self.syntax.children_by_kind(SyntaxKind::Token).next()?;
        let text = token.text(source)?;
        if text.eq_ignore_ascii_case("tables") {
            Some(FormParamSectionKind::Tables)
        } else if text.eq_ignore_ascii_case("using") {
            Some(FormParamSectionKind::Using)
        } else if text.eq_ignore_ascii_case("changing") {
            Some(FormParamSectionKind::Changing)
        } else if text.eq_ignore_ascii_case("raising") {
            Some(FormParamSectionKind::Raising)
        } else {
            None
        }
    }

    pub fn params(self) -> impl DoubleEndedIterator<Item = FormParam<'a>> + Clone + 'a {
        self.syntax.children().filter_map(FormParam::cast)
    }
}

impl<'a> FormParam<'a> {
    pub fn name_token(self) -> Option<DataDeclName<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::DataDeclName)
            .and_then(DataDeclName::cast)
    }

    pub fn passing_kind(self, source: &str) -> FormParamPassingKind {
        let mut tokens = self.syntax.children_by_kind(SyntaxKind::Token);
        let Some(first) = tokens.next() else {
            return FormParamPassingKind::Direct;
        };
        let Some(text) = first.text(source) else {
            return FormParamPassingKind::Direct;
        };
        if text.eq_ignore_ascii_case("value") {
            FormParamPassingKind::Value
        } else if text.eq_ignore_ascii_case("reference") {
            FormParamPassingKind::Reference
        } else {
            FormParamPassingKind::Direct
        }
    }

    pub fn type_clause_kind(self, source: &str) -> Option<TypeClauseKind> {
        self.syntax
            .children_by_kind(SyntaxKind::Token)
            .find_map(|token| {
                let text = token.text(source)?;
                if text.eq_ignore_ascii_case("type") {
                    Some(TypeClauseKind::Type)
                } else if text.eq_ignore_ascii_case("like") {
                    Some(TypeClauseKind::Like)
                } else {
                    None
                }
            })
    }

    pub fn type_ref(self) -> Option<TypeRefSimple<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::TypeRefSimple)
            .and_then(TypeRefSimple::cast)
    }

    pub fn is_resumable(self, source: &str) -> bool {
        self.syntax
            .children_by_kind(SyntaxKind::Token)
            .next()
            .and_then(|token| token.text(source))
            .is_some_and(|text| text.eq_ignore_ascii_case("resumable"))
    }
}

impl<'a> FunctionDecl<'a> {
    pub fn name_token(self) -> Option<DataDeclName<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::DataDeclName)
            .and_then(DataDeclName::cast)
    }

    pub fn signature_text(self, source: &str) -> String {
        block_header_signature_text(self.syntax, source)
    }

    pub fn param_sections(
        self,
    ) -> impl DoubleEndedIterator<Item = FunctionParamSection<'a>> + Clone + 'a {
        self.syntax
            .children()
            .filter_map(FunctionParamSection::cast)
    }
}

impl<'a> FunctionParamSection<'a> {
    pub fn kind(self, source: &str) -> Option<FunctionParamSectionKind> {
        let token = self.syntax.children_by_kind(SyntaxKind::Token).next()?;
        let text = token.text(source)?;
        if text.eq_ignore_ascii_case("importing") {
            Some(FunctionParamSectionKind::Importing)
        } else if text.eq_ignore_ascii_case("exporting") {
            Some(FunctionParamSectionKind::Exporting)
        } else if text.eq_ignore_ascii_case("changing") {
            Some(FunctionParamSectionKind::Changing)
        } else if text.eq_ignore_ascii_case("tables") {
            Some(FunctionParamSectionKind::Tables)
        } else if text.eq_ignore_ascii_case("raising") {
            Some(FunctionParamSectionKind::Raising)
        } else if text.eq_ignore_ascii_case("exceptions") {
            Some(FunctionParamSectionKind::Exceptions)
        } else {
            None
        }
    }

    pub fn params(self) -> impl DoubleEndedIterator<Item = FunctionParam<'a>> + Clone + 'a {
        self.syntax.children().filter_map(FunctionParam::cast)
    }
}

impl<'a> FunctionParam<'a> {
    pub fn name_token(self) -> Option<DataDeclName<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::DataDeclName)
            .and_then(DataDeclName::cast)
    }

    pub fn passing_kind(self, source: &str) -> FormParamPassingKind {
        let mut tokens = self.syntax.children_by_kind(SyntaxKind::Token);
        let Some(first) = tokens.next() else {
            return FormParamPassingKind::Direct;
        };
        let Some(text) = first.text(source) else {
            return FormParamPassingKind::Direct;
        };
        if text.eq_ignore_ascii_case("value") {
            FormParamPassingKind::Value
        } else if text.eq_ignore_ascii_case("reference") {
            FormParamPassingKind::Reference
        } else {
            FormParamPassingKind::Direct
        }
    }

    pub fn type_clause_kind(self, source: &str) -> Option<TypeClauseKind> {
        self.syntax
            .children_by_kind(SyntaxKind::Token)
            .find_map(|token| {
                let text = token.text(source)?;
                if text.eq_ignore_ascii_case("type") {
                    Some(TypeClauseKind::Type)
                } else if text.eq_ignore_ascii_case("like") {
                    Some(TypeClauseKind::Like)
                } else {
                    None
                }
            })
    }

    pub fn type_ref(self) -> Option<TypeRefSimple<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::TypeRefSimple)
            .and_then(TypeRefSimple::cast)
    }

    pub fn is_optional(self, source: &str) -> bool {
        self.syntax
            .children_by_kind(SyntaxKind::Token)
            .any(|token| {
                token
                    .text(source)
                    .is_some_and(|text| text.eq_ignore_ascii_case("optional"))
            })
    }

    pub fn has_default_value(self, source: &str) -> bool {
        self.syntax
            .children_by_kind(SyntaxKind::Token)
            .any(|token| {
                token
                    .text(source)
                    .is_some_and(|text| text.eq_ignore_ascii_case("default"))
            })
    }

    pub fn is_resumable(self, source: &str) -> bool {
        self.syntax
            .children_by_kind(SyntaxKind::Token)
            .next()
            .and_then(|token| token.text(source))
            .is_some_and(|text| text.eq_ignore_ascii_case("resumable"))
    }
}

impl<'a> ClassSectionVisibility<'a> {
    pub fn kind(self, source: &str) -> Option<ClassSectionVisibilityKind> {
        let token = self.syntax.children_by_kind(SyntaxKind::Token).next()?;
        let text = token.text(source)?;
        if text.eq_ignore_ascii_case("public") {
            Some(ClassSectionVisibilityKind::Public)
        } else if text.eq_ignore_ascii_case("protected") {
            Some(ClassSectionVisibilityKind::Protected)
        } else if text.eq_ignore_ascii_case("private") {
            Some(ClassSectionVisibilityKind::Private)
        } else {
            None
        }
    }
}

impl<'a> ClassSectionStmt<'a> {
    pub fn visibility(self) -> Option<ClassSectionVisibility<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::ClassSectionVisibility)
            .and_then(ClassSectionVisibility::cast)
    }
}

impl<'a> ClassInheritanceClause<'a> {
    pub fn type_ref(self) -> Option<TypeRefSimple<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::TypeRefSimple)
            .and_then(TypeRefSimple::cast)
    }
}

impl<'a> ClassDecl<'a> {
    pub fn name_token(self) -> Option<DataDeclName<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::DataDeclName)
            .and_then(DataDeclName::cast)
    }

    pub fn is_implementation(self) -> bool {
        self.syntax
            .child_by_kind(SyntaxKind::ClassImplementationMarker)
            .is_some()
    }

    pub fn inheritance_clause(self) -> Option<ClassInheritanceClause<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::ClassInheritanceClause)
            .and_then(ClassInheritanceClause::cast)
    }

    pub fn superclass(self) -> Option<TypeRefSimple<'a>> {
        self.inheritance_clause()
            .and_then(|clause| clause.type_ref())
    }

    pub fn is_abstract(self, source: &str) -> bool {
        self.syntax.children().any(|child| {
            child.kind() == SyntaxKind::Token
                && child
                    .text(source)
                    .is_some_and(|text| text.eq_ignore_ascii_case("abstract"))
        })
    }
}

impl<'a> InterfaceDecl<'a> {
    pub fn name_token(self) -> Option<DataDeclName<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::DataDeclName)
            .and_then(DataDeclName::cast)
    }
}

impl<'a> MethodDeclTarget<'a> {
    pub fn qualifier(self) -> Option<TypeRefSimple<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::TypeRefSimple)
            .and_then(TypeRefSimple::cast)
    }

    pub fn member_name(self) -> Option<DataDeclName<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::DataDeclName)
            .and_then(DataDeclName::cast)
    }
}

impl<'a> MethodDecl<'a> {
    pub fn target(self) -> Option<MethodDeclTarget<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::MethodDeclTarget)
            .and_then(MethodDeclTarget::cast)
    }
}

impl<'a> IncludeStmt<'a> {
    pub fn names(self) -> impl DoubleEndedIterator<Item = IncludeName<'a>> + Clone + 'a {
        self.syntax.children().filter_map(IncludeName::cast)
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

    pub fn storage_kind(self, source: &str) -> Option<DataLikeStorageKind> {
        match self.syntax.kind() {
            SyntaxKind::ConstantsDecl => Some(DataLikeStorageKind::Constant),
            SyntaxKind::StaticsDecl => Some(DataLikeStorageKind::Static),
            SyntaxKind::ParametersDecl
            | SyntaxKind::TablesDecl
            | SyntaxKind::RangesDecl
            | SyntaxKind::ControlsDecl
            | SyntaxKind::SelectOptionsDecl => Some(DataLikeStorageKind::Instance),
            SyntaxKind::DataDecl => {
                let mut texts = self
                    .syntax
                    .children_by_kind(SyntaxKind::Token)
                    .filter(|token| token.token_kind() != Some(TokenKind::Comment))
                    .filter_map(|token| token.text(source));
                let first = texts.next()?;
                if first.eq_ignore_ascii_case("data") {
                    return Some(DataLikeStorageKind::Instance);
                }
                let second = texts.next()?;
                let third = texts.next()?;
                if first.eq_ignore_ascii_case("class")
                    && second == "-"
                    && third.eq_ignore_ascii_case("data")
                {
                    Some(DataLikeStorageKind::Static)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn signature_text(self, source: &str) -> String {
        render_syntax_text(self.syntax, source, true)
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
                    text.eq_ignore_ascii_case("type")
                        || text.eq_ignore_ascii_case("like")
                        || text.eq_ignore_ascii_case("for")
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
        } else if text.eq_ignore_ascii_case("for") {
            Some(TypeClauseKind::For)
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
                if text.eq_ignore_ascii_case("for") {
                    namespace = Some(TypeClauseKind::For);
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

    pub fn declared_name(self, source: &str) -> Option<(Arc<str>, TextRange)> {
        if let Some(name) = self.name() {
            return Some((name.name(source)?, name.range()));
        }

        let mut tokens = self
            .syntax
            .children_by_kind(SyntaxKind::Token)
            .filter(|token| token.token_kind() != Some(TokenKind::Comment));
        let begin = tokens.next()?;
        let of = tokens.next()?;
        let name = tokens.next()?;
        let part = tokens.next();
        if !begin
            .text(source)
            .is_some_and(|text| text.eq_ignore_ascii_case("begin"))
            || !of
                .text(source)
                .is_some_and(|text| text.eq_ignore_ascii_case("of"))
            || (name
                .text(source)
                .is_some_and(|text| text.eq_ignore_ascii_case("common"))
                && part
                    .and_then(|token| token.text(source))
                    .is_some_and(|text| text.eq_ignore_ascii_case("part")))
        {
            return None;
        }
        Some((name.lower_trimmed_text(source)?, name.range()))
    }
}

impl<'a> StructuredIncludeClause<'a> {
    pub fn kind(self, source: &str) -> Option<StructuredIncludeKind> {
        let mut tokens = self.syntax.children_by_kind(SyntaxKind::Token);
        let include = tokens.next()?;
        if !include
            .text(source)
            .is_some_and(|text| text.eq_ignore_ascii_case("include"))
        {
            return None;
        }
        let kind = tokens.next()?;
        let text = kind.text(source)?;
        if text.eq_ignore_ascii_case("type") {
            Some(StructuredIncludeKind::Type)
        } else if text.eq_ignore_ascii_case("structure") {
            Some(StructuredIncludeKind::Structure)
        } else {
            None
        }
    }

    pub fn type_ref(self) -> Option<TypeRefSimple<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::TypeRefSimple)
            .and_then(TypeRefSimple::cast)
    }

    pub fn alias_name_token(self, source: &str) -> Option<SyntaxNodeRef<'a>> {
        let mut saw_as = false;
        for token in self.syntax.children_by_kind(SyntaxKind::Token) {
            let text = token.text(source)?;
            if saw_as {
                return matches!(token.token_kind(), Some(TokenKind::Ident)).then_some(token);
            }
            if text.eq_ignore_ascii_case("as") {
                saw_as = true;
            }
        }
        None
    }

    pub fn alias_name(self, source: &str) -> Option<Arc<str>> {
        self.alias_name_token(source)?.lower_trimmed_text(source)
    }

    pub fn suffix_token(self, source: &str) -> Option<SyntaxNodeRef<'a>> {
        let mut saw_suffix = false;
        for token in self.syntax.children_by_kind(SyntaxKind::Token) {
            let text = token.text(source)?;
            if saw_suffix {
                return matches!(token.token_kind(), Some(TokenKind::Ident)).then_some(token);
            }
            if text.eq_ignore_ascii_case("suffix") {
                saw_suffix = true;
            }
        }
        None
    }

    pub fn suffix(self, source: &str) -> Option<Arc<str>> {
        self.suffix_token(source)?.lower_trimmed_text(source)
    }
}

fn render_syntax_text(node: SyntaxNodeRef<'_>, source: &str, stop_at_period: bool) -> String {
    fn visit(
        node: SyntaxNodeRef<'_>,
        source: &str,
        stop_at_period: bool,
        rendered: &mut String,
        prev_text: &mut Option<String>,
        done: &mut bool,
    ) {
        if *done {
            return;
        }
        if node.kind() == SyntaxKind::Token {
            if node.token_kind() == Some(TokenKind::Comment) {
                return;
            }
            let Some(text) = node.text(source) else {
                return;
            };
            if stop_at_period && text == "." {
                *done = true;
                return;
            }
            let needs_space = !rendered.is_empty()
                && !matches!(text, "," | ":" | "-" | ")" | "]")
                && !matches!(prev_text.as_deref(), Some("(" | "[" | ":" | "-"));
            if needs_space {
                rendered.push(' ');
            }
            rendered.push_str(text);
            *prev_text = Some(text.to_string());
            return;
        }
        for child in node.children() {
            visit(child, source, stop_at_period, rendered, prev_text, done);
            if *done {
                break;
            }
        }
    }

    let mut rendered = String::new();
    let mut prev_text = None;
    let mut done = false;
    visit(
        node,
        source,
        stop_at_period,
        &mut rendered,
        &mut prev_text,
        &mut done,
    );
    rendered
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

impl<'a> TemplateExpr<'a> {
    pub fn wrapped_expr(self) -> Option<SyntaxNodeRef<'a>> {
        let mut children = self.syntax.children();
        let child = children.next()?;
        if children.next().is_some() || child.kind() == SyntaxKind::TemplateInterpolation {
            return None;
        }
        Some(child)
    }
}

impl<'a> TemplateInterpolation<'a> {
    pub fn expr(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax
            .non_token_children()
            .find(|child| child.kind() != SyntaxKind::TemplateFormatSpec)
    }
}

impl<'a> ParenExpr<'a> {
    pub fn inner_expr(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
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

impl<'a> TableExpr<'a> {
    pub fn base(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> ConstructorExpr<'a> {
    pub fn keyword_token(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.children_by_kind(SyntaxKind::Token).next()
    }

    pub fn keyword(self, source: &str) -> Option<Arc<str>> {
        self.keyword_token()?.lower_trimmed_text(source)
    }

    pub fn type_ref(self) -> Option<TypeRefSimple<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::TypeRefSimple)
            .and_then(TypeRefSimple::cast)
    }

    pub fn arg_list(self) -> Option<CallArgList<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::CallArgList)
            .and_then(CallArgList::cast)
    }
}

impl<'a> LetExpr<'a> {
    pub fn bindings(
        self,
    ) -> impl DoubleEndedIterator<Item = ConstructorLetBinding<'a>> + Clone + 'a {
        self.syntax
            .children()
            .filter_map(ConstructorLetBinding::cast)
    }

    pub fn result_children(self, source: &str) -> Vec<SyntaxNodeRef<'a>> {
        let mut saw_in = false;
        let mut out = Vec::new();
        for child in self.syntax.children() {
            if child.kind() == SyntaxKind::Token
                && child
                    .text(source)
                    .is_some_and(|text| text.eq_ignore_ascii_case("in"))
            {
                saw_in = true;
                continue;
            }
            if saw_in && child.kind() != SyntaxKind::Token {
                out.push(child);
            }
        }
        out
    }
}

impl<'a> ConstructorLetBinding<'a> {
    pub fn name_token(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.children_by_kind(SyntaxKind::Token).next()
    }

    pub fn value_children(self) -> Vec<SyntaxNodeRef<'a>> {
        self.syntax.children().skip(2).collect()
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

    pub fn constructor_when_clauses(
        self,
    ) -> impl DoubleEndedIterator<Item = ConstructorWhenClause<'a>> + Clone + 'a {
        self.syntax
            .children()
            .filter_map(ConstructorWhenClause::cast)
    }

    pub fn constructor_else_clauses(
        self,
    ) -> impl DoubleEndedIterator<Item = ConstructorElseClause<'a>> + Clone + 'a {
        self.syntax
            .children()
            .filter_map(ConstructorElseClause::cast)
    }

    pub fn constructor_for_clauses(
        self,
    ) -> impl DoubleEndedIterator<Item = ConstructorForClause<'a>> + Clone + 'a {
        self.syntax
            .children()
            .filter_map(ConstructorForClause::cast)
    }

    pub fn constructor_init_clauses(
        self,
    ) -> impl DoubleEndedIterator<Item = ConstructorInitClause<'a>> + Clone + 'a {
        self.syntax
            .children()
            .filter_map(ConstructorInitClause::cast)
    }

    pub fn constructor_next_clauses(
        self,
    ) -> impl DoubleEndedIterator<Item = ConstructorNextClause<'a>> + Clone + 'a {
        self.syntax
            .children()
            .filter_map(ConstructorNextClause::cast)
    }

    pub fn constructor_named_assignments(
        self,
    ) -> impl DoubleEndedIterator<Item = ConstructorNamedAssignment<'a>> + Clone + 'a {
        self.syntax
            .children()
            .filter_map(ConstructorNamedAssignment::cast)
    }

    pub fn corresponding_mapping_clauses(
        self,
    ) -> impl DoubleEndedIterator<Item = ConstructorCorrespondingMappingClause<'a>> + Clone + 'a
    {
        self.syntax
            .children()
            .filter_map(ConstructorCorrespondingMappingClause::cast)
    }

    pub fn corresponding_except_clauses(
        self,
    ) -> impl DoubleEndedIterator<Item = ConstructorCorrespondingExceptClause<'a>> + Clone + 'a
    {
        self.syntax
            .children()
            .filter_map(ConstructorCorrespondingExceptClause::cast)
    }

    pub fn let_exprs(self) -> impl DoubleEndedIterator<Item = LetExpr<'a>> + Clone + 'a {
        self.syntax.children().filter_map(LetExpr::cast)
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

impl<'a> ConstructorWhenClause<'a> {
    pub fn condition(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.non_token_children().next()
    }

    pub fn result(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.non_token_children().nth(1)
    }
}

impl<'a> ConstructorElseClause<'a> {
    pub fn result(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> ConstructorForClause<'a> {
    pub fn iterator_name_token(self, source: &str) -> Option<SyntaxNodeRef<'a>> {
        self.syntax
            .children_by_kind(SyntaxKind::Token)
            .find(|token| {
                token.text(source).is_some_and(|text| {
                    !matches!(
                        text.to_ascii_uppercase().as_str(),
                        "FOR" | "IN" | "WHERE" | "THEN" | "UNTIL" | "WHILE" | "="
                    )
                })
            })
    }

    pub fn is_in_form(self, source: &str) -> bool {
        self.syntax
            .children_by_kind(SyntaxKind::Token)
            .any(|token| {
                token
                    .text(source)
                    .is_some_and(|text| text.eq_ignore_ascii_case("in"))
            })
    }

    pub fn source_expr(self, source: &str) -> Option<SyntaxNodeRef<'a>> {
        self.is_in_form(source)
            .then(|| self.syntax.non_token_children().next())
            .flatten()
    }

    pub fn init_expr(self, source: &str) -> Option<SyntaxNodeRef<'a>> {
        (!self.is_in_form(source))
            .then(|| self.syntax.non_token_children().next())
            .flatten()
    }

    pub fn then_expr(self, source: &str) -> Option<SyntaxNodeRef<'a>> {
        if self.is_in_form(source) {
            return None;
        }
        let has_then = self
            .syntax
            .children_by_kind(SyntaxKind::Token)
            .any(|token| {
                token
                    .text(source)
                    .is_some_and(|text| text.eq_ignore_ascii_case("then"))
            });
        has_then
            .then(|| self.syntax.non_token_children().nth(1))
            .flatten()
    }

    pub fn condition_expr(self, source: &str) -> Option<SyntaxNodeRef<'a>> {
        if self.is_in_form(source) {
            return None;
        }
        let has_then = self
            .syntax
            .children_by_kind(SyntaxKind::Token)
            .any(|token| {
                token
                    .text(source)
                    .is_some_and(|text| text.eq_ignore_ascii_case("then"))
            });
        self.syntax
            .non_token_children()
            .nth(if has_then { 2 } else { 1 })
    }

    pub fn where_clause(self) -> Option<ConstructorWhereClause<'a>> {
        self.syntax
            .children()
            .find_map(ConstructorWhereClause::cast)
    }

    pub fn body_children(self, source: &str) -> Vec<SyntaxNodeRef<'a>> {
        let non_tokens: Vec<_> = self.syntax.non_token_children().collect();
        let skip = if self.is_in_form(source) {
            1 + usize::from(self.where_clause().is_some())
        } else {
            2 + usize::from(self.then_expr(source).is_some())
        };
        non_tokens.into_iter().skip(skip).collect()
    }
}

impl<'a> ConstructorWhereClause<'a> {
    pub fn condition(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> ConstructorInitClause<'a> {
    pub fn assignments(
        self,
    ) -> impl DoubleEndedIterator<Item = ConstructorNamedAssignment<'a>> + Clone + 'a {
        self.syntax
            .children()
            .filter_map(ConstructorNamedAssignment::cast)
    }
}

impl<'a> ConstructorNextClause<'a> {
    pub fn assignments(
        self,
    ) -> impl DoubleEndedIterator<Item = ConstructorNamedAssignment<'a>> + Clone + 'a {
        self.syntax
            .children()
            .filter_map(ConstructorNamedAssignment::cast)
    }
}

impl<'a> ConstructorNamedAssignment<'a> {
    pub fn name_token(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.children_by_kind(SyntaxKind::Token).next()
    }

    pub fn value_children(self) -> Vec<SyntaxNodeRef<'a>> {
        self.syntax.children().skip(2).collect()
    }
}

impl<'a> ConstructorCorrespondingMappingClause<'a> {
    pub fn assignments(
        self,
    ) -> impl DoubleEndedIterator<Item = ConstructorCorrespondingMappingAssignment<'a>> + Clone + 'a
    {
        self.syntax
            .children()
            .filter_map(ConstructorCorrespondingMappingAssignment::cast)
    }
}

impl<'a> ConstructorCorrespondingMappingAssignment<'a> {
    pub fn target_token(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.children_by_kind(SyntaxKind::Token).next()
    }

    pub fn source_value(self, source: &str) -> Option<SyntaxNodeRef<'a>> {
        let mut seen_suffix = false;
        for child in self.syntax.children() {
            match child.kind() {
                SyntaxKind::Token => {
                    if child.text(source).is_some_and(|text| {
                        matches!(
                            text.to_ascii_uppercase().as_str(),
                            "DEFAULT" | "MAPPING" | "EXCEPT"
                        )
                    }) {
                        seen_suffix = true;
                    }
                }
                _ if !seen_suffix => return Some(child),
                _ => {}
            }
        }
        None
    }

    pub fn default_value(self, source: &str) -> Option<SyntaxNodeRef<'a>> {
        let mut after_default = false;
        for child in self.syntax.children() {
            match child.kind() {
                SyntaxKind::Token => {
                    after_default = child
                        .text(source)
                        .is_some_and(|text| text.eq_ignore_ascii_case("DEFAULT"));
                }
                _ if after_default => return Some(child),
                _ => {}
            }
        }
        None
    }

    pub fn mapping_clause(self) -> Option<ConstructorCorrespondingMappingClause<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::ConstructorCorrespondingMappingClause)
            .and_then(ConstructorCorrespondingMappingClause::cast)
    }

    pub fn except_clause(self) -> Option<ConstructorCorrespondingExceptClause<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::ConstructorCorrespondingExceptClause)
            .and_then(ConstructorCorrespondingExceptClause::cast)
    }
}

impl<'a> ConstructorBaseClause<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> ConstructorLinesOfClause<'a> {
    pub fn source(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.non_token_children().next()
    }

    pub fn from_value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.non_token_children().nth(1)
    }

    pub fn to_value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.non_token_children().nth(2)
    }
}

impl<'a> ConstructorOptionalExpr<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
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
        self.entries(source)
            .into_iter()
            .next()
            .and_then(|entry| entry.name_token(source))
    }

    pub fn signature_text(self, source: &str) -> String {
        self.entries(source)
            .into_iter()
            .next()
            .map(|entry| entry.signature_text(source))
            .unwrap_or_default()
    }

    pub fn signature(self, source: &str) -> MethodsStmtSignature<'a> {
        self.entries(source)
            .into_iter()
            .next()
            .map(|entry| entry.signature(source))
            .unwrap_or(MethodsStmtSignature {
                is_redefinition: false,
                parameters: Vec::new(),
                raising: Vec::new(),
            })
    }

    pub fn entries(self, source: &str) -> Vec<MethodsStmtEntry<'a>> {
        let items = self.significant_children(source);
        let (kind, mut idx) = match self.member_kind(source) {
            Some(MethodsStmtKind::Instance) => (MethodsStmtKind::Instance, 1),
            Some(MethodsStmtKind::Class) => (MethodsStmtKind::Class, 3),
            None => return Vec::new(),
        };
        let mut entries = Vec::new();
        while idx < items.len() {
            while items
                .get(idx)
                .is_some_and(|item| Self::is_punctuation(*item, source))
            {
                idx += 1;
            }
            if items
                .get(idx)
                .is_some_and(|item| Self::token_text_is(*item, source, "."))
            {
                break;
            }
            if idx >= items.len() {
                break;
            }
            let start = idx;
            let mut depth = 0i32;
            while idx < items.len() {
                let item = items[idx];
                match item.text(source) {
                    Some("(" | "[" | "{") => depth += 1,
                    Some(")" | "]" | "}") => depth -= 1,
                    Some(",") | Some(".") if depth == 0 => break,
                    _ => {}
                }
                idx += 1;
            }
            let entry_items = items[start..idx].to_vec();
            if !entry_items.is_empty() {
                entries.push(MethodsStmtEntry {
                    kind,
                    items: entry_items,
                });
            }
            if items
                .get(idx)
                .is_some_and(|item| Self::token_text_is(*item, source, "."))
            {
                break;
            }
            idx += 1;
        }
        entries
    }

    fn parse_signature_items(
        items: &[SyntaxNodeRef<'a>],
        source: &str,
    ) -> MethodsStmtSignature<'a> {
        let mut signature = MethodsStmtSignature {
            is_redefinition: false,
            parameters: Vec::new(),
            raising: Vec::new(),
        };
        let mut idx = 0usize;
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
        let mut in_raising = false;
        let mut saw_parameter_section = false;
        while idx < items.len() {
            let item = items[idx];
            if Self::token_text_is(item, source, ".") {
                break;
            }
            if let Some(next_idx) = Self::header_modifier_span(items, idx, source) {
                if saw_parameter_section {
                    break;
                }
                if Self::token_text_is(item, source, "redefinition") {
                    signature.is_redefinition = true;
                }
                idx = next_idx;
                continue;
            }
            if Self::token_text_is(item, source, "raising") {
                saw_parameter_section = true;
                in_raising = true;
                section = None;
                idx += 1;
                continue;
            }
            if Self::token_text_is(item, source, "exceptions") {
                break;
            }
            section = match Self::parameter_section(item, source) {
                Some(next_section) => {
                    saw_parameter_section = true;
                    in_raising = false;
                    idx += 1;
                    Some(next_section)
                }
                None => section,
            };
            if Self::stops_parameter_scan(item, source) {
                break;
            }
            if in_raising {
                if let Some((raising, next_idx)) = Self::try_consume_raising(items, idx, source) {
                    signature.raising.push(raising);
                    idx = next_idx;
                    continue;
                }
            }
            if let Some(param_section) = section
                && let Some((param, next_idx)) =
                    Self::try_consume_parameter(items, idx, param_section, source)
            {
                signature.parameters.push(param);
                idx = next_idx;
                continue;
            }
            idx += 1;
        }

        signature
    }

    fn parse_event_handler_items(
        items: &[SyntaxNodeRef<'a>],
        source: &str,
    ) -> Option<MethodsStmtEventHandler<'a>> {
        let mut idx = 0usize;
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
        while idx < items.len() {
            let item = items[idx];
            if let Some(next_idx) = Self::header_modifier_span(items, idx, source) {
                idx = next_idx;
                continue;
            }
            if Self::token_text_is(item, source, "default")
                && items.get(idx + 1).is_some_and(|next| {
                    Self::token_text_is(*next, source, "ignore")
                        || Self::token_text_is(*next, source, "fail")
                })
            {
                idx += 2;
                continue;
            }
            break;
        }
        if !items
            .get(idx)
            .is_some_and(|item| Self::token_text_is(*item, source, "for"))
            || !items
                .get(idx + 1)
                .is_some_and(|item| Self::token_text_is(*item, source, "event"))
        {
            return None;
        }
        idx += 2;

        let event_qualifier = items.get(idx).copied().and_then(|item| {
            items
                .get(idx + 1)
                .filter(|tilde| Self::token_text_is(**tilde, source, "~"))
                .and(items.get(idx + 2))
                .filter(|event_name| Self::is_ident_token(**event_name, source))
                .map(|_| item)
        });
        let event_name = if event_qualifier.is_some() {
            let event_name = *items.get(idx + 2)?;
            idx += 3;
            event_name
        } else {
            let event_name = *items.get(idx)?;
            if !Self::is_ident_token(event_name, source) {
                return None;
            }
            idx += 1;
            event_name
        };

        if !items
            .get(idx)
            .is_some_and(|item| Self::token_text_is(*item, source, "of"))
        {
            return None;
        }
        idx += 1;
        let source_type_ref = items.get(idx).copied().and_then(TypeRefSimple::cast)?;
        idx += 1;

        let mut importing = Vec::new();
        if items
            .get(idx)
            .is_some_and(|item| Self::token_text_is(*item, source, "importing"))
        {
            idx += 1;
            while let Some(item) = items.get(idx).copied() {
                if Self::token_text_is(item, source, ".")
                    || Self::token_text_is(item, source, "raising")
                    || Self::token_text_is(item, source, "exceptions")
                {
                    break;
                }
                if Self::is_punctuation(item, source) {
                    idx += 1;
                    continue;
                }
                if Self::is_ident_token(item, source) {
                    importing.push(item);
                }
                idx += 1;
            }
        }

        Some(MethodsStmtEventHandler {
            event_qualifier,
            event_name,
            source_type_ref,
            importing,
        })
    }

    fn significant_children(self, source: &str) -> Vec<SyntaxNodeRef<'a>> {
        let _ = source;
        self.syntax.children().collect()
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

    fn stops_parameter_scan(item: SyntaxNodeRef<'a>, source: &str) -> bool {
        Self::token_text_is(item, source, ".")
            || Self::token_text_is(item, source, "raising")
            || Self::token_text_is(item, source, "exceptions")
    }

    fn try_consume_parameter(
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
        let next_idx = Self::skip_type_expression(items, j, source);
        let type_display_range = Self::token_span(&items[j..next_idx]);
        let is_optional = items.get(next_idx).is_some_and(|item| {
            Self::token_text_is(*item, source, "optional")
                || Self::token_text_is(*item, source, "default")
        });
        Some((
            MethodsStmtParameter {
                section,
                name,
                type_clause,
                type_ref,
                type_display_range,
                is_optional,
            },
            next_idx,
        ))
    }

    fn try_consume_raising(
        items: &[SyntaxNodeRef<'a>],
        idx: usize,
        source: &str,
    ) -> Option<(MethodsStmtRaising<'a>, usize)> {
        let mut j = idx;
        while items
            .get(j)
            .is_some_and(|item| Self::is_punctuation(*item, source))
        {
            j += 1;
        }

        let item = *items.get(j)?;
        if Self::token_text_is(item, source, "resumable") {
            let lparen = *items.get(j + 1)?;
            let type_ref = TypeRefSimple::cast(*items.get(j + 2)?)?;
            let rparen = *items.get(j + 3)?;
            if !Self::token_text_is(lparen, source, "(")
                || !Self::token_text_is(rparen, source, ")")
            {
                return None;
            }
            return Some((
                MethodsStmtRaising {
                    kind: MethodsRaiseKind::Resumable,
                    type_ref,
                },
                j + 4,
            ));
        }

        TypeRefSimple::cast(item).map(|type_ref| {
            (
                MethodsStmtRaising {
                    kind: MethodsRaiseKind::Raising,
                    type_ref,
                },
                j + 1,
            )
        })
    }

    fn parameter_name(
        items: &[SyntaxNodeRef<'a>],
        idx: usize,
        source: &str,
    ) -> Option<(SyntaxNodeRef<'a>, usize)> {
        let item = *items.get(idx)?;
        if Self::token_text_is(item, source, "!") {
            let ident = *items.get(idx + 1)?;
            if !Self::is_ident_token(ident, source) {
                return None;
            }
            return Some((ident, idx + 2));
        }
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

    fn skip_type_expression(items: &[SyntaxNodeRef<'a>], mut idx: usize, source: &str) -> usize {
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
                && (Self::stops_parameter_scan(item, source)
                    || Self::parameter_section(item, source).is_some()
                    || Self::header_modifier_span(items, idx, source).is_some()
                    || Self::token_text_is(item, source, "optional")
                    || Self::token_text_is(item, source, "default")
                    || Self::token_text_is(item, source, "preferred")
                    || Self::starts_parameter(items, idx, source))
            {
                return idx;
            }
            idx += 1;
        }
        idx
    }

    fn starts_parameter(items: &[SyntaxNodeRef<'a>], idx: usize, source: &str) -> bool {
        Self::parameter_name(items, idx, source)
            .and_then(|(_, next_idx)| items.get(next_idx).copied())
            .is_some_and(|next| {
                Self::token_text_is(next, source, "type")
                    || Self::token_text_is(next, source, "like")
            })
    }

    fn token_span(items: &[SyntaxNodeRef<'a>]) -> Option<(usize, usize)> {
        let mut first = None;
        let mut last = None;
        for item in items {
            for token in item.token_descendants() {
                let range = token.range();
                first.get_or_insert(range.start);
                last = Some(range.end);
            }
        }
        first.zip(last)
    }
}

impl<'a> EventsStmt<'a> {
    pub fn type_refs(&self) -> impl DoubleEndedIterator<Item = TypeRefSimple<'a>> + Clone + 'a {
        self.syntax.children().filter_map(TypeRefSimple::cast)
    }

    pub fn member_kind(self, source: &str) -> Option<EventsStmtKind> {
        let items = self.significant_children(source);
        let first = *items.first()?;
        if Self::token_text_is(first, source, "events") {
            return Some(EventsStmtKind::Instance);
        }
        if items.len() >= 3
            && Self::token_text_is(items[0], source, "class")
            && Self::token_text_is(items[1], source, "-")
            && Self::token_text_is(items[2], source, "events")
        {
            return Some(EventsStmtKind::Class);
        }
        None
    }

    pub fn entries(self, source: &str) -> Vec<EventsStmtEntry<'a>> {
        let items = self.significant_children(source);
        let (kind, mut idx) = match self.member_kind(source) {
            Some(EventsStmtKind::Instance) => (EventsStmtKind::Instance, 1),
            Some(EventsStmtKind::Class) => (EventsStmtKind::Class, 3),
            None => return Vec::new(),
        };
        let mut entries = Vec::new();
        while idx < items.len() {
            while items
                .get(idx)
                .is_some_and(|item| Self::is_punctuation(*item, source))
            {
                idx += 1;
            }
            if items
                .get(idx)
                .is_some_and(|item| Self::token_text_is(*item, source, "."))
            {
                break;
            }
            if idx >= items.len() {
                break;
            }
            let start = idx;
            let mut depth = 0i32;
            while idx < items.len() {
                let item = items[idx];
                match item.text(source) {
                    Some("(" | "[" | "{") => depth += 1,
                    Some(")" | "]" | "}") => depth -= 1,
                    Some(",") | Some(".") if depth == 0 => break,
                    _ => {}
                }
                idx += 1;
            }
            let entry_items = items[start..idx].to_vec();
            if !entry_items.is_empty() {
                entries.push(EventsStmtEntry {
                    kind,
                    items: entry_items,
                });
            }
            if items
                .get(idx)
                .is_some_and(|item| Self::token_text_is(*item, source, "."))
            {
                break;
            }
            idx += 1;
        }
        entries
    }

    fn parse_signature_items(items: &[SyntaxNodeRef<'a>], source: &str) -> EventsStmtSignature<'a> {
        let mut signature = EventsStmtSignature {
            parameters: Vec::new(),
        };
        let mut idx = 0usize;
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
        if !items
            .get(idx)
            .is_some_and(|item| Self::token_text_is(*item, source, "exporting"))
        {
            return signature;
        }
        idx += 1;
        while idx < items.len() {
            let item = items[idx];
            if Self::token_text_is(item, source, ".") {
                break;
            }
            if let Some((param, next_idx)) = Self::try_consume_parameter(items, idx, source) {
                signature.parameters.push(param);
                idx = next_idx;
                continue;
            }
            idx += 1;
        }
        signature
    }

    fn significant_children(self, source: &str) -> Vec<SyntaxNodeRef<'a>> {
        let _ = source;
        self.syntax.children().collect()
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

    fn try_consume_parameter(
        items: &[SyntaxNodeRef<'a>],
        idx: usize,
        source: &str,
    ) -> Option<(EventsStmtParameter<'a>, usize)> {
        let mut j = idx;
        while items
            .get(j)
            .is_some_and(|item| Self::is_punctuation(*item, source))
        {
            j += 1;
        }

        let (name, mut j) = MethodsStmt::parameter_name(items, j, source)?;
        while items
            .get(j)
            .is_some_and(|item| Self::is_punctuation(*item, source))
        {
            j += 1;
        }

        let type_clause = match items.get(j).copied() {
            Some(item) if Self::token_text_is(item, source, "type") => MethodsTypeClauseKind::Type,
            Some(item) if Self::token_text_is(item, source, "like") => MethodsTypeClauseKind::Like,
            _ => return None,
        };
        j += 1;

        let type_ref = items.get(j).copied().and_then(TypeRefSimple::cast);
        let next_idx = MethodsStmt::skip_type_expression(items, j, source);
        let type_display_range = MethodsStmt::token_span(&items[j..next_idx]);
        let is_optional = items.get(next_idx).is_some_and(|item| {
            Self::token_text_is(*item, source, "optional")
                || Self::token_text_is(*item, source, "default")
        });
        Some((
            EventsStmtParameter {
                name,
                type_clause,
                type_ref,
                type_display_range,
                is_optional,
            },
            next_idx,
        ))
    }
}

impl<'a> ClearOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> ClearStmt<'a> {
    pub fn operands(self) -> impl DoubleEndedIterator<Item = ClearOperand<'a>> + Clone + 'a {
        self.syntax.children().filter_map(ClearOperand::cast)
    }
}

impl<'a> ConvertOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> ConvertTargetOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> ConvertTimeZoneOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> ConvertDateTarget<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> ConvertTimeTarget<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> ConvertDaylightSavingTarget<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> ConvertStmt<'a> {
    pub fn operands(self) -> impl DoubleEndedIterator<Item = ConvertOperand<'a>> + Clone + 'a {
        self.syntax.children().filter_map(ConvertOperand::cast)
    }

    pub fn target(self) -> Option<ConvertTargetOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::ConvertTargetOperand)
            .and_then(ConvertTargetOperand::cast)
    }

    pub fn time_zone(self) -> Option<ConvertTimeZoneOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::ConvertTimeZoneOperand)
            .and_then(ConvertTimeZoneOperand::cast)
    }

    pub fn date_target(self) -> Option<ConvertDateTarget<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::ConvertDateTarget)
            .and_then(ConvertDateTarget::cast)
    }

    pub fn time_target(self) -> Option<ConvertTimeTarget<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::ConvertTimeTarget)
            .and_then(ConvertTimeTarget::cast)
    }

    pub fn daylight_saving_target(self) -> Option<ConvertDaylightSavingTarget<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::ConvertDaylightSavingTarget)
            .and_then(ConvertDaylightSavingTarget::cast)
    }
}

impl<'a> DescribeTableOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> DescribeLinesTarget<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> DescribeStmt<'a> {
    pub fn table_operands(
        self,
    ) -> impl DoubleEndedIterator<Item = DescribeTableOperand<'a>> + Clone + 'a {
        self.syntax
            .children()
            .filter_map(DescribeTableOperand::cast)
    }

    pub fn lines_targets(
        self,
    ) -> impl DoubleEndedIterator<Item = DescribeLinesTarget<'a>> + Clone + 'a {
        self.syntax.children().filter_map(DescribeLinesTarget::cast)
    }

    pub fn table_operand(self) -> Option<DescribeTableOperand<'a>> {
        self.table_operands().next()
    }

    pub fn lines_target(self) -> Option<DescribeLinesTarget<'a>> {
        self.lines_targets().next()
    }
}

impl<'a> ReplacePatternOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> ReplaceTargetOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> ReplaceWithOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> ReplaceStmt<'a> {
    pub fn patterns(
        self,
    ) -> impl DoubleEndedIterator<Item = ReplacePatternOperand<'a>> + Clone + 'a {
        self.syntax
            .children()
            .filter_map(ReplacePatternOperand::cast)
    }

    pub fn targets(self) -> impl DoubleEndedIterator<Item = ReplaceTargetOperand<'a>> + Clone + 'a {
        self.syntax
            .children()
            .filter_map(ReplaceTargetOperand::cast)
    }

    pub fn replacements(
        self,
    ) -> impl DoubleEndedIterator<Item = ReplaceWithOperand<'a>> + Clone + 'a {
        self.syntax.children().filter_map(ReplaceWithOperand::cast)
    }
}

impl<'a> WaitOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> WaitStmt<'a> {
    pub fn duration(self) -> Option<WaitOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::WaitOperand)
            .and_then(WaitOperand::cast)
    }
}

impl<'a> PerformStmt<'a> {
    pub fn tokens(self) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax.children_by_kind(SyntaxKind::Token)
    }

    pub fn routine_token(self) -> Option<SyntaxNodeRef<'a>> {
        let token = self.tokens().nth(1)?;
        matches!(token.token_kind(), Some(TokenKind::Ident)).then_some(token)
    }
}

impl<'a> CursorHandleOperand<'a> {
    pub fn tokens(self) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax.children_by_kind(SyntaxKind::Token)
    }
}

impl<'a> SubmitStmt<'a> {
    pub fn tokens(self) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax.children_by_kind(SyntaxKind::Token)
    }

    pub fn target(self) -> Option<SubmitTarget<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SubmitTarget)
            .and_then(SubmitTarget::cast)
    }

    pub fn target_token(self) -> Option<SyntaxNodeRef<'a>> {
        self.target()
            .and_then(|target| target.syntax.children_by_kind(SyntaxKind::Token).next())
            .or_else(|| self.tokens().nth(1))
    }

    pub fn selection_screen(self) -> Option<SubmitSelectionScreenOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SubmitSelectionScreenOperand)
            .and_then(SubmitSelectionScreenOperand::cast)
    }

    pub fn selection_set(self) -> Option<SubmitSelectionSetOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SubmitSelectionSetOperand)
            .and_then(SubmitSelectionSetOperand::cast)
    }

    pub fn selection_sets_program(self) -> Option<SubmitSelectionSetsProgramOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SubmitSelectionSetsProgramOperand)
            .and_then(SubmitSelectionSetsProgramOperand::cast)
    }

    pub fn selection_table(self) -> Option<SubmitSelectionTableOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SubmitSelectionTableOperand)
            .and_then(SubmitSelectionTableOperand::cast)
    }

    pub fn with_clauses(
        self,
    ) -> impl DoubleEndedIterator<Item = SubmitWithClause<'a>> + Clone + 'a {
        self.syntax.children().filter_map(SubmitWithClause::cast)
    }

    pub fn free_selections(self) -> Option<SubmitFreeSelectionsOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SubmitFreeSelectionsOperand)
            .and_then(SubmitFreeSelectionsOperand::cast)
    }

    pub fn line_size(self) -> Option<SubmitLineSizeOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SubmitLineSizeOperand)
            .and_then(SubmitLineSizeOperand::cast)
    }

    pub fn line_count(self) -> Option<SubmitLineCountOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SubmitLineCountOperand)
            .and_then(SubmitLineCountOperand::cast)
    }

    pub fn spool_parameters(self) -> Option<SubmitSpoolParametersOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SubmitSpoolParametersOperand)
            .and_then(SubmitSpoolParametersOperand::cast)
    }

    pub fn archive_parameters(self) -> Option<SubmitArchiveParametersOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SubmitArchiveParametersOperand)
            .and_then(SubmitArchiveParametersOperand::cast)
    }

    pub fn user(self) -> Option<SubmitUserOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SubmitUserOperand)
            .and_then(SubmitUserOperand::cast)
    }

    pub fn job(self) -> Option<SubmitJobOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SubmitJobOperand)
            .and_then(SubmitJobOperand::cast)
    }

    pub fn job_number(self) -> Option<SubmitJobNumberOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SubmitJobNumberOperand)
            .and_then(SubmitJobNumberOperand::cast)
    }

    pub fn language(self) -> Option<SubmitLanguageOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SubmitLanguageOperand)
            .and_then(SubmitLanguageOperand::cast)
    }
}

impl<'a> SubmitTarget<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> SubmitSelectionScreenOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> SubmitSelectionSetOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> SubmitSelectionSetsProgramOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> SubmitSelectionTableOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> SubmitWithClause<'a> {
    pub fn operands(self) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax.non_token_children()
    }
}

impl<'a> SubmitFreeSelectionsOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> SubmitLineSizeOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> SubmitLineCountOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> SubmitSpoolParametersOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> SubmitArchiveParametersOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> SubmitUserOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> SubmitJobOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> SubmitJobNumberOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> SubmitLanguageOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> RaiseStmt<'a> {
    pub fn exception_type_ref(self) -> Option<TypeRefSimple<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::TypeRefSimple)
            .and_then(TypeRefSimple::cast)
    }

    pub fn arg_list(self) -> Option<CallArgList<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::CallArgList)
            .and_then(CallArgList::cast)
    }

    pub fn trailing_children(self) -> Vec<SyntaxNodeRef<'a>> {
        let mut seen_type_ref = false;
        let mut trailing = Vec::new();
        for child in self.syntax.children() {
            if child.kind() == SyntaxKind::TypeRefSimple {
                seen_type_ref = true;
                continue;
            }
            if seen_type_ref {
                trailing.push(child);
            }
        }
        trailing
    }
}

impl<'a> CreateObjectStmt<'a> {
    pub fn target(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.non_token_children().find(|child| {
            child.kind() != SyntaxKind::TypeRefSimple && child.kind() != SyntaxKind::CallArgList
        })
    }

    pub fn type_ref(self) -> Option<TypeRefSimple<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::TypeRefSimple)
            .and_then(TypeRefSimple::cast)
    }

    pub fn arg_list(self) -> Option<CallArgList<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::CallArgList)
            .and_then(CallArgList::cast)
    }
}

impl<'a> CreateDataStmt<'a> {
    pub fn target(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.non_token_children().find(|child| {
            child.kind() != SyntaxKind::TypeRefSimple && child.kind() != SyntaxKind::CallArgList
        })
    }

    pub fn type_clause_kind(self, source: &str) -> Option<TypeClauseKind> {
        self.syntax
            .children_by_kind(SyntaxKind::Token)
            .find_map(|token| {
                let text = token.text(source)?;
                if text.eq_ignore_ascii_case("type") {
                    Some(TypeClauseKind::Type)
                } else if text.eq_ignore_ascii_case("like") {
                    Some(TypeClauseKind::Like)
                } else {
                    None
                }
            })
    }

    pub fn type_ref(self) -> Option<TypeRefSimple<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::TypeRefSimple)
            .and_then(TypeRefSimple::cast)
    }

    pub fn type_value(self, source: &str) -> Option<SyntaxNodeRef<'a>> {
        let mut saw_target = false;
        for child in self.syntax.non_token_children() {
            if !saw_target && Some(child.id()) == self.target().map(|target| target.id()) {
                saw_target = true;
                continue;
            }
            if self.type_clause_kind(source).is_some() {
                return Some(child);
            }
        }
        None
    }
}

impl<'a> CallMethodStmt<'a> {
    pub fn target(self) -> Option<CallMethodTarget<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::CallMethodTarget)
            .and_then(CallMethodTarget::cast)
    }

    pub fn arg_list(self) -> Option<CallArgList<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::CallArgList)
            .and_then(CallArgList::cast)
    }
}

impl<'a> CallMethodTarget<'a> {
    pub fn callee(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> CallStmt<'a> {
    pub fn call_kind(self, source: &str) -> Option<CallStmtKind> {
        let mut tokens = self.syntax.children_by_kind(SyntaxKind::Token);
        let first = tokens.next()?;
        if !first.text(source)?.eq_ignore_ascii_case("call") {
            return None;
        }
        let token = tokens.next()?;
        let text = token.text(source)?;
        if text.eq_ignore_ascii_case("function") {
            Some(CallStmtKind::Function)
        } else if text.eq_ignore_ascii_case("transformation") {
            Some(CallStmtKind::Transformation)
        } else if text.eq_ignore_ascii_case("badi") {
            Some(CallStmtKind::Badi)
        } else if text.eq_ignore_ascii_case("screen") {
            Some(CallStmtKind::Screen)
        } else if text.eq_ignore_ascii_case("transaction") {
            Some(CallStmtKind::Transaction)
        } else {
            Some(CallStmtKind::SystemFunction)
        }
    }

    pub fn system_function_callee(self) -> Option<SyntaxNodeRef<'a>> {
        if self.syntax.children_by_kind(SyntaxKind::Token).count() == 2 {
            return self.syntax.first_non_token_child();
        }
        let call_token = self.syntax.children_by_kind(SyntaxKind::Token).next()?;
        let call_end = call_token.range().end;
        self.syntax.non_token_children().find(|child| {
            child.range().start >= call_end && child.kind() != SyntaxKind::CallArgList
        })
    }

    pub fn callee_token(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.children_by_kind(SyntaxKind::Token).nth(2)
    }

    pub fn direct_call(self) -> Option<CallExpr<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::CallExpr)
            .and_then(CallExpr::cast)
    }

    pub fn arg_list(self) -> Option<CallArgList<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::CallArgList)
            .and_then(CallArgList::cast)
    }
}

impl<'a> InterfacesStmt<'a> {
    pub fn type_refs(&self) -> impl DoubleEndedIterator<Item = TypeRefSimple<'a>> + Clone + 'a {
        self.syntax.children().filter_map(TypeRefSimple::cast)
    }
}

impl<'a> AliasesStmt<'a> {
    pub fn entries(self) -> impl DoubleEndedIterator<Item = AliasEntry<'a>> + Clone + 'a {
        self.syntax.children().filter_map(AliasEntry::cast)
    }
}

impl<'a> AliasEntry<'a> {
    pub fn alias_name(self) -> Option<AliasName<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::AliasName)
            .and_then(AliasName::cast)
    }

    pub fn target_interface(self) -> Option<TypeRefSimple<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::TypeRefSimple)
            .and_then(TypeRefSimple::cast)
    }

    pub fn target_member(self) -> Option<AliasMember<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::AliasMember)
            .and_then(AliasMember::cast)
    }
}

impl<'a> MessageStmt<'a> {
    pub fn head_clause(self) -> Option<MessageHeadClause<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::MessageHeadClause)
            .and_then(MessageHeadClause::cast)
    }

    pub fn with_clause(self) -> Option<MessageWithClause<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::MessageWithClause)
            .and_then(MessageWithClause::cast)
    }

    pub fn into_clause(self) -> Option<MessageIntoClause<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::MessageIntoClause)
            .and_then(MessageIntoClause::cast)
    }

    pub fn display_like_clause(self) -> Option<MessageDisplayLikeClause<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::MessageDisplayLikeClause)
            .and_then(MessageDisplayLikeClause::cast)
    }

    pub fn raising_clause(self) -> Option<MessageRaisingClause<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::MessageRaisingClause)
            .and_then(MessageRaisingClause::cast)
    }
}

impl<'a> MessageHeadClause<'a> {
    pub fn operands(self) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax.non_token_children()
    }
}

impl<'a> MessageWithClause<'a> {
    pub fn operands(self) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax.non_token_children()
    }
}

impl<'a> FindStmt<'a> {
    pub fn pattern(self) -> Option<FindPatternOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::FindPatternOperand)
            .and_then(FindPatternOperand::cast)
    }

    pub fn target(self) -> Option<FindInOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::FindInOperand)
            .and_then(FindInOperand::cast)
    }

    pub fn match_targets(
        self,
    ) -> impl DoubleEndedIterator<Item = FindMatchTarget<'a>> + Clone + 'a {
        self.syntax.children().filter_map(FindMatchTarget::cast)
    }

    pub fn submatch_targets(
        self,
    ) -> impl DoubleEndedIterator<Item = FindSubmatchTarget<'a>> + Clone + 'a {
        self.syntax.children().filter_map(FindSubmatchTarget::cast)
    }

    pub fn results_targets(
        self,
    ) -> impl DoubleEndedIterator<Item = FindResultsTarget<'a>> + Clone + 'a {
        self.syntax.children().filter_map(FindResultsTarget::cast)
    }
}

impl<'a> FindPatternOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> FindInOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> FindMatchTarget<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> FindSubmatchTarget<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> FindResultsTarget<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> ReadTableStmt<'a> {
    pub fn operands(self) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax.non_token_children()
    }

    pub fn source(self) -> Option<SyntaxNodeRef<'a>> {
        self.operands().find(|child| {
            !matches!(
                child.kind(),
                SyntaxKind::DataInlineDecl | SyntaxKind::FieldSymbolInlineDecl
            )
        })
    }

    pub fn data_inline_targets(
        self,
    ) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax.children_by_kind(SyntaxKind::DataInlineDecl)
    }

    pub fn field_symbol_inline_targets(
        self,
    ) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax
            .children_by_kind(SyntaxKind::FieldSymbolInlineDecl)
    }
}

impl<'a> AuthorityCheckStmt<'a> {
    pub fn object(self) -> Option<AuthorityCheckObjectOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::AuthorityCheckObjectOperand)
            .and_then(AuthorityCheckObjectOperand::cast)
    }

    pub fn user(self) -> Option<AuthorityCheckUserOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::AuthorityCheckUserOperand)
            .and_then(AuthorityCheckUserOperand::cast)
    }

    pub fn id_clauses(
        self,
    ) -> impl DoubleEndedIterator<Item = AuthorityCheckIdClause<'a>> + Clone + 'a {
        self.syntax
            .children()
            .filter_map(AuthorityCheckIdClause::cast)
    }
}

impl<'a> AuthorityCheckObjectOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> AuthorityCheckUserOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> AuthorityCheckIdClause<'a> {
    pub fn id(self) -> Option<AuthorityCheckIdOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::AuthorityCheckIdOperand)
            .and_then(AuthorityCheckIdOperand::cast)
    }

    pub fn field(self) -> Option<AuthorityCheckFieldOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::AuthorityCheckFieldOperand)
            .and_then(AuthorityCheckFieldOperand::cast)
    }
}

impl<'a> AuthorityCheckIdOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> AuthorityCheckFieldOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> WriteStmt<'a> {
    pub fn operands(self) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax.non_token_children()
    }
}

impl<'a> SplitSourceOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> SplitSeparatorOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> SplitTargetOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> SplitStmt<'a> {
    pub fn source(self) -> Option<SplitSourceOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SplitSourceOperand)
            .and_then(SplitSourceOperand::cast)
    }

    pub fn separator(self) -> Option<SplitSeparatorOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SplitSeparatorOperand)
            .and_then(SplitSeparatorOperand::cast)
    }

    pub fn targets(self) -> impl DoubleEndedIterator<Item = SplitTargetOperand<'a>> + Clone + 'a {
        self.syntax.children().filter_map(SplitTargetOperand::cast)
    }
}

impl<'a> ConcatenateSourceOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> ConcatenateTargetOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> ConcatenateSeparatorOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> ConcatenateStmt<'a> {
    pub fn sources(
        self,
    ) -> impl DoubleEndedIterator<Item = ConcatenateSourceOperand<'a>> + Clone + 'a {
        self.syntax
            .children()
            .filter_map(ConcatenateSourceOperand::cast)
    }

    pub fn target(self) -> Option<ConcatenateTargetOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::ConcatenateTargetOperand)
            .and_then(ConcatenateTargetOperand::cast)
    }

    pub fn separator(self) -> Option<ConcatenateSeparatorOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::ConcatenateSeparatorOperand)
            .and_then(ConcatenateSeparatorOperand::cast)
    }
}

impl<'a> UpdateSetValueOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> UpdateSetAssignment<'a> {
    pub fn value(self) -> Option<UpdateSetValueOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::UpdateSetValueOperand)
            .and_then(UpdateSetValueOperand::cast)
    }
}

impl<'a> UpdateFromOperand<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> UpdateWhereClause<'a> {
    pub fn value(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }
}

impl<'a> DeleteStmt<'a> {
    pub fn source(self) -> Option<SyntaxNodeRef<'a>> {
        self.syntax.first_non_token_child()
    }

    pub fn where_expr(self, source: &str) -> Option<SyntaxNodeRef<'a>> {
        let mut saw_where = false;
        for child in self.syntax.children() {
            if child.kind() == SyntaxKind::Token
                && child
                    .text(source)
                    .is_some_and(|text| text.eq_ignore_ascii_case("where"))
            {
                saw_where = true;
                continue;
            }
            if saw_where && child.kind() != SyntaxKind::Token {
                return Some(child);
            }
        }
        None
    }

    pub fn comparing_operands(self, source: &str) -> Vec<SyntaxNodeRef<'a>> {
        let mut saw_comparing = false;
        let mut operands = Vec::new();
        for child in self.syntax.children() {
            if child.kind() == SyntaxKind::Token
                && child
                    .text(source)
                    .is_some_and(|text| text.eq_ignore_ascii_case("comparing"))
            {
                saw_comparing = true;
                continue;
            }
            if saw_comparing && child.kind() != SyntaxKind::Token {
                operands.push(child);
            }
        }
        operands
    }
}

impl<'a> SortStmt<'a> {
    pub fn source(self, source: &str) -> Option<SyntaxNodeRef<'a>> {
        for child in self.syntax.children() {
            if child.kind() == SyntaxKind::Token
                && child
                    .text(source)
                    .is_some_and(|text| text.eq_ignore_ascii_case("by"))
            {
                break;
            }
            if child.kind() != SyntaxKind::Token {
                return Some(child);
            }
        }
        None
    }

    pub fn by_operands(self, source: &str) -> Vec<SyntaxNodeRef<'a>> {
        let mut saw_by = false;
        let mut operands = Vec::new();
        for child in self.syntax.children() {
            if child.kind() == SyntaxKind::Token
                && child
                    .text(source)
                    .is_some_and(|text| text.eq_ignore_ascii_case("by"))
            {
                saw_by = true;
                continue;
            }
            if saw_by && child.kind() != SyntaxKind::Token {
                operands.push(child);
            }
        }
        operands
    }
}

impl<'a> UpdateStmt<'a> {
    pub fn target(self) -> Option<UpdateTarget<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::UpdateTarget)
            .and_then(UpdateTarget::cast)
    }

    pub fn set_clause(self) -> Option<UpdateSetClause<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::UpdateSetClause)
            .and_then(UpdateSetClause::cast)
    }

    pub fn from_operand(self) -> Option<UpdateFromOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::UpdateFromOperand)
            .and_then(UpdateFromOperand::cast)
    }

    pub fn where_clause(self) -> Option<UpdateWhereClause<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::UpdateWhereClause)
            .and_then(UpdateWhereClause::cast)
    }
}

impl<'a> UpdateSetClause<'a> {
    pub fn assignments(
        self,
    ) -> impl DoubleEndedIterator<Item = UpdateSetAssignment<'a>> + Clone + 'a {
        self.syntax.children().filter_map(UpdateSetAssignment::cast)
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

impl<'a> OpenCursorStmt<'a> {
    pub fn handle(&self) -> Option<CursorHandleOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::CursorHandleOperand)
            .and_then(CursorHandleOperand::cast)
    }

    pub fn query(&self) -> Option<SelectQuery<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SelectQuery)
            .and_then(SelectQuery::cast)
    }
}

impl<'a> FetchCursorStmt<'a> {
    pub fn handle(&self) -> Option<CursorHandleOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::CursorHandleOperand)
            .and_then(CursorHandleOperand::cast)
    }
}

impl<'a> CloseCursorStmt<'a> {
    pub fn handle(&self) -> Option<CursorHandleOperand<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::CursorHandleOperand)
            .and_then(CursorHandleOperand::cast)
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

impl<'a> SelectProjectionList<'a> {
    pub fn items(self) -> impl DoubleEndedIterator<Item = SqlProjectionItem<'a>> + Clone + 'a {
        self.syntax.children().filter_map(SqlProjectionItem::cast)
    }
}

impl<'a> SelectFromClause<'a> {
    pub fn items(self) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax.non_token_children()
    }
}

impl<'a> SelectIntoClause<'a> {
    pub fn token_texts(self, source: &'a str) -> Vec<&'a str> {
        self.syntax
            .token_descendants()
            .into_iter()
            .filter_map(|token| token.text(source))
            .collect()
    }

    pub fn has_keyword(self, source: &str, keyword: &str) -> bool {
        self.syntax
            .token_descendants()
            .into_iter()
            .filter_map(|token| token.text(source))
            .any(|text| text.eq_ignore_ascii_case(keyword))
    }

    pub fn target_children(
        self,
    ) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax.non_token_children()
    }
}

impl<'a> SelectJoinClause<'a> {
    pub fn parts(self) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax.children()
    }

    pub fn join_kind_text(self, source: &'a str) -> Option<&'a str> {
        let mut start = None;
        let mut end = None;
        for child in self.syntax.children() {
            if child.kind() == SyntaxKind::SqlDataSource {
                break;
            }
            if child.kind() != SyntaxKind::Token {
                continue;
            }
            let range = child.range();
            start.get_or_insert(range.start);
            end = Some(range.end);
        }
        let start = start?;
        let end = end?;
        let range = start..end;
        let text = source.get(range)?.trim();
        (!text.is_empty()).then_some(text)
    }

    pub fn data_source(self) -> Option<SqlDataSource<'a>> {
        self.syntax.children().find_map(SqlDataSource::cast)
    }

    pub fn predicate(self) -> Option<SqlPredicateExpr<'a>> {
        self.syntax.children().find_map(SqlPredicateExpr::cast)
    }
}

impl<'a> SqlPredicateExpr<'a> {
    pub fn operands(self) -> impl DoubleEndedIterator<Item = SqlPredicateOperand<'a>> + Clone + 'a {
        self.syntax.children().filter_map(SqlPredicateOperand::cast)
    }

    pub fn token_texts(self, source: &'a str) -> Vec<&'a str> {
        self.syntax
            .token_descendants()
            .into_iter()
            .filter_map(|token| token.text(source))
            .collect()
    }
}

impl<'a> SqlPredicateOperand<'a> {
    pub fn non_token_children(
        self,
    ) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax.non_token_children()
    }
}

impl<'a> SqlProjectionItem<'a> {
    pub fn alias_clause(self) -> Option<SqlAliasClause<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SqlAliasClause)
            .and_then(SqlAliasClause::cast)
    }

    pub fn alias(self) -> Option<SqlAlias<'a>> {
        self.alias_clause().and_then(|clause| clause.alias())
    }

    pub fn non_token_children(
        self,
    ) -> impl DoubleEndedIterator<Item = SyntaxNodeRef<'a>> + Clone + 'a {
        self.syntax.non_token_children()
    }

    pub fn token_nodes(self) -> Vec<SyntaxNodeRef<'a>> {
        self.syntax.token_descendants()
    }
}

impl<'a> SqlDataSource<'a> {
    pub fn alias_clause(self) -> Option<SqlAliasClause<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SqlAliasClause)
            .and_then(SqlAliasClause::cast)
    }

    pub fn alias(self) -> Option<SqlAlias<'a>> {
        self.alias_clause().and_then(|clause| clause.alias())
    }

    pub fn source_name(self, source: &'a str) -> Option<(&'a str, TextRange)> {
        let alias_clause_range = self.alias_clause().map(|alias| alias.syntax().range());
        let tokens = self.syntax.token_descendants();
        let token_texts = tokens
            .iter()
            .map(|token| token.text(source))
            .collect::<Option<Vec<_>>>()?;
        let mut start = None;
        let mut end = None;
        let mut paren = 0i32;
        let mut bracket = 0i32;
        let mut brace = 0i32;
        for (idx, token) in tokens.into_iter().enumerate() {
            let range = token.range();
            if alias_clause_range
                .as_ref()
                .is_some_and(|alias| range.start >= alias.start)
            {
                break;
            }
            let text = token_texts[idx];
            let is_source_modifier = paren == 0
                && bracket == 0
                && brace == 0
                && ((text.eq_ignore_ascii_case("with")
                    && token_texts
                        .get(idx + 1)
                        .is_some_and(|next| next.eq_ignore_ascii_case("privileged")))
                    || (text.eq_ignore_ascii_case("client")
                        && token_texts
                            .get(idx + 1)
                            .is_some_and(|next| next.eq_ignore_ascii_case("specified"))));
            if text.eq_ignore_ascii_case("as") || is_source_modifier {
                break;
            }
            start.get_or_insert(range.start);
            end = Some(range.end);
            match text {
                "(" => paren += 1,
                ")" => paren -= 1,
                "[" => bracket += 1,
                "]" => bracket -= 1,
                "{" => brace += 1,
                "}" => brace -= 1,
                _ => {}
            }
        }
        let start = start?;
        let end = end?;
        let range = start..end;
        let text = source.get(range.clone())?.trim();
        (!text.is_empty()).then_some((text, range))
    }
}

impl<'a> SqlAliasClause<'a> {
    pub fn alias(self) -> Option<SqlAlias<'a>> {
        self.syntax
            .child_by_kind(SyntaxKind::SqlAlias)
            .and_then(SqlAlias::cast)
    }
}

impl<'a> SqlColumnRef<'a> {
    pub fn parts(self, source: &'a str) -> Option<(Option<Arc<str>>, Arc<str>, TextRange)> {
        let tokens = self.syntax.token_descendants();
        if tokens.len() == 1 {
            let text = tokens[0].text(source)?;
            return Some((
                None,
                Arc::from(text.to_ascii_lowercase()),
                tokens[0].range(),
            ));
        }
        if tokens.len() == 3 {
            let qualifier = tokens[0].text(source)?;
            let sep = tokens[1].text(source)?;
            let column = tokens[2].text(source)?;
            if sep == "~" {
                return Some((
                    Some(Arc::from(qualifier.to_ascii_lowercase())),
                    Arc::from(column.to_ascii_lowercase()),
                    tokens[0].range().start..tokens[2].range().end,
                ));
            }
        }
        None
    }
}

impl<'a> SqlQualifiedColumnRef<'a> {
    pub fn parts(self, source: &'a str) -> Option<(Arc<str>, Arc<str>, TextRange)> {
        let tokens = self.syntax.token_descendants();
        if tokens.len() != 3 {
            return None;
        }
        let qualifier = tokens[0].text(source)?;
        let sep = tokens[1].text(source)?;
        let column = tokens[2].text(source)?;
        (sep == "~").then(|| {
            (
                Arc::from(qualifier.to_ascii_lowercase()),
                Arc::from(column.to_ascii_lowercase()),
                tokens[0].range().start..tokens[2].range().end,
            )
        })
    }
}

impl<'a> SqlQualifiedStar<'a> {
    pub fn qualifier(self, source: &'a str) -> Option<(Arc<str>, TextRange)> {
        let tokens = self.syntax.token_descendants();
        if tokens.len() != 3 {
            return None;
        }
        let qualifier = tokens[0].text(source)?;
        let sep = tokens[1].text(source)?;
        let star = tokens[2].text(source)?;
        (sep == "~" && star == "*").then(|| {
            (
                Arc::from(qualifier.to_ascii_lowercase()),
                tokens[0].range().start..tokens[2].range().end,
            )
        })
    }
}

impl<'a> SqlAggregateCall<'a> {
    pub fn name(self, source: &'a str) -> Option<(Arc<str>, TextRange)> {
        let token = self
            .syntax
            .children()
            .find(|child| child.kind() == SyntaxKind::Token)?;
        let text = token.text(source)?;
        Some((Arc::from(text.to_ascii_lowercase()), token.range()))
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::SyntaxKind;
    use crate::arena::SyntaxTreeBuilder;

    use super::{
        AstNode, CallArgList, CallExpr, DataDecl, DataDeclName, DataLikeDecl, ExprIdent,
        MethodsParamSectionKind, MethodsRaiseKind, MethodsStmt, MethodsStmtKind,
        MethodsTypeClauseKind, SelectProjectionList, SelectStmt, SelectorExpr, SqlAliasClause,
        SqlDataSource, SqlQualifiedColumnRef, SqlQualifiedStar, SyntaxNodeRef,
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
    fn sql_wrappers_expose_projection_and_source_parts() {
        let mut b = SyntaxTreeBuilder::default();
        let col_a = b.leaf(SyntaxKind::Token, 7..8);
        let tilde = b.leaf(SyntaxKind::Token, 8..9);
        let col_b = b.leaf(SyntaxKind::Token, 9..14);
        let column = b.branch(
            SyntaxKind::SqlQualifiedColumnRef,
            7..14,
            &[col_a, tilde, col_b],
        );
        let as_proj_tok = b.leaf(SyntaxKind::Token, 15..17);
        let alias_tok = b.leaf(SyntaxKind::Token, 18..20);
        let alias = b.branch(SyntaxKind::SqlAlias, 18..20, &[alias_tok]);
        let alias_clause = b.branch(SyntaxKind::SqlAliasClause, 15..20, &[as_proj_tok, alias]);
        let item = b.branch(
            SyntaxKind::SqlProjectionItem,
            7..20,
            &[column, alias_clause],
        );
        let proj = b.branch(SyntaxKind::SelectProjectionList, 7..20, &[item]);
        let tab_tok = b.leaf(SyntaxKind::Token, 26..30);
        let as_tok = b.leaf(SyntaxKind::Token, 31..33);
        let ds_alias_tok = b.leaf(SyntaxKind::Token, 34..35);
        let ds_alias = b.branch(SyntaxKind::SqlAlias, 34..35, &[ds_alias_tok]);
        let ds_alias_clause = b.branch(SyntaxKind::SqlAliasClause, 31..35, &[as_tok, ds_alias]);
        let source = b.branch(
            SyntaxKind::SqlDataSource,
            26..35,
            &[tab_tok, ds_alias_clause],
        );
        let from = b.branch(SyntaxKind::SelectFromClause, 21..35, &[source]);
        let qstar_q = b.leaf(SyntaxKind::Token, 36..37);
        let qstar_sep = b.leaf(SyntaxKind::Token, 37..38);
        let qstar_star = b.leaf(SyntaxKind::Token, 38..39);
        let qstar = b.branch(
            SyntaxKind::SqlQualifiedStar,
            36..39,
            &[qstar_q, qstar_sep, qstar_star],
        );
        let query = b.branch(SyntaxKind::SelectQuery, 0..39, &[proj, from, qstar]);
        let tree = b.finish(query);
        let src = "SELECT a~field AS al FROM mara AS m a~*";

        let proj = SelectProjectionList::cast(SyntaxNodeRef::new(&tree, proj)).expect("proj");
        let item = proj.items().next().expect("item");
        let column = item
            .non_token_children()
            .find_map(SqlQualifiedColumnRef::cast)
            .expect("column");
        assert_eq!(
            column.parts(src).map(|(q, c, _)| (q, c)),
            Some((Arc::from("a"), Arc::from("field")))
        );
        let alias_clause = item.alias_clause().expect("projection alias clause");
        assert_eq!(
            alias_clause
                .alias()
                .and_then(|alias| alias.syntax().lower_trimmed_text(src)),
            Some(Arc::from("al"))
        );

        let source = SqlDataSource::cast(SyntaxNodeRef::new(&tree, source)).expect("source");
        assert_eq!(source.source_name(src).map(|(name, _)| name), Some("mara"));
        assert!(
            source
                .alias_clause()
                .and_then(SqlAliasClause::alias)
                .is_some()
        );

        let qstar = SqlQualifiedStar::cast(SyntaxNodeRef::new(&tree, qstar)).expect("qstar");
        assert_eq!(qstar.qualifier(src).map(|(q, _)| q), Some(Arc::from("a")));
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

    #[test]
    fn methods_stmt_parameter_type_display_stops_before_bang_prefixed_next_parameter() {
        let source = "\
CLASS-METHODS read_char_value
  IMPORTING
    !ip_product TYPE ANY
    !ip_charact TYPE ANY
  EXPORTING
    !ep_value TYPE ANY.";
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
        let (name_tok, _) = take("read_char_value", &mut b);
        let (importing_tok, _) = take("IMPORTING", &mut b);
        let (bang_1_tok, _) = take("!", &mut b);
        let (ip_product_tok, _) = take("ip_product", &mut b);
        let (type_1_tok, _) = take("TYPE", &mut b);
        let (any_1_tok, any_1_range) = take("ANY", &mut b);
        let (bang_2_tok, bang_2_range) = take("!", &mut b);
        let any_1_type = b.branch(
            SyntaxKind::TypeRefSimple,
            any_1_range.start..bang_2_range.end,
            &[any_1_tok],
        );
        let (ip_charact_tok, _) = take("ip_charact", &mut b);
        let (type_2_tok, _) = take("TYPE", &mut b);
        let (any_2_tok, any_2_range) = take("ANY", &mut b);
        let any_2_type = b.branch(SyntaxKind::TypeRefSimple, any_2_range.clone(), &[any_2_tok]);
        let (exporting_tok, _) = take("EXPORTING", &mut b);
        let (bang_3_tok, _) = take("!", &mut b);
        let (ep_value_tok, _) = take("ep_value", &mut b);
        let (type_3_tok, _) = take("TYPE", &mut b);
        let (any_3_tok, any_3_range) = take("ANY", &mut b);
        let any_3_type = b.branch(SyntaxKind::TypeRefSimple, any_3_range.clone(), &[any_3_tok]);
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
                bang_1_tok,
                ip_product_tok,
                type_1_tok,
                any_1_type,
                bang_2_tok,
                ip_charact_tok,
                type_2_tok,
                any_2_type,
                exporting_tok,
                bang_3_tok,
                ep_value_tok,
                type_3_tok,
                any_3_type,
                period_tok,
            ],
        );
        let tree = b.finish(methods_stmt);
        let methods_stmt =
            MethodsStmt::cast(SyntaxNodeRef::new(&tree, methods_stmt)).expect("methods stmt");
        let signature = methods_stmt.signature(source);

        assert_eq!(signature.parameters().len(), 3);
        assert_eq!(
            signature.parameters()[0].type_display_text(source),
            Some("ANY")
        );
        assert_eq!(
            signature.parameters()[1].type_display_text(source),
            Some("ANY")
        );
        assert_eq!(
            signature.parameters()[2].type_display_text(source),
            Some("ANY")
        );
    }

    #[test]
    fn methods_stmt_wrappers_parse_raising_and_resumable_exceptions() {
        let source = "METHODS run RAISING resumable(/sttp/cx_demo) cx_other.";
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

        let (methods_tok, methods_range) = take("METHODS", &mut b);
        let (name_tok, _) = take("run", &mut b);
        let (raising_tok, _) = take("RAISING", &mut b);
        let (resumable_tok, _) = take("resumable", &mut b);
        let (lparen_tok, _) = take("(", &mut b);
        let (cx_demo_tok, cx_demo_range) = take("/sttp/cx_demo", &mut b);
        let cx_demo_type = b.branch(
            SyntaxKind::TypeRefSimple,
            cx_demo_range.clone(),
            &[cx_demo_tok],
        );
        let (rparen_tok, _) = take(")", &mut b);
        let (cx_other_tok, cx_other_range) = take("cx_other", &mut b);
        let cx_other_type = b.branch(
            SyntaxKind::TypeRefSimple,
            cx_other_range.clone(),
            &[cx_other_tok],
        );
        let (period_tok, period_range) = take(".", &mut b);

        let methods_stmt = b.branch(
            SyntaxKind::MethodsStmt,
            methods_range.start..period_range.end,
            &[
                methods_tok,
                name_tok,
                raising_tok,
                resumable_tok,
                lparen_tok,
                cx_demo_type,
                rparen_tok,
                cx_other_type,
                period_tok,
            ],
        );
        let tree = b.finish(methods_stmt);
        let methods_stmt =
            MethodsStmt::cast(SyntaxNodeRef::new(&tree, methods_stmt)).expect("methods stmt");

        let signature = methods_stmt.signature(source);
        assert!(signature.parameters().is_empty());
        assert_eq!(signature.raising().len(), 2);
        assert_eq!(signature.raising()[0].kind(), MethodsRaiseKind::Resumable);
        assert_eq!(
            signature.raising()[0].type_ref().display_text(source),
            Some("/sttp/cx_demo")
        );
        assert_eq!(signature.raising()[1].kind(), MethodsRaiseKind::Raising);
        assert_eq!(
            signature.raising()[1].type_ref().display_text(source),
            Some("cx_other")
        );
    }

    #[test]
    fn methods_stmt_wrappers_split_chained_entries() {
        let source = "METHODS: get_response IMPORTING iv_x TYPE i, get_data.";
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

        let (methods_tok, methods_range) = take("METHODS", &mut b);
        let (colon_tok, _) = take(":", &mut b);
        let (get_response_tok, _) = take("get_response", &mut b);
        let (importing_tok, _) = take("IMPORTING", &mut b);
        let (iv_x_tok, _) = take("iv_x", &mut b);
        let (type_tok, _) = take("TYPE", &mut b);
        let (i_tok, i_range) = take("i", &mut b);
        let i_type = b.branch(SyntaxKind::TypeRefSimple, i_range.clone(), &[i_tok]);
        let (comma_tok, _) = take(",", &mut b);
        let (get_data_tok, _) = take("get_data", &mut b);
        let (period_tok, period_range) = take(".", &mut b);

        let methods_stmt = b.branch(
            SyntaxKind::MethodsStmt,
            methods_range.start..period_range.end,
            &[
                methods_tok,
                colon_tok,
                get_response_tok,
                importing_tok,
                iv_x_tok,
                type_tok,
                i_type,
                comma_tok,
                get_data_tok,
                period_tok,
            ],
        );
        let tree = b.finish(methods_stmt);
        let methods_stmt =
            MethodsStmt::cast(SyntaxNodeRef::new(&tree, methods_stmt)).expect("methods stmt");

        let entries = methods_stmt.entries(source);
        assert_eq!(entries.len(), 2);
        assert_eq!(
            entries[0]
                .name_token(source)
                .and_then(|token| token.text(source))
                .as_deref(),
            Some("get_response")
        );
        assert_eq!(
            entries[0].signature_text(source),
            "METHODS get_response IMPORTING iv_x TYPE i"
        );
        assert_eq!(entries[0].signature(source).parameters().len(), 1);
        assert_eq!(
            entries[1]
                .name_token(source)
                .and_then(|token| token.text(source))
                .as_deref(),
            Some("get_data")
        );
        assert_eq!(entries[1].signature_text(source), "METHODS get_data");
        assert!(entries[1].signature(source).parameters().is_empty());
    }
}
