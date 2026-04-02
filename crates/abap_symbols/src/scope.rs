use abap_lexer::TextRange;

use crate::ids::{ScopeId, SymbolId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Namespace {
    Value,
    Type,
    Routine,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    File,
    Form,
    Module,
    EventBlock,
    Class,
    Interface,
    Method,
    IfBranch,
    ElseifBranch,
    ElseBranch,
    WhenBranch,
    CatchClause,
    CleanupClause,
    WhileBlock,
    DoBlock,
    LoopBlock,
    TryBlock,
    SelectBlock,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScopeData {
    pub id: ScopeId,
    pub kind: ScopeKind,
    pub range: TextRange,
    pub parent: Option<ScopeId>,
    pub owner: Option<SymbolId>,
    pub declarations: Vec<SymbolId>,
    pub children: Vec<ScopeId>,
}
