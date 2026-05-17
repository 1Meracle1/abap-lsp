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
    Signature,
    IfBranch,
    ElseifBranch,
    ElseBranch,
    WhenBranch,
    CatchClause,
    CleanupClause,
    WhileBlock,
    DoBlock,
    LoopBlock,
    AtBlock,
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
    /// For [`ScopeKind::LoopBlock`]: `LOOP AT` source rows are scalar-like (elementary, unresolved, or
    /// one-field structure), so `table_line` resolves in this loop. ABAP uses the same pseudo-field
    /// in other internal-table contexts (e.g. `READ TABLE`, `FOR ALL ENTRIES`); those are not driven
    /// by this flag.
    pub allows_internal_table_line_selector: bool,
}
