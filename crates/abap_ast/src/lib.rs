//! ABAP syntax tree types.
//!
//! ## Representations
//!
//! - **[`SyntaxNode`]** — recursive, owned tree (`Vec` of children per node); useful for tests and
//!   for [`arena::SyntaxTree::from_nested`].
//! - **[`SyntaxTree`](arena::SyntaxTree)** — flat arena (see [`arena`]): two buffers (node headers +
//!   child ids). The workspace parser builds this via [`arena::SyntaxTreeBuilder`].
//!
//! For IDE-style incremental parsing, ecosystems often adopt **rowan** (immutable green trees +
//! cheap snapshots) or **salsa**-backed graphs; this crate stays dependency-free and keeps those as
//! optional future directions.

use abap_lexer::TextRange;

pub mod arena;
pub mod ast;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyntaxKind {
    File,
    Token,
    Error,
    /// `|…|` character string template (ABAP 7.40+).
    CharStringTemplate,
    /// Literal fragment inside a string template (`StringTemplateLit` token).
    TemplateLiteral,
    /// `{ expression [ formatting ]* }` inside a string template.
    TemplateInterpolation,
    /// Expression before optional `WIDTH =` / `DECIMALS =` / … clauses.
    TemplateExpr,
    /// Single formatting option, e.g. `DECIMALS = 2` or `ALPHA = IN`.
    TemplateFormatSpec,
    /// Binary operator expression (`+`, `-`, `*`, `/`, `MOD`, `DIV`, `&`).
    BinaryExpr,
    /// Unary `+` or `-`.
    UnaryExpr,
    /// Parenthesized grouping `( expr )`.
    ParenExpr,
    /// Identifier operand in an expression.
    ExprIdent,
    /// Numeric or character literal operand in an expression.
    ExprLiteral,
    /// Selector chain such as `obj->field` or `cls=>member`.
    SelectorExpr,
    /// ABAP offset/length access such as `text+2(8)` or `text(14)`.
    SubstringExpr,
    /// Call-style postfix `callee( ... )`.
    CallExpr,
    /// Parenthesized actual-parameter list of a call/constructor.
    CallArgList,
    /// Named actual-parameter section such as `EXPORTING`.
    CallArgSection,
    /// Named actual parameter such as `iv_name = expr`.
    CallNamedArg,
    /// Positional actual parameter segment.
    CallPositionalArg,
    /// Constructor-style expression such as `NEW ty( ... )`.
    ConstructorExpr,
    /// `DATA [:`] … .` — classic typed declaration (here: explicit `TYPE` with simple type refs).
    DataDecl,
    /// One `name TYPE type_ref` member (comma-separated under `DATA:` or alone after `DATA`).
    DataTypedClause,
    /// `DATA(name) = expr .`
    DataInlineDecl,
    /// `STATICS ... .`
    StaticsDecl,
    /// `TYPES ... .`
    TypesDecl,
    /// One `name TYPE type_ref` member under `TYPES`.
    TypesTypedClause,
    /// `BEGIN OF ... END OF ...` declaration body nested inside a declaration clause.
    StructuredDecl,
    /// One component inside a structured declaration.
    StructuredFieldClause,
    /// `INCLUDE TYPE ...` component inside a structured declaration.
    StructuredIncludeClause,
    /// `CONSTANTS ... .`
    ConstantsDecl,
    /// One constant member with optional `VALUE`.
    ConstantClause,
    /// `FIELD-SYMBOLS ... .`
    FieldSymbolsDecl,
    /// One field-symbol member with `TYPE`/`LIKE`.
    FieldSymbolClause,
    /// Name in a typed `DATA` clause (`screen0100-serial`, …); minus chains only.
    DataDeclName,
    /// Type-like reference in declaration position (`i`, `LIKE LINE OF itab`, table types, `obj=>class` …).
    TypeRefSimple,
    /// Base identifier inside a structured type reference.
    TypeRefName,
    /// Selector chain inside a structured type reference (`zif_demo=>ty_row`, `itab-line`, ...).
    TypeRefSelectorChain,
    /// `VALUE expr` in a declaration.
    ValueClause,
    /// `LENGTH ...` / `DECIMALS ...` tail in declarations.
    LengthSpec,
    /// Assignment statement `lhs = rhs .` or `lhs ?= rhs .` (rhs parsed with the same expression subset as templates).
    AssignStmt,
    /// `ASSIGN ... TO ... .`
    AssignKeywordStmt,
    /// Source expression that drives `ASSIGN ... TO FIELD-SYMBOL(...)` metadata inference.
    AssignSourceExpr,
    /// Inline target declaration `FIELD-SYMBOL(<fs>)`.
    FieldSymbolInlineDecl,
    /// A valid statement kept as raw tokens through a top-level `.` when no more specific kind applies yet.
    UnparsedStmt,
    /// `PUBLIC SECTION.` / `PROTECTED SECTION.` / `PRIVATE SECTION.`
    ClassSectionStmt,
    /// `METHODS ... .` or `CLASS-METHODS ... .`
    MethodsStmt,
    /// `INTERFACES if_name .`
    InterfacesStmt,
    /// `REPORT ... .`
    ReportStmt,
    /// `INCLUDE ... .`
    IncludeStmt,
    /// Deprecated `TYPE-POOLS ... .`
    TypePoolsStmt,
    /// `FORM ... . ... ENDFORM.`
    FormDecl,
    /// `TABLES` / `USING` / `CHANGING` parameter section inside a `FORM`.
    FormParamSection,
    /// `MODULE ... . ... ENDMODULE.`
    ModuleDecl,
    /// Event block such as `START-OF-SELECTION.`.
    EventBlock,
    /// `IF cond. ... [ELSEIF cond. ...]* [ELSE. ...] ENDIF.`
    IfStmt,
    /// One `ELSEIF cond.` branch and its body (children: `elseif` token, condition, `.`, then nested statements).
    ElseifClause,
    /// `ELSE.` and its body (`else`, `.`, nested statements).
    ElseClause,
    /// `CASE expr. ... ENDCASE.`
    CaseStmt,
    /// One `WHEN ... .` branch inside `CASE`.
    WhenClause,
    /// `WHILE cond. ... ENDWHILE.`
    WhileStmt,
    /// `DO [ . | <arith> TIMES . ] ... ENDDO.` — `<arith>` is a parsed expression when `TIMES` is present.
    DoStmt,
    /// `LOOP ... . ... ENDLOOP.`
    LoopStmt,
    /// `AT source` inside `LOOP`.
    LoopSourceClause,
    /// `INTO target` inside `LOOP`.
    LoopIntoClause,
    /// `ASSIGNING target` inside `LOOP`.
    LoopAssigningClause,
    /// `REFERENCE INTO target` inside `LOOP`.
    LoopReferenceIntoClause,
    /// `WHERE cond` inside `LOOP`.
    LoopWhereClause,
    /// `FROM expr` inside `LOOP`.
    LoopFromClause,
    /// `TO expr` inside `LOOP`.
    LoopToClause,
    /// `STEP expr` inside `LOOP`.
    LoopStepClause,
    /// `TRY. ... [CATCH ... . ...]* [CLEANUP. ...] ENDTRY.`
    TryStmt,
    /// `CATCH ... . ...`
    CatchClause,
    /// `CLEANUP. ...`
    CleanupClause,
    /// `CLASS ... . ... ENDCLASS.`
    ClassDecl,
    /// `INTERFACE ... . ... ENDINTERFACE.`
    InterfaceDecl,
    /// `METHOD ... . ... ENDMETHOD.`
    MethodDecl,
    /// `SELECT ... .` or `SELECT ... . ... ENDSELECT.`
    SelectStmt,
    /// Structured SQL query payload inside `SELECT`.
    SelectQuery,
    /// Projection segment between `SELECT` and the next SQL clause.
    SelectProjectionList,
    /// `FROM ...`
    SelectFromClause,
    /// `INTO ...` or `APPENDING ...`
    SelectIntoClause,
    /// `WHERE ...`
    SelectWhereClause,
    /// `... JOIN ... [ON ...]`
    SelectJoinClause,
    /// `GROUP BY ...`
    SelectGroupByClause,
    /// `HAVING ...`
    SelectHavingClause,
    /// `ORDER BY ...`
    SelectOrderByClause,
    /// `FOR ALL ENTRIES IN ...`
    SelectForAllEntriesClause,
    /// `UP TO ... ROWS`
    SelectUpToClause,
    /// `DISTINCT`
    SelectDistinctClause,
    /// Table or view source in a SQL `FROM`/`JOIN`.
    SqlDataSource,
    /// SQL alias introduced via `AS`.
    SqlAlias,
    /// SQL column reference such as `field` or `alias~field`.
    SqlColumnRef,
    /// Plain `*`.
    SqlStar,
    /// Qualified `alias~*`.
    SqlQualifiedStar,
    /// One projection item inside a projection list.
    SqlProjectionItem,
    /// Predicate-like SQL clause content (`WHERE`, `HAVING`, `ON`).
    SqlPredicateExpr,
    /// Host ABAP expression introduced with `@`.
    SqlHostExpr,
    /// Dynamic SQL fragment such as `WHERE (lt_cond)`.
    SqlDynamicWhere,
    /// Parenthesized SQL grouping.
    SqlParenGroup,
    /// `INSERT ... INTO [TABLE] itab ... .` (internal table insert)
    InsertTableStmt,
    /// `APPEND ... TO ... .`
    AppendStmt,
    /// `MOVE-CORRESPONDING ... TO ... .`
    MoveCorrespondingStmt,
    /// `MOVE source TO target .`
    MoveStmt,
    /// `MODIFY ... FROM ... .`
    ModifyStmt,
    /// `DELETE itab ... .` and `DELETE ADJACENT DUPLICATES FROM itab ... .`
    DeleteStmt,
    /// `DELETE dbtab FROM TABLE itab .`
    DeleteDbTableStmt,
    /// `ASSERT ... .`
    AssertStmt,
    /// `CHECK ... .`
    CheckStmt,
    /// `PERFORM ... .`
    PerformStmt,
    /// `CALL FUNCTION ... .`, `CALL TRANSFORMATION ... .`, or `CALL BADI ... .`
    CallStmt,
    /// `CREATE OBJECT ... .`
    CreateObjectStmt,
    /// `CREATE DATA ... TYPE ... .`
    CreateDataStmt,
    /// `CALL METHOD ... .`
    CallMethodStmt,
    /// Legacy `CALL METHOD` target/callee expression.
    CallMethodTarget,
    /// `COMMIT WORK.`
    CommitWorkStmt,
    /// `ROLLBACK WORK.`
    RollbackWorkStmt,
    /// `RAISE ... .`
    RaiseStmt,
    /// `MESSAGE ... .`
    MessageStmt,
    /// `ENDAT.`
    EndAtStmt,
    /// `FIND ... .`
    FindStmt,
    /// `READ TABLE ... .`
    ReadTableStmt,
    /// `GET TIME STAMP FIELD ... .`
    GetTimeStampStmt,
    /// `GET REFERENCE OF ... INTO ... .`
    GetReferenceStmt,
    /// `GET BIT ... OF ... INTO ... .`
    GetBitStmt,
    /// `SET BIT ... OF ... TO ... .`
    SetBitStmt,
    /// `WRITE ... .`
    WriteStmt,
    /// `SPLIT ... AT ... INTO ... .`
    SplitStmt,
    /// `CONCATENATE ... INTO ... .`
    ConcatenateStmt,
    /// `CONDENSE dobj [NO-GAPS] .`
    CondenseStmt,
    /// `SORT itab [STABLE] [AS TEXT] [BY ...] .`
    SortStmt,
    /// `IS [NOT] INITIAL|BOUND|ASSIGNED|REQUESTED|SUPPLIED` in logical conditions.
    IsPredicate,
    /// `IS [NOT] INSTANCE OF type` (type parsed as a concat-level expression).
    InstanceOfPredicate,
    /// Relational `expr BETWEEN low AND high`.
    BetweenExpr,
}

impl SyntaxKind {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::File => "File",
            Self::Token => "Token",
            Self::Error => "Error",
            Self::CharStringTemplate => "CharStringTemplate",
            Self::TemplateLiteral => "TemplateLiteral",
            Self::TemplateInterpolation => "TemplateInterpolation",
            Self::TemplateExpr => "TemplateExpr",
            Self::TemplateFormatSpec => "TemplateFormatSpec",
            Self::BinaryExpr => "BinaryExpr",
            Self::UnaryExpr => "UnaryExpr",
            Self::ParenExpr => "ParenExpr",
            Self::ExprIdent => "ExprIdent",
            Self::ExprLiteral => "ExprLiteral",
            Self::SelectorExpr => "SelectorExpr",
            Self::SubstringExpr => "SubstringExpr",
            Self::CallExpr => "CallExpr",
            Self::CallArgList => "CallArgList",
            Self::CallArgSection => "CallArgSection",
            Self::CallNamedArg => "CallNamedArg",
            Self::CallPositionalArg => "CallPositionalArg",
            Self::ConstructorExpr => "ConstructorExpr",
            Self::DataDecl => "DataDecl",
            Self::DataTypedClause => "DataTypedClause",
            Self::DataInlineDecl => "DataInlineDecl",
            Self::StaticsDecl => "StaticsDecl",
            Self::TypesDecl => "TypesDecl",
            Self::TypesTypedClause => "TypesTypedClause",
            Self::StructuredDecl => "StructuredDecl",
            Self::StructuredFieldClause => "StructuredFieldClause",
            Self::StructuredIncludeClause => "StructuredIncludeClause",
            Self::ConstantsDecl => "ConstantsDecl",
            Self::ConstantClause => "ConstantClause",
            Self::FieldSymbolsDecl => "FieldSymbolsDecl",
            Self::FieldSymbolClause => "FieldSymbolClause",
            Self::DataDeclName => "DataDeclName",
            Self::TypeRefSimple => "TypeRefSimple",
            Self::TypeRefName => "TypeRefName",
            Self::TypeRefSelectorChain => "TypeRefSelectorChain",
            Self::ValueClause => "ValueClause",
            Self::LengthSpec => "LengthSpec",
            Self::AssignStmt => "AssignStmt",
            Self::AssignKeywordStmt => "AssignKeywordStmt",
            Self::AssignSourceExpr => "AssignSourceExpr",
            Self::FieldSymbolInlineDecl => "FieldSymbolInlineDecl",
            Self::UnparsedStmt => "UnparsedStmt",
            Self::ClassSectionStmt => "ClassSectionStmt",
            Self::MethodsStmt => "MethodsStmt",
            Self::InterfacesStmt => "InterfacesStmt",
            Self::ReportStmt => "ReportStmt",
            Self::IncludeStmt => "IncludeStmt",
            Self::TypePoolsStmt => "TypePoolsStmt",
            Self::FormDecl => "FormDecl",
            Self::FormParamSection => "FormParamSection",
            Self::ModuleDecl => "ModuleDecl",
            Self::EventBlock => "EventBlock",
            Self::IfStmt => "IfStmt",
            Self::ElseifClause => "ElseifClause",
            Self::ElseClause => "ElseClause",
            Self::CaseStmt => "CaseStmt",
            Self::WhenClause => "WhenClause",
            Self::WhileStmt => "WhileStmt",
            Self::DoStmt => "DoStmt",
            Self::LoopStmt => "LoopStmt",
            Self::LoopSourceClause => "LoopSourceClause",
            Self::LoopIntoClause => "LoopIntoClause",
            Self::LoopAssigningClause => "LoopAssigningClause",
            Self::LoopReferenceIntoClause => "LoopReferenceIntoClause",
            Self::LoopWhereClause => "LoopWhereClause",
            Self::LoopFromClause => "LoopFromClause",
            Self::LoopToClause => "LoopToClause",
            Self::LoopStepClause => "LoopStepClause",
            Self::TryStmt => "TryStmt",
            Self::CatchClause => "CatchClause",
            Self::CleanupClause => "CleanupClause",
            Self::ClassDecl => "ClassDecl",
            Self::InterfaceDecl => "InterfaceDecl",
            Self::MethodDecl => "MethodDecl",
            Self::SelectStmt => "SelectStmt",
            Self::SelectQuery => "SelectQuery",
            Self::SelectProjectionList => "SelectProjectionList",
            Self::SelectFromClause => "SelectFromClause",
            Self::SelectIntoClause => "SelectIntoClause",
            Self::SelectWhereClause => "SelectWhereClause",
            Self::SelectJoinClause => "SelectJoinClause",
            Self::SelectGroupByClause => "SelectGroupByClause",
            Self::SelectHavingClause => "SelectHavingClause",
            Self::SelectOrderByClause => "SelectOrderByClause",
            Self::SelectForAllEntriesClause => "SelectForAllEntriesClause",
            Self::SelectUpToClause => "SelectUpToClause",
            Self::SelectDistinctClause => "SelectDistinctClause",
            Self::SqlDataSource => "SqlDataSource",
            Self::SqlAlias => "SqlAlias",
            Self::SqlColumnRef => "SqlColumnRef",
            Self::SqlStar => "SqlStar",
            Self::SqlQualifiedStar => "SqlQualifiedStar",
            Self::SqlProjectionItem => "SqlProjectionItem",
            Self::SqlPredicateExpr => "SqlPredicateExpr",
            Self::SqlHostExpr => "SqlHostExpr",
            Self::SqlDynamicWhere => "SqlDynamicWhere",
            Self::SqlParenGroup => "SqlParenGroup",
            Self::InsertTableStmt => "InsertTableStmt",
            Self::AppendStmt => "AppendStmt",
            Self::MoveCorrespondingStmt => "MoveCorrespondingStmt",
            Self::MoveStmt => "MoveStmt",
            Self::ModifyStmt => "ModifyStmt",
            Self::DeleteStmt => "DeleteInternalTableStmt",
            Self::DeleteDbTableStmt => "DeleteDbTableFromTableStmt",
            Self::AssertStmt => "AssertStmt",
            Self::CheckStmt => "CheckStmt",
            Self::PerformStmt => "PerformStmt",
            Self::CallStmt => "CallStmt",
            Self::CreateObjectStmt => "CreateObjectStmt",
            Self::CreateDataStmt => "CreateDataStmt",
            Self::CallMethodStmt => "CallMethodStmt",
            Self::CallMethodTarget => "CallMethodTarget",
            Self::CommitWorkStmt => "CommitWorkStmt",
            Self::RollbackWorkStmt => "RollbackWorkStmt",
            Self::RaiseStmt => "RaiseStmt",
            Self::MessageStmt => "MessageStmt",
            Self::EndAtStmt => "EndAtStmt",
            Self::FindStmt => "FindStmt",
            Self::ReadTableStmt => "ReadTableStmt",
            Self::GetTimeStampStmt => "GetTimeStampStmt",
            Self::GetReferenceStmt => "GetReferenceStmt",
            Self::GetBitStmt => "GetBitStmt",
            Self::SetBitStmt => "SetBitStmt",
            Self::WriteStmt => "WriteStmt",
            Self::SplitStmt => "SplitStmt",
            Self::ConcatenateStmt => "ConcatenateStmt",
            Self::CondenseStmt => "CondenseStmt",
            Self::SortStmt => "SortStmt",
            Self::IsPredicate => "IsPredicate",
            Self::InstanceOfPredicate => "InstanceOfPredicate",
            Self::BetweenExpr => "BetweenExpr",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxNode {
    pub kind: SyntaxKind,
    pub range: TextRange,
    pub children: Vec<SyntaxNode>,
}

/// Parse result root: arena-backed syntax tree.
pub type File = arena::SyntaxTree;

impl SyntaxNode {
    pub fn leaf(kind: SyntaxKind, range: TextRange) -> Self {
        Self {
            kind,
            range,
            children: Vec::new(),
        }
    }

    pub fn branch(kind: SyntaxKind, range: TextRange, children: Vec<SyntaxNode>) -> Self {
        Self {
            kind,
            range,
            children,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{SyntaxKind, SyntaxNode};

    #[test]
    fn builds_branch_nodes() {
        let child = SyntaxNode::leaf(SyntaxKind::Token, 0..4);
        let root = SyntaxNode::branch(SyntaxKind::File, 0..4, vec![child]);

        assert_eq!(root.kind, SyntaxKind::File);
        assert_eq!(root.children.len(), 1);
    }
}
