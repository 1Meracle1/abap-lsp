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
    /// Call-style postfix `callee( ... )`.
    CallExpr,
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
    /// `VALUE expr` in a declaration.
    ValueClause,
    /// `LENGTH ...` / `DECIMALS ...` tail in declarations.
    LengthSpec,
    /// Assignment statement `lhs = rhs .` or `lhs ?= rhs .` (rhs parsed with the same expression subset as templates).
    AssignStmt,
    /// `ASSIGN ... TO ... .`
    AssignKeywordStmt,
    /// Inline target declaration `FIELD-SYMBOL(<fs>)`.
    FieldSymbolInlineDecl,
    /// A simple line-oriented statement parsed as tokens through a top-level `.` (e.g. `REPORT`, `WRITE`, incomplete `DATA` …).
    SimpleStmt,
    /// `REPORT ... .`
    ReportStmt,
    /// `INCLUDE ... .`
    IncludeStmt,
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
    /// `DO [times TIMES]. ... ENDDO.`
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
    /// `APPEND ... TO ... .`
    AppendStmt,
    /// `READ TABLE ... .`
    ReadTableStmt,
    /// `WRITE ... .`
    WriteStmt,
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
            Self::CallExpr => "CallExpr",
            Self::ConstructorExpr => "ConstructorExpr",
            Self::DataDecl => "DataDecl",
            Self::DataTypedClause => "DataTypedClause",
            Self::DataInlineDecl => "DataInlineDecl",
            Self::StaticsDecl => "StaticsDecl",
            Self::TypesDecl => "TypesDecl",
            Self::TypesTypedClause => "TypesTypedClause",
            Self::ConstantsDecl => "ConstantsDecl",
            Self::ConstantClause => "ConstantClause",
            Self::FieldSymbolsDecl => "FieldSymbolsDecl",
            Self::FieldSymbolClause => "FieldSymbolClause",
            Self::DataDeclName => "DataDeclName",
            Self::TypeRefSimple => "TypeRefSimple",
            Self::ValueClause => "ValueClause",
            Self::LengthSpec => "LengthSpec",
            Self::AssignStmt => "AssignStmt",
            Self::AssignKeywordStmt => "AssignKeywordStmt",
            Self::FieldSymbolInlineDecl => "FieldSymbolInlineDecl",
            Self::SimpleStmt => "SimpleStmt",
            Self::ReportStmt => "ReportStmt",
            Self::IncludeStmt => "IncludeStmt",
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
            Self::AppendStmt => "AppendStmt",
            Self::ReadTableStmt => "ReadTableStmt",
            Self::WriteStmt => "WriteStmt",
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
