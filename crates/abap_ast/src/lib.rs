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
    /// Table expression such as `itab[ key = value ]`.
    TableExpr,
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
    /// `LET ... IN ...` expression inside constructor-expression contexts.
    LetExpr,
    /// One `name = expr` binding inside a constructor `LET`.
    ConstructorLetBinding,
    /// `WHEN ... THEN ...` clause inside `COND` / `SWITCH`.
    ConstructorWhenClause,
    /// `ELSE ...` clause inside `COND` / `SWITCH`.
    ConstructorElseClause,
    /// `FOR ...` iterator/conditional clause inside a constructor expression.
    ConstructorForClause,
    /// `WHERE ( ... )` inside a constructor `FOR`.
    ConstructorWhereClause,
    /// `INIT ...` clause inside `REDUCE`.
    ConstructorInitClause,
    /// `NEXT ...` clause inside `REDUCE`.
    ConstructorNextClause,
    /// One `name = expr` assignment inside constructor bodies / `INIT` / `NEXT`.
    ConstructorNamedAssignment,
    /// `MAPPING ...` clause inside `CORRESPONDING`.
    ConstructorCorrespondingMappingClause,
    /// One `target = source ...` mapping relationship inside `CORRESPONDING`.
    ConstructorCorrespondingMappingAssignment,
    /// `EXCEPT ...` clause inside `CORRESPONDING`.
    ConstructorCorrespondingExceptClause,
    /// `BASE expr` inside a value constructor.
    ConstructorBaseClause,
    /// `LINES OF ...` inside a value constructor.
    ConstructorLinesOfClause,
    /// `expr OPTIONAL` inside a constructor-expression context.
    ConstructorOptionalExpr,
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
    /// `CLASS class DEFINITION DEFERRED [PUBLIC].`
    ClassDeferredStmt,
    /// `PUBLIC SECTION.` / `PROTECTED SECTION.` / `PRIVATE SECTION.`
    ClassSectionStmt,
    /// Visibility keyword inside `PUBLIC SECTION.` / `PROTECTED SECTION.` / `PRIVATE SECTION.`
    ClassSectionVisibility,
    /// `METHODS ... .` or `CLASS-METHODS ... .`
    MethodsStmt,
    /// `EVENTS ... .` or `CLASS-EVENTS ... .`
    EventsStmt,
    /// `INTERFACES if_name .`
    InterfacesStmt,
    /// `REPORT ... .`
    ReportStmt,
    /// `TABLES ... .`
    TablesDecl,
    /// `PARAMETERS ... .`
    ParametersDecl,
    /// `SELECT-OPTIONS ... .`
    SelectOptionsDecl,
    /// `SELECTION-SCREEN ... .`
    SelectionScreenStmt,
    /// `INCLUDE ... .`
    IncludeStmt,
    /// One include program name inside `INCLUDE ... .`
    IncludeName,
    /// Deprecated `TYPE-POOLS ... .`
    TypePoolsStmt,
    /// `ALIASES ... .`
    AliasesStmt,
    /// One alias definition inside `ALIASES`.
    AliasEntry,
    /// Alias name in `ALIASES alias FOR if_intf~member`.
    AliasName,
    /// Target member name in `ALIASES alias FOR if_intf~member`.
    AliasMember,
    /// `CLEAR ... .`
    ClearStmt,
    /// One cleared operand inside `CLEAR`.
    ClearOperand,
    /// `CONVERT ... .`
    ConvertStmt,
    /// Source operand inside `CONVERT`.
    ConvertOperand,
    /// Target timestamp operand inside `CONVERT ... INTO TIME STAMP ...`.
    ConvertTargetOperand,
    /// `TIME ZONE ...` operand inside `CONVERT`.
    ConvertTimeZoneOperand,
    /// `DATE ...` target inside `CONVERT TIME STAMP ... INTO ...`.
    ConvertDateTarget,
    /// `TIME ...` target inside `CONVERT TIME STAMP ... INTO ...`.
    ConvertTimeTarget,
    /// `DAYLIGHT SAVING TIME ...` target inside `CONVERT TIME STAMP ... INTO ...`.
    ConvertDaylightSavingTarget,
    /// `DESCRIBE TABLE ... LINES ... .`
    DescribeStmt,
    /// Table operand inside `DESCRIBE TABLE`.
    DescribeTableOperand,
    /// `LINES ...` target inside `DESCRIBE TABLE`.
    DescribeLinesTarget,
    /// `REPLACE ... .`
    ReplaceStmt,
    /// Pattern/source operand inside `REPLACE`.
    ReplacePatternOperand,
    /// Target operand after `IN` inside `REPLACE`.
    ReplaceTargetOperand,
    /// Replacement operand after `WITH` inside `REPLACE`.
    ReplaceWithOperand,
    /// `WAIT UP TO ... SECONDS .`
    WaitStmt,
    /// Duration operand inside `WAIT UP TO ... SECONDS`.
    WaitOperand,
    /// `FORM ... . ... ENDFORM.`
    FormDecl,
    /// `TABLES` / `USING` / `CHANGING` / `RAISING` section inside a `FORM`.
    FormParamSection,
    /// One parameter entry inside a `FORM` header section.
    FormParam,
    /// `FUNCTION ... . ... ENDFUNCTION.`
    FunctionDecl,
    /// `IMPORTING` / `EXPORTING` / `CHANGING` / `TABLES` / `RAISING` / `EXCEPTIONS` section inside a `FUNCTION`.
    FunctionParamSection,
    /// One parameter or exception entry inside a `FUNCTION` header section.
    FunctionParam,
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
    /// `AT FIRST|LAST|NEW ...|END OF ... . ... ENDAT.` inside `LOOP`.
    AtStmt,
    /// `AT source` inside `LOOP`.
    LoopSourceClause,
    /// `GROUP group` in `LOOP AT GROUP group`.
    LoopAtGroupClause,
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
    /// `GROUP BY ...` inside `LOOP`.
    LoopGroupByClause,
    /// `TRY. ... [CATCH ... . ...]* [CLEANUP. ...] ENDTRY.`
    TryStmt,
    /// `CATCH ... . ...`
    CatchClause,
    /// `CLEANUP. ...`
    CleanupClause,
    /// `CLASS ... . ... ENDCLASS.`
    ClassDecl,
    /// `INHERITING FROM ...` clause inside a class header.
    ClassInheritanceClause,
    /// `IMPLEMENTATION` marker inside a class header.
    ClassImplementationMarker,
    /// `INTERFACE ... . ... ENDINTERFACE.`
    InterfaceDecl,
    /// `METHOD ... . ... ENDMETHOD.`
    MethodDecl,
    /// Qualified or unqualified implementation target in a `METHOD ... .` header.
    MethodDeclTarget,
    /// `SELECT ... .` or `SELECT ... . ... ENDSELECT.`
    SelectStmt,
    /// `OPEN CURSOR ... FOR SELECT ... .`
    OpenCursorStmt,
    /// `CLOSE CURSOR cursor .`
    CloseCursorStmt,
    /// Cursor handle operand in `OPEN CURSOR ...`, `FETCH NEXT CURSOR ...`, etc.
    CursorHandleOperand,
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
    /// `AS alias` wrapper in SQL projection/source positions.
    SqlAliasClause,
    /// SQL alias introduced via `AS`.
    SqlAlias,
    /// SQL column reference such as `field`.
    SqlColumnRef,
    /// Qualified SQL column reference such as `alias~field`.
    SqlQualifiedColumnRef,
    /// Plain `*`.
    SqlStar,
    /// Qualified `alias~*`.
    SqlQualifiedStar,
    /// Aggregate call such as `COUNT( * )` or `MAX( col )`.
    SqlAggregateCall,
    /// One projection item inside a projection list.
    SqlProjectionItem,
    /// Predicate-like SQL clause content (`WHERE`, `HAVING`, `ON`).
    SqlPredicateExpr,
    /// One predicate operand between SQL operators/keywords.
    SqlPredicateOperand,
    /// Host ABAP expression introduced with `@`.
    SqlHostExpr,
    /// Dynamic SQL fragment such as `WHERE (lt_cond)`.
    SqlDynamicWhere,
    /// Parenthesized SQL grouping.
    SqlParenGroup,
    /// `INSERT ... INTO [TABLE] itab ... .` (internal table insert)
    InsertTableStmt,
    /// `INSERT dbtab FROM [TABLE] src ... .`
    InsertDbTableStmt,
    /// `INSERT TEXTPOOL prog FROM itab [LANGUAGE lang] .`
    InsertTextpoolStmt,
    /// `APPEND ... TO ... .`
    AppendStmt,
    /// `MOVE-CORRESPONDING ... TO ... .`
    MoveCorrespondingStmt,
    /// `MOVE source TO target .`
    MoveStmt,
    /// `MODIFY ... FROM ... .`
    ModifyStmt,
    /// `UPDATE dbtab ... .`
    UpdateStmt,
    /// Source table/view operand after `UPDATE`.
    UpdateTarget,
    /// `SET ...` clause inside `UPDATE`.
    UpdateSetClause,
    /// One `col = value` assignment inside `UPDATE ... SET`.
    UpdateSetAssignment,
    /// RHS operand inside an `UPDATE ... SET` assignment.
    UpdateSetValueOperand,
    /// Source operand after `FROM` inside `UPDATE`.
    UpdateFromOperand,
    /// `WHERE ...` clause inside `UPDATE`.
    UpdateWhereClause,
    /// `DELETE itab ... .` and `DELETE ADJACENT DUPLICATES FROM itab ... .`
    DeleteStmt,
    /// `DELETE dbtab FROM TABLE itab .`
    DeleteDbTableStmt,
    /// `ASSERT ... .`
    AssertStmt,
    /// `CHECK ... .`
    CheckStmt,
    /// `STOP.`
    StopStmt,
    /// `PERFORM ... .`
    PerformStmt,
    /// `SUBMIT ... .`
    SubmitStmt,
    /// Target report/program name after `SUBMIT`.
    SubmitTarget,
    /// Operand after `USING SELECTION-SCREEN`.
    SubmitSelectionScreenOperand,
    /// Operand after `USING SELECTION-SET`.
    SubmitSelectionSetOperand,
    /// Operand after `USING SELECTION-SETS OF PROGRAM`.
    SubmitSelectionSetsProgramOperand,
    /// Operand after `WITH SELECTION-TABLE`.
    SubmitSelectionTableOperand,
    /// One `WITH sel ...` clause inside `SUBMIT`.
    SubmitWithClause,
    /// Operand after `WITH FREE SELECTIONS`.
    SubmitFreeSelectionsOperand,
    /// Operand after `LINE-SIZE`.
    SubmitLineSizeOperand,
    /// Operand after `LINE-COUNT`.
    SubmitLineCountOperand,
    /// Operand after `SPOOL PARAMETERS`.
    SubmitSpoolParametersOperand,
    /// Operand after `ARCHIVE PARAMETERS`.
    SubmitArchiveParametersOperand,
    /// Operand after `USER`.
    SubmitUserOperand,
    /// Operand after `VIA JOB`.
    SubmitJobOperand,
    /// Operand after `NUMBER` in `VIA JOB`.
    SubmitJobNumberOperand,
    /// Operand after `LANGUAGE` in `VIA JOB`.
    SubmitLanguageOperand,
    /// `CALL cfunc .`, `CALL FUNCTION ... .`, `CALL TRANSFORMATION ... .`, or `CALL BADI ... .`
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
    /// `RAISE EVENT ... .`
    RaiseEventStmt,
    /// `MESSAGE ... .`
    MessageStmt,
    /// `MESSAGE` payload before `WITH` / `INTO` / `DISPLAY LIKE` / `RAISING`.
    MessageHeadClause,
    /// Operand after `MESSAGE ID`.
    MessageIdOperand,
    /// Operand after `MESSAGE ... TYPE`.
    MessageTypeOperand,
    /// Operand after `MESSAGE ... NUMBER`.
    MessageNumberOperand,
    /// Operand after `MESSAGE` in compact/dynamic form.
    MessageCodeOperand,
    /// `WITH ...` inside `MESSAGE`.
    MessageWithClause,
    /// One operand inside `MESSAGE WITH`.
    MessageOperand,
    /// `TEXT-###` message/text-pool id operand.
    MessageTextPoolId,
    /// `INTO ...` inside `MESSAGE`.
    MessageIntoClause,
    /// `DISPLAY LIKE ...` inside `MESSAGE`.
    MessageDisplayLikeClause,
    /// `RAISING ...` inside `MESSAGE`.
    MessageRaisingClause,
    /// `LEAVE LIST-PROCESSING.`
    LeaveStmt,
    /// `ENDAT.`
    EndAtStmt,
    /// `FIND ... .`
    FindStmt,
    /// Pattern operand inside `FIND`.
    FindPatternOperand,
    /// Target operand after `IN` inside `FIND`.
    FindInOperand,
    /// Target operand after `MATCH OFFSET`/`MATCH LENGTH` inside `FIND`.
    FindMatchTarget,
    /// One operand inside `SUBMATCHES` inside `FIND`.
    FindSubmatchTarget,
    /// Target operand after `RESULTS` inside `FIND`.
    FindResultsTarget,
    /// `READ TABLE ... .`
    ReadTableStmt,
    /// `AUTHORITY-CHECK ... .`
    AuthorityCheckStmt,
    /// Operand after `OBJECT` inside `AUTHORITY-CHECK`.
    AuthorityCheckObjectOperand,
    /// Operand after `FOR USER` inside `AUTHORITY-CHECK`.
    AuthorityCheckUserOperand,
    /// One `ID ... FIELD ...|DUMMY` clause inside `AUTHORITY-CHECK`.
    AuthorityCheckIdClause,
    /// Operand after `ID` inside `AUTHORITY-CHECK`.
    AuthorityCheckIdOperand,
    /// Operand after `FIELD` inside `AUTHORITY-CHECK`.
    AuthorityCheckFieldOperand,
    /// `GET TIME STAMP FIELD ... .`
    GetTimeStampStmt,
    /// `GET REFERENCE OF ... INTO ... .`
    GetReferenceStmt,
    /// `GET BIT ... OF ... INTO ... .`
    GetBitStmt,
    /// `SET BIT ... OF ... TO ... .`
    SetBitStmt,
    /// `SET PF-STATUS ... .`
    SetPfStatusStmt,
    /// `SET TITLEBAR ... .`
    SetTitlebarStmt,
    /// `WRITE ... .`
    WriteStmt,
    /// `SPLIT ... AT ... INTO ... .`
    SplitStmt,
    /// Source operand before `AT` inside `SPLIT`.
    SplitSourceOperand,
    /// Separator operand after `AT` inside `SPLIT`.
    SplitSeparatorOperand,
    /// One target after `INTO` inside `SPLIT`.
    SplitTargetOperand,
    /// `CONCATENATE ... INTO ... .`
    ConcatenateStmt,
    /// One source operand before `INTO` inside `CONCATENATE`.
    ConcatenateSourceOperand,
    /// Target operand after `INTO` inside `CONCATENATE`.
    ConcatenateTargetOperand,
    /// Separator operand after `SEPARATED BY` inside `CONCATENATE`.
    ConcatenateSeparatorOperand,
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
            Self::TableExpr => "TableExpr",
            Self::SelectorExpr => "SelectorExpr",
            Self::SubstringExpr => "SubstringExpr",
            Self::CallExpr => "CallExpr",
            Self::CallArgList => "CallArgList",
            Self::CallArgSection => "CallArgSection",
            Self::CallNamedArg => "CallNamedArg",
            Self::CallPositionalArg => "CallPositionalArg",
            Self::ConstructorExpr => "ConstructorExpr",
            Self::LetExpr => "LetExpr",
            Self::ConstructorLetBinding => "ConstructorLetBinding",
            Self::ConstructorWhenClause => "ConstructorWhenClause",
            Self::ConstructorElseClause => "ConstructorElseClause",
            Self::ConstructorForClause => "ConstructorForClause",
            Self::ConstructorWhereClause => "ConstructorWhereClause",
            Self::ConstructorInitClause => "ConstructorInitClause",
            Self::ConstructorNextClause => "ConstructorNextClause",
            Self::ConstructorNamedAssignment => "ConstructorNamedAssignment",
            Self::ConstructorCorrespondingMappingClause => "ConstructorCorrespondingMappingClause",
            Self::ConstructorCorrespondingMappingAssignment => {
                "ConstructorCorrespondingMappingAssignment"
            }
            Self::ConstructorCorrespondingExceptClause => "ConstructorCorrespondingExceptClause",
            Self::ConstructorBaseClause => "ConstructorBaseClause",
            Self::ConstructorLinesOfClause => "ConstructorLinesOfClause",
            Self::ConstructorOptionalExpr => "ConstructorOptionalExpr",
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
            Self::ClassDeferredStmt => "ClassDeferredStmt",
            Self::ClassSectionStmt => "ClassSectionStmt",
            Self::ClassSectionVisibility => "ClassSectionVisibility",
            Self::MethodsStmt => "MethodsStmt",
            Self::EventsStmt => "EventsStmt",
            Self::InterfacesStmt => "InterfacesStmt",
            Self::ReportStmt => "ReportStmt",
            Self::TablesDecl => "TablesDecl",
            Self::ParametersDecl => "ParametersDecl",
            Self::SelectOptionsDecl => "SelectOptionsDecl",
            Self::SelectionScreenStmt => "SelectionScreenStmt",
            Self::IncludeStmt => "IncludeStmt",
            Self::IncludeName => "IncludeName",
            Self::TypePoolsStmt => "TypePoolsStmt",
            Self::AliasesStmt => "AliasesStmt",
            Self::AliasEntry => "AliasEntry",
            Self::AliasName => "AliasName",
            Self::AliasMember => "AliasMember",
            Self::ClearStmt => "ClearStmt",
            Self::ClearOperand => "ClearOperand",
            Self::ConvertStmt => "ConvertStmt",
            Self::ConvertOperand => "ConvertOperand",
            Self::ConvertTargetOperand => "ConvertTargetOperand",
            Self::ConvertTimeZoneOperand => "ConvertTimeZoneOperand",
            Self::ConvertDateTarget => "ConvertDateTarget",
            Self::ConvertTimeTarget => "ConvertTimeTarget",
            Self::ConvertDaylightSavingTarget => "ConvertDaylightSavingTarget",
            Self::DescribeStmt => "DescribeStmt",
            Self::DescribeTableOperand => "DescribeTableOperand",
            Self::DescribeLinesTarget => "DescribeLinesTarget",
            Self::ReplaceStmt => "ReplaceStmt",
            Self::ReplacePatternOperand => "ReplacePatternOperand",
            Self::ReplaceTargetOperand => "ReplaceTargetOperand",
            Self::ReplaceWithOperand => "ReplaceWithOperand",
            Self::WaitStmt => "WaitStmt",
            Self::WaitOperand => "WaitOperand",
            Self::FormDecl => "FormDecl",
            Self::FormParamSection => "FormParamSection",
            Self::FormParam => "FormParam",
            Self::FunctionDecl => "FunctionDecl",
            Self::FunctionParamSection => "FunctionParamSection",
            Self::FunctionParam => "FunctionParam",
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
            Self::AtStmt => "AtStmt",
            Self::LoopSourceClause => "LoopSourceClause",
            Self::LoopAtGroupClause => "LoopAtGroupClause",
            Self::LoopIntoClause => "LoopIntoClause",
            Self::LoopAssigningClause => "LoopAssigningClause",
            Self::LoopReferenceIntoClause => "LoopReferenceIntoClause",
            Self::LoopWhereClause => "LoopWhereClause",
            Self::LoopFromClause => "LoopFromClause",
            Self::LoopToClause => "LoopToClause",
            Self::LoopStepClause => "LoopStepClause",
            Self::LoopGroupByClause => "LoopGroupByClause",
            Self::TryStmt => "TryStmt",
            Self::CatchClause => "CatchClause",
            Self::CleanupClause => "CleanupClause",
            Self::ClassDecl => "ClassDecl",
            Self::ClassInheritanceClause => "ClassInheritanceClause",
            Self::ClassImplementationMarker => "ClassImplementationMarker",
            Self::InterfaceDecl => "InterfaceDecl",
            Self::MethodDecl => "MethodDecl",
            Self::MethodDeclTarget => "MethodDeclTarget",
            Self::SelectStmt => "SelectStmt",
            Self::OpenCursorStmt => "OpenCursorStmt",
            Self::CloseCursorStmt => "CloseCursorStmt",
            Self::CursorHandleOperand => "CursorHandleOperand",
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
            Self::SqlAliasClause => "SqlAliasClause",
            Self::SqlAlias => "SqlAlias",
            Self::SqlColumnRef => "SqlColumnRef",
            Self::SqlQualifiedColumnRef => "SqlQualifiedColumnRef",
            Self::SqlStar => "SqlStar",
            Self::SqlQualifiedStar => "SqlQualifiedStar",
            Self::SqlAggregateCall => "SqlAggregateCall",
            Self::SqlProjectionItem => "SqlProjectionItem",
            Self::SqlPredicateExpr => "SqlPredicateExpr",
            Self::SqlPredicateOperand => "SqlPredicateOperand",
            Self::SqlHostExpr => "SqlHostExpr",
            Self::SqlDynamicWhere => "SqlDynamicWhere",
            Self::SqlParenGroup => "SqlParenGroup",
            Self::InsertTableStmt => "InsertTableStmt",
            Self::InsertDbTableStmt => "InsertDbTableStmt",
            Self::InsertTextpoolStmt => "InsertTextpoolStmt",
            Self::AppendStmt => "AppendStmt",
            Self::MoveCorrespondingStmt => "MoveCorrespondingStmt",
            Self::MoveStmt => "MoveStmt",
            Self::ModifyStmt => "ModifyStmt",
            Self::UpdateStmt => "UpdateStmt",
            Self::UpdateTarget => "UpdateTarget",
            Self::UpdateSetClause => "UpdateSetClause",
            Self::UpdateSetAssignment => "UpdateSetAssignment",
            Self::UpdateSetValueOperand => "UpdateSetValueOperand",
            Self::UpdateFromOperand => "UpdateFromOperand",
            Self::UpdateWhereClause => "UpdateWhereClause",
            Self::DeleteStmt => "DeleteInternalTableStmt",
            Self::DeleteDbTableStmt => "DeleteDbTableFromTableStmt",
            Self::AssertStmt => "AssertStmt",
            Self::CheckStmt => "CheckStmt",
            Self::StopStmt => "StopStmt",
            Self::PerformStmt => "PerformStmt",
            Self::SubmitStmt => "SubmitStmt",
            Self::SubmitTarget => "SubmitTarget",
            Self::SubmitSelectionScreenOperand => "SubmitSelectionScreenOperand",
            Self::SubmitSelectionSetOperand => "SubmitSelectionSetOperand",
            Self::SubmitSelectionSetsProgramOperand => "SubmitSelectionSetsProgramOperand",
            Self::SubmitSelectionTableOperand => "SubmitSelectionTableOperand",
            Self::SubmitWithClause => "SubmitWithClause",
            Self::SubmitFreeSelectionsOperand => "SubmitFreeSelectionsOperand",
            Self::SubmitLineSizeOperand => "SubmitLineSizeOperand",
            Self::SubmitLineCountOperand => "SubmitLineCountOperand",
            Self::SubmitSpoolParametersOperand => "SubmitSpoolParametersOperand",
            Self::SubmitArchiveParametersOperand => "SubmitArchiveParametersOperand",
            Self::SubmitUserOperand => "SubmitUserOperand",
            Self::SubmitJobOperand => "SubmitJobOperand",
            Self::SubmitJobNumberOperand => "SubmitJobNumberOperand",
            Self::SubmitLanguageOperand => "SubmitLanguageOperand",
            Self::CallStmt => "CallStmt",
            Self::CreateObjectStmt => "CreateObjectStmt",
            Self::CreateDataStmt => "CreateDataStmt",
            Self::CallMethodStmt => "CallMethodStmt",
            Self::CallMethodTarget => "CallMethodTarget",
            Self::CommitWorkStmt => "CommitWorkStmt",
            Self::RollbackWorkStmt => "RollbackWorkStmt",
            Self::RaiseStmt => "RaiseStmt",
            Self::RaiseEventStmt => "RaiseEventStmt",
            Self::MessageStmt => "MessageStmt",
            Self::MessageHeadClause => "MessageHeadClause",
            Self::MessageIdOperand => "MessageIdOperand",
            Self::MessageTypeOperand => "MessageTypeOperand",
            Self::MessageNumberOperand => "MessageNumberOperand",
            Self::MessageCodeOperand => "MessageCodeOperand",
            Self::MessageWithClause => "MessageWithClause",
            Self::MessageOperand => "MessageOperand",
            Self::MessageTextPoolId => "MessageTextPoolId",
            Self::MessageIntoClause => "MessageIntoClause",
            Self::MessageDisplayLikeClause => "MessageDisplayLikeClause",
            Self::MessageRaisingClause => "MessageRaisingClause",
            Self::LeaveStmt => "LeaveStmt",
            Self::EndAtStmt => "EndAtStmt",
            Self::FindStmt => "FindStmt",
            Self::FindPatternOperand => "FindPatternOperand",
            Self::FindInOperand => "FindInOperand",
            Self::FindMatchTarget => "FindMatchTarget",
            Self::FindSubmatchTarget => "FindSubmatchTarget",
            Self::FindResultsTarget => "FindResultsTarget",
            Self::ReadTableStmt => "ReadTableStmt",
            Self::AuthorityCheckStmt => "AuthorityCheckStmt",
            Self::AuthorityCheckObjectOperand => "AuthorityCheckObjectOperand",
            Self::AuthorityCheckUserOperand => "AuthorityCheckUserOperand",
            Self::AuthorityCheckIdClause => "AuthorityCheckIdClause",
            Self::AuthorityCheckIdOperand => "AuthorityCheckIdOperand",
            Self::AuthorityCheckFieldOperand => "AuthorityCheckFieldOperand",
            Self::GetTimeStampStmt => "GetTimeStampStmt",
            Self::GetReferenceStmt => "GetReferenceStmt",
            Self::GetBitStmt => "GetBitStmt",
            Self::SetBitStmt => "SetBitStmt",
            Self::SetPfStatusStmt => "SetPfStatusStmt",
            Self::SetTitlebarStmt => "SetTitlebarStmt",
            Self::WriteStmt => "WriteStmt",
            Self::SplitStmt => "SplitStmt",
            Self::SplitSourceOperand => "SplitSourceOperand",
            Self::SplitSeparatorOperand => "SplitSeparatorOperand",
            Self::SplitTargetOperand => "SplitTargetOperand",
            Self::ConcatenateStmt => "ConcatenateStmt",
            Self::ConcatenateSourceOperand => "ConcatenateSourceOperand",
            Self::ConcatenateTargetOperand => "ConcatenateTargetOperand",
            Self::ConcatenateSeparatorOperand => "ConcatenateSeparatorOperand",
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
