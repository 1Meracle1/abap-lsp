use std::sync::Arc;

use abap_lexer::TextRange;

use crate::ids::{ReferenceId, ScopeId, StructureId, SymbolHandle, SymbolId, UnitId};
use crate::scope::{Namespace, ScopeData};
use crate::semantic::SemanticIndex;
use crate::semantic_queries::SemanticQueries;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    BuiltinType,
    BuiltinRoutine,
    BuiltinConstant,
    BuiltinVariable,
    Variable,
    Constant,
    EnumMember,
    TypeDef,
    FieldSymbol,
    Form,
    Parameter,
    Class,
    Interface,
    Method,
    Field,
    Include,
    Event,
    Module,
    Control,
    Report,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Protected,
    Private,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClassMemberKind {
    Attribute,
    Method,
    Event,
}

impl SymbolKind {
    pub const fn is_builtin(self) -> bool {
        matches!(
            self,
            Self::BuiltinType
                | Self::BuiltinRoutine
                | Self::BuiltinConstant
                | Self::BuiltinVariable
        )
    }

    pub fn occupies(self, namespace: Namespace) -> bool {
        match self {
            Self::BuiltinType | Self::TypeDef | Self::Class | Self::Interface => {
                namespace == Namespace::Type
            }
            Self::BuiltinRoutine | Self::Form | Self::Method | Self::Module | Self::Event => {
                namespace == Namespace::Routine
            }
            Self::BuiltinConstant
            | Self::BuiltinVariable
            | Self::Variable
            | Self::Constant
            | Self::EnumMember
            | Self::FieldSymbol
            | Self::Parameter
            | Self::Field
            | Self::Include
            | Self::Control
            | Self::Report => namespace == Namespace::Value,
        }
    }

    pub fn namespaces(self) -> &'static [Namespace] {
        const TYPE_ONLY: &[Namespace] = &[Namespace::Type];
        const VALUE_ONLY: &[Namespace] = &[Namespace::Value];
        const ROUTINE_ONLY: &[Namespace] = &[Namespace::Routine];

        match self {
            Self::BuiltinType | Self::TypeDef | Self::Class | Self::Interface => TYPE_ONLY,
            Self::BuiltinRoutine | Self::Form | Self::Method | Self::Module | Self::Event => {
                ROUTINE_ONLY
            }
            Self::BuiltinConstant
            | Self::BuiltinVariable
            | Self::Variable
            | Self::Constant
            | Self::EnumMember
            | Self::FieldSymbol
            | Self::Parameter
            | Self::Field
            | Self::Include
            | Self::Control
            | Self::Report => VALUE_ONLY,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReferenceKind {
    Identifier,
    TypeRef,
    StructuredDeclEnd,
    MessageClass,
    RoutineCall,
    StaticTarget,
    Include,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Resolution {
    Symbol(SymbolHandle),
    BuiltinType,
    BuiltinRoutine,
    /// ABAP `table_line` pseudo-field for a scalar-like internal-table row (see hover for typical statements).
    InternalTableLine,
    External,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SqlResolution {
    Unresolved,
    External,
    LocalCte,
    InternalTable,
    Hierarchy,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SqlSourceKind {
    From,
    Join,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SqlDynamicFragmentKind {
    Source,
    Projection,
    Where,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SqlQueryData {
    pub id: usize,
    pub scope: ScopeId,
    pub range: TextRange,
    pub projection_clause: Option<TextRange>,
    pub from_clause: Option<TextRange>,
    pub into_clause: Option<TextRange>,
    pub where_clause: Option<TextRange>,
    pub group_by_clause: Option<TextRange>,
    pub having_clause: Option<TextRange>,
    pub order_by_clause: Option<TextRange>,
    pub order_by_primary_key: bool,
    pub order_by_fields: Vec<Arc<str>>,
    pub for_all_entries_clause: Option<TextRange>,
    pub for_update_clause: Option<TextRange>,
    pub up_to_clause: Option<TextRange>,
    pub package_size_clause: Option<TextRange>,
    pub offset_clause: Option<TextRange>,
    pub abap_options_clause: Option<TextRange>,
    pub set_operator_clause: Option<TextRange>,
    pub is_single: bool,
    pub is_distinct: bool,
    pub is_for_update: bool,
    pub has_package_size: bool,
    pub has_set_operators: bool,
    pub has_endselect: bool,
    pub has_dynamic_where: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SqlSourceData {
    pub query_id: usize,
    pub range: TextRange,
    pub source_kind: SqlSourceKind,
    pub name: Arc<str>,
    pub alias: Option<Arc<str>>,
    pub join_kind: Option<Arc<str>>,
    pub resolution: SqlResolution,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SqlDynamicFragmentData {
    pub query_id: usize,
    pub scope: ScopeId,
    pub range: TextRange,
    pub kind: SqlDynamicFragmentKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SqlProjectionKind {
    Star,
    QualifiedStar,
    Column,
    Aggregate,
    Expression,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SqlProjectionData {
    pub query_id: usize,
    pub range: TextRange,
    pub kind: SqlProjectionKind,
    pub source_alias: Option<Arc<str>>,
    pub name: Option<Arc<str>>,
    pub alias: Option<Arc<str>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SqlNameRefKind {
    Source,
    Alias,
    Column,
    QualifiedColumn,
    Star,
    QualifiedStar,
    Aggregate,
    Function,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SqlNameRefData {
    pub query_id: usize,
    pub scope: ScopeId,
    pub range: TextRange,
    pub name: Arc<str>,
    pub qualifier: Option<Arc<str>>,
    pub kind: SqlNameRefKind,
    pub resolution: SqlResolution,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SqlPredicateKind {
    Where,
    JoinOn,
    Having,
    DynamicWhere,
    ForAllEntries,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SqlPredicateData {
    pub query_id: usize,
    pub range: TextRange,
    pub kind: SqlPredicateKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SqlTargetKind {
    Into,
    Appending,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SqlTargetData {
    pub query_id: usize,
    pub scope: ScopeId,
    pub range: TextRange,
    pub target_range: Option<TextRange>,
    pub kind: SqlTargetKind,
    pub target_name: Option<Arc<str>>,
    pub is_table: bool,
    pub is_corresponding: bool,
    pub is_inline: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolData {
    pub id: SymbolId,
    pub name: Arc<str>,
    pub kind: SymbolKind,
    pub scope: ScopeId,
    pub decl_range: TextRange,
    pub structure: Option<StructureId>,
    /// TYPE/LIKE clause from the declaration when present (e.g. built-ins without structure metadata).
    pub declared_type: Option<FieldTypeRefData>,
    /// Verbatim type expression after `TYPE`/`LIKE` (e.g. `STANDARD TABLE OF ty`) for hover; structure
    /// metadata alone often describes only the line type for internal tables.
    pub type_clause_display: Option<Arc<str>>,
    /// Verbatim expression after a declaration `VALUE` clause, used for hover on constants and other
    /// declarations that preserve an initializer.
    pub value_clause_display: Option<Arc<str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReferenceData {
    pub id: ReferenceId,
    pub name: Arc<str>,
    pub namespace: Namespace,
    pub kind: ReferenceKind,
    pub scope: ScopeId,
    pub range: TextRange,
    pub resolution: Option<Resolution>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MessageClassUseData {
    pub name: Arc<str>,
    pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MessageUseData {
    pub range: TextRange,
    pub class_name: Option<Arc<str>>,
    pub class_range: Option<TextRange>,
    pub id: Option<Arc<str>>,
    pub id_range: Option<TextRange>,
    pub with_arg_ranges: Vec<TextRange>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MessageClassEntryData {
    pub class_name: Arc<str>,
    pub id: Arc<str>,
    pub text: Arc<str>,
    pub range: TextRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DiagnosticKind {
    DuplicateDeclaration,
    ShadowedSymbol,
    MismatchedStructuredDeclaration,
    UnresolvedReference,
    UnresolvedInclude,
    IncludeCycle,
    WrongNamespace,
    UnknownField,
    InvalidBuiltinNamedArgument,
    InvalidPerformCall,
    AbstractClassInstantiation,
    MissingMethodImplementation,
    MissingSuperConstructorCall,
    InvalidObjectTypeReference,
    InvalidParameterType,
    IncompatibleAssignmentType,
    IncompatibleArgumentType,
    InvalidConcatenateSource,
    UnknownNamedParameter,
    UnknownFunctionModuleException,
    DuplicateNamedParameter,
    MissingRequiredParameter,
    /// Open SQL `FROM` / join source not confirmed against SAP DDIC/repository (no backend lookup).
    UnverifiedOpenSqlSource,
    /// `INTO` / `APPENDING` target is incompatible with the clause (for example `INTO TABLE` on a non-table variable).
    InvalidOpenSqlIntoTarget,
    /// Open SQL statement mixes legacy and strict/new syntax forms.
    InvalidOpenSqlSyntax,
    InvalidMessage,
    InvalidConstructorForIteratorReuse,
    MissingTablesDeclaration,
    UnreachableCode,
    UseBeforeDefiniteAssignment,
    PossiblyUnboundFieldSymbol,
    DeadStore,
    UnsortedReadTableBinarySearch,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub range: TextRange,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IncludeEdge {
    pub name: Arc<str>,
    pub range: TextRange,
    pub target: Option<UnitId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TableWorkAreaData {
    pub name: Arc<str>,
    pub scope: ScopeId,
    pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldAccess {
    pub scope: ScopeId,
    pub base_namespace: Namespace,
    pub base_name: Arc<str>,
    pub base_range: TextRange,
    pub field_path: Vec<FieldAccessSegment>,
    pub in_type_position: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoopWhereFieldContext {
    pub scope: ScopeId,
    pub range: TextRange,
    pub source_access: FieldAccess,
    pub target_access: Option<FieldAccess>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstructorForBindingData {
    pub scope: ScopeId,
    pub range: TextRange,
    pub name: Arc<str>,
    pub source_access: Option<FieldAccess>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoopAtFieldContext {
    pub scope: ScopeId,
    pub range: TextRange,
    pub source_access: FieldAccess,
    pub target_access: Option<FieldAccess>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldAccessSegment {
    pub name: Arc<str>,
    pub range: TextRange,
}

impl FieldAccessSegment {
    pub fn is_deref(&self) -> bool {
        self.name.as_ref() == "*"
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldTypeRefData {
    pub namespace: Namespace,
    pub is_ref: bool,
    pub base_name: Arc<str>,
    pub field_path: Vec<Arc<str>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TypeFactData {
    pub structure: Option<StructureId>,
    pub declared_type: Option<FieldTypeRefData>,
    pub type_clause_display: Option<Arc<str>>,
    pub table_line: Option<Box<TypeFactData>>,
}

impl TypeFactData {
    pub fn is_known(&self) -> bool {
        self.structure.is_some()
            || self.declared_type.is_some()
            || self.type_clause_display.is_some()
            || self.table_line.is_some()
    }

    pub fn line_fact(&self) -> Option<&TypeFactData> {
        self.table_line.as_deref()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructureFieldData {
    pub name: Arc<str>,
    pub decl_range: Option<TextRange>,
    pub decl_unit: UnitId,
    pub structure: Option<StructureId>,
    pub type_ref: Option<FieldTypeRefData>,
    pub is_key: bool,
    pub value_clause_display: Option<Arc<str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StructureFieldShape {
    Scalar,
    Structured { structure: StructureId },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructureFieldInfo {
    pub owner: StructureId,
    pub owner_unit: UnitId,
    pub name: Arc<str>,
    pub decl_range: Option<TextRange>,
    pub decl_unit: UnitId,
    pub shape: StructureFieldShape,
    pub type_ref: Option<FieldTypeRefData>,
    pub is_key: bool,
    pub value_clause_display: Option<Arc<str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructureData {
    pub id: StructureId,
    pub origin_unit: UnitId,
    pub origin_structure: StructureId,
    pub name: Arc<str>,
    pub fields: Vec<StructureFieldData>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassMemberParameterData {
    pub section: MethodParameterSection,
    pub name: Arc<str>,
    pub range: TextRange,
    pub declared_type: Option<FieldTypeRefData>,
    pub type_clause_display: Option<Arc<str>>,
    pub is_optional: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MethodParameterSection {
    Importing,
    Exporting,
    Changing,
    Receiving,
    Returning,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormParameterSection {
    Tables,
    Using,
    Changing,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormParameterPassingKind {
    Direct,
    Value,
    Reference,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormParameterData {
    pub symbol: SymbolId,
    pub section: FormParameterSection,
    pub passing: FormParameterPassingKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormRoutineData {
    pub symbol: SymbolId,
    pub signature: Arc<str>,
    pub parameters: Vec<FormParameterData>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionModuleParameterSection {
    Importing,
    Exporting,
    Changing,
    Tables,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionModuleParameterData {
    pub section: FunctionModuleParameterSection,
    pub name: Arc<str>,
    pub range: TextRange,
    pub declared_type: Option<FieldTypeRefData>,
    pub type_clause_display: Option<Arc<str>>,
    pub is_untyped: bool,
    pub is_optional: bool,
    pub has_default_value: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionModuleExceptionData {
    pub name: Arc<str>,
    pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionModuleData {
    pub symbol: SymbolId,
    pub signature: Arc<str>,
    pub parameters: Vec<FunctionModuleParameterData>,
    pub exceptions: Vec<FunctionModuleExceptionData>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassMemberImplementationData {
    pub unit: UnitId,
    pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassMemberData {
    pub class_symbol: SymbolId,
    pub name: Arc<str>,
    pub kind: ClassMemberKind,
    pub visibility: Visibility,
    pub is_static: bool,
    pub decl_range: TextRange,
    pub implementation_range: Option<TextRange>,
    pub implementation: Option<ClassMemberImplementationData>,
    pub signature: Arc<str>,
    pub parameters: Vec<ClassMemberParameterData>,
    /// Shape for grouped `CONSTANTS` / `CLASS-DATA` `BEGIN OF ... END OF` when the member is a structure.
    pub structure: Option<StructureId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassInheritanceData {
    pub class_symbol: SymbolId,
    pub superclass_name: Arc<str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassDefinitionData {
    pub class_symbol: SymbolId,
    pub is_abstract: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplementedInterfaceData {
    pub owner_symbol: SymbolId,
    pub interface_name: Arc<str>,
    pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemberAliasData {
    pub owner_symbol: SymbolId,
    pub alias_name: Arc<str>,
    pub target_interface_name: Arc<str>,
    pub target_member_name: Arc<str>,
    pub range: TextRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NamedArgumentSection {
    Exporting,
    Importing,
    Changing,
    Tables,
    Receiving,
    Exceptions,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NamedArgumentTarget {
    Constructor {
        type_name: Arc<str>,
    },
    Function {
        function_name: Arc<str>,
    },
    Report {
        report_name: Arc<str>,
    },
    Routine {
        routine_name: Arc<str>,
    },
    ImplicitMethod {
        method_name: Arc<str>,
    },
    Method {
        base_namespace: Namespace,
        base_name: Arc<str>,
        method_name: Arc<str>,
        interface_qualified: bool,
    },
    Event {
        qualifier: Option<Arc<str>>,
        event_name: Arc<str>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamedArgumentAccess {
    pub scope: ScopeId,
    pub name: Arc<str>,
    pub range: TextRange,
    pub section: Option<NamedArgumentSection>,
    pub target: NamedArgumentTarget,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallArgumentData {
    pub range: TextRange,
    pub name: Option<Arc<str>>,
    pub section: Option<NamedArgumentSection>,
    pub ordinal: usize,
    pub type_fact: TypeFactData,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallSiteData {
    pub scope: ScopeId,
    pub range: TextRange,
    pub target: NamedArgumentTarget,
    pub arguments: Vec<CallArgumentData>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssignmentSiteData {
    pub scope: ScopeId,
    pub range: TextRange,
    pub lhs_range: TextRange,
    pub rhs_range: TextRange,
    pub lhs_target_access: Option<FieldAccess>,
    pub lhs: TypeFactData,
    pub rhs: TypeFactData,
    pub rhs_is_top_level_sum: bool,
    pub assigns_table_line: bool,
    pub is_corresponding: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConcatenateLinesOfSiteData {
    pub scope: ScopeId,
    pub range: TextRange,
    pub source_range: TextRange,
    pub source: TypeFactData,
    pub byte_mode: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpressionFactKind {
    Reference,
    Selector,
    CallResult,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpressionFactData {
    pub scope: ScopeId,
    pub range: TextRange,
    pub kind: ExpressionFactKind,
    pub type_fact: TypeFactData,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueFlowKind {
    Assignment,
    CallArgument,
    FieldSymbolAssignment,
    ConditionalFieldSymbolAssignment,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueFlowTargetData {
    Assignment {
        range: TextRange,
    },
    CallParameter {
        call_range: TextRange,
        target: NamedArgumentTarget,
        parameter_name: Option<Arc<str>>,
        parameter_decl_unit: Option<UnitId>,
        parameter_decl_range: Option<TextRange>,
    },
    FieldSymbol {
        range: TextRange,
        name: Option<Arc<str>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueFlowEdgeData {
    pub scope: ScopeId,
    pub kind: ValueFlowKind,
    pub source_range: TextRange,
    pub source_type: TypeFactData,
    pub target: ValueFlowTargetData,
    pub target_type: TypeFactData,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PerformParameterSection {
    Tables,
    Using,
    Changing,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PerformArgumentData {
    pub range: TextRange,
    pub section: PerformParameterSection,
    pub ordinal_in_section: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PerformProgramData {
    pub name: Arc<str>,
    pub range: TextRange,
    pub is_dynamic: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PerformCallData {
    pub scope: ScopeId,
    pub range: TextRange,
    pub routine_name: Arc<str>,
    pub routine_range: TextRange,
    pub is_dynamic: bool,
    pub program: Option<PerformProgramData>,
    pub has_if_found: bool,
    pub parameters: Vec<PerformParameterSection>,
    pub arguments: Vec<PerformArgumentData>,
    pub section_order_invalid: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RoutineLoopKind {
    While,
    Do,
    Loop,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RoutineSiteKind {
    UnknownEffect,
    Clear,
    Unassign,
    Delete,
    ReadTable,
    Return,
    Raise,
    Leave,
    LeaveListProcessing,
    Exit,
    Continue,
    Stop,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RoutineSiteData {
    pub scope: ScopeId,
    pub range: TextRange,
    pub kind: RoutineSiteKind,
    pub target_range: Option<TextRange>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InternalTableOrderData {
    pub scope: ScopeId,
    pub range: TextRange,
    pub table_name: Arc<str>,
    pub key_fields: Vec<Arc<str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReadTableBinarySearchData {
    pub scope: ScopeId,
    pub range: TextRange,
    pub table_name: Arc<str>,
    pub key_fields: Vec<Arc<str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FindWriteTargetData {
    pub range: TextRange,
    pub definitely_assigned: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FindSiteData {
    pub scope: ScopeId,
    pub range: TextRange,
    pub read_ranges: Vec<TextRange>,
    pub write_targets: Vec<FindWriteTargetData>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SystemFieldStatementKind {
    Append,
    Assign,
    AuthorityCheck,
    CallFunction,
    Convert,
    DeleteReport,
    DeleteTable,
    DeleteDbTable,
    DescribeTable,
    Do,
    Find,
    InsertReport,
    InsertTable,
    InsertDbTable,
    InsertTextpool,
    LoopAt,
    Message,
    ModifyTable,
    ModifyDbTable,
    ReadReport,
    ReadTable,
    Search,
    Select,
    SyntaxCheck,
    UpdateDbTable,
    While,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SystemFieldUpdateData {
    pub scope: ScopeId,
    pub range: TextRange,
    pub statement: SystemFieldStatementKind,
    pub field_name: Arc<str>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldSymbolStateCheckKind {
    IsAssigned,
    IsNotAssigned,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueStateCheckKind {
    IsInitial,
    IsNotInitial,
    EqualsZero,
    NotEqualsZero,
    ConditionProbe,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldSymbolStateCheckData {
    pub scope: ScopeId,
    pub range: TextRange,
    pub symbol_name: Arc<str>,
    pub symbol_range: TextRange,
    pub kind: FieldSymbolStateCheckKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueStateCheckData {
    pub scope: ScopeId,
    pub range: TextRange,
    pub symbol_name: Arc<str>,
    pub symbol_range: TextRange,
    pub field_name: Option<Arc<str>>,
    pub kind: ValueStateCheckKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfRegionData {
    pub scope: ScopeId,
    pub range: TextRange,
    pub then_scope: ScopeId,
    pub elseif_scopes: Vec<ScopeId>,
    pub else_scope: Option<ScopeId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaseRegionData {
    pub scope: ScopeId,
    pub range: TextRange,
    pub when_scopes: Vec<ScopeId>,
    pub has_when_others: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoopRegionData {
    pub scope: ScopeId,
    pub range: TextRange,
    pub kind: RoutineLoopKind,
    pub body_scope: ScopeId,
    pub source_access: Option<FieldAccess>,
    pub target_access: Option<FieldAccess>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AtGroupKind {
    First,
    New,
    EndOf,
    Last,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AtRegionData {
    pub scope: ScopeId,
    pub range: TextRange,
    pub kind: AtGroupKind,
    pub body_scope: ScopeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TryRegionData {
    pub scope: ScopeId,
    pub range: TextRange,
    pub body_scope: ScopeId,
    pub catch_scopes: Vec<ScopeId>,
    pub cleanup_scope: Option<ScopeId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RoutineControlRegionData {
    If(IfRegionData),
    Case(CaseRegionData),
    Loop(LoopRegionData),
    At(AtRegionData),
    Try(TryRegionData),
}

impl RoutineControlRegionData {
    pub fn scope(&self) -> ScopeId {
        match self {
            Self::If(data) => data.scope,
            Self::Case(data) => data.scope,
            Self::Loop(data) => data.scope,
            Self::At(data) => data.scope,
            Self::Try(data) => data.scope,
        }
    }

    pub fn range(&self) -> &TextRange {
        match self {
            Self::If(data) => &data.range,
            Self::Case(data) => &data.range,
            Self::Loop(data) => &data.range,
            Self::At(data) => &data.range,
            Self::Try(data) => &data.range,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnitAnalysis {
    pub unit_id: UnitId,
    pub uri: Arc<str>,
    pub root_scope: ScopeId,
    pub scopes: Vec<ScopeData>,
    pub symbols: Vec<SymbolData>,
    pub structures: Vec<StructureData>,
    pub references: Vec<ReferenceData>,
    pub message_default_class: Option<MessageClassUseData>,
    pub message_uses: Vec<MessageUseData>,
    pub message_class_entries: Vec<MessageClassEntryData>,
    pub diagnostics: Vec<Diagnostic>,
    pub include_edges: Vec<IncludeEdge>,
    pub table_work_areas: Vec<TableWorkAreaData>,
    pub selection_screen_report_type_positions: Vec<TextRange>,
    pub field_accesses: Vec<FieldAccess>,
    pub loop_where_field_contexts: Vec<LoopWhereFieldContext>,
    pub loop_at_field_contexts: Vec<LoopAtFieldContext>,
    pub constructor_for_bindings: Vec<ConstructorForBindingData>,
    pub class_members: Vec<ClassMemberData>,
    pub class_definitions: Vec<ClassDefinitionData>,
    pub class_inheritance: Vec<ClassInheritanceData>,
    pub implemented_interfaces: Vec<ImplementedInterfaceData>,
    pub member_aliases: Vec<MemberAliasData>,
    pub form_routines: Vec<FormRoutineData>,
    pub function_modules: Vec<FunctionModuleData>,
    pub named_arguments: Vec<NamedArgumentAccess>,
    pub call_sites: Vec<CallSiteData>,
    pub assignment_sites: Vec<AssignmentSiteData>,
    pub concatenate_lines_of_sites: Vec<ConcatenateLinesOfSiteData>,
    pub expression_facts: Vec<ExpressionFactData>,
    pub value_flow_edges: Vec<ValueFlowEdgeData>,
    pub perform_calls: Vec<PerformCallData>,
    pub find_sites: Vec<FindSiteData>,
    pub system_field_updates: Vec<SystemFieldUpdateData>,
    pub routine_sites: Vec<RoutineSiteData>,
    pub internal_table_orders: Vec<InternalTableOrderData>,
    pub read_table_binary_searches: Vec<ReadTableBinarySearchData>,
    pub field_symbol_state_checks: Vec<FieldSymbolStateCheckData>,
    pub value_state_checks: Vec<ValueStateCheckData>,
    pub routine_control_regions: Vec<RoutineControlRegionData>,
    pub sql_queries: Vec<SqlQueryData>,
    pub sql_sources: Vec<SqlSourceData>,
    pub sql_dynamic_fragments: Vec<SqlDynamicFragmentData>,
    pub sql_projections: Vec<SqlProjectionData>,
    pub sql_name_refs: Vec<SqlNameRefData>,
    pub sql_predicates: Vec<SqlPredicateData>,
    pub sql_targets: Vec<SqlTargetData>,
    pub provided_names: Vec<Arc<str>>,
    pub(crate) semantic_index: Arc<SemanticIndex>,
}

impl UnitAnalysis {
    pub(crate) fn with_semantic_index(mut self) -> Self {
        self.semantic_index = Arc::new(SemanticIndex::build(&self));
        self
    }

    pub fn rebuild_semantic_index(&mut self) {
        self.semantic_index = Arc::new(SemanticIndex::build(self));
    }

    pub fn symbol(&self, id: SymbolId) -> &SymbolData {
        &self.symbols[id.as_usize()]
    }

    pub fn structure(&self, id: StructureId) -> &StructureData {
        &self.structures[id.as_usize()]
    }

    pub fn scope(&self, id: ScopeId) -> &ScopeData {
        self.scopes
            .get(id.as_usize())
            .or_else(|| self.scopes.get(self.root_scope.as_usize()))
            .expect("unit analysis should always contain its root scope")
    }

    pub fn semantic(&self) -> SemanticQueries<'_> {
        SemanticQueries::new(self)
    }

    pub(crate) fn symbol_at_offset(&self, offset: usize) -> Option<&SymbolData> {
        let semantic_id = self.semantic_index.symbol_at_offset(offset)?;
        let symbol = self.semantic_index.symbol(semantic_id);
        self.symbols.get(symbol.symbol_id.as_usize())
    }

    pub(crate) fn class_member_at_offset(&self, offset: usize) -> Option<&ClassMemberData> {
        let semantic_id = self.semantic_index.class_member_at_offset(offset)?;
        let member = self.semantic_index.class_member(semantic_id);
        self.class_members.get(member.raw_index)
    }

    pub fn class_definition(&self, class_symbol: SymbolId) -> Option<&ClassDefinitionData> {
        self.class_definitions
            .iter()
            .find(|definition| definition.class_symbol == class_symbol)
    }

    pub fn class_is_abstract(&self, class_symbol: SymbolId) -> bool {
        self.class_definition(class_symbol)
            .is_some_and(|definition| definition.is_abstract)
    }

    pub(crate) fn symbol_with_kind_and_decl_range(
        &self,
        kind: SymbolKind,
        range: &TextRange,
    ) -> Option<&SymbolData> {
        let semantic_id = self
            .semantic_index
            .symbol_with_kind_and_decl_range(kind, range)?;
        let symbol = self.semantic_index.symbol(semantic_id);
        self.symbols.get(symbol.symbol_id.as_usize())
    }

    pub(crate) fn structure_field_at_offset(&self, offset: usize) -> Option<StructureFieldInfo> {
        let semantic_id = self.semantic_index.structure_field_at_offset(offset)?;
        let field = self.semantic_index.structure_field(semantic_id);
        let info = self.structure_field_info(field.structure_id, field.name.as_str())?;
        (info.decl_range.as_ref() == Some(&field.decl_range)).then_some(info)
    }

    pub(crate) fn sql_name_ref_at_offset(&self, offset: usize) -> Option<&SqlNameRefData> {
        let semantic_id = self.semantic_index.sql_name_ref_at_offset(offset)?;
        let sql_ref = self.semantic_index.sql_name_ref(semantic_id);
        self.sql_name_refs.get(sql_ref.raw_index)
    }

    pub(crate) fn reference_at_offset(&self, offset: usize) -> Option<&ReferenceData> {
        let semantic_id = self.semantic_index.reference_at_offset(offset)?;
        let reference = self.semantic_index.reference(semantic_id);
        self.references.get(reference.reference_id.as_usize())
    }

    pub(crate) fn reference_at_range(&self, range: &TextRange) -> Option<&ReferenceData> {
        let semantic_id = self.semantic_index.reference_at_range(range)?;
        let reference = self.semantic_index.reference(semantic_id);
        self.references.get(reference.reference_id.as_usize())
    }

    pub(crate) fn type_reference_at_offset(&self, offset: usize) -> Option<&ReferenceData> {
        let reference = self.reference_at_offset(offset)?;
        (reference.kind == ReferenceKind::TypeRef).then_some(reference)
    }

    pub(crate) fn references_resolving_to(
        &self,
        handle: SymbolHandle,
    ) -> impl Iterator<Item = &ReferenceData> + '_ {
        self.semantic_index
            .references_resolving_to(handle)
            .filter_map(|semantic_id| {
                let reference = self.semantic_index.reference(semantic_id);
                self.references.get(reference.reference_id.as_usize())
            })
    }

    pub(crate) fn type_references_named(
        &self,
        name: &str,
    ) -> impl Iterator<Item = &ReferenceData> + '_ {
        self.semantic_index
            .type_references_named(name)
            .filter_map(|semantic_id| {
                let reference = self.semantic_index.reference(semantic_id);
                self.references.get(reference.reference_id.as_usize())
            })
    }

    pub(crate) fn references_in_scope(
        &self,
        scope: ScopeId,
    ) -> impl Iterator<Item = &ReferenceData> + '_ {
        self.semantic_index
            .references_in_scope(scope)
            .filter_map(|semantic_id| {
                let reference = self.semantic_index.reference(semantic_id);
                self.references.get(reference.reference_id.as_usize())
            })
    }

    pub(crate) fn expression_fact_at_offset(&self, offset: usize) -> Option<&ExpressionFactData> {
        self.expression_facts
            .iter()
            .filter(|fact| fact.range.start <= offset && offset < fact.range.end)
            .min_by_key(|fact| {
                (
                    expression_fact_priority(fact.kind),
                    fact.range.end.saturating_sub(fact.range.start),
                )
            })
    }

    pub(crate) fn value_flow_edges_touching_offset(
        &self,
        offset: usize,
    ) -> impl Iterator<Item = &ValueFlowEdgeData> + '_ {
        self.value_flow_edges.iter().filter(move |edge| {
            edge.source_range.start <= offset && offset < edge.source_range.end
                || match &edge.target {
                    ValueFlowTargetData::Assignment { range }
                    | ValueFlowTargetData::FieldSymbol { range, .. } => {
                        range.start <= offset && offset < range.end
                    }
                    ValueFlowTargetData::CallParameter {
                        parameter_decl_range,
                        ..
                    } => parameter_decl_range
                        .as_ref()
                        .is_some_and(|range| range.start <= offset && offset < range.end),
                }
        })
    }

    pub(crate) fn sql_source_name_refs_named(
        &self,
        name: &str,
    ) -> impl Iterator<Item = &SqlNameRefData> + '_ {
        self.semantic_index
            .sql_source_name_refs_named(name)
            .filter_map(|semantic_id| {
                let sql_ref = self.semantic_index.sql_name_ref(semantic_id);
                self.sql_name_refs.get(sql_ref.raw_index)
            })
    }

    pub(crate) fn has_sql_source_named(&self, name: &str) -> bool {
        self.semantic_index.has_sql_source_named(name)
    }

    pub fn structure_field(
        &self,
        structure_id: StructureId,
        field_name: &str,
    ) -> Option<&StructureFieldData> {
        self.structure(structure_id)
            .fields
            .iter()
            .find(|field| field.name.as_ref() == field_name)
    }

    pub fn structure_field_info(
        &self,
        structure_id: StructureId,
        field_name: &str,
    ) -> Option<StructureFieldInfo> {
        let field = self.structure_field(structure_id, field_name)?;
        Some(StructureFieldInfo {
            owner: structure_id,
            owner_unit: self.structure(structure_id).origin_unit,
            name: Arc::clone(&field.name),
            decl_range: field.decl_range.clone(),
            decl_unit: field.decl_unit,
            shape: match field.structure {
                Some(structure) => StructureFieldShape::Structured { structure },
                None => StructureFieldShape::Scalar,
            },
            type_ref: field.type_ref.clone(),
            is_key: field.is_key,
            value_clause_display: field.value_clause_display.clone(),
        })
    }

    pub fn structure_field_infos(&self, structure_id: StructureId) -> Vec<StructureFieldInfo> {
        self.structure(structure_id)
            .fields
            .iter()
            .map(|field| StructureFieldInfo {
                owner: structure_id,
                owner_unit: self.structure(structure_id).origin_unit,
                name: Arc::clone(&field.name),
                decl_range: field.decl_range.clone(),
                decl_unit: field.decl_unit,
                shape: match field.structure {
                    Some(structure) => StructureFieldShape::Structured { structure },
                    None => StructureFieldShape::Scalar,
                },
                type_ref: field.type_ref.clone(),
                is_key: field.is_key,
                value_clause_display: field.value_clause_display.clone(),
            })
            .collect()
    }

    pub fn resolve_structure_field_path(
        &self,
        structure_id: StructureId,
        field_path: &[&str],
    ) -> Option<StructureFieldInfo> {
        let mut current_structure = structure_id;
        let mut current_info = None;
        for field_name in field_path {
            let info = self.structure_field_info(current_structure, field_name)?;
            current_structure = match info.shape {
                StructureFieldShape::Structured { structure } => structure,
                StructureFieldShape::Scalar if *field_name == *field_path.last()? => {
                    current_structure
                }
                StructureFieldShape::Scalar => return None,
            };
            current_info = Some(info);
        }
        current_info
    }

    pub fn class_member(&self, class_symbol: SymbolId, name: &str) -> Option<&ClassMemberData> {
        self.class_members
            .iter()
            .find(|member| member.class_symbol == class_symbol && member.name.as_ref() == name)
    }

    pub fn class_members_for(
        &self,
        class_symbol: SymbolId,
    ) -> impl Iterator<Item = &ClassMemberData> + '_ {
        self.class_members
            .iter()
            .filter(move |member| member.class_symbol == class_symbol)
    }

    pub fn class_superclass(&self, class_symbol: SymbolId) -> Option<&ClassInheritanceData> {
        self.class_inheritance
            .iter()
            .find(|inheritance| inheritance.class_symbol == class_symbol)
    }

    pub fn form_routine(&self, symbol: SymbolId) -> Option<&FormRoutineData> {
        self.form_routines
            .iter()
            .find(|routine| routine.symbol == symbol)
    }

    pub fn form_parameter(&self, symbol: SymbolId) -> Option<&FormParameterData> {
        self.form_routines
            .iter()
            .flat_map(|routine| routine.parameters.iter())
            .find(|parameter| parameter.symbol == symbol)
    }

    pub fn function_module(&self, symbol: SymbolId) -> Option<&FunctionModuleData> {
        self.function_modules
            .iter()
            .find(|function_module| function_module.symbol == symbol)
    }

    pub fn routine_parameters(
        &self,
        routine_symbol: SymbolId,
    ) -> impl Iterator<Item = &SymbolData> + '_ {
        self.scopes
            .iter()
            .filter(move |scope| scope.owner == Some(routine_symbol))
            .flat_map(|scope| scope.declarations.iter().copied())
            .map(|symbol_id| self.symbol(symbol_id))
            .filter(|symbol| symbol.kind == SymbolKind::Parameter)
    }
}

fn expression_fact_priority(kind: ExpressionFactKind) -> u8 {
    match kind {
        ExpressionFactKind::CallResult => 0,
        ExpressionFactKind::Selector => 1,
        ExpressionFactKind::Reference => 2,
    }
}
