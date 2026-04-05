use std::sync::Arc;

use abap_lexer::TextRange;

use crate::ids::{ReferenceId, ScopeId, StructureId, SymbolHandle, SymbolId, UnitId};
use crate::scope::{Namespace, ScopeData};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    BuiltinType,
    BuiltinRoutine,
    BuiltinConstant,
    BuiltinVariable,
    Variable,
    Constant,
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
    RoutineCall,
    StaticTarget,
    Include,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Resolution {
    Symbol(SymbolHandle),
    BuiltinType,
    BuiltinRoutine,
    External,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SqlResolution {
    Unresolved,
    External,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SqlSourceKind {
    From,
    Join,
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
    pub for_all_entries_clause: Option<TextRange>,
    pub up_to_clause: Option<TextRange>,
    pub is_single: bool,
    pub is_distinct: bool,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticKind {
    DuplicateDeclaration,
    ShadowedSymbol,
    UnresolvedReference,
    UnresolvedInclude,
    IncludeCycle,
    WrongNamespace,
    UnknownField,
    InvalidBuiltinNamedArgument,
    InvalidPerformCall,
    MissingSuperConstructorCall,
    /// Open SQL `FROM` / join source not confirmed against SAP DDIC/repository (no backend lookup).
    UnverifiedOpenSqlSource,
    /// `INTO` / `APPENDING` target is incompatible with the clause (for example `INTO TABLE` on a non-table variable).
    InvalidOpenSqlIntoTarget,
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
pub struct FieldAccess {
    pub scope: ScopeId,
    pub base_namespace: Namespace,
    pub base_name: Arc<str>,
    pub field_path: Vec<FieldAccessSegment>,
    pub in_type_position: bool,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructureFieldData {
    pub name: Arc<str>,
    pub decl_range: Option<TextRange>,
    pub structure: Option<StructureId>,
    pub type_ref: Option<FieldTypeRefData>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StructureFieldShape {
    Scalar,
    Structured { structure: StructureId },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructureFieldInfo {
    pub owner: StructureId,
    pub name: Arc<str>,
    pub decl_range: Option<TextRange>,
    pub shape: StructureFieldShape,
    pub type_ref: Option<FieldTypeRefData>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructureData {
    pub id: StructureId,
    pub name: Arc<str>,
    pub fields: Vec<StructureFieldData>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassMemberParameterData {
    pub name: Arc<str>,
    pub range: TextRange,
    pub declared_type: Option<FieldTypeRefData>,
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
    pub parameters: Vec<FormParameterData>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassMemberData {
    pub class_symbol: SymbolId,
    pub name: Arc<str>,
    pub kind: ClassMemberKind,
    pub visibility: Visibility,
    pub is_static: bool,
    pub decl_range: TextRange,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NamedArgumentSection {
    Exporting,
    Importing,
    Changing,
    Receiving,
    Exceptions,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NamedArgumentTarget {
    Constructor {
        type_name: Arc<str>,
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
pub struct PerformCallData {
    pub scope: ScopeId,
    pub range: TextRange,
    pub routine_name: Arc<str>,
    pub routine_range: TextRange,
    pub parameters: Vec<PerformParameterSection>,
    pub arguments: Vec<PerformArgumentData>,
    pub section_order_invalid: bool,
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
    pub diagnostics: Vec<Diagnostic>,
    pub include_edges: Vec<IncludeEdge>,
    pub field_accesses: Vec<FieldAccess>,
    pub class_members: Vec<ClassMemberData>,
    pub class_inheritance: Vec<ClassInheritanceData>,
    pub form_routines: Vec<FormRoutineData>,
    pub named_arguments: Vec<NamedArgumentAccess>,
    pub perform_calls: Vec<PerformCallData>,
    pub sql_queries: Vec<SqlQueryData>,
    pub sql_sources: Vec<SqlSourceData>,
    pub sql_projections: Vec<SqlProjectionData>,
    pub sql_name_refs: Vec<SqlNameRefData>,
    pub sql_predicates: Vec<SqlPredicateData>,
    pub sql_targets: Vec<SqlTargetData>,
    pub provided_names: Vec<Arc<str>>,
}

impl UnitAnalysis {
    pub fn symbol(&self, id: SymbolId) -> &SymbolData {
        &self.symbols[id.as_usize()]
    }

    pub fn structure(&self, id: StructureId) -> &StructureData {
        &self.structures[id.as_usize()]
    }

    pub fn scope(&self, id: ScopeId) -> &ScopeData {
        &self.scopes[id.as_usize()]
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
            name: Arc::clone(&field.name),
            decl_range: field.decl_range.clone(),
            shape: match field.structure {
                Some(structure) => StructureFieldShape::Structured { structure },
                None => StructureFieldShape::Scalar,
            },
            type_ref: field.type_ref.clone(),
        })
    }

    pub fn structure_field_infos(&self, structure_id: StructureId) -> Vec<StructureFieldInfo> {
        self.structure(structure_id)
            .fields
            .iter()
            .map(|field| StructureFieldInfo {
                owner: structure_id,
                name: Arc::clone(&field.name),
                decl_range: field.decl_range.clone(),
                shape: match field.structure {
                    Some(structure) => StructureFieldShape::Structured { structure },
                    None => StructureFieldShape::Scalar,
                },
                type_ref: field.type_ref.clone(),
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
