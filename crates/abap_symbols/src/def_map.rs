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

impl SymbolKind {
    pub const fn is_builtin(self) -> bool {
        matches!(
            self,
            Self::BuiltinType | Self::BuiltinRoutine | Self::BuiltinConstant | Self::BuiltinVariable
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolData {
    pub id: SymbolId,
    pub name: Arc<str>,
    pub kind: SymbolKind,
    pub scope: ScopeId,
    pub decl_range: TextRange,
    pub structure: Option<StructureId>,
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
    pub field_name: Arc<str>,
    pub range: TextRange,
    pub in_type_position: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructureFieldData {
    pub name: Arc<str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructureData {
    pub id: StructureId,
    pub name: Arc<str>,
    pub fields: Vec<StructureFieldData>,
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
}
