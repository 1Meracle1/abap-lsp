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
    /// TYPE/LIKE clause from the declaration when present (e.g. built-ins without structure metadata).
    pub declared_type: Option<FieldTypeRefData>,
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
    pub field_path: Vec<FieldAccessSegment>,
    pub in_type_position: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldAccessSegment {
    pub name: Arc<str>,
    pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldTypeRefData {
    pub namespace: Namespace,
    pub base_name: Arc<str>,
    pub field_path: Vec<Arc<str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructureFieldData {
    pub name: Arc<str>,
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

    pub fn structure_field(&self, structure_id: StructureId, field_name: &str) -> Option<&StructureFieldData> {
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
                StructureFieldShape::Scalar if *field_name == *field_path.last()? => current_structure,
                StructureFieldShape::Scalar => return None,
            };
            current_info = Some(info);
        }
        current_info
    }
}
