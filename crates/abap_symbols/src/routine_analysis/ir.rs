use std::sync::Arc;

use abap_lexer::TextRange;

use crate::ReferenceId;
use crate::def_map::RoutineLoopKind;
use crate::ids::{ScopeId, SymbolHandle, UnitId};

use super::ids::{RoutineId, RoutineInstrId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RoutineKind {
    GlobalDeclarations,
    Method,
    Form,
    Module,
    EventBlock,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RoutineBranchKind {
    If,
    Case,
    At,
    Try,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RoutineTerminatorKind {
    Return,
    Raise,
    Leave,
    LeaveListProcessing,
    Exit,
    Continue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RoutineDescriptor {
    pub id: RoutineId,
    pub unit: UnitId,
    pub scope: ScopeId,
    pub kind: RoutineKind,
    pub owner: Option<SymbolHandle>,
    pub name: Arc<str>,
    pub decl_range: TextRange,
    pub scope_range: TextRange,
    pub executable_range: Option<TextRange>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RoutineInstructionKind {
    Assignment,
    Call,
    Perform,
    SqlQuery,
    Clear,
    Delete,
    ReadTable,
    Find,
    FieldSymbolBind,
    ValueRead,
    UnknownEffect,
    Branch,
    LoopHeader,
    Terminator,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RoutineInstructionSite {
    Assignment { index: u32 },
    Call { index: u32 },
    Perform { index: u32 },
    SqlQuery { index: u32 },
    Clear { index: u32 },
    Delete { index: u32 },
    ReadTable { index: u32 },
    Find { index: u32 },
    FieldSymbolBind { index: u32 },
    ValueRead { reference: ReferenceId },
    UnknownEffect,
    Branch { kind: RoutineBranchKind },
    LoopHeader { kind: RoutineLoopKind },
    Terminator { kind: RoutineTerminatorKind },
}

impl RoutineInstructionSite {
    pub const fn kind(self) -> RoutineInstructionKind {
        match self {
            Self::Assignment { .. } => RoutineInstructionKind::Assignment,
            Self::Call { .. } => RoutineInstructionKind::Call,
            Self::Perform { .. } => RoutineInstructionKind::Perform,
            Self::SqlQuery { .. } => RoutineInstructionKind::SqlQuery,
            Self::Clear { .. } => RoutineInstructionKind::Clear,
            Self::Delete { .. } => RoutineInstructionKind::Delete,
            Self::ReadTable { .. } => RoutineInstructionKind::ReadTable,
            Self::Find { .. } => RoutineInstructionKind::Find,
            Self::FieldSymbolBind { .. } => RoutineInstructionKind::FieldSymbolBind,
            Self::ValueRead { .. } => RoutineInstructionKind::ValueRead,
            Self::UnknownEffect => RoutineInstructionKind::UnknownEffect,
            Self::Branch { .. } => RoutineInstructionKind::Branch,
            Self::LoopHeader { .. } => RoutineInstructionKind::LoopHeader,
            Self::Terminator { .. } => RoutineInstructionKind::Terminator,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RoutineInstruction {
    pub id: RoutineInstrId,
    pub scope: ScopeId,
    pub range: TextRange,
    pub site: RoutineInstructionSite,
}

impl RoutineInstruction {
    pub const fn kind(&self) -> RoutineInstructionKind {
        self.site.kind()
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RoutineIr {
    pub instructions: Vec<RoutineInstruction>,
}
