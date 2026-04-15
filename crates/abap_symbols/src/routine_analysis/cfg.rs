use abap_lexer::TextRange;

use super::ids::{RoutineBlockId, RoutineInstrId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RoutineBlockKind {
    Entry,
    Body,
    Exit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RoutineEdgeKind {
    SyntheticFlow,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RoutineBlock {
    pub id: RoutineBlockId,
    pub kind: RoutineBlockKind,
    pub range: TextRange,
    pub instructions: Vec<RoutineInstrId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RoutineEdge {
    pub from: RoutineBlockId,
    pub to: RoutineBlockId,
    pub kind: RoutineEdgeKind,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RoutineCfg {
    pub entry: Option<RoutineBlockId>,
    pub exit: Option<RoutineBlockId>,
    pub blocks: Vec<RoutineBlock>,
    pub edges: Vec<RoutineEdge>,
}
