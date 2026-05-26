use std::sync::Arc;

use crate::SymbolHandle;

use super::ids::{DataflowValueId, RoutineBlockId, RoutineInstrId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DataflowValueKind {
    Parameter,
    Variable,
    FieldSymbol,
    Constant,
    Other,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RoutineDataflowValue {
    pub id: DataflowValueId,
    pub symbol: SymbolHandle,
    pub name: Arc<str>,
    pub kind: DataflowValueKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstructionDataflowSummary {
    pub instruction: RoutineInstrId,
    pub reads: Vec<DataflowValueId>,
    pub writes: Vec<DataflowValueId>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RoutineDataflowInputs {
    pub values: Vec<RoutineDataflowValue>,
    pub instructions: Vec<InstructionDataflowSummary>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockDataflowSummary {
    pub block: RoutineBlockId,
    pub maybe_written_values: Vec<DataflowValueId>,
    pub definitely_assigned_values: Vec<DataflowValueId>,
    pub definitely_bound_field_symbols: Vec<DataflowValueId>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RoutineDataflowResult {
    pub converged: bool,
    pub iterations: u32,
    pub block_entry: Vec<BlockDataflowSummary>,
    pub block_exit: Vec<BlockDataflowSummary>,
}
