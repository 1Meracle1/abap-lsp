#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ProjectRoutineAnalysisMetrics {
    pub routine_count: usize,
    pub instruction_count: usize,
    pub block_count: usize,
    pub tracked_value_count: usize,
    pub index_micros: u128,
    pub ir_micros: u128,
    pub cfg_micros: u128,
    pub dataflow_micros: u128,
    pub dead_store_micros: u128,
    pub total_micros: u128,
}
