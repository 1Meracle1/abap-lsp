mod cfg;
mod dataflow;
mod ids;
mod ir;
mod metrics;

use std::collections::{HashMap, VecDeque};
use std::sync::Arc;

use abap_lexer::TextRange;

pub use cfg::{RoutineBlock, RoutineBlockKind, RoutineCfg, RoutineEdge, RoutineEdgeKind};
pub use dataflow::{
    BlockDataflowSummary, DataflowValueKind, InstructionDataflowSummary, RoutineDataflowInputs,
    RoutineDataflowResult, RoutineDataflowValue,
};
pub use ids::{DataflowValueId, RoutineBlockId, RoutineId, RoutineInstrId};
pub use ir::{
    RoutineBranchKind, RoutineDescriptor, RoutineInstruction, RoutineInstructionKind,
    RoutineInstructionSite, RoutineIr, RoutineKind, RoutineTerminatorKind,
};
pub use metrics::ProjectRoutineAnalysisMetrics;

use crate::builtin_routine_spec;
use crate::def_map::{
    CaseRegionData, Diagnostic, DiagnosticKind, FieldSymbolStateCheckKind,
    FunctionModuleParameterSection, IfRegionData, LoopRegionData, MethodParameterSection,
    Resolution, RoutineControlRegionData, RoutineSiteKind, SymbolData, SymbolKind, TryRegionData,
    UnitAnalysis, ValueFlowKind, ValueFlowTargetData, ValueStateCheckKind,
};
use crate::ids::{ScopeId, StructureId, SymbolHandle, UnitId};
use crate::project::ProjectAnalysis;
use crate::scope::{Namespace, ScopeKind};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ProjectRoutineAnalysis {
    pub routines: Vec<RoutineAnalysis>,
    pub metrics: ProjectRoutineAnalysisMetrics,
    owner_to_routine: HashMap<SymbolHandle, RoutineId>,
    unit_routines: Vec<Vec<RoutineId>>,
    scope_to_routine: Vec<Vec<Option<RoutineId>>>,
    unit_diagnostics: Vec<Vec<Diagnostic>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RoutineAnalysis {
    pub descriptor: RoutineDescriptor,
    pub ir: RoutineIr,
    pub cfg: RoutineCfg,
    pub diagnostics: Vec<Diagnostic>,
    pub dataflow_inputs: RoutineDataflowInputs,
    pub dataflow_result: RoutineDataflowResult,
}

impl ProjectRoutineAnalysis {
    pub fn routine(&self, id: RoutineId) -> Option<&RoutineAnalysis> {
        self.routines.get(id.as_usize())
    }

    pub fn routine_for_owner(&self, owner: SymbolHandle) -> Option<&RoutineAnalysis> {
        self.owner_to_routine
            .get(&owner)
            .and_then(|routine_id| self.routine(*routine_id))
    }

    pub fn routine_for_scope(&self, unit: UnitId, scope: ScopeId) -> Option<&RoutineAnalysis> {
        self.scope_to_routine
            .get(unit.as_usize())
            .and_then(|scope_map| scope_map.get(scope.as_usize()))
            .copied()
            .flatten()
            .and_then(|routine_id| self.routine(routine_id))
    }

    pub fn routines_for_unit(&self, unit: UnitId) -> impl Iterator<Item = &RoutineAnalysis> + '_ {
        self.unit_routines
            .get(unit.as_usize())
            .into_iter()
            .flat_map(|routine_ids| routine_ids.iter().copied())
            .filter_map(|routine_id| self.routine(routine_id))
    }

    pub fn diagnostics_for_unit(&self, unit: UnitId) -> &[Diagnostic] {
        self.unit_diagnostics
            .get(unit.as_usize())
            .map(Vec::as_slice)
            .unwrap_or(&[])
    }
}

pub fn build_project_routine_analysis(project: &ProjectAnalysis) -> ProjectRoutineAnalysis {
    let total_timer = std::time::Instant::now();
    let mut out = ProjectRoutineAnalysis {
        unit_routines: vec![Vec::new(); project.units.len()],
        scope_to_routine: project
            .units
            .iter()
            .map(|unit| vec![None; unit.scopes.len()])
            .collect(),
        unit_diagnostics: vec![Vec::new(); project.units.len()],
        ..ProjectRoutineAnalysis::default()
    };

    let index_timer = std::time::Instant::now();
    let mut exact_routine_scopes: Vec<Vec<Option<RoutineId>>> = project
        .units
        .iter()
        .map(|unit| vec![None; unit.scopes.len()])
        .collect();

    for unit in &project.units {
        let unit_idx = unit.unit_id.as_usize();
        for scope in &unit.scopes {
            let Some(kind) = routine_kind(scope.kind) else {
                continue;
            };
            let routine_id = RoutineId(out.routines.len() as u32);
            let owner = scope.owner.map(|symbol| SymbolHandle {
                unit: unit.unit_id,
                symbol,
            });
            let (name, decl_range) = owner
                .and_then(|handle| {
                    unit.symbols
                        .get(handle.symbol.as_usize())
                        .map(|symbol| (Arc::clone(&symbol.name), symbol.decl_range.clone()))
                })
                .unwrap_or_else(|| (synthetic_routine_name(kind, scope.id), scope.range.clone()));
            out.routines.push(RoutineAnalysis {
                descriptor: RoutineDescriptor {
                    id: routine_id,
                    unit: unit.unit_id,
                    scope: scope.id,
                    kind,
                    owner,
                    name,
                    decl_range,
                    scope_range: scope.range.clone(),
                    executable_range: None,
                },
                ir: RoutineIr::default(),
                cfg: RoutineCfg::default(),
                diagnostics: Vec::new(),
                dataflow_inputs: RoutineDataflowInputs::default(),
                dataflow_result: RoutineDataflowResult::default(),
            });
            if let Some(owner) = owner {
                out.owner_to_routine.insert(owner, routine_id);
            }
            if let Some(unit_routines) = out.unit_routines.get_mut(unit_idx) {
                unit_routines.push(routine_id);
            }
            if let Some(scope_entry) = exact_routine_scopes
                .get_mut(unit_idx)
                .and_then(|scope_map| scope_map.get_mut(scope.id.as_usize()))
            {
                *scope_entry = Some(routine_id);
            }
        }
    }

    for unit in &project.units {
        let unit_idx = unit.unit_id.as_usize();
        for scope in &unit.scopes {
            let routine_id = enclosing_routine_id(unit, &exact_routine_scopes[unit_idx], scope.id);
            if let Some(scope_entry) = out.scope_to_routine[unit_idx].get_mut(scope.id.as_usize()) {
                *scope_entry = routine_id;
            }
        }
    }
    out.metrics.index_micros = index_timer.elapsed().as_micros();

    let ir_timer = std::time::Instant::now();
    for unit in &project.units {
        let scope_map = &out.scope_to_routine[unit.unit_id.as_usize()];

        for reference in unit.references.iter().filter(|reference| {
            reference.namespace == Namespace::Value
                && !matches!(reference.kind, crate::ReferenceKind::TypeRef)
        }) {
            let Some(routine_id) = scope_map.get(reference.scope.as_usize()).copied().flatten()
            else {
                continue;
            };
            if let Some(routine) = out.routines.get_mut(routine_id.as_usize()) {
                routine.ir.instructions.push(RoutineInstruction {
                    id: RoutineInstrId(0),
                    scope: reference.scope,
                    range: reference.range.clone(),
                    site: RoutineInstructionSite::ValueRead {
                        reference: reference.id,
                    },
                });
            }
        }

        for (idx, assignment) in unit.assignment_sites.iter().enumerate() {
            let Some(routine_id) = scope_map
                .get(assignment.scope.as_usize())
                .copied()
                .flatten()
            else {
                continue;
            };
            if let Some(routine) = out.routines.get_mut(routine_id.as_usize()) {
                routine.ir.instructions.push(RoutineInstruction {
                    id: RoutineInstrId(0),
                    scope: assignment.scope,
                    range: assignment.range.clone(),
                    site: RoutineInstructionSite::Assignment { index: idx as u32 },
                });
            }
        }

        for (idx, call_site) in unit.call_sites.iter().enumerate() {
            let Some(routine_id) = scope_map.get(call_site.scope.as_usize()).copied().flatten()
            else {
                continue;
            };
            if let Some(routine) = out.routines.get_mut(routine_id.as_usize()) {
                routine.ir.instructions.push(RoutineInstruction {
                    id: RoutineInstrId(0),
                    scope: call_site.scope,
                    range: call_site.range.clone(),
                    site: RoutineInstructionSite::Call { index: idx as u32 },
                });
            }
        }

        for (idx, perform_call) in unit.perform_calls.iter().enumerate() {
            let Some(routine_id) = scope_map
                .get(perform_call.scope.as_usize())
                .copied()
                .flatten()
            else {
                continue;
            };
            if let Some(routine) = out.routines.get_mut(routine_id.as_usize()) {
                routine.ir.instructions.push(RoutineInstruction {
                    id: RoutineInstrId(0),
                    scope: perform_call.scope,
                    range: perform_call.range.clone(),
                    site: RoutineInstructionSite::Perform { index: idx as u32 },
                });
            }
        }

        for (idx, query) in unit.sql_queries.iter().enumerate() {
            let Some(routine_id) = scope_map.get(query.scope.as_usize()).copied().flatten() else {
                continue;
            };
            if let Some(routine) = out.routines.get_mut(routine_id.as_usize()) {
                routine.ir.instructions.push(RoutineInstruction {
                    id: RoutineInstrId(0),
                    scope: query.scope,
                    range: query.range.clone(),
                    site: RoutineInstructionSite::SqlQuery { index: idx as u32 },
                });
            }
        }

        for (idx, edge) in unit.value_flow_edges.iter().enumerate() {
            if !matches!(
                edge.kind,
                crate::ValueFlowKind::FieldSymbolAssignment
                    | crate::ValueFlowKind::ConditionalFieldSymbolAssignment
            ) {
                continue;
            }
            let Some(routine_id) = scope_map.get(edge.scope.as_usize()).copied().flatten() else {
                continue;
            };
            let target_range = match &edge.target {
                crate::ValueFlowTargetData::FieldSymbol { range, .. } => range,
                _ => continue,
            };
            let range = edge.source_range.start.min(target_range.start)
                ..edge.source_range.end.max(target_range.end);
            if let Some(routine) = out.routines.get_mut(routine_id.as_usize()) {
                routine.ir.instructions.push(RoutineInstruction {
                    id: RoutineInstrId(0),
                    scope: edge.scope,
                    range,
                    site: RoutineInstructionSite::FieldSymbolBind { index: idx as u32 },
                });
            }
        }

        for (idx, site) in unit.routine_sites.iter().enumerate() {
            let Some(routine_id) = scope_map.get(site.scope.as_usize()).copied().flatten() else {
                continue;
            };
            let instruction_site = match site.kind {
                RoutineSiteKind::UnknownEffect => RoutineInstructionSite::UnknownEffect,
                RoutineSiteKind::Clear => RoutineInstructionSite::Clear { index: idx as u32 },
                RoutineSiteKind::Delete => RoutineInstructionSite::Delete { index: idx as u32 },
                RoutineSiteKind::ReadTable => {
                    RoutineInstructionSite::ReadTable { index: idx as u32 }
                }
                RoutineSiteKind::Return => RoutineInstructionSite::Terminator {
                    kind: RoutineTerminatorKind::Return,
                },
                RoutineSiteKind::Raise => RoutineInstructionSite::Terminator {
                    kind: RoutineTerminatorKind::Raise,
                },
                RoutineSiteKind::Leave => RoutineInstructionSite::Terminator {
                    kind: RoutineTerminatorKind::Leave,
                },
                RoutineSiteKind::LeaveListProcessing => RoutineInstructionSite::Terminator {
                    kind: RoutineTerminatorKind::LeaveListProcessing,
                },
                RoutineSiteKind::Exit => RoutineInstructionSite::Terminator {
                    kind: RoutineTerminatorKind::Exit,
                },
                RoutineSiteKind::Continue => RoutineInstructionSite::Terminator {
                    kind: RoutineTerminatorKind::Continue,
                },
            };
            if let Some(routine) = out.routines.get_mut(routine_id.as_usize()) {
                routine.ir.instructions.push(RoutineInstruction {
                    id: RoutineInstrId(0),
                    scope: site.scope,
                    range: site.range.clone(),
                    site: instruction_site,
                });
            }
        }

        for region in &unit.routine_control_regions {
            let Some(routine_id) = scope_map.get(region.scope().as_usize()).copied().flatten()
            else {
                continue;
            };
            let instruction_site = match region {
                RoutineControlRegionData::If(_) => RoutineInstructionSite::Branch {
                    kind: RoutineBranchKind::If,
                },
                RoutineControlRegionData::Case(_) => RoutineInstructionSite::Branch {
                    kind: RoutineBranchKind::Case,
                },
                RoutineControlRegionData::Try(_) => RoutineInstructionSite::Branch {
                    kind: RoutineBranchKind::Try,
                },
                RoutineControlRegionData::Loop(data) => {
                    RoutineInstructionSite::LoopHeader { kind: data.kind }
                }
            };
            if let Some(routine) = out.routines.get_mut(routine_id.as_usize()) {
                routine.ir.instructions.push(RoutineInstruction {
                    id: RoutineInstrId(0),
                    scope: region.scope(),
                    range: region.range().clone(),
                    site: instruction_site,
                });
            }
        }
    }

    for routine in &mut out.routines {
        routine.ir.instructions.sort_by(|left, right| {
            left.range
                .start
                .cmp(&right.range.start)
                .then(left.range.end.cmp(&right.range.end))
                .then(
                    instruction_kind_sort_key(left.kind())
                        .cmp(&instruction_kind_sort_key(right.kind())),
                )
                .then(
                    instruction_site_sort_key(left.site)
                        .cmp(&instruction_site_sort_key(right.site)),
                )
        });
        for (idx, instruction) in routine.ir.instructions.iter_mut().enumerate() {
            instruction.id = RoutineInstrId(idx as u32);
        }
        routine.descriptor.executable_range = routine
            .ir
            .instructions
            .first()
            .zip(routine.ir.instructions.last())
            .map(|(first, last)| first.range.start..last.range.end);
    }
    out.metrics.ir_micros = ir_timer.elapsed().as_micros();

    let cfg_timer = std::time::Instant::now();
    for routine_idx in 0..out.routines.len() {
        let descriptor = out.routines[routine_idx].descriptor.clone();
        let Some(unit) = project.units.get(descriptor.unit.as_usize()) else {
            continue;
        };
        let Some(scope_map) = out.scope_to_routine.get(descriptor.unit.as_usize()) else {
            continue;
        };
        let (cfg, diagnostics) =
            build_routine_cfg_and_diagnostics(unit, scope_map, &out.routines[routine_idx]);
        out.unit_diagnostics[descriptor.unit.as_usize()].extend(diagnostics.iter().cloned());
        out.routines[routine_idx].cfg = cfg;
        out.routines[routine_idx].diagnostics = diagnostics;
    }
    for diagnostics in &mut out.unit_diagnostics {
        diagnostics.sort_by(|left, right| {
            left.range
                .start
                .cmp(&right.range.start)
                .then(left.range.end.cmp(&right.range.end))
                .then(left.message.cmp(&right.message))
        });
        diagnostics.dedup();
    }
    out.metrics.cfg_micros = cfg_timer.elapsed().as_micros();

    let dataflow_timer = std::time::Instant::now();
    for routine_id in 0..out.routines.len() {
        let descriptor = out.routines[routine_id].descriptor.clone();
        let Some(unit) = project.units.get(descriptor.unit.as_usize()) else {
            continue;
        };
        let Some(scope_map) = out.scope_to_routine.get(descriptor.unit.as_usize()) else {
            continue;
        };
        let (inputs, result, diagnostics, dead_store_micros) =
            build_routine_dataflow(project, unit, scope_map, &out.routines[routine_id]);
        out.metrics.dead_store_micros += dead_store_micros;
        out.routines[routine_id].dataflow_inputs = inputs;
        out.routines[routine_id].dataflow_result = result;
        out.routines[routine_id]
            .diagnostics
            .extend(diagnostics.iter().cloned());
        out.routines[routine_id].diagnostics.sort_by(|left, right| {
            left.range
                .start
                .cmp(&right.range.start)
                .then(left.range.end.cmp(&right.range.end))
                .then(left.message.cmp(&right.message))
        });
        out.routines[routine_id].diagnostics.dedup();
        out.unit_diagnostics[descriptor.unit.as_usize()].extend(diagnostics);
    }
    for diagnostics in &mut out.unit_diagnostics {
        diagnostics.sort_by(|left, right| {
            left.range
                .start
                .cmp(&right.range.start)
                .then(left.range.end.cmp(&right.range.end))
                .then(left.message.cmp(&right.message))
        });
        diagnostics.dedup();
    }
    out.metrics.dataflow_micros = dataflow_timer.elapsed().as_micros();

    out.metrics.routine_count = out.routines.len();
    out.metrics.instruction_count = out
        .routines
        .iter()
        .map(|routine| routine.ir.instructions.len())
        .sum();
    out.metrics.block_count = out
        .routines
        .iter()
        .map(|routine| routine.cfg.blocks.len())
        .sum();
    out.metrics.tracked_value_count = out
        .routines
        .iter()
        .map(|routine| routine.dataflow_inputs.values.len())
        .sum();
    out.metrics.total_micros = total_timer.elapsed().as_micros();
    out
}

#[derive(Debug, Clone)]
struct ReferenceUse {
    reference: crate::ReferenceId,
    range: TextRange,
    value: DataflowValueId,
}

#[derive(Debug, Clone)]
struct ReadOccurrence {
    reference: crate::ReferenceId,
    range: TextRange,
    value: DataflowValueId,
}

#[derive(Debug, Clone)]
struct InstructionTransfer {
    reads: Vec<ReadOccurrence>,
    writes: Vec<DataflowValueId>,
    assigned_writes: Vec<DataflowValueId>,
    structure_field_writes: Vec<StructureFieldWriteTransfer>,
    non_initial_kills: Vec<DataflowValueId>,
    field_symbol_binding: Vec<FieldSymbolBindingTransfer>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DeadStoreWrite {
    value: DataflowValueId,
    range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DeadStoreInstructionSummary {
    reads: Vec<DataflowValueId>,
    writes: Vec<DeadStoreWrite>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DeadStoreBlockSummary {
    live_gen: DenseBitSet,
    kill: DenseBitSet,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct StructureFieldWriteTransfer {
    value: DataflowValueId,
    mask: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct StructureFieldRead {
    value: DataflowValueId,
    mask: u64,
    range: TextRange,
}

#[derive(Debug, Clone)]
struct StructureAssignmentTracker {
    fields_by_name: HashMap<Arc<str>, u64>,
    full_mask: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SelectorStructureWrite {
    base_value: DataflowValueId,
    field_mask: Option<u64>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FieldSymbolBindingTransfer {
    Set(DataflowValueId),
    Copy {
        target: DataflowValueId,
        source: DataflowValueId,
    },
    Clear(DataflowValueId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CallArgumentEffect {
    Unknown,
    InputOnly,
    OutputOnly,
    InOut,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DenseBitSet {
    words: Vec<u64>,
}

impl DenseBitSet {
    fn new(bit_count: usize) -> Self {
        Self {
            words: vec![0; bit_count.div_ceil(64)],
        }
    }

    fn filled(bit_count: usize) -> Self {
        if bit_count == 0 {
            return Self { words: Vec::new() };
        }
        let word_count = bit_count.div_ceil(64);
        let mut words = vec![u64::MAX; word_count];
        let trailing_bits = bit_count % 64;
        if trailing_bits != 0
            && let Some(last) = words.last_mut()
        {
            *last = (1u64 << trailing_bits) - 1;
        }
        Self { words }
    }

    fn insert(&mut self, value: DataflowValueId) {
        let idx = value.as_usize();
        let word = idx / 64;
        let bit = idx % 64;
        if let Some(slot) = self.words.get_mut(word) {
            *slot |= 1u64 << bit;
        }
    }

    fn remove(&mut self, value: DataflowValueId) {
        let idx = value.as_usize();
        let word = idx / 64;
        let bit = idx % 64;
        if let Some(slot) = self.words.get_mut(word) {
            *slot &= !(1u64 << bit);
        }
    }

    fn contains(&self, value: DataflowValueId) -> bool {
        let idx = value.as_usize();
        let word = idx / 64;
        let bit = idx % 64;
        self.words
            .get(word)
            .is_some_and(|slot| (*slot & (1u64 << bit)) != 0)
    }

    fn union_from(&mut self, other: &Self) {
        for (slot, other_slot) in self.words.iter_mut().zip(&other.words) {
            *slot |= *other_slot;
        }
    }

    fn subtract_from(&mut self, other: &Self) {
        for (slot, other_slot) in self.words.iter_mut().zip(&other.words) {
            *slot &= !*other_slot;
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ControlKey {
    scope: ScopeId,
    start: usize,
    end: usize,
    tag: u8,
}

#[derive(Debug)]
struct RoutineBuildIndex<'a> {
    instructions_by_scope: Vec<Vec<RoutineInstrId>>,
    if_regions: HashMap<ControlKey, &'a IfRegionData>,
    case_regions: HashMap<ControlKey, &'a CaseRegionData>,
    try_regions: HashMap<ControlKey, &'a TryRegionData>,
    loop_regions: HashMap<ControlKey, &'a LoopRegionData>,
}

#[derive(Debug, Clone, Copy)]
struct ScopeExit {
    block: RoutineBlockId,
    reachable: bool,
}

#[derive(Debug, Clone, Copy)]
struct LoopFrame {
    header: RoutineBlockId,
    after_loop: RoutineBlockId,
}

#[derive(Debug, Clone)]
struct TryFrame {
    catch_entries: Vec<RoutineBlockId>,
    cleanup_entry: Option<RoutineBlockId>,
}

struct CfgBuilder<'a> {
    routine: &'a RoutineAnalysis,
    index: RoutineBuildIndex<'a>,
    blocks: Vec<RoutineBlock>,
    edges: Vec<RoutineEdge>,
    entry: RoutineBlockId,
    exit: RoutineBlockId,
    loop_stack: Vec<LoopFrame>,
    try_stack: Vec<TryFrame>,
}

impl<'a> RoutineBuildIndex<'a> {
    fn new(
        unit: &'a UnitAnalysis,
        scope_to_routine: &[Option<RoutineId>],
        routine: &RoutineAnalysis,
    ) -> Self {
        let mut instructions_by_scope = vec![Vec::new(); unit.scopes.len()];
        for instruction in &routine.ir.instructions {
            if let Some(entries) = instructions_by_scope.get_mut(instruction.scope.as_usize()) {
                entries.push(instruction.id);
            }
        }

        let mut if_regions = HashMap::new();
        let mut case_regions = HashMap::new();
        let mut try_regions = HashMap::new();
        let mut loop_regions = HashMap::new();
        for region in &unit.routine_control_regions {
            if scope_to_routine
                .get(region.scope().as_usize())
                .copied()
                .flatten()
                != Some(routine.descriptor.id)
            {
                continue;
            }
            match region {
                RoutineControlRegionData::If(data) => {
                    if_regions.insert(control_key(data.scope, &data.range, 0), data);
                }
                RoutineControlRegionData::Case(data) => {
                    case_regions.insert(control_key(data.scope, &data.range, 1), data);
                }
                RoutineControlRegionData::Try(data) => {
                    try_regions.insert(control_key(data.scope, &data.range, 2), data);
                }
                RoutineControlRegionData::Loop(data) => {
                    loop_regions.insert(control_key(data.scope, &data.range, loop_tag(data)), data);
                }
            }
        }

        Self {
            instructions_by_scope,
            if_regions,
            case_regions,
            try_regions,
            loop_regions,
        }
    }

    fn if_region(&self, instruction: &RoutineInstruction) -> Option<&'a IfRegionData> {
        self.if_regions
            .get(&control_key(instruction.scope, &instruction.range, 0))
            .copied()
    }

    fn case_region(&self, instruction: &RoutineInstruction) -> Option<&'a CaseRegionData> {
        self.case_regions
            .get(&control_key(instruction.scope, &instruction.range, 1))
            .copied()
    }

    fn try_region(&self, instruction: &RoutineInstruction) -> Option<&'a TryRegionData> {
        self.try_regions
            .get(&control_key(instruction.scope, &instruction.range, 2))
            .copied()
    }

    fn loop_region(
        &self,
        instruction: &RoutineInstruction,
        kind: crate::RoutineLoopKind,
    ) -> Option<&'a LoopRegionData> {
        self.loop_regions
            .get(&control_key(
                instruction.scope,
                &instruction.range,
                loop_tag_kind(kind),
            ))
            .copied()
    }
}

impl<'a> CfgBuilder<'a> {
    fn new(
        _unit: &'a UnitAnalysis,
        routine: &'a RoutineAnalysis,
        index: RoutineBuildIndex<'a>,
    ) -> Self {
        let entry = RoutineBlockId(0);
        let exit = RoutineBlockId(1);
        let blocks = vec![
            RoutineBlock {
                id: entry,
                kind: RoutineBlockKind::Entry,
                range: zero_range(routine.descriptor.decl_range.start),
                instructions: Vec::new(),
                predecessors: Vec::new(),
                successors: Vec::new(),
                reachable: false,
            },
            RoutineBlock {
                id: exit,
                kind: RoutineBlockKind::Exit,
                range: zero_range(routine.descriptor.scope_range.end),
                instructions: Vec::new(),
                predecessors: Vec::new(),
                successors: Vec::new(),
                reachable: false,
            },
        ];
        Self {
            routine,
            index,
            blocks,
            edges: Vec::new(),
            entry,
            exit,
            loop_stack: Vec::new(),
            try_stack: Vec::new(),
        }
    }

    fn build(mut self) -> RoutineCfg {
        if self.routine.ir.instructions.is_empty() {
            self.add_edge(self.entry, self.exit, RoutineEdgeKind::SyntheticFlow);
            self.finalize();
            return RoutineCfg {
                entry: Some(self.entry),
                exit: Some(self.exit),
                blocks: self.blocks,
                edges: self.edges,
            };
        }

        let start = self.new_block(
            RoutineBlockKind::Body,
            zero_range(
                self.routine
                    .descriptor
                    .executable_range
                    .as_ref()
                    .map(|range| range.start)
                    .unwrap_or(self.routine.descriptor.decl_range.end),
            ),
        );
        self.add_edge(self.entry, start, RoutineEdgeKind::SyntheticFlow);
        let exit_state = self.build_scope(
            self.routine.descriptor.scope,
            ScopeExit {
                block: start,
                reachable: true,
            },
        );
        if exit_state.reachable {
            self.add_edge(exit_state.block, self.exit, RoutineEdgeKind::Fallthrough);
        }
        self.finalize();
        RoutineCfg {
            entry: Some(self.entry),
            exit: Some(self.exit),
            blocks: self.blocks,
            edges: self.edges,
        }
    }

    fn build_scope(&mut self, scope: ScopeId, mut state: ScopeExit) -> ScopeExit {
        let instructions = self
            .index
            .instructions_by_scope
            .get(scope.as_usize())
            .cloned()
            .unwrap_or_default();
        for instruction_id in instructions {
            let instruction = self.routine.ir.instructions[instruction_id.as_usize()].clone();
            match instruction.site {
                RoutineInstructionSite::Assignment { .. }
                | RoutineInstructionSite::Call { .. }
                | RoutineInstructionSite::Perform { .. }
                | RoutineInstructionSite::SqlQuery { .. }
                | RoutineInstructionSite::Clear { .. }
                | RoutineInstructionSite::Delete { .. }
                | RoutineInstructionSite::ReadTable { .. }
                | RoutineInstructionSite::FieldSymbolBind { .. }
                | RoutineInstructionSite::ValueRead { .. }
                | RoutineInstructionSite::UnknownEffect => {
                    self.append_instruction(state.block, instruction_id);
                }
                RoutineInstructionSite::Branch { kind } => {
                    state = self.handle_branch(state, &instruction, kind);
                }
                RoutineInstructionSite::LoopHeader { kind } => {
                    state = self.handle_loop(state, &instruction, kind);
                }
                RoutineInstructionSite::Terminator { kind } => {
                    state = self.handle_terminator(state, &instruction, kind);
                }
            }
        }
        state
    }

    fn handle_branch(
        &mut self,
        state: ScopeExit,
        instruction: &RoutineInstruction,
        kind: RoutineBranchKind,
    ) -> ScopeExit {
        match kind {
            RoutineBranchKind::If => self.handle_if(state, instruction),
            RoutineBranchKind::Case => self.handle_case(state, instruction),
            RoutineBranchKind::Try => self.handle_try(state, instruction),
        }
    }

    fn handle_if(&mut self, state: ScopeExit, instruction: &RoutineInstruction) -> ScopeExit {
        let Some(region) = self.index.if_region(instruction).cloned() else {
            self.append_instruction(state.block, instruction.id);
            return state;
        };
        self.append_instruction(state.block, instruction.id);
        let join = self.new_block(RoutineBlockKind::Body, zero_range(instruction.range.end));
        let mut join_reachable = false;

        let then_entry = self.new_block(RoutineBlockKind::Body, zero_range(region.range.start));
        self.add_edge(state.block, then_entry, RoutineEdgeKind::Branch);
        let then_exit = self.build_scope(
            region.then_scope,
            ScopeExit {
                block: then_entry,
                reachable: state.reachable,
            },
        );
        if then_exit.reachable {
            self.add_edge(then_exit.block, join, RoutineEdgeKind::Fallthrough);
            join_reachable = true;
        }

        for elseif_scope in region.elseif_scopes {
            let elseif_entry =
                self.new_block(RoutineBlockKind::Body, zero_range(region.range.start));
            self.add_edge(state.block, elseif_entry, RoutineEdgeKind::Branch);
            let elseif_exit = self.build_scope(
                elseif_scope,
                ScopeExit {
                    block: elseif_entry,
                    reachable: state.reachable,
                },
            );
            if elseif_exit.reachable {
                self.add_edge(elseif_exit.block, join, RoutineEdgeKind::Fallthrough);
                join_reachable = true;
            }
        }

        if let Some(else_scope) = region.else_scope {
            let else_entry = self.new_block(RoutineBlockKind::Body, zero_range(region.range.start));
            self.add_edge(state.block, else_entry, RoutineEdgeKind::Branch);
            let else_exit = self.build_scope(
                else_scope,
                ScopeExit {
                    block: else_entry,
                    reachable: state.reachable,
                },
            );
            if else_exit.reachable {
                self.add_edge(else_exit.block, join, RoutineEdgeKind::Fallthrough);
                join_reachable = true;
            }
        } else {
            self.add_edge(state.block, join, RoutineEdgeKind::Fallthrough);
            join_reachable |= state.reachable;
        }

        ScopeExit {
            block: join,
            reachable: join_reachable,
        }
    }

    fn handle_case(&mut self, state: ScopeExit, instruction: &RoutineInstruction) -> ScopeExit {
        let Some(region) = self.index.case_region(instruction).cloned() else {
            self.append_instruction(state.block, instruction.id);
            return state;
        };
        self.append_instruction(state.block, instruction.id);
        let join = self.new_block(RoutineBlockKind::Body, zero_range(instruction.range.end));
        let mut join_reachable = false;

        for when_scope in region.when_scopes {
            let when_entry = self.new_block(RoutineBlockKind::Body, zero_range(region.range.start));
            self.add_edge(state.block, when_entry, RoutineEdgeKind::Branch);
            let when_exit = self.build_scope(
                when_scope,
                ScopeExit {
                    block: when_entry,
                    reachable: state.reachable,
                },
            );
            if when_exit.reachable {
                self.add_edge(when_exit.block, join, RoutineEdgeKind::Fallthrough);
                join_reachable = true;
            }
        }

        if !region.has_when_others {
            self.add_edge(state.block, join, RoutineEdgeKind::Fallthrough);
            join_reachable |= state.reachable;
        }

        ScopeExit {
            block: join,
            reachable: join_reachable,
        }
    }

    fn handle_try(&mut self, state: ScopeExit, instruction: &RoutineInstruction) -> ScopeExit {
        let Some(region) = self.index.try_region(instruction).cloned() else {
            self.append_instruction(state.block, instruction.id);
            return state;
        };
        self.append_instruction(state.block, instruction.id);
        let join = self.new_block(RoutineBlockKind::Body, zero_range(instruction.range.end));
        let body_entry = self.new_block(RoutineBlockKind::Body, zero_range(region.range.start));
        let catch_entries: Vec<_> = region
            .catch_scopes
            .iter()
            .map(|_| self.new_block(RoutineBlockKind::Body, zero_range(region.range.start)))
            .collect();
        let cleanup_entry = region
            .cleanup_scope
            .map(|_| self.new_block(RoutineBlockKind::Body, zero_range(region.range.start)));

        self.add_edge(state.block, body_entry, RoutineEdgeKind::SyntheticFlow);
        for &catch_entry in &catch_entries {
            self.add_edge(state.block, catch_entry, RoutineEdgeKind::Exceptional);
        }
        if let Some(cleanup_entry) = cleanup_entry {
            self.add_edge(state.block, cleanup_entry, RoutineEdgeKind::Exceptional);
        }

        self.try_stack.push(TryFrame {
            catch_entries: catch_entries.clone(),
            cleanup_entry,
        });
        let body_exit = self.build_scope(
            region.body_scope,
            ScopeExit {
                block: body_entry,
                reachable: state.reachable,
            },
        );
        self.try_stack.pop();

        let mut join_reachable = false;
        if body_exit.reachable {
            self.add_edge(body_exit.block, join, RoutineEdgeKind::Fallthrough);
            join_reachable = true;
        }

        for (catch_scope, catch_entry) in region.catch_scopes.into_iter().zip(catch_entries) {
            let catch_exit = self.build_scope(
                catch_scope,
                ScopeExit {
                    block: catch_entry,
                    reachable: state.reachable,
                },
            );
            if catch_exit.reachable {
                self.add_edge(catch_exit.block, join, RoutineEdgeKind::Fallthrough);
                join_reachable = true;
            }
        }

        if let Some(cleanup_scope) = region.cleanup_scope
            && let Some(cleanup_entry) = cleanup_entry
        {
            let cleanup_exit = self.build_scope(
                cleanup_scope,
                ScopeExit {
                    block: cleanup_entry,
                    reachable: state.reachable,
                },
            );
            if cleanup_exit.reachable {
                self.add_edge(cleanup_exit.block, join, RoutineEdgeKind::Fallthrough);
                join_reachable = true;
            }
        }

        ScopeExit {
            block: join,
            reachable: join_reachable,
        }
    }

    fn handle_loop(
        &mut self,
        state: ScopeExit,
        instruction: &RoutineInstruction,
        kind: crate::RoutineLoopKind,
    ) -> ScopeExit {
        let Some(region) = self.index.loop_region(instruction, kind).cloned() else {
            self.append_instruction(state.block, instruction.id);
            return state;
        };
        let header = if self.block_has_instructions(state.block) {
            let header =
                self.new_block(RoutineBlockKind::Body, zero_range(instruction.range.start));
            self.add_edge(state.block, header, RoutineEdgeKind::Fallthrough);
            header
        } else {
            state.block
        };
        self.append_instruction(header, instruction.id);

        let body_entry = self.new_block(RoutineBlockKind::Body, zero_range(region.range.start));
        let after_loop = self.new_block(RoutineBlockKind::Body, zero_range(instruction.range.end));
        self.add_edge(header, body_entry, RoutineEdgeKind::LoopEnter);
        self.add_edge(header, after_loop, RoutineEdgeKind::LoopExit);

        self.loop_stack.push(LoopFrame { header, after_loop });
        let body_exit = self.build_scope(
            region.body_scope,
            ScopeExit {
                block: body_entry,
                reachable: state.reachable,
            },
        );
        self.loop_stack.pop();
        if body_exit.reachable {
            self.add_edge(body_exit.block, header, RoutineEdgeKind::LoopBack);
        }

        ScopeExit {
            block: after_loop,
            reachable: state.reachable,
        }
    }

    fn handle_terminator(
        &mut self,
        state: ScopeExit,
        instruction: &RoutineInstruction,
        kind: RoutineTerminatorKind,
    ) -> ScopeExit {
        self.append_instruction(state.block, instruction.id);
        match kind {
            RoutineTerminatorKind::Return => {
                self.add_edge(state.block, self.exit, RoutineEdgeKind::Return);
                self.new_disconnected_successor(instruction.range.end)
            }
            RoutineTerminatorKind::Raise => {
                let mut targeted = false;
                if let Some(frame) = self.try_stack.last().cloned() {
                    if let Some(cleanup_entry) = frame.cleanup_entry {
                        self.add_edge(state.block, cleanup_entry, RoutineEdgeKind::Exceptional);
                        targeted = true;
                    }
                    for catch_entry in frame.catch_entries {
                        self.add_edge(state.block, catch_entry, RoutineEdgeKind::Raise);
                        targeted = true;
                    }
                }
                if !targeted {
                    self.add_edge(state.block, self.exit, RoutineEdgeKind::Raise);
                }
                self.new_disconnected_successor(instruction.range.end)
            }
            RoutineTerminatorKind::Leave => {
                let next =
                    self.new_block(RoutineBlockKind::Body, zero_range(instruction.range.end));
                self.add_edge(state.block, self.exit, RoutineEdgeKind::Leave);
                self.add_edge(state.block, next, RoutineEdgeKind::Fallthrough);
                ScopeExit {
                    block: next,
                    reachable: state.reachable,
                }
            }
            RoutineTerminatorKind::LeaveListProcessing => {
                if leave_list_processing_is_guaranteed_exit(&self.routine.descriptor) {
                    self.add_edge(state.block, self.exit, RoutineEdgeKind::Leave);
                    self.new_disconnected_successor(instruction.range.end)
                } else {
                    let next =
                        self.new_block(RoutineBlockKind::Body, zero_range(instruction.range.end));
                    self.add_edge(state.block, self.exit, RoutineEdgeKind::Leave);
                    self.add_edge(state.block, next, RoutineEdgeKind::Fallthrough);
                    ScopeExit {
                        block: next,
                        reachable: state.reachable,
                    }
                }
            }
            RoutineTerminatorKind::Exit => {
                if let Some(frame) = self.loop_stack.last().copied() {
                    self.add_edge(state.block, frame.after_loop, RoutineEdgeKind::Exit);
                    self.new_disconnected_successor(instruction.range.end)
                } else {
                    let next =
                        self.new_block(RoutineBlockKind::Body, zero_range(instruction.range.end));
                    self.add_edge(state.block, self.exit, RoutineEdgeKind::Exit);
                    self.add_edge(state.block, next, RoutineEdgeKind::Fallthrough);
                    ScopeExit {
                        block: next,
                        reachable: state.reachable,
                    }
                }
            }
            RoutineTerminatorKind::Continue => {
                if let Some(frame) = self.loop_stack.last().copied() {
                    self.add_edge(state.block, frame.header, RoutineEdgeKind::Continue);
                    self.new_disconnected_successor(instruction.range.end)
                } else {
                    let next =
                        self.new_block(RoutineBlockKind::Body, zero_range(instruction.range.end));
                    self.add_edge(state.block, next, RoutineEdgeKind::Fallthrough);
                    ScopeExit {
                        block: next,
                        reachable: state.reachable,
                    }
                }
            }
        }
    }

    fn finalize(&mut self) {
        for edge in &self.edges {
            push_unique(&mut self.blocks[edge.from.as_usize()].successors, edge.to);
            push_unique(&mut self.blocks[edge.to.as_usize()].predecessors, edge.from);
        }

        let mut queue = VecDeque::new();
        self.blocks[self.entry.as_usize()].reachable = true;
        queue.push_back(self.entry);
        while let Some(block_id) = queue.pop_front() {
            let successors = self.blocks[block_id.as_usize()].successors.clone();
            for successor in successors {
                let block = &mut self.blocks[successor.as_usize()];
                if !block.reachable {
                    block.reachable = true;
                    queue.push_back(successor);
                }
            }
        }
    }

    fn new_block(&mut self, kind: RoutineBlockKind, range: TextRange) -> RoutineBlockId {
        let id = RoutineBlockId(self.blocks.len() as u32);
        self.blocks.push(RoutineBlock {
            id,
            kind,
            range,
            instructions: Vec::new(),
            predecessors: Vec::new(),
            successors: Vec::new(),
            reachable: false,
        });
        id
    }

    fn block_has_instructions(&self, block: RoutineBlockId) -> bool {
        !self.blocks[block.as_usize()].instructions.is_empty()
    }

    fn append_instruction(&mut self, block: RoutineBlockId, instruction: RoutineInstrId) {
        let instruction = &self.routine.ir.instructions[instruction.as_usize()];
        let block_data = &mut self.blocks[block.as_usize()];
        if block_data.instructions.is_empty() {
            block_data.range = instruction.range.clone();
        } else {
            block_data.range.start = block_data.range.start.min(instruction.range.start);
            block_data.range.end = block_data.range.end.max(instruction.range.end);
        }
        block_data.instructions.push(instruction.id);
    }

    fn add_edge(&mut self, from: RoutineBlockId, to: RoutineBlockId, kind: RoutineEdgeKind) {
        self.edges.push(RoutineEdge { from, to, kind });
    }

    fn new_disconnected_successor(&mut self, offset: usize) -> ScopeExit {
        ScopeExit {
            block: self.new_block(RoutineBlockKind::Body, zero_range(offset)),
            reachable: false,
        }
    }
}

fn build_routine_cfg_and_diagnostics(
    unit: &UnitAnalysis,
    scope_to_routine: &[Option<RoutineId>],
    routine: &RoutineAnalysis,
) -> (RoutineCfg, Vec<Diagnostic>) {
    let index = RoutineBuildIndex::new(unit, scope_to_routine, routine);
    let cfg = CfgBuilder::new(unit, routine, index).build();
    let diagnostics = unreachable_diagnostics_for_cfg(routine, &cfg);
    (cfg, diagnostics)
}

fn unreachable_diagnostics_for_cfg(routine: &RoutineAnalysis, cfg: &RoutineCfg) -> Vec<Diagnostic> {
    let mut ranges: Vec<_> = cfg
        .blocks
        .iter()
        .filter(|block| {
            block.kind == RoutineBlockKind::Body
                && !block.reachable
                && !block.instructions.is_empty()
        })
        .map(|block| block.range.clone())
        .collect();
    ranges.sort_by(|left, right| left.start.cmp(&right.start).then(right.end.cmp(&left.end)));

    let mut diagnostics = Vec::new();
    let mut kept_ranges = Vec::new();
    for range in ranges {
        if kept_ranges
            .iter()
            .any(|kept: &TextRange| kept.start <= range.start && range.end <= kept.end)
        {
            continue;
        }
        kept_ranges.push(range.clone());
        diagnostics.push(Diagnostic {
            kind: DiagnosticKind::UnreachableCode,
            range,
            message: format!("unreachable code in routine '{}'", routine.descriptor.name),
        });
    }
    diagnostics.sort_by(|left, right| {
        left.range
            .start
            .cmp(&right.range.start)
            .then(left.range.end.cmp(&right.range.end))
            .then(left.message.cmp(&right.message))
    });
    diagnostics
}

fn build_routine_dataflow(
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    scope_to_routine: &[Option<RoutineId>],
    routine: &RoutineAnalysis,
) -> (
    RoutineDataflowInputs,
    RoutineDataflowResult,
    Vec<Diagnostic>,
    u128,
) {
    let routine_index = RoutineBuildIndex::new(unit, scope_to_routine, routine);
    let mut tracked_symbols: Vec<&SymbolData> = unit
        .symbols
        .iter()
        .filter(|symbol| trackable_symbol_kind(symbol.kind))
        .filter(|symbol| {
            scope_to_routine
                .get(symbol.scope.as_usize())
                .copied()
                .flatten()
                == Some(routine.descriptor.id)
        })
        .collect();
    tracked_symbols.sort_by(|left, right| {
        left.decl_range
            .start
            .cmp(&right.decl_range.start)
            .then(left.decl_range.end.cmp(&right.decl_range.end))
            .then((left.kind as u8).cmp(&(right.kind as u8)))
            .then(left.name.cmp(&right.name))
    });

    let mut values = Vec::with_capacity(tracked_symbols.len());
    let mut value_ids_by_symbol = HashMap::with_capacity(tracked_symbols.len());
    let mut structure_assignment_trackers = Vec::with_capacity(tracked_symbols.len());
    for symbol in tracked_symbols {
        let handle = SymbolHandle {
            unit: unit.unit_id,
            symbol: symbol.id,
        };
        let value_id = DataflowValueId(values.len() as u32);
        value_ids_by_symbol.insert(handle, value_id);
        structure_assignment_trackers
            .push(build_structure_assignment_tracker(unit, symbol.structure));
        values.push(RoutineDataflowValue {
            id: value_id,
            symbol: handle,
            name: Arc::clone(&symbol.name),
            kind: dataflow_value_kind(symbol.kind),
        });
    }

    let mut reference_uses = Vec::new();
    for instruction in &routine.ir.instructions {
        if let RoutineInstructionSite::ValueRead { reference } = instruction.site
            && let Some(value) =
                resolved_value_id_for_reference(unit, reference, &value_ids_by_symbol)
        {
            reference_uses.push(ReferenceUse {
                reference,
                range: instruction.range.clone(),
                value,
            });
        }
    }
    reference_uses.sort_by(|left, right| {
        left.range
            .start
            .cmp(&right.range.start)
            .then(left.range.end.cmp(&right.range.end))
            .then(left.reference.0.cmp(&right.reference.0))
            .then(left.value.as_usize().cmp(&right.value.as_usize()))
    });

    let call_argument_effects = build_call_argument_effects(project, unit);
    let safe_field_symbol_checks =
        resolve_safe_field_symbol_checks(unit, &reference_uses, &value_ids_by_symbol);
    let safe_value_state_checks =
        resolve_safe_value_state_checks(unit, &reference_uses, &value_ids_by_symbol);
    let safe_loop_where_field_refs =
        resolve_safe_loop_where_field_refs(unit, &reference_uses, &values);
    let is_not_initial_scope_refinements = resolve_is_not_initial_scope_refinements(
        unit,
        &reference_uses,
        &value_ids_by_symbol,
        values.len(),
    );
    let is_not_initial_field_scope_refinements = resolve_is_not_initial_field_scope_refinements(
        unit,
        &reference_uses,
        &value_ids_by_symbol,
        &structure_assignment_trackers,
        &values,
    );
    let structure_field_reads = resolve_structure_field_reads(
        unit,
        &reference_uses,
        &structure_assignment_trackers,
        &values,
    );
    let block_non_initial_entry_refinements = block_non_initial_entry_refinements(
        unit,
        routine,
        &is_not_initial_scope_refinements,
        values.len(),
    );
    let block_non_initial_field_entry_refinements = block_non_initial_field_entry_refinements(
        unit,
        routine,
        &is_not_initial_field_scope_refinements,
        values.len(),
    );
    let mut safe_read_refs = safe_field_symbol_checks.clone();
    safe_read_refs.extend(safe_value_state_checks);
    safe_read_refs.extend(safe_loop_where_field_refs);
    let mut suppressed_refs = std::collections::HashSet::new();
    for instruction in &routine.ir.instructions {
        match instruction.site {
            RoutineInstructionSite::Assignment { index } => {
                if let Some(assignment) = unit.assignment_sites.get(index as usize) {
                    suppressed_refs.extend(reference_ids_in_range(
                        &reference_uses,
                        &assignment.lhs_range,
                    ));
                    suppressed_refs.extend(reference_ids_in_range(
                        &reference_uses,
                        &assignment.rhs_range,
                    ));
                }
            }
            RoutineInstructionSite::Call { index } => {
                if let Some(call_site) = unit.call_sites.get(index as usize) {
                    suppressed_refs
                        .extend(reference_ids_in_range(&reference_uses, &call_site.range));
                }
            }
            RoutineInstructionSite::Clear { index } => {
                if let Some(site) = unit.routine_sites.get(index as usize) {
                    suppressed_refs.extend(reference_ids_in_range(&reference_uses, &site.range));
                }
            }
            RoutineInstructionSite::Delete { index } => {
                if let Some(site) = unit.routine_sites.get(index as usize) {
                    suppressed_refs.extend(reference_ids_in_range(&reference_uses, &site.range));
                }
            }
            RoutineInstructionSite::ReadTable { index } => {
                if let Some(site) = unit.routine_sites.get(index as usize) {
                    suppressed_refs.extend(reference_ids_in_range(&reference_uses, &site.range));
                }
            }
            RoutineInstructionSite::Perform { index } => {
                if let Some(perform_call) = unit.perform_calls.get(index as usize) {
                    suppressed_refs
                        .extend(reference_ids_in_range(&reference_uses, &perform_call.range));
                }
            }
            RoutineInstructionSite::FieldSymbolBind { index } => {
                if let Some(edge) = unit.value_flow_edges.get(index as usize) {
                    suppressed_refs
                        .extend(reference_ids_in_range(&reference_uses, &edge.source_range));
                    if let ValueFlowTargetData::FieldSymbol { range, .. } = &edge.target {
                        suppressed_refs.extend(reference_ids_in_range(&reference_uses, range));
                    }
                }
            }
            RoutineInstructionSite::SqlQuery { .. }
            | RoutineInstructionSite::ValueRead { .. }
            | RoutineInstructionSite::UnknownEffect
            | RoutineInstructionSite::Branch { .. }
            | RoutineInstructionSite::LoopHeader { .. }
            | RoutineInstructionSite::Terminator { .. } => {}
        }
    }

    let mut instruction_summaries = Vec::with_capacity(routine.ir.instructions.len());
    let mut instruction_transfers = Vec::with_capacity(routine.ir.instructions.len());
    let mut candidate_field_symbols = DenseBitSet::new(values.len());
    for instruction in &routine.ir.instructions {
        let mut transfer = InstructionTransfer {
            reads: Vec::new(),
            writes: Vec::new(),
            assigned_writes: Vec::new(),
            structure_field_writes: Vec::new(),
            non_initial_kills: Vec::new(),
            field_symbol_binding: Vec::new(),
        };
        match instruction.site {
            RoutineInstructionSite::ValueRead { reference } => {
                if !suppressed_refs.contains(&reference)
                    && !safe_read_refs.contains(&reference)
                    && let Some(value) =
                        resolved_value_id_for_reference(unit, reference, &value_ids_by_symbol)
                {
                    transfer.reads.push(ReadOccurrence {
                        reference,
                        range: instruction.range.clone(),
                        value,
                    });
                }
            }
            RoutineInstructionSite::Assignment { index } => {
                if let Some(assignment) = unit.assignment_sites.get(index as usize) {
                    transfer.reads.extend(read_occurrences_in_range(
                        &reference_uses,
                        &assignment.lhs_range,
                        &safe_read_refs,
                    ));
                    transfer.reads.extend(read_occurrences_in_range(
                        &reference_uses,
                        &assignment.rhs_range,
                        &safe_read_refs,
                    ));
                    if let Some(selector_write) = selector_structure_write_for_assignment(
                        unit,
                        assignment,
                        &reference_uses,
                        &structure_assignment_trackers,
                    ) {
                        transfer.reads.retain(|read| {
                            !(read.value == selector_write.base_value
                                && read.range.start >= assignment.lhs_range.start
                                && read.range.end <= assignment.lhs_range.end)
                        });
                        transfer.writes.push(selector_write.base_value);
                        transfer.non_initial_kills.push(selector_write.base_value);
                        if let Some(mask) = selector_write.field_mask {
                            transfer
                                .structure_field_writes
                                .push(StructureFieldWriteTransfer {
                                    value: selector_write.base_value,
                                    mask,
                                });
                        }
                    }
                    if let Some(write_value) = direct_write_value_id_for_assignment(
                        unit,
                        assignment,
                        &reference_uses,
                        &value_ids_by_symbol,
                        &values,
                    ) {
                        transfer.reads.retain(|read| {
                            !(read.range == assignment.lhs_range && read.value == write_value)
                        });
                        transfer.writes.push(write_value);
                        transfer.assigned_writes.push(write_value);
                        transfer.non_initial_kills.push(write_value);
                    }
                }
            }
            RoutineInstructionSite::Call { index } => {
                if let Some(call_site) = unit.call_sites.get(index as usize) {
                    if !is_safe_builtin_call(call_site) {
                        transfer.reads.extend(read_occurrences_in_range(
                            &reference_uses,
                            &call_site.range,
                            &safe_read_refs,
                        ));
                    }
                    for argument in &call_site.arguments {
                        let effect = call_argument_effects
                            .get(&(
                                call_site.range.start,
                                call_site.range.end,
                                argument.range.start,
                                argument.range.end,
                            ))
                            .copied()
                            .unwrap_or(CallArgumentEffect::Unknown);
                        if effect == CallArgumentEffect::OutputOnly {
                            transfer.reads.retain(|read| {
                                read.range.start < argument.range.start
                                    || read.range.end > argument.range.end
                            });
                        }
                        if matches!(
                            effect,
                            CallArgumentEffect::OutputOnly
                                | CallArgumentEffect::InOut
                                | CallArgumentEffect::Unknown
                        ) {
                            transfer.non_initial_kills.extend(
                                direct_non_field_symbol_values_in_range(
                                    &reference_uses,
                                    &argument.range,
                                    &values,
                                ),
                            );
                        }
                        if matches!(
                            effect,
                            CallArgumentEffect::InOut | CallArgumentEffect::Unknown
                        ) {
                            for value in direct_field_symbol_values_in_range(
                                &reference_uses,
                                &argument.range,
                                &values,
                            ) {
                                transfer
                                    .field_symbol_binding
                                    .push(FieldSymbolBindingTransfer::Clear(value));
                            }
                        }
                    }
                }
            }
            RoutineInstructionSite::Clear { index } => {
                if let Some(site) = unit.routine_sites.get(index as usize)
                    && let Some(write_value) =
                        direct_write_value_id_for_clear(&reference_uses, &site.range, &values)
                {
                    transfer.writes.push(write_value);
                    transfer.assigned_writes.push(write_value);
                    transfer.non_initial_kills.push(write_value);
                }
            }
            RoutineInstructionSite::Delete { .. } | RoutineInstructionSite::ReadTable { .. } => {}
            RoutineInstructionSite::Perform { index } => {
                if let Some(perform_call) = unit.perform_calls.get(index as usize) {
                    transfer.reads.extend(read_occurrences_in_range(
                        &reference_uses,
                        &perform_call.range,
                        &safe_read_refs,
                    ));
                    for argument in &perform_call.arguments {
                        transfer
                            .non_initial_kills
                            .extend(direct_non_field_symbol_values_in_range(
                                &reference_uses,
                                &argument.range,
                                &values,
                            ));
                        for value in direct_field_symbol_values_in_range(
                            &reference_uses,
                            &argument.range,
                            &values,
                        ) {
                            transfer
                                .field_symbol_binding
                                .push(FieldSymbolBindingTransfer::Clear(value));
                        }
                    }
                }
            }
            RoutineInstructionSite::FieldSymbolBind { index } => {
                if let Some(edge) = unit.value_flow_edges.get(index as usize)
                    && let Some(target_value) = resolve_field_symbol_target_value_id(
                        unit,
                        edge,
                        &reference_uses,
                        &value_ids_by_symbol,
                        &values,
                    )
                {
                    candidate_field_symbols.insert(target_value);
                    transfer.writes.push(target_value);
                    let direct_source =
                        exact_reference_use_in_range(&reference_uses, &edge.source_range);
                    match edge.kind {
                        ValueFlowKind::FieldSymbolAssignment => {
                            if let Some(source_use) = direct_source {
                                if values[source_use.value.as_usize()].kind
                                    == DataflowValueKind::FieldSymbol
                                {
                                    candidate_field_symbols.insert(source_use.value);
                                    if !safe_read_refs.contains(&source_use.reference) {
                                        transfer.reads.push(ReadOccurrence {
                                            reference: source_use.reference,
                                            range: source_use.range.clone(),
                                            value: source_use.value,
                                        });
                                    }
                                    transfer.field_symbol_binding.push(
                                        FieldSymbolBindingTransfer::Copy {
                                            target: target_value,
                                            source: source_use.value,
                                        },
                                    );
                                } else {
                                    transfer
                                        .field_symbol_binding
                                        .push(FieldSymbolBindingTransfer::Set(target_value));
                                }
                            } else {
                                transfer.reads.extend(read_occurrences_in_range(
                                    &reference_uses,
                                    &edge.source_range,
                                    &safe_read_refs,
                                ));
                                transfer
                                    .field_symbol_binding
                                    .push(FieldSymbolBindingTransfer::Set(target_value));
                            }
                        }
                        ValueFlowKind::ConditionalFieldSymbolAssignment => {
                            transfer.reads.extend(read_occurrences_in_range(
                                &reference_uses,
                                &edge.source_range,
                                &safe_read_refs,
                            ));
                            transfer
                                .field_symbol_binding
                                .push(FieldSymbolBindingTransfer::Clear(target_value));
                        }
                        ValueFlowKind::Assignment | ValueFlowKind::CallArgument => {}
                    }
                }
            }
            RoutineInstructionSite::UnknownEffect => {
                transfer
                    .non_initial_kills
                    .extend(direct_non_field_symbol_values_in_range(
                        &reference_uses,
                        &instruction.range,
                        &values,
                    ));
            }
            RoutineInstructionSite::LoopHeader { kind } => {
                if let Some(region) = routine_index.loop_region(instruction, kind)
                    && let Some(target_access) = region.target_access.as_ref()
                    && let Some(target_value) = resolve_loop_target_value_id(
                        unit,
                        target_access,
                        &reference_uses,
                        &value_ids_by_symbol,
                    )
                {
                    transfer.writes.push(target_value);
                    transfer.non_initial_kills.push(target_value);
                    match values[target_value.as_usize()].kind {
                        DataflowValueKind::FieldSymbol => {
                            candidate_field_symbols.insert(target_value);
                            transfer
                                .field_symbol_binding
                                .push(FieldSymbolBindingTransfer::Set(target_value));
                        }
                        DataflowValueKind::Variable | DataflowValueKind::Parameter => {
                            transfer.assigned_writes.push(target_value);
                        }
                        DataflowValueKind::Constant | DataflowValueKind::Other => {}
                    }
                }
            }
            RoutineInstructionSite::SqlQuery { .. }
            | RoutineInstructionSite::Branch { .. }
            | RoutineInstructionSite::Terminator { .. } => {}
        }
        instruction_summaries.push(InstructionDataflowSummary {
            instruction: instruction.id,
            reads: sorted_unique_value_ids(transfer.reads.iter().map(|read| read.value)),
            writes: sorted_unique_value_ids(transfer.writes.iter().copied()),
        });
        instruction_transfers.push(transfer);
    }

    let mut entry_assigned = DenseBitSet::new(values.len());
    let mut entry_structure_fields = vec![0u64; values.len()];
    for value in &values {
        if matches!(
            value.kind,
            DataflowValueKind::Parameter | DataflowValueKind::Constant
        ) {
            entry_assigned.insert(value.id);
            if let Some(tracker) = structure_assignment_trackers[value.id.as_usize()].as_ref() {
                entry_structure_fields[value.id.as_usize()] = tracker.full_mask;
            }
        }
    }

    let empty = DenseBitSet::new(values.len());
    let top = DenseBitSet::filled(values.len());
    let top_structure_fields = top_structure_field_masks(&structure_assignment_trackers);
    let mut block_entry_assigned = vec![empty.clone(); routine.cfg.blocks.len()];
    let mut block_exit_assigned = routine
        .cfg
        .blocks
        .iter()
        .map(|block| {
            if block.reachable && block.kind != RoutineBlockKind::Entry {
                top.clone()
            } else {
                empty.clone()
            }
        })
        .collect::<Vec<_>>();
    let mut block_entry_structure_fields = vec![vec![0u64; values.len()]; routine.cfg.blocks.len()];
    let mut block_exit_structure_fields = routine
        .cfg
        .blocks
        .iter()
        .map(|block| {
            if block.reachable && block.kind != RoutineBlockKind::Entry {
                top_structure_fields.clone()
            } else {
                vec![0u64; values.len()]
            }
        })
        .collect::<Vec<_>>();
    let mut block_entry_bound = vec![empty.clone(); routine.cfg.blocks.len()];
    let mut block_exit_bound = routine
        .cfg
        .blocks
        .iter()
        .map(|block| {
            if block.reachable && block.kind != RoutineBlockKind::Entry {
                top.clone()
            } else {
                empty.clone()
            }
        })
        .collect::<Vec<_>>();
    let mut block_entry_maybe_written = vec![empty.clone(); routine.cfg.blocks.len()];
    let mut block_exit_maybe_written = vec![empty.clone(); routine.cfg.blocks.len()];
    let mut block_entry_non_initial = vec![empty.clone(); routine.cfg.blocks.len()];
    let mut block_exit_non_initial = routine
        .cfg
        .blocks
        .iter()
        .map(|block| {
            if block.reachable && block.kind != RoutineBlockKind::Entry {
                top.clone()
            } else {
                empty.clone()
            }
        })
        .collect::<Vec<_>>();
    let mut block_entry_non_initial_fields =
        vec![vec![0u64; values.len()]; routine.cfg.blocks.len()];
    let mut block_exit_non_initial_fields = routine
        .cfg
        .blocks
        .iter()
        .map(|block| {
            if block.reachable && block.kind != RoutineBlockKind::Entry {
                top_structure_fields.clone()
            } else {
                vec![0u64; values.len()]
            }
        })
        .collect::<Vec<_>>();
    let mut changed = true;
    let mut iterations = 0u32;
    while changed {
        changed = false;
        iterations += 1;
        for block in &routine.cfg.blocks {
            let block_idx = block.id.as_usize();
            let next_entry_assigned = if !block.reachable {
                DenseBitSet::new(values.len())
            } else if block.kind == RoutineBlockKind::Entry {
                entry_assigned.clone()
            } else {
                intersect_predecessor_bits(&block.predecessors, &block_exit_assigned, values.len())
            };
            let next_entry_structure_fields = if !block.reachable {
                vec![0u64; values.len()]
            } else if block.kind == RoutineBlockKind::Entry {
                entry_structure_fields.clone()
            } else {
                intersect_predecessor_structure_fields(
                    &block.predecessors,
                    &block_exit_structure_fields,
                    values.len(),
                )
            };
            let next_entry_bound = if !block.reachable {
                DenseBitSet::new(values.len())
            } else if block.kind == RoutineBlockKind::Entry {
                DenseBitSet::new(values.len())
            } else {
                intersect_predecessor_bits(&block.predecessors, &block_exit_bound, values.len())
            };
            let next_entry_maybe_written = if !block.reachable {
                DenseBitSet::new(values.len())
            } else {
                union_predecessor_bits(&block.predecessors, &block_exit_maybe_written, values.len())
            };
            let mut next_entry_non_initial = if !block.reachable {
                DenseBitSet::new(values.len())
            } else {
                intersect_predecessor_bits(
                    &block.predecessors,
                    &block_exit_non_initial,
                    values.len(),
                )
            };
            next_entry_non_initial.union_from(&block_non_initial_entry_refinements[block_idx]);
            let mut next_entry_non_initial_fields = if !block.reachable {
                vec![0u64; values.len()]
            } else {
                intersect_predecessor_structure_fields(
                    &block.predecessors,
                    &block_exit_non_initial_fields,
                    values.len(),
                )
            };
            union_structure_field_masks(
                &mut next_entry_non_initial_fields,
                &block_non_initial_field_entry_refinements[block_idx],
            );
            let (
                next_exit_assigned,
                next_exit_structure_fields,
                next_exit_bound,
                next_exit_maybe_written,
                next_exit_non_initial,
                next_exit_non_initial_fields,
            ) = apply_block_transfer(
                block,
                &instruction_transfers,
                next_entry_assigned.clone(),
                next_entry_structure_fields.clone(),
                next_entry_bound.clone(),
                next_entry_maybe_written.clone(),
                next_entry_non_initial.clone(),
                next_entry_non_initial_fields.clone(),
                &structure_assignment_trackers,
            );
            if block_entry_assigned[block_idx] != next_entry_assigned {
                block_entry_assigned[block_idx] = next_entry_assigned;
                changed = true;
            }
            if block_entry_structure_fields[block_idx] != next_entry_structure_fields {
                block_entry_structure_fields[block_idx] = next_entry_structure_fields;
                changed = true;
            }
            if block_entry_bound[block_idx] != next_entry_bound {
                block_entry_bound[block_idx] = next_entry_bound;
                changed = true;
            }
            if block_entry_maybe_written[block_idx] != next_entry_maybe_written {
                block_entry_maybe_written[block_idx] = next_entry_maybe_written;
                changed = true;
            }
            if block_entry_non_initial[block_idx] != next_entry_non_initial {
                block_entry_non_initial[block_idx] = next_entry_non_initial;
                changed = true;
            }
            if block_entry_non_initial_fields[block_idx] != next_entry_non_initial_fields {
                block_entry_non_initial_fields[block_idx] = next_entry_non_initial_fields;
                changed = true;
            }
            if block_exit_assigned[block_idx] != next_exit_assigned {
                block_exit_assigned[block_idx] = next_exit_assigned;
                changed = true;
            }
            if block_exit_structure_fields[block_idx] != next_exit_structure_fields {
                block_exit_structure_fields[block_idx] = next_exit_structure_fields;
                changed = true;
            }
            if block_exit_bound[block_idx] != next_exit_bound {
                block_exit_bound[block_idx] = next_exit_bound;
                changed = true;
            }
            if block_exit_maybe_written[block_idx] != next_exit_maybe_written {
                block_exit_maybe_written[block_idx] = next_exit_maybe_written;
                changed = true;
            }
            if block_exit_non_initial[block_idx] != next_exit_non_initial {
                block_exit_non_initial[block_idx] = next_exit_non_initial;
                changed = true;
            }
            if block_exit_non_initial_fields[block_idx] != next_exit_non_initial_fields {
                block_exit_non_initial_fields[block_idx] = next_exit_non_initial_fields;
                changed = true;
            }
        }
    }

    let mut diagnostics = Vec::new();
    for block in &routine.cfg.blocks {
        if !block.reachable {
            continue;
        }
        let mut assigned = block_entry_assigned[block.id.as_usize()].clone();
        let mut structure_fields = block_entry_structure_fields[block.id.as_usize()].clone();
        let mut bound = block_entry_bound[block.id.as_usize()].clone();
        let mut maybe_written = block_entry_maybe_written[block.id.as_usize()].clone();
        let mut known_non_initial = block_entry_non_initial[block.id.as_usize()].clone();
        let mut known_non_initial_fields =
            block_entry_non_initial_fields[block.id.as_usize()].clone();
        for instruction_id in &block.instructions {
            let transfer = &instruction_transfers[instruction_id.as_usize()];
            for read in &transfer.reads {
                let value = &values[read.value.as_usize()];
                match value.kind {
                    DataflowValueKind::FieldSymbol => {
                        if candidate_field_symbols.contains(read.value)
                            && !bound.contains(read.value)
                        {
                            diagnostics.push(Diagnostic {
                                kind: DiagnosticKind::PossiblyUnboundFieldSymbol,
                                range: read.range.clone(),
                                message: format!(
                                    "field symbol '{}' may be unbound in routine '{}'",
                                    value.name, routine.descriptor.name
                                ),
                            });
                        }
                    }
                    DataflowValueKind::Variable | DataflowValueKind::Parameter => {
                        let (is_assigned, diagnostic_range) = if let Some(field_read) =
                            structure_field_reads.get(&read.reference).cloned()
                        {
                            let diagnostic_range = field_read.range.clone();
                            (
                                is_structure_field_definitely_assigned(
                                    &field_read,
                                    &assigned,
                                    &structure_fields,
                                ) || known_non_initial.contains(field_read.value)
                                    || (known_non_initial_fields[field_read.value.as_usize()]
                                        & field_read.mask)
                                        == field_read.mask,
                                diagnostic_range,
                            )
                        } else {
                            (
                                is_value_read_definitely_assigned(
                                    read.value,
                                    &assigned,
                                    &structure_fields,
                                    &structure_assignment_trackers,
                                ) || known_non_initial.contains(read.value),
                                read.range.clone(),
                            )
                        };
                        if !is_assigned {
                            diagnostics.push(Diagnostic {
                                kind: DiagnosticKind::UseBeforeDefiniteAssignment,
                                range: diagnostic_range,
                                message: format!(
                                    "'{}' may be used before definite assignment in routine '{}'",
                                    value.name, routine.descriptor.name
                                ),
                            });
                        }
                    }
                    DataflowValueKind::Constant | DataflowValueKind::Other => {}
                }
            }
            apply_instruction_transfer(
                transfer,
                &mut assigned,
                &mut structure_fields,
                &mut bound,
                &mut maybe_written,
                &mut known_non_initial,
                &mut known_non_initial_fields,
                &structure_assignment_trackers,
            );
        }
    }
    let dead_store_timer = std::time::Instant::now();
    diagnostics.extend(build_dead_store_diagnostics(
        unit,
        routine,
        &values,
        &reference_uses,
        &value_ids_by_symbol,
        &instruction_summaries,
        &call_argument_effects,
    ));
    diagnostics.sort_by(|left, right| {
        left.range
            .start
            .cmp(&right.range.start)
            .then(left.range.end.cmp(&right.range.end))
            .then(left.message.cmp(&right.message))
    });
    diagnostics.dedup();

    let block_entry = routine
        .cfg
        .blocks
        .iter()
        .map(|block| BlockDataflowSummary {
            block: block.id,
            maybe_written_values: bitset_to_value_ids(
                &block_entry_maybe_written[block.id.as_usize()],
            ),
            definitely_assigned_values: definitely_assigned_value_ids(
                &block_entry_assigned[block.id.as_usize()],
                &block_entry_structure_fields[block.id.as_usize()],
                &structure_assignment_trackers,
            ),
            definitely_bound_field_symbols: bitset_to_value_ids_matching(
                &block_entry_bound[block.id.as_usize()],
                &values,
                DataflowValueKind::FieldSymbol,
            ),
        })
        .collect();
    let block_exit = routine
        .cfg
        .blocks
        .iter()
        .map(|block| BlockDataflowSummary {
            block: block.id,
            maybe_written_values: bitset_to_value_ids(
                &block_exit_maybe_written[block.id.as_usize()],
            ),
            definitely_assigned_values: definitely_assigned_value_ids(
                &block_exit_assigned[block.id.as_usize()],
                &block_exit_structure_fields[block.id.as_usize()],
                &structure_assignment_trackers,
            ),
            definitely_bound_field_symbols: bitset_to_value_ids_matching(
                &block_exit_bound[block.id.as_usize()],
                &values,
                DataflowValueKind::FieldSymbol,
            ),
        })
        .collect();

    (
        RoutineDataflowInputs {
            values,
            instructions: instruction_summaries,
        },
        RoutineDataflowResult {
            converged: true,
            iterations: iterations.max(1),
            block_entry,
            block_exit,
        },
        diagnostics,
        dead_store_timer.elapsed().as_micros(),
    )
}

fn build_dead_store_diagnostics(
    unit: &UnitAnalysis,
    routine: &RoutineAnalysis,
    values: &[RoutineDataflowValue],
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
    instruction_summaries: &[InstructionDataflowSummary],
    call_argument_effects: &HashMap<(usize, usize, usize, usize), CallArgumentEffect>,
) -> Vec<Diagnostic> {
    if values.is_empty() || routine.cfg.blocks.is_empty() {
        return Vec::new();
    }

    let tracked_values = build_dead_store_tracked_values(
        unit,
        routine,
        values,
        reference_uses,
        call_argument_effects,
    );
    if !tracked_values.words.iter().any(|word| *word != 0) {
        return Vec::new();
    }

    let instruction_summaries = build_dead_store_instruction_summaries(
        unit,
        routine,
        values,
        reference_uses,
        value_ids_by_symbol,
        instruction_summaries,
        &tracked_values,
    );
    let block_summaries =
        build_dead_store_block_summaries(routine, &instruction_summaries, values.len());
    let (_, block_live_out) = compute_dead_store_liveness(routine, &block_summaries, values.len());

    let mut diagnostics = Vec::new();
    for block in &routine.cfg.blocks {
        if !block.reachable {
            continue;
        }
        let mut live = block_live_out[block.id.as_usize()].clone();
        for instruction_id in block.instructions.iter().rev() {
            let summary = &instruction_summaries[instruction_id.as_usize()];
            for write in &summary.writes {
                if live.contains(write.value) {
                    continue;
                }
                let value = &values[write.value.as_usize()];
                diagnostics.push(Diagnostic {
                    kind: DiagnosticKind::DeadStore,
                    range: write.range.clone(),
                    message: format!(
                        "write to local variable '{}' is never read in routine '{}'",
                        value.name, routine.descriptor.name
                    ),
                });
            }
            for write in &summary.writes {
                live.remove(write.value);
            }
            for read in &summary.reads {
                live.insert(*read);
            }
        }
    }

    diagnostics.sort_by(|left, right| {
        left.range
            .start
            .cmp(&right.range.start)
            .then(left.range.end.cmp(&right.range.end))
            .then(left.message.cmp(&right.message))
    });
    diagnostics.dedup();
    diagnostics
}

fn build_dead_store_tracked_values(
    unit: &UnitAnalysis,
    routine: &RoutineAnalysis,
    values: &[RoutineDataflowValue],
    reference_uses: &[ReferenceUse],
    call_argument_effects: &HashMap<(usize, usize, usize, usize), CallArgumentEffect>,
) -> DenseBitSet {
    let mut tracked = DenseBitSet::new(values.len());
    for value in values {
        if value.kind == DataflowValueKind::Variable {
            tracked.insert(value.id);
        }
    }

    for instruction in &routine.ir.instructions {
        match instruction.site {
            RoutineInstructionSite::Call { index } => {
                let Some(call_site) = unit.call_sites.get(index as usize) else {
                    continue;
                };
                if is_safe_builtin_call(call_site) {
                    continue;
                }
                for argument in &call_site.arguments {
                    let effect = call_argument_effect_for_call_argument(
                        call_argument_effects,
                        call_site,
                        argument,
                    );
                    if effect == CallArgumentEffect::InputOnly {
                        continue;
                    }
                    for value in direct_non_field_symbol_values_in_range(
                        reference_uses,
                        &argument.range,
                        values,
                    ) {
                        tracked.remove(value);
                    }
                }
            }
            RoutineInstructionSite::Perform { index } => {
                let Some(perform_call) = unit.perform_calls.get(index as usize) else {
                    continue;
                };
                for argument in &perform_call.arguments {
                    for value in direct_non_field_symbol_values_in_range(
                        reference_uses,
                        &argument.range,
                        values,
                    ) {
                        tracked.remove(value);
                    }
                }
            }
            RoutineInstructionSite::FieldSymbolBind { index } => {
                let Some(edge) = unit.value_flow_edges.get(index as usize) else {
                    continue;
                };
                for value in direct_non_field_symbol_values_in_range(
                    reference_uses,
                    &edge.source_range,
                    values,
                ) {
                    tracked.remove(value);
                }
            }
            RoutineInstructionSite::UnknownEffect => {
                for value in direct_non_field_symbol_values_in_range(
                    reference_uses,
                    &instruction.range,
                    values,
                ) {
                    tracked.remove(value);
                }
            }
            RoutineInstructionSite::Assignment { .. }
            | RoutineInstructionSite::SqlQuery { .. }
            | RoutineInstructionSite::Clear { .. }
            | RoutineInstructionSite::Delete { .. }
            | RoutineInstructionSite::ReadTable { .. }
            | RoutineInstructionSite::ValueRead { .. }
            | RoutineInstructionSite::Branch { .. }
            | RoutineInstructionSite::LoopHeader { .. }
            | RoutineInstructionSite::Terminator { .. } => {}
        }
    }

    tracked
}

fn call_argument_effect_for_call_argument(
    call_argument_effects: &HashMap<(usize, usize, usize, usize), CallArgumentEffect>,
    call_site: &crate::CallSiteData,
    argument: &crate::CallArgumentData,
) -> CallArgumentEffect {
    call_argument_effects
        .get(&(
            call_site.range.start,
            call_site.range.end,
            argument.range.start,
            argument.range.end,
        ))
        .copied()
        .unwrap_or(CallArgumentEffect::Unknown)
}

fn build_dead_store_instruction_summaries(
    unit: &UnitAnalysis,
    routine: &RoutineAnalysis,
    values: &[RoutineDataflowValue],
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
    instruction_summaries: &[InstructionDataflowSummary],
    tracked_values: &DenseBitSet,
) -> Vec<DeadStoreInstructionSummary> {
    let mut out = Vec::with_capacity(routine.ir.instructions.len());
    for instruction in &routine.ir.instructions {
        let reads = instruction_summaries
            .get(instruction.id.as_usize())
            .map(|summary| {
                summary
                    .reads
                    .iter()
                    .copied()
                    .filter(|value| tracked_values.contains(*value))
                    .collect()
            })
            .unwrap_or_default();
        let mut writes = Vec::new();

        match instruction.site {
            RoutineInstructionSite::Assignment { index } => {
                if let Some(assignment) = unit.assignment_sites.get(index as usize)
                    && let Some(value) = direct_write_value_id_for_assignment(
                        unit,
                        assignment,
                        reference_uses,
                        value_ids_by_symbol,
                        values,
                    )
                    && tracked_values.contains(value)
                {
                    writes.push(DeadStoreWrite {
                        value,
                        range: assignment.lhs_range.clone(),
                    });
                }
            }
            RoutineInstructionSite::Clear { index } => {
                if let Some(site) = unit.routine_sites.get(index as usize)
                    && let Some(value) =
                        direct_write_value_id_for_clear(reference_uses, &site.range, values)
                    && tracked_values.contains(value)
                {
                    writes.push(DeadStoreWrite {
                        value,
                        range: site.range.clone(),
                    });
                }
            }
            RoutineInstructionSite::Call { .. }
            | RoutineInstructionSite::Perform { .. }
            | RoutineInstructionSite::SqlQuery { .. }
            | RoutineInstructionSite::Delete { .. }
            | RoutineInstructionSite::ReadTable { .. }
            | RoutineInstructionSite::FieldSymbolBind { .. }
            | RoutineInstructionSite::ValueRead { .. }
            | RoutineInstructionSite::UnknownEffect
            | RoutineInstructionSite::Branch { .. }
            | RoutineInstructionSite::LoopHeader { .. }
            | RoutineInstructionSite::Terminator { .. } => {}
        }

        out.push(DeadStoreInstructionSummary { reads, writes });
    }
    out
}

fn build_dead_store_block_summaries(
    routine: &RoutineAnalysis,
    instruction_summaries: &[DeadStoreInstructionSummary],
    bit_count: usize,
) -> Vec<DeadStoreBlockSummary> {
    routine
        .cfg
        .blocks
        .iter()
        .map(|block| {
            let mut live_gen = DenseBitSet::new(bit_count);
            let mut kill = DenseBitSet::new(bit_count);
            for instruction_id in &block.instructions {
                let summary = &instruction_summaries[instruction_id.as_usize()];
                for read in &summary.reads {
                    if !kill.contains(*read) {
                        live_gen.insert(*read);
                    }
                }
                for write in &summary.writes {
                    kill.insert(write.value);
                }
            }
            DeadStoreBlockSummary { live_gen, kill }
        })
        .collect()
}

fn compute_dead_store_liveness(
    routine: &RoutineAnalysis,
    block_summaries: &[DeadStoreBlockSummary],
    bit_count: usize,
) -> (Vec<DenseBitSet>, Vec<DenseBitSet>) {
    let empty = DenseBitSet::new(bit_count);
    let mut live_in = vec![empty.clone(); routine.cfg.blocks.len()];
    let mut live_out = vec![empty; routine.cfg.blocks.len()];
    let mut changed = true;

    while changed {
        changed = false;
        for block in routine.cfg.blocks.iter().rev() {
            let block_idx = block.id.as_usize();
            if !block.reachable {
                continue;
            }

            let mut next_live_out = DenseBitSet::new(bit_count);
            for successor in &block.successors {
                next_live_out.union_from(&live_in[successor.as_usize()]);
            }

            let mut next_live_in = next_live_out.clone();
            next_live_in.subtract_from(&block_summaries[block_idx].kill);
            next_live_in.union_from(&block_summaries[block_idx].live_gen);

            if live_out[block_idx] != next_live_out {
                live_out[block_idx] = next_live_out;
                changed = true;
            }
            if live_in[block_idx] != next_live_in {
                live_in[block_idx] = next_live_in;
                changed = true;
            }
        }
    }

    (live_in, live_out)
}

fn resolved_value_id_for_reference(
    unit: &UnitAnalysis,
    reference: crate::ReferenceId,
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
) -> Option<DataflowValueId> {
    let reference = unit.references.get(reference.as_usize())?;
    let Resolution::Symbol(handle) = reference.resolution? else {
        return None;
    };
    if handle.unit != unit.unit_id {
        return None;
    }
    value_ids_by_symbol.get(&handle).copied()
}

fn build_call_argument_effects(
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
) -> HashMap<(usize, usize, usize, usize), CallArgumentEffect> {
    let mut effects = HashMap::new();
    for edge in &unit.value_flow_edges {
        if edge.kind != ValueFlowKind::CallArgument {
            continue;
        }
        let ValueFlowTargetData::CallParameter {
            call_range,
            parameter_decl_unit,
            parameter_decl_range,
            ..
        } = &edge.target
        else {
            continue;
        };
        let effect = parameter_decl_unit
            .and_then(|unit_id| {
                parameter_decl_range
                    .as_ref()
                    .map(|range| call_argument_effect_for_parameter(project, unit_id, range))
            })
            .unwrap_or(CallArgumentEffect::Unknown);
        effects.insert(
            (
                call_range.start,
                call_range.end,
                edge.source_range.start,
                edge.source_range.end,
            ),
            effect,
        );
    }
    effects
}

fn call_argument_effect_for_parameter(
    project: &ProjectAnalysis,
    unit_id: UnitId,
    range: &TextRange,
) -> CallArgumentEffect {
    let Some(unit) = project.units.get(unit_id.as_usize()) else {
        return CallArgumentEffect::Unknown;
    };
    for member in &unit.class_members {
        for parameter in &member.parameters {
            if &parameter.range != range {
                continue;
            }
            return match parameter.section {
                MethodParameterSection::Importing => CallArgumentEffect::InputOnly,
                MethodParameterSection::Changing => CallArgumentEffect::InOut,
                MethodParameterSection::Exporting
                | MethodParameterSection::Receiving
                | MethodParameterSection::Returning => CallArgumentEffect::OutputOnly,
            };
        }
    }
    for function_module in &unit.function_modules {
        for parameter in &function_module.parameters {
            if &parameter.range != range {
                continue;
            }
            return match parameter.section {
                FunctionModuleParameterSection::Importing => CallArgumentEffect::InputOnly,
                FunctionModuleParameterSection::Exporting => CallArgumentEffect::OutputOnly,
                FunctionModuleParameterSection::Changing
                | FunctionModuleParameterSection::Tables => CallArgumentEffect::InOut,
            };
        }
    }
    CallArgumentEffect::Unknown
}

fn resolve_safe_field_symbol_checks(
    unit: &UnitAnalysis,
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
) -> std::collections::HashSet<crate::ReferenceId> {
    let mut out = std::collections::HashSet::new();
    for check in &unit.field_symbol_state_checks {
        if !matches!(
            check.kind,
            FieldSymbolStateCheckKind::IsAssigned | FieldSymbolStateCheckKind::IsNotAssigned
        ) {
            continue;
        }
        for use_site in reference_uses_in_range(reference_uses, &check.symbol_range) {
            if use_site.range != check.symbol_range {
                continue;
            }
            let Some(reference) = unit.references.get(use_site.reference.as_usize()) else {
                continue;
            };
            let Some(Resolution::Symbol(handle)) = reference.resolution else {
                continue;
            };
            if handle.unit != unit.unit_id
                || !value_ids_by_symbol.contains_key(&handle)
                || reference.scope != check.scope
                || reference.name != check.symbol_name
            {
                continue;
            }
            out.insert(use_site.reference);
        }
    }
    out
}

fn resolve_safe_value_state_checks(
    unit: &UnitAnalysis,
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
) -> std::collections::HashSet<crate::ReferenceId> {
    let mut out = std::collections::HashSet::new();
    for check in &unit.value_state_checks {
        if !matches!(
            check.kind,
            ValueStateCheckKind::IsInitial | ValueStateCheckKind::IsNotInitial
        ) {
            continue;
        }
        for use_site in reference_uses_in_range(reference_uses, &check.symbol_range) {
            if use_site.range != check.symbol_range {
                continue;
            }
            let Some(reference) = unit.references.get(use_site.reference.as_usize()) else {
                continue;
            };
            let Some(Resolution::Symbol(handle)) = reference.resolution else {
                continue;
            };
            if handle.unit != unit.unit_id
                || !value_ids_by_symbol.contains_key(&handle)
                || reference.scope != check.scope
                || reference.name != check.symbol_name
            {
                continue;
            }
            out.insert(use_site.reference);
        }
    }
    out
}

fn resolve_safe_loop_where_field_refs(
    unit: &UnitAnalysis,
    reference_uses: &[ReferenceUse],
    values: &[RoutineDataflowValue],
) -> std::collections::HashSet<crate::ReferenceId> {
    let mut out = std::collections::HashSet::new();
    for context in &unit.loop_where_field_contexts {
        let Some(structure_id) =
            resolve_value_access_structure(unit, reference_uses, values, &context.source_access)
        else {
            continue;
        };
        let Some(structure) = unit.structures.get(structure_id.as_usize()) else {
            continue;
        };
        let field_names: std::collections::HashSet<_> = structure
            .fields
            .iter()
            .map(|field| field.name.as_ref())
            .collect();
        for use_site in reference_uses_in_range(reference_uses, &context.range) {
            let Some(reference) = unit.references.get(use_site.reference.as_usize()) else {
                continue;
            };
            if field_names.contains(reference.name.as_ref()) {
                out.insert(use_site.reference);
            }
        }
    }
    out
}

fn resolve_is_not_initial_scope_refinements(
    unit: &UnitAnalysis,
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
    value_count: usize,
) -> Vec<DenseBitSet> {
    let mut out = vec![DenseBitSet::new(value_count); unit.scopes.len()];
    for check in &unit.value_state_checks {
        if check.kind != ValueStateCheckKind::IsNotInitial {
            continue;
        }
        let Some(scope_bits) = out.get_mut(check.scope.as_usize()) else {
            continue;
        };
        for use_site in reference_uses_in_range(reference_uses, &check.symbol_range) {
            if use_site.range != check.symbol_range {
                continue;
            }
            let Some(reference) = unit.references.get(use_site.reference.as_usize()) else {
                continue;
            };
            let Some(Resolution::Symbol(handle)) = reference.resolution else {
                continue;
            };
            if handle.unit != unit.unit_id
                || !value_ids_by_symbol.contains_key(&handle)
                || reference.scope != check.scope
                || reference.name != check.symbol_name
            {
                continue;
            }
            scope_bits.insert(use_site.value);
        }
    }
    out
}

fn resolve_is_not_initial_field_scope_refinements(
    unit: &UnitAnalysis,
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
    structure_assignment_trackers: &[Option<StructureAssignmentTracker>],
    values: &[RoutineDataflowValue],
) -> Vec<Vec<u64>> {
    let mut out = vec![vec![0u64; values.len()]; unit.scopes.len()];
    for check in &unit.value_state_checks {
        if check.kind != ValueStateCheckKind::IsNotInitial {
            continue;
        }
        let Some(field_name) = check.field_name.as_ref() else {
            continue;
        };
        let Some(scope_masks) = out.get_mut(check.scope.as_usize()) else {
            continue;
        };
        for use_site in reference_uses_in_range(reference_uses, &check.symbol_range) {
            if use_site.range != check.symbol_range {
                continue;
            }
            let Some(reference) = unit.references.get(use_site.reference.as_usize()) else {
                continue;
            };
            let Some(Resolution::Symbol(handle)) = reference.resolution else {
                continue;
            };
            if handle.unit != unit.unit_id
                || !value_ids_by_symbol.contains_key(&handle)
                || reference.scope != check.scope
                || reference.name != check.symbol_name
                || value_symbol_is_internal_table(unit, use_site.value, values)
            {
                continue;
            }
            let Some(mask) = structure_assignment_trackers[use_site.value.as_usize()]
                .as_ref()
                .and_then(|tracker| tracker.fields_by_name.get(field_name).copied())
            else {
                continue;
            };
            scope_masks[use_site.value.as_usize()] |= mask;
        }
    }
    out
}

fn block_non_initial_entry_refinements(
    unit: &UnitAnalysis,
    routine: &RoutineAnalysis,
    scope_refinements: &[DenseBitSet],
    value_count: usize,
) -> Vec<DenseBitSet> {
    routine
        .cfg
        .blocks
        .iter()
        .map(|block| {
            let Some(first_instruction) = block.instructions.first() else {
                return DenseBitSet::new(value_count);
            };
            inherited_non_initial_scope_refinements(
                unit,
                scope_refinements,
                routine.ir.instructions[first_instruction.as_usize()].scope,
                value_count,
            )
        })
        .collect()
}

fn block_non_initial_field_entry_refinements(
    unit: &UnitAnalysis,
    routine: &RoutineAnalysis,
    scope_refinements: &[Vec<u64>],
    value_count: usize,
) -> Vec<Vec<u64>> {
    routine
        .cfg
        .blocks
        .iter()
        .map(|block| {
            let Some(first_instruction) = block.instructions.first() else {
                return vec![0u64; value_count];
            };
            inherited_non_initial_field_scope_refinements(
                unit,
                scope_refinements,
                routine.ir.instructions[first_instruction.as_usize()].scope,
                value_count,
            )
        })
        .collect()
}

fn inherited_non_initial_scope_refinements(
    unit: &UnitAnalysis,
    scope_refinements: &[DenseBitSet],
    scope: ScopeId,
    value_count: usize,
) -> DenseBitSet {
    let mut out = DenseBitSet::new(value_count);
    let mut current = Some(scope);
    let mut child_kind = None;
    while let Some(scope_id) = current {
        let Some(scope_data) = unit.scopes.get(scope_id.as_usize()) else {
            break;
        };
        let skip_current = scope_data.kind == ScopeKind::TryBlock
            && matches!(
                child_kind,
                Some(ScopeKind::CatchClause | ScopeKind::CleanupClause)
            );
        if !skip_current && let Some(bits) = scope_refinements.get(scope_id.as_usize()) {
            out.union_from(bits);
        }
        child_kind = Some(scope_data.kind);
        current = scope_data.parent;
    }
    out
}

fn inherited_non_initial_field_scope_refinements(
    unit: &UnitAnalysis,
    scope_refinements: &[Vec<u64>],
    scope: ScopeId,
    value_count: usize,
) -> Vec<u64> {
    let mut out = vec![0u64; value_count];
    let mut current = Some(scope);
    let mut child_kind = None;
    while let Some(scope_id) = current {
        let Some(scope_data) = unit.scopes.get(scope_id.as_usize()) else {
            break;
        };
        let skip_current = scope_data.kind == ScopeKind::TryBlock
            && matches!(
                child_kind,
                Some(ScopeKind::CatchClause | ScopeKind::CleanupClause)
            );
        if !skip_current && let Some(masks) = scope_refinements.get(scope_id.as_usize()) {
            union_structure_field_masks(&mut out, masks);
        }
        child_kind = Some(scope_data.kind);
        current = scope_data.parent;
    }
    out
}

fn reference_uses_in_range(
    reference_uses: &[ReferenceUse],
    range: &TextRange,
) -> Vec<ReferenceUse> {
    let start_idx = reference_uses.partition_point(|use_site| use_site.range.start < range.start);
    let mut uses = Vec::new();
    for use_site in &reference_uses[start_idx..] {
        if use_site.range.start >= range.end {
            break;
        }
        if use_site.range.end <= range.end {
            uses.push(use_site.clone());
        }
    }
    uses
}

fn reference_ids_in_range(
    reference_uses: &[ReferenceUse],
    range: &TextRange,
) -> Vec<crate::ReferenceId> {
    reference_uses_in_range(reference_uses, range)
        .into_iter()
        .map(|use_site| use_site.reference)
        .collect()
}

fn exact_reference_use_in_range(
    reference_uses: &[ReferenceUse],
    range: &TextRange,
) -> Option<ReferenceUse> {
    let mut matches = reference_uses_in_range(reference_uses, range)
        .into_iter()
        .filter(|use_site| use_site.range == *range);
    let first = matches.next()?;
    matches.next().is_none().then_some(first)
}

fn read_occurrences_in_range(
    reference_uses: &[ReferenceUse],
    range: &TextRange,
    safe_field_symbol_checks: &std::collections::HashSet<crate::ReferenceId>,
) -> Vec<ReadOccurrence> {
    reference_uses_in_range(reference_uses, range)
        .into_iter()
        .filter(|use_site| !safe_field_symbol_checks.contains(&use_site.reference))
        .map(|use_site| ReadOccurrence {
            reference: use_site.reference,
            range: use_site.range,
            value: use_site.value,
        })
        .collect()
}

fn resolve_structure_field_reads(
    unit: &UnitAnalysis,
    reference_uses: &[ReferenceUse],
    structure_assignment_trackers: &[Option<StructureAssignmentTracker>],
    values: &[RoutineDataflowValue],
) -> HashMap<crate::ReferenceId, StructureFieldRead> {
    let mut out = HashMap::new();
    for access in &unit.field_accesses {
        if access.base_namespace != Namespace::Value
            || access.field_path.len() != 1
            || access.field_path[0].is_deref()
        {
            continue;
        }
        let Some(base_use) = reference_uses_in_range(reference_uses, &access.base_range)
            .into_iter()
            .find(|use_site| use_site.range == access.base_range)
        else {
            continue;
        };
        let Some(tracker) = structure_assignment_trackers
            .get(base_use.value.as_usize())
            .and_then(|tracker| tracker.as_ref())
        else {
            continue;
        };
        if value_symbol_is_internal_table(unit, base_use.value, values) {
            continue;
        }
        let Some(mask) = tracker
            .fields_by_name
            .get(&access.field_path[0].name)
            .copied()
        else {
            continue;
        };
        out.insert(
            base_use.reference,
            StructureFieldRead {
                value: base_use.value,
                mask,
                range: access.base_range.start..access.field_path[0].range.end,
            },
        );
    }
    out
}

fn build_structure_assignment_tracker(
    unit: &UnitAnalysis,
    structure_id: Option<StructureId>,
) -> Option<StructureAssignmentTracker> {
    let structure_id = structure_id?;
    let structure = unit.structures.get(structure_id.as_usize())?;
    let field_count = structure.fields.len();
    if field_count == 0 || field_count > 64 {
        return None;
    }
    let mut fields_by_name = HashMap::with_capacity(field_count);
    let mut full_mask = 0u64;
    for (idx, field) in structure.fields.iter().enumerate() {
        let mask = 1u64 << idx;
        full_mask |= mask;
        fields_by_name.insert(Arc::clone(&field.name), mask);
    }
    Some(StructureAssignmentTracker {
        fields_by_name,
        full_mask,
    })
}

fn value_symbol_is_internal_table(
    unit: &UnitAnalysis,
    value: DataflowValueId,
    values: &[RoutineDataflowValue],
) -> bool {
    let Some(symbol) = values
        .get(value.as_usize())
        .and_then(|value| unit.symbols.get(value.symbol.symbol.as_usize()))
    else {
        return false;
    };
    symbol_type_clause_suggests_internal_table(symbol)
        || unit.sql_targets.iter().any(|target| {
            target.is_inline
                && target.is_table
                && target.scope == symbol.scope
                && target.target_name.as_deref() == Some(symbol.name.as_ref())
        })
}

fn resolve_value_access_structure(
    unit: &UnitAnalysis,
    reference_uses: &[ReferenceUse],
    values: &[RoutineDataflowValue],
    access: &crate::FieldAccess,
) -> Option<StructureId> {
    if access.base_namespace != Namespace::Value {
        return None;
    }
    let base_value = exact_reference_use_in_range(reference_uses, &access.base_range)
        .map(|use_site| use_site.value)
        .or_else(|| resolve_declared_value_id_for_access(unit, access, values))?;
    let symbol_id = values.get(base_value.as_usize())?.symbol.symbol;
    let mut structure = unit.symbols.get(symbol_id.as_usize())?.structure;
    if access.field_path.is_empty() {
        return structure;
    }
    for segment in &access.field_path {
        if segment.is_deref() {
            return None;
        }
        let structure_id = structure?;
        let field = unit
            .structures
            .get(structure_id.as_usize())?
            .fields
            .iter()
            .find(|field| field.name == segment.name)?;
        structure = field.structure.or_else(|| {
            field
                .type_ref
                .as_ref()
                .and_then(|type_ref| resolve_type_ref_structure(unit, access.scope, type_ref))
        });
    }
    structure
}

fn resolve_declared_value_id_for_access(
    unit: &UnitAnalysis,
    access: &crate::FieldAccess,
    values: &[RoutineDataflowValue],
) -> Option<DataflowValueId> {
    unit.symbols
        .iter()
        .find(|symbol| {
            symbol.name == access.base_name
                && symbol.decl_range == access.base_range
                && trackable_symbol_kind(symbol.kind)
        })
        .and_then(|symbol| {
            values
                .iter()
                .find(|value| value.symbol.symbol == symbol.id)
                .map(|value| value.id)
        })
}

fn symbol_type_clause_suggests_internal_table(symbol: &crate::SymbolData) -> bool {
    let Some(display) = symbol.type_clause_display.as_deref() else {
        return false;
    };
    let upper = display.to_ascii_uppercase();
    upper.contains("STANDARD TABLE")
        || upper.contains("HASHED TABLE")
        || upper.contains("SORTED TABLE")
        || upper.contains("ANY TABLE")
        || upper.contains("INDEX TABLE")
        || upper.contains("TABLE OF")
}

fn resolve_type_ref_structure(
    unit: &UnitAnalysis,
    scope: ScopeId,
    type_ref: &crate::FieldTypeRefData,
) -> Option<StructureId> {
    let symbol_id =
        resolve_symbol_in_scope_chain(unit, scope, type_ref.namespace, &type_ref.base_name)?;
    let mut structure = unit.symbols.get(symbol_id.as_usize())?.structure;
    for segment in &type_ref.field_path {
        let structure_id = structure?;
        let field = unit
            .structures
            .get(structure_id.as_usize())?
            .fields
            .iter()
            .find(|field| field.name.as_ref() == segment.as_ref())?;
        structure = field.structure.or_else(|| {
            field
                .type_ref
                .as_ref()
                .and_then(|nested| resolve_type_ref_structure(unit, scope, nested))
        });
    }
    structure
}

fn resolve_symbol_in_scope_chain(
    unit: &UnitAnalysis,
    scope: ScopeId,
    namespace: Namespace,
    name: &str,
) -> Option<crate::ids::SymbolId> {
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        if let Some(symbol) = unit.symbols.iter().find(|symbol| {
            symbol.scope == scope_id
                && symbol.name.as_ref() == name
                && symbol.kind.namespaces().contains(&namespace)
        }) {
            return Some(symbol.id);
        }
        current = unit
            .scopes
            .get(scope_id.as_usize())
            .and_then(|scope| scope.parent);
    }
    None
}

fn direct_write_value_id_for_assignment(
    unit: &UnitAnalysis,
    assignment: &crate::AssignmentSiteData,
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
    values: &[RoutineDataflowValue],
) -> Option<DataflowValueId> {
    for symbol in &unit.symbols {
        if symbol.decl_range.start < assignment.lhs_range.start
            || symbol.decl_range.end > assignment.lhs_range.end
        {
            continue;
        }
        let handle = SymbolHandle {
            unit: unit.unit_id,
            symbol: symbol.id,
        };
        let Some(value_id) = value_ids_by_symbol.get(&handle).copied() else {
            continue;
        };
        if values[value_id.as_usize()].kind != DataflowValueKind::FieldSymbol {
            return Some(value_id);
        }
    }
    let direct = exact_reference_use_in_range(reference_uses, &assignment.lhs_range)?;
    (values[direct.value.as_usize()].kind != DataflowValueKind::FieldSymbol).then_some(direct.value)
}

fn direct_write_value_id_for_clear(
    reference_uses: &[ReferenceUse],
    range: &TextRange,
    values: &[RoutineDataflowValue],
) -> Option<DataflowValueId> {
    let direct = exact_reference_use_in_range(reference_uses, range)?;
    (values[direct.value.as_usize()].kind != DataflowValueKind::FieldSymbol).then_some(direct.value)
}

fn selector_structure_write_for_assignment(
    unit: &UnitAnalysis,
    assignment: &crate::AssignmentSiteData,
    reference_uses: &[ReferenceUse],
    structure_assignment_trackers: &[Option<StructureAssignmentTracker>],
) -> Option<SelectorStructureWrite> {
    let access = assignment.lhs_target_access.as_ref()?;
    if access.base_namespace != Namespace::Value
        || access.field_path.is_empty()
        || access.field_path.iter().any(|segment| segment.is_deref())
    {
        return None;
    }
    let base_use = reference_uses_in_range(reference_uses, &assignment.lhs_range)
        .into_iter()
        .filter(|use_site| {
            unit.references
                .get(use_site.reference.as_usize())
                .is_some_and(|reference| reference.name == access.base_name)
        })
        .collect::<Vec<_>>();
    let [base_use] = base_use.as_slice() else {
        return None;
    };
    let tracker = structure_assignment_trackers
        .get(base_use.value.as_usize())?
        .as_ref()?;
    let field_mask = (access.field_path.len() == 1)
        .then(|| {
            tracker
                .fields_by_name
                .get(&access.field_path[0].name)
                .copied()
        })
        .flatten();
    Some(SelectorStructureWrite {
        base_value: base_use.value,
        field_mask,
    })
}

fn resolve_loop_target_value_id(
    unit: &UnitAnalysis,
    access: &crate::FieldAccess,
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
) -> Option<DataflowValueId> {
    if access.base_namespace != Namespace::Value || !access.field_path.is_empty() {
        return None;
    }
    exact_reference_use_in_range(reference_uses, &access.base_range)
        .map(|use_site| use_site.value)
        .or_else(|| {
            unit.symbols.iter().find_map(|symbol| {
                (symbol.name == access.base_name && symbol.decl_range == access.base_range)
                    .then(|| {
                        value_ids_by_symbol.get(&SymbolHandle {
                            unit: unit.unit_id,
                            symbol: symbol.id,
                        })
                    })?
                    .copied()
            })
        })
}

fn is_safe_builtin_call(call_site: &crate::CallSiteData) -> bool {
    matches!(
        &call_site.target,
        crate::NamedArgumentTarget::Routine { routine_name }
            if builtin_routine_spec(routine_name.as_ref())
                .is_some_and(|spec| spec.name.eq_ignore_ascii_case("lines"))
    )
}

fn resolve_field_symbol_target_value_id(
    unit: &UnitAnalysis,
    edge: &crate::ValueFlowEdgeData,
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
    values: &[RoutineDataflowValue],
) -> Option<DataflowValueId> {
    let ValueFlowTargetData::FieldSymbol { range, name } = &edge.target else {
        return None;
    };
    if let Some(name) = name.as_ref() {
        for symbol in &unit.symbols {
            if symbol.kind != SymbolKind::FieldSymbol || symbol.name != *name {
                continue;
            }
            let handle = SymbolHandle {
                unit: unit.unit_id,
                symbol: symbol.id,
            };
            if let Some(value_id) = value_ids_by_symbol.get(&handle).copied()
                && symbol.decl_range == *range
            {
                return Some(value_id);
            }
        }
    }
    let direct = exact_reference_use_in_range(reference_uses, range)?;
    (values[direct.value.as_usize()].kind == DataflowValueKind::FieldSymbol).then_some(direct.value)
}

fn direct_field_symbol_values_in_range(
    reference_uses: &[ReferenceUse],
    range: &TextRange,
    values: &[RoutineDataflowValue],
) -> Vec<DataflowValueId> {
    reference_uses_in_range(reference_uses, range)
        .into_iter()
        .filter(|use_site| use_site.range == *range)
        .map(|use_site| use_site.value)
        .filter(|value| values[value.as_usize()].kind == DataflowValueKind::FieldSymbol)
        .collect()
}

fn direct_non_field_symbol_values_in_range(
    reference_uses: &[ReferenceUse],
    range: &TextRange,
    values: &[RoutineDataflowValue],
) -> Vec<DataflowValueId> {
    sorted_unique_value_ids(
        reference_uses_in_range(reference_uses, range)
            .into_iter()
            .map(|use_site| use_site.value)
            .filter(|value| values[value.as_usize()].kind != DataflowValueKind::FieldSymbol),
    )
}

fn intersect_predecessor_bits(
    predecessors: &[RoutineBlockId],
    block_exit_bits: &[DenseBitSet],
    bit_count: usize,
) -> DenseBitSet {
    let Some((first, rest)) = predecessors.split_first() else {
        return DenseBitSet::new(bit_count);
    };
    let mut out = block_exit_bits[first.as_usize()].clone();
    for predecessor in rest {
        for (slot, other) in out
            .words
            .iter_mut()
            .zip(&block_exit_bits[predecessor.as_usize()].words)
        {
            *slot &= *other;
        }
    }
    out
}

fn union_predecessor_bits(
    predecessors: &[RoutineBlockId],
    block_exit_bits: &[DenseBitSet],
    bit_count: usize,
) -> DenseBitSet {
    let mut out = DenseBitSet::new(bit_count);
    for predecessor in predecessors {
        out.union_from(&block_exit_bits[predecessor.as_usize()]);
    }
    out
}

fn union_structure_field_masks(target: &mut [u64], source: &[u64]) {
    for (slot, other) in target.iter_mut().zip(source) {
        *slot |= *other;
    }
}

fn intersect_predecessor_structure_fields(
    predecessors: &[RoutineBlockId],
    block_exit_fields: &[Vec<u64>],
    value_count: usize,
) -> Vec<u64> {
    let Some((first, rest)) = predecessors.split_first() else {
        return vec![0u64; value_count];
    };
    let mut out = block_exit_fields[first.as_usize()].clone();
    for predecessor in rest {
        for (slot, other) in out
            .iter_mut()
            .zip(&block_exit_fields[predecessor.as_usize()])
        {
            *slot &= *other;
        }
    }
    out
}

fn apply_block_transfer(
    block: &RoutineBlock,
    instruction_transfers: &[InstructionTransfer],
    mut assigned: DenseBitSet,
    mut structure_fields: Vec<u64>,
    mut bound: DenseBitSet,
    mut maybe_written: DenseBitSet,
    mut known_non_initial: DenseBitSet,
    mut known_non_initial_fields: Vec<u64>,
    structure_assignment_trackers: &[Option<StructureAssignmentTracker>],
) -> (
    DenseBitSet,
    Vec<u64>,
    DenseBitSet,
    DenseBitSet,
    DenseBitSet,
    Vec<u64>,
) {
    for instruction_id in &block.instructions {
        let transfer = &instruction_transfers[instruction_id.as_usize()];
        apply_instruction_transfer(
            transfer,
            &mut assigned,
            &mut structure_fields,
            &mut bound,
            &mut maybe_written,
            &mut known_non_initial,
            &mut known_non_initial_fields,
            structure_assignment_trackers,
        );
    }
    (
        assigned,
        structure_fields,
        bound,
        maybe_written,
        known_non_initial,
        known_non_initial_fields,
    )
}

fn apply_instruction_transfer(
    transfer: &InstructionTransfer,
    assigned: &mut DenseBitSet,
    structure_fields: &mut [u64],
    bound: &mut DenseBitSet,
    maybe_written: &mut DenseBitSet,
    known_non_initial: &mut DenseBitSet,
    known_non_initial_fields: &mut [u64],
    structure_assignment_trackers: &[Option<StructureAssignmentTracker>],
) {
    for value in &transfer.writes {
        maybe_written.insert(*value);
    }
    for value in &transfer.assigned_writes {
        assigned.insert(*value);
        if let Some(tracker) = structure_assignment_trackers[value.as_usize()].as_ref() {
            structure_fields[value.as_usize()] = tracker.full_mask;
        }
    }
    for field_write in &transfer.structure_field_writes {
        let slot = &mut structure_fields[field_write.value.as_usize()];
        *slot |= field_write.mask;
        if structure_assignment_trackers[field_write.value.as_usize()]
            .as_ref()
            .is_some_and(|tracker| *slot == tracker.full_mask)
        {
            assigned.insert(field_write.value);
        }
    }
    for value in &transfer.non_initial_kills {
        known_non_initial.remove(*value);
        known_non_initial_fields[value.as_usize()] = 0;
    }
    for binding in &transfer.field_symbol_binding {
        match *binding {
            FieldSymbolBindingTransfer::Set(target) => bound.insert(target),
            FieldSymbolBindingTransfer::Copy { target, source } => {
                if bound.contains(source) {
                    bound.insert(target);
                } else {
                    bound.remove(target);
                }
            }
            FieldSymbolBindingTransfer::Clear(target) => bound.remove(target),
        }
    }
}

fn is_value_read_definitely_assigned(
    value: DataflowValueId,
    assigned: &DenseBitSet,
    structure_fields: &[u64],
    structure_assignment_trackers: &[Option<StructureAssignmentTracker>],
) -> bool {
    if assigned.contains(value) {
        return true;
    }
    structure_assignment_trackers[value.as_usize()]
        .as_ref()
        .is_some_and(|_| structure_fields[value.as_usize()] != 0)
}

fn is_structure_field_definitely_assigned(
    field_read: &StructureFieldRead,
    assigned: &DenseBitSet,
    structure_fields: &[u64],
) -> bool {
    assigned.contains(field_read.value)
        || (structure_fields[field_read.value.as_usize()] & field_read.mask) == field_read.mask
}

fn bitset_to_value_ids(bits: &DenseBitSet) -> Vec<DataflowValueId> {
    let mut out = Vec::new();
    for (word_idx, word) in bits.words.iter().copied().enumerate() {
        let mut remaining = word;
        while remaining != 0 {
            let bit = remaining.trailing_zeros() as usize;
            out.push(DataflowValueId((word_idx * 64 + bit) as u32));
            remaining &= remaining - 1;
        }
    }
    out
}

fn bitset_to_value_ids_matching(
    bits: &DenseBitSet,
    values: &[RoutineDataflowValue],
    kind: DataflowValueKind,
) -> Vec<DataflowValueId> {
    bitset_to_value_ids(bits)
        .into_iter()
        .filter(|value| values[value.as_usize()].kind == kind)
        .collect()
}

fn top_structure_field_masks(
    structure_assignment_trackers: &[Option<StructureAssignmentTracker>],
) -> Vec<u64> {
    structure_assignment_trackers
        .iter()
        .map(|tracker| tracker.as_ref().map_or(0, |tracker| tracker.full_mask))
        .collect()
}

fn definitely_assigned_value_ids(
    bits: &DenseBitSet,
    structure_fields: &[u64],
    structure_assignment_trackers: &[Option<StructureAssignmentTracker>],
) -> Vec<DataflowValueId> {
    let mut out = bitset_to_value_ids(bits);
    for (idx, tracker) in structure_assignment_trackers.iter().enumerate() {
        if tracker
            .as_ref()
            .is_some_and(|tracker| structure_fields[idx] == tracker.full_mask)
        {
            out.push(DataflowValueId(idx as u32));
        }
    }
    out.sort_by_key(|value| value.as_usize());
    out.dedup();
    out
}

fn sorted_unique_value_ids(
    values: impl IntoIterator<Item = DataflowValueId>,
) -> Vec<DataflowValueId> {
    let mut values: Vec<_> = values.into_iter().collect();
    values.sort_by_key(|value| value.as_usize());
    values.dedup();
    values
}

fn enclosing_routine_id(
    unit: &UnitAnalysis,
    exact_routine_scopes: &[Option<RoutineId>],
    scope: ScopeId,
) -> Option<RoutineId> {
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        if let Some(routine_id) = exact_routine_scopes
            .get(scope_id.as_usize())
            .copied()
            .flatten()
        {
            return Some(routine_id);
        }
        current = unit
            .scopes
            .get(scope_id.as_usize())
            .and_then(|scope_data| scope_data.parent);
    }
    None
}

fn instruction_kind_sort_key(kind: RoutineInstructionKind) -> u8 {
    match kind {
        RoutineInstructionKind::Assignment => 0,
        RoutineInstructionKind::Call => 1,
        RoutineInstructionKind::Perform => 2,
        RoutineInstructionKind::SqlQuery => 3,
        RoutineInstructionKind::Clear => 4,
        RoutineInstructionKind::Delete => 5,
        RoutineInstructionKind::ReadTable => 6,
        RoutineInstructionKind::FieldSymbolBind => 7,
        RoutineInstructionKind::ValueRead => 8,
        RoutineInstructionKind::UnknownEffect => 9,
        RoutineInstructionKind::Branch => 10,
        RoutineInstructionKind::LoopHeader => 11,
        RoutineInstructionKind::Terminator => 12,
    }
}

fn instruction_site_sort_key(site: RoutineInstructionSite) -> u32 {
    match site {
        RoutineInstructionSite::Assignment { index }
        | RoutineInstructionSite::Call { index }
        | RoutineInstructionSite::Perform { index }
        | RoutineInstructionSite::SqlQuery { index }
        | RoutineInstructionSite::Clear { index }
        | RoutineInstructionSite::Delete { index }
        | RoutineInstructionSite::ReadTable { index }
        | RoutineInstructionSite::FieldSymbolBind { index } => index,
        RoutineInstructionSite::ValueRead { reference } => reference.0,
        RoutineInstructionSite::UnknownEffect => 0,
        RoutineInstructionSite::Branch { kind } => match kind {
            RoutineBranchKind::If => 0,
            RoutineBranchKind::Case => 1,
            RoutineBranchKind::Try => 2,
        },
        RoutineInstructionSite::LoopHeader { kind } => match kind {
            crate::RoutineLoopKind::While => 0,
            crate::RoutineLoopKind::Do => 1,
            crate::RoutineLoopKind::Loop => 2,
        },
        RoutineInstructionSite::Terminator { kind } => match kind {
            RoutineTerminatorKind::Return => 0,
            RoutineTerminatorKind::Raise => 1,
            RoutineTerminatorKind::Leave => 2,
            RoutineTerminatorKind::LeaveListProcessing => 3,
            RoutineTerminatorKind::Exit => 4,
            RoutineTerminatorKind::Continue => 5,
        },
    }
}

fn routine_kind(kind: ScopeKind) -> Option<RoutineKind> {
    match kind {
        ScopeKind::Method => Some(RoutineKind::Method),
        ScopeKind::Form => Some(RoutineKind::Form),
        ScopeKind::Module => Some(RoutineKind::Module),
        ScopeKind::EventBlock => Some(RoutineKind::EventBlock),
        ScopeKind::File
        | ScopeKind::Class
        | ScopeKind::Interface
        | ScopeKind::IfBranch
        | ScopeKind::ElseifBranch
        | ScopeKind::ElseBranch
        | ScopeKind::WhenBranch
        | ScopeKind::CatchClause
        | ScopeKind::CleanupClause
        | ScopeKind::WhileBlock
        | ScopeKind::DoBlock
        | ScopeKind::LoopBlock
        | ScopeKind::TryBlock
        | ScopeKind::SelectBlock => None,
    }
}

fn synthetic_routine_name(kind: RoutineKind, scope: ScopeId) -> Arc<str> {
    Arc::from(format!(
        "<{}:{}>",
        match kind {
            RoutineKind::Method => "method",
            RoutineKind::Form => "form",
            RoutineKind::Module => "module",
            RoutineKind::EventBlock => "event",
        },
        scope.0
    ))
}

fn trackable_symbol_kind(kind: SymbolKind) -> bool {
    matches!(
        kind,
        SymbolKind::Variable
            | SymbolKind::Parameter
            | SymbolKind::FieldSymbol
            | SymbolKind::Constant
    )
}

fn dataflow_value_kind(kind: SymbolKind) -> DataflowValueKind {
    match kind {
        SymbolKind::Parameter => DataflowValueKind::Parameter,
        SymbolKind::Variable => DataflowValueKind::Variable,
        SymbolKind::FieldSymbol => DataflowValueKind::FieldSymbol,
        SymbolKind::Constant => DataflowValueKind::Constant,
        SymbolKind::BuiltinType
        | SymbolKind::BuiltinRoutine
        | SymbolKind::BuiltinConstant
        | SymbolKind::BuiltinVariable
        | SymbolKind::TypeDef
        | SymbolKind::Form
        | SymbolKind::Class
        | SymbolKind::Interface
        | SymbolKind::Method
        | SymbolKind::Field
        | SymbolKind::Include
        | SymbolKind::Event
        | SymbolKind::Module
        | SymbolKind::Control
        | SymbolKind::Report => DataflowValueKind::Other,
    }
}

fn leave_list_processing_is_guaranteed_exit(descriptor: &RoutineDescriptor) -> bool {
    descriptor.kind == RoutineKind::EventBlock
        && matches!(
            descriptor.name.as_ref(),
            "initialization"
                | "start-of-selection"
                | "end-of-selection"
                | "top-of-page"
                | "end-of-page"
        )
}

fn control_key(scope: ScopeId, range: &TextRange, tag: u8) -> ControlKey {
    ControlKey {
        scope,
        start: range.start,
        end: range.end,
        tag,
    }
}

fn loop_tag(data: &LoopRegionData) -> u8 {
    loop_tag_kind(data.kind)
}

fn loop_tag_kind(kind: crate::RoutineLoopKind) -> u8 {
    match kind {
        crate::RoutineLoopKind::While => 3,
        crate::RoutineLoopKind::Do => 4,
        crate::RoutineLoopKind::Loop => 5,
    }
}

fn push_unique(values: &mut Vec<RoutineBlockId>, value: RoutineBlockId) {
    if !values.contains(&value) {
        values.push(value);
    }
}

fn zero_range(offset: usize) -> TextRange {
    offset..offset
}
