mod cfg;
mod dataflow;
mod ids;
mod ir;
mod metrics;

use std::collections::{HashMap, HashSet, VecDeque};
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
    AtRegionData, CaseRegionData, Diagnostic, DiagnosticKind, FieldSymbolStateCheckKind,
    FormParameterSection, FunctionModuleParameterSection, IfRegionData, InternalTableOrderData,
    LoopRegionData, MethodParameterSection, NamedArgumentSection, PerformParameterSection,
    ReadTableBinarySearchData, Resolution, RoutineControlRegionData, RoutineSiteKind, SymbolData,
    SymbolKind, SystemFieldStatementKind, TryRegionData, UnitAnalysis, ValueFlowKind,
    ValueFlowTargetData, ValueStateCheckKind,
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
            .map_or([].as_slice(), Vec::as_slice)
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
            if kind == RoutineKind::GlobalDeclarations
                && !scope_has_global_declaration_activity(unit, scope.id)
            {
                continue;
            }
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
        out.scope_to_routine[unit_idx] =
            build_scope_to_routine_map(unit, &exact_routine_scopes[unit_idx]);
    }
    out.metrics.index_micros = index_timer.elapsed().as_micros();

    let ir_timer = std::time::Instant::now();
    for unit in &project.units {
        let scope_map = &out.scope_to_routine[unit.unit_id.as_usize()];
        let call_range_index = RangeContainmentIndex::from_call_sites(unit);

        for reference in unit.references.iter().filter(|reference| {
            reference.namespace == Namespace::Value
                && !matches!(
                    reference.kind,
                    crate::ReferenceKind::TypeRef | crate::ReferenceKind::StructuredDeclEnd
                )
                // Structured instruction sites already model execution of nested references.
                && !call_range_index.contains(reference.scope, &reference.range)
        }) {
            push_routine_instruction(
                &mut out.routines,
                scope_map,
                reference.scope,
                reference.range.clone(),
                RoutineInstructionSite::ValueRead {
                    reference: reference.id,
                },
            );
        }

        for (idx, assignment) in unit.assignment_sites.iter().enumerate() {
            push_routine_instruction(
                &mut out.routines,
                scope_map,
                assignment.scope,
                assignment.range.clone(),
                RoutineInstructionSite::Assignment { index: idx as u32 },
            );
        }

        for (idx, call_site) in unit.call_sites.iter().enumerate() {
            push_routine_instruction(
                &mut out.routines,
                scope_map,
                call_site.scope,
                call_site.range.clone(),
                RoutineInstructionSite::Call { index: idx as u32 },
            );
        }

        for (idx, perform_call) in unit.perform_calls.iter().enumerate() {
            push_routine_instruction(
                &mut out.routines,
                scope_map,
                perform_call.scope,
                perform_call.range.clone(),
                RoutineInstructionSite::Perform { index: idx as u32 },
            );
        }

        for (idx, find_site) in unit.find_sites.iter().enumerate() {
            push_routine_instruction(
                &mut out.routines,
                scope_map,
                find_site.scope,
                find_site.range.clone(),
                RoutineInstructionSite::Find { index: idx as u32 },
            );
        }

        for (idx, query) in unit.sql_queries.iter().enumerate() {
            push_routine_instruction(
                &mut out.routines,
                scope_map,
                query.scope,
                query.range.clone(),
                RoutineInstructionSite::SqlQuery { index: idx as u32 },
            );
        }

        for (idx, edge) in unit.value_flow_edges.iter().enumerate() {
            if !matches!(
                edge.kind,
                crate::ValueFlowKind::FieldSymbolAssignment
                    | crate::ValueFlowKind::ConditionalFieldSymbolAssignment
            ) {
                continue;
            }
            let target_range = match &edge.target {
                crate::ValueFlowTargetData::FieldSymbol { range, .. } => range,
                _ => continue,
            };
            let range = edge.source_range.start.min(target_range.start)
                ..edge.source_range.end.max(target_range.end);
            push_routine_instruction(
                &mut out.routines,
                scope_map,
                edge.scope,
                range,
                RoutineInstructionSite::FieldSymbolBind { index: idx as u32 },
            );
        }

        for (idx, site) in unit.routine_sites.iter().enumerate() {
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
                RoutineSiteKind::Stop => RoutineInstructionSite::Terminator {
                    kind: RoutineTerminatorKind::Stop,
                },
            };
            push_routine_instruction(
                &mut out.routines,
                scope_map,
                site.scope,
                site.range.clone(),
                instruction_site,
            );
        }

        for region in &unit.routine_control_regions {
            let instruction_site = match region {
                RoutineControlRegionData::If(_) => RoutineInstructionSite::Branch {
                    kind: RoutineBranchKind::If,
                },
                RoutineControlRegionData::Case(_) => RoutineInstructionSite::Branch {
                    kind: RoutineBranchKind::Case,
                },
                RoutineControlRegionData::At(_) => RoutineInstructionSite::Branch {
                    kind: RoutineBranchKind::At,
                },
                RoutineControlRegionData::Try(_) => RoutineInstructionSite::Branch {
                    kind: RoutineBranchKind::Try,
                },
                RoutineControlRegionData::Loop(data) => {
                    RoutineInstructionSite::LoopHeader { kind: data.kind }
                }
            };
            push_routine_instruction(
                &mut out.routines,
                scope_map,
                region.scope(),
                region.range().clone(),
                instruction_site,
            );
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

    let control_region_indexes: Vec<_> = project
        .units
        .iter()
        .map(|unit| {
            build_routine_control_region_index(unit, &out.scope_to_routine[unit.unit_id.as_usize()])
        })
        .collect();

    let cfg_timer = std::time::Instant::now();
    for routine_idx in 0..out.routines.len() {
        let descriptor = out.routines[routine_idx].descriptor.clone();
        let Some(unit) = project.units.get(descriptor.unit.as_usize()) else {
            continue;
        };
        let routine_control_regions = control_region_indexes[descriptor.unit.as_usize()]
            .get(&descriptor.id)
            .map_or([].as_slice(), Vec::as_slice);
        let (cfg, diagnostics) = build_routine_cfg_and_diagnostics(
            unit,
            routine_control_regions,
            &out.routines[routine_idx],
        );
        out.unit_diagnostics[descriptor.unit.as_usize()].extend(diagnostics.iter().cloned());
        out.routines[routine_idx].cfg = cfg;
        out.routines[routine_idx].diagnostics = diagnostics;
    }
    out.metrics.cfg_micros = cfg_timer.elapsed().as_micros();

    let dataflow_timer = std::time::Instant::now();
    let tracked_symbols_by_routine =
        build_tracked_symbols_by_routine(project, &out.scope_to_routine, out.routines.len());
    let call_argument_effects_by_unit: Vec<_> = project
        .units
        .iter()
        .map(|unit| build_call_argument_effects(project, unit))
        .collect();
    let has_perform_calls = project
        .units
        .iter()
        .any(|unit| !unit.perform_calls.is_empty());
    let routine_has_perform_instruction: Vec<_> = out
        .routines
        .iter()
        .map(|routine| {
            routine.ir.instructions.iter().any(|instruction| {
                matches!(instruction.site, RoutineInstructionSite::Perform { .. })
            })
        })
        .collect();
    out.metrics.perform_routine_count = routine_has_perform_instruction
        .iter()
        .filter(|has_perform| **has_perform)
        .count();
    let max_dataflow_passes = if has_perform_calls { 6 } else { 1 };
    let mut form_parameter_effects = HashMap::new();
    let mut final_dataflow_diagnostics = vec![Vec::new(); out.routines.len()];

    for pass_idx in 0..max_dataflow_passes {
        out.metrics.dataflow_pass_count += 1;
        for routine_id in 0..out.routines.len() {
            if pass_idx > 0 && !routine_has_perform_instruction[routine_id] {
                continue;
            }
            let descriptor = out.routines[routine_id].descriptor.clone();
            let Some(unit) = project.units.get(descriptor.unit.as_usize()) else {
                continue;
            };
            let routine_control_regions = control_region_indexes[descriptor.unit.as_usize()]
                .get(&descriptor.id)
                .map_or([].as_slice(), Vec::as_slice);
            out.metrics.dataflow_routine_runs += 1;
            let (inputs, result, diagnostics, dead_store_micros) = build_routine_dataflow(
                project,
                unit,
                routine_control_regions,
                &out.routines[routine_id],
                &tracked_symbols_by_routine[routine_id],
                &call_argument_effects_by_unit[descriptor.unit.as_usize()],
                &form_parameter_effects,
            );
            out.metrics.dead_store_micros += dead_store_micros;
            out.routines[routine_id].dataflow_inputs = inputs;
            out.routines[routine_id].dataflow_result = result;
            final_dataflow_diagnostics[routine_id] = diagnostics;
        }

        if !has_perform_calls {
            break;
        }

        let next_effects = build_form_parameter_effect_summaries(project, &out.routines);
        if next_effects == form_parameter_effects {
            break;
        }
        form_parameter_effects = next_effects;
    }

    for (routine_id, diagnostics) in final_dataflow_diagnostics.into_iter().enumerate() {
        let descriptor = out.routines[routine_id].descriptor.clone();
        out.routines[routine_id]
            .diagnostics
            .extend(diagnostics.iter().cloned());
        out.unit_diagnostics[descriptor.unit.as_usize()].extend(diagnostics);
    }
    out.metrics.dataflow_micros = dataflow_timer.elapsed().as_micros();

    for (routine_id, diagnostic) in
        build_read_table_binary_search_order_diagnostics(project, &out.scope_to_routine)
    {
        let Some(routine) = out.routines.get_mut(routine_id.as_usize()) else {
            continue;
        };
        let unit_idx = routine.descriptor.unit.as_usize();
        routine.diagnostics.push(diagnostic.clone());
        out.unit_diagnostics[unit_idx].push(diagnostic);
    }
    for routine in &mut out.routines {
        sort_diagnostics(&mut routine.diagnostics);
    }
    for diagnostics in &mut out.unit_diagnostics {
        sort_diagnostics(diagnostics);
    }

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

fn push_routine_instruction(
    routines: &mut [RoutineAnalysis],
    scope_map: &[Option<RoutineId>],
    scope: ScopeId,
    range: TextRange,
    site: RoutineInstructionSite,
) {
    let Some(routine_id) = scope_map.get(scope.as_usize()).copied().flatten() else {
        return;
    };
    routines[routine_id.as_usize()]
        .ir
        .instructions
        .push(RoutineInstruction {
            id: RoutineInstrId(0),
            scope,
            range,
            site,
        });
}

fn compare_diagnostics(left: &Diagnostic, right: &Diagnostic) -> std::cmp::Ordering {
    left.range
        .start
        .cmp(&right.range.start)
        .then(left.range.end.cmp(&right.range.end))
        .then(left.message.cmp(&right.message))
}

fn sort_diagnostics(diagnostics: &mut Vec<Diagnostic>) {
    diagnostics.sort_by(compare_diagnostics);
    diagnostics.dedup();
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
    suppress_definite_assignment: bool,
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
struct ConditionalAssignedTarget {
    value: DataflowValueId,
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
    AssignsOnly,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
struct FormParameterEffectSummary {
    reads_before_write: bool,
    may_write: bool,
}

type CallArgumentEffectMap = HashMap<(usize, usize, usize, usize), CallArgumentEffect>;

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
    at_regions: HashMap<ControlKey, &'a AtRegionData>,
    try_regions: HashMap<ControlKey, &'a TryRegionData>,
    loop_regions: HashMap<ControlKey, &'a LoopRegionData>,
}

#[derive(Debug, Clone, Copy)]
struct RangeContainmentEntry {
    start: usize,
    prefix_max_end: usize,
}

#[derive(Debug, Default)]
struct RangeContainmentIndex {
    by_scope: Vec<Vec<RangeContainmentEntry>>,
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
        control_regions: &[&'a RoutineControlRegionData],
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
        let mut at_regions = HashMap::new();
        let mut try_regions = HashMap::new();
        let mut loop_regions = HashMap::new();
        for &region in control_regions {
            match region {
                RoutineControlRegionData::If(data) => {
                    if_regions.insert(control_key(data.scope, &data.range, 0), data);
                }
                RoutineControlRegionData::Case(data) => {
                    case_regions.insert(control_key(data.scope, &data.range, 1), data);
                }
                RoutineControlRegionData::At(data) => {
                    at_regions.insert(control_key(data.scope, &data.range, 2), data);
                }
                RoutineControlRegionData::Try(data) => {
                    try_regions.insert(control_key(data.scope, &data.range, 3), data);
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
            at_regions,
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

    fn at_region(&self, instruction: &RoutineInstruction) -> Option<&'a AtRegionData> {
        self.at_regions
            .get(&control_key(instruction.scope, &instruction.range, 2))
            .copied()
    }

    fn try_region(&self, instruction: &RoutineInstruction) -> Option<&'a TryRegionData> {
        self.try_regions
            .get(&control_key(instruction.scope, &instruction.range, 3))
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
                | RoutineInstructionSite::Find { .. }
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
            RoutineBranchKind::At => self.handle_at(state, instruction),
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

    fn handle_at(&mut self, state: ScopeExit, instruction: &RoutineInstruction) -> ScopeExit {
        let Some(region) = self.index.at_region(instruction).cloned() else {
            self.append_instruction(state.block, instruction.id);
            return state;
        };
        self.append_instruction(state.block, instruction.id);
        let join = self.new_block(RoutineBlockKind::Body, zero_range(instruction.range.end));
        self.add_edge(state.block, join, RoutineEdgeKind::Branch);
        let at_entry = self.new_block(RoutineBlockKind::Body, zero_range(region.range.start));
        self.add_edge(state.block, at_entry, RoutineEdgeKind::Branch);

        let at_exit = self.build_scope(
            region.body_scope,
            ScopeExit {
                block: at_entry,
                reachable: state.reachable,
            },
        );
        let mut join_reachable = state.reachable;
        if at_exit.reachable {
            self.add_edge(at_exit.block, join, RoutineEdgeKind::Fallthrough);
            join_reachable = true;
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
            RoutineTerminatorKind::Stop => {
                self.add_edge(state.block, self.exit, RoutineEdgeKind::Stop);
                self.new_disconnected_successor(instruction.range.end)
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

impl RangeContainmentIndex {
    fn from_call_sites(unit: &UnitAnalysis) -> Self {
        let mut by_scope = vec![Vec::new(); unit.scopes.len()];
        for call_site in &unit.call_sites {
            if let Some(entries) = by_scope.get_mut(call_site.scope.as_usize()) {
                entries.push(RangeContainmentEntry {
                    start: call_site.range.start,
                    prefix_max_end: call_site.range.end,
                });
            }
        }
        for entries in &mut by_scope {
            entries.sort_by(|left, right| {
                left.start
                    .cmp(&right.start)
                    .then(left.prefix_max_end.cmp(&right.prefix_max_end))
            });
            let mut max_end = 0;
            for entry in entries {
                max_end = max_end.max(entry.prefix_max_end);
                entry.prefix_max_end = max_end;
            }
        }
        Self { by_scope }
    }

    fn contains(&self, scope: ScopeId, inner: &TextRange) -> bool {
        let Some(entries) = self.by_scope.get(scope.as_usize()) else {
            return false;
        };
        let end_idx = entries.partition_point(|entry| entry.start <= inner.start);
        end_idx > 0 && entries[end_idx - 1].prefix_max_end >= inner.end
    }
}

fn build_routine_cfg_and_diagnostics(
    unit: &UnitAnalysis,
    control_regions: &[&RoutineControlRegionData],
    routine: &RoutineAnalysis,
) -> (RoutineCfg, Vec<Diagnostic>) {
    let index = RoutineBuildIndex::new(unit, control_regions, routine);
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
    diagnostics.sort_by(compare_diagnostics);
    diagnostics
}

fn build_routine_dataflow(
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    control_regions: &[&RoutineControlRegionData],
    routine: &RoutineAnalysis,
    tracked_symbols: &[&SymbolData],
    call_argument_effects: &CallArgumentEffectMap,
    form_parameter_effects: &HashMap<SymbolHandle, FormParameterEffectSummary>,
) -> (
    RoutineDataflowInputs,
    RoutineDataflowResult,
    Vec<Diagnostic>,
    u128,
) {
    let routine_index = RoutineBuildIndex::new(unit, control_regions, routine);

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

    let mut reference_uses = unit
        .references
        .iter()
        .filter(|reference| {
            reference.namespace == Namespace::Value
                && !matches!(
                    reference.kind,
                    crate::ReferenceKind::TypeRef | crate::ReferenceKind::StructuredDeclEnd
                )
        })
        .filter_map(|reference| {
            resolved_value_id_for_reference(unit, reference.id, &value_ids_by_symbol).map(|value| {
                ReferenceUse {
                    reference: reference.id,
                    range: reference.range.clone(),
                    value,
                }
            })
        })
        .collect::<Vec<_>>();
    reference_uses.sort_by(|left, right| {
        left.range
            .start
            .cmp(&right.range.start)
            .then(left.range.end.cmp(&right.range.end))
            .then(left.reference.0.cmp(&right.reference.0))
            .then(left.value.as_usize().cmp(&right.value.as_usize()))
    });

    let safe_field_symbol_checks =
        resolve_safe_field_symbol_checks(unit, &reference_uses, &value_ids_by_symbol);
    let safe_value_state_checks =
        resolve_safe_value_state_checks(unit, &reference_uses, &value_ids_by_symbol);
    let condition_probe_reads =
        resolve_condition_probe_reads(unit, &reference_uses, &value_ids_by_symbol);
    let mut safe_loop_field_refs =
        resolve_safe_loop_where_field_refs(project, unit, &reference_uses, &values);
    safe_loop_field_refs.extend(resolve_safe_loop_at_field_refs(
        project,
        unit,
        &reference_uses,
        &values,
    ));
    let is_not_initial_scope_refinements = resolve_is_not_initial_scope_refinements(
        unit,
        &reference_uses,
        &value_ids_by_symbol,
        values.len(),
    );
    let is_not_initial_field_scope_refinements = resolve_is_not_initial_field_scope_refinements(
        project,
        unit,
        &reference_uses,
        &value_ids_by_symbol,
        &structure_assignment_trackers,
        &values,
    );
    let mut sy_subrc_success_bound_scope_refinements =
        resolve_sy_subrc_success_bound_scope_refinements(
            unit,
            &reference_uses,
            &value_ids_by_symbol,
            &values,
            values.len(),
        );
    union_dense_scope_refinements(
        &mut sy_subrc_success_bound_scope_refinements,
        &resolve_is_assigned_bound_scope_refinements(
            unit,
            &reference_uses,
            &value_ids_by_symbol,
            values.len(),
        ),
    );
    let sy_subrc_success_assigned_scope_refinements =
        resolve_sy_subrc_success_assigned_scope_refinements(
            unit,
            &reference_uses,
            &value_ids_by_symbol,
            &structure_assignment_trackers,
            &values,
            values.len(),
        );
    let sy_subrc_success_structure_field_scope_refinements =
        resolve_sy_subrc_success_structure_field_scope_refinements(
            unit,
            &reference_uses,
            &value_ids_by_symbol,
            &structure_assignment_trackers,
            &values,
        );
    let structure_field_reads = resolve_structure_field_reads(
        project,
        unit,
        &reference_uses,
        &structure_assignment_trackers,
        &values,
    );
    let block_non_initial_entry_bits = block_non_initial_entry_refinements(
        unit,
        routine,
        &is_not_initial_scope_refinements,
        values.len(),
    );
    let block_non_initial_field_masks = block_non_initial_field_entry_refinements(
        unit,
        routine,
        &is_not_initial_field_scope_refinements,
        values.len(),
    );
    let mut block_bound_entry_bits = block_bound_entry_refinements(
        unit,
        routine,
        &sy_subrc_success_bound_scope_refinements,
        values.len(),
    );
    let mut block_assigned_entry_refinements = block_non_initial_entry_refinements(
        unit,
        routine,
        &sy_subrc_success_assigned_scope_refinements,
        values.len(),
    );
    let mut block_assigned_field_entry_refinements = block_non_initial_field_entry_refinements(
        unit,
        routine,
        &sy_subrc_success_structure_field_scope_refinements,
        values.len(),
    );
    let (
        sy_subrc_guard_assigned_block_refinements,
        sy_subrc_guard_structure_field_block_refinements,
        sy_subrc_guard_bound_block_refinements,
    ) = resolve_sy_subrc_success_guard_block_refinements(
        unit,
        routine,
        &reference_uses,
        &value_ids_by_symbol,
        &structure_assignment_trackers,
        &values,
    );
    union_dense_scope_refinements(
        &mut block_assigned_entry_refinements,
        &sy_subrc_guard_assigned_block_refinements,
    );
    union_structure_field_refinements(
        &mut block_assigned_field_entry_refinements,
        &sy_subrc_guard_structure_field_block_refinements,
    );
    union_dense_scope_refinements(
        &mut block_bound_entry_bits,
        &sy_subrc_guard_bound_block_refinements,
    );
    let mut safe_read_refs = safe_field_symbol_checks;
    safe_read_refs.extend(safe_value_state_checks);
    safe_read_refs.extend(safe_loop_field_refs);
    let mut suppressed_refs = vec![false; unit.references.len()];
    for instruction in &routine.ir.instructions {
        match instruction.site {
            RoutineInstructionSite::Assignment { index } => {
                if let Some(assignment) = unit.assignment_sites.get(index as usize) {
                    mark_reference_ids_in_range(
                        &mut suppressed_refs,
                        &reference_uses,
                        &assignment.lhs_range,
                    );
                    mark_reference_ids_in_range(
                        &mut suppressed_refs,
                        &reference_uses,
                        &assignment.rhs_range,
                    );
                }
            }
            RoutineInstructionSite::Call { index } => {
                if let Some(call_site) = unit.call_sites.get(index as usize) {
                    mark_reference_ids_in_range(
                        &mut suppressed_refs,
                        &reference_uses,
                        &call_site.range,
                    );
                }
            }
            RoutineInstructionSite::Clear { index } => {
                if let Some(site) = unit.routine_sites.get(index as usize) {
                    mark_reference_ids_in_range(&mut suppressed_refs, &reference_uses, &site.range);
                }
            }
            RoutineInstructionSite::Delete { index } => {
                if let Some(site) = unit.routine_sites.get(index as usize) {
                    mark_reference_ids_in_range(&mut suppressed_refs, &reference_uses, &site.range);
                }
            }
            RoutineInstructionSite::ReadTable { index } => {
                if let Some(site) = unit.routine_sites.get(index as usize) {
                    mark_reference_ids_in_range(&mut suppressed_refs, &reference_uses, &site.range);
                }
            }
            RoutineInstructionSite::Find { index } => {
                if let Some(site) = unit.find_sites.get(index as usize) {
                    for range in &site.read_ranges {
                        mark_reference_ids_in_range(&mut suppressed_refs, &reference_uses, range);
                    }
                    for target in &site.write_targets {
                        mark_reference_ids_in_range(
                            &mut suppressed_refs,
                            &reference_uses,
                            &target.range,
                        );
                    }
                }
            }
            RoutineInstructionSite::Perform { index } => {
                if let Some(perform_call) = unit.perform_calls.get(index as usize) {
                    mark_reference_ids_in_range(
                        &mut suppressed_refs,
                        &reference_uses,
                        &perform_call.range,
                    );
                }
            }
            RoutineInstructionSite::FieldSymbolBind { index } => {
                if let Some(edge) = unit.value_flow_edges.get(index as usize) {
                    mark_reference_ids_in_range(
                        &mut suppressed_refs,
                        &reference_uses,
                        &edge.source_range,
                    );
                    if let ValueFlowTargetData::FieldSymbol { range, .. } = &edge.target {
                        mark_reference_ids_in_range(&mut suppressed_refs, &reference_uses, range);
                    }
                }
            }
            RoutineInstructionSite::SqlQuery { index } => {
                for target in unit
                    .sql_targets
                    .iter()
                    .filter(|target| target.query_id == index as usize)
                {
                    mark_reference_ids_in_range(
                        &mut suppressed_refs,
                        &reference_uses,
                        sql_target_effective_range(target),
                    );
                }
            }
            RoutineInstructionSite::ValueRead { .. }
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
                if !reference_is_marked(&suppressed_refs, reference)
                    && let Some(value) =
                        resolved_value_id_for_reference(unit, reference, &value_ids_by_symbol)
                    && !safe_read_refs.contains(&reference)
                {
                    transfer.reads.push(ReadOccurrence {
                        reference,
                        range: instruction.range.clone(),
                        value,
                        suppress_definite_assignment: condition_probe_reads.contains(&reference),
                    });
                }
            }
            RoutineInstructionSite::Assignment { index } => {
                if let Some(assignment) = unit.assignment_sites.get(index as usize) {
                    let suppress_rhs_definite_assignment =
                        is_table_line_mutation_assignment(unit, &assignment.range);
                    transfer.reads.extend(read_occurrences_in_range(
                        &reference_uses,
                        &assignment.lhs_range,
                        &safe_read_refs,
                        false,
                    ));
                    transfer.reads.extend(read_occurrences_in_range(
                        &reference_uses,
                        &assignment.rhs_range,
                        &safe_read_refs,
                        suppress_rhs_definite_assignment,
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
                            !(read.value == write_value
                                && read.range.start >= assignment.lhs_range.start
                                && read.range.end <= assignment.lhs_range.end)
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
                            false,
                        ));
                    }
                    for argument in &call_site.arguments {
                        let effect = call_argument_effect_for_call_argument(
                            call_argument_effects,
                            call_site,
                            argument,
                        );
                        if matches!(
                            effect,
                            CallArgumentEffect::OutputOnly | CallArgumentEffect::AssignsOnly
                        ) {
                            transfer.reads.retain(|read| {
                                read.range.start < argument.range.start
                                    || read.range.end > argument.range.end
                            });
                        }
                        if matches!(
                            effect,
                            CallArgumentEffect::OutputOnly
                                | CallArgumentEffect::InOut
                                | CallArgumentEffect::AssignsOnly
                                | CallArgumentEffect::Unknown
                        ) {
                            let direct_values = direct_non_field_symbol_write_values_in_range(
                                unit,
                                &reference_uses,
                                &value_ids_by_symbol,
                                &argument.range,
                                &values,
                            );
                            if matches!(
                                effect,
                                CallArgumentEffect::OutputOnly
                                    | CallArgumentEffect::InOut
                                    | CallArgumentEffect::AssignsOnly
                            ) {
                                transfer.writes.extend(direct_values.iter().copied());
                            }
                            if matches!(
                                effect,
                                CallArgumentEffect::OutputOnly | CallArgumentEffect::AssignsOnly
                            ) {
                                transfer
                                    .assigned_writes
                                    .extend(direct_values.iter().copied());
                            }
                            transfer
                                .non_initial_kills
                                .extend(direct_values.iter().copied());
                        }
                        if matches!(
                            effect,
                            CallArgumentEffect::InOut
                                | CallArgumentEffect::AssignsOnly
                                | CallArgumentEffect::Unknown
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
            RoutineInstructionSite::Delete { .. } => {}
            RoutineInstructionSite::ReadTable { index } => {
                if let Some(site) = unit.routine_sites.get(index as usize) {
                    transfer.reads.extend(read_occurrences_in_range(
                        &reference_uses,
                        &site.range,
                        &safe_read_refs,
                        false,
                    ));
                    if let Some(target_range) = site.target_range.as_ref()
                        && let Some(write_value) =
                            direct_write_value_id_for_clear(&reference_uses, target_range, &values)
                    {
                        transfer.writes.push(write_value);
                        transfer.assigned_writes.push(write_value);
                        transfer.non_initial_kills.push(write_value);
                    }
                }
            }
            RoutineInstructionSite::Find { index } => {
                if let Some(site) = unit.find_sites.get(index as usize) {
                    for range in &site.read_ranges {
                        transfer.reads.extend(read_occurrences_in_range(
                            &reference_uses,
                            range,
                            &safe_read_refs,
                            false,
                        ));
                    }
                    for target in &site.write_targets {
                        if let Some(selector_write) = selector_structure_write_for_range(
                            unit,
                            &target.range,
                            &reference_uses,
                            &structure_assignment_trackers,
                        ) {
                            transfer.reads.retain(|read| {
                                !(read.value == selector_write.base_value
                                    && read.range.start >= target.range.start
                                    && read.range.end <= target.range.end)
                            });
                            transfer.writes.push(selector_write.base_value);
                            transfer.non_initial_kills.push(selector_write.base_value);
                            if target.definitely_assigned {
                                transfer.assigned_writes.push(selector_write.base_value);
                            }
                            if let Some(mask) = selector_write.field_mask {
                                transfer
                                    .structure_field_writes
                                    .push(StructureFieldWriteTransfer {
                                        value: selector_write.base_value,
                                        mask,
                                    });
                            }
                            continue;
                        }
                        if let Some(write_value) = direct_write_value_id_for_range(
                            unit,
                            &target.range,
                            &reference_uses,
                            &value_ids_by_symbol,
                            &values,
                        ) {
                            transfer.reads.retain(|read| {
                                !(read.range == target.range && read.value == write_value)
                            });
                            transfer.writes.push(write_value);
                            transfer.non_initial_kills.push(write_value);
                            if target.definitely_assigned {
                                transfer.assigned_writes.push(write_value);
                            }
                        }
                    }
                }
            }
            RoutineInstructionSite::Perform { index } => {
                if let Some(perform_call) = unit.perform_calls.get(index as usize) {
                    transfer.reads.extend(read_occurrences_in_range(
                        &reference_uses,
                        &perform_call.range,
                        &safe_read_refs,
                        false,
                    ));
                    for argument in &perform_call.arguments {
                        let effect = perform_argument_effect_for_argument(
                            project,
                            unit,
                            perform_call,
                            argument,
                            form_parameter_effects,
                        );
                        if argument.section != PerformParameterSection::Using
                            && effect.is_some_and(|summary| !summary.reads_before_write)
                        {
                            transfer.reads.retain(|read| {
                                read.range.start < argument.range.start
                                    || read.range.end > argument.range.end
                            });
                        }
                        if argument.section != PerformParameterSection::Using
                            && effect.is_some_and(|summary| summary.may_write)
                        {
                            let direct_values = direct_non_field_symbol_values_in_range(
                                &reference_uses,
                                &argument.range,
                                &values,
                            );
                            transfer.writes.extend(direct_values.iter().copied());
                            transfer.assigned_writes.extend(direct_values);
                        }
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
                    let ValueFlowTargetData::FieldSymbol { range, name } = &edge.target else {
                        continue;
                    };
                    let is_loop_target = routine_index.loop_regions.values().any(|region| {
                        region.body_scope == instruction.scope
                            && region.target_access.as_ref().is_some_and(|access| {
                                access.base_range == *range
                                    && name.as_ref().map_or(true, |name| {
                                        access.base_name.as_ref() == name.as_ref()
                                    })
                            })
                    });
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
                                            suppress_definite_assignment: false,
                                        });
                                    }
                                    transfer.field_symbol_binding.push(if is_loop_target {
                                        FieldSymbolBindingTransfer::Set(target_value)
                                    } else {
                                        FieldSymbolBindingTransfer::Copy {
                                            target: target_value,
                                            source: source_use.value,
                                        }
                                    });
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
                                    false,
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
                                false,
                            ));
                            transfer.field_symbol_binding.push(if is_loop_target {
                                FieldSymbolBindingTransfer::Set(target_value)
                            } else {
                                FieldSymbolBindingTransfer::Clear(target_value)
                            });
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
            RoutineInstructionSite::SqlQuery { index } => {
                for target in sql_query_conditional_assignment_targets(
                    unit,
                    index as usize,
                    &reference_uses,
                    &value_ids_by_symbol,
                    &structure_assignment_trackers,
                    &values,
                ) {
                    transfer.writes.push(target.value);
                    transfer.non_initial_kills.push(target.value);
                    if let Some(mask) = target.field_mask {
                        transfer
                            .structure_field_writes
                            .push(StructureFieldWriteTransfer {
                                value: target.value,
                                mask,
                            });
                    }
                }
            }
            RoutineInstructionSite::Branch { .. } | RoutineInstructionSite::Terminator { .. } => {}
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
        if value_is_definitely_assigned_on_entry(
            project,
            unit,
            value,
            &values,
            &structure_assignment_trackers,
        ) {
            entry_assigned.insert(value.id);
            if let Some(tracker) = structure_assignment_trackers[value.id.as_usize()].as_ref()
                && value_has_explicit_declaration_initializer(unit, value)
            {
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
            let mut next_entry_assigned = if !block.reachable {
                DenseBitSet::new(values.len())
            } else if block.kind == RoutineBlockKind::Entry {
                entry_assigned.clone()
            } else {
                intersect_predecessor_bits(&block.predecessors, &block_exit_assigned, values.len())
            };
            next_entry_assigned.union_from(&block_assigned_entry_refinements[block_idx]);
            let mut next_entry_structure_fields = if !block.reachable {
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
            union_structure_field_masks(
                &mut next_entry_structure_fields,
                &block_assigned_field_entry_refinements[block_idx],
            );
            let mut next_entry_bound = if !block.reachable || block.kind == RoutineBlockKind::Entry
            {
                DenseBitSet::new(values.len())
            } else {
                intersect_predecessor_bits(&block.predecessors, &block_exit_bound, values.len())
            };
            next_entry_bound.union_from(&block_bound_entry_bits[block_idx]);
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
            next_entry_non_initial.union_from(&block_non_initial_entry_bits[block_idx]);
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
                &block_non_initial_field_masks[block_idx],
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
                        if read.suppress_definite_assignment {
                            continue;
                        }
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
        call_argument_effects,
    ));
    sort_diagnostics(&mut diagnostics);

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
    call_argument_effects: &CallArgumentEffectMap,
) -> Vec<Diagnostic> {
    if values.is_empty() || routine.cfg.blocks.is_empty() {
        return Vec::new();
    }

    let tracked_values = build_dead_store_tracked_values(
        unit,
        routine,
        values,
        reference_uses,
        value_ids_by_symbol,
        call_argument_effects,
    );
    if !tracked_values.words.iter().any(|word| *word != 0) {
        return Vec::new();
    }
    let value_state_check_refs =
        resolve_safe_value_state_checks(unit, reference_uses, value_ids_by_symbol);

    let instruction_summaries = build_dead_store_instruction_summaries(
        unit,
        routine,
        values,
        reference_uses,
        value_ids_by_symbol,
        instruction_summaries,
        &tracked_values,
        call_argument_effects,
        &value_state_check_refs,
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
                    message: dead_store_message(routine, value),
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

    sort_diagnostics(&mut diagnostics);
    diagnostics
}

fn dead_store_message(routine: &RoutineAnalysis, value: &RoutineDataflowValue) -> String {
    match routine.descriptor.kind {
        RoutineKind::GlobalDeclarations => format!(
            "write to global variable '{}' is never read in global declarations",
            value.name
        ),
        RoutineKind::Method | RoutineKind::Form | RoutineKind::Module | RoutineKind::EventBlock => {
            format!(
                "write to local variable '{}' is never read in routine '{}'",
                value.name, routine.descriptor.name
            )
        }
    }
}

fn build_dead_store_tracked_values(
    unit: &UnitAnalysis,
    routine: &RoutineAnalysis,
    values: &[RoutineDataflowValue],
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
    call_argument_effects: &CallArgumentEffectMap,
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
                    for value in direct_non_field_symbol_write_values_in_range(
                        unit,
                        reference_uses,
                        value_ids_by_symbol,
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
            | RoutineInstructionSite::Find { .. }
            | RoutineInstructionSite::ValueRead { .. }
            | RoutineInstructionSite::Branch { .. }
            | RoutineInstructionSite::LoopHeader { .. }
            | RoutineInstructionSite::Terminator { .. } => {}
        }
    }

    tracked
}

fn call_argument_effect_for_call_argument(
    call_argument_effects: &CallArgumentEffectMap,
    call_site: &crate::CallSiteData,
    argument: &crate::CallArgumentData,
) -> CallArgumentEffect {
    let effect = call_argument_effects
        .get(&(
            call_site.range.start,
            call_site.range.end,
            argument.range.start,
            argument.range.end,
        ))
        .copied()
        .unwrap_or(CallArgumentEffect::Unknown);
    if effect == CallArgumentEffect::Unknown {
        call_argument_effect_from_section(argument.section)
    } else {
        effect
    }
}

fn call_argument_effect_from_section(section: Option<NamedArgumentSection>) -> CallArgumentEffect {
    match section {
        None | Some(NamedArgumentSection::Exporting | NamedArgumentSection::Exceptions) => {
            CallArgumentEffect::InputOnly
        }
        Some(NamedArgumentSection::Importing | NamedArgumentSection::Receiving) => {
            CallArgumentEffect::OutputOnly
        }
        Some(NamedArgumentSection::Changing) => CallArgumentEffect::InOut,
        Some(NamedArgumentSection::Tables) => CallArgumentEffect::AssignsOnly,
    }
}

fn build_form_parameter_effect_summaries(
    project: &ProjectAnalysis,
    routines: &[RoutineAnalysis],
) -> HashMap<SymbolHandle, FormParameterEffectSummary> {
    let mut out = HashMap::new();

    for routine in routines {
        let Some(owner) = routine.descriptor.owner else {
            continue;
        };
        let Some(unit) = project.units.get(owner.unit.as_usize()) else {
            continue;
        };
        if unit.symbol(owner.symbol).kind != SymbolKind::Form {
            continue;
        }
        let Some(form) = unit.form_routine(owner.symbol) else {
            continue;
        };
        if routine.cfg.blocks.is_empty() || routine.dataflow_inputs.values.is_empty() {
            continue;
        }

        let value_by_symbol: HashMap<_, _> = routine
            .dataflow_inputs
            .values
            .iter()
            .map(|value| (value.symbol, value.id))
            .collect();
        let mut parameter_values = HashMap::new();
        for parameter in &form.parameters {
            let symbol = SymbolHandle {
                unit: owner.unit,
                symbol: parameter.symbol,
            };
            let Some(value) = value_by_symbol.get(&symbol).copied() else {
                continue;
            };
            parameter_values.insert(value, symbol);
        }
        if parameter_values.is_empty() {
            continue;
        }

        let bit_count = routine.dataflow_inputs.values.len();
        let empty = DenseBitSet::new(bit_count);
        let top = DenseBitSet::filled(bit_count);
        let mut block_entry_written = vec![empty.clone(); routine.cfg.blocks.len()];
        let mut block_exit_written = routine
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

        let mut changed = true;
        while changed {
            changed = false;
            for block in &routine.cfg.blocks {
                let block_idx = block.id.as_usize();
                let next_entry_written = if !block.reachable
                    || block.kind == RoutineBlockKind::Entry
                {
                    DenseBitSet::new(bit_count)
                } else {
                    intersect_predecessor_bits(&block.predecessors, &block_exit_written, bit_count)
                };
                let mut next_exit_written = next_entry_written.clone();
                for instruction_id in &block.instructions {
                    if let Some(summary) = routine
                        .dataflow_inputs
                        .instructions
                        .get(instruction_id.as_usize())
                    {
                        for value in &summary.writes {
                            next_exit_written.insert(*value);
                        }
                    }
                }
                if block_entry_written[block_idx] != next_entry_written {
                    block_entry_written[block_idx] = next_entry_written;
                    changed = true;
                }
                if block_exit_written[block_idx] != next_exit_written {
                    block_exit_written[block_idx] = next_exit_written;
                    changed = true;
                }
            }
        }

        let mut effects: HashMap<_, _> = parameter_values
            .iter()
            .map(|(value, symbol)| (*value, (*symbol, FormParameterEffectSummary::default())))
            .collect();

        for block in routine.cfg.blocks.iter().filter(|block| block.reachable) {
            let mut written = block_entry_written[block.id.as_usize()].clone();
            for instruction_id in &block.instructions {
                let Some(summary) = routine
                    .dataflow_inputs
                    .instructions
                    .get(instruction_id.as_usize())
                else {
                    continue;
                };
                for value in &summary.reads {
                    if let Some((_, effect)) = effects.get_mut(value)
                        && !written.contains(*value)
                    {
                        effect.reads_before_write = true;
                    }
                }
                for value in &summary.writes {
                    if let Some((_, effect)) = effects.get_mut(value) {
                        effect.may_write = true;
                    }
                    written.insert(*value);
                }
            }
        }

        for (_, (symbol, effect)) in effects {
            out.insert(symbol, effect);
        }
    }

    out
}

fn perform_argument_effect_for_argument(
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    perform_call: &crate::PerformCallData,
    argument: &crate::PerformArgumentData,
    form_parameter_effects: &HashMap<SymbolHandle, FormParameterEffectSummary>,
) -> Option<FormParameterEffectSummary> {
    let parameter =
        resolve_perform_argument_parameter_symbol(project, unit, perform_call, argument)?;
    form_parameter_effects.get(&parameter).copied()
}

fn resolve_perform_argument_parameter_symbol(
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    perform_call: &crate::PerformCallData,
    argument: &crate::PerformArgumentData,
) -> Option<SymbolHandle> {
    let handle = project.resolve_perform_call_target(unit, perform_call)?;
    let target_unit = project.units.get(handle.unit.as_usize())?;
    let form = target_unit.form_routine(handle.symbol)?;
    let parameter = form
        .parameters
        .iter()
        .filter(|parameter| parameter.section == perform_section_to_form_section(argument.section))
        .nth(argument.ordinal_in_section)?;
    Some(SymbolHandle {
        unit: handle.unit,
        symbol: parameter.symbol,
    })
}

fn perform_section_to_form_section(section: PerformParameterSection) -> FormParameterSection {
    match section {
        PerformParameterSection::Tables => FormParameterSection::Tables,
        PerformParameterSection::Using => FormParameterSection::Using,
        PerformParameterSection::Changing => FormParameterSection::Changing,
    }
}

fn build_dead_store_instruction_summaries(
    unit: &UnitAnalysis,
    routine: &RoutineAnalysis,
    values: &[RoutineDataflowValue],
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
    instruction_summaries: &[InstructionDataflowSummary],
    tracked_values: &DenseBitSet,
    call_argument_effects: &CallArgumentEffectMap,
    value_state_check_refs: &std::collections::HashSet<crate::ReferenceId>,
) -> Vec<DeadStoreInstructionSummary> {
    let mut out = Vec::with_capacity(routine.ir.instructions.len());
    for instruction in &routine.ir.instructions {
        let mut reads: Vec<DataflowValueId> = instruction_summaries
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
            RoutineInstructionSite::ReadTable { index } => {
                if let Some(site) = unit.routine_sites.get(index as usize)
                    && let Some(target_range) = site.target_range.as_ref()
                    && let Some(value) =
                        direct_write_value_id_for_clear(reference_uses, target_range, values)
                    && tracked_values.contains(value)
                {
                    writes.push(DeadStoreWrite {
                        value,
                        range: target_range.clone(),
                    });
                }
            }
            RoutineInstructionSite::Find { index } => {
                if let Some(site) = unit.find_sites.get(index as usize) {
                    for target in &site.write_targets {
                        if let Some(value) = direct_write_value_id_for_range(
                            unit,
                            &target.range,
                            reference_uses,
                            value_ids_by_symbol,
                            values,
                        ) && tracked_values.contains(value)
                        {
                            writes.push(DeadStoreWrite {
                                value,
                                range: target.range.clone(),
                            });
                        }
                    }
                }
            }
            RoutineInstructionSite::Call { index } => {
                if let Some(call_site) = unit.call_sites.get(index as usize) {
                    for argument in &call_site.arguments {
                        if call_argument_effect_for_call_argument(
                            call_argument_effects,
                            call_site,
                            argument,
                        ) != CallArgumentEffect::AssignsOnly
                        {
                            continue;
                        }
                        reads.extend(
                            direct_non_field_symbol_values_in_range(
                                reference_uses,
                                &argument.range,
                                values,
                            )
                            .into_iter()
                            .filter(|value| tracked_values.contains(*value)),
                        );
                    }
                }
            }
            RoutineInstructionSite::ValueRead { reference } => {
                if value_state_check_refs.contains(&reference)
                    && let Some(value) =
                        resolved_value_id_for_reference(unit, reference, value_ids_by_symbol)
                    && tracked_values.contains(value)
                    && !reads.contains(&value)
                {
                    reads.push(value);
                }
            }
            RoutineInstructionSite::Perform { .. }
            | RoutineInstructionSite::SqlQuery { .. }
            | RoutineInstructionSite::Delete { .. }
            | RoutineInstructionSite::FieldSymbolBind { .. }
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
) -> CallArgumentEffectMap {
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
                | FunctionModuleParameterSection::Tables => CallArgumentEffect::AssignsOnly,
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

fn resolve_condition_probe_reads(
    unit: &UnitAnalysis,
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
) -> std::collections::HashSet<crate::ReferenceId> {
    let mut out = std::collections::HashSet::new();
    for check in &unit.value_state_checks {
        if check.kind != ValueStateCheckKind::ConditionProbe {
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
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    reference_uses: &[ReferenceUse],
    values: &[RoutineDataflowValue],
) -> std::collections::HashSet<crate::ReferenceId> {
    resolve_safe_loop_field_refs(
        project,
        unit,
        reference_uses,
        values,
        unit.loop_where_field_contexts.iter().map(|context| {
            (
                &context.source_access,
                context.target_access.as_ref(),
                &context.range,
            )
        }),
    )
}

fn resolve_safe_loop_at_field_refs(
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    reference_uses: &[ReferenceUse],
    values: &[RoutineDataflowValue],
) -> std::collections::HashSet<crate::ReferenceId> {
    resolve_safe_loop_field_refs(
        project,
        unit,
        reference_uses,
        values,
        unit.loop_at_field_contexts.iter().map(|context| {
            (
                &context.source_access,
                context.target_access.as_ref(),
                &context.range,
            )
        }),
    )
}

fn resolve_safe_loop_field_refs<'a>(
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    reference_uses: &[ReferenceUse],
    values: &[RoutineDataflowValue],
    contexts: impl IntoIterator<
        Item = (
            &'a crate::FieldAccess,
            Option<&'a crate::FieldAccess>,
            &'a TextRange,
        ),
    >,
) -> std::collections::HashSet<crate::ReferenceId> {
    let mut out = std::collections::HashSet::new();
    for (source_access, target_access, range) in contexts {
        let mut field_names = std::collections::HashSet::new();
        for access in std::iter::once(Some(source_access)).chain(std::iter::once(target_access)) {
            let Some(access) = access else {
                continue;
            };
            let Some((structure_unit, structure_id)) = resolve_value_access_structure_project(
                project,
                unit,
                reference_uses,
                values,
                access,
            ) else {
                continue;
            };
            let Some(structure) = structure_unit.structures.get(structure_id.as_usize()) else {
                continue;
            };
            field_names.extend(structure.fields.iter().map(|field| field.name.as_ref()));
        }
        if field_names.is_empty() {
            continue;
        }
        for use_site in reference_uses_in_range(reference_uses, range) {
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
        let Some(refinement_scope) = non_initial_refinement_scope_for_check(unit, check) else {
            continue;
        };
        let Some(scope_bits) = out.get_mut(refinement_scope.as_usize()) else {
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
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
    structure_assignment_trackers: &[Option<StructureAssignmentTracker>],
    values: &[RoutineDataflowValue],
) -> Vec<Vec<u64>> {
    let mut out = vec![vec![0u64; values.len()]; unit.scopes.len()];
    for check in &unit.value_state_checks {
        let Some(refinement_scope) = non_initial_refinement_scope_for_check(unit, check) else {
            continue;
        };
        let Some(field_name) = check.field_name.as_ref() else {
            continue;
        };
        let Some(scope_masks) = out.get_mut(refinement_scope.as_usize()) else {
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
                || value_symbol_is_internal_table(project, unit, use_site.value, values)
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

fn non_initial_refinement_scope_for_check(
    unit: &UnitAnalysis,
    check: &crate::ValueStateCheckData,
) -> Option<ScopeId> {
    match check.kind {
        ValueStateCheckKind::IsNotInitial => Some(check.scope),
        ValueStateCheckKind::IsInitial => explicit_else_scope_for_then_scope(unit, check.scope),
        ValueStateCheckKind::EqualsZero
        | ValueStateCheckKind::NotEqualsZero
        | ValueStateCheckKind::ConditionProbe => None,
    }
}

fn explicit_else_scope_for_then_scope(unit: &UnitAnalysis, then_scope: ScopeId) -> Option<ScopeId> {
    unit.routine_control_regions.iter().find_map(|region| {
        let RoutineControlRegionData::If(if_region) = region else {
            return None;
        };
        (if_region.then_scope == then_scope)
            .then_some(if_region.else_scope)
            .flatten()
    })
}

fn if_region_for_instruction<'a>(
    unit: &'a UnitAnalysis,
    instruction: &RoutineInstruction,
) -> Option<&'a IfRegionData> {
    unit.routine_control_regions.iter().find_map(|region| {
        let RoutineControlRegionData::If(if_region) = region else {
            return None;
        };
        (if_region.scope == instruction.scope && if_region.range == instruction.range)
            .then_some(if_region)
    })
}

fn single_negative_sy_subrc_guard_check_for_scope(
    unit: &UnitAnalysis,
    scope: ScopeId,
) -> Option<&crate::ValueStateCheckData> {
    let mut check = None;
    for candidate in unit
        .value_state_checks
        .iter()
        .filter(|candidate| candidate.scope == scope)
        .filter(|candidate| candidate.kind != ValueStateCheckKind::ConditionProbe)
    {
        if check.is_some() {
            return None;
        }
        check = Some(candidate);
    }
    check.filter(|candidate| sy_subrc_negative_guard_check(unit, candidate))
}

fn resolve_sy_subrc_success_bound_scope_refinements(
    unit: &UnitAnalysis,
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
    values: &[RoutineDataflowValue],
    value_count: usize,
) -> Vec<DenseBitSet> {
    let mut out = vec![DenseBitSet::new(value_count); unit.scopes.len()];
    for check in &unit.value_state_checks {
        if !sy_subrc_success_check(unit, check) {
            continue;
        }
        let Some(scope_bits) = out.get_mut(check.scope.as_usize()) else {
            continue;
        };
        let Some(update) = latest_subrc_update_before_check(unit, check) else {
            continue;
        };
        if !matches!(
            update.statement,
            SystemFieldStatementKind::Assign | SystemFieldStatementKind::ReadTable
        ) {
            continue;
        }
        for edge in &unit.value_flow_edges {
            if edge.kind != ValueFlowKind::ConditionalFieldSymbolAssignment
                || edge.scope != update.scope
                || !system_field_update_contains_field_symbol_bind(update, edge)
            {
                continue;
            }
            if let Some(target_value) = resolve_field_symbol_target_value_id(
                unit,
                edge,
                reference_uses,
                value_ids_by_symbol,
                values,
            ) {
                scope_bits.insert(target_value);
            }
        }
    }
    out
}

fn resolve_is_assigned_bound_scope_refinements(
    unit: &UnitAnalysis,
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
    value_count: usize,
) -> Vec<DenseBitSet> {
    let mut out = vec![DenseBitSet::new(value_count); unit.scopes.len()];
    for check in &unit.field_symbol_state_checks {
        if check.kind != FieldSymbolStateCheckKind::IsAssigned
            || !is_positive_branch_scope(unit, check.scope)
        {
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

fn resolve_sy_subrc_success_assigned_scope_refinements(
    unit: &UnitAnalysis,
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
    structure_assignment_trackers: &[Option<StructureAssignmentTracker>],
    values: &[RoutineDataflowValue],
    value_count: usize,
) -> Vec<DenseBitSet> {
    let mut out = vec![DenseBitSet::new(value_count); unit.scopes.len()];
    for check in &unit.value_state_checks {
        if !sy_subrc_success_check(unit, check) {
            continue;
        }
        let Some(scope_bits) = out.get_mut(check.scope.as_usize()) else {
            continue;
        };
        let Some(update) = latest_subrc_update_before_check(unit, check) else {
            continue;
        };
        for target in conditional_assignment_targets_for_subrc_success_update(
            unit,
            update,
            reference_uses,
            value_ids_by_symbol,
            structure_assignment_trackers,
            values,
        ) {
            if target.field_mask.is_none() {
                scope_bits.insert(target.value);
            }
        }
    }
    out
}

fn resolve_sy_subrc_success_structure_field_scope_refinements(
    unit: &UnitAnalysis,
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
    structure_assignment_trackers: &[Option<StructureAssignmentTracker>],
    values: &[RoutineDataflowValue],
) -> Vec<Vec<u64>> {
    let mut out = vec![vec![0u64; values.len()]; unit.scopes.len()];
    for check in &unit.value_state_checks {
        if !sy_subrc_success_check(unit, check) {
            continue;
        }
        let Some(scope_masks) = out.get_mut(check.scope.as_usize()) else {
            continue;
        };
        let Some(update) = latest_subrc_update_before_check(unit, check) else {
            continue;
        };
        for target in conditional_assignment_targets_for_subrc_success_update(
            unit,
            update,
            reference_uses,
            value_ids_by_symbol,
            structure_assignment_trackers,
            values,
        ) {
            if let Some(mask) = target.field_mask {
                scope_masks[target.value.as_usize()] |= mask;
            }
        }
    }
    out
}

fn sy_subrc_success_check(unit: &UnitAnalysis, check: &crate::ValueStateCheckData) -> bool {
    if !matches!(
        check.kind,
        ValueStateCheckKind::EqualsZero | ValueStateCheckKind::IsInitial
    ) {
        return false;
    }
    is_positive_branch_scope(unit, check.scope) && is_sy_subrc_check(check)
}

fn sy_subrc_negative_guard_check(unit: &UnitAnalysis, check: &crate::ValueStateCheckData) -> bool {
    if !matches!(
        check.kind,
        ValueStateCheckKind::NotEqualsZero | ValueStateCheckKind::IsNotInitial
    ) {
        return false;
    }
    is_positive_branch_scope(unit, check.scope) && is_sy_subrc_check(check)
}

fn is_sy_subrc_check(check: &crate::ValueStateCheckData) -> bool {
    let Some(field_name) = check.field_name.as_ref() else {
        return false;
    };
    field_name.eq_ignore_ascii_case("subrc")
        && (check.symbol_name.eq_ignore_ascii_case("sy")
            || check.symbol_name.eq_ignore_ascii_case("syst"))
}

fn is_positive_branch_scope(unit: &UnitAnalysis, scope: ScopeId) -> bool {
    let Some(scope_data) = unit.scopes.get(scope.as_usize()) else {
        return false;
    };
    matches!(
        scope_data.kind,
        ScopeKind::IfBranch | ScopeKind::ElseifBranch
    )
}

fn union_dense_scope_refinements(out: &mut [DenseBitSet], incoming: &[DenseBitSet]) {
    for (out_bits, incoming_bits) in out.iter_mut().zip(incoming) {
        out_bits.union_from(incoming_bits);
    }
}

fn union_structure_field_refinements(out: &mut [Vec<u64>], incoming: &[Vec<u64>]) {
    for (out_masks, incoming_masks) in out.iter_mut().zip(incoming) {
        union_structure_field_masks(out_masks, incoming_masks);
    }
}

fn latest_subrc_update_before_check<'a>(
    unit: &'a UnitAnalysis,
    check: &crate::ValueStateCheckData,
) -> Option<&'a crate::SystemFieldUpdateData> {
    unit.system_field_updates
        .iter()
        .filter(|update| {
            update.field_name.eq_ignore_ascii_case("subrc")
                && update.range.end <= check.range.start
                && scope_descends_from(unit, check.scope, update.scope)
        })
        .max_by_key(|update| (update.range.end, update.range.start))
}

fn conditional_bound_targets_for_subrc_success_update(
    unit: &UnitAnalysis,
    update: &crate::SystemFieldUpdateData,
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
    values: &[RoutineDataflowValue],
) -> Vec<DataflowValueId> {
    if !matches!(
        update.statement,
        SystemFieldStatementKind::Assign | SystemFieldStatementKind::ReadTable
    ) {
        return Vec::new();
    }
    let mut out = Vec::new();
    for edge in &unit.value_flow_edges {
        if edge.kind != ValueFlowKind::ConditionalFieldSymbolAssignment
            || edge.scope != update.scope
            || !system_field_update_contains_field_symbol_bind(update, edge)
        {
            continue;
        }
        if let Some(target_value) = resolve_field_symbol_target_value_id(
            unit,
            edge,
            reference_uses,
            value_ids_by_symbol,
            values,
        ) {
            out.push(target_value);
        }
    }
    sorted_unique_value_ids(out)
}

fn conditional_assignment_targets_for_subrc_success_update(
    unit: &UnitAnalysis,
    update: &crate::SystemFieldUpdateData,
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
    structure_assignment_trackers: &[Option<StructureAssignmentTracker>],
    values: &[RoutineDataflowValue],
) -> Vec<ConditionalAssignedTarget> {
    match update.statement {
        SystemFieldStatementKind::ReadTable => unit
            .routine_sites
            .iter()
            .find(|site| {
                site.kind == RoutineSiteKind::ReadTable
                    && site.scope == update.scope
                    && site.range == update.range
            })
            .and_then(|site| {
                site.target_range.as_ref().and_then(|target_range| {
                    conditional_assignment_target_for_range(
                        unit,
                        target_range,
                        reference_uses,
                        value_ids_by_symbol,
                        structure_assignment_trackers,
                        values,
                    )
                })
            })
            .into_iter()
            .collect(),
        SystemFieldStatementKind::Select => unit
            .sql_queries
            .iter()
            .filter(|query| {
                scope_descends_from(unit, query.scope, update.scope)
                    && update.range.start <= query.range.start
                    && query.range.end <= update.range.end
            })
            .flat_map(|query| {
                sql_query_conditional_assignment_targets(
                    unit,
                    query.id,
                    reference_uses,
                    value_ids_by_symbol,
                    structure_assignment_trackers,
                    values,
                )
            })
            .collect(),
        _ => Vec::new(),
    }
}

fn resolve_sy_subrc_success_guard_block_refinements(
    unit: &UnitAnalysis,
    routine: &RoutineAnalysis,
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
    structure_assignment_trackers: &[Option<StructureAssignmentTracker>],
    values: &[RoutineDataflowValue],
) -> (Vec<DenseBitSet>, Vec<Vec<u64>>, Vec<DenseBitSet>) {
    let mut assigned = vec![DenseBitSet::new(values.len()); routine.cfg.blocks.len()];
    let mut structure_fields = vec![vec![0u64; values.len()]; routine.cfg.blocks.len()];
    let mut bound = vec![DenseBitSet::new(values.len()); routine.cfg.blocks.len()];

    for block in &routine.cfg.blocks {
        let Some(last_instruction_id) = block.instructions.last() else {
            continue;
        };
        let instruction = &routine.ir.instructions[last_instruction_id.as_usize()];
        let RoutineInstructionSite::Branch {
            kind: RoutineBranchKind::If,
        } = instruction.site
        else {
            continue;
        };
        let Some(region) = if_region_for_instruction(unit, instruction) else {
            continue;
        };
        if region.else_scope.is_some() || !region.elseif_scopes.is_empty() {
            continue;
        }
        let Some(check) = single_negative_sy_subrc_guard_check_for_scope(unit, region.then_scope)
        else {
            continue;
        };
        let Some(continuation_block) = unique_fallthrough_successor(routine, block.id) else {
            continue;
        };
        let Some(continuation) = routine.cfg.blocks.get(continuation_block.as_usize()) else {
            continue;
        };
        if continuation.predecessors.len() != 1 || continuation.predecessors[0] != block.id {
            continue;
        }
        let Some(update) = latest_subrc_update_before_check(unit, check) else {
            continue;
        };

        for target in conditional_assignment_targets_for_subrc_success_update(
            unit,
            update,
            reference_uses,
            value_ids_by_symbol,
            structure_assignment_trackers,
            values,
        ) {
            if let Some(mask) = target.field_mask {
                structure_fields[continuation_block.as_usize()][target.value.as_usize()] |= mask;
            } else {
                assigned[continuation_block.as_usize()].insert(target.value);
            }
        }
        for target in conditional_bound_targets_for_subrc_success_update(
            unit,
            update,
            reference_uses,
            value_ids_by_symbol,
            values,
        ) {
            bound[continuation_block.as_usize()].insert(target);
        }
    }

    (assigned, structure_fields, bound)
}

fn unique_fallthrough_successor(
    routine: &RoutineAnalysis,
    block: RoutineBlockId,
) -> Option<RoutineBlockId> {
    let mut successor = None;
    for edge in routine
        .cfg
        .edges
        .iter()
        .filter(|edge| edge.from == block && edge.kind == RoutineEdgeKind::Fallthrough)
    {
        if successor.is_some() {
            return None;
        }
        successor = Some(edge.to);
    }
    successor
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
            inherited_dense_scope_refinements(
                unit,
                scope_refinements,
                routine.ir.instructions[first_instruction.as_usize()].scope,
                value_count,
            )
        })
        .collect()
}

fn block_bound_entry_refinements(
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
            inherited_dense_scope_refinements(
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

fn inherited_dense_scope_refinements(
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

fn scope_descends_from(unit: &UnitAnalysis, scope: ScopeId, ancestor: ScopeId) -> bool {
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        if scope_id == ancestor {
            return true;
        }
        current = unit
            .scopes
            .get(scope_id.as_usize())
            .and_then(|scope| scope.parent);
    }
    false
}

fn sql_target_effective_range(target: &crate::SqlTargetData) -> &TextRange {
    target.target_range.as_ref().unwrap_or(&target.range)
}

fn build_read_table_binary_search_order_diagnostics(
    project: &ProjectAnalysis,
    scope_to_routine: &[Vec<Option<RoutineId>>],
) -> Vec<(RoutineId, Diagnostic)> {
    let mut out = Vec::new();
    for unit in &project.units {
        let Some(scope_map) = scope_to_routine.get(unit.unit_id.as_usize()) else {
            continue;
        };
        for read in &unit.read_table_binary_searches {
            let Some(routine_id) = scope_map.get(read.scope.as_usize()).copied().flatten() else {
                continue;
            };
            if read_table_binary_search_has_prior_order(unit, scope_map, routine_id, read) {
                continue;
            }
            out.push((
                routine_id,
                Diagnostic {
                    kind: DiagnosticKind::UnsortedReadTableBinarySearch,
                    range: read.range.clone(),
                    message: format!(
                        "READ TABLE with BINARY SEARCH on '{}' has no prior SORT or SELECT ORDER BY on {}",
                        read.table_name,
                        read_table_binary_search_key_label(&read.key_fields)
                    ),
                },
            ));
        }
    }
    out
}

fn read_table_binary_search_has_prior_order(
    unit: &UnitAnalysis,
    scope_map: &[Option<RoutineId>],
    routine_id: RoutineId,
    read: &ReadTableBinarySearchData,
) -> bool {
    if read.key_fields.is_empty() {
        return false;
    }
    unit.internal_table_orders
        .iter()
        .any(|order| internal_table_order_is_prior_match(unit, scope_map, routine_id, read, order))
        || unit
            .sql_queries
            .iter()
            .any(|query| sql_query_order_is_prior_match(unit, scope_map, routine_id, read, query))
}

fn internal_table_order_is_prior_match(
    unit: &UnitAnalysis,
    scope_map: &[Option<RoutineId>],
    routine_id: RoutineId,
    read: &ReadTableBinarySearchData,
    order: &InternalTableOrderData,
) -> bool {
    order.range.end <= read.range.start
        && order
            .table_name
            .as_ref()
            .eq_ignore_ascii_case(&read.table_name)
        && scope_map
            .get(order.scope.as_usize())
            .copied()
            .flatten()
            .is_some_and(|order_routine| order_routine == routine_id)
        && scope_descends_from(unit, read.scope, order.scope)
        && table_order_fields_match_read_key(&order.key_fields, &read.key_fields)
}

fn sql_query_order_is_prior_match(
    unit: &UnitAnalysis,
    scope_map: &[Option<RoutineId>],
    routine_id: RoutineId,
    read: &ReadTableBinarySearchData,
    query: &crate::SqlQueryData,
) -> bool {
    !query.order_by_fields.is_empty()
        && query.range.end <= read.range.start
        && scope_map
            .get(query.scope.as_usize())
            .copied()
            .flatten()
            .is_some_and(|order_routine| order_routine == routine_id)
        && scope_descends_from(unit, read.scope, query.scope)
        && unit.sql_targets.iter().any(|target| {
            target.query_id == query.id
                && target.kind == crate::SqlTargetKind::Into
                && target.is_table
                && target.target_name.as_ref().is_some_and(|table_name| {
                    table_name.as_ref().eq_ignore_ascii_case(&read.table_name)
                })
        })
        && table_order_fields_match_read_key(&query.order_by_fields, &read.key_fields)
}

fn table_order_fields_match_read_key(order_fields: &[Arc<str>], key_fields: &[Arc<str>]) -> bool {
    let effective_order_fields = order_fields
        .iter()
        .skip_while(|field| is_client_column_name(field.as_ref()))
        .collect::<Vec<_>>();
    effective_order_fields.len() >= key_fields.len()
        && effective_order_fields
            .into_iter()
            .zip(key_fields)
            .all(|(ordered, key)| ordered.as_ref().eq_ignore_ascii_case(key.as_ref()))
}

fn is_client_column_name(field_name: &str) -> bool {
    field_name.eq_ignore_ascii_case("mandt") || field_name.eq_ignore_ascii_case("client")
}

fn read_table_binary_search_key_label(key_fields: &[Arc<str>]) -> String {
    if key_fields.is_empty() {
        return "the READ TABLE key fields".to_string();
    }
    format!(
        "key field(s) {}",
        key_fields
            .iter()
            .map(|field| field.as_ref())
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn conditional_assignment_target_for_range(
    unit: &UnitAnalysis,
    range: &TextRange,
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
    structure_assignment_trackers: &[Option<StructureAssignmentTracker>],
    values: &[RoutineDataflowValue],
) -> Option<ConditionalAssignedTarget> {
    if let Some(selector_write) = selector_structure_write_for_range(
        unit,
        range,
        reference_uses,
        structure_assignment_trackers,
    ) {
        return Some(ConditionalAssignedTarget {
            value: selector_write.base_value,
            field_mask: selector_write.field_mask,
        });
    }
    direct_write_value_id_for_range(unit, range, reference_uses, value_ids_by_symbol, values).map(
        |value| ConditionalAssignedTarget {
            value,
            field_mask: None,
        },
    )
}

fn sql_query_conditional_assignment_targets(
    unit: &UnitAnalysis,
    query_id: usize,
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
    structure_assignment_trackers: &[Option<StructureAssignmentTracker>],
    values: &[RoutineDataflowValue],
) -> Vec<ConditionalAssignedTarget> {
    unit.sql_targets
        .iter()
        .filter(|target| target.query_id == query_id)
        .filter_map(|target| {
            conditional_assignment_target_for_range(
                unit,
                sql_target_effective_range(target),
                reference_uses,
                value_ids_by_symbol,
                structure_assignment_trackers,
                values,
            )
        })
        .collect()
}

fn system_field_update_contains_field_symbol_bind(
    update: &crate::SystemFieldUpdateData,
    edge: &crate::ValueFlowEdgeData,
) -> bool {
    let crate::ValueFlowTargetData::FieldSymbol { range, .. } = &edge.target else {
        return false;
    };
    update.range.start <= edge.source_range.start
        && edge.source_range.end <= update.range.end
        && update.range.start <= range.start
        && range.end <= update.range.end
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

fn reference_uses_in_range<'a>(
    reference_uses: &'a [ReferenceUse],
    range: &'a TextRange,
) -> impl Iterator<Item = &'a ReferenceUse> + 'a {
    let start_idx = reference_uses.partition_point(|use_site| use_site.range.start < range.start);
    reference_uses[start_idx..]
        .iter()
        .take_while(move |use_site| use_site.range.start < range.end)
        .filter(move |use_site| use_site.range.end <= range.end)
}

fn mark_reference_ids_in_range(
    marked: &mut [bool],
    reference_uses: &[ReferenceUse],
    range: &TextRange,
) {
    for use_site in reference_uses_in_range(reference_uses, range) {
        if let Some(slot) = marked.get_mut(use_site.reference.as_usize()) {
            *slot = true;
        }
    }
}

fn reference_is_marked(marked: &[bool], reference: crate::ReferenceId) -> bool {
    marked.get(reference.as_usize()).copied().unwrap_or(false)
}

fn exact_reference_use_in_range(
    reference_uses: &[ReferenceUse],
    range: &TextRange,
) -> Option<ReferenceUse> {
    let mut matches =
        reference_uses_in_range(reference_uses, range).filter(|use_site| use_site.range == *range);
    let first = matches.next()?;
    matches.next().is_none().then(|| first.clone())
}

fn read_occurrences_in_range(
    reference_uses: &[ReferenceUse],
    range: &TextRange,
    safe_field_symbol_checks: &std::collections::HashSet<crate::ReferenceId>,
    suppress_definite_assignment: bool,
) -> Vec<ReadOccurrence> {
    reference_uses_in_range(reference_uses, range)
        .filter(|use_site| !safe_field_symbol_checks.contains(&use_site.reference))
        .map(|use_site| ReadOccurrence {
            reference: use_site.reference,
            range: use_site.range.clone(),
            value: use_site.value,
            suppress_definite_assignment,
        })
        .collect()
}

fn is_table_line_mutation_assignment(unit: &UnitAnalysis, range: &TextRange) -> bool {
    unit.system_field_updates.iter().any(|update| {
        matches!(
            update.statement,
            crate::SystemFieldStatementKind::Append | crate::SystemFieldStatementKind::InsertTable
        ) && &update.range == range
    })
}

fn resolve_structure_field_reads(
    project: &ProjectAnalysis,
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
        if value_symbol_is_internal_table(project, unit, base_use.value, values) {
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
    project: &ProjectAnalysis,
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
    let mut seen = HashSet::new();
    symbol_is_internal_table(project, unit, symbol, &mut seen)
}

fn symbol_is_internal_table(
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    symbol: &SymbolData,
    seen: &mut HashSet<(u32, u32)>,
) -> bool {
    if symbol_type_clause_suggests_internal_table(symbol)
        || unit.sql_targets.iter().any(|target| {
            target.is_inline
                && target.is_table
                && target.scope == symbol.scope
                && target.target_name.as_deref() == Some(symbol.name.as_ref())
        })
    {
        return true;
    }

    let Some(type_ref) = symbol.declared_type.as_ref() else {
        return false;
    };
    if !type_ref.field_path.is_empty() {
        return false;
    }
    let Some((resolved_unit, symbol_id)) =
        resolve_type_ref_symbol_project(project, unit, unit, symbol.scope, type_ref)
    else {
        return false;
    };
    if !seen.insert((resolved_unit.unit_id.0, symbol_id.0)) {
        return false;
    }
    let Some(resolved_symbol) = resolved_unit.symbols.get(symbol_id.as_usize()) else {
        return false;
    };
    symbol_is_internal_table(project, resolved_unit, resolved_symbol, seen)
}

fn resolve_value_access_structure_project<'a>(
    project: &'a ProjectAnalysis,
    unit: &'a UnitAnalysis,
    reference_uses: &[ReferenceUse],
    values: &[RoutineDataflowValue],
    access: &crate::FieldAccess,
) -> Option<(&'a UnitAnalysis, StructureId)> {
    if access.base_namespace != Namespace::Value {
        return None;
    }
    let base_handle = resolved_symbol_handle_for_access_base(unit, reference_uses, values, access)?;
    let (mut current_unit, mut current_structure) = resolve_symbol_structure_project(
        project,
        project.units.get(base_handle.unit.as_usize())?,
        unit,
        access.scope,
        base_handle.symbol,
    )?;

    for segment in &access.field_path {
        if segment.is_deref() {
            return None;
        }
        let field = current_unit
            .structures
            .get(current_structure.as_usize())?
            .fields
            .iter()
            .find(|field| field.name == segment.name)?;
        let (next_unit, next_structure) =
            resolve_structure_from_field_project(project, current_unit, unit, access.scope, field)?;
        current_unit = next_unit;
        current_structure = next_structure;
    }

    Some((current_unit, current_structure))
}

fn resolved_symbol_handle_for_access_base(
    unit: &UnitAnalysis,
    reference_uses: &[ReferenceUse],
    values: &[RoutineDataflowValue],
    access: &crate::FieldAccess,
) -> Option<SymbolHandle> {
    if access.base_namespace != Namespace::Value {
        return None;
    }
    let reference = exact_reference_use_in_range(reference_uses, &access.base_range)
        .and_then(|use_site| unit.references.get(use_site.reference.as_usize()))
        .or_else(|| {
            unit.references
                .iter()
                .find(|reference| reference.range == access.base_range)
        });
    if let Some(reference) = reference
        && let Some(Resolution::Symbol(handle)) = reference.resolution
    {
        return Some(handle);
    }
    resolve_declared_value_id_for_access(unit, access, values)
        .and_then(|value_id| values.get(value_id.as_usize()))
        .map(|value| value.symbol)
}

fn resolve_symbol_structure_project<'a>(
    project: &'a ProjectAnalysis,
    current_unit: &'a UnitAnalysis,
    origin_unit: &'a UnitAnalysis,
    scope: ScopeId,
    symbol_id: crate::ids::SymbolId,
) -> Option<(&'a UnitAnalysis, StructureId)> {
    let mut current_unit = current_unit;
    let mut current_symbol_id = symbol_id;
    let mut seen = std::collections::HashSet::new();
    for _ in 0..8 {
        let symbol = current_unit.symbols.get(current_symbol_id.as_usize())?;
        if let Some(structure_id) = symbol.structure {
            return Some((current_unit, structure_id));
        }
        let type_ref = symbol.declared_type.as_ref()?;
        let (next_unit, next_symbol_id) =
            resolve_type_ref_symbol_project(project, current_unit, origin_unit, scope, type_ref)?;
        if !seen.insert((next_unit.unit_id.0, next_symbol_id.0)) {
            return None;
        }
        current_unit = next_unit;
        current_symbol_id = next_symbol_id;
    }
    None
}

fn resolve_structure_from_field_project<'a>(
    project: &'a ProjectAnalysis,
    current_unit: &'a UnitAnalysis,
    origin_unit: &'a UnitAnalysis,
    scope: ScopeId,
    field: &crate::StructureFieldData,
) -> Option<(&'a UnitAnalysis, StructureId)> {
    if let Some(structure_id) = field.structure {
        return Some((current_unit, structure_id));
    }
    let type_ref = field.type_ref.as_ref()?;
    let (next_unit, next_symbol_id) =
        resolve_type_ref_symbol_project(project, current_unit, origin_unit, scope, type_ref)?;
    let (next_unit, next_structure) =
        resolve_symbol_structure_project(project, next_unit, origin_unit, scope, next_symbol_id)?;
    Some((next_unit, next_structure))
}

fn resolve_type_ref_symbol_project<'a>(
    project: &'a ProjectAnalysis,
    current_unit: &'a UnitAnalysis,
    origin_unit: &'a UnitAnalysis,
    scope: ScopeId,
    type_ref: &crate::FieldTypeRefData,
) -> Option<(&'a UnitAnalysis, crate::ids::SymbolId)> {
    if !type_ref.field_path.is_empty() {
        return None;
    }
    let scope = if current_unit.scopes.get(scope.as_usize()).is_some() {
        scope
    } else {
        current_unit.root_scope
    };
    let namespaces = if type_ref.namespace == Namespace::Value {
        [Namespace::Value, Namespace::Type]
    } else {
        [type_ref.namespace, type_ref.namespace]
    };
    for namespace in namespaces {
        if let Some(symbol_id) =
            resolve_symbol_id_in_scope_chain(current_unit, scope, namespace, &type_ref.base_name)
        {
            return Some((current_unit, symbol_id));
        }
        if current_unit.unit_id != origin_unit.unit_id
            && let Some(symbol_id) =
                resolve_symbol_id_in_scope_chain(origin_unit, scope, namespace, &type_ref.base_name)
        {
            return Some((origin_unit, symbol_id));
        }
        if let Some((resolved_unit, symbol_id)) =
            resolve_project_root_symbol(project, namespace, &type_ref.base_name)
        {
            return Some((resolved_unit, symbol_id));
        }
    }
    None
}

fn resolve_symbol_id_in_scope_chain(
    unit: &UnitAnalysis,
    scope: ScopeId,
    namespace: Namespace,
    name: &str,
) -> Option<crate::ids::SymbolId> {
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        if let Some(symbol_id) = unit.symbols.iter().find_map(|symbol| {
            (symbol.scope == scope_id
                && symbol.kind.occupies(namespace)
                && symbol.name.as_ref().eq_ignore_ascii_case(name))
            .then_some(symbol.id)
        }) {
            return Some(symbol_id);
        }
        current = unit
            .scopes
            .get(scope_id.as_usize())
            .and_then(|scope_data| scope_data.parent);
    }
    None
}

fn resolve_project_root_symbol<'a>(
    project: &'a ProjectAnalysis,
    namespace: Namespace,
    name: &str,
) -> Option<(&'a UnitAnalysis, crate::ids::SymbolId)> {
    if let Some(unit_id) =
        project
            .provided_name_to_unit
            .iter()
            .find_map(|(provided_name, unit_id)| {
                provided_name
                    .as_ref()
                    .eq_ignore_ascii_case(name)
                    .then_some(*unit_id)
            })
    {
        let unit = project.units.get(unit_id.as_usize())?;
        if let Some(symbol_id) = unit.symbols.iter().find_map(|symbol| {
            (symbol.scope == unit.root_scope
                && symbol.kind.occupies(namespace)
                && symbol.name.as_ref().eq_ignore_ascii_case(name))
            .then_some(symbol.id)
        }) {
            return Some((unit, symbol_id));
        }
    }
    project.units.iter().find_map(|unit| {
        unit.symbols.iter().find_map(|symbol| {
            (symbol.scope == unit.root_scope
                && symbol.kind.occupies(namespace)
                && symbol.name.as_ref().eq_ignore_ascii_case(name))
            .then_some((unit, symbol.id))
        })
    })
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
    if let Some(access) = assignment.lhs_target_access.as_ref()
        && access.base_namespace == Namespace::Value
        && access.field_path.is_empty()
    {
        if let Some(direct) = exact_reference_use_in_range(reference_uses, &access.base_range)
            && values[direct.value.as_usize()].kind != DataflowValueKind::FieldSymbol
        {
            return Some(direct.value);
        }
        let mut direct_values = reference_uses_in_range(reference_uses, &assignment.lhs_range)
            .filter_map(|use_site| {
                (values[use_site.value.as_usize()].kind != DataflowValueKind::FieldSymbol)
                    .then_some(use_site.value)
            });
        let first = direct_values.next()?;
        return direct_values.all(|value| value == first).then_some(first);
    }
    if let Some(direct) = exact_reference_use_in_range(reference_uses, &assignment.lhs_range)
        && values[direct.value.as_usize()].kind != DataflowValueKind::FieldSymbol
    {
        return Some(direct.value);
    }
    if assignment.lhs_target_access.is_none() {
        let direct_values = reference_uses_in_range(reference_uses, &assignment.lhs_range)
            .filter(|use_site| {
                values[use_site.value.as_usize()].kind != DataflowValueKind::FieldSymbol
            })
            .collect::<Vec<_>>();
        if let [direct] = direct_values.as_slice()
            && direct.range.start == assignment.lhs_range.start
            && direct.range.end + 2 == assignment.lhs_range.end
        {
            return Some(direct.value);
        }
    }
    None
}

fn direct_write_value_id_for_clear(
    reference_uses: &[ReferenceUse],
    range: &TextRange,
    values: &[RoutineDataflowValue],
) -> Option<DataflowValueId> {
    let direct = exact_reference_use_in_range(reference_uses, range)?;
    (values[direct.value.as_usize()].kind != DataflowValueKind::FieldSymbol).then_some(direct.value)
}

fn direct_write_value_id_for_range(
    unit: &UnitAnalysis,
    range: &TextRange,
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
    values: &[RoutineDataflowValue],
) -> Option<DataflowValueId> {
    let declared_values = unit
        .symbols
        .iter()
        .filter(|symbol| {
            trackable_symbol_kind(symbol.kind)
                && symbol.decl_range.start >= range.start
                && symbol.decl_range.end <= range.end
        })
        .filter_map(|symbol| {
            value_ids_by_symbol
                .get(&SymbolHandle {
                    unit: unit.unit_id,
                    symbol: symbol.id,
                })
                .copied()
        })
        .filter(|value| values[value.as_usize()].kind != DataflowValueKind::FieldSymbol)
        .collect::<Vec<_>>();
    if let [value] = declared_values.as_slice() {
        return Some(*value);
    }

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

fn selector_structure_write_for_range(
    unit: &UnitAnalysis,
    range: &TextRange,
    reference_uses: &[ReferenceUse],
    structure_assignment_trackers: &[Option<StructureAssignmentTracker>],
) -> Option<SelectorStructureWrite> {
    let access = unit.field_accesses.iter().find(|access| {
        if access.base_namespace != Namespace::Value
            || access.field_path.is_empty()
            || access.field_path.iter().any(|segment| segment.is_deref())
        {
            return false;
        }
        let Some(last_segment) = access.field_path.last() else {
            return false;
        };
        access.base_range.start == range.start && last_segment.range.end == range.end
    })?;
    let base_use = reference_uses_in_range(reference_uses, range)
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
            .map(|use_site| use_site.value)
            .filter(|value| values[value.as_usize()].kind != DataflowValueKind::FieldSymbol),
    )
}

fn direct_non_field_symbol_write_values_in_range(
    unit: &UnitAnalysis,
    reference_uses: &[ReferenceUse],
    value_ids_by_symbol: &HashMap<SymbolHandle, DataflowValueId>,
    range: &TextRange,
    values: &[RoutineDataflowValue],
) -> Vec<DataflowValueId> {
    let mut out = direct_non_field_symbol_values_in_range(reference_uses, range, values);
    out.extend(
        unit.symbols
            .iter()
            .filter(|symbol| {
                trackable_symbol_kind(symbol.kind)
                    && symbol.decl_range.start >= range.start
                    && symbol.decl_range.end <= range.end
            })
            .filter_map(|symbol| {
                value_ids_by_symbol
                    .get(&SymbolHandle {
                        unit: unit.unit_id,
                        symbol: symbol.id,
                    })
                    .copied()
            })
            .filter(|value| values[value.as_usize()].kind != DataflowValueKind::FieldSymbol),
    );
    sorted_unique_value_ids(out)
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

fn build_scope_to_routine_map(
    unit: &UnitAnalysis,
    exact_routine_scopes: &[Option<RoutineId>],
) -> Vec<Option<RoutineId>> {
    let mut scope_to_routine = vec![None; unit.scopes.len()];
    for scope in &unit.scopes {
        let idx = scope.id.as_usize();
        scope_to_routine[idx] = exact_routine_scopes
            .get(idx)
            .copied()
            .flatten()
            .or_else(|| {
                scope.parent.and_then(|parent| {
                    let parent_idx = parent.as_usize();
                    (parent_idx < idx)
                        .then(|| scope_to_routine.get(parent_idx).copied().flatten())
                        .flatten()
                })
            });
    }
    scope_to_routine
}

fn build_routine_control_region_index<'a>(
    unit: &'a UnitAnalysis,
    scope_to_routine: &[Option<RoutineId>],
) -> HashMap<RoutineId, Vec<&'a RoutineControlRegionData>> {
    let mut out: HashMap<RoutineId, Vec<&RoutineControlRegionData>> = HashMap::new();
    for region in &unit.routine_control_regions {
        let Some(routine_id) = scope_to_routine
            .get(region.scope().as_usize())
            .copied()
            .flatten()
        else {
            continue;
        };
        out.entry(routine_id).or_default().push(region);
    }
    out
}

fn build_tracked_symbols_by_routine<'a>(
    project: &'a ProjectAnalysis,
    scope_to_routine: &[Vec<Option<RoutineId>>],
    routine_count: usize,
) -> Vec<Vec<&'a SymbolData>> {
    let mut out = vec![Vec::new(); routine_count];
    for unit in &project.units {
        let unit_idx = unit.unit_id.as_usize();
        let Some(scope_map) = scope_to_routine.get(unit_idx) else {
            continue;
        };
        for symbol in unit
            .symbols
            .iter()
            .filter(|symbol| trackable_symbol_kind(symbol.kind))
        {
            let Some(routine_id) = scope_map.get(symbol.scope.as_usize()).copied().flatten() else {
                continue;
            };
            if let Some(symbols) = out.get_mut(routine_id.as_usize()) {
                symbols.push(symbol);
            }
        }
    }
    for symbols in &mut out {
        symbols.sort_by(|left, right| {
            left.decl_range
                .start
                .cmp(&right.decl_range.start)
                .then(left.decl_range.end.cmp(&right.decl_range.end))
                .then((left.kind as u8).cmp(&(right.kind as u8)))
                .then(left.name.cmp(&right.name))
        });
    }
    out
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
        RoutineInstructionKind::Find => 7,
        RoutineInstructionKind::FieldSymbolBind => 8,
        RoutineInstructionKind::ValueRead => 9,
        RoutineInstructionKind::UnknownEffect => 10,
        RoutineInstructionKind::Branch => 11,
        RoutineInstructionKind::LoopHeader => 12,
        RoutineInstructionKind::Terminator => 13,
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
        | RoutineInstructionSite::Find { index }
        | RoutineInstructionSite::FieldSymbolBind { index } => index,
        RoutineInstructionSite::ValueRead { reference } => reference.0,
        RoutineInstructionSite::UnknownEffect => 0,
        RoutineInstructionSite::Branch { kind } => match kind {
            RoutineBranchKind::If => 0,
            RoutineBranchKind::Case => 1,
            RoutineBranchKind::At => 2,
            RoutineBranchKind::Try => 3,
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
            RoutineTerminatorKind::Stop => 6,
        },
    }
}

fn routine_kind(kind: ScopeKind) -> Option<RoutineKind> {
    match kind {
        ScopeKind::File => Some(RoutineKind::GlobalDeclarations),
        ScopeKind::Method => Some(RoutineKind::Method),
        ScopeKind::Form => Some(RoutineKind::Form),
        ScopeKind::Module => Some(RoutineKind::Module),
        ScopeKind::EventBlock => Some(RoutineKind::EventBlock),
        ScopeKind::Class
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
        | ScopeKind::AtBlock
        | ScopeKind::TryBlock
        | ScopeKind::SelectBlock => None,
    }
}

fn synthetic_routine_name(kind: RoutineKind, scope: ScopeId) -> Arc<str> {
    Arc::from(format!(
        "<{}:{}>",
        match kind {
            RoutineKind::GlobalDeclarations => "global_declarations",
            RoutineKind::Method => "method",
            RoutineKind::Form => "form",
            RoutineKind::Module => "module",
            RoutineKind::EventBlock => "event",
        },
        scope.0
    ))
}

fn scope_has_global_declaration_activity(unit: &UnitAnalysis, file_scope: ScopeId) -> bool {
    unit.assignment_sites
        .iter()
        .any(|site| scope_maps_to_global_declarations(unit, site.scope, file_scope))
        || unit
            .call_sites
            .iter()
            .any(|site| scope_maps_to_global_declarations(unit, site.scope, file_scope))
        || unit
            .perform_calls
            .iter()
            .any(|call| scope_maps_to_global_declarations(unit, call.scope, file_scope))
        || unit
            .find_sites
            .iter()
            .any(|site| scope_maps_to_global_declarations(unit, site.scope, file_scope))
        || unit
            .sql_queries
            .iter()
            .any(|query| scope_maps_to_global_declarations(unit, query.scope, file_scope))
        || unit.value_flow_edges.iter().any(|edge| {
            matches!(
                edge.kind,
                crate::ValueFlowKind::FieldSymbolAssignment
                    | crate::ValueFlowKind::ConditionalFieldSymbolAssignment
            ) && scope_maps_to_global_declarations(unit, edge.scope, file_scope)
        })
        || unit
            .routine_sites
            .iter()
            .any(|site| scope_maps_to_global_declarations(unit, site.scope, file_scope))
        || unit
            .routine_control_regions
            .iter()
            .any(|region| scope_maps_to_global_declarations(unit, region.scope(), file_scope))
}

fn scope_maps_to_global_declarations(
    unit: &UnitAnalysis,
    scope: ScopeId,
    file_scope: ScopeId,
) -> bool {
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        let Some(scope_data) = unit.scopes.get(scope_id.as_usize()) else {
            return false;
        };
        match scope_data.kind {
            ScopeKind::File => return scope_id == file_scope,
            ScopeKind::Method | ScopeKind::Form | ScopeKind::Module | ScopeKind::EventBlock => {
                return false;
            }
            ScopeKind::Class
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
            | ScopeKind::AtBlock
            | ScopeKind::TryBlock
            | ScopeKind::SelectBlock => current = scope_data.parent,
        }
    }
    false
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

fn value_is_definitely_assigned_on_entry(
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    value: &RoutineDataflowValue,
    values: &[RoutineDataflowValue],
    structure_assignment_trackers: &[Option<StructureAssignmentTracker>],
) -> bool {
    match value.kind {
        DataflowValueKind::Parameter | DataflowValueKind::Constant => true,
        DataflowValueKind::Variable => {
            if value_has_explicit_declaration_initializer(unit, value) {
                return true;
            }
            if value_is_implicit_me_symbol(unit, value) {
                return true;
            }
            if value_is_constructor_expression_binding(unit, value) {
                return true;
            }
            unit.symbols
                .get(value.symbol.symbol.as_usize())
                .is_some_and(|symbol| {
                    symbol.type_clause_display.is_some()
                        && (structure_assignment_trackers[value.id.as_usize()].is_none()
                            || value_symbol_is_internal_table(project, unit, value.id, values))
                })
        }
        DataflowValueKind::FieldSymbol | DataflowValueKind::Other => false,
    }
}

fn value_is_implicit_me_symbol(unit: &UnitAnalysis, value: &RoutineDataflowValue) -> bool {
    let Some(symbol) = unit.symbols.get(value.symbol.symbol.as_usize()) else {
        return false;
    };
    if symbol.kind != SymbolKind::Variable || symbol.name.as_ref() != "me" {
        return false;
    }
    let Some(scope) = unit.scopes.get(symbol.scope.as_usize()) else {
        return false;
    };
    if scope.kind != ScopeKind::Method {
        return false;
    }
    let Some(owner_id) = scope.owner else {
        return false;
    };
    let Some(owner_symbol) = unit.symbols.get(owner_id.as_usize()) else {
        return false;
    };
    if owner_symbol.kind != SymbolKind::Method {
        return false;
    }
    symbol.decl_range == (0..0) || symbol.decl_range == owner_symbol.decl_range
}

fn value_is_constructor_expression_binding(
    unit: &UnitAnalysis,
    value: &RoutineDataflowValue,
) -> bool {
    let Some(symbol) = unit.symbols.get(value.symbol.symbol.as_usize()) else {
        return false;
    };
    let Some(scope) = unit.scopes.get(symbol.scope.as_usize()) else {
        return false;
    };
    // Constructor/LET/REDUCE binders use LoopBlock scopes but have no statement loop header
    // transfer; the expression itself initializes them before body reads execute.
    scope.kind == ScopeKind::LoopBlock
        && !unit.routine_control_regions.iter().any(|region| {
            matches!(
                region,
                RoutineControlRegionData::Loop(data) if data.body_scope == scope.id
            )
        })
}

fn value_has_explicit_declaration_initializer(
    unit: &UnitAnalysis,
    value: &RoutineDataflowValue,
) -> bool {
    unit.symbols
        .get(value.symbol.symbol.as_usize())
        .is_some_and(|symbol| symbol.value_clause_display.is_some())
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
