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

use crate::def_map::{
    CaseRegionData, Diagnostic, DiagnosticKind, IfRegionData, LoopRegionData, Resolution,
    RoutineControlRegionData, RoutineSiteKind, SymbolData, SymbolKind, TryRegionData, UnitAnalysis,
};
use crate::ids::{ScopeId, SymbolHandle, UnitId};
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

        for site in &unit.routine_sites {
            let Some(routine_id) = scope_map.get(site.scope.as_usize()).copied().flatten() else {
                continue;
            };
            let instruction_site = match site.kind {
                RoutineSiteKind::UnknownEffect => RoutineInstructionSite::UnknownEffect,
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
        let (inputs, result) = build_routine_dataflow(unit, scope_map, &out.routines[routine_id]);
        out.routines[routine_id].dataflow_inputs = inputs;
        out.routines[routine_id].dataflow_result = result;
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
    range: TextRange,
    value: DataflowValueId,
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
    unit: &UnitAnalysis,
    scope_to_routine: &[Option<RoutineId>],
    routine: &RoutineAnalysis,
) -> (RoutineDataflowInputs, RoutineDataflowResult) {
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
    for symbol in tracked_symbols {
        let handle = SymbolHandle {
            unit: unit.unit_id,
            symbol: symbol.id,
        };
        let value_id = DataflowValueId(values.len() as u32);
        value_ids_by_symbol.insert(handle, value_id);
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
            .then(left.value.as_usize().cmp(&right.value.as_usize()))
    });

    let mut instruction_summaries = Vec::with_capacity(routine.ir.instructions.len());
    for instruction in &routine.ir.instructions {
        let mut reads = Vec::new();
        let mut writes = Vec::new();
        match instruction.site {
            RoutineInstructionSite::ValueRead { reference } => {
                if let Some(value) =
                    resolved_value_id_for_reference(unit, reference, &value_ids_by_symbol)
                {
                    reads.push(value);
                }
            }
            RoutineInstructionSite::Assignment { index } => {
                if let Some(assignment) = unit.assignment_sites.get(index as usize) {
                    writes = value_ids_in_range(&reference_uses, &assignment.lhs_range);
                    reads = value_ids_in_range(&reference_uses, &assignment.rhs_range);
                }
            }
            RoutineInstructionSite::Call { .. }
            | RoutineInstructionSite::Perform { .. }
            | RoutineInstructionSite::SqlQuery { .. }
            | RoutineInstructionSite::UnknownEffect
            | RoutineInstructionSite::Branch { .. }
            | RoutineInstructionSite::LoopHeader { .. }
            | RoutineInstructionSite::Terminator { .. } => {}
        }
        instruction_summaries.push(InstructionDataflowSummary {
            instruction: instruction.id,
            reads,
            writes,
        });
    }

    let local_writes: Vec<Vec<DataflowValueId>> = routine
        .cfg
        .blocks
        .iter()
        .map(|block| {
            sorted_unique_value_ids(
                block
                    .instructions
                    .iter()
                    .filter_map(|instruction| instruction_summaries.get(instruction.as_usize()))
                    .flat_map(|summary| summary.writes.iter().copied()),
            )
        })
        .collect();

    let mut block_entry_values = vec![Vec::new(); routine.cfg.blocks.len()];
    let mut block_exit_values = vec![Vec::new(); routine.cfg.blocks.len()];
    let mut changed = true;
    let mut iterations = 0u32;
    while changed {
        changed = false;
        iterations += 1;
        for block in &routine.cfg.blocks {
            let block_idx = block.id.as_usize();
            let next_entry = if block.kind == RoutineBlockKind::Entry || !block.reachable {
                Vec::new()
            } else {
                sorted_unique_value_ids(
                    block
                        .predecessors
                        .iter()
                        .filter_map(|predecessor| block_exit_values.get(predecessor.as_usize()))
                        .flat_map(|values| values.iter().copied()),
                )
            };
            let next_exit = if !block.reachable {
                Vec::new()
            } else {
                sorted_unique_value_ids(
                    next_entry
                        .iter()
                        .copied()
                        .chain(local_writes[block_idx].iter().copied()),
                )
            };
            if block_entry_values[block_idx] != next_entry {
                block_entry_values[block_idx] = next_entry;
                changed = true;
            }
            if block_exit_values[block_idx] != next_exit {
                block_exit_values[block_idx] = next_exit;
                changed = true;
            }
        }
    }

    let block_entry = routine
        .cfg
        .blocks
        .iter()
        .map(|block| BlockDataflowSummary {
            block: block.id,
            maybe_written_values: block_entry_values[block.id.as_usize()].clone(),
        })
        .collect();
    let block_exit = routine
        .cfg
        .blocks
        .iter()
        .map(|block| BlockDataflowSummary {
            block: block.id,
            maybe_written_values: block_exit_values[block.id.as_usize()].clone(),
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
    )
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

fn value_ids_in_range(reference_uses: &[ReferenceUse], range: &TextRange) -> Vec<DataflowValueId> {
    let start_idx = reference_uses.partition_point(|use_site| use_site.range.start < range.start);
    let mut values = Vec::new();
    for use_site in &reference_uses[start_idx..] {
        if use_site.range.start >= range.end {
            break;
        }
        if use_site.range.end <= range.end {
            values.push(use_site.value);
        }
    }
    sorted_unique_value_ids(values)
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
        RoutineInstructionKind::ValueRead => 4,
        RoutineInstructionKind::UnknownEffect => 5,
        RoutineInstructionKind::Branch => 6,
        RoutineInstructionKind::LoopHeader => 7,
        RoutineInstructionKind::Terminator => 8,
    }
}

fn instruction_site_sort_key(site: RoutineInstructionSite) -> u32 {
    match site {
        RoutineInstructionSite::Assignment { index }
        | RoutineInstructionSite::Call { index }
        | RoutineInstructionSite::Perform { index }
        | RoutineInstructionSite::SqlQuery { index } => index,
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
