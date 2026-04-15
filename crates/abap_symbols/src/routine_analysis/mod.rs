mod cfg;
mod dataflow;
mod ids;
mod ir;
mod metrics;

use std::collections::HashMap;
use std::sync::Arc;

use abap_lexer::TextRange;

pub use cfg::{RoutineBlock, RoutineBlockKind, RoutineCfg, RoutineEdge, RoutineEdgeKind};
pub use dataflow::{
    BlockDataflowSummary, DataflowValueKind, InstructionDataflowSummary, RoutineDataflowInputs,
    RoutineDataflowResult, RoutineDataflowValue,
};
pub use ids::{DataflowValueId, RoutineBlockId, RoutineId, RoutineInstrId};
pub use ir::{
    RoutineDescriptor, RoutineInstruction, RoutineInstructionKind, RoutineInstructionSite,
    RoutineIr, RoutineKind,
};
pub use metrics::ProjectRoutineAnalysisMetrics;

use crate::def_map::{Resolution, SymbolData, SymbolKind, UnitAnalysis};
use crate::ids::{ScopeId, SymbolHandle, UnitId};
use crate::project::ProjectAnalysis;
use crate::scope::ScopeKind;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ProjectRoutineAnalysis {
    pub routines: Vec<RoutineAnalysis>,
    pub metrics: ProjectRoutineAnalysisMetrics,
    owner_to_routine: HashMap<SymbolHandle, RoutineId>,
    unit_routines: Vec<Vec<RoutineId>>,
    scope_to_routine: Vec<Vec<Option<RoutineId>>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RoutineAnalysis {
    pub descriptor: RoutineDescriptor,
    pub ir: RoutineIr,
    pub cfg: RoutineCfg,
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
                },
                ir: RoutineIr::default(),
                cfg: RoutineCfg::default(),
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
            reference.namespace == crate::scope::Namespace::Value
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
    }
    out.metrics.ir_micros = ir_timer.elapsed().as_micros();

    let cfg_timer = std::time::Instant::now();
    for routine in &mut out.routines {
        let body_range = routine
            .ir
            .instructions
            .first()
            .zip(routine.ir.instructions.last())
            .map(|(first, last)| first.range.start..last.range.end)
            .unwrap_or_else(|| routine.descriptor.scope_range.clone());
        let entry = RoutineBlockId(0);
        let body = RoutineBlockId(1);
        let exit = RoutineBlockId(2);
        routine.cfg = RoutineCfg {
            entry: Some(entry),
            exit: Some(exit),
            blocks: vec![
                RoutineBlock {
                    id: entry,
                    kind: RoutineBlockKind::Entry,
                    range: routine.descriptor.decl_range.start..routine.descriptor.decl_range.start,
                    instructions: Vec::new(),
                },
                RoutineBlock {
                    id: body,
                    kind: RoutineBlockKind::Body,
                    range: body_range,
                    instructions: routine.ir.instructions.iter().map(|inst| inst.id).collect(),
                },
                RoutineBlock {
                    id: exit,
                    kind: RoutineBlockKind::Exit,
                    range: routine.descriptor.scope_range.end..routine.descriptor.scope_range.end,
                    instructions: Vec::new(),
                },
            ],
            edges: vec![
                RoutineEdge {
                    from: entry,
                    to: body,
                    kind: RoutineEdgeKind::SyntheticFlow,
                },
                RoutineEdge {
                    from: body,
                    to: exit,
                    kind: RoutineEdgeKind::SyntheticFlow,
                },
            ],
        };
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
            | RoutineInstructionSite::SqlQuery { .. } => {}
        }
        instruction_summaries.push(InstructionDataflowSummary {
            instruction: instruction.id,
            reads,
            writes,
        });
    }

    let body_written = sorted_unique_value_ids(
        instruction_summaries
            .iter()
            .flat_map(|summary| summary.writes.iter().copied()),
    );
    let mut block_entry = Vec::with_capacity(routine.cfg.blocks.len());
    let mut block_exit = Vec::with_capacity(routine.cfg.blocks.len());
    for block in &routine.cfg.blocks {
        match block.kind {
            RoutineBlockKind::Entry => {
                block_entry.push(BlockDataflowSummary {
                    block: block.id,
                    maybe_written_values: Vec::new(),
                });
                block_exit.push(BlockDataflowSummary {
                    block: block.id,
                    maybe_written_values: Vec::new(),
                });
            }
            RoutineBlockKind::Body => {
                block_entry.push(BlockDataflowSummary {
                    block: block.id,
                    maybe_written_values: Vec::new(),
                });
                block_exit.push(BlockDataflowSummary {
                    block: block.id,
                    maybe_written_values: body_written.clone(),
                });
            }
            RoutineBlockKind::Exit => {
                block_entry.push(BlockDataflowSummary {
                    block: block.id,
                    maybe_written_values: body_written.clone(),
                });
                block_exit.push(BlockDataflowSummary {
                    block: block.id,
                    maybe_written_values: body_written.clone(),
                });
            }
        }
    }

    (
        RoutineDataflowInputs {
            values,
            instructions: instruction_summaries,
        },
        RoutineDataflowResult {
            converged: true,
            iterations: 1,
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
    }
}

fn instruction_site_sort_key(site: RoutineInstructionSite) -> u32 {
    match site {
        RoutineInstructionSite::Assignment { index }
        | RoutineInstructionSite::Call { index }
        | RoutineInstructionSite::Perform { index }
        | RoutineInstructionSite::SqlQuery { index } => index,
        RoutineInstructionSite::ValueRead { reference } => reference.0,
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
