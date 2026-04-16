use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::Arc;

use abap_lexer::TextRange;
use abap_symbols::{
    DataflowValueKind, FormParameterPassingKind, FormParameterSection,
    FunctionModuleParameterSection, MethodParameterSection, ProjectAnalysis,
    ProjectRoutineAnalysis, RoutineAnalysis, RoutineInstructionSite, RoutineTerminatorKind,
    ScopeId, ScopeKind, SymbolHandle, SymbolKind, UnitAnalysis, UnitId,
};

use super::call_graph::{CallGraphResolutionStatus, ProjectCallGraph};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallableParameterDirection {
    Input,
    Output,
    InOut,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallableParameterSummary {
    pub symbol: Option<SymbolHandle>,
    pub name: Arc<str>,
    pub decl_unit: UnitId,
    pub decl_range: TextRange,
    pub direction: CallableParameterDirection,
    pub reference_like: bool,
    pub may_read: bool,
    pub may_write: bool,
}

impl CallableParameterSummary {
    fn direct_input(&self) -> bool {
        matches!(
            self.direction,
            CallableParameterDirection::Input | CallableParameterDirection::InOut
        )
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ProjectCallableSummaryMetrics {
    pub summary_count: usize,
    pub parameter_count: usize,
    pub direct_micros: u128,
    pub propagation_micros: u128,
    pub total_micros: u128,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallableSummary {
    pub routine: abap_symbols::RoutineId,
    pub owner: SymbolHandle,
    pub unit: UnitId,
    pub kind: abap_symbols::RoutineKind,
    pub name: Arc<str>,
    pub decl_range: TextRange,
    pub parameters: Vec<CallableParameterSummary>,
    pub may_terminate_non_locally: bool,
    pub may_not_return_normally: bool,
    pub may_read_through_reference_inputs: bool,
    pub may_write_through_reference_inputs: bool,
    pub may_bind_field_symbols: bool,
    pub dataflow_barrier: bool,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ProjectCallableSummaryAnalysis {
    pub summaries: Vec<CallableSummary>,
    pub metrics: ProjectCallableSummaryMetrics,
    owner_to_summary: HashMap<SymbolHandle, usize>,
    unit_summaries: Vec<Vec<usize>>,
    parameter_lookup: HashMap<(UnitId, usize, usize), (usize, usize)>,
}

impl ProjectCallableSummaryAnalysis {
    pub fn summary_for_owner(&self, owner: SymbolHandle) -> Option<&CallableSummary> {
        self.owner_to_summary
            .get(&owner)
            .and_then(|idx| self.summaries.get(*idx))
    }

    pub fn summary_for_parameter_decl(
        &self,
        unit: UnitId,
        range: &TextRange,
    ) -> Option<(&CallableSummary, &CallableParameterSummary)> {
        let (summary_idx, parameter_idx) = self
            .parameter_lookup
            .get(&(unit, range.start, range.end))
            .copied()?;
        let summary = self.summaries.get(summary_idx)?;
        let parameter = summary.parameters.get(parameter_idx)?;
        Some((summary, parameter))
    }

    pub fn summaries_for_unit(&self, unit: UnitId) -> impl Iterator<Item = &CallableSummary> + '_ {
        self.unit_summaries
            .get(unit.as_usize())
            .into_iter()
            .flat_map(|indexes| indexes.iter().copied())
            .filter_map(|idx| self.summaries.get(idx))
    }
}

pub(crate) fn build_project_callable_summary_analysis(
    project: &ProjectAnalysis,
    routine_analysis: &ProjectRoutineAnalysis,
    call_graph: &ProjectCallGraph,
) -> ProjectCallableSummaryAnalysis {
    let total_timer = std::time::Instant::now();
    let direct_timer = std::time::Instant::now();
    let mut out = ProjectCallableSummaryAnalysis {
        unit_summaries: vec![Vec::new(); project.units.len()],
        ..ProjectCallableSummaryAnalysis::default()
    };

    let mut node_ids = Vec::new();
    for routine in &routine_analysis.routines {
        let Some(owner) = routine.descriptor.owner else {
            continue;
        };
        let Some(unit) = project.units.get(owner.unit.as_usize()) else {
            continue;
        };
        let reachable = reachable_instruction_bitmap(routine);
        let mut parameters = build_parameter_summaries(unit, routine);
        annotate_direct_parameter_effects(&mut parameters, routine, &reachable);

        let may_terminate_non_locally = routine.ir.instructions.iter().any(|instruction| {
            reachable
                .get(instruction.id.as_usize())
                .copied()
                .unwrap_or(false)
                && matches!(
                    instruction.site,
                    RoutineInstructionSite::Terminator {
                        kind: RoutineTerminatorKind::Raise
                            | RoutineTerminatorKind::Leave
                            | RoutineTerminatorKind::LeaveListProcessing
                    }
                )
        });
        let may_bind_field_symbols = direct_field_symbol_binding(routine, &reachable);
        let has_unknown_effect = routine.ir.instructions.iter().any(|instruction| {
            reachable
                .get(instruction.id.as_usize())
                .copied()
                .unwrap_or(false)
                && matches!(instruction.site, RoutineInstructionSite::UnknownEffect)
        });
        let may_read_through_reference_inputs = parameters.iter().any(|parameter| {
            parameter.reference_like && parameter.direct_input() && parameter.may_read
        });
        let may_write_through_reference_inputs = parameters.iter().any(|parameter| {
            parameter.reference_like && parameter.direct_input() && parameter.may_write
        });
        let summary_idx = out.summaries.len();
        out.owner_to_summary.insert(owner, summary_idx);
        out.unit_summaries[owner.unit.as_usize()].push(summary_idx);
        out.summaries.push(CallableSummary {
            routine: routine.descriptor.id,
            owner,
            unit: owner.unit,
            kind: routine.descriptor.kind,
            name: Arc::clone(&routine.descriptor.name),
            decl_range: routine.descriptor.decl_range.clone(),
            parameters,
            may_terminate_non_locally,
            may_not_return_normally: may_terminate_non_locally,
            may_read_through_reference_inputs,
            may_write_through_reference_inputs,
            may_bind_field_symbols,
            dataflow_barrier: has_unknown_effect
                || may_terminate_non_locally
                || may_write_through_reference_inputs
                || may_bind_field_symbols,
        });
        node_ids.push(call_graph_node_id_for_owner(call_graph, project, owner));
    }

    for (summary_idx, summary) in out.summaries.iter().enumerate() {
        for (parameter_idx, parameter) in summary.parameters.iter().enumerate() {
            out.parameter_lookup.insert(
                (
                    parameter.decl_unit,
                    parameter.decl_range.start,
                    parameter.decl_range.end,
                ),
                (summary_idx, parameter_idx),
            );
        }
    }

    out.metrics.summary_count = out.summaries.len();
    out.metrics.parameter_count = out
        .summaries
        .iter()
        .map(|summary| summary.parameters.len())
        .sum();
    out.metrics.direct_micros = direct_timer.elapsed().as_micros();

    let propagation_timer = std::time::Instant::now();
    let mut reverse_edges = vec![Vec::new(); out.summaries.len()];
    let node_to_summary: HashMap<_, _> = node_ids
        .iter()
        .enumerate()
        .filter_map(|(idx, node_id)| node_id.as_ref().map(|node_id| (Arc::clone(node_id), idx)))
        .collect();

    for (summary_idx, node_id) in node_ids.iter().enumerate() {
        let Some(node_id) = node_id.as_ref() else {
            continue;
        };
        for edge in call_graph.outbound_calls(node_id) {
            if edge.resolution_status == CallGraphResolutionStatus::Unresolved {
                out.summaries[summary_idx].dataflow_barrier = true;
                continue;
            }
            let Some(target) = edge.target.as_ref() else {
                continue;
            };
            let Some(&callee_idx) = node_to_summary.get(target) else {
                continue;
            };
            reverse_edges[callee_idx].push(summary_idx);
        }
    }

    propagate_boolean_summaries(
        &mut out.summaries,
        &reverse_edges,
        |summary| summary.may_terminate_non_locally,
        |summary, value| summary.may_terminate_non_locally = value,
    );
    propagate_boolean_summaries(
        &mut out.summaries,
        &reverse_edges,
        |summary| summary.may_not_return_normally,
        |summary, value| summary.may_not_return_normally = value,
    );
    propagate_boolean_summaries(
        &mut out.summaries,
        &reverse_edges,
        |summary| summary.dataflow_barrier,
        |summary, value| summary.dataflow_barrier = value,
    );
    out.metrics.propagation_micros = propagation_timer.elapsed().as_micros();
    out.metrics.total_micros = total_timer.elapsed().as_micros();
    out
}

fn propagate_boolean_summaries(
    summaries: &mut [CallableSummary],
    reverse_edges: &[Vec<usize>],
    get_flag: impl Fn(&CallableSummary) -> bool,
    mut set_flag: impl FnMut(&mut CallableSummary, bool),
) {
    let mut queue = VecDeque::new();
    let mut queued = vec![false; summaries.len()];
    for (idx, summary) in summaries.iter().enumerate() {
        if get_flag(summary) {
            queue.push_back(idx);
            queued[idx] = true;
        }
    }

    while let Some(callee_idx) = queue.pop_front() {
        queued[callee_idx] = false;
        if !get_flag(&summaries[callee_idx]) {
            continue;
        }
        for &caller_idx in &reverse_edges[callee_idx] {
            if get_flag(&summaries[caller_idx]) {
                continue;
            }
            set_flag(&mut summaries[caller_idx], true);
            if !queued[caller_idx] {
                queue.push_back(caller_idx);
                queued[caller_idx] = true;
            }
        }
    }
}

fn build_parameter_summaries(
    unit: &UnitAnalysis,
    routine: &RoutineAnalysis,
) -> Vec<CallableParameterSummary> {
    let Some(owner) = routine.descriptor.owner else {
        return Vec::new();
    };
    let symbol = unit.symbol(owner.symbol);
    let routine_symbols = routine_parameter_symbols(unit, owner);

    let parameters = match symbol.kind {
        SymbolKind::Method => method_parameter_summaries(unit, routine, &routine_symbols),
        SymbolKind::Form => form_parameter_summaries(unit, owner, &routine_symbols),
        SymbolKind::Module => function_module_parameter_summaries(unit, owner, &routine_symbols),
        _ => Vec::new(),
    };

    if !parameters.is_empty() {
        return parameters;
    }

    fallback_parameter_summaries(unit, owner, &routine_symbols)
}

fn method_parameter_summaries(
    unit: &UnitAnalysis,
    routine: &RoutineAnalysis,
    routine_symbols: &HashMap<Arc<str>, SymbolHandle>,
) -> Vec<CallableParameterSummary> {
    let Some(member) = unit
        .semantic()
        .decls()
        .class_member_at_offset(routine.descriptor.decl_range.start)
        .or_else(|| {
            unit.semantic()
                .decls()
                .class_member_at_offset(routine.descriptor.scope_range.start)
        })
    else {
        return Vec::new();
    };

    member
        .parameters
        .iter()
        .map(|parameter| CallableParameterSummary {
            symbol: routine_symbols.get(&parameter.name).copied(),
            name: Arc::clone(&parameter.name),
            decl_unit: unit.unit_id,
            decl_range: parameter.range.clone(),
            direction: match parameter.section {
                MethodParameterSection::Importing => CallableParameterDirection::Input,
                MethodParameterSection::Exporting
                | MethodParameterSection::Receiving
                | MethodParameterSection::Returning => CallableParameterDirection::Output,
                MethodParameterSection::Changing => CallableParameterDirection::InOut,
            },
            reference_like: matches!(
                parameter.section,
                MethodParameterSection::Importing | MethodParameterSection::Changing
            ) || parameter
                .declared_type
                .as_ref()
                .is_some_and(|declared_type| declared_type.is_ref),
            may_read: false,
            may_write: false,
        })
        .collect()
}

fn function_module_parameter_summaries(
    unit: &UnitAnalysis,
    owner: SymbolHandle,
    routine_symbols: &HashMap<Arc<str>, SymbolHandle>,
) -> Vec<CallableParameterSummary> {
    let Some(function_module) = unit.function_module(owner.symbol) else {
        return Vec::new();
    };

    function_module
        .parameters
        .iter()
        .map(|parameter| CallableParameterSummary {
            symbol: routine_symbols.get(&parameter.name).copied(),
            name: Arc::clone(&parameter.name),
            decl_unit: unit.unit_id,
            decl_range: parameter.range.clone(),
            direction: match parameter.section {
                FunctionModuleParameterSection::Importing => CallableParameterDirection::Input,
                FunctionModuleParameterSection::Exporting => CallableParameterDirection::Output,
                FunctionModuleParameterSection::Changing
                | FunctionModuleParameterSection::Tables => CallableParameterDirection::InOut,
            },
            reference_like: !matches!(parameter.section, FunctionModuleParameterSection::Exporting)
                || parameter
                    .declared_type
                    .as_ref()
                    .is_some_and(|declared_type| declared_type.is_ref),
            may_read: false,
            may_write: false,
        })
        .collect()
}

fn form_parameter_summaries(
    unit: &UnitAnalysis,
    owner: SymbolHandle,
    routine_symbols: &HashMap<Arc<str>, SymbolHandle>,
) -> Vec<CallableParameterSummary> {
    let Some(form) = unit.form_routine(owner.symbol) else {
        return Vec::new();
    };

    form.parameters
        .iter()
        .map(|parameter| {
            let symbol = SymbolHandle {
                unit: unit.unit_id,
                symbol: parameter.symbol,
            };
            let symbol_data = unit.symbol(parameter.symbol);
            CallableParameterSummary {
                symbol: routine_symbols
                    .get(&symbol_data.name)
                    .copied()
                    .or(Some(symbol)),
                name: Arc::clone(&symbol_data.name),
                decl_unit: unit.unit_id,
                decl_range: symbol_data.decl_range.clone(),
                direction: match parameter.section {
                    FormParameterSection::Using => CallableParameterDirection::Input,
                    FormParameterSection::Tables | FormParameterSection::Changing => {
                        CallableParameterDirection::InOut
                    }
                },
                reference_like: matches!(
                    parameter.section,
                    FormParameterSection::Tables | FormParameterSection::Changing
                ) || parameter.passing != FormParameterPassingKind::Value
                    || symbol_data
                        .declared_type
                        .as_ref()
                        .is_some_and(|declared_type| declared_type.is_ref),
                may_read: false,
                may_write: false,
            }
        })
        .collect()
}

fn fallback_parameter_summaries(
    unit: &UnitAnalysis,
    owner: SymbolHandle,
    routine_symbols: &HashMap<Arc<str>, SymbolHandle>,
) -> Vec<CallableParameterSummary> {
    unit.semantic()
        .decls()
        .routine_parameters(owner.symbol)
        .map(|parameter| CallableParameterSummary {
            symbol: routine_symbols
                .get(&parameter.name)
                .copied()
                .or(Some(SymbolHandle {
                    unit: unit.unit_id,
                    symbol: parameter.id,
                })),
            name: Arc::clone(&parameter.name),
            decl_unit: unit.unit_id,
            decl_range: parameter.decl_range.clone(),
            direction: CallableParameterDirection::InOut,
            reference_like: true,
            may_read: false,
            may_write: false,
        })
        .collect()
}

fn routine_parameter_symbols(
    unit: &UnitAnalysis,
    owner: SymbolHandle,
) -> HashMap<Arc<str>, SymbolHandle> {
    unit.semantic()
        .decls()
        .routine_parameters(owner.symbol)
        .map(|parameter| {
            (
                Arc::clone(&parameter.name),
                SymbolHandle {
                    unit: unit.unit_id,
                    symbol: parameter.id,
                },
            )
        })
        .collect()
}

fn annotate_direct_parameter_effects(
    parameters: &mut [CallableParameterSummary],
    routine: &RoutineAnalysis,
    reachable: &[bool],
) {
    if parameters.is_empty() {
        return;
    }

    let value_by_symbol: HashMap<_, _> = routine
        .dataflow_inputs
        .values
        .iter()
        .map(|value| (value.symbol, value.id))
        .collect();
    let parameter_by_value: HashMap<_, _> = parameters
        .iter()
        .enumerate()
        .filter_map(|(idx, parameter)| {
            parameter.symbol.and_then(|symbol| {
                value_by_symbol
                    .get(&symbol)
                    .copied()
                    .map(|value| (value, idx))
            })
        })
        .collect();

    for summary in &routine.dataflow_inputs.instructions {
        if !reachable
            .get(summary.instruction.as_usize())
            .copied()
            .unwrap_or(false)
        {
            continue;
        }
        for &value in &summary.reads {
            if let Some(&parameter_idx) = parameter_by_value.get(&value) {
                parameters[parameter_idx].may_read = true;
            }
        }
        for &value in &summary.writes {
            if let Some(&parameter_idx) = parameter_by_value.get(&value) {
                parameters[parameter_idx].may_write = true;
            }
        }
    }
}

fn direct_field_symbol_binding(routine: &RoutineAnalysis, reachable: &[bool]) -> bool {
    let field_symbol_values: HashSet<_> = routine
        .dataflow_inputs
        .values
        .iter()
        .filter(|value| value.kind == DataflowValueKind::FieldSymbol)
        .map(|value| value.id)
        .collect();
    if field_symbol_values.is_empty() {
        return false;
    }

    routine.dataflow_inputs.instructions.iter().any(|summary| {
        reachable
            .get(summary.instruction.as_usize())
            .copied()
            .unwrap_or(false)
            && summary
                .writes
                .iter()
                .any(|value| field_symbol_values.contains(value))
    })
}

fn reachable_instruction_bitmap(routine: &RoutineAnalysis) -> Vec<bool> {
    let mut reachable = vec![false; routine.ir.instructions.len()];
    for block in routine.cfg.blocks.iter().filter(|block| block.reachable) {
        for instruction in &block.instructions {
            if let Some(slot) = reachable.get_mut(instruction.as_usize()) {
                *slot = true;
            }
        }
    }
    reachable
}

fn call_graph_node_id_for_owner(
    call_graph: &ProjectCallGraph,
    project: &ProjectAnalysis,
    owner: SymbolHandle,
) -> Option<Arc<str>> {
    let unit = project.units.get(owner.unit.as_usize())?;
    let symbol = unit.symbol(owner.symbol);
    match symbol.kind {
        SymbolKind::Method => {
            let class_symbol = enclosing_class_owner(unit, symbol.scope)?;
            call_graph
                .method_node(owner.unit, class_symbol, symbol.name.as_ref())
                .or_else(|| {
                    method_tail_name(&symbol.name).and_then(|tail| {
                        call_graph.method_node(owner.unit, class_symbol, tail.as_ref())
                    })
                })
                .map(|node| Arc::clone(&node.id))
        }
        SymbolKind::Form | SymbolKind::Module | SymbolKind::Event => call_graph
            .symbol_node(owner)
            .map(|node| Arc::clone(&node.id)),
        _ => None,
    }
}

fn method_tail_name(name: &Arc<str>) -> Option<Arc<str>> {
    let tail = name.rsplit('~').next()?;
    (tail != name.as_ref()).then(|| Arc::from(tail))
}

fn enclosing_class_owner(unit: &UnitAnalysis, scope: ScopeId) -> Option<abap_symbols::SymbolId> {
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        let scope = unit.scopes.get(scope_id.as_usize())?;
        if scope.kind == ScopeKind::Class {
            return scope.owner;
        }
        current = scope.parent;
    }
    None
}
