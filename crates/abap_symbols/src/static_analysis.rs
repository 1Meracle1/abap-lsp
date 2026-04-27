use std::collections::HashMap;
use std::sync::Arc;

use abap_lexer::TextRange;

use crate::def_map::{Diagnostic, DiagnosticKind, UnitAnalysis};
use crate::ids::{ScopeId, SymbolHandle, UnitId};
use crate::project::ProjectAnalysis;
use crate::routine_analysis::{ProjectRoutineAnalysis, RoutineId, RoutineKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StaticAnalysisFindingKind {
    UnreachableCode,
    UseBeforeDefiniteAssignment,
    PossiblyUnboundFieldSymbol,
    DeadStore,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StaticAnalysisFinding {
    pub kind: StaticAnalysisFindingKind,
    pub range: TextRange,
    pub message: String,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RoutineStaticAnalysisFindingCounts {
    pub unreachable_code: usize,
    pub use_before_definite_assignment: usize,
    pub possibly_unbound_field_symbol: usize,
    pub dead_store: usize,
}

impl RoutineStaticAnalysisFindingCounts {
    pub fn total(&self) -> usize {
        self.unreachable_code
            + self.use_before_definite_assignment
            + self.possibly_unbound_field_symbol
            + self.dead_store
    }

    fn record(&mut self, kind: StaticAnalysisFindingKind) {
        match kind {
            StaticAnalysisFindingKind::UnreachableCode => self.unreachable_code += 1,
            StaticAnalysisFindingKind::UseBeforeDefiniteAssignment => {
                self.use_before_definite_assignment += 1;
            }
            StaticAnalysisFindingKind::PossiblyUnboundFieldSymbol => {
                self.possibly_unbound_field_symbol += 1;
            }
            StaticAnalysisFindingKind::DeadStore => self.dead_store += 1,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RoutineStaticAnalysisSummary {
    pub routine: RoutineId,
    pub unit: UnitId,
    pub scope: ScopeId,
    pub owner: Option<SymbolHandle>,
    pub kind: RoutineKind,
    pub name: Arc<str>,
    pub decl_range: TextRange,
    pub executable_range: Option<TextRange>,
    pub instruction_count: usize,
    pub reachable_instruction_count: usize,
    pub block_count: usize,
    pub reachable_block_count: usize,
    pub unreachable_block_count: usize,
    pub dataflow_converged: bool,
    pub dataflow_iterations: u32,
    pub finding_counts: RoutineStaticAnalysisFindingCounts,
    pub findings: Vec<StaticAnalysisFinding>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ProjectStaticAnalysisSummaryMetrics {
    pub routine_count: usize,
    pub finding_count: usize,
    pub collect_micros: u128,
    pub index_micros: u128,
    pub total_micros: u128,
}

/// Compact, user-facing routine-analysis surface derived from the heavier CFG/dataflow artifact.
///
/// This summary intentionally exposes only stable routine metadata and grouped findings so snapshot
/// consumers do not need to depend on the full IR/CFG/dataflow internals.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ProjectStaticAnalysisSummary {
    pub routines: Vec<RoutineStaticAnalysisSummary>,
    pub metrics: ProjectStaticAnalysisSummaryMetrics,
    owner_to_routine: HashMap<SymbolHandle, RoutineId>,
    unit_routines: Vec<Vec<RoutineId>>,
    scope_to_routine: Vec<Vec<Option<RoutineId>>>,
}

impl ProjectStaticAnalysisSummary {
    pub fn routine(&self, id: RoutineId) -> Option<&RoutineStaticAnalysisSummary> {
        self.routines.get(id.as_usize())
    }

    pub fn routine_for_owner(&self, owner: SymbolHandle) -> Option<&RoutineStaticAnalysisSummary> {
        self.owner_to_routine
            .get(&owner)
            .and_then(|routine_id| self.routine(*routine_id))
    }

    pub fn routine_for_scope(
        &self,
        unit: UnitId,
        scope: ScopeId,
    ) -> Option<&RoutineStaticAnalysisSummary> {
        self.scope_to_routine
            .get(unit.as_usize())
            .and_then(|scope_map| scope_map.get(scope.as_usize()))
            .copied()
            .flatten()
            .and_then(|routine_id| self.routine(routine_id))
    }

    pub fn routines_for_unit(
        &self,
        unit: UnitId,
    ) -> impl Iterator<Item = &RoutineStaticAnalysisSummary> + '_ {
        self.unit_routines
            .get(unit.as_usize())
            .into_iter()
            .flat_map(|routine_ids| routine_ids.iter().copied())
            .filter_map(|routine_id| self.routine(routine_id))
    }

    pub fn findings_touching_offset(
        &self,
        unit: UnitId,
        offset: usize,
    ) -> impl Iterator<Item = &StaticAnalysisFinding> + '_ {
        self.routines_for_unit(unit)
            .flat_map(|routine| routine.findings.iter())
            .filter(move |finding| finding.range.start <= offset && offset < finding.range.end)
    }
}

pub fn build_project_static_analysis_summary(
    project: &ProjectAnalysis,
    routine_analysis: &ProjectRoutineAnalysis,
) -> ProjectStaticAnalysisSummary {
    let total_timer = std::time::Instant::now();
    let mut out = ProjectStaticAnalysisSummary {
        unit_routines: vec![Vec::new(); project.units.len()],
        scope_to_routine: project
            .units
            .iter()
            .map(|unit| vec![None; unit.scopes.len()])
            .collect(),
        ..ProjectStaticAnalysisSummary::default()
    };

    let collect_timer = std::time::Instant::now();
    let mut exact_routine_scopes: Vec<Vec<Option<RoutineId>>> = project
        .units
        .iter()
        .map(|unit| vec![None; unit.scopes.len()])
        .collect();

    for routine in &routine_analysis.routines {
        let descriptor = &routine.descriptor;
        let routine_id = descriptor.id;
        let unit_idx = descriptor.unit.as_usize();
        let block_count = routine.cfg.blocks.len();
        let reachable_block_count = routine
            .cfg
            .blocks
            .iter()
            .filter(|block| block.reachable)
            .count();
        let reachable_instruction_count = routine
            .cfg
            .blocks
            .iter()
            .filter(|block| block.reachable)
            .map(|block| block.instructions.len())
            .sum();
        let mut finding_counts = RoutineStaticAnalysisFindingCounts::default();
        let findings: Vec<_> = routine
            .diagnostics
            .iter()
            .filter_map(static_analysis_finding)
            .inspect(|finding| finding_counts.record(finding.kind))
            .collect();

        if let Some(owner) = descriptor.owner {
            out.owner_to_routine.insert(owner, routine_id);
        }
        out.unit_routines[unit_idx].push(routine_id);
        exact_routine_scopes[unit_idx][descriptor.scope.as_usize()] = Some(routine_id);
        out.routines.push(RoutineStaticAnalysisSummary {
            routine: routine_id,
            unit: descriptor.unit,
            scope: descriptor.scope,
            owner: descriptor.owner,
            kind: descriptor.kind,
            name: Arc::clone(&descriptor.name),
            decl_range: descriptor.decl_range.clone(),
            executable_range: descriptor.executable_range.clone(),
            instruction_count: routine.ir.instructions.len(),
            reachable_instruction_count,
            block_count,
            reachable_block_count,
            unreachable_block_count: block_count.saturating_sub(reachable_block_count),
            dataflow_converged: routine.dataflow_result.converged,
            dataflow_iterations: routine.dataflow_result.iterations,
            finding_counts,
            findings,
        });
    }
    out.metrics.routine_count = out.routines.len();
    out.metrics.finding_count = out
        .routines
        .iter()
        .map(|routine| routine.finding_counts.total())
        .sum();
    out.metrics.collect_micros = collect_timer.elapsed().as_micros();

    let index_timer = std::time::Instant::now();
    for unit in &project.units {
        let unit_idx = unit.unit_id.as_usize();
        out.scope_to_routine[unit_idx] =
            build_scope_to_routine_map(unit, &exact_routine_scopes[unit_idx]);
    }
    out.metrics.index_micros = index_timer.elapsed().as_micros();
    out.metrics.total_micros = total_timer.elapsed().as_micros();
    out
}

fn static_analysis_finding(diagnostic: &Diagnostic) -> Option<StaticAnalysisFinding> {
    let kind = match diagnostic.kind {
        DiagnosticKind::UnreachableCode => StaticAnalysisFindingKind::UnreachableCode,
        DiagnosticKind::UseBeforeDefiniteAssignment => {
            StaticAnalysisFindingKind::UseBeforeDefiniteAssignment
        }
        DiagnosticKind::PossiblyUnboundFieldSymbol => {
            StaticAnalysisFindingKind::PossiblyUnboundFieldSymbol
        }
        DiagnosticKind::DeadStore => StaticAnalysisFindingKind::DeadStore,
        DiagnosticKind::DuplicateDeclaration
        | DiagnosticKind::ShadowedSymbol
        | DiagnosticKind::MismatchedStructuredDeclaration
        | DiagnosticKind::UnresolvedReference
        | DiagnosticKind::UnresolvedInclude
        | DiagnosticKind::IncludeCycle
        | DiagnosticKind::WrongNamespace
        | DiagnosticKind::UnknownField
        | DiagnosticKind::InvalidBuiltinNamedArgument
        | DiagnosticKind::InvalidPerformCall
        | DiagnosticKind::AbstractClassInstantiation
        | DiagnosticKind::MissingMethodImplementation
        | DiagnosticKind::MissingSuperConstructorCall
        | DiagnosticKind::InvalidObjectTypeReference
        | DiagnosticKind::IncompatibleAssignmentType
        | DiagnosticKind::IncompatibleArgumentType
        | DiagnosticKind::UnknownNamedParameter
        | DiagnosticKind::DuplicateNamedParameter
        | DiagnosticKind::MissingRequiredParameter
        | DiagnosticKind::UnverifiedOpenSqlSource
        | DiagnosticKind::InvalidOpenSqlIntoTarget
        | DiagnosticKind::InvalidOpenSqlSyntax
        | DiagnosticKind::InvalidConstructorForIteratorReuse
        | DiagnosticKind::MissingTablesDeclaration
        | DiagnosticKind::UnsortedReadTableBinarySearch => return None,
    };
    Some(StaticAnalysisFinding {
        kind,
        range: diagnostic.range.clone(),
        message: diagnostic.message.clone(),
    })
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
