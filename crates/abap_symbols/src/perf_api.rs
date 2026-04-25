use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use abap_parser::ParseResult;

use crate::collector::collect_unit;
use crate::def_map::UnitAnalysis;
use crate::ids::UnitId;
use crate::project::{
    IncrementalProjectAnalysisResult, LocallyResolvedUnit, ProjectAnalysis, ProjectUpdateMetrics,
    analyze_project_incremental_from_locals, analyze_unit_locally_for_project,
    analyze_unit_locally_phased, build_workspace_index_from_units, collect_project_diagnostics,
    exported_signature_for_unit, link_class_member_implementations,
    resolve_include_edges_for_units,
};
use crate::resolver::{build_scope_index, resolve_unit_with_index};
use crate::validate::{
    validate_project_with_scope_indexes, validate_project_with_scope_indexes_for_units,
};

#[doc(hidden)]
pub fn collect_unit_only(
    uri: impl Into<Arc<str>>,
    source: &str,
    parse: &ParseResult,
) -> UnitAnalysis {
    collect_unit(UnitId(0), uri.into(), source, &parse.file, &parse.tokens)
}

#[doc(hidden)]
pub fn resolve_unit_only(unit: &mut UnitAnalysis) {
    let scope_index = build_scope_index(unit);
    resolve_unit_with_index(unit, &scope_index);
    unit.rebuild_semantic_index();
}

#[doc(hidden)]
pub fn validate_single_unit(unit: UnitAnalysis) -> ProjectAnalysis {
    let scope_indexes = vec![build_scope_index(&unit)];
    let uri = Arc::clone(&unit.uri);
    let mut project = ProjectAnalysis {
        units: vec![unit],
        uri_to_unit: HashMap::from([(uri, UnitId(0))]),
        provided_name_to_unit: HashMap::new(),
        diagnostics: Vec::new(),
    };
    validate_project_with_scope_indexes(&mut project, &scope_indexes);
    for diagnostic in &project.units[0].diagnostics {
        if !project.diagnostics.contains(diagnostic) {
            project.diagnostics.push(diagnostic.clone());
        }
    }
    project
}

#[doc(hidden)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalAnalysis {
    pub uri: Arc<str>,
    pub unit: UnitAnalysis,
    pub scope_index: Vec<HashMap<(crate::scope::Namespace, Arc<str>), Vec<crate::ids::SymbolId>>>,
}

#[doc(hidden)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IncrementalProjectUpdate {
    pub project: ProjectAnalysis,
    pub dirty_uris: HashSet<Arc<str>>,
    pub full_rebuild: bool,
    pub unit_count: usize,
    pub dirty_unit_count: usize,
    pub scope_index_clone_micros: u128,
    pub build_workspace_index_micros: u128,
    pub compute_dirty_set_micros: u128,
    pub clone_previous_units_micros: u128,
    pub apply_local_updates_micros: u128,
    pub resolve_include_edges_micros: u128,
    pub resolve_cross_unit_micros: u128,
    pub infer_semantic_facts_micros: u128,
    pub rebuild_semantic_index_micros: u128,
    pub validate_micros: u128,
    pub collect_project_diagnostics_micros: u128,
}

#[doc(hidden)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreviewProjectUpdate {
    pub project: ProjectAnalysis,
    pub changed_unit: UnitAnalysis,
    pub committed_context_only: bool,
    pub fell_back_to_single_document: bool,
}

#[doc(hidden)]
pub fn analyze_unit_local_state(
    unit_id: UnitId,
    uri: impl Into<Arc<str>>,
    source: &str,
    parse: &ParseResult,
) -> LocalAnalysis {
    let local = analyze_unit_locally_phased(unit_id, uri.into(), source, parse);
    LocalAnalysis {
        uri: Arc::clone(&local.unit.uri),
        unit: local.unit,
        scope_index: local.scope_index,
    }
}

#[doc(hidden)]
pub fn analyze_unit_local_state_for_project_build(
    unit_id: UnitId,
    uri: impl Into<Arc<str>>,
    source: &str,
    parse: &ParseResult,
) -> LocalAnalysis {
    let local = analyze_unit_locally_for_project(unit_id, uri.into(), source, parse);
    LocalAnalysis {
        uri: Arc::clone(&local.unit.uri),
        unit: local.unit,
        scope_index: local.scope_index,
    }
}

#[doc(hidden)]
pub fn incremental_project_update(
    previous_project: Option<&ProjectAnalysis>,
    previous_locals: Option<&HashMap<Arc<str>, LocalAnalysis>>,
    locals: Vec<LocalAnalysis>,
    changed_uris: &HashSet<Arc<str>>,
    force_full: bool,
) -> IncrementalProjectUpdate {
    let previous_locals = previous_locals.map(|locals| {
        locals
            .iter()
            .map(|(uri, local)| {
                (
                    Arc::clone(uri),
                    LocallyResolvedUnit {
                        unit: local.unit.clone(),
                        scope_index: local.scope_index.clone(),
                        exported_signature: super::project::exported_signature_for_unit(
                            &local.unit,
                        ),
                    },
                )
            })
            .collect::<HashMap<_, _>>()
    });
    let locals = locals
        .into_iter()
        .map(|local| {
            let exported_signature = super::project::exported_signature_for_unit(&local.unit);
            LocallyResolvedUnit {
                unit: local.unit,
                scope_index: local.scope_index,
                exported_signature,
            }
        })
        .collect();

    let IncrementalProjectAnalysisResult {
        project,
        dirty_set,
        metrics,
    } = analyze_project_incremental_from_locals(
        previous_project,
        previous_locals.as_ref(),
        locals,
        changed_uris,
        force_full,
    );
    let ProjectUpdateMetrics {
        full_rebuild,
        unit_count,
        dirty_unit_count,
        scope_index_clone_micros,
        build_workspace_index_micros,
        compute_dirty_set_micros,
        clone_previous_units_micros,
        apply_local_updates_micros,
        resolve_include_edges_micros,
        resolve_cross_unit_micros,
        infer_semantic_facts_micros,
        rebuild_semantic_index_micros,
        validate_micros,
        collect_project_diagnostics_micros,
    } = metrics;
    IncrementalProjectUpdate {
        project,
        dirty_uris: dirty_set.uris,
        full_rebuild,
        unit_count,
        dirty_unit_count,
        scope_index_clone_micros,
        build_workspace_index_micros,
        compute_dirty_set_micros,
        clone_previous_units_micros,
        apply_local_updates_micros,
        resolve_include_edges_micros,
        resolve_cross_unit_micros,
        infer_semantic_facts_micros,
        rebuild_semantic_index_micros,
        validate_micros,
        collect_project_diagnostics_micros,
    }
}

#[doc(hidden)]
pub fn preview_project_update(
    previous_project: Option<&ProjectAnalysis>,
    previous_locals: Option<&HashMap<Arc<str>, LocalAnalysis>>,
    local: LocalAnalysis,
) -> PreviewProjectUpdate {
    let exported_signature = exported_signature_for_unit(&local.unit);
    let current = LocallyResolvedUnit {
        unit: local.unit,
        scope_index: local.scope_index,
        exported_signature,
    };

    let Some(previous_project) = previous_project else {
        let project = validate_single_unit(current.unit.clone());
        return PreviewProjectUpdate {
            changed_unit: project.units[0].clone(),
            project,
            committed_context_only: false,
            fell_back_to_single_document: true,
        };
    };
    let Some(previous_locals) = previous_locals else {
        let project = validate_single_unit(current.unit.clone());
        return PreviewProjectUpdate {
            changed_unit: project.units[0].clone(),
            project,
            committed_context_only: false,
            fell_back_to_single_document: true,
        };
    };

    let mut units = previous_project.units.clone();
    let changed_unit_id = current.unit.unit_id;
    match changed_unit_id.as_usize().cmp(&units.len()) {
        std::cmp::Ordering::Less => {
            units[changed_unit_id.as_usize()] = current.unit.clone();
        }
        std::cmp::Ordering::Equal => {
            units.push(current.unit.clone());
        }
        std::cmp::Ordering::Greater => {
            let project = validate_single_unit(current.unit.clone());
            return PreviewProjectUpdate {
                changed_unit: project.units[0].clone(),
                project,
                committed_context_only: false,
                fell_back_to_single_document: true,
            };
        }
    }

    let workspace_index = build_workspace_index_from_units(&units);
    let dirty_unit_ids = HashSet::from([changed_unit_id]);
    let mut fell_back_to_single_document = false;
    let mut scope_indexes = Vec::with_capacity(units.len());
    for unit in &units {
        if unit.unit_id == changed_unit_id {
            scope_indexes.push(current.scope_index.clone());
        } else if let Some(previous) = previous_locals.get(unit.uri.as_ref()) {
            scope_indexes.push(previous.scope_index.clone());
        } else {
            fell_back_to_single_document = true;
            scope_indexes.push(build_scope_index(unit));
        }
    }

    resolve_include_edges_for_units(&mut units, &workspace_index, &dirty_unit_ids);
    crate::resolver::resolve_project_cross_unit_for_units(&mut units, &dirty_unit_ids);
    link_class_member_implementations(&mut units);
    for unit_id in &dirty_unit_ids {
        units[unit_id.as_usize()].rebuild_semantic_index();
    }

    let mut project = ProjectAnalysis {
        units,
        uri_to_unit: workspace_index.uri_to_unit,
        provided_name_to_unit: workspace_index.provided_name_to_unit,
        diagnostics: Vec::new(),
    };
    validate_project_with_scope_indexes_for_units(&mut project, &scope_indexes, &dirty_unit_ids);
    collect_project_diagnostics(&mut project);

    PreviewProjectUpdate {
        changed_unit: project.units[changed_unit_id.as_usize()].clone(),
        project,
        committed_context_only: true,
        fell_back_to_single_document,
    }
}
