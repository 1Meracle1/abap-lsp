use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use abap_parser::ParseResult;

use crate::collector::collect_unit;
use crate::def_map::UnitAnalysis;
use crate::ids::UnitId;
use crate::project::{
    IncrementalProjectAnalysisResult, LocallyResolvedUnit, ProjectAnalysis,
    analyze_project_incremental_from_locals, analyze_unit_locally_phased,
};
use crate::resolver::{build_scope_index, resolve_unit_with_index};
use crate::validate::validate_project_with_scope_indexes;

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
                        exported_signature: super::project::exported_signature_for_unit(&local.unit),
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

    let IncrementalProjectAnalysisResult { project, dirty_set } =
        analyze_project_incremental_from_locals(
            previous_project,
            previous_locals.as_ref(),
            locals,
            changed_uris,
            force_full,
        );
    IncrementalProjectUpdate {
        project,
        dirty_uris: dirty_set.uris,
    }
}
