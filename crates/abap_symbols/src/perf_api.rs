use std::collections::HashMap;
use std::sync::Arc;

use abap_parser::ParseResult;

use crate::collector::collect_unit;
use crate::def_map::UnitAnalysis;
use crate::ids::UnitId;
use crate::project::ProjectAnalysis;
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
