use std::collections::HashMap;
use std::sync::Arc;

use abap_parser::ParseResult;

use crate::collector::collect_unit;
use crate::def_map::{Diagnostic, DiagnosticKind, UnitAnalysis};
use crate::ids::UnitId;
use crate::resolver::{build_scope_index, resolve_project_cross_unit, resolve_unit_with_index};
use crate::validate::validate_project_with_scope_indexes;

#[derive(Debug, Clone, Copy)]
pub struct ProjectInput<'a> {
    pub uri: &'a str,
    pub source: &'a str,
    pub parse: &'a ParseResult,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectAnalysis {
    pub units: Vec<UnitAnalysis>,
    pub uri_to_unit: HashMap<Arc<str>, UnitId>,
    pub provided_name_to_unit: HashMap<Arc<str>, UnitId>,
    pub diagnostics: Vec<Diagnostic>,
}

impl ProjectAnalysis {
    pub fn unit_by_uri(&self, uri: &str) -> Option<&UnitAnalysis> {
        self.uri_to_unit
            .get(uri)
            .and_then(|unit_id| self.units.get(unit_id.as_usize()))
    }
}

pub fn analyze_unit_locally(
    unit_id: UnitId,
    uri: impl Into<Arc<str>>,
    source: &str,
    parse: &ParseResult,
) -> UnitAnalysis {
    let uri = uri.into();
    let mut unit = collect_unit(unit_id, uri, source, &parse.file, &parse.tokens);
    let scope_index = build_scope_index(&unit);
    resolve_unit_with_index(&mut unit, &scope_index);
    unit.rebuild_semantic_index();
    unit
}

pub fn analyze_project_from_units(mut units: Vec<UnitAnalysis>) -> ProjectAnalysis {
    let scope_indexes: Vec<_> = units.iter().map(build_scope_index).collect();
    let mut uri_to_unit = HashMap::new();
    let mut provided_name_to_unit = HashMap::new();

    for unit in &units {
        let unit_id = unit.unit_id;
        uri_to_unit.insert(Arc::clone(&unit.uri), unit_id);
        for name in &unit.provided_names {
            provided_name_to_unit
                .entry(Arc::clone(name))
                .or_insert(unit_id);
        }
    }

    for unit in &mut units {
        for edge in &mut unit.include_edges {
            edge.target = provided_name_to_unit.get(&edge.name).copied();
            if edge.target.is_none() {
                unit.diagnostics.push(Diagnostic {
                    kind: DiagnosticKind::UnresolvedInclude,
                    range: edge.range.clone(),
                    message: format!("unresolved include '{}'", edge.name),
                });
            }
        }
    }

    resolve_project_cross_unit(&mut units);
    for unit in &mut units {
        unit.rebuild_semantic_index();
    }

    let mut project = ProjectAnalysis {
        units,
        uri_to_unit,
        provided_name_to_unit,
        diagnostics: Vec::new(),
    };
    validate_project_with_scope_indexes(&mut project, &scope_indexes);
    for unit in &project.units {
        for diagnostic in &unit.diagnostics {
            if !project.diagnostics.contains(diagnostic) {
                project.diagnostics.push(diagnostic.clone());
            }
        }
    }
    project
}

pub fn analyze_unit(uri: impl Into<Arc<str>>, source: &str, parse: &ParseResult) -> UnitAnalysis {
    let uri = uri.into();
    let mut unit = collect_unit(
        UnitId(0),
        Arc::clone(&uri),
        source,
        &parse.file,
        &parse.tokens,
    );
    let scope_indexes = vec![build_scope_index(&unit)];
    resolve_unit_with_index(&mut unit, &scope_indexes[0]);
    unit.rebuild_semantic_index();
    let mut project = ProjectAnalysis {
        units: vec![unit],
        uri_to_unit: HashMap::from([(uri, UnitId(0))]),
        provided_name_to_unit: HashMap::new(),
        diagnostics: Vec::new(),
    };
    validate_project_with_scope_indexes(&mut project, &scope_indexes);
    project.units.pop().expect("single unit analysis")
}

pub fn analyze_project(inputs: &[ProjectInput<'_>]) -> ProjectAnalysis {
    let units: Vec<_> = inputs
        .iter()
        .enumerate()
        .map(|(idx, input)| {
            analyze_unit_locally(
                UnitId(idx as u32),
                Arc::from(input.uri),
                input.source,
                input.parse,
            )
        })
        .collect();
    analyze_project_from_units(units)
}
