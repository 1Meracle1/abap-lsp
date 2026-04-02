use std::collections::HashMap;
use std::sync::Arc;

use abap_parser::ParseResult;

use crate::collector::collect_unit;
use crate::def_map::{Diagnostic, DiagnosticKind, UnitAnalysis};
use crate::ids::UnitId;
use crate::resolver::{resolve_project_cross_unit, resolve_unit};
use crate::validate::validate_project;

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

pub fn analyze_unit(uri: impl Into<Arc<str>>, source: &str, parse: &ParseResult) -> UnitAnalysis {
    let uri = uri.into();
    let mut unit = collect_unit(
        UnitId(0),
        Arc::clone(&uri),
        source,
        &parse.file,
        &parse.tokens,
    );
    resolve_unit(&mut unit);
    let mut project = ProjectAnalysis {
        units: vec![unit],
        uri_to_unit: HashMap::from([(uri, UnitId(0))]),
        provided_name_to_unit: HashMap::new(),
        diagnostics: Vec::new(),
    };
    validate_project(&mut project);
    project.units.pop().expect("single unit analysis")
}

pub fn analyze_project(inputs: &[ProjectInput<'_>]) -> ProjectAnalysis {
    let mut units = Vec::with_capacity(inputs.len());
    let mut uri_to_unit = HashMap::new();
    let mut provided_name_to_unit = HashMap::new();

    for (idx, input) in inputs.iter().enumerate() {
        let unit_id = UnitId(idx as u32);
        let uri: Arc<str> = Arc::from(input.uri);
        let mut unit = collect_unit(
            unit_id,
            Arc::clone(&uri),
            input.source,
            &input.parse.file,
            &input.parse.tokens,
        );
        resolve_unit(&mut unit);
        uri_to_unit.insert(uri, unit_id);
        for name in &unit.provided_names {
            provided_name_to_unit
                .entry(Arc::clone(name))
                .or_insert(unit_id);
        }
        units.push(unit);
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

    let mut project = ProjectAnalysis {
        units,
        uri_to_unit,
        provided_name_to_unit,
        diagnostics: Vec::new(),
    };
    validate_project(&mut project);
    for unit in &project.units {
        for diagnostic in &unit.diagnostics {
            if !project.diagnostics.contains(diagnostic) {
                project.diagnostics.push(diagnostic.clone());
            }
        }
    }
    project
}
