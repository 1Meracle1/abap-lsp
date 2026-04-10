use std::collections::HashMap;
use std::sync::Arc;

use abap_parser::ParseResult;

use crate::collector::collect_unit;
use crate::def_map::{
    Diagnostic, DiagnosticKind, SqlProjectionKind, StructureData, StructureFieldData, UnitAnalysis,
};
use crate::ids::{SymbolId, UnitId};
use crate::resolver::{
    ScopeIndex, build_scope_index, resolve_project_cross_unit, resolve_unit_with_index,
};
use crate::scope::Namespace;
use crate::validate::validate_project_with_scope_indexes;

fn resolve_value_symbol_in_scope_chain(
    unit: &UnitAnalysis,
    scope_index: &ScopeIndex,
    scope: crate::ids::ScopeId,
    name: &Arc<str>,
) -> Option<SymbolId> {
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        if let Some(symbols) =
            scope_index[scope_id.as_usize()].get(&(Namespace::Value, Arc::clone(name)))
            && let Some(symbol) = symbols.last().copied()
        {
            return Some(symbol);
        }
        current = unit.scope(scope_id).parent;
    }
    None
}

fn infer_inline_select_target_shapes(unit: &mut UnitAnalysis, scope_index: &ScopeIndex) {
    let targets: Vec<_> = unit
        .sql_targets
        .iter()
        .filter(|target| target.is_inline && target.is_table)
        .filter_map(|target| {
            Some((
                target.query_id,
                target.scope,
                Arc::clone(target.target_name.as_ref()?),
            ))
        })
        .collect();

    for (query_id, scope, target_name) in targets {
        let Some(symbol_id) =
            resolve_value_symbol_in_scope_chain(unit, scope_index, scope, &target_name)
        else {
            continue;
        };
        if unit.symbol(symbol_id).structure.is_some() {
            continue;
        }

        let mut fields = Vec::new();
        for projection in unit
            .sql_projections
            .iter()
            .filter(|projection| projection.query_id == query_id)
        {
            let field_name = match projection.kind {
                SqlProjectionKind::Column => {
                    projection.alias.clone().or_else(|| projection.name.clone())
                }
                SqlProjectionKind::Aggregate | SqlProjectionKind::Expression => {
                    projection.alias.clone()
                }
                SqlProjectionKind::Star | SqlProjectionKind::QualifiedStar => None,
            };
            let Some(field_name) = field_name else {
                continue;
            };
            if fields
                .iter()
                .any(|field: &StructureFieldData| field.name == field_name)
            {
                continue;
            }
            fields.push(StructureFieldData {
                name: field_name,
                decl_range: Some(projection.range.clone()),
                decl_unit: unit.unit_id,
                structure: None,
                type_ref: None,
                value_clause_display: None,
            });
        }

        if fields.is_empty() {
            continue;
        }

        let structure_id = crate::ids::StructureId(unit.structures.len() as u32);
        unit.structures.push(StructureData {
            id: structure_id,
            origin_unit: unit.unit_id,
            origin_structure: structure_id,
            name: Arc::from(format!("<open_sql_inline:{}>", target_name.as_ref())),
            fields,
        });
        unit.symbols[symbol_id.as_usize()].structure = Some(structure_id);
    }
}

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
    infer_inline_select_target_shapes(&mut unit, &scope_index);
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
    infer_inline_select_target_shapes(&mut unit, &scope_indexes[0]);
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
