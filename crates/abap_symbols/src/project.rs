use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::Arc;

use abap_parser::ParseResult;

use crate::collector::collect_unit;
use crate::def_map::{
    Diagnostic, DiagnosticKind, Resolution, SqlProjectionKind, StructureData, StructureFieldData,
    UnitAnalysis,
};
use crate::facts::infer_semantic_facts;
use crate::ids::{SymbolHandle, SymbolId, UnitId};
use crate::resolver::{
    ScopeIndex, build_scope_index, resolve_project_cross_unit,
    resolve_project_cross_unit_for_units, resolve_unit_with_index,
};
use crate::scope::Namespace;
use crate::validate::{
    validate_project_with_scope_indexes, validate_project_with_scope_indexes_for_units,
};

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CollectedUnit {
    pub(crate) unit: UnitAnalysis,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExportedSignature {
    pub(crate) provided_names: Vec<Arc<str>>,
    pub(crate) root_exports: Vec<(Namespace, Arc<str>)>,
    pub(crate) class_members: Vec<(Arc<str>, Arc<str>, crate::ClassMemberKind)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct LocallyResolvedUnit {
    pub(crate) unit: UnitAnalysis,
    pub(crate) scope_index: ScopeIndex,
    pub(crate) exported_signature: ExportedSignature,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct WorkspaceIndex {
    pub(crate) uri_to_unit: HashMap<Arc<str>, UnitId>,
    pub(crate) provided_name_to_unit: HashMap<Arc<str>, UnitId>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct DirtySet {
    pub(crate) unit_ids: HashSet<UnitId>,
    pub(crate) uris: HashSet<Arc<str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub(crate) struct ValidatedUnitResult {
    pub(crate) unit: UnitAnalysis,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct IncrementalProjectAnalysisResult {
    pub(crate) project: ProjectAnalysis,
    pub(crate) dirty_set: DirtySet,
}

impl ProjectAnalysis {
    pub fn unit_by_uri(&self, uri: &str) -> Option<&UnitAnalysis> {
        self.uri_to_unit
            .get(uri)
            .and_then(|unit_id| self.units.get(unit_id.as_usize()))
    }
}

fn namespace_sort_key(namespace: Namespace) -> u8 {
    match namespace {
        Namespace::Value => 0,
        Namespace::Type => 1,
        Namespace::Routine => 2,
    }
}

fn resolve_value_symbol_in_scope_chain(
    unit: &UnitAnalysis,
    scope_index: &ScopeIndex,
    scope: crate::ids::ScopeId,
    name: &Arc<str>,
) -> Option<SymbolId> {
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        if let Some(symbols) = scope_index
            .get(scope_id.as_usize())
            .and_then(|scope_map| scope_map.get(&(Namespace::Value, Arc::clone(name))))
            && let Some(symbol) = symbols.last().copied()
        {
            return Some(symbol);
        }
        current = unit
            .scopes
            .get(scope_id.as_usize())
            .and_then(|scope| scope.parent);
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

pub(crate) fn collect_unit_phase(
    unit_id: UnitId,
    uri: impl Into<Arc<str>>,
    source: &str,
    parse: &ParseResult,
) -> CollectedUnit {
    CollectedUnit {
        unit: collect_unit(unit_id, uri.into(), source, &parse.file, &parse.tokens),
    }
}

pub(crate) fn exported_signature_for_unit(unit: &UnitAnalysis) -> ExportedSignature {
    let mut root_exports = Vec::new();
    let mut class_members = Vec::new();
    for symbol in &unit.symbols {
        if symbol.scope != unit.root_scope {
            continue;
        }
        for &namespace in symbol.kind.namespaces() {
            root_exports.push((namespace, Arc::clone(&symbol.name)));
        }
    }
    for member in &unit.class_members {
        let owner = unit.symbol(member.class_symbol);
        class_members.push((
            Arc::clone(&owner.name),
            Arc::clone(&member.name),
            member.kind,
        ));
    }
    root_exports.sort_by(|left, right| {
        namespace_sort_key(left.0)
            .cmp(&namespace_sort_key(right.0))
            .then(left.1.cmp(&right.1))
    });
    root_exports.dedup();
    class_members.sort_by(|left, right| {
        left.0
            .cmp(&right.0)
            .then(left.1.cmp(&right.1))
            .then((left.2 as u8).cmp(&(right.2 as u8)))
    });
    class_members.dedup();

    let mut provided_names = unit.provided_names.clone();
    provided_names.sort();
    provided_names.dedup();

    ExportedSignature {
        provided_names,
        root_exports,
        class_members,
    }
}

pub(crate) fn resolve_local_phase(mut collected: CollectedUnit) -> LocallyResolvedUnit {
    let scope_index = build_scope_index(&collected.unit);
    resolve_unit_with_index(&mut collected.unit, &scope_index);
    infer_inline_select_target_shapes(&mut collected.unit, &scope_index);
    infer_semantic_facts(std::slice::from_mut(&mut collected.unit));
    collected.unit.rebuild_semantic_index();
    let exported_signature = exported_signature_for_unit(&collected.unit);
    LocallyResolvedUnit {
        unit: collected.unit,
        scope_index,
        exported_signature,
    }
}

pub(crate) fn analyze_unit_locally_phased(
    unit_id: UnitId,
    uri: impl Into<Arc<str>>,
    source: &str,
    parse: &ParseResult,
) -> LocallyResolvedUnit {
    resolve_local_phase(collect_unit_phase(unit_id, uri, source, parse))
}

pub fn analyze_unit_locally(
    unit_id: UnitId,
    uri: impl Into<Arc<str>>,
    source: &str,
    parse: &ParseResult,
) -> UnitAnalysis {
    analyze_unit_locally_phased(unit_id, uri, source, parse).unit
}

pub(crate) fn build_workspace_index(local_units: &[LocallyResolvedUnit]) -> WorkspaceIndex {
    let mut uri_to_unit = HashMap::new();
    let mut provided_name_to_unit = HashMap::new();

    for local in local_units {
        let unit = &local.unit;
        let unit_id = unit.unit_id;
        uri_to_unit.insert(Arc::clone(&unit.uri), unit_id);
        for name in &local.exported_signature.provided_names {
            provided_name_to_unit
                .entry(Arc::clone(name))
                .or_insert(unit_id);
        }
    }

    WorkspaceIndex {
        uri_to_unit,
        provided_name_to_unit,
    }
}

pub(crate) fn resolve_include_edges_for_units(
    units: &mut [UnitAnalysis],
    provided_name_to_unit: &HashMap<Arc<str>, UnitId>,
    dirty_unit_ids: &HashSet<UnitId>,
) {
    for unit in units {
        if !dirty_unit_ids.contains(&unit.unit_id) {
            continue;
        }
        unit.diagnostics
            .retain(|diagnostic| !matches!(diagnostic.kind, DiagnosticKind::UnresolvedInclude));
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
}

pub(crate) fn collect_project_diagnostics(project: &mut ProjectAnalysis) {
    project.diagnostics.clear();
    for unit in &project.units {
        for diagnostic in &unit.diagnostics {
            if !project.diagnostics.contains(diagnostic) {
                project.diagnostics.push(diagnostic.clone());
            }
        }
    }
}

fn dirty_set_for_all_units(local_units: &[LocallyResolvedUnit]) -> DirtySet {
    let mut dirty = DirtySet::default();
    for local in local_units {
        dirty.unit_ids.insert(local.unit.unit_id);
        dirty.uris.insert(Arc::clone(&local.unit.uri));
    }
    dirty
}

fn include_component_dirty_set(
    local_units: &[LocallyResolvedUnit],
    provided_name_to_unit: &HashMap<Arc<str>, UnitId>,
    seed_unit_ids: &HashSet<UnitId>,
) -> HashSet<UnitId> {
    let mut adjacency: HashMap<UnitId, HashSet<UnitId>> = HashMap::new();
    for local in local_units {
        let unit_id = local.unit.unit_id;
        for edge in &local.unit.include_edges {
            let Some(target) = provided_name_to_unit.get(&edge.name).copied() else {
                continue;
            };
            adjacency.entry(unit_id).or_default().insert(target);
            adjacency.entry(target).or_default().insert(unit_id);
        }
    }

    let mut visited = HashSet::new();
    let mut queue: VecDeque<UnitId> = seed_unit_ids.iter().copied().collect();
    while let Some(unit_id) = queue.pop_front() {
        if !visited.insert(unit_id) {
            continue;
        }
        if let Some(neighbors) = adjacency.get(&unit_id) {
            for neighbor in neighbors {
                queue.push_back(*neighbor);
            }
        }
    }
    visited
}

fn signature_changed(
    previous: Option<&LocallyResolvedUnit>,
    current: &LocallyResolvedUnit,
) -> bool {
    previous
        .map(|previous| previous.exported_signature != current.exported_signature)
        .unwrap_or(true)
}

fn compute_dirty_set(
    previous_project: Option<&ProjectAnalysis>,
    previous_locals: Option<&HashMap<Arc<str>, LocallyResolvedUnit>>,
    local_units: &[LocallyResolvedUnit],
    workspace_index: &WorkspaceIndex,
    changed_uris: &HashSet<Arc<str>>,
    force_full: bool,
) -> DirtySet {
    if force_full || previous_project.is_none() || previous_locals.is_none() {
        return dirty_set_for_all_units(local_units);
    }

    let previous_project = previous_project.expect("checked above");
    let previous_locals = previous_locals.expect("checked above");
    let mut dirty = DirtySet::default();
    let mut changed_unit_ids = HashSet::new();
    let mut changed_export_names = HashSet::<Arc<str>>::new();
    let mut changed_provided_names = HashSet::<Arc<str>>::new();

    for uri in changed_uris {
        let Some(&unit_id) = workspace_index.uri_to_unit.get(uri) else {
            return dirty_set_for_all_units(local_units);
        };
        changed_unit_ids.insert(unit_id);
        dirty.unit_ids.insert(unit_id);
        dirty.uris.insert(Arc::clone(uri));

        let current = local_units
            .iter()
            .find(|local| local.unit.unit_id == unit_id)
            .expect("workspace index should resolve local unit");
        let previous = previous_locals.get(uri.as_ref());
        if signature_changed(previous, current) {
            if let Some(previous) = previous {
                for (_, name) in &previous.exported_signature.root_exports {
                    changed_export_names.insert(Arc::clone(name));
                }
                for name in &previous.exported_signature.provided_names {
                    changed_provided_names.insert(Arc::clone(name));
                }
            }
            for (_, name) in &current.exported_signature.root_exports {
                changed_export_names.insert(Arc::clone(name));
            }
            for name in &current.exported_signature.provided_names {
                changed_provided_names.insert(Arc::clone(name));
            }
        }
    }

    for unit_id in include_component_dirty_set(
        local_units,
        &workspace_index.provided_name_to_unit,
        &changed_unit_ids,
    ) {
        dirty.unit_ids.insert(unit_id);
        dirty
            .uris
            .insert(Arc::clone(&local_units[unit_id.as_usize()].unit.uri));
    }

    for unit in &previous_project.units {
        if dirty.unit_ids.contains(&unit.unit_id) {
            continue;
        }

        let depends_on_changed_exports = !changed_export_names.is_empty()
            && (unit
                .include_edges
                .iter()
                .filter_map(|edge| edge.target)
                .any(|target| changed_unit_ids.contains(&target))
                || unit.references.iter().any(|reference| {
                    matches!(
                        reference.resolution,
                        Some(Resolution::Symbol(SymbolHandle { unit, .. }))
                            if changed_unit_ids.contains(&unit)
                    )
                })
                || unit
                    .references
                    .iter()
                    .any(|reference| changed_export_names.contains(&reference.name))
                || unit
                    .sql_name_refs
                    .iter()
                    .any(|sql_ref| changed_export_names.contains(&sql_ref.name))
                || unit
                    .include_edges
                    .iter()
                    .any(|edge| changed_provided_names.contains(&edge.name)));

        if depends_on_changed_exports {
            dirty.unit_ids.insert(unit.unit_id);
            dirty.uris.insert(Arc::clone(&unit.uri));
        }
    }

    dirty
}

pub(crate) fn analyze_project_incremental_from_locals(
    previous_project: Option<&ProjectAnalysis>,
    previous_locals: Option<&HashMap<Arc<str>, LocallyResolvedUnit>>,
    local_units: Vec<LocallyResolvedUnit>,
    changed_uris: &HashSet<Arc<str>>,
    force_full: bool,
) -> IncrementalProjectAnalysisResult {
    let workspace_index = build_workspace_index(&local_units);
    let scope_indexes: Vec<_> = local_units
        .iter()
        .map(|local| local.scope_index.clone())
        .collect();
    let dirty_set = compute_dirty_set(
        previous_project,
        previous_locals,
        &local_units,
        &workspace_index,
        changed_uris,
        force_full,
    );

    if force_full
        || previous_project.is_none()
        || previous_project.is_some_and(|previous| previous.units.len() != local_units.len())
    {
        let dirty_set = dirty_set_for_all_units(&local_units);
        let project = analyze_project_from_local_units(local_units);
        return IncrementalProjectAnalysisResult { project, dirty_set };
    }

    let previous_project = previous_project.expect("checked above");
    let mut units = previous_project.units.clone();
    for local in &local_units {
        if dirty_set.unit_ids.contains(&local.unit.unit_id) {
            units[local.unit.unit_id.as_usize()] = local.unit.clone();
        }
    }

    resolve_include_edges_for_units(
        &mut units,
        &workspace_index.provided_name_to_unit,
        &dirty_set.unit_ids,
    );
    resolve_project_cross_unit_for_units(&mut units, &dirty_set.unit_ids);
    infer_semantic_facts(&mut units);
    for unit_id in &dirty_set.unit_ids {
        units[unit_id.as_usize()].rebuild_semantic_index();
    }

    let mut project = ProjectAnalysis {
        units,
        uri_to_unit: workspace_index.uri_to_unit.clone(),
        provided_name_to_unit: workspace_index.provided_name_to_unit.clone(),
        diagnostics: Vec::new(),
    };
    validate_project_with_scope_indexes_for_units(
        &mut project,
        &scope_indexes,
        &dirty_set.unit_ids,
    );
    collect_project_diagnostics(&mut project);

    IncrementalProjectAnalysisResult { project, dirty_set }
}

fn analyze_project_from_local_units(local_units: Vec<LocallyResolvedUnit>) -> ProjectAnalysis {
    let workspace_index = build_workspace_index(&local_units);
    let scope_indexes: Vec<_> = local_units
        .iter()
        .map(|local| local.scope_index.clone())
        .collect();
    let mut units: Vec<_> = local_units.into_iter().map(|local| local.unit).collect();
    let dirty_unit_ids: HashSet<_> = units.iter().map(|unit| unit.unit_id).collect();

    resolve_include_edges_for_units(
        &mut units,
        &workspace_index.provided_name_to_unit,
        &dirty_unit_ids,
    );
    resolve_project_cross_unit(&mut units);
    infer_semantic_facts(&mut units);
    for unit in &mut units {
        unit.rebuild_semantic_index();
    }

    let mut project = ProjectAnalysis {
        units,
        uri_to_unit: workspace_index.uri_to_unit,
        provided_name_to_unit: workspace_index.provided_name_to_unit,
        diagnostics: Vec::new(),
    };
    validate_project_with_scope_indexes(&mut project, &scope_indexes);
    collect_project_diagnostics(&mut project);
    project
}

pub fn analyze_project_from_units(units: Vec<UnitAnalysis>) -> ProjectAnalysis {
    let local_units: Vec<_> = units
        .into_iter()
        .map(|unit| {
            let scope_index = build_scope_index(&unit);
            let exported_signature = exported_signature_for_unit(&unit);
            LocallyResolvedUnit {
                unit,
                scope_index,
                exported_signature,
            }
        })
        .collect();
    analyze_project_from_local_units(local_units)
}

pub fn analyze_unit(uri: impl Into<Arc<str>>, source: &str, parse: &ParseResult) -> UnitAnalysis {
    let uri = uri.into();
    let local = analyze_unit_locally_phased(UnitId(0), Arc::clone(&uri), source, parse);
    let LocallyResolvedUnit {
        unit, scope_index, ..
    } = local;
    let mut project = ProjectAnalysis {
        units: vec![unit],
        uri_to_unit: HashMap::from([(uri, UnitId(0))]),
        provided_name_to_unit: HashMap::new(),
        diagnostics: Vec::new(),
    };
    validate_project_with_scope_indexes(&mut project, &[scope_index]);
    collect_project_diagnostics(&mut project);
    project.units.pop().expect("single unit analysis")
}

pub fn analyze_project(inputs: &[ProjectInput<'_>]) -> ProjectAnalysis {
    let locals: Vec<_> = inputs
        .iter()
        .enumerate()
        .map(|(idx, input)| {
            analyze_unit_locally_phased(
                UnitId(idx as u32),
                Arc::from(input.uri),
                input.source,
                input.parse,
            )
        })
        .collect();
    analyze_project_from_local_units(locals)
}
