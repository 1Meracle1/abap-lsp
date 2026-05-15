use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::Arc;

use abap_parser::ParseResult;

use crate::collector::collect_unit;
use crate::def_map::{
    ClassMemberImplementationData, Diagnostic, DiagnosticKind, PerformCallData, ReferenceData,
    ReferenceKind, Resolution, SqlNameRefKind, SqlProjectionKind, StructureData,
    StructureFieldData, SymbolKind, UnitAnalysis,
};
use crate::facts::infer_semantic_facts_with_scope_indexes;
use crate::ids::{ReferenceId, SymbolHandle, SymbolId, UnitId};
use crate::resolver::{
    ScopeIndex, build_scope_index, include_predecessor_units_for_units, resolve_project_cross_unit,
    resolve_project_cross_unit_for_units, resolve_unit_with_index,
};
use crate::scope::{Namespace, ScopeKind};
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
    pub(crate) provided_name_to_units: HashMap<Arc<str>, Vec<UnitId>>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct DirtySet {
    pub(crate) unit_ids: HashSet<UnitId>,
    pub(crate) uris: HashSet<Arc<str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct IncrementalProjectAnalysisResult {
    pub(crate) project: ProjectAnalysis,
    pub(crate) dirty_set: DirtySet,
    pub(crate) metrics: ProjectUpdateMetrics,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct ProjectUpdateMetrics {
    pub(crate) full_rebuild: bool,
    pub(crate) unit_count: usize,
    pub(crate) dirty_unit_count: usize,
    pub(crate) diagnostic_scope_unit_count: usize,
    pub(crate) validation_unit_count: usize,
    pub(crate) scope_index_clone_micros: u128,
    pub(crate) build_workspace_index_micros: u128,
    pub(crate) compute_dirty_set_micros: u128,
    pub(crate) clone_previous_units_micros: u128,
    pub(crate) apply_local_updates_micros: u128,
    pub(crate) resolve_include_edges_micros: u128,
    pub(crate) resolve_cross_unit_micros: u128,
    pub(crate) infer_semantic_facts_micros: u128,
    pub(crate) rebuild_semantic_index_micros: u128,
    pub(crate) validate_micros: u128,
    pub(crate) collect_project_diagnostics_micros: u128,
}

impl ProjectAnalysis {
    pub fn unit_by_uri(&self, uri: &str) -> Option<&UnitAnalysis> {
        self.uri_to_unit
            .get(uri)
            .and_then(|unit_id| self.units.get(unit_id.as_usize()))
    }

    pub fn resolve_perform_call_target(
        &self,
        caller_unit: &UnitAnalysis,
        perform_call: &PerformCallData,
    ) -> Option<SymbolHandle> {
        if perform_call.is_dynamic {
            return None;
        }

        if let Some(program) = &perform_call.program {
            if program.is_dynamic {
                return None;
            }
            let target_unit = self.resolve_perform_program_unit(program.name.as_ref())?;
            return self
                .find_root_form_in_unit_or_includes(target_unit, &perform_call.routine_name);
        }

        caller_unit
            .references
            .iter()
            .find(|reference| {
                reference.kind == ReferenceKind::RoutineCall
                    && reference.namespace == Namespace::Routine
                    && reference.range == perform_call.routine_range
                    && reference.name == perform_call.routine_name
            })
            .and_then(|reference| match reference.resolution {
                Some(Resolution::Symbol(handle)) => Some(handle),
                _ => None,
            })
    }

    fn resolve_perform_program_unit(&self, program_name: &str) -> Option<UnitId> {
        let lowered = program_name.to_ascii_lowercase();
        self.provided_name_to_unit
            .get(lowered.as_str())
            .copied()
            .or_else(|| {
                self.units.iter().find_map(|unit| {
                    unit.provided_names
                        .iter()
                        .any(|name| name.as_ref().eq_ignore_ascii_case(&lowered))
                        .then_some(unit.unit_id)
                })
            })
    }

    pub fn visible_type_owner_handle(
        &self,
        from_unit: UnitId,
        name: &Arc<str>,
    ) -> Option<SymbolHandle> {
        if let Some(current) = self.units.get(from_unit.as_usize())
            && let Some(symbol) = root_type_owner_symbol(current, name, true)
        {
            return Some(SymbolHandle {
                unit: from_unit,
                symbol,
            });
        }
        let predecessors = include_predecessor_units_for_units(&self.units);
        self.visible_type_owner_handle_with_predecessors(from_unit, name, &predecessors)
    }

    pub(crate) fn visible_type_owner_handle_with_predecessors(
        &self,
        from_unit: UnitId,
        name: &Arc<str>,
        predecessors: &[Vec<UnitId>],
    ) -> Option<SymbolHandle> {
        self.visible_type_owner_handle_inner(from_unit, name, true, predecessors)
            .or_else(|| self.visible_type_owner_handle_inner(from_unit, name, false, predecessors))
    }

    fn visible_type_owner_handle_inner(
        &self,
        from_unit: UnitId,
        name: &Arc<str>,
        require_definition: bool,
        predecessors: &[Vec<UnitId>],
    ) -> Option<SymbolHandle> {
        let current = self.units.get(from_unit.as_usize())?;
        if let Some(symbol) = root_type_owner_symbol(current, name, require_definition) {
            return Some(SymbolHandle {
                unit: from_unit,
                symbol,
            });
        }

        for unit_id in predecessors
            .get(from_unit.as_usize())
            .into_iter()
            .flatten()
            .rev()
            .copied()
        {
            let unit = &self.units[unit_id.as_usize()];
            if let Some(symbol) = root_type_owner_symbol(unit, name, require_definition) {
                return Some(SymbolHandle {
                    unit: unit_id,
                    symbol,
                });
            }
        }

        self.units.iter().find_map(|unit| {
            root_type_owner_symbol(unit, name, require_definition).map(|symbol| SymbolHandle {
                unit: unit.unit_id,
                symbol,
            })
        })
    }

    pub fn class_member_definition_for_method_symbol(
        &self,
        implementation_unit: UnitId,
        method_symbol: SymbolId,
    ) -> Option<(UnitId, &crate::ClassMemberData)> {
        let unit = self.units.get(implementation_unit.as_usize())?;
        let method = unit.symbol(method_symbol);
        if method.kind != SymbolKind::Method {
            return None;
        }
        let class_symbol = enclosing_class_owner_in_unit(unit, method.scope)?;
        let class_name = Arc::clone(&unit.symbol(class_symbol).name);
        let definition = self.visible_type_owner_handle(implementation_unit, &class_name)?;
        let definition_unit = &self.units[definition.unit.as_usize()];
        let member = definition_unit.class_member(definition.symbol, method.name.as_ref())?;
        Some((definition.unit, member))
    }

    pub fn include_predecessor_units(&self, unit_id: UnitId) -> Vec<UnitId> {
        include_predecessor_units_for_units(&self.units)
            .get(unit_id.as_usize())
            .cloned()
            .unwrap_or_default()
    }

    pub(crate) fn include_predecessor_units_by_unit(&self) -> Vec<Vec<UnitId>> {
        include_predecessor_units_for_units(&self.units)
    }

    fn find_root_form_in_unit_or_includes(
        &self,
        start_unit: UnitId,
        routine_name: &Arc<str>,
    ) -> Option<SymbolHandle> {
        let mut visited = HashSet::new();
        let mut queue = VecDeque::from([start_unit]);
        while let Some(unit_id) = queue.pop_front() {
            if !visited.insert(unit_id) {
                continue;
            }
            let unit = self.units.get(unit_id.as_usize())?;
            if let Some(symbol) = unit.symbols.iter().find(|symbol| {
                symbol.scope == unit.root_scope
                    && symbol.kind == SymbolKind::Form
                    && symbol
                        .name
                        .as_ref()
                        .eq_ignore_ascii_case(routine_name.as_ref())
            }) {
                return Some(SymbolHandle {
                    unit: unit_id,
                    symbol: symbol.id,
                });
            }
            queue.extend(unit.include_edges.iter().filter_map(|edge| edge.target));
        }
        None
    }
}

fn root_type_owner_symbol(
    unit: &UnitAnalysis,
    name: &Arc<str>,
    require_definition: bool,
) -> Option<SymbolId> {
    unit.symbols.iter().find_map(|symbol| {
        if symbol.scope != unit.root_scope || symbol.name != *name {
            return None;
        }
        match symbol.kind {
            SymbolKind::Interface => Some(symbol.id),
            SymbolKind::Class
                if !require_definition || unit.class_definition(symbol.id).is_some() =>
            {
                Some(symbol.id)
            }
            _ => None,
        }
    })
}

fn enclosing_class_owner_in_unit(
    unit: &UnitAnalysis,
    scope: crate::ids::ScopeId,
) -> Option<SymbolId> {
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        let scope = unit.scope(scope_id);
        if scope.kind == ScopeKind::Class {
            return scope.owner;
        }
        current = scope.parent;
    }
    None
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
                is_key: false,
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

fn reclassify_project_open_sql_predicate_host_variables(units: &mut [UnitAnalysis]) {
    let mut root_value_symbols: HashMap<Arc<str>, SymbolHandle> = HashMap::new();
    for unit in units.iter() {
        for symbol in unit.symbols.iter().filter(|symbol| {
            symbol.scope == unit.root_scope && symbol.kind.occupies(Namespace::Value)
        }) {
            root_value_symbols
                .entry(Arc::clone(&symbol.name))
                .or_insert(SymbolHandle {
                    unit: unit.unit_id,
                    symbol: symbol.id,
                });
        }
    }

    for unit in units.iter_mut() {
        let predicate_ranges_by_query: HashMap<usize, Vec<std::ops::Range<usize>>> = unit
            .sql_predicates
            .iter()
            .fold(HashMap::new(), |mut ranges, predicate| {
                ranges
                    .entry(predicate.query_id)
                    .or_default()
                    .push(predicate.range.clone());
                ranges
            });
        let mut rewritten_sql_refs = Vec::with_capacity(unit.sql_name_refs.len());

        for sql_ref in unit.sql_name_refs.drain(..) {
            let in_predicate = predicate_ranges_by_query
                .get(&sql_ref.query_id)
                .is_some_and(|ranges| {
                    ranges.iter().any(|range| {
                        range.start <= sql_ref.range.start && sql_ref.range.end <= range.end
                    })
                });
            let promoted_handle = (sql_ref.kind == SqlNameRefKind::Column && in_predicate)
                .then(|| root_value_symbols.get(&sql_ref.name).copied())
                .flatten();

            if let Some(handle) = promoted_handle {
                let already_present = unit.references.iter().any(|reference| {
                    reference.namespace == Namespace::Value
                        && reference.kind == ReferenceKind::Identifier
                        && reference.name == sql_ref.name
                        && reference.range == sql_ref.range
                });
                if !already_present {
                    let id = ReferenceId(unit.references.len() as u32);
                    unit.references.push(ReferenceData {
                        id,
                        name: Arc::clone(&sql_ref.name),
                        namespace: Namespace::Value,
                        kind: ReferenceKind::Identifier,
                        scope: sql_ref.scope,
                        range: sql_ref.range.clone(),
                        resolution: Some(Resolution::Symbol(handle)),
                    });
                }
                continue;
            }

            rewritten_sql_refs.push(sql_ref);
        }

        unit.sql_name_refs = rewritten_sql_refs;
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
    infer_semantic_facts_with_scope_indexes(
        std::slice::from_mut(&mut collected.unit),
        std::slice::from_ref(&scope_index),
    );
    collected.unit.rebuild_semantic_index();
    let exported_signature = exported_signature_for_unit(&collected.unit);
    LocallyResolvedUnit {
        unit: collected.unit,
        scope_index,
        exported_signature,
    }
}

pub(crate) fn resolve_local_phase_for_project(mut collected: CollectedUnit) -> LocallyResolvedUnit {
    let scope_index = build_scope_index(&collected.unit);
    resolve_unit_with_index(&mut collected.unit, &scope_index);
    infer_inline_select_target_shapes(&mut collected.unit, &scope_index);
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

pub(crate) fn analyze_unit_locally_for_project(
    unit_id: UnitId,
    uri: impl Into<Arc<str>>,
    source: &str,
    parse: &ParseResult,
) -> LocallyResolvedUnit {
    resolve_local_phase_for_project(collect_unit_phase(unit_id, uri, source, parse))
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
    let mut provided_name_to_units = HashMap::<Arc<str>, Vec<UnitId>>::new();

    for local in local_units {
        let unit = &local.unit;
        let unit_id = unit.unit_id;
        uri_to_unit.insert(Arc::clone(&unit.uri), unit_id);
        for name in &local.exported_signature.provided_names {
            provided_name_to_unit
                .entry(Arc::clone(name))
                .or_insert(unit_id);
            provided_name_to_units
                .entry(Arc::clone(name))
                .or_default()
                .push(unit_id);
        }
    }

    WorkspaceIndex {
        uri_to_unit,
        provided_name_to_unit,
        provided_name_to_units,
    }
}

pub(crate) fn build_workspace_index_from_units(units: &[UnitAnalysis]) -> WorkspaceIndex {
    let mut uri_to_unit = HashMap::with_capacity(units.len());
    let mut provided_name_to_unit = HashMap::new();
    let mut provided_name_to_units = HashMap::<Arc<str>, Vec<UnitId>>::new();

    for unit in units {
        let unit_id = unit.unit_id;
        uri_to_unit.insert(Arc::clone(&unit.uri), unit_id);
        for name in &unit.provided_names {
            provided_name_to_unit
                .entry(Arc::clone(name))
                .or_insert(unit_id);
            provided_name_to_units
                .entry(Arc::clone(name))
                .or_default()
                .push(unit_id);
        }
    }

    WorkspaceIndex {
        uri_to_unit,
        provided_name_to_unit,
        provided_name_to_units,
    }
}

fn normalized_uri_path_key(uri: &str) -> String {
    uri.trim()
        .replace('\\', "/")
        .trim_end_matches('/')
        .to_ascii_lowercase()
}

fn uri_parent_dir_key(uri: &str) -> Option<String> {
    let normalized = normalized_uri_path_key(uri);
    let slash = normalized.rfind('/')?;
    Some(normalized[..slash].to_string())
}

fn child_dir_key(parent: &str, child: &str) -> String {
    let child = child.trim_matches('/').to_ascii_lowercase();
    if parent.is_empty() {
        child
    } else {
        format!("{}/{}", parent.trim_end_matches('/'), child)
    }
}

fn find_include_candidate_in_dir(
    candidates: &[UnitId],
    unit_dir_keys: &[Option<String>],
    dir_key: &str,
) -> Option<UnitId> {
    candidates.iter().copied().find(|candidate| {
        unit_dir_keys
            .get(candidate.as_usize())
            .and_then(|dir| dir.as_deref())
            .is_some_and(|candidate_dir| candidate_dir == dir_key)
    })
}

fn resolve_include_target(
    source_dir_key: Option<&str>,
    include_name: &Arc<str>,
    workspace_index: &WorkspaceIndex,
    unit_dir_keys: &[Option<String>],
) -> Option<UnitId> {
    let candidates = workspace_index
        .provided_name_to_units
        .get(include_name.as_ref())?;

    if let Some(source_dir_key) = source_dir_key {
        if let Some(target) =
            find_include_candidate_in_dir(candidates, unit_dir_keys, source_dir_key)
        {
            return Some(target);
        }

        let includes_dir_key = child_dir_key(source_dir_key, "includes");
        if let Some(target) =
            find_include_candidate_in_dir(candidates, unit_dir_keys, &includes_dir_key)
        {
            return Some(target);
        }
    }

    workspace_index
        .provided_name_to_unit
        .get(include_name.as_ref())
        .copied()
        .or_else(|| candidates.first().copied())
}

pub(crate) fn resolve_include_edges_for_units(
    units: &mut [UnitAnalysis],
    workspace_index: &WorkspaceIndex,
    dirty_unit_ids: &HashSet<UnitId>,
) {
    let unit_dir_keys: Vec<_> = units
        .iter()
        .map(|unit| uri_parent_dir_key(unit.uri.as_ref()))
        .collect();

    for unit in units {
        if !dirty_unit_ids.contains(&unit.unit_id) {
            continue;
        }
        let source_dir_key = unit_dir_keys
            .get(unit.unit_id.as_usize())
            .and_then(|dir| dir.as_deref());
        unit.diagnostics
            .retain(|diagnostic| !matches!(diagnostic.kind, DiagnosticKind::UnresolvedInclude));
        for edge in &mut unit.include_edges {
            edge.target =
                resolve_include_target(source_dir_key, &edge.name, workspace_index, &unit_dir_keys);
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

pub(crate) fn link_class_member_implementations(units: &mut [UnitAnalysis]) {
    for unit in units.iter_mut() {
        let unit_id = unit.unit_id;
        for member in &mut unit.class_members {
            member.implementation =
                member
                    .implementation_range
                    .clone()
                    .map(|range| ClassMemberImplementationData {
                        unit: unit_id,
                        range,
                    });
        }
    }

    let predecessors = include_predecessor_units_for_units(units);
    let method_implementations: Vec<_> = units
        .iter()
        .flat_map(|unit| {
            unit.symbols
                .iter()
                .filter(|symbol| symbol.kind == SymbolKind::Method)
                .filter_map(|symbol| {
                    let class_symbol = enclosing_class_owner_in_unit(unit, symbol.scope)?;
                    Some((
                        unit.unit_id,
                        Arc::clone(&unit.symbol(class_symbol).name),
                        Arc::clone(&symbol.name),
                        symbol.decl_range.clone(),
                    ))
                })
                .collect::<Vec<_>>()
        })
        .collect();

    for (implementation_unit, class_name, method_name, implementation_range) in
        method_implementations
    {
        let Some(prior_units) = predecessors.get(implementation_unit.as_usize()) else {
            continue;
        };
        for definition_unit in prior_units.iter().rev().copied() {
            let Some(class_symbol) =
                root_type_owner_symbol(&units[definition_unit.as_usize()], &class_name, true)
            else {
                continue;
            };
            let Some(member) = units[definition_unit.as_usize()]
                .class_members
                .iter_mut()
                .find(|member| {
                    member.class_symbol == class_symbol
                        && member.kind == crate::ClassMemberKind::Method
                        && member.name == method_name
                })
            else {
                continue;
            };
            if member.implementation.is_none() {
                member.implementation = Some(ClassMemberImplementationData {
                    unit: implementation_unit,
                    range: implementation_range,
                });
            }
            break;
        }
    }
}

pub(crate) fn collect_project_diagnostics(project: &mut ProjectAnalysis) {
    project.diagnostics.clear();
    let mut seen = HashSet::new();
    for unit in &project.units {
        for diagnostic in &unit.diagnostics {
            if seen.insert((
                diagnostic.kind,
                diagnostic.range.start,
                diagnostic.range.end,
                diagnostic.message.as_str(),
            )) {
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

fn include_closure_for_unit_ids(
    units: &[UnitAnalysis],
    roots: &HashSet<UnitId>,
) -> HashSet<UnitId> {
    let mut out = HashSet::new();
    for &root in roots {
        collect_include_closure_for_unit_id(units, root, &mut out);
    }
    out
}

fn collect_include_closure_for_unit_id(
    units: &[UnitAnalysis],
    unit_id: UnitId,
    out: &mut HashSet<UnitId>,
) {
    if units.get(unit_id.as_usize()).is_none() || !out.insert(unit_id) {
        return;
    }
    for target in units[unit_id.as_usize()]
        .include_edges
        .iter()
        .filter_map(|edge| edge.target)
    {
        collect_include_closure_for_unit_id(units, target, out);
    }
}

fn diagnostic_scope_for_units(
    units: &[UnitAnalysis],
    diagnostic_scope_roots: Option<&HashSet<UnitId>>,
) -> Option<HashSet<UnitId>> {
    diagnostic_scope_roots.map(|roots| include_closure_for_unit_ids(units, roots))
}

fn validation_unit_ids_for_dirty_set(
    dirty_unit_ids: &HashSet<UnitId>,
    diagnostic_scope_unit_ids: Option<&HashSet<UnitId>>,
) -> HashSet<UnitId> {
    match diagnostic_scope_unit_ids {
        Some(scope) => dirty_unit_ids
            .iter()
            .copied()
            .filter(|unit_id| scope.contains(unit_id))
            .collect(),
        None => dirty_unit_ids.clone(),
    }
}

fn clear_diagnostics_outside_scope(
    units: &mut [UnitAnalysis],
    diagnostic_scope_unit_ids: Option<&HashSet<UnitId>>,
) {
    let Some(scope) = diagnostic_scope_unit_ids else {
        return;
    };
    for unit in units {
        if !scope.contains(&unit.unit_id) {
            unit.diagnostics.clear();
        }
    }
}

fn include_component_dirty_set(
    local_units: &[LocallyResolvedUnit],
    workspace_index: &WorkspaceIndex,
    seed_unit_ids: &HashSet<UnitId>,
) -> HashSet<UnitId> {
    let mut adjacency: HashMap<UnitId, HashSet<UnitId>> = HashMap::new();
    let unit_dir_keys: Vec<_> = local_units
        .iter()
        .map(|local| uri_parent_dir_key(local.unit.uri.as_ref()))
        .collect();
    for local in local_units {
        let unit_id = local.unit.unit_id;
        let source_dir_key = unit_dir_keys
            .get(unit_id.as_usize())
            .and_then(|dir| dir.as_deref());
        for edge in &local.unit.include_edges {
            let Some(target) =
                resolve_include_target(source_dir_key, &edge.name, workspace_index, &unit_dir_keys)
            else {
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

fn signature_changed(previous: Option<&ExportedSignature>, current: &LocallyResolvedUnit) -> bool {
    previous
        .map(|previous| previous != &current.exported_signature)
        .unwrap_or(true)
}

fn compute_dirty_set(
    previous_project: Option<&ProjectAnalysis>,
    previous_signatures: Option<&HashMap<Arc<str>, ExportedSignature>>,
    local_units: &[LocallyResolvedUnit],
    workspace_index: &WorkspaceIndex,
    changed_uris: &HashSet<Arc<str>>,
    force_full: bool,
) -> DirtySet {
    if force_full || previous_project.is_none() || previous_signatures.is_none() {
        return dirty_set_for_all_units(local_units);
    }

    let previous_project = previous_project.expect("checked above");
    let previous_signatures = previous_signatures.expect("checked above");
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
        let previous = previous_signatures.get(uri.as_ref());
        if signature_changed(previous, current) {
            if let Some(previous) = previous {
                for (_, name) in &previous.root_exports {
                    changed_export_names.insert(Arc::clone(name));
                }
                for name in &previous.provided_names {
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

    for unit_id in include_component_dirty_set(local_units, workspace_index, &changed_unit_ids) {
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
    previous_signatures: Option<&HashMap<Arc<str>, ExportedSignature>>,
    local_units: Vec<LocallyResolvedUnit>,
    changed_uris: &HashSet<Arc<str>>,
    force_full: bool,
    diagnostic_scope_roots: Option<&HashSet<UnitId>>,
) -> IncrementalProjectAnalysisResult {
    let mut metrics = ProjectUpdateMetrics {
        unit_count: local_units.len(),
        ..ProjectUpdateMetrics::default()
    };
    let build_workspace_index_timer = std::time::Instant::now();
    let workspace_index = build_workspace_index(&local_units);
    metrics.build_workspace_index_micros = build_workspace_index_timer.elapsed().as_micros();

    let scope_index_clone_timer = std::time::Instant::now();
    let scope_indexes: Vec<_> = local_units
        .iter()
        .map(|local| local.scope_index.clone())
        .collect();
    metrics.scope_index_clone_micros = scope_index_clone_timer.elapsed().as_micros();

    let compute_dirty_set_timer = std::time::Instant::now();
    let dirty_set = compute_dirty_set(
        previous_project,
        previous_signatures,
        &local_units,
        &workspace_index,
        changed_uris,
        force_full,
    );
    metrics.compute_dirty_set_micros = compute_dirty_set_timer.elapsed().as_micros();
    metrics.dirty_unit_count = dirty_set.unit_ids.len();

    if force_full
        || previous_project.is_none()
        || previous_project.is_some_and(|previous| previous.units.len() > local_units.len())
    {
        metrics.full_rebuild = true;
        let dirty_set = dirty_set_for_all_units(&local_units);
        metrics.dirty_unit_count = dirty_set.unit_ids.len();
        let (project, full_metrics) =
            analyze_project_from_local_units_profiled_with_diagnostic_scope(
                local_units,
                diagnostic_scope_roots,
            );
        metrics.resolve_include_edges_micros = full_metrics.resolve_include_edges_micros;
        metrics.resolve_cross_unit_micros = full_metrics.resolve_cross_unit_micros;
        metrics.infer_semantic_facts_micros = full_metrics.infer_semantic_facts_micros;
        metrics.rebuild_semantic_index_micros = full_metrics.rebuild_semantic_index_micros;
        metrics.validate_micros = full_metrics.validate_micros;
        metrics.collect_project_diagnostics_micros =
            full_metrics.collect_project_diagnostics_micros;
        metrics.diagnostic_scope_unit_count = full_metrics.diagnostic_scope_unit_count;
        metrics.validation_unit_count = full_metrics.validation_unit_count;
        return IncrementalProjectAnalysisResult {
            project,
            dirty_set,
            metrics,
        };
    }

    let previous_project = previous_project.expect("checked above");
    let clone_previous_units_timer = std::time::Instant::now();
    let mut units = previous_project.units.clone();
    metrics.clone_previous_units_micros = clone_previous_units_timer.elapsed().as_micros();
    let apply_local_updates_timer = std::time::Instant::now();
    for local in &local_units {
        if dirty_set.unit_ids.contains(&local.unit.unit_id) {
            let unit_idx = local.unit.unit_id.as_usize();
            if unit_idx == units.len() {
                units.push(local.unit.clone());
            } else {
                units[unit_idx] = local.unit.clone();
            }
        }
    }
    metrics.apply_local_updates_micros = apply_local_updates_timer.elapsed().as_micros();

    let resolve_include_edges_timer = std::time::Instant::now();
    resolve_include_edges_for_units(&mut units, &workspace_index, &dirty_set.unit_ids);
    metrics.resolve_include_edges_micros = resolve_include_edges_timer.elapsed().as_micros();
    let resolve_cross_unit_timer = std::time::Instant::now();
    resolve_project_cross_unit_for_units(&mut units, &dirty_set.unit_ids);
    link_class_member_implementations(&mut units);
    reclassify_project_open_sql_predicate_host_variables(&mut units);
    metrics.resolve_cross_unit_micros = resolve_cross_unit_timer.elapsed().as_micros();
    let infer_semantic_facts_timer = std::time::Instant::now();
    infer_semantic_facts_with_scope_indexes(&mut units, &scope_indexes);
    metrics.infer_semantic_facts_micros = infer_semantic_facts_timer.elapsed().as_micros();
    let rebuild_semantic_index_timer = std::time::Instant::now();
    for unit_id in &dirty_set.unit_ids {
        units[unit_id.as_usize()].rebuild_semantic_index();
    }
    metrics.rebuild_semantic_index_micros = rebuild_semantic_index_timer.elapsed().as_micros();

    let mut project = ProjectAnalysis {
        units,
        uri_to_unit: workspace_index.uri_to_unit.clone(),
        provided_name_to_unit: workspace_index.provided_name_to_unit,
        diagnostics: Vec::new(),
    };
    let diagnostic_scope_unit_ids =
        diagnostic_scope_for_units(&project.units, diagnostic_scope_roots);
    metrics.diagnostic_scope_unit_count = diagnostic_scope_unit_ids
        .as_ref()
        .map_or(project.units.len(), HashSet::len);
    let validation_unit_ids =
        validation_unit_ids_for_dirty_set(&dirty_set.unit_ids, diagnostic_scope_unit_ids.as_ref());
    metrics.validation_unit_count = validation_unit_ids.len();
    let validate_timer = std::time::Instant::now();
    validate_project_with_scope_indexes_for_units(
        &mut project,
        &scope_indexes,
        &validation_unit_ids,
    );
    metrics.validate_micros = validate_timer.elapsed().as_micros();
    clear_diagnostics_outside_scope(&mut project.units, diagnostic_scope_unit_ids.as_ref());
    let collect_project_diagnostics_timer = std::time::Instant::now();
    collect_project_diagnostics(&mut project);
    metrics.collect_project_diagnostics_micros =
        collect_project_diagnostics_timer.elapsed().as_micros();

    IncrementalProjectAnalysisResult {
        project,
        dirty_set,
        metrics,
    }
}

fn analyze_project_from_local_units(local_units: Vec<LocallyResolvedUnit>) -> ProjectAnalysis {
    analyze_project_from_local_units_profiled(local_units).0
}

fn analyze_project_from_local_units_profiled(
    local_units: Vec<LocallyResolvedUnit>,
) -> (ProjectAnalysis, ProjectUpdateMetrics) {
    analyze_project_from_local_units_profiled_with_diagnostic_scope(local_units, None)
}

fn analyze_project_from_local_units_profiled_with_diagnostic_scope(
    local_units: Vec<LocallyResolvedUnit>,
    diagnostic_scope_roots: Option<&HashSet<UnitId>>,
) -> (ProjectAnalysis, ProjectUpdateMetrics) {
    let workspace_index = build_workspace_index(&local_units);
    let scope_indexes: Vec<_> = local_units
        .iter()
        .map(|local| local.scope_index.clone())
        .collect();
    let mut units: Vec<_> = local_units.into_iter().map(|local| local.unit).collect();
    let dirty_unit_ids: HashSet<_> = units.iter().map(|unit| unit.unit_id).collect();
    let mut metrics = ProjectUpdateMetrics {
        full_rebuild: true,
        unit_count: units.len(),
        dirty_unit_count: dirty_unit_ids.len(),
        ..ProjectUpdateMetrics::default()
    };

    let resolve_include_edges_timer = std::time::Instant::now();
    resolve_include_edges_for_units(&mut units, &workspace_index, &dirty_unit_ids);
    metrics.resolve_include_edges_micros = resolve_include_edges_timer.elapsed().as_micros();
    let resolve_cross_unit_timer = std::time::Instant::now();
    resolve_project_cross_unit(&mut units);
    link_class_member_implementations(&mut units);
    reclassify_project_open_sql_predicate_host_variables(&mut units);
    metrics.resolve_cross_unit_micros = resolve_cross_unit_timer.elapsed().as_micros();
    let infer_semantic_facts_timer = std::time::Instant::now();
    infer_semantic_facts_with_scope_indexes(&mut units, &scope_indexes);
    metrics.infer_semantic_facts_micros = infer_semantic_facts_timer.elapsed().as_micros();
    let rebuild_semantic_index_timer = std::time::Instant::now();
    for unit in &mut units {
        unit.rebuild_semantic_index();
    }
    metrics.rebuild_semantic_index_micros = rebuild_semantic_index_timer.elapsed().as_micros();

    let mut project = ProjectAnalysis {
        units,
        uri_to_unit: workspace_index.uri_to_unit,
        provided_name_to_unit: workspace_index.provided_name_to_unit,
        diagnostics: Vec::new(),
    };
    let diagnostic_scope_unit_ids =
        diagnostic_scope_for_units(&project.units, diagnostic_scope_roots);
    metrics.diagnostic_scope_unit_count = diagnostic_scope_unit_ids
        .as_ref()
        .map_or(project.units.len(), HashSet::len);
    let validation_unit_ids =
        validation_unit_ids_for_dirty_set(&dirty_unit_ids, diagnostic_scope_unit_ids.as_ref());
    metrics.validation_unit_count = validation_unit_ids.len();
    let validate_timer = std::time::Instant::now();
    validate_project_with_scope_indexes_for_units(
        &mut project,
        &scope_indexes,
        &validation_unit_ids,
    );
    metrics.validate_micros = validate_timer.elapsed().as_micros();
    clear_diagnostics_outside_scope(&mut project.units, diagnostic_scope_unit_ids.as_ref());
    let collect_project_diagnostics_timer = std::time::Instant::now();
    collect_project_diagnostics(&mut project);
    metrics.collect_project_diagnostics_micros =
        collect_project_diagnostics_timer.elapsed().as_micros();
    (project, metrics)
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
