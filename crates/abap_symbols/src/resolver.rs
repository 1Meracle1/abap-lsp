use std::collections::HashMap;
use std::sync::Arc;

use crate::def_map::{Resolution, UnitAnalysis};
use crate::ids::{ScopeId, SymbolHandle, SymbolId};
use crate::scope::Namespace;

fn is_builtin_type(name: &str) -> bool {
    let lower = name.trim();
    matches!(
        lower,
        "i"
            | "int1"
            | "int2"
            | "int4"
            | "int8"
            | "f"
            | "p"
            | "decfloat16"
            | "decfloat34"
            | "string"
            | "c"
            | "n"
            | "d"
            | "t"
            | "x"
            | "xstring"
            | "data"
    ) || (lower.starts_with("char") && lower[4..].chars().all(|ch| ch.is_ascii_digit()))
}

fn is_builtin_routine(name: &str) -> bool {
    matches!(
        name.trim(),
        "strlen" | "numofchar" | "xstrlen" | "lines" | "charlen" | "dbmaxlen" | "line_exists"
    )
}

type ScopeIndex = Vec<HashMap<(Namespace, Arc<str>), Vec<SymbolId>>>;

fn build_scope_index(unit: &UnitAnalysis) -> ScopeIndex {
    let mut out: ScopeIndex = vec![HashMap::new(); unit.scopes.len()];
    for symbol in &unit.symbols {
        for &namespace in symbol.kind.namespaces() {
            out[symbol.scope.as_usize()]
                .entry((namespace, Arc::clone(&symbol.name)))
                .or_default()
                .push(symbol.id);
        }
    }
    out
}

fn lookup_scope_chain(
    unit: &UnitAnalysis,
    scope_index: &ScopeIndex,
    scope: ScopeId,
    namespace: Namespace,
    name: &Arc<str>,
) -> Option<SymbolId> {
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        if let Some(symbols) = scope_index[scope_id.as_usize()].get(&(namespace, Arc::clone(name)))
            && let Some(symbol) = symbols.last().copied()
        {
            return Some(symbol);
        }
        current = unit.scope(scope_id).parent;
    }
    None
}

pub fn resolve_unit(unit: &mut UnitAnalysis) {
    let scope_index = build_scope_index(unit);
    let unit_id = unit.unit_id;
    for idx in 0..unit.references.len() {
        let (scope, namespace, name) = {
            let reference = &unit.references[idx];
            (reference.scope, reference.namespace, Arc::clone(&reference.name))
        };
        let resolution = match lookup_scope_chain(unit, &scope_index, scope, namespace, &name) {
            Some(symbol) => Some(Resolution::Symbol(SymbolHandle {
                unit: unit_id,
                symbol,
            })),
            None if namespace == Namespace::Type && is_builtin_type(name.as_ref()) => {
                Some(Resolution::BuiltinType)
            }
            None if namespace == Namespace::Routine && is_builtin_routine(name.as_ref()) => {
                Some(Resolution::BuiltinRoutine)
            }
            None => None,
        };
        unit.references[idx].resolution = resolution;
    }
}

pub fn resolve_project_cross_unit(units: &mut [UnitAnalysis]) {
    let mut root_index: HashMap<(Namespace, Arc<str>), Vec<SymbolHandle>> = HashMap::new();
    let mut per_unit_root_index: Vec<HashMap<(Namespace, Arc<str>), SymbolId>> =
        vec![HashMap::new(); units.len()];
    for unit in units.iter() {
        for symbol in &unit.symbols {
            if symbol.scope != unit.root_scope {
                continue;
            }
            for &namespace in symbol.kind.namespaces() {
                per_unit_root_index[unit.unit_id.as_usize()]
                    .entry((namespace, Arc::clone(&symbol.name)))
                    .or_insert(symbol.id);
                root_index
                    .entry((namespace, Arc::clone(&symbol.name)))
                    .or_default()
                    .push(SymbolHandle {
                        unit: unit.unit_id,
                        symbol: symbol.id,
                    });
            }
        }
    }

    let symbol_by_unit: Vec<HashMap<Arc<str>, SymbolId>> = units
        .iter()
        .map(|unit| {
            unit.symbols
                .iter()
                .filter(|symbol| symbol.scope == unit.root_scope)
                .map(|symbol| (Arc::clone(&symbol.name), symbol.id))
                .collect()
        })
        .collect();

    for unit_idx in 0..units.len() {
        let include_targets: Vec<_> = units[unit_idx]
            .include_edges
            .iter()
            .filter_map(|edge| edge.target)
            .collect();
        for reference in &mut units[unit_idx].references {
            if reference.resolution.is_some() {
                continue;
            }
            for target in &include_targets {
                if let Some(symbol_id) = per_unit_root_index[target.as_usize()]
                    .get(&(reference.namespace, Arc::clone(&reference.name)))
                    .copied()
                {
                    reference.resolution = Some(Resolution::Symbol(SymbolHandle {
                        unit: *target,
                        symbol: symbol_id,
                    }));
                    break;
                }
            }
            if reference.resolution.is_some() {
                continue;
            }
            if let Some(handles) = root_index.get(&(reference.namespace, Arc::clone(&reference.name)))
                && let Some(symbol) = handles.first().copied()
            {
                reference.resolution = Some(Resolution::Symbol(symbol));
                continue;
            }
            if matches!(reference.namespace, Namespace::Type | Namespace::Routine)
                && symbol_by_unit
                    .iter()
                    .any(|names| names.contains_key(&reference.name))
            {
                reference.resolution = Some(Resolution::External);
            }
        }
    }
}
