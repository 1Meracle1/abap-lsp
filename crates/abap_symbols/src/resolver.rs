use std::collections::HashMap;
use std::sync::Arc;

use crate::builtins::builtin_routine_spec;
use crate::def_map::{ReferenceKind, Resolution, UnitAnalysis};
use crate::ids::{ScopeId, SymbolHandle, SymbolId};
use crate::scope::{Namespace, ScopeKind};

fn is_builtin_type(name: &str) -> bool {
    let lower = name.trim();
    matches!(
        lower,
        "i" | "int1"
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
            | "any"
    ) || (lower.starts_with("char") && lower[4..].chars().all(|ch| ch.is_ascii_digit()))
}

fn is_builtin_routine(name: &str) -> bool {
    builtin_routine_spec(name.trim()).is_some()
}

pub(crate) type ScopeIndex = Vec<HashMap<(Namespace, Arc<str>), Vec<SymbolId>>>;

pub(crate) fn build_scope_index(unit: &UnitAnalysis) -> ScopeIndex {
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
    let key = (namespace, Arc::clone(name));
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        if let Some(symbols) = scope_index[scope_id.as_usize()].get(&key)
            && let Some(symbol) = symbols.last().copied()
        {
            return Some(symbol);
        }
        current = unit.scope(scope_id).parent;
    }
    None
}

fn lookup_reference_scope_chain(
    unit: &UnitAnalysis,
    scope_index: &ScopeIndex,
    scope: ScopeId,
    namespace: Namespace,
    kind: ReferenceKind,
    name: &Arc<str>,
) -> Option<SymbolId> {
    if let Some(symbol) = lookup_scope_chain(unit, scope_index, scope, namespace, name) {
        return Some(symbol);
    }

    if kind == ReferenceKind::TypeRef && namespace == Namespace::Value {
        return lookup_scope_chain(unit, scope_index, scope, Namespace::Type, name);
    }

    None
}

fn enclosing_class_owner(unit: &UnitAnalysis, scope: ScopeId) -> Option<SymbolId> {
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

fn resolve_super_reference_in_unit(
    unit: &UnitAnalysis,
    scope_index: &ScopeIndex,
    scope: ScopeId,
) -> Option<SymbolId> {
    let class_symbol = enclosing_class_owner(unit, scope)?;
    let superclass = unit.class_superclass(class_symbol)?;
    lookup_scope_chain(
        unit,
        scope_index,
        scope,
        Namespace::Type,
        &superclass.superclass_name,
    )
}

pub(crate) fn resolve_unit_with_index(unit: &mut UnitAnalysis, scope_index: &ScopeIndex) {
    let unit_id = unit.unit_id;
    for idx in 0..unit.references.len() {
        let (scope, namespace, kind, name) = {
            let reference = &unit.references[idx];
            (
                reference.scope,
                reference.namespace,
                reference.kind,
                Arc::clone(&reference.name),
            )
        };
        let resolution =
            match lookup_reference_scope_chain(unit, &scope_index, scope, namespace, kind, &name) {
                Some(symbol) => Some(Resolution::Symbol(SymbolHandle {
                    unit: unit_id,
                    symbol,
                })),
                None if namespace == Namespace::Value && name.as_ref() == "super" => {
                    resolve_super_reference_in_unit(unit, &scope_index, scope).map(|symbol| {
                        Resolution::Symbol(SymbolHandle {
                            unit: unit_id,
                            symbol,
                        })
                    })
                }
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

#[allow(dead_code)]
pub fn resolve_unit(unit: &mut UnitAnalysis) {
    let scope_index = build_scope_index(unit);
    resolve_unit_with_index(unit, &scope_index);
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
        for reference_idx in 0..units[unit_idx].references.len() {
            if units[unit_idx].references[reference_idx]
                .resolution
                .is_some()
            {
                continue;
            }
            let mut resolved = None;
            let reference_scope = units[unit_idx].references[reference_idx].scope;
            let reference_namespace = units[unit_idx].references[reference_idx].namespace;
            let reference_kind = units[unit_idx].references[reference_idx].kind;
            let reference_name = Arc::clone(&units[unit_idx].references[reference_idx].name);

            if reference_namespace == Namespace::Value && reference_name.as_ref() == "super" {
                let class_symbol = enclosing_class_owner(&units[unit_idx], reference_scope);
                let superclass_name = class_symbol.and_then(|class_symbol| {
                    units[unit_idx]
                        .class_superclass(class_symbol)
                        .map(|inheritance| Arc::clone(&inheritance.superclass_name))
                });
                if let Some(superclass_name) = superclass_name {
                    if let Some(symbol_id) = per_unit_root_index[unit_idx]
                        .get(&(Namespace::Type, Arc::clone(&superclass_name)))
                        .copied()
                    {
                        resolved = Some(Resolution::Symbol(SymbolHandle {
                            unit: units[unit_idx].unit_id,
                            symbol: symbol_id,
                        }));
                    }
                    if resolved.is_none()
                        && let Some(handles) = root_index.get(&(Namespace::Type, superclass_name))
                        && let Some(symbol) = handles.first().copied()
                    {
                        resolved = Some(Resolution::Symbol(symbol));
                    }
                }
            }

            let namespaces = if reference_kind == ReferenceKind::TypeRef
                && reference_namespace == Namespace::Value
            {
                [Namespace::Value, Namespace::Type]
            } else {
                [reference_namespace, reference_namespace]
            };
            if resolved.is_none() {
                for namespace in namespaces {
                    for target in &include_targets {
                        if let Some(symbol_id) = per_unit_root_index[target.as_usize()]
                            .get(&(namespace, Arc::clone(&reference_name)))
                            .copied()
                        {
                            resolved = Some(Resolution::Symbol(SymbolHandle {
                                unit: *target,
                                symbol: symbol_id,
                            }));
                            break;
                        }
                    }
                    if resolved.is_some() {
                        break;
                    }
                }
            }
            if resolved.is_none() {
                for namespace in namespaces {
                    if let Some(handles) = root_index.get(&(namespace, Arc::clone(&reference_name)))
                        && let Some(symbol) = handles.first().copied()
                    {
                        resolved = Some(Resolution::Symbol(symbol));
                        break;
                    }
                }
            }
            if resolved.is_none()
                && matches!(reference_namespace, Namespace::Type | Namespace::Routine)
                && symbol_by_unit
                    .iter()
                    .any(|names| names.contains_key(&reference_name))
            {
                resolved = Some(Resolution::External);
            }

            units[unit_idx].references[reference_idx].resolution = resolved;
        }
    }
}
