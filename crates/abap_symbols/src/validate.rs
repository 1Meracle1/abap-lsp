use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::def_map::{Diagnostic, DiagnosticKind};
use crate::ids::{ScopeId, SymbolId};
use crate::project::ProjectAnalysis;
use crate::scope::Namespace;

fn collect_global_names(project: &ProjectAnalysis) -> HashMap<Arc<str>, HashSet<Namespace>> {
    let mut out: HashMap<Arc<str>, HashSet<Namespace>> = HashMap::new();
    for unit in &project.units {
        for symbol in &unit.symbols {
            if symbol.scope != unit.root_scope {
                continue;
            }
            let entry = out.entry(Arc::clone(&symbol.name)).or_default();
            for &namespace in symbol.kind.namespaces() {
                entry.insert(namespace);
            }
        }
    }
    out
}

fn build_scope_names(unit: &crate::UnitAnalysis) -> HashMap<Arc<str>, HashSet<Namespace>> {
    let mut scope_names: HashMap<Arc<str>, HashSet<Namespace>> = HashMap::new();
    for symbol in &unit.symbols {
        let entry = scope_names.entry(Arc::clone(&symbol.name)).or_default();
        for &namespace in symbol.kind.namespaces() {
            entry.insert(namespace);
        }
    }
    scope_names
}

fn build_scope_index(
    unit: &crate::UnitAnalysis,
) -> Vec<HashMap<(Namespace, Arc<str>), Vec<SymbolId>>> {
    let mut out: Vec<HashMap<(Namespace, Arc<str>), Vec<SymbolId>>> =
        vec![HashMap::new(); unit.scopes.len()];
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

fn resolve_symbol_in_scope_chain(
    unit: &crate::UnitAnalysis,
    scope_index: &[HashMap<(Namespace, Arc<str>), Vec<SymbolId>>],
    scope: ScopeId,
    namespace: Namespace,
    name: &Arc<str>,
) -> Option<SymbolId> {
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        if let Some(symbols) = scope_index[scope_id.as_usize()].get(&(namespace, Arc::clone(name)))
            && let Some(symbol_id) = symbols.last().copied()
        {
            return Some(symbol_id);
        }
        current = unit.scope(scope_id).parent;
    }
    None
}

fn resolve_field_access_base_symbol(
    unit: &crate::UnitAnalysis,
    scope_index: &[HashMap<(Namespace, Arc<str>), Vec<SymbolId>>],
    access: &crate::FieldAccess,
) -> Option<SymbolId> {
    if let Some(symbol_id) = resolve_symbol_in_scope_chain(
        unit,
        scope_index,
        access.scope,
        access.base_namespace,
        &access.base_name,
    ) {
        return Some(symbol_id);
    }

    if access.in_type_position {
        let fallback_namespace = match access.base_namespace {
            Namespace::Type => Namespace::Value,
            Namespace::Value => Namespace::Type,
            Namespace::Routine => return None,
        };
        return resolve_symbol_in_scope_chain(
            unit,
            scope_index,
            access.scope,
            fallback_namespace,
            &access.base_name,
        );
    }

    None
}

pub fn validate_project(project: &mut ProjectAnalysis) {
    let global_names = collect_global_names(project);
    project.diagnostics.clear();

    for unit in &mut project.units {
        let retained: Vec<_> = unit
            .diagnostics
            .iter()
            .filter(|diag| {
                matches!(
                    diag.kind,
                    DiagnosticKind::DuplicateDeclaration
                        | DiagnosticKind::ShadowedSymbol
                        | DiagnosticKind::UnresolvedInclude
                        | DiagnosticKind::IncludeCycle
                )
            })
            .cloned()
            .collect();
        unit.diagnostics = retained;
        let scope_names = build_scope_names(unit);
        let scope_index = build_scope_index(unit);

        for reference in &unit.references {
            if reference.resolution.is_some() {
                continue;
            }

            let local_namespaces = scope_names.get(&reference.name);
            let global_namespaces = global_names.get(&reference.name);
            let has_other_namespace = local_namespaces
                .or(global_namespaces)
                .is_some_and(|namespaces| !namespaces.contains(&reference.namespace));

            let (kind, message) = if has_other_namespace {
                (
                    DiagnosticKind::WrongNamespace,
                    format!(
                        "'{}' exists, but not in the {:?} namespace",
                        reference.name, reference.namespace
                    ),
                )
            } else {
                let subject = match reference.namespace {
                    Namespace::Type => "type",
                    Namespace::Routine => "routine",
                    Namespace::Value => "symbol",
                };
                (
                    DiagnosticKind::UnresolvedReference,
                    format!("unknown {} '{}'", subject, reference.name),
                )
            };

            unit.diagnostics.push(Diagnostic {
                kind,
                range: reference.range.clone(),
                message,
            });
        }

        for access in &unit.field_accesses {
            let Some(base_symbol_id) = resolve_field_access_base_symbol(unit, &scope_index, access)
            else {
                continue;
            };
            let Some(mut structure_id) = unit.symbol(base_symbol_id).structure else {
                continue;
            };
            let subject = if access.in_type_position {
                "type"
            } else {
                "structure"
            };
            let mut qualifier = access.base_name.to_string();
            for (idx, step) in access.field_path.iter().enumerate() {
                let structure = unit.structure(structure_id);
                let Some(field) = structure
                    .fields
                    .iter()
                    .find(|field| field.name.as_ref() == step.name.as_ref())
                else {
                    unit.diagnostics.push(Diagnostic {
                        kind: DiagnosticKind::UnknownField,
                        range: step.range.clone(),
                        message: format!(
                            "unknown field '{}' for {} '{}'",
                            step.name, subject, qualifier
                        ),
                    });
                    break;
                };

                qualifier.push('-');
                qualifier.push_str(field.name.as_ref());

                if idx + 1 == access.field_path.len() {
                    break;
                }

                let Some(next_structure_id) = field.structure else {
                    let next_step = &access.field_path[idx + 1];
                    unit.diagnostics.push(Diagnostic {
                        kind: DiagnosticKind::UnknownField,
                        range: next_step.range.clone(),
                        message: format!(
                            "unknown field '{}' for {} '{}'",
                            next_step.name, subject, qualifier
                        ),
                    });
                    break;
                };
                structure_id = next_structure_id;
            }
        }

        for diagnostic in &unit.diagnostics {
            project.diagnostics.push(diagnostic.clone());
        }
    }

    let mut visiting = HashSet::new();
    let mut visited = HashSet::new();
    for idx in 0..project.units.len() {
        detect_include_cycles(project, idx as u32, &mut visiting, &mut visited);
    }
    project.diagnostics.clear();
    for unit in &project.units {
        for diagnostic in &unit.diagnostics {
            project.diagnostics.push(diagnostic.clone());
        }
    }
}

fn detect_include_cycles(
    project: &mut ProjectAnalysis,
    unit_idx: u32,
    visiting: &mut HashSet<u32>,
    visited: &mut HashSet<u32>,
) {
    if visited.contains(&unit_idx) {
        return;
    }
    if !visiting.insert(unit_idx) {
        if let Some(unit) = project.units.get_mut(unit_idx as usize) {
            unit.diagnostics.push(Diagnostic {
                kind: DiagnosticKind::IncludeCycle,
                range: unit.scope(unit.root_scope).range.clone(),
                message: "include cycle detected".to_string(),
            });
        }
        return;
    }

    let targets: Vec<u32> = project.units[unit_idx as usize]
        .include_edges
        .iter()
        .filter_map(|edge| edge.target)
        .map(|target| target.0)
        .collect();
    for target in targets {
        detect_include_cycles(project, target, visiting, visited);
    }

    visiting.remove(&unit_idx);
    visited.insert(unit_idx);
}
