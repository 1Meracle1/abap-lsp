use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::builtins::builtin_routine_spec;
use crate::def_map::{
    Diagnostic, DiagnosticKind, FormParameterData, FormParameterSection, PerformParameterSection,
    Resolution,
};
use crate::ids::{ScopeId, SymbolId};
use crate::project::ProjectAnalysis;
use crate::scope::{Namespace, ScopeKind};
use crate::{ClassMemberKind, SymbolKind, Visibility};

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

fn enclosing_class_owner(unit: &crate::UnitAnalysis, scope: ScopeId) -> Option<SymbolId> {
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

fn class_member_visible_to(
    unit: &crate::UnitAnalysis,
    access: &crate::FieldAccess,
    member: &crate::ClassMemberData,
) -> bool {
    match member.visibility {
        Visibility::Public => true,
        Visibility::Protected | Visibility::Private => {
            enclosing_class_owner(unit, access.scope) == Some(member.class_symbol)
        }
    }
}

fn count_form_section(parameters: &[FormParameterData], section: FormParameterSection) -> usize {
    parameters
        .iter()
        .filter(|current| current.section == section)
        .count()
}

fn count_perform_section(
    parameters: &[PerformParameterSection],
    section: PerformParameterSection,
) -> usize {
    parameters
        .iter()
        .copied()
        .filter(|current| *current == section)
        .count()
}

fn format_perform_signature(using_count: usize, changing_count: usize) -> String {
    let mut parts = Vec::new();
    if using_count > 0 {
        parts.push(format!("USING {using_count}"));
    }
    if changing_count > 0 {
        parts.push(format!("CHANGING {changing_count}"));
    }
    if parts.is_empty() {
        "no parameters".to_string()
    } else {
        parts.join(", ")
    }
}

pub fn validate_project(project: &mut ProjectAnalysis) {
    let global_names = collect_global_names(project);
    let form_signatures: HashMap<(u32, u32), Vec<FormParameterData>> = project
        .units
        .iter()
        .flat_map(|unit| {
            unit.form_routines.iter().map(|routine| {
                (
                    (unit.unit_id.0, routine.symbol.0),
                    routine.parameters.clone(),
                )
            })
        })
        .collect();
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
            let base_symbol = unit.symbol(base_symbol_id);
            if access.base_namespace == Namespace::Type && base_symbol.kind == SymbolKind::Class {
                for (idx, step) in access.field_path.iter().enumerate() {
                    let Some(member) = unit.class_member(base_symbol_id, step.name.as_ref()) else {
                        unit.diagnostics.push(Diagnostic {
                            kind: DiagnosticKind::UnknownField,
                            range: step.range.clone(),
                            message: format!(
                                "unknown static member '{}' for class '{}'",
                                step.name, access.base_name
                            ),
                        });
                        break;
                    };
                    if !member.is_static
                        || member.kind != ClassMemberKind::Method
                        || !class_member_visible_to(unit, access, member)
                    {
                        unit.diagnostics.push(Diagnostic {
                            kind: DiagnosticKind::UnknownField,
                            range: step.range.clone(),
                            message: format!(
                                "unknown static member '{}' for class '{}'",
                                step.name, access.base_name
                            ),
                        });
                        break;
                    }
                    if idx + 1 != access.field_path.len() {
                        let next_step = &access.field_path[idx + 1];
                        unit.diagnostics.push(Diagnostic {
                            kind: DiagnosticKind::UnknownField,
                            range: next_step.range.clone(),
                            message: format!(
                                "unknown static member '{}' for class '{}=>{}'",
                                next_step.name, access.base_name, member.name
                            ),
                        });
                        break;
                    }
                }
                continue;
            }
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

        for named_argument in &unit.named_arguments {
            let crate::NamedArgumentTarget::Routine { routine_name } = &named_argument.target
            else {
                continue;
            };
            if builtin_routine_spec(routine_name.as_ref()).is_some() {
                unit.diagnostics.push(Diagnostic {
                    kind: DiagnosticKind::InvalidBuiltinNamedArgument,
                    range: named_argument.range.clone(),
                    message: format!(
                        "built-in function '{}' does not support named parameter passing",
                        routine_name
                    ),
                });
            }
        }

        for perform_call in &unit.perform_calls {
            if perform_call.section_order_invalid {
                unit.diagnostics.push(Diagnostic {
                    kind: DiagnosticKind::InvalidPerformCall,
                    range: perform_call.range.clone(),
                    message: format!(
                        "PERFORM '{}' uses invalid TABLES/USING/CHANGING section order",
                        perform_call.routine_name
                    ),
                });
                continue;
            }

            let Some(reference) = unit.references.iter().find(|reference| {
                reference.kind == crate::ReferenceKind::RoutineCall
                    && reference.namespace == Namespace::Routine
                    && reference.range == perform_call.routine_range
                    && reference.name.as_ref() == perform_call.routine_name.as_ref()
            }) else {
                continue;
            };
            let Some(Resolution::Symbol(handle)) = reference.resolution else {
                continue;
            };
            let Some(parameters) = form_signatures.get(&(handle.unit.0, handle.symbol.0)) else {
                continue;
            };

            let expected_using = count_form_section(parameters, FormParameterSection::Using);
            let expected_changing = count_form_section(parameters, FormParameterSection::Changing);
            let actual_using =
                count_perform_section(&perform_call.parameters, PerformParameterSection::Using);
            let actual_changing =
                count_perform_section(&perform_call.parameters, PerformParameterSection::Changing);

            if expected_using == actual_using && expected_changing == actual_changing {
                continue;
            }

            unit.diagnostics.push(Diagnostic {
                kind: DiagnosticKind::InvalidPerformCall,
                range: perform_call.range.clone(),
                message: format!(
                    "PERFORM '{}' expects {}, but call provides USING {} and CHANGING {} argument(s)",
                    perform_call.routine_name,
                    format_perform_signature(expected_using, expected_changing),
                    actual_using,
                    actual_changing
                ),
            });
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
