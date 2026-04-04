use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::builtins::builtin_routine_spec;
use crate::def_map::{
    Diagnostic, DiagnosticKind, FormParameterData, FormParameterSection, PerformParameterSection,
    Resolution,
};
use crate::ids::{ScopeId, SymbolHandle, SymbolId};
use crate::project::ProjectAnalysis;
use crate::resolver::{ScopeIndex, build_scope_index};
use crate::scope::{Namespace, ScopeKind};
use crate::{SymbolKind, Visibility};

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

fn resolve_symbol_in_scope_chain(
    unit: &crate::UnitAnalysis,
    scope_index: &ScopeIndex,
    scope: ScopeId,
    namespace: Namespace,
    name: &Arc<str>,
) -> Option<SymbolId> {
    let key = (namespace, Arc::clone(name));
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        if let Some(symbols) = scope_index[scope_id.as_usize()].get(&key)
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
    scope_index: &ScopeIndex,
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

fn enclosing_method_owner(unit: &crate::UnitAnalysis, scope: ScopeId) -> Option<SymbolId> {
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        let scope = unit.scope(scope_id);
        if scope.kind == ScopeKind::Method {
            return scope.owner;
        }
        current = scope.parent;
    }
    None
}

fn scope_descends_from(unit: &crate::UnitAnalysis, scope: ScopeId, ancestor: ScopeId) -> bool {
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        if scope_id == ancestor {
            return true;
        }
        current = unit.scope(scope_id).parent;
    }
    false
}

fn resolve_class_symbol(
    project: &ProjectAnalysis,
    unit: &crate::UnitAnalysis,
    scope_index: &ScopeIndex,
    scope: ScopeId,
    name: &Arc<str>,
) -> Option<SymbolHandle> {
    if let Some(symbol) =
        resolve_symbol_in_scope_chain(unit, scope_index, scope, Namespace::Type, name)
        && unit.symbol(symbol).kind == SymbolKind::Class
    {
        return Some(SymbolHandle {
            unit: unit.unit_id,
            symbol,
        });
    }

    for candidate_unit in &project.units {
        for symbol in &candidate_unit.symbols {
            if symbol.scope == candidate_unit.root_scope
                && symbol.kind == SymbolKind::Class
                && symbol.name.as_ref() == name.as_ref()
            {
                return Some(SymbolHandle {
                    unit: candidate_unit.unit_id,
                    symbol: symbol.id,
                });
            }
        }
    }

    None
}

fn is_valid_super_reference(unit: &crate::UnitAnalysis, scope: ScopeId) -> bool {
    let Some(_) = enclosing_method_owner(unit, scope) else {
        return false;
    };
    let Some(class_symbol) = enclosing_class_owner(unit, scope) else {
        return false;
    };
    unit.class_superclass(class_symbol).is_some()
}

fn validate_super_constructor_calls(
    project: &ProjectAnalysis,
    unit: &crate::UnitAnalysis,
    scope_index: &ScopeIndex,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    for scope in &unit.scopes {
        if scope.kind != ScopeKind::Method {
            continue;
        }
        let Some(method_symbol) = scope.owner else {
            continue;
        };
        let method = unit.symbol(method_symbol);
        if method.name.as_ref() != "constructor" {
            continue;
        }
        let Some(class_symbol) = enclosing_class_owner(unit, scope.id) else {
            continue;
        };
        let Some(inheritance) = unit.class_superclass(class_symbol) else {
            continue;
        };

        let superclass = resolve_class_symbol(
            project,
            unit,
            scope_index,
            scope.id,
            &inheritance.superclass_name,
        );

        let has_super_call = unit.field_accesses.iter().any(|access| {
            scope_descends_from(unit, access.scope, scope.id)
                && access.base_namespace == Namespace::Value
                && access.base_name.as_ref() == "super"
                && access
                    .field_path
                    .last()
                    .is_some_and(|segment| segment.name.as_ref() == "constructor")
        });
        if !has_super_call {
            diagnostics.push(Diagnostic {
                kind: DiagnosticKind::MissingSuperConstructorCall,
                range: method.decl_range.clone(),
                message: format!(
                    "constructor of subclass '{}' must call super->constructor( )",
                    unit.symbol(class_symbol).name
                ),
            });
            continue;
        }

        let Some(superclass) = superclass else {
            continue;
        };
        let superclass_unit = &project.units[superclass.unit.as_usize()];
        let Some(super_constructor) =
            superclass_unit.class_member(superclass.symbol, "constructor")
        else {
            continue;
        };
        if super_constructor.parameters.is_empty() {
            continue;
        }

        let provided_args: HashSet<&str> = unit
            .named_arguments
            .iter()
            .filter(|argument| scope_descends_from(unit, argument.scope, scope.id))
            .filter_map(|argument| match &argument.target {
                crate::NamedArgumentTarget::Method {
                    base_namespace,
                    base_name,
                    method_name,
                } if *base_namespace == Namespace::Value
                    && base_name.as_ref() == "super"
                    && method_name.as_ref() == "constructor" =>
                {
                    Some(argument.name.as_ref())
                }
                _ => None,
            })
            .collect();

        let missing: Vec<_> = super_constructor
            .parameters
            .iter()
            .filter(|parameter| !provided_args.contains(parameter.name.as_ref()))
            .map(|parameter| parameter.name.to_string())
            .collect();
        if missing.is_empty() {
            continue;
        }

        diagnostics.push(Diagnostic {
            kind: DiagnosticKind::MissingSuperConstructorCall,
            range: method.decl_range.clone(),
            message: format!(
                "super->constructor( ) in subclass '{}' must pass parent constructor argument(s): {}",
                unit.symbol(class_symbol).name,
                missing.join(", ")
            ),
        });
    }

    diagnostics
}

fn resolve_project_class_symbol<'a>(
    project: &'a ProjectAnalysis,
    preferred_unit: &'a crate::UnitAnalysis,
    name: &Arc<str>,
) -> Option<SymbolHandle> {
    preferred_unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.scope == preferred_unit.root_scope
                && symbol.kind == SymbolKind::Class
                && symbol.name == *name
        })
        .map(|symbol| SymbolHandle {
            unit: preferred_unit.unit_id,
            symbol: symbol.id,
        })
        .or_else(|| {
            project.units.iter().find_map(|candidate_unit| {
                candidate_unit
                    .symbols
                    .iter()
                    .find(|symbol| {
                        symbol.scope == candidate_unit.root_scope
                            && symbol.kind == SymbolKind::Class
                            && symbol.name == *name
                    })
                    .map(|symbol| SymbolHandle {
                        unit: candidate_unit.unit_id,
                        symbol: symbol.id,
                    })
            })
        })
}

fn direct_superclass_handle(
    project: &ProjectAnalysis,
    unit: &crate::UnitAnalysis,
    class_symbol: SymbolId,
) -> Option<SymbolHandle> {
    let inheritance = unit.class_superclass(class_symbol)?;
    resolve_project_class_symbol(project, unit, &inheritance.superclass_name)
}

fn class_is_or_inherits_from(
    project: &ProjectAnalysis,
    descendant: SymbolHandle,
    ancestor: SymbolHandle,
) -> bool {
    let mut current = descendant;
    let mut visited = HashSet::new();
    loop {
        if !visited.insert(current) {
            return false;
        }
        if current == ancestor {
            return true;
        }
        let unit = &project.units[current.unit.as_usize()];
        let Some(next) = direct_superclass_handle(project, unit, current.symbol) else {
            return false;
        };
        current = next;
    }
}

fn class_member_visible_to(
    project: &ProjectAnalysis,
    caller_unit: &crate::UnitAnalysis,
    caller_scope: ScopeId,
    target_unit: &crate::UnitAnalysis,
    member: &crate::ClassMemberData,
) -> bool {
    match member.visibility {
        Visibility::Public => true,
        Visibility::Private => {
            caller_unit.unit_id == target_unit.unit_id
                && enclosing_class_owner(caller_unit, caller_scope) == Some(member.class_symbol)
        }
        Visibility::Protected => {
            let Some(caller_class_symbol) = enclosing_class_owner(caller_unit, caller_scope) else {
                return false;
            };
            class_is_or_inherits_from(
                project,
                SymbolHandle {
                    unit: caller_unit.unit_id,
                    symbol: caller_class_symbol,
                },
                SymbolHandle {
                    unit: target_unit.unit_id,
                    symbol: member.class_symbol,
                },
            )
        }
    }
}

fn resolve_class_member_in_hierarchy<'a>(
    project: &'a ProjectAnalysis,
    class_unit: &'a crate::UnitAnalysis,
    class_symbol: SymbolId,
    member_name: &str,
) -> Option<(&'a crate::UnitAnalysis, &'a crate::ClassMemberData)> {
    let mut current = SymbolHandle {
        unit: class_unit.unit_id,
        symbol: class_symbol,
    };
    let mut visited = HashSet::new();
    loop {
        if !visited.insert(current) {
            return None;
        }
        let unit = &project.units[current.unit.as_usize()];
        if let Some(member) = unit.class_member(current.symbol, member_name) {
            return Some((unit, member));
        }
        current = direct_superclass_handle(project, unit, current.symbol)?;
    }
}

fn resolve_class_selector_base<'a>(
    project: &'a ProjectAnalysis,
    unit: &crate::UnitAnalysis,
    scope_index: &ScopeIndex,
    access: &crate::FieldAccess,
    base_symbol_id: SymbolId,
) -> Option<(&'a crate::UnitAnalysis, SymbolId, bool)> {
    let base_symbol = unit.symbol(base_symbol_id);
    if access.base_namespace == Namespace::Type && base_symbol.kind == SymbolKind::Class {
        let class_unit = &project.units[unit.unit_id.as_usize()];
        return Some((class_unit, base_symbol_id, true));
    }
    if access.base_namespace != Namespace::Value {
        return None;
    }
    let declared_type = base_symbol.declared_type.as_ref()?;
    if !declared_type.is_ref || !declared_type.field_path.is_empty() {
        return None;
    }
    let class_handle = resolve_class_symbol(
        project,
        unit,
        scope_index,
        access.scope,
        &declared_type.base_name,
    )?;
    let class_unit = &project.units[class_handle.unit.as_usize()];
    Some((class_unit, class_handle.symbol, false))
}

fn split_leading_deref<'a>(
    access: &'a crate::FieldAccess,
) -> (bool, &'a [crate::FieldAccessSegment]) {
    if access
        .field_path
        .first()
        .is_some_and(|segment| segment.is_deref())
    {
        return (true, &access.field_path[1..]);
    }
    (false, &access.field_path)
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

#[allow(dead_code)]
pub fn validate_project(project: &mut ProjectAnalysis) {
    let scope_indexes: Vec<_> = project.units.iter().map(build_scope_index).collect();
    validate_project_with_scope_indexes(project, &scope_indexes);
}

pub(crate) fn validate_project_with_scope_indexes(
    project: &mut ProjectAnalysis,
    scope_indexes: &[ScopeIndex],
) {
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

    for unit_idx in 0..project.units.len() {
        let scope_names = build_scope_names(&project.units[unit_idx]);
        let scope_index = &scope_indexes[unit_idx];
        let constructor_diagnostics =
            validate_super_constructor_calls(project, &project.units[unit_idx], &scope_index);
        let field_access_bases: Vec<_> = project.units[unit_idx]
            .field_accesses
            .iter()
            .map(|access| {
                let unit = &project.units[unit_idx];
                let base_symbol_id = resolve_field_access_base_symbol(unit, &scope_index, access)?;
                Some((
                    base_symbol_id,
                    resolve_class_selector_base(project, unit, &scope_index, access, base_symbol_id)
                        .map(|(class_unit, class_symbol_id, requires_static)| {
                            (
                                class_unit.unit_id.as_usize(),
                                class_symbol_id,
                                requires_static,
                            )
                        }),
                ))
            })
            .collect();

        let unit = &project.units[unit_idx];
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
        let mut unit_diagnostics = retained;

        for reference in &unit.references {
            if reference.resolution.is_some() {
                continue;
            }
            if reference.namespace == Namespace::Value
                && reference.name.as_ref() == "super"
                && is_valid_super_reference(unit, reference.scope)
            {
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

            unit_diagnostics.push(Diagnostic {
                kind,
                range: reference.range.clone(),
                message,
            });
        }

        for (access, base_info) in unit.field_accesses.iter().zip(&field_access_bases) {
            let Some((base_symbol_id, class_selector_base)) = *base_info else {
                continue;
            };
            let base_symbol = unit.symbol(base_symbol_id);
            let (has_leading_deref, field_path) = split_leading_deref(access);
            if access.base_namespace == Namespace::Type && base_symbol.kind == SymbolKind::Class {
                for (idx, step) in field_path.iter().enumerate() {
                    let Some((member_unit, member)) = resolve_class_member_in_hierarchy(
                        project,
                        unit,
                        base_symbol_id,
                        step.name.as_ref(),
                    ) else {
                        unit_diagnostics.push(Diagnostic {
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
                        || !class_member_visible_to(
                            project,
                            unit,
                            access.scope,
                            member_unit,
                            member,
                        )
                    {
                        unit_diagnostics.push(Diagnostic {
                            kind: DiagnosticKind::UnknownField,
                            range: step.range.clone(),
                            message: format!(
                                "unknown static member '{}' for class '{}'",
                                step.name, access.base_name
                            ),
                        });
                        break;
                    }
                    if idx + 1 != field_path.len() {
                        let next_step = &field_path[idx + 1];
                        unit_diagnostics.push(Diagnostic {
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
            if !has_leading_deref
                && let Some((class_unit_idx, class_symbol_id, requires_static)) =
                    class_selector_base
            {
                let class_unit = &project.units[class_unit_idx];
                let class_name = Arc::clone(&class_unit.symbol(class_symbol_id).name);
                for (idx, step) in field_path.iter().enumerate() {
                    let Some((member_unit, member)) = resolve_class_member_in_hierarchy(
                        project,
                        class_unit,
                        class_symbol_id,
                        step.name.as_ref(),
                    ) else {
                        unit_diagnostics.push(Diagnostic {
                            kind: DiagnosticKind::UnknownField,
                            range: step.range.clone(),
                            message: format!(
                                "unknown member '{}' for class '{}'",
                                step.name, class_name
                            ),
                        });
                        break;
                    };
                    if (requires_static && !member.is_static)
                        || !class_member_visible_to(
                            project,
                            unit,
                            access.scope,
                            member_unit,
                            member,
                        )
                    {
                        let qualifier = if requires_static {
                            "static member"
                        } else {
                            "member"
                        };
                        unit_diagnostics.push(Diagnostic {
                            kind: DiagnosticKind::UnknownField,
                            range: step.range.clone(),
                            message: format!(
                                "unknown {} '{}' for class '{}'",
                                qualifier, step.name, class_name
                            ),
                        });
                        break;
                    }
                    if idx + 1 != field_path.len() {
                        let next_step = &field_path[idx + 1];
                        unit_diagnostics.push(Diagnostic {
                            kind: DiagnosticKind::UnknownField,
                            range: next_step.range.clone(),
                            message: format!(
                                "unknown member '{}' for class '{}->{}'",
                                next_step.name, class_name, member.name
                            ),
                        });
                        break;
                    }
                }
                continue;
            }
            if has_leading_deref && field_path.is_empty() {
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
            if has_leading_deref {
                qualifier.push_str("->*");
            }
            for (idx, step) in field_path.iter().enumerate() {
                let structure = unit.structure(structure_id);
                let Some(field) = structure
                    .fields
                    .iter()
                    .find(|field| field.name.as_ref() == step.name.as_ref())
                else {
                    unit_diagnostics.push(Diagnostic {
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

                if idx + 1 == field_path.len() {
                    break;
                }

                let Some(next_structure_id) = field.structure else {
                    let next_step = &field_path[idx + 1];
                    unit_diagnostics.push(Diagnostic {
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
                unit_diagnostics.push(Diagnostic {
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
                unit_diagnostics.push(Diagnostic {
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

            unit_diagnostics.push(Diagnostic {
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

        unit_diagnostics.extend(constructor_diagnostics);

        {
            let unit = &mut project.units[unit_idx];
            unit.diagnostics = unit_diagnostics;
        }

        for diagnostic in &project.units[unit_idx].diagnostics {
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
