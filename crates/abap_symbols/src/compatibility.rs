use std::sync::Arc;

use crate::def_map::{MethodParameterSection, NamedArgumentSection, TypeFactData};
use crate::ids::SymbolHandle;
use crate::project::ProjectAnalysis;
use crate::{Namespace, SymbolKind, UnitAnalysis};

#[derive(Debug, Clone)]
enum ClassifiedType {
    Scalar,
    Structure,
    Table(Option<Box<ClassifiedType>>),
    Ref {
        target_name: Arc<str>,
        target_handle: Option<SymbolHandle>,
    },
}

pub(crate) fn parameter_is_required(section: MethodParameterSection, is_optional: bool) -> bool {
    !is_optional
        && matches!(
            section,
            MethodParameterSection::Importing | MethodParameterSection::Changing
        )
}

pub(crate) fn call_section_matches_parameter(
    call_section: Option<NamedArgumentSection>,
    parameter_section: MethodParameterSection,
) -> bool {
    match call_section {
        None | Some(NamedArgumentSection::Exporting) => {
            parameter_section == MethodParameterSection::Importing
        }
        Some(NamedArgumentSection::Changing) => {
            parameter_section == MethodParameterSection::Changing
        }
        Some(NamedArgumentSection::Importing) => {
            parameter_section == MethodParameterSection::Exporting
        }
        Some(NamedArgumentSection::Receiving) => {
            parameter_section == MethodParameterSection::Returning
        }
        Some(NamedArgumentSection::Tables | NamedArgumentSection::Exceptions) => false,
    }
}

pub(crate) fn positional_parameter_section(section: MethodParameterSection) -> bool {
    matches!(
        section,
        MethodParameterSection::Importing | MethodParameterSection::Changing
    )
}

pub(crate) fn type_facts_compatible(
    project: &ProjectAnalysis,
    expected_unit: &UnitAnalysis,
    expected: &TypeFactData,
    actual_unit: &UnitAnalysis,
    actual: &TypeFactData,
) -> Option<bool> {
    let expected = classify_type_fact(project, expected_unit, expected, 0)?;
    let actual = classify_type_fact(project, actual_unit, actual, 0)?;
    types_compatible(project, &expected, &actual)
}

fn types_compatible(
    project: &ProjectAnalysis,
    expected: &ClassifiedType,
    actual: &ClassifiedType,
) -> Option<bool> {
    match (expected, actual) {
        (ClassifiedType::Scalar, ClassifiedType::Scalar) => Some(true),
        (ClassifiedType::Structure, ClassifiedType::Structure) => Some(true),
        (ClassifiedType::Table(expected_line), ClassifiedType::Table(actual_line)) => {
            match (expected_line.as_deref(), actual_line.as_deref()) {
                (Some(expected_line), Some(actual_line)) => {
                    types_compatible(project, expected_line, actual_line)
                }
                _ => Some(true),
            }
        }
        (
            ClassifiedType::Ref {
                target_name: expected_name,
                target_handle: expected_handle,
            },
            ClassifiedType::Ref {
                target_name: actual_name,
                target_handle: actual_handle,
            },
        ) => {
            if expected_name == actual_name {
                return Some(true);
            }
            match (expected_handle, actual_handle) {
                (Some(expected_handle), Some(actual_handle)) => Some(
                    symbol_handle_is_same_or_subtype(project, *actual_handle, *expected_handle),
                ),
                _ => None,
            }
        }
        _ => Some(false),
    }
}

fn classify_type_fact(
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    fact: &TypeFactData,
    depth: usize,
) -> Option<ClassifiedType> {
    if depth >= 8 {
        return None;
    }
    if fact
        .type_clause_display
        .as_deref()
        .is_some_and(is_internal_table_type_display)
    {
        let line_fact = TypeFactData {
            structure: fact.structure,
            declared_type: fact.declared_type.clone(),
            type_clause_display: None,
        };
        let line = classify_type_fact(project, unit, &line_fact, depth + 1).map(Box::new);
        return Some(ClassifiedType::Table(line));
    }

    let declared_type = fact.declared_type.as_ref()?;
    if declared_type.is_ref {
        let target_handle =
            resolve_type_symbol_handle(project, unit, declared_type.base_name.as_ref());
        return Some(ClassifiedType::Ref {
            target_name: Arc::clone(&declared_type.base_name),
            target_handle,
        });
    }
    if fact.structure.is_some() {
        return Some(ClassifiedType::Structure);
    }
    if declared_type.namespace == Namespace::Type {
        if is_builtin_scalar_name(declared_type.base_name.as_ref()) {
            return Some(ClassifiedType::Scalar);
        }
        if let Some((resolved_unit, resolved_fact)) =
            resolve_named_type_fact(project, unit, declared_type.base_name.as_ref())
        {
            return classify_type_fact(project, resolved_unit, &resolved_fact, depth + 1);
        }
    }
    Some(ClassifiedType::Scalar)
}

fn resolve_named_type_fact<'a>(
    project: &'a ProjectAnalysis,
    preferred_unit: &'a UnitAnalysis,
    name: &str,
) -> Option<(&'a UnitAnalysis, TypeFactData)> {
    for unit in std::iter::once(preferred_unit).chain(project.units.iter()) {
        let Some(symbol) = unit.symbols.iter().find(|symbol| {
            symbol.scope == unit.root_scope
                && matches!(
                    symbol.kind,
                    SymbolKind::TypeDef
                        | SymbolKind::Class
                        | SymbolKind::Interface
                        | SymbolKind::BuiltinType
                )
                && symbol.name.as_ref() == name
        }) else {
            continue;
        };
        return Some((
            unit,
            TypeFactData {
                structure: symbol.structure,
                declared_type: symbol.declared_type.clone(),
                type_clause_display: symbol.type_clause_display.clone(),
            },
        ));
    }
    None
}

fn resolve_type_symbol_handle(
    project: &ProjectAnalysis,
    preferred_unit: &UnitAnalysis,
    name: &str,
) -> Option<SymbolHandle> {
    resolve_named_type_fact(project, preferred_unit, name).and_then(|(unit, _fact)| {
        unit.symbols
            .iter()
            .find(|symbol| {
                symbol.scope == unit.root_scope
                    && symbol.name.as_ref() == name
                    && matches!(
                        symbol.kind,
                        SymbolKind::Class
                            | SymbolKind::Interface
                            | SymbolKind::TypeDef
                            | SymbolKind::BuiltinType
                    )
            })
            .map(|symbol| SymbolHandle {
                unit: unit.unit_id,
                symbol: symbol.id,
            })
    })
}

fn symbol_handle_is_same_or_subtype(
    project: &ProjectAnalysis,
    actual: SymbolHandle,
    expected: SymbolHandle,
) -> bool {
    if actual == expected {
        return true;
    }
    let mut current = actual;
    for _ in 0..16 {
        if current == expected {
            return true;
        }
        let unit = &project.units[current.unit.as_usize()];
        let Some(inheritance) = unit.class_superclass(current.symbol) else {
            return false;
        };
        let Some(next) =
            resolve_type_symbol_handle(project, unit, inheritance.superclass_name.as_ref())
        else {
            return false;
        };
        current = next;
    }
    false
}

fn is_internal_table_type_display(display: &str) -> bool {
    let upper = display.trim().to_ascii_uppercase();
    upper.contains(" TABLE OF ")
}

fn is_builtin_scalar_name(name: &str) -> bool {
    matches!(
        name,
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
            | "abap_bool"
            | "flag"
            | "xfeld"
    ) || (name.starts_with("char") && name[4..].chars().all(|ch| ch.is_ascii_digit()))
}
