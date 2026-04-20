use std::sync::Arc;

use crate::def_map::{
    FieldTypeRefData, MethodParameterSection, NamedArgumentSection, TypeFactData,
};
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

pub fn parameter_is_required(section: MethodParameterSection, is_optional: bool) -> bool {
    !is_optional
        && matches!(
            section,
            MethodParameterSection::Importing | MethodParameterSection::Changing
        )
}

pub fn call_section_matches_parameter(
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
    let (expected_unit, expected) = normalize_type_fact(project, expected_unit, expected, 0)?;
    let (actual_unit, actual) = normalize_type_fact(project, actual_unit, actual, 0)?;
    if normalized_internal_table_displays_match(&expected, &actual) {
        return Some(true);
    }
    if normalized_type_facts_match_by_name(&expected, &actual) {
        return Some(true);
    }
    let expected = classify_normalized_type_fact(project, expected_unit, &expected, 0)?;
    let actual = classify_normalized_type_fact(project, actual_unit, &actual, 0)?;
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

fn classify_normalized_type_fact(
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    fact: &TypeFactData,
    depth: usize,
) -> Option<ClassifiedType> {
    if depth >= 8 {
        return None;
    }
    if let Some(line_fact) = fact.table_line.as_deref() {
        let line = classify_normalized_type_fact(project, unit, line_fact, depth + 1).map(Box::new);
        return Some(ClassifiedType::Table(line));
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
            table_line: None,
        };
        let line =
            classify_normalized_type_fact(project, unit, &line_fact, depth + 1).map(Box::new);
        return Some(ClassifiedType::Table(line));
    }

    let Some(declared_type) = fact.declared_type.as_ref() else {
        return Some(if fact.structure.is_some() {
            ClassifiedType::Structure
        } else {
            ClassifiedType::Scalar
        });
    };
    if declared_type.is_ref {
        let target_handle =
            resolve_type_symbol_handle(project, unit, declared_type.base_name.as_ref());
        return Some(ClassifiedType::Ref {
            target_name: Arc::clone(&declared_type.base_name),
            target_handle,
        });
    }
    if declared_type.namespace == Namespace::Type {
        if is_builtin_scalar_name(declared_type.base_name.as_ref()) {
            return Some(ClassifiedType::Scalar);
        }
        if let Some((resolved_unit, resolved_fact)) =
            resolve_named_type_fact(project, unit, declared_type.base_name.as_ref())
        {
            return classify_normalized_type_fact(
                project,
                resolved_unit,
                &resolved_fact,
                depth + 1,
            );
        }
    }
    if fact.structure.is_some() {
        return Some(ClassifiedType::Structure);
    }
    Some(ClassifiedType::Scalar)
}

fn normalized_type_facts_match_by_name(expected: &TypeFactData, actual: &TypeFactData) -> bool {
    let expected_is_table = fact_is_table_shape(expected);
    let actual_is_table = fact_is_table_shape(actual);
    if expected_is_table != actual_is_table {
        return false;
    }

    match (expected.table_line.as_deref(), actual.table_line.as_deref()) {
        (Some(expected_line), Some(actual_line)) => {
            normalized_type_facts_match_by_name(expected_line, actual_line)
        }
        _ => named_type_refs_match(
            expected.declared_type.as_ref(),
            actual.declared_type.as_ref(),
        ),
    }
}

fn named_type_refs_match(
    expected: Option<&FieldTypeRefData>,
    actual: Option<&FieldTypeRefData>,
) -> bool {
    matches!(
        (expected, actual),
        (Some(expected), Some(actual))
            if !expected.is_ref
                && !actual.is_ref
                && expected.base_name == actual.base_name
                && expected.field_path == actual.field_path
                && (expected.namespace == actual.namespace
                    || (expected.field_path.is_empty() && actual.field_path.is_empty()))
    )
}

fn normalized_internal_table_displays_match(
    expected: &TypeFactData,
    actual: &TypeFactData,
) -> bool {
    let Some(expected_display) = expected.type_clause_display.as_deref() else {
        return false;
    };
    let Some(actual_display) = actual.type_clause_display.as_deref() else {
        return false;
    };
    let Some(expected_table) = parse_internal_table_display(expected_display) else {
        return false;
    };
    let Some(actual_table) = parse_internal_table_display(actual_display) else {
        return false;
    };

    expected_table.kind == actual_table.kind
        && expected_table
            .line_display
            .eq_ignore_ascii_case(actual_table.line_display.as_ref())
}

fn fact_is_table_shape(fact: &TypeFactData) -> bool {
    fact.table_line.is_some()
        || fact
            .type_clause_display
            .as_deref()
            .is_some_and(is_internal_table_type_display)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum InternalTableDisplayKind {
    Standard,
    Sorted,
    Hashed,
    Any,
    Index,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct InternalTableDisplay<'a> {
    kind: InternalTableDisplayKind,
    line_display: &'a str,
}

fn parse_internal_table_display(display: &str) -> Option<InternalTableDisplay<'_>> {
    let trimmed = display.trim();
    let upper = trimmed.to_ascii_uppercase();

    for (prefix, kind) in [
        ("STANDARD TABLE OF ", InternalTableDisplayKind::Standard),
        ("TABLE OF ", InternalTableDisplayKind::Standard),
        ("SORTED TABLE OF ", InternalTableDisplayKind::Sorted),
        ("HASHED TABLE OF ", InternalTableDisplayKind::Hashed),
        ("ANY TABLE OF ", InternalTableDisplayKind::Any),
        ("INDEX TABLE OF ", InternalTableDisplayKind::Index),
    ] {
        if upper.starts_with(prefix) {
            let line_display = trimmed[prefix.len()..].trim();
            return Some(InternalTableDisplay {
                kind,
                line_display: trim_internal_table_line_display(line_display),
            });
        }
    }

    None
}

fn trim_internal_table_line_display(display: &str) -> &str {
    let upper = display.to_ascii_uppercase();
    for marker in [" WITH ", " INITIAL "] {
        if let Some(idx) = upper.find(marker) {
            return display[..idx].trim_end();
        }
    }
    display.trim()
}

#[cfg(test)]
mod tests {
    use super::{
        InternalTableDisplayKind, named_type_refs_match, normalized_internal_table_displays_match,
        parse_internal_table_display,
    };
    use crate::def_map::{FieldTypeRefData, TypeFactData};
    use crate::scope::Namespace;
    use std::sync::Arc;

    #[test]
    fn matches_plain_named_type_and_like_references_with_same_base_name() {
        let like_ref = FieldTypeRefData {
            namespace: Namespace::Value,
            is_ref: false,
            base_name: Arc::from("ltap_conf"),
            field_path: Vec::new(),
        };
        let type_ref = FieldTypeRefData {
            namespace: Namespace::Type,
            is_ref: false,
            base_name: Arc::from("ltap_conf"),
            field_path: Vec::new(),
        };

        assert!(named_type_refs_match(Some(&like_ref), Some(&type_ref)));
    }

    #[test]
    fn parses_bare_table_of_as_standard_table_display() {
        let parsed = parse_internal_table_display("TABLE OF tline").expect("table display");
        assert_eq!(parsed.kind, InternalTableDisplayKind::Standard);
        assert_eq!(parsed.line_display, "tline");
    }

    #[test]
    fn strips_table_key_additions_from_internal_table_display() {
        let parsed =
            parse_internal_table_display("TABLE OF tline WITH EMPTY KEY").expect("table display");
        assert_eq!(parsed.line_display, "tline");
    }

    #[test]
    fn matches_standard_and_bare_table_displays_case_insensitively() {
        let expected = TypeFactData {
            structure: None,
            declared_type: None,
            type_clause_display: Some(Arc::from("STANDARD TABLE OF TLINE")),
            table_line: None,
        };
        let actual = TypeFactData {
            structure: None,
            declared_type: None,
            type_clause_display: Some(Arc::from("TABLE OF tline")),
            table_line: None,
        };

        assert!(normalized_internal_table_displays_match(&expected, &actual));
    }
}

fn normalize_type_fact<'a>(
    project: &'a ProjectAnalysis,
    unit: &'a UnitAnalysis,
    fact: &TypeFactData,
    depth: usize,
) -> Option<(&'a UnitAnalysis, TypeFactData)> {
    if depth >= 8 {
        return None;
    }

    let Some(type_ref) = fact.declared_type.as_ref() else {
        return Some((unit, fact.clone()));
    };
    if type_ref.field_path.is_empty() || type_ref.namespace != Namespace::Type || type_ref.is_ref {
        return Some((unit, fact.clone()));
    }

    let (mut current_unit, base_fact) =
        resolve_named_type_fact(project, unit, type_ref.base_name.as_ref())?;
    let mut current_structure = base_fact.structure;
    let mut current_declared_type = base_fact.declared_type;

    for field_name in &type_ref.field_path {
        while current_structure.is_none() {
            let next_type_ref = current_declared_type.as_ref()?;
            if next_type_ref.namespace != Namespace::Type
                || next_type_ref.is_ref
                || !next_type_ref.field_path.is_empty()
            {
                return None;
            }
            let (next_unit, next_fact) =
                resolve_named_type_fact(project, current_unit, next_type_ref.base_name.as_ref())?;
            current_unit = next_unit;
            current_structure = next_fact.structure;
            current_declared_type = next_fact.declared_type;
        }

        let field = current_unit
            .structure(current_structure?)
            .fields
            .iter()
            .find(|field| field.name.as_ref() == field_name.as_ref())?;
        current_structure = field.structure;
        current_declared_type = field.type_ref.clone();
    }

    Some((
        current_unit,
        TypeFactData {
            structure: current_structure,
            declared_type: current_declared_type,
            type_clause_display: None,
            table_line: None,
        },
    ))
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
                table_line: None,
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
