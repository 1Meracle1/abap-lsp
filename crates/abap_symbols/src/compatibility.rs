use std::{collections::HashSet, sync::Arc};

use crate::def_map::{
    FieldTypeRefData, MethodParameterSection, NamedArgumentSection, TypeFactData,
};
use crate::ids::SymbolHandle;
use crate::project::ProjectAnalysis;
use crate::{Namespace, SymbolKind, UnitAnalysis};

#[derive(Debug, Clone)]
enum ClassifiedType {
    Scalar(ScalarCompatibilityKind),
    Structure,
    Table {
        kind: InternalTableDisplayKind,
        line: Option<Box<ClassifiedType>>,
    },
    Ref {
        target_name: Arc<str>,
        target_handle: Option<SymbolHandle>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ScalarCompatibilityKind {
    Any,
    Simple,
    Numeric,
    DecFloat,
    CharacterLike,
    TextLike,
    ByteLike,
    NumericConcrete,
    DecFloatConcrete,
    TextConcrete,
    CharacterConcrete,
    ByteConcrete,
    Date,
    Time,
    Elementary,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TypeCompatibility {
    Compatible,
    Convertible,
    Incompatible,
    Unknown,
}

impl TypeCompatibility {
    pub(crate) fn is_incompatible(self) -> bool {
        self == Self::Incompatible
    }
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

pub(crate) fn type_facts_compatibility(
    project: &ProjectAnalysis,
    expected_unit: &UnitAnalysis,
    expected: &TypeFactData,
    actual_unit: &UnitAnalysis,
    actual: &TypeFactData,
) -> TypeCompatibility {
    type_facts_compatibility_inner(
        project,
        expected_unit,
        expected,
        actual_unit,
        actual,
        true,
        true,
    )
}

pub(crate) fn type_facts_parameter_compatibility(
    project: &ProjectAnalysis,
    expected_unit: &UnitAnalysis,
    expected: &TypeFactData,
    actual_unit: &UnitAnalysis,
    actual: &TypeFactData,
) -> TypeCompatibility {
    type_facts_compatibility_inner(
        project,
        expected_unit,
        expected,
        actual_unit,
        actual,
        false,
        false,
    )
}

pub(crate) fn type_facts_strict_table_kind_compatibility(
    project: &ProjectAnalysis,
    expected_unit: &UnitAnalysis,
    expected: &TypeFactData,
    actual_unit: &UnitAnalysis,
    actual: &TypeFactData,
) -> TypeCompatibility {
    type_facts_compatibility_inner(
        project,
        expected_unit,
        expected,
        actual_unit,
        actual,
        false,
        true,
    )
}

fn type_facts_compatibility_inner(
    project: &ProjectAnalysis,
    expected_unit: &UnitAnalysis,
    expected: &TypeFactData,
    actual_unit: &UnitAnalysis,
    actual: &TypeFactData,
    allow_table_kind_conversion: bool,
    allow_scalar_conversion: bool,
) -> TypeCompatibility {
    let Some((expected_unit, expected)) = normalize_type_fact(project, expected_unit, expected, 0)
    else {
        return TypeCompatibility::Unknown;
    };
    let Some((actual_unit, actual)) = normalize_type_fact(project, actual_unit, actual, 0) else {
        return TypeCompatibility::Unknown;
    };
    if !expected.is_known() || !actual.is_known() {
        return TypeCompatibility::Unknown;
    }
    if normalized_internal_table_displays_match(&expected, &actual) {
        return TypeCompatibility::Compatible;
    }
    if resolved_internal_table_displays_match(
        project,
        expected_unit,
        &expected,
        actual_unit,
        &actual,
    ) {
        return TypeCompatibility::Compatible;
    }
    if normalized_type_facts_match_by_name(&expected, &actual)
        && !internal_table_kinds_differ(project, expected_unit, &expected, actual_unit, &actual)
    {
        return TypeCompatibility::Compatible;
    }
    let Some(expected) = classify_normalized_type_fact(project, expected_unit, &expected, 0) else {
        return TypeCompatibility::Unknown;
    };
    let Some(actual) = classify_normalized_type_fact(project, actual_unit, &actual, 0) else {
        return TypeCompatibility::Unknown;
    };
    types_compatibility(
        project,
        &expected,
        &actual,
        allow_table_kind_conversion,
        allow_scalar_conversion,
    )
}

fn types_compatibility(
    project: &ProjectAnalysis,
    expected: &ClassifiedType,
    actual: &ClassifiedType,
    allow_table_kind_conversion: bool,
    allow_scalar_conversion: bool,
) -> TypeCompatibility {
    match (expected, actual) {
        (ClassifiedType::Scalar(expected), ClassifiedType::Scalar(actual)) => {
            let compatibility = scalar_kinds_compatibility(*expected, *actual);
            if compatibility == TypeCompatibility::Convertible && !allow_scalar_conversion {
                TypeCompatibility::Incompatible
            } else {
                compatibility
            }
        }
        (ClassifiedType::Scalar(ScalarCompatibilityKind::Any), _) => TypeCompatibility::Compatible,
        (_, ClassifiedType::Scalar(ScalarCompatibilityKind::Any)) => TypeCompatibility::Unknown,
        (ClassifiedType::Structure, ClassifiedType::Structure) => TypeCompatibility::Compatible,
        (
            ClassifiedType::Table {
                kind: expected_kind,
                line: expected_line,
            },
            ClassifiedType::Table {
                kind: actual_kind,
                line: actual_line,
            },
        ) => {
            let table = table_kinds_compatibility(
                *expected_kind,
                *actual_kind,
                allow_table_kind_conversion,
            );
            if table.is_incompatible() {
                return table;
            }
            let line = match (expected_line.as_deref(), actual_line.as_deref()) {
                (Some(expected_line), Some(actual_line)) => types_compatibility(
                    project,
                    expected_line,
                    actual_line,
                    allow_table_kind_conversion,
                    allow_scalar_conversion,
                ),
                _ => TypeCompatibility::Compatible,
            };
            combine_compatibility(table, line)
        }
        (ClassifiedType::Scalar(expected), ClassifiedType::Structure)
            if matches!(
                expected,
                ScalarCompatibilityKind::Any
                    | ScalarCompatibilityKind::Simple
                    | ScalarCompatibilityKind::CharacterLike
            ) =>
        {
            TypeCompatibility::Unknown
        }
        (ClassifiedType::Scalar(expected), _)
            if scalar_kind_is_generic(*expected)
                && !matches!(expected, ScalarCompatibilityKind::Any) =>
        {
            TypeCompatibility::Incompatible
        }
        (_, ClassifiedType::Scalar(actual)) if scalar_kind_is_generic(*actual) => {
            TypeCompatibility::Unknown
        }
        (ClassifiedType::Structure, ClassifiedType::Scalar(_)) => TypeCompatibility::Incompatible,
        (ClassifiedType::Scalar(_), ClassifiedType::Structure) => TypeCompatibility::Incompatible,
        (ClassifiedType::Table { .. }, _) | (_, ClassifiedType::Table { .. }) => {
            TypeCompatibility::Incompatible
        }
        (ClassifiedType::Ref { .. }, ClassifiedType::Scalar(_))
        | (ClassifiedType::Scalar(_), ClassifiedType::Ref { .. })
        | (ClassifiedType::Structure, ClassifiedType::Ref { .. })
        | (ClassifiedType::Ref { .. }, ClassifiedType::Structure) => {
            TypeCompatibility::Incompatible
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
                return TypeCompatibility::Compatible;
            }
            if expected_name.as_ref() == "data" {
                return match ref_is_object(project, actual_name.as_ref(), *actual_handle) {
                    Some(false) => TypeCompatibility::Compatible,
                    Some(true) => TypeCompatibility::Incompatible,
                    None => TypeCompatibility::Unknown,
                };
            }
            if expected_name.as_ref() == "object" {
                return match ref_is_object(project, actual_name.as_ref(), *actual_handle) {
                    Some(true) => TypeCompatibility::Compatible,
                    Some(false) => TypeCompatibility::Incompatible,
                    None => TypeCompatibility::Unknown,
                };
            }
            if actual_name.as_ref() == "data" || actual_name.as_ref() == "object" {
                return TypeCompatibility::Incompatible;
            }
            match (expected_handle, actual_handle) {
                (Some(expected_handle), Some(actual_handle)) => {
                    if symbol_handle_is_same_or_subtype(project, *actual_handle, *expected_handle) {
                        TypeCompatibility::Compatible
                    } else {
                        TypeCompatibility::Incompatible
                    }
                }
                _ => TypeCompatibility::Unknown,
            }
        }
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
    let table_display = fact
        .type_clause_display
        .as_deref()
        .and_then(parse_internal_table_display);
    if fact.table_line.is_some() || table_display.is_some() {
        let line = if let Some(line_fact) = fact.table_line.as_deref() {
            classify_normalized_type_fact(project, unit, line_fact, depth + 1).map(Box::new)
        } else if table_display
            .as_ref()
            .is_some_and(|display| display.line_display.is_some())
        {
            let line_fact = TypeFactData {
                structure: fact.structure,
                declared_type: fact.declared_type.clone(),
                type_clause_display: None,
                table_line: None,
            };
            classify_normalized_type_fact(project, unit, &line_fact, depth + 1).map(Box::new)
        } else {
            None
        };
        return Some(ClassifiedType::Table {
            kind: table_display
                .as_ref()
                .map(|display| display.kind)
                .or_else(|| internal_table_fact_kind(project, unit, fact, 0))
                .unwrap_or(InternalTableDisplayKind::Any),
            line,
        });
    }

    let Some(declared_type) = fact.declared_type.as_ref() else {
        return Some(if fact.structure.is_some() {
            ClassifiedType::Structure
        } else {
            ClassifiedType::Scalar(ScalarCompatibilityKind::Elementary)
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
    if declared_type.field_path.is_empty() {
        if is_builtin_scalar_name(declared_type.base_name.as_ref()) {
            return Some(ClassifiedType::Scalar(scalar_compatibility_kind(
                declared_type.base_name.as_ref(),
            )));
        }
        match declared_type.namespace {
            Namespace::Type => {
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
            Namespace::Value => {
                if let Some((resolved_unit, resolved_fact)) =
                    resolve_named_value_fact(project, unit, declared_type.base_name.as_ref())
                        .or_else(|| {
                            resolve_named_type_fact(project, unit, declared_type.base_name.as_ref())
                        })
                {
                    return classify_normalized_type_fact(
                        project,
                        resolved_unit,
                        &resolved_fact,
                        depth + 1,
                    );
                }
            }
            Namespace::Routine => {}
        }
    }
    if fact.structure.is_some() {
        return Some(ClassifiedType::Structure);
    }
    Some(ClassifiedType::Scalar(ScalarCompatibilityKind::Elementary))
}

fn combine_compatibility(left: TypeCompatibility, right: TypeCompatibility) -> TypeCompatibility {
    match (left, right) {
        (TypeCompatibility::Incompatible, _) | (_, TypeCompatibility::Incompatible) => {
            TypeCompatibility::Incompatible
        }
        (TypeCompatibility::Unknown, _) | (_, TypeCompatibility::Unknown) => {
            TypeCompatibility::Unknown
        }
        (TypeCompatibility::Convertible, _) | (_, TypeCompatibility::Convertible) => {
            TypeCompatibility::Convertible
        }
        _ => TypeCompatibility::Compatible,
    }
}

fn table_kinds_compatibility(
    expected: InternalTableDisplayKind,
    actual: InternalTableDisplayKind,
    allow_conversion: bool,
) -> TypeCompatibility {
    if expected == actual || expected == InternalTableDisplayKind::Any {
        return TypeCompatibility::Compatible;
    }
    if actual == InternalTableDisplayKind::Any {
        return TypeCompatibility::Unknown;
    }
    match (expected, actual) {
        (
            InternalTableDisplayKind::Index,
            InternalTableDisplayKind::Standard
            | InternalTableDisplayKind::Sorted
            | InternalTableDisplayKind::Index,
        ) => TypeCompatibility::Compatible,
        (
            InternalTableDisplayKind::Standard | InternalTableDisplayKind::Sorted,
            InternalTableDisplayKind::Index,
        ) => TypeCompatibility::Unknown,
        (InternalTableDisplayKind::Standard, InternalTableDisplayKind::Sorted)
        | (InternalTableDisplayKind::Sorted, InternalTableDisplayKind::Standard)
            if allow_conversion =>
        {
            TypeCompatibility::Convertible
        }
        _ => TypeCompatibility::Incompatible,
    }
}

fn scalar_kinds_compatibility(
    expected: ScalarCompatibilityKind,
    actual: ScalarCompatibilityKind,
) -> TypeCompatibility {
    if expected == actual || scalar_kind_covers(expected, actual) {
        return TypeCompatibility::Compatible;
    }
    if matches!(
        (expected, actual),
        (ScalarCompatibilityKind::Elementary, _) | (_, ScalarCompatibilityKind::Elementary)
    ) {
        return TypeCompatibility::Unknown;
    }
    if scalar_kind_is_generic(expected) {
        return TypeCompatibility::Incompatible;
    }
    if scalar_kind_is_generic(actual) {
        return TypeCompatibility::Unknown;
    }
    if matches!(
        (expected, actual),
        (ScalarCompatibilityKind::Date, ScalarCompatibilityKind::Time)
            | (ScalarCompatibilityKind::Time, ScalarCompatibilityKind::Date)
    ) {
        TypeCompatibility::Incompatible
    } else {
        TypeCompatibility::Convertible
    }
}

fn scalar_kind_is_generic(kind: ScalarCompatibilityKind) -> bool {
    matches!(
        kind,
        ScalarCompatibilityKind::Any
            | ScalarCompatibilityKind::Simple
            | ScalarCompatibilityKind::Numeric
            | ScalarCompatibilityKind::DecFloat
            | ScalarCompatibilityKind::CharacterLike
            | ScalarCompatibilityKind::TextLike
            | ScalarCompatibilityKind::ByteLike
    )
}

fn scalar_kind_covers(expected: ScalarCompatibilityKind, actual: ScalarCompatibilityKind) -> bool {
    match expected {
        ScalarCompatibilityKind::Any => true,
        ScalarCompatibilityKind::Simple => actual != ScalarCompatibilityKind::Any,
        ScalarCompatibilityKind::Numeric => matches!(
            actual,
            ScalarCompatibilityKind::Numeric
                | ScalarCompatibilityKind::DecFloat
                | ScalarCompatibilityKind::NumericConcrete
                | ScalarCompatibilityKind::DecFloatConcrete
        ),
        ScalarCompatibilityKind::DecFloat => matches!(
            actual,
            ScalarCompatibilityKind::DecFloat | ScalarCompatibilityKind::DecFloatConcrete
        ),
        ScalarCompatibilityKind::CharacterLike => matches!(
            actual,
            ScalarCompatibilityKind::CharacterLike
                | ScalarCompatibilityKind::TextLike
                | ScalarCompatibilityKind::TextConcrete
                | ScalarCompatibilityKind::CharacterConcrete
                | ScalarCompatibilityKind::Date
                | ScalarCompatibilityKind::Time
        ),
        ScalarCompatibilityKind::TextLike => matches!(
            actual,
            ScalarCompatibilityKind::TextLike | ScalarCompatibilityKind::TextConcrete
        ),
        ScalarCompatibilityKind::ByteLike => matches!(
            actual,
            ScalarCompatibilityKind::ByteLike | ScalarCompatibilityKind::ByteConcrete
        ),
        _ => false,
    }
}

fn scalar_compatibility_kind(name: &str) -> ScalarCompatibilityKind {
    match name {
        "any" | "data" => ScalarCompatibilityKind::Any,
        "simple" => ScalarCompatibilityKind::Simple,
        "numeric" => ScalarCompatibilityKind::Numeric,
        "decfloat" => ScalarCompatibilityKind::DecFloat,
        "clike" => ScalarCompatibilityKind::CharacterLike,
        "csequence" => ScalarCompatibilityKind::TextLike,
        "xsequence" => ScalarCompatibilityKind::ByteLike,
        "i" | "int1" | "int2" | "int4" | "int8" | "f" | "p" => {
            ScalarCompatibilityKind::NumericConcrete
        }
        "decfloat16" | "decfloat34" => ScalarCompatibilityKind::DecFloatConcrete,
        "c" | "string" => ScalarCompatibilityKind::TextConcrete,
        "n" | "abap_bool" | "flag" | "xfeld" => ScalarCompatibilityKind::CharacterConcrete,
        "d" => ScalarCompatibilityKind::Date,
        "t" => ScalarCompatibilityKind::Time,
        "x" | "xstring" => ScalarCompatibilityKind::ByteConcrete,
        _ if name.starts_with("char") && name[4..].chars().all(|ch| ch.is_ascii_digit()) => {
            ScalarCompatibilityKind::TextConcrete
        }
        _ => ScalarCompatibilityKind::Elementary,
    }
}

fn normalized_type_facts_match_by_name(expected: &TypeFactData, actual: &TypeFactData) -> bool {
    let expected_is_table = fact_is_table_shape(expected);
    let actual_is_table = fact_is_table_shape(actual);
    if expected_is_table != actual_is_table {
        return false;
    }
    if let (Some(expected), Some(actual)) = (
        expected
            .type_clause_display
            .as_deref()
            .and_then(parse_internal_table_display),
        actual
            .type_clause_display
            .as_deref()
            .and_then(parse_internal_table_display),
    ) && expected.kind != actual.kind
    {
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

fn internal_table_kinds_differ(
    project: &ProjectAnalysis,
    expected_unit: &UnitAnalysis,
    expected: &TypeFactData,
    actual_unit: &UnitAnalysis,
    actual: &TypeFactData,
) -> bool {
    matches!(
        (
            internal_table_fact_kind(project, expected_unit, expected, 0),
            internal_table_fact_kind(project, actual_unit, actual, 0),
        ),
        (Some(expected), Some(actual)) if expected != actual
    )
}

fn resolved_internal_table_displays_match(
    project: &ProjectAnalysis,
    expected_unit: &UnitAnalysis,
    expected: &TypeFactData,
    actual_unit: &UnitAnalysis,
    actual: &TypeFactData,
) -> bool {
    let Some((expected_kind, expected_line)) =
        internal_table_display_parts(project, expected_unit, expected, 0)
    else {
        return false;
    };
    let Some((actual_kind, actual_line)) =
        internal_table_display_parts(project, actual_unit, actual, 0)
    else {
        return false;
    };
    expected_kind == actual_kind
        && match (expected_line, actual_line) {
            (Some(expected_line), Some(actual_line)) => {
                expected_line.eq_ignore_ascii_case(&actual_line)
            }
            _ => true,
        }
}

fn internal_table_display_parts(
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    fact: &TypeFactData,
    depth: usize,
) -> Option<(InternalTableDisplayKind, Option<String>)> {
    if depth >= 8 {
        return None;
    }
    if let Some(display) = fact
        .type_clause_display
        .as_deref()
        .and_then(parse_internal_table_display)
    {
        return Some((
            display.kind,
            display.line_display.map(|line| line.to_string()),
        ));
    }
    let declared_type = fact.declared_type.as_ref()?;
    if declared_type.namespace != Namespace::Type
        || declared_type.is_ref
        || !declared_type.field_path.is_empty()
    {
        return None;
    }
    let (unit, fact) = resolve_named_type_fact(project, unit, declared_type.base_name.as_ref())?;
    internal_table_display_parts(project, unit, &fact, depth + 1)
}

fn internal_table_fact_kind(
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    fact: &TypeFactData,
    depth: usize,
) -> Option<InternalTableDisplayKind> {
    if depth >= 8 {
        return None;
    }
    if let Some(kind) = fact
        .type_clause_display
        .as_deref()
        .and_then(parse_internal_table_display)
        .map(|display| display.kind)
    {
        return Some(kind);
    }
    let declared_type = fact.declared_type.as_ref()?;
    if declared_type.namespace != Namespace::Type
        || declared_type.is_ref
        || !declared_type.field_path.is_empty()
    {
        return None;
    }
    let (unit, fact) = resolve_named_type_fact(project, unit, declared_type.base_name.as_ref())?;
    internal_table_fact_kind(project, unit, &fact, depth + 1)
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
        && match (expected_table.line_display, actual_table.line_display) {
            (Some(expected_line), Some(actual_line)) => {
                expected_line.eq_ignore_ascii_case(actual_line)
            }
            _ => true,
        }
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
    line_display: Option<&'a str>,
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
        ("RANGE OF ", InternalTableDisplayKind::Standard),
    ] {
        if upper.starts_with(prefix) {
            let line_display = trimmed[prefix.len()..].trim();
            return Some(InternalTableDisplay {
                kind,
                line_display: Some(trim_internal_table_line_display(line_display)),
            });
        }
    }

    for (phrase, kind) in [
        ("STANDARD TABLE", InternalTableDisplayKind::Standard),
        ("TABLE", InternalTableDisplayKind::Standard),
        ("SORTED TABLE", InternalTableDisplayKind::Sorted),
        ("HASHED TABLE", InternalTableDisplayKind::Hashed),
        ("ANY TABLE", InternalTableDisplayKind::Any),
        ("INDEX TABLE", InternalTableDisplayKind::Index),
    ] {
        if upper == phrase {
            return Some(InternalTableDisplay {
                kind,
                line_display: None,
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

fn resolve_named_value_fact<'a>(
    project: &'a ProjectAnalysis,
    preferred_unit: &'a UnitAnalysis,
    name: &str,
) -> Option<(&'a UnitAnalysis, TypeFactData)> {
    for unit in std::iter::once(preferred_unit).chain(project.units.iter()) {
        let Some(symbol) = unit.symbols.iter().find(|symbol| {
            symbol.scope == unit.root_scope
                && symbol.kind.occupies(Namespace::Value)
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

fn ref_is_object(
    project: &ProjectAnalysis,
    name: &str,
    handle: Option<SymbolHandle>,
) -> Option<bool> {
    match name {
        "object" => return Some(true),
        "data" => return Some(false),
        _ => {}
    }
    let handle = handle?;
    project.units.get(handle.unit.as_usize()).map(|unit| {
        matches!(
            unit.symbol(handle.symbol).kind,
            SymbolKind::Class | SymbolKind::Interface
        )
    })
}

fn symbol_handle_is_same_or_subtype(
    project: &ProjectAnalysis,
    actual: SymbolHandle,
    expected: SymbolHandle,
) -> bool {
    symbol_handle_is_same_or_subtype_inner(project, actual, expected, &mut HashSet::new())
}

fn symbol_handle_is_same_or_subtype_inner(
    project: &ProjectAnalysis,
    actual: SymbolHandle,
    expected: SymbolHandle,
    visited: &mut HashSet<SymbolHandle>,
) -> bool {
    if actual == expected {
        return true;
    }
    if !visited.insert(actual) {
        return false;
    }

    let unit = &project.units[actual.unit.as_usize()];
    for implemented in unit
        .implemented_interfaces
        .iter()
        .filter(|implemented| implemented.owner_symbol == actual.symbol)
    {
        let Some(interface) =
            resolve_type_symbol_handle(project, unit, implemented.interface_name.as_ref())
        else {
            continue;
        };
        if symbol_handle_is_same_or_subtype_inner(project, interface, expected, visited) {
            return true;
        }
    }

    unit.class_superclass(actual.symbol)
        .and_then(|inheritance| {
            resolve_type_symbol_handle(project, unit, inheritance.superclass_name.as_ref())
        })
        .is_some_and(|superclass| {
            symbol_handle_is_same_or_subtype_inner(project, superclass, expected, visited)
        })
}

fn is_internal_table_type_display(display: &str) -> bool {
    parse_internal_table_display(display).is_some()
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
            | "simple"
            | "numeric"
            | "decfloat"
            | "clike"
            | "csequence"
            | "xsequence"
            | "abap_bool"
            | "flag"
            | "xfeld"
    ) || (name.starts_with("char") && name[4..].chars().all(|ch| ch.is_ascii_digit()))
}

#[cfg(test)]
mod tests {
    use super::{
        InternalTableDisplayKind, ScalarCompatibilityKind, TypeCompatibility,
        named_type_refs_match, normalized_internal_table_displays_match,
        normalized_type_facts_match_by_name, parse_internal_table_display,
        scalar_kinds_compatibility, table_kinds_compatibility,
    };
    use crate::def_map::{FieldTypeRefData, TypeFactData};
    use crate::scope::Namespace;
    use std::sync::Arc;

    fn type_ref(name: &str) -> FieldTypeRefData {
        FieldTypeRefData {
            namespace: Namespace::Type,
            is_ref: false,
            base_name: Arc::from(name),
            field_path: Vec::new(),
        }
    }

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
        assert_eq!(parsed.line_display, Some("tline"));
    }

    #[test]
    fn strips_table_key_additions_from_internal_table_display() {
        let parsed =
            parse_internal_table_display("TABLE OF tline WITH EMPTY KEY").expect("table display");
        assert_eq!(parsed.line_display, Some("tline"));
    }

    #[test]
    fn parses_generic_standard_table_display_without_line_type() {
        let parsed = parse_internal_table_display("STANDARD TABLE").expect("table display");
        assert_eq!(parsed.kind, InternalTableDisplayKind::Standard);
        assert_eq!(parsed.line_display, None);
    }

    #[test]
    fn parses_range_display_as_standard_table_display() {
        let parsed =
            parse_internal_table_display("RANGE OF zattp_param_value").expect("range display");
        assert_eq!(parsed.kind, InternalTableDisplayKind::Standard);
        assert_eq!(parsed.line_display, Some("zattp_param_value"));
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

    #[test]
    fn does_not_match_table_facts_by_line_type_when_kinds_differ() {
        let expected = TypeFactData {
            structure: None,
            declared_type: None,
            type_clause_display: Some(Arc::from("STANDARD TABLE OF i")),
            table_line: Some(Box::new(TypeFactData {
                structure: None,
                declared_type: Some(type_ref("i")),
                type_clause_display: None,
                table_line: None,
            })),
        };
        let actual = TypeFactData {
            structure: None,
            declared_type: None,
            type_clause_display: Some(Arc::from("SORTED TABLE OF i WITH UNIQUE KEY table_line")),
            table_line: Some(Box::new(TypeFactData {
                structure: None,
                declared_type: Some(type_ref("i")),
                type_clause_display: None,
                table_line: None,
            })),
        };

        assert!(!normalized_type_facts_match_by_name(&expected, &actual));
    }

    #[test]
    fn matches_generic_and_specific_standard_table_displays() {
        let expected = TypeFactData {
            structure: None,
            declared_type: None,
            type_clause_display: Some(Arc::from("STANDARD TABLE")),
            table_line: None,
        };
        let actual = TypeFactData {
            structure: None,
            declared_type: None,
            type_clause_display: Some(Arc::from("STANDARD TABLE OF tline")),
            table_line: None,
        };

        assert!(normalized_internal_table_displays_match(&expected, &actual));
    }

    #[test]
    fn treats_unresolved_elementary_scalar_differences_as_unknown() {
        assert_eq!(
            scalar_kinds_compatibility(
                ScalarCompatibilityKind::Elementary,
                ScalarCompatibilityKind::Elementary,
            ),
            TypeCompatibility::Compatible
        );
        assert_eq!(
            scalar_kinds_compatibility(
                ScalarCompatibilityKind::Date,
                ScalarCompatibilityKind::Elementary,
            ),
            TypeCompatibility::Unknown
        );
        assert_eq!(
            scalar_kinds_compatibility(
                ScalarCompatibilityKind::Time,
                ScalarCompatibilityKind::Elementary,
            ),
            TypeCompatibility::Unknown
        );
        assert_eq!(
            scalar_kinds_compatibility(
                ScalarCompatibilityKind::Date,
                ScalarCompatibilityKind::Time
            ),
            TypeCompatibility::Incompatible
        );
        assert_eq!(
            scalar_kinds_compatibility(
                ScalarCompatibilityKind::Time,
                ScalarCompatibilityKind::Date
            ),
            TypeCompatibility::Incompatible
        );
    }

    #[test]
    fn classifies_generic_scalar_subsets() {
        assert_eq!(
            scalar_kinds_compatibility(
                ScalarCompatibilityKind::Numeric,
                ScalarCompatibilityKind::NumericConcrete,
            ),
            TypeCompatibility::Compatible
        );
        assert_eq!(
            scalar_kinds_compatibility(
                ScalarCompatibilityKind::Numeric,
                ScalarCompatibilityKind::TextConcrete,
            ),
            TypeCompatibility::Incompatible
        );
        assert_eq!(
            scalar_kinds_compatibility(
                ScalarCompatibilityKind::CharacterLike,
                ScalarCompatibilityKind::Date,
            ),
            TypeCompatibility::Compatible
        );
        assert_eq!(
            scalar_kinds_compatibility(
                ScalarCompatibilityKind::Simple,
                ScalarCompatibilityKind::TextLike,
            ),
            TypeCompatibility::Compatible
        );
        assert_eq!(
            scalar_kinds_compatibility(
                ScalarCompatibilityKind::TextLike,
                ScalarCompatibilityKind::CharacterConcrete,
            ),
            TypeCompatibility::Incompatible
        );
        assert_eq!(
            scalar_kinds_compatibility(
                ScalarCompatibilityKind::ByteLike,
                ScalarCompatibilityKind::ByteConcrete,
            ),
            TypeCompatibility::Compatible
        );
    }

    #[test]
    fn classifies_generic_table_categories() {
        assert_eq!(
            table_kinds_compatibility(
                InternalTableDisplayKind::Any,
                InternalTableDisplayKind::Hashed,
                true,
            ),
            TypeCompatibility::Compatible
        );
        assert_eq!(
            table_kinds_compatibility(
                InternalTableDisplayKind::Index,
                InternalTableDisplayKind::Sorted,
                true,
            ),
            TypeCompatibility::Compatible
        );
        assert_eq!(
            table_kinds_compatibility(
                InternalTableDisplayKind::Index,
                InternalTableDisplayKind::Hashed,
                true,
            ),
            TypeCompatibility::Incompatible
        );
        assert_eq!(
            table_kinds_compatibility(
                InternalTableDisplayKind::Sorted,
                InternalTableDisplayKind::Index,
                true,
            ),
            TypeCompatibility::Unknown
        );
        assert_eq!(
            table_kinds_compatibility(
                InternalTableDisplayKind::Standard,
                InternalTableDisplayKind::Sorted,
                true,
            ),
            TypeCompatibility::Convertible
        );
        assert_eq!(
            table_kinds_compatibility(
                InternalTableDisplayKind::Sorted,
                InternalTableDisplayKind::Standard,
                true,
            ),
            TypeCompatibility::Convertible
        );
        assert_eq!(
            table_kinds_compatibility(
                InternalTableDisplayKind::Standard,
                InternalTableDisplayKind::Sorted,
                false,
            ),
            TypeCompatibility::Incompatible
        );
        assert_eq!(
            table_kinds_compatibility(
                InternalTableDisplayKind::Sorted,
                InternalTableDisplayKind::Standard,
                false,
            ),
            TypeCompatibility::Incompatible
        );
    }
}
