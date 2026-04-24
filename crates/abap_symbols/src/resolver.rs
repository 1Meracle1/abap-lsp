use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::builtins::builtin_routine_spec;
use crate::def_map::{
    ClassMemberKind, FieldTypeRefData, ReferenceKind, Resolution, StructureData,
    StructureFieldData, SymbolData, SymbolKind, UnitAnalysis, Visibility,
};
use crate::ids::{ScopeId, StructureId, SymbolHandle, SymbolId, UnitId};
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
        if let Some(symbols) = scope_index
            .get(scope_id.as_usize())
            .and_then(|scope_map| scope_map.get(&key))
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
        let scope = unit.scopes.get(scope_id.as_usize())?;
        if scope.kind == ScopeKind::Class {
            return scope.owner;
        }
        current = scope.parent;
    }
    None
}

fn class_scope_symbol(
    unit: &UnitAnalysis,
    class_symbol: SymbolId,
    namespace: Namespace,
    name: &Arc<str>,
) -> Option<SymbolId> {
    unit.symbols.iter().find_map(|symbol| {
        (symbol.name == *name
            && symbol.kind.occupies(namespace)
            && unit.scope(symbol.scope).kind == ScopeKind::Class
            && unit.scope(symbol.scope).owner == Some(class_symbol))
        .then_some(symbol.id)
    })
}

fn inherited_class_scope_symbol(
    unit: &UnitAnalysis,
    class_symbol: SymbolId,
    namespace: Namespace,
    name: &Arc<str>,
) -> Option<SymbolId> {
    let symbol_id = class_scope_symbol(unit, class_symbol, namespace, name)?;
    if namespace != Namespace::Value {
        return Some(symbol_id);
    }
    let symbol = unit.symbol(symbol_id);
    if !matches!(
        symbol.kind,
        crate::SymbolKind::Variable | crate::SymbolKind::Constant
    ) {
        return Some(symbol_id);
    }
    let member = unit.class_member(class_symbol, name.as_ref())?;
    (member.kind == ClassMemberKind::Attribute && member.visibility != Visibility::Private)
        .then_some(symbol_id)
}

fn resolve_direct_superclass_handle_in_project(
    units: &[UnitAnalysis],
    current: SymbolHandle,
    per_unit_root_index: &[HashMap<(Namespace, Arc<str>), SymbolId>],
    visible_units: &[Vec<UnitId>],
    root_index: &HashMap<(Namespace, Arc<str>), Vec<SymbolHandle>>,
) -> Option<SymbolHandle> {
    let unit = &units[current.unit.as_usize()];
    let inheritance = unit.class_superclass(current.symbol)?;
    if let Some(symbol_id) = per_unit_root_index[current.unit.as_usize()]
        .get(&(Namespace::Type, Arc::clone(&inheritance.superclass_name)))
        .copied()
    {
        return Some(SymbolHandle {
            unit: current.unit,
            symbol: symbol_id,
        });
    }
    if let Some(symbol) = resolve_root_symbol_in_visible_units(
        current.unit.as_usize(),
        Namespace::Type,
        &inheritance.superclass_name,
        per_unit_root_index,
        visible_units,
    ) {
        return Some(symbol);
    }
    root_index
        .get(&(Namespace::Type, Arc::clone(&inheritance.superclass_name)))
        .and_then(|handles| handles.first().copied())
}

fn resolve_inherited_symbol_in_unit(
    unit: &UnitAnalysis,
    scope_index: &ScopeIndex,
    scope: ScopeId,
    namespace: Namespace,
    name: &Arc<str>,
) -> Option<SymbolId> {
    let mut current_class = enclosing_class_owner(unit, scope)?;
    let mut visited = std::collections::HashSet::new();
    loop {
        if !visited.insert(current_class) {
            return None;
        }
        let inheritance = unit.class_superclass(current_class)?;
        let superclass_symbol = lookup_scope_chain(
            unit,
            scope_index,
            scope,
            Namespace::Type,
            &inheritance.superclass_name,
        )?;
        if let Some(symbol_id) =
            inherited_class_scope_symbol(unit, superclass_symbol, namespace, name)
        {
            return Some(symbol_id);
        }
        current_class = superclass_symbol;
    }
}

fn resolve_inherited_symbol_in_project(
    units: &[UnitAnalysis],
    unit_idx: usize,
    scope: ScopeId,
    namespace: Namespace,
    name: &Arc<str>,
    per_unit_root_index: &[HashMap<(Namespace, Arc<str>), SymbolId>],
    visible_units: &[Vec<UnitId>],
    root_index: &HashMap<(Namespace, Arc<str>), Vec<SymbolHandle>>,
) -> Option<SymbolHandle> {
    let unit = &units[unit_idx];
    let mut current = SymbolHandle {
        unit: unit.unit_id,
        symbol: enclosing_class_owner(unit, scope)?,
    };
    let mut visited = std::collections::HashSet::new();
    loop {
        if !visited.insert(current) {
            return None;
        }
        current = resolve_direct_superclass_handle_in_project(
            units,
            current,
            per_unit_root_index,
            visible_units,
            root_index,
        )?;
        let superclass_unit = &units[current.unit.as_usize()];
        if let Some(symbol_id) =
            inherited_class_scope_symbol(superclass_unit, current.symbol, namespace, name)
        {
            return Some(SymbolHandle {
                unit: current.unit,
                symbol: symbol_id,
            });
        }
    }
}

fn innermost_loop_allows_internal_table_line_selector(unit: &UnitAnalysis, scope: ScopeId) -> bool {
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        let Some(scope_data) = unit.scopes.get(scope_id.as_usize()) else {
            return false;
        };
        if scope_data.kind == ScopeKind::LoopBlock {
            return scope_data.allows_internal_table_line_selector;
        }
        current = scope_data.parent;
    }
    false
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
        let resolution = if let Some(symbol) =
            lookup_reference_scope_chain(unit, &scope_index, scope, namespace, kind, &name)
        {
            Some(Resolution::Symbol(SymbolHandle {
                unit: unit_id,
                symbol,
            }))
        } else if let Some(symbol) =
            resolve_inherited_symbol_in_unit(unit, scope_index, scope, namespace, &name)
        {
            Some(Resolution::Symbol(SymbolHandle {
                unit: unit_id,
                symbol,
            }))
        } else if namespace == Namespace::Value && name.as_ref() == "super" {
            resolve_super_reference_in_unit(unit, &scope_index, scope).map(|symbol| {
                Resolution::Symbol(SymbolHandle {
                    unit: unit_id,
                    symbol,
                })
            })
        } else if namespace == Namespace::Type && is_builtin_type(name.as_ref()) {
            Some(Resolution::BuiltinType)
        } else if namespace == Namespace::Routine && is_builtin_routine(name.as_ref()) {
            Some(Resolution::BuiltinRoutine)
        } else if namespace == Namespace::Value
            && kind == ReferenceKind::Identifier
            && name.as_ref().eq_ignore_ascii_case("table_line")
            && innermost_loop_allows_internal_table_line_selector(unit, scope)
        {
            Some(Resolution::InternalTableLine)
        } else {
            None
        };
        unit.references[idx].resolution = resolution;
    }
}

#[allow(dead_code)]
pub fn resolve_unit(unit: &mut UnitAnalysis) {
    let scope_index = build_scope_index(unit);
    resolve_unit_with_index(unit, &scope_index);
}

fn resolve_project_cross_unit_with_filter(
    units: &mut [UnitAnalysis],
    dirty_units: Option<&HashSet<UnitId>>,
) {
    let mut root_index: HashMap<(Namespace, Arc<str>), Vec<SymbolHandle>> = HashMap::new();
    let mut per_unit_root_index: Vec<HashMap<(Namespace, Arc<str>), SymbolId>> =
        vec![HashMap::new(); units.len()];
    let mut provided_name_to_unit: HashMap<Arc<str>, SymbolHandle> = HashMap::new();
    for unit in units.iter() {
        for name in &unit.provided_names {
            provided_name_to_unit
                .entry(Arc::clone(name))
                .or_insert(SymbolHandle {
                    unit: unit.unit_id,
                    symbol: SymbolId(0),
                });
        }
        for symbol in &unit.symbols {
            if symbol.scope != unit.root_scope {
                continue;
            }
            for &namespace in symbol.kind.namespaces() {
                per_unit_root_index[unit.unit_id.as_usize()]
                    .entry((namespace, Arc::clone(&symbol.name)))
                    .or_insert(symbol.id);
                if root_symbol_is_visible_across_units_by_default(symbol) {
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
    }

    let root_symbol_names: HashSet<_> = root_index
        .keys()
        .map(|(_, name)| Arc::clone(name))
        .collect();
    let visible_units = include_visible_units_for_units(units);
    let predecessor_units = include_predecessor_units_for_units(units);

    for unit_idx in 0..units.len() {
        if dirty_units.is_some_and(|dirty| !dirty.contains(&units[unit_idx].unit_id)) {
            continue;
        }
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
                    if resolved.is_none() {
                        resolved = resolve_root_symbol_in_visible_units(
                            unit_idx,
                            Namespace::Type,
                            &superclass_name,
                            &per_unit_root_index,
                            &visible_units,
                        )
                        .map(Resolution::Symbol);
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
                    if let Some(symbol) = resolve_inherited_symbol_in_project(
                        units,
                        unit_idx,
                        reference_scope,
                        namespace,
                        &reference_name,
                        &per_unit_root_index,
                        &visible_units,
                        &root_index,
                    ) {
                        resolved = Some(Resolution::Symbol(symbol));
                        break;
                    }
                }
            }
            if resolved.is_none() {
                for namespace in namespaces {
                    if let Some(symbol) = resolve_class_member_symbol_in_visible_definition(
                        units,
                        unit_idx,
                        reference_scope,
                        namespace,
                        &reference_name,
                        &per_unit_root_index,
                        &predecessor_units,
                        &visible_units,
                    ) {
                        resolved = Some(Resolution::Symbol(symbol));
                        break;
                    }
                }
            }
            if resolved.is_none() {
                for namespace in namespaces {
                    if let Some(symbol) = resolve_root_symbol_in_visible_units(
                        unit_idx,
                        namespace,
                        &reference_name,
                        &per_unit_root_index,
                        &visible_units,
                    ) {
                        resolved = Some(Resolution::Symbol(symbol));
                        break;
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
                && reference_kind == ReferenceKind::MessageClass
                && provided_name_to_unit.contains_key(&reference_name)
            {
                resolved = Some(Resolution::External);
            }
            if resolved.is_none()
                && matches!(reference_namespace, Namespace::Type | Namespace::Routine)
                && root_symbol_names.contains(&reference_name)
            {
                resolved = Some(Resolution::External);
            }

            units[unit_idx].references[reference_idx].resolution = resolved;
        }
    }

    let snapshot = units.to_vec();
    for unit_idx in 0..units.len() {
        let mut imported = HashMap::<(u32, u32), StructureId>::new();

        let symbol_inputs: Vec<_> = units[unit_idx]
            .symbols
            .iter()
            .map(|symbol| (symbol.structure, symbol.declared_type.clone()))
            .collect();
        let mut symbol_structures = Vec::with_capacity(symbol_inputs.len());
        for (existing_structure, declared_type) in symbol_inputs {
            let structure = existing_structure.or_else(|| {
                declared_type.as_ref().and_then(|type_ref| {
                    import_structure_for_type_ref(
                        &snapshot,
                        unit_idx,
                        type_ref,
                        &per_unit_root_index,
                        &visible_units,
                        &root_index,
                        &mut units[unit_idx].structures,
                        &mut imported,
                    )
                })
            });
            symbol_structures.push(structure);
        }
        for (symbol, structure) in units[unit_idx]
            .symbols
            .iter_mut()
            .zip(symbol_structures.into_iter())
        {
            symbol.structure = structure;
        }

        let mut structure_idx = 0usize;
        while structure_idx < units[unit_idx].structures.len() {
            let field_inputs: Vec<_> = units[unit_idx].structures[structure_idx]
                .fields
                .iter()
                .map(|field| (field.structure, field.type_ref.clone()))
                .collect();
            let mut resolved_fields = Vec::with_capacity(field_inputs.len());
            for (existing_structure, type_ref) in field_inputs {
                let structure = existing_structure.or_else(|| {
                    type_ref.as_ref().and_then(|type_ref| {
                        import_structure_for_type_ref(
                            &snapshot,
                            unit_idx,
                            type_ref,
                            &per_unit_root_index,
                            &visible_units,
                            &root_index,
                            &mut units[unit_idx].structures,
                            &mut imported,
                        )
                    })
                });
                resolved_fields.push(structure);
            }
            for (field, resolved_structure) in units[unit_idx].structures[structure_idx]
                .fields
                .iter_mut()
                .zip(resolved_fields.into_iter())
            {
                field.structure = resolved_structure;
            }
            structure_idx += 1;
        }
    }

    let snapshot = units.to_vec();
    for unit_idx in 0..units.len() {
        let symbol_inputs: Vec<_> = snapshot[unit_idx]
            .symbols
            .iter()
            .map(|symbol| symbol.declared_type.clone())
            .collect();
        let mut normalized_symbols = Vec::with_capacity(symbol_inputs.len());
        for declared_type in symbol_inputs {
            normalized_symbols.push(declared_type.as_ref().and_then(|type_ref| {
                normalize_field_type_ref_for_target(
                    &snapshot,
                    unit_idx,
                    type_ref,
                    &per_unit_root_index,
                    &visible_units,
                    &root_index,
                    &mut units[unit_idx].structures,
                )
            }));
        }
        for (symbol, normalized) in units[unit_idx]
            .symbols
            .iter_mut()
            .zip(normalized_symbols.into_iter())
        {
            if let Some((structure, declared_type)) = normalized {
                symbol.structure = structure;
                symbol.declared_type = Some(declared_type);
            }
        }
    }
}

fn resolve_class_member_symbol_in_visible_definition(
    units: &[UnitAnalysis],
    unit_idx: usize,
    scope: ScopeId,
    namespace: Namespace,
    name: &Arc<str>,
    per_unit_root_index: &[HashMap<(Namespace, Arc<str>), SymbolId>],
    predecessor_units: &[Vec<UnitId>],
    visible_units: &[Vec<UnitId>],
) -> Option<SymbolHandle> {
    let current_unit = units.get(unit_idx)?;
    let current_class = enclosing_class_owner(current_unit, scope)?;
    let class_name = Arc::clone(&current_unit.symbol(current_class).name);

    let mut candidate_units = Vec::new();
    candidate_units.push(current_unit.unit_id);
    if let Some(predecessors) = predecessor_units.get(unit_idx) {
        candidate_units.extend(predecessors.iter().rev().copied());
    }
    if let Some(visible) = visible_units.get(unit_idx) {
        candidate_units.extend(visible.iter().copied());
    }

    let mut seen = HashSet::new();
    for candidate_unit in candidate_units {
        if !seen.insert(candidate_unit) {
            continue;
        }
        let candidate_idx = candidate_unit.as_usize();
        let Some(class_symbol) = per_unit_root_index
            .get(candidate_idx)
            .and_then(|index| index.get(&(Namespace::Type, Arc::clone(&class_name))))
            .copied()
        else {
            continue;
        };
        let candidate = &units[candidate_idx];
        if candidate.class_definition(class_symbol).is_none() {
            continue;
        }
        if let Some(symbol) = class_scope_symbol(candidate, class_symbol, namespace, name) {
            return Some(SymbolHandle {
                unit: candidate.unit_id,
                symbol,
            });
        }
    }

    None
}

fn root_symbol_is_visible_across_units_by_default(symbol: &SymbolData) -> bool {
    match symbol.kind {
        SymbolKind::Class | SymbolKind::Interface | SymbolKind::TypeDef => {
            !name_looks_program_local(symbol.name.as_ref())
        }
        SymbolKind::Module | SymbolKind::Report => true,
        _ => false,
    }
}

fn name_looks_program_local(name: &str) -> bool {
    let lower = name.trim().to_ascii_lowercase();
    lower.starts_with("lcl_")
        || lower.starts_with("lif_")
        || lower.starts_with("lty_")
        || lower.starts_with("ty_")
        || lower.starts_with("tty_")
}

fn include_visible_units_for_units(units: &[UnitAnalysis]) -> Vec<Vec<UnitId>> {
    let mut visible_units = vec![Vec::new(); units.len()];
    for unit in units {
        let mut stack = HashSet::new();
        let mut expansion = Vec::new();
        collect_include_expansion(units, unit.unit_id, &mut stack, &mut expansion);
        for &participant in &expansion {
            let Some(participant_visible) = visible_units.get_mut(participant.as_usize()) else {
                continue;
            };
            for &candidate in &expansion {
                if candidate != participant && !participant_visible.contains(&candidate) {
                    participant_visible.push(candidate);
                }
            }
        }
    }
    visible_units
}

fn collect_include_expansion(
    units: &[UnitAnalysis],
    unit_id: UnitId,
    stack: &mut HashSet<UnitId>,
    expansion: &mut Vec<UnitId>,
) {
    let Some(unit) = units.get(unit_id.as_usize()) else {
        return;
    };
    if !stack.insert(unit_id) {
        return;
    }
    expansion.push(unit_id);
    for target in unit.include_edges.iter().filter_map(|edge| edge.target) {
        collect_include_expansion(units, target, stack, expansion);
    }
    stack.remove(&unit_id);
}

pub(crate) fn include_predecessor_units_for_units(units: &[UnitAnalysis]) -> Vec<Vec<UnitId>> {
    let mut predecessors = vec![Vec::new(); units.len()];
    for unit in units {
        let mut stack = HashSet::new();
        record_include_predecessors(
            units,
            unit.unit_id,
            Vec::new(),
            &mut predecessors,
            &mut stack,
        );
    }
    for unit_predecessors in &mut predecessors {
        let mut seen = HashSet::new();
        unit_predecessors.retain(|unit_id| seen.insert(*unit_id));
    }
    predecessors
}

fn record_include_predecessors(
    units: &[UnitAnalysis],
    unit_id: UnitId,
    inherited_prior: Vec<UnitId>,
    predecessors: &mut [Vec<UnitId>],
    stack: &mut HashSet<UnitId>,
) -> Vec<UnitId> {
    if units.get(unit_id.as_usize()).is_none() || !stack.insert(unit_id) {
        return Vec::new();
    }

    let mut expansion = vec![unit_id];
    let mut prior = inherited_prior;
    push_unique_unit(&mut prior, unit_id);
    let targets: Vec<_> = units[unit_id.as_usize()]
        .include_edges
        .iter()
        .filter_map(|edge| edge.target)
        .collect();
    for target in targets {
        if let Some(target_predecessors) = predecessors.get_mut(target.as_usize()) {
            target_predecessors.extend(prior.iter().copied());
        }
        let nested_expansion =
            record_include_predecessors(units, target, prior.clone(), predecessors, stack);
        for expanded_unit in nested_expansion {
            push_unique_unit(&mut prior, expanded_unit);
            push_unique_unit(&mut expansion, expanded_unit);
        }
    }

    stack.remove(&unit_id);
    expansion
}

fn push_unique_unit(units: &mut Vec<UnitId>, unit_id: UnitId) {
    if !units.contains(&unit_id) {
        units.push(unit_id);
    }
}

fn resolve_root_symbol_in_visible_units(
    unit_idx: usize,
    namespace: Namespace,
    name: &Arc<str>,
    per_unit_root_index: &[HashMap<(Namespace, Arc<str>), SymbolId>],
    visible_units: &[Vec<UnitId>],
) -> Option<SymbolHandle> {
    for visible_unit in visible_units.get(unit_idx)? {
        let target_idx = visible_unit.as_usize();
        if let Some(symbol_id) = per_unit_root_index
            .get(target_idx)
            .and_then(|index| index.get(&(namespace, Arc::clone(name))))
            .copied()
        {
            return Some(SymbolHandle {
                unit: *visible_unit,
                symbol: symbol_id,
            });
        }
    }
    None
}

pub fn resolve_project_cross_unit(units: &mut [UnitAnalysis]) {
    resolve_project_cross_unit_with_filter(units, None);
}

pub(crate) fn resolve_project_cross_unit_for_units(
    units: &mut [UnitAnalysis],
    dirty_units: &HashSet<UnitId>,
) {
    resolve_project_cross_unit_with_filter(units, Some(dirty_units));
}

fn import_structure_for_type_ref(
    snapshot: &[UnitAnalysis],
    unit_idx: usize,
    type_ref: &FieldTypeRefData,
    per_unit_root_index: &[HashMap<(Namespace, Arc<str>), SymbolId>],
    visible_units: &[Vec<UnitId>],
    root_index: &HashMap<(Namespace, Arc<str>), Vec<SymbolHandle>>,
    target_structures: &mut Vec<StructureData>,
    imported: &mut HashMap<(u32, u32), StructureId>,
) -> Option<StructureId> {
    let handle = resolve_root_symbol_handle(
        snapshot,
        unit_idx,
        type_ref,
        per_unit_root_index,
        visible_units,
        root_index,
    )?;
    let source_unit_idx = handle.unit.as_usize();
    let mut seen = HashSet::new();
    let mut structure_id = resolve_symbol_structure_for_target(
        snapshot,
        unit_idx,
        source_unit_idx,
        handle.symbol,
        per_unit_root_index,
        visible_units,
        root_index,
        target_structures,
        imported,
        &mut seen,
    )?;
    for field_name in &type_ref.field_path {
        let field = target_structures[structure_id.as_usize()]
            .fields
            .iter()
            .find(|field| field.name.as_ref() == field_name.as_ref())?;
        structure_id = field.structure?;
    }
    Some(structure_id)
}

fn resolve_root_symbol_handle(
    units: &[UnitAnalysis],
    unit_idx: usize,
    type_ref: &FieldTypeRefData,
    per_unit_root_index: &[HashMap<(Namespace, Arc<str>), SymbolId>],
    visible_units: &[Vec<UnitId>],
    root_index: &HashMap<(Namespace, Arc<str>), Vec<SymbolHandle>>,
) -> Option<SymbolHandle> {
    let namespaces = if type_ref.namespace == Namespace::Value {
        [Namespace::Value, Namespace::Type]
    } else {
        [type_ref.namespace, type_ref.namespace]
    };
    for namespace in namespaces {
        if let Some(symbol_id) = per_unit_root_index[unit_idx]
            .get(&(namespace, Arc::clone(&type_ref.base_name)))
            .copied()
        {
            return Some(SymbolHandle {
                unit: units[unit_idx].unit_id,
                symbol: symbol_id,
            });
        }
        if let Some(handle) = resolve_root_symbol_in_visible_units(
            unit_idx,
            namespace,
            &type_ref.base_name,
            per_unit_root_index,
            visible_units,
        ) {
            return Some(handle);
        }
        if let Some(handles) = root_index.get(&(namespace, Arc::clone(&type_ref.base_name)))
            && let Some(handle) = handles.first().copied()
        {
            return Some(handle);
        }
    }
    None
}

fn import_structure(
    snapshot: &[UnitAnalysis],
    source_unit_idx: usize,
    source_structure_id: StructureId,
    target_structures: &mut Vec<StructureData>,
    imported: &mut HashMap<(u32, u32), StructureId>,
) -> StructureId {
    let key = (source_unit_idx as u32, source_structure_id.0);
    if let Some(existing) = imported.get(&key).copied() {
        return existing;
    }

    let source = snapshot[source_unit_idx].structure(source_structure_id);
    let new_id = StructureId(target_structures.len() as u32);
    target_structures.push(StructureData {
        id: new_id,
        origin_unit: source.origin_unit,
        origin_structure: source.origin_structure,
        name: Arc::clone(&source.name),
        fields: Vec::new(),
    });
    imported.insert(key, new_id);

    let fields = source
        .fields
        .iter()
        .map(|field| StructureFieldData {
            name: Arc::clone(&field.name),
            decl_range: field.decl_range.clone(),
            decl_unit: field.decl_unit,
            structure: field.structure.map(|nested| {
                import_structure(
                    snapshot,
                    source_unit_idx,
                    nested,
                    target_structures,
                    imported,
                )
            }),
            type_ref: field.type_ref.clone(),
            value_clause_display: field.value_clause_display.clone(),
        })
        .collect();
    target_structures[new_id.as_usize()].fields = fields;
    new_id
}

fn resolve_symbol_structure_for_target(
    snapshot: &[UnitAnalysis],
    target_unit_idx: usize,
    current_unit_idx: usize,
    symbol_id: SymbolId,
    per_unit_root_index: &[HashMap<(Namespace, Arc<str>), SymbolId>],
    visible_units: &[Vec<UnitId>],
    root_index: &HashMap<(Namespace, Arc<str>), Vec<SymbolHandle>>,
    target_structures: &mut Vec<StructureData>,
    imported: &mut HashMap<(u32, u32), StructureId>,
    seen: &mut HashSet<(u32, u32)>,
) -> Option<StructureId> {
    if !seen.insert((current_unit_idx as u32, symbol_id.0)) {
        return None;
    }

    let symbol = snapshot[current_unit_idx].symbol(symbol_id);
    if let Some(structure_id) = symbol.structure {
        return Some(if current_unit_idx == target_unit_idx {
            structure_id
        } else {
            import_structure(
                snapshot,
                current_unit_idx,
                structure_id,
                target_structures,
                imported,
            )
        });
    }

    let next_type_ref = symbol.declared_type.as_ref()?;
    let handle = resolve_root_symbol_handle(
        snapshot,
        current_unit_idx,
        next_type_ref,
        per_unit_root_index,
        visible_units,
        root_index,
    )?;
    resolve_symbol_structure_for_target(
        snapshot,
        target_unit_idx,
        handle.unit.as_usize(),
        handle.symbol,
        per_unit_root_index,
        visible_units,
        root_index,
        target_structures,
        imported,
        seen,
    )
}

fn normalize_field_type_ref_for_target(
    snapshot: &[UnitAnalysis],
    unit_idx: usize,
    type_ref: &FieldTypeRefData,
    per_unit_root_index: &[HashMap<(Namespace, Arc<str>), SymbolId>],
    visible_units: &[Vec<UnitId>],
    root_index: &HashMap<(Namespace, Arc<str>), Vec<SymbolHandle>>,
    target_structures: &mut Vec<StructureData>,
) -> Option<(Option<StructureId>, FieldTypeRefData)> {
    if type_ref.field_path.is_empty() {
        return None;
    }

    let base_ref = FieldTypeRefData {
        namespace: type_ref.namespace,
        is_ref: type_ref.is_ref,
        base_name: Arc::clone(&type_ref.base_name),
        field_path: Vec::new(),
    };
    let handle = resolve_root_symbol_handle(
        snapshot,
        unit_idx,
        &base_ref,
        per_unit_root_index,
        visible_units,
        root_index,
    )?;

    let mut imported = HashMap::new();
    let mut seen = HashSet::new();
    let mut structure_id = resolve_symbol_structure_for_target(
        snapshot,
        unit_idx,
        handle.unit.as_usize(),
        handle.symbol,
        per_unit_root_index,
        visible_units,
        root_index,
        target_structures,
        &mut imported,
        &mut seen,
    )?;

    for (idx, field_name) in type_ref.field_path.iter().enumerate() {
        let field = target_structures[structure_id.as_usize()]
            .fields
            .iter()
            .find(|field| field.name.as_ref() == field_name.as_ref())?;
        if idx + 1 == type_ref.field_path.len() {
            return field
                .type_ref
                .clone()
                .map(|declared_type| (field.structure, declared_type));
        }
        structure_id = field.structure?;
    }

    None
}
