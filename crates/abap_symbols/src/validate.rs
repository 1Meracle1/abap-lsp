use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::Arc;

use abap_lexer::TextRange;

use crate::builtins::builtin_routine_spec;
use crate::compatibility::{
    TypeFactLookup, call_section_matches_parameter, parameter_is_required,
    positional_parameter_section, type_facts_compatibility, type_facts_parameter_compatibility,
    type_facts_strict_table_kind_compatibility,
};
use crate::def_map::{
    Diagnostic, DiagnosticKind, FieldAccess, FieldTypeRefData, FormParameterData,
    FormParameterSection, FunctionModuleData, FunctionModuleParameterData,
    FunctionModuleParameterSection, NamedArgumentTarget, PerformParameterSection, ReferenceKind,
    Resolution, SqlNameRefData, SqlNameRefKind, SqlSourceData, StructureFieldShape, TypeFactData,
    ValueFlowKind, ValueFlowTargetData,
};
use crate::ids::{ScopeId, StructureId, SymbolHandle, SymbolId, UnitId};
use crate::project::ProjectAnalysis;
use crate::resolver::ScopeIndex;
use crate::scope::{Namespace, ScopeKind};
use crate::{ClassMemberKind, SymbolKind, Visibility};

struct ValidationLookup<'a> {
    scope_indexes: &'a [ScopeIndex],
    per_unit_root_index: Vec<HashMap<(Namespace, Arc<str>), Vec<SymbolId>>>,
    root_index: HashMap<(Namespace, Arc<str>), Vec<SymbolHandle>>,
    message_class_entries: HashMap<Arc<str>, HashMap<Arc<str>, MessageClassEntryHandle>>,
    include_predecessors: Vec<Vec<UnitId>>,
    include_order: IncludeOrderIndex,
    type_fact_lookup: TypeFactLookup,
}

#[derive(Clone, Copy)]
struct MessageClassEntryHandle {
    unit: usize,
    entry: usize,
}

#[derive(Default)]
struct OpenSqlOrderByValidation {
    diagnostics: Vec<Diagnostic>,
    resolved_primary_key_fields: Vec<(usize, Vec<Arc<str>>)>,
}

fn validate_message_uses(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    for message in &unit.message_uses {
        let class_name = message
            .class_name
            .as_ref()
            .or_else(|| unit.message_default_class.as_ref().map(|class| &class.name));
        let Some(class_name) = class_name else {
            continue;
        };
        let Some(id) = &message.id else {
            continue;
        };
        let Some(entries) = message_class_entries_named(lookup, class_name) else {
            continue;
        };
        let Some(handle) = entries.get(id.as_ref()) else {
            diagnostics.push(Diagnostic {
                kind: DiagnosticKind::InvalidMessage,
                range: message
                    .id_range
                    .clone()
                    .or_else(|| message.class_range.clone())
                    .unwrap_or_else(|| message.range.clone()),
                message: format!(
                    "unknown message id '{}' in message class '{}'",
                    id, class_name
                ),
            });
            continue;
        };
        let entry = &project.units[handle.unit].message_class_entries[handle.entry];

        let expected = message_parameter_count(entry.text.as_ref());
        let actual = message.with_arg_ranges.len();
        if expected == actual {
            continue;
        }
        diagnostics.push(Diagnostic {
            kind: DiagnosticKind::InvalidMessage,
            range: if actual > expected {
                message
                    .with_arg_ranges
                    .get(expected)
                    .cloned()
                    .unwrap_or_else(|| message.range.clone())
            } else {
                message
                    .id_range
                    .clone()
                    .or_else(|| message.class_range.clone())
                    .unwrap_or_else(|| message.range.clone())
            },
            message: format!(
                "message {} in class {} expects {} parameter(s), but MESSAGE WITH provides {}",
                id, class_name, expected, actual
            ),
        });
    }
    diagnostics
}

fn message_class_entries_named<'a>(
    lookup: &'a ValidationLookup<'_>,
    class_name: &str,
) -> Option<&'a HashMap<Arc<str>, MessageClassEntryHandle>> {
    lookup.message_class_entries.get(class_name).or_else(|| {
        has_ascii_uppercase(class_name)
            .then(|| class_name.to_ascii_lowercase())
            .and_then(|lower| lookup.message_class_entries.get(lower.as_str()))
    })
}

fn has_ascii_uppercase(text: &str) -> bool {
    text.bytes().any(|byte| byte.is_ascii_uppercase())
}

fn message_parameter_count(text: &str) -> usize {
    let mut indexed = 0usize;
    let mut highest = 0usize;
    let mut bare = 0usize;
    let mut chars = text.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch != '&' {
            continue;
        }
        match chars.peek().and_then(|next| next.to_digit(10)) {
            Some(digit @ 1..=9) => {
                indexed += 1;
                highest = highest.max(digit as usize);
                chars.next();
            }
            _ => bare += 1,
        }
    }
    if highest > 0 {
        highest.max(indexed + bare)
    } else {
        bare
    }
}

#[derive(Clone, Copy)]
struct LoopFieldContextView<'a> {
    scope: ScopeId,
    range: &'a TextRange,
    source_access: &'a FieldAccess,
    target_access: Option<&'a FieldAccess>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct StructureKey {
    unit: UnitId,
    structure: StructureId,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ConstructorForIteratorUsageKind {
    ForBinding,
    LoopTarget,
    OtherReference,
}

struct ConstructorForIteratorUsage<'a> {
    scope: ScopeId,
    name: Arc<str>,
    range: TextRange,
    kind: ConstructorForIteratorUsageKind,
    binding: Option<&'a crate::ConstructorForBindingData>,
}

#[derive(Default)]
struct IncludeOrderIndex {
    roots: Vec<IncludeOrderRoot>,
}

struct IncludeOrderRoot {
    unit_prefixes: HashMap<UnitId, Vec<Vec<usize>>>,
}

impl IncludeOrderIndex {
    fn type_decl_after_reference(
        &self,
        reference_unit: UnitId,
        reference_offset: usize,
        decl_unit: UnitId,
        decl_offset: usize,
    ) -> bool {
        if reference_unit == decl_unit {
            return decl_offset > reference_offset;
        }
        self.roots.iter().any(|root| {
            let Some(reference_prefixes) = root.unit_prefixes.get(&reference_unit) else {
                return false;
            };
            let Some(decl_prefixes) = root.unit_prefixes.get(&decl_unit) else {
                return false;
            };
            reference_prefixes.iter().any(|reference_prefix| {
                let reference_key = location_order_key(reference_prefix, reference_offset);
                decl_prefixes
                    .iter()
                    .any(|decl_prefix| location_order_key(decl_prefix, decl_offset) > reference_key)
            })
        })
    }
}

fn location_order_key(prefix: &[usize], offset: usize) -> Vec<usize> {
    let mut key = prefix.to_vec();
    key.push(order_component(offset, 0));
    key
}

fn order_component(offset: usize, tag: usize) -> usize {
    offset.saturating_mul(2).saturating_add(tag)
}

fn build_include_order_index(project: &ProjectAnalysis) -> IncludeOrderIndex {
    if !project
        .units
        .iter()
        .any(|unit| unit.include_edges.iter().any(|edge| edge.target.is_some()))
    {
        return IncludeOrderIndex::default();
    }

    let mut roots = Vec::new();
    for unit in &project.units {
        if !unit.include_edges.iter().any(|edge| edge.target.is_some()) {
            continue;
        }
        let mut root = IncludeOrderRoot {
            unit_prefixes: HashMap::new(),
        };
        let mut prefix = Vec::new();
        collect_include_order_prefixes(
            project,
            unit.unit_id,
            &mut prefix,
            &mut root,
            &mut HashSet::new(),
        );
        roots.push(root);
    }
    IncludeOrderIndex { roots }
}

fn collect_include_order_prefixes(
    project: &ProjectAnalysis,
    unit_id: UnitId,
    prefix: &mut Vec<usize>,
    root: &mut IncludeOrderRoot,
    stack: &mut HashSet<UnitId>,
) {
    let Some(unit) = project.units.get(unit_id.as_usize()) else {
        return;
    };
    if !stack.insert(unit_id) {
        return;
    }

    root.unit_prefixes
        .entry(unit_id)
        .or_default()
        .push(prefix.clone());

    let mut edges = unit
        .include_edges
        .iter()
        .filter_map(|edge| Some((edge.range.start, edge.target?)))
        .collect::<Vec<_>>();
    edges.sort_by_key(|(offset, _)| *offset);
    for (offset, target) in edges {
        prefix.push(order_component(offset, 1));
        collect_include_order_prefixes(project, target, prefix, root, stack);
        prefix.pop();
    }

    stack.remove(&unit_id);
}

fn build_validation_lookup<'a>(
    project: &ProjectAnalysis,
    scope_indexes: &'a [ScopeIndex],
) -> ValidationLookup<'a> {
    let mut per_unit_root_index = vec![HashMap::new(); project.units.len()];
    let mut root_index = HashMap::new();

    for unit in &project.units {
        for symbol in &unit.symbols {
            if symbol.scope != unit.root_scope {
                continue;
            }
            for &namespace in symbol.kind.namespaces() {
                per_unit_root_index[unit.unit_id.as_usize()]
                    .entry((namespace, Arc::clone(&symbol.name)))
                    .or_insert_with(Vec::new)
                    .push(symbol.id);
                root_index
                    .entry((namespace, Arc::clone(&symbol.name)))
                    .or_insert_with(Vec::new)
                    .push(SymbolHandle {
                        unit: unit.unit_id,
                        symbol: symbol.id,
                    });
            }
        }
    }

    ValidationLookup {
        scope_indexes,
        per_unit_root_index,
        root_index,
        message_class_entries: build_message_class_lookup(project),
        include_predecessors: project.include_predecessor_units_by_unit(),
        include_order: build_include_order_index(project),
        type_fact_lookup: TypeFactLookup::new(project),
    }
}

fn build_message_class_lookup(
    project: &ProjectAnalysis,
) -> HashMap<Arc<str>, HashMap<Arc<str>, MessageClassEntryHandle>> {
    if !project
        .units
        .iter()
        .any(|unit| !unit.message_uses.is_empty())
    {
        return HashMap::new();
    }

    let mut by_class = HashMap::<Arc<str>, HashMap<Arc<str>, MessageClassEntryHandle>>::new();
    for (unit_idx, unit) in project.units.iter().enumerate() {
        for (entry_idx, entry) in unit.message_class_entries.iter().enumerate() {
            let class_name = if has_ascii_uppercase(&entry.class_name) {
                Arc::from(entry.class_name.to_ascii_lowercase())
            } else {
                Arc::clone(&entry.class_name)
            };
            by_class.entry(class_name).or_default().insert(
                Arc::clone(&entry.id),
                MessageClassEntryHandle {
                    unit: unit_idx,
                    entry: entry_idx,
                },
            );
        }
    }
    by_class
}

fn root_symbol_handle_matching<F>(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    preferred_unit: &crate::UnitAnalysis,
    namespace: Namespace,
    name: &Arc<str>,
    predicate: F,
) -> Option<SymbolHandle>
where
    F: Fn(&crate::SymbolData) -> bool,
{
    let key = (namespace, Arc::clone(name));
    if let Some(symbol_ids) =
        lookup.per_unit_root_index[preferred_unit.unit_id.as_usize()].get(&key)
    {
        for &symbol_id in symbol_ids {
            let handle = SymbolHandle {
                unit: preferred_unit.unit_id,
                symbol: symbol_id,
            };
            if predicate(project.units[handle.unit.as_usize()].symbol(handle.symbol)) {
                return Some(handle);
            }
        }
    }

    lookup.root_index.get(&key).and_then(|handles| {
        handles.iter().copied().find(|handle| {
            handle.unit != preferred_unit.unit_id
                && predicate(project.units[handle.unit.as_usize()].symbol(handle.symbol))
        })
    })
}

fn collect_global_names(project: &ProjectAnalysis) -> HashMap<Arc<str>, u8> {
    let mut out = HashMap::new();
    for unit in &project.units {
        for symbol in &unit.symbols {
            if symbol.scope != unit.root_scope {
                continue;
            }
            for &namespace in symbol.kind.namespaces() {
                *out.entry(Arc::clone(&symbol.name)).or_default() |= 1 << namespace as u8;
            }
        }
    }
    out
}

fn build_scope_names(unit: &crate::UnitAnalysis) -> HashMap<Arc<str>, u8> {
    let mut scope_names = HashMap::new();
    for symbol in &unit.symbols {
        for &namespace in symbol.kind.namespaces() {
            *scope_names.entry(Arc::clone(&symbol.name)).or_default() |= 1 << namespace as u8;
        }
    }
    scope_names
}

fn reference_is_tables_decl_type_ref(
    unit: &crate::UnitAnalysis,
    reference: &crate::ReferenceData,
) -> bool {
    reference.kind == ReferenceKind::TypeRef
        && reference.namespace == Namespace::Type
        && unit
            .table_work_areas
            .iter()
            .any(|work_area| work_area.name == reference.name && work_area.range == reference.range)
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
        if let Some(symbols) = scope_index
            .get(scope_id.as_usize())
            .and_then(|scope| scope.get(&key))
            && let Some(symbol_id) = symbols.last().copied()
        {
            return Some(symbol_id);
        }
        current = unit
            .scopes
            .get(scope_id.as_usize())
            .and_then(|scope| scope.parent);
    }
    None
}

fn resolve_symbol_handle_in_scope_or_includes(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_index: &ScopeIndex,
    scope: ScopeId,
    namespace: Namespace,
    name: &Arc<str>,
) -> Option<SymbolHandle> {
    if let Some(symbol_id) =
        resolve_symbol_in_scope_chain(unit, scope_index, scope, namespace, name)
    {
        return Some(SymbolHandle {
            unit: unit.unit_id,
            symbol: symbol_id,
        });
    }

    if let Some(class_symbol) = enclosing_class_owner(unit, scope) {
        let class_name = Arc::clone(&unit.symbol(class_symbol).name);
        let predecessors = lookup
            .include_predecessors
            .get(unit.unit_id.as_usize())
            .map(Vec::as_slice)
            .unwrap_or(&[]);
        for unit_id in std::iter::once(unit.unit_id).chain(predecessors.iter().rev().copied()) {
            let Some(class) =
                root_class_definition_handle_in_unit(project, lookup, unit_id, &class_name)
            else {
                continue;
            };
            let class_unit = &project.units[class.unit.as_usize()];
            if let Some(symbol) = class_unit.symbols.iter().find(|symbol| {
                symbol.name == *name
                    && symbol.kind.occupies(namespace)
                    && class_unit.scope(symbol.scope).kind == ScopeKind::Class
                    && class_unit.scope(symbol.scope).owner == Some(class.symbol)
            }) {
                return Some(SymbolHandle {
                    unit: class.unit,
                    symbol: symbol.id,
                });
            }
        }
    }

    let key = (namespace, Arc::clone(name));
    let mut visited = HashSet::new();
    let mut queue: VecDeque<_> = unit
        .include_edges
        .iter()
        .filter_map(|edge| edge.target)
        .collect();
    while let Some(target_unit_id) = queue.pop_front() {
        if !visited.insert(target_unit_id) {
            continue;
        }
        if let Some(symbol_ids) = lookup.per_unit_root_index[target_unit_id.as_usize()].get(&key)
            && let Some(symbol_id) = symbol_ids.last().copied()
        {
            return Some(SymbolHandle {
                unit: target_unit_id,
                symbol: symbol_id,
            });
        }
        queue.extend(
            project.units[target_unit_id.as_usize()]
                .include_edges
                .iter()
                .filter_map(|edge| edge.target),
        );
    }

    for predecessor_unit_id in lookup
        .include_predecessors
        .get(unit.unit_id.as_usize())
        .into_iter()
        .flatten()
        .rev()
        .copied()
    {
        if let Some(symbol_ids) =
            lookup.per_unit_root_index[predecessor_unit_id.as_usize()].get(&key)
            && let Some(symbol_id) = symbol_ids.last().copied()
        {
            return Some(SymbolHandle {
                unit: predecessor_unit_id,
                symbol: symbol_id,
            });
        }
    }

    None
}

fn scope_for_unit(unit: &crate::UnitAnalysis, scope: ScopeId) -> ScopeId {
    if unit.scopes.get(scope.as_usize()).is_some() {
        scope
    } else {
        unit.root_scope
    }
}

fn resolve_field_access_base_symbol(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_index: &ScopeIndex,
    access: &crate::FieldAccess,
) -> Option<SymbolHandle> {
    if let Some(handle) = resolve_symbol_handle_in_scope_or_includes(
        project,
        lookup,
        unit,
        scope_index,
        access.scope,
        access.base_namespace,
        &access.base_name,
    ) {
        return Some(handle);
    }

    if let Some(handle) = unit.references.iter().find_map(|reference| {
        (reference.range == access.base_range
            && reference.namespace == access.base_namespace
            && reference.name == access.base_name)
            .then_some(reference.resolution)
            .flatten()
            .and_then(|resolution| match resolution {
                Resolution::Symbol(handle) => Some(handle),
                _ => None,
            })
    }) {
        return Some(handle);
    }

    if access.in_type_position {
        let fallback_namespace = match access.base_namespace {
            Namespace::Type => Namespace::Value,
            Namespace::Value => Namespace::Type,
            Namespace::Routine => return None,
        };
        return resolve_symbol_handle_in_scope_or_includes(
            project,
            lookup,
            unit,
            scope_index,
            access.scope,
            fallback_namespace,
            &access.base_name,
        );
    }

    None
}

fn class_scoped_type_symbol_for_owner(
    unit: &crate::UnitAnalysis,
    owner_symbol: SymbolId,
    type_name: &Arc<str>,
) -> Option<SymbolId> {
    unit.symbols
        .iter()
        .find(|symbol| {
            symbol.kind == SymbolKind::TypeDef
                && symbol.name == *type_name
                && unit.scope(symbol.scope).owner == Some(owner_symbol)
                && matches!(
                    unit.scope(symbol.scope).kind,
                    ScopeKind::Class | ScopeKind::Interface
                )
        })
        .map(|symbol| symbol.id)
}

fn resolve_class_scoped_type_handle(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope: ScopeId,
    type_name: &Arc<str>,
) -> Option<SymbolHandle> {
    let class_symbol = enclosing_class_owner(unit, scope)?;
    let class_name = Arc::clone(&unit.symbol(class_symbol).name);
    let mut current = project.visible_type_owner_handle_with_predecessors(
        unit.unit_id,
        &class_name,
        &lookup.include_predecessors,
    )?;
    let mut seen = HashSet::new();
    loop {
        if !seen.insert((current.unit, current.symbol)) {
            return None;
        }
        let current_unit = &project.units[current.unit.as_usize()];
        if let Some(symbol) =
            class_scoped_type_symbol_for_owner(current_unit, current.symbol, type_name)
        {
            return Some(SymbolHandle {
                unit: current.unit,
                symbol,
            });
        }
        let superclass_name = &current_unit
            .class_superclass(current.symbol)?
            .superclass_name;
        current = root_symbol_handle_matching(
            project,
            lookup,
            current_unit,
            Namespace::Type,
            superclass_name,
            |symbol| symbol.kind == SymbolKind::Class,
        )?;
    }
}

fn enclosing_class_owner(unit: &crate::UnitAnalysis, scope: ScopeId) -> Option<SymbolId> {
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

fn enclosing_method_owner(unit: &crate::UnitAnalysis, scope: ScopeId) -> Option<SymbolId> {
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        let scope = unit.scopes.get(scope_id.as_usize())?;
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
        current = unit
            .scopes
            .get(scope_id.as_usize())
            .and_then(|scope| scope.parent);
    }
    false
}

fn resolve_class_symbol(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
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

    root_symbol_handle_matching(project, lookup, unit, Namespace::Type, name, |symbol| {
        symbol.kind == SymbolKind::Class
    })
}

fn resolve_interface_symbol(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_index: &ScopeIndex,
    scope: ScopeId,
    name: &Arc<str>,
) -> Option<SymbolHandle> {
    if let Some(symbol) =
        resolve_symbol_in_scope_chain(unit, scope_index, scope, Namespace::Type, name)
        && unit.symbol(symbol).kind == SymbolKind::Interface
    {
        return Some(SymbolHandle {
            unit: unit.unit_id,
            symbol,
        });
    }

    root_symbol_handle_matching(project, lookup, unit, Namespace::Type, name, |symbol| {
        symbol.kind == SymbolKind::Interface
    })
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
    lookup: &ValidationLookup<'_>,
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
            lookup,
            unit,
            scope_index,
            scope.id,
            &inheritance.superclass_name,
        );

        let has_super_call = unit.field_accesses.iter().any(|access| {
            scope_descends_from(unit, access.scope, scope.id)
                && access.base_namespace == Namespace::Value
                && access.base_name.as_ref().eq_ignore_ascii_case("super")
                && access.field_path.last().is_some_and(|segment| {
                    segment.name.as_ref().eq_ignore_ascii_case("constructor")
                })
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
                    && base_name.as_ref().eq_ignore_ascii_case("super")
                    && method_name.as_ref().eq_ignore_ascii_case("constructor") =>
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

fn validate_missing_method_implementations(unit: &crate::UnitAnalysis) -> Vec<Diagnostic> {
    unit.class_members
        .iter()
        .filter(|member| member.kind == ClassMemberKind::Method)
        .filter(|member| member.implementation.is_none())
        .filter(|member| unit.symbol(member.class_symbol).kind == SymbolKind::Class)
        .filter(|member| {
            !unit.member_aliases.iter().any(|alias| {
                alias.owner_symbol == member.class_symbol && alias.alias_name == member.name
            })
        })
        .filter(|member| {
            !member
                .signature
                .split_ascii_whitespace()
                .any(|part| part.eq_ignore_ascii_case("abstract"))
        })
        .map(|member| Diagnostic {
            kind: DiagnosticKind::MissingMethodImplementation,
            range: member.decl_range.clone(),
            message: format!(
                "method '{}' is declared but missing an implementation",
                member.name
            ),
        })
        .collect()
}

fn validate_abstract_class_instantiations(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_index: &ScopeIndex,
) -> Vec<Diagnostic> {
    unit.call_sites
        .iter()
        .filter(|call_site| matches!(call_site.target, NamedArgumentTarget::Constructor { .. }))
        .filter_map(|call_site| {
            let handle = resolve_method_target_handle(
                project,
                lookup,
                unit,
                scope_index,
                call_site.scope,
                &call_site.target,
            )?;
            let target_unit = &project.units[handle.unit.as_usize()];
            let target_symbol = target_unit.symbol(handle.symbol);
            if target_symbol.kind != SymbolKind::Class
                || !target_unit.class_is_abstract(handle.symbol)
            {
                return None;
            }
            Some(Diagnostic {
                kind: DiagnosticKind::AbstractClassInstantiation,
                range: call_site.range.clone(),
                message: format!("cannot instantiate abstract class '{}'", target_symbol.name),
            })
        })
        .collect()
}

fn validate_constructor_for_iterator_reuse(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    scope_index: &ScopeIndex,
) -> Vec<Diagnostic> {
    if unit.constructor_for_bindings.is_empty() {
        return Vec::new();
    }

    let binding_names = unit
        .constructor_for_bindings
        .iter()
        .map(|binding| constructor_for_iterator_usage_key(binding.scope, binding.name.as_ref()))
        .collect::<HashSet<_>>();
    let mut usages = Vec::new();
    for binding in &unit.constructor_for_bindings {
        usages.push(ConstructorForIteratorUsage {
            scope: binding.scope,
            name: Arc::<str>::from(binding.name.as_ref().to_ascii_lowercase()),
            range: binding.range.clone(),
            kind: ConstructorForIteratorUsageKind::ForBinding,
            binding: Some(binding),
        });
    }

    let mut restricted_loop_target_ranges = HashSet::new();
    for region in &unit.routine_control_regions {
        let crate::RoutineControlRegionData::Loop(loop_region) = region else {
            continue;
        };
        if loop_region.kind != crate::RoutineLoopKind::Loop {
            continue;
        }
        let Some(target_access) = loop_region.target_access.as_ref() else {
            continue;
        };
        if target_access.base_namespace != Namespace::Value || !target_access.field_path.is_empty()
        {
            continue;
        }
        let key =
            constructor_for_iterator_usage_key(loop_region.scope, target_access.base_name.as_ref());
        if !binding_names.contains(&key) {
            continue;
        }
        restricted_loop_target_ranges
            .insert((target_access.base_range.start, target_access.base_range.end));
        usages.push(ConstructorForIteratorUsage {
            scope: loop_region.scope,
            name: key.1,
            range: target_access.base_range.clone(),
            kind: ConstructorForIteratorUsageKind::LoopTarget,
            binding: None,
        });
    }

    for reference in &unit.references {
        if reference.resolution.is_some()
            || reference.namespace != Namespace::Value
            || reference.kind != ReferenceKind::Identifier
            || restricted_loop_target_ranges.contains(&(reference.range.start, reference.range.end))
        {
            continue;
        }
        let key = constructor_for_iterator_usage_key(reference.scope, reference.name.as_ref());
        if !binding_names.contains(&key) {
            continue;
        }
        usages.push(ConstructorForIteratorUsage {
            scope: reference.scope,
            name: key.1,
            range: reference.range.clone(),
            kind: ConstructorForIteratorUsageKind::OtherReference,
            binding: None,
        });
    }

    usages.sort_by_key(|usage| (usage.range.start, usage.range.end));

    let mut diagnostics = Vec::new();
    let mut first_usage_by_scope_name = HashMap::<(ScopeId, Arc<str>), usize>::new();
    for idx in 0..usages.len() {
        let key = (usages[idx].scope, Arc::clone(&usages[idx].name));
        if let Some(first_idx) = first_usage_by_scope_name.get(&key).copied() {
            if let Some(diagnostic) = constructor_for_iterator_usage_diagnostic(
                project,
                lookup,
                unit,
                scope_indexes,
                scope_index,
                &usages[first_idx],
                &usages[idx],
            ) {
                diagnostics.push(diagnostic);
            }
        } else {
            first_usage_by_scope_name.insert(key, idx);
        }
    }

    diagnostics
}

fn constructor_for_iterator_usage_key(scope: ScopeId, name: &str) -> (ScopeId, Arc<str>) {
    (scope, Arc::<str>::from(name.to_ascii_lowercase()))
}

fn constructor_for_iterator_usage_diagnostic(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    scope_index: &ScopeIndex,
    first: &ConstructorForIteratorUsage<'_>,
    current: &ConstructorForIteratorUsage<'_>,
) -> Option<Diagnostic> {
    match (first.kind, current.kind) {
        (
            ConstructorForIteratorUsageKind::ForBinding,
            ConstructorForIteratorUsageKind::ForBinding,
        ) => {
            let first_binding = first.binding?;
            let current_binding = current.binding?;
            (!constructor_for_binding_sources_compatible(
                project,
                lookup,
                unit,
                scope_indexes,
                scope_index,
                first_binding,
                current_binding,
            ))
            .then(|| Diagnostic {
                kind: DiagnosticKind::InvalidConstructorForIteratorReuse,
                range: current.range.clone(),
                message: format!(
                    "constructor FOR iterator '{}' can only be reused with an internal table of the same row type",
                    current.name
                ),
            })
        }
        (ConstructorForIteratorUsageKind::ForBinding, _) => Some(Diagnostic {
            kind: DiagnosticKind::InvalidConstructorForIteratorReuse,
            range: current.range.clone(),
            message: format!(
                "constructor FOR iterator '{}' can only be reused in FOR ... IN expressions in the same scope",
                current.name
            ),
        }),
        (_, ConstructorForIteratorUsageKind::ForBinding) => Some(Diagnostic {
            kind: DiagnosticKind::InvalidConstructorForIteratorReuse,
            range: current.range.clone(),
            message: format!(
                "constructor FOR iterator '{}' conflicts with an earlier same-scope usage; it can only be reused by another FOR ... IN expression with the same row type",
                current.name
            ),
        }),
        _ => None,
    }
}

fn constructor_for_binding_sources_compatible(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    scope_index: &ScopeIndex,
    first: &crate::ConstructorForBindingData,
    current: &crate::ConstructorForBindingData,
) -> bool {
    let Some(first_key) = constructor_for_binding_row_structure_key(
        project,
        lookup,
        unit,
        scope_indexes,
        scope_index,
        first,
    ) else {
        return true;
    };
    let Some(current_key) = constructor_for_binding_row_structure_key(
        project,
        lookup,
        unit,
        scope_indexes,
        scope_index,
        current,
    ) else {
        return true;
    };
    first_key == current_key
}

fn constructor_for_binding_row_structure_key(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    scope_index: &ScopeIndex,
    binding: &crate::ConstructorForBindingData,
) -> Option<StructureKey> {
    let access = binding.source_access.as_ref()?;
    if access.base_namespace != Namespace::Value {
        return None;
    }
    let base_handle = resolve_field_access_base_symbol(project, lookup, unit, scope_index, access)?;
    if access.field_path.is_empty() {
        let base_unit = &project.units[base_handle.unit.as_usize()];
        let base_symbol = base_unit.symbol(base_handle.symbol);
        let mut seen = HashSet::new();
        if !symbol_is_internal_table(
            project,
            lookup,
            base_unit,
            scope_indexes,
            base_symbol,
            &mut seen,
        ) {
            return None;
        }
    }
    let (structure_unit, structure_id) =
        resolve_field_access_structure(project, lookup, unit, scope_indexes, access)?;
    let structure = structure_unit.structure(structure_id);
    Some(StructureKey {
        unit: structure.origin_unit,
        structure: structure.origin_structure,
    })
}

fn reference_is_restricted_constructor_for_iterator_use(
    unit: &crate::UnitAnalysis,
    reference: &crate::ReferenceData,
) -> bool {
    reference.namespace == Namespace::Value
        && reference.kind == ReferenceKind::Identifier
        && unit.constructor_for_bindings.iter().any(|binding| {
            binding.scope == reference.scope
                && binding
                    .name
                    .as_ref()
                    .eq_ignore_ascii_case(reference.name.as_ref())
        })
}

fn resolve_project_class_symbol<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    preferred_unit: &'a crate::UnitAnalysis,
    name: &Arc<str>,
) -> Option<SymbolHandle> {
    root_symbol_handle_matching(
        project,
        lookup,
        preferred_unit,
        Namespace::Type,
        name,
        |symbol| symbol.kind == SymbolKind::Class,
    )
}

fn direct_superclass_handle(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    class_symbol: SymbolId,
) -> Option<SymbolHandle> {
    let inheritance = unit.class_superclass(class_symbol)?;
    resolve_project_class_symbol(project, lookup, unit, &inheritance.superclass_name)
}

fn class_is_or_inherits_from(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
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
        let Some(next) = direct_superclass_handle(project, lookup, unit, current.symbol) else {
            return false;
        };
        current = next;
    }
}

fn class_member_visible_to(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
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
                lookup,
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
    lookup: &ValidationLookup<'_>,
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
            if !class_member_uses_inherited_signature(member) {
                return Some((unit, member));
            }
        }
        if let Some(member) =
            resolve_class_member_alias_target(project, lookup, current, member_name)
        {
            return Some(member);
        }
        current = direct_superclass_handle(project, lookup, unit, current.symbol)?;
    }
}

fn resolve_class_member_alias_target<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    owner: SymbolHandle,
    alias_name: &str,
) -> Option<(&'a crate::UnitAnalysis, &'a crate::ClassMemberData)> {
    let unit = &project.units[owner.unit.as_usize()];
    let alias = unit.member_aliases.iter().find(|alias| {
        alias.owner_symbol == owner.symbol && alias.alias_name.as_ref() == alias_name
    })?;
    let target = resolve_exposed_interface_handle(
        project,
        lookup,
        owner,
        alias.target_interface_name.as_ref(),
    )?;
    let target_unit = &project.units[target.unit.as_usize()];
    target_unit
        .class_member(target.symbol, alias.target_member_name.as_ref())
        .map(|member| (target_unit, member))
}

fn resolve_inherited_attribute_symbol<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    caller_unit: &'a crate::UnitAnalysis,
    caller_scope: ScopeId,
    name: &str,
) -> Option<SymbolHandle> {
    let class_symbol = enclosing_class_owner(caller_unit, caller_scope)?;
    let mut current = SymbolHandle {
        unit: caller_unit.unit_id,
        symbol: class_symbol,
    };
    let mut visited = HashSet::new();
    loop {
        current = direct_superclass_handle(
            project,
            lookup,
            &project.units[current.unit.as_usize()],
            current.symbol,
        )?;
        if !visited.insert(current) {
            return None;
        }
        let unit = &project.units[current.unit.as_usize()];
        let Some(member) = unit.class_member(current.symbol, name) else {
            continue;
        };
        if member.kind != ClassMemberKind::Attribute
            || !class_member_visible_to(project, lookup, caller_unit, caller_scope, unit, member)
        {
            continue;
        }
        let symbol = unit.symbols.iter().find(|symbol| {
            matches!(
                symbol.kind,
                SymbolKind::Variable | SymbolKind::Constant | SymbolKind::EnumMember
            ) && symbol.name.as_ref() == name
                && unit.scope(symbol.scope).kind == ScopeKind::Class
                && unit.scope(symbol.scope).owner == Some(current.symbol)
        })?;
        return Some(SymbolHandle {
            unit: current.unit,
            symbol: symbol.id,
        });
    }
}

fn class_member_uses_inherited_signature(member: &crate::ClassMemberData) -> bool {
    member.kind == ClassMemberKind::Method
        && member.parameters.is_empty()
        && member.signature.split_ascii_whitespace().any(|part| {
            let keyword = part.trim_end_matches('.');
            keyword.eq_ignore_ascii_case("redefinition")
        })
}

fn resolve_exposed_interface_handle(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    owner: SymbolHandle,
    interface_name: &str,
) -> Option<SymbolHandle> {
    resolve_exposed_interface_handle_inner(
        project,
        lookup,
        owner,
        interface_name,
        &mut HashSet::new(),
    )
}

fn resolve_exposed_interface_handle_inner(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    owner: SymbolHandle,
    interface_name: &str,
    visited: &mut HashSet<SymbolHandle>,
) -> Option<SymbolHandle> {
    if !visited.insert(owner) {
        return None;
    }
    let owner_unit = &project.units[owner.unit.as_usize()];
    for implemented in owner_unit
        .implemented_interfaces
        .iter()
        .filter(|implemented| implemented.owner_symbol == owner.symbol)
    {
        let Some(interface_handle) = resolve_interface_symbol(
            project,
            lookup,
            owner_unit,
            &lookup.scope_indexes[owner_unit.unit_id.as_usize()],
            owner_unit.symbol(owner.symbol).scope,
            &implemented.interface_name,
        ) else {
            continue;
        };
        if implemented
            .interface_name
            .as_ref()
            .eq_ignore_ascii_case(interface_name)
        {
            return Some(interface_handle);
        }
        if let Some(found) = resolve_exposed_interface_handle_inner(
            project,
            lookup,
            interface_handle,
            interface_name,
            visited,
        ) {
            return Some(found);
        }
    }

    if owner_unit.symbol(owner.symbol).kind == SymbolKind::Class
        && let Some(superclass) =
            direct_superclass_handle(project, lookup, owner_unit, owner.symbol)
    {
        return resolve_exposed_interface_handle_inner(
            project,
            lookup,
            superclass,
            interface_name,
            visited,
        );
    }

    None
}

fn resolve_interface_member_path<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    interface_handle: SymbolHandle,
    path: &'a [crate::FieldAccessSegment],
) -> Option<(&'a crate::UnitAnalysis, &'a crate::ClassMemberData)> {
    let interface_unit = &project.units[interface_handle.unit.as_usize()];
    let (first, rest) = path.split_first()?;
    if rest.is_empty() {
        return interface_unit
            .class_member(interface_handle.symbol, first.name.as_ref())
            .map(|member| (interface_unit, member));
    }
    let nested =
        resolve_exposed_interface_handle(project, lookup, interface_handle, first.name.as_ref())?;
    resolve_interface_member_path(project, lookup, nested, rest)
}

fn resolve_fallback_qualified_class_member_in_hierarchy<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    class_unit: &'a crate::UnitAnalysis,
    class_symbol: SymbolId,
    interface_name: &str,
    member_name: &str,
) -> Option<(&'a crate::UnitAnalysis, &'a crate::ClassMemberData)> {
    let qualified_name = format!(
        "{}~{}",
        interface_name.to_ascii_lowercase(),
        member_name.to_ascii_lowercase()
    );
    resolve_class_member_in_hierarchy(project, lookup, class_unit, class_symbol, &qualified_name)
}

fn resolve_fallback_qualified_method_symbol_in_hierarchy(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    class_unit: &crate::UnitAnalysis,
    class_symbol: SymbolId,
    interface_name: &str,
    member_name: &str,
) -> Option<SymbolHandle> {
    let qualified_name = format!(
        "{}~{}",
        interface_name.to_ascii_lowercase(),
        member_name.to_ascii_lowercase()
    );
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
        if let Some(symbol) = unit.symbols.iter().find(|symbol| {
            symbol.kind == SymbolKind::Method
                && symbol.name.as_ref() == qualified_name
                && enclosing_class_owner(unit, symbol.scope) == Some(current.symbol)
        }) {
            return Some(SymbolHandle {
                unit: unit.unit_id,
                symbol: symbol.id,
            });
        }
        current = direct_superclass_handle(project, lookup, unit, current.symbol)?;
    }
}

fn resolve_fallback_qualified_redefinition_member_in_hierarchy<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    class_unit: &'a crate::UnitAnalysis,
    class_symbol: SymbolId,
    interface_name: &str,
    member_name: &str,
) -> Option<(&'a crate::UnitAnalysis, &'a crate::ClassMemberData)> {
    let pattern_spaced = format!(
        "{} ~ {}",
        interface_name.to_ascii_lowercase(),
        member_name.to_ascii_lowercase()
    );
    let pattern_compact = pattern_spaced.replace(" ~ ", "~");
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
        if let Some(member) = unit.class_members.iter().find(|member| {
            member.class_symbol == current.symbol
                && member.kind == ClassMemberKind::Method
                && member.name.as_ref() == interface_name
                && {
                    let signature = member.signature.to_ascii_lowercase();
                    signature.contains(&pattern_spaced) || signature.contains(&pattern_compact)
                }
        }) {
            return Some((unit, member));
        }
        current = direct_superclass_handle(project, lookup, unit, current.symbol)?;
    }
}

fn class_hierarchy_supports_named_interface_member(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    class_unit: &crate::UnitAnalysis,
    class_symbol: SymbolId,
    interface_name: &str,
    member_name: &str,
) -> bool {
    resolve_fallback_qualified_class_member_in_hierarchy(
        project,
        lookup,
        class_unit,
        class_symbol,
        interface_name,
        member_name,
    )
    .is_some()
        || resolve_fallback_qualified_method_symbol_in_hierarchy(
            project,
            lookup,
            class_unit,
            class_symbol,
            interface_name,
            member_name,
        )
        .is_some()
        || resolve_fallback_qualified_redefinition_member_in_hierarchy(
            project,
            lookup,
            class_unit,
            class_symbol,
            interface_name,
            member_name,
        )
        .is_some()
}

fn resolve_qualified_interface_method_context<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &'a crate::UnitAnalysis,
    scope: ScopeId,
) -> Option<(&'a crate::UnitAnalysis, &'a crate::ClassMemberData)> {
    let method_symbol = enclosing_method_owner(unit, scope)?;
    let method_name = unit.symbol(method_symbol).name.as_ref();
    let (interface_name, member_name) = method_name.split_once('~')?;
    let class_symbol = enclosing_class_owner(unit, scope)?;
    let interface_handle = resolve_exposed_interface_handle(
        project,
        lookup,
        SymbolHandle {
            unit: unit.unit_id,
            symbol: class_symbol,
        },
        interface_name,
    )?;
    let interface_unit = &project.units[interface_handle.unit.as_usize()];
    let member = interface_unit.class_member(interface_handle.symbol, member_name)?;
    Some((interface_unit, member))
}

fn resolve_inherited_redefinition_method_context<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &'a crate::UnitAnalysis,
    scope: ScopeId,
) -> Option<(&'a crate::UnitAnalysis, &'a crate::ClassMemberData)> {
    let method_symbol = enclosing_method_owner(unit, scope)?;
    let method_name = unit.symbol(method_symbol).name.as_ref();
    if method_name.contains('~') {
        return None;
    }
    let class_symbol = enclosing_class_owner(unit, scope)?;
    let superclass = direct_superclass_handle(project, lookup, unit, class_symbol)?;
    let superclass_unit = &project.units[superclass.unit.as_usize()];
    resolve_class_member_in_hierarchy(
        project,
        lookup,
        superclass_unit,
        superclass.symbol,
        method_name,
    )
}

fn root_class_definition_handle_in_unit(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit_id: UnitId,
    class_name: &Arc<str>,
) -> Option<SymbolHandle> {
    let unit = project.units.get(unit_id.as_usize())?;
    lookup
        .per_unit_root_index
        .get(unit_id.as_usize())?
        .get(&(Namespace::Type, Arc::clone(class_name)))?
        .iter()
        .rev()
        .copied()
        .find(|&symbol_id| {
            unit.symbol(symbol_id).kind == SymbolKind::Class
                && unit.class_definition(symbol_id).is_some()
        })
        .map(|symbol| SymbolHandle {
            unit: unit_id,
            symbol,
        })
}

fn resolve_declared_method_context<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &'a crate::UnitAnalysis,
    scope: ScopeId,
) -> Option<(&'a crate::UnitAnalysis, &'a crate::ClassMemberData)> {
    let method_symbol = enclosing_method_owner(unit, scope)?;
    let method_name = unit.symbol(method_symbol).name.as_ref();
    if method_name.contains('~') {
        return None;
    }
    let class_symbol = enclosing_class_owner(unit, scope)?;
    let class_name = Arc::clone(&unit.symbol(class_symbol).name);
    let handle = if let Some(handle) =
        root_class_definition_handle_in_unit(project, lookup, unit.unit_id, &class_name)
    {
        handle
    } else {
        lookup
            .include_predecessors
            .get(unit.unit_id.as_usize())?
            .iter()
            .rev()
            .find_map(|&unit_id| {
                root_class_definition_handle_in_unit(project, lookup, unit_id, &class_name)
            })?
    };
    let method_unit = &project.units[handle.unit.as_usize()];
    let member = method_unit.class_member(handle.symbol, method_name)?;
    (member.kind == ClassMemberKind::Method && !member.parameters.is_empty())
        .then_some((method_unit, member))
}

fn inject_symbol_into_scope_index(
    scope_index: &mut ScopeIndex,
    scope: ScopeId,
    symbol_id: SymbolId,
    namespace: Namespace,
    name: Arc<str>,
) {
    scope_index[scope.as_usize()]
        .entry((namespace, name))
        .or_default()
        .push(symbol_id);
}

fn qualified_interface_method_scope_symbol_specs(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
) -> Vec<(ScopeId, crate::SymbolData)> {
    let method_scopes: Vec<_> = unit
        .scopes
        .iter()
        .filter(|scope| scope.kind == ScopeKind::Method)
        .map(|scope| scope.id)
        .collect();
    let mut out = Vec::new();

    for scope_id in method_scopes {
        let Some((_, member)) =
            resolve_qualified_interface_method_context(project, lookup, unit, scope_id)
        else {
            continue;
        };
        let member_is_static = member.is_static;
        let member_parameters = member.parameters.clone();
        let Some(_method_symbol) = enclosing_method_owner(unit, scope_id) else {
            continue;
        };
        let Some(class_symbol) = enclosing_class_owner(unit, scope_id) else {
            continue;
        };
        let class_name = Arc::clone(&unit.symbol(class_symbol).name);

        let has_me = unit.symbols.iter().any(|symbol| {
            symbol.scope == scope_id
                && symbol.kind == SymbolKind::Variable
                && symbol.name.as_ref() == "me"
        });
        if !member_is_static && !has_me {
            out.push((
                scope_id,
                crate::SymbolData {
                    id: SymbolId(0),
                    name: Arc::from("me"),
                    kind: SymbolKind::Variable,
                    scope: scope_id,
                    decl_range: 0..0,
                    structure: None,
                    declared_type: Some(FieldTypeRefData {
                        namespace: Namespace::Type,
                        is_ref: true,
                        base_name: class_name,
                        field_path: Vec::new(),
                    }),
                    type_clause_display: None,
                    value_clause_display: None,
                },
            ));
        }

        for param in &member_parameters {
            let has_param = unit.symbols.iter().any(|symbol| {
                symbol.scope == scope_id
                    && symbol.kind == SymbolKind::Parameter
                    && symbol.name == param.name
            });
            if has_param {
                continue;
            }
            out.push((
                scope_id,
                crate::SymbolData {
                    id: SymbolId(0),
                    name: Arc::clone(&param.name),
                    kind: SymbolKind::Parameter,
                    scope: scope_id,
                    decl_range: 0..0,
                    structure: None,
                    declared_type: param.declared_type.clone(),
                    type_clause_display: None,
                    value_clause_display: None,
                },
            ));
        }
    }
    out
}

fn inherited_redefinition_method_scope_symbol_specs(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
) -> Vec<(ScopeId, crate::SymbolData)> {
    let method_scopes: Vec<_> = unit
        .scopes
        .iter()
        .filter(|scope| scope.kind == ScopeKind::Method)
        .map(|scope| scope.id)
        .collect();
    let mut out = Vec::new();

    for scope_id in method_scopes {
        let Some((_, member)) =
            resolve_inherited_redefinition_method_context(project, lookup, unit, scope_id)
        else {
            continue;
        };
        for param in &member.parameters {
            let has_param = unit.symbols.iter().any(|symbol| {
                symbol.scope == scope_id
                    && symbol.kind == SymbolKind::Parameter
                    && symbol.name == param.name
            });
            if has_param {
                continue;
            }
            out.push((
                scope_id,
                crate::SymbolData {
                    id: SymbolId(0),
                    name: Arc::clone(&param.name),
                    kind: SymbolKind::Parameter,
                    scope: scope_id,
                    decl_range: 0..0,
                    structure: None,
                    declared_type: param.declared_type.clone(),
                    type_clause_display: None,
                    value_clause_display: None,
                },
            ));
        }
    }

    out
}

fn declared_method_scope_symbol_specs(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
) -> Vec<(ScopeId, crate::SymbolData)> {
    let method_scopes: Vec<_> = unit
        .scopes
        .iter()
        .filter(|scope| scope.kind == ScopeKind::Method)
        .map(|scope| scope.id)
        .collect();
    let mut out = Vec::new();

    for scope_id in method_scopes {
        let Some((_, member)) = resolve_declared_method_context(project, lookup, unit, scope_id)
        else {
            continue;
        };
        for param in &member.parameters {
            let has_param = unit.symbols.iter().any(|symbol| {
                symbol.scope == scope_id
                    && symbol.kind == SymbolKind::Parameter
                    && symbol.name == param.name
            });
            if has_param {
                continue;
            }
            out.push((
                scope_id,
                crate::SymbolData {
                    id: SymbolId(0),
                    name: Arc::clone(&param.name),
                    kind: SymbolKind::Parameter,
                    scope: scope_id,
                    decl_range: 0..0,
                    structure: None,
                    declared_type: param.declared_type.clone(),
                    type_clause_display: param.type_clause_display.clone(),
                    value_clause_display: None,
                },
            ));
        }
    }

    out
}

fn resolve_event_handler_method_context<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &'a crate::UnitAnalysis,
    scope: ScopeId,
) -> Option<(&'a crate::UnitAnalysis, &'a crate::ClassMemberData)> {
    let method_symbol = enclosing_method_owner(unit, scope)?;
    let method_name = unit.symbol(method_symbol).name.as_ref();
    let class_symbol = enclosing_class_owner(unit, scope)?;
    let (member_unit, member) =
        resolve_class_member_in_hierarchy(project, lookup, unit, class_symbol, method_name)?;
    member
        .signature
        .split_ascii_whitespace()
        .any(|part| part.eq_ignore_ascii_case("for"))
        .then_some((member_unit, member))
        .filter(|(_, member)| member.signature.to_ascii_lowercase().contains("for event"))
}

fn event_handler_method_scope_symbol_specs(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
) -> Vec<(ScopeId, crate::SymbolData)> {
    let method_scopes: Vec<_> = unit
        .scopes
        .iter()
        .filter(|scope| scope.kind == ScopeKind::Method)
        .map(|scope| scope.id)
        .collect();
    let mut out = Vec::new();

    for scope_id in method_scopes {
        let Some((_, member)) =
            resolve_event_handler_method_context(project, lookup, unit, scope_id)
        else {
            continue;
        };
        for param in &member.parameters {
            let has_param = unit.symbols.iter().any(|symbol| {
                symbol.scope == scope_id
                    && symbol.kind == SymbolKind::Parameter
                    && symbol.name == param.name
            });
            if has_param {
                continue;
            }
            out.push((
                scope_id,
                crate::SymbolData {
                    id: SymbolId(0),
                    name: Arc::clone(&param.name),
                    kind: SymbolKind::Parameter,
                    scope: scope_id,
                    decl_range: 0..0,
                    structure: None,
                    declared_type: param.declared_type.clone(),
                    type_clause_display: param.type_clause_display.clone(),
                    value_clause_display: None,
                },
            ));
        }
    }

    out
}

fn loop_field_scope_symbol_specs<'a>(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    contexts: impl IntoIterator<Item = LoopFieldContextView<'a>>,
) -> Vec<(ScopeId, crate::SymbolData)> {
    let mut out = Vec::new();
    let mut seen: HashSet<(u32, Arc<str>)> = HashSet::new();

    for context in contexts {
        let mut push_fields =
            |scope: ScopeId, fields_unit: &crate::UnitAnalysis, structure_id: StructureId| {
                for field in structure_field_infos_project(
                    project,
                    lookup,
                    scope_indexes,
                    fields_unit,
                    scope,
                    structure_id,
                ) {
                    let name = Arc::clone(&field.name);
                    if !seen.insert((scope.0, Arc::clone(&name))) {
                        continue;
                    }
                    if unit.symbols.iter().any(|symbol| {
                        symbol.scope == scope
                            && symbol.kind.occupies(Namespace::Value)
                            && symbol.name == name
                    }) {
                        continue;
                    }

                    out.push((
                        scope,
                        crate::SymbolData {
                            id: SymbolId(0),
                            name,
                            kind: crate::SymbolKind::Field,
                            scope,
                            decl_range: loop_field_synthetic_decl_range(
                                unit.unit_id,
                                &field,
                                context.range,
                            ),
                            structure: match field.shape {
                                StructureFieldShape::Structured { structure } => Some(structure),
                                StructureFieldShape::Scalar => None,
                            },
                            declared_type: field.type_ref.clone(),
                            type_clause_display: None,
                            value_clause_display: None,
                        },
                    ));
                }
            };

        if let Some((fields_unit, structure_id)) =
            resolve_loop_field_source_structure(project, lookup, unit, scope_indexes, context)
        {
            push_fields(context.scope, fields_unit, structure_id);
        }
        if let Some((fields_unit, structure_id)) = context.target_access.and_then(|access| {
            resolve_field_access_structure(project, lookup, unit, scope_indexes, access)
        }) {
            push_fields(context.scope, fields_unit, structure_id);
        }
    }

    out
}

fn loop_where_scope_symbol_specs(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
) -> Vec<(ScopeId, crate::SymbolData)> {
    loop_field_scope_symbol_specs(
        project,
        lookup,
        unit,
        scope_indexes,
        unit.loop_where_field_contexts
            .iter()
            .map(|context| LoopFieldContextView {
                scope: context.scope,
                range: &context.range,
                source_access: &context.source_access,
                target_access: context.target_access.as_ref(),
            }),
    )
}

fn loop_at_scope_symbol_specs(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
) -> Vec<(ScopeId, crate::SymbolData)> {
    loop_field_scope_symbol_specs(
        project,
        lookup,
        unit,
        scope_indexes,
        unit.loop_at_field_contexts
            .iter()
            .map(|context| LoopFieldContextView {
                scope: context.scope,
                range: &context.range,
                source_access: &context.source_access,
                target_access: context.target_access.as_ref(),
            }),
    )
}

fn loop_field_synthetic_decl_range(
    unit_id: UnitId,
    field: &crate::StructureFieldInfo,
    context_range: &TextRange,
) -> TextRange {
    if field.decl_unit == unit_id {
        field
            .decl_range
            .clone()
            .unwrap_or(context_range.start..context_range.start)
    } else {
        context_range.start..context_range.start
    }
}

#[cfg(test)]
fn loop_where_synthetic_decl_range(
    unit_id: UnitId,
    field: &crate::StructureFieldInfo,
    context_range: &TextRange,
) -> TextRange {
    loop_field_synthetic_decl_range(unit_id, field, context_range)
}

fn resolve_class_type_symbol_in_hierarchy(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    class_unit: &crate::UnitAnalysis,
    class_symbol: SymbolId,
    type_name: &str,
) -> Option<SymbolHandle> {
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
        if let Some(symbol_id) = unit.symbols.iter().find_map(|symbol| {
            (symbol.name.as_ref() == type_name
                && symbol.kind == SymbolKind::TypeDef
                && unit.scope(symbol.scope).kind == ScopeKind::Class
                && unit.scope(symbol.scope).owner == Some(current.symbol))
            .then_some(symbol.id)
        }) {
            return Some(SymbolHandle {
                unit: current.unit,
                symbol: symbol_id,
            });
        }
        current = direct_superclass_handle(project, lookup, unit, current.symbol)?;
    }
}

fn resolve_class_selector_base<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    _unit: &crate::UnitAnalysis,
    access: &crate::FieldAccess,
    base_handle: SymbolHandle,
) -> Option<(&'a crate::UnitAnalysis, SymbolId, bool)> {
    let base_unit = &project.units[base_handle.unit.as_usize()];
    let base_symbol = base_unit.symbol(base_handle.symbol);
    if access.base_namespace == Namespace::Type && base_symbol.kind == SymbolKind::Class {
        return Some((base_unit, base_handle.symbol, true));
    }
    if access.base_namespace != Namespace::Value {
        return None;
    }
    let declared_type = base_symbol.declared_type.as_ref()?;
    if !declared_type.is_ref || !declared_type.field_path.is_empty() {
        return None;
    }
    let class_handle =
        resolve_type_owner_symbol(project, lookup, base_unit, &declared_type.base_name)?;
    let class_unit = &project.units[class_handle.unit.as_usize()];
    Some((class_unit, class_handle.symbol, false))
}

fn split_leading_deref(access: &crate::FieldAccess) -> (bool, &[crate::FieldAccessSegment]) {
    if access
        .field_path
        .first()
        .is_some_and(|segment| segment.is_deref())
    {
        return (true, &access.field_path[1..]);
    }
    (false, &access.field_path)
}

fn dereference_field_metadata(
    unit: &crate::UnitAnalysis,
    scope_index: &ScopeIndex,
    scope: ScopeId,
    structure: Option<StructureId>,
    declared_type: Option<FieldTypeRefData>,
) -> Option<(Option<StructureId>, Option<FieldTypeRefData>)> {
    let type_ref = declared_type?;
    if !type_ref.is_ref {
        return None;
    }
    let structure = structure.or_else(|| {
        if type_ref.namespace != Namespace::Type || !type_ref.field_path.is_empty() {
            return None;
        }
        resolve_symbol_in_scope_chain(
            unit,
            scope_index,
            scope,
            Namespace::Type,
            &type_ref.base_name,
        )
        .and_then(|symbol_id| unit.symbol(symbol_id).structure)
    });
    Some((
        structure,
        Some(FieldTypeRefData {
            namespace: type_ref.namespace,
            is_ref: false,
            base_name: type_ref.base_name,
            field_path: type_ref.field_path,
        }),
    ))
}

fn normalize_field_metadata(
    unit: &crate::UnitAnalysis,
    scope_index: &ScopeIndex,
    scope: ScopeId,
    mut structure: Option<StructureId>,
    mut declared_type: Option<FieldTypeRefData>,
) -> (Option<StructureId>, Option<FieldTypeRefData>) {
    for _ in 0..8 {
        if structure.is_some() {
            break;
        }
        let Some(type_ref) = declared_type.as_ref() else {
            break;
        };
        if type_ref.namespace != Namespace::Type
            || type_ref.is_ref
            || !type_ref.field_path.is_empty()
        {
            break;
        }
        let Some(symbol_id) = resolve_symbol_in_scope_chain(
            unit,
            scope_index,
            scope,
            Namespace::Type,
            &type_ref.base_name,
        ) else {
            break;
        };
        let symbol = unit.symbol(symbol_id);
        if symbol.structure.is_none() && symbol.declared_type.is_none() {
            break;
        }
        structure = symbol.structure;
        declared_type = symbol.declared_type.clone();
    }
    (structure, declared_type)
}

fn normalize_field_metadata_project<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    scope_indexes: &[ScopeIndex],
    unit: &'a crate::UnitAnalysis,
    scope: ScopeId,
    mut structure: Option<StructureId>,
    mut declared_type: Option<FieldTypeRefData>,
) -> (
    &'a crate::UnitAnalysis,
    Option<StructureId>,
    Option<FieldTypeRefData>,
) {
    let mut current_unit = unit;
    for _ in 0..8 {
        if structure.is_some() {
            break;
        }
        let Some(type_ref) = declared_type.as_ref() else {
            break;
        };
        if type_ref.namespace != Namespace::Type
            || type_ref.is_ref
            || !type_ref.field_path.is_empty()
        {
            break;
        }
        let Some(handle) = resolve_type_like_symbol_handle(
            project,
            lookup,
            current_unit,
            scope_indexes,
            scope_for_unit(current_unit, scope),
            type_ref,
        ) else {
            break;
        };
        let symbol_unit = &project.units[handle.unit.as_usize()];
        let symbol = symbol_unit.symbol(handle.symbol);
        if symbol.structure.is_none() && symbol.declared_type.is_none() {
            break;
        }
        current_unit = symbol_unit;
        structure = symbol.structure;
        declared_type = symbol.declared_type.clone();
    }
    (current_unit, structure, declared_type)
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

fn perform_section_to_form_section(section: PerformParameterSection) -> FormParameterSection {
    match section {
        PerformParameterSection::Tables => FormParameterSection::Tables,
        PerformParameterSection::Using => FormParameterSection::Using,
        PerformParameterSection::Changing => FormParameterSection::Changing,
    }
}

fn form_parameter_for_perform_argument<'a>(
    parameters: &'a [FormParameterData],
    argument: &crate::PerformArgumentData,
) -> Option<&'a FormParameterData> {
    parameters
        .iter()
        .filter(|parameter| parameter.section == perform_section_to_form_section(argument.section))
        .nth(argument.ordinal_in_section)
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

fn type_fact_label(fact: &TypeFactData) -> String {
    if let Some(display) = fact.type_clause_display.as_ref() {
        return display.to_string();
    }
    if let Some(type_ref) = fact.declared_type.as_ref() {
        if type_ref.is_ref {
            return format!("REF TO {}", type_ref.base_name);
        }
        return type_ref.base_name.to_string();
    }
    if fact.structure.is_some() {
        return "structure".to_string();
    }
    "value".to_string()
}

enum MoveCorrespondingOperand<'a> {
    Structure(&'a crate::UnitAnalysis, StructureId),
    Table(TypeFactData),
    Dynamic,
    Other,
    Unknown,
}

fn move_corresponding_operand<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &'a crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    scope: ScopeId,
    fact: &TypeFactData,
    depth: usize,
) -> MoveCorrespondingOperand<'a> {
    if !fact.is_known() || depth >= 8 {
        return MoveCorrespondingOperand::Unknown;
    }
    if fact
        .declared_type
        .as_ref()
        .is_some_and(symbol_is_generic_dynamic_type)
    {
        return MoveCorrespondingOperand::Dynamic;
    }
    if fact.table_line.is_some()
        || fact
            .type_clause_display
            .as_deref()
            .is_some_and(type_display_suggests_internal_table)
    {
        return MoveCorrespondingOperand::Table(
            fact.table_line.as_deref().cloned().unwrap_or_default(),
        );
    }
    if let Some((structure_unit, structure)) =
        move_corresponding_structure(project, lookup, unit, scope_indexes, scope, fact, depth)
    {
        return MoveCorrespondingOperand::Structure(structure_unit, structure);
    }
    MoveCorrespondingOperand::Other
}

fn move_corresponding_structure<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &'a crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    scope: ScopeId,
    fact: &TypeFactData,
    depth: usize,
) -> Option<(&'a crate::UnitAnalysis, StructureId)> {
    if let Some(structure) = fact.structure {
        return Some((unit, structure));
    }
    let type_ref = fact.declared_type.as_ref()?;
    if type_ref.is_ref || !type_ref.field_path.is_empty() {
        return None;
    }
    let handle =
        resolve_type_like_symbol_handle(project, lookup, unit, scope_indexes, scope, type_ref)?;
    let resolved_unit = &project.units[handle.unit.as_usize()];
    let symbol = resolved_unit.symbol(handle.symbol);
    if symbol_type_clause_suggests_internal_table(symbol) {
        return None;
    }
    if let Some(structure) = symbol.structure {
        return Some((resolved_unit, structure));
    }
    let next = TypeFactData {
        structure: None,
        declared_type: symbol.declared_type.clone(),
        type_clause_display: symbol.type_clause_display.clone(),
        table_line: None,
    };
    move_corresponding_structure(
        project,
        lookup,
        resolved_unit,
        scope_indexes,
        scope_for_unit(resolved_unit, scope),
        &next,
        depth + 1,
    )
}

fn move_corresponding_field_fact(field: &crate::StructureFieldInfo) -> TypeFactData {
    TypeFactData {
        structure: match field.shape {
            StructureFieldShape::Structured { structure } => Some(structure),
            StructureFieldShape::Scalar => None,
        },
        declared_type: field.type_ref.clone(),
        type_clause_display: None,
        table_line: None,
    }
}

fn move_corresponding_operand_diagnostic(assignment: &crate::AssignmentSiteData) -> Diagnostic {
    Diagnostic {
        kind: DiagnosticKind::IncompatibleAssignmentType,
        range: assignment.range.clone(),
        message: format!(
            "MOVE-CORRESPONDING operands must both be structures or both be internal tables, got '{}' and '{}'",
            type_fact_label(&assignment.rhs),
            type_fact_label(&assignment.lhs)
        ),
    }
}

fn validate_move_corresponding_components(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    scope_indexes: &[ScopeIndex],
    target_unit: &crate::UnitAnalysis,
    source_unit: &crate::UnitAnalysis,
    scope: ScopeId,
    target_structure: StructureId,
    source_structure: StructureId,
    range: &TextRange,
    out: &mut Vec<Diagnostic>,
) {
    let source_fields = structure_field_infos_project(
        project,
        lookup,
        scope_indexes,
        source_unit,
        scope_for_unit(source_unit, scope),
        source_structure,
    );
    let source_by_name: HashMap<_, _> = source_fields
        .iter()
        .map(|field| (field.name.to_ascii_lowercase(), field))
        .collect();

    for target in structure_field_infos_project(
        project,
        lookup,
        scope_indexes,
        target_unit,
        scope_for_unit(target_unit, scope),
        target_structure,
    ) {
        let Some(source) = source_by_name.get(&target.name.to_ascii_lowercase()) else {
            continue;
        };
        validate_move_corresponding_component_pair(
            project,
            lookup,
            scope_indexes,
            &target,
            source,
            scope,
            range,
            out,
        );
    }
}

fn validate_move_corresponding_component_pair(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    scope_indexes: &[ScopeIndex],
    target: &crate::StructureFieldInfo,
    source: &crate::StructureFieldInfo,
    scope: ScopeId,
    range: &TextRange,
    out: &mut Vec<Diagnostic>,
) {
    let target_unit = &project.units[target.owner_unit.as_usize()];
    let source_unit = &project.units[source.owner_unit.as_usize()];
    let target_fact = move_corresponding_field_fact(target);
    let source_fact = move_corresponding_field_fact(source);
    match (
        move_corresponding_operand(
            project,
            lookup,
            target_unit,
            scope_indexes,
            scope_for_unit(target_unit, scope),
            &target_fact,
            0,
        ),
        move_corresponding_operand(
            project,
            lookup,
            source_unit,
            scope_indexes,
            scope_for_unit(source_unit, scope),
            &source_fact,
            0,
        ),
    ) {
        (
            MoveCorrespondingOperand::Structure(next_target_unit, next_target),
            MoveCorrespondingOperand::Structure(next_source_unit, next_source),
        ) => validate_move_corresponding_components(
            project,
            lookup,
            scope_indexes,
            next_target_unit,
            next_source_unit,
            scope,
            next_target,
            next_source,
            range,
            out,
        ),
        (MoveCorrespondingOperand::Structure(_, _), _)
        | (_, MoveCorrespondingOperand::Structure(_, _)) => {}
        _ if type_facts_compatibility(
            project,
            &lookup.type_fact_lookup,
            target_unit,
            &target_fact,
            source_unit,
            &source_fact,
        )
        .is_incompatible() =>
        {
            out.push(Diagnostic {
                kind: DiagnosticKind::IncompatibleAssignmentType,
                range: range.clone(),
                message: format!(
                    "MOVE-CORRESPONDING component '{}' target '{}' is incompatible with source '{}'",
                    target.name,
                    type_fact_label(&target_fact),
                    type_fact_label(&source_fact)
                ),
            });
        }
        _ => {}
    }
}

fn validate_move_corresponding_assignment(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    assignment: &crate::AssignmentSiteData,
) -> Vec<Diagnostic> {
    let target = move_corresponding_operand(
        project,
        lookup,
        unit,
        scope_indexes,
        assignment.scope,
        &assignment.lhs,
        0,
    );
    let source = move_corresponding_operand(
        project,
        lookup,
        unit,
        scope_indexes,
        assignment.scope,
        &assignment.rhs,
        0,
    );
    let mut out = Vec::new();
    match (target, source) {
        (
            MoveCorrespondingOperand::Structure(target_unit, target_structure),
            MoveCorrespondingOperand::Structure(source_unit, source_structure),
        ) => validate_move_corresponding_components(
            project,
            lookup,
            scope_indexes,
            target_unit,
            source_unit,
            assignment.scope,
            target_structure,
            source_structure,
            &assignment.range,
            &mut out,
        ),
        (
            MoveCorrespondingOperand::Table(target_line),
            MoveCorrespondingOperand::Table(source_line),
        ) => {
            validate_move_corresponding_table_lines(
                project,
                lookup,
                unit,
                scope_indexes,
                assignment,
                &target_line,
                &source_line,
                &mut out,
            );
        }
        (MoveCorrespondingOperand::Dynamic, _)
        | (_, MoveCorrespondingOperand::Dynamic)
        | (MoveCorrespondingOperand::Unknown, _)
        | (_, MoveCorrespondingOperand::Unknown) => {}
        _ => out.push(move_corresponding_operand_diagnostic(assignment)),
    }
    out
}

fn validate_move_corresponding_table_lines(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    assignment: &crate::AssignmentSiteData,
    target_line: &TypeFactData,
    source_line: &TypeFactData,
    out: &mut Vec<Diagnostic>,
) {
    match (
        move_corresponding_operand(
            project,
            lookup,
            unit,
            scope_indexes,
            assignment.scope,
            target_line,
            0,
        ),
        move_corresponding_operand(
            project,
            lookup,
            unit,
            scope_indexes,
            assignment.scope,
            source_line,
            0,
        ),
    ) {
        (
            MoveCorrespondingOperand::Structure(target_unit, target_structure),
            MoveCorrespondingOperand::Structure(source_unit, source_structure),
        ) => validate_move_corresponding_components(
            project,
            lookup,
            scope_indexes,
            target_unit,
            source_unit,
            assignment.scope,
            target_structure,
            source_structure,
            &assignment.range,
            out,
        ),
        (MoveCorrespondingOperand::Structure(_, _), _)
        | (_, MoveCorrespondingOperand::Structure(_, _))
        | (MoveCorrespondingOperand::Dynamic, _)
        | (_, MoveCorrespondingOperand::Dynamic)
        | (MoveCorrespondingOperand::Unknown, _)
        | (_, MoveCorrespondingOperand::Unknown) => {}
        _ if type_facts_compatibility(
            project,
            &lookup.type_fact_lookup,
            unit,
            target_line,
            unit,
            source_line,
        )
        .is_incompatible() =>
        {
            out.push(Diagnostic {
                kind: DiagnosticKind::IncompatibleAssignmentType,
                range: assignment.range.clone(),
                message: format!(
                    "MOVE-CORRESPONDING table line target '{}' is incompatible with source '{}'",
                    type_fact_label(target_line),
                    type_fact_label(source_line)
                ),
            });
        }
        _ => {}
    }
}

fn form_parameter_type_fact(
    unit: &crate::UnitAnalysis,
    parameter: &FormParameterData,
) -> TypeFactData {
    let symbol = unit.symbol(parameter.symbol);
    TypeFactData {
        structure: symbol.structure,
        declared_type: symbol.declared_type.clone(),
        type_clause_display: symbol.type_clause_display.clone(),
        table_line: None,
    }
}

fn perform_argument_type_fact(
    unit: &crate::UnitAnalysis,
    argument: &crate::PerformArgumentData,
) -> Option<TypeFactData> {
    unit.expression_fact_at_offset(argument.range.end.checked_sub(1)?)
        .map(|fact| fact.type_fact.clone())
}

fn method_parameter_type_fact(parameter: &crate::ClassMemberParameterData) -> TypeFactData {
    TypeFactData {
        structure: None,
        declared_type: parameter.declared_type.clone(),
        type_clause_display: parameter.type_clause_display.clone(),
        table_line: None,
    }
}

fn function_module_parameter_type_fact(parameter: &FunctionModuleParameterData) -> TypeFactData {
    let declared_type = (!parameter.is_untyped)
        .then(|| parameter.declared_type.clone())
        .flatten();
    let type_clause_display = if parameter.section == FunctionModuleParameterSection::Tables {
        parameter.type_clause_display.as_ref().map(|display| {
            if display.as_ref().to_ascii_uppercase().contains(" TABLE OF ") {
                Arc::clone(display)
            } else {
                Arc::from(format!("STANDARD TABLE OF {display}"))
            }
        })
    } else {
        parameter.type_clause_display.clone()
    };
    let mut fact = TypeFactData {
        structure: None,
        declared_type: declared_type.clone(),
        type_clause_display,
        table_line: None,
    };
    if parameter.section == FunctionModuleParameterSection::Tables {
        fact.table_line = Some(Box::new(TypeFactData {
            structure: None,
            declared_type,
            type_clause_display: None,
            table_line: None,
        }));
    }
    fact
}

fn call_section_matches_function_parameter(
    section: Option<crate::NamedArgumentSection>,
    parameter: &FunctionModuleParameterData,
) -> bool {
    matches!(
        (section, parameter.section),
        (
            Some(crate::NamedArgumentSection::Exporting),
            FunctionModuleParameterSection::Importing
        ) | (
            Some(crate::NamedArgumentSection::Importing),
            FunctionModuleParameterSection::Exporting
        ) | (
            Some(crate::NamedArgumentSection::Changing),
            FunctionModuleParameterSection::Changing
        ) | (
            Some(crate::NamedArgumentSection::Tables),
            FunctionModuleParameterSection::Tables
        )
    )
}

fn function_module_parameter_is_required(parameter: &FunctionModuleParameterData) -> bool {
    !parameter.is_optional
        && !parameter.has_default_value
        && matches!(
            parameter.section,
            FunctionModuleParameterSection::Importing
                | FunctionModuleParameterSection::Changing
                | FunctionModuleParameterSection::Tables
        )
}

fn resolve_type_owner_symbol(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    preferred_unit: &crate::UnitAnalysis,
    name: &Arc<str>,
) -> Option<SymbolHandle> {
    if let Some(handle) = project.visible_type_owner_handle_with_predecessors(
        preferred_unit.unit_id,
        name,
        &lookup.include_predecessors,
    ) {
        return Some(handle);
    }
    root_symbol_handle_matching(
        project,
        lookup,
        preferred_unit,
        Namespace::Type,
        name,
        |symbol| matches!(symbol.kind, SymbolKind::Class | SymbolKind::Interface),
    )
}

fn resolve_method_target_handle(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_index: &ScopeIndex,
    scope: ScopeId,
    target: &NamedArgumentTarget,
) -> Option<SymbolHandle> {
    match target {
        NamedArgumentTarget::Constructor { type_name } => {
            resolve_type_owner_symbol(project, lookup, unit, type_name)
        }
        NamedArgumentTarget::ImplicitMethod { .. } => {
            enclosing_class_owner(unit, scope).map(|symbol| SymbolHandle {
                unit: unit.unit_id,
                symbol,
            })
        }
        NamedArgumentTarget::Method {
            base_namespace,
            base_name,
            ..
        } => match base_namespace {
            Namespace::Type => resolve_type_owner_symbol(project, lookup, unit, base_name),
            Namespace::Value if base_name.as_ref().eq_ignore_ascii_case("super") => {
                let class_symbol = enclosing_class_owner(unit, scope)?;
                let inheritance = unit.class_superclass(class_symbol)?;
                resolve_type_owner_symbol(project, lookup, unit, &inheritance.superclass_name)
            }
            Namespace::Value => {
                let handle = resolve_symbol_handle_in_scope_or_includes(
                    project,
                    lookup,
                    unit,
                    scope_index,
                    scope,
                    Namespace::Value,
                    base_name,
                )?;
                let target_unit = &project.units[handle.unit.as_usize()];
                let declared_type = target_unit.symbol(handle.symbol).declared_type.as_ref()?;
                if !declared_type.is_ref || declared_type.namespace != Namespace::Type {
                    return None;
                }
                resolve_type_owner_symbol(project, lookup, target_unit, &declared_type.base_name)
            }
            Namespace::Routine => None,
        },
        NamedArgumentTarget::Event { .. } => None,
        NamedArgumentTarget::Function { .. }
        | NamedArgumentTarget::Report { .. }
        | NamedArgumentTarget::Routine { .. } => None,
    }
}

fn resolve_call_target_member<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &'a crate::UnitAnalysis,
    scope_index: &ScopeIndex,
    call_site: &crate::CallSiteData,
) -> Option<(&'a crate::UnitAnalysis, &'a crate::ClassMemberData)> {
    let handle = resolve_method_target_handle(
        project,
        lookup,
        unit,
        scope_index,
        call_site.scope,
        &call_site.target,
    )?;
    let target_unit = &project.units[handle.unit.as_usize()];
    let method_name = match &call_site.target {
        NamedArgumentTarget::Constructor { .. } => "constructor",
        NamedArgumentTarget::ImplicitMethod { method_name } => method_name.as_ref(),
        NamedArgumentTarget::Method { method_name, .. } => method_name.as_ref(),
        NamedArgumentTarget::Event { .. } => return None,
        NamedArgumentTarget::Function { .. }
        | NamedArgumentTarget::Report { .. }
        | NamedArgumentTarget::Routine { .. } => return None,
    };
    if target_unit.symbol(handle.symbol).kind == SymbolKind::Interface {
        return target_unit
            .class_member(handle.symbol, method_name)
            .map(|member| (target_unit, member));
    }
    resolve_class_member_in_hierarchy(project, lookup, target_unit, handle.symbol, method_name)
        .or_else(|| {
            target_unit
                .class_member(handle.symbol, method_name)
                .map(|member| (target_unit, member))
        })
}

fn call_section_matches_event_parameter(
    call_section: Option<crate::NamedArgumentSection>,
    parameter_section: crate::MethodParameterSection,
) -> bool {
    matches!(
        (call_section, parameter_section),
        (
            None | Some(crate::NamedArgumentSection::Exporting),
            crate::MethodParameterSection::Exporting
        )
    )
}

fn event_parameter_is_required(parameter: &crate::ClassMemberParameterData) -> bool {
    !parameter.is_optional && parameter.section == crate::MethodParameterSection::Exporting
}

fn resolve_call_target_event_member<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &'a crate::UnitAnalysis,
    call_site: &crate::CallSiteData,
) -> Option<(&'a crate::UnitAnalysis, &'a crate::ClassMemberData)> {
    let NamedArgumentTarget::Event {
        qualifier,
        event_name,
    } = &call_site.target
    else {
        return None;
    };
    let class_symbol = enclosing_class_owner(unit, call_site.scope)?;
    resolve_class_event_in_hierarchy(
        project,
        lookup,
        SymbolHandle {
            unit: unit.unit_id,
            symbol: class_symbol,
        },
        qualifier.as_deref(),
        event_name.as_ref(),
    )
}

fn resolve_class_event_in_hierarchy<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    owner: SymbolHandle,
    qualifier: Option<&str>,
    event_name: &str,
) -> Option<(&'a crate::UnitAnalysis, &'a crate::ClassMemberData)> {
    resolve_class_event_in_hierarchy_inner(
        project,
        lookup,
        owner,
        qualifier,
        event_name,
        &mut HashSet::new(),
    )
}

fn resolve_class_event_in_hierarchy_inner<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    owner: SymbolHandle,
    qualifier: Option<&str>,
    event_name: &str,
    visited: &mut HashSet<(SymbolHandle, Option<Arc<str>>, Arc<str>)>,
) -> Option<(&'a crate::UnitAnalysis, &'a crate::ClassMemberData)> {
    let key = (
        owner,
        qualifier.map(Arc::<str>::from),
        Arc::<str>::from(event_name),
    );
    if !visited.insert(key) {
        return None;
    }

    let direct_owner = if let Some(interface_name) = qualifier {
        resolve_exposed_interface_handle(project, lookup, owner, interface_name)?
    } else {
        owner
    };
    let direct_owner_unit = &project.units[direct_owner.unit.as_usize()];
    if let Some(member) = direct_owner_unit
        .class_member(direct_owner.symbol, event_name)
        .filter(|member| member.kind == ClassMemberKind::Event)
    {
        return Some((direct_owner_unit, member));
    }

    if let Some(alias) = direct_owner_unit.member_aliases.iter().find(|alias| {
        alias.owner_symbol == direct_owner.symbol && alias.alias_name.as_ref() == event_name
    }) {
        return resolve_class_event_in_hierarchy_inner(
            project,
            lookup,
            direct_owner,
            Some(alias.target_interface_name.as_ref()),
            alias.target_member_name.as_ref(),
            visited,
        );
    }

    if qualifier.is_none() {
        for implemented in direct_owner_unit
            .implemented_interfaces
            .iter()
            .filter(|implemented| implemented.owner_symbol == direct_owner.symbol)
        {
            let Some(interface_handle) = resolve_exposed_interface_handle(
                project,
                lookup,
                direct_owner,
                implemented.interface_name.as_ref(),
            ) else {
                continue;
            };
            if let Some(found) = resolve_class_event_in_hierarchy_inner(
                project,
                lookup,
                interface_handle,
                None,
                event_name,
                visited,
            ) {
                return Some(found);
            }
        }
    }

    if qualifier.is_none()
        && direct_owner_unit.symbol(direct_owner.symbol).kind == SymbolKind::Class
        && let Some(superclass) =
            direct_superclass_handle(project, lookup, direct_owner_unit, direct_owner.symbol)
    {
        return resolve_class_event_in_hierarchy_inner(
            project, lookup, superclass, None, event_name, visited,
        );
    }

    None
}

fn resolve_call_target_function_module<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &'a crate::UnitAnalysis,
    scope_index: &ScopeIndex,
    call_site: &crate::CallSiteData,
) -> Option<(&'a crate::UnitAnalysis, &'a FunctionModuleData)> {
    let NamedArgumentTarget::Function { function_name } = &call_site.target else {
        return None;
    };
    let handle = resolve_symbol_handle_in_scope_or_includes(
        project,
        lookup,
        unit,
        scope_index,
        call_site.scope,
        Namespace::Routine,
        function_name,
    )
    .or_else(|| {
        root_symbol_handle_matching(
            project,
            lookup,
            unit,
            Namespace::Routine,
            function_name,
            |symbol| symbol.kind == SymbolKind::Module,
        )
    })?;
    let target_unit = &project.units[handle.unit.as_usize()];
    target_unit
        .function_module(handle.symbol)
        .map(|function_module| (target_unit, function_module))
}

fn workspace_root_defines_type_name(lookup: &ValidationLookup<'_>, name: &Arc<str>) -> bool {
    lookup
        .root_index
        .contains_key(&(Namespace::Type, Arc::clone(name)))
}

fn open_sql_source_has_workspace_type_definition(
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_index: &ScopeIndex,
    query_scope: ScopeId,
    name: &Arc<str>,
) -> bool {
    if resolve_symbol_in_scope_chain(unit, scope_index, query_scope, Namespace::Type, name)
        .is_some()
    {
        return true;
    }
    workspace_root_defines_type_name(lookup, name)
}

fn open_sql_source_symbol_handle(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_index: &ScopeIndex,
    query_scope: ScopeId,
    name: &Arc<str>,
) -> Option<SymbolHandle> {
    if let Some(symbol_id) =
        resolve_symbol_in_scope_chain(unit, scope_index, query_scope, Namespace::Type, name)
    {
        return Some(SymbolHandle {
            unit: unit.unit_id,
            symbol: symbol_id,
        });
    }

    root_symbol_handle_matching(project, lookup, unit, Namespace::Type, name, |symbol| {
        symbol.kind.occupies(Namespace::Type)
    })
}

fn open_sql_source_structure_for_name<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &'a crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    scope_index: &ScopeIndex,
    query_scope: ScopeId,
    name: &Arc<str>,
) -> Option<(&'a crate::UnitAnalysis, StructureId)> {
    let handle =
        open_sql_source_symbol_handle(project, lookup, unit, scope_index, query_scope, name)?;
    let source_unit = &project.units[handle.unit.as_usize()];
    resolve_symbol_structure_project(
        project,
        lookup,
        source_unit,
        scope_indexes,
        scope_for_unit(source_unit, query_scope),
        handle.symbol,
    )
}

fn validate_open_sql_sources(
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_index: &ScopeIndex,
) -> Vec<Diagnostic> {
    let query_scope_by_id: HashMap<usize, ScopeId> = unit
        .sql_queries
        .iter()
        .map(|query| (query.id, query.scope))
        .collect();
    let mut out = Vec::new();
    for sql_ref in &unit.sql_name_refs {
        if sql_ref.kind != SqlNameRefKind::Source {
            continue;
        }
        let Some(&query_scope) = query_scope_by_id.get(&sql_ref.query_id) else {
            continue;
        };
        if open_sql_source_has_workspace_type_definition(
            lookup,
            unit,
            scope_index,
            query_scope,
            &sql_ref.name,
        ) {
            continue;
        }
        out.push(Diagnostic {
            kind: DiagnosticKind::UnverifiedOpenSqlSource,
            range: sql_ref.range.clone(),
            message: format!(
                "Open SQL source '{}' is not verified against a SAP system (DDIC/repository lookup is not connected)",
                sql_ref.name
            ),
        });
    }
    out
}

fn sql_source_matches_qualifier(source: &SqlSourceData, qualifier: &Arc<str>) -> bool {
    source.alias.as_ref() == Some(qualifier) || source.name == *qualifier
}

fn sql_source_for_name_ref<'a>(
    unit: &'a crate::UnitAnalysis,
    sql_ref: &SqlNameRefData,
) -> Option<&'a SqlSourceData> {
    let sources: Vec<_> = unit
        .sql_sources
        .iter()
        .filter(|source| source.query_id == sql_ref.query_id)
        .collect();
    if let Some(qualifier) = sql_ref.qualifier.as_ref() {
        return sources
            .into_iter()
            .find(|source| sql_source_matches_qualifier(source, qualifier));
    }
    if sources.len() == 1 {
        return sources.first().copied();
    }
    None
}

fn validate_open_sql_fields(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    scope_index: &ScopeIndex,
) -> Vec<Diagnostic> {
    let mut out = Vec::new();
    let mut emitted = HashSet::<(usize, usize)>::new();

    for sql_ref in &unit.sql_name_refs {
        if !matches!(
            sql_ref.kind,
            SqlNameRefKind::Column | SqlNameRefKind::QualifiedColumn
        ) {
            continue;
        }
        let Some(source) = sql_source_for_name_ref(unit, sql_ref) else {
            continue;
        };
        let Some((source_unit, structure_id)) = open_sql_source_structure_for_name(
            project,
            lookup,
            unit,
            scope_indexes,
            scope_index,
            sql_ref.scope,
            &source.name,
        ) else {
            continue;
        };
        if resolve_structure_field_info_project(
            project,
            lookup,
            scope_indexes,
            source_unit,
            sql_ref.scope,
            structure_id,
            sql_ref.name.as_ref(),
        )
        .is_some()
            || structure_has_unresolved_proxy_include_fields(
                project,
                lookup,
                scope_indexes,
                source_unit,
                sql_ref.scope,
                structure_id,
            )
        {
            continue;
        }
        if !emitted.insert((sql_ref.range.start, sql_ref.range.end)) {
            continue;
        }
        out.push(Diagnostic {
            kind: DiagnosticKind::UnknownField,
            range: sql_ref.range.clone(),
            message: format!(
                "unknown Open SQL field '{}' for source '{}'",
                sql_ref.name, source.name
            ),
        });
    }

    out
}

fn validate_open_sql_order_by(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    scope_index: &ScopeIndex,
) -> OpenSqlOrderByValidation {
    let mut out = OpenSqlOrderByValidation::default();
    for query in &unit.sql_queries {
        let Some(order_by_range) = query.order_by_clause.as_ref() else {
            continue;
        };
        if query.is_single {
            out.diagnostics.push(Diagnostic {
                kind: DiagnosticKind::InvalidOpenSqlSyntax,
                range: order_by_range.clone(),
                message: "Open SQL ORDER BY cannot be used with SELECT SINGLE".to_string(),
            });
        }
        if query.for_all_entries_clause.is_some() && !query.order_by_primary_key {
            out.diagnostics.push(Diagnostic {
                kind: DiagnosticKind::InvalidOpenSqlSyntax,
                range: order_by_range.clone(),
                message: "Open SQL FOR ALL ENTRIES only permits ORDER BY PRIMARY KEY".to_string(),
            });
        }
        if !query.order_by_primary_key {
            continue;
        }
        if query.has_set_operators {
            out.diagnostics.push(Diagnostic {
                kind: DiagnosticKind::InvalidOpenSqlSyntax,
                range: order_by_range.clone(),
                message:
                    "Open SQL ORDER BY PRIMARY KEY cannot be used with UNION, INTERSECT, or EXCEPT"
                        .to_string(),
            });
        }

        let sources = open_sql_query_sources(unit, query.id);
        let single_source = sources
            .as_slice()
            .first()
            .copied()
            .filter(|_| sources.len() == 1)
            .filter(|source| source.source_kind == crate::SqlSourceKind::From);
        let Some(source) = single_source else {
            out.diagnostics.push(Diagnostic {
                kind: DiagnosticKind::InvalidOpenSqlSyntax,
                range: order_by_range.clone(),
                message: "Open SQL ORDER BY PRIMARY KEY requires a single static data source"
                    .to_string(),
            });
            continue;
        };

        let Some(primary_key_fields) = open_sql_primary_key_fields_for_source(
            project,
            lookup,
            unit,
            scope_indexes,
            scope_index,
            query.scope,
            source,
        ) else {
            continue;
        };
        out.resolved_primary_key_fields
            .push((query.id, primary_key_fields.clone()));

        if query.for_all_entries_clause.is_some()
            && !open_sql_projection_covers_primary_key_except_client(
                unit,
                query.id,
                source,
                &primary_key_fields,
            )
        {
            out.diagnostics.push(Diagnostic {
                kind: DiagnosticKind::InvalidOpenSqlSyntax,
                range: order_by_range.clone(),
                message: "Open SQL ORDER BY PRIMARY KEY with FOR ALL ENTRIES requires all non-client primary-key fields in the SELECT list".to_string(),
            });
        }
    }
    out
}

fn open_sql_query_sources(unit: &crate::UnitAnalysis, query_id: usize) -> Vec<&SqlSourceData> {
    unit.sql_sources
        .iter()
        .filter(|source| source.query_id == query_id)
        .collect()
}

fn open_sql_primary_key_fields_for_source(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    scope_index: &ScopeIndex,
    query_scope: ScopeId,
    source: &SqlSourceData,
) -> Option<Vec<Arc<str>>> {
    let (source_unit, structure_id) = open_sql_source_structure_for_name(
        project,
        lookup,
        unit,
        scope_indexes,
        scope_index,
        query_scope,
        &source.name,
    )?;
    let fields = structure_field_infos_project(
        project,
        lookup,
        scope_indexes,
        source_unit,
        scope_for_unit(source_unit, query_scope),
        structure_id,
    )
    .into_iter()
    .filter(|field| field.is_key)
    .map(|field| field.name)
    .collect::<Vec<_>>();
    (!fields.is_empty()).then_some(fields)
}

fn open_sql_projection_covers_primary_key_except_client(
    unit: &crate::UnitAnalysis,
    query_id: usize,
    source: &SqlSourceData,
    primary_key_fields: &[Arc<str>],
) -> bool {
    primary_key_fields
        .iter()
        .filter(|field| !is_client_column_name(field.as_ref()))
        .all(|field| open_sql_projection_covers_field(unit, query_id, source, field.as_ref()))
}

fn open_sql_projection_covers_field(
    unit: &crate::UnitAnalysis,
    query_id: usize,
    source: &SqlSourceData,
    field_name: &str,
) -> bool {
    unit.sql_projections
        .iter()
        .filter(|projection| projection.query_id == query_id)
        .any(|projection| match projection.kind {
            crate::SqlProjectionKind::Star => true,
            crate::SqlProjectionKind::QualifiedStar => projection
                .source_alias
                .as_ref()
                .is_some_and(|alias| sql_source_matches_qualifier(source, alias)),
            crate::SqlProjectionKind::Column => {
                projection
                    .name
                    .as_ref()
                    .is_some_and(|name| name.as_ref().eq_ignore_ascii_case(field_name))
                    && projection
                        .source_alias
                        .as_ref()
                        .is_none_or(|alias| sql_source_matches_qualifier(source, alias))
            }
            crate::SqlProjectionKind::Aggregate | crate::SqlProjectionKind::Expression => false,
        })
}

fn is_client_column_name(field_name: &str) -> bool {
    field_name.eq_ignore_ascii_case("mandt") || field_name.eq_ignore_ascii_case("client")
}

fn symbol_type_clause_suggests_internal_table(symbol: &crate::SymbolData) -> bool {
    symbol
        .type_clause_display
        .as_deref()
        .is_some_and(type_display_suggests_internal_table)
}

fn type_display_suggests_internal_table(display: &str) -> bool {
    let upper = display.to_ascii_uppercase();
    upper.contains("STANDARD TABLE")
        || upper.contains("HASHED TABLE")
        || upper.contains("SORTED TABLE")
        || upper.contains("ANY TABLE")
        || upper.contains("INDEX TABLE")
        || upper.contains("TABLE OF")
}

fn resolve_type_like_symbol_handle(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    scope: ScopeId,
    type_ref: &crate::FieldTypeRefData,
) -> Option<SymbolHandle> {
    let namespaces = if type_ref.namespace == Namespace::Value {
        [Namespace::Value, Namespace::Type]
    } else {
        [type_ref.namespace, type_ref.namespace]
    };

    for namespace in namespaces {
        if let Some(symbol) = resolve_symbol_in_scope_chain(
            unit,
            &scope_indexes[unit.unit_id.as_usize()],
            scope,
            namespace,
            &type_ref.base_name,
        ) {
            return Some(SymbolHandle {
                unit: unit.unit_id,
                symbol,
            });
        }

        if namespace == Namespace::Type
            && type_ref.field_path.is_empty()
            && let Some(handle) =
                resolve_class_scoped_type_handle(project, lookup, unit, scope, &type_ref.base_name)
        {
            return Some(handle);
        }

        if let Some(handle) = root_symbol_handle_matching(
            project,
            lookup,
            unit,
            namespace,
            &type_ref.base_name,
            |symbol| symbol.kind.namespaces().contains(&namespace),
        ) {
            return Some(handle);
        }
    }

    None
}

fn derive_ddic_include_field_name(type_name: &str) -> String {
    let tail = type_name
        .rsplit('/')
        .next()
        .unwrap_or(type_name)
        .trim()
        .to_ascii_lowercase();
    tail.strip_prefix("s_")
        .or_else(|| tail.strip_prefix("t_"))
        .unwrap_or(&tail)
        .to_string()
}

fn field_looks_like_ddic_proxy_include(field: &crate::StructureFieldInfo) -> bool {
    let Some(type_ref) = field.type_ref.as_ref() else {
        return false;
    };
    type_ref.namespace == Namespace::Type
        && !type_ref.is_ref
        && type_ref.field_path.is_empty()
        && field
            .name
            .as_ref()
            .eq_ignore_ascii_case(&derive_ddic_include_field_name(type_ref.base_name.as_ref()))
}

fn included_structure_for_proxy_field<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    scope_indexes: &[ScopeIndex],
    current_unit: &'a crate::UnitAnalysis,
    scope: ScopeId,
    field: &crate::StructureFieldInfo,
) -> Option<(&'a crate::UnitAnalysis, StructureId)> {
    let type_ref = field.type_ref.as_ref()?;
    let lookup_scope = if current_unit.scopes.get(scope.as_usize()).is_some() {
        scope
    } else {
        current_unit.root_scope
    };
    let handle = resolve_type_like_symbol_handle(
        project,
        lookup,
        current_unit,
        scope_indexes,
        lookup_scope,
        type_ref,
    )?;
    let resolved_unit = &project.units[handle.unit.as_usize()];
    resolve_symbol_structure_project(
        project,
        lookup,
        resolved_unit,
        scope_indexes,
        lookup_scope,
        handle.symbol,
    )
}

fn structure_has_unresolved_proxy_include_fields(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    scope_indexes: &[ScopeIndex],
    current_unit: &crate::UnitAnalysis,
    scope: ScopeId,
    structure_id: StructureId,
) -> bool {
    fn inner(
        project: &ProjectAnalysis,
        lookup: &ValidationLookup<'_>,
        scope_indexes: &[ScopeIndex],
        current_unit: &crate::UnitAnalysis,
        scope: ScopeId,
        structure_id: StructureId,
        seen: &mut HashSet<(u32, u32)>,
    ) -> bool {
        if !seen.insert((current_unit.unit_id.0, structure_id.0)) {
            return false;
        }
        for field in current_unit
            .semantic()
            .decls()
            .structure_field_infos(structure_id)
        {
            if !field_looks_like_ddic_proxy_include(&field) {
                continue;
            }
            let Some(type_ref) = field.type_ref.as_ref() else {
                continue;
            };
            let lookup_scope = if current_unit.scopes.get(scope.as_usize()).is_some() {
                scope
            } else {
                current_unit.root_scope
            };
            let Some(handle) = resolve_type_like_symbol_handle(
                project,
                lookup,
                current_unit,
                scope_indexes,
                lookup_scope,
                type_ref,
            ) else {
                return true;
            };
            let resolved_unit = &project.units[handle.unit.as_usize()];
            let Some((included_unit, included_structure)) = resolve_symbol_structure_project(
                project,
                lookup,
                resolved_unit,
                scope_indexes,
                lookup_scope,
                handle.symbol,
            ) else {
                continue;
            };
            let nested_scope = if included_unit.scopes.get(scope.as_usize()).is_some() {
                scope
            } else {
                included_unit.root_scope
            };
            if inner(
                project,
                lookup,
                scope_indexes,
                included_unit,
                nested_scope,
                included_structure,
                seen,
            ) {
                return true;
            }
        }
        false
    }

    inner(
        project,
        lookup,
        scope_indexes,
        current_unit,
        scope,
        structure_id,
        &mut HashSet::new(),
    )
}

fn resolve_structure_field_info_project<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    scope_indexes: &[ScopeIndex],
    current_unit: &'a crate::UnitAnalysis,
    scope: ScopeId,
    structure_id: StructureId,
    field_name: &str,
) -> Option<crate::StructureFieldInfo> {
    fn inner<'a>(
        project: &'a ProjectAnalysis,
        lookup: &ValidationLookup<'_>,
        scope_indexes: &[ScopeIndex],
        current_unit: &'a crate::UnitAnalysis,
        scope: ScopeId,
        structure_id: StructureId,
        field_name: &str,
        seen: &mut HashSet<(u32, u32)>,
    ) -> Option<crate::StructureFieldInfo> {
        if !seen.insert((current_unit.unit_id.0, structure_id.0)) {
            return None;
        }
        if let Some(field) = current_unit
            .semantic()
            .decls()
            .structure_field_info(structure_id, field_name)
        {
            return Some(field);
        }
        for field in current_unit
            .semantic()
            .decls()
            .structure_field_infos(structure_id)
        {
            if !field_looks_like_ddic_proxy_include(&field) {
                continue;
            }
            let Some((included_unit, included_structure)) = included_structure_for_proxy_field(
                project,
                lookup,
                scope_indexes,
                current_unit,
                scope,
                &field,
            ) else {
                continue;
            };
            let nested_scope = if included_unit.scopes.get(scope.as_usize()).is_some() {
                scope
            } else {
                included_unit.root_scope
            };
            if let Some(info) = inner(
                project,
                lookup,
                scope_indexes,
                included_unit,
                nested_scope,
                included_structure,
                field_name,
                seen,
            ) {
                return Some(info);
            }
        }
        None
    }

    let mut seen = HashSet::new();
    inner(
        project,
        lookup,
        scope_indexes,
        current_unit,
        scope,
        structure_id,
        field_name,
        &mut seen,
    )
}

fn structure_field_infos_project(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    scope_indexes: &[ScopeIndex],
    current_unit: &crate::UnitAnalysis,
    scope: ScopeId,
    structure_id: StructureId,
) -> Vec<crate::StructureFieldInfo> {
    fn collect(
        project: &ProjectAnalysis,
        lookup: &ValidationLookup<'_>,
        scope_indexes: &[ScopeIndex],
        current_unit: &crate::UnitAnalysis,
        scope: ScopeId,
        structure_id: StructureId,
        seen_structures: &mut HashSet<(u32, u32)>,
        seen_fields: &mut HashSet<Arc<str>>,
        out: &mut Vec<crate::StructureFieldInfo>,
    ) {
        if !seen_structures.insert((current_unit.unit_id.0, structure_id.0)) {
            return;
        }
        for field in current_unit
            .semantic()
            .decls()
            .structure_field_infos(structure_id)
        {
            if seen_fields.insert(Arc::clone(&field.name)) {
                out.push(field.clone());
            }
            if !field_looks_like_ddic_proxy_include(&field) {
                continue;
            }
            let Some((included_unit, included_structure)) = included_structure_for_proxy_field(
                project,
                lookup,
                scope_indexes,
                current_unit,
                scope,
                &field,
            ) else {
                continue;
            };
            let nested_scope = if included_unit.scopes.get(scope.as_usize()).is_some() {
                scope
            } else {
                included_unit.root_scope
            };
            collect(
                project,
                lookup,
                scope_indexes,
                included_unit,
                nested_scope,
                included_structure,
                seen_structures,
                seen_fields,
                out,
            );
        }
    }

    let mut out = Vec::new();
    let mut seen_structures = HashSet::new();
    let mut seen_fields = HashSet::new();
    collect(
        project,
        lookup,
        scope_indexes,
        current_unit,
        scope,
        structure_id,
        &mut seen_structures,
        &mut seen_fields,
        &mut out,
    );
    out
}

fn resolve_symbol_structure_project<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &'a crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    scope: ScopeId,
    symbol_id: SymbolId,
) -> Option<(&'a crate::UnitAnalysis, StructureId)> {
    let mut current_unit = unit;
    let mut current_symbol_id = symbol_id;
    let mut seen = HashSet::new();
    for _ in 0..8 {
        let symbol = current_unit.symbol(current_symbol_id);
        if let Some(structure_id) = symbol.structure {
            return Some((current_unit, structure_id));
        }
        let type_ref = symbol.declared_type.as_ref()?;
        let handle = resolve_type_like_symbol_handle(
            project,
            lookup,
            current_unit,
            scope_indexes,
            scope,
            type_ref,
        )?;
        if !seen.insert((handle.unit.0, handle.symbol.0)) {
            return None;
        }
        current_unit = &project.units[handle.unit.as_usize()];
        current_symbol_id = handle.symbol;
    }
    None
}

fn resolve_loop_field_source_structure<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &'a crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    context: LoopFieldContextView<'_>,
) -> Option<(&'a crate::UnitAnalysis, StructureId)> {
    if context.source_access.base_namespace != Namespace::Value {
        return None;
    }
    let scope_index = &scope_indexes[unit.unit_id.as_usize()];
    let base_handle = resolve_field_access_base_symbol(
        project,
        lookup,
        unit,
        scope_index,
        context.source_access,
    )?;
    let base_unit = &project.units[base_handle.unit.as_usize()];
    let (current_unit, mut current_structure) = resolve_symbol_structure_project(
        project,
        lookup,
        base_unit,
        scope_indexes,
        scope_for_unit(base_unit, context.scope),
        base_handle.symbol,
    )?;
    if context.source_access.field_path.is_empty() {
        return Some((current_unit, current_structure));
    }

    for (idx, segment) in context.source_access.field_path.iter().enumerate() {
        if segment.is_deref() {
            return None;
        }
        let field = resolve_structure_field_info_project(
            project,
            lookup,
            scope_indexes,
            current_unit,
            context.scope,
            current_structure,
            segment.name.as_ref(),
        )?;
        if idx + 1 == context.source_access.field_path.len() {
            if let Some(type_ref) = field.type_ref.as_ref() {
                let handle = resolve_type_like_symbol_handle(
                    project,
                    lookup,
                    current_unit,
                    scope_indexes,
                    context.scope,
                    type_ref,
                )?;
                let resolved_unit = &project.units[handle.unit.as_usize()];
                return resolve_symbol_structure_project(
                    project,
                    lookup,
                    resolved_unit,
                    scope_indexes,
                    context.scope,
                    handle.symbol,
                );
            }
            return match field.shape {
                StructureFieldShape::Structured { structure } => Some((current_unit, structure)),
                StructureFieldShape::Scalar => None,
            };
        }
        current_structure = match field.shape {
            StructureFieldShape::Structured { structure } => structure,
            StructureFieldShape::Scalar => return None,
        };
    }
    Some((current_unit, current_structure))
}

fn resolve_field_access_structure<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &'a crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    access: &crate::FieldAccess,
) -> Option<(&'a crate::UnitAnalysis, StructureId)> {
    let scope_index = scope_indexes.get(unit.unit_id.as_usize())?;
    let base_handle = resolve_field_access_base_symbol(project, lookup, unit, scope_index, access)?;
    let base_unit = &project.units[base_handle.unit.as_usize()];
    let (current_unit, mut current_structure) = resolve_symbol_structure_project(
        project,
        lookup,
        base_unit,
        scope_indexes,
        scope_for_unit(base_unit, access.scope),
        base_handle.symbol,
    )?;

    if access.field_path.is_empty() {
        return Some((current_unit, current_structure));
    }

    for segment in &access.field_path {
        if segment.is_deref() {
            return None;
        }
        let field = resolve_structure_field_info_project(
            project,
            lookup,
            scope_indexes,
            current_unit,
            access.scope,
            current_structure,
            segment.name.as_ref(),
        )?;
        current_structure = match field.shape {
            StructureFieldShape::Structured { structure } => structure,
            StructureFieldShape::Scalar => return None,
        };
    }

    Some((current_unit, current_structure))
}

fn loop_field_reference_matches_source_field<'a>(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    reference: &crate::ReferenceData,
    contexts: impl IntoIterator<Item = LoopFieldContextView<'a>>,
) -> bool {
    if reference.namespace != Namespace::Value || reference.kind != ReferenceKind::Identifier {
        return false;
    }
    contexts.into_iter().any(|context| {
        context.range.start <= reference.range.start
            && reference.range.end <= context.range.end
            && {
                let source_matches = resolve_loop_field_source_structure(
                    project,
                    lookup,
                    unit,
                    scope_indexes,
                    context,
                )
                .is_some_and(|(structure_unit, structure_id)| {
                    resolve_structure_field_info_project(
                        project,
                        lookup,
                        scope_indexes,
                        structure_unit,
                        context.scope,
                        structure_id,
                        reference.name.as_ref(),
                    )
                    .is_some()
                        || structure_has_unresolved_proxy_include_fields(
                            project,
                            lookup,
                            scope_indexes,
                            structure_unit,
                            context.scope,
                            structure_id,
                        )
                });
                source_matches
                    || context
                        .target_access
                        .as_ref()
                        .and_then(|access| {
                            resolve_field_access_structure(
                                project,
                                lookup,
                                unit,
                                scope_indexes,
                                access,
                            )
                        })
                        .is_some_and(|(structure_unit, structure_id)| {
                            resolve_structure_field_info_project(
                                project,
                                lookup,
                                scope_indexes,
                                structure_unit,
                                context.scope,
                                structure_id,
                                reference.name.as_ref(),
                            )
                            .is_some()
                                || structure_has_unresolved_proxy_include_fields(
                                    project,
                                    lookup,
                                    scope_indexes,
                                    structure_unit,
                                    context.scope,
                                    structure_id,
                                )
                        })
            }
    })
}

fn loop_where_reference_matches_source_field(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    reference: &crate::ReferenceData,
) -> bool {
    loop_field_reference_matches_source_field(
        project,
        lookup,
        unit,
        scope_indexes,
        reference,
        unit.loop_where_field_contexts
            .iter()
            .map(|context| LoopFieldContextView {
                scope: context.scope,
                range: &context.range,
                source_access: &context.source_access,
                target_access: context.target_access.as_ref(),
            }),
    )
}

fn loop_at_reference_matches_source_field(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    reference: &crate::ReferenceData,
) -> bool {
    loop_field_reference_matches_source_field(
        project,
        lookup,
        unit,
        scope_indexes,
        reference,
        unit.loop_at_field_contexts
            .iter()
            .map(|context| LoopFieldContextView {
                scope: context.scope,
                range: &context.range,
                source_access: &context.source_access,
                target_access: context.target_access.as_ref(),
            }),
    )
}

fn reference_depends_on_unresolved_field_access_base(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_index: &ScopeIndex,
    reference: &crate::ReferenceData,
) -> bool {
    if reference.namespace != Namespace::Value || reference.kind != ReferenceKind::Identifier {
        return false;
    }

    unit.field_accesses.iter().any(|access| {
        let matching_segment_idx = access.field_path.iter().position(|segment| {
            segment.range == reference.range && segment.name.as_ref() == reference.name.as_ref()
        });
        let segment_idx = matching_segment_idx.unwrap_or(access.field_path.len());
        if matching_segment_idx.is_none() {
            let Some(last_segment) = access.field_path.last() else {
                return false;
            };
            if access.scope != reference.scope
                || last_segment.range.end > reference.range.start
                || reference.range.start.saturating_sub(last_segment.range.end) > 16
            {
                return false;
            }
        }

        let Some(base_handle) =
            resolve_field_access_base_symbol(project, lookup, unit, scope_index, access)
        else {
            return false;
        };
        let base_unit = &project.units[base_handle.unit.as_usize()];
        let base_scope_index = &lookup.scope_indexes[base_unit.unit_id.as_usize()];
        let access_scope = scope_for_unit(base_unit, access.scope);
        let mut structure_id = base_unit.symbol(base_handle.symbol).structure;
        let mut declared_type = base_unit.symbol(base_handle.symbol).declared_type.clone();

        for step in access.field_path.iter().take(segment_idx) {
            if step.is_deref() {
                let Some((next_structure_id, next_declared_type)) = dereference_field_metadata(
                    base_unit,
                    base_scope_index,
                    access_scope,
                    structure_id,
                    declared_type,
                ) else {
                    return true;
                };
                structure_id = next_structure_id;
                declared_type = next_declared_type;
                continue;
            }

            (structure_id, declared_type) = normalize_field_metadata(
                base_unit,
                base_scope_index,
                access_scope,
                structure_id,
                declared_type,
            );
            let Some(current_structure_id) = structure_id else {
                return true;
            };
            let Some(field) = base_unit
                .structure(current_structure_id)
                .fields
                .iter()
                .find(|field| field.name.as_ref() == step.name.as_ref())
            else {
                return false;
            };
            structure_id = field.structure;
            declared_type = field.type_ref.clone();
        }

        let (structure_id, _) =
            normalize_field_metadata(unit, scope_index, access.scope, structure_id, declared_type);
        structure_id.is_none()
    })
}

fn reference_is_field_symbol_binding_target(
    unit: &crate::UnitAnalysis,
    reference: &crate::ReferenceData,
) -> bool {
    if reference.namespace != Namespace::Value || reference.kind != ReferenceKind::Identifier {
        return false;
    }

    unit.value_flow_edges.iter().any(|edge| {
        matches!(
            edge.kind,
            ValueFlowKind::FieldSymbolAssignment | ValueFlowKind::ConditionalFieldSymbolAssignment
        ) && matches!(
            &edge.target,
            ValueFlowTargetData::FieldSymbol {
                range,
                name: Some(name),
            } if range == &reference.range && name.as_ref() == reference.name.as_ref()
        )
    })
}

fn symbol_is_internal_table(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    symbol: &crate::SymbolData,
    seen: &mut HashSet<(u32, u32)>,
) -> bool {
    if unit.sql_targets.iter().any(|target| {
        target.is_inline
            && target.is_table
            && target.target_name.as_deref() == Some(symbol.name.as_ref())
            && resolve_symbol_in_scope_chain(
                unit,
                &scope_indexes[unit.unit_id.as_usize()],
                target.scope,
                Namespace::Value,
                &symbol.name,
            ) == Some(symbol.id)
    }) {
        return true;
    }

    if symbol_type_clause_suggests_internal_table(symbol) {
        return true;
    }

    let Some(type_ref) = symbol.declared_type.as_ref() else {
        return false;
    };
    if !type_ref.field_path.is_empty() {
        return false;
    }

    let Some(handle) = resolve_type_like_symbol_handle(
        project,
        lookup,
        unit,
        scope_indexes,
        symbol.scope,
        type_ref,
    ) else {
        return false;
    };
    if !seen.insert((handle.unit.0, handle.symbol.0)) {
        return false;
    }

    let resolved_unit = &project.units[handle.unit.as_usize()];
    let resolved_symbol = resolved_unit.symbol(handle.symbol);
    symbol_is_internal_table(
        project,
        lookup,
        resolved_unit,
        scope_indexes,
        resolved_symbol,
        seen,
    )
}

fn symbol_is_structure_like_for_into(symbol: &crate::SymbolData) -> bool {
    if symbol.structure.is_some() {
        return true;
    }
    if symbol.kind == crate::SymbolKind::FieldSymbol
        && symbol
            .declared_type
            .as_ref()
            .is_some_and(symbol_is_generic_dynamic_type)
    {
        return true;
    }
    symbol
        .type_clause_display
        .as_deref()
        .is_some_and(|display| {
            let upper = display.to_ascii_uppercase();
            upper.contains("BEGIN OF")
        })
}

fn symbol_is_generic_dynamic_type(type_ref: &crate::FieldTypeRefData) -> bool {
    type_ref.namespace == Namespace::Type
        && !type_ref.is_ref
        && type_ref.field_path.is_empty()
        && matches!(type_ref.base_name.as_ref(), "any" | "data")
}

fn into_target_identifier_range(
    unit: &crate::UnitAnalysis,
    target: &crate::def_map::SqlTargetData,
    name: &Arc<str>,
) -> std::ops::Range<usize> {
    let search_range = target.target_range.as_ref().unwrap_or(&target.range);
    unit.references
        .iter()
        .filter(|reference| {
            reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
                && reference.name.as_ref().eq_ignore_ascii_case(name.as_ref())
                && reference.range.start >= search_range.start
                && reference.range.end <= search_range.end
        })
        .min_by_key(|reference| reference.range.end.saturating_sub(reference.range.start))
        .map(|reference| reference.range.clone())
        .unwrap_or_else(|| search_range.clone())
}

fn validate_open_sql_into_targets(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
) -> Vec<Diagnostic> {
    let mut out = Vec::new();
    let scope_index = &scope_indexes[unit.unit_id.as_usize()];
    for target in &unit.sql_targets {
        if target.is_inline || target.target_name.is_none() {
            continue;
        }
        let name = target.target_name.as_ref().unwrap();
        let Some(symbol_id) =
            resolve_symbol_in_scope_chain(unit, scope_index, target.scope, Namespace::Value, name)
        else {
            continue;
        };
        let symbol = unit.symbol(symbol_id);
        let diag_range = into_target_identifier_range(unit, target, name);

        if target.is_table
            && !symbol_is_internal_table(
                project,
                lookup,
                unit,
                scope_indexes,
                symbol,
                &mut HashSet::new(),
            )
        {
            out.push(Diagnostic {
                kind: DiagnosticKind::InvalidOpenSqlIntoTarget,
                range: diag_range.clone(),
                message: format!(
                    "INTO TABLE / APPENDING ... TABLE target '{}' should be typed as an internal table (STANDARD/HASHED/SORTED/TABLE OF …)",
                    name
                ),
            });
        }
        // `INTO CORRESPONDING FIELDS OF wa` needs a structure-like work area; `... OF TABLE itab`
        // needs an internal table (checked above). Both set `is_corresponding`; only the former
        // should be validated as structure-like — otherwise we false-positive when the line type
        // is unresolved and `SymbolData::structure` is absent despite `STANDARD TABLE OF ...`.
        if target.is_corresponding && !target.is_table && !symbol_is_structure_like_for_into(symbol)
        {
            out.push(Diagnostic {
                kind: DiagnosticKind::InvalidOpenSqlIntoTarget,
                range: diag_range,
                message: format!(
                    "INTO CORRESPONDING FIELDS target '{}' should be a structure (typed with BEGIN OF / structure type)",
                    name
                ),
            });
        }
    }
    out
}

struct ReportTablesContext {
    units: HashSet<UnitId>,
    table_names: HashSet<Arc<str>>,
}

fn unit_is_report(unit: &crate::UnitAnalysis) -> bool {
    unit.symbols
        .iter()
        .any(|symbol| symbol.scope == unit.root_scope && symbol.kind == SymbolKind::Report)
}

fn unit_is_ddic_table_like_dependency(unit: &crate::UnitAnalysis) -> bool {
    let uri = unit.uri.as_ref().to_ascii_lowercase();
    if uri.contains("ddic-table-type") || uri.contains("/ddic/tabletypes/") {
        return false;
    }
    uri.contains("/ddic/tables/")
        || uri.contains("/ddic/database-tables/")
        || uri.contains("/ddic/views/")
        || uri.contains("/dictionary/database-tables/")
        || uri.contains("/dictionary/views/")
        || uri.contains("/ddic-table/")
        || uri.contains("/ddic-view/")
        || uri.contains("\\ddic-table\\")
        || uri.contains("\\ddic-view\\")
        || uri.contains("object_type/tabldt/")
        || uri.contains("object_type/viewdv/")
        || uri.contains("kind=ddic-table")
        || uri.contains("kind=ddic-view")
}

fn include_closure(project: &ProjectAnalysis, root: UnitId) -> HashSet<UnitId> {
    let mut out = HashSet::new();
    let mut queue = VecDeque::from([root]);
    while let Some(unit_id) = queue.pop_front() {
        if !out.insert(unit_id) {
            continue;
        }
        queue.extend(
            project.units[unit_id.as_usize()]
                .include_edges
                .iter()
                .filter_map(|edge| edge.target),
        );
    }
    out
}

fn build_report_tables_contexts(project: &ProjectAnalysis) -> Vec<ReportTablesContext> {
    project
        .units
        .iter()
        .filter(|unit| unit_is_report(unit))
        .map(|unit| {
            let units = include_closure(project, unit.unit_id);
            let mut table_names = HashSet::new();
            for unit_id in &units {
                let context_unit = &project.units[unit_id.as_usize()];
                table_names.extend(
                    context_unit
                        .table_work_areas
                        .iter()
                        .filter(|work_area| work_area.scope == context_unit.root_scope)
                        .map(|work_area| Arc::clone(&work_area.name)),
                );
            }
            ReportTablesContext { units, table_names }
        })
        .collect()
}

fn validate_missing_tables_declarations(
    project: &ProjectAnalysis,
    report_contexts: &[ReportTablesContext],
    unit: &crate::UnitAnalysis,
) -> Vec<Diagnostic> {
    if unit_is_ddic_table_like_dependency(unit) {
        return Vec::new();
    }

    let contexts = report_contexts
        .iter()
        .filter(|context| context.units.contains(&unit.unit_id))
        .collect::<Vec<_>>();
    if contexts.is_empty() {
        return Vec::new();
    }

    let mut diagnostics = Vec::new();
    let mut emitted = HashSet::<(Arc<str>, usize, usize)>::new();
    let selection_screen_report_type_positions = unit
        .selection_screen_report_type_positions
        .iter()
        .map(|range| (range.start, range.end))
        .collect::<HashSet<_>>();
    for reference in unit.references.iter().filter(|reference| {
        reference.kind == ReferenceKind::TypeRef
            && selection_screen_report_type_positions
                .contains(&(reference.range.start, reference.range.end))
    }) {
        let Some(Resolution::Symbol(handle)) = reference.resolution else {
            continue;
        };
        if !unit_is_ddic_table_like_dependency(&project.units[handle.unit.as_usize()]) {
            continue;
        }
        if unit
            .table_work_areas
            .iter()
            .any(|work_area| work_area.name == reference.name && work_area.range == reference.range)
        {
            continue;
        }
        if contexts
            .iter()
            .all(|context| context.table_names.contains(&reference.name))
        {
            continue;
        }
        if !emitted.insert((
            Arc::clone(&reference.name),
            reference.range.start,
            reference.range.end,
        )) {
            continue;
        }
        diagnostics.push(Diagnostic {
            kind: DiagnosticKind::MissingTablesDeclaration,
            range: reference.range.clone(),
            message: format!(
                "DDIC table/view '{}' is used as a report type without a top-level TABLES {} declaration in the report or its includes",
                reference.name, reference.name
            ),
        });
    }
    diagnostics
}

fn invalid_object_type_reference_diagnostic(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    scope: ScopeId,
    range: &TextRange,
    type_ref: &FieldTypeRefData,
) -> Option<Diagnostic> {
    if type_ref.namespace != Namespace::Type || type_ref.is_ref || !type_ref.field_path.is_empty() {
        return None;
    }

    let handle =
        resolve_type_like_symbol_handle(project, lookup, unit, scope_indexes, scope, type_ref)?;
    let symbol = project.units[handle.unit.as_usize()].symbol(handle.symbol);
    if !matches!(symbol.kind, SymbolKind::Class | SymbolKind::Interface) {
        return None;
    }

    Some(Diagnostic {
        kind: DiagnosticKind::InvalidObjectTypeReference,
        range: range.clone(),
        message: format!(
            "object type '{}' can only be referenced using REF TO",
            type_ref.base_name
        ),
    })
}

fn validate_object_type_references(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    original_symbol_count: usize,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    let mut emitted = HashSet::<(Arc<str>, usize, usize)>::new();

    for symbol in unit.symbols.iter().take(original_symbol_count) {
        let Some(type_ref) = symbol.declared_type.as_ref() else {
            continue;
        };
        let Some(diagnostic) = invalid_object_type_reference_diagnostic(
            project,
            lookup,
            unit,
            scope_indexes,
            symbol.scope,
            &symbol.decl_range,
            type_ref,
        ) else {
            continue;
        };
        if emitted.insert((
            Arc::clone(&type_ref.base_name),
            diagnostic.range.start,
            diagnostic.range.end,
        )) {
            diagnostics.push(diagnostic);
        }
    }

    diagnostics
}

fn parameter_type_uses_inline_table_type(display: &str) -> bool {
    let upper = display
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
        .to_ascii_uppercase();
    upper.starts_with("TABLE OF ") || upper.contains(" TABLE OF ")
}

fn invalid_parameter_type_diagnostic(name: &str, range: &TextRange, display: &str) -> Diagnostic {
    Diagnostic {
        kind: DiagnosticKind::InvalidParameterType,
        range: range.clone(),
        message: format!(
            "parameter '{name}' uses inline table type '{display}'; define a table type and reference it with TYPE"
        ),
    }
}

fn validate_parameter_types(unit: &crate::UnitAnalysis) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    for member in &unit.class_members {
        if member.kind != ClassMemberKind::Method {
            continue;
        }
        for parameter in &member.parameters {
            let Some(display) = parameter.type_clause_display.as_deref() else {
                continue;
            };
            if parameter_type_uses_inline_table_type(display) {
                diagnostics.push(invalid_parameter_type_diagnostic(
                    parameter.name.as_ref(),
                    &parameter.range,
                    display,
                ));
            }
        }
    }
    for routine in &unit.form_routines {
        for parameter in &routine.parameters {
            if parameter.section == FormParameterSection::Tables {
                continue;
            }
            let symbol = unit.symbol(parameter.symbol);
            let Some(display) = symbol.type_clause_display.as_deref() else {
                continue;
            };
            if parameter_type_uses_inline_table_type(display) {
                diagnostics.push(invalid_parameter_type_diagnostic(
                    symbol.name.as_ref(),
                    &symbol.decl_range,
                    display,
                ));
            }
        }
    }
    diagnostics
}

pub(crate) fn validate_project_with_scope_indexes(
    project: &mut ProjectAnalysis,
    scope_indexes: &[ScopeIndex],
) {
    let dirty_unit_ids: HashSet<_> = project.units.iter().map(|unit| unit.unit_id).collect();
    validate_project_with_scope_indexes_for_units(project, scope_indexes, &dirty_unit_ids);
}

pub(crate) fn validate_project_with_scope_indexes_for_units(
    project: &mut ProjectAnalysis,
    scope_indexes: &[ScopeIndex],
    dirty_unit_ids: &HashSet<crate::ids::UnitId>,
) {
    let lookup = build_validation_lookup(project, scope_indexes);
    let global_names = collect_global_names(project);
    let report_tables_contexts = build_report_tables_contexts(project);

    for unit_idx in 0..project.units.len() {
        if !dirty_unit_ids.contains(&project.units[unit_idx].unit_id) {
            continue;
        }
        let original_symbol_count = project.units[unit_idx].symbols.len();
        let mut scope_index = scope_indexes[unit_idx].clone();
        let synthetic_symbols = {
            let unit = &project.units[unit_idx];
            let mut symbols = qualified_interface_method_scope_symbol_specs(project, &lookup, unit);
            symbols.extend(inherited_redefinition_method_scope_symbol_specs(
                project, &lookup, unit,
            ));
            symbols.extend(declared_method_scope_symbol_specs(project, &lookup, unit));
            symbols.extend(event_handler_method_scope_symbol_specs(
                project, &lookup, unit,
            ));
            symbols.extend(loop_where_scope_symbol_specs(
                project,
                &lookup,
                unit,
                scope_indexes,
            ));
            symbols.extend(loop_at_scope_symbol_specs(
                project,
                &lookup,
                unit,
                scope_indexes,
            ));
            let mut next_symbol_id = unit.symbols.len() as u32;
            for (_, symbol) in &mut symbols {
                symbol.id = SymbolId(next_symbol_id);
                next_symbol_id += 1;
            }
            symbols
        };
        {
            let unit = &mut project.units[unit_idx];
            for (scope_id, symbol) in synthetic_symbols {
                let symbol_id = symbol.id;
                let symbol_name = Arc::clone(&symbol.name);
                let symbol_kind = symbol.kind;
                unit.symbols.push(symbol);
                unit.scopes[scope_id.as_usize()]
                    .declarations
                    .push(symbol_id);
                for &namespace in symbol_kind.namespaces() {
                    inject_symbol_into_scope_index(
                        &mut scope_index,
                        scope_id,
                        symbol_id,
                        namespace,
                        Arc::clone(&symbol_name),
                    );
                }
            }
        }
        let synthetic_reference_resolutions: Vec<_> = {
            let unit = &project.units[unit_idx];
            unit.references
                .iter()
                .enumerate()
                .filter_map(|(idx, reference)| {
                    if reference.resolution.is_some() {
                        return None;
                    }
                    if let Some(symbol_id) = resolve_symbol_in_scope_chain(
                        unit,
                        &scope_index,
                        reference.scope,
                        reference.namespace,
                        &reference.name,
                    ) {
                        return Some((
                            idx,
                            SymbolHandle {
                                unit: unit.unit_id,
                                symbol: symbol_id,
                            },
                        ));
                    }
                    if reference.namespace == Namespace::Value
                        && reference.kind == ReferenceKind::Identifier
                    {
                        let handle = resolve_inherited_attribute_symbol(
                            project,
                            &lookup,
                            unit,
                            reference.scope,
                            reference.name.as_ref(),
                        )?;
                        return Some((idx, handle));
                    }
                    None
                })
                .collect()
        };
        {
            let unit = &mut project.units[unit_idx];
            for (idx, handle) in synthetic_reference_resolutions {
                unit.references[idx].resolution = Some(Resolution::Symbol(handle));
            }
        }
        let scope_names = build_scope_names(&project.units[unit_idx]);
        let constructor_diagnostics = validate_super_constructor_calls(
            project,
            &lookup,
            &project.units[unit_idx],
            &scope_index,
        );
        let object_type_diagnostics = validate_object_type_references(
            project,
            &lookup,
            &project.units[unit_idx],
            scope_indexes,
            original_symbol_count,
        );
        let parameter_type_diagnostics = validate_parameter_types(&project.units[unit_idx]);
        let abstract_instantiation_diagnostics = validate_abstract_class_instantiations(
            project,
            &lookup,
            &project.units[unit_idx],
            &scope_index,
        );
        let constructor_for_iterator_diagnostics = validate_constructor_for_iterator_reuse(
            project,
            &lookup,
            &project.units[unit_idx],
            scope_indexes,
            &scope_index,
        );
        let field_access_bases: Vec<_> = project.units[unit_idx]
            .field_accesses
            .iter()
            .map(|access| {
                let unit = &project.units[unit_idx];
                let base_handle =
                    resolve_field_access_base_symbol(project, &lookup, unit, &scope_index, access)?;
                Some((
                    (base_handle.unit.as_usize(), base_handle.symbol),
                    resolve_class_selector_base(project, &lookup, unit, access, base_handle).map(
                        |(class_unit, class_symbol_id, requires_static)| {
                            (
                                class_unit.unit_id.as_usize(),
                                class_symbol_id,
                                requires_static,
                            )
                        },
                    ),
                ))
            })
            .collect();

        let order_by_validation = validate_open_sql_order_by(
            project,
            &lookup,
            &project.units[unit_idx],
            scope_indexes,
            &scope_index,
        );
        {
            let unit = &mut project.units[unit_idx];
            for query in unit
                .sql_queries
                .iter_mut()
                .filter(|query| query.order_by_primary_key)
            {
                query.order_by_fields.clear();
            }
            for (query_id, fields) in &order_by_validation.resolved_primary_key_fields {
                if let Some(query) = unit
                    .sql_queries
                    .iter_mut()
                    .find(|query| query.id == *query_id)
                {
                    query.order_by_fields = fields.clone();
                }
            }
        }

        let unit = &project.units[unit_idx];
        let retained: Vec<_> = unit
            .diagnostics
            .iter()
            .filter(|diag| {
                matches!(
                    diag.kind,
                    DiagnosticKind::DuplicateDeclaration
                        | DiagnosticKind::ShadowedSymbol
                        | DiagnosticKind::MismatchedStructuredDeclaration
                        | DiagnosticKind::UnresolvedInclude
                        | DiagnosticKind::InvalidOpenSqlSyntax
                )
            })
            .cloned()
            .collect();
        let mut unit_diagnostics = retained;
        unit_diagnostics.extend(constructor_for_iterator_diagnostics);

        for reference in &unit.references {
            let Some(Resolution::Symbol(handle)) = reference.resolution else {
                continue;
            };
            if reference.kind != ReferenceKind::TypeRef || reference.namespace != Namespace::Type {
                continue;
            }
            let symbol_unit = &project.units[handle.unit.as_usize()];
            let symbol = symbol_unit.symbol(handle.symbol);
            if matches!(symbol.kind, SymbolKind::TypeDef | SymbolKind::Class)
                && lookup.include_order.type_decl_after_reference(
                    unit.unit_id,
                    reference.range.start,
                    handle.unit,
                    symbol.decl_range.start,
                )
            {
                unit_diagnostics.push(Diagnostic {
                    kind: DiagnosticKind::UnresolvedReference,
                    range: reference.range.clone(),
                    message: format!("type '{}' is declared after its use", reference.name),
                });
            }
        }

        for reference in &unit.references {
            if reference.resolution.is_some() {
                continue;
            }
            if reference_is_restricted_constructor_for_iterator_use(unit, reference) {
                continue;
            }
            let is_field_symbol_binding_target =
                reference_is_field_symbol_binding_target(unit, reference);
            if loop_where_reference_matches_source_field(
                project,
                &lookup,
                unit,
                scope_indexes,
                reference,
            ) {
                continue;
            }
            if loop_at_reference_matches_source_field(
                project,
                &lookup,
                unit,
                scope_indexes,
                reference,
            ) {
                continue;
            }
            if reference_depends_on_unresolved_field_access_base(
                project,
                &lookup,
                unit,
                &scope_index,
                reference,
            ) && !is_field_symbol_binding_target
            {
                continue;
            }
            if reference.namespace == Namespace::Value
                && reference.name.as_ref() == "super"
                && is_valid_super_reference(unit, reference.scope)
            {
                continue;
            }

            let has_other_namespace = !reference_is_tables_decl_type_ref(unit, reference)
                && scope_names
                    .get(&reference.name)
                    .or_else(|| global_names.get(&reference.name))
                    .is_some_and(|namespaces| namespaces & (1 << reference.namespace as u8) == 0);

            let (kind, message) = if has_other_namespace {
                (
                    DiagnosticKind::WrongNamespace,
                    format!(
                        "'{}' exists, but not in the {:?} namespace",
                        reference.name, reference.namespace
                    ),
                )
            } else {
                let subject = match (reference.kind, reference.namespace) {
                    (ReferenceKind::MessageClass, _) => "message class",
                    (_, Namespace::Type) => "type",
                    (_, Namespace::Routine) => "routine",
                    (_, Namespace::Value) => "symbol",
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

        unit_diagnostics.extend(validate_message_uses(project, &lookup, unit));

        for (access, base_info) in unit.field_accesses.iter().zip(&field_access_bases) {
            let Some(((base_unit_idx, base_symbol_id), class_selector_base)) = *base_info else {
                continue;
            };
            let base_unit = &project.units[base_unit_idx];
            let access_scope = scope_for_unit(base_unit, access.scope);
            let base_symbol = base_unit.symbol(base_symbol_id);
            let (has_leading_deref, field_path) = split_leading_deref(access);
            if access.base_namespace == Namespace::Type && base_symbol.kind == SymbolKind::Class {
                let mut idx = 0usize;
                let mut structure_tail: Option<(&crate::UnitAnalysis, crate::StructureId)> = None;
                let mut static_structure_holder: Option<Arc<str>> = None;
                while idx < field_path.len() {
                    let step = &field_path[idx];
                    if let Some((structure_unit, structure_id)) = structure_tail {
                        let holder = static_structure_holder.as_deref().unwrap_or("?");
                        let Some(field) = resolve_structure_field_info_project(
                            project,
                            &lookup,
                            scope_indexes,
                            structure_unit,
                            access.scope,
                            structure_id,
                            step.name.as_ref(),
                        ) else {
                            unit_diagnostics.push(Diagnostic {
                                kind: DiagnosticKind::UnknownField,
                                range: step.range.clone(),
                                message: format!(
                                    "unknown field '{}' for '{}=>{}'",
                                    step.name, access.base_name, holder
                                ),
                            });
                            break;
                        };
                        idx += 1;
                        if idx == field_path.len() {
                            break;
                        }
                        let StructureFieldShape::Structured {
                            structure: next_structure,
                        } = field.shape
                        else {
                            let next_step = &field_path[idx];
                            unit_diagnostics.push(Diagnostic {
                                kind: DiagnosticKind::UnknownField,
                                range: next_step.range.clone(),
                                message: format!(
                                    "unknown field '{}' for '{}=>{}'",
                                    next_step.name, access.base_name, holder
                                ),
                            });
                            break;
                        };
                        structure_tail = Some((structure_unit, next_structure));
                        continue;
                    }

                    let Some((member_unit, member)) = resolve_class_member_in_hierarchy(
                        project,
                        &lookup,
                        base_unit,
                        base_symbol_id,
                        step.name.as_ref(),
                    ) else {
                        if let Some(type_symbol) = resolve_class_type_symbol_in_hierarchy(
                            project,
                            &lookup,
                            base_unit,
                            base_symbol_id,
                            step.name.as_ref(),
                        ) {
                            let type_unit = &project.units[type_symbol.unit.as_usize()];
                            let type_symbol = type_unit.symbol(type_symbol.symbol);
                            idx += 1;
                            if idx == field_path.len() {
                                break;
                            }
                            let Some(next_structure) = type_symbol.structure else {
                                let next_step = &field_path[idx];
                                unit_diagnostics.push(Diagnostic {
                                    kind: DiagnosticKind::UnknownField,
                                    range: next_step.range.clone(),
                                    message: format!(
                                        "unknown static member '{}' for class '{}=>{}'",
                                        next_step.name, access.base_name, step.name
                                    ),
                                });
                                break;
                            };
                            static_structure_holder = Some(Arc::clone(&type_symbol.name));
                            structure_tail = Some((type_unit, next_structure));
                            continue;
                        }
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
                            &lookup,
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
                    idx += 1;
                    if idx == field_path.len() {
                        break;
                    }
                    let Some(next_structure) = member.structure else {
                        let next_step = &field_path[idx];
                        unit_diagnostics.push(Diagnostic {
                            kind: DiagnosticKind::UnknownField,
                            range: next_step.range.clone(),
                            message: format!(
                                "unknown static member '{}' for class '{}=>{}'",
                                next_step.name, access.base_name, member.name
                            ),
                        });
                        break;
                    };
                    static_structure_holder = Some(Arc::clone(&member.name));
                    structure_tail = Some((member_unit, next_structure));
                }
                continue;
            }
            if !has_leading_deref
                && let Some((class_unit_idx, class_symbol_id, requires_static)) =
                    class_selector_base
            {
                let class_unit = &project.units[class_unit_idx];
                let class_name = Arc::clone(&class_unit.symbol(class_symbol_id).name);
                let mut structure_tail: Option<(&crate::UnitAnalysis, StructureId)> = None;
                let mut structure_holder: Option<Arc<str>> = None;
                let mut field_start_idx = 0usize;
                if !requires_static && field_path.len() >= 2 {
                    let class_handle = SymbolHandle {
                        unit: class_unit.unit_id,
                        symbol: class_symbol_id,
                    };
                    if let Some(interface_handle) = resolve_exposed_interface_handle(
                        project,
                        &lookup,
                        class_handle,
                        field_path[0].name.as_ref(),
                    ) {
                        if resolve_interface_member_path(
                            project,
                            &lookup,
                            interface_handle,
                            &field_path[1..],
                        )
                        .is_some()
                        {
                            continue;
                        }
                    }

                    if field_path.len() == 2
                        && class_hierarchy_supports_named_interface_member(
                            project,
                            &lookup,
                            class_unit,
                            class_symbol_id,
                            field_path[0].name.as_ref(),
                            field_path[1].name.as_ref(),
                        )
                    {
                        continue;
                    }

                    if let Some((member_unit, member)) =
                        resolve_fallback_qualified_class_member_in_hierarchy(
                            project,
                            &lookup,
                            class_unit,
                            class_symbol_id,
                            field_path[0].name.as_ref(),
                            field_path[1].name.as_ref(),
                        )
                        .filter(|(member_unit, member)| {
                            class_member_visible_to(
                                project,
                                &lookup,
                                unit,
                                access.scope,
                                member_unit,
                                member,
                            )
                        })
                    {
                        field_start_idx = 2;
                        if field_start_idx == field_path.len() {
                            continue;
                        }
                        let Some(next_structure) = member.structure else {
                            let next_step = &field_path[field_start_idx];
                            unit_diagnostics.push(Diagnostic {
                                kind: DiagnosticKind::UnknownField,
                                range: next_step.range.clone(),
                                message: format!(
                                    "unknown member '{}' for class '{}->{}'",
                                    next_step.name, class_name, member.name
                                ),
                            });
                            continue;
                        };
                        structure_tail = Some((member_unit, next_structure));
                        structure_holder = Some(Arc::clone(&member.name));
                    }
                }
                for (idx, step) in field_path.iter().enumerate().skip(field_start_idx) {
                    if let Some((structure_unit, structure_id)) = structure_tail {
                        let holder = structure_holder.as_deref().unwrap_or("?");
                        let Some(field) = resolve_structure_field_info_project(
                            project,
                            &lookup,
                            scope_indexes,
                            structure_unit,
                            access.scope,
                            structure_id,
                            step.name.as_ref(),
                        ) else {
                            unit_diagnostics.push(Diagnostic {
                                kind: DiagnosticKind::UnknownField,
                                range: step.range.clone(),
                                message: format!(
                                    "unknown field '{}' for '{}->{}'",
                                    step.name, class_name, holder
                                ),
                            });
                            break;
                        };
                        if idx + 1 == field_path.len() {
                            break;
                        }
                        let StructureFieldShape::Structured { structure } = field.shape else {
                            let next_step = &field_path[idx + 1];
                            unit_diagnostics.push(Diagnostic {
                                kind: DiagnosticKind::UnknownField,
                                range: next_step.range.clone(),
                                message: format!(
                                    "unknown field '{}' for '{}->{}'",
                                    next_step.name, class_name, holder
                                ),
                            });
                            break;
                        };
                        structure_tail = Some((structure_unit, structure));
                        continue;
                    }

                    let Some((member_unit, member)) = resolve_class_member_in_hierarchy(
                        project,
                        &lookup,
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
                            &lookup,
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
                    if idx + 1 == field_path.len() {
                        break;
                    }
                    let Some(next_structure) = member.structure else {
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
                    };
                    structure_tail = Some((member_unit, next_structure));
                    structure_holder = Some(Arc::clone(&member.name));
                }
                continue;
            }
            if has_leading_deref && field_path.is_empty() {
                continue;
            }
            let mut current_unit = base_unit;
            let mut structure_id = base_unit.symbol(base_symbol_id).structure;
            let mut declared_type = base_unit.symbol(base_symbol_id).declared_type.clone();
            let subject = if access.in_type_position {
                "type"
            } else {
                "structure"
            };
            let mut qualifier = access.base_name.to_string();
            for step in &access.field_path {
                if step.is_deref() {
                    let current_scope = scope_for_unit(current_unit, access.scope);
                    let Some((next_structure_id, next_declared_type)) = dereference_field_metadata(
                        current_unit,
                        &scope_indexes[current_unit.unit_id.as_usize()],
                        current_scope,
                        structure_id,
                        declared_type,
                    ) else {
                        break;
                    };
                    structure_id = next_structure_id;
                    declared_type = next_declared_type;
                    qualifier.push_str("->*");
                    continue;
                }

                (current_unit, structure_id, declared_type) = normalize_field_metadata_project(
                    project,
                    &lookup,
                    scope_indexes,
                    current_unit,
                    access_scope,
                    structure_id,
                    declared_type,
                );
                let Some(current_structure_id) = structure_id else {
                    break;
                };
                let Some(field) = resolve_structure_field_info_project(
                    project,
                    &lookup,
                    scope_indexes,
                    current_unit,
                    scope_for_unit(current_unit, access.scope),
                    current_structure_id,
                    step.name.as_ref(),
                ) else {
                    if structure_has_unresolved_proxy_include_fields(
                        project,
                        &lookup,
                        scope_indexes,
                        current_unit,
                        scope_for_unit(current_unit, access.scope),
                        current_structure_id,
                    ) {
                        break;
                    }
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
                current_unit = &project.units[field.owner_unit.as_usize()];
                structure_id = match field.shape {
                    StructureFieldShape::Structured { structure } => Some(structure),
                    StructureFieldShape::Scalar => None,
                };
                declared_type = field.type_ref.clone();
            }
        }

        for assignment in &unit.assignment_sites {
            if assignment.is_corresponding {
                unit_diagnostics.extend(validate_move_corresponding_assignment(
                    project,
                    &lookup,
                    unit,
                    scope_indexes,
                    assignment,
                ));
                continue;
            }
            if type_facts_compatibility(
                project,
                &lookup.type_fact_lookup,
                unit,
                &assignment.lhs,
                unit,
                &assignment.rhs,
            )
            .is_incompatible()
            {
                unit_diagnostics.push(Diagnostic {
                    kind: DiagnosticKind::IncompatibleAssignmentType,
                    range: assignment.range.clone(),
                    message: format!(
                        "assignment target '{}' is incompatible with source '{}'",
                        type_fact_label(&assignment.lhs),
                        type_fact_label(&assignment.rhs)
                    ),
                });
            }
        }

        for call_site in &unit.call_sites {
            if let Some((target_unit, function_module)) =
                resolve_call_target_function_module(project, &lookup, unit, &scope_index, call_site)
            {
                let mut matched_required = HashSet::<Arc<str>>::new();
                let mut seen_named = HashSet::<Arc<str>>::new();
                let mut seen_exceptions = HashSet::<Arc<str>>::new();

                for argument in &call_site.arguments {
                    let Some(name) = argument.name.as_ref() else {
                        continue;
                    };
                    if argument.section == Some(crate::NamedArgumentSection::Exceptions) {
                        if !seen_exceptions.insert(Arc::clone(name)) {
                            unit_diagnostics.push(Diagnostic {
                                kind: DiagnosticKind::DuplicateNamedParameter,
                                range: argument.range.clone(),
                                message: format!("duplicate function module exception '{}'", name),
                            });
                            continue;
                        }
                        if name.eq_ignore_ascii_case("others") {
                            continue;
                        }
                        if !function_module
                            .exceptions
                            .iter()
                            .any(|exception| exception.name == *name)
                        {
                            unit_diagnostics.push(Diagnostic {
                                kind: DiagnosticKind::UnknownFunctionModuleException,
                                range: argument.range.clone(),
                                message: format!(
                                    "unknown exception '{}' for function module '{}'",
                                    name,
                                    target_unit.symbol(function_module.symbol).name
                                ),
                            });
                        }
                        continue;
                    }
                    if !seen_named.insert(Arc::clone(name)) {
                        unit_diagnostics.push(Diagnostic {
                            kind: DiagnosticKind::DuplicateNamedParameter,
                            range: argument.range.clone(),
                            message: format!("duplicate named parameter '{}'", name),
                        });
                        continue;
                    }
                    let Some(parameter) = function_module.parameters.iter().find(|parameter| {
                        parameter.name == *name
                            && call_section_matches_function_parameter(argument.section, parameter)
                    }) else {
                        unit_diagnostics.push(Diagnostic {
                            kind: DiagnosticKind::UnknownNamedParameter,
                            range: argument.range.clone(),
                            message: format!(
                                "unknown named parameter '{}' for function module '{}'",
                                name,
                                target_unit.symbol(function_module.symbol).name
                            ),
                        });
                        continue;
                    };
                    if function_module_parameter_is_required(parameter) {
                        matched_required.insert(Arc::clone(&parameter.name));
                    }
                    let expected = function_module_parameter_type_fact(parameter);
                    let compatibility = match parameter.section {
                        FunctionModuleParameterSection::Tables => {
                            type_facts_parameter_compatibility(
                                project,
                                &lookup.type_fact_lookup,
                                target_unit,
                                &expected,
                                unit,
                                &argument.type_fact,
                            )
                        }
                        FunctionModuleParameterSection::Importing
                        | FunctionModuleParameterSection::Changing => {
                            type_facts_strict_table_kind_compatibility(
                                project,
                                &lookup.type_fact_lookup,
                                target_unit,
                                &expected,
                                unit,
                                &argument.type_fact,
                            )
                        }
                        FunctionModuleParameterSection::Exporting => type_facts_compatibility(
                            project,
                            &lookup.type_fact_lookup,
                            target_unit,
                            &expected,
                            unit,
                            &argument.type_fact,
                        ),
                    };
                    if !parameter.is_untyped && compatibility.is_incompatible() {
                        unit_diagnostics.push(Diagnostic {
                            kind: DiagnosticKind::IncompatibleArgumentType,
                            range: argument.range.clone(),
                            message: format!(
                                "argument '{}' expects '{}', got '{}'",
                                parameter.name,
                                type_fact_label(&expected),
                                type_fact_label(&argument.type_fact)
                            ),
                        });
                    }
                }

                for parameter in &function_module.parameters {
                    if function_module_parameter_is_required(parameter)
                        && !matched_required.contains(&parameter.name)
                    {
                        unit_diagnostics.push(Diagnostic {
                            kind: DiagnosticKind::MissingRequiredParameter,
                            range: call_site.range.clone(),
                            message: format!(
                                "missing required parameter '{}' for function module '{}'",
                                parameter.name,
                                target_unit.symbol(function_module.symbol).name
                            ),
                        });
                    }
                }

                continue;
            }

            if let Some((target_unit, member)) =
                resolve_call_target_event_member(project, &lookup, unit, call_site)
            {
                let mut matched_required = HashSet::<Arc<str>>::new();
                let mut seen_named = HashSet::<Arc<str>>::new();

                for argument in &call_site.arguments {
                    let Some(name) = argument.name.as_ref() else {
                        continue;
                    };
                    if !seen_named.insert(Arc::clone(name)) {
                        unit_diagnostics.push(Diagnostic {
                            kind: DiagnosticKind::DuplicateNamedParameter,
                            range: argument.range.clone(),
                            message: format!("duplicate named parameter '{}'", name),
                        });
                        continue;
                    }
                    let Some(parameter) = member.parameters.iter().find(|parameter| {
                        parameter.name == *name
                            && call_section_matches_event_parameter(
                                argument.section,
                                parameter.section,
                            )
                    }) else {
                        unit_diagnostics.push(Diagnostic {
                            kind: DiagnosticKind::UnknownNamedParameter,
                            range: argument.range.clone(),
                            message: format!(
                                "unknown named parameter '{}' for event '{}'",
                                name, member.name
                            ),
                        });
                        continue;
                    };
                    if event_parameter_is_required(parameter) {
                        matched_required.insert(Arc::clone(&parameter.name));
                    }
                    if type_facts_parameter_compatibility(
                        project,
                        &lookup.type_fact_lookup,
                        target_unit,
                        &method_parameter_type_fact(parameter),
                        unit,
                        &argument.type_fact,
                    )
                    .is_incompatible()
                    {
                        unit_diagnostics.push(Diagnostic {
                            kind: DiagnosticKind::IncompatibleArgumentType,
                            range: argument.range.clone(),
                            message: format!(
                                "argument '{}' expects '{}', got '{}'",
                                parameter.name,
                                type_fact_label(&method_parameter_type_fact(parameter)),
                                type_fact_label(&argument.type_fact)
                            ),
                        });
                    }
                }

                for parameter in &member.parameters {
                    if event_parameter_is_required(parameter)
                        && !matched_required.contains(&parameter.name)
                    {
                        unit_diagnostics.push(Diagnostic {
                            kind: DiagnosticKind::MissingRequiredParameter,
                            range: call_site.range.clone(),
                            message: format!(
                                "missing required parameter '{}' for event '{}'",
                                parameter.name, member.name
                            ),
                        });
                    }
                }

                continue;
            }

            let Some((target_unit, member)) =
                resolve_call_target_member(project, &lookup, unit, &scope_index, call_site)
            else {
                continue;
            };
            if member.kind != ClassMemberKind::Method {
                unit_diagnostics.push(Diagnostic {
                    kind: DiagnosticKind::UnknownField,
                    range: call_site.range.clone(),
                    message: format!(
                        "'{}' is not a method and cannot be used with CALL METHOD",
                        member.name
                    ),
                });
                continue;
            }
            let positional_parameters: Vec<_> = member
                .parameters
                .iter()
                .filter(|parameter| positional_parameter_section(parameter.section))
                .collect();
            let mut matched_required = HashSet::<Arc<str>>::new();
            let mut seen_named = HashSet::<Arc<str>>::new();
            let mut seen_exceptions = HashSet::<Arc<str>>::new();
            let mut positional_idx = 0usize;

            for argument in &call_site.arguments {
                if let Some(name) = argument.name.as_ref() {
                    if argument.section == Some(crate::NamedArgumentSection::Exceptions) {
                        if !seen_exceptions.insert(Arc::clone(name)) {
                            unit_diagnostics.push(Diagnostic {
                                kind: DiagnosticKind::DuplicateNamedParameter,
                                range: argument.range.clone(),
                                message: format!("duplicate method exception '{}'", name),
                            });
                        }
                        // Legacy CALL METHOD exceptions are not normal method parameters, and we do
                        // not currently model declared non-class-based method exceptions.
                        continue;
                    }
                    if !seen_named.insert(Arc::clone(name)) {
                        unit_diagnostics.push(Diagnostic {
                            kind: DiagnosticKind::DuplicateNamedParameter,
                            range: argument.range.clone(),
                            message: format!("duplicate named parameter '{}'", name),
                        });
                        continue;
                    }
                    let Some(parameter) = member.parameters.iter().find(|parameter| {
                        parameter.name == *name
                            && call_section_matches_parameter(argument.section, parameter.section)
                    }) else {
                        unit_diagnostics.push(Diagnostic {
                            kind: DiagnosticKind::UnknownNamedParameter,
                            range: argument.range.clone(),
                            message: format!(
                                "unknown named parameter '{}' for method '{}'",
                                name, member.name
                            ),
                        });
                        continue;
                    };
                    if parameter_is_required(parameter.section, parameter.is_optional) {
                        matched_required.insert(Arc::clone(&parameter.name));
                    }
                    if type_facts_parameter_compatibility(
                        project,
                        &lookup.type_fact_lookup,
                        target_unit,
                        &method_parameter_type_fact(parameter),
                        unit,
                        &argument.type_fact,
                    )
                    .is_incompatible()
                    {
                        unit_diagnostics.push(Diagnostic {
                            kind: DiagnosticKind::IncompatibleArgumentType,
                            range: argument.range.clone(),
                            message: format!(
                                "argument '{}' expects '{}', got '{}'",
                                parameter.name,
                                type_fact_label(&method_parameter_type_fact(parameter)),
                                type_fact_label(&argument.type_fact)
                            ),
                        });
                    }
                    continue;
                }

                let Some(parameter) = positional_parameters.get(positional_idx).copied() else {
                    continue;
                };
                positional_idx += 1;
                if parameter_is_required(parameter.section, parameter.is_optional) {
                    matched_required.insert(Arc::clone(&parameter.name));
                }
                if type_facts_parameter_compatibility(
                    project,
                    &lookup.type_fact_lookup,
                    target_unit,
                    &method_parameter_type_fact(parameter),
                    unit,
                    &argument.type_fact,
                )
                .is_incompatible()
                {
                    unit_diagnostics.push(Diagnostic {
                        kind: DiagnosticKind::IncompatibleArgumentType,
                        range: argument.range.clone(),
                        message: format!(
                            "argument for '{}' expects '{}', got '{}'",
                            parameter.name,
                            type_fact_label(&method_parameter_type_fact(parameter)),
                            type_fact_label(&argument.type_fact)
                        ),
                    });
                }
            }

            for parameter in &member.parameters {
                if parameter_is_required(parameter.section, parameter.is_optional)
                    && !matched_required.contains(&parameter.name)
                {
                    unit_diagnostics.push(Diagnostic {
                        kind: DiagnosticKind::MissingRequiredParameter,
                        range: call_site.range.clone(),
                        message: format!(
                            "missing required parameter '{}' for method '{}'",
                            parameter.name, member.name
                        ),
                    });
                }
            }
        }

        for named_argument in &unit.named_arguments {
            if let crate::NamedArgumentTarget::Routine { routine_name } = &named_argument.target {
                if let Some(spec) = builtin_routine_spec(routine_name.as_ref())
                    && !spec.supports_named_arguments
                {
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

            let Some(handle) = project.resolve_perform_call_target(unit, perform_call) else {
                continue;
            };
            let target_unit = &project.units[handle.unit.as_usize()];
            let Some(routine) = target_unit.form_routine(handle.symbol) else {
                continue;
            };
            let parameters = routine.parameters.as_slice();

            let expected_using = count_form_section(parameters, FormParameterSection::Using);
            let expected_changing = count_form_section(parameters, FormParameterSection::Changing);
            let actual_using =
                count_perform_section(&perform_call.parameters, PerformParameterSection::Using);
            let actual_changing =
                count_perform_section(&perform_call.parameters, PerformParameterSection::Changing);

            if expected_using != actual_using || expected_changing != actual_changing {
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
                continue;
            }

            for argument in &perform_call.arguments {
                let Some(parameter) = form_parameter_for_perform_argument(parameters, argument)
                else {
                    continue;
                };
                let Some(actual) = perform_argument_type_fact(unit, argument) else {
                    continue;
                };
                let expected = form_parameter_type_fact(target_unit, parameter);
                if type_facts_parameter_compatibility(
                    project,
                    &lookup.type_fact_lookup,
                    target_unit,
                    &expected,
                    unit,
                    &actual,
                )
                .is_incompatible()
                {
                    let parameter_name = &target_unit.symbol(parameter.symbol).name;
                    unit_diagnostics.push(Diagnostic {
                        kind: DiagnosticKind::IncompatibleArgumentType,
                        range: argument.range.clone(),
                        message: format!(
                            "argument '{}' expects '{}', got '{}'",
                            parameter_name,
                            type_fact_label(&expected),
                            type_fact_label(&actual)
                        ),
                    });
                }
            }
        }

        unit_diagnostics.extend(validate_open_sql_sources(&lookup, unit, &scope_index));
        unit_diagnostics.extend(validate_open_sql_fields(
            project,
            &lookup,
            unit,
            scope_indexes,
            &scope_index,
        ));
        unit_diagnostics.extend(order_by_validation.diagnostics);
        unit_diagnostics.extend(validate_open_sql_into_targets(
            project,
            &lookup,
            unit,
            scope_indexes,
        ));
        unit_diagnostics.extend(validate_missing_tables_declarations(
            project,
            &report_tables_contexts,
            unit,
        ));
        unit_diagnostics.extend(object_type_diagnostics);
        unit_diagnostics.extend(parameter_type_diagnostics);
        unit_diagnostics.extend(abstract_instantiation_diagnostics);
        unit_diagnostics.extend(validate_missing_method_implementations(unit));
        unit_diagnostics.extend(constructor_diagnostics);

        {
            let unit = &mut project.units[unit_idx];
            unit.diagnostics = unit_diagnostics;
        }
    }

    for unit in &mut project.units {
        if dirty_unit_ids.contains(&unit.unit_id) {
            unit.diagnostics
                .retain(|diagnostic| diagnostic.kind != DiagnosticKind::IncludeCycle);
        }
    }

    let mut visiting = HashSet::new();
    let mut visited = HashSet::new();
    for unit_id in dirty_unit_ids {
        detect_include_cycles(project, unit_id.0, &mut visiting, &mut visited);
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

#[cfg(test)]
mod tests {
    use super::loop_where_synthetic_decl_range;
    use crate::def_map::{StructureFieldInfo, StructureFieldShape};
    use crate::ids::{StructureId, UnitId};

    fn field_info(
        decl_unit: UnitId,
        decl_range: Option<std::ops::Range<usize>>,
    ) -> StructureFieldInfo {
        StructureFieldInfo {
            owner: StructureId(0),
            owner_unit: decl_unit,
            name: "field_a".into(),
            decl_range,
            decl_unit,
            shape: StructureFieldShape::Scalar,
            type_ref: None,
            is_key: false,
            value_clause_display: None,
        }
    }

    #[test]
    fn loop_where_synthetic_decl_range_keeps_local_field_decl_span() {
        let context_range = 120..128;
        assert_eq!(
            loop_where_synthetic_decl_range(
                UnitId(7),
                &field_info(UnitId(7), Some(36..44)),
                &context_range
            ),
            36..44
        );
    }

    #[test]
    fn loop_where_synthetic_decl_range_zeros_foreign_field_decl_span() {
        let context_range = 120..128;
        assert_eq!(
            loop_where_synthetic_decl_range(
                UnitId(7),
                &field_info(UnitId(3), Some(36..44)),
                &context_range
            ),
            120..120
        );
    }
}
