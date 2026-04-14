use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::Arc;

use abap_lexer::TextRange;

use crate::builtins::builtin_routine_spec;
use crate::compatibility::{
    call_section_matches_parameter, parameter_is_required, positional_parameter_section,
    type_facts_compatible,
};
use crate::def_map::{
    Diagnostic, DiagnosticKind, FieldTypeRefData, FormParameterData, FormParameterSection,
    FunctionModuleData, FunctionModuleParameterData, FunctionModuleParameterSection,
    LoopWhereFieldContext, NamedArgumentTarget, PerformParameterSection, ReferenceKind, Resolution,
    SqlNameRefKind, StructureFieldShape, TypeFactData,
};
use crate::ids::{ScopeId, StructureId, SymbolHandle, SymbolId, UnitId};
use crate::project::ProjectAnalysis;
use crate::resolver::{ScopeIndex, build_scope_index};
use crate::scope::{Namespace, ScopeKind};
use crate::{ClassMemberKind, SymbolKind, Visibility};

struct ValidationLookup<'a> {
    scope_indexes: &'a [ScopeIndex],
    per_unit_root_index: Vec<HashMap<(Namespace, Arc<str>), Vec<SymbolId>>>,
    root_index: HashMap<(Namespace, Arc<str>), Vec<SymbolHandle>>,
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
    }
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
            (*handle).unit != preferred_unit.unit_id
                && predicate(project.units[handle.unit.as_usize()].symbol(handle.symbol))
        })
    })
}

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
        current = direct_superclass_handle(project, lookup, unit, current.symbol)?;
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
    let mut next_symbol_id = unit.symbols.len() as u32;

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
            let id = SymbolId(next_symbol_id);
            next_symbol_id += 1;
            out.push((
                scope_id,
                crate::SymbolData {
                    id,
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
            let id = SymbolId(next_symbol_id);
            next_symbol_id += 1;
            out.push((
                scope_id,
                crate::SymbolData {
                    id,
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
    let mut next_symbol_id = unit.symbols.len() as u32;

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
            let id = SymbolId(next_symbol_id);
            next_symbol_id += 1;
            out.push((
                scope_id,
                crate::SymbolData {
                    id,
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

fn loop_where_scope_symbol_specs(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
) -> Vec<(ScopeId, crate::SymbolData)> {
    let mut out = Vec::new();
    let mut next_symbol_id = unit.symbols.len() as u32;
    let mut seen: HashSet<(u32, Arc<str>)> = HashSet::new();

    for context in &unit.loop_where_field_contexts {
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

                    let id = SymbolId(next_symbol_id);
                    next_symbol_id += 1;
                    out.push((
                        scope,
                        crate::SymbolData {
                            id,
                            name,
                            kind: crate::SymbolKind::Variable,
                            scope,
                            decl_range: loop_where_synthetic_decl_range(
                                unit.unit_id,
                                &field,
                                &context.range,
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
            resolve_loop_where_source_structure(project, lookup, unit, scope_indexes, context)
        {
            push_fields(context.scope, fields_unit, structure_id);
        }
        if let Some((fields_unit, structure_id)) =
            context.target_access.as_ref().and_then(|access| {
                resolve_field_access_structure(project, lookup, unit, scope_indexes, access)
            })
        {
            push_fields(context.scope, fields_unit, structure_id);
        }
    }

    out
}

fn loop_where_synthetic_decl_range(
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

fn method_parameter_type_fact(parameter: &crate::ClassMemberParameterData) -> TypeFactData {
    TypeFactData {
        structure: None,
        declared_type: parameter.declared_type.clone(),
        type_clause_display: parameter.type_clause_display.clone(),
        table_line: None,
    }
}

fn function_module_parameter_type_fact(parameter: &FunctionModuleParameterData) -> TypeFactData {
    TypeFactData {
        structure: None,
        declared_type: parameter.declared_type.clone(),
        type_clause_display: parameter.type_clause_display.clone(),
        table_line: None,
    }
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
        NamedArgumentTarget::Function { .. } | NamedArgumentTarget::Routine { .. } => None,
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
        NamedArgumentTarget::Function { .. } | NamedArgumentTarget::Routine { .. } => return None,
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

fn symbol_type_clause_suggests_internal_table(symbol: &crate::SymbolData) -> bool {
    let Some(display) = symbol.type_clause_display.as_deref() else {
        return false;
    };
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

fn structure_has_proxy_include_fields(
    current_unit: &crate::UnitAnalysis,
    structure_id: StructureId,
) -> bool {
    current_unit
        .semantic()
        .decls()
        .structure_field_infos(structure_id)
        .iter()
        .any(field_looks_like_ddic_proxy_include)
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

fn resolve_loop_where_source_structure<'a>(
    project: &'a ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &'a crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    context: &LoopWhereFieldContext,
) -> Option<(&'a crate::UnitAnalysis, StructureId)> {
    if context.source_access.base_namespace != Namespace::Value {
        return None;
    }
    let scope_index = &scope_indexes[unit.unit_id.as_usize()];
    let base_symbol_id = resolve_symbol_in_scope_chain(
        unit,
        scope_index,
        context.scope,
        Namespace::Value,
        &context.source_access.base_name,
    )?;
    let (current_unit, mut current_structure) = resolve_symbol_structure_project(
        project,
        lookup,
        unit,
        scope_indexes,
        context.scope,
        base_symbol_id,
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

fn loop_where_reference_matches_source_field(
    project: &ProjectAnalysis,
    lookup: &ValidationLookup<'_>,
    unit: &crate::UnitAnalysis,
    scope_indexes: &[ScopeIndex],
    reference: &crate::ReferenceData,
) -> bool {
    if reference.namespace != Namespace::Value || reference.kind != ReferenceKind::Identifier {
        return false;
    }
    unit.loop_where_field_contexts.iter().any(|context| {
        context.range.start <= reference.range.start
            && reference.range.end <= context.range.end
            && {
                let source_matches = resolve_loop_where_source_structure(
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
                        || structure_has_proxy_include_fields(structure_unit, structure_id)
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
                                || structure_has_proxy_include_fields(structure_unit, structure_id)
                        })
            }
    })
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
    symbol
        .type_clause_display
        .as_deref()
        .is_some_and(|display| {
            let upper = display.to_ascii_uppercase();
            upper.contains("BEGIN OF")
        })
}

fn into_target_identifier_range(
    unit: &crate::UnitAnalysis,
    target: &crate::def_map::SqlTargetData,
    name: &Arc<str>,
) -> std::ops::Range<usize> {
    unit.references
        .iter()
        .filter(|reference| {
            reference.namespace == Namespace::Value
                && reference.kind == ReferenceKind::Identifier
                && reference.name.as_ref().eq_ignore_ascii_case(name.as_ref())
                && reference.range.start >= target.range.start
                && reference.range.end <= target.range.end
        })
        .min_by_key(|reference| reference.range.end.saturating_sub(reference.range.start))
        .map(|reference| reference.range.clone())
        .unwrap_or_else(|| target.range.clone())
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

#[allow(dead_code)]
pub fn validate_project(project: &mut ProjectAnalysis) {
    let scope_indexes: Vec<_> = project.units.iter().map(build_scope_index).collect();
    validate_project_with_scope_indexes(project, &scope_indexes);
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
        if !dirty_unit_ids.contains(&project.units[unit_idx].unit_id) {
            continue;
        }
        let mut scope_index = scope_indexes[unit_idx].clone();
        let synthetic_symbols = {
            let unit = &project.units[unit_idx];
            let mut symbols = qualified_interface_method_scope_symbol_specs(project, &lookup, unit);
            symbols.extend(inherited_redefinition_method_scope_symbol_specs(
                project, &lookup, unit,
            ));
            symbols.extend(loop_where_scope_symbol_specs(
                project,
                &lookup,
                unit,
                scope_indexes,
            ));
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
                    let symbol_id = resolve_symbol_in_scope_chain(
                        unit,
                        &scope_index,
                        reference.scope,
                        reference.namespace,
                        &reference.name,
                    )?;
                    Some((idx, symbol_id))
                })
                .collect()
        };
        {
            let unit = &mut project.units[unit_idx];
            for (idx, symbol_id) in synthetic_reference_resolutions {
                unit.references[idx].resolution = Some(Resolution::Symbol(SymbolHandle {
                    unit: unit.unit_id,
                    symbol: symbol_id,
                }));
            }
        }
        let scope_names = build_scope_names(&project.units[unit_idx]);
        let constructor_diagnostics = validate_super_constructor_calls(
            project,
            &lookup,
            &project.units[unit_idx],
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
                )
            })
            .cloned()
            .collect();
        let mut unit_diagnostics = retained;

        for reference in &unit.references {
            if reference.resolution.is_some() {
                continue;
            }
            if loop_where_reference_matches_source_field(
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
            ) {
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
            let Some(((base_unit_idx, base_symbol_id), class_selector_base)) = *base_info else {
                continue;
            };
            let base_unit = &project.units[base_unit_idx];
            let base_scope_index = &scope_indexes[base_unit_idx];
            let access_scope = scope_for_unit(base_unit, access.scope);
            let base_symbol = base_unit.symbol(base_symbol_id);
            let (has_leading_deref, field_path) = split_leading_deref(access);
            if access.base_namespace == Namespace::Type && base_symbol.kind == SymbolKind::Class {
                let mut idx = 0usize;
                let mut structure_tail: Option<crate::StructureId> = None;
                let mut static_structure_holder: Option<Arc<str>> = None;
                while idx < field_path.len() {
                    let step = &field_path[idx];
                    if let Some(structure_id) = structure_tail {
                        let holder = static_structure_holder.as_deref().unwrap_or("?");
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
                        let Some(next_structure) = field.structure else {
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
                        structure_tail = Some(next_structure);
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
                            structure_tail = Some(next_structure);
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
                    structure_tail = Some(next_structure);
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
                }
                for (idx, step) in field_path.iter().enumerate() {
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
                    let Some((next_structure_id, next_declared_type)) = dereference_field_metadata(
                        base_unit,
                        base_scope_index,
                        access_scope,
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

                (structure_id, declared_type) = normalize_field_metadata(
                    base_unit,
                    base_scope_index,
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
                    base_unit,
                    access_scope,
                    current_structure_id,
                    step.name.as_ref(),
                ) else {
                    if structure_has_proxy_include_fields(base_unit, current_structure_id) {
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
                structure_id = match field.shape {
                    StructureFieldShape::Structured { structure } => Some(structure),
                    StructureFieldShape::Scalar => None,
                };
                declared_type = field.type_ref.clone();
            }
        }

        for assignment in &unit.assignment_sites {
            if matches!(
                type_facts_compatible(project, unit, &assignment.lhs, unit, &assignment.rhs),
                Some(false)
            ) {
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
                                kind: DiagnosticKind::UnknownNamedParameter,
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
                    if matches!(
                        type_facts_compatible(
                            project,
                            target_unit,
                            &function_module_parameter_type_fact(parameter),
                            unit,
                            &argument.type_fact,
                        ),
                        Some(false)
                    ) {
                        unit_diagnostics.push(Diagnostic {
                            kind: DiagnosticKind::IncompatibleArgumentType,
                            range: argument.range.clone(),
                            message: format!(
                                "argument '{}' expects '{}', got '{}'",
                                parameter.name,
                                type_fact_label(&function_module_parameter_type_fact(parameter)),
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
            let mut positional_idx = 0usize;

            for argument in &call_site.arguments {
                if let Some(name) = argument.name.as_ref() {
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
                    if matches!(
                        type_facts_compatible(
                            project,
                            target_unit,
                            &method_parameter_type_fact(parameter),
                            unit,
                            &argument.type_fact,
                        ),
                        Some(false)
                    ) {
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
                if matches!(
                    type_facts_compatible(
                        project,
                        target_unit,
                        &method_parameter_type_fact(parameter),
                        unit,
                        &argument.type_fact,
                    ),
                    Some(false)
                ) {
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

        unit_diagnostics.extend(validate_open_sql_sources(&lookup, unit, &scope_index));
        unit_diagnostics.extend(validate_open_sql_into_targets(
            project,
            &lookup,
            unit,
            scope_indexes,
        ));
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
