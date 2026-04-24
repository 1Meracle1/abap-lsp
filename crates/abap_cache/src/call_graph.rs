use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::Arc;

use abap_lexer::TextRange;
use abap_symbols::{
    CallSiteData, ClassMemberData, ClassMemberKind, NamedArgumentTarget, Namespace,
    ProjectAnalysis, ReferenceKind, Resolution, ScopeId, ScopeKind, SymbolData, SymbolHandle,
    SymbolId, SymbolKind, UnitAnalysis, UnitId,
};
use serde::Serialize;

use super::ScopeIndex;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CallGraphNodeKind {
    Method,
    Form,
    FunctionModule,
    EventBlock,
    Report,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CallGraphEdgeKind {
    MethodCall,
    Perform,
    FunctionCall,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CallGraphResolutionStatus {
    Resolved,
    Unresolved,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallGraphNode {
    pub id: Arc<str>,
    pub kind: CallGraphNodeKind,
    pub name: Arc<str>,
    pub qualified_name: Arc<str>,
    pub unit_uri: Arc<str>,
    pub decl_range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallGraphEdge {
    pub source: Arc<str>,
    pub target: Option<Arc<str>>,
    pub edge_kind: CallGraphEdgeKind,
    pub resolution_status: CallGraphResolutionStatus,
    pub target_name: Arc<str>,
    pub source_range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct MethodNodeKey {
    unit: UnitId,
    class_symbol: SymbolId,
    member_name: Arc<str>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct SymbolNodeKey {
    unit: UnitId,
    symbol: SymbolId,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ProjectCallGraph {
    pub nodes: Vec<CallGraphNode>,
    pub edges: Vec<CallGraphEdge>,
    node_positions: HashMap<Arc<str>, usize>,
    outbound_edges: HashMap<Arc<str>, Vec<usize>>,
    inbound_edges: HashMap<Arc<str>, Vec<usize>>,
    method_nodes: HashMap<MethodNodeKey, Arc<str>>,
    symbol_nodes: HashMap<SymbolNodeKey, Arc<str>>,
}

impl ProjectCallGraph {
    pub fn node(&self, id: &str) -> Option<&CallGraphNode> {
        self.node_positions
            .get(id)
            .and_then(|idx| self.nodes.get(*idx))
    }

    pub fn find_nodes(&self, query: &str) -> Vec<&CallGraphNode> {
        let lowered = query.trim().to_ascii_lowercase();
        if lowered.is_empty() {
            return Vec::new();
        }

        let exact: Vec<_> = self
            .nodes
            .iter()
            .filter(|node| {
                node.id.as_ref().eq_ignore_ascii_case(query)
                    || node.name.as_ref().eq_ignore_ascii_case(query)
                    || node.qualified_name.as_ref().eq_ignore_ascii_case(query)
            })
            .collect();
        if !exact.is_empty() {
            return exact;
        }

        self.nodes
            .iter()
            .filter(|node| {
                node.id.as_ref().to_ascii_lowercase().contains(&lowered)
                    || node.name.as_ref().to_ascii_lowercase().contains(&lowered)
                    || node
                        .qualified_name
                        .as_ref()
                        .to_ascii_lowercase()
                        .contains(&lowered)
            })
            .collect()
    }

    pub fn outbound_calls(&self, node_id: &str) -> Vec<&CallGraphEdge> {
        self.outbound_edges
            .get(node_id)
            .into_iter()
            .flat_map(|indices| indices.iter().filter_map(|idx| self.edges.get(*idx)))
            .collect()
    }

    pub fn inbound_callers(&self, node_id: &str) -> Vec<&CallGraphEdge> {
        self.inbound_edges
            .get(node_id)
            .into_iter()
            .flat_map(|indices| indices.iter().filter_map(|idx| self.edges.get(*idx)))
            .collect()
    }

    pub fn unresolved_outbound_calls(&self, node_id: &str) -> Vec<&CallGraphEdge> {
        self.outbound_calls(node_id)
            .into_iter()
            .filter(|edge| edge.resolution_status == CallGraphResolutionStatus::Unresolved)
            .collect()
    }

    pub fn method_node(
        &self,
        unit: UnitId,
        class_symbol: SymbolId,
        member_name: &str,
    ) -> Option<&CallGraphNode> {
        self.method_nodes
            .get(&MethodNodeKey {
                unit,
                class_symbol,
                member_name: Arc::from(member_name.to_ascii_lowercase()),
            })
            .and_then(|id| self.node(id))
    }

    pub fn symbol_node(&self, handle: SymbolHandle) -> Option<&CallGraphNode> {
        self.symbol_nodes
            .get(&SymbolNodeKey {
                unit: handle.unit,
                symbol: handle.symbol,
            })
            .and_then(|id| self.node(id))
    }

    fn build_indexes(&mut self) {
        self.node_positions.clear();
        self.outbound_edges.clear();
        self.inbound_edges.clear();
        for (idx, node) in self.nodes.iter().enumerate() {
            self.node_positions.insert(Arc::clone(&node.id), idx);
        }
        for (idx, edge) in self.edges.iter().enumerate() {
            self.outbound_edges
                .entry(Arc::clone(&edge.source))
                .or_default()
                .push(idx);
            if let Some(target) = edge.target.as_ref() {
                self.inbound_edges
                    .entry(Arc::clone(target))
                    .or_default()
                    .push(idx);
            }
        }
    }
}

pub(crate) fn build_project_call_graph(
    project: &ProjectAnalysis,
    scope_indexes: &[ScopeIndex],
) -> ProjectCallGraph {
    let mut graph = ProjectCallGraph::default();
    let mut callable_scopes = HashMap::<(UnitId, ScopeId), Arc<str>>::new();

    for unit in &project.units {
        collect_symbol_nodes(&mut graph, unit);
        collect_method_nodes(&mut graph, unit);
    }

    for unit in &project.units {
        index_callable_scopes(&graph, unit, &mut callable_scopes);
    }

    for unit in &project.units {
        let Some(scope_index) = scope_indexes.get(unit.unit_id.as_usize()) else {
            continue;
        };
        collect_call_site_edges(&mut graph, project, unit, scope_index, &callable_scopes);
        collect_perform_edges(&mut graph, project, unit, &callable_scopes);
        collect_reference_fallback_edges(&mut graph, project, unit, &callable_scopes);
    }

    graph.nodes.sort_by(|left, right| {
        left.id
            .cmp(&right.id)
            .then(left.unit_uri.cmp(&right.unit_uri))
            .then(left.decl_range.start.cmp(&right.decl_range.start))
            .then(left.decl_range.end.cmp(&right.decl_range.end))
    });
    graph.edges.sort_by(|left, right| {
        left.source
            .cmp(&right.source)
            .then(left.edge_kind.cmp(&right.edge_kind))
            .then(left.resolution_status.cmp(&right.resolution_status))
            .then(left.target.cmp(&right.target))
            .then(left.target_name.cmp(&right.target_name))
            .then(left.source_range.start.cmp(&right.source_range.start))
            .then(left.source_range.end.cmp(&right.source_range.end))
    });
    graph.build_indexes();
    graph
}

fn collect_symbol_nodes(graph: &mut ProjectCallGraph, unit: &UnitAnalysis) {
    for scope in &unit.scopes {
        let Some(owner) = scope.owner else {
            continue;
        };
        let Some((kind, qualified_name)) = callable_symbol_kind(unit, scope.kind, owner) else {
            continue;
        };
        let symbol = unit.symbol(owner);
        let node = CallGraphNode {
            id: symbol_node_id(unit, kind, symbol),
            kind,
            name: Arc::clone(&symbol.name),
            qualified_name,
            unit_uri: Arc::clone(&unit.uri),
            decl_range: symbol.decl_range.clone(),
        };
        graph.symbol_nodes.insert(
            SymbolNodeKey {
                unit: unit.unit_id,
                symbol: owner,
            },
            Arc::clone(&node.id),
        );
        graph.nodes.push(node);
    }

    for symbol in &unit.symbols {
        if symbol.kind != SymbolKind::Report {
            continue;
        }
        let node = CallGraphNode {
            id: symbol_node_id(unit, CallGraphNodeKind::Report, symbol),
            kind: CallGraphNodeKind::Report,
            name: Arc::clone(&symbol.name),
            qualified_name: Arc::clone(&symbol.name),
            unit_uri: Arc::clone(&unit.uri),
            decl_range: symbol.decl_range.clone(),
        };
        graph.symbol_nodes.insert(
            SymbolNodeKey {
                unit: unit.unit_id,
                symbol: symbol.id,
            },
            Arc::clone(&node.id),
        );
        graph.nodes.push(node);
    }
}

fn collect_method_nodes(graph: &mut ProjectCallGraph, unit: &UnitAnalysis) {
    for member in &unit.class_members {
        if member.kind != ClassMemberKind::Method {
            continue;
        }
        let owner = unit.symbol(member.class_symbol);
        let qualified_name = Arc::<str>::from(format!("{}~{}", owner.name, member.name));
        let node = CallGraphNode {
            id: method_node_id(unit, member),
            kind: CallGraphNodeKind::Method,
            name: Arc::clone(&member.name),
            qualified_name,
            unit_uri: Arc::clone(&unit.uri),
            decl_range: member.decl_range.clone(),
        };
        graph.method_nodes.insert(
            MethodNodeKey {
                unit: unit.unit_id,
                class_symbol: member.class_symbol,
                member_name: Arc::clone(&member.name),
            },
            Arc::clone(&node.id),
        );
        graph.nodes.push(node);
    }
}

fn index_callable_scopes(
    graph: &ProjectCallGraph,
    unit: &UnitAnalysis,
    callable_scopes: &mut HashMap<(UnitId, ScopeId), Arc<str>>,
) {
    for scope in &unit.scopes {
        let node_id = match scope.kind {
            ScopeKind::Form | ScopeKind::Module | ScopeKind::EventBlock => {
                scope.owner.and_then(|owner| {
                    graph
                        .symbol_nodes
                        .get(&SymbolNodeKey {
                            unit: unit.unit_id,
                            symbol: owner,
                        })
                        .cloned()
                })
            }
            ScopeKind::Method => method_node_id_for_scope(graph, unit, scope),
            ScopeKind::File => unit
                .symbols
                .iter()
                .find(|symbol| symbol.kind == SymbolKind::Report)
                .and_then(|symbol| {
                    graph
                        .symbol_nodes
                        .get(&SymbolNodeKey {
                            unit: unit.unit_id,
                            symbol: symbol.id,
                        })
                        .cloned()
                }),
            _ => None,
        };
        if let Some(node_id) = node_id {
            callable_scopes.insert((unit.unit_id, scope.id), node_id);
        }
    }
}

fn method_node_id_for_scope(
    graph: &ProjectCallGraph,
    unit: &UnitAnalysis,
    scope: &abap_symbols::ScopeData,
) -> Option<Arc<str>> {
    let member = scope
        .owner
        .and_then(|owner| {
            unit.semantic()
                .decls()
                .class_member_at_offset(unit.symbol(owner).decl_range.start)
        })
        .or_else(|| {
            unit.semantic()
                .decls()
                .class_member_at_offset(scope.range.start)
        })?;
    graph
        .method_nodes
        .get(&MethodNodeKey {
            unit: unit.unit_id,
            class_symbol: member.class_symbol,
            member_name: Arc::clone(&member.name),
        })
        .cloned()
}

fn collect_call_site_edges(
    graph: &mut ProjectCallGraph,
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    scope_index: &ScopeIndex,
    callable_scopes: &HashMap<(UnitId, ScopeId), Arc<str>>,
) {
    for call_site in &unit.call_sites {
        let Some(source) = enclosing_callable_node(unit, call_site.scope, callable_scopes) else {
            continue;
        };
        let Some(edge_kind) = call_site_edge_kind(&call_site.target) else {
            continue;
        };
        let resolved = resolve_call_site_target(project, unit, scope_index, call_site);
        let (target, resolution_status, target_name) = match resolved {
            Some(ResolvedCallTarget::Method { unit, member }) => (
                graph
                    .method_nodes
                    .get(&MethodNodeKey {
                        unit: unit.unit_id,
                        class_symbol: member.class_symbol,
                        member_name: Arc::clone(&member.name),
                    })
                    .cloned(),
                CallGraphResolutionStatus::Resolved,
                Arc::<str>::from(format!(
                    "{}~{}",
                    unit.symbol(member.class_symbol).name,
                    member.name
                )),
            ),
            Some(ResolvedCallTarget::FunctionModule { unit, symbol }) => (
                graph
                    .symbol_nodes
                    .get(&SymbolNodeKey {
                        unit: unit.unit_id,
                        symbol,
                    })
                    .cloned(),
                CallGraphResolutionStatus::Resolved,
                Arc::clone(&unit.symbol(symbol).name),
            ),
            Some(ResolvedCallTarget::Report { unit, symbol }) => (
                graph
                    .symbol_nodes
                    .get(&SymbolNodeKey {
                        unit: unit.unit_id,
                        symbol,
                    })
                    .cloned(),
                CallGraphResolutionStatus::Resolved,
                Arc::clone(&unit.symbol(symbol).name),
            ),
            None => (
                None,
                CallGraphResolutionStatus::Unresolved,
                unresolved_call_target_name(&call_site.target),
            ),
        };
        graph.edges.push(CallGraphEdge {
            source,
            target,
            edge_kind,
            resolution_status,
            target_name,
            source_range: call_site.range.clone(),
        });
    }
}

fn collect_perform_edges(
    graph: &mut ProjectCallGraph,
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    callable_scopes: &HashMap<(UnitId, ScopeId), Arc<str>>,
) {
    for perform_call in &unit.perform_calls {
        let Some(source) = enclosing_callable_node(unit, perform_call.scope, callable_scopes)
        else {
            continue;
        };
        let resolved = project.resolve_perform_call_target(unit, perform_call);
        let (target, resolution_status, target_name) = match resolved {
            Some(handle) => (
                graph
                    .symbol_nodes
                    .get(&SymbolNodeKey {
                        unit: handle.unit,
                        symbol: handle.symbol,
                    })
                    .cloned(),
                CallGraphResolutionStatus::Resolved,
                Arc::clone(&perform_call.routine_name),
            ),
            None => (
                None,
                CallGraphResolutionStatus::Unresolved,
                Arc::clone(&perform_call.routine_name),
            ),
        };
        graph.edges.push(CallGraphEdge {
            source,
            target,
            edge_kind: CallGraphEdgeKind::Perform,
            resolution_status,
            target_name,
            source_range: perform_call.range.clone(),
        });
    }
}

fn collect_reference_fallback_edges(
    graph: &mut ProjectCallGraph,
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    callable_scopes: &HashMap<(UnitId, ScopeId), Arc<str>>,
) {
    for reference in &unit.references {
        if reference.kind != ReferenceKind::RoutineCall || reference.namespace != Namespace::Routine
        {
            continue;
        }
        if perform_call_covers_reference(unit, reference.range.clone())
            || call_site_covers_reference(unit, reference)
        {
            continue;
        }

        let Some(source) = enclosing_callable_node(unit, reference.scope, callable_scopes) else {
            continue;
        };

        let edge = match reference.resolution {
            Some(Resolution::Symbol(handle)) => {
                let target_unit = &project.units[handle.unit.as_usize()];
                match target_unit.symbol(handle.symbol).kind {
                    SymbolKind::Module => Some(CallGraphEdge {
                        source,
                        target: graph
                            .symbol_nodes
                            .get(&SymbolNodeKey {
                                unit: handle.unit,
                                symbol: handle.symbol,
                            })
                            .cloned(),
                        edge_kind: CallGraphEdgeKind::FunctionCall,
                        resolution_status: CallGraphResolutionStatus::Resolved,
                        target_name: Arc::clone(&target_unit.symbol(handle.symbol).name),
                        source_range: reference.range.clone(),
                    }),
                    SymbolKind::Method => resolve_method_reference_target(project, handle).map(
                        |(member_unit, member)| CallGraphEdge {
                            source,
                            target: graph
                                .method_nodes
                                .get(&MethodNodeKey {
                                    unit: member_unit.unit_id,
                                    class_symbol: member.class_symbol,
                                    member_name: Arc::clone(&member.name),
                                })
                                .cloned(),
                            edge_kind: CallGraphEdgeKind::MethodCall,
                            resolution_status: CallGraphResolutionStatus::Resolved,
                            target_name: Arc::from(format!(
                                "{}~{}",
                                member_unit.symbol(member.class_symbol).name,
                                member.name
                            )),
                            source_range: reference.range.clone(),
                        },
                    ),
                    _ => None,
                }
            }
            Some(Resolution::BuiltinRoutine)
            | Some(Resolution::BuiltinType)
            | Some(Resolution::InternalTableLine)
            | Some(Resolution::External) => None,
            None => Some(CallGraphEdge {
                source,
                target: None,
                edge_kind: if enclosing_class_owner(unit, reference.scope).is_some() {
                    CallGraphEdgeKind::MethodCall
                } else {
                    CallGraphEdgeKind::FunctionCall
                },
                resolution_status: CallGraphResolutionStatus::Unresolved,
                target_name: Arc::clone(&reference.name),
                source_range: reference.range.clone(),
            }),
        };

        if let Some(edge) = edge {
            graph.edges.push(edge);
        }
    }
}

fn callable_symbol_kind(
    unit: &UnitAnalysis,
    scope_kind: ScopeKind,
    owner: SymbolId,
) -> Option<(CallGraphNodeKind, Arc<str>)> {
    let symbol = unit.symbol(owner);
    match (scope_kind, symbol.kind) {
        (ScopeKind::Form, SymbolKind::Form) => {
            Some((CallGraphNodeKind::Form, Arc::clone(&symbol.name)))
        }
        (ScopeKind::Module, SymbolKind::Module) if unit.function_module(owner).is_some() => {
            Some((CallGraphNodeKind::FunctionModule, Arc::clone(&symbol.name)))
        }
        (ScopeKind::EventBlock, SymbolKind::Event) => {
            Some((CallGraphNodeKind::EventBlock, Arc::clone(&symbol.name)))
        }
        (ScopeKind::File, SymbolKind::Report) => {
            Some((CallGraphNodeKind::Report, Arc::clone(&symbol.name)))
        }
        _ => None,
    }
}

fn symbol_node_id(unit: &UnitAnalysis, kind: CallGraphNodeKind, symbol: &SymbolData) -> Arc<str> {
    let prefix = match kind {
        CallGraphNodeKind::Method => "method",
        CallGraphNodeKind::Form => "form",
        CallGraphNodeKind::FunctionModule => "function",
        CallGraphNodeKind::EventBlock => "event",
        CallGraphNodeKind::Report => "report",
    };
    Arc::from(format!("{prefix}:{}#{}", unit.uri, symbol.name))
}

fn method_node_id(unit: &UnitAnalysis, member: &ClassMemberData) -> Arc<str> {
    let owner = unit.symbol(member.class_symbol);
    Arc::from(format!(
        "method:{}#{}~{}",
        unit.uri, owner.name, member.name
    ))
}

fn enclosing_callable_node(
    unit: &UnitAnalysis,
    scope: ScopeId,
    callable_scopes: &HashMap<(UnitId, ScopeId), Arc<str>>,
) -> Option<Arc<str>> {
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        if let Some(node_id) = callable_scopes.get(&(unit.unit_id, scope_id)) {
            return Some(Arc::clone(node_id));
        }
        current = unit.scope(scope_id).parent;
    }
    None
}

fn call_site_edge_kind(target: &NamedArgumentTarget) -> Option<CallGraphEdgeKind> {
    match target {
        NamedArgumentTarget::Constructor { .. }
        | NamedArgumentTarget::ImplicitMethod { .. }
        | NamedArgumentTarget::Method { .. } => Some(CallGraphEdgeKind::MethodCall),
        NamedArgumentTarget::Function { .. } | NamedArgumentTarget::Report { .. } => {
            Some(CallGraphEdgeKind::FunctionCall)
        }
        NamedArgumentTarget::Routine { .. } => None,
    }
}

fn unresolved_call_target_name(target: &NamedArgumentTarget) -> Arc<str> {
    match target {
        NamedArgumentTarget::Constructor { type_name } => {
            Arc::from(format!("{type_name}~constructor"))
        }
        NamedArgumentTarget::Function { function_name } => Arc::clone(function_name),
        NamedArgumentTarget::Report { report_name } => Arc::clone(report_name),
        NamedArgumentTarget::Routine { routine_name } => Arc::clone(routine_name),
        NamedArgumentTarget::ImplicitMethod { method_name } => Arc::clone(method_name),
        NamedArgumentTarget::Method {
            base_name,
            method_name,
            ..
        } => Arc::from(format!("{base_name}->{method_name}")),
    }
}

fn perform_call_covers_reference(unit: &UnitAnalysis, range: TextRange) -> bool {
    unit.perform_calls
        .iter()
        .any(|perform_call| perform_call.routine_range == range)
}

fn call_site_covers_reference(
    unit: &UnitAnalysis,
    reference: &abap_symbols::ReferenceData,
) -> bool {
    unit.call_sites.iter().any(|call_site| {
        if call_site.scope != reference.scope {
            return false;
        }
        let Some(target_name) = routine_target_name(&call_site.target) else {
            return false;
        };
        if target_name.as_ref() != reference.name.as_ref() {
            return false;
        }
        (call_site.range.start <= reference.range.start
            && reference.range.end <= call_site.range.end)
            || (reference.range.end <= call_site.range.start
                && call_site.range.start.saturating_sub(reference.range.end) <= 1)
    })
}

fn routine_target_name(target: &NamedArgumentTarget) -> Option<&Arc<str>> {
    match target {
        NamedArgumentTarget::Function { function_name } => Some(function_name),
        NamedArgumentTarget::Report { report_name } => Some(report_name),
        NamedArgumentTarget::ImplicitMethod { method_name } => Some(method_name),
        NamedArgumentTarget::Routine { routine_name } => Some(routine_name),
        NamedArgumentTarget::Constructor { .. } | NamedArgumentTarget::Method { .. } => None,
    }
}

enum ResolvedCallTarget<'a> {
    Method {
        unit: &'a UnitAnalysis,
        member: &'a ClassMemberData,
    },
    FunctionModule {
        unit: &'a UnitAnalysis,
        symbol: SymbolId,
    },
    Report {
        unit: &'a UnitAnalysis,
        symbol: SymbolId,
    },
}

fn resolve_call_site_target<'a>(
    project: &'a ProjectAnalysis,
    unit: &'a UnitAnalysis,
    scope_index: &ScopeIndex,
    call_site: &CallSiteData,
) -> Option<ResolvedCallTarget<'a>> {
    match &call_site.target {
        NamedArgumentTarget::Function { function_name } => {
            let handle = resolve_symbol_in_scope_or_project(
                project,
                unit,
                scope_index,
                call_site.scope,
                Namespace::Routine,
                function_name,
                |symbol| symbol.kind == SymbolKind::Module,
            )?;
            let target_unit = &project.units[handle.unit.as_usize()];
            target_unit
                .function_module(handle.symbol)
                .map(|_| ResolvedCallTarget::FunctionModule {
                    unit: target_unit,
                    symbol: handle.symbol,
                })
        }
        NamedArgumentTarget::Report { report_name } => {
            let handle = resolve_symbol_in_scope_or_project(
                project,
                unit,
                scope_index,
                call_site.scope,
                Namespace::Value,
                report_name,
                |symbol| symbol.kind == SymbolKind::Report,
            )?;
            let target_unit = &project.units[handle.unit.as_usize()];
            Some(ResolvedCallTarget::Report {
                unit: target_unit,
                symbol: handle.symbol,
            })
        }
        NamedArgumentTarget::Constructor { .. }
        | NamedArgumentTarget::ImplicitMethod { .. }
        | NamedArgumentTarget::Method { .. } => {
            let handle = resolve_method_target(
                project,
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
                NamedArgumentTarget::Function { .. }
                | NamedArgumentTarget::Report { .. }
                | NamedArgumentTarget::Routine { .. } => {
                    return None;
                }
            };
            if target_unit.symbol(handle.symbol).kind == SymbolKind::Interface {
                return target_unit
                    .class_member(handle.symbol, method_name)
                    .map(|member| ResolvedCallTarget::Method {
                        unit: target_unit,
                        member,
                    });
            }
            resolve_class_member_in_hierarchy(project, target_unit, handle.symbol, method_name)
                .or_else(|| {
                    target_unit
                        .class_member(handle.symbol, method_name)
                        .map(|member| (target_unit, member))
                })
                .map(|(member_unit, member)| ResolvedCallTarget::Method {
                    unit: member_unit,
                    member,
                })
        }
        NamedArgumentTarget::Routine { .. } => None,
    }
}

fn resolve_method_target(
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    scope_index: &ScopeIndex,
    scope: ScopeId,
    target: &NamedArgumentTarget,
) -> Option<SymbolHandle> {
    match target {
        NamedArgumentTarget::Constructor { type_name } => {
            resolve_type_symbol(project, unit, scope_index, scope, type_name)
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
            Namespace::Type => resolve_type_symbol(project, unit, scope_index, scope, base_name),
            Namespace::Value if base_name.as_ref().eq_ignore_ascii_case("super") => {
                let class_symbol = enclosing_class_owner(unit, scope)?;
                let inheritance = unit.class_superclass(class_symbol)?;
                resolve_type_symbol(
                    project,
                    unit,
                    scope_index,
                    scope,
                    &inheritance.superclass_name,
                )
            }
            Namespace::Value => {
                let handle = resolve_symbol_in_scope_or_project(
                    project,
                    unit,
                    scope_index,
                    scope,
                    Namespace::Value,
                    base_name,
                    |_| true,
                )?;
                let target_unit = &project.units[handle.unit.as_usize()];
                let declared_type = target_unit.symbol(handle.symbol).declared_type.as_ref()?;
                if !declared_type.is_ref || declared_type.namespace != Namespace::Type {
                    return None;
                }
                resolve_type_symbol(
                    project,
                    target_unit,
                    scope_index,
                    scope,
                    &declared_type.base_name,
                )
            }
            Namespace::Routine => None,
        },
        NamedArgumentTarget::Function { .. }
        | NamedArgumentTarget::Report { .. }
        | NamedArgumentTarget::Routine { .. } => None,
    }
}

fn resolve_type_symbol(
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    scope_index: &ScopeIndex,
    scope: ScopeId,
    name: &Arc<str>,
) -> Option<SymbolHandle> {
    resolve_symbol_in_scope_or_project(
        project,
        unit,
        scope_index,
        scope,
        Namespace::Type,
        name,
        |symbol| matches!(symbol.kind, SymbolKind::Class | SymbolKind::Interface),
    )
}

fn resolve_method_reference_target<'a>(
    project: &'a ProjectAnalysis,
    handle: SymbolHandle,
) -> Option<(&'a UnitAnalysis, &'a ClassMemberData)> {
    let target_unit = &project.units[handle.unit.as_usize()];
    let target_symbol = target_unit.symbol(handle.symbol);
    if target_symbol.kind != SymbolKind::Method {
        return None;
    }
    let class_symbol = enclosing_class_owner(target_unit, target_symbol.scope)?;
    let member_name = target_symbol
        .name
        .as_ref()
        .rsplit('~')
        .next()
        .unwrap_or(target_symbol.name.as_ref());
    resolve_class_member_in_hierarchy(project, target_unit, class_symbol, member_name).or_else(
        || {
            target_unit
                .class_member(class_symbol, member_name)
                .map(|member| (target_unit, member))
        },
    )
}

fn resolve_symbol_in_scope_or_project<F>(
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    scope_index: &ScopeIndex,
    scope: ScopeId,
    namespace: Namespace,
    name: &Arc<str>,
    predicate: F,
) -> Option<SymbolHandle>
where
    F: Fn(&SymbolData) -> bool,
{
    if let Some(symbol_id) = lookup_scope_chain(unit, scope_index, scope, namespace, name)
        && predicate(unit.symbol(symbol_id))
    {
        return Some(SymbolHandle {
            unit: unit.unit_id,
            symbol: symbol_id,
        });
    }

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
        let include_unit = &project.units[target_unit_id.as_usize()];
        if let Some(symbol_id) = include_unit
            .symbols
            .iter()
            .find(|symbol| {
                symbol.scope == include_unit.root_scope
                    && symbol.name == *name
                    && symbol.kind.occupies(namespace)
                    && predicate(symbol)
            })
            .map(|symbol| symbol.id)
        {
            return Some(SymbolHandle {
                unit: include_unit.unit_id,
                symbol: symbol_id,
            });
        }
        queue.extend(
            include_unit
                .include_edges
                .iter()
                .filter_map(|edge| edge.target),
        );
    }

    project.units.iter().find_map(|candidate| {
        candidate
            .symbols
            .iter()
            .find(|symbol| {
                symbol.scope == candidate.root_scope
                    && symbol.name == *name
                    && symbol.kind.occupies(namespace)
                    && predicate(symbol)
            })
            .map(|symbol| SymbolHandle {
                unit: candidate.unit_id,
                symbol: symbol.id,
            })
    })
}

fn resolve_class_member_in_hierarchy<'a>(
    project: &'a ProjectAnalysis,
    class_unit: &'a UnitAnalysis,
    class_symbol: SymbolId,
    member_name: &str,
) -> Option<(&'a UnitAnalysis, &'a ClassMemberData)> {
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
        if let Some(member) = unit.class_member(current.symbol, member_name)
            && !class_member_uses_inherited_signature(member)
        {
            return Some((unit, member));
        }
        current = direct_superclass_handle(project, unit, current.symbol)?;
    }
}

fn class_member_uses_inherited_signature(member: &ClassMemberData) -> bool {
    member.kind == ClassMemberKind::Method
        && member.parameters.is_empty()
        && member.signature.split_ascii_whitespace().any(|part| {
            let keyword = part.trim_end_matches('.');
            keyword.eq_ignore_ascii_case("redefinition")
        })
}

fn direct_superclass_handle(
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    class_symbol: SymbolId,
) -> Option<SymbolHandle> {
    let inheritance = unit.class_superclass(class_symbol)?;
    project.units.iter().find_map(|candidate| {
        candidate
            .symbols
            .iter()
            .find(|symbol| {
                symbol.scope == candidate.root_scope
                    && symbol.name == inheritance.superclass_name
                    && symbol.kind == SymbolKind::Class
            })
            .map(|symbol| SymbolHandle {
                unit: candidate.unit_id,
                symbol: symbol.id,
            })
    })
}

fn lookup_scope_chain(
    unit: &UnitAnalysis,
    scope_index: &ScopeIndex,
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

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::{DocumentInput, DocumentStore};

    use super::{
        CallGraphEdgeKind, CallGraphNodeKind, CallGraphResolutionStatus, ProjectCallGraph,
    };

    fn graph_for(inputs: Vec<DocumentInput>, target_uri: &str) -> ProjectCallGraph {
        let store = DocumentStore::default();
        let snapshots = store.replace_all(inputs);
        snapshots
            .get(target_uri)
            .expect("target snapshot")
            .call_graph()
            .clone()
    }

    #[test]
    fn resolves_method_to_method_edges() {
        let graph = graph_for(
            vec![DocumentInput {
                uri: Arc::from("file:///method.abap"),
                version: 1,
                text: Arc::from(
                    "\
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS helper.
    METHODS run.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD helper.
  ENDMETHOD.

  METHOD run.
    helper( ).
  ENDMETHOD.
ENDCLASS.",
                ),
                is_dependency: false,
                object_name: None,
            }],
            "file:///method.abap",
        );

        let run = graph.find_nodes("zcl_demo~run")[0];
        let helper = graph.find_nodes("zcl_demo~helper")[0];
        let outbound = graph.outbound_calls(run.id.as_ref());
        assert_eq!(outbound.len(), 1);
        assert_eq!(outbound[0].edge_kind, CallGraphEdgeKind::MethodCall);
        assert_eq!(
            outbound[0].resolution_status,
            CallGraphResolutionStatus::Resolved
        );
        assert_eq!(outbound[0].target.as_deref(), Some(helper.id.as_ref()));
        let inbound = graph.inbound_callers(helper.id.as_ref());
        assert_eq!(inbound.len(), 1);
        assert_eq!(inbound[0].source.as_ref(), run.id.as_ref());
    }

    #[test]
    fn resolves_perform_edges_across_include_units() {
        let graph = graph_for(
            vec![
                DocumentInput {
                    uri: Arc::from("file:///main.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
REPORT zmain.
INCLUDE zforms.

START-OF-SELECTION.
  PERFORM do_work.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///zforms.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
FORM do_work.
ENDFORM.",
                    ),
                    is_dependency: true,
                    object_name: None,
                },
            ],
            "file:///main.abap",
        );

        let event = graph.find_nodes("start-of-selection")[0];
        let form = graph
            .find_nodes("do_work")
            .into_iter()
            .find(|node| node.kind == CallGraphNodeKind::Form)
            .expect("form node");
        let outbound = graph.outbound_calls(event.id.as_ref());
        assert_eq!(outbound.len(), 1);
        assert_eq!(outbound[0].edge_kind, CallGraphEdgeKind::Perform);
        assert_eq!(outbound[0].target.as_deref(), Some(form.id.as_ref()));
    }

    #[test]
    fn resolves_static_perform_in_program_edges_to_target_program_form() {
        let graph = graph_for(
            vec![
                DocumentInput {
                    uri: Arc::from("file:///caller.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
REPORT zcaller.

START-OF-SELECTION.
  PERFORM do_work IN PROGRAM zcallee IF FOUND.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///callee.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
REPORT zcallee.
FORM do_work.
ENDFORM.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
            ],
            "file:///caller.abap",
        );

        let event = graph.find_nodes("start-of-selection")[0];
        let form = graph
            .find_nodes("do_work")
            .into_iter()
            .find(|node| {
                node.kind == CallGraphNodeKind::Form
                    && node.unit_uri.as_ref() == "file:///callee.abap"
            })
            .expect("callee form node");
        let outbound = graph.outbound_calls(event.id.as_ref());
        assert_eq!(outbound.len(), 1);
        assert_eq!(outbound[0].edge_kind, CallGraphEdgeKind::Perform);
        assert_eq!(
            outbound[0].resolution_status,
            CallGraphResolutionStatus::Resolved
        );
        assert_eq!(outbound[0].target.as_deref(), Some(form.id.as_ref()));
    }

    #[test]
    fn resolves_chained_perform_edges() {
        let graph = graph_for(
            vec![DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(
                    "\
REPORT zmain.

FORM do_work USING iv_name TYPE string.
ENDFORM.

START-OF-SELECTION.
  PERFORM do_work USING:
    'first',
    'second'.",
                ),
                is_dependency: false,
                object_name: None,
            }],
            "file:///main.abap",
        );

        let event = graph.find_nodes("start-of-selection")[0];
        let form = graph
            .find_nodes("do_work")
            .into_iter()
            .find(|node| node.kind == CallGraphNodeKind::Form)
            .expect("form node");
        let outbound = graph.outbound_calls(event.id.as_ref());
        assert_eq!(outbound.len(), 2);
        assert!(outbound.iter().all(|edge| {
            edge.edge_kind == CallGraphEdgeKind::Perform
                && edge.target.as_deref() == Some(form.id.as_ref())
        }));
    }

    #[test]
    fn resolves_call_function_edges_across_units() {
        let graph = graph_for(
            vec![
                DocumentInput {
                    uri: Arc::from("file:///caller.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
REPORT zcaller.

START-OF-SELECTION.
  CALL FUNCTION 'z_demo_fn'.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///z_demo_fn.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
FUNCTION z_demo_fn.
ENDFUNCTION.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
            ],
            "file:///caller.abap",
        );

        let event = graph.find_nodes("start-of-selection")[0];
        let function = graph
            .find_nodes("z_demo_fn")
            .into_iter()
            .find(|node| node.kind == CallGraphNodeKind::FunctionModule)
            .expect("function module node");
        let outbound = graph.outbound_calls(event.id.as_ref());
        assert_eq!(outbound.len(), 1);
        assert_eq!(outbound[0].edge_kind, CallGraphEdgeKind::FunctionCall);
        assert_eq!(outbound[0].target.as_deref(), Some(function.id.as_ref()));
    }

    #[test]
    fn resolves_inherited_method_targets_to_parent_member() {
        let graph = graph_for(
            vec![
                DocumentInput {
                    uri: Arc::from("file:///base.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
CLASS zcl_base DEFINITION.
  PUBLIC SECTION.
    METHODS helper.
ENDCLASS.

CLASS zcl_base IMPLEMENTATION.
  METHOD helper.
  ENDMETHOD.
ENDCLASS.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///child.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
CLASS zcl_child DEFINITION INHERITING FROM zcl_base.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS zcl_child IMPLEMENTATION.
  METHOD run.
    helper( ).
  ENDMETHOD.
ENDCLASS.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
            ],
            "file:///child.abap",
        );

        let run = graph.find_nodes("zcl_child~run")[0];
        let helper = graph.find_nodes("zcl_base~helper")[0];
        let outbound = graph.outbound_calls(run.id.as_ref());
        assert_eq!(outbound.len(), 1);
        assert_eq!(outbound[0].target.as_deref(), Some(helper.id.as_ref()));
    }

    #[test]
    fn keeps_unresolved_call_edges() {
        let graph = graph_for(
            vec![DocumentInput {
                uri: Arc::from("file:///unresolved.abap"),
                version: 1,
                text: Arc::from(
                    "\
REPORT zunresolved.

START-OF-SELECTION.
  CALL FUNCTION lv_func.",
                ),
                is_dependency: false,
                object_name: None,
            }],
            "file:///unresolved.abap",
        );

        let event = graph.find_nodes("start-of-selection")[0];
        let unresolved = graph.unresolved_outbound_calls(event.id.as_ref());
        assert_eq!(unresolved.len(), 1);
        assert_eq!(unresolved[0].edge_kind, CallGraphEdgeKind::FunctionCall);
        assert_eq!(
            unresolved[0].resolution_status,
            CallGraphResolutionStatus::Unresolved
        );
        assert!(unresolved[0].target.is_none());
        assert_eq!(unresolved[0].target_name.as_ref(), "lv_func");
    }

    #[test]
    fn submit_creates_report_call_edges() {
        let graph = graph_for(
            vec![
                DocumentInput {
                    uri: Arc::from("file:///caller.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
REPORT zcaller.

START-OF-SELECTION.
  SUBMIT ztarget AND RETURN.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///target.abap"),
                    version: 1,
                    text: Arc::from("REPORT ztarget.\n"),
                    is_dependency: false,
                    object_name: None,
                },
            ],
            "file:///caller.abap",
        );

        let caller = graph.find_nodes("start-of-selection")[0];
        let target = graph.find_nodes("ztarget")[0];
        let outbound = graph.outbound_calls(caller.id.as_ref());
        assert_eq!(outbound.len(), 1);
        assert_eq!(outbound[0].edge_kind, CallGraphEdgeKind::FunctionCall);
        assert_eq!(outbound[0].target.as_deref(), Some(target.id.as_ref()));
    }
}
