use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};
use std::sync::Arc;

use abap_lexer::TextRange;
use abap_symbols::{
    AssignmentSiteData, CallSiteData, NamedArgumentSection, NamedArgumentTarget, Namespace,
    PerformCallData, ProjectAnalysis, Resolution, RoutineAnalysis, ScopeId, SqlNameRefKind,
    SqlProjectionData, SqlProjectionKind, SqlQueryData, SqlSourceData, SqlSourceKind,
    SqlTargetData, SqlTargetKind, SymbolHandle, SymbolKind, UnitAnalysis,
};
use serde::Serialize;

use super::{AnalysisSnapshot, CallGraphEdgeKind, CallGraphNodeKind, CallableParameterDirection};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct CallDataflowByteRange {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CallDataflowQuery {
    pub target: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub caller: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub occurrence: Option<usize>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub struct CallDataflowLifecycle {
    pub nodes: Vec<CallDataflowLifecycleNode>,
    pub edges: Vec<CallDataflowLifecycleEdge>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CallDataflowLifecycleNode {
    pub id: String,
    pub kind: String,
    pub name: String,
    pub unit_uri: String,
    pub decl_range: CallDataflowByteRange,
    pub synthetic: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CallDataflowLifecycleEdge {
    pub source: String,
    pub target: String,
    pub kind: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub label: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_range: Option<CallDataflowByteRange>,
    pub synthetic: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CallDataflowMatch {
    pub occurrence: usize,
    pub target_kind: String,
    pub target_name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub caller_kind: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub caller_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub caller_unit_uri: Option<String>,
    pub unit_uri: String,
    pub call_range: CallDataflowByteRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CallDataflowSelectedCall {
    pub occurrence: usize,
    pub target_kind: String,
    pub target_name: String,
    pub unit_uri: String,
    pub call_range: CallDataflowByteRange,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub caller_node_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub caller_kind: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub caller_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub caller_unit_uri: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub target_node_id: Option<String>,
    pub argument_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CallDataflowFieldMapping {
    pub target_path: String,
    pub source_kind: String,
    pub source_display: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_unit_uri: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_range: Option<CallDataflowByteRange>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub statement_text: Option<String>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub struct CallDataflowProvenanceGraph {
    pub nodes: Vec<CallDataflowProvenanceNode>,
    pub edges: Vec<CallDataflowProvenanceEdge>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CallDataflowProvenanceNode {
    pub id: String,
    pub kind: String,
    pub label: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub unit_uri: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub range: Option<CallDataflowByteRange>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub statement_text: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CallDataflowProvenanceEdge {
    pub source: String,
    pub target: String,
    pub kind: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub label: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CallDataflowParameterTrace {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parameter_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub section: Option<String>,
    pub direction: String,
    pub argument_text: String,
    pub argument_range: CallDataflowByteRange,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub argument_type: Option<String>,
    pub field_mappings: Vec<CallDataflowFieldMapping>,
    #[serde(skip_serializing_if = "call_dataflow_provenance_is_empty")]
    pub provenance: CallDataflowProvenanceGraph,
    pub notes: Vec<String>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub struct CallDataflowSummary {
    pub match_count: usize,
    pub ambiguous: bool,
    pub lifecycle_node_count: usize,
    pub lifecycle_edge_count: usize,
    pub synthetic_edge_count: usize,
    pub parameter_count: usize,
    pub mapping_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CallDataflowTrace {
    pub schema: &'static str,
    pub schema_version: u32,
    pub query: CallDataflowQuery,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub selected_call: Option<CallDataflowSelectedCall>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub matches: Vec<CallDataflowMatch>,
    pub lifecycle: CallDataflowLifecycle,
    pub parameter_traces: Vec<CallDataflowParameterTrace>,
    pub summary: CallDataflowSummary,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TraceDirection {
    Input,
    Output,
    InOut,
}

impl TraceDirection {
    const fn as_str(self) -> &'static str {
        match self {
            Self::Input => "input",
            Self::Output => "output",
            Self::InOut => "in_out",
        }
    }

    const fn is_input_like(self) -> bool {
        matches!(self, Self::Input | Self::InOut)
    }
}

#[derive(Debug, Clone)]
struct MatchedCall<'a> {
    occurrence: usize,
    unit: &'a UnitAnalysis,
    call: &'a CallSiteData,
    caller: Option<&'a RoutineAnalysis>,
    caller_node_id: Option<Arc<str>>,
    target_node_id: Option<Arc<str>>,
    target_kind: String,
    target_name: String,
}

#[derive(Debug, Clone)]
struct ValueAccess {
    handle: SymbolHandle,
    field_path: Vec<String>,
    display: String,
    range: TextRange,
}

#[derive(Debug, Clone, Copy)]
struct TraceContext<'a> {
    unit: &'a UnitAnalysis,
    routine: Option<&'a RoutineAnalysis>,
    sink_offset: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TraceVisitKey {
    context_owner: Option<(u32, u32)>,
    symbol: (u32, u32),
    field_path: Vec<String>,
    target_path: String,
}

#[derive(Debug, Default)]
struct ParameterProvenanceBuilder {
    parameter_path: String,
    graph: CallDataflowProvenanceGraph,
    node_keys: HashMap<String, String>,
    edge_keys: HashSet<String>,
    next_node_id: usize,
}

pub fn build_call_dataflow_trace(
    snapshot: &AnalysisSnapshot,
    query: CallDataflowQuery,
) -> CallDataflowTrace {
    let mut builder = TraceBuilder::new(snapshot);
    builder.build(query)
}

struct TraceBuilder<'a> {
    snapshot: &'a AnalysisSnapshot,
    visited: HashSet<TraceVisitKey>,
}

impl<'a> TraceBuilder<'a> {
    fn new(snapshot: &'a AnalysisSnapshot) -> Self {
        Self {
            snapshot,
            visited: HashSet::new(),
        }
    }

    fn build(&mut self, query: CallDataflowQuery) -> CallDataflowTrace {
        let matches = self.match_calls(&query);
        let summary_matches = matches.len();
        if matches.len() > 1 && query.occurrence.is_none() {
            let matches = matches
                .into_iter()
                .map(|matched| self.match_json(&matched))
                .collect::<Vec<_>>();
            return CallDataflowTrace {
                schema: "abap.call_dataflow_trace",
                schema_version: 1,
                query,
                selected_call: None,
                matches,
                lifecycle: CallDataflowLifecycle::default(),
                parameter_traces: Vec::new(),
                summary: CallDataflowSummary {
                    match_count: summary_matches,
                    ambiguous: true,
                    ..CallDataflowSummary::default()
                },
            };
        }

        let selected = if let Some(occurrence) = query.occurrence {
            matches
                .into_iter()
                .find(|matched| matched.occurrence == occurrence)
        } else {
            matches.into_iter().next()
        };

        let Some(selected) = selected else {
            return CallDataflowTrace {
                schema: "abap.call_dataflow_trace",
                schema_version: 1,
                query,
                selected_call: None,
                matches: Vec::new(),
                lifecycle: CallDataflowLifecycle::default(),
                parameter_traces: Vec::new(),
                summary: CallDataflowSummary {
                    match_count: summary_matches,
                    ambiguous: false,
                    ..CallDataflowSummary::default()
                },
            };
        };

        let lifecycle = self.build_lifecycle(&selected);
        let parameter_traces = self.build_parameter_traces(&selected);
        let mapping_count = parameter_traces
            .iter()
            .map(|trace| trace.field_mappings.len())
            .sum();
        let synthetic_edge_count = lifecycle.edges.iter().filter(|edge| edge.synthetic).count();
        let summary = CallDataflowSummary {
            match_count: summary_matches,
            ambiguous: false,
            lifecycle_node_count: lifecycle.nodes.len(),
            lifecycle_edge_count: lifecycle.edges.len(),
            synthetic_edge_count,
            parameter_count: parameter_traces.len(),
            mapping_count,
        };

        CallDataflowTrace {
            schema: "abap.call_dataflow_trace",
            schema_version: 1,
            query,
            selected_call: Some(self.selected_call_json(&selected)),
            matches: Vec::new(),
            lifecycle,
            parameter_traces,
            summary,
        }
    }

    fn match_calls(&self, query: &CallDataflowQuery) -> Vec<MatchedCall<'a>> {
        let mut matched = Vec::new();
        let target_query = query.target.trim().to_ascii_lowercase();
        let caller_query = query
            .caller
            .as_deref()
            .map(|caller| caller.trim().to_ascii_lowercase());

        for unit in &self.snapshot.project.units {
            for call in &unit.call_sites {
                if !call_target_matches(&call.target, &target_query) {
                    continue;
                }
                let caller = self
                    .snapshot
                    .routine_analysis()
                    .routine_for_scope(unit.unit_id, call.scope);
                if let Some(caller_query) = caller_query.as_deref()
                    && !caller_matches(caller, caller_query)
                {
                    continue;
                }
                let caller_node_id = caller
                    .and_then(|routine| routine.descriptor.owner)
                    .and_then(|owner| self.snapshot.call_graph().symbol_node(owner))
                    .map(|node| Arc::clone(&node.id));
                let target_edge = caller_node_id.as_ref().and_then(|caller_node_id| {
                    self.snapshot
                        .call_graph()
                        .outbound_calls(caller_node_id.as_ref())
                        .into_iter()
                        .find(|edge| {
                            edge.source_range.start == call.range.start
                                && edge.source_range.end == call.range.end
                        })
                });
                matched.push(MatchedCall {
                    occurrence: 0,
                    unit,
                    call,
                    caller,
                    caller_node_id,
                    target_node_id: target_edge.and_then(|edge| edge.target.as_ref().cloned()),
                    target_kind: call_target_kind_name(&call.target).to_string(),
                    target_name: call_target_name(&call.target),
                });
            }
        }

        matched.sort_by(|left, right| {
            left.unit
                .uri
                .cmp(&right.unit.uri)
                .then(left.call.range.start.cmp(&right.call.range.start))
                .then(left.call.range.end.cmp(&right.call.range.end))
                .then(left.target_name.cmp(&right.target_name))
        });
        for (idx, matched_call) in matched.iter_mut().enumerate() {
            matched_call.occurrence = idx + 1;
        }
        matched
    }

    fn build_lifecycle(&self, selected: &MatchedCall<'a>) -> CallDataflowLifecycle {
        let mut nodes = HashMap::<String, CallDataflowLifecycleNode>::new();
        let mut edges = Vec::<CallDataflowLifecycleEdge>::new();
        let mut visited = BTreeSet::<String>::new();
        let mut queue = VecDeque::<String>::new();

        if let Some(caller_node_id) = selected.caller_node_id.as_ref() {
            queue.push_back(caller_node_id.to_string());
        }
        while let Some(node_id) = queue.pop_front() {
            if !visited.insert(node_id.clone()) {
                continue;
            }
            if let Some(node) = self.snapshot.call_graph().node(&node_id) {
                nodes
                    .entry(node_id.clone())
                    .or_insert_with(|| lifecycle_node_from_call_graph(node, false));
                for edge in self.snapshot.call_graph().inbound_callers(node_id.as_str()) {
                    nodes.entry(edge.source.to_string()).or_insert_with(|| {
                        self.snapshot
                            .call_graph()
                            .node(edge.source.as_ref())
                            .map(|source| lifecycle_node_from_call_graph(source, false))
                            .unwrap_or(CallDataflowLifecycleNode {
                                id: edge.source.to_string(),
                                kind: "callable".to_string(),
                                name: edge.source.to_string(),
                                unit_uri: String::new(),
                                decl_range: CallDataflowByteRange { start: 0, end: 0 },
                                synthetic: false,
                            })
                    });
                    if let Some(target) = edge.target.as_ref() {
                        nodes.entry(target.to_string()).or_insert_with(|| {
                            self.snapshot
                                .call_graph()
                                .node(target.as_ref())
                                .map(|target| lifecycle_node_from_call_graph(target, false))
                                .unwrap_or(CallDataflowLifecycleNode {
                                    id: target.to_string(),
                                    kind: "callable".to_string(),
                                    name: target.to_string(),
                                    unit_uri: String::new(),
                                    decl_range: CallDataflowByteRange { start: 0, end: 0 },
                                    synthetic: false,
                                })
                        });
                    }
                    edges.push(CallDataflowLifecycleEdge {
                        source: edge.source.to_string(),
                        target: edge
                            .target
                            .as_ref()
                            .map(|target| target.to_string())
                            .unwrap_or_default(),
                        kind: call_graph_edge_kind_name(edge.edge_kind).to_string(),
                        label: None,
                        source_range: Some(byte_range(&edge.source_range)),
                        synthetic: false,
                    });
                    queue.push_back(edge.source.to_string());
                }
            }
        }

        if let (Some(caller_node_id), Some(target_node_id)) = (
            selected.caller_node_id.as_ref(),
            selected.target_node_id.as_ref(),
        ) {
            if let Some(target_node) = self.snapshot.call_graph().node(target_node_id.as_ref()) {
                nodes
                    .entry(target_node_id.to_string())
                    .or_insert_with(|| lifecycle_node_from_call_graph(target_node, false));
            } else {
                nodes
                    .entry(target_node_id.to_string())
                    .or_insert(CallDataflowLifecycleNode {
                        id: target_node_id.to_string(),
                        kind: selected.target_kind.clone(),
                        name: selected.target_name.clone(),
                        unit_uri: selected.unit.uri.to_string(),
                        decl_range: byte_range(&selected.call.range),
                        synthetic: false,
                    });
            }
            edges.push(CallDataflowLifecycleEdge {
                source: caller_node_id.to_string(),
                target: target_node_id.to_string(),
                kind: "selected_call".to_string(),
                label: Some(selected.target_name.clone()),
                source_range: Some(byte_range(&selected.call.range)),
                synthetic: false,
            });
        }

        self.add_context_outbound_edges(selected, &mut nodes, &mut edges);
        self.add_synthetic_screen_edges(&mut nodes, &mut edges);

        let mut out_nodes: Vec<_> = nodes.into_values().collect();
        out_nodes.sort_by(|left, right| {
            left.kind
                .cmp(&right.kind)
                .then(left.name.cmp(&right.name))
                .then(left.unit_uri.cmp(&right.unit_uri))
                .then(left.decl_range.start.cmp(&right.decl_range.start))
        });
        edges.sort_by(|left, right| {
            left.source
                .cmp(&right.source)
                .then(left.target.cmp(&right.target))
                .then(left.kind.cmp(&right.kind))
                .then(left.synthetic.cmp(&right.synthetic))
        });
        edges.dedup_by(|left, right| {
            left.source == right.source
                && left.target == right.target
                && left.kind == right.kind
                && left.label == right.label
                && left.synthetic == right.synthetic
        });

        CallDataflowLifecycle {
            nodes: out_nodes,
            edges,
        }
    }

    fn add_synthetic_screen_edges(
        &self,
        nodes: &mut HashMap<String, CallDataflowLifecycleNode>,
        edges: &mut Vec<CallDataflowLifecycleEdge>,
    ) {
        let target_nodes: Vec<_> = nodes
            .values()
            .filter_map(|node| {
                screen_module_name(&node.name)
                    .map(|(screen, phase)| (node.id.clone(), screen, phase))
            })
            .collect();
        if target_nodes.is_empty() {
            return;
        }

        for unit in &self.snapshot.project.units {
            let Some(source) = self.snapshot.project_text(unit.uri.as_ref()) else {
                continue;
            };
            for site in call_screen_sites(unit, source) {
                let caller = self
                    .snapshot
                    .routine_analysis()
                    .routine_for_scope(unit.unit_id, site.scope);
                let Some(caller_owner) = caller.and_then(|routine| routine.descriptor.owner) else {
                    continue;
                };
                let Some(caller_node) = self.snapshot.call_graph().symbol_node(caller_owner) else {
                    continue;
                };
                nodes
                    .entry(caller_node.id.to_string())
                    .or_insert_with(|| lifecycle_node_from_call_graph(caller_node, false));
                for (target_id, screen, phase) in &target_nodes {
                    if screen != &site.screen_number || phase != &site.phase {
                        continue;
                    }
                    edges.push(CallDataflowLifecycleEdge {
                        source: caller_node.id.to_string(),
                        target: target_id.clone(),
                        kind: "screen_dispatch".to_string(),
                        label: Some(format!("CALL SCREEN {} ({})", screen, phase)),
                        source_range: Some(byte_range(&site.range)),
                        synthetic: true,
                    });
                }
            }
        }
    }

    fn add_context_outbound_edges(
        &self,
        selected: &MatchedCall<'a>,
        nodes: &mut HashMap<String, CallDataflowLifecycleNode>,
        edges: &mut Vec<CallDataflowLifecycleEdge>,
    ) {
        let anchor_node_ids: Vec<_> = nodes.keys().cloned().collect();
        for node_id in anchor_node_ids {
            for edge in self.snapshot.call_graph().outbound_calls(node_id.as_str()) {
                let Some(target_id) = edge.target.as_ref() else {
                    continue;
                };
                if selected.caller_node_id.as_deref() == Some(edge.source.as_ref())
                    && selected.target_node_id.as_deref() == Some(target_id.as_ref())
                    && edge.source_range.start == selected.call.range.start
                    && edge.source_range.end == selected.call.range.end
                {
                    continue;
                }
                if let Some(target_node) = self.snapshot.call_graph().node(target_id.as_ref()) {
                    nodes
                        .entry(target_id.to_string())
                        .or_insert_with(|| lifecycle_node_from_call_graph(target_node, false));
                }
                edges.push(CallDataflowLifecycleEdge {
                    source: edge.source.to_string(),
                    target: target_id.to_string(),
                    kind: call_graph_edge_kind_name(edge.edge_kind).to_string(),
                    label: None,
                    source_range: Some(byte_range(&edge.source_range)),
                    synthetic: false,
                });
            }
        }
    }

    fn build_parameter_traces(
        &mut self,
        selected: &MatchedCall<'a>,
    ) -> Vec<CallDataflowParameterTrace> {
        let mut traces = Vec::new();
        let unit = selected.unit;
        let context = TraceContext {
            unit,
            routine: selected.caller,
            sink_offset: selected.call.range.start,
        };

        for argument in &selected.call.arguments {
            let parameter_name = argument
                .name
                .as_ref()
                .map(|name| name.to_string())
                .or_else(|| Some(format!("arg{}", argument.ordinal + 1)));
            let direction = self.argument_direction(unit, selected.call, argument);
            let mut field_mappings = Vec::new();
            let mut notes = Vec::new();
            let target_path = parameter_name
                .clone()
                .unwrap_or_else(|| format!("arg{}", argument.ordinal + 1));
            let section = argument
                .section
                .map(named_argument_section_name)
                .map(str::to_string);
            let argument_type = argument
                .type_fact
                .type_clause_display
                .as_ref()
                .map(|text| text.to_string());
            let mut provenance = ParameterProvenanceBuilder::new(
                &target_path,
                direction.as_str(),
                section.as_deref(),
                argument_type.as_deref(),
            );
            let sink_node_id = provenance.root_id();
            let argument_text = snippet(
                self.snapshot.project_text(unit.uri.as_ref()),
                &argument.range,
            );

            if direction.is_input_like() {
                self.trace_argument_range(
                    context,
                    &argument.range,
                    &target_path,
                    &mut field_mappings,
                    &mut notes,
                    0,
                    &mut provenance,
                    sink_node_id.as_str(),
                );
            } else {
                let output_node_id = provenance.add_flow_node(
                    "call_output",
                    &format!(
                        "{}.{}",
                        selected.target_name,
                        parameter_name
                            .clone()
                            .unwrap_or_else(|| format!("arg{}", argument.ordinal + 1))
                    ),
                    Some(unit.uri.as_ref()),
                    Some(byte_range(&selected.call.range)),
                    Some(snippet(
                        self.snapshot.project_text(unit.uri.as_ref()),
                        &selected.call.range,
                    )),
                );
                provenance.add_edge(&output_node_id, sink_node_id.as_str(), "produces", None);
                field_mappings.push(CallDataflowFieldMapping {
                    target_path,
                    source_kind: "call_output".to_string(),
                    source_display: format!(
                        "{}.{}",
                        selected.target_name,
                        parameter_name
                            .clone()
                            .unwrap_or_else(|| format!("arg{}", argument.ordinal + 1))
                    ),
                    source_unit_uri: Some(unit.uri.to_string()),
                    source_range: Some(byte_range(&selected.call.range)),
                    statement_text: Some(snippet(
                        self.snapshot.project_text(unit.uri.as_ref()),
                        &selected.call.range,
                    )),
                });
            }

            sort_and_dedup_mappings(&mut field_mappings);
            traces.push(CallDataflowParameterTrace {
                parameter_name: parameter_name.clone(),
                section,
                direction: direction.as_str().to_string(),
                argument_text,
                argument_range: byte_range(&argument.range),
                argument_type,
                field_mappings,
                provenance: provenance.finish(),
                notes,
            });
        }

        traces
    }

    fn argument_direction(
        &self,
        unit: &UnitAnalysis,
        call: &CallSiteData,
        argument: &abap_symbols::CallArgumentData,
    ) -> TraceDirection {
        for edge in &unit.value_flow_edges {
            let abap_symbols::ValueFlowTargetData::CallParameter {
                call_range,
                parameter_decl_unit,
                parameter_decl_range,
                ..
            } = &edge.target
            else {
                continue;
            };
            if edge.kind != abap_symbols::ValueFlowKind::CallArgument
                || call_range.start != call.range.start
                || call_range.end != call.range.end
                || edge.source_range.start != argument.range.start
                || edge.source_range.end != argument.range.end
            {
                continue;
            }
            if let (Some(parameter_decl_unit), Some(parameter_decl_range)) =
                (parameter_decl_unit, parameter_decl_range.as_ref())
                && let Some((_, parameter)) = self
                    .snapshot
                    .callable_summaries()
                    .summary_for_parameter_decl(*parameter_decl_unit, parameter_decl_range)
            {
                return match parameter.direction {
                    CallableParameterDirection::Input => TraceDirection::Input,
                    CallableParameterDirection::Output => TraceDirection::Output,
                    CallableParameterDirection::InOut => TraceDirection::InOut,
                };
            }
        }

        match argument.section {
            Some(NamedArgumentSection::Exporting) | None => TraceDirection::Input,
            Some(NamedArgumentSection::Importing) | Some(NamedArgumentSection::Receiving) => {
                TraceDirection::Output
            }
            Some(NamedArgumentSection::Changing) | Some(NamedArgumentSection::Tables) => {
                TraceDirection::InOut
            }
            Some(NamedArgumentSection::Exceptions) => TraceDirection::Output,
        }
    }

    fn trace_argument_range(
        &mut self,
        context: TraceContext<'a>,
        range: &TextRange,
        target_path: &str,
        field_mappings: &mut Vec<CallDataflowFieldMapping>,
        notes: &mut Vec<String>,
        depth: usize,
        provenance: &mut ParameterProvenanceBuilder,
        sink_node_id: &str,
    ) {
        if depth > 12 {
            notes.push(format!("trace depth limit reached for {target_path}"));
            return;
        }

        let accesses = value_accesses_in_range(context.unit, range);
        if accesses.len() != 1 {
            field_mappings.push(CallDataflowFieldMapping {
                target_path: target_path.to_string(),
                source_kind: if accesses.is_empty() {
                    "literal_or_expression".to_string()
                } else {
                    "composite_expression".to_string()
                },
                source_display: snippet(
                    self.snapshot.project_text(context.unit.uri.as_ref()),
                    range,
                ),
                source_unit_uri: Some(context.unit.uri.to_string()),
                source_range: Some(byte_range(range)),
                statement_text: None,
            });
            let expression_kind = if accesses.is_empty() {
                "literal_or_expression"
            } else {
                "composite_expression"
            };
            let expression_node_id = provenance.add_flow_node(
                expression_kind,
                &snippet(self.snapshot.project_text(context.unit.uri.as_ref()), range),
                Some(context.unit.uri.as_ref()),
                Some(byte_range(range)),
                None,
            );
            provenance.add_edge(&expression_node_id, sink_node_id, "flows_to", None);
            return;
        }

        self.trace_symbol_access(
            context,
            &accesses[0],
            target_path,
            field_mappings,
            notes,
            depth,
            provenance,
            sink_node_id,
        );
    }

    fn trace_bound_range(
        &mut self,
        context: TraceContext<'a>,
        range: &TextRange,
        preserved_field_path: &[String],
        target_path: &str,
        field_mappings: &mut Vec<CallDataflowFieldMapping>,
        notes: &mut Vec<String>,
        depth: usize,
        provenance: &mut ParameterProvenanceBuilder,
        sink_node_id: &str,
    ) {
        if preserved_field_path.is_empty() {
            self.trace_argument_range(
                context,
                range,
                target_path,
                field_mappings,
                notes,
                depth,
                provenance,
                sink_node_id,
            );
            return;
        }

        let accesses = value_accesses_in_range(context.unit, range);
        if accesses.len() != 1 {
            self.trace_argument_range(
                context,
                range,
                target_path,
                field_mappings,
                notes,
                depth,
                provenance,
                sink_node_id,
            );
            return;
        }

        let mut access = accesses[0].clone();
        if access.field_path.is_empty() {
            let bound_unit = &self.snapshot.project.units[access.handle.unit.as_usize()];
            access.field_path = preserved_field_path.to_vec();
            access.display = format_access_display_from_path(
                bound_unit.symbol(access.handle.symbol).name.as_ref(),
                &access.field_path,
            );
        }

        self.trace_symbol_access(
            context,
            &access,
            target_path,
            field_mappings,
            notes,
            depth,
            provenance,
            sink_node_id,
        );
    }

    fn trace_symbol_access(
        &mut self,
        context: TraceContext<'a>,
        access: &ValueAccess,
        target_path: &str,
        field_mappings: &mut Vec<CallDataflowFieldMapping>,
        notes: &mut Vec<String>,
        depth: usize,
        provenance: &mut ParameterProvenanceBuilder,
        sink_node_id: &str,
    ) {
        let context_owner = context
            .routine
            .and_then(|routine| routine.descriptor.owner)
            .map(|owner| (owner.unit.0, owner.symbol.0));
        let visit_key = TraceVisitKey {
            context_owner,
            symbol: (access.handle.unit.0, access.handle.symbol.0),
            field_path: access.field_path.clone(),
            target_path: target_path.to_string(),
        };
        if !self.visited.insert(visit_key) {
            return;
        }

        let unit = &self.snapshot.project.units[access.handle.unit.as_usize()];
        let symbol = unit.symbol(access.handle.symbol);
        if symbol.kind == SymbolKind::Constant || symbol.kind == SymbolKind::BuiltinConstant {
            field_mappings.push(CallDataflowFieldMapping {
                target_path: target_path.to_string(),
                source_kind: "constant".to_string(),
                source_display: access.display.clone(),
                source_unit_uri: Some(unit.uri.to_string()),
                source_range: Some(byte_range(&access.range)),
                statement_text: symbol
                    .value_clause_display
                    .as_ref()
                    .map(|text| text.to_string()),
            });
            let source_node_id = provenance.add_flow_node(
                "constant",
                &access.display,
                Some(unit.uri.as_ref()),
                Some(byte_range(&access.range)),
                symbol
                    .value_clause_display
                    .as_ref()
                    .map(|text| text.to_string()),
            );
            provenance.add_edge(&source_node_id, sink_node_id, "flows_to", None);
            return;
        }

        let mut found = false;
        if unit.form_parameter(access.handle.symbol).is_some() {
            found |= self.trace_form_parameter_bindings(
                context,
                access,
                target_path,
                field_mappings,
                notes,
                depth + 1,
                provenance,
                sink_node_id,
            );
        }
        found |= self.trace_field_symbol_bindings(
            context,
            access,
            target_path,
            field_mappings,
            notes,
            depth + 1,
            provenance,
            sink_node_id,
        );
        found |= self.trace_loop_target_bindings(
            context,
            access,
            target_path,
            field_mappings,
            notes,
            depth + 1,
            provenance,
            sink_node_id,
        );
        found |= self.trace_read_table_bindings(
            context,
            access,
            target_path,
            field_mappings,
            notes,
            depth + 1,
            provenance,
            sink_node_id,
        );

        found |= self.trace_assignment_writers(
            context,
            access,
            target_path,
            field_mappings,
            notes,
            depth + 1,
            provenance,
            sink_node_id,
        );
        found |= self.trace_producer_perform_calls(
            context,
            access,
            target_path,
            field_mappings,
            notes,
            depth + 1,
            provenance,
            sink_node_id,
        );
        found |= self.trace_producer_call_outputs(
            context,
            access,
            target_path,
            field_mappings,
            provenance,
            sink_node_id,
        );
        found |= self.trace_sql_writers(
            context,
            access,
            target_path,
            field_mappings,
            provenance,
            sink_node_id,
        );

        if !found && symbol.scope == unit.root_scope {
            found |= self.trace_global_from_callers(
                context,
                access,
                target_path,
                field_mappings,
                notes,
                depth + 1,
                provenance,
                sink_node_id,
            );
            if !found {
                found |= self.trace_project_global_writers(
                    context,
                    access,
                    target_path,
                    field_mappings,
                    notes,
                    depth + 1,
                    provenance,
                    sink_node_id,
                );
            }
        }

        if !found {
            let source_kind = classify_terminal_symbol(unit, symbol).to_string();
            field_mappings.push(CallDataflowFieldMapping {
                target_path: target_path.to_string(),
                source_kind: source_kind.clone(),
                source_display: access.display.clone(),
                source_unit_uri: Some(unit.uri.to_string()),
                source_range: Some(byte_range(&access.range)),
                statement_text: None,
            });
            let source_node_id = provenance.add_flow_node(
                &source_kind,
                &access.display,
                Some(unit.uri.as_ref()),
                Some(byte_range(&access.range)),
                None,
            );
            provenance.add_edge(&source_node_id, sink_node_id, "flows_to", None);
        }
    }

    fn trace_form_parameter_bindings(
        &mut self,
        context: TraceContext<'a>,
        access: &ValueAccess,
        target_path: &str,
        field_mappings: &mut Vec<CallDataflowFieldMapping>,
        notes: &mut Vec<String>,
        depth: usize,
        provenance: &mut ParameterProvenanceBuilder,
        sink_node_id: &str,
    ) -> bool {
        let unit = &self.snapshot.project.units[access.handle.unit.as_usize()];
        let Some(symbol) = unit.routine_parameters_owner(access.handle.symbol) else {
            return false;
        };
        let Some(form_parameter) = unit.form_parameter(access.handle.symbol) else {
            return false;
        };
        let Some(inbound) = self.inbound_perform_calls(symbol) else {
            return false;
        };
        let formal_name = unit.symbol(access.handle.symbol).name.as_ref();
        let callee_name = context
            .routine
            .map(|routine| routine.descriptor.name.as_ref())
            .unwrap_or("<unknown>");

        let mut found = false;
        for (caller_unit, caller_routine, perform_call) in inbound {
            let Some(argument) = perform_call.arguments.iter().find(|argument| {
                argument.section == perform_section_for_form_parameter(form_parameter.section)
                    && perform_argument_ordinal(perform_call, argument)
                        == form_parameter_ordinal(unit, symbol, form_parameter)
            }) else {
                continue;
            };

            field_mappings.push(CallDataflowFieldMapping {
                target_path: target_path.to_string(),
                source_kind: "perform_binding".to_string(),
                source_display: snippet(
                    self.snapshot.project_text(caller_unit.uri.as_ref()),
                    &argument.range,
                ),
                source_unit_uri: Some(caller_unit.uri.to_string()),
                source_range: Some(byte_range(&argument.range)),
                statement_text: Some(snippet(
                    self.snapshot.project_text(caller_unit.uri.as_ref()),
                    &perform_call.range,
                )),
            });
            let binding_node_id = provenance.add_flow_node(
                "perform_binding",
                &format!(
                    "{} -> {}.{}",
                    snippet(
                        self.snapshot.project_text(caller_unit.uri.as_ref()),
                        &argument.range
                    ),
                    callee_name,
                    formal_name,
                ),
                Some(caller_unit.uri.as_ref()),
                Some(byte_range(&perform_call.range)),
                Some(snippet(
                    self.snapshot.project_text(caller_unit.uri.as_ref()),
                    &perform_call.range,
                )),
            );
            provenance.add_edge(&binding_node_id, sink_node_id, "binds_to", None);
            let caller_context = TraceContext {
                unit: caller_unit,
                routine: caller_routine,
                sink_offset: perform_call.range.start,
            };
            self.trace_bound_range(
                caller_context,
                &argument.range,
                &access.field_path,
                target_path,
                field_mappings,
                notes,
                depth,
                provenance,
                &binding_node_id,
            );
            found = true;
        }
        found
    }

    fn trace_field_symbol_bindings(
        &mut self,
        context: TraceContext<'a>,
        access: &ValueAccess,
        target_path: &str,
        field_mappings: &mut Vec<CallDataflowFieldMapping>,
        notes: &mut Vec<String>,
        depth: usize,
        provenance: &mut ParameterProvenanceBuilder,
        sink_node_id: &str,
    ) -> bool {
        let source_unit = &self.snapshot.project.units[access.handle.unit.as_usize()];
        let symbol = source_unit.symbol(access.handle.symbol);
        if symbol.kind != SymbolKind::FieldSymbol {
            return false;
        }
        let Some(current_routine) = context.routine else {
            return false;
        };

        let mut found = false;
        for edge in &context.unit.value_flow_edges {
            if edge.source_range.end > context.sink_offset
                || !same_routine_scope(self.snapshot, context.unit, edge.scope, current_routine)
            {
                continue;
            }
            if !matches!(
                edge.kind,
                abap_symbols::ValueFlowKind::FieldSymbolAssignment
                    | abap_symbols::ValueFlowKind::ConditionalFieldSymbolAssignment
            ) {
                continue;
            }
            let abap_symbols::ValueFlowTargetData::FieldSymbol { name, range } = &edge.target
            else {
                continue;
            };
            if name.as_deref() != Some(symbol.name.as_ref()) {
                continue;
            }

            field_mappings.push(CallDataflowFieldMapping {
                target_path: target_path.to_string(),
                source_kind: "field_symbol_binding".to_string(),
                source_display: snippet(
                    self.snapshot.project_text(context.unit.uri.as_ref()),
                    &edge.source_range,
                ),
                source_unit_uri: Some(context.unit.uri.to_string()),
                source_range: Some(byte_range(&edge.source_range)),
                statement_text: Some(snippet(
                    self.snapshot.project_text(context.unit.uri.as_ref()),
                    range,
                )),
            });
            let binding_node_id = provenance.add_flow_node(
                "field_symbol_binding",
                &format!(
                    "{} <- {}",
                    symbol.name,
                    snippet(
                        self.snapshot.project_text(context.unit.uri.as_ref()),
                        &edge.source_range,
                    )
                ),
                Some(context.unit.uri.as_ref()),
                Some(byte_range(range)),
                Some(snippet(
                    self.snapshot.project_text(context.unit.uri.as_ref()),
                    range,
                )),
            );
            provenance.add_edge(&binding_node_id, sink_node_id, "binds_to", None);
            self.trace_bound_range(
                TraceContext {
                    sink_offset: edge.source_range.start,
                    ..context
                },
                &edge.source_range,
                &access.field_path,
                target_path,
                field_mappings,
                notes,
                depth,
                provenance,
                &binding_node_id,
            );
            found = true;
        }
        found
    }

    fn trace_read_table_bindings(
        &mut self,
        context: TraceContext<'a>,
        access: &ValueAccess,
        target_path: &str,
        field_mappings: &mut Vec<CallDataflowFieldMapping>,
        notes: &mut Vec<String>,
        depth: usize,
        provenance: &mut ParameterProvenanceBuilder,
        sink_node_id: &str,
    ) -> bool {
        let Some(current_routine) = context.routine else {
            return false;
        };

        let mut found = false;
        for site in &context.unit.routine_sites {
            if site.kind != abap_symbols::RoutineSiteKind::ReadTable
                || site.range.end > context.sink_offset
                || !same_routine_scope(self.snapshot, context.unit, site.scope, current_routine)
            {
                continue;
            }
            let Some(target_range) = site.target_range.as_ref() else {
                continue;
            };
            let target_accesses = value_accesses_in_range(context.unit, target_range);
            if !target_accesses
                .iter()
                .any(|target_access| target_access.handle == access.handle)
            {
                continue;
            }

            field_mappings.push(CallDataflowFieldMapping {
                target_path: target_path.to_string(),
                source_kind: "read_table_binding".to_string(),
                source_display: snippet(
                    self.snapshot.project_text(context.unit.uri.as_ref()),
                    &site.range,
                ),
                source_unit_uri: Some(context.unit.uri.to_string()),
                source_range: Some(byte_range(&site.range)),
                statement_text: Some(snippet(
                    self.snapshot.project_text(context.unit.uri.as_ref()),
                    target_range,
                )),
            });
            let binding_node_id = provenance.add_flow_node(
                "read_table_binding",
                &format!(
                    "{} <- READ TABLE {}",
                    context.unit.symbol(access.handle.symbol).name,
                    snippet(
                        self.snapshot.project_text(context.unit.uri.as_ref()),
                        &site.range,
                    )
                ),
                Some(context.unit.uri.as_ref()),
                Some(byte_range(target_range)),
                Some(snippet(
                    self.snapshot.project_text(context.unit.uri.as_ref()),
                    target_range,
                )),
            );
            provenance.add_edge(&binding_node_id, sink_node_id, "binds_to", None);
            self.trace_bound_range(
                TraceContext {
                    sink_offset: site.range.start,
                    ..context
                },
                &site.range,
                &access.field_path,
                target_path,
                field_mappings,
                notes,
                depth,
                provenance,
                &binding_node_id,
            );
            found = true;
        }
        found
    }

    fn trace_loop_target_bindings(
        &mut self,
        context: TraceContext<'a>,
        access: &ValueAccess,
        target_path: &str,
        field_mappings: &mut Vec<CallDataflowFieldMapping>,
        notes: &mut Vec<String>,
        depth: usize,
        provenance: &mut ParameterProvenanceBuilder,
        sink_node_id: &str,
    ) -> bool {
        let Some(current_routine) = context.routine else {
            return false;
        };
        if access.handle.unit != context.unit.unit_id {
            return false;
        }
        if context.unit.symbol(access.handle.symbol).kind == SymbolKind::FieldSymbol {
            return false;
        }

        let mut best_region = None;
        let mut best_start = 0usize;
        let mut best_end = usize::MAX;
        for region in &context.unit.routine_control_regions {
            let abap_symbols::RoutineControlRegionData::Loop(region) = region else {
                continue;
            };
            if region.range.start >= context.sink_offset
                || access.range.start < region.range.start
                || access.range.end > region.range.end
                || !same_routine_scope(self.snapshot, context.unit, region.scope, current_routine)
            {
                continue;
            }
            if region.source_access.is_none() {
                continue;
            }
            let Some(target_access) = region.target_access.as_ref() else {
                continue;
            };
            let Some(target_handle) = resolve_access_handle(context.unit, target_access) else {
                continue;
            };
            if target_handle != access.handle {
                continue;
            }
            let target_field_path = field_access_path(target_access);
            if relevant_target_suffix(&target_field_path, &access.field_path).is_none() {
                continue;
            }
            if best_region.is_none()
                || region.range.start > best_start
                || (region.range.start == best_start && region.range.end < best_end)
            {
                best_start = region.range.start;
                best_end = region.range.end;
                best_region = Some(region);
            }
        }

        let Some(region) = best_region else {
            return false;
        };
        let Some(source_access) = region.source_access.as_ref() else {
            return false;
        };
        let Some(target_access) = region.target_access.as_ref() else {
            return false;
        };
        let target_field_path = field_access_path(target_access);
        let Some(target_suffix) = relevant_target_suffix(&target_field_path, &access.field_path)
        else {
            return false;
        };
        let Some(source_value_access) =
            field_access_to_value_access(context.unit, source_access, &target_suffix)
        else {
            return false;
        };

        let header_text = loop_header_statement_text(
            self.snapshot.project_text(context.unit.uri.as_ref()),
            region,
        );
        field_mappings.push(CallDataflowFieldMapping {
            target_path: target_path.to_string(),
            source_kind: "loop_binding".to_string(),
            source_display: source_value_access.display.clone(),
            source_unit_uri: Some(context.unit.uri.to_string()),
            source_range: Some(byte_range(&field_access_range(source_access))),
            statement_text: Some(header_text.clone()),
        });
        let binding_node_id = provenance.add_flow_node(
            "loop_binding",
            &format!("{} <- {}", access.display, header_text),
            Some(context.unit.uri.as_ref()),
            Some(byte_range(&region.range)),
            Some(header_text),
        );
        provenance.add_edge(&binding_node_id, sink_node_id, "binds_to", None);
        self.trace_symbol_access(
            TraceContext {
                sink_offset: region.range.start,
                ..context
            },
            &source_value_access,
            target_path,
            field_mappings,
            notes,
            depth,
            provenance,
            &binding_node_id,
        );
        true
    }

    fn trace_assignment_writers(
        &mut self,
        context: TraceContext<'a>,
        access: &ValueAccess,
        target_path: &str,
        field_mappings: &mut Vec<CallDataflowFieldMapping>,
        notes: &mut Vec<String>,
        depth: usize,
        provenance: &mut ParameterProvenanceBuilder,
        sink_node_id: &str,
    ) -> bool {
        let Some(current_routine) = context.routine else {
            return false;
        };
        let mut found = false;
        for assignment in &context.unit.assignment_sites {
            if assignment.range.end > context.sink_offset {
                continue;
            }
            if !same_routine_scope(
                self.snapshot,
                context.unit,
                assignment.scope,
                current_routine,
            ) {
                continue;
            }
            let Some(lhs_access) = assignment_access(context.unit, assignment) else {
                continue;
            };
            if lhs_access.handle != access.handle {
                continue;
            }
            let Some(target_suffix) =
                relevant_target_suffix(&access.field_path, &lhs_access.field_path)
            else {
                continue;
            };
            let statement_text = snippet(
                self.snapshot.project_text(context.unit.uri.as_ref()),
                &assignment.range,
            );
            if statement_text.to_ascii_lowercase().starts_with("append ")
                && target_suffix.is_empty()
            {
                let append_target = if access.field_path.is_empty() {
                    append_target_path(target_path)
                } else {
                    target_path.to_string()
                };
                let append_target_node_id = provenance.ensure_target_path_node(&append_target);
                field_mappings.push(CallDataflowFieldMapping {
                    target_path: append_target.clone(),
                    source_kind: "append_row".to_string(),
                    source_display: snippet(
                        self.snapshot.project_text(context.unit.uri.as_ref()),
                        &assignment.rhs_range,
                    ),
                    source_unit_uri: Some(context.unit.uri.to_string()),
                    source_range: Some(byte_range(&assignment.rhs_range)),
                    statement_text: Some(statement_text),
                });
                let append_node_id = provenance.add_flow_node(
                    "append_row",
                    &snippet(
                        self.snapshot.project_text(context.unit.uri.as_ref()),
                        &assignment.range,
                    ),
                    Some(context.unit.uri.as_ref()),
                    Some(byte_range(&assignment.range)),
                    Some(snippet(
                        self.snapshot.project_text(context.unit.uri.as_ref()),
                        &assignment.range,
                    )),
                );
                provenance.add_edge(&append_node_id, &append_target_node_id, "appends", None);
                if provenance.node_kind(sink_node_id) == Some("perform_write")
                    && append_target_node_id != sink_node_id
                {
                    provenance.add_edge(&append_target_node_id, sink_node_id, "flows_to", None);
                }
                self.trace_bound_range(
                    TraceContext {
                        sink_offset: assignment.range.start,
                        ..context
                    },
                    &assignment.rhs_range,
                    &access.field_path,
                    &append_target,
                    field_mappings,
                    notes,
                    depth,
                    provenance,
                    &append_node_id,
                );
                found = true;
                continue;
            }

            let assignment_target = join_target_path(target_path, &target_suffix);
            let assignment_target_node_id = provenance.ensure_target_path_node(&assignment_target);
            field_mappings.push(CallDataflowFieldMapping {
                target_path: assignment_target.clone(),
                source_kind: "assignment".to_string(),
                source_display: snippet(
                    self.snapshot.project_text(context.unit.uri.as_ref()),
                    &assignment.rhs_range,
                ),
                source_unit_uri: Some(context.unit.uri.to_string()),
                source_range: Some(byte_range(&assignment.rhs_range)),
                statement_text: Some(statement_text),
            });
            let assignment_node_id = provenance.add_flow_node(
                "assignment",
                &snippet(
                    self.snapshot.project_text(context.unit.uri.as_ref()),
                    &assignment.range,
                ),
                Some(context.unit.uri.as_ref()),
                Some(byte_range(&assignment.range)),
                Some(snippet(
                    self.snapshot.project_text(context.unit.uri.as_ref()),
                    &assignment.range,
                )),
            );
            provenance.add_edge(
                &assignment_node_id,
                &assignment_target_node_id,
                "writes",
                None,
            );
            if provenance.node_kind(sink_node_id) == Some("perform_write")
                && assignment_target_node_id != sink_node_id
            {
                provenance.add_edge(&assignment_target_node_id, sink_node_id, "flows_to", None);
            }
            self.trace_bound_range(
                TraceContext {
                    sink_offset: assignment.range.start,
                    ..context
                },
                &assignment.rhs_range,
                &access.field_path,
                &assignment_target,
                field_mappings,
                notes,
                depth,
                provenance,
                &assignment_node_id,
            );
            found = true;
        }
        found
    }

    fn trace_producer_perform_calls(
        &mut self,
        context: TraceContext<'a>,
        access: &ValueAccess,
        target_path: &str,
        field_mappings: &mut Vec<CallDataflowFieldMapping>,
        notes: &mut Vec<String>,
        depth: usize,
        provenance: &mut ParameterProvenanceBuilder,
        sink_node_id: &str,
    ) -> bool {
        let Some(current_routine) = context.routine else {
            return false;
        };
        let mut found = false;
        for perform_call in &context.unit.perform_calls {
            if perform_call.range.end > context.sink_offset
                || !same_routine_scope(
                    self.snapshot,
                    context.unit,
                    perform_call.scope,
                    current_routine,
                )
            {
                continue;
            }
            let Some(callee_owner) =
                perform_call_target(&self.snapshot.project, context.unit, perform_call)
            else {
                continue;
            };
            let Some(callee_summary) = self
                .snapshot
                .callable_summaries()
                .summary_for_owner(callee_owner)
            else {
                continue;
            };

            for argument in &perform_call.arguments {
                let actual_accesses = value_accesses_in_range(context.unit, &argument.range);
                if actual_accesses.len() != 1 {
                    continue;
                }
                if actual_accesses[0].handle != access.handle {
                    continue;
                }
                if actual_accesses[0].field_path != access.field_path
                    && !actual_accesses[0].field_path.is_empty()
                {
                    continue;
                }
                let Some(parameter_index) = perform_parameter_index(perform_call, argument) else {
                    continue;
                };
                let Some(parameter) = callee_summary.parameters.get(parameter_index) else {
                    continue;
                };
                if !parameter.may_write {
                    continue;
                }
                field_mappings.push(CallDataflowFieldMapping {
                    target_path: target_path.to_string(),
                    source_kind: "perform_write".to_string(),
                    source_display: format!("{}:{}", callee_summary.name, parameter.name),
                    source_unit_uri: Some(context.unit.uri.to_string()),
                    source_range: Some(byte_range(&perform_call.range)),
                    statement_text: Some(snippet(
                        self.snapshot.project_text(context.unit.uri.as_ref()),
                        &perform_call.range,
                    )),
                });
                let write_summary = parameter
                    .symbol
                    .and_then(|parameter_symbol| {
                        let callee_unit =
                            &self.snapshot.project.units[parameter_symbol.unit.as_usize()];
                        let callee_routine = self
                            .snapshot
                            .routine_analysis()
                            .routine_for_owner(callee_owner);
                        self.summarize_perform_write_behavior(
                            callee_unit,
                            callee_routine,
                            parameter_symbol,
                        )
                    })
                    .map(|summary| {
                        format!(
                            "{} writes {} ({summary})",
                            callee_summary.name, parameter.name
                        )
                    })
                    .unwrap_or_else(|| {
                        format!("{} writes {}", callee_summary.name, parameter.name)
                    });
                let perform_write_node_id = provenance.add_flow_node(
                    "perform_write",
                    &write_summary,
                    Some(context.unit.uri.as_ref()),
                    Some(byte_range(&perform_call.range)),
                    Some(snippet(
                        self.snapshot.project_text(context.unit.uri.as_ref()),
                        &perform_call.range,
                    )),
                );
                provenance.add_edge(&perform_write_node_id, sink_node_id, "writes", None);

                if let Some(parameter_symbol) = parameter.symbol {
                    let callee_unit =
                        &self.snapshot.project.units[parameter_symbol.unit.as_usize()];
                    let callee_routine = self
                        .snapshot
                        .routine_analysis()
                        .routine_for_owner(callee_owner);
                    let parameter_access = ValueAccess {
                        handle: parameter_symbol,
                        field_path: access.field_path.clone(),
                        display: format_access_display_from_path(
                            callee_unit.symbol(parameter_symbol.symbol).name.as_ref(),
                            &access.field_path,
                        ),
                        range: callee_unit
                            .symbol(parameter_symbol.symbol)
                            .decl_range
                            .clone(),
                    };
                    self.trace_symbol_access(
                        TraceContext {
                            unit: callee_unit,
                            routine: callee_routine,
                            sink_offset: usize::MAX,
                        },
                        &parameter_access,
                        target_path,
                        field_mappings,
                        notes,
                        depth,
                        provenance,
                        &perform_write_node_id,
                    );
                }
                found = true;
            }

            found |= self.trace_global_writes_from_perform_call(
                context,
                access,
                target_path,
                field_mappings,
                notes,
                depth,
                provenance,
                sink_node_id,
                perform_call,
                callee_owner,
                callee_summary,
            );
        }
        found
    }

    #[allow(clippy::too_many_arguments)]
    fn trace_global_writes_from_perform_call(
        &mut self,
        context: TraceContext<'a>,
        access: &ValueAccess,
        target_path: &str,
        field_mappings: &mut Vec<CallDataflowFieldMapping>,
        notes: &mut Vec<String>,
        depth: usize,
        provenance: &mut ParameterProvenanceBuilder,
        sink_node_id: &str,
        perform_call: &abap_symbols::PerformCallData,
        callee_owner: SymbolHandle,
        callee_summary: &super::CallableSummary,
    ) -> bool {
        let access_unit = &self.snapshot.project.units[access.handle.unit.as_usize()];
        let access_symbol = access_unit.symbol(access.handle.symbol);
        if access_symbol.scope != access_unit.root_scope {
            return false;
        }

        let callee_unit = &self.snapshot.project.units[callee_owner.unit.as_usize()];
        let callee_routine = self
            .snapshot
            .routine_analysis()
            .routine_for_owner(callee_owner);
        let Some(callee_routine) = callee_routine else {
            return false;
        };
        if !self.routine_has_assignment_writer(callee_unit, callee_routine, access) {
            return false;
        }

        let call_text = snippet(
            self.snapshot.project_text(context.unit.uri.as_ref()),
            &perform_call.range,
        );
        field_mappings.push(CallDataflowFieldMapping {
            target_path: target_path.to_string(),
            source_kind: "perform_write".to_string(),
            source_display: format!("{}:{}", callee_summary.name, access.display),
            source_unit_uri: Some(context.unit.uri.to_string()),
            source_range: Some(byte_range(&perform_call.range)),
            statement_text: Some(call_text.clone()),
        });
        let perform_write_node_id = provenance.add_flow_node(
            "perform_write",
            &format!("{} writes {}", callee_summary.name, access.display),
            Some(context.unit.uri.as_ref()),
            Some(byte_range(&perform_call.range)),
            Some(call_text),
        );
        provenance.add_edge(&perform_write_node_id, sink_node_id, "writes", None);
        self.trace_symbol_access(
            TraceContext {
                unit: callee_unit,
                routine: Some(callee_routine),
                sink_offset: usize::MAX,
            },
            access,
            target_path,
            field_mappings,
            notes,
            depth,
            provenance,
            &perform_write_node_id,
        );
        true
    }

    fn summarize_perform_write_behavior(
        &self,
        unit: &UnitAnalysis,
        routine: Option<&RoutineAnalysis>,
        parameter_symbol: SymbolHandle,
    ) -> Option<String> {
        let routine = routine?;
        let source_text = self.snapshot.project_text(unit.uri.as_ref())?;
        let formal_name = unit.symbol(parameter_symbol.symbol).name.as_ref();
        let formal_name_lower = formal_name.to_ascii_lowercase();

        let cleared_target = unit.routine_sites.iter().any(|site| {
            site.kind == abap_symbols::RoutineSiteKind::Clear
                && same_routine_scope(self.snapshot, unit, site.scope, routine)
                && (site.target_range.as_ref().is_some_and(|target_range| {
                    value_accesses_in_range(unit, target_range)
                        .iter()
                        .any(|access| access.handle == parameter_symbol)
                }) || snippet_without_comments(Some(source_text), &site.range)
                    .to_ascii_lowercase()
                    .contains(&formal_name_lower))
        });

        let appended_target = unit.assignment_sites.iter().any(|assignment| {
            same_routine_scope(self.snapshot, unit, assignment.scope, routine)
                && assignment_access(unit, assignment)
                    .is_some_and(|lhs_access| lhs_access.handle == parameter_symbol)
                && snippet_without_comments(Some(source_text), &assignment.range)
                    .to_ascii_lowercase()
                    .starts_with("append ")
        });

        let mut read_keys = Vec::<String>::new();
        let mut seen_read_table_ranges = HashSet::<(usize, usize)>::new();
        for update in &unit.system_field_updates {
            if update.statement != abap_symbols::SystemFieldStatementKind::ReadTable
                || !same_routine_scope(self.snapshot, unit, update.scope, routine)
                || !seen_read_table_ranges.insert((update.range.start, update.range.end))
            {
                continue;
            }
            let statement = snippet_without_comments(Some(source_text), &update.range);
            let statement_lower = statement.to_ascii_lowercase();
            if !statement_lower.starts_with("read table ")
                || !statement_lower.contains(&formal_name_lower)
            {
                continue;
            }
            read_keys.extend(parse_read_table_key_fields(&statement));
        }
        dedup_case_insensitive(&mut read_keys);

        let mut additive_fields = Vec::<String>::new();
        for assignment in &unit.assignment_sites {
            if !same_routine_scope(self.snapshot, unit, assignment.scope, routine) {
                continue;
            }
            let Some(lhs_access) = assignment_access(unit, assignment) else {
                continue;
            };
            if !assignment.rhs_is_top_level_sum {
                continue;
            }
            let mutates_target = lhs_access.handle == parameter_symbol
                || (unit.symbol(lhs_access.handle.symbol).kind == SymbolKind::FieldSymbol
                    && field_symbol_bound_to_table_parameter(
                        unit,
                        routine,
                        source_text,
                        &formal_name_lower,
                        unit.symbol(lhs_access.handle.symbol).name.as_ref(),
                        assignment.range.start,
                    ));
            if !mutates_target {
                continue;
            }
            if let Some(field_name) = lhs_access.field_path.last() {
                additive_fields.push(field_name.clone());
            }
        }
        dedup_case_insensitive(&mut additive_fields);

        let mut parts = Vec::new();
        if !read_keys.is_empty() {
            parts.push(format!("merge by {}", read_keys.join(", ")));
        } else if cleared_target && appended_target {
            parts.push("rebuild rows".to_string());
        }
        if !additive_fields.is_empty() {
            parts.push(format!("sum {}", additive_fields.join(", ")));
        } else if appended_target && read_keys.is_empty() {
            parts.push("append rows".to_string());
        }
        if parts.is_empty() && cleared_target {
            parts.push(format!("clear {formal_name}"));
        }

        (!parts.is_empty()).then(|| parts.join("; "))
    }

    fn routine_has_assignment_writer(
        &self,
        unit: &UnitAnalysis,
        routine: &RoutineAnalysis,
        access: &ValueAccess,
    ) -> bool {
        unit.assignment_sites.iter().any(|assignment| {
            same_routine_scope(self.snapshot, unit, assignment.scope, routine)
                && assignment_access(unit, assignment).is_some_and(|lhs_access| {
                    lhs_access.handle == access.handle
                        && relevant_target_suffix(&access.field_path, &lhs_access.field_path)
                            .is_some()
                })
        })
    }

    fn trace_producer_call_outputs(
        &self,
        context: TraceContext<'a>,
        access: &ValueAccess,
        target_path: &str,
        field_mappings: &mut Vec<CallDataflowFieldMapping>,
        provenance: &mut ParameterProvenanceBuilder,
        sink_node_id: &str,
    ) -> bool {
        let Some(current_routine) = context.routine else {
            return false;
        };
        let mut found = false;
        for call_site in &context.unit.call_sites {
            if call_site.range.end > context.sink_offset
                || !same_routine_scope(
                    self.snapshot,
                    context.unit,
                    call_site.scope,
                    current_routine,
                )
            {
                continue;
            }
            for argument in &call_site.arguments {
                let accesses = value_accesses_in_range(context.unit, &argument.range);
                if accesses.len() != 1
                    || accesses[0].handle != access.handle
                    || accesses[0].field_path != access.field_path
                {
                    continue;
                }
                let direction = self.argument_direction(context.unit, call_site, argument);
                if matches!(direction, TraceDirection::Input) {
                    continue;
                }
                field_mappings.push(CallDataflowFieldMapping {
                    target_path: target_path.to_string(),
                    source_kind: "call_output".to_string(),
                    source_display: format!(
                        "{}.{}",
                        call_target_name(&call_site.target),
                        argument
                            .name
                            .as_ref()
                            .map(|name| name.to_string())
                            .unwrap_or_else(|| format!("arg{}", argument.ordinal + 1))
                    ),
                    source_unit_uri: Some(context.unit.uri.to_string()),
                    source_range: Some(byte_range(&call_site.range)),
                    statement_text: Some(snippet(
                        self.snapshot.project_text(context.unit.uri.as_ref()),
                        &call_site.range,
                    )),
                });
                let call_output_node_id = provenance.add_flow_node(
                    "call_output",
                    &format!(
                        "{}.{}",
                        call_target_name(&call_site.target),
                        argument
                            .name
                            .as_ref()
                            .map(|name| name.to_string())
                            .unwrap_or_else(|| format!("arg{}", argument.ordinal + 1))
                    ),
                    Some(context.unit.uri.as_ref()),
                    Some(byte_range(&call_site.range)),
                    Some(snippet(
                        self.snapshot.project_text(context.unit.uri.as_ref()),
                        &call_site.range,
                    )),
                );
                provenance.add_edge(&call_output_node_id, sink_node_id, "produces", None);
                found = true;
            }
        }
        found
    }

    fn trace_sql_writers(
        &self,
        context: TraceContext<'a>,
        access: &ValueAccess,
        target_path: &str,
        field_mappings: &mut Vec<CallDataflowFieldMapping>,
        provenance: &mut ParameterProvenanceBuilder,
        sink_node_id: &str,
    ) -> bool {
        let Some(current_routine) = context.routine else {
            return false;
        };
        let symbol = &self.snapshot.project.units[access.handle.unit.as_usize()].symbols
            [access.handle.symbol.as_usize()];
        let mut found = false;
        for target in &context.unit.sql_targets {
            if target.range.end > context.sink_offset
                || !same_routine_scope(self.snapshot, context.unit, target.scope, current_routine)
                || target.target_name.as_deref() != Some(symbol.name.as_ref())
            {
                continue;
            }
            field_mappings.push(CallDataflowFieldMapping {
                target_path: target_path.to_string(),
                source_kind: "sql_target".to_string(),
                source_display: access.display.clone(),
                source_unit_uri: Some(context.unit.uri.to_string()),
                source_range: Some(byte_range(&target.range)),
                statement_text: Some(snippet(
                    self.snapshot.project_text(context.unit.uri.as_ref()),
                    &target.range,
                )),
            });
            if let Some(query) = context
                .unit
                .sql_queries
                .iter()
                .find(|query| query.id == target.query_id)
            {
                let source_text = self.snapshot.project_text(context.unit.uri.as_ref());
                let query_statement = snippet(source_text, &query.range);
                let query_node_id = provenance.add_query_node(
                    context.unit.uri.as_ref(),
                    query,
                    &summarize_sql_query(context.unit, query, target, source_text),
                    query_statement,
                );
                let mut source_node_ids = HashMap::<String, String>::new();
                for source in context
                    .unit
                    .sql_sources
                    .iter()
                    .filter(|source| source.query_id == query.id)
                {
                    let source_node_id =
                        provenance.add_sql_source_node(context.unit.uri.as_ref(), query.id, source);
                    provenance.add_edge(&source_node_id, &query_node_id, "reads_from", None);
                    source_node_ids.insert(source.name.to_string(), source_node_id.clone());
                    if let Some(alias) = source.alias.as_deref() {
                        source_node_ids.insert(alias.to_string(), source_node_id.clone());
                    }
                }
                let relevant_projections =
                    sql_relevant_query_projections(context.unit, query.id, access);
                let target_field_node_id = ((!access.field_path.is_empty()
                    || relevant_projections.len() == 1)
                    && !access.display.is_empty())
                .then(|| {
                    let target_field_range =
                        sql_target_field_precise_range(context.unit, target, access)
                            .or_else(|| target.target_range.clone())
                            .unwrap_or_else(|| target.range.clone());
                    provenance.add_flow_node(
                        "sql_target_field",
                        &access.display,
                        Some(context.unit.uri.as_ref()),
                        Some(byte_range(&target_field_range)),
                        None,
                    )
                });
                if let Some(target_field_node_id) = target_field_node_id.as_ref() {
                    provenance.add_edge(&query_node_id, target_field_node_id, "selects_into", None);
                    provenance.add_edge(target_field_node_id, sink_node_id, "flows_to", None);
                } else {
                    provenance.add_edge(&query_node_id, sink_node_id, "selects_into", None);
                }
                for projection in relevant_projections {
                    let projection_label = sql_projection_source_field_label(
                        context.unit,
                        query.id,
                        projection,
                        source_text,
                    )
                    .unwrap_or_else(|| sql_projection_label(projection, source_text));
                    let projection_node_id = provenance.add_flow_node(
                        "sql_source_field",
                        &projection_label,
                        Some(context.unit.uri.as_ref()),
                        Some(byte_range(
                            &sql_projection_precise_range(context.unit, query.id, projection)
                                .unwrap_or_else(|| projection.range.clone()),
                        )),
                        None,
                    );
                    provenance.add_edge(&projection_node_id, &query_node_id, "projects", None);
                    if let Some(source_lookup_key) =
                        sql_projection_source_lookup_key(context.unit, query.id, projection)
                        && let Some(source_node_id) =
                            source_node_ids.get(source_lookup_key.as_str())
                    {
                        provenance.add_edge(source_node_id, &projection_node_id, "column", None);
                    }
                }
                for predicate in context
                    .unit
                    .sql_predicates
                    .iter()
                    .filter(|predicate| predicate.query_id == query.id)
                {
                    let predicate_statement = snippet(source_text, &predicate.range);
                    let predicate_node_id = provenance.add_flow_node(
                        "sql_predicate",
                        &summarize_sql_predicate(predicate, source_text),
                        Some(context.unit.uri.as_ref()),
                        Some(byte_range(&predicate.range)),
                        Some(predicate_statement),
                    );
                    provenance.add_edge(&predicate_node_id, &query_node_id, "filters", None);

                    for input_access in value_accesses_in_range(context.unit, &predicate.range) {
                        let input_unit =
                            &self.snapshot.project.units[input_access.handle.unit.as_usize()];
                        let input_symbol = input_unit.symbol(input_access.handle.symbol);
                        let input_kind = if input_symbol.kind == SymbolKind::Constant
                            || input_symbol.kind == SymbolKind::BuiltinConstant
                        {
                            "constant".to_string()
                        } else {
                            classify_terminal_symbol(input_unit, input_symbol).to_string()
                        };
                        let input_node_id = provenance.add_flow_node(
                            &input_kind,
                            &input_access.display,
                            Some(input_unit.uri.as_ref()),
                            Some(byte_range(&input_access.range)),
                            input_symbol
                                .value_clause_display
                                .as_ref()
                                .map(|text| text.to_string()),
                        );
                        provenance.add_edge(&input_node_id, &predicate_node_id, "uses", None);
                    }
                }
            } else {
                let sql_node_id = provenance.add_flow_node(
                    "sql_target",
                    &snippet(
                        self.snapshot.project_text(context.unit.uri.as_ref()),
                        &target.range,
                    ),
                    Some(context.unit.uri.as_ref()),
                    Some(byte_range(&target.range)),
                    Some(snippet(
                        self.snapshot.project_text(context.unit.uri.as_ref()),
                        &target.range,
                    )),
                );
                provenance.add_edge(&sql_node_id, sink_node_id, "selects_into", None);
            }
            found = true;
        }
        found
    }

    fn trace_global_from_callers(
        &mut self,
        context: TraceContext<'a>,
        access: &ValueAccess,
        target_path: &str,
        field_mappings: &mut Vec<CallDataflowFieldMapping>,
        notes: &mut Vec<String>,
        depth: usize,
        provenance: &mut ParameterProvenanceBuilder,
        sink_node_id: &str,
    ) -> bool {
        let Some(current_routine) = context.routine else {
            return false;
        };
        let Some(owner) = current_routine.descriptor.owner else {
            return false;
        };
        let Some(inbound) = self.inbound_perform_calls(owner) else {
            return false;
        };
        let mut found = false;
        for (caller_unit, caller_routine, perform_call) in inbound {
            let caller_context = TraceContext {
                unit: caller_unit,
                routine: caller_routine,
                sink_offset: perform_call.range.start,
            };
            self.trace_symbol_access(
                caller_context,
                access,
                target_path,
                field_mappings,
                notes,
                depth,
                provenance,
                sink_node_id,
            );
            found = true;
        }
        found
    }

    fn trace_project_global_writers(
        &mut self,
        context: TraceContext<'a>,
        access: &ValueAccess,
        target_path: &str,
        field_mappings: &mut Vec<CallDataflowFieldMapping>,
        notes: &mut Vec<String>,
        depth: usize,
        provenance: &mut ParameterProvenanceBuilder,
        sink_node_id: &str,
    ) -> bool {
        let mut found = false;
        for writer_unit in &self.snapshot.project.units {
            for writer_routine in self
                .snapshot
                .routine_analysis()
                .routines_for_unit(writer_unit.unit_id)
            {
                let writer_context = TraceContext {
                    unit: writer_unit,
                    routine: Some(writer_routine),
                    sink_offset: if writer_unit.unit_id == context.unit.unit_id
                        && writer_routine.descriptor.owner
                            == context.routine.and_then(|routine| routine.descriptor.owner)
                    {
                        context.sink_offset
                    } else {
                        usize::MAX
                    },
                };
                found |= self.trace_assignment_writers(
                    writer_context,
                    access,
                    target_path,
                    field_mappings,
                    notes,
                    depth,
                    provenance,
                    sink_node_id,
                );
                found |= self.trace_sql_writers(
                    writer_context,
                    access,
                    target_path,
                    field_mappings,
                    provenance,
                    sink_node_id,
                );
            }
        }
        found
    }

    fn inbound_perform_calls(
        &self,
        owner: SymbolHandle,
    ) -> Option<
        Vec<(
            &'a UnitAnalysis,
            Option<&'a RoutineAnalysis>,
            &'a PerformCallData,
        )>,
    > {
        let callee_name = self.snapshot.project.units[owner.unit.as_usize()]
            .symbol(owner.symbol)
            .name
            .to_ascii_lowercase();
        let mut out = Vec::new();
        for caller_unit in &self.snapshot.project.units {
            for perform_call in &caller_unit.perform_calls {
                let resolves_to_owner =
                    perform_call_target(&self.snapshot.project, caller_unit, perform_call)
                        == Some(owner);
                let matches_name = perform_call
                    .routine_name
                    .as_ref()
                    .eq_ignore_ascii_case(&callee_name);
                if !resolves_to_owner && !matches_name {
                    continue;
                }
                let caller_routine = self
                    .snapshot
                    .routine_analysis()
                    .routine_for_scope(caller_unit.unit_id, perform_call.scope);
                out.push((caller_unit, caller_routine, perform_call));
            }
        }
        (!out.is_empty()).then_some(out)
    }

    fn match_json(&self, matched: &MatchedCall<'a>) -> CallDataflowMatch {
        CallDataflowMatch {
            occurrence: matched.occurrence,
            target_kind: matched.target_kind.clone(),
            target_name: matched.target_name.clone(),
            caller_kind: matched.caller.map(|caller| routine_kind_name(caller)),
            caller_name: matched
                .caller
                .map(|caller| caller.descriptor.name.to_string()),
            caller_unit_uri: matched.caller.map(|caller| {
                self.snapshot.project.units[caller.descriptor.unit.as_usize()]
                    .uri
                    .to_string()
            }),
            unit_uri: matched.unit.uri.to_string(),
            call_range: byte_range(&matched.call.range),
        }
    }

    fn selected_call_json(&self, matched: &MatchedCall<'a>) -> CallDataflowSelectedCall {
        CallDataflowSelectedCall {
            occurrence: matched.occurrence,
            target_kind: matched.target_kind.clone(),
            target_name: matched.target_name.clone(),
            unit_uri: matched.unit.uri.to_string(),
            call_range: byte_range(&matched.call.range),
            caller_node_id: matched.caller_node_id.as_ref().map(|id| id.to_string()),
            caller_kind: matched.caller.map(|caller| routine_kind_name(caller)),
            caller_name: matched
                .caller
                .map(|caller| caller.descriptor.name.to_string()),
            caller_unit_uri: matched.caller.map(|caller| {
                self.snapshot.project.units[caller.descriptor.unit.as_usize()]
                    .uri
                    .to_string()
            }),
            target_node_id: matched.target_node_id.as_ref().map(|id| id.to_string()),
            argument_count: matched.call.arguments.len(),
        }
    }
}

impl ParameterProvenanceBuilder {
    fn new(
        parameter_path: &str,
        direction: &str,
        section: Option<&str>,
        argument_type: Option<&str>,
    ) -> Self {
        let mut builder = Self {
            parameter_path: parameter_path.to_string(),
            ..Self::default()
        };
        let mut label = parameter_path.to_string();
        label.push_str(" [");
        label.push_str(direction);
        if let Some(section) = section {
            label.push_str(" / ");
            label.push_str(section);
        }
        label.push(']');
        if let Some(argument_type) = argument_type {
            label.push_str(" : ");
            label.push_str(argument_type);
        }
        builder.create_node(
            &format!("target|{}", parameter_path),
            "parameter",
            &label,
            None,
            None,
            None,
        );
        builder
    }

    fn finish(mut self) -> CallDataflowProvenanceGraph {
        self.graph.nodes.sort_by(|left, right| {
            left.kind
                .cmp(&right.kind)
                .then(left.label.cmp(&right.label))
                .then(left.unit_uri.cmp(&right.unit_uri))
                .then(
                    left.range
                        .as_ref()
                        .map(|range| range.start)
                        .cmp(&right.range.as_ref().map(|range| range.start)),
                )
                .then(
                    left.range
                        .as_ref()
                        .map(|range| range.end)
                        .cmp(&right.range.as_ref().map(|range| range.end)),
                )
        });
        self.graph.edges.sort_by(|left, right| {
            left.source
                .cmp(&right.source)
                .then(left.target.cmp(&right.target))
                .then(left.kind.cmp(&right.kind))
                .then(left.label.cmp(&right.label))
        });
        self.graph
    }

    fn root_id(&mut self) -> String {
        let parameter_path = self.parameter_path.clone();
        self.ensure_target_path_node(&parameter_path)
    }

    fn ensure_target_path_node(&mut self, target_path: &str) -> String {
        if target_path == self.parameter_path {
            let parameter_path = self.parameter_path.clone();
            return self.create_node(
                &format!("target|{}", parameter_path),
                "parameter",
                &parameter_path,
                None,
                None,
                None,
            );
        }

        let node_kind = target_node_kind(target_path);
        let node_id = self.create_node(
            &format!("target|{target_path}"),
            node_kind,
            target_path,
            None,
            None,
            None,
        );
        if let Some(parent_path) = parent_target_path(target_path, &self.parameter_path) {
            let parent_id = self.ensure_target_path_node(&parent_path);
            self.add_edge(&node_id, &parent_id, "populates", None);
        }
        node_id
    }

    fn add_flow_node(
        &mut self,
        kind: &str,
        label: &str,
        unit_uri: Option<&str>,
        range: Option<CallDataflowByteRange>,
        statement_text: Option<String>,
    ) -> String {
        let range_key = range
            .as_ref()
            .map(|range| format!("{}:{}", range.start, range.end))
            .unwrap_or_default();
        let statement_key = statement_text.as_deref().unwrap_or_default();
        self.create_node(
            &format!(
                "flow|{kind}|{}|{}|{}|{}",
                label,
                unit_uri.unwrap_or_default(),
                range_key,
                statement_key
            ),
            kind,
            label,
            unit_uri.map(str::to_string),
            range,
            statement_text,
        )
    }

    fn add_query_node(
        &mut self,
        unit_uri: &str,
        query: &SqlQueryData,
        label: &str,
        statement_text: String,
    ) -> String {
        self.create_node(
            &format!("sql_query|{unit_uri}|{}", query.id),
            "sql_query",
            label,
            Some(unit_uri.to_string()),
            Some(byte_range(&query.range)),
            Some(statement_text),
        )
    }

    fn add_sql_source_node(
        &mut self,
        unit_uri: &str,
        query_id: usize,
        source: &SqlSourceData,
    ) -> String {
        let mut label = match source.source_kind {
            SqlSourceKind::From => format!("FROM {}", source.name),
            SqlSourceKind::Join => format!(
                "{} {}",
                source
                    .join_kind
                    .as_deref()
                    .map(str::to_string)
                    .unwrap_or_else(|| "JOIN".to_string()),
                source.name
            ),
        };
        if let Some(alias) = source.alias.as_deref() {
            label.push_str(" AS ");
            label.push_str(alias);
        }
        self.create_node(
            &format!(
                "sql_source|{unit_uri}|{query_id}|{}|{}",
                source.name, source.range.start
            ),
            "sql_source",
            &label,
            Some(unit_uri.to_string()),
            Some(byte_range(&source.range)),
            None,
        )
    }

    fn add_edge(&mut self, source: &str, target: &str, kind: &str, label: Option<String>) {
        let edge_key = format!(
            "{source}|{target}|{kind}|{}",
            label.as_deref().unwrap_or_default()
        );
        if !self.edge_keys.insert(edge_key) {
            return;
        }
        self.graph.edges.push(CallDataflowProvenanceEdge {
            source: source.to_string(),
            target: target.to_string(),
            kind: kind.to_string(),
            label,
        });
    }

    fn node_kind(&self, node_id: &str) -> Option<&str> {
        self.graph
            .nodes
            .iter()
            .find(|node| node.id == node_id)
            .map(|node| node.kind.as_str())
    }

    fn create_node(
        &mut self,
        key: &str,
        kind: &str,
        label: &str,
        unit_uri: Option<String>,
        range: Option<CallDataflowByteRange>,
        statement_text: Option<String>,
    ) -> String {
        if let Some(existing) = self.node_keys.get(key) {
            return existing.clone();
        }
        let node_id = format!("p{}", self.next_node_id);
        self.next_node_id += 1;
        self.graph.nodes.push(CallDataflowProvenanceNode {
            id: node_id.clone(),
            kind: kind.to_string(),
            label: label.to_string(),
            unit_uri,
            range,
            statement_text,
        });
        self.node_keys.insert(key.to_string(), node_id.clone());
        node_id
    }
}

fn byte_range(range: &TextRange) -> CallDataflowByteRange {
    CallDataflowByteRange {
        start: range.start,
        end: range.end,
    }
}

fn call_graph_edge_kind_name(kind: CallGraphEdgeKind) -> &'static str {
    match kind {
        CallGraphEdgeKind::MethodCall => "method_call",
        CallGraphEdgeKind::Perform => "perform",
        CallGraphEdgeKind::FunctionCall => "function_call",
    }
}

fn call_target_kind_name(target: &NamedArgumentTarget) -> &'static str {
    match target {
        NamedArgumentTarget::Constructor { .. } => "constructor",
        NamedArgumentTarget::Function { .. } => "function",
        NamedArgumentTarget::Report { .. } => "report",
        NamedArgumentTarget::Routine { .. } => "routine",
        NamedArgumentTarget::ImplicitMethod { .. } | NamedArgumentTarget::Method { .. } => "method",
        NamedArgumentTarget::Event { .. } => "event",
    }
}

fn call_target_name(target: &NamedArgumentTarget) -> String {
    match target {
        NamedArgumentTarget::Constructor { type_name } => format!("{type_name}~constructor"),
        NamedArgumentTarget::Function { function_name } => function_name.to_string(),
        NamedArgumentTarget::Report { report_name } => report_name.to_string(),
        NamedArgumentTarget::Routine { routine_name } => routine_name.to_string(),
        NamedArgumentTarget::ImplicitMethod { method_name } => method_name.to_string(),
        NamedArgumentTarget::Method {
            base_name,
            method_name,
            ..
        } => format!("{base_name}->{method_name}"),
        NamedArgumentTarget::Event {
            qualifier,
            event_name,
        } => qualifier
            .as_ref()
            .map(|qualifier| format!("{qualifier}~{event_name}"))
            .unwrap_or_else(|| event_name.to_string()),
    }
}

fn call_target_matches(target: &NamedArgumentTarget, query: &str) -> bool {
    let name = call_target_name(target).to_ascii_lowercase();
    if name == query {
        return true;
    }
    match target {
        NamedArgumentTarget::Method {
            base_name,
            method_name,
            ..
        } => {
            method_name.as_ref().eq_ignore_ascii_case(query)
                || format!("{base_name}~{method_name}").eq_ignore_ascii_case(query)
        }
        NamedArgumentTarget::ImplicitMethod { method_name } => {
            method_name.as_ref().eq_ignore_ascii_case(query)
        }
        NamedArgumentTarget::Function { function_name } => {
            function_name.as_ref().eq_ignore_ascii_case(query)
        }
        NamedArgumentTarget::Routine { routine_name } => {
            routine_name.as_ref().eq_ignore_ascii_case(query)
        }
        NamedArgumentTarget::Report { report_name } => {
            report_name.as_ref().eq_ignore_ascii_case(query)
        }
        NamedArgumentTarget::Constructor { type_name } => {
            format!("{type_name}~constructor").eq_ignore_ascii_case(query)
        }
        NamedArgumentTarget::Event {
            qualifier,
            event_name,
        } => event_name.as_ref().eq_ignore_ascii_case(query)
            || qualifier
                .as_ref()
                .is_some_and(|qualifier| format!("{qualifier}~{event_name}").eq_ignore_ascii_case(query)),
    }
}

fn caller_matches(caller: Option<&RoutineAnalysis>, query: &str) -> bool {
    let Some(caller) = caller else {
        return false;
    };
    caller.descriptor.name.as_ref().eq_ignore_ascii_case(query)
}

fn routine_kind_name(routine: &RoutineAnalysis) -> String {
    match routine.descriptor.kind {
        abap_symbols::RoutineKind::GlobalDeclarations => "global_declarations",
        abap_symbols::RoutineKind::Method => "method",
        abap_symbols::RoutineKind::Form => "form",
        abap_symbols::RoutineKind::Module => "module",
        abap_symbols::RoutineKind::EventBlock => "event_block",
    }
    .to_string()
}

fn lifecycle_node_from_call_graph(
    node: &super::CallGraphNode,
    synthetic: bool,
) -> CallDataflowLifecycleNode {
    CallDataflowLifecycleNode {
        id: node.id.to_string(),
        kind: call_graph_node_kind_name(node.kind).to_string(),
        name: node.name.to_string(),
        unit_uri: node.unit_uri.to_string(),
        decl_range: byte_range(&node.decl_range),
        synthetic,
    }
}

fn call_graph_node_kind_name(kind: CallGraphNodeKind) -> &'static str {
    match kind {
        CallGraphNodeKind::Method => "method",
        CallGraphNodeKind::Form => "form",
        CallGraphNodeKind::FunctionModule => "function_module",
        CallGraphNodeKind::EventBlock => "event_block",
        CallGraphNodeKind::Report => "report",
    }
}

fn same_routine_scope(
    snapshot: &AnalysisSnapshot,
    unit: &UnitAnalysis,
    scope: ScopeId,
    routine: &RoutineAnalysis,
) -> bool {
    snapshot
        .routine_analysis()
        .routine_for_scope(unit.unit_id, scope)
        .map(|candidate| candidate.descriptor.id == routine.descriptor.id)
        .unwrap_or(false)
}

fn assignment_access(unit: &UnitAnalysis, assignment: &AssignmentSiteData) -> Option<ValueAccess> {
    if let Some(access) = assignment.lhs_target_access.as_ref() {
        return field_access_to_value_access(unit, access, &[]);
    }
    let accesses = value_accesses_in_range(unit, &assignment.lhs_range);
    (accesses.len() == 1).then(|| accesses[0].clone())
}

fn resolve_access_handle(
    unit: &UnitAnalysis,
    access: &abap_symbols::FieldAccess,
) -> Option<SymbolHandle> {
    let reference = unit.references.iter().find(|reference| {
        reference.scope == access.scope
            && reference.namespace == access.base_namespace
            && reference.range == access.base_range
            && reference.name == access.base_name
    })?;
    let Resolution::Symbol(handle) = reference.resolution? else {
        return None;
    };
    Some(handle)
}

fn value_accesses_in_range(unit: &UnitAnalysis, range: &TextRange) -> Vec<ValueAccess> {
    let mut out = Vec::new();

    for access in &unit.field_accesses {
        if access.base_namespace != Namespace::Value {
            continue;
        }
        let access_end = access
            .field_path
            .last()
            .map(|segment| segment.range.end)
            .unwrap_or(access.base_range.end);
        if access.base_range.start < range.start || access_end > range.end {
            continue;
        }
        let Some(handle) = resolve_access_handle(unit, access) else {
            continue;
        };
        out.push(ValueAccess {
            handle,
            field_path: access
                .field_path
                .iter()
                .map(|segment| segment.name.to_string())
                .collect(),
            display: format_access_display(&access.base_name, &access.field_path),
            range: access.base_range.start..access_end,
        });
    }
    if !out.is_empty() {
        out.sort_by(|left, right| {
            left.range
                .start
                .cmp(&right.range.start)
                .then(left.range.end.cmp(&right.range.end))
                .then(left.display.cmp(&right.display))
        });
        out.dedup_by(|left, right| left.range == right.range && left.display == right.display);
        return out;
    }

    let mut refs: Vec<_> = unit
        .references
        .iter()
        .filter(|reference| {
            reference.namespace == Namespace::Value
                && reference.range.start >= range.start
                && reference.range.end <= range.end
        })
        .filter_map(|reference| {
            let Resolution::Symbol(handle) = reference.resolution? else {
                return None;
            };
            Some(ValueAccess {
                handle,
                field_path: Vec::new(),
                display: reference.name.to_string(),
                range: reference.range.clone(),
            })
        })
        .collect();
    refs.sort_by(|left, right| {
        left.range
            .start
            .cmp(&right.range.start)
            .then(left.range.end.cmp(&right.range.end))
            .then(left.display.cmp(&right.display))
    });
    refs.dedup_by(|left, right| left.range == right.range && left.display == right.display);
    refs
}

fn field_symbol_bound_to_table_parameter(
    unit: &UnitAnalysis,
    routine: &RoutineAnalysis,
    source_text: &str,
    table_parameter_name_lower: &str,
    field_symbol_name: &str,
    before_offset: usize,
) -> bool {
    let scope_range = &routine.descriptor.scope_range;
    let field_symbol_name_lower = field_symbol_name.to_ascii_lowercase();
    unit.system_field_updates.iter().any(|update| {
        update.statement == abap_symbols::SystemFieldStatementKind::ReadTable
            && update.range.start >= scope_range.start
            && update.range.end <= scope_range.end
            && update.range.end <= before_offset
            && {
                let statement = snippet_without_comments(Some(source_text), &update.range);
                let statement_lower = statement.to_ascii_lowercase();
                statement_lower.starts_with("read table ")
                    && statement_lower.contains(table_parameter_name_lower)
                    && statement_lower.contains(&field_symbol_name_lower)
            }
    })
}

fn parse_read_table_key_fields(statement: &str) -> Vec<String> {
    let Some(key_clause) = read_table_key_clause(statement) else {
        return Vec::new();
    };
    let normalized = key_clause
        .replace('=', " = ")
        .replace('.', " ")
        .replace(',', " ");
    let tokens: Vec<_> = normalized.split_whitespace().collect();
    let mut fields = Vec::new();
    for index in 1..tokens.len() {
        if tokens[index] != "=" {
            continue;
        }
        let field = clean_identifier_token(tokens[index - 1]);
        if !field.is_empty() {
            fields.push(field.to_string());
        }
    }
    dedup_case_insensitive(&mut fields);
    fields
}

fn read_table_key_clause(statement: &str) -> Option<&str> {
    let uppercase = statement.to_ascii_uppercase();
    let key_start = uppercase
        .find(" WITH TABLE KEY ")
        .map(|index| index + " WITH TABLE KEY ".len())
        .or_else(|| {
            uppercase
                .find(" WITH KEY ")
                .map(|index| index + " WITH KEY ".len())
        })?;
    let suffix = &statement[key_start..];
    let suffix_upper = &uppercase[key_start..];
    let mut end = suffix.len();
    for marker in [
        " BINARY SEARCH",
        " TRANSPORTING",
        " COMPARING",
        " INTO ",
        " ASSIGNING ",
        " REFERENCE INTO ",
    ] {
        if let Some(index) = suffix_upper.find(marker) {
            end = end.min(index);
        }
    }
    Some(suffix[..end].trim())
}

fn clean_identifier_token(token: &str) -> &str {
    token.trim_matches(|ch: char| matches!(ch, '(' | ')' | '[' | ']' | ',' | '.'))
}

fn dedup_case_insensitive(values: &mut Vec<String>) {
    let mut seen = HashSet::<String>::new();
    values.retain(|value| seen.insert(value.to_ascii_lowercase()));
}

fn field_access_path(access: &abap_symbols::FieldAccess) -> Vec<String> {
    access
        .field_path
        .iter()
        .map(|segment| segment.name.to_string())
        .collect()
}

fn field_access_range(access: &abap_symbols::FieldAccess) -> TextRange {
    access
        .field_path
        .last()
        .map(|segment| access.base_range.start..segment.range.end)
        .unwrap_or_else(|| access.base_range.clone())
}

fn field_access_to_value_access(
    unit: &UnitAnalysis,
    access: &abap_symbols::FieldAccess,
    suffix: &[String],
) -> Option<ValueAccess> {
    if access.base_namespace != Namespace::Value {
        return None;
    }
    let handle = resolve_access_handle(unit, access)?;
    let mut field_path = field_access_path(access);
    field_path.extend(suffix.iter().cloned());
    Some(ValueAccess {
        handle,
        display: format_access_display_from_path(access.base_name.as_ref(), &field_path),
        field_path,
        range: field_access_range(access),
    })
}

fn relevant_target_suffix(source_path: &[String], lhs_path: &[String]) -> Option<Vec<String>> {
    if source_path.is_empty() {
        return Some(lhs_path.to_vec());
    }
    if lhs_path.starts_with(source_path) {
        return Some(lhs_path[source_path.len()..].to_vec());
    }
    if lhs_path.is_empty() {
        return Some(Vec::new());
    }
    None
}

fn append_target_path(target_path: &str) -> String {
    if target_path.ends_with("[*]") {
        target_path.to_string()
    } else {
        format!("{target_path}[*]")
    }
}

fn join_target_path(base: &str, suffix: &[String]) -> String {
    if suffix.is_empty() {
        return base.to_string();
    }
    if base.is_empty() {
        return suffix.join(".");
    }
    format!("{base}.{}", suffix.join("."))
}

fn parent_target_path(target_path: &str, parameter_path: &str) -> Option<String> {
    if target_path == parameter_path {
        return None;
    }
    if let Some((parent, _)) = target_path.rsplit_once('.') {
        return Some(parent.to_string());
    }
    Some(parameter_path.to_string())
}

fn target_node_kind(target_path: &str) -> &'static str {
    if target_path.ends_with("[*]") {
        "target_table_row"
    } else if target_path.contains("[*].") {
        "target_table_field"
    } else if target_path.contains('.') {
        "target_field"
    } else {
        "target_value"
    }
}

fn summarize_sql_query(
    unit: &UnitAnalysis,
    query: &SqlQueryData,
    target: &SqlTargetData,
    source_text: Option<&str>,
) -> String {
    let mut lines = Vec::new();
    let mut select_line = String::from("SELECT");
    if query.is_single {
        select_line.push_str(" SINGLE");
    }
    if query.is_distinct {
        select_line.push_str(" DISTINCT");
    }
    let projection_text = sql_query_projection_text(unit, query, source_text);
    if let Some(projection_text) = projection_text {
        select_line.push(' ');
        select_line.push_str(&projection_text);
    } else {
        select_line.push_str(" *");
    }
    lines.push(select_line);

    if let Some(source_clause) = sql_query_source_clause(unit, query.id) {
        lines.push(format!("FROM {source_clause}"));
    }
    if let Some(target_clause) = sql_query_target_clause(target, source_text) {
        lines.push(target_clause);
    }
    for clause in sql_query_non_target_clause_lines(query, source_text) {
        lines.push(clause);
    }
    let host_inputs = sql_query_host_inputs(unit, query);
    if !host_inputs.is_empty() {
        lines.push(format!("HOSTS {}", host_inputs.join(", ")));
    }
    lines.join("\n")
}

fn summarize_sql_predicate(
    predicate: &abap_symbols::SqlPredicateData,
    source_text: Option<&str>,
) -> String {
    let kind = match predicate.kind {
        abap_symbols::SqlPredicateKind::Where => "WHERE",
        abap_symbols::SqlPredicateKind::JoinOn => "JOIN ON",
        abap_symbols::SqlPredicateKind::Having => "HAVING",
        abap_symbols::SqlPredicateKind::DynamicWhere => "DYNAMIC WHERE",
        abap_symbols::SqlPredicateKind::ForAllEntries => "FOR ALL ENTRIES",
    };
    let clause = snippet_without_comments(source_text, &predicate.range);
    if clause.trim().is_empty() {
        kind.to_string()
    } else {
        clause
    }
}

fn sql_query_projection_labels(
    unit: &UnitAnalysis,
    query_id: usize,
    source_text: Option<&str>,
) -> Vec<String> {
    let mut projections: Vec<_> = unit
        .sql_projections
        .iter()
        .filter(|projection| projection.query_id == query_id)
        .collect();
    projections.sort_by(|left, right| left.range.start.cmp(&right.range.start));
    projections
        .into_iter()
        .map(|projection| sql_projection_label(projection, source_text))
        .filter(|label| !label.is_empty())
        .collect()
}

fn sql_query_projection_text(
    unit: &UnitAnalysis,
    query: &SqlQueryData,
    source_text: Option<&str>,
) -> Option<String> {
    let structured = sql_query_projection_labels(unit, query.id, source_text);
    let clause_text = query
        .projection_clause
        .as_ref()
        .map(|range| projection_clause_text(source_text, range))
        .filter(|text| !text.is_empty());

    match (structured.is_empty(), clause_text) {
        (false, Some(clause_text))
            if structured.len() <= 1 && clause_text.split_whitespace().count() > 1 =>
        {
            Some(clause_text)
        }
        (false, _) => Some(structured.join(", ")),
        (true, Some(clause_text)) => Some(clause_text),
        (true, None) => None,
    }
}

fn sql_projection_label(projection: &SqlProjectionData, source_text: Option<&str>) -> String {
    match projection.kind {
        SqlProjectionKind::Star => "*".to_string(),
        SqlProjectionKind::QualifiedStar => projection
            .source_alias
            .as_deref()
            .map(|alias| format!("{alias}~*"))
            .unwrap_or_else(|| "*".to_string()),
        SqlProjectionKind::Column => {
            let Some(name) = projection.name.as_deref() else {
                return snippet_without_comments(source_text, &projection.range);
            };
            let mut label = projection
                .source_alias
                .as_deref()
                .map(|alias| format!("{alias}~{name}"))
                .unwrap_or_else(|| name.to_string());
            if let Some(alias) = projection.alias.as_deref()
                && alias != name
            {
                label.push_str(" AS ");
                label.push_str(alias);
            }
            label
        }
        SqlProjectionKind::Aggregate | SqlProjectionKind::Expression => {
            snippet_without_comments(source_text, &projection.range)
        }
    }
}

fn sql_relevant_query_projections<'a>(
    unit: &'a UnitAnalysis,
    query_id: usize,
    access: &ValueAccess,
) -> Vec<&'a SqlProjectionData> {
    let mut projections: Vec<_> = unit
        .sql_projections
        .iter()
        .filter(|projection| projection.query_id == query_id)
        .collect();
    projections.sort_by(|left, right| left.range.start.cmp(&right.range.start));
    if projections.is_empty() {
        return Vec::new();
    }

    if let Some(target_field_name) = access.field_path.last().map(String::as_str) {
        let matching: Vec<_> = projections
            .iter()
            .copied()
            .filter(|projection| {
                projection
                    .alias
                    .as_deref()
                    .or(projection.name.as_deref())
                    .is_some_and(|name| name.eq_ignore_ascii_case(target_field_name))
            })
            .collect();
        if !matching.is_empty() {
            return matching;
        }
    }

    if access.field_path.is_empty() && projections.len() == 1 {
        return vec![projections[0]];
    }

    Vec::new()
}

fn sql_projection_source_field_label(
    unit: &UnitAnalysis,
    query_id: usize,
    projection: &SqlProjectionData,
    source_text: Option<&str>,
) -> Option<String> {
    let field_name = projection.name.as_deref()?;
    let source_name = sql_projection_source_lookup_key(unit, query_id, projection)?;
    let mut label = format!("{source_name}.{field_name}");
    if let Some(alias) = projection.alias.as_deref()
        && alias != field_name
    {
        label.push_str(" AS ");
        label.push_str(alias);
    }
    if label.is_empty() {
        Some(sql_projection_label(projection, source_text))
    } else {
        Some(label)
    }
}

fn sql_projection_source_lookup_key(
    unit: &UnitAnalysis,
    query_id: usize,
    projection: &SqlProjectionData,
) -> Option<String> {
    if let Some(source_alias) = projection.source_alias.as_deref()
        && let Some(source) = unit.sql_sources.iter().find(|source| {
            source.query_id == query_id
                && (source.alias.as_deref() == Some(source_alias)
                    || source.name.as_ref() == source_alias)
        })
    {
        return Some(source.name.to_string());
    }

    let sources: Vec<_> = unit
        .sql_sources
        .iter()
        .filter(|source| source.query_id == query_id)
        .collect();
    if sources.len() == 1 {
        return Some(sources[0].name.to_string());
    }

    None
}

fn sql_projection_precise_range(
    unit: &UnitAnalysis,
    query_id: usize,
    projection: &SqlProjectionData,
) -> Option<TextRange> {
    let name = projection.name.as_deref()?;
    let mut matches: Vec<_> = unit
        .sql_name_refs
        .iter()
        .filter(|name_ref| {
            name_ref.query_id == query_id
                && name_ref.range.start >= projection.range.start
                && name_ref.range.end <= projection.range.end
                && name_ref.name.as_ref().eq_ignore_ascii_case(name)
                && matches!(
                    name_ref.kind,
                    SqlNameRefKind::Column | SqlNameRefKind::QualifiedColumn
                )
                && match projection.source_alias.as_deref() {
                    Some(alias) => name_ref.qualifier.as_deref() == Some(alias),
                    None => true,
                }
        })
        .collect();
    matches.sort_by(|left, right| {
        left.range
            .start
            .cmp(&right.range.start)
            .then(left.range.end.cmp(&right.range.end))
    });
    matches.first().map(|name_ref| name_ref.range.clone())
}

fn sql_target_field_precise_range(
    unit: &UnitAnalysis,
    target: &SqlTargetData,
    access: &ValueAccess,
) -> Option<TextRange> {
    let target_range = target.target_range.as_ref()?;
    let accesses = value_accesses_in_range(unit, target_range);
    accesses
        .iter()
        .find(|candidate| {
            candidate.handle == access.handle && candidate.field_path == access.field_path
        })
        .or_else(|| {
            access.field_path.is_empty().then(|| {
                accesses
                    .iter()
                    .find(|candidate| candidate.handle == access.handle)
            })?
        })
        .map(|candidate| candidate.range.clone())
}

fn sql_query_source_clause(unit: &UnitAnalysis, query_id: usize) -> Option<String> {
    let mut sources: Vec<_> = unit
        .sql_sources
        .iter()
        .filter(|source| source.query_id == query_id)
        .collect();
    sources.sort_by(|left, right| left.range.start.cmp(&right.range.start));
    if sources.is_empty() {
        return None;
    }

    let mut parts = Vec::new();
    for source in sources {
        let mut label = match source.source_kind {
            SqlSourceKind::From => source.name.to_string(),
            SqlSourceKind::Join => format!(
                "{} {}",
                source.join_kind.as_deref().unwrap_or("JOIN"),
                source.name
            ),
        };
        if let Some(alias) = source.alias.as_deref()
            && alias != source.name.as_ref()
        {
            label.push_str(" AS ");
            label.push_str(alias);
        }
        parts.push(label);
    }
    Some(parts.join(" "))
}

fn sql_query_target_clause(target: &SqlTargetData, source_text: Option<&str>) -> Option<String> {
    if let Some(target_name) = target.target_name.as_deref() {
        let mut label = match target.kind {
            SqlTargetKind::Into => "INTO ".to_string(),
            SqlTargetKind::Appending => "APPENDING ".to_string(),
        };
        if target.is_corresponding {
            label.push_str("CORRESPONDING FIELDS OF ");
        }
        if target.is_table {
            label.push_str("TABLE ");
        }
        label.push_str(target_name);
        return Some(label);
    }

    let label = snippet_without_comments(source_text, &target.range);
    (!label.is_empty()).then_some(label)
}

fn sql_query_non_target_clause_lines(
    query: &SqlQueryData,
    source_text: Option<&str>,
) -> Vec<String> {
    let mut clauses = Vec::new();
    for range in [
        query.for_all_entries_clause.as_ref(),
        query.where_clause.as_ref(),
        query.group_by_clause.as_ref(),
        query.having_clause.as_ref(),
        query.order_by_clause.as_ref(),
        query.up_to_clause.as_ref(),
    ]
    .into_iter()
    .flatten()
    {
        let clause = snippet_without_comments(source_text, range);
        if !clause.is_empty() {
            clauses.push(clause);
        }
    }
    clauses
}

fn sql_query_host_inputs(unit: &UnitAnalysis, query: &SqlQueryData) -> Vec<String> {
    let mut inputs = Vec::new();
    for predicate in unit
        .sql_predicates
        .iter()
        .filter(|predicate| predicate.query_id == query.id)
    {
        for access in value_accesses_in_range(unit, &predicate.range) {
            inputs.push(access.display);
        }
    }
    inputs.sort();
    inputs.dedup();
    inputs
}

fn snippet(text: Option<&str>, range: &TextRange) -> String {
    let Some(text) = text else {
        return format!("{}..{}", range.start, range.end);
    };
    let Some(slice) = text.get(range.clone()) else {
        return format!("{}..{}", range.start, range.end);
    };
    normalize_whitespace(slice)
}

fn projection_clause_text(text: Option<&str>, range: &TextRange) -> String {
    let lines = snippet_without_comments_lines(text, range);
    match lines.len() {
        0 => String::new(),
        1 => lines[0].clone(),
        _ => lines.join(", "),
    }
}

fn snippet_without_comments(text: Option<&str>, range: &TextRange) -> String {
    normalize_whitespace(&snippet_without_comments_lines(text, range).join(" "))
}

fn snippet_without_comments_lines(text: Option<&str>, range: &TextRange) -> Vec<String> {
    let Some(text) = text else {
        return vec![format!("{}..{}", range.start, range.end)];
    };
    let Some(slice) = text.get(range.clone()) else {
        return vec![format!("{}..{}", range.start, range.end)];
    };
    let mut parts = Vec::new();
    for raw_line in slice.lines() {
        if raw_line.starts_with('*') {
            continue;
        }
        let without_comment = raw_line.split('"').next().unwrap_or_default().trim();
        if !without_comment.is_empty() {
            parts.push(normalize_whitespace(without_comment));
        }
    }
    parts
}

fn normalize_whitespace(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn loop_header_statement_text(text: Option<&str>, region: &abap_symbols::LoopRegionData) -> String {
    let snippet = snippet_without_comments(text, &region.range);
    let header = snippet
        .split_once('.')
        .map(|(head, _)| head.trim())
        .unwrap_or_else(|| snippet.trim());
    if header.is_empty() {
        format!("LOOP @ {}..{}", region.range.start, region.range.end)
    } else if header.ends_with('.') {
        header.to_string()
    } else {
        format!("{header}.")
    }
}

fn call_dataflow_provenance_is_empty(provenance: &CallDataflowProvenanceGraph) -> bool {
    provenance.nodes.is_empty() || provenance.edges.is_empty()
}

fn format_access_display(
    base_name: &Arc<str>,
    field_path: &[abap_symbols::FieldAccessSegment],
) -> String {
    let mut out = base_name.to_string();
    for segment in field_path {
        if segment.is_deref() {
            out.push_str("->*");
        } else {
            out.push('-');
            out.push_str(segment.name.as_ref());
        }
    }
    out
}

fn format_access_display_from_path(base_name: &str, field_path: &[String]) -> String {
    let mut out = base_name.to_string();
    for segment in field_path {
        out.push('-');
        out.push_str(segment);
    }
    out
}

fn sort_and_dedup_mappings(mappings: &mut Vec<CallDataflowFieldMapping>) {
    mappings.sort_by(|left, right| {
        left.target_path
            .cmp(&right.target_path)
            .then(left.source_kind.cmp(&right.source_kind))
            .then(left.source_display.cmp(&right.source_display))
            .then(left.source_unit_uri.cmp(&right.source_unit_uri))
            .then(
                left.source_range
                    .as_ref()
                    .map(|range| (range.start, range.end))
                    .cmp(
                        &right
                            .source_range
                            .as_ref()
                            .map(|range| (range.start, range.end)),
                    ),
            )
    });
    mappings.dedup_by(|left, right| {
        left.target_path == right.target_path
            && left.source_kind == right.source_kind
            && left.source_display == right.source_display
            && left.source_unit_uri == right.source_unit_uri
            && left.source_range == right.source_range
    });
}

fn named_argument_section_name(section: NamedArgumentSection) -> &'static str {
    match section {
        NamedArgumentSection::Exporting => "exporting",
        NamedArgumentSection::Importing => "importing",
        NamedArgumentSection::Changing => "changing",
        NamedArgumentSection::Tables => "tables",
        NamedArgumentSection::Receiving => "receiving",
        NamedArgumentSection::Exceptions => "exceptions",
    }
}

fn classify_terminal_symbol(
    unit: &UnitAnalysis,
    symbol: &abap_symbols::SymbolData,
) -> &'static str {
    if symbol.scope == unit.root_scope {
        "global_state"
    } else if symbol.kind == SymbolKind::Parameter {
        "routine_input"
    } else {
        "symbol"
    }
}

fn perform_call_target(
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    perform_call: &PerformCallData,
) -> Option<SymbolHandle> {
    project.resolve_perform_call_target(unit, perform_call)
}

fn perform_parameter_index(
    perform_call: &PerformCallData,
    argument: &abap_symbols::PerformArgumentData,
) -> Option<usize> {
    let mut count = 0usize;
    for (idx, section) in perform_call.parameters.iter().copied().enumerate() {
        if section == argument.section {
            if count == argument.ordinal_in_section {
                return Some(idx);
            }
            count += 1;
        }
    }
    None
}

fn perform_argument_ordinal(
    _perform_call: &PerformCallData,
    argument: &abap_symbols::PerformArgumentData,
) -> usize {
    argument.ordinal_in_section
}

fn perform_section_for_form_parameter(
    section: abap_symbols::FormParameterSection,
) -> abap_symbols::PerformParameterSection {
    match section {
        abap_symbols::FormParameterSection::Tables => abap_symbols::PerformParameterSection::Tables,
        abap_symbols::FormParameterSection::Using => abap_symbols::PerformParameterSection::Using,
        abap_symbols::FormParameterSection::Changing => {
            abap_symbols::PerformParameterSection::Changing
        }
    }
}

fn form_parameter_ordinal(
    unit: &UnitAnalysis,
    owner: SymbolHandle,
    parameter: &abap_symbols::FormParameterData,
) -> usize {
    unit.form_routine(owner.symbol)
        .map(|form| {
            form.parameters
                .iter()
                .filter(|candidate| candidate.section == parameter.section)
                .position(|candidate| candidate.symbol == parameter.symbol)
                .unwrap_or(0)
        })
        .unwrap_or(0)
}

fn screen_module_name(name: &str) -> Option<(String, String)> {
    if let Some(screen) = name.strip_prefix("status_") {
        return Some((screen.to_string(), "output".to_string()));
    }
    if let Some(screen) = name.strip_prefix("user_command_") {
        return Some((screen.to_string(), "input".to_string()));
    }
    None
}

struct CallScreenSite {
    scope: ScopeId,
    screen_number: String,
    phase: String,
    range: TextRange,
}

fn call_screen_sites(unit: &UnitAnalysis, source: &str) -> Vec<CallScreenSite> {
    let mut out = Vec::new();
    let lower = source.to_ascii_lowercase();
    let needle = "call screen";
    let mut search_start = 0usize;
    while let Some(relative) = lower[search_start..].find(needle) {
        let start = search_start + relative;
        let mut idx = start + needle.len();
        while idx < lower.len() && lower.as_bytes()[idx].is_ascii_whitespace() {
            idx += 1;
        }
        let screen_start = idx;
        while idx < lower.len() && lower.as_bytes()[idx].is_ascii_digit() {
            idx += 1;
        }
        if idx == screen_start {
            search_start = start + needle.len();
            continue;
        }
        let screen_number = source[screen_start..idx].to_string();
        let end = lower[idx..]
            .find('.')
            .map(|period| idx + period + 1)
            .unwrap_or(idx);
        let scope = unit
            .scopes
            .iter()
            .filter(|scope| scope.range.start <= start && scope.range.end >= end)
            .min_by_key(|scope| scope.range.end - scope.range.start)
            .map(|scope| scope.id)
            .unwrap_or(unit.root_scope);
        out.push(CallScreenSite {
            scope,
            screen_number,
            phase: "input".to_string(),
            range: start..end,
        });
        search_start = idx;
    }
    out
}

trait UnitAnalysisExt {
    fn routine_parameters_owner(
        &self,
        parameter_symbol: abap_symbols::SymbolId,
    ) -> Option<SymbolHandle>;
}

impl UnitAnalysisExt for UnitAnalysis {
    fn routine_parameters_owner(
        &self,
        parameter_symbol: abap_symbols::SymbolId,
    ) -> Option<SymbolHandle> {
        self.form_routines.iter().find_map(|routine| {
            routine
                .parameters
                .iter()
                .any(|parameter| parameter.symbol == parameter_symbol)
                .then_some(SymbolHandle {
                    unit: self.unit_id,
                    symbol: routine.symbol,
                })
        })
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use abap_symbols::{
        Namespace, ReferenceData, ReferenceId, Resolution, SymbolHandle, SymbolId, UnitId,
    };

    use crate::{CallDataflowQuery, DocumentInput, DocumentStore, build_call_dataflow_trace};

    use super::{field_access_to_value_access, resolve_access_handle};

    fn snapshot_for(inputs: Vec<DocumentInput>, target_uri: &str) -> Arc<crate::AnalysisSnapshot> {
        let store = DocumentStore::default();
        let snapshots = store.replace_all(inputs);
        snapshots.get(target_uri).expect("target snapshot").clone()
    }

    #[test]
    fn traces_structure_and_table_mappings_with_synthetic_screen_edges() {
        let snapshot = snapshot_for(
            vec![
                DocumentInput {
                    uri: Arc::from("file:///main.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
REPORT zmain.
PARAMETERS p_vendor TYPE string.
INCLUDE zhelpers.
INCLUDE zpai.

END-OF-SELECTION.
  CALL SCREEN 9000.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///zhelpers.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
TYPES: BEGIN OF ty_header,
         doc_type TYPE string,
         vendor TYPE string,
       END OF ty_header.
TYPES: BEGIN OF ty_item,
         matnr TYPE string,
         qty TYPE i,
       END OF ty_item.
DATA gs_header TYPE ty_header.
DATA gt_items TYPE STANDARD TABLE OF ty_item WITH EMPTY KEY.

FORM build_header CHANGING cs_header TYPE ty_header.
  DATA lv_doc TYPE string.
  lv_doc = 'NB'.
  cs_header-doc_type = lv_doc.
  cs_header-vendor = p_vendor.
ENDFORM.

FORM build_items CHANGING ct_items TYPE STANDARD TABLE OF ty_item WITH EMPTY KEY.
  DATA ls_item TYPE ty_item.
  ls_item-matnr = 'MAT1'.
  ls_item-qty = 1.
  APPEND ls_item TO ct_items.
ENDFORM.

FORM call_api USING us_header TYPE ty_header
              CHANGING ct_items TYPE STANDARD TABLE OF ty_item WITH EMPTY KEY.
  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader = us_header
    TABLES
      poitem = ct_items.
ENDFORM.

FORM create_sto.
  PERFORM build_header CHANGING gs_header.
  PERFORM build_items CHANGING gt_items.
  PERFORM call_api USING gs_header
                   CHANGING gt_items.
ENDFORM.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///zpai.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
MODULE user_command_9000 INPUT.
  PERFORM create_sto.
ENDMODULE.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///bapi.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
FUNCTION bapi_po_create1
  IMPORTING
    poheader TYPE string
  TABLES
    poitem TYPE i.
ENDFUNCTION.",
                    ),
                    is_dependency: true,
                    object_name: None,
                },
            ],
            "file:///main.abap",
        );

        let trace = build_call_dataflow_trace(
            snapshot.as_ref(),
            CallDataflowQuery {
                target: "BAPI_PO_CREATE1".to_string(),
                caller: Some("call_api".to_string()),
                occurrence: None,
            },
        );

        let selected = trace.selected_call.expect("selected call");
        assert_eq!(selected.caller_name.as_deref(), Some("call_api"));
        assert!(
            trace
                .lifecycle
                .edges
                .iter()
                .any(|edge| edge.synthetic && edge.kind == "screen_dispatch"),
            "{:?}",
            trace.lifecycle.edges
        );
        let poheader = trace
            .parameter_traces
            .iter()
            .find(|trace| trace.parameter_name.as_deref() == Some("poheader"))
            .expect("poheader trace");
        assert!(
            poheader.field_mappings.iter().any(|mapping| {
                mapping.target_path == "poheader.doc_type"
                    && mapping.source_display.contains("lv_doc")
                    && mapping.source_kind == "assignment"
            }),
            "{:?}",
            poheader.field_mappings
        );
        assert!(
            poheader.field_mappings.iter().any(|mapping| {
                mapping.target_path == "poheader.vendor"
                    && mapping.source_display.contains("p_vendor")
            }),
            "{:?}",
            poheader.field_mappings
        );

        let poitem = trace
            .parameter_traces
            .iter()
            .find(|trace| trace.parameter_name.as_deref() == Some("poitem"))
            .expect("poitem trace");
        assert!(
            poitem.field_mappings.iter().any(|mapping| {
                mapping.target_path == "poitem[*].matnr" && mapping.source_display.contains("MAT1")
            }),
            "{:?}",
            poitem.field_mappings
        );
        assert!(
            poitem.field_mappings.iter().any(|mapping| {
                mapping.target_path == "poitem[*].qty" && mapping.source_display.contains("1")
            }),
            "{:?}",
            poitem.field_mappings
        );
    }

    #[test]
    fn resolves_field_access_handles_by_base_name_when_ranges_collide() {
        let snapshot = snapshot_for(
            vec![DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(
                    "\
REPORT z_resolve_field_access.

TYPES: BEGIN OF ty_row,
         matnr TYPE string,
       END OF ty_row.
DATA ls_source TYPE ty_row.
DATA ls_other TYPE ty_row.

FORM foo.
  DATA lv_text TYPE string.
  lv_text = ls_source-matnr.
ENDFORM.",
                ),
                is_dependency: false,
                object_name: None,
            }],
            "file:///main.abap",
        );

        let mut unit = snapshot.symbols.as_ref().clone();
        let access = unit
            .field_accesses
            .iter()
            .find(|access| {
                access.base_namespace == Namespace::Value
                    && access.base_name.as_ref() == "ls_source"
                    && access.field_path.len() == 1
                    && access.field_path[0].name.as_ref() == "matnr"
            })
            .expect("field access")
            .clone();
        let correct_reference = unit
            .references
            .iter()
            .find(|reference| {
                reference.scope == access.scope
                    && reference.namespace == access.base_namespace
                    && reference.range == access.base_range
                    && reference.name.as_ref() == "ls_source"
            })
            .expect("source reference")
            .clone();
        let wrong_symbol = unit
            .symbols
            .iter()
            .position(|symbol| symbol.name.as_ref() == "ls_other")
            .expect("other symbol");
        unit.references.insert(
            0,
            ReferenceData {
                id: ReferenceId(u32::MAX),
                name: Arc::from("ls_other"),
                namespace: Namespace::Value,
                kind: correct_reference.kind,
                scope: access.scope,
                range: access.base_range.clone(),
                resolution: Some(Resolution::Symbol(SymbolHandle {
                    unit: unit.unit_id,
                    symbol: SymbolId(wrong_symbol as u32),
                })),
            },
        );

        let handle = resolve_access_handle(&unit, &access).expect("resolved handle");

        assert_eq!(unit.symbol(handle.symbol).name.as_ref(), "ls_source");
    }

    #[test]
    fn keeps_field_access_display_from_access_text_for_cross_unit_handles() {
        let snapshot = snapshot_for(
            vec![DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(
                    "\
REPORT z_cross_unit_display.

TYPES: BEGIN OF ty_row,
         matnr TYPE string,
       END OF ty_row.
DATA ls_source TYPE ty_row.
DATA ls_other TYPE ty_row.

FORM foo.
  DATA lv_text TYPE string.
  lv_text = ls_source-matnr.
ENDFORM.",
                ),
                is_dependency: false,
                object_name: None,
            }],
            "file:///main.abap",
        );

        let mut unit = snapshot.symbols.as_ref().clone();
        let access = unit
            .field_accesses
            .iter()
            .find(|access| {
                access.base_namespace == Namespace::Value
                    && access.base_name.as_ref() == "ls_source"
                    && access.field_path.len() == 1
                    && access.field_path[0].name.as_ref() == "matnr"
            })
            .expect("field access")
            .clone();
        let wrong_symbol = unit
            .symbols
            .iter()
            .position(|symbol| symbol.name.as_ref() == "ls_other")
            .expect("other symbol");
        let source_reference = unit
            .references
            .iter_mut()
            .find(|reference| {
                reference.scope == access.scope
                    && reference.namespace == access.base_namespace
                    && reference.range == access.base_range
                    && reference.name.as_ref() == "ls_source"
            })
            .expect("source reference");
        source_reference.resolution = Some(Resolution::Symbol(SymbolHandle {
            unit: UnitId(u32::MAX),
            symbol: SymbolId(wrong_symbol as u32),
        }));

        let value_access = field_access_to_value_access(&unit, &access, &[]).expect("value access");

        assert_eq!(value_access.display, "ls_source-matnr");
        assert_eq!(value_access.handle.unit, UnitId(u32::MAX));
    }

    #[test]
    fn traces_global_sql_writers_across_sibling_routines() {
        let snapshot = snapshot_for(
            vec![
                DocumentInput {
                    uri: Arc::from("file:///main.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
REPORT z_global_sql_writers.

TYPES: BEGIN OF ty_state,
         bukrs TYPE string,
       END OF ty_state.
DATA gs_state TYPE ty_state.

FORM load_state.
  SELECT SINGLE bukrs
    FROM t001k
    INTO gs_state
    WHERE bwkey = 'PL01'.
ENDFORM.

FORM call_api.
  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader = gs_state-bukrs.
ENDFORM.

START-OF-SELECTION.
  PERFORM load_state.
  PERFORM call_api.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///bapi.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
FUNCTION bapi_po_create1
  IMPORTING
    poheader TYPE string.
ENDFUNCTION.",
                    ),
                    is_dependency: true,
                    object_name: None,
                },
            ],
            "file:///main.abap",
        );

        let trace = build_call_dataflow_trace(
            snapshot.as_ref(),
            CallDataflowQuery {
                target: "BAPI_PO_CREATE1".to_string(),
                caller: Some("call_api".to_string()),
                occurrence: None,
            },
        );

        let poheader = trace
            .parameter_traces
            .iter()
            .find(|trace| trace.parameter_name.as_deref() == Some("poheader"))
            .expect("poheader trace");
        assert!(
            poheader.field_mappings.iter().any(|mapping| {
                mapping.source_kind == "sql_target" && mapping.source_display == "gs_state-bukrs"
            }),
            "{:?}",
            poheader.field_mappings
        );
        assert!(
            poheader
                .provenance
                .nodes
                .iter()
                .any(|node| node.kind == "sql_query" && node.label.contains("FROM t001k")),
            "{:?}",
            poheader.provenance.nodes
        );
    }

    #[test]
    fn traces_sql_query_sources_in_parameter_provenance() {
        let snapshot = snapshot_for(
            vec![
                DocumentInput {
                    uri: Arc::from("file:///main.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
REPORT z_sql_trace.

TYPES: BEGIN OF ty_header,
         matnr TYPE string,
       END OF ty_header.
DATA gs_header TYPE ty_header.

FORM build_header CHANGING cs_header TYPE ty_header.
  DATA lv_matnr TYPE string.
  SELECT SINGLE matnr
    FROM mara
    INTO @lv_matnr.
  cs_header-matnr = lv_matnr.
ENDFORM.

FORM call_api USING us_header TYPE ty_header.
  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader = us_header.
ENDFORM.

START-OF-SELECTION.
  PERFORM build_header CHANGING gs_header.
  PERFORM call_api USING gs_header.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///bapi.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
FUNCTION bapi_po_create1
  IMPORTING
    poheader TYPE string.
ENDFUNCTION.",
                    ),
                    is_dependency: true,
                    object_name: None,
                },
            ],
            "file:///main.abap",
        );

        let trace = build_call_dataflow_trace(
            snapshot.as_ref(),
            CallDataflowQuery {
                target: "BAPI_PO_CREATE1".to_string(),
                caller: Some("call_api".to_string()),
                occurrence: None,
            },
        );

        let poheader = trace
            .parameter_traces
            .iter()
            .find(|trace| trace.parameter_name.as_deref() == Some("poheader"))
            .expect("poheader trace");
        assert!(
            poheader
                .provenance
                .nodes
                .iter()
                .any(|node| node.kind == "sql_query" && node.label.contains("FROM mara")),
            "{:?}",
            poheader.provenance.nodes
        );
        assert!(
            poheader
                .provenance
                .nodes
                .iter()
                .any(|node| node.kind == "sql_source" && node.label.contains("mara")),
            "{:?}",
            poheader.provenance.nodes
        );
        assert!(
            poheader
                .provenance
                .edges
                .iter()
                .any(|edge| edge.kind == "selects_into"),
            "{:?}",
            poheader.provenance.edges
        );
    }

    #[test]
    fn traces_sql_predicate_inputs_in_parameter_provenance() {
        let snapshot = snapshot_for(
            vec![
                DocumentInput {
                    uri: Arc::from("file:///main.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
REPORT z_sql_predicate_trace.
PARAMETERS p_matnr TYPE string.

TYPES: BEGIN OF ty_header,
         matnr TYPE string,
       END OF ty_header.
DATA gs_header TYPE ty_header.

FORM build_header CHANGING cs_header TYPE ty_header.
  DATA lv_matnr TYPE string.
  SELECT SINGLE matnr
    FROM mara
    INTO @lv_matnr
    WHERE matnr = @p_matnr.
  cs_header-matnr = lv_matnr.
ENDFORM.

FORM call_api USING us_header TYPE ty_header.
  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader = us_header.
ENDFORM.

START-OF-SELECTION.
  PERFORM build_header CHANGING gs_header.
  PERFORM call_api USING gs_header.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///bapi.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
FUNCTION bapi_po_create1
  IMPORTING
    poheader TYPE string.
ENDFUNCTION.",
                    ),
                    is_dependency: true,
                    object_name: None,
                },
            ],
            "file:///main.abap",
        );

        let trace = build_call_dataflow_trace(
            snapshot.as_ref(),
            CallDataflowQuery {
                target: "BAPI_PO_CREATE1".to_string(),
                caller: Some("call_api".to_string()),
                occurrence: None,
            },
        );

        let poheader = trace
            .parameter_traces
            .iter()
            .find(|trace| trace.parameter_name.as_deref() == Some("poheader"))
            .expect("poheader trace");
        assert!(
            poheader.provenance.nodes.iter().any(|node| {
                node.kind == "sql_query"
                    && node.label.contains("WHERE matnr = @p_matnr")
                    && node.label.contains("HOSTS p_matnr")
            }),
            "{:?}",
            poheader.provenance.nodes
        );
        assert!(
            poheader
                .provenance
                .nodes
                .iter()
                .any(|node| node.kind == "sql_predicate" && node.label.contains("WHERE")),
            "{:?}",
            poheader.provenance.nodes
        );
        assert!(
            poheader
                .provenance
                .edges
                .iter()
                .any(|edge| edge.kind == "filters"),
            "{:?}",
            poheader.provenance.edges
        );
        assert!(
            poheader
                .provenance
                .edges
                .iter()
                .any(|edge| edge.kind == "uses"),
            "{:?}",
            poheader.provenance.edges
        );
    }

    #[test]
    fn summarizes_sql_queries_from_structured_metadata_without_comments() {
        let snapshot = snapshot_for(
            vec![
                DocumentInput {
                    uri: Arc::from("file:///main.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
REPORT z_sql_comment_trace.
PARAMETERS p_matnr TYPE string.

TYPES: BEGIN OF ty_header,
         matnr TYPE string,
       END OF ty_header.
TYPES: BEGIN OF ty_mara,
         matnr TYPE string,
         meins TYPE string,
       END OF ty_mara.
DATA gs_header TYPE ty_header.

FORM build_header CHANGING cs_header TYPE ty_header.
  DATA ls_mara TYPE ty_mara.
  SELECT SINGLE matnr      \" material
                meins      \" base unit
    FROM mara              \" material master
    INTO @ls_mara
    WHERE matnr = @p_matnr. \" host filter
  cs_header-matnr = ls_mara-matnr.
ENDFORM.

FORM call_api USING us_header TYPE ty_header.
  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader = us_header.
ENDFORM.

START-OF-SELECTION.
  PERFORM build_header CHANGING gs_header.
  PERFORM call_api USING gs_header.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///bapi.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
FUNCTION bapi_po_create1
  IMPORTING
    poheader TYPE string.
ENDFUNCTION.",
                    ),
                    is_dependency: true,
                    object_name: None,
                },
            ],
            "file:///main.abap",
        );

        let trace = build_call_dataflow_trace(
            snapshot.as_ref(),
            CallDataflowQuery {
                target: "BAPI_PO_CREATE1".to_string(),
                caller: Some("call_api".to_string()),
                occurrence: None,
            },
        );

        let poheader = trace
            .parameter_traces
            .iter()
            .find(|trace| trace.parameter_name.as_deref() == Some("poheader"))
            .expect("poheader trace");
        let sql_query = poheader
            .provenance
            .nodes
            .iter()
            .find(|node| node.kind == "sql_query")
            .expect("sql query node");

        assert!(
            sql_query.label.contains("SELECT SINGLE matnr, meins"),
            "{}",
            sql_query.label
        );
        assert!(sql_query.label.contains("FROM mara"), "{}", sql_query.label);
        assert!(
            sql_query.label.contains("INTO ls_mara"),
            "{}",
            sql_query.label
        );
        assert!(
            sql_query.label.contains("WHERE matnr = @p_matnr"),
            "{}",
            sql_query.label
        );
        assert!(!sql_query.label.contains("material"), "{}", sql_query.label);
        assert!(
            !sql_query.label.contains("base unit"),
            "{}",
            sql_query.label
        );
        assert!(!sql_query.label.contains("master"), "{}", sql_query.label);
    }

    #[test]
    fn traces_field_level_sql_projection_flow_for_scalar_targets() {
        let snapshot = snapshot_for(
            vec![
                DocumentInput {
                    uri: Arc::from("file:///main.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
REPORT z_sql_projection_flow.

TYPES: BEGIN OF ty_header,
         matnr TYPE string,
       END OF ty_header.
DATA gs_header TYPE ty_header.

FORM build_header CHANGING cs_header TYPE ty_header.
  DATA lv_matnr TYPE string.
  SELECT SINGLE matnr
    FROM mara
    INTO @lv_matnr.
  cs_header-matnr = lv_matnr.
ENDFORM.

FORM call_api USING us_header TYPE ty_header.
  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader = us_header.
ENDFORM.

START-OF-SELECTION.
  PERFORM build_header CHANGING gs_header.
  PERFORM call_api USING gs_header.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///bapi.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
FUNCTION bapi_po_create1
  IMPORTING
    poheader TYPE string.
ENDFUNCTION.",
                    ),
                    is_dependency: true,
                    object_name: None,
                },
            ],
            "file:///main.abap",
        );

        let trace = build_call_dataflow_trace(
            snapshot.as_ref(),
            CallDataflowQuery {
                target: "BAPI_PO_CREATE1".to_string(),
                caller: Some("call_api".to_string()),
                occurrence: None,
            },
        );

        let poheader = trace
            .parameter_traces
            .iter()
            .find(|trace| trace.parameter_name.as_deref() == Some("poheader"))
            .expect("poheader trace");
        assert!(
            poheader
                .provenance
                .nodes
                .iter()
                .any(|node| node.kind == "sql_source_field" && node.label == "mara.matnr"),
            "{:?}",
            poheader.provenance.nodes
        );
        assert!(
            poheader
                .provenance
                .nodes
                .iter()
                .any(|node| node.kind == "sql_target_field" && node.label == "lv_matnr"),
            "{:?}",
            poheader.provenance.nodes
        );
        assert!(
            poheader
                .provenance
                .edges
                .iter()
                .any(|edge| edge.kind == "projects"),
            "{:?}",
            poheader.provenance.edges
        );
        assert!(
            poheader
                .provenance
                .edges
                .iter()
                .any(|edge| edge.kind == "selects_into"),
            "{:?}",
            poheader.provenance.edges
        );
    }

    #[test]
    fn traces_precise_sql_source_field_range_for_multi_column_select() {
        let snapshot = snapshot_for(
            vec![
                DocumentInput {
                    uri: Arc::from("file:///main.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
REPORT z_sql_projection_range.

TYPES: BEGIN OF ty_header,
         bsart TYPE string,
       END OF ty_header.
TYPES: BEGIN OF ty_sql,
         reswk TYPE string,
         werks TYPE string,
         bsart TYPE string,
       END OF ty_sql.
DATA gs_header TYPE ty_header.

FORM build_header CHANGING cs_header TYPE ty_header.
  DATA ls_sql TYPE ty_sql.
  SELECT SINGLE reswk
                werks
                bsart
    FROM t161w
    INTO @ls_sql.
  cs_header-bsart = ls_sql-bsart.
ENDFORM.

FORM call_api USING us_header TYPE ty_header.
  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader = us_header.
ENDFORM.

START-OF-SELECTION.
  PERFORM build_header CHANGING gs_header.
  PERFORM call_api USING gs_header.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///bapi.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
FUNCTION bapi_po_create1
  IMPORTING
    poheader TYPE string.
ENDFUNCTION.",
                    ),
                    is_dependency: true,
                    object_name: None,
                },
            ],
            "file:///main.abap",
        );

        let trace = build_call_dataflow_trace(
            snapshot.as_ref(),
            CallDataflowQuery {
                target: "BAPI_PO_CREATE1".to_string(),
                caller: Some("call_api".to_string()),
                occurrence: None,
            },
        );

        let poheader = trace
            .parameter_traces
            .iter()
            .find(|trace| trace.parameter_name.as_deref() == Some("poheader"))
            .expect("poheader trace");
        let sql_field = poheader
            .provenance
            .nodes
            .iter()
            .find(|node| node.kind == "sql_source_field" && node.label == "t161w.bsart")
            .expect("sql source field");
        let range = sql_field.range.as_ref().expect("range");
        let source_text = snapshot
            .project_text("file:///main.abap")
            .expect("main source text");
        let exact = source_text
            .get(range.start..range.end)
            .expect("slice")
            .trim();

        assert_eq!(exact, "bsart");
    }

    #[test]
    fn traces_sql_backed_structure_field_reads() {
        let snapshot = snapshot_for(
            vec![
                DocumentInput {
                    uri: Arc::from("file:///main.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
REPORT z_sql_field_trace.

TYPES: BEGIN OF ty_header,
         matnr TYPE string,
       END OF ty_header.
TYPES: BEGIN OF ty_mara,
         matnr TYPE string,
       END OF ty_mara.
DATA gs_header TYPE ty_header.

FORM build_header CHANGING cs_header TYPE ty_header.
  DATA ls_mara TYPE ty_mara.
  SELECT SINGLE *
    FROM mara
    INTO @ls_mara.
  cs_header-matnr = ls_mara-matnr.
ENDFORM.

FORM call_api USING us_header TYPE ty_header.
  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader = us_header.
ENDFORM.

START-OF-SELECTION.
  PERFORM build_header CHANGING gs_header.
  PERFORM call_api USING gs_header.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///bapi.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
FUNCTION bapi_po_create1
  IMPORTING
    poheader TYPE string.
ENDFUNCTION.",
                    ),
                    is_dependency: true,
                    object_name: None,
                },
            ],
            "file:///main.abap",
        );

        let trace = build_call_dataflow_trace(
            snapshot.as_ref(),
            CallDataflowQuery {
                target: "BAPI_PO_CREATE1".to_string(),
                caller: Some("call_api".to_string()),
                occurrence: None,
            },
        );

        let poheader = trace
            .parameter_traces
            .iter()
            .find(|trace| trace.parameter_name.as_deref() == Some("poheader"))
            .expect("poheader trace");
        assert!(
            poheader.field_mappings.iter().any(|mapping| {
                mapping.target_path == "poheader.matnr"
                    && mapping.source_kind == "sql_target"
                    && mapping.source_display.contains("ls_mara-matnr")
            }),
            "{:?}",
            poheader.field_mappings
        );
        assert!(
            poheader
                .provenance
                .nodes
                .iter()
                .any(|node| node.kind == "sql_source" && node.label.contains("mara")),
            "{:?}",
            poheader.provenance.nodes
        );
    }

    #[test]
    fn traces_read_table_binding_back_to_sql_table_source() {
        let snapshot = snapshot_for(
            vec![
                DocumentInput {
                    uri: Arc::from("file:///main.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
REPORT z_sql_read_table.

TYPES: BEGIN OF ty_header,
         matnr TYPE string,
       END OF ty_header.
TYPES: BEGIN OF ty_mara,
         matnr TYPE string,
       END OF ty_mara.
DATA gs_header TYPE ty_header.
DATA gt_mara TYPE STANDARD TABLE OF ty_mara WITH EMPTY KEY.

FORM build_header CHANGING cs_header TYPE ty_header.
  DATA ls_mara TYPE ty_mara.
  SELECT matnr
    FROM mara
    INTO TABLE @gt_mara.
  READ TABLE gt_mara INTO ls_mara INDEX 1.
  cs_header-matnr = ls_mara-matnr.
ENDFORM.

FORM call_api USING us_header TYPE ty_header.
  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader = us_header.
ENDFORM.

START-OF-SELECTION.
  PERFORM build_header CHANGING gs_header.
  PERFORM call_api USING gs_header.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///bapi.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
FUNCTION bapi_po_create1
  IMPORTING
    poheader TYPE string.
ENDFUNCTION.",
                    ),
                    is_dependency: true,
                    object_name: None,
                },
            ],
            "file:///main.abap",
        );

        let trace = build_call_dataflow_trace(
            snapshot.as_ref(),
            CallDataflowQuery {
                target: "BAPI_PO_CREATE1".to_string(),
                caller: Some("call_api".to_string()),
                occurrence: None,
            },
        );

        let poheader = trace
            .parameter_traces
            .iter()
            .find(|trace| trace.parameter_name.as_deref() == Some("poheader"))
            .expect("poheader trace");
        assert!(
            poheader.field_mappings.iter().any(|mapping| {
                mapping.target_path == "poheader.matnr"
                    && mapping.source_kind == "read_table_binding"
                    && mapping.source_display.contains("gt_mara")
            }),
            "{:?}",
            poheader.field_mappings
        );
        assert!(
            poheader
                .provenance
                .nodes
                .iter()
                .any(|node| node.kind == "sql_query" && node.label.contains("FROM mara")),
            "{:?}",
            poheader.provenance.nodes
        );
    }

    #[test]
    fn traces_field_symbol_binding_back_to_sql_table_source() {
        let snapshot = snapshot_for(
            vec![
                DocumentInput {
                    uri: Arc::from("file:///main.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
REPORT z_sql_read_table_fs.

TYPES: BEGIN OF ty_header,
         matnr TYPE string,
       END OF ty_header.
TYPES: BEGIN OF ty_mara,
         matnr TYPE string,
       END OF ty_mara.
DATA gs_header TYPE ty_header.
DATA gt_mara TYPE STANDARD TABLE OF ty_mara WITH EMPTY KEY.

FORM build_header CHANGING cs_header TYPE ty_header.
  SELECT matnr
    FROM mara
    INTO TABLE @gt_mara.
  READ TABLE gt_mara ASSIGNING FIELD-SYMBOL(<ls_mara>) INDEX 1.
  cs_header-matnr = <ls_mara>-matnr.
ENDFORM.

FORM call_api USING us_header TYPE ty_header.
  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader = us_header.
ENDFORM.

START-OF-SELECTION.
  PERFORM build_header CHANGING gs_header.
  PERFORM call_api USING gs_header.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///bapi.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
FUNCTION bapi_po_create1
  IMPORTING
    poheader TYPE string.
ENDFUNCTION.",
                    ),
                    is_dependency: true,
                    object_name: None,
                },
            ],
            "file:///main.abap",
        );

        let trace = build_call_dataflow_trace(
            snapshot.as_ref(),
            CallDataflowQuery {
                target: "BAPI_PO_CREATE1".to_string(),
                caller: Some("call_api".to_string()),
                occurrence: None,
            },
        );

        let poheader = trace
            .parameter_traces
            .iter()
            .find(|trace| trace.parameter_name.as_deref() == Some("poheader"))
            .expect("poheader trace");
        assert!(
            poheader.field_mappings.iter().any(|mapping| {
                mapping.target_path == "poheader.matnr"
                    && mapping.source_kind == "field_symbol_binding"
                    && mapping.source_display.contains("gt_mara")
            }),
            "{:?}",
            poheader.field_mappings
        );
        assert!(
            poheader
                .provenance
                .nodes
                .iter()
                .any(|node| node.kind == "sql_source" && node.label.contains("mara")),
            "{:?}",
            poheader.provenance.nodes
        );
    }

    #[test]
    fn traces_loop_bound_table_rows_back_to_sql_table_source() {
        let snapshot = snapshot_for(
            vec![
                DocumentInput {
                    uri: Arc::from("file:///main.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
REPORT z_sql_loop_trace.

TYPES: BEGIN OF ty_item,
         matnr TYPE string,
       END OF ty_item.
DATA gt_items TYPE STANDARD TABLE OF ty_item WITH EMPTY KEY.
DATA gt_filtered TYPE STANDARD TABLE OF ty_item WITH EMPTY KEY.

FORM load_items CHANGING ct_items TYPE STANDARD TABLE OF ty_item.
  SELECT matnr
    FROM mara
    INTO TABLE @ct_items.
ENDFORM.

FORM filter_items USING ut_items TYPE STANDARD TABLE OF ty_item
                  CHANGING ct_filtered TYPE STANDARD TABLE OF ty_item.
  LOOP AT ut_items ASSIGNING FIELD-SYMBOL(<ls_item>).
    APPEND <ls_item> TO ct_filtered.
  ENDLOOP.
ENDFORM.

FORM call_api USING ut_items TYPE STANDARD TABLE OF ty_item.
  LOOP AT ut_items ASSIGNING FIELD-SYMBOL(<ls_item>).
    CALL FUNCTION 'BAPI_PO_CREATE1'
      EXPORTING
        poheader = <ls_item>-matnr.
  ENDLOOP.
ENDFORM.

START-OF-SELECTION.
  PERFORM load_items CHANGING gt_items.
  PERFORM filter_items USING gt_items CHANGING gt_filtered.
  PERFORM call_api USING gt_filtered.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///bapi.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
FUNCTION bapi_po_create1
  IMPORTING
    poheader TYPE string.
ENDFUNCTION.",
                    ),
                    is_dependency: true,
                    object_name: None,
                },
            ],
            "file:///main.abap",
        );

        let trace = build_call_dataflow_trace(
            snapshot.as_ref(),
            CallDataflowQuery {
                target: "BAPI_PO_CREATE1".to_string(),
                caller: Some("call_api".to_string()),
                occurrence: None,
            },
        );

        let poheader = trace
            .parameter_traces
            .iter()
            .find(|trace| trace.parameter_name.as_deref() == Some("poheader"))
            .expect("poheader trace");
        assert!(
            poheader.field_mappings.iter().any(|mapping| {
                mapping.source_kind == "field_symbol_binding"
                    && mapping.source_display.contains("ut_items")
            }),
            "{:?}",
            poheader.field_mappings
        );
        assert!(
            poheader
                .provenance
                .nodes
                .iter()
                .any(|node| node.kind == "sql_query" && node.label.contains("FROM mara")),
            "{:?}",
            poheader.provenance.nodes
        );
        assert!(
            poheader
                .provenance
                .nodes
                .iter()
                .any(|node| node.kind == "sql_source" && node.label.contains("mara")),
            "{:?}",
            poheader.provenance.nodes
        );
    }

    #[test]
    fn summarizes_mutating_perform_writes_and_links_internal_table_writes() {
        let snapshot = snapshot_for(
            vec![
                DocumentInput {
                    uri: Arc::from("file:///main.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
REPORT z_perform_write_summary.

TYPES: BEGIN OF ty_schedule,
         po_item TYPE string,
         delivery_date TYPE string,
         quantity TYPE i,
       END OF ty_schedule.
TYPES ty_schedule_tab TYPE STANDARD TABLE OF ty_schedule WITH DEFAULT KEY.

DATA gt_schedule TYPE ty_schedule_tab.

FORM merge_schedule CHANGING ct_schedule TYPE ty_schedule_tab.
  DATA lt_schedule TYPE ty_schedule_tab.
  DATA ls_schedule TYPE ty_schedule.
  FIELD-SYMBOLS <ls_existing> TYPE ty_schedule.

  ls_schedule-po_item = '10'.
  ls_schedule-delivery_date = '20240101'.
  ls_schedule-quantity = 1.
  APPEND ls_schedule TO ct_schedule.
  APPEND ls_schedule TO ct_schedule.

  lt_schedule = ct_schedule.
  CLEAR ct_schedule.
  SORT lt_schedule BY po_item delivery_date.
  LOOP AT lt_schedule INTO ls_schedule.
    READ TABLE ct_schedule ASSIGNING <ls_existing>
      WITH KEY po_item = ls_schedule-po_item
               delivery_date = ls_schedule-delivery_date.
    IF sy-subrc = 0.
      <ls_existing>-quantity = <ls_existing>-quantity + ls_schedule-quantity.
    ELSE.
      APPEND ls_schedule TO ct_schedule.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM call_api USING ut_schedule TYPE ty_schedule_tab.
  CALL FUNCTION 'TARGET_API'
    EXPORTING
      it_schedule = ut_schedule.
ENDFORM.

START-OF-SELECTION.
  PERFORM merge_schedule CHANGING gt_schedule.
  PERFORM call_api USING gt_schedule.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///target.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
FUNCTION target_api
  IMPORTING
    it_schedule TYPE string.
ENDFUNCTION.",
                    ),
                    is_dependency: true,
                    object_name: None,
                },
            ],
            "file:///main.abap",
        );

        let trace = build_call_dataflow_trace(
            snapshot.as_ref(),
            CallDataflowQuery {
                target: "TARGET_API".to_string(),
                caller: Some("call_api".to_string()),
                occurrence: None,
            },
        );

        let it_schedule = trace
            .parameter_traces
            .iter()
            .find(|trace| trace.parameter_name.as_deref() == Some("it_schedule"))
            .expect("it_schedule trace");
        let perform_write = it_schedule
            .provenance
            .nodes
            .iter()
            .find(|node| {
                node.kind == "perform_write"
                    && node.label.contains("merge_schedule writes ct_schedule")
            })
            .expect("perform write node");
        assert!(
            perform_write
                .label
                .contains("merge by po_item, delivery_date"),
            "{}",
            perform_write.label
        );
        assert!(
            perform_write.label.contains("sum quantity"),
            "{}",
            perform_write.label
        );

        let table_row_node = it_schedule
            .provenance
            .nodes
            .iter()
            .find(|node| node.kind == "target_table_row" && node.label == "it_schedule[*]")
            .expect("target table row node");
        assert!(
            it_schedule.provenance.edges.iter().any(|edge| {
                edge.source == table_row_node.id
                    && edge.target == perform_write.id
                    && edge.kind == "flows_to"
            }),
            "{:?}",
            it_schedule.provenance.edges
        );
    }

    #[test]
    fn traces_loop_into_row_bindings_back_to_original_schedule_assignments() {
        let snapshot = snapshot_for(
            vec![
                DocumentInput {
                    uri: Arc::from("file:///main.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
REPORT z_loop_into_schedule_trace.

TYPES: BEGIN OF ty_schedule,
         po_item TYPE string,
         delivery_date TYPE string,
         quantity TYPE i,
       END OF ty_schedule.
TYPES ty_schedule_tab TYPE STANDARD TABLE OF ty_schedule WITH DEFAULT KEY.

DATA gt_schedule TYPE ty_schedule_tab.

FORM populate_schedule.
  DATA ls_schedule TYPE ty_schedule.
  DATA lv_quantity TYPE i.

  lv_quantity = 5.
  ls_schedule-po_item = '10'.
  ls_schedule-delivery_date = '20240101'.
  ls_schedule-quantity = lv_quantity.
  APPEND ls_schedule TO gt_schedule.
ENDFORM.

FORM merge_schedule CHANGING ct_schedule TYPE ty_schedule_tab.
  DATA lt_schedule TYPE ty_schedule_tab.
  DATA ls_schedule TYPE ty_schedule.
  FIELD-SYMBOLS <ls_existing> TYPE ty_schedule.

  lt_schedule[] = ct_schedule[].
  CLEAR ct_schedule[].
  SORT lt_schedule BY po_item delivery_date.
  LOOP AT lt_schedule INTO ls_schedule.
    READ TABLE ct_schedule ASSIGNING <ls_existing>
      WITH KEY po_item = ls_schedule-po_item
               delivery_date = ls_schedule-delivery_date.
    IF sy-subrc = 0.
      <ls_existing>-quantity = <ls_existing>-quantity + ls_schedule-quantity.
    ELSE.
      APPEND ls_schedule TO ct_schedule.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM call_api.
  PERFORM populate_schedule.
  PERFORM merge_schedule CHANGING gt_schedule.
  CALL FUNCTION 'TARGET_API'
    TABLES
      it_schedule = gt_schedule.
ENDFORM.

START-OF-SELECTION.
  PERFORM call_api.",
                    ),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///target.abap"),
                    version: 1,
                    text: Arc::from(
                        "\
FUNCTION target_api
  TABLES
    it_schedule TYPE string.
ENDFUNCTION.",
                    ),
                    is_dependency: true,
                    object_name: None,
                },
            ],
            "file:///main.abap",
        );

        let trace = build_call_dataflow_trace(
            snapshot.as_ref(),
            CallDataflowQuery {
                target: "TARGET_API".to_string(),
                caller: Some("call_api".to_string()),
                occurrence: None,
            },
        );

        let it_schedule = trace
            .parameter_traces
            .iter()
            .find(|trace| trace.parameter_name.as_deref() == Some("it_schedule"))
            .expect("it_schedule trace");
        assert!(
            it_schedule.field_mappings.iter().any(|mapping| {
                mapping.source_kind == "perform_write"
                    && mapping.source_display == "populate_schedule:gt_schedule"
            }),
            "{:?}",
            it_schedule.field_mappings
        );
        assert!(
            it_schedule.field_mappings.iter().any(|mapping| {
                mapping.source_kind == "loop_binding"
                    && mapping.source_display == "lt_schedule"
                    && mapping
                        .statement_text
                        .as_deref()
                        .is_some_and(|text| text.contains("LOOP AT lt_schedule INTO ls_schedule"))
            }),
            "{:?}",
            it_schedule.field_mappings
        );
        assert!(
            it_schedule.field_mappings.iter().any(|mapping| {
                mapping.target_path == "it_schedule[*].quantity"
                    && mapping.source_kind == "assignment"
                    && mapping.source_display == "lv_quantity"
            }),
            "{:?}",
            it_schedule.field_mappings
        );
        assert!(
            it_schedule.provenance.nodes.iter().any(|node| {
                node.kind == "loop_binding"
                    && node
                        .label
                        .contains("ls_schedule <- LOOP AT lt_schedule INTO ls_schedule")
            }),
            "{:?}",
            it_schedule.provenance.nodes
        );
    }

    #[test]
    fn returns_ambiguous_matches_in_stable_order() {
        let snapshot = snapshot_for(
            vec![DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(
                    "\
FORM first.
  CALL FUNCTION 'BAPI_PO_CREATE1'.
ENDFORM.

FORM second.
  CALL FUNCTION 'BAPI_PO_CREATE1'.
ENDFORM.",
                ),
                is_dependency: false,
                object_name: None,
            }],
            "file:///main.abap",
        );

        let trace = build_call_dataflow_trace(
            snapshot.as_ref(),
            CallDataflowQuery {
                target: "BAPI_PO_CREATE1".to_string(),
                caller: None,
                occurrence: None,
            },
        );

        assert!(trace.selected_call.is_none());
        assert_eq!(trace.matches.len(), 2);
        assert_eq!(trace.matches[0].occurrence, 1);
        assert_eq!(trace.matches[1].occurrence, 2);
        assert!(trace.summary.ambiguous);
    }
}
