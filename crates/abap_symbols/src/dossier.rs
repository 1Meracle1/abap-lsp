use std::collections::BTreeSet;
use std::sync::Arc;

use abap_lexer::TextRange;
use abap_parser::ParseError;
use serde::Serialize;

use crate::def_map::{
    AssignmentSiteData, CallArgumentData, CallSiteData, ClassInheritanceData, ClassMemberData,
    ClassMemberKind, ClassMemberParameterData, Diagnostic, DiagnosticKind, ExpressionFactData,
    ExpressionFactKind, FieldTypeRefData, FunctionModuleData, FunctionModuleExceptionData,
    FunctionModuleParameterData, FunctionModuleParameterSection, ImplementedInterfaceData,
    IncludeEdge, MemberAliasData, MethodParameterSection, NamedArgumentSection,
    NamedArgumentTarget, PerformArgumentData, PerformCallData, PerformParameterSection,
    ReferenceData, ReferenceKind, Resolution, SqlDynamicFragmentData, SqlDynamicFragmentKind,
    SqlNameRefData, SqlNameRefKind, SqlPredicateData, SqlPredicateKind, SqlProjectionData,
    SqlProjectionKind, SqlQueryData, SqlResolution, SqlSourceData, SqlSourceKind, SqlTargetData,
    SqlTargetKind, StructureData, StructureFieldData, SymbolData, SymbolKind,
    SystemFieldStatementKind, SystemFieldUpdateData, TypeFactData, UnitAnalysis, ValueFlowEdgeData,
    ValueFlowKind, ValueFlowTargetData, Visibility,
};
use crate::ids::{SymbolHandle, UnitId};
use crate::project::ProjectAnalysis;
use crate::routine_analysis::RoutineKind;
use crate::scope::{Namespace, ScopeData, ScopeKind};
use crate::static_analysis::{
    ProjectStaticAnalysisSummary, RoutineStaticAnalysisFindingCounts, RoutineStaticAnalysisSummary,
    StaticAnalysisFinding, StaticAnalysisFindingKind,
};

#[derive(Debug, Clone, Copy)]
pub struct SemanticDossierContext<'a> {
    pub parse_errors: &'a [ParseError],
    pub project: Option<&'a ProjectAnalysis>,
    pub static_analysis: Option<&'a ProjectStaticAnalysisSummary>,
    pub target_path: Option<&'a str>,
    pub object_name: Option<&'a str>,
    pub is_dependency: bool,
    pub workspace_root_uri: Option<&'a str>,
    pub manifest_present: bool,
    pub project_unit_count: Option<usize>,
    pub dependency_unit_count: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SemanticDossier {
    pub schema: &'static str,
    pub schema_version: u32,
    pub target: DossierTarget,
    pub project: Option<DossierProject>,
    pub summary: DossierSummary,
    pub parse_diagnostics: Vec<ParseDiagnosticDossier>,
    pub semantic_diagnostics: Vec<SemanticDiagnosticDossier>,
    pub static_analysis: Option<StaticAnalysisSectionDossier>,
    pub structures: Vec<StructureDossier>,
    pub symbols: Vec<SymbolDossier>,
    pub references: Vec<ReferenceDossier>,
    pub scopes: Vec<ScopeDossier>,
    pub classes: ClassFactsDossier,
    pub function_modules: Vec<FunctionModuleDossier>,
    pub call_sites: Vec<CallSiteDossier>,
    pub assignment_sites: Vec<AssignmentSiteDossier>,
    pub expression_facts: Vec<ExpressionFactDossier>,
    pub value_flow_edges: Vec<ValueFlowEdgeDossier>,
    pub perform_calls: Vec<PerformCallDossier>,
    pub system_field_updates: Vec<SystemFieldUpdateDossier>,
    pub sql: SqlSectionDossier,
    pub includes: Vec<IncludeEdgeDossier>,
    pub unresolved_names: UnresolvedNamesDossier,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct DossierTarget {
    pub unit_id: u32,
    pub uri: String,
    pub path: Option<String>,
    pub object_name: Option<String>,
    pub is_dependency: bool,
    pub provided_names: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct DossierProject {
    pub workspace_root_uri: Option<String>,
    pub manifest_present: bool,
    pub unit_count: usize,
    pub dependency_unit_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct DossierSummary {
    pub parse_diagnostic_count: usize,
    pub semantic_diagnostic_count: usize,
    pub static_analysis_routine_count: usize,
    pub static_analysis_finding_count: usize,
    pub structure_count: usize,
    pub symbol_count: usize,
    pub reference_count: usize,
    pub resolved_reference_count: usize,
    pub unresolved_reference_count: usize,
    pub scope_count: usize,
    pub class_member_count: usize,
    pub inheritance_fact_count: usize,
    pub call_site_count: usize,
    pub assignment_site_count: usize,
    pub expression_fact_count: usize,
    pub value_flow_edge_count: usize,
    pub perform_call_count: usize,
    pub system_field_update_count: usize,
    pub function_module_count: usize,
    pub sql_query_count: usize,
    pub sql_source_count: usize,
    pub sql_dynamic_fragment_count: usize,
    pub sql_touched_object_count: usize,
    pub include_edge_count: usize,
    pub unresolved_include_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ByteRange {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ParseDiagnosticDossier {
    pub range: ByteRange,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SemanticDiagnosticDossier {
    pub kind: &'static str,
    pub range: ByteRange,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct StaticAnalysisFindingDossier {
    pub kind: &'static str,
    pub range: ByteRange,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct RoutineStaticAnalysisFindingCountsDossier {
    pub unreachable_code: usize,
    pub use_before_definite_assignment: usize,
    pub possibly_unbound_field_symbol: usize,
    pub dead_store: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct RoutineStaticAnalysisDossier {
    pub routine_id: u32,
    pub scope_id: u32,
    pub owner: Option<ResolvedSymbolDossier>,
    pub kind: &'static str,
    pub name: String,
    pub decl_range: ByteRange,
    pub executable_range: Option<ByteRange>,
    pub instruction_count: usize,
    pub reachable_instruction_count: usize,
    pub block_count: usize,
    pub reachable_block_count: usize,
    pub unreachable_block_count: usize,
    pub dataflow_converged: bool,
    pub dataflow_iterations: u32,
    pub finding_counts: RoutineStaticAnalysisFindingCountsDossier,
    pub findings: Vec<StaticAnalysisFindingDossier>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct StaticAnalysisSectionDossier {
    pub routine_count: usize,
    pub finding_count: usize,
    pub routines: Vec<RoutineStaticAnalysisDossier>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct TypeRefDossier {
    pub namespace: &'static str,
    pub is_ref: bool,
    pub base_name: String,
    pub field_path: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct StructureFieldDossier {
    pub name: String,
    pub decl_range: Option<ByteRange>,
    pub decl_unit_id: u32,
    pub nested_structure_id: Option<u32>,
    pub type_ref: Option<TypeRefDossier>,
    pub value_clause_display: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct StructureDossier {
    pub id: u32,
    pub origin_unit_id: u32,
    pub origin_structure_id: u32,
    pub name: String,
    pub fields: Vec<StructureFieldDossier>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SymbolDossier {
    pub id: u32,
    pub name: String,
    pub kind: &'static str,
    pub namespaces: Vec<&'static str>,
    pub is_builtin: bool,
    pub scope_id: u32,
    pub decl_range: ByteRange,
    pub structure_id: Option<u32>,
    pub declared_type: Option<TypeRefDossier>,
    pub type_clause_display: Option<String>,
    pub value_clause_display: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ResolvedSymbolDossier {
    pub unit_id: u32,
    pub uri: Option<String>,
    pub symbol_id: u32,
    pub name: Option<String>,
    pub kind: Option<&'static str>,
    pub decl_range: Option<ByteRange>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "status", rename_all = "snake_case")]
pub enum ReferenceResolutionDossier {
    Symbol { symbol: ResolvedSymbolDossier },
    BuiltinType,
    BuiltinRoutine,
    InternalTableLine,
    External,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ReferenceDossier {
    pub id: u32,
    pub name: String,
    pub namespace: &'static str,
    pub kind: &'static str,
    pub scope_id: u32,
    pub range: ByteRange,
    pub resolution: Option<ReferenceResolutionDossier>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ScopeDossier {
    pub id: u32,
    pub kind: &'static str,
    pub range: ByteRange,
    pub parent_scope_id: Option<u32>,
    pub owner_symbol_id: Option<u32>,
    pub declaration_symbol_ids: Vec<u32>,
    pub child_scope_ids: Vec<u32>,
    pub allows_internal_table_line_selector: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ClassMemberParameterDossier {
    pub section: &'static str,
    pub name: String,
    pub range: ByteRange,
    pub declared_type: Option<TypeRefDossier>,
    pub type_clause_display: Option<String>,
    pub is_optional: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ClassMemberDossier {
    pub class_symbol_id: u32,
    pub class_name: Option<String>,
    pub name: String,
    pub kind: &'static str,
    pub visibility: &'static str,
    pub is_static: bool,
    pub decl_range: ByteRange,
    pub implementation_range: Option<ByteRange>,
    pub signature: String,
    pub parameters: Vec<ClassMemberParameterDossier>,
    pub structure_id: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ClassInheritanceDossier {
    pub class_symbol_id: u32,
    pub class_name: Option<String>,
    pub superclass_name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ImplementedInterfaceDossier {
    pub owner_symbol_id: u32,
    pub owner_name: Option<String>,
    pub interface_name: String,
    pub range: ByteRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct MemberAliasDossier {
    pub owner_symbol_id: u32,
    pub owner_name: Option<String>,
    pub alias_name: String,
    pub target_interface_name: String,
    pub target_member_name: String,
    pub range: ByteRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ClassFactsDossier {
    pub members: Vec<ClassMemberDossier>,
    pub inheritance: Vec<ClassInheritanceDossier>,
    pub implemented_interfaces: Vec<ImplementedInterfaceDossier>,
    pub aliases: Vec<MemberAliasDossier>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FunctionModuleParameterDossier {
    pub section: &'static str,
    pub name: String,
    pub range: ByteRange,
    pub declared_type: Option<TypeRefDossier>,
    pub type_clause_display: Option<String>,
    pub is_optional: bool,
    pub has_default_value: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FunctionModuleExceptionDossier {
    pub name: String,
    pub range: ByteRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FunctionModuleDossier {
    pub symbol_id: u32,
    pub name: Option<String>,
    pub signature: String,
    pub parameters: Vec<FunctionModuleParameterDossier>,
    pub exceptions: Vec<FunctionModuleExceptionDossier>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct TypeFactDossier {
    pub known: bool,
    pub structure_id: Option<u32>,
    pub declared_type: Option<TypeRefDossier>,
    pub type_clause_display: Option<String>,
    pub table_line: Option<Box<TypeFactDossier>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CallArgumentDossier {
    pub range: ByteRange,
    pub name: Option<String>,
    pub section: Option<&'static str>,
    pub ordinal: usize,
    pub type_fact: TypeFactDossier,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum CallTargetDossier {
    Constructor {
        type_name: String,
    },
    Function {
        function_name: String,
    },
    Report {
        report_name: String,
    },
    Routine {
        routine_name: String,
    },
    ImplicitMethod {
        method_name: String,
    },
    Method {
        base_namespace: &'static str,
        base_name: String,
        method_name: String,
    },
    Event {
        qualifier: Option<String>,
        event_name: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CallSiteDossier {
    pub scope_id: u32,
    pub range: ByteRange,
    pub target: CallTargetDossier,
    pub arguments: Vec<CallArgumentDossier>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AssignmentSiteDossier {
    pub scope_id: u32,
    pub range: ByteRange,
    pub lhs_range: ByteRange,
    pub rhs_range: ByteRange,
    pub lhs: TypeFactDossier,
    pub rhs: TypeFactDossier,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ExpressionFactDossier {
    pub scope_id: u32,
    pub range: ByteRange,
    pub kind: &'static str,
    pub type_fact: TypeFactDossier,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SystemFieldUpdateDossier {
    pub scope_id: u32,
    pub range: ByteRange,
    pub statement: &'static str,
    pub field_name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ValueFlowTargetDossier {
    Assignment {
        range: ByteRange,
    },
    CallParameter {
        call_range: ByteRange,
        target: CallTargetDossier,
        parameter_name: Option<String>,
        parameter_decl_unit_id: Option<u32>,
        parameter_decl_range: Option<ByteRange>,
    },
    FieldSymbol {
        range: ByteRange,
        name: Option<String>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ValueFlowEdgeDossier {
    pub scope_id: u32,
    pub kind: &'static str,
    pub source_range: ByteRange,
    pub source_type: TypeFactDossier,
    pub target: ValueFlowTargetDossier,
    pub target_type: TypeFactDossier,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PerformArgumentDossier {
    pub range: ByteRange,
    pub section: &'static str,
    pub ordinal_in_section: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PerformCallDossier {
    pub scope_id: u32,
    pub range: ByteRange,
    pub routine_name: String,
    pub routine_range: ByteRange,
    pub is_dynamic: bool,
    pub program_name: Option<String>,
    pub program_range: Option<ByteRange>,
    pub program_is_dynamic: bool,
    pub has_if_found: bool,
    pub parameters: Vec<&'static str>,
    pub arguments: Vec<PerformArgumentDossier>,
    pub section_order_invalid: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SqlSourceDossier {
    pub range: ByteRange,
    pub source_kind: &'static str,
    pub name: String,
    pub alias: Option<String>,
    pub join_kind: Option<String>,
    pub resolution: &'static str,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SqlDynamicFragmentDossier {
    pub scope_id: u32,
    pub range: ByteRange,
    pub kind: &'static str,
    pub verification: &'static str,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SqlProjectionDossier {
    pub range: ByteRange,
    pub kind: &'static str,
    pub source_alias: Option<String>,
    pub name: Option<String>,
    pub alias: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SqlNameRefDossier {
    pub scope_id: u32,
    pub range: ByteRange,
    pub name: String,
    pub qualifier: Option<String>,
    pub kind: &'static str,
    pub resolution: &'static str,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SqlPredicateDossier {
    pub range: ByteRange,
    pub kind: &'static str,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SqlTargetDossier {
    pub scope_id: u32,
    pub range: ByteRange,
    pub kind: &'static str,
    pub target_name: Option<String>,
    pub is_table: bool,
    pub is_corresponding: bool,
    pub is_inline: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SqlQueryDossier {
    pub id: usize,
    pub scope_id: u32,
    pub range: ByteRange,
    pub clauses: SqlClauseRangesDossier,
    pub is_single: bool,
    pub is_distinct: bool,
    pub is_for_update: bool,
    pub has_package_size: bool,
    pub has_set_operators: bool,
    pub has_endselect: bool,
    pub has_dynamic_where: bool,
    pub order_by_primary_key: bool,
    pub order_by_fields: Vec<String>,
    pub sources: Vec<SqlSourceDossier>,
    pub projections: Vec<SqlProjectionDossier>,
    pub predicates: Vec<SqlPredicateDossier>,
    pub targets: Vec<SqlTargetDossier>,
    pub name_refs: Vec<SqlNameRefDossier>,
    pub dynamic_fragments: Vec<SqlDynamicFragmentDossier>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SqlClauseRangesDossier {
    pub projection: Option<ByteRange>,
    pub from: Option<ByteRange>,
    pub into: Option<ByteRange>,
    pub where_clause: Option<ByteRange>,
    pub group_by: Option<ByteRange>,
    pub having: Option<ByteRange>,
    pub order_by: Option<ByteRange>,
    pub for_all_entries: Option<ByteRange>,
    pub for_update: Option<ByteRange>,
    pub up_to: Option<ByteRange>,
    pub package_size: Option<ByteRange>,
    pub offset: Option<ByteRange>,
    pub abap_options: Option<ByteRange>,
    pub set_operator: Option<ByteRange>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SqlSectionDossier {
    pub touched_objects: Vec<String>,
    pub dynamic_fragments: Vec<SqlDynamicFragmentDossier>,
    pub queries: Vec<SqlQueryDossier>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct IncludeEdgeDossier {
    pub name: String,
    pub range: ByteRange,
    pub target_unit_id: Option<u32>,
    pub target_uri: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct UnresolvedReferenceDossier {
    pub id: u32,
    pub name: String,
    pub namespace: &'static str,
    pub kind: &'static str,
    pub range: ByteRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct UnresolvedSqlNameDossier {
    pub query_id: usize,
    pub name: String,
    pub qualifier: Option<String>,
    pub kind: &'static str,
    pub range: ByteRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct UnresolvedIncludeDossier {
    pub name: String,
    pub range: ByteRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct UnresolvedNamesDossier {
    pub references: Vec<UnresolvedReferenceDossier>,
    pub sql_name_refs: Vec<UnresolvedSqlNameDossier>,
    pub includes: Vec<UnresolvedIncludeDossier>,
}

pub fn build_semantic_dossier(
    unit: &UnitAnalysis,
    context: SemanticDossierContext<'_>,
) -> SemanticDossier {
    let parse_diagnostics: Vec<_> = context
        .parse_errors
        .iter()
        .map(|error| ParseDiagnosticDossier {
            range: byte_range(&error.range),
            message: error.message.clone(),
        })
        .collect();
    let semantic_diagnostics: Vec<_> = unit
        .diagnostics
        .iter()
        .map(semantic_diagnostic_dossier)
        .collect();
    let static_analysis = context
        .static_analysis
        .map(|summary| static_analysis_section_dossier(unit, context.project, summary));
    let structures: Vec<_> = unit.structures.iter().map(structure_dossier).collect();
    let symbols: Vec<_> = unit.symbols.iter().map(symbol_dossier).collect();
    let references: Vec<_> = unit
        .references
        .iter()
        .map(|reference| reference_dossier(unit, context.project, reference))
        .collect();
    let scopes: Vec<_> = unit.scopes.iter().map(scope_dossier).collect();
    let class_facts = ClassFactsDossier {
        members: unit
            .class_members
            .iter()
            .map(|member| class_member_dossier(unit, member))
            .collect(),
        inheritance: unit
            .class_inheritance
            .iter()
            .map(|inheritance| class_inheritance_dossier(unit, inheritance))
            .collect(),
        implemented_interfaces: unit
            .implemented_interfaces
            .iter()
            .map(|interface| implemented_interface_dossier(unit, interface))
            .collect(),
        aliases: unit
            .member_aliases
            .iter()
            .map(|alias| member_alias_dossier(unit, alias))
            .collect(),
    };
    let function_modules: Vec<_> = unit
        .function_modules
        .iter()
        .map(|function_module| function_module_dossier(unit, function_module))
        .collect();
    let call_sites: Vec<_> = unit.call_sites.iter().map(call_site_dossier).collect();
    let assignment_sites: Vec<_> = unit
        .assignment_sites
        .iter()
        .map(assignment_site_dossier)
        .collect();
    let expression_facts: Vec<_> = unit
        .expression_facts
        .iter()
        .map(expression_fact_dossier)
        .collect();
    let value_flow_edges: Vec<_> = unit
        .value_flow_edges
        .iter()
        .map(value_flow_edge_dossier)
        .collect();
    let perform_calls: Vec<_> = unit
        .perform_calls
        .iter()
        .map(perform_call_dossier)
        .collect();
    let system_field_updates: Vec<_> = unit
        .system_field_updates
        .iter()
        .map(system_field_update_dossier)
        .collect();
    let queries: Vec<_> = unit
        .sql_queries
        .iter()
        .map(|query| sql_query_dossier(unit, query))
        .collect();
    let dynamic_fragments: Vec<_> = unit
        .sql_dynamic_fragments
        .iter()
        .map(sql_dynamic_fragment_dossier)
        .collect();
    let touched_objects: Vec<_> = unit
        .sql_sources
        .iter()
        .filter(|source| source.resolution == SqlResolution::External)
        .map(|source| source.name.to_string())
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect();
    let includes: Vec<_> = unit
        .include_edges
        .iter()
        .map(|edge| include_edge_dossier(context.project, edge))
        .collect();
    let unresolved_names = UnresolvedNamesDossier {
        references: unit
            .references
            .iter()
            .filter(|reference| reference.resolution.is_none())
            .map(unresolved_reference_dossier)
            .collect(),
        sql_name_refs: unit
            .sql_name_refs
            .iter()
            .filter(|sql_ref| sql_ref.resolution == SqlResolution::Unresolved)
            .map(unresolved_sql_name_dossier)
            .collect(),
        includes: unit
            .include_edges
            .iter()
            .filter(|edge| edge.target.is_none())
            .map(unresolved_include_dossier)
            .collect(),
    };

    SemanticDossier {
        schema: "abap.semantic_dossier",
        schema_version: 5,
        target: DossierTarget {
            unit_id: unit.unit_id.0,
            uri: unit.uri.to_string(),
            path: context.target_path.map(str::to_string),
            object_name: context.object_name.map(str::to_string),
            is_dependency: context.is_dependency,
            provided_names: unit
                .provided_names
                .iter()
                .map(|name| name.to_string())
                .collect(),
        },
        project: context.project_unit_count.map(|unit_count| DossierProject {
            workspace_root_uri: context.workspace_root_uri.map(str::to_string),
            manifest_present: context.manifest_present,
            unit_count,
            dependency_unit_count: context.dependency_unit_count.unwrap_or(0),
        }),
        summary: DossierSummary {
            parse_diagnostic_count: parse_diagnostics.len(),
            semantic_diagnostic_count: semantic_diagnostics.len(),
            static_analysis_routine_count: static_analysis
                .as_ref()
                .map_or(0, |summary| summary.routine_count),
            static_analysis_finding_count: static_analysis
                .as_ref()
                .map_or(0, |summary| summary.finding_count),
            structure_count: structures.len(),
            symbol_count: symbols.len(),
            reference_count: references.len(),
            resolved_reference_count: references
                .iter()
                .filter(|reference| reference.resolution.is_some())
                .count(),
            unresolved_reference_count: unresolved_names.references.len(),
            scope_count: scopes.len(),
            class_member_count: class_facts.members.len(),
            inheritance_fact_count: class_facts.inheritance.len(),
            call_site_count: call_sites.len(),
            assignment_site_count: assignment_sites.len(),
            expression_fact_count: expression_facts.len(),
            value_flow_edge_count: value_flow_edges.len(),
            perform_call_count: perform_calls.len(),
            system_field_update_count: system_field_updates.len(),
            function_module_count: function_modules.len(),
            sql_query_count: queries.len(),
            sql_source_count: unit.sql_sources.len(),
            sql_dynamic_fragment_count: unit.sql_dynamic_fragments.len(),
            sql_touched_object_count: touched_objects.len(),
            include_edge_count: includes.len(),
            unresolved_include_count: unresolved_names.includes.len(),
        },
        parse_diagnostics,
        semantic_diagnostics,
        static_analysis,
        structures,
        symbols,
        references,
        scopes,
        classes: class_facts,
        function_modules,
        call_sites,
        assignment_sites,
        expression_facts,
        value_flow_edges,
        perform_calls,
        system_field_updates,
        sql: SqlSectionDossier {
            touched_objects,
            dynamic_fragments,
            queries,
        },
        includes,
        unresolved_names,
    }
}

fn byte_range(range: &TextRange) -> ByteRange {
    ByteRange {
        start: range.start,
        end: range.end,
    }
}

fn semantic_diagnostic_dossier(diagnostic: &Diagnostic) -> SemanticDiagnosticDossier {
    SemanticDiagnosticDossier {
        kind: diagnostic_kind_name(diagnostic.kind),
        range: byte_range(&diagnostic.range),
        message: diagnostic.message.clone(),
    }
}

fn static_analysis_section_dossier(
    current_unit: &UnitAnalysis,
    project: Option<&ProjectAnalysis>,
    static_analysis: &ProjectStaticAnalysisSummary,
) -> StaticAnalysisSectionDossier {
    let routines: Vec<_> = static_analysis
        .routines_for_unit(current_unit.unit_id)
        .map(|routine| routine_static_analysis_dossier(current_unit, project, routine))
        .collect();
    let finding_count = routines.iter().map(|routine| routine.findings.len()).sum();
    StaticAnalysisSectionDossier {
        routine_count: routines.len(),
        finding_count,
        routines,
    }
}

fn routine_static_analysis_dossier(
    current_unit: &UnitAnalysis,
    project: Option<&ProjectAnalysis>,
    routine: &RoutineStaticAnalysisSummary,
) -> RoutineStaticAnalysisDossier {
    RoutineStaticAnalysisDossier {
        routine_id: routine.routine.0,
        scope_id: routine.scope.0,
        owner: routine
            .owner
            .map(|owner| resolved_symbol_dossier(current_unit, project, owner)),
        kind: routine_kind_name(routine.kind),
        name: routine.name.to_string(),
        decl_range: byte_range(&routine.decl_range),
        executable_range: routine.executable_range.as_ref().map(byte_range),
        instruction_count: routine.instruction_count,
        reachable_instruction_count: routine.reachable_instruction_count,
        block_count: routine.block_count,
        reachable_block_count: routine.reachable_block_count,
        unreachable_block_count: routine.unreachable_block_count,
        dataflow_converged: routine.dataflow_converged,
        dataflow_iterations: routine.dataflow_iterations,
        finding_counts: routine_static_analysis_finding_counts_dossier(&routine.finding_counts),
        findings: routine
            .findings
            .iter()
            .map(static_analysis_finding_dossier)
            .collect(),
    }
}

fn routine_static_analysis_finding_counts_dossier(
    counts: &RoutineStaticAnalysisFindingCounts,
) -> RoutineStaticAnalysisFindingCountsDossier {
    RoutineStaticAnalysisFindingCountsDossier {
        unreachable_code: counts.unreachable_code,
        use_before_definite_assignment: counts.use_before_definite_assignment,
        possibly_unbound_field_symbol: counts.possibly_unbound_field_symbol,
        dead_store: counts.dead_store,
    }
}

fn static_analysis_finding_dossier(
    finding: &StaticAnalysisFinding,
) -> StaticAnalysisFindingDossier {
    StaticAnalysisFindingDossier {
        kind: static_analysis_finding_kind_name(finding.kind),
        range: byte_range(&finding.range),
        message: finding.message.clone(),
    }
}

fn type_ref_dossier(type_ref: &FieldTypeRefData) -> TypeRefDossier {
    TypeRefDossier {
        namespace: namespace_name(type_ref.namespace),
        is_ref: type_ref.is_ref,
        base_name: type_ref.base_name.to_string(),
        field_path: type_ref
            .field_path
            .iter()
            .map(|segment| segment.to_string())
            .collect(),
    }
}

fn structure_field_dossier(field: &StructureFieldData) -> StructureFieldDossier {
    StructureFieldDossier {
        name: field.name.to_string(),
        decl_range: field.decl_range.as_ref().map(byte_range),
        decl_unit_id: field.decl_unit.0,
        nested_structure_id: field.structure.map(|id| id.0),
        type_ref: field.type_ref.as_ref().map(type_ref_dossier),
        value_clause_display: field.value_clause_display.as_ref().map(arc_str_to_string),
    }
}

fn structure_dossier(structure: &StructureData) -> StructureDossier {
    StructureDossier {
        id: structure.id.0,
        origin_unit_id: structure.origin_unit.0,
        origin_structure_id: structure.origin_structure.0,
        name: structure.name.to_string(),
        fields: structure
            .fields
            .iter()
            .map(structure_field_dossier)
            .collect(),
    }
}

fn symbol_dossier(symbol: &SymbolData) -> SymbolDossier {
    SymbolDossier {
        id: symbol.id.0,
        name: symbol.name.to_string(),
        kind: symbol_kind_name(symbol.kind),
        namespaces: symbol
            .kind
            .namespaces()
            .iter()
            .map(|namespace| namespace_name(*namespace))
            .collect(),
        is_builtin: symbol.kind.is_builtin(),
        scope_id: symbol.scope.0,
        decl_range: byte_range(&symbol.decl_range),
        structure_id: symbol.structure.map(|id| id.0),
        declared_type: symbol.declared_type.as_ref().map(type_ref_dossier),
        type_clause_display: symbol.type_clause_display.as_ref().map(arc_str_to_string),
        value_clause_display: symbol.value_clause_display.as_ref().map(arc_str_to_string),
    }
}

fn reference_dossier(
    unit: &UnitAnalysis,
    project: Option<&ProjectAnalysis>,
    reference: &ReferenceData,
) -> ReferenceDossier {
    ReferenceDossier {
        id: reference.id.0,
        name: reference.name.to_string(),
        namespace: namespace_name(reference.namespace),
        kind: reference_kind_name(reference.kind),
        scope_id: reference.scope.0,
        range: byte_range(&reference.range),
        resolution: reference
            .resolution
            .as_ref()
            .map(|resolution| reference_resolution_dossier(unit, project, resolution)),
    }
}

fn scope_dossier(scope: &ScopeData) -> ScopeDossier {
    ScopeDossier {
        id: scope.id.0,
        kind: scope_kind_name(scope.kind),
        range: byte_range(&scope.range),
        parent_scope_id: scope.parent.map(|id| id.0),
        owner_symbol_id: scope.owner.map(|id| id.0),
        declaration_symbol_ids: scope.declarations.iter().map(|id| id.0).collect(),
        child_scope_ids: scope.children.iter().map(|id| id.0).collect(),
        allows_internal_table_line_selector: scope.allows_internal_table_line_selector,
    }
}

fn class_member_parameter_dossier(
    parameter: &ClassMemberParameterData,
) -> ClassMemberParameterDossier {
    ClassMemberParameterDossier {
        section: method_parameter_section_name(parameter.section),
        name: parameter.name.to_string(),
        range: byte_range(&parameter.range),
        declared_type: parameter.declared_type.as_ref().map(type_ref_dossier),
        type_clause_display: parameter
            .type_clause_display
            .as_ref()
            .map(arc_str_to_string),
        is_optional: parameter.is_optional,
    }
}

fn class_member_dossier(unit: &UnitAnalysis, member: &ClassMemberData) -> ClassMemberDossier {
    ClassMemberDossier {
        class_symbol_id: member.class_symbol.0,
        class_name: unit_symbol_name(unit, member.class_symbol),
        name: member.name.to_string(),
        kind: class_member_kind_name(member.kind),
        visibility: visibility_name(member.visibility),
        is_static: member.is_static,
        decl_range: byte_range(&member.decl_range),
        implementation_range: member.implementation_range.as_ref().map(byte_range),
        signature: member.signature.to_string(),
        parameters: member
            .parameters
            .iter()
            .map(class_member_parameter_dossier)
            .collect(),
        structure_id: member.structure.map(|id| id.0),
    }
}

fn class_inheritance_dossier(
    unit: &UnitAnalysis,
    inheritance: &ClassInheritanceData,
) -> ClassInheritanceDossier {
    ClassInheritanceDossier {
        class_symbol_id: inheritance.class_symbol.0,
        class_name: unit_symbol_name(unit, inheritance.class_symbol),
        superclass_name: inheritance.superclass_name.to_string(),
    }
}

fn implemented_interface_dossier(
    unit: &UnitAnalysis,
    interface: &ImplementedInterfaceData,
) -> ImplementedInterfaceDossier {
    ImplementedInterfaceDossier {
        owner_symbol_id: interface.owner_symbol.0,
        owner_name: unit_symbol_name(unit, interface.owner_symbol),
        interface_name: interface.interface_name.to_string(),
        range: byte_range(&interface.range),
    }
}

fn member_alias_dossier(unit: &UnitAnalysis, alias: &MemberAliasData) -> MemberAliasDossier {
    MemberAliasDossier {
        owner_symbol_id: alias.owner_symbol.0,
        owner_name: unit_symbol_name(unit, alias.owner_symbol),
        alias_name: alias.alias_name.to_string(),
        target_interface_name: alias.target_interface_name.to_string(),
        target_member_name: alias.target_member_name.to_string(),
        range: byte_range(&alias.range),
    }
}

fn function_module_parameter_dossier(
    parameter: &FunctionModuleParameterData,
) -> FunctionModuleParameterDossier {
    FunctionModuleParameterDossier {
        section: function_module_parameter_section_name(parameter.section),
        name: parameter.name.to_string(),
        range: byte_range(&parameter.range),
        declared_type: parameter.declared_type.as_ref().map(type_ref_dossier),
        type_clause_display: parameter
            .type_clause_display
            .as_ref()
            .map(arc_str_to_string),
        is_optional: parameter.is_optional,
        has_default_value: parameter.has_default_value,
    }
}

fn function_module_exception_dossier(
    exception: &FunctionModuleExceptionData,
) -> FunctionModuleExceptionDossier {
    FunctionModuleExceptionDossier {
        name: exception.name.to_string(),
        range: byte_range(&exception.range),
    }
}

fn function_module_dossier(
    unit: &UnitAnalysis,
    function_module: &FunctionModuleData,
) -> FunctionModuleDossier {
    FunctionModuleDossier {
        symbol_id: function_module.symbol.0,
        name: unit_symbol_name(unit, function_module.symbol),
        signature: function_module.signature.to_string(),
        parameters: function_module
            .parameters
            .iter()
            .map(function_module_parameter_dossier)
            .collect(),
        exceptions: function_module
            .exceptions
            .iter()
            .map(function_module_exception_dossier)
            .collect(),
    }
}

fn type_fact_dossier(type_fact: &TypeFactData) -> TypeFactDossier {
    TypeFactDossier {
        known: type_fact.is_known(),
        structure_id: type_fact.structure.map(|id| id.0),
        declared_type: type_fact.declared_type.as_ref().map(type_ref_dossier),
        type_clause_display: type_fact
            .type_clause_display
            .as_ref()
            .map(arc_str_to_string),
        table_line: type_fact
            .table_line
            .as_ref()
            .map(|line| Box::new(type_fact_dossier(line))),
    }
}

fn call_argument_dossier(argument: &CallArgumentData) -> CallArgumentDossier {
    CallArgumentDossier {
        range: byte_range(&argument.range),
        name: argument.name.as_ref().map(arc_str_to_string),
        section: argument.section.map(named_argument_section_name),
        ordinal: argument.ordinal,
        type_fact: type_fact_dossier(&argument.type_fact),
    }
}

fn call_target_dossier(target: &NamedArgumentTarget) -> CallTargetDossier {
    match target {
        NamedArgumentTarget::Constructor { type_name } => CallTargetDossier::Constructor {
            type_name: type_name.to_string(),
        },
        NamedArgumentTarget::Function { function_name } => CallTargetDossier::Function {
            function_name: function_name.to_string(),
        },
        NamedArgumentTarget::Report { report_name } => CallTargetDossier::Report {
            report_name: report_name.to_string(),
        },
        NamedArgumentTarget::Routine { routine_name } => CallTargetDossier::Routine {
            routine_name: routine_name.to_string(),
        },
        NamedArgumentTarget::ImplicitMethod { method_name } => CallTargetDossier::ImplicitMethod {
            method_name: method_name.to_string(),
        },
        NamedArgumentTarget::Method {
            base_namespace,
            base_name,
            method_name,
            ..
        } => CallTargetDossier::Method {
            base_namespace: namespace_name(*base_namespace),
            base_name: base_name.to_string(),
            method_name: method_name.to_string(),
        },
        NamedArgumentTarget::Event {
            qualifier,
            event_name,
        } => CallTargetDossier::Event {
            qualifier: qualifier.as_ref().map(arc_str_to_string),
            event_name: event_name.to_string(),
        },
    }
}

fn call_site_dossier(call_site: &CallSiteData) -> CallSiteDossier {
    CallSiteDossier {
        scope_id: call_site.scope.0,
        range: byte_range(&call_site.range),
        target: call_target_dossier(&call_site.target),
        arguments: call_site
            .arguments
            .iter()
            .map(call_argument_dossier)
            .collect(),
    }
}

fn assignment_site_dossier(assignment: &AssignmentSiteData) -> AssignmentSiteDossier {
    AssignmentSiteDossier {
        scope_id: assignment.scope.0,
        range: byte_range(&assignment.range),
        lhs_range: byte_range(&assignment.lhs_range),
        rhs_range: byte_range(&assignment.rhs_range),
        lhs: type_fact_dossier(&assignment.lhs),
        rhs: type_fact_dossier(&assignment.rhs),
    }
}

fn expression_fact_dossier(fact: &ExpressionFactData) -> ExpressionFactDossier {
    ExpressionFactDossier {
        scope_id: fact.scope.0,
        range: byte_range(&fact.range),
        kind: expression_fact_kind_name(fact.kind),
        type_fact: type_fact_dossier(&fact.type_fact),
    }
}

fn system_field_update_dossier(update: &SystemFieldUpdateData) -> SystemFieldUpdateDossier {
    SystemFieldUpdateDossier {
        scope_id: update.scope.0,
        range: byte_range(&update.range),
        statement: system_field_statement_kind_name(update.statement),
        field_name: update.field_name.to_string(),
    }
}

fn value_flow_edge_dossier(edge: &ValueFlowEdgeData) -> ValueFlowEdgeDossier {
    ValueFlowEdgeDossier {
        scope_id: edge.scope.0,
        kind: value_flow_kind_name(edge.kind),
        source_range: byte_range(&edge.source_range),
        source_type: type_fact_dossier(&edge.source_type),
        target: value_flow_target_dossier(&edge.target),
        target_type: type_fact_dossier(&edge.target_type),
    }
}

fn value_flow_target_dossier(target: &ValueFlowTargetData) -> ValueFlowTargetDossier {
    match target {
        ValueFlowTargetData::Assignment { range } => ValueFlowTargetDossier::Assignment {
            range: byte_range(range),
        },
        ValueFlowTargetData::CallParameter {
            call_range,
            target,
            parameter_name,
            parameter_decl_unit,
            parameter_decl_range,
        } => ValueFlowTargetDossier::CallParameter {
            call_range: byte_range(call_range),
            target: call_target_dossier(target),
            parameter_name: parameter_name.as_ref().map(arc_str_to_string),
            parameter_decl_unit_id: parameter_decl_unit.map(|unit_id| unit_id.0),
            parameter_decl_range: parameter_decl_range.as_ref().map(byte_range),
        },
        ValueFlowTargetData::FieldSymbol { range, name } => ValueFlowTargetDossier::FieldSymbol {
            range: byte_range(range),
            name: name.as_ref().map(arc_str_to_string),
        },
    }
}

fn perform_argument_dossier(argument: &PerformArgumentData) -> PerformArgumentDossier {
    PerformArgumentDossier {
        range: byte_range(&argument.range),
        section: perform_parameter_section_name(argument.section),
        ordinal_in_section: argument.ordinal_in_section,
    }
}

fn perform_call_dossier(perform_call: &PerformCallData) -> PerformCallDossier {
    PerformCallDossier {
        scope_id: perform_call.scope.0,
        range: byte_range(&perform_call.range),
        routine_name: perform_call.routine_name.to_string(),
        routine_range: byte_range(&perform_call.routine_range),
        is_dynamic: perform_call.is_dynamic,
        program_name: perform_call
            .program
            .as_ref()
            .map(|program| program.name.to_string()),
        program_range: perform_call
            .program
            .as_ref()
            .map(|program| byte_range(&program.range)),
        program_is_dynamic: perform_call
            .program
            .as_ref()
            .is_some_and(|program| program.is_dynamic),
        has_if_found: perform_call.has_if_found,
        parameters: perform_call
            .parameters
            .iter()
            .map(|section| perform_parameter_section_name(*section))
            .collect(),
        arguments: perform_call
            .arguments
            .iter()
            .map(perform_argument_dossier)
            .collect(),
        section_order_invalid: perform_call.section_order_invalid,
    }
}

fn sql_query_dossier(unit: &UnitAnalysis, query: &SqlQueryData) -> SqlQueryDossier {
    SqlQueryDossier {
        id: query.id,
        scope_id: query.scope.0,
        range: byte_range(&query.range),
        clauses: SqlClauseRangesDossier {
            projection: query.projection_clause.as_ref().map(byte_range),
            from: query.from_clause.as_ref().map(byte_range),
            into: query.into_clause.as_ref().map(byte_range),
            where_clause: query.where_clause.as_ref().map(byte_range),
            group_by: query.group_by_clause.as_ref().map(byte_range),
            having: query.having_clause.as_ref().map(byte_range),
            order_by: query.order_by_clause.as_ref().map(byte_range),
            for_all_entries: query.for_all_entries_clause.as_ref().map(byte_range),
            for_update: query.for_update_clause.as_ref().map(byte_range),
            up_to: query.up_to_clause.as_ref().map(byte_range),
            package_size: query.package_size_clause.as_ref().map(byte_range),
            offset: query.offset_clause.as_ref().map(byte_range),
            abap_options: query.abap_options_clause.as_ref().map(byte_range),
            set_operator: query.set_operator_clause.as_ref().map(byte_range),
        },
        is_single: query.is_single,
        is_distinct: query.is_distinct,
        is_for_update: query.is_for_update,
        has_package_size: query.has_package_size,
        has_set_operators: query.has_set_operators,
        has_endselect: query.has_endselect,
        has_dynamic_where: query.has_dynamic_where,
        order_by_primary_key: query.order_by_primary_key,
        order_by_fields: query
            .order_by_fields
            .iter()
            .map(|field| field.to_string())
            .collect(),
        sources: unit
            .sql_sources
            .iter()
            .filter(|source| source.query_id == query.id)
            .map(sql_source_dossier)
            .collect(),
        projections: unit
            .sql_projections
            .iter()
            .filter(|projection| projection.query_id == query.id)
            .map(sql_projection_dossier)
            .collect(),
        predicates: unit
            .sql_predicates
            .iter()
            .filter(|predicate| predicate.query_id == query.id)
            .map(sql_predicate_dossier)
            .collect(),
        targets: unit
            .sql_targets
            .iter()
            .filter(|target| target.query_id == query.id)
            .map(sql_target_dossier)
            .collect(),
        name_refs: unit
            .sql_name_refs
            .iter()
            .filter(|name_ref| name_ref.query_id == query.id)
            .map(sql_name_ref_dossier)
            .collect(),
        dynamic_fragments: unit
            .sql_dynamic_fragments
            .iter()
            .filter(|fragment| fragment.query_id == query.id)
            .map(sql_dynamic_fragment_dossier)
            .collect(),
    }
}

fn sql_source_dossier(source: &SqlSourceData) -> SqlSourceDossier {
    SqlSourceDossier {
        range: byte_range(&source.range),
        source_kind: sql_source_kind_name(source.source_kind),
        name: source.name.to_string(),
        alias: source.alias.as_ref().map(arc_str_to_string),
        join_kind: source.join_kind.as_ref().map(arc_str_to_string),
        resolution: sql_resolution_name(source.resolution),
    }
}

fn sql_dynamic_fragment_dossier(fragment: &SqlDynamicFragmentData) -> SqlDynamicFragmentDossier {
    SqlDynamicFragmentDossier {
        scope_id: fragment.scope.0,
        range: byte_range(&fragment.range),
        kind: sql_dynamic_fragment_kind_name(fragment.kind),
        verification: "dynamic_sql_cannot_verify",
    }
}

fn sql_projection_dossier(projection: &SqlProjectionData) -> SqlProjectionDossier {
    SqlProjectionDossier {
        range: byte_range(&projection.range),
        kind: sql_projection_kind_name(projection.kind),
        source_alias: projection.source_alias.as_ref().map(arc_str_to_string),
        name: projection.name.as_ref().map(arc_str_to_string),
        alias: projection.alias.as_ref().map(arc_str_to_string),
    }
}

fn sql_name_ref_dossier(name_ref: &SqlNameRefData) -> SqlNameRefDossier {
    SqlNameRefDossier {
        scope_id: name_ref.scope.0,
        range: byte_range(&name_ref.range),
        name: name_ref.name.to_string(),
        qualifier: name_ref.qualifier.as_ref().map(arc_str_to_string),
        kind: sql_name_ref_kind_name(name_ref.kind),
        resolution: sql_resolution_name(name_ref.resolution),
    }
}

fn sql_predicate_dossier(predicate: &SqlPredicateData) -> SqlPredicateDossier {
    SqlPredicateDossier {
        range: byte_range(&predicate.range),
        kind: sql_predicate_kind_name(predicate.kind),
    }
}

fn sql_target_dossier(target: &SqlTargetData) -> SqlTargetDossier {
    SqlTargetDossier {
        scope_id: target.scope.0,
        range: byte_range(&target.range),
        kind: sql_target_kind_name(target.kind),
        target_name: target.target_name.as_ref().map(arc_str_to_string),
        is_table: target.is_table,
        is_corresponding: target.is_corresponding,
        is_inline: target.is_inline,
    }
}

fn include_edge_dossier(
    project: Option<&ProjectAnalysis>,
    edge: &IncludeEdge,
) -> IncludeEdgeDossier {
    IncludeEdgeDossier {
        name: edge.name.to_string(),
        range: byte_range(&edge.range),
        target_unit_id: edge.target.map(|id| id.0),
        target_uri: edge
            .target
            .and_then(|unit_id| project_unit_uri(project, unit_id))
            .map(str::to_string),
    }
}

fn unresolved_reference_dossier(reference: &ReferenceData) -> UnresolvedReferenceDossier {
    UnresolvedReferenceDossier {
        id: reference.id.0,
        name: reference.name.to_string(),
        namespace: namespace_name(reference.namespace),
        kind: reference_kind_name(reference.kind),
        range: byte_range(&reference.range),
    }
}

fn unresolved_sql_name_dossier(sql_ref: &SqlNameRefData) -> UnresolvedSqlNameDossier {
    UnresolvedSqlNameDossier {
        query_id: sql_ref.query_id,
        name: sql_ref.name.to_string(),
        qualifier: sql_ref.qualifier.as_ref().map(arc_str_to_string),
        kind: sql_name_ref_kind_name(sql_ref.kind),
        range: byte_range(&sql_ref.range),
    }
}

fn unresolved_include_dossier(edge: &IncludeEdge) -> UnresolvedIncludeDossier {
    UnresolvedIncludeDossier {
        name: edge.name.to_string(),
        range: byte_range(&edge.range),
    }
}

fn reference_resolution_dossier(
    current_unit: &UnitAnalysis,
    project: Option<&ProjectAnalysis>,
    resolution: &Resolution,
) -> ReferenceResolutionDossier {
    match resolution {
        Resolution::Symbol(handle) => ReferenceResolutionDossier::Symbol {
            symbol: resolved_symbol_dossier(current_unit, project, *handle),
        },
        Resolution::BuiltinType => ReferenceResolutionDossier::BuiltinType,
        Resolution::BuiltinRoutine => ReferenceResolutionDossier::BuiltinRoutine,
        Resolution::InternalTableLine => ReferenceResolutionDossier::InternalTableLine,
        Resolution::External => ReferenceResolutionDossier::External,
    }
}

fn resolved_symbol_dossier(
    current_unit: &UnitAnalysis,
    project: Option<&ProjectAnalysis>,
    handle: SymbolHandle,
) -> ResolvedSymbolDossier {
    let resolved_unit = project
        .and_then(|project| project.units.get(handle.unit.as_usize()))
        .or_else(|| (handle.unit == current_unit.unit_id).then_some(current_unit));
    let resolved_symbol = resolved_unit.and_then(|unit| unit.symbols.get(handle.symbol.as_usize()));

    ResolvedSymbolDossier {
        unit_id: handle.unit.0,
        uri: project_unit_uri(project, handle.unit)
            .map(str::to_string)
            .or_else(|| {
                (handle.unit == current_unit.unit_id).then(|| current_unit.uri.to_string())
            }),
        symbol_id: handle.symbol.0,
        name: resolved_symbol.map(|symbol| symbol.name.to_string()),
        kind: resolved_symbol.map(|symbol| symbol_kind_name(symbol.kind)),
        decl_range: resolved_symbol.map(|symbol| byte_range(&symbol.decl_range)),
    }
}

fn unit_symbol_name(unit: &UnitAnalysis, symbol_id: crate::ids::SymbolId) -> Option<String> {
    unit.symbols
        .get(symbol_id.as_usize())
        .map(|symbol| symbol.name.to_string())
}

fn project_unit_uri(project: Option<&ProjectAnalysis>, unit_id: UnitId) -> Option<&str> {
    project
        .and_then(|project| project.units.get(unit_id.as_usize()))
        .map(|unit| unit.uri.as_ref())
}

fn arc_str_to_string(value: &Arc<str>) -> String {
    value.to_string()
}

fn namespace_name(namespace: Namespace) -> &'static str {
    match namespace {
        Namespace::Value => "value",
        Namespace::Type => "type",
        Namespace::Routine => "routine",
    }
}

fn routine_kind_name(kind: RoutineKind) -> &'static str {
    match kind {
        RoutineKind::GlobalDeclarations => "global_declarations",
        RoutineKind::Method => "method",
        RoutineKind::Form => "form",
        RoutineKind::Module => "module",
        RoutineKind::EventBlock => "event_block",
    }
}

fn scope_kind_name(kind: ScopeKind) -> &'static str {
    match kind {
        ScopeKind::File => "file",
        ScopeKind::Form => "form",
        ScopeKind::Module => "module",
        ScopeKind::EventBlock => "event_block",
        ScopeKind::Class => "class",
        ScopeKind::Interface => "interface",
        ScopeKind::Method => "method",
        ScopeKind::IfBranch => "if_branch",
        ScopeKind::ElseifBranch => "elseif_branch",
        ScopeKind::ElseBranch => "else_branch",
        ScopeKind::WhenBranch => "when_branch",
        ScopeKind::CatchClause => "catch_clause",
        ScopeKind::CleanupClause => "cleanup_clause",
        ScopeKind::WhileBlock => "while_block",
        ScopeKind::DoBlock => "do_block",
        ScopeKind::LoopBlock => "loop_block",
        ScopeKind::AtBlock => "at_block",
        ScopeKind::TryBlock => "try_block",
        ScopeKind::SelectBlock => "select_block",
    }
}

fn symbol_kind_name(kind: SymbolKind) -> &'static str {
    match kind {
        SymbolKind::BuiltinType => "builtin_type",
        SymbolKind::BuiltinRoutine => "builtin_routine",
        SymbolKind::BuiltinConstant => "builtin_constant",
        SymbolKind::BuiltinVariable => "builtin_variable",
        SymbolKind::Variable => "variable",
        SymbolKind::Constant => "constant",
        SymbolKind::EnumMember => "enum_member",
        SymbolKind::TypeDef => "type_def",
        SymbolKind::FieldSymbol => "field_symbol",
        SymbolKind::Form => "form",
        SymbolKind::Parameter => "parameter",
        SymbolKind::Class => "class",
        SymbolKind::Interface => "interface",
        SymbolKind::Method => "method",
        SymbolKind::Field => "field",
        SymbolKind::Include => "include",
        SymbolKind::Event => "event",
        SymbolKind::Module => "module",
        SymbolKind::Control => "control",
        SymbolKind::Report => "report",
    }
}

fn reference_kind_name(kind: ReferenceKind) -> &'static str {
    match kind {
        ReferenceKind::Identifier => "identifier",
        ReferenceKind::TypeRef => "type_ref",
        ReferenceKind::StructuredDeclEnd => "structured_decl_end",
        ReferenceKind::MessageClass => "message_class",
        ReferenceKind::RoutineCall => "routine_call",
        ReferenceKind::StaticTarget => "static_target",
        ReferenceKind::Include => "include",
    }
}

fn expression_fact_kind_name(kind: ExpressionFactKind) -> &'static str {
    match kind {
        ExpressionFactKind::Reference => "reference",
        ExpressionFactKind::Selector => "selector",
        ExpressionFactKind::CallResult => "call_result",
    }
}

fn system_field_statement_kind_name(kind: SystemFieldStatementKind) -> &'static str {
    match kind {
        SystemFieldStatementKind::Append => "append",
        SystemFieldStatementKind::Assign => "assign",
        SystemFieldStatementKind::AuthorityCheck => "authority_check",
        SystemFieldStatementKind::CallFunction => "call_function",
        SystemFieldStatementKind::Convert => "convert",
        SystemFieldStatementKind::DeleteReport => "delete_report",
        SystemFieldStatementKind::DeleteTable => "delete_table",
        SystemFieldStatementKind::DeleteDbTable => "delete_db_table",
        SystemFieldStatementKind::DescribeTable => "describe_table",
        SystemFieldStatementKind::Do => "do",
        SystemFieldStatementKind::Find => "find",
        SystemFieldStatementKind::InsertReport => "insert_report",
        SystemFieldStatementKind::InsertTable => "insert_table",
        SystemFieldStatementKind::InsertDbTable => "insert_db_table",
        SystemFieldStatementKind::InsertTextpool => "insert_textpool",
        SystemFieldStatementKind::LoopAt => "loop_at",
        SystemFieldStatementKind::Message => "message",
        SystemFieldStatementKind::ModifyTable => "modify_table",
        SystemFieldStatementKind::ModifyDbTable => "modify_db_table",
        SystemFieldStatementKind::ReadReport => "read_report",
        SystemFieldStatementKind::ReadTable => "read_table",
        SystemFieldStatementKind::Search => "search",
        SystemFieldStatementKind::Select => "select",
        SystemFieldStatementKind::SyntaxCheck => "syntax_check",
        SystemFieldStatementKind::UpdateDbTable => "update_db_table",
        SystemFieldStatementKind::While => "while",
    }
}

fn value_flow_kind_name(kind: ValueFlowKind) -> &'static str {
    match kind {
        ValueFlowKind::Assignment => "assignment",
        ValueFlowKind::CallArgument => "call_argument",
        ValueFlowKind::FieldSymbolAssignment => "field_symbol_assignment",
        ValueFlowKind::ConditionalFieldSymbolAssignment => "conditional_field_symbol_assignment",
    }
}

fn diagnostic_kind_name(kind: DiagnosticKind) -> &'static str {
    match kind {
        DiagnosticKind::DuplicateDeclaration => "duplicate_declaration",
        DiagnosticKind::ShadowedSymbol => "shadowed_symbol",
        DiagnosticKind::MismatchedStructuredDeclaration => "mismatched_structured_declaration",
        DiagnosticKind::UnresolvedReference => "unresolved_reference",
        DiagnosticKind::UnresolvedInclude => "unresolved_include",
        DiagnosticKind::IncludeCycle => "include_cycle",
        DiagnosticKind::WrongNamespace => "wrong_namespace",
        DiagnosticKind::UnknownField => "unknown_field",
        DiagnosticKind::InvalidBuiltinNamedArgument => "invalid_builtin_named_argument",
        DiagnosticKind::InvalidPerformCall => "invalid_perform_call",
        DiagnosticKind::AbstractClassInstantiation => "abstract_class_instantiation",
        DiagnosticKind::MissingMethodImplementation => "missing_method_implementation",
        DiagnosticKind::MissingSuperConstructorCall => "missing_super_constructor_call",
        DiagnosticKind::InvalidObjectTypeReference => "invalid_object_type_reference",
        DiagnosticKind::InvalidParameterType => "invalid_parameter_type",
        DiagnosticKind::IncompatibleAssignmentType => "incompatible_assignment_type",
        DiagnosticKind::IncompatibleArgumentType => "incompatible_argument_type",
        DiagnosticKind::UnknownNamedParameter => "unknown_named_parameter",
        DiagnosticKind::UnknownFunctionModuleException => "unknown_function_module_exception",
        DiagnosticKind::DuplicateNamedParameter => "duplicate_named_parameter",
        DiagnosticKind::MissingRequiredParameter => "missing_required_parameter",
        DiagnosticKind::UnverifiedOpenSqlSource => "unverified_open_sql_source",
        DiagnosticKind::InvalidOpenSqlIntoTarget => "invalid_open_sql_into_target",
        DiagnosticKind::InvalidOpenSqlSyntax => "invalid_open_sql_syntax",
        DiagnosticKind::InvalidMessage => "invalid_message",
        DiagnosticKind::InvalidConstructorForIteratorReuse => {
            "invalid_constructor_for_iterator_reuse"
        }
        DiagnosticKind::MissingTablesDeclaration => "missing_tables_declaration",
        DiagnosticKind::UnreachableCode => "unreachable_code",
        DiagnosticKind::UseBeforeDefiniteAssignment => "use_before_definite_assignment",
        DiagnosticKind::PossiblyUnboundFieldSymbol => "possibly_unbound_field_symbol",
        DiagnosticKind::DeadStore => "dead_store",
        DiagnosticKind::UnsortedReadTableBinarySearch => "unsorted_read_table_binary_search",
    }
}

fn static_analysis_finding_kind_name(kind: StaticAnalysisFindingKind) -> &'static str {
    match kind {
        StaticAnalysisFindingKind::UnreachableCode => "unreachable_code",
        StaticAnalysisFindingKind::UseBeforeDefiniteAssignment => "use_before_definite_assignment",
        StaticAnalysisFindingKind::PossiblyUnboundFieldSymbol => "possibly_unbound_field_symbol",
        StaticAnalysisFindingKind::DeadStore => "dead_store",
    }
}

fn visibility_name(visibility: Visibility) -> &'static str {
    match visibility {
        Visibility::Public => "public",
        Visibility::Protected => "protected",
        Visibility::Private => "private",
    }
}

fn class_member_kind_name(kind: ClassMemberKind) -> &'static str {
    match kind {
        ClassMemberKind::Attribute => "attribute",
        ClassMemberKind::Method => "method",
        ClassMemberKind::Event => "event",
    }
}

fn method_parameter_section_name(section: MethodParameterSection) -> &'static str {
    match section {
        MethodParameterSection::Importing => "importing",
        MethodParameterSection::Exporting => "exporting",
        MethodParameterSection::Changing => "changing",
        MethodParameterSection::Receiving => "receiving",
        MethodParameterSection::Returning => "returning",
    }
}

fn function_module_parameter_section_name(section: FunctionModuleParameterSection) -> &'static str {
    match section {
        FunctionModuleParameterSection::Importing => "importing",
        FunctionModuleParameterSection::Exporting => "exporting",
        FunctionModuleParameterSection::Changing => "changing",
        FunctionModuleParameterSection::Tables => "tables",
    }
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

fn perform_parameter_section_name(section: PerformParameterSection) -> &'static str {
    match section {
        PerformParameterSection::Tables => "tables",
        PerformParameterSection::Using => "using",
        PerformParameterSection::Changing => "changing",
    }
}

fn sql_source_kind_name(kind: SqlSourceKind) -> &'static str {
    match kind {
        SqlSourceKind::From => "from",
        SqlSourceKind::Join => "join",
    }
}

fn sql_dynamic_fragment_kind_name(kind: SqlDynamicFragmentKind) -> &'static str {
    match kind {
        SqlDynamicFragmentKind::Source => "source",
        SqlDynamicFragmentKind::Projection => "projection",
        SqlDynamicFragmentKind::Where => "where",
    }
}

fn sql_projection_kind_name(kind: SqlProjectionKind) -> &'static str {
    match kind {
        SqlProjectionKind::Star => "star",
        SqlProjectionKind::QualifiedStar => "qualified_star",
        SqlProjectionKind::Column => "column",
        SqlProjectionKind::Aggregate => "aggregate",
        SqlProjectionKind::Expression => "expression",
    }
}

fn sql_name_ref_kind_name(kind: SqlNameRefKind) -> &'static str {
    match kind {
        SqlNameRefKind::Source => "source",
        SqlNameRefKind::Alias => "alias",
        SqlNameRefKind::Column => "column",
        SqlNameRefKind::QualifiedColumn => "qualified_column",
        SqlNameRefKind::Star => "star",
        SqlNameRefKind::QualifiedStar => "qualified_star",
        SqlNameRefKind::Aggregate => "aggregate",
    }
}

fn sql_predicate_kind_name(kind: SqlPredicateKind) -> &'static str {
    match kind {
        SqlPredicateKind::Where => "where",
        SqlPredicateKind::JoinOn => "join_on",
        SqlPredicateKind::Having => "having",
        SqlPredicateKind::DynamicWhere => "dynamic_where",
        SqlPredicateKind::ForAllEntries => "for_all_entries",
    }
}

fn sql_target_kind_name(kind: SqlTargetKind) -> &'static str {
    match kind {
        SqlTargetKind::Into => "into",
        SqlTargetKind::Appending => "appending",
    }
}

fn sql_resolution_name(resolution: SqlResolution) -> &'static str {
    match resolution {
        SqlResolution::Unresolved => "unresolved",
        SqlResolution::External => "external",
        SqlResolution::LocalCte => "local_cte",
        SqlResolution::InternalTable => "internal_table",
        SqlResolution::Hierarchy => "hierarchy",
    }
}
