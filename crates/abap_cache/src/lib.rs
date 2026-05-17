use std::collections::{HashMap, HashSet};
use std::ops::Range;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};

use abap_ast::SyntaxKind;
use abap_ast::ast::{
    AstNode, ConstructorCorrespondingMappingAssignment, ConstructorExpr, SyntaxNodeRef,
};
use abap_lexer::{TokenKind, tokenize};
pub use abap_lints::{
    ABAP_LSP_DEAD_STORE, ABAP_LSP_DYNAMIC_OPEN_SQL, ABAP_LSP_FOR_ALL_ENTRIES_WITHOUT_GUARD,
    ABAP_LSP_IGNORED_AUTHORITY_CHECK, ABAP_LSP_IGNORED_CALL_FUNCTION_RESULT,
    ABAP_LSP_POSSIBLY_UNBOUND_FIELD_SYMBOL, ABAP_LSP_SELECT_IN_LOOP,
    ABAP_LSP_SELECT_SINGLE_WITHOUT_FULL_KEY, ABAP_LSP_SELECT_STAR, ABAP_LSP_UNREACHABLE_CODE,
    ABAP_LSP_UNSORTED_READ_TABLE_BINARY_SEARCH, ABAP_LSP_USE_BEFORE_DEFINITE_ASSIGNMENT,
    EPC_INVALID_OPEN_SQL_INTO_TARGET, EPC_MISSING_TABLES_DECLARATION,
    EPC_UNVERIFIED_OPEN_SQL_SOURCE, LintDiagnostic, LintGroup, LintId, LintLevel, LintMetadata,
    LintOrigin, LintPolicy, LintSuppression, LintSuppressionKind, ProjectLintAnalysis,
    SapAtcLintConfig, SapAtcLintMode, SuppressionIndex, lint_docs_anchor, metadata_for, registry,
};
use abap_parser::{ParseResult, parse};
use abap_symbols::{
    CallArgumentData, CallSiteData, ClassMemberData, ClassMemberKind, ClassMemberParameterData,
    Diagnostic, DiagnosticKind, FieldTypeRefData, FormParameterData, FormParameterPassingKind,
    FormParameterSection, FormRoutineData, FunctionModuleData, FunctionModuleExceptionData,
    FunctionModuleParameterData, FunctionModuleParameterSection, MethodParameterSection,
    NamedArgumentAccess, NamedArgumentSection, NamedArgumentTarget, Namespace, PerformArgumentData,
    PerformCallData, PerformParameterSection, ProjectAnalysis, ProjectRoutineAnalysis,
    ProjectStaticAnalysisSummary, ReferenceData, ReferenceKind, Resolution,
    RoutineControlRegionData, RoutineLoopKind, RoutineSiteKind, ScopeId, ScopeKind,
    SqlDynamicFragmentKind, SqlNameRefData, SqlNameRefKind, SqlProjectionKind, SqlQueryData,
    SqlSourceData, SqlSourceKind, StructureFieldData, StructureFieldInfo, StructureFieldShape,
    StructureId, SymbolData, SymbolHandle, SymbolId, SymbolKind, SystemFieldStatementKind,
    SystemFieldUpdateData, UnitAnalysis, UnitId, ValueStateCheckData, ValueStateCheckKind,
    Visibility, build_project_routine_analysis, build_project_routine_analysis_for_units,
    build_project_static_analysis_summary, builtin_routine_spec, call_section_matches_parameter,
    parameter_is_required,
    perf_api::{
        IncrementalProjectUpdate, LocalAnalysis, analyze_unit_local_state,
        analyze_unit_local_state_for_project_build, incremental_project_update,
    },
};
use parking_lot::RwLock;
use rayon::prelude::*;

mod call_dataflow;
mod call_graph;
mod callable_summary;
mod effective_source;
mod keyword_completion;
mod workspace;
pub use call_dataflow::{
    CallDataflowByteRange, CallDataflowFieldMapping, CallDataflowLifecycle,
    CallDataflowLifecycleEdge, CallDataflowLifecycleNode, CallDataflowMatch,
    CallDataflowParameterTrace, CallDataflowProvenanceEdge, CallDataflowProvenanceGraph,
    CallDataflowProvenanceNode, CallDataflowQuery, CallDataflowSelectedCall, CallDataflowSummary,
    CallDataflowTrace, build_call_dataflow_trace,
};
pub use call_graph::{
    CallGraphEdge, CallGraphEdgeKind, CallGraphNode, CallGraphNodeKind, CallGraphResolutionStatus,
    ProjectCallGraph,
};
pub use callable_summary::{
    CallableParameterDirection, CallableParameterSummary, CallableSummary,
    ProjectCallableSummaryAnalysis, ProjectCallableSummaryMetrics,
};
pub use effective_source::{
    EffectiveSource, EffectiveSourceDiagnostic, EffectiveSourceLimits, EffectiveSourceSegment,
    EffectiveSourceUnit, build_effective_source, build_effective_source_with_limits,
};
pub use workspace::{
    DEFAULT_REMOTE_REQUESTS_PER_SECOND, DEPENDENCY_MODE_REMOTE_ON_DEMAND,
    EDITOR_FIRST_DEPENDENCY_MEMBER_THRESHOLD, EDITOR_FIRST_MANIFEST_BYTES_THRESHOLD,
    EDITOR_FIRST_UNIT_COUNT_THRESHOLD, LocalDependencySourceMode, LocalExportConfig,
    LocalExportResolveProfile, LocalExportResolver, ManifestDiagnostic, ManifestPerformance,
    ManifestResolution, ManifestTextRange, ManifestUnit, ManifestUnitDependencyOf,
    ManifestUnitMember, OpenDocumentOverlay, WORKSPACE_PERFORMANCE_MODE_AUTO,
    WORKSPACE_PERFORMANCE_MODE_EDITOR_FIRST, WORKSPACE_PERFORMANCE_MODE_FULL_WORKSPACE,
    WorkspaceDocument, WorkspaceLoadResult, WorkspaceManifest, WorkspacePerformanceMode,
    ddic_xml_to_abap_source, file_uri_to_path, is_remote_lookup_candidate,
    is_remote_lookup_candidate_after_local_resolution, is_remote_lookup_name,
    load_effective_manifest_from_workspace_result, load_manifest_diagnostics_from_workspace,
    load_manifest_from_workspace, load_manifest_from_workspace_result, load_workspace_documents,
    load_workspace_documents_with_progress, local_export_candidate_kind_for_reference,
    local_export_config_for_source, manifest_declares_uri, manifest_diagnostics_for_manifest_text,
    manifest_document_metadata, manifest_supports_remote_resolution, normalize_dependency_mode,
    normalize_workspace_performance_mode, path_to_file_uri,
    resolve_local_export_dependency_document, resolve_local_export_dependency_document_profiled,
    resolve_local_export_dependency_documents, resolve_local_export_dependency_documents_profiled,
    resolve_local_export_function_module_documents_by_prefix, resolve_workspace_performance_mode,
    uri_starts_with_workspace, workspace_relative_path,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalysisSnapshot {
    pub uri: Arc<str>,
    pub version: i32,
    pub text: Arc<str>,
    pub line_index: Arc<LineIndex>,
    pub project_texts: Arc<HashMap<Arc<str>, Arc<str>>>,
    pub is_dependency: bool,
    pub object_name: Option<Arc<str>>,
    pub parse: Arc<ParseResult>,
    pub symbols: Arc<UnitAnalysis>,
    pub project: Arc<ProjectAnalysis>,
    pub routine_analysis: Arc<ProjectRoutineAnalysis>,
    pub lint_analysis: Arc<ProjectLintAnalysis>,
    pub static_analysis: Option<Arc<ProjectStaticAnalysisSummary>>,
    pub callable_summaries: Arc<ProjectCallableSummaryAnalysis>,
    pub call_graph: Arc<ProjectCallGraph>,
    pub scope_index: Arc<ScopeIndex>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineIndex {
    line_starts: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocumentInput {
    pub uri: Arc<str>,
    pub version: i32,
    pub text: Arc<str>,
    pub is_dependency: bool,
    pub object_name: Option<Arc<str>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DependencyDiagnosticsMode {
    All,
    EditableAndIncludes,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SnapshotBuildPlan {
    pub routine_analysis: bool,
    pub static_analysis: bool,
    pub call_graph: bool,
    pub callable_summaries: bool,
    pub lint_analysis: bool,
    pub dependency_diagnostics: DependencyDiagnosticsMode,
}

impl SnapshotBuildPlan {
    pub const FULL: Self = Self {
        routine_analysis: true,
        static_analysis: true,
        call_graph: true,
        callable_summaries: true,
        lint_analysis: true,
        dependency_diagnostics: DependencyDiagnosticsMode::All,
    };

    pub const SEMANTIC_DOSSIER: Self = Self {
        routine_analysis: true,
        static_analysis: true,
        call_graph: false,
        callable_summaries: false,
        lint_analysis: true,
        dependency_diagnostics: DependencyDiagnosticsMode::All,
    };

    pub const EFFECTIVE_SOURCE: Self = Self {
        routine_analysis: false,
        static_analysis: false,
        call_graph: false,
        callable_summaries: false,
        lint_analysis: true,
        dependency_diagnostics: DependencyDiagnosticsMode::All,
    };

    pub const REMOTE_CANDIDATES: Self = Self::EFFECTIVE_SOURCE;

    pub const EDITOR_WORKSPACE: Self = Self {
        routine_analysis: true,
        static_analysis: false,
        call_graph: false,
        callable_summaries: false,
        lint_analysis: true,
        dependency_diagnostics: DependencyDiagnosticsMode::EditableAndIncludes,
    };

    pub const CALL_GRAPH: Self = Self {
        routine_analysis: false,
        static_analysis: false,
        call_graph: true,
        callable_summaries: false,
        lint_analysis: true,
        dependency_diagnostics: DependencyDiagnosticsMode::All,
    };

    pub const CALL_DATAFLOW: Self = Self {
        routine_analysis: true,
        static_analysis: false,
        call_graph: true,
        callable_summaries: true,
        lint_analysis: true,
        dependency_diagnostics: DependencyDiagnosticsMode::All,
    };

    pub const fn normalized(self) -> Self {
        Self {
            routine_analysis: self.routine_analysis
                || self.static_analysis
                || self.callable_summaries,
            static_analysis: self.static_analysis,
            call_graph: self.call_graph || self.callable_summaries,
            callable_summaries: self.callable_summaries,
            lint_analysis: self.lint_analysis,
            dependency_diagnostics: self.dependency_diagnostics,
        }
    }
}

impl Default for SnapshotBuildPlan {
    fn default() -> Self {
        Self::FULL
    }
}

#[derive(Debug, Clone)]
struct StagedDocument {
    uri: Arc<str>,
    version: i32,
    text: Arc<str>,
    is_dependency: bool,
    object_name: Option<Arc<str>>,
    previous: Option<Arc<AnalysisSnapshot>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HoveredComponentKind {
    Scalar,
    Structured { structure_name: Arc<str> },
    Attribute,
    Method,
    Interface,
    Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HoveredComponentInfo {
    pub base_name: Arc<str>,
    pub base_namespace: Namespace,
    pub component_path: Vec<Arc<str>>,
    pub field_name: Arc<str>,
    /// Structure that directly contains this field (for example `syst` for `sy-subrc`).
    pub field_owner_structure_name: Option<Arc<str>>,
    pub range: Range<usize>,
    pub declared_type: Option<String>,
    pub description: Option<String>,
    pub value_clause_display: Option<Arc<str>>,
    pub declaration: Option<String>,
    pub kind: HoveredComponentKind,
    pub is_static_method: bool,
    pub in_type_position: bool,
}

/// Hover payload for a resolved reference or declaration at a byte offset (LSP-agnostic).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HoveredSymbolInfo {
    pub range: Range<usize>,
    pub display_name: Arc<str>,
    pub markdown_lines: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParameterInlayHintInfo {
    pub position: usize,
    pub label: Arc<str>,
    pub trailing_colon: bool,
    pub padding_left: bool,
    pub padding_right: bool,
    pub tooltip_markdown: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeInlayHintInfo {
    pub position: usize,
    pub label: Arc<str>,
    pub tooltip_markdown: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefinitionTarget {
    pub uri: Arc<str>,
    pub range: Range<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MissingMethodImplementationAction {
    pub title: String,
    pub edit_range: Range<usize>,
    pub new_text: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodParameterCommentsAction {
    pub title: String,
    pub edit_range: Range<usize>,
    pub new_text: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReferenceTarget {
    pub uri: Arc<str>,
    pub range: Range<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RenamePlan {
    pub range: Range<usize>,
    pub placeholder: String,
    pub locations: Vec<ReferenceTarget>,
}

impl RenamePlan {
    pub fn validate_new_name(&self, new_name: &str) -> Result<(), String> {
        validate_rename_identifier(self.placeholder.as_str(), new_name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectorCompletionItem {
    pub name: Arc<str>,
    pub declared_type: Option<String>,
    pub declaration: Option<String>,
    pub kind: HoveredComponentKind,
    pub field_owner_structure_name: Option<Arc<str>>,
    pub insertion: CompletionInsertion,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectorCompletionInfo {
    pub replace_range: Range<usize>,
    pub items: Vec<SelectorCompletionItem>,
    pub in_type_position: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompletionInsertion {
    pub plain_text: String,
    pub snippet_text: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamedArgumentCompletionItem {
    pub name: Arc<str>,
    pub declared_type: Option<String>,
    pub declaration: Option<String>,
    pub insertion: CompletionInsertion,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolCompletionItem {
    pub name: Arc<str>,
    pub kind: SymbolKind,
    pub declared_type: Option<String>,
    pub declaration: Option<String>,
    pub insertion: CompletionInsertion,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TemplateCompletionItem {
    pub name: Arc<str>,
    pub detail: Option<String>,
    pub insertion: CompletionInsertion,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallableCompletionKind {
    FunctionModule,
    Form,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallableCompletionItem {
    pub name: Arc<str>,
    pub declaration: Option<String>,
    pub kind: CallableCompletionKind,
    pub insertion: CompletionInsertion,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KeywordCompletionItem {
    pub name: Arc<str>,
    pub insertion: CompletionInsertion,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompletionItem {
    Selector(SelectorCompletionItem),
    NamedArgument(NamedArgumentCompletionItem),
    Symbol(SymbolCompletionItem),
    Template(TemplateCompletionItem),
    Callable(CallableCompletionItem),
    Keyword(KeywordCompletionItem),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompletionInfo {
    pub replace_range: Range<usize>,
    pub items: Vec<CompletionItem>,
    pub in_type_position: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallableStatementCompletionContext {
    pub replace_range: Range<usize>,
    pub prefix: Arc<str>,
    pub kind: CallableCompletionKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SelectorCompletionQuery {
    scope: ScopeId,
    base_name: Arc<str>,
    base_namespace: Namespace,
    component_path: Vec<Arc<str>>,
    replace_range: Range<usize>,
    prefix: Arc<str>,
    in_type_position: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct OpenSqlFieldCompletionQuery {
    scope: ScopeId,
    source_name: Arc<str>,
    replace_range: Range<usize>,
    prefix: Arc<str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct OpenSqlSourceCompletionQuery {
    scope: ScopeId,
    replace_range: Range<usize>,
    prefix: Arc<str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SelectorCursorContext {
    range: Range<usize>,
    in_type_position: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct BareWhereFieldQuery {
    scope: ScopeId,
    structure_unit_id: UnitId,
    structure_id: StructureId,
    replace_range: Range<usize>,
    prefix: Arc<str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CallableStatementCompletionQuery {
    scope: ScopeId,
    replace_range: Range<usize>,
    prefix: Arc<str>,
    kind: CallableCompletionKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TemplateCompletionQuery {
    replace_range: Range<usize>,
    class_name_hint: Arc<str>,
    kind: LocalClassTemplateKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct BareIdentifierCompletionContext {
    replace_range: Range<usize>,
    prefix: Arc<str>,
    in_type_position: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct MethodDefinitionTemplateQuery {
    replace_range: Range<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TypesBeginTemplateQuery {
    replace_range: Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LocalClassTemplateKind {
    Standard,
    Test,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct InferredFunctionModuleCallTemplate {
    sections: Vec<(NamedArgumentSection, Vec<Arc<str>>)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct BareWhereFieldTarget {
    structure_unit_id: UnitId,
    field: StructureFieldInfo,
    range: Range<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct InferredDdicFieldTarget {
    field_name: Arc<str>,
    field_owner_structure_name: Option<Arc<str>>,
    declared_type_name: Arc<str>,
    definition: DefinitionTarget,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ResolvedSqlFieldTarget {
    range: Range<usize>,
    source_name: Arc<str>,
    source_alias: Option<Arc<str>>,
    field: StructureFieldInfo,
    field_owner_structure_name: Arc<str>,
    description: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ReferenceSearchTarget {
    Symbol(abap_symbols::SymbolHandle),
    ClassMember {
        unit: UnitId,
        class_symbol: SymbolId,
        name: Arc<str>,
    },
    StructField {
        unit: UnitId,
        owner: StructureId,
        name: Arc<str>,
    },
    /// DDIC-style type name shared between `TYPE ...` references and Open SQL `FROM` sources.
    DdLikeTypeName {
        unit: UnitId,
        name: Arc<str>,
    },
}

fn message_class_entries<'a>(
    project: &'a ProjectAnalysis,
    class_name: &str,
) -> Vec<&'a abap_symbols::MessageClassEntryData> {
    let class_name = class_name.to_ascii_lowercase();
    let mut entries = project
        .units
        .iter()
        .flat_map(|unit| unit.message_class_entries.iter())
        .filter(|entry| {
            entry
                .class_name
                .as_ref()
                .eq_ignore_ascii_case(class_name.as_str())
        })
        .collect::<Vec<_>>();
    entries.sort_by(|left, right| left.id.cmp(&right.id).then(left.text.cmp(&right.text)));
    entries.dedup_by(|left, right| left.id == right.id && left.text == right.text);
    entries
}

fn message_class_entry<'a>(
    project: &'a ProjectAnalysis,
    class_name: &str,
    id: &str,
) -> Option<&'a abap_symbols::MessageClassEntryData> {
    message_class_entries(project, class_name)
        .into_iter()
        .find(|entry| entry.id.as_ref() == id)
}

fn hovered_message_class(
    snapshot: &AnalysisSnapshot,
    class_name: &Arc<str>,
    range: Range<usize>,
) -> HoveredSymbolInfo {
    HoveredSymbolInfo {
        range,
        display_name: Arc::clone(class_name),
        markdown_lines: markdown_lines_for_message_class(snapshot.project.as_ref(), class_name),
    }
}

fn markdown_lines_for_message_class(project: &ProjectAnalysis, class_name: &str) -> Vec<String> {
    let entries = message_class_entries(project, class_name);
    let mut lines = vec![format!("Message class `{class_name}`")];
    if entries.is_empty() {
        return lines;
    }
    lines.push(String::new());
    lines.push("Messages:".to_string());
    for entry in entries {
        lines.push(format!("`{}` {}", entry.id, entry.text));
    }
    lines
}

fn markdown_lines_for_message_entry(entry: &abap_symbols::MessageClassEntryData) -> Vec<String> {
    vec![
        format!("Message `{}` in class `{}`", entry.id, entry.class_name),
        String::new(),
        entry.text.to_string(),
    ]
}

fn markdown_lines_for_sql_name_ref(
    snapshot: &AnalysisSnapshot,
    sql_ref: &SqlNameRefData,
) -> Vec<String> {
    let title = match sql_ref.kind {
        SqlNameRefKind::Source => "Open SQL data source (DDIC object)",
        SqlNameRefKind::Alias => "Open SQL alias",
        SqlNameRefKind::Column => "Open SQL column",
        SqlNameRefKind::QualifiedColumn => "Open SQL column",
        SqlNameRefKind::Star => "Open SQL `*` projection",
        SqlNameRefKind::QualifiedStar => "Open SQL qualified `*` projection",
        SqlNameRefKind::Aggregate => "Open SQL aggregate",
        SqlNameRefKind::Function => "Open SQL function",
    };
    let mut lines = vec![format!("`{}`", sql_ref.name), title.to_string()];
    if let Some(qual) = sql_ref.qualifier.as_ref() {
        lines.push(format!("Table alias `{}`", qual));
    }
    if matches!(sql_ref.kind, SqlNameRefKind::Source) {
        if let Some(target) = snapshot.definition_target_for_sql_name_ref_at(sql_ref.range.start)
            && let Some(description) =
                description_for_definition_target(snapshot, &target).filter(|text| !text.is_empty())
        {
            lines.push(description);
            return lines;
        }
        lines.push(
            "The analyzer emits a warning until the source is verified against SAP DDIC/repository (not connected in this build). Use SAP ADT or the VS Code remote dependency fetch for metadata."
                .to_string(),
        );
        return lines;
    }
    if matches!(
        sql_ref.kind,
        SqlNameRefKind::Column | SqlNameRefKind::QualifiedColumn
    ) && let Some(target) = snapshot.sql_field_target_at(sql_ref.range.start)
    {
        if let Some(description) = target.description {
            lines.push(description);
        }
        if let Some(type_ref) = &target.field.type_ref {
            lines.push(format_hover_type_clause(&format_field_type_ref(type_ref)));
        }
        let mut source_line = format!("column of `{}`", target.field_owner_structure_name);
        if let Some(alias) = target.source_alias.as_ref()
            && alias.as_ref() != target.source_name.as_ref()
        {
            source_line.push_str(&format!(" via alias `{}`", alias));
        }
        lines.push(source_line);
    }
    lines
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct NamedArgumentParameterInfo {
    name: Arc<str>,
    declared_type: Option<FieldTypeRefData>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FormParameterHoverInfo {
    form_name: Arc<str>,
    name: Arc<str>,
    section: FormParameterSection,
    passing: FormParameterPassingKind,
    declared_type: Option<FieldTypeRefData>,
}

type ScopeIndex = Vec<HashMap<(Namespace, Arc<str>), Vec<SymbolId>>>;

fn build_scope_index(unit: &UnitAnalysis) -> ScopeIndex {
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

pub struct SemanticTokenLookupContext<'a> {
    snapshot: &'a AnalysisSnapshot,
    scope_index: &'a ScopeIndex,
}

impl AnalysisSnapshot {
    pub fn scope_index(&self) -> &ScopeIndex {
        self.scope_index.as_ref()
    }

    pub fn project_text(&self, uri: &str) -> Option<&str> {
        self.project_texts.get(uri).map(|text| text.as_ref())
    }

    pub fn call_graph(&self) -> &ProjectCallGraph {
        self.call_graph.as_ref()
    }

    pub fn routine_analysis(&self) -> &ProjectRoutineAnalysis {
        self.routine_analysis.as_ref()
    }

    pub fn lint_analysis(&self) -> &ProjectLintAnalysis {
        self.lint_analysis.as_ref()
    }

    pub fn lint_diagnostics(&self) -> &[LintDiagnostic] {
        self.lint_analysis.diagnostics_for_uri(self.uri.as_ref())
    }

    pub fn static_analysis(&self) -> Option<&ProjectStaticAnalysisSummary> {
        self.static_analysis.as_deref()
    }

    pub fn static_analysis_findings_touching_offset(
        &self,
        offset: usize,
    ) -> Vec<&abap_symbols::StaticAnalysisFinding> {
        self.static_analysis()
            .into_iter()
            .flat_map(|summary| summary.findings_touching_offset(self.symbols.unit_id, offset))
            .collect()
    }

    pub fn callable_summaries(&self) -> &ProjectCallableSummaryAnalysis {
        self.callable_summaries.as_ref()
    }

    pub fn semantic_token_lookup_context(&self) -> SemanticTokenLookupContext<'_> {
        SemanticTokenLookupContext {
            snapshot: self,
            scope_index: self.scope_index.as_ref(),
        }
    }

    pub fn offset_to_line_utf16_position(&self, offset: usize) -> Option<(u32, u32)> {
        self.line_index
            .offset_to_line_utf16_position(self.text.as_ref(), offset)
    }

    pub fn line_utf16_position_to_offset(&self, line: u32, character: u32) -> Option<usize> {
        self.line_index
            .line_utf16_position_to_offset(self.text.as_ref(), line, character)
    }

    pub fn structure_field_infos(&self, structure_id: StructureId) -> Vec<StructureFieldInfo> {
        self.symbols
            .semantic()
            .decls()
            .structure_field_infos(structure_id)
    }

    pub fn structure_field_info(
        &self,
        structure_id: StructureId,
        field_name: &str,
    ) -> Option<StructureFieldInfo> {
        self.symbols
            .semantic()
            .decls()
            .structure_field_info(structure_id, field_name)
    }

    pub fn resolve_structure_field_path(
        &self,
        structure_id: StructureId,
        field_path: &[&str],
    ) -> Option<StructureFieldInfo> {
        self.symbols
            .semantic()
            .decls()
            .resolve_structure_field_path(structure_id, field_path)
    }

    pub fn symbol_structure_field_infos(
        &self,
        symbol_id: SymbolId,
    ) -> Option<Vec<StructureFieldInfo>> {
        let structure_id = self.symbols.symbol(symbol_id).structure?;
        Some(self.structure_field_infos(structure_id))
    }

    pub fn resolve_symbol_field_path(
        &self,
        symbol_id: SymbolId,
        field_path: &[&str],
    ) -> Option<StructureFieldInfo> {
        let structure_id = self.symbols.symbol(symbol_id).structure?;
        self.resolve_structure_field_path(structure_id, field_path)
    }

    pub fn hovered_component_at(&self, offset: usize) -> Option<HoveredComponentInfo> {
        if let Some((access, segment_index)) =
            self.symbols.field_accesses.iter().find_map(|access| {
                access
                    .field_path
                    .iter()
                    .enumerate()
                    .find_map(|(idx, segment)| {
                        (segment.range.start <= offset && offset < segment.range.end)
                            .then_some((access, idx))
                    })
            })
        {
            let (unit, symbol_id) = resolve_field_access_base_symbol(self, access)?;
            if segment_index == 0
                && let Some((interface_unit, interface_symbol)) =
                    resolve_interface_selector_qualifier_with_scope_index(
                        self,
                        self.scope_index(),
                        access,
                        unit,
                        symbol_id,
                    )
            {
                return Some(HoveredComponentInfo {
                    base_name: Arc::clone(&access.base_name),
                    base_namespace: access.base_namespace,
                    component_path: vec![Arc::clone(&access.field_path[0].name)],
                    field_name: Arc::clone(&access.field_path[0].name),
                    field_owner_structure_name: None,
                    range: access.field_path[0].range.clone(),
                    declared_type: None,
                    description: None,
                    value_clause_display: None,
                    declaration: Some(format!(
                        "INTERFACE {}",
                        interface_unit.symbol(interface_symbol).name
                    )),
                    kind: HoveredComponentKind::Interface,
                    is_static_method: false,
                    in_type_position: access.in_type_position,
                });
            }
            if segment_index == 0
                && resolve_interface_selector_method_symbol(self, access, unit, symbol_id).is_some()
            {
                return Some(HoveredComponentInfo {
                    base_name: Arc::clone(&access.base_name),
                    base_namespace: access.base_namespace,
                    component_path: vec![Arc::clone(&access.field_path[0].name)],
                    field_name: Arc::clone(&access.field_path[0].name),
                    field_owner_structure_name: None,
                    range: access.field_path[0].range.clone(),
                    declared_type: None,
                    description: None,
                    value_clause_display: None,
                    declaration: Some(format!("INTERFACE {}", access.field_path[0].name)),
                    kind: HoveredComponentKind::Interface,
                    is_static_method: false,
                    in_type_position: access.in_type_position,
                });
            }
            if let Some((type_unit, type_symbol)) =
                resolve_class_selector_type_symbol_with_scope_index(
                    self,
                    self.scope_index(),
                    access,
                    segment_index,
                    unit,
                    symbol_id,
                )
            {
                return Some(HoveredComponentInfo {
                    base_name: Arc::clone(&access.base_name),
                    base_namespace: access.base_namespace,
                    component_path: access
                        .field_path
                        .iter()
                        .take(segment_index + 1)
                        .map(|segment| Arc::clone(&segment.name))
                        .collect(),
                    field_name: Arc::clone(&type_symbol.name),
                    field_owner_structure_name: None,
                    range: access.field_path[segment_index].range.clone(),
                    declared_type: symbol_selector_declared_type(type_unit, type_symbol),
                    description: None,
                    value_clause_display: None,
                    declaration: Some(format_selector_type_declaration(type_unit, type_symbol)),
                    kind: HoveredComponentKind::Type,
                    is_static_method: false,
                    in_type_position: access.in_type_position,
                });
            }
            if let Some((_, member)) =
                resolve_class_selector_member(self, access, segment_index, unit, symbol_id)
            {
                return Some(HoveredComponentInfo {
                    base_name: Arc::clone(&access.base_name),
                    base_namespace: access.base_namespace,
                    component_path: access
                        .field_path
                        .iter()
                        .take(segment_index + 1)
                        .map(|segment| Arc::clone(&segment.name))
                        .collect(),
                    field_name: Arc::clone(&member.name),
                    field_owner_structure_name: None,
                    range: access.field_path[segment_index].range.clone(),
                    declared_type: None,
                    description: None,
                    value_clause_display: None,
                    declaration: Some(format_class_member_signature(unit, member)),
                    kind: hovered_component_kind_for_class_member(member),
                    is_static_method: member.is_static,
                    in_type_position: access.in_type_position,
                });
            }
            if segment_index == 1
                && let Some((_, method_symbol)) =
                    resolve_interface_selector_method_symbol(self, access, unit, symbol_id)
            {
                return Some(HoveredComponentInfo {
                    base_name: Arc::clone(&access.base_name),
                    base_namespace: access.base_namespace,
                    component_path: access
                        .field_path
                        .iter()
                        .take(segment_index + 1)
                        .map(|segment| Arc::clone(&segment.name))
                        .collect(),
                    field_name: Arc::clone(&access.field_path[1].name),
                    field_owner_structure_name: None,
                    range: access.field_path[segment_index].range.clone(),
                    declared_type: None,
                    description: None,
                    value_clause_display: None,
                    declaration: Some(format!("METHOD {}", method_symbol.name)),
                    kind: HoveredComponentKind::Method,
                    is_static_method: false,
                    in_type_position: access.in_type_position,
                });
            }
            if let Some((owner_structure_name, kind, declared_type)) =
                resolve_well_known_external_field_access_segment(
                    unit,
                    access,
                    segment_index,
                    symbol_id,
                )
            {
                return Some(HoveredComponentInfo {
                    base_name: Arc::clone(&access.base_name),
                    base_namespace: access.base_namespace,
                    component_path: access
                        .field_path
                        .iter()
                        .take(segment_index + 1)
                        .map(|segment| Arc::clone(&segment.name))
                        .collect(),
                    field_name: Arc::clone(&access.field_path[segment_index].name),
                    field_owner_structure_name: Some(owner_structure_name),
                    range: access.field_path[segment_index].range.clone(),
                    declared_type: Some(format_field_type_ref(&declared_type)),
                    description: None,
                    value_clause_display: None,
                    declaration: Some("well-known external DDIC structure field".to_string()),
                    kind,
                    is_static_method: false,
                    in_type_position: access.in_type_position,
                });
            }
            let Some((field_unit, field)) = resolve_field_access_component_with_scope_index(
                self,
                self.scope_index(),
                access,
                segment_index,
                unit,
                symbol_id,
            ) else {
                let (structure_unit, structure_id) =
                    resolve_field_access_container_structure_with_scope_index(
                        self,
                        self.scope_index(),
                        access,
                        segment_index,
                        unit,
                        symbol_id,
                    )?;
                let inferred = inferred_ddic_data_element_target(
                    self,
                    structure_unit,
                    structure_id,
                    access.field_path[segment_index].name.as_ref(),
                )?;
                return Some(HoveredComponentInfo {
                    base_name: Arc::clone(&access.base_name),
                    base_namespace: access.base_namespace,
                    component_path: access
                        .field_path
                        .iter()
                        .take(segment_index + 1)
                        .map(|segment| Arc::clone(&segment.name))
                        .collect(),
                    field_name: Arc::clone(&inferred.field_name),
                    field_owner_structure_name: inferred.field_owner_structure_name,
                    range: access.field_path[segment_index].range.clone(),
                    declared_type: Some(format!("TYPE {}", inferred.declared_type_name)),
                    description: description_for_definition_target(self, &inferred.definition),
                    value_clause_display: None,
                    declaration: Some("DDIC field inferred from incomplete cache".to_string()),
                    kind: HoveredComponentKind::Scalar,
                    is_static_method: false,
                    in_type_position: access.in_type_position,
                });
            };
            let kind = match field.shape {
                StructureFieldShape::Scalar => HoveredComponentKind::Scalar,
                StructureFieldShape::Structured { structure } => HoveredComponentKind::Structured {
                    structure_name: Arc::clone(&field_unit.structure(structure).name),
                },
            };
            let field_owner_structure_name =
                Some(Arc::clone(&field_unit.structure(field.owner).name));
            return Some(HoveredComponentInfo {
                base_name: Arc::clone(&access.base_name),
                base_namespace: access.base_namespace,
                component_path: access
                    .field_path
                    .iter()
                    .take(segment_index + 1)
                    .map(|segment| Arc::clone(&segment.name))
                    .collect(),
                field_name: Arc::clone(&field.name),
                field_owner_structure_name,
                range: access.field_path[segment_index].range.clone(),
                declared_type: field.type_ref.as_ref().map(format_field_type_ref),
                description: description_for_field_info(self, &field),
                value_clause_display: field.value_clause_display.clone(),
                declaration: None,
                kind,
                is_static_method: false,
                in_type_position: access.in_type_position,
            });
        }
        if let Some(target) = self.bare_where_field_target_at(offset) {
            let structure_unit = &self.project.units[target.structure_unit_id.as_usize()];
            let field = &target.field;
            let kind = match field.shape {
                StructureFieldShape::Scalar => HoveredComponentKind::Scalar,
                StructureFieldShape::Structured { structure } => HoveredComponentKind::Structured {
                    structure_name: Arc::clone(&structure_unit.structure(structure).name),
                },
            };
            return Some(HoveredComponentInfo {
                base_name: Arc::clone(&field.name),
                base_namespace: Namespace::Value,
                component_path: vec![Arc::clone(&field.name)],
                field_name: Arc::clone(&field.name),
                field_owner_structure_name: Some(Arc::clone(
                    &structure_unit.structure(field.owner).name,
                )),
                range: target.range,
                declared_type: field.type_ref.as_ref().map(format_field_type_ref),
                description: description_for_field_info(self, field),
                value_clause_display: field.value_clause_display.clone(),
                declaration: None,
                kind,
                is_static_method: false,
                in_type_position: false,
            });
        }
        if let Some(query) = self.bare_where_field_query_at(offset) {
            let (token_start, token_end) =
                token_window_for_range(&self.parse, &statement_query_range(&self.parse, offset)?)?;
            let token_idx = prefix_token_at_offset(&self.parse, token_start, token_end, offset)?;
            let token = &self.parse.tokens[token_idx];
            let field_name =
                Arc::<str>::from(token.lexeme(self.text.as_ref()).to_ascii_lowercase());
            let structure_unit = &self.project.units[query.structure_unit_id.as_usize()];
            if let Some(inferred) = inferred_ddic_data_element_target(
                self,
                structure_unit,
                query.structure_id,
                field_name.as_ref(),
            ) {
                return Some(HoveredComponentInfo {
                    base_name: Arc::clone(&field_name),
                    base_namespace: Namespace::Value,
                    component_path: vec![Arc::clone(&field_name)],
                    field_name,
                    field_owner_structure_name: inferred.field_owner_structure_name,
                    range: token.range.clone(),
                    declared_type: Some(format!("TYPE {}", inferred.declared_type_name)),
                    description: description_for_definition_target(self, &inferred.definition),
                    value_clause_display: None,
                    declaration: Some("DDIC field inferred from incomplete cache".to_string()),
                    kind: HoveredComponentKind::Scalar,
                    is_static_method: false,
                    in_type_position: false,
                });
            }
        }
        synthetic_loop_where_hovered_component_at(self, offset)
    }

    pub fn classify_field_access_segment(
        &self,
        access: &abap_symbols::FieldAccess,
        segment_index: usize,
    ) -> Option<HoveredComponentKind> {
        classify_field_access_segment_with_scope_index(
            self,
            self.scope_index(),
            access,
            segment_index,
        )
    }

    pub fn completion_at(&self, offset: usize) -> Option<CompletionInfo> {
        if let Some(completion) = self.template_completion_at(offset) {
            return Some(completion);
        }
        if let Some(completion) = self.selector_completion_at(offset) {
            return Some(CompletionInfo {
                replace_range: completion.replace_range,
                items: completion
                    .items
                    .into_iter()
                    .map(CompletionItem::Selector)
                    .collect(),
                in_type_position: completion.in_type_position,
            });
        }
        if let Some(completion) = self.open_sql_source_completion_at(offset) {
            return Some(completion);
        }
        if let Some(completion) = self.callable_statement_completion_at(offset) {
            return Some(completion);
        }
        if let Some(completion) = self.named_argument_completion_at(offset) {
            return Some(completion);
        }
        self.bare_identifier_completion_at(offset)
    }

    pub fn callable_statement_completion_context_at(
        &self,
        offset: usize,
    ) -> Option<CallableStatementCompletionContext> {
        let query = self.callable_statement_completion_query_at(offset)?;
        Some(CallableStatementCompletionContext {
            replace_range: query.replace_range,
            prefix: query.prefix,
            kind: query.kind,
        })
    }

    pub fn hovered_named_argument_at(&self, offset: usize) -> Option<HoveredSymbolInfo> {
        let access = self
            .symbols
            .named_arguments
            .iter()
            .find(|access| access.range.start <= offset && offset < access.range.end)?;
        let parameter = resolve_named_argument_parameter(self, access)?;
        Some(HoveredSymbolInfo {
            range: access.range.clone(),
            display_name: Arc::clone(&parameter.name),
            markdown_lines: markdown_lines_for_named_argument(access, &parameter),
        })
    }

    pub fn hovered_call_target_at(&self, offset: usize) -> Option<HoveredSymbolInfo> {
        let (range, member_unit, member) = self.call_target_member_at(offset)?;
        Some(HoveredSymbolInfo {
            range,
            display_name: Arc::clone(&member.name),
            markdown_lines: markdown_lines_for_class_member(member_unit, member),
        })
    }

    pub fn has_named_argument_parameter(&self, access: &NamedArgumentAccess) -> bool {
        resolve_named_argument_parameter_with_scope_index(self, self.scope_index(), access)
            .is_some()
    }

    pub fn perform_parameter_inlay_hints_in_range(
        &self,
        range: Range<usize>,
    ) -> Vec<ParameterInlayHintInfo> {
        let mut hints: Vec<_> = self
            .symbols
            .perform_calls
            .iter()
            .flat_map(|perform_call| {
                perform_call.arguments.iter().filter_map(|argument| {
                    if argument.range.start < range.start || argument.range.start >= range.end {
                        return None;
                    }
                    let parameter =
                        resolve_perform_argument_parameter(self, perform_call, argument)?;
                    Some(ParameterInlayHintInfo {
                        position: argument.range.start,
                        label: Arc::clone(&parameter.name),
                        trailing_colon: true,
                        padding_left: false,
                        padding_right: true,
                        tooltip_markdown: perform_parameter_inlay_hint_markdown(&parameter),
                    })
                })
            })
            .collect();
        hints.sort_by_key(|hint| hint.position);
        hints
    }

    pub fn function_module_parameter_inlay_hints_in_range(
        &self,
        range: Range<usize>,
    ) -> Vec<ParameterInlayHintInfo> {
        let mut hints: Vec<_> = self
            .symbols
            .call_sites
            .iter()
            .flat_map(|call_site| {
                call_site.arguments.iter().filter_map(|argument| {
                    function_module_parameter_inlay_hint(self, call_site, argument)
                        .filter(|hint| range.start <= hint.position && hint.position < range.end)
                })
            })
            .collect();
        hints.sort_by_key(|hint| hint.position);
        hints
    }

    pub fn method_parameter_inlay_hints_in_range(
        &self,
        range: Range<usize>,
    ) -> Vec<ParameterInlayHintInfo> {
        let mut hints: Vec<_> = self
            .symbols
            .call_sites
            .iter()
            .flat_map(|call_site| {
                call_site.arguments.iter().filter_map(|argument| {
                    method_parameter_inlay_hint(self, call_site, argument)
                        .filter(|hint| range.start <= hint.position && hint.position < range.end)
                })
            })
            .collect();
        hints.sort_by_key(|hint| hint.position);
        hints
    }

    pub fn inline_variable_type_inlay_hints_in_range(
        &self,
        range: Range<usize>,
    ) -> Vec<TypeInlayHintInfo> {
        let mut hints = Vec::new();
        let mut stack = vec![self.parse.file.root()];
        while let Some(node) = stack.pop() {
            if let Some(symbol_kind) = match self.parse.file.kind(node) {
                SyntaxKind::DataInlineDecl => Some(SymbolKind::Variable),
                SyntaxKind::FieldSymbolInlineDecl => Some(SymbolKind::FieldSymbol),
                _ => None,
            } && let Some(name_range) = self.parse.file.children(node).find_map(|child| {
                (self.parse.file.kind(child) == SyntaxKind::DataDeclName)
                    .then(|| self.parse.file.range(child))
            }) {
                let position = name_range.end;
                if range.start <= position
                    && position < range.end
                    && let Some(symbol) = self.symbols.symbols.iter().find(|symbol| {
                        symbol.kind == symbol_kind && symbol.decl_range == name_range
                    })
                    && let Some(type_presentation) =
                        symbol_inlay_type_presentation(Some(self), symbol)
                {
                    hints.push(TypeInlayHintInfo {
                        position,
                        label: Arc::from(type_presentation.hint_label),
                        tooltip_markdown: format_hover_type_clause(
                            &type_presentation.rendered_clause,
                        ),
                    });
                }
            }

            for child in self.parse.file.children(node) {
                stack.push(child);
            }
        }

        hints.sort_by_key(|hint| hint.position);
        hints
    }

    /// Hover for an Open SQL name span (`FROM` source, column, alias, and similar).
    pub fn hovered_sql_name_ref_at(&self, offset: usize) -> Option<HoveredSymbolInfo> {
        let sql_ref = self.symbols.semantic().sql().name_ref_at_offset(offset)?;
        Some(HoveredSymbolInfo {
            range: sql_ref.range.clone(),
            display_name: Arc::clone(&sql_ref.name),
            markdown_lines: markdown_lines_for_sql_name_ref(self, sql_ref),
        })
    }

    pub fn hovered_message_at(&self, offset: usize) -> Option<HoveredSymbolInfo> {
        for message in &self.symbols.message_uses {
            if let (Some(class_name), Some(range)) = (&message.class_name, &message.class_range)
                && range.start <= offset
                && offset < range.end
            {
                return Some(hovered_message_class(self, class_name, range.clone()));
            }
            if let (Some(id), Some(range)) = (&message.id, &message.id_range)
                && range.start <= offset
                && offset < range.end
            {
                let class_name = message.class_name.as_ref().or_else(|| {
                    self.symbols
                        .message_default_class
                        .as_ref()
                        .map(|class| &class.name)
                })?;
                let entry = message_class_entry(self.project.as_ref(), class_name, id)?;
                return Some(HoveredSymbolInfo {
                    range: range.clone(),
                    display_name: Arc::clone(&entry.id),
                    markdown_lines: markdown_lines_for_message_entry(entry),
                });
            }
        }

        let default = self.symbols.message_default_class.as_ref()?;
        if default.range.start <= offset && offset < default.range.end {
            return Some(hovered_message_class(
                self,
                &default.name,
                default.range.clone(),
            ));
        }
        None
    }

    pub fn definition_at(&self, offset: usize) -> Option<DefinitionTarget> {
        if let Some(target) = self.definition_target_for_component_at(offset) {
            return Some(target);
        }
        if let Some(target) = self.definition_target_for_call_target_at(offset) {
            return Some(target);
        }
        if let Some(target) = self.definition_target_for_perform_target_at(offset) {
            return Some(target);
        }
        if let Some(target) = self.definition_target_for_named_argument_at(offset) {
            return Some(target);
        }
        if let Some(target) = self.definition_target_for_sql_name_ref_at(offset) {
            return Some(target);
        }
        self.definition_target_for_resolved_symbol_at(offset)
            .or_else(|| self.definition_target_for_bare_where_field_at(offset))
    }

    pub fn missing_method_implementation_action_at(
        &self,
        offset: usize,
    ) -> Option<MissingMethodImplementationAction> {
        let member = self
            .symbols
            .semantic()
            .decls()
            .class_member_at_offset(offset)?;
        if member.kind != ClassMemberKind::Method
            || member.implementation.is_some()
            || self.symbols.symbol(member.class_symbol).kind != SymbolKind::Class
            || self.symbols.member_aliases.iter().any(|alias| {
                alias.owner_symbol == member.class_symbol && alias.alias_name == member.name
            })
            || member
                .signature
                .split_ascii_whitespace()
                .any(|part| part.eq_ignore_ascii_case("abstract"))
        {
            return None;
        }

        let class_name = self.symbols.symbol(member.class_symbol).name.as_ref();
        let implementation =
            class_implementation_edit_target(&self.parse, self.text.as_ref(), class_name)?;
        Some(MissingMethodImplementationAction {
            title: format!("Create method implementation '{}'", member.name),
            edit_range: implementation.edit_range.clone(),
            new_text: match implementation.kind {
                ClassImplementationEditKind::ExistingBody { body_is_empty } => {
                    build_missing_method_implementation_text(
                        self.text.as_ref(),
                        member.name.as_ref(),
                        body_is_empty,
                    )
                }
                ClassImplementationEditKind::MissingBlock => {
                    build_missing_class_implementation_text(
                        self.text.as_ref(),
                        class_name,
                        member.name.as_ref(),
                        implementation.edit_range.end < self.text.len(),
                    )
                }
            },
        })
    }

    pub fn method_parameter_comments_action_at(
        &self,
        offset: usize,
    ) -> Option<MethodParameterCommentsAction> {
        let (_, member, signature_member) =
            method_implementation_signature_member_at_offset(self, offset)?;
        let implementation = member.implementation.as_ref()?;
        let header_end =
            method_implementation_parameter_anchor(self.text.as_ref(), &implementation.range);
        let insertion = line_end_including_newline(self.text.as_ref(), header_end);
        let managed_range =
            managed_method_parameter_comment_block_range(self.text.as_ref(), insertion);

        if signature_member.parameters.is_empty() {
            let edit_range = managed_range?;
            return Some(MethodParameterCommentsAction {
                title: "Remove ABAP LSP method parameter comments".to_string(),
                edit_range,
                new_text: String::new(),
            });
        }

        let new_text = build_method_parameter_comment_block(
            self.text.as_ref(),
            &implementation.range,
            &signature_member.parameters,
        );
        let edit_range = managed_range.unwrap_or(insertion..insertion);
        Some(MethodParameterCommentsAction {
            title: format!("Sync method parameter comments for '{}'", member.name),
            edit_range,
            new_text,
        })
    }

    fn reference_search_target_at(&self, offset: usize) -> Option<ReferenceSearchTarget> {
        if let Some(target) = self.reference_search_target_for_component_at(offset) {
            return Some(target);
        }
        if let Some(target) = self.reference_search_target_for_named_argument_at(offset) {
            return Some(target);
        }
        if let Some(target) = self.reference_search_target_for_dd_like_type_name(offset) {
            return Some(target);
        }
        self.reference_search_target_for_resolved_symbol_at(offset)
    }

    fn rename_target_at(&self, offset: usize) -> Option<(ReferenceSearchTarget, Range<usize>)> {
        if let Some(target) = self.rename_target_for_component_at(offset) {
            return Some(target);
        }
        if let Some(target) = self.rename_target_for_named_argument_at(offset) {
            return Some(target);
        }
        self.rename_target_for_resolved_symbol_at(offset)
    }

    /// Hover for a resolved reference (narrowest matching range) or, if none, a symbol declaration
    /// covering the offset.
    pub fn hovered_resolved_symbol_at(&self, offset: usize) -> Option<HoveredSymbolInfo> {
        if let Some(reference) = self.symbols.semantic().refs().reference_at_offset(offset)
            && let Some(resolution) = reference.resolution
        {
            return Some(HoveredSymbolInfo {
                range: reference.range.clone(),
                display_name: Arc::clone(&reference.name),
                markdown_lines: markdown_lines_for_resolution(self, &reference.name, resolution),
            });
        }

        if let Some(member) = self
            .symbols
            .semantic()
            .decls()
            .class_member_at_offset(offset)
        {
            return Some(HoveredSymbolInfo {
                range: class_member_name_range_at_offset(member, offset)
                    .cloned()
                    .unwrap_or_else(|| member.decl_range.clone()),
                display_name: Arc::clone(&member.name),
                markdown_lines: markdown_lines_for_class_member(self.symbols.as_ref(), member),
            });
        }

        let symbol = self.symbols.semantic().decls().symbol_at_offset(offset)?;

        if symbol.kind == SymbolKind::Method
            && let Some((definition_unit, member)) = self
                .project
                .class_member_definition_for_method_symbol(self.symbols.unit_id, symbol.id)
        {
            let unit = &self.project.units[definition_unit.as_usize()];
            return Some(HoveredSymbolInfo {
                range: rename_method_symbol_range(self.text.as_ref(), symbol)
                    .unwrap_or_else(|| symbol.decl_range.clone()),
                display_name: Arc::clone(&member.name),
                markdown_lines: markdown_lines_for_class_member(unit, member),
            });
        }

        Some(HoveredSymbolInfo {
            range: symbol.decl_range.clone(),
            display_name: Arc::clone(&symbol.name),
            markdown_lines: markdown_lines_for_declared_symbol(self, self.symbols.as_ref(), symbol),
        })
    }

    fn definition_target_for_component_at(&self, offset: usize) -> Option<DefinitionTarget> {
        if let Some((access, segment_index)) =
            self.symbols.field_accesses.iter().find_map(|access| {
                access
                    .field_path
                    .iter()
                    .enumerate()
                    .find_map(|(idx, segment)| {
                        (segment.range.start <= offset && offset < segment.range.end)
                            .then_some((access, idx))
                    })
            })
        {
            let (unit, symbol_id) = resolve_field_access_base_symbol(self, access)?;
            if segment_index == 0
                && let Some((interface_unit, interface_symbol)) =
                    resolve_interface_selector_qualifier_with_scope_index(
                        self,
                        self.scope_index(),
                        access,
                        unit,
                        symbol_id,
                    )
            {
                return Some(definition_target_for_symbol(
                    interface_unit,
                    interface_unit.symbol(interface_symbol),
                ));
            }
            if segment_index == 0
                && let Some((method_unit, method_symbol)) =
                    resolve_interface_selector_method_symbol(self, access, unit, symbol_id)
            {
                return Some(definition_target_for_range(
                    method_unit,
                    qualified_method_symbol_qualifier_range(
                        method_symbol,
                        access.field_path[0].name.as_ref(),
                    ),
                ));
            }
            if let Some((type_unit, type_symbol)) =
                resolve_class_selector_type_symbol_with_scope_index(
                    self,
                    self.scope_index(),
                    access,
                    segment_index,
                    unit,
                    symbol_id,
                )
            {
                return Some(definition_target_for_symbol(type_unit, type_symbol));
            }
            if let Some((member_unit, member)) =
                resolve_class_selector_member(self, access, segment_index, unit, symbol_id)
            {
                if member.kind == ClassMemberKind::Method
                    && self
                        .symbols
                        .semantic()
                        .decls()
                        .symbol_at_offset(offset)
                        .is_none()
                {
                    return Some(definition_target_for_class_member_implementation_or_decl(
                        self.project.as_ref(),
                        member_unit,
                        member,
                    ));
                }
                return Some(definition_target_for_class_member(member_unit, member));
            }
            if segment_index == 1
                && let Some((method_unit, method_symbol)) =
                    resolve_interface_selector_method_symbol(self, access, unit, symbol_id)
            {
                return Some(definition_target_for_range(
                    method_unit,
                    qualified_method_symbol_member_range(
                        method_symbol,
                        access.field_path[1].name.as_ref(),
                    ),
                ));
            }
            let Some((_field_unit, field)) = resolve_field_access_component_with_scope_index(
                self,
                self.scope_index(),
                access,
                segment_index,
                unit,
                symbol_id,
            ) else {
                let (structure_unit, structure_id) =
                    resolve_field_access_container_structure_with_scope_index(
                        self,
                        self.scope_index(),
                        access,
                        segment_index,
                        unit,
                        symbol_id,
                    )?;
                return inferred_ddic_data_element_target(
                    self,
                    structure_unit,
                    structure_id,
                    access.field_path[segment_index].name.as_ref(),
                )
                .map(|target| target.definition);
            };
            let decl_range = field.decl_range?;
            return Some(definition_target_for_range(
                &self.project.units[field.decl_unit.as_usize()],
                decl_range,
            ));
        }
        None
    }

    fn definition_target_for_bare_where_field_at(&self, offset: usize) -> Option<DefinitionTarget> {
        if let Some(target) = self.bare_where_field_target_at(offset) {
            let decl_range = target.field.decl_range?;
            return Some(definition_target_for_range(
                &self.project.units[target.field.decl_unit.as_usize()],
                decl_range,
            ));
        }
        let query = self.bare_where_field_query_at(offset)?;
        let (token_start, token_end) =
            token_window_for_range(&self.parse, &statement_query_range(&self.parse, offset)?)?;
        let token_idx = prefix_token_at_offset(&self.parse, token_start, token_end, offset)?;
        let token = &self.parse.tokens[token_idx];
        let field_name = Arc::<str>::from(token.lexeme(self.text.as_ref()).to_ascii_lowercase());
        let structure_unit = &self.project.units[query.structure_unit_id.as_usize()];
        inferred_ddic_data_element_target(
            self,
            structure_unit,
            query.structure_id,
            field_name.as_ref(),
        )
        .map(|target| target.definition)
    }

    fn definition_target_for_call_target_at(&self, offset: usize) -> Option<DefinitionTarget> {
        let (_, member_unit, member) = self.call_target_member_at(offset)?;
        Some(definition_target_for_class_member_implementation_or_decl(
            self.project.as_ref(),
            member_unit,
            member,
        ))
    }

    fn reference_search_target_for_component_at(
        &self,
        offset: usize,
    ) -> Option<ReferenceSearchTarget> {
        self.component_reference_search_target_at(offset)
            .map(|(target, _)| target)
    }

    fn rename_target_for_component_at(
        &self,
        offset: usize,
    ) -> Option<(ReferenceSearchTarget, Range<usize>)> {
        self.component_reference_search_target_at(offset)
    }

    fn component_reference_search_target_at(
        &self,
        offset: usize,
    ) -> Option<(ReferenceSearchTarget, Range<usize>)> {
        if let Some((access, segment_index)) =
            self.symbols.field_accesses.iter().find_map(|access| {
                access
                    .field_path
                    .iter()
                    .enumerate()
                    .find_map(|(idx, segment)| {
                        (segment.range.start <= offset && offset < segment.range.end)
                            .then_some((access, idx))
                    })
            })
        {
            let (unit, symbol_id) = resolve_field_access_base_symbol(self, access)?;
            if let Some((member_unit, member)) =
                resolve_class_selector_member(self, access, segment_index, unit, symbol_id)
            {
                return Some((
                    ReferenceSearchTarget::ClassMember {
                        unit: member_unit.unit_id,
                        class_symbol: member.class_symbol,
                        name: Arc::clone(&member.name),
                    },
                    access.field_path[segment_index].range.clone(),
                ));
            }
            let (structure_unit, field) = resolve_field_access_component_with_scope_index(
                self,
                self.scope_index(),
                access,
                segment_index,
                unit,
                symbol_id,
            )?;
            return Some((
                ReferenceSearchTarget::StructField {
                    unit: field.owner_unit,
                    owner: structure_unit.structure(field.owner).origin_structure,
                    name: Arc::clone(&field.name),
                },
                access.field_path[segment_index].range.clone(),
            ));
        }
        let target = self.bare_where_field_target_at(offset)?;
        Some((
            ReferenceSearchTarget::StructField {
                unit: target.field.owner_unit,
                owner: self.project.units[target.structure_unit_id.as_usize()]
                    .structure(target.field.owner)
                    .origin_structure,
                name: Arc::clone(&target.field.name),
            },
            target.range,
        ))
    }

    fn definition_target_for_named_argument_at(&self, offset: usize) -> Option<DefinitionTarget> {
        let access = self
            .symbols
            .named_arguments
            .iter()
            .find(|access| access.range.start <= offset && offset < access.range.end)?;
        resolve_named_argument_target(self, access)
    }

    fn reference_search_target_for_named_argument_at(
        &self,
        offset: usize,
    ) -> Option<ReferenceSearchTarget> {
        let access = self
            .symbols
            .named_arguments
            .iter()
            .find(|access| access.range.start <= offset && offset < access.range.end)?;
        Some(ReferenceSearchTarget::Symbol(
            resolve_named_argument_symbol(self, access)?,
        ))
    }

    fn rename_target_for_named_argument_at(
        &self,
        offset: usize,
    ) -> Option<(ReferenceSearchTarget, Range<usize>)> {
        let access = self
            .symbols
            .named_arguments
            .iter()
            .find(|access| access.range.start <= offset && offset < access.range.end)?;
        let handle = resolve_named_argument_symbol(self, access)?;
        Some((
            self.rename_search_target_for_symbol_handle(handle)?,
            access.range.clone(),
        ))
    }

    fn definition_target_for_resolved_symbol_at(&self, offset: usize) -> Option<DefinitionTarget> {
        if let Some(reference) = self.symbols.semantic().refs().reference_at_offset(offset) {
            if reference.kind == ReferenceKind::Include {
                return self
                    .definition_target_for_include_reference(reference)
                    .or_else(|| {
                        reference.resolution.and_then(|resolution| {
                            definition_target_for_resolution(self, resolution)
                        })
                    });
            }
            if let Some(resolution) = reference.resolution {
                return definition_target_for_resolution(self, resolution).or_else(|| {
                    self.symbols
                        .semantic()
                        .decls()
                        .symbol_at_offset(reference.range.start)
                        .filter(|symbol| symbol.decl_range == reference.range)
                        .map(|symbol| definition_target_for_symbol(self.symbols.as_ref(), symbol))
                });
            }
        }

        if let Some(member) = self
            .symbols
            .semantic()
            .decls()
            .class_member_at_offset(offset)
        {
            return Some(definition_target_for_class_member_name_at(
                self.project.as_ref(),
                self.symbols.as_ref(),
                member,
                offset,
            ));
        }

        if let Some(symbol) = self.symbols.semantic().decls().symbol_at_offset(offset) {
            if symbol.kind == SymbolKind::Method
                && let Some((definition_unit, member)) = self
                    .project
                    .class_member_definition_for_method_symbol(self.symbols.unit_id, symbol.id)
            {
                let unit = &self.project.units[definition_unit.as_usize()];
                return Some(definition_target_for_class_member(unit, member));
            }
            return Some(definition_target_for_symbol(self.symbols.as_ref(), symbol));
        }

        None
    }

    fn definition_target_for_include_reference(
        &self,
        reference: &abap_symbols::ReferenceData,
    ) -> Option<DefinitionTarget> {
        let target = self
            .symbols
            .include_edges
            .iter()
            .find(|edge| edge.range == reference.range && edge.name == reference.name)?
            .target?;
        let unit = &self.project.units[target.as_usize()];
        Some(DefinitionTarget {
            uri: Arc::clone(&unit.uri),
            range: 0..0,
        })
    }

    fn definition_target_for_perform_target_at(&self, offset: usize) -> Option<DefinitionTarget> {
        let perform_call = self.symbols.perform_calls.iter().find(|perform_call| {
            perform_call.routine_range.start <= offset && offset < perform_call.routine_range.end
        })?;
        let handle = self
            .project
            .resolve_perform_call_target(self.symbols.as_ref(), perform_call)?;
        let unit = &self.project.units[handle.unit.as_usize()];
        Some(definition_target_for_symbol(
            unit,
            unit.symbol(handle.symbol),
        ))
    }

    fn reference_search_target_for_resolved_symbol_at(
        &self,
        offset: usize,
    ) -> Option<ReferenceSearchTarget> {
        if let Some(reference) = self.symbols.semantic().refs().reference_at_offset(offset)
            && let Some(Resolution::Symbol(handle)) = reference.resolution
        {
            return Some(ReferenceSearchTarget::Symbol(handle));
        }

        if let Some(member) = self
            .symbols
            .semantic()
            .decls()
            .class_member_at_offset(offset)
        {
            return Some(ReferenceSearchTarget::ClassMember {
                unit: self.symbols.unit_id,
                class_symbol: member.class_symbol,
                name: Arc::clone(&member.name),
            });
        }

        if let Some(symbol) = self.symbols.semantic().decls().symbol_at_offset(offset) {
            if symbol.kind == SymbolKind::Method
                && let Some((definition_unit, member)) = self
                    .project
                    .class_member_definition_for_method_symbol(self.symbols.unit_id, symbol.id)
            {
                return Some(ReferenceSearchTarget::ClassMember {
                    unit: definition_unit,
                    class_symbol: member.class_symbol,
                    name: Arc::clone(&member.name),
                });
            }
            return Some(ReferenceSearchTarget::Symbol(abap_symbols::SymbolHandle {
                unit: self.symbols.unit_id,
                symbol: symbol.id,
            }));
        }

        self.reference_search_target_for_declared_structure_field_at(offset)
    }

    fn rename_target_for_resolved_symbol_at(
        &self,
        offset: usize,
    ) -> Option<(ReferenceSearchTarget, Range<usize>)> {
        if let Some(reference) = self.symbols.semantic().refs().reference_at_offset(offset)
            && let Some(Resolution::Symbol(handle)) = reference.resolution
        {
            return Some((
                self.rename_search_target_for_symbol_handle(handle)?,
                reference.range.clone(),
            ));
        }

        if let Some(member) = self
            .symbols
            .semantic()
            .decls()
            .class_member_at_offset(offset)
        {
            let range = rename_range_for_class_member(self.text.as_ref(), member, offset)?;
            return Some((
                ReferenceSearchTarget::ClassMember {
                    unit: self.symbols.unit_id,
                    class_symbol: member.class_symbol,
                    name: Arc::clone(&member.name),
                },
                range,
            ));
        }

        if let Some(symbol) = self.symbols.semantic().decls().symbol_at_offset(offset) {
            if symbol.kind == SymbolKind::Method
                && let Some((definition_unit, member)) = self
                    .project
                    .class_member_definition_for_method_symbol(self.symbols.unit_id, symbol.id)
            {
                return Some((
                    ReferenceSearchTarget::ClassMember {
                        unit: definition_unit,
                        class_symbol: member.class_symbol,
                        name: Arc::clone(&member.name),
                    },
                    rename_method_symbol_range(self.text.as_ref(), symbol)?,
                ));
            }
            let handle = abap_symbols::SymbolHandle {
                unit: self.symbols.unit_id,
                symbol: symbol.id,
            };
            return Some((
                self.rename_search_target_for_symbol_handle(handle)?,
                symbol.decl_range.clone(),
            ));
        }

        let field = self
            .symbols
            .semantic()
            .decls()
            .structure_field_at_offset(offset)?;
        Some((
            ReferenceSearchTarget::StructField {
                unit: field.owner_unit,
                owner: self.symbols.structure(field.owner).origin_structure,
                name: field.name,
            },
            field.decl_range?,
        ))
    }

    fn reference_search_target_for_declared_structure_field_at(
        &self,
        offset: usize,
    ) -> Option<ReferenceSearchTarget> {
        self.symbols
            .semantic()
            .decls()
            .structure_field_at_offset(offset)
            .map(|field| ReferenceSearchTarget::StructField {
                unit: field.owner_unit,
                owner: self.symbols.structure(field.owner).origin_structure,
                name: field.name,
            })
    }

    fn rename_search_target_for_symbol_handle(
        &self,
        handle: abap_symbols::SymbolHandle,
    ) -> Option<ReferenceSearchTarget> {
        let unit = &self.project.units[handle.unit.as_usize()];
        let symbol = unit.symbol(handle.symbol);
        if !rename_supported_symbol_kind(symbol.kind) {
            return None;
        }
        if symbol.kind == SymbolKind::Method
            && let Some(class_symbol) = enclosing_class_owner(unit, symbol.scope)
            && unit
                .semantic()
                .decls()
                .class_member(class_symbol, symbol.name.as_ref())
                .is_some()
        {
            return Some(ReferenceSearchTarget::ClassMember {
                unit: handle.unit,
                class_symbol,
                name: Arc::clone(&symbol.name),
            });
        }
        Some(ReferenceSearchTarget::Symbol(handle))
    }

    fn definition_target_for_sql_source_matching_type_ref(
        &self,
        offset: usize,
    ) -> Option<DefinitionTarget> {
        let sql_ref = self.symbols.semantic().sql().name_ref_at_offset(offset)?;
        if sql_ref.kind != SqlNameRefKind::Source {
            return None;
        }
        let name = sql_ref.name.as_ref();
        let unit = self.symbols.as_ref();
        unit.semantic()
            .refs()
            .type_named(name)
            .filter_map(|reference| {
                let resolution = reference.resolution.as_ref()?;
                let target = definition_target_for_resolution(self, *resolution)?;
                Some((
                    reference.range.end.saturating_sub(reference.range.start),
                    target,
                ))
            })
            .min_by_key(|(width, _)| *width)
            .map(|(_, target)| target)
            .or_else(|| definition_target_for_dd_like_type_name(self.project.as_ref(), name))
    }

    fn definition_target_for_sql_name_ref_at(&self, offset: usize) -> Option<DefinitionTarget> {
        let sql_ref = self.symbols.semantic().sql().name_ref_at_offset(offset)?;
        match sql_ref.kind {
            SqlNameRefKind::Source => {
                self.definition_target_for_sql_source_matching_type_ref(offset)
            }
            SqlNameRefKind::Column | SqlNameRefKind::QualifiedColumn => {
                let target = self.sql_field_target_at(offset)?;
                let decl_range = target.field.decl_range?;
                Some(definition_target_for_range(
                    &self.project.units[target.field.decl_unit.as_usize()],
                    decl_range,
                ))
            }
            _ => None,
        }
    }

    fn sql_field_target_at(&self, offset: usize) -> Option<ResolvedSqlFieldTarget> {
        let sql_ref = self.symbols.semantic().sql().name_ref_at_offset(offset)?;
        if !matches!(
            sql_ref.kind,
            SqlNameRefKind::Column | SqlNameRefKind::QualifiedColumn
        ) {
            return None;
        }
        let source_name = self.sql_source_name_for_ref(sql_ref)?;
        let (source_unit, structure_id) =
            self.sql_source_structure_for_name(sql_ref.scope, &source_name)?;
        let field = source_unit
            .semantic()
            .decls()
            .structure_field_info(structure_id, sql_ref.name.as_ref())?;
        let field_owner_structure_name = Arc::clone(&source_unit.structure(field.owner).name);
        Some(ResolvedSqlFieldTarget {
            range: sql_ref.range.clone(),
            source_name,
            source_alias: sql_ref.qualifier.clone(),
            description: description_for_field_info(self, &field),
            field,
            field_owner_structure_name,
        })
    }

    fn sql_source_name_for_ref(&self, sql_ref: &SqlNameRefData) -> Option<Arc<str>> {
        if matches!(sql_ref.kind, SqlNameRefKind::Source) {
            return Some(Arc::clone(&sql_ref.name));
        }
        let sources: Vec<_> = self
            .symbols
            .sql_sources
            .iter()
            .filter(|source| source.query_id == sql_ref.query_id)
            .collect();
        if let Some(qualifier) = sql_ref.qualifier.as_ref() {
            return sources
                .into_iter()
                .find(|source| {
                    source.alias.as_ref() == Some(qualifier) || source.name == *qualifier
                })
                .map(|source| Arc::clone(&source.name));
        }
        if sources.len() == 1 {
            return sources.first().map(|source| Arc::clone(&source.name));
        }
        None
    }

    fn sql_source_structure_for_name(
        &self,
        scope: ScopeId,
        source_name: &Arc<str>,
    ) -> Option<(&UnitAnalysis, StructureId)> {
        let (unit, symbol_id) =
            resolve_symbol_from_context(self, scope, Namespace::Type, source_name, false).or_else(
                || resolve_symbol_from_context(self, scope, Namespace::Value, source_name, false),
            )?;
        Some((unit, unit.symbol(symbol_id).structure?))
    }

    fn reference_search_target_for_dd_like_type_name(
        &self,
        offset: usize,
    ) -> Option<ReferenceSearchTarget> {
        let unit = self.symbols.as_ref();
        if let Some(sql_ref) = unit.semantic().sql().name_ref_at_offset(offset) {
            if sql_ref.kind == SqlNameRefKind::Source {
                return Some(ReferenceSearchTarget::DdLikeTypeName {
                    unit: unit.unit_id,
                    name: Arc::clone(&sql_ref.name),
                });
            }
        }

        let type_ref = unit.semantic().refs().type_reference_at_offset(offset)?;

        let name = &type_ref.name;
        let slash_name = name.as_ref().contains('/');
        let used_in_sql = unit.semantic().sql().has_source_named(name.as_ref());
        if slash_name || used_in_sql {
            return Some(ReferenceSearchTarget::DdLikeTypeName {
                unit: unit.unit_id,
                name: Arc::clone(name),
            });
        }
        None
    }

    fn local_references_for_target(&self, target: &ReferenceSearchTarget) -> Vec<ReferenceTarget> {
        match target {
            ReferenceSearchTarget::Symbol(handle) => self.local_symbol_references(*handle),
            ReferenceSearchTarget::ClassMember {
                unit,
                class_symbol,
                name,
            } => self.local_class_member_references(*unit, *class_symbol, name),
            ReferenceSearchTarget::StructField { unit, owner, name } => {
                self.local_structure_field_references(*unit, *owner, name)
            }
            ReferenceSearchTarget::DdLikeTypeName { unit, name } => {
                self.local_dd_like_type_name_references(*unit, name)
            }
        }
    }

    fn local_rename_locations_for_target(
        &self,
        target: &ReferenceSearchTarget,
    ) -> Vec<ReferenceTarget> {
        let mut out = self.local_references_for_target(target);
        out.extend(self.local_declaration_locations_for_target(target));
        out.sort_by(|left, right| {
            left.uri
                .cmp(&right.uri)
                .then(left.range.start.cmp(&right.range.start))
                .then(left.range.end.cmp(&right.range.end))
        });
        out.dedup_by(|left, right| left.uri == right.uri && left.range == right.range);
        out
    }

    fn local_declaration_locations_for_target(
        &self,
        target: &ReferenceSearchTarget,
    ) -> Vec<ReferenceTarget> {
        match target {
            ReferenceSearchTarget::Symbol(handle) => {
                equivalent_symbol_handles(self.project.as_ref(), *handle)
                    .into_iter()
                    .filter(|equivalent| equivalent.unit == self.symbols.unit_id)
                    .map(|equivalent| ReferenceTarget {
                        uri: Arc::clone(&self.uri),
                        range: self.project.units[equivalent.unit.as_usize()]
                            .symbol(equivalent.symbol)
                            .decl_range
                            .clone(),
                    })
                    .collect()
            }
            ReferenceSearchTarget::ClassMember {
                unit,
                class_symbol,
                name,
            } => {
                let target_unit = &self.project.units[unit.as_usize()];
                let Some(member) = target_unit
                    .semantic()
                    .decls()
                    .class_member(*class_symbol, name.as_ref())
                else {
                    return Vec::new();
                };
                let mut out = Vec::new();
                if *unit == self.symbols.unit_id {
                    out.push(ReferenceTarget {
                        uri: Arc::clone(&self.uri),
                        range: member.decl_range.clone(),
                    });
                }
                if let Some(range) = implementation_range_for_unit_text(
                    self.text.as_ref(),
                    member,
                    self.symbols.unit_id,
                ) {
                    out.push(ReferenceTarget {
                        uri: Arc::clone(&self.uri),
                        range,
                    });
                }
                out
            }
            ReferenceSearchTarget::StructField { .. } => {
                reference_target_for_search_target(self.project.as_ref(), target)
                    .filter(|target| target.uri.as_ref() == self.uri.as_ref())
                    .into_iter()
                    .collect()
            }
            ReferenceSearchTarget::DdLikeTypeName { .. } => Vec::new(),
        }
    }

    fn local_dd_like_type_name_references(
        &self,
        target_unit: UnitId,
        name: &Arc<str>,
    ) -> Vec<ReferenceTarget> {
        if self.symbols.unit_id != target_unit {
            return Vec::new();
        }
        let mut out: Vec<ReferenceTarget> = self
            .symbols
            .semantic()
            .refs()
            .type_named(name.as_ref())
            .map(|reference| ReferenceTarget {
                uri: Arc::clone(&self.uri),
                range: reference.range.clone(),
            })
            .collect();
        out.extend(
            self.symbols
                .semantic()
                .sql()
                .source_name_refs_named(name.as_ref())
                .map(|sql_ref| ReferenceTarget {
                    uri: Arc::clone(&self.uri),
                    range: sql_ref.range.clone(),
                }),
        );
        out.sort_by(|left, right| {
            left.range
                .start
                .cmp(&right.range.start)
                .then(left.range.end.cmp(&right.range.end))
        });
        out.dedup_by(|left, right| left.range == right.range);
        out
    }

    fn local_symbol_references(&self, handle: abap_symbols::SymbolHandle) -> Vec<ReferenceTarget> {
        let related_handles = equivalent_symbol_handles(self.project.as_ref(), handle);
        let mut out: Vec<_> = self
            .related_symbol_references(&related_handles)
            .map(|reference| ReferenceTarget {
                uri: Arc::clone(&self.uri),
                range: reference.range.clone(),
            })
            .collect();
        let symbol = self.project.units[handle.unit.as_usize()].symbol(handle.symbol);
        if symbol.kind == SymbolKind::Parameter {
            out.extend(self.local_named_argument_references_for_parameter(&related_handles));
            out.extend(self.local_perform_argument_references_for_parameter(&related_handles));
        }
        out
    }

    fn related_symbol_references<'a>(
        &'a self,
        handles: &'a [abap_symbols::SymbolHandle],
    ) -> impl Iterator<Item = &'a abap_symbols::ReferenceData> + 'a {
        handles
            .iter()
            .flat_map(|handle| self.symbols.semantic().refs().resolving_to(*handle))
    }

    fn local_named_argument_references_for_parameter(
        &self,
        handles: &[abap_symbols::SymbolHandle],
    ) -> Vec<ReferenceTarget> {
        self.symbols
            .named_arguments
            .iter()
            .filter_map(|access| {
                resolve_named_argument_symbol(self, access)
                    .filter(|handle| handles.contains(handle))
                    .map(|_| ReferenceTarget {
                        uri: Arc::clone(&self.uri),
                        range: access.range.clone(),
                    })
            })
            .collect()
    }

    fn local_perform_argument_references_for_parameter(
        &self,
        handles: &[abap_symbols::SymbolHandle],
    ) -> Vec<ReferenceTarget> {
        self.symbols
            .perform_calls
            .iter()
            .flat_map(|perform_call| {
                perform_call.arguments.iter().filter_map(|argument| {
                    resolve_perform_argument_symbol(self, perform_call, argument)
                        .filter(|handle| handles.contains(handle))
                        .map(|_| ReferenceTarget {
                            uri: Arc::clone(&self.uri),
                            range: argument.range.clone(),
                        })
                })
            })
            .collect()
    }

    fn local_class_member_references(
        &self,
        target_unit: UnitId,
        class_symbol: SymbolId,
        name: &Arc<str>,
    ) -> Vec<ReferenceTarget> {
        let target_analysis = &self.project.units[target_unit.as_usize()];
        let method_handles: Vec<_> = target_analysis
            .symbols
            .iter()
            .filter(|symbol| {
                symbol.kind == SymbolKind::Method
                    && symbol.name == *name
                    && enclosing_class_owner(target_analysis, symbol.scope) == Some(class_symbol)
            })
            .map(|symbol| abap_symbols::SymbolHandle {
                unit: target_unit,
                symbol: symbol.id,
            })
            .collect();
        let mut references: Vec<_> = self
            .related_symbol_references(&method_handles)
            .map(|reference| ReferenceTarget {
                uri: Arc::clone(&self.uri),
                range: reference.range.clone(),
            })
            .collect();
        references.extend(
            self.symbols
                .field_accesses
                .iter()
                .filter_map(|access| {
                    let (unit, symbol_id) = resolve_field_access_base_symbol(self, access)?;
                    access
                        .field_path
                        .iter()
                        .enumerate()
                        .find_map(|(segment_index, segment)| {
                            let (member_unit, member) = resolve_class_selector_member(
                                self,
                                access,
                                segment_index,
                                unit,
                                symbol_id,
                            )?;
                            (member_unit.unit_id == target_unit
                                && member.class_symbol == class_symbol
                                && member.name == *name)
                                .then(|| ReferenceTarget {
                                    uri: Arc::clone(&self.uri),
                                    range: segment.range.clone(),
                                })
                        })
                })
                .collect::<Vec<_>>(),
        );
        references
    }

    fn local_structure_field_references(
        &self,
        target_unit: UnitId,
        owner: StructureId,
        name: &Arc<str>,
    ) -> Vec<ReferenceTarget> {
        let mut out = Vec::new();
        for access in &self.symbols.field_accesses {
            let Some((unit, symbol_id)) = resolve_field_access_base_symbol(self, access) else {
                continue;
            };
            for segment_index in 0..access.field_path.len() {
                if resolve_class_selector_member(self, access, segment_index, unit, symbol_id)
                    .is_some()
                {
                    continue;
                }
                let Some((structure_unit, field)) = resolve_field_access_component_with_scope_index(
                    self,
                    self.scope_index(),
                    access,
                    segment_index,
                    unit,
                    symbol_id,
                ) else {
                    continue;
                };
                if field.owner_unit == target_unit
                    && structure_unit.structure(field.owner).origin_structure == owner
                    && field.name == *name
                {
                    out.push(ReferenceTarget {
                        uri: Arc::clone(&self.uri),
                        range: access.field_path[segment_index].range.clone(),
                    });
                }
            }
        }
        out
    }

    pub fn selector_completion_at(&self, offset: usize) -> Option<SelectorCompletionInfo> {
        let Some(query) = self.selector_completion_query_at(offset) else {
            return self
                .bare_where_field_completion_at(offset)
                .or_else(|| self.open_sql_field_completion_at(offset));
        };
        if query.component_path.is_empty()
            && let Some((unit, class_symbol_id, requires_static)) =
                resolve_method_target_from_context(
                    self,
                    query.scope,
                    query.base_namespace,
                    &query.base_name,
                )
        {
            let mut items: Vec<_> = if query.in_type_position {
                collect_class_types_in_hierarchy(self, unit, class_symbol_id)
                    .into_iter()
                    .filter(|(_, symbol)| symbol.name.as_ref().starts_with(query.prefix.as_ref()))
                    .map(|(type_unit, type_symbol)| {
                        selector_completion_item_for_type_symbol(type_unit, type_symbol)
                    })
                    .collect()
            } else {
                collect_class_value_members_in_hierarchy(self, unit, class_symbol_id)
                    .into_iter()
                    .filter(|member| {
                        let (member_unit, member) = member;
                        (!requires_static || member.is_static)
                            && class_member_visible_to(
                                self,
                                self.symbols.as_ref(),
                                query.scope,
                                member_unit,
                                member,
                            )
                            && member.name.as_ref().starts_with(query.prefix.as_ref())
                    })
                    .map(|(member_unit, member)| SelectorCompletionItem {
                        name: Arc::clone(&member.name),
                        declared_type: None,
                        declaration: Some(format_class_member_signature(member_unit, member)),
                        kind: hovered_component_kind_for_class_member(member),
                        field_owner_structure_name: None,
                        insertion: if member.kind == ClassMemberKind::Method {
                            callable_completion_insertion(member)
                        } else {
                            identifier_completion_insertion(member.name.as_ref())
                        },
                    })
                    .collect()
            };
            items.sort_by(|left, right| left.name.cmp(&right.name));
            return Some(SelectorCompletionInfo {
                replace_range: query.replace_range,
                items,
                in_type_position: query.in_type_position,
            });
        }
        let Some((unit, symbol_id)) = resolve_symbol_from_context(
            self,
            query.scope,
            query.base_namespace,
            &query.base_name,
            query.in_type_position,
        ) else {
            return self.open_sql_field_completion_at(offset);
        };
        let Some((structure_unit, structure_id)) =
            resolve_selector_component_path_structure_with_scope_index(
                self,
                self.scope_index(),
                query.scope,
                query.base_namespace,
                &query.base_name,
                query.in_type_position,
                unit,
                symbol_id,
                &query.component_path,
            )
        else {
            return self.open_sql_field_completion_at(offset);
        };

        let mut items: Vec<_> = structure_unit
            .semantic()
            .decls()
            .structure_field_infos(structure_id)
            .into_iter()
            .filter(|field| field.name.as_ref().starts_with(query.prefix.as_ref()))
            .map(|field| SelectorCompletionItem {
                name: Arc::clone(&field.name),
                declared_type: field.type_ref.as_ref().map(format_field_type_ref),
                declaration: None,
                kind: match field.shape {
                    StructureFieldShape::Scalar => HoveredComponentKind::Scalar,
                    StructureFieldShape::Structured { structure } => {
                        HoveredComponentKind::Structured {
                            structure_name: Arc::clone(&structure_unit.structure(structure).name),
                        }
                    }
                },
                field_owner_structure_name: Some(Arc::clone(
                    &structure_unit.structure(field.owner).name,
                )),
                insertion: identifier_completion_insertion(field.name.as_ref()),
            })
            .collect();
        items.sort_by(|left, right| left.name.cmp(&right.name));
        Some(SelectorCompletionInfo {
            replace_range: query.replace_range,
            items,
            in_type_position: query.in_type_position,
        })
    }

    fn selector_completion_query_at(&self, offset: usize) -> Option<SelectorCompletionQuery> {
        let query = selector_completion_context(&self.parse, offset)
            .and_then(|context| {
                parse_selector_completion_query(self.text.as_ref(), &self.parse, offset, &context)
            })
            .or_else(|| {
                selector_completion_statement_context(&self.parse, offset).and_then(|context| {
                    parse_selector_completion_query(
                        self.text.as_ref(),
                        &self.parse,
                        offset,
                        &context,
                    )
                })
            })
            .or_else(|| {
                parse_corresponding_mapping_field_query(self.text.as_ref(), &self.parse, offset)
            })?;
        Some(SelectorCompletionQuery {
            scope: innermost_scope_at(&self.symbols, query.replace_range.start),
            base_name: query.base_name,
            base_namespace: query.base_namespace,
            component_path: query.component_path,
            replace_range: query.replace_range,
            prefix: query.prefix,
            in_type_position: query.in_type_position,
        })
    }

    fn bare_where_field_completion_at(&self, offset: usize) -> Option<SelectorCompletionInfo> {
        let query = self.bare_where_field_query_at(offset)?;
        let structure_unit = &self.project.units[query.structure_unit_id.as_usize()];
        let mut items: Vec<_> = structure_unit
            .semantic()
            .decls()
            .structure_field_infos(query.structure_id)
            .into_iter()
            .filter(|field| field.name.as_ref().starts_with(query.prefix.as_ref()))
            .map(|field| SelectorCompletionItem {
                name: Arc::clone(&field.name),
                declared_type: field.type_ref.as_ref().map(format_field_type_ref),
                declaration: None,
                kind: match field.shape {
                    StructureFieldShape::Scalar => HoveredComponentKind::Scalar,
                    StructureFieldShape::Structured { structure } => {
                        HoveredComponentKind::Structured {
                            structure_name: Arc::clone(&structure_unit.structure(structure).name),
                        }
                    }
                },
                field_owner_structure_name: Some(Arc::clone(
                    &structure_unit.structure(field.owner).name,
                )),
                insertion: identifier_completion_insertion(field.name.as_ref()),
            })
            .collect();
        items.sort_by(|left, right| left.name.cmp(&right.name));
        Some(SelectorCompletionInfo {
            replace_range: query.replace_range,
            items,
            in_type_position: false,
        })
    }

    fn open_sql_field_completion_at(&self, offset: usize) -> Option<SelectorCompletionInfo> {
        let query = self.open_sql_field_query_at(offset)?;
        let (structure_unit, structure_id) =
            self.sql_source_structure_for_name(query.scope, &query.source_name)?;
        let mut items: Vec<_> = structure_unit
            .semantic()
            .decls()
            .structure_field_infos(structure_id)
            .into_iter()
            .filter(|field| field.name.as_ref().starts_with(query.prefix.as_ref()))
            .map(|field| SelectorCompletionItem {
                name: Arc::clone(&field.name),
                declared_type: field.type_ref.as_ref().map(format_field_type_ref),
                declaration: None,
                kind: match field.shape {
                    StructureFieldShape::Scalar => HoveredComponentKind::Scalar,
                    StructureFieldShape::Structured { structure } => {
                        HoveredComponentKind::Structured {
                            structure_name: Arc::clone(&structure_unit.structure(structure).name),
                        }
                    }
                },
                field_owner_structure_name: Some(Arc::clone(
                    &structure_unit.structure(field.owner).name,
                )),
                insertion: identifier_completion_insertion(field.name.as_ref()),
            })
            .collect();
        if items.is_empty() {
            return None;
        }
        items.sort_by(|left, right| left.name.cmp(&right.name));
        Some(SelectorCompletionInfo {
            replace_range: query.replace_range,
            items,
            in_type_position: false,
        })
    }

    fn open_sql_source_completion_at(&self, offset: usize) -> Option<CompletionInfo> {
        let query = self.open_sql_source_completion_query_at(offset)?;
        let mut items = Vec::new();
        let mut seen = HashSet::<Arc<str>>::new();

        let mut current = Some(query.scope);
        while let Some(scope_id) = current {
            if let Some(scope_map) = self.scope_index().get(scope_id.as_usize()) {
                for ((namespace, name), symbols) in scope_map {
                    if *namespace != Namespace::Type
                        || !name.as_ref().starts_with(query.prefix.as_ref())
                        || !seen.insert(Arc::clone(name))
                    {
                        continue;
                    }
                    let Some(symbol_id) = symbols.iter().rev().copied().find(|symbol_id| {
                        self.symbols.symbol(*symbol_id).structure.is_some()
                            && self
                                .symbols
                                .symbol(*symbol_id)
                                .kind
                                .occupies(Namespace::Type)
                    }) else {
                        continue;
                    };
                    items.push(CompletionItem::Symbol(symbol_completion_item(
                        self,
                        self.symbols.as_ref(),
                        self.symbols.symbol(symbol_id),
                    )));
                }
            }
            current = self.symbols.scope(scope_id).parent;
        }

        for unit in &self.project.units {
            for symbol in &unit.symbols {
                if symbol.scope != unit.root_scope
                    || symbol.structure.is_none()
                    || !symbol.kind.occupies(Namespace::Type)
                    || !symbol.name.as_ref().starts_with(query.prefix.as_ref())
                    || !seen.insert(Arc::clone(&symbol.name))
                {
                    continue;
                }
                items.push(CompletionItem::Symbol(symbol_completion_item(
                    self, unit, symbol,
                )));
            }
        }

        if items.is_empty() {
            return None;
        }
        items.sort_by(|left, right| completion_item_name(left).cmp(completion_item_name(right)));
        Some(CompletionInfo {
            replace_range: query.replace_range,
            items,
            in_type_position: true,
        })
    }

    fn callable_statement_completion_at(&self, offset: usize) -> Option<CompletionInfo> {
        let query = self.callable_statement_completion_query_at(offset)?;
        let mut items: Vec<CompletionItem> = Vec::new();
        match query.kind {
            CallableCompletionKind::FunctionModule => {
                let mut seen = HashSet::<Arc<str>>::new();
                for unit in self.callable_completion_units() {
                    for function_module in &unit.function_modules {
                        let symbol = unit.symbol(function_module.symbol);
                        if !symbol.name.as_ref().starts_with(query.prefix.as_ref())
                            || !seen.insert(Arc::clone(&symbol.name))
                        {
                            continue;
                        }
                        items.push(CompletionItem::Callable(CallableCompletionItem {
                            name: Arc::clone(&symbol.name),
                            declaration: render_function_module_signature(unit, symbol),
                            kind: CallableCompletionKind::FunctionModule,
                            insertion: function_module_completion_insertion(
                                symbol.name.as_ref(),
                                function_module,
                            ),
                        }));
                    }
                }
                for (function_name, template) in inferred_function_module_templates(self) {
                    if !function_name.as_ref().starts_with(query.prefix.as_ref())
                        || !seen.insert(Arc::clone(&function_name))
                    {
                        continue;
                    }
                    items.push(CompletionItem::Callable(CallableCompletionItem {
                        name: Arc::clone(&function_name),
                        declaration: Some(format!(
                            "CALL FUNCTION '{}' (inferred from project call sites)",
                            function_name
                        )),
                        kind: CallableCompletionKind::FunctionModule,
                        insertion: inferred_function_module_completion_insertion(
                            function_name.as_ref(),
                            &template,
                        ),
                    }));
                }
            }
            CallableCompletionKind::Form => {
                let mut seen = HashSet::<(UnitId, SymbolId)>::new();
                for unit in self.callable_completion_units() {
                    for form_routine in &unit.form_routines {
                        let symbol = unit.symbol(form_routine.symbol);
                        if !symbol.name.as_ref().starts_with(query.prefix.as_ref()) {
                            continue;
                        }
                        let Some((resolved_unit, resolved_symbol_id)) = resolve_symbol_from_context(
                            self,
                            query.scope,
                            Namespace::Routine,
                            &symbol.name,
                            false,
                        ) else {
                            continue;
                        };
                        if resolved_unit.symbol(resolved_symbol_id).kind != SymbolKind::Form
                            || !seen.insert((resolved_unit.unit_id, resolved_symbol_id))
                        {
                            continue;
                        }
                        let resolved_symbol = resolved_unit.symbol(resolved_symbol_id);
                        let Some(resolved_form) = resolved_unit
                            .semantic()
                            .decls()
                            .form_routine(resolved_symbol_id)
                        else {
                            continue;
                        };
                        items.push(CompletionItem::Callable(CallableCompletionItem {
                            name: Arc::clone(&resolved_symbol.name),
                            declaration: render_form_signature(resolved_unit, resolved_symbol),
                            kind: CallableCompletionKind::Form,
                            insertion: form_completion_insertion(
                                resolved_unit,
                                resolved_symbol.name.as_ref(),
                                resolved_form,
                            ),
                        }));
                    }
                }
            }
        }
        if items.is_empty() {
            return None;
        }
        items.sort_by(|left, right| completion_item_name(left).cmp(completion_item_name(right)));
        Some(CompletionInfo {
            replace_range: query.replace_range,
            items,
            in_type_position: false,
        })
    }

    fn callable_statement_completion_query_at(
        &self,
        offset: usize,
    ) -> Option<CallableStatementCompletionQuery> {
        parse_function_module_completion_query(self, offset)
            .or_else(|| parse_form_completion_query(self, offset))
    }

    fn template_completion_at(&self, offset: usize) -> Option<CompletionInfo> {
        if let Some(query) = parse_local_class_template_query(self, offset) {
            let class_name =
                normalized_local_class_template_name(query.kind, query.class_name_hint.as_ref());
            let (detail, insertion) = match query.kind {
                LocalClassTemplateKind::Standard => (
                    "Local class definition".to_string(),
                    local_class_template_completion_insertion(class_name.as_ref()),
                ),
                LocalClassTemplateKind::Test => (
                    "Local test class definition".to_string(),
                    local_test_class_template_completion_insertion(class_name.as_ref()),
                ),
            };
            return Some(CompletionInfo {
                replace_range: query.replace_range,
                items: vec![CompletionItem::Template(TemplateCompletionItem {
                    name: Arc::clone(&class_name),
                    detail: Some(detail),
                    insertion,
                })],
                in_type_position: false,
            });
        }

        if let Some(query) = parse_types_begin_template_query(self, offset) {
            return Some(CompletionInfo {
                replace_range: query.replace_range,
                items: vec![CompletionItem::Template(TemplateCompletionItem {
                    name: Arc::from("BEGIN OF type_name"),
                    detail: Some("TYPES structure scaffold".to_string()),
                    insertion: types_begin_template_completion_insertion(),
                })],
                in_type_position: true,
            });
        }

        let query = parse_method_definition_template_query(self, offset)?;
        Some(CompletionInfo {
            replace_range: query.replace_range,
            items: vec![CompletionItem::Template(TemplateCompletionItem {
                name: Arc::from("methods"),
                detail: Some("Method definition".to_string()),
                insertion: method_definition_template_completion_insertion(),
            })],
            in_type_position: false,
        })
    }

    fn call_target_member_at(
        &self,
        offset: usize,
    ) -> Option<(Range<usize>, &UnitAnalysis, &ClassMemberData)> {
        self.symbols
            .call_sites
            .iter()
            .filter_map(|call_site| match &call_site.target {
                NamedArgumentTarget::ImplicitMethod { method_name }
                | NamedArgumentTarget::Method { method_name, .. } => {
                    let range =
                        call_site_target_name_range(self.text.as_ref(), call_site, method_name)?;
                    (range.start <= offset && offset < range.end).then_some((call_site, range))
                }
                NamedArgumentTarget::Event {
                    qualifier,
                    event_name,
                } => {
                    let range = call_site_event_name_range(
                        self.text.as_ref(),
                        call_site,
                        qualifier.as_ref(),
                        event_name,
                    )?;
                    (range.start <= offset && offset < range.end).then_some((call_site, range))
                }
                _ => None,
            })
            .filter_map(|(call_site, range)| {
                let (member_unit, member) = resolve_call_target_member(self, call_site)?;
                Some((range, member_unit, member))
            })
            .min_by_key(|(range, _, _)| range.end.saturating_sub(range.start))
    }

    fn named_argument_completion_at(&self, offset: usize) -> Option<CompletionInfo> {
        let call_site = self
            .symbols
            .call_sites
            .iter()
            .filter(|call_site| call_site.range.start <= offset && offset <= call_site.range.end)
            .min_by_key(|call_site| call_site.range.end - call_site.range.start)?;
        let (replace_range, prefix, section) =
            named_argument_completion_context(self, call_site, offset)?;
        let callable = resolve_callable_completion_target(self, call_site)?;
        let present_named_parameters: HashSet<_> = call_site
            .arguments
            .iter()
            .filter_map(|argument| argument.name.as_ref())
            .map(|name| name.to_ascii_lowercase())
            .collect();
        let mut items: Vec<_> = match callable {
            CallableCompletionTarget::Method(member) => member
                .parameters
                .iter()
                .filter(|parameter| {
                    call_section_matches_parameter(section, parameter.section)
                        && !present_named_parameters.contains(parameter.name.as_ref())
                        && parameter.name.as_ref().starts_with(prefix.as_ref())
                })
                .map(|parameter| {
                    CompletionItem::NamedArgument(NamedArgumentCompletionItem {
                        name: Arc::clone(&parameter.name),
                        declared_type: parameter_completion_declared_type(parameter),
                        declaration: Some(format_parameter_completion_declaration(parameter)),
                        insertion: named_argument_completion_insertion(parameter.name.as_ref()),
                    })
                })
                .collect(),
            CallableCompletionTarget::Event(member) => member
                .parameters
                .iter()
                .filter(|parameter| {
                    call_section_matches_event_parameter(section, parameter)
                        && !present_named_parameters.contains(parameter.name.as_ref())
                        && parameter.name.as_ref().starts_with(prefix.as_ref())
                })
                .map(|parameter| {
                    CompletionItem::NamedArgument(NamedArgumentCompletionItem {
                        name: Arc::clone(&parameter.name),
                        declared_type: parameter_completion_declared_type(parameter),
                        declaration: Some(format_parameter_completion_declaration(parameter)),
                        insertion: named_argument_completion_insertion(parameter.name.as_ref()),
                    })
                })
                .collect(),
            CallableCompletionTarget::Function(function_module) => function_module
                .parameters
                .iter()
                .filter(|parameter| {
                    call_section_matches_function_parameter(section, parameter)
                        && !present_named_parameters.contains(parameter.name.as_ref())
                        && parameter.name.as_ref().starts_with(prefix.as_ref())
                })
                .map(|parameter| {
                    CompletionItem::NamedArgument(NamedArgumentCompletionItem {
                        name: Arc::clone(&parameter.name),
                        declared_type: function_module_parameter_completion_declared_type(
                            parameter,
                        ),
                        declaration: Some(format_function_module_parameter_completion_declaration(
                            parameter,
                        )),
                        insertion: named_argument_completion_insertion(parameter.name.as_ref()),
                    })
                })
                .collect(),
        };
        if items.is_empty() {
            return None;
        }
        items.sort_by(|left, right| completion_item_name(left).cmp(completion_item_name(right)));
        Some(CompletionInfo {
            replace_range,
            items,
            in_type_position: false,
        })
    }

    fn method_parameter_completion_at(&self, offset: usize) -> Option<CompletionInfo> {
        if self
            .symbols
            .call_sites
            .iter()
            .any(|call_site| call_site.range.start <= offset && offset <= call_site.range.end)
        {
            return None;
        }
        let context = self.bare_identifier_completion_context(offset)?;
        if context.in_type_position {
            return None;
        }
        let scope = innermost_scope_at(&self.symbols, context.replace_range.start);
        let method_scope = enclosing_method_scope_with_owner(&self.symbols, scope)?;
        let mut items: Vec<_> = self
            .symbols
            .symbols
            .iter()
            .filter(|symbol| symbol.kind == SymbolKind::Parameter && symbol.scope == method_scope)
            .filter(|symbol| symbol.name.as_ref().starts_with(context.prefix.as_ref()))
            .map(|symbol| {
                CompletionItem::NamedArgument(NamedArgumentCompletionItem {
                    name: Arc::clone(&symbol.name),
                    declared_type: symbol_completion_declared_type(symbol),
                    declaration: Some(format_symbol_completion_declaration(symbol)),
                    insertion: identifier_completion_insertion(symbol.name.as_ref()),
                })
            })
            .collect();
        if items.is_empty() {
            return None;
        }
        items.sort_by(|left, right| completion_item_name(left).cmp(completion_item_name(right)));
        Some(CompletionInfo {
            replace_range: context.replace_range,
            items,
            in_type_position: false,
        })
    }

    fn bare_identifier_completion_at(&self, offset: usize) -> Option<CompletionInfo> {
        let mut completion = self.method_parameter_completion_at(offset);
        merge_completion(&mut completion, self.visible_symbol_completion_at(offset));
        merge_completion(&mut completion, self.class_member_completion_at(offset));
        merge_completion(&mut completion, self.keyword_completion_at(offset));
        completion
    }

    fn class_member_completion_at(&self, offset: usize) -> Option<CompletionInfo> {
        let context = self.bare_identifier_completion_context(offset)?;
        if context.in_type_position {
            return None;
        }
        let scope = innermost_scope_at(&self.symbols, context.replace_range.start);
        let method_scope = enclosing_method_scope_with_owner(&self.symbols, scope)?;
        let class_symbol = enclosing_class_owner(&self.symbols, method_scope)?;
        let requires_static = lookup_scope_chain(
            &self.symbols,
            self.scope_index(),
            method_scope,
            Namespace::Value,
            &Arc::<str>::from("me"),
        )
        .is_none();
        let mut items: Vec<_> =
            collect_class_value_members_in_hierarchy(self, self.symbols.as_ref(), class_symbol)
                .into_iter()
                .filter(|(member_unit, member)| {
                    (!requires_static || member.is_static)
                        && class_member_visible_to(
                            self,
                            self.symbols.as_ref(),
                            scope,
                            member_unit,
                            member,
                        )
                        && member.name.as_ref().starts_with(context.prefix.as_ref())
                })
                .map(|(member_unit, member)| {
                    CompletionItem::Selector(SelectorCompletionItem {
                        name: Arc::clone(&member.name),
                        declared_type: None,
                        declaration: Some(format_class_member_signature(member_unit, member)),
                        kind: hovered_component_kind_for_class_member(member),
                        field_owner_structure_name: None,
                        insertion: if member.kind == ClassMemberKind::Method {
                            callable_completion_insertion(member)
                        } else {
                            identifier_completion_insertion(member.name.as_ref())
                        },
                    })
                })
                .collect();
        if items.is_empty() {
            return None;
        }
        items.sort_by(|left, right| completion_item_name(left).cmp(completion_item_name(right)));
        Some(CompletionInfo {
            replace_range: context.replace_range,
            items,
            in_type_position: false,
        })
    }

    fn visible_symbol_completion_at(&self, offset: usize) -> Option<CompletionInfo> {
        let context = self.bare_identifier_completion_context(offset)?;
        let scope = innermost_scope_at(&self.symbols, context.replace_range.start);
        let namespace = if context.in_type_position {
            Namespace::Type
        } else {
            Namespace::Value
        };
        let mut items = Vec::new();
        let mut seen = HashSet::<Arc<str>>::new();
        let mut current = Some(scope);
        while let Some(scope_id) = current {
            if let Some(scope_map) = self.scope_index().get(scope_id.as_usize()) {
                for ((candidate_namespace, name), symbols) in scope_map {
                    if *candidate_namespace != namespace
                        || !name.as_ref().starts_with(context.prefix.as_ref())
                    {
                        continue;
                    }
                    let Some(symbol_id) = symbols.iter().rev().copied().find(|symbol_id| {
                        symbol_completion_kind_supported(
                            self.symbols.symbol(*symbol_id).kind,
                            namespace,
                        )
                    }) else {
                        continue;
                    };
                    if !seen.insert(Arc::clone(name)) {
                        continue;
                    }
                    let symbol = self.symbols.symbol(symbol_id);
                    items.push(CompletionItem::Symbol(symbol_completion_item(
                        self,
                        self.symbols.as_ref(),
                        symbol,
                    )));
                }
            }
            current = self.symbols.scope(scope_id).parent;
        }
        if items.is_empty() {
            return None;
        }
        items.sort_by(|left, right| completion_item_name(left).cmp(completion_item_name(right)));
        Some(CompletionInfo {
            replace_range: context.replace_range,
            items,
            in_type_position: context.in_type_position,
        })
    }

    fn keyword_completion_at(&self, offset: usize) -> Option<CompletionInfo> {
        let context = self.bare_identifier_completion_context(offset)?;
        let items = keyword_completion::keyword_completion_items(context.prefix.as_ref());
        if items.is_empty() {
            return None;
        }
        Some(CompletionInfo {
            replace_range: context.replace_range,
            items: items.into_iter().map(CompletionItem::Keyword).collect(),
            in_type_position: context.in_type_position,
        })
    }

    fn bare_identifier_completion_context(
        &self,
        offset: usize,
    ) -> Option<BareIdentifierCompletionContext> {
        let range = statement_query_range(&self.parse, offset)?;
        let (token_start, token_end) = token_window_for_range(&self.parse, &range)?;
        if let Some((replace_range, prefix)) =
            partial_field_symbol_prefix(self.text.as_ref(), range.start, offset)
        {
            return Some(BareIdentifierCompletionContext {
                replace_range,
                prefix,
                in_type_position: false,
            });
        }
        let prefix_idx = prefix_token_at_offset(&self.parse, token_start, token_end, offset)?;
        let token = &self.parse.tokens[prefix_idx];
        if previous_significant_token(&self.parse, token_start, prefix_idx).is_some_and(
            |prev_idx| {
                matches!(
                    self.parse.tokens[prev_idx].kind.as_str(),
                    "Minus" | "Arrow" | "Tilde" | "FatArrow"
                )
            },
        ) {
            return None;
        }
        let prefix_end = offset.min(token.range.end);
        let replace_range = token.range.start..prefix_end;
        let selector_context = selector_completion_context(&self.parse, offset);
        let parsed_type_position = selector_context
            .as_ref()
            .is_some_and(|context| context.in_type_position);
        let in_type_position = parsed_type_position
            || (offset_is_in_error_node(&self.parse, offset)
                && bare_identifier_token_context_is_type_position(
                    &self.parse,
                    self.text.as_ref(),
                    token_start,
                    prefix_idx,
                ));
        Some(BareIdentifierCompletionContext {
            in_type_position,
            prefix: Arc::<str>::from(self.text[token.range.start..prefix_end].to_ascii_lowercase()),
            replace_range,
        })
    }

    fn bare_where_field_target_at(&self, offset: usize) -> Option<BareWhereFieldTarget> {
        let query = self.bare_where_field_query_at(offset)?;
        let (token_start, token_end) =
            token_window_for_range(&self.parse, &statement_query_range(&self.parse, offset)?)?;
        let token_idx = prefix_token_at_offset(&self.parse, token_start, token_end, offset)?;
        let token = &self.parse.tokens[token_idx];
        let field_name = Arc::<str>::from(token.lexeme(self.text.as_ref()).to_ascii_lowercase());
        let structure_unit = &self.project.units[query.structure_unit_id.as_usize()];
        let field = resolve_structure_field_info_with_scope_index(
            self,
            self.scope_index(),
            structure_unit,
            query.scope,
            query.structure_id,
            field_name.as_ref(),
        )?;
        Some(BareWhereFieldTarget {
            structure_unit_id: query.structure_unit_id,
            field,
            range: token.range.clone(),
        })
    }

    fn bare_where_field_query_at(&self, offset: usize) -> Option<BareWhereFieldQuery> {
        let statement_range = statement_query_range(&self.parse, offset)?;
        let (token_start, token_end) = token_window_for_range(&self.parse, &statement_range)?;
        let mut parsed = parse_bare_where_field_query(
            self.text.as_ref(),
            &self.parse,
            token_start,
            token_end,
            offset,
        )?;
        parsed.scope = innermost_scope_at(&self.symbols, statement_range.start);
        let source_access = access_from_selector_query(
            parsed.scope,
            &parsed.base_name,
            parsed.base_namespace,
            &parsed.component_path,
        );
        let (structure_unit, structure_id) = resolve_field_access_structure_with_scope_index(
            self,
            self.scope_index(),
            &source_access,
        )?;
        Some(BareWhereFieldQuery {
            scope: parsed.scope,
            structure_unit_id: structure_unit.unit_id,
            structure_id,
            replace_range: parsed.replace_range,
            prefix: parsed.prefix,
        })
    }

    fn open_sql_field_query_at(&self, offset: usize) -> Option<OpenSqlFieldCompletionQuery> {
        let statement_range = statement_query_range(&self.parse, offset)?;
        let (token_start, token_end) = token_window_for_range(&self.parse, &statement_range)?;
        let mut query = parse_open_sql_field_completion_query(
            self.text.as_ref(),
            &self.parse,
            token_start,
            token_end,
            offset,
        )?;
        query.scope = innermost_scope_at(&self.symbols, statement_range.start);
        Some(query)
    }

    fn open_sql_source_completion_query_at(
        &self,
        offset: usize,
    ) -> Option<OpenSqlSourceCompletionQuery> {
        let statement_range = statement_query_range(&self.parse, offset)?;
        let (token_start, token_end) = token_window_for_range(&self.parse, &statement_range)?;
        let mut query = parse_open_sql_source_completion_query(
            self.text.as_ref(),
            &self.parse,
            token_start,
            token_end,
            offset,
        )?;
        query.scope = innermost_scope_at(&self.symbols, statement_range.start);
        Some(query)
    }

    fn callable_completion_units(&self) -> Vec<&UnitAnalysis> {
        let current_unit = self.symbols.as_ref();
        let mut units = Vec::with_capacity(self.project.units.len() + 1);
        units.push(current_unit);
        units.extend(
            self.project
                .units
                .iter()
                .filter(|unit| unit.unit_id != current_unit.unit_id),
        );
        units
    }
}

impl LineIndex {
    pub fn new(text: &str) -> Self {
        let mut line_starts =
            Vec::with_capacity(text.bytes().filter(|byte| *byte == b'\n').count() + 1);
        line_starts.push(0);
        for (idx, byte) in text.bytes().enumerate() {
            if byte == b'\n' {
                line_starts.push(idx + 1);
            }
        }
        Self { line_starts }
    }

    pub fn offset_to_line_utf16_position(&self, text: &str, offset: usize) -> Option<(u32, u32)> {
        if offset > text.len() {
            return None;
        }
        let line = self.line_for_offset(offset)?;
        let (line_start, line_end) = self.line_bounds(text, line)?;
        if offset < line_start || offset > line_end {
            return None;
        }
        let character = text[line_start..offset]
            .chars()
            .map(|ch| ch.len_utf16() as u32)
            .sum();
        Some((line, character))
    }

    pub fn line_utf16_position_to_offset(
        &self,
        text: &str,
        line: u32,
        character: u32,
    ) -> Option<usize> {
        let (line_start, line_end) = self.line_bounds(text, line)?;
        let line_text = &text[line_start..line_end];
        let mut utf16_units = 0u32;
        for (idx, ch) in line_text.char_indices() {
            if utf16_units == character {
                return Some(line_start + idx);
            }
            utf16_units += ch.len_utf16() as u32;
            if utf16_units > character {
                return None;
            }
        }
        (utf16_units == character).then_some(line_start + line_text.len())
    }

    fn line_for_offset(&self, offset: usize) -> Option<u32> {
        if self.line_starts.is_empty() {
            return None;
        }
        let line = match self.line_starts.binary_search(&offset) {
            Ok(line) => line,
            Err(next_line) => next_line.checked_sub(1)?,
        };
        u32::try_from(line).ok()
    }

    fn line_bounds(&self, text: &str, line: u32) -> Option<(usize, usize)> {
        let line = usize::try_from(line).ok()?;
        let line_start = *self.line_starts.get(line)?;
        let next_line_start = self
            .line_starts
            .get(line + 1)
            .copied()
            .unwrap_or(text.len());
        let mut line_end = next_line_start;
        if line_end > line_start && text.as_bytes().get(line_end - 1) == Some(&b'\n') {
            line_end -= 1;
        }
        if line_end > line_start && text.as_bytes().get(line_end - 1) == Some(&b'\r') {
            line_end -= 1;
        }
        Some((line_start, line_end))
    }
}

impl<'a> SemanticTokenLookupContext<'a> {
    pub fn classify_field_access_segment(
        &self,
        access: &abap_symbols::FieldAccess,
        segment_index: usize,
    ) -> Option<HoveredComponentKind> {
        classify_field_access_segment_with_scope_index(
            self.snapshot,
            self.scope_index,
            access,
            segment_index,
        )
    }

    pub fn has_named_argument_parameter(&self, access: &NamedArgumentAccess) -> bool {
        resolve_named_argument_parameter_with_scope_index(self.snapshot, self.scope_index, access)
            .is_some()
    }
}

fn format_field_type_ref(type_ref: &abap_symbols::FieldTypeRefData) -> String {
    let keyword = match type_ref.namespace {
        Namespace::Type => "TYPE",
        Namespace::Value => "LIKE",
        Namespace::Routine => "TYPE",
    };
    let mut rendered = String::from(keyword);
    if type_ref.is_ref {
        rendered.push_str(" REF TO ");
    } else {
        rendered.push(' ');
    }
    rendered.push_str(type_ref.base_name.as_ref());
    for segment in &type_ref.field_path {
        rendered.push('-');
        rendered.push_str(segment.as_ref());
    }
    rendered
}

fn format_hover_type_clause(rendered_type: &str) -> String {
    format!("```abap\n{rendered_type}\n```")
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SymbolTypePresentation {
    rendered_clause: String,
    hint_label: String,
}

fn type_presentation_from_display(
    declared_type: Option<&FieldTypeRefData>,
    display: &str,
) -> SymbolTypePresentation {
    let keyword = match declared_type.map(|type_ref| type_ref.namespace) {
        Some(Namespace::Value) => "LIKE",
        _ => "TYPE",
    };
    SymbolTypePresentation {
        rendered_clause: format!("{keyword} {}", display.trim()),
        hint_label: display.trim().to_string(),
    }
}

fn strip_type_clause_keyword(rendered_clause: &str) -> &str {
    rendered_clause
        .strip_prefix("TYPE ")
        .or_else(|| rendered_clause.strip_prefix("LIKE "))
        .unwrap_or(rendered_clause)
}

fn symbol_inline_explicit_type_display(
    snapshot: &AnalysisSnapshot,
    decl_range: &Range<usize>,
) -> Option<String> {
    let mut stack = vec![snapshot.parse.file.root()];
    while let Some(node) = stack.pop() {
        if snapshot.parse.file.kind(node) == SyntaxKind::DataInlineDecl {
            let name_matches = snapshot.parse.file.children(node).any(|child| {
                snapshot.parse.file.kind(child) == SyntaxKind::DataDeclName
                    && snapshot.parse.file.range(child) == *decl_range
            });
            if name_matches {
                let rhs_expr = snapshot.parse.file.children(node).find(|&child| {
                    !matches!(
                        snapshot.parse.file.kind(child),
                        SyntaxKind::Token | SyntaxKind::DataDeclName
                    )
                })?;
                let constructor_node =
                    if snapshot.parse.file.kind(rhs_expr) == SyntaxKind::ConstructorExpr {
                        Some(rhs_expr)
                    } else {
                        snapshot
                            .parse
                            .file
                            .find_first_kind(rhs_expr, SyntaxKind::ConstructorExpr)
                    }?;
                let constructor = ConstructorExpr::cast(SyntaxNodeRef::new(
                    &snapshot.parse.file,
                    constructor_node,
                ))?;
                let type_ref = constructor.type_ref()?;
                let display = type_ref.display_text(snapshot.text.as_ref())?.trim();
                if display == "#" {
                    return None;
                }
                let keyword = constructor.keyword(snapshot.text.as_ref())?;
                return Some(if keyword.as_ref() == "new" {
                    format!("REF TO {display}")
                } else {
                    display.to_string()
                });
            }
        }

        for child in snapshot.parse.file.children(node) {
            stack.push(child);
        }
    }
    None
}

fn symbol_type_presentation(
    snapshot: Option<&AnalysisSnapshot>,
    symbol: &SymbolData,
) -> Option<SymbolTypePresentation> {
    if let Some(display) = symbol.type_clause_display.as_deref() {
        return Some(type_presentation_from_display(
            symbol.declared_type.as_ref(),
            display,
        ));
    }
    if let Some(snapshot) = snapshot
        && let Some(display) = symbol_inline_explicit_type_display(snapshot, &symbol.decl_range)
    {
        return Some(type_presentation_from_display(
            symbol.declared_type.as_ref(),
            &display,
        ));
    }
    let type_ref = symbol.declared_type.as_ref()?;
    let rendered_clause = format_field_type_ref(type_ref);
    Some(SymbolTypePresentation {
        hint_label: strip_type_clause_keyword(&rendered_clause).to_string(),
        rendered_clause,
    })
}

fn symbol_hover_type_clause(
    snapshot: Option<&AnalysisSnapshot>,
    unit: &UnitAnalysis,
    symbol: &SymbolData,
) -> Option<String> {
    if let Some(type_presentation) = symbol_type_presentation(snapshot, symbol) {
        return Some(format_hover_type_clause(&type_presentation.rendered_clause));
    }
    let structure_id = symbol.structure?;
    let name = unit.structure(structure_id).name.as_ref();
    Some(format_hover_type_clause(&format!("TYPE {name}")))
}

fn symbol_inlay_type_presentation(
    snapshot: Option<&AnalysisSnapshot>,
    symbol: &SymbolData,
) -> Option<SymbolTypePresentation> {
    symbol_type_presentation(snapshot, symbol)
}

fn try_format_method_signature(signature: &str) -> Option<String> {
    let tokens: Vec<&str> = signature.split_whitespace().collect();
    if tokens.len() < 2 {
        return None;
    }

    let first_section_idx = tokens
        .iter()
        .position(|token| {
            matches!(
                token.to_ascii_uppercase().as_str(),
                "IMPORTING"
                    | "EXPORTING"
                    | "CHANGING"
                    | "RECEIVING"
                    | "RETURNING"
                    | "RAISING"
                    | "EXCEPTIONS"
            )
        })
        .unwrap_or(tokens.len());
    if first_section_idx >= tokens.len() {
        return None;
    }

    let header = tokens[..first_section_idx].join(" ");
    if !matches!(
        header.split_whitespace().next().map(|token| token.to_ascii_uppercase()),
        Some(keyword) if keyword == "METHODS" || keyword == "CLASS-METHODS"
    ) {
        return None;
    }

    let mut lines = vec![header];
    let mut idx = first_section_idx;
    while idx < tokens.len() {
        let section = tokens[idx].to_ascii_uppercase();
        idx += 1;

        let next_section_idx = tokens[idx..]
            .iter()
            .position(|token| {
                matches!(
                    token.to_ascii_uppercase().as_str(),
                    "IMPORTING"
                        | "EXPORTING"
                        | "CHANGING"
                        | "RECEIVING"
                        | "RETURNING"
                        | "RAISING"
                        | "EXCEPTIONS"
                )
            })
            .map(|offset| idx + offset)
            .unwrap_or(tokens.len());
        let section_tokens = &tokens[idx..next_section_idx];
        lines.push(format!("  {section}"));

        match section.as_str() {
            "IMPORTING" | "EXPORTING" | "CHANGING" | "RECEIVING" | "RETURNING" => {
                let mut params: Vec<Vec<&str>> = Vec::new();
                let mut cursor = 0usize;
                while cursor < section_tokens.len() {
                    let start = cursor;
                    cursor += 1;
                    while cursor < section_tokens.len()
                        && !matches!(
                            section_tokens[cursor].to_ascii_uppercase().as_str(),
                            "TYPE" | "LIKE"
                        )
                    {
                        cursor += 1;
                    }
                    if cursor < section_tokens.len() {
                        cursor += 1;
                        while cursor < section_tokens.len()
                            && !is_method_param_start_at(section_tokens, cursor)
                        {
                            cursor += 1;
                        }
                    }
                    params.push(section_tokens[start..cursor].to_vec());
                }

                let left_width = params
                    .iter()
                    .map(|param| method_param_left_right(param).0.len())
                    .max()
                    .unwrap_or(0);
                for param in params {
                    let (left, right) = method_param_left_right(&param);
                    if right.is_empty() {
                        lines.push(format!("    {left}"));
                    } else {
                        lines.push(format!("    {left:<left_width$} {right}"));
                    }
                }
            }
            "RAISING" | "EXCEPTIONS" => {
                for token in section_tokens {
                    lines.push(format!("    {token}"));
                }
            }
            _ => {
                if !section_tokens.is_empty() {
                    lines.push(format!("    {}", section_tokens.join(" ")));
                }
            }
        }

        idx = next_section_idx;
    }

    Some(lines.join("\n"))
}

fn is_method_param_start_at(tokens: &[&str], idx: usize) -> bool {
    let Some(token) = tokens.get(idx).copied() else {
        return false;
    };
    token == "!"
        || token.starts_with("VALUE(")
        || token.starts_with("REFERENCE(")
        || tokens
            .get(idx + 1)
            .is_some_and(|next| matches!(next.to_ascii_uppercase().as_str(), "TYPE" | "LIKE"))
}

fn method_param_left_right(tokens: &[&str]) -> (String, String) {
    let split_idx = tokens
        .iter()
        .position(|token| matches!(token.to_ascii_uppercase().as_str(), "TYPE" | "LIKE"));
    match split_idx {
        Some(idx) => (tokens[..idx].join(" "), tokens[idx..].join(" ")),
        None => (tokens.join(" "), String::new()),
    }
}

fn structured_member_keyword(signature: &str) -> &'static str {
    let upper = signature.to_ascii_uppercase();
    if upper.contains("CONSTANTS") {
        "CONSTANTS"
    } else if upper.contains("STATICS") {
        "STATICS"
    } else if upper.contains("CLASS-DATA") || upper.contains("CLASS - DATA") {
        "CLASS-DATA"
    } else {
        "DATA"
    }
}

fn render_structured_member_field(
    unit: &UnitAnalysis,
    field: &StructureFieldData,
    indent: usize,
    lines: &mut Vec<String>,
) {
    let padding = " ".repeat(indent);
    if let Some(structure_id) = field.structure {
        lines.push(format!("{padding}BEGIN OF {},", field.name));
        for nested in &unit.structure(structure_id).fields {
            render_structured_member_field(unit, nested, indent + 2, lines);
        }
        lines.push(format!("{padding}END OF {},", field.name));
        return;
    }

    let mut rendered = format!("{padding}{}", field.name);
    if let Some(type_ref) = field.type_ref.as_ref() {
        rendered.push(' ');
        rendered.push_str(&format_field_type_ref(type_ref));
    }
    if let Some(value) = field.value_clause_display.as_ref() {
        rendered.push_str(" VALUE ");
        rendered.push_str(value.trim());
    }
    rendered.push(',');
    lines.push(rendered);
}

fn format_structured_class_member_signature(
    unit: &UnitAnalysis,
    member: &ClassMemberData,
) -> Option<String> {
    let structure_id = member.structure?;
    let structure = unit.structure(structure_id);
    let keyword = structured_member_keyword(member.signature.as_ref());
    let mut lines = vec![
        format!("{keyword}:"),
        format!("  BEGIN OF {},", structure.name),
    ];
    for field in &structure.fields {
        render_structured_member_field(unit, field, 4, &mut lines);
    }
    lines.push(format!("  END OF {}.", structure.name));
    Some(lines.join("\n"))
}

fn format_class_member_signature(unit: &UnitAnalysis, member: &ClassMemberData) -> String {
    if member.kind == ClassMemberKind::Method
        && let Some(formatted) = try_format_method_signature(member.signature.as_ref())
    {
        return formatted;
    }
    if let Some(formatted) = format_structured_class_member_signature(unit, member) {
        return formatted;
    }
    member.signature.to_string()
}

fn identifier_completion_insertion(name: &str) -> CompletionInsertion {
    CompletionInsertion {
        plain_text: name.to_string(),
        snippet_text: None,
    }
}

fn callable_completion_insertion(member: &ClassMemberData) -> CompletionInsertion {
    fn push_call_section(
        plain_lines: &mut Vec<String>,
        snippet_lines: &mut Vec<String>,
        keyword: &str,
        parameters: &[&ClassMemberParameterData],
        tabstop: &mut usize,
    ) {
        if parameters.is_empty() {
            return;
        }
        plain_lines.push(format!("  {keyword}"));
        snippet_lines.push(format!("  {keyword}"));
        for parameter in parameters {
            plain_lines.push(format!("    {} = ", parameter.name));
            snippet_lines.push(format!("    {} = ${{{tabstop}}}", parameter.name));
            *tabstop += 1;
        }
    }

    let call_exporting: Vec<_> = member
        .parameters
        .iter()
        .filter(|parameter| {
            parameter_is_required(parameter.section, parameter.is_optional)
                && parameter.section == MethodParameterSection::Importing
        })
        .collect();
    let call_importing: Vec<_> = member
        .parameters
        .iter()
        .filter(|parameter| parameter.section == MethodParameterSection::Exporting)
        .collect();
    let call_changing: Vec<_> = member
        .parameters
        .iter()
        .filter(|parameter| {
            parameter_is_required(parameter.section, parameter.is_optional)
                && parameter.section == MethodParameterSection::Changing
        })
        .collect();
    if call_exporting.is_empty() && call_importing.is_empty() && call_changing.is_empty() {
        return CompletionInsertion {
            plain_text: format!("{}( )", member.name),
            snippet_text: Some(format!("{}( )$0", member.name)),
        };
    }

    let mut plain_lines = vec![format!("{}(", member.name)];
    let mut snippet_lines = vec![format!("{}(", member.name)];
    let mut tabstop = 1usize;

    if call_importing.is_empty() && call_changing.is_empty() {
        for parameter in call_exporting {
            plain_lines.push(format!("  {} = ", parameter.name));
            snippet_lines.push(format!("  {} = ${{{tabstop}}}", parameter.name));
            tabstop += 1;
        }
    } else {
        push_call_section(
            &mut plain_lines,
            &mut snippet_lines,
            "EXPORTING",
            &call_exporting,
            &mut tabstop,
        );
        push_call_section(
            &mut plain_lines,
            &mut snippet_lines,
            "IMPORTING",
            &call_importing,
            &mut tabstop,
        );
        push_call_section(
            &mut plain_lines,
            &mut snippet_lines,
            "CHANGING",
            &call_changing,
            &mut tabstop,
        );
    }
    plain_lines.push(")".to_string());
    snippet_lines.push(")$0".to_string());

    CompletionInsertion {
        plain_text: plain_lines.join("\n"),
        snippet_text: Some(snippet_lines.join("\n")),
    }
}

fn function_module_completion_insertion(
    function_name: &str,
    function_module: &FunctionModuleData,
) -> CompletionInsertion {
    fn push_named_section(
        plain_lines: &mut Vec<String>,
        snippet_lines: &mut Vec<String>,
        keyword: &str,
        parameters: &[&FunctionModuleParameterData],
        tabstop: &mut usize,
    ) {
        if parameters.is_empty() {
            return;
        }
        plain_lines.push(format!("  {keyword}"));
        snippet_lines.push(format!("  {keyword}"));
        for parameter in parameters {
            plain_lines.push(format!("    {} = ", parameter.name));
            snippet_lines.push(format!("    {} = ${{{tabstop}}}", parameter.name));
            *tabstop += 1;
        }
    }

    fn push_exception_section(
        plain_lines: &mut Vec<String>,
        snippet_lines: &mut Vec<String>,
        exceptions: &[FunctionModuleExceptionData],
        tabstop: &mut usize,
    ) {
        if exceptions.is_empty() {
            return;
        }
        plain_lines.push("  EXCEPTIONS".to_string());
        snippet_lines.push("  EXCEPTIONS".to_string());
        for (idx, exception) in exceptions.iter().enumerate() {
            let code = idx + 1;
            plain_lines.push(format!("    {} = {code}", exception.name));
            snippet_lines.push(format!("    {} = ${{{tabstop}:{code}}}", exception.name));
            *tabstop += 1;
        }
    }

    let call_exporting: Vec<_> = function_module
        .parameters
        .iter()
        .filter(|parameter| {
            parameter.section == FunctionModuleParameterSection::Importing
                && !parameter.is_optional
                && !parameter.has_default_value
        })
        .collect();
    let call_importing: Vec<_> = function_module
        .parameters
        .iter()
        .filter(|parameter| parameter.section == FunctionModuleParameterSection::Exporting)
        .collect();
    let call_changing: Vec<_> = function_module
        .parameters
        .iter()
        .filter(|parameter| parameter.section == FunctionModuleParameterSection::Changing)
        .collect();
    let call_tables: Vec<_> = function_module
        .parameters
        .iter()
        .filter(|parameter| parameter.section == FunctionModuleParameterSection::Tables)
        .collect();

    let mut plain_lines = vec![format!("{function_name}'")];
    let mut snippet_lines = vec![format!("{function_name}'")];
    let mut tabstop = 1usize;

    push_named_section(
        &mut plain_lines,
        &mut snippet_lines,
        "EXPORTING",
        &call_exporting,
        &mut tabstop,
    );
    push_named_section(
        &mut plain_lines,
        &mut snippet_lines,
        "IMPORTING",
        &call_importing,
        &mut tabstop,
    );
    push_named_section(
        &mut plain_lines,
        &mut snippet_lines,
        "CHANGING",
        &call_changing,
        &mut tabstop,
    );
    push_named_section(
        &mut plain_lines,
        &mut snippet_lines,
        "TABLES",
        &call_tables,
        &mut tabstop,
    );
    push_exception_section(
        &mut plain_lines,
        &mut snippet_lines,
        &function_module.exceptions,
        &mut tabstop,
    );

    if let Some(last) = plain_lines.last_mut() {
        last.push('.');
    }
    if let Some(last) = snippet_lines.last_mut() {
        last.push_str(".$0");
    }

    CompletionInsertion {
        plain_text: plain_lines.join("\n"),
        snippet_text: Some(snippet_lines.join("\n")),
    }
}

fn inferred_function_module_templates(
    snapshot: &AnalysisSnapshot,
) -> HashMap<Arc<str>, InferredFunctionModuleCallTemplate> {
    let mut templates = HashMap::<Arc<str>, InferredFunctionModuleCallTemplate>::new();
    for unit in snapshot.callable_completion_units() {
        for call_site in &unit.call_sites {
            let NamedArgumentTarget::Function { function_name } = &call_site.target else {
                continue;
            };
            let template = inferred_function_module_template_from_call_site(call_site);
            if template.sections.is_empty() {
                continue;
            }
            let score = inferred_function_module_template_score(&template);
            let replace = templates
                .get(function_name)
                .is_none_or(|current| inferred_function_module_template_score(current) < score);
            if replace {
                templates.insert(Arc::clone(function_name), template);
            }
        }
    }
    templates
}

fn inferred_function_module_template_from_call_site(
    call_site: &abap_symbols::CallSiteData,
) -> InferredFunctionModuleCallTemplate {
    let mut sections = Vec::new();
    for section in [
        NamedArgumentSection::Exporting,
        NamedArgumentSection::Importing,
        NamedArgumentSection::Changing,
        NamedArgumentSection::Tables,
        NamedArgumentSection::Exceptions,
    ] {
        let mut names: Vec<_> = call_site
            .arguments
            .iter()
            .filter(|argument| argument.section == Some(section))
            .filter_map(|argument| argument.name.as_ref().map(|name| (argument.ordinal, name)))
            .collect();
        names.sort_by_key(|(ordinal, _)| *ordinal);
        names.dedup_by(|left, right| left.1 == right.1);
        if !names.is_empty() {
            sections.push((
                section,
                names
                    .into_iter()
                    .map(|(_, name)| Arc::clone(name))
                    .collect(),
            ));
        }
    }
    InferredFunctionModuleCallTemplate { sections }
}

fn inferred_function_module_template_score(template: &InferredFunctionModuleCallTemplate) -> usize {
    template
        .sections
        .iter()
        .map(|(_, names)| names.len())
        .sum::<usize>()
}

fn inferred_function_module_completion_insertion(
    function_name: &str,
    template: &InferredFunctionModuleCallTemplate,
) -> CompletionInsertion {
    let mut plain_lines = vec![format!("{function_name}'")];
    let mut snippet_lines = vec![format!("{function_name}'")];
    let mut tabstop = 1usize;

    for (section, names) in &template.sections {
        let keyword = match section {
            NamedArgumentSection::Exporting => "EXPORTING",
            NamedArgumentSection::Importing => "IMPORTING",
            NamedArgumentSection::Changing => "CHANGING",
            NamedArgumentSection::Tables => "TABLES",
            NamedArgumentSection::Exceptions => "EXCEPTIONS",
            NamedArgumentSection::Receiving => "RECEIVING",
        };
        if names.is_empty() {
            continue;
        }
        plain_lines.push(format!("  {keyword}"));
        snippet_lines.push(format!("  {keyword}"));
        for (idx, name) in names.iter().enumerate() {
            if *section == NamedArgumentSection::Exceptions {
                let code = idx + 1;
                plain_lines.push(format!("    {name} = {code}"));
                snippet_lines.push(format!("    {name} = ${{{tabstop}:{code}}}"));
            } else {
                plain_lines.push(format!("    {name} = "));
                snippet_lines.push(format!("    {name} = ${{{tabstop}}}"));
            }
            tabstop += 1;
        }
    }

    if let Some(last) = plain_lines.last_mut() {
        last.push('.');
    }
    if let Some(last) = snippet_lines.last_mut() {
        last.push_str(".$0");
    }

    CompletionInsertion {
        plain_text: plain_lines.join("\n"),
        snippet_text: Some(snippet_lines.join("\n")),
    }
}

pub fn function_module_completion_items_from_source(
    uri: &str,
    text: &str,
    object_name: Option<Arc<str>>,
) -> Vec<CallableCompletionItem> {
    let parse = Arc::new(parse(text));
    let local = local_analysis_with_object_name(
        analyze_unit_local_state(UnitId(0), Arc::from(uri), text, parse.as_ref()),
        object_name.as_ref(),
    );
    let unit = local.unit;
    let mut seen = HashSet::<Arc<str>>::new();
    let mut items = Vec::new();
    for function_module in &unit.function_modules {
        let symbol = unit.symbol(function_module.symbol);
        if !seen.insert(Arc::clone(&symbol.name)) {
            continue;
        }
        items.push(CallableCompletionItem {
            name: Arc::clone(&symbol.name),
            declaration: render_function_module_signature(&unit, symbol),
            kind: CallableCompletionKind::FunctionModule,
            insertion: function_module_completion_insertion(symbol.name.as_ref(), function_module),
        });
    }
    items.sort_by(|left, right| left.name.cmp(&right.name));
    items
}

fn call_section_matches_function_parameter(
    section: Option<NamedArgumentSection>,
    parameter: &FunctionModuleParameterData,
) -> bool {
    matches!(
        (section, parameter.section),
        (
            Some(NamedArgumentSection::Exporting),
            FunctionModuleParameterSection::Importing
        ) | (
            Some(NamedArgumentSection::Importing),
            FunctionModuleParameterSection::Exporting
        ) | (
            Some(NamedArgumentSection::Changing),
            FunctionModuleParameterSection::Changing
        ) | (
            Some(NamedArgumentSection::Tables),
            FunctionModuleParameterSection::Tables
        )
    )
}

fn call_section_matches_event_parameter(
    section: Option<NamedArgumentSection>,
    parameter: &ClassMemberParameterData,
) -> bool {
    matches!(
        (section, parameter.section),
        (
            None | Some(NamedArgumentSection::Exporting),
            MethodParameterSection::Exporting
        )
    )
}

fn named_argument_completion_insertion(name: &str) -> CompletionInsertion {
    CompletionInsertion {
        plain_text: format!("{name} = "),
        snippet_text: Some(format!("{name} = ${{1}}")),
    }
}

fn normalized_local_class_template_name(kind: LocalClassTemplateKind, name_hint: &str) -> Arc<str> {
    let trimmed = name_hint.trim();
    match kind {
        LocalClassTemplateKind::Standard => {
            if trimmed.is_empty()
                || trimmed.eq_ignore_ascii_case("lcl")
                || trimmed.eq_ignore_ascii_case("lcl_")
            {
                Arc::from("lcl_demo")
            } else {
                Arc::from(trimmed)
            }
        }
        LocalClassTemplateKind::Test => {
            if trimmed.is_empty()
                || trimmed.eq_ignore_ascii_case("ltcl")
                || trimmed.eq_ignore_ascii_case("ltcl_")
            {
                Arc::from("ltcl_demo")
            } else {
                Arc::from(trimmed)
            }
        }
    }
}

fn local_class_template_completion_insertion(class_name: &str) -> CompletionInsertion {
    CompletionInsertion {
        plain_text: format!(
            "CLASS {class_name} DEFINITION.\n  PUBLIC SECTION.\nENDCLASS.\n\nCLASS {class_name} IMPLEMENTATION.\nENDCLASS."
        ),
        snippet_text: Some(format!(
            "CLASS ${{1:{class_name}}} DEFINITION.\n  PUBLIC SECTION.\n    $0\nENDCLASS.\n\nCLASS ${{1:{class_name}}} IMPLEMENTATION.\nENDCLASS."
        )),
    }
}

fn local_test_class_template_completion_insertion(class_name: &str) -> CompletionInsertion {
    CompletionInsertion {
        plain_text: format!(
            "CLASS {class_name} DEFINITION FOR TESTING \n  DURATION SHORT\n  RISK LEVEL HARMLESS.\n\n  PRIVATE SECTION.\n    METHODS:\n      setup,\n      teardown,\n      test_demo FOR TESTING.\nENDCLASS.\n\nCLASS {class_name} IMPLEMENTATION.\n\n  METHOD setup.\n  ENDMETHOD.\n\n  METHOD teardown.\n  ENDMETHOD.\n\n  METHOD test_demo.\n    cl_abap_unit_assert=>assert_equals(\n      act = abap_true \n      exp = abap_true \n    ).\n  ENDMETHOD.\nENDCLASS."
        ),
        snippet_text: Some(format!(
            "CLASS ${{1:{class_name}}} DEFINITION FOR TESTING \n  DURATION SHORT\n  RISK LEVEL HARMLESS.\n\n  PRIVATE SECTION.\n    METHODS:\n      setup,\n      teardown,\n      ${{2:test_demo}} FOR TESTING.\nENDCLASS.\n\nCLASS ${{1:{class_name}}} IMPLEMENTATION.\n\n  METHOD setup.\n  ENDMETHOD.\n\n  METHOD teardown.\n  ENDMETHOD.\n\n  METHOD ${{2:test_demo}}.\n    cl_abap_unit_assert=>assert_equals(\n      act = ${{3:abap_true}} \n      exp = ${{4:abap_true}} \n    ).\n    $0\n  ENDMETHOD.\nENDCLASS."
        )),
    }
}

fn method_definition_template_completion_insertion() -> CompletionInsertion {
    CompletionInsertion {
        plain_text: "METHODS method_name\n  IMPORTING\n    iv_importing TYPE i\n  EXPORTING\n    ev_exporting TYPE i\n  CHANGING\n    cv_changing TYPE i\n  RECEIVING\n    VALUE(rv_receiving) TYPE i\n  RETURNING\n    VALUE(rv_returning) TYPE i.".to_string(),
        snippet_text: Some(
            "METHODS ${1:method_name}\n  IMPORTING\n    ${2:iv_importing} TYPE ${3:i}\n  EXPORTING\n    ${4:ev_exporting} TYPE ${5:i}\n  CHANGING\n    ${6:cv_changing} TYPE ${7:i}\n  RECEIVING\n    VALUE(${8:rv_receiving}) TYPE ${9:i}\n  RETURNING\n    VALUE(${10:rv_returning}) TYPE ${11:i}.$0".to_string(),
        ),
    }
}

fn types_begin_template_completion_insertion() -> CompletionInsertion {
    CompletionInsertion {
        plain_text: "BEGIN OF type_name,\nEND OF type_name.".to_string(),
        snippet_text: Some("BEGIN OF ${1:type_name},\n  $0\nEND OF ${1:type_name}.".to_string()),
    }
}

fn form_completion_insertion(
    unit: &UnitAnalysis,
    form_name: &str,
    form_routine: &FormRoutineData,
) -> CompletionInsertion {
    fn push_form_section(
        plain_lines: &mut Vec<String>,
        snippet_lines: &mut Vec<String>,
        keyword: &str,
        parameters: &[&FormParameterData],
        unit: &UnitAnalysis,
        tabstop: &mut usize,
    ) {
        if parameters.is_empty() {
            return;
        }
        plain_lines.push(format!("  {keyword}"));
        snippet_lines.push(format!("  {keyword}"));
        for parameter in parameters {
            let name = unit.symbol(parameter.symbol).name.as_ref();
            plain_lines.push(format!("    {name}"));
            snippet_lines.push(format!("    ${{{tabstop}:{name}}}"));
            *tabstop += 1;
        }
    }

    let tables: Vec<_> = form_routine
        .parameters
        .iter()
        .filter(|parameter| parameter.section == FormParameterSection::Tables)
        .collect();
    let using: Vec<_> = form_routine
        .parameters
        .iter()
        .filter(|parameter| parameter.section == FormParameterSection::Using)
        .collect();
    let changing: Vec<_> = form_routine
        .parameters
        .iter()
        .filter(|parameter| parameter.section == FormParameterSection::Changing)
        .collect();

    let mut plain_lines = vec![form_name.to_string()];
    let mut snippet_lines = vec![form_name.to_string()];
    let mut tabstop = 1usize;

    push_form_section(
        &mut plain_lines,
        &mut snippet_lines,
        "TABLES",
        &tables,
        unit,
        &mut tabstop,
    );
    push_form_section(
        &mut plain_lines,
        &mut snippet_lines,
        "USING",
        &using,
        unit,
        &mut tabstop,
    );
    push_form_section(
        &mut plain_lines,
        &mut snippet_lines,
        "CHANGING",
        &changing,
        unit,
        &mut tabstop,
    );

    if let Some(last) = plain_lines.last_mut() {
        last.push('.');
    }
    if let Some(last) = snippet_lines.last_mut() {
        last.push_str(".$0");
    }

    CompletionInsertion {
        plain_text: plain_lines.join("\n"),
        snippet_text: Some(snippet_lines.join("\n")),
    }
}

fn parameter_completion_declared_type(parameter: &ClassMemberParameterData) -> Option<String> {
    parameter
        .type_clause_display
        .as_ref()
        .map(|display| display.trim().to_string())
        .or_else(|| parameter.declared_type.as_ref().map(format_field_type_ref))
}

fn symbol_completion_declared_type(symbol: &SymbolData) -> Option<String> {
    symbol
        .type_clause_display
        .as_ref()
        .map(|display| display.trim().to_string())
        .or_else(|| symbol.declared_type.as_ref().map(format_field_type_ref))
}

fn function_module_parameter_completion_declared_type(
    parameter: &FunctionModuleParameterData,
) -> Option<String> {
    parameter
        .type_clause_display
        .as_ref()
        .map(|display| display.trim().to_string())
        .or_else(|| parameter.declared_type.as_ref().map(format_field_type_ref))
}

fn format_function_module_parameter_completion_declaration(
    parameter: &FunctionModuleParameterData,
) -> String {
    match function_module_parameter_completion_declared_type(parameter) {
        Some(declared_type) => format!("{} {}", parameter.name, declared_type),
        None => parameter.name.to_string(),
    }
}

fn format_parameter_completion_declaration(parameter: &ClassMemberParameterData) -> String {
    match parameter_completion_declared_type(parameter) {
        Some(declared_type) => format!("{} {}", parameter.name, declared_type),
        None => parameter.name.to_string(),
    }
}

fn format_symbol_completion_declaration(symbol: &SymbolData) -> String {
    match symbol_completion_declared_type(symbol) {
        Some(declared_type) => format!("{} {}", symbol.name, declared_type),
        None => symbol.name.to_string(),
    }
}

fn symbol_completion_kind_supported(kind: SymbolKind, namespace: Namespace) -> bool {
    match namespace {
        Namespace::Type => matches!(
            kind,
            SymbolKind::BuiltinType
                | SymbolKind::TypeDef
                | SymbolKind::Class
                | SymbolKind::Interface
        ),
        Namespace::Value => matches!(
            kind,
            SymbolKind::BuiltinConstant
                | SymbolKind::BuiltinVariable
                | SymbolKind::Variable
                | SymbolKind::Constant
                | SymbolKind::EnumMember
                | SymbolKind::FieldSymbol
                | SymbolKind::Parameter
                | SymbolKind::Control
                | SymbolKind::Report
        ),
        Namespace::Routine => false,
    }
}

fn symbol_completion_declared_type_for_snapshot(
    snapshot: &AnalysisSnapshot,
    unit: &UnitAnalysis,
    symbol: &SymbolData,
) -> Option<String> {
    let type_snapshot = (unit.uri == snapshot.uri).then_some(snapshot);
    symbol_type_presentation(type_snapshot, symbol)
        .map(|presentation| presentation.rendered_clause)
        .or_else(|| {
            symbol
                .structure
                .map(|structure_id| format!("TYPE {}", unit.structure(structure_id).name))
        })
}

fn symbol_completion_declaration(
    symbol: &SymbolData,
    declared_type: Option<&str>,
) -> Option<String> {
    let declaration = match symbol.kind {
        SymbolKind::TypeDef
            if symbol.declared_type.is_none()
                && symbol.type_clause_display.is_none()
                && symbol.structure.is_some() =>
        {
            format!("TYPES {}.", symbol.name)
        }
        SymbolKind::TypeDef => match declared_type {
            Some(declared_type) => format!("TYPES {} {}.", symbol.name, declared_type),
            None => format!("TYPES {}.", symbol.name),
        },
        SymbolKind::Variable | SymbolKind::BuiltinVariable => match declared_type {
            Some(declared_type) => format!("DATA {} {}.", symbol.name, declared_type),
            None => symbol.name.to_string(),
        },
        SymbolKind::Constant | SymbolKind::EnumMember | SymbolKind::BuiltinConstant => {
            match declared_type {
                Some(declared_type) => format!("CONSTANTS {} {}.", symbol.name, declared_type),
                None => symbol.name.to_string(),
            }
        }
        SymbolKind::FieldSymbol => match declared_type {
            Some(declared_type) => format!("FIELD-SYMBOLS {} {}.", symbol.name, declared_type),
            None => symbol.name.to_string(),
        },
        SymbolKind::Class => format!("CLASS {}.", symbol.name),
        SymbolKind::Interface => format!("INTERFACE {}.", symbol.name),
        SymbolKind::Parameter => format_symbol_completion_declaration(symbol),
        _ => return None,
    };
    Some(declaration)
}

fn symbol_completion_item(
    snapshot: &AnalysisSnapshot,
    unit: &UnitAnalysis,
    symbol: &SymbolData,
) -> SymbolCompletionItem {
    let declared_type = symbol_completion_declared_type_for_snapshot(snapshot, unit, symbol);
    let declaration = symbol_completion_declaration(symbol, declared_type.as_deref());
    SymbolCompletionItem {
        name: Arc::clone(&symbol.name),
        kind: symbol.kind,
        declared_type,
        declaration,
        insertion: identifier_completion_insertion(symbol.name.as_ref()),
    }
}

fn merge_completion(completion: &mut Option<CompletionInfo>, extra: Option<CompletionInfo>) {
    let Some(extra) = extra else {
        return;
    };
    let Some(existing) = completion.as_mut() else {
        *completion = Some(extra);
        return;
    };
    if existing.replace_range != extra.replace_range
        || existing.in_type_position != extra.in_type_position
    {
        return;
    }
    let mut seen: HashSet<_> = existing
        .items
        .iter()
        .map(|item| completion_item_name(item).to_ascii_lowercase())
        .collect();
    existing.items.extend(
        extra
            .items
            .into_iter()
            .filter(|item| seen.insert(completion_item_name(item).to_ascii_lowercase())),
    );
}

fn completion_item_name(item: &CompletionItem) -> &str {
    match item {
        CompletionItem::Selector(item) => item.name.as_ref(),
        CompletionItem::NamedArgument(item) => item.name.as_ref(),
        CompletionItem::Symbol(item) => item.name.as_ref(),
        CompletionItem::Template(item) => item.name.as_ref(),
        CompletionItem::Callable(item) => item.name.as_ref(),
        CompletionItem::Keyword(item) => item.name.as_ref(),
    }
}

#[derive(Debug, Clone, Copy)]
enum CallableCompletionTarget<'a> {
    Method(&'a ClassMemberData),
    Event(&'a ClassMemberData),
    Function(&'a FunctionModuleData),
}

fn symbol_kind_label(kind: SymbolKind) -> &'static str {
    match kind {
        SymbolKind::BuiltinType => "Built-in type",
        SymbolKind::BuiltinRoutine => "Built-in routine",
        SymbolKind::BuiltinConstant => "Built-in constant",
        SymbolKind::BuiltinVariable => "Built-in variable",
        SymbolKind::Variable => "Variable",
        SymbolKind::Constant => "Constant",
        SymbolKind::EnumMember => "Enum member",
        SymbolKind::TypeDef => "Type definition",
        SymbolKind::FieldSymbol => "Field symbol",
        SymbolKind::Form => "Form",
        SymbolKind::Parameter => "Parameter",
        SymbolKind::Class => "Class",
        SymbolKind::Interface => "Interface",
        SymbolKind::Method => "Method",
        SymbolKind::Field => "Field",
        SymbolKind::Include => "Include program",
        SymbolKind::Event => "Event",
        SymbolKind::Module => "Module",
        SymbolKind::Control => "Control",
        SymbolKind::Report => "Report",
    }
}

fn symbol_value_line(symbol: &SymbolData) -> Option<String> {
    if !matches!(symbol.kind, SymbolKind::Constant | SymbolKind::EnumMember) {
        return None;
    }
    let value = symbol.value_clause_display.as_ref()?;
    Some(format_hover_abap(&format!("VALUE {}", value.trim())))
}

fn format_hover_abap(rendered: &str) -> String {
    format!("```abap\n{rendered}\n```")
}

fn description_for_field_info(
    snapshot: &AnalysisSnapshot,
    field: &StructureFieldInfo,
) -> Option<String> {
    let uri = snapshot
        .project
        .units
        .get(field.decl_unit.as_usize())?
        .uri
        .as_ref();
    let text = snapshot.project_text(uri)?;
    let range = field.decl_range.as_ref()?;
    inline_comment_after_range(text, range)
}

fn description_for_definition_target(
    snapshot: &AnalysisSnapshot,
    target: &DefinitionTarget,
) -> Option<String> {
    let text = snapshot.project_text(target.uri.as_ref())?;
    inline_comment_after_range(text, &target.range)
}

fn inline_comment_after_range(text: &str, range: &Range<usize>) -> Option<String> {
    let line_end = text[range.start..]
        .find('\n')
        .map(|offset| range.start + offset)
        .unwrap_or(text.len());
    let line = text.get(range.end.min(line_end)..line_end)?;
    let comment = line.split_once('"')?.1.trim();
    (!comment.is_empty()).then(|| comment.to_string())
}

fn form_parameter_section_keyword(section: FormParameterSection) -> &'static str {
    match section {
        FormParameterSection::Tables => "TABLES",
        FormParameterSection::Using => "USING",
        FormParameterSection::Changing => "CHANGING",
    }
}

fn render_form_parameter_signature(info: &FormParameterHoverInfo) -> String {
    let rendered_name = match info.passing {
        FormParameterPassingKind::Direct => info.name.to_string(),
        FormParameterPassingKind::Value => format!("VALUE({})", info.name),
        FormParameterPassingKind::Reference => format!("REFERENCE({})", info.name),
    };
    let mut rendered = rendered_name;
    if let Some(type_clause) = info.declared_type.as_ref().map(format_field_type_ref) {
        rendered.push(' ');
        rendered.push_str(&type_clause);
    }
    rendered
}

fn render_form_parameter_hover_signature(info: &FormParameterHoverInfo) -> String {
    format!(
        "{}\n  {}",
        form_parameter_section_keyword(info.section),
        render_form_parameter_signature(info)
    )
}

fn render_form_parameter_signature_data(
    unit: &UnitAnalysis,
    parameter: &FormParameterData,
) -> String {
    let symbol = unit.symbol(parameter.symbol);
    render_form_parameter_signature(&FormParameterHoverInfo {
        form_name: Arc::from(""),
        name: Arc::clone(&symbol.name),
        section: parameter.section,
        passing: parameter.passing,
        declared_type: symbol.declared_type.clone(),
    })
}

const FORM_SIGNATURE_SECTION_KEYWORDS: &[&str] = &["TABLES", "USING", "CHANGING", "RAISING"];
const FUNCTION_SIGNATURE_SECTION_KEYWORDS: &[&str] = &[
    "IMPORTING",
    "EXPORTING",
    "CHANGING",
    "TABLES",
    "RAISING",
    "EXCEPTIONS",
];

fn signature_section_items<'a>(
    signature: &'a str,
    section: &str,
    section_keywords: &[&str],
) -> Vec<&'a str> {
    let tokens: Vec<_> = signature.split_whitespace().collect();
    let Some(start_idx) = tokens
        .iter()
        .position(|token| token.eq_ignore_ascii_case(section))
    else {
        return Vec::new();
    };
    let mut items = Vec::new();
    let mut idx = start_idx + 1;
    while idx < tokens.len() {
        if section_keywords
            .iter()
            .any(|keyword| tokens[idx].eq_ignore_ascii_case(keyword))
        {
            break;
        }
        items.push(tokens[idx]);
        idx += 1;
    }
    items
}

fn append_signature_section_lines(
    lines: &mut Vec<String>,
    signature: &str,
    section: &str,
    section_keywords: &[&str],
) {
    let items = signature_section_items(signature, section, section_keywords);
    if items.is_empty() {
        return;
    }
    lines.push(format!("  {section}"));
    for item in items {
        lines.push(format!("    {item}"));
    }
}

fn render_form_signature(unit: &UnitAnalysis, symbol: &SymbolData) -> Option<String> {
    let routine = unit.semantic().decls().form_routine(symbol.id)?;
    let mut lines = vec![format!("FORM {}", symbol.name)];
    let mut current_section = None;
    for parameter in &routine.parameters {
        if current_section != Some(parameter.section) {
            current_section = Some(parameter.section);
            lines.push(format!(
                "  {}",
                form_parameter_section_keyword(parameter.section)
            ));
        }
        lines.push(format!(
            "    {}",
            render_form_parameter_signature_data(unit, parameter)
        ));
    }
    append_signature_section_lines(
        &mut lines,
        routine.signature.as_ref(),
        "RAISING",
        FORM_SIGNATURE_SECTION_KEYWORDS,
    );
    Some(lines.join("\n"))
}

fn function_module_parameter_section_keyword(
    section: FunctionModuleParameterSection,
) -> &'static str {
    match section {
        FunctionModuleParameterSection::Importing => "IMPORTING",
        FunctionModuleParameterSection::Exporting => "EXPORTING",
        FunctionModuleParameterSection::Changing => "CHANGING",
        FunctionModuleParameterSection::Tables => "TABLES",
    }
}

fn method_parameter_section_keyword(section: MethodParameterSection) -> &'static str {
    match section {
        MethodParameterSection::Importing => "IMPORTING",
        MethodParameterSection::Exporting => "EXPORTING",
        MethodParameterSection::Changing => "CHANGING",
        MethodParameterSection::Receiving => "RECEIVING",
        MethodParameterSection::Returning => "RETURNING",
    }
}

fn render_method_parameter_signature(parameter: &ClassMemberParameterData) -> String {
    let mut rendered = parameter.name.to_string();
    if let Some(type_clause) = parameter
        .declared_type
        .as_ref()
        .map(format_field_type_ref)
        .or_else(|| {
            parameter
                .type_clause_display
                .as_ref()
                .map(|display| display.trim().to_string())
        })
    {
        rendered.push(' ');
        rendered.push_str(&type_clause);
    }
    if parameter.is_optional {
        rendered.push_str(" OPTIONAL");
    }
    rendered
}

fn render_function_module_parameter_signature(parameter: &FunctionModuleParameterData) -> String {
    let mut rendered = parameter.name.to_string();
    if let Some(type_clause) = parameter.declared_type.as_ref().map(format_field_type_ref) {
        rendered.push(' ');
        rendered.push_str(&type_clause);
    }
    if parameter.is_optional {
        rendered.push_str(" OPTIONAL");
    }
    if parameter.has_default_value {
        rendered.push_str(" DEFAULT ...");
    }
    rendered
}

fn render_function_module_signature(unit: &UnitAnalysis, symbol: &SymbolData) -> Option<String> {
    let function_module = unit.function_module(symbol.id)?;
    let mut lines = vec![format!("FUNCTION {}", symbol.name)];
    let mut current_section = None;
    for parameter in &function_module.parameters {
        if current_section != Some(parameter.section) {
            current_section = Some(parameter.section);
            lines.push(format!(
                "  {}",
                function_module_parameter_section_keyword(parameter.section)
            ));
        }
        lines.push(format!(
            "    {}",
            render_function_module_parameter_signature(parameter)
        ));
    }
    append_signature_section_lines(
        &mut lines,
        function_module.signature.as_ref(),
        "RAISING",
        FUNCTION_SIGNATURE_SECTION_KEYWORDS,
    );
    if !function_module.exceptions.is_empty() {
        lines.push("  EXCEPTIONS".to_string());
        for exception in &function_module.exceptions {
            lines.push(format!("    {}", exception.name));
        }
    }
    Some(lines.join("\n"))
}

fn markdown_lines_for_form_parameter(info: &FormParameterHoverInfo) -> Vec<String> {
    vec![
        format!("`{}`", info.name),
        "Parameter".to_string(),
        format_hover_abap(&render_form_parameter_hover_signature(info)),
        format!("parameter of FORM `{}`", info.form_name),
    ]
}

fn markdown_lines_for_form(unit: &UnitAnalysis, symbol: &SymbolData) -> Vec<String> {
    if let Some(signature) = render_form_signature(unit, symbol) {
        return vec![format_hover_abap(&signature)];
    }
    vec![format!("`{}`", symbol.name), "Form".to_string()]
}

fn perform_parameter_inlay_hint_markdown(info: &FormParameterHoverInfo) -> String {
    format!(
        "parameter of FORM `{}`\n\n{}",
        info.form_name,
        format_hover_abap(&render_form_parameter_signature(info))
    )
}

fn function_module_parameter_inlay_hint_markdown(
    function_name: &Arc<str>,
    parameter: &FunctionModuleParameterData,
) -> String {
    format!(
        "parameter of FUNCTION MODULE `{}`\n\n{}",
        function_name,
        format_hover_abap(&format!(
            "{}\n  {}",
            function_module_parameter_section_keyword(parameter.section),
            render_function_module_parameter_signature(parameter)
        ))
    )
}

fn method_parameter_inlay_hint_markdown(
    call_site: &CallSiteData,
    member: &ClassMemberData,
    parameter: &ClassMemberParameterData,
) -> String {
    let owner = match &call_site.target {
        NamedArgumentTarget::Constructor { type_name } => {
            format!("parameter of CONSTRUCTOR `{}`", type_name)
        }
        NamedArgumentTarget::Event { event_name, .. } => {
            format!("parameter of EVENT `{}`", event_name)
        }
        _ => format!("parameter of METHOD `{}`", member.name),
    };
    format!(
        "{owner}\n\n{}",
        format_hover_abap(&format!(
            "{}\n  {}",
            method_parameter_section_keyword(parameter.section),
            render_method_parameter_signature(parameter)
        ))
    )
}

fn method_implementation_parameter_anchor(text: &str, range: &Range<usize>) -> usize {
    let Some(tail) = text.get(range.end..) else {
        return range.end;
    };
    for (offset, ch) in tail.char_indices() {
        if ch == '.' {
            return range.end + offset + ch.len_utf8();
        }
        if ch == '\n' || !ch.is_whitespace() {
            break;
        }
    }
    range.end
}

fn line_start(text: &str, offset: usize) -> usize {
    text.get(..offset)
        .and_then(|prefix| prefix.rfind('\n').map(|idx| idx + 1))
        .unwrap_or(0)
}

fn line_end_including_newline(text: &str, offset: usize) -> usize {
    text.get(offset..)
        .and_then(|tail| tail.find('\n').map(|idx| offset + idx + 1))
        .unwrap_or(text.len())
}

fn line_ending_at(text: &str, offset: usize) -> &'static str {
    if text.get(offset..).is_some_and(|tail| tail.contains("\r\n")) {
        "\r\n"
    } else {
        "\n"
    }
}

fn line_indent(text: &str, offset: usize) -> &str {
    let start = line_start(text, offset);
    let line_prefix = text.get(start..offset).unwrap_or_default();
    let indent_end = line_prefix
        .char_indices()
        .find_map(|(idx, ch)| (!matches!(ch, ' ' | '\t')).then_some(idx))
        .unwrap_or(line_prefix.len());
    &line_prefix[..indent_end]
}

fn method_implementation_parameter_signature(parameter: &ClassMemberParameterData) -> String {
    let rendered_name = if matches!(
        parameter.section,
        MethodParameterSection::Receiving | MethodParameterSection::Returning
    ) {
        format!("VALUE({})", parameter.name)
    } else {
        parameter.name.to_string()
    };
    let mut rendered = rendered_name;
    if let Some(type_clause) = parameter
        .declared_type
        .as_ref()
        .map(format_field_type_ref)
        .or_else(|| {
            parameter
                .type_clause_display
                .as_ref()
                .map(|display| display.trim().to_string())
        })
    {
        rendered.push(' ');
        rendered.push_str(&type_clause);
    }
    if parameter.is_optional {
        rendered.push_str(" OPTIONAL");
    }
    rendered
}

fn method_implementation_parameters_body(parameters: &[ClassMemberParameterData]) -> Vec<String> {
    let mut lines = Vec::new();
    let mut current_section = None;
    for parameter in parameters {
        if current_section != Some(parameter.section) {
            current_section = Some(parameter.section);
            lines.push(format!(
                "  {}",
                method_parameter_section_keyword(parameter.section)
            ));
        }
        lines.push(format!(
            "    {}",
            method_implementation_parameter_signature(parameter)
        ));
    }
    lines
}

fn build_method_parameter_comment_block(
    text: &str,
    implementation_range: &Range<usize>,
    parameters: &[ClassMemberParameterData],
) -> String {
    let newline = line_ending_at(text, implementation_range.end);
    let body_indent = format!("{}  ", line_indent(text, implementation_range.start));
    let lines: Vec<_> = method_implementation_parameters_body(parameters)
        .into_iter()
        .map(|line| {
            let display = line.strip_prefix("  ").unwrap_or(&line);
            format!("{body_indent}\" {display}")
        })
        .collect();
    format!("{}{}", lines.join(newline), newline)
}

fn managed_method_parameter_comment_block_range(
    text: &str,
    insertion: usize,
) -> Option<Range<usize>> {
    let mut cursor = insertion;
    while cursor < text.len() {
        let line_end = line_end_including_newline(text, cursor);
        let line = text.get(cursor..line_end)?;
        if !line.trim().is_empty() {
            break;
        }
        cursor = line_end;
    }
    let start = cursor;
    let first_end = line_end_including_newline(text, cursor);
    let first = text.get(cursor..first_end)?;
    if first
        .trim_start()
        .starts_with("\" abap-lsp: parameters begin")
    {
        cursor = first_end;
        while cursor < text.len() {
            let line_end = line_end_including_newline(text, cursor);
            let line = text.get(cursor..line_end)?;
            cursor = line_end;
            if line.trim_start().starts_with("\" abap-lsp: parameters end") {
                return Some(start..cursor);
            }
        }
        return None;
    }

    let first_content = abap_comment_content(first)?;
    if !first.trim_start().starts_with('"') || !is_method_parameter_section_comment(first_content) {
        return None;
    }
    cursor = first_end;
    let mut saw_parameter = false;
    while cursor < text.len() {
        let line_end = line_end_including_newline(text, cursor);
        let line = text.get(cursor..line_end)?;
        let Some(content) = abap_comment_content(line) else {
            break;
        };
        if is_method_parameter_section_comment(content) {
            cursor = line_end;
            continue;
        }
        if is_method_parameter_detail_comment(content) {
            saw_parameter = true;
            cursor = line_end;
            continue;
        }
        break;
    }
    saw_parameter.then_some(start..cursor)
}

fn abap_comment_content(line: &str) -> Option<&str> {
    let trimmed = line.trim_start();
    let content = trimmed.strip_prefix('"')?;
    Some(content.trim_end_matches(['\r', '\n']))
}

fn is_method_parameter_section_comment(content: &str) -> bool {
    matches!(
        content.trim(),
        "IMPORTING" | "EXPORTING" | "CHANGING" | "RECEIVING" | "RETURNING"
    )
}

fn is_method_parameter_detail_comment(content: &str) -> bool {
    content
        .strip_prefix(' ')
        .and_then(|content| content.strip_prefix(' '))
        .is_some_and(|content| !content.trim().is_empty())
}

fn markdown_lines_for_declared_symbol(
    snapshot: &AnalysisSnapshot,
    unit: &UnitAnalysis,
    symbol: &SymbolData,
) -> Vec<String> {
    if let Some(info) = form_parameter_hover_info(unit, symbol) {
        return markdown_lines_for_form_parameter(&info);
    }
    if symbol.kind == SymbolKind::Form {
        return markdown_lines_for_form(unit, symbol);
    }
    if symbol.kind == SymbolKind::Module
        && let Some(signature) = render_function_module_signature(unit, symbol)
    {
        return vec![format_hover_abap(&signature)];
    }
    let mut lines = vec![
        format!("`{}`", symbol.name),
        symbol_kind_label(symbol.kind).to_string(),
    ];
    if let Some(type_line) = symbol_hover_type_clause(Some(snapshot), unit, symbol) {
        lines.push(type_line);
    }
    if let Some(value_line) = symbol_value_line(symbol) {
        lines.push(value_line);
    }
    lines
}

fn markdown_lines_for_named_argument(
    access: &NamedArgumentAccess,
    parameter: &NamedArgumentParameterInfo,
) -> Vec<String> {
    let mut lines = vec![format!("`{}`", access.name), "Parameter".to_string()];
    if let Some(type_ref) = &parameter.declared_type {
        lines.push(format_hover_type_clause(&format_field_type_ref(type_ref)));
    }
    lines
}

fn markdown_lines_for_class_member(unit: &UnitAnalysis, member: &ClassMemberData) -> Vec<String> {
    let class_name = unit.symbol(member.class_symbol).name.as_ref();
    let visibility = match member.visibility {
        Visibility::Public => "Public",
        Visibility::Protected => "Protected",
        Visibility::Private => "Private",
    };
    let storage = if member.is_static {
        "static"
    } else {
        "instance"
    };
    let kind = match member.kind {
        ClassMemberKind::Attribute => "attribute",
        ClassMemberKind::Method => "method",
        ClassMemberKind::Event => "event",
    };
    vec![
        format!(
            "```abap\n{}\n```",
            format_class_member_signature(unit, member)
        ),
        format!("{visibility} {storage} {kind} of `{class_name}`"),
    ]
}

fn markdown_lines_for_resolution(
    snapshot: &AnalysisSnapshot,
    at_name: &Arc<str>,
    resolution: Resolution,
) -> Vec<String> {
    match resolution {
        Resolution::Symbol(handle) => {
            let unit = &snapshot.project.units[handle.unit.as_usize()];
            let symbol = unit.symbol(handle.symbol);
            if at_name.as_ref() == "super" && symbol.kind == SymbolKind::Class {
                return vec![
                    format!("`{at_name}`"),
                    "Direct superclass reference".to_string(),
                    format!("resolves to class `{}`", symbol.name),
                ];
            }
            if let Some(info) = form_parameter_hover_info(unit, symbol) {
                return markdown_lines_for_form_parameter(&info);
            }
            if symbol.kind == SymbolKind::Form {
                return markdown_lines_for_form(unit, symbol);
            }
            if symbol.kind == SymbolKind::Module
                && let Some(signature) = render_function_module_signature(unit, symbol)
            {
                return vec![format_hover_abap(&signature)];
            }
            let mut lines = vec![
                format!("`{at_name}`"),
                symbol_kind_label(symbol.kind).to_string(),
            ];
            let type_snapshot = (unit.uri == snapshot.uri).then_some(snapshot);
            if let Some(type_line) = symbol_hover_type_clause(type_snapshot, unit, symbol) {
                lines.push(type_line);
            }
            if let Some(value_line) = symbol_value_line(symbol) {
                lines.push(value_line);
            }
            lines
        }
        Resolution::BuiltinType => vec![format!("`{at_name}`"), "Built-in ABAP type".to_string()],
        Resolution::BuiltinRoutine => markdown_lines_for_builtin_routine(at_name),
        Resolution::InternalTableLine => vec![
            format!("`{at_name}`"),
            "ABAP pseudo-component for the current row of an internal table whose line type is scalar-like (elementary, unresolved, or a one-field structure). Typical uses include `LOOP AT ... WHERE`, `READ TABLE ... WITH KEY`, and `SELECT ... FOR ALL ENTRIES IN ...`.".to_string(),
        ],
        Resolution::External => vec![
            format!("`{at_name}`"),
            "External reference (not resolved in this workspace)".to_string(),
        ],
    }
}

fn markdown_lines_for_builtin_routine(name: &Arc<str>) -> Vec<String> {
    let Some(spec) = builtin_routine_spec(name.as_ref()) else {
        return vec![format!("`{name}`"), "Built-in ABAP routine".to_string()];
    };
    let rendered_params = spec.hover_params.to_vec().join(", ");
    vec![
        format!("```abap\n{}( {} )\n```", spec.name, rendered_params),
        "Built-in ABAP routine".to_string(),
        format!("returns `{}`", spec.return_type),
        spec.description.to_string(),
    ]
}

fn definition_target_for_symbol(unit: &UnitAnalysis, symbol: &SymbolData) -> DefinitionTarget {
    DefinitionTarget {
        uri: Arc::clone(&unit.uri),
        range: symbol.decl_range.clone(),
    }
}

fn definition_target_for_class_member(
    unit: &UnitAnalysis,
    member: &ClassMemberData,
) -> DefinitionTarget {
    DefinitionTarget {
        uri: Arc::clone(&unit.uri),
        range: member.decl_range.clone(),
    }
}

fn definition_target_for_class_member_implementation_or_decl(
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    member: &ClassMemberData,
) -> DefinitionTarget {
    match &member.implementation {
        Some(implementation) => definition_target_for_range(
            &project.units[implementation.unit.as_usize()],
            implementation_name_range_for_method(member, implementation.range.clone()),
        ),
        None => definition_target_for_class_member(unit, member),
    }
}

fn implementation_name_range_for_method(
    member: &ClassMemberData,
    range: Range<usize>,
) -> Range<usize> {
    let name = member
        .name
        .rsplit('~')
        .next()
        .unwrap_or(member.name.as_ref());
    if range.end.saturating_sub(range.start) >= name.len() {
        range.end - name.len()..range.end
    } else {
        range
    }
}

fn class_member_name_range_at_offset(
    member: &ClassMemberData,
    offset: usize,
) -> Option<&Range<usize>> {
    if member.decl_range.start <= offset && offset < member.decl_range.end {
        return Some(&member.decl_range);
    }
    member
        .implementation_range
        .as_ref()
        .filter(|range| range.start <= offset && offset < range.end)
}

fn rename_range_for_class_member(
    text: &str,
    member: &ClassMemberData,
    offset: usize,
) -> Option<Range<usize>> {
    if member.decl_range.start <= offset && offset < member.decl_range.end {
        return Some(member.decl_range.clone());
    }
    let range = rename_implementation_range_for_class_member(text, member)?;
    (range.start <= offset && offset < range.end).then_some(range)
}

fn implementation_range_for_unit_text(
    text: &str,
    member: &ClassMemberData,
    unit_id: UnitId,
) -> Option<Range<usize>> {
    let implementation = member.implementation.as_ref()?;
    if implementation.unit != unit_id {
        return None;
    }
    rename_method_name_range(text, implementation.range.clone(), member.name.as_ref())
}

fn rename_implementation_range_for_class_member(
    text: &str,
    member: &ClassMemberData,
) -> Option<Range<usize>> {
    let range = member.implementation_range.clone()?;
    rename_method_name_range(text, range, member.name.as_ref())
}

fn rename_method_symbol_range(text: &str, symbol: &SymbolData) -> Option<Range<usize>> {
    rename_method_name_range(text, symbol.decl_range.clone(), symbol.name.as_ref())
}

fn rename_method_name_range(
    text: &str,
    range: Range<usize>,
    method_name: &str,
) -> Option<Range<usize>> {
    let Some(implementation_text) = text.get(range.clone()) else {
        return Some(range);
    };
    let Some(separator) = implementation_text.rfind('~') else {
        return Some(range);
    };
    let member_start = range.start + separator + '~'.len_utf8();
    let member_range = member_start..range.end;
    text.get(member_range.clone())
        .filter(|slice| {
            slice.eq_ignore_ascii_case(method_name.rsplit('~').next().unwrap_or(method_name))
        })
        .map(|_| member_range)
}

fn definition_target_for_class_member_name_at(
    project: &ProjectAnalysis,
    unit: &UnitAnalysis,
    member: &ClassMemberData,
    offset: usize,
) -> DefinitionTarget {
    let target_range = match class_member_name_range_at_offset(member, offset) {
        Some(range) if *range == member.decl_range => {
            if let Some(implementation) = &member.implementation {
                let implementation_unit = &project.units[implementation.unit.as_usize()];
                return definition_target_for_range(
                    implementation_unit,
                    implementation.range.clone(),
                );
            }
            member.decl_range.clone()
        }
        Some(_) => member.decl_range.clone(),
        None => member.decl_range.clone(),
    };
    definition_target_for_range(unit, target_range)
}

fn definition_target_for_range(unit: &UnitAnalysis, range: Range<usize>) -> DefinitionTarget {
    DefinitionTarget {
        uri: Arc::clone(&unit.uri),
        range,
    }
}

fn qualified_method_symbol_qualifier_range(
    method_symbol: &SymbolData,
    interface_name: &str,
) -> Range<usize> {
    method_symbol.decl_range.start..(method_symbol.decl_range.start + interface_name.len())
}

fn qualified_method_symbol_member_range(
    method_symbol: &SymbolData,
    member_name: &str,
) -> Range<usize> {
    (method_symbol.decl_range.end - member_name.len())..method_symbol.decl_range.end
}

fn synthetic_method_scope_definition_target(
    snapshot: &AnalysisSnapshot,
    unit: &UnitAnalysis,
    symbol: &SymbolData,
) -> Option<DefinitionTarget> {
    if symbol.decl_range.start != symbol.decl_range.end {
        return None;
    }
    let scope = unit.scope(symbol.scope);
    if scope.kind != ScopeKind::Method {
        return None;
    }

    if symbol.kind == SymbolKind::Variable && symbol.name.as_ref() == "me" {
        let class_symbol = enclosing_class_owner(unit, symbol.scope)?;
        return Some(definition_target_for_symbol(
            unit,
            unit.symbol(class_symbol),
        ));
    }

    if symbol.kind == SymbolKind::Parameter {
        let method_symbol = scope.owner?;
        let method_name = unit.symbol(method_symbol).name.as_ref();
        if let Some((interface_name, member_name)) = method_name.split_once('~') {
            let class_symbol = enclosing_class_owner(unit, symbol.scope)?;
            let class_handle = SymbolHandle {
                unit: unit.unit_id,
                symbol: class_symbol,
            };
            let interface_name = Arc::<str>::from(interface_name.to_ascii_lowercase());
            let (interface_unit, interface_symbol) =
                resolve_exposed_interface_handle_with_scope_index(
                    snapshot,
                    snapshot.scope_index(),
                    unit,
                    class_handle.symbol,
                    symbol.scope,
                    &interface_name,
                )?;
            let member = interface_unit
                .semantic()
                .decls()
                .class_member(interface_symbol, member_name)?;
            let parameter = member
                .parameters
                .iter()
                .find(|parameter| parameter.name == symbol.name)?;
            return Some(definition_target_for_range(
                interface_unit,
                parameter.range.clone(),
            ));
        }

        if let Some((definition_unit_id, member)) = snapshot
            .project
            .class_member_definition_for_method_symbol(unit.unit_id, method_symbol)
        {
            let definition_unit = &snapshot.project.units[definition_unit_id.as_usize()];
            if let Some(parameter) = member
                .parameters
                .iter()
                .find(|parameter| parameter.name == symbol.name)
            {
                return Some(definition_target_for_range(
                    definition_unit,
                    parameter.range.clone(),
                ));
            }
        }

        let (super_unit, super_symbol) = resolve_direct_superclass_from_scope_with_scope_index(
            snapshot,
            snapshot.scope_index(),
            symbol.scope,
        )?;
        let (member_unit, member) =
            resolve_class_member_in_hierarchy(snapshot, super_unit, super_symbol, method_name)?;
        let parameter = member
            .parameters
            .iter()
            .find(|parameter| parameter.name == symbol.name)?;
        return Some(definition_target_for_range(
            member_unit,
            parameter.range.clone(),
        ));
    }

    None
}

fn synthetic_loop_where_definition_target(
    snapshot: &AnalysisSnapshot,
    unit: &UnitAnalysis,
    symbol: &SymbolData,
) -> Option<DefinitionTarget> {
    for context in &unit.loop_where_field_contexts {
        if context.scope != symbol.scope {
            continue;
        }
        let source_structure = resolve_loop_where_source_structure_with_scope_index(
            snapshot,
            snapshot.scope_index(),
            context.scope,
            &context.source_access,
        );
        let target_structure = context.target_access.as_ref().and_then(|access| {
            resolve_field_access_structure_with_scope_index(
                snapshot,
                snapshot.scope_index(),
                access,
            )
        });
        for (fields_unit, structure_id) in source_structure.into_iter().chain(target_structure) {
            let lookup_scope = if fields_unit.scopes.get(context.scope.as_usize()).is_some() {
                context.scope
            } else {
                fields_unit.root_scope
            };
            let Some(field) = resolve_structure_field_info_with_scope_index(
                snapshot,
                snapshot.scope_index(),
                fields_unit,
                lookup_scope,
                structure_id,
                symbol.name.as_ref(),
            ) else {
                continue;
            };
            if let Some(range) = field.decl_range {
                return Some(definition_target_for_range(
                    &snapshot.project.units[field.decl_unit.as_usize()],
                    range,
                ));
            }
        }
    }
    None
}

fn synthetic_loop_where_hovered_component_at(
    snapshot: &AnalysisSnapshot,
    offset: usize,
) -> Option<HoveredComponentInfo> {
    let reference = snapshot
        .symbols
        .semantic()
        .refs()
        .reference_at_offset(offset)?;
    let Resolution::Symbol(handle) = reference.resolution? else {
        return None;
    };
    let unit = &snapshot.project.units[handle.unit.as_usize()];
    let symbol = unit.symbol(handle.symbol);

    for context in &unit.loop_where_field_contexts {
        if context.scope != symbol.scope {
            continue;
        }
        let source_structure = resolve_loop_where_source_structure_with_scope_index(
            snapshot,
            snapshot.scope_index(),
            context.scope,
            &context.source_access,
        );
        let target_structure = context.target_access.as_ref().and_then(|access| {
            resolve_field_access_structure_with_scope_index(
                snapshot,
                snapshot.scope_index(),
                access,
            )
        });
        for (fields_unit, structure_id) in source_structure.into_iter().chain(target_structure) {
            let lookup_scope = if fields_unit.scopes.get(context.scope.as_usize()).is_some() {
                context.scope
            } else {
                fields_unit.root_scope
            };
            let Some(field) = resolve_structure_field_info_with_scope_index(
                snapshot,
                snapshot.scope_index(),
                fields_unit,
                lookup_scope,
                structure_id,
                symbol.name.as_ref(),
            ) else {
                continue;
            };
            let kind = match field.shape {
                StructureFieldShape::Scalar => HoveredComponentKind::Scalar,
                StructureFieldShape::Structured { structure } => HoveredComponentKind::Structured {
                    structure_name: Arc::clone(&fields_unit.structure(structure).name),
                },
            };
            return Some(HoveredComponentInfo {
                base_name: Arc::clone(&field.name),
                base_namespace: Namespace::Value,
                component_path: vec![Arc::clone(&field.name)],
                field_name: Arc::clone(&field.name),
                field_owner_structure_name: Some(Arc::clone(
                    &fields_unit.structure(field.owner).name,
                )),
                range: reference.range.clone(),
                declared_type: field.type_ref.as_ref().map(format_field_type_ref),
                description: description_for_field_info(snapshot, &field),
                value_clause_display: field.value_clause_display.clone(),
                declaration: None,
                kind,
                is_static_method: false,
                in_type_position: false,
            });
        }
    }
    None
}

fn namespaced_ddic_prefix(name: &str) -> Option<&str> {
    if !name.starts_with('/') {
        return None;
    }
    let rest = &name[1..];
    let idx = rest.find('/')?;
    Some(&name[..idx + 2])
}

fn inferred_ddic_data_element_target(
    snapshot: &AnalysisSnapshot,
    current_unit: &UnitAnalysis,
    structure_id: StructureId,
    field_name: &str,
) -> Option<InferredDdicFieldTarget> {
    let mut owner_structure_name = current_unit
        .semantic()
        .decls()
        .structure_field_infos(structure_id)
        .into_iter()
        .find(|field| field_looks_like_ddic_proxy_include(current_unit, field))
        .and_then(|field| {
            field
                .type_ref
                .as_ref()
                .map(|type_ref| Arc::<str>::from(type_ref.base_name.as_ref().to_ascii_lowercase()))
        });

    if owner_structure_name.is_none() {
        owner_structure_name = Some(Arc::clone(&current_unit.structure(structure_id).name));
    }
    let owner_name = owner_structure_name.as_ref()?;
    let prefix = namespaced_ddic_prefix(owner_name.as_ref())?;
    let data_element_name = Arc::<str>::from(format!("{prefix}e_{field_name}"));

    let (unit, symbol) = snapshot.project.units.iter().find_map(|unit| {
        unit.symbols
            .iter()
            .find(|symbol| {
                symbol.scope == unit.root_scope
                    && symbol.kind.occupies(Namespace::Type)
                    && symbol.name == data_element_name
            })
            .map(|symbol| (unit, symbol))
    })?;

    Some(InferredDdicFieldTarget {
        field_name: Arc::<str>::from(field_name.to_ascii_lowercase()),
        field_owner_structure_name: owner_structure_name,
        declared_type_name: data_element_name,
        definition: definition_target_for_symbol(unit, symbol),
    })
}

fn definition_target_for_resolution(
    snapshot: &AnalysisSnapshot,
    resolution: Resolution,
) -> Option<DefinitionTarget> {
    match resolution {
        Resolution::Symbol(handle) => {
            let unit = &snapshot.project.units[handle.unit.as_usize()];
            let symbol = unit.symbol(handle.symbol);
            if let Some(target) = synthetic_method_scope_definition_target(snapshot, unit, symbol) {
                return Some(target);
            }
            if let Some(target) = synthetic_loop_where_definition_target(snapshot, unit, symbol) {
                return Some(target);
            }
            Some(definition_target_for_symbol(unit, symbol))
        }
        Resolution::BuiltinType
        | Resolution::BuiltinRoutine
        | Resolution::InternalTableLine
        | Resolution::External => None,
    }
}

fn reference_target_for_search_target(
    project: &ProjectAnalysis,
    target: &ReferenceSearchTarget,
) -> Option<ReferenceTarget> {
    match target {
        ReferenceSearchTarget::Symbol(handle) => {
            let unit = &project.units[handle.unit.as_usize()];
            let symbol = unit.symbol(handle.symbol);
            Some(ReferenceTarget {
                uri: Arc::clone(&unit.uri),
                range: symbol.decl_range.clone(),
            })
        }
        ReferenceSearchTarget::ClassMember {
            unit,
            class_symbol,
            name,
        } => {
            let unit = &project.units[unit.as_usize()];
            let member = unit
                .semantic()
                .decls()
                .class_member(*class_symbol, name.as_ref())?;
            Some(ReferenceTarget {
                uri: Arc::clone(&unit.uri),
                range: member.decl_range.clone(),
            })
        }
        ReferenceSearchTarget::StructField { unit, owner, name } => {
            let unit = &project.units[unit.as_usize()];
            let field = unit
                .semantic()
                .decls()
                .structure_field_info(*owner, name.as_ref())?;
            Some(ReferenceTarget {
                uri: Arc::clone(&project.units[field.decl_unit.as_usize()].uri),
                range: field.decl_range?,
            })
        }
        ReferenceSearchTarget::DdLikeTypeName { name, .. } => {
            let target = definition_target_for_dd_like_type_name(project, name.as_ref())?;
            Some(ReferenceTarget {
                uri: target.uri,
                range: target.range,
            })
        }
    }
}

fn definition_target_for_dd_like_type_name(
    project: &ProjectAnalysis,
    name: &str,
) -> Option<DefinitionTarget> {
    let unit = dd_like_type_definition_unit(project, name)?;
    if let Some(symbol) = unit.symbols.iter().find(|symbol| {
        symbol.scope == unit.root_scope
            && symbol.kind.occupies(Namespace::Type)
            && symbol.name.eq_ignore_ascii_case(name)
    }) {
        return Some(definition_target_for_symbol(unit, symbol));
    }

    Some(definition_target_for_range(unit, 0..0))
}

fn dd_like_type_definition_unit<'a>(
    project: &'a ProjectAnalysis,
    name: &str,
) -> Option<&'a UnitAnalysis> {
    project
        .provided_name_to_unit
        .get(name)
        .and_then(|unit_id| project.units.get(unit_id.as_usize()))
        .or_else(|| {
            project.units.iter().find(|unit| {
                unit.provided_names
                    .iter()
                    .any(|provided| provided.eq_ignore_ascii_case(name))
            })
        })
        .or_else(|| {
            project.units.iter().find(|unit| {
                unit.symbols.iter().any(|symbol| {
                    symbol.scope == unit.root_scope
                        && symbol.kind.occupies(Namespace::Type)
                        && symbol.name.eq_ignore_ascii_case(name)
                })
            })
        })
}

fn symbol_handle_for_decl_range(
    unit: &UnitAnalysis,
    range: &Range<usize>,
    kind: SymbolKind,
) -> Option<abap_symbols::SymbolHandle> {
    unit.semantic()
        .decls()
        .symbol_with_kind_and_decl_range(kind, range)
        .map(|symbol| abap_symbols::SymbolHandle {
            unit: unit.unit_id,
            symbol: symbol.id,
        })
}

fn equivalent_symbol_handles(
    project: &ProjectAnalysis,
    handle: abap_symbols::SymbolHandle,
) -> Vec<abap_symbols::SymbolHandle> {
    let unit = &project.units[handle.unit.as_usize()];
    let symbol = unit.symbol(handle.symbol);
    if symbol.kind != SymbolKind::Parameter {
        return vec![handle];
    }
    let mut out = vec![handle];
    if let Some(owner) = unit.scope(symbol.scope).owner {
        out.extend(
            unit.semantic()
                .decls()
                .routine_parameters(owner)
                .filter(|candidate| candidate.name == symbol.name)
                .map(|candidate| abap_symbols::SymbolHandle {
                    unit: unit.unit_id,
                    symbol: candidate.id,
                }),
        );
    }

    let method_member = if let Some(owner) = unit.scope(symbol.scope).owner {
        let owner_symbol = unit.symbol(owner);
        if owner_symbol.kind == SymbolKind::Method {
            enclosing_class_owner(unit, symbol.scope)
                .map(|class_symbol| (class_symbol, &owner_symbol.name))
        } else {
            None
        }
    } else {
        unit.class_members.iter().find_map(|member| {
            member
                .parameters
                .iter()
                .any(|parameter| {
                    parameter.name == symbol.name && parameter.range == symbol.decl_range
                })
                .then_some((member.class_symbol, &member.name))
        })
    };

    if let Some((class_symbol, method_name)) = method_member {
        if let Some(member) = unit
            .semantic()
            .decls()
            .class_member(class_symbol, method_name.as_ref())
        {
            out.extend(
                member
                    .parameters
                    .iter()
                    .filter(|parameter| parameter.name == symbol.name)
                    .filter_map(|parameter| {
                        symbol_handle_for_decl_range(unit, &parameter.range, SymbolKind::Parameter)
                    }),
            );
        }
        if let Some(method_symbol) = unit.symbols.iter().find(|candidate| {
            candidate.kind == SymbolKind::Method
                && candidate.name == *method_name
                && enclosing_class_owner(unit, candidate.scope) == Some(class_symbol)
        }) {
            out.extend(
                unit.semantic()
                    .decls()
                    .routine_parameters(method_symbol.id)
                    .filter(|candidate| candidate.name == symbol.name)
                    .map(|candidate| abap_symbols::SymbolHandle {
                        unit: unit.unit_id,
                        symbol: candidate.id,
                    }),
            );
        }
    }

    if out.is_empty() {
        out.push(handle);
    }
    out.sort_by_key(|handle| handle.symbol.0);
    out.dedup();
    out
}

fn rename_supported_symbol_kind(kind: SymbolKind) -> bool {
    matches!(
        kind,
        SymbolKind::Variable
            | SymbolKind::Constant
            | SymbolKind::EnumMember
            | SymbolKind::TypeDef
            | SymbolKind::FieldSymbol
            | SymbolKind::Form
            | SymbolKind::Parameter
            | SymbolKind::Class
            | SymbolKind::Interface
            | SymbolKind::Method
            | SymbolKind::Field
            | SymbolKind::Event
            | SymbolKind::Module
            | SymbolKind::Control
    )
}

fn validate_rename_identifier(current: &str, new_name: &str) -> Result<(), String> {
    if new_name.is_empty() {
        return Err("new name must not be empty".to_string());
    }
    if new_name.trim() != new_name {
        return Err("new name must not contain leading or trailing whitespace".to_string());
    }
    let current_is_field_symbol = current.starts_with('<') && current.ends_with('>');
    let new_is_field_symbol = new_name.starts_with('<') && new_name.ends_with('>');
    if current_is_field_symbol && !new_is_field_symbol {
        return Err("field-symbol rename must keep angle brackets".to_string());
    }
    if !current_is_field_symbol && new_is_field_symbol {
        return Err("new name is not a valid ABAP identifier".to_string());
    }

    let tokenized = abap_lexer::tokenize(new_name);
    if !tokenized.errors.is_empty() {
        return Err("new name is not a valid ABAP identifier".to_string());
    }
    let tokens = tokenized.tokens.as_ref();
    if tokens.len() != 2
        || tokens[0].kind != abap_lexer::TokenKind::Ident
        || tokens[0].range != (0..new_name.len())
        || tokens[1].kind != abap_lexer::TokenKind::Eof
    {
        return Err("new name is not a valid ABAP identifier".to_string());
    }
    Ok(())
}

fn resolve_project_class_symbol<'a>(
    snapshot: &'a AnalysisSnapshot,
    preferred_unit: &'a UnitAnalysis,
    name: &Arc<str>,
) -> Option<(&'a UnitAnalysis, SymbolId)> {
    if let Some(handle) = snapshot
        .project
        .visible_type_owner_handle(preferred_unit.unit_id, name)
        && snapshot.project.units[handle.unit.as_usize()]
            .symbol(handle.symbol)
            .kind
            == SymbolKind::Class
    {
        let unit = &snapshot.project.units[handle.unit.as_usize()];
        return Some((unit, handle.symbol));
    }
    preferred_unit
        .symbols
        .iter()
        .find(|symbol| {
            symbol.scope == preferred_unit.root_scope
                && symbol.kind == SymbolKind::Class
                && symbol.name == *name
        })
        .map(|symbol| (preferred_unit, symbol.id))
        .or_else(|| {
            snapshot.project.units.iter().find_map(|candidate_unit| {
                candidate_unit
                    .symbols
                    .iter()
                    .find(|symbol| {
                        symbol.scope == candidate_unit.root_scope
                            && symbol.kind == SymbolKind::Class
                            && symbol.name == *name
                    })
                    .map(|symbol| (candidate_unit, symbol.id))
            })
        })
}

fn resolve_project_interface_symbol<'a>(
    snapshot: &'a AnalysisSnapshot,
    preferred_unit: &'a UnitAnalysis,
    name: &Arc<str>,
) -> Option<(&'a UnitAnalysis, SymbolId)> {
    let handle = snapshot
        .project
        .visible_type_owner_handle(preferred_unit.unit_id, name)?;
    let unit = &snapshot.project.units[handle.unit.as_usize()];
    (unit.symbol(handle.symbol).kind == SymbolKind::Interface).then_some((unit, handle.symbol))
}

fn direct_superclass_from_class<'a>(
    snapshot: &'a AnalysisSnapshot,
    unit: &'a UnitAnalysis,
    class_symbol: SymbolId,
) -> Option<(&'a UnitAnalysis, SymbolId)> {
    let inheritance = unit.semantic().decls().class_superclass(class_symbol)?;
    resolve_project_class_symbol(snapshot, unit, &inheritance.superclass_name)
}

fn class_is_or_inherits_from(
    snapshot: &AnalysisSnapshot,
    descendant: (UnitId, SymbolId),
    ancestor: (UnitId, SymbolId),
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
        let unit = &snapshot.project.units[current.0.as_usize()];
        let Some((next_unit, next_symbol)) =
            direct_superclass_from_class(snapshot, unit, current.1)
        else {
            return false;
        };
        current = (next_unit.unit_id, next_symbol);
    }
}

fn visible_class_handle(
    snapshot: &AnalysisSnapshot,
    unit: &UnitAnalysis,
    class_symbol: SymbolId,
) -> (UnitId, SymbolId) {
    let class_name = &unit.symbol(class_symbol).name;
    snapshot
        .project
        .visible_type_owner_handle(unit.unit_id, class_name)
        .map(|handle| (handle.unit, handle.symbol))
        .unwrap_or((unit.unit_id, class_symbol))
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

fn resolve_field_access_base_symbol<'a>(
    snapshot: &'a AnalysisSnapshot,
    access: &abap_symbols::FieldAccess,
) -> Option<(&'a UnitAnalysis, SymbolId)> {
    resolve_field_access_base_symbol_with_scope_index(snapshot, snapshot.scope_index(), access)
}

fn resolve_field_access_base_symbol_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    access: &abap_symbols::FieldAccess,
) -> Option<(&'a UnitAnalysis, SymbolId)> {
    if access.base_namespace == Namespace::Value && access.base_name.as_ref() == "super" {
        return resolve_direct_superclass_from_scope_with_scope_index(
            snapshot,
            scope_index,
            access.scope,
        );
    }
    resolve_symbol_from_context_with_scope_index(
        snapshot,
        scope_index,
        access.scope,
        access.base_namespace,
        &access.base_name,
        access.in_type_position,
    )
    .or_else(|| {
        resolved_reference_symbol_in_scope(
            snapshot,
            access.scope,
            access.base_namespace,
            &access.base_name,
        )
    })
}

fn resolve_symbol_structure_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    mut unit: &'a UnitAnalysis,
    scope: ScopeId,
    mut symbol_id: SymbolId,
) -> Option<(&'a UnitAnalysis, StructureId)> {
    let mut seen = HashSet::new();
    for _ in 0..8 {
        let symbol = unit.symbol(symbol_id);
        if let Some(structure_id) = symbol.structure {
            return Some((unit, structure_id));
        }

        let declared_type = symbol.declared_type.as_ref()?;
        let (resolved_unit, resolved_symbol_id) = resolve_symbol_from_context_with_scope_index(
            snapshot,
            scope_index,
            scope,
            declared_type.namespace,
            &declared_type.base_name,
            declared_type.namespace == Namespace::Value,
        )?;
        if !seen.insert((resolved_unit.unit_id.0, resolved_symbol_id.0)) {
            return None;
        }

        if declared_type.field_path.is_empty() {
            unit = resolved_unit;
            symbol_id = resolved_symbol_id;
            continue;
        }

        if declared_type.namespace == Namespace::Type
            && matches!(
                resolved_unit.symbol(resolved_symbol_id).kind,
                SymbolKind::Class | SymbolKind::Interface
            )
        {
            let (type_name, rest) = declared_type.field_path.split_first()?;
            let (type_unit, type_symbol) = resolve_class_type_symbol_in_hierarchy(
                snapshot,
                resolved_unit,
                resolved_symbol_id,
                type_name.as_ref(),
            )?;
            let (base_unit, base_structure_id) = resolve_symbol_structure_with_scope_index(
                snapshot,
                scope_index,
                type_unit,
                scope,
                type_symbol.id,
            )?;
            if rest.is_empty() {
                return Some((base_unit, base_structure_id));
            }
            let path: Vec<_> = rest.iter().map(|part| part.as_ref()).collect();
            let field = base_unit
                .semantic()
                .decls()
                .resolve_structure_field_path(base_structure_id, &path)?;
            return match field.shape {
                StructureFieldShape::Structured { structure } => Some((base_unit, structure)),
                StructureFieldShape::Scalar => None,
            };
        }

        let (base_unit, base_structure_id) = resolve_symbol_structure_with_scope_index(
            snapshot,
            scope_index,
            resolved_unit,
            scope,
            resolved_symbol_id,
        )?;
        let path: Vec<_> = declared_type
            .field_path
            .iter()
            .map(|part| part.as_ref())
            .collect();
        let field = base_unit
            .semantic()
            .decls()
            .resolve_structure_field_path(base_structure_id, &path)?;
        return match field.shape {
            StructureFieldShape::Structured { structure } => Some((base_unit, structure)),
            StructureFieldShape::Scalar => None,
        };
    }
    None
}

fn resolve_well_known_external_field_path(
    declared_type: &FieldTypeRefData,
    field_path: &[abap_symbols::FieldAccessSegment],
    segment_index: usize,
) -> Option<(Arc<str>, HoveredComponentKind, FieldTypeRefData)> {
    if declared_type.namespace != Namespace::Type
        || declared_type.is_ref
        || !declared_type.field_path.is_empty()
        || segment_index >= field_path.len()
    {
        return None;
    }

    let mut current_structure_name =
        Arc::<str>::from(declared_type.base_name.as_ref().to_ascii_lowercase());
    for (idx, segment) in field_path.iter().take(segment_index + 1).enumerate() {
        let (type_name, nested_structure_name) =
            abap_symbols::well_known_external_structure_field_type(
                current_structure_name.as_ref(),
                segment.name.as_ref(),
            )?;
        let field_type = FieldTypeRefData {
            namespace: Namespace::Type,
            is_ref: false,
            base_name: Arc::<str>::from(type_name),
            field_path: Vec::new(),
        };
        if idx == segment_index {
            let kind =
                nested_structure_name.map_or(HoveredComponentKind::Scalar, |structure_name| {
                    HoveredComponentKind::Structured {
                        structure_name: Arc::<str>::from(structure_name.to_ascii_lowercase()),
                    }
                });
            return Some((current_structure_name, kind, field_type));
        }
        current_structure_name = Arc::<str>::from(nested_structure_name?.to_ascii_lowercase());
    }
    None
}

fn resolve_well_known_external_field_access_segment(
    unit: &UnitAnalysis,
    access: &abap_symbols::FieldAccess,
    segment_index: usize,
    symbol_id: SymbolId,
) -> Option<(Arc<str>, HoveredComponentKind, FieldTypeRefData)> {
    let symbol = unit.symbol(symbol_id);
    let declared_type = symbol.declared_type.as_ref()?;
    resolve_well_known_external_field_path(declared_type, &access.field_path, segment_index)
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

fn field_looks_like_ddic_proxy_include(unit: &UnitAnalysis, field: &StructureFieldInfo) -> bool {
    let matches_type_ref = field.type_ref.as_ref().is_some_and(|type_ref| {
        type_ref.namespace == Namespace::Type
            && !type_ref.is_ref
            && type_ref.field_path.is_empty()
            && field
                .name
                .as_ref()
                .eq_ignore_ascii_case(&derive_ddic_include_field_name(type_ref.base_name.as_ref()))
    });
    let matches_shape =
        match field.shape {
            StructureFieldShape::Structured { structure } => field
                .name
                .as_ref()
                .eq_ignore_ascii_case(&derive_ddic_include_field_name(
                    unit.structure(structure).name.as_ref(),
                )),
            StructureFieldShape::Scalar => false,
        };
    matches_type_ref || matches_shape
}

fn included_structure_for_proxy_field_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    current_unit: &'a UnitAnalysis,
    scope: ScopeId,
    field: &StructureFieldInfo,
) -> Option<(&'a UnitAnalysis, StructureId)> {
    if let StructureFieldShape::Structured { structure } = field.shape {
        return Some((current_unit, structure));
    }
    let type_ref = field.type_ref.as_ref()?;
    let lookup_scope = if current_unit.scopes.get(scope.as_usize()).is_some() {
        scope
    } else {
        current_unit.root_scope
    };
    let (resolved_unit, resolved_symbol_id) = resolve_symbol_from_context_with_scope_index(
        snapshot,
        scope_index,
        lookup_scope,
        type_ref.namespace,
        &type_ref.base_name,
        type_ref.namespace == Namespace::Value,
    )?;
    resolve_symbol_structure_with_scope_index(
        snapshot,
        scope_index,
        resolved_unit,
        lookup_scope,
        resolved_symbol_id,
    )
}

fn resolve_structure_field_info_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    current_unit: &'a UnitAnalysis,
    scope: ScopeId,
    structure_id: StructureId,
    field_name: &str,
) -> Option<StructureFieldInfo> {
    fn inner<'a>(
        snapshot: &'a AnalysisSnapshot,
        scope_index: &ScopeIndex,
        current_unit: &'a UnitAnalysis,
        scope: ScopeId,
        structure_id: StructureId,
        field_name: &str,
        seen: &mut HashSet<(u32, u32)>,
    ) -> Option<StructureFieldInfo> {
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
            if !field_looks_like_ddic_proxy_include(current_unit, &field) {
                continue;
            }
            let Some((included_unit, included_structure)) =
                included_structure_for_proxy_field_with_scope_index(
                    snapshot,
                    scope_index,
                    current_unit,
                    scope,
                    &field,
                )
            else {
                continue;
            };
            let nested_scope = if included_unit.scopes.get(scope.as_usize()).is_some() {
                scope
            } else {
                included_unit.root_scope
            };
            if let Some(info) = inner(
                snapshot,
                scope_index,
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
        snapshot,
        scope_index,
        current_unit,
        scope,
        structure_id,
        field_name,
        &mut seen,
    )
}

fn resolve_field_access_structure_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    access: &abap_symbols::FieldAccess,
) -> Option<(&'a UnitAnalysis, StructureId)> {
    let (current_unit, base_symbol_id) =
        resolve_field_access_base_symbol_with_scope_index(snapshot, scope_index, access)?;
    let (current_unit, mut current_structure) = resolve_symbol_structure_with_scope_index(
        snapshot,
        scope_index,
        current_unit,
        access.scope,
        base_symbol_id,
    )?;
    if access.field_path.is_empty() {
        return Some((current_unit, current_structure));
    }

    for (idx, segment) in access.field_path.iter().enumerate() {
        if segment.is_deref() {
            return None;
        }
        let field = resolve_structure_field_info_with_scope_index(
            snapshot,
            scope_index,
            current_unit,
            access.scope,
            current_structure,
            segment.name.as_ref(),
        )?;
        if idx + 1 == access.field_path.len() {
            if let Some(type_ref) = field.type_ref.as_ref() {
                let (resolved_unit, resolved_symbol_id) =
                    resolve_symbol_from_context_with_scope_index(
                        snapshot,
                        scope_index,
                        access.scope,
                        type_ref.namespace,
                        &type_ref.base_name,
                        type_ref.namespace == Namespace::Value,
                    )?;
                return resolve_symbol_structure_with_scope_index(
                    snapshot,
                    scope_index,
                    resolved_unit,
                    access.scope,
                    resolved_symbol_id,
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

fn resolve_field_access_container_structure_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    access: &abap_symbols::FieldAccess,
    segment_index: usize,
    unit: &'a UnitAnalysis,
    symbol_id: SymbolId,
) -> Option<(&'a UnitAnalysis, StructureId)> {
    if access.field_path.is_empty() || segment_index >= access.field_path.len() {
        return None;
    }

    if segment_index > 0
        && access.in_type_position
        && let Some((type_unit, type_symbol)) = resolve_class_selector_type_symbol_with_scope_index(
            snapshot,
            scope_index,
            access,
            0,
            unit,
            symbol_id,
        )
    {
        let mut structure_id = type_symbol.structure?;
        let lookup_scope = if type_unit.scopes.get(access.scope.as_usize()).is_some() {
            access.scope
        } else {
            type_unit.root_scope
        };
        for segment in &access.field_path[1..segment_index] {
            let field = resolve_structure_field_info_with_scope_index(
                snapshot,
                scope_index,
                type_unit,
                lookup_scope,
                structure_id,
                segment.name.as_ref(),
            )?;
            structure_id = match field.shape {
                StructureFieldShape::Structured { structure } => structure,
                StructureFieldShape::Scalar => return None,
            };
        }
        return Some((type_unit, structure_id));
    }

    if segment_index > 0
        && let Some((member_unit, member)) = resolve_class_selector_member_with_scope_index(
            snapshot,
            scope_index,
            access,
            0,
            unit,
            symbol_id,
        )
    {
        let mut structure_id = member.structure?;
        let lookup_scope = if member_unit.scopes.get(access.scope.as_usize()).is_some() {
            access.scope
        } else {
            member_unit.root_scope
        };
        for segment in &access.field_path[1..segment_index] {
            let field = resolve_structure_field_info_with_scope_index(
                snapshot,
                scope_index,
                member_unit,
                lookup_scope,
                structure_id,
                segment.name.as_ref(),
            )?;
            structure_id = match field.shape {
                StructureFieldShape::Structured { structure } => structure,
                StructureFieldShape::Scalar => return None,
            };
        }
        return Some((member_unit, structure_id));
    }

    let (structure_unit, mut structure_id) = resolve_symbol_structure_with_scope_index(
        snapshot,
        scope_index,
        unit,
        access.scope,
        symbol_id,
    )?;
    for segment in &access.field_path[..segment_index] {
        let field = resolve_structure_field_info_with_scope_index(
            snapshot,
            scope_index,
            structure_unit,
            access.scope,
            structure_id,
            segment.name.as_ref(),
        )?;
        structure_id = match field.shape {
            StructureFieldShape::Structured { structure } => structure,
            StructureFieldShape::Scalar => return None,
        };
    }
    Some((structure_unit, structure_id))
}

fn resolve_field_access_component_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    access: &abap_symbols::FieldAccess,
    segment_index: usize,
    unit: &'a UnitAnalysis,
    symbol_id: SymbolId,
) -> Option<(&'a UnitAnalysis, StructureFieldInfo)> {
    let (structure_unit, structure_id) = resolve_field_access_container_structure_with_scope_index(
        snapshot,
        scope_index,
        access,
        segment_index,
        unit,
        symbol_id,
    )?;
    let field = resolve_structure_field_info_with_scope_index(
        snapshot,
        scope_index,
        structure_unit,
        access.scope,
        structure_id,
        access.field_path[segment_index].name.as_ref(),
    )?;
    Some((structure_unit, field))
}

fn resolve_selector_component_path_structure_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    scope: ScopeId,
    base_namespace: Namespace,
    base_name: &Arc<str>,
    in_type_position: bool,
    unit: &'a UnitAnalysis,
    symbol_id: SymbolId,
    component_path: &[Arc<str>],
) -> Option<(&'a UnitAnalysis, StructureId)> {
    let mut current_unit = unit;
    let mut current_structure = None;
    let mut start_idx = 0usize;

    if !component_path.is_empty() {
        let synthetic_access = abap_symbols::FieldAccess {
            scope,
            base_namespace,
            base_name: Arc::clone(base_name),
            base_range: 0..0,
            field_path: component_path
                .iter()
                .map(|name| abap_symbols::FieldAccessSegment {
                    name: Arc::clone(name),
                    range: 0..0,
                })
                .collect(),
            in_type_position,
        };
        if in_type_position
            && let Some((type_unit, type_symbol)) =
                resolve_class_selector_type_symbol_with_scope_index(
                    snapshot,
                    scope_index,
                    &synthetic_access,
                    0,
                    unit,
                    symbol_id,
                )
        {
            current_unit = type_unit;
            current_structure = Some(type_symbol.structure?);
            start_idx = 1;
        } else if let Some((member_unit, member)) = resolve_class_selector_member_with_scope_index(
            snapshot,
            scope_index,
            &synthetic_access,
            0,
            unit,
            symbol_id,
        ) {
            current_unit = member_unit;
            current_structure = Some(member.structure?);
            start_idx = 1;
        }
    }

    let mut structure_id = match current_structure {
        Some(structure_id) => structure_id,
        None => {
            let (resolved_unit, resolved_structure_id) = resolve_symbol_structure_with_scope_index(
                snapshot,
                scope_index,
                current_unit,
                scope,
                symbol_id,
            )?;
            current_unit = resolved_unit;
            resolved_structure_id
        }
    };

    for segment in &component_path[start_idx..] {
        let field = resolve_structure_field_info_with_scope_index(
            snapshot,
            scope_index,
            current_unit,
            scope,
            structure_id,
            segment.as_ref(),
        )?;
        structure_id = match field.shape {
            StructureFieldShape::Structured { structure } => structure,
            StructureFieldShape::Scalar => return None,
        };
    }

    Some((current_unit, structure_id))
}

fn resolve_loop_where_source_structure_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    loop_scope: ScopeId,
    source_access: &abap_symbols::FieldAccess,
) -> Option<(&'a UnitAnalysis, StructureId)> {
    if source_access.base_namespace != Namespace::Value {
        return None;
    }
    let (current_unit, base_symbol_id) = resolve_symbol_from_context_with_scope_index(
        snapshot,
        scope_index,
        source_access.scope,
        Namespace::Value,
        &source_access.base_name,
        false,
    )?;
    let (current_unit, mut current_structure) = resolve_symbol_structure_with_scope_index(
        snapshot,
        scope_index,
        current_unit,
        loop_scope,
        base_symbol_id,
    )?;
    if source_access.field_path.is_empty() {
        return Some((current_unit, current_structure));
    }

    for (idx, segment) in source_access.field_path.iter().enumerate() {
        if segment.is_deref() {
            return None;
        }
        let field = resolve_structure_field_info_with_scope_index(
            snapshot,
            scope_index,
            current_unit,
            loop_scope,
            current_structure,
            segment.name.as_ref(),
        )?;
        if idx + 1 == source_access.field_path.len() {
            if let Some(type_ref) = field.type_ref.as_ref() {
                let (resolved_unit, resolved_symbol_id) =
                    resolve_symbol_from_context_with_scope_index(
                        snapshot,
                        scope_index,
                        loop_scope,
                        type_ref.namespace,
                        &type_ref.base_name,
                        type_ref.namespace == Namespace::Value,
                    )?;
                return resolve_symbol_structure_with_scope_index(
                    snapshot,
                    scope_index,
                    resolved_unit,
                    loop_scope,
                    resolved_symbol_id,
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

fn perform_section_to_form_section(section: PerformParameterSection) -> FormParameterSection {
    match section {
        PerformParameterSection::Tables => FormParameterSection::Tables,
        PerformParameterSection::Using => FormParameterSection::Using,
        PerformParameterSection::Changing => FormParameterSection::Changing,
    }
}

fn form_parameter_hover_info(
    unit: &UnitAnalysis,
    symbol: &SymbolData,
) -> Option<FormParameterHoverInfo> {
    if symbol.kind != SymbolKind::Parameter {
        return None;
    }
    let form_symbol = unit.scope(symbol.scope).owner?;
    let form_routine = unit.semantic().decls().form_routine(form_symbol)?;
    let parameter = form_routine
        .parameters
        .iter()
        .find(|parameter| parameter.symbol == symbol.id)?;
    Some(FormParameterHoverInfo {
        form_name: Arc::clone(&unit.symbol(form_symbol).name),
        name: Arc::clone(&symbol.name),
        section: parameter.section,
        passing: parameter.passing,
        declared_type: symbol.declared_type.clone(),
    })
}

fn form_parameter_hover_info_from_metadata(
    unit: &UnitAnalysis,
    form_symbol: SymbolId,
    parameter: &FormParameterData,
) -> Option<FormParameterHoverInfo> {
    let symbol = unit.symbol(parameter.symbol);
    Some(FormParameterHoverInfo {
        form_name: Arc::clone(&unit.symbol(form_symbol).name),
        name: Arc::clone(&symbol.name),
        section: parameter.section,
        passing: parameter.passing,
        declared_type: symbol.declared_type.clone(),
    })
}

fn resolve_perform_argument_parameter(
    snapshot: &AnalysisSnapshot,
    perform_call: &PerformCallData,
    argument: &PerformArgumentData,
) -> Option<FormParameterHoverInfo> {
    let handle = snapshot
        .project
        .resolve_perform_call_target(snapshot.symbols.as_ref(), perform_call)?;
    let unit = &snapshot.project.units[handle.unit.as_usize()];
    let routine_symbol_id = handle.symbol;
    if unit.symbol(routine_symbol_id).kind != SymbolKind::Form {
        return None;
    }
    let parameter = unit
        .semantic()
        .decls()
        .form_routine(routine_symbol_id)?
        .parameters
        .iter()
        .filter(|parameter| parameter.section == perform_section_to_form_section(argument.section))
        .nth(argument.ordinal_in_section)?;
    form_parameter_hover_info_from_metadata(unit, routine_symbol_id, parameter)
}

fn resolve_perform_argument_symbol(
    snapshot: &AnalysisSnapshot,
    perform_call: &PerformCallData,
    argument: &PerformArgumentData,
) -> Option<abap_symbols::SymbolHandle> {
    let handle = snapshot
        .project
        .resolve_perform_call_target(snapshot.symbols.as_ref(), perform_call)?;
    let unit = &snapshot.project.units[handle.unit.as_usize()];
    let routine_symbol_id = handle.symbol;
    if unit.symbol(routine_symbol_id).kind != SymbolKind::Form {
        return None;
    }
    let parameter = unit
        .semantic()
        .decls()
        .form_routine(routine_symbol_id)?
        .parameters
        .iter()
        .filter(|parameter| parameter.section == perform_section_to_form_section(argument.section))
        .nth(argument.ordinal_in_section)?;
    Some(abap_symbols::SymbolHandle {
        unit: handle.unit,
        symbol: parameter.symbol,
    })
}

fn named_argument_section_keyword(text: &str) -> Option<NamedArgumentSection> {
    match text {
        "exporting" => Some(NamedArgumentSection::Exporting),
        "importing" => Some(NamedArgumentSection::Importing),
        "changing" => Some(NamedArgumentSection::Changing),
        "tables" => Some(NamedArgumentSection::Tables),
        "receiving" => Some(NamedArgumentSection::Receiving),
        "exceptions" => Some(NamedArgumentSection::Exceptions),
        _ => None,
    }
}

fn call_site_target_name_range(
    text: &str,
    call_site: &abap_symbols::CallSiteData,
    method_name: &Arc<str>,
) -> Option<Range<usize>> {
    let call_text = text.get(call_site.range.clone())?;
    let args_start = call_text.find('(')?;
    let target_text = &call_text[..args_start];
    let method_name = method_name.as_ref().to_ascii_lowercase();
    let target_text_lower = target_text.to_ascii_lowercase();
    let rel_start = target_text_lower.rfind(&method_name)?;
    Some(call_site.range.start + rel_start..call_site.range.start + rel_start + method_name.len())
}

fn call_site_event_name_range(
    text: &str,
    call_site: &abap_symbols::CallSiteData,
    qualifier: Option<&Arc<str>>,
    event_name: &Arc<str>,
) -> Option<Range<usize>> {
    let call_text = text.get(call_site.range.clone())?;
    let lowered = call_text.to_ascii_lowercase();
    let event_name = event_name.as_ref().to_ascii_lowercase();
    if let Some(qualifier) = qualifier {
        let qualifier_lower = qualifier.as_ref().to_ascii_lowercase();
        let pattern = format!("{qualifier_lower}~{event_name}");
        let rel_start = lowered.find(&pattern)? + qualifier_lower.len() + 1;
        return Some(
            call_site.range.start + rel_start..call_site.range.start + rel_start + event_name.len(),
        );
    }
    let rel_start = lowered.find(&event_name)?;
    Some(call_site.range.start + rel_start..call_site.range.start + rel_start + event_name.len())
}

fn call_site_interface_qualifier<'a>(
    unit: &'a UnitAnalysis,
    call_site: &CallSiteData,
    method_name: &str,
) -> Option<&'a Arc<str>> {
    let NamedArgumentTarget::Method {
        base_namespace,
        base_name,
        ..
    } = &call_site.target
    else {
        return None;
    };
    unit.field_accesses.iter().find_map(|access| {
        if access.scope != call_site.scope
            || access.base_namespace != *base_namespace
            || access.base_name.as_ref() != base_name.as_ref()
            || access.base_range.start < call_site.range.start
        {
            return None;
        }
        let last = access.field_path.last()?;
        if last.name.as_ref() != method_name || last.range.end > call_site.range.end {
            return None;
        }
        access
            .field_path
            .get(access.field_path.len().checked_sub(2)?)
            .map(|segment| &segment.name)
    })
}

fn resolve_interface_qualified_call_member<'a>(
    snapshot: &'a AnalysisSnapshot,
    owner_unit: &'a UnitAnalysis,
    owner_symbol: SymbolId,
    call_site: &CallSiteData,
    method_name: &str,
) -> Option<(&'a UnitAnalysis, &'a ClassMemberData)> {
    let interface_name =
        call_site_interface_qualifier(snapshot.symbols.as_ref(), call_site, method_name)?;
    let (interface_unit, interface_symbol) =
        resolve_exposed_interface_handle(snapshot, owner_unit, owner_symbol, interface_name)?;
    interface_unit
        .semantic()
        .decls()
        .class_member(interface_symbol, method_name)
        .map(|member| (interface_unit, member))
}

fn resolve_bare_interface_qualified_call_member<'a>(
    snapshot: &'a AnalysisSnapshot,
    call_site: &CallSiteData,
    interface_name: &Arc<str>,
    method_name: &Arc<str>,
) -> Option<(&'a UnitAnalysis, &'a ClassMemberData)> {
    let unit = snapshot.symbols.as_ref();
    let class_symbol = enclosing_class_owner(unit, call_site.scope)?;
    let (interface_unit, interface_symbol) =
        resolve_exposed_interface_handle(snapshot, unit, class_symbol, interface_name)?;
    interface_unit
        .semantic()
        .decls()
        .class_member(interface_symbol, method_name.as_ref())
        .filter(|member| member.kind == ClassMemberKind::Method)
        .map(|member| (interface_unit, member))
}

fn resolve_call_target_member<'a>(
    snapshot: &'a AnalysisSnapshot,
    call_site: &abap_symbols::CallSiteData,
) -> Option<(&'a UnitAnalysis, &'a ClassMemberData)> {
    match &call_site.target {
        NamedArgumentTarget::Event {
            qualifier,
            event_name,
        } => {
            let (member_unit, member) = resolve_event_target_member_from_context(
                snapshot,
                snapshot.scope_index(),
                call_site.scope,
                qualifier.as_ref(),
                event_name,
            )?;
            class_member_visible_to(
                snapshot,
                snapshot.symbols.as_ref(),
                call_site.scope,
                member_unit,
                member,
            )
            .then_some((member_unit, member))
        }
        NamedArgumentTarget::ImplicitMethod { method_name } => {
            let unit = snapshot.symbols.as_ref();
            let class_symbol_id = enclosing_class_owner(unit, call_site.scope)?;
            let (member_unit, member) =
                resolve_class_member_in_hierarchy(snapshot, unit, class_symbol_id, method_name)?;
            if member.kind != ClassMemberKind::Method {
                return None;
            }
            class_member_visible_to(
                snapshot,
                snapshot.symbols.as_ref(),
                call_site.scope,
                member_unit,
                member,
            )
            .then_some((member_unit, member))
        }
        NamedArgumentTarget::Method {
            base_namespace,
            base_name,
            method_name,
            interface_qualified,
        } => {
            if *interface_qualified {
                let (member_unit, member) = resolve_bare_interface_qualified_call_member(
                    snapshot,
                    call_site,
                    base_name,
                    method_name,
                )?;
                return class_member_visible_to(
                    snapshot,
                    snapshot.symbols.as_ref(),
                    call_site.scope,
                    member_unit,
                    member,
                )
                .then_some((member_unit, member));
            }
            let (unit, class_symbol_id, requires_static) = resolve_method_target_from_context(
                snapshot,
                call_site.scope,
                *base_namespace,
                base_name,
            )
            .or_else(|| {
                let (base_unit, base_symbol_id) = resolved_call_site_base_symbol(
                    snapshot,
                    call_site,
                    *base_namespace,
                    base_name,
                )?;
                resolve_method_target_from_base_symbol_with_scope_index(
                    snapshot,
                    snapshot.scope_index(),
                    call_site.scope,
                    *base_namespace,
                    base_unit,
                    base_symbol_id,
                )
            })?;
            if let Some((member_unit, member)) = resolve_interface_qualified_call_member(
                snapshot,
                unit,
                class_symbol_id,
                call_site,
                method_name,
            ) {
                if member.kind != ClassMemberKind::Method || (requires_static && !member.is_static)
                {
                    return None;
                }
                return class_member_visible_to(
                    snapshot,
                    snapshot.symbols.as_ref(),
                    call_site.scope,
                    member_unit,
                    member,
                )
                .then_some((member_unit, member));
            }
            let (member_unit, member) =
                resolve_class_member_in_hierarchy(snapshot, unit, class_symbol_id, method_name)?;
            if member.kind != ClassMemberKind::Method || (requires_static && !member.is_static) {
                return None;
            }
            class_member_visible_to(
                snapshot,
                snapshot.symbols.as_ref(),
                call_site.scope,
                member_unit,
                member,
            )
            .then_some((member_unit, member))
        }
        _ => None,
    }
}

fn resolved_call_site_base_symbol<'a>(
    snapshot: &'a AnalysisSnapshot,
    call_site: &abap_symbols::CallSiteData,
    namespace: Namespace,
    base_name: &Arc<str>,
) -> Option<(&'a UnitAnalysis, SymbolId)> {
    snapshot
        .symbols
        .references
        .iter()
        .filter(|reference| {
            reference.scope == call_site.scope
                && reference.namespace == namespace
                && reference.name == *base_name
                && call_site.range.start <= reference.range.start
                && reference.range.end <= call_site.range.end
        })
        .filter_map(|reference| match reference.resolution {
            Some(Resolution::Symbol(handle)) => Some((
                &snapshot.project.units[handle.unit.as_usize()],
                handle.symbol,
                reference.range.end.saturating_sub(reference.range.start),
            )),
            _ => None,
        })
        .min_by_key(|(_, _, width)| *width)
        .map(|(unit, symbol_id, _)| (unit, symbol_id))
}

fn named_argument_section_before_offset(
    parse: &ParseResult,
    text: &str,
    start: usize,
    end: usize,
    offset: usize,
) -> Option<NamedArgumentSection> {
    let mut section = None;
    for idx in start..end {
        let token = &parse.tokens[idx];
        if token.range.start >= offset {
            break;
        }
        if token.kind != TokenKind::Ident {
            continue;
        }
        let lexeme = token.lexeme(text).to_ascii_lowercase();
        if let Some(next) = named_argument_section_keyword(lexeme.as_ref()) {
            section = Some(next);
        }
    }
    section
}

fn resolve_callable_completion_target<'a>(
    snapshot: &'a AnalysisSnapshot,
    call_site: &abap_symbols::CallSiteData,
) -> Option<CallableCompletionTarget<'a>> {
    match &call_site.target {
        NamedArgumentTarget::Constructor { type_name } => {
            let (unit, class_symbol_id) = resolve_symbol_from_context(
                snapshot,
                call_site.scope,
                Namespace::Type,
                type_name,
                false,
            )?;
            if unit.symbol(class_symbol_id).kind != SymbolKind::Class {
                return None;
            }
            Some(CallableCompletionTarget::Method(
                unit.semantic()
                    .decls()
                    .class_member(class_symbol_id, "constructor")?,
            ))
        }
        NamedArgumentTarget::ImplicitMethod { method_name } => {
            let unit = snapshot.symbols.as_ref();
            let class_symbol_id = enclosing_class_owner(unit, call_site.scope)?;
            let (member_unit, member) =
                resolve_class_member_in_hierarchy(snapshot, unit, class_symbol_id, method_name)?;
            if member.kind != ClassMemberKind::Method
                || !class_member_visible_to(
                    snapshot,
                    snapshot.symbols.as_ref(),
                    call_site.scope,
                    member_unit,
                    member,
                )
            {
                return None;
            }
            Some(CallableCompletionTarget::Method(member))
        }
        NamedArgumentTarget::Method {
            base_namespace,
            base_name,
            method_name,
            interface_qualified,
        } => {
            if *interface_qualified {
                let (member_unit, member) = resolve_bare_interface_qualified_call_member(
                    snapshot,
                    call_site,
                    base_name,
                    method_name,
                )?;
                if class_member_visible_to(
                    snapshot,
                    snapshot.symbols.as_ref(),
                    call_site.scope,
                    member_unit,
                    member,
                ) {
                    return Some(CallableCompletionTarget::Method(member));
                }
                return None;
            }
            let (unit, class_symbol_id, requires_static) = resolve_method_target_from_context(
                snapshot,
                call_site.scope,
                *base_namespace,
                base_name,
            )?;
            if let Some((member_unit, member)) = resolve_interface_qualified_call_member(
                snapshot,
                unit,
                class_symbol_id,
                call_site,
                method_name,
            ) {
                if member.kind == ClassMemberKind::Method
                    && !(requires_static && !member.is_static)
                    && class_member_visible_to(
                        snapshot,
                        snapshot.symbols.as_ref(),
                        call_site.scope,
                        member_unit,
                        member,
                    )
                {
                    return Some(CallableCompletionTarget::Method(member));
                }
                return None;
            }
            let (member_unit, member) =
                resolve_class_member_in_hierarchy(snapshot, unit, class_symbol_id, method_name)?;
            if member.kind != ClassMemberKind::Method
                || (requires_static && !member.is_static)
                || !class_member_visible_to(
                    snapshot,
                    snapshot.symbols.as_ref(),
                    call_site.scope,
                    member_unit,
                    member,
                )
            {
                return None;
            }
            Some(CallableCompletionTarget::Method(member))
        }
        NamedArgumentTarget::Function { function_name } => {
            let (unit, function_symbol_id) = resolve_symbol_from_context(
                snapshot,
                call_site.scope,
                Namespace::Routine,
                function_name,
                false,
            )?;
            Some(CallableCompletionTarget::Function(
                unit.function_module(function_symbol_id)?,
            ))
        }
        NamedArgumentTarget::Report { .. } => None,
        NamedArgumentTarget::Routine { .. } => None,
        NamedArgumentTarget::Event {
            qualifier,
            event_name,
        } => {
            let (member_unit, member) = resolve_event_target_member_from_context(
                snapshot,
                snapshot.scope_index(),
                call_site.scope,
                qualifier.as_ref(),
                event_name,
            )?;
            if member.kind != ClassMemberKind::Event
                || !class_member_visible_to(
                    snapshot,
                    snapshot.symbols.as_ref(),
                    call_site.scope,
                    member_unit,
                    member,
                )
            {
                return None;
            }
            Some(CallableCompletionTarget::Event(member))
        }
    }
}

fn named_argument_completion_context(
    snapshot: &AnalysisSnapshot,
    call_site: &abap_symbols::CallSiteData,
    offset: usize,
) -> Option<(Range<usize>, Arc<str>, Option<NamedArgumentSection>)> {
    let (token_start, token_end) = token_window_for_range(&snapshot.parse, &call_site.range)?;
    let section = named_argument_section_before_offset(
        &snapshot.parse,
        snapshot.text.as_ref(),
        token_start,
        token_end,
        offset,
    );
    if let Some(prefix_idx) =
        prefix_token_at_offset(&snapshot.parse, token_start, token_end, offset)
    {
        let token = &snapshot.parse.tokens[prefix_idx];
        let prefix = token.lexeme(snapshot.text.as_ref()).to_ascii_lowercase();
        if named_argument_section_keyword(prefix.as_ref()).is_some() {
            return None;
        }
        if previous_significant_token(&snapshot.parse, token_start, prefix_idx)
            .is_some_and(|prev_idx| snapshot.parse.tokens[prev_idx].kind == TokenKind::Eq)
        {
            return None;
        }
        let prefix_end = offset.min(token.range.end);
        return Some((
            token.range.start..prefix_end,
            Arc::from(snapshot.text[token.range.start..prefix_end].to_ascii_lowercase()),
            section,
        ));
    }

    let next_idx =
        first_token_starting_at_or_after(&snapshot.parse, token_start, token_end, offset);
    let prev_idx = previous_significant_token(&snapshot.parse, token_start, next_idx)?;
    let prev = &snapshot.parse.tokens[prev_idx];
    if prev.kind == TokenKind::Eq {
        return None;
    }
    if prev.kind != TokenKind::LParen
        && named_argument_section_keyword(
            prev.lexeme(snapshot.text.as_ref())
                .to_ascii_lowercase()
                .as_ref(),
        )
        .is_none()
    {
        return None;
    }
    Some((offset..offset, Arc::from(""), section))
}

fn resolve_named_argument_parameter(
    snapshot: &AnalysisSnapshot,
    access: &NamedArgumentAccess,
) -> Option<NamedArgumentParameterInfo> {
    resolve_named_argument_parameter_with_scope_index(snapshot, snapshot.scope_index(), access)
}

fn call_site_for_named_argument<'a>(
    snapshot: &'a AnalysisSnapshot,
    access: &NamedArgumentAccess,
) -> Option<&'a CallSiteData> {
    snapshot.symbols.call_sites.iter().find(|call_site| {
        call_site.scope == access.scope
            && call_site.target == access.target
            && call_site.range.start <= access.range.start
            && access.range.end <= call_site.range.end
    })
}

fn resolve_named_argument_parameter_with_scope_index(
    snapshot: &AnalysisSnapshot,
    scope_index: &ScopeIndex,
    access: &NamedArgumentAccess,
) -> Option<NamedArgumentParameterInfo> {
    match &access.target {
        NamedArgumentTarget::Constructor { type_name } => {
            let (unit, class_symbol_id) = resolve_symbol_from_context_with_scope_index(
                snapshot,
                scope_index,
                access.scope,
                Namespace::Type,
                type_name,
                false,
            )?;
            if unit.symbol(class_symbol_id).kind != SymbolKind::Class {
                return None;
            }
            let parameter = unit
                .semantic()
                .decls()
                .class_member(class_symbol_id, "constructor")?
                .parameters
                .iter()
                .find(|parameter| parameter.name == access.name)?;
            Some(NamedArgumentParameterInfo {
                name: Arc::clone(&parameter.name),
                declared_type: parameter.declared_type.clone(),
            })
        }
        NamedArgumentTarget::Function { function_name } => {
            let (unit, function_symbol_id) = resolve_symbol_from_context_with_scope_index(
                snapshot,
                scope_index,
                access.scope,
                Namespace::Routine,
                function_name,
                false,
            )?;
            let function_module = unit.function_module(function_symbol_id)?;
            if let Some(parameter) = function_module
                .parameters
                .iter()
                .find(|parameter| parameter.name == access.name)
            {
                return Some(NamedArgumentParameterInfo {
                    name: Arc::clone(&parameter.name),
                    declared_type: parameter.declared_type.clone(),
                });
            }
            let exception = function_module
                .exceptions
                .iter()
                .find(|exception| exception.name == access.name)?;
            Some(NamedArgumentParameterInfo {
                name: Arc::clone(&exception.name),
                declared_type: None,
            })
        }
        NamedArgumentTarget::Report { .. } => None,
        NamedArgumentTarget::Event {
            qualifier,
            event_name,
        } => {
            let (member_unit, member) = resolve_event_target_member_from_context(
                snapshot,
                scope_index,
                access.scope,
                qualifier.as_ref(),
                event_name,
            )?;
            if !class_member_visible_to(
                snapshot,
                snapshot.symbols.as_ref(),
                access.scope,
                member_unit,
                member,
            ) {
                return None;
            }
            let parameter = member.parameters.iter().find(|parameter| {
                parameter.name == access.name
                    && call_section_matches_event_parameter(access.section, parameter)
            })?;
            Some(NamedArgumentParameterInfo {
                name: Arc::clone(&parameter.name),
                declared_type: parameter.declared_type.clone(),
            })
        }
        NamedArgumentTarget::Routine { routine_name } => {
            resolve_routine_named_argument_parameter_with_scope_index(
                snapshot,
                scope_index,
                access.scope,
                routine_name,
                &access.name,
            )
        }
        NamedArgumentTarget::ImplicitMethod { method_name } => {
            let unit = snapshot.symbols.as_ref();
            let class_symbol_id = enclosing_class_owner(unit, access.scope)?;
            let (member_unit, member) =
                resolve_class_member_in_hierarchy(snapshot, unit, class_symbol_id, method_name)?;
            if member.kind != ClassMemberKind::Method {
                return None;
            }
            if !class_member_visible_to(
                snapshot,
                snapshot.symbols.as_ref(),
                access.scope,
                member_unit,
                member,
            ) {
                return None;
            }
            let parameter = member
                .parameters
                .iter()
                .find(|parameter| parameter.name == access.name)?;
            Some(NamedArgumentParameterInfo {
                name: Arc::clone(&parameter.name),
                declared_type: parameter.declared_type.clone(),
            })
        }
        NamedArgumentTarget::Method {
            base_namespace,
            base_name,
            method_name,
            ..
        } => {
            if let Some(call_site) = call_site_for_named_argument(snapshot, access)
                && let Some((_, member)) = resolve_call_target_member(snapshot, call_site)
            {
                let parameter = member
                    .parameters
                    .iter()
                    .find(|parameter| parameter.name == access.name)?;
                return Some(NamedArgumentParameterInfo {
                    name: Arc::clone(&parameter.name),
                    declared_type: parameter.declared_type.clone(),
                });
            }
            let (unit, class_symbol_id, requires_static) =
                resolve_method_target_from_context_with_scope_index(
                    snapshot,
                    scope_index,
                    access.scope,
                    *base_namespace,
                    base_name,
                )?;
            let (member_unit, member) =
                resolve_class_member_in_hierarchy(snapshot, unit, class_symbol_id, method_name)?;
            if member.kind != ClassMemberKind::Method || (requires_static && !member.is_static) {
                return None;
            }
            if !class_member_visible_to(
                snapshot,
                snapshot.symbols.as_ref(),
                access.scope,
                member_unit,
                member,
            ) {
                return None;
            }
            let parameter = member
                .parameters
                .iter()
                .find(|parameter| parameter.name == access.name)?;
            Some(NamedArgumentParameterInfo {
                name: Arc::clone(&parameter.name),
                declared_type: parameter.declared_type.clone(),
            })
        }
    }
}

fn function_module_parameter_inlay_hint(
    snapshot: &AnalysisSnapshot,
    call_site: &CallSiteData,
    argument: &CallArgumentData,
) -> Option<ParameterInlayHintInfo> {
    let NamedArgumentTarget::Function { function_name } = &call_site.target else {
        return None;
    };
    if matches!(argument.section, Some(NamedArgumentSection::Exceptions)) {
        return None;
    }
    let argument_name = argument.name.as_ref()?;
    let (unit, function_symbol_id) = resolve_symbol_from_context_with_scope_index(
        snapshot,
        snapshot.scope_index(),
        call_site.scope,
        Namespace::Routine,
        function_name,
        false,
    )?;
    let function_module = unit.function_module(function_symbol_id)?;
    let parameter = function_module.parameters.iter().find(|parameter| {
        parameter.name == *argument_name
            && call_section_matches_function_parameter(argument.section, parameter)
    })?;
    let position = named_argument_value_inlay_position(snapshot.text.as_ref(), &argument.range)?;
    let label = function_module_parameter_completion_declared_type(parameter)?;
    Some(ParameterInlayHintInfo {
        position,
        label: Arc::from(label),
        trailing_colon: false,
        padding_left: false,
        padding_right: true,
        tooltip_markdown: function_module_parameter_inlay_hint_markdown(function_name, parameter),
    })
}

fn method_parameter_inlay_hint(
    snapshot: &AnalysisSnapshot,
    call_site: &CallSiteData,
    argument: &CallArgumentData,
) -> Option<ParameterInlayHintInfo> {
    if matches!(
        call_site.target,
        NamedArgumentTarget::Function { .. }
            | NamedArgumentTarget::Routine { .. }
            | NamedArgumentTarget::Report { .. }
    ) {
        return None;
    }
    let argument_name = argument.name.as_ref()?;
    let callable = resolve_callable_completion_target(snapshot, call_site)?;
    let (member, parameter) = match callable {
        CallableCompletionTarget::Method(member) => {
            let parameter = member.parameters.iter().find(|parameter| {
                parameter.name == *argument_name
                    && call_section_matches_parameter(argument.section, parameter.section)
            })?;
            (member, parameter)
        }
        CallableCompletionTarget::Event(member) => {
            let parameter = member.parameters.iter().find(|parameter| {
                parameter.name == *argument_name
                    && call_section_matches_event_parameter(argument.section, parameter)
            })?;
            (member, parameter)
        }
        CallableCompletionTarget::Function(_) => return None,
    };
    let position = named_argument_value_inlay_position(snapshot.text.as_ref(), &argument.range)?;
    let label = parameter_completion_declared_type(parameter)?;
    Some(ParameterInlayHintInfo {
        position,
        label: Arc::from(label),
        trailing_colon: false,
        padding_left: false,
        padding_right: true,
        tooltip_markdown: method_parameter_inlay_hint_markdown(call_site, member, parameter),
    })
}

fn named_argument_value_inlay_position(text: &str, range: &Range<usize>) -> Option<usize> {
    let arg_text = text.get(range.clone())?;
    if let Some(eq_offset) = arg_text.find('=') {
        let value_start = range.start + eq_offset + 1;
        let remaining = text.get(value_start..range.end)?;
        let non_whitespace_offset = remaining
            .char_indices()
            .find_map(|(offset, ch)| (!ch.is_whitespace()).then_some(offset))?;
        return Some(value_start + non_whitespace_offset);
    }
    arg_text
        .char_indices()
        .find_map(|(offset, ch)| (!ch.is_whitespace()).then_some(range.start + offset))
}

fn resolve_named_argument_target(
    snapshot: &AnalysisSnapshot,
    access: &NamedArgumentAccess,
) -> Option<DefinitionTarget> {
    match &access.target {
        NamedArgumentTarget::Constructor { type_name } => {
            let (unit, class_symbol_id) = resolve_symbol_from_context(
                snapshot,
                access.scope,
                Namespace::Type,
                type_name,
                false,
            )?;
            if unit.symbol(class_symbol_id).kind != SymbolKind::Class {
                return None;
            }
            let parameter = unit
                .semantic()
                .decls()
                .class_member(class_symbol_id, "constructor")?
                .parameters
                .iter()
                .find(|parameter| parameter.name == access.name)?;
            Some(definition_target_for_range(unit, parameter.range.clone()))
        }
        NamedArgumentTarget::Function { function_name } => {
            let (unit, function_symbol_id) = resolve_symbol_from_context(
                snapshot,
                access.scope,
                Namespace::Routine,
                function_name,
                false,
            )?;
            let function_module = unit.function_module(function_symbol_id)?;
            if let Some(parameter) = function_module
                .parameters
                .iter()
                .find(|parameter| parameter.name == access.name)
            {
                return Some(definition_target_for_range(unit, parameter.range.clone()));
            }
            let exception = function_module
                .exceptions
                .iter()
                .find(|exception| exception.name == access.name)?;
            Some(definition_target_for_range(unit, exception.range.clone()))
        }
        NamedArgumentTarget::Report { .. } => None,
        NamedArgumentTarget::Event {
            qualifier,
            event_name,
        } => {
            let (member_unit, member) = resolve_event_target_member_from_context(
                snapshot,
                snapshot.scope_index(),
                access.scope,
                qualifier.as_ref(),
                event_name,
            )?;
            if !class_member_visible_to(
                snapshot,
                snapshot.symbols.as_ref(),
                access.scope,
                member_unit,
                member,
            ) {
                return None;
            }
            let parameter = member.parameters.iter().find(|parameter| {
                parameter.name == access.name
                    && call_section_matches_event_parameter(access.section, parameter)
            })?;
            Some(definition_target_for_range(
                member_unit,
                parameter.range.clone(),
            ))
        }
        NamedArgumentTarget::Routine { routine_name } => {
            let (unit, routine_symbol_id) = resolve_symbol_from_context(
                snapshot,
                access.scope,
                Namespace::Routine,
                routine_name,
                false,
            )?;
            let parameter = unit
                .semantic()
                .decls()
                .routine_parameters(routine_symbol_id)
                .find(|symbol| symbol.name == access.name)?;
            Some(definition_target_for_symbol(unit, parameter))
        }
        NamedArgumentTarget::ImplicitMethod { method_name } => {
            let unit = snapshot.symbols.as_ref();
            let class_symbol_id = enclosing_class_owner(unit, access.scope)?;
            let (member_unit, member) =
                resolve_class_member_in_hierarchy(snapshot, unit, class_symbol_id, method_name)?;
            if member.kind != ClassMemberKind::Method {
                return None;
            }
            if !class_member_visible_to(
                snapshot,
                snapshot.symbols.as_ref(),
                access.scope,
                member_unit,
                member,
            ) {
                return None;
            }
            let parameter = member
                .parameters
                .iter()
                .find(|parameter| parameter.name == access.name)?;
            Some(definition_target_for_range(
                member_unit,
                parameter.range.clone(),
            ))
        }
        NamedArgumentTarget::Method {
            base_namespace,
            base_name,
            method_name,
            ..
        } => {
            if let Some(call_site) = call_site_for_named_argument(snapshot, access)
                && let Some((member_unit, member)) = resolve_call_target_member(snapshot, call_site)
            {
                let parameter = member
                    .parameters
                    .iter()
                    .find(|parameter| parameter.name == access.name)?;
                return Some(definition_target_for_range(
                    member_unit,
                    parameter.range.clone(),
                ));
            }
            let (unit, class_symbol_id, requires_static) = resolve_method_target_from_context(
                snapshot,
                access.scope,
                *base_namespace,
                base_name,
            )?;
            let (member_unit, member) =
                resolve_class_member_in_hierarchy(snapshot, unit, class_symbol_id, method_name)?;
            if member.kind != ClassMemberKind::Method || (requires_static && !member.is_static) {
                return None;
            }
            if !class_member_visible_to(
                snapshot,
                snapshot.symbols.as_ref(),
                access.scope,
                member_unit,
                member,
            ) {
                return None;
            }
            let parameter = member
                .parameters
                .iter()
                .find(|parameter| parameter.name == access.name)?;
            Some(definition_target_for_range(
                member_unit,
                parameter.range.clone(),
            ))
        }
    }
}

fn resolve_named_argument_symbol(
    snapshot: &AnalysisSnapshot,
    access: &NamedArgumentAccess,
) -> Option<abap_symbols::SymbolHandle> {
    match &access.target {
        NamedArgumentTarget::Constructor { type_name } => {
            let (unit, class_symbol_id) = resolve_symbol_from_context(
                snapshot,
                access.scope,
                Namespace::Type,
                type_name,
                false,
            )?;
            if unit.symbol(class_symbol_id).kind != SymbolKind::Class {
                return None;
            }
            let parameter = unit
                .semantic()
                .decls()
                .class_member(class_symbol_id, "constructor")?
                .parameters
                .iter()
                .find(|parameter| parameter.name == access.name)?;
            symbol_handle_for_decl_range(unit, &parameter.range, SymbolKind::Parameter)
        }
        NamedArgumentTarget::Function { function_name } => {
            let (unit, function_symbol_id) = resolve_symbol_from_context(
                snapshot,
                access.scope,
                Namespace::Routine,
                function_name,
                false,
            )?;
            let function_module = unit.function_module(function_symbol_id)?;
            if let Some(parameter) = function_module
                .parameters
                .iter()
                .find(|parameter| parameter.name == access.name)
            {
                return symbol_handle_for_decl_range(unit, &parameter.range, SymbolKind::Parameter);
            }
            None
        }
        NamedArgumentTarget::Report { .. } => None,
        NamedArgumentTarget::Event {
            qualifier,
            event_name,
        } => {
            let (member_unit, member) = resolve_event_target_member_from_context(
                snapshot,
                snapshot.scope_index(),
                access.scope,
                qualifier.as_ref(),
                event_name,
            )?;
            if !class_member_visible_to(
                snapshot,
                snapshot.symbols.as_ref(),
                access.scope,
                member_unit,
                member,
            ) {
                return None;
            }
            let parameter = member.parameters.iter().find(|parameter| {
                parameter.name == access.name
                    && call_section_matches_event_parameter(access.section, parameter)
            })?;
            symbol_handle_for_decl_range(member_unit, &parameter.range, SymbolKind::Parameter)
        }
        NamedArgumentTarget::Routine { routine_name } => {
            let (unit, routine_symbol_id) = resolve_symbol_from_context(
                snapshot,
                access.scope,
                Namespace::Routine,
                routine_name,
                false,
            )?;
            let parameter = unit
                .semantic()
                .decls()
                .routine_parameters(routine_symbol_id)
                .find(|symbol| symbol.name == access.name)?;
            Some(abap_symbols::SymbolHandle {
                unit: unit.unit_id,
                symbol: parameter.id,
            })
        }
        NamedArgumentTarget::ImplicitMethod { method_name } => {
            let unit = snapshot.symbols.as_ref();
            let class_symbol_id = enclosing_class_owner(unit, access.scope)?;
            let (member_unit, member) =
                resolve_class_member_in_hierarchy(snapshot, unit, class_symbol_id, method_name)?;
            if member.kind != ClassMemberKind::Method {
                return None;
            }
            if !class_member_visible_to(
                snapshot,
                snapshot.symbols.as_ref(),
                access.scope,
                member_unit,
                member,
            ) {
                return None;
            }
            let parameter = member
                .parameters
                .iter()
                .find(|parameter| parameter.name == access.name)?;
            symbol_handle_for_decl_range(member_unit, &parameter.range, SymbolKind::Parameter)
        }
        NamedArgumentTarget::Method {
            base_namespace,
            base_name,
            method_name,
            ..
        } => {
            if let Some(call_site) = call_site_for_named_argument(snapshot, access)
                && let Some((member_unit, member)) = resolve_call_target_member(snapshot, call_site)
            {
                let parameter = member
                    .parameters
                    .iter()
                    .find(|parameter| parameter.name == access.name)?;
                return symbol_handle_for_decl_range(
                    member_unit,
                    &parameter.range,
                    SymbolKind::Parameter,
                );
            }
            let (unit, class_symbol_id, requires_static) = resolve_method_target_from_context(
                snapshot,
                access.scope,
                *base_namespace,
                base_name,
            )?;
            let (member_unit, member) =
                resolve_class_member_in_hierarchy(snapshot, unit, class_symbol_id, method_name)?;
            if member.kind != ClassMemberKind::Method || (requires_static && !member.is_static) {
                return None;
            }
            if !class_member_visible_to(
                snapshot,
                snapshot.symbols.as_ref(),
                access.scope,
                member_unit,
                member,
            ) {
                return None;
            }
            let parameter = member
                .parameters
                .iter()
                .find(|parameter| parameter.name == access.name)?;
            symbol_handle_for_decl_range(member_unit, &parameter.range, SymbolKind::Parameter)
        }
    }
}

fn resolve_routine_named_argument_parameter_with_scope_index(
    snapshot: &AnalysisSnapshot,
    scope_index: &ScopeIndex,
    scope: ScopeId,
    routine_name: &Arc<str>,
    parameter_name: &Arc<str>,
) -> Option<NamedArgumentParameterInfo> {
    if let Some((unit, routine_symbol_id)) = resolve_symbol_from_context_with_scope_index(
        snapshot,
        scope_index,
        scope,
        Namespace::Routine,
        routine_name,
        false,
    ) {
        let parameter = unit
            .routine_parameters(routine_symbol_id)
            .find(|symbol| symbol.name == *parameter_name)?;
        return Some(NamedArgumentParameterInfo {
            name: Arc::clone(&parameter.name),
            declared_type: parameter.declared_type.clone(),
        });
    }
    None
}

fn resolve_symbol_from_context<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope: ScopeId,
    namespace: Namespace,
    name: &Arc<str>,
    in_type_position: bool,
) -> Option<(&'a UnitAnalysis, SymbolId)> {
    resolve_symbol_from_context_with_scope_index(
        snapshot,
        snapshot.scope_index(),
        scope,
        namespace,
        name,
        in_type_position,
    )
}

fn resolve_symbol_from_context_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    scope: ScopeId,
    namespace: Namespace,
    name: &Arc<str>,
    in_type_position: bool,
) -> Option<(&'a UnitAnalysis, SymbolId)> {
    let current_unit = &snapshot.symbols;
    for namespace in [
        Some(namespace),
        fallback_namespace_for_context(namespace, in_type_position),
    ] {
        let Some(namespace) = namespace else {
            continue;
        };
        if let Some(symbol_id) =
            lookup_scope_chain(current_unit, scope_index, scope, namespace, name)
        {
            if namespace == Namespace::Type
                && current_unit.symbol(symbol_id).kind == SymbolKind::Class
                && current_unit.class_definition(symbol_id).is_none()
                && let Some(handle) = snapshot
                    .project
                    .visible_type_owner_handle(current_unit.unit_id, name)
                && handle.unit != current_unit.unit_id
            {
                let unit = &snapshot.project.units[handle.unit.as_usize()];
                return Some((unit, handle.symbol));
            }
            return Some((current_unit, symbol_id));
        }
        if namespace == Namespace::Type
            && let Some(class_symbol) = enclosing_class_owner(current_unit, scope)
        {
            let class_name = Arc::clone(&current_unit.symbol(class_symbol).name);
            if let Some(class_handle) = snapshot
                .project
                .visible_type_owner_handle(current_unit.unit_id, &class_name)
            {
                let class_unit = &snapshot.project.units[class_handle.unit.as_usize()];
                if let Some((type_unit, type_symbol)) = resolve_class_type_symbol_in_hierarchy(
                    snapshot,
                    class_unit,
                    class_handle.symbol,
                    name.as_ref(),
                ) {
                    return Some((type_unit, type_symbol.id));
                }
            }
        }
    }

    let namespaces = [
        Some(namespace),
        fallback_namespace_for_context(namespace, in_type_position),
    ];
    for namespace in namespaces {
        let Some(namespace) = namespace else {
            continue;
        };
        for target in current_unit
            .include_edges
            .iter()
            .filter_map(|edge| edge.target)
        {
            let unit = &snapshot.project.units[target.as_usize()];
            if let Some(symbol_id) = unit
                .symbols
                .iter()
                .find(|symbol| {
                    symbol.scope == unit.root_scope
                        && symbol.name == *name
                        && symbol.kind.occupies(namespace)
                })
                .map(|symbol| symbol.id)
            {
                return Some((unit, symbol_id));
            }
        }
    }

    for namespace in namespaces {
        let Some(namespace) = namespace else {
            continue;
        };
        for unit in &snapshot.project.units {
            if let Some(symbol_id) = unit
                .symbols
                .iter()
                .find(|symbol| {
                    symbol.scope == unit.root_scope
                        && symbol.name == *name
                        && symbol.kind.occupies(namespace)
                })
                .map(|symbol| symbol.id)
            {
                return Some((unit, symbol_id));
            }
        }
    }

    None
}

fn resolve_method_target_from_base_symbol_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    scope: ScopeId,
    namespace: Namespace,
    unit: &'a UnitAnalysis,
    symbol_id: SymbolId,
) -> Option<(&'a UnitAnalysis, SymbolId, bool)> {
    let base_symbol = unit.symbol(symbol_id);
    if namespace == Namespace::Type && base_symbol.kind == SymbolKind::Class {
        return Some((unit, symbol_id, true));
    }
    if namespace == Namespace::Value && base_symbol.kind == SymbolKind::Class {
        return Some((unit, symbol_id, false));
    }
    if namespace != Namespace::Value {
        return None;
    }
    let declared_type = base_symbol.declared_type.as_ref()?;
    if !declared_type.is_ref || !declared_type.field_path.is_empty() {
        return None;
    }
    let (class_unit, class_symbol_id) = resolve_symbol_from_context_with_scope_index(
        snapshot,
        scope_index,
        scope,
        Namespace::Type,
        &declared_type.base_name,
        false,
    )?;
    matches!(
        class_unit.symbol(class_symbol_id).kind,
        SymbolKind::Class | SymbolKind::Interface
    )
    .then_some((class_unit, class_symbol_id, false))
}

fn resolve_method_target_from_context<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope: ScopeId,
    namespace: Namespace,
    name: &Arc<str>,
) -> Option<(&'a UnitAnalysis, SymbolId, bool)> {
    resolve_method_target_from_context_with_scope_index(
        snapshot,
        snapshot.scope_index(),
        scope,
        namespace,
        name,
    )
}

fn resolve_method_target_from_context_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    scope: ScopeId,
    namespace: Namespace,
    name: &Arc<str>,
) -> Option<(&'a UnitAnalysis, SymbolId, bool)> {
    if namespace == Namespace::Value && name.as_ref() == "super" {
        let (unit, symbol_id) =
            resolve_direct_superclass_from_scope_with_scope_index(snapshot, scope_index, scope)?;
        return Some((unit, symbol_id, false));
    }
    let (unit, symbol_id) = resolve_symbol_from_context_with_scope_index(
        snapshot,
        scope_index,
        scope,
        namespace,
        name,
        false,
    )
    .or_else(|| resolved_reference_symbol_in_scope(snapshot, scope, namespace, name))?;
    resolve_method_target_from_base_symbol_with_scope_index(
        snapshot,
        scope_index,
        scope,
        namespace,
        unit,
        symbol_id,
    )
}

fn resolved_reference_symbol_in_scope<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope: ScopeId,
    namespace: Namespace,
    name: &Arc<str>,
) -> Option<(&'a UnitAnalysis, SymbolId)> {
    let handle = snapshot
        .symbols
        .references
        .iter()
        .find(|reference| {
            reference.scope == scope
                && reference.namespace == namespace
                && reference.name.as_ref() == name.as_ref()
        })
        .and_then(|reference| match reference.resolution.as_ref()? {
            Resolution::Symbol(handle) => Some(*handle),
            _ => None,
        })?;
    Some((
        &snapshot.project.units[handle.unit.as_usize()],
        handle.symbol,
    ))
}

fn resolve_event_target_member_from_context<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    scope: ScopeId,
    qualifier: Option<&Arc<str>>,
    event_name: &Arc<str>,
) -> Option<(&'a UnitAnalysis, &'a ClassMemberData)> {
    let unit = snapshot.symbols.as_ref();
    let class_symbol_id = enclosing_class_owner(unit, scope)?;
    resolve_class_event_in_hierarchy_with_scope_index(
        snapshot,
        scope_index,
        unit,
        class_symbol_id,
        scope,
        qualifier.map(|name| name.as_ref()),
        event_name.as_ref(),
    )
}

fn resolve_direct_superclass_from_scope_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    scope: ScopeId,
) -> Option<(&'a UnitAnalysis, SymbolId)> {
    let class_symbol = enclosing_class_owner(snapshot.symbols.as_ref(), scope)?;
    let inheritance = snapshot
        .symbols
        .semantic()
        .decls()
        .class_superclass(class_symbol)?;
    let (unit, symbol_id) = resolve_symbol_from_context_with_scope_index(
        snapshot,
        scope_index,
        scope,
        Namespace::Type,
        &inheritance.superclass_name,
        false,
    )?;
    (unit.symbol(symbol_id).kind == SymbolKind::Class).then_some((unit, symbol_id))
}

fn fallback_namespace_for_context(
    namespace: Namespace,
    in_type_position: bool,
) -> Option<Namespace> {
    if !in_type_position {
        return None;
    }
    match namespace {
        Namespace::Type => Some(Namespace::Value),
        Namespace::Value => Some(Namespace::Type),
        Namespace::Routine => None,
    }
}

fn enclosing_class_owner(unit: &UnitAnalysis, scope: ScopeId) -> Option<SymbolId> {
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        let scope = unit.scope(scope_id);
        if scope.kind == abap_symbols::ScopeKind::Class {
            return scope.owner;
        }
        current = scope.parent;
    }
    None
}

fn class_member_visible_to(
    snapshot: &AnalysisSnapshot,
    caller_unit: &UnitAnalysis,
    caller_scope: ScopeId,
    target_unit: &UnitAnalysis,
    member: &ClassMemberData,
) -> bool {
    match member.visibility {
        Visibility::Public => true,
        Visibility::Private => {
            let Some(caller_class_symbol) = enclosing_class_owner(caller_unit, caller_scope) else {
                return false;
            };
            visible_class_handle(snapshot, caller_unit, caller_class_symbol)
                == visible_class_handle(snapshot, target_unit, member.class_symbol)
        }
        Visibility::Protected => {
            let Some(caller_class_symbol) = enclosing_class_owner(caller_unit, caller_scope) else {
                return false;
            };
            class_is_or_inherits_from(
                snapshot,
                visible_class_handle(snapshot, caller_unit, caller_class_symbol),
                visible_class_handle(snapshot, target_unit, member.class_symbol),
            )
        }
    }
}

fn resolve_class_member_in_hierarchy<'a>(
    snapshot: &'a AnalysisSnapshot,
    class_unit: &'a UnitAnalysis,
    class_symbol: SymbolId,
    member_name: &str,
) -> Option<(&'a UnitAnalysis, &'a ClassMemberData)> {
    let mut current = visible_class_handle(snapshot, class_unit, class_symbol);
    let mut visited = HashSet::new();
    loop {
        if !visited.insert(current) {
            return None;
        }
        let unit = &snapshot.project.units[current.0.as_usize()];
        if let Some(member) = unit.semantic().decls().class_member(current.1, member_name) {
            if !class_member_uses_inherited_signature(member) {
                return Some((unit, member));
            }
        }
        if let Some(member) =
            resolve_class_member_alias_target(snapshot, unit, current.1, member_name)
        {
            return Some(member);
        }
        let (next_unit, next_symbol) = direct_superclass_from_class(snapshot, unit, current.1)?;
        current = (next_unit.unit_id, next_symbol);
    }
}

fn resolve_class_member_alias_target<'a>(
    snapshot: &'a AnalysisSnapshot,
    owner_unit: &'a UnitAnalysis,
    owner_symbol: SymbolId,
    alias_name: &str,
) -> Option<(&'a UnitAnalysis, &'a ClassMemberData)> {
    let alias = owner_unit.member_aliases.iter().find(|alias| {
        alias.owner_symbol == owner_symbol && alias.alias_name.as_ref() == alias_name
    })?;
    let (target_unit, target_symbol) = resolve_exposed_interface_handle(
        snapshot,
        owner_unit,
        owner_symbol,
        &alias.target_interface_name,
    )?;
    target_unit
        .semantic()
        .decls()
        .class_member(target_symbol, alias.target_member_name.as_ref())
        .map(|member| (target_unit, member))
}

fn resolve_exposed_interface_handle<'a>(
    snapshot: &'a AnalysisSnapshot,
    owner_unit: &'a UnitAnalysis,
    owner_symbol: SymbolId,
    interface_name: &Arc<str>,
) -> Option<(&'a UnitAnalysis, SymbolId)> {
    resolve_exposed_interface_handle_inner_simple(
        snapshot,
        owner_unit,
        owner_symbol,
        interface_name,
        &mut HashSet::new(),
    )
}

fn resolve_exposed_interface_handle_inner_simple<'a>(
    snapshot: &'a AnalysisSnapshot,
    owner_unit: &'a UnitAnalysis,
    owner_symbol: SymbolId,
    interface_name: &Arc<str>,
    visited: &mut HashSet<(UnitId, SymbolId)>,
) -> Option<(&'a UnitAnalysis, SymbolId)> {
    if !visited.insert((owner_unit.unit_id, owner_symbol)) {
        return None;
    }
    for implemented in owner_unit
        .implemented_interfaces
        .iter()
        .filter(|implemented| implemented.owner_symbol == owner_symbol)
    {
        let Some((interface_unit, interface_symbol)) =
            resolve_project_interface_symbol(snapshot, owner_unit, &implemented.interface_name)
        else {
            continue;
        };
        if implemented.interface_name == *interface_name {
            return Some((interface_unit, interface_symbol));
        }
        if let Some(found) = resolve_exposed_interface_handle_inner_simple(
            snapshot,
            interface_unit,
            interface_symbol,
            interface_name,
            visited,
        ) {
            return Some(found);
        }
    }
    if owner_unit.symbol(owner_symbol).kind == SymbolKind::Class
        && let Some((super_unit, super_symbol)) =
            direct_superclass_from_class(snapshot, owner_unit, owner_symbol)
    {
        return resolve_exposed_interface_handle_inner_simple(
            snapshot,
            super_unit,
            super_symbol,
            interface_name,
            visited,
        );
    }
    None
}

fn class_member_uses_inherited_signature(member: &ClassMemberData) -> bool {
    member.kind == ClassMemberKind::Method
        && member.parameters.is_empty()
        && member.signature.split_ascii_whitespace().any(|part| {
            let keyword = part.trim_end_matches('.');
            keyword.eq_ignore_ascii_case("redefinition")
        })
}

fn method_implementation_signature_member<'a>(
    snapshot: &'a AnalysisSnapshot,
    member_unit: &'a UnitAnalysis,
    member: &'a ClassMemberData,
) -> (&'a UnitAnalysis, &'a ClassMemberData) {
    if class_member_uses_inherited_signature(member)
        && let Some((resolved_unit, resolved_member)) = resolve_class_member_in_hierarchy(
            snapshot,
            member_unit,
            member.class_symbol,
            member.name.as_ref(),
        )
    {
        return (resolved_unit, resolved_member);
    }
    (member_unit, member)
}

fn method_implementation_signature_member_at_offset(
    snapshot: &AnalysisSnapshot,
    offset: usize,
) -> Option<(&UnitAnalysis, &ClassMemberData, &ClassMemberData)> {
    for unit in &snapshot.project.units {
        for member in &unit.class_members {
            if member.kind != ClassMemberKind::Method {
                continue;
            }
            let Some(implementation) = member.implementation.as_ref() else {
                continue;
            };
            if implementation.unit != snapshot.symbols.unit_id {
                continue;
            }
            let anchor = method_implementation_parameter_anchor(
                snapshot.text.as_ref(),
                &implementation.range,
            );
            let header_start = line_start(snapshot.text.as_ref(), implementation.range.start);
            let header_end = line_end_including_newline(snapshot.text.as_ref(), anchor);
            if offset < header_start || offset > header_end {
                continue;
            }
            let (_, signature_member) =
                method_implementation_signature_member(snapshot, unit, member);
            return Some((unit, member, signature_member));
        }
    }
    None
}

fn collect_class_value_members_in_hierarchy<'a>(
    snapshot: &'a AnalysisSnapshot,
    class_unit: &'a UnitAnalysis,
    class_symbol: SymbolId,
) -> Vec<(&'a UnitAnalysis, &'a ClassMemberData)> {
    let mut current = (class_unit.unit_id, class_symbol);
    let mut visited_classes = HashSet::new();
    let mut seen_names = HashSet::new();
    let mut out = Vec::new();
    loop {
        if !visited_classes.insert(current) {
            break;
        }
        let unit = &snapshot.project.units[current.0.as_usize()];
        for member in unit.semantic().decls().class_members_for(current.1) {
            if !matches!(
                member.kind,
                ClassMemberKind::Attribute | ClassMemberKind::Method
            ) || !seen_names.insert(Arc::clone(&member.name))
            {
                continue;
            }
            out.push((unit, member));
        }
        let Some((next_unit, next_symbol)) =
            direct_superclass_from_class(snapshot, unit, current.1)
        else {
            break;
        };
        current = (next_unit.unit_id, next_symbol);
    }
    out
}

fn hovered_component_kind_for_class_member(member: &ClassMemberData) -> HoveredComponentKind {
    match member.kind {
        ClassMemberKind::Attribute => HoveredComponentKind::Attribute,
        ClassMemberKind::Method => HoveredComponentKind::Method,
        ClassMemberKind::Event => HoveredComponentKind::Method,
    }
}

fn class_scoped_type_symbol_for_owner<'a>(
    unit: &'a UnitAnalysis,
    owner_symbol: SymbolId,
    type_name: &str,
) -> Option<&'a SymbolData> {
    unit.symbols.iter().find(|symbol| {
        symbol.kind == SymbolKind::TypeDef
            && symbol.name.as_ref() == type_name
            && unit.scope(symbol.scope).owner == Some(owner_symbol)
            && matches!(
                unit.scope(symbol.scope).kind,
                ScopeKind::Class | ScopeKind::Interface
            )
    })
}

fn resolve_class_type_symbol_in_hierarchy<'a>(
    snapshot: &'a AnalysisSnapshot,
    class_unit: &'a UnitAnalysis,
    class_symbol: SymbolId,
    type_name: &str,
) -> Option<(&'a UnitAnalysis, &'a SymbolData)> {
    let mut current = (class_unit.unit_id, class_symbol);
    let mut visited = HashSet::new();
    loop {
        if !visited.insert(current) {
            return None;
        }
        let unit = &snapshot.project.units[current.0.as_usize()];
        if let Some(symbol) = class_scoped_type_symbol_for_owner(unit, current.1, type_name) {
            return Some((unit, symbol));
        }
        let (next_unit, next_symbol) = direct_superclass_from_class(snapshot, unit, current.1)?;
        current = (next_unit.unit_id, next_symbol);
    }
}

fn collect_class_types_in_hierarchy<'a>(
    snapshot: &'a AnalysisSnapshot,
    class_unit: &'a UnitAnalysis,
    class_symbol: SymbolId,
) -> Vec<(&'a UnitAnalysis, &'a SymbolData)> {
    let mut current = (class_unit.unit_id, class_symbol);
    let mut visited_classes = HashSet::new();
    let mut seen_names = HashSet::new();
    let mut out = Vec::new();
    loop {
        if !visited_classes.insert(current) {
            break;
        }
        let unit = &snapshot.project.units[current.0.as_usize()];
        for symbol in unit.symbols.iter().filter(|symbol| {
            symbol.kind == SymbolKind::TypeDef
                && unit.scope(symbol.scope).owner == Some(current.1)
                && matches!(
                    unit.scope(symbol.scope).kind,
                    ScopeKind::Class | ScopeKind::Interface
                )
        }) {
            if !seen_names.insert(Arc::clone(&symbol.name)) {
                continue;
            }
            out.push((unit, symbol));
        }
        let Some((next_unit, next_symbol)) =
            direct_superclass_from_class(snapshot, unit, current.1)
        else {
            break;
        };
        current = (next_unit.unit_id, next_symbol);
    }
    out
}

fn symbol_selector_declared_type(unit: &UnitAnalysis, symbol: &SymbolData) -> Option<String> {
    symbol_type_presentation(None, symbol)
        .map(|presentation| presentation.rendered_clause)
        .or_else(|| {
            symbol
                .structure
                .map(|structure_id| format!("TYPE {}", unit.structure(structure_id).name))
        })
}

fn format_selector_type_declaration(unit: &UnitAnalysis, symbol: &SymbolData) -> String {
    match symbol_selector_declared_type(unit, symbol) {
        Some(declared_type) => format!("TYPES {} {}.", symbol.name, declared_type),
        None => format!("TYPES {}.", symbol.name),
    }
}

fn selector_completion_item_for_type_symbol(
    unit: &UnitAnalysis,
    symbol: &SymbolData,
) -> SelectorCompletionItem {
    SelectorCompletionItem {
        name: Arc::clone(&symbol.name),
        declared_type: symbol_selector_declared_type(unit, symbol),
        declaration: Some(format_selector_type_declaration(unit, symbol)),
        kind: HoveredComponentKind::Type,
        field_owner_structure_name: None,
        insertion: identifier_completion_insertion(symbol.name.as_ref()),
    }
}

fn resolve_class_selector_member<'a>(
    snapshot: &'a AnalysisSnapshot,
    access: &abap_symbols::FieldAccess,
    segment_index: usize,
    unit: &'a UnitAnalysis,
    symbol_id: SymbolId,
) -> Option<(&'a UnitAnalysis, &'a ClassMemberData)> {
    resolve_class_selector_member_with_scope_index(
        snapshot,
        snapshot.scope_index(),
        access,
        segment_index,
        unit,
        symbol_id,
    )
}

fn resolve_interface_selector_method_symbol<'a>(
    snapshot: &'a AnalysisSnapshot,
    access: &abap_symbols::FieldAccess,
    unit: &'a UnitAnalysis,
    symbol_id: SymbolId,
) -> Option<(&'a UnitAnalysis, &'a SymbolData)> {
    resolve_interface_selector_method_symbol_with_scope_index(
        snapshot,
        snapshot.scope_index(),
        access,
        unit,
        symbol_id,
    )
}

fn resolve_interface_selector_method_symbol_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    access: &abap_symbols::FieldAccess,
    unit: &'a UnitAnalysis,
    symbol_id: SymbolId,
) -> Option<(&'a UnitAnalysis, &'a SymbolData)> {
    let (class_unit, class_symbol_id, requires_static) =
        resolve_class_selector_base_with_scope_index(
            snapshot,
            scope_index,
            access,
            unit,
            symbol_id,
        )?;
    if requires_static || access.field_path.len() < 2 {
        return None;
    }
    let (method_unit, method_symbol_id) = resolve_fallback_qualified_method_symbol_in_hierarchy(
        snapshot,
        class_unit,
        class_symbol_id,
        &access.field_path[0].name,
        access.field_path[1].name.as_ref(),
    )?;
    Some((method_unit, method_unit.symbol(method_symbol_id)))
}

fn resolve_class_selector_type_symbol_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    access: &abap_symbols::FieldAccess,
    segment_index: usize,
    unit: &'a UnitAnalysis,
    symbol_id: SymbolId,
) -> Option<(&'a UnitAnalysis, &'a SymbolData)> {
    if segment_index != 0 || access.base_namespace != Namespace::Type {
        return None;
    }
    let (class_unit, class_symbol_id, _) = resolve_class_selector_base_with_scope_index(
        snapshot,
        scope_index,
        access,
        unit,
        symbol_id,
    )?;
    resolve_class_type_symbol_in_hierarchy(
        snapshot,
        class_unit,
        class_symbol_id,
        access.field_path[segment_index].name.as_ref(),
    )
}

fn resolve_class_selector_member_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    access: &abap_symbols::FieldAccess,
    segment_index: usize,
    unit: &'a UnitAnalysis,
    symbol_id: SymbolId,
) -> Option<(&'a UnitAnalysis, &'a ClassMemberData)> {
    if access.base_namespace == Namespace::Type
        && unit.symbol(symbol_id).kind == SymbolKind::Interface
    {
        return resolve_interface_member_path(
            snapshot,
            scope_index,
            unit,
            symbol_id,
            access.scope,
            &[access.field_path[segment_index].name.as_ref()],
        );
    }
    if segment_index != 0 {
        return resolve_interface_selector_member_with_scope_index(
            snapshot,
            scope_index,
            access,
            segment_index,
            unit,
            symbol_id,
        );
    }
    let (class_unit, class_symbol_id, requires_static) =
        resolve_class_selector_base_with_scope_index(
            snapshot,
            scope_index,
            access,
            unit,
            symbol_id,
        )?;
    if class_unit.symbol(class_symbol_id).kind == SymbolKind::Interface {
        return resolve_interface_member_path(
            snapshot,
            scope_index,
            class_unit,
            class_symbol_id,
            access.scope,
            &[access.field_path[segment_index].name.as_ref()],
        );
    }
    let (member_unit, member) = resolve_class_member_in_hierarchy(
        snapshot,
        class_unit,
        class_symbol_id,
        access.field_path[segment_index].name.as_ref(),
    )?;
    if requires_static && !member.is_static {
        return None;
    }
    class_member_visible_to(
        snapshot,
        snapshot.symbols.as_ref(),
        access.scope,
        member_unit,
        member,
    )
    .then_some((member_unit, member))
}

fn resolve_interface_selector_member_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    access: &abap_symbols::FieldAccess,
    segment_index: usize,
    unit: &'a UnitAnalysis,
    symbol_id: SymbolId,
) -> Option<(&'a UnitAnalysis, &'a ClassMemberData)> {
    let (class_unit, class_symbol_id, requires_static) =
        resolve_class_selector_base_with_scope_index(
            snapshot,
            scope_index,
            access,
            unit,
            symbol_id,
        )?;
    if requires_static || segment_index == 0 {
        return None;
    }
    let interface_name = &access.field_path[0].name;
    let member_path: Vec<_> = access.field_path[1..=segment_index]
        .iter()
        .map(|segment| segment.name.as_ref())
        .collect();
    let (member_unit, member) = resolve_exposed_interface_handle_with_scope_index(
        snapshot,
        scope_index,
        class_unit,
        class_symbol_id,
        access.scope,
        interface_name,
    )
    .and_then(|interface_handle| {
        resolve_interface_member_path(
            snapshot,
            scope_index,
            interface_handle.0,
            interface_handle.1,
            access.scope,
            &member_path,
        )
    })
    .or_else(|| {
        if segment_index != 1 {
            return None;
        }
        resolve_interface_member_via_qualified_class_member_with_scope_index(
            snapshot,
            scope_index,
            class_unit,
            class_symbol_id,
            access.scope,
            interface_name,
            access.field_path[1].name.as_ref(),
        )
    })?;
    class_member_visible_to(
        snapshot,
        snapshot.symbols.as_ref(),
        access.scope,
        member_unit,
        member,
    )
    .then_some((member_unit, member))
}

fn resolve_interface_selector_qualifier_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    access: &abap_symbols::FieldAccess,
    unit: &'a UnitAnalysis,
    symbol_id: SymbolId,
) -> Option<(&'a UnitAnalysis, SymbolId)> {
    let (class_unit, class_symbol_id, requires_static) =
        resolve_class_selector_base_with_scope_index(
            snapshot,
            scope_index,
            access,
            unit,
            symbol_id,
        )?;
    if requires_static || access.field_path.len() < 2 {
        return None;
    }
    resolve_exposed_interface_handle_with_scope_index(
        snapshot,
        scope_index,
        class_unit,
        class_symbol_id,
        access.scope,
        &access.field_path[0].name,
    )
    .or_else(|| {
        resolve_named_interface_from_qualified_class_member_with_scope_index(
            snapshot,
            scope_index,
            class_unit,
            class_symbol_id,
            access.scope,
            &access.field_path[0].name,
            access.field_path[1].name.as_ref(),
        )
    })
}

fn resolve_fallback_qualified_class_member_in_hierarchy<'a>(
    snapshot: &'a AnalysisSnapshot,
    class_unit: &'a UnitAnalysis,
    class_symbol_id: SymbolId,
    interface_name: &Arc<str>,
    member_name: &str,
) -> Option<(&'a UnitAnalysis, &'a ClassMemberData)> {
    let qualified_name = format!(
        "{}~{}",
        interface_name.as_ref().to_ascii_lowercase(),
        member_name.to_ascii_lowercase()
    );
    resolve_class_member_in_hierarchy(snapshot, class_unit, class_symbol_id, &qualified_name)
}

fn resolve_fallback_qualified_method_symbol_in_hierarchy<'a>(
    snapshot: &'a AnalysisSnapshot,
    class_unit: &'a UnitAnalysis,
    class_symbol_id: SymbolId,
    interface_name: &Arc<str>,
    member_name: &str,
) -> Option<(&'a UnitAnalysis, SymbolId)> {
    let qualified_name = format!(
        "{}~{}",
        interface_name.as_ref().to_ascii_lowercase(),
        member_name.to_ascii_lowercase()
    );
    let mut current = (class_unit, class_symbol_id);
    let mut visited = HashSet::new();
    loop {
        if !visited.insert((current.0.unit_id, current.1)) {
            return None;
        }
        if let Some(symbol) = current.0.symbols.iter().find(|symbol| {
            symbol.kind == SymbolKind::Method
                && symbol.name.as_ref() == qualified_name
                && enclosing_class_owner(current.0, symbol.scope) == Some(current.1)
        }) {
            return Some((current.0, symbol.id));
        }
        current = direct_superclass_from_class(snapshot, current.0, current.1)?;
    }
}

fn resolve_fallback_qualified_redefinition_member_in_hierarchy<'a>(
    snapshot: &'a AnalysisSnapshot,
    class_unit: &'a UnitAnalysis,
    class_symbol_id: SymbolId,
    interface_name: &Arc<str>,
    member_name: &str,
) -> Option<(&'a UnitAnalysis, &'a ClassMemberData)> {
    let pattern_spaced = format!(
        "{} ~ {}",
        interface_name.as_ref().to_ascii_lowercase(),
        member_name.to_ascii_lowercase()
    );
    let pattern_compact = pattern_spaced.replace(" ~ ", "~");
    let mut current = (class_unit, class_symbol_id);
    let mut visited = HashSet::new();
    loop {
        if !visited.insert((current.0.unit_id, current.1)) {
            return None;
        }
        if let Some(member) = current.0.class_members.iter().find(|member| {
            member.class_symbol == current.1
                && member.kind == ClassMemberKind::Method
                && member.name == *interface_name
                && {
                    let signature = member.signature.to_ascii_lowercase();
                    signature.contains(&pattern_spaced) || signature.contains(&pattern_compact)
                }
        }) {
            return Some((current.0, member));
        }
        current = direct_superclass_from_class(snapshot, current.0, current.1)?;
    }
}

fn class_hierarchy_supports_named_interface_member(
    snapshot: &AnalysisSnapshot,
    class_unit: &UnitAnalysis,
    class_symbol_id: SymbolId,
    interface_name: &Arc<str>,
    member_name: &str,
) -> bool {
    resolve_fallback_qualified_class_member_in_hierarchy(
        snapshot,
        class_unit,
        class_symbol_id,
        interface_name,
        member_name,
    )
    .is_some()
        || resolve_fallback_qualified_method_symbol_in_hierarchy(
            snapshot,
            class_unit,
            class_symbol_id,
            interface_name,
            member_name,
        )
        .is_some()
        || resolve_fallback_qualified_redefinition_member_in_hierarchy(
            snapshot,
            class_unit,
            class_symbol_id,
            interface_name,
            member_name,
        )
        .is_some()
}

fn resolve_named_interface_from_qualified_class_member_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    class_unit: &'a UnitAnalysis,
    class_symbol_id: SymbolId,
    scope: ScopeId,
    interface_name: &Arc<str>,
    member_name: &str,
) -> Option<(&'a UnitAnalysis, SymbolId)> {
    if !class_hierarchy_supports_named_interface_member(
        snapshot,
        class_unit,
        class_symbol_id,
        interface_name,
        member_name,
    ) {
        return None;
    }
    let (interface_unit, interface_symbol) = resolve_symbol_from_context_with_scope_index(
        snapshot,
        scope_index,
        scope,
        Namespace::Type,
        interface_name,
        false,
    )?;
    (interface_unit.symbol(interface_symbol).kind == SymbolKind::Interface)
        .then_some((interface_unit, interface_symbol))
}

fn resolve_interface_member_via_qualified_class_member_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    class_unit: &'a UnitAnalysis,
    class_symbol_id: SymbolId,
    scope: ScopeId,
    interface_name: &Arc<str>,
    member_name: &str,
) -> Option<(&'a UnitAnalysis, &'a ClassMemberData)> {
    let class_member = resolve_fallback_qualified_class_member_in_hierarchy(
        snapshot,
        class_unit,
        class_symbol_id,
        interface_name,
        member_name,
    )?;
    if let Some((interface_unit, interface_symbol)) =
        resolve_named_interface_from_qualified_class_member_with_scope_index(
            snapshot,
            scope_index,
            class_unit,
            class_symbol_id,
            scope,
            interface_name,
            member_name,
        )
        && let Some(interface_member) = interface_unit
            .semantic()
            .decls()
            .class_member(interface_symbol, member_name)
    {
        return Some((interface_unit, interface_member));
    }
    Some(class_member)
}

fn resolve_exposed_interface_handle_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    owner_unit: &'a UnitAnalysis,
    owner_symbol: SymbolId,
    scope: ScopeId,
    interface_name: &Arc<str>,
) -> Option<(&'a UnitAnalysis, SymbolId)> {
    resolve_exposed_interface_handle_inner(
        snapshot,
        scope_index,
        owner_unit,
        owner_symbol,
        scope,
        interface_name,
        &mut HashSet::new(),
    )
}

fn resolve_class_event_in_hierarchy_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    owner_unit: &'a UnitAnalysis,
    owner_symbol: SymbolId,
    scope: ScopeId,
    qualifier: Option<&str>,
    event_name: &str,
) -> Option<(&'a UnitAnalysis, &'a ClassMemberData)> {
    resolve_class_event_in_hierarchy_inner_with_scope_index(
        snapshot,
        scope_index,
        owner_unit,
        owner_symbol,
        scope,
        qualifier,
        event_name,
        &mut HashSet::new(),
    )
}

fn resolve_class_event_in_hierarchy_inner_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    owner_unit: &'a UnitAnalysis,
    owner_symbol: SymbolId,
    scope: ScopeId,
    qualifier: Option<&str>,
    event_name: &str,
    visited: &mut HashSet<(UnitId, SymbolId, Option<Arc<str>>, Arc<str>)>,
) -> Option<(&'a UnitAnalysis, &'a ClassMemberData)> {
    let key = (
        owner_unit.unit_id,
        owner_symbol,
        qualifier.map(Arc::<str>::from),
        Arc::<str>::from(event_name),
    );
    if !visited.insert(key) {
        return None;
    }

    let (direct_unit, direct_symbol) = if let Some(interface_name) = qualifier {
        let interface_name = Arc::<str>::from(interface_name.to_ascii_lowercase());
        resolve_exposed_interface_handle_with_scope_index(
            snapshot,
            scope_index,
            owner_unit,
            owner_symbol,
            scope,
            &interface_name,
        )?
    } else {
        (owner_unit, owner_symbol)
    };

    if let Some(member) = direct_unit
        .semantic()
        .decls()
        .class_member(direct_symbol, event_name)
        .filter(|member| member.kind == ClassMemberKind::Event)
    {
        return Some((direct_unit, member));
    }

    if let Some(alias) = direct_unit.member_aliases.iter().find(|alias| {
        alias.owner_symbol == direct_symbol && alias.alias_name.as_ref() == event_name
    }) {
        return resolve_class_event_in_hierarchy_inner_with_scope_index(
            snapshot,
            scope_index,
            direct_unit,
            direct_symbol,
            scope,
            Some(alias.target_interface_name.as_ref()),
            alias.target_member_name.as_ref(),
            visited,
        );
    }

    if qualifier.is_none() {
        for implemented in direct_unit
            .implemented_interfaces
            .iter()
            .filter(|implemented| implemented.owner_symbol == direct_symbol)
        {
            let Some((interface_unit, interface_symbol)) =
                resolve_exposed_interface_handle_with_scope_index(
                    snapshot,
                    scope_index,
                    direct_unit,
                    direct_symbol,
                    scope,
                    &implemented.interface_name,
                )
            else {
                continue;
            };
            if let Some(found) = resolve_class_event_in_hierarchy_inner_with_scope_index(
                snapshot,
                scope_index,
                interface_unit,
                interface_symbol,
                scope,
                None,
                event_name,
                visited,
            ) {
                return Some(found);
            }
        }
    }

    if qualifier.is_none()
        && direct_unit.symbol(direct_symbol).kind == SymbolKind::Class
        && let Some((super_unit, super_symbol)) =
            direct_superclass_from_class(snapshot, direct_unit, direct_symbol)
    {
        return resolve_class_event_in_hierarchy_inner_with_scope_index(
            snapshot,
            scope_index,
            super_unit,
            super_symbol,
            scope,
            None,
            event_name,
            visited,
        );
    }

    None
}

fn resolve_exposed_interface_handle_inner<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    owner_unit: &'a UnitAnalysis,
    owner_symbol: SymbolId,
    scope: ScopeId,
    interface_name: &Arc<str>,
    visited: &mut HashSet<(UnitId, SymbolId)>,
) -> Option<(&'a UnitAnalysis, SymbolId)> {
    if !visited.insert((owner_unit.unit_id, owner_symbol)) {
        return None;
    }

    for implemented in owner_unit
        .implemented_interfaces
        .iter()
        .filter(|implemented| implemented.owner_symbol == owner_symbol)
    {
        let Some((interface_unit, interface_symbol)) = resolve_symbol_from_context_with_scope_index(
            snapshot,
            scope_index,
            scope,
            Namespace::Type,
            &implemented.interface_name,
            false,
        ) else {
            continue;
        };
        if interface_unit.symbol(interface_symbol).kind != SymbolKind::Interface {
            continue;
        }
        if implemented.interface_name == *interface_name {
            return Some((interface_unit, interface_symbol));
        }
        if let Some(found) = resolve_exposed_interface_handle_inner(
            snapshot,
            scope_index,
            interface_unit,
            interface_symbol,
            scope,
            interface_name,
            visited,
        ) {
            return Some(found);
        }
    }

    if owner_unit.symbol(owner_symbol).kind == SymbolKind::Class
        && let Some((super_unit, super_symbol)) =
            direct_superclass_from_class(snapshot, owner_unit, owner_symbol)
    {
        return resolve_exposed_interface_handle_inner(
            snapshot,
            scope_index,
            super_unit,
            super_symbol,
            scope,
            interface_name,
            visited,
        );
    }

    None
}

fn resolve_interface_member_path<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    interface_unit: &'a UnitAnalysis,
    interface_symbol: SymbolId,
    scope: ScopeId,
    member_path: &[&str],
) -> Option<(&'a UnitAnalysis, &'a ClassMemberData)> {
    let (first, rest) = member_path.split_first()?;
    if rest.is_empty() {
        return interface_unit
            .semantic()
            .decls()
            .class_member(interface_symbol, first)
            .map(|member| (interface_unit, member));
    }

    let nested_name = Arc::<str>::from(first.to_ascii_lowercase());
    let (nested_unit, nested_symbol) = resolve_exposed_interface_handle_with_scope_index(
        snapshot,
        scope_index,
        interface_unit,
        interface_symbol,
        scope,
        &nested_name,
    )?;
    resolve_interface_member_path(
        snapshot,
        scope_index,
        nested_unit,
        nested_symbol,
        scope,
        rest,
    )
}

fn resolve_class_selector_base_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    access: &abap_symbols::FieldAccess,
    unit: &'a UnitAnalysis,
    symbol_id: SymbolId,
) -> Option<(&'a UnitAnalysis, SymbolId, bool)> {
    let base_symbol = unit.symbol(symbol_id);
    if access.base_namespace == Namespace::Type
        && matches!(base_symbol.kind, SymbolKind::Class | SymbolKind::Interface)
    {
        return Some((unit, symbol_id, base_symbol.kind == SymbolKind::Class));
    }
    if access.base_namespace == Namespace::Value
        && access.base_name.as_ref() == "super"
        && base_symbol.kind == SymbolKind::Class
    {
        return Some((unit, symbol_id, false));
    }
    if access.base_namespace != Namespace::Value {
        return None;
    }
    let declared_type = base_symbol.declared_type.as_ref()?;
    if !declared_type.is_ref || !declared_type.field_path.is_empty() {
        return None;
    }
    let (class_unit, class_symbol_id) = resolve_symbol_from_context_with_scope_index(
        snapshot,
        scope_index,
        access.scope,
        Namespace::Type,
        &declared_type.base_name,
        false,
    )?;
    matches!(
        class_unit.symbol(class_symbol_id).kind,
        SymbolKind::Class | SymbolKind::Interface
    )
    .then_some((class_unit, class_symbol_id, false))
}

fn classify_field_access_segment_with_scope_index(
    snapshot: &AnalysisSnapshot,
    scope_index: &ScopeIndex,
    access: &abap_symbols::FieldAccess,
    segment_index: usize,
) -> Option<HoveredComponentKind> {
    let (unit, symbol_id) =
        resolve_field_access_base_symbol_with_scope_index(snapshot, scope_index, access)?;
    if segment_index == 0
        && resolve_interface_selector_qualifier_with_scope_index(
            snapshot,
            scope_index,
            access,
            unit,
            symbol_id,
        )
        .is_some()
    {
        return Some(HoveredComponentKind::Interface);
    }
    if segment_index == 0
        && resolve_interface_selector_method_symbol_with_scope_index(
            snapshot,
            scope_index,
            access,
            unit,
            symbol_id,
        )
        .is_some()
    {
        return Some(HoveredComponentKind::Interface);
    }
    if let Some((_, _)) = resolve_class_selector_type_symbol_with_scope_index(
        snapshot,
        scope_index,
        access,
        segment_index,
        unit,
        symbol_id,
    ) {
        return Some(HoveredComponentKind::Type);
    }
    if let Some((_, member)) = resolve_class_selector_member_with_scope_index(
        snapshot,
        scope_index,
        access,
        segment_index,
        unit,
        symbol_id,
    ) {
        return Some(hovered_component_kind_for_class_member(member));
    }
    if segment_index == 1
        && resolve_interface_selector_method_symbol_with_scope_index(
            snapshot,
            scope_index,
            access,
            unit,
            symbol_id,
        )
        .is_some()
    {
        return Some(HoveredComponentKind::Method);
    }
    if let Some((_, kind, _)) =
        resolve_well_known_external_field_access_segment(unit, access, segment_index, symbol_id)
    {
        return Some(kind);
    }

    let (structure_unit, field) = resolve_field_access_component_with_scope_index(
        snapshot,
        scope_index,
        access,
        segment_index,
        unit,
        symbol_id,
    )?;
    Some(match field.shape {
        StructureFieldShape::Scalar => HoveredComponentKind::Scalar,
        StructureFieldShape::Structured { structure } => HoveredComponentKind::Structured {
            structure_name: Arc::clone(&structure_unit.structure(structure).name),
        },
    })
}

fn innermost_scope_at(unit: &UnitAnalysis, offset: usize) -> ScopeId {
    unit.scopes
        .iter()
        .filter(|scope| scope.range.start <= offset && offset <= scope.range.end)
        .min_by_key(|scope| scope.range.end.saturating_sub(scope.range.start))
        .map(|scope| scope.id)
        .unwrap_or(unit.root_scope)
}

fn enclosing_method_scope_with_owner(unit: &UnitAnalysis, scope: ScopeId) -> Option<ScopeId> {
    let mut current = Some(scope);
    while let Some(scope_id) = current {
        let current_scope = unit.scope(scope_id);
        if current_scope.kind == ScopeKind::Method && current_scope.owner.is_some() {
            return Some(scope_id);
        }
        current = current_scope.parent;
    }
    None
}

fn selector_completion_context(
    parse: &ParseResult,
    offset: usize,
) -> Option<SelectorCursorContext> {
    let root = parse.file.root();
    let root_range = parse.file.range(root);
    let path = if root_range.start <= offset && offset <= root_range.end {
        let mut path = vec![root];
        let mut current = root;
        while let Some(next) = child_at_offset_prefer_left_boundary(parse, current, offset) {
            path.push(next);
            current = next;
        }
        path
    } else {
        Vec::new()
    };

    if let Some(type_ref) = path
        .iter()
        .rev()
        .copied()
        .find(|&node| parse.file.kind(node).as_str() == "TypeRefSimple")
    {
        return Some(SelectorCursorContext {
            range: parse.file.range(type_ref),
            in_type_position: true,
        });
    }

    let container = path
        .iter()
        .rev()
        .copied()
        .find(|&node| is_selector_query_container(parse.file.kind(node).as_str()))?;
    Some(SelectorCursorContext {
        range: parse.file.range(container),
        in_type_position: false,
    })
}

fn node_path_at_offset(parse: &ParseResult, offset: usize) -> Vec<abap_ast::arena::NodeId> {
    let root = parse.file.root();
    let root_range = parse.file.range(root);
    if !(root_range.start <= offset && offset <= root_range.end) {
        return Vec::new();
    }

    let mut path = vec![root];
    let mut current = root;
    while let Some(next) = child_at_offset_prefer_left_boundary(parse, current, offset) {
        path.push(next);
        current = next;
    }
    path
}

fn offset_is_in_error_node(parse: &ParseResult, offset: usize) -> bool {
    node_path_at_offset(parse, offset)
        .into_iter()
        .any(|node| parse.file.kind(node) == SyntaxKind::Error)
}

fn child_at_offset_prefer_left_boundary(
    parse: &ParseResult,
    parent: abap_ast::arena::NodeId,
    offset: usize,
) -> Option<abap_ast::arena::NodeId> {
    parse
        .file
        .children(parent)
        .filter(|&child| {
            let range = parse.file.range(child);
            range.start <= offset && offset <= range.end
        })
        .min_by_key(|&child| {
            let range = parse.file.range(child);
            (range.start == offset, range.end.saturating_sub(range.start))
        })
}

fn corresponding_constructor_source_query(
    text: &str,
    parse: &ParseResult,
    constructor: ConstructorExpr<'_>,
) -> Option<SelectorCompletionQuery> {
    let arg_list = constructor.arg_list()?;
    let source_arg = arg_list.positional_args().next()?;
    let range = source_arg.syntax().range();
    let (token_start, token_end) = token_window_for_range(parse, &range)?;
    let token_ids: Vec<_> = (token_start..token_end)
        .filter(|&idx| !matches!(parse.tokens[idx].kind.as_str(), "Comment" | "Eof"))
        .collect();
    parse_value_access_tokens(text, parse, &token_ids)
}

fn corresponding_constructor_target_query(
    text: &str,
    parse: &ParseResult,
    constructor: ConstructorExpr<'_>,
) -> Option<SelectorCompletionQuery> {
    let type_ref = constructor.type_ref()?;
    let range = type_ref.syntax().range();
    let (token_start, token_end) = token_window_for_range(parse, &range)?;
    let token_ids: Vec<_> = (token_start..token_end)
        .filter(|&idx| !matches!(parse.tokens[idx].kind.as_str(), "Comment" | "Eof"))
        .collect();
    let mut query = parse_value_access_tokens(text, parse, &token_ids)?;
    query.base_namespace = Namespace::Type;
    query.in_type_position = false;
    Some(query)
}

fn corresponding_relative_component_names(
    text: &str,
    parse: &ParseResult,
    range: &Range<usize>,
) -> Option<Vec<Arc<str>>> {
    let (token_start, token_end) = token_window_for_range(parse, range)?;
    let token_ids: Vec<_> = (token_start..token_end)
        .filter(|&idx| !matches!(parse.tokens[idx].kind.as_str(), "Comment" | "Eof"))
        .collect();
    let query = parse_value_access_tokens(text, parse, &token_ids)?;
    let mut names = Vec::with_capacity(query.component_path.len() + 1);
    names.push(query.base_name);
    names.extend(query.component_path);
    Some(names)
}

fn corresponding_component_completion_prefix(
    text: &str,
    parse: &ParseResult,
    range: &Range<usize>,
    offset: usize,
) -> Option<(Vec<Arc<str>>, Range<usize>, Arc<str>)> {
    let (token_start, token_end) = token_window_for_range(parse, range)?;
    let prefix_idx = prefix_token_at_offset(parse, token_start, token_end, offset)?;
    let prefix_token = &parse.tokens[prefix_idx];
    if prefix_token.kind.as_str() != "Ident" {
        return None;
    }

    let significant: Vec<_> = (token_start..token_end)
        .filter(|&idx| !matches!(parse.tokens[idx].kind.as_str(), "Comment" | "Eof"))
        .collect();
    let ident_positions: Vec<_> = significant
        .iter()
        .copied()
        .filter(|&idx| parse.tokens[idx].kind.as_str() == "Ident")
        .collect();
    let prefix_pos = ident_positions.iter().position(|&idx| idx == prefix_idx)?;
    let path = ident_positions[..prefix_pos]
        .iter()
        .map(|&idx| Arc::<str>::from(parse.tokens[idx].lexeme(text).to_ascii_lowercase()))
        .collect::<Vec<_>>();
    let prefix_end = offset.min(prefix_token.range.end);
    Some((
        path,
        prefix_token.range.start..prefix_end,
        Arc::<str>::from(text[prefix_token.range.start..prefix_end].to_ascii_lowercase()),
    ))
}

fn parse_corresponding_mapping_field_query(
    text: &str,
    parse: &ParseResult,
    offset: usize,
) -> Option<SelectorCompletionQuery> {
    let path = node_path_at_offset(parse, offset);
    let assignment_id = path.iter().rev().copied().find(|&node| {
        parse.file.kind(node) == SyntaxKind::ConstructorCorrespondingMappingAssignment
    })?;
    let constructor_id = path.iter().rev().copied().find(|&node| {
        ConstructorExpr::cast(SyntaxNodeRef::new(&parse.file, node))
            .and_then(|expr| expr.keyword(text))
            .is_some_and(|keyword| keyword.as_ref() == "corresponding")
    })?;
    let assignment_chain: Vec<_> = path
        .iter()
        .copied()
        .filter(|&node| {
            parse.file.kind(node) == SyntaxKind::ConstructorCorrespondingMappingAssignment
        })
        .collect();

    let constructor = ConstructorExpr::cast(SyntaxNodeRef::new(&parse.file, constructor_id))?;
    let assignment = ConstructorCorrespondingMappingAssignment::cast(SyntaxNodeRef::new(
        &parse.file,
        assignment_id,
    ))?;
    let target_token = assignment.target_token()?;
    let target_range = target_token.range();
    let source_range = assignment.source_value(text).map(|value| value.range());

    let in_target = target_range.start <= offset && offset <= target_range.end;
    let in_source = source_range
        .as_ref()
        .is_some_and(|range| range.start <= offset && offset <= range.end);
    if !in_target && !in_source {
        return None;
    }

    let mut target_query = corresponding_constructor_target_query(text, parse, constructor)?;
    let mut source_query = corresponding_constructor_source_query(text, parse, constructor)?;

    for ancestor_id in assignment_chain {
        if ancestor_id == assignment_id {
            break;
        }
        let ancestor = ConstructorCorrespondingMappingAssignment::cast(SyntaxNodeRef::new(
            &parse.file,
            ancestor_id,
        ))?;
        let ancestor_target = ancestor.target_token()?;
        let ancestor_target_name =
            Arc::<str>::from(ancestor_target.text(text)?.to_ascii_lowercase());
        target_query.component_path.push(ancestor_target_name);

        let ancestor_source_range = ancestor.source_value(text)?.range();
        source_query
            .component_path
            .extend(corresponding_relative_component_names(
                text,
                parse,
                &ancestor_source_range,
            )?);
    }

    if in_target {
        let (_, replace_range, prefix) =
            corresponding_component_completion_prefix(text, parse, &target_range, offset)?;
        target_query.replace_range = replace_range;
        target_query.prefix = prefix;
        return Some(target_query);
    }

    let source_range = source_range?;
    let (relative_path, replace_range, prefix) =
        corresponding_component_completion_prefix(text, parse, &source_range, offset)?;
    source_query.component_path.extend(relative_path);
    source_query.replace_range = replace_range;
    source_query.prefix = prefix;
    Some(source_query)
}

fn selector_completion_statement_context(
    parse: &ParseResult,
    offset: usize,
) -> Option<SelectorCursorContext> {
    let anchor = parse
        .tokens
        .iter()
        .position(|token| token.kind.as_str() != "Eof" && token.range.end >= offset)
        .or_else(|| {
            parse
                .tokens
                .iter()
                .rposition(|token| !matches!(token.kind.as_str(), "Comment" | "Eof"))
        })?;

    let start = (0..=anchor)
        .rev()
        .find(|&idx| parse.tokens[idx].kind.as_str() == "Period")
        .map(|idx| idx + 1)
        .unwrap_or(0);
    let end = (anchor..parse.tokens.len())
        .find(|&idx| parse.tokens[idx].kind.as_str() == "Period")
        .or_else(|| previous_significant_token(parse, start, parse.tokens.len()))?;
    let start_token = parse.tokens.get(start)?;
    let end_token = parse.tokens.get(end)?;
    Some(SelectorCursorContext {
        range: start_token.range.start..end_token.range.end,
        in_type_position: false,
    })
}

fn statement_query_range(parse: &ParseResult, offset: usize) -> Option<Range<usize>> {
    selector_completion_statement_context(parse, offset).map(|mut context| {
        if offset > context.range.end {
            context.range.end = offset;
        }
        context.range
    })
}

fn significant_statement_tokens(parse: &ParseResult, range: &Range<usize>) -> Option<Vec<usize>> {
    let (token_start, token_end) = token_window_for_range(parse, range)?;
    Some(
        (token_start..token_end)
            .filter(|&idx| !matches!(parse.tokens[idx].kind.as_str(), "Comment" | "Eof"))
            .collect(),
    )
}

fn string_literal_content_range(text: &str, token: &abap_lexer::Token) -> Option<Range<usize>> {
    if token.kind != TokenKind::String {
        return None;
    }
    let lexeme = text.get(token.range.clone())?;
    let quote = lexeme.chars().next()?;
    if quote != '\'' && quote != '`' {
        return None;
    }
    let start = token.range.start + quote.len_utf8();
    let end = if lexeme.ends_with(quote) && token.range.end > start {
        token.range.end - quote.len_utf8()
    } else {
        token.range.end
    };
    Some(start..end)
}

fn line_end_offset(text: &str, offset: usize) -> usize {
    let mut end = text
        .get(offset..)
        .and_then(|tail| tail.find('\n').map(|idx| offset + idx))
        .unwrap_or(text.len());
    if end > offset && text.as_bytes().get(end - 1) == Some(&b'\r') {
        end -= 1;
    }
    end
}

struct ClassImplementationEditTarget {
    edit_range: Range<usize>,
    kind: ClassImplementationEditKind,
}

enum ClassImplementationEditKind {
    ExistingBody { body_is_empty: bool },
    MissingBlock,
}

fn class_implementation_edit_target(
    parse: &ParseResult,
    text: &str,
    class_name: &str,
) -> Option<ClassImplementationEditTarget> {
    let root = parse.file.root();
    let mut definition_end = None;
    for child in parse.file.children(root) {
        let range = parse.file.range(child);
        let (token_start, token_end) = token_window_for_range(parse, &range)?;
        let header_period =
            (token_start..token_end).find(|&idx| parse.tokens[idx].kind == TokenKind::Period)?;
        let header_tokens: Vec<_> = (token_start..=header_period)
            .filter(|&idx| !matches!(parse.tokens[idx].kind.as_str(), "Comment" | "Eof"))
            .collect();
        if header_tokens.len() < 3 {
            continue;
        }

        let class_token = &parse.tokens[header_tokens[0]];
        let name_token = &parse.tokens[header_tokens[1]];
        let implementation_token = &parse.tokens[header_tokens[2]];
        if !class_token.lexeme(text).eq_ignore_ascii_case("class")
            || name_token.kind != TokenKind::Ident
            || !name_token.lexeme(text).eq_ignore_ascii_case(class_name)
        {
            continue;
        }

        if implementation_token
            .lexeme(text)
            .eq_ignore_ascii_case("definition")
        {
            let final_period_idx = (token_start..token_end)
                .rfind(|&idx| parse.tokens[idx].kind == TokenKind::Period)?;
            definition_end = Some(parse.tokens[final_period_idx].range.end);
            continue;
        }

        if !implementation_token
            .lexeme(text)
            .eq_ignore_ascii_case("implementation")
        {
            continue;
        }

        let endclass_idx = (token_start..token_end).rfind(|&idx| {
            parse.tokens[idx].kind == TokenKind::Ident
                && parse.tokens[idx]
                    .lexeme(text)
                    .eq_ignore_ascii_case("endclass")
        })?;
        let body_range =
            parse.tokens[header_period].range.end..parse.tokens[endclass_idx].range.start;
        return Some(ClassImplementationEditTarget {
            edit_range: parse.tokens[endclass_idx].range.start
                ..parse.tokens[endclass_idx].range.start,
            kind: ClassImplementationEditKind::ExistingBody {
                body_is_empty: text[body_range].trim().is_empty(),
            },
        });
    }

    let definition_end = definition_end?;
    let gap_end = blank_line_gap_end(text, definition_end);
    Some(ClassImplementationEditTarget {
        edit_range: definition_end..gap_end,
        kind: ClassImplementationEditKind::MissingBlock,
    })
}

fn build_missing_method_implementation_text(
    text: &str,
    method_name: &str,
    body_is_empty: bool,
) -> String {
    let newline = if text.contains("\r\n") { "\r\n" } else { "\n" };
    let prefix = if body_is_empty { "" } else { newline };
    format!("{prefix}  METHOD {method_name}.{newline}  ENDMETHOD.{newline}")
}

fn build_missing_class_implementation_text(
    text: &str,
    class_name: &str,
    method_name: &str,
    has_following_content: bool,
) -> String {
    let newline = if text.contains("\r\n") { "\r\n" } else { "\n" };
    let suffix = if has_following_content {
        format!("{newline}{newline}")
    } else {
        newline.to_string()
    };
    format!(
        "{newline}{newline}CLASS {class_name} IMPLEMENTATION.{newline}  METHOD {method_name}.{newline}  ENDMETHOD.{newline}ENDCLASS.{suffix}"
    )
}

fn blank_line_gap_end(text: &str, offset: usize) -> usize {
    let bytes = text.as_bytes();
    let len = bytes.len();
    let mut idx = offset;

    if idx >= len {
        return idx;
    }
    if bytes[idx] == b'\r' && bytes.get(idx + 1) == Some(&b'\n') {
        idx += 2;
    } else if bytes[idx] == b'\n' {
        idx += 1;
    } else {
        return idx;
    }

    loop {
        let line_start = idx;
        while idx < len && matches!(bytes[idx], b' ' | b'\t') {
            idx += 1;
        }
        if idx >= len {
            return idx;
        }
        if bytes[idx] == b'\r' && bytes.get(idx + 1) == Some(&b'\n') {
            idx += 2;
            continue;
        }
        if bytes[idx] == b'\n' {
            idx += 1;
            continue;
        }
        return line_start;
    }
}

fn parse_function_module_completion_query(
    snapshot: &AnalysisSnapshot,
    offset: usize,
) -> Option<CallableStatementCompletionQuery> {
    let statement_range = statement_query_range(&snapshot.parse, offset)?;
    let significant = significant_statement_tokens(&snapshot.parse, &statement_range)?;
    if significant.len() < 3 {
        return None;
    }
    let call_idx = significant[0];
    let function_idx = significant[1];
    let target_idx = significant[2];
    let call_token = &snapshot.parse.tokens[call_idx];
    let function_token = &snapshot.parse.tokens[function_idx];
    let target = &snapshot.parse.tokens[target_idx];
    if !call_token
        .lexeme(snapshot.text.as_ref())
        .eq_ignore_ascii_case("call")
        || !function_token
            .lexeme(snapshot.text.as_ref())
            .eq_ignore_ascii_case("function")
    {
        return None;
    }

    let line_end = line_end_offset(snapshot.text.as_ref(), target.range.start);

    if significant.iter().copied().skip(3).any(|idx| {
        let token = &snapshot.parse.tokens[idx];
        token.range.start < line_end && token.kind != TokenKind::Period
    }) {
        return None;
    }

    let content_range = string_literal_content_range(snapshot.text.as_ref(), target)?;
    if offset < content_range.start || offset > content_range.end {
        return None;
    }

    let prefix_end = offset.min(content_range.end);
    Some(CallableStatementCompletionQuery {
        scope: innermost_scope_at(&snapshot.symbols, statement_range.start),
        replace_range: content_range.start..line_end,
        prefix: Arc::from(snapshot.text[content_range.start..prefix_end].to_ascii_lowercase()),
        kind: CallableCompletionKind::FunctionModule,
    })
}

fn parse_form_completion_query(
    snapshot: &AnalysisSnapshot,
    offset: usize,
) -> Option<CallableStatementCompletionQuery> {
    let statement_range = statement_query_range(&snapshot.parse, offset)?;
    let significant = significant_statement_tokens(&snapshot.parse, &statement_range)?;
    let perform_idx = *significant.first()?;
    let perform_token = &snapshot.parse.tokens[perform_idx];
    if !perform_token
        .lexeme(snapshot.text.as_ref())
        .eq_ignore_ascii_case("perform")
    {
        return None;
    }

    let target_idx = significant
        .get(1)
        .copied()
        .filter(|&idx| snapshot.parse.tokens[idx].kind == TokenKind::Ident);

    let (replace_range, prefix, remaining_start) = if let Some(target_idx) = target_idx {
        let target = &snapshot.parse.tokens[target_idx];
        let line_end = line_end_offset(snapshot.text.as_ref(), target.range.start);
        if offset < target.range.start || offset > target.range.end {
            return None;
        }
        let prefix_end = offset.min(target.range.end);
        (
            target.range.start..line_end,
            Arc::from(snapshot.text[target.range.start..prefix_end].to_ascii_lowercase()),
            2usize,
        )
    } else {
        let line_end = line_end_offset(snapshot.text.as_ref(), perform_token.range.start);
        if offset < perform_token.range.end || offset > line_end {
            return None;
        }
        (offset..line_end, Arc::<str>::from(""), 1usize)
    };

    let line_end = replace_range.end;

    if significant
        .iter()
        .copied()
        .skip(remaining_start)
        .any(|idx| {
            let token = &snapshot.parse.tokens[idx];
            token.range.start < line_end && token.kind != TokenKind::Period
        })
    {
        return None;
    }

    Some(CallableStatementCompletionQuery {
        scope: innermost_scope_at(&snapshot.symbols, statement_range.start),
        replace_range,
        prefix,
        kind: CallableCompletionKind::Form,
    })
}

fn parse_local_class_template_query(
    snapshot: &AnalysisSnapshot,
    offset: usize,
) -> Option<TemplateCompletionQuery> {
    let statement_range = statement_query_range(&snapshot.parse, offset)?;
    let significant = significant_statement_tokens(&snapshot.parse, &statement_range)?;
    let token_idx = significant.iter().copied().find(|&idx| {
        let token = &snapshot.parse.tokens[idx];
        token.kind == TokenKind::Ident && token.range.start <= offset && offset <= token.range.end
    })?;
    let token = &snapshot.parse.tokens[token_idx];
    if token.kind != TokenKind::Ident || offset < token.range.start || offset > token.range.end {
        return None;
    }
    let line_end = line_end_offset(snapshot.text.as_ref(), token.range.start);
    if significant
        .iter()
        .copied()
        .any(|idx| idx != token_idx && snapshot.parse.tokens[idx].range.start < line_end)
    {
        return None;
    }

    let prefix_end = offset.min(token.range.end);
    let class_name_hint = snapshot.text[token.range.start..prefix_end].trim();
    if class_name_hint.is_empty() {
        return None;
    }
    let lower = class_name_hint.to_ascii_lowercase();
    let kind = if lower.starts_with("ltcl") {
        LocalClassTemplateKind::Test
    } else if lower.starts_with("lcl") {
        LocalClassTemplateKind::Standard
    } else {
        return None;
    };

    Some(TemplateCompletionQuery {
        replace_range: token.range.start..line_end,
        class_name_hint: Arc::from(class_name_hint),
        kind,
    })
}

fn parse_method_definition_template_query(
    snapshot: &AnalysisSnapshot,
    offset: usize,
) -> Option<MethodDefinitionTemplateQuery> {
    let statement_range = statement_query_range(&snapshot.parse, offset)?;
    if !inside_class_definition_at(snapshot, statement_range.start) {
        return None;
    }

    let significant = significant_statement_tokens(&snapshot.parse, &statement_range)?;
    let token = &snapshot.parse.tokens[*significant.first()?];
    if token.kind != TokenKind::Ident || offset < token.range.start || offset > token.range.end {
        return None;
    }
    let line_end = line_end_offset(snapshot.text.as_ref(), token.range.start);
    if significant.iter().copied().skip(1).any(|idx| {
        let next = &snapshot.parse.tokens[idx];
        next.range.start < line_end && next.kind != TokenKind::Period
    }) {
        return None;
    }

    let prefix_end = offset.min(token.range.end);
    let prefix = snapshot.text[token.range.start..prefix_end].trim();
    if prefix.len() < 4 {
        return None;
    }
    let lower = prefix.to_ascii_lowercase();
    if !"methods".starts_with(lower.as_str()) {
        return None;
    }

    Some(MethodDefinitionTemplateQuery {
        replace_range: token.range.start..line_end,
    })
}

fn parse_types_begin_template_query(
    snapshot: &AnalysisSnapshot,
    offset: usize,
) -> Option<TypesBeginTemplateQuery> {
    let statement_range = statement_query_range(&snapshot.parse, offset)?;
    let significant = significant_statement_tokens(&snapshot.parse, &statement_range)?;
    let first_idx = *significant.first()?;
    let first = &snapshot.parse.tokens[first_idx];
    if first.kind != TokenKind::Ident
        || !first
            .lexeme(snapshot.text.as_ref())
            .eq_ignore_ascii_case("types")
    {
        return None;
    }
    let colon_pos = significant
        .iter()
        .position(|&idx| snapshot.parse.tokens[idx].kind == TokenKind::Colon)?;

    let token_idx = significant.iter().copied().rev().find(|&idx| {
        let token = &snapshot.parse.tokens[idx];
        token.kind == TokenKind::Ident
            && token.range.start <= offset
            && offset <= line_end_offset(snapshot.text.as_ref(), token.range.start)
    })?;
    if significant.iter().position(|&idx| idx == token_idx)? <= colon_pos {
        return None;
    }

    let token = &snapshot.parse.tokens[token_idx];
    let previous_idx = previous_significant_token(&snapshot.parse, first_idx, token_idx)?;
    if !matches!(
        snapshot.parse.tokens[previous_idx].kind,
        TokenKind::Colon | TokenKind::Comma
    ) {
        return None;
    }

    let line_end = line_end_offset(snapshot.text.as_ref(), token.range.start);
    if significant.iter().copied().any(|idx| {
        idx != token_idx && {
            let other = &snapshot.parse.tokens[idx];
            token.range.start < other.range.start && other.range.start < line_end
        }
    }) {
        return None;
    }

    let prefix_end = offset.min(token.range.end);
    let prefix = snapshot.text[token.range.start..prefix_end].trim();
    if prefix.is_empty() {
        return None;
    }
    let lower = prefix.to_ascii_lowercase();
    if !"begin".starts_with(lower.as_str()) {
        return None;
    }

    Some(TypesBeginTemplateQuery {
        replace_range: token.range.start..line_end,
    })
}

fn inside_class_definition_at(snapshot: &AnalysisSnapshot, offset: usize) -> bool {
    let Some(statement_range) = statement_query_range(&snapshot.parse, offset) else {
        return false;
    };
    let Some((target_token_start, _)) = token_window_for_range(&snapshot.parse, &statement_range)
    else {
        return false;
    };

    let tokens = snapshot.parse.tokens.as_ref();
    let text = snapshot.text.as_ref();
    let mut stack = Vec::<DependencyBlock>::new();
    let mut idx = 0usize;

    while idx < target_token_start {
        while idx < target_token_start && tokens[idx].kind == TokenKind::Comment {
            idx += 1;
        }
        if idx >= target_token_start {
            break;
        }

        let Some(period_idx) = tokens[idx..]
            .iter()
            .position(|token| token.kind == TokenKind::Period)
            .map(|offset| idx + offset)
        else {
            break;
        };
        if period_idx >= target_token_start {
            break;
        }

        let keywords = statement_keywords(tokens, text, idx, period_idx);
        let first = keywords.first().map(String::as_str);
        let second = keywords.get(1).map(String::as_str);

        match stack.last_mut() {
            Some(DependencyBlock::Method) => {
                if first == Some("endmethod") {
                    stack.pop();
                }
            }
            Some(DependencyBlock::Form) => {
                if first == Some("endform") {
                    stack.pop();
                }
            }
            Some(DependencyBlock::Function) => {
                if first == Some("endfunction") {
                    stack.pop();
                }
            }
            Some(DependencyBlock::ClassImplementation) => match first {
                Some("method") => stack.push(DependencyBlock::Method),
                Some("endclass") => {
                    stack.pop();
                }
                _ => {}
            },
            Some(DependencyBlock::ClassDefinition { visibility }) => {
                if first == Some("endclass") {
                    stack.pop();
                    idx = period_idx + 1;
                    continue;
                }

                if matches!(first, Some("public" | "protected" | "private"))
                    && second == Some("section")
                {
                    *visibility = match first.expect("section keyword") {
                        "public" => DependencyVisibility::Public,
                        "protected" => DependencyVisibility::Protected,
                        _ => DependencyVisibility::Private,
                    };
                    idx = period_idx + 1;
                    continue;
                }

                if let Some(block) = dependency_class_block_for_keywords(&keywords) {
                    stack.push(block);
                } else if let Some(block) = dependency_block_for_keywords(&keywords) {
                    stack.push(block);
                }
            }
            None => match first {
                Some("class") => {
                    if let Some(block) = dependency_class_block_for_keywords(&keywords) {
                        stack.push(block);
                    }
                }
                _ => {
                    if let Some(block) = dependency_block_for_keywords(&keywords) {
                        stack.push(block);
                    }
                }
            },
        }

        idx = period_idx + 1;
    }

    matches!(stack.last(), Some(DependencyBlock::ClassDefinition { .. }))
}

fn is_selector_query_container(kind: &str) -> bool {
    matches!(
        kind,
        "SelectorExpr"
            | "CallExpr"
            | "ConstructorExpr"
            | "CharStringTemplate"
            | "TemplateInterpolation"
            | "TemplateExpr"
            | "BinaryExpr"
            | "UnaryExpr"
            | "ParenExpr"
            | "IsPredicate"
            | "InstanceOfPredicate"
            | "BetweenExpr"
            | "AssignStmt"
            | "UnparsedStmt"
            | "AliasesStmt"
            | "ClearStmt"
            | "CallStmt"
            | "ConvertStmt"
            | "DescribeStmt"
            | "MethodsStmt"
            | "MoveCorrespondingStmt"
            | "MoveStmt"
            | "ReplaceStmt"
            | "SortStmt"
            | "CloseCursorStmt"
            | "FetchCursorStmt"
            | "SetPfStatusStmt"
            | "SetTitlebarStmt"
            | "AssertStmt"
            | "CheckStmt"
            | "PerformStmt"
            | "CreateObjectStmt"
            | "CallMethodStmt"
            | "RaiseStmt"
            | "MessageStmt"
            | "EndAtStmt"
            | "FindStmt"
            | "Error"
            | "WaitStmt"
            | "WriteStmt"
            | "ReadTableStmt"
            | "SelectStmt"
            | "IfStmt"
            | "ElseifClause"
            | "ElseClause"
            | "CaseStmt"
            | "WhenClause"
            | "WhileStmt"
            | "DoStmt"
            | "LoopStmt"
            | "TryStmt"
            | "CatchClause"
            | "CleanupClause"
    )
}

fn parse_selector_completion_query(
    text: &str,
    parse: &ParseResult,
    offset: usize,
    context: &SelectorCursorContext,
) -> Option<SelectorCompletionQuery> {
    if offset > text.len() || offset < context.range.start || offset > context.range.end {
        return None;
    }

    let (token_start, token_end) = token_window_for_range(parse, &context.range)?;
    let prefix_token = prefix_token_at_offset(parse, token_start, token_end, offset);
    let (replace_range, prefix, cursor) = if let Some(prefix_idx) = prefix_token {
        let prefix_token = &parse.tokens[prefix_idx];
        let prefix_end = offset.min(prefix_token.range.end);
        (
            prefix_token.range.start..prefix_end,
            Arc::<str>::from(text[prefix_token.range.start..prefix_end].to_ascii_lowercase()),
            prefix_idx,
        )
    } else {
        (
            offset..offset,
            Arc::<str>::from(""),
            first_token_starting_at_or_after(parse, token_start, token_end, offset),
        )
    };
    let mut reversed_segments = Vec::new();
    let mut cursor = cursor;

    loop {
        // The completion scanner skips over bracket groups so `itab[ ... ]-field` behaves like a
        // selector on the selected line type, while legacy `itab[]` is ignored as non-selector.
        let (op_idx, op_kind) = selector_operator_before_token(parse, token_start, cursor)?;
        let ident_idx = previous_selector_significant_token(parse, token_start, op_idx)?;
        let ident = &parse.tokens[ident_idx];
        if ident.kind.as_str() != "Ident" {
            return None;
        }
        reversed_segments.push(Arc::<str>::from(
            text[ident.range.start..ident.range.end].to_ascii_lowercase(),
        ));
        cursor = ident_idx;

        if selector_operator_before_token(parse, token_start, cursor).is_none() {
            let base_name = reversed_segments.pop()?;
            reversed_segments.reverse();
            let base_namespace = match op_kind {
                SelectorOperator::FatArrow => Namespace::Type,
                _ => Namespace::Value,
            };
            return Some(SelectorCompletionQuery {
                scope: ScopeId(0),
                base_name,
                base_namespace,
                component_path: reversed_segments,
                replace_range,
                prefix,
                in_type_position: context.in_type_position
                    || type_keyword_before_base(parse, text, token_start, cursor),
            });
        }
    }
}

#[derive(Debug, Clone)]
struct ParsedOpenSqlSource {
    name: Arc<str>,
    alias: Option<Arc<str>>,
}

fn open_sql_token_is_keyword(text: &str, parse: &ParseResult, idx: usize, keyword: &str) -> bool {
    parse.tokens[idx].kind.as_str() == "Ident"
        && parse.tokens[idx].lexeme(text).eq_ignore_ascii_case(keyword)
}

fn open_sql_clause_keyword(text: &str, parse: &ParseResult, idx: usize) -> bool {
    if parse.tokens[idx].kind.as_str() != "Ident" {
        return false;
    }
    matches!(
        parse.tokens[idx].lexeme(text).to_ascii_lowercase().as_str(),
        "fields"
            | "where"
            | "into"
            | "appending"
            | "group"
            | "having"
            | "order"
            | "for"
            | "up"
            | "package"
            | "offset"
            | "bypassing"
            | "connection"
            | "client"
            | "union"
            | "intersect"
            | "except"
    )
}

fn open_sql_join_starts(text: &str, parse: &ParseResult, idx: usize) -> bool {
    open_sql_token_is_keyword(text, parse, idx, "join")
}

fn open_sql_source_end(
    text: &str,
    parse: &ParseResult,
    significant: &[usize],
    start_sig: usize,
) -> usize {
    significant
        .iter()
        .enumerate()
        .skip(start_sig)
        .find_map(|(pos, &idx)| {
            (open_sql_clause_keyword(text, parse, idx)
                || open_sql_join_starts(text, parse, idx)
                || open_sql_token_is_keyword(text, parse, idx, "on"))
            .then_some(pos)
        })
        .unwrap_or(significant.len())
}

fn parse_open_sql_sources(
    text: &str,
    parse: &ParseResult,
    significant: &[usize],
) -> Vec<ParsedOpenSqlSource> {
    let mut sources = Vec::new();
    let mut sig = 0usize;
    while sig < significant.len() {
        let idx = significant[sig];
        let source_start_sig = if open_sql_token_is_keyword(text, parse, idx, "from")
            || open_sql_token_is_keyword(text, parse, idx, "join")
        {
            Some(sig + 1)
        } else {
            None
        };
        let Some(source_start_sig) = source_start_sig else {
            sig += 1;
            continue;
        };
        let Some(&source_idx) = significant.get(source_start_sig) else {
            break;
        };
        if parse.tokens[source_idx].kind.as_str() != "Ident" {
            sig = source_start_sig + 1;
            continue;
        }
        let source_end_sig = open_sql_source_end(text, parse, significant, source_start_sig + 1);
        let mut alias = None;
        if source_start_sig + 2 < source_end_sig {
            let maybe_as_idx = significant[source_start_sig + 1];
            let maybe_alias_idx = significant[source_start_sig + 2];
            if open_sql_token_is_keyword(text, parse, maybe_as_idx, "as")
                && parse.tokens[maybe_alias_idx].kind.as_str() == "Ident"
            {
                alias = Some(Arc::<str>::from(
                    parse.tokens[maybe_alias_idx]
                        .lexeme(text)
                        .to_ascii_lowercase(),
                ));
            }
        } else if source_start_sig + 1 < source_end_sig {
            let maybe_alias_idx = significant[source_start_sig + 1];
            if parse.tokens[maybe_alias_idx].kind.as_str() == "Ident"
                && !open_sql_clause_keyword(text, parse, maybe_alias_idx)
            {
                alias = Some(Arc::<str>::from(
                    parse.tokens[maybe_alias_idx]
                        .lexeme(text)
                        .to_ascii_lowercase(),
                ));
            }
        }
        sources.push(ParsedOpenSqlSource {
            name: Arc::<str>::from(parse.tokens[source_idx].lexeme(text).to_ascii_lowercase()),
            alias,
        });
        sig = source_end_sig.max(source_start_sig + 1);
    }
    sources
}

fn open_sql_significant_tokens(
    parse: &ParseResult,
    token_start: usize,
    token_end: usize,
) -> Vec<usize> {
    (token_start..token_end)
        .filter(|&idx| !matches!(parse.tokens[idx].kind.as_str(), "Comment" | "Eof"))
        .collect()
}

fn open_sql_first_select_sig(
    text: &str,
    parse: &ParseResult,
    significant: &[usize],
) -> Option<usize> {
    significant
        .iter()
        .position(|&idx| open_sql_token_is_keyword(text, parse, idx, "select"))
}

fn open_sql_sig_at_or_after_offset(
    parse: &ParseResult,
    significant: &[usize],
    offset: usize,
) -> usize {
    significant
        .iter()
        .position(|&idx| parse.tokens[idx].range.start >= offset)
        .unwrap_or(significant.len())
}

fn open_sql_prefix_at_offset(
    text: &str,
    parse: &ParseResult,
    significant: &[usize],
    offset: usize,
) -> (Range<usize>, Arc<str>, usize) {
    let prefix_sig = significant.iter().position(|&idx| {
        let token = &parse.tokens[idx];
        token.kind.as_str() == "Ident" && token.range.start <= offset && offset <= token.range.end
    });
    if let Some(prefix_sig) = prefix_sig {
        let token = &parse.tokens[significant[prefix_sig]];
        let prefix_end = offset.min(token.range.end);
        return (
            token.range.start..prefix_end,
            Arc::<str>::from(text[token.range.start..prefix_end].to_ascii_lowercase()),
            prefix_sig,
        );
    }
    let insertion_sig = open_sql_sig_at_or_after_offset(parse, significant, offset);
    (offset..offset, Arc::<str>::from(""), insertion_sig)
}

fn open_sql_field_source_name(
    text: &str,
    parse: &ParseResult,
    significant: &[usize],
    prefix_sig: usize,
    sources: &[ParsedOpenSqlSource],
) -> Option<Arc<str>> {
    if prefix_sig >= 2 {
        let prev_idx = significant[prefix_sig - 1];
        let qualifier_idx = significant[prefix_sig - 2];
        if parse.tokens[prev_idx].kind.as_str() == "Tilde"
            && parse.tokens[qualifier_idx].kind.as_str() == "Ident"
        {
            let qualifier = parse.tokens[qualifier_idx]
                .lexeme(text)
                .to_ascii_lowercase();
            return sources
                .iter()
                .find(|source| {
                    source.name.as_ref() == qualifier
                        || source
                            .alias
                            .as_ref()
                            .is_some_and(|alias| alias.as_ref() == qualifier)
                })
                .map(|source| Arc::clone(&source.name));
        }
    }

    (sources.len() == 1).then(|| Arc::clone(&sources[0].name))
}

fn open_sql_offset_in_source_position(
    text: &str,
    parse: &ParseResult,
    significant: &[usize],
    offset: usize,
) -> bool {
    let (_, _, sig_at_offset) = open_sql_prefix_at_offset(text, parse, significant, offset);
    let prev_sig = sig_at_offset.checked_sub(1);
    if let Some(prev_sig) = prev_sig {
        let prev_idx = significant[prev_sig];
        if open_sql_token_is_keyword(text, parse, prev_idx, "from")
            || open_sql_token_is_keyword(text, parse, prev_idx, "join")
        {
            return true;
        }
    }
    if sig_at_offset < significant.len() {
        let token_idx = significant[sig_at_offset];
        if parse.tokens[token_idx].kind.as_str() == "Ident"
            && let Some(prev_sig) = sig_at_offset.checked_sub(1)
        {
            let prev_idx = significant[prev_sig];
            return open_sql_token_is_keyword(text, parse, prev_idx, "from")
                || open_sql_token_is_keyword(text, parse, prev_idx, "join");
        }
    }
    false
}

fn open_sql_offset_in_field_position(
    text: &str,
    parse: &ParseResult,
    significant: &[usize],
    select_sig: usize,
    offset: usize,
) -> bool {
    if open_sql_offset_in_source_position(text, parse, significant, offset) {
        return false;
    }
    let before_or_at_offset = significant
        .iter()
        .enumerate()
        .take_while(|(_, idx)| parse.tokens[**idx].range.start <= offset)
        .map(|(sig, _)| sig)
        .last()
        .unwrap_or(select_sig);

    let from_sig = significant
        .iter()
        .position(|&idx| open_sql_token_is_keyword(text, parse, idx, "from"));
    if let Some(from_sig) = from_sig {
        if select_sig < before_or_at_offset && before_or_at_offset < from_sig {
            return true;
        }
    }

    let mut active_clause = None;
    for (sig, &idx) in significant.iter().enumerate().skip(select_sig + 1) {
        if parse.tokens[idx].range.start > offset {
            break;
        }
        if open_sql_token_is_keyword(text, parse, idx, "fields")
            || open_sql_token_is_keyword(text, parse, idx, "where")
            || open_sql_token_is_keyword(text, parse, idx, "having")
            || open_sql_token_is_keyword(text, parse, idx, "on")
            || open_sql_token_is_keyword(text, parse, idx, "order")
            || open_sql_token_is_keyword(text, parse, idx, "group")
        {
            active_clause = Some(sig);
            continue;
        }
        if open_sql_clause_keyword(text, parse, idx) || open_sql_join_starts(text, parse, idx) {
            active_clause = None;
        }
    }

    active_clause.is_some_and(|sig| {
        let keyword = parse.tokens[significant[sig]].lexeme(text);
        matches!(
            keyword.to_ascii_lowercase().as_str(),
            "fields" | "where" | "having" | "on" | "order" | "group"
        ) && offset >= parse.tokens[significant[sig]].range.end
    })
}

fn parse_open_sql_field_completion_query(
    text: &str,
    parse: &ParseResult,
    token_start: usize,
    token_end: usize,
    offset: usize,
) -> Option<OpenSqlFieldCompletionQuery> {
    let significant = open_sql_significant_tokens(parse, token_start, token_end);
    let select_sig = open_sql_first_select_sig(text, parse, &significant)?;
    if !open_sql_offset_in_field_position(text, parse, &significant, select_sig, offset) {
        return None;
    }
    let sources = parse_open_sql_sources(text, parse, &significant);
    if sources.is_empty() {
        return None;
    }
    let (replace_range, prefix, prefix_sig) =
        open_sql_prefix_at_offset(text, parse, &significant, offset);
    if prefix_sig > 0 {
        let prev_idx = significant[prefix_sig - 1];
        if matches!(
            parse.tokens[prev_idx].kind.as_str(),
            "At" | "Minus" | "Arrow" | "FatArrow"
        ) {
            return None;
        }
    }
    let source_name = open_sql_field_source_name(text, parse, &significant, prefix_sig, &sources)?;
    Some(OpenSqlFieldCompletionQuery {
        scope: ScopeId(0),
        source_name,
        replace_range,
        prefix,
    })
}

fn parse_open_sql_source_completion_query(
    text: &str,
    parse: &ParseResult,
    token_start: usize,
    token_end: usize,
    offset: usize,
) -> Option<OpenSqlSourceCompletionQuery> {
    let significant = open_sql_significant_tokens(parse, token_start, token_end);
    open_sql_first_select_sig(text, parse, &significant)?;
    if !open_sql_offset_in_source_position(text, parse, &significant, offset) {
        return None;
    }
    let (replace_range, prefix, prefix_sig) =
        open_sql_prefix_at_offset(text, parse, &significant, offset);
    if prefix_sig > 0 {
        let prev_idx = significant[prefix_sig - 1];
        if !(open_sql_token_is_keyword(text, parse, prev_idx, "from")
            || open_sql_token_is_keyword(text, parse, prev_idx, "join"))
        {
            return None;
        }
    }
    Some(OpenSqlSourceCompletionQuery {
        scope: ScopeId(0),
        replace_range,
        prefix,
    })
}

fn parse_bare_where_field_query(
    text: &str,
    parse: &ParseResult,
    token_start: usize,
    token_end: usize,
    offset: usize,
) -> Option<SelectorCompletionQuery> {
    let significant: Vec<usize> = (token_start..token_end)
        .filter(|&idx| !matches!(parse.tokens[idx].kind.as_str(), "Comment" | "Eof"))
        .collect();
    let first_idx = *significant.first()?;
    let first = parse.tokens[first_idx].lexeme(text);
    let find_keyword = |keyword: &str| {
        significant.iter().position(|&idx| {
            parse.tokens[idx].kind.as_str() == "Ident"
                && parse.tokens[idx].lexeme(text).eq_ignore_ascii_case(keyword)
        })
    };
    let (source_start_sig, source_end_sig, clause_sig, clause_end_sig) =
        if first.eq_ignore_ascii_case("delete") {
            if let Some(comparing_sig) = find_keyword("comparing") {
                let from_sig = find_keyword("from")?;
                let source_start_sig = from_sig + 1;
                let source_end_sig = significant
                    .iter()
                    .enumerate()
                    .skip(source_start_sig)
                    .find_map(|(pos, &idx)| {
                        let lexeme = parse.tokens[idx].lexeme(text);
                        matches!(lexeme.to_ascii_lowercase().as_str(), "using" | "comparing")
                            .then_some(pos)
                    })
                    .unwrap_or(comparing_sig);
                if source_start_sig >= source_end_sig || source_end_sig > comparing_sig {
                    return None;
                }
                (
                    source_start_sig,
                    source_end_sig,
                    comparing_sig,
                    significant.len(),
                )
            } else {
                let where_sig = find_keyword("where")?;
                if where_sig <= 1 {
                    return None;
                }
                (1usize, where_sig, where_sig, significant.len())
            }
        } else if first.eq_ignore_ascii_case("loop") {
            if significant
                .get(1)
                .is_none_or(|&idx| !parse.tokens[idx].lexeme(text).eq_ignore_ascii_case("at"))
            {
                return None;
            }
            let where_sig = significant.iter().position(|&idx| {
                parse.tokens[idx].kind.as_str() == "Ident"
                    && parse.tokens[idx].lexeme(text).eq_ignore_ascii_case("where")
            })?;
            if where_sig <= 2 {
                return None;
            }
            let source_end_sig = significant
                .iter()
                .enumerate()
                .skip(2)
                .find_map(|(pos, &idx)| {
                    let lexeme = parse.tokens[idx].lexeme(text);
                    matches!(
                        lexeme.to_ascii_lowercase().as_str(),
                        "into"
                            | "assigning"
                            | "reference"
                            | "transporting"
                            | "where"
                            | "from"
                            | "to"
                            | "step"
                    )
                    .then_some(pos)
                })
                .unwrap_or(where_sig);
            (2usize, source_end_sig, where_sig, significant.len())
        } else if first.eq_ignore_ascii_case("read")
            && significant.get(1).is_some_and(|&idx| {
                parse.tokens[idx].kind.as_str() == "Ident"
                    && parse.tokens[idx].lexeme(text).eq_ignore_ascii_case("table")
            })
        {
            let with_sig = significant.iter().position(|&idx| {
                parse.tokens[idx].kind.as_str() == "Ident"
                    && parse.tokens[idx].lexeme(text).eq_ignore_ascii_case("with")
            })?;
            let key_sig = with_sig + 1;
            if significant.get(key_sig).is_none_or(|&idx| {
                parse.tokens[idx].kind.as_str() != "Ident"
                    || !parse.tokens[idx].lexeme(text).eq_ignore_ascii_case("key")
            }) {
                return None;
            }
            let source_start_sig = 2usize;
            let source_end_sig = significant
                .iter()
                .enumerate()
                .skip(source_start_sig)
                .find_map(|(pos, &idx)| {
                    let lexeme = parse.tokens[idx].lexeme(text);
                    matches!(
                        lexeme.to_ascii_lowercase().as_str(),
                        "into"
                            | "assigning"
                            | "with"
                            | "index"
                            | "using"
                            | "transporting"
                            | "comparing"
                            | "binary"
                            | "reference"
                    )
                    .then_some(pos)
                })
                .unwrap_or(with_sig);
            (source_start_sig, source_end_sig, key_sig, significant.len())
        } else if first.eq_ignore_ascii_case("modify") {
            let from_sig = find_keyword("from")?;
            let source_start_sig = if significant.get(1).is_some_and(|&idx| {
                parse.tokens[idx].kind.as_str() == "Ident"
                    && parse.tokens[idx].lexeme(text).eq_ignore_ascii_case("table")
            }) {
                2usize
            } else {
                1usize
            };
            if from_sig <= source_start_sig {
                return None;
            }

            let transporting_sig = find_keyword("transporting")?;
            let where_sig = find_keyword("where");
            let transporting_idx = *significant.get(transporting_sig)?;
            let where_idx = where_sig.and_then(|sig| significant.get(sig).copied());

            if offset >= parse.tokens[transporting_idx].range.end
                && where_idx.is_none_or(|idx| offset <= parse.tokens[idx].range.start)
            {
                (
                    source_start_sig,
                    from_sig,
                    transporting_sig,
                    where_sig.unwrap_or(significant.len()),
                )
            } else if let Some(where_sig) = where_sig {
                let where_idx = *significant.get(where_sig)?;
                if offset < parse.tokens[where_idx].range.end {
                    return None;
                }
                (source_start_sig, from_sig, where_sig, significant.len())
            } else {
                return None;
            }
        } else {
            return None;
        };

    let clause_idx = *significant.get(clause_sig)?;
    let after_clause = parse.tokens[clause_idx].range.end;
    let clause_end = if clause_end_sig < significant.len() {
        parse.tokens[*significant.get(clause_end_sig)?].range.start
    } else {
        parse.tokens[*significant.last()?].range.end.max(offset)
    };
    if offset < after_clause || offset > clause_end {
        return None;
    }

    let source_tokens: Vec<usize> = significant[source_start_sig..source_end_sig]
        .iter()
        .copied()
        .filter(|&idx| parse.tokens[idx].range.end <= parse.tokens[clause_idx].range.start)
        .collect();
    let source = parse_value_access_tokens(text, parse, &source_tokens)?;

    let prefix_token = significant[clause_sig + 1..clause_end_sig]
        .iter()
        .copied()
        .find(|&idx| {
            parse.tokens[idx].kind.as_str() == "Ident"
                && parse.tokens[idx].range.start <= offset
                && offset <= parse.tokens[idx].range.end
        });
    let (replace_range, prefix) = if let Some(prefix_idx) = prefix_token {
        let token = &parse.tokens[prefix_idx];
        let prefix_end = offset.min(token.range.end);
        (
            token.range.start..prefix_end,
            Arc::<str>::from(text[token.range.start..prefix_end].to_ascii_lowercase()),
        )
    } else {
        (offset..offset, Arc::<str>::from(""))
    };

    Some(SelectorCompletionQuery {
        scope: ScopeId(0),
        base_name: source.base_name,
        base_namespace: source.base_namespace,
        component_path: source.component_path,
        replace_range,
        prefix,
        in_type_position: false,
    })
}

fn parse_value_access_tokens(
    text: &str,
    parse: &ParseResult,
    tokens: &[usize],
) -> Option<SelectorCompletionQuery> {
    let mut significant: Vec<usize> = tokens
        .iter()
        .copied()
        .filter(|&idx| !matches!(parse.tokens[idx].kind.as_str(), "Comment" | "Eof"))
        .collect();
    while significant
        .last()
        .is_some_and(|&idx| matches!(parse.tokens[idx].kind.as_str(), "RBracket" | "LBracket"))
    {
        significant.pop();
    }
    let base_idx = *significant.first()?;
    if parse.tokens[base_idx].kind.as_str() != "Ident" {
        return None;
    }
    let base_name = Arc::<str>::from(parse.tokens[base_idx].lexeme(text).to_ascii_lowercase());
    let mut component_path = Vec::new();
    let mut base_namespace = Namespace::Value;
    let mut idx = 1usize;
    while idx + 1 < significant.len() {
        let op_idx = significant[idx];
        let name_idx = significant[idx + 1];
        if parse.tokens[name_idx].kind.as_str() != "Ident" {
            return None;
        }
        match parse.tokens[op_idx].kind.as_str() {
            "Minus" | "Arrow" | "Tilde" => {}
            "FatArrow" => base_namespace = Namespace::Type,
            _ => return None,
        }
        component_path.push(Arc::<str>::from(
            parse.tokens[name_idx].lexeme(text).to_ascii_lowercase(),
        ));
        idx += 2;
    }
    Some(SelectorCompletionQuery {
        scope: ScopeId(0),
        base_name,
        base_namespace,
        component_path,
        replace_range: 0..0,
        prefix: Arc::from(""),
        in_type_position: false,
    })
}

fn access_from_selector_query(
    scope: ScopeId,
    base_name: &Arc<str>,
    base_namespace: Namespace,
    component_path: &[Arc<str>],
) -> abap_symbols::FieldAccess {
    abap_symbols::FieldAccess {
        scope,
        base_namespace,
        base_name: Arc::clone(base_name),
        base_range: 0..0,
        field_path: component_path
            .iter()
            .map(|name| abap_symbols::FieldAccessSegment {
                name: Arc::clone(name),
                range: 0..0,
            })
            .collect(),
        in_type_position: false,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SelectorOperator {
    Minus,
    Arrow,
    Tilde,
    FatArrow,
}

fn token_window_for_range(parse: &ParseResult, range: &Range<usize>) -> Option<(usize, usize)> {
    let start = parse
        .tokens
        .iter()
        .position(|token| token.kind.as_str() != "Eof" && token.range.end > range.start)?;
    let end = parse
        .tokens
        .iter()
        .rposition(|token| token.kind.as_str() != "Eof" && token.range.start < range.end)?;
    (start <= end).then_some((start, end + 1))
}

fn prefix_token_at_offset(
    parse: &ParseResult,
    start: usize,
    end: usize,
    offset: usize,
) -> Option<usize> {
    (start..end).find(|&idx| {
        let token = &parse.tokens[idx];
        token.kind.as_str() == "Ident" && token.range.start <= offset && offset <= token.range.end
    })
}

fn partial_field_symbol_prefix(
    text: &str,
    range_start: usize,
    offset: usize,
) -> Option<(Range<usize>, Arc<str>)> {
    let prefix_text = text.get(range_start..offset)?;
    let mut start = None;
    for (idx, ch) in prefix_text.char_indices().rev() {
        if ch == '<' {
            start = Some(range_start + idx);
            break;
        }
        if !field_symbol_name_char(ch) {
            return None;
        }
    }
    let start = start?;
    let prefix = text.get(start..offset)?;
    let body = &prefix[1..];
    if body.is_empty() && !text[range_start..start].trim().is_empty() {
        return None;
    }
    if body.chars().next().is_none_or(field_symbol_name_start_char)
        && body.chars().all(field_symbol_name_char)
    {
        Some((start..offset, Arc::<str>::from(prefix.to_ascii_lowercase())))
    } else {
        None
    }
}

fn field_symbol_name_start_char(ch: char) -> bool {
    matches!(ch, '_' | '/') || ch.is_ascii_alphabetic()
}

fn field_symbol_name_char(ch: char) -> bool {
    field_symbol_name_start_char(ch) || ch.is_ascii_digit()
}

fn first_token_starting_at_or_after(
    parse: &ParseResult,
    start: usize,
    end: usize,
    offset: usize,
) -> usize {
    (start..end)
        .find(|&idx| parse.tokens[idx].range.start >= offset)
        .unwrap_or(end)
}

fn previous_significant_token(parse: &ParseResult, start: usize, mut end: usize) -> Option<usize> {
    while end > start {
        end -= 1;
        if !matches!(parse.tokens[end].kind.as_str(), "Comment" | "Eof") {
            return Some(end);
        }
    }
    None
}

fn previous_selector_significant_token(
    parse: &ParseResult,
    start: usize,
    mut end: usize,
) -> Option<usize> {
    while end > start {
        end -= 1;
        match parse.tokens[end].kind.as_str() {
            "Comment" | "Eof" => {}
            "RBracket" => end = matching_group_start(parse, start, end, "LBracket", "RBracket")?,
            _ => return Some(end),
        }
    }
    None
}

fn matching_group_start(
    parse: &ParseResult,
    start: usize,
    end: usize,
    open_kind: &str,
    close_kind: &str,
) -> Option<usize> {
    let mut depth = 1usize;
    let mut idx = end;
    while idx > start {
        idx -= 1;
        match parse.tokens[idx].kind.as_str() {
            "Comment" | "Eof" => {}
            kind if kind == close_kind => depth += 1,
            kind if kind == open_kind => {
                depth -= 1;
                if depth == 0 {
                    return Some(idx);
                }
            }
            _ => {}
        }
    }
    None
}

fn selector_operator_before_token(
    parse: &ParseResult,
    start: usize,
    end: usize,
) -> Option<(usize, SelectorOperator)> {
    let op_idx = previous_selector_significant_token(parse, start, end)?;
    let op = &parse.tokens[op_idx];
    let prev_raw_idx = previous_significant_token(parse, start, op_idx)?;
    let left_idx = previous_selector_significant_token(parse, start, op_idx)?;
    let left = &parse.tokens[left_idx];
    if left.kind.as_str() != "Ident" {
        return None;
    }

    let kind = match op.kind.as_str() {
        "Minus" if parse.tokens[prev_raw_idx].range.end >= op.range.start => {
            SelectorOperator::Minus
        }
        "Arrow" => SelectorOperator::Arrow,
        "Tilde" => SelectorOperator::Tilde,
        "FatArrow" => SelectorOperator::FatArrow,
        _ => return None,
    };
    Some((op_idx, kind))
}

fn type_keyword_before_base(
    parse: &ParseResult,
    text: &str,
    start: usize,
    base_idx: usize,
) -> bool {
    let Some(keyword_idx) = previous_selector_significant_token(parse, start, base_idx) else {
        return false;
    };
    let keyword = parse.tokens[keyword_idx].lexeme(text);
    keyword.eq_ignore_ascii_case("type") || keyword.eq_ignore_ascii_case("like")
}

fn bare_identifier_token_context_is_type_position(
    parse: &ParseResult,
    text: &str,
    start: usize,
    ident_idx: usize,
) -> bool {
    let Some(prev_idx) = previous_significant_token(parse, start, ident_idx) else {
        return false;
    };
    let prev = parse.tokens[prev_idx].lexeme(text);
    if prev.eq_ignore_ascii_case("type") {
        return true;
    }
    if prev.eq_ignore_ascii_case("like") {
        return false;
    }
    if !prev.eq_ignore_ascii_case("of") && !prev.eq_ignore_ascii_case("to") {
        return false;
    }

    let mut cursor = prev_idx;
    while let Some(idx) = previous_significant_token(parse, start, cursor) {
        match parse.tokens[idx].kind.as_str() {
            "Comma" | "Colon" | "Period" => return false,
            _ => {}
        }
        let lexeme = parse.tokens[idx].lexeme(text);
        if lexeme.eq_ignore_ascii_case("type") {
            return true;
        }
        if lexeme.eq_ignore_ascii_case("like") {
            return false;
        }
        cursor = idx;
    }
    false
}

#[derive(Debug, Default)]
pub struct DocumentStore {
    documents: RwLock<HashMap<Arc<str>, Arc<AnalysisSnapshot>>>,
    analysis: RwLock<Option<CachedWorkspaceAnalysis>>,
    analysis_revision: RwLock<u64>,
    preview_metrics: RwLock<Option<PreviewMetrics>>,
    lint_policy: RwLock<Arc<LintPolicy>>,
}

impl Clone for DocumentStore {
    fn clone(&self) -> Self {
        Self {
            documents: RwLock::new(self.documents.read().clone()),
            analysis: RwLock::new(self.analysis.read().clone()),
            analysis_revision: RwLock::new(*self.analysis_revision.read()),
            preview_metrics: RwLock::new(self.preview_metrics.read().clone()),
            lint_policy: RwLock::new(Arc::clone(&self.lint_policy.read())),
        }
    }
}

fn snapshot_matches_input(snapshot: &AnalysisSnapshot, input: &DocumentInput) -> bool {
    snapshot.text.as_ref() == input.text.as_ref()
        && snapshot.is_dependency == input.is_dependency
        && snapshot.object_name == input.object_name
}

fn build_plan_matches_cached_analysis(
    analysis: Option<&CachedWorkspaceAnalysis>,
    build_plan: SnapshotBuildPlan,
    lint_policy: &LintPolicy,
) -> bool {
    analysis.is_some_and(|analysis| {
        analysis.build_plan == build_plan.normalized()
            && analysis.lint_policy.as_ref() == lint_policy
    })
}

fn clone_snapshot_with_version(snapshot: &AnalysisSnapshot, version: i32) -> Arc<AnalysisSnapshot> {
    Arc::new(AnalysisSnapshot {
        scope_index: Arc::clone(&snapshot.scope_index),
        uri: Arc::clone(&snapshot.uri),
        version,
        text: Arc::clone(&snapshot.text),
        line_index: Arc::clone(&snapshot.line_index),
        project_texts: Arc::clone(&snapshot.project_texts),
        is_dependency: snapshot.is_dependency,
        object_name: snapshot.object_name.clone(),
        parse: Arc::clone(&snapshot.parse),
        symbols: Arc::clone(&snapshot.symbols),
        project: Arc::clone(&snapshot.project),
        routine_analysis: Arc::clone(&snapshot.routine_analysis),
        lint_analysis: Arc::clone(&snapshot.lint_analysis),
        static_analysis: snapshot.static_analysis.as_ref().map(Arc::clone),
        callable_summaries: Arc::clone(&snapshot.callable_summaries),
        call_graph: Arc::clone(&snapshot.call_graph),
    })
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct AnalysisMetrics {
    parse_count: usize,
    local_phase_count: usize,
    dirty_uri_count: usize,
    parse_micros: u128,
    local_phase_micros: u128,
    dependency_projection_micros: u128,
    parse_work_micros: u128,
    local_phase_work_micros: u128,
    project_update_micros: u128,
    snapshot_build_micros: u128,
    routine_analysis_micros: u128,
    routine_analysis_index_micros: u128,
    routine_analysis_ir_micros: u128,
    routine_analysis_cfg_micros: u128,
    routine_analysis_dataflow_micros: u128,
    routine_analysis_dead_store_micros: u128,
    routine_analysis_perform_routine_count: usize,
    routine_analysis_dataflow_pass_count: usize,
    routine_analysis_dataflow_routine_runs: usize,
    static_analysis_summary_micros: u128,
    callable_summary_micros: u128,
    full_rebuild: bool,
    unit_count: usize,
    dirty_unit_count: usize,
    diagnostic_scope_unit_count: usize,
    validation_unit_count: usize,
    scope_index_clone_micros: u128,
    build_workspace_index_micros: u128,
    compute_dirty_set_micros: u128,
    clone_previous_units_micros: u128,
    apply_local_updates_micros: u128,
    resolve_include_edges_micros: u128,
    resolve_cross_unit_micros: u128,
    infer_semantic_facts_micros: u128,
    rebuild_semantic_index_micros: u128,
    validate_micros: u128,
    collect_project_diagnostics_micros: u128,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct PreviewMetrics {
    parse_count: usize,
    local_phase_count: usize,
    build_micros: u128,
    committed_context_only: bool,
    fell_back_to_single_document: bool,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct WorkspaceAnalysisMetricsSnapshot {
    pub parse_count: usize,
    pub local_phase_count: usize,
    pub dirty_uri_count: usize,
    pub parse_micros: u128,
    pub local_phase_micros: u128,
    pub dependency_projection_micros: u128,
    pub parse_work_micros: u128,
    pub local_phase_work_micros: u128,
    pub project_update_micros: u128,
    pub snapshot_build_micros: u128,
    pub routine_analysis_micros: u128,
    pub routine_analysis_index_micros: u128,
    pub routine_analysis_ir_micros: u128,
    pub routine_analysis_cfg_micros: u128,
    pub routine_analysis_dataflow_micros: u128,
    pub routine_analysis_dead_store_micros: u128,
    pub routine_analysis_perform_routine_count: usize,
    pub routine_analysis_dataflow_pass_count: usize,
    pub routine_analysis_dataflow_routine_runs: usize,
    pub static_analysis_summary_micros: u128,
    pub callable_summary_micros: u128,
    pub full_rebuild: bool,
    pub unit_count: usize,
    pub dirty_unit_count: usize,
    pub diagnostic_scope_unit_count: usize,
    pub validation_unit_count: usize,
    pub scope_index_clone_micros: u128,
    pub build_workspace_index_micros: u128,
    pub compute_dirty_set_micros: u128,
    pub clone_previous_units_micros: u128,
    pub apply_local_updates_micros: u128,
    pub resolve_include_edges_micros: u128,
    pub resolve_cross_unit_micros: u128,
    pub infer_semantic_facts_micros: u128,
    pub rebuild_semantic_index_micros: u128,
    pub validate_micros: u128,
    pub collect_project_diagnostics_micros: u128,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WorkspacePreviewMetricsSnapshot {
    pub parse_count: usize,
    pub local_phase_count: usize,
    pub build_micros: u128,
    pub committed_context_only: bool,
    pub fell_back_to_single_document: bool,
}

#[derive(Debug, Clone)]
struct CachedWorkspaceAnalysis {
    uri_order: Vec<Arc<str>>,
    locals: HashMap<Arc<str>, LocalAnalysis>,
    dirty_uris: HashSet<Arc<str>>,
    metrics: AnalysisMetrics,
    build_plan: SnapshotBuildPlan,
    lint_policy: Arc<LintPolicy>,
}

#[derive(Debug, Clone)]
struct PreparedDocument {
    uri: Arc<str>,
    version: i32,
    text: Arc<str>,
    analysis_text: Option<Arc<str>>,
    is_dependency: bool,
    object_name: Option<Arc<str>>,
    previous: Option<Arc<AnalysisSnapshot>>,
    parse: Arc<ParseResult>,
    local: LocalAnalysis,
}

fn current_uri_order(
    existing: &HashMap<Arc<str>, Arc<AnalysisSnapshot>>,
    analysis: Option<&CachedWorkspaceAnalysis>,
) -> Vec<Arc<str>> {
    if let Some(analysis) = analysis {
        return analysis.uri_order.clone();
    }
    let mut uris: Vec<_> = existing.keys().cloned().collect();
    uris.sort();
    uris
}

fn staged_documents_for_inputs(
    inputs: &[DocumentInput],
    existing: &HashMap<Arc<str>, Arc<AnalysisSnapshot>>,
    analysis: Option<&CachedWorkspaceAnalysis>,
) -> Vec<StagedDocument> {
    let input_by_uri: HashMap<_, _> = inputs
        .iter()
        .map(|input| (Arc::clone(&input.uri), input))
        .collect();
    let mut staged = Vec::with_capacity(inputs.len());
    let mut seen = HashSet::new();

    for uri in current_uri_order(existing, analysis) {
        let Some(input) = input_by_uri.get(&uri) else {
            continue;
        };
        let snapshot = existing.get(uri.as_ref());
        staged.push(StagedDocument {
            uri: Arc::clone(&input.uri),
            version: input.version,
            text: Arc::clone(&input.text),
            is_dependency: input.is_dependency,
            object_name: input.object_name.clone(),
            previous: snapshot.cloned(),
        });
        seen.insert(Arc::clone(&uri));
    }

    let mut remaining: Vec<_> = inputs
        .iter()
        .filter(|input| !seen.contains(input.uri.as_ref()))
        .collect();
    remaining.sort_by(|left, right| left.uri.cmp(&right.uri));
    for input in remaining {
        staged.push(StagedDocument {
            uri: Arc::clone(&input.uri),
            version: input.version,
            text: Arc::clone(&input.text),
            is_dependency: input.is_dependency,
            object_name: input.object_name.clone(),
            previous: existing.get(input.uri.as_ref()).cloned(),
        });
    }

    staged
}

fn prepare_documents(
    inputs: &[DocumentInput],
    existing: Option<&HashMap<Arc<str>, Arc<AnalysisSnapshot>>>,
    previous_analysis: Option<&CachedWorkspaceAnalysis>,
    progress: Option<&(dyn Fn(usize, usize) + Sync)>,
    build_plan: SnapshotBuildPlan,
) -> (Vec<PreparedDocument>, AnalysisMetrics) {
    let parse_timer = std::time::Instant::now();
    let empty_existing = HashMap::new();
    let staged = staged_documents_for_inputs(
        inputs,
        existing.unwrap_or(&empty_existing),
        previous_analysis,
    );
    let processed = AtomicUsize::new(0);
    let parse_count = AtomicUsize::new(0);
    let local_phase_count = AtomicUsize::new(0);
    let dependency_projection_micros = AtomicU64::new(0);
    let parse_work_micros = AtomicU64::new(0);
    let local_phase_work_micros = AtomicU64::new(0);
    let total = staged.len();
    let metrics = AnalysisMetrics::default();
    let prepared: Vec<_> = staged
        .par_iter()
        .enumerate()
        .map(|(idx, entry)| {
            let input = DocumentInput {
                uri: Arc::clone(&entry.uri),
                version: entry.version,
                text: Arc::clone(&entry.text),
                is_dependency: entry.is_dependency,
                object_name: entry.object_name.clone(),
            };
            let previous_local =
                previous_analysis.and_then(|analysis| analysis.locals.get(entry.uri.as_ref()));
            let previous_snapshot = entry.previous.as_ref();
            let previous = previous_snapshot
                .filter(|snapshot| snapshot_matches_input(snapshot, &input))
                .cloned();
            let reuse_previous = previous.is_some() && previous_local.is_some();
            let defer_analysis_text = reuse_previous
                && entry.is_dependency
                && build_plan.dependency_diagnostics
                    == DependencyDiagnosticsMode::EditableAndIncludes;
            let mut analysis_text = None;
            let parse = if let Some(snapshot) = previous.as_ref().filter(|_| reuse_previous) {
                if !defer_analysis_text {
                    let projection_timer = std::time::Instant::now();
                    analysis_text = Some(analysis_text_for_input(&input));
                    dependency_projection_micros.fetch_add(
                        projection_timer.elapsed().as_micros() as u64,
                        Ordering::Relaxed,
                    );
                }
                Arc::clone(&snapshot.parse)
            } else {
                let projection_timer = std::time::Instant::now();
                let projected = analysis_text_for_input(&input);
                dependency_projection_micros.fetch_add(
                    projection_timer.elapsed().as_micros() as u64,
                    Ordering::Relaxed,
                );
                parse_count.fetch_add(1, Ordering::Relaxed);
                let parse_work_timer = std::time::Instant::now();
                let parsed = Arc::new(parse(projected.as_ref()));
                parse_work_micros.fetch_add(
                    parse_work_timer.elapsed().as_micros() as u64,
                    Ordering::Relaxed,
                );
                analysis_text = Some(projected);
                if let Some(snapshot) = previous_snapshot {
                    if snapshot.parse.as_ref().tokens == parsed.as_ref().tokens
                        && snapshot.parse.as_ref().errors == parsed.as_ref().errors
                    {
                        Arc::clone(&snapshot.parse)
                    } else {
                        parsed
                    }
                } else {
                    parsed
                }
            };
            let local = if let Some(previous) = previous_local.filter(|_| reuse_previous) {
                previous.clone()
            } else {
                local_phase_count.fetch_add(1, Ordering::Relaxed);
                let local_phase_work_timer = std::time::Instant::now();
                let analysis_text = analysis_text
                    .as_ref()
                    .expect("local analysis should have projected text");
                let local = analyze_unit_local_state_for_project_build(
                    UnitId(idx as u32),
                    Arc::clone(&entry.uri),
                    analysis_text.as_ref(),
                    parse.as_ref(),
                );
                local_phase_work_micros.fetch_add(
                    local_phase_work_timer.elapsed().as_micros() as u64,
                    Ordering::Relaxed,
                );
                local
            };
            let mut local = local;
            if let Some(object_name) = &entry.object_name {
                local.unit.provided_names.push(Arc::clone(object_name));
                local.unit.provided_names.sort();
                local.unit.provided_names.dedup();
            }
            if let Some(progress) = progress {
                let done = processed.fetch_add(1, Ordering::Relaxed) + 1;
                progress(done, total);
            }
            PreparedDocument {
                uri: Arc::clone(&entry.uri),
                version: entry.version,
                text: Arc::clone(&entry.text),
                analysis_text,
                is_dependency: entry.is_dependency,
                object_name: entry.object_name.clone(),
                previous,
                parse,
                local,
            }
        })
        .collect();

    let mut metrics = metrics;
    metrics.parse_count = parse_count.load(Ordering::Relaxed);
    metrics.local_phase_count = local_phase_count.load(Ordering::Relaxed);
    metrics.dependency_projection_micros =
        dependency_projection_micros.load(Ordering::Relaxed) as u128;
    metrics.parse_work_micros = parse_work_micros.load(Ordering::Relaxed) as u128;
    metrics.local_phase_work_micros = local_phase_work_micros.load(Ordering::Relaxed) as u128;
    metrics.parse_micros = parse_timer.elapsed().as_micros();
    metrics.local_phase_micros = metrics.parse_micros;
    (prepared, metrics)
}

fn materialize_snapshots(
    prepared: Vec<PreparedDocument>,
    update: IncrementalProjectUpdate,
    build_plan: SnapshotBuildPlan,
    lint_policy: Arc<LintPolicy>,
) -> (
    HashMap<Arc<str>, Arc<AnalysisSnapshot>>,
    CachedWorkspaceAnalysis,
) {
    let snapshot_timer = std::time::Instant::now();
    let build_plan = build_plan.normalized();
    let project = Arc::new(update.project);
    let project_texts = Arc::new(
        prepared
            .iter()
            .map(|prepared| (Arc::clone(&prepared.uri), Arc::clone(&prepared.text)))
            .collect::<HashMap<_, _>>(),
    );
    let scope_indexes = if build_plan.call_graph {
        let mut scope_indexes = vec![ScopeIndex::default(); project.units.len()];
        for prepared in &prepared {
            scope_indexes[prepared.local.unit.unit_id.as_usize()] =
                prepared.local.scope_index.clone();
        }
        scope_indexes
    } else {
        Vec::new()
    };
    let mut prepared_units = Vec::with_capacity(prepared.len());
    for prepared in prepared {
        let unit = project
            .unit_by_uri(prepared.uri.as_ref())
            .cloned()
            .expect("project analysis should include every prepared document");
        prepared_units.push((prepared, unit));
    }
    let diagnostic_unit_ids =
        diagnostic_unit_ids_for_build_plan(&prepared_units, project.as_ref(), build_plan);
    let (routine_analysis, routine_analysis_micros) = if build_plan.routine_analysis {
        let routine_analysis_timer = std::time::Instant::now();
        let routine_analysis = if build_plan.dependency_diagnostics
            == DependencyDiagnosticsMode::EditableAndIncludes
            && !build_plan.static_analysis
            && !build_plan.callable_summaries
        {
            Arc::new(build_project_routine_analysis_for_units(
                project.as_ref(),
                &diagnostic_unit_ids,
            ))
        } else {
            Arc::new(build_project_routine_analysis(project.as_ref()))
        };
        (
            routine_analysis,
            routine_analysis_timer.elapsed().as_micros(),
        )
    } else {
        (Arc::new(ProjectRoutineAnalysis::default()), 0)
    };
    let (static_analysis, static_analysis_summary_micros) = if build_plan.static_analysis {
        let static_analysis_timer = std::time::Instant::now();
        (
            Some(Arc::new(build_project_static_analysis_summary(
                project.as_ref(),
                routine_analysis.as_ref(),
            ))),
            static_analysis_timer.elapsed().as_micros(),
        )
    } else {
        (None, 0)
    };
    let call_graph = if build_plan.call_graph {
        Arc::new(call_graph::build_project_call_graph(
            project.as_ref(),
            &scope_indexes,
        ))
    } else {
        Arc::new(ProjectCallGraph::default())
    };
    let (callable_summaries, callable_summary_micros) = if build_plan.callable_summaries {
        let callable_summary_timer = std::time::Instant::now();
        (
            Arc::new(callable_summary::build_project_callable_summary_analysis(
                project.as_ref(),
                routine_analysis.as_ref(),
                call_graph.as_ref(),
            )),
            callable_summary_timer.elapsed().as_micros(),
        )
    } else {
        (Arc::new(ProjectCallableSummaryAnalysis::default()), 0)
    };
    let mut snapshots = HashMap::with_capacity(prepared_units.len());
    let mut locals = HashMap::with_capacity(prepared_units.len());
    let mut uri_order = Vec::with_capacity(prepared_units.len());

    for (_, unit) in &mut prepared_units {
        if !diagnostic_unit_ids.contains(&unit.unit_id) {
            unit.diagnostics.clear();
            continue;
        }
        if build_plan.routine_analysis {
            *unit = augment_unit_with_routine_diagnostics(unit.clone(), routine_analysis.as_ref());
        }
    }
    let lint_analysis = if build_plan.lint_analysis {
        let lint_scope_indexes: Vec<&ScopeIndex> = prepared_units
            .iter()
            .map(|(prepared, _)| &prepared.local.scope_index)
            .collect();
        let lint_lookup = build_lint_metadata_lookup(project.as_ref());
        let lint_context = ProjectLintContext {
            project: project.as_ref(),
            scope_indexes: &lint_scope_indexes,
            lookup: &lint_lookup,
        };
        Arc::new(build_project_lint_analysis(
            &lint_context,
            prepared_units
                .iter()
                .filter(|(_, unit)| diagnostic_unit_ids.contains(&unit.unit_id))
                .map(|(prepared, unit)| (prepared, unit)),
            lint_policy.as_ref(),
        ))
    } else {
        Arc::new(ProjectLintAnalysis::default())
    };

    for (prepared, unit) in prepared_units {
        let scope_index = Arc::new(build_scope_index(&unit));
        let line_index = prepared
            .previous
            .as_ref()
            .map(|snapshot| Arc::clone(&snapshot.line_index))
            .unwrap_or_else(|| Arc::new(LineIndex::new(prepared.text.as_ref())));
        locals.insert(Arc::clone(&prepared.uri), prepared.local);
        uri_order.push(Arc::clone(&prepared.uri));
        snapshots.insert(
            Arc::clone(&prepared.uri),
            Arc::new(AnalysisSnapshot {
                scope_index,
                uri: Arc::clone(&prepared.uri),
                version: prepared.version,
                text: Arc::clone(&prepared.text),
                line_index,
                project_texts: Arc::clone(&project_texts),
                is_dependency: prepared.is_dependency,
                object_name: prepared.object_name.clone(),
                parse: Arc::clone(&prepared.parse),
                symbols: Arc::new(unit),
                project: Arc::clone(&project),
                routine_analysis: Arc::clone(&routine_analysis),
                lint_analysis: Arc::clone(&lint_analysis),
                static_analysis: static_analysis.as_ref().map(Arc::clone),
                callable_summaries: Arc::clone(&callable_summaries),
                call_graph: Arc::clone(&call_graph),
            }),
        );
    }

    let metrics = AnalysisMetrics {
        dirty_uri_count: update.dirty_uris.len(),
        snapshot_build_micros: snapshot_timer.elapsed().as_micros(),
        routine_analysis_micros,
        routine_analysis_index_micros: routine_analysis.metrics.index_micros,
        routine_analysis_ir_micros: routine_analysis.metrics.ir_micros,
        routine_analysis_cfg_micros: routine_analysis.metrics.cfg_micros,
        routine_analysis_dataflow_micros: routine_analysis.metrics.dataflow_micros,
        routine_analysis_dead_store_micros: routine_analysis.metrics.dead_store_micros,
        routine_analysis_perform_routine_count: routine_analysis.metrics.perform_routine_count,
        routine_analysis_dataflow_pass_count: routine_analysis.metrics.dataflow_pass_count,
        routine_analysis_dataflow_routine_runs: routine_analysis.metrics.dataflow_routine_runs,
        static_analysis_summary_micros,
        callable_summary_micros,
        ..AnalysisMetrics::default()
    };
    (
        snapshots,
        CachedWorkspaceAnalysis {
            uri_order,
            locals,
            dirty_uris: update.dirty_uris,
            metrics,
            build_plan,
            lint_policy,
        },
    )
}

fn augment_unit_with_routine_diagnostics(
    mut unit: UnitAnalysis,
    routine_analysis: &ProjectRoutineAnalysis,
) -> UnitAnalysis {
    let diagnostics = routine_analysis.diagnostics_for_unit(unit.unit_id);
    if diagnostics.is_empty() {
        return unit;
    }
    unit.diagnostics.extend_from_slice(diagnostics);
    unit.diagnostics.sort_by(|left, right| {
        left.range
            .start
            .cmp(&right.range.start)
            .then(left.range.end.cmp(&right.range.end))
            .then(left.message.cmp(&right.message))
    });
    unit.diagnostics.dedup();
    unit
}

fn diagnostic_scope_roots_for_build_plan(
    prepared: &[PreparedDocument],
    build_plan: SnapshotBuildPlan,
) -> Option<HashSet<UnitId>> {
    match build_plan.normalized().dependency_diagnostics {
        DependencyDiagnosticsMode::All => None,
        DependencyDiagnosticsMode::EditableAndIncludes => Some(
            prepared
                .iter()
                .filter(|prepared| !prepared.is_dependency)
                .map(|prepared| prepared.local.unit.unit_id)
                .collect(),
        ),
    }
}

fn diagnostic_unit_ids_for_build_plan(
    prepared_units: &[(PreparedDocument, UnitAnalysis)],
    project: &ProjectAnalysis,
    build_plan: SnapshotBuildPlan,
) -> HashSet<UnitId> {
    match build_plan.normalized().dependency_diagnostics {
        DependencyDiagnosticsMode::All => project.units.iter().map(|unit| unit.unit_id).collect(),
        DependencyDiagnosticsMode::EditableAndIncludes => {
            let roots: HashSet<_> = prepared_units
                .iter()
                .filter(|(prepared, _)| !prepared.is_dependency)
                .map(|(_, unit)| unit.unit_id)
                .collect();
            include_closure_for_unit_ids(&project.units, &roots)
        }
    }
}

fn include_closure_for_unit_ids(
    units: &[UnitAnalysis],
    roots: &HashSet<UnitId>,
) -> HashSet<UnitId> {
    let mut out = HashSet::new();
    for &root in roots {
        collect_include_closure_for_unit_id(units, root, &mut out);
    }
    out
}

fn collect_include_closure_for_unit_id(
    units: &[UnitAnalysis],
    unit_id: UnitId,
    out: &mut HashSet<UnitId>,
) {
    if units.get(unit_id.as_usize()).is_none() || !out.insert(unit_id) {
        return;
    }
    for target in units[unit_id.as_usize()]
        .include_edges
        .iter()
        .filter_map(|edge| edge.target)
    {
        collect_include_closure_for_unit_id(units, target, out);
    }
}

pub fn lint_metadata_for_diagnostic_kind(kind: DiagnosticKind) -> Option<&'static LintMetadata> {
    let id = match kind {
        DiagnosticKind::UnreachableCode => ABAP_LSP_UNREACHABLE_CODE,
        DiagnosticKind::UseBeforeDefiniteAssignment => ABAP_LSP_USE_BEFORE_DEFINITE_ASSIGNMENT,
        DiagnosticKind::PossiblyUnboundFieldSymbol => ABAP_LSP_POSSIBLY_UNBOUND_FIELD_SYMBOL,
        DiagnosticKind::DeadStore => ABAP_LSP_DEAD_STORE,
        DiagnosticKind::UnsortedReadTableBinarySearch => ABAP_LSP_UNSORTED_READ_TABLE_BINARY_SEARCH,
        DiagnosticKind::UnverifiedOpenSqlSource => EPC_UNVERIFIED_OPEN_SQL_SOURCE,
        DiagnosticKind::InvalidOpenSqlIntoTarget => EPC_INVALID_OPEN_SQL_INTO_TARGET,
        DiagnosticKind::MissingTablesDeclaration => EPC_MISSING_TABLES_DECLARATION,
        _ => return None,
    };
    abap_lints::metadata_for(id)
}

pub fn lint_id_for_diagnostic_kind(kind: DiagnosticKind) -> Option<LintId> {
    lint_metadata_for_diagnostic_kind(kind).map(|metadata| metadata.id)
}

struct ProjectLintContext<'a> {
    project: &'a ProjectAnalysis,
    scope_indexes: &'a [&'a ScopeIndex],
    lookup: &'a LintMetadataLookup,
}

struct LintMetadataLookup {
    per_unit_root_index: Vec<HashMap<(Namespace, Arc<str>), Vec<SymbolId>>>,
    root_index: HashMap<(Namespace, Arc<str>), Vec<SymbolHandle>>,
}

fn build_project_lint_analysis<'a>(
    context: &ProjectLintContext<'_>,
    units: impl IntoIterator<Item = (&'a PreparedDocument, &'a UnitAnalysis)>,
    lint_policy: &LintPolicy,
) -> ProjectLintAnalysis {
    let mut diagnostics = Vec::new();
    for (prepared, unit) in units {
        let analysis_text = prepared
            .analysis_text
            .as_ref()
            .map(Arc::clone)
            .unwrap_or_else(|| {
                analysis_text_for_document(prepared.text.as_ref(), prepared.is_dependency)
            });
        let suppression_index =
            SuppressionIndex::new(analysis_text.as_ref(), &prepared.parse.lexed);
        for diagnostic in &unit.diagnostics {
            if let Some(mut lint_diagnostic) =
                lint_diagnostic_from_symbol_diagnostic(diagnostic, lint_policy)
            {
                push_filtered_lint_diagnostic(
                    &mut diagnostics,
                    unit.uri.as_ref(),
                    &suppression_index,
                    lint_policy,
                    &mut lint_diagnostic,
                );
            }
        }
        for mut lint_diagnostic in
            build_local_lint_diagnostics(context, unit, analysis_text.as_ref(), lint_policy)
        {
            push_filtered_lint_diagnostic(
                &mut diagnostics,
                unit.uri.as_ref(),
                &suppression_index,
                lint_policy,
                &mut lint_diagnostic,
            );
        }
    }
    ProjectLintAnalysis::from_diagnostics(diagnostics)
}

fn push_filtered_lint_diagnostic(
    diagnostics: &mut Vec<(String, LintDiagnostic)>,
    uri: &str,
    suppression_index: &SuppressionIndex,
    lint_policy: &LintPolicy,
    lint_diagnostic: &mut LintDiagnostic,
) {
    if !lint_diagnostic.suppressed
        && let Some(suppression) = suppression_index.suppression_for(lint_diagnostic)
    {
        if !lint_policy.report_suppressed() {
            return;
        }
        mark_lint_suppressed(lint_diagnostic, suppression);
    }
    diagnostics.push((uri.to_string(), lint_diagnostic.clone()));
}

fn build_local_lint_diagnostics(
    context: &ProjectLintContext<'_>,
    unit: &UnitAnalysis,
    source: &str,
    lint_policy: &LintPolicy,
) -> Vec<LintDiagnostic> {
    let mut diagnostics = Vec::new();
    lint_select_star(unit, lint_policy, &mut diagnostics);
    lint_select_in_loop(unit, lint_policy, &mut diagnostics);
    lint_select_single_without_full_key(context, unit, lint_policy, &mut diagnostics);
    lint_for_all_entries_without_guard(unit, lint_policy, &mut diagnostics);
    lint_dynamic_open_sql(unit, lint_policy, &mut diagnostics);
    let system_field_index = SystemFieldLintIndex::new(unit);
    lint_ignored_authority_check(unit, &system_field_index, lint_policy, &mut diagnostics);
    lint_ignored_call_function_result(
        unit,
        &system_field_index,
        source,
        lint_policy,
        &mut diagnostics,
    );
    diagnostics
}

fn lint_select_star(
    unit: &UnitAnalysis,
    lint_policy: &LintPolicy,
    diagnostics: &mut Vec<LintDiagnostic>,
) {
    let Some(metadata) = abap_lints::metadata_for(ABAP_LSP_SELECT_STAR) else {
        return;
    };
    for projection in &unit.sql_projections {
        if !matches!(
            projection.kind,
            SqlProjectionKind::Star | SqlProjectionKind::QualifiedStar
        ) {
            continue;
        }
        let message = match projection.source_alias.as_ref() {
            Some(alias) => format!(
                "Open SQL SELECT uses '{}~*'; list the required columns explicitly",
                alias
            ),
            None => "Open SQL SELECT * reads all columns; list the required columns explicitly"
                .to_string(),
        };
        emit_lint_diagnostic(
            diagnostics,
            metadata,
            projection.range.clone(),
            message,
            lint_policy,
        );
    }
}

fn lint_select_in_loop(
    unit: &UnitAnalysis,
    lint_policy: &LintPolicy,
    diagnostics: &mut Vec<LintDiagnostic>,
) {
    let Some(metadata) = abap_lints::metadata_for(ABAP_LSP_SELECT_IN_LOOP) else {
        return;
    };
    for query in &unit.sql_queries {
        let Some(loop_kind) = enclosing_loop_kind_for_scope(unit, query.scope) else {
            continue;
        };
        let loop_name = match loop_kind {
            RoutineLoopKind::Do => "DO",
            RoutineLoopKind::While => "WHILE",
            RoutineLoopKind::Loop => "LOOP",
        };
        let range = query
            .from_clause
            .clone()
            .unwrap_or_else(|| query.range.clone());
        emit_lint_diagnostic(
            diagnostics,
            metadata,
            range,
            format!(
                "Open SQL SELECT runs inside a {loop_name} body; prefer bulk selection before the loop"
            ),
            lint_policy,
        );
    }
}

fn lint_select_single_without_full_key(
    context: &ProjectLintContext<'_>,
    unit: &UnitAnalysis,
    lint_policy: &LintPolicy,
    diagnostics: &mut Vec<LintDiagnostic>,
) {
    let Some(metadata) = abap_lints::metadata_for(ABAP_LSP_SELECT_SINGLE_WITHOUT_FULL_KEY) else {
        return;
    };
    for query in &unit.sql_queries {
        if !query.is_single || query.has_dynamic_where {
            continue;
        }
        let Some(source) = single_static_from_source(unit, query.id) else {
            continue;
        };
        let Some(primary_key_fields) =
            open_sql_primary_key_fields_for_source(context, unit, query.scope, source)
        else {
            continue;
        };
        let required_key_fields = primary_key_fields
            .iter()
            .filter(|field| !is_client_column_name(field.as_ref()))
            .collect::<Vec<_>>();
        if required_key_fields.is_empty() {
            continue;
        }
        let where_fields = open_sql_where_column_names(unit, query, source);
        let missing_key_fields = required_key_fields
            .iter()
            .filter(|field| !where_fields.contains(&field.to_ascii_lowercase()))
            .map(|field| field.as_ref().to_string())
            .collect::<Vec<_>>();
        if missing_key_fields.is_empty() {
            continue;
        }

        let range = query
            .where_clause
            .clone()
            .or_else(|| query.from_clause.clone())
            .unwrap_or_else(|| query.range.clone());
        emit_lint_diagnostic(
            diagnostics,
            metadata,
            range,
            format!(
                "SELECT SINGLE from '{}' does not restrict primary-key field(s) {} in the WHERE clause",
                source.name,
                missing_key_fields.join(", ")
            ),
            lint_policy,
        );
    }
}

fn single_static_from_source(unit: &UnitAnalysis, query_id: usize) -> Option<&SqlSourceData> {
    let sources = unit
        .sql_sources
        .iter()
        .filter(|source| source.query_id == query_id)
        .collect::<Vec<_>>();
    sources
        .as_slice()
        .first()
        .copied()
        .filter(|source| sources.len() == 1 && source.source_kind == SqlSourceKind::From)
}

fn open_sql_where_column_names(
    unit: &UnitAnalysis,
    query: &SqlQueryData,
    source: &SqlSourceData,
) -> HashSet<String> {
    let Some(where_range) = query.where_clause.as_ref() else {
        return HashSet::new();
    };
    unit.sql_name_refs
        .iter()
        .filter(|sql_ref| sql_ref.query_id == query.id)
        .filter(|sql_ref| {
            matches!(
                sql_ref.kind,
                SqlNameRefKind::Column | SqlNameRefKind::QualifiedColumn
            )
        })
        .filter(|sql_ref| {
            where_range.start <= sql_ref.range.start && sql_ref.range.end <= where_range.end
        })
        .filter(|sql_ref| {
            sql_ref
                .qualifier
                .as_ref()
                .is_none_or(|qualifier| sql_source_matches_qualifier(source, qualifier))
        })
        .map(|sql_ref| sql_ref.name.to_ascii_lowercase())
        .collect()
}

fn build_lint_metadata_lookup(project: &ProjectAnalysis) -> LintMetadataLookup {
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

    LintMetadataLookup {
        per_unit_root_index,
        root_index,
    }
}

fn open_sql_primary_key_fields_for_source(
    context: &ProjectLintContext<'_>,
    unit: &UnitAnalysis,
    query_scope: ScopeId,
    source: &SqlSourceData,
) -> Option<Vec<Arc<str>>> {
    let scope_index = *context.scope_indexes.get(unit.unit_id.as_usize())?;
    let (source_unit, structure_id) =
        open_sql_source_structure_for_name(context, unit, scope_index, query_scope, &source.name)?;
    if !unit_is_ddic_table_like_metadata(source_unit) {
        return None;
    }
    let fields = structure_field_infos_project(
        context,
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

fn open_sql_source_structure_for_name<'a>(
    context: &'a ProjectLintContext<'_>,
    unit: &'a UnitAnalysis,
    scope_index: &ScopeIndex,
    query_scope: ScopeId,
    name: &Arc<str>,
) -> Option<(&'a UnitAnalysis, StructureId)> {
    let handle = open_sql_source_symbol_handle(context, unit, scope_index, query_scope, name)?;
    let source_unit = &context.project.units[handle.unit.as_usize()];
    resolve_symbol_structure_project(
        context,
        source_unit,
        scope_for_unit(source_unit, query_scope),
        handle.symbol,
    )
}

fn open_sql_source_symbol_handle(
    context: &ProjectLintContext<'_>,
    unit: &UnitAnalysis,
    scope_index: &ScopeIndex,
    query_scope: ScopeId,
    name: &Arc<str>,
) -> Option<SymbolHandle> {
    if let Some(symbol_id) =
        resolve_symbol_in_lint_scope_chain(unit, scope_index, query_scope, Namespace::Type, name)
    {
        return Some(SymbolHandle {
            unit: unit.unit_id,
            symbol: symbol_id,
        });
    }

    root_symbol_handle_matching(context, unit, Namespace::Type, name, |symbol| {
        symbol.kind.occupies(Namespace::Type)
    })
}

fn resolve_symbol_structure_project<'a>(
    context: &'a ProjectLintContext<'_>,
    unit: &'a UnitAnalysis,
    scope: ScopeId,
    symbol_id: SymbolId,
) -> Option<(&'a UnitAnalysis, StructureId)> {
    let mut current_unit = unit;
    let mut current_symbol_id = symbol_id;
    let mut seen = HashSet::new();
    for _ in 0..8 {
        let symbol = current_unit.symbol(current_symbol_id);
        if let Some(structure_id) = symbol.structure {
            return Some((current_unit, structure_id));
        }
        let type_ref = symbol.declared_type.as_ref()?;
        let handle = resolve_type_like_symbol_handle(context, current_unit, scope, type_ref)?;
        if !seen.insert((handle.unit.0, handle.symbol.0)) {
            return None;
        }
        current_unit = &context.project.units[handle.unit.as_usize()];
        current_symbol_id = handle.symbol;
    }
    None
}

fn resolve_type_like_symbol_handle(
    context: &ProjectLintContext<'_>,
    unit: &UnitAnalysis,
    scope: ScopeId,
    type_ref: &FieldTypeRefData,
) -> Option<SymbolHandle> {
    let namespaces = if type_ref.namespace == Namespace::Value {
        [Namespace::Value, Namespace::Type]
    } else {
        [type_ref.namespace, type_ref.namespace]
    };
    let scope_index = *context.scope_indexes.get(unit.unit_id.as_usize())?;

    for namespace in namespaces {
        if let Some(symbol_id) = resolve_symbol_in_lint_scope_chain(
            unit,
            scope_index,
            scope,
            namespace,
            &type_ref.base_name,
        ) {
            return Some(SymbolHandle {
                unit: unit.unit_id,
                symbol: symbol_id,
            });
        }

        if let Some(handle) =
            root_symbol_handle_matching(context, unit, namespace, &type_ref.base_name, |symbol| {
                symbol.kind.namespaces().contains(&namespace)
            })
        {
            return Some(handle);
        }
    }

    None
}

fn root_symbol_handle_matching<F>(
    context: &ProjectLintContext<'_>,
    preferred_unit: &UnitAnalysis,
    namespace: Namespace,
    name: &Arc<str>,
    predicate: F,
) -> Option<SymbolHandle>
where
    F: Fn(&SymbolData) -> bool,
{
    let key = (namespace, Arc::clone(name));
    if let Some(symbol_ids) =
        context.lookup.per_unit_root_index[preferred_unit.unit_id.as_usize()].get(&key)
    {
        for &symbol_id in symbol_ids {
            let handle = SymbolHandle {
                unit: preferred_unit.unit_id,
                symbol: symbol_id,
            };
            if predicate(context.project.units[handle.unit.as_usize()].symbol(handle.symbol)) {
                return Some(handle);
            }
        }
    }

    context.lookup.root_index.get(&key).and_then(|handles| {
        handles.iter().copied().find(|handle| {
            handle.unit != preferred_unit.unit_id
                && predicate(context.project.units[handle.unit.as_usize()].symbol(handle.symbol))
        })
    })
}

fn resolve_symbol_in_lint_scope_chain(
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

fn structure_field_infos_project(
    context: &ProjectLintContext<'_>,
    current_unit: &UnitAnalysis,
    scope: ScopeId,
    structure_id: StructureId,
) -> Vec<StructureFieldInfo> {
    fn collect(
        context: &ProjectLintContext<'_>,
        current_unit: &UnitAnalysis,
        scope: ScopeId,
        structure_id: StructureId,
        seen_structures: &mut HashSet<(u32, u32)>,
        seen_fields: &mut HashSet<Arc<str>>,
        out: &mut Vec<StructureFieldInfo>,
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
            if !field_looks_like_ddic_proxy_include(current_unit, &field) {
                continue;
            }
            let Some((included_unit, included_structure)) =
                included_structure_for_proxy_field(context, current_unit, scope, &field)
            else {
                continue;
            };
            let nested_scope = scope_for_unit(included_unit, scope);
            collect(
                context,
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
        context,
        current_unit,
        scope,
        structure_id,
        &mut seen_structures,
        &mut seen_fields,
        &mut out,
    );
    out
}

fn included_structure_for_proxy_field<'a>(
    context: &'a ProjectLintContext<'_>,
    current_unit: &'a UnitAnalysis,
    scope: ScopeId,
    field: &StructureFieldInfo,
) -> Option<(&'a UnitAnalysis, StructureId)> {
    let type_ref = field.type_ref.as_ref()?;
    let handle = resolve_type_like_symbol_handle(
        context,
        current_unit,
        scope_for_unit(current_unit, scope),
        type_ref,
    )?;
    let resolved_unit = &context.project.units[handle.unit.as_usize()];
    resolve_symbol_structure_project(
        context,
        resolved_unit,
        scope_for_unit(resolved_unit, scope),
        handle.symbol,
    )
}

fn scope_for_unit(unit: &UnitAnalysis, scope: ScopeId) -> ScopeId {
    if unit.scopes.get(scope.as_usize()).is_some() {
        scope
    } else {
        unit.root_scope
    }
}

fn sql_source_matches_qualifier(source: &SqlSourceData, qualifier: &Arc<str>) -> bool {
    source.alias.as_ref() == Some(qualifier) || source.name == *qualifier
}

fn is_client_column_name(field_name: &str) -> bool {
    field_name.eq_ignore_ascii_case("mandt") || field_name.eq_ignore_ascii_case("client")
}

fn unit_is_ddic_table_like_metadata(unit: &UnitAnalysis) -> bool {
    let uri = unit.uri.to_ascii_lowercase().replace('\\', "/");
    if uri.contains("ddic-table-type") || uri.contains("/ddic/tabletypes/") {
        return false;
    }
    uri.contains("/ddic/tables/")
        || uri.contains("/ddic/database-tables/")
        || uri.contains("/ddic/views/")
        || uri.contains("/ddic-table/")
        || uri.contains("/ddic-view/")
        || uri.contains("/dictionary/database-tables/")
        || uri.contains("/dictionary/views/")
        || uri.contains("kind=ddic-table")
        || uri.contains("kind=ddic-view")
}

fn lint_for_all_entries_without_guard(
    unit: &UnitAnalysis,
    lint_policy: &LintPolicy,
    diagnostics: &mut Vec<LintDiagnostic>,
) {
    let Some(metadata) = abap_lints::metadata_for(ABAP_LSP_FOR_ALL_ENTRIES_WITHOUT_GUARD) else {
        return;
    };
    for query in &unit.sql_queries {
        let Some(clause_range) = query.for_all_entries_clause.as_ref() else {
            continue;
        };
        let Some(table_ref) = for_all_entries_table_reference(unit, clause_range) else {
            continue;
        };
        if has_for_all_entries_guard(unit, query.scope, query.range.start, table_ref) {
            continue;
        }
        emit_lint_diagnostic(
            diagnostics,
            metadata,
            clause_range.clone(),
            format!(
                "FOR ALL ENTRIES on '{}' is not guarded by an initial-table check",
                table_ref.name
            ),
            lint_policy,
        );
    }
}

fn lint_dynamic_open_sql(
    unit: &UnitAnalysis,
    lint_policy: &LintPolicy,
    diagnostics: &mut Vec<LintDiagnostic>,
) {
    let Some(metadata) = abap_lints::metadata_for(ABAP_LSP_DYNAMIC_OPEN_SQL) else {
        return;
    };
    for fragment in &unit.sql_dynamic_fragments {
        let fragment_kind = match fragment.kind {
            SqlDynamicFragmentKind::Source => "source",
            SqlDynamicFragmentKind::Projection => "projection",
            SqlDynamicFragmentKind::Where => "WHERE",
        };
        emit_lint_diagnostic(
            diagnostics,
            metadata,
            fragment.range.clone(),
            format!(
                "Open SQL uses a dynamic {fragment_kind} fragment that cannot be statically verified"
            ),
            lint_policy,
        );
    }
}

type SystemFieldUpdateKey = (ScopeId, usize, usize, usize);
type CallSiteKey = (ScopeId, usize, usize);

struct SystemFieldLintIndex<'a> {
    observed_subrc_updates: HashSet<SystemFieldUpdateKey>,
    call_function_sites: HashMap<CallSiteKey, &'a CallSiteData>,
}

impl<'a> SystemFieldLintIndex<'a> {
    fn new(unit: &'a UnitAnalysis) -> Self {
        let mut observed_subrc_updates = HashSet::new();
        for check in unit
            .value_state_checks
            .iter()
            .filter(|check| is_sy_subrc_check(check))
        {
            if let Some(update) = latest_subrc_update_before_check(unit, check) {
                observed_subrc_updates.insert(system_field_update_key(update));
            }
        }

        let mut call_function_sites = HashMap::new();
        for call_site in &unit.call_sites {
            if matches!(call_site.target, NamedArgumentTarget::Function { .. }) {
                call_function_sites
                    .entry(call_site_key(call_site.scope, &call_site.range))
                    .or_insert(call_site);
            }
        }

        Self {
            observed_subrc_updates,
            call_function_sites,
        }
    }
}

fn system_field_update_key(update: &SystemFieldUpdateData) -> SystemFieldUpdateKey {
    (
        update.scope,
        update.range.start,
        update.range.end,
        update.statement as usize,
    )
}

fn call_site_key(scope: ScopeId, range: &Range<usize>) -> CallSiteKey {
    (scope, range.start, range.end)
}

fn lint_ignored_authority_check(
    unit: &UnitAnalysis,
    system_field_index: &SystemFieldLintIndex<'_>,
    lint_policy: &LintPolicy,
    diagnostics: &mut Vec<LintDiagnostic>,
) {
    let Some(metadata) = abap_lints::metadata_for(ABAP_LSP_IGNORED_AUTHORITY_CHECK) else {
        return;
    };
    for update in &unit.system_field_updates {
        if update.statement != SystemFieldStatementKind::AuthorityCheck
            || !update.field_name.eq_ignore_ascii_case("subrc")
        {
            continue;
        }
        if system_field_update_result_is_observed(system_field_index, update) {
            continue;
        }
        emit_lint_diagnostic(
            diagnostics,
            metadata,
            update.range.clone(),
            "AUTHORITY-CHECK result is not checked via sy-subrc before it is overwritten"
                .to_string(),
            lint_policy,
        );
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CallFunctionResultEvidence {
    SySubrcOverwritten,
    ResultArgumentIgnored,
}

fn lint_ignored_call_function_result(
    unit: &UnitAnalysis,
    system_field_index: &SystemFieldLintIndex<'_>,
    source: &str,
    lint_policy: &LintPolicy,
    diagnostics: &mut Vec<LintDiagnostic>,
) {
    let Some(metadata) = abap_lints::metadata_for(ABAP_LSP_IGNORED_CALL_FUNCTION_RESULT) else {
        return;
    };
    for update in &unit.system_field_updates {
        if update.statement != SystemFieldStatementKind::CallFunction
            || !update.field_name.eq_ignore_ascii_case("subrc")
        {
            continue;
        }
        let Some(call_site) = call_function_site_for_update(system_field_index, update) else {
            continue;
        };
        if system_field_update_result_is_observed(system_field_index, update)
            || call_function_has_potentially_handled_result_argument(unit, call_site)
        {
            continue;
        }

        let evidence = if call_function_has_ignored_output_result(unit, call_site) {
            Some(CallFunctionResultEvidence::ResultArgumentIgnored)
        } else if call_function_has_nonzero_exception_mapping(call_site, source)
            && next_proven_subrc_update_after(unit, update).is_some()
        {
            Some(CallFunctionResultEvidence::SySubrcOverwritten)
        } else {
            None
        };
        let Some(evidence) = evidence else {
            continue;
        };
        let message = match evidence {
            CallFunctionResultEvidence::SySubrcOverwritten => {
                "CALL FUNCTION result in sy-subrc is not checked before it is overwritten"
                    .to_string()
            }
            CallFunctionResultEvidence::ResultArgumentIgnored => {
                "CALL FUNCTION output result is ignored and sy-subrc is not checked".to_string()
            }
        };
        emit_lint_diagnostic(
            diagnostics,
            metadata,
            update.range.clone(),
            message,
            lint_policy,
        );
    }
}

fn emit_lint_diagnostic(
    diagnostics: &mut Vec<LintDiagnostic>,
    metadata: &'static LintMetadata,
    range: Range<usize>,
    message: String,
    lint_policy: &LintPolicy,
) {
    if let Some(diagnostic) = lint_diagnostic_from_metadata(metadata, range, message, lint_policy) {
        diagnostics.push(diagnostic);
    }
}

fn lint_diagnostic_from_metadata(
    metadata: &'static LintMetadata,
    range: Range<usize>,
    message: String,
    lint_policy: &LintPolicy,
) -> Option<LintDiagnostic> {
    let level = lint_policy.level_for(metadata.id);
    let is_config_suppressed = !level.is_enabled();
    if is_config_suppressed && !lint_policy.report_suppressed() {
        return None;
    }
    let mut lint = LintDiagnostic {
        id: metadata.id.to_string(),
        range: range.clone(),
        message,
        level: if is_config_suppressed {
            LintLevel::Info
        } else {
            level
        },
        origin: metadata.origin,
        group: metadata.group,
        tags: metadata.tags.iter().map(|tag| (*tag).to_string()).collect(),
        sap_aliases: metadata
            .sap_aliases
            .iter()
            .map(|alias| (*alias).to_string())
            .collect(),
        suppressed: false,
        suppression: None,
    };
    if is_config_suppressed {
        mark_lint_suppressed(
            &mut lint,
            LintSuppression {
                kind: LintSuppressionKind::Config,
                range: range.start..range.start,
                token: "config".to_string(),
            },
        );
    }
    Some(lint)
}

fn enclosing_loop_kind_for_scope(unit: &UnitAnalysis, scope: ScopeId) -> Option<RoutineLoopKind> {
    unit.routine_control_regions.iter().find_map(|region| {
        let RoutineControlRegionData::Loop(loop_region) = region else {
            return None;
        };
        scope_descends_from(unit, scope, loop_region.body_scope).then_some(loop_region.kind)
    })
}

fn for_all_entries_table_reference<'a>(
    unit: &'a UnitAnalysis,
    clause_range: &Range<usize>,
) -> Option<&'a ReferenceData> {
    unit.references
        .iter()
        .filter(|reference| {
            reference.namespace == Namespace::Value
                && range_contains(clause_range, &reference.range)
                && !reference.name.as_ref().is_empty()
        })
        .min_by_key(|reference| (reference.range.start, reference.range.end))
}

fn has_for_all_entries_guard(
    unit: &UnitAnalysis,
    query_scope: ScopeId,
    query_start: usize,
    table_ref: &ReferenceData,
) -> bool {
    unit.value_state_checks.iter().any(|check| {
        if check.range.start > query_start
            || check.field_name.is_some()
            || !value_state_check_matches_reference(unit, check, table_ref)
        {
            return false;
        }
        non_initial_guard_scope_for_check(unit, check)
            .is_some_and(|guard_scope| scope_descends_from(unit, query_scope, guard_scope))
            || initial_exit_guard_scope_for_check(unit, check).is_some_and(
                |(guard_scope, guard_end)| {
                    guard_end <= query_start && scope_descends_from(unit, query_scope, guard_scope)
                },
            )
    })
}

fn value_state_check_matches_reference(
    unit: &UnitAnalysis,
    check: &ValueStateCheckData,
    reference: &ReferenceData,
) -> bool {
    if !check
        .symbol_name
        .eq_ignore_ascii_case(reference.name.as_ref())
    {
        return false;
    }
    let Some(check_reference) = reference_for_value_state_check(unit, check) else {
        return true;
    };
    match (check_reference.resolution, reference.resolution) {
        (Some(Resolution::Symbol(left)), Some(Resolution::Symbol(right))) => left == right,
        _ => true,
    }
}

fn reference_for_value_state_check<'a>(
    unit: &'a UnitAnalysis,
    check: &ValueStateCheckData,
) -> Option<&'a ReferenceData> {
    unit.references.iter().find(|reference| {
        reference.namespace == Namespace::Value
            && reference.scope == check.scope
            && reference.range == check.symbol_range
            && reference.name == check.symbol_name
    })
}

fn non_initial_guard_scope_for_check(
    unit: &UnitAnalysis,
    check: &ValueStateCheckData,
) -> Option<ScopeId> {
    match check.kind {
        ValueStateCheckKind::IsNotInitial => Some(check.scope),
        ValueStateCheckKind::IsInitial => explicit_else_scope_for_then_scope(unit, check.scope),
        ValueStateCheckKind::EqualsZero
        | ValueStateCheckKind::NotEqualsZero
        | ValueStateCheckKind::ConditionProbe => None,
    }
}

fn explicit_else_scope_for_then_scope(unit: &UnitAnalysis, then_scope: ScopeId) -> Option<ScopeId> {
    unit.routine_control_regions.iter().find_map(|region| {
        let RoutineControlRegionData::If(if_region) = region else {
            return None;
        };
        (if_region.then_scope == then_scope)
            .then_some(if_region.else_scope)
            .flatten()
    })
}

fn initial_exit_guard_scope_for_check(
    unit: &UnitAnalysis,
    check: &ValueStateCheckData,
) -> Option<(ScopeId, usize)> {
    if check.kind != ValueStateCheckKind::IsInitial {
        return None;
    }
    unit.routine_control_regions.iter().find_map(|region| {
        let RoutineControlRegionData::If(if_region) = region else {
            return None;
        };
        if if_region.then_scope != check.scope
            || !scope_has_direct_terminating_site(unit, if_region.then_scope)
        {
            return None;
        }
        Some((if_region.scope, if_region.range.end))
    })
}

fn scope_has_direct_terminating_site(unit: &UnitAnalysis, scope: ScopeId) -> bool {
    unit.routine_sites.iter().any(|site| {
        site.scope == scope
            && matches!(
                site.kind,
                RoutineSiteKind::Return
                    | RoutineSiteKind::Raise
                    | RoutineSiteKind::Leave
                    | RoutineSiteKind::LeaveListProcessing
                    | RoutineSiteKind::Stop
            )
    })
}

fn system_field_update_result_is_observed(
    system_field_index: &SystemFieldLintIndex<'_>,
    update: &SystemFieldUpdateData,
) -> bool {
    system_field_index
        .observed_subrc_updates
        .contains(&system_field_update_key(update))
}

fn call_function_site_for_update<'a>(
    system_field_index: &'a SystemFieldLintIndex<'a>,
    update: &SystemFieldUpdateData,
) -> Option<&'a CallSiteData> {
    system_field_index
        .call_function_sites
        .get(&call_site_key(update.scope, &update.range))
        .copied()
}

fn call_function_has_potentially_handled_result_argument(
    unit: &UnitAnalysis,
    call_site: &CallSiteData,
) -> bool {
    call_site
        .arguments
        .iter()
        .any(|argument| match argument.section {
            Some(NamedArgumentSection::Changing) => true,
            Some(NamedArgumentSection::Importing | NamedArgumentSection::Tables) => {
                !call_function_output_argument_is_proven_ignored(unit, call_site, argument)
            }
            _ => false,
        })
}

fn call_function_has_ignored_output_result(unit: &UnitAnalysis, call_site: &CallSiteData) -> bool {
    let mut saw_output_argument = false;
    for argument in call_site.arguments.iter().filter(|argument| {
        matches!(
            argument.section,
            Some(NamedArgumentSection::Importing | NamedArgumentSection::Tables)
        )
    }) {
        saw_output_argument = true;
        if !call_function_output_argument_is_proven_ignored(unit, call_site, argument) {
            return false;
        }
    }
    saw_output_argument
}

fn call_function_output_argument_is_proven_ignored(
    unit: &UnitAnalysis,
    call_site: &CallSiteData,
    argument: &CallArgumentData,
) -> bool {
    if unit.diagnostics.iter().any(|diagnostic| {
        diagnostic.kind == DiagnosticKind::DeadStore
            && range_contains(&argument.range, &diagnostic.range)
    }) {
        return true;
    }
    let Some(symbol) = call_function_output_argument_local_symbol(unit, call_site, argument) else {
        return false;
    };
    !unit.references.iter().any(|reference| {
        reference.namespace == Namespace::Value
            && reference.range.start >= call_site.range.end
            && reference_targets_symbol(reference, symbol)
            && scopes_may_share_sequential_flow(unit, call_site.scope, reference.scope)
    })
}

fn call_function_output_argument_local_symbol(
    unit: &UnitAnalysis,
    call_site: &CallSiteData,
    argument: &CallArgumentData,
) -> Option<SymbolHandle> {
    let mut matches = unit.references.iter().filter_map(|reference| {
        if reference.namespace != Namespace::Value
            || !range_contains(&argument.range, &reference.range)
        {
            return None;
        }
        let Some(Resolution::Symbol(symbol)) = reference.resolution else {
            return None;
        };
        if symbol.unit != unit.unit_id
            || !scopes_may_share_sequential_flow(unit, call_site.scope, reference.scope)
        {
            return None;
        }
        let symbol_data = unit.symbol(symbol.symbol);
        let symbol_scope = unit.scopes.get(symbol_data.scope.as_usize())?;
        (symbol_scope.kind != ScopeKind::File).then_some(symbol)
    });
    let first = matches.next()?;
    matches.next().is_none().then_some(first)
}

fn reference_targets_symbol(reference: &ReferenceData, symbol: SymbolHandle) -> bool {
    matches!(reference.resolution, Some(Resolution::Symbol(target)) if target == symbol)
}

fn scopes_may_share_sequential_flow(unit: &UnitAnalysis, left: ScopeId, right: ScopeId) -> bool {
    left == right
        || scope_descends_from(unit, left, right)
        || scope_descends_from(unit, right, left)
}

fn call_function_has_nonzero_exception_mapping(call_site: &CallSiteData, source: &str) -> bool {
    call_site.arguments.iter().any(|argument| {
        argument.section == Some(NamedArgumentSection::Exceptions)
            && source
                .get(argument.range.clone())
                .is_some_and(exception_mapping_value_is_nonzero_literal)
    })
}

fn exception_mapping_value_is_nonzero_literal(value: &str) -> bool {
    let value = value
        .rsplit_once('=')
        .map(|(_, value)| value)
        .unwrap_or(value)
        .trim();
    !value.is_empty()
        && value.chars().all(|ch| ch.is_ascii_digit())
        && value.chars().any(|ch| ch != '0')
}

fn next_proven_subrc_update_after<'a>(
    unit: &'a UnitAnalysis,
    update: &SystemFieldUpdateData,
) -> Option<&'a SystemFieldUpdateData> {
    unit.system_field_updates
        .iter()
        .filter(|candidate| {
            candidate.field_name.eq_ignore_ascii_case("subrc")
                && !same_system_field_update(candidate, update)
                && candidate.range.start >= update.range.end
                && subrc_update_is_proven_later_on_same_flow(unit, update, candidate)
        })
        .min_by_key(|candidate| (candidate.range.start, candidate.range.end))
}

fn subrc_update_is_proven_later_on_same_flow(
    unit: &UnitAnalysis,
    earlier: &SystemFieldUpdateData,
    later: &SystemFieldUpdateData,
) -> bool {
    earlier.scope == later.scope || scope_descends_from(unit, earlier.scope, later.scope)
}

fn latest_subrc_update_before_check<'a>(
    unit: &'a UnitAnalysis,
    check: &ValueStateCheckData,
) -> Option<&'a SystemFieldUpdateData> {
    unit.system_field_updates
        .iter()
        .filter(|update| {
            update.field_name.eq_ignore_ascii_case("subrc")
                && update.range.end <= check.range.start
                && scope_descends_from(unit, check.scope, update.scope)
        })
        .max_by_key(|update| (update.range.end, update.range.start))
}

fn is_sy_subrc_check(check: &ValueStateCheckData) -> bool {
    check
        .field_name
        .as_ref()
        .is_some_and(|field_name| field_name.eq_ignore_ascii_case("subrc"))
        && (check.symbol_name.eq_ignore_ascii_case("sy")
            || check.symbol_name.eq_ignore_ascii_case("syst"))
}

fn same_system_field_update(left: &SystemFieldUpdateData, right: &SystemFieldUpdateData) -> bool {
    left.scope == right.scope
        && left.range == right.range
        && left.statement == right.statement
        && left
            .field_name
            .eq_ignore_ascii_case(right.field_name.as_ref())
}

fn scope_descends_from(unit: &UnitAnalysis, scope: ScopeId, ancestor: ScopeId) -> bool {
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

fn range_contains(outer: &Range<usize>, inner: &Range<usize>) -> bool {
    outer.start <= inner.start && inner.end <= outer.end
}

fn lint_diagnostic_from_symbol_diagnostic(
    diagnostic: &Diagnostic,
    lint_policy: &LintPolicy,
) -> Option<LintDiagnostic> {
    let metadata = lint_metadata_for_diagnostic_kind(diagnostic.kind)?;
    let level = lint_policy.level_for(metadata.id);
    let is_config_suppressed = !level.is_enabled();
    if is_config_suppressed && !lint_policy.report_suppressed() {
        return None;
    }
    let mut lint = LintDiagnostic {
        id: metadata.id.to_string(),
        range: diagnostic.range.clone(),
        message: diagnostic.message.clone(),
        level: if is_config_suppressed {
            LintLevel::Info
        } else {
            level
        },
        origin: metadata.origin,
        group: metadata.group,
        tags: metadata.tags.iter().map(|tag| (*tag).to_string()).collect(),
        sap_aliases: metadata
            .sap_aliases
            .iter()
            .map(|alias| (*alias).to_string())
            .collect(),
        suppressed: false,
        suppression: None,
    };
    if is_config_suppressed {
        mark_lint_suppressed(
            &mut lint,
            LintSuppression {
                kind: LintSuppressionKind::Config,
                range: diagnostic.range.start..diagnostic.range.start,
                token: "config".to_string(),
            },
        );
    }
    Some(lint)
}

fn mark_lint_suppressed(lint: &mut LintDiagnostic, suppression: LintSuppression) {
    lint.suppressed = true;
    lint.suppression = Some(suppression);
    lint.level = LintLevel::Info;
    if !lint.tags.iter().any(|tag| tag == "suppressed") {
        lint.tags.push("suppressed".to_string());
    }
}

fn analyze_inputs_with_progress(
    inputs: &[DocumentInput],
    existing: Option<&HashMap<Arc<str>, Arc<AnalysisSnapshot>>>,
    previous_analysis: Option<&CachedWorkspaceAnalysis>,
    progress: Option<&(dyn Fn(usize, usize) + Sync)>,
    changed_uris: &HashSet<Arc<str>>,
    force_full: bool,
    build_plan: SnapshotBuildPlan,
    lint_policy: Arc<LintPolicy>,
) -> (
    HashMap<Arc<str>, Arc<AnalysisSnapshot>>,
    CachedWorkspaceAnalysis,
) {
    let build_plan = build_plan.normalized();
    let (prepared, mut metrics) =
        prepare_documents(inputs, existing, previous_analysis, progress, build_plan);
    let diagnostic_scope_roots = diagnostic_scope_roots_for_build_plan(&prepared, build_plan);
    let locals: Vec<_> = prepared
        .iter()
        .map(|prepared| prepared.local.clone())
        .collect();
    let previous_project = existing
        .and_then(|existing| existing.values().next())
        .map(|snapshot| snapshot.project.as_ref());
    let previous_locals = previous_analysis.map(|analysis| &analysis.locals);
    let update_timer = std::time::Instant::now();
    let update = incremental_project_update(
        previous_project,
        previous_locals,
        locals,
        changed_uris,
        force_full,
        diagnostic_scope_roots.as_ref(),
    );
    metrics.project_update_micros = update_timer.elapsed().as_micros();
    metrics.full_rebuild = update.full_rebuild;
    metrics.unit_count = update.unit_count;
    metrics.dirty_unit_count = update.dirty_unit_count;
    metrics.diagnostic_scope_unit_count = update.diagnostic_scope_unit_count;
    metrics.validation_unit_count = update.validation_unit_count;
    metrics.scope_index_clone_micros = update.scope_index_clone_micros;
    metrics.build_workspace_index_micros = update.build_workspace_index_micros;
    metrics.compute_dirty_set_micros = update.compute_dirty_set_micros;
    metrics.clone_previous_units_micros = update.clone_previous_units_micros;
    metrics.apply_local_updates_micros = update.apply_local_updates_micros;
    metrics.resolve_include_edges_micros = update.resolve_include_edges_micros;
    metrics.resolve_cross_unit_micros = update.resolve_cross_unit_micros;
    metrics.infer_semantic_facts_micros = update.infer_semantic_facts_micros;
    metrics.rebuild_semantic_index_micros = update.rebuild_semantic_index_micros;
    metrics.validate_micros = update.validate_micros;
    metrics.collect_project_diagnostics_micros = update.collect_project_diagnostics_micros;
    let (snapshots, mut analysis) =
        materialize_snapshots(prepared, update, build_plan, lint_policy);
    analysis.metrics.parse_count = metrics.parse_count;
    analysis.metrics.local_phase_count = metrics.local_phase_count;
    analysis.metrics.parse_micros = metrics.parse_micros;
    analysis.metrics.local_phase_micros = metrics.local_phase_micros;
    analysis.metrics.dependency_projection_micros = metrics.dependency_projection_micros;
    analysis.metrics.parse_work_micros = metrics.parse_work_micros;
    analysis.metrics.local_phase_work_micros = metrics.local_phase_work_micros;
    analysis.metrics.project_update_micros = metrics.project_update_micros;
    analysis.metrics.full_rebuild = metrics.full_rebuild;
    analysis.metrics.unit_count = metrics.unit_count;
    analysis.metrics.dirty_unit_count = metrics.dirty_unit_count;
    analysis.metrics.diagnostic_scope_unit_count = metrics.diagnostic_scope_unit_count;
    analysis.metrics.validation_unit_count = metrics.validation_unit_count;
    analysis.metrics.scope_index_clone_micros = metrics.scope_index_clone_micros;
    analysis.metrics.build_workspace_index_micros = metrics.build_workspace_index_micros;
    analysis.metrics.compute_dirty_set_micros = metrics.compute_dirty_set_micros;
    analysis.metrics.clone_previous_units_micros = metrics.clone_previous_units_micros;
    analysis.metrics.apply_local_updates_micros = metrics.apply_local_updates_micros;
    analysis.metrics.resolve_include_edges_micros = metrics.resolve_include_edges_micros;
    analysis.metrics.resolve_cross_unit_micros = metrics.resolve_cross_unit_micros;
    analysis.metrics.infer_semantic_facts_micros = metrics.infer_semantic_facts_micros;
    analysis.metrics.rebuild_semantic_index_micros = metrics.rebuild_semantic_index_micros;
    analysis.metrics.validate_micros = metrics.validate_micros;
    analysis.metrics.collect_project_diagnostics_micros =
        metrics.collect_project_diagnostics_micros;
    (snapshots, analysis)
}

fn force_full_rebuild(
    previous_analysis: Option<&CachedWorkspaceAnalysis>,
    inputs: &[DocumentInput],
) -> bool {
    let Some(previous_analysis) = previous_analysis else {
        return true;
    };
    if inputs.len() < previous_analysis.uri_order.len() {
        return true;
    }
    previous_analysis
        .uri_order
        .iter()
        .zip(inputs)
        .any(|(uri, input)| uri.as_ref() != input.uri.as_ref())
}

fn changed_uris_for_inputs(
    inputs: &[DocumentInput],
    existing: Option<&HashMap<Arc<str>, Arc<AnalysisSnapshot>>>,
) -> HashSet<Arc<str>> {
    inputs
        .iter()
        .filter(|input| {
            existing
                .and_then(|existing| existing.get(input.uri.as_ref()))
                .is_none_or(|snapshot| !snapshot_matches_input(snapshot, input))
        })
        .map(|input| Arc::clone(&input.uri))
        .collect()
}

fn document_inputs_for_publish(
    existing: &HashMap<Arc<str>, Arc<AnalysisSnapshot>>,
    analysis: Option<&CachedWorkspaceAnalysis>,
    input: &DocumentInput,
) -> Vec<DocumentInput> {
    let mut out = Vec::with_capacity(
        existing.len() + usize::from(!existing.contains_key(input.uri.as_ref())),
    );
    let mut seen = HashSet::new();
    for uri in current_uri_order(existing, analysis) {
        if uri.as_ref() == input.uri.as_ref() {
            out.push(input.clone());
            seen.insert(Arc::clone(&uri));
            continue;
        }
        let Some(snapshot) = existing.get(uri.as_ref()) else {
            continue;
        };
        out.push(DocumentInput {
            uri: Arc::clone(&snapshot.uri),
            version: snapshot.version,
            text: Arc::clone(&snapshot.text),
            is_dependency: snapshot.is_dependency,
            object_name: snapshot.object_name.clone(),
        });
        seen.insert(Arc::clone(&snapshot.uri));
    }
    if !seen.contains(input.uri.as_ref()) {
        out.push(input.clone());
    }
    out
}

fn document_inputs_for_publish_many(
    existing: &HashMap<Arc<str>, Arc<AnalysisSnapshot>>,
    analysis: Option<&CachedWorkspaceAnalysis>,
    inputs: &[DocumentInput],
) -> Vec<DocumentInput> {
    let input_by_uri: HashMap<_, _> = inputs
        .iter()
        .map(|input| (Arc::clone(&input.uri), input))
        .collect();
    let mut out = Vec::with_capacity(
        existing.len()
            + inputs
                .iter()
                .filter(|input| !existing.contains_key(input.uri.as_ref()))
                .count(),
    );
    let mut seen = HashSet::new();
    for uri in current_uri_order(existing, analysis) {
        if let Some(input) = input_by_uri.get(&uri) {
            out.push((*input).clone());
            seen.insert(Arc::clone(&uri));
            continue;
        }
        let Some(snapshot) = existing.get(uri.as_ref()) else {
            continue;
        };
        out.push(DocumentInput {
            uri: Arc::clone(&snapshot.uri),
            version: snapshot.version,
            text: Arc::clone(&snapshot.text),
            is_dependency: snapshot.is_dependency,
            object_name: snapshot.object_name.clone(),
        });
        seen.insert(Arc::clone(&snapshot.uri));
    }

    let mut remaining: Vec<_> = inputs
        .iter()
        .filter(|input| !seen.contains(input.uri.as_ref()))
        .cloned()
        .collect();
    remaining.sort_by(|left, right| left.uri.cmp(&right.uri));
    out.extend(remaining);
    out
}

fn analysis_text_for_input(input: &DocumentInput) -> Arc<str> {
    analysis_text_for_document(input.text.as_ref(), input.is_dependency)
}

pub fn analysis_text_for_document(text: &str, is_dependency: bool) -> Arc<str> {
    if is_dependency {
        return dependency_surface_text(text);
    }
    opened_function_module_dependency_analysis_text(text).unwrap_or_else(|| Arc::from(text))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DependencyVisibility {
    Public,
    Protected,
    Private,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DependencyBlock {
    ClassDefinition { visibility: DependencyVisibility },
    ClassImplementation,
    Method,
    Form,
    Function,
}

const BEGIN_FUNCTION_MODULE_MARKER: &str = "* >>> BEGIN FUNCTION MODULE ";
const END_FUNCTION_MODULE_MARKER: &str = "* <<< END FUNCTION MODULE ";

fn dependency_block_for_keywords(keywords: &[String]) -> Option<DependencyBlock> {
    let first = keywords.first().map(String::as_str);
    let second = keywords.get(1).map(String::as_str);
    match (first, second) {
        (Some("form"), _) => Some(DependencyBlock::Form),
        (Some("function"), Some("pool")) => None,
        (Some("function"), _) => Some(DependencyBlock::Function),
        _ => None,
    }
}

fn dependency_surface_text(text: &str) -> Arc<str> {
    let tokenized = tokenize(text);
    let mut projected = text.as_bytes().to_vec();
    let tokens = tokenized.tokens.as_ref();
    let mut stack = Vec::<DependencyBlock>::new();
    let mut idx = 0usize;

    while idx < tokens.len() {
        while idx < tokens.len() && tokens[idx].kind == TokenKind::Comment {
            idx += 1;
        }
        if idx >= tokens.len() {
            break;
        }

        let Some(period_idx) = tokens[idx..]
            .iter()
            .position(|token| token.kind == TokenKind::Period)
            .map(|offset| idx + offset)
        else {
            break;
        };

        let keywords = statement_keywords(tokens, text, idx, period_idx);
        let first = keywords.first().map(String::as_str);
        let second = keywords.get(1).map(String::as_str);
        let statement_range = tokens[idx].range.start..tokens[period_idx].range.end;

        match stack.last_mut() {
            Some(DependencyBlock::Method) => {
                if first == Some("endmethod") {
                    stack.pop();
                } else if !dependency_surface_keeps_statement(&keywords) {
                    blank_range_preserving_layout(&mut projected, statement_range);
                }
                idx = period_idx + 1;
                continue;
            }
            Some(DependencyBlock::Form) => {
                if first == Some("endform") {
                    stack.pop();
                } else if !dependency_surface_keeps_statement(&keywords) {
                    blank_range_preserving_layout(&mut projected, statement_range);
                }
                idx = period_idx + 1;
                continue;
            }
            Some(DependencyBlock::Function) => {
                if first == Some("endfunction") {
                    stack.pop();
                } else if !dependency_surface_keeps_statement(&keywords) {
                    blank_range_preserving_layout(&mut projected, statement_range);
                }
                idx = period_idx + 1;
                continue;
            }
            Some(DependencyBlock::ClassImplementation) => {
                match first {
                    Some("method") => stack.push(DependencyBlock::Method),
                    Some("endclass") => {
                        stack.pop();
                    }
                    Some("include") if dependency_surface_keeps_statement(&keywords) => {}
                    _ => {
                        blank_range_preserving_layout(&mut projected, statement_range);
                    }
                }
                idx = period_idx + 1;
                continue;
            }
            Some(DependencyBlock::ClassDefinition { visibility }) => {
                if first == Some("endclass") {
                    stack.pop();
                    idx = period_idx + 1;
                    continue;
                }

                if matches!(first, Some("public" | "protected" | "private"))
                    && second == Some("section")
                {
                    *visibility = match first.expect("section keyword") {
                        "public" => DependencyVisibility::Public,
                        "protected" => DependencyVisibility::Protected,
                        _ => DependencyVisibility::Private,
                    };
                    if *visibility == DependencyVisibility::Private {
                        blank_range_preserving_layout(&mut projected, statement_range);
                    }
                    idx = period_idx + 1;
                    continue;
                }

                if *visibility == DependencyVisibility::Private {
                    blank_range_preserving_layout(&mut projected, statement_range.clone());
                }

                if let Some(block) = dependency_class_block_for_keywords(&keywords) {
                    stack.push(block);
                } else if let Some(block) = dependency_block_for_keywords(&keywords) {
                    stack.push(block);
                }

                idx = period_idx + 1;
                continue;
            }
            None => {}
        }

        match first {
            Some("class") => {
                if let Some(block) = dependency_class_block_for_keywords(&keywords) {
                    stack.push(block);
                }
            }
            _ => {
                if let Some(block) = dependency_block_for_keywords(&keywords) {
                    stack.push(block);
                }
            }
        }

        idx = period_idx + 1;
    }

    restore_function_signature_statements(tokens, text, &mut projected);

    Arc::from(
        String::from_utf8(projected).expect("dependency surface projection should stay utf-8"),
    )
}

fn restore_function_signature_statements(
    tokens: &[abap_lexer::Token],
    text: &str,
    projected: &mut [u8],
) {
    let original = text.as_bytes();
    let mut idx = 0usize;

    while idx < tokens.len() {
        while idx < tokens.len() && tokens[idx].kind == TokenKind::Comment {
            idx += 1;
        }
        if idx >= tokens.len() {
            break;
        }

        let Some(period_idx) = tokens[idx..]
            .iter()
            .position(|token| token.kind == TokenKind::Period)
            .map(|offset| idx + offset)
        else {
            break;
        };

        let keywords = statement_keywords(tokens, text, idx, period_idx);
        if keywords.first().map(String::as_str) == Some("function")
            && keywords.get(1).map(String::as_str) != Some("pool")
        {
            let statement_range = tokens[idx].range.start..tokens[period_idx].range.end;
            projected[statement_range.clone()].copy_from_slice(&original[statement_range]);
        }

        idx = period_idx + 1;
    }
}

fn opened_function_module_dependency_analysis_text(text: &str) -> Option<Arc<str>> {
    if !text.contains(BEGIN_FUNCTION_MODULE_MARKER) || !text.contains(END_FUNCTION_MODULE_MARKER) {
        return None;
    }

    let projected_text = dependency_surface_text(text);
    let mut projected = projected_text.as_bytes().to_vec();
    restore_function_module_blocks(text.as_bytes(), &mut projected);
    Some(Arc::from(String::from_utf8(projected).expect(
        "function module dependency projection should stay utf-8",
    )))
}

fn restore_function_module_blocks(original: &[u8], projected: &mut [u8]) {
    let Ok(text) = std::str::from_utf8(original) else {
        return;
    };
    let mut search_from = 0usize;
    while let Some(marker_offset) = text[search_from..].find(BEGIN_FUNCTION_MODULE_MARKER) {
        let marker_start = search_from + marker_offset;
        let line_start = text[..marker_start]
            .rfind('\n')
            .map(|idx| idx + 1)
            .unwrap_or(0);
        let after_marker = marker_start + BEGIN_FUNCTION_MODULE_MARKER.len();
        let Some(end_marker_rel) = text[after_marker..].find(END_FUNCTION_MODULE_MARKER) else {
            break;
        };
        let end_marker_start = after_marker + end_marker_rel;
        let line_end = text[end_marker_start..]
            .find('\n')
            .map(|idx| end_marker_start + idx + 1)
            .unwrap_or(text.len());
        projected[line_start..line_end].copy_from_slice(&original[line_start..line_end]);
        search_from = line_end;
    }
}

fn has_matching_input_set(
    existing: &HashMap<Arc<str>, Arc<AnalysisSnapshot>>,
    inputs: &[DocumentInput],
) -> bool {
    existing.len() == inputs.len()
        && inputs
            .iter()
            .all(|input| existing.contains_key(input.uri.as_ref()))
}

fn local_analysis_with_object_name(
    mut local: LocalAnalysis,
    object_name: Option<&Arc<str>>,
) -> LocalAnalysis {
    if let Some(object_name) = object_name {
        local.unit.provided_names.push(Arc::clone(object_name));
        local.unit.provided_names.sort();
        local.unit.provided_names.dedup();
    }
    local
}

fn dependency_class_block_for_keywords(keywords: &[String]) -> Option<DependencyBlock> {
    if keywords.first().map(String::as_str) != Some("class") {
        return None;
    }
    let second = keywords.get(1).map(String::as_str);
    let third = keywords.get(2).map(String::as_str);
    if matches!(second, Some("methods" | "data" | "events")) {
        return None;
    }
    if third == Some("implementation") {
        return Some(DependencyBlock::ClassImplementation);
    }
    if third == Some("definition") {
        if keywords
            .iter()
            .any(|keyword| matches!(keyword.as_str(), "load" | "deferred"))
        {
            return None;
        }
        return Some(DependencyBlock::ClassDefinition {
            visibility: DependencyVisibility::Private,
        });
    }
    None
}

fn dependency_surface_keeps_statement(keywords: &[String]) -> bool {
    matches!(keywords.first().map(String::as_str), Some("include"))
        && !matches!(
            keywords.get(1).map(String::as_str),
            Some("type" | "structure")
        )
}

fn statement_keywords(
    tokens: &[abap_lexer::Token],
    text: &str,
    start: usize,
    period_idx: usize,
) -> Vec<String> {
    tokens[start..period_idx]
        .iter()
        .filter(|token| token.kind == TokenKind::Ident)
        .map(|token| token.lexeme(text).to_ascii_lowercase())
        .collect()
}

fn blank_range_preserving_layout(text: &mut [u8], range: Range<usize>) {
    for byte in &mut text[range] {
        if *byte != b'\n' && *byte != b'\r' {
            *byte = b' ';
        }
    }
}

impl DocumentStore {
    pub fn set_lint_policy(&self, lint_policy: LintPolicy) {
        *self.lint_policy.write() = Arc::new(lint_policy);
    }

    pub fn lint_policy(&self) -> Arc<LintPolicy> {
        Arc::clone(&self.lint_policy.read())
    }

    pub fn replace_all(
        &self,
        inputs: Vec<DocumentInput>,
    ) -> HashMap<Arc<str>, Arc<AnalysisSnapshot>> {
        self.replace_all_with_progress(inputs, None)
    }

    pub fn replace_all_with_progress(
        &self,
        inputs: Vec<DocumentInput>,
        progress: Option<&(dyn Fn(usize, usize) + Sync)>,
    ) -> HashMap<Arc<str>, Arc<AnalysisSnapshot>> {
        self.replace_all_with_build_plan_and_progress(inputs, SnapshotBuildPlan::FULL, progress)
    }

    pub fn replace_all_with_build_plan(
        &self,
        inputs: Vec<DocumentInput>,
        build_plan: SnapshotBuildPlan,
    ) -> HashMap<Arc<str>, Arc<AnalysisSnapshot>> {
        self.replace_all_with_build_plan_and_progress(inputs, build_plan, None)
    }

    pub fn replace_all_with_build_plan_and_progress(
        &self,
        inputs: Vec<DocumentInput>,
        build_plan: SnapshotBuildPlan,
        progress: Option<&(dyn Fn(usize, usize) + Sync)>,
    ) -> HashMap<Arc<str>, Arc<AnalysisSnapshot>> {
        let existing = self.documents.read();
        let analysis = self.analysis.read();
        let lint_policy = self.lint_policy();
        let changed_uris = changed_uris_for_inputs(&inputs, Some(&existing));
        let (rebuilt, rebuilt_analysis) = analyze_inputs_with_progress(
            &inputs,
            has_matching_input_set(&existing, &inputs).then_some(&existing),
            analysis.as_ref(),
            progress,
            &changed_uris,
            true,
            build_plan,
            Arc::clone(&lint_policy),
        );
        drop(analysis);
        drop(existing);
        self.documents.write().clone_from(&rebuilt);
        *self.analysis.write() = Some(rebuilt_analysis);
        *self.analysis_revision.write() += 1;
        rebuilt
    }

    pub fn publish(
        &self,
        uri: impl Into<Arc<str>>,
        version: i32,
        text: &str,
    ) -> Arc<AnalysisSnapshot> {
        self.publish_input(DocumentInput {
            uri: uri.into(),
            version,
            text: Arc::from(text),
            is_dependency: false,
            object_name: None,
        })
    }

    pub fn publish_input(&self, input: DocumentInput) -> Arc<AnalysisSnapshot> {
        self.publish_input_with_build_plan(input, SnapshotBuildPlan::FULL)
    }

    pub fn publish_input_with_build_plan(
        &self,
        input: DocumentInput,
        build_plan: SnapshotBuildPlan,
    ) -> Arc<AnalysisSnapshot> {
        let existing = self.documents.read();
        let analysis = self.analysis.read();
        let lint_policy = self.lint_policy();
        if let Some(current) = existing.get(input.uri.as_ref())
            && current.text.as_ref() == input.text.as_ref()
            && current.is_dependency == input.is_dependency
            && current.object_name == input.object_name
            && build_plan_matches_cached_analysis(
                analysis.as_ref(),
                build_plan,
                lint_policy.as_ref(),
            )
        {
            let snapshot = clone_snapshot_with_version(current, input.version);
            drop(existing);
            drop(analysis);
            self.documents
                .write()
                .insert(Arc::clone(&input.uri), Arc::clone(&snapshot));
            return snapshot;
        }
        let inputs = document_inputs_for_publish(&existing, analysis.as_ref(), &input);
        let force_full = force_full_rebuild(analysis.as_ref(), &inputs);
        let changed_uris = HashSet::from([Arc::clone(&input.uri)]);
        let (rebuilt, rebuilt_analysis) = analyze_inputs_with_progress(
            &inputs,
            Some(&existing),
            analysis.as_ref(),
            None,
            &changed_uris,
            force_full,
            build_plan,
            Arc::clone(&lint_policy),
        );
        drop(analysis);
        drop(existing);
        self.documents.write().clone_from(&rebuilt);
        *self.analysis.write() = Some(rebuilt_analysis);
        *self.analysis_revision.write() += 1;
        rebuilt
            .get(input.uri.as_ref())
            .cloned()
            .expect("published snapshot should exist")
    }

    pub fn publish_inputs(
        &self,
        inputs: Vec<DocumentInput>,
    ) -> HashMap<Arc<str>, Arc<AnalysisSnapshot>> {
        self.publish_inputs_with_progress(inputs, None)
    }

    pub fn publish_inputs_with_progress(
        &self,
        inputs: Vec<DocumentInput>,
        progress: Option<&(dyn Fn(usize, usize) + Sync)>,
    ) -> HashMap<Arc<str>, Arc<AnalysisSnapshot>> {
        self.publish_inputs_with_build_plan_and_progress(inputs, SnapshotBuildPlan::FULL, progress)
    }

    pub fn publish_inputs_with_build_plan(
        &self,
        inputs: Vec<DocumentInput>,
        build_plan: SnapshotBuildPlan,
    ) -> HashMap<Arc<str>, Arc<AnalysisSnapshot>> {
        self.publish_inputs_with_build_plan_and_progress(inputs, build_plan, None)
    }

    pub fn publish_inputs_with_build_plan_and_progress(
        &self,
        inputs: Vec<DocumentInput>,
        build_plan: SnapshotBuildPlan,
        progress: Option<&(dyn Fn(usize, usize) + Sync)>,
    ) -> HashMap<Arc<str>, Arc<AnalysisSnapshot>> {
        if inputs.is_empty() {
            return self.documents.read().clone();
        }

        let existing = self.documents.read();
        let analysis = self.analysis.read();
        let lint_policy = self.lint_policy();
        let changed_uris: HashSet<_> = inputs
            .iter()
            .filter(|input| {
                existing
                    .get(input.uri.as_ref())
                    .is_none_or(|snapshot| !snapshot_matches_input(snapshot, input))
            })
            .map(|input| Arc::clone(&input.uri))
            .collect();
        if changed_uris.is_empty()
            && build_plan_matches_cached_analysis(
                analysis.as_ref(),
                build_plan,
                lint_policy.as_ref(),
            )
        {
            let mut updated = existing.clone();
            for input in &inputs {
                let Some(current) = existing.get(input.uri.as_ref()) else {
                    continue;
                };
                updated.insert(
                    Arc::clone(&input.uri),
                    clone_snapshot_with_version(current, input.version),
                );
            }
            drop(analysis);
            drop(existing);
            self.documents.write().clone_from(&updated);
            return updated;
        }

        let merged_inputs = document_inputs_for_publish_many(&existing, analysis.as_ref(), &inputs);
        let force_full = force_full_rebuild(analysis.as_ref(), &merged_inputs);
        let (rebuilt, rebuilt_analysis) = analyze_inputs_with_progress(
            &merged_inputs,
            Some(&existing),
            analysis.as_ref(),
            progress,
            &changed_uris,
            force_full,
            build_plan,
            Arc::clone(&lint_policy),
        );
        drop(analysis);
        drop(existing);
        self.documents.write().clone_from(&rebuilt);
        *self.analysis.write() = Some(rebuilt_analysis);
        *self.analysis_revision.write() += 1;
        rebuilt
    }

    pub fn preview_publish_input(&self, input: DocumentInput) -> Arc<AnalysisSnapshot> {
        let started = std::time::Instant::now();
        let existing = self.documents.read();
        let analysis = self.analysis.read();
        let lint_policy = self.lint_policy();
        if let Some(current) = existing.get(input.uri.as_ref())
            && current.text.as_ref() == input.text.as_ref()
            && current.is_dependency == input.is_dependency
            && current.object_name == input.object_name
        {
            let snapshot = clone_snapshot_with_version(current, input.version);
            *self.preview_metrics.write() = Some(PreviewMetrics {
                parse_count: 0,
                local_phase_count: 0,
                build_micros: started.elapsed().as_micros(),
                committed_context_only: false,
                fell_back_to_single_document: false,
            });
            return snapshot;
        }
        let build_plan = analysis
            .as_ref()
            .map(|analysis| analysis.build_plan)
            .unwrap_or(SnapshotBuildPlan::FULL);
        let inputs = document_inputs_for_publish(&existing, analysis.as_ref(), &input);
        let force_full = force_full_rebuild(analysis.as_ref(), &inputs);
        let changed_uris = HashSet::from([Arc::clone(&input.uri)]);
        let (rebuilt, rebuilt_analysis) = analyze_inputs_with_progress(
            &inputs,
            Some(&existing),
            analysis.as_ref(),
            None,
            &changed_uris,
            force_full,
            build_plan,
            lint_policy,
        );
        let snapshot = rebuilt
            .get(input.uri.as_ref())
            .cloned()
            .expect("preview snapshot should exist");
        *self.preview_metrics.write() = Some(PreviewMetrics {
            parse_count: rebuilt_analysis.metrics.parse_count,
            local_phase_count: rebuilt_analysis.metrics.local_phase_count,
            build_micros: started.elapsed().as_micros(),
            committed_context_only: false,
            fell_back_to_single_document: false,
        });
        snapshot
    }

    pub fn get(&self, uri: &str) -> Option<Arc<AnalysisSnapshot>> {
        self.documents.read().get(uri).cloned()
    }

    pub fn insert_snapshot(&self, snapshot: Arc<AnalysisSnapshot>) {
        self.documents
            .write()
            .insert(Arc::clone(&snapshot.uri), snapshot);
    }

    #[doc(hidden)]
    pub fn last_dirty_uris(&self) -> HashSet<Arc<str>> {
        self.analysis
            .read()
            .as_ref()
            .map(|analysis| analysis.dirty_uris.clone())
            .unwrap_or_default()
    }

    #[doc(hidden)]
    pub fn last_analysis_revision(&self) -> u64 {
        *self.analysis_revision.read()
    }

    #[doc(hidden)]
    pub fn last_analysis_metrics(&self) -> Option<(usize, usize, usize)> {
        self.analysis.read().as_ref().map(|analysis| {
            (
                analysis.metrics.parse_count,
                analysis.metrics.local_phase_count,
                analysis.metrics.dirty_uri_count,
            )
        })
    }

    #[doc(hidden)]
    pub fn last_analysis_metrics_snapshot(&self) -> Option<WorkspaceAnalysisMetricsSnapshot> {
        self.analysis
            .read()
            .as_ref()
            .map(|analysis| WorkspaceAnalysisMetricsSnapshot {
                parse_count: analysis.metrics.parse_count,
                local_phase_count: analysis.metrics.local_phase_count,
                dirty_uri_count: analysis.metrics.dirty_uri_count,
                parse_micros: analysis.metrics.parse_micros,
                local_phase_micros: analysis.metrics.local_phase_micros,
                dependency_projection_micros: analysis.metrics.dependency_projection_micros,
                parse_work_micros: analysis.metrics.parse_work_micros,
                local_phase_work_micros: analysis.metrics.local_phase_work_micros,
                project_update_micros: analysis.metrics.project_update_micros,
                snapshot_build_micros: analysis.metrics.snapshot_build_micros,
                routine_analysis_micros: analysis.metrics.routine_analysis_micros,
                routine_analysis_index_micros: analysis.metrics.routine_analysis_index_micros,
                routine_analysis_ir_micros: analysis.metrics.routine_analysis_ir_micros,
                routine_analysis_cfg_micros: analysis.metrics.routine_analysis_cfg_micros,
                routine_analysis_dataflow_micros: analysis.metrics.routine_analysis_dataflow_micros,
                routine_analysis_dead_store_micros: analysis
                    .metrics
                    .routine_analysis_dead_store_micros,
                routine_analysis_perform_routine_count: analysis
                    .metrics
                    .routine_analysis_perform_routine_count,
                routine_analysis_dataflow_pass_count: analysis
                    .metrics
                    .routine_analysis_dataflow_pass_count,
                routine_analysis_dataflow_routine_runs: analysis
                    .metrics
                    .routine_analysis_dataflow_routine_runs,
                static_analysis_summary_micros: analysis.metrics.static_analysis_summary_micros,
                callable_summary_micros: analysis.metrics.callable_summary_micros,
                full_rebuild: analysis.metrics.full_rebuild,
                unit_count: analysis.metrics.unit_count,
                dirty_unit_count: analysis.metrics.dirty_unit_count,
                diagnostic_scope_unit_count: analysis.metrics.diagnostic_scope_unit_count,
                validation_unit_count: analysis.metrics.validation_unit_count,
                scope_index_clone_micros: analysis.metrics.scope_index_clone_micros,
                build_workspace_index_micros: analysis.metrics.build_workspace_index_micros,
                compute_dirty_set_micros: analysis.metrics.compute_dirty_set_micros,
                clone_previous_units_micros: analysis.metrics.clone_previous_units_micros,
                apply_local_updates_micros: analysis.metrics.apply_local_updates_micros,
                resolve_include_edges_micros: analysis.metrics.resolve_include_edges_micros,
                resolve_cross_unit_micros: analysis.metrics.resolve_cross_unit_micros,
                infer_semantic_facts_micros: analysis.metrics.infer_semantic_facts_micros,
                rebuild_semantic_index_micros: analysis.metrics.rebuild_semantic_index_micros,
                validate_micros: analysis.metrics.validate_micros,
                collect_project_diagnostics_micros: analysis
                    .metrics
                    .collect_project_diagnostics_micros,
            })
    }

    #[doc(hidden)]
    pub fn last_preview_metrics_snapshot(&self) -> Option<WorkspacePreviewMetricsSnapshot> {
        self.preview_metrics
            .read()
            .as_ref()
            .map(|metrics| WorkspacePreviewMetricsSnapshot {
                parse_count: metrics.parse_count,
                local_phase_count: metrics.local_phase_count,
                build_micros: metrics.build_micros,
                committed_context_only: metrics.committed_context_only,
                fell_back_to_single_document: metrics.fell_back_to_single_document,
            })
    }

    pub fn references(
        &self,
        uri: &str,
        offset: usize,
        include_declaration: bool,
    ) -> Option<Vec<ReferenceTarget>> {
        let snapshot = self.get(uri)?;
        self.references_for_snapshot(snapshot.as_ref(), offset, include_declaration)
    }

    pub fn references_for_snapshot(
        &self,
        snapshot: &AnalysisSnapshot,
        offset: usize,
        include_declaration: bool,
    ) -> Option<Vec<ReferenceTarget>> {
        let target = snapshot.reference_search_target_at(offset)?;
        let mut references = snapshot.local_references_for_target(&target);
        references.extend(
            self.documents
                .read()
                .values()
                .filter(|candidate| candidate.uri.as_ref() != snapshot.uri.as_ref())
                .flat_map(|candidate| candidate.local_references_for_target(&target)),
        );
        if include_declaration
            && let Some(declaration) =
                reference_target_for_search_target(snapshot.project.as_ref(), &target)
        {
            references.push(declaration);
        }
        references.sort_by(|left, right| {
            left.uri
                .cmp(&right.uri)
                .then(left.range.start.cmp(&right.range.start))
                .then(left.range.end.cmp(&right.range.end))
        });
        references.dedup_by(|left, right| left.uri == right.uri && left.range == right.range);
        Some(references)
    }

    pub fn rename_plan(&self, uri: &str, offset: usize) -> Option<RenamePlan> {
        let snapshot = self.get(uri)?;
        self.rename_plan_for_snapshot(snapshot.as_ref(), offset)
    }

    pub fn rename_plan_for_snapshot(
        &self,
        snapshot: &AnalysisSnapshot,
        offset: usize,
    ) -> Option<RenamePlan> {
        let (target, range) = snapshot.rename_target_at(offset)?;
        let placeholder = snapshot.text.get(range.clone())?.to_string();
        let mut locations = snapshot.local_rename_locations_for_target(&target);
        locations.extend(
            self.documents
                .read()
                .values()
                .filter(|candidate| candidate.uri.as_ref() != snapshot.uri.as_ref())
                .flat_map(|candidate| candidate.local_rename_locations_for_target(&target)),
        );
        locations.sort_by(|left, right| {
            left.uri
                .cmp(&right.uri)
                .then(left.range.start.cmp(&right.range.start))
                .then(left.range.end.cmp(&right.range.end))
        });
        locations.dedup_by(|left, right| left.uri == right.uri && left.range == right.range);
        Some(RenamePlan {
            range,
            placeholder,
            locations,
        })
    }

    pub fn len(&self) -> usize {
        self.documents.read().len()
    }

    pub fn uris(&self) -> Vec<Arc<str>> {
        self.documents.read().keys().cloned().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::{
        ABAP_LSP_DEAD_STORE, ABAP_LSP_DYNAMIC_OPEN_SQL, ABAP_LSP_FOR_ALL_ENTRIES_WITHOUT_GUARD,
        ABAP_LSP_IGNORED_AUTHORITY_CHECK, ABAP_LSP_IGNORED_CALL_FUNCTION_RESULT,
        ABAP_LSP_SELECT_IN_LOOP, ABAP_LSP_SELECT_SINGLE_WITHOUT_FULL_KEY, ABAP_LSP_SELECT_STAR,
        AnalysisSnapshot, CallableSummary, DefinitionTarget, DocumentInput, DocumentStore,
        HoveredComponentKind, LintPolicy, LintSuppressionKind, ReferenceTarget, SnapshotBuildPlan,
        ddic_xml_to_abap_source, dependency_surface_text,
        opened_function_module_dependency_analysis_text,
    };
    use abap_symbols::{
        Diagnostic, DiagnosticKind, Namespace, ReferenceKind, Resolution, RoutineBlockKind,
        RoutineBranchKind, RoutineEdgeKind, RoutineInstructionSite, RoutineKind, ScopeId,
        ScopeKind, StructureFieldShape, SymbolHandle, SymbolId, SymbolKind,
    };
    use std::sync::Arc;

    fn assert_target_slice(target: &DefinitionTarget, uri: &str, text: &str, expected: &str) {
        assert_eq!(target.uri.as_ref(), uri);
        assert_eq!(&text[target.range.clone()], expected);
    }

    fn assert_reference_slices(references: &[ReferenceTarget], entries: &[(&str, &str, &str)]) {
        let actual: Vec<_> = references
            .iter()
            .map(|reference| {
                let entry = entries
                    .iter()
                    .find(|(uri, _, _)| *uri == reference.uri.as_ref())
                    .expect("reference text for URI");
                (
                    reference.uri.as_ref().to_string(),
                    entry.1[reference.range.clone()].to_string(),
                )
            })
            .collect();
        let expected: Vec<_> = entries
            .iter()
            .map(|(uri, _, expected_slice)| (uri.to_string(), expected_slice.to_string()))
            .collect();
        assert_eq!(actual, expected);
    }

    fn enclosing_class_symbol(snapshot: &AnalysisSnapshot, scope: ScopeId) -> Option<SymbolId> {
        let mut current = Some(scope);
        while let Some(scope_id) = current {
            let scope = snapshot.symbols.scopes.get(scope_id.as_usize())?;
            if scope.kind == ScopeKind::Class {
                return scope.owner;
            }
            current = scope.parent;
        }
        None
    }

    fn callable_method_summary<'a>(
        snapshot: &'a AnalysisSnapshot,
        class_name: &str,
        method_name: &str,
    ) -> &'a CallableSummary {
        let member = snapshot
            .symbols
            .class_members
            .iter()
            .find(|member| {
                snapshot.symbols.symbol(member.class_symbol).name.as_ref() == class_name
                    && member.name.as_ref() == method_name
            })
            .expect("class member");
        let method = snapshot
            .symbols
            .symbols
            .iter()
            .find(|symbol| {
                symbol.kind == SymbolKind::Method
                    && enclosing_class_symbol(snapshot, symbol.scope) == Some(member.class_symbol)
                    && (symbol.name.as_ref() == method_name
                        || symbol.name.rsplit('~').next() == Some(method_name))
            })
            .expect("method symbol");
        snapshot
            .callable_summaries()
            .summary_for_owner(SymbolHandle {
                unit: snapshot.symbols.unit_id,
                symbol: method.id,
            })
            .expect("callable summary for method owner")
    }

    fn static_analysis_method_summary<'a>(
        snapshot: &'a AnalysisSnapshot,
        class_name: &str,
        method_name: &str,
    ) -> &'a abap_symbols::RoutineStaticAnalysisSummary {
        let member = snapshot
            .symbols
            .class_members
            .iter()
            .find(|member| {
                snapshot.symbols.symbol(member.class_symbol).name.as_ref() == class_name
                    && member.name.as_ref() == method_name
            })
            .expect("class member");
        let method = snapshot
            .symbols
            .symbols
            .iter()
            .find(|symbol| {
                symbol.kind == SymbolKind::Method
                    && enclosing_class_symbol(snapshot, symbol.scope) == Some(member.class_symbol)
                    && (symbol.name.as_ref() == method_name
                        || symbol.name.rsplit('~').next() == Some(method_name))
            })
            .expect("method symbol");
        snapshot
            .static_analysis()
            .expect("static analysis summary")
            .routine_for_owner(SymbolHandle {
                unit: snapshot.symbols.unit_id,
                symbol: method.id,
            })
            .expect("static analysis summary for method owner")
    }

    fn diagnostic_slices(
        src: &str,
        diagnostics: &[Diagnostic],
        kind: DiagnosticKind,
    ) -> Vec<String> {
        diagnostics
            .iter()
            .filter(|diag| diag.kind == kind)
            .map(|diag| src[diag.range.clone()].to_string())
            .collect()
    }

    fn lint_slices(src: &str, snapshot: &AnalysisSnapshot, id: &str) -> Vec<String> {
        snapshot
            .lint_diagnostics()
            .iter()
            .filter(|diag| diag.id == id)
            .map(|diag| src[diag.range.clone()].to_string())
            .collect()
    }

    #[test]
    fn dependency_surface_projection_strips_private_sections_and_routine_bodies() {
        let src = "\
CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS pub RETURNING VALUE(rv_value) TYPE string.
  PROTECTED SECTION.
    DATA mv_visible TYPE string.
  PRIVATE SECTION.
    METHODS priv.
ENDCLASS.

CLASS zcl_dep IMPLEMENTATION.
  METHOD pub.
    rv_value = zcl_hidden=>make( ).
    INCLUDE zinc_method.
  ENDMETHOD.
  METHOD priv.
    DATA lv_private TYPE zcl_private.
  ENDMETHOD.
ENDCLASS.

FORM keep USING iv_value TYPE zcl_form_type.
  DATA lv_form TYPE zcl_form_impl.
  INCLUDE zinc_form.
ENDFORM.

FUNCTION z_keep.
  DATA lv_fm TYPE zcl_fm_impl.
  INCLUDE zinc_function.
ENDFUNCTION.
";
        let projected = dependency_surface_text(src);

        assert!(projected.contains("METHODS pub RETURNING VALUE(rv_value) TYPE string."));
        assert!(projected.contains("DATA mv_visible TYPE string."));
        assert!(projected.contains("FORM keep USING iv_value TYPE zcl_form_type."));
        assert!(projected.contains("FUNCTION z_keep."));
        assert!(!projected.contains("PRIVATE SECTION."));
        assert!(!projected.contains("METHODS priv."));
        assert!(!projected.contains("zcl_hidden=>make"));
        assert!(!projected.contains("zcl_private"));
        assert!(!projected.contains("zcl_form_impl"));
        assert!(!projected.contains("zcl_fm_impl"));
        assert!(projected.contains("INCLUDE zinc_method."));
        assert!(projected.contains("INCLUDE zinc_form."));
        assert!(projected.contains("INCLUDE zinc_function."));
    }

    #[test]
    fn dependency_surface_projection_drops_structured_include_type_from_blankened_local_types() {
        let src = "\
CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_pub.
      INCLUDE TYPE ty_inner AS inner.
      TYPES field TYPE i,
      END OF ty_pub.
ENDCLASS.

CLASS zcl_dep IMPLEMENTATION.
  METHOD run.
    TYPES:
      BEGIN OF ty_local,
        field TYPE i.
    INCLUDE TYPE ty_other AS other.
    TYPES: END OF ty_local.
  ENDMETHOD.
ENDCLASS.";
        let projected = dependency_surface_text(src);

        assert!(projected.contains("INCLUDE TYPE ty_inner AS inner."));
        assert!(!projected.contains("INCLUDE TYPE ty_other AS other."));
    }

    #[test]
    fn dependency_surface_projection_keeps_function_pool_and_function_modules_distinct() {
        let src = "\
FUNCTION-POOL zfg.
INCLUDE lzfgtop.

FUNCTION z_keep.
  DATA lv_fm TYPE zcl_fm_impl.
ENDFUNCTION.
";
        let projected = dependency_surface_text(src);

        assert!(projected.contains("FUNCTION-POOL zfg."));
        assert!(projected.contains("FUNCTION z_keep."));
        assert!(!projected.contains("zcl_fm_impl"));
    }

    #[test]
    fn opened_function_module_dependency_projection_restores_function_block_body() {
        let src = "\
* >>> BEGIN INCLUDE ltop
FORM helper.
  DATA lv_top TYPE i.
  lv_top = 1.
ENDFORM.
* <<< END INCLUDE ltop

* >>> BEGIN FUNCTION MODULE z_keep
FUNCTION z_keep.
  DATA lv_body TYPE i.
  lv_body = 1.
ENDFUNCTION.
* <<< END FUNCTION MODULE z_keep
";
        let projected = opened_function_module_dependency_analysis_text(src)
            .expect("projected dependency text");

        assert!(!projected.contains("lv_top = 1."));
        assert!(projected.contains("lv_body = 1."));
    }

    #[test]
    fn dependency_surface_keeps_protected_super_members_visible_to_child_resolution() {
        let store = DocumentStore::default();
        let main_src = "\
CLASS zcl_child DEFINITION INHERITING FROM zcl_base.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS zcl_child IMPLEMENTATION.
  METHOD run.
    super->prot_value = 'x'.
  ENDMETHOD.
ENDCLASS.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///dep.abap"),
                version: 1,
                text: Arc::from(
                    "\
CLASS zcl_base DEFINITION.
  PROTECTED SECTION.
    DATA prot_value TYPE string.
ENDCLASS.
CLASS zcl_base IMPLEMENTATION.
ENDCLASS.",
                ),
                is_dependency: true,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");
        let offset = main_src.find("prot_value").expect("field access") + 1;

        let hovered = snapshot
            .hovered_component_at(offset)
            .expect("hovered protected component");

        assert_eq!(hovered.field_name.as_ref(), "prot_value");
    }

    #[test]
    fn dependency_chain_resolves_bare_inherited_attributes_across_multiple_superclasses() {
        let store = DocumentStore::default();
        let base_src = "\
CLASS /pkg/cl_base_inst DEFINITION.
  PUBLIC SECTION.
    DATA mo_messages TYPE REF TO object.
    CLASS-DATA gv_dummy_msg TYPE string.
ENDCLASS.
CLASS /pkg/cl_base_inst IMPLEMENTATION.
ENDCLASS.";
        let mid_src = "\
CLASS /pkg/cl_rep_base DEFINITION INHERITING FROM /pkg/cl_base_inst.
ENDCLASS.
CLASS /pkg/cl_rep_base IMPLEMENTATION.
ENDCLASS.";
        let parent_src = "\
CLASS /pkg/cl_rep_ru DEFINITION INHERITING FROM /pkg/cl_rep_base.
  PUBLIC SECTION.
    METHODS noop.
ENDCLASS.
CLASS /pkg/cl_rep_ru IMPLEMENTATION.
  METHOD noop.
  ENDMETHOD.
ENDCLASS.";
        let main_src = "\
CLASS zcl_child DEFINITION INHERITING FROM /pkg/cl_rep_ru.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS zcl_child IMPLEMENTATION.
  METHOD run.
    CLEAR gv_dummy_msg.
    IF mo_messages IS BOUND.
    ENDIF.
  ENDMETHOD.
ENDCLASS.";

        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///deps/%2FPKG%2FCL_BASE_INST.abap"),
                version: 1,
                text: Arc::from(base_src),
                is_dependency: true,
                object_name: Some(Arc::from("/pkg/cl_base_inst")),
            },
            DocumentInput {
                uri: Arc::from("file:///deps/%2FPKG%2FCL_REP_BASE.abap"),
                version: 1,
                text: Arc::from(mid_src),
                is_dependency: true,
                object_name: Some(Arc::from("/pkg/cl_rep_base")),
            },
            DocumentInput {
                uri: Arc::from("file:///deps/%2FPKG%2FCL_REP_RU.abap"),
                version: 1,
                text: Arc::from(parent_src),
                is_dependency: true,
                object_name: Some(Arc::from("/pkg/cl_rep_ru")),
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");

        assert!(
            snapshot
                .symbols
                .diagnostics
                .iter()
                .all(|diag| !diag.message.contains("gv_dummy_msg")
                    && !diag.message.contains("mo_messages")),
            "{:?}",
            snapshot.symbols.diagnostics
        );
    }

    #[test]
    fn completes_bare_inherited_attributes_inside_instance_method() {
        let store = DocumentStore::default();
        let src = "\
CLASS zcl_base DEFINITION.
  PROTECTED SECTION.
    DATA mo_context TYPE REF TO object.
ENDCLASS.
CLASS zcl_base IMPLEMENTATION.
ENDCLASS.

CLASS zcl_parent DEFINITION INHERITING FROM zcl_base.
  PROTECTED SECTION.
    DATA mo_injection TYPE REF TO object.
ENDCLASS.
CLASS zcl_parent IMPLEMENTATION.
ENDCLASS.

CLASS zcl_child DEFINITION INHERITING FROM zcl_parent.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS zcl_child IMPLEMENTATION.
  METHOD run.
    mo
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///main.abap", 1, src);
        let offset = src.rfind("mo").expect("prefix") + 2;
        let completion = snapshot.completion_at(offset).expect("completion");
        let labels = completion
            .items
            .iter()
            .map(|item| super::completion_item_name(item))
            .collect::<Vec<_>>();

        assert_eq!(&src[completion.replace_range], "mo");
        assert!(labels.contains(&"mo_context"), "{labels:?}");
        assert!(labels.contains(&"mo_injection"), "{labels:?}");
    }

    #[test]
    fn dependency_surface_keeps_public_methods_after_class_methods() {
        let store = DocumentStore::default();
        let snapshots = store.replace_all(vec![DocumentInput {
            uri: Arc::from("file:///dep.abap"),
            version: 1,
            text: Arc::from(
                "\
CLASS /cdbasis/cl_messages DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS compose_message.
    CLASS-METHODS compose_message_bapi
      IMPORTING iv_loglevel TYPE i.
    METHODS constructor.
    CLASS-METHODS conv2string
      RETURNING VALUE(rv_output) TYPE string.
ENDCLASS.
CLASS /cdbasis/cl_messages IMPLEMENTATION.
ENDCLASS.",
            ),
            is_dependency: true,
            object_name: Some(Arc::from("/cdbasis/cl_messages")),
        }]);
        let snapshot = snapshots
            .get("file:///dep.abap")
            .expect("dependency snapshot");
        let method_names: Vec<_> = snapshot
            .symbols
            .class_members
            .iter()
            .filter(|member| member.kind == abap_symbols::ClassMemberKind::Method)
            .map(|member| member.name.as_ref())
            .collect();

        assert!(
            method_names.contains(&"compose_message"),
            "expected first class-method, got {method_names:?}"
        );
        assert!(
            method_names.contains(&"compose_message_bapi"),
            "expected later public class-method, got {method_names:?}"
        );
        assert!(
            method_names.contains(&"constructor"),
            "expected instance method after class-methods, got {method_names:?}"
        );
        assert!(
            method_names.contains(&"conv2string"),
            "expected subsequent class-method, got {method_names:?}"
        );
    }

    #[test]
    fn dependency_surface_keeps_public_methods_after_class_load_declaration() {
        let store = DocumentStore::default();
        let snapshots = store.replace_all(vec![DocumentInput {
            uri: Arc::from("file:///dep.abap"),
            version: 1,
            text: Arc::from(
                "\
CLASS cl_document_bcs DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.
  GLOBAL FRIENDS cb_document_bcs
                 cl_bcs.

PUBLIC SECTION.
  CLASS ca_document_bcs DEFINITION LOAD.

  CLASS-METHODS create_document
    RETURNING VALUE(result) TYPE REF TO cl_document_bcs.
  METHODS add_attachment.

PROTECTED SECTION.
  DATA subject TYPE string.
ENDCLASS.
CLASS cl_document_bcs IMPLEMENTATION.
ENDCLASS.",
            ),
            is_dependency: true,
            object_name: Some(Arc::from("cl_document_bcs")),
        }]);
        let snapshot = snapshots
            .get("file:///dep.abap")
            .expect("dependency snapshot");
        let class_symbol = snapshot
            .symbols
            .symbols
            .iter()
            .find(|symbol| symbol.name.as_ref() == "cl_document_bcs")
            .expect("class symbol")
            .id;
        let member_names: Vec<_> = snapshot
            .symbols
            .class_members
            .iter()
            .filter(|member| member.class_symbol == class_symbol)
            .map(|member| member.name.as_ref())
            .collect();

        assert!(
            member_names.contains(&"create_document"),
            "expected class method before instance method, got {member_names:?}"
        );
        assert!(
            member_names.contains(&"add_attachment"),
            "expected public instance method after CLASS ... DEFINITION LOAD, got {member_names:?}"
        );
        assert!(
            member_names.contains(&"subject"),
            "expected protected members to remain visible, got {member_names:?}"
        );
    }

    #[test]
    fn publishes_snapshots_immutably() {
        let store = DocumentStore::default();
        let snapshot = store.publish("file:///demo.abap", 1, "DATA foo TYPE i.");

        assert_eq!(store.len(), 1);
        assert!(
            snapshot
                .symbols
                .symbols
                .iter()
                .any(|symbol| symbol.name.as_ref() == "foo")
        );
        assert_eq!(store.get("file:///demo.abap").unwrap().version, 1);
    }

    #[test]
    fn reuses_analysis_when_publish_text_is_unchanged() {
        let store = DocumentStore::default();
        let first = store.publish("file:///demo.abap", 1, "DATA foo TYPE i.");
        let second = store.publish("file:///demo.abap", 2, "DATA foo TYPE i.");

        assert_eq!(second.version, 2);
        assert!(Arc::ptr_eq(&first.parse, &second.parse));
        assert!(Arc::ptr_eq(&first.symbols, &second.symbols));
        assert!(Arc::ptr_eq(&first.project, &second.project));
        assert!(Arc::ptr_eq(
            &first.routine_analysis,
            &second.routine_analysis
        ));
        assert_eq!(store.get("file:///demo.abap").unwrap().version, 2);
    }

    #[test]
    fn append_only_publish_uses_incremental_project_update() {
        let store = DocumentStore::default();
        let main_src = "DATA lo_dep TYPE REF TO zcl_dep.";
        store.publish_inputs_with_build_plan(
            vec![DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            }],
            SnapshotBuildPlan::EDITOR_WORKSPACE,
        );
        let main_before = store.get("file:///main.abap").expect("main snapshot");

        let dep_src = "CLASS zcl_dep DEFINITION.\nENDCLASS.";
        store.publish_inputs_with_build_plan(
            vec![DocumentInput {
                uri: Arc::from("file:///dep.abap"),
                version: 1,
                text: Arc::from(dep_src),
                is_dependency: true,
                object_name: None,
            }],
            SnapshotBuildPlan::EDITOR_WORKSPACE,
        );

        let metrics = store
            .last_analysis_metrics_snapshot()
            .expect("analysis metrics");
        let main_after = store.get("file:///main.abap").expect("main snapshot");
        assert!(!metrics.full_rebuild);
        assert_eq!(metrics.unit_count, 2);
        assert!(Arc::ptr_eq(&main_before.line_index, &main_after.line_index));
    }

    #[test]
    fn snapshot_exposes_built_routine_analysis_foundation() {
        let store = DocumentStore::default();
        let src = "\
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_total TYPE i.
    lv_total = lv_total + 1.
  ENDMETHOD.
ENDCLASS.";

        let first = store.publish("file:///routine_foundation.abap", 1, src);
        let method = first
            .symbols
            .symbols
            .iter()
            .find(|symbol| symbol.kind == SymbolKind::Method && symbol.name.as_ref() == "run")
            .expect("method symbol");
        let method_scope = first
            .symbols
            .scopes
            .iter()
            .find(|scope| scope.kind == ScopeKind::Method && scope.owner == Some(method.id))
            .expect("method scope");
        let routine = first
            .routine_analysis()
            .routine_for_owner(SymbolHandle {
                unit: first.symbols.unit_id,
                symbol: method.id,
            })
            .expect("routine analysis for method owner");

        assert_eq!(routine.descriptor.kind, RoutineKind::Method);
        assert_eq!(routine.descriptor.scope, method_scope.id);
        assert_eq!(
            first
                .routine_analysis()
                .routine_for_scope(first.symbols.unit_id, method_scope.id)
                .expect("routine analysis for scope")
                .descriptor
                .id,
            routine.descriptor.id
        );
        assert!(!routine.ir.instructions.is_empty());
        assert_eq!(routine.cfg.blocks.len(), 3);
        assert!(
            routine
                .cfg
                .blocks
                .iter()
                .any(|block| block.kind == RoutineBlockKind::Body)
        );
        assert!(
            routine
                .dataflow_inputs
                .values
                .iter()
                .any(|value| value.name.as_ref() == "lv_total")
        );
        assert!(
            routine
                .dataflow_inputs
                .instructions
                .iter()
                .any(|summary| !summary.writes.is_empty())
        );
        assert!(
            routine
                .dataflow_result
                .block_exit
                .iter()
                .any(|summary| !summary.maybe_written_values.is_empty())
        );
        assert!(first.routine_analysis().metrics.routine_count >= 1);
        assert!(
            first.routine_analysis().metrics.instruction_count >= routine.ir.instructions.len()
        );

        let second = store.publish("file:///routine_foundation.abap", 2, src);
        assert!(Arc::ptr_eq(
            &first.routine_analysis,
            &second.routine_analysis
        ));
    }

    #[test]
    fn routine_analysis_metrics_only_recompute_perform_routines_across_passes() {
        let store = DocumentStore::default();
        let src = "\
REPORT zperf.

DATA lv_value TYPE i.

START-OF-SELECTION.
  PERFORM set_value CHANGING lv_value.

FORM set_value CHANGING cv_value TYPE i.
  cv_value = 1.
ENDFORM.";

        let snapshot = store.publish("file:///routine_metrics_perform.abap", 1, src);
        let metrics = &snapshot.routine_analysis().metrics;

        assert!(metrics.routine_count >= 2);
        assert_eq!(metrics.perform_routine_count, 1);
        assert!(metrics.dataflow_pass_count >= 2);
        assert!(metrics.perform_routine_count < metrics.routine_count);
        assert_eq!(
            metrics.dataflow_routine_runs,
            metrics.routine_count
                + metrics.perform_routine_count * (metrics.dataflow_pass_count - 1)
        );
    }

    #[test]
    fn callable_summaries_track_direct_effects_and_propagate_barriers() {
        let store = DocumentStore::default();
        let src = "\
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    DATA mv_text TYPE string.
    METHODS leaf
      IMPORTING iv_value TYPE i
      CHANGING cv_value TYPE i.
    METHODS wrapper
      CHANGING cv_value TYPE i.
    METHODS bind_fs.
    METHODS abort.
    METHODS run.
    METHODS unresolved.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD leaf.
    DATA lv_tmp TYPE i.
    lv_tmp = iv_value.
    cv_value = lv_tmp.
  ENDMETHOD.

  METHOD wrapper.
    me->leaf(
      EXPORTING iv_value = cv_value
      CHANGING cv_value = cv_value ).
  ENDMETHOD.

  METHOD bind_fs.
    FIELD-SYMBOLS <lv_text> TYPE string.
    ASSIGN mv_text TO <lv_text>.
  ENDMETHOD.

  METHOD abort.
    LEAVE PROGRAM.
  ENDMETHOD.

  METHOD run.
    me->abort( ).
  ENDMETHOD.

  METHOD unresolved.
    me->missing( ).
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///callable_summary.abap", 1, src);
        let leaf = callable_method_summary(&snapshot, "zcl_demo", "leaf");
        let wrapper = callable_method_summary(&snapshot, "zcl_demo", "wrapper");
        let bind_fs = callable_method_summary(&snapshot, "zcl_demo", "bind_fs");
        let abort = callable_method_summary(&snapshot, "zcl_demo", "abort");
        let run = callable_method_summary(&snapshot, "zcl_demo", "run");
        let unresolved = callable_method_summary(&snapshot, "zcl_demo", "unresolved");

        let leaf_iv_value = leaf
            .parameters
            .iter()
            .find(|parameter| parameter.name.as_ref() == "iv_value")
            .expect("leaf importing parameter");
        let leaf_cv_value = leaf
            .parameters
            .iter()
            .find(|parameter| parameter.name.as_ref() == "cv_value")
            .expect("leaf changing parameter");
        assert!(leaf_iv_value.may_read);
        assert!(!leaf_iv_value.may_write);
        assert!(!leaf_cv_value.may_read);
        assert!(leaf_cv_value.may_write);
        assert!(leaf.may_read_through_reference_inputs);
        assert!(leaf.may_write_through_reference_inputs);
        assert!(leaf.dataflow_barrier);

        assert!(wrapper.dataflow_barrier);
        assert!(bind_fs.may_bind_field_symbols);
        assert!(bind_fs.dataflow_barrier);
        assert!(abort.may_terminate_non_locally);
        assert!(abort.may_not_return_normally);
        assert!(run.may_terminate_non_locally);
        assert!(run.may_not_return_normally);
        assert!(run.dataflow_barrier);
        assert!(unresolved.dataflow_barrier);
        assert!(!unresolved.may_terminate_non_locally);
    }

    #[test]
    fn callable_summary_artifact_is_shared_and_reports_metrics() {
        let store = DocumentStore::default();
        let src = "\
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_value TYPE i.
    lv_value = 1.
  ENDMETHOD.
ENDCLASS.";

        let first = store.publish("file:///callable_summary_metrics.abap", 1, src);
        let metrics = store
            .last_analysis_metrics_snapshot()
            .expect("analysis metrics snapshot");
        println!(
            "callable_summary_micros={} snapshot_build_micros={} summary_count={}",
            metrics.callable_summary_micros,
            metrics.snapshot_build_micros,
            first.callable_summaries().metrics.summary_count
        );

        assert!(metrics.snapshot_build_micros >= metrics.callable_summary_micros);
        assert!(
            first.callable_summaries().metrics.summary_count >= 1,
            "expected at least one callable summary"
        );
        assert!(
            first.callable_summaries().metrics.total_micros
                >= first.callable_summaries().metrics.direct_micros
        );

        let second = store.publish("file:///callable_summary_metrics.abap", 2, src);
        assert!(Arc::ptr_eq(
            &first.callable_summaries,
            &second.callable_summaries
        ));
    }

    #[test]
    fn static_analysis_summary_artifact_is_shared_and_reports_metrics() {
        let store = DocumentStore::default();
        let src = "\
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_value TYPE i.
    RETURN.
    lv_value = 1.
  ENDMETHOD.
ENDCLASS.";

        let first = store.publish("file:///static_summary_metrics.abap", 1, src);
        let metrics = store
            .last_analysis_metrics_snapshot()
            .expect("analysis metrics snapshot");
        let summary = static_analysis_method_summary(&first, "zcl_demo", "run");

        assert!(metrics.snapshot_build_micros >= metrics.static_analysis_summary_micros);
        assert_eq!(summary.finding_counts.unreachable_code, 1);
        assert_eq!(summary.findings.len(), 1);
        assert!(summary.instruction_count >= summary.reachable_instruction_count);

        let second = store.publish("file:///static_summary_metrics.abap", 2, src);
        assert!(Arc::ptr_eq(
            first
                .static_analysis
                .as_ref()
                .expect("first static analysis"),
            second
                .static_analysis
                .as_ref()
                .expect("second static analysis")
        ));
    }

    #[test]
    fn replace_all_with_effective_source_plan_skips_unrequested_artifacts() {
        let store = DocumentStore::default();
        let snapshots = store.replace_all_with_build_plan(
            vec![DocumentInput {
                uri: Arc::from("file:///effective_source_plan.abap"),
                version: 1,
                text: Arc::from(
                    "\
REPORT zplan.

DATA lv_value TYPE i.
lv_value = 1.",
                ),
                is_dependency: false,
                object_name: None,
            }],
            SnapshotBuildPlan::EFFECTIVE_SOURCE,
        );
        let snapshot = snapshots
            .get("file:///effective_source_plan.abap")
            .expect("snapshot");
        let metrics = store
            .last_analysis_metrics_snapshot()
            .expect("analysis metrics snapshot");

        assert!(snapshot.routine_analysis().routines.is_empty());
        assert!(snapshot.static_analysis().is_none());
        assert!(snapshot.call_graph().nodes.is_empty());
        assert!(snapshot.callable_summaries().summaries.is_empty());
        assert_eq!(metrics.routine_analysis_micros, 0);
        assert_eq!(metrics.static_analysis_summary_micros, 0);
        assert_eq!(metrics.callable_summary_micros, 0);
    }

    #[test]
    fn build_plan_can_skip_lint_analysis_until_committed_publish() {
        let store = DocumentStore::default();
        let src = "SELECT * FROM mara INTO TABLE @DATA(lt_mara).";
        let input = DocumentInput {
            uri: Arc::from("file:///lint_plan.abap"),
            version: 1,
            text: Arc::from(src),
            is_dependency: false,
            object_name: None,
        };
        let mut hydration_plan = SnapshotBuildPlan::EDITOR_WORKSPACE;
        hydration_plan.lint_analysis = false;

        let snapshots = store.replace_all_with_build_plan(vec![input.clone()], hydration_plan);
        let snapshot = snapshots.get("file:///lint_plan.abap").expect("snapshot");
        assert!(snapshot.lint_diagnostics().is_empty());

        let snapshots =
            store.replace_all_with_build_plan(vec![input], SnapshotBuildPlan::EDITOR_WORKSPACE);
        let snapshot = snapshots.get("file:///lint_plan.abap").expect("snapshot");
        assert_eq!(
            lint_slices(src, snapshot, ABAP_LSP_SELECT_STAR),
            vec!["*".to_string()]
        );
    }

    #[test]
    fn editor_workspace_defers_dependency_diagnostics_but_keeps_editable_diagnostics_and_resolution()
     {
        let dependency_src = "\
CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    DATA dep_value TYPE zty_dep_missing.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
ENDCLASS.";
        let main_src = "\
DATA lo_dep TYPE REF TO zcl_dep.
DATA lv_missing TYPE zty_main_missing.
START-OF-SELECTION.
  DATA lv_seen TYPE i.
  lv_seen = 1.";
        let inputs = vec![
            DocumentInput {
                uri: Arc::from("file:///dep.abap"),
                version: 1,
                text: Arc::from(dependency_src),
                is_dependency: true,
                object_name: Some(Arc::from("zcl_dep")),
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ];

        let full_store = DocumentStore::default();
        let full = full_store.replace_all_with_build_plan(inputs.clone(), SnapshotBuildPlan::FULL);
        let full_dep = full.get("file:///dep.abap").expect("full dependency");
        assert!(
            full_dep
                .symbols
                .diagnostics
                .iter()
                .any(|diagnostic| diagnostic.message.contains("zty_dep_missing")),
            "{:?}",
            full_dep.symbols.diagnostics
        );

        let editor_store = DocumentStore::default();
        let editor =
            editor_store.replace_all_with_build_plan(inputs, SnapshotBuildPlan::EDITOR_WORKSPACE);
        let metrics = editor_store
            .last_analysis_metrics_snapshot()
            .expect("analysis metrics snapshot");
        let editor_dep = editor.get("file:///dep.abap").expect("editor dependency");
        let editor_main = editor.get("file:///main.abap").expect("editor main");

        assert_eq!(metrics.diagnostic_scope_unit_count, 1);
        assert_eq!(metrics.validation_unit_count, 1);
        assert_eq!(
            editor_dep
                .routine_analysis()
                .routines_for_unit(editor_dep.symbols.unit_id)
                .count(),
            0
        );
        assert!(
            editor_main
                .routine_analysis()
                .routines_for_unit(editor_main.symbols.unit_id)
                .count()
                > 0
        );
        assert!(
            editor_dep
                .symbols
                .diagnostics
                .iter()
                .all(|diagnostic| !diagnostic.message.contains("zty_dep_missing")),
            "{:?}",
            editor_dep.symbols.diagnostics
        );
        assert!(
            editor_main
                .symbols
                .diagnostics
                .iter()
                .any(|diagnostic| diagnostic.message.contains("zty_main_missing")),
            "{:?}",
            editor_main.symbols.diagnostics
        );
        assert!(
            editor_main
                .symbols
                .diagnostics
                .iter()
                .all(|diagnostic| !diagnostic.message.contains("zcl_dep")),
            "{:?}",
            editor_main.symbols.diagnostics
        );
        assert!(
            editor_main.symbols.references.iter().any(|reference| {
                reference.name.as_ref() == "zcl_dep"
                    && matches!(reference.resolution, Some(Resolution::Symbol(_)))
            }),
            "{:?}",
            editor_main.symbols.references
        );
    }

    #[test]
    fn editor_workspace_keeps_dependency_include_components_in_diagnostic_scope() {
        let store = DocumentStore::default();
        let main_src = "\
REPORT zmain.
INCLUDE zinc_dep.";
        let include_src = "DATA lv_inc TYPE zty_include_missing.";

        let snapshots = store.replace_all_with_build_plan(
            vec![
                DocumentInput {
                    uri: Arc::from("file:///main.abap"),
                    version: 1,
                    text: Arc::from(main_src),
                    is_dependency: false,
                    object_name: None,
                },
                DocumentInput {
                    uri: Arc::from("file:///zinc_dep.abap"),
                    version: 1,
                    text: Arc::from(include_src),
                    is_dependency: true,
                    object_name: Some(Arc::from("zinc_dep")),
                },
            ],
            SnapshotBuildPlan::EDITOR_WORKSPACE,
        );
        let metrics = store
            .last_analysis_metrics_snapshot()
            .expect("analysis metrics snapshot");
        let main = snapshots.get("file:///main.abap").expect("main snapshot");
        let include = snapshots
            .get("file:///zinc_dep.abap")
            .expect("include snapshot");

        assert_eq!(metrics.diagnostic_scope_unit_count, 2);
        assert_eq!(metrics.validation_unit_count, 2);
        assert!(
            main.symbols
                .include_edges
                .iter()
                .any(|edge| edge.name.as_ref() == "zinc_dep" && edge.target.is_some())
        );
        assert!(
            include
                .symbols
                .diagnostics
                .iter()
                .any(|diagnostic| diagnostic.message.contains("zty_include_missing")),
            "{:?}",
            include.symbols.diagnostics
        );
    }

    #[test]
    fn routine_analysis_discovers_supported_executable_regions() {
        let store = DocumentStore::default();
        let src = "\
REPORT zroutine_regions.

CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_method TYPE i.
    lv_method = 1.
  ENDMETHOD.
ENDCLASS.

FORM local_form.
  DATA lv_form TYPE i.
  lv_form = 2.
ENDFORM.

FUNCTION z_demo_function.
  DATA lv_func TYPE i.
  lv_func = 3.
ENDFUNCTION.

START-OF-SELECTION.
  DATA lv_event TYPE i.
  lv_event = 4.";

        let snapshot = store.publish("file:///routine_regions.abap", 1, src);
        let routines: Vec<_> = snapshot
            .routine_analysis()
            .routines_for_unit(snapshot.symbols.unit_id)
            .collect();

        assert!(routines.iter().any(|routine| {
            routine.descriptor.kind == RoutineKind::Method
                && routine.descriptor.executable_range.is_some()
        }));
        assert!(routines.iter().any(|routine| {
            routine.descriptor.kind == RoutineKind::Form
                && routine.descriptor.executable_range.is_some()
        }));
        assert!(routines.iter().any(|routine| {
            routine.descriptor.kind == RoutineKind::Module
                && routine.descriptor.executable_range.is_some()
        }));
        assert!(routines.iter().any(|routine| {
            routine.descriptor.kind == RoutineKind::EventBlock
                && routine.descriptor.executable_range.is_some()
        }));
    }

    #[test]
    fn routine_analysis_marks_code_after_exhaustive_if_elseif_return_unreachable() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_value TYPE i.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_after_if TYPE i.
    IF iv_value = 1.
      RETURN.
    ELSEIF iv_value = 2.
      RETURN.
    ELSE.
      RETURN.
    ENDIF.
    lv_after_if = 1.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_if_return.abap", 1, src);
        let unreachable: Vec<_> = snapshot
            .symbols
            .diagnostics
            .iter()
            .filter(|diag| diag.kind == DiagnosticKind::UnreachableCode)
            .collect();

        assert!(
            unreachable
                .iter()
                .any(|diag| { src[diag.range.clone()].contains("lv_after_if") })
        );
    }

    #[test]
    fn routine_analysis_coalesces_nested_unreachable_blocks_after_early_return() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS get_response IMPORTING iv_restart TYPE char1
                                   iv_http_code TYPE char10.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD get_response.
    RETURN.
    IF iv_restart IS NOT INITIAL.
      IF iv_http_code = '429'.
        MESSAGE 'x' TYPE 'S'.
      ELSE.
        MESSAGE 'y' TYPE 'S'.
      ENDIF.
    ELSE.
      MESSAGE 'z' TYPE 'S'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_early_return.abap", 1, src);
        let unreachable: Vec<_> = snapshot
            .symbols
            .diagnostics
            .iter()
            .filter(|diag| {
                diag.kind == DiagnosticKind::UnreachableCode
                    && diag.message.contains("get_response")
            })
            .collect();

        assert_eq!(unreachable.len(), 1);
        assert!(src[unreachable[0].range.clone()].contains("IF iv_restart"));
    }

    #[test]
    fn routine_analysis_treats_leave_list_processing_in_method_as_conservative() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS get_response.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD get_response.
    DATA lv_after_leave TYPE i.
    LEAVE LIST-PROCESSING.
    lv_after_leave = 1.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///leave_list_processing_method.abap", 1, src);
        assert!(
            snapshot.symbols.diagnostics.iter().all(|diag| {
                diag.kind != DiagnosticKind::UnreachableCode
                    && !diag.message.contains("unknown symbol 'list'")
            }),
            "{:?}",
            snapshot.symbols.diagnostics
        );
    }

    #[test]
    fn routine_analysis_treats_leave_list_processing_in_event_block_as_exit() {
        let store = DocumentStore::default();
        let src = "\
REPORT zleave_list_processing.

START-OF-SELECTION.
  WRITE 'before'.
  LEAVE LIST-PROCESSING.
  WRITE 'after'.";

        let snapshot = store.publish("file:///leave_list_processing_event.abap", 1, src);
        let unreachable: Vec<_> = snapshot
            .symbols
            .diagnostics
            .iter()
            .filter(|diag| diag.kind == DiagnosticKind::UnreachableCode)
            .collect();

        assert_eq!(unreachable.len(), 1);
        assert!(src[unreachable[0].range.clone()].contains("WRITE 'after'"));
        assert!(
            snapshot
                .symbols
                .diagnostics
                .iter()
                .all(|diag| !diag.message.contains("unknown symbol 'list'")),
            "{:?}",
            snapshot.symbols.diagnostics
        );
    }

    #[test]
    fn routine_analysis_marks_code_after_exhaustive_case_return_unreachable() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_value TYPE i.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_after_case TYPE i.
    CASE iv_value.
      WHEN 1.
        RETURN.
      WHEN OTHERS.
        RETURN.
    ENDCASE.
    lv_after_case = 1.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_case_return.abap", 1, src);
        let unreachable: Vec<_> = snapshot
            .symbols
            .diagnostics
            .iter()
            .filter(|diag| diag.kind == DiagnosticKind::UnreachableCode)
            .collect();

        assert!(
            unreachable
                .iter()
                .any(|diag| { src[diag.range.clone()].contains("lv_after_case") })
        );
        let method = snapshot
            .routine_analysis()
            .routines_for_unit(snapshot.symbols.unit_id)
            .find(|routine| routine.descriptor.name.as_ref() == "run")
            .expect("method routine");
        assert!(method.ir.instructions.iter().any(|instruction| {
            matches!(
                instruction.site,
                RoutineInstructionSite::Branch {
                    kind: RoutineBranchKind::Case
                }
            )
        }));
    }

    #[test]
    fn routine_analysis_builds_loop_cfg_and_flags_loop_local_unreachable_code() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_total TYPE i.
    DATA lt_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    WHILE lv_total < 1.
      lv_total = lv_total + 1.
    ENDWHILE.
    WHILE lv_total < 10.
      CONTINUE.
      lv_total = lv_total + 1.
    ENDWHILE.
    DO 1 TIMES.
      EXIT.
      lv_total = 1.
    ENDDO.
    LOOP AT lt_values INTO DATA(lv_item).
      EXIT.
      lv_total = lv_item.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_loops.abap", 1, src);
        let routine = snapshot
            .routine_analysis()
            .routines_for_unit(snapshot.symbols.unit_id)
            .find(|routine| routine.descriptor.name.as_ref() == "run")
            .expect("method routine");

        assert!(
            routine
                .cfg
                .edges
                .iter()
                .any(|edge| edge.kind == RoutineEdgeKind::LoopEnter)
        );
        assert!(
            routine
                .cfg
                .edges
                .iter()
                .any(|edge| edge.kind == RoutineEdgeKind::LoopBack)
        );

        let unreachable: Vec<_> = snapshot
            .symbols
            .diagnostics
            .iter()
            .filter(|diag| diag.kind == DiagnosticKind::UnreachableCode)
            .map(|diag| src[diag.range.clone()].to_string())
            .collect();
        assert!(
            unreachable
                .iter()
                .any(|slice| slice.contains("lv_total = lv_total + 1"))
        );
        assert!(
            unreachable
                .iter()
                .any(|slice| slice.contains("lv_total = 1"))
        );
        assert!(
            unreachable
                .iter()
                .any(|slice| slice.contains("lv_total = lv_item"))
        );
    }

    #[test]
    fn routine_analysis_tracks_try_catch_and_early_raise_unreachable_code() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_total TYPE i.
    TRY.
      RAISE EXCEPTION TYPE cx_root.
      lv_total = 1.
    CATCH cx_root INTO DATA(lo_err).
      lv_total = 2.
    ENDTRY.
    RAISE EXCEPTION TYPE cx_root.
    lv_total = 3.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_try_raise.abap", 1, src);
        let routine = snapshot
            .routine_analysis()
            .routines_for_unit(snapshot.symbols.unit_id)
            .find(|routine| routine.descriptor.name.as_ref() == "run")
            .expect("method routine");

        assert!(routine.ir.instructions.iter().any(|instruction| {
            matches!(
                instruction.site,
                RoutineInstructionSite::Branch {
                    kind: RoutineBranchKind::Try
                }
            )
        }));
        assert!(
            routine
                .cfg
                .edges
                .iter()
                .any(|edge| edge.kind == RoutineEdgeKind::Exceptional)
        );

        let unreachable: Vec<_> = snapshot
            .symbols
            .diagnostics
            .iter()
            .filter(|diag| diag.kind == DiagnosticKind::UnreachableCode)
            .map(|diag| src[diag.range.clone()].to_string())
            .collect();
        assert!(
            unreachable
                .iter()
                .any(|slice| slice.contains("lv_total = 1"))
        );
        assert!(
            unreachable
                .iter()
                .any(|slice| slice.contains("lv_total = 3"))
        );
        assert!(
            !unreachable
                .iter()
                .any(|slice| slice.contains("lv_total = 2"))
        );
    }

    #[test]
    fn routine_analysis_does_not_flag_raise_exception_exporting_arguments_as_unreachable() {
        let store = DocumentStore::default();
        let src = "\
CLASS cx_demo DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        textid TYPE string.
ENDCLASS.

CLASS cx_demo IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS raise_ilm_exception_with_msg
      IMPORTING iv_textid TYPE string
      RAISING cx_demo.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD raise_ilm_exception_with_msg.
    RAISE EXCEPTION TYPE cx_demo
      EXPORTING
        textid = iv_textid.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///raise_exception_exporting_unreachable.abap", 1, src);
        let unreachable: Vec<_> = snapshot
            .symbols
            .diagnostics
            .iter()
            .filter(|diag| diag.kind == DiagnosticKind::UnreachableCode)
            .map(|diag| src[diag.range.clone()].to_string())
            .collect();
        assert!(
            !unreachable.iter().any(|slice| {
                slice.contains("iv_textid") || slice.contains("textid = iv_textid")
            }),
            "unexpected unreachable diagnostics: {:?}",
            unreachable
        );
    }

    #[test]
    fn routine_analysis_does_not_flag_classic_scalar_after_branch_join() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_flag TYPE i.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_value TYPE i.
    IF iv_flag = 1.
      lv_value = 1.
    ENDIF.
    DATA lv_copy TYPE i.
    lv_copy = lv_value.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_def_assign_if.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );

        let relevant: Vec<_> = use_before
            .iter()
            .filter(|slice| **slice == "lv_value")
            .cloned()
            .collect();

        assert!(relevant.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_does_not_flag_classic_scalar_after_loop_join() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_limit TYPE i.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_value TYPE i.
    WHILE iv_limit > 0.
      lv_value = iv_limit.
      EXIT.
    ENDWHILE.
    DATA lv_copy TYPE i.
    lv_copy = lv_value.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_def_assign_loop.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );

        let relevant: Vec<_> = use_before
            .iter()
            .filter(|slice| **slice == "lv_value")
            .cloned()
            .collect();

        assert!(relevant.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_skips_plain_if_condition_probes_for_scalar_values() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_action TYPE i.
    DATA lv_rc TYPE i.
    IF lv_action <> 1 OR lv_rc = 0.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_condition_probe_if.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );
        let relevant: Vec<_> = use_before
            .iter()
            .filter(|slice| matches!(slice.as_str(), "lv_action" | "lv_rc"))
            .cloned()
            .collect();

        assert!(relevant.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_skips_plain_while_condition_probes_for_scalar_values() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_len TYPE i VALUE 3.
    DATA lv_off TYPE i.
    WHILE lv_off < lv_len.
      lv_off = lv_len.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_condition_probe_while.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );
        let relevant: Vec<_> = use_before
            .iter()
            .filter(|slice| **slice == "lv_off")
            .cloned()
            .collect();

        assert!(relevant.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_does_not_flag_classic_scalar_after_try_catch_join() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_flag TYPE i.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_value TYPE i.
    TRY.
      IF iv_flag = 1.
        RAISE EXCEPTION TYPE cx_root.
      ENDIF.
      lv_value = 1.
    CATCH cx_root.
    ENDTRY.
    DATA lv_copy TYPE i.
    lv_copy = lv_value.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_def_assign_try.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );

        let relevant: Vec<_> = use_before
            .iter()
            .filter(|slice| **slice == "lv_value")
            .cloned()
            .collect();

        assert!(relevant.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_flags_read_table_inline_data_as_possibly_unassigned() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lt_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    READ TABLE lt_values INTO DATA(lv_value) INDEX 1.
    DATA lv_copy TYPE i.
    lv_copy = lv_value.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_read_table_inline_data.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );

        assert!(use_before.iter().any(|slice| slice.contains("lv_value")));
    }

    #[test]
    fn routine_analysis_allows_read_table_inline_data_after_sy_subrc_success_guard() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    TYPES: BEGIN OF ty_legisl,
             ruleid TYPE string,
             ifname TYPE string,
           END OF ty_legisl.
    DATA lt_legisl TYPE STANDARD TABLE OF ty_legisl WITH EMPTY KEY.
    DATA lv_ruleid TYPE string.
    lv_ruleid = 'A'.
    SORT lt_legisl BY ruleid ASCENDING.
    READ TABLE lt_legisl INTO DATA(ls_legisl) WITH KEY ruleid = lv_ruleid BINARY SEARCH.
    IF sy-subrc = 0.
      DATA lv_ifname TYPE string.
      lv_ifname = ls_legisl-ifname.
    ENDIF.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish(
            "file:///routine_read_table_inline_data_guarded.abap",
            1,
            src,
        );
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );

        assert!(
            !use_before.iter().any(|slice| slice.contains("ls_legisl")),
            "{use_before:?}"
        );
    }

    #[test]
    fn routine_analysis_flags_read_table_assigning_field_symbol_as_possibly_unbound() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lt_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    READ TABLE lt_values ASSIGNING FIELD-SYMBOL(<lv_value>) INDEX 1.
    DATA lv_copy TYPE i.
    lv_copy = <lv_value>.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_read_table_assigning.abap", 1, src);
        let unbound = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::PossiblyUnboundFieldSymbol,
        );

        assert!(unbound.iter().any(|slice| slice.contains("<lv_value>")));
    }

    #[test]
    fn routine_analysis_unassign_clears_field_symbol_without_reading_it() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lt_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    READ TABLE lt_values ASSIGNING FIELD-SYMBOL(<lv_value>) INDEX 1.
    UNASSIGN <lv_value>.
    DATA lv_copy TYPE i.
    lv_copy = <lv_value>.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_unassign_field_symbol.abap", 1, src);
        let unbound = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::PossiblyUnboundFieldSymbol,
        );

        assert_eq!(unbound, vec!["<lv_value>".to_string()]);
    }

    #[test]
    fn routine_analysis_allows_read_table_assigning_field_symbol_after_sy_subrc_success_guard() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lt_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    READ TABLE lt_values ASSIGNING FIELD-SYMBOL(<lv_value>) INDEX 1.
    IF sy-subrc = 0.
      DATA lv_copy TYPE i.
      lv_copy = <lv_value>.
    ENDIF.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_read_table_assigning_guarded.abap", 1, src);
        let unbound = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::PossiblyUnboundFieldSymbol,
        );

        assert!(unbound.is_empty(), "{unbound:?}");
    }

    #[test]
    fn routine_analysis_keeps_read_table_assigning_field_symbol_unbound_after_other_subrc_update() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lt_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    READ TABLE lt_values ASSIGNING FIELD-SYMBOL(<lv_value>) INDEX 1.
    FIND '1' IN '123'.
    IF sy-subrc = 0.
      DATA lv_copy TYPE i.
      lv_copy = <lv_value>.
    ENDIF.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish(
            "file:///routine_read_table_assigning_guarded_after_find.abap",
            1,
            src,
        );
        let unbound = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::PossiblyUnboundFieldSymbol,
        );

        assert!(unbound.iter().any(|slice| slice.contains("<lv_value>")));
    }

    #[test]
    fn routine_analysis_distinguishes_direct_and_dynamic_field_symbol_assign() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    TYPES: BEGIN OF ty_row,
             text TYPE string,
           END OF ty_row.
    DATA lv_value TYPE string.
    DATA ls_row TYPE ty_row.
    DATA lv_name TYPE string.
    FIELD-SYMBOLS <lv_text> TYPE string.
    FIELD-SYMBOLS <lv_dyn> TYPE string.
    lv_value = 'ok'.
    ls_row-text = lv_value.
    ASSIGN lv_value TO <lv_text>.
    DATA lv_copy TYPE string.
    lv_copy = <lv_text>.
    lv_name = 'TEXT'.
    ASSIGN COMPONENT lv_name OF STRUCTURE ls_row TO <lv_dyn>.
    lv_copy = <lv_dyn>.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_assign_field_symbol.abap", 1, src);
        let unbound = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::PossiblyUnboundFieldSymbol,
        );

        assert!(unbound.iter().any(|slice| slice.contains("<lv_dyn>")));
        assert!(!unbound.iter().any(|slice| slice.contains("<lv_text>")));
    }

    #[test]
    fn routine_analysis_treats_full_structure_selector_initialization_as_definite_assignment() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_job,
             jobname TYPE string,
             username TYPE string,
           END OF ty_job.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA ls_job TYPE ty_job.
    ls_job-jobname = 'BATCH'.
    ls_job-username = 'USER'.
    DATA ls_copy TYPE ty_job.
    ls_copy = ls_job.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_struct_selector_full.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );

        assert!(use_before.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_treats_explicit_structure_initializer_as_definite_assignment() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_job,
             jobname TYPE string,
             username TYPE string,
           END OF ty_job.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA ls_job TYPE ty_job VALUE IS INITIAL.
    DATA lv_username TYPE string.
    lv_username = ls_job-username.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_struct_explicit_init.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );

        assert!(use_before.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_treats_concatenate_into_target_as_definite_assignment() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_keyvalue TYPE string.
    CONCATENATE 'a' 'b' INTO lv_keyvalue.
    DATA lv_copy TYPE string.
    lv_copy = lv_keyvalue.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_concatenate_into.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );

        assert!(use_before.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_treats_move_to_structure_selector_as_definite_assignment() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_job,
             jobname TYPE string,
             username TYPE string,
           END OF ty_job.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA ls_job TYPE ty_job.
    MOVE 'BATCH' TO ls_job-jobname.
    ls_job-username = 'USER'.
    DATA ls_copy TYPE ty_job.
    ls_copy = ls_job.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_move_structure_selector.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );

        assert!(use_before.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_treats_move_corresponding_target_as_definite_assignment() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_job,
             jobname TYPE string,
             username TYPE string,
           END OF ty_job.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA ls_source TYPE ty_job.
    ls_source-jobname = 'BATCH'.
    ls_source-username = 'USER'.
    DATA ls_target TYPE ty_job.
    MOVE-CORRESPONDING ls_source TO ls_target.
    DATA ls_copy TYPE ty_job.
    ls_copy = ls_target.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_move_corresponding_target.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );

        assert!(use_before.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_allows_partial_structure_selector_initialization_for_whole_value_reads() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_job,
             jobname TYPE string,
             username TYPE string,
           END OF ty_job.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA ls_job TYPE ty_job.
    ls_job-jobname = 'BATCH'.
    DATA ls_copy TYPE ty_job.
    ls_copy = ls_job.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_struct_selector_partial.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );

        assert!(use_before.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_allows_reads_of_written_structure_selectors() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_job,
             jobname TYPE string,
             username TYPE string,
           END OF ty_job.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA ls_job TYPE ty_job.
    ls_job-jobname = 'BATCH'.
    DATA lv_jobname TYPE string.
    lv_jobname = ls_job-jobname.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_struct_selector_read_written.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );

        assert!(use_before.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_keeps_unwritten_structure_selector_reads_conservative() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_job,
             jobname TYPE string,
             username TYPE string,
           END OF ty_job.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA ls_job TYPE ty_job.
    ls_job-jobname = 'BATCH'.
    DATA lv_username TYPE string.
    lv_username = ls_job-username.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish(
            "file:///routine_struct_selector_read_unwritten.abap",
            1,
            src,
        );
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );

        assert!(use_before.iter().any(|slice| *slice == "ls_job-username"));
    }

    #[test]
    fn routine_analysis_allows_selector_initial_checks_and_reuses_field_guards() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_job,
             jobname TYPE string,
             username TYPE string,
           END OF ty_job.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lt_jobs TYPE STANDARD TABLE OF ty_job WITH EMPTY KEY.
    READ TABLE lt_jobs INTO DATA(ls_job) INDEX 1.
    IF ls_job-jobname IS NOT INITIAL.
      DATA lv_copy TYPE string.
      lv_copy = ls_job-jobname.
    ENDIF.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_selector_field_guard.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );

        assert!(use_before.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_allows_selector_read_after_structure_non_initial_guard() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_job,
             jobname TYPE string,
             username TYPE string,
           END OF ty_job.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lt_jobs TYPE STANDARD TABLE OF ty_job WITH EMPTY KEY.
    READ TABLE lt_jobs INTO DATA(ls_job) INDEX 1.
    IF ls_job IS NOT INITIAL.
      DATA lv_copy TYPE string.
      lv_copy = ls_job-jobname.
    ENDIF.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_selector_after_struct_guard.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );

        assert!(use_before.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_allows_reference_reads_after_bound_guard() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lo_http_client TYPE REF TO object.
    IF lo_http_client IS BOUND.
      DATA lo_copy TYPE REF TO object.
      lo_copy = lo_http_client.
    ENDIF.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_ref_bound_guard.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );

        let relevant: Vec<_> = use_before
            .iter()
            .filter(|slice| **slice == "lo_http_client")
            .cloned()
            .collect();

        assert!(relevant.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_allows_reference_reads_in_not_bound_else_branch() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lo_http_client TYPE REF TO object.
    IF lo_http_client IS NOT BOUND.
      RETURN.
    ELSE.
      DATA lo_copy TYPE REF TO object.
      lo_copy = lo_http_client.
    ENDIF.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_ref_not_bound_else.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );

        let relevant: Vec<_> = use_before
            .iter()
            .filter(|slice| **slice == "lo_http_client")
            .cloned()
            .collect();

        assert!(relevant.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_skips_clear_initial_checks_and_lines_builtin_for_tables() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lt_jobs TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    CLEAR lt_jobs.
    IF lt_jobs IS NOT INITIAL.
      IF lines( lt_jobs ) > 1.
        DATA lv_text TYPE string.
        lv_text = 'many'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_safe_table_probes.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );

        assert!(use_before.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_does_not_flag_typed_table_reads_after_table_mutation() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lt_jobs TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA lt_copy TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    IF lt_jobs IS NOT INITIAL.
      lt_copy = lt_jobs.
      DELETE lt_jobs WHERE table_line IS INITIAL.
      lt_copy = lt_jobs.
      IF lt_jobs IS NOT INITIAL.
        lt_copy = lt_jobs.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_table_guard_recheck.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );

        let relevant: Vec<_> = use_before
            .iter()
            .filter(|slice| **slice == "lt_jobs")
            .cloned()
            .collect();

        assert!(relevant.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_supports_legacy_table_body_initial_guards() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lt_jobs TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA lt_copy TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    IF lt_jobs[] IS NOT INITIAL.
      lt_copy = lt_jobs.
    ENDIF.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_legacy_table_body_guard.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );

        let relevant: Vec<_> = use_before
            .iter()
            .filter(|slice| **slice == "lt_jobs")
            .cloned()
            .collect();

        assert!(relevant.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_inherits_is_not_initial_guards_into_nested_loop_scopes() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lt_jobs TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA lv_limit TYPE i.
    IF lt_jobs IS NOT INITIAL.
      IF lv_limit IS NOT INITIAL.
        DO lv_limit TIMES.
          READ TABLE lt_jobs INTO DATA(lv_job) INDEX 1.
          IF lv_limit > 0.
            LOOP AT lt_jobs INTO DATA(lv_loop_job).
            ENDLOOP.
          ENDIF.
        ENDDO.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_nested_guarded_loops.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );
        let relevant: Vec<_> = use_before
            .iter()
            .filter(|slice| matches!(slice.as_str(), "lt_jobs" | "lv_limit"))
            .cloned()
            .collect();

        assert!(relevant.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_assigns_loop_inline_targets_and_skips_row_field_where_probes() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_evt,
             rep_evtid TYPE string,
             priority TYPE string,
             msguid_out TYPE string,
           END OF ty_evt.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lt_evt TYPE STANDARD TABLE OF ty_evt WITH EMPTY KEY.
    LOOP AT lt_evt INTO DATA(ls_evt) WHERE priority = 'X'.
      DATA lv_guid TYPE string.
      lv_guid = ls_evt-msguid_out.
      DELETE lt_evt WHERE rep_evtid = ls_evt-rep_evtid.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_loop_where_inline_target.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );
        let relevant: Vec<_> = use_before
            .iter()
            .filter(|slice| matches!(slice.as_str(), "priority" | "rep_evtid" | "ls_evt"))
            .cloned()
            .collect();

        assert!(relevant.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_preserves_pre_loop_assignments_at_nested_call_sites() {
        let store = DocumentStore::default();
        let src = "\
CLASS cx_demo DEFINITION.
ENDCLASS.
CLASS cx_demo IMPLEMENTATION.
ENDCLASS.

CLASS lcl_obj DEFINITION.
  PUBLIC SECTION.
    METHODS ping RAISING cx_demo.
ENDCLASS.
CLASS lcl_obj IMPLEMENTATION.
  METHOD ping.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA(lo_obj) = NEW lcl_obj( ).
    DATA lv_limit TYPE i.
    DATA lt_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    lv_limit = 1.
    DO lv_limit TIMES.
      READ TABLE lt_values INTO DATA(lv_value) INDEX 1.
      IF lv_value IS INITIAL.
        CLEAR lv_value.
        EXIT.
      ENDIF.
      IF lv_limit > 0.
        IF lt_values IS INITIAL.
          EXIT.
        ENDIF.
        LOOP AT lt_values INTO DATA(lv_loop).
          IF lv_limit > 0.
            TRY.
                lo_obj->ping( ).
              CATCH cx_demo.
            ENDTRY.
          ENDIF.
          CLEAR lv_loop.
        ENDLOOP.
      ENDIF.
    ENDDO.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_pre_loop_assignment.abap", 1, src);
        let routine = snapshot
            .routine_analysis()
            .routines_for_unit(snapshot.symbols.unit_id)
            .find(|routine| routine.descriptor.name.as_ref() == "run")
            .expect("method routine");
        let lo_obj = routine
            .dataflow_inputs
            .values
            .iter()
            .find(|value| value.name.as_ref() == "lo_obj")
            .expect("lo_obj dataflow value")
            .id;
        let call_block_idx = routine
            .cfg
            .blocks
            .iter()
            .position(|block| {
                block.instructions.iter().any(|instr_id| {
                    let instr = &routine.ir.instructions[instr_id.as_usize()];
                    matches!(instr.site, RoutineInstructionSite::Call { .. })
                        && src[instr.range.clone()].contains("lo_obj->ping")
                })
            })
            .expect("block containing lo_obj call");
        let call_entry = &routine.dataflow_result.block_entry[call_block_idx];

        assert!(
            call_entry.definitely_assigned_values.contains(&lo_obj),
            "{:?}",
            call_entry.definitely_assigned_values
        );
    }

    #[test]
    fn routine_analysis_propagates_nested_perform_changing_writes() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS fill EXPORTING ev_value TYPE string.
ENDCLASS.

CLASS lcl_dep IMPLEMENTATION.
  METHOD fill.
    ev_value = 'ok'.
  ENDMETHOD.
ENDCLASS.

FORM inner CHANGING cv_value TYPE string.
  DATA lo_dep TYPE REF TO lcl_dep.
  lo_dep = NEW lcl_dep( ).
  lo_dep->fill( IMPORTING ev_value = cv_value ).
ENDFORM.

FORM outer CHANGING cv_value TYPE string.
  PERFORM inner CHANGING cv_value.
ENDFORM.

FORM run.
  DATA lv_value TYPE string.
  PERFORM outer CHANGING lv_value.
  DATA lv_copy TYPE string.
  lv_copy = lv_value.
ENDFORM.

START-OF-SELECTION.
  PERFORM run.";

        let snapshot = store.publish("file:///routine_perform_changing_write.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );
        let relevant: Vec<_> = use_before
            .iter()
            .filter(|slice| **slice == "lv_value")
            .cloned()
            .collect();

        assert!(relevant.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_does_not_flag_conditional_for_iterator_initializer() {
        let store = DocumentStore::default();
        let src = "\
TYPES: stringtab TYPE STANDARD TABLE OF string WITH EMPTY KEY.

START-OF-SELECTION.
  DATA(lt_text) = VALUE stringtab( FOR n = 1 UNTIL n > 3 ( |{ n }| ) ).";

        let snapshot = store.publish("file:///routine_value_for_iterator.abap", 1, src);
        let use_before = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::UseBeforeDefiniteAssignment,
        );
        let relevant: Vec<_> = use_before
            .iter()
            .filter(|slice| **slice == "n")
            .cloned()
            .collect();

        assert!(relevant.is_empty(), "{use_before:?}");
    }

    #[test]
    fn routine_analysis_flags_dead_store_on_overwrite_before_read() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_value TYPE i.
    IF 1 = 1.
      lv_value = 1.
    ENDIF.
    lv_value = 2.
    IF lv_value > 0.
    ENDIF.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_dead_store_overwrite.abap", 1, src);
        let dead_store = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::DeadStore,
        );

        assert_eq!(
            dead_store
                .iter()
                .filter(|slice| slice.as_str() == "lv_value")
                .count(),
            1,
            "{dead_store:?}"
        );
    }

    #[test]
    fn routine_analysis_flags_dead_store_on_last_write_before_return() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_unused TYPE i.
    lv_unused = 1.
    RETURN.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_dead_store_return.abap", 1, src);
        let dead_store = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::DeadStore,
        );

        assert!(
            dead_store.iter().any(|slice| slice == "lv_unused"),
            "{dead_store:?}"
        );
    }

    #[test]
    fn routine_analysis_flags_dead_store_in_global_declarations() {
        let store = DocumentStore::default();
        let src = "\
REPORT zdead_store.

DATA gv_unused TYPE i.
gv_unused = 1.";

        let snapshot = store.publish("file:///global_dead_store.abap", 1, src);
        let dead_store = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::DeadStore,
        );
        let dead_store_messages: Vec<_> = snapshot
            .symbols
            .diagnostics
            .iter()
            .filter(|diag| diag.kind == DiagnosticKind::DeadStore)
            .map(|diag| diag.message.as_str())
            .collect();

        assert!(
            dead_store.iter().any(|slice| slice == "gv_unused"),
            "{dead_store:?}"
        );
        assert!(
            dead_store_messages.iter().any(|message| {
                *message
                    == "write to global variable 'gv_unused' is never read in global declarations"
            }),
            "{dead_store_messages:?}"
        );
    }

    #[test]
    fn lint_suppression_pragma_alias_hides_dead_store_lint() {
        let store = DocumentStore::default();
        let src = "\
REPORT zdead_store.

DATA gv_unused TYPE i.
gv_unused = 1 ##NEEDED.";

        let snapshot = store.publish("file:///global_dead_store_needed.abap", 1, src);

        assert!(
            snapshot
                .symbols
                .diagnostics
                .iter()
                .any(|diag| diag.kind == DiagnosticKind::DeadStore),
            "raw diagnostics should still retain the routine finding"
        );
        assert!(
            snapshot
                .lint_diagnostics()
                .iter()
                .all(|diag| diag.id != ABAP_LSP_DEAD_STORE),
            "{:#?}",
            snapshot.lint_diagnostics()
        );
    }

    #[test]
    fn local_lint_pack_flags_select_star() {
        let store = DocumentStore::default();
        let src = "SELECT * FROM scarr INTO TABLE @DATA(lt_scarr).";

        let snapshot = store.publish("file:///lint_select_star.abap", 1, src);
        let select_star = lint_slices(src, &snapshot, ABAP_LSP_SELECT_STAR);

        assert_eq!(select_star, vec!["*".to_string()]);
    }

    #[test]
    fn local_lint_pack_honors_select_star_code_inspector_alias() {
        let store = DocumentStore::default();
        let src = "SELECT * FROM scarr INTO TABLE @DATA(lt_scarr). \"#EC CI_ALL_FIELDS_NEEDED";

        let snapshot = store.publish("file:///lint_select_star_suppressed.abap", 1, src);

        assert!(
            snapshot
                .lint_diagnostics()
                .iter()
                .all(|diag| diag.id != ABAP_LSP_SELECT_STAR),
            "{:#?}",
            snapshot.lint_diagnostics()
        );
    }

    #[test]
    fn lint_allow_next_line_comment_layout_suppresses_only_next_select_star() {
        let store = DocumentStore::default();
        let src = "\
REPORT zlint.
\" generated compatibility query
\" abap-lsp:allow-next-line(abap-lsp.select-star)
* keep this comment between suppression and statement
SELECT * FROM scarr INTO TABLE @DATA(lt_scarr).

SELECT * FROM spfli INTO TABLE @DATA(lt_spfli).";

        let snapshot = store.publish("file:///lint_allow_next_line_select_star.abap", 1, src);
        let select_star = lint_slices(src, &snapshot, ABAP_LSP_SELECT_STAR);

        assert_eq!(select_star, vec!["*".to_string()]);
    }

    #[test]
    fn lint_allow_file_header_comment_layout_suppresses_all_select_star() {
        let store = DocumentStore::default();
        let src = "\
*----------------------------------------------------------------------*
* generated extractor
* abap-lsp:allow-file(abap-lsp.select-star)
*----------------------------------------------------------------------*
REPORT zlint.

SELECT * FROM scarr INTO TABLE @DATA(lt_scarr).
SELECT * FROM spfli INTO TABLE @DATA(lt_spfli).";

        let snapshot = store.publish("file:///lint_allow_file_select_star.abap", 1, src);

        assert!(
            snapshot
                .lint_diagnostics()
                .iter()
                .all(|diag| diag.id != ABAP_LSP_SELECT_STAR),
            "{:#?}",
            snapshot.lint_diagnostics()
        );
    }

    #[test]
    fn lint_report_suppressed_keeps_source_suppression_metadata() {
        let store = DocumentStore::default();
        store.set_lint_policy(LintPolicy::default().with_report_suppressed(true));
        let src = "\
REPORT zlint.
\" abap-lsp:allow-next-line(abap-lsp.select-star)
SELECT * FROM scarr INTO TABLE @DATA(lt_scarr).";

        let snapshot = store.publish("file:///lint_report_suppressed_select_star.abap", 1, src);
        let diagnostic = snapshot
            .lint_diagnostics()
            .iter()
            .find(|diag| diag.id == ABAP_LSP_SELECT_STAR)
            .expect("suppressed select-star lint");
        let suppression = diagnostic
            .suppression
            .as_ref()
            .expect("source suppression metadata");

        assert!(diagnostic.suppressed);
        assert_eq!(diagnostic.level, super::LintLevel::Info);
        assert_eq!(suppression.kind, LintSuppressionKind::AbapLspAllow);
        assert_eq!(
            suppression.token,
            "\" abap-lsp:allow-next-line(abap-lsp.select-star)"
        );
    }

    #[test]
    fn local_lint_pack_flags_select_inside_loop() {
        let store = DocumentStore::default();
        let src = "\
DATA lt_ids TYPE STANDARD TABLE OF i WITH EMPTY KEY.

LOOP AT lt_ids INTO DATA(lv_id).
  SELECT SINGLE carrid FROM scarr INTO @DATA(lv_carrid).
ENDLOOP.";

        let snapshot = store.publish("file:///lint_select_in_loop.abap", 1, src);
        let select_in_loop = lint_slices(src, &snapshot, ABAP_LSP_SELECT_IN_LOOP);

        assert!(
            select_in_loop
                .iter()
                .any(|slice| slice.contains("FROM scarr")),
            "{select_in_loop:?}"
        );
    }

    #[test]
    fn local_lint_pack_flags_select_single_without_full_known_key() {
        let store = DocumentStore::default();
        let ddic_src = "\
TYPES: BEGIN OF zflight,
         mandt  TYPE c LENGTH 3, \" primary key; client
         carrid TYPE c LENGTH 3, \" primary key; carrier
         connid TYPE c LENGTH 4, \" primary key; connection
         fldate TYPE d,
       END OF zflight.";
        let main_src = "\
DATA lv_carrid TYPE c LENGTH 3.

SELECT SINGLE carrid
  FROM zflight
  INTO @DATA(lv_carrid_out)
  WHERE carrid = @lv_carrid.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///packages/z/ddic-table/zflight.abap"),
                version: 1,
                text: Arc::from(ddic_src),
                is_dependency: true,
                object_name: Some(Arc::from("zflight")),
            },
            DocumentInput {
                uri: Arc::from("file:///select_single_missing_key.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots
            .get("file:///select_single_missing_key.abap")
            .expect("main snapshot");
        let diagnostics = snapshot
            .lint_diagnostics()
            .iter()
            .filter(|diag| diag.id == ABAP_LSP_SELECT_SINGLE_WITHOUT_FULL_KEY)
            .collect::<Vec<_>>();

        assert_eq!(diagnostics.len(), 1, "{:#?}", snapshot.lint_diagnostics());
        assert_eq!(diagnostics[0].level, super::LintLevel::Info);
        assert!(
            diagnostics[0].message.contains("connid") && !diagnostics[0].message.contains("mandt"),
            "{:#?}",
            diagnostics[0]
        );
        assert!(main_src[diagnostics[0].range.clone()].contains("WHERE"));
    }

    #[test]
    fn local_lint_pack_accepts_select_single_with_full_known_key() {
        let store = DocumentStore::default();
        let ddic_src = "\
TYPES: BEGIN OF zflight,
         mandt  TYPE c LENGTH 3, \" primary key; client
         carrid TYPE c LENGTH 3, \" primary key; carrier
         connid TYPE c LENGTH 4, \" primary key; connection
         fldate TYPE d,
       END OF zflight.";
        let main_src = "\
DATA lv_carrid TYPE c LENGTH 3.
DATA lv_connid TYPE c LENGTH 4.

SELECT SINGLE carrid
  FROM zflight
  INTO @DATA(lv_carrid_out)
  WHERE carrid = @lv_carrid
    AND connid = @lv_connid.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///packages/z/ddic-table/zflight.abap"),
                version: 1,
                text: Arc::from(ddic_src),
                is_dependency: true,
                object_name: Some(Arc::from("zflight")),
            },
            DocumentInput {
                uri: Arc::from("file:///select_single_full_key.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots
            .get("file:///select_single_full_key.abap")
            .expect("main snapshot");

        assert!(
            snapshot
                .lint_diagnostics()
                .iter()
                .all(|diag| diag.id != ABAP_LSP_SELECT_SINGLE_WITHOUT_FULL_KEY),
            "{:#?}",
            snapshot.lint_diagnostics()
        );
    }

    #[test]
    fn local_lint_pack_skips_select_single_when_key_metadata_is_missing() {
        let store = DocumentStore::default();
        let src = "\
DATA lv_carrid TYPE c LENGTH 3.

SELECT SINGLE carrid
  FROM scarr
  INTO @DATA(lv_carrid_out)
  WHERE carrid = @lv_carrid.";

        let snapshot = store.publish("file:///select_single_no_key_metadata.abap", 1, src);

        assert!(
            snapshot
                .lint_diagnostics()
                .iter()
                .all(|diag| diag.id != ABAP_LSP_SELECT_SINGLE_WITHOUT_FULL_KEY),
            "{:#?}",
            snapshot.lint_diagnostics()
        );
    }

    #[test]
    fn lint_allow_next_line_suppresses_select_single_without_full_key() {
        let store = DocumentStore::default();
        store.set_lint_policy(LintPolicy::default().with_report_suppressed(true));
        let ddic_src = "\
TYPES: BEGIN OF zflight,
         mandt  TYPE c LENGTH 3, \" primary key; client
         carrid TYPE c LENGTH 3, \" primary key; carrier
         connid TYPE c LENGTH 4, \" primary key; connection
       END OF zflight.";
        let main_src = "\
DATA lv_carrid TYPE c LENGTH 3.

\" abap-lsp:allow-next-line(abap-lsp.select-single-without-full-key)
SELECT SINGLE carrid
  FROM zflight
  INTO @DATA(lv_carrid_out)
  WHERE carrid = @lv_carrid.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///packages/z/ddic-table/zflight.abap"),
                version: 1,
                text: Arc::from(ddic_src),
                is_dependency: true,
                object_name: Some(Arc::from("zflight")),
            },
            DocumentInput {
                uri: Arc::from("file:///select_single_suppressed.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots
            .get("file:///select_single_suppressed.abap")
            .expect("main snapshot");
        let diagnostic = snapshot
            .lint_diagnostics()
            .iter()
            .find(|diag| diag.id == ABAP_LSP_SELECT_SINGLE_WITHOUT_FULL_KEY)
            .expect("reported suppressed lint");
        let suppression = diagnostic.suppression.as_ref().expect("suppression");

        assert!(diagnostic.suppressed);
        assert_eq!(suppression.kind, LintSuppressionKind::AbapLspAllow);
        assert_eq!(
            suppression.token,
            "\" abap-lsp:allow-next-line(abap-lsp.select-single-without-full-key)"
        );
    }

    #[test]
    fn local_lint_pack_flags_for_all_entries_without_guard() {
        let store = DocumentStore::default();
        let src = "\
DATA lt_keys TYPE STANDARD TABLE OF string WITH EMPTY KEY.

SELECT carrid
  FROM scarr
  INTO TABLE @DATA(lt_scarr)
  FOR ALL ENTRIES IN @lt_keys
  WHERE carrid = @lt_keys.";

        let snapshot = store.publish("file:///lint_fae_unguarded.abap", 1, src);
        let fae = lint_slices(src, &snapshot, ABAP_LSP_FOR_ALL_ENTRIES_WITHOUT_GUARD);

        assert_eq!(
            fae,
            vec!["FOR ALL ENTRIES IN @lt_keys".to_string()],
            "{fae:?}"
        );
    }

    #[test]
    fn local_lint_pack_accepts_enclosing_for_all_entries_guard() {
        let store = DocumentStore::default();
        let src = "\
DATA lt_keys TYPE STANDARD TABLE OF string WITH EMPTY KEY.

IF lt_keys IS NOT INITIAL.
  SELECT carrid
    FROM scarr
    INTO TABLE @DATA(lt_scarr)
    FOR ALL ENTRIES IN @lt_keys
    WHERE carrid = @lt_keys.
ENDIF.";

        let snapshot = store.publish("file:///lint_fae_guarded.abap", 1, src);
        let fae = lint_slices(src, &snapshot, ABAP_LSP_FOR_ALL_ENTRIES_WITHOUT_GUARD);

        assert!(fae.is_empty(), "{fae:?}");
    }

    #[test]
    fn local_lint_pack_accepts_enclosing_for_all_entries_lines_positive_guard() {
        let store = DocumentStore::default();
        let src = "\
DATA lt_keys TYPE STANDARD TABLE OF string WITH EMPTY KEY.

IF lines( lt_keys ) > 0.
  SELECT carrid
    FROM scarr
    INTO TABLE @DATA(lt_scarr)
    FOR ALL ENTRIES IN @lt_keys
    WHERE carrid = @lt_keys.
ENDIF.";

        let snapshot = store.publish("file:///lint_fae_lines_positive_guarded.abap", 1, src);
        let fae = lint_slices(src, &snapshot, ABAP_LSP_FOR_ALL_ENTRIES_WITHOUT_GUARD);

        assert!(fae.is_empty(), "{fae:?}");
    }

    #[test]
    fn local_lint_pack_accepts_for_all_entries_initial_return_guard() {
        let store = DocumentStore::default();
        let src = "\
DATA lt_keys TYPE STANDARD TABLE OF string WITH EMPTY KEY.

IF lt_keys IS INITIAL.
  RETURN.
ENDIF.

SELECT carrid
  FROM scarr
  INTO TABLE @DATA(lt_scarr)
  FOR ALL ENTRIES IN @lt_keys
  WHERE carrid = @lt_keys.";

        let snapshot = store.publish("file:///lint_fae_return_guarded.abap", 1, src);
        let fae = lint_slices(src, &snapshot, ABAP_LSP_FOR_ALL_ENTRIES_WITHOUT_GUARD);

        assert!(fae.is_empty(), "{fae:?}");
    }

    #[test]
    fn local_lint_pack_accepts_for_all_entries_lines_zero_return_guard() {
        let store = DocumentStore::default();
        let src = "\
DATA lt_keys TYPE STANDARD TABLE OF string WITH EMPTY KEY.

IF lines( lt_keys ) = 0.
  RETURN.
ENDIF.

SELECT carrid
  FROM scarr
  INTO TABLE @DATA(lt_scarr)
  FOR ALL ENTRIES IN @lt_keys
  WHERE carrid = @lt_keys.";

        let snapshot = store.publish("file:///lint_fae_lines_zero_guarded.abap", 1, src);
        let fae = lint_slices(src, &snapshot, ABAP_LSP_FOR_ALL_ENTRIES_WITHOUT_GUARD);

        assert!(fae.is_empty(), "{fae:?}");
    }

    #[test]
    fn local_lint_pack_flags_dynamic_open_sql_fragments() {
        let store = DocumentStore::default();
        let src = "\
DATA lv_fields TYPE string.
DATA lv_table TYPE string.
DATA lv_where TYPE string.

SELECT (lv_fields)
  FROM (lv_table)
  INTO TABLE @DATA(lt_rows)
  WHERE (lv_where).";

        let snapshot = store.publish("file:///lint_dynamic_open_sql.abap", 1, src);
        let dynamic_sql = lint_slices(src, &snapshot, ABAP_LSP_DYNAMIC_OPEN_SQL);

        assert!(
            dynamic_sql.iter().any(|slice| slice == "(lv_fields)")
                && dynamic_sql.iter().any(|slice| slice == "(lv_table)")
                && dynamic_sql.iter().any(|slice| slice == "WHERE (lv_where)"),
            "{dynamic_sql:?}"
        );
    }

    #[test]
    fn local_lint_pack_flags_ignored_authority_check_result() {
        let store = DocumentStore::default();
        let src = "\
AUTHORITY-CHECK OBJECT 'S_CARRID'
  ID 'ACTVT' FIELD '03'.
WRITE 'ok'.";

        let snapshot = store.publish("file:///lint_ignored_authority_check.abap", 1, src);
        let ignored = lint_slices(src, &snapshot, ABAP_LSP_IGNORED_AUTHORITY_CHECK);

        assert_eq!(
            ignored,
            vec!["AUTHORITY-CHECK OBJECT 'S_CARRID'\n  ID 'ACTVT' FIELD '03'.".to_string()],
            "{ignored:?}"
        );
    }

    #[test]
    fn local_lint_pack_accepts_authority_check_subrc_guard() {
        let store = DocumentStore::default();
        let src = "\
AUTHORITY-CHECK OBJECT 'S_CARRID'
  ID 'ACTVT' FIELD '03'.
IF sy-subrc <> 0.
  RETURN.
ENDIF.";

        let snapshot = store.publish("file:///lint_authority_check_guarded.abap", 1, src);
        let ignored = lint_slices(src, &snapshot, ABAP_LSP_IGNORED_AUTHORITY_CHECK);

        assert!(ignored.is_empty(), "{ignored:?}");
    }

    #[test]
    fn local_lint_pack_flags_call_function_subrc_overwritten_before_check() {
        let store = DocumentStore::default();
        let src = "\
CALL FUNCTION 'Z_DEMO'
  EXCEPTIONS
    failed = 1.
SELECT SINGLE carrid FROM scarr INTO @DATA(lv_carrid).
IF sy-subrc <> 0.
  RETURN.
ENDIF.";

        let snapshot = store.publish("file:///lint_call_function_subrc_overwritten.abap", 1, src);
        let ignored = lint_slices(src, &snapshot, ABAP_LSP_IGNORED_CALL_FUNCTION_RESULT);

        assert_eq!(
            ignored,
            vec!["CALL FUNCTION 'Z_DEMO'\n  EXCEPTIONS\n    failed = 1.".to_string()],
            "{ignored:?}"
        );
    }

    #[test]
    fn local_lint_pack_accepts_call_function_subrc_guard() {
        let store = DocumentStore::default();
        let src = "\
CALL FUNCTION 'Z_DEMO'
  EXCEPTIONS
    failed = 1.
IF sy-subrc <> 0.
  RETURN.
ENDIF.
SELECT SINGLE carrid FROM scarr INTO @DATA(lv_carrid).";

        let snapshot = store.publish("file:///lint_call_function_subrc_guarded.abap", 1, src);
        let ignored = lint_slices(src, &snapshot, ABAP_LSP_IGNORED_CALL_FUNCTION_RESULT);

        assert!(ignored.is_empty(), "{ignored:?}");
    }

    #[test]
    fn local_lint_pack_does_not_flag_call_function_without_overwrite_or_ignored_result() {
        let store = DocumentStore::default();
        let src = "\
CALL FUNCTION 'Z_DEMO'
  EXCEPTIONS
    failed = 1.
WRITE 'ok'.";

        let snapshot = store.publish("file:///lint_call_function_no_proof.abap", 1, src);
        let ignored = lint_slices(src, &snapshot, ABAP_LSP_IGNORED_CALL_FUNCTION_RESULT);

        assert!(ignored.is_empty(), "{ignored:?}");
    }

    #[test]
    fn local_lint_pack_flags_call_function_output_result_without_later_read() {
        let store = DocumentStore::default();
        let src = "\
FORM run.
  DATA lv_result TYPE string.
  CALL FUNCTION 'Z_DEMO'
    IMPORTING
      ev_result = lv_result.
ENDFORM.";

        let snapshot = store.publish("file:///lint_call_function_ignored_output.abap", 1, src);
        let ignored = lint_slices(src, &snapshot, ABAP_LSP_IGNORED_CALL_FUNCTION_RESULT);

        assert_eq!(
            ignored,
            vec!["CALL FUNCTION 'Z_DEMO'\n    IMPORTING\n      ev_result = lv_result.".to_string()],
            "{ignored:?}"
        );
    }

    #[test]
    fn local_lint_pack_accepts_call_function_output_result_read() {
        let store = DocumentStore::default();
        let src = "\
FORM run.
  DATA lv_result TYPE string.
  CALL FUNCTION 'Z_DEMO'
    IMPORTING
      ev_result = lv_result.
  WRITE lv_result.
  SELECT SINGLE carrid FROM scarr INTO @DATA(lv_carrid).
ENDFORM.";

        let snapshot = store.publish("file:///lint_call_function_output_read.abap", 1, src);
        let ignored = lint_slices(src, &snapshot, ABAP_LSP_IGNORED_CALL_FUNCTION_RESULT);

        assert!(ignored.is_empty(), "{ignored:?}");
    }

    #[test]
    fn local_lint_pack_honors_call_function_result_allow_comment() {
        let store = DocumentStore::default();
        let src = "\
CALL FUNCTION 'Z_DEMO'
  EXCEPTIONS
    failed = 1. \" abap-lsp:allow(abap-lsp.ignored-call-function-result)
SELECT SINGLE carrid FROM scarr INTO @DATA(lv_carrid).";

        let snapshot = store.publish("file:///lint_call_function_allowed.abap", 1, src);

        assert!(
            snapshot
                .lint_diagnostics()
                .iter()
                .all(|diag| diag.id != ABAP_LSP_IGNORED_CALL_FUNCTION_RESULT),
            "{:#?}",
            snapshot.lint_diagnostics()
        );
    }

    #[test]
    fn routine_analysis_keeps_loop_carried_variable_writes_live() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_limit TYPE i.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_total TYPE i.
    DATA lv_remaining TYPE i.
    lv_total = 0.
    lv_remaining = iv_limit.
    WHILE lv_remaining > 0.
      lv_total = lv_total + 1.
      lv_remaining = lv_remaining - 1.
    ENDWHILE.
    IF lv_total > 0.
    ENDIF.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_dead_store_loop.abap", 1, src);
        let dead_store = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::DeadStore,
        );

        assert!(
            dead_store.iter().all(|slice| slice != "lv_total"),
            "{dead_store:?}"
        );
    }

    #[test]
    fn routine_analysis_keeps_branch_merge_writes_live() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_flag TYPE i.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_value TYPE i.
    IF iv_flag = 1.
      lv_value = 1.
    ELSE.
      lv_value = 2.
    ENDIF.
    IF lv_value > 0.
    ENDIF.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_dead_store_branch.abap", 1, src);
        let dead_store = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::DeadStore,
        );

        assert!(
            dead_store.iter().all(|slice| slice != "lv_value"),
            "{dead_store:?}"
        );
    }

    #[test]
    fn routine_analysis_treats_is_initial_check_as_dead_store_read() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_flag TYPE abap_bool.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_value TYPE abap_bool.
    IF iv_flag = abap_true.
      CLEAR lv_value.
    ELSE.
      lv_value = abap_true.
    ENDIF.
    IF lv_value IS INITIAL.
    ENDIF.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_dead_store_is_initial.abap", 1, src);
        let dead_store = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::DeadStore,
        );

        assert!(
            dead_store.iter().all(|slice| slice != "lv_value"),
            "{dead_store:?}"
        );
    }

    #[test]
    fn routine_analysis_suppresses_dead_store_for_changing_and_outward_visible_state() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run CHANGING cv_value TYPE i.
  PRIVATE SECTION.
    DATA mv_state TYPE i.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    cv_value = 1.
    mv_state = 1.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_dead_store_outward.abap", 1, src);
        let dead_store = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::DeadStore,
        );

        assert!(dead_store.is_empty(), "{dead_store:?}");
    }

    #[test]
    fn routine_analysis_suppresses_dead_store_around_changing_calls() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS touch CHANGING cv_value TYPE i.
ENDCLASS.

CLASS lcl_dep IMPLEMENTATION.
  METHOD touch.
    cv_value = cv_value + 1.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lo_dep TYPE REF TO lcl_dep.
    DATA lv_value TYPE i.
    lo_dep = NEW lcl_dep( ).
    lv_value = 1.
    lo_dep->touch( CHANGING cv_value = lv_value ).
    lv_value = 2.
  ENDMETHOD.
ENDCLASS.";

        let snapshot = store.publish("file:///routine_dead_store_changing_call.abap", 1, src);
        let dead_store = diagnostic_slices(
            src,
            &snapshot.symbols.diagnostics,
            DiagnosticKind::DeadStore,
        );

        assert!(dead_store.is_empty(), "{dead_store:?}");
    }

    #[test]
    fn reuses_cross_document_type_validation_for_unchanged_publish() {
        let store = DocumentStore::default();
        let dep_src = "\
CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS take IMPORTING it_values TYPE STANDARD TABLE OF i WITH EMPTY KEY.
ENDCLASS.

CLASS zcl_dep IMPLEMENTATION.
  METHOD take.
  ENDMETHOD.
ENDCLASS.";
        let main_src = "\
DATA lo_dep TYPE REF TO zcl_dep.
DATA lv_value TYPE i.

START-OF-SELECTION.
  lo_dep->take( it_values = lv_value ).";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///dep.abap"),
                version: 1,
                text: Arc::from(dep_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let first = Arc::clone(snapshots.get("file:///main.abap").expect("main snapshot"));

        assert!(first.symbols.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::IncompatibleArgumentType
                && diag.message.contains("it_values")
        }));

        let second = store.publish("file:///main.abap", 2, main_src);
        assert_eq!(second.version, 2);
        assert!(Arc::ptr_eq(&first.parse, &second.parse));
        assert!(Arc::ptr_eq(&first.symbols, &second.symbols));
        assert!(Arc::ptr_eq(&first.project, &second.project));
        assert!(second.symbols.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::IncompatibleArgumentType
                && diag.message.contains("it_values")
        }));
    }

    #[test]
    fn body_only_publish_marks_only_edited_uri_dirty() {
        let store = DocumentStore::default();
        let provider_v1 = "\
CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS value RETURNING VALUE(rv_value) TYPE i.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
  METHOD value.
    rv_value = 1.
  ENDMETHOD.
ENDCLASS.";
        let provider_v2 = provider_v1.replace("rv_value = 1.", "rv_value = 2.");
        let consumer = "\
DATA lo_dep TYPE REF TO zcl_dep.
START-OF-SELECTION.
  lo_dep->value( ).";
        let unrelated = "REPORT zother.";

        store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///dep.abap"),
                version: 1,
                text: Arc::from(provider_v1),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///consumer.abap"),
                version: 1,
                text: Arc::from(consumer),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///other.abap"),
                version: 1,
                text: Arc::from(unrelated),
                is_dependency: false,
                object_name: None,
            },
        ]);

        store.publish_input(DocumentInput {
            uri: Arc::from("file:///dep.abap"),
            version: 2,
            text: Arc::from(provider_v2),
            is_dependency: false,
            object_name: None,
        });

        let dirty = store.last_dirty_uris();
        assert_eq!(dirty.len(), 1);
        assert!(dirty.contains("file:///dep.abap"));
        let metrics = store.last_analysis_metrics().expect("analysis metrics");
        assert_eq!(metrics.2, 1);
    }

    #[test]
    fn exported_signature_publish_marks_dependents_dirty() {
        let store = DocumentStore::default();
        let provider_v1 = "\
CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS value RETURNING VALUE(rv_value) TYPE i.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
  METHOD value.
    rv_value = 1.
  ENDMETHOD.
ENDCLASS.";
        let provider_v2 = "\
CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS value RETURNING VALUE(rv_value) TYPE i.
    METHODS extra.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
  METHOD value.
    rv_value = 1.
  ENDMETHOD.
  METHOD extra.
  ENDMETHOD.
ENDCLASS.";
        let consumer = "\
DATA lo_dep TYPE REF TO zcl_dep.
START-OF-SELECTION.
  lo_dep->value( ).";

        store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///dep.abap"),
                version: 1,
                text: Arc::from(provider_v1),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///consumer.abap"),
                version: 1,
                text: Arc::from(consumer),
                is_dependency: false,
                object_name: None,
            },
        ]);

        store.publish_input(DocumentInput {
            uri: Arc::from("file:///dep.abap"),
            version: 2,
            text: Arc::from(provider_v2),
            is_dependency: false,
            object_name: None,
        });

        let dirty = store.last_dirty_uris();
        assert!(dirty.contains("file:///dep.abap"));
        assert!(dirty.contains("file:///consumer.abap"));
    }

    #[test]
    fn batch_publish_updates_only_changed_inputs_and_dependents() {
        let store = DocumentStore::default();
        let provider_v1 = "\
CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS value RETURNING VALUE(rv_value) TYPE i.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
  METHOD value.
    rv_value = 1.
  ENDMETHOD.
ENDCLASS.";
        let provider_v2 = provider_v1.replace("rv_value = 1.", "rv_value = 2.");
        let unrelated_v1 = "REPORT zother.\nWRITE 'one'.";
        let unrelated_v2 = "REPORT zother.\nWRITE 'two'.";
        let consumer = "\
DATA lo_dep TYPE REF TO zcl_dep.
START-OF-SELECTION.
  lo_dep->value( ).";

        store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///dep.abap"),
                version: 1,
                text: Arc::from(provider_v1),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///consumer.abap"),
                version: 1,
                text: Arc::from(consumer),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///other.abap"),
                version: 1,
                text: Arc::from(unrelated_v1),
                is_dependency: false,
                object_name: None,
            },
        ]);

        store.publish_inputs(vec![
            DocumentInput {
                uri: Arc::from("file:///dep.abap"),
                version: 2,
                text: Arc::from(provider_v2.as_str()),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///other.abap"),
                version: 2,
                text: Arc::from(unrelated_v2),
                is_dependency: false,
                object_name: None,
            },
        ]);

        let dirty = store.last_dirty_uris();
        assert_eq!(dirty.len(), 2);
        assert!(dirty.contains("file:///dep.abap"));
        assert!(dirty.contains("file:///other.abap"));
        assert!(!dirty.contains("file:///consumer.abap"));
        let metrics = store.last_analysis_metrics().expect("analysis metrics");
        assert_eq!(metrics.2, 2);
    }

    #[test]
    fn preview_body_only_edit_reanalyzes_only_changed_unit() {
        let store = DocumentStore::default();
        let provider_v1 = "\
CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS value RETURNING VALUE(rv_value) TYPE i.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
  METHOD value.
    rv_value = 1.
  ENDMETHOD.
ENDCLASS.";
        let provider_v2 = provider_v1.replace("rv_value = 1.", "rv_value = 2.");
        let consumer = "\
DATA lo_dep TYPE REF TO zcl_dep.
START-OF-SELECTION.
  lo_dep->value( ).";

        store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///dep.abap"),
                version: 1,
                text: Arc::from(provider_v1),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///consumer.abap"),
                version: 1,
                text: Arc::from(consumer),
                is_dependency: false,
                object_name: None,
            },
        ]);

        let committed_provider = store.get("file:///dep.abap").expect("committed provider");
        let committed_consumer = store
            .get("file:///consumer.abap")
            .expect("committed consumer");
        let preview = store.preview_publish_input(DocumentInput {
            uri: Arc::from("file:///dep.abap"),
            version: 2,
            text: Arc::from(provider_v2.as_str()),
            is_dependency: false,
            object_name: None,
        });

        let preview_metrics = store
            .last_preview_metrics_snapshot()
            .expect("preview metrics");
        assert_eq!(preview_metrics.parse_count, 1);
        assert_eq!(preview_metrics.local_phase_count, 1);
        assert!(!preview_metrics.committed_context_only);
        assert!(!preview_metrics.fell_back_to_single_document);
        assert_eq!(preview.version, 2);
        assert!(preview.text.contains("rv_value = 2."));
        assert_eq!(store.get("file:///dep.abap").unwrap().version, 1);
        assert!(Arc::ptr_eq(
            &committed_provider,
            &store.get("file:///dep.abap").expect("stored provider"),
        ));
        assert!(Arc::ptr_eq(
            &committed_consumer,
            &store.get("file:///consumer.abap").expect("stored consumer"),
        ));
    }

    #[test]
    fn preview_signature_change_keeps_dependents_on_committed_state() {
        let store = DocumentStore::default();
        let provider_v1 = "\
CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS value RETURNING VALUE(rv_value) TYPE i.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
  METHOD value.
    rv_value = 1.
  ENDMETHOD.
ENDCLASS.";
        let provider_v2 = "\
CLASS zcl_dep DEFINITION.
  PUBLIC SECTION.
    METHODS value RETURNING VALUE(rv_value) TYPE i.
    METHODS extra.
ENDCLASS.
CLASS zcl_dep IMPLEMENTATION.
  METHOD value.
    rv_value = 1.
  ENDMETHOD.
  METHOD extra.
  ENDMETHOD.
ENDCLASS.";
        let consumer = "\
DATA lo_dep TYPE REF TO zcl_dep.
START-OF-SELECTION.
  lo_dep->value( ).";

        store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///dep.abap"),
                version: 1,
                text: Arc::from(provider_v1),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///consumer.abap"),
                version: 1,
                text: Arc::from(consumer),
                is_dependency: false,
                object_name: None,
            },
        ]);

        let committed_consumer = store
            .get("file:///consumer.abap")
            .expect("committed consumer");
        let preview = store.preview_publish_input(DocumentInput {
            uri: Arc::from("file:///dep.abap"),
            version: 2,
            text: Arc::from(provider_v2),
            is_dependency: false,
            object_name: None,
        });

        let preview_metrics = store
            .last_preview_metrics_snapshot()
            .expect("preview metrics");
        assert_eq!(preview_metrics.parse_count, 1);
        assert_eq!(preview_metrics.local_phase_count, 1);
        assert!(!preview_metrics.committed_context_only);
        assert!(!preview_metrics.fell_back_to_single_document);
        assert!(
            preview
                .symbols
                .class_members
                .iter()
                .any(|member| member.name.as_ref() == "extra")
        );
        assert_eq!(store.get("file:///dep.abap").unwrap().version, 1);
        assert!(Arc::ptr_eq(
            &committed_consumer,
            &store.get("file:///consumer.abap").expect("stored consumer"),
        ));
    }

    #[test]
    fn preview_rebuilds_static_analysis_summary() {
        let store = DocumentStore::default();
        let committed = "\
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_value TYPE i.
    lv_value = 1.
  ENDMETHOD.
ENDCLASS.";

        store.publish("file:///preview_static_analysis.abap", 1, committed);

        let preview = store.preview_publish_input(DocumentInput {
            uri: Arc::from("file:///preview_static_analysis.abap"),
            version: 2,
            text: Arc::from(
                "\
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_value TYPE i.
    RETURN.
    lv_value = 1.
  ENDMETHOD.
ENDCLASS.",
            ),
            is_dependency: false,
            object_name: None,
        });
        let preview_metrics = store
            .last_preview_metrics_snapshot()
            .expect("preview metrics");

        assert!(!preview_metrics.committed_context_only);
        assert!(preview.static_analysis().is_some());
    }

    #[test]
    fn exposes_structure_field_queries_on_snapshot() {
        let store = DocumentStore::default();
        let snapshot = store.publish(
            "file:///demo.abap",
            1,
            "\
TYPES: BEGIN OF ty_inner,
         a TYPE i,
       END OF ty_inner.
TYPES: BEGIN OF ty_outer,
         inner TYPE ty_inner,
       END OF ty_outer.
DATA ls_outer TYPE ty_outer.",
        );

        let ls_outer = snapshot
            .symbols
            .symbols
            .iter()
            .find(|symbol| symbol.name.as_ref() == "ls_outer")
            .expect("ls_outer symbol");
        let fields = snapshot
            .symbol_structure_field_infos(ls_outer.id)
            .expect("symbol field infos");
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].name.as_ref(), "inner");
        assert!(matches!(
            fields[0].shape,
            StructureFieldShape::Structured { .. }
        ));

        let nested = snapshot
            .resolve_symbol_field_path(ls_outer.id, &["inner", "a"])
            .expect("nested field info");
        assert_eq!(nested.name.as_ref(), "a");
        assert!(matches!(nested.shape, StructureFieldShape::Scalar));
    }

    #[test]
    fn resolves_fields_from_namespaced_ddic_structure_dependency() {
        let store = DocumentStore::default();
        let xml = r#"
<abapsource:elementInfo adtcore:name="/sttp/epc1"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="controller">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">prxctrltab</abapsource:entry>
      <abapsource:entry abapsource:key="ddicDataType">ttyp</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="content">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataType">string</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#;
        let dependency_text =
            ddic_xml_to_abap_source("/STTP/EPC1", "ddic-structure", xml).expect("dependency");
        let main_src = "\
DATA ls_epc TYPE /sttp/epc1.
ls_epc-controller = VALUE #( ).
ls_epc-content = 'x'.";

        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FEPC1.xml"),
                version: 1,
                text: Arc::from(dependency_text.clone()),
                is_dependency: true,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");
        let offset = main_src.find("content").expect("field use") + 1;

        let hovered = snapshot
            .hovered_component_at(offset)
            .expect("hovered dependency field");
        assert_eq!(hovered.field_name.as_ref(), "content");
        assert_eq!(hovered.declared_type.as_deref(), Some("TYPE string"));

        let definition = snapshot.definition_at(offset).expect("field definition");
        assert_eq!(definition.uri.as_ref(), "file:///deps/%2FSTTP%2FEPC1.xml");

        let dependency_snapshot = snapshots
            .get("file:///deps/%2FSTTP%2FEPC1.xml")
            .expect("dependency snapshot");
        let decl_offset = dependency_text.find("content").expect("field declaration") + 1;
        let references = store
            .references("file:///deps/%2FSTTP%2FEPC1.xml", decl_offset, true)
            .expect("field references");
        assert_reference_slices(
            &references,
            &[
                (
                    "file:///deps/%2FSTTP%2FEPC1.xml",
                    dependency_text.as_str(),
                    "content",
                ),
                ("file:///main.abap", main_src, "content"),
            ],
        );
        assert!(
            dependency_snapshot
                .reference_search_target_at(decl_offset)
                .is_some()
        );
    }

    #[test]
    fn resolves_message_class_reference_from_cached_message_class_dependency() {
        let xml = r#"
<mc:messageClass adtcore:name="/STTP/INT_MSG"
    xmlns:mc="http://www.sap.com/adt/MessageClass"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <mc:messages mc:msgno="043" mc:msgtext="Received &amp;1 documents for &amp;2 maintenance (&amp;3)"/>
</mc:messageClass>
"#;
        let dependency_text =
            ddic_xml_to_abap_source("/STTP/INT_MSG", "message-class", xml).expect("dependency");
        let main_src = "\
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING lv_lines TYPE i iv_logsys TYPE string iv_mode TYPE string.
ENDCLASS.
CLASS zcl_demo IMPLEMENTATION.
  METHOD run.
    MESSAGE i043(/sttp/int_msg) WITH lv_lines iv_logsys iv_mode INTO DATA(lv_message).
  ENDMETHOD.
ENDCLASS.";

        let store = DocumentStore::default();
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FINT_MSG.xml"),
                version: 1,
                text: Arc::from(dependency_text),
                is_dependency: true,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");
        let reference = snapshot
            .symbols
            .references
            .iter()
            .find(|reference| {
                reference.name.as_ref() == "/sttp/int_msg"
                    && reference.kind == ReferenceKind::MessageClass
            })
            .expect("message class reference");
        assert!(
            reference.resolution.is_some(),
            "{:?}",
            snapshot.symbols.references
        );
        assert!(
            snapshot
                .symbols
                .diagnostics
                .iter()
                .all(|diag| !diag.message.contains("/sttp/int_msg")),
            "{:?}",
            snapshot.symbols.diagnostics
        );
    }

    #[test]
    fn resolves_numeric_message_class_reference_from_cached_message_class_dependency() {
        let xml = r#"
<mc:messageClass adtcore:name="00"
    xmlns:mc="http://www.sap.com/adt/MessageClass"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <mc:messages mc:msgno="007" mc:msgtext="&amp;1 is empty"/>
</mc:messageClass>
"#;
        let dependency_text =
            ddic_xml_to_abap_source("00", "message-class", xml).expect("dependency");
        let main_src = "\
CLASS zcl_demo IMPLEMENTATION.
  METHOD run.
    MESSAGE s398(00) WITH TEXT-007 DISPLAY LIKE 'E'.
  ENDMETHOD.
ENDCLASS.";

        let store = DocumentStore::default();
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///deps/00.xml"),
                version: 1,
                text: Arc::from(dependency_text),
                is_dependency: true,
                object_name: Some(Arc::from("00")),
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");
        let reference = snapshot
            .symbols
            .references
            .iter()
            .find(|reference| {
                reference.name.as_ref() == "00" && reference.kind == ReferenceKind::MessageClass
            })
            .expect("message class reference");
        assert!(
            reference.resolution.is_some(),
            "{:?}",
            snapshot.symbols.references
        );
        assert!(
            snapshot
                .symbols
                .diagnostics
                .iter()
                .all(|diag| !diag.message.contains("unknown type '00'")),
            "{:?}",
            snapshot.symbols.diagnostics
        );
    }

    #[test]
    fn resolves_report_message_id_reference_from_cached_message_class_dependency() {
        let xml = r#"
<mc:messageClass adtcore:name="ZFIC"
    xmlns:mc="http://www.sap.com/adt/MessageClass"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <mc:messages mc:msgno="043" mc:msgtext="Demo message"/>
</mc:messageClass>
"#;
        let dependency_text =
            ddic_xml_to_abap_source("ZFIC", "message-class", xml).expect("dependency");
        let main_src = "\
REPORT zmain MESSAGE-ID zfic.
START-OF-SELECTION.
  MESSAGE i043.";

        let store = DocumentStore::default();
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///deps/zfic.xml"),
                version: 1,
                text: Arc::from(dependency_text),
                is_dependency: true,
                object_name: Some(Arc::from("zfic")),
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");
        let reference = snapshot
            .symbols
            .references
            .iter()
            .find(|reference| {
                reference.name.as_ref() == "zfic" && reference.kind == ReferenceKind::MessageClass
            })
            .expect("message class reference");
        assert!(
            reference.resolution.is_some(),
            "{:?}",
            snapshot.symbols.references
        );
        assert!(
            snapshot
                .definition_at(main_src.find("zfic").expect("message class use") + 1)
                .is_some(),
            "expected definition target for REPORT MESSAGE-ID reference"
        );
        assert!(
            snapshot.symbols.diagnostics.iter().all(|diag| {
                !diag.message.contains("unknown type 'zfic'")
                    && !diag.message.contains("unknown symbol 'i043'")
            }),
            "{:?}",
            snapshot.symbols.diagnostics
        );
    }

    #[test]
    fn validates_message_id_and_parameter_count_from_message_class_dependency() {
        let xml = r#"
<mc:messageClass adtcore:name="/STTP/INT_MSG"
    xmlns:mc="http://www.sap.com/adt/MessageClass"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <mc:messages mc:msgno="043" mc:msgtext="Received &amp;1 documents for &amp;2 maintenance (&amp;3)"/>
  <mc:messages mc:msgno="044" mc:msgtext="Done"/>
</mc:messageClass>
"#;
        let dependency_text =
            ddic_xml_to_abap_source("/STTP/INT_MSG", "message-class", xml).expect("dependency");
        let main_src = "\
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING lv_lines TYPE i iv_logsys TYPE string.
ENDCLASS.
CLASS zcl_demo IMPLEMENTATION.
  METHOD run.
    MESSAGE ID '/STTP/INT_MSG' TYPE 'I' NUMBER '043' WITH lv_lines iv_logsys sy-msgv3.
    MESSAGE i043(/sttp/int_msg) WITH lv_lines iv_logsys.
    MESSAGE i999(/sttp/int_msg).
    MESSAGE i044(/sttp/int_msg) WITH lv_lines.
  ENDMETHOD.
ENDCLASS.";

        let store = DocumentStore::default();
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FINT_MSG.xml"),
                version: 1,
                text: Arc::from(dependency_text),
                is_dependency: true,
                object_name: Some(Arc::from("/STTP/INT_MSG")),
            },
            DocumentInput {
                uri: Arc::from("file:///deps/duplicate-%2FSTTP%2FINT_MSG.xml"),
                version: 1,
                text: Arc::from(
                    ddic_xml_to_abap_source("/STTP/INT_MSG", "message-class", xml)
                        .expect("duplicate dependency"),
                ),
                is_dependency: true,
                object_name: Some(Arc::from("/STTP/INT_MSG")),
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");
        let invalid_messages = snapshot
            .symbols
            .diagnostics
            .iter()
            .filter(|diag| diag.kind == DiagnosticKind::InvalidMessage)
            .map(|diag| diag.message.as_str())
            .collect::<Vec<_>>();
        assert_eq!(invalid_messages.len(), 3, "{invalid_messages:?}");
        assert!(
            invalid_messages
                .iter()
                .any(|message| message.contains("expects 3 parameter(s)")),
            "{invalid_messages:?}"
        );
        assert!(
            invalid_messages
                .iter()
                .any(|message| message.contains("unknown message id '999'")),
            "{invalid_messages:?}"
        );
        assert!(
            invalid_messages
                .iter()
                .any(|message| message.contains("expects 0 parameter(s)")),
            "{invalid_messages:?}"
        );
    }

    #[test]
    fn hovers_message_class_and_id_from_message_class_dependency() {
        let xml = r#"
<mc:messageClass adtcore:name="/STTP/INT_MSG"
    xmlns:mc="http://www.sap.com/adt/MessageClass"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <mc:messages mc:msgno="043" mc:msgtext="Received &amp;1 documents for &amp;2 maintenance (&amp;3)"/>
</mc:messageClass>
"#;
        let dependency_text =
            ddic_xml_to_abap_source("/STTP/INT_MSG", "message-class", xml).expect("dependency");
        let main_src = "\
CLASS zcl_demo IMPLEMENTATION.
  METHOD run.
    MESSAGE i043(/sttp/int_msg) WITH sy-msgv1 sy-msgv2 sy-msgv3.
  ENDMETHOD.
ENDCLASS.";

        let store = DocumentStore::default();
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FINT_MSG.xml"),
                version: 1,
                text: Arc::from(dependency_text),
                is_dependency: true,
                object_name: Some(Arc::from("/STTP/INT_MSG")),
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");

        let class_offset = main_src.find("/sttp/int_msg").expect("message class") + 1;
        let class_hover = snapshot
            .hovered_message_at(class_offset)
            .expect("message class hover");
        assert!(
            class_hover
                .markdown_lines
                .iter()
                .any(|line| line.contains("Message class `/sttp/int_msg`")),
            "{:?}",
            class_hover.markdown_lines
        );
        assert!(
            class_hover
                .markdown_lines
                .iter()
                .any(|line| line.contains("043") && line.contains("Received")),
            "{:?}",
            class_hover.markdown_lines
        );
        assert_eq!(
            class_hover
                .markdown_lines
                .iter()
                .filter(|line| line.contains("043") && line.contains("Received"))
                .count(),
            1,
            "{:?}",
            class_hover.markdown_lines
        );
        assert!(
            snapshot.definition_at(class_offset).is_some(),
            "expected definition target for message class"
        );

        let id_offset = main_src.find("043(/sttp").expect("message id") + 1;
        let id_hover = snapshot
            .hovered_message_at(id_offset)
            .expect("message id hover");
        assert!(
            id_hover
                .markdown_lines
                .iter()
                .any(|line| line.contains("Received &1 documents")),
            "{:?}",
            id_hover.markdown_lines
        );
    }

    #[test]
    fn resolves_nested_fields_across_recursive_ddic_structure_dependencies() {
        let store = DocumentStore::default();
        let epcisdocument_xml = r#"
<abapsource:elementInfo adtcore:name="/sttp/epcisdocument"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="epcisdocument">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">/sttp/epcisdocument_type</abapsource:entry>
      <abapsource:entry abapsource:key="ddicDataType">stru</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#;
        let epcisdocument_type_xml = r#"
<abapsource:elementInfo adtcore:name="/sttp/epcisdocument_type"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="epcisbody">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">/sttp/epcisbody_type</abapsource:entry>
      <abapsource:entry abapsource:key="ddicDataType">stru</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#;
        let epcisbody_type_xml = r#"
<abapsource:elementInfo adtcore:name="/sttp/epcisbody_type"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="event_list">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">/sttp/event_list_type</abapsource:entry>
      <abapsource:entry abapsource:key="ddicDataType">stru</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#;
        let event_list_type_xml = r#"
<abapsource:elementInfo adtcore:name="/sttp/event_list_type"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="choice">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">/sttp/event_list_type_choice</abapsource:entry>
      <abapsource:entry abapsource:key="ddicDataType">ttyp</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#;
        let main_src = "\
DATA ls_doc TYPE /sttp/epcisdocument.
ls_doc-epcisdocument-epcisbody-event_list-choice = VALUE #( ).";

        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FEPCISDOCUMENT.xml"),
                version: 1,
                text: Arc::from(
                    ddic_xml_to_abap_source(
                        "/STTP/EPCISDOCUMENT",
                        "ddic-structure",
                        epcisdocument_xml,
                    )
                    .expect("epcisdocument"),
                ),
                is_dependency: true,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FEPCISDOCUMENT_TYPE.xml"),
                version: 1,
                text: Arc::from(
                    ddic_xml_to_abap_source(
                        "/STTP/EPCISDOCUMENT_TYPE",
                        "ddic-structure",
                        epcisdocument_type_xml,
                    )
                    .expect("epcisdocument_type"),
                ),
                is_dependency: true,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FEPCISBODY_TYPE.xml"),
                version: 1,
                text: Arc::from(
                    ddic_xml_to_abap_source(
                        "/STTP/EPCISBODY_TYPE",
                        "ddic-structure",
                        epcisbody_type_xml,
                    )
                    .expect("epcisbody_type"),
                ),
                is_dependency: true,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FEVENT_LIST_TYPE.xml"),
                version: 1,
                text: Arc::from(
                    ddic_xml_to_abap_source(
                        "/STTP/EVENT_LIST_TYPE",
                        "ddic-structure",
                        event_list_type_xml,
                    )
                    .expect("event_list_type"),
                ),
                is_dependency: true,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");
        let offset = main_src.find("choice").expect("choice use") + 1;

        let hovered = snapshot
            .hovered_component_at(offset)
            .expect("hovered deep dependency field");
        assert_eq!(hovered.field_name.as_ref(), "choice");
        assert_eq!(
            hovered.declared_type.as_deref(),
            Some("TYPE /sttp/event_list_type_choice")
        );

        let definition = snapshot
            .definition_at(offset)
            .expect("deep field definition");
        assert_eq!(
            definition.uri.as_ref(),
            "file:///deps/%2FSTTP%2FEVENT_LIST_TYPE.xml"
        );
    }

    #[test]
    fn resolves_fields_inside_ddic_proxy_include_structures() {
        let store = DocumentStore::default();
        let encode_decode_xml = r#"
<abapsource:elementInfo adtcore:name="/sttp/s_encode_decode"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DS" adtcore:name=".include">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicIncludeName">/sttp/s_obj_ids</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="enc_type">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">/sttp/e_enc_type</abapsource:entry>
      <abapsource:entry abapsource:key="ddicDataType">char</abapsource:entry>
      <abapsource:entry abapsource:key="ddicIsPartOfInclude">/sttp/s_obj_ids</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#;
        let obj_ids_xml = r#"
<abapsource:elementInfo adtcore:name="/sttp/s_obj_ids"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="owner">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">/sttp/e_gen_owner</abapsource:entry>
      <abapsource:entry abapsource:key="ddicDataType">char</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#;
        let main_src = "\
DATA ls_encode_decode TYPE /sttp/s_encode_decode.
ls_encode_decode-obj_ids-owner = 'x'.";

        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FS_ENCODE_DECODE.xml"),
                version: 1,
                text: Arc::from(
                    ddic_xml_to_abap_source(
                        "/STTP/S_ENCODE_DECODE",
                        "ddic-structure",
                        encode_decode_xml,
                    )
                    .expect("s_encode_decode"),
                ),
                is_dependency: true,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FS_OBJ_IDS.xml"),
                version: 1,
                text: Arc::from(
                    ddic_xml_to_abap_source("/STTP/S_OBJ_IDS", "ddic-structure", obj_ids_xml)
                        .expect("s_obj_ids"),
                ),
                is_dependency: true,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");
        let offset = main_src.find("owner").expect("owner use") + 1;

        let hovered = snapshot
            .hovered_component_at(offset)
            .expect("hovered included field");
        assert_eq!(hovered.field_name.as_ref(), "owner");

        let definition = snapshot
            .definition_at(offset)
            .expect("included field definition");
        assert_eq!(
            definition.uri.as_ref(),
            "file:///deps/%2FSTTP%2FS_OBJ_IDS.xml"
        );

        let direct_src = "\
DATA ls_encode_decode TYPE /sttp/s_encode_decode.
ls_encode_decode-enc_type = 'x'.";
        let direct_snapshot = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FS_ENCODE_DECODE.xml"),
                version: 1,
                text: Arc::from(
                    ddic_xml_to_abap_source(
                        "/STTP/S_ENCODE_DECODE",
                        "ddic-structure",
                        encode_decode_xml,
                    )
                    .expect("s_encode_decode"),
                ),
                is_dependency: true,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FS_OBJ_IDS.xml"),
                version: 1,
                text: Arc::from(
                    ddic_xml_to_abap_source("/STTP/S_OBJ_IDS", "ddic-structure", obj_ids_xml)
                        .expect("s_obj_ids"),
                ),
                is_dependency: true,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///direct.abap"),
                version: 1,
                text: Arc::from(direct_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let direct_snapshot = direct_snapshot
            .get("file:///direct.abap")
            .expect("direct snapshot");
        let direct_offset = direct_src.find("enc_type").expect("enc_type use") + 1;
        let direct_hover = direct_snapshot
            .hovered_component_at(direct_offset)
            .expect("hovered direct included field");
        assert_eq!(direct_hover.field_name.as_ref(), "enc_type");
    }

    #[test]
    fn definition_at_returns_ddic_field_declaration_for_value_for_where_bare_field() {
        let store = DocumentStore::default();
        let obj_ids_xml = r#"
<abapsource:elementInfo adtcore:name="/sttp/s_obj_ids"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="objid">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">/sttp/e_objid</abapsource:entry>
      <abapsource:entry abapsource:key="ddicDataType">char</abapsource:entry>
    </abapsource:properties>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#;
        let main_src = "\
DATA lv_parent TYPE string.
DATA mt_obj_ids_native TYPE STANDARD TABLE OF /sttp/s_obj_ids WITH EMPTY KEY.

DATA(lt_filtered) = VALUE #(
  FOR ls_obj IN mt_obj_ids_native
  WHERE ( objid <> lv_parent )
  ( ls_obj-objid ) ).
";

        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FS_OBJ_IDS.xml"),
                version: 1,
                text: Arc::from(
                    ddic_xml_to_abap_source("/STTP/S_OBJ_IDS", "ddic-structure", obj_ids_xml)
                        .expect("s_obj_ids"),
                ),
                is_dependency: true,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");
        let offset = main_src.find("objid <>").expect("bare objid use") + 1;

        let definition = snapshot.definition_at(offset).expect("definition target");
        assert_eq!(
            definition.uri.as_ref(),
            "file:///deps/%2FSTTP%2FS_OBJ_IDS.xml"
        );

        let dep_src = snapshots
            .get("file:///deps/%2FSTTP%2FS_OBJ_IDS.xml")
            .expect("dependency snapshot")
            .text
            .as_ref();
        assert_target_slice(
            &definition,
            "file:///deps/%2FSTTP%2FS_OBJ_IDS.xml",
            dep_src,
            "objid",
        );
    }

    #[test]
    fn hover_and_definition_work_for_bare_where_field_inside_ddic_proxy_include_structure() {
        let store = DocumentStore::default();
        let include_src = "\
TYPES: BEGIN OF /sttp/s_dm_obj_itm,\n\
         uom TYPE string,\n\
       END OF /sttp/s_dm_obj_itm.\n";
        let row_src = "\
TYPES: BEGIN OF /sttp/dm_obj_itm,\n\
         dm_obj_itm TYPE /sttp/s_dm_obj_itm,\n\
       END OF /sttp/dm_obj_itm.\n";
        let table_src =
            "TYPES /sttp/t_dm_obj_itm TYPE STANDARD TABLE OF /sttp/dm_obj_itm WITH EMPTY KEY.\n";
        let main_src = "\
DATA mt_obj_itm TYPE /sttp/t_dm_obj_itm.\n\
DATA(lt_obj_itm) = mt_obj_itm.\n\
DELETE lt_obj_itm WHERE uom NE 'PK'.\n";

        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FS_DM_OBJ_ITM.abap"),
                version: 1,
                text: Arc::from(include_src),
                is_dependency: true,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FDM_OBJ_ITM.abap"),
                version: 1,
                text: Arc::from(row_src),
                is_dependency: true,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FT_DM_OBJ_ITM.abap"),
                version: 1,
                text: Arc::from(table_src),
                is_dependency: true,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");
        let offset = main_src.find("uom NE").expect("uom use") + 1;

        let hovered = snapshot
            .hovered_component_at(offset)
            .expect("hovered bare where field");
        assert_eq!(hovered.field_name.as_ref(), "uom");
        assert_eq!(hovered.declared_type.as_deref(), Some("TYPE string"));

        let definition = snapshot.definition_at(offset).expect("definition target");
        assert_eq!(
            definition.uri.as_ref(),
            "file:///deps/%2FSTTP%2FS_DM_OBJ_ITM.abap"
        );

        let dep_src = snapshots
            .get("file:///deps/%2FSTTP%2FS_DM_OBJ_ITM.abap")
            .expect("dependency snapshot")
            .text
            .as_ref();
        assert_target_slice(
            &definition,
            "file:///deps/%2FSTTP%2FS_DM_OBJ_ITM.abap",
            dep_src,
            "uom",
        );
    }

    #[test]
    fn hover_and_definition_work_for_bare_where_field_inside_method_inline_copy_from_attribute() {
        let store = DocumentStore::default();
        let include_src = "\
TYPES: BEGIN OF /sttp/s_dm_obj_itm,\n\
         uom TYPE string,\n\
       END OF /sttp/s_dm_obj_itm.\n";
        let row_src = "\
TYPES: BEGIN OF /sttp/dm_obj_itm,\n\
         dm_obj_itm TYPE /sttp/s_dm_obj_itm,\n\
       END OF /sttp/dm_obj_itm.\n";
        let table_src =
            "TYPES /sttp/t_dm_obj_itm TYPE STANDARD TABLE OF /sttp/dm_obj_itm WITH EMPTY KEY.\n";
        let main_src = "\
CLASS zcl_main DEFINITION.\n\
  PRIVATE SECTION.\n\
    DATA mt_obj_itm TYPE /sttp/t_dm_obj_itm.\n\
    METHODS run.\n\
ENDCLASS.\n\
\n\
CLASS zcl_main IMPLEMENTATION.\n\
  METHOD run.\n\
    DATA(lt_obj_itm) = mt_obj_itm.\n\
    DELETE lt_obj_itm WHERE uom NE 'PK'.\n\
  ENDMETHOD.\n\
ENDCLASS.\n";

        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FS_DM_OBJ_ITM.abap"),
                version: 1,
                text: Arc::from(include_src),
                is_dependency: true,
                object_name: Some(Arc::from("/sttp/s_dm_obj_itm")),
            },
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FDM_OBJ_ITM.abap"),
                version: 1,
                text: Arc::from(row_src),
                is_dependency: true,
                object_name: Some(Arc::from("/sttp/dm_obj_itm")),
            },
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FT_DM_OBJ_ITM.abap"),
                version: 1,
                text: Arc::from(table_src),
                is_dependency: true,
                object_name: Some(Arc::from("/sttp/t_dm_obj_itm")),
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");
        let offset = main_src.find("uom NE").expect("uom use") + 1;

        let hovered = snapshot
            .hovered_component_at(offset)
            .expect("hovered bare where field");
        assert_eq!(hovered.field_name.as_ref(), "uom");

        let definition = snapshot.definition_at(offset).expect("definition target");
        assert_eq!(
            definition.uri.as_ref(),
            "file:///deps/%2FSTTP%2FS_DM_OBJ_ITM.abap"
        );
    }

    #[test]
    fn finds_hovered_component_at_offset() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF ty_inner,
         a TYPE i,
       END OF ty_inner.
TYPES: BEGIN OF ty_outer,
         inner TYPE ty_inner,
       END OF ty_outer.
DATA ls_outer TYPE ty_outer.
ls_outer-inner-a = 1.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.find("inner-a").expect("inner-a segment") + "inner-".len();

        let hovered = snapshot
            .hovered_component_at(offset)
            .expect("hovered component info");
        assert_eq!(hovered.base_name.as_ref(), "ls_outer");
        assert_eq!(
            hovered
                .component_path
                .iter()
                .map(|part| part.as_ref())
                .collect::<Vec<_>>(),
            vec!["inner", "a"]
        );
        assert_eq!(hovered.field_name.as_ref(), "a");
        assert_eq!(hovered.declared_type.as_deref(), Some("TYPE i"));
        assert!(matches!(hovered.kind, HoveredComponentKind::Scalar));
    }

    #[test]
    fn finds_hovered_static_method_at_offset() {
        let store = DocumentStore::default();
        let src = "\
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS exec
      IMPORTING
        iv_value TYPE i.
ENDCLASS.

some_class=>exec( iv_value = 1 ).";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.rfind("exec").expect("method use") + 1;

        let hovered = snapshot
            .hovered_component_at(offset)
            .expect("hovered method info");
        assert_eq!(hovered.base_name.as_ref(), "some_class");
        assert_eq!(hovered.field_name.as_ref(), "exec");
        assert!(matches!(hovered.kind, HoveredComponentKind::Method));
        assert!(
            hovered
                .declaration
                .as_deref()
                .is_some_and(|declaration| declaration.contains("CLASS-METHODS exec"))
        );
        assert!(
            hovered
                .declaration
                .as_deref()
                .is_some_and(|declaration| declaration.contains("iv_value TYPE i"))
        );
    }

    #[test]
    fn hovered_resolved_symbol_at_finds_resolved_reference() {
        let store = DocumentStore::default();
        let src = "DATA lv TYPE i.\nlv = 1.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.rfind("lv").expect("use of lv") + 1;

        let hovered = snapshot
            .hovered_resolved_symbol_at(offset)
            .expect("resolved symbol hover");
        assert_eq!(hovered.display_name.as_ref(), "lv");
        assert!(
            hovered.markdown_lines.iter().any(|line| line == "Variable"),
            "{:?}",
            hovered.markdown_lines
        );
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line == "```abap\nTYPE i\n```"),
            "{:?}",
            hovered.markdown_lines
        );
    }

    #[test]
    fn hovered_resolved_symbol_at_falls_back_to_declaration() {
        let store = DocumentStore::default();
        let src = "DATA lv TYPE i.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.find("lv").expect("lv name") + 1;

        let hovered = snapshot
            .hovered_resolved_symbol_at(offset)
            .expect("declaration hover");
        assert_eq!(hovered.display_name.as_ref(), "lv");
        assert!(hovered.markdown_lines.iter().any(|line| line == "Variable"));
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line == "```abap\nTYPE i\n```"),
            "{:?}",
            hovered.markdown_lines
        );
    }

    #[test]
    fn hovered_resolved_symbol_at_formats_cross_include_method_implementation_signature() {
        let store = DocumentStore::default();
        let main_src = "INCLUDE top.\nINCLUDE f01.";
        let top_src = "\
CLASS lcl_demo DEFINITION.
  PROTECTED SECTION.
    METHODS status_from_rep_evt_status
      IMPORTING
        iv_status_rep_evt TYPE i
      RETURNING
        VALUE(rv_status) TYPE string.
ENDCLASS.";
        let f01_src = "\
CLASS lcl_demo IMPLEMENTATION.
  METHOD status_from_rep_evt_status.
  ENDMETHOD.
ENDCLASS.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///top.abap"),
                version: 1,
                text: Arc::from(top_src),
                is_dependency: false,
                object_name: Some(Arc::from("top")),
            },
            DocumentInput {
                uri: Arc::from("file:///f01.abap"),
                version: 1,
                text: Arc::from(f01_src),
                is_dependency: false,
                object_name: Some(Arc::from("f01")),
            },
        ]);
        let f01 = snapshots.get("file:///f01.abap").expect("f01 snapshot");
        let offset = f01_src.find("status_from_rep_evt_status").expect("method") + 1;

        let hovered = f01
            .hovered_resolved_symbol_at(offset)
            .expect("method implementation hover");
        assert_eq!(hovered.display_name.as_ref(), "status_from_rep_evt_status");
        assert!(f01_src[hovered.range.clone()].eq_ignore_ascii_case("status_from_rep_evt_status"));
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line.contains("METHODS status_from_rep_evt_status")),
            "{:?}",
            hovered.markdown_lines
        );
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line.contains("iv_status_rep_evt TYPE i")),
            "{:?}",
            hovered.markdown_lines
        );
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line.contains("VALUE(rv_status) TYPE string")),
            "{:?}",
            hovered.markdown_lines
        );
    }

    #[test]
    fn hovered_resolved_symbol_at_preserves_ref_to_type_clause() {
        let store = DocumentStore::default();
        let src = "\
CLASS some_class DEFINITION.
ENDCLASS.

CLASS some_class IMPLEMENTATION.
ENDCLASS.

DATA lo_instance TYPE REF TO some_class.
CREATE OBJECT lo_instance.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.rfind("lo_instance").expect("lo_instance use") + 1;

        let hovered = snapshot
            .hovered_resolved_symbol_at(offset)
            .expect("resolved symbol hover");
        assert_eq!(hovered.display_name.as_ref(), "lo_instance");
        assert!(hovered.markdown_lines.iter().any(|line| line == "Variable"));
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line == "```abap\nTYPE REF TO some_class\n```"),
            "{:?}",
            hovered.markdown_lines
        );
    }

    #[test]
    fn hovered_resolved_symbol_at_shows_internal_table_wrapper_type() {
        let store = DocumentStore::default();
        let src = "\
DATA lt_gs1_gcp TYPE STANDARD TABLE OF /sttp/gs1_gcp.
LOOP AT lt_gs1_gcp INTO DATA(ls).
ENDLOOP.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.rfind("lt_gs1_gcp").expect("loop table") + 2;

        let hovered = snapshot
            .hovered_resolved_symbol_at(offset)
            .expect("resolved symbol hover");
        assert_eq!(hovered.display_name.as_ref(), "lt_gs1_gcp");
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| { line == "```abap\nTYPE STANDARD TABLE OF /sttp/gs1_gcp\n```" }),
            "{:?}",
            hovered.markdown_lines
        );
    }

    #[test]
    fn hover_infers_loop_inline_target_from_interface_method_table_return() {
        let store = DocumentStore::default();
        let src = "\
INTERFACE /iwbep/if_mgw_req_filter.
  METHODS get_filter_select_options
    RETURNING VALUE(rt_filter_select_options) TYPE /iwbep/t_mgw_select_option.
ENDINTERFACE.

INTERFACE /iwbep/if_mgw_req_entityset.
  METHODS get_filter
    RETURNING VALUE(ro_filter) TYPE REF TO /iwbep/if_mgw_req_filter.
ENDINTERFACE.

TYPES: BEGIN OF /iwbep/s_mgw_select_option,
         property TYPE string,
       END OF /iwbep/s_mgw_select_option.
TYPES /iwbep/t_mgw_select_option TYPE STANDARD TABLE OF /iwbep/s_mgw_select_option WITH EMPTY KEY.

CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING io_tech_request_context TYPE REF TO /iwbep/if_mgw_req_entityset.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD run.
    DATA(lo_filter) = io_tech_request_context->get_filter( ).
    DATA(lt_filter_sel_opts) = lo_filter->get_filter_select_options( ).
    LOOP AT lt_filter_sel_opts INTO DATA(ls_filter_sel_opt).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.";
        let snapshot = store.publish("file:///odata.abap", 1, src);
        let offset = src.find("ls_filter_sel_opt").expect("loop target") + 1;

        let hovered = snapshot
            .hovered_resolved_symbol_at(offset)
            .expect("loop target hover");
        assert_eq!(hovered.display_name.as_ref(), "ls_filter_sel_opt");
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line == "```abap\nTYPE /iwbep/s_mgw_select_option\n```"),
            "{:?}",
            hovered.markdown_lines
        );
    }

    #[test]
    fn hover_and_definition_work_for_split_into_table_inline_target_and_source_field() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF ty_trn,
         trncode TYPE string,
       END OF ty_trn.
DATA ls_trn TYPE ty_trn.

SPLIT ls_trn-trncode AT ':' INTO TABLE DATA(lt_split).
CLEAR lt_split.";
        let snapshot = store.publish("file:///demo.abap", 1, src);

        let lt_split_offset = src.rfind("lt_split").expect("lt_split use") + 1;
        let lt_split_hover = snapshot
            .hovered_resolved_symbol_at(lt_split_offset)
            .expect("lt_split hover");
        assert_eq!(lt_split_hover.display_name.as_ref(), "lt_split");
        assert!(
            lt_split_hover
                .markdown_lines
                .iter()
                .any(|line| line == "```abap\nTYPE STANDARD TABLE OF string\n```"),
            "{:?}",
            lt_split_hover.markdown_lines
        );

        let ls_trn_offset = src.find("ls_trn-trncode").expect("ls_trn use") + 1;
        let ls_trn_hover = snapshot
            .hovered_resolved_symbol_at(ls_trn_offset)
            .expect("ls_trn hover");
        assert_eq!(ls_trn_hover.display_name.as_ref(), "ls_trn");
        assert!(
            ls_trn_hover
                .markdown_lines
                .iter()
                .any(|line| line == "```abap\nTYPE ty_trn\n```"),
            "{:?}",
            ls_trn_hover.markdown_lines
        );

        let trncode_use = src.rfind("trncode").expect("trncode use");
        let trncode_hover = snapshot
            .hovered_component_at(trncode_use + 1)
            .expect("trncode hover");
        assert_eq!(trncode_hover.base_name.as_ref(), "ls_trn");
        assert_eq!(trncode_hover.field_name.as_ref(), "trncode");
        assert_eq!(trncode_hover.declared_type.as_deref(), Some("TYPE string"));

        let target = snapshot
            .definition_at(trncode_use + 1)
            .expect("trncode definition");
        assert_target_slice(&target, "file:///demo.abap", src, "trncode");
        assert_eq!(
            target.range.start,
            src.find("trncode TYPE string")
                .expect("trncode declaration")
        );
    }

    #[test]
    fn hover_works_for_split_after_read_table_inline_into_source() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF /sttp/dm_trn,
         bizttype TYPE i,
         trncode TYPE string,
       END OF /sttp/dm_trn.
TYPES /sttp/t_dm_trn TYPE STANDARD TABLE OF /sttp/dm_trn WITH EMPTY KEY.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    DATA mt_trn TYPE /sttp/t_dm_trn.
    METHODS run.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    READ TABLE mt_trn INTO DATA(ls_trn) WITH KEY bizttype = 60.
    SPLIT ls_trn-trncode AT ':' INTO TABLE DATA(lt_split).
    CLEAR lt_split.
  ENDMETHOD.
ENDCLASS.";
        let snapshot = store.publish("file:///demo.abap", 1, src);

        let ls_trn_offset = src.find("ls_trn-trncode").expect("ls_trn use") + 1;
        let ls_trn_hover = snapshot
            .hovered_resolved_symbol_at(ls_trn_offset)
            .expect("ls_trn hover");
        assert_eq!(ls_trn_hover.display_name.as_ref(), "ls_trn");
        assert!(
            ls_trn_hover
                .markdown_lines
                .iter()
                .any(|line| line == "```abap\nTYPE /sttp/dm_trn\n```"),
            "{:?}",
            ls_trn_hover.markdown_lines
        );

        let trncode_use = src.rfind("trncode").expect("trncode use");
        let trncode_hover = snapshot
            .hovered_component_at(trncode_use + 1)
            .expect("trncode hover");
        assert_eq!(trncode_hover.base_name.as_ref(), "ls_trn");
        assert_eq!(trncode_hover.field_name.as_ref(), "trncode");
        assert_eq!(trncode_hover.declared_type.as_deref(), Some("TYPE string"));
    }

    #[test]
    fn definition_at_returns_read_table_with_key_field_declaration() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF ty_vbfa,
         vbeln TYPE string,
         posnn TYPE string,
       END OF ty_vbfa.
TYPES ty_vbfa_tab TYPE STANDARD TABLE OF ty_vbfa WITH EMPTY KEY.
DATA t_vbfa TYPE ty_vbfa_tab.
DATA ls_vbfa TYPE ty_vbfa.
DATA us_ltap TYPE ty_vbfa.

READ TABLE t_vbfa INTO ls_vbfa WITH KEY vbeln = us_ltap-vbeln
                                      posnn = us_ltap-posnn.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.rfind("posnn = us_ltap-posnn").expect("key field use") + 1;

        let hovered = snapshot
            .hovered_component_at(offset)
            .expect("hovered read table key field");
        assert_eq!(hovered.field_name.as_ref(), "posnn");

        let definition = snapshot.definition_at(offset).expect("field definition");
        assert_eq!(definition.uri.as_ref(), "file:///demo.abap");
        assert_eq!(&src[definition.range], "posnn");
    }

    #[test]
    fn hovered_sql_name_ref_at_shows_open_sql_source() {
        let store = DocumentStore::default();
        let src = "SELECT * FROM /sttp/gs1_gcp INTO TABLE DATA(lt).\n";
        let snapshot = store.publish("file:///sql.abap", 1, src);
        let offset = src.find("/sttp/gs1_gcp").expect("table") + 4;

        let hovered = snapshot
            .hovered_sql_name_ref_at(offset)
            .expect("sql name hover");
        assert_eq!(hovered.display_name.as_ref(), "/sttp/gs1_gcp");
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line.contains("Open SQL data source")),
            "{:?}",
            hovered.markdown_lines
        );
    }

    #[test]
    fn sql_hover_and_definition_use_ddic_short_texts_for_source_and_fields() {
        let store = DocumentStore::default();
        let lagp_xml = r#"
<abapsource:elementInfo adtcore:name="lagp"
    xmlns:abapsource="http://www.sap.com/adt/abapsource"
    xmlns:adtcore="http://www.sap.com/adt/core">
  <abapsource:properties/>
  <abapsource:documentation abapsource:rel="shorttext" abapsource:type="text/plain">
    Storage bins
  </abapsource:documentation>
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="lgnum">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">lgnum</abapsource:entry>
    </abapsource:properties>
    <abapsource:documentation abapsource:rel="shorttext" abapsource:type="text/plain">
      Warehouse number
    </abapsource:documentation>
  </abapsource:elementInfo>
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="lgpla">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">lgpla</abapsource:entry>
    </abapsource:properties>
    <abapsource:documentation abapsource:rel="shorttext" abapsource:type="text/plain">
      Storage bin
    </abapsource:documentation>
  </abapsource:elementInfo>
  <abapsource:elementInfo adtcore:type="TABL/DTF" adtcore:name="skzsi">
    <abapsource:properties>
      <abapsource:entry abapsource:key="ddicDataElement">lagp_skzsi</abapsource:entry>
    </abapsource:properties>
    <abapsource:documentation abapsource:rel="shorttext" abapsource:type="text/plain">
      Blocking indicator: current inventory (system)
    </abapsource:documentation>
  </abapsource:elementInfo>
</abapsource:elementInfo>
"#;
        let dep_src = ddic_xml_to_abap_source("LAGP", "ddic-table", lagp_xml).expect("dependency");
        let main_src = "\
DATA lv_lgnum TYPE lgnum.\n\
DATA lv_lgpla TYPE lgpla.\n\
SELECT SINGLE lgpla\n\
  FROM lagp\n\
  INTO @DATA(lv_storage_bin)\n\
  WHERE lgnum = @lv_lgnum\n\
    AND lgpla = @lv_lgpla.\n";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///deps/LAGP.xml"),
                version: 1,
                text: Arc::from(dep_src),
                is_dependency: true,
                object_name: Some(Arc::from("lagp")),
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");

        let source_offset = main_src.find("lagp").expect("lagp source") + 1;
        let source_hover = snapshot
            .hovered_sql_name_ref_at(source_offset)
            .expect("sql source hover");
        assert!(
            source_hover
                .markdown_lines
                .iter()
                .any(|line| line == "Storage bins"),
            "{:?}",
            source_hover.markdown_lines
        );
        assert!(
            source_hover
                .markdown_lines
                .iter()
                .all(|line| !line.contains("not connected in this build")),
            "{:?}",
            source_hover.markdown_lines
        );

        let select_field_offset = main_src.find("lgpla\n").expect("select field") + 1;
        let select_field_hover = snapshot
            .hovered_sql_name_ref_at(select_field_offset)
            .expect("select field hover");
        assert!(
            select_field_hover
                .markdown_lines
                .iter()
                .any(|line| line == "Storage bin"),
            "{:?}",
            select_field_hover.markdown_lines
        );
        assert!(
            select_field_hover
                .markdown_lines
                .iter()
                .any(|line| line == "```abap\nTYPE lgpla\n```"),
            "{:?}",
            select_field_hover.markdown_lines
        );

        let select_definition = snapshot
            .definition_at(select_field_offset)
            .expect("select field definition");
        assert_eq!(select_definition.uri.as_ref(), "file:///deps/LAGP.xml");
        let dep_text = snapshots
            .get("file:///deps/LAGP.xml")
            .expect("dependency snapshot")
            .text
            .as_ref();
        assert_target_slice(
            &select_definition,
            "file:///deps/LAGP.xml",
            dep_text,
            "lgpla",
        );

        let where_field_offset = main_src.find("lgnum =").expect("where field") + 1;
        let where_hover = snapshot
            .hovered_sql_name_ref_at(where_field_offset)
            .expect("where field hover");
        assert!(
            where_hover
                .markdown_lines
                .iter()
                .any(|line| line == "Warehouse number"),
            "{:?}",
            where_hover.markdown_lines
        );
        assert!(
            where_hover
                .markdown_lines
                .iter()
                .any(|line| line == "```abap\nTYPE lgnum\n```"),
            "{:?}",
            where_hover.markdown_lines
        );

        let where_definition = snapshot
            .definition_at(where_field_offset)
            .expect("where field definition");
        assert_target_slice(
            &where_definition,
            "file:///deps/LAGP.xml",
            dep_text,
            "lgnum",
        );
    }

    #[test]
    fn find_references_includes_type_clause_and_from_for_dd_like_name() {
        let store = DocumentStore::default();
        let src = "\
DATA lt TYPE STANDARD TABLE OF /sttp/gs1_gcp.
SELECT * FROM /sttp/gs1_gcp INTO TABLE lt.
";
        store.publish("file:///sql.abap", 1, src);
        let from_offset = src.rfind("/sttp/gs1_gcp").expect("from table") + 2;
        let refs = store
            .references("file:///sql.abap", from_offset, false)
            .expect("refs");
        assert!(
            refs.len() >= 2,
            "expected at least type and from refs, got {:?}",
            refs
        );

        let type_offset = src.find("/sttp/gs1_gcp").expect("type table") + 2;
        let refs_from_type = store
            .references("file:///sql.abap", type_offset, false)
            .expect("refs from type");
        assert!(
            refs_from_type.len() >= 2,
            "expected refs from type position too, got {:?}",
            refs_from_type
        );
    }

    #[test]
    fn definition_from_select_from_matches_resolving_type_reference() {
        let store = DocumentStore::default();
        let src = "\
TYPES ty_demo TYPE i.
DATA lt TYPE STANDARD TABLE OF ty_demo.
SELECT * FROM ty_demo INTO TABLE lt.
";
        let snapshot = store.publish("file:///sql.abap", 1, src);
        let offset = src.rfind("ty_demo").expect("from ty_demo");
        let def = snapshot.definition_at(offset).expect("definition target");
        assert_eq!(def.uri.as_ref(), "file:///sql.abap");
        assert_eq!(&src[def.range], "ty_demo");
    }

    #[test]
    fn definition_from_select_from_resolves_dependency_ddic_object_without_local_type_ref() {
        let store = DocumentStore::default();
        let dep_src = "\
TYPES: BEGIN OF ekpo,
         ebeln TYPE string,
       END OF ekpo.
";
        let main_src = "SELECT ebeln FROM ekpo INTO TABLE @DATA(lt_ekpo).\n";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///deps/EKPO.abap"),
                version: 1,
                text: Arc::from(dep_src),
                is_dependency: true,
                object_name: Some(Arc::from("ekpo")),
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");
        let offset = main_src.find("FROM ekpo").expect("sql source") + "FROM ".len() + 1;

        let def = snapshot.definition_at(offset).expect("definition target");

        assert_eq!(def.uri.as_ref(), "file:///deps/EKPO.abap");
        assert_target_slice(&def, "file:///deps/EKPO.abap", dep_src, "ekpo");
    }

    #[test]
    fn hovered_resolved_symbol_at_returns_none_on_whitespace() {
        let store = DocumentStore::default();
        let src = "DATA lv TYPE i.\n";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.len() - 1;

        assert!(snapshot.hovered_resolved_symbol_at(offset).is_none());
    }

    #[test]
    fn definition_at_returns_variable_declaration() {
        let store = DocumentStore::default();
        let src = "DATA lv TYPE i.\nlv = 1.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.rfind("lv").expect("variable use") + 1;

        let target = snapshot.definition_at(offset).expect("definition target");
        assert_target_slice(&target, "file:///demo.abap", src, "lv");
        assert_eq!(
            target.range.start,
            src.find("lv").expect("variable declaration")
        );
    }

    #[test]
    fn definition_at_returns_definition_site_when_cursor_is_on_declaration() {
        let store = DocumentStore::default();
        let src = "DATA lv TYPE i.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let decl_start = src.find("lv").expect("variable declaration");

        let target = snapshot
            .definition_at(decl_start + 1)
            .expect("definition target");
        assert_target_slice(&target, "file:///demo.abap", src, "lv");
        assert_eq!(target.range, decl_start..decl_start + 2);
    }

    #[test]
    fn definition_at_returns_type_declaration() {
        let store = DocumentStore::default();
        let src = "\
CLASS some_class DEFINITION.
ENDCLASS.

CLASS some_class IMPLEMENTATION.
ENDCLASS.

DATA lo_instance TYPE REF TO some_class.
CREATE OBJECT lo_instance.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let type_use = src.rfind("some_class").expect("type reference use");

        let target = snapshot
            .definition_at(type_use + 1)
            .expect("definition target");
        assert_target_slice(&target, "file:///demo.abap", src, "some_class");
        assert_eq!(
            target.range.start,
            src.find("some_class").expect("class declaration")
        );
    }

    #[test]
    fn definition_at_returns_selector_method_declaration() {
        let store = DocumentStore::default();
        let src = "\
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS exec
      IMPORTING
        iv_value TYPE i.
ENDCLASS.

some_class=>exec( iv_value = 1 ).";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let method_use = src.rfind("exec").expect("method use");

        let target = snapshot
            .definition_at(method_use + 1)
            .expect("definition target");
        assert_target_slice(&target, "file:///demo.abap", src, "exec");
        assert_eq!(
            target.range.start,
            src.find("exec").expect("method declaration")
        );
    }

    #[test]
    fn definition_at_returns_selector_method_implementation() {
        let store = DocumentStore::default();
        let src = "\
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS exec.
ENDCLASS.

CLASS some_class IMPLEMENTATION.
  METHOD exec.
  ENDMETHOD.
ENDCLASS.

some_class=>exec( ).";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let method_use = src.rfind("exec").expect("method use");
        let implementation =
            src.find("METHOD exec").expect("method implementation") + "METHOD ".len();

        let target = snapshot
            .definition_at(method_use + 1)
            .expect("definition target");
        assert_target_slice(&target, "file:///demo.abap", src, "exec");
        assert_eq!(target.range.start, implementation);
    }

    #[test]
    fn definition_at_returns_interface_qualifier_declaration_for_selector() {
        let store = DocumentStore::default();
        let src = "\
INTERFACE i1.
  METHODS meth.
ENDINTERFACE.

CLASS c1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES i1.
ENDCLASS.

CLASS c1 IMPLEMENTATION.
  METHOD i1~meth.
  ENDMETHOD.
ENDCLASS.

DATA lo_obj TYPE REF TO c1.
lo_obj->i1~meth( ).";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let qualifier_use = src.rfind("i1~meth").expect("interface-qualified call");

        let target = snapshot
            .definition_at(qualifier_use + 1)
            .expect("interface definition target");
        assert_target_slice(&target, "file:///demo.abap", src, "i1");
        assert_eq!(
            target.range.start,
            src.find("i1").expect("interface declaration")
        );
    }

    #[test]
    fn definition_at_returns_interface_method_implementation_for_selector() {
        let store = DocumentStore::default();
        let src = "\
INTERFACE i1.
  METHODS meth.
ENDINTERFACE.

CLASS c1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES i1.
ENDCLASS.

CLASS c1 IMPLEMENTATION.
  METHOD i1~meth.
  ENDMETHOD.
ENDCLASS.

DATA lo_obj TYPE REF TO c1.
lo_obj->i1~meth( ).";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let method_use = src.rfind("meth").expect("method use");

        let target = snapshot
            .definition_at(method_use + 1)
            .expect("interface method implementation target");
        assert_target_slice(&target, "file:///demo.abap", src, "meth");
        assert_eq!(
            target.range.start,
            src.find("METHOD i1~meth")
                .expect("interface method implementation")
                + "METHOD i1~".len()
        );
    }

    #[test]
    fn definition_and_hover_fallback_to_qualified_method_symbol_when_interface_is_unresolved() {
        let store = DocumentStore::default();
        let src = "\
CLASS c1 DEFINITION.
  PUBLIC SECTION.
    METHODS i1~meth.
ENDCLASS.

CLASS c1 IMPLEMENTATION.
  METHOD i1~meth.
  ENDMETHOD.
ENDCLASS.

DATA lo_obj TYPE REF TO c1.
lo_obj->i1~meth( ).";
        let snapshot = store.publish("file:///demo.abap", 1, src);

        let qualifier_use = src.rfind("i1~meth").expect("qualified call");
        let qualifier_hover = snapshot
            .hovered_component_at(qualifier_use + 1)
            .expect("qualifier hover");
        assert!(matches!(
            qualifier_hover.kind,
            HoveredComponentKind::Interface
        ));
        assert_eq!(qualifier_hover.declaration.as_deref(), Some("INTERFACE i1"));

        let qualifier_target = snapshot
            .definition_at(qualifier_use + 1)
            .expect("qualifier definition");
        assert_target_slice(&qualifier_target, "file:///demo.abap", src, "i1");
        assert_eq!(
            qualifier_target.range.start,
            src.find("METHOD i1~meth").expect("implementation header") + "METHOD ".len()
        );

        let member_use = qualifier_use + "i1~".len();
        let member_hover = snapshot
            .hovered_component_at(member_use + 1)
            .expect("member hover");
        assert!(matches!(member_hover.kind, HoveredComponentKind::Method));
        assert_eq!(member_hover.declaration.as_deref(), Some("METHOD i1~meth"));

        let member_target = snapshot
            .definition_at(member_use + 1)
            .expect("member definition");
        assert_target_slice(&member_target, "file:///demo.abap", src, "meth");
        assert_eq!(
            member_target.range.start,
            src.find("METHOD i1~meth").expect("implementation header") + "METHOD i1~".len()
        );
    }

    #[test]
    fn definition_at_returns_interface_method_declaration_for_interface_typed_value_selector() {
        let store = DocumentStore::default();
        let interface_src = "\
INTERFACE i1.
  METHODS meth.
ENDINTERFACE.";
        let main_src = "\
CLASS demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS demo IMPLEMENTATION.
  METHOD run.
    DATA lo_obj TYPE REF TO i1.
    lo_obj->meth( ).
  ENDMETHOD.
ENDCLASS.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///i1.abap"),
                version: 1,
                text: Arc::from(interface_src),
                is_dependency: true,
                object_name: Some(Arc::from("i1")),
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots
            .get("file:///main.abap")
            .expect("main snapshot should exist");
        let method_use = main_src.rfind("meth").expect("method use");

        let target = snapshot
            .definition_at(method_use + 1)
            .expect("interface method definition target");
        assert_target_slice(&target, "file:///i1.abap", interface_src, "meth");
        assert_eq!(
            target.range.start,
            interface_src
                .find("meth")
                .expect("interface method declaration")
        );
    }

    #[test]
    fn definition_at_returns_interface_targets_for_qualified_method_implementation_header() {
        let store = DocumentStore::default();
        let src = "\
INTERFACE i1.
  METHODS meth.
ENDINTERFACE.

CLASS c1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES i1.
ENDCLASS.

CLASS c1 IMPLEMENTATION.
  METHOD i1~meth.
  ENDMETHOD.
ENDCLASS.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let implementation_offset = src.rfind("i1~meth").expect("implementation header");

        let interface_target = snapshot
            .definition_at(implementation_offset + 1)
            .expect("interface definition target");
        assert_target_slice(&interface_target, "file:///demo.abap", src, "i1");
        assert_eq!(
            interface_target.range.start,
            src.find("i1").expect("interface declaration")
        );

        let method_offset = implementation_offset + "i1~".len();
        let method_target = snapshot
            .definition_at(method_offset + 1)
            .expect("interface method definition target");
        assert_target_slice(&method_target, "file:///demo.abap", src, "meth");
        assert_eq!(
            method_target.range.start,
            src.find("meth").expect("interface method declaration")
        );
    }

    #[test]
    fn hovered_resolved_symbol_at_formats_function_module_signature() {
        let store = DocumentStore::default();
        let dep_src = "\
FUNCTION /AIF/FILE_PROCESS_DATA
  IMPORTING
    iv_name TYPE string
  CHANGING
    cv_text TYPE string
  EXCEPTIONS
    failed.
ENDFUNCTION.";
        let main_src = "\
START-OF-SELECTION.
  CALL FUNCTION '/AIF/FILE_PROCESS_DATA'.";
        store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///fm_hover_main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///fm_hover_dep.abap"),
                version: 1,
                text: Arc::from(dep_src),
                is_dependency: true,
                object_name: None,
            },
        ]);
        let snapshot = store
            .documents
            .read()
            .get("file:///fm_hover_main.abap")
            .cloned()
            .expect("main snapshot");
        let offset = main_src.find("/AIF/FILE_PROCESS_DATA").expect("fm name") + 2;

        let hovered = snapshot
            .hovered_resolved_symbol_at(offset)
            .expect("function module hover");
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line.contains("FUNCTION /aif/file_process_data")),
            "{:?}",
            hovered.markdown_lines
        );
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line.contains("EXCEPTIONS")),
            "{:?}",
            hovered.markdown_lines
        );
    }

    #[test]
    fn hovered_resolved_symbol_at_includes_function_module_raising_signature_section() {
        let store = DocumentStore::default();
        let dep_src = "\
CLASS cx_demo DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

FUNCTION z_demo
  IMPORTING
    iv_name TYPE string
  RAISING
    resumable(cx_demo)
    cx_other.
ENDFUNCTION.

CLASS cx_other DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.";
        let main_src = "\
START-OF-SELECTION.
  CALL FUNCTION 'Z_DEMO'.";
        store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///fm_hover_raising_main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///fm_hover_raising_dep.abap"),
                version: 1,
                text: Arc::from(dep_src),
                is_dependency: true,
                object_name: None,
            },
        ]);
        let snapshot = store
            .documents
            .read()
            .get("file:///fm_hover_raising_main.abap")
            .cloned()
            .expect("main snapshot");
        let offset = main_src.find("Z_DEMO").expect("fm name") + 1;

        let hovered = snapshot
            .hovered_resolved_symbol_at(offset)
            .expect("function module hover");
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line.contains("RAISING")),
            "{:?}",
            hovered.markdown_lines
        );
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line.contains("resumable(cx_demo)")),
            "{:?}",
            hovered.markdown_lines
        );
    }

    #[test]
    fn hovered_resolved_symbol_at_formats_form_parameter_signature() {
        let store = DocumentStore::default();
        let src = "\
FORM f USING VALUE(iv_input) TYPE i CHANGING cv_text TYPE string.
  cv_text = |{ iv_input }|.
ENDFORM.";
        let snapshot = store.publish("file:///form_hover.abap", 1, src);
        let offset = src.rfind("iv_input").expect("parameter use") + 1;

        let hovered = snapshot
            .hovered_resolved_symbol_at(offset)
            .expect("form parameter hover");
        assert_eq!(hovered.display_name.as_ref(), "iv_input");
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line == "```abap\nUSING\n  VALUE(iv_input) TYPE i\n```"),
            "{:?}",
            hovered.markdown_lines
        );
        assert!(
            hovered
                .markdown_lines
                .iter()
                .all(|line| !line.contains("FORM f")),
            "{:?}",
            hovered.markdown_lines
        );
    }

    #[test]
    fn hovered_resolved_symbol_at_includes_form_raising_signature_section() {
        let store = DocumentStore::default();
        let src = "\
CLASS cx_demo DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.
CLASS cx_other DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

FORM f USING VALUE(iv_input) TYPE i RAISING resumable(cx_demo) cx_other.
ENDFORM.

        START-OF-SELECTION.
  DATA lv_input TYPE i VALUE 1.
  PERFORM f USING lv_input.
";
        let snapshot = store.publish("file:///form_hover_raising.abap", 1, src);
        let offset = src.rfind("PERFORM f").expect("perform target") + "PERFORM ".len();

        let hovered = snapshot
            .hovered_resolved_symbol_at(offset)
            .expect("form hover");
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line.contains("RAISING")),
            "{:?}",
            hovered.markdown_lines
        );
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line.contains("cx_other")),
            "{:?}",
            hovered.markdown_lines
        );
    }

    #[test]
    fn call_function_resolves_from_composite_function_group_dependency_source() {
        let store = DocumentStore::default();
        let dep_src = "\
FUNCTION-POOL btch.
INCLUDE lbtchtop.

FUNCTION BP_JOB_SELECT
  IMPORTING
    jobselect_dialog TYPE c
  EXCEPTIONS
    invalid_dialog_type.
ENDFUNCTION.";
        let main_src = "\
START-OF-SELECTION.
  CALL FUNCTION 'BP_JOB_SELECT'
    EXCEPTIONS
      invalid_dialog_type = 1.";
        store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///fm_group_main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///fm_group_dep.abap"),
                version: 1,
                text: Arc::from(dep_src),
                is_dependency: true,
                object_name: Some(Arc::from("bp_job_select")),
            },
        ]);
        let snapshot = store
            .documents
            .read()
            .get("file:///fm_group_main.abap")
            .cloned()
            .expect("main snapshot");

        assert!(
            snapshot
                .symbols
                .diagnostics
                .iter()
                .all(|diagnostic| diagnostic.message != "unknown routine 'bp_job_select'"),
            "{:?}",
            snapshot.symbols.diagnostics
        );
    }

    #[test]
    fn call_function_resolves_from_dependency_source_with_multiline_function_signature() {
        let store = DocumentStore::default();
        let dep_src = "\
FUNCTION-POOL rsdg.
INCLUDE lrsdctop.

FUNCTION POPUP_TO_CONFIRM_WITH_TABLE
  IMPORTING
    TITLEBAR TYPE ANY DEFAULT SPACE ##ADT_PARAMETER_UNTYPED
    START_COLUMN TYPE SY-CUCOL DEFAULT 25
    START_ROW TYPE SY-CUROW DEFAULT 6
    END_COLUMN TYPE SY-CUCOL DEFAULT 90
    END_ROW TYPE SY-CUROW DEFAULT 20
    COLUMNNAME TYPE ANY ##ADT_PARAMETER_UNTYPED
  EXPORTING
    ANSWER TYPE ANY ##ADT_PARAMETER_UNTYPED
  CHANGING
    CT_DISPLAYTABLE TYPE SESF_STRING_TAB.
  DATA icon_question(21) TYPE c VALUE 'ICON_MESSAGE_QUESTION'.
  answer = icon_question.
ENDFUNCTION.

MODULE init_0100 OUTPUT.
  WRITE space.
ENDMODULE.
";
        let main_src = "\
START-OF-SELECTION.
  CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_TABLE'.";
        store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///popup_main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///popup_dep.abap"),
                version: 1,
                text: Arc::from(dep_src),
                is_dependency: true,
                object_name: Some(Arc::from("popup_to_confirm_with_table")),
            },
        ]);
        let snapshot = store
            .documents
            .read()
            .get("file:///popup_main.abap")
            .cloned()
            .expect("main snapshot");

        assert!(
            snapshot.symbols.diagnostics.iter().all(|diagnostic| {
                diagnostic.message != "unknown routine 'popup_to_confirm_with_table'"
            }),
            "{:?}",
            snapshot.symbols.diagnostics
        );
    }

    #[test]
    fn call_function_resolves_from_realistic_local_export_function_module_source() {
        let dep_src = "\
*******************************************************************
*   System-defined Include-files.                                 *
*******************************************************************
  INCLUDE LRSDCTOP.                          \" Global Data
* INCLUDE LRSDCUXX. Omitted in dependency cache; function module stays in its own unit.

*******************************************************************
*   User-defined Include-files (if necessary).                    *
*******************************************************************
  INCLUDE LRSDCF00.                          \" Subprograms
* INCLUDE LRSDCO...                          \" PBO-Modules
* INCLUDE LRSDCI...                          \" PAI-Modules

TYPES: BEGIN OF ty_s_displaytab,
         row(40) TYPE c,
       END OF ty_s_displaytab.
TYPES: ty_t_displaytab TYPE STANDARD TABLE OF ty_s_displaytab WITH DEFAULT KEY.

CONSTANTS gc_max_height TYPE i VALUE '30'.

DATA ok_code               TYPE sy-ucomm.
DATA g_title(80)           TYPE c.
DATA g_columnname          TYPE SCRTEXT_L.
DATA g_antwort(1)          TYPE c.
DATA gt_displaytab         TYPE ty_t_displaytab.
DATA gr_table              TYPE REF TO cl_salv_table.
DATA gr_container          TYPE REF TO cl_gui_custom_container.

CLASS cl_gui_cfw DEFINITION LOAD.

FUNCTION POPUP_TO_CONFIRM_WITH_TABLE
  IMPORTING
    TITLEBAR TYPE ANY DEFAULT SPACE ##ADT_PARAMETER_UNTYPED
    START_COLUMN TYPE SY-CUCOL DEFAULT 25
    START_ROW TYPE SY-CUROW DEFAULT 6
    END_COLUMN TYPE SY-CUCOL DEFAULT 90
    END_ROW TYPE SY-CUROW DEFAULT 20
    COLUMNNAME TYPE ANY ##ADT_PARAMETER_UNTYPED
  EXPORTING
    ANSWER TYPE ANY ##ADT_PARAMETER_UNTYPED
  CHANGING
    CT_DISPLAYTABLE TYPE SESF_STRING_TAB.



  DATA icon_question(21) TYPE c VALUE 'ICON_MESSAGE_QUESTION'.

* Set texts
  g_title      = titlebar.
  g_columnname = columnname.

* Move data
  PERFORM map_input_data USING ct_displaytable CHANGING gt_displaytab.

* Call popup
  CALL SCREEN 0200 STARTING AT start_column start_row
                     ENDING AT end_column end_row.
  answer = g_antwort.
ENDFUNCTION.

MODULE init_0100 OUTPUT.
  PERFORM display_data.
ENDMODULE.

FORM map_input_data USING ct_displaytable TYPE sesf_string_tab
                    CHANGING ct_grid TYPE ty_t_displaytab.
  LOOP AT ct_displaytable INTO DATA(lv_line).
    APPEND VALUE #( row = lv_line ) TO ct_grid.
  ENDLOOP.
ENDFORM.

FORM display_data.
  WRITE space.
ENDFORM.
";
        let projected = dependency_surface_text(dep_src);
        assert!(
            projected.contains("FUNCTION POPUP_TO_CONFIRM_WITH_TABLE"),
            "{projected}"
        );
        let store = DocumentStore::default();
        let main_src = "\
START-OF-SELECTION.
  CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_TABLE'.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///popup_real_main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///popup_real_dep.abap"),
                version: 1,
                text: Arc::from(dep_src),
                is_dependency: true,
                object_name: Some(Arc::from("popup_to_confirm_with_table")),
            },
        ]);
        let snapshot = snapshots
            .get("file:///popup_real_main.abap")
            .cloned()
            .expect("main snapshot");
        let dep = snapshots
            .get("file:///popup_real_dep.abap")
            .cloned()
            .expect("dep snapshot");

        assert!(
            !dep.symbols.function_modules.is_empty(),
            "{:#?}",
            dep.symbols.function_modules
        );
        assert!(
            snapshot.symbols.diagnostics.iter().all(|diagnostic| {
                diagnostic.message != "unknown routine 'popup_to_confirm_with_table'"
            }),
            "{:?}",
            snapshot.symbols.diagnostics
        );
    }

    #[test]
    fn opened_composite_function_module_dependency_keeps_function_body_analysis() {
        let store = DocumentStore::default();
        let dep_src = "\
* >>> BEGIN INCLUDE ltop
FORM helper.
  DATA lv_top TYPE i.
  lv_top = 1.
ENDFORM.
* <<< END INCLUDE ltop

* >>> BEGIN FUNCTION MODULE z_keep
FUNCTION z_keep.
  DATA lv_body TYPE i.
  lv_body = 1.
ENDFUNCTION.
* <<< END FUNCTION MODULE z_keep
";

        let top_use_offset = dep_src
            .match_indices("lv_top")
            .nth(1)
            .map(|(offset, _)| offset + 1)
            .expect("top include use");
        let body_use_offset = dep_src
            .match_indices("lv_body")
            .nth(1)
            .map(|(offset, _)| offset + 1)
            .expect("function body use");

        let snapshot = store.publish_input(DocumentInput {
            uri: Arc::from("file:///fm_open_dep.abap"),
            version: 1,
            text: Arc::from(dep_src),
            is_dependency: false,
            object_name: Some(Arc::from("z_keep")),
        });

        assert!(snapshot.definition_at(top_use_offset).is_none());
        assert!(snapshot.definition_at(body_use_offset).is_some());
    }

    #[test]
    fn completion_and_definition_work_for_call_function_named_arguments() {
        let store = DocumentStore::default();
        let dep_src = "\
FUNCTION /AIF/FILE_PROCESS_DATA
  IMPORTING
    iv_name TYPE string
    iv_mode TYPE i OPTIONAL
  CHANGING
    cv_text TYPE string
  EXCEPTIONS
    failed.
ENDFUNCTION.";
        let main_src = "\
START-OF-SELECTION.
  DATA lv_text TYPE string.
  CALL FUNCTION '/AIF/FILE_PROCESS_DATA'
    EXPORTING
      iv
    CHANGING
      cv_text = lv_text
    EXCEPTIONS
      failed = 1.";
        store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///fm_completion_main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///fm_completion_dep.abap"),
                version: 1,
                text: Arc::from(dep_src),
                is_dependency: true,
                object_name: None,
            },
        ]);
        let snapshot = store
            .documents
            .read()
            .get("file:///fm_completion_main.abap")
            .cloned()
            .expect("main snapshot");

        let completion_offset = main_src.find("iv\n").expect("iv prefix") + 2;
        let completion = snapshot
            .completion_at(completion_offset)
            .expect("named argument completion");
        let names: Vec<_> = completion
            .items
            .iter()
            .map(|item| match item {
                crate::CompletionItem::Selector(item) => item.name.as_ref(),
                crate::CompletionItem::NamedArgument(item) => item.name.as_ref(),
                crate::CompletionItem::Symbol(item) => item.name.as_ref(),
                crate::CompletionItem::Template(item) => item.name.as_ref(),
                crate::CompletionItem::Callable(item) => item.name.as_ref(),
                crate::CompletionItem::Keyword(item) => item.name.as_ref(),
            })
            .collect();
        assert_eq!(names, vec!["iv_mode", "iv_name"]);

        let parameter_offset = main_src.find("cv_text").expect("cv_text use") + 2;
        let hovered = snapshot
            .hovered_named_argument_at(parameter_offset)
            .expect("parameter hover");
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line == "```abap\nTYPE string\n```"),
            "{:?}",
            hovered.markdown_lines
        );

        let target = snapshot
            .definition_at(parameter_offset)
            .expect("parameter definition");
        assert_eq!(target.uri.as_ref(), "file:///fm_completion_dep.abap");
        assert_eq!(&dep_src[target.range], "cv_text");
    }

    #[test]
    fn completion_and_definition_work_for_raise_event_named_arguments() {
        let store = DocumentStore::default();
        let src = "\
INTERFACE lif_source.
  EVENTS changed EXPORTING VALUE(value) TYPE string.
ENDINTERFACE.

CLASS lcl_sender DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_source.
    METHODS trigger.
ENDCLASS.

CLASS lcl_sender IMPLEMENTATION.
  METHOD trigger.
    RAISE EVENT changed
      EXPORTING
        val.
  ENDMETHOD.
ENDCLASS.";
        let snapshot = store.publish("file:///event_completion.abap", 1, src);

        let completion_offset = src.find("val.\n").expect("value prefix") + 2;
        let completion = snapshot
            .completion_at(completion_offset)
            .expect("event named argument completion");
        let names: Vec<_> = completion
            .items
            .iter()
            .map(|item| match item {
                crate::CompletionItem::Selector(item) => item.name.as_ref(),
                crate::CompletionItem::NamedArgument(item) => item.name.as_ref(),
                crate::CompletionItem::Symbol(item) => item.name.as_ref(),
                crate::CompletionItem::Template(item) => item.name.as_ref(),
                crate::CompletionItem::Callable(item) => item.name.as_ref(),
                crate::CompletionItem::Keyword(item) => item.name.as_ref(),
            })
            .collect();
        assert_eq!(names, vec!["value"]);

        let src_with_value = src.replace("val.", "value = 'x'.");
        let snapshot = store.publish("file:///event_completion.abap", 2, &src_with_value);
        let parameter_offset = src_with_value.rfind("value").expect("event parameter use") + 1;

        let hovered = snapshot
            .hovered_named_argument_at(parameter_offset)
            .expect("event parameter hover");
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line == "```abap\nTYPE string\n```"),
            "{:?}",
            hovered.markdown_lines
        );

        let target = snapshot
            .definition_at(parameter_offset)
            .expect("event parameter definition");
        assert_target_slice(
            &target,
            "file:///event_completion.abap",
            &src_with_value,
            "value",
        );
        assert_eq!(
            target.range.start,
            src_with_value
                .find("value) TYPE string")
                .expect("event parameter declaration")
        );
    }

    #[test]
    fn completion_returns_keyword_combinations_for_bare_identifier_prefix() {
        let store = DocumentStore::default();
        let src = "cl";
        let snapshot = store.publish("file:///keyword_completion.abap", 1, src);

        let completion = snapshot
            .completion_at(src.len())
            .expect("keyword completion");
        let labels: Vec<_> = completion
            .items
            .iter()
            .filter_map(|item| match item {
                crate::CompletionItem::Keyword(item) => Some(item.name.as_ref()),
                _ => None,
            })
            .collect();

        assert!(labels.contains(&"CLASS"));
        assert!(labels.contains(&"CLASS-DATA"));
        assert!(labels.contains(&"CLASS-METHODS"));
        assert!(labels.contains(&"CLASS DEFINITION"));
    }

    #[test]
    fn completion_appends_keywords_after_method_parameter_symbols() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING iv_input TYPE i.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    i
  ENDMETHOD.
ENDCLASS.";
        let snapshot = store.publish("file:///keyword_after_symbols.abap", 1, src);
        let completion_offset = src.find("    i\n").expect("completion prefix") + "    i".len();

        let completion = snapshot
            .completion_at(completion_offset)
            .expect("mixed completion");
        let first = completion.items.first().expect("first completion item");
        assert!(matches!(
            first,
            crate::CompletionItem::NamedArgument(item) if item.name.as_ref() == "iv_input"
        ));
        assert!(completion.items.iter().any(|item| {
            matches!(item, crate::CompletionItem::Keyword(item) if item.name.as_ref() == "IF")
        }));
    }

    #[test]
    fn completion_returns_global_variables_and_types_in_scope() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF ts_obj,
        objid TYPE c LENGTH 50,
        status_pack TYPE i,
       END OF ts_obj,

       tt_obj TYPE TABLE OF ts_obj.

TYPES: BEGIN OF ts_obj_ids,
        objid TYPE c LENGTH 50,
        serial TYPE c LENGTH 60,
       END OF ts_obj_ids,

       tt_obj_ids TYPE TABLE OF ts_obj_ids.

TYPES: BEGIN OF ts_loc,
        locno TYPE c LENGTH 6,
        gln TYPE c LENGTH 13,
       END OF ts_loc,

       tt_loc TYPE TABLE OF ts_loc.

DATA: lt_obj TYPE tt_obj,
      ls_obj TYPE ts_obj,
      lt_obj_ids TYPE tt_obj_ids,
      ls_obj_ids TYPE ts_obj_ids,
      lt_loc TYPE tt_loc,
      ls_loc TYPE ts_loc.

MOVE-CORRESPONDING ls_loc TO ls_obj.

SORT lt_obj BY objid.
";
        let snapshot = store.publish("file:///global_completion.abap", 1, src);

        let type_offset =
            src.find("lt_obj TYPE tt_obj").expect("type usage") + "lt_obj TYPE tt_".len();
        let type_completion = snapshot
            .completion_at(type_offset)
            .expect("type completion");
        assert!(type_completion.in_type_position);
        let type_names: Vec<_> = type_completion
            .items
            .iter()
            .filter_map(|item| match item {
                crate::CompletionItem::Symbol(item) if item.kind == SymbolKind::TypeDef => {
                    Some(item.name.as_ref())
                }
                _ => None,
            })
            .collect();
        assert!(
            type_names.contains(&"tt_obj")
                && type_names.contains(&"tt_obj_ids")
                && type_names.contains(&"tt_loc"),
            "expected table type names in completion: {type_names:?}"
        );

        let value_offset = src.find("MOVE-CORRESPONDING ls_loc").expect("value usage")
            + "MOVE-CORRESPONDING ls_".len();
        let value_completion = snapshot
            .completion_at(value_offset)
            .expect("value completion");
        assert!(!value_completion.in_type_position);
        let value_names: Vec<_> = value_completion
            .items
            .iter()
            .filter_map(|item| match item {
                crate::CompletionItem::Symbol(item) if item.kind == SymbolKind::Variable => {
                    Some(item.name.as_ref())
                }
                _ => None,
            })
            .collect();
        assert!(
            value_names.contains(&"ls_loc")
                && value_names.contains(&"ls_obj")
                && value_names.contains(&"ls_obj_ids"),
            "expected global variables in completion: {value_names:?}"
        );

        let sort_offset = src.find("SORT lt_obj").expect("sort usage") + "SORT lt_".len();
        let sort_completion = snapshot
            .completion_at(sort_offset)
            .expect("sort target completion");
        let sort_names: Vec<_> = sort_completion
            .items
            .iter()
            .filter_map(|item| match item {
                crate::CompletionItem::Symbol(item) if item.kind == SymbolKind::Variable => {
                    Some(item.name.as_ref())
                }
                _ => None,
            })
            .collect();
        assert!(
            sort_names.contains(&"lt_obj")
                && sort_names.contains(&"lt_obj_ids")
                && sort_names.contains(&"lt_loc"),
            "expected global table variables in completion: {sort_names:?}"
        );
    }

    #[test]
    fn completion_returns_field_symbol_for_unclosed_angle_prefix() {
        let store = DocumentStore::default();
        let src = "\
DATA lt_rows TYPE STANDARD TABLE OF i.
APPEND INITIAL LINE TO lt_rows ASSIGNING FIELD-SYMBOL(<fs_row>).
<fs";
        let snapshot = store.publish("file:///field_symbol_completion.abap", 1, src);

        let completion = snapshot
            .completion_at(src.len())
            .expect("field symbol completion");
        assert_eq!(&src[completion.replace_range.clone()], "<fs");
        assert!(completion.items.iter().any(|item| {
            matches!(
                item,
                crate::CompletionItem::Symbol(item)
                    if item.kind == SymbolKind::FieldSymbol && item.name.as_ref() == "<fs_row>"
            )
        }));
    }

    #[test]
    fn completion_returns_field_symbol_after_lone_angle_at_statement_start() {
        let store = DocumentStore::default();
        let src = "\
DATA lt_rows TYPE STANDARD TABLE OF i.
APPEND INITIAL LINE TO lt_rows ASSIGNING FIELD-SYMBOL(<fs_row>).
<";
        let snapshot = store.publish("file:///field_symbol_angle_completion.abap", 1, src);

        let completion = snapshot
            .completion_at(src.len())
            .expect("field symbol completion");
        assert_eq!(&src[completion.replace_range.clone()], "<");
        assert!(completion.items.iter().any(|item| {
            matches!(
                item,
                crate::CompletionItem::Symbol(item)
                    if item.kind == SymbolKind::FieldSymbol && item.name.as_ref() == "<fs_row>"
            )
        }));
    }

    #[test]
    fn completion_returns_type_names_inside_types_table_declaration() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF ts_obj,
        objid TYPE c LENGTH 50,
       END OF ts_obj,
       tt_obj TYPE TABLE OF ts_.
";
        let snapshot = store.publish("file:///types_table_completion.abap", 1, src);
        let completion_offset = src
            .find("tt_obj TYPE TABLE OF ts_")
            .expect("table type declaration")
            + "tt_obj TYPE TABLE OF ts_".len();

        let completion = snapshot
            .completion_at(completion_offset)
            .expect("type completion");
        assert!(completion.in_type_position);
        assert!(
            completion.items.iter().any(|item| {
                matches!(
                    item,
                    crate::CompletionItem::Symbol(item)
                        if item.kind == SymbolKind::TypeDef && item.name.as_ref() == "ts_obj"
                )
            }),
            "expected ts_obj type completion: {:?}",
            completion.items
        );
    }

    #[test]
    fn completion_returns_type_names_inside_unterminated_types_table_declaration() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF ts_obj,
        objid TYPE c LENGTH 50,
       END OF ts_obj.

TYPES tt_obj TYPE TABLE OF ts_";
        let snapshot = store.publish("file:///unterminated_types_table_completion.abap", 1, src);

        let completion = snapshot.completion_at(src.len()).expect("type completion");
        assert!(completion.in_type_position);
        assert!(
            completion.items.iter().any(|item| {
                matches!(
                    item,
                    crate::CompletionItem::Symbol(item)
                        if item.kind == SymbolKind::TypeDef && item.name.as_ref() == "ts_obj"
                )
            }),
            "expected ts_obj type completion: {:?}",
            completion.items
        );
    }

    #[test]
    fn completion_returns_function_module_call_templates() {
        let store = DocumentStore::default();
        let dep_src = "\
FUNCTION z_demo_call
  IMPORTING
    iv_name TYPE string
    iv_mode TYPE i OPTIONAL
  EXPORTING
    ev_text TYPE string
  CHANGING
    cv_total TYPE i
  TABLES
    tt_return STRUCTURE bapiret2
  EXCEPTIONS
    failed
    missing_input.
ENDFUNCTION.";
        let main_src = "\
START-OF-SELECTION.
  CALL FUNCTION 'z_de";
        store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///fm_template_main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///fm_template_dep.abap"),
                version: 1,
                text: Arc::from(dep_src),
                is_dependency: true,
                object_name: None,
            },
        ]);
        let snapshot = store
            .documents
            .read()
            .get("file:///fm_template_main.abap")
            .cloned()
            .expect("main snapshot");

        let completion_offset = main_src.len();
        let completion = snapshot
            .completion_at(completion_offset)
            .expect("call function completion");
        let item = completion
            .items
            .iter()
            .find_map(|item| match item {
                crate::CompletionItem::Callable(item) if item.name.as_ref() == "z_demo_call" => {
                    Some(item)
                }
                _ => None,
            })
            .expect("function module completion item");
        assert_eq!(
            item.insertion.plain_text,
            "z_demo_call'\n  EXPORTING\n    iv_name = \n  IMPORTING\n    ev_text = \n  CHANGING\n    cv_total = \n  TABLES\n    tt_return = \n  EXCEPTIONS\n    failed = 1\n    missing_input = 2."
        );
        assert_eq!(
            item.insertion.snippet_text.as_deref(),
            Some(
                "z_demo_call'\n  EXPORTING\n    iv_name = ${1}\n  IMPORTING\n    ev_text = ${2}\n  CHANGING\n    cv_total = ${3}\n  TABLES\n    tt_return = ${4}\n  EXCEPTIONS\n    failed = ${5:1}\n    missing_input = ${6:2}.$0"
            )
        );
    }

    #[test]
    fn completion_returns_method_parameters_inside_method_implementation() {
        let store = DocumentStore::default();
        let src = "\
CLASS lo_epcis_builder DEFINITION.
  PUBLIC SECTION.
    METHODS method_name
      IMPORTING
        iv_importing TYPE i
      EXPORTING
        ev_exporting TYPE i
      CHANGING
        cv_changing TYPE i
      RETURNING
        VALUE(rv_returning) TYPE i.
ENDCLASS.

CLASS lo_epcis_builder IMPLEMENTATION.
  METHOD method_name.
    rv_returning = iv_imp
  ENDMETHOD.
ENDCLASS.";
        store.replace_all(vec![DocumentInput {
            uri: Arc::from("file:///method_impl_param_completion.abap"),
            version: 1,
            text: Arc::from(src),
            is_dependency: false,
            object_name: None,
        }]);
        let snapshot = store
            .documents
            .read()
            .get("file:///method_impl_param_completion.abap")
            .cloned()
            .expect("snapshot");

        let completion_offset =
            src.rfind("iv_imp").expect("iv_imp prefix in method body") + "iv_imp".len();
        let completion = snapshot
            .completion_at(completion_offset)
            .expect("method parameter completion");
        let names: Vec<_> = completion
            .items
            .iter()
            .map(|item| match item {
                crate::CompletionItem::Selector(item) => item.name.as_ref(),
                crate::CompletionItem::NamedArgument(item) => item.name.as_ref(),
                crate::CompletionItem::Symbol(item) => item.name.as_ref(),
                crate::CompletionItem::Template(item) => item.name.as_ref(),
                crate::CompletionItem::Callable(item) => item.name.as_ref(),
                crate::CompletionItem::Keyword(item) => item.name.as_ref(),
            })
            .collect();
        assert_eq!(names, vec!["iv_importing"]);

        let item = completion
            .items
            .iter()
            .find_map(|item| match item {
                crate::CompletionItem::NamedArgument(item)
                    if item.name.as_ref() == "iv_importing" =>
                {
                    Some(item)
                }
                _ => None,
            })
            .expect("iv_importing completion item");
        assert_eq!(item.insertion.plain_text, "iv_importing");
        assert_eq!(item.declaration.as_deref(), Some("iv_importing i"));
    }

    #[test]
    fn completion_returns_method_parameter_after_incomplete_block_before_endmethod() {
        let store = DocumentStore::default();
        for (uri, incomplete_stmt) in [
            ("file:///case_param_completion.abap", "    CASE iv"),
            ("file:///if_param_completion.abap", "    IF iv"),
        ] {
            let src = format!(
                "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS status_from_rep_evt_status
      IMPORTING iv_status_rep_evt TYPE i
      RETURNING VALUE(rv_status) TYPE string.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD status_from_rep_evt_status.
    CASE iv_status_rep_evt.
      WHEN 0.
        rv_status = ''.
    ENDCASE.
{incomplete_stmt}
  ENDMETHOD.
ENDCLASS."
            );
            let snapshot = store.publish(uri, 1, &src);
            let completion_offset = src.rfind(" iv").expect("completion prefix") + " iv".len();

            let completion = snapshot
                .completion_at(completion_offset)
                .expect("method parameter completion");

            assert!(completion.items.iter().any(|item| {
                matches!(item, crate::CompletionItem::NamedArgument(item) if item.name.as_ref() == "iv_status_rep_evt")
                    || matches!(item, crate::CompletionItem::Symbol(item) if item.name.as_ref() == "iv_status_rep_evt")
            }));
        }
    }

    #[test]
    fn completion_returns_function_module_call_templates_before_following_statement() {
        let store = DocumentStore::default();
        let dep_src = "\
FUNCTION job_close
  IMPORTING
    jobcount TYPE btcjobcnt
    jobname TYPE btcjob
  EXCEPTIONS
    cant_start_immediate.
ENDFUNCTION.";
        let main_src = "\
START-OF-SELECTION.
  CALL FUNCTION 'JOB_
  LOOP AT lt_items INTO DATA(ls_item).
  ENDLOOP.";
        store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///fm_line_local_main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///fm_line_local_dep.abap"),
                version: 1,
                text: Arc::from(dep_src),
                is_dependency: true,
                object_name: None,
            },
        ]);
        let snapshot = store
            .documents
            .read()
            .get("file:///fm_line_local_main.abap")
            .cloned()
            .expect("main snapshot");

        let completion_offset = main_src.find("JOB_").expect("JOB_ prefix") + 4;
        let completion = snapshot
            .completion_at(completion_offset)
            .expect("call function completion");
        assert_eq!(&main_src[completion.replace_range.clone()], "JOB_");
        let item = completion
            .items
            .iter()
            .find_map(|item| match item {
                crate::CompletionItem::Callable(item) if item.name.as_ref() == "job_close" => {
                    Some(item)
                }
                _ => None,
            })
            .expect("function module completion item");
        assert_eq!(
            item.insertion.snippet_text.as_deref(),
            Some(
                "job_close'\n  EXPORTING\n    jobcount = ${1}\n    jobname = ${2}\n  EXCEPTIONS\n    cant_start_immediate = ${3:1}.$0"
            )
        );
    }

    #[test]
    fn completion_infers_function_module_template_from_other_project_call_sites() {
        let store = DocumentStore::default();
        let src = "\
REPORT zdemo.
CALL FUNCTION 'z_vle_attp_del_block_unblock
LOOP AT lt_items INTO DATA(ls_item).
ENDLOOP.

CALL FUNCTION 'Z_VLE_ATTP_DEL_BLOCK_UNBLOCK'
  EXPORTING
    iw_warehouse   = lw_lgnum
    iw_plant       = lw_werks
    iw_delivery    = lw_vbeln
    iw_uname       = sy-uname
    iw_clear_block = abap_true.";
        let snapshot = store.publish("file:///fm_inferred_template.abap", 1, src);

        let completion_offset = src.find("z_vle_attp_del_block_unblock").expect("prefix")
            + "z_vle_attp_del_block_unblock".len();
        let completion = snapshot
            .completion_at(completion_offset)
            .expect("call function completion");
        let item = completion
            .items
            .iter()
            .find_map(|item| match item {
                crate::CompletionItem::Callable(item)
                    if item.name.as_ref() == "z_vle_attp_del_block_unblock" =>
                {
                    Some(item)
                }
                _ => None,
            })
            .expect("inferred function module completion item");
        assert!(
            item.declaration
                .as_deref()
                .is_some_and(|declaration| declaration.contains("inferred from project call sites"))
        );
        assert_eq!(
            item.insertion.plain_text,
            "z_vle_attp_del_block_unblock'\n  EXPORTING\n    iw_warehouse = \n    iw_plant = \n    iw_delivery = \n    iw_uname = \n    iw_clear_block = ."
        );
        assert_eq!(
            item.insertion.snippet_text.as_deref(),
            Some(
                "z_vle_attp_del_block_unblock'\n  EXPORTING\n    iw_warehouse = ${1}\n    iw_plant = ${2}\n    iw_delivery = ${3}\n    iw_uname = ${4}\n    iw_clear_block = ${5}.$0"
            )
        );
    }

    #[test]
    fn completion_returns_perform_call_templates() {
        let store = DocumentStore::default();
        let src = "\
FORM update_item USING uv_name TYPE string CHANGING cv_total TYPE i.
ENDFORM.

START-OF-SELECTION.
  PERFORM up";
        let snapshot = store.publish("file:///perform_template.abap", 1, src);

        let completion = snapshot
            .completion_at(src.len())
            .expect("perform completion");
        let item = completion
            .items
            .iter()
            .find_map(|item| match item {
                crate::CompletionItem::Callable(item) if item.name.as_ref() == "update_item" => {
                    Some(item)
                }
                _ => None,
            })
            .expect("perform completion item");
        assert_eq!(
            item.insertion.plain_text,
            "update_item\n  USING\n    uv_name\n  CHANGING\n    cv_total."
        );
        assert_eq!(
            item.insertion.snippet_text.as_deref(),
            Some("update_item\n  USING\n    ${1:uv_name}\n  CHANGING\n    ${2:cv_total}.$0")
        );
    }

    #[test]
    fn completion_returns_perform_call_templates_before_endform() {
        let store = DocumentStore::default();
        let src = "\
FORM run.
  PERFORM he
ENDFORM.

FORM helper USING iv_value TYPE i.
ENDFORM.";
        let snapshot = store.publish("file:///perform_before_endform.abap", 1, src);

        let completion_offset =
            src.find("PERFORM he").expect("perform prefix") + "PERFORM he".len();
        let completion = snapshot
            .completion_at(completion_offset)
            .expect("perform completion");
        assert_eq!(&src[completion.replace_range.clone()], "he");
        let item = completion
            .items
            .iter()
            .find_map(|item| match item {
                crate::CompletionItem::Callable(item) if item.name.as_ref() == "helper" => {
                    Some(item)
                }
                _ => None,
            })
            .expect("perform completion item");
        assert_eq!(item.insertion.plain_text, "helper\n  USING\n    iv_value.");
        assert_eq!(
            item.insertion.snippet_text.as_deref(),
            Some("helper\n  USING\n    ${1:iv_value}.$0")
        );
    }

    #[test]
    fn completion_returns_function_module_call_templates_before_endform() {
        let store = DocumentStore::default();
        let dep_src = "\
FUNCTION z_demo_call
  IMPORTING
    iv_name TYPE string
  EXPORTING
    ev_text TYPE string
  EXCEPTIONS
    failed.
ENDFUNCTION.";
        let main_src = "\
FORM run.
  CALL FUNCTION 'z_de
ENDFORM.";
        store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///fm_before_endform_main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///fm_before_endform_dep.abap"),
                version: 1,
                text: Arc::from(dep_src),
                is_dependency: true,
                object_name: None,
            },
        ]);
        let snapshot = store
            .documents
            .read()
            .get("file:///fm_before_endform_main.abap")
            .cloned()
            .expect("main snapshot");

        let completion_offset = main_src.find("z_de").expect("function prefix") + "z_de".len();
        let completion = snapshot
            .completion_at(completion_offset)
            .expect("call function completion");
        assert_eq!(&main_src[completion.replace_range.clone()], "z_de");
        let item = completion
            .items
            .iter()
            .find_map(|item| match item {
                crate::CompletionItem::Callable(item) if item.name.as_ref() == "z_demo_call" => {
                    Some(item)
                }
                _ => None,
            })
            .expect("function module completion item");
        assert_eq!(
            item.insertion.plain_text,
            "z_demo_call'\n  EXPORTING\n    iv_name = \n  IMPORTING\n    ev_text = \n  EXCEPTIONS\n    failed = 1."
        );
    }

    #[test]
    fn completion_returns_types_begin_template_inside_types_section() {
        let store = DocumentStore::default();
        let src = "\
REPORT zdemo.

TYPES:
  beg";
        let snapshot = store.publish("file:///types_begin_template.abap", 1, src);

        let completion = snapshot
            .completion_at(src.len())
            .expect("types begin template completion");
        assert_eq!(&src[completion.replace_range.clone()], "beg");
        assert!(completion.in_type_position);
        let item = completion
            .items
            .iter()
            .find_map(|item| {
                if let crate::CompletionItem::Template(item) = item
                    && item.name.as_ref() == "BEGIN OF type_name"
                {
                    Some(item)
                } else {
                    None
                }
            })
            .expect("types begin template item");
        assert_eq!(item.detail.as_deref(), Some("TYPES structure scaffold"));
        assert_eq!(
            item.insertion.plain_text,
            "BEGIN OF type_name,\nEND OF type_name."
        );
        assert_eq!(
            item.insertion.snippet_text.as_deref(),
            Some("BEGIN OF ${1:type_name},\n  $0\nEND OF ${1:type_name}.")
        );
    }

    #[test]
    fn completion_returns_types_begin_template_for_same_line_begin_prefix() {
        let store = DocumentStore::default();
        let src = "TYPES: BEGIN";
        let snapshot = store.publish("file:///types_begin_template_same_line.abap", 1, src);

        let completion = snapshot
            .completion_at(src.len())
            .expect("types begin template completion");
        assert_eq!(&src[completion.replace_range.clone()], "BEGIN");
        assert!(completion.items.iter().any(|item| {
            matches!(
                item,
                crate::CompletionItem::Template(item)
                    if item.name.as_ref() == "BEGIN OF type_name"
            )
        }));
    }

    #[test]
    fn completion_returns_types_begin_template_when_begin_keyword_typing_starts() {
        let store = DocumentStore::default();
        let src = "TYPES: B";
        let snapshot = store.publish("file:///types_begin_template_typing_begin.abap", 1, src);

        let completion = snapshot
            .completion_at(src.len())
            .expect("types begin template completion");
        assert_eq!(&src[completion.replace_range.clone()], "B");
        assert!(completion.items.iter().any(|item| {
            matches!(
                item,
                crate::CompletionItem::Template(item)
                    if item.name.as_ref() == "BEGIN OF type_name"
            )
        }));
    }

    #[test]
    fn completion_returns_types_begin_template_after_same_line_begin_prefix_whitespace() {
        let store = DocumentStore::default();
        let src = "TYPES: BEGIN ";
        let snapshot = store.publish(
            "file:///types_begin_template_same_line_whitespace.abap",
            1,
            src,
        );

        let completion = snapshot
            .completion_at(src.len())
            .expect("types begin template completion");
        assert_eq!(&src[completion.replace_range.clone()], "BEGIN ");
        assert!(completion.items.iter().any(|item| {
            matches!(
                item,
                crate::CompletionItem::Template(item)
                    if item.name.as_ref() == "BEGIN OF type_name"
            )
        }));
    }

    #[test]
    fn completion_returns_types_begin_template_after_chained_types_clause() {
        let store = DocumentStore::default();
        let src = "\
REPORT zdemo.

TYPES:
  ty_count TYPE i,
  beg";
        let snapshot = store.publish("file:///types_begin_template_after_clause.abap", 1, src);

        let completion = snapshot
            .completion_at(src.len())
            .expect("types begin template completion");
        assert_eq!(&src[completion.replace_range.clone()], "beg");
        assert!(completion.items.iter().any(|item| {
            matches!(
                item,
                crate::CompletionItem::Template(item)
                    if item.name.as_ref() == "BEGIN OF type_name"
            )
        }));
    }

    #[test]
    fn completion_does_not_return_types_begin_template_outside_types_section() {
        let store = DocumentStore::default();
        let src = "\
REPORT zdemo.

beg";
        let snapshot = store.publish("file:///types_begin_template_outside.abap", 1, src);

        let completion = snapshot
            .completion_at(src.len())
            .expect("keyword completion");
        assert!(!completion.items.iter().any(|item| {
            matches!(
                item,
                crate::CompletionItem::Template(item)
                    if item.name.as_ref() == "BEGIN OF type_name"
            )
        }));
    }

    #[test]
    fn completion_returns_local_class_definition_template_from_lcl_shorthand() {
        let store = DocumentStore::default();
        let src = "\
REPORT zdemo.

lcl_demo";
        let snapshot = store.publish("file:///local_class_template.abap", 1, src);

        let completion = snapshot
            .completion_at(src.len())
            .expect("local class template completion");
        assert_eq!(&src[completion.replace_range.clone()], "lcl_demo");
        let item = completion
            .items
            .iter()
            .find_map(|item| match item {
                crate::CompletionItem::Template(item) if item.name.as_ref() == "lcl_demo" => {
                    Some(item)
                }
                _ => None,
            })
            .expect("local class template item");
        assert_eq!(item.detail.as_deref(), Some("Local class definition"));
        assert_eq!(
            item.insertion.plain_text,
            "CLASS lcl_demo DEFINITION.\n  PUBLIC SECTION.\nENDCLASS.\n\nCLASS lcl_demo IMPLEMENTATION.\nENDCLASS."
        );
        assert_eq!(
            item.insertion.snippet_text.as_deref(),
            Some(
                "CLASS ${1:lcl_demo} DEFINITION.\n  PUBLIC SECTION.\n    $0\nENDCLASS.\n\nCLASS ${1:lcl_demo} IMPLEMENTATION.\nENDCLASS."
            )
        );
    }

    #[test]
    fn completion_defaults_local_class_template_name_when_only_lcl_prefix_is_typed() {
        let store = DocumentStore::default();
        let src = "\
REPORT zdemo.

lcl";
        let snapshot = store.publish("file:///local_class_template_default.abap", 1, src);

        let completion = snapshot
            .completion_at(src.len())
            .expect("local class template completion");
        let item = completion
            .items
            .iter()
            .find_map(|item| match item {
                crate::CompletionItem::Template(item) => Some(item),
                _ => None,
            })
            .expect("local class template item");
        assert_eq!(item.name.as_ref(), "lcl_demo");
        assert!(
            item.insertion
                .plain_text
                .starts_with("CLASS lcl_demo DEFINITION."),
            "{}",
            item.insertion.plain_text
        );
    }

    #[test]
    fn completion_returns_local_test_class_template_from_ltcl_shorthand() {
        let store = DocumentStore::default();
        let src = "\
REPORT zdemo.

ltcl_demo";
        let snapshot = store.publish("file:///local_test_class_template.abap", 1, src);

        let completion = snapshot
            .completion_at(src.len())
            .expect("local test class template completion");
        assert_eq!(&src[completion.replace_range.clone()], "ltcl_demo");
        let item = completion
            .items
            .iter()
            .find_map(|item| match item {
                crate::CompletionItem::Template(item) if item.name.as_ref() == "ltcl_demo" => {
                    Some(item)
                }
                _ => None,
            })
            .expect("local test class template item");
        assert_eq!(item.detail.as_deref(), Some("Local test class definition"));
        assert_eq!(
            item.insertion.plain_text,
            "CLASS ltcl_demo DEFINITION FOR TESTING \n  DURATION SHORT\n  RISK LEVEL HARMLESS.\n\n  PRIVATE SECTION.\n    METHODS:\n      setup,\n      teardown,\n      test_demo FOR TESTING.\nENDCLASS.\n\nCLASS ltcl_demo IMPLEMENTATION.\n\n  METHOD setup.\n  ENDMETHOD.\n\n  METHOD teardown.\n  ENDMETHOD.\n\n  METHOD test_demo.\n    cl_abap_unit_assert=>assert_equals(\n      act = abap_true \n      exp = abap_true \n    ).\n  ENDMETHOD.\nENDCLASS."
        );
        assert_eq!(
            item.insertion.snippet_text.as_deref(),
            Some(
                "CLASS ${1:ltcl_demo} DEFINITION FOR TESTING \n  DURATION SHORT\n  RISK LEVEL HARMLESS.\n\n  PRIVATE SECTION.\n    METHODS:\n      setup,\n      teardown,\n      ${2:test_demo} FOR TESTING.\nENDCLASS.\n\nCLASS ${1:ltcl_demo} IMPLEMENTATION.\n\n  METHOD setup.\n  ENDMETHOD.\n\n  METHOD teardown.\n  ENDMETHOD.\n\n  METHOD ${2:test_demo}.\n    cl_abap_unit_assert=>assert_equals(\n      act = ${3:abap_true} \n      exp = ${4:abap_true} \n    ).\n    $0\n  ENDMETHOD.\nENDCLASS."
            )
        );
    }

    #[test]
    fn completion_defaults_local_test_class_template_name_when_only_ltcl_prefix_is_typed() {
        let store = DocumentStore::default();
        let src = "\
REPORT zdemo.

ltcl";
        let snapshot = store.publish("file:///local_test_class_template_default.abap", 1, src);

        let completion = snapshot
            .completion_at(src.len())
            .expect("local test class template completion");
        let item = completion
            .items
            .iter()
            .find_map(|item| match item {
                crate::CompletionItem::Template(item) => Some(item),
                _ => None,
            })
            .expect("local test class template item");
        assert_eq!(item.name.as_ref(), "ltcl_demo");
        assert!(
            item.insertion
                .plain_text
                .starts_with("CLASS ltcl_demo DEFINITION FOR TESTING"),
            "{}",
            item.insertion.plain_text
        );
    }

    #[test]
    fn completion_returns_method_definition_template_inside_class_definition() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    meth
ENDCLASS.";
        let completion_offset = src.find("meth").expect("method prefix") + "meth".len();
        let snapshot = store.publish("file:///method_definition_template.abap", 1, src);

        let completion = snapshot
            .completion_at(completion_offset)
            .expect("method definition template completion");
        assert_eq!(&src[completion.replace_range.clone()], "meth");
        let item = completion
            .items
            .iter()
            .find_map(|item| match item {
                crate::CompletionItem::Template(item) if item.name.as_ref() == "methods" => {
                    Some(item)
                }
                _ => None,
            })
            .expect("method definition template item");
        assert_eq!(item.detail.as_deref(), Some("Method definition"));
        assert_eq!(
            item.insertion.plain_text,
            "METHODS method_name\n  IMPORTING\n    iv_importing TYPE i\n  EXPORTING\n    ev_exporting TYPE i\n  CHANGING\n    cv_changing TYPE i\n  RECEIVING\n    VALUE(rv_receiving) TYPE i\n  RETURNING\n    VALUE(rv_returning) TYPE i."
        );
        assert_eq!(
            item.insertion.snippet_text.as_deref(),
            Some(
                "METHODS ${1:method_name}\n  IMPORTING\n    ${2:iv_importing} TYPE ${3:i}\n  EXPORTING\n    ${4:ev_exporting} TYPE ${5:i}\n  CHANGING\n    ${6:cv_changing} TYPE ${7:i}\n  RECEIVING\n    VALUE(${8:rv_receiving}) TYPE ${9:i}\n  RETURNING\n    VALUE(${10:rv_returning}) TYPE ${11:i}.$0"
            )
        );
    }

    #[test]
    fn completion_does_not_return_method_definition_template_outside_class_definition() {
        let store = DocumentStore::default();
        let src = "\
REPORT zdemo.

meth";
        let snapshot = store.publish("file:///method_definition_template_global.abap", 1, src);

        let completion = snapshot
            .completion_at(src.len())
            .expect("keyword completion");
        assert!(!completion.items.iter().any(|item| {
            matches!(
                item,
                crate::CompletionItem::Template(item) if item.name.as_ref() == "methods"
            )
        }));
    }

    #[test]
    fn completion_keeps_local_class_template_after_same_class_already_exists() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.

lcl_demo";
        let snapshot = store.publish("file:///local_class_template_repeat.abap", 1, src);

        let completion = snapshot
            .completion_at(src.len())
            .expect("repeated local class template completion");
        let item = completion
            .items
            .iter()
            .find_map(|item| match item {
                crate::CompletionItem::Template(item) if item.name.as_ref() == "lcl_demo" => {
                    Some(item)
                }
                _ => None,
            })
            .expect("local class template item");
        assert_eq!(item.detail.as_deref(), Some("Local class definition"));
    }

    #[test]
    fn completion_keeps_local_class_template_between_abap_statements() {
        let store = DocumentStore::default();
        let src = "\
CLASS lo_epcis_builder DEFINITION.
  PUBLIC SECTION.
    METHODS build.
ENDCLASS.

CLASS lo_epcis_builder IMPLEMENTATION.
  METHOD build.
    
  ENDMETHOD.
ENDCLASS.

lcl

CLASS lcl_object_event DEFINITION.
  PUBLIC SECTION.
    METHODS add_to_epcis
      CHANGING
        co_epcis_builder TYPE REF TO lo_epcis_builder.
ENDCLASS.

CLASS lcl_object_event IMPLEMENTATION.

ENDCLASS.";
        let completion_offset = src.find("\nlcl\n").expect("lcl line") + "\nlcl".len();
        let snapshot = store.publish(
            "file:///local_class_template_between_statements.abap",
            1,
            src,
        );

        let completion = snapshot
            .completion_at(completion_offset)
            .expect("local class template completion");
        assert_eq!(&src[completion.replace_range.clone()], "lcl");
        let item = completion
            .items
            .iter()
            .find_map(|item| match item {
                crate::CompletionItem::Template(item) if item.name.as_ref() == "lcl_demo" => {
                    Some(item)
                }
                _ => None,
            })
            .expect("local class template item");
        assert_eq!(item.detail.as_deref(), Some("Local class definition"));
    }

    #[test]
    fn completion_keeps_local_test_class_template_after_same_class_already_exists() {
        let store = DocumentStore::default();
        let src = "\
CLASS ltcl_demo DEFINITION FOR TESTING.
  DURATION SHORT.
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test_demo FOR TESTING.
ENDCLASS.

CLASS ltcl_demo IMPLEMENTATION.
  METHOD test_demo.
  ENDMETHOD.
ENDCLASS.

ltcl_demo";
        let snapshot = store.publish("file:///local_test_class_template_repeat.abap", 1, src);

        let completion = snapshot
            .completion_at(src.len())
            .expect("repeated local test class template completion");
        let item = completion
            .items
            .iter()
            .find_map(|item| match item {
                crate::CompletionItem::Template(item) if item.name.as_ref() == "ltcl_demo" => {
                    Some(item)
                }
                _ => None,
            })
            .expect("local test class template item");
        assert_eq!(item.detail.as_deref(), Some("Local test class definition"));
    }

    #[test]
    fn definition_at_returns_namespaced_interface_targets_for_implementation_header() {
        let store = DocumentStore::default();
        let src = "\
INTERFACE /sttp/if_badi_rule_processing.
  METHODS execute.
ENDINTERFACE.

CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    INTERFACES /sttp/if_badi_rule_processing.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD /sttp/if_badi_rule_processing~execute.
  ENDMETHOD.
ENDCLASS.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let implementation_offset = src
            .rfind("/sttp/if_badi_rule_processing~execute")
            .expect("implementation header");

        let interface_target = snapshot
            .definition_at(implementation_offset + 1)
            .expect("interface definition target");
        assert_target_slice(
            &interface_target,
            "file:///demo.abap",
            src,
            "/sttp/if_badi_rule_processing",
        );

        let method_offset = implementation_offset + "/sttp/if_badi_rule_processing~".len();
        let method_target = snapshot
            .definition_at(method_offset + 1)
            .expect("interface method definition target");
        assert_target_slice(&method_target, "file:///demo.abap", src, "execute");
        assert_eq!(
            method_target.range.start,
            src.find("execute").expect("interface method declaration")
        );
    }

    #[test]
    fn definition_at_routes_qualified_interface_method_scope_symbols_to_real_targets() {
        let store = DocumentStore::default();
        let interface_src = "\
INTERFACE /sttp/if_badi_rule_processing.
  METHODS execute
    IMPORTING
      !iv_evtid TYPE /sttp/e_evtid
      !is_rule_keys TYPE /sttp/s_rules_key OPTIONAL
    CHANGING
      !co_messages TYPE REF TO /sttp/cl_messages OPTIONAL.
ENDINTERFACE.";
        let main_src = "\
CLASS zattp_cl_rs_rule_proc DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_badi_interface.
    INTERFACES /sttp/if_badi_rule_processing.
    METHODS prepare_data
      IMPORTING
        VALUE(is_rule_keys) TYPE /sttp/s_rules_key.
ENDCLASS.

CLASS zattp_cl_rs_rule_proc IMPLEMENTATION.
  METHOD /sttp/if_badi_rule_processing~execute.
    CALL METHOD me->prepare_data
      EXPORTING
        is_rule_keys = is_rule_keys.
  ENDMETHOD.
ENDCLASS.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///dep.abap"),
                version: 1,
                text: Arc::from(interface_src),
                is_dependency: true,
                object_name: Some(Arc::from("/sttp/if_badi_rule_processing")),
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");

        let me_offset = main_src.find("me->prepare_data").expect("me use") + 1;
        let me_target = snapshot
            .definition_at(me_offset)
            .expect("me definition target");
        assert_target_slice(
            &me_target,
            "file:///main.abap",
            main_src,
            "zattp_cl_rs_rule_proc",
        );
        assert_eq!(
            me_target.range.start,
            main_src
                .find("zattp_cl_rs_rule_proc")
                .expect("class declaration")
        );

        let parameter_use = main_src.rfind("is_rule_keys").expect("parameter use") + 1;
        let parameter_target = snapshot
            .definition_at(parameter_use)
            .expect("parameter definition target");
        assert_target_slice(
            &parameter_target,
            "file:///dep.abap",
            interface_src,
            "is_rule_keys",
        );
        assert_eq!(
            parameter_target.range.start,
            interface_src
                .find("is_rule_keys")
                .expect("interface parameter declaration")
        );
    }

    #[test]
    fn definition_at_resolves_inherited_interface_method_from_dependency_class_selector() {
        let store = DocumentStore::default();
        let interface_src = "\
INTERFACE i1.
  METHODS meth.
ENDINTERFACE.";
        let super_src = "\
CLASS super DEFINITION.
  PUBLIC SECTION.
    INTERFACES i1.
ENDCLASS.

CLASS super IMPLEMENTATION.
  METHOD i1~meth.
  ENDMETHOD.
ENDCLASS.";
        let sub_src = "\
CLASS sub DEFINITION
  PUBLIC
  INHERITING FROM super.
PUBLIC SECTION.
  METHODS i1~meth REDEFINITION.
ENDCLASS.

CLASS sub IMPLEMENTATION.
  METHOD i1~meth.
  ENDMETHOD.
ENDCLASS.";
        let main_src = "\
CLASS demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS demo IMPLEMENTATION.
  METHOD run.
    DATA lo_obj TYPE REF TO sub.
    CREATE OBJECT lo_obj.
    lo_obj->i1~meth( ).
  ENDMETHOD.
ENDCLASS.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///i1.abap"),
                version: 1,
                text: Arc::from(interface_src),
                is_dependency: true,
                object_name: Some(Arc::from("i1")),
            },
            DocumentInput {
                uri: Arc::from("file:///super.abap"),
                version: 1,
                text: Arc::from(super_src),
                is_dependency: true,
                object_name: Some(Arc::from("super")),
            },
            DocumentInput {
                uri: Arc::from("file:///sub.abap"),
                version: 1,
                text: Arc::from(sub_src),
                is_dependency: true,
                object_name: Some(Arc::from("sub")),
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let main = snapshots
            .get("file:///main.abap")
            .cloned()
            .expect("main snapshot should exist");
        let method_use = main_src.rfind("meth").expect("method use");

        let target = main
            .definition_at(method_use + 1)
            .expect("interface method definition target");
        assert_target_slice(&target, "file:///i1.abap", interface_src, "meth");
    }

    #[test]
    fn hover_and_definition_work_for_bare_call_to_inherited_dependency_alias() {
        let store = DocumentStore::default();
        let interface_src = "\
INTERFACE /iwbep/if_mgw_conv_srv_runtime.
  METHODS copy_data_to_ref
    IMPORTING is_data TYPE string
    CHANGING cr_data TYPE string.
ENDINTERFACE.";
        let grandparent_src = "\
CLASS /iwbep/cl_mgw_abs_data DEFINITION.
  PUBLIC SECTION.
    INTERFACES /iwbep/if_mgw_conv_srv_runtime.
    ALIASES copy_data_to_ref
      FOR /iwbep/if_mgw_conv_srv_runtime~copy_data_to_ref.
ENDCLASS.

CLASS /iwbep/cl_mgw_abs_data IMPLEMENTATION.
  METHOD /iwbep/if_mgw_conv_srv_runtime~copy_data_to_ref.
  ENDMETHOD.
ENDCLASS.";
        let parent_src = "\
CLASS /iwbep/cl_mgw_push_abs_data DEFINITION INHERITING FROM /iwbep/cl_mgw_abs_data.
ENDCLASS.";
        let main_src = "\
CLASS zcl_dpc DEFINITION INHERITING FROM /iwbep/cl_mgw_push_abs_data.
  PUBLIC SECTION.
    METHODS create_entity.
ENDCLASS.

CLASS zcl_dpc IMPLEMENTATION.
  METHOD create_entity.
    DATA lv_data TYPE string.
    copy_data_to_ref(
      EXPORTING is_data = lv_data
      CHANGING cr_data = lv_data ).
  ENDMETHOD.
ENDCLASS.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///if_mgw_conv_srv_runtime.abap"),
                version: 1,
                text: Arc::from(interface_src),
                is_dependency: true,
                object_name: Some(Arc::from("/iwbep/if_mgw_conv_srv_runtime")),
            },
            DocumentInput {
                uri: Arc::from("file:///cl_mgw_abs_data.abap"),
                version: 1,
                text: Arc::from(grandparent_src),
                is_dependency: true,
                object_name: Some(Arc::from("/iwbep/cl_mgw_abs_data")),
            },
            DocumentInput {
                uri: Arc::from("file:///cl_mgw_push_abs_data.abap"),
                version: 1,
                text: Arc::from(parent_src),
                is_dependency: true,
                object_name: Some(Arc::from("/iwbep/cl_mgw_push_abs_data")),
            },
            DocumentInput {
                uri: Arc::from("file:///zcl_dpc.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: Some(Arc::from("zcl_dpc")),
            },
        ]);
        let main = snapshots
            .get("file:///zcl_dpc.abap")
            .expect("main snapshot");
        let method_use = main_src.rfind("copy_data_to_ref").expect("method use") + 1;

        let hover = main
            .hovered_call_target_at(method_use)
            .expect("inherited alias hover");
        assert_eq!(hover.display_name.as_ref(), "copy_data_to_ref");

        let target = main
            .definition_at(method_use)
            .expect("inherited alias definition target");
        assert_target_slice(
            &target,
            "file:///if_mgw_conv_srv_runtime.abap",
            interface_src,
            "copy_data_to_ref",
        );
    }

    #[test]
    fn inherited_redefinition_method_body_uses_parent_parameters_without_unknown_symbol() {
        let store = DocumentStore::default();
        let super_src = "\
CLASS super DEFINITION.
  PUBLIC SECTION.
    METHODS resend_notification_generic
      EXPORTING ev_resnd_err TYPE abap_bool.
ENDCLASS.

CLASS super IMPLEMENTATION.
  METHOD resend_notification_generic.
  ENDMETHOD.
ENDCLASS.";
        let sub_src = "\
CLASS sub DEFINITION INHERITING FROM super.
  PUBLIC SECTION.
    METHODS resend_notification_generic REDEFINITION.
ENDCLASS.

CLASS sub IMPLEMENTATION.
  METHOD resend_notification_generic.
    ev_resnd_err = abap_true.
  ENDMETHOD.
ENDCLASS.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///super.abap"),
                version: 1,
                text: Arc::from(super_src),
                is_dependency: true,
                object_name: Some(Arc::from("super")),
            },
            DocumentInput {
                uri: Arc::from("file:///sub.abap"),
                version: 1,
                text: Arc::from(sub_src),
                is_dependency: false,
                object_name: Some(Arc::from("sub")),
            },
        ]);
        let snapshot = snapshots.get("file:///sub.abap").expect("sub snapshot");
        assert!(
            snapshot
                .symbols
                .diagnostics
                .iter()
                .all(|diag| !diag.message.contains("unknown symbol 'ev_resnd_err'")),
            "{:?}",
            snapshot.symbols.diagnostics
        );
    }

    #[test]
    fn definition_at_resolves_inherited_redefinition_parameter_to_parent_signature() {
        let store = DocumentStore::default();
        let super_src = "\
CLASS super DEFINITION.
  PUBLIC SECTION.
    METHODS resend_notification_generic
      EXPORTING ev_resnd_err TYPE abap_bool.
ENDCLASS.

CLASS super IMPLEMENTATION.
  METHOD resend_notification_generic.
  ENDMETHOD.
ENDCLASS.";
        let sub_src = "\
CLASS sub DEFINITION INHERITING FROM super.
  PUBLIC SECTION.
    METHODS resend_notification_generic REDEFINITION.
ENDCLASS.

CLASS sub IMPLEMENTATION.
  METHOD resend_notification_generic.
    ev_resnd_err = abap_true.
  ENDMETHOD.
ENDCLASS.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///super.abap"),
                version: 1,
                text: Arc::from(super_src),
                is_dependency: true,
                object_name: Some(Arc::from("super")),
            },
            DocumentInput {
                uri: Arc::from("file:///sub.abap"),
                version: 1,
                text: Arc::from(sub_src),
                is_dependency: false,
                object_name: Some(Arc::from("sub")),
            },
        ]);
        let snapshot = snapshots.get("file:///sub.abap").expect("sub snapshot");
        let parameter_use = sub_src.rfind("ev_resnd_err").expect("parameter use") + 1;
        let target = snapshot
            .definition_at(parameter_use)
            .expect("parameter definition target");
        assert_target_slice(&target, "file:///super.abap", super_src, "ev_resnd_err");
    }

    #[test]
    fn definition_at_resolves_named_argument_of_inherited_redefinition_to_parent_signature() {
        let store = DocumentStore::default();
        let super_src = "\
CLASS super DEFINITION.
  PUBLIC SECTION.
    METHODS set_processing_data
      IMPORTING
        iv_evtid TYPE i
        is_rule_key TYPE i.
ENDCLASS.

CLASS super IMPLEMENTATION.
  METHOD set_processing_data.
  ENDMETHOD.
ENDCLASS.";
        let sub_src = "\
CLASS sub DEFINITION INHERITING FROM super.
  PUBLIC SECTION.
    METHODS set_processing_data REDEFINITION.
    METHODS run.
ENDCLASS.

CLASS sub IMPLEMENTATION.
  METHOD set_processing_data.
  ENDMETHOD.

  METHOD run.
    me->set_processing_data(
      iv_evtid = 1
      is_rule_key = 2 ).
  ENDMETHOD.
ENDCLASS.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///super.abap"),
                version: 1,
                text: Arc::from(super_src),
                is_dependency: true,
                object_name: Some(Arc::from("super")),
            },
            DocumentInput {
                uri: Arc::from("file:///sub.abap"),
                version: 1,
                text: Arc::from(sub_src),
                is_dependency: false,
                object_name: Some(Arc::from("sub")),
            },
        ]);
        let snapshot = snapshots.get("file:///sub.abap").expect("sub snapshot");
        let parameter_use = sub_src.find("iv_evtid = 1").expect("named argument use") + 1;
        let target = snapshot
            .definition_at(parameter_use)
            .expect("parameter definition target");
        assert_target_slice(&target, "file:///super.abap", super_src, "iv_evtid");
    }

    #[test]
    fn definition_at_returns_interface_method_declaration_for_alias_target() {
        let store = DocumentStore::default();
        let src = "\
INTERFACE i1.
  METHODS meth.
ENDINTERFACE.

INTERFACE i2.
  ALIASES m1 FOR i1~meth.
ENDINTERFACE.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let method_use = src.rfind("meth").expect("alias target method use");

        let target = snapshot
            .definition_at(method_use + 1)
            .expect("interface method definition target");
        assert_target_slice(&target, "file:///demo.abap", src, "meth");
        assert_eq!(
            target.range.start,
            src.find("meth").expect("interface method declaration")
        );
    }

    #[test]
    fn definition_at_switches_between_class_method_declaration_and_implementation() {
        let store = DocumentStore::default();
        let src = "\
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    METHODS exec.
ENDCLASS.

CLASS some_class IMPLEMENTATION.
  METHOD exec.
  ENDMETHOD.
ENDCLASS.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let declaration_offset = src.find("exec").expect("method declaration");
        let implementation_offset = src.rfind("exec").expect("method implementation");

        let implementation_target = snapshot
            .definition_at(declaration_offset + 1)
            .expect("implementation target");
        assert_target_slice(&implementation_target, "file:///demo.abap", src, "exec");
        assert_eq!(implementation_target.range.start, implementation_offset);

        let declaration_target = snapshot
            .definition_at(implementation_offset + 1)
            .expect("declaration target");
        assert_target_slice(&declaration_target, "file:///demo.abap", src, "exec");
        assert_eq!(declaration_target.range.start, declaration_offset);
    }

    #[test]
    fn definition_at_links_class_method_declaration_and_implementation_across_report_includes() {
        let store = DocumentStore::default();
        let main_src = "\
REPORT zmain.
INCLUDE: ztop,
         zcls.";
        let top_src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS exec.
ENDCLASS.";
        let cls_src = "\
CLASS lcl_demo IMPLEMENTATION.
  METHOD exec.
  ENDMETHOD.
ENDCLASS.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///zmain.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///ztop.abap"),
                version: 1,
                text: Arc::from(top_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///zcls.abap"),
                version: 1,
                text: Arc::from(cls_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let top = snapshots.get("file:///ztop.abap").expect("top snapshot");
        let cls = snapshots.get("file:///zcls.abap").expect("class snapshot");
        let declaration_offset = top_src.find("exec").expect("method declaration");
        let implementation_offset = cls_src.rfind("exec").expect("method implementation");

        let implementation_target = top
            .definition_at(declaration_offset + 1)
            .expect("implementation target");
        assert_target_slice(&implementation_target, "file:///zcls.abap", cls_src, "exec");
        assert_eq!(implementation_target.range.start, implementation_offset);

        let declaration_target = cls
            .definition_at(implementation_offset + 1)
            .expect("declaration target");
        assert_target_slice(&declaration_target, "file:///ztop.abap", top_src, "exec");
        assert_eq!(declaration_target.range.start, declaration_offset);
    }

    #[test]
    fn hover_formats_implicit_method_call_signature_from_definition_include() {
        let store = DocumentStore::default();
        let main_src = "\
REPORT zmain.
INCLUDE: ztop,
         zf01.";
        let top_src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
  PROTECTED SECTION.
    METHODS status_from_rep_evt_status
      IMPORTING iv_status_rep_evt TYPE i
      RETURNING VALUE(rv_status) TYPE string.
ENDCLASS.";
        let f01_src = "\
CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
    DATA lv_status TYPE string.
    lv_status = status_from_rep_evt_status( 1 ).
  ENDMETHOD.

  METHOD status_from_rep_evt_status.
  ENDMETHOD.
ENDCLASS.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///zmain.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///ztop.abap"),
                version: 1,
                text: Arc::from(top_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///zf01.abap"),
                version: 1,
                text: Arc::from(f01_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let f01 = snapshots.get("file:///zf01.abap").expect("f01 snapshot");
        let offset = f01_src
            .find("status_from_rep_evt_status(")
            .expect("method call")
            + 1;

        let hovered = f01
            .hovered_call_target_at(offset)
            .expect("method signature hover");

        assert_eq!(hovered.display_name.as_ref(), "status_from_rep_evt_status");
        let signature = hovered
            .markdown_lines
            .iter()
            .find(|line| line.contains("status_from_rep_evt_status"))
            .expect("signature markdown");
        assert!(signature.contains("IMPORTING"));
        assert!(signature.contains("iv_status_rep_evt TYPE i"));
        assert!(signature.contains("RETURNING"));
        assert!(signature.contains("VALUE(rv_status) TYPE string"));
    }

    #[test]
    fn hover_completion_and_diagnostics_use_class_local_loop_row_type_from_top_include() {
        let store = DocumentStore::default();
        let main_src = "\
REPORT zmain.
INCLUDE: ztop,
         zf01.";
        let top_src = "\
CLASS lcl_app DEFINITION.
  PROTECTED SECTION.
    TYPES: BEGIN OF ty_object_info,
             evtid TYPE string,
             status_rep_evt TYPE i,
           END OF ty_object_info.
    TYPES tt_object_info TYPE STANDARD TABLE OF ty_object_info.
    DATA mt_object_info TYPE tt_object_info.
    METHODS run.
ENDCLASS.";
        let f01_src = "\
CLASS lcl_app IMPLEMENTATION.
  METHOD run.
    LOOP AT mt_object_info INTO DATA(ls_obj_info).
      DATA(lv_evtid) = ls_obj_info-evtid.
      DATA(lv_bad) = ls_obj_info-missing.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///zmain.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///ztop.abap"),
                version: 1,
                text: Arc::from(top_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///zf01.abap"),
                version: 1,
                text: Arc::from(f01_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let f01 = snapshots.get("file:///zf01.abap").expect("f01 snapshot");
        let selector_offset =
            f01_src.find("ls_obj_info-evtid").expect("selector") + "ls_obj_info-".len();

        let completion = f01
            .selector_completion_at(selector_offset)
            .expect("selector completion");
        assert_eq!(
            completion
                .items
                .iter()
                .map(|item| item.name.as_ref())
                .collect::<Vec<_>>(),
            vec!["evtid", "status_rep_evt"]
        );

        let hovered = f01
            .hovered_component_at(selector_offset + 1)
            .expect("field hover");
        assert_eq!(hovered.field_name.as_ref(), "evtid");
        assert_eq!(hovered.declared_type.as_deref(), Some("TYPE string"));
        assert!(f01.symbols.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnknownField && diag.message.contains("missing")
        }));
    }

    #[test]
    fn hover_resolves_ddic_structure_field_from_top_include_attribute() {
        let store = DocumentStore::default();
        let main_src = "\
REPORT zmain.
INCLUDE: ztop,
         zf01.";
        let top_src = "\
CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS display_alv.
  PRIVATE SECTION.
    DATA ms_layout TYPE lvc_s_layo.
ENDCLASS.";
        let f01_src = "\
CLASS lcl_app IMPLEMENTATION.
  METHOD display_alv.
    ms_layout-zebra = abap_true.
  ENDMETHOD.
ENDCLASS.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///zmain.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///ztop.abap"),
                version: 1,
                text: Arc::from(top_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///zf01.abap"),
                version: 1,
                text: Arc::from(f01_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///deps/lvc_s_layo.abap"),
                version: 1,
                text: Arc::from(
                    "TYPES: BEGIN OF lvc_s_layo,\n  zebra TYPE lvc_zebra,\nEND OF lvc_s_layo.",
                ),
                is_dependency: true,
                object_name: Some(Arc::from("lvc_s_layo")),
            },
        ]);
        let f01 = snapshots.get("file:///zf01.abap").expect("f01 snapshot");
        let offset = f01_src.find("zebra").expect("zebra") + 1;

        let hovered = f01.hovered_component_at(offset).expect("zebra hover");

        assert_eq!(hovered.field_name.as_ref(), "zebra");
        assert_eq!(hovered.declared_type.as_deref(), Some("TYPE lvc_zebra"));
    }

    #[test]
    fn definition_at_returns_class_attribute_declaration_from_definition_include() {
        let store = DocumentStore::default();
        let main_src = "\
REPORT zmain.
INCLUDE: ztop,
         zcls.";
        let top_src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    DATA: lv_jobname  TYPE string,
          lv_jobcount TYPE string.
    METHODS get_data.
ENDCLASS.";
        let cls_src = "\
CLASS lcl_demo IMPLEMENTATION.
  METHOD get_data.
    lv_jobname = 'demo'.
    IF lv_jobcount IS INITIAL.
      lv_jobcount = lv_jobname.
    ENDIF.
  ENDMETHOD.
ENDCLASS.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///zmain.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///ztop.abap"),
                version: 1,
                text: Arc::from(top_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///zcls.abap"),
                version: 1,
                text: Arc::from(cls_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let cls = snapshots.get("file:///zcls.abap").expect("class snapshot");
        let jobname_use = cls_src.find("lv_jobname =").expect("lv_jobname use");
        let jobcount_use = cls_src.find("lv_jobcount IS").expect("lv_jobcount use");

        let jobname_target = cls
            .definition_at(jobname_use + 1)
            .expect("lv_jobname definition");
        assert_target_slice(&jobname_target, "file:///ztop.abap", top_src, "lv_jobname");

        let jobcount_target = cls
            .definition_at(jobcount_use + 1)
            .expect("lv_jobcount definition");
        assert_target_slice(
            &jobcount_target,
            "file:///ztop.abap",
            top_src,
            "lv_jobcount",
        );
    }

    #[test]
    fn definition_at_returns_structure_field_declaration() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF ty_inner,
         alpha TYPE i,
       END OF ty_inner.
TYPES: BEGIN OF ty_outer,
         inner TYPE ty_inner,
       END OF ty_outer.
DATA ls_outer TYPE ty_outer.
ls_outer-inner-alpha = 1.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let field_use = src.rfind("alpha").expect("field use");

        let target = snapshot
            .definition_at(field_use + 1)
            .expect("definition target");
        assert_target_slice(&target, "file:///demo.abap", src, "alpha");
        assert_eq!(
            target.range.start,
            src.find("alpha").expect("field declaration")
        );
    }

    #[test]
    fn hover_and_definition_work_for_structure_field_under_class_member_selector() {
        let store = DocumentStore::default();
        let src = "\
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS get_value RETURNING VALUE(rv_value) TYPE i.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF gcs_struct_field,
        p0 TYPE i VALUE 1,
        p1 TYPE i VALUE 2,
      END OF gcs_struct_field.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD get_value.
    rv_value = me->gcs_struct_field-p0.
  ENDMETHOD.
ENDCLASS.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let field_use = src.rfind("p0").expect("field use");

        let hovered = snapshot
            .hovered_component_at(field_use + 1)
            .expect("hovered component");
        assert_eq!(hovered.base_name.as_ref(), "me");
        assert_eq!(
            hovered
                .component_path
                .iter()
                .map(|part| part.as_ref())
                .collect::<Vec<_>>(),
            vec!["gcs_struct_field", "p0"]
        );
        assert_eq!(hovered.field_name.as_ref(), "p0");
        assert_eq!(hovered.declared_type.as_deref(), Some("TYPE i"));
        assert!(matches!(hovered.kind, HoveredComponentKind::Scalar));

        let target = snapshot
            .definition_at(field_use + 1)
            .expect("definition target");
        assert_target_slice(&target, "file:///demo.abap", src, "p0");
        assert_eq!(
            target.range.start,
            src.find("p0 TYPE i").expect("field declaration")
        );
    }

    #[test]
    fn hover_and_definition_work_for_static_class_type_selector() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_repro DEFINITION.
  PUBLIC SECTION.
    TYPES tr_errors TYPE RANGE OF string.
ENDCLASS.

DATA lt_data TYPE lcl_repro=>tr_errors.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let type_use = src.rfind("tr_errors").expect("type use");

        let hovered = snapshot
            .hovered_component_at(type_use + 1)
            .expect("hovered component");
        assert_eq!(hovered.base_name.as_ref(), "lcl_repro");
        assert_eq!(
            hovered
                .component_path
                .iter()
                .map(|part| part.as_ref())
                .collect::<Vec<_>>(),
            vec!["tr_errors"]
        );
        assert_eq!(hovered.field_name.as_ref(), "tr_errors");
        assert_eq!(
            hovered.declared_type.as_deref(),
            Some("TYPE RANGE OF string")
        );
        assert_eq!(
            hovered.declaration.as_deref(),
            Some("TYPES tr_errors TYPE RANGE OF string.")
        );
        assert!(matches!(hovered.kind, HoveredComponentKind::Type));

        let target = snapshot
            .definition_at(type_use + 1)
            .expect("definition target");
        assert_target_slice(&target, "file:///demo.abap", src, "tr_errors");
        assert_eq!(
            target.range.start,
            src.find("tr_errors TYPE RANGE OF string")
                .expect("type declaration")
        );
    }

    #[test]
    fn hover_and_definition_work_for_value_constructor_named_field() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF ty_selopt,
         sign TYPE c LENGTH 1,
         option TYPE c LENGTH 2,
       END OF ty_selopt.
DATA(ls_selopt) = VALUE ty_selopt(
  sign = 'I'
  option = 'EQ' ).";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let field_use = src.rfind("option").expect("value field use");

        let hovered = snapshot
            .hovered_component_at(field_use + 1)
            .expect("hovered value-constructor field");
        assert_eq!(hovered.base_name.as_ref(), "ty_selopt");
        assert_eq!(
            hovered
                .component_path
                .iter()
                .map(|part| part.as_ref())
                .collect::<Vec<_>>(),
            vec!["option"]
        );
        assert_eq!(hovered.field_name.as_ref(), "option");
        assert_eq!(hovered.declared_type.as_deref(), Some("TYPE c"));
        assert!(matches!(hovered.kind, HoveredComponentKind::Scalar));

        let target = snapshot
            .definition_at(field_use + 1)
            .expect("definition target");
        assert_target_slice(&target, "file:///demo.abap", src, "option");
        assert_eq!(
            target.range.start,
            src.find("option TYPE c").expect("field declaration")
        );
    }

    #[test]
    fn definition_at_returns_named_argument_parameter_declaration() {
        let store = DocumentStore::default();
        let src = "\
CLASS zcl_program DEFINITION.
  PUBLIC SECTION.
    METHODS add_statement
      IMPORTING io_stmt TYPE string.
ENDCLASS.

CLASS zcl_program IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA(lo_prog) = NEW zcl_program( ).
  lo_prog->add_statement( io_stmt = 'x' ).";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let parameter_use = src.rfind("io_stmt").expect("named argument use");

        let target = snapshot
            .definition_at(parameter_use + 1)
            .expect("definition target");
        assert_target_slice(&target, "file:///demo.abap", src, "io_stmt");
        assert_eq!(
            target.range.start,
            src.find("io_stmt").expect("parameter declaration")
        );
    }

    #[test]
    fn definition_at_returns_variable_declaration_for_perform_argument() {
        let store = DocumentStore::default();
        let src = "\
FORM f USING VALUE(iv_input) TYPE i CHANGING cv_text TYPE string.
  cv_text = |{ iv_input }|.
ENDFORM.

START-OF-SELECTION.
  DATA lv_input TYPE i VALUE 1.
  DATA lv_text TYPE string.
  PERFORM f USING lv_input CHANGING lv_text.
";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let argument_use = src.rfind("lv_input").expect("perform argument use");

        let target = snapshot
            .definition_at(argument_use + 1)
            .expect("definition target");
        assert_target_slice(&target, "file:///demo.abap", src, "lv_input");
        assert_eq!(
            target.range.start,
            src.find("lv_input").expect("variable declaration")
        );
    }

    #[test]
    fn definition_and_inlay_hints_resolve_static_perform_in_program_target() {
        let store = DocumentStore::default();
        let callee_src = "\
REPORT zcallee.
FORM process_data USING pv_mode TYPE string.
ENDFORM.
";
        let caller_src = "\
REPORT zcaller.
DATA lv_mode TYPE string.
PERFORM process_data IN PROGRAM zcallee IF FOUND USING lv_mode.
";
        let snapshots = store.publish_inputs(vec![
            DocumentInput {
                uri: Arc::from("file:///zcallee.abap"),
                version: 1,
                text: Arc::from(callee_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///zcaller.abap"),
                version: 1,
                text: Arc::from(caller_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots
            .get("file:///zcaller.abap")
            .expect("caller snapshot");
        let form_use = caller_src.rfind("process_data").expect("perform target");

        let target = snapshot
            .definition_at(form_use + 1)
            .expect("perform target definition");
        assert_target_slice(&target, "file:///zcallee.abap", callee_src, "process_data");
        assert_eq!(
            target.range.start,
            callee_src.find("process_data").expect("callee form")
        );

        let hints = snapshot.perform_parameter_inlay_hints_in_range(0..caller_src.len());
        assert!(hints.iter().any(|hint| {
            hint.label.as_ref() == "pv_mode"
                && hint.position == caller_src.rfind("lv_mode").expect("perform argument")
        }));
    }

    #[test]
    fn definition_at_returns_variable_declaration_for_chained_perform_argument() {
        let store = DocumentStore::default();
        let src = "\
FORM append_fldcat1
    USING pv_field TYPE string
          pv_len TYPE i
          pv_text TYPE string
          pv_flag TYPE c.
ENDFORM.

START-OF-SELECTION.
  DATA lv_flag1 TYPE c.
  DATA lv_flag2 TYPE c.
  PERFORM append_fldcat1 USING:
    'MATNR' 18 'Material' lv_flag1,
    'MAKTX' 40 'Description' lv_flag2.
";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let argument_use = src.rfind("lv_flag2").expect("perform argument use");

        let target = snapshot
            .definition_at(argument_use + 1)
            .expect("definition target");
        assert_target_slice(&target, "file:///demo.abap", src, "lv_flag2");
        assert_eq!(
            target.range.start,
            src.find("lv_flag2 TYPE c").expect("variable declaration")
        );
    }

    #[test]
    fn definition_at_returns_none_for_builtin_type() {
        let store = DocumentStore::default();
        let src = "DATA text TYPE string.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.find("string").expect("builtin type") + 1;

        assert!(snapshot.definition_at(offset).is_none());
    }

    #[test]
    fn definition_at_for_include_statement_opens_fetched_include_file() {
        let store = DocumentStore::default();
        let main_src = "INCLUDE /sttp/int_global.\nlv_inc = 1.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from(
                    "file:///d:/dev/abap/lsp_development_examples/.abapls/cache/dependencies/include/%2FSTTP%2FINT_GLOBAL.abap",
                ),
                version: 1,
                text: Arc::from("DATA lv_inc TYPE i."),
                is_dependency: true,
                object_name: Some(Arc::from("/sttp/int_global")),
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");
        let offset = main_src.find("/sttp/int_global").expect("include name") + 1;

        let target = snapshot.definition_at(offset).expect("definition target");
        assert_eq!(
            target.uri.as_ref(),
            "file:///d:/dev/abap/lsp_development_examples/.abapls/cache/dependencies/include/%2FSTTP%2FINT_GLOBAL.abap"
        );
        assert_eq!(target.range, 0..0);
    }

    #[test]
    fn definition_at_resolves_underlying_type_in_table_type_clause() {
        let store = DocumentStore::default();
        let src = "\
CLASS zcl_stmt DEFINITION.
ENDCLASS.

CLASS zcl_stmt IMPLEMENTATION.
ENDCLASS.

CLASS zcl_program DEFINITION.
  PUBLIC SECTION.
    TYPES ty_stmt_tab TYPE STANDARD TABLE OF REF TO zcl_stmt WITH DEFAULT KEY.
ENDCLASS.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let type_use = src.rfind("zcl_stmt").expect("wrapped type use");

        let target = snapshot
            .definition_at(type_use + 1)
            .expect("definition target");
        assert_target_slice(&target, "file:///demo.abap", src, "zcl_stmt");
        assert_eq!(
            target.range.start,
            src.find("zcl_stmt").expect("class declaration")
        );
    }

    #[test]
    fn lists_selector_completion_items_for_partial_component() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF ty_inner,
         alpha TYPE i,
         amount TYPE i,
       END OF ty_inner.
TYPES: BEGIN OF ty_outer,
         inner TYPE ty_inner,
       END OF ty_outer.
DATA ls_outer TYPE ty_outer.
ls_outer-inner-a";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.len();

        let completion = snapshot
            .selector_completion_at(offset)
            .expect("selector completion");
        assert_eq!(
            completion
                .items
                .iter()
                .map(|item| item.name.as_ref())
                .collect::<Vec<_>>(),
            vec!["alpha", "amount"]
        );
        assert_eq!(&src[completion.replace_range], "a");
    }

    #[test]
    fn lists_selector_completion_items_after_trailing_dash() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF ty_inner,
         alpha TYPE i,
       END OF ty_inner.
TYPES: BEGIN OF ty_outer,
         inner TYPE ty_inner,
       END OF ty_outer.
DATA ls_outer TYPE ty_outer.
ls_outer-inner-";
        let snapshot = store.publish("file:///demo.abap", 1, src);

        let completion = snapshot
            .selector_completion_at(src.len())
            .expect("selector completion");
        assert_eq!(completion.items.len(), 1);
        assert_eq!(completion.items[0].name.as_ref(), "alpha");
        assert!(completion.replace_range.is_empty());
    }

    #[test]
    fn lists_selector_completion_items_after_class_member_structure_dash() {
        let store = DocumentStore::default();
        let src = "\
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS get_value RETURNING VALUE(rv_value) TYPE i.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF gcs_struct_field,
        p0 TYPE i VALUE 1,
        p1 TYPE i VALUE 2,
      END OF gcs_struct_field.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD get_value.
    rv_value = me->gcs_struct_field-.
  ENDMETHOD.
ENDCLASS.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset =
            src.find("me->gcs_struct_field-").expect("selector") + "me->gcs_struct_field-".len();

        let completion = snapshot
            .selector_completion_at(offset)
            .expect("selector completion");
        assert_eq!(
            completion
                .items
                .iter()
                .map(|item| item.name.as_ref())
                .collect::<Vec<_>>(),
            vec!["p0", "p1"]
        );
        assert!(completion.replace_range.is_empty());
    }

    #[test]
    fn lists_field_symbol_components_from_fetched_table_line_dependency() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS display_alv.
  PRIVATE SECTION.
    DATA mt_fieldcat TYPE lvc_t_fcat.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD display_alv.
    APPEND INITIAL LINE TO mt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    <fs_fcat>-fieldname1 = 'DOCNUM'.
    <fs_fcat>-
  ENDMETHOD.
ENDCLASS.";
        let snapshots = store.publish_inputs(vec![
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///deps/lvc_t_fcat.abap"),
                version: 1,
                text: Arc::from(
                    "TYPES lvc_t_fcat TYPE STANDARD TABLE OF lvc_s_fcat WITH EMPTY KEY.",
                ),
                is_dependency: true,
                object_name: Some(Arc::from("lvc_t_fcat")),
            },
            DocumentInput {
                uri: Arc::from("file:///deps/lvc_s_fcat.abap"),
                version: 1,
                text: Arc::from(
                    "TYPES: BEGIN OF lvc_s_fcat,\n  fieldname TYPE string,\n  coltext TYPE string,\nEND OF lvc_s_fcat.",
                ),
                is_dependency: true,
                object_name: Some(Arc::from("lvc_s_fcat")),
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");
        let offset = src.find("<fs_fcat>-").expect("selector") + "<fs_fcat>-".len();

        let completion = snapshot
            .selector_completion_at(offset)
            .expect("field-symbol selector completion");

        assert_eq!(
            completion
                .items
                .iter()
                .map(|item| item.name.as_ref())
                .collect::<Vec<_>>(),
            vec!["coltext", "fieldname"]
        );
        assert!(snapshot.symbols.diagnostics.iter().any(|diag| {
            diag.kind == DiagnosticKind::UnknownField && diag.message.contains("fieldname1")
        }));
    }

    #[test]
    fn lists_public_static_methods_after_fat_arrow() {
        let store = DocumentStore::default();
        let src = "\
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS exec.
    CLASS-METHODS expose.
  PRIVATE SECTION.
    CLASS-METHODS hidden.
ENDCLASS.

some_class=>e";
        let snapshot = store.publish("file:///demo.abap", 1, src);

        let completion = snapshot
            .selector_completion_at(src.len())
            .expect("selector completion");
        assert_eq!(
            completion
                .items
                .iter()
                .map(|item| item.name.as_ref())
                .collect::<Vec<_>>(),
            vec!["exec", "expose"]
        );
        assert!(
            completion
                .items
                .iter()
                .all(|item| matches!(item.kind, HoveredComponentKind::Method))
        );
        assert!(completion.items.iter().all(|item| {
            item.declaration
                .as_deref()
                .is_some_and(|decl| decl.contains("CLASS-METHODS"))
        }));
    }

    #[test]
    fn lists_public_static_grouped_constants_after_bare_fat_arrow() {
        let store = DocumentStore::default();
        let src = "\
CLASS z_demo DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gcs_aif_ifname,
        BEGIN OF europe,
          aggregation_epa_32    TYPE string  VALUE 'ZEU_EPA_32' ##no_text,
          dispatch_edp_33       TYPE string  VALUE 'ZEU_EDP_33' ##no_text,
        END OF europe,
      END OF gcs_aif_ifname.
ENDCLASS.

z_demo=>";
        let snapshot = store.publish("file:///demo.abap", 1, src);

        let completion = snapshot
            .selector_completion_at(src.len())
            .expect("selector completion");
        assert_eq!(
            completion
                .items
                .iter()
                .map(|item| item.name.as_ref())
                .collect::<Vec<_>>(),
            vec!["gcs_aif_ifname"]
        );
        assert!(completion.replace_range.is_empty());
        assert!(matches!(
            completion.items[0].kind,
            HoveredComponentKind::Attribute
        ));
        assert_eq!(
            completion.items[0].insertion.plain_text.as_str(),
            "gcs_aif_ifname"
        );
        assert!(
            completion.items[0]
                .declaration
                .as_deref()
                .is_some_and(|decl| decl.contains("BEGIN OF gcs_aif_ifname"))
        );
    }

    #[test]
    fn lists_public_class_types_after_fat_arrow_in_type_position() {
        let store = DocumentStore::default();
        let src = "\
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    TYPES tr_errors TYPE RANGE OF string.
    TYPES ty_output TYPE string.
    CLASS-METHODS exec.
ENDCLASS.

DATA lt_data TYPE some_class=>t";
        let snapshot = store.publish("file:///demo.abap", 1, src);

        let completion = snapshot
            .selector_completion_at(src.len())
            .expect("selector completion");
        assert!(completion.in_type_position);
        assert_eq!(
            completion
                .items
                .iter()
                .map(|item| item.name.as_ref())
                .collect::<Vec<_>>(),
            vec!["tr_errors", "ty_output"]
        );
        assert!(
            completion
                .items
                .iter()
                .all(|item| matches!(item.kind, HoveredComponentKind::Type))
        );
        assert_eq!(
            completion.items[0].declared_type.as_deref(),
            Some("TYPE RANGE OF string")
        );
        assert_eq!(
            completion.items[0].declaration.as_deref(),
            Some("TYPES tr_errors TYPE RANGE OF string.")
        );
        assert!(
            completion
                .items
                .iter()
                .all(|item| item.name.as_ref() != "exec"),
            "unexpected method completion in type position: {:?}",
            completion
                .items
                .iter()
                .map(|item| item.name.as_ref())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn lists_public_class_types_after_bare_fat_arrow_in_type_position() {
        let store = DocumentStore::default();
        let src = "\
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    TYPES tr_errors TYPE RANGE OF string.
    TYPES ty_output TYPE string.
ENDCLASS.

DATA lt_data TYPE some_class=>";
        let snapshot = store.publish("file:///demo.abap", 1, src);

        let query = snapshot
            .selector_completion_query_at(src.len())
            .expect("selector query");
        assert_eq!(query.base_name.as_ref(), "some_class");
        assert_eq!(query.base_namespace, Namespace::Type);
        assert!(query.component_path.is_empty());
        assert!(query.in_type_position);
        assert_eq!(query.prefix.as_ref(), "");

        let completion = snapshot
            .selector_completion_at(src.len())
            .expect("selector completion");
        assert!(completion.in_type_position);
        assert_eq!(
            completion
                .items
                .iter()
                .map(|item| item.name.as_ref())
                .collect::<Vec<_>>(),
            vec!["tr_errors", "ty_output"]
        );
    }

    #[test]
    fn lists_selector_completion_items_after_class_type_structure_dash_in_type_position() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_outer,
             low TYPE i,
             high TYPE i,
           END OF ty_outer.
ENDCLASS.

DATA lr_data TYPE lcl_demo=>ty_outer-l";
        let snapshot = store.publish("file:///demo.abap", 1, src);

        let completion = snapshot
            .selector_completion_at(src.len())
            .expect("selector completion");
        assert!(completion.in_type_position);
        assert_eq!(
            completion
                .items
                .iter()
                .map(|item| item.name.as_ref())
                .collect::<Vec<_>>(),
            vec!["low"]
        );
        assert_eq!(completion.items[0].declared_type.as_deref(), Some("TYPE i"));
    }

    #[test]
    fn completes_inherited_parameter_typed_as_interface_scoped_structure() {
        let store = DocumentStore::default();
        let runtime_src = "\
INTERFACE /iwbep/if_mgw_appl_srv_runtime.
  TYPES: BEGIN OF ty_s_mgw_response_context,
           inlinecount TYPE i,
           count TYPE i,
         END OF ty_s_mgw_response_context.
ENDINTERFACE.";
        let super_src = "\
CLASS zcl_dpc DEFINITION.
  PROTECTED SECTION.
    METHODS prodset_get_entityset
      EXPORTING
        es_response_context TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_context.
ENDCLASS.

CLASS zcl_dpc IMPLEMENTATION.
  METHOD prodset_get_entityset.
  ENDMETHOD.
ENDCLASS.";
        let sub_src = "\
CLASS zcl_dpc_ext DEFINITION INHERITING FROM zcl_dpc.
  PROTECTED SECTION.
    METHODS prodset_get_entityset REDEFINITION.
ENDCLASS.

CLASS zcl_dpc_ext IMPLEMENTATION.
  METHOD prodset_get_entityset.
    es_response_context-
  ENDMETHOD.
ENDCLASS.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///runtime.abap"),
                version: 1,
                text: Arc::from(runtime_src),
                is_dependency: true,
                object_name: Some(Arc::from("/iwbep/if_mgw_appl_srv_runtime")),
            },
            DocumentInput {
                uri: Arc::from("file:///super.abap"),
                version: 1,
                text: Arc::from(super_src),
                is_dependency: true,
                object_name: Some(Arc::from("zcl_dpc")),
            },
            DocumentInput {
                uri: Arc::from("file:///sub.abap"),
                version: 1,
                text: Arc::from(sub_src),
                is_dependency: false,
                object_name: Some(Arc::from("zcl_dpc_ext")),
            },
        ]);
        let snapshot = snapshots.get("file:///sub.abap").expect("sub snapshot");
        let offset =
            sub_src.find("es_response_context-").expect("selector") + "es_response_context-".len();

        let completion = snapshot
            .selector_completion_at(offset)
            .expect("selector completion");

        assert_eq!(
            completion
                .items
                .iter()
                .map(|item| item.name.as_ref())
                .collect::<Vec<_>>(),
            vec!["count", "inlinecount"]
        );
        assert!(completion.replace_range.is_empty());
    }

    #[test]
    fn lists_selector_completion_items_with_whitespace_after_operator() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF ty_inner,
         alpha TYPE i,
       END OF ty_inner.
TYPES: BEGIN OF ty_outer,
         inner TYPE ty_inner,
       END OF ty_outer.
DATA ls_outer TYPE ty_outer.
ls_outer-inner-  a";
        let snapshot = store.publish("file:///demo.abap", 1, src);

        let completion = snapshot
            .selector_completion_at(src.len())
            .expect("selector completion");
        assert_eq!(completion.items.len(), 1);
        assert_eq!(completion.items[0].name.as_ref(), "alpha");
        assert_eq!(&src[completion.replace_range], "a");
    }

    #[test]
    fn lists_selector_completion_items_in_type_position() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF ty_inner,
         alpha TYPE i,
       END OF ty_inner.
TYPES: BEGIN OF ty_outer,
         inner TYPE ty_inner,
       END OF ty_outer.
DATA lv_value TYPE ty_outer-inner-";
        let snapshot = store.publish("file:///demo.abap", 1, src);

        let completion = snapshot
            .selector_completion_at(src.len())
            .expect("selector completion");
        assert!(completion.in_type_position);
        assert_eq!(completion.items.len(), 1);
        assert_eq!(completion.items[0].name.as_ref(), "alpha");
    }

    #[test]
    fn lists_selector_completion_items_after_table_expression() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF ty_rep,
         type TYPE string,
         tag  TYPE string,
       END OF ty_rep.
TYPES ty_rep_tab TYPE STANDARD TABLE OF ty_rep WITH EMPTY KEY.
DATA lt_rep TYPE ty_rep_tab.
lt_rep[ 1 ]-t";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let query = snapshot
            .selector_completion_query_at(src.len())
            .expect("selector query");
        assert_eq!(query.base_name.as_ref(), "lt_rep");
        assert!(query.component_path.is_empty());
        assert_eq!(query.prefix.as_ref(), "t");

        let completion = snapshot
            .selector_completion_at(src.len())
            .expect("selector completion");
        assert_eq!(
            completion
                .items
                .iter()
                .map(|item| item.name.as_ref())
                .collect::<Vec<_>>(),
            vec!["tag", "type"]
        );
        assert_eq!(&src[completion.replace_range], "t");
    }

    #[test]
    fn does_not_treat_legacy_table_body_operator_as_selector_completion() {
        let store = DocumentStore::default();
        let src = "\
DATA lt_tab TYPE STANDARD TABLE OF string WITH EMPTY KEY.
IF lt_tab[] IS NOT INITIAL.";
        let snapshot = store.publish("file:///demo.abap", 1, src);

        assert!(
            snapshot
                .selector_completion_at(src.find("[]").expect("legacy []") + 2)
                .is_none()
        );
    }

    #[test]
    fn does_not_treat_binary_minus_as_selector_completion() {
        let store = DocumentStore::default();
        let src = "DATA a TYPE i. DATA b TYPE i. a - b";
        let snapshot = store.publish("file:///demo.abap", 1, src);

        assert!(snapshot.selector_completion_at(src.len()).is_none());
    }

    #[test]
    fn lists_selector_completion_items_inside_template_expression() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF ty_inner,
         alpha TYPE i,
       END OF ty_inner.
TYPES: BEGIN OF ty_outer,
         inner TYPE ty_inner,
       END OF ty_outer.
DATA ls_outer TYPE ty_outer.
WRITE |TYPE { ls_outer-inner- }|.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.find("inner-").expect("selector") + "inner-".len();

        let completion = snapshot
            .selector_completion_at(offset)
            .expect("selector completion");
        assert!(!completion.in_type_position);
        assert_eq!(completion.items.len(), 1);
        assert_eq!(completion.items[0].name.as_ref(), "alpha");
    }

    #[test]
    fn finds_hovered_method_inside_assignment_template_expression() {
        let store = DocumentStore::default();
        let src = "\
CLASS zcl_expr DEFINITION.
  PUBLIC SECTION.
    METHODS to_string
      RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_expr IMPLEMENTATION.
  METHOD to_string.
    rv_text = 'expr'.
  ENDMETHOD.
ENDCLASS.

DATA lo_expr TYPE REF TO zcl_expr.
DATA rv_text TYPE string.
rv_text = |value: { lo_expr->to_string( ) }|.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.rfind("to_string").expect("method name") + 1;

        let hovered = snapshot
            .hovered_component_at(offset)
            .expect("hovered method info");
        assert_eq!(hovered.base_name.as_ref(), "lo_expr");
        assert_eq!(hovered.field_name.as_ref(), "to_string");
        assert!(matches!(hovered.kind, HoveredComponentKind::Method));
        assert!(
            hovered
                .declaration
                .as_deref()
                .is_some_and(|declaration| declaration.contains("METHODS to_string"))
        );
    }

    #[test]
    fn finds_hovered_inherited_method_inside_assignment_template_expression() {
        let store = DocumentStore::default();
        let src = "\
CLASS zcl_ast_node DEFINITION.
  PUBLIC SECTION.
    METHODS to_string
      RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_ast_node IMPLEMENTATION.
ENDCLASS.

CLASS zcl_expr DEFINITION INHERITING FROM zcl_ast_node.
ENDCLASS.

CLASS zcl_expr IMPLEMENTATION.
ENDCLASS.

DATA lo_expr TYPE REF TO zcl_expr.
DATA rv_text TYPE string.
rv_text = |value: { lo_expr->to_string( ) }|.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.rfind("to_string").expect("method name") + 1;

        let hovered = snapshot
            .hovered_component_at(offset)
            .expect("hovered method info");
        assert_eq!(hovered.base_name.as_ref(), "lo_expr");
        assert_eq!(hovered.field_name.as_ref(), "to_string");
        assert!(matches!(hovered.kind, HoveredComponentKind::Method));
        assert!(
            hovered
                .declaration
                .as_deref()
                .is_some_and(|declaration| declaration.contains("METHODS to_string"))
        );
    }

    #[test]
    fn hover_and_definition_work_for_zero_arg_selector_method_inherited_via_dependency_chain() {
        let store = DocumentStore::default();
        let base_messages_src = "\
CLASS /cdbasis/cl_messages DEFINITION.
  PUBLIC SECTION.
    METHODS set_message.
ENDCLASS.
CLASS /cdbasis/cl_messages IMPLEMENTATION.
ENDCLASS.";
        let sub_messages_src = "\
CLASS /sttp/cl_messages DEFINITION INHERITING FROM /cdbasis/cl_messages.
ENDCLASS.
CLASS /sttp/cl_messages IMPLEMENTATION.
ENDCLASS.";
        let base_inst_src = "\
CLASS /sttp/cl_base_inst DEFINITION.
  PUBLIC SECTION.
    DATA mo_messages TYPE REF TO /sttp/cl_messages.
ENDCLASS.
CLASS /sttp/cl_base_inst IMPLEMENTATION.
ENDCLASS.";
        let rep_base_src = "\
CLASS /sttp/cl_rep_base DEFINITION INHERITING FROM /sttp/cl_base_inst.
ENDCLASS.
CLASS /sttp/cl_rep_base IMPLEMENTATION.
ENDCLASS.";
        let rep_ru_src = "\
CLASS /sttp/cl_rep_ru DEFINITION INHERITING FROM /sttp/cl_rep_base.
ENDCLASS.
CLASS /sttp/cl_rep_ru IMPLEMENTATION.
ENDCLASS.";
        let main_src = "\
CLASS zcl_child DEFINITION INHERITING FROM /sttp/cl_rep_ru.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS zcl_child IMPLEMENTATION.
  METHOD run.
    mo_messages->set_message( ).
  ENDMETHOD.
ENDCLASS.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///deps/%2FCDBASIS%2FCL_MESSAGES.abap"),
                version: 1,
                text: Arc::from(base_messages_src),
                is_dependency: true,
                object_name: Some(Arc::from("/cdbasis/cl_messages")),
            },
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FCL_MESSAGES.abap"),
                version: 1,
                text: Arc::from(sub_messages_src),
                is_dependency: true,
                object_name: Some(Arc::from("/sttp/cl_messages")),
            },
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FCL_BASE_INST.abap"),
                version: 1,
                text: Arc::from(base_inst_src),
                is_dependency: true,
                object_name: Some(Arc::from("/sttp/cl_base_inst")),
            },
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FCL_REP_BASE.abap"),
                version: 1,
                text: Arc::from(rep_base_src),
                is_dependency: true,
                object_name: Some(Arc::from("/sttp/cl_rep_base")),
            },
            DocumentInput {
                uri: Arc::from("file:///deps/%2FSTTP%2FCL_REP_RU.abap"),
                version: 1,
                text: Arc::from(rep_ru_src),
                is_dependency: true,
                object_name: Some(Arc::from("/sttp/cl_rep_ru")),
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");
        let offset = main_src.rfind("set_message").expect("method use") + 1;

        let hovered = snapshot
            .hovered_call_target_at(offset)
            .expect("hovered method target");
        assert_eq!(hovered.display_name.as_ref(), "set_message");
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line.contains("METHODS set_message")),
            "{:?}",
            hovered.markdown_lines
        );

        let definition = snapshot.definition_at(offset).expect("definition target");
        assert_eq!(
            definition.uri.as_ref(),
            "file:///deps/%2FCDBASIS%2FCL_MESSAGES.abap"
        );
        assert_eq!(&base_messages_src[definition.range], "set_message");
    }

    #[test]
    fn finds_hovered_interface_method_for_interface_typed_value_selector() {
        let store = DocumentStore::default();
        let interface_src = "\
INTERFACE i1.
  METHODS meth
    IMPORTING
      iv_value TYPE i.
ENDINTERFACE.";
        let main_src = "\
CLASS demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS demo IMPLEMENTATION.
  METHOD run.
    DATA lo_obj TYPE REF TO i1.
    lo_obj->meth(
      iv_value = 1 ).
  ENDMETHOD.
ENDCLASS.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///i1.abap"),
                version: 1,
                text: Arc::from(interface_src),
                is_dependency: true,
                object_name: Some(Arc::from("i1")),
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots
            .get("file:///main.abap")
            .expect("main snapshot should exist");
        let offset = main_src.rfind("meth").expect("method use") + 1;

        let hovered = snapshot
            .hovered_component_at(offset)
            .expect("hovered method info");
        assert_eq!(hovered.base_name.as_ref(), "lo_obj");
        assert_eq!(hovered.field_name.as_ref(), "meth");
        assert!(matches!(hovered.kind, HoveredComponentKind::Method));
        assert!(
            hovered
                .declaration
                .as_deref()
                .is_some_and(|declaration| declaration.contains("METHODS meth"))
        );
        assert!(
            hovered
                .declaration
                .as_deref()
                .is_some_and(|declaration| declaration.contains("iv_value TYPE i"))
        );
    }

    #[test]
    fn inline_method_result_infers_returned_interface_for_hover_and_completion() {
        let store = DocumentStore::default();
        let filter_src = "\
INTERFACE /iwbep/if_mgw_req_filter.
  METHODS get_filter_select_options.
  METHODS get_filter_string
    RETURNING VALUE(rv_filter) TYPE string.
ENDINTERFACE.";
        let context_src = "\
INTERFACE /iwbep/if_mgw_req_entityset.
  METHODS get_filter
    RETURNING VALUE(ro_filter) TYPE REF TO /iwbep/if_mgw_req_filter.
ENDINTERFACE.";
        let main_src = "\
CLASS zcl_dpc_ext DEFINITION.
  PUBLIC SECTION.
    METHODS prodset_get_entityset
      IMPORTING
        io_tech_request_context TYPE REF TO /iwbep/if_mgw_req_entityset.
ENDCLASS.

CLASS zcl_dpc_ext IMPLEMENTATION.
  METHOD prodset_get_entityset.
    DATA(lo_filter) = io_tech_request_context->get_filter( ).
    lo_filter->
  ENDMETHOD.
ENDCLASS.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///deps/filter.abap"),
                version: 1,
                text: Arc::from(filter_src),
                is_dependency: true,
                object_name: Some(Arc::from("/iwbep/if_mgw_req_filter")),
            },
            DocumentInput {
                uri: Arc::from("file:///deps/context.abap"),
                version: 1,
                text: Arc::from(context_src),
                is_dependency: true,
                object_name: Some(Arc::from("/iwbep/if_mgw_req_entityset")),
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");
        let lo_filter = snapshot
            .symbols
            .symbols
            .iter()
            .find(|symbol| symbol.name.as_ref() == "lo_filter")
            .expect("inline lo_filter symbol");
        let declared_type = lo_filter
            .declared_type
            .as_ref()
            .expect("inline method result type");
        assert!(declared_type.is_ref);
        assert_eq!(declared_type.base_name.as_ref(), "/iwbep/if_mgw_req_filter");

        let hover = snapshot
            .hovered_resolved_symbol_at(main_src.find("lo_filter").expect("lo_filter") + 1)
            .expect("lo_filter hover");
        assert!(
            hover
                .markdown_lines
                .iter()
                .any(|line| { line == "```abap\nTYPE REF TO /iwbep/if_mgw_req_filter\n```" }),
            "{:?}",
            hover.markdown_lines
        );

        let completion = snapshot
            .selector_completion_at(
                main_src.find("lo_filter->").expect("selector") + "lo_filter->".len(),
            )
            .expect("lo_filter method completion");
        let labels = completion
            .items
            .iter()
            .map(|item| item.name.as_ref())
            .collect::<Vec<_>>();
        assert!(labels.contains(&"get_filter_select_options"), "{labels:?}");
        assert!(labels.contains(&"get_filter_string"), "{labels:?}");
    }

    #[test]
    fn lists_method_completion_items_inside_assignment_template_expression() {
        let store = DocumentStore::default();
        let src = "\
CLASS zcl_expr DEFINITION.
  PUBLIC SECTION.
    METHODS to_source.
    METHODS to_string
      RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_expr IMPLEMENTATION.
ENDCLASS.

DATA lo_expr TYPE REF TO zcl_expr.
DATA rv_text TYPE string.
rv_text = |value: { lo_expr->to_ }|.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.rfind("to_").expect("method prefix") + "to_".len();

        let completion = snapshot
            .selector_completion_at(offset)
            .expect("selector completion");
        assert!(!completion.in_type_position);
        assert_eq!(
            completion
                .items
                .iter()
                .map(|item| item.name.as_ref())
                .collect::<Vec<_>>(),
            vec!["to_source", "to_string"]
        );
        assert_eq!(&src[completion.replace_range], "to_");
    }

    #[test]
    fn lists_inherited_method_completion_items_inside_assignment_template_expression() {
        let store = DocumentStore::default();
        let src = "\
CLASS zcl_ast_node DEFINITION.
  PUBLIC SECTION.
    METHODS to_source.
    METHODS to_string
      RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_ast_node IMPLEMENTATION.
ENDCLASS.

CLASS zcl_expr DEFINITION INHERITING FROM zcl_ast_node.
ENDCLASS.

CLASS zcl_expr IMPLEMENTATION.
ENDCLASS.

DATA lo_expr TYPE REF TO zcl_expr.
DATA rv_text TYPE string.
rv_text = |value: { lo_expr->to_ }|.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.rfind("to_").expect("method prefix") + "to_".len();

        let completion = snapshot
            .selector_completion_at(offset)
            .expect("selector completion");
        assert!(!completion.in_type_position);
        assert_eq!(
            completion
                .items
                .iter()
                .map(|item| item.name.as_ref())
                .collect::<Vec<_>>(),
            vec!["to_source", "to_string"]
        );
        assert_eq!(&src[completion.replace_range], "to_");
    }

    #[test]
    fn lists_selector_completion_items_in_unterminated_binary_expression() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF ty_inner,
         alpha TYPE i,
       END OF ty_inner.
TYPES: BEGIN OF ty_outer,
         inner TYPE ty_inner,
       END OF ty_outer.
DATA ls_outer TYPE ty_outer.
DATA lv_total TYPE i.
lv_total = ls_outer-inner- + 1";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.find("inner-").expect("selector") + "inner-".len();

        let completion = snapshot
            .selector_completion_at(offset)
            .expect("selector completion");
        assert!(!completion.in_type_position);
        assert_eq!(completion.items.len(), 1);
        assert_eq!(completion.items[0].name.as_ref(), "alpha");
    }

    #[test]
    fn lists_bare_where_field_completion_items_after_where_keyword() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF ty_row,
         status_trn TYPE i,
         trn_id TYPE i,
       END OF ty_row.
TYPES ty_tab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lt_trans_del TYPE ty_tab.
DELETE lt_trans_del WHERE ";
        let snapshot = store.publish("file:///demo.abap", 1, src);

        let completion = snapshot
            .selector_completion_at(src.len())
            .expect("bare where completion");
        assert_eq!(
            completion
                .items
                .iter()
                .map(|item| item.name.as_ref())
                .collect::<Vec<_>>(),
            vec!["status_trn", "trn_id"]
        );
        assert!(completion.replace_range.is_empty());
    }

    #[test]
    fn lists_bare_where_field_completion_items_with_prefix() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF ty_row,
         status_trn TYPE i,
         trn_id TYPE i,
       END OF ty_row.
TYPES ty_tab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lt_trans_del TYPE ty_tab.
DELETE lt_trans_del WHERE sta";
        let snapshot = store.publish("file:///demo.abap", 1, src);

        let completion = snapshot
            .selector_completion_at(src.len())
            .expect("bare where completion");
        assert_eq!(completion.items.len(), 1);
        assert_eq!(completion.items[0].name.as_ref(), "status_trn");
        assert_eq!(&src[completion.replace_range], "sta");
    }

    #[test]
    fn lists_open_sql_source_completion_items_after_from_keyword() {
        let store = DocumentStore::default();
        let dep_src = "\
TYPES: BEGIN OF /sttp/loc,
         locno TYPE string,
       END OF /sttp/loc.
";
        let main_src = "SELECT * FROM /sttp/l";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///deps/STTP_LOC.abap"),
                version: 1,
                text: Arc::from(dep_src),
                is_dependency: true,
                object_name: Some(Arc::from("/sttp/loc")),
            },
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots.get("file:///main.abap").expect("main snapshot");

        let completion = snapshot
            .completion_at(main_src.len())
            .expect("Open SQL source completion");
        assert!(
            completion.items.iter().any(|item| {
                matches!(item, crate::CompletionItem::Symbol(item) if item.name.as_ref() == "/sttp/loc")
            }),
            "expected /sttp/loc source completion: {:?}",
            completion.items
        );
        assert_eq!(&main_src[completion.replace_range], "/sttp/l");
    }

    #[test]
    fn lists_open_sql_projection_field_completion_items() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF /sttp/loc,
         locno TYPE string,
         gln TYPE string,
       END OF /sttp/loc.
SELECT lo FROM /sttp/loc INTO TABLE @DATA(lt_loc).";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.find("SELECT lo").expect("projection") + "SELECT lo".len();

        let completion = snapshot
            .selector_completion_at(offset)
            .expect("Open SQL projection completion");
        assert_eq!(completion.items.len(), 1);
        assert_eq!(completion.items[0].name.as_ref(), "locno");
        assert_eq!(&src[completion.replace_range], "lo");
    }

    #[test]
    fn lists_open_sql_where_field_completion_items() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF /sttp/loc,
         locno TYPE string,
         gln TYPE string,
       END OF /sttp/loc.
SELECT * FROM /sttp/loc INTO TABLE @DATA(lt_loc) WHERE gl";
        let snapshot = store.publish("file:///demo.abap", 1, src);

        let completion = snapshot
            .selector_completion_at(src.len())
            .expect("Open SQL WHERE field completion");
        assert_eq!(completion.items.len(), 1);
        assert_eq!(completion.items[0].name.as_ref(), "gln");
        assert_eq!(&src[completion.replace_range], "gl");
    }

    #[test]
    fn lists_open_sql_qualified_field_completion_items_after_alias_tilde() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF /sttp/loc,
         locno TYPE string,
         gln TYPE string,
       END OF /sttp/loc.
SELECT * FROM /sttp/loc AS loc INTO TABLE @DATA(lt_loc) WHERE loc~";
        let snapshot = store.publish("file:///demo.abap", 1, src);

        let completion = snapshot
            .selector_completion_at(src.len())
            .expect("Open SQL qualified field completion");
        assert_eq!(
            completion
                .items
                .iter()
                .map(|item| item.name.as_ref())
                .collect::<Vec<_>>(),
            vec!["gln", "locno"]
        );
        assert!(completion.replace_range.is_empty());
    }

    #[test]
    fn lists_read_table_with_key_field_completion_items_with_prefix() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF ty_row,
         vbeln TYPE string,
         posnn TYPE string,
       END OF ty_row.
TYPES ty_tab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA t_vbfa TYPE ty_tab.
DATA ls_vbfa TYPE ty_row.
READ TABLE t_vbfa INTO ls_vbfa WITH KEY vb";
        let snapshot = store.publish("file:///demo.abap", 1, src);

        let completion = snapshot
            .selector_completion_at(src.len())
            .expect("read table key completion");
        assert_eq!(completion.items.len(), 1);
        assert_eq!(completion.items[0].name.as_ref(), "vbeln");
        assert_eq!(&src[completion.replace_range], "vb");
    }

    #[test]
    fn lists_corresponding_mapping_target_field_completion_items_with_prefix() {
        let store = DocumentStore::default();
        let src = "\
TYPES ty_objid_rng TYPE RANGE OF i.
TYPES: BEGIN OF ty_evt,
         objid TYPE i,
       END OF ty_evt.
DATA ct_amdp_rec_evt_objid TYPE STANDARD TABLE OF ty_evt WITH EMPTY KEY.
DATA(lr_objid) = CORRESPONDING ty_objid_rng(
                   ct_amdp_rec_evt_objid
                 MAPPING lo = objid ).";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.find("lo = objid").expect("target prefix") + 2;

        let completion = snapshot
            .selector_completion_at(offset)
            .expect("mapping target completion");
        assert!(
            completion
                .items
                .iter()
                .any(|item| item.name.as_ref() == "low"),
            "expected mapping target field completion: {:?}",
            completion
                .items
                .iter()
                .map(|item| item.name.as_ref())
                .collect::<Vec<_>>()
        );
        assert_eq!(&src[completion.replace_range], "lo");
    }

    #[test]
    fn lists_corresponding_mapping_source_field_completion_items_with_prefix() {
        let store = DocumentStore::default();
        let src = "\
TYPES ty_objid_rng TYPE RANGE OF i.
TYPES: BEGIN OF ty_evt,
         objid TYPE i,
       END OF ty_evt.
DATA ct_amdp_rec_evt_objid TYPE STANDARD TABLE OF ty_evt WITH EMPTY KEY.
DATA(lr_objid) = CORRESPONDING ty_objid_rng(
                   ct_amdp_rec_evt_objid
                 MAPPING low = obj ).";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.find("obj ).").expect("source prefix") + 3;

        let completion = snapshot
            .selector_completion_at(offset)
            .expect("mapping source completion");
        assert!(
            completion
                .items
                .iter()
                .any(|item| item.name.as_ref() == "objid"),
            "expected mapping source field completion: {:?}",
            completion
                .items
                .iter()
                .map(|item| item.name.as_ref())
                .collect::<Vec<_>>()
        );
        assert_eq!(&src[completion.replace_range], "obj");
    }

    #[test]
    fn definition_at_returns_first_refresh_target_declaration() {
        let store = DocumentStore::default();
        let src = "\
DATA lt_packing TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA lt_prot TYPE STANDARD TABLE OF string WITH EMPTY KEY.

REFRESH:
  lt_packing,
  lt_prot.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.find("lt_packing,").expect("refresh target") + 1;

        let hovered = snapshot
            .hovered_resolved_symbol_at(offset)
            .expect("refresh target hover");
        assert_eq!(hovered.display_name.as_ref(), "lt_packing");

        let definition = snapshot
            .definition_at(offset)
            .expect("refresh target definition");
        assert_eq!(definition.uri.as_ref(), "file:///demo.abap");
        assert_eq!(&src[definition.range], "lt_packing");
    }

    #[test]
    fn definition_at_returns_bare_delete_where_field_declaration() {
        let store = DocumentStore::default();
        let src = "\
TYPES: BEGIN OF ty_row,
         status_trn TYPE i,
         trn_id TYPE i,
       END OF ty_row.
TYPES ty_tab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
DATA lt_trans_del TYPE ty_tab.
DELETE lt_trans_del WHERE status_trn IS NOT INITIAL.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.rfind("status_trn").expect("field use") + 1;

        let hovered = snapshot
            .hovered_component_at(offset)
            .expect("hovered bare where field");
        assert_eq!(hovered.field_name.as_ref(), "status_trn");

        let definition = snapshot.definition_at(offset).expect("field definition");
        assert_eq!(definition.uri.as_ref(), "file:///demo.abap");
        assert_eq!(&src[definition.range], "status_trn");
    }

    #[test]
    fn hover_prefers_selector_component_for_open_sql_legacy_host_expr() {
        let store = DocumentStore::default();
        let src = "\
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD run.
    TYPES: BEGIN OF ty_mat,
             matid TYPE string,
           END OF ty_mat.
    DATA ls_mat TYPE ty_mat.
    SELECT * FROM demo INTO TABLE @DATA(lt_rows) WHERE mandt = ls_mat-matid.
  ENDMETHOD.
ENDCLASS.";
        let snapshot = store.publish("file:///demo.abap", 1, src);
        let offset = src.rfind("matid").expect("selector field use") + 1;

        let hovered = snapshot
            .hovered_component_at(offset)
            .expect("hovered selector component");
        assert_eq!(hovered.base_name.as_ref(), "ls_mat");
        assert_eq!(hovered.field_name.as_ref(), "matid");
        assert!(snapshot.hovered_sql_name_ref_at(offset).is_none());
    }

    #[test]
    fn references_include_declaration_and_uses_for_variable_across_documents() {
        let store = DocumentStore::default();
        let main_src = "INCLUDE helper.\nDATA lv TYPE i.\nlv = 1.";
        let helper_src = "DATA lv_other TYPE i.\nlv = lv_other.";
        let main = store.publish("file:///main.abap", 1, main_src);
        store.publish("file:///helper.abap", 1, helper_src);

        let offset = main_src.rfind("lv").expect("variable use") + 1;
        let references = store
            .references("file:///main.abap", offset, true)
            .expect("references");

        assert_reference_slices(
            &references,
            &[
                ("file:///helper.abap", helper_src, "lv"),
                ("file:///main.abap", main_src, "lv"),
                ("file:///main.abap", main_src, "lv"),
            ],
        );
        assert_eq!(main.version, 1);
    }

    #[test]
    fn rename_plan_includes_method_declaration_implementation_and_call() {
        let store = DocumentStore::default();
        let src = "\
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
    METHODS caller.
ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
  METHOD caller.
    run( ).
  ENDMETHOD.
ENDCLASS.";
        let snapshot = store.publish("file:///rename_method.abap", 1, src);

        let offset = src.rfind("run(").expect("method call") + 1;
        let plan = store
            .rename_plan("file:///rename_method.abap", offset)
            .expect("rename plan");

        assert_eq!(plan.placeholder, "run");
        assert_reference_slices(
            &plan.locations,
            &[("file:///rename_method.abap", src, "run"); 3],
        );
        assert_eq!(snapshot.version, 1);
    }

    #[test]
    fn rename_plan_uses_member_segment_for_interface_qualified_method_calls() {
        let store = DocumentStore::default();
        let src = "\
INTERFACE i1.
  METHODS meth.
ENDINTERFACE.

CLASS c1 DEFINITION.
  PUBLIC SECTION.
    INTERFACES i1.
ENDCLASS.

CLASS c1 IMPLEMENTATION.
  METHOD i1~meth.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo_obj TYPE REF TO c1.
  lo_obj->i1~meth( ).";
        let snapshot = store.publish("file:///qualified_method.abap", 1, src);

        let offset = src.rfind("meth(").expect("qualified method call") + 1;
        let plan = store
            .rename_plan("file:///qualified_method.abap", offset)
            .expect("rename plan");

        assert_eq!(plan.placeholder, "meth");
        assert_reference_slices(
            &plan.locations,
            &[("file:///qualified_method.abap", src, "meth"); 3],
        );
        assert_eq!(snapshot.version, 1);
    }

    #[test]
    fn method_body_include_resolves_symbols_from_dependency_include() {
        let store = DocumentStore::default();
        let main_src = "\
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD run.
    INCLUDE zinc_method.
    lv_inc = 1.
  ENDMETHOD.
ENDCLASS.";
        let snapshot = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///zinc_method.abap"),
                version: 1,
                text: Arc::from("DATA lv_inc TYPE i."),
                is_dependency: true,
                object_name: None,
            },
        ]);
        let main = snapshot.get("file:///main.abap").expect("main snapshot");
        let offset = main_src.rfind("lv_inc").expect("method include use") + 1;
        let hovered = main
            .hovered_resolved_symbol_at(offset)
            .expect("included symbol hover");

        assert_eq!(hovered.display_name.as_ref(), "lv_inc");
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line.contains("Variable"))
        );
    }

    #[test]
    fn new_shorthand_constructor_parameter_hover_uses_target_from_top_include() {
        let store = DocumentStore::default();
        let main_src = "INCLUDE top.\nINCLUDE f01.";
        let child_src = "\
CLASS zcl_child DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING container_name TYPE string.
ENDCLASS.";
        let top_src = "\
CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS display.
  PRIVATE SECTION.
    DATA mo_cont TYPE REF TO zcl_child.
ENDCLASS.";
        let f01_src = "\
CLASS lcl_app IMPLEMENTATION.
  METHOD display.
    mo_cont = NEW #( container_name = 'CCONTAINER' ).
  ENDMETHOD.
ENDCLASS.";
        let snapshot = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///top.abap"),
                version: 1,
                text: Arc::from(top_src),
                is_dependency: false,
                object_name: Some(Arc::from("top")),
            },
            DocumentInput {
                uri: Arc::from("file:///f01.abap"),
                version: 1,
                text: Arc::from(f01_src),
                is_dependency: false,
                object_name: Some(Arc::from("f01")),
            },
            DocumentInput {
                uri: Arc::from("file:///zcl_child.abap"),
                version: 1,
                text: Arc::from(child_src),
                is_dependency: true,
                object_name: Some(Arc::from("zcl_child")),
            },
        ]);
        let f01 = snapshot.get("file:///f01.abap").expect("f01 snapshot");
        let offset = f01_src.find("container_name").expect("parameter") + 1;
        let access = f01
            .symbols
            .named_arguments
            .iter()
            .find(|access| access.name.as_ref() == "container_name")
            .expect("recorded constructor argument");
        assert!(f01.has_named_argument_parameter(access));
        let hovered = f01
            .hovered_named_argument_at(offset)
            .expect("constructor parameter hover");

        assert_eq!(hovered.display_name.as_ref(), "container_name");
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line == "Parameter")
        );
        assert!(
            hovered
                .markdown_lines
                .iter()
                .any(|line| line == "```abap\nTYPE string\n```")
        );
    }

    #[test]
    fn encoded_dependency_include_uri_resolves_by_object_name_hint() {
        let store = DocumentStore::default();
        let main_src = "INCLUDE /sttp/int_global.\nlv_inc = 1.";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from(
                    "file:///d:/dev/abap/lsp_development_examples/.abapls/cache/dependencies/include/%2FSTTP%2FINT_GLOBAL.abap",
                ),
                version: 1,
                text: Arc::from("DATA lv_inc TYPE i."),
                is_dependency: true,
                object_name: Some(Arc::from("/sttp/int_global")),
            },
        ]);
        let main = snapshots.get("file:///main.abap").expect("main snapshot");

        assert!(
            main.symbols
                .include_edges
                .iter()
                .any(|edge| edge.name.as_ref() == "/sttp/int_global" && edge.target.is_some())
        );
        assert!(
            !main
                .project
                .diagnostics
                .iter()
                .any(|diag| diag.message.contains("/sttp/int_global"))
        );
    }

    #[test]
    fn references_find_method_selector_uses_across_documents() {
        let store = DocumentStore::default();
        let decl_src = "\
CLASS some_class DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS exec.
ENDCLASS.
";
        let use_src = "some_class=>exec( ).";
        let decl = store.publish("file:///class.abap", 1, decl_src);
        store.publish("file:///use.abap", 1, use_src);

        let offset = decl_src.find("exec").expect("method declaration") + 1;
        let references = store
            .references("file:///class.abap", offset, true)
            .expect("references");

        assert_reference_slices(
            &references,
            &[
                ("file:///class.abap", decl_src, "exec"),
                ("file:///use.abap", use_src, "exec"),
            ],
        );
        assert_eq!(decl.version, 1);
    }

    #[test]
    fn updating_one_document_keeps_cross_document_references_working() {
        let store = DocumentStore::default();
        let main_v1 = "INCLUDE helper.\nDATA lv TYPE i.\nlv = 1.";
        let main_v2 = "INCLUDE helper.\nDATA lv TYPE i.\nlv = 2.";
        let helper_src = "DATA lv_other TYPE i.\nlv = lv_other.";
        store.publish("file:///main.abap", 1, main_v1);
        store.publish("file:///helper.abap", 1, helper_src);
        let main = store.publish("file:///main.abap", 2, main_v2);

        let offset = main_v2.rfind("lv").expect("variable use") + 1;
        let references = store
            .references("file:///main.abap", offset, true)
            .expect("references");

        assert_reference_slices(
            &references,
            &[
                ("file:///helper.abap", helper_src, "lv"),
                ("file:///main.abap", main_v2, "lv"),
                ("file:///main.abap", main_v2, "lv"),
            ],
        );
        assert_eq!(main.version, 2);
        assert_eq!(store.get("file:///helper.abap").unwrap().version, 1);
    }

    #[test]
    fn references_find_named_argument_labels_for_method_parameters() {
        let store = DocumentStore::default();
        let src = "\
CLASS zcl_program DEFINITION.
  PUBLIC SECTION.
    METHODS add_statement
      IMPORTING io_stmt TYPE string.
ENDCLASS.

CLASS zcl_program IMPLEMENTATION.
  METHOD add_statement.
    DATA lv_copy TYPE string.
    lv_copy = io_stmt.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA(lo_prog) = NEW zcl_program( ).
  lo_prog->add_statement( io_stmt = 'x' ).";
        let snapshot = store.publish("file:///refs_param.abap", 1, src);

        let offset = src.find("io_stmt").expect("parameter declaration") + 1;
        let references = store
            .references("file:///refs_param.abap", offset, true)
            .expect("references");

        assert_reference_slices(
            &references,
            &[("file:///refs_param.abap", src, "io_stmt"); 3],
        );
        assert_eq!(snapshot.version, 1);
    }

    #[test]
    fn references_on_perform_argument_follow_actual_variable_symbol() {
        let store = DocumentStore::default();
        let src = "\
FORM f USING VALUE(iv_input) TYPE i.
  DATA lv_copy TYPE i.
  lv_copy = iv_input.
ENDFORM.

START-OF-SELECTION.
  DATA lv_input TYPE i VALUE 1.
  WRITE lv_input.
  PERFORM f USING lv_input.
";
        let snapshot = store.publish("file:///refs_perform.abap", 1, src);

        let offset = src.rfind("lv_input").expect("perform argument use") + 1;
        let references = store
            .references("file:///refs_perform.abap", offset, true)
            .expect("references");

        assert_reference_slices(
            &references,
            &[("file:///refs_perform.abap", src, "lv_input"); 3],
        );
        assert_eq!(snapshot.version, 1);
    }

    #[test]
    fn snapshot_line_index_maps_offsets_and_utf16_positions() {
        let store = DocumentStore::default();
        let src = "WRITE / |a😀|.\r\nWRITE / |bó|.\n";
        let snapshot = store.publish("file:///line_index.abap", 1, src);

        let emoji_offset = src.find("😀").expect("emoji offset");
        assert_eq!(
            snapshot.offset_to_line_utf16_position(emoji_offset),
            Some((0, 10))
        );
        assert_eq!(
            snapshot.line_utf16_position_to_offset(0, 10),
            Some(emoji_offset)
        );

        let accented_offset = src.find("ó").expect("accented offset");
        assert_eq!(
            snapshot.offset_to_line_utf16_position(accented_offset),
            Some((1, 10))
        );
        assert_eq!(
            snapshot.line_utf16_position_to_offset(1, 10),
            Some(accented_offset)
        );

        let cr_offset = src.find('\r').expect("carriage return offset");
        assert_eq!(
            snapshot.offset_to_line_utf16_position(cr_offset),
            Some((0, 14))
        );
        assert_eq!(
            snapshot.line_utf16_position_to_offset(0, 14),
            Some(cr_offset)
        );

        let newline_offset = src.find('\n').expect("newline offset");
        assert_eq!(snapshot.offset_to_line_utf16_position(newline_offset), None);
    }
}
