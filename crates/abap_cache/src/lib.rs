use std::collections::{HashMap, HashSet};
use std::ops::Range;
use std::sync::Arc;

use abap_lexer::TokenKind;
use abap_parser::{ParseResult, parse};
use abap_symbols::{
    ClassMemberData, ClassMemberKind, FieldTypeRefData, FormParameterData,
    FormParameterPassingKind, FormParameterSection, NamedArgumentAccess, NamedArgumentTarget,
    Namespace, PerformArgumentData, PerformCallData, PerformParameterSection, ProjectAnalysis,
    ReferenceKind, Resolution, ScopeId, ScopeKind, SqlNameRefData, SqlNameRefKind,
    StructureFieldInfo, StructureFieldShape, StructureId, SymbolData, SymbolHandle, SymbolId,
    SymbolKind, UnitAnalysis, UnitId, Visibility, analyze_project_from_units, analyze_unit_locally,
    builtin_routine_spec,
};
use parking_lot::RwLock;
use rayon::prelude::*;

mod workspace;
pub use workspace::{
    DEFAULT_REMOTE_REQUEST_PARALLELISM, DEFAULT_REMOTE_REQUESTS_PER_SECOND,
    DEPENDENCY_MODE_LOCAL_FIRST, DEPENDENCY_MODE_REMOTE_ON_DEMAND, ManifestResolution,
    ManifestUnit, ManifestUnitMember, OpenDocumentOverlay, UNKNOWN_SYMBOL_MODE_LOG,
    UNKNOWN_SYMBOL_MODE_REMOTE, WorkspaceDocument, WorkspaceLoadResult, WorkspaceManifest,
    ddic_xml_to_abap_source, file_uri_to_path, is_remote_lookup_candidate, is_remote_lookup_name,
    load_manifest_from_workspace, load_manifest_from_workspace_result, load_workspace_documents,
    manifest_cache_dir, manifest_supports_remote_resolution, normalize_dependency_mode,
    normalize_unknown_symbol_mode, path_to_file_uri, uri_starts_with_workspace,
    workspace_relative_path,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalysisSnapshot {
    pub uri: Arc<str>,
    pub version: i32,
    pub text: Arc<str>,
    pub parse: Arc<ParseResult>,
    pub symbols: Arc<UnitAnalysis>,
    pub project: Arc<ProjectAnalysis>,
    scope_index: Arc<ScopeIndex>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocumentInput {
    pub uri: Arc<str>,
    pub version: i32,
    pub text: Arc<str>,
    pub is_dependency: bool,
    pub object_name: Option<Arc<str>>,
}

#[derive(Debug, Clone)]
struct StagedDocument {
    uri: Arc<str>,
    version: i32,
    text: Arc<str>,
    parse: Arc<ParseResult>,
    previous: Option<Arc<AnalysisSnapshot>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HoveredComponentKind {
    Scalar,
    Structured { structure_name: Arc<str> },
    Attribute,
    Method,
    Interface,
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
pub struct DefinitionTarget {
    pub uri: Arc<str>,
    pub range: Range<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReferenceTarget {
    pub uri: Arc<str>,
    pub range: Range<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectorCompletionItem {
    pub name: Arc<str>,
    pub declared_type: Option<String>,
    pub declaration: Option<String>,
    pub kind: HoveredComponentKind,
    pub field_owner_structure_name: Option<Arc<str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectorCompletionInfo {
    pub replace_range: Range<usize>,
    pub items: Vec<SelectorCompletionItem>,
    pub in_type_position: bool,
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
struct BareWhereFieldTarget {
    structure_unit_id: UnitId,
    field: StructureFieldInfo,
    range: Range<usize>,
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

fn markdown_lines_for_sql_name_ref(sql_ref: &SqlNameRefData) -> Vec<String> {
    let title = match sql_ref.kind {
        SqlNameRefKind::Source => "Open SQL data source (DDIC object)",
        SqlNameRefKind::Alias => "Open SQL alias",
        SqlNameRefKind::Column => "Open SQL column",
        SqlNameRefKind::QualifiedColumn => "Open SQL column",
        SqlNameRefKind::Star => "Open SQL `*` projection",
        SqlNameRefKind::QualifiedStar => "Open SQL qualified `*` projection",
        SqlNameRefKind::Aggregate => "Open SQL aggregate",
    };
    let mut lines = vec![format!("`{}`", sql_ref.name), title.to_string()];
    if let Some(qual) = sql_ref.qualifier.as_ref() {
        lines.push(format!("Table alias `{}`", qual));
    }
    if matches!(sql_ref.kind, SqlNameRefKind::Source) {
        lines.push(
            "The analyzer emits a warning until the source is verified against SAP DDIC/repository (not connected in this build). Use SAP ADT or the VS Code remote dependency fetch for metadata."
                .to_string(),
        );
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
    signature: String,
}

type ScopeIndex = Vec<HashMap<(Namespace, Arc<str>), Vec<SymbolId>>>;

pub struct SemanticTokenLookupContext<'a> {
    snapshot: &'a AnalysisSnapshot,
    scope_index: ScopeIndex,
}

impl AnalysisSnapshot {
    pub fn scope_index(&self) -> &ScopeIndex {
        self.scope_index.as_ref()
    }

    pub fn semantic_token_lookup_context(&self) -> SemanticTokenLookupContext<'_> {
        SemanticTokenLookupContext {
            snapshot: self,
            scope_index: self.scope_index.as_ref().clone(),
        }
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
                    declaration: Some(format!(
                        "INTERFACE {}",
                        interface_unit.symbol(interface_symbol).name
                    )),
                    kind: HoveredComponentKind::Interface,
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
                    declaration: Some(format_class_member_signature(member)),
                    kind: hovered_component_kind_for_class_member(member),
                    is_static_method: member.is_static,
                    in_type_position: access.in_type_position,
                });
            }
            let (structure_unit, structure_id) = resolve_symbol_structure_with_scope_index(
                self,
                self.scope_index(),
                unit,
                access.scope,
                symbol_id,
            )?;
            let field_path: Vec<_> = access
                .field_path
                .iter()
                .take(segment_index + 1)
                .map(|segment| segment.name.as_ref())
                .collect();
            let field = structure_unit
                .semantic()
                .decls()
                .resolve_structure_field_path(structure_id, &field_path)?;
            let kind = match field.shape {
                StructureFieldShape::Scalar => HoveredComponentKind::Scalar,
                StructureFieldShape::Structured { structure } => HoveredComponentKind::Structured {
                    structure_name: Arc::clone(&structure_unit.structure(structure).name),
                },
            };
            let field_owner_structure_name =
                Some(Arc::clone(&structure_unit.structure(field.owner).name));
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
                declaration: None,
                kind,
                is_static_method: false,
                in_type_position: access.in_type_position,
            });
        }
        let target = self.bare_where_field_target_at(offset)?;
        let structure_unit = &self.project.units[target.structure_unit_id.as_usize()];
        let field = &target.field;
        let kind = match field.shape {
            StructureFieldShape::Scalar => HoveredComponentKind::Scalar,
            StructureFieldShape::Structured { structure } => HoveredComponentKind::Structured {
                structure_name: Arc::clone(&structure_unit.structure(structure).name),
            },
        };
        Some(HoveredComponentInfo {
            base_name: Arc::clone(&field.name),
            base_namespace: Namespace::Value,
            component_path: vec![Arc::clone(&field.name)],
            field_name: Arc::clone(&field.name),
            field_owner_structure_name: Some(Arc::clone(
                &structure_unit.structure(field.owner).name,
            )),
            range: target.range,
            declared_type: field.type_ref.as_ref().map(format_field_type_ref),
            declaration: None,
            kind,
            is_static_method: false,
            in_type_position: false,
        })
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

    pub fn has_named_argument_parameter(&self, access: &NamedArgumentAccess) -> bool {
        resolve_named_argument_parameter_with_scope_index(self, self.scope_index(), access)
            .is_some()
    }

    pub fn hovered_perform_argument_at(&self, offset: usize) -> Option<HoveredSymbolInfo> {
        let (perform_call, argument) = self
            .symbols
            .perform_calls
            .iter()
            .filter_map(|perform_call| {
                perform_call
                    .arguments
                    .iter()
                    .find(|argument| argument.range.start <= offset && offset < argument.range.end)
                    .map(|argument| (perform_call, argument))
            })
            .min_by_key(|(_, argument)| argument.range.end.saturating_sub(argument.range.start))?;
        let parameter = resolve_perform_argument_parameter(self, perform_call, argument)?;
        Some(HoveredSymbolInfo {
            range: argument.range.clone(),
            display_name: Arc::clone(&parameter.name),
            markdown_lines: markdown_lines_for_form_parameter(&parameter),
        })
    }

    /// Hover for an Open SQL name span (`FROM` source, column, alias, and similar).
    pub fn hovered_sql_name_ref_at(&self, offset: usize) -> Option<HoveredSymbolInfo> {
        let sql_ref = self.symbols.semantic().sql().name_ref_at_offset(offset)?;
        Some(HoveredSymbolInfo {
            range: sql_ref.range.clone(),
            display_name: Arc::clone(&sql_ref.name),
            markdown_lines: markdown_lines_for_sql_name_ref(sql_ref),
        })
    }

    pub fn definition_at(&self, offset: usize) -> Option<DefinitionTarget> {
        if let Some(target) = self.definition_target_for_component_at(offset) {
            return Some(target);
        }
        if let Some(target) = self.definition_target_for_perform_argument_at(offset) {
            return Some(target);
        }
        if let Some(target) = self.definition_target_for_named_argument_at(offset) {
            return Some(target);
        }
        if let Some(target) = self.definition_target_for_sql_source_matching_type_ref(offset) {
            return Some(target);
        }
        self.definition_target_for_resolved_symbol_at(offset)
            .or_else(|| self.definition_target_for_bare_where_field_at(offset))
    }

    fn reference_search_target_at(&self, offset: usize) -> Option<ReferenceSearchTarget> {
        if let Some(target) = self.reference_search_target_for_component_at(offset) {
            return Some(target);
        }
        if let Some(target) = self.reference_search_target_for_perform_argument_at(offset) {
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

        Some(HoveredSymbolInfo {
            range: symbol.decl_range.clone(),
            display_name: Arc::clone(&symbol.name),
            markdown_lines: markdown_lines_for_declared_symbol(self.symbols.as_ref(), symbol),
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
            if let Some((member_unit, member)) =
                resolve_class_selector_member(self, access, segment_index, unit, symbol_id)
            {
                return Some(definition_target_for_class_member(member_unit, member));
            }
            let (structure_unit, structure_id) = resolve_symbol_structure_with_scope_index(
                self,
                self.scope_index(),
                unit,
                access.scope,
                symbol_id,
            )?;
            let field_path: Vec<_> = access
                .field_path
                .iter()
                .take(segment_index + 1)
                .map(|segment| segment.name.as_ref())
                .collect();
            let field = structure_unit
                .semantic()
                .decls()
                .resolve_structure_field_path(structure_id, &field_path)?;
            let decl_range = field.decl_range?;
            return Some(definition_target_for_range(
                &self.project.units[field.decl_unit.as_usize()],
                decl_range,
            ));
        }
        None
    }

    fn definition_target_for_bare_where_field_at(&self, offset: usize) -> Option<DefinitionTarget> {
        let target = self.bare_where_field_target_at(offset)?;
        let decl_range = target.field.decl_range?;
        Some(definition_target_for_range(
            &self.project.units[target.field.decl_unit.as_usize()],
            decl_range,
        ))
    }

    fn reference_search_target_for_component_at(
        &self,
        offset: usize,
    ) -> Option<ReferenceSearchTarget> {
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
                return Some(ReferenceSearchTarget::ClassMember {
                    unit: member_unit.unit_id,
                    class_symbol: member.class_symbol,
                    name: Arc::clone(&member.name),
                });
            }
            let (structure_unit, structure_id) = resolve_symbol_structure_with_scope_index(
                self,
                self.scope_index(),
                unit,
                access.scope,
                symbol_id,
            )?;
            let field_path: Vec<_> = access
                .field_path
                .iter()
                .take(segment_index + 1)
                .map(|segment| segment.name.as_ref())
                .collect();
            let field = structure_unit
                .semantic()
                .decls()
                .resolve_structure_field_path(structure_id, &field_path)?;
            return Some(ReferenceSearchTarget::StructField {
                unit: field.owner_unit,
                owner: structure_unit.structure(field.owner).origin_structure,
                name: Arc::clone(&field.name),
            });
        }
        let target = self.bare_where_field_target_at(offset)?;
        Some(ReferenceSearchTarget::StructField {
            unit: target.field.owner_unit,
            owner: self.project.units[target.structure_unit_id.as_usize()]
                .structure(target.field.owner)
                .origin_structure,
            name: Arc::clone(&target.field.name),
        })
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

    fn definition_target_for_perform_argument_at(&self, offset: usize) -> Option<DefinitionTarget> {
        let (perform_call, argument) = self
            .symbols
            .perform_calls
            .iter()
            .filter_map(|perform_call| {
                perform_call
                    .arguments
                    .iter()
                    .find(|argument| argument.range.start <= offset && offset < argument.range.end)
                    .map(|argument| (perform_call, argument))
            })
            .min_by_key(|(_, argument)| argument.range.end.saturating_sub(argument.range.start))?;
        resolve_perform_argument_target(self, perform_call, argument)
    }

    fn reference_search_target_for_perform_argument_at(
        &self,
        offset: usize,
    ) -> Option<ReferenceSearchTarget> {
        let (perform_call, argument) = self
            .symbols
            .perform_calls
            .iter()
            .filter_map(|perform_call| {
                perform_call
                    .arguments
                    .iter()
                    .find(|argument| argument.range.start <= offset && offset < argument.range.end)
                    .map(|argument| (perform_call, argument))
            })
            .min_by_key(|(_, argument)| argument.range.end.saturating_sub(argument.range.start))?;
        Some(ReferenceSearchTarget::Symbol(
            resolve_perform_argument_symbol(self, perform_call, argument)?,
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
                self.symbols.as_ref(),
                member,
                offset,
            ));
        }

        self.symbols
            .semantic()
            .decls()
            .symbol_at_offset(offset)
            .map(|symbol| definition_target_for_symbol(self.symbols.as_ref(), symbol))
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
            return Some(ReferenceSearchTarget::Symbol(abap_symbols::SymbolHandle {
                unit: self.symbols.unit_id,
                symbol: symbol.id,
            }));
        }

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
        self.symbols
            .field_accesses
            .iter()
            .filter_map(|access| {
                let (unit, symbol_id) = resolve_field_access_base_symbol(self, access)?;
                let (member_unit, member) =
                    resolve_class_selector_member(self, access, 0, unit, symbol_id)?;
                (member_unit.unit_id == target_unit
                    && member.class_symbol == class_symbol
                    && member.name == *name)
                    .then(|| ReferenceTarget {
                        uri: Arc::clone(&self.uri),
                        range: access.field_path[0].range.clone(),
                    })
            })
            .collect()
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
            let Some((structure_unit, structure_id)) = resolve_symbol_structure_with_scope_index(
                self,
                self.scope_index(),
                unit,
                access.scope,
                symbol_id,
            ) else {
                continue;
            };
            for segment_index in 0..access.field_path.len() {
                if resolve_class_selector_member(self, access, segment_index, unit, symbol_id)
                    .is_some()
                {
                    continue;
                }
                let field_path: Vec<_> = access
                    .field_path
                    .iter()
                    .take(segment_index + 1)
                    .map(|segment| segment.name.as_ref())
                    .collect();
                let Some(field) = structure_unit
                    .semantic()
                    .decls()
                    .resolve_structure_field_path(structure_id, &field_path)
                else {
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
            return self.bare_where_field_completion_at(offset);
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
            let mut items: Vec<_> = collect_class_methods_in_hierarchy(self, unit, class_symbol_id)
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
                .map(|(_, member)| SelectorCompletionItem {
                    name: Arc::clone(&member.name),
                    declared_type: None,
                    declaration: Some(format_class_member_signature(member)),
                    kind: HoveredComponentKind::Method,
                    field_owner_structure_name: None,
                })
                .collect();
            items.sort_by(|left, right| left.name.cmp(&right.name));
            return Some(SelectorCompletionInfo {
                replace_range: query.replace_range,
                items,
                in_type_position: query.in_type_position,
            });
        }
        let (unit, symbol_id) = resolve_symbol_from_context(
            self,
            query.scope,
            query.base_namespace,
            &query.base_name,
            query.in_type_position,
        )?;
        let (structure_unit, mut structure_id) = resolve_symbol_structure_with_scope_index(
            self,
            self.scope_index(),
            unit,
            query.scope,
            symbol_id,
        )?;
        if !query.component_path.is_empty() {
            let path: Vec<_> = query
                .component_path
                .iter()
                .map(|part| part.as_ref())
                .collect();
            let field = structure_unit
                .semantic()
                .decls()
                .resolve_structure_field_path(structure_id, &path)?;
            structure_id = match field.shape {
                StructureFieldShape::Structured { structure } => structure,
                StructureFieldShape::Scalar => return None,
            };
        }

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
            })
            .collect();
        items.sort_by(|left, right| left.name.cmp(&right.name));
        Some(SelectorCompletionInfo {
            replace_range: query.replace_range,
            items,
            in_type_position: false,
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
        let field = structure_unit
            .semantic()
            .decls()
            .structure_field_info(query.structure_id, field_name.as_ref())?;
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
}

impl<'a> SemanticTokenLookupContext<'a> {
    pub fn classify_field_access_segment(
        &self,
        access: &abap_symbols::FieldAccess,
        segment_index: usize,
    ) -> Option<HoveredComponentKind> {
        classify_field_access_segment_with_scope_index(
            self.snapshot,
            &self.scope_index,
            access,
            segment_index,
        )
    }

    pub fn has_named_argument_parameter(&self, access: &NamedArgumentAccess) -> bool {
        resolve_named_argument_parameter_with_scope_index(self.snapshot, &self.scope_index, access)
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

fn format_class_member_signature(member: &ClassMemberData) -> String {
    if member.kind == ClassMemberKind::Method
        && let Some(formatted) = try_format_method_signature(member.signature.as_ref())
    {
        return formatted;
    }
    member.signature.to_string()
}

fn symbol_kind_label(kind: SymbolKind) -> &'static str {
    match kind {
        SymbolKind::BuiltinType => "Built-in type",
        SymbolKind::BuiltinRoutine => "Built-in routine",
        SymbolKind::BuiltinConstant => "Built-in constant",
        SymbolKind::BuiltinVariable => "Built-in variable",
        SymbolKind::Variable => "Variable",
        SymbolKind::Constant => "Constant",
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

fn symbol_type_line(unit: &UnitAnalysis, symbol: &SymbolData) -> Option<String> {
    if let Some(display) = symbol.type_clause_display.as_ref() {
        let keyword = match symbol.declared_type.as_ref().map(|t| t.namespace) {
            Some(Namespace::Value) => "LIKE",
            _ => "TYPE",
        };
        return Some(format_hover_type_clause(&format!(
            "{keyword} {}",
            display.trim()
        )));
    }
    if let Some(structure_id) = symbol.structure {
        let name = unit.structure(structure_id).name.as_ref();
        return Some(format_hover_type_clause(&format!("TYPE {name}")));
    }
    let type_ref = symbol.declared_type.as_ref()?;
    Some(format_hover_type_clause(&format_field_type_ref(type_ref)))
}

fn symbol_value_line(symbol: &SymbolData) -> Option<String> {
    if symbol.kind != SymbolKind::Constant {
        return None;
    }
    let value = symbol.value_clause_display.as_ref()?;
    Some(format_hover_abap(&format!("VALUE {}", value.trim())))
}

fn format_hover_abap(rendered: &str) -> String {
    format!("```abap\n{rendered}\n```")
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
        signature: String::new(),
    })
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
    Some(lines.join("\n"))
}

fn markdown_lines_for_form_parameter(info: &FormParameterHoverInfo) -> Vec<String> {
    vec![
        format!("`{}`", info.name),
        "Parameter".to_string(),
        format_hover_abap(&info.signature),
        format!("parameter of FORM `{}`", info.form_name),
    ]
}

fn markdown_lines_for_form(unit: &UnitAnalysis, symbol: &SymbolData) -> Vec<String> {
    if let Some(signature) = render_form_signature(unit, symbol) {
        return vec![format_hover_abap(&signature)];
    }
    vec![format!("`{}`", symbol.name), "Form".to_string()]
}

fn markdown_lines_for_declared_symbol(unit: &UnitAnalysis, symbol: &SymbolData) -> Vec<String> {
    if let Some(info) = form_parameter_hover_info(unit, symbol) {
        return markdown_lines_for_form_parameter(&info);
    }
    if symbol.kind == SymbolKind::Form {
        return markdown_lines_for_form(unit, symbol);
    }
    let mut lines = vec![
        format!("`{}`", symbol.name),
        symbol_kind_label(symbol.kind).to_string(),
    ];
    if let Some(type_line) = symbol_type_line(unit, symbol) {
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
    };
    vec![
        format!("```abap\n{}\n```", format_class_member_signature(member)),
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
            let mut lines = vec![
                format!("`{at_name}`"),
                symbol_kind_label(symbol.kind).to_string(),
            ];
            if let Some(type_line) = symbol_type_line(unit, symbol) {
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
    let rendered_params = spec
        .hover_params
        .iter()
        .copied()
        .collect::<Vec<_>>()
        .join(", ");
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

fn definition_target_for_class_member_name_at(
    unit: &UnitAnalysis,
    member: &ClassMemberData,
    offset: usize,
) -> DefinitionTarget {
    let target_range = match class_member_name_range_at_offset(member, offset) {
        Some(range) if *range == member.decl_range => member
            .implementation_range
            .clone()
            .unwrap_or_else(|| member.decl_range.clone()),
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
        let (interface_name, member_name) = method_name.split_once('~')?;
        let class_symbol = enclosing_class_owner(unit, symbol.scope)?;
        let class_handle = SymbolHandle {
            unit: unit.unit_id,
            symbol: class_symbol,
        };
        let interface_name = Arc::<str>::from(interface_name.to_ascii_lowercase());
        let (interface_unit, interface_symbol) = resolve_exposed_interface_handle_with_scope_index(
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
            let Some(field) = fields_unit
                .semantic()
                .decls()
                .structure_field_info(structure_id, symbol.name.as_ref())
            else {
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
        ReferenceSearchTarget::DdLikeTypeName { .. } => None,
    }
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

fn resolve_project_class_symbol<'a>(
    snapshot: &'a AnalysisSnapshot,
    preferred_unit: &'a UnitAnalysis,
    name: &Arc<str>,
) -> Option<(&'a UnitAnalysis, SymbolId)> {
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
        let field = current_unit
            .semantic()
            .decls()
            .structure_field_info(current_structure, segment.name.as_ref())?;
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
        let field = current_unit
            .semantic()
            .decls()
            .structure_field_info(current_structure, segment.name.as_ref())?;
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
    let signature = render_form_signature(unit, unit.symbol(form_symbol))?;
    Some(FormParameterHoverInfo {
        form_name: Arc::clone(&unit.symbol(form_symbol).name),
        name: Arc::clone(&symbol.name),
        section: parameter.section,
        passing: parameter.passing,
        declared_type: symbol.declared_type.clone(),
        signature,
    })
}

fn form_parameter_hover_info_from_metadata(
    unit: &UnitAnalysis,
    form_symbol: SymbolId,
    parameter: &FormParameterData,
) -> Option<FormParameterHoverInfo> {
    let symbol = unit.symbol(parameter.symbol);
    let signature = render_form_signature(unit, unit.symbol(form_symbol))?;
    Some(FormParameterHoverInfo {
        form_name: Arc::clone(&unit.symbol(form_symbol).name),
        name: Arc::clone(&symbol.name),
        section: parameter.section,
        passing: parameter.passing,
        declared_type: symbol.declared_type.clone(),
        signature,
    })
}

fn resolve_perform_argument_parameter(
    snapshot: &AnalysisSnapshot,
    perform_call: &PerformCallData,
    argument: &PerformArgumentData,
) -> Option<FormParameterHoverInfo> {
    let (unit, routine_symbol_id) = resolve_symbol_from_context(
        snapshot,
        perform_call.scope,
        Namespace::Routine,
        &perform_call.routine_name,
        false,
    )?;
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

fn resolve_perform_argument_target(
    snapshot: &AnalysisSnapshot,
    perform_call: &PerformCallData,
    argument: &PerformArgumentData,
) -> Option<DefinitionTarget> {
    let (unit, routine_symbol_id) = resolve_symbol_from_context(
        snapshot,
        perform_call.scope,
        Namespace::Routine,
        &perform_call.routine_name,
        false,
    )?;
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
    Some(definition_target_for_symbol(
        unit,
        unit.symbol(parameter.symbol),
    ))
}

fn resolve_perform_argument_symbol(
    snapshot: &AnalysisSnapshot,
    perform_call: &PerformCallData,
    argument: &PerformArgumentData,
) -> Option<abap_symbols::SymbolHandle> {
    let (unit, routine_symbol_id) = resolve_symbol_from_context(
        snapshot,
        perform_call.scope,
        Namespace::Routine,
        &perform_call.routine_name,
        false,
    )?;
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
        unit: unit.unit_id,
        symbol: parameter.symbol,
    })
}

fn resolve_named_argument_parameter<'a>(
    snapshot: &'a AnalysisSnapshot,
    access: &NamedArgumentAccess,
) -> Option<NamedArgumentParameterInfo> {
    resolve_named_argument_parameter_with_scope_index(snapshot, snapshot.scope_index(), access)
}

fn resolve_named_argument_parameter_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
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
        NamedArgumentTarget::Function { .. } => None,
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
        } => {
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
        NamedArgumentTarget::Function { .. } => None,
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
        } => {
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
        NamedArgumentTarget::Function { .. } => None,
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
        } => {
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
            lookup_scope_chain(current_unit, &scope_index, scope, namespace, name)
        {
            return Some((current_unit, symbol_id));
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
    )?;
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
    (class_unit.symbol(class_symbol_id).kind == SymbolKind::Class).then_some((
        class_unit,
        class_symbol_id,
        false,
    ))
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
            caller_unit.unit_id == target_unit.unit_id
                && enclosing_class_owner(caller_unit, caller_scope) == Some(member.class_symbol)
        }
        Visibility::Protected => {
            let Some(caller_class_symbol) = enclosing_class_owner(caller_unit, caller_scope) else {
                return false;
            };
            class_is_or_inherits_from(
                snapshot,
                (caller_unit.unit_id, caller_class_symbol),
                (target_unit.unit_id, member.class_symbol),
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
    let mut current = (class_unit.unit_id, class_symbol);
    let mut visited = HashSet::new();
    loop {
        if !visited.insert(current) {
            return None;
        }
        let unit = &snapshot.project.units[current.0.as_usize()];
        if let Some(member) = unit.semantic().decls().class_member(current.1, member_name) {
            return Some((unit, member));
        }
        let (next_unit, next_symbol) = direct_superclass_from_class(snapshot, unit, current.1)?;
        current = (next_unit.unit_id, next_symbol);
    }
}

fn collect_class_methods_in_hierarchy<'a>(
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
            if member.kind != ClassMemberKind::Method
                || !seen_names.insert(Arc::clone(&member.name))
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
    let interface_handle = resolve_exposed_interface_handle_with_scope_index(
        snapshot,
        scope_index,
        class_unit,
        class_symbol_id,
        access.scope,
        interface_name,
    )?;
    let member_path: Vec<_> = access.field_path[1..=segment_index]
        .iter()
        .map(|segment| segment.name.as_ref())
        .collect();
    let (member_unit, member) = resolve_interface_member_path(
        snapshot,
        scope_index,
        interface_handle.0,
        interface_handle.1,
        access.scope,
        &member_path,
    )?;
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
    (class_unit.symbol(class_symbol_id).kind == SymbolKind::Class).then_some((
        class_unit,
        class_symbol_id,
        false,
    ))
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

    let (structure_unit, structure_id) = resolve_symbol_structure_with_scope_index(
        snapshot,
        scope_index,
        unit,
        access.scope,
        symbol_id,
    )?;
    let field_path: Vec<_> = access
        .field_path
        .iter()
        .take(segment_index + 1)
        .map(|segment| segment.name.as_ref())
        .collect();
    let field = structure_unit
        .semantic()
        .decls()
        .resolve_structure_field_path(structure_id, &field_path)?;
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

fn selector_completion_context(
    parse: &ParseResult,
    offset: usize,
) -> Option<SelectorCursorContext> {
    let root = parse.file.root();
    let root_range = parse.file.range(root);
    let path = if root_range.start <= offset && offset <= root_range.end {
        let mut path = vec![root];
        let mut current = root;
        loop {
            let Some(next) = parse
                .file
                .children(current)
                .filter(|&child| {
                    let range = parse.file.range(child);
                    range.start <= offset && offset <= range.end
                })
                .min_by_key(|&child| {
                    let range = parse.file.range(child);
                    range.end.saturating_sub(range.start)
                })
            else {
                break;
            };
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
            | "CallStmt"
            | "MethodsStmt"
            | "MoveCorrespondingStmt"
            | "MoveStmt"
            | "SortStmt"
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
    let (source_start_sig, source_end_sig, where_sig) = if first.eq_ignore_ascii_case("delete") {
        let where_sig = significant.iter().position(|&idx| {
            parse.tokens[idx].kind.as_str() == "Ident"
                && parse.tokens[idx].lexeme(text).eq_ignore_ascii_case("where")
        })?;
        if where_sig <= 1 {
            return None;
        }
        (1usize, where_sig, where_sig)
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
        (2usize, source_end_sig, where_sig)
    } else {
        return None;
    };

    let where_idx = *significant.get(where_sig)?;
    let after_where = parse.tokens[where_idx].range.end;
    let statement_end = parse.tokens[*significant.last()?].range.end.max(offset);
    if offset < after_where || offset > statement_end {
        return None;
    }

    let source_tokens: Vec<usize> = significant[source_start_sig..source_end_sig]
        .iter()
        .copied()
        .filter(|&idx| parse.tokens[idx].range.end <= parse.tokens[where_idx].range.start)
        .collect();
    let source = parse_value_access_tokens(text, parse, &source_tokens)?;

    let prefix_token = significant
        .iter()
        .copied()
        .skip(where_sig + 1)
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

#[derive(Debug, Default)]
pub struct DocumentStore {
    documents: RwLock<HashMap<Arc<str>, Arc<AnalysisSnapshot>>>,
}

fn retain_local_analysis_state(unit: &mut UnitAnalysis) {
    unit.diagnostics.retain(|diagnostic| {
        matches!(
            diagnostic.kind,
            abap_symbols::DiagnosticKind::DuplicateDeclaration
                | abap_symbols::DiagnosticKind::ShadowedSymbol
        )
    });
    for edge in &mut unit.include_edges {
        edge.target = None;
    }
    for reference in &mut unit.references {
        match reference.resolution {
            Some(Resolution::Symbol(handle)) if handle.unit == unit.unit_id => {}
            Some(
                Resolution::BuiltinType
                | Resolution::BuiltinRoutine
                | Resolution::InternalTableLine,
            ) => {}
            _ => reference.resolution = None,
        }
    }
    unit.rebuild_semantic_index();
}

fn remap_local_unit_id(unit: &mut UnitAnalysis, unit_id: UnitId) {
    let previous_unit_id = unit.unit_id;
    if previous_unit_id == unit_id {
        return;
    }
    unit.unit_id = unit_id;
    for reference in &mut unit.references {
        if let Some(Resolution::Symbol(handle)) = &mut reference.resolution
            && handle.unit == previous_unit_id
        {
            handle.unit = unit_id;
        }
    }
    unit.rebuild_semantic_index();
}

fn reused_local_unit(snapshot: &AnalysisSnapshot, unit_id: UnitId) -> UnitAnalysis {
    let mut unit = snapshot.symbols.as_ref().clone();
    remap_local_unit_id(&mut unit, unit_id);
    retain_local_analysis_state(&mut unit);
    unit
}

fn staged_documents_for_publish(
    existing: &HashMap<Arc<str>, Arc<AnalysisSnapshot>>,
    uri: &Arc<str>,
    version: i32,
    text: &Arc<str>,
    parse: &Arc<ParseResult>,
) -> Vec<StagedDocument> {
    let mut staged =
        Vec::with_capacity(existing.len() + usize::from(!existing.contains_key(uri.as_ref())));
    let mut seen: HashSet<Arc<str>> = HashSet::new();

    if let Some(project) = existing
        .values()
        .next()
        .map(|snapshot| Arc::clone(&snapshot.project))
    {
        for unit in &project.units {
            if unit.uri.as_ref() == uri.as_ref() {
                staged.push(StagedDocument {
                    uri: Arc::clone(uri),
                    version,
                    text: Arc::clone(text),
                    parse: Arc::clone(parse),
                    previous: existing.get(uri.as_ref()).cloned(),
                });
                seen.insert(Arc::clone(uri));
                continue;
            }
            if let Some(snapshot) = existing.get(unit.uri.as_ref()) {
                staged.push(StagedDocument {
                    uri: Arc::clone(&snapshot.uri),
                    version: snapshot.version,
                    text: Arc::clone(&snapshot.text),
                    parse: Arc::clone(&snapshot.parse),
                    previous: Some(Arc::clone(snapshot)),
                });
                seen.insert(Arc::clone(&snapshot.uri));
            }
        }
    }

    for snapshot in existing.values() {
        if seen.insert(Arc::clone(&snapshot.uri)) {
            staged.push(StagedDocument {
                uri: Arc::clone(&snapshot.uri),
                version: snapshot.version,
                text: Arc::clone(&snapshot.text),
                parse: Arc::clone(&snapshot.parse),
                previous: Some(Arc::clone(snapshot)),
            });
        }
    }

    if seen.insert(Arc::clone(uri)) {
        staged.push(StagedDocument {
            uri: Arc::clone(uri),
            version,
            text: Arc::clone(text),
            parse: Arc::clone(parse),
            previous: existing.get(uri.as_ref()).cloned(),
        });
    }

    staged
}

fn analyze_inputs(inputs: &[DocumentInput]) -> HashMap<Arc<str>, Arc<AnalysisSnapshot>> {
    let parsed: Vec<_> = inputs
        .par_iter()
        .map(|input| {
            let analysis_text = analysis_text_for_input(input);
            let parse = Arc::new(parse(analysis_text.as_ref()));
            let source = analysis_text.to_string();
            (
                Arc::clone(&input.uri),
                input.version,
                input.text.clone(),
                input.object_name.clone(),
                parse,
                source,
            )
        })
        .collect();
    let units: Vec<_> = parsed
        .par_iter()
        .enumerate()
        .map(|(idx, (uri, _, _, object_name, parse, source))| {
            let mut unit =
                analyze_unit_locally(UnitId(idx as u32), Arc::clone(uri), source, parse.as_ref());
            if let Some(object_name) = object_name {
                unit.provided_names.push(Arc::clone(object_name));
                unit.provided_names.sort();
                unit.provided_names.dedup();
            }
            unit
        })
        .collect();
    let project = Arc::new(analyze_project_from_units(units));
    let mut snapshots = HashMap::with_capacity(parsed.len());
    for (uri, version, text, _, parse, _) in parsed {
        let unit = project
            .unit_by_uri(uri.as_ref())
            .cloned()
            .expect("project analysis should include every input document");
        snapshots.insert(
            Arc::clone(&uri),
            Arc::new(AnalysisSnapshot {
                scope_index: Arc::new(build_scope_index(&unit)),
                uri,
                version,
                text,
                parse,
                symbols: Arc::new(unit),
                project: Arc::clone(&project),
            }),
        );
    }
    snapshots
}

fn analysis_text_for_input(input: &DocumentInput) -> Arc<str> {
    if !input.is_dependency {
        return Arc::clone(&input.text);
    }
    dependency_surface_text(input.text.as_ref())
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

fn dependency_surface_text(text: &str) -> Arc<str> {
    let parsed = parse(text);
    let mut projected = text.as_bytes().to_vec();
    let tokens = &parsed.tokens;
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
                } else if !dependency_surface_keeps_statement(first) {
                    blank_range_preserving_layout(&mut projected, statement_range);
                }
                idx = period_idx + 1;
                continue;
            }
            Some(DependencyBlock::Form) => {
                if first == Some("endform") {
                    stack.pop();
                } else if !dependency_surface_keeps_statement(first) {
                    blank_range_preserving_layout(&mut projected, statement_range);
                }
                idx = period_idx + 1;
                continue;
            }
            Some(DependencyBlock::Function) => {
                if first == Some("endfunction") {
                    stack.pop();
                } else if !dependency_surface_keeps_statement(first) {
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
                    Some("include") => {}
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
                } else if first == Some("form") {
                    stack.push(DependencyBlock::Form);
                } else if first == Some("function") {
                    stack.push(DependencyBlock::Function);
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
            Some("form") => stack.push(DependencyBlock::Form),
            Some("function") => stack.push(DependencyBlock::Function),
            _ => {}
        }

        idx = period_idx + 1;
    }

    Arc::from(
        String::from_utf8(projected).expect("dependency surface projection should stay utf-8"),
    )
}

fn dependency_class_block_for_keywords(keywords: &[String]) -> Option<DependencyBlock> {
    if keywords.first().map(String::as_str) != Some("class") {
        return None;
    }
    if keywords.iter().any(|keyword| keyword == "implementation") {
        return Some(DependencyBlock::ClassImplementation);
    }
    if keywords.iter().any(|keyword| keyword == "definition") {
        return Some(DependencyBlock::ClassDefinition {
            visibility: DependencyVisibility::Private,
        });
    }
    None
}

fn dependency_surface_keeps_statement(first_keyword: Option<&str>) -> bool {
    matches!(first_keyword, Some("include"))
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
    pub fn replace_all(
        &self,
        inputs: Vec<DocumentInput>,
    ) -> HashMap<Arc<str>, Arc<AnalysisSnapshot>> {
        let rebuilt = analyze_inputs(&inputs);
        self.documents.write().clone_from(&rebuilt);
        rebuilt
    }

    pub fn publish(
        &self,
        uri: impl Into<Arc<str>>,
        version: i32,
        text: &str,
    ) -> Arc<AnalysisSnapshot> {
        let uri = uri.into();
        let text = Arc::<str>::from(text);
        let existing = self.documents.read();
        if let Some(current) = existing.get(uri.as_ref())
            && current.text.as_ref() == text.as_ref()
        {
            let snapshot = Arc::new(AnalysisSnapshot {
                scope_index: Arc::clone(&current.scope_index),
                uri: Arc::clone(&current.uri),
                version,
                text: Arc::clone(&current.text),
                parse: Arc::clone(&current.parse),
                symbols: Arc::clone(&current.symbols),
                project: Arc::clone(&current.project),
            });
            drop(existing);
            self.documents
                .write()
                .insert(Arc::clone(&uri), Arc::clone(&snapshot));
            return snapshot;
        }
        let parse = Arc::new(parse(&text));
        let staged = staged_documents_for_publish(&existing, &uri, version, &text, &parse);
        drop(existing);

        let units: Vec<_> = staged
            .iter()
            .enumerate()
            .map(|(idx, entry)| {
                let unit_id = UnitId(idx as u32);
                if entry.uri.as_ref() == uri.as_ref() {
                    analyze_unit_locally(
                        unit_id,
                        Arc::clone(&entry.uri),
                        entry.text.as_ref(),
                        entry.parse.as_ref(),
                    )
                } else {
                    reused_local_unit(
                        entry
                            .previous
                            .as_ref()
                            .expect("unchanged staged document should have prior snapshot"),
                        unit_id,
                    )
                }
            })
            .collect();
        let project = Arc::new(analyze_project_from_units(units));

        let mut rebuilt = HashMap::new();
        let mut published = None;
        for entry in staged {
            let unit = project
                .unit_by_uri(entry.uri.as_ref())
                .cloned()
                .expect("project analysis should include every published document");
            let snapshot = Arc::new(AnalysisSnapshot {
                scope_index: Arc::new(build_scope_index(&unit)),
                uri: Arc::clone(&entry.uri),
                version: entry.version,
                text: Arc::clone(&entry.text),
                parse: Arc::clone(&entry.parse),
                symbols: Arc::new(unit),
                project: Arc::clone(&project),
            });
            if entry.uri.as_ref() == uri.as_ref() {
                published = Some(Arc::clone(&snapshot));
            }
            rebuilt.insert(entry.uri, snapshot);
        }

        self.documents.write().clone_from(&rebuilt);
        published.expect("published snapshot should exist")
    }

    pub fn get(&self, uri: &str) -> Option<Arc<AnalysisSnapshot>> {
        self.documents.read().get(uri).cloned()
    }

    pub fn references(
        &self,
        uri: &str,
        offset: usize,
        include_declaration: bool,
    ) -> Option<Vec<ReferenceTarget>> {
        let snapshot = self.get(uri)?;
        let target = snapshot.reference_search_target_at(offset)?;
        let mut references: Vec<_> = self
            .documents
            .read()
            .values()
            .flat_map(|candidate| candidate.local_references_for_target(&target))
            .collect();
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
        DefinitionTarget, DocumentInput, DocumentStore, HoveredComponentKind, ReferenceTarget,
        ddic_xml_to_abap_source, dependency_surface_text,
    };
    use abap_symbols::{ReferenceKind, StructureFieldShape};
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
        assert_eq!(store.get("file:///demo.abap").unwrap().version, 2);
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
        assert_eq!(&src[def.range.clone()], "ty_demo");
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
    fn definition_at_returns_interface_method_declaration_for_selector() {
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
            .expect("interface method definition target");
        assert_target_slice(&target, "file:///demo.abap", src, "meth");
        assert_eq!(
            target.range.start,
            src.find("meth").expect("interface method declaration")
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
    fn definition_at_returns_form_parameter_declaration_for_perform_argument() {
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
        assert_target_slice(&target, "file:///demo.abap", src, "iv_input");
        assert_eq!(
            target.range.start,
            src.find("iv_input").expect("parameter declaration")
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
        let main_src = "DATA lv TYPE i.\nlv = 1.";
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
        let main_v1 = "DATA lv TYPE i.\nlv = 1.";
        let main_v2 = "DATA lv TYPE i.\nlv = 2.";
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
}
