use std::collections::{HashMap, HashSet};
use std::ops::Range;
use std::sync::Arc;

use abap_parser::{ParseResult, parse};
use abap_symbols::{
    ClassMemberData, ClassMemberKind, FieldTypeRefData, FormParameterData,
    FormParameterPassingKind, FormParameterSection, NamedArgumentAccess, NamedArgumentTarget,
    Namespace, PerformArgumentData, PerformCallData, PerformParameterSection, ProjectAnalysis,
    ReferenceKind, Resolution, ScopeId, SqlNameRefData, SqlNameRefKind, StructureFieldInfo,
    StructureFieldShape, StructureId, SymbolData, SymbolId, SymbolKind, UnitAnalysis, UnitId,
    Visibility, analyze_project_from_units, analyze_unit_locally, builtin_routine_spec,
};
use parking_lot::RwLock;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalysisSnapshot {
    pub uri: Arc<str>,
    pub version: i32,
    pub text: Arc<str>,
    pub parse: Arc<ParseResult>,
    pub symbols: Arc<UnitAnalysis>,
    pub project: Arc<ProjectAnalysis>,
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

fn sql_name_ref_at_offset(unit: &UnitAnalysis, offset: usize) -> Option<&SqlNameRefData> {
    unit.sql_name_refs
        .iter()
        .filter(|reference| reference.range.start <= offset && offset < reference.range.end)
        .min_by_key(|reference| reference.range.end.saturating_sub(reference.range.start))
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
    pub fn semantic_token_lookup_context(&self) -> SemanticTokenLookupContext<'_> {
        SemanticTokenLookupContext {
            snapshot: self,
            scope_index: build_scope_index(self.symbols.as_ref()),
        }
    }

    pub fn structure_field_infos(&self, structure_id: StructureId) -> Vec<StructureFieldInfo> {
        self.symbols.structure_field_infos(structure_id)
    }

    pub fn structure_field_info(
        &self,
        structure_id: StructureId,
        field_name: &str,
    ) -> Option<StructureFieldInfo> {
        self.symbols.structure_field_info(structure_id, field_name)
    }

    pub fn resolve_structure_field_path(
        &self,
        structure_id: StructureId,
        field_path: &[&str],
    ) -> Option<StructureFieldInfo> {
        self.symbols
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
        let (access, segment_index) = self.symbols.field_accesses.iter().find_map(|access| {
            access
                .field_path
                .iter()
                .enumerate()
                .find_map(|(idx, segment)| {
                    (segment.range.start <= offset && offset < segment.range.end)
                        .then_some((access, idx))
                })
        })?;
        let (unit, symbol_id) = resolve_field_access_base_symbol(self, access)?;
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
                declaration: Some(member.signature.to_string()),
                kind: hovered_component_kind_for_class_member(member),
                is_static_method: member.is_static,
                in_type_position: access.in_type_position,
            });
        }
        let symbol = unit.symbol(symbol_id);
        let structure_id = symbol.structure?;
        let field_path: Vec<_> = access
            .field_path
            .iter()
            .take(segment_index + 1)
            .map(|segment| segment.name.as_ref())
            .collect();
        let field = unit.resolve_structure_field_path(structure_id, &field_path)?;
        let kind = match field.shape {
            StructureFieldShape::Scalar => HoveredComponentKind::Scalar,
            StructureFieldShape::Structured { structure } => HoveredComponentKind::Structured {
                structure_name: Arc::clone(&unit.structure(structure).name),
            },
        };
        let field_owner_structure_name = Some(Arc::clone(&unit.structure(field.owner).name));
        Some(HoveredComponentInfo {
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
        })
    }

    pub fn classify_field_access_segment(
        &self,
        access: &abap_symbols::FieldAccess,
        segment_index: usize,
    ) -> Option<HoveredComponentKind> {
        let scope_index = build_scope_index(self.symbols.as_ref());
        classify_field_access_segment_with_scope_index(self, &scope_index, access, segment_index)
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
        let scope_index = build_scope_index(self.symbols.as_ref());
        resolve_named_argument_parameter_with_scope_index(self, &scope_index, access).is_some()
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
        let sql_ref = sql_name_ref_at_offset(self.symbols.as_ref(), offset)?;
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
        if let Some((reference, resolution)) = self
            .symbols
            .references
            .iter()
            .filter_map(|reference| {
                if reference.range.start <= offset && offset < reference.range.end {
                    reference
                        .resolution
                        .map(|resolution| (reference, resolution))
                } else {
                    None
                }
            })
            .min_by_key(|(reference, _)| reference.range.end.saturating_sub(reference.range.start))
        {
            return Some(HoveredSymbolInfo {
                range: reference.range.clone(),
                display_name: Arc::clone(&reference.name),
                markdown_lines: markdown_lines_for_resolution(self, &reference.name, resolution),
            });
        }

        if let Some(member) = self
            .symbols
            .class_members
            .iter()
            .filter(|member| member.decl_range.start <= offset && offset < member.decl_range.end)
            .min_by_key(|member| {
                member
                    .decl_range
                    .end
                    .saturating_sub(member.decl_range.start)
            })
        {
            return Some(HoveredSymbolInfo {
                range: member.decl_range.clone(),
                display_name: Arc::clone(&member.name),
                markdown_lines: markdown_lines_for_class_member(self.symbols.as_ref(), member),
            });
        }

        let symbol = self
            .symbols
            .symbols
            .iter()
            .filter(|symbol| symbol.decl_range.start <= offset && offset < symbol.decl_range.end)
            .min_by_key(|symbol| {
                symbol
                    .decl_range
                    .end
                    .saturating_sub(symbol.decl_range.start)
            })?;

        Some(HoveredSymbolInfo {
            range: symbol.decl_range.clone(),
            display_name: Arc::clone(&symbol.name),
            markdown_lines: markdown_lines_for_declared_symbol(self.symbols.as_ref(), symbol),
        })
    }

    fn definition_target_for_component_at(&self, offset: usize) -> Option<DefinitionTarget> {
        let (access, segment_index) = self.symbols.field_accesses.iter().find_map(|access| {
            access
                .field_path
                .iter()
                .enumerate()
                .find_map(|(idx, segment)| {
                    (segment.range.start <= offset && offset < segment.range.end)
                        .then_some((access, idx))
                })
        })?;
        let (unit, symbol_id) = resolve_field_access_base_symbol(self, access)?;
        if let Some((member_unit, member)) =
            resolve_class_selector_member(self, access, segment_index, unit, symbol_id)
        {
            return Some(definition_target_for_class_member(member_unit, member));
        }
        let symbol = unit.symbol(symbol_id);
        let structure_id = symbol.structure?;
        let field_path: Vec<_> = access
            .field_path
            .iter()
            .take(segment_index + 1)
            .map(|segment| segment.name.as_ref())
            .collect();
        let field = unit.resolve_structure_field_path(structure_id, &field_path)?;
        let decl_range = field.decl_range?;
        Some(definition_target_for_range(unit, decl_range))
    }

    fn reference_search_target_for_component_at(
        &self,
        offset: usize,
    ) -> Option<ReferenceSearchTarget> {
        let (access, segment_index) = self.symbols.field_accesses.iter().find_map(|access| {
            access
                .field_path
                .iter()
                .enumerate()
                .find_map(|(idx, segment)| {
                    (segment.range.start <= offset && offset < segment.range.end)
                        .then_some((access, idx))
                })
        })?;
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
        let symbol = unit.symbol(symbol_id);
        let structure_id = symbol.structure?;
        let field_path: Vec<_> = access
            .field_path
            .iter()
            .take(segment_index + 1)
            .map(|segment| segment.name.as_ref())
            .collect();
        let field = unit.resolve_structure_field_path(structure_id, &field_path)?;
        Some(ReferenceSearchTarget::StructField {
            unit: unit.unit_id,
            owner: field.owner,
            name: Arc::clone(&field.name),
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
        if let Some((reference, resolution)) = self
            .symbols
            .references
            .iter()
            .filter_map(|reference| {
                if reference.range.start <= offset && offset < reference.range.end {
                    reference
                        .resolution
                        .map(|resolution| (reference, resolution))
                } else {
                    None
                }
            })
            .min_by_key(|(reference, _)| reference.range.end.saturating_sub(reference.range.start))
        {
            return definition_target_for_resolution(self, resolution).or_else(|| {
                self.symbols
                    .symbols
                    .iter()
                    .filter(|symbol| symbol.decl_range == reference.range)
                    .min_by_key(|symbol| {
                        symbol
                            .decl_range
                            .end
                            .saturating_sub(symbol.decl_range.start)
                    })
                    .map(|symbol| definition_target_for_symbol(self.symbols.as_ref(), symbol))
            });
        }

        if let Some(member) = self
            .symbols
            .class_members
            .iter()
            .filter(|member| member.decl_range.start <= offset && offset < member.decl_range.end)
            .min_by_key(|member| {
                member
                    .decl_range
                    .end
                    .saturating_sub(member.decl_range.start)
            })
        {
            return Some(definition_target_for_class_member(
                self.symbols.as_ref(),
                member,
            ));
        }

        self.symbols
            .symbols
            .iter()
            .filter(|symbol| symbol.decl_range.start <= offset && offset < symbol.decl_range.end)
            .min_by_key(|symbol| {
                symbol
                    .decl_range
                    .end
                    .saturating_sub(symbol.decl_range.start)
            })
            .map(|symbol| definition_target_for_symbol(self.symbols.as_ref(), symbol))
    }

    fn reference_search_target_for_resolved_symbol_at(
        &self,
        offset: usize,
    ) -> Option<ReferenceSearchTarget> {
        if let Some((_, Resolution::Symbol(handle))) = self
            .symbols
            .references
            .iter()
            .filter_map(|reference| {
                if reference.range.start <= offset && offset < reference.range.end {
                    reference
                        .resolution
                        .map(|resolution| (reference, resolution))
                } else {
                    None
                }
            })
            .min_by_key(|(reference, _)| reference.range.end.saturating_sub(reference.range.start))
        {
            return Some(ReferenceSearchTarget::Symbol(handle));
        }

        if let Some(member) = self
            .symbols
            .class_members
            .iter()
            .filter(|member| member.decl_range.start <= offset && offset < member.decl_range.end)
            .min_by_key(|member| {
                member
                    .decl_range
                    .end
                    .saturating_sub(member.decl_range.start)
            })
        {
            return Some(ReferenceSearchTarget::ClassMember {
                unit: self.symbols.unit_id,
                class_symbol: member.class_symbol,
                name: Arc::clone(&member.name),
            });
        }

        if let Some(symbol) = self
            .symbols
            .symbols
            .iter()
            .filter(|symbol| symbol.decl_range.start <= offset && offset < symbol.decl_range.end)
            .min_by_key(|symbol| {
                symbol
                    .decl_range
                    .end
                    .saturating_sub(symbol.decl_range.start)
            })
        {
            return Some(ReferenceSearchTarget::Symbol(abap_symbols::SymbolHandle {
                unit: self.symbols.unit_id,
                symbol: symbol.id,
            }));
        }

        self.symbols
            .structures
            .iter()
            .flat_map(|structure| {
                structure.fields.iter().filter_map(move |field| {
                    let decl_range = field.decl_range.as_ref()?;
                    (decl_range.start <= offset && offset < decl_range.end).then_some(
                        ReferenceSearchTarget::StructField {
                            unit: self.symbols.unit_id,
                            owner: structure.id,
                            name: Arc::clone(&field.name),
                        },
                    )
                })
            })
            .min_by_key(|target| match target {
                ReferenceSearchTarget::StructField { owner, name, .. } => self
                    .symbols
                    .structure_field_info(*owner, name.as_ref())
                    .and_then(|field| field.decl_range)
                    .map(|range| range.end.saturating_sub(range.start))
                    .unwrap_or(usize::MAX),
                _ => usize::MAX,
            })
    }

    fn definition_target_for_sql_source_matching_type_ref(
        &self,
        offset: usize,
    ) -> Option<DefinitionTarget> {
        let sql_ref = sql_name_ref_at_offset(self.symbols.as_ref(), offset)?;
        if sql_ref.kind != SqlNameRefKind::Source {
            return None;
        }
        let name = sql_ref.name.as_ref();
        let unit = self.symbols.as_ref();
        unit.references
            .iter()
            .filter(|reference| {
                reference.namespace == Namespace::Type
                    && reference.kind == ReferenceKind::TypeRef
                    && reference.name.as_ref().eq_ignore_ascii_case(name)
            })
            .filter_map(|reference| {
                let resolution = reference.resolution.as_ref()?;
                let target = definition_target_for_resolution(self, *resolution)?;
                Some((
                    reference
                        .range
                        .end
                        .saturating_sub(reference.range.start),
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
        if let Some(sql_ref) = sql_name_ref_at_offset(unit, offset) {
            if sql_ref.kind == SqlNameRefKind::Source {
                return Some(ReferenceSearchTarget::DdLikeTypeName {
                    unit: unit.unit_id,
                    name: Arc::clone(&sql_ref.name),
                });
            }
        }

        let type_ref = unit
            .references
            .iter()
            .filter(|reference| {
                reference.range.start <= offset
                    && offset < reference.range.end
                    && reference.namespace == Namespace::Type
                    && reference.kind == ReferenceKind::TypeRef
            })
            .min_by_key(|reference| {
                reference
                    .range
                    .end
                    .saturating_sub(reference.range.start)
            })?;

        let name = &type_ref.name;
        let slash_name = name.as_ref().contains('/');
        let used_in_sql = unit.sql_sources.iter().any(|source| {
            source
                .name
                .as_ref()
                .eq_ignore_ascii_case(name.as_ref())
        });
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
            .references
            .iter()
            .filter(|reference| {
                reference.namespace == Namespace::Type
                    && reference.kind == ReferenceKind::TypeRef
                    && reference.name.as_ref().eq_ignore_ascii_case(name.as_ref())
            })
            .map(|reference| ReferenceTarget {
                uri: Arc::clone(&self.uri),
                range: reference.range.clone(),
            })
            .collect();
        out.extend(
            self.symbols
                .sql_name_refs
                .iter()
                .filter(|sql_ref| {
                    sql_ref.kind == SqlNameRefKind::Source
                        && sql_ref.name.as_ref().eq_ignore_ascii_case(name.as_ref())
                })
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
            .symbols
            .references
            .iter()
            .filter_map(|reference| match reference.resolution {
                Some(Resolution::Symbol(candidate)) if related_handles.contains(&candidate) => {
                    Some(ReferenceTarget {
                        uri: Arc::clone(&self.uri),
                        range: reference.range.clone(),
                    })
                }
                _ => None,
            })
            .collect();
        let symbol = self.project.units[handle.unit.as_usize()].symbol(handle.symbol);
        if symbol.kind == SymbolKind::Parameter {
            out.extend(self.local_named_argument_references_for_parameter(&related_handles));
            out.extend(self.local_perform_argument_references_for_parameter(&related_handles));
        }
        out
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
            let Some(structure_id) = unit.symbol(symbol_id).structure else {
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
                let Some(field) = unit.resolve_structure_field_path(structure_id, &field_path)
                else {
                    continue;
                };
                if unit.unit_id == target_unit && field.owner == owner && field.name == *name {
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
        let query = self.selector_completion_query_at(offset)?;
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
                    declaration: Some(member.signature.to_string()),
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
        let mut structure_id = unit.symbol(symbol_id).structure?;
        if !query.component_path.is_empty() {
            let path: Vec<_> = query
                .component_path
                .iter()
                .map(|part| part.as_ref())
                .collect();
            let field = unit.resolve_structure_field_path(structure_id, &path)?;
            structure_id = match field.shape {
                StructureFieldShape::Structured { structure } => structure,
                StructureFieldShape::Scalar => return None,
            };
        }

        let mut items: Vec<_> = unit
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
                            structure_name: Arc::clone(&unit.structure(structure).name),
                        }
                    }
                },
                field_owner_structure_name: Some(Arc::clone(&unit.structure(field.owner).name)),
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
        let context = selector_completion_context(&self.parse, offset)?;
        let query =
            parse_selector_completion_query(self.text.as_ref(), &self.parse, offset, &context)?;
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
        return Some(format_hover_type_clause(&format!("{keyword} {}", display.trim())));
    }
    if let Some(structure_id) = symbol.structure {
        let name = unit.structure(structure_id).name.as_ref();
        return Some(format_hover_type_clause(&format!("TYPE {name}")));
    }
    let type_ref = symbol.declared_type.as_ref()?;
    Some(format_hover_type_clause(&format_field_type_ref(type_ref)))
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
    let routine = unit.form_routine(symbol.id)?;
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
        format!("```abap\n{}\n```", member.signature),
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
            lines
        }
        Resolution::BuiltinType => vec![format!("`{at_name}`"), "Built-in ABAP type".to_string()],
        Resolution::BuiltinRoutine => markdown_lines_for_builtin_routine(at_name),
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

fn definition_target_for_range(unit: &UnitAnalysis, range: Range<usize>) -> DefinitionTarget {
    DefinitionTarget {
        uri: Arc::clone(&unit.uri),
        range,
    }
}

fn definition_target_for_resolution(
    snapshot: &AnalysisSnapshot,
    resolution: Resolution,
) -> Option<DefinitionTarget> {
    match resolution {
        Resolution::Symbol(handle) => {
            let unit = &snapshot.project.units[handle.unit.as_usize()];
            let symbol = unit.symbol(handle.symbol);
            Some(definition_target_for_symbol(unit, symbol))
        }
        Resolution::BuiltinType | Resolution::BuiltinRoutine | Resolution::External => None,
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
            let member = unit.class_member(*class_symbol, name.as_ref())?;
            Some(ReferenceTarget {
                uri: Arc::clone(&unit.uri),
                range: member.decl_range.clone(),
            })
        }
        ReferenceSearchTarget::StructField { unit, owner, name } => {
            let unit = &project.units[unit.as_usize()];
            let field = unit.structure_field_info(*owner, name.as_ref())?;
            Some(ReferenceTarget {
                uri: Arc::clone(&unit.uri),
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
    unit.symbols.iter().find_map(|symbol| {
        (symbol.kind == kind && symbol.decl_range == *range).then_some(abap_symbols::SymbolHandle {
            unit: unit.unit_id,
            symbol: symbol.id,
        })
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
            unit.routine_parameters(owner)
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
        if let Some(member) = unit.class_member(class_symbol, method_name.as_ref()) {
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
                unit.routine_parameters(method_symbol.id)
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
    let inheritance = unit.class_superclass(class_symbol)?;
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
    let scope_index = build_scope_index(snapshot.symbols.as_ref());
    resolve_field_access_base_symbol_with_scope_index(snapshot, &scope_index, access)
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
    let form_routine = unit.form_routine(form_symbol)?;
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
    let scope_index = build_scope_index(snapshot.symbols.as_ref());
    resolve_named_argument_parameter_with_scope_index(snapshot, &scope_index, access)
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
                .class_member(class_symbol_id, "constructor")?
                .parameters
                .iter()
                .find(|parameter| parameter.name == access.name)?;
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
                .class_member(class_symbol_id, "constructor")?
                .parameters
                .iter()
                .find(|parameter| parameter.name == access.name)?;
            Some(definition_target_for_range(unit, parameter.range.clone()))
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
                .class_member(class_symbol_id, "constructor")?
                .parameters
                .iter()
                .find(|parameter| parameter.name == access.name)?;
            symbol_handle_for_decl_range(unit, &parameter.range, SymbolKind::Parameter)
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
    let current_unit = &snapshot.symbols;
    let scope_index = build_scope_index(current_unit);
    resolve_symbol_from_context_with_scope_index(
        snapshot,
        &scope_index,
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
    let scope_index = build_scope_index(snapshot.symbols.as_ref());
    resolve_method_target_from_context_with_scope_index(
        snapshot,
        &scope_index,
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
    let inheritance = snapshot.symbols.class_superclass(class_symbol)?;
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
        if let Some(member) = unit.class_member(current.1, member_name) {
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
        for member in unit.class_members_for(current.1) {
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
    let scope_index = build_scope_index(snapshot.symbols.as_ref());
    resolve_class_selector_member_with_scope_index(
        snapshot,
        &scope_index,
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
    if segment_index != 0 {
        return None;
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

fn resolve_class_selector_base_with_scope_index<'a>(
    snapshot: &'a AnalysisSnapshot,
    scope_index: &ScopeIndex,
    access: &abap_symbols::FieldAccess,
    unit: &'a UnitAnalysis,
    symbol_id: SymbolId,
) -> Option<(&'a UnitAnalysis, SymbolId, bool)> {
    let base_symbol = unit.symbol(symbol_id);
    if access.base_namespace == Namespace::Type && base_symbol.kind == SymbolKind::Class {
        return Some((unit, symbol_id, true));
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

    let symbol = unit.symbol(symbol_id);
    let structure_id = symbol.structure?;
    let field_path: Vec<_> = access
        .field_path
        .iter()
        .take(segment_index + 1)
        .map(|segment| segment.name.as_ref())
        .collect();
    let field = unit.resolve_structure_field_path(structure_id, &field_path)?;
    Some(match field.shape {
        StructureFieldShape::Scalar => HoveredComponentKind::Scalar,
        StructureFieldShape::Structured { structure } => HoveredComponentKind::Structured {
            structure_name: Arc::clone(&unit.structure(structure).name),
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
    let mut path = Vec::new();
    let mut stack = vec![(parse.file.root(), Vec::new())];
    while let Some((node, mut current_path)) = stack.pop() {
        let range = parse.file.range(node);
        if !(range.start <= offset && offset <= range.end) {
            continue;
        }
        current_path.push(node);
        if current_path.len() > path.len() {
            path = current_path.clone();
        }
        let children: Vec<_> = parse.file.children(node).collect();
        for child in children.into_iter().rev() {
            stack.push((child, current_path.clone()));
        }
    }

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
        let (op_idx, op_kind) = selector_operator_before_token(parse, token_start, cursor)?;
        let ident_idx = previous_significant_token(parse, token_start, op_idx)?;
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

fn selector_operator_before_token(
    parse: &ParseResult,
    start: usize,
    end: usize,
) -> Option<(usize, SelectorOperator)> {
    let op_idx = previous_significant_token(parse, start, end)?;
    let op = &parse.tokens[op_idx];
    let left_idx = previous_significant_token(parse, start, op_idx)?;
    let left = &parse.tokens[left_idx];
    if left.kind.as_str() != "Ident" || left.range.end < op.range.start {
        return None;
    }

    let kind = match op.kind.as_str() {
        "Minus" => SelectorOperator::Minus,
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
    let Some(keyword_idx) = previous_significant_token(parse, start, base_idx) else {
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
            Some(Resolution::BuiltinType | Resolution::BuiltinRoutine) => {}
            _ => reference.resolution = None,
        }
    }
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

impl DocumentStore {
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
}

#[cfg(test)]
mod tests {
    use super::{DefinitionTarget, DocumentStore, HoveredComponentKind, ReferenceTarget};
    use abap_symbols::StructureFieldShape;
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
            hovered.markdown_lines.iter().any(|line| {
                line == "```abap\nTYPE STANDARD TABLE OF /sttp/gs1_gcp\n```"
            }),
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
