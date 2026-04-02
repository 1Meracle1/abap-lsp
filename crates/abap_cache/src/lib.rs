use std::collections::HashMap;
use std::ops::Range;
use std::sync::Arc;

use abap_parser::{ParseResult, parse};
use abap_symbols::{
    Namespace, ProjectAnalysis, ProjectInput, Resolution, ScopeId, StructureFieldInfo,
    StructureFieldShape, StructureId, SymbolData, SymbolId, SymbolKind, UnitAnalysis,
    analyze_project,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HoveredComponentKind {
    Scalar,
    Structured { structure_name: Arc<str> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HoveredComponentInfo {
    pub base_name: Arc<str>,
    pub component_path: Vec<Arc<str>>,
    pub field_name: Arc<str>,
    pub range: Range<usize>,
    pub declared_type: Option<String>,
    pub kind: HoveredComponentKind,
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
pub struct SelectorCompletionItem {
    pub name: Arc<str>,
    pub declared_type: Option<String>,
    pub kind: HoveredComponentKind,
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

type ScopeIndex = Vec<HashMap<(Namespace, Arc<str>), Vec<SymbolId>>>;

impl AnalysisSnapshot {
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
        Some(HoveredComponentInfo {
            base_name: Arc::clone(&access.base_name),
            component_path: access
                .field_path
                .iter()
                .take(segment_index + 1)
                .map(|segment| Arc::clone(&segment.name))
                .collect(),
            field_name: Arc::clone(&field.name),
            range: access.field_path[segment_index].range.clone(),
            declared_type: field.type_ref.as_ref().map(format_field_type_ref),
            kind,
            in_type_position: access.in_type_position,
        })
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

    pub fn selector_completion_at(&self, offset: usize) -> Option<SelectorCompletionInfo> {
        let query = self.selector_completion_query_at(offset)?;
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
                kind: match field.shape {
                    StructureFieldShape::Scalar => HoveredComponentKind::Scalar,
                    StructureFieldShape::Structured { structure } => {
                        HoveredComponentKind::Structured {
                            structure_name: Arc::clone(&unit.structure(structure).name),
                        }
                    }
                },
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

fn format_field_type_ref(type_ref: &abap_symbols::FieldTypeRefData) -> String {
    let keyword = match type_ref.namespace {
        Namespace::Type => "TYPE",
        Namespace::Value => "LIKE",
        Namespace::Routine => "TYPE",
    };
    let mut rendered = type_ref.base_name.to_string();
    for segment in &type_ref.field_path {
        rendered.push('-');
        rendered.push_str(segment.as_ref());
    }
    format!("{keyword} {rendered}")
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
    if let Some(structure_id) = symbol.structure {
        let name = unit.structure(structure_id).name.as_ref();
        return Some(format!("Declared as TYPE `{name}`"));
    }
    let type_ref = symbol.declared_type.as_ref()?;
    let keyword = match type_ref.namespace {
        Namespace::Type => "TYPE",
        Namespace::Value => "LIKE",
        Namespace::Routine => "TYPE",
    };
    let mut rendered = type_ref.base_name.to_string();
    for segment in &type_ref.field_path {
        rendered.push('-');
        rendered.push_str(segment.as_ref());
    }
    Some(format!("Declared as {keyword} `{rendered}`"))
}

fn markdown_lines_for_declared_symbol(unit: &UnitAnalysis, symbol: &SymbolData) -> Vec<String> {
    let mut lines = vec![
        format!("`{}`", symbol.name),
        symbol_kind_label(symbol.kind).to_string(),
    ];
    if let Some(type_line) = symbol_type_line(unit, symbol) {
        lines.push(type_line);
    }
    lines
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
        Resolution::BuiltinRoutine => {
            vec![format!("`{at_name}`"), "Built-in ABAP routine".to_string()]
        }
        Resolution::External => vec![
            format!("`{at_name}`"),
            "External reference (not resolved in this workspace)".to_string(),
        ],
    }
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
    resolve_symbol_from_context(
        snapshot,
        access.scope,
        access.base_namespace,
        &access.base_name,
        access.in_type_position,
    )
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
            | "TemplateExpr"
            | "BinaryExpr"
            | "UnaryExpr"
            | "ParenExpr"
            | "IsPredicate"
            | "InstanceOfPredicate"
            | "BetweenExpr"
            | "AssignStmt"
            | "SimpleStmt"
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

impl DocumentStore {
    pub fn publish(
        &self,
        uri: impl Into<Arc<str>>,
        version: i32,
        text: &str,
    ) -> Arc<AnalysisSnapshot> {
        let uri = uri.into();
        let text = Arc::<str>::from(text);
        let parse = Arc::new(parse(&text));

        let existing = self.documents.read();
        let mut staged: Vec<(Arc<str>, i32, Arc<str>, Arc<ParseResult>)> = existing
            .values()
            .map(|snapshot| {
                (
                    Arc::clone(&snapshot.uri),
                    snapshot.version,
                    Arc::clone(&snapshot.text),
                    Arc::clone(&snapshot.parse),
                )
            })
            .collect();
        drop(existing);

        if let Some(existing) = staged
            .iter_mut()
            .find(|(existing_uri, _, _, _)| existing_uri.as_ref() == uri.as_ref())
        {
            *existing = (
                Arc::clone(&uri),
                version,
                Arc::clone(&text),
                Arc::clone(&parse),
            );
        } else {
            staged.push((
                Arc::clone(&uri),
                version,
                Arc::clone(&text),
                Arc::clone(&parse),
            ));
        }

        let inputs: Vec<ProjectInput<'_>> = staged
            .iter()
            .map(|(uri, _, text, parse)| ProjectInput {
                uri: uri.as_ref(),
                source: text.as_ref(),
                parse,
            })
            .collect();
        let project = Arc::new(analyze_project(&inputs));

        let mut rebuilt = HashMap::new();
        let mut published = None;
        for (entry_uri, entry_version, entry_text, entry_parse) in staged {
            let unit = project
                .unit_by_uri(entry_uri.as_ref())
                .cloned()
                .expect("project analysis should include every published document");
            let snapshot = Arc::new(AnalysisSnapshot {
                uri: Arc::clone(&entry_uri),
                version: entry_version,
                text: entry_text,
                parse: entry_parse,
                symbols: Arc::new(unit),
                project: Arc::clone(&project),
            });
            if entry_uri.as_ref() == uri.as_ref() {
                published = Some(Arc::clone(&snapshot));
            }
            rebuilt.insert(entry_uri, snapshot);
        }

        self.documents.write().clone_from(&rebuilt);
        published.expect("published snapshot should exist")
    }

    pub fn get(&self, uri: &str) -> Option<Arc<AnalysisSnapshot>> {
        self.documents.read().get(uri).cloned()
    }

    pub fn len(&self) -> usize {
        self.documents.read().len()
    }
}

#[cfg(test)]
mod tests {
    use super::{DocumentStore, HoveredComponentKind};
    use abap_symbols::StructureFieldShape;

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
                .any(|line| line == "Declared as TYPE `i`"),
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
                .any(|line| line == "Declared as TYPE `i`"),
            "{:?}",
            hovered.markdown_lines
        );
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
}
