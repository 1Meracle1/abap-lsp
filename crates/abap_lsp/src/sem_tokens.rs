//! Semantic highlighting via LSP `textDocument/semanticTokens/full`, driven by `UnitAnalysis`.

use std::sync::OnceLock;

use abap_cache::AnalysisSnapshot;
use abap_symbols::{
    ProjectAnalysis, ReferenceData, ReferenceKind, Resolution, SqlNameRefKind, SymbolData,
    SymbolHandle, SymbolKind,
};
use lsp_types::{SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens};

/// Legend and type indices must stay aligned with [`semantic_tokens_legend`].
#[derive(Clone, Copy)]
struct SemanticTokenTypeIndices {
    type_: u32,
    class: u32,
    parameter: u32,
    variable: u32,
    property: u32,
    function: u32,
    method: u32,
    event: u32,
    namespace: u32,
}

const TOKEN_TYPE_INDICES: SemanticTokenTypeIndices = SemanticTokenTypeIndices {
    type_: 0,
    class: 1,
    parameter: 3,
    variable: 4,
    property: 5,
    function: 6,
    method: 7,
    event: 8,
    namespace: 9,
};

#[derive(Clone, Copy)]
struct ModifierIndices {
    declaration: u32,
    readonly: u32,
}

const MODIFIER_INDICES: ModifierIndices = ModifierIndices {
    declaration: 1 << 0,
    readonly: 1 << 1,
};

fn semantic_tokens_legend_static() -> &'static lsp_types::SemanticTokensLegend {
    static LEGEND: OnceLock<lsp_types::SemanticTokensLegend> = OnceLock::new();
    LEGEND.get_or_init(|| lsp_types::SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::TYPE,
            SemanticTokenType::CLASS,
            SemanticTokenType::INTERFACE,
            SemanticTokenType::PARAMETER,
            SemanticTokenType::VARIABLE,
            SemanticTokenType::PROPERTY,
            SemanticTokenType::FUNCTION,
            SemanticTokenType::METHOD,
            SemanticTokenType::EVENT,
            SemanticTokenType::NAMESPACE,
        ],
        token_modifiers: vec![
            SemanticTokenModifier::DECLARATION,
            SemanticTokenModifier::READONLY,
        ],
    })
}

pub fn semantic_tokens_legend() -> lsp_types::SemanticTokensLegend {
    semantic_tokens_legend_static().clone()
}

fn lookup_symbol<'a>(project: &'a ProjectAnalysis, handle: SymbolHandle) -> Option<&'a SymbolData> {
    let unit = project.units.get(handle.unit.as_usize())?;
    Some(unit.symbol(handle.symbol))
}

fn symbol_kind_type_index(kind: SymbolKind, ix: SemanticTokenTypeIndices) -> u32 {
    match kind {
        SymbolKind::BuiltinType | SymbolKind::TypeDef => ix.type_,
        SymbolKind::Class => ix.class,
        // Many client themes do not style the dedicated `interface` token kind.
        // Treat interfaces as general types so they render consistently.
        SymbolKind::Interface => ix.type_,
        SymbolKind::Parameter => ix.parameter,
        SymbolKind::Method => ix.method,
        SymbolKind::Form | SymbolKind::Module | SymbolKind::BuiltinRoutine => ix.function,
        SymbolKind::Event => ix.event,
        SymbolKind::Field => ix.property,
        SymbolKind::Variable
        | SymbolKind::FieldSymbol
        | SymbolKind::Constant
        | SymbolKind::BuiltinVariable
        | SymbolKind::BuiltinConstant
        | SymbolKind::Include
        | SymbolKind::Control
        | SymbolKind::Report => ix.variable,
    }
}

fn reference_fallback_type(reference: &ReferenceData, ix: SemanticTokenTypeIndices) -> u32 {
    match reference.kind {
        ReferenceKind::TypeRef | ReferenceKind::MessageClass | ReferenceKind::StaticTarget => {
            ix.type_
        }
        ReferenceKind::RoutineCall => ix.function,
        ReferenceKind::Include => ix.variable,
        ReferenceKind::Identifier => ix.variable,
    }
}

#[derive(Clone, Copy)]
struct PendingToken {
    start: usize,
    end: usize,
    /// Lower sorts earlier when spans tie-break (declaration beats use).
    priority: u8,
    token_type: u32,
    modifiers: u32,
}

fn push_pending(
    out: &mut Vec<PendingToken>,
    start: usize,
    end: usize,
    priority: u8,
    token_type: u32,
    modifiers: u32,
) {
    if start < end {
        out.push(PendingToken {
            start,
            end,
            priority,
            token_type,
            modifiers,
        });
    }
}

fn collect_pending(
    snapshot: &AnalysisSnapshot,
    ty_ix: SemanticTokenTypeIndices,
    mod_ix: ModifierIndices,
) -> Vec<PendingToken> {
    let lookup = snapshot.semantic_token_lookup_context();
    let unit = snapshot.symbols.as_ref();
    let project = snapshot.project.as_ref();
    let structure_fields = unit
        .structures
        .iter()
        .map(|structure| structure.fields.len())
        .sum::<usize>();
    let field_access_segments = unit
        .field_accesses
        .iter()
        .map(|access| access.field_path.len())
        .sum::<usize>();
    let mut pending = Vec::with_capacity(
        unit.symbols.len()
            + structure_fields
            + unit.class_members.len()
            + unit.references.len()
            + field_access_segments
            + unit.named_arguments.len()
            + unit.sql_name_refs.len(),
    );

    for symbol in &unit.symbols {
        let mut mods = mod_ix.declaration;
        if matches!(
            symbol.kind,
            SymbolKind::Constant | SymbolKind::BuiltinConstant
        ) {
            mods |= mod_ix.readonly;
        }
        push_pending(
            &mut pending,
            symbol.decl_range.start,
            symbol.decl_range.end,
            1,
            symbol_kind_type_index(symbol.kind, ty_ix),
            mods,
        );
    }

    for structure in &unit.structures {
        for field in &structure.fields {
            if field.decl_unit != unit.unit_id {
                continue;
            }
            let Some(decl_range) = field.decl_range.as_ref() else {
                continue;
            };
            push_pending(
                &mut pending,
                decl_range.start,
                decl_range.end,
                0,
                ty_ix.property,
                mod_ix.declaration,
            );
        }
    }

    for member in &unit.class_members {
        let token_type = match member.kind {
            abap_symbols::ClassMemberKind::Attribute => ty_ix.property,
            abap_symbols::ClassMemberKind::Method => ty_ix.method,
        };
        push_pending(
            &mut pending,
            member.decl_range.start,
            member.decl_range.end,
            0,
            token_type,
            mod_ix.declaration,
        );
    }

    for reference in &unit.references {
        let token_type = match &reference.resolution {
            Some(Resolution::Symbol(handle)) => lookup_symbol(project, *handle)
                .map(|symbol| symbol_kind_type_index(symbol.kind, ty_ix))
                .unwrap_or_else(|| reference_fallback_type(reference, ty_ix)),
            Some(Resolution::BuiltinType) => ty_ix.type_,
            Some(Resolution::BuiltinRoutine) => ty_ix.function,
            Some(Resolution::InternalTableLine) => ty_ix.property,
            Some(Resolution::External) => reference_fallback_type(reference, ty_ix),
            None => reference_fallback_type(reference, ty_ix),
        };
        push_pending(
            &mut pending,
            reference.range.start,
            reference.range.end,
            2,
            token_type,
            0,
        );
    }

    for access in &unit.field_accesses {
        for (segment_index, segment) in access.field_path.iter().enumerate() {
            let token_type = lookup
                .classify_field_access_segment(access, segment_index)
                .map(|kind| match kind {
                    abap_cache::HoveredComponentKind::Scalar => ty_ix.property,
                    abap_cache::HoveredComponentKind::Structured { .. } => ty_ix.property,
                    abap_cache::HoveredComponentKind::Attribute => ty_ix.property,
                    abap_cache::HoveredComponentKind::Method => ty_ix.method,
                    abap_cache::HoveredComponentKind::Interface => ty_ix.type_,
                })
                .unwrap_or(ty_ix.property);
            push_pending(
                &mut pending,
                segment.range.start,
                segment.range.end,
                3,
                token_type,
                0,
            );
        }
    }

    for named_argument in &unit.named_arguments {
        if lookup.has_named_argument_parameter(named_argument) {
            push_pending(
                &mut pending,
                named_argument.range.start,
                named_argument.range.end,
                3,
                ty_ix.parameter,
                0,
            );
        }
    }

    for sql_ref in &unit.sql_name_refs {
        let token_type = match sql_ref.kind {
            SqlNameRefKind::Source => ty_ix.namespace,
            SqlNameRefKind::Alias => ty_ix.variable,
            SqlNameRefKind::Column | SqlNameRefKind::QualifiedColumn => ty_ix.property,
            SqlNameRefKind::Star | SqlNameRefKind::QualifiedStar => ty_ix.type_,
            SqlNameRefKind::Aggregate => ty_ix.function,
        };
        push_pending(
            &mut pending,
            sql_ref.range.start,
            sql_ref.range.end,
            4,
            token_type,
            0,
        );
    }

    pending
}

/// Prefer non-overlapping spans; on overlap, keep the narrower span; same span favors lower `priority`.
fn merge_non_overlapping(mut pending: Vec<PendingToken>) -> Vec<(usize, usize, u32, u32)> {
    pending.sort_by(|a, b| {
        a.start
            .cmp(&b.start)
            .then(
                a.end
                    .saturating_sub(a.start)
                    .cmp(&b.end.saturating_sub(b.start)),
            )
            .then(a.priority.cmp(&b.priority))
    });

    let mut out: Vec<(usize, usize, u32, u32)> = Vec::with_capacity(pending.len());
    let mut last_end = 0usize;
    for token in pending {
        if token.start < last_end {
            continue;
        }
        last_end = token.end;
        out.push((token.start, token.end, token.token_type, token.modifiers));
    }
    out
}

#[derive(Clone, Copy, Default)]
struct TextCursor {
    byte_offset: usize,
    line: u32,
    character: u32,
}

fn advance_cursor_to(text: &str, cursor: &mut TextCursor, target: usize) -> Option<(u32, u32)> {
    if target < cursor.byte_offset || target > text.len() {
        return None;
    }
    let segment = text.get(cursor.byte_offset..target)?;
    for ch in segment.chars() {
        if ch == '\n' {
            cursor.line += 1;
            cursor.character = 0;
        } else {
            cursor.character += ch.len_utf16() as u32;
        }
    }
    cursor.byte_offset = target;
    Some((cursor.line, cursor.character))
}

fn encode_deltas(text: &str, merged: Vec<(usize, usize, u32, u32)>) -> Vec<SemanticToken> {
    let mut out = Vec::with_capacity(merged.len());
    let mut cursor = TextCursor::default();
    let mut prev_line = 0u32;
    let mut prev_char = 0u32;
    for (start, end, token_type, token_modifiers_bitset) in merged {
        let Some((line, character)) = advance_cursor_to(text, &mut cursor, start) else {
            continue;
        };
        let Some((end_line, end_character)) = advance_cursor_to(text, &mut cursor, end) else {
            continue;
        };
        if end_line != line {
            continue;
        }
        let length = end_character.saturating_sub(character);
        if length == 0 {
            continue;
        }
        let delta_line = line.saturating_sub(prev_line);
        let delta_start = if delta_line == 0 {
            character.saturating_sub(prev_char)
        } else {
            character
        };
        out.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type,
            token_modifiers_bitset,
        });

        prev_line = line;
        prev_char = character;
    }
    out
}

pub fn build_semantic_tokens(snapshot: &AnalysisSnapshot) -> SemanticTokens {
    let pending = collect_pending(snapshot, TOKEN_TYPE_INDICES, MODIFIER_INDICES);
    let merged = merge_non_overlapping(pending);
    let data = encode_deltas(snapshot.text.as_ref(), merged);
    SemanticTokens {
        result_id: None,
        data,
    }
}

#[cfg(test)]
mod tests {
    use super::{encode_deltas, semantic_tokens_legend};
    use crate::build_semantic_tokens;
    use abap_cache::{DocumentInput, DocumentStore};
    use lsp_types::{SemanticToken, SemanticTokenType};
    use std::sync::Arc;

    fn byte_offset_to_line_character_utf16_reference(
        text: &str,
        offset: usize,
    ) -> Option<(u32, u32)> {
        if offset > text.len() {
            return None;
        }
        let mut line = 0u32;
        let mut line_start = 0usize;
        for (idx, ch) in text.char_indices() {
            if idx >= offset {
                break;
            }
            if ch == '\n' {
                line += 1;
                line_start = idx + ch.len_utf8();
            }
        }
        let line_end = text[line_start..]
            .find('\n')
            .map(|rel| line_start + rel)
            .unwrap_or(text.len());
        let line_text = text[line_start..line_end]
            .strip_suffix('\r')
            .unwrap_or(&text[line_start..line_end]);
        if offset < line_start || offset > line_start + line_text.len() {
            return None;
        }
        let character = line_text[..offset - line_start]
            .chars()
            .map(|ch| ch.len_utf16() as u32)
            .sum();
        Some((line, character))
    }

    fn encode_deltas_reference(
        text: &str,
        merged: Vec<(usize, usize, u32, u32)>,
    ) -> Vec<lsp_types::SemanticToken> {
        let mut out = Vec::with_capacity(merged.len());
        let mut prev_line = 0u32;
        let mut prev_char = 0u32;
        for (start, end, token_type, token_modifiers_bitset) in merged {
            let Some((line, character)) =
                byte_offset_to_line_character_utf16_reference(text, start)
            else {
                continue;
            };
            let length: u32 = text[start..end]
                .chars()
                .map(|ch| ch.len_utf16() as u32)
                .sum();
            if length == 0 {
                continue;
            }
            let delta_line = line.saturating_sub(prev_line);
            let delta_start = if delta_line == 0 {
                character.saturating_sub(prev_char)
            } else {
                character
            };
            out.push(lsp_types::SemanticToken {
                delta_line,
                delta_start,
                length,
                token_type,
                token_modifiers_bitset,
            });
            prev_line = line;
            prev_char = character;
        }
        out
    }

    fn semantic_token_type_at(tokens: &[SemanticToken], line: u32, character: u32) -> Option<u32> {
        let mut current_line = 0u32;
        let mut current_char = 0u32;
        for token in tokens {
            current_line += token.delta_line;
            if token.delta_line == 0 {
                current_char += token.delta_start;
            } else {
                current_char = token.delta_start;
            }
            if current_line == line
                && current_char <= character
                && character < current_char + token.length
            {
                return Some(token.token_type);
            }
        }
        None
    }

    #[test]
    fn encode_deltas_matches_reference_for_unicode_and_crlf() {
        let text =
            "DATA lv.\r\nWRITE 'ab'.\r\nDATA lv_emoji TYPE string.\r\nlv_emoji = 'a😀b'.\r\n";
        let merged = vec![
            (5, 7, 4, 1),
            (17, 19, 6, 0),
            (28, 36, 4, 1),
            (51, 59, 4, 0),
            (63, 69, 4, 0),
        ];

        let actual = encode_deltas(text, merged.clone());
        let expected = encode_deltas_reference(text, merged);

        assert_eq!(actual, expected);
    }

    #[test]
    fn semantic_tokens_ignore_imported_structure_field_decl_ranges_from_other_units() {
        let store = DocumentStore::default();
        let dep_src = "\
TYPES: BEGIN OF zdep,
         field_a TYPE i,
         field_b TYPE i,
       END OF zdep.
";
        let main_src = "\
DATA ls_dep TYPE zdep.
CLASS lcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///dep.abap"),
                version: 1,
                text: Arc::from(dep_src),
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
        let tokens = build_semantic_tokens(snapshot.as_ref());
        let legend = semantic_tokens_legend();
        let property_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::PROPERTY)
            .expect("legend has property") as u32;

        let property_tokens: Vec<_> = tokens
            .data
            .iter()
            .filter(|token| token.token_type == property_idx)
            .collect();
        assert!(
            property_tokens.is_empty(),
            "expected no property tokens in consumer doc, got {property_tokens:?}"
        );
    }

    #[test]
    fn semantic_tokens_ignore_imported_loop_where_synthetic_decl_ranges() {
        let store = DocumentStore::default();
        let dep_src = "\
TYPES: BEGIN OF zdep,
         field_a TYPE i,
         field_b TYPE i,
       END OF zdep.
";
        let main_src = "\
DATA lt_dep TYPE STANDARD TABLE OF zdep WITH EMPTY KEY.
DATA total TYPE i.
LOOP AT lt_dep INTO DATA(ls_dep) WHERE field_a = 1.
  total = total + field_b.
ENDLOOP.
";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///dep.abap"),
                version: 1,
                text: Arc::from(dep_src),
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
        let tokens = build_semantic_tokens(snapshot.as_ref());

        assert_eq!(
            semantic_token_type_at(&tokens.data, 0, 29),
            None,
            "expected no semantic token on keyword area polluted by imported loop WHERE fields: {:?}",
            tokens.data
        );
    }

    #[test]
    fn semantic_tokens_ignore_imported_loop_where_proxy_include_synthetic_decl_ranges() {
        let store = DocumentStore::default();
        let dep_src = "\
TYPES: BEGIN OF s_dep_inc,
         field_a TYPE i,
         field_b TYPE i,
       END OF s_dep_inc.
TYPES: BEGIN OF s_dep_root,
         dep_inc TYPE s_dep_inc,
       END OF s_dep_root.
";
        let main_src = "\
TYPES: BEGIN OF ty_local_root,
         dep_root TYPE s_dep_root,
       END OF ty_local_root.
TYPES ty_local_tab TYPE STANDARD TABLE OF ty_local_root WITH EMPTY KEY.
DATA lt_dep TYPE ty_local_tab.
DATA total TYPE i.
LOOP AT lt_dep INTO DATA(ls_dep) WHERE field_a = 1.
  total = total + field_b.
ENDLOOP.
";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///dep.abap"),
                version: 1,
                text: Arc::from(dep_src),
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
        let tokens = build_semantic_tokens(snapshot.as_ref());

        assert_eq!(
            semantic_token_type_at(&tokens.data, 0, 29),
            None,
            "expected no semantic token on top-level keyword area polluted by proxy-include loop WHERE fields: {:?}",
            tokens.data
        );
    }

    #[test]
    fn semantic_tokens_mark_bapiret2_selector_fields_as_property() {
        let store = DocumentStore::default();
        let src = "\
FIELD-SYMBOLS <ls_return> TYPE bapiret2.
DATA(lv_msgty) = <ls_return>-type.
DATA(lv_msgid) = <ls_return>-id.
DATA(lv_msgno) = <ls_return>-number.
DATA(lv_msgv1) = <ls_return>-message_v1.
";
        let snapshot = store.publish("file:///bapiret2_selector_tokens.abap", 1, src);
        let tokens = build_semantic_tokens(snapshot.as_ref());
        let legend = semantic_tokens_legend();
        let property_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::PROPERTY)
            .expect("legend has property") as u32;

        for field_name in ["type", "id", "number", "message_v1"] {
            let field_offset = src
                .find(&format!("-{field_name}"))
                .map(|offset| offset + 1)
                .expect("field use");
            let (line, character) =
                byte_offset_to_line_character_utf16_reference(src, field_offset)
                    .expect("field position");
            assert_eq!(
                semantic_token_type_at(&tokens.data, line, character),
                Some(property_idx),
                "expected `{field_name}` to highlight as property"
            );
        }
    }

    #[test]
    fn semantic_tokens_mark_delete_comparing_fields_as_property() {
        let store = DocumentStore::default();
        let src = "\
FORM run.
  TYPES: BEGIN OF ty_row,
           matnr TYPE string,
           lgnum TYPE string,
         END OF ty_row.
  DATA lt_lqua TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

  DELETE ADJACENT DUPLICATES FROM lt_lqua COMPARING matnr lgnum.
ENDFORM.
";
        let snapshot = store.publish("file:///semantic_delete_comparing.abap", 1, src);
        let tokens = build_semantic_tokens(snapshot.as_ref());
        let legend = semantic_tokens_legend();
        let property_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::PROPERTY)
            .expect("legend has property") as u32;
        let mut search_from = src.find("COMPARING ").expect("COMPARING") + "COMPARING ".len();

        for field_name in ["matnr", "lgnum"] {
            let field_offset = src
                .get(search_from..)
                .and_then(|tail| tail.find(field_name).map(|offset| search_from + offset))
                .expect("field use");
            search_from = field_offset + field_name.len();
            let (line, character) =
                byte_offset_to_line_character_utf16_reference(src, field_offset)
                    .expect("field position");
            assert_eq!(
                semantic_token_type_at(&tokens.data, line, character),
                Some(property_idx),
                "expected `{field_name}` to highlight as property"
            );
        }
    }

    #[test]
    fn semantic_tokens_mark_read_table_with_key_fields_as_property() {
        let store = DocumentStore::default();
        let src = "\
FORM run.
  TYPES: BEGIN OF ty_row,
           vbeln TYPE string,
           posnn TYPE string,
         END OF ty_row.
  DATA t_vbfa TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
  DATA ls_vbfa TYPE ty_row.
  DATA us_ltap TYPE ty_row.

  READ TABLE t_vbfa INTO ls_vbfa WITH KEY vbeln = us_ltap-vbeln
                                          posnn = us_ltap-posnn.
ENDFORM.
";
        let snapshot = store.publish("file:///semantic_read_table_with_key.abap", 1, src);
        let tokens = build_semantic_tokens(snapshot.as_ref());
        let legend = semantic_tokens_legend();
        let property_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::PROPERTY)
            .expect("legend has property") as u32;
        let key_offset = src.find("WITH KEY ").expect("WITH KEY") + "WITH KEY ".len();

        for field_name in ["vbeln", "posnn"] {
            let field_offset = src[key_offset..]
                .find(field_name)
                .map(|offset| key_offset + offset)
                .expect("field use");
            let (line, character) =
                byte_offset_to_line_character_utf16_reference(src, field_offset)
                    .expect("field position");
            assert_eq!(
                semantic_token_type_at(&tokens.data, line, character),
                Some(property_idx),
                "expected `{field_name}` to highlight as property"
            );
        }
    }

    #[test]
    fn semantic_tokens_mark_modify_transporting_and_where_fields_as_property() {
        let store = DocumentStore::default();
        let src = "\
FORM run.
  TYPES: BEGIN OF ty_row,
           low TYPE string,
           sign TYPE string,
           option TYPE string,
         END OF ty_row.
  DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
  DATA ls_row TYPE ty_row.

  MODIFY lt_rows FROM ls_row
    TRANSPORTING sign option
    WHERE low IS NOT INITIAL
      AND sign IS INITIAL
      AND option IS INITIAL.
ENDFORM.
";
        let snapshot = store.publish("file:///semantic_modify_transporting_where.abap", 1, src);
        let tokens = build_semantic_tokens(snapshot.as_ref());
        let legend = semantic_tokens_legend();
        let property_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::PROPERTY)
            .expect("legend has property") as u32;

        let transporting_offset = src.find("TRANSPORTING ").expect("TRANSPORTING");
        let transporting_start = transporting_offset + "TRANSPORTING ".len();
        let where_offset = src.find("WHERE ").expect("WHERE");
        let where_start = where_offset + "WHERE ".len();

        for (field_name, start_offset) in [
            ("sign", transporting_start),
            ("option", transporting_start),
            ("low", where_start),
            ("sign", where_start),
            ("option", where_start),
        ] {
            let field_offset = src[start_offset..]
                .find(field_name)
                .map(|offset| start_offset + offset)
                .expect("field use");
            let (line, character) =
                byte_offset_to_line_character_utf16_reference(src, field_offset)
                    .expect("field position");
            assert_eq!(
                semantic_token_type_at(&tokens.data, line, character),
                Some(property_idx),
                "expected `{field_name}` to highlight as property"
            );
        }
    }

    #[test]
    fn semantic_tokens_do_not_mark_open_sql_eq_operator_as_property() {
        let store = DocumentStore::default();
        let src = "\
FORM run.
  TYPES: BEGIN OF ty_key,
           matnr TYPE string,
           lgnum TYPE string,
         END OF ty_key.
  DATA lt_lqua TYPE STANDARD TABLE OF ty_key WITH EMPTY KEY.

  SELECT matnr
         lgnum
    FROM mlgn
    INTO TABLE @DATA(lt_mlgn)
    FOR ALL ENTRIES IN lt_lqua
    WHERE matnr EQ lt_lqua-matnr
      AND lgnum EQ lt_lqua-lgnum.
ENDFORM.
";
        let snapshot = store.publish("file:///semantic_sql_eq.abap", 1, src);
        let tokens = build_semantic_tokens(snapshot.as_ref());
        let legend = semantic_tokens_legend();
        let property_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::PROPERTY)
            .expect("legend has property") as u32;

        let where_matnr_offset = src
            .find("WHERE matnr")
            .map(|offset| offset + "WHERE ".len())
            .expect("WHERE matnr offset");
        let eq_offset = src.find("EQ lt_lqua-matnr").expect("EQ offset");
        let (where_matnr_line, where_matnr_char) =
            byte_offset_to_line_character_utf16_reference(src, where_matnr_offset)
                .expect("WHERE matnr position");
        let (eq_line, eq_char) =
            byte_offset_to_line_character_utf16_reference(src, eq_offset).expect("EQ position");

        assert_eq!(
            semantic_token_type_at(&tokens.data, where_matnr_line, where_matnr_char),
            Some(property_idx),
            "expected Open SQL column in WHERE to highlight as property"
        );
        assert_ne!(
            semantic_token_type_at(&tokens.data, eq_line, eq_char),
            Some(property_idx),
            "EQ operator must not highlight as Open SQL column/property"
        );
    }

    #[test]
    fn semantic_tokens_cover_function_module_type_refs_and_selectors() {
        let store = DocumentStore::default();
        let src = "\
FUNCTION z_demo.
  DATA:
    lr_data      TYPE REF TO data,
    lr_data_row  TYPE REF TO data,
    lt_return    TYPE bapiret2_t.

  FIELD-SYMBOLS:
    <ls_return> TYPE bapiret2.

  LOOP AT lt_return ASSIGNING <ls_return>.
    DATA(lv_msgty) = <ls_return>-type.
    DATA(lv_msgid) = <ls_return>-id.
    DATA(lv_msgno) = <ls_return>-number.
  ENDLOOP.
ENDFUNCTION.
";
        let snapshot = store.publish("file:///function_module_sem_tokens.abap", 1, src);
        let tokens = build_semantic_tokens(snapshot.as_ref());
        let legend = semantic_tokens_legend();
        let type_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::TYPE)
            .expect("legend has type") as u32;
        let property_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::PROPERTY)
            .expect("legend has property") as u32;

        for (needle, expected) in [
            ("TYPE REF TO data", type_idx),
            ("TYPE bapiret2_t", type_idx),
            ("TYPE bapiret2", type_idx),
            ("-type", property_idx),
            ("-id", property_idx),
            ("-number", property_idx),
        ] {
            let offset = src
                .find(needle)
                .map(|base| {
                    if needle.starts_with('-') {
                        base + 1
                    } else {
                        base + needle.rfind(' ').unwrap_or(0) + 1
                    }
                })
                .expect("needle offset");
            let (line, character) =
                byte_offset_to_line_character_utf16_reference(src, offset).expect("position");
            assert_eq!(
                semantic_token_type_at(&tokens.data, line, character),
                Some(expected),
                "expected semantic token for `{needle}`"
            );
        }
    }

    #[test]
    fn dependency_snapshot_keeps_semantic_tokens_for_public_methods_after_class_methods() {
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
        let tokens = build_semantic_tokens(snapshot.as_ref());
        let legend = semantic_tokens_legend();
        let method_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::METHOD)
            .expect("legend has method") as u32;
        let decl_mod = 1u32
            << legend
                .token_modifiers
                .iter()
                .position(|m| m.as_str() == "declaration")
                .expect("declaration modifier");

        let method_tokens: Vec<_> = tokens
            .data
            .iter()
            .filter(|token| {
                token.token_type == method_idx && (token.token_modifiers_bitset & decl_mod) != 0
            })
            .collect();
        assert!(
            method_tokens.len() >= 4,
            "expected dependency method declaration tokens, got {method_tokens:?}"
        );
    }

    #[test]
    fn semantic_tokens_mark_interface_qualifier_in_qualified_call_as_type() {
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
        let tokens = build_semantic_tokens(snapshot.as_ref());
        let legend = semantic_tokens_legend();
        let type_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::TYPE)
            .expect("legend has type") as u32;
        let method_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::METHOD)
            .expect("legend has method") as u32;

        let call_line = src
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("lo_obj->i1~meth"))
            .expect("qualified call line");
        let i1_col = call_line.1.find("i1").expect("interface qualifier column") as u32;
        let meth_col = call_line.1.find("meth").expect("method column") as u32;

        assert_eq!(
            semantic_token_type_at(&tokens.data, call_line.0 as u32, i1_col),
            Some(type_idx)
        );
        assert_eq!(
            semantic_token_type_at(&tokens.data, call_line.0 as u32, meth_col),
            Some(method_idx)
        );
    }

    #[test]
    fn semantic_tokens_mark_interface_names_in_declarations_and_impl_headers_as_type() {
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
        let tokens = build_semantic_tokens(snapshot.as_ref());
        let legend = semantic_tokens_legend();
        let type_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::TYPE)
            .expect("legend has type") as u32;

        let interfaces_line = src
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("INTERFACES /sttp/if_badi_rule_processing"))
            .expect("interfaces line");
        let interfaces_col = interfaces_line
            .1
            .find("/sttp/if_badi_rule_processing")
            .expect("interfaces col") as u32;
        assert_eq!(
            semantic_token_type_at(&tokens.data, interfaces_line.0 as u32, interfaces_col),
            Some(type_idx)
        );

        let impl_line = src
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains("METHOD /sttp/if_badi_rule_processing~execute"))
            .expect("impl line");
        let impl_col = impl_line
            .1
            .find("/sttp/if_badi_rule_processing")
            .expect("impl col") as u32;
        assert_eq!(
            semantic_token_type_at(&tokens.data, impl_line.0 as u32, impl_col),
            Some(type_idx)
        );
    }

    #[test]
    fn semantic_tokens_skip_parenthesized_legacy_call_method_section_keywords() {
        let store = DocumentStore::default();
        let src = "\
CLASS zcl_demo DEFINITION.
  PUBLIC SECTION.
    METHODS populate_codes
      IMPORTING iv_rule_type TYPE string
                is_req_data TYPE string
      EXPORTING et_kodovi TYPE stringtab
                et_kod_all TYPE stringtab.
    METHODS exec.
ENDCLASS.

CLASS zcl_demo IMPLEMENTATION.
  METHOD populate_codes.
  ENDMETHOD.

  METHOD exec.
    DATA iv_rule_type TYPE string.
    FIELD-SYMBOLS <fs_req_data> TYPE string.
    ASSIGN iv_rule_type TO <fs_req_data>.
    IF <fs_req_data> IS ASSIGNED.
      CALL METHOD populate_codes(
        EXPORTING
          iv_rule_type = iv_rule_type
          is_req_data  = <fs_req_data>
        IMPORTING
          et_kodovi    = DATA(lt_kodovi)
          et_kod_all   = DATA(lt_kodovi_all) ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.";
        let snapshot = store.publish("file:///legacy_parenthesized_call_method.abap", 1, src);
        let tokens = build_semantic_tokens(snapshot.as_ref());

        for keyword in ["EXPORTING", "IMPORTING"] {
            let offset = src.rfind(keyword).expect("keyword offset");
            let (line, character) = byte_offset_to_line_character_utf16_reference(src, offset)
                .expect("keyword position");
            assert_eq!(
                semantic_token_type_at(&tokens.data, line, character),
                None,
                "expected no semantic token on CALL METHOD section keyword `{keyword}`: {:?}",
                tokens.data
            );
        }
    }

    #[test]
    fn semantic_tokens_mark_call_function_parameter_and_exception_names() {
        let store = DocumentStore::default();
        let dep_src = "\
FUNCTION z_demo_call
  IMPORTING
    iv_name TYPE string
  CHANGING
    cv_text TYPE string
  EXCEPTIONS
    failed.
ENDFUNCTION.
";
        let main_src = "\
START-OF-SELECTION.
  DATA lv_name TYPE string.
  DATA lv_text TYPE string.
  CALL FUNCTION 'z_demo_call'
    EXPORTING
      iv_name = lv_name
    CHANGING
      cv_text = lv_text
    EXCEPTIONS
      failed = 1.
";
        let snapshots = store.replace_all(vec![
            DocumentInput {
                uri: Arc::from("file:///function_sem_tokens_dep.abap"),
                version: 1,
                text: Arc::from(dep_src),
                is_dependency: true,
                object_name: None,
            },
            DocumentInput {
                uri: Arc::from("file:///function_sem_tokens_main.abap"),
                version: 1,
                text: Arc::from(main_src),
                is_dependency: false,
                object_name: None,
            },
        ]);
        let snapshot = snapshots
            .get("file:///function_sem_tokens_main.abap")
            .expect("main snapshot");
        let tokens = build_semantic_tokens(snapshot.as_ref());
        let legend = semantic_tokens_legend();
        let parameter_idx = legend
            .token_types
            .iter()
            .position(|t| *t == SemanticTokenType::PARAMETER)
            .expect("legend has parameter") as u32;

        for needle in ["iv_name", "cv_text", "failed"] {
            let offset = main_src.find(needle).expect("needle offset");
            let (line, character) =
                byte_offset_to_line_character_utf16_reference(main_src, offset + 1)
                    .expect("needle position");
            assert_eq!(
                semantic_token_type_at(&tokens.data, line, character),
                Some(parameter_idx),
                "expected CALL FUNCTION name `{needle}` to highlight as parameter"
            );
        }
    }
}
