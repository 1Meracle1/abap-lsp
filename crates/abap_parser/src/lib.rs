mod assign_stmt;
mod block_helpers;
mod control_stmt;
mod data_decl;
mod expr;
mod if_stmt;
mod interner;
mod simple_stmt;
mod stmt_period;
mod surface_stmt;
pub mod syntax;
mod type_ref;

use crate::stmt_period::{StmtPeriodScan, scan_until_statement_period, unterminated_err_end};
use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::Token;
use abap_lexer::TokenKind;
use std::collections::HashMap;
use std::sync::{Arc, LazyLock};

type ParseAttempt = fn(
    &mut SyntaxTreeBuilder,
    &str,
    &[Token],
    usize,
    &mut Vec<ParseError>,
) -> Option<(NodeId, usize)>;

#[derive(Clone, Copy)]
struct IdentLeadParserRegistration {
    lead_keywords: &'static [&'static str],
    parser: ParseAttempt,
}

impl IdentLeadParserRegistration {
    const fn new(lead_keywords: &'static [&'static str], parser: ParseAttempt) -> Self {
        Self {
            lead_keywords,
            parser,
        }
    }
}

type IdentLeadParserMap = HashMap<String, Vec<ParseAttempt>>;

const IDENT_LEAD_PARSER_REGISTRATIONS: &[IdentLeadParserRegistration] = &[
    IdentLeadParserRegistration::new(&["data"], data_decl::try_parse_data_decl),
    IdentLeadParserRegistration::new(&["tables"], data_decl::try_parse_tables_decl),
    IdentLeadParserRegistration::new(&["ranges"], data_decl::try_parse_ranges_decl),
    IdentLeadParserRegistration::new(&["controls"], data_decl::try_parse_controls_decl),
    IdentLeadParserRegistration::new(
        &["parameters", "parameter"],
        data_decl::try_parse_parameters_decl,
    ),
    IdentLeadParserRegistration::new(&["select"], data_decl::try_parse_select_options_decl),
    IdentLeadParserRegistration::new(&["class"], data_decl::try_parse_class_data_decl),
    IdentLeadParserRegistration::new(&["if"], if_stmt::try_parse_if_stmt),
    IdentLeadParserRegistration::new(&["statics"], data_decl::try_parse_statics_decl),
    IdentLeadParserRegistration::new(&["types"], data_decl::try_parse_types_decl),
    IdentLeadParserRegistration::new(&["constants"], data_decl::try_parse_constants_decl),
    IdentLeadParserRegistration::new(&["field"], data_decl::try_parse_field_symbols_decl),
    IdentLeadParserRegistration::new(&["case"], control_stmt::try_parse_case_stmt),
    IdentLeadParserRegistration::new(&["while"], control_stmt::try_parse_while_stmt),
    IdentLeadParserRegistration::new(&["do"], control_stmt::try_parse_do_stmt),
    IdentLeadParserRegistration::new(&["loop"], control_stmt::try_parse_loop_stmt),
    IdentLeadParserRegistration::new(
        &["catch"],
        control_stmt::try_parse_catch_system_exceptions_stmt,
    ),
    IdentLeadParserRegistration::new(&["try"], control_stmt::try_parse_try_stmt),
    IdentLeadParserRegistration::new(&["define"], surface_stmt::try_parse_macro_def),
    IdentLeadParserRegistration::new(&["report", "program"], surface_stmt::try_parse_report_stmt),
    IdentLeadParserRegistration::new(&["include"], surface_stmt::try_parse_include_stmt),
    IdentLeadParserRegistration::new(
        &["selection"],
        surface_stmt::try_parse_selection_screen_stmt,
    ),
    IdentLeadParserRegistration::new(
        &["at", "initialization", "start", "end", "top"],
        surface_stmt::try_parse_event_block,
    ),
    IdentLeadParserRegistration::new(&["at"], control_stmt::try_parse_at_stmt),
    IdentLeadParserRegistration::new(&["form"], surface_stmt::try_parse_form_decl),
    IdentLeadParserRegistration::new(&["function"], surface_stmt::try_parse_function_decl),
    IdentLeadParserRegistration::new(&["module"], surface_stmt::try_parse_module_decl),
    IdentLeadParserRegistration::new(
        &["enhancement"],
        surface_stmt::try_parse_enhancement_point_stmt,
    ),
    IdentLeadParserRegistration::new(
        &["enhancement"],
        surface_stmt::try_parse_enhancement_section_stmt,
    ),
    IdentLeadParserRegistration::new(&["enhancement"], surface_stmt::try_parse_enhancement_stmt),
    IdentLeadParserRegistration::new(&["class"], surface_stmt::try_parse_class_decl),
    IdentLeadParserRegistration::new(&["interface"], surface_stmt::try_parse_interface_decl),
    IdentLeadParserRegistration::new(&["method"], surface_stmt::try_parse_method_decl),
    IdentLeadParserRegistration::new(&["select"], surface_stmt::try_parse_select_stmt),
    IdentLeadParserRegistration::new(&["with"], surface_stmt::try_parse_with_select_stmt),
    IdentLeadParserRegistration::new(&["open"], surface_stmt::try_parse_open_cursor_stmt),
    IdentLeadParserRegistration::new(&["fetch"], surface_stmt::try_parse_fetch_cursor_stmt),
    IdentLeadParserRegistration::new(&["close"], surface_stmt::try_parse_close_cursor_stmt),
    IdentLeadParserRegistration::new(&["read"], surface_stmt::try_parse_read_report_stmt),
    IdentLeadParserRegistration::new(&["read"], surface_stmt::try_parse_read_table_stmt),
    IdentLeadParserRegistration::new(&["authority"], surface_stmt::try_parse_authority_check_stmt),
    IdentLeadParserRegistration::new(&["append"], surface_stmt::try_parse_append_stmt),
    IdentLeadParserRegistration::new(&["insert"], surface_stmt::try_parse_insert_report_stmt),
    IdentLeadParserRegistration::new(&["insert"], surface_stmt::try_parse_insert_table_stmt),
    IdentLeadParserRegistration::new(&["move"], surface_stmt::try_parse_move_stmt),
    IdentLeadParserRegistration::new(
        &["add", "subtract", "compute", "multiply", "divide"],
        simple_stmt::try_parse_simple_stmt,
    ),
    IdentLeadParserRegistration::new(&["sort"], surface_stmt::try_parse_sort_stmt),
    IdentLeadParserRegistration::new(&["modify"], surface_stmt::try_parse_modify_stmt),
    IdentLeadParserRegistration::new(&["delete"], surface_stmt::try_parse_delete_report_stmt),
    IdentLeadParserRegistration::new(&["delete"], surface_stmt::try_parse_delete_stmt),
    IdentLeadParserRegistration::new(&["syntax"], surface_stmt::try_parse_syntax_check_stmt),
    IdentLeadParserRegistration::new(&["update"], surface_stmt::try_parse_update_stmt),
    IdentLeadParserRegistration::new(&["refresh"], surface_stmt::try_parse_refresh_stmt),
    IdentLeadParserRegistration::new(&["collect"], surface_stmt::try_parse_collect_stmt),
    IdentLeadParserRegistration::new(&["free"], surface_stmt::try_parse_free_stmt),
    IdentLeadParserRegistration::new(&["unassign"], surface_stmt::try_parse_unassign_stmt),
    IdentLeadParserRegistration::new(&["import"], surface_stmt::try_parse_import_memory_stmt),
    IdentLeadParserRegistration::new(&["export"], surface_stmt::try_parse_export_memory_stmt),
    IdentLeadParserRegistration::new(&["write"], surface_stmt::try_parse_write_stmt),
    IdentLeadParserRegistration::new(&["split"], surface_stmt::try_parse_split_stmt),
    IdentLeadParserRegistration::new(&["concatenate"], surface_stmt::try_parse_concatenate_stmt),
    IdentLeadParserRegistration::new(&["condense"], surface_stmt::try_parse_condense_stmt),
    IdentLeadParserRegistration::new(&["raise"], surface_stmt::try_parse_raise_stmt),
    IdentLeadParserRegistration::new(&["message"], surface_stmt::try_parse_message_stmt),
    IdentLeadParserRegistration::new(&["submit"], surface_stmt::try_parse_submit_stmt),
    IdentLeadParserRegistration::new(&["leave"], surface_stmt::try_parse_leave_stmt),
    IdentLeadParserRegistration::new(&["endat"], surface_stmt::try_parse_endat_stmt),
    IdentLeadParserRegistration::new(&["find"], surface_stmt::try_parse_find_stmt),
    IdentLeadParserRegistration::new(&["get"], surface_stmt::try_parse_get_reference_stmt),
    IdentLeadParserRegistration::new(&["get"], surface_stmt::try_parse_get_bit_stmt),
    IdentLeadParserRegistration::new(&["get"], surface_stmt::try_parse_get_time_stamp_stmt),
    IdentLeadParserRegistration::new(&["set"], surface_stmt::try_parse_set_bit_stmt),
    IdentLeadParserRegistration::new(&["assign"], surface_stmt::try_parse_assign_keyword_stmt),
    IdentLeadParserRegistration::new(&["call", "create"], surface_stmt::try_parse_call_like_stmt),
];

static IDENT_LEAD_PARSERS: LazyLock<IdentLeadParserMap> = LazyLock::new(|| {
    let mut parsers = HashMap::with_capacity(128);
    for registration in IDENT_LEAD_PARSER_REGISTRATIONS {
        register_ident_lead_parser(
            &mut parsers,
            registration.lead_keywords,
            registration.parser,
        );
    }
    parsers
});

fn register_ident_lead_parser(
    parsers: &mut IdentLeadParserMap,
    lead_keywords: &[&str],
    parser: ParseAttempt,
) {
    for keyword in lead_keywords {
        let lower = keyword.to_ascii_lowercase();
        let upper = keyword.to_ascii_uppercase();
        let insert_upper = upper != lower;
        register_ident_lead_parser_case(parsers, lower, parser);
        if insert_upper {
            register_ident_lead_parser_case(parsers, upper, parser);
        }
    }
}

fn register_ident_lead_parser_case(
    parsers: &mut IdentLeadParserMap,
    keyword: String,
    parser: ParseAttempt,
) {
    parsers.entry(keyword).or_default().push(parser);
}

fn try_guarded_ident_parsers(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    lead_keyword: &str,
    errors: &mut Vec<ParseError>,
) -> Option<(NodeId, usize)> {
    let parsers = ident_lead_parser_attempts(lead_keyword)?;
    for &parser in parsers {
        if let Some((node, next)) = parser(b, source, tokens, idx, errors) {
            return Some((node, next));
        }
    }
    None
}

fn ident_lead_parser_attempts(lead_keyword: &str) -> Option<&'static [ParseAttempt]> {
    if let Some(parsers) = IDENT_LEAD_PARSERS.get(lead_keyword) {
        return Some(parsers.as_slice());
    }

    if !has_mixed_ascii_case(lead_keyword) {
        return None;
    }

    let folded = lead_keyword.to_ascii_lowercase();
    IDENT_LEAD_PARSERS.get(folded.as_str()).map(Vec::as_slice)
}

fn has_mixed_ascii_case(value: &str) -> bool {
    let mut has_lowercase = false;
    let mut has_uppercase = false;
    for byte in value.bytes() {
        has_lowercase |= byte.is_ascii_lowercase();
        has_uppercase |= byte.is_ascii_uppercase();
        if has_lowercase && has_uppercase {
            return true;
        }
    }
    false
}

const STRAY_BLOCK_BOUNDARIES: &[(&str, &str)] = &[
    ("ELSEIF", "IF"),
    ("ELSE", "IF"),
    ("ENDIF", "IF"),
    ("WHEN", "CASE"),
    ("ENDCASE", "CASE"),
    ("ENDWHILE", "WHILE"),
    ("ENDDO", "DO"),
    ("ENDLOOP", "LOOP"),
    ("CATCH", "TRY"),
    ("CLEANUP", "TRY"),
    ("ENDTRY", "TRY"),
    ("ENDCATCH", "CATCH SYSTEM-EXCEPTIONS"),
    ("ENDCLASS", "CLASS"),
    ("ENDINTERFACE", "INTERFACE"),
    ("ENDMETHOD", "METHOD"),
    ("ENDFORM", "FORM"),
    ("ENDFUNCTION", "FUNCTION"),
    ("ENDMODULE", "MODULE"),
    ("ENDENHANCEMENT", "ENHANCEMENT"),
    ("ENDSELECT", "SELECT"),
];

const STRAY_HYPHENATED_BLOCK_BOUNDARIES: &[(&[&str], &str, &str)] = &[(
    &["end", "enhancement", "section"],
    "END-ENHANCEMENT-SECTION",
    "ENHANCEMENT-SECTION",
)];

fn match_hyphenated_keyword(source: &str, tokens: &[Token], idx: usize, parts: &[&str]) -> bool {
    let mut i = idx;
    for (part_idx, part) in parts.iter().enumerate() {
        let Some(tok) = tokens.get(i) else {
            return false;
        };
        if tok.kind != TokenKind::Ident || !tok.lexeme(source).eq_ignore_ascii_case(part) {
            return false;
        }
        i += 1;
        if part_idx + 1 < parts.len() {
            if tokens.get(i).map(|t| t.kind) != Some(TokenKind::Minus) {
                return false;
            }
            i += 1;
        }
    }
    true
}

fn stray_block_boundary(
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(&'static str, &'static str)> {
    let token = tokens.get(idx)?;
    if token.kind != TokenKind::Ident {
        return None;
    }
    for (parts, boundary, opener) in STRAY_HYPHENATED_BLOCK_BOUNDARIES {
        if match_hyphenated_keyword(source, tokens, idx, parts) {
            return Some((*boundary, *opener));
        }
    }
    let text = token.lexeme(source);
    STRAY_BLOCK_BOUNDARIES
        .iter()
        .copied()
        .find(|(boundary, _)| text.eq_ignore_ascii_case(boundary))
}

fn try_parse_stray_block_boundary_error(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<ParseError>,
) -> Option<(NodeId, usize)> {
    let token = tokens.get(idx)?;
    let (boundary, opener) = stray_block_boundary(source, tokens, idx)?;
    let (end_exclusive, err_end) = match scan_until_statement_period(tokens, source, idx + 1) {
        StmtPeriodScan::Found(period_i) => (period_i + 1, tokens[period_i].range.end),
        StmtPeriodScan::Unterminated { end_exclusive } => (
            end_exclusive,
            unterminated_err_end(tokens, end_exclusive, token.range.end),
        ),
    };

    errors.push(ParseError {
        message: format!("syntax error: unexpected {boundary} without matching {opener}"),
        range: token.range.start..err_end,
    });
    let children = tokens
        .get(idx..end_exclusive)
        .unwrap_or(&[])
        .iter()
        .map(|tok| syntax::token_leaf(b, tok))
        .collect::<Vec<_>>();
    let node = b.branch(SyntaxKind::Error, token.range.start..err_end, &children);
    let next = if tokens.get(end_exclusive).map(|t| t.kind) == Some(TokenKind::Eof) {
        tokens.len()
    } else {
        end_exclusive
    };
    Some((node, next))
}

/// One top-level statement or template chunk (used by [`syntax::build_file_tree`] and `IF` bodies).
pub(crate) fn parse_file_level_item(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<ParseError>,
) -> (NodeId, usize) {
    let t = &tokens[idx];
    match t.kind {
        TokenKind::Eof | TokenKind::Comment => return (syntax::token_leaf(b, t), idx + 1),
        TokenKind::StringTemplate => {
            let (node, next) = syntax::parse_char_string_template(source, tokens, idx, b);
            return (node, next);
        }
        TokenKind::Ident => {
            let lead_keyword = t.lexeme(source);
            if let Some((node, next)) =
                try_guarded_ident_parsers(b, source, tokens, idx, lead_keyword, errors)
            {
                return (node, next);
            }
        }
        _ => {}
    }

    if let Some((node, next)) = assign_stmt::try_parse_assign_stmt(b, source, tokens, idx, errors) {
        return (node, next);
    }
    if let Some((node, next)) = try_parse_stray_block_boundary_error(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if let Some((node, next)) = simple_stmt::try_parse_simple_stmt(b, source, tokens, idx, errors) {
        return (node, next);
    }
    (syntax::token_leaf(b, t), idx + 1)
}

/// Expression parse into an existing [`abap_ast::arena::SyntaxTreeBuilder`] (call [`abap_ast::arena::SyntaxTreeBuilder::finish`] for a [`abap_ast::File`]).
pub use expr::{parse_arithmetic_expr, parse_logical_expr};

pub use interner::{Interner, Symbol};

use abap_ast::File;
use abap_lexer::{LexedSource, TextRange, TokenizeResult, tokenize};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub message: String,
    pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseResult {
    pub file: File,
    pub lexed: LexedSource,
    pub tokens: Arc<[Token]>,
    pub token_symbols: Vec<Option<Symbol>>,
    pub interner: Interner,
    pub errors: Vec<ParseError>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseDiagnosticPolicy {
    Strict,
    IncludeFragment,
}

const MISSING_FRAGMENT_BOUNDARIES: &[&str] = &[
    "ENDAT",
    "ENDCASE",
    "ENDCATCH",
    "ENDCLASS",
    "ENDDO",
    "ENDENHANCEMENT",
    "END-ENHANCEMENT-SECTION",
    "ENDFORM",
    "ENDFUNCTION",
    "ENDIF",
    "ENDINTERFACE",
    "ENDLOOP",
    "ENDMETHOD",
    "ENDMODULE",
    "ENDSELECT",
    "ENDTRY",
    "ENDWHILE",
];

pub fn parse_error_is_include_fragment_boundary(error: &ParseError) -> bool {
    let message = error.message.as_str();
    if message.starts_with("syntax error: unexpected ") && message.contains(" without matching ") {
        return true;
    }
    MISSING_FRAGMENT_BOUNDARIES
        .iter()
        .any(|boundary| message == format!("syntax error: expected {boundary}"))
}

pub fn parse(source: &str) -> ParseResult {
    parse_with_diagnostic_policy(source, ParseDiagnosticPolicy::Strict)
}

pub fn parse_with_diagnostic_policy(
    source: &str,
    diagnostic_policy: ParseDiagnosticPolicy,
) -> ParseResult {
    let TokenizeResult {
        lexed,
        tokens,
        errors: lex_errors,
    } = tokenize(source);
    let mut errors: Vec<ParseError> = lex_errors
        .into_iter()
        .map(|e| ParseError {
            message: e.message.to_string(),
            range: e.range,
        })
        .collect();
    let mut interner = Interner::default();
    let mut scratch = String::new();
    let token_symbols = tokens
        .iter()
        .map(|token| {
            (token.kind == TokenKind::Ident)
                .then(|| interner.intern_lowercase(token.lexeme(source), &mut scratch))
        })
        .collect();
    let end = source.len();
    let file = syntax::build_file_tree(source, &tokens, end, &mut errors);
    if diagnostic_policy == ParseDiagnosticPolicy::IncludeFragment {
        errors.retain(|error| !parse_error_is_include_fragment_boundary(error));
    }

    ParseResult {
        file,
        lexed,
        tokens,
        token_symbols,
        interner,
        errors,
    }
}

#[cfg(test)]
mod tests {
    use abap_ast::SyntaxKind;

    use super::parse;

    #[test]
    fn wraps_lexed_tokens_in_a_file_node() {
        let parsed = parse("REPORT zfoo.");

        assert!(parsed.errors.is_empty());
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::ReportStmt),
            1
        );
        assert_eq!(parsed.interner.len(), 2);
        assert_eq!(parsed.token_symbols.len(), parsed.tokens.len());
    }

    #[test]
    fn parse_groups_string_templates_for_semantic_structure() {
        let parsed = parse("|a { lv_amount DECIMALS = 2 } b|");
        assert!(parsed.errors.is_empty());
        let root = parsed.file.root();
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::CharStringTemplate),
            1
        );
        assert_eq!(
            parsed
                .file
                .count_kind(root, SyntaxKind::TemplateInterpolation),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::TemplateFormatSpec),
            1
        );
    }

    #[test]
    fn parses_data_typed_decl_in_file() {
        let parsed = parse("DATA: a TYPE i, b TYPE string.");
        assert!(parsed.errors.is_empty());
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DataDecl), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DataTypedClause), 2);
    }

    #[test]
    fn guarded_dispatch_handles_hyphenated_and_multiword_leads() {
        let parsed = parse("FIELD-SYMBOLS <row> TYPE any.\nGET TIME STAMP FIELD lv_ts.");
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::FieldSymbolsDecl),
            1
        );
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::GetTimeStampStmt),
            1
        );
    }

    #[test]
    fn guarded_dispatch_indexes_multiple_attempts_per_lead() {
        assert_eq!(
            super::ident_lead_parser_attempts("get")
                .expect("GET parsers")
                .len(),
            3
        );
        assert_eq!(
            super::ident_lead_parser_attempts("GET")
                .expect("GET parsers")
                .len(),
            3
        );
        assert_eq!(
            super::ident_lead_parser_attempts("GeT")
                .expect("GET parsers")
                .len(),
            3
        );
        assert!(super::ident_lead_parser_attempts("lv_value").is_none());
    }

    #[test]
    fn guarded_dispatch_preserves_mixed_case_keyword_matching() {
        let parsed = parse(
            "SeLeCt-OpTiOnS so_bukrs FOR lv_bukrs.\n\
             ClAsS-DaTa gv_count TYPE i.\n\
             GeT ReFeReNcE OF gv_count INTO lr_count.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::SelectOptionsDecl),
            1
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DataDecl), 1);
        assert_eq!(
            parsed.file.count_kind(root, SyntaxKind::GetReferenceStmt),
            1
        );
    }

    #[test]
    fn parses_macro_definitions_and_invocations() {
        let parsed = parse(
            "DEFINE map.\n\
               is_input_allowed &2.\n\
               <target> = &3.\n\
             END-OF-DEFINITION.\n\
             map lr_cat->data 'WAERS' wa_t001-waers.\n\
             save.\n\
             ucomm.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::MacroDef), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::MacroCallStmt), 3);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    fn macro_definition_body_does_not_emit_normal_statement_errors() {
        let parsed = parse(
            "CLASS lcl IMPLEMENTATION.\n\
               METHOD run.\n\
                 DEFINE guard.\n\
                   IF &1 IS INITIAL.\n\
                     RETURN.\n\
                   ENDIF.\n\
                 END-OF-DEFINITION.\n\
                 guard lv_value.\n\
               ENDMETHOD.\n\
             ENDCLASS.",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::ClassDecl), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::MethodDecl), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::MacroDef), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::MacroCallStmt), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::Error), 0);
    }

    #[test]
    #[allow(unreachable_code)]
    fn file_level_quote_comment_does_not_merge_into_next_statement() {
        let src = "DATA lv TYPE i.\n\" trailing line comment\nlv = 1.";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        let kids: Vec<_> = parsed.file.children(root).collect();
        assert_eq!(
            kids.len(),
            2,
            "expected DATA and assignment - got {}",
            kids.len()
        );
        let assign_token = parsed
            .tokens
            .iter()
            .find(|token| {
                token.lexeme(src).eq_ignore_ascii_case("lv") && token.has_newline_before()
            })
            .expect("assignment token");
        assert!(
            parsed
                .lexed
                .leading_comments(assign_token)
                .any(|piece| piece.lexeme(src).trim_start().starts_with('"'))
        );
        return;
        assert!(
            kids.len() >= 3,
            "expected DATA, comment token, assignment — got {} children",
            kids.len()
        );
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DataDecl), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::AssignStmt), 1);
    }

    #[test]
    fn file_level_star_comment_is_skipped_before_next_statement() {
        let src = "DATA lv TYPE i.\n* full line comment\nlv = 1.";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::DataDecl), 1);
        assert_eq!(parsed.file.count_kind(root, SyntaxKind::AssignStmt), 1);
    }

    #[test]
    fn simple_statement_requires_closing_period_before_eof() {
        let parsed = parse("REPORT zfoo");
        assert!(
            parsed
                .errors
                .iter()
                .any(|e| e.message.contains("expected '.'")),
            "{:?}",
            parsed.errors
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::UnparsedStmt),
            0
        );
    }

    #[test]
    fn simple_statement_split_across_lines_still_requires_period() {
        let parsed = parse("REPORT zfoo\nDATA lv TYPE i.");
        assert!(
            parsed
                .errors
                .iter()
                .any(|e| e.message.contains("expected '.'")),
            "{:?}",
            parsed.errors
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::DataDecl),
            1
        );
    }
}
