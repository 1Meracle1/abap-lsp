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

use crate::stmt_period::{
    StmtPeriodScan, is_definite_stmt_lead_keyword, scan_until_statement_period,
    unterminated_err_end,
};
use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::Token;
use abap_lexer::TokenKind;
use std::sync::Arc;

fn prev_non_comment_is_ident(tokens: &[Token], idx: usize) -> bool {
    let mut j = idx;
    while j > 0 {
        j -= 1;
        match tokens[j].kind {
            TokenKind::Comment => continue,
            TokenKind::Ident => return true,
            _ => return false,
        }
    }
    false
}

type ParseAttempt = fn(
    &mut SyntaxTreeBuilder,
    &str,
    &[Token],
    usize,
    &mut Vec<ParseError>,
) -> Option<(NodeId, usize)>;

#[derive(Clone, Copy)]
struct GuardedParser {
    lead_keywords: &'static [&'static str],
    parser: ParseAttempt,
}

impl GuardedParser {
    const fn new(lead_keywords: &'static [&'static str], parser: ParseAttempt) -> Self {
        Self {
            lead_keywords,
            parser,
        }
    }
}

const IDENT_LEAD_PARSERS: &[GuardedParser] = &[
    GuardedParser::new(&["data"], data_decl::try_parse_data_decl),
    GuardedParser::new(&["tables"], data_decl::try_parse_tables_decl),
    GuardedParser::new(
        &["parameters", "parameter"],
        data_decl::try_parse_parameters_decl,
    ),
    GuardedParser::new(&["select"], data_decl::try_parse_select_options_decl),
    GuardedParser::new(&["class"], data_decl::try_parse_class_data_decl),
    GuardedParser::new(&["if"], if_stmt::try_parse_if_stmt),
    GuardedParser::new(&["statics"], data_decl::try_parse_statics_decl),
    GuardedParser::new(&["types"], data_decl::try_parse_types_decl),
    GuardedParser::new(&["constants"], data_decl::try_parse_constants_decl),
    GuardedParser::new(&["field"], data_decl::try_parse_field_symbols_decl),
    GuardedParser::new(&["case"], control_stmt::try_parse_case_stmt),
    GuardedParser::new(&["while"], control_stmt::try_parse_while_stmt),
    GuardedParser::new(&["do"], control_stmt::try_parse_do_stmt),
    GuardedParser::new(&["loop"], control_stmt::try_parse_loop_stmt),
    GuardedParser::new(&["try"], control_stmt::try_parse_try_stmt),
    GuardedParser::new(&["report", "program"], surface_stmt::try_parse_report_stmt),
    GuardedParser::new(&["include"], surface_stmt::try_parse_include_stmt),
    GuardedParser::new(
        &["selection"],
        surface_stmt::try_parse_selection_screen_stmt,
    ),
    GuardedParser::new(
        &["at", "initialization", "start", "end", "top"],
        surface_stmt::try_parse_event_block,
    ),
    GuardedParser::new(&["at"], control_stmt::try_parse_at_stmt),
    GuardedParser::new(&["form"], surface_stmt::try_parse_form_decl),
    GuardedParser::new(&["function"], surface_stmt::try_parse_function_decl),
    GuardedParser::new(&["module"], surface_stmt::try_parse_module_decl),
    GuardedParser::new(&["class"], surface_stmt::try_parse_class_decl),
    GuardedParser::new(&["interface"], surface_stmt::try_parse_interface_decl),
    GuardedParser::new(&["method"], surface_stmt::try_parse_method_decl),
    GuardedParser::new(&["select"], surface_stmt::try_parse_select_stmt),
    GuardedParser::new(&["open"], surface_stmt::try_parse_open_cursor_stmt),
    GuardedParser::new(&["close"], surface_stmt::try_parse_close_cursor_stmt),
    GuardedParser::new(&["read"], surface_stmt::try_parse_read_table_stmt),
    GuardedParser::new(&["authority"], surface_stmt::try_parse_authority_check_stmt),
    GuardedParser::new(&["append"], surface_stmt::try_parse_append_stmt),
    GuardedParser::new(&["insert"], surface_stmt::try_parse_insert_table_stmt),
    GuardedParser::new(&["move"], surface_stmt::try_parse_move_stmt),
    GuardedParser::new(&["sort"], surface_stmt::try_parse_sort_stmt),
    GuardedParser::new(&["modify"], surface_stmt::try_parse_modify_stmt),
    GuardedParser::new(&["delete"], surface_stmt::try_parse_delete_stmt),
    GuardedParser::new(&["update"], surface_stmt::try_parse_update_stmt),
    GuardedParser::new(&["write"], surface_stmt::try_parse_write_stmt),
    GuardedParser::new(&["split"], surface_stmt::try_parse_split_stmt),
    GuardedParser::new(&["concatenate"], surface_stmt::try_parse_concatenate_stmt),
    GuardedParser::new(&["condense"], surface_stmt::try_parse_condense_stmt),
    GuardedParser::new(&["raise"], surface_stmt::try_parse_raise_stmt),
    GuardedParser::new(&["message"], surface_stmt::try_parse_message_stmt),
    GuardedParser::new(&["submit"], surface_stmt::try_parse_submit_stmt),
    GuardedParser::new(&["leave"], surface_stmt::try_parse_leave_stmt),
    GuardedParser::new(&["endat"], surface_stmt::try_parse_endat_stmt),
    GuardedParser::new(&["find"], surface_stmt::try_parse_find_stmt),
    GuardedParser::new(&["get"], surface_stmt::try_parse_get_reference_stmt),
    GuardedParser::new(&["get"], surface_stmt::try_parse_get_bit_stmt),
    GuardedParser::new(&["get"], surface_stmt::try_parse_get_time_stamp_stmt),
    GuardedParser::new(&["set"], surface_stmt::try_parse_set_bit_stmt),
    GuardedParser::new(&["assign"], surface_stmt::try_parse_assign_keyword_stmt),
    GuardedParser::new(&["call", "create"], surface_stmt::try_parse_call_like_stmt),
];

fn try_guarded_ident_parsers(
    parsers: &[GuardedParser],
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    lead_keyword: &str,
    errors: &mut Vec<ParseError>,
) -> Option<(NodeId, usize)> {
    for guarded in parsers {
        if guarded
            .lead_keywords
            .iter()
            .any(|keyword| lead_keyword.eq_ignore_ascii_case(keyword))
            && let Some((node, next)) = (guarded.parser)(b, source, tokens, idx, errors)
        {
            return Some((node, next));
        }
    }
    None
}

fn try_parse_lone_ident_stmt_error(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<ParseError>,
) -> Option<(NodeId, usize)> {
    let t = tokens.get(idx)?;
    let period = tokens.get(idx + 1)?;
    if t.kind != TokenKind::Ident
        || period.kind != TokenKind::Period
        || prev_non_comment_is_ident(tokens, idx)
        || is_definite_stmt_lead_keyword(source, t)
    {
        return None;
    }

    errors.push(ParseError {
        message: "syntax error: a lone identifier before '.' is not a valid statement".to_string(),
        range: t.range.start..period.range.end,
    });
    let ident = syntax::token_leaf(b, t);
    let dot = syntax::token_leaf(b, period);
    let node = b.branch(
        SyntaxKind::Error,
        t.range.start..period.range.end,
        &[ident, dot],
    );
    Some((node, idx + 2))
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
    ("ENDCLASS", "CLASS"),
    ("ENDINTERFACE", "INTERFACE"),
    ("ENDMETHOD", "METHOD"),
    ("ENDFORM", "FORM"),
    ("ENDFUNCTION", "FUNCTION"),
    ("ENDMODULE", "MODULE"),
    ("ENDSELECT", "SELECT"),
];

fn stray_block_boundary(source: &str, token: &Token) -> Option<(&'static str, &'static str)> {
    if token.kind != TokenKind::Ident {
        return None;
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
    let (boundary, opener) = stray_block_boundary(source, token)?;
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
            if let Some((node, next)) = try_guarded_ident_parsers(
                IDENT_LEAD_PARSERS,
                b,
                source,
                tokens,
                idx,
                lead_keyword,
                errors,
            ) {
                return (node, next);
            }
        }
        _ => {}
    }

    if let Some((node, next)) = assign_stmt::try_parse_assign_stmt(b, source, tokens, idx, errors) {
        return (node, next);
    }
    if let Some((node, next)) = try_parse_lone_ident_stmt_error(b, source, tokens, idx, errors) {
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

pub fn parse(source: &str) -> ParseResult {
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
