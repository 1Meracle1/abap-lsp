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

use crate::stmt_period::is_definite_stmt_lead_keyword;
use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::Token;
use abap_lexer::TokenKind;

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
    GuardedParser::new(&["report"], surface_stmt::try_parse_report_stmt),
    GuardedParser::new(&["include"], surface_stmt::try_parse_include_stmt),
    GuardedParser::new(
        &["initialization", "start", "end", "top"],
        surface_stmt::try_parse_event_block,
    ),
    GuardedParser::new(&["form"], surface_stmt::try_parse_form_decl),
    GuardedParser::new(&["module"], surface_stmt::try_parse_module_decl),
    GuardedParser::new(&["class"], surface_stmt::try_parse_class_decl),
    GuardedParser::new(&["interface"], surface_stmt::try_parse_interface_decl),
    GuardedParser::new(&["method"], surface_stmt::try_parse_method_decl),
    GuardedParser::new(&["select"], surface_stmt::try_parse_select_stmt),
    GuardedParser::new(&["read"], surface_stmt::try_parse_read_table_stmt),
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
    if let Some((node, next)) = simple_stmt::try_parse_simple_stmt(b, source, tokens, idx, errors) {
        return (node, next);
    }
    (syntax::token_leaf(b, t), idx + 1)
}

/// Expression parse into an existing [`abap_ast::arena::SyntaxTreeBuilder`] (call [`abap_ast::arena::SyntaxTreeBuilder::finish`] for a [`abap_ast::File`]).
pub use expr::{parse_arithmetic_expr, parse_logical_expr};

pub use interner::{Interner, Symbol};

use abap_ast::File;
use abap_lexer::{TextRange, TokenizeResult, tokenize};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub message: String,
    pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseResult {
    pub file: File,
    pub tokens: Vec<Token>,
    pub token_symbols: Vec<Option<Symbol>>,
    pub interner: Interner,
    pub errors: Vec<ParseError>,
}

pub fn parse(source: &str) -> ParseResult {
    let TokenizeResult {
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
    fn file_level_quote_comment_does_not_merge_into_next_statement() {
        let src = "DATA lv TYPE i.\n\" trailing line comment\nlv = 1.";
        let parsed = parse(src);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let root = parsed.file.root();
        let kids: Vec<_> = parsed.file.children(root).collect();
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
