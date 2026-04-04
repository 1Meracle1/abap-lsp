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

/// One top-level statement or template chunk (used by [`syntax::build_file_tree`] and `IF` bodies).
pub(crate) fn parse_file_level_item(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<ParseError>,
) -> (NodeId, usize) {
    let t = &tokens[idx];
    if t.kind == TokenKind::Eof {
        return (syntax::token_leaf(b, t), idx + 1);
    }

    // Lexer already maps `"` … EOL and full-line `*` comments to [`TokenKind::Comment`]; surface them in the
    // tree so later statements are not parsed as continuations of the previous construct.
    if t.kind == TokenKind::Comment {
        return (syntax::token_leaf(b, t), idx + 1);
    }

    if t.kind == TokenKind::StringTemplate {
        let (node, next) = syntax::parse_char_string_template(source, tokens, idx, b);
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && t.lexeme(source).eq_ignore_ascii_case("data")
        && let Some((node, next)) = data_decl::try_parse_data_decl(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident && t.lexeme(source).eq_ignore_ascii_case("if") {
        if let Some((node, next)) = if_stmt::try_parse_if_stmt(b, source, tokens, idx, errors) {
            return (node, next);
        }
    }
    if t.kind == TokenKind::Ident
        && t.lexeme(source).eq_ignore_ascii_case("statics")
        && let Some((node, next)) =
            data_decl::try_parse_statics_decl(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && t.lexeme(source).eq_ignore_ascii_case("types")
        && let Some((node, next)) = data_decl::try_parse_types_decl(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && t.lexeme(source).eq_ignore_ascii_case("constants")
        && let Some((node, next)) =
            data_decl::try_parse_constants_decl(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) =
            data_decl::try_parse_field_symbols_decl(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) =
            control_stmt::try_parse_case_stmt(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) =
            control_stmt::try_parse_while_stmt(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) = control_stmt::try_parse_do_stmt(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) =
            control_stmt::try_parse_loop_stmt(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) = control_stmt::try_parse_try_stmt(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) =
            surface_stmt::try_parse_report_stmt(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) =
            surface_stmt::try_parse_include_stmt(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) =
            surface_stmt::try_parse_event_block(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) =
            surface_stmt::try_parse_form_decl(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) =
            surface_stmt::try_parse_module_decl(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) =
            surface_stmt::try_parse_class_decl(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) =
            surface_stmt::try_parse_interface_decl(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) =
            surface_stmt::try_parse_method_decl(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) =
            surface_stmt::try_parse_select_stmt(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) =
            surface_stmt::try_parse_read_table_stmt(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) =
            surface_stmt::try_parse_append_stmt(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) =
            surface_stmt::try_parse_write_stmt(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) =
            surface_stmt::try_parse_raise_stmt(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) =
            surface_stmt::try_parse_endat_stmt(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) =
            surface_stmt::try_parse_assign_keyword_stmt(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && let Some((node, next)) =
            surface_stmt::try_parse_call_like_stmt(b, source, tokens, idx, errors)
    {
        return (node, next);
    }
    if let Some((node, next)) = assign_stmt::try_parse_assign_stmt(b, source, tokens, idx, errors) {
        return (node, next);
    }
    if t.kind == TokenKind::Ident
        && tokens.get(idx + 1).map(|x| x.kind) == Some(TokenKind::Period)
        && !prev_non_comment_is_ident(tokens, idx)
        && !is_definite_stmt_lead_keyword(source, t)
    {
        let period = &tokens[idx + 1];
        errors.push(ParseError {
            message: "syntax error: a lone identifier before '.' is not a valid statement"
                .to_string(),
            range: t.range.start..period.range.end,
        });
        let a = syntax::token_leaf(b, t);
        let p = syntax::token_leaf(b, period);
        let node = b.branch(SyntaxKind::Error, t.range.start..period.range.end, &[a, p]);
        return (node, idx + 2);
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
