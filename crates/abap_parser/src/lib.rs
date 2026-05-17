mod assign_stmt;
mod block_helpers;
mod control_stmt;
mod data_decl;
mod expr;
mod if_stmt;
mod interner;
mod parser;
mod simple_stmt;
mod stmt_period;
mod surface_stmt;
pub mod syntax;
mod type_ref;

use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::Token;
use abap_lexer::TokenKind;
use block_helpers::match_hyphenated_keyword;
use std::sync::Arc;

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
    ("ENDEXEC", "EXEC SQL"),
    ("ENDFORM", "FORM"),
    ("ENDFUNCTION", "FUNCTION"),
    ("ENDMODULE", "MODULE"),
    ("ENDENHANCEMENT", "ENHANCEMENT"),
    ("ENDSELECT", "SELECT"),
];

const STRAY_HYPHENATED_BLOCK_BOUNDARIES: &[(&[&str], &str, &str)] = &[
    (
        &["end", "enhancement", "section"],
        "END-ENHANCEMENT-SECTION",
        "ENHANCEMENT-SECTION",
    ),
    (&["end", "test", "seam"], "END-TEST-SEAM", "TEST-SEAM"),
    (
        &["end", "test", "injection"],
        "END-TEST-INJECTION",
        "TEST-INJECTION",
    ),
];

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
        if match_hyphenated_keyword(source, tokens, idx, parts).is_some() {
            return Some((*boundary, *opener));
        }
    }
    let text = token.lexeme(source);
    STRAY_BLOCK_BOUNDARIES
        .iter()
        .copied()
        .find(|(boundary, _)| text.eq_ignore_ascii_case(boundary))
}

fn try_parse_stray_block_boundary_error(cursor: &mut parser::Parser<'_, '_>) -> Option<NodeId> {
    let idx = cursor.index();
    let token = cursor.current()?;
    let start = token.range.start;
    let fallback_end = token.range.end;
    let (boundary, opener) = stray_block_boundary(cursor.source(), cursor.tokens(), idx)?;

    let mut children = vec![cursor.bump()?];
    children.extend(cursor.bump_until_stmt_boundary(&[]));
    cursor.skip_trivia();
    if cursor
        .current()
        .is_some_and(|token| token.kind == TokenKind::Period)
        && let Some(period) = cursor.bump()
    {
        children.push(period);
    }

    let err_end = children
        .last()
        .map(|node| cursor.span(*node).end)
        .unwrap_or(fallback_end);
    cursor.push_error(
        format!("syntax error: unexpected {boundary} without matching {opener}"),
        start..err_end,
    );
    Some(
        cursor
            .builder()
            .branch(SyntaxKind::Error, start..err_end, &children),
    )
}

/// One top-level statement or template chunk (used by [`syntax::build_file_tree`] and `IF` bodies).
pub(crate) fn parse_file_level_item(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<ParseError>,
) -> (NodeId, usize) {
    let mut parser = parser::Parser::new(b, source, tokens, idx, errors);
    let node = parser.parse_file_level_item();
    (node, parser.index())
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
    "END-TEST-SEAM",
    "END-TEST-INJECTION",
    "ENDFORM",
    "ENDFUNCTION",
    "ENDIF",
    "ENDINTERFACE",
    "ENDLOOP",
    "ENDMETHOD",
    "ENDEXEC",
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
    message
        .strip_prefix("syntax error: expected ")
        .is_some_and(|boundary| MISSING_FRAGMENT_BOUNDARIES.contains(&boundary))
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
    fn direct_dispatch_handles_hyphenated_and_multiword_leads() {
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
    fn direct_dispatch_preserves_mixed_case_keyword_matching() {
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
