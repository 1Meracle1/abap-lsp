use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::{Token, TokenKind};

use crate::stmt_period::{StmtPeriodScan, scan_until_statement_period, unterminated_err_end};
use crate::syntax::token_leaf;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Boundary<'a> {
    Keyword(&'a str),
    Eof,
}

#[inline]
pub(crate) fn is_keyword(source: &str, token: &Token, kw: &str) -> bool {
    token.kind == TokenKind::Ident && token.lexeme(source).eq_ignore_ascii_case(kw)
}

pub(crate) fn skip_trivia(tokens: &[Token], mut idx: usize) -> usize {
    while idx < tokens.len() && tokens[idx].kind == TokenKind::Comment {
        idx += 1;
    }
    idx
}

pub(crate) fn next_after_unterminated_scan(tokens: &[Token], end_exclusive: usize) -> usize {
    if tokens.get(end_exclusive).map(|t| t.kind) == Some(TokenKind::Eof) {
        tokens.len()
    } else {
        end_exclusive
    }
}

pub(crate) fn ensure_forward_progress(tokens: &[Token], current: usize, next: usize) -> usize {
    if next > current {
        next
    } else if current < tokens.len() {
        current + 1
    } else {
        next
    }
}

pub(crate) fn error_token_children(
    b: &mut SyntaxTreeBuilder,
    tokens: &[Token],
    start: usize,
    end_exclusive: usize,
) -> Vec<NodeId> {
    let mut children = Vec::with_capacity(end_exclusive.saturating_sub(start));
    for t in &tokens[start..end_exclusive] {
        children.push(token_leaf(b, t));
    }
    children
}

pub(crate) fn scan_boundary_keywords<'a>(
    source: &'a str,
    tokens: &'a [Token],
    mut idx: usize,
    keywords: &'a [&'a str],
) -> Option<Boundary<'a>> {
    while idx < tokens.len() {
        match tokens[idx].kind {
            TokenKind::Comment => idx += 1,
            TokenKind::Ident => {
                let lit = tokens[idx].lexeme(source);
                for kw in keywords {
                    if lit.eq_ignore_ascii_case(kw) {
                        return Some(Boundary::Keyword(kw));
                    }
                }
                return None;
            }
            TokenKind::Eof => return Some(Boundary::Eof),
            _ => return None,
        }
    }
    Some(Boundary::Eof)
}

pub(crate) fn recover_skip_after_keyword(
    source: &str,
    tokens: &[Token],
    mut idx: usize,
    keyword: &str,
) -> usize {
    while idx < tokens.len() {
        if tokens[idx].kind == TokenKind::Ident
            && tokens[idx].lexeme(source).eq_ignore_ascii_case(keyword)
        {
            let mut j = idx + 1;
            j = skip_trivia(tokens, j);
            if tokens.get(j).map(|t| t.kind) == Some(TokenKind::Period) {
                return j + 1;
            }
        }
        idx += 1;
    }
    tokens.len()
}

pub(crate) fn parse_body_until_keywords(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    mut idx: usize,
    errors: &mut Vec<crate::ParseError>,
    stop_keywords: &[&str],
) -> (Vec<NodeId>, usize) {
    let mut nodes = Vec::new();
    loop {
        if let Some(Boundary::Keyword(_)) | Some(Boundary::Eof) =
            scan_boundary_keywords(source, tokens, idx, stop_keywords)
        {
            break;
        }
        if idx >= tokens.len() || tokens[idx].kind == TokenKind::Eof {
            break;
        }
        let (node, next) = crate::parse_file_level_item(b, source, tokens, idx, errors);
        nodes.push(node);
        idx = ensure_forward_progress(tokens, idx, next);
    }
    (nodes, idx)
}

pub(crate) fn parse_header_until_period(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    keyword_idx: usize,
    body_start_idx: usize,
    errors: &mut Vec<crate::ParseError>,
    missing_period_message: &str,
) -> (Vec<NodeId>, usize) {
    match scan_until_statement_period(tokens, source, body_start_idx) {
        StmtPeriodScan::Found(period_i) => {
            let mut children = Vec::with_capacity(period_i.saturating_sub(keyword_idx) + 1);
            for t in &tokens[keyword_idx..=period_i] {
                children.push(token_leaf(b, t));
            }
            (children, period_i + 1)
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let start_tok = &tokens[keyword_idx];
            let err_end = unterminated_err_end(tokens, end_exclusive, start_tok.range.end);
            errors.push(crate::ParseError {
                message: missing_period_message.to_string(),
                range: start_tok.range.start..err_end,
            });
            let err_children = error_token_children(b, tokens, keyword_idx, end_exclusive);
            let header = b.branch(
                SyntaxKind::Error,
                start_tok.range.start..err_end,
                &err_children,
            );
            (
                vec![header],
                next_after_unterminated_scan(tokens, end_exclusive),
            )
        }
    }
}
