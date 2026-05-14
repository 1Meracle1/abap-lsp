use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::{Token, TokenKind, have_space_between};

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

pub(crate) fn parse_inline_name(
    b: &mut SyntaxTreeBuilder,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let name_tok = tokens.get(idx)?;
    if name_tok.kind != TokenKind::Ident {
        return None;
    }
    let leaf = token_leaf(b, name_tok);
    Some((
        b.branch(SyntaxKind::DataDeclName, name_tok.range.clone(), &[leaf]),
        idx + 1,
    ))
}

pub(crate) fn inline_name_spacing_is_valid(
    tokens: &[Token],
    lparen_idx: usize,
    name_idx: usize,
    rparen_idx: usize,
) -> bool {
    let lparen = &tokens[lparen_idx];
    let name = &tokens[name_idx];
    let rparen = &tokens[rparen_idx];
    !have_space_between(lparen, name) && !have_space_between(name, rparen)
}

pub(crate) fn match_hyphenated_keyword(
    source: &str,
    tokens: &[Token],
    idx: usize,
    parts: &[&str],
) -> Option<usize> {
    let mut i = idx;
    for (part_idx, part) in parts.iter().enumerate() {
        let tok = tokens.get(i)?;
        if !is_keyword(source, tok, part) {
            return None;
        }
        i += 1;
        if part_idx + 1 < parts.len() {
            if tokens.get(i).map(|t| t.kind) != Some(TokenKind::Minus) {
                return None;
            }
            i += 1;
        }
    }
    Some(i)
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

pub(crate) fn first_boundary_keyword_between(
    source: &str,
    tokens: &[Token],
    start: usize,
    end: usize,
    keywords: &[&str],
) -> Option<usize> {
    let mut idx = start;
    let end = end.min(tokens.len());
    while idx < end {
        if matches!(
            scan_boundary_keywords(source, tokens, idx, keywords),
            Some(Boundary::Keyword(_))
        ) {
            let boundary = skip_trivia(tokens, idx);
            if boundary < end {
                return Some(boundary);
            }
        }
        idx += 1;
    }
    None
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

pub(crate) fn parse_end_keyword(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    start_tok: &Token,
    end_kw: &str,
    missing_message: &str,
    errors: &mut Vec<crate::ParseError>,
) -> (Vec<NodeId>, usize, usize) {
    let end_idx = skip_trivia(tokens, idx);
    let Some(end_tok) = tokens.get(end_idx) else {
        errors.push(crate::ParseError {
            message: missing_message.to_string(),
            range: start_tok.range.clone(),
        });
        return (Vec::new(), tokens.len(), start_tok.range.end);
    };
    if !is_keyword(source, end_tok, end_kw) {
        errors.push(crate::ParseError {
            message: missing_message.to_string(),
            range: start_tok.range.start..end_tok.range.end,
        });
        let recover = recover_skip_after_keyword(source, tokens, idx, end_kw);
        return (Vec::new(), recover, end_tok.range.end);
    }

    let mut j = end_idx + 1;
    j = skip_trivia(tokens, j);
    let Some(period_tok) = tokens.get(j) else {
        errors.push(crate::ParseError {
            message: format!("syntax error: expected '.' after {end_kw}"),
            range: end_tok.range.clone(),
        });
        let recover = recover_skip_after_keyword(source, tokens, end_idx, end_kw);
        return (vec![token_leaf(b, end_tok)], recover, end_tok.range.end);
    };
    if period_tok.kind != TokenKind::Period {
        errors.push(crate::ParseError {
            message: format!("syntax error: expected '.' after {end_kw}"),
            range: end_tok.range.start..period_tok.range.end,
        });
        let recover = recover_skip_after_keyword(source, tokens, end_idx, end_kw);
        return (vec![token_leaf(b, end_tok)], recover, period_tok.range.end);
    }

    (
        vec![token_leaf(b, end_tok), token_leaf(b, period_tok)],
        j + 1,
        period_tok.range.end,
    )
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
        if next >= tokens.len()
            && let Some(boundary) =
                first_boundary_keyword_between(source, tokens, idx + 1, next, stop_keywords)
        {
            idx = boundary;
            break;
        }
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
