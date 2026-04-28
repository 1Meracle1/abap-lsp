use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::{Token, TokenKind};

use crate::block_helpers::is_keyword;
use crate::stmt_period::{is_definite_stmt_lead_keyword, token_begins_line};
use crate::syntax::token_leaf;

fn type_ref_name_node(b: &mut SyntaxTreeBuilder, token: &Token) -> NodeId {
    let leaf = token_leaf(b, token);
    b.branch(SyntaxKind::TypeRefName, token.range.clone(), &[leaf])
}

fn parse_type_ref_head(b: &mut SyntaxTreeBuilder, tokens: &[Token]) -> Option<(NodeId, usize)> {
    let first = tokens.first()?;
    if first.kind != TokenKind::Ident {
        return None;
    }
    let base = type_ref_name_node(b, first);
    let mut children = vec![base];
    let mut i = 1usize;
    while i + 1 < tokens.len() {
        let op = &tokens[i];
        let next = &tokens[i + 1];
        if !matches!(
            op.kind,
            TokenKind::Minus | TokenKind::Arrow | TokenKind::Tilde | TokenKind::FatArrow
        ) || next.kind != TokenKind::Ident
        {
            break;
        }
        children.push(token_leaf(b, op));
        children.push(type_ref_name_node(b, next));
        i += 2;
    }
    if i == 1 {
        return Some((base, 1));
    }
    let end = b.span(*children.last().unwrap()).end;
    Some((
        b.branch(
            SyntaxKind::TypeRefSelectorChain,
            first.range.start..end,
            &children,
        ),
        i,
    ))
}

fn type_ref_starts_with_ref_to(source: &str, tokens: &[Token]) -> bool {
    tokens
        .first()
        .is_some_and(|token| is_keyword(source, token, "ref"))
        && tokens
            .get(1)
            .is_some_and(|token| is_keyword(source, token, "to"))
}

fn first_top_level_of_keyword(source: &str, tokens: &[Token]) -> Option<usize> {
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    for (idx, token) in tokens.iter().enumerate() {
        match token.kind {
            TokenKind::LParen => paren += 1,
            TokenKind::RParen => paren -= 1,
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace => brace -= 1,
            _ => {}
        }
        if paren == 0 && bracket == 0 && brace == 0 && is_keyword(source, token, "of") {
            return Some(idx);
        }
    }
    None
}

fn build_type_ref_children(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
) -> Vec<NodeId> {
    if tokens.is_empty() {
        return Vec::new();
    }
    if type_ref_starts_with_ref_to(source, tokens) {
        let mut children = vec![token_leaf(b, &tokens[0]), token_leaf(b, &tokens[1])];
        if tokens.len() > 2 {
            children.push(build_type_ref_node(b, source, &tokens[2..]));
        }
        return children;
    }
    if let Some(of_idx) = first_top_level_of_keyword(source, tokens)
        && of_idx + 1 < tokens.len()
    {
        let mut children = Vec::with_capacity(of_idx + 2);
        for token in &tokens[..=of_idx] {
            children.push(token_leaf(b, token));
        }
        children.push(build_type_ref_node(b, source, &tokens[of_idx + 1..]));
        return children;
    }
    if let Some((head, consumed)) = parse_type_ref_head(b, tokens) {
        let mut children = vec![head];
        for token in &tokens[consumed..] {
            children.push(token_leaf(b, token));
        }
        return children;
    }
    tokens.iter().map(|token| token_leaf(b, token)).collect()
}

pub(crate) fn build_type_ref_node(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
) -> NodeId {
    let start = tokens.first().map(|token| token.range.start).unwrap_or(0);
    let end = tokens.last().map(|token| token.range.end).unwrap_or(start);
    let children = build_type_ref_children(b, source, tokens);
    b.branch(SyntaxKind::TypeRefSimple, start..end, &children)
}

pub(crate) fn parse_type_ref_tokens(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    stop_keywords: &[&str],
) -> Option<(NodeId, usize)> {
    let first = tokens.get(idx)?;
    if matches!(
        first.kind,
        TokenKind::Comma | TokenKind::Period | TokenKind::Colon | TokenKind::Eof
    ) {
        return None;
    }

    let mut i = idx;
    let mut paren = 0i32;
    let mut bracket = 0i32;
    let mut brace = 0i32;
    while i < tokens.len() {
        let tok = &tokens[i];
        if paren == 0 && bracket == 0 && brace == 0 {
            if matches!(
                tok.kind,
                TokenKind::Comma | TokenKind::Period | TokenKind::Eof
            ) {
                break;
            }
            if tok.kind == TokenKind::Ident
                && stop_keywords
                    .iter()
                    .any(|kw| tok.lexeme(source).eq_ignore_ascii_case(kw))
            {
                break;
            }
            if i > idx && tok.kind == TokenKind::Ident && token_begins_line(tok) {
                if is_definite_stmt_lead_keyword(source, tok) {
                    break;
                }
                let next_kind = tokens.get(i + 1).map(|next| next.kind);
                if matches!(next_kind, Some(TokenKind::Eq | TokenKind::QuestionEq)) {
                    break;
                }
            }
        }
        match tok.kind {
            TokenKind::LParen => paren += 1,
            TokenKind::RParen => paren -= 1,
            TokenKind::LBracket => bracket += 1,
            TokenKind::RBracket => bracket -= 1,
            TokenKind::LBrace => brace += 1,
            TokenKind::RBrace => brace -= 1,
            _ => {}
        }
        i += 1;
    }
    if i == idx {
        return None;
    }
    let node = build_type_ref_node(b, source, &tokens[idx..i]);
    Some((node, i))
}
