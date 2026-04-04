//! Declaration parsing for `DATA`-family statements plus adjacent typed declaration forms.

use abap_ast::SyntaxKind;
use abap_ast::arena::{NodeId, SyntaxTreeBuilder};
use abap_lexer::{Token, TokenKind, have_space_between};

use crate::stmt_period::{StmtPeriodScan, scan_until_statement_period, unterminated_err_end};
use crate::type_ref::parse_type_ref_tokens;

fn token_leaf(b: &mut SyntaxTreeBuilder, token: &Token) -> NodeId {
    b.leaf(SyntaxKind::Token, token.range.clone())
}

#[inline]
fn is_keyword(source: &str, token: &Token, kw: &str) -> bool {
    token.kind == TokenKind::Ident && token.lexeme(source).eq_ignore_ascii_case(kw)
}

/// If `tokens[idx]` begins `DATA … TYPE … .` (optionally `DATA:` and comma-separated clauses),
/// returns the structured node and the index after the closing `.`. Otherwise `None`.
pub fn try_parse_data_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let data_tok = tokens.get(idx)?;
    if !is_keyword(source, data_tok, "data") {
        return None;
    }
    if tokens.get(idx + 1).map(|t| t.kind) == Some(TokenKind::LParen) {
        return try_parse_data_inline_decl(b, source, tokens, idx, errors);
    }

    let scan = scan_until_statement_period(tokens, source, idx);
    if let Some((node, next)) = try_parse_structured_data_decl(b, source, tokens, idx) {
        return Some((node, next));
    }

    let malformed = match scan {
        StmtPeriodScan::Found(period_i) => {
            classify_malformed_data_decl(source, tokens, idx, period_i)
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            if looks_like_typed_data_candidate(source, &tokens[idx..end_exclusive]) {
                Some("syntax error: expected '.' to end DATA declaration")
            } else {
                None
            }
        }
    };
    let Some(message) = malformed else {
        return None;
    };

    let (end_exclusive, err_end) = match scan {
        StmtPeriodScan::Found(period_i) => (period_i + 1, tokens[period_i].range.end),
        StmtPeriodScan::Unterminated { end_exclusive } => (
            end_exclusive,
            unterminated_err_end(tokens, end_exclusive, data_tok.range.end),
        ),
    };
    errors.push(crate::ParseError {
        message: message.to_string(),
        range: data_tok.range.start..err_end,
    });
    let mut children = Vec::with_capacity(end_exclusive.saturating_sub(idx));
    for t in &tokens[idx..end_exclusive] {
        children.push(token_leaf(b, t));
    }
    let node = b.branch(SyntaxKind::Error, data_tok.range.start..err_end, &children);
    let next = if tokens.get(end_exclusive).map(|t| t.kind) == Some(TokenKind::Eof) {
        tokens.len()
    } else {
        end_exclusive
    };
    Some((node, next))
}

pub fn try_parse_class_data_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    _errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let kw_end = match_hyphenated_keyword(source, tokens, idx, &["class", "data"])?;
    try_parse_chained_decl_after_keyword_span(
        b,
        source,
        tokens,
        idx,
        kw_end,
        SyntaxKind::DataDecl,
        SyntaxKind::DataTypedClause,
        true,
        false,
    )
}

fn try_parse_structured_data_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let data_tok = tokens.get(idx)?;
    let mut i = idx + 1;
    let has_colon = match tokens.get(i).map(|t| t.kind) {
        Some(TokenKind::Colon) => {
            i += 1;
            true
        }
        _ => false,
    };

    let mut clause_nodes = Vec::new();
    loop {
        while tokens.get(i).map(|t| t.kind) == Some(TokenKind::Comment) {
            i += 1;
        }
        let (clause, next_i) = parse_decl_clause(
            b,
            source,
            tokens,
            i,
            SyntaxKind::DataTypedClause,
            true,
            has_colon,
        )
        .or_else(|| {
            parse_begin_of_decl_clause(
                b,
                source,
                tokens,
                i,
                SyntaxKind::DataTypedClause,
                false,
                has_colon,
            )
        })?;
        clause_nodes.push(clause);
        i = next_i;

        let next = tokens.get(i)?;
        match next.kind {
            TokenKind::Comma => {
                if !has_colon {
                    return None;
                }
                i += 1;
            }
            TokenKind::Period => {
                let end = next.range.end;
                i += 1;
                let mut child_ids: Vec<NodeId> = Vec::with_capacity(2 + clause_nodes.len());
                child_ids.push(token_leaf(b, data_tok));
                child_ids.extend(clause_nodes);
                child_ids.push(token_leaf(b, next));
                let node = b.branch(SyntaxKind::DataDecl, data_tok.range.start..end, &child_ids);
                return Some((node, i));
            }
            _ => return None,
        }
    }
}

fn looks_like_typed_data_candidate(source: &str, stmt_tokens: &[Token]) -> bool {
    stmt_tokens.iter().any(|t| is_keyword(source, t, "type"))
        || matches!(stmt_tokens.get(1).map(|t| t.kind), Some(TokenKind::Colon))
}

fn classify_malformed_data_decl(
    source: &str,
    tokens: &[Token],
    idx: usize,
    period_i: usize,
) -> Option<&'static str> {
    let stmt_tokens = &tokens[idx..period_i];
    if stmt_tokens.is_empty() || !looks_like_typed_data_candidate(source, stmt_tokens) {
        return None;
    }

    let after_data = stmt_tokens.get(1)?;
    if after_data.kind == TokenKind::Colon {
        match stmt_tokens.get(2).map(|t| t.kind) {
            None | Some(TokenKind::Comma | TokenKind::Period) => {
                return Some("syntax error: expected declaration name in DATA statement");
            }
            _ => {}
        }
    } else if is_keyword(source, after_data, "type") {
        return Some("syntax error: expected declaration name in DATA statement");
    }

    let mut saw_type = false;
    for (rel_i, tok) in stmt_tokens.iter().enumerate() {
        if tok.kind == TokenKind::Comma {
            let prev_kind = rel_i
                .checked_sub(1)
                .and_then(|j| stmt_tokens.get(j))
                .map(|t| t.kind);
            let next_kind = stmt_tokens.get(rel_i + 1).map(|t| t.kind);
            if matches!(prev_kind, None | Some(TokenKind::Colon | TokenKind::Comma))
                || matches!(next_kind, None | Some(TokenKind::Comma | TokenKind::Period))
            {
                return Some("syntax error: expected declaration after ',' in DATA statement");
            }
        }
        if is_keyword(source, tok, "type") {
            saw_type = true;
            match stmt_tokens.get(rel_i + 1) {
                None => {
                    return Some("syntax error: expected type name after TYPE in DATA declaration");
                }
                Some(next) if matches!(next.kind, TokenKind::Comma | TokenKind::Period) => {
                    return Some("syntax error: expected type name after TYPE in DATA declaration");
                }
                _ => {}
            }
        }
    }

    if !saw_type {
        return None;
    }
    if stmt_tokens
        .last()
        .map(|t| t.kind == TokenKind::Comma)
        .unwrap_or(false)
    {
        return Some("syntax error: expected declaration after ',' in DATA statement");
    }

    None
}

fn parse_data_decl_name(
    b: &mut SyntaxTreeBuilder,
    _source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let first = tokens.get(idx)?;
    if first.kind != TokenKind::Ident {
        return None;
    }
    let mut children = vec![token_leaf(b, first)];
    let mut i = idx + 1;
    loop {
        let op = match tokens.get(i) {
            Some(t) => t,
            None => break,
        };
        if op.kind != TokenKind::Minus {
            break;
        }
        let prev = tokens.get(i.wrapping_sub(1))?;
        if have_space_between(prev, op) {
            break;
        }
        let field = tokens.get(i + 1)?;
        if field.kind != TokenKind::Ident {
            return None;
        }
        children.push(token_leaf(b, op));
        children.push(token_leaf(b, field));
        i += 2;
    }
    let start = first.range.start;
    let end = b.span(*children.last().unwrap()).end;
    Some((b.branch(SyntaxKind::DataDeclName, start..end, &children), i))
}

fn parse_optional_length_spec(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    stop_keywords: &[&str],
) -> Option<(NodeId, usize)> {
    let first = tokens.get(idx)?;
    if first.kind != TokenKind::Ident
        || (!first.lexeme(source).eq_ignore_ascii_case("length")
            && !first.lexeme(source).eq_ignore_ascii_case("decimals"))
    {
        return None;
    }
    let start = idx;
    let mut i = idx;
    while i < tokens.len() {
        let tok = &tokens[i];
        if i > start
            && tok.kind == TokenKind::Ident
            && stop_keywords
                .iter()
                .any(|kw| tok.lexeme(source).eq_ignore_ascii_case(kw))
        {
            break;
        }
        if i > start
            && matches!(
                tok.kind,
                TokenKind::Comma | TokenKind::Period | TokenKind::Eof
            )
        {
            break;
        }
        if i > start
            && tok.kind == TokenKind::Ident
            && (tok.lexeme(source).eq_ignore_ascii_case("length")
                || tok.lexeme(source).eq_ignore_ascii_case("decimals"))
        {
            break;
        }
        i += 1;
    }
    let mut children = Vec::with_capacity(i - start);
    for t in &tokens[start..i] {
        children.push(token_leaf(b, t));
    }
    let end = b.span(*children.last().unwrap()).end;
    Some((
        b.branch(SyntaxKind::LengthSpec, first.range.start..end, &children),
        i,
    ))
}

fn parse_value_clause(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let value_tok = tokens.get(idx)?;
    if !is_keyword(source, value_tok, "value") {
        return None;
    }
    let value_kw = token_leaf(b, value_tok);
    let (expr, next) = parse_type_ref_tokens(b, source, tokens, idx + 1, &[])?;
    let range = value_tok.range.start..b.span(expr).end;
    Some((
        b.branch(SyntaxKind::ValueClause, range, &[value_kw, expr]),
        next,
    ))
}

fn parse_inline_name(
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

fn inline_name_spacing_is_valid(
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

fn match_hyphenated_keyword(
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

fn parse_optional_paren_length(
    b: &mut SyntaxTreeBuilder,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let lparen = tokens.get(idx)?;
    if lparen.kind != TokenKind::LParen {
        return None;
    }
    let mut next = idx + 1;
    let mut depth = 1i32;
    while next < tokens.len() {
        match tokens[next].kind {
            TokenKind::LParen => depth += 1,
            TokenKind::RParen => {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            TokenKind::Eof => return None,
            _ => {}
        }
        next += 1;
    }
    let rparen = tokens.get(next)?;
    if rparen.kind != TokenKind::RParen {
        return None;
    }
    let mut expr_children = Vec::with_capacity(next.saturating_sub(idx + 1));
    for t in &tokens[idx + 1..next] {
        expr_children.push(token_leaf(b, t));
    }
    let expr = if expr_children.is_empty() {
        b.branch(SyntaxKind::Error, lparen.range.end..rparen.range.start, &[])
    } else {
        b.branch(
            SyntaxKind::LengthSpec,
            tokens[idx + 1].range.start..tokens[next - 1].range.end,
            &expr_children,
        )
    };
    let l = token_leaf(b, lparen);
    let r = token_leaf(b, rparen);
    Some((
        b.branch(
            SyntaxKind::LengthSpec,
            lparen.range.start..rparen.range.end,
            &[l, expr, r],
        ),
        next + 1,
    ))
}

fn try_parse_data_inline_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let data_tok = tokens.get(idx)?;
    let lparen = tokens.get(idx + 1)?;
    if lparen.kind != TokenKind::LParen {
        return None;
    }
    let (name, i) = parse_inline_name(b, tokens, idx + 2)?;
    let rparen = tokens.get(i)?;
    if rparen.kind != TokenKind::RParen {
        return None;
    }
    if !inline_name_spacing_is_valid(tokens, idx + 1, idx + 2, i) {
        match scan_until_statement_period(tokens, source, idx + 1) {
            StmtPeriodScan::Found(period_i) => {
                errors.push(crate::ParseError {
                    message:
                        "syntax error: inline DATA declaration must not contain whitespace inside parentheses"
                            .to_string(),
                    range: data_tok.range.start..tokens[period_i].range.end,
                });
                let mut children = Vec::with_capacity(period_i - idx + 1);
                for t in &tokens[idx..=period_i] {
                    children.push(token_leaf(b, t));
                }
                let node = b.branch(
                    SyntaxKind::Error,
                    data_tok.range.start..tokens[period_i].range.end,
                    &children,
                );
                return Some((node, period_i + 1));
            }
            StmtPeriodScan::Unterminated { end_exclusive } => {
                let err_end = unterminated_err_end(tokens, end_exclusive, data_tok.range.end);
                errors.push(crate::ParseError {
                    message:
                        "syntax error: inline DATA declaration must not contain whitespace inside parentheses"
                            .to_string(),
                    range: data_tok.range.start..err_end,
                });
                let mut children = Vec::with_capacity(end_exclusive.saturating_sub(idx));
                for t in &tokens[idx..end_exclusive] {
                    children.push(token_leaf(b, t));
                }
                let node = b.branch(SyntaxKind::Error, data_tok.range.start..err_end, &children);
                return Some((node, end_exclusive));
            }
        }
    }
    let eq_tok = tokens.get(i + 1)?;
    if eq_tok.kind != TokenKind::Eq {
        return None;
    }
    match scan_until_statement_period(tokens, source, i + 2) {
        StmtPeriodScan::Found(period_i) => {
            let rhs = crate::expr::parse_arithmetic_expr(
                b,
                source,
                &tokens[i + 2..period_i],
                Some(eq_tok),
            );
            let data_leaf = token_leaf(b, data_tok);
            let lparen_leaf = token_leaf(b, lparen);
            let rparen_leaf = token_leaf(b, rparen);
            let eq_leaf = token_leaf(b, eq_tok);
            let period_leaf = token_leaf(b, &tokens[period_i]);
            let node = b.branch(
                SyntaxKind::DataInlineDecl,
                data_tok.range.start..tokens[period_i].range.end,
                &[
                    data_leaf,
                    lparen_leaf,
                    name,
                    rparen_leaf,
                    eq_leaf,
                    rhs,
                    period_leaf,
                ],
            );
            Some((node, period_i + 1))
        }
        StmtPeriodScan::Unterminated { end_exclusive } => {
            let err_end = unterminated_err_end(tokens, end_exclusive, data_tok.range.end);
            errors.push(crate::ParseError {
                message: "syntax error: expected '.' to end inline DATA declaration".to_string(),
                range: data_tok.range.start..err_end,
            });
            let mut children = Vec::with_capacity(end_exclusive - idx);
            for t in &tokens[idx..end_exclusive] {
                children.push(token_leaf(b, t));
            }
            let node = b.branch(SyntaxKind::Error, data_tok.range.start..err_end, &children);
            Some((node, end_exclusive))
        }
    }
}

fn parse_decl_clause(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    node_kind: SyntaxKind,
    allow_like: bool,
    allow_value: bool,
) -> Option<(NodeId, usize)> {
    let (name, mut i) = parse_data_decl_name(b, source, tokens, idx)?;
    let mut children = vec![name];

    if let Some((legacy_len, j)) = parse_optional_paren_length(b, tokens, i) {
        children.push(legacy_len);
        i = j;
    }

    if let Some((length, j)) =
        parse_optional_length_spec(b, source, tokens, i, &["TYPE", "LIKE", "VALUE"])
    {
        children.push(length);
        i = j;
    }

    let type_kw = tokens.get(i)?;
    if !is_keyword(source, type_kw, "type") && !(allow_like && is_keyword(source, type_kw, "like"))
    {
        return None;
    }
    children.push(token_leaf(b, type_kw));
    i += 1;

    let (typed, j) = parse_type_ref_tokens(b, source, tokens, i, &["VALUE"])?;
    children.push(typed);
    i = j;

    while let Some((length, j)) = parse_optional_length_spec(b, source, tokens, i, &["VALUE"]) {
        children.push(length);
        i = j;
    }

    if allow_value && let Some((value, j)) = parse_value_clause(b, source, tokens, i) {
        children.push(value);
        i = j;
    }

    let range = b.span(*children.first().unwrap()).start..b.span(*children.last().unwrap()).end;
    Some((b.branch(node_kind, range, &children), i))
}

fn parse_begin_of_decl_clause(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    node_kind: SyntaxKind,
    allow_like: bool,
    allow_value: bool,
) -> Option<(NodeId, usize)> {
    parse_structured_decl(b, source, tokens, idx, node_kind, allow_like, allow_value)
}

fn parse_structured_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    node_kind: SyntaxKind,
    allow_like: bool,
    allow_value: bool,
) -> Option<(NodeId, usize)> {
    let begin_tok = tokens.get(idx)?;
    if !is_keyword(source, begin_tok, "begin") {
        return None;
    }
    if !tokens
        .get(idx + 1)
        .is_some_and(|tok| is_keyword(source, tok, "of"))
    {
        return None;
    }
    if tokens.get(idx + 2)?.kind != TokenKind::Ident {
        return None;
    }

    let mut children = vec![
        token_leaf(b, begin_tok),
        token_leaf(b, tokens.get(idx + 1)?),
        token_leaf(b, tokens.get(idx + 2)?),
    ];
    let mut i = idx + 3;
    if tokens.get(i).map(|t| t.kind) == Some(TokenKind::Comma) {
        children.push(token_leaf(b, tokens.get(i)?));
        i += 1;
    }

    while i < tokens.len() {
        while tokens.get(i).map(|t| t.kind) == Some(TokenKind::Comment) {
            children.push(token_leaf(b, tokens.get(i)?));
            i += 1;
        }
        let tok = tokens.get(i)?;
        if tok.kind == TokenKind::Eof {
            return None;
        }
        if is_keyword(source, tok, "end")
            && tokens
                .get(i + 1)
                .is_some_and(|next| is_keyword(source, next, "of"))
        {
            let end_name = tokens.get(i + 2)?;
            if end_name.kind != TokenKind::Ident {
                return None;
            }
            children.push(token_leaf(b, tok));
            children.push(token_leaf(b, tokens.get(i + 1)?));
            children.push(token_leaf(b, end_name));
            let node = b.branch(
                node_kind,
                begin_tok.range.start..end_name.range.end,
                &children,
            );
            return Some((node, i + 3));
        }

        let (component, next_i) = parse_structured_decl(
            b,
            source,
            tokens,
            i,
            SyntaxKind::StructuredDecl,
            allow_like,
            allow_value,
        )
        .or_else(|| parse_structured_include_clause(b, source, tokens, i))
        .or_else(|| parse_structured_field_clause(b, source, tokens, i, allow_like, allow_value))?;
        children.push(component);
        i = next_i;

        while tokens.get(i).map(|t| t.kind) == Some(TokenKind::Comment) {
            children.push(token_leaf(b, tokens.get(i)?));
            i += 1;
        }
        if tokens.get(i).map(|t| t.kind) == Some(TokenKind::Comma) {
            children.push(token_leaf(b, tokens.get(i)?));
            i += 1;
        }
    }
    None
}

fn parse_structured_include_clause(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let include_tok = tokens.get(idx)?;
    if !is_keyword(source, include_tok, "include") {
        return None;
    }
    let kind_tok = tokens.get(idx + 1)?;
    if !is_keyword(source, kind_tok, "type") && !is_keyword(source, kind_tok, "structure") {
        return None;
    }

    let mut children = vec![token_leaf(b, include_tok), token_leaf(b, kind_tok)];
    let (type_ref, mut i) = parse_type_ref_tokens(b, source, tokens, idx + 2, &["AS", "RENAMING"])?;
    children.push(type_ref);

    while let Some(tok) = tokens.get(i) {
        if matches!(tok.kind, TokenKind::Comma | TokenKind::Period | TokenKind::Eof) {
            break;
        }
        children.push(token_leaf(b, tok));
        i += 1;
    }

    let range = b.span(*children.first().unwrap()).start..b.span(*children.last().unwrap()).end;
    Some((b.branch(SyntaxKind::StructuredIncludeClause, range, &children), i))
}

fn parse_structured_field_clause(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    allow_like: bool,
    allow_value: bool,
) -> Option<(NodeId, usize)> {
    parse_decl_clause(
        b,
        source,
        tokens,
        idx,
        SyntaxKind::StructuredFieldClause,
        allow_like,
        allow_value,
    )
    .or_else(|| parse_untyped_structured_field_clause(b, source, tokens, idx))
}

fn parse_untyped_structured_field_clause(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let (name, mut i) = parse_data_decl_name(b, source, tokens, idx)?;
    let mut children = vec![name];

    if let Some((legacy_len, j)) = parse_optional_paren_length(b, tokens, i) {
        children.push(legacy_len);
        i = j;
    }

    while let Some((length, j)) =
        parse_optional_length_spec(b, source, tokens, i, &["TYPE", "LIKE", "VALUE"])
    {
        children.push(length);
        i = j;
    }

    if tokens
        .get(i)
        .is_some_and(|tok| is_keyword(source, tok, "type") || is_keyword(source, tok, "like"))
    {
        return None;
    }

    let range = b.span(*children.first().unwrap()).start..b.span(*children.last().unwrap()).end;
    Some((
        b.branch(SyntaxKind::StructuredFieldClause, range, &children),
        i,
    ))
}

fn try_parse_chained_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    keyword: &str,
    decl_kind: SyntaxKind,
    clause_kind: SyntaxKind,
    allow_like: bool,
    allow_value: bool,
) -> Option<(NodeId, usize)> {
    let kw_tok = tokens.get(idx)?;
    if !is_keyword(source, kw_tok, keyword) {
        return None;
    }
    try_parse_chained_decl_after_keyword_span(
        b,
        source,
        tokens,
        idx,
        idx + 1,
        decl_kind,
        clause_kind,
        allow_like,
        allow_value,
    )
}

fn try_parse_chained_decl_after_keyword_span(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    kw_end: usize,
    decl_kind: SyntaxKind,
    clause_kind: SyntaxKind,
    allow_like: bool,
    allow_value: bool,
) -> Option<(NodeId, usize)> {
    let mut i = kw_end;
    let has_colon = match tokens.get(i).map(|t| t.kind) {
        Some(TokenKind::Colon) => {
            i += 1;
            true
        }
        _ => false,
    };

    let mut clause_nodes = Vec::new();
    loop {
        while tokens.get(i).map(|t| t.kind) == Some(TokenKind::Comment) {
            i += 1;
        }
        let (clause, next_i) = parse_decl_clause(
            b,
            source,
            tokens,
            i,
            clause_kind,
            allow_like,
            allow_value,
        )
        .or_else(|| {
            parse_begin_of_decl_clause(b, source, tokens, i, clause_kind, allow_like, allow_value)
        })?;
        clause_nodes.push(clause);
        i = next_i;
        let next = tokens.get(i)?;
        match next.kind {
            TokenKind::Comma if has_colon => i += 1,
            TokenKind::Period => {
                let mut children =
                    Vec::with_capacity(clause_nodes.len() + (kw_end - idx) + 1);
                for token in &tokens[idx..kw_end] {
                    children.push(token_leaf(b, token));
                }
                children.extend(clause_nodes);
                children.push(token_leaf(b, next));
                let node = b.branch(decl_kind, tokens[idx].range.start..next.range.end, &children);
                return Some((node, i + 1));
            }
            _ => return None,
        }
    }
}

pub fn try_parse_statics_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    _errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    try_parse_chained_decl(
        b,
        source,
        tokens,
        idx,
        "statics",
        SyntaxKind::StaticsDecl,
        SyntaxKind::DataTypedClause,
        true,
        true,
    )
}

pub fn try_parse_types_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    _errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    try_parse_types_structured_block_decl(b, source, tokens, idx).or_else(|| {
        try_parse_chained_decl(
            b,
            source,
            tokens,
            idx,
            "types",
            SyntaxKind::TypesDecl,
            SyntaxKind::TypesTypedClause,
            false,
            false,
        )
    })
}

fn try_parse_types_structured_block_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let types_tok = tokens.get(idx)?;
    if !is_keyword(source, types_tok, "types") {
        return None;
    }

    let mut i = idx + 1;
    if tokens.get(i).map(|t| t.kind) == Some(TokenKind::Colon) {
        i += 1;
    }

    let (structured, next_i) = parse_types_structured_block(b, source, tokens, i)?;
    let types_kw = token_leaf(b, types_tok);
    let node = b.branch(
        SyntaxKind::TypesDecl,
        types_tok.range.start..b.span(structured).end,
        &[types_kw, structured],
    );
    Some((node, next_i))
}

fn parse_types_structured_block(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
) -> Option<(NodeId, usize)> {
    let begin_tok = tokens.get(idx)?;
    let of_tok = tokens.get(idx + 1)?;
    let name_tok = tokens.get(idx + 2)?;
    let begin_period = tokens.get(idx + 3)?;
    if !is_keyword(source, begin_tok, "begin")
        || !is_keyword(source, of_tok, "of")
        || name_tok.kind != TokenKind::Ident
        || begin_period.kind != TokenKind::Period
    {
        return None;
    }

    let mut children = vec![
        token_leaf(b, begin_tok),
        token_leaf(b, of_tok),
        token_leaf(b, name_tok),
        token_leaf(b, begin_period),
    ];
    let mut i = idx + 4;

    loop {
        while tokens.get(i).map(|t| t.kind) == Some(TokenKind::Comment) {
            children.push(token_leaf(b, tokens.get(i)?));
            i += 1;
        }

        let tok = tokens.get(i)?;
        if tok.kind == TokenKind::Eof {
            return None;
        }
        if is_keyword(source, tok, "end")
            && tokens
                .get(i + 1)
                .is_some_and(|next| is_keyword(source, next, "of"))
            && tokens.get(i + 2)?.kind == TokenKind::Ident
            && tokens.get(i + 3).map(|t| t.kind) == Some(TokenKind::Period)
        {
            children.push(token_leaf(b, tok));
            children.push(token_leaf(b, tokens.get(i + 1)?));
            children.push(token_leaf(b, tokens.get(i + 2)?));
            children.push(token_leaf(b, tokens.get(i + 3)?));
            let node = b.branch(
                SyntaxKind::StructuredDecl,
                begin_tok.range.start..tokens.get(i + 3)?.range.end,
                &children,
            );
            return Some((node, i + 4));
        }

        if is_keyword(source, tok, "include") {
            let (include, next_i) = parse_structured_include_clause(b, source, tokens, i)?;
            if tokens.get(next_i).map(|t| t.kind) != Some(TokenKind::Period) {
                return None;
            }
            children.push(include);
            children.push(token_leaf(b, tokens.get(next_i)?));
            i = next_i + 1;
            continue;
        }

        if is_keyword(source, tok, "types") {
            i += 1;
            if tokens.get(i).map(|t| t.kind) == Some(TokenKind::Colon) {
                i += 1;
            }
            let next_i = parse_structured_types_component_run(
                b,
                source,
                tokens,
                i,
                &mut children,
            )?;
            i = next_i;
            continue;
        }

        return None;
    }
}

fn parse_structured_types_component_run(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    mut idx: usize,
    out: &mut Vec<NodeId>,
) -> Option<usize> {
    loop {
        while tokens.get(idx).map(|t| t.kind) == Some(TokenKind::Comment) {
            out.push(token_leaf(b, tokens.get(idx)?));
            idx += 1;
        }

        let tok = tokens.get(idx)?;
        if tok.kind == TokenKind::Eof {
            return None;
        }
        if is_keyword(source, tok, "end")
            && tokens
                .get(idx + 1)
                .is_some_and(|next| is_keyword(source, next, "of"))
        {
            return Some(idx);
        }

        let (component, next_i) = parse_structured_decl(
            b,
            source,
            tokens,
            idx,
            SyntaxKind::StructuredDecl,
            false,
            false,
        )
        .or_else(|| parse_structured_include_clause(b, source, tokens, idx))
        .or_else(|| parse_decl_clause(
            b,
            source,
            tokens,
            idx,
            SyntaxKind::TypesTypedClause,
            false,
            false,
        ))
        ?;
        out.push(component);
        idx = next_i;

        while tokens.get(idx).map(|t| t.kind) == Some(TokenKind::Comment) {
            out.push(token_leaf(b, tokens.get(idx)?));
            idx += 1;
        }

        if tokens.get(idx).map(|t| t.kind) == Some(TokenKind::Comma) {
            idx += 1;
            continue;
        }

        if tokens
            .get(idx)
            .is_some_and(|tok| is_keyword(source, tok, "end"))
            && tokens
                .get(idx + 1)
                .is_some_and(|next| is_keyword(source, next, "of"))
        {
            return Some(idx);
        }

        return None;
    }
}

pub fn try_parse_constants_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    _errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    try_parse_chained_decl(
        b,
        source,
        tokens,
        idx,
        "constants",
        SyntaxKind::ConstantsDecl,
        SyntaxKind::ConstantClause,
        true,
        true,
    )
}

pub fn try_parse_field_symbols_decl(
    b: &mut SyntaxTreeBuilder,
    source: &str,
    tokens: &[Token],
    idx: usize,
    _errors: &mut Vec<crate::ParseError>,
) -> Option<(NodeId, usize)> {
    let kw_end = match_hyphenated_keyword(source, tokens, idx, &["field", "symbols"])?;
    let mut i = kw_end;
    let has_colon = match tokens.get(i).map(|t| t.kind) {
        Some(TokenKind::Colon) => {
            i += 1;
            true
        }
        _ => false,
    };
    let mut clause_nodes = Vec::new();
    loop {
        let (clause, next_i) = parse_decl_clause(
            b,
            source,
            tokens,
            i,
            SyntaxKind::FieldSymbolClause,
            true,
            false,
        )?;
        clause_nodes.push(clause);
        i = next_i;
        let next = tokens.get(i)?;
        match next.kind {
            TokenKind::Comma if has_colon => i += 1,
            TokenKind::Period => {
                let mut children = Vec::new();
                for t in &tokens[idx..kw_end] {
                    children.push(token_leaf(b, t));
                }
                children.extend(clause_nodes);
                children.push(token_leaf(b, next));
                let node = b.branch(
                    SyntaxKind::FieldSymbolsDecl,
                    tokens[idx].range.start..next.range.end,
                    &children,
                );
                return Some((node, i + 1));
            }
            _ => return None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::build_file_tree;
    use abap_ast::File;
    use abap_lexer::tokenize;

    fn tree_ok(src: &str) -> File {
        let tok = tokenize(src);
        let mut err = Vec::new();
        let file = build_file_tree(src, &tok.tokens, src.len(), &mut err);
        assert!(err.is_empty(), "{:?}", err);
        file
    }

    #[test]
    fn single_data_typed_decl() {
        let src = "DATA lv_count TYPE i.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple), 1);
    }

    #[test]
    fn chained_data_colon() {
        let src = "DATA: lv_a TYPE i, lv_b TYPE string.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 2);
    }

    #[test]
    fn chained_data_accepts_table_type_and_inline_comment() {
        let src =
            "DATA: lt_split TYPE STANDARD TABLE OF string, \" comment\n      ls_split TYPE string.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 2);
    }

    #[test]
    fn chained_data_accepts_value_clause() {
        let src = "DATA: lv_a TYPE i, lv_b TYPE int2 VALUE 1.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 2);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ValueClause), 1);
    }

    #[test]
    fn chained_data_accepts_like_clauses() {
        let src = "DATA: ls_line LIKE LINE OF itab, lt_copy LIKE itab.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 2);
        assert!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple) >= 2);
    }

    #[test]
    fn class_data_accepts_like_clause() {
        let src = "CLASS-DATA gt_copy LIKE gt_source.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataTypedClause), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple), 1);
    }

    #[test]
    fn data_begin_end_of_clause() {
        let file = tree_ok("DATA: BEGIN OF ls_dat, yyyy(4), mm(2), dd(2), END OF ls_dat.");
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
    }

    #[test]
    fn data_name_minus_chain() {
        let src = "DATA screen0100-serial TYPE c.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDeclName), 1);
    }

    #[test]
    fn type_ref_selector_chain() {
        let src = "DATA r TYPE ty_ref=>elem.";
        let file = tree_ok(src);
        let type_refs = file.count_kind(file.root(), SyntaxKind::TypeRefSimple);
        assert!(type_refs >= 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::TypeRefSelectorChain),
            1
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefName), 2);
    }

    #[test]
    fn data_ref_to_class_type_ref() {
        let src = "DATA lo_instance TYPE REF TO some_class.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple), 2);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefName), 1);
    }

    #[test]
    fn table_wrapper_type_ref_nests_inner_type() {
        let src = "TYPES ty_stmt_tab TYPE STANDARD TABLE OF REF TO zcl_stmt WITH DEFAULT KEY.";
        let file = tree_ok(src);
        let type_ref = file
            .find_first_kind(file.root(), SyntaxKind::TypeRefSimple)
            .expect("type ref");
        assert_eq!(file.count_kind(type_ref, SyntaxKind::TypeRefName), 1);
        assert!(file.count_kind(type_ref, SyntaxKind::TypeRefSimple) >= 2);
    }

    #[test]
    fn data_inline_decl() {
        let file = tree_ok("DATA(lv_value) = 1.");
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataInlineDecl), 1);
    }

    #[test]
    fn data_inline_decl_rejects_whitespace_inside_parentheses() {
        for src in [
            "DATA( lv_var) = 1.",
            "DATA( lv_var ) = 1.",
            "DATA(lv_var ) = 1.",
            "DATA(\nlv_var) = 1.",
        ] {
            let parsed = crate::parse(src);
            assert!(
                parsed
                    .errors
                    .iter()
                    .any(|err| err.message.contains("inline DATA declaration")),
                "{src}: {:?}",
                parsed.errors
            );
            assert_eq!(
                parsed
                    .file
                    .count_kind(parsed.file.root(), SyntaxKind::DataInlineDecl),
                0
            );
            assert!(
                parsed
                    .file
                    .count_kind(parsed.file.root(), SyntaxKind::Error)
                    >= 1
            );
        }
    }

    #[test]
    fn statics_decl_with_value() {
        let file = tree_ok("STATICS sv_last TYPE tznzone VALUE 'UTC'.");
        assert_eq!(file.count_kind(file.root(), SyntaxKind::StaticsDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ValueClause), 1);
    }

    #[test]
    fn types_chain_decl() {
        let file = tree_ok("TYPES: ty_int TYPE i, ty_name TYPE string.");
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypesDecl), 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::TypesTypedClause),
            2
        );
    }

    #[test]
    fn constants_decl_with_length_and_value() {
        let file = tree_ok("CONSTANTS lcv_max(14) TYPE p DECIMALS 7 VALUE '0.9999999'.");
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ConstantsDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ConstantClause), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::LengthSpec), 2);
    }

    #[test]
    fn field_symbols_like_line_of() {
        let file = tree_ok("FIELD-SYMBOLS <line> LIKE LINE OF itab.");
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::FieldSymbolsDecl),
            1
        );
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::FieldSymbolClause),
            1
        );
    }

    #[test]
    fn types_begin_end_of_clause() {
        let file = tree_ok("TYPES: BEGIN OF ty_pair, a TYPE i, b TYPE string, END OF ty_pair.");
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypesDecl), 1);
    }

    #[test]
    fn structured_types_clause_parses_component_fields() {
        let file = tree_ok(
            "TYPES: BEGIN OF ts_cust_info, type TYPE char1, root TYPE string, END OF ts_cust_info.",
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypesDecl), 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::StructuredFieldClause),
            2
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple), 2);
    }

    #[test]
    fn structured_types_clause_parses_include_components() {
        let file = tree_ok(
            "TYPES: BEGIN OF ty_outer, INCLUDE TYPE ty_inner AS inner, field TYPE i, END OF ty_outer.",
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypesDecl), 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::StructuredIncludeClause),
            1
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple), 2);
    }

    #[test]
    fn block_structured_types_clause_parses_include_components() {
        let file = tree_ok(
            "TYPES: BEGIN OF ty_outer. INCLUDE TYPE ty_inner AS inner. TYPES: field TYPE i, other TYPE string, END OF ty_outer.",
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypesDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::StructuredDecl), 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::StructuredIncludeClause),
            1
        );
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::TypesTypedClause),
            2
        );
    }

    #[test]
    fn nested_structured_types_clause_creates_nested_nodes() {
        let file = tree_ok(
            "TYPES: BEGIN OF ty_outer, BEGIN OF inner, a TYPE i, END OF inner, END OF ty_outer.",
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypesDecl), 1);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::StructuredDecl), 1);
        assert_eq!(
            file.count_kind(file.root(), SyntaxKind::StructuredFieldClause),
            1
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::TypeRefSimple), 1);
    }

    #[test]
    fn constants_begin_end_of_clause() {
        let file = tree_ok(
            "CONSTANTS: BEGIN OF gc_pair, a TYPE i VALUE 1, b TYPE i VALUE 2, END OF gc_pair.",
        );
        assert_eq!(file.count_kind(file.root(), SyntaxKind::ConstantsDecl), 1);
    }

    #[test]
    fn data_inline_not_matched() {
        let src = "DATA(ref) = 1.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 0);
    }

    #[test]
    fn comma_chain_requires_colon() {
        let src = "DATA a TYPE i, b TYPE i.";
        let file = tree_ok(src);
        assert_eq!(file.count_kind(file.root(), SyntaxKind::DataDecl), 0);
    }

    /// Unsupported `DATA` forms must not get a `DataDecl` wrapper or spurious diagnostics.
    fn assert_not_classified_as_data_decl(src: &str) {
        let parsed = crate::parse(src);
        assert!(
            parsed.errors.is_empty(),
            "expected no errors for {src:?}, got {:?}",
            parsed.errors
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::DataDecl),
            0,
            "expected no DataDecl node for {src:?}"
        );
    }

    fn assert_malformed_data_decl(src: &str, message_fragment: &str) {
        let parsed = crate::parse(src);
        assert!(
            parsed
                .errors
                .iter()
                .any(|e| e.message.contains(message_fragment)),
            "expected parser error containing {message_fragment:?} for {src:?}, got {:?}",
            parsed.errors
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::DataDecl),
            0,
            "expected malformed DATA to avoid DataDecl node for {src:?}"
        );
        assert!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::Error)
                >= 1,
            "expected malformed DATA to produce an Error node for {src:?}"
        );
    }

    #[test]
    fn negative_missing_variable_name() {
        assert_malformed_data_decl("DATA TYPE i.", "expected declaration name");
    }

    #[test]
    fn negative_missing_type_ref() {
        assert_malformed_data_decl("DATA lv TYPE .", "expected type name");
    }

    #[test]
    fn negative_missing_trailing_period() {
        let parsed = crate::parse("DATA lv TYPE string");
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::DataDecl),
            0
        );
        assert!(
            parsed
                .errors
                .iter()
                .any(|e| e.message.contains("expected '.'")),
            "{:?}",
            parsed.errors
        );
    }

    #[test]
    fn negative_like_instead_of_type() {
        assert_not_classified_as_data_decl("DATA lv LIKE lv_other.");
    }

    #[test]
    fn negative_value_clause_unsupported() {
        assert_not_classified_as_data_decl("DATA lv TYPE i VALUE 0.");
    }

    #[test]
    fn negative_assignment_style_not_typed_decl() {
        assert_not_classified_as_data_decl("DATA lv = 1.");
    }

    #[test]
    fn negative_empty_chain_after_colon() {
        assert_malformed_data_decl("DATA: .", "expected declaration name");
    }

    #[test]
    fn negative_double_comma() {
        assert_malformed_data_decl(
            "DATA: lv_a TYPE i,, lv_b TYPE i.",
            "expected declaration after ','",
        );
    }

    #[test]
    fn malformed_data_recovers_before_next_statement() {
        let parsed = crate::parse("DATA lv TYPE string\nlv_after = 1.");
        assert!(
            parsed
                .errors
                .iter()
                .any(|e| e.message.contains("expected '.' to end DATA declaration")),
            "{:?}",
            parsed.errors
        );
        assert_eq!(
            parsed
                .file
                .count_kind(parsed.file.root(), SyntaxKind::AssignStmt),
            1
        );
    }
}
